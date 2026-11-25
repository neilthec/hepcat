// Native counterpart of SM-5-point-construction.nb. From the HEPCAT repository root:
//   cmake -S csource -B tests/results/native-reduce-build -DCMAKE_BUILD_TYPE=Release
//   cmake --build tests/results/native-reduce-build --target hepcat-sm-five-point
//   tests/results/native-reduce-build/hepcat-sm-five-point --list
//   tests/results/native-reduce-build/hepcat-sm-five-point --family hhhhh
// Omit the filter for the full SM. --sector selects a notebook sector. Use --start-at
// FAMILY to continue a sweep at a family with an unfinished report. Each invocation
// writes a fresh run under tests/results/spinas-output/SM-5-point. A case is saved
// only after its native amplitude checks pass. The separate family result includes
// CalcHEP and MadGraph. --native-only runs the signed native checks alone.
// The default is one worker and 100,000 phase-space points per numerical comparison.

#include "amplitude.hpp"
#include "construction-progress.hpp"
#include "external_export.hpp"

#include <algorithm>
#include <charconv>
#include <chrono>
#include <filesystem>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <limits>
#include <map>
#include <set>
#include <sstream>
#include <stdexcept>

namespace fs = std::filesystem;
using namespace hepcat;
using legs = std::vector<std::string>;

namespace {
struct entry {
    std::string id, family_id, name, sector, particles, incoming, outgoing;
    bool has_tree;
    std::size_t labeled_helicities;
};
const entry inventory[] = {
#include "sm-five-point-inventory.inc"
};

legs split(const std::string &line) {
    std::istringstream stream(line);
    legs out;
    for (std::string item; std::getline(stream, item, ',');) out.push_back(item);
    return out;
}
std::string species(const std::string &p) { return p.substr(0,p.find('.')); }
legs sorted(legs v) { std::sort(v.begin(),v.end()); return v; }

struct settings {
    fs::path root, output, spinas;
    std::string family, sector, start, case_id;
    bool list = false, list_cases = false, native_only = false;
    std::size_t workers = 1;
    external_options external;
};
std::size_t number(const std::string &text) {
    std::size_t value = 0;
    auto result = std::from_chars(text.data(), text.data()+text.size(), value);
    if (result.ec!=std::errc{} || result.ptr!=text.data()+text.size())
        throw std::invalid_argument("Expected an unsigned integer: "+text);
    return value;
}
settings options(int argc, char **argv) {
    settings s;
    s.external.points = 100000;
    for (int i=1; i<argc; ++i) {
        std::string arg=argv[i];
        if (arg=="--help") {
            std::cout << "hepcat-sm-five-point [--list | --list-cases] [--sector NAME] [--family NAME|ID] [--case ID --native-only]\n"
                         "  [--start-at NAME|ID] [--native-only] [--points N] [--seed N] [--workers N]\n"
                         "  [--root DIR] [--output DIR] [--spinas DIR] [--calchep DIR] [--madgraph DIR]\n"
                         "  [--python EXE] [--cmake EXE]\n";
            std::exit(0);
        }
        if (arg=="--list") { s.list=true; continue; }
        if (arg=="--list-cases") { s.list_cases=true; continue; }
        if (arg=="--native-only") { s.native_only=true; continue; }
        if (++i==argc) throw std::invalid_argument("Missing value for "+arg);
        std::string value=argv[i];
        if (arg=="--root") s.root=value;
        else if (arg=="--output") s.output=value;
        else if (arg=="--case") s.case_id=value;
        else if (arg=="--sector") s.sector=value;
        else if (arg=="--family") s.family=value;
        else if (arg=="--start-at") s.start=value;
        else if (arg=="--workers") s.workers=number(value);
        else if (arg=="--points") s.external.points=number(value);
        else if (arg=="--seed") s.external.seed=number(value);
        else if (arg=="--calchep") s.external.calchep_directory=fs::absolute(value);
        else if (arg=="--madgraph") s.external.madgraph_directory=fs::absolute(value);
        else if (arg=="--spinas") s.spinas=fs::absolute(value);
        else if (arg=="--python") s.external.python_executable=value;
        else if (arg=="--cmake") s.external.cmake_executable=value;
        else throw std::invalid_argument("Unknown option "+arg);
    }
    if (!s.case_id.empty() && !s.native_only && !s.list && !s.list_cases)
        throw std::invalid_argument("Use --native-only with --case. External comparisons require the full family.");
    if (s.root.empty()) {
        s.root=fs::current_path();
        while (!fs::is_regular_file(s.root/"models/amps1.mdl")) {
            const auto parent=s.root.parent_path();
            if (parent==s.root)
                throw std::invalid_argument("Specify the HEPCAT checkout with --root DIR");
            s.root=parent;
        }
    }
    s.root=fs::canonical(s.root);
    if (s.external.points<10000) throw std::invalid_argument("Use at least 10,000 points");
    if (!s.external.seed) s.external.seed=std::chrono::system_clock::now().time_since_epoch().count();
    if (s.output.empty()) s.output=s.root/"tests/results/spinas-output/SM-5-point";
    if (s.spinas.empty()) s.spinas=s.root.parent_path()/"spinas";
    auto &e=s.external;
    if (e.calchep_directory.empty()) e.calchep_directory=s.root.parent_path()/"CalcHEP/calchep_3.9.2";
    if (e.madgraph_directory.empty()) e.madgraph_directory=s.root.parent_path()/"MadGraph";
    if (e.python_executable=="python3") {
        std::ifstream launcher(e.madgraph_directory/"bin/mg5_aMC");
        std::string line;
        if (std::getline(launcher,line) && line.rfind("#!/",0)==0) {
            fs::path interpreter=line.substr(2);
            if (interpreter.filename().string().rfind("python",0)==0 && fs::is_regular_file(interpreter))
                e.python_executable=interpreter.string();
        }
    }
    e.runtime_directory=s.root/"source/internal/export";
    e.template_directory=s.root/"csource/external_templates";
    e.calchep_model_directory=s.root/"tests/reference-models/calchep";
    e.calchep_model_number=1;
    e.madgraph_model=(s.root/"tests/reference-models/madgraph/sm").string();
    e.madgraph_parameter_rules["lamWS"]=0;
    e.progress=notebook_output::print_stage;
    return s;
}

// Join a new three-point vertex to every possible leg of a smaller tree.
using multisets = std::set<legs>;
multisets attach(const multisets &bases, const std::map<std::string,multisets> &ends,
                 const model &sm) {
    multisets result;
    for (const auto &v:bases) for (std::size_t i=0;i<v.size();++i) {
        auto where=ends.find(sm.antiparticle(v[i]));
        if (where==ends.end()) continue;
        auto rest=v;
        rest.erase(rest.begin()+i);
        for (const auto &other:where->second) {
            legs joined=rest;
            joined.insert(joined.end(),other.begin(),other.end());
            result.insert(sorted(std::move(joined)));
        }
    }
    return result;
}

using family = std::vector<const entry*>;
std::vector<family> audit_inventory(const model &sm) {
    multisets cubic,quartic,complete,listed;
    std::map<std::string,multisets> ends;
    for (const auto &v:sm.vertices()) {
        if (v.legs.size()==4) { quartic.insert(sorted(v.legs)); continue; }
        if (v.legs.size()!=3) throw std::runtime_error("Unexpected elementary interaction order");
        cubic.insert(sorted(v.legs));
        for (std::size_t i=0;i<3;++i) {
            auto tail=v.legs;
            tail.erase(tail.begin()+i);
            ends[v.legs[i]].insert(sorted(tail));
        }
    }
    auto bases=attach(cubic,ends,sm);
    bases.insert(quartic.begin(),quartic.end());
    const auto trees=attach(bases,ends,sm);
    for (const auto &tree:trees) {
        multisets choices{{}};
        for (const auto &p:tree) {
            multisets next;
            for (auto partial:choices) {
                partial.push_back(p);
                next.insert(partial);
                if (p!=species(p)) { partial.back()=sm.antiparticle(p); next.insert(partial); }
            }
            choices=std::move(next);
        }
        for (auto v:choices) complete.insert(sorted(std::move(v)));
    }
    std::vector<family> groups;
    std::map<std::string,std::size_t> group_index;
    for (const auto &r:inventory) {
        auto p=split(r.particles), signature=sorted(p);
        if (p.size()!=5 || !listed.insert(signature).second || r.has_tree!=bool(trees.count(signature)))
            throw std::runtime_error("Notebook/tree inventory mismatch for "+r.id);
        legs in{species(p[0]),species(p[1])},out;
        for (std::size_t i=2;i<5;++i) out.push_back(species(sm.antiparticle(p[i])));
        if (in!=split(r.incoming) || out!=split(r.outgoing))
            throw std::runtime_error("Wrong physical crossing for "+r.id);
        auto [it,added]=group_index.emplace(r.family_id,groups.size());
        if (added) groups.emplace_back();
        groups[it->second].push_back(&r);
    }
    if (complete!=listed) throw std::runtime_error("Notebook omits or adds model particle/helicity multisets");
    return groups;
}

std::ofstream output(const fs::path &path) {
    std::ofstream out(path); out.exceptions(std::ios::badbit | std::ios::failbit);
    return out;
}
void save_checks(const fs::path &path,const validation_report &v) {
    auto out=output(path);
    out<<"seed "<<v.seed<<", points "<<v.requested<<"\n";
    for (const auto &r:v.checks)
        out<<r.kind<<'\t'<<r.label<<'\t'<<notebook_output::status(r.status)<<'\t'<<r.samples<<'\t'<<r.detail<<'\n';
    out << std::setprecision(std::numeric_limits<long double>::max_digits10);
    for (const auto &r:v.numerical) {
        out<<r.kind<<'\t'<<notebook_output::status(r.status)<<'\t'<<r.compared<<'\t'<<r.failures
           <<'\t'<<r.max_relative_error<<'\t'<<r.max_absolute_error<<"\treplays="<<r.replays<<"\tmax_replay_rel="<<r.max_replay_relative_error
           <<"\tmax_replay_abs="<<r.max_replay_absolute_error;
        if (r.first_raw_outlier_seed) out<<"\tfirst_raw_outlier_seed="<<*r.first_raw_outlier_seed;
        if (r.first_failure_seed) out<<"\tfirst_failure_seed="<<*r.first_failure_seed;
        out<<'\t'<<r.detail<<'\n';
    }
}

bool run_family(const family &rows,const settings &s,const model &sm,const model &cuts,
                const model &reference,const fs::path &run,std::ostream &ledger) {
    const auto &f=*rows.front();
    const auto dir=run/f.family_id;
    fs::create_directory(dir);
    std::vector<spinas_component> components;
    for (const auto *row:rows) {
        const auto p=split(row->particles);
        const auto stem=dir/row->id;
        std::cout<<"  "<<row->id<<"  "<<row->particles<<std::endl;
        try {
            amplitude process(p,sm,cuts);
            process.set_feynman_model(reference); // construct() consults this only in its final validation stage.
            construct_options opt;
            opt.workers=s.workers;
            opt.validation.points=s.external.points;
            opt.validation.seed=s.external.seed;
            opt.progress=notebook_output::print_stage;
            const auto &amp=process.construct(opt);
            const auto &report=process.last_construction();
            save_checks(stem.string()+".checks.tsv",report.validation);
            const auto &numerical=report.validation.numerical;
            if (std::none_of(numerical.begin(),numerical.end(),[&](const auto& r){
                return r.kind=="Feynman" && r.status==check_outcome::pass && r.compared==s.external.points;
            })) throw std::runtime_error("Missing signed Feynman comparison");
            notebook_output::write_expression(stem,amp);
            std::cout<<"    Complexity "<<amp.complexity()<<", "<<amp.leaf_count()<<" leaves, "
                     <<notebook_output::seconds(report.total_seconds)<<" total. Factorization: "
                     <<process.last_factorization().route<<std::endl;
            if (const auto weight=amp.permutation_summand_complexity())
                std::cout<<"    Complexity of the displayed permutation summand: "<<*weight<<std::endl;
            ledger<<row->id<<'\t'<<f.name<<"\tNATIVE_PASS\t"<<amp.complexity()<<'\t'<<amp.leaf_count()
                  <<'\t'<<report.total_seconds<<"\t\t"<<std::endl;
            notebook_output::print_validation(report.validation);
            components.push_back({p,amp});
        } catch(const std::exception &error) {
            if (const auto *v=dynamic_cast<const construction_validation_error*>(&error))
                save_checks(stem.string()+".checks.tsv",v->report);
            output(stem.string()+".error.txt")<<error.what()<<'\n';
            std::cerr<<"  Unresolved: "<<error.what()<<std::endl;
            ledger<<row->id<<'\t'<<f.name<<"\tUNRESOLVED\t\t\t\t\t"<<std::endl;
        }
    }
    if (components.size()!=rows.size()) {
        ledger<<f.family_id<<'\t'<<f.name<<"\tINCOMPLETE_FAMILY\t\t\t\t\t"<<std::endl;
        return false;
    }
    if (s.native_only) return true;
    spinas_export_options w;
    w.name=f.name; w.spinas_directory=s.spinas; w.runtime_directory=s.external.runtime_directory;
    const auto proj=write_spinas_process(dir/"spinas",sm,components,w);
    if (!proj.complete_helicities || proj.helicities.size()!=f.labeled_helicities)
        throw std::runtime_error("Export is missing labeled helicities");
    write_external_project(proj,sm,s.external);
    auto checked=validate_external_project(proj,s.external);
    auto build=proj.directory/"build";
    for (const auto *name:{"external-configure.log","external-build-calchep.log","external-build-madgraph.log",
         "calchep-comparison.log","calchep-precision.json","calchep-precision.log","calchep-outliers.csv",
         "calchep-chart.csv","madgraph-comparison.log","madgraph-comparison.json"}) {
        if (fs::is_regular_file(build/name)) fs::copy_file(build/name,dir/name);
    }
    ledger<<f.family_id<<'\t'<<f.name<<'\t'<<(checked.passed()?"EXTERNAL_PASS":"EXTERNAL_FAILED")
          <<"\t\t\t\t"<<checked.calchep<<'\t'<<checked.madgraph<<std::endl;
    if (checked.passed()) fs::remove_all(proj.directory);
    return checked.passed();
}
} // namespace

int main(int argc,char **argv) try {
    const auto s=options(argc,argv);
    const model sm(s.root/"models",1,"SM");
    const auto all=audit_inventory(sm);
    std::vector<family> selected;
    bool started=s.start.empty();
    for (const auto &group:all) {
        const auto &f=*group.front();
        if (!s.sector.empty() && f.sector!=s.sector) continue;
        if (s.start==f.family_id || s.start==f.name) started=true;
        if (!started || (!s.family.empty() && s.family!=f.family_id && s.family!=f.name)) continue;
        family matching;
        for (const auto* row : group)
            if (s.case_id.empty() || s.case_id==row->id) matching.push_back(row);
        if (!matching.empty()) selected.push_back(std::move(matching));
    }
    if (selected.empty()) throw std::invalid_argument("No matching five-point family in the selection");
    if (s.list_cases) {
        for (const auto &group : selected) {
            for (const auto *row : group)
                std::cout << row->sector << '\t' << row->name << '\t' << row->id << '\t'
                          << row->particles << '\t' << (row->has_tree ? "tree" : "no tree") << '\n';
        }
        return 0;
    }
    if (s.list) {
        for (const auto &group:selected) {
            const auto &f=*group.front();
            std::cout<<f.sector<<'\t'<<f.name<<'\t'<<f.family_id<<'\t'<<group.size()<<'\t'<<f.labeled_helicities<<'\n';
        }
        return 0;
    }
    const model cuts(s.root/"models",2,"SM"), reference(s.external.calchep_model_directory,1,"Feynman");
    fs::create_directories(s.output);
    auto run=s.output/("run-"+std::to_string(std::chrono::system_clock::now().time_since_epoch().count()));
    if (!fs::create_directory(run)) throw std::runtime_error("Output directory already exists");
    auto ledger=output(run/"results.tsv");
    ledger<<"id\tfamily\tstatus\tcomplexity\tleaves\tseconds\tcalchep_exit\tmadgraph_exit\n";
    std::cout<<"SM five-point construction, "<<selected.size()<<" families, "<<s.external.points
             <<" points per comparison, seed "<<*s.external.seed<<"\nOutput: "<<run<<std::endl;
    std::size_t passed=0;
    for (const auto &group:selected) {
        const auto &f=*group.front();
        std::cout<<"\n"<<f.sector<<"   "<<f.name<<"   "<<f.incoming<<" -> "<<f.outgoing<<std::endl;
        try { passed+=run_family(group,s,sm,cuts,reference,run,ledger); }
        catch(const std::exception &error) {
            output(run/f.family_id/"family-error.txt")<<error.what()<<'\n';
            ledger<<f.family_id<<'\t'<<f.name<<"\tUNRESOLVED_FAMILY\t\t\t\t\t"<<std::endl;
            std::cerr<<error.what()<<std::endl;
        }
    }
    std::cout<<"\n"<<passed<<"/"<<selected.size()<<" families passed "
             <<(s.native_only?"native checks":"native, CalcHEP and MadGraph checks")<<".\n";
    return passed==selected.size()?0:1;
} catch (const std::exception &e) { std::cerr<<e.what()<<'\n'; return 2; }
