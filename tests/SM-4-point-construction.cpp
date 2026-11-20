// C++ counterpart of SM-4-point-construction.nb. From the HEPCAT repository root:
//   cmake -S csource -B tests/results/native-reduce-build -DCMAKE_BUILD_TYPE=Release
//   cmake --build tests/results/native-reduce-build --target hepcat-sm-four-point --parallel
//   tests/results/native-reduce-build/hepcat-sm-four-point
//
// Try one family first by adding --family sm01_uUCc. Use --notebook-only for the
// notebook's 78 families, or run without options to include all flavor assignments.
// Numerical checks use 100,000 phase-space points by default. Set --points N to
// choose a different sample size (minimum 10,000).
// Each run prints its directory under tests/results/spinas-output/SM-4-point.
// Open scores.tsv for the summary or a family directory for its .expr files and
// validation reports. Use --help to set the installed SPINAS, CalcHEP, and MadGraph paths.
#include "amplitude.hpp"
#include "construction-progress.hpp"
#include "external_export.hpp"

#include <algorithm>
#include <charconv>
#include <chrono>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <map>
#include <set>
#include <sstream>

namespace fs = std::filesystem;
using namespace hepcat;

namespace {
struct notebook_case {
    std::string id, family;
    std::vector<std::string> particles, incoming, outgoing;
    std::size_t weight, leaves;
    std::string reference;
};
struct settings {
    fs::path root = fs::current_path(), output, spinas;
    std::string family;
    std::size_t workers = 0;
    bool list = false, construct_only = false, notebook_only = false;
    external_options engines;
};
std::uint64_t integer(const std::string &s) {
    std::uint64_t n = 0;
    auto parsed = std::from_chars(s.data(), s.data() + s.size(), n);
    if (parsed.ec != std::errc{} || parsed.ptr != s.data() + s.size())
        throw std::invalid_argument("Expected a nonnegative integer, got " + s);
    return n;
}
std::vector<std::string> split(const std::string &s, char separator) {
    std::istringstream in(s);
    std::vector<std::string> fields;
    for (std::string item; std::getline(in, item, separator);)
        fields.push_back(item);
    return fields;
}
settings options(int argc, char **argv) {
    settings s;
    s.engines.points = 100000;
    for (int i = 1; i < argc; ++i) {
        std::string arg = argv[i];
        if (arg == "--help") {
            std::cout << "hepcat-sm-four-point [--root DIR] [--output DIR] [--family NAME] [--list]\n"
                         "  [--notebook-only] [--construct-only] [--workers N] [--points N] [--seed N]\n"
                         "  [--spinas DIR] [--calchep DIR] [--madgraph DIR] [--python EXE] [--cmake EXE]\n"
                         "  Numerical validation defaults to 100,000 points (minimum 10,000).\n";
            std::exit(0);
        }
        if (arg == "--list") {
            s.list = true;
            continue;
        }
        if (arg == "--notebook-only") {
            s.notebook_only = true;
            continue;
        }
        if (arg == "--construct-only") {
            s.construct_only = true;
            continue;
        }
        if (i + 1 == argc)
            throw std::invalid_argument("Missing value for " + arg);
        std::string v = argv[++i];
        if (arg == "--root")
            s.root = v;
        else if (arg == "--output")
            s.output = v;
        else if (arg == "--family")
            s.family = v;
        else if (arg == "--workers")
            s.workers = integer(v);
        else if (arg == "--points")
            s.engines.points = integer(v);
        else if (arg == "--seed")
            s.engines.seed = integer(v);
        else if (arg == "--spinas")
            s.spinas = v;
        else if (arg == "--calchep")
            s.engines.calchep_directory = v;
        else if (arg == "--madgraph")
            s.engines.madgraph_directory = v;
        else if (arg == "--python")
            s.engines.python_executable = v;
        else if (arg == "--cmake")
            s.engines.cmake_executable = v;
        else
            throw std::invalid_argument("Unknown option " + arg);
    }
    s.root = fs::absolute(s.root).lexically_normal();
    if (s.engines.points < 10000)
        throw std::invalid_argument("Use at least 10,000 validation points");
    if (!s.engines.seed)
        s.engines.seed = std::chrono::system_clock::now().time_since_epoch().count();
    if (s.output.empty())
        s.output = s.root / "tests/results/spinas-output/SM-4-point";
    s.output = fs::absolute(s.output);
    if (s.spinas.empty())
        s.spinas = s.root.parent_path() / "spinas";
    if (s.engines.calchep_directory.empty())
        s.engines.calchep_directory = s.root.parent_path() / "CalcHEP/calchep_3.9.2";
    if (s.engines.madgraph_directory.empty())
        s.engines.madgraph_directory = s.root.parent_path() / "MadGraph";
    if (s.engines.python_executable == "python3") {
        std::ifstream launcher(s.engines.madgraph_directory / "bin/mg5_aMC");
        std::string line;
        if (std::getline(launcher, line) && line.rfind("#!/", 0) == 0) {
            fs::path interpreter(line.substr(2));
            if (interpreter.filename().string().rfind("python", 0) == 0 && fs::is_regular_file(interpreter))
                s.engines.python_executable = interpreter.string();
        }
    }
    s.engines.runtime_directory = s.root / "source/internal/export";
    s.engines.template_directory = s.root / "csource/external_templates";
    s.engines.calchep_model_directory = s.root / "tests/reference-models/calchep";
    s.engines.calchep_model_number = 1;
    s.engines.madgraph_model = (s.root / "tests/reference-models/madgraph/sm").string();
    s.engines.madgraph_parameter_rules["lamWS"] = 0;
    s.engines.progress = notebook_output::print_stage;
    return s;
}
std::vector<notebook_case> read_notebook(const fs::path &path, std::size_t cases, std::size_t groups) {
    std::ifstream in(path);
    std::string line;
    if (!std::getline(in, line) ||
        line != "case_id\tfamily\tparticles\tincoming\toutgoing\tnotebook_weight\tnotebook_leaves\texpression")
        throw std::runtime_error("Invalid notebook catalog " + path.string());
    std::vector<notebook_case> rows;
    std::set<std::string> ids, families;
    while (std::getline(in, line)) {
        auto c = split(line, '\t');
        if (c.size() != 8 || !ids.insert(c[0]).second)
            throw std::runtime_error("Invalid notebook row");
        auto legs = split(c[2], ','), incoming = split(c[3], ','), outgoing = split(c[4], ',');
        if (legs.size() != 4 || incoming.size() != 2 || outgoing.size() != 2)
            throw std::runtime_error("Invalid external legs in " + c[0]);
        families.insert(c[1]);
        rows.push_back({c[0], c[1], legs, incoming, outgoing, integer(c[5]), integer(c[6]), c[7]});
    }
    if (rows.size() != cases || families.size() != groups)
        throw std::runtime_error("Incomplete notebook inventory");
    return rows;
}
const char *label(check_outcome s) {
    if (s == check_outcome::pass)
        return "PASS";
    return s == check_outcome::fail ? "FAIL" : "UNRESOLVED";
}
void checks(std::ostream &out, const validation_report &r) {
    out << "seed " << r.seed << "  requested " << r.requested << '\n';
    for (const auto &c : r.checks)
        out << label(c.status) << '\t' << c.kind << '\t' << c.label << '\t' << c.samples << '\t' << c.detail
            << '\n';
    for (const auto &n : r.numerical) {
        out << label(n.status) << '\t' << n.kind << '\t' << n.compared << " points, " << n.failures
            << " mismatches, max relative " << n.max_relative_error << ", max absolute " << n.max_absolute_error
            << ", precision replays " << n.replays << ", max replay absolute " << n.max_replay_absolute_error;
        if (n.first_failure_seed)
            out << ", first failed point seed " << *n.first_failure_seed;
        out << '\t' << n.detail << '\n';
    }
}
std::ofstream output(const fs::path &path) {
    std::ofstream stream(path);
    stream.exceptions(std::ios::failbit | std::ios::badbit);
    return stream;
}
std::string species(std::string particle) {
    auto helicity = particle.find('.');
    return particle.substr(0, helicity);
}
std::string bind_scope(std::string text, const std::string &scope) {
    const std::string marker = "\"NOTEBOOK\"", replacement = "\"" + scope + "\"";
    for (std::size_t pos = 0; (pos = text.find(marker, pos)) != std::string::npos; pos += replacement.size())
        text.replace(pos, marker.size(), replacement);
    return text;
}

bool run_family(const std::vector<notebook_case> &family, const model &sm, const model &feynman,
                const settings &s, const fs::path &directory, std::ostream &scores) {
    const auto &name = family.front().family;
    fs::create_directories(directory);
    auto log = output(directory / "checks.txt");
    std::vector<spinas_component> generated;
    bool simple = true;
    std::size_t case_number = 0;
    for (const auto &c : family) {
        std::cout << "  Amplitude " << ++case_number << '/' << family.size() << "  " << c.id
                  << "  (all incoming: " << c.particles[0] << ", " << c.particles[1] << ", "
                  << c.particles[2] << ", " << c.particles[3] << ')' << std::endl;
        log << c.id << '\n';
        try {
            for (unsigned i = 0; i < 2; ++i)
                if (species(c.particles[i]) != c.incoming[i] ||
                    species(sm.antiparticle(c.particles[i + 2])) != c.outgoing[i])
                    throw std::runtime_error("Physical crossing disagrees with the notebook");
            // The same elementary model supplies both vertices and the two sides of a four-point cut.
            amplitude amp(c.particles, sm, sm, feynman);
            construct_options opts;
            opts.progress = notebook_output::print_stage;
            opts.workers = s.workers;
            opts.validation.points = s.engines.points;
            opts.validation.seed = s.engines.seed;
            // construct uses this only in the last, signed numerical validation.
            opts.reference_amplitude.emplace(bind_scope(c.reference, sm.parameter_scope()),
                                             amp.external_masses());
            if (opts.reference_amplitude->complexity() != c.weight ||
                opts.reference_amplitude->leaf_count() != c.leaves)
                throw std::runtime_error("Reference expression disagrees with the notebook complexity record");
            auto result = amp.construct(opts);
            const auto &report = amp.last_construction();
            checks(log, report.validation);
            notebook_output::print_validation(report.validation);
            const auto weight = result.complexity();
            const auto leaves = result.leaf_count();
            const bool within = weight <= c.weight && leaves <= c.leaves;
            simple &= within;
            output(directory / (c.id + ".expr")) << result.str() << '\n';
            scores << c.id << '\t' << name << '\t' << (within ? "PASS" : "VALID_BUT_LONGER") << '\t' << weight
                   << '\t' << c.weight << '\t' << leaves << '\t' << c.leaves << '\t'
                   << report.total_seconds << '\n'
                   << std::flush;
            std::cout << "    " << (within ? "PASS" : "FAIL") << " notebook size comparison: complexity "
                      << weight << " (notebook " << c.weight << "), leaves " << leaves << " (notebook "
                      << c.leaves << "). Construction and validation took " << report.total_seconds << " s.\n"
                      << "    Expression: " << (directory / (c.id + ".expr")).string() << std::endl;
            generated.push_back({c.particles, std::move(result)});
        } catch (const std::exception &e) {
            scores << c.id << '\t' << name << "\tUNRESOLVED\t\t" << c.weight << "\t\t" << c.leaves << "\t\n"
                   << std::flush;
            if (auto error = dynamic_cast<const construction_validation_error *>(&e)) {
                checks(log, error->report);
                notebook_output::print_validation(error->report);
            }
            log << e.what() << '\n';
            std::cerr << "    UNRESOLVED " << c.id << ": " << e.what() << '\n';
        }
        log.flush();
    }
    if (generated.size() != family.size()) {
        if (!s.construct_only) {
            scores << "external\t" << name << "\tINCOMPLETE_FAMILY\t\t\t\t\t\n" << std::flush;
            std::cout << "  Skipping CalcHEP and MadGraph: " << generated.size() << '/' << family.size()
                      << " amplitudes available. See " << (directory / "checks.txt").string() << std::endl;
        }
        return false;
    }
    if (s.construct_only)
        return simple;
    spinas_export_options write;
    write.name = name;
    write.spinas_directory = fs::absolute(s.spinas);
    write.runtime_directory = s.engines.runtime_directory;
    // Each invocation owns a fresh run directory. Preserve comparison receipts before discarding builds.
    notebook_output::print_stage("Exporting SPINAS and writing the independent validation projects");
    auto project = write_spinas_process(directory / "spinas", sm, generated, write);
    write_external_project(project, sm, s.engines);
    const auto ext = validate_external_project(project, s.engines);
    auto build = project.directory / "build";
    for (const auto *filename :
         {"external-configure.log", "external-build-calchep.log", "external-build-madgraph.log",
          "calchep-comparison.log", "calchep-precision.json", "calchep-precision.log", "calchep-outliers.csv",
          "calchep-chart.csv",
          "madgraph-comparison.log", "madgraph-comparison.json"})
        if (fs::is_regular_file(build / filename))
            fs::copy_file(build / filename, directory / filename);
    log << "External configure " << ext.configuration << ", CalcHEP " << ext.calchep << ", MadGraph "
        << ext.madgraph << '\n';
    scores << "external\t" << name << '\t' << (ext.passed() ? "PASS" : "FAILED") << "\t\t\t\t\t\n"
           << std::flush;
    if (ext.passed())
        fs::remove_all(project.directory);
    return simple && ext.passed();
}
} // namespace
int main(int argc, char **argv) {
    try {
        const auto s = options(argc, argv);
        const auto inventory = s.root / "tests/benchmarks/native-sm-four-point";
        auto rows = read_notebook(inventory / "catalog.tsv", 138, 78);
        if (!s.notebook_only) {
            auto closure = read_notebook(inventory / "flavor-closure.tsv", 254, 180);
            rows.insert(rows.end(), closure.begin(), closure.end());
        }
        std::map<std::string, std::vector<notebook_case>> selected;
        std::size_t cases = 0;
        for (const auto &c : rows)
            if (s.family.empty() || c.family == s.family) {
                selected[c.family].push_back(c);
                ++cases;
            }
        if (selected.empty())
            throw std::invalid_argument("Unknown notebook family " + s.family);
        if (s.list) {
            for (const auto &[name, group] : selected)
                std::cout << name << '\t' << group.size() << '\n';
            return 0;
        }
        const auto start = std::chrono::steady_clock::now();
        fs::create_directories(s.output);
        const auto run = "run-" + std::to_string(std::chrono::system_clock::now().time_since_epoch().count());
        auto dir = s.output / run;
        for (unsigned suffix = 1; !fs::create_directory(dir); ++suffix)
            dir = s.output / (run + "-" + std::to_string(suffix));
        std::cout << "SM four-point construction: " << cases << (cases == 1 ? " amplitude in " : " amplitudes in ")
                  << selected.size() << (selected.size() == 1 ? " family" : " families")
                  << ". Seed " << *s.engines.seed << ", " << s.engines.points
                  << " phase-space points per numerical comparison.\nOutput: " << dir.string() << std::endl;
        if (s.construct_only)
            std::cout << "Running native checks only (--construct-only). CalcHEP and MadGraph are skipped."
                      << std::endl;
        notebook_output::print_stage("Loading the elementary SM and the independent Feynman model");
        const model sm(s.root / "models", 1, "SM"), feynman(s.engines.calchep_model_directory, 1, "Feynman");
        auto scores = output(dir / "scores.tsv");
        scores << "case\tfamily\tstatus\tweight\tnotebook_weight\tleaves\tnotebook_leaves\tseconds\n";
        std::size_t family_number = 0, passed = 0;
        for (const auto &[name, group] : selected) {
            const auto &process = group.front();
            std::cout << "\nFamily " << ++family_number << '/' << selected.size() << "  " << name << "  "
                      << process.incoming[0] << ' ' << process.incoming[1] << " -> " << process.outgoing[0]
                      << ' ' << process.outgoing[1] << std::endl;
            try {
                const bool ok = run_family(group, sm, feynman, s, dir / name, scores);
                passed += ok;
                std::cout << (ok ? "PASS " : "FAIL ") << name << "  Reports: " << (dir / name).string()
                          << std::endl;
            } catch (const std::exception &e) {
                std::cerr << "UNRESOLVED " << name << ": " << e.what() << '\n';
                scores << "external\t" << name << "\tUNRESOLVED\t\t\t\t\t\n";
                output(dir / name / "error.txt") << e.what() << '\n';
            }
        }
        const bool ok = passed == selected.size();
        const auto seconds = std::chrono::duration<double>(std::chrono::steady_clock::now() - start).count();
        std::cout << '\n' << (ok ? "PASS" : "FAIL") << "  Families passed: " << passed << '/' << selected.size()
                  << " (" << (s.construct_only ? "native checks and size comparisons"
                                               : "native checks, size comparisons, CalcHEP and MadGraph")
                  << "). Total time: " << seconds << " s.\nSummary: " << (dir / "scores.tsv").string()
                  << std::endl;
        return ok ? 0 : 1;
    } catch (const std::exception &e) {
        std::cerr << e.what() << '\n';
        return 2;
    }
}
