// Native counterpart of QED-6-point-construction.nb. Run with --help or --list.
#include "QED-all-positive-replay.hpp"
#include "amplitude.hpp"
#include "external_export.hpp"
#include "generation_sew.hpp"
#include <algorithm>
#include <chrono>
#include <fstream>
#include <iostream>
#include <set>
#include <sstream>

namespace fs = std::filesystem;
using namespace hepcat;
namespace {
struct example {
    std::string helicities;         // Photon helicities in the all-incoming HEPCAT convention.
    std::size_t physical, spurious; // Measured notebook complexity (including its final worked forms).
};
struct family {
    std::string name;
    std::vector<std::string> fermions;
    std::vector<example> examples;
};
const std::vector<family> notebook = {
    {"eeAA", {"e", "E"}, {{"++", 1, 1}, {"+-", 10, 10}, {"-+", 10, 10}, {"--", 1, 1}}},
    {"eeee", {"e", "E", "E", "e"}, {{"", 8, 8}}},
    {"eeAAA", {"e", "E"}, {{"+++", 27, 30}, {"++-", 71, 117}, {"+--", 71, 117}, {"---", 27, 30}}},
    {"eeeeA", {"e", "E", "E", "e"}, {{"+", 160, 160}, {"-", 160, 160}}},
    {"eeAAAA",
     {"e", "E"},
     {{"++++", 108, 120},
      {"+++-", 1173, 528},
      // Re-evaluating the notebook's literal qed6PPMM gives 2238 with its current score.
      {"++--", 2238, 1044},
      {"+---", 327, 528},
      {"----", 108, 120}}},
    {"eeeeAA", {"e", "E", "E", "e"}, {{"++", 2592, 1216}, {"+-", 2576, 2272}, {"--", 2592, 1216}}},
    {"eeeeee", {"e", "E", "e", "E", "e", "E"}, {{"", 3960, 3168}}},
    {"eeAAAAA", {"e", "E"}, {{"+++++", 820, 0}}}};
struct settings {
    fs::path root = fs::current_path(), output, spinas;
    external_options engines;
    std::string family_filter, preference = "both", helicity_filter;
    bool list = false, construct_only = false, formula_only = false, no_formulas = false;
    std::optional<unsigned> photons;
    std::size_t workers = 0;
    std::uint64_t seed = std::chrono::high_resolution_clock::now().time_since_epoch().count();
};

void usage() {
    std::cout
        << "hepcat-qed [--root HEPCAT] [--output DIRECTORY] [--seed N] [--points N>=10000]\n"
           "  --list                         Show the notebook cases without calculating them.\n"
           "  --family eeAA|eeee|eeAAA|eeeeA|eeAAAA|eeeeAA|eeeeee|eeAAAAA\n"
           "  --helicity ++-                 Select a notebook cell (construction only).\n"
           "  --preference physical|spurious|both   --workers N\n"
           "  --construct-only               Run constructive, Feynman and signed reference checks.\n"
           "  --formulas-only [--photons N]  Compare both all-positive predictions with MadGraph "
           "(N=3..8).\n"
           "  --skip-formulas                Omit that separate formula study.\n"
           "  --spinas DIR --calchep DIR --madgraph DIR [--python EXE] [--cmake EXE]\n"
           "  --python overrides the interpreter in MadGraph's launcher (fallback: python3 on PATH).\n"
           "Every complete family is exported and compared with both engines by default.\n"
           "One run directory contains expression files, scores, check counts and generated numerical "
           "projects.\n";
}
unsigned long long integer_option(const std::string& value) {
    if (value.empty() || value.find_first_not_of("0123456789") != value.npos)
        throw std::invalid_argument("expected a nonnegative integer, got " + value);
    return std::stoull(value);
}
settings options(int argc, char** argv) {
    settings s;
    bool explicit_python = false;
    for (int i = 1; i < argc; ++i) {
        std::string key = argv[i];
        if (key == "--help") {
            usage();
            std::exit(0);
        }
        if (key == "--list") {
            s.list = true;
            continue;
        }
        if (key == "--construct-only") {
            s.construct_only = true;
            continue;
        }
        if (key == "--formulas-only") {
            s.formula_only = true;
            continue;
        }
        if (key == "--skip-formulas") {
            s.no_formulas = true;
            continue;
        }
        if (++i == argc)
            throw std::invalid_argument("missing argument for " + key);
        std::string v = argv[i];
        if (key == "--root")
            s.root = v;
        else if (key == "--output")
            s.output = v;
        else if (key == "--workers")
            s.workers = integer_option(v);
        else if (key == "--points")
            s.engines.points = integer_option(v);
        else if (key == "--seed")
            s.seed = integer_option(v);
        else if (key == "--family")
            s.family_filter = v;
        else if (key == "--helicity")
            s.helicity_filter = v;
        else if (key == "--preference")
            s.preference = v;
        else if (key == "--spinas")
            s.spinas = v;
        else if (key == "--madgraph")
            s.engines.madgraph_directory = v;
        else if (key == "--calchep")
            s.engines.calchep_directory = v;
        else if (key == "--python") {
            s.engines.python_executable = v;
            explicit_python = true;
        } else if (key == "--cmake")
            s.engines.cmake_executable = v;
        else if (key == "--photons") {
            auto n = integer_option(v);
            if (n < 3 || n > 8)
                throw std::invalid_argument("formula multiplicity must be 3..8");
            s.photons = n;
        } else
            throw std::invalid_argument("unknown option " + key);
    }
    if (s.preference != "physical" && s.preference != "spurious" && s.preference != "both")
        throw std::invalid_argument("pole preference must be physical, spurious or both");
    if (s.engines.points < 10000)
        throw std::invalid_argument("at least 10000 comparison points are required");
    if (!s.family_filter.empty() && std::none_of(notebook.begin(), notebook.end(), [&](const family& f) {
            return f.name == s.family_filter;
        }))
        throw std::invalid_argument("unknown notebook family " + s.family_filter);
    if (!s.helicity_filter.empty()) {
        if (s.family_filter.empty() || !s.construct_only)
            throw std::invalid_argument("--helicity requires --family and --construct-only");
        for (auto& f : notebook)
            if (f.name == s.family_filter &&
                std::none_of(f.examples.begin(), f.examples.end(),
                             [&](auto& e) { return e.helicities == s.helicity_filter; }))
                throw std::invalid_argument("helicity is not a notebook cell for " + f.name);
    }
    if (s.formula_only && (s.no_formulas || s.construct_only || !s.family_filter.empty()))
        throw std::invalid_argument(
            "--formulas-only conflicts with the requested construction selection");
    s.root = fs::absolute(s.root).lexically_normal();
    if (s.output.empty())
        s.output = s.root / "tests/results/spinas-output/native-qed";
    s.output = fs::absolute(s.output).lexically_normal();
    auto sibling = s.root.parent_path();
    if (s.spinas.empty())
        s.spinas = sibling / "spinas";
    if (s.engines.calchep_directory.empty())
        s.engines.calchep_directory = sibling / "CalcHEP/calchep_3.9.2";
    if (s.engines.madgraph_directory.empty())
        s.engines.madgraph_directory = sibling / "MadGraph";
    s.spinas = fs::absolute(s.spinas);
    s.engines.calchep_directory = fs::absolute(s.engines.calchep_directory);
    s.engines.madgraph_directory = fs::absolute(s.engines.madgraph_directory);
    if (!explicit_python && !s.construct_only && !s.list) {
        std::ifstream launcher(s.engines.madgraph_directory / "bin/mg5_aMC");
        std::string line;
        if (std::getline(launcher, line) && line.rfind("#!/", 0) == 0) {
            std::string interpreter = line.substr(2);
            if (fs::path(interpreter).filename().string().rfind("python", 0) == 0 &&
                fs::is_regular_file(interpreter))
                s.engines.python_executable = interpreter;
        }
    }
    s.engines.runtime_directory = s.root / "source/internal/export";
    s.engines.calchep_model_directory = s.root / "tests/reference-models/calchep";
    s.engines.calchep_model_number = 2;
    s.engines.madgraph_model = "sm-full";
    s.engines.allowed_pdgs = {11, 22};
    s.engines.seed = s.seed;
    return s;
}

std::vector<std::string> process(const family& f, const example& e) {
    auto legs = f.fermions;
    for (char h : e.helicities)
        legs.emplace_back(h == '+' ? "A.+" : "A.-");
    return legs;
}

// Model 4/5 contains one representative per photon permutation. Bind its slots
// in the requested order, preserving the order of identical external fermions.
amp_expression catalog_reference(const model& catalog, const std::vector<std::string>& legs) {
    for (std::size_t i = 0; i < catalog.vertices().size(); ++i) {
        const auto& entry = catalog.vertices()[i];
        if (!std::is_permutation(legs.begin(), legs.end(), entry.legs.begin(), entry.legs.end()))
            continue;
        std::set<int> used;
        std::vector<int> slots;
        for (auto& particle : entry.legs) {
            for (std::size_t k = 0; k < legs.size(); ++k)
                if (legs[k] == particle && used.insert(int(k + 1)).second) {
                    slots.push_back(int(k + 1));
                    break;
                }
        }
        Algebra a(&catalog.algebra());
        std::vector<std::string> masses;
        for (auto& leg : legs)
            masses.push_back(catalog.particle(leg).mass);
        generation::term row{{{i, slots}}, {}, 1};
        auto value = generation::sew(a, catalog, legs, {{row}}, false);
        return amp_expression(a.format(value), masses);
    }
    throw std::runtime_error("missing signed catalog reference for requested external legs");
}

std::ofstream file(const fs::path& path) {
    std::ofstream stream(path);
    stream.exceptions(std::ios::badbit | std::ios::failbit);
    return stream;
}
void list() {
    for (auto& f : notebook) {
        std::cout << f.name << "  ";
        for (auto& e : f.examples)
            std::cout << (e.helicities.empty() ? "massive" : e.helicities) << " (" << e.physical << "/"
                      << (e.spurious ? std::to_string(e.spurious) : "formula") << ")  ";
        std::cout << '\n';
    }
    std::cout << "Scores are AllPhysical/AllSpurious. All-positive formula comparisons use 3 through 8 "
                 "photons.\n";
}
void print_checks(std::ostream& out, const validation_report& report) {
    for (const auto& row : report.checks)
        out << row.kind << ' ' << row.label << ' '
            << (row.status == check_outcome::pass   ? "PASS"
                : row.status == check_outcome::fail ? "FAIL"
                                                    : "UNRESOLVED")
            << ' ' << row.detail << '\n';
    for (const auto& row : report.numerical)
        out << row.kind << " points=" << row.compared << " failures=" << row.failures
            << " raw_max_relative=" << row.max_relative_error << " replays=" << row.replays
            << " replay_max_relative=" << row.max_replay_relative_error << " " << row.detail << '\n';
}
std::string clean_cell(std::string text) {
    for (char& c : text)
        if (c == '\n' || c == '\r' || c == '\t')
            c = ' ';
    return text;
}

bool construct_family(const settings& s, const model& vertices, const model& catalog, const model& rules,
                      const family& f, bool spurious, std::ostream& scores) {
    std::string policy = spurious ? "spurious" : "physical";
    auto directory = s.output / policy / f.name;
    fs::create_directories(directory);
    auto cells = file(directory / "checks.txt");
    std::vector<spinas_component> rows;
    bool passed = true;
    for (auto& e : f.examples) {
        if (!s.helicity_filter.empty() && e.helicities != s.helicity_filter)
            continue;
        auto legs = process(f, e);
        std::string branch = e.helicities.empty() ? "massive" : e.helicities;
        std::cout << policy << ' ' << f.name << ' ' << branch << '\n' << std::flush;
        auto baseline = spurious ? e.spurious : e.physical;
        try {
            const bool seven = legs.size() == 7;
            auto ref = seven ? qed_notebook::all_positive_reference(vertices, 5).expression(spurious)
                             : catalog_reference(catalog, legs);
            // The catalog contains the final hand simplified representation for
            // the notebook cells that explicitly replace their automatic result.
            auto catalog_score = ref.complexity();
            auto target = baseline ? std::min(baseline, catalog_score) : catalog_score;
            const model& cuts = legs.size() == 4 ? vertices : catalog;
            amplitude amp(legs, vertices, cuts, rules);
            construct_options opts;
            opts.workers = s.workers;
            opts.pole_preference =
                spurious ? generation_references::cyclic : generation_references::physical;
            opts.validation.seed = s.seed;
            opts.validation.points = s.engines.points;
            opts.reference_amplitude = ref;
            auto result = amp.construct(opts);
            std::optional<std::size_t> complexity;
            try {
                complexity = result.complexity();
            } catch (const std::length_error& err) {
                cells << branch << " complexity unavailable: " << err.what() << '\n';
            } catch (const std::overflow_error& err) {
                cells << branch << " complexity unavailable: " << err.what() << '\n';
            }
            cells << branch << " signed reference and Feynman comparisons\n";
            print_checks(cells, amp.last_construction().validation);
            file(directory / (branch + ".expr")) << result.str() << '\n';
            bool simple = complexity && *complexity <= target;
            const auto measured = complexity ? std::to_string(*complexity) : "UNMEASURED";
            scores << policy << '\t' << f.name << '\t' << branch << '\t'
                   << (simple       ? "PASS"
                       : complexity ? "VALID_BUT_LONGER"
                                    : "VALID_SCORE_UNAVAILABLE")
                   << '\t' << measured << '\t' << target << '\t' << catalog_score << '\t'
                   << s.engines.points << '\t' << amp.last_construction().total_seconds << "\t\n"
                   << std::flush;
            std::cout << "  validated " << s.engines.points << " points; complexity " << measured
                      << " vs " << target << '\n';
            if (!simple && catalog_score <= target) {
                // The author selected shorter verified forms after construction
                // in the notebook. Keep that step visible, with its own checks.
                auto verified = amp.validate(ref, opts.validation);
                cells << branch << " worked representative, independently checked\n";
                print_checks(cells, verified);
                if (!verified.passed())
                    throw std::runtime_error("worked representative failed its own validation");
                file(directory / (branch + ".selected.expr")) << ref.str() << '\n';
                scores << policy << '\t' << f.name << '\t' << branch << "\tWORKED_FORM_PASS\t"
                       << catalog_score << '\t' << target << '\t' << catalog_score << '\t'
                       << s.engines.points << "\t\tvalidated after construction\n"
                       << std::flush;
                rows.push_back({std::move(legs), std::move(ref)});
            } else {
                passed &= simple;
                rows.push_back({std::move(legs), std::move(result)});
            }
        } catch (const std::exception& err) {
            scores << policy << '\t' << f.name << '\t' << branch << "\tUNRESOLVED\t\t" << baseline
                   << "\t\t\t\t" << clean_cell(err.what()) << '\n'
                   << std::flush;
            if (auto failure = dynamic_cast<const construction_validation_error*>(&err))
                print_checks(cells, failure->report);
            std::cerr << "  " << err.what() << '\n';
            passed = false;
        }
    }
    if (s.construct_only)
        return passed;
    if (rows.size() != f.examples.size()) {
        std::cerr << "  Cannot export a complete " << f.name
                  << " helicity sum: a construction failed.\n";
        return false;
    }
    spinas_export_options write;
    write.name = f.name;
    write.spinas_directory = s.spinas;
    write.runtime_directory = s.engines.runtime_directory;
    auto backends = s.engines;
    if (f.name == "eeAAAAA") {
        // The notebook has only the all-positive five-photon row. CalcHEP
        // sums over all photon polarizations; that observable needs 32 rows.
        backends.run_calchep = false;
        backends.calchep_directory.clear();
        backends.calchep_model_directory.clear();
        for (std::size_t i = 2; i < 7; ++i)
            write.fixed_helicities[i] = 2;
    }
    auto project = write_spinas_process(directory / "spinas", vertices, rows, write);
    write_external_project(project, vertices, backends);
    const auto ext = validate_external_project(project, backends);
    if (backends.run_calchep)
        cells << "CalcHEP summed report " << ext.calchep_report << " exit=" << ext.calchep << '\n';
    else
        cells << "CalcHEP unpolarized comparison requires the full helicity family.\n";
    cells << "MadGraph polarized and summed report " << ext.madgraph_report << " exit=" << ext.madgraph
          << '\n';
    return ext.passed() && passed;
}

// MadGraph supplies its own generator and Fortran matrix-element evaluator.
// All physics expressions, kinematics and comparisons on the HEPCAT side are C++.
std::string json_string(const std::string& value) {
    std::string s = "\"";
    for (char c : value) {
        if (static_cast<unsigned char>(c) < 0x20)
            throw std::invalid_argument("control character in MadGraph path");
        if (c == '"' || c == '\\')
            s += '\\';
        s += c;
    }
    return s + '"';
}
std::string contents(const fs::path& p) {
    std::ifstream in(p);
    if (!in)
        throw std::runtime_error("missing result file " + p.string());
    return {std::istreambuf_iterator<char>(in), {}};
}
void command(std::vector<std::string> args, const fs::path& log) {
    if (run_external_command(args, log) != 0)
        throw std::runtime_error("command failed; see " + log.string());
}
void formula_study(const settings& s) {
    auto out = s.output / "all-positive";
    fs::create_directories(out);
    auto build = out / "build";
    const auto cmake = s.engines.cmake_executable;
    command({cmake, "-S", (s.root / "tests/benchmarks/all-positive").string(), "-B", build.string(),
             "-DCMAKE_BUILD_TYPE=Release", "-DSPINAS_ROOT=" + s.spinas.string()},
            out / "configure.log");
    command({cmake, "--build", build.string(), "--target", "qed-all-positive"}, out / "compile.log");
    for (unsigned n = 3; n <= 8; ++n) {
        if (s.photons && n != *s.photons)
            continue;
        std::cout << "Independent MadGraph comparison of both all-positive formulas, N=" << n << '\n'
                  << std::flush;
        auto dir = out / ("N" + std::to_string(n));
        fs::create_directories(dir);
        auto config = dir / "config.json", reference = dir / "madgraph";
        // These mass and charge values match the direct formula studies,
        // which test the relation beyond the constructive catalog.
        double mass = n == 8 ? 1.5 : 1.0;
        {
            auto data = file(config);
            data << "{\"MadGraphDirectory\":" << json_string(s.engines.madgraph_directory.string())
                 << ",\"MadGraphModel\":" << json_string(s.engines.madgraph_model)
                 << ",\"AllowedParticlePDGs\":[11,22],\"PhysicalPDGs\":[11,-11";
            for (unsigned i = 0; i < n; ++i)
                data << ",22";
            data << "],\"Masses\":[" << mass << ',' << mass;
            for (unsigned i = 0; i < n; ++i)
                data << ",0";
            data << "],\"Parameters\":[{\"Name\":\"Me\",\"Value\":" << mass
                 << "},{\"Name\":\"EE\",\"Value\":0.3333333333333333}],"
                 << "\"ParameterMap\":{},\"ParameterRules\":{},\"ZeroWidths\":true,"
                    "\"OptimizationLevel\":0,"
                 << "\"TimeConstraint\":7200,\"JampSplit\":" << (n >= 7 ? 128 : 0) << "}";
        }
        command({s.engines.python_executable,
                 (s.engines.runtime_directory / "madgraph-runtime.py").string(), "--config",
                 config.string(), "--output", reference.string()},
                dir / "madgraph.log");
        auto library = reference / "libhepcat_madgraph.so";
        if (!fs::exists(library))
            library = reference / "libhepcat_madgraph.dylib";
        auto status =
            run_external_command({(build / "qed-all-positive").string(), "--photons", std::to_string(n),
                                  "--points", std::to_string(s.engines.points), "--seed",
                                  std::to_string(s.seed), "--mass", std::to_string(mass), "--reference",
                                  library.string(), "--output", (dir / "check").string()},
                                 dir / "comparison.log");
        // Some Fortran runtimes can terminate a caller with STOP 0. Require a
        // fresh, complete numerical receipt as well as the child exit code.
        auto receipt = contents(dir / "check.json");
        for (const auto& token : std::vector<std::string>{
                 "\"Points\":" + std::to_string(s.engines.points) + ",",
                 "\"Photons\":" + std::to_string(n) + ",", "\"Seed\":" + std::to_string(s.seed) + ","})
            if (receipt.find(token) == receipt.npos)
                throw std::runtime_error("incomplete formula comparison at N=" + std::to_string(n));
        if (status == 1 && receipt.find(R"("Status":"FAIL",)") != receipt.npos) {
            const model vertices(s.root / "models", 3);
            auto count = qed_notebook::replay_formulas(vertices, n, dir / "check", s.engines.points, mass);
            if (receipt.find("\"Failures\":" + std::to_string(count) + ",") == receipt.npos)
                throw std::runtime_error("raw outlier count and precision replay disagree");
            file(dir / "outcome.txt") << "PASS " << s.engines.points << " points, " << count
                                      << " checked at 160 bits\n";
        } else if (status == 0 && receipt.find(R"("Status":"PASS",)") != receipt.npos &&
                   receipt.find(R"("Failures":0,)") != receipt.npos) {
            file(dir / "outcome.txt") << "PASS " << s.engines.points << " points at native precision\n";
        } else {
            throw std::runtime_error("failed or incomplete formula comparison: " + dir.string());
        }
        // Formula references can occupy gigabytes at N=8. Keep the config,
        // independent receipt, values, points and logs for this run.
        fs::copy_file(reference / "result.json", dir / "reference.json");
        fs::remove_all(reference);
    }
}
} // namespace

int main(int argc, char** argv) {
    try {
        auto s = options(argc, argv);
        if (s.list) {
            list();
            return 0;
        }
        if (!fs::is_regular_file(s.root / "models/amps3.mdl"))
            throw std::runtime_error("specify the HEPCAT checkout with --root");
        fs::create_directories(s.output);
        auto now = std::chrono::steady_clock::now().time_since_epoch().count();
        s.output /= "run-" + std::to_string(s.seed) + "-" + std::to_string(now);
        if (!fs::create_directory(s.output))
            throw std::runtime_error("run directory already exists");
        std::cout << "Seed " << s.seed << "\nResults " << s.output << '\n' << std::flush;
        bool all = true;
        if (!s.formula_only) {
            const model vertices(s.root / "models", 3), rules(s.engines.calchep_model_directory, 2);
            auto scores = file(s.output / "construction.tsv");
            scores << "preference\tfamily\thelicity\tstatus\tcpp_complexity\tthreshold\treference_"
                      "complexity\tpoints\tseconds\tdetail\n";
            for (bool spurious : {false, true}) {
                if ((spurious && s.preference == "physical") ||
                    (!spurious && s.preference == "spurious"))
                    continue;
                const model catalog(s.root / "models", spurious ? 5 : 4);
                for (auto& f : notebook)
                    if (s.family_filter.empty() || s.family_filter == f.name) {
                        try {
                            if (!construct_family(s, vertices, catalog, rules, f, spurious, scores))
                                all = false;
                        } catch (const std::exception& err) {
                            all = false;
                            std::cerr << f.name << " " << err.what() << '\n';
                        }
                    }
            }
        }
        if (!s.construct_only && !s.no_formulas && s.family_filter.empty())
            formula_study(s);
        return all ? 0 : 1;
    } catch (const std::exception& err) {
        std::cerr << err.what() << '\n';
        return 2;
    }
}
