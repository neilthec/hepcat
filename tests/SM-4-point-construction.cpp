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
// Open scores.tsv for the summary. Each family directory contains a readable
// spinor formula (.txt), a paper formula (.tex), a Mathematica HEPCAT file (.wl),
// the exact native expression (.expr), and the validation reports.
// Use --help for the SPINAS, CalcHEP, and MadGraph paths.
//
// Each catalog row describes one all-incoming amplitude. A family groups its
// helicities for the same physical two-to-two process. Start at construct_case
// for the physics and run_family for the complete set of comparisons.
//
// To add a process, add its construction and export to the Mathematica notebook.
// Regenerate catalog.tsv and flavor-closure.tsv using the instructions in
// tests/benchmarks/native-sm-four-point/README.md. Update the inventory counts
// in select_families and the catalog tools when changing their coverage.
#include "amplitude.hpp"
#include "construction-progress.hpp"
#include "external_export.hpp"

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
    std::size_t reference_weight, reference_leaves;
    std::string reference_expression;
};

struct settings {
    fs::path root = fs::current_path(), output, spinas;
    std::string family;
    std::size_t workers = 0;
    bool list = false, construct_only = false, notebook_only = false;
    external_options engines;
};

std::string species(const std::string &particle) {
    auto helicity = particle.find('.');
    return particle.substr(0, helicity);
}

// Notebook references mark model-local parameters with the placeholder scope "NOTEBOOK".
std::string bind_scope(std::string text, const std::string &scope) {
    const std::string marker = "\"NOTEBOOK\"", replacement = "\"" + scope + "\"";
    for (std::size_t pos = 0; (pos = text.find(marker, pos)) != std::string::npos; pos += replacement.size())
        text.replace(pos, marker.size(), replacement);
    return text;
}

// Construct one all-incoming amplitude from cubic and quartic interactions.
amplitude construct_case(const notebook_case &entry, const model &sm, const model &feynman,
                         const settings &run) {
    // Cross the last two analytic legs to compare with the physical process in the notebook.
    for (unsigned leg = 0; leg < 2; ++leg) {
        if (species(entry.particles[leg]) != entry.incoming[leg] ||
            species(sm.antiparticle(entry.particles[leg + 2])) != entry.outgoing[leg])
            throw std::runtime_error("Physical crossing disagrees with the notebook");
    }

    // The same elementary model supplies both vertices and the two sides of a four-point cut.
    amplitude process(entry.particles, sm, sm, feynman);
    construct_options construction;
    construction.progress = notebook_output::print_stage;
    construction.workers = run.workers;
    construction.validation.points = run.engines.points;
    construction.validation.seed = run.engines.seed;

    // The saved expression supplies an independent signed comparison in the final validation.
    construction.reference_amplitude.emplace(bind_scope(entry.reference_expression, sm.parameter_scope()),
                                             process.external_masses());
    if (construction.reference_amplitude->complexity() != entry.reference_weight ||
        construction.reference_amplitude->leaf_count() != entry.reference_leaves)
        throw std::runtime_error("Reference expression disagrees with the notebook complexity record");
    process.construct(construction);
    return process;
}

std::ofstream output(const fs::path &path);
void write_checks(std::ostream &out, const validation_report &report);

// Record both size measures so equal weighted scores can still be distinguished.
bool record_amplitude(const notebook_case &entry, const amplitude &process, const fs::path &directory,
                      std::ostream &log, std::ostream &scores) {
    const auto &report = process.last_construction();
    write_checks(log, report.validation);
    notebook_output::print_validation(report.validation);

    const auto &expression = process.final_amplitude();
    const auto weight = expression.complexity();
    const auto leaves = expression.leaf_count();
    const bool within = weight <= entry.reference_weight && leaves <= entry.reference_leaves;
    notebook_output::write_expression(directory / entry.id, expression);
    scores << entry.id << '\t' << entry.family << '\t' << (within ? "PASS" : "VALID_BUT_LONGER")
           << '\t' << weight << '\t' << entry.reference_weight << '\t' << leaves << '\t'
           << entry.reference_leaves << '\t' << report.total_seconds << '\n' << std::flush;

    std::cout << "    " << (within ? "PASS" : "FAIL") << "  Notebook size comparison\n"
              << "          Complexity " << notebook_output::number(weight)
              << " (target " << notebook_output::number(entry.reference_weight) << "), leaves "
              << notebook_output::number(leaves) << " (target " << notebook_output::number(entry.reference_leaves)
              << ")   " << notebook_output::seconds(report.total_seconds)
              << "\n    Formula files   " << entry.family << '/' << entry.id << ".txt"
              << "  (also .tex, .wl and .expr)" << std::endl;
    return within;
}

bool validate_family(const std::string &name, const std::vector<spinas_component> &generated,
                     const model &sm, const settings &config, const fs::path &directory,
                     std::ostream &log, std::ostream &scores) {
    spinas_export_options write;
    write.name = name;
    write.spinas_directory = fs::absolute(config.spinas);
    write.runtime_directory = config.engines.runtime_directory;
    notebook_output::print_stage("Exporting the family to SPINAS");
    auto project = write_spinas_process(directory / "spinas", sm, generated, write);
    write_external_project(project, sm, config.engines);
    const auto external = validate_external_project(project, config.engines);

    // Preserve the reports before reclaiming a successful build.
    const auto build = project.directory / "build";
    for (const auto *filename :
         {"external-configure.log", "external-build-calchep.log", "external-build-madgraph.log",
          "calchep-comparison.log", "calchep-precision.json", "calchep-precision.log", "calchep-outliers.csv",
          "calchep-chart.csv", "madgraph-comparison.log", "madgraph-comparison.json"}) {
        if (fs::is_regular_file(build / filename))
            fs::copy_file(build / filename, directory / filename);
    }
    log << "External configure " << external.configuration << ", CalcHEP " << external.calchep << ", MadGraph "
        << external.madgraph << '\n';
    scores << "external\t" << name << '\t' << (external.passed() ? "PASS" : "FAILED") << "\t\t\t\t\t\n"
           << std::flush;
    if (external.passed())
        fs::remove_all(project.directory);
    return external.passed();
}

// Combine all helicities of the same physical process before comparing polarization sums.
bool run_family(const std::vector<notebook_case> &family, const model &sm, const model &feynman,
                const settings &config, const fs::path &directory, std::ostream &scores) {
    const auto &name = family.front().family;
    fs::create_directories(directory);
    auto log = output(directory / "checks.txt");
    std::vector<spinas_component> generated;
    bool sizes_pass = true;
    std::size_t case_number = 0;

    for (const auto &entry : family) {
        const auto &legs = entry.particles;
        std::cout << "\n  Amplitude " << ++case_number << '/' << family.size() << "   " << entry.id
                  << "\n    All incoming   " << legs[0] << ", " << legs[1] << ", " << legs[2] << ", " << legs[3]
                  << std::endl;
        log << entry.id << '\n';
        try {
            auto process = construct_case(entry, sm, feynman, config);
            if (!record_amplitude(entry, process, directory, log, scores))
                sizes_pass = false;
            generated.push_back({entry.particles, process.final_amplitude()});
        } catch (const std::exception &e) {
            scores << entry.id << '\t' << name << "\tUNRESOLVED\t\t" << entry.reference_weight
                   << "\t\t" << entry.reference_leaves << "\t\n" << std::flush;
            if (auto error = dynamic_cast<const construction_validation_error *>(&e)) {
                write_checks(log, error->report);
                notebook_output::print_validation(error->report);
            }
            log << e.what() << '\n';
            std::cerr << "    UNRESOLVED " << entry.id << ": " << e.what() << '\n';
        }
        log.flush();
    }
    if (generated.size() != family.size()) {
        if (!config.construct_only) {
            scores << "external\t" << name << "\tINCOMPLETE_FAMILY\t\t\t\t\t\n" << std::flush;
            std::cout << "  Skipping CalcHEP and MadGraph: " << generated.size() << '/' << family.size()
                      << " amplitudes available. See " << name << "/checks.txt" << std::endl;
        }
        return false;
    }
    if (config.construct_only)
        return sizes_pass;

    std::cout << "\n  External comparisons" << std::endl;
    const bool external_pass = validate_family(name, generated, sm, config, directory, log, scores);
    return sizes_pass && external_pass;
}

// Command-line settings, catalog input, and persistent report formats.
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
    settings config;
    config.engines.points = 100000;
    for (int i = 1; i < argc; ++i) {
        std::string arg = argv[i];
        if (arg == "--help") {
            std::cout << R"(Usage: hepcat-sm-four-point [options]

Choose the calculation
  --list             Print two tab-separated columns: family ID and ordered amplitude count.
  --family NAME      Run one family, for example sm01_uUCc.
  --notebook-only    Select the 78 families written out in the notebook.
                     The full catalog also includes their flavor and crossing assignments.
  --construct-only   Run the native checks and the notebook size comparison.
                     The default run also checks CalcHEP and MadGraph.

Validation and output
  --points N         Phase-space points per comparison (default 100,000, minimum 10,000).
  --seed N           Reproduce a sample. The default uses the current time.
  --workers N        Construction workers (default uses available hardware).
  --output DIR       Create a new run directory here. Each family has a readable
                     .txt formula and an exact .expr file for every amplitude.
  --root DIR         HEPCAT checkout (default is the current directory).

Installations
  --spinas DIR   --calchep DIR   --madgraph DIR   --python EXE   --cmake EXE
)";
            std::exit(0);
        }
        if (arg == "--list") {
            config.list = true;
            continue;
        }
        if (arg == "--notebook-only") {
            config.notebook_only = true;
            continue;
        }
        if (arg == "--construct-only") {
            config.construct_only = true;
            continue;
        }
        if (i + 1 == argc)
            throw std::invalid_argument("Missing value for " + arg);
        std::string v = argv[++i];
        if (arg == "--root")
            config.root = v;
        else if (arg == "--output")
            config.output = v;
        else if (arg == "--family")
            config.family = v;
        else if (arg == "--workers")
            config.workers = integer(v);
        else if (arg == "--points")
            config.engines.points = integer(v);
        else if (arg == "--seed")
            config.engines.seed = integer(v);
        else if (arg == "--spinas")
            config.spinas = v;
        else if (arg == "--calchep")
            config.engines.calchep_directory = v;
        else if (arg == "--madgraph")
            config.engines.madgraph_directory = v;
        else if (arg == "--python")
            config.engines.python_executable = v;
        else if (arg == "--cmake")
            config.engines.cmake_executable = v;
        else
            throw std::invalid_argument("Unknown option " + arg);
    }
    config.root = fs::absolute(config.root).lexically_normal();
    if (config.engines.points < 10000)
        throw std::invalid_argument("Use at least 10,000 validation points");
    if (!config.engines.seed)
        config.engines.seed = std::chrono::system_clock::now().time_since_epoch().count();
    if (config.output.empty())
        config.output = config.root / "tests/results/spinas-output/SM-4-point";
    config.output = fs::absolute(config.output);
    if (config.spinas.empty())
        config.spinas = config.root.parent_path() / "spinas";
    if (config.engines.calchep_directory.empty())
        config.engines.calchep_directory = config.root.parent_path() / "CalcHEP/calchep_3.9.2";
    if (config.engines.madgraph_directory.empty())
        config.engines.madgraph_directory = config.root.parent_path() / "MadGraph";
    if (config.engines.python_executable == "python3") {
        std::ifstream launcher(config.engines.madgraph_directory / "bin/mg5_aMC");
        std::string line;
        if (std::getline(launcher, line) && line.rfind("#!/", 0) == 0) {
            fs::path interpreter(line.substr(2));
            if (interpreter.filename().string().rfind("python", 0) == 0 && fs::is_regular_file(interpreter))
                config.engines.python_executable = interpreter.string();
        }
    }
    config.engines.runtime_directory = config.root / "source/internal/export";
    config.engines.template_directory = config.root / "csource/external_templates";
    config.engines.calchep_model_directory = config.root / "tests/reference-models/calchep";
    config.engines.calchep_model_number = 1;
    config.engines.madgraph_model = (config.root / "tests/reference-models/madgraph/sm").string();
    // The constructive SM uses generation-diagonal quark mixing.
    config.engines.madgraph_parameter_rules["lamWS"] = 0;
    config.engines.progress = notebook_output::print_stage;
    return config;
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
        const auto fields = split(line, '\t');
        if (fields.size() != 8 || !ids.insert(fields[0]).second)
            throw std::runtime_error("Invalid notebook row");
        auto legs = split(fields[2], ',');
        auto incoming = split(fields[3], ',');
        auto outgoing = split(fields[4], ',');
        if (legs.size() != 4 || incoming.size() != 2 || outgoing.size() != 2)
            throw std::runtime_error("Invalid external legs in " + fields[0]);
        families.insert(fields[1]);
        rows.push_back({fields[0], fields[1], legs, incoming, outgoing,
                        integer(fields[5]), integer(fields[6]), fields[7]});
    }
    if (rows.size() != cases || families.size() != groups)
        throw std::runtime_error("Incomplete notebook inventory");
    return rows;
}

void write_checks(std::ostream &out, const validation_report &r) {
    out << "seed " << r.seed << "  requested " << r.requested << '\n';
    for (const auto &c : r.checks)
        out << notebook_output::status(c.status) << '\t' << c.kind << '\t' << c.label << '\t' << c.samples << '\t' << c.detail
            << '\n';
    for (const auto &n : r.numerical) {
        out << notebook_output::status(n.status) << '\t' << n.kind << '\t' << n.compared << " points, " << n.failures
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

using family_map = std::map<std::string, std::vector<notebook_case>>;

family_map select_families(const settings &config) {
    const auto inventory = config.root / "tests/benchmarks/native-sm-four-point";
    auto rows = read_notebook(inventory / "catalog.tsv", 138, 78);
    if (!config.notebook_only) {
        auto closure = read_notebook(inventory / "flavor-closure.tsv", 254, 180);
        rows.insert(rows.end(), closure.begin(), closure.end());
    }
    family_map selected;
    for (const auto &entry : rows) {
        if (config.family.empty() || entry.family == config.family)
            selected[entry.family].push_back(entry);
    }
    if (selected.empty())
        throw std::invalid_argument("Unknown notebook family " + config.family);
    return selected;
}

int run_notebook(const settings &config, const family_map &selected) {
    const auto start = std::chrono::steady_clock::now();
    fs::create_directories(config.output);
    const auto run = "run-" + std::to_string(std::chrono::system_clock::now().time_since_epoch().count());
    auto dir = config.output / run;
    for (unsigned suffix = 1; !fs::create_directory(dir); ++suffix)
        dir = config.output / (run + "-" + std::to_string(suffix));
    std::size_t cases = 0;
    for (const auto &family : selected)
        cases += family.second.size();
    std::cout << "Standard Model four-point amplitudes\n"
              << "  Selection    " << cases << (cases == 1 ? " amplitude in " : " amplitudes in ")
              << selected.size() << (selected.size() == 1 ? " family" : " families") << '\n'
              << "  Validation   " << notebook_output::number(config.engines.points)
              << " phase-space points per numerical comparison\n"
              << "  Seed         " << *config.engines.seed << '\n'
              << "  Output       " << dir.string() << '\n';
    if (config.construct_only)
        std::cout << "  Mode         Native checks only. CalcHEP and MadGraph skipped (--construct-only).\n";
    std::cout << std::endl;
    notebook_output::print_stage("Loading the elementary SM and the independent Feynman model");
    const model sm(config.root / "models", 1, "SM");
    const model feynman(config.engines.calchep_model_directory, 1, "Feynman");
    auto scores = output(dir / "scores.tsv");
    scores << "case\tfamily\tstatus\tweight\tnotebook_weight\tleaves\tnotebook_leaves\tseconds\n";
    std::size_t family_number = 0, passed = 0;
    for (const auto &[name, group] : selected) {
        const auto &process = group.front();
        std::cout << "\nFamily " << ++family_number << '/' << selected.size() << "   "
                  << process.incoming[0] << ' ' << process.incoming[1] << " -> "
                  << process.outgoing[0] << ' ' << process.outgoing[1] << "   [" << name << ']'
                  << std::endl;
        try {
            const bool ok = run_family(group, sm, feynman, config, dir / name, scores);
            passed += ok;
            std::cout << "\n  " << (ok ? "PASS" : "FAIL") << "  " << name
                      << "   Details in " << name << "/checks.txt" << std::endl;
        } catch (const std::exception &e) {
            std::cerr << "UNRESOLVED " << name << ": " << e.what() << '\n';
            scores << "external\t" << name << "\tUNRESOLVED\t\t\t\t\t\n";
            output(dir / name / "error.txt") << e.what() << '\n';
        }
    }
    const bool ok = passed == selected.size();
    const auto seconds = std::chrono::duration<double>(std::chrono::steady_clock::now() - start).count();
    std::cout << "\nSM four-point summary\n"
              << "  Result       " << (ok ? "PASS" : "FAIL") << "   " << passed << '/' << selected.size()
              << (selected.size() == 1 ? " family passed" : " families passed") << '\n'
              << "  Elapsed      " << notebook_output::seconds(seconds) << '\n'
              << "  Scores       " << (dir / "scores.tsv").string() << std::endl;
    return ok ? 0 : 1;
}
} // namespace

int main(int argc, char **argv) {
    try {
        const auto config = options(argc, argv);
        const auto selected = select_families(config);
        if (config.list) {
            for (const auto &[name, group] : selected)
                std::cout << name << '\t' << group.size() << '\n';
            return 0;
        }
        return run_notebook(config, selected);
    } catch (const std::exception &e) {
        std::cerr << e.what() << '\n';
        return 2;
    }
}
