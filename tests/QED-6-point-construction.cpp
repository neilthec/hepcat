// C++ counterpart of QED-6-point-construction.nb. From the HEPCAT repository root:
//   cmake -S csource -B tests/results/native-reduce-build -DCMAKE_BUILD_TYPE=Release
//   cmake --build tests/results/native-reduce-build --target hepcat-qed --parallel
//   tests/results/native-reduce-build/hepcat-qed --family eeAA --preference physical
// The example above constructs all four photon helicities and runs the CalcHEP
// and MadGraph checks at 100,000 phase-space points by default. To run the entire
// notebook, including the all-positive formula comparisons through eight photons,
// run the same executable without arguments.
// Use --list to see families and --help for installation paths and other options.
// Each run prints its directory under tests/results/spinas-output. Open construction.tsv
// for scores and, inside each pole preference and family, checks.txt for validations
// and the .txt amplitudes in spinor notation. Use .tex for papers, .wl with
// Mathematica HEPCAT, and .expr to reload the exact native expression.
//
// To extend the notebook, start at the process list below. construct_helicity()
// shows how to set up, construct, check, and save one amplitude. construct_family()
// collects the checked helicities for the numerical export.

#include "QED-all-positive-replay.hpp"
#include "amplitude.hpp"
#include "construction-progress.hpp"
#include "external_export.hpp"
#include "generation_sew.hpp"
#include <algorithm>
#include <chrono>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <map>
#include <set>
#include <sstream>
#include <tuple>

namespace fs = std::filesystem;
using namespace hepcat;
namespace {

struct helicity_case {
    std::string helicities;         // One sign for each photon, in all-incoming order.
    std::size_t physical, spurious; // Complexity ceilings for each pole preference.
};

enum class reference_source { model_catalog, all_positive_formula };

struct family {
    std::string name;
    std::vector<std::string> fermions; // Ordered external fermions. The photons follow them.
    std::vector<helicity_case> examples;
    reference_source reference = reference_source::model_catalog;
};

// Add a family or helicity here. For example, {"++-", 62, 63} measures three
// photons against the physical and spurious complexity targets, respectively.
// An empty helicity string is a purely massive process. Supply its signed
// reference in the corresponding model, and both catalog rows for --publication.
// A complete family can use photon permutations of the listed representatives.
// A single photon-helicity assignment is exported as a polarized process. The
// last family uses the independent electron-pair all-positive reference formula.
const std::vector<family> notebook = {
    // Four external particles
    {"eeAA", {"e", "E"}, {{"++", 1, 1}, {"+-", 10, 10}, {"-+", 10, 10}, {"--", 1, 1}}},
    {"eeee", {"e", "E", "E", "e"}, {{"", 8, 8}}},

    // Five external particles
    {"eeAAA", {"e", "E"}, {{"+++", 27, 30}, {"++-", 62, 63}, {"+--", 62, 63}, {"---", 27, 30}}},
    {"eeeeA", {"e", "E", "E", "e"}, {{"+", 160, 160}, {"-", 160, 160}}},

    // Six external particles
    {"eeAAAA",
     {"e", "E"},
     {{"++++", 108, 120},
      {"+++-", 327, 416},
      {"++--", 692, 712},
      {"+---", 327, 416},
      {"----", 108, 120}}},
    {"eeeeAA", {"e", "E", "E", "e"}, {{"++", 1200, 1216}, {"+-", 1600, 1632}, {"--", 1200, 1216}}},
    {"eeeeee", {"e", "E", "e", "E", "e", "E"}, {{"", 3168, 3168}}},

    // Seven external particles, with a fixed-helicity MadGraph comparison
    {"eeAAAAA", {"e", "E"}, {{"+++++", 820, 920}}, reference_source::all_positive_formula},
};

struct settings {
    fs::path root = fs::current_path(), output, spinas;
    external_options engines;
    std::string family_filter, preference = "both", helicity_filter;
    bool publication = false, list = false, construct_only = false, formula_only = false,
         no_formulas = false;
    fs::path formula_replay;
    bool model_formula_replay = false, summed_formula_replay = false;
    std::optional<unsigned> photons;
    std::size_t workers = 0;
    std::uint64_t seed = std::chrono::high_resolution_clock::now().time_since_epoch().count();
};

struct publication_row {
    std::size_t weighted, leaves;
    std::string expression;
};
using publication_key = std::tuple<std::string, std::string, std::string>;
using publication_table = std::map<publication_key, publication_row>;

// The file readers and detailed report writers are below the construction workflow.
amp_expression catalog_reference(const model& catalog, const std::vector<std::string>& legs);
publication_table read_publication(const fs::path& path, const std::string& scope);
std::ofstream file(const fs::path& path);
void print_checks(std::ostream& out, const validation_report& report);
std::string clean_cell(std::string text);

struct construction_models {
    const model& vertices;
    const model& factorization;
    const model& feynman;
};

std::vector<std::string> external_legs(const family& process, const helicity_case& entry) {
    auto legs = process.fermions;
    for (char sign : entry.helicities) {
        if (sign != '+' && sign != '-')
            throw std::invalid_argument("Expected + or - in the photon helicities for " + process.name);
        legs.emplace_back(sign == '+' ? "A.+" : "A.-");
    }
    return legs;
}

// Each signed reference enters only the validation and size comparison.
amp_expression signed_reference(const family& process, const helicity_case& entry,
                                const construction_models& models, const std::vector<std::string>& legs,
                                const amplitude& amp, bool spurious, const publication_row* published) {
    if (published)
        return amp_expression(published->expression, amp.external_masses());
    if (process.reference == reference_source::all_positive_formula) {
        if (process.fermions != std::vector<std::string>{"e", "E"} || entry.helicities.empty() ||
            entry.helicities.find_first_not_of('+') != std::string::npos)
            throw std::invalid_argument("The all-positive reference requires e, E followed by positive photons");
        return qed_notebook::all_positive_reference(models.vertices, entry.helicities.size()).expression(spurious);
    }
    return catalog_reference(models.factorization, legs);
}

std::optional<std::size_t> measure_complexity(const amp_expression& expression, std::ostream& log,
                                              const std::string& branch) {
    try {
        return expression.complexity();
    } catch (const std::length_error& error) {
        log << branch << " complexity unavailable: " << error.what() << '\n';
    } catch (const std::overflow_error& error) {
        log << branch << " complexity unavailable: " << error.what() << '\n';
    }
    return std::nullopt;
}

struct selected_amplitude {
    spinas_component component;
    bool meets_target;
};

selected_amplitude construct_helicity(const settings& run, const construction_models& models,
                                      const family& process, const helicity_case& entry, bool spurious,
                                      const publication_table& publication, const fs::path& directory,
                                      std::ostream& log, std::ostream& scores) {
    const auto legs = external_legs(process, entry);
    const std::string policy = spurious ? "spurious" : "physical";
    const std::string branch = entry.helicities.empty() ? "massive" : entry.helicities;
    const auto relative_dir = directory.lexically_relative(run.output);

    // A four-point cut needs only cubic vertices. Higher-point cuts use the
    // lower-point amplitudes stored in the selected factorization model.
    const model& cuts = legs.size() == 4 ? models.vertices : models.factorization;
    amplitude amp(legs, models.vertices, cuts, models.feynman);
    const auto* published = run.publication ? &publication.at({policy, process.name, branch}) : nullptr;
    auto reference = signed_reference(process, entry, models, legs, amp, spurious, published);

    construct_options options;
    options.workers = run.workers;
    options.pole_preference =
        spurious ? generation_references::cyclic : generation_references::physical;
    options.validation.seed = run.seed;
    options.validation.points = run.engines.points;
    options.reference_amplitude = reference;
    options.progress = notebook_output::print_stage;

    const auto reference_score = reference.complexity();
    if (published && reference_score != published->weighted)
        throw std::runtime_error(
            "native and publication complexity measures disagree on the reference");
    const auto baseline = spurious ? entry.spurious : entry.physical;
    const auto target = baseline ? std::min(baseline, reference_score) : reference_score;

    auto result = amp.construct(options);
    const auto& report = amp.last_construction();
    log << branch << " signed reference and Feynman comparisons\n";
    print_checks(log, report.validation);
    notebook_output::print_validation(report.validation);
    notebook_output::write_expression(directory / branch, result);

    const auto complexity = measure_complexity(result, log, branch);
    const bool meets_target = complexity && *complexity <= target;
    const auto measured = complexity ? std::to_string(*complexity) : "UNMEASURED";
    const char* score_status = meets_target ? "PASS" : "VALID_BUT_LONGER";
    if (!complexity)
        score_status = "VALID_SCORE_UNAVAILABLE";
    const auto leaves = result.leaf_count();
    scores << policy << '\t' << process.name << '\t' << branch << '\t' << score_status << '\t'
           << measured << '\t' << target << '\t' << reference_score << '\t' << run.engines.points
           << '\t' << report.total_seconds << "\t\t" << leaves << '\t'
           << (published ? published->leaves : reference.leaf_count()) << '\n'
           << std::flush;
    std::cout << "    Complexity        " << measured << " (target " << target << ", "
              << (meets_target ? "met" : "unmet") << "), " << notebook_output::number(leaves)
              << " leaves\n"
              << "    Time              " << notebook_output::seconds(report.total_seconds) << '\n'
              << "    Formula files     " << (relative_dir / (branch + ".txt")).string()
              << "  (also .tex, .wl and .expr)" << std::endl;

    // As in the notebook, validate a shorter worked form before selecting it
    // for export. Publication mode always measures the automatic construction.
    if (meets_target || run.publication || reference_score > target)
        return {{legs, std::move(result)}, meets_target};

    notebook_output::print_stage("Validating the notebook's shorter worked expression");
    const auto verified = amp.validate(reference, options.validation);
    log << branch << " worked representative, independently checked\n";
    print_checks(log, verified);
    notebook_output::print_validation(verified);
    if (!verified.passed())
        throw std::runtime_error("worked representative failed its own validation");
    notebook_output::write_expression(directory / (branch + ".selected"), reference);
    scores << policy << '\t' << process.name << '\t' << branch << "\tWORKED_FORM_PASS\t"
           << reference_score << '\t' << target << '\t' << reference_score << '\t' << run.engines.points
           << "\t\tvalidated after construction\n"
           << std::flush;
    std::cout << "    Selected for export  " << (relative_dir / (branch + ".selected.txt")).string()
              << " (complexity " << reference_score << ")\n";
    return {{legs, std::move(reference)}, true};
}

bool validate_export(const settings& run, const model& vertices, const family& process,
                     const std::vector<spinas_component>& components, const fs::path& directory,
                     std::ostream& log) {
    spinas_export_options write;
    write.name = process.name;
    write.spinas_directory = run.spinas;
    write.runtime_directory = run.engines.runtime_directory;
    auto backends = run.engines;
    backends.progress = notebook_output::print_stage;
    if (process.examples.size() == 1 && !process.examples.front().helicities.empty()) {
        // With one photon-helicity assignment we can compare its polarized
        // rate with MadGraph. CalcHEP needs every helicity for its spin sum.
        const auto& helicities = process.examples.front().helicities;
        backends.run_calchep = false;
        backends.calchep_directory.clear();
        backends.calchep_model_directory.clear();
        for (std::size_t i = 0; i < helicities.size(); ++i)
            write.fixed_helicities[process.fermions.size() + i] = helicities[i] == '+' ? 2 : -2;
        std::cout << "    SKIP CalcHEP polarization sum (only one photon-helicity assignment is available).\n";
    }
    notebook_output::print_stage("Writing the SPINAS process and external comparison programs");
    auto project = write_spinas_process(directory / "spinas", vertices, components, write);
    write_external_project(project, vertices, backends);
    const auto external = validate_external_project(project, backends);

    if (backends.run_calchep)
        log << "CalcHEP summed report " << external.calchep_report << " exit=" << external.calchep
            << '\n';
    else
        log << "CalcHEP unpolarized comparison requires the full helicity family.\n";
    log << "MadGraph polarized and summed report " << external.madgraph_report
        << " exit=" << external.madgraph << '\n';
    return external.passed();
}

bool construct_family(const settings& run, const construction_models& models, const family& process,
                      bool spurious, const publication_table& publication, std::ostream& scores) {
    const std::string policy = spurious ? "spurious" : "physical";
    const auto directory = run.output / policy / process.name;
    fs::create_directories(directory);
    auto log = file(directory / "checks.txt");

    bool meets_targets = true;
    std::vector<spinas_component> components;
    for (const auto& entry : process.examples) {
        if (!run.helicity_filter.empty() && run.helicity_filter != entry.helicities)
            continue;
        const std::string branch = entry.helicities.empty() ? "massive" : entry.helicities;
        const auto legs = external_legs(process, entry);
        std::cout << "\n  ";
        if (entry.helicities.empty())
            std::cout << "Massive external particles";
        else
            std::cout << "Photon helicities " << entry.helicities;
        std::cout << "    (all incoming:";
        for (const auto& leg : legs)
            std::cout << ' ' << leg;
        std::cout << ")\n" << std::flush;
        try {
            auto selected = construct_helicity(run, models, process, entry, spurious, publication,
                                               directory, log, scores);
            meets_targets &= selected.meets_target;
            components.push_back(std::move(selected.component));
        } catch (const std::exception& error) {
            const auto baseline = spurious ? entry.spurious : entry.physical;
            scores << policy << '\t' << process.name << '\t' << branch << "\tUNRESOLVED\t\t" << baseline
                   << "\t\t\t\t" << clean_cell(error.what()) << '\n'
                   << std::flush;
            if (auto failure = dynamic_cast<const construction_validation_error*>(&error)) {
                print_checks(log, failure->report);
                notebook_output::print_validation(failure->report);
            }
            std::cerr << "    UNRESOLVED  " << branch << ": " << error.what() << "\n    See "
                      << (directory / "checks.txt").lexically_relative(run.output).string() << '\n';
            meets_targets = false;
        }
    }
    if (run.construct_only)
        return meets_targets;
    if (components.size() != process.examples.size()) {
        std::cerr << "    SKIP external validation. A required helicity could not be constructed.\n";
        return false;
    }
    std::cout << "\n  External validation\n" << std::flush;
    const bool external_passed =
        validate_export(run, models.vertices, process, components, directory, log);
    return meets_targets && external_passed;
}

bool run_families(const settings& run) {
    notebook_output::print_stage("Reading the QED vertices and Feynman rules");
    const model vertices(run.root / "models", 3);
    const model feynman(run.engines.calchep_model_directory, 2);
    publication_table publication;
    if (run.publication)
        publication = read_publication(run.root / "tests/benchmarks/publication-qed/catalog.tsv",
                                       vertices.parameter_scope());

    auto scores = file(run.output / "construction.tsv");
    scores << "preference\tfamily\thelicity\tstatus\tcpp_complexity\tthreshold\treference_complexity"
              "\tpoints\tseconds\tdetail\tcpp_leaves\treference_leaves\n";

    const auto family_count = run.family_filter.empty() ? notebook.size() : 1;
    const auto total = family_count * (run.preference == "both" ? 2 : 1);
    std::size_t number = 0, passed = 0;
    for (bool spurious : {false, true}) {
        const std::string policy = spurious ? "spurious" : "physical";
        if (run.preference != "both" && run.preference != policy)
            continue;
        const model catalog(run.root / "models", spurious ? 5 : 4);
        const construction_models models{vertices, catalog, feynman};
        for (const auto& process : notebook) {
            if (!run.family_filter.empty() && run.family_filter != process.name)
                continue;
            std::cout << "\n[" << ++number << '/' << total << "] " << process.name << "  |  "
                      << (spurious ? "AllSpurious" : "AllPhysical") << std::endl;
            try {
                const bool success =
                    construct_family(run, models, process, spurious, publication, scores);
                passed += success;
                std::cout << "  " << (success ? "PASS" : "FAIL") << "  " << process.name << " ("
                          << policy << " poles)\n"
                          << std::flush;
            } catch (const std::exception& error) {
                std::cerr << "  FAIL  " << process.name << " (" << policy << " poles): " << error.what()
                          << '\n';
            }
        }
    }
    std::cout << "\nFamily runs passed    " << passed << '/' << total << "\n"
              << "Construction scores   construction.tsv\n";
    return passed == total;
}

// Signed model and publication references, expression files, and detailed validation logs.

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

unsigned long long integer_option(const std::string& value) {
    if (value.empty() || value.find_first_not_of("0123456789") != value.npos)
        throw std::invalid_argument("expected a nonnegative integer, got " + value);
    return std::stoull(value);
}

publication_table read_publication(const fs::path& path, const std::string& scope) {
    std::ifstream input(path);
    std::string line;
    if (!std::getline(input, line) || line != "form\tfamily\thelicity\tweighted\tleaves\texpression")
        throw std::runtime_error("missing or invalid publication catalog: " + path.string());
    publication_table table;
    while (std::getline(input, line)) {
        std::istringstream stream(line);
        std::string mode, family, helicity, weighted, leaves, expression;
        for (auto column : {&mode, &family, &helicity, &weighted, &leaves})
            if (!std::getline(stream, *column, '\t'))
                throw std::runtime_error("incomplete publication reference");
        if (!std::getline(stream, expression) || expression.empty())
            throw std::runtime_error("missing signed publication expression");
        const std::string placeholder = "\"PUBLICATION\"";
        const std::string replacement = "\"" + scope + "\"";
        std::size_t pos = 0;
        while ((pos = expression.find(placeholder, pos)) != std::string::npos) {
            expression.replace(pos, placeholder.size(), replacement);
            pos += replacement.size();
        }
        publication_row row{integer_option(weighted), integer_option(leaves), std::move(expression)};
        if (!table.emplace(publication_key{mode, family, helicity}, std::move(row)).second)
            throw std::runtime_error("duplicate publication reference");
    }
    std::size_t expected = 0;
    for (const auto& f : notebook)
        expected += 2 * f.examples.size();
    if (table.size() != expected)
        throw std::runtime_error("incomplete publication catalog");
    return table;
}

// Independent all-positive formula study and precision replay.

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
unsigned formula_precision_bits(const std::string& receipt) {
    const std::string key = "\"FormulaPrecisionBits\":";
    const auto pos = receipt.find(key);
    if (pos == receipt.npos)
        throw std::runtime_error("missing source precision in the formula receipt");
    return static_cast<unsigned>(std::stoul(receipt.substr(pos + key.size())));
}
void write_formula_config(const fs::path& config, const external_options& engines, unsigned n,
                          double mass) {
    auto data = file(config);
    data << "{\"MadGraphDirectory\":" << json_string(engines.madgraph_directory.string())
         << ",\"MadGraphModel\":" << json_string(engines.madgraph_model)
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

void formula_study(const settings& s) {
    std::cout << "\nAll-positive photon formulas\n" << std::flush;
    auto out = s.output / "all-positive";
    fs::create_directories(out);
    auto build = out / "build";
    const auto cmake = s.engines.cmake_executable;
    notebook_output::print_stage("Building the all-positive formula comparison");
    command({cmake, "-S", (s.root / "tests/benchmarks/all-positive").string(), "-B", build.string(),
             "-DCMAKE_BUILD_TYPE=Release", "-DSPINAS_ROOT=" + s.spinas.string()},
            out / "configure.log");
    command({cmake, "--build", build.string(), "--target", "qed-all-positive"}, out / "compile.log");
    notebook_output::print_stage("Running the small-multiplicity formula controls");
    command({(build / "qed-all-positive").string(), "--self-test"}, out / "small-formula-controls.log");
    std::cout << "  PASS small-multiplicity controls\n" << std::flush;
    for (unsigned n = 2; n <= 8; ++n) {
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
        write_formula_config(config, s.engines, n, mass);
        notebook_output::print_stage("Generating and compiling the independent MadGraph reference");
        command({s.engines.python_executable,
                 (s.engines.runtime_directory / "madgraph-runtime.py").string(), "--config",
                 config.string(), "--output", reference.string()},
                dir / "madgraph.log");
        auto library = reference / "libhepcat_madgraph.so";
        if (!fs::exists(library))
            library = reference / "libhepcat_madgraph.dylib";
        notebook_output::print_stage("Comparing both formulas with MadGraph at " +
                                     notebook_output::number(s.engines.points) + " phase-space points");
        auto status = run_external_command(
            {(build / "qed-all-positive").string(), "--photons", std::to_string(n), "--points",
             std::to_string(s.engines.points), "--seed", std::to_string(s.seed), "--mass",
             std::to_string(mass), "--control-points", "0", "--output-mode", "outliers", "--reference",
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
        bool passed = true;
        std::ostringstream outcome;
        if (status == 1 && receipt.find(R"("Status":"FAIL",)") != receipt.npos) {
            notebook_output::print_stage("Replaying saved formula outliers at 160 bits");
            const model vertices(s.root / "models", 3);
            auto replay = qed_notebook::replay_formulas(vertices, n, dir / "check", s.engines.points,
                                                        mass, 1e-7L, true, true, mpq_class(1, 3),
                                                        formula_precision_bits(receipt));
            auto count = std::to_string(replay.replayed) + ",";
            if (receipt.find("\"Failures\":" + count) == receipt.npos ||
                receipt.find("\"StoredRows\":" + count) == receipt.npos)
                throw std::runtime_error("raw outlier count and precision replay disagree");
            passed = replay.unchanged_failures == 0 && replay.projected_identity_failures == 0;
            outcome << (passed ? "PASS " : "FAIL ") << s.engines.points << " points, "
                    << replay.replayed << " replayed at 160 bits. " << replay.unchanged_failures
                    << " unresolved on original momenta (worst relative MG "
                    << replay.worst_unchanged_mg << "). Separate on-shell projection, "
                    << replay.projected_identity_failures
                    << " failed signed identities (worst relative " << replay.worst_projected_identity
                    << ").\n";
        } else if (status == 0 && receipt.find(R"("Status":"PASS",)") != receipt.npos &&
                   receipt.find(R"("Failures":0,)") != receipt.npos) {
            outcome << "PASS " << s.engines.points << " points at native precision\n";
        } else {
            throw std::runtime_error("failed or incomplete formula comparison: " + dir.string());
        }
        file(dir / "outcome.txt") << outcome.str();
        if (status == 0) {
            std::cout << "    PASS  Both formulas agree with MadGraph at "
                      << notebook_output::number(s.engines.points) << " points.\n";
        } else {
            std::cout << "    " << outcome.str();
        }
        std::cout << "    Formula report    "
                  << (dir / "check.json").lexically_relative(s.output).string() << std::endl;
        if (!passed)
            throw std::runtime_error("formula precision comparison exceeds tolerance: " + dir.string());
        // Formula references can occupy gigabytes at N=8. Keep the config,
        // independent receipt, values, points and logs for this run.
        fs::copy_file(reference / "result.json", dir / "reference.json");
        fs::remove_all(reference);
    }
}

bool replay_archived_formula(const settings& s) {
    const unsigned n = *s.photons;
    const bool model = s.model_formula_replay;
    std::cout << "Replaying saved points at 160 bits from " << s.formula_replay.string() << '\n'
              << std::flush;
    auto result = qed_notebook::replay_formulas(
        hepcat::model(s.root / "models", 3), n, s.formula_replay, s.engines.points,
        model ? 0.0005 : (n == 8 ? 1.5 : 1.0), 1e-7L, true, !s.summed_formula_replay,
        model ? mpq_class(31333, 100000) : mpq_class(1, 3),
        formula_precision_bits(contents(s.formula_replay.string() + ".json")));
    std::cout << "Replayed " << notebook_output::number(result.replayed)
              << " events at 160 bits\n  Original momenta     " << result.unchanged_failures
              << " failures, maximum MG relative difference " << result.worst_unchanged_mg
              << "\n  Projected momenta    " << result.projected_identity_failures
              << " signed identity failures, maximum " << result.worst_projected_identity
              << ", maximum shift " << result.largest_shift << '\n';
    const bool passed = !result.unchanged_failures && !result.projected_identity_failures;
    std::cout << (passed ? "PASS" : "FAIL") << " formula replay\n";
    return passed;
}

// Command-line selection and installation paths.

void usage() {
    std::cout << R"(HEPCAT QED amplitude construction

Usage
  hepcat-qed [options]
  hepcat-qed --family eeAA --preference physical
  hepcat-qed --family eeAAA --helicity ++- --construct-only
  hepcat-qed --formulas-only --photons 2

Process selection
  --list                     Show the available families, helicities, and score targets.
  --family NAME              Run one family from the list.
  --helicity SIGNS           Select one entry with --family and --construct-only.
  --preference MODE          physical, spurious, or both (default).
  --construct-only           Run the native construction and signed checks.
  --publication              Use the signed publication catalog and its complexity targets.

Sampling and files
  --points N                 Phase-space points per comparison (default 100,000, minimum 10,000).
  --seed N                   Repeat a run with the same random seed.
  --workers N                Number of construction and validation workers.
  --root DIR                 HEPCAT checkout (default is the working directory).
  --output DIR               Place this run's directory under DIR.

External programs
  --spinas DIR               SPINAS checkout.
  --calchep DIR              CalcHEP installation.
  --madgraph DIR             MadGraph installation.
  --python EXE               Python interpreter for MadGraph (default from the launcher).
  --cmake EXE                CMake executable.

All-positive formula study
  --formulas-only            Compare both formulas with MadGraph for 2 through 8 photons.
  --photons N                Choose a single photon multiplicity for the study.
  --skip-formulas            Omit the separate formula comparisons.
  --replay-general PREFIX    Recheck saved general-formula outliers at 160 bits.
  --replay-model PREFIX      Recheck saved model-formula outliers.
  --replay-model-summed PREFIX
                             Recheck saved model outliers using the summed representation.
                             For a replay, supply the original --points and --photons.

Running without options constructs both pole preferences, checks the complete
families with CalcHEP and MadGraph, and runs the separate all-positive study.
Publication runs use at least 100,000 points and default to seed 861811.
)";
}

void list() {
    std::cout << "QED notebook inventory\n\n"
              << std::left << std::setw(11) << "Family" << std::setw(18) << "Photon helicities"
              << std::right << std::setw(11) << "AllPhysical" << std::setw(14) << "AllSpurious" << '\n';
    for (const auto& process : notebook) {
        bool first = true;
        for (const auto& entry : process.examples) {
            const auto helicities = entry.helicities.empty() ? "(no photons)" : entry.helicities;
            std::cout << std::left << std::setw(11) << (first ? process.name : "") << std::setw(18)
                      << helicities << std::right << std::setw(11) << entry.physical << std::setw(14)
                      << (entry.spurious ? std::to_string(entry.spurious) : "formula") << '\n';
            first = false;
        }
    }
    std::cout << "\nThe two score columns give the weighted complexity targets.\n"
              << "The separate all-positive formula study covers 2 through 8 photons.\n";
}

settings options(int argc, char** argv) {
    settings s;
    bool explicit_python = false, explicit_seed = false, explicit_points = false;
    for (int i = 1; i < argc; ++i) {
        std::string key = argv[i];
        if (key == "--help") {
            usage();
            std::exit(0);
        }
        if (key == "--publication") {
            s.publication = true;
            continue;
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
        else if (key == "--points") {
            s.engines.points = integer_option(v);
            explicit_points = true;
        } else if (key == "--seed") {
            s.seed = integer_option(v);
            explicit_seed = true;
        } else if (key == "--family")
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
        else if (key == "--replay-general" || key == "--replay-model" ||
                 key == "--replay-model-summed") {
            s.formula_replay = fs::absolute(v);
            s.model_formula_replay = key != "--replay-general";
            s.summed_formula_replay = key == "--replay-model-summed";
        } else if (key == "--photons") {
            auto n = integer_option(v);
            if (n < 2 || n > 8)
                throw std::invalid_argument("formula multiplicity must be 2..8");
            s.photons = n;
        } else
            throw std::invalid_argument("unknown option " + key);
    }
    if (!explicit_points && (s.formula_replay.empty() || s.publication))
        s.engines.points = 100000;
    if (s.publication) {
        if (!explicit_seed)
            s.seed = 861811;
        if (s.engines.points < 100000)
            throw std::invalid_argument("--publication requires at least 100000 comparison points");
    }
    if (!s.formula_replay.empty()) {
        if (!s.photons || !s.engines.points)
            throw std::invalid_argument(
                "a formula replay requires --photons and a positive point count");
        s.root = fs::absolute(s.root).lexically_normal();
        return s;
    }
    if (s.preference != "physical" && s.preference != "spurious" && s.preference != "both")
        throw std::invalid_argument("pole preference must be physical, spurious or both");
    if (s.engines.points < 10000)
        throw std::invalid_argument("at least 10000 comparison points are required");
    if (!s.family_filter.empty() &&
        std::none_of(notebook.begin(), notebook.end(),
                     [&](const family& f) { return f.name == s.family_filter; }))
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
        s.output = s.root / "tests/results/spinas-output";
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

} // namespace

int main(int argc, char** argv) {
    try {
        auto run = options(argc, argv);
        if (run.list && run.formula_replay.empty()) {
            list();
            return 0;
        }
        if (!run.formula_replay.empty())
            return replay_archived_formula(run) ? 0 : 1;
        if (!fs::is_regular_file(run.root / "models/amps3.mdl"))
            throw std::runtime_error("specify the HEPCAT checkout with --root");

        fs::create_directories(run.output);
        const auto start = std::chrono::steady_clock::now();
        run.output /=
            "run-" + std::to_string(run.seed) + "-" + std::to_string(start.time_since_epoch().count());
        if (!fs::create_directory(run.output))
            throw std::runtime_error("run directory already exists");
        std::cout << "QED amplitude construction\n"
                  << "  Phase-space points   " << notebook_output::number(run.engines.points)
                  << " per comparison\n"
                  << "  Random seed          " << run.seed << '\n'
                  << "  Output directory     " << run.output.string() << std::endl;
        if (run.construct_only)
            std::cout << "  External comparisons skipped (--construct-only).\n" << std::flush;

        bool passed = true;
        if (!run.formula_only)
            passed = run_families(run);
        if (!run.construct_only && !run.no_formulas && run.family_filter.empty())
            formula_study(run);

        const std::chrono::duration<double> elapsed = std::chrono::steady_clock::now() - start;
        std::cout << "\n"
                  << (passed ? "PASS" : "FAIL") << "  QED notebook selection\n"
                  << "  Total time           " << notebook_output::seconds(elapsed.count()) << '\n'
                  << "  Output directory     " << run.output.string() << '\n';
        return passed ? 0 : 1;
    } catch (const std::exception& error) {
        std::cerr << "QED run stopped: " << error.what() << '\n';
        return 2;
    }
}
