// Audit the QED factorization models against the independent publication amplitudes.
// From the repository root, build the hepcat-check-qed-catalog target and run:
//   tests/results/native-reduce-build/hepcat-check-qed-catalog .
//   tests/results/native-reduce-build/hepcat-check-qed-catalog . --full
// The full audit also sews each amplitude's lower-point channels and compares its
// signed value with the independent off-shell Feynman diagrams at 100,000 points.
#include "amplitude.hpp"
#include "reduce.hpp"
#include "generation_feynman.hpp"
#include "validation_feynman.hpp"
#include "validation_dirac_current.hpp"
#include "validation_numeric.hpp"

#include <algorithm>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <map>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

using namespace hepcat;
namespace fs = std::filesystem;

namespace {
using key = std::tuple<std::string, std::string, std::string>;

using row = std::vector<std::string>;

std::map<key, row> read_rows(const fs::path& file, std::size_t columns) {
    std::ifstream input(file);
    if (!input) throw std::runtime_error("Cannot open " + file.string());
    std::string line;
    std::getline(input, line);
    std::map<key, row> result;
    while (std::getline(input, line)) {
        std::istringstream row(line);
        std::vector<std::string> fields;
        std::string field;
        while (std::getline(row, field, '\t')) fields.push_back(field);
        if (fields.size() != columns) throw std::runtime_error("Incomplete row in " + file.string());
        // The stored representatives use positive photon helicities first.
        if (fields[1].size() > 6 || (fields[1] == "eeAA" && fields[2] == "-+")) continue;
        const key label{fields[0], fields[1], fields[2]};
        if (!result.emplace(label, std::move(fields)).second)
            throw std::runtime_error("Duplicate publication row in " + file.string());
    }
    if (result.size() != 38) throw std::runtime_error("Expected 19 helicity representatives per model");
    return result;
}

std::pair<std::size_t, std::size_t> best_size(const row& sizes) {
    // The fixture has three weight columns followed by the three corresponding leaf counts.
    std::pair<std::size_t, std::size_t> best{std::stoull(sizes[3]), std::stoull(sizes[6])};
    for (std::size_t column : {4u, 5u}) {
        if (sizes[column] == "NA") continue;
        std::pair<std::size_t, std::size_t> candidate{std::stoull(sizes[column]), std::stoull(sizes[column + 3])};
        best = std::min(best, candidate);
    }
    return best;
}

bool check_full(const model& vertices, const model& cuts, const model& feynman,
                const std::vector<std::string>& legs, const amp_expression& expression,
                std::uint64_t seed, bool expect_spurious) {
    amplitude process(legs, vertices, cuts, feynman);
    validation_options options;
    options.seed = seed;
    options.points = 100000;
    const auto report = process.validate(expression, options);
    for (const auto& item : report.checks)
        if (item.status != check_outcome::pass) std::cerr << item.kind << "  " << item.detail << '\n';
    bool compared = false;
    for (const auto& item : report.numerical) {
        if (item.status != check_outcome::pass) std::cerr << item.kind << "  " << item.detail << '\n';
        if (item.kind == "Feynman" && item.compared == options.points && item.nonzero > 0)
            compared = true;
    }
    const bool has_spurious = std::any_of(report.checks.begin(), report.checks.end(),
        [](const auto& check) { return check.kind == "Spurious" && check.samples != 0; });
    if (has_spurious != expect_spurious)
        std::cerr << "Unexpected denominator structure for the selected pole representation\n";
    return report.passed() && compared && has_spurious == expect_spurious;
}
// The single photon in this family uses the emitting fermion pair as its
// reference in model 5. Check that every possible pair supplies its pole.
bool emitting_pair_poles(Algebra& a, const model_vertex& row, bool spurious) {
    const Id photon = a.integer(9);
    const auto invariant = [&](std::vector<Id> labels) {
        return a.call("PropDen", {a.call("Mom", {a.call("Multiparticle", std::move(labels))}), a.integer(0)});
    };
    std::set<Id> poles;
    for (unsigned i = 0; i < 8; ++i)
        for (unsigned j = i + 1; j < 8; ++j) {
            if (row.legs[i] == row.legs[j]) continue;
            const auto x = a.integer(i+1), y = a.integer(j+1);
            const auto diff = a.add({invariant({x,y,photon}), a.mul({a.integer(-1),invariant({x,y})})});
            poles.insert(a.pow(diff, -1));
        }
    std::vector<Id> pending{row.expression};
    std::vector<bool> visited(a.size());
    unsigned found = 0;
    while (!pending.empty()) {
        const auto id = pending.back();
        pending.pop_back();
        if (visited[id]) continue;
        visited[id] = true;
        if (poles.count(id)) ++found;
        const auto& node = a.node(id);
        pending.insert(pending.end(), node.args.begin(), node.args.end());
    }
    if (found == (spurious ? poles.size() : 0)) return true;
    std::cerr << "Found " << found << " emitting-pair poles in the single-photon catalog entry\n";
    return false;
}
// Compare the rows as read from disk, including their model-local coefficients.
bool check_higher_point(const model& cuts, const model_vertex& row, const model& rules) {
    using namespace validation;
    Algebra a(&cuts.algebra()), fa(&rules.algebra());
    const auto params = model_parameters(cuts);
    std::vector<Id> cut_masses, rule_masses, trees;
    std::vector<real> masses;
    for (const auto& leg : row.legs) {
        cut_masses.push_back(a.parse(cuts.particle(leg).mass));
        masses.push_back(evaluate_scalar(a,cut_masses.back(),params).real());
        rule_masses.push_back(fa.parse(rules.particle(generation::feynman_external(rules,leg)).mass));
    }
    a.setKinematics(cut_masses,{});
    fa.setKinematics(rule_masses,{});
    expression_evaluator stored(a,a.mul({row.factor,row.expression}),params);
    if (row.legs.size() == 9) {
        // Accumulate the off-shell QED trees as Dirac currents. This keeps the
        // independent signed check affordable for up to 18,144 diagrams.
        std::vector<int> species;
        for (const auto& leg : row.legs)
            species.push_back(leg == "e" ? 1 : leg == "E" ? -1 : leg == "A.+" ? 2 : -2);
        dirac_current::recursion reference(species);
        const auto e = -evaluate_scalar(fa, rules.vertices().at(0).factor, model_parameters(rules)).real();
        for (unsigned i = 0; i < 32; ++i) {
            const auto point = sample_point(masses, 913813 + i);
            const auto y = reference(point, e, dirac_current::gauge::independent);
            auto x = stored(point);
            if (std::abs(x-y) > 1e-8L * std::abs(y)) x = stored.precise(point);
            if (!(std::abs(y) > 0 && std::abs(x-y) <= 1e-8L * std::abs(y))) return false;
        }
        return true;
    }
    for (const auto& plan : generation::diagram_plans(rules,row.legs))
        trees.push_back(feynman_to_spinors(fa,generation::sew_feynman(fa,rules,row.legs,plan)));
    if (trees.empty()) return false;
    expression_evaluator reference(fa,fa.add(trees),model_parameters(rules));
    for (unsigned i=0;i<8;++i) {
        const auto point = sample_point(masses,706181+i);
        const auto x = stored(point), y = reference(point);
        const auto scale = std::max(std::abs(x),std::abs(y));
        if (!(scale > 0 && std::abs(x-y) <= 1e-8L*scale)) return false;
    }
    return true;
}
} // namespace

int main(int argc, char** argv) {
    try {
        if (argc < 2 || argc > 3 || (argc == 3 && std::string(argv[2]) != "--full"))
            throw std::invalid_argument("Usage: hepcat-check-qed-catalog ROOT [--full]");
        const auto root = fs::absolute(argv[1]);
        const auto evidence = root / "tests/benchmarks/publication-qed";
        const auto formulas = read_rows(evidence / "catalog.tsv", 6);
        const auto sizes = read_rows(evidence / "complexities.tsv", 9);
        const model vertices(root / "models", 3, "PUBLICATION");
        const model feynman(root / "tests/reference-models/calchep", 2);
        std::set<key> seen;
        std::size_t passed = 0;
        for (int number : {4, 5}) {
            const model catalog(root / "models", number, vertices.parameter_scope());
            for (const auto& vertex : catalog.vertices()) {
                if (vertex.legs.size() < 4 || vertex.legs.size() > 9) continue;
                std::string family, helicities;
                std::vector<std::string> masses;
                for (const auto& leg : vertex.legs) {
                    masses.push_back(catalog.particle(leg).mass);
                    if (leg == "A.+" || leg == "A.-") {
                        family += 'A';
                        helicities += leg.back();
                    } else if (leg == "e" || leg == "E") family += 'e';
                    else throw std::runtime_error("Unexpected QED particle " + leg);
                }
                if (helicities.empty()) helicities = "massive";
                const key label{number == 4 ? "physical" : "spurious", family, helicities};
                if (!seen.insert(label).second) throw std::runtime_error("Duplicate QED model row");
                Algebra joined(&catalog.algebra());
                const auto value = joined.mul({vertex.factor, vertex.expression});
                if (vertex.legs.size() >= 7) {
                    bool ok = check_higher_point(catalog,vertex,feynman);
                    if (family == "eeeeeeeeA") ok = emitting_pair_poles(joined, vertex, number == 5) && ok;
                    if (argc == 3)
                        ok = check_full(vertices,catalog,feynman,vertex.legs,amp_expression(joined.format(value), masses),
                                        706181,number == 5 && family.find('A') != std::string::npos) && ok;
                    std::cout << (ok ? "PASS" : "FAIL") << "  model " << number << "  " << family
                              << ' ' << helicities << "  signed Feynman diagrams" << std::endl;
                    if (ok) ++passed;
                    continue;
                }
                const amp_expression stored(joined.format(value), masses);
                const amp_expression published(formulas.at(label).back(), masses);
                identity_check_options checks;
                checks.seed = 601787 + 41 * seen.size();
                checks.samples = 8;
                const auto signed_check = stored.check_identity(published, checks);
                const auto measured = std::make_pair(stored.complexity(), stored.leaf_count());
                const auto ceiling = best_size(sizes.at(label));
                bool ok = signed_check.passed() && measured <= ceiling;
                if (!signed_check.passed()) std::cerr << signed_check.detail << '\n';
                if (argc == 3) {
                    // A lone photon has no other external photon to use as its reference.
                    const bool expect_spurious = number == 5 && vertex.legs.size() >= 5 &&
                                                 std::count(family.begin(), family.end(), 'A') >= 2;
                    ok = check_full(vertices, catalog, feynman, vertex.legs, stored,
                                    *checks.seed, expect_spurious) && ok;
                }
                std::cout << (ok ? "PASS" : "FAIL") << "  model " << number << "  " << family << ' '
                          << helicities << "  complexity/leaves " << measured.first << '/' << measured.second
                          << " (limit " << ceiling.first << '/' << ceiling.second << "), " << signed_check.compared
                          << " signed chart points" << (argc == 3 ? ", full validation" : "") << std::endl;
                if (ok) ++passed;
            }
        }
        for (const auto& policy : {"physical","spurious"})
            for (unsigned legs : {7u, 8u, 9u})
                for (unsigned pairs = 1; 2*pairs <= legs; ++pairs) {
                    const auto photons = legs - 2*pairs;
                    const auto family = std::string(2*pairs,'e') + std::string(photons,'A');
                    for (unsigned minus = 0; minus <= photons; ++minus) {
                        const auto h = photons ? std::string(photons-minus,'+') + std::string(minus,'-') : "massive";
                        if (!seen.count({policy,family,h}))
                            throw std::runtime_error("Missing QED catalog entry in " + family + " " + h);
                    }
                }
        const auto expected = formulas.size()+24+32+40;
        std::cout << passed << '/' << expected << " QED catalog entries passed\n";
        return passed == expected && seen.size() == expected ? 0 : 1;
    } catch (const std::exception& e) {
        std::cerr << e.what() << '\n';
        return 1;
    }
}
