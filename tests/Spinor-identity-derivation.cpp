// Explore a six-point spinor derivation in C++. Run from the HEPCAT repository root:
//   cmake -S csource -B tests/results/native-reduce-build -DCMAKE_BUILD_TYPE=Release
//   cmake --build tests/results/native-reduce-build --target hepcat-derive-example --parallel
//   tests/results/native-reduce-build/hepcat-derive-example models
//
// Start from the sum of the independent Feynman diagrams for e E -> four photons.
// Extract common factors at a selected occurrence, then reduce the remaining spinor
// chains. Check both steps and compare the final expression with the separately
// stored constructive amplitude. Internal propagators stay off shell throughout.
#include "amplitude.hpp"
#include "reduce.hpp"
#include "validation_feynman.hpp"

#include <filesystem>
#include <iostream>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

using namespace hepcat;
namespace fs = std::filesystem;

namespace {
amp_expression catalog_expression(const model& catalog, const std::vector<std::string>& particles) {
    for (const auto& row : catalog.vertices()) {
        if (row.legs != particles) continue;
        std::vector<std::string> masses;
        for (const auto& leg : particles) masses.push_back(catalog.particle(leg).mass);
        Algebra joined(&catalog.algebra());
        return amp_expression(joined.format(joined.mul({row.factor, row.expression})), masses);
    }
    throw std::runtime_error("Missing six-point model entry");
}

amp_expression feynman_sum(amplitude& process) {
    process.generate_feynman_diagrams();
    Algebra algebra;
    std::vector<Id> masses, terms;
    for (const auto& mass : process.external_masses()) masses.push_back(algebra.parse(mass));
    algebra.setKinematics(masses, {});
    for (const auto& diagram : process.feynman_diagrams())
        terms.push_back(feynman_to_spinors(algebra, algebra.parse(diagram.expression.str())));
    std::cout << "    " << terms.size() << " Feynman diagrams\n";
    return amp_expression(algebra.format(algebra.add(terms)), process.external_masses());
}

void check_step(const amp_expression& before, const amp_expression& after, std::uint64_t seed) {
    identity_check_options options;
    options.samples = 32;
    options.seed = seed;
    const auto report = before.check_identity(after, options);
    if (!report.passed()) throw std::runtime_error(report.detail);
    std::cout << "    Signed agreement on " << report.compared
              << " external on-shell samples (seed " << seed << ").\n";
}

void show_size(const amp_expression& expression) {
    std::cout << "    Complexity " << expression.complexity()
              << ", expression leaves " << expression.leaf_count() << '\n';
}
} // namespace

int main(int argc, char** argv) {
    try {
        if (argc != 2) throw std::invalid_argument("Usage: hepcat-derive-example MODEL_DIRECTORY");
        const auto directory = fs::canonical(argv[1]);
        const std::vector<std::string> legs{"e", "E", "A.+", "A.+", "A.-", "A.-"};
        // The two descriptions of QED use the same mass and charge.
        const model vertices(directory, 3, "QED-example"), catalog(directory, 4, "QED-example");
        const model feynman(directory.parent_path() / "tests/reference-models/calchep", 2, "QED-example");
        amplitude process(legs, vertices, catalog, feynman);
        std::cout << "Six-point QED   e, E, A.+, A.+, A.-, A.-\nStarting Feynman expression\n";
        const auto input = feynman_sum(process);
        show_size(input);

        // Only build the most compact candidate. Extracting a shared factor can
        // shorten the formula even if the momentum-weighted score stays the same.
        identity_options options;
        options.rule = identity_rule::factor_common;
        options.limit = 0;
        const auto candidates = input.identity_candidates(options);
        auto best = std::make_pair(input.complexity(), input.leaf_count());
        std::optional<identity_candidate> chosen;
        for (const auto& edit : candidates) {
            const auto [weight, leaves] = input.preview_identity(edit);
            const auto score = std::make_pair(weight, leaves);
            if (score < best) {
                best = score;
                chosen = edit;
            }
        }
        auto current = input;
        if (chosen) {
            std::cout << "\nDirected identity   " << chosen->description() << '\n';
            current = input.apply_identity(*chosen);
            show_size(current);
            check_step(input, current, 41711);
        } else {
            std::cout << "\nAll common factors have already been extracted.\n";
        }
        const auto factored = current;
        current.reduce();
        std::cout << "\nAutomatic spinor reduction\n";
        show_size(current);
        check_step(factored, current, 41711);

        std::cout << "\nComparison with the constructive amplitude in model 4\n";
        const auto reference = catalog_expression(catalog, legs);
        show_size(reference);
        check_step(current, reference, 61711);
        return 0;
    } catch (const std::exception& error) {
        std::cerr << error.what() << '\n';
        return 1;
    }
}
