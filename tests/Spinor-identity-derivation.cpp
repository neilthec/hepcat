// A checked, six-point spinor derivation in C++. Run from the HEPCAT repository root:
//   cmake -S csource -B tests/results/native-reduce-build -DCMAKE_BUILD_TYPE=Release
//   cmake --build tests/results/native-reduce-build --target hepcat-derive-example --parallel
//   tests/results/native-reduce-build/hepcat-derive-example models
//
// Start with the complete amplitude stored in QED model 4, before any new
// simplification. This representation differs from the one obtained when
// constructing the same process afresh. Only external particles are on shell.
#include "amp_expression.hpp"
#include "model.hpp"
#include "reduce.hpp"

#include <iostream>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

using namespace hepcat;

namespace {

amp_expression load_case(const model& catalog, const std::vector<std::string>& particles) {
    for (const auto& row : catalog.vertices()) {
        if (row.legs != particles) continue;
        std::vector<std::string> masses;
        for (const auto& particle : particles) masses.push_back(catalog.particle(particle).mass);

        // An ampsN.mdl row stores its overall factor separately from its spinor expression.
        const auto& algebra = catalog.algebra();
        return amp_expression("Times[" + algebra.format(row.factor) + "," +
                              algebra.format(row.expression) + "]", masses);
    }
    throw std::runtime_error("The selected model has no row in this external-particle ordering");
}

void check_step(const amp_expression& before, const amp_expression& after, std::uint64_t seed = 41711) {
    identity_check_options options;
    options.samples = 32;
    options.seed = seed; // Reproducible rational kinematics for this worked example.
    const auto report = before.check_identity(after, options);
    if (!report.passed()) throw std::runtime_error(report.detail);
    std::cout << "    Signed agreement on " << report.compared << " external on-shell samples (seed " << seed << ").\n";
}

void show_size(const amp_expression& expression) {
    std::cout << "    Complexity " << expression.complexity()
              << ", expression leaves " << expression.leaf_count() << '\n';
}

} // namespace

int main(int argc, char** argv) {
    try {
        if (argc != 2) throw std::invalid_argument("Usage: hepcat-derive-example MODEL_DIRECTORY");
        const model catalog(argv[1], 4);
        auto current = load_case(catalog, {"e", "E", "A.+", "A.+", "A.-", "A.-"});
        std::cout << "Six-point QED   e, E, A.+, A.+, A.-, A.-\nStarting model expression\n";
        show_size(current);

        // Establish how much the ordinary automatic reduction already achieves.
        const auto original = current;
        current.reduce();
        std::cout << "\nOrdinary reduction\n";
        check_step(original, current);
        show_size(current);

        // The Clifford anticommutator rewrites ... /p /q ... as a term
        // proportional to 2 p.q minus ... /q /p ... . Compare all adjacent
        // pairs using the momentum-weighted score, breaking ties by leaf count.
        // A trial can decrease that score even when its written formula grows.
        // Previewing each candidate keeps only the winning edit in the arena.
        identity_options search;
        search.rule = identity_rule::clifford_swap;
        search.limit = 0; // Enumerate all occurrences of this one identity.
        for (unsigned step = 1; step <= 2; ++step) {
            auto best = std::make_pair(current.complexity(), current.leaf_count());
            std::optional<identity_candidate> chosen;
            const auto edits = current.identity_candidates(search);
            for (const auto& edit : edits) {
                const auto [weight, leaves] = current.preview_identity(edit);
                const auto score = std::make_pair(weight, leaves);
                if (score < best) {
                    best = score;
                    chosen = edit;
                }
            }
            if (!chosen) {
                std::cout << "No improving Clifford swap among " << edits.size()
                          << " legal choices.\n";
                break;
            }
            auto trial = current.apply_identity(*chosen);
            std::cout << "\nStep " << step << " of 2   " << chosen->description()
                      << "\n    Chosen from " << edits.size() << " possible swaps.\n";
            check_step(current, trial);
            current = std::move(trial);
            show_size(current);
        }

        // Check the final result directly against the stored model amplitude
        // on a fresh set of kinematics, independent of the intermediate checks.
        std::cout << "\nFinal comparison with the stored expression\n";
        check_step(original, current, 61711);
        return 0;
    } catch (const std::exception& error) {
        std::cerr << error.what() << '\n';
        return 1;
    }
}
