// Compare each signed permutation sum directly with 240 Feynman diagrams.
// Build and run instructions are in seven-point-four-fermions.md.
#include "amp_expression.hpp"
#include "generation_feynman.hpp"
#include "validation_feynman.hpp"
#include "validation_numeric.hpp"
#include "validation_precision.hpp"
#include <algorithm>
#include <array>
#include <cmath>
#include <cstdint>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <optional>
#include <vector>
#include <sstream>

using namespace hepcat;
using namespace hepcat::validation;
namespace fs = std::filesystem;

namespace {
using labels = std::array<unsigned, 7>;
struct image {
    labels from;
    int sign;
};

std::vector<image> signed_images() {
    std::vector<image> result;
    labels photons{0, 1, 2, 3, 4, 5, 6};
    do {
        for (bool electron : {false, true})
            for (bool positron : {false, true}) {
                auto g = photons;
                if (electron)
                    std::swap(g[0], g[3]);
                if (positron)
                    std::swap(g[1], g[2]);
                const int sign = electron == positron ? 1 : -1;
                result.push_back({g, sign});
                // C acts first on the summand, before the two Fermi exchanges.
                std::swap(g[0], g[1]);
                std::swap(g[3], g[2]);
                result.push_back({g, -sign});
            }
    } while (std::next_permutation(photons.begin() + 4, photons.end()));
    return result;
}

template <class Chart> Chart relabel(const Chart& in, const labels& from) {
    auto out = in;
    for (unsigned i = 0; i < from.size(); ++i) {
        const auto j = from[i];
        out.momenta[i] = in.momenta[j];
        out.angles[i] = in.angles[j];
        out.squares[i] = in.squares[j];
        out.angle_basis[i] = in.angle_basis[j];
        out.square_basis[i] = in.square_basis[j];
        for (unsigned k = 0; k < from.size(); ++k)
            out.scalar_products[from.size() * i + k] = in.scalar_products[from.size() * j + from[k]];
    }
    return out;
}

std::string read(const fs::path& path) {
    std::ifstream input(path);
    if (!input)
        throw std::runtime_error("Cannot open " + path.string());
    std::ostringstream text;
    text << input.rdbuf();
    return text.str();
}

struct comparison {
    std::string name;
    expression_evaluator seed;
    real raw_max = 0, replay_max = 0;
    unsigned replays = 0, failures = 0;
};

int run(const fs::path& construction, const fs::path& paper, unsigned points, std::uint64_t first_seed) {
    const std::vector<std::string> names{"e", "E", "E", "e", "A.+", "A.+", "A.+"};
    model qed("models", 3), rules("tests/reference-models/calchep", 2);
    Algebra a(&qed.algebra()), fa(&rules.algebra());
    const auto mass = qed.particle("e").mass;
    const auto m = a.parse(mass), z = a.integer(0);
    a.setKinematics({m, m, m, m, z, z, z}, {});
    std::vector<Id> fm, trees;
    for (const auto& name : names)
        fm.push_back(fa.parse(rules.particle(generation::feynman_external(rules, name)).mass));
    fa.setKinematics(fm, {});
    for (const auto& graph : generation::diagram_plans(rules, names))
        trees.push_back(feynman_to_spinors(fa, generation::sew_feynman(fa, rules, names, graph)));
    if (trees.size() != 240)
        throw std::runtime_error("Incomplete Feynman diagram inventory");
    const expression_evaluator reference(fa, fa.add(trees), model_parameters(rules));
    const auto parameters = model_parameters(qed);
    const auto mass_value = evaluate_scalar(a, m, parameters).real();
    const std::vector<real> masses{mass_value, mass_value, mass_value, mass_value, 0, 0, 0};
    const std::vector<std::string> mass_rules{mass, mass, mass, mass, "0", "0", "0"};
    std::vector<comparison> cases;
    for (const std::string pref : {"physical", "spurious"}) {
        auto input = read(construction / pref / "eeeeAAA" / "+++.expr");
        amp_expression expression(input, mass_rules);
        const auto native = expression.reduce().signed_permutation_form();
        if (!native || native->symmetric_groups != std::vector<std::vector<int>>{{5, 6, 7}} ||
            native->alternating_groups != std::vector<std::vector<int>>{{1, 4}, {2, 3}} ||
            !native->joint_exchange || native->joint_exchange->sign != -1 ||
            native->joint_exchange->swaps != std::vector<std::pair<int, int>>{{1, 2}, {4, 3}})
            throw std::runtime_error("Unexpected native permutation group for " + pref);
        for (bool automatic : {false, true}) {
            auto text = automatic ? native->summand : read(paper / (pref + ".expr"));
            std::string name = pref + (automatic ? " native" : " paper");
            amp_expression seed(text, mass_rules);
            std::cout << name << " summand weight " << seed.complexity() << ", leaves " << seed.leaf_count()
                      << '\n';
            cases.push_back({name, expression_evaluator(a, a.parse(text), parameters)});
        }
    }
    const auto group = signed_images();
    constexpr real tolerance = 1e-8L;
    for (unsigned i = 0; i < points; ++i) {
        const auto p = sample_point(masses, first_seed + i);
        const auto ref = reference(p);
        std::vector<complex> sums(cases.size());
        for (const auto& g : group) {
            auto scene = relabel(p, g.from);
            for (size_t j = 0; j < cases.size(); ++j)
                sums[j] += real(g.sign) * cases[j].seed(scene);
        }
        std::optional<precision::phase_point> wide;
        precision::number wide_ref;
        for (size_t j = 0; j < cases.size(); ++j) {
            auto& c = cases[j];
            const auto scale = std::max(std::abs(ref), std::abs(sums[j]));
            const auto rel = scale == 0 ? 0 : std::abs(ref - sums[j]) / scale;
            c.raw_max = std::max(c.raw_max, rel);
            if (std::isfinite(rel) && rel <= tolerance)
                continue;
            ++c.replays;
            if (!wide) {
                wide = precision::reconstruct(p);
                wide_ref = reference.precise(p, *wide);
            }
            precision::number sum;
            for (const auto& g : group) {
                auto value = c.seed.precise(p, relabel(*wide, g.from));
                sum = g.sign < 0 ? sum - value : sum + value;
            }
            const auto denom = std::max(sum.magnitude(), wide_ref.magnitude());
            const auto error = denom == 0 ? 0 : precision::narrow_real((sum - wide_ref).magnitude() / denom);
            c.replay_max = std::max(c.replay_max, error);
            if (!std::isfinite(error) || error > tolerance)
                ++c.failures;
        }
    }
    unsigned failures = 0;
    for (const auto& c : cases) {
        failures += c.failures;
        std::cout << c.name << ", " << points << " points, native maximum " << c.raw_max
                  << ", 160-bit replays " << c.replays << ", replay maximum " << c.replay_max << ", failures "
                  << c.failures << '\n';
    }
    return failures ? 1 : 0;
}
} // namespace

int main(int argc, char** argv) {
    if (argc < 3 || argc > 5) {
        std::cerr << "Usage: replay-four-fermions RUN_DIR PAPER_SEED_DIR [POINTS [SEED]]\n";
        return 2;
    }
    try {
        const unsigned points = argc > 3 ? std::stoul(argv[3]) : 100000;
        if (!points)
            throw std::invalid_argument("Positive sample count required");
        return run(argv[1], argv[2], points, argc > 4 ? std::stoull(argv[4]) : 704733);
    } catch (const std::exception& e) {
        std::cerr << e.what() << '\n';
        return 2;
    }
}
