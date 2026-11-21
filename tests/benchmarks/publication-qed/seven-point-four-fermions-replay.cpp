// Compare the signed seven-point permutation sums with the independent Feynman model.
// Build and run instructions are in seven-point-four-fermions.md and seven-point-six-fermions.md.
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
    int leaf_sign = 1;
};

// Act on the paper's labels directly. Each Bose group contributes unit
// weights, while a permutation of either charged species contributes its sign.
void append_permutations(std::vector<image>& images, const std::vector<unsigned>& positions, bool alternating) {
    auto order = positions;
    std::vector<image> next;
    do {
        int sign = 1;
        if (alternating)
            for (size_t i = 0; i < order.size(); ++i)
                for (size_t j = 0; j < i; ++j)
                    if (order[j] > order[i]) sign = -sign;
        for (const auto& in : images) {
            auto out = in;
            for (size_t i = 0; i < order.size(); ++i)
                out.from[positions[i]-1] = in.from[order[i]-1];
            out.sign *= sign;
            next.push_back(out);
        }
    } while (std::next_permutation(order.begin(),order.end()));
    images = std::move(next);
}

std::vector<image> signed_images(const std::vector<std::vector<int>>& photon_groups,
                                 const std::vector<std::vector<int>>& charged_groups,
                                 const signed_exchange& charge_reversal) {
    std::vector<image> images{{{0,1,2,3,4,5,6},1}};
    for (const auto& legs : photon_groups)
        append_permutations(images,{legs.begin(),legs.end()},false);
    for (const auto& legs : charged_groups)
        append_permutations(images,{legs.begin(),legs.end()},true);
    const auto count = images.size();
    for (size_t h = 0; h < count; ++h) {
        auto reversed = images[h];
        for (auto [l,r] : charge_reversal.swaps) std::swap(reversed.from[l-1],reversed.from[r-1]);
        reversed.sign *= charge_reversal.sign;
        images.push_back(reversed);
    }
    return images;
}

// Compose the two outer-current reversals on the right of charge reversal.
// Only the emitting-leaf contribution changes sign under a <-> b.
void append_local_reversal(std::vector<image>& images, unsigned a, unsigned b, int leaf_sign) {
    const auto size = images.size();
    for (size_t i = 0; i < size; ++i) {
        auto next = images[i];
        std::swap(next.from[a-1], next.from[b-1]);
        next.leaf_sign *= leaf_sign;
        images.push_back(next);
    }
}
bool single_swap(const std::vector<signed_exchange>& exchanges, int a, int b, int sign) {
    return exchanges.size() == 1 && exchanges[0].sign == sign &&
        exchanges[0].swaps == std::vector<std::pair<int,int>>{{a,b}};
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
    expression_evaluator center;
    std::optional<expression_evaluator> leaf;
    real raw_max = 0, replay_max = 0;
    unsigned replays = 0, failures = 0;
    std::uint64_t worst_seed = 0;
};

int run(const fs::path& construction, const fs::path& paper, unsigned points, std::uint64_t first_seed,
        const std::string& helicities) {
    const bool six_fermions = helicities == "+" || helicities == "-";
    if (!six_fermions && helicities != "+++" && helicities != "++-")
        throw std::invalid_argument("Expected + or - for six fermions, +++ or ++- for four");
    using groups = std::vector<std::vector<int>>;
    const groups photons = six_fermions ? groups{} : helicities == "+++" ? groups{{5,6,7}} : groups{{5,6}};
    const groups charges = six_fermions ? groups{{1,3,5},{2,4,6}} : groups{{1,4},{2,3}};
    const signed_exchange reversal = six_fermions ? signed_exchange{{{1,6},{3,4},{5,2}},1}
                                                 : signed_exchange{{{1,2},{4,3}},-1};
    auto group = signed_images(photons,charges,reversal);
    if (six_fermions) {
        append_local_reversal(group,5,6,1);
        append_local_reversal(group,1,2,-1);
    }
    if (group.size() != (six_fermions ? 288u : helicities == "+++" ? 48u : 16u))
        throw std::runtime_error("Incomplete signed permutation group");
    std::vector<std::string> names = six_fermions ? std::vector<std::string>{"e","E","e","E","e","E"}
                                                  : std::vector<std::string>{"e","E","E","e"};
    for (const auto helicity : helicities)
        names.push_back(std::string("A.") + helicity);
    model qed("models", 3), rules("tests/reference-models/calchep", 2);
    Algebra a(&qed.algebra()), fa(&rules.algebra());
    std::vector<Id> model_masses, fm, trees;
    std::vector<std::string> mass_rules;
    std::vector<real> masses;
    const auto parameters = model_parameters(qed);
    for (const auto& name : names) {
        mass_rules.push_back(qed.particle(name).mass);
        model_masses.push_back(a.parse(mass_rules.back()));
        masses.push_back(evaluate_scalar(a,model_masses.back(),parameters).real());
        fm.push_back(fa.parse(rules.particle(generation::feynman_external(rules, name)).mass));
    }
    a.setKinematics(model_masses, {});
    fa.setKinematics(fm, {});
    for (const auto& graph : generation::diagram_plans(rules, names))
        trees.push_back(feynman_to_spinors(fa, generation::sew_feynman(fa, rules, names, graph)));
    if (trees.size() != (six_fermions ? 252u : 240u))
        throw std::runtime_error("Incomplete Feynman diagram inventory");
    const expression_evaluator reference(fa, fa.add(trees), model_parameters(rules));
    std::vector<comparison> cases;
    for (const std::string pref : {"physical", "spurious"}) {
        auto input = read(construction / pref / (six_fermions ? "eeeeeeA" : "eeeeAAA") / (helicities + ".expr"));
        amp_expression expression(input, mass_rules);
        if (six_fermions) expression.reduce(pref == "physical" ? generation_references::physical
                                                              : generation_references::cyclic);
        else expression.reduce();
        const auto native = expression.signed_permutation_form();
        if (!native || native->symmetric_groups != photons || native->alternating_groups != charges ||
            !native->joint_exchange || native->joint_exchange->sign != reversal.sign ||
            native->joint_exchange->swaps != reversal.swaps)
            throw std::runtime_error("Unexpected native permutation group for " + pref);
        auto evaluator = [&](const std::string& text) {
            return expression_evaluator(a, a.parse(text), parameters);
        };
        std::string native_leaf, native_center;
        if (six_fermions) {
            if (!single_swap(native->inner_exchanges,5,6,1) || native->projected_terms.size() != 2)
                throw std::runtime_error("Unexpected common outer-current projection for " + pref);
            for (const auto& term : native->projected_terms) {
                if (single_swap(term.exchanges,1,2,-1)) native_leaf = term.summand;
                else if (single_swap(term.exchanges,1,2,1)) native_center = term.summand;
                else throw std::runtime_error("Unknown emitting-line projection");
            }
            if (native_leaf.empty() || native_center.empty())
                throw std::runtime_error("Incomplete emitting-line projections");
        }
        for (bool automatic : {false, true}) {
            const auto name = pref + (automatic ? " native" : " paper");
            const auto center = automatic ? (six_fermions ? native_center : native->summand)
                : read(paper / (pref + (six_fermions ? "-center.expr" : ".expr")));
            amp_expression core(center, mass_rules);
            comparison result{name, evaluator(center), {}};
            auto weight = core.complexity(), leaves = core.leaf_count();
            if (six_fermions) {
                const auto leaf = automatic ? native_leaf : read(paper / (pref + "-leaf.expr"));
                amp_expression emission(leaf, mass_rules);
                weight += emission.complexity();
                leaves += emission.leaf_count();
                result.leaf.emplace(evaluator(leaf));
            }
            std::cout << name << " summand weight " << weight << ", leaves " << leaves << '\n';
            cases.push_back(std::move(result));
        }
    }
    std::cout << helicities << ", " << group.size() << " signed images, " << trees.size()
              << " Feynman diagrams, first seed " << first_seed << '\n';
    constexpr real tolerance = 1e-8L;
    for (unsigned i = 0; i < points; ++i) {
        const auto p = sample_point(masses, first_seed + i);
        const auto ref = reference(p);
        std::vector<complex> sums(cases.size());
        for (const auto& g : group) {
            auto scene = relabel(p, g.from);
            for (size_t j = 0; j < cases.size(); ++j) {
                auto term = cases[j].center(scene);
                if (cases[j].leaf) term += real(g.leaf_sign) * (*cases[j].leaf)(scene);
                sums[j] += real(g.sign) * term;
            }
        }
        std::optional<precision::phase_point> wide;
        precision::number wide_ref;
        for (size_t j = 0; j < cases.size(); ++j) {
            auto& c = cases[j];
            const auto scale = std::max(std::abs(ref), std::abs(sums[j]));
            const auto rel = scale == 0 ? 0 : std::abs(ref - sums[j]) / scale;
            if (rel > c.raw_max || !std::isfinite(rel)) {
                c.raw_max = rel;
                c.worst_seed = first_seed + i;
            }
            if (std::isfinite(rel) && rel <= tolerance)
                continue;
            ++c.replays;
            if (!wide) {
                wide = precision::reconstruct(p);
                wide_ref = reference.precise(p, *wide);
            }
            precision::number sum;
            for (const auto& g : group) {
                const auto scene = relabel(*wide, g.from);
                auto value = c.center.precise(p, scene);
                if (c.leaf) {
                    const auto addition = c.leaf->precise(p, scene);
                    value = g.leaf_sign < 0 ? value - addition : value + addition;
                }
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
                  << ", worst seed " << c.worst_seed << ", 160-bit replays " << c.replays << ", replay maximum " << c.replay_max << ", failures "
                  << c.failures << '\n';
    }
    return failures ? 1 : 0;
}
} // namespace

int main(int argc, char** argv) {
    if (argc < 3 || argc > 6) {
        std::cerr << "Usage: replay-four-fermions RUN_DIR PAPER_SEED_DIR [POINTS [SEED [+|-|+++|++-]]]\n";
        return 2;
    }
    try {
        const unsigned points = argc > 3 ? std::stoul(argv[3]) : 100000;
        if (!points)
            throw std::invalid_argument("Positive sample count required");
        return run(argv[1], argv[2], points, argc > 4 ? std::stoull(argv[4]) : 704733,
                   argc > 5 ? argv[5] : "+++");
    } catch (const std::exception& e) {
        std::cerr << e.what() << '\n';
        return 2;
    }
}
