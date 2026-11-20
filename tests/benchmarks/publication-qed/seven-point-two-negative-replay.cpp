// Signed replay of the two pair-current formulas. Build and run from the
// HEPCAT checkout as described in seven-point-two-negative.md.
#include "amp_expression.hpp"
#include "model.hpp"
#include "validation_numeric.hpp"
#include "validation_precision.hpp"

#include <algorithm>
#include <array>
#include <fstream>
#include <iostream>
#include <sstream>

using namespace hepcat;
using namespace hepcat::validation;

namespace {
using ordering = std::array<unsigned, 7>;

std::string read_expression(const char* path) {
    std::ifstream file(path);
    if (!file) throw std::runtime_error(std::string("Cannot open ") + path);
    std::ostringstream contents;
    contents << file.rdbuf();
    return contents.str();
}

std::vector<ordering> photon_orders(bool cyclic) {
    ordering p{0, 1, 2, 3, 4, 5, 6};
    std::vector<ordering> result;
    do {
        if (cyclic && p[3] != 2 + (p[2] - 2 + 1) % 3) continue;
        result.push_back(p);
        std::swap(p[5], p[6]);
        result.push_back(p);
        std::swap(p[5], p[6]);
    } while (std::next_permutation(p.begin()+2, p.begin()+5));
    return result;
}

template<class Chart> Chart relabel(const Chart& input, const ordering& from) {
    auto out = input;
    for (unsigned i = 0; i < from.size(); ++i) {
        const auto j = from[i];
        out.momenta[i] = input.momenta[j];
        out.angles[i] = input.angles[j];
        out.squares[i] = input.squares[j];
        out.angle_basis[i] = input.angle_basis[j];
        out.square_basis[i] = input.square_basis[j];
        for (unsigned k = 0; k < from.size(); ++k)
            out.scalar_products[from.size()*i+k] = input.scalar_products[from.size()*j+from[k]];
    }
    return out;
}

int compare(const expression_evaluator& elementary, const expression_evaluator& seed,
            const std::vector<ordering>& orders, real mass, unsigned count) {
    constexpr real tolerance = 1e-8L;
    const std::vector<real> masses{mass, mass, 0, 0, 0, 0, 0};
    unsigned replays = 0, failures = 0;
    real native_max = 0, replay_max = 0;
    for (unsigned i = 0; i < count; ++i) {
        const auto point = sample_point(masses, 134791+i);
        complex sum = 0;
        for (const auto& order : orders) sum += seed(relabel(point, order));
        const auto reference = elementary(point);
        const auto scale = std::max(std::abs(reference), std::abs(sum));
        const auto relative = scale == 0 ? 0 : std::abs(sum-reference)/scale;
        native_max = std::max(native_max, relative);
        if (std::isfinite(relative) && relative <= tolerance) continue;
        ++replays;
        const auto wide = precision::reconstruct(point);
        const auto expected = elementary.precise(point, wide);
        precision::number total;
        for (const auto& order : orders)
            total = total + seed.precise(point, relabel(wide, order));
        const auto normalization = std::max(expected.magnitude(), total.magnitude());
        const real error = normalization == 0 ? 0 :
            precision::narrow_real((total-expected).magnitude()/normalization);
        replay_max = std::max(replay_max, error);
        if (!std::isfinite(error) || error > tolerance) ++failures;
    }
    std::cout << "Points " << count << ", maximum native relative difference " << native_max
              << ", 160-bit replays " << replays << ", maximum replay difference " << replay_max
              << ", mismatches " << failures << '\n';
    return failures ? 1 : 0;
}
} // namespace

int main(int argc, char** argv) {
    if (argc < 4 || argc > 5) {
        std::cerr << "Usage: replay physical|spurious elementary.expr paper-seed.expr|auto [points]\n";
        return 2;
    }
    try {
        const std::string preference = argv[1];
        if (preference != "physical" && preference != "spurious")
            throw std::invalid_argument("Unknown pole preference");
        const unsigned count = argc == 5 ? std::stoul(argv[4]) : 100000;
        if (!count) throw std::invalid_argument("Positive sample count required");
        model qed("models", 3);
        Algebra algebra(&qed.algebra());
        const auto mass = qed.particle("e").mass;
        const auto m = algebra.parse(mass), zero = algebra.integer(0);
        algebra.setKinematics({m,m,zero,zero,zero,zero,zero}, {});
        const auto params = model_parameters(qed);
        const auto original = read_expression(argv[2]);
        const bool automatic = std::string(argv[3]) == "auto";
        std::string summand;
        if (automatic) {
            amp_expression amp(original, {mass,mass,"0","0","0","0","0"});
            const auto compact = amp.reduce().grouped_permutation_form();
            if (!compact || compact->groups != std::vector<std::vector<int>>{{3,4,5},{6,7}})
                throw std::runtime_error("Expected separate positive and negative photon permutation groups");
            summand = compact->summand;
        } else {
            summand = read_expression(argv[3]);
        }
        const expression_evaluator full(algebra, algebra.parse(original), params);
        const expression_evaluator seed(algebra, algebra.parse(summand), params);
        return compare(full, seed, photon_orders(!automatic && preference == "spurious"),
                       evaluate_scalar(algebra, m, params).real(), count);
    } catch (const std::exception& e) {
        std::cerr << e.what() << '\n';
        return 2;
    }
}
