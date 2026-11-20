#include "validation_numeric.hpp"
#include "validation_precision.hpp"
#include <algorithm>
#include <cmath>
#include <iostream>
#include <limits>
#include <stdexcept>

using namespace hepcat;
using namespace hepcat::validation;

namespace {
void require(bool ok, const char *message) {
    if (!ok)
        throw std::runtime_error(message);
}
real product(const four_vector &p, const four_vector &q) {
    return p[0] * q[0] - p[1] * q[1] - p[2] * q[2] - p[3] * q[3];
}
void check_chart() {
    for (const auto &m :
         std::vector<std::vector<real>>{{.01L, .01L, 0, 0}, {1, 3, 0, 4, 0}, {0, 0, 1, 3, 0, 2}}) {
        for (unsigned k = 0; k < 128; ++k) {
            auto p = sample_point(m, k);
            four_vector sum{};
            real energy = p.momenta[0][0] + p.momenta[1][0];
            for (size_t i = 0; i < m.size(); ++i) {
                auto v = p.momenta[i];
                for (size_t j = 0; j < 4; ++j)
                    sum[j] += v[j];
                require(std::abs(product(v, v) - m[i] * m[i]) < 1e-12L * energy * energy, "mass shell");
                require((v[0] > 0) == (i < 2), "incoming momentum sign");
                auto angle = p.angles[i], square = p.squares[i];
                if (m[i] == 0) {
                    require(std::abs(angle[0] * square[1] - complex(v[1], -v[2])) < 1e-12L * energy,
                            "massless Weyl frame");
                } else {
                    // Weyl equation in the same frame checks both massive spin projections and crossing.
                    spinor z{(v[0] + v[3]) * square[1] - complex(v[1], -v[2]) * square[0],
                             complex(v[1], v[2]) * square[1] - (v[0] - v[3]) * square[0]};
                    for (size_t j = 0; j < 2; ++j)
                        require(std::abs(z[j] + m[i] * angle[j]) < 1e-12L * energy * std::sqrt(energy),
                                "massive Weyl equation");
                }
            }
            for (auto x : sum)
                require(std::abs(x) < 1e-12L * energy, "momentum conservation");
        }
    }
}
void check_evaluator() {
    Algebra a;
    auto f = [&](const std::string &text) { return expression_evaluator(a, a.parse(text), {}); };
    auto brackets = f("Times[SpinorChain[Spinor[\"Helicity\",\"Angle\",3],Spinor[\"Helicity\",\"Angle\",4]],"
                      "SpinorChain[Spinor[\"Helicity\",\"Square\",3],Spinor[\"Helicity\",\"Square\",4]]]");
    auto invariant = f("PropDen[Mom[Multiparticle[3,4]],0]");
    auto ref = f("SpinorChain[Spinor[\"Xi\",\"Angle\",3],Spinor[\"Spin\",\"Angle\",1]]");
    auto trace = f("SpinorTrace[Mom[1],Mom[2]]");
    auto dot = f("MomProd[1,2]");
    for (unsigned k = 0; k < 128; ++k) {
        auto p = sample_point({1, 1, 0, 0}, k);
        require(std::abs(brackets(p) + invariant(p)) < 1e-12L * std::abs(invariant(p)), "spinor invariant");
        require(std::abs(trace(p) - 2.L * dot(p)) < 1e-12L * std::abs(dot(p)), "spin trace");
        auto before = ref(p), b = brackets(p);
        p.change_references(k + 1000);
        require(std::abs(ref(p) - before) > 1e-12L && brackets(p) == b, "reference-only change");
    }
    // The scalar arithmetic loses both components of a small complex summand.
    // Replay must use the same spinor sample, including its imaginary phase.
    std::string chain = "SpinorChain[Spinor[\"Spin\",\"Angle\",1],Spinor[\"Helicity\",\"Angle\",3]]";
    auto target = f(chain);
    mpz_class factor = mpz_class(1) << (std::numeric_limits<real>::digits + 8);
    auto scale = factor.get_str();
    auto cancellation =
        f("Plus[Times[" + scale + "," + chain + "]," + chain + ",Times[-" + scale + "," + chain + "]]");
    auto fixed = sample_point({1, 1, 0, 0}, 17);
    auto expected = target(fixed);
    require(std::abs(cancellation(fixed) - expected) > 0.1L * std::abs(expected),
            "precision control too weak");
    require(std::abs(cancellation.precise(fixed) - expected) < 1e-14L * std::abs(expected),
            "GMP scalar replay");
    auto odd_root = f("Power[Complex[-1,0],Rational[-3,2]]");
    require(std::abs(odd_root.precise(fixed) - complex(0, 1)) < 1e-14L,
            "complex half-integer power in replay");
    // This projection scales as m^2/E. Direct multiplication by the full
    // momentum matrix loses it when the large column cancels.
    auto mass_suppressed = f("SpinorChain[Spinor[\"Spin\",\"Upper\",\"Angle\",1,Rational[-1,2]],"
                             "Mom[1],Spinor[\"Helicity\",\"Square\",3]]");
    auto dirac_rhs = f("Times[Mass[1],SpinorChain[Spinor[\"Spin\",\"Upper\",\"Square\",1,Rational[-1,2]],"
                       "Spinor[\"Helicity\",\"Square\",3]]]");
    for (unsigned k = 0; k < 32; ++k) {
        auto p = sample_point({1e-9L, 1e-9L, 0, 0}, k);
        auto expected = dirac_rhs(p);
        require(std::abs(mass_suppressed(p) - expected) < 1e-12L * std::abs(expected),
                "mass-suppressed Dirac identity");
        auto chart = precision::reconstruct(p);
        auto precise_lhs = mass_suppressed.precise(p, chart), precise_rhs = dirac_rhs.precise(p, chart);
        auto error = (precise_lhs - precise_rhs).magnitude();
        require(error < precise_rhs.magnitude() * precision::scalar("1e-29", precision::bits),
                "precision replay retained machine-precision Weyl contractions");
    }
    std::string i = "ColorIndex[1]", j = "ColorIndex[2]", k = "ColorIndex[3]", l = "ColorIndex[4]";
    auto gen = [](const std::string &i, const std::string &j) {
        return "ColorGenerator[" + i + "," + j + ",dummy]";
    };
    auto delta = [](const std::string &i, const std::string &j) { return "ColorDelta[" + i + "," + j + "]"; };
    expression_evaluator lhs(a, a.parse("Times[" + gen(i, j) + "," + gen(k, l) + "]"), {}, {3, -3, 3, -3});
    expression_evaluator rhs(a,
                             a.parse("Plus[Times[Rational[1,2]," + delta(i, l) + "," + delta(j, k) +
                                     "],Times[Rational[-1,6]," + delta(i, j) + "," + delta(k, l) + "]]"),
                             {}, {3, -3, 3, -3});
    auto p = sample_point({1, 1, 1, 1}, 500);
    auto color_chart = precision::reconstruct(p);
    int active = 0;
    for (unsigned t = 0; t < 500; ++t) {
        p.change_colors(t);
        require(std::abs(lhs(p) - rhs(p)) < 1e-14L, "SU(3) fundamental completeness");
        if (t < 64) {
            color_chart.color_seed = p.color_seed;
            auto error = (lhs.precise(p, color_chart) - rhs.precise(p, color_chart)).magnitude();
            require(error < precision::scalar("1e-38", precision::bits),
                    "precision replay SU(3) completeness");
        }
        if (std::abs(lhs(p)) > 0.01L)
            ++active;
    }
    require(active > 20, "color sample diversity");
    // Two contributions with internal colors 3 and 8 cancel for this external
    // assignment. Rounding their separate matrix entries can leave a false pole test.
    expression_evaluator mixed(a,a.parse("Times[ColorGenerator[ColorIndex[2],ColorIndex[1],u],"
                                         "ColorStructure[u,ColorIndex[3],ColorIndex[4]]]"),{},{3,-3,8,8});
    p.change_colors(11612486440731981006ULL);
    color_chart.color_seed=p.color_seed;
    require(mixed(p)==complex{} && mixed.precise(p,color_chart).magnitude()==0,
            "exact cancellation of adjoint exchange at fixed external colors");

    bool caught = false;
    try {
        f("UnknownVertex[1]");
    } catch (const std::invalid_argument &) {
        caught = true;
    }
    require(caught, "unsupported primitive accepted");
    auto supported_native = f("System`Sin[1]");
    require(std::abs(supported_native(p) - std::sin(1.L)) < 1e-14L,
            "unavailable replay invalidated a native function");
    caught = false;
    try {
        supported_native.precise(p);
    } catch (const std::invalid_argument &) {
        caught = true;
    }
    require(caught, "unsupported high-precision function silently fell back to machine precision");
}
} // namespace
int main() {
    try {
        check_chart();
        check_evaluator();
        std::cout << "physical charts, numerical spinors, and SU(3) passed\n";
    } catch (const std::exception &e) {
        std::cerr << e.what() << "\n";
        return 1;
    }
}
