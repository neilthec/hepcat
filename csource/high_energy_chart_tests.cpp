#include "high_energy_chart.hpp"
#include "residue.hpp"
#include <iostream>
#include <stdexcept>

using namespace hepcat;
namespace {
void check(bool ok, const char* name) {
    if (!ok) throw std::runtime_error(name);
}
} // namespace
int main() {
    Algebra a;
    auto m = a.atom("m"), z = a.integer(0), one = a.integer(1), neg = a.integer(-1);
    a.setKinematics({m, m, z, z}, {});
    auto spin = [&](int leg, const char* ch) {
        return a.call("Spinor", {a.atom(leg <= 2 ? "\"Spin\"" : "\"Helicity\""), a.atom(ch), a.integer(leg)});
    };
    auto c = [&](std::vector<Id> p) { return a.call("SpinorChain", std::move(p)); };
    auto p = [&](int i) { return a.call("Mom", {a.integer(i)}); };
    auto lhs = spin(1, "\"Angle\""), rhs = spin(2, "\"Square\"");
    auto dirac = a.add({c({lhs, p(1), rhs}), a.mul({neg, m, c({spin(1, "\"Square\""), rhs})})});
    auto sum = a.add({c({lhs, p(1), rhs}), c({lhs, p(2), rhs}), c({lhs, p(3), rhs}), c({lhs, p(4), rhs})});
    auto shell = a.add({a.pow(p(1), 2), a.mul({neg, a.pow(m, 2)})});
    auto dots = a.parse("Plus[MomProd[1,1],MomProd[1,2],MomProd[1,3],MomProd[1,4]]");
    auto f = a.parse("PropDen[Mom[Multiparticle[1,3]],m]"), g = a.add({one, f});
    auto complementary = a.add({f, a.mul({neg, a.parse("PropDen[Mom[Multiparticle[2,4]],m]")})});
    auto brackets = a.mul(
        {c({spin(3, "\"Angle\""), spin(4, "\"Angle\"")}), c({spin(3, "\"Square\""), spin(4, "\"Square\"")})});
    // Canonically normalized helicity spinors give <ij>[ij] = -s_ij.
    auto invariant = a.add({brackets, a.parse("Mandelstahm[3,4]")});
    std::map<Id, mpq_class> params{{m, mpq_class(7, 3)}};
    std::vector<Id> quotient{a.mul({a.pow(f, 8), a.pow(g, -1)})};
    for (long i = 0; i < 8; ++i)
        quotient.push_back(a.mul({a.integer(i % 2 ? -1 : 1), a.pow(f, i)}));
    auto cancel = a.add(quotient); // D^8/(D+1) minus its polynomial part gives 1/(D+1).
    for (unsigned sample = 0, regular = 0; regular < 5; ++sample) {
        check(sample < 100, "all charts singular");
        try {
            check(high_energy_chart_rows(a, {sum, shell, dirac, complementary, dots, invariant}, -5, sample,
                                         params)
                      .empty(),
                  "Dirac, shell, conservation or relative helicity sign");
            check(high_energy_chart_rows(a, {cancel}, -2, sample, params).empty(),
                  "Laurent cancellation through E^-2");
            check(!high_energy_chart_rows(a, {cancel}, -3, sample, params).empty(),
                  "missed subleading reciprocal term");
            auto fields = high_energy_chart_rows(
                a, {f, a.mul({f, a.parse("Power[2,Rational[1,2]]")}), a.mul({f, a.atom("I")})}, 1, sample,
                params);
            check(fields.size() == 3, "independent real, sqrt2 and imaginary rows");
            ++regular;
        } catch (const residue_singular&) {
        }
    }
    // A coherent bracket between two massive legs is linear in each spin
    // variable. Four distinct spin projections certify its energy bound.
    auto b = c({spin(1, "\"Angle\""), spin(2, "\"Angle\"")});
    auto v = high_energy_validate_chart(a, b, 1, 0, params, 128);
    check(v.passes && v.exhaustive_spin && v.spin_states == 4, "full coherent-spin grid");
    v = high_energy_validate_chart(a, b, 1, 0, params, 2);
    check(v.passes && !v.exhaustive_spin && v.spin_states == 1, "bounded coherent-spin grid");
    v = high_energy_validate_chart(a, a.pow(b, -1), 1, 0, params, 128);
    check(v.passes && !v.exhaustive_spin, "inverse spin polynomial cannot claim exhaustive coverage");
    // Fit a deliberately wrong expression to the very first coherent spin
    // coordinate. It passes that sample but fails the interpolation grid.
    Id spinColumn = c({spin(1, "\"Square\""), spin(2, "\"Square\"")});
    Id fixedSpin = a.parse("Spinor[\"Spin\",\"Upper\",\"Square\",1,Rational[1,2]]");
    Id fixedColumn = c({fixedSpin, spin(2, "\"Square\"")});
    auto spinRow = high_energy_chart_rows(a, {spinColumn, fixedColumn}, 0, 0, params);
    check(spinRow.size() == 1 && spinRow[0][1] != 0, "nondegenerate leading square coefficient");
    auto tuned = a.add({spinColumn, a.mul({a.exact(-spinRow[0][0] / spinRow[0][1]), fixedColumn})});
    check(high_energy_validate_chart(a, tuned, 0, 0, params, 1).passes, "fixed-spin control");
    check(!high_energy_validate_chart(a, tuned, 0, 0, params, 128).passes,
          "missed failure in another spin state");
    bool rejected = false;
    try {
        high_energy_chart_rows(a, {a.call("UnknownTensor", {one})}, 0, 0, params);
    } catch (const residue_unsupported&) {
        rejected = true;
    }
    check(rejected, "unsupported expression returned PASS");
    std::cout << "Exact fixed-angle kinematics, spin coverage and Laurent checks passed\n";
}
