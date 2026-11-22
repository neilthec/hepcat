// Independent spinor, momentum and singularity checks on conserved cut charts.
#include "residue.hpp"

#include <iostream>
#include <stdexcept>

using namespace hepcat;

namespace {
std::vector<mpq_class> regular(const Algebra& a, const numerator_shell& cut, const std::vector<Id>& row,
                               std::size_t branch) {
    for (std::size_t i = branch; i < branch + 256; i += 4) {
        try {
            return residue_sample(a, cut, row, i);
        } catch (const residue_singular&) {
        }
    }
    throw std::runtime_error("no regular cut point on requested branch");
}
void require(bool ok, const char* message) {
    if (!ok) throw std::runtime_error(message);
}
} // namespace
int main() {
    Algebra a;
    Id m = a.atom("m"), zero = a.integer(0);
    auto spin = [&](int i, bool angle, bool massive) {
        return a.call("Spinor", {a.atom(massive ? "\"Spin\"" : "\"Helicity\""),
                                 a.atom(angle ? "\"Angle\"" : "\"Square\""), a.integer(i)});
    };
    auto mom = [&](int i) { return a.call("Mom", {a.integer(i)}); };
    auto chain = [&](std::initializer_list<Id> word) { return a.call("SpinorChain", word); };
    auto den = [&](int i, int j, Id mass) {
        return a.call("PropDen",
                      {a.call("Mom", {a.call("Multiparticle", {a.integer(i), a.integer(j)})}), mass});
    };
    auto pole = [&](Id num, Id d) { return a.mul({num, a.pow(d, -1)}); };
    a.setKinematics({m, m, zero, zero}, {});
    Id d = den(1, 3, m), q = den(2, 4, m);
    auto l = spin(1, true, true), r = spin(2, false, true);
    auto schouten = [&](bool angle) {
        auto br = [&](int i, int j) { return chain({spin(i, angle, i <= 2), spin(j, angle, j <= 2)}); };
        return a.add({a.mul({br(1, 2), br(3, 4)}), a.mul({br(1, 4), br(2, 3)}),
                      a.mul({a.integer(-1), br(1, 3), br(2, 4)})});
    };
    Id pSum =
        a.add({chain({l, mom(1), r}), chain({l, mom(2), r}), chain({l, mom(3), r}), chain({l, mom(4), r})});
    Id sqr = a.pow(a.integer(2), 2);
    auto pi = a.parse("Power[2,Rational[1,2]]"), im = a.atom("I");
    for (unsigned i = 0; i < 12; ++i) {
        auto row = regular(a, {{1, 3}, "m"},
                           {a.pow(d, -1), a.pow(q, -1), a.pow(den(1, 4, m), -1), pole(schouten(true), d),
                            pole(schouten(false), d), pole(pSum, d), pole(chain({l, mom(1), r}), d),
                            pole(a.mul({m, chain({spin(1, false, true), r})}), d),
                            pole(a.mul({pi, pi, im, im}), d), pole(sqr, d)},
                           i);
        require(row[0] == 1 && row[1] == 1 && row[2] == 0, "complementary and unrelated propagators");
        require(row[3] == 0 && row[4] == 0 && row[5] == 0, "Schouten and conservation");
        require(row[6] == row[7], "massive Dirac sign");
        require(row[8] == -2 && row[9] == 4, "exact algebraic coefficients");
    }
    // The same inverse propagator is written in two different ways. Its
    // vanishing constant term removes one pole and its derivative is one.
    auto hidden_d = a.add({a.parse("Mandelstahm[1,3]"), a.mul({a.integer(-1), a.pow(m,2)})});
    auto momentum_d = a.add({a.parse("Power[Mom[Multiparticle[1,3]],2]"), a.mul({a.integer(-1), a.pow(m,2)})});
    for(unsigned i=0; i<4; ++i) {
        auto row=regular(a,{{1,3},"m"},{a.mul({hidden_d,a.pow(d,-2)}),a.mul({momentum_d,a.pow(d,-2)}),a.pow(d,-1)},i);
        require(row[0]==1 && row[1]==row[2], "normalized double-pole derivative");
    }
    // A large regular background cannot alter a simple residue, or suppress
    // the derivative required when two descriptions of a double pole cancel.
    std::vector<Id> background;
    Id numerator = chain({l, mom(3), r});
    for (int i = 1; i <= 512; ++i)
        background.push_back(a.mul({numerator, a.add({m, a.integer(i)})}));
    Id regular_background = a.add(background);
    require(a.cost(regular_background) > 4096, "large-residue fixture size");
    auto mixed = a.add({regular_background, pole(a.integer(7), d)});
    auto derivative = a.add({regular_background, a.mul({hidden_d, a.pow(d, -2)})});
    auto large_row = regular(a, {{1, 3}, "m"}, {mixed, derivative}, 0);
    require(large_row[0] == 7 && large_row[1] == 1, "large sum Laurent coefficients");
    a.setKinematics({zero, zero, zero, zero}, {});
    Id angle = chain({spin(1, true, false), spin(2, true, false)});
    Id square = chain({spin(1, false, false), spin(2, false, false)});
    auto massless = den(1, 2, zero);
    bool sawAngle = false, sawSquare = false;
    for (unsigned i = 0; i < 12; ++i) {
        auto row = regular(a, {{1, 2}, "0"},
                           {a.pow(angle, -1), pole(square, massless), a.pow(square, -1), pole(angle, massless),
                            a.pow(a.mul({angle, square}), -1),
                            a.pow(a.call("MomProd", {a.integer(1), a.integer(2)}), -1)},
                           i);
        require(row[0] == -row[1] && row[2] == -row[3] && row[4] == -1 && row[5] == 2,
                "chiral and physical propagator residues");
        sawAngle |= row[0] != 0;
        sawSquare |= row[2] != 0;
    }
    require(sawAngle && sawSquare, "both collinear branches");
    a.setKinematics({m, m, m, m}, {});
    Id pair = a.call("xFactor", {a.integer(1), a.integer(2)});
    Id tilde = a.call("xTildeFactor", {a.integer(1), a.integer(2)});
    for (unsigned i = 0; i < 8; ++i) {
        auto row = regular(a, {{1, 2}, "0"}, {pole(a.mul({pair, tilde}), massless)}, i);
        require(row[0] == 1, "selected-cut x times x-tilde");
    }
    for (Id bad :
         {a.pow(massless, -2), pole(a.call("UnknownTensor", {a.integer(1)}), massless), pole(pair, massless)}) {
        bool rejected = false;
        try {
            regular(a, {{1, 2}, "0"}, {bad}, 7);
        } catch (const residue_unsupported&) {
            rejected = true;
        }
        require(rejected, "unsupported input silently accepted");
    }
    std::cout << "Exact cut kinematics and spinor residues passed\n";
}
