#include "reduce.hpp"

#include <iostream>
#include <stdexcept>

namespace hepcat {

void testKinematics() {
    Algebra a;
    const std::vector<Id> masses = {a.integer(2), a.integer(3), a.integer(5), a.integer(7)};
    auto equal = [&](const char* input, const char* answer) {
        const auto result = a.simplify(a.parse(input));
        const auto expected = a.call("Plus", {a.parse(answer)});
        if (result != expected)
            throw std::runtime_error(std::string("Kinematics: ") + input + " -> " +
                a.format(result) + ", expected " + a.format(expected));
        if (a.simplify(result) != result)
            throw std::runtime_error(std::string("Non-idempotent kinematic reduction: ") + input);
    };
    a.setKinematics(masses, {});
    equal("Mass[2]", "3");
    equal("MomProd[1,1]", "4");
    equal("MomProd[2,1]", "MomProd[1,2]");
    equal("MomProd[Multiparticle[1,2,3,4],1]", "0");
    equal("Power[Mom[Multiparticle[2,3,4]],2]", "4");
    equal("MomProd[Multiparticle[1,2],Multiparticle[1,2]]",
          "MomProd[Multiparticle[1,2],Multiparticle[1,2]]");
    equal("MomProd[1,5]", "MomProd[1,5]");
    equal("Mass[5]", "Mass[5]");

    // Removing all numerator momenta is preferred even when two endpoint
    // brackets occupy more display space than the original single chain.
    equal("SpinorChain[Spinor[\"Spin\",\"Square\",1],Mom[Multiparticle[1,2]],Spinor[\"Spin\",\"Angle\",2]]",
          "Plus[Times[2,SpinorChain[Spinor[\"Spin\",\"Angle\",1],Spinor[\"Spin\",\"Angle\",2]]],"
          "Times[-3,SpinorChain[Spinor[\"Spin\",\"Square\",1],Spinor[\"Spin\",\"Square\",2]]]]");

    a.setKinematics(masses, {{{1,2}, a.integer(11)}});
    equal("Mass[Multiparticle[1,2]]", "11");
    equal("Power[Mom[Multiparticle[1,2]],2]", "121");
    equal("Power[Mom[Multiparticle[3,4]],2]", "121");
    equal("Mandelstahm[4,3]", "121");
    equal("MomProd[1,2]", "54");
    equal("MomProd[3,4]", "Rational[47,2]");
    equal("MomProd[1,Multiparticle[1,2]]", "58");
    equal("MomProd[1,3]", "MomProd[1,3]");
    equal("Power[PropDen[Mom[Multiparticle[1,2]],11],-1]",
          "Power[PropDen[Mom[Multiparticle[1,2]],11],-1]");
    equal("Power[MomProd[1,2],-1]", "Power[MomProd[1,2],-1]");

    equal("Plus[Power[MomProd[1,2],-1],MomProd[1,2]]",
          "Plus[54,Power[MomProd[1,2],-1]]");
    equal("Plus[MomProd[1,2],Power[MomProd[1,2],-1]]",
          "Plus[54,Power[MomProd[1,2],-1]]");

    // Adjacent slashes reduce to q^2, for either declared side of a cut.
    equal("SpinorChain[Spinor[\"Spin\",\"Angle\",1],Mom[Multiparticle[3,4]],"
          "Mom[Multiparticle[3,4]],Spinor[\"Spin\",\"Angle\",2]]",
          "Times[121,SpinorChain[Spinor[\"Spin\",\"Angle\",1],Spinor[\"Spin\",\"Angle\",2]]]");
    // A missing cut, repeated member, or unknown leg cannot imply a shell.
    equal("Power[Mom[Multiparticle[1,1,2]],2]", "Power[Mom[Multiparticle[1,1,2]],2]");
    equal("Power[Mom[Multiparticle[1,2,5]],2]", "Power[Mom[Multiparticle[1,2,5]],2]");
    equal("SpinorChain[Spinor[\"Spin\",\"Angle\",1],Mom[Multiparticle[1,2,3,4]],"
          "Spinor[\"Spin\",\"Square\",2]]", "0");
    // Adjacent massless spinors and null slashes annihilate their own term
    // in a composite momentum without expanding the surviving sum.
    {
        Algebra chart;
        const Id m = chart.atom("m"), zero = chart.integer(0);
        chart.setKinematics({m,m,zero,zero,zero,zero,zero,zero},{});
        auto slash = [&](const std::string& legs, bool adjacent_slash) {
            std::string word = "SpinorChain[Spinor[\"Helicity\",\"Square\",3],";
            if (adjacent_slash) word += "Mom[4],";
            word += "Mom[Multiparticle[" + legs + "]],Spinor[\"Helicity\",";
            return chart.simplify(chart.parse(word + (adjacent_slash ? "\"Square\"" : "\"Angle\"") + ",8]]"));
        };
        if (slash("3,4,5",false) != slash("4,5",false) || slash("4,5,6",true) != slash("5,6",true))
            throw std::runtime_error("null neighbor inside a composite slash");
    }
    // A determinant of two crossed sandwiches reduces on the declared cut.
    // The two reference chiralities and a reversed two-slash word use the
    // same rule. A numerator shell never propagates into an inverse power.
    const Id m = a.atom("m"), z = a.integer(0);
    const std::vector<Id> diracMasses{m, m, z, z};
    for (auto c : {"Angle", "Square"}) {
        bool ang = std::string(c) == "Angle";
        auto polar = [&](int leg, bool angle) {
            return a.call("Spinor", {a.atom("\"Helicity\""), a.atom(angle ? "\"Angle\"" : "\"Square\""),
                                     a.integer(leg)});
        };
        auto mom = [&](int leg) { return a.call("Mom", {a.integer(leg)}); };
        Id b = polar(3, ang), end = polar(4, ang);
        auto word = [&](bool reversed) {
            Id wide = a.call("SpinorChain", {end, mom(reversed ? 2 : 1), mom(reversed ? 1 : 2), end});
            return a.mul({wide, a.call("SpinorChain", {b, mom(2), polar(4, !ang)})});
        };
        a.setKinematics(diracMasses, {});
        Id uncrossed = a.simplify(word(false));
        Id reciprocal = a.simplify(a.pow(word(false), -1));
        a.setKinematics(diracMasses, {{{1, 3}, m}});
        Id onCut =
            a.simplify(a.mul({a.integer(-2), a.pow(m, 2), a.call("MomProd", {a.integer(2), a.integer(3)}),
                              a.call("SpinorChain", {end, b})}));
        if (a.simplify(word(false)) != onCut || a.simplify(word(true)) != a.mul({a.integer(-1), onCut}) ||
            uncrossed == onCut || reciprocal != a.simplify(a.pow(word(false), -1)))
            throw std::runtime_error("cut sandwich determinant, reversal, or denominator protection");
    }
}
} // namespace hepcat

int main() {
    try {
        hepcat::testKinematics();
    } catch (const std::exception& error) {
        std::cerr << error.what() << '\n';
        return 1;
    }
    return 0;
}
