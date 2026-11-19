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
