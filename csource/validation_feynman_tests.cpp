#include "generation_feynman.hpp"
#include "validation_feynman.hpp"

#include <filesystem>
#include <iostream>
#include <stdexcept>

using namespace hepcat;

namespace {

void require(bool condition, const std::string& message) {
    if (!condition)
        throw std::runtime_error(message);
}

template <class F> void rejects(F operation, const char* why) {
    try {
        operation();
    } catch (const std::invalid_argument&) {
        return;
    }
    throw std::runtime_error(why);
}

void contraction_tests() {
    Algebra a;
    const auto convert = [&](const std::string& expr) { return feynman_to_spinors(a, a.parse(expr)); };
    const auto angle = a.parse("SpinorChain[Spinor[\"Spin\",\"Angle\",1],Spinor[\"Spin\",\"Angle\",2]]");
    const auto square = a.parse("SpinorChain[Spinor[\"Spin\",\"Square\",1],Spinor[\"Spin\",\"Square\",2]]");
    require(convert("DiracChain[FermionBar[1,m,\"Dirac\"],DiracWord[],FermionKet[2,m,\"Dirac\"]]") ==
                a.add({a.mul({a.integer(-1), angle}), a.mul({a.integer(-1), square})}),
            "all-incoming Yukawa bilinear");
    require(
        convert("DiracChain[FermionBar[1,m,\"Dirac\"],DiracWord[DiracGamma5],FermionKet[2,m,\"Dirac\"]]") ==
            a.add({angle, a.mul({a.integer(-1), square})}),
        "gamma5 must act with opposite chiral signs");
    require(convert("Times[LorentzMetric[i,j],LorentzMetric[j,i]]") == a.integer(4), "trace of metric");
    require(convert("Times[LorentzMomentum[Mom[1],i],LorentzMetric[i,j],LorentzMomentum[Mom[2],j]]") ==
                a.parse("MomProd[1,2]"),
            "momentum contraction");
    require(convert("DiracChain[FermionBar[1,0,\"L\"],DiracWord[],FermionKet[2,0,\"L\"]]") == a.integer(0),
            "forbidden massless chiral bilinear");
    // This valid matrix contraction has 2^1000 chiral expansions. Reject it
    // before instantiating any intermediate powers.
    try {
        convert("Power[DiracChain[FermionBar[1,m,\"Dirac\"],DiracWord[],FermionKet[2,m,\"Dirac\"]],1000]");
        throw std::runtime_error("accepted an unbounded Lorentz expansion");
    } catch (const std::length_error& error) {
        require(std::string(error.what()).find("100000") != std::string::npos,
                "missing expansion limit detail");
    }
    rejects([&] { convert("Times[PolarizationVector[1,i,0,Automatic],LorentzMomentum[Mom[2],i]]"); },
            "factored vector contraction accepted an unspecified massless helicity");
    rejects([&] { convert("LorentzMomentum[Mom[1],freeIndex]"); }, "accepted an open Lorentz index");
    rejects([&] { convert("PolarizationVector[3,mu,0,Automatic]"); }, "accepted ambiguous photon helicity");
    rejects(
        [&] { convert("DiracChain[FermionBar[1,m,\"Dirac\"],DiracWord[Other],FermionKet[2,m,\"Dirac\"]]"); },
        "accepted an unknown Dirac operator");
}

void process_test(const model& m, const std::vector<std::string>& process, size_t expected,
                  bool emit_oracle) {
    Algebra a(&m.algebra());
    std::vector<std::string> particles;
    std::vector<Id> masses;
    for (const auto& state : process) {
        particles.push_back(generation::feynman_external(m, state));
        masses.push_back(a.parse(m.particle(particles.back()).mass));
    }
    a.setKinematics(masses, {});
    const auto plans = generation::diagram_plans(m, particles);
    require(plans.size() == expected, "unexpected Feynman diagram count");
    size_t index = 0;
    for (const auto& plan : plans) {
        auto raw = generation::sew_feynman(a, m, process, plan);
        auto result = a.format(feynman_to_spinors(a, raw));
        require(!result.empty() && result.find("Dirac") == result.npos &&
                    result.find("PolarizationVector") == result.npos && result.find("Lorentz") == result.npos,
                "raw tensors remain in contracted Feynman diagram");
        if (emit_oracle) {
            // Tab-separated records keep FullForm source intact even for the large vector trees.
            std::string label;
            for (const auto& name : process)
                label += name;
            std::cout << label << '\t' << ++index << '\t' << a.format(raw) << '\t' << result << '\n';
        }
    }
}

} // namespace

int main(int argc, char** argv) {
    try {
        contraction_tests();
        const auto directory = argc > 1 ? std::filesystem::path(argv[1])
                                        : std::filesystem::path("tests/reference-models/calchep");
        const bool emit_oracle = argc == 3 && std::string(argv[2]) == "--emit-oracle";
        if (argc > 2 && !emit_oracle)
            throw std::invalid_argument("usage: test_validation_feynman [model-directory [--emit-oracle]]");
        model sm(directory, 1), qed(directory, 2);
        process_test(qed, {"e", "E", "A.+", "A.-"}, 2, emit_oracle);
        process_test(qed, {"e", "E", "A.+", "A.+", "A.-"}, 6, emit_oracle);
        process_test(qed, {"e", "E", "E", "e"}, 2, emit_oracle);
        process_test(sm, {"Z", "Z", "W+", "W-"}, 4, emit_oracle);
        process_test(sm, {"W+", "W-", "W+", "W-"}, 7, emit_oracle);
        process_test(sm, {"G.+", "G.+", "G.-", "G.-"}, 6, emit_oracle);
        process_test(sm, {"ne", "Ne", "e", "E"}, 2, emit_oracle);
        process_test(sm, {"h", "h", "h", "h"}, 4, emit_oracle);
        process_test(sm, {"W+", "W-", "h", "h"}, 4, emit_oracle);
        process_test(sm, {"u", "U", "C", "c"}, 4, emit_oracle);
        if (!emit_oracle)
            std::cout << "Native Feynman tensor and spinor contractions passed.\n";
    } catch (const std::exception& e) {
        std::cerr << e.what() << '\n';
        return 1;
    }
}
