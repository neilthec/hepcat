#include "amplitude.hpp"
#include <filesystem>
#include <iostream>
#include <stdexcept>

using namespace hepcat;
namespace {
void require(bool condition, const char* why) {
    if (!condition) throw std::runtime_error(why);
}
template <class F> void rejects(F operation, const char* why) {
    try {
        operation();
    } catch (const std::exception&) {
        return;
    }
    throw std::runtime_error(why);
}
bool equal_rows(const std::vector<amplitude_item>& a, const std::vector<amplitude_item>& b) {
    if (a.size() != b.size()) return false;
    for (std::size_t i = 0; i < a.size(); ++i) {
        if (a[i].expression.str() != b[i].expression.str() || a[i].shells.size() != b[i].shells.size())
            return false;
        for (std::size_t j = 0; j < a[i].shells.size(); ++j) {
            if (a[i].shells[j].legs != b[i].shells[j].legs || a[i].shells[j].mass != b[i].shells[j].mass)
                return false;
        }
        auto lhs = a[i].expression.discarded_remainder(), rhs = b[i].expression.discarded_remainder();
        if (bool(lhs) != bool(rhs) || (lhs && lhs->str() != rhs->str())) return false;
    }
    return true;
}
} // namespace
int main(int argc, char** argv) {
    try {
        const std::filesystem::path models = argc > 1 ? argv[1] : "models";
        model qed(models, 3), qf(models, 4), sm(models, 1), sf(models, 2);
        std::vector<std::string> three = {"e", "E", "A.+", "A.+", "A.-"};
        amplitude serial(three, qed, qf), parallel(three, qed, qf);
        serial.generate_diagrams(1);
        require(serial.diagrams().size() == 6 && serial.factorization_products().empty(),
                "diagram-only generation");
        serial.generate_factorization_products(1);
        parallel.generate(4);
        require(parallel.last_worker_count() == 4 && parallel.factorization_products().size() == 6,
                "one combined queue");
        require(equal_rows(parallel.diagrams(), serial.diagrams()) &&
                    equal_rows(parallel.factorization_products(), serial.factorization_products()),
                "generation order, shells, reductions or remainders depend on scheduling");
        require(!serial.complete_factorization(2).str().empty() &&
                    !serial.complete_high_energy(2).str().empty(),
                "generated mixed helicity process failed native completion");
        parallel.reduce();
        require(parallel.last_worker_count() == 0, "generated rows were reduced again");
        parallel.generate();
        require(parallel.last_worker_count() == 0 && parallel.diagrams().size() == 6,
                "repeat generation duplicated rows");
        parallel.add_diagram("1");
        parallel.reduce(2);
        require(parallel.diagrams().size() == 7 && parallel.last_worker_count() == 1,
                "manual input appended after generation");
        amplitude gamma({"A.+", "A.+", "A.-", "A.-"}, qed, qf);
        gamma.generate();
        require(gamma.diagrams().empty() && gamma.factorization_products().empty(),
                "absent tree should have empty collections");
        amplitude manual({"m", "m", "m", "m"});
        rejects([&] { manual.generate(); }, "missing generation model accepted");
        amplitude h({"h", "h", "h", "h"}, sm, sf);
        auto before = h.complete_high_energy().str();
        require(h.diagrams().size() == 4 && h.factorization_products().size() == 3 && !before.empty() &&
                    h.last_high_energy().route == "Unchanged",
                "generation to exact model-aware Higgs completion");
        amplitude only(three, qed);
        only.generate();
        rejects([&] { only.generate_factorization_products(); }, "missing lower-point model accepted");
        require(only.diagrams().size() == 6, "failure mutated existing rows");
        model wrong_scope(models, 4, "another-model");
        rejects([&] { amplitude invalid(three, qed, wrong_scope); },
                "conflicting external model masses were accepted");
        rejects([&] { amplitude invalid({"e", "E", "not-a-particle"}, qed, qf); }, "unknown particle accepted");
        for (const std::string helicity : {"A.+", "A.-"}) {
            const std::vector<std::string> cubic{"e", "E", helicity};
            for (auto reference : {generation_references::automatic, generation_references::physical,
                                   generation_references::keep}) {
                amplitude bare(cubic, qed, qf, reference);
                bare.generate();
                require(bare.diagrams().size() == 1 && bare.factorization_products().empty(),
                        "one three-point vertex with no factorization cuts");
                const auto row = bare.diagrams().front().expression.str();
                auto expected = helicity == "A.+" ? "xFactor[1,2]" : "xTildeFactor[1,2]";
                require(reference == generation_references::keep ? row.find("\"Xi\"") != std::string::npos :
                            (row.find(expected) != std::string::npos && row.find("\"Xi\"") == std::string::npos),
                        "three-point polarization reference convention");
            }
        }
        for (const std::string helicity : {"A.+", "A.-"}) {
            amplitude same_helicity({"e","E",helicity,helicity},qed,qf,generation_references::physical);
            const auto final = same_helicity.complete_high_energy().str();
            require(same_helicity.diagrams().size() == 2 && same_helicity.factorization_products().size() == 2 &&
                    same_helicity.last_high_energy().route == "Unchanged" && final != "0" &&
                    final.find("],Mom[") == std::string::npos && final.find("MomProd[") == std::string::npos &&
                    final.find("PropDen[") != std::string::npos && final.find("\"Xi\"") == std::string::npos,
                    "same-helicity physical-reference generation and both completions");
        }
        std::cout << "Native model-based generation integration tests passed.\n";
    } catch (const std::exception& e) {
        std::cerr << e.what() << '\n';
        return 1;
    }
}
