#include "amplitude.hpp"
#include "reduce.hpp"
#include <algorithm>
#include <iostream>
#include <stdexcept>

using namespace hepcat;

namespace {
void check(bool value, const char* description) {
    if (!value) throw std::runtime_error(description);
}
template <class F> void rejects(F run, const char* error) {
    try {
        run();
    } catch (const std::exception&) {
        return;
    }
    throw std::runtime_error(error);
}
bool same(const std::string& x, const std::string& y) {
    Algebra a;
    return a.parse(x) == a.parse(y);
}
} // namespace
int main() {
    try {
        std::vector<std::string> mass(4, "m");
        amplitude process(mass);
        const std::string growth = "Mandelstahm[1,3]";
        amp_expression seed("Plus[7," + growth + "]", mass);
        high_energy_options serial;
        serial.workers = 1;
        check(process.complete_high_energy(seed, serial).str() == "7",
              "remove excessive growth and preserve constant");
        auto first = process.last_high_energy();
        check(first.bound == 0 && first.rank == 1 && first.training_samples > 0 && first.validation_samples > 0,
              "missing validation statistics");
        amp_expression factored("Times[g,Plus[7," + growth + "]]", mass);
        auto serial_result = process.complete_high_energy(factored, serial).str();
        auto parallel = serial;
        parallel.workers = 3;
        check(same(serial_result, "Times[7,g]") &&
                  process.complete_high_energy(factored, parallel).str() == serial_result,
              "factored contact or parallel result");
        const std::string pole = "Power[PropDen[Mom[Multiparticle[1,3]],m],-1]";
        amp_expression with_exchange("Plus[" + pole + ",7," + growth + "]", mass);
        auto supplied = serial;
        supplied.correction_basis = {growth, "1"};
        auto result = process.complete_high_energy(with_exchange, supplied).str();
        check(same(result, "Plus[7," + pole + "]"), "contact correction changed the exchange or regular seed");
        check(process.last_high_energy().candidates == 2 && process.last_high_energy().rank == 1,
              "bounded free contact direction did not remain free");
        check(process.last_high_energy().free_coefficients == 1, "sampled coefficient nullity not reported");
        for (bool reverse : {false, true}) {
            supplied.correction_basis = {"Plus[5," + growth + "]", growth};
            if (reverse) std::reverse(supplied.correction_basis.begin(), supplied.correction_basis.end());
            check(same(process.complete_high_energy(with_exchange, supplied).str(), "Plus[7," + pole + "]"),
                  "free constant changed when redundant explicit directions were reordered");
            check(process.last_high_energy().free_coefficients == 1, "rank-deficient fit was claimed unique");
        }
        auto invalid = serial;
        invalid.correction_basis = {pole};
        rejects([&] { process.complete_high_energy(seed, invalid); }, "accepted a pole-bearing direction");
        check(process.last_high_energy().rank == 1, "failed fit discarded earlier result");
        for (std::string forbidden : {"0", "1", "System`I", "I", "z", "ConstructiveDiagrams`z", "True", "Null", "Mom[1]", "\"symbol\""}) {
            invalid = serial;
            invalid.parameter_points = {{{forbidden, "0"}}};
            rejects([&] { process.complete_high_energy(seed, invalid); },
                    "allowed a parameter binding to override a constant");
        }
        rejects([&] { process.complete_high_energy(amp_expression("1", {"x", "x", "x", "x"}), serial); },
                "external mass mismatch was accepted");
        process.add_diagram("7");
        rejects([&] { process.last_high_energy(); }, "adding a diagram failed to invalidate the result");
        check(process.complete_high_energy(1).str() == "7" && process.last_factorization().route == "Contact" &&
                  process.last_high_energy().route == "Unchanged",
              "factorization to high energy");
        process.complete_factorization(1);
        rejects([&] { process.last_high_energy(); },
                "repeating factorization failed to invalidate high energy");
        amplitude five({"m", "m", "0", "0", "0"});
        amp_expression five_seed("Plus[9," + pole + "]", five.external_masses());
        check(same(five.complete_high_energy(five_seed, serial).str(), pole) &&
                  five.last_high_energy().bound == -1,
              "five-point default bound and pole preservation");
        amplitude unsupported(mass);
        unsupported.add_diagram("Mystery[1]");
        rejects([&] { unsupported.complete_high_energy(1); }, "uninterpreted chart operator was accepted");
        rejects([&] { unsupported.last_factorization(); }, "failed full operation published partial state");
        std::cout << "Native high-energy completion tests passed.\n";
    } catch (const std::exception& e) {
        std::cerr << e.what() << "\n";
        return 1;
    }
}
