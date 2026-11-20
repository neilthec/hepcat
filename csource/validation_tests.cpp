#include "amplitude.hpp"

#include <algorithm>
#include <iostream>
#include <stdexcept>

using namespace hepcat;
namespace {
void require(bool condition, const char* message) {
    if (!condition)
        throw std::runtime_error(message);
}
const numerical_check& numerical(const validation_report& r, const std::string& kind) {
    const auto it = std::find_if(r.numerical.begin(), r.numerical.end(),
                                 [&](const auto& row) { return row.kind == kind; });
    if (it == r.numerical.end())
        throw std::runtime_error("missing numerical row: " + kind);
    return *it;
}
void check_report(const validation_report& r) {
    if (r.passed())
        return;
    for (const auto& b : r.checks)
        if (b.status != check_outcome::pass)
            std::cerr << b.kind << " " << b.label << " " << b.detail << '\n';
    for (const auto& b : r.numerical)
        if (b.status != check_outcome::pass)
            std::cerr << b.kind << " compared " << b.compared << " mismatches " << b.failures
                      << " maximum relative " << b.max_relative_error << " " << b.detail << '\n';
    throw std::runtime_error("validation failed");
}
} // namespace
int main(int argc, char** argv) {
    try {
        if (argc != 3)
            throw std::runtime_error("expected constructive and CalcHEP model directories");
        model v(argv[1], 3), f(argv[1], 4), q(argv[2], 2);
        validation_options opts;
        opts.seed = 41711;
        opts.workers = 3;
        amplitude electrons({"e", "E", "E", "e"}, v, f, generation_references::physical);
        auto before = electrons.complete_high_energy().str();
        check_report(electrons.validate(opts));
        // Attaching an independent model after completion keeps the final expression available.
        electrons.set_feynman_model(q);
        auto positive = electrons.validate(opts);
        check_report(positive);
        require(electrons.final_amplitude().str() == before && electrons.feynman_diagrams().size() == 2,
                "attaching the reference erased the result or generated duplicate rows");
        require(numerical(positive, "Feynman").compared == opts.points &&
                    numerical(positive, "Feynman").nonzero == opts.points,
                "missing independent signed comparisons");
        amp_expression flipped("Times[-1," + before + "]", electrons.external_masses());
        auto sign = electrons.validate(flipped, opts);
        require(!sign.passed() && numerical(sign, "Feynman").failures == opts.points,
                "lost the overall phase");
        auto serial = opts;
        serial.workers = 1;
        auto again = electrons.validate(serial);
        require(numerical(positive, "Feynman").max_absolute_error ==
                    numerical(again, "Feynman").max_absolute_error,
                "parallel evaluation changed the validation sample");
        try {
            auto small = opts;
            small.points = 9999;
            electrons.validate(small);
            throw std::runtime_error("accepted fewer than 10000 points");
        } catch (const std::invalid_argument&) {
        }
        amplitude photons({"e", "E", "A.+", "A.+"}, v, f, q, generation_references::physical);
        photons.generate(3).complete_high_energy();
        check_report(photons.validate(opts));
        require(photons.diagrams().size() == 2 && photons.factorization_products().size() == 2 &&
                    photons.feynman_diagrams().size() == 2 && photons.feynman_diagrams()[0].shells.empty(),
                "three-role generation or Feynman internal-shell error");
        model sm(argv[1], 1), sf(argv[1], 2), calc(argv[2], 1);
        amplitude higgs({"h", "h", "h", "h"}, sm, sf, calc);
        higgs.generate(4).complete_high_energy();
        auto scalar = higgs.validate(opts);
        check_report(scalar);
        require(numerical(scalar, "FinalReduction").compared == opts.points &&
                    higgs.feynman_diagrams().size() == 4,
                "final reduction agreement or independent scalar contact missing");
        // Force loss of the amplitude between two equal and opposite large
        // numbers. The same-point replay must recover it and retain the raw error.
        const std::string large = "1000000000000000000000000000000";
        amp_expression cancellation("Plus[" + large + "," + higgs.final_amplitude().str() + ",-" + large +
                                        "]",
                                    higgs.external_masses());
        auto recovered = higgs.validate(cancellation, opts);
        check_report(recovered);
        const auto& repeated = numerical(recovered, "Feynman");
        require(repeated.replays == opts.points && repeated.failures == 0 &&
                    repeated.max_relative_error > 0.5L &&
                    repeated.max_replay_relative_error < opts.relative_tolerance &&
                    repeated.first_raw_outlier_seed && !repeated.first_failure_seed,
                "roundoff replay discarded its raw discrepancy or failed to recover the amplitude");
        // A constant shift is allowed by scalar spin weights and the four-Higgs
        // growth bound, so the Feynman comparison must catch it on its own.
        amp_expression wrong("Plus[1," + higgs.final_amplitude().str() + "]", higgs.external_masses());
        auto negative = higgs.validate(wrong, opts);
        require(std::all_of(negative.checks.begin(), negative.checks.end(),
                            [](const auto& row) { return row.status == check_outcome::pass; }) &&
                    !negative.passed() && numerical(negative, "Feynman").failures == opts.points &&
                    numerical(negative, "Feynman").first_failure_seed.has_value(),
                "missed a scalar contact shift");
        amp_expression gauge_dependent(
            "SpinorChain[Spinor[\"Xi\",\"Angle\",3],Spinor[\"Helicity\",\"Angle\",4]]",
            photons.external_masses());
        auto gauge = photons.validate(gauge_dependent, opts);
        require(!gauge.passed() && numerical(gauge, "Reference").failures == opts.points,
                "missed an explicit reference dependence");
        // Check a user-supplied contact before generating its independent channel inventory.
        amplitude supplied({"A.+", "A.+", "A.-", "A.-"}, v, f);
        high_energy_options custom;
        custom.energy_bound = 2;
        supplied.complete_high_energy(amp_expression("0", supplied.external_masses()), custom);
        check_report(supplied.validate(opts));
        require(supplied.final_amplitude().str() == "0" && supplied.last_high_energy().bound == 2,
                "validating an explicit final expression discarded the result or its bound");
        auto unknown =
            electrons.validate(amp_expression("UnknownVertex[1]", electrons.external_masses()), opts);
        require(!unknown.passed() && numerical(unknown, "PhaseSpace").status == check_outcome::unresolved,
                "unsupported expression passed");
        // This contact has distinct off-shell values before and after an artificial diagram cut.
        // The discarded diagram-only condition must never reach the completed result.
        amplitude external({"1", "1", "0", "0"});
        external.add_prepared_diagram("0", {{{1, 3}, "1"}}, "MomProd[1,3]");
        high_energy_options loose;
        loose.energy_bound = 2;
        const auto& reduced = external.complete_high_energy(
            amp_expression("Plus[MomProd[1,1],MomProd[1,3]]", external.external_masses()), loose);
        require(reduced.str().find("MomProd[1,3]") != std::string::npos &&
                    reduced.str().find("MomProd[1,1]") == std::string::npos,
                "final reduction skipped an external shell or used an internal shell");
        auto light = opts;
        light.parameters[calc.particle("h").mass] = 14;
        require(!higgs.validate(light).passed(), "accepted different mass shells in the two models");
        std::cout << "Native full-amplitude validation passed.\n";
    } catch (const std::exception& e) {
        std::cerr << e.what() << '\n';
        return 1;
    }
}
