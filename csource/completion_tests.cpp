#include "amplitude.hpp"
#include <iostream>
#include <stdexcept>

namespace {
using hepcat::amplitude;
using hepcat::factorization_options;
using hepcat::numerator_shell;
void check(bool condition, const char* message) {
    if (!condition) throw std::runtime_error(message);
}
std::string prop(int i, int j, const std::string& mass) {
    return "PropDen[Mom[Multiparticle[" + std::to_string(i) + "," + std::to_string(j) + "]]," + mass + "]";
}
std::string inverse(std::string p) { return "Power[" + p + ",-1]"; }
std::string times(std::string a, std::string b) { return "Times[" + a + "," + b + "]"; }
std::string plus(std::string a, std::string b) { return "Plus[" + a + "," + b + "]"; }
} // namespace

int main() {
    try {
        std::vector<std::string> masses{"me", "me", "0", "0"};
        numerator_shell t{{1, 3}, "me"}, u{{1, 4}, "me"};
        auto T = inverse(prop(1, 3, "me")), U = inverse(prop(1, 4, "me"));
        auto combined = times(T, U);
        amplitude seed(masses);
        seed.add_prepared_diagram(plus("contact", times("2", T)));
        seed.add_prepared_factorization_product(times("-1", T), t);
        auto result = seed.complete_factorization().str();
        check(result.find("contact") != std::string::npos && result.find("Times[2,") == std::string::npos,
              "common rescaling changed the contact term or left duplicate pole");
        check(seed.last_factorization().route == "RescaledSeed", "proportional repair");
        check(seed.last_factorization().validation_samples >= 4, "fresh validation missing");
        seed.add_prepared_diagram("extraContact");
        try {
            seed.last_factorization();
            throw 0;
        } catch (const std::logic_error&) {
        }
        check(seed.complete_factorization().str().find("extraContact") != std::string::npos,
              "stale completed expression");
        amplitude composite({"Times[b,a]", "Times[a,b]", "0", "0"});
        auto composite_pole = inverse(prop(1, 3, "Times[a,b]"));
        composite.add_prepared_diagram(composite_pole);
        composite.add_prepared_factorization_product(times("-1", composite_pole), {{1, 3}, "Times[b,a]"});
        composite.complete_factorization(1);
        check(composite.last_factorization().channels == 1 &&
                  composite.last_factorization().route == "Unchanged",
              "equivalent model-mass products created duplicate poles");
        amplitude correction(masses);
        correction.add_prepared_diagram(T, {}, combined);
        correction.add_prepared_factorization_product(times("-1", plus(T, combined)), t);
        correction.add_prepared_factorization_product(times("-1", combined), u);
        auto parallel = correction;
        auto repaired = correction.complete_factorization(1).str();
        check(correction.last_factorization().route == "RetainedRemainders", "remainder repair");
        check(parallel.complete_factorization(2).str() == repaired &&
                  parallel.last_factorization().workers == 2,
              "parallel changes the solution");
        amplitude missing(masses);
        missing.add_prepared_diagram(plus(T, U));
        missing.add_prepared_factorization_product(times("-1", T), t);
        missing.complete_factorization(1);
        check(missing.last_factorization().channels == 2, "extra physical pole omitted");
        auto equivalent = [&](const std::string& hidden, const std::string& mass) {
            amplitude test(masses);
            test.add_prepared_diagram(plus(U, hidden));
            test.add_prepared_factorization_product(times("-1", U), u);
            test.complete_factorization(1);
            check(test.last_factorization().channels == 2 && test.last_factorization().route != "Unchanged",
                  "alternate invariant notation hid an unwanted pole");
            test.add_prepared_factorization_product(times("-1", inverse(prop(1, 3, mass))), {{1, 3}, mass});
            test.complete_factorization(1);
            check(test.last_factorization().channels == 2 && test.last_factorization().route == "Unchanged",
                  "one pole representation introduced extra cut obligations");
        };
        equivalent(inverse("Mandelstahm[1,3]"), "0");
        equivalent("Power[Mom[Multiparticle[1,3]],-2]", "0");
        equivalent(inverse("Plus[Mandelstahm[1,3],Times[-1,Power[me,2]]]"), "me");
        equivalent(inverse("Times[2,MomProd[1,3]]"), "me");
        amplitude numerator_only(masses);
        numerator_only.add_prepared_diagram(times(U, "Mandelstahm[1,3]"));
        numerator_only.add_prepared_factorization_product(times("-1", times(U, "Mandelstahm[1,3]")), u);
        numerator_only.complete_factorization(1);
        check(numerator_only.last_factorization().channels == 1, "numerator introduced a pole obligation");
        amplitude contour(masses);
        contour.add_prepared_diagram(T);
        auto one = times("Rational[-1,2]", times("Power[ConstructiveDiagrams`z,-1]", T));
        contour.add_prepared_factorization_product(one, t);
        contour.add_prepared_factorization_product(times("Rational[-1,2]", inverse(prop(2, 4, "me"))),
                                                   {{2, 4}, "me"});
        contour.complete_factorization();
        check(contour.last_factorization().channels == 1 && contour.last_factorization().route == "Unchanged",
              "sewing sign, contour or complementary state summation");
        amplitude invalid(masses);
        invalid.add_prepared_factorization_product(times("ConstructiveDiagrams`z", T), t);
        try {
            invalid.complete_factorization();
            throw 0;
        } catch (const std::runtime_error&) {
        }
        try {
            invalid.last_factorization();
            throw 0;
        } catch (const std::logic_error&) {
        }
        amplitude incompatible({"me", "me", "me", "me"});
        incompatible.add_prepared_factorization_product(times("-1", combined), t);
        incompatible.add_prepared_factorization_product(times("-2", combined), u);
        try {
            incompatible.complete_factorization();
            throw 0;
        } catch (const std::runtime_error&) {
        }
        amplitude absent(masses);
        absent.add_prepared_diagram(T);
        try {
            absent.complete_factorization();
            throw 0;
        } catch (const std::runtime_error&) {
        }
        absent.add_prepared_factorization_product("0", t);
        check(absent.complete_factorization().str() == "0", "declared zero target was ignored");
        amplitude raw_failure(masses);
        const std::string original = "Plus[MomProd[1,1]," + T + "]";
        raw_failure.add_diagram(original, {t});
        try {
            raw_failure.complete_factorization();
            throw 0;
        } catch (const std::runtime_error&) {
        }
        check(raw_failure.diagrams().at(0).expression.str() == original && raw_failure.last_worker_count() == 0,
              "failed completion published a partially prepared batch");
        amplitude contact(masses);
        contact.add_diagram("contact");
        check(contact.complete_factorization().str() == "contact", "regular input lost");
    } catch (const std::exception& e) {
        std::cerr << e.what() << '\n';
        return 1;
    } catch (...) {
        std::cerr << "expected an explicit failure\n";
        return 1;
    }
}
