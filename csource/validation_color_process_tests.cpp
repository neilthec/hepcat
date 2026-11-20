// Compare the published constructive four-quark row to independently generated CalcHEP trees.
#include "amplitude.hpp"
#include "reduce.hpp"
#include <algorithm>
#include <iostream>
#include <stdexcept>

using namespace hepcat;
namespace {
bool contains_generator(const Algebra& a, Id id) {
    const auto& node = a.node(id);
    return node.head == "ConstructiveDiagrams`ColorGenerator" ||
           std::any_of(node.args.begin(), node.args.end(), [&](Id x) { return contains_generator(a, x); });
}
numerical_check compare(amplitude& amp, const Algebra& a, Id id, bool expect_pass) {
    validation_options options;
    options.seed = 41711;
    options.workers = 3;
    const auto report = amp.validate(amp_expression(a.format(id), amp.external_masses()), options);
    if (report.passed() != expect_pass) {
        for (const auto& check : report.checks)
            if (check.status != check_outcome::pass)
                std::cerr << check.kind << " " << check.detail << '\n';
        throw std::runtime_error("unexpected combined four-quark validation outcome");
    }
    for (const auto& check : report.numerical)
        if (check.kind == "Feynman")
            return check;
    throw std::runtime_error("missing Feynman comparison: " +
                             (report.numerical.empty() ? std::string{} : report.numerical.front().detail));
}
} // namespace
int main(int argc, char** argv) {
    try {
        if (argc != 3)
            throw std::runtime_error("expected constructive and CalcHEP model directories");
        const model vertices(argv[1], 1), catalog(argv[1], 2), rules(argv[2], 1);
        const std::vector<std::string> process{"u", "U", "C", "c"};
        amplitude amp(process, vertices, catalog, rules);
        const auto& entries = catalog.vertices();
        auto entry =
            std::find_if(entries.begin(), entries.end(), [&](const auto& v) { return v.legs == process; });
        if (entry == entries.end())
            throw std::runtime_error("missing stored four-quark row");
        Algebra a(&catalog.algebra());
        auto positive = compare(amp, a, a.mul({entry->factor, entry->expression}), true);
        if (positive.status != check_outcome::pass || positive.compared != 10000 || !positive.nonzero)
            throw std::runtime_error("four-quark agreement failed at " + std::to_string(positive.failures) +
                                     " points: " + positive.detail);
        std::cout << "Four quarks, signed Feynman comparison: " << positive.compared << " points, "
                  << positive.nonzero << " nonzero, max relative error " << positive.max_relative_error
                  << ".\n";

        // Reverse only the octet coefficient. Singlet exchange, masses and spin frames are identical.
        const auto& sum = a.node(entry->expression);
        if (sum.head != "Plus")
            throw std::runtime_error("expected independent singlet and octet terms");
        auto terms = sum.args;
        size_t octet = 0;
        for (auto& term : terms)
            if (contains_generator(a, term)) {
                term = a.mul({a.integer(-1), term});
                ++octet;
            }
        if (!octet || octet == terms.size())
            throw std::runtime_error("color control did not isolate the octet coefficient");
        auto negative = compare(amp, a, a.mul({entry->factor, a.add(terms)}), false);
        if (negative.status != check_outcome::fail || negative.compared != 10000 ||
            !negative.first_failure_seed || !negative.failures || negative.failures > negative.nonzero)
            throw std::runtime_error("missed relative color sign or failed to sample the control");
        std::cout << "Octet-only sign reversal failed at " << negative.failures << " points.\n";
    } catch (const std::exception& e) {
        std::cerr << e.what() << '\n';
        return 1;
    }
}
