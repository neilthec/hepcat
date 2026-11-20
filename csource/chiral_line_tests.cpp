// Signed checks for splitting a massive Dirac line into two chiral currents.
#include "amp_expression.hpp"
#include "mixed_physical_current.hpp"

#include <algorithm>
#include <stdexcept>

namespace {
using namespace hepcat;

void compare(std::vector<int> plus, std::vector<int> minus,
             size_t physical_limit = 0, size_t spurious_limit = 0) {
    // Interleave the two massive endpoints among the photon labels.
    const int particle_count = 2 + static_cast<int>(plus.size() + minus.size());
    std::vector<std::string> masses(particle_count, "0");
    masses[1] = masses[4] = "m";
    Algebra a;
    std::vector<Id> labels;
    for (const auto& value : masses) labels.push_back(a.parse(value));
    a.setKinematics(labels, {});
    auto current = [&](bool physical) {
        const auto make = physical ? mixed_physical_current : mixed_spurious_current;
        const auto id = make(a, 2, 5, a.atom("m"), plus, minus, a.integer(1));
        if (!id) throw std::runtime_error("Missing mixed Dirac current");
        return amp_expression(a.format(*id), masses);
    };
    auto physical = current(true);
    auto spurious = current(false);
    if (physical_limit && physical.reduce().complexity() > physical_limit)
        throw std::runtime_error("Mixed physical current exceeds its tested size");
    if (spurious_limit && spurious.reduce().complexity() > spurious_limit)
        throw std::runtime_error("Mixed spurious current exceeds its tested size");
    identity_check_options options;
    options.samples = 3;
    options.seed = 234567;
    if (!physical.check_identity(spurious, options).passed())
        throw std::runtime_error("Physical and massless gauges disagree for the mixed Dirac line");
    // Change the reference photons without changing any external labels.
    std::reverse(plus.begin(), plus.end());
    std::reverse(minus.begin(), minus.end());
    if (!physical.check_identity(current(false), options).passed())
        throw std::runtime_error("Mixed Dirac current depends on its reference photons");
}
}

int main() {
    compare({1, 3, 4, 6}, {7}, 3966, 4634); // Single Pauli emission and determinant remainder.
    compare({1, 4}, {3, 6, 7}, 7683, 12672); // Odd multiplicity, parity-conjugate mixed helicities.
    compare({1, 3, 7}, {4, 6, 8});    // Even multiplicity, split at a propagator or a contact.
}
