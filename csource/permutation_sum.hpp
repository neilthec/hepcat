#pragma once

#include "reduce.hpp"
#include "amp_expression.hpp"

namespace hepcat {

// An unnormalized sum over every permutation of legs, acting on kernel.
// The elementary expression remains available for algebra and numerical export.
struct permutation_sum {
    std::vector<int> legs;
    Id kernel;
    std::vector<std::size_t> orbit_sizes;
    std::vector<std::vector<int>> other_groups{};
    std::vector<std::vector<int>> alternating_groups{};
    std::optional<signed_exchange> joint_exchange{};
};
// Complete the three-photon Weyl kernel by simultaneous charge conjugation.
// Only photons of the same helicity enter the symmetric group.
permutation_sum radiation_exchange_sum(Algebra&, const std::vector<int>& fermions,
                                      const std::vector<int>& antifermions, const std::vector<int>& photons,
                                      const std::vector<int>& types, Id mass, Id coefficient, bool physical);
// Preserves the sum over all permutations of legs. Individual summands can change.
Id simplify_permutation_kernel(Algebra&, Id, const std::vector<int>& legs,
                               const std::vector<std::vector<int>>& other_groups = {});
// Reduce a summand modulo its certified signed exchange symmetries.
Id simplify_signed_kernel(Algebra&, Id, const permutation_sum&);
// Explicitly sum disjoint secondary groups for clients of the single-group API.
Id expand_permutation_groups(Algebra&, Id, const std::vector<std::vector<int>>&, bool alternating = false);
Id expand_signed_operators(Algebra&, const permutation_sum&);
// Recognize an equivalent current from its signed coefficients and external masses.
std::optional<permutation_sum> recognize_ordered_current(Algebra& a, Id input);
std::optional<permutation_sum> compact_permutations(Algebra& a, Id expression);

} // namespace hepcat
