#pragma once

#include "reduce.hpp"

namespace hepcat {

// An unnormalized sum over every permutation of legs, acting on kernel.
// The elementary expression remains available for algebra and numerical export.
struct permutation_sum {
    std::vector<int> legs;
    Id kernel;
    std::vector<std::size_t> orbit_sizes;
    std::vector<std::vector<int>> other_groups{};
};
// Preserves the sum over all permutations of legs. Individual summands can change.
Id simplify_permutation_kernel(Algebra&, Id, const std::vector<int>& legs,
                               const std::vector<std::vector<int>>& other_groups = {});
// Explicitly sum disjoint secondary groups for clients of the single-group API.
Id expand_permutation_groups(Algebra&, Id, const std::vector<std::vector<int>>&);
// Recognize an equivalent current from its signed coefficients and external masses.
std::optional<permutation_sum> recognize_ordered_current(Algebra& a, Id input);
std::optional<permutation_sum> compact_permutations(Algebra& a, Id expression);

} // namespace hepcat
