#pragma once

#include "reduce.hpp"

namespace hepcat {

// An unnormalized sum over every permutation of legs, acting on kernel.
// The elementary expression remains available for algebra and numerical export.
struct permutation_sum {
    std::vector<int> legs;
    Id kernel;
    std::vector<std::size_t> orbit_sizes;
};
// Preserves the sum over all permutations of legs. Individual summands can change.
Id simplify_permutation_kernel(Algebra& a, Id summand, const std::vector<int>& legs);
// Recognize an equivalent current from its signed coefficients and external masses.
std::optional<permutation_sum> recognize_ordered_current(Algebra& a, Id input);
std::optional<permutation_sum> compact_permutations(Algebra& a, Id expression);

} // namespace hepcat
