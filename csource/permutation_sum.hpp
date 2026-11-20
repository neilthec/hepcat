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
std::optional<permutation_sum> compact_permutations(Algebra& a, Id expression);

} // namespace hepcat
