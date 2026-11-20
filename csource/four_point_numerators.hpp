#pragma once
#include "reduce.hpp"

namespace hepcat {
// Primitive numerator directions classified by their total number of momentum insertions.
std::vector<Id> four_point_numerators(Algebra&, Id source, const std::vector<Id>& poles,
                                      const std::vector<Id>& model_masses, unsigned insertions);
} // namespace hepcat
