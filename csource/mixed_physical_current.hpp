#pragma once
#include "reduce.hpp"
namespace hepcat {
// Second-order minimal Dirac coupling, with each polarization projected against
// the left massive endpoint. All denominator factors are physical virtualities.
std::optional<Id> mixed_physical_current(Algebra&, int left, int right, Id mass,
                                         const std::vector<int>& positive, const std::vector<int>& negative,
                                         Id vertex_product);
// Join pairs of emissions in the common massless-reference gauge at the middle
// of a minimal line with two photons of each helicity.
std::optional<Id> mixed_spurious_current(Algebra&, int left, int right, Id mass,
                                         const std::vector<int>& positive, const std::vector<int>& negative,
                                         Id vertex_product);
} // namespace hepcat
