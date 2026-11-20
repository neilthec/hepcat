#pragma once
#include "reduce.hpp"
namespace hepcat {
// Second-order minimal Dirac coupling with polarization references built from
// massive endpoints. All kinematic denominators are physical virtualities.
std::optional<Id> mixed_physical_current(Algebra&, int left, int right, Id mass,
                                         const std::vector<int>& positive, const std::vector<int>& negative,
                                         Id vertex_product);
// Use external massless reference legs and combine the two helicities along
// the Dirac line. All reference dependence is explicit in external brackets.
std::optional<Id> mixed_spurious_current(Algebra&, int left, int right, Id mass,
                                         const std::vector<int>& positive, const std::vector<int>& negative,
                                         Id vertex_product);
} // namespace hepcat
