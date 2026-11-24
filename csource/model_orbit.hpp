#pragma once
#include "reduce.hpp"
#include <string_view>
namespace hepcat::model_detail {
// Read a compact signed external-leg permutation sum into its model arena.
// Expansion is local to the caller and requires no shared mutable caches.
Id read_leg_orbit(Algebra&, std::string_view, Id scope, const std::vector<bool>& massive);
Id expand_leg_orbit(Algebra&, Id);
}
