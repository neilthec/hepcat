#pragma once
#include "reduce.hpp"

namespace hepcat::generation {
// Sew opposite little-group indices for one internal on-shell line. Identical
// indices at an endpoint are symmetrized, including their factorial weight.
Id sew_spin(Algebra&, Id expression, Id momentum, Id mass, int twice_spin);

// Contract an internal null state, including little-group balanced rational
// lower amplitudes. A reference continuation is needed only for remaining
// ratios between internal spinor components.
Id sew_null_spin(Algebra&, Id expression, Id internal, const std::vector<int>& external_null,
                 bool replace_reference);

// Compare complementary three-null branches on the exact four-particle
// momentum-conservation variety. Return their common continuation if certified.
std::optional<Id> common_four_null_continuation(Algebra&, Id branch_a, Id branch_b);
} // namespace hepcat::generation
