#pragma once
#include "reduce.hpp"
#include <string_view>

namespace hepcat::model_detail {
// The mass list is indexed by vertex-local slot. An empty list reads scalar
// expressions with no compact spinor or propagator notation.
Id parse_expression(Algebra& arena, std::string_view text, Id scope, const std::vector<bool>& massive = {},
                    bool feynman = false);
} // namespace hepcat::model_detail
