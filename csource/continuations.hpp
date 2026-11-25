#pragma once

#include "amp_expression.hpp"
#include "reduce.hpp"

#include <utility>
#include <map>

namespace hepcat {

// Supply the momentum or null spinor used for each independent Xi endpoint.
struct reference_choice { int emitter, target; bool physical; };
Id replace_external_references(Algebra&, Id expression, const std::map<Id, reference_choice>&);

// Alternate off-cut lifts of a sewn channel, obtained by changing its gauge
// references. Only the simultaneous residue check can admit a lift.
std::vector<Id> channel_continuations(Algebra&, Id sewn_amplitude, const numerator_shell& selected,
                                      const std::vector<numerator_shell>& inventory);

std::vector<Id>
shifted_channel_continuations(Algebra&, const std::vector<std::pair<numerator_shell, Id>>& signed_full_sources,
                              int square_leg, int angle_leg);
// Shift along a null direction orthogonal to a light and a massive momentum.
// Transport the massive external spinors with their original spin labels.
std::vector<Id> massive_shifted_channel_continuations(
    Algebra&, const std::vector<std::pair<numerator_shell,Id>>&,
    int light_leg, int massive_leg, bool shift_square);
} // namespace hepcat
