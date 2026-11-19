#pragma once

#include "amp_expression.hpp"
#include "reduce.hpp"

#include <utility>

namespace hepcat {

// Alternate off-cut lifts of a sewn channel, obtained by changing its gauge
// references. Only the simultaneous residue check can admit a lift.
std::vector<Id> channel_continuations(Algebra&, Id sewn_amplitude, const numerator_shell& selected,
                                      const std::vector<numerator_shell>& inventory);

std::vector<Id>
shifted_channel_continuations(Algebra&, const std::vector<std::pair<numerator_shell, Id>>& signed_full_sources,
                              int square_leg, int angle_leg);
} // namespace hepcat
