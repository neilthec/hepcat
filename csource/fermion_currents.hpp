#pragma once
#include "model.hpp"
#include "reduce.hpp"
namespace hepcat {
std::optional<Id> fermion_current_proposal(Algebra &, const model &,
                                           const std::vector<std::string> &,
                                           std::size_t diagram_count,
                                           bool physical);
}
