#pragma once
#include "model.hpp"
#include "permutation_sum.hpp"
namespace hepcat {
// Reorganize a complete set of ordered minimal Dirac-line trees by photon
// subsets.
std::optional<Id> dirac_line_proposal(Algebra &, const model &,
                                      const std::vector<std::string> &,
                                      std::size_t diagram_count, bool physical,
                                      std::optional<permutation_sum>* presentation = nullptr);
} // namespace hepcat
