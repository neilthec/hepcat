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
std::optional<Id> minimal_dirac_line(Algebra&, int left, int right, const std::vector<int>& photons,
                                     const std::vector<bool>& square, Id vertex_product, bool physical,
                                     std::optional<permutation_sum>* presentation);
} // namespace hepcat
