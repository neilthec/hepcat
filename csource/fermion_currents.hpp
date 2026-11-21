#pragma once
#include "model.hpp"
#include <functional>
#include "permutation_sum.hpp"
#include "reduce.hpp"
namespace hepcat {
// For each conserved photon allocation, try its admissible spin frames until
// the visitor recognizes the signed term. Every allocation must be accepted.
bool verify_radiation_exchange(Algebra&, const std::vector<int>& fermions,
                               const std::vector<int>& antifermions, const std::vector<int>& photons,
                               const std::vector<int>& types, bool physical, const std::function<bool(Id,Id)>& accepts);
std::optional<Id> fermion_current_proposal(Algebra &, const model &,
                                           const std::vector<std::string> &,
                                           std::size_t diagram_count,
                                           bool physical,
                                           std::optional<permutation_sum>* presentation = nullptr);
}
