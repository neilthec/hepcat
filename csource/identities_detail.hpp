#pragma once
#include "identities.hpp"
#include "reduce.hpp"

namespace hepcat::identity_detail {

// Work in a temporary arena borrowing the input DAG and its external masses.
// Edits do not require diagram-local on-shell conditions.
std::vector<identity_candidate> enumerate(Algebra& trial, Id root, const std::shared_ptr<const void>& context,
                                          const identity_options& options);
Id apply(Algebra& trial, Id root, const identity_candidate& edit);

} // namespace hepcat::identity_detail
