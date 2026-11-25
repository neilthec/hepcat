#pragma once
#include "reduce.hpp"

namespace hepcat {
class amplitude;
class model;
struct factorization_report;

// Reconstruct simple-pole numerators from constructive lower-point products.
std::optional<Id> five_point_local_poles(Algebra &, const amplitude &, const model &, factorization_report &);
} // namespace hepcat
