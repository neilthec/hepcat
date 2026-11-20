#pragma once
#include "reduce.hpp"
#include <map>

namespace hepcat {
// Rows contain the coefficients of powers above E^energy_bound, one column
// per expression. Kinematics and normalized coherent spin states vary with
// the sample index. Entries are exact rationals in independent field components.
std::vector<std::vector<mpq_class>>
high_energy_chart_rows(const Algebra& algebra, const std::vector<Id>& expressions, int energy_bound,
                       std::size_t sample, const std::map<Id, mpq_class>& parameter_values = {});

struct energy_chart_validation {
    bool passes;
    std::size_t spin_states;
    bool exhaustive_spin;
};
// For a polynomial in the external coherent spin coordinates, test a grid
// large enough to certify every spin coefficient at this fixed-angle direction.
// If its tensor-product grid exceeds the limit, use one generic spin projection.
energy_chart_validation high_energy_validate_chart(const Algebra&, Id expression, int bound,
                                                   std::size_t sample,
                                                   const std::map<Id, mpq_class>& parameter_values,
                                                   std::size_t max_spin_states = 128);
// Test cancellation at every finite pole on one conserved polynomial deformation.
struct regular_chart_validation {
    bool polynomial;
    std::vector<bool> varied;
};
regular_chart_validation regularity_chart(const Algebra&, Id cleared_amplitude,
                                          const std::vector<Id>& probe_denominators, std::size_t sample,
                                          std::size_t pairing,
                                          const std::map<Id, mpq_class>& parameter_values = {});
} // namespace hepcat
