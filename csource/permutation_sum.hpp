#pragma once

#include "reduce.hpp"
#include "amp_expression.hpp"

namespace hepcat {

struct projected_kernel {
    Id kernel;
    std::vector<signed_exchange> exchanges;
};

// An unnormalized sum over every permutation of legs, acting on kernel.
// The elementary expression remains available for algebra and numerical export.
struct permutation_sum {
    std::vector<int> legs;
    Id kernel;
    std::vector<std::size_t> orbit_sizes;
    std::vector<std::vector<int>> other_groups{};
    std::vector<std::vector<int>> alternating_groups{};
    std::optional<signed_exchange> joint_exchange{};
    // kernel already includes the term-local operations. These common
    // operations act afterwards, before joint_exchange.
    std::vector<signed_exchange> inner_exchanges{};
    std::vector<projected_kernel> projected_terms{};
};
// Complete the three-photon Weyl kernel by simultaneous charge conjugation.
// Only photons of the same helicity enter the symmetric group.
permutation_sum radiation_exchange_sum(Algebra&, const std::vector<int>& fermions,
                                      const std::vector<int>& antifermions, const std::vector<int>& photons,
                                      const std::vector<int>& types, Id mass, Id coefficient, bool physical);
// Preserves the sum over all permutations of legs. Individual summands can change.
Id simplify_permutation_kernel(Algebra&, Id, const std::vector<int>& legs,
                               const std::vector<std::vector<int>>& other_groups = {});
// An exact summand weight is available when the expansion fits the scoring budget.
std::optional<std::size_t> bounded_summand_weight(const Algebra&, Id);
// Reduce one independently projected candidate in a bounded-lifetime algebra arena.
Id reduce_projected_candidate(Algebra&, Id, const permutation_sum&);
// Reduce a summand modulo its certified signed exchange symmetries.
Id simplify_signed_kernel(Algebra&, Id, const permutation_sum&);
// Relabel external momenta and spinors in a certified current. The source
// and destination give the same complete or partial permutation domain.
std::optional<Id> permute_current(Algebra &, Id, const std::vector<int> &from, const std::vector<int> &to);
// Explicitly sum disjoint secondary groups for clients of the single-group API.
Id expand_permutation_groups(Algebra&, Id, const std::vector<std::vector<int>>&, bool alternating = false);
Id apply_exchanges(Algebra&, Id, const std::vector<signed_exchange>&);
Id sum_projected_terms(Algebra&, const std::vector<projected_kernel>&);
Id expand_signed_operators(Algebra&, const permutation_sum&);
// Recognize an equivalent current from its signed coefficients and external masses.
std::optional<permutation_sum> recognize_ordered_current(Algebra& a, Id input,
                                                        std::optional<bool> physical = {});
std::optional<permutation_sum> compact_permutations(Algebra& a, Id expression);

} // namespace hepcat
