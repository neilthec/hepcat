#pragma once

#include "identities.hpp"
#include "identity_check.hpp"
#include <cstddef>
#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <utility>
#include <vector>

namespace hepcat {

enum class generation_references { automatic, keep, physical, cyclic };

class Algebra;
class amp_expression;
namespace validation_detail {
std::uint32_t import_expression(Algebra&, const amp_expression&, bool remainder = false);
}

struct numerator_shell {
    std::vector<int> legs;
    std::string mass;
};

// Apply every permutation of legs to the native FullForm summand and add
// the results. Multiplicities of identical images are already normalized.
struct permutation_formula {
    std::vector<int> legs;
    std::string summand;
    std::vector<std::size_t> orbit_sizes; // Empty for a kernel derived from ordered diagrams.
};

// Form K + sign * C(K), where C swaps every listed pair simultaneously.
struct signed_exchange {
    std::vector<std::pair<int,int>> swaps;
    int sign;
};

// FullForm input preserves model-local symbolic masses. External particle
// labels are 1 through masses.size(), in the all-incoming convention.
class amp_expression {
public:
    amp_expression(const std::string& expression,
                   const std::vector<std::string>& masses,
                   const std::vector<numerator_shell>& shells = {});
    amp_expression& reduce();
    // Choose the pole form for a recognized, equivalent permutation summand.
    amp_expression& reduce(generation_references preference);
    // Evaluated with external kinematics only. Adding this to the current
    // expression recovers the input away from the cut.
    std::optional<amp_expression> discarded_remainder() const;
    std::string str() const;
    // Readable spinor brackets and invariants for display. str() preserves exact FullForm.
    std::string plain_text() const;
    std::string latex() const;
    std::optional<permutation_formula> permutation_form() const;
    // Each group permutes independently. The single-group API expands the
    // secondary groups so its original summation convention still applies.
    struct permutation_product {
        std::vector<std::vector<int>> groups;
        std::string summand;
    };
    std::optional<permutation_product> grouped_permutation_form() const;
    // Each odd permutation in an alternating group contributes a minus sign.
    // Apply joint_exchange to the summand first, then sum over the groups.
    // The unsigned accessors expand the exchange and alternating groups.
    struct projected_term {
        std::string summand;
        std::vector<signed_exchange> exchanges;
    };
    struct signed_permutation_product {
        std::vector<std::vector<int>> symmetric_groups, alternating_groups;
        std::string summand;
        std::optional<signed_exchange> joint_exchange{};
        std::vector<std::size_t> orbit_sizes{};
        // The ordinary summand above already includes all local exchanges.
        // For its compact version apply each term's exchanges in order, add
        // the terms, then apply these common inner exchanges in order. Apply
        // joint_exchange and the global permutation groups to either result.
        std::vector<signed_exchange> inner_exchanges{};
        std::vector<projected_term> projected_terms{};
    };
    std::optional<signed_permutation_product> signed_permutation_form() const;
    // Weighted score of the displayed summand, before any permutation sums.
    std::optional<std::size_t> permutation_summand_complexity() const;
    // Load HEPCAT, then evaluate the result in a Wolfram kernel. By default
    // the expression is paired with its external and diagram-local mass rules.
    std::string mathematica(bool include_mass_rules = true) const;

    // Explore a single occurrence. The alternative shares unchanged nodes.
    std::vector<identity_candidate> identity_candidates(const identity_options& options = {}) const;
    // Measure a single edit in scratch space without retaining its alternative.
    identity_score preview_identity(const identity_candidate& edit) const;
    amp_expression apply_identity(const identity_candidate& edit) const;
    identity_check_report check_identity(const amp_expression& alternative,
                                         const identity_check_options& options = {}) const;
    std::size_t particle_count() const;
    std::size_t complexity() const;
    std::size_t leaf_count() const;
private:
    friend class amplitude;
    friend std::uint32_t validation_detail::import_expression(Algebra&, const amp_expression&, bool);
    amp_expression& reduce_with_factor_passes(unsigned passes);
    void restore_prepared(const std::string& remainder);
    std::uint32_t import_into(Algebra& destination, bool remainder = false) const;
    std::vector<std::uint32_t> import_masses(Algebra& destination) const;
    void reduce_from(Algebra& worker, std::uint32_t source, std::optional<std::uint32_t> original = {});
    void set_from(const Algebra& source, std::uint32_t result);
    std::uint32_t apply_in_trial(Algebra& trial, const identity_candidate& edit) const;
    struct context;
    // This representation belongs to a certified complete graph sum. Propagate
    // it through exact reductions and drop it if completion changes the amplitude.
    std::shared_ptr<const signed_permutation_product> ordered_formula_;
    struct cached_permutations;
    const cached_permutations& reduced_permutations() const; // caller holds context lock
    mutable std::shared_ptr<const cached_permutations> permutations_;
    static std::string format_plain_text(const Algebra&, std::uint32_t);
    static std::string format_latex(const Algebra&, std::uint32_t);
    std::shared_ptr<const context> kinematics_;
    std::shared_ptr<Algebra> arena_;
    std::uint32_t root_;
    bool reduced_ = false;
    std::optional<bool> summand_physical_;
    std::optional<std::uint32_t> remainder_;
};

} // namespace hepcat
