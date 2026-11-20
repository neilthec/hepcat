// Targeted external-kinematic rewrites for a manual native C++ derivation.
#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <vector>

namespace hepcat {

enum class identity_rule {
    endpoint_mass,
    expand_endpoint_mass,
    chain_square,
    clifford_swap,
    dot_into_chain,
    momentum_conservation,
    split_momentum,
    split_massless,
    join_massless,
    massless_brackets,
    trace_pair,
    dot_to_trace,
    schouten_expand,
    schouten_contract,
    spinor_contraction,
    common_momentum_minor,
    external_kinematics,
    invariant_to_dots,
    distribute,
    factor_common
};

const char* identity_name(identity_rule rule);

struct identity_options {
    std::optional<identity_rule> rule; // Empty selects every rule.
    std::vector<std::size_t> subtree;  // Zero-based FullForm argument path.
    bool include_expansions = true;    // Include inverse and branching identities.
    std::size_t limit = 256;           // Maximum returned; zero requests all.
};

// Structural scores of an alternative evaluated in scratch storage.
struct identity_score {
    std::size_t complexity;
    std::size_t leaves;
};

namespace identity_detail {
struct access;
}

// A candidate is valid on the originating expression and on its unchanged
// value copies. Its immutable site identifies a single occurrence in the DAG.
// Internal cut conditions are never used by these ordinary identities.
class identity_candidate {
public:
    identity_rule rule() const { return rule_; }
    const std::vector<std::size_t>& path() const { return path_; }
    // Positions inside that occurrence: a chain argument, two factors, or
    // (factor, factor, cut, cut) for Schouten. Cuts count momentum insertions.
    // Null joins use (factor, factor, end, end), with 0 = left and 1 = right.
    std::vector<std::size_t> positions() const;
    std::string description() const;

private:
    friend class amp_expression;
    friend struct identity_detail::access;
    identity_candidate() = default;
    bool belongs_to(const std::shared_ptr<const void>& context, std::uint32_t root) const {
        return root == root_ && context_.lock() == context;
    }

    std::weak_ptr<const void> context_;
    std::uint32_t root_ = 0;
    identity_rule rule_ = identity_rule::endpoint_mass;
    std::vector<std::size_t> path_;
    std::array<std::size_t, 4> slots_{};
};

} // namespace hepcat
