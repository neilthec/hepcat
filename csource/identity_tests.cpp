// Explicit identities are validated as signed amplitudes at conserved kinematics.
// The inputs below are algebraic probes and carry no built-in process answers.
#include "amp_expression.hpp"

#include <cmath>
#include <iostream>
#include <stdexcept>

using namespace hepcat;
using rule = identity_rule;

namespace {
void require(bool ok, const std::string& message) {
    if (!ok)
        throw std::runtime_error(message);
}
std::string end(int i, bool angle, bool massless = false) {
    return std::string("Spinor[") + (massless ? "\"Helicity\"," : "\"Spin\",") +
           (angle ? "\"Angle\"," : "\"Square\",") + std::to_string(i) + "]";
}
std::string chain(int i, bool angle, std::vector<int> insertions, int j) {
    auto word = "SpinorChain[" + end(i, angle);
    for (auto p : insertions)
        word += ",Mom[" + std::to_string(p) + "]";
    return word + "," + end(j, angle ^ (insertions.size() % 2)) + "]";
}
std::vector<identity_candidate> edits(const amp_expression& amp, identity_rule r) {
    identity_options options;
    options.rule = r;
    options.limit = 0;
    return amp.identity_candidates(options);
}
void equal(const amp_expression& a, const amp_expression& b) {
    identity_check_options options;
    options.seed = 184967;
    options.samples = 3;
    auto check = a.check_identity(b, options);
    require(check.passed() && check.compared == 3, a.str() + " and " + b.str() + ": " + check.detail);
}

// Check every site for this rule, including both choices of ends/chirality.
void verify(const std::string& source, identity_rule r, const std::vector<std::string>& masses) {
    amp_expression a(source, masses);
    const auto candidates = edits(a, r);
    require(!candidates.empty(), "No candidates for " + std::string(identity_name(r)));
    for (const auto& edit : candidates) {
        require(!edit.description().empty(), "Empty identity description");
        auto metric = a.preview_identity(edit);
        auto b = a.apply_identity(edit);
        require(metric.complexity == b.complexity() && metric.leaves == b.leaf_count(),
                "Trial scores differ from the accepted rewrite: " + edit.description());
        require(b.str() != a.str(), "Identity returned the original expression: " + edit.description());
        equal(a, b);
    }
    require(a.str() == source, "Exploring candidates mutated the original expression");
}

template <class F> void invalid(F&& operation) {
    try {
        operation();
    } catch (const std::invalid_argument&) {
        return;
    }
    throw std::runtime_error("Expected to reject an invalid or stale identity");
}

void algebra() {
    const std::vector<std::string> massive{"2", "3", "5", "7", "11", "13"};
    for (bool angle : {false, true}) {
        verify(chain(1, angle, {1, 4}, 4), rule::endpoint_mass, massive);
        verify(chain(1, angle, {}, 4), rule::expand_endpoint_mass, massive);
        verify(chain(1, angle, {2, 2, 3}, 4), rule::chain_square, massive);
        verify(chain(1, angle, {2, 3, 4}, 4), rule::clifford_swap, massive);
        verify(chain(1, angle, {3, 2}, 4), rule::momentum_conservation, massive);
        verify("Times[MomProd[2,3],Power[" + chain(1, angle, {2}, 4) + ",2]]", rule::dot_into_chain, massive);
        for (bool second : {false, true})
            verify("Times[" + chain(1, angle, {3, 2}, 4) + "," + chain(5, second, {2, 3}, 6) + "]",
                   rule::schouten_expand, massive);
        verify("Times[" + chain(1, angle, {2}, 4) + "," + chain(5, angle, {2}, 6) + "]",
               rule::common_momentum_minor, massive);
        verify("Plus[Times[" + chain(1, angle, {}, 2) + "," + chain(3, angle, {}, 4) + "],Times[" +
                   chain(1, angle, {}, 4) + "," + chain(2, angle, {}, 3) + "]]",
               rule::schouten_contract, massive);
    }
    verify("SpinorTrace[Mom[1],Mom[2],Mom[3],Mom[4]]", rule::clifford_swap, massive);
    verify("SpinorTrace[Mom[3],Mom[3]]", rule::chain_square, massive);
    verify("SpinorTrace[Mom[1],Mom[Multiparticle[2,3]]]", rule::split_momentum, massive);
    verify("Times[Plus[3,5],MomProd[1,2]]", rule::distribute, massive);
    verify("Plus[Times[3,MomProd[1,2]],Times[5,MomProd[1,2]]]", rule::factor_common, massive);
    verify("Mandelstahm[2,3,6]", rule::invariant_to_dots, massive);
    verify("MomProd[3,4]", rule::momentum_conservation, massive);
    verify("MomProd[3,3]", rule::momentum_conservation, massive);
    const auto scalar_moves = edits(amp_expression("MomProd[3,4]", massive), rule::momentum_conservation);
    require(scalar_moves.size() == 2 && scalar_moves[0].positions() == std::vector<std::size_t>{0} &&
                scalar_moves[1].positions() == std::vector<std::size_t>{1},
            "Missing scalar momentum substitution");
    require(edits(amp_expression("MomProd[3,3]", massive), rule::momentum_conservation).size() == 1,
            "Duplicate scalar substitution for the same momentum");
    for (const std::string second : {"1", "2", "Multiparticle[2,5]"}) {
        verify("SpinorTrace[Mom[1],Mom[" + second + "]]", rule::trace_pair, massive);
        verify("MomProd[1," + second + "]", rule::dot_to_trace, massive);
    }
    for (const std::string invalid : {"0", "7", "Multiparticle[3,3]", "Q"}) {
        require(edits(amp_expression("MomProd[1," + invalid + "]", massive), rule::dot_to_trace).empty(),
                "Unexpected trace expansion outside the declared external momenta");
        require(edits(amp_expression("SpinorTrace[Mom[1],Mom[" + invalid + "]]", massive), rule::trace_pair)
                    .empty(),
                "Unexpected trace contraction outside the declared external momenta");
    }
    verify("MomProd[2,2]", rule::external_kinematics, massive);
    verify(chain(4, true, {1}, 3), rule::spinor_contraction, massive);
    const std::vector<std::string> mixed{"2", "0", "0", "2", "0", "0"};
    verify("MomProd[2,3]", rule::massless_brackets, mixed);
    verify(chain(1, false, {2, 3, 2}, 4), rule::split_massless, mixed);
    // Two massless endpoints may meet at either end of either chain. Verify
    // the reversal signs independently, including a powered spectator copy.
    for (bool angle : {false, true})
        for (bool right_u : {false, true})
            for (bool right_v : {false, true}) {
                auto U = end(1, !angle), P = end(2, angle, true), Q = end(2, !angle, true),
                     V = end(4, !angle);
                auto u = "SpinorChain[" + (right_u ? U + ",Mom[3]," + P : P + ",Mom[3]," + U) + "]";
                auto v = "SpinorChain[" + (right_v ? V + "," + Q : Q + "," + V) + "]";
                verify("Times[Power[" + u + ",2]," + v + "]", rule::join_massless, mixed);
            }
    verify("Power[" + chain(1, true, {2, 3}, 4) + ",-1]", rule::clifford_swap, massive);
}

void occurrences_and_domains() {
    std::vector<std::string> masses{"2", "3", "5", "7"};
    const auto word = chain(1, false, {1}, 2);
    amp_expression a("Plus[" + word + "," + word + "]", masses);
    identity_options options;
    options.rule = rule::endpoint_mass;
    options.subtree = {1};
    auto edits_right = a.identity_candidates(options);
    require(edits_right.size() == 1 && edits_right.front().path() == options.subtree, "Wrong occurrence");
    require(edits_right.front().positions() == std::vector<std::size_t>{0}, "Wrong endpoint argument");
    auto changed = a.apply_identity(edits_right.front());
    require(changed.str().find(word) != std::string::npos, "Both shared occurrences changed");
    equal(a, changed);
    invalid([&] { changed.apply_identity(edits_right.front()); });
    invalid([&] { changed.preview_identity(edits_right.front()); });
    amp_expression another(a.str(), masses);
    invalid([&] { another.apply_identity(edits_right.front()); });
    options.subtree = {42};
    invalid([&] { a.identity_candidates(options); });

    options = {};
    options.limit = 1;
    require(a.identity_candidates(options).size() == 1, "Candidate budget ignored");
    options = {};
    options.rule = rule::expand_endpoint_mass;
    options.include_expansions = false;
    require(a.identity_candidates(options).empty(), "Expansion filter ignored");
    amp_expression with_cut("MomProd[1,2]", masses, {{{1, 2}, "11"}});
    require(edits(with_cut, rule::external_kinematics).empty(), "Internal cut treated as a global identity");
    amp_expression opaque("PropDen[Mom[Multiparticle[1,2]]," + word + "]", masses);
    require(opaque.identity_candidates().empty(), "Inspected propagator internals");
    amp_expression xi("SpinorChain[Spinor[\"Xi\",\"Angle\",1],Mom[1]," + end(2, false) + "]", masses);
    require(edits(xi, rule::endpoint_mass).empty(), "Applied a Dirac equation to a reference spinor");

    // A diagram's retained off-cut difference survives an ordinary algebraic step.
    amp_expression diagram("Times[MomProd[1,2]," + chain(1, false, {}, 4) + "]", masses, {{{1, 2}, "11"}});
    diagram.reduce();
    auto remainder = diagram.discarded_remainder();
    require(remainder.has_value(), "Missing diagram-local remainder");
    auto step = diagram.apply_identity(edits(diagram, rule::expand_endpoint_mass).at(0));
    require(step.discarded_remainder().has_value() && step.discarded_remainder()->str() == remainder->str(),
            "Lost the previous off-cut remainder");
    equal(diagram, step);
}
} // namespace

int main() {
    try {
        algebra();
        occurrences_and_domains();
        std::cout << "Signed explicit identity and occurrence tests passed\n";
        return 0;
    } catch (const std::exception& e) {
        std::cerr << e.what() << '\n';
        return 1;
    }
}
