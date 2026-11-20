// Ordinary identities at individual occurrences in an external on-shell expression.
// A candidate stores a recipe. Only the chosen right hand side is constructed.
#include "identities_detail.hpp"
#include "spinor_word.hpp"

#include <algorithm>
#include <sstream>
#include <stdexcept>
#include <unordered_set>

namespace hepcat {
using rule = identity_rule;

const char* identity_name(rule r) {
    switch (r) {
    case rule::endpoint_mass:
        return "Apply the external Dirac equation";
    case rule::expand_endpoint_mass:
        return "Insert an endpoint momentum (nonzero external mass)";
    case rule::chain_square:
        return "Contract adjacent equal momenta";
    case rule::clifford_swap:
        return "Anticommute adjacent momenta";
    case rule::dot_into_chain:
        return "Insert a scalar product into a chain";
    case rule::momentum_conservation:
        return "Eliminate an external momentum by conservation";
    case rule::split_momentum:
        return "Expand a sum of momenta by linearity";
    case rule::split_massless:
        return "Split a null momentum into helicity spinors";
    case rule::join_massless:
        return "Join helicity factors on a shared null momentum";
    case rule::invariant_to_dots:
        return "Expand a Mandelstam invariant into scalar products";
    case rule::trace_pair:
        return "Contract a two-momentum trace to twice a scalar product";
    case rule::dot_to_trace:
        return "Write a scalar product as half a two-momentum trace";
    case rule::massless_brackets:
        return "Write a null scalar product as two brackets";
    case rule::schouten_expand:
        return "Recouple two chains using Schouten";
    case rule::schouten_contract:
        return "Collect a Schouten sum";
    case rule::spinor_contraction:
        return "Reduce ordinary spinor contractions";
    case rule::common_momentum_minor:
        return "Exchange spinors on a common momentum";
    case rule::external_kinematics:
        return "Use external masses and scalar kinematics";
    case rule::distribute:
        return "Distribute one factor over a sum";
    case rule::factor_common:
        return "Extract common factors";
    }
    throw std::invalid_argument("unknown identity rule");
}

std::vector<std::size_t> identity_candidate::positions() const {
    switch (rule_) {
    case rule::schouten_expand:
    case rule::join_massless:
        return {slots_.begin(), slots_.end()};
    case rule::dot_into_chain:
        return {slots_.begin(), slots_.begin() + 3};
    case rule::common_momentum_minor:
        return {slots_.begin(), slots_.begin() + 2};
    case rule::endpoint_mass:
    case rule::expand_endpoint_mass:
    case rule::clifford_swap:
    case rule::chain_square:
    case rule::split_massless:
    case rule::split_momentum:
    case rule::momentum_conservation:
    case rule::distribute:
        return {slots_[0]};
    default:
        return {};
    }
}

std::string identity_candidate::description() const {
    std::ostringstream out;
    out << identity_name(rule_) << " at root";
    for (auto i : path_)
        out << '[' << i << ']';
    switch (rule_) {
    case rule::endpoint_mass:
    case rule::expand_endpoint_mass:
        out << (slots_[0] == 0 ? ", left end" : ", right end");
        break;
    case rule::dot_into_chain:
        out << ", dot factor " << slots_[0] << ", chain factor " << slots_[1] << ", insertion at argument "
            << slots_[2];
        break;
    case rule::schouten_expand:
        out << ", factors " << slots_[0] << " and " << slots_[1] << ", cuts after " << slots_[2] << " and "
            << slots_[3] << " insertions";
        break;
    case rule::join_massless:
        out << ", factors " << slots_[0] << " and " << slots_[1] << ", " << (slots_[2] ? "right" : "left")
            << " and " << (slots_[3] ? "right" : "left") << " ends";
        break;
    case rule::common_momentum_minor:
        out << ", factors " << slots_[0] << " and " << slots_[1];
        break;
    case rule::clifford_swap:
    case rule::chain_square:
    case rule::split_massless:
    case rule::split_momentum:
    case rule::momentum_conservation:
    case rule::distribute:
        out << ", argument " << slots_[0];
        break;
    default:
        break;
    }
    return out.str();
}

namespace identity_detail {
using slots = std::array<std::size_t, 4>;

struct access {
    static identity_candidate candidate(const std::shared_ptr<const void>& context, Id root,
                                        const std::vector<std::size_t>& path, rule r, slots indices) {
        identity_candidate result;
        result.context_ = context;
        result.root_ = root;
        result.path_ = path;
        result.rule_ = r;
        result.slots_ = indices;
        return result;
    }
    static slots indices(const identity_candidate& c) { return c.slots_; }
};

namespace {
using namespace spinor_detail;

bool expands(rule r) {
    return r == rule::expand_endpoint_mass || r == rule::clifford_swap || r == rule::dot_into_chain ||
           r == rule::momentum_conservation || r == rule::split_momentum || r == rule::split_massless ||
           r == rule::invariant_to_dots || r == rule::dot_to_trace || r == rule::massless_brackets ||
           r == rule::schouten_expand || r == rule::common_momentum_minor || r == rule::distribute;
}

bool valid_chain(const Algebra& a, const Node& n) {
    if (n.head != "SpinorChain" || n.args.size() < 2)
        return false;
    auto l = endpoint(a, n.args.front()), r = endpoint(a, n.args.back());
    return l.kind != Kind::Unknown && r.kind != Kind::Unknown &&
           ((l.angle == r.angle) == (n.args.size() % 2 == 0)) &&
           std::all_of(n.args.begin() + 1, n.args.end() - 1,
                       [&](Id x) { return a.node(x).head == "Mom" && a.node(x).args.size() == 1; });
}

// MomProd and SpinorTrace both admit a labelled external momentum or an
// unsigned sum of distinct external momenta. This identity requires no cut.
bool external_momentum_label(const Algebra& a, Id label) {
    if (external(a, a.leg(label)))
        return true;
    const auto& n = a.node(label);
    if (n.head != "Multiparticle" || n.args.empty())
        return false;
    auto members = n.args;
    std::sort(members.begin(), members.end());
    return std::adjacent_find(members.begin(), members.end()) == members.end() &&
           std::all_of(members.begin(), members.end(), [&](Id leg) { return external(a, a.leg(leg)); });
}

Id helicity(Algebra& a, Id label, bool angle) {
    return a.call("Spinor", {a.atom("\"Helicity\""), a.atom(angle ? "\"Angle\"" : "\"Square\""), label});
}

// One Schouten cut treats each side of a chain as a two-component spinor.
// Reversing the cut prefixes/suffixes gives (-1)^(m + |q| - n) in HEPCAT.
Id recouple(Algebra& a, const Word& u, const Word& v, std::size_t m, std::size_t n) {
    Word lp(u.begin() + 1, u.begin() + 1 + m), rp(u.begin() + 1 + m, u.end() - 1);
    Word lq(v.begin() + 1, v.begin() + 1 + n), rq(v.begin() + 1 + n, v.end() - 1);
    std::reverse(lp.begin(), lp.end());
    std::reverse(rq.begin(), rq.end());
    auto join = [&](Id first, const Word& prefix, const Word& suffix, Id last) {
        Word w{first};
        w.insert(w.end(), prefix.begin(), prefix.end());
        w.insert(w.end(), suffix.begin(), suffix.end());
        w.push_back(last);
        return a.call("SpinorChain", std::move(w));
    };
    auto direct = a.mul({join(v.back(), rq, rp, u.back()), join(v.front(), lq, lp, u.front())});
    auto cross = a.mul({join(v.back(), rq, lp, u.front()), join(v.front(), lq, rp, u.back())});
    return signedValue(a, (m + rq.size()) % 2 ? -1 : 1, a.add({direct, signedValue(a, -1, cross)}));
}

// Strip one copy at each of two distinct factor positions. A powered bracket
// can participate without expanding its other copies.
struct factor {
    Id base;
    long copies;
};
factor positive_factor(const Algebra& a, Id id) {
    const auto& n = a.node(id);
    if (n.head == "Power" && n.args.size() == 2)
        return {n.args[0], a.leg(n.args[1])};
    return {id, 1};
}
Id replace_pair(Algebra& a, const Word& factors, std::size_t i, std::size_t j, Id value) {
    Word out{value};
    for (std::size_t k = 0; k < factors.size(); ++k) {
        if (k != i && k != j)
            out.push_back(factors[k]);
        else {
            const auto f = positive_factor(a, factors[k]);
            if (f.copies > 1)
                out.push_back(a.pow(f.base, f.copies - 1));
        }
    }
    return a.mul(std::move(out));
}

// Enumerating and applying share the same legality predicates. Enumeration
// omits the cost of building a right hand side with many terms.
class search {
public:
    search(Algebra& a, Id root, const identity_options& opts, const std::shared_ptr<const void>& context,
           const identity_candidate* selected = nullptr)
        : a(a), root(root), opts(opts), context(context), selected(selected) {}

    std::vector<identity_candidate> list(const std::vector<std::size_t>& subtree) {
        path = subtree;
        walk(locate(root, subtree));
        return std::move(found);
    }

    Id apply_one() {
        path = selected->path();
        Id current = locate(root, path);
        local(current);
        if (!replacement)
            throw std::invalid_argument("no longer a legal identity at this occurrence");
        // Rebuild only the path to this occurrence in the DAG.
        std::vector<Id> ancestors;
        current = root;
        for (auto i : path) {
            ancestors.push_back(current);
            current = a.node(current).args.at(i);
        }
        Id result = *replacement;
        for (std::size_t depth = path.size(); depth > 0; --depth) {
            const auto& parent = a.node(ancestors[depth - 1]);
            Word args = parent.args;
            args[path[depth - 1]] = result;
            result = a.call(parent.head, std::move(args));
        }
        return result;
    }

private:
    Algebra& a;
    Id root;
    const identity_options& opts;
    const std::shared_ptr<const void>& context;
    const identity_candidate* selected;
    std::vector<std::size_t> path;
    std::vector<identity_candidate> found;
    // A shared subtree with no matches needs to be searched only once. Hits
    // are visited at every occurrence because each edit has a distinct path.
    std::unordered_set<Id> empty_subtrees;
    std::optional<Id> replacement;

    bool full() const { return replacement.has_value() || (opts.limit && found.size() >= opts.limit); }
    bool wants(rule r) const {
        return !full() && (!opts.rule || *opts.rule == r) && (opts.include_expansions || !expands(r));
    }
    template <class Build> void offer(rule r, slots position, Build&& rhs) {
        if (!wants(r))
            return;
        if (!selected)
            found.push_back(access::candidate(context, root, path, r, position));
        else if (position == access::indices(*selected))
            replacement = rhs();
    }
    // For existing cost-reducing operations the native routine also determines
    // applicability. New nodes are temporary and never accumulate in the input arena.
    template <class Build> void offer_changed(rule r, Id original, Build&& rhs, slots position = {}) {
        if (!wants(r) || (selected && position != access::indices(*selected)))
            return;
        const auto candidate = rhs();
        if (candidate != original)
            offer(r, position, [&] { return candidate; });
    }
    static bool child_accessible(const Node& n, std::size_t index) {
        return index < n.args.size() &&
               (n.head == "Times" || n.head == "Plus" || (n.head == "Power" && index == 0));
    }
    Id locate(Id current, const std::vector<std::size_t>& address) const {
        for (auto index : address) {
            const auto& n = a.node(current);
            if (!child_accessible(n, index))
                throw std::invalid_argument(
                    "identity path leaves the arithmetic expression or has an invalid index");
            current = n.args[index];
        }
        return current;
    }
    void walk(Id id) {
        const auto& n = a.node(id);
        if (!n.compound || empty_subtrees.count(id))
            return;
        const auto prior = found.size();
        local(id);
        for (std::size_t i = 0; i < n.args.size() && !full(); ++i) {
            if (!child_accessible(n, i))
                continue;
            path.push_back(i);
            walk(n.args[i]);
            path.pop_back();
        }
        if (prior == found.size())
            empty_subtrees.insert(id);
    }

    void words(Id id, const Node& n, bool trace) {
        const auto& w = n.args;
        const std::size_t start = trace ? 0 : 1, stop = trace ? w.size() : w.size() - 1;
        if (!trace) {
            for (bool right : {false, true}) {
                auto k = right ? w.size() - 1 : 0;
                auto e = endpoint(a, w[k]);
                if (!external(a, e.leg) || e.kind == Kind::Xi)
                    continue;
                auto mass = a.masses()[e.leg - 1];
                if (e.kind == Kind::Helicity && mass != a.integer(0))
                    continue;
                auto near = right ? w.size() - 2 : 1;
                if (start < stop && momentumLeg(a, w[near]) == e.leg)
                    offer(rule::endpoint_mass, {k}, [&] {
                        auto new_word = w;
                        new_word[k] = flip(a, w[k], e);
                        new_word.erase(new_word.begin() + near);
                        return a.mul({a.integer(right ? -1 : 1), mass, a.call(n.head, std::move(new_word))});
                    });
                if (e.kind == Kind::Spin && mass != a.integer(0))
                    offer(rule::expand_endpoint_mass, {k}, [&] {
                        auto new_word = w;
                        new_word[k] = flip(a, w[k], e);
                        auto mom = a.call("Mom", {e.label});
                        new_word.insert(new_word.begin() + (right ? k : 1), mom);
                        return a.mul({a.integer(right ? -1 : 1), a.pow(mass, -1),
                                      a.call(n.head, std::move(new_word))});
                    });
            }
        }
        for (std::size_t i = start; i < stop && !full(); ++i) {
            const auto& slot = a.node(w[i]);
            if (slot.head != "Mom" || slot.args.size() != 1)
                continue;
            const auto& label = a.node(slot.args[0]);
            const int leg = a.leg(slot.args[0]);
            if (label.head == "Multiparticle" && label.args.size() > 1 &&
                std::all_of(label.args.begin(), label.args.end(),
                            [&](Id x) { return external(a, a.leg(x)); })) {
                offer(rule::split_momentum, {i}, [&] {
                    Word sum;
                    for (auto member : label.args) {
                        auto item = w;
                        item[i] = a.call("Mom", {member});
                        sum.push_back(a.call(n.head, std::move(item)));
                    }
                    return a.add(std::move(sum));
                });
            }
            if (!external(a, leg))
                continue;
            if (a.masses().size() > 1)
                offer(rule::momentum_conservation, {i}, [&] {
                    Word sum;
                    for (std::size_t j = 0; j < a.masses().size(); ++j) {
                        if (static_cast<int>(j + 1) == leg)
                            continue;
                        auto item = w;
                        item[i] = a.call("Mom", {a.integer(static_cast<long>(j + 1))});
                        sum.push_back(a.call(n.head, std::move(item)));
                    }
                    return signedValue(a, -1, a.add(std::move(sum)));
                });
            if (!trace && a.masses()[leg - 1] == a.integer(0))
                offer(rule::split_massless, {i}, [&] {
                    bool angle = (i % 2 == 1) == endpoint(a, w.front()).angle;
                    Word left(w.begin(), w.begin() + i), right(w.begin() + i + 1, w.end());
                    left.push_back(helicity(a, slot.args[0], angle));
                    right.insert(right.begin(), helicity(a, slot.args[0], !angle));
                    return a.mul({a.call(n.head, std::move(left)), a.call(n.head, std::move(right))});
                });
            if (i + 1 >= stop)
                continue;
            const int next = momentumLeg(a, w[i + 1]);
            if (!external(a, next))
                continue;
            auto shorter = [&] {
                auto shortened = w;
                shortened.erase(shortened.begin() + i, shortened.begin() + i + 2);
                if (trace && shortened.empty())
                    return a.integer(2);
                return a.call(n.head, std::move(shortened));
            };
            if (leg == next)
                offer(rule::chain_square, {i},
                      [&] { return a.mul({a.pow(a.masses()[leg - 1], 2), shorter()}); });
            else
                offer(rule::clifford_swap, {i}, [&] {
                    auto swap = w;
                    std::swap(swap[i], swap[i + 1]);
                    return a.add({a.mul({a.integer(2), dot(a, leg, next), shorter()}),
                                  signedValue(a, -1, a.call(n.head, std::move(swap)))});
                });
        }
        offer_changed(rule::spinor_contraction, id, [&] { return reduceSpinor(a, id); });
    }

    void products(Id id, const Word& factors) {
        offer_changed(rule::spinor_contraction, id, [&] { return reduceSpinor(a, id); });
        std::vector<std::size_t> chains, dots;
        for (std::size_t i = 0; i < factors.size(); ++i) {
            const auto f = positive_factor(a, factors[i]);
            if (!f.copies)
                continue;
            const auto& n = a.node(f.base);
            if (valid_chain(a, n))
                chains.push_back(i);
            if (n.head == "MomProd" && n.args.size() == 2 && external(a, a.leg(n.args[0])) &&
                external(a, a.leg(n.args[1])))
                dots.push_back(i);
            if (f.copies == 1 && n.head == "Plus" && n.args.size() > 1)
                offer(rule::distribute, {i}, [&] {
                    Word terms;
                    for (auto summand : n.args) {
                        auto product = factors;
                        product[i] = summand;
                        terms.push_back(a.mul(std::move(product)));
                    }
                    return a.add(std::move(terms));
                });
        }
        if (wants(rule::dot_into_chain))
            for (auto di : dots)
                for (auto ci : chains) {
                    const auto& d = a.node(positive_factor(a, factors[di]).base).args;
                    const auto& w = a.node(positive_factor(a, factors[ci]).base).args;
                    for (std::size_t pos = 1; pos < w.size() && !full(); ++pos)
                        offer(rule::dot_into_chain, {di, ci, pos}, [&] {
                            auto append = [&](Id p, Id q) {
                                auto word = w;
                                word.insert(word.begin() + pos, {a.call("Mom", {p}), a.call("Mom", {q})});
                                return a.call("SpinorChain", std::move(word));
                            };
                            Id value = a.mul(
                                {a.exact(mpq_class(1, 2)), a.add({append(d[0], d[1]), append(d[1], d[0])})});
                            return replace_pair(a, factors, di, ci, value);
                        });
                    if (full())
                        return;
                }

        if (!wants(rule::schouten_expand) && !wants(rule::common_momentum_minor) &&
            !wants(rule::join_massless))
            return;
        for (std::size_t ix = 0; ix < chains.size(); ++ix)
            for (std::size_t jx = ix + 1; jx < chains.size(); ++jx) {
                const auto i = chains[ix], j = chains[jx];
                const auto x = positive_factor(a, factors[i]).base, y = positive_factor(a, factors[j]).base;
                const auto& u = a.node(x).args;
                const auto& v = a.node(y).args;
                if (wants(rule::join_massless)) {
                    for (bool right_u : {false, true})
                        for (bool right_v : {false, true}) {
                            const Id p = right_u ? u.back() : u.front(), q = right_v ? v.back() : v.front();
                            const auto ep = endpoint(a, p), eq = endpoint(a, q);
                            if (ep.kind != Kind::Helicity || eq.kind != Kind::Helicity || ep.leg != eq.leg ||
                                !external(a, ep.leg) || a.masses()[ep.leg - 1] != a.integer(0) ||
                                ep.angle == eq.angle || a.node(p).args.size() != 3 ||
                                a.node(q).args.size() != 3)
                                continue;
                            offer(rule::join_massless, {i, j, right_u, right_v}, [&] {
                                auto left = u, right = v;
                                if (!right_u)
                                    std::reverse(left.begin(), left.end());
                                if (right_v)
                                    std::reverse(right.begin(), right.end());
                                const bool negate =
                                    (!right_u && left.size() % 2 == 0) != (right_v && right.size() % 2 == 0);
                                left.back() = a.call("Mom", {ep.label});
                                left.insert(left.end(), right.begin() + 1, right.end());
                                return replace_pair(
                                    a, factors, i, j,
                                    signedValue(a, negate ? -1 : 1, a.call("SpinorChain", std::move(left))));
                            });
                        }
                }
                if (wants(rule::common_momentum_minor)) {
                    if (auto swapped = commonMomentumSwap(a, x, y))
                        offer(rule::common_momentum_minor, {i, j},
                              [&] { return replace_pair(a, factors, i, j, *swapped); });
                }
                if (!wants(rule::schouten_expand))
                    continue;
                auto left_u = endpoint(a, u.front()), left_v = endpoint(a, v.front());
                for (std::size_t m = 0; m + 1 < u.size() && !full(); ++m)
                    for (std::size_t n = 0; n + 1 < v.size() && !full(); ++n) {
                        if ((left_u.angle ^ bool(m % 2)) != (left_v.angle ^ bool(n % 2)))
                            continue;
                        // Recoupling adjacent identical half-words only reproduces the original pairing.
                        if (m == n && std::equal(u.begin(), u.begin() + m + 1, v.begin()))
                            continue;
                        if (u.size() - m == v.size() - n &&
                            std::equal(u.begin() + m + 1, u.end(), v.begin() + n + 1))
                            continue;
                        offer(rule::schouten_expand, {i, j, m, n},
                              [&] { return replace_pair(a, factors, i, j, recouple(a, u, v, m, n)); });
                    }
                if (full())
                    return;
            }
    }

    void local(Id id) {
        const auto& n = a.node(id);
        if (!n.compound)
            return;
        if (valid_chain(a, n)) {
            words(id, n, false);
            return;
        }
        if (n.head == "SpinorTrace" && n.args.size() % 2 == 0 &&
            std::all_of(n.args.begin(), n.args.end(),
                        [&](Id x) { return a.node(x).head == "Mom" && a.node(x).args.size() == 1; })) {
            if (n.args.size() == 2) {
                const Id p = a.node(n.args[0]).args[0], q = a.node(n.args[1]).args[0];
                if (external_momentum_label(a, p) && external_momentum_label(a, q))
                    offer(rule::trace_pair, {},
                          [&] { return a.mul({a.integer(2), a.call("MomProd", {p, q})}); });
            }
            words(id, n, true);
            return;
        }
        if (n.head == "Times") {
            products(id, n.args);
            return;
        }
        if (n.head == "Plus") {
            offer_changed(rule::schouten_contract, id, [&] { return reduceSpinor(a, id); });
            offer_changed(rule::factor_common, id, [&] { return a.factorSum(id); });
            return;
        }
        if (n.head == "Mass" || n.head == "MomProd" || n.head == "Power" || n.head == "Mandelstahm")
            offer_changed(rule::external_kinematics, id, [&] { return reduceKinematics(a, id, false); });
        if (n.head == "Mandelstahm" && n.args.size() >= 2 &&
            std::all_of(n.args.begin(), n.args.end(), [&](Id x) { return external(a, a.leg(x)); })) {
            offer(rule::invariant_to_dots, {}, [&] {
                Word terms;
                for (std::size_t i = 0; i < n.args.size(); ++i) {
                    const int p = a.leg(n.args[i]);
                    terms.push_back(dot(a, p, p));
                    for (std::size_t j = 0; j < i; ++j)
                        terms.push_back(a.mul({a.integer(2), dot(a, p, a.leg(n.args[j]))}));
                }
                return a.add(std::move(terms));
            });
        }
        if (n.head != "MomProd" || n.args.size() != 2)
            return;
        if (external_momentum_label(a, n.args[0]) && external_momentum_label(a, n.args[1]))
            offer(rule::dot_to_trace, {}, [&] {
                Id p = a.call("Mom", {n.args[0]}), q = a.call("Mom", {n.args[1]});
                return a.mul({a.exact(mpq_class(1, 2)), a.call("SpinorTrace", {p, q})});
            });
        int i = a.leg(n.args[0]), j = a.leg(n.args[1]);
        if (!external(a, i) || !external(a, j))
            return;
        if (a.masses().size() > 1) {
            for (std::size_t k = 0; k < (i == j ? 1u : 2u); ++k) {
                const int eliminate = k == 0 ? i : j, fixed = k == 0 ? j : i;
                offer(rule::momentum_conservation, {k}, [&] {
                    Word terms;
                    for (std::size_t leg = 1; leg <= a.masses().size(); ++leg)
                        if (int(leg) != eliminate)
                            terms.push_back(dot(a, fixed, static_cast<int>(leg)));
                    return signedValue(a, -1, a.add(std::move(terms)));
                });
            }
        }
        if (i == j || a.masses()[i - 1] != a.integer(0) || a.masses()[j - 1] != a.integer(0))
            return;
        offer(rule::massless_brackets, {}, [&] {
            Id p = n.args[0], q = n.args[1];
            auto ab = a.call("SpinorChain", {helicity(a, p, true), helicity(a, q, true)});
            auto ba = a.call("SpinorChain", {helicity(a, q, false), helicity(a, p, false)});
            return a.mul({a.exact(mpq_class(1, 2)), ab, ba});
        });
    }
};
} // namespace

std::vector<identity_candidate> enumerate(Algebra& trial, Id root, const std::shared_ptr<const void>& context,
                                          const identity_options& options) {
    return search(trial, root, options, context).list(options.subtree);
}
Id apply(Algebra& trial, Id root, const identity_candidate& edit) {
    identity_options options;
    options.rule = edit.rule();
    std::shared_ptr<const void> unused;
    return search(trial, root, options, unused, &edit).apply_one();
}

} // namespace identity_detail
} // namespace hepcat
