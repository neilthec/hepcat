#include "reduce.hpp"

#include <algorithm>
#include <iterator>
#include <limits>
#include <unordered_map>
#include <optional>

namespace hepcat {
namespace {

using Legs = std::vector<int>;

bool external(const Algebra& algebra, int leg) {
    return leg > 0 && static_cast<std::size_t>(leg) <= algebra.masses().size();
}

// A Multiparticle denotes an unsigned sum of distinct all-incoming momenta.
// Decline signed, deformed or otherwise unrecognized labels.
std::optional<Legs> members(const Algebra& algebra, Id label) {
    if (int leg = algebra.leg(label); external(algebra, leg))
        return Legs{leg};
    const auto& node = algebra.node(label);
    if (node.head != "Multiparticle" || node.args.empty())
        return std::nullopt;
    Legs legs;
    for (Id item : node.args) {
        int leg = algebra.leg(item);
        if (!external(algebra, leg))
            return std::nullopt;
        legs.push_back(leg);
    }
    std::sort(legs.begin(), legs.end());
    if (std::adjacent_find(legs.begin(), legs.end()) != legs.end())
        return std::nullopt;
    return legs;
}

Legs complement(const Algebra& algebra, const Legs& legs) {
    Legs result;
    auto next = legs.begin();
    for (int i = 1; static_cast<std::size_t>(i) <= algebra.masses().size(); ++i) {
        if (next != legs.end() && *next == i)
            ++next;
        else
            result.push_back(i);
    }
    return result;
}

std::optional<Id> directMass(const Algebra& algebra, const Legs& legs, bool useInternal) {
    if (legs.size() == 1)
        return algebra.masses()[legs.front() - 1];
    if (useInternal) {
        auto found = algebra.shells().find(legs);
        if (found != algebra.shells().end())
            return found->second;
    }
    return std::nullopt;
}

std::optional<Id> knownSquare(Algebra& algebra, const Legs& legs, bool useInternal) {
    if (legs.empty() || legs.size() == algebra.masses().size())
        return algebra.integer(0);
    auto mass = directMass(algebra, legs, useInternal);
    if (!mass)
        mass = directMass(algebra, complement(algebra, legs), useInternal);
    if (mass)
        return algebra.pow(*mass, 2);
    return std::nullopt;
}

Id sub(Algebra& algebra, Id left, Id right) {
    return algebra.add({left, algebra.mul({algebra.integer(-1), right})});
}

Id half(Algebra& algebra, Id value) {
    return algebra.mul({algebra.pow(algebra.integer(2), -1), value});
}

std::optional<Id> knownDot(Algebra& algebra, const Legs& a, const Legs& b, bool useInternal) {
    if (a == b)
        return knownSquare(algebra, a, useInternal);
    if (a.size() == algebra.masses().size() || b.size() == algebra.masses().size())
        return algebra.integer(0);
    auto aa = knownSquare(algebra, a, useInternal);
    auto bb = knownSquare(algebra, b, useInternal);
    if (!aa || !bb)
        return std::nullopt;
    Legs difference;
    std::set_symmetric_difference(a.begin(), a.end(), b.begin(), b.end(),
                                  std::back_inserter(difference));
    if (difference.size() == a.size() + b.size()) {
        // Disjoint sums: 2 A.B = (A+B)^2 - A^2 - B^2.
        if (auto sum = knownSquare(algebra, difference, useInternal))
            return half(algebra, sub(algebra, sub(algebra, *sum, *aa), *bb));
    } else if (std::includes(a.begin(), a.end(), b.begin(), b.end()) ||
               std::includes(b.begin(), b.end(), a.begin(), a.end())) {
        // Nested sums: their difference, up to sign, is another subset.
        if (auto diff = knownSquare(algebra, difference, useInternal))
            return half(algebra, sub(algebra, algebra.add({*aa, *bb}), *diff));
    }
    return std::nullopt;
}

Id momentum(Algebra& algebra, const Legs& legs) {
    std::vector<Id> indices;
    for (int leg : legs)
        indices.push_back(algebra.integer(leg));
    Id label = indices.size() == 1 ? indices.front() :
               algebra.call("Multiparticle", std::move(indices));
    return algebra.call("Mom", {label});
}

std::optional<Legs> insertion(const Algebra& algebra, Id slot) {
    const auto& node = algebra.node(slot);
    if (node.head == "Mom" && node.args.size() == 1)
        return members(algebra, node.args.front());
    // Also recognize the expanded slot Mom[i] + Mom[j] + ... .
    // A repeated momentum is legal algebraically but is not a subset.
    if (node.head != "Plus")
        return std::nullopt;
    Legs sum;
    for (Id term : node.args) {
        const auto& mom = algebra.node(term);
        if (mom.head != "Mom" || mom.args.size() != 1)
            return std::nullopt;
        auto legs = members(algebra, mom.args.front());
        if (!legs)
            return std::nullopt;
        sum.insert(sum.end(), legs->begin(), legs->end());
    }
    std::sort(sum.begin(), sum.end());
    if (std::adjacent_find(sum.begin(), sum.end()) != sum.end())
        return std::nullopt;
    return sum;
}

// A term containing d numerator momenta has HEPCAT complexity 1 + 4d.
// For these local candidate comparisons count implicit branches of composite
// momentum insertions before expanding. Independent products obey the product
// rule on (term count, sum of degrees). Cancellations across distributed terms
// may lower the final Mathematica score; this is a search ranking only.
struct DegreeCount {
    std::size_t terms = 1;
    std::size_t degree = 0;
};

std::size_t boundedAdd(std::size_t a, std::size_t b) {
    constexpr auto limit = std::numeric_limits<std::size_t>::max();
    return a > limit - b ? limit : a + b;
}

std::size_t boundedMul(std::size_t a, std::size_t b) {
    constexpr auto limit = std::numeric_limits<std::size_t>::max();
    return b && a > limit / b ? limit : a * b;
}

DegreeCount multiplyCounts(DegreeCount a, DegreeCount b) {
    return {boundedMul(a.terms, b.terms),
            boundedAdd(boundedMul(a.degree, b.terms), boundedMul(b.degree, a.terms))};
}

class CandidateRank {
public:
    explicit CandidateRank(const Algebra& algebra) : algebra_(algebra) {}

    std::pair<std::size_t, std::size_t> operator()(Id id) {
        auto counts = count(id);
        return {boundedAdd(counts.terms, boundedMul(4, counts.degree)), algebra_.cost(id)};
    }
private:
    const Algebra& algebra_;
    std::unordered_map<Id, DegreeCount> cache_;

    DegreeCount count(Id id) {
        if (auto it = cache_.find(id); it != cache_.end())
            return it->second;
        const auto& n = algebra_.node(id);
        DegreeCount result;
        if (n.head == "0" && !n.compound) {
            result = {0, 0};
        } else if (n.head == "Plus") {
            result = {0, 0};
            for (Id child : n.args) {
                auto part = count(child);
                result.terms = boundedAdd(result.terms, part.terms);
                result.degree = boundedAdd(result.degree, part.degree);
            }
        } else if (n.head == "Times") {
            for (Id child : n.args)
                result = multiplyCounts(result, count(child));
        } else if (n.head == "Power" && n.args.size() == 2) {
            // All inverse and fractional powers are opaque to numerator ranking.
            if (int exponent = algebra_.leg(n.args[1])) {
                auto base = count(n.args[0]);
                while (exponent) {
                    if (exponent % 2)
                        result = multiplyCounts(result, base);
                    exponent /= 2;
                    if (exponent)
                        base = multiplyCounts(base, base);
                }
            }
        } else if (n.head == "SpinorTrace" || (n.head == "SpinorChain" && n.args.size() >= 2)) {
            std::size_t offset = n.head == "SpinorChain" ? 1 : 0;
            for (std::size_t j = offset; j < n.args.size() - offset; ++j) {
                auto labels = insertion(algebra_, n.args[j]);
                std::size_t width = labels ? labels->size() : 1;
                result = multiplyCounts(result, {width, width});
            }
        } else if (n.head == "MomProd" || n.head == "Mandelstahm") {
            result.degree = 2;
        } else if (n.head == "Mom") {
            result.degree = 1;
        }
        cache_.emplace(id, result);
        return result;
    }
};

Id reduceInsertion(Algebra& algebra, Id id, bool useInternal) {
    const auto& chain = algebra.node(id);
    if (chain.args.size() < 3)
        return id;
    Id best = id;
    CandidateRank rank(algebra);
    for (std::size_t slot = 1; slot + 1 < chain.args.size(); ++slot) {
        auto legs = insertion(algebra, chain.args[slot]);
        if (!legs || legs->empty())
            continue;
        if (legs->size() == algebra.masses().size())
            return algebra.integer(0);
        // Adjacent slashes give q^2 times the identity, even for a composite q.
        if (slot + 2 < chain.args.size() && insertion(algebra, chain.args[slot + 1]) == legs) {
            if (auto square = knownSquare(algebra, *legs, useInternal)) {
                auto args = chain.args;
                args.erase(args.begin() + slot, args.begin() + slot + 2);
                return algebra.mul({*square, reduceSpinor(algebra, algebra.call("SpinorChain", std::move(args)))});
            }
        }
        if (legs->size() < 2)
            continue;
        auto makeChain = [&](Id mom) {
            auto args = chain.args;
            args[slot] = mom;
            return reduceSpinor(algebra, algebra.call("SpinorChain", std::move(args)));
        };
        // Both representations are exact. Score after applying endpoint/Dirac
        // identities: a whole branch may vanish at a massless endpoint.
        auto select = [&](Id trial) {
            if (rank(trial) < rank(best))
                best = trial;
        };
        auto trial = [&](const Legs& terms, int sign) {
            Id factor = algebra.integer(sign);
            select(algebra.mul({factor, makeChain(momentum(algebra, terms))}));
            if (terms.size() == 1)
                return;
            std::vector<Id> expanded;
            for (int leg : terms)
                expanded.push_back(makeChain(momentum(algebra, {leg})));
            select(algebra.mul({factor, algebra.add(std::move(expanded))}));
        };
        trial(*legs, 1);
        trial(complement(algebra, *legs), -1);
    }
    return best;
}

// For four conserved momenta, [a|K P|a] = -[ab]<b|P|a], where a,b
// are null. The determinant of the two crossed P sandwiches splits into
// two diagonal sandwiches and P^2 times the bare brackets. If either
// P.a or P.b vanishes on this numerator's cut, the diagonal product is zero,
// leaving P^2 (K^2-P^2-2P.a-2P.b) [ab]. Angle parity obeys the same identity.
Id fourPointCutMinor(Algebra& a, Id product) {
    const auto& factors = a.node(product).args;
    if (std::count_if(a.masses().begin(), a.masses().end(),
                      [&](Id mass) { return a.node(mass).head == "0"; }) < 2) return product;
    auto nullEnd = [&](Id x) {
        const auto& n = a.node(x);
        if (n.head != "Spinor" || n.args.size() != 3 || a.node(n.args[0]).head != "\"Helicity\"") return 0;
        int i = a.leg(n.args[2]);
        return external(a, i) && a.node(a.masses()[i - 1]).head == "0" ? i : 0;
    };
    auto legOfMom = [&](Id x) {
        const auto& n = a.node(x);
        return n.head == "Mom" && n.args.size() == 1 ? a.leg(n.args[0]) : 0;
    };
    for (size_t i = 0; i < factors.size(); ++i) {
        const auto& wide = a.node(factors[i]);
        if (wide.head != "SpinorChain" || wide.args.size() != 4 || wide.args.front() != wide.args.back())
            continue;
        int legA = nullEnd(wide.args[0]), left = legOfMom(wide.args[1]), right = legOfMom(wide.args[2]);
        if (!legA || !external(a, left) || !external(a, right) || left == right || left == legA ||
            right == legA)
            continue;
        const Id endA = wide.args.front(), chirality = a.node(endA).args[1];
        for (size_t j = 0; j < factors.size(); ++j) {
            const auto& shortChain = a.node(factors[j]);
            if (shortChain.head != "SpinorChain" || shortChain.args.size() != 3) continue;
            int p = legOfMom(shortChain.args[1]);
            if (p != left && p != right) continue;
            Id b = shortChain.args.front(), opposite = shortChain.args.back();
            if (!nullEnd(b) || !nullEnd(opposite)) continue;
            if (a.node(b).args[1] != chirality) std::swap(b, opposite);
            int legB = nullEnd(b), other = p == right ? left : right;
            if (nullEnd(opposite) != legA || a.node(opposite).args[1] == chirality ||
                a.node(b).args[1] != chirality || legB == legA || legB == left || legB == right)
                continue;
            auto pa = knownDot(a, {p}, {legA}, true), pb = knownDot(a, {p}, {legB}, true);
            Id zero = a.integer(0);
            if (pa != zero && pb != zero) continue;
            auto dot = [&](int x) {
                return a.call("MomProd", {a.integer(std::min(p, x)), a.integer(std::max(p, x))});
            };
            auto mp2 = a.pow(a.masses()[p - 1], 2);
            auto scalar = a.add({a.pow(a.masses()[other - 1], 2), a.mul({a.integer(-1), mp2}),
                                 a.mul({a.integer(-2), pa.value_or(dot(legA))}),
                                 a.mul({a.integer(-2), pb.value_or(dot(legB))})});
            auto out = factors;
            out[i] = a.mul({a.integer(p == right ? 1 : -1), mp2, scalar,
                            reduceSpinor(a, a.call("SpinorChain", {endA, b}))});
            out[j] = a.integer(1);
            return a.mul(std::move(out));
        }
    }
    return product;
}

} // namespace

Id reduceKinematics(Algebra& algebra, Id id, bool useInternal) {
    const auto& n = algebra.node(id);
    if (n.head == "Times" && useInternal && algebra.masses().size() == 4 && !algebra.shells().empty())
        return fourPointCutMinor(algebra, id);
    if (n.head == "Mass" && n.args.size() == 1) {
        if (auto legs = members(algebra, n.args[0]))
            if (auto mass = directMass(algebra, *legs, useInternal))
                return *mass;
    } else if (n.head == "MomProd" && n.args.size() == 2) {
        auto a = members(algebra, n.args[0]);
        auto b = members(algebra, n.args[1]);
        if (a && b) {
            if (auto product = knownDot(algebra, *a, *b, useInternal))
                return *product;
            if (*b < *a)
                return algebra.call("MomProd", {n.args[1], n.args[0]});
        }
    } else if (n.head == "Power" && n.args.size() == 2 && algebra.leg(n.args[1]) == 2) {
        if (auto legs = insertion(algebra, n.args[0]))
            if (auto square = knownSquare(algebra, *legs, useInternal))
                return *square;
        const auto& base = algebra.node(n.args[0]);
        if (base.head == "Mass" && base.args.size() == 1)
            if (auto legs = members(algebra, base.args[0]))
                if (auto square = knownSquare(algebra, *legs, useInternal))
                    return *square;
    } else if (n.head == "Mandelstahm" && n.args.size() == 2) {
        int i = algebra.leg(n.args[0]), j = algebra.leg(n.args[1]);
        if (external(algebra, i) && external(algebra, j) && i != j) {
            if (auto square = knownSquare(algebra, {std::min(i, j), std::max(i, j)}, useInternal))
                return *square;
        }
    } else if (n.head == "SpinorChain") {
        return reduceInsertion(algebra, id, useInternal);
    }
    return id;
}

} // namespace hepcat
