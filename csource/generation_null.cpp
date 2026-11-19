#include "generation_spin.hpp"
#include <algorithm>
#include <array>
#include <limits>
#include <map>
#include <optional>
#include <stdexcept>

namespace hepcat::generation {
namespace {
using word = std::vector<Id>;
class null_sum {
    Algebra& a;
    Id label, ang, sq, hel, xi;
    std::unordered_map<Id, bool> dependency;
    const std::vector<int>& null;
    bool replace;
    int port(Id id) const {
        auto& n = a.node(id);
        if (n.head != "Spinor" || n.args.size() != 3 || n.args[0] != hel || n.args[2] != label) return 0;
        return n.args[1] == ang ? 1 : -1;
    }
    bool depends(Id id) {
        if (auto it = dependency.find(id); it != dependency.end()) return it->second;
        auto& n = a.node(id);
        bool yes = port(id) || std::any_of(n.args.begin(), n.args.end(), [&](Id x) { return depends(x); });
        return dependency.emplace(id, yes).first->second;
    }
    std::optional<long> exponent(Id x) const {
        mpq_class q;
        if (!a.number(x, q) || q.get_den() != 1 || !q.get_num().fits_slong_p()) return {};
        long k = q.get_num().get_si();
        if (k < -128 || k > 128) return {};
        return k;
    }
    long weight(Id id) {
        if (!depends(id)) return 0;
        if (int p = port(id)) return p;
        auto& n = a.node(id);
        if (n.head == "Power") {
            auto k = exponent(n.args[1]);
            if (k) return *k * weight(n.args[0]);
        } else if (n.head == "SpinorChain" || n.head == "Times") {
            long w = 0;
            for (Id x : n.args)
                w += weight(x);
            return w;
        } else if (n.head == "Plus") {
            long w = weight(n.args[0]);
            for (Id x : n.args)
                if (weight(x) != w)
                    throw std::invalid_argument("inhomogeneous helicity on an internal null line");
            return w;
        }
        throw std::invalid_argument("unsupported internal null spin representation: " + n.head);
    }
    // Convert equal powers <a Q> [Q b] into <a|Q|b]. The same rule applies
    // in the denominator when both factors are inverse powers.
    std::optional<Id> polynomial(Id id) {
        auto& n = a.node(id);
        if (n.head == "Plus") {
            word terms;
            for (Id child : n.args) {
                auto x = polynomial(child);
                if (!x) return {};
                terms.push_back(*x);
            }
            return a.add(std::move(terms));
        }
        word independent;
        struct bracket {
            word w;
            int chirality;
            bool left;
            long exponent;
        };
        std::vector<bracket> brackets;
        std::vector<std::pair<Id, long>> factors{{id, 1}};
        while (!factors.empty()) {
            auto [z, k] = factors.back();
            factors.pop_back();
            if (!depends(z)) {
                independent.push_back(a.pow(z, k));
                continue;
            }
            if (a.node(z).head == "Times") {
                for (Id child : a.node(z).args)
                    factors.emplace_back(child, k);
                continue;
            }
            if (a.node(z).head == "Power") {
                const auto& power = a.node(z).args;
                auto value = exponent(power[1]);
                if (!value) return {};
                if (*value != 0 && std::abs(k) > 128 / std::abs(*value)) return {};
                factors.emplace_back(power[0], k * (*value));
                continue;
            }
            auto& term = a.node(z);
            if (term.head != "SpinorChain" || term.args.size() < 2) return {};
            int l = port(term.args.front()), r = port(term.args.back());
            if ((l != 0) == (r != 0)) return {};
            brackets.push_back({term.args, l ? l : r, l != 0, k});
        }
        // Direct ket-bra joins take priority over joins that reverse a word.
        // This also fixes the continuation when several identical powers can
        // be paired on a rank-one massless pole in more than one way.
        for (bool reverse : {false, true})
            for (size_t i = 0; i < brackets.size(); ++i) {
                auto& l = brackets[i];
                for (size_t j = i + 1; j < brackets.size() && l.exponent != 0; ++j) {
                    auto& r = brackets[j];
                    if ((l.left == r.left) != reverse || l.chirality == r.chirality ||
                        (l.exponent > 0) != (r.exponent > 0) || !r.exponent)
                        continue;
                    long k =
                        (l.exponent > 0 ? 1 : -1) * std::min(std::abs(l.exponent), std::abs(r.exponent));
                    word left = l.w, right = r.w;
                    int sign = 1;
                    if (l.left) {
                        std::reverse(left.begin(), left.end());
                        if (left.size() % 2 == 0) sign = -sign;
                    }
                    if (!r.left) {
                        std::reverse(right.begin(), right.end());
                        if (right.size() % 2 == 0) sign = -sign;
                    }
                    left.back() = a.call("Mom", {label});
                    left.insert(left.end(), right.begin() + 1, right.end());
                    independent.push_back(a.pow(a.call("SpinorChain", std::move(left)), k));
                    if (sign < 0 && k % 2) independent.push_back(a.integer(-1));
                    l.exponent -= k;
                    r.exponent -= k;
                }
            }
        if (std::any_of(brackets.begin(), brackets.end(), [](const auto& b) { return b.exponent != 0; }))
            return {};
        return a.mul(std::move(independent));
    }
    // Choose generic reference spinors. If explicit references were replaced,
    // use linear combinations of the available external null spinors. Their
    // normalization drops out of the complete little-group-neutral residue.
    word references(bool angle) {
        word result;
        if (replace && !null.empty())
            for (int leg : null)
                result.push_back(a.call("Spinor", {hel, angle ? ang : sq, a.integer(leg)}));
        else {
            long tag = 2000000;
            for (Id leg : a.node(label).args)
                tag = (tag * 31 + a.leg(leg)) % 1000000000;
            result.push_back(a.call("Spinor", {xi, angle ? ang : sq, a.integer(tag)}));
        }
        return result;
    }

  public:
    null_sum(Algebra& ar, Id line, const std::vector<int>& light, bool change)
        : a(ar), label(line), ang(a.atom("\"Angle\"")), sq(a.atom("\"Square\"")),
          hel(a.atom("\"Helicity\"")), xi(a.atom("\"Xi\"")), null(light), replace(change) {}
    Id apply(Id expr) {
        if (!depends(expr)) return expr;
        if (weight(expr) != 0)
            throw std::invalid_argument("nonzero little-group weight on a sewn massless edge");
        if (auto result = polynomial(expr)) return *result;
        auto lref = references(true), rref = references(false);
        Id mom = a.call("Mom", {label});
        word pieces;
        for (Id l : lref)
            for (Id r : rref)
                pieces.push_back(a.call("SpinorChain", {l, mom, r}));
        Id norm = a.add(std::move(pieces));
        std::unordered_map<Id, Id> cache;
        auto rewrite = [&](auto& self, Id id) -> Id {
            if (!depends(id)) return id;
            if (auto it = cache.find(id); it != cache.end()) return it->second;
            const auto& n = a.node(id);
            word args;
            Id out;
            if (n.head == "SpinorChain") {
                if (n.args.size() < 2) throw std::invalid_argument("invalid spinor chain");
                int first = port(n.args.front()), last = port(n.args.back());
                auto left = first ? (first == 1 ? rref : lref) : word{n.args.front()};
                auto right = last ? (last == 1 ? rref : lref) : word{n.args.back()};
                word variants;
                for (Id l : left)
                    for (Id r : right) {
                        word chain{l};
                        if (first) chain.push_back(mom);
                        chain.insert(chain.end(), n.args.begin() + 1, n.args.end() - 1);
                        if (last) chain.push_back(mom);
                        chain.push_back(r);
                        variants.push_back(a.call("SpinorChain", std::move(chain)));
                    }
                out = a.mul({a.integer((first == 1) != (last == -1) ? -1 : 1),
                             a.pow(norm, -int(first < 0) - int(last < 0)), a.add(std::move(variants))});
            } else {
                for (Id child : n.args)
                    args.push_back(self(self, child));
                out = a.call(n.head, std::move(args));
            }
            cache.emplace(id, out);
            return out;
        };
        return rewrite(rewrite, expr);
    }
};
} // namespace
Id sew_null_spin(Algebra& a, Id expression, Id internal, const std::vector<int>& light, bool refs) {
    return null_sum(a, internal, light, refs).apply(expression);
}
} // namespace hepcat::generation

namespace hepcat::generation {
namespace {
class four_null {
    Algebra& a;
    Id angle, square, helicity, scale;
    std::unordered_map<Id, Id> reduced, normalized;
    std::pair<int, int> ends(Id id) {
        const auto& s = a.node(id);
        if (s.head != "Spinor" || s.args.size() != 3 || s.args[0] != helicity) return {};
        return {a.leg(s.args[2]), s.args[1] == angle ? 1 : -1};
    }
    Id bracket(int i, int j, int chirality) {
        if (i == j) return a.integer(0);
        auto sp = [&](int k) {
            return a.call("Spinor", {helicity, chirality == 1 ? angle : square, a.integer(k)});
        };
        return a.mul(
            {a.integer(i > j ? -1 : 1), a.call("SpinorChain", {sp(std::min(i, j)), sp(std::max(i, j))})});
    }
    Id raised(Id value, long k) {
        const Node& n = a.node(value);
        if (n.head != "Times") return a.pow(value, k);
        std::vector<Id> parts;
        for (Id f : n.args)
            parts.push_back(a.pow(f, k));
        return a.mul(std::move(parts));
    }
    Id rewrite(Id id, bool for_key) {
        auto& cache = for_key ? normalized : reduced;
        if (auto it = cache.find(id); it != cache.end()) return it->second;
        auto& n = a.node(id);
        Id out = id;
        if (n.head == "SpinorChain" && n.args.size() >= 2) {
            auto [i, ic] = ends(n.args.front());
            auto [j, jc] = ends(n.args.back());
            if (!i || !j || i > 4 || j > 4) return id;
            if (n.args.size() == 2 && ic == jc) {
                if (!for_key || ic == 1 || i == j)
                    out = bracket(i, j, ic);
                else {
                    // For four null conserved momenta, [ij] = rho * eps_ijkl * <kl>
                    // for one common little-group scale rho and ordered k<l.
                    std::array<int, 4> seq{i, j, 0, 0};
                    int pos = 2;
                    for (int v = 1; v <= 4; ++v)
                        if (v != i && v != j) seq[pos++] = v;
                    int sign = 1;
                    for (int m = 0; m < 4; ++m)
                        for (int k = 0; k < m; ++k)
                            if (seq[k] > seq[m]) sign = -sign;
                    out = a.mul({a.integer(sign), scale, bracket(seq[2], seq[3], 1)});
                }
            } else if (n.args.size() == 3 && ic == -jc && !for_key) {
                const auto& p = a.node(n.args[1]);
                if (p.head != "Mom" || p.args.size() != 1) return id;
                const auto& set = a.node(p.args[0]);
                if (set.head != "Multiparticle" || set.args.size() != 2) return id;
                std::vector<int> pair{a.leg(set.args[0]), a.leg(set.args[1])}, complement;
                for (int k = 1; k <= 4; ++k)
                    if (std::find(pair.begin(), pair.end(), k) == pair.end()) complement.push_back(k);
                if (complement.size() != 2) return id;
                auto score = [initial = i, final = j](const auto& v) {
                    return std::count(v.begin(), v.end(), initial) + std::count(v.begin(), v.end(), final);
                };
                int sign = 1;
                if (score(complement) > score(pair)) {
                    pair.swap(complement);
                    sign = -1;
                }
                word pieces;
                for (int k : pair)
                    pieces.push_back(a.mul({bracket(i, k, ic), bracket(k, j, jc)}));
                out = a.mul({a.integer(sign), a.add(std::move(pieces))});
            }
        } else if (n.head == "Times" || n.head == "Plus") {
            word args;
            for (Id x : n.args)
                args.push_back(rewrite(x, for_key));
            out = a.call(n.head, std::move(args));
        } else if (n.head == "Power" && n.args.size() == 2) {
            mpq_class q;
            auto base = rewrite(n.args[0], for_key);
            if (a.number(n.args[1], q) && q.get_den() == 1 && q.get_num().fits_slong_p())
                out = raised(base, q.get_num().get_si());
            else
                out = a.call(n.head, {base, n.args[1]});
        } else if (for_key && n.head == "ConstructiveDiagrams`ColorStructure") {
            auto args = n.args;
            int sign = 1;
            for (size_t j = 0; j < args.size(); ++j)
                for (size_t i = 0; i < j; ++i) {
                    if (args[i] == args[j]) return a.integer(0);
                    if (args[i] > args[j]) sign = -sign;
                }
            std::sort(args.begin(), args.end());
            out = a.mul({a.integer(sign), a.call(n.head, std::move(args))});
        }
        cache.emplace(id, out);
        return out;
    }

  public:
    explicit four_null(Algebra& ar)
        : a(ar), angle(a.atom("\"Angle\"")), square(a.atom("\"Square\"")), helicity(a.atom("\"Helicity\"")),
          scale(a.atom("NativePrivate`fourNullBracketScale")) {}
    std::optional<Id> common(Id l, Id r) {
        Id x = rewrite(l, false), y = rewrite(r, false);
        if (rewrite(x, true) == rewrite(y, true)) return a.cost(x) <= a.cost(y) ? x : y;
        return {};
    }
};
} // namespace
std::optional<Id> common_four_null_continuation(Algebra& a, Id x, Id y) {
    return four_null(a).common(x, y);
}
} // namespace hepcat::generation
