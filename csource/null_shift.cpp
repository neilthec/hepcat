// Continue a sewn numerator by shifting two external momenta onto its cut.
// At least one shifted leg is massless. The physical channels constrain any
// boundary term left by the shift.
#include "continuations.hpp"

#include <algorithm>
#include <map>
#include <set>
#include <stdexcept>

namespace hepcat {
namespace {
struct unavailable {};

class null_shift {
    Algebra& a;
    int square_leg, angle_leg, massive_leg;
    Id zero, one, minus_one, angle, square, left, right, root;
    std::vector<int> selected;
    std::map<Id, Id> memo;
    std::map<Id, std::set<int>> excluded;
    using choice = std::pair<Id, Id>;

    Id h(Id kind, int leg) { return a.call("Spinor", {a.atom("\"Helicity\""), kind, a.integer(leg)}); }
    bool is_spinor(Id x, const char* type) const {
        const auto& v = a.node(x);
        return v.head == "Spinor" && v.args.size() >= 3 && a.node(v.args[0]).head == type;
    }
    Id kind(Id x) const {
        const auto& n = a.node(x);
        if (n.head != "Spinor") throw unavailable{};
        for (Id p : n.args)
            if (p == angle || p == square) return p;
        throw unavailable{};
    }
    void gather_references(Id x, std::set<Id>& visited) {
        if (!visited.insert(x).second) return;
        const auto& v = a.node(x);
        if (v.head == "Power" && v.args.size() == 2) {
            mpq_class q;
            const auto& c = a.node(v.args[0]);
            if (a.number(v.args[1], q) && q < 0 && c.head == "SpinorChain" && c.args.size() == 2)
                for (unsigned i = 0; i < 2; ++i)
                    if (is_spinor(c.args[i], "\"Xi\"") && is_spinor(c.args[1 - i], "\"Helicity\""))
                        excluded[c.args[i]].insert(a.leg(a.node(c.args[1 - i]).args[2]));
        }
        for (Id p : v.args)
            gather_references(p, visited);
    }
    std::vector<int> legs(Id label) const {
        const auto& v = a.node(label);
        if (v.head == "Mom" && v.args.size() == 1) return legs(v.args[0]);
        if (int l = a.leg(label); l > 0 && unsigned(l) <= a.masses().size()) return {l};
        if (v.head != "Multiparticle") throw unavailable{};
        std::vector<int> result;
        for (Id p : v.args) {
            int l = a.leg(p);
            if (l < 1 || unsigned(l) > a.masses().size()) throw unavailable{};
            result.push_back(l);
        }
        return result;
    }
    int eta(const std::vector<int>& ls) const {
        return std::count(ls.begin(), ls.end(), square_leg) - std::count(ls.begin(), ls.end(), angle_leg);
    }
    Id slope(const std::vector<int>& ls) {
        std::vector<Id> terms;
        for (int leg : ls)
            if (leg != square_leg && leg != angle_leg)
                terms.push_back(chain({left, a.call("Mom", {a.integer(leg)}), right}));
        return a.add(std::move(terms));
    }
    std::vector<choice> endpoint(Id p) {
        if (is_spinor(p, "\"Xi\"")) {
            if (massive_leg) throw unavailable{};
            auto& blocked = excluded[p];
            std::vector<int> choices = kind(p) == square ? std::vector<int>{angle_leg, square_leg}
                                                         : std::vector<int>{square_leg, angle_leg};
            for (std::size_t i = 0; i < a.masses().size(); ++i)
                if (a.masses()[i] == zero) choices.push_back(static_cast<int>(i + 1));
            auto it = std::find_if(choices.begin(), choices.end(), [&](int l) { return !blocked.count(l); });
            if (it == choices.end()) throw unavailable{};
            p = h(kind(p), *it);
        }
        if (is_spinor(p, "\"Helicity\"")) {
            if (a.node(p).args.size() != 3 || a.leg(a.node(p).args[2]) <= 0) throw unavailable{};
            if (p == h(square, square_leg)) return {{one, p}, {root, right}};
            if (p == h(angle, angle_leg)) return {{one, p}, {a.mul({minus_one, root}), left}};
        } else if (is_spinor(p, "\"Spin\"")) {
            // A rank-one Lorentz transformation on the massive leg
            // transports its external spinors along with its momentum.
            // It acts on only one chirality and has unit determinant.
            const auto& spin = a.node(p);
            if (massive_leg && spin.args.size() == 3 && a.leg(spin.args[2]) == massive_leg) {
                const bool angle_moves = massive_leg == angle_leg;
                if (kind(p) == (angle_moves ? angle : square)) {
                    Id basis = angle_moves ? left : right;
                    Id scale = a.mul({angle_moves ? minus_one : one, root, chain({basis, p})});
                    return {{one, p}, {scale, basis}};
                }
            }
        } else {
            throw unavailable{};
        }
        return {{one, p}};
    }
    Id chain(const std::vector<Id>& p) {
        if (p.size() == 2 && p.front() == p.back()) return zero;
        if (!massive_leg) return a.call("SpinorChain", p);
        // q is rank one. Its auxiliary endpoint is p_s acting on the
        // unshifted spinor of the light leg. Transposition at the right
        // endpoint contributes the same sign as a momentum reference.
        const Id auxiliary = massive_leg == square_leg ? left : right;
        const Id light = massive_leg == square_leg ? right : left;
        const Id momentum = a.call("Mom", {a.integer(massive_leg)});
        auto word = p;
        if (word.front() == auxiliary) {
            word.front() = light;
            word.insert(word.begin() + 1, momentum);
        }
        const bool reverse = word.back() == auxiliary;
        if (reverse) {
            word.back() = momentum;
            word.push_back(light);
        }
        Id value = a.call("SpinorChain", std::move(word));
        return reverse ? a.mul({minus_one, value}) : value;
    }
    Id shifted_chain(const std::vector<Id>& w) {
        if (w.size() < 2) throw unavailable{};
        std::vector<Id> sum;
        // Each rank-one insertion splits the current chain into two brackets.
        for (auto l : endpoint(w.front()))
            for (auto r : endpoint(w.back())) {
                std::vector<std::pair<Id, std::vector<Id>>> terms{{a.mul({l.first, r.first}), {l.second}}};
                for (std::size_t i = 1; i + 1 < w.size(); ++i) {
                    if (a.node(w[i]).head != "Mom") throw unavailable{};
                    int sign = eta(legs(w[i]));
                    auto size = terms.size();
                    if (sign != 0 && size >= 256) throw unavailable{};
                    for (std::size_t j = 0; j < size; ++j) {
                        auto& p = terms[j].second;
                        if (sign != 0) {
                            bool ang = (kind(p.front()) == angle) == (p.size() % 2 == 1);
                            auto first = p;
                            first.push_back(ang ? left : right);
                            Id coeff = a.mul({terms[j].first, root, a.integer(sign), chain(first)});
                            p.push_back(w[i]);
                            terms.push_back({coeff, {ang ? right : left}});
                        } else
                            p.push_back(w[i]);
                    }
                }
                for (auto& term : terms) {
                    term.second.push_back(r.second);
                    sum.push_back(a.mul({term.first, chain(term.second)}));
                }
            }
        return a.add(std::move(sum));
    }
    Id transform(Id x) {
        if (auto it = memo.find(x); it != memo.end()) return it->second;
        const auto& v = a.node(x);
        Id result = x;
        if (v.head == "SpinorChain")
            result = shifted_chain(v.args);
        else if (v.head == "PropDen" && v.args.size() == 2) {
            auto ls = legs(v.args[0]);
            result = a.add({x, a.mul({root, a.integer(eta(ls)), slope(ls)})});
        } else if (v.head == "MomProd" && v.args.size() == 2) {
            auto i = legs(v.args[0]), j = legs(v.args[1]);
            result = a.add(
                {x,
                 a.mul({a.exact(mpq_class(1, 2)), root,
                        a.add({a.mul({a.integer(eta(i)), slope(j)}), a.mul({a.integer(eta(j)), slope(i)})})})});
        } else if (v.head == "Mandelstahm") {
            std::vector<int> all;
            for (Id p : v.args) {
                auto ls = legs(p);
                all.insert(all.end(), ls.begin(), ls.end());
            }
            result = a.add({x, a.mul({root, a.integer(eta(all)), slope(all)})});
        } else if (v.head == "Plus" || v.head == "Times" || v.head == "Power") {
            std::vector<Id> args;
            for (Id p : v.args)
                args.push_back(transform(p));
            if (v.head == "Power" && args[0] == zero) {
                mpq_class q;
                if (!a.number(args[1], q) || q < 0) throw unavailable{};
            }
            result = a.call(v.head, std::move(args));
        } else if (v.compound && v.head != "ModelParameter" && v.head != "Rational" &&
                   v.head != "System`Complex")
            throw unavailable{};
        memo.emplace(x, result);
        return result;
    }
    bool own_prop(Id x, Id mass) const {
        const auto& v = a.node(x);
        if (v.head != "PropDen" || v.args.size() != 2 || v.args[1] != mass) return false;
        auto ls = legs(v.args[0]);
        std::sort(ls.begin(), ls.end());
        if (ls == selected) return true;
        std::vector<int> other;
        for (int i = 1; i <= int(a.masses().size()); ++i)
            if (!std::binary_search(ls.begin(), ls.end(), i)) other.push_back(i);
        return other == selected;
    }
    Id numerator(Id x, Id mass) {
        const auto& v = a.node(x);
        if (v.head == "Plus") {
            std::vector<Id> terms;
            for (Id p : v.args)
                terms.push_back(numerator(p, mass));
            return a.add(std::move(terms));
        }
        auto factors = v.head == "Times" ? v.args : std::vector<Id>{x};
        for (auto p = factors.begin(); p != factors.end(); ++p) {
            const auto& inv = a.node(*p);
            if (inv.head == "Power" && inv.args.size() == 2 && inv.args[1] == minus_one &&
                own_prop(inv.args[0], mass)) {
                factors.erase(p);
                return a.mul(std::move(factors));
            }
        }
        throw unavailable{};
    }

  public:
    null_shift(Algebra& tree, int i, int j, int massive = 0)
        : a(tree), square_leg(i), angle_leg(j), massive_leg(massive), zero(a.integer(0)), one(a.integer(1)), minus_one(a.integer(-1)),
          angle(a.atom("\"Angle\"")), square(a.atom("\"Square\"")), left(h(angle, i)), right(h(square, j)),
          root(zero) {}
    Id lift(const numerator_shell& channel, Id sewn) {
        selected = channel.legs;
        std::sort(selected.begin(), selected.end());
        auto sign = eta(selected);
        if (!sign) throw unavailable{};
        auto derivative = a.mul({a.integer(sign), slope(selected)});
        if (derivative == zero) throw unavailable{};
        std::vector<Id> labels;
        for (int leg : selected)
            labels.push_back(a.integer(leg));
        Id mass = a.parse(channel.mass);
        Id prop = a.call("PropDen", {a.call("Mom", {a.call("Multiparticle", labels)}), mass});
        root = a.mul({minus_one, prop, a.pow(derivative, -1)});
        memo.clear();
        excluded.clear();
        auto state = numerator(sewn, mass);
        std::set<Id> visited;
        gather_references(state, visited);
        return a.mul({a.pow(prop, -1), transform(state)});
    }
};
} // namespace

std::vector<Id> shifted_channel_continuations(Algebra& a,
                                              const std::vector<std::pair<numerator_shell, Id>>& sources,
                                              int square_leg, int angle_leg) {
    if (square_leg == angle_leg || square_leg < 1 || angle_leg < 1 ||
        unsigned(std::max(square_leg, angle_leg)) > a.masses().size() ||
        a.masses()[square_leg - 1] != a.integer(0) || a.masses()[angle_leg - 1] != a.integer(0))
        return {};
    null_shift shift(a, square_leg, angle_leg);
    std::vector<Id> result;
    for (const auto& source : sources) {
        try {
            Id value = shift.lift(source.first, source.second);
            if (value != a.integer(0)) result.push_back(value);
        } catch (const unavailable&) { /* Try another supported channel. */
        }
    }
    return result;
}

std::vector<Id> massive_shifted_channel_continuations(Algebra& a,
        const std::vector<std::pair<numerator_shell, Id>>& sources,
        int light, int massive, bool shift_square) {
    if (light < 1 || massive < 1 || unsigned(std::max(light, massive)) > a.masses().size() ||
        a.masses()[light - 1] != a.integer(0) || a.masses()[massive - 1] == a.integer(0)) return {};
    null_shift shift(a, shift_square ? light : massive, shift_square ? massive : light, massive);
    std::vector<Id> pieces;
    for (const auto& [cut, product] : sources) {
        auto includes = [&](int l) {
            return std::find(cut.legs.begin(), cut.legs.end(), l) != cut.legs.end();
        };
        if (product == a.integer(0) || includes(light) == includes(massive)) continue;
        try {
            pieces.push_back(shift.lift(cut, product));
        } catch (const unavailable&) {
            return {}; // All moving, nonzero channels are required for a contour sum.
        }
    }
    return pieces;
}
} // namespace hepcat
