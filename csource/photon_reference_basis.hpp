#pragma once
#include "reduce.hpp"
#include <algorithm>

namespace hepcat {
// Insert the two-spinor identity in a diagonal massless sandwich. For example,
// [i|p q|i] = (tau_pi <r|q|i] - tau_qi [i|p|r>) / <ri>.
// Each tau is 2 p.i or 2 q.i, expressed as an ordinary external virtuality.
// The same reference r is used throughout the current. Exchanging angle and
// square gives the other chirality. Only external mass shells enter.
inline std::optional<Id> photon_reference_basis(Algebra& a, Id current, const std::vector<int>& photons) {
    const Id helicity = a.atom("\"Helicity\""), square = a.atom("\"Square\""), angle = a.atom("\"Angle\"");
    auto spin = [&](Id chirality, int leg) {
        return a.call("Spinor", {helicity, chirality, a.integer(leg)});
    };
    auto virtuality = [&](int p, int i) {
        auto labels = std::minmax(p, i);
        auto external_sum = a.call("Multiparticle", {a.integer(labels.first), a.integer(labels.second)});
        return a.call("PropDen", {a.call("Mom", {external_sum}), a.masses()[p - 1]});
    };
    std::optional<Id> best;
    std::size_t best_score = 0;
    for (int reference : photons) {
        std::unordered_map<Id, Id> memo;
        bool expanded = false;
        auto project = [&](auto& self, Id id) -> Id {
            if (auto it = memo.find(id); it != memo.end())
                return it->second;
            const auto& n = a.node(id);
            if (!n.compound)
                return id;
            Id value = id;
            if (n.head == "SpinorChain" && n.args.size() == 4 && n.args.front() == n.args.back()) {
                const auto& endpoint = a.node(n.args.front());
                const auto& first = a.node(n.args[1]);
                const auto& second = a.node(n.args[2]);
                if (endpoint.head == "Spinor" && endpoint.args.size() == 3 && endpoint.args[0] == helicity &&
                    first.head == "Mom" && first.args.size() == 1 && second.head == "Mom" && second.args.size() == 1) {
                    int i = a.leg(endpoint.args[2]), p = a.leg(first.args[0]), q = a.leg(second.args[0]);
                    auto external = [&](int k) { return k > 0 && std::size_t(k) <= a.masses().size(); };
                    if (external(i) && external(p) && external(q) && i != reference && p != i && q != i &&
                        p != q && a.masses()[i - 1] == a.integer(0)) {
                        Id opposite = endpoint.args[1] == angle ? square : angle;
                        Id r = spin(opposite, reference);
                        Id den = a.call("SpinorChain", {r, spin(opposite, i)});
                        auto left = a.call("SpinorChain", {r, n.args[2], n.args.back()});
                        auto right = a.call("SpinorChain", {n.args.front(), n.args[1], r});
                        value = a.mul({a.pow(den, -1), a.add({a.mul({virtuality(p, i), left}),
                                            a.mul({a.integer(-1), virtuality(q, i), right})})});
                        expanded = true;
                    }
                }
            } else {
                auto args = n.args;
                for (auto& x : args)
                    x = self(self, x);
                if (args != n.args)
                    value = a.call(n.head, std::move(args));
            }
            return memo.emplace(id, value).first->second;
        };
        auto candidate = project(project, current);
        if (!expanded)
            continue;
        auto score = expression_complexity(a, candidate);
        if (!best || score < best_score) {
            best = candidate;
            best_score = score;
        }
    }
    return best;
}
} // namespace hepcat
