#include "continuations.hpp"

#include <algorithm>
#include <functional>
#include <map>
#include <set>
#include <utility>

namespace hepcat {
namespace {
bool spinor(const Algebra& a, Id x, const char* type) {
    const auto& n = a.node(x);
    return n.head == "Spinor" && n.args.size() >= 3 && a.node(n.args.front()).head == type;
}

std::vector<int> cut_key(std::vector<int> legs, int n) {
    std::sort(legs.begin(), legs.end());
    std::vector<int> complement;
    for (int i = 1; i <= n; ++i)
        if (!std::binary_search(legs.begin(), legs.end(), i)) complement.push_back(i);
    return std::min(legs, complement);
}

struct reference {
    int emitter, target;
    bool physical;
};
using assignments = std::map<Id, reference>;

Id replace_references(Algebra& a, Id root, const assignments& choices) {
    std::map<Id, Id> memo;
    auto helicity = [&](Id kind, int leg) {
        return a.call("Spinor", {a.atom("\"Helicity\""), kind, a.integer(leg)});
    };
    auto momentum = [&](int leg) { return a.call("Mom", {a.integer(leg)}); };
    std::function<Id(Id)> visit = [&](Id x) -> Id {
        if (auto it = memo.find(x); it != memo.end()) return it->second;
        const auto& n = a.node(x);
        Id result = x;
        if (n.head == "SpinorChain" && n.args.size() >= 2) {
            std::vector<Id> parts;
            bool negative = false;
            for (std::size_t i = 0; i < n.args.size(); ++i) {
                Id p = n.args[i];
                auto it = choices.find(p);
                if (it == choices.end()) {
                    parts.push_back(p);
                    continue;
                }
                bool left = i == 0;
                const auto& ref = it->second;
                Id kind = a.node(p).args[1];
                if (!ref.physical) {
                    parts.push_back(helicity(kind, ref.target));
                    continue;
                }
                Id opposite = a.atom(a.node(kind).head == "\"Angle\"" ? "\"Square\"" : "\"Angle\"");
                if (left) {
                    parts.push_back(helicity(opposite, ref.emitter));
                    parts.push_back(momentum(ref.target));
                } else {
                    parts.push_back(momentum(ref.target));
                    parts.push_back(helicity(opposite, ref.emitter));
                    negative = !negative;
                }
            }
            result = a.call(n.head, std::move(parts));
            if (negative) result = a.mul({a.integer(-1), result});
        } else if (n.compound && n.head != "PropDen" && n.head != "Spinor") {
            // In a polarization denominator the two extra spinors make a
            // scalar virtuality. The right-end reference has the crossing sign.
            mpq_class power;
            if (n.head == "Power" && n.args.size() == 2 && a.number(n.args[1], power) && power < 0) {
                const auto& chain = a.node(n.args[0]);
                if (chain.head == "SpinorChain" && chain.args.size() == 2) {
                    for (unsigned side = 0; side != 2; ++side) {
                        auto it = choices.find(chain.args[side]);
                        if (it == choices.end() || !it->second.physical) continue;
                        auto r = it->second;
                        Id other = chain.args[1 - side];
                        if (!spinor(a, other, "\"Helicity\"") || a.leg(a.node(other).args[2]) != r.emitter ||
                            a.node(other).args[1] != a.node(chain.args[side]).args[1])
                            continue;
                        auto pair = std::minmax(r.emitter, r.target);
                        Id k = a.call(
                            "Mom", {a.call("Multiparticle", {a.integer(pair.first), a.integer(pair.second)})});
                        Id d = a.call("PropDen", {k, a.masses().at(r.target - 1)});
                        if (side == 1) d = a.mul({a.integer(-1), d});
                        result = a.call("Power", {d, n.args[1]});
                        memo.emplace(x, result);
                        return result;
                    }
                }
            }
            std::vector<Id> args;
            for (Id p : n.args)
                args.push_back(visit(p));
            result = a.call(n.head, std::move(args));
        }
        memo.emplace(x, result);
        return result;
    };
    return visit(root);
}
} // namespace

std::vector<Id> channel_continuations(Algebra& a, Id expr, const numerator_shell& selected,
                                      const std::vector<numerator_shell>& inventory) {
    const int n = static_cast<int>(a.masses().size());
    std::map<Id, std::set<int>> emitters;
    std::set<Id> seen;
    std::function<void(Id)> scan = [&](Id x) {
        if (!seen.insert(x).second) return;
        const auto& v = a.node(x);
        if (spinor(a, x, "\"Xi\"")) emitters[x];
        mpq_class power;
        if (v.head == "Power" && v.args.size() == 2 && a.number(v.args[1], power) && power < 0) {
            const auto& c = a.node(v.args[0]);
            if (c.head == "SpinorChain" && c.args.size() == 2)
                for (unsigned side = 0; side != 2; ++side) {
                    Id ref = c.args[side], end = c.args[1 - side];
                    if (spinor(a, ref, "\"Xi\"") && spinor(a, end, "\"Helicity\"") &&
                        a.node(ref).args[1] == a.node(end).args[1])
                        emitters[ref].insert(a.leg(a.node(end).args[2]));
                }
        }
        for (Id p : v.args)
            scan(p);
    };
    scan(expr);
    if (emitters.empty()) return {};
    using pole = std::pair<std::vector<int>, Id>;
    const pole own{cut_key(selected.legs, n), a.parse(selected.mass)};
    std::set<pole> physical;
    for (const auto& s : inventory)
        physical.emplace(cut_key(s.legs, n), a.parse(s.mass));
    std::map<Id, std::vector<reference>> groups;
    Id zero = a.integer(0);
    for (const auto& entry : emitters) {
        if (entry.second.size() != 1) return {};
        int leg = *entry.second.begin();
        const auto& chirality = a.node(a.node(entry.first).args[1]).head;
        if (leg < 1 || leg > n || a.masses()[leg - 1] != zero ||
            (chirality != "\"Square\"" && chirality != "\"Angle\""))
            return {};
        auto& choices = groups[entry.first];
        for (bool phys : {true, false})
            for (int target = 1; target <= n; ++target) {
                if (target == leg) continue;
                pole pair{cut_key({leg, target}, n), a.masses()[target - 1]};
                if (pair == own) continue;
                if (phys ? physical.count(pair) != 0 : pair.second == zero)
                    choices.push_back({leg, target, phys});
            }
        if (choices.empty()) return {};
    }
    // Cycle the admissible targets in both gauges. Limit the proposal count
    // independently of the number of external polarizations.
    std::vector<Id> result;
    for (bool physical_first : {true, false})
        for (unsigned offset = 0; offset < 4; ++offset) {
            assignments choice;
            for (const auto& entry : groups) {
                std::vector<reference> preferred;
                for (auto r : entry.second)
                    if (r.physical == physical_first) preferred.push_back(r);
                const auto& options = preferred.empty() ? entry.second : preferred;
                choice.emplace(entry.first, options[offset % options.size()]);
            }
            Id value = replace_references(a, expr, choice);
            if (value != expr && std::find(result.begin(), result.end(), value) == result.end())
                result.push_back(value);
        }
    return result;
}
} // namespace hepcat
