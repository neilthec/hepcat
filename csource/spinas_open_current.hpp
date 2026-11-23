// Recover a common open Weyl spinor before contracting it with different probes.
#pragma once
#include "reduce.hpp"
#include <algorithm>
#include <map>
#include <set>

namespace hepcat::spinas_detail {
struct current_term {
    Id chain;
    std::vector<Id> weights;
};
struct open_current {
    bool probe_on_left;
    Id probe;
    std::vector<current_term> terms;
};
using current_catalog = std::map<Id, open_current>;

inline current_catalog find_open_currents(const Algebra& tree, Id root, size_t minimum_arithmetic = 0) {
    using key = std::vector<std::vector<Id>>;
    using endpoint = std::pair<Id, bool>;
    struct occurrence { Id sum; open_current current; };
    std::map<key, std::vector<occurrence>> groups;
    std::vector<Id> pending{root};
    std::set<Id> seen;
    size_t arithmetic = 0;
    auto named = [](const Node& n, const char* name) {
        return n.head.compare(n.head.rfind('`') + 1, std::string::npos, name) == 0;
    };
    while (!pending.empty()) {
        auto id = pending.back();
        pending.pop_back();
        if (!seen.insert(id).second) continue;
        const auto& sum = tree.node(id);
        pending.insert(pending.end(), sum.args.begin(), sum.args.end());
        const bool plus = named(sum, "Plus");
        if (plus || named(sum, "Times") || named(sum, "Power")) ++arithmetic;
        if (!plus || sum.args.size() < 3) continue;
        std::map<endpoint, std::vector<std::pair<std::vector<Id>, current_term>>> candidates;
        for (size_t i = 0; i < sum.args.size(); ++i) {
            const auto& term = tree.node(sum.args[i]);
            const auto factors = named(term, "Times") ? term.args : std::vector<Id>{sum.args[i]};
            std::set<endpoint> present;
            for (size_t j = 0; j < factors.size(); ++j) {
                const auto& chain = tree.node(factors[j]);
                if (!named(chain, "SpinorChain") || chain.args.size() < 2) continue;
                for (bool left : {false, true}) {
                    endpoint probe{left ? chain.args.front() : chain.args.back(), left};
                    if ((i && !candidates.count(probe)) || !present.insert(probe).second) continue;
                    auto weights = factors;
                    weights.erase(weights.begin() + j);
                    // Length-delimit the weights, then include the orientation and
                    // every retained endpoint and insertion in its original order.
                    std::vector<Id> signature{Id(weights.size()), Id(left)};
                    signature.insert(signature.end(), weights.begin(), weights.end());
                    signature.insert(signature.end(), chain.args.begin() + left, chain.args.end() - !left);
                    candidates[probe].push_back({std::move(signature), {factors[j], std::move(weights)}});
                }
            }
            for (auto it = candidates.begin(); it != candidates.end(); )
                if (!present.count(it->first)) it = candidates.erase(it); else ++it;
            if (candidates.empty()) break;
        }
        for (auto& [ep, terms] : candidates) {
            std::sort(terms.begin(), terms.end(), [](const auto& a, const auto& b) { return a.first < b.first; });
            key signature;
            open_current current{ep.second, ep.first, {}};
            for (auto& item : terms) {
                signature.push_back(std::move(item.first));
                current.terms.push_back(std::move(item.second));
            }
            groups[std::move(signature)].push_back({id, std::move(current)});
        }
    }
    if (arithmetic < minimum_arithmetic) return {};
    current_catalog selected;
    std::map<Id, size_t> savings;
    for (auto& [signature, occurrences] : groups) {
        std::set<Id> probes;
        for (auto& site : occurrences) probes.insert(site.current.probe);
        size_t projections = 0;
        for (Id probe : probes) {
            const auto& spinor = tree.node(probe);
            projections += tree.node(spinor.args.front()).head == "\"Spin\"" ? 2 : 1;
        }
        const auto terms = signature.size();
        // Each scalar bracket already has a point cache. Charge both components
        // of the open sum and the final two-component dot at every projection.
        const auto old_cost = projections * (2 * terms - 1);
        const auto new_cost = 2 * (2 * terms - 1) + 3 * projections;
        if (probes.size() < 2 || old_cost <= new_cost) continue;
        const auto gain = old_cost - new_cost;
        for (auto& site : occurrences) {
            if (savings[site.sum] >= gain) continue;
            savings[site.sum] = gain;
            selected[site.sum] = std::move(site.current);
        }
    }
    return selected;
}
} // namespace hepcat::spinas_detail
