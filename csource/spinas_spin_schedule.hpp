// Schedule scalar contractions by their actual external spin dependencies.
// The inner spin loops consume the completed multi-spin sums from outer loops.
#pragma once

#include <algorithm>
#include <cstdint>
#include <limits>
#include <string>
#include <vector>

namespace hepcat::spinas_detail {

// The native scalar emitter names temporaries v0, v1, ... and accesses a
// spin slot as indices[leg][slot]. Inspect only these two generated tokens.
template<class Visit> void spin_operands(const std::string& expression, Visit visit) {
    for (size_t i = 0; i < expression.size(); ++i) {
        const bool temp = expression[i] == 'v';
        if (!temp && (expression[i] != 'i' || expression.compare(i, 8, "indices[") != 0)) continue;
        size_t end = i + (temp ? 1 : 8);
        if (end == expression.size() || expression[end] < '0' || expression[end] > '9') continue;
        unsigned index = 0;
        do {
            index = index * 10 + unsigned(expression[end++] - '0');
        } while (end < expression.size() && expression[end] >= '0' && expression[end] <= '9');
        visit(temp, index, i, end);
        i = end - 1;
    }
}

struct spin_schedule {
    std::vector<int> legs, stage, local, shared;
    size_t shared_count = 0;
    std::uint64_t assignments = 0;
};

// Find the loop order with the fewest scalar assignments. The subset counts
// include exactly those contractions evaluable with the spins in the subset.
inline spin_schedule schedule_spins(const std::vector<std::string>& rhs, const std::vector<int>& legs) {
    spin_schedule plan;
    if (legs.empty() || legs.size() > 12 || rhs.empty()) return plan;
    const unsigned states = 1u << legs.size();
    std::vector<unsigned> bit(*std::max_element(legs.begin(), legs.end()) + 1);
    for (size_t j = 0; j < legs.size(); ++j) bit[legs[j]] = 1u << j;
    std::vector<unsigned> masks;
    std::vector<std::uint64_t> count(states), cost(states);
    for (const auto& expr : rhs) {
        unsigned mask = 0;
        spin_operands(expr, [&](bool temp, unsigned i, size_t, size_t) {
            mask |= temp ? masks.at(i) : bit.at(i);
        });
        masks.push_back(mask);
        ++count[mask];
    }
    for (unsigned b = 1; b < states; b <<= 1)
        for (unsigned s = 0; s < states; ++s)
            if (s & b) count[s] += count[s ^ b];
    std::vector<unsigned> last(states);
    cost[0] = count[0];
    for (unsigned s = 1; s < states; ++s) {
        cost[s] = std::numeric_limits<std::uint64_t>::max();
        const unsigned evaluations = 1u << __builtin_popcount(s);
        for (unsigned bits = s; bits; bits &= bits - 1) {
            const auto b = bits & -bits, prev = s ^ b;
            const auto trial = cost[prev] + evaluations * (count[s] - count[prev]);
            if (trial < cost[s]) { cost[s] = trial; last[s] = b; }
        }
    }
    plan.assignments = cost.back();
    std::vector<int> depth(states);
    for (unsigned s = states - 1; s; s ^= last[s]) {
        depth[last[s]] = __builtin_popcount(s);
        plan.legs.push_back(legs[__builtin_ctz(last[s])]);
    }
    std::reverse(plan.legs.begin(), plan.legs.end());
    for (unsigned s = 1; s < states; ++s)
        depth[s] = std::max(depth[s & -s], depth[s & (s - 1)]);
    std::vector<int> next(legs.size()+1);
    plan.shared.resize(rhs.size(), -1);
    for (size_t j = 0; j < rhs.size(); ++j) {
        // The last statement also supplies the polarized return value.
        const int stage = j + 1 == rhs.size() ? int(legs.size()) : depth[masks[j]];
        plan.stage.push_back(stage);
        plan.local.push_back(next[stage]++);
        spin_operands(rhs[j], [&](bool temp, unsigned i, size_t, size_t) {
            if (temp && plan.stage[i] != stage && plan.shared[i] < 0)
                plan.shared[i] = int(plan.shared_count++);
        });
    }
    return plan;
}

inline std::string scheduled_rhs(const std::string& expr, const spin_schedule& plan, int stage) {
    std::string result;
    size_t start = 0;
    spin_operands(expr, [&](bool temp, unsigned i, size_t begin, size_t end) {
        if (!temp) return;
        result.append(expr, start, begin-start);
        result += plan.stage[i] == stage ? "v" + std::to_string(plan.local[i])
                                         : "shared[" + std::to_string(plan.shared[i]) + "]";
        start = end;
    });
    return result.append(expr, start);
}
} // namespace hepcat::spinas_detail
