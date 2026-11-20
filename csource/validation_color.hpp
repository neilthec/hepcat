#pragma once
#include "validation_color_polynomial.hpp"
#include "validation_inventory.hpp"
#include "residue.hpp"
#include <numeric>
#include <set>

namespace hepcat::validation_detail {

class fundamental_color_projections {
    using edge = std::pair<Id, Id>;
    struct flow {
        mpq_class weight{1};
        std::vector<edge> deltas;
    };
    Algebra& a;
    const std::vector<int>& reps;
    color_polynomial pieces;
    std::map<std::vector<Id>, std::vector<flow>> cache;

    std::vector<flow> expand(const std::vector<Id>& factors) const {
        std::vector<flow> flows(1);
        std::map<Id, std::vector<edge>> lines;
        std::map<Id, unsigned> internal_count;
        auto index = [&](Id id, int dimension) {
            const auto& node = a.node(id);
            auto name = short_head(node);
            if (name == "ColorIndex" && node.args.size() == 1) {
                auto part = a.leg(node.args[0]);
                if (dimension != 3 || part <= 0 || std::size_t(part) > reps.size() ||
                    std::abs(reps[part - 1]) != 3)
                    throw residue_unsupported(
                        "exact color projection requires external triplets and antitriplets");
            } else {
                if (node.compound && (name != "colorInternalIndex" || node.args.size() != 2 ||
                                      a.leg(node.args[1]) != dimension))
                    throw residue_unsupported("inconsistent internal color index dimension");
                ++internal_count[id];
            }
        };
        for (auto factor : factors) {
            const auto& node = a.node(factor);
            const auto& ids = node.args;
            auto name = short_head(node);
            if (name == "ColorDelta" && ids.size() == 2) {
                index(ids[0], 3);
                index(ids[1], 3);
                flows.front().deltas.emplace_back(ids[0], ids[1]);
            } else if (name == "ColorGenerator" && ids.size() == 3) {
                index(ids[0], 3);
                index(ids[1], 3);
                index(ids[2], 8);
                lines[ids[2]].emplace_back(ids[0], ids[1]);
            } else {
                throw residue_unsupported("exact color projection cannot contract " + name);
            }
        }
        for (auto [id, count] : internal_count)
            if (count != 2)
                throw residue_unsupported("unpaired or ambiguous internal color sum");
        for (const auto& [id, ends] : lines) {
            if (ends.size() != 2)
                throw residue_unsupported("unpaired adjoint generator index");
            auto [i, j] = ends[0];
            auto [k, l] = ends[1];
            if (flows.size() >= 1024)
                throw residue_unsupported("color-flow expansion exceeds budget");
            const auto old = flows.size();
            for (std::size_t f = 0; f < old; ++f) {
                flow direct = flows[f];
                flows[f].weight /= 2;
                flows[f].deltas.emplace_back(i, l);
                flows[f].deltas.emplace_back(k, j);
                direct.weight /= -6;
                direct.deltas.emplace_back(i, j);
                direct.deltas.emplace_back(k, l);
                flows.push_back(std::move(direct));
            }
        }
        return flows;
    }

    // Delta edges identify fundamental colors. A connected component without
    // an external color contributes the exact dimension of the internal loop.
    mpq_class value(const flow& f, const std::vector<int>& external) const {
        std::map<Id, Id> sets;
        auto root = [&](Id id) {
            auto [it, inserted] = sets.emplace(id, id);
            while (it->second != it->first)
                it = sets.find(it->second);
            return it->first;
        };
        for (auto [a, b] : f.deltas) {
            Id x = root(a), y = root(b);
            sets[x] = y;
        }
        std::map<Id, int> colors;
        for (auto [index, ignored] : sets) {
            Id group = root(index);
            auto [it, added] = colors.emplace(group, -1);
            const auto& n = a.node(index);
            if (short_head(n) == "ColorIndex") {
                auto color = external.at(a.leg(n.args[0]) - 1);
                if (it->second >= 0 && it->second != color)
                    return 0;
                it->second = color;
            }
        }
        auto result = f.weight;
        for (auto [group, color] : colors)
            if (color < 0)
                result *= 3;
        return result;
    }

  public:
    fundamental_color_projections(Algebra& tree, const std::vector<int>& representations)
        : a(tree), reps(representations), pieces(tree) {}

    std::vector<std::vector<Id>> project(const std::vector<Id>& roots) {
        if (std::none_of(roots.begin(), roots.end(), [&](Id x) { return pieces.contains_color(x); }))
            return {roots};
        if (reps.size() != a.masses().size())
            throw residue_unsupported("color validation requires the external color representations");
        std::size_t assignments = 1;
        for (int rep : reps) {
            if (std::abs(rep) == 1)
                continue;
            if (std::abs(rep) != 3)
                throw residue_unsupported("exact external adjoint color checks are unavailable");
            if (assignments > 729 / 3)
                throw residue_unsupported("external color tensor exceeds the exhaustive projection budget");
            assignments *= 3;
        }
        std::vector<std::map<std::vector<Id>, Id>> terms;
        for (Id root : roots) {
            auto& grouped = terms.emplace_back();
            for (auto& term : pieces.split(root)) {
                std::sort(term.tensors.begin(), term.tensors.end());
                if (!cache.count(term.tensors))
                    cache.emplace(term.tensors, expand(term.tensors));
                auto& coefficient = grouped.try_emplace(term.tensors, a.integer(0)).first->second;
                coefficient = a.add({coefficient, a.mul(std::move(term.coefficients))});
            }
        }
        std::set<std::vector<Id>> distinct;
        for (std::size_t j = 0; j < assignments; ++j) {
            std::vector<int> colors(reps.size());
            std::size_t code = j;
            for (std::size_t i = 0; i < colors.size(); ++i)
                if (std::abs(reps[i]) == 3) {
                    colors[i] = code % 3;
                    code /= 3;
                }
            std::map<std::vector<Id>, Id> weights;
            for (const auto& [factors, flows] : cache) {
                mpq_class sum;
                for (const auto& flow : flows)
                    sum += value(flow, colors);
                weights.emplace(factors, a.exact(sum));
            }
            std::vector<Id> component;
            for (const auto& grouped : terms) {
                std::vector<Id> contributions;
                for (const auto& [factors, coeff] : grouped)
                    contributions.push_back(a.mul({weights.at(factors), coeff}));
                component.push_back(a.add(std::move(contributions)));
            }
            distinct.insert(std::move(component));
        }
        return {distinct.begin(), distinct.end()};
    }
};
} // namespace hepcat::validation_detail
