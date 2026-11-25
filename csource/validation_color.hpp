#pragma once
#include "validation_color_polynomial.hpp"
#include "residue.hpp"
#include <array>
#include <complex>
#include <set>

namespace hepcat::validation_detail {

// Use B_a = 2 T_a (a < 8), B_8 = 2 sqrt(3) T_8. Every matrix entry and
// structure tensor in this orthogonal basis is a Gaussian integer. Contracting
// an adjoint color requires the inverse metric 1/4 or 1/12, including when
// summing external colors for a squared amplitude. No sqrt(3) enters a cut.
using color_number = std::complex<mpq_class>;
using color_matrix = std::array<std::array<std::complex<int>, 3>, 3>;
struct color_basis {
    std::array<color_matrix, 8> b{};
    int structure[8][8][8]{};
    color_basis() {
        const int left[] = {0, 0, 1}, right[] = {1, 2, 2}, even[] = {0, 3, 5}, odd[] = {1, 4, 6};
        for (int k = 0; k < 3; ++k) {
            auto i = left[k], j = right[k];
            b[even[k]][i][j] = b[even[k]][j][i] = 1;
            b[odd[k]][i][j] = {0, -1};
            b[odd[k]][j][i] = {0, 1};
        }
        b[2][0][0] = 1;
        b[2][1][1] = -1;
        b[7][0][0] = b[7][1][1] = 1;
        b[7][2][2] = -2;
        for (int x = 0; x < 8; ++x)
            for (int y = 0; y < 8; ++y)
                for (int z = 0; z < 8; ++z)
                    for (int i = 0; i < 3; ++i)
                        for (int j = 0; j < 3; ++j)
                            for (int k = 0; k < 3; ++k)
                                structure[x][y][z] +=
                                    2 * std::imag((b[x][i][j] * b[y][j][k] - b[y][i][j] * b[x][j][k]) *
                                                  b[z][k][i]);
    }
    static const color_basis& instance() {
        static const color_basis basis;
        return basis;
    }
    static int metric(int adjoint) { return adjoint == 7 ? 12 : 4; }
};

class color_network {
    enum class kind { delta, generator, structure, trace };
    struct tensor {
        kind type;
        std::vector<int> slots;
    };
    struct index {
        int dimension = 0, leg = -1, incidence = 0;
    };
    std::vector<tensor> tensors;
    std::vector<index> indices;
    std::vector<int> sums;
    // A sparse tensor can force the entire contraction to zero as soon as
    // its last index is assigned, even with other internal colors still free.
    std::vector<std::vector<std::size_t>> ready;
    bool supported(std::size_t depth, const std::vector<int>& values) const {
        for (auto i : ready[depth]) {
            const auto& t = tensors[i];
            auto color = [&](std::size_t j) { return values[t.slots[j]]; };
            if (t.type == kind::delta && color(0) != color(1)) return false;
            if (t.type == kind::structure && !color_basis::instance().structure[color(0)][color(1)][color(2)])
                return false;
            if (t.type == kind::generator &&
                color_basis::instance().b[color(2)][color(0)][color(1)] == std::complex<int>{}) return false;
        }
        return true;
    }

    color_number contract(const std::vector<int>& v) const {
        const auto& b = color_basis::instance();
        color_number product(1);
        for (const auto& t : tensors) {
            auto value = [&](size_t i) { return v[t.slots[i]]; };
            if (t.type == kind::delta) {
                if (value(0) != value(1))
                    return {};
                if (indices[t.slots[0]].dimension == 8)
                    product *= mpq_class(b.metric(value(0)));
            } else if (t.type == kind::structure) {
                int f = b.structure[value(0)][value(1)][value(2)];
                if (!f)
                    return {};
                product *= mpq_class(f);
            } else if (t.type == kind::generator) {
                auto g = b.b[value(2)][value(0)][value(1)];
                if (g == std::complex<int>{})
                    return {};
                product *= color_number(g.real(), g.imag());
            } else {
                std::array<std::array<color_number, 3>, 3> matrix{};
                for (int i = 0; i < 3; ++i)
                    matrix[i][i] = color_number(1);
                for (auto slot : t.slots) {
                    decltype(matrix) next{};
                    const auto& g = b.b[v[slot]];
                    for (int i = 0; i < 3; ++i)
                        for (int j = 0; j < 3; ++j)
                            for (int k = 0; k < 3; ++k)
                                if (g[k][j] != std::complex<int>{})
                                    next[i][j] += matrix[i][k] * color_number(g[k][j].real(), g[k][j].imag());
                    matrix = std::move(next);
                }
                product *= matrix[0][0] + matrix[1][1] + matrix[2][2];
            }
        }
        return product;
    }
    color_number sum(size_t next, std::vector<int>& values) const {
        if (next == sums.size())
            return contract(values);
        auto slot = sums[next], dim = indices[slot].dimension;
        color_number result{};
        for (int j = 0; j < dim; ++j) {
            values[slot] = j;
            if (!supported(next + 1, values)) continue;
            auto part = sum(next + 1, values);
            if (dim == 8)
                part /= mpq_class(color_basis::metric(j));
            result += part;
        }
        return result;
    }

  public:
    color_network(const Algebra& a, const std::vector<int>& reps, const std::vector<Id>& factors) {
        std::map<Id, int> slots;
        auto constrain = [&](int slot, int dim) {
            auto& d = indices[slot].dimension;
            if ((dim != 3 && dim != 8) || (d && d != dim))
                throw residue_unsupported("incompatible SU(3) index dimension");
            d = dim;
        };
        for (Id factor : factors) {
            const auto& n = a.node(factor);
            auto name = short_head(n);
            auto type = name == "ColorDelta"       ? kind::delta
                        : name == "ColorGenerator" ? kind::generator
                        : name == "ColorStructure" ? kind::structure
                                                   : kind::trace;
            if ((type == kind::delta && n.args.size() != 2) ||
                ((type == kind::generator || type == kind::structure) && n.args.size() != 3))
                throw residue_unsupported("incorrect SU(3) tensor rank");
            auto& tensor = tensors.emplace_back(color_network::tensor{type, {}});
            for (Id arg : n.args) {
                auto [it, added] = slots.emplace(arg, indices.size());
                auto slot = it->second;
                if (added) {
                    auto& info = indices.emplace_back();
                    const auto& n = a.node(arg);
                    if (short_head(n) == "ColorIndex" && n.args.size() == 1) {
                        int leg = a.leg(n.args[0]);
                        if (leg <= 0 || size_t(leg) > reps.size())
                            throw residue_unsupported("missing external color representation");
                        info.leg = leg - 1;
                        constrain(slot, std::abs(reps[info.leg]));
                    } else if (short_head(n) == "colorInternalIndex" && n.args.size() == 2)
                        constrain(slot, a.leg(n.args[1]));
                    else if (n.compound)
                        throw residue_unsupported("unsupported color index " + a.format(arg));
                }
                ++indices[slot].incidence;
                if (type != kind::delta)
                    constrain(slot, type == kind::generator && tensor.slots.size() < 2 ? 3 : 8);
                tensor.slots.push_back(slot);
            }
        }
        bool changed = true;
        while (changed) {
            changed = false;
            for (const auto& tensor : tensors)
                if (tensor.type == kind::delta) {
                    int i = tensor.slots[0], j = tensor.slots[1];
                    if (indices[i].dimension && indices[j].dimension != indices[i].dimension) {
                        constrain(j, indices[i].dimension);
                        changed = true;
                    } else if (!indices[i].dimension && indices[j].dimension) {
                        constrain(i, indices[j].dimension);
                        changed = true;
                    }
                }
        }
        for (size_t i = 0; i < indices.size(); ++i) {
            const auto& idx = indices[i];
            if (!idx.dimension || (idx.leg < 0 && idx.incidence != 2))
                throw residue_unsupported("ambiguous or unpaired SU(3) index");
            if (idx.leg < 0)
                sums.push_back(i);
        }
        if (!sums.empty()) {
            ready.resize(sums.size() + 1);
            std::vector<std::size_t> assigned(indices.size(), 0);
            for (std::size_t j = 0; j < sums.size(); ++j) assigned[sums[j]] = j + 1;
            for (std::size_t i = 0; i < tensors.size(); ++i) {
                if (tensors[i].type == kind::trace) continue;
                std::size_t depth = 0;
                for (auto slot : tensors[i].slots) depth = std::max(depth, assigned[slot]);
                ready[depth].push_back(i);
            }
        }
    }
    // One factor of 1/sqrt(metric) per free adjoint occurrence restores
    // the conventional normalized Gell-Mann basis.
    std::vector<int> external_degrees(std::size_t legs) const {
        std::vector<int> degree(legs);
        for (const auto& idx : indices)
            if (idx.leg >= 0)
                degree[idx.leg] = idx.incidence;
        return degree;
    }
    color_number operator()(const std::vector<int>& colors) const {
        std::vector<int> values(indices.size());
        for (size_t i = 0; i < indices.size(); ++i)
            if (indices[i].leg >= 0)
                values[i] = colors.at(indices[i].leg);
        if (!sums.empty() && !supported(0, values)) return {};
        return sum(0, values);
    }
};

inline Id color_scalar(Algebra& a, const color_number& v) {
    if (v.imag() == 0)
        return a.exact(v.real());
    return a.add({a.exact(v.real()), a.mul({a.atom("I"), a.exact(v.imag())})});
}
struct projected_colors {
    // Leg 1 is the least significant digit in the mixed-radix color index.
    std::vector<std::vector<Id>> components;
    std::vector<size_t> assignment_component;
    std::vector<mpq_class> summed_metric;
};
class fundamental_color_projections {
    Algebra& a;
    const std::vector<int>& reps;

  public:
    fundamental_color_projections(Algebra& tree, const std::vector<int>& representations)
        : a(tree), reps(representations) {}
    projected_colors components(const std::vector<Id>& roots) const {
        color_polynomial pieces(a);
        if (std::none_of(roots.begin(), roots.end(), [&](Id x) { return pieces.contains_color(x); }))
            return {{roots}, {0}, {1}};
        std::size_t count = 1;
        for (int signed_dim : reps) {
            int dim = std::abs(signed_dim);
            if (dim != 1 && dim != 3 && dim != 8)
                throw residue_unsupported("unsupported external SU(3) representation");
            if (count > 65536 / size_t(dim))
                throw residue_unsupported("external color tensor exceeds the exhaustive projection budget");
            count *= dim;
        }
        std::map<std::vector<Id>, size_t> index;
        std::vector<color_network> networks;
        std::vector<std::map<size_t, Id>> terms;
        for (Id root : roots) {
            auto& grouped = terms.emplace_back();
            for (auto& term : pieces.split(root)) {
                std::sort(term.tensors.begin(), term.tensors.end());
                auto [it, added] = index.emplace(term.tensors, networks.size());
                if (added)
                    networks.emplace_back(a, reps, term.tensors);
                auto [c, unused] = grouped.emplace(it->second, a.integer(0));
                c->second = a.add({c->second, a.mul(std::move(term.coefficients))});
            }
        }
        projected_colors out;
        std::map<std::vector<Id>, size_t> profiles, component_index;
        for (size_t k = 0; k < count; ++k) {
            std::vector<int> colors(reps.size());
            mpq_class metric(1);
            size_t code = k;
            for (size_t i = 0; i < reps.size(); ++i) {
                int d = std::abs(reps[i]);
                colors[i] = code % d;
                code /= d;
                if (d == 8)
                    metric /= color_basis::metric(colors[i]);
            }
            std::vector<Id> weights;
            for (const auto& network : networks)
                weights.push_back(color_scalar(a, network(colors)));
            auto [p, new_profile] = profiles.emplace(weights, 0);
            if (new_profile) {
                std::vector<Id> component;
                for (const auto& grouped : terms) {
                    std::vector<Id> summands;
                    for (auto [network, coefficient] : grouped)
                        summands.push_back(a.mul({weights[network], coefficient}));
                    component.push_back(a.add(std::move(summands)));
                }
                auto [c, added] = component_index.emplace(component, out.components.size());
                if (added) {
                    out.components.push_back(std::move(component));
                    out.summed_metric.emplace_back(0);
                }
                p->second = c->second;
            }
            out.assignment_component.push_back(p->second);
            out.summed_metric[p->second] += metric;
        }
        return out;
    }
    std::vector<std::vector<Id>> project(const std::vector<Id>& roots) const {
        return components(roots).components;
    }
};
} // namespace hepcat::validation_detail
