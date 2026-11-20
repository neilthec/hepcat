#include "vector_compact.hpp"
#include "chart_scalar.hpp"
#include "modular_fit.hpp"
#include <algorithm>
#include <array>
#include <map>
#include <numeric>
#include <optional>
#include <random>
#include <set>

namespace hepcat {
namespace {
using Q = mpq_class;
using ids = std::vector<Id>;
using fit_system = fit_detail::modular_system;
using field = fit_system::word;
using support = std::map<int, field>;
std::vector<support> sparse_supports(const fit_system &, const std::vector<unsigned> &);
using bindings = std::map<Id, Q>;
struct direction {
    Id x;
    int momentum_degree;
};
struct term {
    Id x;
    int momentum_degree = 0;
    std::array<unsigned, 4> legs{};
    std::map<Id, int> scalar;
    Id pole = 0;
};
struct sector {
    numerator_shell cut;
    ids original_terms;
    std::array<std::set<Id>, 2> coefficients_by_degree;
};
struct tensor_reduction {
    Algebra &a;
    Id source, one;
    numerator_shell generic_cut{{1, 2}, "NativeCompactInvariant"};
    Id generic_pole;
    std::map<Id, sector> poles;
    std::set<Id> parameter_ids, mass_ids;
    std::array<std::set<Id>, 2> locals;
    std::vector<direction> tensors;
    size_t remaining_evaluations = 4000000;
    tensor_reduction(Algebra &ar, Id root)
        : a(ar), source(root), one(a.integer(1)),
          generic_pole(
              a.call("PropDen", {a.call("Mom", {a.call("Multiparticle", {a.integer(1), a.integer(2)})}),
                                 a.atom(generic_cut.mass)})) {}
    int integer_power(Id id) {
        Q n;
        if (!a.number(id, n) || n.get_den() != 1 || abs(n) > 8)
            throw residue_unsupported("unsupported vector polynomial power");
        return n.get_num().get_si();
    }
    void add_scalar(term &t, Id id, int exponent) {
        Q q;
        if (a.number(id, q))
            return;
        const auto &n = a.node(id);
        if (n.head == "Times") {
            for (auto f : n.args)
                add_scalar(t, f, exponent);
            return;
        }
        if (n.head == "Power" && n.args.size() == 2) {
            if (a.number(n.args[1], q) && q.get_den() == 1) {
                if (std::abs(exponent) > 64)
                    throw residue_unsupported("vector scalar exponent budget");
                add_scalar(t, n.args[0], exponent * integer_power(n.args[1]));
                return;
            }
            if (a.number(n.args[1], q) && q.get_den() == 2 && abs(q) <= 8 && a.node(n.args[0]).head == "2") {
                t.scalar[a.parse("Power[2,Rational[1,2]]")] += exponent * q.get_num().get_si();
                return;
            }
        }
        if (!chart_detail::free_parameter(n))
            throw residue_unsupported("nonmonomial vector parameter");
        parameter_ids.insert(id);
        t.scalar[id] += exponent;
    }
    std::vector<term> expand_terms(Id id) {
        const auto &n = a.node(id);
        std::vector<term> out;
        if (n.head == "Plus") {
            for (auto v : n.args) {
                auto b = expand_terms(v);
                out.insert(out.end(), b.begin(), b.end());
                if (out.size() > 600)
                    throw residue_unsupported("vector polynomial budget");
            }
            return out;
        }
        auto product = [&](const std::vector<term> &A, const std::vector<term> &B) {
            std::vector<term> result;
            if (A.size() * B.size() > 600)
                throw residue_unsupported("vector polynomial budget");
            for (auto &l : A)
                for (auto &r : B) {
                    auto t = l;
                    t.x = a.mul({l.x, r.x});
                    for (int j = 0; j < 4; j++)
                        t.legs[j] += r.legs[j];
                    t.momentum_degree += r.momentum_degree;
                    for (auto &[k, z] : r.scalar)
                        t.scalar[k] += z;
                    if (r.pole) {
                        if (t.pole)
                            throw residue_unsupported("multiple exchange poles");
                        t.pole = r.pole;
                    }
                    result.push_back(std::move(t));
                }
            return result;
        };
        int exponent = 1;
        Id base = id;
        if (n.head == "Power" && n.args.size() == 2 && a.node(n.args[0]).head != "ModelParameter") {
            Q q;
            if (a.number(n.args[1], q) && q.get_den() == 1) {
                exponent = integer_power(n.args[1]);
                base = n.args[0];
            }
        }
        if (n.head == "Times") {
            out.push_back({one});
            for (auto c : n.args)
                out = product(out, expand_terms(c));
            return out;
        }
        if (base != id && exponent > 0) {
            auto list = expand_terms(base);
            out.push_back({one});
            while (exponent--)
                out = product(out, list);
            return out;
        }
        const auto &b = a.node(base);
        term t{id};
        if (b.head == "PropDen" && exponent == -1 && b.args.size() == 2) {
            t.pole = base;
        } else if (b.head == "SpinorChain" && b.args.size() >= 2 && exponent == 1) {
            for (auto v : {b.args.front(), b.args.back()}) {
                const auto &e = a.node(v);
                if (e.head != "Spinor" || e.args.size() != 3 || a.node(e.args[0]).head != "\"Spin\"")
                    throw residue_unsupported("noncoherent vector endpoint");
                int leg = a.leg(e.args[2]);
                if (leg < 1 || leg > 4)
                    throw residue_unsupported("nonexternal endpoint");
                t.legs[leg - 1]++;
            }
            for (size_t i = 1; i + 1 < b.args.size(); i++)
                if (a.node(b.args[i]).head != "Mom")
                    throw residue_unsupported("nonordinary vector momentum");
            t.momentum_degree = b.args.size() - 2;
        } else if (b.head == "MomProd" || b.head == "Mandelstahm") {
            if (exponent != 1)
                throw residue_unsupported("nonlocal invariant");
            t.momentum_degree = 2;
        } else
            add_scalar(t, id, 1);
        return {t};
    }
    // A Dirac reduction can trade each removed numerator momentum for one
    // external or exchanged mass. Preserve the input coupling monomials.
    void insert_mass_monomials(std::set<Id> &out, std::map<Id, int> factors, int n) {
        if (n < 0 || n > 3)
            return;
        if (n) {
            for (auto m : mass_ids) {
                auto f = factors;
                f[m]++;
                insert_mass_monomials(out, std::move(f), n - 1);
            }
            return;
        }
        ids product;
        auto sqrt2 = a.parse("Power[2,Rational[1,2]]");
        for (auto [k, p] : factors) {
            if (k == sqrt2)
                p = ((p % 2) + 2) % 2;
            if (p)
                product.push_back(a.pow(k, p));
        }
        out.insert(a.mul(product));
    }
    bool inventory() {
        if (a.masses().size() != 4 || !a.shells().empty() || a.cost(source) > 24000)
            return false;
        for (auto m : a.masses()) {
            if (!chart_detail::free_parameter(a.node(m)))
                return false;
            parameter_ids.insert(m);
            mass_ids.insert(m);
        }
        auto terms = expand_terms(source);
        if (terms.size() < 8)
            return false;
        for (auto &t : terms) {
            if (t.legs != std::array<unsigned, 4>{2, 2, 2, 2} || t.momentum_degree > 3)
                return false;
            if (t.pole && !poles.count(t.pole)) {
                auto &b = a.node(t.pole);
                auto &m = a.node(b.args[0]);
                if (m.head != "Mom" || m.args.size() != 1)
                    return false;
                auto &multi = a.node(m.args[0]);
                if (multi.head != "Multiparticle" || multi.args.size() != 2)
                    return false;
                poles.emplace(
                    t.pole,
                    sector{{{a.leg(multi.args[0]), a.leg(multi.args[1])}, a.format(b.args[1])}, {}, {}});
                if (chart_detail::free_parameter(a.node(b.args[1])))
                    parameter_ids.insert(b.args[1]);
            }
        }
        if (poles.empty() || poles.size() > 8 || parameter_ids.count(a.atom(generic_cut.mass)))
            return false;
        for (auto &t : terms) {
            if (t.pole)
                poles.at(t.pole).original_terms.push_back(t.x);
            auto masses = mass_ids;
            if (t.pole) {
                auto exchange = a.node(t.pole).args[1];
                if (chart_detail::free_parameter(a.node(exchange)))
                    mass_ids.insert(exchange);
            }
            for (int d = 0; d <= 1; d++) {
                if (t.pole)
                    insert_mass_monomials(poles.at(t.pole).coefficients_by_degree[d], t.scalar,
                                          t.momentum_degree - d);
                insert_mass_monomials(locals[d], t.scalar, t.momentum_degree - d - (t.pole ? 2 : 0));
            }
            mass_ids = std::move(masses);
        }
        return true;
    }
    // Each massive vector contributes two coherent spinors. Enumerate covariant
    // products of four brackets with zero or one slash, including sandwiches
    // with two spinors from the same leg.
    void enumerate_tensors() {
        Id spin[4][2];
        for (int i = 0; i < 4; i++)
            for (int j = 0; j < 2; j++)
                spin[i][j] = a.call(
                    "Spinor", {a.atom("\"Spin\""), a.atom(j ? "\"Square\"" : "\"Angle\""), a.integer(i + 1)});
        struct edge {
            int i, j, momentum_degree;
            Id x;
        };
        std::vector<edge> edges;
        for (int i = 0; i < 4; i++)
            for (int j = i + 1; j < 4; j++)
                for (int t = 0; t < 2; t++)
                    edges.push_back({i, j, 0, a.call("SpinorChain", {spin[i][t], spin[j][t]})});
        for (int i = 0; i < 4; i++)
            for (int j = 0; j < 4; j++) {
                std::vector<int> ks;
                for (int k = 0; k < 4; k++)
                    if (k != i && k != j)
                        ks.push_back(k);
                for (int k : ks)
                    edges.push_back(
                        {i, j, 1,
                         a.call("SpinorChain", {spin[i][0], a.call("Mom", {a.integer(k + 1)}), spin[j][1]})});
            }
        std::array<unsigned, 4> occurrences{};
        ids factors;
        auto build = [&](auto self, size_t start, int momentum_degree) -> void {
            if (factors.size() == 4) {
                tensors.push_back({a.mul(factors), momentum_degree});
                return;
            }
            for (size_t k = start; k < edges.size(); k++) {
                auto &e = edges[k];
                if (momentum_degree + e.momentum_degree > 1)
                    continue;
                ++occurrences[e.i];
                ++occurrences[e.j];
                if (occurrences[e.i] <= 2 && occurrences[e.j] <= 2) {
                    factors.push_back(e.x);
                    self(self, k, momentum_degree + e.momentum_degree);
                    factors.pop_back();
                }
                --occurrences[e.i];
                --occurrences[e.j];
            }
        };
        build(build, 0, 0);
        std::stable_sort(tensors.begin(), tensors.end(),
                         [](auto x, auto y) { return x.momentum_degree < y.momentum_degree; });
    }
    std::optional<fit_system> sample_system(const numerator_shell &cut, const ids &roots,
                                            const bindings &fixed, size_t limit, size_t seed = 0) {
        fit_system s(roots.size() - 1);
        size_t unchanged = 0;
        for (size_t i = 0; i < limit + 64; i++) {
            if (roots.size() > remaining_evaluations)
                throw residue_unsupported("vector interpolation budget");
            remaining_evaluations -= roots.size();
            std::vector<std::array<Q, 4>> sample;
            try {
                sample = residue_components(a, cut, roots, seed + i, fixed);
            } catch (const residue_singular &) {
                continue;
            }
            size_t rank = s.rank();
            for (int axis = 0; axis < 4; axis++) {
                std::vector<Q> row;
                bool nonzero = false;
                for (auto &x : sample) {
                    row.push_back(std::move(x[axis]));
                    nonzero |= bool(row.back());
                }
                if (nonzero && !s.add(row, 0, row.back()))
                    return {};
            }
            if (rank == s.rank()) {
                if (++unchanged >= 12)
                    return s;
            } else
                unchanged = 0;
            if (s.rank() >= limit)
                return {};
        }
        return {};
    }
    // Propose tensor support at one fixed parameter assignment. Refit its mass
    // and coupling coefficients on independently varying exact parameter values.
    std::optional<Id> reconstruct_sector(Id target, Id divisor, const numerator_shell &cut,
                                         const std::array<std::set<Id>, 2> &coefficients_by_degree,
                                         bool local, int attempt) {
        bindings fixed;
        int ordinal = 0;
        for (auto m : parameter_ids) {
            auto &value = fixed[m];
            value = Q(5 + 2 * ordinal++, 3);
            value.canonicalize();
        }
        std::vector<unsigned> weights;
        ids columns;
        for (auto d : tensors) {
            columns.push_back(a.mul({d.x, a.pow(divisor, -1)}));
            weights.push_back(1 + 4 * d.momentum_degree);
        }
        columns.push_back(target);
        auto fit = sample_system(cut, columns, fixed, 180);
        if (!fit)
            return {};
        auto tensor_supports = sparse_supports(*fit, weights);
        std::optional<Id> best;
        size_t rank = SIZE_MAX;
        for (auto &support : tensor_supports) {
            if (support.empty())
                return a.integer(0);
            ids symbolic;
            for (auto [j, v] : support)
                for (auto coeff : coefficients_by_degree[tensors[j].momentum_degree]) {
                    symbolic.push_back(a.mul({columns[j], coeff}));
                }
            if (symbolic.size() > 320 || symbolic.empty())
                continue;
            symbolic.push_back(target);
            auto symbolic_fit = sample_system(cut, symbolic, {}, symbolic.size() + 1, 707 + attempt * 1000);
            if (!symbolic_fit)
                continue;
            auto result = symbolic_fit->solution();
            if (!result)
                continue;
            ids terms;
            for (size_t i = 0; i < result->size(); ++i)
                if ((*result)[i] != 0)
                    terms.push_back(a.mul({symbolic[i], a.exact((*result)[i])}));
            if (terms.empty())
                continue;
            Id answer = a.add(terms);
            if (local)
                answer = a.mul({answer, divisor});
            auto cost = expression_complexity(a, answer);
            if (cost < rank) {
                rank = cost;
                best = answer;
            }
        }
        return best;
    }
    Id run() {
        // Solve the cuts first. Their continuations can differ by a local tensor.
        // Resolve that tensor by matching the *whole* input on ordinary kinematics.
        if (!inventory())
            return source;
        enumerate_tensors();
        ids parts;
        int part = 0;
        for (auto &[pole, g] : poles) {
            Id x = a.add(g.original_terms);
            if (expression_complexity(a, x) <= 12) {
                parts.push_back(x);
                continue;
            }
            auto out = reconstruct_sector(x, pole, g.cut, g.coefficients_by_degree, false, part++);
            if (!out)
                return source;
            parts.push_back(*out);
        }
        Id pole_sum = a.add(parts), gap = a.add({source, a.mul({a.integer(-1), pole_sum})}),
           inverse_pole = a.pow(generic_pole, -1);
        auto correction =
            reconstruct_sector(a.mul({gap, inverse_pole}), generic_pole, generic_cut, locals, true, 77);
        if (!correction)
            return source;
        auto result = a.add({pole_sum, *correction});
        auto delta = a.mul({a.add({source, a.mul({a.integer(-1), result})}), inverse_pole});
        // Dividing by an auxiliary channel with an independently varying mass lets
        // the exact cut chart sample generic scattering invariants. It is absent
        // from the returned expression.
        size_t passed = 0;
        for (size_t i = 40001; i < 40100 && passed < 24; i++)
            try {
                auto s = residue_components(a, generic_cut, {delta}, i, {});
                if (std::any_of(s[0].begin(), s[0].end(), [](auto &v) { return bool(v); }))
                    return source;
                ++passed;
            } catch (const residue_singular &) {
            }
        if (passed != 24)
            return source;
        if (expression_complexity(a, result) >= expression_complexity(a, source))
            return source;
        Id polished = collectCandidate(a, a.simplify(result));
        if (expression_complexity(a, polished) <= expression_complexity(a, result))
            result = polished;
        return collect_scalar_factors(a, result);
    }
};
// Pivot between equivalent bases in the modular equation system. A pivot
// replaces one independent tensor at a time. Prefer elimination of tensors
// with momenta; explore tied choices because a different support can give
// simpler *symbolic* coefficients after the masses vary.
std::vector<support> sparse_supports(const fit_system &system, const std::vector<unsigned> &weights) {
    if (!system.rank())
        return {{}};
    constexpr auto prime = fit_system::prime;
    const size_t column_count = weights.size();
    auto score = [&](auto &x) {
        int w = 0;
        for (auto [i, v] : x)
            w += weights[i];
        return w;
    };

    std::vector<int> original_terms;
    std::vector<std::vector<field>> data;
    for (size_t col = 0; col < column_count; ++col)
        if (!system.echelon()[col].empty()) {
            original_terms.push_back(col);
            data.push_back(system.echelon()[col]);
        }
    int n = data.size();
    for (int j = n - 1; j >= 0; --j) {
        int col = original_terms[j];
        for (int k = 0; k < j; ++k)
            if (long f = data[k][col])
                for (size_t i = 0; i <= column_count; i++)
                    data[k][i] = (data[k][i] + prime - fit_system::product(f, data[j][i])) % prime;
    }
    support baseline;
    for (size_t i = 0; i < data.size(); ++i)
        if (data[i][column_count])
            baseline[original_terms[i]] = data[i][column_count];
    auto best = baseline;
    int best_weight = score(best);
    std::vector<support> result{best};
    std::mt19937 random(323);
    for (int repeat = 0; repeat < 10; repeat++) {
        int current_weight = score(baseline);
        auto a = data;
        auto basis = original_terms;
        std::vector<bool> used(column_count);
        for (int c : basis)
            used[c] = true;
        std::vector<int> last_removed(column_count, -1000);
        for (int it = 0; it < 50; it++) {
            int best_gain = -3, entering = -1, leaving = -1, ties = 0;
            for (size_t c = 0; c < column_count; c++)
                if (!used[c] && it - last_removed[c] > 8) {
                    int introduced = weights[c];
                    std::map<long, std::pair<int, int>> hits;
                    for (int j = 0; j < n; j++)
                        if (long x = a[j][c]) {
                            if (a[j][column_count]) {
                                long t = fit_system::product(a[j][column_count], fit_system::inverse(x));
                                hits[t].first += weights[basis[j]];
                                hits[t].second = j;
                            } else
                                introduced += weights[basis[j]];
                        }
                    for (auto &[t, h] : hits) {
                        int gain = h.first - introduced;
                        if (gain > best_gain) {
                            best_gain = gain;
                            entering = c;
                            leaving = h.second;
                            ties = 1;
                        } else if (gain == best_gain && random() % ++ties == 0) {
                            entering = c;
                            leaving = h.second;
                        }
                    }
                }
            if (entering < 0)
                break;
            current_weight -= best_gain;
            long multiplier = fit_system::inverse(a[leaving][entering]);
            std::vector<int> nonzero;
            for (int i = 0; i <= column_count; i++)
                if (a[leaving][i]) {
                    a[leaving][i] = fit_system::product(a[leaving][i], multiplier);
                    nonzero.push_back(i);
                }
            for (int j = 0; j < n; j++)
                if (j != leaving)
                    if (long f = a[j][entering])
                        for (int i : nonzero)
                            a[j][i] = (a[j][i] + prime - fit_system::product(f, a[leaving][i])) % prime;
            used[basis[leaving]] = false;
            last_removed[basis[leaving]] = it;
            basis[leaving] = entering;
            used[entering] = true;
            if (current_weight <= best_weight) {
                if (current_weight < best_weight)
                    result.clear();
                best_weight = current_weight;
                best.clear();
                for (int j = 0; j < n; j++)
                    if (a[j][column_count])
                        best[basis[j]] = a[j][column_count];
                if (result.size() < 24 && std::find(result.begin(), result.end(), best) == result.end())
                    result.push_back(best);
            }
        }
    }
    return result;
}

} // namespace
Id compact_vector_amplitude(Algebra &a, Id source) {
    try {
        return tensor_reduction(a, source).run();
    } catch (const residue_unsupported &) {
        return source;
    } catch (const residue_singular &) {
        return source;
    } catch (const std::length_error &) {
        return source;
    } catch (const std::overflow_error &) {
        return source;
    }
}
} // namespace hepcat
