#include "five_point_poles.hpp"
#include "chart_scalar.hpp"
#include "modular_fit.hpp"
#include "validation_inventory.hpp"

#include <algorithm>
#include <numeric>
#include <set>

namespace hepcat {
namespace {
using ids = std::vector<Id>;
using bindings = std::map<Id, mpq_class>;
using system = fit_detail::modular_system;
using cut = validation_detail::channel_obligation;

struct tensor {
    Id expression;
    int insertions;
};
struct edge {
    int first, last, insertions;
    Id chain;
};

// Enumerate tensors of the declared external spin degrees. Momentum conservation
// eliminates the last slash momentum. Endpoint Dirac equations eliminate an
// adjacent slash on the same leg. Scalars have no spinor endpoints.
std::vector<tensor> spin_tensors(Algebra &a, const std::vector<int> &spins, int budget) {
    const int count = static_cast<int>(spins.size());
    auto endpoint = [&](int i, int ch) {
        return a.call("Spinor",
                      {a.atom("\"Spin\""), a.atom(ch ? "\"Square\"" : "\"Angle\""), a.integer(i + 1)});
    };
    auto momentum = [&](int i) { return a.call("Mom", {a.integer(i + 1)}); };
    std::vector<edge> edges;
    for (int i = 0; i < count; ++i)
        if (spins[i])
            for (int j = i + 1; j < count; ++j)
                if (spins[j])
                    for (int ch : {0, 1})
                        edges.push_back({i, j, 0, a.call("SpinorChain", {endpoint(i, ch), endpoint(j, ch)})});
    if (budget >= 1)
        for (int i = 0; i < count; ++i)
            if (spins[i])
                for (int j = 0; j < count; ++j)
                    if (spins[j])
                        for (int k = 0; k < count - 1; ++k)
                            if (k != i && k != j)
                                edges.push_back(
                                    {i, j, 1,
                                     a.call("SpinorChain", {endpoint(i, 0), momentum(k), endpoint(j, 1)})});
    if (budget >= 2)
        for (int i = 0; i < count; ++i)
            if (spins[i])
                for (int j = i; j < count; ++j)
                    if (spins[j])
                        for (int k = 0; k < count - 1; ++k)
                            if (k != i)
                                for (int l = 0; l < count - 1; ++l)
                                    if (l != j && l != k)
                                        for (int ch : {0, 1})
                                            edges.push_back(
                                                {i, j, 2,
                                                 a.call("SpinorChain", {endpoint(i, ch), momentum(k),
                                                                        momentum(l), endpoint(j, ch)})});
    std::vector<tensor> result;
    std::set<Id> seen;
    auto append = [&](Id x, int degree) {
        if (seen.insert(x).second)
            result.push_back({x, degree});
    };
    auto left = spins;
    ids factors;
    auto build = [&](auto self, std::size_t first, int remaining) -> void {
        if (std::all_of(left.begin(), left.end(), [](int k) { return k == 0; })) {
            Id product = a.mul(factors);
            append(product, budget - remaining);
            if (remaining >= 2)
                for (int k = 0; k < count - 1; ++k)
                    for (int l = k + 1; l < count - 1; ++l)
                        append(a.mul({product, a.call("MomProd", {a.integer(k + 1), a.integer(l + 1)})}),
                               budget - remaining + 2);
            return;
        }
        for (std::size_t j = first; j < edges.size(); ++j) {
            const auto &e = edges[j];
            if (e.insertions > remaining || !left[e.first] || !left[e.last] ||
                (e.first == e.last && left[e.first] < 2))
                continue;
            --left[e.first];
            --left[e.last];
            factors.push_back(e.chain);
            self(self, j, remaining - e.insertions);
            factors.pop_back();
            ++left[e.first];
            ++left[e.last];
        }
    };
    build(build, 0, budget);
    return result;
}

// Collect the model scalar monomials multiplying terms with a simple selected
// pole. All kinematic heads remain opaque, so this never expands the amplitude.
using monomial = std::map<Id, int>;
using term = std::pair<int, monomial>;
class scalar_support {
    Algebra &a;
    Id pole, root2;
    std::map<Id, std::set<term>> memo;

    const std::set<term> &signatures(Id x) {
        if (auto it = memo.find(x); it != memo.end())
            return it->second;
        const auto &n = a.node(x);
        std::set<term> out{{0, {}}};
        if (x == pole)
            out = {{1, {}}};
        else if (n.head == "ModelParameter" || x == root2 || n.head == "I" || n.head == "System`I")
            out = {{0, {{x, 1}}}};
        else if (n.head == "Times") {
            for (Id arg : n.args) {
                const auto &part = signatures(arg);
                std::set<term> next;
                for (const auto &p : out)
                    for (const auto &q : part) {
                        auto combined = p;
                        combined.first += q.first;
                        for (auto [k, e] : q.second)
                            combined.second[k] += e;
                        next.insert(std::move(combined));
                        if (next.size() > 512)
                            throw residue_unsupported("simple-pole scalar support budget");
                    }
                out = std::move(next);
            }
        } else if (n.head == "Plus") {
            out.clear();
            for (Id arg : n.args) {
                const auto &part = signatures(arg);
                out.insert(part.begin(), part.end());
                if (out.size() > 512)
                    throw residue_unsupported("simple-pole scalar support budget");
            }
        } else if (n.head == "Power" && n.args.size() == 2) {
            mpq_class exponent;
            if (a.number(n.args[1], exponent) && exponent.get_den() == 1 && abs(exponent) < 16) {
                const auto &base = signatures(n.args[0]);
                if (base.size() == 1) {
                    auto value = *base.begin();
                    int power = exponent.get_num().get_si();
                    value.first *= power;
                    for (auto &kv : value.second)
                        kv.second *= power;
                    out = {std::move(value)};
                }
            }
        }
        return memo.emplace(x, std::move(out)).first->second;
    }

  public:
    scalar_support(Algebra &tree, Id denominator)
        : a(tree), pole(denominator), root2(a.parse("Power[2,Rational[1,2]]")) {}
    ids factors(Id seed, Id target, const std::set<Id> &masses, Id unit) {
        std::set<monomial> normalized;
        for (Id x : {seed, target})
            for (auto [order, m] : signatures(x)) {
                if (order != -1)
                    continue;
                int dimension = 0;
                for (auto [k, e] : m)
                    if (masses.count(k))
                        dimension += e;
                m[unit] -= dimension;
                for (auto it = m.begin(); it != m.end();) {
                    if (it->first == root2)
                        it->second = (it->second % 2 + 2) % 2;
                    if (!it->second)
                        it = m.erase(it);
                    else
                        ++it;
                }
                normalized.insert(std::move(m));
            }
        ids out;
        for (const auto &m : normalized) {
            ids f;
            for (auto [k, e] : m)
                f.push_back(a.pow(k, e));
            out.push_back(a.mul(f));
        }
        std::stable_sort(out.begin(), out.end(), [&](Id x, Id y) { return a.cost(x) < a.cost(y); });
        return out;
    }
};

struct tensor_solution {
    ids columns;
    std::vector<system::word> weights;
};
using samples = std::vector<bindings>;

bool agrees(Algebra &a, const cut &channel, Id candidate, const samples &points, unsigned start,
            factorization_report &report) {
    for (std::size_t i = 0; i < points.size(); ++i)
        for (unsigned branch = 0; branch < 4; ++branch) {
            bool tested = false;
            for (unsigned retry = 0; retry < 32 && !tested; ++retry)
                try {
                    const auto r = residue_components(a, channel.shell, {candidate, channel.target},
                                                      start + 4096 * i + branch + 4 * retry, points[i]);
                    ++report.validation_samples;
                    if (r[0] != r[1])
                        return false;
                    tested = true;
                } catch (const residue_singular &) {
                }
            if (!tested)
                return false;
        }
    return true;
}

std::optional<tensor_solution> fit_tensors(Algebra &a, const cut &channel, Id seed, const ids &basis,
                                           const bindings &point, unsigned start, bool previously_independent,
                                           factorization_report &report) {
    ids columns{channel.target, seed};
    columns.insert(columns.end(), basis.begin(), basis.end());
    system fit(basis.size());
    unsigned dependent = 0;
    for (std::size_t j = 0; j < 4 * basis.size() + 110; ++j)
        try {
            const auto r = residue_sample(a, channel.shell, columns, start + j, point);
            const auto rank = fit.rank();
            ++report.training_samples;
            if (!fit.add(r, 2, r[0] - r[1]))
                return {};
            dependent = rank == fit.rank() ? dependent + 1 : 0;
            if (previously_independent ? fit.rank() != basis.size() : dependent < 5)
                continue;
            tensor_solution result;
            const auto weights = fit.residues();
            for (std::size_t k = 0; k < weights.size(); ++k)
                if (weights[k] || previously_independent) {
                    result.columns.push_back(basis[k]);
                    result.weights.push_back(weights[k]);
                }
            return result;
        } catch (const residue_singular &) {
        }
    return {};
}

// Identify the coefficient component in Q(sqrt(2),i) before fitting rational
// weights. All bare massive spin tensors on the rational chart are rational.
std::optional<unsigned> residual_phase(Algebra &a, const cut &channel, Id seed, const bindings &point) {
    for (unsigned j = 0; j < 32; ++j)
        try {
            const auto v = residue_components(a, channel.shell, {channel.target, seed}, 27000 + j, point);
            std::optional<unsigned> phase;
            for (unsigned c = 0; c < 4; ++c)
                if (v[0][c] != v[1][c]) {
                    if (phase)
                        return {};
                    phase = c;
                }
            if (phase)
                return phase;
        } catch (const residue_singular &) {
        }
    return {};
}

ids scalar_weights(Algebra &a, const cut &channel, Id seed, const std::set<Id> &masses, Id unit,
                   const bindings &point, unsigned phase, bool include_ratios) {
    scalar_support support(a, channel.propagator);
    auto factors = support.factors(seed, channel.target, masses, unit);
    chart_detail::Parameters values(a, 0, point);
    const auto root2 = a.parse("Power[2,Rational[1,2]]");
    for (Id &q : factors) {
        auto value = values.value(q);
        unsigned k = 0;
        while (k < 4 && !value.c[k])
            ++k;
        if (k == 4)
            return {};
        if ((phase ^ k) & 1)
            q = a.mul({q, root2});
        if ((phase ^ k) & 2)
            q = a.mul({q, a.atom("I")});
    }
    if (include_ratios) {
        // Removing two slashes can also introduce a squared mass from a
        // different external leg. Test those weights against additional model
        // assignments rather than fixing that ratio to its numerical value.
        const auto originals = factors;
        std::set<Id> seen(originals.begin(), originals.end());
        for (Id mass : masses)
            if (mass != unit && mass != a.integer(0))
                for (int exponent : {-2, 2})
                    for (Id q : originals) {
                        Id next = a.mul({q, a.pow(mass, exponent), a.pow(unit, -exponent)});
                        if (seen.insert(next).second)
                            factors.push_back(next);
                    }
    }
    return factors;
}

std::optional<Id> pole_correction(Algebra &a, const cut &channel, Id seed, const std::vector<tensor> &tensors,
                                  const std::set<Id> &masses, Id unit, int spinor_pairs, const samples &points,
                                  const samples &controls, bool include_ratios, factorization_report &report) {
    if (agrees(a, channel, seed, {points[0], points[1]}, 17015, report))
        return a.integer(0);
    const auto phase = residual_phase(a, channel, seed, points[0]);
    if (!phase)
        return {};
    const auto factors = scalar_weights(a, channel, seed, masses, unit, points[0], *phase, include_ratios);
    if (factors.empty())
        return {};
    ids columns;
    for (auto t : tensors) {
        // At five points the numerator of a single pole has mass dimension one.
        columns.push_back(a.mul({t.expression, a.pow(channel.propagator, -1),
                                 a.pow(unit, 1 - spinor_pairs - t.insertions), factors.front()}));
    }
    auto support = fit_tensors(a, channel, seed, columns, points[0], 23124, false, report);
    if (!support || support->columns.empty())
        return {};
    ids ratios;
    for (Id q : factors)
        ratios.push_back(a.mul({q, a.pow(factors.front(), -1)}));
    std::vector<system> coefficients(support->columns.size(), system(ratios.size()));
    for (std::size_t i = 0; i < points.size(); ++i) {
        const auto fitted =
            i ? fit_tensors(a, channel, seed, support->columns, points[i], 33000 + 701 * i, true, report)
              : support;
        if (!fitted || fitted->weights.size() != coefficients.size())
            return {};
        chart_detail::Parameters values(a, 0, points[i]);
        std::vector<mpq_class> weights;
        for (Id q : ratios) {
            const auto scalar = values.value(q);
            if (!scalar.rational())
                return {};
            weights.push_back(scalar.c[0]);
        }
        ids terms;
        for (std::size_t k = 0; k < coefficients.size(); ++k) {
            if (!coefficients[k].add(weights, 0, mpq_class(fitted->weights[k])))
                return {};
            const auto solution = coefficients[k].solution();
            if (!solution)
                continue;
            ids prefactor;
            for (std::size_t j = 0; j < solution->size(); ++j)
                if ((*solution)[j] != 0)
                    prefactor.push_back(a.mul({a.exact((*solution)[j]), ratios[j]}));
            terms.push_back(a.mul({a.add(prefactor), support->columns[k]}));
        }
        if (terms.size() != coefficients.size())
            continue;
        Id correction = a.add(terms);
        if (agrees(a, channel, a.add({seed, correction}), controls, 73121, report))
            return correction;
    }
    return {};
}

samples model_points(Algebra &a, const model &vertices, const ids &roots, const std::vector<unsigned> &starts) {
    std::set<std::string> names;
    std::set<Id> seen;
    auto visit = [&](auto self, Id x) -> void {
        if (!seen.insert(x).second)
            return;
        const auto &n = a.node(x);
        if (n.head == "ModelParameter")
            names.insert(a.format(x));
        else
            for (Id child : n.args)
                self(self, child);
    };
    for (Id x : roots)
        visit(visit, x);
    samples result;
    for (auto first : starts)
        for (const auto &settings :
             vertices.high_energy_parameter_points({names.begin(), names.end()}, first)) {
            bindings values;
            for (const auto &[key, value] : settings) {
                mpq_class q;
                if (!a.number(a.parse(value), q))
                    return {};
                values.emplace(a.parse(key), q);
            }
            if (values.size() != names.size())
                return {};
            result.push_back(std::move(values));
        }
    return result;
}
} // namespace

std::optional<Id> five_point_local_poles(Algebra &a, const amplitude &process, const model &vertices,
                                         factorization_report &report) {
    if (process.particles().size() != 5 || vertices.is_feynman())
        return {};
    std::vector<int> spins;
    int anchor = 0, vectors = 0;
    Id unit = a.integer(0);
    for (const auto &name : process.particles()) {
        const auto &p = vertices.particle(name);
        if (p.color != 1 || p.mass == "0" || (p.spin2 != 0 && p.spin2 != 2))
            return {};
        spins.push_back(p.spin2);
        if (p.spin2 == 0)
            anchor = static_cast<int>(spins.size());
        else {
            if (!vectors)
                unit = a.parse(p.mass);
            ++vectors;
        }
    }
    if (!anchor || vectors < 3 || process.factorization_products().empty())
        return {};
    try {
        const auto inventory =
            validation_detail::validation_inventory(a, process, amp_expression("0", process.external_masses()));
        // At five points the short side is a pair. Channels containing one
        // fixed external leg cannot coexist in a tree. Use the complete 3 x 4
        // products there. Use the ordinary constructive diagrams for the rest.
        auto anchored = [&](const numerator_shell &shell) {
            const auto &legs = shell.legs;
            bool contains = std::find(legs.begin(), legs.end(), anchor) != legs.end();
            return legs.size() == 2 ? contains : !contains;
        };
        ids seed_terms;
        for (const auto &c : inventory.channels)
            if (anchored(c.shell))
                seed_terms.push_back(c.target);
        for (const auto &row : process.diagrams())
            if (std::none_of(row.shells.begin(), row.shells.end(), anchored))
                seed_terms.push_back(validation_detail::import_expression(a, row.expression));
        Id seed = validation_detail::validation_inventory(
                      a, process, amp_expression(a, a.add(seed_terms), process.external_masses()))
                      .final;
        std::set<Id> masses(a.masses().begin(), a.masses().end());
        ids roots{seed};
        for (const auto &c : inventory.channels) {
            roots.push_back(c.target);
            masses.insert(a.parse(c.shell.mass));
        }
        roots.insert(roots.end(), masses.begin(), masses.end());
        const auto points =
            model_points(a, vertices, roots, {1, 4, 7, 10, 13, 16, 19, 22, 25, 28, 31, 34, 37, 40, 43, 46});
        const auto controls = model_points(a, vertices, roots, {61});
        const auto final_controls = model_points(a, vertices, roots, {77});
        if (points.size() < 2 || controls.empty() || final_controls.empty())
            return {};
        const auto directions = spin_tensors(a, spins, vectors == 3 ? 2 : 1);
        ids completed{seed};
        for (const auto &c : inventory.channels) {
            auto correction = pole_correction(a, c, seed, directions, masses, unit, vectors, points, controls,
                                              vectors == 3, report);
            if (!correction)
                return {};
            completed.push_back(*correction);
        }
        Id result = a.add(completed);
        // Every fitted numerator has only its own simple denominator. Check
        // the assembled expression on every cut, including zero residues.
        for (const auto &c : inventory.channels)
            if (!agrees(a, c, result, final_controls, 99357, report))
                return {};
        return result;
    } catch (const residue_unsupported &) {
        return {};
    } catch (const residue_singular &) {
        return {};
    } catch (const std::invalid_argument &) {
        // Models with parameters outside the exact rational charts retain the ordinary completion path.
        return {};
    }
}
} // namespace hepcat
