#include "high_energy_contacts.hpp"
#include "four_point_numerators.hpp"
#include <algorithm>
#include <functional>
#include <set>
#include <stdexcept>
#include <unordered_map>

namespace hepcat {
bool leg_independent_scalar(const Algebra& a, Id id) {
    const auto& n = a.node(id);
    if (!n.compound || n.head == "ModelParameter" || n.head == "Rational" || n.head == "System`Complex")
        return true;
    if (n.head != "Plus" && n.head != "Times" && n.head != "Power") return false;
    return std::all_of(n.args.begin(), n.args.end(), [&](Id x) { return leg_independent_scalar(a, x); });
}
namespace {
bool kinematic_head(const std::string& h) {
    return h == "Spinor" || h == "SpinorChain" || h == "SpinorTrace" || h == "Mom" || h == "MomProd" ||
           h == "Mandelstahm" || h == "PropDen" || h == "Mass";
}
bool local(const Algebra& a, Id id, std::unordered_map<Id, bool>& cache) {
    if (auto p = cache.find(id); p != cache.end()) return p->second;
    const auto& n = a.node(id);
    bool ok = leg_independent_scalar(a, id);
    if (!ok) {
        if (n.head == "Power" && n.args.size() == 2) {
            mpq_class power;
            ok = a.number(n.args[1], power) && power >= 0 && power.get_den() == 1 && local(a, n.args[0], cache);
        } else if (n.head == "Spinor") {
            ok = n.args.size() >= 3 &&
                 (a.node(n.args[0]).head == "\"Spin\"" || a.node(n.args[0]).head == "\"Helicity\"") &&
                 a.leg(n.args[n.args.size() == 5 ? 3 : 2]) > 0;
        } else if (n.head == "Multiparticle" || n.head == "Times" || n.head == "Plus" ||
                   kinematic_head(n.head)) {
            ok = std::all_of(n.args.begin(), n.args.end(), [&](Id x) { return local(a, x, cache); });
        }
    }
    cache.emplace(id, ok);
    return ok;
}
using weights = std::vector<int>;
struct unsupported_catalog {};
// Coherent massive spin degree and massless little-group weight.
weights spin_weights(Algebra& a, Id id, std::unordered_map<Id, weights>& signatures) {
    if (auto f = signatures.find(id); f != signatures.end()) return f->second;
    const auto& n = a.node(id);
    weights w(a.masses().size());
    if (n.head == "SpinorChain" && n.args.size() >= 2) {
        for (Id end : {n.args.front(), n.args.back()}) {
            const auto& spin = a.node(end);
            if (spin.head != "Spinor" || spin.args.size() != 3) throw unsupported_catalog{};
            const auto& kind = a.node(spin.args[0]).head;
            if (kind == "\"Xi\"") continue;
            int leg = a.leg(spin.args[2]) - 1;
            if (leg < 0 || std::size_t(leg) >= w.size()) throw unsupported_catalog{};
            w[leg] += a.masses()[leg] == a.integer(0) && a.node(spin.args[1]).head == "\"Square\"" ? -1 : 1;
        }
    } else if (n.head == "Plus" || n.head == "Times") {
        bool first = true;
        for (Id p : n.args) {
            auto child = spin_weights(a,p,signatures);
            if (n.head == "Times") {
                for (std::size_t i = 0; i < w.size(); ++i)
                    if (__builtin_add_overflow(w[i],child[i],&w[i])) throw unsupported_catalog{};
            } else if (first)
                w = child;
            else if (w != child)
                throw unsupported_catalog{};
            first = false;
        }
    } else if (n.head == "Power" && n.args.size() == 2) {
        auto base = spin_weights(a,n.args[0],signatures);
        mpq_class q;
        if (std::any_of(base.begin(), base.end(), [](int x) { return x != 0; })) {
            if (!a.number(n.args[1], q) || q.get_den() != 1 || abs(q) > 32) throw unsupported_catalog{};
            for (std::size_t i = 0; i < w.size(); ++i)
                if (__builtin_mul_overflow(base[i],q.get_num().get_si(),&w[i])) throw unsupported_catalog{};
        }
    }
    signatures.emplace(id, w);
    return w;
}

class catalog_builder {
    Algebra& a;
    const Id zero, one;
    std::set<Id> masses;
    std::unordered_map<Id, weights> signatures;
    std::unordered_map<Id, std::set<Id>> scalar_parts;
    std::unordered_map<Id, int> dimensions;
    static constexpr std::size_t max_shapes = 512;

    weights signature(Id id) { return spin_weights(a,id,signatures); }

    // Propagator mass labels determine engineering dimensions without any
    // assumptions about how a particular model names masses and couplings.
    void mass_labels(Id id, std::set<Id>& seen) {
        if (!seen.insert(id).second) return;
        const auto& n = a.node(id);
        if (n.head == "PropDen" && n.args.size() >= 2 && n.args[1] != zero) masses.insert(n.args[1]);
        for (Id x : n.args)
            mass_labels(x, seen);
    }
    int dimension(Id id) {
        if (masses.count(id)) return 1;
        if (auto f = dimensions.find(id); f != dimensions.end()) return f->second;
        const auto& n = a.node(id);
        int d = 0;
        if (n.head == "Times")
            for (Id x : n.args)
                d += dimension(x);
        if (n.head == "Plus" && !n.args.empty()) {
            d = dimension(n.args.front());
            for (Id x : n.args)
                if (dimension(x) != d) throw unsupported_catalog{};
        }
        if (n.head == "Power" && n.args.size() == 2 && dimension(n.args[0]) != 0) {
            mpq_class q;
            if (!a.number(n.args[1], q) || q.get_den() != 1 || abs(q) > 128) throw unsupported_catalog{};
            d = dimension(n.args[0]) * q.get_num().get_si();
        }
        dimensions.emplace(id, d);
        return d;
    }
    Id strip_masses(Id id) {
        if (masses.count(id)) return one;
        const auto& n = a.node(id);
        if (n.head != "Plus" && n.head != "Times" && n.head != "Power") return id;
        std::vector<Id> args;
        for (Id x : n.args)
            args.push_back(strip_masses(x));
        return a.call(n.head, std::move(args));
    }
    std::set<Id> coefficients(Id id) {
        if (auto f = scalar_parts.find(id); f != scalar_parts.end()) return f->second;
        const auto& n = a.node(id);
        std::set<Id> c;
        mpq_class q;
        if (a.number(id, q))
            c.insert(one);
        else if (n.head == "Plus") {
            for (Id p : n.args) {
                auto child = coefficients(p);
                c.insert(child.begin(), child.end());
            }
        } else if (n.head == "Times") {
            c.insert(one);
            for (Id p : n.args) {
                auto child = coefficients(p);
                std::set<Id> next;
                for (Id l : c)
                    for (Id r : child)
                        next.insert(a.mul({l, r}));
                c = std::move(next);
                if (c.size() > 128) throw unsupported_catalog{};
            }
        } else
            c.insert(leg_independent_scalar(a, id) ? id : one);
        scalar_parts.emplace(id, c);
        return c;
    }
    struct edge {
        int l, r;
        bool angle;
    };
    Id bracket(int l, int r, bool angle) {
        auto endpoint = [&](int leg) {
            return a.call("Spinor", {a.atom(a.masses()[leg] == zero ? "\"Helicity\"" : "\"Spin\""),
                                     a.atom(angle ? "\"Angle\"" : "\"Square\""), a.integer(leg + 1)});
        };
        return a.call("SpinorChain", {endpoint(l), endpoint(r)});
    }
    using shapes = std::set<Id>;
    void pairings(const weights& w, weights& left, std::vector<edge>& edges, shapes& all) {
        std::size_t l = 0;
        while (l < left.size() && !left[l])
            ++l;
        if (l == left.size()) {
            std::vector<Id> terms;
            for (auto e : edges)
                terms.push_back(bracket(e.l, e.r, e.angle));
            all.insert(a.mul(std::move(terms)));
            if (all.size() > max_shapes) throw unsupported_catalog{};
            return;
        }
        --left[l];
        for (std::size_t r = l + 1; r < left.size(); ++r)
            if (left[r]) {
                for (bool angle : {true, false}) {
                    if ((a.masses()[l] == zero && angle != (w[l] > 0)) ||
                        (a.masses()[r] == zero && angle != (w[r] > 0)))
                        continue;
                    // Enumerate equivalent endpoints once. The noncrossing
                    // contractions for either chirality span by Schouten.
                    if (!edges.empty() && edges.back().l == int(l) &&
                        std::make_pair(int(r), angle) < std::make_pair(edges.back().r, edges.back().angle))
                        continue;
                    if (std::any_of(edges.begin(), edges.end(), [&](edge e) {
                            return e.angle == angle && e.l < int(l) && int(l) < e.r && e.r < int(r);
                        }))
                        continue;
                    --left[r];
                    edges.push_back({int(l), int(r), angle});
                    pairings(w, left, edges, all);
                    edges.pop_back();
                    ++left[r];
                }
            }
        ++left[l];
    }
    void metric_products(std::vector<int> legs, std::vector<Id>& parts, shapes& out) {
        if (legs.empty()) {
            out.insert(a.mul(parts));
            return;
        }
        for (std::size_t j = 1; j < legs.size(); ++j) {
            int l = legs[0], r = legs[j];
            auto other = legs;
            other.erase(other.begin() + j);
            other.erase(other.begin());
            parts.push_back(a.mul({bracket(l, r, true), bracket(l, r, false)}));
            metric_products(std::move(other), parts, out);
            parts.pop_back();
        }
    }

    std::set<Id> numerator_shapes(const weights& w, unsigned insertions) {
        weights left = w;
        for (int& slots : left)
            slots = std::abs(slots);
        std::set<Id> shapes;
        std::vector<Id> words;
        std::function<void(unsigned)> generate_words = [&](unsigned left_insertions) {
            std::size_t i = 0;
            while (i < left.size() && !left[i])
                ++i;
            if (i == left.size()) {
                if (left_insertions == 0)
                    shapes.insert(a.mul(words));
                return;
            }
            --left[i];
            for (std::size_t j = i; j < left.size(); ++j)
                if (left[j]) {
                    --left[j];
                    for (unsigned length = 0; length <= left_insertions; ++length) {
                        if (i == j && !length)
                            continue;
                        std::vector<int> momenta;
                        for (int k = 0; k < 4; ++k)
                            if (k != int(i) && k != int(j))
                                momenta.push_back(k);
                        std::vector<std::vector<int>> sequences;
                        if (!length)
                            sequences.emplace_back();
                        if (length == 1) {
                            // The final choice is eliminated by momentum conservation
                            // and the endpoint Dirac equations.
                            for (std::size_t k = 0; k + 1 < momenta.size(); ++k)
                                sequences.push_back({momenta[k]});
                        }
                        if (length == 2) {
                            // Moving either endpoint momentum through the word produces
                            // shorter words and dot products already in the basis.
                            // Reversing the two interior momenta likewise gives their
                            // anticommutator plus the original ordering.
                            for (std::size_t k = 0; k < momenta.size(); ++k)
                                for (std::size_t m = k + 1; m < momenta.size(); ++m)
                                    sequences.push_back({momenta[k], momenta[m]});
                        }
                        for (const auto& sequence : sequences)
                            for (bool angle : {false, true}) {
                                bool right_angle = (length % 2) ? !angle : angle;
                                if ((a.masses()[i] == zero && angle != (w[i] > 0)) ||
                                    (a.masses()[j] == zero && right_angle != (w[j] > 0)))
                                    continue;
                                auto spin = [&](std::size_t leg, bool ang) {
                                    return a.call(
                                        "Spinor",
                                        {a.atom(a.masses()[leg] == zero ? "\"Helicity\"" : "\"Spin\""),
                                         a.atom(ang ? "\"Angle\"" : "\"Square\""), a.integer(leg + 1)});
                                };
                                std::vector<Id> word{spin(i, angle)};
                                for (int k : sequence)
                                    word.push_back(a.call("Mom", {a.integer(k + 1)}));
                                word.push_back(spin(j, right_angle));
                                words.push_back(a.call("SpinorChain", word));
                                generate_words(left_insertions - length);
                                words.pop_back();
                            }
                    }
                    ++left[j];
                }
            ++left[i];
        };
        generate_words(insertions);
        return shapes;
    }

  public:
    explicit catalog_builder(Algebra& arena) : a(arena), zero(a.integer(0)), one(a.integer(1)) {}
    std::vector<Id> numerator_basis(Id source, const std::vector<Id>& poles,
                                    const std::vector<Id>& model_masses, unsigned insertions) {
        // Four legs have two independent scalar invariants. Matching the
        // little-group degrees fixes the number of spinor endpoints; mass
        // dimension then fixes the remaining powers of masses and invariants.
        if (poles.size() < 2 || poles.size() > 3 || a.masses().size() != 4)
            return {};
        auto w = signature(source);
        int endpoints = 0;
        for (int n : w)
            endpoints += std::abs(n);
        if (endpoints % 2 || endpoints == 0 || endpoints > 8)
            return {};
        std::set<Id> external;
        for (auto m : a.masses())
            if (m != zero)
                external.insert(m);
        masses.insert(external.begin(), external.end());
        for (auto m : model_masses)
            if (m != zero)
                masses.insert(m);
        std::set<Id> seen;
        mass_labels(source, seen);
        std::vector<Id> raw_coeffs;
        for (auto c : coefficients(source)) {
            raw_coeffs.push_back(c);
            // Keep dimensionless interactions when identical massive external
            // wavefunctions supply compensating powers of an external mass.
            std::function<Id(Id)> strip_external = [&](Id x) {
                if (external.count(x))
                    return one;
                const auto& n = a.node(x);
                if (n.head != "Power" && n.head != "Times")
                    return x;
                std::vector<Id> children;
                for (auto arg : n.args)
                    children.push_back(strip_external(arg));
                return a.call(n.head, children);
            };
            raw_coeffs.push_back(strip_external(c));
        }
        auto shapes = numerator_shapes(w, insertions);
        std::set<Id> directions;
        // Pairs of simple propagators span the four-point trees in this basis.
        // The dimension of the common numerator is four.
        std::map<int, std::set<Id>> powers{{0, {one}}};
        auto monomials = [&](int degree) -> const std::set<Id>& {
            for (int d = 1; d <= degree; ++d)
                if (!powers.count(d)) {
                    auto& products = powers[d];
                    for (auto x : powers[d - 1])
                        for (auto m : external)
                            products.insert(a.mul({x, m}));
                    if (d >= 2)
                        for (auto x : powers[d - 2])
                            for (std::size_t j = 0; j < 2; ++j)
                                products.insert(a.mul({x, poles[j]}));
                }
            return powers.at(degree);
        };
        for (Id prefactor : raw_coeffs) {
            int missing = 4 - endpoints / 2 - int(insertions) - dimension(prefactor);
            if (missing < 0 || missing > 5)
                continue;
            for (auto extra : monomials(missing))
                for (auto shape : shapes)
                    for (std::size_t i = 0; i < poles.size(); ++i)
                        for (std::size_t j = i + 1; j < poles.size(); ++j) {
                            directions.insert(
                                a.mul({prefactor, shape, extra, a.pow(poles[i], -1), a.pow(poles[j], -1)}));
                            if (directions.size() > 3000)
                                return {};
                        }
        }
        std::vector<Id> result(directions.begin(), directions.end());
        std::stable_sort(result.begin(), result.end(), [&](Id x, Id y) { return a.cost(x) < a.cost(y); });
        return result;
    }
    contact_catalog generate(Id seed) {
        contact_catalog result;
        try {
            auto w = signature(seed), left = w;
            int endpoints = 0;
            std::vector<int> vectors;
            bool vector_only = true;
            for (std::size_t leg = 0; leg < w.size(); ++leg) {
                if (a.masses()[leg] != zero && w[leg] < 0) throw unsupported_catalog{};
                left[leg] = std::abs(w[leg]);
                endpoints += left[leg];
                if (a.masses()[leg] != zero) masses.insert(a.masses()[leg]);
                if (w[leg] == 2 && a.masses()[leg] != zero)
                    vectors.push_back(leg);
                else if (w[leg] != 0)
                    vector_only = false;
            }
            if (endpoints % 2 || endpoints > 16) return result;
            std::set<Id> seen;
            mass_labels(seed, seen);
            auto factors = coefficients(seed);
            int degree = 4 - int(w.size()) - endpoints / 2;
            Id scale = zero;
            for (Id mass : a.masses())
                if (mass != zero) {
                    scale = mass;
                    break;
                }
            if (scale == zero && !masses.empty()) scale = *masses.begin();
            std::set<Id> simple, extended;
            for (Id f : factors) {
                if (degree == 0 || scale != zero)
                    simple.insert(a.mul({strip_masses(f), degree ? a.pow(scale, degree) : one}));
                int delta = degree - dimension(f);
                if (delta == 0 || scale != zero) extended.insert(a.mul({f, delta ? a.pow(scale, delta) : one}));
            }
            auto append = [&](std::vector<Id>& target, const shapes& basis, const std::set<Id>& scalars) {
                for (Id scalar : scalars)
                    for (Id shape : basis)
                        target.push_back(a.mul({scalar, shape}));
            };
            if (vector_only && vectors.size() % 2 == 0) {
                shapes metric;
                std::vector<Id> factors;
                metric_products(vectors, factors, metric);
                append(result.polarization, metric, simple);
            }
            shapes contractions;
            std::vector<edge> edges;
            pairings(w, left, edges, contractions);
            append(result.minimal, contractions, simple);
            append(result.extended, contractions, extended);
        } catch (const unsupported_catalog&) {
        }
        return result;
    }
};
} // namespace
bool high_energy_local(const Algebra& a, Id id) {
    std::unordered_map<Id, bool> cache;
    return local(a, id, cache);
}
std::optional<std::vector<int>> external_spin_weights(Algebra& a, Id expression) {
    std::unordered_map<Id, weights> cache;
    try {
        return spin_weights(a,expression,cache);
    } catch (const unsupported_catalog&) {
        return {};
    }
}
contact_catalog high_energy_contact_catalog(Algebra& a, Id seed) { return catalog_builder(a).generate(seed); }
std::vector<Id> four_point_numerators(Algebra& a, Id source, const std::vector<Id>& poles,
                                      const std::vector<Id>& model_masses, unsigned insertions) {
    try {
        return catalog_builder(a).numerator_basis(source, poles, model_masses, insertions);
    } catch (const unsupported_catalog&) {
        return {};
    }
}
} // namespace hepcat
