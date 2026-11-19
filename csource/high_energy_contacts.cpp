#include "high_energy_contacts.hpp"
#include <algorithm>
#include <functional>
#include <set>
#include <stdexcept>
#include <unordered_map>

namespace hepcat {
namespace {
bool kinematic_head(const std::string& h) {
    return h == "Spinor" || h == "SpinorChain" || h == "SpinorTrace" || h == "Mom" || h == "MomProd" ||
           h == "Mandelstahm" || h == "PropDen" || h == "Mass";
}
bool scalar(const Algebra& a, Id id) {
    const auto& n = a.node(id);
    if (!n.compound || n.head == "ModelParameter" || n.head == "Rational" || n.head == "System`Complex")
        return true;
    if (n.head != "Plus" && n.head != "Times" && n.head != "Power") return false;
    return std::all_of(n.args.begin(), n.args.end(), [&](Id x) { return scalar(a, x); });
}
bool local(const Algebra& a, Id id, std::unordered_map<Id, bool>& cache) {
    if (auto p = cache.find(id); p != cache.end()) return p->second;
    const auto& n = a.node(id);
    bool ok = scalar(a, id);
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
class catalog_builder {
    Algebra& a;
    const Id zero, one;
    std::set<Id> masses;
    std::unordered_map<Id, weights> signatures;
    std::unordered_map<Id, std::set<Id>> scalar_parts;
    std::unordered_map<Id, int> dimensions;
    static constexpr std::size_t max_shapes = 512;

    // Coherent massive spin degree and massless little-group weight.
    weights signature(Id id) {
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
                w[leg] += a.masses()[leg] == zero && a.node(spin.args[1]).head == "\"Square\"" ? -1 : 1;
            }
        } else if (n.head == "Plus" || n.head == "Times") {
            bool first = true;
            for (Id p : n.args) {
                auto child = signature(p);
                if (n.head == "Times")
                    for (std::size_t i = 0; i < w.size(); ++i)
                        w[i] += child[i];
                else if (first)
                    w = child;
                else if (w != child)
                    throw unsupported_catalog{};
                first = false;
            }
        } else if (n.head == "Power" && n.args.size() == 2) {
            auto base = signature(n.args[0]);
            mpq_class q;
            if (std::any_of(base.begin(), base.end(), [](int x) { return x != 0; })) {
                if (!a.number(n.args[1], q) || q.get_den() != 1 || abs(q) > 32) throw unsupported_catalog{};
                for (std::size_t i = 0; i < w.size(); ++i)
                    w[i] = base[i] * q.get_num().get_si();
            }
        }
        signatures.emplace(id, w);
        return w;
    }

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
            c.insert(scalar(a, id) ? id : one);
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

  public:
    explicit catalog_builder(Algebra& arena) : a(arena), zero(a.integer(0)), one(a.integer(1)) {}
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
contact_catalog high_energy_contact_catalog(Algebra& a, Id seed) { return catalog_builder(a).generate(seed); }
} // namespace hepcat
