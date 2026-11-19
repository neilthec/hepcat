#include "generation_feynman.hpp"

#include <algorithm>
#include <map>
#include <stdexcept>

namespace hepcat::generation {
namespace {
const std::string wl = "ConstructiveDiagrams`";
bool unphysical(const std::string& p) {
    return p.size() > 2 && p[p.size() - 2] == '.' && std::string("fcC").find(p.back()) != std::string::npos;
}
struct momentum {
    std::vector<int> labels;
    int sign;
};
struct field {
    std::string particle;
    int slot;
    momentum p;
    std::vector<Id> lorentz;
    Id color;
};
struct propagator {
    std::vector<int> labels;
    bool keep_a;
    Id p, mass, denominator;
};
struct monomial {
    Id coefficient;
    std::vector<Id> operators;
};
using polynomial = std::vector<monomial>;

// The model stores noncommuting gamma operators in a DiracWord. Keep the
// order only inside that word. All scalar and open Lorentz/color factors
// use the same interned expression arena as the constructive reducer.
class attachment {
    Algebra& a;
    const model& m;
    const std::vector<std::string>& external;
    const term& tree;
    std::vector<propagator> props;
    std::vector<std::vector<field>> incident;
    std::vector<std::vector<int>> fermions;
    std::vector<Id> rules;
    std::vector<Id> factors;
    Id one, zero;
    Id head(const std::string& name, std::vector<Id> args) { return a.call(wl + name, std::move(args)); }
    Id multiparticle(const std::vector<int>& legs) {
        std::vector<Id> words;
        for (auto n : legs)
            words.push_back(a.integer(n));
        return a.call("Multiparticle", std::move(words));
    }
    Id mom(int leg) { return a.call("Mom", {a.integer(leg)}); }
    Id slash(Id p) { return head("DiracSlash", {p}); }
    Id propagator_mass(const model_particle& p) { return a.parse(p.mass); }
    const model_particle& species(const field& f) const { return m.particle(f.particle); }
    int dimension(const field& f) const {
        const auto& p = species(f);
        return p.color == 3 && p.antiname == f.particle ? -3 : p.color;
    }
    Id color(const std::vector<field>& legs, int& phase) {
        std::vector<const field*> colored;
        for (auto& f : legs)
            if (dimension(f) != 1) colored.push_back(&f);
        if (colored.empty()) return one;
        if (colored.size() == 2) {
            auto x = dimension(*colored[0]), y = dimension(*colored[1]);
            if ((x == 8 && y == 8) || (x * y == -9))
                return head("ColorDelta", {colored[0]->color, colored[1]->color});
        }
        if (colored.size() == 3) {
            const field *fund = nullptr, *anti = nullptr, *adj = nullptr;
            bool all_adj = true;
            for (const auto* f : colored) {
                auto d = dimension(*f);
                if (d == 3) fund = f;
                if (d == -3) anti = f;
                if (d == 8)
                    adj = f;
                else
                    all_adj = false;
            }
            if (all_adj) {
                phase += 3; // CalcHEP ordered three-adjoint vertex multiplier -i.
                return head("ColorStructure", {colored[0]->color, colored[1]->color, colored[2]->color});
            }
            if (fund && anti && adj) return head("ColorGenerator", {anti->color, fund->color, adj->color});
        }
        throw std::invalid_argument("unsupported CalcHEP vertex color representation");
    }
    Id apply(Id x, const std::vector<field>& legs) {
        const auto& n = a.node(x);
        if (n.head == wl + "LorentzIndex") {
            auto& f = legs.at(a.leg(n.args.at(0)) - 1);
            return f.lorentz.at(n.args.size() == 2 ? 1 : 0);
        }
        if (n.head == wl + "LorentzScalarProduct" || n.head == "MomProd") {
            const auto& l =
                legs.at((n.head == "MomProd" ? a.leg(n.args[0]) : a.leg(a.node(n.args[0]).args.at(0))) - 1);
            const auto& r =
                legs.at((n.head == "MomProd" ? a.leg(n.args[1]) : a.leg(a.node(n.args[1]).args.at(0))) - 1);
            std::vector<Id> pieces;
            for (int i : l.p.labels)
                for (int j : r.p.labels) {
                    if (i == j) {
                        auto name = feynman_external(m, external.at(i - 1));
                        pieces.push_back(a.pow(propagator_mass(m.particle(name)), 2));
                    } else
                        pieces.push_back(
                            a.call("MomProd", {a.integer(std::min(i, j)), a.integer(std::max(i, j))}));
                }
            return a.mul({a.integer(l.p.sign * r.p.sign), a.add(std::move(pieces))});
        }
        if (n.head == wl + "LorentzMomentum" || n.head == wl + "DiracSlash") {
            auto& p = legs.at(a.leg(a.node(n.args.at(0)).args.at(0)) - 1).p;
            std::vector<Id> pieces;
            for (int leg : p.labels)
                pieces.push_back(n.head == wl + "DiracSlash"
                                     ? slash(mom(leg))
                                     : head("LorentzMomentum", {mom(leg), apply(n.args[1], legs)}));
            return a.mul({a.integer(p.sign), a.add(std::move(pieces))});
        }
        if (n.head == wl + "DiracWord") {
            if (std::none_of(legs.begin(), legs.end(), [&](const field& f) { return species(f).spin2 == 1; }))
                throw std::invalid_argument("a Feynman Dirac word requires fermion ports");
            polynomial product{{one, {}}};
            for (Id op : n.args) {
                Id applied = apply(op, legs);
                polynomial terms;
                auto append = [&](Id t, Id c) {
                    if (a.node(t).head == "Times")
                        throw std::invalid_argument("unexpected coefficient in Dirac operator");
                    terms.push_back({c, {t}});
                };
                Id coefficient = one;
                if (a.node(applied).head == "Times") {
                    auto ps = a.node(applied).args;
                    if (ps.size() != 2) throw std::logic_error("nonlinear internal slash coefficient");
                    coefficient = ps[0];
                    applied = ps[1];
                }
                if (a.node(applied).head == "Plus")
                    for (auto t : a.node(applied).args)
                        append(t, coefficient);
                else
                    append(applied, coefficient);
                product = multiply(product, terms);
            }
            std::vector<Id> words;
            for (auto& t : product)
                words.push_back(a.mul({t.coefficient, head("DiracWord", std::move(t.operators))}));
            return a.add(std::move(words));
        }
        if (!n.compound || n.head == "ModelParameter") return x;
        std::vector<Id> args;
        for (auto c : n.args)
            args.push_back(apply(c, legs));
        return a.call(n.head, std::move(args));
    }
    polynomial multiply(const polynomial& x, const polynomial& y) {
        polynomial result;
        for (const auto& l : x)
            for (const auto& r : y) {
                auto ops = l.operators;
                ops.insert(ops.end(), r.operators.begin(), r.operators.end());
                result.push_back({a.mul({l.coefficient, r.coefficient}), std::move(ops)});
            }
        return result;
    }
    polynomial ordered(Id x) {
        const auto& n = a.node(x);
        if (n.head == wl + "DiracWord") return {{one, n.args}};
        if (n.head == "Plus") {
            polynomial result;
            for (auto c : n.args) {
                auto terms = ordered(c);
                result.insert(result.end(), std::make_move_iterator(terms.begin()),
                              std::make_move_iterator(terms.end()));
            }
            return result;
        }
        if (n.head == "Times") {
            polynomial result{{one, {}}};
            for (auto c : n.args)
                result = multiply(result, ordered(c));
            return result;
        }
        return {{x, {}}};
    }
    Id endpoint(const std::string& kind, int leg) {
        auto label = feynman_external(m, external.at(leg - 1));
        const auto& p = m.particle(label);
        auto chirality = p.auxiliary == "L" || p.auxiliary == "R" ? p.auxiliary : "Dirac";
        return head(kind, {a.integer(leg), propagator_mass(p), a.atom("\"" + chirality + "\"")});
    }
    Id finish_chain(polynomial& word, int bar, int ket) {
        auto b = endpoint("FermionBar", bar), k = endpoint("FermionKet", ket);
        std::vector<Id> terms;
        for (auto& t : word)
            terms.push_back(
                a.mul({t.coefficient, head("DiracChain", {b, head("DiracWord", std::move(t.operators)), k})}));
        return a.add(std::move(terms));
    }
    const field& find_port(std::size_t v, int slot) const {
        for (const auto& f : incident.at(v))
            if (f.slot == slot) return f;
        throw std::logic_error("missing Feynman propagator endpoint");
    }

  public:
    attachment(Algebra& tree_arena, const model& model, const std::vector<std::string>& process,
               const term& tree)
        : a(tree_arena), m(model), external(process), tree(tree), one(a.integer(1)), zero(a.integer(0)) {}
    Id evaluate() {
        // Exactly one factor of i per vertex and propagator, then divide by
        // i for the amplitude. Vector/tensor propagators carry -i.
        int phase = int(tree.vertices.size()) - 1;
        for (auto& edge : tree.edges) {
            std::vector<int> complement;
            for (int l = 1; std::size_t(l) <= external.size(); ++l)
                if (!std::binary_search(edge.legs.begin(), edge.legs.end(), l)) complement.push_back(l);
            bool keep_a = edge.legs.size() < complement.size() ||
                          (edge.legs.size() == complement.size() && edge.legs < complement);
            auto legs = keep_a ? edge.legs : std::move(complement);
            Id p = a.call("Mom", {multiparticle(legs)}), mass = propagator_mass(m.particle(edge.particle_a));
            props.push_back({std::move(legs), keep_a, p, mass, a.call("PropDen", {p, mass})});
        }
        auto n = tree.vertices.size();
        incident.resize(n);
        fermions.resize(n);
        rules.resize(n);
        std::map<int, std::size_t> terminal;
        int dummy = 0;
        for (std::size_t v = 0; v < n; ++v) {
            const auto& vertex = tree.vertices[v];
            const auto& row = m.vertices()[vertex.row];
            for (std::size_t j = 0; j < row.legs.size(); ++j) {
                int slot = vertex.slots[j];
                const auto& species = m.particle(row.legs[j]);
                int rank = species.auxiliary == "T" ? 2 : (species.spin2 == 2 ? 1 : 0);
                if (species.spin2 != 0 && species.spin2 != 1 && species.spin2 != 2 && species.auxiliary != "T")
                    throw std::invalid_argument("unsupported Feynman field spin");
                field f{row.legs[j], slot, {}, {}, 0};
                for (int k = 0; k < rank; ++k)
                    f.lorentz.push_back(a.atom(wl + "Private`nativeLorentz$" + std::to_string(++dummy)));
                if (slot > 0) {
                    f.p = {{slot}, 1};
                    f.color = head("ColorIndex", {a.integer(slot)});
                    if (species.spin2 == 2) {
                        const auto& original = external.at(slot - 1);
                        Id hel = a.atom("System`Automatic");
                        if (original != row.legs[j])
                            hel = a.integer(original.back() == '+' ? 1 : original.back() == '-' ? -1 : 0);
                        factors.push_back(head("PolarizationVector",
                                               {a.integer(slot), f.lorentz[0], propagator_mass(species), hel}));
                    }
                    if (species.spin2 == 1) terminal[slot] = v;
                } else {
                    auto e = std::size_t(-1 - slot);
                    const auto& p = props.at(e);
                    // Incoming momentum at vertex a is the complementary
                    // subset. Sign follows the chosen shortest representative.
                    bool plus = (v == tree.edges[e].a) != p.keep_a;
                    f.p = {p.labels, plus ? 1 : -1};
                    f.color = a.atom(wl + "Private`nativeColor$" + std::to_string(e + 1));
                }
                if (species.spin2 == 1) fermions[v].push_back(slot);
                incident[v].push_back(std::move(f));
            }
            if (!fermions[v].empty()) {
                if (fermions[v].size() != 2) throw std::invalid_argument("unsupported Feynman Dirac vertex");
                const auto& bar = find_port(v, fermions[v][0]);
                const auto& ket = find_port(v, fermions[v][1]);
                if (species(bar).antiname != bar.particle || species(ket).name != ket.particle ||
                    species(bar).name == species(bar).antiname || species(ket).name == species(ket).antiname)
                    throw std::invalid_argument(
                        "Feynman Dirac vertex requires ordered antifermion and fermion");
            }
            rules[v] = a.mul({row.factor, color(incident[v], phase), apply(row.expression, incident[v])});
        }
        for (std::size_t i = 0; i < tree.edges.size(); ++i) {
            const auto& e = tree.edges[i];
            const auto& p = props[i];
            int slot = -1 - int(i);
            const auto& x = find_port(e.a, slot);
            const auto& y = find_port(e.b, slot);
            const auto& species = m.particle(e.particle_a);
            if (species.auxiliary == "T") {
                phase += 3;
                for (int j = 0; j < 2; ++j)
                    factors.push_back(head("LorentzMetric", {x.lorentz[j], y.lorentz[j]}));
                continue;
            }
            factors.push_back(a.pow(p.denominator, -1));
            if (species.spin2 == 2) {
                phase += 3;
                Id metric = head("LorentzMetric", {x.lorentz[0], y.lorentz[0]});
                if (p.mass != zero)
                    metric = a.add({metric, a.mul({a.integer(-1), a.pow(p.mass, -2),
                                                   head("LorentzMomentum", {p.p, x.lorentz[0]}),
                                                   head("LorentzMomentum", {p.p, y.lorentz[0]})})});
                factors.push_back(metric);
            } else
                ++phase;
        }
        std::vector<bool> visited(n);
        std::vector<int> kets;
        for (const auto& [bar, start] : terminal) {
            auto name = feynman_external(m, external[bar - 1]);
            if (m.particle(name).antiname != name) continue;
            auto v = start;
            int incoming = bar;
            polynomial word{{one, {}}};
            for (;;) {
                const auto& ps = fermions[v];
                if (visited[v] || ps.front() != incoming)
                    throw std::invalid_argument("inconsistent off-shell Dirac flow");
                visited[v] = true;
                word = multiply(word, ordered(rules[v]));
                int next = ps.back();
                if (next > 0) {
                    kets.push_back(next);
                    factors.push_back(finish_chain(word, bar, next));
                    break;
                }
                auto i = std::size_t(-1 - next);
                auto& flow = find_port(v, next).p;
                polynomial numerator{{props[i].mass, {}}};
                // The propagator numerator is slash(momentum) + mass.
                numerator.push_back({a.integer(flow.sign), {slash(props[i].p)}});
                word = multiply(word, numerator);
                const auto& e = tree.edges[i];
                v = v == e.a ? e.b : e.a;
                incoming = next;
            }
        }
        for (std::size_t i = 0; i < n; ++i)
            if (!visited[i]) {
                if (!fermions[i].empty()) throw std::invalid_argument("unpaired Feynman fermion line");
                factors.push_back(rules[i]);
            }
        for (std::size_t j = 0; j < kets.size(); ++j)
            for (std::size_t i = 0; i < j; ++i)
                if (kets[j] < kets[i]) phase += 2;
        if ((phase & 2) != 0) factors.push_back(a.integer(-1));
        if ((phase & 1) != 0) factors.push_back(a.atom("System`I"));
        return a.mul(std::move(factors));
    }
};
} // namespace

std::string feynman_external(const model& m, const std::string& name) {
    if (unphysical(name)) throw std::invalid_argument("unphysical external Feynman field: " + name);
    auto exact = std::find_if(m.particles().begin(), m.particles().end(),
                              [&](const model_particle& p) { return p.name == name || p.antiname == name; });
    if (exact != m.particles().end()) {
        if (exact->auxiliary == "T") throw std::invalid_argument("external tensor contact auxiliary: " + name);
        return name;
    }
    if (name.size() > 2 && name[name.size() - 2] == '.' &&
        std::string("+-0").find(name.back()) != std::string::npos) {
        auto base = name.substr(0, name.size() - 2);
        const auto& p = m.particle(base);
        if (p.spin2 == 2 && !(name.back() == '0' && p.mass == "0")) return base;
    }
    throw std::invalid_argument("unknown external Feynman state: " + name);
}

Id sew_feynman(Algebra& a, const model& m, const std::vector<std::string>& external, const plan& p) {
    std::vector<Id> terms;
    for (const auto& t : p.terms)
        terms.push_back(attachment(a, m, external, t).evaluate());
    return a.add(std::move(terms));
}
} // namespace hepcat::generation
