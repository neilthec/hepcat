#include "generation_sew.hpp"
#include "model_orbit.hpp"
#include "generation_current.hpp"
#include "generation_spin.hpp"
#include <algorithm>
#include <array>
#include <limits>
#include <numeric>
#include <set>

namespace hepcat::generation {
namespace {
using ids = std::vector<Id>;
bool is_x(const std::string& h) { return h == "ConstructiveDiagrams`xFactor" || h == "xFactor"; }
bool is_xt(const std::string& h) { return h == "ConstructiveDiagrams`xTildeFactor" || h == "xTildeFactor"; }
std::string short_name(const std::string& h) { return h.substr(h.find_last_of('`') + 1); }
struct labels {
    Id left, right, aligned, propagator, mass;
    bool massive;
    int spin;
};
class sewing {
    Algebra& a;
    const model& mdl;
    const std::vector<std::string>& process;
    const term& tree;
    bool channel, cyclic, physical;
    bool stored_reference = false;
    std::optional<std::size_t> chiral_vertex;
    unsigned chiral_kind = 0;
    Id zero, one, angle, square, spin, helicity, xi, upper, lower;
    std::vector<Id> mass;
    std::vector<int> light;
    std::vector<labels> edges;
    size_t next_reference = 10;
    Id group(const std::vector<int>& legs) {
        ids out;
        for (int leg : legs)
            out.push_back(a.integer(leg));
        return a.call("Multiparticle", std::move(out));
    }
    Id mom(Id label) { return a.call("Mom", {label}); }
    Id external_spinor(Id leg, Id kind, bool ang) {
        return a.call("Spinor", {kind, ang ? angle : square, leg});
    }
    Id den(const labels& e) { return a.call("PropDen", {mom(e.propagator), e.mass}); }
    Id raw_label(size_t v, int slot) {
        if (slot > 0) return a.integer(slot);
        auto i = size_t(-slot - 1);
        return v == tree.edges[i].a ? edges[i].right : edges[i].left;
    }
    Id slot_label(size_t v, Id label) {
        if (int i = a.leg(label)) return raw_label(v, tree.vertices[v].slots.at(i - 1));
        auto& n = a.node(label);
        if (n.head != "Multiparticle")
            throw std::invalid_argument("invalid particle label in model row: " + n.head);
        ids parts;
        for (Id x : n.args) {
            Id y = slot_label(v, x);
            const auto& p = a.node(y);
            if (p.head == "Multiparticle")
                parts.insert(parts.end(), p.args.begin(), p.args.end());
            else
                parts.push_back(y);
        }
        std::sort(parts.begin(), parts.end(), [&](Id x, Id y) { return a.leg(x) < a.leg(y); });
        return a.call("Multiparticle", std::move(parts));
    }
    Id mass_of(Id label) {
        if (int i = a.leg(label)) return mass.at(i - 1);
        for (const auto& e : edges)
            if (label == e.left || label == e.right) return e.mass;
        throw std::invalid_argument("no mass assigned to composite model label " + a.format(label));
    }
    std::pair<Id, int> endpoint(size_t v, const Node& node) {
        auto args = node.args;
        if (args.size() != 3 && args.size() != 5 && args.size() != 4)
            throw std::invalid_argument("unsupported spinor endpoint metadata");
        size_t j = args.size() == 5 ? 3 : 2;
        if (args[0] == xi) {
            stored_reference = true;
            return {a.call("Spinor", std::move(args)), 1};
        }
        int leg = tree.vertices[v].slots.at(a.leg(args[j]) - 1);
        bool vector_slot = false;
        if (args.size() == 4) {
            const auto& tag = a.node(args[3]);
            vector_slot = short_name(tag.head) == "VectorSpinIndex" && tag.args.size() == 2 &&
                          tag.args[0] == args[j] && (a.leg(tag.args[1]) == 1 || a.leg(tag.args[1]) == 2);
            if (vector_slot && leg > 0) args[3] = a.call(tag.head, {a.integer(leg), tag.args[1]});
        }
        if (leg > 0) {
            args[j] = a.integer(leg);
            return {a.call("Spinor", std::move(args)), 1};
        }
        size_t i = size_t(-leg - 1);
        bool crossing = raw_label(v, leg) != edges[i].aligned, ang = args[j - 1] == angle;
        if (edges[i].massive) {
            if (args.size() != 3 && !(vector_slot && edges[i].spin == 2))
                throw std::invalid_argument(
                    "unsupported fixed little-group index on an internal massive spin");
            return {a.call("Spinor", {spin, v == tree.edges[i].upper ? upper : lower, args[1],
                                      edges[i].aligned, a.integer(i + 1)}),
                    crossing && ang ? -1 : 1};
        }
        args[j] = edges[i].aligned;
        return {a.call("Spinor", std::move(args)), crossing && ang ? -1 : 1};
    }
    Id kinematic(size_t v, Id id, std::unordered_map<Id, Id>& cache) {
        if (auto it = cache.find(id); it != cache.end()) return it->second;
        const auto& n = a.node(id);
        Id out = id;
        if (n.head == "SpinorChain") {
            ids word;
            int sign = 1;
            for (size_t j = 0; j < n.args.size(); ++j) {
                auto part = n.args[j];
                const auto& p = a.node(part);
                if (j == 0 || j + 1 == n.args.size()) {
                    if (p.head != "Spinor")
                        throw std::invalid_argument("unsupported endpoint in spinor chain: " + p.head);
                    auto value = endpoint(v, p);
                    word.push_back(value.first);
                    sign *= value.second;
                } else {
                    auto m = kinematic(v, part, cache);
                    const auto& c = a.node(m);
                    if (c.head == "Times" && c.args.size() == 2 && c.args.front() == a.integer(-1)) {
                        sign = -sign;
                        m = c.args.back();
                    }
                    word.push_back(m);
                }
            }
            out = a.mul({a.integer(sign), a.call(n.head, std::move(word))});
        } else if ((is_x(n.head) || is_xt(n.head)) && n.args.size() == 2 &&
                   tree.vertices[v].slots.size() == 3) {
            // An x factor carries the helicity of the third leg implicitly.
            // Expose that spinor before sewing a cut against an explicit
            // higher-point helicity amplitude. Crossing Q -> -Q flips x.
            const auto& slots = tree.vertices[v].slots;
            int i = a.leg(n.args[0]), j = a.leg(n.args[1]), qslot = 6 - i - j;
            if (i < 1 || i > 3 || j < 1 || j > 3 || i == j)
                throw std::invalid_argument("unsupported particle labels in cubic x factor");
            int port = slots.at(qslot - 1);
            Id source = raw_label(v, slots.at(j - 1));
            if (port < 0) {
                const auto& edge = edges.at(size_t(-port - 1));
                if (edge.massive)
                    throw std::invalid_argument("a cubic x factor requires a massless third leg");
                bool ang = is_x(n.head);
                int sign = raw_label(v, port) == edge.aligned ? 1 : -1;
                ids prefix;
                if (!chiral_vertex && cyclic && !light.empty()) {
                    int probe = light.front();
                    prefix = {external_spinor(a.integer(probe), helicity, ang)};
                } else
                    prefix = {external_spinor(a.atom("\"CutPhoton\""), xi, ang)};
                ids num = prefix, denom = prefix;
                num.push_back(mom(source));
                num.push_back(external_spinor(edge.aligned, helicity, !ang));
                denom.push_back(external_spinor(edge.aligned, helicity, ang));
                out =
                    a.mul({a.integer(sign), a.call("SpinorChain", std::move(num)),
                           a.pow(mass_of(source), -1), a.pow(a.call("SpinorChain", std::move(denom)), -1)});
            } else
                out = a.call(n.head, {raw_label(v, slots.at(i - 1)), source});
        } else if (n.head == "Mass") {
            out = mass_of(slot_label(v, n.args.at(0)));
        } else if (n.head == "Mom" || n.head == "MomProd" || n.head == "Mandelstahm" || is_x(n.head) ||
                   is_xt(n.head)) {
            ids args;
            int sign = 1;
            for (Id slot : n.args) {
                Id label = slot_label(v, slot);
                if (channel && v == tree.edges[0].a && label == edges[0].right && !is_x(n.head) &&
                    !is_xt(n.head)) {
                    label = edges[0].aligned;
                    if (n.head == "Mom" || n.head == "MomProd") sign = -sign;
                }
                args.push_back(label);
            }
            out = a.mul({a.integer(sign), a.call(n.head, std::move(args))});
        } else if (short_name(n.head).rfind("Color", 0) == 0) {
            out = relabel_color(a, mdl, tree, v, id, process);
        } else if (n.compound && n.head != "ModelParameter") {
            ids args;
            for (Id child : n.args)
                args.push_back(kinematic(v, child, cache));
            out = a.call(n.head, std::move(args));
        }
        cache.emplace(id, out);
        return out;
    }
    Id vertex_value(size_t v) {
        const auto& row = mdl.vertices().at(tree.vertices[v].row);
        std::unordered_map<Id, Id> cache;
        stored_reference = false;
        Id kinematics = kinematic(v, model_detail::expand_leg_orbit(a, row.expression), cache);
        if (chiral_vertex && *chiral_vertex == v)
            kinematics = a.call("NullChannelBranch", {group(light), chiral_kind == 1 ? angle : square, kinematics});
        Id expr = a.mul({kinematic(v, row.factor, cache), kinematics, vertex_color(a, mdl, tree, v, process)});
        return cyclic && stored_reference ? replace_stored_references(expr) : expr;
    }
    Id leg_spinor(size_t v, size_t slot, bool ang, int& coefficient) {
        const auto& vert = tree.vertices[v];
        Id sp = external_spinor(
            a.integer(slot + 1),
            mdl.particle(mdl.vertices()[vert.row].legs[slot]).mass == "0" ? helicity : spin, ang);
        auto [x, s] = endpoint(v, a.node(sp));
        coefficient *= s;
        return x;
    }
    // A complete pair x<ab>, xt[ab] on a transverse spin-one exchange
    // gives a local four-spinor current. Its other ports may be internal.
    bool standard_current(size_t v, size_t edge_no) {
        const auto& vert = tree.vertices[v];
        const auto& row = mdl.vertices()[vert.row];
        if (vert.slots.size() != 3 || vert.slots[2] != -int(edge_no + 1)) return false;
        for (int i : {0, 1})
            if (mdl.particle(row.legs[i]).spin2 != 1 ||
                mdl.particle(row.legs[i]).mass != mdl.particle(row.legs[0]).mass)
                return false;
        const auto& n = a.node(row.expression);
        if (n.head != "Times" || n.args.size() != 2) return false;
        bool x = false, bracket = false, as = false, bs = false;
        for (Id child : n.args) {
            const auto& y = a.node(child);
            if ((is_x(y.head) || is_xt(y.head)) && y.args == ids{a.integer(1), a.integer(2)}) {
                x = true;
                as = is_x(y.head);
            } else if (y.head == "SpinorChain" && y.args.size() == 2) {
                const auto& l = a.node(y.args.front());
                const auto& r = a.node(y.args.back());
                if (l.head == "Spinor" && r.head == "Spinor" && l.args.size() == 3 && r.args.size() == 3 &&
                    l.args[2] == a.integer(1) && r.args[2] == a.integer(2) && l.args[1] == r.args[1]) {
                    bracket = true;
                    bs = l.args[1] == angle;
                }
            }
        }
        return x && bracket && as == bs;
    }
    Id fermion_current(size_t e) {
        const auto& edge = tree.edges[e];
        ids pieces;
        for (int i : {0, 1})
            for (int j : {0, 1}) {
                int sign = 1;
                auto l = leg_spinor(edge.a, i, true, sign), r = leg_spinor(edge.b, j, true, sign);
                auto ls = leg_spinor(edge.a, 1 - i, false, sign),
                     rs = leg_spinor(edge.b, 1 - j, false, sign);
                pieces.push_back(a.mul(
                    {a.integer(sign), a.call("SpinorChain", {l, r}), a.call("SpinorChain", {ls, rs})}));
            }
        ids values{a.add(std::move(pieces))};
        for (size_t v : {edge.a, edge.b}) {
            const auto& row = mdl.vertices()[tree.vertices[v].row];
            values.push_back(row.factor);
            values.push_back(vertex_color(a, mdl, tree, v, process));
        }
        return a.mul(std::move(values));
    }
    ids indices(Id label) const {
        const auto& n = a.node(label);
        return n.head == "Multiparticle" ? n.args : ids{label};
    }
    int missing_photon(const ids& labels) {
        std::vector<bool> used(mass.size());
        for (Id id : labels)
            for (Id n : indices(id))
                used.at(a.leg(n) - 1) = true;
        int missing = 0;
        for (size_t i = 0; i < used.size(); ++i)
            if (!used[i]) {
                if (missing) return 0;
                missing = int(i + 1);
            }
        return missing;
    }
    // On three-particle kinematics 2 q.p_i = m_j^2 - m_i^2 for a null q.
    // Equal spectator masses make both possible momentum reference insertions
    // identically singular, even away from any internal factorization pole.
    bool degenerate_three_point_reference(int photon) const {
        if (mass.size() != 3 || mass.at(photon - 1) != zero) return false;
        return mass[photon % 3] == mass[(photon + 1) % 3];
    }
    int physical_spectator(int photon) {
        // Try massive momenta first. A massless momentum is also a valid
        // reference insertion when its two-leg invariant is generic here.
        for (bool massive : {true, false})
            for (size_t i = 0; i < mass.size(); ++i) {
                if ((mass[i] != zero) != massive || int(i + 1) == photon) continue;
                std::vector<int> pair{int(i + 1), photon};
                std::sort(pair.begin(), pair.end());
                auto label = group(pair);
                if (std::none_of(edges.begin(), edges.end(),
                                 [&](const auto& e) { return label == e.left || label == e.right; }))
                    return int(i + 1);
            }
        throw std::invalid_argument("no nondegenerate physical reference insertion for leg " +
                                    std::to_string(photon));
    }
    Id replace_stored_references(Id expr) {
        // A stored lower-point amplitude may have an explicit Xi in several
        // chains. Infer its external photon from the normalization bracket,
        // then use the same spin frame at every occurrence in this vertex.
        std::map<Id, std::set<int>> owners;
        std::set<std::pair<Id, bool>> visited;
        auto endpoint_ref = [&](Id id) {
            const auto& n = a.node(id);
            return n.head == "Spinor" && n.args.size() == 3 && n.args[0] == xi;
        };
        auto inspect = [&](auto& self, Id id, bool denom) -> void {
            if (!visited.emplace(id, denom).second) return;
            const auto& n = a.node(id);
            if (endpoint_ref(id)) {
                owners[n.args[2]];
                return;
            }
            if (denom && n.head == "SpinorChain" && n.args.size() == 2) {
                for (size_t j = 0; j < 2; ++j)
                    if (endpoint_ref(n.args[j])) {
                        const auto& other = a.node(n.args[1 - j]);
                        if (other.head == "Spinor" && other.args.size() == 3 && other.args[0] == helicity) {
                            int leg = a.leg(other.args[2]);
                            if (leg) owners[a.node(n.args[j]).args[2]].insert(leg);
                        }
                    }
            }
            if (n.head == "Power") {
                mpq_class p;
                self(self, n.args[0], a.number(n.args[1], p) && p < 0 ? !denom : denom);
            } else
                for (Id child : n.args)
                    self(self, child, denom);
        };
        inspect(inspect, expr, false);
        if (owners.empty()) return expr;
        // A stored Xi may be the only nonsingular description of a bare vertex.
        if (!light.empty() && degenerate_three_point_reference(light.front())) return expr;
        struct replacement {
            int spinor, insert;
        };
        std::map<Id, replacement> choices;
        for (const auto& [tag, associated] : owners) {
            if (light.empty())
                throw std::invalid_argument("no external helicity spinor for a stored reference");
            int owner = associated.empty() ? light.front() : *associated.begin();
            auto first = std::find(light.begin(), light.end(), owner);
            if (first == light.end())
                throw std::invalid_argument("reference normalization refers to a massive leg");
            int ref = 0;
            if (!physical && light.size() > 1) {
                size_t start = size_t(first - light.begin());
                for (size_t i = 1; i < light.size(); ++i)
                    if (!associated.count(light[(start + i) % light.size()])) {
                        ref = light[(start + i) % light.size()];
                        break;
                    }
            }
            choices[tag] = ref ? replacement{ref, 0} : replacement{owner, physical_spectator(owner)};
        }
        std::unordered_map<Id, Id> cache;
        auto rewrite = [&](auto& self, Id id) -> Id {
            if (auto it = cache.find(id); it != cache.end()) return it->second;
            const auto& n = a.node(id);
            Id out = id;
            if (n.head == "SpinorChain") {
                ids w;
                int sign = 1;
                for (size_t i = 0; i < n.args.size(); ++i) {
                    Id child = n.args[i];
                    if (!endpoint_ref(child)) {
                        w.push_back(child);
                        continue;
                    }
                    const auto& sp = a.node(child);
                    const auto r = choices.at(sp.args[2]);
                    bool ang = sp.args[1] == angle;
                    bool right = i + 1 == n.args.size();
                    if (r.insert && right) {
                        w.push_back(mom(a.integer(r.insert)));
                        sign = -sign;
                    }
                    w.push_back(external_spinor(a.integer(r.spinor), helicity, r.insert ? !ang : ang));
                    if (r.insert && !right) w.push_back(mom(a.integer(r.insert)));
                }
                out = a.mul({a.integer(sign), a.call(n.head, std::move(w))});
            } else if (endpoint_ref(id))
                throw std::invalid_argument("a reference spinor must be an endpoint of a chain");
            else if (n.compound) {
                ids args;
                for (Id child : n.args)
                    args.push_back(self(self, child));
                out = a.call(n.head, std::move(args));
            }
            cache.emplace(id, out);
            return out;
        };
        return rewrite(rewrite, expr);
    }
    Id external_x(Id id, std::unordered_map<Id, Id>& cache) {
        if (auto it = cache.find(id); it != cache.end()) return it->second;
        const auto& n = a.node(id);
        Id out = id;
        if ((is_x(n.head) || is_xt(n.head)) && n.args.size() == 2) {
            int photon = missing_photon(n.args);
            if (photon) {
                if (mass[photon - 1] != zero)
                    throw std::invalid_argument("the missing leg of an x factor must be massless");
                if (cyclic && mass_of(n.args[1]) != zero && degenerate_three_point_reference(photon))
                    return cache.emplace(id, id).first->second;
                bool ang = is_x(n.head);
                auto q = a.integer(photon);
                Id reference = external_spinor(a.integer(next_reference++), xi, ang);
                ids prefix{reference};
                if (cyclic) {
                    auto it = std::find(light.begin(), light.end(), photon);
                    if (light.size() > 1 && !physical)
                        prefix = {external_spinor(a.integer(light[(it - light.begin() + 1) % light.size()]),
                                                  helicity, ang)};
                    else
                        prefix = {external_spinor(q, helicity, !ang),
                                  mom(a.integer(physical_spectator(photon)))};
                }
                auto numerator = prefix, denominator = prefix;
                numerator.push_back(mom(n.args[1]));
                numerator.push_back(external_spinor(q, helicity, !ang));
                denominator.push_back(external_spinor(q, helicity, ang));
                out = a.mul({a.call("SpinorChain", std::move(numerator)), a.pow(mass_of(n.args[1]), -1),
                             a.pow(a.call("SpinorChain", std::move(denominator)), -1)});
            }
        } else if (n.compound && n.head != "ModelParameter") {
            ids args;
            for (Id x : n.args)
                args.push_back(external_x(x, cache));
            out = a.call(n.head, std::move(args));
        }
        cache.emplace(id, out);
        return out;
    }

  public:
    sewing(Algebra& arena, const model& m, const std::vector<std::string>& p, const term& t, bool ch,
           bool refs, bool phys)
        : a(arena), mdl(m), process(p), tree(t), channel(ch), cyclic(refs || phys), physical(phys),
          zero(a.integer(0)), one(a.integer(1)), angle(a.atom("\"Angle\"")), square(a.atom("\"Square\"")),
          spin(a.atom("\"Spin\"")), helicity(a.atom("\"Helicity\"")), xi(a.atom("\"Xi\"")),
          upper(a.atom("\"Upper\"")), lower(a.atom("\"Lower\"")) {
        for (size_t i = 0; i < p.size(); ++i) {
            mass.push_back(a.parse(m.particle(p[i]).mass));
            if (mass.back() == zero) light.push_back(i + 1);
        }
        for (const auto& edge : t.edges) {
            std::vector<int> other;
            for (size_t i = 0; i < mass.size(); ++i)
                if (!std::binary_search(edge.legs.begin(), edge.legs.end(), int(i + 1)))
                    other.push_back(i + 1);
            Id left = group(edge.legs), right = group(other);
            auto& particle = m.particle(edge.particle_a);
            bool left_first = edge.legs.front() == 1;
            Id aligned =
                ch || edge.legs.size() < other.size() || (edge.legs.size() == other.size() && left_first)
                    ? left
                    : right;
            edges.push_back({left, right, aligned, (ch || left_first) ? left : right,
                             a.parse(particle.mass), particle.mass != "0", particle.spin2});
        }
    }
    std::vector<size_t> currents() {
        std::vector<size_t> out;
        for (size_t i = 0; i < edges.size(); ++i)
            if (!edges[i].massive && edges[i].spin == 2 && standard_current(tree.edges[i].a, i) &&
                standard_current(tree.edges[i].b, i))
                out.push_back(i);
        return out;
    }
    int chirality(std::size_t v) {
        const auto& row = mdl.vertices()[tree.vertices[v].row];
        if (row.legs.size() != 3) return 0;
        for (const auto& leg : row.legs) if (mdl.particle(leg).mass != "0") return 0;
        int kinds = 0;
        std::set<Id> seen;
        auto visit = [&](auto& self, Id id) -> void {
            if (kinds == 3 || !seen.insert(id).second) return;
            const auto& n = a.node(id);
            if (n.head == "Spinor") {
                if (n.args.size() != 3 || n.args[0] != helicity) kinds = 3;
                else kinds |= n.args[1] == angle ? 1 : 2;
            } else if (n.head == "Mom" || n.head == "MomProd" || is_x(n.head) || is_xt(n.head))
                kinds = 3;
            else for (Id child : n.args) self(self,child);
        };
        visit(visit,row.expression);
        return kinds == 3 ? 0 : kinds;
    }
    std::pair<int, int> branch() {
        if (process.size() != 4 || tree.vertices.size() != 2 || edges.size() != 1 || edges.front().massive)
            return {};
        auto left = chirality(0), right = chirality(1);
        if (light.size() == 4) return {left, right};
        if (channel && light.size() == 2 && (left || right)) {
            // On the massless three-particle side only the corresponding
            // chiral branch contributes. The cubic vertex on the massive
            // side has generic spinor references on both branches.
            chiral_vertex = left ? 0 : 1;
            chiral_kind = left ? left : right;
        }
        return {};
    }
    Id run(const std::vector<size_t>& paired) {
        ids factors;
        std::vector<bool> skip(tree.vertices.size());
        int sign = tree.sign;
        if (!channel && tree.edges.size() % 2)
            sign = -sign; // i per vertex and propagator; strip the overall i
        for (size_t i : paired) {
            auto& e = tree.edges[i];
            skip[e.a] = skip[e.b] = true;
            factors.push_back(fermion_current(i));
        }
        for (size_t v = 0; v < tree.vertices.size(); ++v)
            if (!skip[v]) factors.push_back(vertex_value(v));
        for (auto& e : edges)
            factors.push_back(a.pow(den(e), -1));
        if (channel) factors.push_back(a.pow(a.atom("ConstructiveDiagrams`z"), -1));
        factors.push_back(a.integer(sign));
        Id out = a.mul(std::move(factors));
        for (const auto& e : edges) {
            if (e.massive && e.spin > 0)
                out = sew_spin(a, out, e.aligned, e.mass, e.spin);
            else if (!e.massive && e.spin > 0 && !chiral_vertex)
                out = sew_null_spin(a, out, e.aligned, light, cyclic);
        }
        std::unordered_map<Id, Id> cache;
        return external_x(out, cache);
    }
};
} // namespace
Id sew(Algebra& a, const model& m, const std::vector<std::string>& p, const plan& row, bool channel,
       bool cyclic, bool phys) {
    for (const auto& tree : row.terms)
        for (const auto& vertex : tree.vertices)
            if (m.vertices().at(vertex.row).pending_constructive_derivation)
                throw std::domain_error("Sewing requires a lower amplitude pending a constructive derivation");
    ids expressions;
    std::vector<std::pair<int, int>> branches;
    for (size_t i = 0; i < row.terms.size(); ++i) {
        const auto& term = row.terms[i];
        sewing work(a, m, p, term, channel, cyclic, phys);
        auto [left, right] = work.branch();
        if (left && left == right) continue;
        auto paired = work.currents();
        // Combining the two helicities requires equal couplings and the
        // same assignment at every other vertex. Consume only actual partners.
        if (!paired.empty() && paired.size() < std::numeric_limits<size_t>::digits &&
            row.terms.size() == (size_t(1) << paired.size())) {
            bool matching = true;
            std::set<std::vector<std::string>> helicity_assignments;
            for (auto& alternative : row.terms) {
                if (alternative.sign != term.sign || alternative.vertices.size() != term.vertices.size() ||
                    alternative.edges.size() != term.edges.size()) {
                    matching = false;
                    break;
                }
                std::vector<std::string> assignment;
                for (auto e : paired)
                    assignment.push_back(alternative.edges[e].particle_a);
                if (!helicity_assignments.insert(std::move(assignment)).second) {
                    matching = false;
                    break;
                }
                for (size_t v = 0; v < term.vertices.size(); ++v) {
                    auto& first = term.vertices[v];
                    auto& second = alternative.vertices[v];
                    if (first.slots != second.slots ||
                        m.vertices()[first.row].factor != m.vertices()[second.row].factor)
                        matching = false;
                    if (first.row != second.row && std::none_of(paired.begin(), paired.end(), [&](auto e) {
                            return v == term.edges[e].a || v == term.edges[e].b;
                        }))
                        matching = false;
                }
                if (sewing(a, m, p, alternative, channel, cyclic, phys).currents() != paired)
                    matching = false;
            }
            if (matching) return work.run(paired);
        }
        expressions.push_back(work.run({}));
        branches.emplace_back(left, right);
    }
    if (channel && branches.size() == 2 && branches[0].first && branches[0].second &&
        branches[0].first == branches[1].second && branches[0].second == branches[1].first)
        if (auto common = common_four_null_continuation(a, expressions[0], expressions[1])) return *common;
    return a.add(std::move(expressions));
}
} // namespace hepcat::generation
