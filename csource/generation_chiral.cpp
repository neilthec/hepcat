#include "generation_current.hpp"
#include "residue.hpp"
#include "validation_color_polynomial.hpp"
#include <algorithm>
#include <array>
#include <set>

namespace hepcat::generation {
namespace {
struct chiral_term {
    int angle = 0, square = 0;
    Id coupling;
};

// Recognize the minimal chiral current <f V>[g V] in a cubic vertex,
// retaining its coefficient and the antisymmetry of both brackets.
std::vector<chiral_term> current(Algebra &a, const vertex &v, const model_vertex &row) {
    const auto &n = a.node(row.expression);
    std::vector<chiral_term> terms;
    for (Id term : n.head == "Plus" ? n.args : std::vector<Id>{row.expression}) {
        std::vector<Id> factors;
        const auto &monomial = a.node(term);
        chiral_term item{};
        for (Id factor : monomial.head == "Times" ? monomial.args : std::vector<Id>{term}) {
            const auto &b = a.node(factor);
            if (b.head != "SpinorChain") {
                if (b.compound && b.head != "ModelParameter" && b.head != "Rational")
                    return {};
                mpq_class scalar;
                if (!b.compound && !a.number(factor, scalar))
                    return {};
                factors.push_back(factor);
                continue;
            }
            if (b.args.size() != 2)
                return {};
            const auto &l = a.node(b.args.front());
            const auto &r = a.node(b.args.back());
            if (l.head != "Spinor" || r.head != "Spinor" || l.args.size() != 3 || r.args.size() != 3 ||
                l.args[1] != r.args[1])
                return {};
            const int i = a.leg(l.args.back()), j = a.leg(r.args.back());
            if (i < 1 || j < 1 || i > 3 || j > 3)
                return {};
            const int p = v.slots[i - 1], q = v.slots[j - 1];
            if ((p < 0) == (q < 0))
                return {};
            auto chirality = a.node(l.args[1]).head;
            if (chirality != "\"Angle\"" && chirality != "\"Square\"")
                return {};
            int &external = chirality == "\"Angle\"" ? item.angle : item.square;
            if (external)
                return {};
            external = std::max(p, q);
            if (p < 0)
                factors.push_back(a.integer(-1));
        }
        if (!item.angle || !item.square)
            return {};
        item.coupling = a.mul(std::move(factors));
        terms.push_back(item);
    }
    return terms;
}
} // namespace

std::optional<Id> massive_vector_fermion_exchange(Algebra &a, const model &m,
                                                  const std::vector<std::string> &names, const plan &p) {
    if (names.size() != 4 || p.terms.size() != 1)
        return {};
    const auto &t = p.terms[0];
    if (t.edges.size() != 1 || t.vertices.size() != 2)
        return {};
    const auto &e = t.edges.front();
    const auto &mediator = m.particle(e.particle_a);
    if (mediator.spin2 != 2 || mediator.mass == "0" ||
        std::count_if(names.begin(), names.end(), [&](const auto &n) { return m.particle(n).spin2 == 1; }) < 2)
        return {};
    std::array<std::vector<chiral_term>, 2> currents;
    std::vector<Id> factors{a.integer(t.sign)};
    for (std::size_t i = 0; i != 2; ++i) {
        const auto &v = t.vertices[i];
        const auto &row = m.vertices()[v.row];
        validation_detail::color_polynomial color(a);
        if (v.slots.size() != 3 || color.contains_color(row.factor) || color.contains_color(row.expression))
            return {};
        currents[i] = current(a, v, row);
        if (currents[i].empty())
            return {};
        // Each minimal current has a fixed pair of external endpoints. The
        // only alternative allowed in its chiral sum interchanges those endpoints.
        const auto first = currents[i].front();
        for (const auto &term : currents[i])
            if (std::minmax(term.angle, term.square) != std::minmax(first.angle, first.square))
                return {};
        factors.push_back(row.factor);
        factors.push_back(vertex_color(a, m, t, i, names));
    }
    auto mass = [&](int leg) { return a.parse(m.particle(names[leg - 1]).mass); };
    auto spin = [&](int leg, bool square) {
        return a.call("Spinor", {a.atom(mass(leg) == a.integer(0) ? "\"Helicity\"" : "\"Spin\""),
                                 a.atom(square ? "\"Square\"" : "\"Angle\""), a.integer(leg)});
    };
    auto bracket = [&](int i, int j, bool square) {
        return a.call("SpinorChain", {spin(i, square), spin(j, square)});
    };
    auto divergence = [&](const std::vector<chiral_term> &terms, const vertex &vtx) {
        const int x = terms.front().angle, y = terms.front().square;
        if (x == y) {
            // A vector-scalar-vector vertex carries both current spinors on
            // the external vector. Its own momentum annihilates this sandwich.
            auto other =
                std::find_if(vtx.slots.begin(), vtx.slots.end(), [&](int leg) { return leg > 0 && leg != x; });
            std::vector<Id> couplings;
            for (const auto &v : terms)
                couplings.push_back(v.coupling);
            return a.mul(
                {a.add(couplings),
                 a.call("SpinorChain", {spin(x, true), a.call("Mom", {a.integer(*other)}), spin(x, false)})});
        }
        std::vector<Id> left, right;
        for (const auto &v : terms)
            (v.angle == x ? left : right).push_back(v.coupling);
        const Id l = a.add(left), r = a.add(right);
        const auto negative = [&](Id term) { return a.mul({a.integer(-1), term}); };
        // For equal masses the divergence carries the axial coupling alone.

        if (mass(x) == mass(y)) {
            Id axial = a.add({l, negative(r)});
            return a.mul({mass(x), axial, a.add({bracket(x, y, true), negative(bracket(x, y, false))})});
        }
        return a.add(
            {a.mul({a.add({a.mul({r, mass(x)}), negative(a.mul({l, mass(y)}))}), bracket(x, y, false)}),
             a.mul({a.add({a.mul({l, mass(x)}), negative(a.mul({r, mass(y)}))}), bracket(x, y, true)})});
    };
    std::vector<Id> transverse;
    for (const auto &l : currents[0])
        for (const auto &r : currents[1])
            transverse.push_back(a.mul(
                {l.coupling, r.coupling, bracket(l.angle, r.angle, false), bracket(l.square, r.square, true)}));
    Id mass_v = a.parse(mediator.mass);
    // The symmetric massive-spin projector is the metric contraction plus the product
    // of the two divergences. Only the external fermion equations enter this continuation.
    factors.push_back(a.add({a.mul({a.integer(-1), a.pow(mass_v, 2), a.add(std::move(transverse))}),
                             a.mul({a.exact(mpq_class(1, 2)), divergence(currents[0], t.vertices[0]),
                                    divergence(currents[1], t.vertices[1])})}));
    std::vector<Id> channel;
    for (int leg : e.legs)
        channel.push_back(a.integer(leg));
    factors.push_back(
        a.pow(a.call("PropDen", {a.call("Mom", {a.call("Multiparticle", channel)}), mass_v}), -1));
    return a.mul(std::move(factors));
}
// Promote the conserved three-vector current to a current with open Lorentz
// index. Determine its normalization from the original cubic sewing on the
// exchange pole. The cubic interaction can have any overall coupling.
std::optional<Id> covariant_fermion_vector_exchange(Algebra &a, const model &m,
                                                    const std::vector<std::string> &names, const plan &p,
                                                    Id raw) {
    if (names.size() != 4 || p.terms.size() != 1)
        return {};
    const auto &t = p.terms.front();
    if (t.edges.size() != 1 || t.vertices.size() != 2)
        return {};
    const auto &e = t.edges.front();
    const auto &mediator = m.particle(e.particle_a);
    if (mediator.spin2 != 2 || mediator.mass == "0")
        return {};
    std::vector<Id> masses;
    std::array<int, 2> bosons{};
    std::size_t count = 0;
    for (std::size_t i = 0; i < names.size(); ++i) {
        const auto &leg = m.particle(names[i]);
        if (leg.spin2 == 2 && leg.mass != "0" && count < 2)
            bosons[count++] = i + 1;
        else if (leg.spin2 != 1)
            return {};
        masses.push_back(a.parse(leg.mass));
    }
    if (count != 2)
        return {};
    const int u = bosons[0], v = bosons[1];
    const auto &ff = t.vertices[std::find(t.vertices[0].slots.begin(), t.vertices[0].slots.end(), u) ==
                                        t.vertices[0].slots.end()
                                    ? 0
                                    : 1];
    if (std::find(ff.slots.begin(), ff.slots.end(), v) != ff.slots.end())
        return {};
    auto fs = current(a, ff, m.vertices()[ff.row]);
    if (fs.empty())
        return {};
    validation_detail::color_polynomial colors(a);
    for (const auto &vertex : t.vertices) {
        const auto &entry = m.vertices()[vertex.row];
        if (colors.contains_color(entry.factor) || colors.contains_color(entry.expression))
            return {};
    }
    auto parts = colors.split(raw);
    if (parts.empty())
        return {};
    const auto tensors = parts[0].tensors;
    std::vector<Id> source_terms;
    for (const auto &part : parts) {
        if (part.tensors != tensors)
            return {};
        source_terms.push_back(a.mul(part.coefficients));
    }
    a.setKinematics(masses, {});
    const Id zero = a.integer(0), neg = a.integer(-1), two = a.integer(2), mass = a.parse(mediator.mass);
    auto spin = [&](int i, bool sq) {
        return a.call("Spinor", {a.atom(masses[i - 1] == zero ? "\"Helicity\"" : "\"Spin\""),
                                 a.atom(sq ? "\"Square\"" : "\"Angle\""), a.integer(i)});
    };
    auto b = [&](int i, int j, bool sq) { return a.call("SpinorChain", {spin(i, sq), spin(j, sq)}); };
    auto sand = [&](int y, int k, int x) {
        return a.call("SpinorChain", {spin(y, true), a.call("Mom", {a.integer(k)}), spin(x, false)});
    };
    auto sub = [&](Id x, Id y) { return a.add({x, a.mul({neg, y})}); };
    Id uv = a.mul({b(u, v, false), b(u, v, true)});
    std::vector<Id> transfer, dipole, longitudinal, schouten_u, schouten_v;
    for (const auto &j : fs) {
        int x = j.angle, y = j.square;
        transfer.push_back(a.mul({j.coupling, uv, sub(sand(y, u, x), sand(y, v, x))}));
        dipole.push_back(a.mul({two, j.coupling,
                                sub(a.mul({sand(u, v, u), b(x, v, false), b(y, v, true)}),
                                    a.mul({sand(v, u, v), b(x, u, false), b(y, u, true)}))}));
        auto divergence = sub(a.mul({masses[x - 1], b(x, y, true)}), a.mul({masses[y - 1], b(x, y, false)}));
        // Use p_u-p_v = 2 p_u + p_x + p_y and Schouten on the square
        // endpoints, then on the angle endpoints. Both newly adjacent p_u
        // insertions reduce by the external Dirac equation.
        auto schouten = [&](int i, int k) {
            auto contracted = sub(a.mul({b(i, k, false), b(y, k, true), sand(i, i, x)}),
                                  a.mul({b(y, i, true), b(x, k, false), sand(k, i, i)}));
            return a.mul({j.coupling, a.add({a.mul({uv, divergence}), a.mul({two, contracted}),
                                             a.mul({two, b(x, k, false), b(y, k, true), sand(i, k, i)})})});
        };
        schouten_u.push_back(schouten(u, v));
        schouten_v.push_back(a.mul({neg, schouten(v, u)}));
        longitudinal.push_back(a.mul({j.coupling, uv, divergence,
                                      sub(a.pow(masses[u - 1], 2), a.pow(masses[v - 1], 2)), a.pow(mass, -2)}));
    }
    const Id vv = a.add(dipole), momentum = a.add(transfer), div = a.add(longitudinal);
    std::vector<Id> cut_legs;
    for (int l : e.legs)
        cut_legs.push_back(a.integer(l));
    const numerator_shell cut{e.legs, mediator.mass};
    const Id prop = a.call("PropDen", {a.call("Mom", {a.call("Multiparticle", cut_legs)}), mass});
    const Id coupling = a.mul(
        {m.vertices()[t.vertices[0].row].factor, m.vertices()[t.vertices[1].row].factor, a.pow(prop, -1)});
    const Id source = reduceInto(a, a.add(source_terms), masses, {});
    // Dividing the massive spin sums leaves a degree-zero combination of
    // masses. Infer the mass ratio along with the constant from independent cuts.
    std::set<Id> unique;
    std::vector<Id> directions;
    std::set<Id> scales;
    for (Id numerator : {mass, masses[u - 1], masses[v - 1]})
        for (Id denominator : {mass, masses[u - 1], masses[v - 1]})
            scales.insert(a.mul({coupling, numerator, a.pow(denominator, -1)}));
    for (int magnetic_sign : {1, -1})
        for (int longitudinal_sign : {-1, 1}) {
            Id metric = a.add({momentum, a.mul({a.integer(magnetic_sign), vv})});
            std::vector<Id> choices{metric};
            if (magnetic_sign == 1) {
                choices.push_back(a.add(schouten_u));
                choices.push_back(a.add(schouten_v));
            }
            Id shortest = zero;
            std::pair<std::size_t, std::size_t> best{SIZE_MAX, SIZE_MAX};
            for (Id value : choices) {
                auto numerator =
                    reduceInto(a, a.add({value, a.mul({a.integer(longitudinal_sign), div})}), masses, {});
                auto rank = std::make_pair(expression_complexity(a, numerator), a.cost(numerator));
                if (rank < best) {
                    best = rank;
                    shortest = numerator;
                }
            }
            for (Id scale : scales) {
                Id dir = a.mul({scale, shortest});
                if (dir != zero && unique.insert(dir).second)
                    directions.push_back(dir);
            }
        }
    std::vector<Id> columns{source};
    columns.insert(columns.end(), directions.begin(), directions.end());
    std::vector<std::optional<mpq_class>> ratios(directions.size());
    std::vector<bool> eligible(directions.size(), true);
    try {
        for (unsigned k = 0; k < 8; ++k) {
            auto values = residue_components(a, cut, columns, 4 * 17 + k, {});
            for (std::size_t i = 0; i < directions.size(); ++i) {
                if (!eligible[i])
                    continue;
                for (std::size_t j = 0; j < 4; ++j) {
                    const auto &x = values[0][j];
                    const auto &y = values[i + 1][j];
                    if (y == 0) {
                        if (x != 0)
                            eligible[i] = false;
                    } else if (!ratios[i])
                        ratios[i] = mpq_class(x / y);
                    else if (x != *ratios[i] * y)
                        eligible[i] = false;
                }
            }
            if (std::none_of(eligible.begin(), eligible.end(), [](bool x) { return x; }))
                return {};
        }
    } catch (const residue_unsupported &) {
        return {};
    } catch (const residue_singular &) {
        return {};
    }
    for (std::size_t i = 0; i < directions.size(); ++i)
        if (eligible[i] && ratios[i] && *ratios[i] != 0) {
            auto factors = tensors;
            factors.push_back(a.exact(*ratios[i]));
            factors.push_back(directions[i]);
            return a.mul(factors);
        }
    return {};
}
} // namespace hepcat::generation
