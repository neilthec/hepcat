// Ordered star and path numerators in two-component spinors. The input pairs
// are (1,2), (3,4), (5,6), (7,8); antisymmetrization of the result is external.
#pragma once
#include "permutation_sum.hpp"
#include <algorithm>
#include <array>
#include <optional>
#include <vector>
namespace eight_point_weyl {
using namespace hepcat;
class charged_line {
    Algebra &a;
    Id m;
    Id spin(int i, bool square) {
        if (i < 0) {
            // Two fixed little-group columns of the first massive leg resolve
            // the identity in spinor space during the spin-summed replay.
            return a.call("Spinor", {a.atom("\"Spin\""), a.atom("\"Upper\""),
                                     a.atom(square ? "\"Square\"" : "\"Angle\""), a.integer(1),
                                     a.exact(mpq_class(i == -1 ? -1 : 1, 2))});
        }
        return a.call("Spinor",
                      {a.atom("\"Spin\""), a.atom(square ? "\"Square\"" : "\"Angle\""), a.integer(i)});
    }
    Id momentum(std::initializer_list<int> legs) {
        std::vector<Id> labels;
        for (auto i : legs)
            labels.push_back(a.integer(i));
        std::sort(labels.begin(), labels.end(), [&](Id l, Id r) { return a.leg(l) < a.leg(r); });
        return a.call("Mom", {a.call("Multiparticle", labels)});
    }
    Id bracket(bool sq, int i, int j) { return a.call("SpinorChain", {spin(i, sq), spin(j, sq)}); }
    Id neg(Id x) { return a.mul({a.integer(-1), x}); }
    struct component {
        Id factor;
        int end;
        std::optional<Id> insertion;
    };
    using wave = std::vector<component>;
    wave bare(int i) { return {{a.integer(1), i, {}}}; }
    wave bra(bool sq, int b, int c, int d) {
        const Id q = momentum({b, c, d}), db = bracket(false, d, b), bc = bracket(true, b, c);
        if (sq)
            return {{neg(bc), d, q}, {a.mul({m, db}), c, {}}};
        return {{db, c, q}, {neg(a.mul({m, bc})), d, {}}};
    }
    wave ket(bool sq, int t, int c, int d) {
        const Id q = momentum({t, c, d}), dt = bracket(false, d, t), ct = bracket(true, c, t);
        if (sq)
            return {{ct, d, q}, {neg(a.mul({m, dt})), c, {}}};
        return {{dt, c, q}, {neg(a.mul({m, ct})), d, {}}};
    }
    Id join(bool sq, const wave &x, const wave &y) {
        std::vector<Id> terms;
        for (const auto &l : x)
            for (const auto &r : y) {
                std::vector<Id> chain{spin(l.end, sq != l.insertion.has_value())};
                if (l.insertion)
                    chain.push_back(*l.insertion);
                if (r.insertion)
                    chain.push_back(*r.insertion);
                chain.push_back(spin(r.end, sq != r.insertion.has_value()));
                terms.push_back(a.mul({l.factor, r.factor, a.call("SpinorChain", chain)}));
            }
        return a.add(terms);
    }
    Id inv(std::initializer_list<int> side, Id mass) {
        return a.pow(a.call("PropDen", {momentum(side), mass}), -1);
    }
    // Contract one dressed endpoint with an external spinor. Its two leaf
    // orientations share the same photon and fermion propagators.
    Id leaf(bool square, bool left, int end, int c, int d, int other) {
        auto orientation = [&](int particle, int antiparticle) {
            if (left)
                return join(square, bra(square, end, particle, antiparticle), bare(other));
            return join(square, bare(other), ket(square, end, particle, antiparticle));
        };
        return a.mul(
            {inv({c, d}, a.integer(0)), inv({end, c, d}, m), a.add({orientation(c, d), orientation(d, c)})});
    }
    Id star_end(bool square, bool left, int end, int c, int d, int other) {
        const auto first = leaf(square, left, end, c, d, other);
        const auto second =
            left ? leaf(square, left, d, c, end, other) : leaf(square, left, c, end, d, other);
        return a.add({first, neg(second)});
    }
    Id star_partition(int t, int b, int c, int d, int u, int v, int f, int g) {
        auto middle = [&](int i, int j) {
            return a.add({a.mul({star_end(false, false, t, f, g, j), star_end(true, true, b, c, d, i)}),
                          a.mul({star_end(false, true, b, c, d, j), star_end(true, false, t, f, g, i)})});
        };
        return a.mul({inv({u, v}, a.integer(0)), a.add({middle(u, v), middle(v, u)})});
    }
    // Four-fermion current on one side of the middle photon pole.
    // angle_axis and square_axis label the two open spinor components.
    Id path_end(bool left, int e, int b, int c, int d, int angle_axis, int square_axis) {
        auto order = [&](int t, int u, int particle, int antiparticle) {
            if (left)
                return a.add({a.mul({bracket(false, t, angle_axis),
                                     leaf(true, true, u, particle, antiparticle, square_axis)}),
                              a.mul({leaf(false, true, u, particle, antiparticle, angle_axis),
                                     bracket(true, t, square_axis)})});
            return a.add({a.mul({bracket(false, angle_axis, u),
                                 leaf(true, false, t, particle, antiparticle, square_axis)}),
                          a.mul({leaf(false, false, t, particle, antiparticle, angle_axis),
                                 bracket(true, square_axis, u)})});
        };
        auto reversed = [&](int t, int u, int particle, int antiparticle) {
            return a.add({order(t, u, particle, antiparticle), neg(order(u, t, particle, antiparticle))});
        };
        return a.add({reversed(e, b, c, d), neg(reversed(c, b, e, d)), neg(reversed(e, d, c, b)),
                      reversed(c, d, e, b)});
    }
    static int parity(const std::array<int, 4> &legs) {
        int sign = 1;
        for (size_t i = 0; i < legs.size(); ++i)
            for (size_t j = i + 1; j < legs.size(); ++j)
                if (legs[i] > legs[j])
                    sign = -sign;
        return sign;
    }

  public:
    charged_line(Algebra &arena, Id mass) : a(arena), m(mass) {}
    Id star() {
        auto term = a.add(
            {a.mul({join(false, bare(6), ket(false, 1, 7, 8)), join(true, bra(true, 2, 3, 4), bare(5))}),
             a.mul({join(false, bra(false, 2, 3, 4), bare(6)), join(true, bare(5), ket(true, 1, 7, 8))})});
        const Id zero = a.integer(0);
        auto k = a.mul({a.integer(8), term, inv({3, 4}, zero), inv({5, 6}, zero), inv({7, 8}, zero),
                        inv({2, 3, 4}, m), inv({1, 7, 8}, m)});
        return apply_exchanges(a, k, {{{{3, 4}}, 1}, {{{5, 6}}, 1}, {{{7, 8}}, 1}});
    }
    Id path() {
        const auto la = bra(false, 4, 1, 2), ls = bra(true, 4, 1, 2), ra = ket(false, 5, 7, 8),
                   rs = ket(true, 5, 7, 8);
        auto term = a.add({a.mul({bracket(false, 6, 3), join(true, ls, rs)}),
                           neg(a.mul({join(false, la, bare(6)), join(true, bare(3), rs)})),
                           neg(a.mul({join(false, bare(3), ra), join(true, ls, bare(6))})),
                           a.mul({join(false, la, ra), bracket(true, 6, 3)})});
        const Id zero = a.integer(0);
        auto k = a.mul({a.integer(8), term, inv({1, 2}, zero), inv({1, 2, 3, 4}, zero), inv({7, 8}, zero),
                        inv({1, 2, 4}, m), inv({5, 7, 8}, m)});
        return apply_exchanges(a, k, {{{{1, 2}}, 1}, {{{7, 8}}, 1}, {{{3, 4}}, -1}, {{{5, 6}}, -1}});
    }
    // Distribute the two alternating sums over the two sides of each cut.
    // On the path, resolve each cross-cut bracket by Schouten in the two
    // fixed columns. This lets the same four-leg spin table serve every
    // assignment on the opposite side.
    Id grouped() {
        std::vector<Id> stars, paths;
        std::array<int, 4> electrons{1, 3, 5, 7}, positrons{2, 4, 6, 8};
        do {
            const auto [t, c, u, f] = electrons;
            do {
                const auto [b, d, v, g] = positrons;
                const auto sign = a.integer(parity(electrons) * parity(positrons));
                if (b < d && t < f)
                    stars.push_back(a.mul({sign, star_partition(t, b, c, d, u, v, f, g)}));
                if (t > c || b > d || u > f || v > g)
                    continue;
                std::vector<Id> numerator;
                for (int h : {-1, -2})
                    for (int k : {-1, -2}) {
                        auto l = path_end(true, c, d, t, b, h, k);
                        auto r = path_end(false, u, v, f, g, -3 - h, -3 - k);
                        numerator.push_back(a.mul({a.integer(h == k ? 1 : -1), l, r}));
                    }
                paths.push_back(a.mul({sign, inv({t, b, c, d}, a.integer(0)), a.add(numerator)}));
            } while (std::next_permutation(positrons.begin(), positrons.end()));
        } while (std::next_permutation(electrons.begin(), electrons.end()));
        const Id determinant = a.mul({bracket(false, -1, -2), bracket(true, -1, -2)});
        return a.mul({a.integer(8), a.add({a.add(stars), a.mul({a.exact(mpq_class(-1, 2)),
                                                                a.pow(determinant, -1), a.add(paths)})})});
    }
};
} // namespace eight_point_weyl
