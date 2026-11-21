#pragma once
#include "reduce.hpp"
#include <algorithm>
#include <initializer_list>
#include <utility>

namespace hepcat::detail {

// Common spinor and pole conventions for the two oriented charged-line currents.
class radiation_kinematics {
protected:
    using legs = std::vector<int>;
    Algebra& ar;
    const Id mass;
    const bool square, physical; // Chirality of the like-helicity pair and choice of poles.

    radiation_kinematics(Algebra& algebra, Id mass, bool square, bool physical)
        : ar(algebra), mass(mass), square(square), physical(physical) {}

    Id neg(Id value) { return ar.mul({ar.integer(-1), value}); }
    Id minus(Id left, Id right) { return ar.add({left, neg(right)}); }
    Id over(Id value, std::initializer_list<Id> denominators) {
        return ar.mul({value, ar.pow(ar.mul(denominators), -1)});
    }
    Id momentum(legs labels) {
        if (labels.size() == 1)
            return ar.call("Mom", {ar.integer(labels.front())});
        std::sort(labels.begin(), labels.end());
        std::vector<Id> ids;
        for (int leg : labels)
            ids.push_back(ar.integer(leg));
        return ar.call("Mom", {ar.call("Multiparticle", std::move(ids))});
    }
    Id spinor(int leg, bool sq) {
        const bool massless = ar.masses().at(leg - 1) == ar.integer(0);
        return ar.call("Spinor", {ar.atom(massless ? "\"Helicity\"" : "\"Spin\""),
                                  ar.atom(sq ? "\"Square\"" : "\"Angle\""), ar.integer(leg)});
    }
    // Each insertion is a single momentum matrix. Its label list can be a sum.
    Id chain(bool sq, int left, int right, std::initializer_list<legs> insertions = {}) {
        if (insertions.size() == 0 && left == right)
            return ar.integer(0);
        std::vector<Id> word{spinor(left, sq)};
        for (const auto& p : insertions)
            word.push_back(momentum(p));
        word.push_back(spinor(right, insertions.size() % 2 == 0 ? sq : !sq));
        return ar.call("SpinorChain", std::move(word));
    }
    Id pole(legs labels, Id internal_mass) {
        return ar.call("PropDen", {momentum(std::move(labels)), internal_mass});
    }
    Id tau(legs labels) { return pole(std::move(labels), mass); }
    Id dipole(bool sq, int a, int b, int h) {
        return over(neg(chain(sq, h, h, {{b}, {a}})), {tau({a, h}), tau({b, h})});
    }
    Id spin_shift(bool sq, int t, int h) { return over(chain(sq, h, t), {tau({t, h})}); }
    Id like_helicity_pair(bool sq, int t, int i, int j) {
        return over(neg(ar.pow(ar.mul({mass, chain(sq, i, j)}), 2)),
                    {tau({t, i}), tau({t, j}), tau({t, i, j})});
    }
    Id ordered_pair(int a, int b, int i, int j, Id fi, Id fj, Id ca, Id cb) {
        if (physical)
            return over(ar.add({ar.mul({fi, fj}), ca, cb}), {ar.integer(2)});
        // Schouten expresses the two dipoles as an ordered determinant. The
        // i,j exchange cancels its reference bracket and restores the physical pair.
        const auto cross = over(chain(square, i, j, {{a}, {b}}), {tau({a, i}), tau({b, j})});
        const auto diagonal = ar.add({over(ar.integer(1), {tau({a, i}), tau({a, i, j})}),
                                      over(ar.integer(1), {tau({b, i}), tau({b, i, j})})});
        return over(minus(cross, ar.mul({ar.pow(mass, 2), chain(square, i, j), diagonal})),
                    {chain(!square, i, j)});
    }
};

} // namespace hepcat::detail
