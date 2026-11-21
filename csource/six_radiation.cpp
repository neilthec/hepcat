#include "six_radiation.hpp"
#include "radiation_kinematics.hpp"

namespace hepcat {
namespace {

class exchange_radiation : private detail::radiation_kinematics {
    const int a, b, x, y, c, d, k;
    const legs q;
    Id active(int l, int r) { return chain(square, l, r); }
    Id passive(int l, int r) { return chain(!square, l, r); }
    Id sum(std::initializer_list<Id> terms) { return ar.add(terms); }
    Id product(Id u, Id v) { return ar.mul({u, v}); }

    // Propagate the right current into a test spinor of the given chirality.
    // Keep one Weyl orientation of the spectator vector current. The inner
    // exchange c,d restores the second orientation throughout the seed.
    Id vertex(int l, bool active_end) {
        // P=-(p_c+p_d+p_y). Move p_y through the conserved cd current.
        // The endpoint Dirac equations and Schouten leave one slash per term.
        const bool chirality = active_end ? square : !square;
        const auto inserted = active_end ? chain(square, l, c, {{d}}) : chain(square, c, l, {{d}});
        const auto mass_and_slash = minus(product(mass, chain(chirality, l, c)), inserted);
        return minus(product(chain(chirality, y, d), mass_and_slash),
                     product(chain(chirality, y, l), chain(square, d, c, {{y}})));
    }

    // The remaining bare polarization contact after moving the middle
    // slash through epsilon. The physical reference is p_x |k].
    Id contact() {
        const auto lk = product(passive(b, x), active(a, k));
        const auto rk = product(passive(y, d), active(k, c));
        const auto lq = product(active(b, x), chain(square, k, a, {{x}}));
        const auto rq = product(active(y, d), chain(square, k, c, {{x}}));
        return over(minus(product(lk, rq), product(lq, rk)), {ar.integer(2), tau({x, k})});
    }

public:
    exchange_radiation(Algebra& ar, const legs& e, const legs& f, int h, Id m, bool sq, bool phys)
        : radiation_kinematics(ar, m, sq, phys), a(e.at(0)), b(f.at(0)), x(e.at(1)), y(f.at(1)),
          c(e.at(2)), d(f.at(2)), k(h), q{a, b, x} {}

    six_radiation_terms kernel() {
        const auto zero = ar.integer(0), two = ar.integer(2);
        const auto dq = tau(q), dp = tau({c, d, y}), spectator = pole({c, d}, zero);
        const auto ua = vertex(a, false), ub = vertex(b, false), va = vertex(a, true), vk = vertex(k, true);

        // The orientation of L has [bx]|a> and <bx>|a]. D_a and D_x act
        // on the active Weyl spinor. For D_a the crossed orientation of L
        // also contributes whenever a is the spinor contracted with x.
        const auto bare = sum({product(active(b, x), ua), product(passive(b, x), va)});
        const auto spin_a = sum({product(active(k, x), ub), product(passive(b, x), vk)});
        const auto spin_x = product(active(b, k), ua);
        const auto spin_open = ar.mul({active(k, a), passive(b, x), vk});
        const auto leaf = sum({product(dipole(square, a, b, k), bare), product(spin_shift(square, a, k), spin_a)});

        Id center;
        if (physical) {
            // In the physical reference p_x|k], anticommuting the polarization
            // through the propagator leaves an open spin shift and a contact.
            const auto orbital = over(sum({dipole(square, x, y, k),
                                           over(chain(square, k, k, {{y}, {c, d}}), {tau({y, k}), dq})}), {two});
            center = sum({product(orbital, bare), product(spin_shift(square, x, k), spin_x),
                          over(spin_open, {two, dq}), contact()});
        } else {
            // The reference Q|k] sets epsilon.Q=0. Partial fractions in
            // w=D(Q+k)-D(Q) put one fermion propagator on each ordered half.
            const auto w = chain(square, k, k, {q});
            const auto orbital = over(neg(chain(square, k, k, {{a, b}, {x}})), {tau({x, k}), w});
            center = sum({product(orbital, bare), product(spin_shift(square, x, k), spin_x),
                          neg(over(spin_open, {w}))});
        }
        return {over(leaf, {pole({a, b, k}, zero), spectator, dp}), over(center, {pole({a, b}, zero), spectator, dp})};
    }
};
}

six_radiation_terms six_radiation_kernel(Algebra& ar, const std::vector<int>& particles, const std::vector<int>& antiparticles,
                        int photon, Id mass, bool square, bool physical) {
    return exchange_radiation(ar, particles, antiparticles, photon, mass, square, physical).kernel();
}

} // namespace hepcat
