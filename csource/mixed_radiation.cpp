#include "mixed_radiation.hpp"
#include "radiation_kinematics.hpp"

namespace hepcat {
namespace {

// One Weyl component of a minimally coupled charged line. Its external
// photons i,j have the same helicity and k has the opposite helicity.
class mixed_currents : private detail::radiation_kinematics {
    const int k;
    bool photon_square(int h) const { return h == k ? !square : square; }

    // Momentum insertion from photon l in the one-photon endpoint wave h.
    Id insertion(int t, int h, int l) {
        const auto sandwich = h != k ? chain(square, h, l, {{t}}) : chain(square, l, h, {{t}});
        return over(ar.mul({chain(photon_square(h), h, l), sandwich}), {tau({t, h})});
    }
    Id connected_pair(int t, int h, int l) {
        if (photon_square(h) == photon_square(l))
            return like_helicity_pair(photon_square(h), t, h, l);
        const auto field = chain(square, h != k ? h : l, k, {{t}});
        return over(neg(ar.pow(field, 2)), {tau({t, h}), tau({t, l}), tau({t, h, l})});
    }
    Id recoil(int t, int h, int l) {
        if (photon_square(h) == photon_square(l))
            return over(neg(ar.mul({connected_pair(t, h, l), chain(!photon_square(h), h, t)})), {mass});
        return over(ar.mul({insertion(t, l, h), spin_shift(photon_square(h), t, h)}), {tau({t, h, l})});
    }
    Id ordered_scalar(int t, int i, int j) {
        const auto pair_jk =
            ar.mul({ar.add({insertion(t, i, j), insertion(t, i, k)}), connected_pair(t, j, k)});
        const auto pair_ij = ar.mul({insertion(t, k, i), connected_pair(t, i, j)});
        return over(ar.add({pair_jk, pair_ij}), {tau({t, i, j, k})});
    }
    Id ordered_spin(int t, int i, int j) {
        const auto from_j = ar.mul({ar.add({insertion(t, j, i), insertion(t, j, k)}), recoil(t, i, k)});
        const auto from_k = ar.mul({ar.add({insertion(t, k, i), insertion(t, k, j)}), recoil(t, i, j)});
        const auto pair = ar.mul({connected_pair(t, j, k), chain(square, i, t),
                                  ar.add({ar.integer(1), over(tau({t, j, k}), {tau({t, i})})})});
        return over(ar.add({from_j, from_k, pair, ar.mul({chain(square, i, j), recoil(t, j, k)})}),
                    {tau({t, i, j, k})});
    }

public:
    mixed_currents(Algebra& a, Id m, bool sq, bool phys, int minority)
        : radiation_kinematics(a, m, sq, phys), k(minority) {}
    Id exchange(int a, int b, int c, int d, int i, int j) {
        const auto zero = ar.integer(0);
        const auto fi = dipole(square, a, b, i), fj = dipole(square, a, b, j), fk = dipole(!square, a, b, k);
        const auto v = spin_shift(square, a, i), w = spin_shift(!square, b, k);
        const auto pair = ordered_pair(a, b, i, j, fi, fj, connected_pair(a, i, j), connected_pair(b, i, j));
        const auto shift = ar.add({ar.mul({fj, v}), recoil(a, i, j)});

        // Contract |y>[x| with the bare or once-radiating second line.
        auto dot = [&](int y, int x, int h) {
            const auto yc = chain(!square, y, c), yd = chain(!square, y, d);
            const auto xc = chain(square, x, c), xd = chain(square, x, d);
            const auto bare = ar.add({ar.mul({yd, xc}), ar.mul({yc, xd})});
            if (!h)
                return bare;
            const auto shift_c = spin_shift(photon_square(h), c, h),
                       shift_d = spin_shift(photon_square(h), d, h);
            const auto spin =
                h == k ? ar.mul({chain(!square, y, h), minus(ar.mul({xd, shift_c}), ar.mul({xc, shift_d}))})
                       : ar.mul({chain(square, x, h), minus(ar.mul({yd, shift_c}), ar.mul({yc, shift_d}))});
            return ar.add({ar.mul({dipole(photon_square(h), c, d, h), bare}), spin});
        };
        auto project = [&](int y, Id bare, Id shifted, int h = 0) {
            return ar.add({ar.mul({bare, dot(y, a, h)}), ar.mul({shifted, dot(y, i, h)})});
        };

        const auto pair_ik = ar.add({connected_pair(a, i, k), connected_pair(b, i, k)});
        const auto scalar =
            ar.add({ar.mul({fj, pair_ik}), ordered_scalar(a, i, j), neg(ordered_scalar(b, i, j))});
        const auto spin_i = ar.add(
            {ar.mul({v, connected_pair(b, j, k)}), ar.mul({recoil(a, i, k), fj}), ordered_spin(a, i, j)});
        const auto recoil_ki = recoil(b, k, i);
        const auto recoil_kj = recoil(b, k, j);
        const auto contact = ar.mul({w, connected_pair(b, i, j), pole({i, k}, zero)});
        const auto backspin = ar.mul({ar.add({insertion(b, j, i), insertion(b, j, k)}), recoil_ki});
        const auto spin_k =
            ar.add({ar.mul({fj, recoil_ki}), over(minus(contact, backspin), {tau({b, i, j, k})})});

        // Three disjoint exchange topologies after summing i,j and applying
        // the independent Fermi signs and the simultaneous line reversal.
        const auto triple =
            ar.add({project(b, ar.add({ar.mul({fk, pair}), scalar}), ar.add({ar.mul({fk, shift}), spin_i})),
                    project(k, minus(spin_k, ar.mul({w, pair})),
                            minus(ar.mul({v, recoil_kj}), ar.mul({w, shift})))});
        const auto equal_pair = project(b, pair, shift, k);
        const auto mixed_pair = ar.add(
            {project(b, ar.add({ar.mul({fi, fk}), pair_ik}), ar.add({ar.mul({fk, v}), recoil(a, i, k)}), j),
             project(k, minus(recoil_ki, ar.mul({w, fi})), neg(ar.mul({w, v})), j)});
        return ar.add({over(triple, {pole({c, d}, zero)}), over(equal_pair, {pole({c, d, k}, zero)}),
                       over(mixed_pair, {pole({c, d, j}, zero)})});
    }
};
} // namespace

Id mixed_radiation_kernel(Algebra& ar, int a, int b, int c, int d, int i, int j, int k, Id mass,
                          bool majority_square, bool physical) {
    return mixed_currents(ar, mass, majority_square, physical, k).exchange(a, b, c, d, i, j);
}
} // namespace hepcat
