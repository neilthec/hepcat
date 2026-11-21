#include "chiral_radiation.hpp"
#include "radiation_kinematics.hpp"
#include <functional>
#include <stdexcept>

namespace hepcat {
namespace {
// An active covector is [t| or <t|P. Parity interchanges both chiralities.
struct covector {
    int leg;
    std::vector<int> momentum{};
};
using projection = std::function<Id(const covector&)>;

class chiral_currents : private detail::radiation_kinematics {
    // One Weyl orientation of the conserved current on line (a,b).
    // Photon permutations complete its ordered representative. The other
    // orientation is supplied by reversing both charged lines.
    Id oriented(int a, int b, const legs& photons, const projection& project) {
        const int i = photons[0], j = photons[1];
        const Id fi = dipole(square, a, b, i);
        const Id ca = like_helicity_pair(square, a, i, j), cb = like_helicity_pair(square, b, i, j);
        const Id pair = ordered_pair(a, b, i, j, fi, dipole(square, a, b, j), ca, cb);
        const Id recoil = over(neg(ar.mul({ca, chain(!square, i, a), project({i})})), {mass});
        auto spin = [&](int v) { return spin_shift(square, a, v); };
        if (photons.size() == 2)
            return ar.add({ar.mul({pair, project({a})}), ar.mul({fi, spin(j), project({j})}), recoil});

        const int k = photons[2];
        const auto fk = dipole(square, a, b, k);
        auto recoil_factor = [&](int t) {
            return over(ar.mul({chain(square, k, i), chain(square, k, i, {{t}})}),
                        {tau({t, k}), tau({t, i, j, k})});
        };
        const auto scalar = ar.add(
            {over(ar.mul({fk, ar.add({pair, ca, cb})}), {ar.integer(3)}), neg(ar.mul({cb, recoil_factor(b)}))});
        // The endpoint Dirac equation joins the bare square spinor and
        // the sum of its three spin shifts into <a|(p_a+p_i+p_j+p_k).
        return ar.add({ar.mul({scalar, project({a})}), ar.mul({pair, spin(k), project({k})}),
                       ar.mul({fk, recoil}),
                       over(ar.mul({ca, recoil_factor(a), project({a, {a, i, j, k}})}), {mass})});
    }
    Id bracket(const covector& x, int end) {
        return x.momentum.empty() ? chain(square, x.leg, end) : chain(!square, x.leg, end, {x.momentum});
    }

public:
    chiral_currents(Algebra& algebra, Id mass, bool sq, bool phys)
        : radiation_kinematics(algebra, mass, sq, phys) {}
    Id exchange(int a, int b, int c, int d, const legs& photons) {
        const int i = photons[0], j = photons[1], k = photons[2];
        const auto bd = chain(!square, b, d), bc = chain(!square, b, c);
        auto bare = [&](const covector& x) {
            return ar.add({ar.mul({bd, bracket(x, c)}), ar.mul({bc, bracket(x, d)})});
        };
        const auto spin = ar.add({over(ar.mul({bd, chain(square, k, c)}), {tau({c, k})}),
                                  neg(over(ar.mul({bc, chain(square, k, d)}), {tau({d, k})}))});
        const auto fk = dipole(square, c, d, k);
        auto radiating = [&](const covector& x) {
            return ar.add({ar.mul({fk, bare(x)}), ar.mul({spin, bracket(x, k)})});
        };
        const auto zero = ar.integer(0);
        return ar.add({over(oriented(a, b, photons, bare), {pole({c, d}, zero)}),
                       over(oriented(a, b, {i, j}, radiating), {pole({c, d, k}, zero)})});
    }
};
} // namespace

Id chiral_radiation_kernel(Algebra& a, int l, int r, int u, int v, const std::vector<int>& photons, Id mass,
                           bool square, bool physical) {
    if (photons.size() != 3)
        throw std::invalid_argument("Three photon labels required");
    return chiral_currents(a, mass, square, physical).exchange(l, r, u, v, photons);
}
} // namespace hepcat
