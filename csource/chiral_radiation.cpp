#include "chiral_radiation.hpp"
#include <algorithm>
#include <functional>
#include <stdexcept>

namespace hepcat {
namespace {
using legs = std::vector<int>;

// An active covector is [t| or <t|P. Parity interchanges both chiralities.
struct covector {
    int leg;
    legs momentum{};
};
using projection = std::function<Id(const covector&)>;

class chiral_currents {
    Algebra& ar;
    Id m;
    bool square, physical;

    Id neg(Id x) { return ar.mul({ar.integer(-1), x}); }
    Id over(Id x, Id den) { return ar.mul({x, ar.pow(den, -1)}); }
    Id momentum(legs labels) {
        std::sort(labels.begin(), labels.end());
        if (labels.size() == 1)
            return ar.call("Mom", {ar.integer(labels.front())});
        std::vector<Id> words;
        for (auto i : labels)
            words.push_back(ar.integer(i));
        return ar.call("Mom", {ar.call("Multiparticle", words)});
    }
    Id spin(int t, bool sq) {
        const bool photon = ar.masses().at(t - 1) == ar.integer(0);
        return ar.call("Spinor", {ar.atom(photon ? "\"Helicity\"" : "\"Spin\""),
                                  ar.atom(sq ? "\"Square\"" : "\"Angle\""), ar.integer(t)});
    }
    Id chain(bool sq, int left, int right, std::initializer_list<legs> insertions = {}) {
        if (insertions.size() == 0 && left == right)
            return ar.integer(0);
        std::vector<Id> word{spin(left, sq)};
        for (const auto& p : insertions)
            word.push_back(momentum(p));
        const bool end_square = insertions.size() % 2 == 0 ? sq : !sq;
        word.push_back(spin(right, end_square));
        return ar.call("SpinorChain", word);
    }
    Id pole(legs labels, Id mass) { return ar.call("PropDen", {momentum(std::move(labels)), mass}); }
    Id tau(legs labels) { return pole(std::move(labels), m); }
    Id dipole(int a, int b, int i) {
        return over(neg(chain(square, i, i, {{b}, {a}})), ar.mul({tau({a, i}), tau({b, i})}));
    }
    Id determinant(int t, int i, int j) {
        return over(neg(ar.pow(ar.mul({m, chain(square, i, j)}), 2)),
                    ar.mul({tau({t, i}), tau({t, j}), tau({t, i, j})}));
    }
    Id scalar_pair(int a, int b, int i, int j, Id fi, Id fj, Id ca, Id cb) {
        if (physical)
            return over(ar.add({ar.mul({fi, fj}), ca, cb}), ar.integer(2));
        // Schouten writes the product of two dipoles as a determinant.
        // Exchange i and j in one cross term; tau_tij-tau_ti-tau_tj=s_ij
        // combines the diagonal determinants with ca and cb.
        const auto cross = over(chain(square, i, j, {{a}, {b}}), ar.mul({tau({a, i}), tau({b, j})}));
        std::vector<Id> ends;
        for (auto t : {a, b})
            ends.push_back(ar.pow(ar.mul({tau({t, i}), tau({t, i, j})}), -1));
        return over(ar.add({cross, neg(ar.mul({ar.pow(m, 2), chain(square, i, j), ar.add(ends)}))}),
                    chain(!square, i, j));
    }

    // One Weyl orientation of the conserved current on line (a,b).
    // Photon permutations complete its ordered representative. The other
    // orientation is supplied by reversing both charged lines.
    Id oriented(int a, int b, const legs& photons, const projection& project) {
        const int i = photons[0], j = photons[1];
        const Id fi = dipole(a, b, i), ca = determinant(a, i, j), cb = determinant(b, i, j);
        const Id pair = scalar_pair(a, b, i, j, fi, dipole(a, b, j), ca, cb);
        const Id recoil = over(neg(ar.mul({ca, chain(!square, i, a), project({i})})), m);
        auto spin = [&](int v) { return over(chain(square, v, a), tau({a, v})); };
        if (photons.size() == 2)
            return ar.add({ar.mul({pair, project({a})}), ar.mul({fi, spin(j), project({j})}), recoil});

        const int k = photons[2];
        const auto fk = dipole(a, b, k);
        auto recoil_factor = [&](int t) {
            return over(ar.mul({chain(square, k, i), chain(square, k, i, {{t}})}),
                        ar.mul({tau({t, k}), tau({t, i, j, k})}));
        };
        const auto scalar = ar.add(
            {over(ar.mul({fk, ar.add({pair, ca, cb})}), ar.integer(3)), neg(ar.mul({cb, recoil_factor(b)}))});
        // The endpoint Dirac equation joins the bare square spinor and
        // the sum of its three spin shifts into <a|(p_a+p_i+p_j+p_k).
        return ar.add({ar.mul({scalar, project({a})}), ar.mul({pair, spin(k), project({k})}),
                       ar.mul({fk, recoil}),
                       over(ar.mul({ca, recoil_factor(a), project({a, {a, i, j, k}})}), m)});
    }
    Id bracket(const covector& x, int end) {
        return x.momentum.empty() ? chain(square, x.leg, end) : chain(!square, x.leg, end, {x.momentum});
    }

public:
    chiral_currents(Algebra& algebra, Id mass, bool sq, bool phys)
        : ar(algebra), m(mass), square(sq), physical(phys) {}
    Id exchange(int a, int b, int c, int d, const legs& photons) {
        const int i = photons[0], j = photons[1], k = photons[2];
        const auto bd = chain(!square, b, d), bc = chain(!square, b, c);
        auto bare = [&](const covector& x) {
            return ar.add({ar.mul({bd, bracket(x, c)}), ar.mul({bc, bracket(x, d)})});
        };
        const auto spin = ar.add({over(ar.mul({bd, chain(square, k, c)}), tau({c, k})),
                                  neg(over(ar.mul({bc, chain(square, k, d)}), tau({d, k})))});
        const auto fk = dipole(c, d, k);
        auto radiating = [&](const covector& x) {
            return ar.add({ar.mul({fk, bare(x)}), ar.mul({spin, bracket(x, k)})});
        };
        const auto zero = ar.integer(0);
        return ar.add({over(oriented(a, b, photons, bare), pole({c, d}, zero)),
                       over(oriented(a, b, {i, j}, radiating), pole({c, d, k}, zero))});
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
