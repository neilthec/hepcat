#include "validation_precision.hpp"

#include <algorithm>
#include <cmath>
#include <iostream>
#include <stdexcept>

namespace v = hepcat::validation;
namespace p = v::precision;
namespace {
using p::number;
using p::scalar;
const scalar zero(0, p::bits), one(1, p::bits), tolerance("1e-38", p::bits);
void require(bool cond, const char* why) {
    if (!cond)
        throw std::runtime_error(why);
}
void close(const scalar& x, const scalar& y) {
    scalar difference = abs(x - y), bound = tolerance * (one + abs(x) + abs(y));
    require(difference <= bound, "wide kinematic identity lost precision");
}
void same_draw(const number& x, v::complex y) {
    require(std::abs(x.narrow() - y) < 2e-11L * (1 + std::abs(y)), "different spinor draw");
}
void check(const std::vector<v::real>& masses, std::uint64_t seed) {
    auto native = v::sample_point(masses, seed);
    native.change_spins(seed + 17);
    native.change_references(seed + 51);
    auto wide = p::reconstruct(native);
    const auto n = masses.size();
    std::array<scalar, 4> total{zero, zero, zero, zero};
    for (size_t i = 0; i < n; ++i) {
        const auto& k = wide.momenta[i];
        for (size_t j = 0; j < 4; ++j) {
            total[j] += k[j];
            require(k[j].get_prec() >= p::bits, "momentum component stored at reduced precision");
            require(std::abs(p::narrow_real(k[j]) - native.momenta[i][j]) <
                        2e-11L * (1 + std::abs(native.momenta[i][j])),
                    "different phase-space draw");
        }
        close(k[0] * k[0] - k[1] * k[1] - k[2] * k[2] - k[3] * k[3], wide.masses[i] * wide.masses[i]);
        const auto& a = wide.angles[i];
        const auto& s = wide.squares[i];
        same_draw(a[0], native.angles[i][0]);
        same_draw(a[1], native.angles[i][1]);
        same_draw(s[0], native.squares[i][0]);
        same_draw(s[1], native.squares[i][1]);
        const auto ref = wide.reference(i + 1, true);
        const auto original = native.reference(i + 1, true);
        same_draw(ref[0], original[0]);
        same_draw(ref[1], original[1]);
        close(ref[0].x * ref[0].x + ref[0].y * ref[0].y + ref[1].x * ref[1].x + ref[1].y * ref[1].y, one);

        // m square = epsilon adj(p) angle, with signed incoming momenta.
        const number x = number(-k[1], -k[2]) * a[0] + number(k[0] + k[3], zero) * a[1];
        const number y = number(-k[0] + k[3], zero) * a[0] + number(k[1], -k[2]) * a[1];
        close(x.x, wide.masses[i] * s[0].x);
        close(x.y, wide.masses[i] * s[0].y);
        close(y.x, wide.masses[i] * s[1].x);
        close(y.y, wide.masses[i] * s[1].y);
    }
    for (auto& component : total)
        close(component, zero);

    // Generalized Mandelstam row sums vanish at conserved on-shell momenta.
    for (size_t i = 0; i < n; ++i) {
        scalar sum = zero;
        for (size_t j = 0; j < n; ++j)
            sum += wide.scalar_products[i * n + j];
        close(sum, zero);
    }
}
} // namespace
int main() {
    try {
        check({80.385L, 80.385L, 80.385L, 80.385L}, 18384999939163226806ULL);
        check({0.0005L, 0.0005L, 0, 0, 0}, 3051909980850375ULL);
        check({1, 2, 3, 0, 5, 1}, 123454);
        check({0, 0, 0, 0}, 591);
        std::vector<v::real> m{80.385L, 80.385L, 80.385L, 80.385L};
        std::vector<scalar> exact(4, scalar("80.385", p::bits));
        auto wide = p::reconstruct(v::sample_point(m, 18384999939163226806ULL), exact);
        // Independent arbitrary-precision Wolfram replay of the same integer draws.
        close(wide.momenta[0][0], scalar("780.06248091603861349063834705645214834575", p::bits));
        std::cout << "Conserved high-precision charts and Weyl equations passed.\n";
    } catch (const std::exception& error) {
        std::cerr << error.what() << '\n';
        return 1;
    }
}
