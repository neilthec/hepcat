#include "validation_precision.hpp"
#include "chart_scalar.hpp"

#include <algorithm>
#include <cmath>
#include <stdexcept>

namespace hepcat::validation::precision {
namespace {
scalar integer(long value) {
    return scalar(value, bits);
}
scalar tiny() {
    scalar value = integer(1);
    mpf_div_2exp(value.get_mpf_t(), value.get_mpf_t(), bits + 4);
    return value;
}

// The draw only needs exp on [-log(3),2log(3)], log on a positive
// variate and sine/cosine of a turn. Splitting the arguments into bounded
// intervals gives rapidly convergent series at the GMP replay precision.
scalar log_mantissa(const scalar& value) {
    const scalar t = (value - 1) / (value + 1), t2 = t * t;
    scalar power = t, total = t;
    const scalar eps = tiny();
    for (unsigned k = 3;; k += 2) {
        power *= t2;
        const scalar term = power / k;
        total += term;
        if (abs(term) < eps)
            return 2 * total;
    }
}

scalar positive_log(const scalar& value) {
    if (value <= 0)
        throw std::domain_error("nonpositive replay logarithm");
    long exponent;
    mpf_get_d_2exp(&exponent, value.get_mpf_t());
    scalar fraction = value;
    if (exponent > 0)
        mpf_div_2exp(fraction.get_mpf_t(), fraction.get_mpf_t(), exponent);
    else if (exponent < 0)
        mpf_mul_2exp(fraction.get_mpf_t(), fraction.get_mpf_t(), -exponent);
    static const scalar ln2 = log_mantissa(integer(2));
    return log_mantissa(fraction) + exponent * ln2;
}

scalar energy_exponential(const scalar& x) {
    scalar term = integer(1), sum = term;
    const scalar quarter = x / 4, eps = tiny();
    for (unsigned k = 1;; ++k) {
        term *= quarter;
        term /= k;
        sum += term;
        if (abs(term) < eps) {
            sum *= sum;
            sum *= sum;
            return sum;
        }
    }
}

number unit_phase(const scalar& turn) {
    static const scalar half_pi(
        "1.570796326794896619231321691639751442098584699687552910487472296153908203143104499", bits);
    const scalar quarter = 4 * turn;
    unsigned quadrant = quarter.get_ui();
    const scalar x = (quarter - quadrant) * half_pi, minus_square = -x * x, eps = tiny();
    scalar sine = x, sterm = x, cosine = integer(1), cterm = cosine;
    for (unsigned k = 1;; ++k) {
        cterm *= minus_square;
        cterm /= (2 * k - 1) * (2 * k);
        sterm *= minus_square;
        sterm /= (2 * k) * (2 * k + 1);
        sine += sterm;
        cosine += cterm;
        if (abs(cterm) < eps)
            break;
    }
    switch (quadrant) {
    case 0:
        return {cosine, sine};
    case 1:
        return {-sine, cosine};
    case 2:
        return {-cosine, -sine};
    default:
        return {sine, -cosine};
    }
}
number scale(const number& z, const scalar& factor) {
    return {z.x * factor, z.y * factor};
}

struct random_draw {
    std::uint64_t state;
    scalar uniform() {
        // The numerator is a 52-bit integer plus 1/2. Binary64 represents
        // the uniform coordinate exactly, including its open interval endpoints.
        state += 0x9e3779b97f4a7c15ULL;
        return scalar(std::ldexp(static_cast<double>(chart_detail::mix(state) >> 12) + 0.5, -52), bits);
    }
    std::array<scalar, 3> direction() {
        const scalar z = 2 * uniform() - 1;
        const number phase = unit_phase(uniform());
        const scalar radial = sqrt((1 - z) * (1 + z));
        return {radial * phase.x, radial * phase.y, z};
    }
    vector spin_state() {
        const scalar r = uniform();
        const number left = unit_phase(uniform()), right = unit_phase(uniform());
        return {scale(left, sqrt(r)), scale(right, sqrt(1 - r))};
    }
};
scalar minkowski(const four& p, const four& q) {
    return p[0] * q[0] - p[1] * q[1] - p[2] * q[2] - p[3] * q[3];
}
scalar norm3(const four& p) {
    return sqrt(p[1] * p[1] + p[2] * p[2] + p[3] * p[3]);
}
scalar massive_energy(const scalar& p, const scalar& m) {
    return sqrt(p * p + m * m);
}
scalar breakup(const scalar& e, const scalar& m1, const scalar& m2) {
    return sqrt((e - m1 - m2) * (e + m1 + m2) * (e - m1 + m2) * (e + m1 - m2)) / (2 * e);
}
void back_to_back(phase_point& pt, random_draw& gen, size_t first, const scalar& energy, int sign) {
    const auto axis = gen.direction();
    const scalar k = breakup(energy, pt.masses[first], pt.masses[first + 1]);
    for (size_t i = first; i < first + 2; ++i) {
        auto& p = pt.momenta[i];
        p[0] = sign * massive_energy(k, pt.masses[i]);
        for (size_t j = 0; j < 3; ++j)
            p[j + 1] = sign * k * axis[j] * (i == first ? 1 : -1);
    }
}
void rambo(phase_point& pt, random_draw& gen, const scalar& energy) {
    const auto n = pt.masses.size();
    four total{integer(0), integer(0), integer(0), integer(0)};
    for (size_t i = 2; i < n; ++i) {
        auto& p = pt.momenta[i];
        auto axis = gen.direction();
        const scalar first = gen.uniform(), second = gen.uniform();
        p[0] = -positive_log(first * second);
        for (size_t j = 0; j < 3; ++j)
            p[j + 1] = p[0] * axis[j];
        for (size_t j = 0; j < 4; ++j)
            total[j] += p[j];
    }
    scalar invariant = minkowski(total, total);
    if (invariant <= 0)
        throw std::domain_error("singular replay phase-space boost");
    const scalar cm = sqrt(invariant);
    size_t pivot = 2;
    for (size_t i = 2; i < n; ++i) {
        auto& p = pt.momenta[i];
        const scalar rest = minkowski(p, total) / cm;
        for (size_t j = 1; j < 4; ++j)
            p[j] = (p[j] - total[j] * (p[0] + rest) / (total[0] + cm)) * energy / cm;
        p[0] = rest * energy / cm;
        if (p[0] > pt.momenta[pivot][0])
            pivot = i;
    }
    for (size_t j = 1; j < 4; ++j) {
        scalar sum = integer(0);
        for (size_t i = 2; i < n; ++i)
            if (i != pivot)
                sum += pt.momenta[i][j];
        pt.momenta[pivot][j] = -sum;
    }
    scalar spatial = integer(0);
    bool massive_final = false;
    for (size_t i = 2; i < n; ++i) {
        auto& p = pt.momenta[i];
        p[0] = norm3(p);
        spatial += p[0];
        massive_final |= pt.masses[i] != 0;
    }
    scalar lo = integer(0), hi = energy / spatial;
    if (massive_final)
        for (size_t k = 0; k < bits + 4; ++k) {
            const scalar mid = (lo + hi) / 2;
            scalar sum = integer(0);
            for (size_t i = 2; i < n; ++i)
                sum += massive_energy(mid * pt.momenta[i][0], pt.masses[i]);
            if (sum < energy)
                lo = mid;
            else
                hi = mid;
        }
    for (size_t i = 2; i < n; ++i) {
        auto& p = pt.momenta[i];
        p[0] = -massive_energy(hi * p[0], pt.masses[i]);
        for (size_t j = 1; j < 4; ++j)
            p[j] *= -hi;
    }
}

void make_spinors(phase_point& pt, std::uint64_t spin_seed) {
    random_draw gen{spin_seed};
    for (size_t i = 0; i < pt.masses.size(); ++i) {
        const auto& p = pt.momenta[i];
        int direction = p[0] < 0 ? -1 : 1;
        const scalar x = direction * p[1], y = direction * p[2], z = direction * p[3], k = norm3(p);
        vector u{number(integer(1), integer(0)), number()};
        if (k != 0) {
            const scalar d = sqrt(2 * k * (k + abs(z)));
            if (z >= 0)
                u = {number((k + z) / d, integer(0)), number(x / d, y / d)};
            else
                u = {number(x / d, -y / d), number((k - z) / d, integer(0))};
        }
        const vector v = {-u[1].conj(), u[0].conj()};
        const scalar big = sqrt(abs(p[0]) + k), small = pt.masses[i] / big;
        auto& ab = pt.angle_basis[i];
        auto& sb = pt.square_basis[i];
        for (size_t j = 0; j < 2; ++j) {
            ab[0][j] = scale(u[j], big);
            ab[1][j] = scale(v[j], small);
            sb[0][j] = scale(v[j].conj(), -direction * small);
            sb[1][j] = scale(u[j].conj(), direction * big);
            pt.angles[i][j] = ab[0][j];
            pt.squares[i][j] = sb[1][j];
        }
        if (pt.masses[i] != 0) {
            const auto w = gen.spin_state();
            for (size_t j = 0; j < 2; ++j) {
                pt.angles[i][j] = ab[0][j] * w[0] + ab[1][j] * w[1];
                pt.squares[i][j] = sb[0][j] * w[0] + sb[1][j] * w[1];
            }
        }
    }
}
} // namespace

vector phase_point::reference(std::uint64_t label, bool angle) const {
    random_draw gen{chart_detail::mix(reference_seed ^ chart_detail::mix(label))};
    auto ref = gen.spin_state();
    if (!angle)
        for (auto& component : ref)
            component = component.conj();
    return ref;
}

phase_point reconstruct(const validation::point& source, const std::vector<scalar>& masses) {
    const size_t n = source.masses.size();
    if (n < 4)
        throw std::invalid_argument("two-beam precision replay requires at least four legs");
    phase_point out;
    if (!masses.empty() && masses.size() != n)
        throw std::invalid_argument("invalid precise mass count");
    for (size_t i = 0; i < n; ++i) {
        out.masses.push_back(masses.empty() ? wide_real(source.masses[i]) : scalar(masses[i], bits));
        if (out.masses.back() < 0 || (source.masses[i] == 0) != (out.masses.back() == 0))
            throw std::invalid_argument("precision replay requires the same external mass shells");
    }
    out.reference_seed = source.reference_seed;
    out.color_seed = source.color_seed;
    out.momenta.resize(n, four{integer(0), integer(0), integer(0), integer(0)});
    out.angles.resize(n);
    out.squares.resize(n);
    out.angle_basis.resize(n);
    out.square_basis.resize(n);
    out.scalar_products.assign(n * n, integer(0));
    random_draw gen{source.phase_seed};
    scalar threshold = out.masses[0] + out.masses[1];
    scalar final_mass = integer(0);
    for (size_t i = 2; i < n; ++i)
        final_mass += out.masses[i];
    if (final_mass > threshold)
        threshold = final_mass;
    const scalar one = integer(1);
    static const scalar ln3 = positive_log(integer(3));
    scalar energy = threshold + std::max(threshold, one) * energy_exponential((3 * gen.uniform() - 1) * ln3);
    back_to_back(out, gen, 0, energy, 1);
    if (n == 4)
        back_to_back(out, gen, 2, energy, -1);
    else
        rambo(out, gen, energy);
    make_spinors(out, source.spin_seed);
    for (size_t i = 0; i < n; ++i) {
        out.scalar_products[i * n + i] = out.masses[i] * out.masses[i];
        for (size_t j = 0; j < i; ++j) {
            // The two Weyl columns factor the on-shell momentum matrix.
            // This determinant identity is stable even for nearly collinear legs.
            scalar sum = integer(0);
            for (const auto& u : out.angle_basis[i])
                for (const auto& v : out.angle_basis[j]) {
                    const number wedge = u[0] * v[1] - u[1] * v[0];
                    sum += wedge.x * wedge.x + wedge.y * wedge.y;
                }
            sum /= (i < 2) == (j < 2) ? 2 : -2;
            out.scalar_products[i * n + j] = out.scalar_products[j * n + i] = sum;
        }
    }
    return out;
}
} // namespace hepcat::validation::precision
