#pragma once
#include "validation_numeric.hpp"
#include <cmath>
#include <limits>
#include <stdexcept>

namespace hepcat::validation::precision {
using scalar = mpf_class;
inline constexpr mp_bitcnt_t bits = 160;
inline scalar wide_real(real x) {
    if (!std::isfinite(x))
        throw std::domain_error("nonfinite scalar in precision replay");
    if constexpr (std::numeric_limits<real>::digits <= std::numeric_limits<double>::digits)
        return mpf_class(static_cast<double>(x), bits);
    // Transfer wider long doubles in exact binary chunks without a double cast.
    int exponent = 0;
    real fraction = std::frexp(std::abs(x), &exponent);
    constexpr int digits = std::numeric_limits<real>::digits;
    mpf_class result(0, bits);
    for (int remaining = digits; remaining > 0;) {
        int shift = std::min(remaining, 24);
        fraction = std::ldexp(fraction, shift);
        auto word = static_cast<unsigned long>(fraction);
        mpf_mul_2exp(result.get_mpf_t(), result.get_mpf_t(), shift);
        result += word;
        fraction -= word;
        remaining -= shift;
    }
    if (exponent > digits)
        mpf_mul_2exp(result.get_mpf_t(), result.get_mpf_t(), exponent - digits);
    else
        mpf_div_2exp(result.get_mpf_t(), result.get_mpf_t(), digits - exponent);
    if (x < 0)
        result = -result;
    return result;
}
inline real narrow_real(const mpf_class &x) {
    if constexpr (std::numeric_limits<real>::digits <= std::numeric_limits<double>::digits)
        return static_cast<real>(x.get_d());
    if (x == 0)
        return 0;
    // Preserve the additional mantissa/exponent bits on x86 and quad hosts.
    mp_exp_t exponent = 0;
    auto digits = x.get_str(exponent, 10, std::numeric_limits<real>::max_digits10 + 2);
    bool negative = digits.front() == '-';
    return std::stold(std::string(negative ? "-0." : "0.") + digits.substr(negative ? 1 : 0) + "e" +
                      std::to_string(exponent));
}
struct number {
    mpf_class x{0, bits}, y{0, bits};
    number() = default;
    explicit number(const scalar &r) : x(r, bits), y(0, bits) {}
    explicit number(complex z) : x(wide_real(z.real())), y(wide_real(z.imag())) {}
    number(const mpf_class &r, const mpf_class &i) : x(r, bits), y(i, bits) {}
    complex narrow() const { return {narrow_real(x), narrow_real(y)}; }
    number operator*(const number &z) const {
        return {mpf_class(x * z.x - y * z.y, bits), mpf_class(x * z.y + y * z.x, bits)};
    }
    number operator+(const number &z) const { return {scalar(x + z.x, bits), scalar(y + z.y, bits)}; }
    number operator-(const number &z) const { return {scalar(x - z.x, bits), scalar(y - z.y, bits)}; }
    number operator-() const { return {scalar(-x, bits), scalar(-y, bits)}; }
    number conj() const { return {x, scalar(-y, bits)}; }
    scalar magnitude() const { return scalar(sqrt(x * x + y * y), bits); }
    number &operator+=(const number &z) {
        x += z.x;
        y += z.y;
        return *this;
    }
    number &operator*=(const number &z) { return *this = *this * z; }
    number reciprocal() const {
        mpf_class norm(x * x + y * y, bits);
        if (norm == 0)
            throw std::domain_error("zero denominator in precision replay");
        return {mpf_class(x / norm, bits), mpf_class(-y / norm, bits)};
    }
    number square_root() const {
        if (y == 0) {
            if (x >= 0)
                return {mpf_class(sqrt(x), bits), wide_real(0)};
            return {wide_real(0), mpf_class(sqrt(-x), bits)};
        }
        mpf_class radius(sqrt(x * x + y * y), bits);
        mpf_class component(sqrt((radius + abs(x)) / 2), bits);
        if (x >= 0)
            return {component, mpf_class(y / (2 * component), bits)};
        if (y < 0)
            component = -component;
        return {mpf_class(y / (2 * component), bits), component};
    }
    number power(long k) const {
        if (k == std::numeric_limits<long>::min())
            throw std::domain_error("invalid precision replay exponent");
        auto base = k < 0 ? reciprocal() : *this;
        if (k < 0)
            k = -k;
        number result(complex{1, 0});
        for (; k; k /= 2) {
            if (k % 2)
                result = result * base;
            if (k > 1)
                base = base * base;
        }
        return result;
    }
};

using vector = std::array<number, 2>;
using four = std::array<scalar, 4>;
struct phase_point {
    std::vector<four> momenta;
    std::vector<scalar> masses, scalar_products;
    std::vector<vector> angles, squares;
    std::vector<std::array<vector, 2>> angle_basis, square_basis;
    std::uint64_t reference_seed = 0, color_seed = 0;
    vector reference(std::uint64_t label, bool angle) const;
};
// Build Weyl spinors and scalar products for already conserved, on-shell all-incoming momenta.
phase_point from_momenta(std::vector<four> momenta, std::vector<scalar> masses, std::uint64_t spin_seed,
                         std::uint64_t reference_seed = 0, std::uint64_t color_seed = 0);
phase_point reconstruct(const point &, const std::vector<scalar> &masses = {});
} // namespace hepcat::validation::precision
