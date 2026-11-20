// Binary128 math for a private rebuild of the generated SPINAS process using
// GNU C++23 and libquadmath. Retain the installed headers and source algebra.
#pragma once

#include <cmath>
#include <complex>
#include <iostream>
#include <limits>
#include <quadmath.h>
#include <stdfloat>
#include <string>

namespace hepcat_quad {
using real = std::float128_t;
using complex = std::complex<real>;
static_assert(std::numeric_limits<real>::digits == 113);
using std::abs, std::acos, std::arg, std::atan2, std::copysign, std::cos, std::floor, std::fma;
using std::frexp, std::hypot, std::ldexp, std::polar, std::sin, std::sqrt, std::to_string;
inline real abs(real x) { return fabsq(x); }
inline real sqrt(real x) { return sqrtq(x); }
inline real sin(real x) { return sinq(x); }
inline real cos(real x) { return cosq(x); }
inline real acos(real x) { return acosq(x); }
inline real atan2(real y, real x) { return atan2q(y,x); }
inline real hypot(real x, real y) { return hypotq(x,y); }
inline real copysign(real x, real y) { return copysignq(x,y); }
inline real floor(real x) { return floorq(x); }
inline real fma(real x, real y, real z) { return fmaq(x,y,z); }
inline real ldexp(real x, int exponent) { return ldexpq(x,exponent); }
inline real frexp(real x, int* exponent) { return frexpq(x,exponent); }
inline real abs(const complex& z) { return hypotq(z.real(),z.imag()); }
inline real arg(const complex& z) { return atan2q(z.imag(),z.real()); }
inline complex polar(const real& radius, const real& angle) { return {radius*cosq(angle),radius*sinq(angle)}; }
inline complex sqrt(const complex& z) {
    __complex128 x;
    __real__ x = z.real();
    __imag__ x = z.imag();
    const auto result = csqrtq(x);
    return {crealq(result),cimagq(result)};
}
inline std::string to_string(real x) {
    char buffer[128];
    quadmath_snprintf(buffer,sizeof(buffer),"%.36Qg",x);
    return buffer;
}
inline std::ostream& operator<<(std::ostream& out,real x) { return out << to_string(x); }
inline std::ostream& operator<<(std::ostream& out,const complex& z) {
    return out << '(' << to_string(z.real()) << ',' << to_string(z.imag()) << ')';
}
}
namespace spinas { using hepcat_quad::operator<<; }
