// HEPCAT, Copyright (C) 2026 Neil Christensen. GPL-3.0-or-later.
#pragma once
#include "spinas-runtime.hpp"
#include <limits>
#include <vector>

namespace hepcat_spinas {

// First-order forward-error estimate, in units of machine epsilon. This
// measures cancellation in the expression from its cached kinematic factors.
struct conditioned_complex {
    cdouble value{};
    ldouble error = 0;
    conditioned_complex() = default;
    conditioned_complex(ldouble x) : value(x) {}
    conditioned_complex(cdouble z) : value(z), error(std::abs(z)) {}
    conditioned_complex(cdouble z, ldouble e) : value(z), error(e) {}
};
inline conditioned_complex operator+(conditioned_complex a, conditioned_complex b) {
    const auto z = a.value + b.value;
    return {z, a.error + b.error + std::abs(z)};
}
inline conditioned_complex& operator+=(conditioned_complex& a, conditioned_complex b) { return a = a+b; }
inline conditioned_complex operator-(conditioned_complex a) { return {-a.value, a.error}; }
inline conditioned_complex operator*(conditioned_complex a, conditioned_complex b) {
    const auto z = a.value * b.value;
    return {z, std::abs(a.value)*b.error + std::abs(b.value)*a.error + std::abs(z)};
}
inline conditioned_complex operator/(conditioned_complex a, conditioned_complex b) {
    const auto z = a.value / b.value;
    return {z, (a.error + std::abs(z)*b.error) / std::abs(b.value) + std::abs(z)};
}
inline cdouble power(cdouble a, cdouble b) { return std::pow(a,b); }
inline cdouble power(cdouble a, ldouble b) { return std::pow(a,b); }
inline conditioned_complex power(conditioned_complex a, conditioned_complex b) {
    const auto z = std::pow(a.value,b.value);
    if (a.value == cdouble{}) return {z, a.error == 0 ? 0 : std::numeric_limits<ldouble>::infinity()};
    return {z, std::abs(z)*(1+std::abs(b.value)*a.error/std::abs(a.value)+std::abs(std::log(a.value))*b.error)};
}
inline cdouble root(cdouble x) { return std::sqrt(x); }
inline conditioned_complex root(conditioned_complex x) {
    const auto z = std::sqrt(x.value);
    return {z, std::abs(z) + (x.error == 0 ? 0 : x.error/(2*std::abs(z)))};
}
inline ldouble relative_condition(conditioned_complex a) {
    if (!finite_complex(a.value) || !std::isfinite(a.error)) return std::numeric_limits<ldouble>::infinity();
    if (a.error == 0) return 0;
    return a.error / std::abs(a.value);
}

// Try a bounded set of permutations that preserve species, external role and
// available helicities. The input always remains a single physical point.
template<std::size_t N> std::vector<std::array<std::size_t,N>> bose_permutations(
        const std::vector<std::vector<std::size_t>>& groups) {
    using permutation = std::array<std::size_t,N>;
    permutation identity{};
    for (std::size_t i=0;i<N;++i) identity[i]=i;
    std::vector<permutation> result{identity};
    constexpr std::size_t limit = 24;
    // Try all single transpositions first, including legs late in a large group.
    for (const auto& group : groups)
        for (std::size_t distance=1;distance<group.size();++distance)
            for (std::size_t i=0;i+distance<group.size() && result.size()<limit;++i) {
                auto p=identity;
                std::swap(p[group[i]],p[group[i+distance]]);
                result.push_back(p);
            }
    for (std::size_t row=1;row<result.size() && result.size()<limit;++row)
        for (const auto& group : groups)
            for (std::size_t i=0;i+1<group.size() && result.size()<limit;++i) {
                auto p=result[row];
                std::swap(p[group[i]],p[group[i+1]]);
                if (std::find(result.begin(),result.end(),p)==result.end()) result.push_back(p);
            }
    return result;
}
} // namespace hepcat_spinas
