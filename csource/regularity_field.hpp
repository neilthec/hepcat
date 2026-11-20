#pragma once
#include "chart_scalar.hpp"
#include <algorithm>
#include <vector>

namespace hepcat::chart_detail {
// Rational functions of a deformation parameter, evaluated in a prime field
// that admits the HEPCAT coefficient embeddings of sqrt(2) and i.
class polynomial_field {
    using word = std::uint64_t;
    using poly = std::vector<word>; // ascending coefficients, empty for zero
    word prime, sqrt2, imag;
    static void trim(poly& values) {
        while (!values.empty() && !values.back())
            values.pop_back();
    }
    word mul(word a, word b) const { return a * b % prime; }
    word inverse(word a) const {
        if (!a)
            throw residue_singular("singular prime-field chart coefficient");
        word result = 1;
        for (word n = prime - 2; n; n /= 2) {
            if (n % 2)
                result = mul(result, a);
            a = mul(a, a);
        }
        return result;
    }
    word project(const Q& x) const {
        return mul(mpz_fdiv_ui(x.get_num_mpz_t(), prime), inverse(mpz_fdiv_ui(x.get_den_mpz_t(), prime)));
    }
    poly scaled(poly p, word factor) const {
        for (auto& x : p)
            x = mul(x, factor);
        trim(p);
        return p;
    }
    poly product(const poly& a, const poly& b) const {
        if (a.empty() || b.empty())
            return {};
        if (a.size() + b.size() > 2048)
            throw residue_unsupported("regularity polynomial exceeds degree budget");
        poly result(a.size() + b.size() - 1);
        for (std::size_t i = 0; i < a.size(); ++i)
            for (std::size_t j = 0; j < b.size(); ++j)
                result[i + j] = (result[i + j] + a[i] * b[j]) % prime;
        return result;
    }
    poly sum(poly a, const poly& b) const {
        a.resize(std::max(a.size(), b.size()));
        for (std::size_t i = 0; i < b.size(); ++i)
            a[i] = (a[i] + b[i]) % prime;
        trim(a);
        return a;
    }
    std::pair<poly, poly> divide(poly a, const poly& b) const {
        word inv = inverse(b.back());
        poly quotient(a.size() < b.size() ? 0 : a.size() - b.size() + 1);
        while (a.size() >= b.size()) {
            std::size_t i = a.size() - b.size();
            word scale = mul(a.back(), inv);
            quotient[i] = scale;
            for (std::size_t j = 0; j < b.size(); ++j)
                a[i + j] = (prime + a[i + j] - mul(b[j], scale)) % prime;
            trim(a);
        }
        return {std::move(quotient), std::move(a)};
    }

  public:
    struct fraction {
        poly num, den{1};
    };
    explicit polynomial_field(std::size_t seed) {
        static constexpr word fields[][3] = {{998244353, 116195171, 86583718},
                                             {1004535809, 427335889, 483363861},
                                             {469762049, 156324964, 19610091},
                                             {2013265921, 632622901, 284861408}};
        auto& f = fields[seed % 4];
        prime = f[0];
        sqrt2 = (seed & 4) ? prime - f[1] : f[1];
        imag = (seed & 8) ? prime - f[2] : f[2];
    }
    word coefficient(const Scalar& x) const {
        auto r = project(x.c[0]);
        r += mul(sqrt2, project(x.c[1]));
        r += mul(imag, project(x.c[2]));
        r += mul(mul(sqrt2, imag), project(x.c[3]));
        return r % prime;
    }
    fraction reduced(poly num, poly den) const {
        trim(num);
        trim(den);
        if (den.empty())
            throw residue_singular("zero denominator on the sampled finite-field deformation");
        if (num.empty())
            return {};
        if (den.size() > 1 && num.size() > 1) {
            auto a = num, b = den;
            while (!b.empty()) {
                auto rem = divide(std::move(a), b).second;
                a = std::move(b);
                b = std::move(rem);
            }
            if (a.size() > 1) {
                num = divide(std::move(num), a).first;
                den = divide(std::move(den), a).first;
            }
        }
        word norm = inverse(den.back());
        return {scaled(std::move(num), norm), scaled(std::move(den), norm)};
    }
    fraction add(const fraction& a, const fraction& b) const {
        if (a.den == b.den)
            return reduced(sum(a.num, b.num), a.den);
        return reduced(sum(product(a.num, b.den), product(b.num, a.den)), product(a.den, b.den));
    }
    fraction multiply(const fraction& a, const fraction& b) const {
        return reduced(product(a.num, b.num), product(a.den, b.den));
    }
    fraction pow(fraction x, long k) const {
        if (k < -4096 || k > 4096)
            throw residue_unsupported("excessive regularity exponent");
        if (k < 0) {
            if (x.num.empty())
                throw residue_singular("zero reciprocal on finite-field chart");
            std::swap(x.num, x.den);
            k = -k;
        }
        fraction result{{1}, {1}};
        for (; k; k /= 2) {
            if (k % 2)
                result = multiply(result, x);
            if (k > 1)
                x = multiply(x, x);
        }
        return result;
    }
};
} // namespace hepcat::chart_detail
