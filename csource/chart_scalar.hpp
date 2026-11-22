#pragma once
#include "residue.hpp"
#include <array>
#include <cctype>
#include <string_view>
#include <unordered_map>

namespace hepcat::chart_detail {
using Q = mpq_class;
// Coefficients in Q(sqrt(2),i) keep the HEPCAT normalization exact.
inline std::uint64_t hashText(std::string_view s) {
    std::uint64_t h = 14695981039346656037ULL;
    for (unsigned char c : s) {
        h ^= c;
        h *= 1099511628211ULL;
    }
    return h;
}
inline std::uint64_t mix(std::uint64_t h) {
    h = (h ^ (h >> 30)) * 0xbf58476d1ce4e5b9ULL;
    h = (h ^ (h >> 27)) * 0x94d049bb133111ebULL;
    return h ^ (h >> 31);
}
struct Scalar {
    std::array<Q, 4> c{}; // 1, sqrt2, i, i*sqrt2
    Scalar() = default;
    explicit Scalar(const Q& q) { c[0] = q; }
    static Scalar root2() {
        Scalar z;
        z.c[1] = 1;
        return z;
    }
    static Scalar imaginary() {
        Scalar z;
        z.c[2] = 1;
        return z;
    }
    bool zero() const { return !c[0] && !c[1] && !c[2] && !c[3]; }
    bool rational() const { return !c[1] && !c[2] && !c[3]; }
};
inline Scalar& operator+=(Scalar& p, const Scalar& q) {
    for (unsigned i = 0; i < 4; ++i)
        if (q.c[i]) p.c[i] += q.c[i];
    return p;
}
inline Scalar operator+(Scalar p, const Scalar& q) {
    p += q;
    return p;
}
inline Scalar operator*(const Scalar& p, const Scalar& q) {
    if (p.rational() && q.rational()) return Scalar(p.c[0] * q.c[0]);
    Scalar z;
    for (unsigned i = 0; i < 4; ++i)
        if (p.c[i])
            for (unsigned j = 0; j < 4; ++j)
                if (q.c[j]) {
                    int factor = ((i & j) & 1) ? 2 : 1;
                    if ((i & j) & 2) factor = -factor;
                    z.c[i ^ j] += factor * p.c[i] * q.c[j];
                }
    return z;
}
inline Scalar inverse(const Scalar& p) {
    if (p.zero()) throw residue_singular("zero chart denominator");
    if (p.rational()) return Scalar(1 / p.c[0]);
    Scalar conj = p;
    conj.c[2] *= -1;
    conj.c[3] *= -1;
    Scalar norm = p * conj, conjugateNorm = norm;
    conjugateNorm.c[1] *= -1;
    Scalar den = norm * conjugateNorm;
    if (!den.rational() || !den.c[0]) throw std::logic_error("invalid quadratic coefficient norm");
    return conj * conjugateNorm * Scalar(1 / den.c[0]);
}
inline Scalar power(Scalar p, long n) {
    if (n < -100000 || n > 100000) throw residue_unsupported("exponent exceeds exact chart budget");
    if (n < 0) {
        p = inverse(p);
        n = -n;
    }
    Scalar z(Q(1));
    for (; n; n /= 2) {
        if (n % 2) z = z * p;
        if (n > 1) p = p * p;
    }
    return z;
}
inline bool sqrtRational(const Q& r, Q& out) {
    if (r < 0 || !mpz_perfect_square_p(r.get_num_mpz_t()) || !mpz_perfect_square_p(r.get_den_mpz_t()))
        return false;
    mpz_class n, d;
    mpz_sqrt(n.get_mpz_t(), r.get_num_mpz_t());
    mpz_sqrt(d.get_mpz_t(), r.get_den_mpz_t());
    out = Q(n, d);
    return true;
}
inline Scalar squareRoot(const Scalar& value) {
    if (!value.rational()) throw residue_unsupported("nested algebraic radical");
    Q rad = value.c[0], root;
    bool neg = rad < 0;
    if (neg) rad = -rad;
    Scalar r;
    if (sqrtRational(rad, root))
        r = Scalar(root);
    else if (sqrtRational(rad / 2, root))
        r = Scalar(root) * Scalar::root2();
    else
        throw residue_unsupported("radical outside Q(sqrt(2),i)");
    return neg ? r * Scalar::imaginary() : r;
}

inline bool free_parameter(const Node& node) {
    if (node.compound) return node.head == "ModelParameter" && node.args.size() == 3;
    const auto& name = node.head;
    if (name.empty() || !(std::isalpha(static_cast<unsigned char>(name.front())) || name.front() == '$'))
        return false;
    return name.rfind("System`", 0) != 0 && name != "I" && name != "ConstructiveDiagrams`z" &&
           name != "z" && name != "Indeterminate" && name != "ComplexInfinity" && name != "Infinity" &&
           name != "Null" && name != "True" && name != "False";
}

class Parameters {
    const Algebra& a;
    std::uint64_t salt;
    std::unordered_map<Id, Scalar> cache;

  public:
    Parameters(const Algebra& arena, std::size_t sample, const std::map<Id, Q>& fixed = {})
        : a(arena), salt(mix(sample + 13)) {
        for (const auto& [id, value] : fixed)
            cache.emplace(id, Scalar(value));
    }
    // Unknown atomic scalars are free model parameters. Compound heads require
    // explicit semantics: in particular a color tensor is never randomized.
    Scalar value(Id id) {
        if (auto found = cache.find(id); found != cache.end()) return found->second;
        const auto& n = a.node(id);
        Q q;
        Scalar z;
        if (a.number(id, q))
            z = Scalar(q);
        else if (n.head == "System`I" || n.head == "I")
            z = Scalar::imaginary();
        else if (free_parameter(n)) {
            auto h = mix(hashText(a.format(id)) ^ salt);
            // Denominator and numerator are positive. This avoids zero masses
            // and fixes the square-root branch for positive mass parameters.
            z = Scalar(
                Q(static_cast<unsigned long>(2 + h % 89), static_cast<unsigned long>(1 + (h >> 22) % 17)));
            z.c[0].canonicalize();
        } else if (n.head == "Plus" || n.head == "Times") {
            bool sum = n.head == "Plus";
            z = Scalar(Q(sum ? 0 : 1));
            for (Id child : n.args)
                z = sum ? z + value(child) : z * value(child);
        } else if (n.head == "Power" && n.args.size() == 2 && a.number(n.args[1], q)) {
            if (!q.get_num().fits_slong_p()) throw residue_unsupported("exponent too large");
            if (q.get_den() == 1)
                z = power(value(n.args[0]), q.get_num().get_si());
            else if (q.get_den() == 2)
                z = power(squareRoot(value(n.args[0])), q.get_num().get_si());
            else
                throw residue_unsupported("unsupported scalar root");
        } else if ((n.head == "System`Complex" || n.head == "Complex") && n.args.size() == 2) {
            z = value(n.args[0]) + Scalar::imaginary() * value(n.args[1]);
        } else
            throw residue_unsupported("unmapped scalar head: " + n.head);
        cache.emplace(id, z);
        return z;
    }
    Q rational(Id id) {
        Scalar z = value(id);
        if (!z.rational()) throw residue_unsupported("chart mass must be rational at the test point");
        return z.c[0];
    }
};

} // namespace hepcat::chart_detail
