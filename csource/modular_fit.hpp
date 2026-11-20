#pragma once
#include "residue.hpp"
#include <numeric>

namespace hepcat::fit_detail {
// Reduce large pencil matrices modulo a prime and reconstruct small rational
// proposal coefficients. Acceptance always uses the original characteristic-zero
// amplitude on independent charts. Explicit or small systems use exact GMP fits.
class modular_system {
    using Q = mpq_class;
  public:
    using word = std::uint32_t;
    static constexpr word prime = 2147483647;
  private:
    std::size_t n, rank_ = 0;
    std::vector<std::vector<word>> pivots;
  public:
    const auto& echelon() const { return pivots; }
    static word product(word x, word y) { return std::uint64_t(x) * y % prime; }
    static word inverse(word x) {
        word a = 1;
        for (word pow = prime - 2; pow; pow >>= 1, x = product(x, x))
            if (pow & 1) a = product(a, x);
        return a;
    }
    static word field(const Q& x) {
        word num = mpz_fdiv_ui(x.get_num_mpz_t(), prime);
        if (!num) return 0;
        word den = mpz_fdiv_ui(x.get_den_mpz_t(), prime);
        if (!den) throw residue_singular("chart denominator divisible by fit prime");
        return den == 1 ? num : product(num, inverse(den));
    }
    static std::optional<Q> reconstruct(word x) {
        std::int64_t r = prime, s = x, u = 0, v = 1;
        constexpr int bound = 32767;
        while (s > bound) {
            auto quotient = r / s;
            auto next = r - quotient * s;
            r = s;
            s = next;
            next = u - quotient * v;
            u = v;
            v = next;
        }
        if (std::abs(v) > bound || v == 0 || std::gcd(s, std::abs(v)) != 1) return {};
        Q result(static_cast<long>(s), static_cast<long>(v));
        result.canonicalize();
        return result;
    }

  public:
    explicit modular_system(std::size_t size) : n(size), pivots(n) {}
    bool add(const std::vector<Q>& input, std::size_t offset, const Q& rhs) {
        std::vector<word> row(n + 1);
        for (std::size_t j = 0; j < n; ++j)
            row[j] = field(input[j + offset]);
        row[n] = field(rhs);
        for (std::size_t j = 0; j < n; ++j)
            if (word c = row[j]) {
                if (pivots[j].empty()) {
                    word inv = inverse(c);
                    for (std::size_t k = j; k <= n; ++k)
                        row[k] = product(row[k], inv);
                    pivots[j] = std::move(row);
                    ++rank_;
                    return true;
                }
                for (std::size_t k = j; k <= n; ++k) {
                    auto v = product(c, pivots[j][k]);
                    row[k] = row[k] >= v ? row[k] - v : row[k] + (prime - v);
                }
            }
        return row[n] == 0;
    }
    std::size_t rank() const { return rank_; }
    std::vector<word> residues() const {
        std::vector<word> solved(n);
        for (std::size_t i = n; i-- > 0;) {
            if (pivots[i].empty()) continue;
            word value = pivots[i][n];
            for (std::size_t j = i + 1; j < n; ++j)
                if (solved[j] && pivots[i][j]) {
                    word x = product(pivots[i][j], solved[j]);
                    value = value >= x ? value - x : value + (prime - x);
                }
            solved[i] = value;
        }
        return solved;
    }
    std::optional<std::vector<Q>> solution() const {
        std::vector<Q> result;
        for (word x : residues()) {
            auto q = reconstruct(x);
            if (!q) return {};
            result.push_back(std::move(*q));
        }
        return result;
    }
};

} // namespace hepcat::fit_detail
