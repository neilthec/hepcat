// Numerical evaluation of the open Dirac tensors with reusable off-shell
// currents. Units and external spinors are supplied by the caller, all momenta
// incoming.
#pragma once
#include "validation_numeric.hpp"
#include <algorithm>
#include <stdexcept>

namespace hepcat::validation::dirac_current {

using vector = std::array<complex, 4>;
using mask = unsigned;
inline unsigned count(mask x) { return __builtin_popcount(x); }
inline unsigned first(mask x) { return __builtin_ctz(x); }
inline complex dot(const vector &a, const vector &b) {
  return a[0] * b[0] - a[1] * b[1] - a[2] * b[2] - a[3] * b[3];
}
inline complex contract(const vector &row, const vector &col) {
  return row[0] * col[0] + row[1] * col[1] + row[2] * col[2] + row[3] * col[3];
}
inline vector slash(const vector &q, const vector &x, bool row = false) {
  // Weyl blocks sigma(q), adj(sigma(q)). Transpose for a row spinor.
  const complex I(0, 1), a = q[0] + q[3], b = q[1] - I * q[2],
                         c = q[1] + I * q[2], d = q[0] - q[3];
  if (row)
    return {d * x[2] - c * x[3], -b * x[2] + a * x[3], a * x[0] + c * x[1],
            b * x[0] + d * x[1]};
  return {a * x[2] + b * x[3], c * x[2] + d * x[3], d * x[0] - b * x[1],
          -c * x[0] + a * x[1]};
}
inline vector current_vector(const vector &bra, const vector &ket) {
  const complex I(0, 1);
  const complex a = bra[0] * ket[2], b = bra[0] * ket[3], c = bra[1] * ket[2],
                d = bra[1] * ket[3];
  const complex e = bra[2] * ket[0], f = bra[2] * ket[1], g = bra[3] * ket[0],
                h = bra[3] * ket[1];
  return {a + d + e + h, -b - c + f + g, I * (b - c - f + g), -a + d + e - h};
}
inline spinor raise(spinor x) { return {x[1], -x[0]}; }
inline complex inner(spinor x, spinor y) { return x[0] * y[0] + x[1] * y[1]; }
enum class gauge { independent, physical, spurious };

inline vector polarization(const point &p, int i, int helicity, int ref,
                           gauge policy, int electron, int partner = -1) {
  const bool plus = helicity > 0;
  auto xi = ref < 0 ? p.reference(i + 1, plus)
                    : (plus ? p.angles : p.squares).at(ref);
  const auto angle = plus ? xi : p.angles[i], square = plus ? p.squares[i] : xi;
  vector bra{}, ket{};
  const auto left = raise(angle), right = raise(square);
  if (plus) {
    bra = {left[0], left[1], 0, 0};
    ket = {0, 0, right[0], right[1]};
  } else {
    bra = {0, 0, square[0], square[1]};
    ket = {angle[0], angle[1], 0, 0};
  }
  auto out = current_vector(bra, ket);
  const auto den = std::sqrt(2.L) * (plus ? inner(left, p.angles[i])
                                          : inner(xi, raise(p.squares[i])));
  for (auto &x : out)
    x /= -den;
  if (policy == gauge::physical || partner >= 0) {
    vector parent{}, photon{};
    for (int j = 0; j < 4; ++j) {
      parent[j] = p.momenta[electron][j];
      if (partner >= 0)
        parent[j] += p.momenta[partner][j];
      photon[j] = p.momenta[i][j];
    }
    const complex shift = dot(parent, out) / dot(parent, photon);
    for (int j = 0; j < 4; ++j)
      out[j] -= shift * photon[j];
  }
  return out;
}

// The sign is the parity of the shuffle that puts external fermion labels
// back into increasing order. Bosons carry no Grassmann sign.
inline int shuffle(mask a, mask b) {
  unsigned parity = 0;
  while (a) {
    mask low = a & -a;
    parity += count(b & (low - 1));
    a ^= low;
  }
  return parity % 2 ? -1 : 1;
}
class recursion {
  struct term {
    mask left, right;
    real sign;
  };
  int n, root, direct, paired_reference;
  mask charged = 0;
  std::vector<int> species, excess, reference;
  std::vector<std::vector<term>> cuts;

public:
  // Species 1 and -1 denote electron and positron, +/-2 the photon helicities.
  explicit recursion(std::vector<int> input)
      : n(input.size()), species(std::move(input)) {
    if (n < 4 || n > 15)
      throw std::invalid_argument("Unsupported current size");
    root = std::find(species.begin(), species.end(), -1) - species.begin();
    if (root == n)
      throw std::invalid_argument("Missing positron");
    std::vector<int> bars, kets;
    for (int i = 0; i < n; ++i) {
      if (species[i] == -1)
        bars.push_back(i);
      if (species[i] == 1)
        kets.push_back(i);
      if (std::abs(species[i]) == 1)
        charged |= 1u << i;
    }
    if (kets.size() != bars.size())
      throw std::invalid_argument("Unbalanced external charges");
    // With only one photon, its spurious gauge uses a charged pair momentum.
    paired_reference = species[first(charged)] < 0 ? kets.front() : bars.front();
    const mask full = (1u << n) - 1, top = full ^ (1u << root);
    direct = bars.size() % 2 ? -1 : 1;
    mask prefix = 0;
    for (unsigned j = 0; j < bars.size(); ++j)
      for (int leg : {bars[j], kets[j]}) {
        direct *= shuffle(prefix, 1u << leg);
        prefix |= 1u << leg;
      }
    direct *= shuffle(1u << root, top & charged);
    excess.resize(full + 1);
    cuts.resize(full + 1);
    for (mask s = 1; s <= full; ++s) {
      const mask k = s & -s;
      excess[s] = excess[s ^ k] + (k & charged ? species[first(k)] : 0);
      if ((s & (1u << root)) || !(s & charged) || std::abs(excess[s]) > 1)
        continue;
      for (mask a = (s - 1) & s; a; a = (a - 1) & s) {
        mask b = s ^ a;
        if (excess[s] == 0 ? excess[a] != -1 || excess[b] != 1
                           : excess[a] != 0 || excess[b] != excess[s] ||
                                 (!(a & charged) && count(a) > 1))
          continue;
        cuts[s].push_back({a, b, real(shuffle(a & charged, b & charged))});
      }
    }
    reference.assign(n, -1);
    for (int i = 0; i < n; ++i)
      if (!(charged & (1u << i))) {
        for (int offset = 1; offset < n; ++offset) {
          int j = (i + offset) % n;
          if (species[j] == -species[i]) {
            reference[i] = j;
            break;
          }
          if (reference[i] < 0 && species[j] == species[i])
            reference[i] = j;
        }
      }
  }
  complex operator()(const point &p, real e, gauge policy) const {
    const mask top = ((1u << n) - 1) ^ (1u << root);
    std::vector<vector> waves(top + 1), mom(top + 1);
    std::vector<real> invariant(top + 1);
    const real m = p.masses[root];
    auto barred = [&](int i) {
      const auto a = raise(p.angles[i]), s = p.squares[i];
      return vector{-a[0], -a[1], s[0], s[1]};
    };
    for (mask s = 1; s <= top; ++s) {
      if (s & (1u << root))
        continue;
      const mask low = s & -s, rest = s ^ low;
      const int i = first(low);
      for (int j = 0; j < 4; ++j)
        mom[s][j] = mom[rest][j] + p.momenta[i][j];
      invariant[s] = invariant[rest] + p.masses[i] * p.masses[i];
      for (mask bits = rest; bits; bits &= bits - 1)
        invariant[s] += 2 * p.scalar_products[i * n + first(bits)];
      if (!rest) {
        if (species[i] == -1)
          waves[s] = barred(i);
        else if (species[i] == 1) {
          const auto a = p.angles[i], b = raise(p.squares[i]);
          waves[s] = {a[0], a[1], -b[0], -b[1]};
        } else {
          const bool fallback = policy == gauge::spurious && reference[i] < 0;
          waves[s] = polarization(
              p, i, species[i], policy == gauge::spurious ? reference[i] : -1,
              policy, first(charged), fallback ? paired_reference : -1);
        }
        continue;
      }
      if (cuts[s].empty())
        continue;
      vector x{};
      for (const auto &t : cuts[s]) {
        const auto &a = waves[t.left];
        const auto &b = waves[t.right];
        auto y =
            excess[s] == 0 ? current_vector(a, b) : slash(a, b, excess[s] < 0);
        for (int j = 0; j < 4; ++j)
          x[j] += t.sign * y[j];
      }
      if (s != top) {
        if (excess[s]) {
          const auto y = slash(mom[s], x, excess[s] < 0);
          for (int j = 0; j < 4; ++j)
            x[j] = (m * x[j] + real(excess[s]) * y[j]) / (invariant[s] - m * m);
        } else
          for (auto &v : x)
            v /= invariant[s];
      }
      waves[s] = x;
    }
    return real(direct) * std::pow(e, n - 2) *
           contract(barred(root), waves[top]);
  }
};
} // namespace hepcat::validation::dirac_current
