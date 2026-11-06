#pragma once

// Direct numerical evaluation of the two previously fixed all-positive QED
// formulas. No diagram generator, completion solver or reference answer is used.
// Formula definitions: HEPCAT-Model.wl, constructPhysicalChiralCurrent and
// constructSameChiralSpuriousSubsetCurrent; tests/SIMPLIFICATION.md.
#include "spinas.h"
#include <array>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <stdexcept>
#include <unordered_map>
#include <vector>

namespace hepcat_all_positive {

struct Amplitudes {
  cdouble physical;
  cdouble spurious;
};

// The input is e- e+ -> N photons, with positive-energy physical momenta.
// All photons have fixed positive helicity. Only the two massive spin labels
// remain. The all-ingoing formulas are crossed exactly once in this class.
template<std::size_t N>
class Formulas {
 public:
  static_assert(N >= 2 && N <= 12, "The numerical subset evaluator supports 2 through 12 photons.");
  using FourMomentum = std::array<ldouble, 4>;
  using Momenta = std::array<FourMomentum, N + 2>;
  using Mask = std::uint32_t;
  static constexpr std::size_t subset_count = std::size_t(1) << N;

  Formulas(const Momenta& momenta, ldouble mass, ldouble charge)
      : mass_(mass), charge_(charge), left_pole_(subset_count, 0),
        right_pole_(subset_count, 0), physical_current_(subset_count),
        physical_ready_(subset_count, false),
        spurious_current_(N * subset_count), spurious_ready_(N * subset_count, false),
        ordered_current_(N * subset_count), ordered_ready_(N * subset_count, false) {
    if (!(mass > 0) || !std::isfinite(mass) || !std::isfinite(charge))
      throw std::invalid_argument("All-positive formulas require a finite positive mass and finite charge.");
    for (std::size_t leg = 0; leg < N + 2; ++leg) {
      particles_[leg] = spinas::particle(leg < 2 ? mass : ldouble(0));
      particles_[leg].set_momentum(momenta[leg].data());
    }
    initialize_brackets();
    initialize_poles(momenta);
    physical_coefficient_ = physical_formula();
    spurious_coefficient_ = spurious_formula(false);
  }

  Amplitudes amplitudes(int spin1, int spin2) const {
    const cdouble angle12 = angle12_[spin_index(spin1) * 2 + spin_index(spin2)];
    return {physical_coefficient_ * angle12, spurious_coefficient_ * angle12};
  }

  cdouble physical_coefficient() const { return physical_coefficient_; }
  cdouble spurious_coefficient() const { return spurious_coefficient_; }

  // An ordinary ordered-current recursion checks the determinant-collected
  // spurious recurrence independently, without factorially expanding it.
  cdouble spurious_ordered_coefficient() { return spurious_formula(true); }

 private:
  struct ComplexSum {
    cdouble value = cdouble(0, 0), correction = cdouble(0, 0);
    void add(const cdouble& item) {
      const cdouble adjusted = item - correction;
      const cdouble next = value + adjusted;
      correction = (next - value) - adjusted;
      value = next;
    }
  };

  ldouble mass_, charge_;
  std::array<spinas::particle, N + 2> particles_;
  std::array<cdouble, 4> angle12_;
  std::array<std::array<cdouble, N>, N> angle_, square_, insertion1_, insertion2_;
  std::array<std::array<std::array<cdouble, N>, N>, N> photon_insertion_;
  std::array<cdouble, N> endpoint_sandwich_;
  std::vector<ldouble> left_pole_, right_pole_;
  std::vector<cdouble> physical_current_, spurious_current_, ordered_current_;
  std::vector<bool> physical_ready_, spurious_ready_, ordered_ready_;
  std::unordered_map<std::uint64_t, cdouble> physical_weight_, spurious_weight_;
  cdouble physical_coefficient_, spurious_coefficient_;

  static Mask bit(std::size_t i) { return Mask(1) << i; }
  static Mask all() { return Mask(subset_count - 1); }
  static bool contains(Mask subset, std::size_t i) { return (subset & bit(i)) != 0; }
  static std::size_t count(Mask subset) {
    std::size_t result = 0;
    while (subset) { subset &= subset - 1; ++result; }
    return result;
  }
  static std::size_t spin_index(int spin) {
    if (spin != -1 && spin != 1)
      throw std::invalid_argument("Massive doubled-spin labels must be -1 or +1.");
    return spin == -1 ? 0 : 1;
  }
  static ldouble dot(const FourMomentum& a, const FourMomentum& b) {
    ldouble value = std::fma(-a[1], b[1], a[0] * b[0]);
    value = std::fma(-a[2], b[2], value);
    return std::fma(-a[3], b[3], value);
  }
  static cdouble divide(const cdouble& numerator, ldouble denominator) {
    if (denominator == 0 || !std::isfinite(denominator))
      throw std::runtime_error("A physical propagator is singular or nonfinite at this phase-space point.");
    return numerator / denominator;
  }
  static cdouble divide(const cdouble& numerator, const cdouble& denominator) {
    if (denominator == cdouble(0, 0) || !std::isfinite(std::real(denominator)) ||
        !std::isfinite(std::imag(denominator)))
      throw std::runtime_error("A reference bracket is singular or nonfinite at this phase-space point.");
    return numerator / denominator;
  }

  void initialize_brackets() {
    spinas::sproduct angle12(ANGLE, &particles_[0], &particles_[1]);
    for (int first : {-1, 1}) for (int second : {-1, 1})
      angle12_[spin_index(first) * 2 + spin_index(second)] = angle12.v(first, second);
    for (std::size_t i = 0; i < N; ++i) {
      spinas::sproduct sandwich(SQUARE, &particles_[i + 2], &particles_[0],
                                &particles_[1], &particles_[i + 2]);
      endpoint_sandwich_[i] = sandwich.v();
      for (std::size_t j = 0; j < N; ++j) {
        if (i == j) {
          angle_[i][j] = square_[i][j] = insertion1_[i][j] = insertion2_[i][j] = 0;
        } else {
          spinas::sproduct angle(ANGLE, &particles_[i + 2], &particles_[j + 2]);
          spinas::sproduct square(SQUARE, &particles_[i + 2], &particles_[j + 2]);
          spinas::sproduct first(ANGLE, &particles_[i + 2], &particles_[0], &particles_[j + 2]);
          spinas::sproduct second(ANGLE, &particles_[i + 2], &particles_[1], &particles_[j + 2]);
          // Both photon angle spinors flip in <ij>, and neither flips in [ij].
          // One photon angle spinor flips in <i|p1|j] and <i|p2|j].
          angle_[i][j] = angle.v();
          square_[i][j] = square.v();
          insertion1_[i][j] = -first.v();
          insertion2_[i][j] = -second.v();
        }
        for (std::size_t k = 0; k < N; ++k) {
          if (i == j || i == k || j == k) photon_insertion_[i][k][j] = 0;
          else {
            spinas::sproduct middle(ANGLE, &particles_[i + 2], &particles_[k + 2], &particles_[j + 2]);
            // The reference angle and the inserted outgoing photon momentum
            // each flip sign, so this crossed insertion has a plus sign.
            photon_insertion_[i][k][j] = middle.v();
          }
        }
      }
    }
  }

  void initialize_poles(const Momenta& momenta) {
    // External on-shell identities only. Expanding the invariant avoids
    // subtracting m^2 from a nearly equal composite square. No internal line
    // is represented as an on-shell SPINAS particle.
    for (Mask subset = 1; subset < subset_count; ++subset) {
      ldouble left = 0, right = 0;
      for (std::size_t i = 0; i < N; ++i) if (contains(subset, i)) {
        left -= 2 * dot(momenta[0], momenta[i + 2]);
        right -= 2 * dot(momenta[1], momenta[i + 2]);
        for (std::size_t j = i + 1; j < N; ++j) if (contains(subset, j)) {
          const ldouble pair = 2 * dot(momenta[i + 2], momenta[j + 2]);
          left += pair; right += pair;
        }
      }
      left_pole_[subset] = left;
      right_pole_[subset] = right;
    }
  }

  cdouble physical_leading(Mask subset) const {
    cdouble result(1, 0);
    for (std::size_t i = 0; i < N; ++i) if (contains(subset, i))
      result *= divide(endpoint_sandwich_[i], right_pole_[bit(i)]);
    return result;
  }
  cdouble physical_emission(std::size_t i, Mask subset) const {
    ComplexSum result;
    for (std::size_t j = 0; j < N; ++j) if (contains(subset, j))
      result.add(square_[i][j] * insertion1_[j][i]);
    return result.value;
  }
  cdouble physical_weight(Mask subset, Mask pair) {
    const std::uint64_t key = (std::uint64_t(subset) << N) | pair;
    const auto previous = physical_weight_.find(key);
    if (previous != physical_weight_.end()) return previous->second;
    const Mask rest = subset & ~pair;
    cdouble leading(1, 0);
    for (std::size_t i = 0; i < N; ++i) if (contains(rest, i)) leading *= endpoint_sandwich_[i];
    ComplexSum result; result.add(leading);
    for (std::size_t i = 0; i < N; ++i) if (contains(rest, i)) {
      const Mask remaining = subset & ~bit(i);
      result.add(divide(physical_emission(i, all() & ~subset) * right_pole_[bit(i)] *
                        physical_weight(remaining, pair), right_pole_[remaining]));
      for (std::size_t j = i + 1; j < N; ++j) if (contains(rest, j)) {
        const Mask remaining_pair = subset & ~bit(i) & ~bit(j);
        result.add(-divide(mass_ * mass_ * square_[i][j] * square_[i][j] *
                    right_pole_[bit(i)] * right_pole_[bit(j)] *
                    physical_weight(remaining_pair, pair), right_pole_[remaining_pair]));
      }
    }
    physical_weight_.emplace(key, result.value);
    return result.value;
  }
  cdouble physical_current(Mask subset) {
    if (physical_ready_[subset]) return physical_current_[subset];
    ComplexSum result; result.add(physical_leading(subset));
    if (count(subset) >= 2) {
      const ldouble denominator = right_pole_[subset] * single_pole_product(subset);
      for (std::size_t i = 0; i < N; ++i) if (contains(subset, i)) {
        for (std::size_t j = i + 1; j < N; ++j) if (contains(subset, j)) {
          const Mask pair = bit(i) | bit(j);
          result.add(-divide(mass_ * mass_ * square_[i][j] * square_[i][j] *
                     left_pole_[bit(i)] * left_pole_[bit(j)] * physical_weight(subset, pair),
                     denominator));
        }
      }
    }
    physical_ready_[subset] = true;
    return physical_current_[subset] = result.value;
  }
  ldouble single_pole_product(Mask subset) const {
    ldouble result = 1;
    for (std::size_t i = 0; i < N; ++i) if (contains(subset, i)) result *= right_pole_[bit(i)];
    return result;
  }
  cdouble physical_formula() {
    ComplexSum result;
    ldouble denominator = 1, coupling = 1;
    const ldouble vertex = std::sqrt(ldouble(2)) * charge_;
    for (std::size_t i = 0; i < N; ++i) {
      denominator *= left_pole_[bit(i)];
      coupling *= vertex;
      for (std::size_t j = i + 1; j < N; ++j)
        result.add(square_[i][j] * square_[i][j] * physical_current(all() & ~bit(i) & ~bit(j)));
    }
    return divide(coupling * mass_ * result.value, denominator);
  }

  cdouble ratio(std::size_t reference, std::size_t photon) const {
    return divide(insertion1_[reference][photon], angle_[reference][photon]);
  }
  cdouble spurious_leading(std::size_t reference, Mask subset) const {
    cdouble result(1, 0);
    for (std::size_t i = 0; i < N; ++i) if (contains(subset, i)) result *= ratio(reference, i);
    return result;
  }
  cdouble spurious_emission(std::size_t reference, Mask subset, std::size_t photon) const {
    ComplexSum result;
    if (N - count(subset) < count(subset)) {
      result.add(-insertion2_[reference][photon]);
      const Mask complement = all() & ~subset & ~bit(reference);
      for (std::size_t j = 0; j < N; ++j) if (contains(complement, j))
        result.add(-photon_insertion_[reference][j][photon]);
    } else {
      result.add(insertion1_[reference][photon]);
      const Mask earlier = subset & ~bit(photon);
      for (std::size_t j = 0; j < N; ++j) if (contains(earlier, j))
        result.add(photon_insertion_[reference][j][photon]);
    }
    return divide(result.value, angle_[reference][photon]);
  }
  cdouble spurious_weight(std::size_t reference, Mask subset, Mask pair) {
    const std::uint64_t key = ((std::uint64_t(reference) * subset_count + subset) << N) | pair;
    const auto previous = spurious_weight_.find(key);
    if (previous != spurious_weight_.end()) return previous->second;
    const Mask rest = subset & ~pair;
    ComplexSum result; result.add(spurious_leading(reference, rest));
    for (std::size_t i = 0; i < N; ++i) if (contains(rest, i))
      result.add(left_pole_[bit(i)] * spurious_emission(reference, subset, i) *
                 spurious_weight(reference, subset & ~bit(i), pair));
    const cdouble answer = divide(result.value, left_pole_[subset]);
    spurious_weight_.emplace(key, answer);
    return answer;
  }
  cdouble spurious_current(std::size_t reference, Mask subset) {
    const std::size_t key = reference * subset_count + subset;
    if (spurious_ready_[key]) return spurious_current_[key];
    ComplexSum result; result.add(spurious_leading(reference, subset));
    ldouble denominator = 1;
    for (std::size_t i = 0; i < N; ++i) if (contains(subset, i)) {
      denominator *= left_pole_[bit(i)];
      for (std::size_t j = i + 1; j < N; ++j) if (contains(subset, j))
        result.add(-mass_ * mass_ * square_[i][j] * square_[i][j] *
                   spurious_weight(reference, subset, bit(i) | bit(j)));
    }
    spurious_ready_[key] = true;
    return spurious_current_[key] = divide(result.value, denominator);
  }
  cdouble ordered_current(std::size_t reference, Mask subset) {
    const std::size_t key = reference * subset_count + subset;
    if (ordered_ready_[key]) return ordered_current_[key];
    if (!subset) { ordered_ready_[key] = true; return ordered_current_[key] = cdouble(1, 0); }
    ComplexSum result;
    for (std::size_t i = 0; i < N; ++i) if (contains(subset, i))
      result.add(ordered_current(reference, subset & ~bit(i)) * spurious_emission(reference, subset, i));
    ordered_ready_[key] = true;
    return ordered_current_[key] = divide(result.value, left_pole_[subset]);
  }
  cdouble spurious_formula(bool ordered) {
    ComplexSum result;
    ldouble coupling = 1;
    const ldouble vertex = -std::sqrt(ldouble(2)) * charge_;
    for (std::size_t i = 0; i < N; ++i) {
      coupling *= vertex;
      for (std::size_t j = i + 1; j < N; ++j) {
        const Mask rest = all() & ~bit(i) & ~bit(j);
        const cdouble first = ordered ? ordered_current(i, rest) : spurious_current(i, rest);
        const cdouble second = ordered ? ordered_current(j, rest) : spurious_current(j, rest);
        result.add(divide(square_[i][j], angle_[i][j]) *
                   (divide(first, right_pole_[bit(i)]) + divide(second, right_pole_[bit(j)])));
      }
    }
    return coupling * mass_ * result.value;
  }
};

} // namespace hepcat_all_positive
