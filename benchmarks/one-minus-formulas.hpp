#pragma once

// Massive QED with one negative photon and any number of positive photons.
// Analytic formula and derivation: tests/QED-one-minus-formula.wl and
// benchmarks/one-minus/README.md. No diagram/completion code is called here.
#include "spinas.h"
#include <array>
#include <cmath>
#include <cstdint>
#include <stdexcept>
#include <vector>

namespace hepcat_one_minus {

template<std::size_t N>
class Formula {
 public:
  static_assert(N >= 2 && N <= 12, "Use 2 through 12 photons.");
  using FourMomentum = std::array<ldouble, 4>;
  using Momenta = std::array<FourMomentum, N + 2>;
  using Mask = std::uint32_t;
  static constexpr std::size_t subset_count = std::size_t(1) << (N - 1);

  // Physical positive-energy e- e+ -> N photons. negative_photon is a zero-based
  // photon index, not an external-leg label. Photon crossing is applied once.
  Formula(const Momenta& momenta, ldouble mass, ldouble charge,
          std::size_t negative_photon = N - 1,
          const FourMomentum* square_reference = nullptr)
      : orbital_(0, 0), spin_(0, 0), ward_(0, 0) {
    if (!(mass > 0) || !std::isfinite(mass) || !std::isfinite(charge))
      throw std::invalid_argument("The one-minus formula requires positive mass and finite charge.");
    if (negative_photon >= N)
      throw std::invalid_argument("The negative photon index is out of range.");
    const std::size_t negative = negative_photon + 2;
    std::array<spinas::particle, N + 2> particles;
    for (std::size_t leg = 0; leg < N + 2; ++leg) {
      particles[leg] = spinas::particle(leg < 2 ? mass : ldouble(0));
      particles[leg].set_momentum(momenta[leg].data());
    }
    std::array<std::size_t, N - 1> positive;
    for (std::size_t leg = 2, cursor = 0; leg < N + 2; ++leg)
      if (leg != negative) positive[cursor++] = leg;
    // Reference independence permits a geometric choice. Avoid a nearly
    // collinear default and make it independent of photon list order. Dividing
    // by E_q removes the arbitrary energy normalization of the reference.
    std::size_t reference_leg = positive[0];
    ldouble best_separation = -1;
    if (!square_reference) for (const auto leg : positive) {
      spinas::sproduct bracket(SQUARE, &particles[negative], &particles[leg]);
      const ldouble separation = std::norm(bracket.v()) / momenta[leg][0];
      if (separation > best_separation) {
        best_separation = separation;
        reference_leg = leg;
      }
    }
    spinas::particle reference(0);
    reference.set_momentum(square_reference ? square_reference->data()
                                          : momenta[reference_leg].data());
    spinas::sproduct reference_bracket(SQUARE, &particles[negative], &reference);
    const cdouble reference_denominator = reference_bracket.v();
    if (reference_denominator == cdouble(0, 0))
      throw std::invalid_argument("Choose a noncollinear square reference for the negative photon.");

    std::array<std::vector<cdouble>, 2> current;
    std::array<std::array<cdouble, N - 1>, N - 1> photon_vertex;
    std::array<cdouble, N - 1> angle, reference_vertex, ward_vertex;
    std::array<std::array<cdouble, N - 1>, 2> endpoint_vertex;
    for (std::size_t endpoint = 0; endpoint < 2; ++endpoint) {
      current[endpoint].resize(subset_count);
      current[endpoint][0] = cdouble(1, 0);
    }
    for (std::size_t i = 0; i < N - 1; ++i) {
      const std::size_t leg = positive[i];
      spinas::sproduct bracket(ANGLE, &particles[negative], &particles[leg]);
      angle[i] = bracket.v();
      if (angle[i] == cdouble(0, 0))
        throw std::invalid_argument("A positive photon is collinear with the chosen angle reference.");
      for (std::size_t endpoint = 0; endpoint < 2; ++endpoint) {
        spinas::sproduct chain(ANGLE, &particles[negative], &particles[endpoint], &particles[leg]);
        endpoint_vertex[endpoint][i] = -chain.v();
      }
      spinas::sproduct ref(ANGLE, &particles[negative], &particles[leg], &reference);
      reference_vertex[i] = ref.v();
      spinas::sproduct wrd(ANGLE, &particles[negative], &particles[leg], &particles[negative]);
      ward_vertex[i] = wrd.v();
      for (std::size_t j = 0; j < N - 1; ++j) {
        if (i == j) photon_vertex[i][j] = cdouble(0, 0);
        else {
          spinas::sproduct chain(ANGLE, &particles[negative], &particles[positive[j]], &particles[leg]);
          photon_vertex[i][j] = chain.v();
        }
      }
    }
    // Cache each subset's current. Every internal virtuality remains off shell.
    for (std::size_t endpoint = 0; endpoint < 2; ++endpoint)
      for (Mask subset = 1; subset < subset_count; ++subset) {
        ComplexSum sum;
        ldouble denominator = 0;
        for (std::size_t i = 0; i < N - 1; ++i) if (subset & bit(i)) {
          denominator -= 2 * dot(momenta[endpoint], momenta[positive[i]]);
          for (std::size_t j = i + 1; j < N - 1; ++j) if (subset & bit(j))
            denominator += 2 * dot(momenta[positive[i]], momenta[positive[j]]);
          const Mask previous = subset & ~bit(i);
          ComplexSum vertex;
          vertex.add(endpoint_vertex[endpoint][i]);
          for (std::size_t j = 0; j < N - 1; ++j) if (previous & bit(j))
            vertex.add(photon_vertex[i][j]);
          sum.add(current[endpoint][previous] * vertex.value / angle[i]);
        }
        if (denominator == 0 || !std::isfinite(denominator))
          throw std::runtime_error("A subset lies on a physical fermion pole.");
        current[endpoint][subset] = sum.value / denominator;
      }
    spinas::sproduct initial(ANGLE, &particles[negative], &particles[0], &reference);
    spinas::sproduct initial_ward(ANGLE, &particles[negative], &particles[0], &particles[negative]);
    ComplexSum orbital, spin, ward;
    for (Mask left = 0; left < subset_count; ++left) {
      const Mask right = Mask(subset_count - 1) ^ left;
      const ldouble sign = count(right) % 2 ? -1 : 1;
      const cdouble weight = sign * current[0][left] * current[1][right];
      ComplexSum vertex, ward_numerator;
      vertex.add(-initial.v());
      ward_numerator.add(-initial_ward.v());
      for (std::size_t j = 0; j < N - 1; ++j) if (left & bit(j)) {
        vertex.add(reference_vertex[j]);
        ward_numerator.add(ward_vertex[j]);
      }
      orbital.add(weight * vertex.value / reference_denominator);
      spin.add(weight);
      ward.add(weight * ward_numerator.value);
    }
    orbital_ = orbital.value;
    spin_ = spin.value;
    ward_ = ward.value;
    ldouble coefficient = 1 / mass;
    for (std::size_t photon = 0; photon < N; ++photon)
      coefficient *= -std::sqrt(ldouble(2)) * charge;
    spinas::sproduct angle12(ANGLE, &particles[0], &particles[1]);
    spinas::sproduct angle1r(ANGLE, &particles[0], &particles[negative]);
    spinas::sproduct angle2r(ANGLE, &particles[1], &particles[negative]);
    for (int first : {-1, 1}) for (int second : {-1, 1}) {
      // Each photon angle spinor changes sign under outgoing crossing, so
      // <1r><2r> has no net crossing sign. The two coefficients above are crossed.
      amplitudes_[index(first) * 2 + index(second)] = coefficient *
        (angle12.v(first, second) * orbital_ + angle1r.v(first) * angle2r.v(second) * spin_);
    }
  }

  cdouble amplitude(int first_spin, int second_spin) const {
    return amplitudes_[index(first_spin) * 2 + index(second_spin)];
  }
  ldouble spin_sum() const {
    ldouble result = 0;
    for (const auto& value : amplitudes_) result += std::norm(value);
    return result;
  }
  cdouble orbital_coefficient() const { return orbital_; }
  cdouble spin_coefficient() const { return spin_; }
  cdouble ward_identity() const { return ward_; }

 private:
  struct ComplexSum {
    cdouble value = cdouble(0, 0), correction = cdouble(0, 0);
    void add(cdouble x) {
      const cdouble adjusted = x - correction, next = value + adjusted;
      correction = (next - value) - adjusted;
      value = next;
    }
  };
  static Mask bit(std::size_t i) { return Mask(1) << i; }
  static unsigned count(Mask subset) {
    unsigned n = 0;
    for (; subset; subset &= subset - 1) ++n;
    return n;
  }
  static std::size_t index(int spin) {
    if (spin != -1 && spin != 1) throw std::invalid_argument("Use massive doubled spins -1 or +1.");
    return spin == -1 ? 0 : 1;
  }
  static ldouble dot(const FourMomentum& a, const FourMomentum& b) {
    ldouble result = std::fma(-a[1], b[1], a[0] * b[0]);
    result = std::fma(-a[2], b[2], result);
    return std::fma(-a[3], b[3], result);
  }
  std::array<cdouble, 4> amplitudes_;
  cdouble orbital_, spin_, ward_;
};

} // namespace hepcat_one_minus
