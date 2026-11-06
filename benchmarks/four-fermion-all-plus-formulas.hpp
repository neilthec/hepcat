#pragma once

// Two massive electron/positron pairs and N positive-helicity photons.
// Exact second-order rewriting of ordinary tree QED. Internal lines remain
// off shell, including in numerators. No constructive completion is invoked.
#include "spinas.h"
#include <algorithm>
#include <array>
#include <cmath>
#include <cstdint>
#include <limits>
#include <stdexcept>
#include <vector>

namespace hepcat_four_fermion_all_plus {

template<std::size_t N>
class Formula {
 public:
  static_assert(N <= 12, "The cached numerical evaluator supports up to 12 photons.");
  using FourMomentum = std::array<ldouble, 4>;
  using Momenta = std::array<FourMomentum, N + 4>;
  using Spins = std::array<int, 4>;
  using Mask = std::uint32_t;
  using ComplexVector = std::array<cdouble, 4>;
  static constexpr std::size_t subset_count = std::size_t(1) << N;

  // Physical positive-energy e- e+ -> e- e+ + N photons. The corresponding
  // all-incoming labels are {e,E,E,e,photons}. Reference, if supplied, is null.
  Formula(const Momenta& momenta, ldouble mass, ldouble charge,
          const FourMomentum* common_angle_reference = nullptr)
      : coefficient_(charge * charge / (mass * mass)),
        ward_relative_(0), component_ward_relative_(0) {
    if (!(mass > 0) || !std::isfinite(mass) || !std::isfinite(charge))
      throw std::invalid_argument("Use a finite positive fermion mass and finite charge.");
    for (std::size_t leg = 0; leg < N + 4; ++leg) {
      particles_[leg] = spinas::particle(leg < 4 ? mass : ldouble(0));
      particles_[leg].set_momentum(momenta[leg].data());
      for (std::size_t mu = 0; mu < 4; ++mu)
        incoming_[leg][mu] = (leg < 2 ? 1 : -1) * momenta[leg][mu];
    }
    subset_momentum_.resize(subset_count);
    subset_momentum_[0] = FourMomentum{{0, 0, 0, 0}};
    for (Mask subset = 1; subset < subset_count; ++subset) {
      const std::size_t photon = first(subset);
      subset_momentum_[subset] = add(subset_momentum_[subset & ~bit(photon)], incoming_[photon + 4]);
    }
    reference_ = spinas::particle(0);
    const FourMomentum selected = common_angle_reference ? *common_angle_reference
                                                         : choose_reference(momenta, mass);
    reference_.set_momentum(selected.data());
    for (std::size_t photon = 0; photon < N; ++photon) {
      coefficient_ *= -std::sqrt(ldouble(2)) * charge;
      spinas::sproduct bracket(ANGLE, &reference_, &particles_[photon + 4]);
      reference_bracket_[photon] = -bracket.v(); // outgoing photon angle
      if (reference_bracket_[photon] == cdouble(0, 0))
        throw std::invalid_argument("The common angle reference is collinear with a photon.");
      for (std::size_t mu = 0; mu < 4; ++mu)
        polarization_[photon][mu] = metric(mu) *
          (reference_.langle() * sigma(mu, LOWER) * particles_[photon + 4].rsquare()) /
          (std::sqrt(ldouble(2)) * reference_bracket_[photon]);
    }
    for (std::size_t endpoint = 0; endpoint < 4; ++endpoint) build_scalar_current(endpoint, momenta);
    // Pairings (1,2)(4,3) and (1,3)(4,2), in the all-incoming convention.
    build_line(0, 0, 1);
    build_line(1, 3, 2);
    build_line(2, 0, 2);
    build_line(3, 3, 1);
    for (std::size_t pairing = 0; pairing < 2; ++pairing) {
      photon_pole_[pairing].resize(subset_count);
      const std::size_t partner = pairing == 0 ? 1 : 2;
      for (Mask subset = 0; subset < subset_count; ++subset) {
        const FourMomentum k = add(add(incoming_[0], incoming_[partner]), subset_momentum_[subset]);
        const ldouble denominator = dot(k, k);
        if (denominator == 0 || !std::isfinite(denominator))
          throw std::runtime_error("A sampled point is singular at a physical photon propagator.");
        photon_pole_[pairing][subset] = denominator;
      }
    }
  }

  cdouble amplitude(const Spins& spins) const {
    for (const int spin : spins) index(spin);
    ComplexSum result;
    for (Mask left = 0; left < subset_count; ++left) {
      const Mask right = Mask(subset_count - 1) ^ left;
      result.add(dot(line_[0][left][component(spins[0], spins[1])],
                     line_[1][right][component(spins[3], spins[2])]) / photon_pole_[0][left]);
      result.add(-dot(line_[2][left][component(spins[0], spins[2])],
                      line_[3][right][component(spins[3], spins[1])]) / photon_pole_[1][left]);
    }
    return coefficient_ * result.value;
  }

  ldouble spin_sum() const {
    ldouble result = 0;
    for (int a : {-1, 1}) for (int b : {-1, 1})
      for (int c : {-1, 1}) for (int d : {-1, 1})
        result += std::norm(amplitude(Spins{{a, b, c, d}}));
    return result;
  }
  ldouble ward_relative_error() const { return ward_relative_; }
  ldouble component_ward_relative_error() const { return component_ward_relative_; }

 private:
  struct ComplexSum {
    cdouble value = cdouble(0, 0), correction = cdouble(0, 0);
    void add(cdouble item) {
      const cdouble adjusted = item - correction, next = value + adjusted;
      correction = (next - value) - adjusted;
      value = next;
    }
  };
  using SpinVectors = std::array<ComplexVector, 4>;
  static Mask bit(std::size_t i) { return Mask(1) << i; }
  static std::size_t first(Mask subset) {
    std::size_t result = 0;
    while (!(subset & 1)) { ++result; subset >>= 1; }
    return result;
  }
  static unsigned count(Mask subset) {
    unsigned result = 0;
    for (; subset; subset &= subset - 1) ++result;
    return result;
  }
  static ldouble metric(std::size_t mu) { return mu == 0 ? 1 : -1; }
  static std::size_t index(int spin) {
    if (spin != -1 && spin != 1) throw std::invalid_argument("Use massive doubled spins -1 or +1.");
    return spin == -1 ? 0 : 1;
  }
  static std::size_t component(int a, int b) { return 2 * index(a) + index(b); }
  static FourMomentum add(FourMomentum a, const FourMomentum& b) {
    for (std::size_t mu = 0; mu < 4; ++mu) a[mu] += b[mu];
    return a;
  }
  static ldouble dot(const FourMomentum& a, const FourMomentum& b) {
    ldouble value = std::fma(-a[1], b[1], a[0] * b[0]);
    value = std::fma(-a[2], b[2], value);
    return std::fma(-a[3], b[3], value);
  }
  static cdouble dot(const ComplexVector& a, const ComplexVector& b) {
    return a[0] * b[0] - a[1] * b[1] - a[2] * b[2] - a[3] * b[3];
  }
  static spinas::cmatrix sigma(std::size_t mu, bool upper) {
    FourMomentum unit{{0, 0, 0, 0}};
    unit[mu] = 1;
    return spinas::cmatrix(unit.data(), upper);
  }
  static FourMomentum choose_reference(const Momenta& momenta, ldouble mass) {
    FourMomentum best{{1, 0, 1, 0}};
    ldouble best_score = -1;
    for (std::size_t h = 4; h < N + 4; ++h) {
      // Null direction proportional to p1|h]; its normalization cancels.
      const ldouble product = dot(momenta[0], momenta[h]);
      FourMomentum candidate;
      for (std::size_t mu = 0; mu < 4; ++mu)
        candidate[mu] = 2 * product * momenta[0][mu] - mass * mass * momenta[h][mu];
      const ldouble length = std::sqrt(candidate[1] * candidate[1] +
        candidate[2] * candidate[2] + candidate[3] * candidate[3]);
      if (!(length > 0) || !std::isfinite(length)) continue;
      candidate[0] = 1;
      for (std::size_t mu = 1; mu < 4; ++mu) candidate[mu] /= length;
      ldouble separation = std::numeric_limits<ldouble>::infinity();
      for (std::size_t i = 4; i < N + 4; ++i)
        separation = std::min(separation, dot(candidate, momenta[i]) / momenta[i][0]);
      if (separation > best_score) { best_score = separation; best = candidate; }
    }
    return best;
  }
  void build_scalar_current(std::size_t endpoint, const Momenta& momenta) {
    auto& current = scalar_current_[endpoint];
    current.resize(subset_count);
    current[0] = cdouble(1, 0);
    for (Mask subset = 1; subset < subset_count; ++subset) {
      ComplexSum result;
      ldouble denominator = 0;
      for (std::size_t i = 0; i < N; ++i) if (subset & bit(i)) {
        denominator -= 2 * (endpoint < 2 ? 1 : -1) * dot(momenta[endpoint], momenta[i + 4]);
        for (std::size_t j = i + 1; j < N; ++j) if (subset & bit(j))
          denominator += 2 * dot(momenta[i + 4], momenta[j + 4]);
        const Mask previous = subset & ~bit(i);
        const FourMomentum Q = add(incoming_[endpoint], subset_momentum_[previous]);
        const cdouble vertex = reference_.langle() * spinas::cmatrix(Q.data(), LOWER) *
          particles_[i + 4].rsquare();
        result.add(current[previous] * vertex / reference_bracket_[i]);
      }
      if (denominator == 0 || !std::isfinite(denominator))
        throw std::runtime_error("A sampled subset lies on a physical fermion propagator.");
      current[subset] = result.value / denominator;
    }
  }
  void build_line(std::size_t family, std::size_t a, std::size_t b) {
    std::vector<cdouble> density(subset_count);
    auto& values = line_[family];
    values.resize(subset_count);
    for (Mask subset = 0; subset < subset_count; ++subset) {
      ComplexSum scalar;
      std::array<ComplexSum, 4> orbital;
      for (Mask left = subset;; left = (left - 1) & subset) {
        const Mask right = subset ^ left;
        const cdouble weight = ldouble(count(right) % 2 ? -1 : 1) *
          scalar_current_[a][left] * scalar_current_[b][right];
        scalar.add(weight);
        const FourMomentum Q = add(incoming_[a], subset_momentum_[left]);
        for (std::size_t mu = 0; mu < 4; ++mu) orbital[mu].add(ldouble(2) * weight * Q[mu]);
        if (!left) break;
      }
      density[subset] = scalar.value;
      // Required two-photon vertex of the second-order Dirac rewriting.
      // Only one photon here is internal. It is not an independent QED coupling.
      for (std::size_t i = 0; i < N; ++i) if (subset & bit(i))
        for (std::size_t mu = 0; mu < 4; ++mu)
          orbital[mu].add(-std::sqrt(ldouble(2)) * density[subset & ~bit(i)] * polarization_[i][mu]);
      FourMomentum k = add(add(incoming_[a], incoming_[b]), subset_momentum_[subset]);
      for (auto& entry : k) entry = -entry;
      ldouble tensor_divergence_norm = 0, tensor_current_norm = 0, momentum_norm = 0;
      for (const auto entry : k) momentum_norm += entry * entry;
      for (int first_spin : {-1, 1}) for (int second_spin : {-1, 1}) {
        const ldouble crossing = (a < 2 ? 1 : -1) * (b < 2 ? 1 : -1);
        spinas::sproduct bracket(ANGLE, &particles_[a], &particles_[b]);
        const cdouble ab = crossing * bracket.v(first_spin, second_spin);
        auto& vector = values[subset][component(first_spin, second_spin)];
        for (std::size_t mu = 0; mu < 4; ++mu)
          vector[mu] = ab * orbital[mu].value + crossing * scalar.value *
            (particles_[a].langle(first_spin) * sigma(mu, UPPER) *
             spinas::cmatrix(k.data(), UPPER) * particles_[b].rangle(second_spin));
        ComplexSum divergence;
        ldouble k_norm = 0, vector_norm = 0;
        for (std::size_t mu = 0; mu < 4; ++mu) {
          divergence.add(metric(mu) * k[mu] * vector[mu]);
          k_norm += k[mu] * k[mu]; vector_norm += std::norm(vector[mu]);
        }
        const ldouble scale = std::sqrt(k_norm * vector_norm);
        const ldouble error = scale ? std::abs(divergence.value) / scale : std::abs(divergence.value);
        component_ward_relative_ = std::max(component_ward_relative_, error);
        tensor_divergence_norm += std::norm(divergence.value);
        tensor_current_norm += vector_norm;
      }
      // Normalize the complete coherent spin tensor. Dividing by one tiny,
      // helicity-suppressed component gives a basis-dependent apparent failure.
      // Retain that old component diagnostic above; no component is omitted.
      const ldouble tensor_scale = std::sqrt(momentum_norm * tensor_current_norm);
      const ldouble tensor_error = tensor_scale ? std::sqrt(tensor_divergence_norm) / tensor_scale
                                               : std::sqrt(tensor_divergence_norm);
      ward_relative_ = std::max(ward_relative_, tensor_error);
    }
  }

  ldouble coefficient_, ward_relative_, component_ward_relative_;
  std::array<spinas::particle, N + 4> particles_;
  spinas::particle reference_;
  Momenta incoming_;
  std::vector<FourMomentum> subset_momentum_;
  std::array<std::vector<cdouble>, 4> scalar_current_;
  std::array<cdouble, N> reference_bracket_;
  std::array<ComplexVector, N> polarization_;
  std::array<std::vector<SpinVectors>, 4> line_;
  std::array<std::vector<ldouble>, 2> photon_pole_;
};

} // namespace hepcat_four_fermion_all_plus
