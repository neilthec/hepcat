#ifndef HEPCAT_SPINAS_VALIDATION_HPP
#define HEPCAT_SPINAS_VALIDATION_HPP

// Compare the HEPCAT/SPINAS and reference squared amplitudes at the same
// randomly generated physical momenta. A run passes only if every point agrees.
//
// Reading guide:
//   1. Settings and results.
//   2. Random phase-space generation: make_random_momenta(...).
//   3. Point-by-point comparison: compare_squared_amplitudes(...).
//   4. Command-line options and printed reports.
//
// These are validation samples, not a uniformly weighted phase-space integral.

#include "spinas-runtime.hpp"
#include <cstdint>
#include <iomanip>
#include <ostream>
#include <random>
#include <sstream>

namespace hepcat_spinas {

// ============================================================================
// 1. Settings and results
// ============================================================================

struct ValidationOptions {
  std::size_t points = 1000;
  std::uint64_t seed = UINT64_C(12345);
  ldouble relative_tolerance = 1e-7L;
  // A fixed absolute tolerance can hide a wrong zero for a small amplitude.
  // Users may supply one explicitly when their numerical problem requires it.
  ldouble absolute_tolerance = 0;
  // Zero selects an automatic bound above the incoming and outgoing thresholds.
  ldouble minimum_center_of_mass_energy = 0;
  ldouble maximum_center_of_mass_energy = 0;
  // Zero disables progress messages.
  std::size_t progress_every = 100;
};

struct ValidationFailure {
  // Human-facing indices start at one. Together with the seed and settings,
  // the full common momentum array makes this point independently replayable.
  std::size_t point_index = 0;
  std::vector<FourMomentum> momenta;
  ldouble hepcat_value = std::numeric_limits<ldouble>::quiet_NaN();
  ldouble reference_value = std::numeric_limits<ldouble>::quiet_NaN();
  ldouble absolute_error = std::numeric_limits<ldouble>::quiet_NaN();
  ldouble relative_error = std::numeric_limits<ldouble>::quiet_NaN();
  std::string message;
};

struct ValidationReport {
  bool passed = false;
  std::string status = "NOT RUN";
  std::string message;
  std::uint64_t seed = 0;
  std::size_t requested_points = 0;
  std::size_t generated_points = 0;
  // A point is compared only when both evaluations are finite and nonnegative.
  std::size_t compared_points = 0;
  std::size_t mismatches = 0;
  std::size_t evaluation_errors = 0;
  ldouble maximum_relative_error = 0;
  ldouble maximum_absolute_error = 0;
  ldouble minimum_center_of_mass_energy = 0;
  ldouble maximum_center_of_mass_energy = 0;
  bool has_first_failure = false;
  ValidationFailure first_failure;
  std::vector<std::string> errors;
};

inline ValidationReport unavailable_validation(const ValidationOptions& options,
                                               const std::string& message) {
  ValidationReport report;
  report.status = "UNAVAILABLE";
  report.requested_points = options.points;
  report.seed = options.seed;
  report.message = message;
  return report;
}

inline ValidationReport failed_validation(const ValidationOptions& options,
                                          const std::string& message) {
  ValidationReport report = unavailable_validation(options, message);
  report.status = "ERROR";
  report.errors.push_back(message);
  return report;
}

namespace detail {

// Convert one random-engine draw to a number in [0, 1), using its upper 53 bits.
// Keeping this conversion explicit preserves the random draw sequence across
// standard-library implementations; uniform_real_distribution may draw differently.
inline ldouble validation_uniform(std::mt19937_64& engine) {
  return static_cast<ldouble>(engine() >> 11) / ldouble(9007199254740992.0L);
}

template <std::size_t LegCount>
ldouble validation_threshold(const std::array<ldouble, LegCount>& masses,
                            const std::array<std::size_t, 2>& incoming) {
  check_external_masses(masses, incoming);
  ldouble outgoing_mass_sum = 0;
  for (std::size_t leg = 0; leg < LegCount; ++leg) {
    if (leg != incoming[0] && leg != incoming[1]) {
      outgoing_mass_sum += masses[leg];
    }
  }
  const ldouble incoming_mass_sum = masses[incoming[0]] + masses[incoming[1]];
  return std::max(outgoing_mass_sum, incoming_mass_sum);
}

template <std::size_t LegCount>
std::array<ldouble, 2> validation_energy_range(
    const std::array<ldouble, LegCount>& masses,
    const std::array<std::size_t, 2>& incoming,
    const ValidationOptions& options) {
  const ldouble threshold = validation_threshold(masses, incoming);
  const ldouble ordinary_energy = example_center_of_mass_energy(masses, incoming);
  const ldouble minimum_energy = options.minimum_center_of_mass_energy == 0
      ? ordinary_energy / 2 : options.minimum_center_of_mass_energy;
  const ldouble maximum_energy = options.maximum_center_of_mass_energy == 0
      ? ordinary_energy * 2 : options.maximum_center_of_mass_energy;
  if (!std::isfinite(minimum_energy) || !std::isfinite(maximum_energy)
      || minimum_energy <= threshold || maximum_energy < minimum_energy) {
    throw std::invalid_argument(
      "Validation energies must be finite, ordered and above both external thresholds.");
  }
  return {{minimum_energy, maximum_energy}};
}

// Keep a short list of distinct explanations, rather than repeating one error
// for every sampled point. Counters still include every failure.
inline void validation_record_error(ValidationReport& report, const std::string& message) {
  if (report.errors.size() < 8 &&
      std::find(report.errors.begin(), report.errors.end(), message) == report.errors.end()) {
    report.errors.push_back(message);
  }
}

template <std::size_t LegCount>
void validation_record_failure(
    ValidationReport& report, const std::size_t point_number,
    const Momenta<LegCount>& momenta, const std::string& message,
    const ldouble hepcat_squared, const ldouble reference_squared,
    const ldouble absolute_error, const ldouble relative_error) {
  // Save only the first failure, including the complete momenta for replay.
  if (report.has_first_failure) {
    return;
  }
  report.has_first_failure = true;
  report.first_failure.point_index = point_number;
  report.first_failure.momenta.assign(momenta.begin(), momenta.end());
  report.first_failure.hepcat_value = hepcat_squared;
  report.first_failure.reference_value = reference_squared;
  report.first_failure.absolute_error = absolute_error;
  report.first_failure.relative_error = relative_error;
  report.first_failure.message = message;
}

} // namespace detail

// ============================================================================
// 2. Random phase-space generation
// ============================================================================

// Construct physical 2 -> N momenta by successive two-body decays of a cluster.
// Each four-vector is (E, px, py, pz), with positive energies on both sides:
//     p_incoming[0] + p_incoming[1] = sum of outgoing momenta.
//
// At each split, cluster -> one outgoing particle + the remaining cluster.
// Intermediate cluster masses use the interior 15%-85% of their allowed range
// to avoid threshold-degenerate decays. Directions cover the full solid angle.
// This is diagnostic sampling, not uniform Lorentz-invariant phase space.
template <std::size_t LegCount>
Momenta<LegCount> make_random_momenta(
    const std::array<ldouble, LegCount>& masses,
    const std::array<std::size_t, 2>& incoming,
    const ldouble center_of_mass_energy, std::mt19937_64& engine) {
  const ldouble threshold = detail::validation_threshold(masses, incoming);
  if (!std::isfinite(center_of_mass_energy) || center_of_mass_energy <= threshold) {
    throw std::invalid_argument("Random phase-space energy must exceed both external thresholds.");
  }

  // Identify outgoing legs without assuming that the incoming legs are 1 and 2.
  std::vector<std::size_t> outgoing;
  for (std::size_t leg = 0; leg < LegCount; ++leg) {
    if (leg != incoming[0] && leg != incoming[1]) {
      outgoing.push_back(leg);
    }
  }

  // Step 1: put the incoming particles along +z and -z in their COM frame.
  Momenta<LegCount> momenta = {};
  const ldouble beam_momentum = detail::two_body_momentum(
    center_of_mass_energy, masses[incoming[0]], masses[incoming[1]]);
  momenta[incoming[0]] = {{
    std::sqrt(beam_momentum * beam_momentum + masses[incoming[0]] * masses[incoming[0]]),
    0, 0, beam_momentum
  }};
  momenta[incoming[1]] = {{
    std::sqrt(beam_momentum * beam_momentum + masses[incoming[1]] * masses[incoming[1]]),
    0, 0, -beam_momentum
  }};

  // Initially the outgoing cluster is the entire COM system, at rest.
  FourMomentum cluster_momentum = {{center_of_mass_energy, 0, 0, 0}};
  ldouble cluster_mass = center_of_mass_energy;
  const ldouble pi = std::acos(ldouble(-1));

  for (std::size_t step = 0; step + 1 < outgoing.size(); ++step) {
    const std::size_t emitted_leg = outgoing[step];

    // Step 2: choose the mass left for the not-yet-emitted particles.
    ldouble remaining_mass_threshold = 0;
    for (std::size_t rest = step + 1; rest < outgoing.size(); ++rest) {
      remaining_mass_threshold += masses[outgoing[rest]];
    }
    // At the final split the remaining cluster is just the last real particle.
    const ldouble remaining_mass = step + 2 == outgoing.size()
      ? masses[outgoing.back()]
      : remaining_mass_threshold
          + (ldouble(0.15L) + ldouble(0.7L) * detail::validation_uniform(engine))
            * (cluster_mass - masses[emitted_leg] - remaining_mass_threshold);

    // Step 3: construct a two-body decay in this cluster's own rest frame.
    // Uniform cos(theta) and phi give a uniform direction on the sphere.
    const ldouble decay_momentum = detail::two_body_momentum(
      cluster_mass, masses[emitted_leg], remaining_mass);
    const ldouble cos_theta = 2 * detail::validation_uniform(engine) - 1;
    const ldouble sin_theta = std::sqrt(std::max(ldouble(0), 1 - cos_theta * cos_theta));
    const ldouble phi = 2 * pi * detail::validation_uniform(engine);
    const FourMomentum emitted_rest = {{
      std::sqrt(decay_momentum * decay_momentum + masses[emitted_leg] * masses[emitted_leg]),
      decay_momentum * sin_theta * std::cos(phi),
      decay_momentum * sin_theta * std::sin(phi),
      decay_momentum * cos_theta
    }};
    const FourMomentum remaining_rest = {{
      std::sqrt(decay_momentum * decay_momentum + remaining_mass * remaining_mass),
      -emitted_rest[1], -emitted_rest[2], -emitted_rest[3]
    }};

    // Step 4: boost both products back to the overall COM frame. Save the
    // emitted particle and continue splitting only the remaining cluster.
    momenta[emitted_leg] = detail::boost_from_rest(
      emitted_rest, cluster_momentum, cluster_mass);
    cluster_momentum = detail::boost_from_rest(
      remaining_rest, cluster_momentum, cluster_mass);
    cluster_mass = remaining_mass;
  }
  momenta[outgoing.back()] = cluster_momentum;

  // Step 5: verify all external mass shells and four-momentum conservation.
  const auto validation = validate_momenta(momenta, masses, incoming);
  if (!validation.valid) {
    throw std::runtime_error("The random point failed its kinematic checks: " + validation.message);
  }
  return momenta;
}

// CalcHEP's matrix-element return type is double. Use one double-valued point
// for both evaluators rather than comparing different rounded kinematics.
template <std::size_t LegCount>
Momenta<LegCount> quantize_validation_momenta(const Momenta<LegCount>& point) {
  Momenta<LegCount> result;
  for (std::size_t leg = 0; leg < LegCount; ++leg) {
    for (std::size_t component = 0; component < 4; ++component) {
      const double common = static_cast<double>(point[leg][component]);
      if (!std::isfinite(common)) {
        throw std::runtime_error(
          "A validation momentum cannot be represented at common double precision.");
      }
      result[leg][component] = static_cast<ldouble>(common);
    }
  }
  return result;
}

template <std::size_t LegCount>
Momenta<LegCount> make_random_validation_momenta(
    const std::array<ldouble, LegCount>& masses,
    const std::array<std::size_t, 2>& incoming,
    std::mt19937_64& engine, const ValidationOptions& options = ValidationOptions()) {
  const auto energies = detail::validation_energy_range(masses, incoming, options);
  // Sample sqrt(s) uniformly in its logarithm, covering different energy scales.
  // Keep this draw even for a fixed energy, so saved seeds retain their sequence.
  const ldouble fraction = detail::validation_uniform(engine);
  const ldouble energy = energies[0] == energies[1] ? energies[0]
    : std::exp(std::log(energies[0])
      + fraction * (std::log(energies[1]) - std::log(energies[0])));
  const auto point = quantize_validation_momenta(
    make_random_momenta(masses, incoming, energy, engine));
  const auto validation = validate_momenta(point, masses, incoming);
  if (!validation.valid) {
    throw std::runtime_error(
      "The common double-precision point failed its kinematic checks: " + validation.message);
  }
  return point;
}

// ============================================================================
// 3. Compare squared amplitudes on identical momenta
// ============================================================================

// Callbacks receive the same immutable physical-momentum array. A callback
// returns |M|^2, already summed/averaged in the convention agreed by the caller.
// This routine does not square again or add any normalization factors.
// Normalization, model matching and external-leg mapping belong to that caller.
template <std::size_t LegCount, class HEPCATEvaluator, class ReferenceEvaluator>
ValidationReport compare_squared_amplitudes(
    const std::array<ldouble, LegCount>& masses,
    const std::array<std::size_t, 2>& incoming,
    HEPCATEvaluator evaluate_hepcat, ReferenceEvaluator evaluate_reference,
    const ValidationOptions& options = ValidationOptions(), std::ostream* progress_output = nullptr) {
  ValidationReport report;
  report.seed = options.seed;
  report.requested_points = options.points;
  const ldouble unavailable_value = std::numeric_limits<ldouble>::quiet_NaN();
  // Reject invalid run settings before generating or evaluating any point.
  try {
    if (options.points == 0 || !std::isfinite(options.relative_tolerance)
        || options.relative_tolerance < 0 || !std::isfinite(options.absolute_tolerance)
        || options.absolute_tolerance < 0) {
      throw std::invalid_argument(
        "Validation needs a positive point count and finite nonnegative tolerances.");
    }
    const auto energies = detail::validation_energy_range(masses, incoming, options);
    report.minimum_center_of_mass_energy = energies[0];
    report.maximum_center_of_mass_energy = energies[1];
  } catch (const std::exception& configuration_error) {
    report.status = "ERROR";
    report.message = configuration_error.what();
    detail::validation_record_error(report, report.message);
    return report;
  }
  std::mt19937_64 engine(options.seed);
  for (std::size_t point_index = 0; point_index < options.points; ++point_index) {
    Momenta<LegCount> point = {};
    ldouble hepcat_squared = unavailable_value;
    ldouble reference_squared = unavailable_value;
    std::string point_error;
    // Step 1: generate one common physical point. Never retry a failed point.
    try {
      point = make_random_validation_momenta(masses, incoming, engine, options);
      ++report.generated_points;
    } catch (const std::exception& failure) {
      point_error = std::string("Random kinematics: ") + failure.what();
    } catch (...) {
      point_error = "Random kinematics raised an unknown exception.";
    }
    if (point_error.empty()) {
      // Step 2: evaluate each implementation at those exact same momenta.
      // Still call the reference if HEPCAT fails, to preserve its diagnostic.
      try {
        hepcat_squared = evaluate_hepcat(point);
        if (!std::isfinite(hepcat_squared) || hepcat_squared < 0) {
          point_error = "HEPCAT returned a nonfinite or negative squared amplitude.";
        }
      } catch (const std::exception& failure) {
        point_error = std::string("HEPCAT evaluator: ") + failure.what();
      } catch (...) {
        point_error = "HEPCAT evaluator raised an unknown exception.";
      }
      try {
        reference_squared = evaluate_reference(point);
        if (!std::isfinite(reference_squared) || reference_squared < 0) {
          if (!point_error.empty()) {
            point_error += " ";
          }
          point_error += "Reference returned a nonfinite or negative squared amplitude.";
        }
      } catch (const std::exception& failure) {
        if (!point_error.empty()) {
          point_error += " ";
        }
        point_error += std::string("Reference evaluator: ") + failure.what();
      } catch (...) {
        if (!point_error.empty()) {
          point_error += " ";
        }
        point_error += "Reference evaluator raised an unknown exception.";
      }
    }
    // Step 3: compare only finite, nonnegative results. A generation or
    // evaluation error is counted once for this point and can never pass.
    if (!point_error.empty()) {
      ++report.evaluation_errors;
      detail::validation_record_error(report, point_error);
      detail::validation_record_failure(
        report, point_index + 1, point, point_error,
        hepcat_squared, reference_squared, unavailable_value, unavailable_value);
    } else {
      ++report.compared_points;
      const ldouble absolute_error = std::abs(hepcat_squared - reference_squared);
      const ldouble comparison_scale = std::max(std::abs(hepcat_squared), std::abs(reference_squared));
      const ldouble relative_error = comparison_scale == 0 ? 0 : absolute_error / comparison_scale;
      report.maximum_absolute_error = std::max(report.maximum_absolute_error, absolute_error);
      report.maximum_relative_error = std::max(report.maximum_relative_error, relative_error);
      // Acceptance: |HEPCAT - reference| <= absolute tolerance
      //              + relative tolerance * max(|HEPCAT|, |reference|).
      if (absolute_error > options.absolute_tolerance + options.relative_tolerance * comparison_scale) {
        ++report.mismatches;
        detail::validation_record_failure(
          report, point_index + 1, point,
          "The squared amplitudes disagree beyond the supplied tolerances.",
          hepcat_squared, reference_squared, absolute_error, relative_error);
      }
    }
    // Step 4: report progress without changing which points are tested.
    if (progress_output && options.progress_every != 0 &&
        ((point_index + 1) % options.progress_every == 0 || point_index + 1 == options.points)) {
      *progress_output << "Amplitude validation: " << point_index + 1 << '/' << options.points
        << " points; " << report.mismatches << " mismatches; "
        << report.evaluation_errors << " errors.\n";
    }
  }
  // Every requested point must have been generated, evaluated and accepted.
  report.passed = report.generated_points == options.points && report.compared_points == options.points
    && report.mismatches == 0 && report.evaluation_errors == 0;
  report.status = report.passed ? "PASS" : (report.evaluation_errors ? "ERROR" : "FAIL");
  report.message = report.passed ? "Every requested common phase-space point agrees."
    : "The requested validation did not pass; inspect counts and the first failing point.";
  return report;
}

// ============================================================================
// 4. Command-line options and printed reports
// ============================================================================

inline bool validation_help_requested(int argc, char** argv) {
  for (int argument = 1; argument < argc; ++argument) {
    if (std::string(argv[argument]) == "--help") {
      return true;
    }
  }
  return false;
}

inline void print_validation_usage(std::ostream& output) {
  output << "--points N --seed S --relative-tolerance R --absolute-tolerance A\n";
}

inline ValidationOptions parse_validation_arguments(
    int argc, char** argv, ValidationOptions options = ValidationOptions()) {
  for (int argument = 1; argument < argc; ++argument) {
    const std::string flag(argv[argument]);
    if (argument + 1 == argc) {
      throw std::invalid_argument("Missing validator option value.");
    }
    const std::string value(argv[++argument]);
    std::size_t consumed = 0;
    if (flag == "--points" || flag == "--seed") {
      if (value.empty() || value[0] == '-') {
        throw std::invalid_argument("Expected an unsigned integer.");
      }
      const auto number = std::stoull(value, &consumed);
      if (consumed != value.size()) {
        throw std::invalid_argument("Invalid integer option.");
      }
      if (flag == "--points") {
        if (number > std::numeric_limits<std::size_t>::max()) {
          throw std::invalid_argument("The point count exceeds this platform's range.");
        }
        options.points = static_cast<std::size_t>(number);
      } else {
        options.seed = number;
      }
    } else if (flag == "--relative-tolerance" || flag == "--absolute-tolerance") {
      const auto number = std::stold(value, &consumed);
      if (consumed != value.size() || !std::isfinite(number) || number < 0) {
        throw std::invalid_argument("Expected a finite nonnegative tolerance.");
      }
      if (flag == "--relative-tolerance") {
        options.relative_tolerance = number;
      } else {
        options.absolute_tolerance = number;
      }
    } else {
      throw std::invalid_argument("Unknown validator option.");
    }
  }
  return options;
}

inline void print_validation_report(const ValidationReport& report, std::ostream& output) {
  std::ostringstream text;
  text << std::setprecision(std::numeric_limits<ldouble>::max_digits10);
  text << "Amplitude validation: " << report.status << '\n'
    << "Seed: " << report.seed << '\n'
    << "Points requested/generated/compared: " << report.requested_points << '/'
    << report.generated_points << '/' << report.compared_points << '\n'
    << "Mismatches/errors: " << report.mismatches << '/' << report.evaluation_errors << '\n'
    << "Maximum relative/absolute error: " << report.maximum_relative_error << '/'
    << report.maximum_absolute_error << '\n';
  if (!report.message.empty()) {
    text << report.message << '\n';
  }
  if (report.has_first_failure) {
    const auto& failure = report.first_failure;
    text << "First failing point: " << failure.point_index << '\n'
      << failure.message << '\n'
      << "HEPCAT squared amplitude: " << failure.hepcat_value << '\n'
      << "Reference squared amplitude: " << failure.reference_value << '\n';
    for (std::size_t leg = 0; leg < failure.momenta.size(); ++leg) {
      text << "  p" << leg + 1 << " =";
      for (const auto component : failure.momenta[leg]) {
        text << ' ' << component;
      }
      text << '\n';
    }
  }
  output << text.str();
}

} // namespace hepcat_spinas
#endif // HEPCAT_SPINAS_VALIDATION_HPP
