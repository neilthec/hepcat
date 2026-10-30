#ifndef HEPCAT_SPINAS_VALIDATION_HPP
#define HEPCAT_SPINAS_VALIDATION_HPP

// Shared random kinematics and numerical comparisons for generated processes.
// This is diagnostic sampling, not a uniformly weighted phase-space integral.
// Neither evaluator may change the common point or discard a disagreement.

#include "spinas-runtime.hpp"
#include <cstdint>
#include <iomanip>
#include <ostream>
#include <random>
#include <sstream>

namespace hepcat_spinas {

struct ValidationOptions {
  std::size_t points = 1000;
  std::uint64_t seed = UINT64_C(12345);
  ldouble relative_tolerance = 1e-7L;
  // A fixed absolute tolerance can hide a wrong zero for a small amplitude.
  // Users may supply one explicitly when their numerical problem requires it.
  ldouble absolute_tolerance = 0;
  ldouble minimum_center_of_mass_energy = 0;
  ldouble maximum_center_of_mass_energy = 0;
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

inline bool validation_help_requested(int argc, char** argv) {
  for (int argument = 1; argument < argc; ++argument) {
    if (std::string(argv[argument]) == "--help") return true;
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
      if (flag == "--relative-tolerance") options.relative_tolerance = number;
      else options.absolute_tolerance = number;
    } else {
      throw std::invalid_argument("Unknown validator option.");
    }
  }
  return options;
}

namespace detail {

// The random engine and its conversion are explicit. In particular, this
// avoids implementation-dependent uniform_real_distribution draw counts.
inline ldouble validation_uniform(std::mt19937_64& engine) {
  return static_cast<ldouble>(engine() >> 11) / ldouble(9007199254740992.0L);
}

template <std::size_t N>
ldouble validation_threshold(const std::array<ldouble, N>& masses,
                            const std::array<std::size_t, 2>& incoming) {
  check_external_masses(masses, incoming);
  ldouble outgoing = 0;
  for (std::size_t leg = 0; leg < N; ++leg) {
    if (leg != incoming[0] && leg != incoming[1]) {
      outgoing += masses[leg];
    }
  }
  return std::max(outgoing, masses[incoming[0]] + masses[incoming[1]]);
}

template <std::size_t N>
std::array<ldouble, 2> validation_energy_range(
    const std::array<ldouble, N>& masses,
    const std::array<std::size_t, 2>& incoming,
    const ValidationOptions& options) {
  const ldouble threshold = validation_threshold(masses, incoming);
  const ldouble ordinary_energy = example_center_of_mass_energy(masses, incoming);
  const ldouble low = options.minimum_center_of_mass_energy == 0
      ? ordinary_energy / 2 : options.minimum_center_of_mass_energy;
  const ldouble high = options.maximum_center_of_mass_energy == 0
      ? ordinary_energy * 2 : options.maximum_center_of_mass_energy;
  if (!std::isfinite(low) || !std::isfinite(high) || low <= threshold || high < low) {
    throw std::invalid_argument(
      "Validation energies must be finite, ordered and above both external thresholds.");
  }
  return {{low, high}};
}

inline void validation_record_error(ValidationReport& report, const std::string& message) {
  if (report.errors.size() < 8 &&
      std::find(report.errors.begin(), report.errors.end(), message) == report.errors.end()) {
    report.errors.push_back(message);
  }
}

template <std::size_t N>
void validation_record_failure(ValidationReport& report, const std::size_t point,
                               const Momenta<N>& momenta, const std::string& message,
                               const ldouble first, const ldouble second,
                               const ldouble absolute, const ldouble relative) {
  if (report.has_first_failure) {
    return;
  }
  report.has_first_failure = true;
  report.first_failure.point_index = point;
  report.first_failure.momenta.assign(momenta.begin(), momenta.end());
  report.first_failure.hepcat_value = first;
  report.first_failure.reference_value = second;
  report.first_failure.absolute_error = absolute;
  report.first_failure.relative_error = relative;
  report.first_failure.message = message;
}

} // namespace detail

// Sequential random two-body decays generate valid massive or massless 2->N
// configurations. Intermediate masses use the interior 15%-85% of their legal
// interval to avoid threshold-degenerate decays; all solid angles are sampled.
// The resulting points are deliberately not asserted to have uniform LIPS weight.
template <std::size_t N>
Momenta<N> make_random_momenta(const std::array<ldouble, N>& masses,
                              const std::array<std::size_t, 2>& incoming,
                              const ldouble sqrt_s, std::mt19937_64& engine) {
  const ldouble threshold = detail::validation_threshold(masses, incoming);
  if (!std::isfinite(sqrt_s) || sqrt_s <= threshold) {
    throw std::invalid_argument("Random phase-space energy must exceed both external thresholds.");
  }
  std::vector<std::size_t> outgoing;
  for (std::size_t leg = 0; leg < N; ++leg) {
    if (leg != incoming[0] && leg != incoming[1]) {
      outgoing.push_back(leg);
    }
  }
  Momenta<N> result = {};
  const ldouble beam = detail::two_body_momentum(
    sqrt_s, masses[incoming[0]], masses[incoming[1]]);
  result[incoming[0]] = {{std::sqrt(beam * beam + masses[incoming[0]] * masses[incoming[0]]),
                         0, 0, beam}};
  result[incoming[1]] = {{std::sqrt(beam * beam + masses[incoming[1]] * masses[incoming[1]]),
                         0, 0, -beam}};
  FourMomentum frame = {{sqrt_s, 0, 0, 0}};
  ldouble frame_mass = sqrt_s;
  const ldouble pi = std::acos(ldouble(-1));
  for (std::size_t step = 0; step + 1 < outgoing.size(); ++step) {
    const std::size_t leg = outgoing[step];
    ldouble remaining_threshold = 0;
    for (std::size_t rest = step + 1; rest < outgoing.size(); ++rest) {
      remaining_threshold += masses[outgoing[rest]];
    }
    const ldouble remaining_mass = step + 2 == outgoing.size()
      ? masses[outgoing.back()]
      : remaining_threshold + (ldouble(0.15L) + ldouble(0.7L) * detail::validation_uniform(engine))
          * (frame_mass - masses[leg] - remaining_threshold);
    const ldouble magnitude = detail::two_body_momentum(frame_mass, masses[leg], remaining_mass);
    const ldouble cosine = 2 * detail::validation_uniform(engine) - 1;
    const ldouble sine = std::sqrt(std::max(ldouble(0), 1 - cosine * cosine));
    const ldouble azimuth = 2 * pi * detail::validation_uniform(engine);
    const FourMomentum rest_first = {{std::sqrt(magnitude * magnitude + masses[leg] * masses[leg]),
      magnitude * sine * std::cos(azimuth), magnitude * sine * std::sin(azimuth), magnitude * cosine}};
    const FourMomentum rest_remaining = {{std::sqrt(magnitude * magnitude + remaining_mass * remaining_mass),
      -rest_first[1], -rest_first[2], -rest_first[3]}};
    result[leg] = detail::boost_from_rest(rest_first, frame, frame_mass);
    frame = detail::boost_from_rest(rest_remaining, frame, frame_mass);
    frame_mass = remaining_mass;
  }
  result[outgoing.back()] = frame;
  const auto validation = validate_momenta(result, masses, incoming);
  if (!validation.valid) {
    throw std::runtime_error("The random point failed its kinematic checks: " + validation.message);
  }
  return result;
}

// CalcHEP's matrix-element return type is double. Use one double-valued point
// for both evaluators rather than comparing different rounded kinematics.
template <std::size_t N>
Momenta<N> quantize_validation_momenta(const Momenta<N>& point) {
  Momenta<N> result;
  for (std::size_t leg = 0; leg < N; ++leg) {
    for (std::size_t component = 0; component < 4; ++component) {
      const double common = static_cast<double>(point[leg][component]);
      if (!std::isfinite(common)) {
        throw std::runtime_error("A validation momentum cannot be represented at common double precision.");
      }
      result[leg][component] = static_cast<ldouble>(common);
    }
  }
  return result;
}

template <std::size_t N>
Momenta<N> make_random_validation_momenta(
    const std::array<ldouble, N>& masses,
    const std::array<std::size_t, 2>& incoming,
    std::mt19937_64& engine, const ValidationOptions& options = ValidationOptions()) {
  const auto energies = detail::validation_energy_range(masses, incoming, options);
  const ldouble fraction = detail::validation_uniform(engine);
  const ldouble energy = energies[0] == energies[1] ? energies[0]
    : std::exp(std::log(energies[0])
      + fraction * (std::log(energies[1]) - std::log(energies[0])));
  const auto point = quantize_validation_momenta(make_random_momenta(masses, incoming, energy, engine));
  const auto validation = validate_momenta(point, masses, incoming);
  if (!validation.valid) {
    throw std::runtime_error("The common double-precision point failed its kinematic checks: " + validation.message);
  }
  return point;
}

// Callbacks receive the same immutable physical-momentum array. A callback
// returns its squared amplitude in the convention agreed by the caller.
// Normalization, model matching and external-leg mapping belong to that caller.
template <std::size_t N, class HEPCATEvaluator, class ReferenceEvaluator>
ValidationReport compare_squared_amplitudes(
    const std::array<ldouble, N>& masses,
    const std::array<std::size_t, 2>& incoming,
    HEPCATEvaluator hepcat, ReferenceEvaluator reference,
    const ValidationOptions& options = ValidationOptions(), std::ostream* progress = nullptr) {
  ValidationReport report;
  report.seed = options.seed;
  report.requested_points = options.points;
  const ldouble missing = std::numeric_limits<ldouble>::quiet_NaN();
  try {
    if (options.points == 0 || !std::isfinite(options.relative_tolerance)
        || options.relative_tolerance < 0 || !std::isfinite(options.absolute_tolerance)
        || options.absolute_tolerance < 0) {
      throw std::invalid_argument("Validation needs a positive point count and finite nonnegative tolerances.");
    }
    const auto energies = detail::validation_energy_range(masses, incoming, options);
    report.minimum_center_of_mass_energy = energies[0];
    report.maximum_center_of_mass_energy = energies[1];
  } catch (const std::exception& error) {
    report.status = "ERROR";
    report.message = error.what();
    detail::validation_record_error(report, report.message);
    return report;
  }
  std::mt19937_64 engine(options.seed);
  for (std::size_t index = 0; index < options.points; ++index) {
    Momenta<N> point = {};
    ldouble first = missing;
    ldouble second = missing;
    std::string error;
    try {
      point = make_random_validation_momenta(masses, incoming, engine, options);
      ++report.generated_points;
    } catch (const std::exception& failure) {
      error = std::string("Random kinematics: ") + failure.what();
    } catch (...) {
      error = "Random kinematics raised an unknown exception.";
    }
    if (error.empty()) {
      try {
        first = hepcat(point);
        if (!std::isfinite(first) || first < 0) {
          error = "HEPCAT returned a nonfinite or negative squared amplitude.";
        }
      } catch (const std::exception& failure) {
        error = std::string("HEPCAT evaluator: ") + failure.what();
      } catch (...) {
        error = "HEPCAT evaluator raised an unknown exception.";
      }
      try {
        second = reference(point);
        if (!std::isfinite(second) || second < 0) {
          if (!error.empty()) error += " ";
          error += "Reference returned a nonfinite or negative squared amplitude.";
        }
      } catch (const std::exception& failure) {
        if (!error.empty()) error += " ";
        error += std::string("Reference evaluator: ") + failure.what();
      } catch (...) {
        if (!error.empty()) error += " ";
        error += "Reference evaluator raised an unknown exception.";
      }
    }
    if (!error.empty()) {
      ++report.evaluation_errors;
      detail::validation_record_error(report, error);
      detail::validation_record_failure(report, index + 1, point, error, first, second, missing, missing);
    } else {
      ++report.compared_points;
      const ldouble absolute = std::abs(first - second);
      const ldouble scale = std::max(std::abs(first), std::abs(second));
      const ldouble relative = scale == 0 ? 0 : absolute / scale;
      report.maximum_absolute_error = std::max(report.maximum_absolute_error, absolute);
      report.maximum_relative_error = std::max(report.maximum_relative_error, relative);
      if (absolute > options.absolute_tolerance + options.relative_tolerance * scale) {
        ++report.mismatches;
        detail::validation_record_failure(report, index + 1, point,
          "The squared amplitudes disagree beyond the supplied tolerances.", first, second, absolute, relative);
      }
    }
    if (progress && options.progress_every != 0 &&
        ((index + 1) % options.progress_every == 0 || index + 1 == options.points)) {
      *progress << "Amplitude validation: " << index + 1 << '/' << options.points
        << " points; " << report.mismatches << " mismatches; "
        << report.evaluation_errors << " errors.\n";
    }
  }
  report.passed = report.generated_points == options.points && report.compared_points == options.points
    && report.mismatches == 0 && report.evaluation_errors == 0;
  report.status = report.passed ? "PASS" : (report.evaluation_errors ? "ERROR" : "FAIL");
  report.message = report.passed ? "Every requested common phase-space point agrees."
    : "The requested validation did not pass; inspect counts and the first failing point.";
  return report;
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
      for (const auto value : failure.momenta[leg]) text << ' ' << value;
      text << '\n';
    }
  }
  output << text.str();
}

} // namespace hepcat_spinas
#endif
