/*
HEPCAT - High Energy Physics Constructive Amplitude Tool

Copyright (C) 2026 Neil Christensen

This file is part of HEPCAT.

HEPCAT is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

HEPCAT is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with HEPCAT.  If not, see <https://www.gnu.org/licenses/>.
*/

#pragma once

// Shared machinery for the QED timing program. Formulae come exclusively from
// the supplied WriteSPINASProcess exports and the two independent generators.
#include "madgraph-runtime.hpp"
#include "calchep-runtime.hpp"
#include <functional>
#include <iostream>
#include <memory>
#include <set>

namespace hepcat_benchmark {
using Clock = std::chrono::steady_clock;
using Real = ldouble;

struct Options {
  std::size_t points = 10000;
  std::size_t repetitions = 5;
  std::size_t warmup = 100;
  std::uint64_t seed = hepcat_spinas::detail::validation_seed_from_time();
  Real energy = 100;  // GeV, fixed for every photon multiplicity.
  Real tolerance = 1e-7L;
  std::string output = "photon-timings";
  std::vector<std::size_t> requested_photons;
};

struct Result {
  std::size_t photons = 0;
  std::array<std::vector<double>, 3> seconds; // SPINAS, CalcHEP, MadGraph.
  std::array<Real, 2> maximum_relative_error = {{0, 0}};
  std::string parameters_json;
};

struct Configuration {
  std::string name;
  std::string calchep_model_directory;
  int calchep_model_number = 0;
  std::vector<int> pdgs;
  std::vector<int> allowed_pdgs;
  std::vector<std::string> parameter_names;
  hepcat_spinas::MadGraphConfiguration madgraph;
};

using Runner = std::function<Result(const Options&)>;
struct Entry { std::size_t photons; Runner run; };
void register_processes(std::vector<Entry>& entries);
Options parse_options(int argc, char** argv);
void write_results(const Options& options, const std::vector<Result>& results);

// RAMBO for massless outgoing particles. The exponential null-vector sample,
// common boost, and scale give uniform Lorentz-invariant massless phase space.
// Incoming particles are massive, back-to-back, and are not part of RAMBO.
// There are no cuts, integrations or event weights in the timed calculation.
template <std::size_t Legs>
hepcat_spinas::Momenta<Legs> make_point(Real energy, Real mass, std::mt19937_64& random) {
  static_assert(Legs >= 4, "At least two outgoing photons are required.");
  using namespace hepcat_spinas;
  if (!(energy > 2 * mass) || mass <= 0 || !std::isfinite(energy))
    throw std::invalid_argument("Use a positive electron mass and energy above threshold.");
  const auto uniform = [&]() -> Real {
    // Strictly inside (0,1), preserving the exact same draws across libraries.
    return (Real(random() >> 12) + Real(0.5L)) / Real(4503599627370496.0L);
  };
  Momenta<Legs> point = {};
  std::array<Real, 4> total = {{0, 0, 0, 0}};
  const Real pi = std::acos(Real(-1));
  for (std::size_t leg = 2; leg < Legs; ++leg) {
    const Real cosine = 2 * uniform() - 1;
    const Real sine = std::sqrt((1 - cosine) * (1 + cosine));
    const Real phi = 2 * pi * uniform();
    const Real q = -std::log(uniform()) - std::log(uniform());
    point[leg] = {{q, q * sine * std::cos(phi), q * sine * std::sin(phi), q * cosine}};
    for (std::size_t component = 0; component < 4; ++component)
      total[component] += point[leg][component];
  }
  const Real invariant = std::sqrt(total[0] * total[0] - total[1] * total[1]
                                  - total[2] * total[2] - total[3] * total[3]);
  if (!(invariant > 0)) throw std::runtime_error("Degenerate RAMBO draw.");
  const Real gamma = total[0] / invariant;
  const Real scale = energy / invariant;
  for (std::size_t leg = 2; leg < Legs; ++leg) {
    const auto original = point[leg];
    const Real dot = (total[1] * original[1] + total[2] * original[2]
                      + total[3] * original[3]) / invariant;
    point[leg][0] = scale * (gamma * original[0] - dot);
    for (std::size_t component = 1; component < 4; ++component)
      point[leg][component] = scale * (original[component]
          + total[component] / invariant * (dot / (gamma + 1) - original[0]));
  }
  const Real beam_energy = energy / 2;
  const Real beam_momentum = std::sqrt((beam_energy - mass) * (beam_energy + mass));
  point[0] = {{beam_energy, 0, 0, beam_momentum}};
  point[1] = {{beam_energy, 0, 0, -beam_momentum}};
  // The common input is rounded ONCE to double, including for SPINAS.
  point = quantize_validation_momenta(point);
  std::array<Real, Legs> masses = {};
  masses[0] = masses[1] = mass;
  const auto check = validate_momenta(point, masses, {{0, 1}});
  if (!check.valid) throw std::runtime_error("Invalid RAMBO point: " + check.message);
  return point;
}

inline Real relative_error(Real a, Real b) {
  if (!std::isfinite(a) || !std::isfinite(b) || a < 0 || b < 0)
    throw std::runtime_error("Nonfinite or negative squared amplitude.");
  const Real scale = std::max(std::abs(a), std::abs(b));
  return scale == 0 ? 0 : std::abs(a - b) / scale;
}

inline double median(std::vector<double> values) {
  if (values.empty()) throw std::invalid_argument("No timing measurements.");
  std::sort(values.begin(), values.end());
  const auto middle = values.size() / 2;
  return values.size() % 2 ? values[middle] : (values[middle - 1] + values[middle]) / 2;
}

template <class Process>
Result run_process(const Options& options, Configuration config) {
  using namespace hepcat_spinas;
  constexpr std::size_t legs = Process::leg_count;
  const std::size_t photons = legs - 2;
  Process process;
  if (Process::incoming_legs() != std::array<std::size_t, 2>{{0, 1}})
    throw std::invalid_argument("This benchmark requires incoming electron and positron first.");
  if (process.masses()[0] != process.masses()[1] || process.masses()[0] <= 0)
    throw std::invalid_argument("The electron and positron must have equal positive mass.");
  for (std::size_t leg = 2; leg < legs; ++leg)
    if (process.masses()[leg] != 0) throw std::invalid_argument("Outgoing photons must be massless.");
  if (photons >= std::numeric_limits<std::size_t>::digits)
    throw std::invalid_argument("Too many photon helicities to enumerate.");
  std::set<std::vector<int>> coverage;
  for (const auto& spins : Process::helicity_configurations()) {
    if (spins[0] || spins[1]) throw std::invalid_argument("Massive spins must be summed.");
    std::vector<int> row;
    for (std::size_t leg = 2; leg < legs; ++leg) {
      if (spins[leg] != -2 && spins[leg] != 2)
        throw std::invalid_argument("Invalid photon helicity.");
      row.push_back(spins[leg]);
    }
    if (!coverage.insert(row).second) throw std::invalid_argument("Duplicate helicity configuration.");
  }
  if (coverage.size() != (std::size_t(1) << photons))
    throw std::invalid_argument("Cannot benchmark an incomplete photon-helicity family.");
  Real factorial = 1;
  for (std::size_t i = 2; i <= photons; ++i) factorial *= i;
  const Real normalization = 1 / (4 * factorial);
  if (std::abs(Process::incoming_spin_average() - 4) > 1e-14L
      || std::abs(Process::symmetry_factor() * factorial - 1) > 1e-14L
      || std::abs(config.madgraph.normalization / normalization - 1) > 1e-14L)
    throw std::invalid_argument("The process does not use the common 1/(4 N!) normalization.");

  std::cout << "N=" << photons << ": generating " << options.points
            << " common RAMBO points; preparing independent references..." << std::endl;
  Result result;
  result.photons = photons;
  std::ostringstream parameters_json;
  parameters_json << '{';
  std::vector<CalcHEPParameter> parameters;
  for (std::size_t index = 0; index < config.parameter_names.size(); ++index) {
    const auto value = process.get_parameters()[index];
    parameters.push_back({config.parameter_names[index], checked_calchep_parameter(value, config.parameter_names[index])});
    if (index) parameters_json << ',';
    parameters_json << madgraph_detail::json_string(config.parameter_names[index]) << ':';
    madgraph_detail::json_number(parameters_json, std::real(value));
  }
  parameters_json << '}';
  result.parameters_json = parameters_json.str();

  // Generate and save all kinematics BEFORE either generation or timing.
  std::mt19937_64 random(options.seed + photons); // Recorded, distinct multiplicity streams.
  std::vector<typename Process::Momenta> points;
  std::vector<std::vector<double>> mg_points;
  points.reserve(options.points);
  mg_points.reserve(options.points);
  std::ofstream saved(options.output + "-points-N" + std::to_string(photons) + ".csv");
  if (!saved) throw std::runtime_error("Cannot write the common phase-space points.");
  saved << "point,leg,E,px,py,pz\n" << std::setprecision(17);
  for (std::size_t index = 0; index < options.points; ++index) {
    points.push_back(make_point<legs>(options.energy, process.masses()[0], random));
    std::vector<double> flat;
    for (const auto leg : config.madgraph.leg_order)
      for (const auto component : points.back()[leg]) flat.push_back(static_cast<double>(component));
    mg_points.push_back(std::move(flat));
    for (std::size_t leg = 0; leg < legs; ++leg) {
      saved << index + 1 << ',' << leg + 1;
      for (const auto component : points.back()[leg]) saved << ',' << static_cast<double>(component);
      saved << '\n';
    }
  }
  saved.close();
  if (!saved) throw std::runtime_error("Writing phase-space points failed.");

  CalcHEPProcess calchep(config.calchep_model_directory, config.calchep_model_number,
      config.pdgs, parameters, 1.0, false, config.allowed_pdgs, 3);
  check_calchep_masses(process.masses(), calchep.masses());
  config.madgraph.optimization_level = 3;
  madgraph_detail::validate_configuration(process, config.madgraph);
  madgraph_detail::Workspace workspace;
  const auto configuration = workspace.path() + "/configuration.json";
  const auto output = workspace.path() + "/reference";
  const auto log = workspace.path() + "/preparation.log";
  madgraph_detail::write_configuration(configuration, process, config.madgraph);
  const auto status = madgraph_detail::run_helper(
      {config.madgraph.python_executable, config.madgraph.helper_path,
       "--config", configuration, "--output", output}, log,
      config.madgraph.preparation_timeout_seconds + 10);
  if (status) throw std::runtime_error("MadGraph preparation failed: " + madgraph_detail::read_tail(log));
#ifdef __APPLE__
  const auto library = output + "/libhepcat_madgraph.dylib";
#else
  const auto library = output + "/libhepcat_madgraph.so";
#endif
  madgraph_detail::Reference madgraph(library);
  if (!madgraph.has_sum()) throw std::runtime_error("MadGraph reference lacks the exhaustive spin-sum interface.");
  // Preserve generator/model/compiler provenance before its temporary files go away.
  std::ofstream build_report(options.output + "-madgraph-N" + std::to_string(photons) + ".json");
  build_report << madgraph_detail::read_text(output + "/result.json");
  if (!build_report) throw std::runtime_error("Cannot save MadGraph build provenance.");

  const auto evaluate = [&](std::size_t backend, std::size_t index) -> Real {
    if (backend == 0) {
      process.set_momenta(points[index]);
      return process.amp2();
    }
    if (backend == 1) return calchep.squared_amplitude(points[index]);
    return normalization * madgraph.evaluate_sum(mg_points[index]);
  };
  std::array<std::vector<Real>, 3> values;
  for (auto& backend : values) backend.resize(options.points);
  std::cout << "N=" << photons << ": checking all points, including the full helicity sums..." << std::endl;
  std::ofstream checks(options.output + "-values-N" + std::to_string(photons) + ".csv");
  if (!checks) throw std::runtime_error("Cannot write the squared-amplitude comparisons.");
  checks << "point,spinas,spinas_exhaustive,calchep,madgraph,calchep_relative_error,madgraph_relative_error\n"
         << std::setprecision(std::numeric_limits<Real>::max_digits10);
  std::size_t failures = 0;
  for (std::size_t index = 0; index < options.points; ++index) {
    for (std::size_t backend = 0; backend < 3; ++backend) values[backend][index] = evaluate(backend, index);
    // Check the optimized sum against every public complex helicity amplitude.
    // This independent enumeration is validation only, outside all timers.
    Real exhaustive = 0;
    for (auto spins : Process::helicity_configurations())
      for (int first = -1; first <= 1; first += 2)
        for (int second = -1; second <= 1; second += 2) {
          spins[0] = first;
          spins[1] = second;
          exhaustive += std::norm(process.amp(spins));
        }
    exhaustive *= normalization;
    if (relative_error(values[0][index], exhaustive) > options.tolerance)
      throw std::runtime_error("Helicity-reused result disagrees with exhaustive public amplitudes.");
    const Real ch_error = relative_error(values[0][index], values[1][index]);
    const Real mg_error = relative_error(values[0][index], values[2][index]);
    result.maximum_relative_error[0] = std::max(result.maximum_relative_error[0], ch_error);
    result.maximum_relative_error[1] = std::max(result.maximum_relative_error[1], mg_error);
    if (ch_error > options.tolerance || mg_error > options.tolerance) ++failures;
    checks << index + 1 << ',' << values[0][index] << ',' << exhaustive << ',' << values[1][index] << ','
           << values[2][index] << ',' << ch_error << ',' << mg_error << '\n';
  }
  checks.close();
  if (!checks) throw std::runtime_error("Writing validation values failed.");
  if (failures) throw std::runtime_error(std::to_string(failures) +
      " points disagree. No performance row accepted. See the saved values CSV.");

  const auto expected = values[0];
  // Warm caches first. Every measured pass evaluates the same complete point set.
  for (std::size_t backend = 0; backend < 3; ++backend)
    for (std::size_t index = 0; index < options.warmup; ++index)
      values[backend][index % options.points] = evaluate(backend, index % options.points);
  const char* names[] = {"SPINAS", "CalcHEP", "MadGraph"};
  for (std::size_t repetition = 0; repetition < options.repetitions; ++repetition) {
    for (std::size_t slot = 0; slot < 3; ++slot) {
      const auto backend = (slot + repetition + photons) % 3;
      const auto start = Clock::now();
      for (std::size_t index = 0; index < options.points; ++index)
        values[backend][index] = evaluate(backend, index);
      const double seconds = std::chrono::duration<double>(Clock::now() - start).count();
      result.seconds[backend].push_back(seconds);
      // Consume and validate every stored value outside the timed region.
      Real checksum = 0;
      for (std::size_t index = 0; index < options.points; ++index) {
        if (relative_error(values[backend][index], expected[index]) > options.tolerance)
          throw std::runtime_error("A timed result differs from the validated spin sum.");
        checksum += values[backend][index];
      }
      if (!std::isfinite(checksum)) throw std::runtime_error("Nonfinite timing checksum.");
      std::cout << "  " << names[backend] << ", pass " << repetition + 1 << ": "
                << seconds << " s; checksum " << checksum << std::endl;
    }
  }
  return result;
}
} // namespace hepcat_benchmark
