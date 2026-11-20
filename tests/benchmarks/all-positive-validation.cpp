// Direct numerical tests of the two proposed all-N formulas. No constructive
// diagrams, preparation, factorization completion or symbolic expansion runs.
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

#include "all-positive-formulas.hpp"
#include "madgraph-runtime.hpp"
#include "../checks/spinas-export-reference.hpp"
#include <iomanip>
#include <iostream>
#include <sstream>

namespace {
struct Options {
  unsigned photons = 7;
  std::size_t points = 100000;
  std::uint64_t seed = hepcat_spinas::detail::validation_seed_from_time();
  ldouble mass = 1, charge = ldouble(1) / 3;
  ldouble minimum_energy = 10, maximum_energy = 100, tolerance = 1e-7L;
  std::string reference, output = "all-positive", archived_rates, check = "both";
  bool self_test_only = false, outliers_only = false;
  std::size_t control_points = 100000;
  hepcat_all_positive::PoleEvaluation poles = hepcat_all_positive::PoleEvaluation::ExternalShellExpansion;
};

std::uint64_t unsigned_value(const std::string& value) {
  if (value.empty() || value.find_first_not_of("0123456789") != std::string::npos)
    throw std::invalid_argument("Expected an unsigned integer: " + value);
  return std::stoull(value);
}

ldouble real_value(const std::string& value) {
  std::size_t used = 0;
  const ldouble result = std::stold(value, &used);
  if (used != value.size() || !std::isfinite(result))
    throw std::invalid_argument("Expected a finite real number: " + value);
  return result;
}

ldouble relative(const cdouble& a, const cdouble& b) {
  if (!hepcat_spinas::finite_complex(a) || !hepcat_spinas::finite_complex(b))
    throw std::runtime_error("A formula returned a nonfinite complex amplitude.");
  const ldouble scale = std::max(std::abs(a), std::abs(b));
  return scale ? std::abs(a - b) / scale : 0;
}

ldouble relative(ldouble a, ldouble b) {
  if (!std::isfinite(a) || !std::isfinite(b))
    throw std::runtime_error("A comparison contains a nonfinite squared amplitude.");
  const ldouble scale = std::max(std::abs(a), std::abs(b));
  return scale ? std::abs(a - b) / scale : 0;
}

template<std::size_t N>
hepcat_spinas::Momenta<N + 2> point(ldouble mass, std::mt19937_64& random,
                                   ldouble minimum, ldouble maximum) {
  std::array<ldouble, N + 2> masses = {};
  masses[0] = masses[1] = mass;
  hepcat_spinas::ValidationOptions options;
  options.minimum_center_of_mass_energy = minimum;
  options.maximum_center_of_mass_energy = maximum;
  return hepcat_spinas::make_random_validation_momenta(
      masses, {{0, 1}}, random, options);
}

// Established small-N formulas are controls for this numerical transcription,
// not answers supplied at either new multiplicity.
template<std::size_t N> cdouble small_oracle(
    const hepcat_spinas::Momenta<N + 2>& p, ldouble m, ldouble e, int s1, int s2);

template<> cdouble small_oracle<2>(const hepcat_spinas::Momenta<4>& p,
    ldouble m, ldouble e, int s1, int s2) {
  std::array<ldouble, 4> masses = {{m, m, 0, 0}};
  auto particles = hepcat_export_reference::particles(p, masses);
  spinas::sproduct a12(ANGLE, &particles[0], &particles[1]);
  spinas::sproduct s34(SQUARE, &particles[2], &particles[3]);
  return 2 * e * e * m * a12.v(s1, s2) * s34.v() * s34.v() /
    hepcat_export_reference::electron_denominator(p, 0, 2, m) /
    hepcat_export_reference::electron_denominator(p, 0, 3, m);
}

template<> cdouble small_oracle<3>(const hepcat_spinas::Momenta<5>& p,
    ldouble m, ldouble e, int s1, int s2) {
  return hepcat_export_reference::five_all_plus(p, {{m, m, 0, 0, 0}}, e, s1, s2);
}

template<> cdouble small_oracle<4>(const hepcat_spinas::Momenta<6>& p,
    ldouble m, ldouble e, int s1, int s2) {
  return hepcat_export_reference::six_all_plus(p, {{m, m, 0, 0, 0, 0}}, e, s1, s2);
}

template<std::size_t N> void small_check(std::mt19937_64& random, std::size_t points) {
  ldouble worst = 0;
  // Alternate two independent mass and charge settings.
  for (std::size_t sample = 0; sample / 2 < points; ++sample) {
    const ldouble mass = sample % 2 ? ldouble(1.5L) : ldouble(1);
    const ldouble charge = sample % 2 ? ldouble(2) / 7 : ldouble(1) / 3;
    const auto p = point<N>(mass, random, 10, 100);
    hepcat_all_positive::Formulas<N> formulas(p, mass, charge);
    worst = std::max(worst, relative(formulas.spurious_coefficient(),
                                     formulas.spurious_ordered_coefficient()));
    for (int a : {-1, 1}) for (int b : {-1, 1}) {
      const auto result = formulas.amplitudes(a, b);
      const auto expected = small_oracle<N>(p, mass, charge, a, b);
      worst = std::max(worst, relative(result.physical, expected));
      worst = std::max(worst, relative(formulas.collected_physical_amplitude(a, b), expected));
      worst = std::max(worst, relative(result.spurious, expected));
    }
  }
  if (!std::isfinite(worst) || worst > 1e-8L)
    throw std::runtime_error("Small-N formula control failed at N=" + std::to_string(N) +
                             ", relative error=" + std::to_string(worst));
  std::cout << "N=" << N << " small-formula controls PASS; maximum relative error "
            << worst << " (" << points << " points per parameter setting)\n";
}

// The portable audit can stream archived independent rates on stdin. Recalculate
// a deterministic subset with the supplied MadGraph library to verify the seed,
// spin convention and normalization. An ordinary run calls the library at every point.
class ReferenceRates {
  bool archived_, averaged_;
  std::size_t points_;
public:
  std::size_t live_points = 0;
  ldouble maximum_archive_difference = 0;

  explicit ReferenceRates(const Options& o) : archived_(!o.archived_rates.empty()),
      averaged_(o.archived_rates == "averaged"), points_(o.points) {
    if (archived_) {
      std::string header;
      std::getline(std::cin, header);
      const auto prefix = averaged_ ? "point,hepcat,madgraph_squared," : "point,physical,spurious,madgraph,";
      if (header.rfind(prefix, 0) != 0) throw std::runtime_error("Unexpected MadGraph rate CSV header.");
    }
  }
  bool wants_live(std::size_t i) const { return !archived_ || i == 1 || i == points_ || i % 10000 == 0; }
  ldouble read(std::size_t i, ldouble live) {
    if (wants_live(i)) ++live_points;
    if (!archived_) return live;
    std::string line, field;
    if (!std::getline(std::cin, line)) throw std::runtime_error("Missing archived MadGraph result.");
    std::istringstream row(line);
    std::getline(row, field, ',');
    if (unsigned_value(field) != i) throw std::runtime_error("Unexpected archived MadGraph event index.");
    for (unsigned c = 1; c <= (averaged_ ? 2 : 3); ++c)
      if (!std::getline(row, field, ',')) throw std::runtime_error("Missing archived MadGraph rate.");
    ldouble rate = real_value(field) * (averaged_ ? 4 : 1);
    if (rate < 0) throw std::runtime_error("Negative squared amplitude in archive.");
    if (wants_live(i)) compare_live(i, rate, live);
    return rate;
  }
  void compare_live(std::size_t i, ldouble saved, ldouble live) {
    const auto error = relative(saved, live);
    maximum_archive_difference = std::max(maximum_archive_difference, error);
    if (error > 2e-12L) throw std::runtime_error("Saved and freshly calculated MadGraph rates differ at event " + std::to_string(i));
  }
  void finish() const {
    std::string extra;
    if (archived_ && std::getline(std::cin, extra)) throw std::runtime_error("Extra archived rate rows.");
  }
};

template<std::size_t N> int validate(const Options& options) {
  using namespace hepcat_spinas;
  madgraph_detail::Reference reference(options.reference);
  ReferenceRates rates(options);
  std::mt19937_64 random(options.seed);
  std::ofstream values(options.output + "-values.csv");
  std::ofstream points(options.output + "-points.csv");
  if (!values || !points) throw std::runtime_error("Cannot write validation outputs.");
  values << "point,physical,spurious,madgraph,physical_mg_relative_error,"
            "spurious_mg_relative_error,physical_spurious_complex_error,"
            "ordered_compact_current_error,collected_physical,collected_mg_relative_error,collected_original_complex_error\n" << std::setprecision(std::numeric_limits<ldouble>::max_digits10);
  points << "point,leg,E,px,py,pz\n" << std::setprecision(std::numeric_limits<ldouble>::max_digits10);
  std::size_t failures = 0, stored_rows = 0;
  ldouble worst_collected_mg = 0, worst_collected_signed = 0;
  ldouble worst_physical = 0, worst_spurious = 0, worst_complex = 0, worst_ordered = 0;
  std::vector<int> helicities(N + 2, 1);
  for (std::size_t sample = 0; sample < options.points; ++sample) {
    const bool live = rates.wants_live(sample + 1);
    const auto p = point<N>(options.mass, random, options.minimum_energy, options.maximum_energy);
    hepcat_all_positive::Formulas<N> formulas(p, options.mass, options.charge, options.poles);
    std::vector<double> reference_momenta;
    for (const auto& momentum : p)
      for (auto component : momentum)
        reference_momenta.push_back(static_cast<double>(component));
    auto madgraph_sum = [&]() {
      ldouble sum = 0;
      for (int a : {-1, 1}) for (int b : {-1, 1}) {
        helicities[0] = a; helicities[1] = b;
        const double term = reference.evaluate(reference_momenta, helicities);
        if (!std::isfinite(term) || term < 0) throw std::runtime_error("Invalid MadGraph squared amplitude.");
        sum += term;
      }
      return sum;
    };
    ldouble physical = 0, spurious = 0, complex_error = 0;
    ldouble collected = 0, collected_signed = 0;
    // Raw spin sums on both sides, with all outgoing photons fixed at +.
    // No initial averaging or identical-particle phase-space factor is applied.
    for (int a : {-1, 1}) for (int b : {-1, 1}) {
      const auto amplitudes = formulas.amplitudes(a, b);
      const cdouble reduced = formulas.collected_physical_amplitude(a, b);
      collected += std::norm(reduced);
      collected_signed = std::max(collected_signed, relative(amplitudes.physical, reduced));
      physical += std::norm(amplitudes.physical);
      spurious += std::norm(amplitudes.spurious);
      complex_error = std::max(complex_error,
                               relative(amplitudes.physical, amplitudes.spurious));
    }
    const ldouble expected = rates.read(sample + 1, live ? madgraph_sum() : 0);
    const ldouble ordered_error = relative(formulas.spurious_coefficient(),
                                            formulas.spurious_ordered_coefficient());
    const ldouble collected_mg = relative(collected, expected);
    worst_collected_mg = std::max(worst_collected_mg, collected_mg);
    worst_collected_signed = std::max(worst_collected_signed, collected_signed);
    const ldouble physical_error = relative(physical, expected);
    const ldouble spurious_error = relative(spurious, expected);
    worst_physical = std::max(worst_physical, physical_error);
    worst_spurious = std::max(worst_spurious, spurious_error);
    worst_complex = std::max(worst_complex, complex_error);
    worst_ordered = std::max(worst_ordered, ordered_error);
    const bool finite = std::isfinite(physical) && std::isfinite(spurious) &&
      std::isfinite(physical_error) && std::isfinite(spurious_error) &&
      std::isfinite(complex_error) && std::isfinite(ordered_error);
    const bool physical_failed = std::max(physical_error, collected_mg) > options.tolerance;
    const bool spurious_failed = spurious_error > options.tolerance;
    const bool identity_failed = std::max({complex_error, ordered_error, collected_signed}) > options.tolerance;
    const bool failed = !finite || (options.check != "spurious" && physical_failed) ||
        (options.check != "physical" && spurious_failed) || (options.check == "both" && identity_failed);
    if (failed) {
      ++failures;
      if (!live) {
        rates.compare_live(sample + 1, expected, madgraph_sum());
        ++rates.live_points;
      }
    }
    if (!options.outliers_only || failed) {
      ++stored_rows;
      for (std::size_t leg = 0; leg < p.size(); ++leg) {
        points << sample + 1 << ',' << leg + 1;
        for (auto component : p[leg]) points << ',' << component;
        points << '\n';
      }
      values << sample + 1 << ',' << physical << ',' << spurious << ',' << expected << ','
           << physical_error << ',' << spurious_error << ',' << complex_error << ','
           << ordered_error << ',' << collected << ',' << collected_mg << ',' << collected_signed << '\n';
    }
    if ((sample + 1) % 100 == 0 || sample + 1 == options.points)
      std::cout << "N=" << N << ": " << sample + 1 << '/' << options.points
                << " points; failures " << failures << std::endl;
  }
  rates.finish();
  std::ofstream report(options.output + ".json");
  if (!report) throw std::runtime_error("Cannot write validation report.");
  report << std::setprecision(20)
    << "{\n\"Status\":\"" << (failures ? "FAIL" : "PASS") << "\",\n"
    << "\"Photons\":" << N << ",\n\"ExternalParticles\":" << N + 2
    << ",\n\"Points\":" << options.points << ",\n\"Failures\":" << failures
    << ",\n\"Seed\":" << options.seed << ",\n\"Mass\":" << options.mass
    << ",\n\"Charge\":" << options.charge
    << ",\n\"MinimumCenterOfMassEnergy\":" << options.minimum_energy
    << ",\n\"MaximumCenterOfMassEnergy\":" << options.maximum_energy
    << ",\n\"RelativeTolerance\":" << options.tolerance
    << ",\n\"LiveMadGraphPoints\":" << rates.live_points
    << ",\n\"MaximumArchivedLiveMadGraphDifference\":" << rates.maximum_archive_difference
    << ",\n\"MadGraphRateSource\":\"" << (options.archived_rates.empty() ? "live" : "archive") << "\""
    << ",\n\"Checked\":\"" << options.check << "\""
    << ",\n\"StoredRows\":" << stored_rows
    << ",\n\"OutputMode\":\"" << (options.outliers_only ? "outliers" : "all") << "\""
    << ",\n\"PoleEvaluation\":\"" << (options.poles == hepcat_all_positive::PoleEvaluation::SummedMomentum ? "summed" : "expanded") << "\""
    << ",\n\"FormulaPrecisionBits\":" << std::numeric_limits<ldouble>::digits
    << ",\n\"MassiveSpinConfigurations\":4"
    << ",\n\"MaximumCollectedPhysicalMadGraphRelativeError\":" << worst_collected_mg
    << ",\n\"MaximumCollectedPhysicalOriginalComplexError\":" << worst_collected_signed
    << ",\n\"MaximumPhysicalMadGraphRelativeError\":" << worst_physical
    << ",\n\"MaximumSpuriousMadGraphRelativeError\":" << worst_spurious
    << ",\n\"MaximumPhysicalSpuriousComplexError\":" << worst_complex
    << ",\n\"MaximumOrderedCompactCurrentError\":" << worst_ordered
    << ",\n\"ReferenceLibrary\":" << madgraph_detail::json_string(options.reference)
    << ",\n\"Normalization\":\"Raw sum over four massive-fermion spins; photons fixed positive; no averages or symmetry factor.\",\n"
    << "\"ConstructiveGenerationUsed\":false,\n\"AbsoluteTolerance\":0\n}\n";
  std::cout << (failures ? "FAIL" : "PASS") << ": N=" << N
            << "; maximum relative errors physical/MG=" << worst_physical
            << ", spurious/MG=" << worst_spurious << '\n';
  return failures ? 1 : 0;
}
} // namespace

int main(int argc, char** argv) {
  try {
    Options options;
    for (int i = 1; i < argc; ++i) {
      const std::string key(argv[i]);
      if (key == "--self-test") { options.self_test_only = true; continue; }
      if (++i == argc) throw std::invalid_argument("Missing value for " + key);
      const std::string value(argv[i]);
      if (key == "--photons") {
        const auto count = unsigned_value(value);
        if (count < 2 || count > 8) throw std::invalid_argument("Select 2 through 8 photons.");
        options.photons = static_cast<unsigned>(count);
      }
      else if (key == "--archived-rates") {
        if (value != "raw" && value != "averaged") throw std::invalid_argument("Select raw or averaged archive.");
        options.archived_rates = value;
      }
      else if (key == "--check") {
        if (value != "both" && value != "physical" && value != "spurious")
          throw std::invalid_argument("Select both, physical or spurious.");
        options.check = value;
      }
      else if (key == "--control-points") options.control_points = unsigned_value(value);
      else if (key == "--output-mode") {
        if (value != "all" && value != "outliers") throw std::invalid_argument("Select all or outliers.");
        options.outliers_only = value == "outliers";
      }
      else if (key == "--poles") {
        if (value == "summed") options.poles = hepcat_all_positive::PoleEvaluation::SummedMomentum;
        else if (value == "expanded") options.poles = hepcat_all_positive::PoleEvaluation::ExternalShellExpansion;
        else throw std::invalid_argument("Select summed or expanded poles.");
      }
      else if (key == "--points") options.points = unsigned_value(value);
      else if (key == "--seed") options.seed = unsigned_value(value);
      else if (key == "--mass") options.mass = real_value(value);
      else if (key == "--charge") options.charge = real_value(value);
      else if (key == "--minimum-energy") options.minimum_energy = real_value(value);
      else if (key == "--maximum-energy") options.maximum_energy = real_value(value);
      else if (key == "--relative-tolerance") options.tolerance = real_value(value);
      else if (key == "--reference") options.reference = value;
      else if (key == "--output") options.output = value;
      else throw std::invalid_argument("Unknown option " + key);
    }
    std::cout << std::setprecision(12);
    std::mt19937_64 control_random(202611050708ULL);
    if (options.control_points) {
      small_check<2>(control_random, options.control_points);
      small_check<3>(control_random, options.control_points);
      small_check<4>(control_random, options.control_points);
    } else if (options.self_test_only) throw std::invalid_argument("Select at least one control point.");
    if (options.self_test_only) return 0;
    if (!options.points || options.reference.empty() || options.output.empty() ||
        !std::isfinite(options.mass) || options.mass <= 0 || !std::isfinite(options.charge) ||
        !std::isfinite(options.tolerance) || options.tolerance < 0)
      throw std::invalid_argument("Select a reference, positive mass and point count, and finite inputs.");
    std::cout << "Seed: " << options.seed << '\n';
    switch (options.photons) {
      case 2: return validate<2>(options);
      case 3: return validate<3>(options);
      case 4: return validate<4>(options);
      case 5: return validate<5>(options);
      case 6: return validate<6>(options);
      case 7: return validate<7>(options);
      case 8: return validate<8>(options);
      default: throw std::invalid_argument("This driver tests 2 through 8 photons.");
    }
  } catch (const std::exception& error) {
    std::cerr << "Validation error: " << error.what() << '\n';
    return 2;
  }
}
