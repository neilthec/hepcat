// Independent fixed-helicity MadGraph tests of the one-minus QED formula.
// The formula itself is evaluated without diagram generation or completion.
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

#include "one-minus-formulas.hpp"
#include "madgraph-runtime.hpp"
#include <algorithm>
#include <chrono>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <limits>
#include <random>
#include <sstream>
#include <string>
#include <vector>

namespace {
struct Options {
  unsigned photons = 5;
  std::size_t points = 1000;
  std::uint64_t seed = hepcat_spinas::detail::validation_seed_from_time();
  ldouble tolerance = 1.e-7L;
  ldouble mass = 1, charge = ldouble(1)/3;
  std::string reference, output = "one-minus";
};

Options arguments(int argc, char** argv) {
  Options result;
  for (int i = 1; i < argc; ++i) {
    const std::string flag = argv[i];
    if (flag == "--help") {
      std::cout << "one-minus-validation --photons 2..8 --points 1000 "
                   "--reference /path/to/libhepcat_madgraph.dylib "
                   "--output prefix [--seed INTEGER] [--mass VALUE] [--charge VALUE] "
                   "[--relative-tolerance VALUE]\n"
                   "Mass and charge must match the reference card; defaults m=1, e=1/3. "
                   "Reference widths must be zero. "
                   "Incoming electron and positron; final photon has negative helicity.\n";
      std::exit(0);
    }
    if (++i == argc) throw std::invalid_argument("Missing value after " + flag);
    const std::string value = argv[i];
    if (flag == "--photons") result.photons = unsigned(std::stoul(value));
    else if (flag == "--points") result.points = std::stoull(value);
    else if (flag == "--seed") result.seed = std::stoull(value);
    else if (flag == "--mass") result.mass = std::stold(value);
    else if (flag == "--charge") result.charge = std::stold(value);
    else if (flag == "--reference") result.reference = value;
    else if (flag == "--output") result.output = value;
    else if (flag == "--relative-tolerance") result.tolerance = std::stold(value);
    else throw std::invalid_argument("Unknown option: " + flag);
  }
  if (result.photons < 2 || result.photons > 8 || !result.points ||
      result.reference.empty() || !(result.tolerance > 0) ||
      !std::isfinite(result.tolerance) || !(result.mass > 0) ||
      !(result.charge > 0) || !std::isfinite(result.mass) || !std::isfinite(result.charge))
    throw std::invalid_argument("Select 2 through 8 photons, positive points/tolerance/mass/charge, and a reference library.");
  return result;
}

std::string quoted(const std::string& value) {
  std::string result = "\"";
  for (char c : value) {
    if (c == '\\' || c == '"') result += '\\';
    if (c == '\n') result += "\\n";
    else if (c == '\r') result += "\\r";
    else if (c == '\t') result += "\\t";
    else result += c;
  }
  return result + "\"";
}

ldouble relative(ldouble a, ldouble b) {
  if (!std::isfinite(a) || !std::isfinite(b)) return std::numeric_limits<ldouble>::infinity();
  const ldouble scale = std::max(std::abs(a), std::abs(b));
  return scale == 0 ? 0 : std::abs(a-b)/scale;
}

template<std::size_t N>
ldouble components(const hepcat_one_minus::Formula<N>& first,
                   const hepcat_one_minus::Formula<N>& second) {
  ldouble scale = 0, difference = 0;
  for (int a : {-1,1}) for (int b : {-1,1}) {
    const cdouble x = first.amplitude(a,b), y = second.amplitude(a,b);
    if (!std::isfinite(std::abs(x)) || !std::isfinite(std::abs(y)))
      return std::numeric_limits<ldouble>::infinity();
    scale = std::max(scale, std::max(std::abs(x),std::abs(y)));
    difference = std::max(difference,std::abs(x-y));
  }
  return scale == 0 ? 0 : difference/scale;
}

struct Maximum {
  ldouble value = 0;
  std::size_t failures = 0;
  void add(ldouble error, ldouble tolerance) {
    if (!std::isfinite(error)) value = std::numeric_limits<ldouble>::infinity();
    else value = std::max(value,error);
    if (!(error <= tolerance)) ++failures;
  }
};

void json_number(std::ostream& output, ldouble value) {
  if (std::isfinite(value)) output << value;
  else output << "null";
}

template<std::size_t N>
int validate(const Options& options) {
  using Formula = hepcat_one_minus::Formula<N>;
  using Momenta = typename Formula::Momenta;
  const ldouble mass = options.mass, charge = options.charge;
  std::array<ldouble,N+2> masses = {};
  masses[0] = masses[1] = mass;
  hepcat_spinas::ValidationOptions generation;
  generation.minimum_center_of_mass_energy = 10;
  generation.maximum_center_of_mass_energy = 100;
  std::mt19937_64 random(options.seed);
  hepcat_spinas::madgraph_detail::Reference reference(options.reference);
  std::ofstream values(options.output + "-values.csv"),
                points(options.output + "-points.csv");
  if (!values || !points) throw std::runtime_error("Could not open the output CSV files.");
  values << std::setprecision(std::numeric_limits<ldouble>::max_digits10)
         << "point,formula,madgraph,mg_relative_error,reference_component_error,positive_permutation_component_error,minus_relocation_component_error,ward_absolute,ward_relative";
  for (int a : {-1,1}) for (int b : {-1,1})
    values << ",amp_" << (a<0?'m':'p') << (b<0?'m':'p') << "_real,amp_"
           << (a<0?'m':'p') << (b<0?'m':'p') << "_imag";
  values << '\n';
  points << std::setprecision(std::numeric_limits<ldouble>::max_digits10)
         << "point,leg,E,px,py,pz\n";
  Maximum mg, gauge, permutation, relocation, ward;
  std::size_t failed_points = 0;
  const typename Formula::FourMomentum alternate_reference = {{1,0,1,0}};
  const auto started = std::chrono::steady_clock::now();
  for (std::size_t sample = 0; sample < options.points; ++sample) {
    const Momenta p = hepcat_spinas::make_random_validation_momenta(masses,{{0,1}},random,generation);
    Formula formula(p,mass,charge), changed_reference(p,mass,charge,N-1,&alternate_reference);
    Momenta permuted = p, reversed = p;
    // Reverse the order of just the positive photons, leaving the minus last.
    std::reverse(permuted.begin()+2,permuted.end()-1);
    // Move the negative photon from last to first, transporting its momentum.
    std::reverse(reversed.begin()+2,reversed.end());
    Formula changed_permutation(permuted,mass,charge), changed_location(reversed,mass,charge,0);
    std::vector<double> flattened;
    for (std::size_t leg = 0; leg < N+2; ++leg) {
      points << sample << ',' << leg+1;
      for (ldouble component : p[leg]) {
        flattened.push_back(double(component));
        points << ',' << component;
      }
      points << '\n';
    }
    std::vector<int> helicities(N+2,1); helicities[N+1] = -1;
    ldouble expected = 0;
    for (int a : {-1,1}) for (int b : {-1,1}) {
      helicities[0] = a; helicities[1] = b;
      expected += reference.evaluate(flattened,helicities);
    }
    const ldouble actual = formula.spin_sum();
    const ldouble mg_error = relative(actual,expected),
      gauge_error = components(formula,changed_reference),
      permutation_error = components(formula,changed_permutation),
      location_error = components(formula,changed_location);
    const ldouble energy = p[0][0]+p[1][0], absolute_ward = std::abs(formula.ward_identity());
    const ldouble ward_scale = std::max(energy*std::abs(formula.orbital_coefficient()),
                                      energy*energy*std::abs(formula.spin_coefficient()));
    const ldouble ward_error = ward_scale == 0 ? absolute_ward : absolute_ward/ward_scale;
    mg.add(mg_error,options.tolerance); gauge.add(gauge_error,options.tolerance);
    permutation.add(permutation_error,options.tolerance); relocation.add(location_error,options.tolerance);
    ward.add(ward_error,options.tolerance);
    const bool passed = mg_error <= options.tolerance && gauge_error <= options.tolerance &&
      permutation_error <= options.tolerance && location_error <= options.tolerance &&
      ward_error <= options.tolerance;
    if (!passed) ++failed_points;
    values << sample << ',' << actual << ',' << expected << ',' << mg_error << ','
           << gauge_error << ',' << permutation_error << ',' << location_error << ','
           << absolute_ward << ',' << ward_error;
    for (int a : {-1,1}) for (int b : {-1,1}) {
      const cdouble value = formula.amplitude(a,b);
      values << ',' << value.real() << ',' << value.imag();
    }
    values << '\n';
    if (!passed && failed_points <= 3)
      std::cerr << "N=" << N << " point=" << sample << " MG=" << mg_error
                << " reference=" << gauge_error << " permutation=" << permutation_error
                << " relocation=" << location_error << " Ward=" << ward_error << '\n';
  }
  const double elapsed = std::chrono::duration<double>(std::chrono::steady_clock::now()-started).count();
  std::ofstream summary(options.output + ".json");
  if (!summary) throw std::runtime_error("Could not open the output summary.");
  summary << std::setprecision(std::numeric_limits<ldouble>::max_digits10)
    << "{\n  \"Status\": " << quoted(failed_points ? "FAIL" : "PASS")
    << ",\n  \"Photons\": " << N << ",\n  \"ExternalParticles\": " << N+2
    << ",\n  \"Points\": " << options.points << ",\n  \"FailedPoints\": " << failed_points
    << ",\n  \"Seed\": " << options.seed
    << ",\n  \"Mass\": " << mass << ",\n  \"Charge\": " << charge
    << ",\n  \"MinimumCenterOfMassEnergy\": 10,\n  \"MaximumCenterOfMassEnergy\": 100"
    << ",\n  \"ScalarPrecisionBits\": " << std::numeric_limits<ldouble>::digits
    << ",\n  \"MadGraphPrecisionBits\": " << std::numeric_limits<double>::digits
    << ",\n  \"RelativeTolerance\": " << options.tolerance
    << ",\n  \"ElapsedSeconds\": " << elapsed
    << ",\n  \"ReferenceLibrary\": " << quoted(options.reference)
    << ",\n  \"ValuesCSV\": " << quoted(options.output + "-values.csv")
    << ",\n  \"PhaseSpaceCSV\": " << quoted(options.output + "-points.csv")
    << ",\n  \"Normalization\": \"Raw sum over four massive-fermion spins; last photon negative, others positive; no initial averages or final symmetry factor.\""
    << ",\n  \"ConstructiveGenerationUsed\": false,\n  \"Checks\": {\n";
  const char* names[] = {"MadGraph","SquareReference","PositivePermutation","MinusRelocation","WardIdentity"};
  const Maximum* checks[] = {&mg,&gauge,&permutation,&relocation,&ward};
  for (std::size_t i = 0; i < 5; ++i) {
    summary << "    " << quoted(names[i]) << ": {\"MaximumRelativeError\": ";
    json_number(summary,checks[i]->value);
    summary << ", \"Failures\": " << checks[i]->failures << "}" << (i<4 ? ",\n" : "\n");
  }
  summary << "  }\n}\n";
  std::cout << "N=" << N << " points=" << options.points << " seed=" << options.seed
            << " MG=" << mg.value << " reference=" << gauge.value
            << " permutation=" << permutation.value << " relocation=" << relocation.value
            << " Ward=" << ward.value << " seconds=" << elapsed
            << " status=" << (failed_points ? "FAIL" : "PASS") << '\n';
  return failed_points ? 1 : 0;
}
} // namespace

int main(int argc, char** argv) {
  try {
    const Options options = arguments(argc,argv);
    switch (options.photons) {
      case 2: return validate<2>(options);
      case 3: return validate<3>(options);
      case 4: return validate<4>(options);
      case 5: return validate<5>(options);
      case 6: return validate<6>(options);
      case 7: return validate<7>(options);
      case 8: return validate<8>(options);
    }
  } catch (const std::exception& error) {
    std::cerr << error.what() << '\n';
    return 2;
  }
  return 2;
}
