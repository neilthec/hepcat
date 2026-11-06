// Fixed-photon-helicity tests of the general four-fermion QED currents.
// This driver never calls constructive generation or factorization completion.
#include "four-fermion-all-plus-formulas.hpp"
#include "madgraph-runtime.hpp"
#include <algorithm>
#include <chrono>
#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <limits>
#include <random>
#include <string>
#include <vector>

namespace {
struct Options {
  unsigned photons = 3;
  std::size_t points = 1000;
  std::uint64_t seed = hepcat_spinas::detail::validation_seed_from_time();
  ldouble mass = 1, charge = ldouble(1)/3, tolerance = 1.e-7L;
  std::string reference, output = "four-fermion-all-plus";
};

Options arguments(int argc, char** argv) {
  Options result;
  for (int i = 1; i < argc; ++i) {
    const std::string flag = argv[i];
    if (flag == "--help") {
      std::cout << "four-fermion-all-plus-validation --photons 0..5 --points 1000 "
                   "--reference /path/to/libhepcat_madgraph.dylib --output prefix "
                   "[--seed INTEGER] [--mass VALUE] [--charge VALUE] "
                   "[--relative-tolerance VALUE]\n"
                   "Physical process: e- e+ -> e- e+ + N positive-helicity photons. "
                   "Match mass, charge and zero widths to the independent reference card.\n";
      std::exit(0);
    }
    if (++i == argc) throw std::invalid_argument("Missing value after " + flag);
    const std::string value = argv[i];
    if (flag == "--photons") result.photons = unsigned(std::stoul(value));
    else if (flag == "--points") result.points = std::stoull(value);
    else if (flag == "--seed") result.seed = std::stoull(value);
    else if (flag == "--mass") result.mass = std::stold(value);
    else if (flag == "--charge") result.charge = std::stold(value);
    else if (flag == "--relative-tolerance") result.tolerance = std::stold(value);
    else if (flag == "--reference") result.reference = value;
    else if (flag == "--output") result.output = value;
    else throw std::invalid_argument("Unknown option: " + flag);
  }
  if (result.photons > 5 || !result.points || result.reference.empty() ||
      !(result.mass > 0) || !(result.charge > 0) || !(result.tolerance > 0) ||
      !std::isfinite(result.mass) || !std::isfinite(result.charge) ||
      !std::isfinite(result.tolerance))
    throw std::invalid_argument("Select 0 through 5 photons, positive points/mass/charge/tolerance, and a reference library.");
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

ldouble relative(ldouble first, ldouble second) {
  if (!std::isfinite(first) || !std::isfinite(second))
    return std::numeric_limits<ldouble>::infinity();
  const ldouble scale = std::max(std::abs(first),std::abs(second));
  return scale ? std::abs(first-second)/scale : 0;
}

template<std::size_t N>
ldouble component_error(hepcat_four_fermion_all_plus::Formula<N>& first,
                        hepcat_four_fermion_all_plus::Formula<N>& second) {
  ldouble scale = 0, difference = 0;
  for (int a : {-1,1}) for (int b : {-1,1})
    for (int c : {-1,1}) for (int d : {-1,1}) {
      const std::array<int,4> spins = {{a,b,c,d}};
      const cdouble x = first.amplitude(spins), y = second.amplitude(spins);
      if (!std::isfinite(std::abs(x)) || !std::isfinite(std::abs(y)))
        return std::numeric_limits<ldouble>::infinity();
      scale = std::max(scale,std::max(std::abs(x),std::abs(y)));
      difference = std::max(difference,std::abs(x-y));
    }
  return scale ? difference/scale : 0;
}

struct Maximum {
  ldouble value = 0;
  std::size_t failures = 0;
  void add(ldouble error, ldouble tolerance) {
    value = std::isfinite(error) ? std::max(value,error)
                                : std::numeric_limits<ldouble>::infinity();
    if (!(error <= tolerance)) ++failures;
  }
};

void number(std::ostream& stream, ldouble value) {
  if (std::isfinite(value)) stream << value;
  else stream << "null";
}

template<std::size_t N>
int validate(const Options& options) {
  using Formula = hepcat_four_fermion_all_plus::Formula<N>;
  using Momenta = typename Formula::Momenta;
  std::array<ldouble,N+4> masses = {};
  for (std::size_t leg = 0; leg < 4; ++leg) masses[leg] = options.mass;
  hepcat_spinas::ValidationOptions generation;
  generation.minimum_center_of_mass_energy = 10;
  generation.maximum_center_of_mass_energy = 100;
  hepcat_spinas::madgraph_detail::Reference reference(options.reference);
  std::mt19937_64 random(options.seed);
  std::ofstream values(options.output + "-values.csv"), points(options.output + "-points.csv");
  if (!values || !points) throw std::runtime_error("Cannot open output CSV files.");
  values << std::setprecision(std::numeric_limits<ldouble>::max_digits10)
         << "point,formula,madgraph,mg_relative_error,reference_component_error,"
            "bose_component_error,ward_relative_error,component_ward_relative_error";
  for (int a : {-1,1}) for (int b : {-1,1})
    for (int c : {-1,1}) for (int d : {-1,1}) {
      const std::string label = std::string(a<0?"m":"p") + (b<0?"m":"p") +
        (c<0?"m":"p") + (d<0?"m":"p");
      values << ",amp_" << label << "_real,amp_" << label << "_imag";
    }
  values << '\n';
  points << std::setprecision(std::numeric_limits<ldouble>::max_digits10)
         << "point,leg,E,px,py,pz\n";
  const typename Formula::FourMomentum alternate = {{1,0,1,0}};
  Maximum mg, gauge, bose, ward;
  ldouble component_ward_maximum = 0;
  std::size_t failures = 0;
  const auto start = std::chrono::steady_clock::now();
  for (std::size_t sample = 0; sample < options.points; ++sample) {
    const Momenta p = hepcat_spinas::make_random_validation_momenta(masses,{{0,1}},random,generation);
    Formula formula(p,options.mass,options.charge),
      changed_reference(p,options.mass,options.charge,&alternate);
    Momenta permuted = p;
    std::reverse(permuted.begin()+4,permuted.end());
    Formula changed_permutation(permuted,options.mass,options.charge);
    std::vector<double> flat;
    for (std::size_t leg = 0; leg < N+4; ++leg) {
      points << sample << ',' << leg+1;
      for (ldouble entry : p[leg]) { flat.push_back(double(entry)); points << ',' << entry; }
      points << '\n';
    }
    ldouble expected = 0;
    std::vector<int> helicities(N+4,1);
    for (int a : {-1,1}) for (int b : {-1,1})
      for (int c : {-1,1}) for (int d : {-1,1}) {
        helicities[0]=a; helicities[1]=b; helicities[2]=c; helicities[3]=d;
        const double item = reference.evaluate(flat,helicities);
        if (!std::isfinite(item) || item < 0)
          throw std::runtime_error("MadGraph returned an invalid squared amplitude.");
        expected += item;
      }
    const ldouble actual = formula.spin_sum(), mg_error = relative(actual,expected),
      gauge_error = component_error(formula,changed_reference),
      bose_error = component_error(formula,changed_permutation),
      ward_error = formula.ward_relative_error(),
      component_ward_error = formula.component_ward_relative_error();
    component_ward_maximum = std::max(component_ward_maximum,component_ward_error);
    mg.add(mg_error,options.tolerance); gauge.add(gauge_error,options.tolerance);
    bose.add(bose_error,options.tolerance); ward.add(ward_error,options.tolerance);
    const bool passed = mg_error <= options.tolerance && gauge_error <= options.tolerance &&
      bose_error <= options.tolerance && ward_error <= options.tolerance;
    if (!passed) ++failures;
    values << sample << ',' << actual << ',' << expected << ',' << mg_error << ','
           << gauge_error << ',' << bose_error << ',' << ward_error << ',' << component_ward_error;
    for (int a : {-1,1}) for (int b : {-1,1})
      for (int c : {-1,1}) for (int d : {-1,1}) {
        const cdouble amp = formula.amplitude({{a,b,c,d}});
        values << ',' << amp.real() << ',' << amp.imag();
      }
    values << '\n';
    if (!passed && failures <= 3)
      std::cerr << "N=" << N << " point=" << sample << " MG=" << mg_error
                << " reference=" << gauge_error << " Bose=" << bose_error
                << " Ward=" << ward_error << '\n';
  }
  const double elapsed = std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count();
  std::ofstream summary(options.output + ".json");
  if (!summary) throw std::runtime_error("Cannot open output summary.");
  summary << std::setprecision(std::numeric_limits<ldouble>::max_digits10)
    << "{\n  \"Status\": " << quoted(failures?"FAIL":"PASS")
    << ",\n  \"Photons\": " << N << ",\n  \"ExternalParticles\": " << N+4
    << ",\n  \"Points\": " << options.points << ",\n  \"FailedPoints\": " << failures
    << ",\n  \"Seed\": " << options.seed << ",\n  \"Mass\": " << options.mass
    << ",\n  \"Charge\": " << options.charge
    << ",\n  \"MinimumCenterOfMassEnergy\": 10,\n  \"MaximumCenterOfMassEnergy\": 100"
    << ",\n  \"ScalarPrecisionBits\": " << std::numeric_limits<ldouble>::digits
    << ",\n  \"MadGraphPrecisionBits\": " << std::numeric_limits<double>::digits
    << ",\n  \"RelativeTolerance\": " << options.tolerance
    << ",\n  \"ElapsedSeconds\": " << elapsed
    << ",\n  \"ReferenceLibrary\": " << quoted(options.reference)
    << ",\n  \"ValuesCSV\": " << quoted(options.output + "-values.csv")
    << ",\n  \"PhaseSpaceCSV\": " << quoted(options.output + "-points.csv")
    << ",\n  \"Normalization\": \"Raw sum over 16 massive spins at fixed positive photon helicities; no initial averages or identical-photon factors.\""
    << ",\n  \"Process\": \"e- e+ -> e- e+ + N positive photons\""
    << ",\n  \"WardDiagnosticNormalization\": \"Each line/subset uses all four massive-spin current vectors and divergences in coherent Frobenius norms; no spin component is removed.\""
    << ",\n  \"MaximumIndividualComponentWardError\": ";
  number(summary,component_ward_maximum);
  summary << ",\n  \"IndividualComponentWardPolicy\": \"Informational: normalization by a nearly vanishing individual spin current can amplify arithmetic cancellation. Accepted Ward check uses the complete spin tensor.\""
    << ",\n  \"ConstructiveGenerationUsed\": false,\n  \"Checks\": {\n";
  const char* names[] = {"MadGraph","CommonAngleReference","PhotonBoseSymmetry","CurrentWardIdentity"};
  const Maximum* checks[] = {&mg,&gauge,&bose,&ward};
  for (std::size_t i=0; i<4; ++i) {
    summary << "    " << quoted(names[i]) << ": {\"MaximumRelativeError\": ";
    number(summary,checks[i]->value);
    summary << ", \"Failures\": " << checks[i]->failures << "}" << (i<3?",\n":"\n");
  }
  summary << "  }\n}\n";
  std::cout << "N=" << N << " points=" << options.points << " seed=" << options.seed
            << " MG=" << mg.value << " reference=" << gauge.value << " Bose=" << bose.value
            << " Ward=" << ward.value << " component_Ward=" << component_ward_maximum
            << " seconds=" << elapsed
            << " status=" << (failures?"FAIL":"PASS") << '\n';
  return failures?1:0;
}
}

int main(int argc,char** argv) {
  try {
    const Options options = arguments(argc,argv);
    switch(options.photons) {
      case 0:return validate<0>(options); case 1:return validate<1>(options);
      case 2:return validate<2>(options); case 3:return validate<3>(options);
      case 4:return validate<4>(options); case 5:return validate<5>(options);
    }
  } catch(const std::exception& error) { std::cerr << error.what() << '\n'; return 2; }
  return 2;
}
