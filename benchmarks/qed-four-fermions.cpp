// Compare complete e- e+ -> e- e+ + N photons, N=0,1,2.
// Every backend receives the same saved massive, physical phase-space points.
#include "benchmark.hpp"
#include <ctime>
#include <thread>
#include <type_traits>
#include <sys/utsname.h>

namespace hepcat_four_fermion_benchmark {

using Real = hepcat_benchmark::Real;
using Clock = hepcat_benchmark::Clock;
using Configuration = hepcat_benchmark::Configuration;
using hepcat_benchmark::median;
using hepcat_benchmark::relative_error;
static_assert(std::is_same<Real, double>::value,
              "This timing comparison requires double SPINAS exports and library.");

const std::array<const char*, 4> backend_names = {{
    "HEPCAT/SPINAS AllPhysical", "HEPCAT/SPINAS AllSpurious", "CalcHEP", "MadGraph"}};
const std::array<const char*, 4> backend_columns = {{
    "physical", "spurious", "calchep", "madgraph"}};

struct Options : hepcat_benchmark::Options {
  Options() { repetitions = 9; output = "four-fermion-timings"; }
};

struct Result {
  std::size_t photons = 0;
  std::array<std::vector<double>, 4> seconds;
  std::array<Real, 3> maximum_relative_error = {{0, 0, 0}};
  std::array<Real, 2> maximum_exhaustive_error = {{0, 0}};
  std::string parameters_json;
};

using Runner = std::function<Result(const Options&)>;
struct Entry { std::size_t photons; Runner run; };
void register_processes(std::vector<Entry>& entries);

inline double quantile(std::vector<double> values, double fraction) {
  if (values.empty()) throw std::invalid_argument("No timing measurements.");
  std::sort(values.begin(), values.end());
  const double position = fraction * (values.size() - 1);
  const auto lower = static_cast<std::size_t>(std::floor(position));
  const auto upper = static_cast<std::size_t>(std::ceil(position));
  return values[lower] + (position - lower) * (values[upper] - values[lower]);
}

Options parse_options(int argc, char** argv) {
  Options options;
  for (int index = 1; index < argc; ++index) {
    const std::string key(argv[index]);
    if (key == "--help") {
      std::cout << "qed_four_fermion_benchmark [--points 10000] [--repeats 9] [--warmup 100]\n"
                   "  [--energy 100] [--seed INTEGER] [--tolerance 1e-7]\n"
                   "  [--output four-fermion-timings]\n"
                   "Energy is sqrt(s) in GeV; output is a filename prefix.\n";
      std::exit(0);
    }
    if (index + 1 == argc) throw std::invalid_argument("Missing value for " + key);
    const std::string value(argv[++index]);
    std::size_t consumed = 0;
    if (key == "--output") { options.output = value; continue; }
    if (key == "--energy" || key == "--tolerance") {
      const Real number = std::stod(value, &consumed);
      if (consumed != value.size() || !std::isfinite(number) || number <= 0)
        throw std::invalid_argument("Invalid value for " + key);
      if (key == "--energy") options.energy = number; else options.tolerance = number;
    } else {
      if (value.empty() || value[0] == '-') throw std::invalid_argument("Invalid value for " + key);
      const auto number = std::stoull(value, &consumed);
      if (consumed != value.size()) throw std::invalid_argument("Invalid value for " + key);
      if (key == "--seed") options.seed = number;
      else if (key == "--points") options.points = number;
      else if (key == "--repeats") options.repetitions = number;
      else if (key == "--warmup") options.warmup = number;
      else throw std::invalid_argument("Unknown option " + key);
    }
  }
  if (!options.points || !options.repetitions || options.output.empty())
    throw std::invalid_argument("Point count, repetitions and output prefix must be nonempty.");
  return options;
}

template <class Process>
void check_process_interface(const Process& process, std::size_t photons, Real normalization) {
  constexpr std::size_t legs = Process::leg_count;
  if (Process::incoming_legs() != std::array<std::size_t, 2>{{0, 1}})
    throw std::invalid_argument("The incoming electron and positron must occupy the first two legs.");
  const Real mass = process.masses()[0];
  if (!(mass > 0)) throw std::invalid_argument("Use a positive electron mass.");
  for (std::size_t leg = 0; leg < legs; ++leg) {
    const Real expected = leg < 4 ? mass : 0;
    if (process.masses()[leg] != expected)
      throw std::invalid_argument("Expected four equal-mass fermions followed by massless photons.");
  }
  std::set<std::vector<int>> coverage;
  for (const auto& spins : Process::helicity_configurations()) {
    std::vector<int> photons_row;
    for (std::size_t leg = 0; leg < legs; ++leg) {
      if (leg < 4 && spins[leg]) throw std::invalid_argument("All four massive spins must be summed.");
      if (leg >= 4) {
        if (spins[leg] != -2 && spins[leg] != 2)
          throw std::invalid_argument("Each photon must have a definite +/-2 doubled helicity.");
        photons_row.push_back(spins[leg]);
      }
    }
    if (!coverage.insert(photons_row).second)
      throw std::invalid_argument("Duplicate photon-helicity configuration.");
  }
  if (coverage.size() != (std::size_t(1) << photons))
    throw std::invalid_argument("A complete set of all photon-helicity combinations is required.");
  const Real exported = Process::symmetry_factor() / Process::incoming_spin_average();
  if (std::abs(Process::incoming_spin_average() - 4) > 1e-14
      || relative_error(exported, normalization) > 1e-14)
    throw std::invalid_argument("Use the common incoming 1/4 average and outgoing 1/N! photon factor.");
}

template <class Process>
Real exhaustive_squared_amplitude(Process& process, Real normalization) {
  Real total = 0;
  for (auto spins : Process::helicity_configurations())
    for (int a : {-1, 1}) for (int b : {-1, 1})
      for (int c : {-1, 1}) for (int d : {-1, 1}) {
        spins[0] = a; spins[1] = b; spins[2] = c; spins[3] = d;
        const auto amplitude = process.amp(spins);
        if (!std::isfinite(amplitude.real()) || !std::isfinite(amplitude.imag()))
          throw std::runtime_error("A public complex amplitude is nonfinite.");
        total += std::norm(amplitude);
      }
  return normalization * total;
}

template <class PhysicalProcess, class SpuriousProcess>
Result run_process(const Options& options, Configuration config) {
  using namespace hepcat_spinas;
  constexpr std::size_t legs = PhysicalProcess::leg_count;
  static_assert(legs >= 4 && legs <= 6, "This benchmark covers four fermions plus zero, one or two photons.");
  static_assert(SpuriousProcess::leg_count == legs, "The two representations must describe the same process.");
  const std::size_t photons = legs - 4;
  Real factorial = 1;
  for (std::size_t n = 2; n <= photons; ++n) factorial *= n;
  const Real normalization = 1 / (4 * factorial);
  PhysicalProcess physical;
  SpuriousProcess spurious;
  madgraph_detail::prepare_zero_width_validation(physical, config.madgraph.width_parameter_indices, 0);
  madgraph_detail::prepare_zero_width_validation(spurious, config.madgraph.width_parameter_indices, 0);
  check_process_interface(physical, photons, normalization);
  check_process_interface(spurious, photons, normalization);
  if (physical.masses() != spurious.masses()
      || physical.get_parameters().size() != spurious.get_parameters().size())
    throw std::invalid_argument("The physical and spurious exports use different masses or parameter lists.");
  for (std::size_t i = 0; i < physical.get_parameters().size(); ++i)
    if (physical.get_parameters()[i] != spurious.get_parameters()[i])
      throw std::invalid_argument("The physical and spurious exports use different model parameters.");
  std::vector<int> expected_pdgs = {11, -11, 11, -11};
  expected_pdgs.insert(expected_pdgs.end(), photons, 22);
  if (config.pdgs != expected_pdgs || config.madgraph.physical_pdgs != expected_pdgs)
    throw std::invalid_argument("Expected the physical external order e- e+ -> e- e+ photons.");
  if (config.allowed_pdgs != std::vector<int>({11, 22})
      || config.madgraph.allowed_particle_pdgs != std::vector<int>({11, 22}))
    throw std::invalid_argument("Explicitly restrict both reference models to electrons and photons.");
  if (relative_error(config.madgraph.normalization, normalization) > 1e-14)
    throw std::invalid_argument("The MadGraph export uses a different normalization.");
  if (config.parameter_names.size() != physical.get_parameters().size())
    throw std::invalid_argument("Reference parameter names do not match the process parameter list.");

  Result result;
  result.photons = photons;
  std::ostringstream parameters_json;
  parameters_json << '{';
  std::vector<CalcHEPParameter> parameters;
  for (std::size_t i = 0; i < config.parameter_names.size(); ++i) {
    const auto value = physical.get_parameters()[i];
    parameters.push_back({config.parameter_names[i], checked_calchep_parameter(value, config.parameter_names[i])});
    if (i) parameters_json << ',';
    parameters_json << madgraph_detail::json_string(config.parameter_names[i]) << ':';
    madgraph_detail::json_number(parameters_json, std::real(value));
  }
  parameters_json << '}';
  result.parameters_json = parameters_json.str();

  std::cout << "N=" << photons << ": generating " << options.points
            << " common massive phase-space points..." << std::endl;
  std::mt19937_64 random(options.seed + photons);
  ValidationOptions generation;
  generation.minimum_center_of_mass_energy = options.energy;
  generation.maximum_center_of_mass_energy = options.energy;
  std::vector<typename PhysicalProcess::Momenta> points;
  std::vector<std::vector<double>> mg_points;
  points.reserve(options.points); mg_points.reserve(options.points);
  const auto suffix = "-N" + std::to_string(photons);
  std::ofstream saved(options.output + "-points" + suffix + ".csv");
  if (!saved) throw std::runtime_error("Cannot write the common phase-space points.");
  saved << "point,leg,E,px,py,pz\n" << std::setprecision(17);
  for (std::size_t index = 0; index < options.points; ++index) {
    points.push_back(make_random_validation_momenta(physical.masses(), PhysicalProcess::incoming_legs(), random, generation));
    std::vector<double> flat;
    for (const auto leg : config.madgraph.leg_order) {
      if (leg >= legs) throw std::invalid_argument("The MadGraph external-leg map is invalid.");
      for (const auto component : points.back()[leg]) flat.push_back(component);
    }
    mg_points.push_back(std::move(flat));
    for (std::size_t leg = 0; leg < legs; ++leg) {
      saved << index + 1 << ',' << leg + 1;
      for (const auto component : points.back()[leg]) saved << ',' << component;
      saved << '\n';
    }
  }
  saved.close();
  if (!saved) throw std::runtime_error("Writing the common points failed.");

  std::cout << "N=" << photons << ": preparing the independent zero-width references..." << std::endl;
  CalcHEPProcess calchep(config.calchep_model_directory, config.calchep_model_number,
      config.pdgs, parameters, 1.0, false, config.allowed_pdgs, 3);
  check_calchep_masses(physical.masses(), calchep.masses());
  config.madgraph.optimization_level = 3;
  madgraph_detail::validate_configuration(physical, config.madgraph);
  madgraph_detail::Workspace workspace;
  const auto configuration = workspace.path() + "/configuration.json";
  const auto output = workspace.path() + "/reference";
  const auto log = workspace.path() + "/preparation.log";
  madgraph_detail::write_configuration(configuration, physical, config.madgraph);
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
  if (!madgraph.has_sum()) throw std::runtime_error("MadGraph lacks the exhaustive spin-sum interface.");
  std::ofstream provenance(options.output + "-madgraph" + suffix + ".json");
  provenance << madgraph_detail::read_text(output + "/result.json");
  if (!provenance) throw std::runtime_error("Cannot save MadGraph preparation provenance.");

  const auto evaluate = [&](std::size_t backend, std::size_t index) -> Real {
    if (backend == 0) { physical.set_momenta(points[index]); return physical.amp2(); }
    if (backend == 1) { spurious.set_momenta(points[index]); return spurious.amp2(); }
    if (backend == 2) return calchep.squared_amplitude(points[index]);
    return normalization * madgraph.evaluate_sum(mg_points[index]);
  };
  std::array<std::vector<Real>, 4> values;
  for (auto& backend : values) backend.resize(options.points);
  std::ofstream checks(options.output + "-values" + suffix + ".csv");
  if (!checks) throw std::runtime_error("Cannot write the full validation comparisons.");
  checks << "point,physical,physical_exhaustive,spurious,spurious_exhaustive,calchep,madgraph,"
            "spurious_relative_error,calchep_relative_error,madgraph_relative_error\n"
         << std::setprecision(17);
  std::cout << "N=" << photons << ": validating every point and all 16 massive-spin combinations x "
            << (std::size_t(1) << photons) << " photon helicities..." << std::endl;
  std::size_t failures = 0;
  for (std::size_t index = 0; index < options.points; ++index) {
    for (std::size_t backend = 0; backend < 4; ++backend) values[backend][index] = evaluate(backend, index);
    // The optimized amp2 and the complete public complex-amplitude inventory
    // must agree before any timing is accepted. No helicity is omitted.
    const Real physical_full = exhaustive_squared_amplitude(physical, normalization);
    const Real spurious_full = exhaustive_squared_amplitude(spurious, normalization);
    const Real physical_error = relative_error(values[0][index], physical_full);
    const Real spurious_error = relative_error(values[1][index], spurious_full);
    result.maximum_exhaustive_error[0] = std::max(result.maximum_exhaustive_error[0], physical_error);
    result.maximum_exhaustive_error[1] = std::max(result.maximum_exhaustive_error[1], spurious_error);
    std::array<Real, 3> errors;
    bool failed = physical_error > options.tolerance || spurious_error > options.tolerance;
    for (std::size_t reference = 0; reference < 3; ++reference) {
      errors[reference] = relative_error(values[0][index], values[reference + 1][index]);
      result.maximum_relative_error[reference] = std::max(result.maximum_relative_error[reference], errors[reference]);
      failed = failed || errors[reference] > options.tolerance;
    }
    failures += failed;
    checks << index + 1 << ',' << values[0][index] << ',' << physical_full << ','
           << values[1][index] << ',' << spurious_full << ',' << values[2][index] << ',' << values[3][index];
    for (const auto error : errors) checks << ',' << error;
    checks << '\n';
  }
  checks.close();
  if (!checks) throw std::runtime_error("Writing validation results failed.");
  if (failures) throw std::runtime_error(std::to_string(failures)
      + " validation points disagree. No timing row accepted; see the saved values CSV.");

  const auto expected = values[0];
  for (std::size_t backend = 0; backend < 4; ++backend)
    for (std::size_t index = 0; index < options.warmup; ++index)
      values[backend][index % options.points] = evaluate(backend, index % options.points);
  for (std::size_t repetition = 0; repetition < options.repetitions; ++repetition)
    for (std::size_t slot = 0; slot < 4; ++slot) {
      // Rotate which backend runs first, but never run two backends together.
      const auto backend = (slot + repetition + photons) % 4;
      const auto start = Clock::now();
      for (std::size_t index = 0; index < options.points; ++index)
        values[backend][index] = evaluate(backend, index);
      const double seconds = std::chrono::duration<double>(Clock::now() - start).count();
      result.seconds[backend].push_back(seconds);
      Real checksum = 0;
      for (std::size_t index = 0; index < options.points; ++index) {
        if (relative_error(values[backend][index], expected[index]) > options.tolerance)
          throw std::runtime_error("A timed value differs from the validated complete spin sum.");
        checksum += values[backend][index];
      }
      if (!std::isfinite(checksum)) throw std::runtime_error("A timing checksum is nonfinite.");
      std::cout << "  " << backend_names[backend] << ", pass " << repetition + 1 << ": "
                << seconds << " s; checksum " << checksum << std::endl;
    }
  return result;
}

void write_results(const Options& options, const std::vector<Result>& results) {
  using namespace hepcat_spinas;
  if (results.empty() || results.front().photons != 0)
    throw std::runtime_error("The N=0 four-fermion baseline is required.");
  std::ofstream summary(options.output + ".csv");
  std::ofstream repetitions(options.output + "-repetitions.csv");
  std::ofstream metadata(options.output + "-metadata.json");
  if (!summary || !repetitions || !metadata) throw std::runtime_error("Cannot open the timing output files.");
  summary << "photons,points,repetitions";
  for (const auto name : backend_columns) summary << ',' << name << "_seconds";
  for (const auto name : backend_columns)
    summary << ',' << name << "_min_seconds," << name << "_max_seconds,"
            << name << "_q25_seconds," << name << "_q75_seconds";
  for (const auto name : backend_columns) summary << ',' << name << "_relative_N0";
  summary << ",spurious_max_relative_error,calchep_max_relative_error,madgraph_max_relative_error,"
             "physical_exhaustive_max_error,spurious_exhaustive_max_error\n" << std::setprecision(12);
  repetitions << "photons,backend,repetition,points,seconds,seconds_per_point\n" << std::setprecision(12);
  for (const auto& result : results) {
    summary << result.photons << ',' << options.points << ',' << options.repetitions;
    for (const auto& measurements : result.seconds) summary << ',' << median(measurements);
    for (const auto& measurements : result.seconds)
      summary << ',' << quantile(measurements, 0) << ',' << quantile(measurements, 1)
              << ',' << quantile(measurements, .25) << ',' << quantile(measurements, .75);
    for (std::size_t backend = 0; backend < 4; ++backend)
      summary << ',' << median(result.seconds[backend]) / median(results.front().seconds[backend]);
    for (const auto error : result.maximum_relative_error) summary << ',' << error;
    for (const auto error : result.maximum_exhaustive_error) summary << ',' << error;
    summary << '\n';
    for (std::size_t backend = 0; backend < 4; ++backend)
      for (std::size_t pass = 0; pass < result.seconds[backend].size(); ++pass)
        repetitions << result.photons << ',' << backend_columns[backend] << ',' << pass + 1 << ','
                    << options.points << ',' << result.seconds[backend][pass] << ','
                    << result.seconds[backend][pass] / options.points << '\n';
  }
  struct utsname machine;
  const bool have_machine = uname(&machine) == 0;
  const auto now = std::time(nullptr);
  char timestamp[64];
  std::strftime(timestamp, sizeof(timestamp), "%Y-%m-%dT%H:%M:%SZ", std::gmtime(&now));
  metadata << "{\n\"Status\":" << madgraph_detail::json_string(results.size() == 3 ? "COMPLETE" : "PARTIAL")
    << ",\n\"ValidatedRows\":\"PASS\",\n\"PhysicalProcess\":\"e- e+ -> e- e+ + N photons\""
    << ",\n\"RequestedPhotonCounts\":[0,1,2],\n\"CompletedPhotonCounts\":[";
  for (std::size_t i = 0; i < results.size(); ++i) { if (i) metadata << ','; metadata << results[i].photons; }
  metadata << "],\n\"UTC\":" << madgraph_detail::json_string(timestamp)
    << ",\n\"Seed\":" << options.seed << ",\n\"MultiplicitySeed\":\"Seed + N\",\n\"Points\":" << options.points
    << ",\n\"Repetitions\":" << options.repetitions << ",\n\"Warmup\":" << options.warmup
    << ",\n\"CenterOfMassEnergyGeV\":" << options.energy << ",\n\"Tolerance\":" << options.tolerance
    << ",\n\"Sampling\":\"Mass-aware sequential isotropic physical phase space at fixed energy; no cuts; common saved double momenta; not integration weights\""
    << ",\n\"Timing\":\"Serial wall time for set_momenta plus complete summed/averaged squared amplitude; median, minimum, maximum and interquartile range of full sweeps\""
    << ",\n\"Excluded\":\"Code generation, compilation, initialization, point generation, validation, warmup and output\""
    << ",\n\"Normalization\":\"Incoming spin average 1/4, outgoing identical-photon factor 1/N!; outgoing electron and positron are distinguishable\""
    << ",\n\"SPINASSpinSumValidation\":\"Every point checked against all16 massive-spin combinations and all2^N photon helicities in the public complex amplitude interface\""
    << ",\n\"MadGraphSum\":\"Exhaustive native MATRIX spin/helicity sum; no sampling or cached good-helicity mask\""
    << ",\n\"Widths\":\"Zero in both generated SPINAS evaluators and private CalcHEP/MadGraph reference models\""
    << ",\n\"ScalarPrecision\":\"double\",\n\"SPINASPrecisionBits\":" << std::numeric_limits<Real>::digits
    << ",\n\"LogicalProcessors\":" << std::thread::hardware_concurrency()
    << ",\n\"System\":" << madgraph_detail::json_string(have_machine ? std::string(machine.sysname) + " " + machine.release + " " + machine.machine : "unknown")
    << ",\n\"Build\":" << madgraph_detail::read_text(HEPCAT_BENCHMARK_BUILD_INFO)
    << ",\n\"ParametersByMultiplicity\":[";
  for (std::size_t i = 0; i < results.size(); ++i) {
    if (i) metadata << ',';
    metadata << "{\"N\":" << results[i].photons << ",\"Parameters\":" << results[i].parameters_json << '}';
  }
  metadata << "]\n}\n";
  if (!summary || !repetitions || !metadata) throw std::runtime_error("Writing the timing results failed.");
}

} // namespace hepcat_four_fermion_benchmark

// four-fermion-configure.py writes this include from the grouped process exports.
// It includes process.hpp files and registers run_process<Physical,Spurious>.
#ifndef HEPCAT_FOUR_FERMION_REGISTRATION_HEADER
#error "Define HEPCAT_FOUR_FERMION_REGISTRATION_HEADER to the generated registry include."
#endif
#include HEPCAT_FOUR_FERMION_REGISTRATION_HEADER

int main(int argc, char** argv) {
  using namespace hepcat_four_fermion_benchmark;
  try {
    auto options = parse_options(argc, argv);
    std::vector<Entry> entries;
    register_processes(entries);
    std::sort(entries.begin(), entries.end(), [](const Entry& a, const Entry& b) { return a.photons < b.photons; });
    if (entries.size() != 3 || entries[0].photons != 0 || entries[1].photons != 1 || entries[2].photons != 2)
      throw std::runtime_error("Configure complete N=0,1,2 pairs of physical/spurious exports.");
    std::cout << "Seed: " << options.seed << "; sqrt(s): " << options.energy << " GeV; "
              << options.points << " common points per photon count; " << options.repetitions
              << " repetitions, one backend at a time.\n";
    std::vector<Result> results;
    for (const auto& entry : entries) {
      Result result = entry.run(options);
      if (!results.empty() && result.parameters_json != results.front().parameters_json)
        throw std::runtime_error("Model parameters changed between multiplicities; normalization would be misleading.");
      results.push_back(std::move(result));
      write_results(options, results);
    }
    std::cout << "Wrote " << options.output << ".csv and its repetitions, points, validation and provenance.\n";
    return 0;
  } catch (const std::exception& error) {
    std::cerr << "Benchmark stopped: " << error.what() << '\n';
    return 1;
  }
}
