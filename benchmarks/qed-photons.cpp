// Time the same stored physical points in SPINAS, CalcHEP and independent MG.
#include "benchmark.hpp"
#include <ctime>
#include <iostream>
#include <thread>
#include <sys/utsname.h>

namespace hepcat_benchmark {
Options parse_options(int argc, char** argv) {
  Options options;
  for (int index = 1; index < argc; ++index) {
    const std::string key(argv[index]);
    if (key == "--help") {
      std::cout << "qed_photon_benchmark [--points 10000] [--repeats 5] [--warmup 100]\n"
                   "  [--energy 100] [--seed INTEGER] [--tolerance 1e-7]\n"
                   "  [--output photon-timings]\n"
                   "energy is sqrt(s) in GeV; output is a filename prefix.\n";
      std::exit(0);
    }
    if (index + 1 == argc) throw std::invalid_argument("Missing value for " + key);
    const std::string value(argv[++index]);
    std::size_t consumed = 0;
    if (key == "--output") { options.output = value; continue; }
    if (key == "--energy" || key == "--tolerance") {
      const Real number = std::stold(value, &consumed);
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

void write_results(const Options& options, const std::vector<Result>& results) {
  using namespace hepcat_spinas;
  if (results.empty() || results.front().photons != 2)
    throw std::runtime_error("The N=2 baseline is required for normalization.");
  std::ofstream summary(options.output + ".csv");
  std::ofstream repetitions(options.output + "-repetitions.csv");
  std::ofstream metadata(options.output + "-metadata.json");
  if (!summary || !repetitions || !metadata) throw std::runtime_error("Cannot open benchmark output files.");
  summary << "photons,points,repetitions,spinas_seconds,calchep_seconds,madgraph_seconds,"
             "spinas_relative_N2,calchep_relative_N2,madgraph_relative_N2,"
             "calchep_max_relative_error,madgraph_max_relative_error\n" << std::setprecision(12);
  repetitions << "photons,backend,repetition,points,seconds,seconds_per_point\n" << std::setprecision(12);
  const char* names[] = {"SPINAS", "CalcHEP", "MadGraph"};
  for (const auto& result : results) {
    summary << result.photons << ',' << options.points << ',' << options.repetitions;
    for (const auto& values : result.seconds) summary << ',' << median(values);
    for (std::size_t backend = 0; backend < 3; ++backend)
      summary << ',' << median(result.seconds[backend]) / median(results.front().seconds[backend]);
    for (const auto error : result.maximum_relative_error) summary << ',' << error;
    summary << '\n';
    for (std::size_t backend = 0; backend < 3; ++backend)
      for (std::size_t pass = 0; pass < result.seconds[backend].size(); ++pass)
        repetitions << result.photons << ',' << names[backend] << ',' << pass + 1 << ','
                    << options.points << ',' << result.seconds[backend][pass] << ','
                    << result.seconds[backend][pass] / options.points << '\n';
  }
  struct utsname machine;
  const bool have_machine = uname(&machine) == 0;
  const auto now = std::time(nullptr);
  char timestamp[64];
  std::strftime(timestamp, sizeof(timestamp), "%Y-%m-%dT%H:%M:%SZ", std::gmtime(&now));
  metadata << "{\n\"Status\":" << madgraph_detail::json_string(
      results.size() == options.requested_photons.size() ? "COMPLETE" : "PARTIAL")
    << ",\n\"ValidatedRows\":\"PASS\",\n\"RequestedPhotonCounts\":[";
  for (std::size_t index = 0; index < options.requested_photons.size(); ++index) {
    if (index) metadata << ',';
    metadata << options.requested_photons[index];
  }
  metadata << "],\n\"CompletedPhotonCounts\":[";
  for (std::size_t index = 0; index < results.size(); ++index) {
    if (index) metadata << ',';
    metadata << results[index].photons;
  }
  metadata << "],\n\"UTC\":" << madgraph_detail::json_string(timestamp)
    << ",\n\"Seed\":" << options.seed << ",\n\"MultiplicitySeed\":\"Seed + N\",\n\"Points\":"
    << options.points << ",\n\"Repetitions\":" << options.repetitions << ",\n\"Warmup\":" << options.warmup
    << ",\n\"CenterOfMassEnergyGeV\":" << options.energy << ",\n\"Tolerance\":" << options.tolerance
    << ",\n\"Sampling\":\"Massless RAMBO, fixed energy, no cuts; saved common double momenta\""
    << ",\n\"Timing\":\"Serial wall time, set_momenta plus fully summed squared amplitude; median of full sweeps\""
    << ",\n\"Excluded\":\"Code generation, compilation, initialization, phase-space generation, validation, warmup, output\""
    << ",\n\"Normalization\":\"Incoming spin average 1/4, outgoing identical-photon factor 1/N!\""
    << ",\n\"MadGraphSum\":\"Exhaustive native MATRIX loop over full generated NHEL inventory; no helicity sampling or recycling\""
    << ",\n\"ReferencePrecision\":\"double\",\n\"SPINASPrecisionBits\":" << std::numeric_limits<Real>::digits
#ifdef WITH_LONG_DOUBLE
    << ",\n\"SPINASScalarType\":\"long double\""
#else
    << ",\n\"SPINASScalarType\":\"double\""
#endif
    << ",\n\"LogicalProcessors\":" << std::thread::hardware_concurrency()
    << ",\n\"System\":" << madgraph_detail::json_string(have_machine ? std::string(machine.sysname) + " " + machine.release + " " + machine.machine : "unknown")
    << ",\n\"Build\":" << madgraph_detail::read_text(HEPCAT_BENCHMARK_BUILD_INFO)
    << ",\n\"ParametersByMultiplicity\":[";
  for (std::size_t index = 0; index < results.size(); ++index) {
    if (index) metadata << ',';
    metadata << "{\"N\":" << results[index].photons << ",\"Parameters\":" << results[index].parameters_json << '}';
  }
  metadata << "]\n}\n";
  if (!summary || !repetitions || !metadata) throw std::runtime_error("Writing benchmark results failed.");
}
} // namespace hepcat_benchmark

int main(int argc, char** argv) {
  using namespace hepcat_benchmark;
  try {
    auto options = parse_options(argc, argv);
    std::vector<Entry> entries;
    register_processes(entries);
    std::sort(entries.begin(), entries.end(), [](const Entry& a, const Entry& b) { return a.photons < b.photons; });
    if (entries.empty() || entries.front().photons != 2)
      throw std::runtime_error("Configure a complete N=2 process as the baseline.");
    for (const auto& entry : entries) options.requested_photons.push_back(entry.photons);
    std::cout << "Seed: " << options.seed << "; sqrt(s): " << options.energy
              << " GeV; " << options.points << " points per multiplicity; "
              << options.repetitions << " repetitions, one backend at a time.\n";
    std::vector<Result> results;
    for (const auto& entry : entries) {
      results.push_back(entry.run(options));
      // Completed multiplicities remain usable if a larger process later fails.
      write_results(options, results);
    }
    std::cout << "Wrote " << options.output << ".csv and the repetition/validation/provenance files.\n";
    return 0;
  } catch (const std::exception& error) {
    std::cerr << "Benchmark stopped: " << error.what() << '\n';
    return 1;
  }
}
