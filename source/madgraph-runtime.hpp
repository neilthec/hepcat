#ifndef HEPCAT_MADGRAPH_RUNTIME_HPP
#define HEPCAT_MADGRAPH_RUNTIME_HPP

// Independent MadGraph validation of an exported SPINAS process.
// The reference is generated once in a private workspace. Each massless
// helicity choice is checked on the same physical momenta; massive spins are
// summed independently because the two programs need not use the same basis.

#include "spinas-validation.hpp"
#include <cerrno>
#include <cstring>
#include <cstdlib>
#include <fstream>
#include <iterator>
#include <map>
#include <mutex>
#include <thread>

#if defined(__unix__) || defined(__APPLE__)
#include <dirent.h>
#include <dlfcn.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#endif

namespace hepcat_spinas {

struct MadGraphValidationOptions : ValidationOptions {
  // Zero keeps the per-project preparation budget.
  unsigned preparation_timeout_seconds = 0;
  ldouble zero_absolute_tolerance = 1e-22L;
  MadGraphValidationOptions() = default;
  MadGraphValidationOptions(const ValidationOptions& base) : ValidationOptions(base) {}
};

struct MadGraphConfiguration {
  bool available = true;
  std::string reason;
  std::string madgraph_directory;
  std::string model;
  std::string python_executable;
  std::string helper_path;
  std::string fortran_compiler;
  // MadGraph receives incoming particles first. This maps each reference slot
  // to its zero-based leg in the exported SPINAS process.
  std::vector<std::size_t> leg_order;
  std::vector<int> physical_pdgs;
  // User-selected restriction: positive absolute PDGs allowed in the
  // reference process. Empty keeps every particle type in the selected UFO;
  // no particle-content restriction is inferred from the HEPCAT amplitude.
  std::vector<int> allowed_particle_pdgs;
  std::vector<int> spin2;
  bool full_color = false;
  std::vector<int> color_representations;
  // Original leg order: doubled massless helicities, zero for summed massive
  // particles and scalars. The exporter supplies every requested combination.
  std::vector<std::vector<int> > helicities;
  // The exporter identifies these from symbolic zero expressions.
  std::vector<std::vector<int> > exact_zero_helicities;
  std::vector<std::string> parameter_names;
  // Model-declared width roles; validation changes only a private process copy.
  std::vector<std::size_t> width_parameter_indices;
  std::map<std::string, double> parameter_rules;
  std::map<std::string, std::string> parameter_map;
  // Names are model-local; PDG mass roles resolve different reference names.
  std::map<std::string, std::vector<int> > mass_parameter_pdgs;
  // Apply incoming averages and the chosen final-state symmetry convention
  // once, identically on both sides. The MG bridge returns raw |M|^2.
  ldouble normalization = 1;
  unsigned preparation_timeout_seconds = 600;
  // Preserve ordinary validation builds; timing clients can request 1, 2 or 3.
  int optimization_level = 0;
};

struct MadGraphHelicityReport {
  std::vector<int> helicities;
  ValidationReport report;
  bool expected_exact_zero = false;
  ldouble zero_absolute_tolerance = 0;
};

struct MadGraphValidationReport {
  bool passed = false;
  std::string status = "NOT RUN";
  std::string message;
  std::uint64_t seed = 0;
  std::vector<MadGraphHelicityReport> rows;
  std::string build_log;
  std::string reference_build_json;
};

namespace madgraph_detail {

inline std::string json_string(const std::string& value) {
  std::ostringstream result;
  result << '"';
  for (const unsigned char c : value) {
    switch (c) {
    case '"': result << "\\\""; break;
    case '\\': result << "\\\\"; break;
    case '\n': result << "\\n"; break;
    case '\r': result << "\\r"; break;
    case '\t': result << "\\t"; break;
    default:
      if (c < 32) result << "\\u" << std::hex << std::setw(4)
                         << std::setfill('0') << static_cast<unsigned>(c) << std::dec;
      else result << c;
    }
  }
  result << '"';
  return result.str();
}

inline void json_number(std::ostream& out, ldouble value) {
  if (std::isfinite(value)) out << std::setprecision(std::numeric_limits<ldouble>::max_digits10) << value;
  else out << "null";
}

inline std::string read_text(const std::string& path, std::size_t limit = 131072) {
  std::ifstream input(path.c_str(), std::ios::binary);
  std::string value;
  char buffer[4096];
  while (input && value.size() < limit) {
    input.read(buffer, std::min(sizeof(buffer), limit - value.size()));
    value.append(buffer, static_cast<std::size_t>(input.gcount()));
  }
  return value;
}

inline std::string read_tail(const std::string& path, std::size_t limit = 32768) {
  std::ifstream input(path.c_str(), std::ios::binary);
  if (!input) return "";
  input.seekg(0, std::ios::end);
  const std::streamoff length = input.tellg();
  if (length < 0) return "";
  input.seekg(length > static_cast<std::streamoff>(limit)
    ? length - static_cast<std::streamoff>(limit) : 0);
  return std::string(std::istreambuf_iterator<char>(input), std::istreambuf_iterator<char>());
}

inline std::string preparation_failure_message(const std::string& log, int status) {
  const std::string prefix = "MadGraph reference preparation failed: ";
  const std::size_t position = log.rfind(prefix);
  if (position != std::string::npos) {
    std::string message = log.substr(position + prefix.size());
    const auto last = message.find_last_not_of(" \t\r\n");
    message.erase(last == std::string::npos ? 0 : last + 1);
    if (!message.empty()) return message.size() > 2048 ? message.substr(0, 2048) + "..." : message;
  }
  if (status == 124) return "MadGraph reference preparation exceeded its time limit.";
  if (status == 127 || status == 126)
    return "Could not execute the MadGraph reference helper. Check PythonExecutable and the helper path.";
  return "MadGraph reference preparation failed; inspect BuildLog and ReferenceBuildJSON.";
}

inline std::mutex& reference_mutex() {
  static std::mutex mutex;
  return mutex;
}

#if defined(__unix__) || defined(__APPLE__)
// Only used on the directory returned by mkdtemp. lstat avoids following links
// when deleting the private generated reference process.
inline void remove_tree(const std::string& path) noexcept {
  struct stat info;
  if (lstat(path.c_str(), &info) != 0) return;
  if (!S_ISDIR(info.st_mode)) { unlink(path.c_str()); return; }
  DIR* directory = opendir(path.c_str());
  if (directory) {
    while (struct dirent* entry = readdir(directory)) {
      if (std::strcmp(entry->d_name, ".") && std::strcmp(entry->d_name, ".."))
        remove_tree(path + "/" + entry->d_name);
    }
    closedir(directory);
  }
  rmdir(path.c_str());
}

class Workspace {
public:
  Workspace() {
    const char* temporary = std::getenv("TMPDIR");
    std::string pattern = std::string(temporary && *temporary ? temporary : "/tmp")
                        + "/hepcat-madgraph-XXXXXX";
    std::vector<char> writable(pattern.begin(), pattern.end());
    writable.push_back('\0');
    char* made = mkdtemp(writable.data());
    if (!made) throw std::runtime_error("Could not create a private MadGraph workspace.");
    path_ = made;
  }
  ~Workspace() { remove_tree(path_); }
  const std::string& path() const { return path_; }
  Workspace(const Workspace&) = delete;
  Workspace& operator=(const Workspace&) = delete;
private:
  std::string path_;
};

inline int run_helper(const std::vector<std::string>& arguments,
                      const std::string& log, unsigned timeout_seconds) {
  // Prepare argv before fork, so the child does not allocate C++ memory in
  // a potentially multithreaded caller before exec.
  std::vector<char*> argv;
  for (const auto& value : arguments) argv.push_back(const_cast<char*>(value.c_str()));
  argv.push_back(nullptr);
  const pid_t process = fork();
  if (process < 0) throw std::runtime_error("Could not start the MadGraph reference builder.");
  if (process == 0) {
    setpgid(0, 0);
    const int output = open(log.c_str(), O_WRONLY | O_CREAT | O_TRUNC, 0600);
    if (output < 0) _exit(126);
    dup2(output, STDOUT_FILENO);
    dup2(output, STDERR_FILENO);
    close(output);
    execvp(argv[0], argv.data());
    _exit(127);
  }
  setpgid(process, process);
  const auto started = std::chrono::steady_clock::now();
  int status = 0;
  for (;;) {
    const pid_t result = waitpid(process, &status, WNOHANG);
    if (result == process) break;
    if (result < 0 && errno != EINTR) {
      kill(-process, SIGKILL);
      while (waitpid(process, &status, 0) < 0 && errno == EINTR) {}
      throw std::runtime_error("Could not wait for the MadGraph reference builder.");
    }
    if (std::chrono::steady_clock::now() - started >= std::chrono::seconds(timeout_seconds)) {
      kill(-process, SIGKILL);
      while (waitpid(process, &status, 0) < 0 && errno == EINTR) {}
      return 124;
    }
    std::this_thread::sleep_for(std::chrono::milliseconds(50));
  }
  return WIFEXITED(status) ? WEXITSTATUS(status) : 128 + WTERMSIG(status);
}

class Reference {
public:
  typedef int (*Initialize)();
  typedef void (*Evaluate)(const double*, const int*, double*);
  typedef void (*EvaluateSum)(const double*, double*);
  explicit Reference(const std::string& path) : handle_(nullptr), evaluate_(nullptr), evaluate_sum_(nullptr) {
    handle_ = dlopen(path.c_str(), RTLD_NOW | RTLD_LOCAL);
    if (!handle_) {
      const char* error = dlerror();
      throw std::runtime_error(std::string("Could not load the MadGraph reference: ")
                             + (error ? error : "unknown loader error"));
    }
    Initialize initialize = reinterpret_cast<Initialize>(dlsym(handle_, "hepcat_mg_initialize"));
    evaluate_ = reinterpret_cast<Evaluate>(dlsym(handle_, "hepcat_mg_evaluate"));
    // Optional so older independently compiled references remain usable.
    evaluate_sum_ = reinterpret_cast<EvaluateSum>(dlsym(handle_, "hepcat_mg_evaluate_sum"));
    if (!initialize || !evaluate_) {
      dlclose(handle_); handle_ = nullptr;
      throw std::runtime_error("The MadGraph reference does not provide the expected C interface.");
    }
    if (initialize() != 0) {
      dlclose(handle_); handle_ = nullptr;
      throw std::runtime_error("The MadGraph reference could not initialize its model parameters.");
    }
  }
  ~Reference() { if (handle_) dlclose(handle_); }
  double evaluate(const std::vector<double>& momenta, const std::vector<int>& helicities) const {
    double answer = std::numeric_limits<double>::quiet_NaN();
    evaluate_(momenta.data(), helicities.data(), &answer);
    return answer;
  }
  bool has_sum() const { return evaluate_sum_ != nullptr; }
  // Raw exhaustive |M|^2 sum; callers apply averages and symmetry factors once.
  double evaluate_sum(const std::vector<double>& momenta) const {
    if (!evaluate_sum_)
      throw std::runtime_error("This MadGraph reference lacks the exhaustive spin-sum interface; rebuild it.");
    double answer = std::numeric_limits<double>::quiet_NaN();
    evaluate_sum_(momenta.data(), &answer);
    return answer;
  }
  Reference(const Reference&) = delete;
  Reference& operator=(const Reference&) = delete;
private:
  void* handle_;
  Evaluate evaluate_;
  EvaluateSum evaluate_sum_;
};
#endif

template <class Process>
void validate_configuration(const Process& process, const MadGraphConfiguration& config) {
  const std::size_t count = Process::leg_count;
  if (config.full_color && config.color_representations.size() != count)
    throw std::invalid_argument("A certified color process needs one representation per external leg.");
  if (config.leg_order.size() != count || config.physical_pdgs.size() != count
      || config.spin2.size() != count || config.helicities.empty()
      || config.parameter_names.size() != process.get_parameters().size())
    throw std::invalid_argument("MadGraph process metadata has inconsistent dimensions.");
  std::vector<bool> seen(count, false);
  for (const auto leg : config.leg_order) {
    if (leg >= count || seen[leg]) throw std::invalid_argument("Invalid MadGraph external-leg order.");
    seen[leg] = true;
  }
  if (!config.allowed_particle_pdgs.empty()) {
    for (const int pdg : config.allowed_particle_pdgs)
      if (pdg <= 0) throw std::invalid_argument("Allowed particle types must use positive absolute PDGs.");
    for (const int pdg : config.physical_pdgs) {
      if (pdg == std::numeric_limits<int>::min())
        throw std::invalid_argument("An external particle has an invalid PDG identifier.");
      const int absolute_pdg = pdg < 0 ? -pdg : pdg;
      if (std::find(config.allowed_particle_pdgs.begin(), config.allowed_particle_pdgs.end(), absolute_pdg)
          == config.allowed_particle_pdgs.end())
        throw std::invalid_argument("An external particle is absent from the allowed reference theory.");
    }
  }
  const auto incoming = Process::incoming_legs();
  if (count < 2 || config.leg_order[0] != incoming[0] || config.leg_order[1] != incoming[1])
    throw std::invalid_argument("MadGraph must receive the declared incoming legs first and in order.");
  if (config.optimization_level < 0 || config.optimization_level > 3)
    throw std::invalid_argument("The MadGraph optimization level must be from zero through three.");
  if (!std::isfinite(config.normalization) || config.normalization <= 0)
    throw std::invalid_argument("The common MadGraph validation normalization must be finite and positive.");
  if (config.python_executable.empty() || config.helper_path.empty()
      || config.madgraph_directory.empty() || config.model.empty()
      || !config.preparation_timeout_seconds)
    throw std::invalid_argument("MadGraph validation needs its installation, model, Python and reference helper.");
  for (const auto& zero : config.exact_zero_helicities) {
    if (std::find(config.helicities.begin(), config.helicities.end(), zero) == config.helicities.end())
      throw std::invalid_argument("A symbolic zero helicity is absent from the exported family.");
  }
  for (const auto& row : config.helicities) {
    if (row.size() != count) throw std::invalid_argument("A MadGraph helicity row has the wrong length.");
    for (std::size_t leg = 0; leg < count; ++leg) {
      const int spin = config.spin2[leg];
      if (spin < 0 || spin > 2)
        throw std::invalid_argument("MadGraph validation currently supports scalar, spin-1/2 and spin-1 external states.");
      const bool summed = process.masses()[leg] > 0 || spin == 0;
      if ((summed && row[leg] != 0) || (!summed && row[leg] != spin && row[leg] != -spin))
        throw std::invalid_argument("A helicity row must fix each massless spin and leave massive spins summed.");
    }
  }
}

// Generated processes support disabling all propagator widths, including
// literal and compound width expressions. Legacy custom test processes without
// width roles need no mutation; declared roles require the complete interface.
template <class Process>
auto prepare_zero_width_validation(Process& process,
    const std::vector<std::size_t>& indices, int)
    -> decltype(process.set_widths_enabled(false),
                process.set_parameters(process.get_parameters()), void()) {
  auto parameters = process.get_parameters();
  for (const auto index : indices) {
    if (index >= parameters.size())
      throw std::invalid_argument("A validation width parameter index is out of range.");
    parameters[index] = cdouble(0, 0);
  }
  if (!indices.empty()) process.set_parameters(parameters);
  process.set_widths_enabled(false);
}
template <class Process>
void prepare_zero_width_validation(Process&,
    const std::vector<std::size_t>& indices, long) {
  if (!indices.empty())
    throw std::invalid_argument("Re-export this process to validate with zero widths.");
}

template <class Process>
void write_configuration(const std::string& path, const Process& process,
                         const MadGraphConfiguration& config, unsigned preparation_timeout = 0) {
  std::ofstream out(path.c_str());
  if (!out) throw std::runtime_error("Could not write the private MadGraph model configuration.");
  out << "{\n\"ZeroWidths\":true,\n\"MadGraphDirectory\":" << json_string(config.madgraph_directory)
      << ",\n\"MadGraphModel\":" << json_string(config.model) << ",\n\"PhysicalPDGs\":[";
  for (std::size_t i = 0; i < config.physical_pdgs.size(); ++i) {
    if (i) out << ',';
    out << config.physical_pdgs[i];
  }
  out << "],\n\"FullColor\":" << (config.full_color ? "true" : "false")
      << ",\n\"ColorRepresentations\":[";
  if (config.full_color) for (std::size_t i = 0; i < config.leg_order.size(); ++i) {
    if (i) out << ',';
    out << config.color_representations.at(config.leg_order[i]);
  }
  out << "],\n\"Masses\":[";
  for (std::size_t i = 0; i < config.leg_order.size(); ++i) {
    if (i) out << ',';
    json_number(out, process.masses()[config.leg_order[i]]);
  }
  out << "],\n\"Parameters\":[";
  const auto& parameters = process.get_parameters();
  for (std::size_t i = 0; i < parameters.size(); ++i) {
    const ldouble real = std::real(parameters[i]);
    if (!std::isfinite(real) || std::imag(parameters[i]) != 0)
      throw std::invalid_argument("MadGraph model matching currently requires finite real exported parameters.");
    if (i) out << ',';
    out << "{\"Name\":" << json_string(config.parameter_names[i]) << ",\"Value\":";
    json_number(out, real); out << '}';
  }
  out << "],\n\"ParameterRules\":{";
  bool first = true;
  for (const auto& rule : config.parameter_rules) {
    if (!std::isfinite(rule.second)) throw std::invalid_argument("A MadGraph parameter override is not finite.");
    if (!first) out << ',';
    first = false;
    out << json_string(rule.first) << ':'; json_number(out, rule.second);
  }
  out << "},\n\"ParameterMap\":{";
  first = true;
  for (const auto& mapping : config.parameter_map) {
    if (!first) out << ',';
    first = false;
    out << json_string(mapping.first) << ':' << json_string(mapping.second);
  }
  out << "},\n\"MassParameterPDGs\":{";
  first = true;
  for (const auto& role : config.mass_parameter_pdgs) {
    if (!first) out << ',';
    first = false;
    out << json_string(role.first) << ":[";
    for (std::size_t i = 0; i < role.second.size(); ++i) {
      if (i) out << ',';
      out << role.second[i];
    }
    out << ']';
  }
  out << '}';
  if (!config.allowed_particle_pdgs.empty()) {
    out << ",\n\"AllowedParticlePDGs\":[";
    for (std::size_t i = 0; i < config.allowed_particle_pdgs.size(); ++i) {
      if (i) out << ',';
      out << config.allowed_particle_pdgs[i];
    }
    out << ']';
  }
  if (!config.fortran_compiler.empty())
    out << ",\n\"FortranCompiler\":" << json_string(config.fortran_compiler);
  out << ",\n\"OptimizationLevel\":" << config.optimization_level;
  out << ",\n\"TimeConstraint\":"
      << (preparation_timeout ? preparation_timeout : config.preparation_timeout_seconds)
      << "\n}\n";
  if (!out) throw std::runtime_error("Writing the private MadGraph configuration failed.");
}

// The integer labels enumerate each basis completely. They are never compared
// state by state across programs for massive particles.
template <class Process, class Evaluator>
ldouble sum_massive_spins(const Process& process, const MadGraphConfiguration& config,
                          typename Process::Spins& spins, std::size_t leg, Evaluator evaluate) {
  if (leg == Process::leg_count) return evaluate(spins);
  const int spin = config.spin2[leg];
  if (process.masses()[leg] == 0 || spin == 0)
    return sum_massive_spins(process, config, spins, leg + 1, evaluate);
  ldouble result = 0;
  for (int state = -spin; state <= spin; state += 2) {
    spins[leg] = state;
    result += sum_massive_spins(process, config, spins, leg + 1, evaluate);
  }
  return result;
}

// Native SPINAS vector labels follow the supplied all-ingoing spinor formula.
// HELAS conjugates an outgoing vector wavefunction, so its massless outgoing
// label is the opposite one. Incoming vectors and already-crossed fermion
// states retain their sign. Massive vector spins are summed, not identified
// individually across the two spin bases.
inline int madgraph_helicity(int doubled, int spin2,
                            bool incoming = true, bool massless = true) {
  const int helicity = spin2 == 2 ? doubled / 2 : doubled;
  return !incoming && massless && spin2 == 2 ? -helicity : helicity;
}

// A certified colored process contracts its complete color basis coherently.
// Existing colorless and user-written processes retain their ordinary amp.
template <class Process>
auto squared_amplitude(Process& process, const typename Process::Spins& spins, int)
    -> decltype(process.color_spin_sum(spins)) { return process.color_spin_sum(spins); }
template <class Process>
auto squared_amplitude(Process& process, const typename Process::Spins& spins, long)
    -> decltype(std::norm(process.amp(spins))) { return std::norm(process.amp(spins)); }

} // namespace madgraph_detail

template <class Process>
MadGraphValidationReport validate_madgraph(
    Process& original_process, const MadGraphConfiguration& config,
    const MadGraphValidationOptions& options = MadGraphValidationOptions(), std::ostream* progress = nullptr) {
  MadGraphValidationReport result;
  result.seed = options.seed;
  if (!config.available) {
    result.status = "UNAVAILABLE";
    result.message = config.reason.empty() ? "MadGraph validation is unavailable for this export." : config.reason;
    return result;
  }
#if defined(__unix__) || defined(__APPLE__)
  // Fortran model readers and COMMON blocks use process-global state. Keep
  // validation serial even if independent C++ callers start at the same time.
  std::lock_guard<std::mutex> guard(madgraph_detail::reference_mutex());
  try {
    Process process(original_process);
    madgraph_detail::prepare_zero_width_validation(process, config.width_parameter_indices, 0);
    madgraph_detail::validate_configuration(process, config);
    if (!options.points || !std::isfinite(options.relative_tolerance) || options.relative_tolerance < 0
        || !std::isfinite(options.absolute_tolerance) || options.absolute_tolerance < 0
        || !std::isfinite(options.zero_absolute_tolerance) || options.zero_absolute_tolerance < 0)
      throw std::invalid_argument("Validation needs a positive point count and finite nonnegative tolerances.");
    detail::validation_energy_range(process.masses(), Process::incoming_legs(), options);
    if (progress) *progress << "Preparing independent MadGraph reference...\n" << std::flush;
    madgraph_detail::Workspace workspace;
    const std::string configuration = workspace.path() + "/configuration.json";
    const std::string output = workspace.path() + "/reference";
    const std::string log = workspace.path() + "/preparation.log";
    const unsigned preparation_budget = options.preparation_timeout_seconds
      ? options.preparation_timeout_seconds : config.preparation_timeout_seconds;
    madgraph_detail::write_configuration(configuration, process, config, preparation_budget);
    // The helper owns its compiler/MG process groups. Let it terminate them
    // and write diagnostics before the emergency outer watchdog fires.
    const unsigned watchdog_budget = preparation_budget > std::numeric_limits<unsigned>::max() - 10
      ? preparation_budget : preparation_budget + 10;
    const int status = madgraph_detail::run_helper(
      {config.python_executable, config.helper_path, "--config", configuration, "--output", output},
      log, watchdog_budget);
    result.build_log = madgraph_detail::read_tail(log, 131072);
    result.reference_build_json = madgraph_detail::read_text(output + "/result.json");
    if (status != 0) {
      const std::string message = madgraph_detail::preparation_failure_message(result.build_log, status);
      // Preserve actual tool diagnostics before the private reference is removed.
      for (const std::string step : {"model.log", "generate.log", "build.log", "initialize.log", "exception.log"}) {
        const std::string text = madgraph_detail::read_tail(output + "/" + step);
        if (!text.empty()) result.build_log += "\n[" + step + "]\n" + text;
      }
      throw std::runtime_error(message);
    }
#ifdef __APPLE__
    const std::string library = output + "/libhepcat_madgraph.dylib";
#else
    const std::string library = output + "/libhepcat_madgraph.so";
#endif
    madgraph_detail::Reference reference(library);
    for (const auto& helicities : config.helicities) {
      MadGraphHelicityReport row;
      row.helicities = helicities;
      row.expected_exact_zero = std::find(config.exact_zero_helicities.begin(),
        config.exact_zero_helicities.end(), helicities) != config.exact_zero_helicities.end();
      ValidationOptions comparison = options;
      if (row.expected_exact_zero) {
        row.zero_absolute_tolerance = options.zero_absolute_tolerance;
        comparison.absolute_tolerance = row.zero_absolute_tolerance;
        comparison.relative_tolerance = 0;
      }
      if (progress) {
        *progress << "Checking doubled helicities:";
        for (int helicity : helicities) *progress << ' ' << helicity;
        *progress << " (massive spins summed)\n";
      }
      typename Process::Spins base_spins = {};
      std::copy(helicities.begin(), helicities.end(), base_spins.begin());
      row.report = compare_squared_amplitudes<Process::leg_count>(
        process.masses(), Process::incoming_legs(),
        [&](const Momenta<Process::leg_count>& momenta) {
          process.set_momenta(momenta);
          auto spins = base_spins;
          const ldouble squared = madgraph_detail::sum_massive_spins(
            process, config, spins, 0, [&](const typename Process::Spins& state) {
              return madgraph_detail::squared_amplitude(process, state, 0);
            });
          if (row.expected_exact_zero && squared != 0)
            throw std::runtime_error("An exported symbolic zero evaluated to a nonzero squared amplitude.");
          return config.normalization * squared;
        },
        [&](const Momenta<Process::leg_count>& momenta) {
          std::vector<double> physical_momenta(4 * Process::leg_count);
          for (std::size_t slot = 0; slot < Process::leg_count; ++slot)
            for (std::size_t component = 0; component < 4; ++component)
              physical_momenta[4 * slot + component] = static_cast<double>(momenta[config.leg_order[slot]][component]);
          auto spins = base_spins;
          return config.normalization * madgraph_detail::sum_massive_spins(
            process, config, spins, 0, [&](const typename Process::Spins& state) {
              std::vector<int> reference_helicities(Process::leg_count);
              for (std::size_t slot = 0; slot < Process::leg_count; ++slot) {
                const std::size_t leg = config.leg_order[slot];
                reference_helicities[slot] = madgraph_detail::madgraph_helicity(
                  state[leg], config.spin2[leg], slot < Process::incoming_legs().size(),
                  process.masses()[leg] == 0);
              }
              return static_cast<ldouble>(reference.evaluate(physical_momenta, reference_helicities));
            });
        }, comparison, progress);
      result.rows.push_back(row);
    }
    result.passed = !result.rows.empty();
    bool errors = false;
    for (const auto& row : result.rows) {
      result.passed = result.passed && row.report.passed;
      errors = errors || row.report.status == "ERROR";
    }
    result.status = result.passed ? "PASS" : (errors ? "ERROR" : "FAIL");
    result.message = result.passed ? "Every requested massless-helicity comparison agrees."
      : "At least one requested helicity comparison did not pass; inspect its first failing point.";
  } catch (const std::exception& error) {
    result.status = "ERROR";
    result.message = error.what();
  }
#else
  (void)process; (void)options; (void)progress;
  result.status = "UNAVAILABLE";
  result.message = "This MadGraph validation bridge currently requires macOS or Linux.";
#endif
  return result;
}

inline void print_madgraph_validation_report(std::ostream& out, const MadGraphValidationReport& report) {
  out << "MadGraph helicity validation: " << report.status << '\n'
      << "Seed: " << report.seed << '\n' << report.message << '\n';
  for (const auto& row : report.rows) {
    out << "  Helicity";
    for (int h : row.helicities) out << ' ' << h;
    out << ": " << row.report.status << "; " << row.report.compared_points
        << '/' << row.report.requested_points << " points; max relative error "
        << row.report.maximum_relative_error << "; max absolute error "
        << row.report.maximum_absolute_error << "; mismatches " << row.report.mismatches
        << "; evaluation errors " << row.report.evaluation_errors;
    if (row.expected_exact_zero)
      out << "; symbolic zero, absolute tolerance " << row.zero_absolute_tolerance;
    out << '\n';
    if (row.report.has_first_failure) {
      const auto& failure = row.report.first_failure;
      out << "    First failing point " << failure.point_index << ": " << failure.message << '\n';
      for (std::size_t leg = 0; leg < failure.momenta.size(); ++leg) {
        out << "    p" << leg + 1 << " =";
        for (auto component : failure.momenta[leg]) out << ' ' << component;
        out << '\n';
      }
    }
  }
}

inline void write_madgraph_validation_report_json(
    std::ostream& out, const MadGraphValidationReport& report) {
  using madgraph_detail::json_number;
  using madgraph_detail::json_string;
  out << "{\"Status\":" << json_string(report.status) << ",\"Passed\":"
      << (report.passed ? "true" : "false") << ",\"Seed\":" << report.seed
      << ",\"Message\":" << json_string(report.message) << ",\"Rows\":[";
  for (std::size_t index = 0; index < report.rows.size(); ++index) {
    if (index) out << ',';
    const auto& row = report.rows[index];
    const auto& value = row.report;
    out << "{\"Helicities\":[";
    for (std::size_t leg = 0; leg < row.helicities.size(); ++leg) {
      if (leg) out << ',';
      out << row.helicities[leg];
    }
    out << "],\"ExpectedExactZero\":" << (row.expected_exact_zero ? "true" : "false");
    if (row.expected_exact_zero) {
      out << ",\"ZeroAbsoluteTolerance\":";
      json_number(out, row.zero_absolute_tolerance);
    }
    out << ",\"Status\":" << json_string(value.status) << ",\"Passed\":"
        << (value.passed ? "true" : "false") << ",\"Points\":" << value.compared_points
        << ",\"RequestedPoints\":" << value.requested_points
        << ",\"GeneratedPoints\":" << value.generated_points
        << ",\"ComparedPoints\":" << value.compared_points
        << ",\"Mismatches\":" << value.mismatches
        << ",\"EvaluationErrors\":" << value.evaluation_errors
        << ",\"MaximumRelativeError\":";
    json_number(out, value.maximum_relative_error);
    out << ",\"MaximumAbsoluteError\":"; json_number(out, value.maximum_absolute_error);
    out << ",\"MinimumCenterOfMassEnergy\":"; json_number(out, value.minimum_center_of_mass_energy);
    out << ",\"MaximumCenterOfMassEnergy\":"; json_number(out, value.maximum_center_of_mass_energy);
    out << ",\"Message\":" << json_string(value.message) << ",\"FirstFailure\":";
    if (!value.has_first_failure) out << "null";
    else {
      const auto& failure = value.first_failure;
      out << "{\"Point\":" << failure.point_index << ",\"Momenta\":[";
      for (std::size_t leg = 0; leg < failure.momenta.size(); ++leg) {
        if (leg) out << ',';
        out << '[';
        for (std::size_t component = 0; component < 4; ++component) {
          if (component) out << ',';
          json_number(out, failure.momenta[leg][component]);
        }
        out << ']';
      }
      out << "],\"HEPCATValue\":"; json_number(out, failure.hepcat_value);
      out << ",\"MadGraphValue\":"; json_number(out, failure.reference_value);
      out << ",\"AbsoluteError\":"; json_number(out, failure.absolute_error);
      out << ",\"RelativeError\":"; json_number(out, failure.relative_error);
      out << ",\"Message\":" << json_string(failure.message) << '}';
    }
    out << '}';
  }
  out << "],\"BuildLog\":" << json_string(report.build_log)
      << ",\"ReferenceBuildJSON\":" << json_string(report.reference_build_json) << "}\n";
}

inline void print_madgraph_validation_report(const MadGraphValidationReport& report, std::ostream& out) {
  print_madgraph_validation_report(out, report);
}

inline void write_madgraph_validation_report_json(
    const MadGraphValidationReport& report, std::ostream& out) {
  write_madgraph_validation_report_json(out, report);
}

inline void write_madgraph_validation_report_json(
    const std::string& path, const MadGraphValidationReport& report) {
  std::ofstream out(path.c_str());
  if (!out) throw std::runtime_error("Could not open the MadGraph validation report file.");
  write_madgraph_validation_report_json(out, report);
  if (!out) throw std::runtime_error("Writing the MadGraph validation report failed.");
}

} // namespace hepcat_spinas
#endif
