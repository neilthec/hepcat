#pragma once

// The CalcHEP interface owns process-global model state. This adapter is for
// the generated validator executable, not for concurrent in-process sessions.
// It leaves the user's installation and working directory untouched.

#include <algorithm>
#include <array>
#include <cerrno>
#include <cmath>
#include <complex>
#include <complex.h>
#include <cstddef>
#include <cstdlib>
#include <cstring>
#include <dirent.h>
#include <fcntl.h>
#include <fstream>
#include <iterator>
#include <limits>
#include <mutex>
#include <sstream>
#include <stdexcept>
#include <string>
#include <sys/stat.h>
#include <unistd.h>
#include <utility>
#include <vector>

// The installed CalcHEP header puts its includes inside extern "C". Load the
// numerical types first so libc++ templates are never parsed with C linkage.
#pragma push_macro("log10")
#include "nType.h"
extern "C" {
#include "dynamic_cs.h"
#include "vp.h"
}
#pragma pop_macro("log10")

namespace hepcat_spinas {

struct CalcHEPParameter {
  std::string name;
  double value;
};

template <class T>
double checked_calchep_parameter(const std::complex<T>& value, const std::string& name) {
  const double real = static_cast<double>(std::real(value));
  if (std::imag(value) != 0 || !std::isfinite(real) ||
      (std::real(value) != 0 && real == 0)) {
    throw std::invalid_argument("CalcHEP parameter '" + name +
      "' must have a finite real value representable at double precision.");
  }
  return real;
}

template <class T, std::size_t N>
void check_calchep_masses(const std::array<T, N>& expected,
                         const std::vector<long double>& actual,
                         const std::array<std::size_t, N>& external_order) {
  if (actual.size() != N) {
    throw std::invalid_argument("CalcHEP and SPINAS have different external particle counts.");
  }
  std::array<bool, N> used = {};
  for (std::size_t leg = 0; leg < N; ++leg) {
    const std::size_t source = external_order[leg];
    if (source >= N || used[source]) {
      throw std::invalid_argument("The CalcHEP external leg order is not a permutation.");
    }
    used[source] = true;
    const long double mass = static_cast<long double>(expected[source]);
    if (!std::isfinite(mass) || !std::isfinite(actual[leg]) || mass < 0 || actual[leg] < 0) {
      throw std::invalid_argument("CalcHEP and SPINAS external masses must be finite and nonnegative.");
    }
    const long double scale = std::max(std::abs(mass), std::abs(actual[leg]));
    if ((mass == 0) != (actual[leg] == 0) || std::abs(mass - actual[leg]) > 1e-10L * scale) {
      throw std::invalid_argument("CalcHEP and SPINAS external masses differ.");
    }
  }
}

template <class T, std::size_t N>
void check_calchep_masses(const std::array<T, N>& expected,
                         const std::vector<long double>& actual) {
  std::array<std::size_t, N> identity;
  for (std::size_t leg = 0; leg < N; ++leg) identity[leg] = leg;
  check_calchep_masses(expected, actual, identity);
}

template <class T, std::size_t N>
std::array<std::array<T, 4>, N> calchep_momenta_in_order(
    const std::array<std::array<T, 4>, N>& momenta,
    const std::array<std::size_t, N>& external_order) {
  std::array<std::array<T, 4>, N> ordered;
  for (std::size_t leg = 0; leg < N; ++leg) {
    if (external_order[leg] >= N) throw std::invalid_argument("Invalid CalcHEP momentum leg.");
    ordered[leg] = momenta[external_order[leg]];
  }
  return ordered;
}

namespace calchep_detail {

inline std::mutex& session_mutex() {
  static std::mutex value;
  return value;
}

inline std::string error_message(const std::string& action) {
  return action + ": " + std::strerror(errno);
}

inline void remove_directory(const std::string& path) {
  DIR* directory = opendir(path.c_str());
  if (!directory) return;
  while (dirent* entry = readdir(directory)) {
    const std::string name(entry->d_name);
    if (name == "." || name == "..") continue;
    const std::string child = path + "/" + name;
    struct stat information;
    if (lstat(child.c_str(), &information)) continue;
    if (S_ISDIR(information.st_mode)) remove_directory(child);
    else unlink(child.c_str());
  }
  closedir(directory);
  rmdir(path.c_str());
}

class Workspace {
 public:
  explicit Workspace(bool keep) : keep_(keep) {
    // CalcHEP's internal shell commands do not quote all workspace paths.
    // A fresh, safe /tmp name also prevents reuse of a different process's ME.
    char pattern[] = "/tmp/hepcat-calchep-XXXXXX";
    char* created = mkdtemp(pattern);
    if (!created) throw std::runtime_error(error_message("Create CalcHEP workspace"));
    path_ = created;
  }

  ~Workspace() {
    if (!keep_) remove_directory(path_);
  }

  const std::string& path() const { return path_; }

  Workspace(const Workspace&) = delete;
  Workspace& operator=(const Workspace&) = delete;

 private:
  std::string path_;
  bool keep_;
};

// A benchmark can request optimized matrix-element compilation without editing
// the installation's FlagsForSh or changing the process environment. CalcHEP's
// newProcess script finds its installation from its own path, so this private
// overlay supplies that script and flags while sharing read-only support files.
inline std::string optimized_compiler_installation(
    const std::string& installation, const std::string& workspace, int level) {
  const std::string directory = workspace + "/optimized-calchep";
  if (mkdir(directory.c_str(), 0700) || mkdir((directory + "/sbin").c_str(), 0700)) {
    throw std::runtime_error(error_message("Create private CalcHEP compiler directory"));
  }
  const char* shared[] = {"bin", "include", "lib", "sbin/make-j"};
  for (const char* name : shared) {
    if (symlink((installation + "/" + name).c_str(), (directory + "/" + name).c_str())) {
      throw std::runtime_error(error_message("Link private CalcHEP compiler support"));
    }
  }
  const char* copied[] = {"FlagsForSh", "sbin/newProcess"};
  for (const char* name : copied) {
    std::ifstream input((installation + "/" + name).c_str(), std::ios::binary);
    std::ofstream output((directory + "/" + name).c_str(), std::ios::binary);
    if (!input || !output) throw std::runtime_error("Cannot copy private CalcHEP compiler configuration.");
    output << input.rdbuf();
    if (!output || input.bad()) throw std::runtime_error("Cannot save private CalcHEP compiler configuration.");
  }
  if (chmod((directory + "/sbin/newProcess").c_str(), 0700)) {
    throw std::runtime_error(error_message("Make private CalcHEP compiler script executable"));
  }
  std::ofstream flags((directory + "/FlagsForSh").c_str(), std::ios::app);
  flags << "\n# HEPCAT call-local matrix-element optimization.\n"
        << "CFLAGS=\"$CFLAGS -O" << level << "\"\nexport CFLAGS\n";
  if (!flags) throw std::runtime_error("Cannot save private CalcHEP optimization flags.");
  return directory;
}

class CompilerInstallation {
 public:
  explicit CompilerInstallation(const std::string& directory)
      : previous_(calchepDir), directory_(directory) {
    if (!directory_.empty()) calchepDir = &directory_[0];
  }
  ~CompilerInstallation() { calchepDir = previous_; }
  CompilerInstallation(const CompilerInstallation&) = delete;
  CompilerInstallation& operator=(const CompilerInstallation&) = delete;
 private:
  char* previous_;
  std::string directory_;
};

class WorkingDirectory {
 public:
  explicit WorkingDirectory(const std::string& path) : descriptor_(open(".", O_RDONLY)) {
    if (descriptor_ < 0) throw std::runtime_error(error_message("Save working directory"));
    if (chdir(path.c_str())) {
      const std::string message = error_message("Enter CalcHEP workspace");
      close(descriptor_);
      descriptor_ = -1;
      throw std::runtime_error(message);
    }
  }

  ~WorkingDirectory() {
    if (descriptor_ >= 0) {
      fchdir(descriptor_);
      close(descriptor_);
    }
  }

  WorkingDirectory(const WorkingDirectory&) = delete;
  WorkingDirectory& operator=(const WorkingDirectory&) = delete;

 private:
  int descriptor_;
};

inline std::string trim_table_field(const std::string& text) {
  const std::size_t begin = text.find_first_not_of(" \t\r\n");
  if (begin == std::string::npos) return "";
  return text.substr(begin, text.find_last_not_of(" \t\r\n") - begin + 1);
}

inline std::vector<std::string> split_table_fields(const std::string& line) {
  std::vector<std::string> fields;
  std::size_t begin = 0;
  for (;;) {
    const std::size_t end = line.find('|', begin);
    fields.push_back(trim_table_field(line.substr(begin,
        end == std::string::npos ? std::string::npos : end - begin)));
    if (end == std::string::npos) break;
    begin = end + 1;
  }
  const std::string trimmed = trim_table_field(line);
  if (!fields.empty() && fields.back().empty() &&
      !trimmed.empty() && trimmed.back() == '|') fields.pop_back();
  return fields;
}

inline void write_compact_model_table(std::ostream& output, const std::string& header,
                                      std::istream& input, const std::string& kind, bool zero_widths = false) {
  const auto headings = split_table_fields(header);
  // External-library tables carry free-form commands/prototypes in one
  // column. A pipe inside such a command must not become a field separator.
  if (kind == "Libraries" || headings.size() < 2) {
    output << header << '\n';
    const std::string remainder((std::istreambuf_iterator<char>(input)), std::istreambuf_iterator<char>());
    output << remainder;
    return;
  }
  std::size_t width_column = headings.size();
  if (zero_widths && kind == "Particles") {
    for (std::size_t index = 0; index < headings.size(); ++index) {
      std::string heading = headings[index];
      std::transform(heading.begin(), heading.end(), heading.begin(),
        [](unsigned char c) { return c >= 'A' && c <= 'Z' ? c - 'A' + 'a' : c; });
      if (heading == "width") width_column = index;
    }
    if (width_column == headings.size())
      throw std::runtime_error("The CalcHEP particle table has no identifiable Width column.");
  }
  std::vector<std::size_t> widths;
  for (const auto& heading : headings) widths.push_back(heading.size());
  std::vector<std::pair<std::string, std::vector<std::string>>> rows;
  std::string line;
  while (std::getline(input, line)) {
    std::vector<std::string> fields;
    const bool separator = line.find_first_not_of(" \t\r=+-|") == std::string::npos;
    if (line.find('|') != std::string::npos && !separator) {
      fields = split_table_fields(line);
      if (fields.size() > headings.size()) {
        throw std::runtime_error("A compact CalcHEP model row has more fields than its header.");
      }
      fields.resize(headings.size());
      if (width_column < fields.size()) fields[width_column] = "0";
      for (std::size_t index = 0; index < fields.size(); ++index) {
        widths[index] = std::max(widths[index], fields[index].size());
      }
    }
    rows.push_back(std::make_pair(line, fields));
  }
  const auto write_row = [&](const std::vector<std::string>& fields) {
    for (std::size_t index = 0; index < fields.size(); ++index) {
      output << fields[index] << std::string(widths[index] + 1 - fields[index].size(), ' ') << '|';
    }
    output << '\n';
  };
  write_row(headings);
  for (const auto& row : rows) {
    if (row.second.empty()) output << row.first << '\n';
    else write_row(row.second);
  }
}

inline void copy_model_table(const std::string& source, const std::string& destination,
                             const std::string& kind, bool optional, bool zero_widths = false) {
  std::ifstream input(source.c_str(), std::ios::binary);
  if (!input && !optional) {
    throw std::runtime_error("Cannot read CalcHEP model table: " + source);
  }
  std::ofstream output(destination.c_str(), std::ios::binary);
  if (!output) throw std::runtime_error("Cannot write private CalcHEP model table: " + destination);
  if (!input) {
    output << "HEPCAT validation model\nLibraries\n"
           << "External libraries and function prototypes |\n";
  } else {
    std::string title;
    std::string second;
    if (!std::getline(input, title) || !std::getline(input, second)) {
      throw std::runtime_error("CalcHEP model table has fewer than two header lines: " + source);
    }
    output << title << '\n';
    // HEPCAT also reads a compact two-header form. CalcHEP requires a table
    // kind between the title and pipe header. Only that missing line is added;
    // all parameter values, constraints and vertex field texts stay unchanged.
    // Validation alone replaces particle Width fields by zero in this copy.
    // Compact header widths also need to cover their actual row contents.
    if (second.find('|') != std::string::npos) {
      output << kind << '\n';
      write_compact_model_table(output, second, input, kind, zero_widths);
    } else {
      output << second << '\n';
      if (zero_widths && kind == "Particles") {
        std::string heading;
        if (!std::getline(input, heading))
          throw std::runtime_error("The CalcHEP particle table has no column header.");
        write_compact_model_table(output, heading, input, kind, true);
      } else {
        const std::string remainder((std::istreambuf_iterator<char>(input)), std::istreambuf_iterator<char>());
        output << remainder;
      }
    }
  }
  if (!output) throw std::runtime_error("Could not finish private CalcHEP model table: " + destination);
}

inline std::string stage_model(const std::string& directory, int number,
                               const std::string& workspace) {
  if (number <= 0) throw std::invalid_argument("CalcHEP model number must be positive.");
  char* resolved = realpath(directory.c_str(), NULL);
  if (!resolved) throw std::runtime_error(error_message("Resolve CalcHEP model directory"));
  const std::string source(resolved);
  std::free(resolved);
  const std::string destination = workspace + "/input_model";
  if (mkdir(destination.c_str(), 0700)) {
    throw std::runtime_error(error_message("Create private CalcHEP model directory"));
  }
  const char* prefixes[] = {"vars", "func", "prtcls", "lgrng", "extlib"};
  const char* kinds[] = {"Variables", "Constraints", "Particles", "Lagrangian", "Libraries"};
  for (std::size_t index = 0; index < 5; ++index) {
    const std::string filename = std::string(prefixes[index]) + std::to_string(number) + ".mdl";
    copy_model_table(source + "/" + filename,
                     destination + "/" + std::string(prefixes[index]) + "1.mdl",
                     kinds[index], index == 4, index == 2);
  }
  return destination;
}

// A reference may express a fermion mass as a running constraint while the
// supplied theory declares that same mass as an independent input. Change
// only that numerical mass scheme in the private model. Every downstream
// vertex/Yukawa expression continues to use the same mass parameter name.
inline std::vector<CalcHEPParameter> promote_private_mass_inputs(
    const std::string& staged, const std::vector<CalcHEPParameter>& parameters,
    const std::vector<std::string>& independent_masses) {
  if (independent_masses.empty() || parameters.empty()) return {};
  std::vector<std::string> variables, functions, particles;
  const auto read_lines = [](const std::string& path) {
    std::ifstream input(path.c_str());
    if (!input) throw std::runtime_error("Cannot read a private CalcHEP model table: " + path);
    std::vector<std::string> lines;
    std::string line;
    while (std::getline(input, line)) lines.push_back(line);
    return lines;
  };
  variables = read_lines(staged + "/vars1.mdl");
  functions = read_lines(staged + "/func1.mdl");
  particles = read_lines(staged + "/prtcls1.mdl");
  if (variables.size() < 3 || functions.size() < 3 || particles.size() < 3)
    throw std::runtime_error("A private CalcHEP model table is incomplete.");
  std::vector<std::string> mass_names;
  for (std::size_t row = 3; row < particles.size(); ++row) {
    const auto fields = split_table_fields(particles[row]);
    if (fields.size() > 5) mass_names.push_back(fields[5]);
  }
  std::vector<CalcHEPParameter> promoted;
  for (const auto& parameter : parameters) {
    if (std::find(independent_masses.begin(), independent_masses.end(), parameter.name) == independent_masses.end() ||
        std::find(mass_names.begin(), mass_names.end(), parameter.name) == mass_names.end()) continue;
    bool already_input = false;
    for (std::size_t row = 3; row < variables.size(); ++row) {
      const auto fields = split_table_fields(variables[row]);
      if (fields.size() >= 2 && fields[0] == parameter.name) already_input = true;
    }
    if (already_input) continue;
    std::vector<std::size_t> definitions;
    for (std::size_t row = 3; row < functions.size(); ++row) {
      const auto fields = split_table_fields(functions[row]);
      if (fields.size() >= 2 && fields[0] == parameter.name) definitions.push_back(row);
    }
    if (definitions.empty()) continue;
    if (definitions.size() != 1 || !std::isfinite(parameter.value) || parameter.value < 0)
      throw std::invalid_argument("Ambiguous or invalid matched CalcHEP mass input: " + parameter.name);
    functions.erase(functions.begin() + definitions.front());
    std::ostringstream input_row;
    input_row.precision(std::numeric_limits<double>::max_digits10);
    input_row << parameter.name << " | " << parameter.value
              << " | Matched independent HEPCAT mass; private validation model";
    auto position = variables.end();
    for (auto row = variables.begin() + 3; row != variables.end(); ++row) {
      const std::string trimmed = trim_table_field(*row);
      if (trimmed.size() >= 4 && trimmed.find_first_not_of("=+-|") == std::string::npos) {
        position = row;
        break;
      }
    }
    variables.insert(position, input_row.str());
    promoted.push_back(parameter);
  }
  if (promoted.empty()) return promoted;
  const auto write_lines = [](const std::string& path, const std::vector<std::string>& lines,
                              const std::string& kind) {
    std::ofstream output(path.c_str(), std::ios::trunc);
    if (!output) throw std::runtime_error("Cannot write a private CalcHEP mass scheme: " + path);
    output << lines[0] << '\n' << lines[1] << '\n';
    std::ostringstream body;
    for (std::size_t row = 3; row < lines.size(); ++row) body << lines[row] << '\n';
    std::istringstream input(body.str());
    write_compact_model_table(output, lines[2], input, kind);
    if (!output) throw std::runtime_error("Could not finish a private CalcHEP mass scheme: " + path);
  };
  write_lines(staged + "/vars1.mdl", variables, "Variables");
  write_lines(staged + "/func1.mdl", functions, "Constraints");
  return promoted;
}

inline bool safe_particle_name(const std::string& name) {
  if (name.empty()) return false;
  for (const char character : name) {
    const unsigned char value = static_cast<unsigned char>(character);
    if (!((value >= 'a' && value <= 'z') || (value >= 'A' && value <= 'Z') ||
          (value >= '0' && value <= '9') || character == '_' || character == '+' ||
          character == '-' || character == '~' || character == '.')) return false;
  }
  return true;
}

inline bool parameter_agrees(double actual, double expected) {
  if (!std::isfinite(actual) || !std::isfinite(expected)) return false;
  if (actual == expected) return true;
  return std::abs(actual - expected) <=
      1e-10 * std::max(std::abs(actual), std::abs(expected));
}

} // namespace calchep_detail

class CalcHEPProcess {
 public:
  // Physical ordering is two incoming legs followed by all outgoing legs.
  // sqme already includes initial spin/color averages and the final identical-
  // particle factorial. Do not multiply those factors a second time.
  // Level 0 preserves installation compiler flags; 1, 2 or 3 appends -Olevel
  // in a private workspace for this process only, useful for timing studies.
  CalcHEPProcess(const std::string& model_directory, int model_number,
                 const std::vector<int>& physical_pdgs,
                 const std::vector<CalcHEPParameter>& parameters,
                 double strong_coupling = 1.0, bool keep_work_files = false,
                 const std::vector<int>& allowed_particle_pdgs = std::vector<int>(),
                 int optimization_level = 0,
                 const std::vector<std::string>& independent_mass_parameters = std::vector<std::string>())
      : lock_(calchep_detail::session_mutex(), std::try_to_lock),
        workspace_(keep_work_files), matrix_element_(NULL), pdgs_(physical_pdgs),
        strong_coupling_(strong_coupling), allowed_pdgs_(allowed_particle_pdgs),
        optimization_level_(optimization_level) {
    if (!lock_.owns_lock()) {
      throw std::runtime_error("Another CalcHEP validation session is active; run validators sequentially.");
    }
    if (optimization_level_ < 0 || optimization_level_ > 3) {
      throw std::invalid_argument("CalcHEP optimization level must be 0 (installation default), 1, 2 or 3.");
    }
    if (pdgs_.size() < 4 || pdgs_.size() > maxNp) {
      throw std::invalid_argument("CalcHEP validation requires two incoming and at least two outgoing legs.");
    }
    if (!std::isfinite(strong_coupling_) || strong_coupling_ < 0) {
      throw std::invalid_argument("The CalcHEP strong coupling must be finite and nonnegative.");
    }
    std::sort(allowed_pdgs_.begin(), allowed_pdgs_.end());
    allowed_pdgs_.erase(std::unique(allowed_pdgs_.begin(), allowed_pdgs_.end()), allowed_pdgs_.end());
    for (const int allowed : allowed_pdgs_) {
      if (allowed <= 0) {
        throw std::invalid_argument("Allowed CalcHEP particle types must be positive absolute PDG integers.");
      }
    }
    if (!allowed_pdgs_.empty()) {
      for (const int pdg : pdgs_) {
        if (pdg == std::numeric_limits<int>::min() ||
            !std::binary_search(allowed_pdgs_.begin(), allowed_pdgs_.end(), std::abs(pdg))) {
          throw std::invalid_argument("The CalcHEP particle restriction excludes an external particle type.");
        }
      }
    }
    const std::string staged = calchep_detail::stage_model(
        model_directory, model_number, workspace_.path());
    promoted_mass_parameters_ = calchep_detail::promote_private_mass_inputs(
        staged, parameters, independent_mass_parameters);
    calchep_detail::WorkingDirectory working_directory(workspace_.path());
    try {
      initialize(staged, parameters);
    } catch (...) {
      delAllLib();
      matrix_element_ = NULL;
      throw;
    }
  }

  ~CalcHEPProcess() {
    if (matrix_element_) delAllLib();
  }

  CalcHEPProcess(const CalcHEPProcess&) = delete;
  CalcHEPProcess& operator=(const CalcHEPProcess&) = delete;

  const std::vector<long double>& masses() const { return masses_; }
  const std::vector<int>& particle_pdgs() const { return pdgs_; }
  const std::string& process_name() const { return process_; }
  const std::string& work_directory() const { return workspace_.path(); }
  const std::vector<int>& allowed_particle_pdgs() const { return allowed_pdgs_; }
  const std::vector<std::string>& excluded_particle_names() const { return excluded_names_; }
  const std::vector<CalcHEPParameter>& promoted_mass_parameters() const { return promoted_mass_parameters_; }
  std::string mass_scheme_summary() const {
    if (promoted_mass_parameters_.empty()) return "";
    std::ostringstream summary;
    summary.precision(std::numeric_limits<double>::max_digits10);
    summary << "Private CalcHEP independent mass inputs:";
    for (const auto& parameter : promoted_mass_parameters_)
      summary << ' ' << parameter.name << '=' << parameter.value;
    return summary.str();
  }

  template <typename Real, std::size_t Size>
  double squared_amplitude(const std::array<std::array<Real, 4>, Size>& momenta) {
    if (Size != pdgs_.size()) {
      throw std::invalid_argument("Phase-space multiplicity does not match the CalcHEP process.");
    }
    std::vector<REAL> buffer(4 * Size);
    for (std::size_t leg = 0; leg < Size; ++leg) {
      // CalcHEP may sort its external states when generating the process.
      // Keep the caller's physical ordering and translate only at this boundary.
      const std::size_t physical_leg = reference_order_[leg];
      for (std::size_t component = 0; component < 4; ++component) {
        if (!std::isfinite(momenta[physical_leg][component])) {
          throw std::invalid_argument("A CalcHEP phase-space component is not finite.");
        }
        buffer[4 * leg + component] = static_cast<REAL>(momenta[physical_leg][component]);
      }
    }
    int error = 0;
    FError = 0;
    const double value = matrix_element_->interface->sqme(
        1, strong_coupling_, buffer.data(), NULL, &error);
    if (error || FError || !std::isfinite(value)) {
      std::ostringstream message;
      message << "CalcHEP squared amplitude failed (error " << error
              << ", FError " << FError << ").";
      throw std::runtime_error(message.str());
    }
    return value;
  }

 private:
  void initialize(const std::string& directory,
                   const std::vector<CalcHEPParameter>& parameters) {
    std::vector<char> model_path(directory.begin(), directory.end());
    model_path.push_back('\0');
    if (setModel(model_path.data(), 1)) {
      throw std::runtime_error("CalcHEP could not load the staged model in " + workspace_.path());
    }
    std::vector<CalcHEPParameter> constrained;
    std::vector<CalcHEPParameter> process_constraints;
    for (const auto& parameter : parameters) {
      if (!std::isfinite(parameter.value)) {
        throw std::invalid_argument("A CalcHEP model parameter is not finite: " + parameter.name);
      }
      if (parameter.name == "GG" &&
          !calchep_detail::parameter_agrees(parameter.value, strong_coupling_)) {
        throw std::invalid_argument("CalcHEP's external strong coupling disagrees with the supplied GG parameter.");
      }
      if (assignVal(parameter.name.c_str(), parameter.value)) {
        double value;
        if (findVal(parameter.name.c_str(), &value)) {
          // GG is also the documented external argument of sqme. Some models
          // use that formal coupling without declaring a model-variable row.
          if (parameter.name == "GG") continue;
          // Local dependent parameters are intentionally absent from findVal.
          // A used local constraint becomes available only in the generated ME.
          process_constraints.push_back(parameter);
          continue;
        }
        constrained.push_back(parameter);
      }
    }
    if (calcMainFunc()) throw std::runtime_error("CalcHEP model constraints could not be calculated.");
    for (const auto& parameter : constrained) {
      double value;
      if (findVal(parameter.name.c_str(), &value) || !std::isfinite(value) ||
          !calchep_detail::parameter_agrees(value, parameter.value)) {
        throw std::invalid_argument("A CalcHEP constrained parameter disagrees with HEPCAT: " + parameter.name);
      }
    }
    std::vector<std::string> names;
    for (const int pdg : pdgs_) {
      const char* name = pdg2name(pdg);
      if (!name || !calchep_detail::safe_particle_name(name)) {
        throw std::invalid_argument("The CalcHEP model has no supported particle name for PDG " +
                                    std::to_string(pdg));
      }
      names.push_back(name);
    }
    process_ = names[0] + "," + names[1] + "->";
    for (std::size_t index = 2; index < names.size(); ++index) {
      if (index > 2) process_ += ",";
      process_ += names[index];
    }
    std::vector<char> process(process_.begin(), process_.end());
    process.push_back('\0');
    char library[] = "hepcat_validation";
    build_particle_restriction();
    std::string exclusion;
    for (const auto& particle : excluded_names_) {
      if (!exclusion.empty()) exclusion += ",";
      exclusion += particle;
    }
    std::vector<char> excluded(exclusion.begin(), exclusion.end());
    excluded.push_back('\0');
    char excluded_output[] = "";
    // The fourth argument filters virtual particles. The fifth instead filters
    // unspecified outgoing states and must not be used for an internal cut.
    // Unitary gauge; do not add t-channel Breit-Wigner factors.
    {
      const std::string compiler_directory = optimization_level_ == 0 ? std::string() :
          calchep_detail::optimized_compiler_installation(
              calchepDir, workspace_.path(), optimization_level_);
      calchep_detail::CompilerInstallation compiler(compiler_directory);
      matrix_element_ = getMEcode(0, 1, process.data(),
          excluded_names_.empty() ? NULL : excluded.data(), excluded_output, library);
    }
    if (!matrix_element_) {
      throw std::runtime_error("CalcHEP could not generate " + process_ + " in " + workspace_.path());
    }
    // passParameters has a fatal legacy path for absent links. Diagnose it
    // before calling the public routine so the generated validator can fail.
    for (int index = 1; index <= matrix_element_->interface->nvar; ++index) {
      if (!matrix_element_->link[index] ||
          matrix_element_->link[index] - varValues >= *currentVarPtr) {
        throw std::runtime_error("CalcHEP could not resolve process parameter " +
                                 std::string(matrix_element_->interface->varName[index]));
      }
    }
    if (passParameters(matrix_element_)) {
      throw std::runtime_error("CalcHEP could not transfer the model parameters to the process.");
    }
    for (const auto& parameter : process_constraints) {
      bool found = false;
      for (int index = 1; index <= matrix_element_->interface->nvar +
                                   matrix_element_->interface->nfunc; ++index) {
        if (parameter.name != matrix_element_->interface->varName[index]) continue;
        found = true;
        const double value = static_cast<double>(matrix_element_->interface->va[index]);
        if (!calchep_detail::parameter_agrees(value, parameter.value)) {
          throw std::invalid_argument("A CalcHEP local constrained parameter disagrees with HEPCAT: " +
                                      parameter.name);
        }
        break;
      }
      if (!found) {
        throw std::invalid_argument("CalcHEP does not expose a used model parameter: " + parameter.name);
      }
    }
    if (matrix_element_->interface->nin != 2 ||
        matrix_element_->interface->nout + 2 != static_cast<int>(pdgs_.size()) ||
        matrix_element_->interface->nprc != 1) {
      throw std::runtime_error("CalcHEP generated an unexpected subprocess or multiplicity.");
    }
    reference_order_.resize(pdgs_.size());
    masses_.resize(pdgs_.size());
    std::vector<bool> assigned(pdgs_.size(), false);
    for (std::size_t index = 0; index < pdgs_.size(); ++index) {
      REAL mass;
      int pdg;
      matrix_element_->interface->pinf(1, static_cast<int>(index + 1), &mass, &pdg);
      if (!std::isfinite(mass) || mass < 0) {
        throw std::runtime_error("CalcHEP returned an invalid external mass.");
      }
      // Match each native slot to one unused requested slot of the same species
      // and role. Repeated identical particles use stable first-unused matches.
      const std::size_t first = index < 2 ? 0 : 2;
      const std::size_t last = index < 2 ? 2 : pdgs_.size();
      std::size_t physical_leg = first;
      while (physical_leg < last &&
             (assigned[physical_leg] || pdgs_[physical_leg] != pdg)) {
        ++physical_leg;
      }
      if (physical_leg == last) {
        throw std::runtime_error("CalcHEP changed the external particle species or incoming/outgoing roles.");
      }
      assigned[physical_leg] = true;
      reference_order_[index] = physical_leg;
      masses_[physical_leg] = static_cast<long double>(mass);
    }
  }

  void build_particle_restriction() {
    if (allowed_pdgs_.empty()) return; // All particles in the explicitly selected model.
    std::vector<int> known;
    for (int index = 0; index < nModelParticles; ++index) {
      const ModelPrtclsStr& particle = ModelPrtcls[index];
      if (particle.NPDG == std::numeric_limits<int>::min()) {
        throw std::runtime_error("The CalcHEP model has an invalid particle PDG code.");
      }
      const int pdg = std::abs(particle.NPDG);
      // PDG-zero auxiliary rows implement vertices and are not physical types.
      if (pdg == 0) continue;
      known.push_back(pdg);
      if (std::binary_search(allowed_pdgs_.begin(), allowed_pdgs_.end(), pdg)) continue;
      const char* names[] = {particle.name, particle.aname};
      for (const char* name : names) {
        if (!name || !calchep_detail::safe_particle_name(name)) {
          throw std::runtime_error("A CalcHEP particle name cannot be used in the requested restriction.");
        }
        excluded_names_.push_back(name);
      }
    }
    for (const int pdg : allowed_pdgs_) {
      if (std::find(known.begin(), known.end(), pdg) == known.end()) {
        throw std::invalid_argument("The CalcHEP reference model lacks allowed PDG type " + std::to_string(pdg));
      }
    }
    std::sort(excluded_names_.begin(), excluded_names_.end());
    excluded_names_.erase(std::unique(excluded_names_.begin(), excluded_names_.end()), excluded_names_.end());
  }

  std::unique_lock<std::mutex> lock_;
  calchep_detail::Workspace workspace_;
  numout* matrix_element_;
  std::vector<int> pdgs_;
  // Native CalcHEP external slot -> caller's physical external slot.
  std::vector<std::size_t> reference_order_;
  std::vector<long double> masses_;
  double strong_coupling_;
  std::string process_;
  std::vector<int> allowed_pdgs_;
  std::vector<std::string> excluded_names_;
  std::vector<CalcHEPParameter> promoted_mass_parameters_;
  int optimization_level_;
};

} // namespace hepcat_spinas
