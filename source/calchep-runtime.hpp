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
                                      std::istream& input, const std::string& kind) {
  const auto headings = split_table_fields(header);
  // External-library tables carry free-form commands/prototypes in one
  // column. A pipe inside such a command must not become a field separator.
  if (kind == "Libraries" || headings.size() < 2) {
    output << header << '\n';
    const std::string remainder((std::istreambuf_iterator<char>(input)), std::istreambuf_iterator<char>());
    output << remainder;
    return;
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
                             const std::string& kind, bool optional) {
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
    // Compact header widths also need to cover their actual row contents.
    if (second.find('|') != std::string::npos) {
      output << kind << '\n';
      write_compact_model_table(output, second, input, kind);
    } else {
      output << second << '\n';
      const std::string remainder((std::istreambuf_iterator<char>(input)), std::istreambuf_iterator<char>());
      output << remainder;
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
                     kinds[index], index == 4);
  }
  return destination;
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
  CalcHEPProcess(const std::string& model_directory, int model_number,
                 const std::vector<int>& physical_pdgs,
                 const std::vector<CalcHEPParameter>& parameters,
                 double strong_coupling = 1.0, bool keep_work_files = false)
      : lock_(calchep_detail::session_mutex(), std::try_to_lock),
        workspace_(keep_work_files), matrix_element_(NULL), pdgs_(physical_pdgs),
        strong_coupling_(strong_coupling) {
    if (!lock_.owns_lock()) {
      throw std::runtime_error("Another CalcHEP validation session is active; run validators sequentially.");
    }
    if (pdgs_.size() < 4 || pdgs_.size() > maxNp) {
      throw std::invalid_argument("CalcHEP validation requires two incoming and at least two outgoing legs.");
    }
    if (!std::isfinite(strong_coupling_) || strong_coupling_ < 0) {
      throw std::invalid_argument("The CalcHEP strong coupling must be finite and nonnegative.");
    }
    const std::string staged = calchep_detail::stage_model(
        model_directory, model_number, workspace_.path());
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

  template <typename Real, std::size_t Size>
  double squared_amplitude(const std::array<std::array<Real, 4>, Size>& momenta) {
    if (Size != pdgs_.size()) {
      throw std::invalid_argument("Phase-space multiplicity does not match the CalcHEP process.");
    }
    std::vector<REAL> buffer(4 * Size);
    for (std::size_t leg = 0; leg < Size; ++leg) {
      for (std::size_t component = 0; component < 4; ++component) {
        if (!std::isfinite(momenta[leg][component])) {
          throw std::invalid_argument("A CalcHEP phase-space component is not finite.");
        }
        buffer[4 * leg + component] = static_cast<REAL>(momenta[leg][component]);
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
    char excluded[] = "";
    // Unitary gauge; do not add t-channel Breit-Wigner factors.
    matrix_element_ = getMEcode(0, 1, process.data(), NULL, excluded, library);
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
    for (std::size_t index = 0; index < pdgs_.size(); ++index) {
      REAL mass;
      int pdg;
      matrix_element_->interface->pinf(1, static_cast<int>(index + 1), &mass, &pdg);
      if (pdg != pdgs_[index]) {
        throw std::runtime_error("CalcHEP changed the external particle ordering.");
      }
      if (!std::isfinite(mass) || mass < 0) {
        throw std::runtime_error("CalcHEP returned an invalid external mass.");
      }
      masses_.push_back(static_cast<long double>(mass));
    }
  }

  std::unique_lock<std::mutex> lock_;
  calchep_detail::Workspace workspace_;
  numout* matrix_element_;
  std::vector<int> pdgs_;
  std::vector<long double> masses_;
  double strong_coupling_;
  std::string process_;
};

} // namespace hepcat_spinas
