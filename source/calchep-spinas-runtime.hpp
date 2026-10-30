#pragma once

// CalcHEP's numerical engine with a supplied SPINAS matrix element.
// This adapter never asks CalcHEP to generate or simplify an amplitude.
// One executable owns one model; each integration thread owns its own caches.

#include <algorithm>
#include <array>
#include <cctype>
#include <cmath>
#include <complex>
#include <complex.h>
#include <cstddef>
#include <cstring>
#include <limits>
#include <stdexcept>
#include <string>
#include <vector>

// Respect the installed REAL ABI, including a long-double CalcHEP build.
// nType.h defines log10 as a macro, which must not escape into C++ code.
#pragma push_macro("log10")
#include "nType.h"
extern "C" {
#include "num_out.h"
#include "VandP.h"
}
#pragma pop_macro("log10")

namespace hepcat_calchep {

struct ParameterDefinition {
  const char* name;
  REAL value;
};

struct ParticleDefinition {
  const char* name;
  const char* antiname;
  int pdg;
  int spin2;
  int color;
  int charge3;
  const char* mass;
  const char* width;
  bool self_conjugate;
};

struct LegDefinition {
  const char* name;
  int pdg;
  int spin2;
  int color;
  bool neutral;
  int states;
};

struct DenominatorDefinition {
  // Binary one-based leg indices, terminated by zero, as in den_info_ext.
  const char* legs;
  // Zero-based model-parameter indices; -1 denotes an exactly zero mass/width.
  int mass_parameter;
  int width_parameter;
  // Zero-based ModelPrtcls index; -1 means no particle identification.
  int particle_index;
};

struct Configuration {
  const char* calchep_directory = nullptr;
  int independent_count = 0;
  std::vector<ParameterDefinition> parameters;
  std::vector<ParticleDefinition> particles;
  std::vector<LegDefinition> legs;
  // CalcHEP incoming-first slot -> original generated-process leg.
  std::vector<std::size_t> external_order;
  // Generated Process::Parameters slot -> zero-based model parameter.
  std::vector<int> process_parameter_indices;
  std::vector<DenominatorDefinition> denominators;
  // Legacy single-helicity exports omit the final identical-species factor.
  // Families already include it in amp2 and therefore leave this equal to one.
  REAL amplitude_factor = static_cast<REAL>(1);
  // Exact CalcHEP GG is supplied by its scale/running-alpha_s machinery.
  // Other couplings keep the values selected in the ordinary model menu.
  int running_strong_parameter = -1;
  // Recompute dependent model parameters in their stored topological order.
  // Return zero on success, or the failing zero-based slot plus one.
  int (*evaluate)(REAL* values) = nullptr;
};

template <class Process>
class NumericalProcess {
public:
  explicit NumericalProcess(const Configuration& configuration)
    : config(configuration), interface_values(config.parameters.size() + 1, static_cast<REAL>(0)),
      model_storage(config.parameters.size(), static_cast<REAL>(0)),
      width_range(2.7), t_width(0), gauge_t_width(0), gauge_s_width(0),
      automatic_width(nullptr) {
    if (active() != nullptr) {
      throw std::logic_error("A numerical CalcHEP executable supports one SPINAS process.");
    }
    validate_configuration();
    interface_names.push_back(const_cast<char*>("P(cms)"));
    for (std::size_t i = 0; i < config.parameters.size(); ++i) {
      model_name_storage.push_back(const_cast<char*>(config.parameters[i].name));
      interface_names.push_back(const_cast<char*>(config.parameters[i].name));
      model_storage[i] = config.parameters[i].value;
      interface_values[i + 1] = config.parameters[i].value;
    }
    // The installed main-model diagnostic indexes varNames by a positive
    // error code. Keep its final error index valid without adding a parameter.
    model_name_storage.push_back(const_cast<char*>("InvalidInput"));
    if (model_storage.empty()) model_name_storage.push_back(const_cast<char*>("InvalidInput"));
    for (const auto& particle : config.particles) {
      const int spin_states = particle.spin2 > 0 && std::strcmp(particle.mass, "0") == 0
        ? 2 : particle.spin2 + 1;
      ModelPrtclsStr row = {
        const_cast<char*>(particle.name), const_cast<char*>(particle.antiname),
        particle.self_conjugate ? 1 : 0, particle.pdg,
        const_cast<char*>(particle.mass), const_cast<char*>(particle.width),
        particle.spin2, particle.color, spin_states * std::abs(particle.color), particle.charge3};
      particle_storage.push_back(row);
    }
    polarized_storage[0] = const_cast<char*>("");
    polarized_storage[1] = const_cast<char*>("");
    polarized_storage[2] = const_cast<char*>("");
    color_basis.pow = 0;
    color_basis.nC = 0;
    color_basis.chains = nullptr;
    active() = this;
    if (calculate_model() != 0) {
      active() = nullptr;
      throw std::invalid_argument("Invalid numerical model defaults: " + last_error());
    }
  }

  NumericalProcess(const NumericalProcess&) = delete;
  NumericalProcess& operator=(const NumericalProcess&) = delete;
  ~NumericalProcess() { if (active() == this) active() = nullptr; }

  int independent_count() const { return config.independent_count; }
  int dependent_count() const { return static_cast<int>(config.parameters.size()) - config.independent_count; }
  int model_count() const { return static_cast<int>(config.parameters.size()); }
  int particle_count() const { return static_cast<int>(particle_storage.size()); }
  char** model_names() { return model_name_storage.data(); }
  REAL* model_values() { return model_storage.data(); }
  ModelPrtclsStr* model_particles() { return particle_storage.data(); }
  const std::string& error() const { return last_error(); }

  // Called by CalcHEP's normal parameter menu/session update path.
  int calculate_model() noexcept {
    try {
      for (std::size_t i = 0; i < model_storage.size(); ++i) {
        if (i < static_cast<std::size_t>(config.independent_count) && !finite(model_storage[i])) {
          return model_error(i, "Independent parameter is not finite.");
        }
      }
      if (config.evaluate != nullptr) {
        const int status = config.evaluate(model_storage.data());
        if (status != 0) {
          last_error() = "A dependent model parameter could not be evaluated.";
          return status > 0 && status <= model_count() ? status : 1;
        }
      }
      for (std::size_t i = 0; i < model_storage.size(); ++i) {
        if (!finite(model_storage[i])) return model_error(i, "Model parameter is not finite.");
      }
      typename Process::Parameters values = parameters_from(model_storage.data());
      // Validate masses and model constraints before changing integration inputs.
      Process checked(values);
      external_mass_storage = checked.masses();
      for (std::size_t i = 0; i < model_storage.size(); ++i) interface_values[i + 1] = model_storage[i];
      last_error().clear();
      return 0;
    } catch (const std::exception& exception) {
      last_error() = exception.what();
      return 1;
    } catch (...) {
      last_error() = "Unknown error while updating the SPINAS model.";
      return 1;
    }
  }

  // All slots are exposed in the process interface, so CalcHEP's name-based
  // export copies both independent and evaluated dependent model values.
  CalcHEP_interface process_interface() {
    CalcHEP_interface result = {};
    result.forceUG = 1;
    result.CALCHEP = const_cast<char*>(config.calchep_directory);
    result.nvar = model_count();
    result.nfunc = 0;
    result.varName = interface_names.data();
    result.va = interface_values.data();
    result.nin = 2;
    result.nout = static_cast<int>(Process::leg_count) - 2;
    result.nprc = 1;
    result.pinf = &particle_info;
    result.pinfAux = &particle_aux;
    // No adjustable polarization menu is advertised. The supplied amplitude
    // family already determines which incoming/outgoing helicities are summed.
    result.polarized = polarized_storage;
    result.calcFunc = &calculate_interface;
    result.BWrange = &width_range;
    result.twidth = &t_width;
    result.gtwidth = &gauge_t_width;
    result.gswidth = &gauge_s_width;
    result.aWidth = &automatic_width;
    result.sqme = &squared_amplitude;
    result.den_info = &denominator_info;
    result.cb = &color_basis;
    return result;
  }

  int quantum_numbers(const char* name, int* spin2, int* charge3, int* color) const noexcept {
    if (name == nullptr) return 0;
    for (const auto& particle : config.particles) {
      int sign = 0;
      if (std::strcmp(name, particle.name) == 0) sign = 1;
      else if (std::strcmp(name, particle.antiname) == 0) sign = -1;
      if (sign == 0) continue;
      if (spin2 != nullptr) *spin2 = particle.spin2;
      if (charge3 != nullptr) *charge3 = sign * particle.charge3;
      if (color != nullptr) *color = sign < 0 && std::abs(particle.color) == 3
        ? -particle.color : particle.color;
      return sign * particle.pdg;
    }
    return 0;
  }

private:
  Configuration config;
  std::vector<REAL> interface_values;
  std::vector<REAL> model_storage;
  std::array<ldouble, Process::leg_count> external_mass_storage;
  std::vector<char*> interface_names;
  std::vector<char*> model_name_storage;
  std::vector<ModelPrtclsStr> particle_storage;
  char* polarized_storage[3];
  colorBasis color_basis;
  double width_range;
  int t_width;
  int gauge_t_width;
  int gauge_s_width;
  double (*automatic_width)(char*);

  static NumericalProcess*& active() {
    static NumericalProcess* instance = nullptr;
    return instance;
  }

  static std::string& last_error() {
    static thread_local std::string message;
    return message;
  }

  static bool finite(REAL value) { return std::isfinite(static_cast<long double>(value)); }

  static bool safe_name(const char* name) {
    if (name == nullptr || name[0] == 0 || std::strlen(name) > 11) return false;
    for (const unsigned char* c = reinterpret_cast<const unsigned char*>(name); *c; ++c) {
      if (std::isspace(*c) || std::iscntrl(*c) || *c == ',' || *c == '{' || *c == '}') return false;
    }
    return true;
  }

  int model_error(std::size_t index, const char* message) {
    last_error() = std::string(message) + " Parameter: " + config.parameters[index].name;
    return static_cast<int>(index) + 1;
  }

  void validate_configuration() const {
    if (config.calchep_directory == nullptr || config.calchep_directory[0] == 0 ||
        config.independent_count < 0 || config.independent_count > static_cast<int>(config.parameters.size()) ||
        config.legs.size() != Process::leg_count ||
        Process::leg_count < 4 || Process::leg_count > maxNp ||
        config.external_order.size() != Process::leg_count ||
        config.process_parameter_indices.size() != Process::parameter_count) {
      throw std::invalid_argument("Incomplete CalcHEP numerical-process metadata.");
    }
    if (!finite(config.amplitude_factor) || config.amplitude_factor <= 0) {
      throw std::invalid_argument("The numerical amplitude normalization must be finite and positive.");
    }
    if (config.running_strong_parameter < -1 || config.running_strong_parameter >= model_count()) {
      throw std::invalid_argument("Invalid running strong-coupling parameter binding.");
    }
    std::vector<std::size_t> order = config.external_order;
    std::sort(order.begin(), order.end());
    for (std::size_t i = 0; i < order.size(); ++i) {
      if (order[i] != i) throw std::invalid_argument("CalcHEP external order is not a permutation.");
    }
    const auto incoming = Process::incoming_legs();
    if (!((config.external_order[0] == incoming[0] && config.external_order[1] == incoming[1]) ||
          (config.external_order[0] == incoming[1] && config.external_order[1] == incoming[0]))) {
      throw std::invalid_argument("CalcHEP's first two external slots must be the incoming particles.");
    }
    for (std::size_t i = 0; i < config.parameters.size(); ++i) {
      const char* name = config.parameters[i].name;
      if (!safe_name(name)) throw std::invalid_argument("CalcHEP numerical names must be nonempty, at most 11 characters and contain no separators.");
      for (std::size_t j = 0; j < i; ++j) {
        if (std::strcmp(name, config.parameters[j].name) == 0) {
          throw std::invalid_argument("Numerical model parameter names must be unique.");
        }
      }
    }
    for (const int index : config.process_parameter_indices) {
      if (index < 0 || index >= static_cast<int>(config.parameters.size())) {
        throw std::invalid_argument("A SPINAS parameter has no numerical model binding.");
      }
    }
    for (const auto& particle : config.particles) {
      if (!safe_name(particle.name) || !safe_name(particle.antiname) || particle.mass == nullptr ||
          particle.width == nullptr || particle.width[0] == '!') {
        throw std::invalid_argument("Automatic widths require separate decay amplitudes and are not supported.");
      }
    }
    for (const auto& leg : config.legs) {
      if (!safe_name(leg.name) || leg.states <= 0 || std::abs(leg.color) != 1) {
        throw std::invalid_argument("Invalid external particle metadata for colorless SPINAS integration.");
      }
    }
    for (const auto& denominator : config.denominators) {
      if (denominator.legs == nullptr || denominator.legs[0] == 0 ||
          denominator.mass_parameter < -1 || denominator.width_parameter < -1 ||
          denominator.mass_parameter >= model_count() || denominator.width_parameter >= model_count() ||
          denominator.particle_index < -1 || denominator.particle_index >= static_cast<int>(config.particles.size())) {
        throw std::invalid_argument("Invalid integration denominator metadata.");
      }
      for (const unsigned char* leg = reinterpret_cast<const unsigned char*>(denominator.legs); *leg; ++leg) {
        if (*leg > Process::leg_count) throw std::invalid_argument("Invalid denominator momentum index.");
      }
    }
  }

  typename Process::Parameters parameters_from(const REAL* values) const {
    typename Process::Parameters parameters;
    for (std::size_t i = 0; i < parameters.size(); ++i) {
      const REAL value = values[config.process_parameter_indices[i]];
      if (!finite(value)) throw std::invalid_argument("A bound SPINAS parameter is not finite.");
      parameters[i] = typename Process::Parameters::value_type(static_cast<ldouble>(value), 0);
    }
    return parameters;
  }

  Process& thread_process(const double* strong_coupling = nullptr) const {
    struct Worker {
      typename Process::Parameters parameters;
      Process process;
      bool initialized;
      Worker() : process(), initialized(false) {}
    };
    static thread_local Worker worker;
    typename Process::Parameters parameters;
    if (strong_coupling != nullptr && config.running_strong_parameter >= 0) {
      if (!std::isfinite(*strong_coupling) || *strong_coupling < 0) {
        throw std::invalid_argument("CalcHEP supplied an invalid running strong coupling.");
      }
      std::vector<REAL> local(interface_values.begin() + 1, interface_values.end());
      local[static_cast<std::size_t>(config.running_strong_parameter)] = static_cast<REAL>(*strong_coupling);
      if (config.evaluate != nullptr && config.evaluate(local.data()) != 0) {
        throw std::invalid_argument("A running-coupling dependent parameter could not be evaluated.");
      }
      parameters = parameters_from(local.data());
    } else {
      parameters = parameters_from(interface_values.data() + 1);
    }
    if (!worker.initialized || worker.parameters != parameters) {
      worker.process.set_parameters(parameters);
      worker.parameters = parameters;
      worker.initialized = true;
    }
    if (strong_coupling != nullptr && config.running_strong_parameter >= 0) {
      if (worker.process.masses() != external_mass_storage) {
        throw std::invalid_argument("Running strong coupling must not change the external phase-space masses.");
      }
    }
    return worker.process;
  }

  static int calculate_interface() noexcept {
    NumericalProcess* self = active();
    if (self == nullptr) return 1;
    for (int i = 0; i < self->config.independent_count; ++i) {
      self->model_storage[static_cast<std::size_t>(i)] = self->interface_values[static_cast<std::size_t>(i) + 1];
    }
    return self->calculate_model();
  }

  static char* particle_info(int subprocess, int particle, REAL* mass, int* pdg) noexcept {
    NumericalProcess* self = active();
    if (self == nullptr || subprocess != 1 || particle < 1 || particle > static_cast<int>(Process::leg_count)) {
      if (mass != nullptr) *mass = std::numeric_limits<REAL>::quiet_NaN();
      if (pdg != nullptr) *pdg = 0;
      return nullptr;
    }
    const std::size_t index = static_cast<std::size_t>(particle - 1);
    if (pdg != nullptr) *pdg = self->config.legs[index].pdg;
    if (mass != nullptr) {
      try {
        *mass = static_cast<REAL>(self->thread_process().masses()[self->config.external_order[index]]);
      } catch (const std::exception& exception) {
        last_error() = exception.what();
        *mass = std::numeric_limits<REAL>::quiet_NaN();
      } catch (...) {
        last_error() = "Unknown SPINAS mass update failure.";
        *mass = std::numeric_limits<REAL>::quiet_NaN();
      }
    }
    return const_cast<char*>(self->config.legs[index].name);
  }

  static int particle_aux(int subprocess, int particle, int* spin2, int* color,
                          int* neutral, int* states) noexcept {
    NumericalProcess* self = active();
    if (self == nullptr || subprocess != 1 || particle < 1 || particle > static_cast<int>(Process::leg_count)) return 0;
    const auto& leg = self->config.legs[static_cast<std::size_t>(particle - 1)];
    if (spin2 != nullptr) *spin2 = leg.spin2;
    if (color != nullptr) *color = leg.color;
    if (neutral != nullptr) *neutral = leg.neutral ? 1 : 0;
    if (states != nullptr) *states = leg.states;
    return leg.pdg;
  }

  static char* denominator_info(int subprocess, int number, int* mass, int* width, int* particle) noexcept {
    if (mass != nullptr) *mass = 0;
    if (width != nullptr) *width = 0;
    if (particle != nullptr) *particle = -1;
    NumericalProcess* self = active();
    if (self == nullptr || subprocess != 1 || number < 1 || number > static_cast<int>(self->config.denominators.size())) return nullptr;
    const auto& denominator = self->config.denominators[static_cast<std::size_t>(number - 1)];
    if (mass != nullptr) *mass = denominator.mass_parameter + 1;
    if (width != nullptr) *width = denominator.width_parameter + 1;
    if (particle != nullptr) *particle = denominator.particle_index;
    return const_cast<char*>(denominator.legs);
  }

  static double squared_amplitude(int subprocess, double strong_coupling, REAL* momenta,
                                  REAL* color_coefficients, int* error) noexcept {
    if (error != nullptr) *error = 1;
    try {
      NumericalProcess* self = active();
      if (self == nullptr || subprocess != 1 || momenta == nullptr) {
        throw std::invalid_argument("Invalid SPINAS numerical matrix-element request.");
      }
      if (self->t_width != 0 || self->gauge_t_width != 0 || self->gauge_s_width != 0) {
        throw std::invalid_argument("CalcHEP width switches cannot change the supplied SPINAS formula; use its explicit width parameters.");
      }
      if (color_coefficients != nullptr) {
        throw std::invalid_argument("Color-flow coefficients are unavailable for this colorless export.");
      }
      typename Process::Momenta point;
      for (std::size_t slot = 0; slot < Process::leg_count; ++slot) {
        for (std::size_t component = 0; component < 4; ++component) {
          const REAL value = momenta[4 * slot + component];
          if (!finite(value)) throw std::invalid_argument("A numerical momentum is not finite.");
          point[self->config.external_order[slot]][component] = static_cast<ldouble>(value);
        }
      }
      Process& process = self->thread_process(&strong_coupling);
      process.set_momenta(point);
      // amp2 already includes the incoming average and identical-particle factor.
      // CalcHEP supplies the flux, phase-space measure, cuts and optional PDFs.
      const long double value = static_cast<long double>(process.amp2()) *
        static_cast<long double>(self->config.amplitude_factor);
      const double result = static_cast<double>(value);
      if (!std::isfinite(value) || value < 0 || !std::isfinite(result) || (value != 0 && result == 0)) {
        throw std::runtime_error("The squared amplitude is not a finite nonnegative CalcHEP double.");
      }
      if (error != nullptr) *error = 0;
      last_error().clear();
      return result;
    } catch (const std::exception& exception) {
      last_error() = exception.what();
    } catch (...) {
      last_error() = "Unknown SPINAS matrix-element failure.";
    }
    return std::numeric_limits<double>::quiet_NaN();
  }
};

} // namespace hepcat_calchep
