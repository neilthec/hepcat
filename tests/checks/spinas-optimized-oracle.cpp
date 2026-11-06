// Compiled by spinas-optimized-tests.wl with its generated project inventory.
#include "optimized-projects.hpp"
#include <algorithm>
#include <functional>
#include <iostream>
#include <map>
#include <stdexcept>
#include <utility>

static unsigned comparisons = 0;

void same(cdouble first, cdouble second, const char* label) {
  ++comparisons;
  const ldouble scale = std::max(std::abs(first), std::abs(second));
  if (!hepcat_spinas::finite_complex(first) ||
      !hepcat_spinas::finite_complex(second) ||
      std::abs(first - second) > ldouble(3e-10L) * scale + ldouble(1e-23L)) {
    std::cerr << label << ": " << first << " vs " << second << '\n';
    throw std::runtime_error(label);
  }
}

template<class Process>
void set_values(Process& process, int round) {
  auto values = process.get_parameters();
  for (const auto& entry : parameter_indices<Process>()) {
    if (entry.first == "Me") values[entry.second] = ldouble(0.8L) + ldouble(round) / 5;
    if (entry.first == "EE") values[entry.second] = cdouble(
      ldouble(0.25L) + ldouble(round) / 30, round == 1 ? ldouble(0.11L) : ldouble(0));
    if (entry.first == "width") values[entry.second] = ldouble(0.03L) + ldouble(round) / 100;
    if (entry.first == "internal_mass") values[entry.second] = cdouble(ldouble(1.4L), round==1 ? ldouble(0.2L) : ldouble(0));
    if (entry.first == "momentum_factor_1") {
      values[entry.second] = round == 0 ? cdouble(0, ldouble(1) / 7)
        : round == 1 ? cdouble(-2, 0) : cdouble(ldouble(2) / 5, -ldouble(1) / 3);
    }
  }
  process.set_parameters(values);
}

template<class Process>
void require_uninitialized(Process& process) {
  bool amplitude=false, sum=false, normalized=false;
  try { process.amp(Process::default_spins()); } catch(const std::logic_error&) { amplitude=true; }
  try { process.spin_sum(); } catch(const std::logic_error&) { sum=true; }
  try { process.amp2(); } catch(const std::logic_error&) { normalized=true; }
  if(!amplitude || !sum || !normalized)
    throw std::runtime_error("Default process accepted an evaluation without momenta");
}

template<class Readable, class Optimized>
void compare(const char* label, const std::array<int, Readable::leg_count>& ranks) {
  Readable readable;
  Optimized optimized;
  require_uninitialized(readable);
  require_uninitialized(optimized);
  for (int round = 0; round < 3; ++round) {
    set_values(readable, round);
    set_values(optimized, round);
    typename Readable::Spins spins = Readable::default_spins();
    bool readable_rejected = false, optimized_rejected = false;
    try { readable.amp(spins); } catch (const std::logic_error&) { readable_rejected = true; }
    try { optimized.amp(spins); } catch (const std::logic_error&) { optimized_rejected = true; }
    if (!readable_rejected || !optimized_rejected)
      throw std::runtime_error("Parameter update retained stale momentum state");
    readable_rejected = optimized_rejected = false;
    try { readable.spin_sum(); } catch (const std::logic_error&) { readable_rejected = true; }
    try { optimized.spin_sum(); } catch (const std::logic_error&) { optimized_rejected = true; }
    if (!readable_rejected || !optimized_rejected)
      throw std::runtime_error("Spin sum accepted unavailable momentum state");
    readable_rejected = optimized_rejected = false;
    try { readable.amp2(); } catch (const std::logic_error&) { readable_rejected = true; }
    try { optimized.amp2(); } catch (const std::logic_error&) { optimized_rejected = true; }
    if (!readable_rejected || !optimized_rejected)
      throw std::runtime_error("Normalized spin sum accepted unavailable momentum state");
    for (unsigned point_index = 0; point_index < 4; ++point_index) {
      const auto point = hepcat_spinas::make_example_momenta(
        readable.masses(), Readable::incoming_legs(),
        ldouble(50 + 7 * point_index), point_index + round * 11);
      readable.set_momenta(point);
      optimized.set_momenta(point);
      // Exercise demand-driven preparation before public amp() requests the
      // complementary helicities. The full enumeration below remains oracle.
      const ldouble readable_early_amp2 = readable.amp2();
      const ldouble optimized_early_amp2 = optimized.amp2();
      const ldouble readable_early_sum = readable.spin_sum();
      const ldouble optimized_early_sum = optimized.spin_sum();
      Readable readable_early_copy(readable);
      Optimized optimized_early_copy(optimized);
      same(cdouble(readable_early_copy.spin_sum()), cdouble(readable_early_sum),
           "readable copy after shortened sum");
      same(cdouble(optimized_early_copy.spin_sum()), cdouble(optimized_early_sum),
           "optimized copy after shortened sum");
      auto invalid = Readable::default_spins();
      invalid[0] = ranks[0] + 2;
      bool bad_a=false,bad_b=false;
      try { readable.amp(invalid); } catch(const std::invalid_argument&) { bad_a=true; }
      try { optimized.amp(invalid); } catch(const std::invalid_argument&) { bad_b=true; }
      if(!bad_a || !bad_b) throw std::runtime_error("Invalid public spin accepted");
      for(unsigned leg=0;leg<Readable::leg_count;++leg) {
        if(readable.masses()[leg]!=0 || ranks[leg]==0) continue;
        invalid=Readable::default_spins(); invalid[leg]=0;bad_a=bad_b=false;
        try { readable.amp(invalid); } catch(const std::invalid_argument&) { bad_a=true; }
        try { optimized.amp(invalid); } catch(const std::invalid_argument&) { bad_b=true; }
        if(!bad_a || !bad_b) throw std::runtime_error("Invalid public helicity accepted");
      }
      // Use the public complex amplitudes as an exhaustive oracle, independently
      // of any symmetry shortcut shared by the two generated spin sums.
      ldouble readable_exhaustive = 0;
      ldouble optimized_exhaustive = 0;
      for (const auto& helicities : Readable::helicity_configurations()) {
        spins = helicities;
        std::function<void(unsigned)> visit = [&](unsigned leg) {
          if (leg == Readable::leg_count) {
            const cdouble readable_amplitude = readable.amp(spins);
            const cdouble optimized_amplitude = optimized.amp(spins);
            same(readable_amplitude, optimized_amplitude, label);
            readable_exhaustive += std::norm(readable_amplitude);
            optimized_exhaustive += std::norm(optimized_amplitude);
            return;
          }
          if (readable.masses()[leg] == 0 || ranks[leg] == 0) {
            visit(leg + 1);
            return;
          }
          for (spins[leg] = -ranks[leg]; spins[leg] <= ranks[leg]; spins[leg] += 2)
            visit(leg + 1);
        };
        visit(0);
      }
      same(cdouble(readable.spin_sum()), cdouble(optimized.spin_sum()), "raw spin sum");
      same(cdouble(readable.amp2()), cdouble(optimized.amp2()), "normalized spin sum");
      same(cdouble(readable.spin_sum()), cdouble(readable_exhaustive),
           "readable raw spin sum versus exhaustive public amplitudes");
      same(cdouble(optimized.spin_sum()), cdouble(optimized_exhaustive),
           "optimized raw spin sum versus exhaustive public amplitudes");
      same(cdouble(readable_early_sum), cdouble(readable_exhaustive),
           "readable early sum versus exhaustive public amplitudes");
      same(cdouble(optimized_early_sum), cdouble(optimized_exhaustive),
           "optimized early sum versus exhaustive public amplitudes");
      const ldouble normalization = Readable::symmetry_factor() /
                                   Readable::incoming_spin_average();
      same(cdouble(readable_early_amp2), cdouble(normalization * readable_exhaustive),
           "readable early amp2 versus exhaustive public amplitudes");
      same(cdouble(optimized_early_amp2), cdouble(normalization * optimized_exhaustive),
           "optimized early amp2 versus exhaustive public amplitudes");
      same(cdouble(readable.amp2()), cdouble(normalization * readable_exhaustive),
           "readable normalized spin sum versus exhaustive public amplitudes");
      same(cdouble(optimized.amp2()), cdouble(normalization * optimized_exhaustive),
           "optimized normalized spin sum versus exhaustive public amplitudes");
      spins = Readable::default_spins();
      Readable readable_copy(readable);
      Optimized optimized_copy(optimized);
      same(readable_copy.amp(spins), optimized_copy.amp(spins), "copy");
      auto deferred_spins = spins;
      for (const auto& helicities : Readable::helicity_configurations()) {
        bool has_negative_pivot = false;
        for (unsigned leg = 0; leg < Readable::leg_count; ++leg) {
          if (readable.masses()[leg] != 0 || ranks[leg] == 0) continue;
          has_negative_pivot = helicities[leg] < 0;
          break;
        }
        if (!has_negative_pivot) continue;
        for (unsigned leg = 0; leg < Readable::leg_count; ++leg)
          if (readable.masses()[leg] == 0) deferred_spins[leg] = helicities[leg];
        break;
      }
      same(readable_early_copy.amp(deferred_spins), optimized_early_copy.amp(deferred_spins),
           "public omitted-helicity amplitude completes a copied shortened cache");
      // Relocation must rebind both products and shared matrices to their owner.
      Readable readable_relocated(std::move(readable_early_copy));
      Optimized optimized_relocated(std::move(optimized_early_copy));
      const auto relocated_point = hepcat_spinas::make_example_momenta(
        readable.masses(), Readable::incoming_legs(), ldouble(65 + point_index), round + 23);
      readable_relocated.set_momenta(relocated_point);
      optimized_relocated.set_momenta(relocated_point);
      same(readable_relocated.amp(deferred_spins), optimized_relocated.amp(deferred_spins),
           "relocated process cache owns its particles");
      Readable readable_assignment;
      Optimized optimized_assignment;
      readable_assignment = readable;
      optimized_assignment = optimized;
      same(readable_assignment.amp(spins), optimized_assignment.amp(spins), "assignment");
    }
  }
  readable.set_masses(ldouble(1.7L));
  optimized.set_masses(ldouble(1.7L));
  const auto point = hepcat_spinas::make_example_momenta(
    readable.masses(), Readable::incoming_legs(), ldouble(77), 13);
  readable.set_momenta(point);
  optimized.set_momenta(point);
  same(readable.amp(Readable::default_spins()),
       optimized.amp(Optimized::default_spins()), "mass setter");
  const auto readable_parameters = readable.get_parameters();
  const auto optimized_parameters = optimized.get_parameters();
  const auto with_widths = optimized.amp(Optimized::default_spins());
  readable.set_widths_enabled(false);
  optimized.set_widths_enabled(false);
  require_uninitialized(readable);
  require_uninitialized(optimized);
  if (readable.get_parameters() != readable_parameters ||
      optimized.get_parameters() != optimized_parameters)
    throw std::runtime_error("Width selection changed model parameters");
  readable.set_momenta(point);
  optimized.set_momenta(point);
  same(cdouble(readable.amp2()), cdouble(optimized.amp2()), "early zero-width amp2");
  same(readable.amp(Readable::default_spins()),
       optimized.amp(Optimized::default_spins()), "zero-width selection");
  Readable zero_width_readable(readable);
  Optimized zero_width_optimized(optimized);
  same(zero_width_readable.amp(Readable::default_spins()),
       zero_width_optimized.amp(Optimized::default_spins()), "copied zero-width selection");
  readable.set_widths_enabled(true);
  optimized.set_widths_enabled(true);
  readable.set_momenta(point);
  optimized.set_momenta(point);
  same(optimized.amp(Optimized::default_spins()), with_widths,
       "normal simulation widths restored");
  same(readable.amp(Readable::default_spins()),
       optimized.amp(Optimized::default_spins()), "restored-width selection");
}

template<class Process>
void scalar_oracle() {
  Process process;
  const auto indices = parameter_indices<Process>();
  for (int round = 0; round < 3; ++round) {
    set_values(process, round);
    const auto values = process.get_parameters();
    const ldouble mass = std::real(values[indices.at("Me")]);
    const cdouble charge = values[indices.at("EE")];
    const cdouble coupling = values[indices.at("momentum_factor_1")];
    const cdouble width = values[indices.at("width")];
    for (unsigned point_index = 0; point_index < 5; ++point_index) {
      const auto point = hepcat_spinas::make_example_momenta(
        process.masses(), Process::incoming_legs(), ldouble(60 + point_index * 3), point_index);
      process.set_momenta(point);
      auto transferred = point[0];
      for (unsigned component = 0; component < 4; ++component)
        transferred[component] -= point[2][component];
      const cdouble constant = std::acos(ldouble(-1)) + std::sqrt(ldouble(2)) +
        std::exp(cdouble(0, ldouble(1) / 3));
      const cdouble expected = constant * std::pow(coupling + charge * mass, 2) *
        std::pow(coupling, ldouble(0.5L)) *
        (ldouble(1) - hepcat_spinas::dot(point[0], point[2])) /
        (cdouble(hepcat_spinas::mass2(transferred) - mass * mass) +
          cdouble(0, 1) * mass * width);
      same(process.amp(typename Process::Spins{{0, 0, 0, 0}}), expected,
           "direct complex scalar and width oracle");
    }
  }
}

template<class Process>
void tiny_imaginary_mass_oracle() {
  Process process;
  auto values=process.get_parameters();
  const cdouble mass(1,4*std::numeric_limits<ldouble>::epsilon());
  values[parameter_indices<Process>().at("Me")]=mass;
  process.set_parameters(values);
  const auto point=hepcat_spinas::make_example_momenta(
    process.masses(),Process::incoming_legs(),ldouble(30),7);
  process.set_momenta(point);
  auto transferred=point[0];
  for(unsigned component=0;component<4;++component)transferred[component]-=point[2][component];
  const cdouble expected=ldouble(1)/(cdouble(hepcat_spinas::mass2(transferred))-mass*mass);
  const cdouble actual=process.amp(typename Process::Spins{{0,0,0,0}});
  same(actual,expected,"tiny imaginary mass propagator");
  // The ordinary amplitude tolerance alone could hide a lost tiny component.
  if(std::imag(actual)==0 || std::abs(std::imag(actual)-std::imag(expected))>
      ldouble(1e-10L)*std::abs(std::imag(expected)))
    throw std::runtime_error("A parameter-dependent propagator lost its tiny imaginary part");
}

#include "optimized-comparisons.inc"

int main() {
  try {
    run_style_comparisons();
    scalar_oracle<hepcat_generated::optimized_scalar_readable>();
    scalar_oracle<hepcat_generated::optimized_scalar_optimized>();
    scalar_oracle<hepcat_generated::p1>();
    tiny_imaginary_mass_oracle<hepcat_generated::optimized_tiny_mass_readable>();
    tiny_imaginary_mass_oracle<hepcat_generated::optimized_tiny_mass_optimized>();
    std::cout << "OPTIMIZED_COMPLEX_COMPARISONS=" << comparisons << '\n';
  } catch (const std::exception& error) {
    std::cerr << error.what() << '\n';
    return 1;
  }
}
