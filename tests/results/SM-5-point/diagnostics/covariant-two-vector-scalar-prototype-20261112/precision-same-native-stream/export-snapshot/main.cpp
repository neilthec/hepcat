#include "process.hpp"
#include <iomanip>
#include <iostream>
#include <stdexcept>

int main() {
  try {
    using Process = hepcat_generated::experimentaleebarZZh;
    Process process;
    const auto incoming = Process::incoming_legs();
    const ldouble energy = hepcat_spinas::example_center_of_mass_energy(
      process.masses(), incoming);

    std::cout << std::setprecision(18);
    std::cout << "HEPCAT/SPINAS physical 2-to-N examples; all exported massless helicities.\n";
    for (unsigned point = 0; point < 3; ++point) {
      const auto momenta = hepcat_spinas::make_example_momenta(
        process.masses(), incoming, energy, point);
      const auto check = hepcat_spinas::validate_momenta(
        momenta, process.masses(), incoming);
      if (!check.valid) {
        throw std::runtime_error(check.message);
      }

      process.set_momenta(momenta);
      const auto spins = Process::default_spins();
      const auto amplitude = process.amp(spins);
      const auto averaged = process.amp2();
      if (!hepcat_spinas::finite_complex(amplitude) || !std::isfinite(averaged)) {
        throw std::runtime_error("An example amplitude is not finite.");
      }

      std::cout << "Point " << point + 1 << "; sqrt(s) = " << energy << '\n';
      for (std::size_t i = 0; i < Process::leg_count; ++i) {
        std::cout << "  p" << i + 1 << " =";
        for (const auto x : momenta[i]) {
          std::cout << ' ' << x;
        }
        std::cout << '\n';
      }
      std::cout << "  conservation/shell scaled error = " << check.maximum_scaled_error << '\n'
        << "  amplitude(default doubled spins) = " << amplitude << '\n'
        << "  spin/helicity-summed, initial-averaged, symmetry-weighted |M|^2 = " << averaged << '\n';
    }
    std::cout << "The final-state identical-particle symmetry factor is included in amp2().\n";
    return 0;
  } catch (const std::exception& error) {
    std::cerr << "Example failed: " << error.what() << '\n';
    return 1;
  }
}
