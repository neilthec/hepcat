// The timed call includes both momentum setup and the full spin/helicity sum.
#include "process.hpp"
#include <algorithm>
#include <memory>

using Process = hepcat_generated::qed_rate;
static std::unique_ptr<Process> process;

extern "C" void rate_init(int mass_slot, int charge_slot) {
    auto values = Process::default_parameters();
    values.at(mass_slot) = 1.;
    values.at(charge_slot) = 1./3;
    process = std::make_unique<Process>(values);
}
extern "C" void rate_set_point(const double* momenta) {
    Process::Momenta p;
    for (auto& particle : p) {
        std::copy_n(momenta, 4, particle.begin());
        momenta += 4;
    }
    process->set_momenta(p);
}

extern "C" void rate_amplitude(const int* spins, double* result) {
    Process::Spins state;
    std::copy_n(spins, state.size(), state.begin());
    const auto amplitude = process->amp(state);
    result[0] = amplitude.real();
    result[1] = amplitude.imag();
}

extern "C" double rate(const double* momenta) {
    rate_set_point(momenta);
    return process->amp2();
}
