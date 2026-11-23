// Compare complex polarized QED amplitudes from two compiled versions of the
// same export. Build with the same include and link flags as time-rate.cpp.
#include "validation_numeric.hpp"
#include <dlfcn.h>
#include <algorithm>
#include <cmath>
#include <complex>
#include <iostream>
#include <stdexcept>
#include <vector>

struct library {
    void* handle;
    void (*set_point)(const double*);
    void (*amplitude)(const int*, double*);
    template<typename T> T function(const char* name) {
        auto symbol = dlsym(handle, name);
        if (!symbol) throw std::runtime_error(dlerror());
        return reinterpret_cast<T>(symbol);
    }
    explicit library(const char* path) : handle(dlopen(path, RTLD_NOW | RTLD_LOCAL)) {
        if (!handle) throw std::runtime_error(dlerror());
        function<void (*)(int, int)>("rate_init")(0, 1);
        set_point = function<decltype(set_point)>("rate_set_point");
        amplitude = function<decltype(amplitude)>("rate_amplitude");
    }
    ~library() { dlclose(handle); }
};

int main(int argc, char** argv) try {
    if (argc != 7) throw std::invalid_argument("check-rate-amplitudes OLD NEW PAIRS PHOTONS POINTS SEED");
    const int pairs = std::stoi(argv[3]), photons = std::stoi(argv[4]);
    const int legs = 2*pairs + photons, samples = std::stoi(argv[5]);
    if (pairs < 1 || photons < 0 || legs < 4 || legs > 9 || samples < 1)
        throw std::invalid_argument("Invalid sample size or QED family");
    const auto seed = std::stoull(argv[6]);
    library old(argv[1]), optimized(argv[2]);
    std::vector<long double> masses(2*pairs, 1);
    masses.resize(legs, 0);
    std::vector<int> spins(legs);
    double worst = 0;
    for (int i = 0; i < samples; ++i) {
        const auto point = hepcat::validation::sample_point(masses, seed+i);
        std::vector<double> momenta;
        for (int k = 0; k < legs; ++k)
            for (auto x : point.momenta[k]) momenta.push_back(k < 2 ? x : -x);
        old.set_point(momenta.data());
        optimized.set_point(momenta.data());
        for (int state = 0; state < (1 << legs); ++state) {
            for (int k = 0; k < legs; ++k)
                spins[k] = ((state & (1 << k)) ? 1 : -1) * (k < 2*pairs ? 1 : 2);
            double a[2], b[2];
            old.amplitude(spins.data(), a);
            optimized.amplitude(spins.data(), b);
            const std::complex<double> x(a[0], a[1]), y(b[0], b[1]);
            const double delta = std::abs(x-y), scale = std::max(std::abs(x), std::abs(y));
            if (!std::isfinite(delta) || delta > 1e-10*scale + 1e-30)
                throw std::runtime_error("Polarized mismatch at point " + std::to_string(i) +
                                         ", spin assignment " + std::to_string(state));
            if (scale > 0) worst = std::max(worst, delta / scale);
        }
    }
    std::cout << "amplitudes " << samples*(1 << legs) << "\nmax_relative " << worst << '\n';
    return 0;
} catch (const std::exception& e) {
    std::cerr << e.what() << '\n';
    return 1;
}
