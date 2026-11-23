// Compare complex polarized QED amplitudes from two compiled versions of the
// same export. Build with the same include and link flags as time-rate.cpp.
#include "validation_numeric.hpp"
#include <dlfcn.h>
#include <algorithm>
#include <array>
#include <chrono>
#include <cmath>
#include <complex>
#include <filesystem>
#include <iostream>
#include <optional>
#include <stdexcept>
#include <string>
#include <vector>

struct library {
    void* handle;
    template<typename T> T function(const char* name) {
        auto symbol = dlsym(handle, name);
        if (!symbol) throw std::runtime_error(dlerror());
        return reinterpret_cast<T>(symbol);
    }
    explicit library(const char* path) : handle(dlopen(path, RTLD_NOW | RTLD_LOCAL)) {
        if (!handle) throw std::runtime_error(dlerror());
    }
    library(const library&) = delete;
    library& operator=(const library&) = delete;
    ~library() { dlclose(handle); }
};

struct spinas_library : library {
    void (*set_point)(const double*);
    void (*amplitude)(const int*, double*);
    explicit spinas_library(const char* path) : library(path) {
        function<void (*)(int, int)>("rate_init")(0, 1);
        set_point = function<decltype(set_point)>("rate_set_point");
        amplitude = function<decltype(amplitude)>("rate_amplitude");
    }
};

struct mg_library : library {
    void (*fixed)(double*, int*, double*);
    explicit mg_library(const char* path) : library(path) {
        const auto card = std::filesystem::absolute(path).parent_path() / "process/Cards/param_card.dat";
        if (!std::filesystem::is_regular_file(card)) throw std::runtime_error("Missing MadGraph parameter card");
        int quiet = 0;
        function<void(*)(int*)>("setparamlog_")(&quiet);
        const auto name = card.string();
        function<void(*)(const char*, size_t)>("setpara_")(name.data(), name.size());
        fixed = function<decltype(fixed)>("hepcat_mg_evaluate");
    }
};

static void set_spins(std::vector<int>& spins, int state, int fermions) {
    for (size_t k = 0; k < spins.size(); ++k)
        spins[k] = ((state & (1 << k)) ? 1 : -1) * (static_cast<int>(k) < fermions ? 1 : 2);
}

static void time_polarizations(spinas_library& old, spinas_library& updated,
                               std::vector<int>& spins, int fermions) {
    for (int round = 0; round < 2; ++round) {
        std::array<spinas_library*, 2> order{&old, &updated};
        if (round) std::reverse(order.begin(), order.end());
        for (auto* evaluator : order) {
            const auto start = std::chrono::steady_clock::now();
            double checksum = 0;
            for (int state = 0; state < (1 << spins.size()); ++state) {
                set_spins(spins, state, fermions);
                double value[2];
                evaluator->amplitude(spins.data(), value);
                checksum += value[0] + value[1];
            }
            const std::chrono::duration<double> elapsed = std::chrono::steady_clock::now() - start;
            std::cout << (evaluator == &old ? "old " : "new ") << elapsed.count() << ' ' << checksum << '\n';
        }
    }
}

int main(int argc, char** argv) try {
    if (argc != 7 && argc != 8) throw std::invalid_argument("check-rate-amplitudes OLD NEW PAIRS PHOTONS POINTS SEED [MG|--time-polarized]");
    const int pairs = std::stoi(argv[3]), photons = std::stoi(argv[4]);
    const int legs = 2*pairs + photons, samples = std::stoi(argv[5]);
    if (pairs < 1 || photons < 0 || legs < 4 || legs > 9 || samples < 1)
        throw std::invalid_argument("Invalid sample size or QED family");
    const auto seed = std::stoull(argv[6]);
    spinas_library old(argv[1]), optimized(argv[2]);
    std::optional<mg_library> mg;
    const bool timing = argc == 8 && std::string(argv[7]) == "--time-polarized";
    if (argc == 8 && !timing) mg.emplace(argv[7]);
    std::vector<long double> masses(2*pairs, 1);
    masses.resize(legs, 0);
    std::vector<int> spins(legs);
    double worst = 0, worst_mg = 0;
    for (int i = 0; i < samples; ++i) {
        const auto point = hepcat::validation::sample_point(masses, seed+i);
        std::vector<double> momenta;
        for (int k = 0; k < legs; ++k)
            for (auto x : point.momenta[k]) momenta.push_back(k < 2 ? x : -x);
        old.set_point(momenta.data());
        optimized.set_point(momenta.data());
        if (timing) {
            time_polarizations(old, optimized, spins, 2*pairs);
            continue;
        }
        std::vector<double> photon_rates(1 << photons), reference(1 << photons);
        std::vector<int> mg_spins(legs);
        for (int state = 0; state < (1 << legs); ++state) {
            set_spins(spins, state, 2*pairs);
            double a[2], b[2];
            old.amplitude(spins.data(), a);
            optimized.amplitude(spins.data(), b);
            const std::complex<double> x(a[0], a[1]), y(b[0], b[1]);
            const double delta = std::abs(x-y), scale = std::max(std::abs(x), std::abs(y));
            if (!std::isfinite(delta) || delta > 1e-10*scale + 1e-30)
                throw std::runtime_error("Polarized mismatch at point " + std::to_string(i) +
                                         ", spin assignment " + std::to_string(state));
            if (scale > 0) worst = std::max(worst, delta / scale);
            if (mg) {
                std::transform(spins.begin(), spins.end(), mg_spins.begin(), [](int h) { return h > 0 ? 1 : -1; });
                double squared;
                mg->fixed(momenta.data(), mg_spins.data(), &squared);
                const auto photon_state = state >> (2*pairs);
                photon_rates[photon_state] += std::norm(y);
                reference[photon_state] += squared;
            }
        }
        if (mg) for (size_t h = 0; h < reference.size(); ++h) {
            const auto scale = std::max(reference[h], photon_rates[h]);
            const auto diff = std::abs(reference[h] - photon_rates[h]);
            if (!std::isfinite(diff) || diff > 1e-7 * scale)
                throw std::runtime_error("MadGraph photon-helicity mismatch at point " + std::to_string(i) +
                                         ", helicity word " + std::to_string(h));
            if (scale > 0) worst_mg = std::max(worst_mg, diff/scale);
        }
    }
    if (timing) return 0;
    if (mg) std::cout << "photon_rates " << samples*(1 << photons) << "\nmax_relative_mg " << worst_mg << '\n';
    std::cout << "amplitudes " << samples*(1 << legs) << "\nmax_relative " << worst << '\n';
    return 0;
} catch (const std::exception& e) {
    std::cerr << e.what() << '\n';
    return 1;
}
