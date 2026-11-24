// Compare each photon helicity after summing massive spins. Inputs to MadGraph
// use physical crossing. Both spinor expressions use the same all-incoming point.
// Usage: check-contracted-external TABLE MADGRAPH_LIBRARY POINTS FIRST_SEED
#include "contracted-evaluation.hpp"
#include <dlfcn.h>
#include <filesystem>
#include "spinas.h"

namespace {
// SPINAS takes future-directed external momenta. For the all-incoming
// chart used by these polynomials, crossing can be implemented by changing
// the square column's sign. Moving that sign to the angle column changes
// only an external-state phase in the squared comparison.
void populate_spinors(hepcat::validation::point &point) {
    using hepcat::validation::spinor;
    for (unsigned i = 0; i < point.momenta.size(); ++i) {
        const ldouble crossing = i < 2 ? 1 : -1;
        ldouble momentum[4];
        for (unsigned j = 0; j < 4; ++j) momentum[j] = crossing*point.momenta[i][j];
        spinas::particle part(point.masses[i]);
        part.set_momentum(momentum);
        auto copy = [](const spinas::cvector &v, ldouble sign) -> spinor {
            return {sign*v.get(0), sign*v.get(1)};
        };
        if (point.masses[i] == 0) {
            point.angles[i] = copy(part.rangle(),1);
            point.squares[i] = copy(part.lsquare(),crossing);
        } else {
            for (unsigned s = 0; s < 2; ++s) {
                const int spin = s ? 1 : -1;
                point.angle_basis[i][s] = copy(part.rangle(spin),1);
                point.square_basis[i][s] = copy(part.lsquare(spin),crossing);
            }
        }
    }
}
template<class T> T load(void *library, const char *name) {
    auto symbol = dlsym(library, name);
    if (!symbol) throw std::runtime_error(std::string("Missing MadGraph function ") + name);
    return reinterpret_cast<T>(symbol);
}
}

int main(int argc, char **argv) try {
    using namespace contracted_qed;
    if (argc != 5) throw std::invalid_argument("Use TABLE MADGRAPH_LIBRARY POINTS FIRST_SEED");
    const table input(argv[1]);
    const program amplitude(input);
    const unsigned n = input.legs.size(), charged = n - input.nphot;
    std::vector<real> masses(n);
    std::vector<int> roles(n), helicities(n);
    for (unsigned j = 0; j < n; ++j) {
        roles[j] = input.legs[j] == "e" ? 1 : input.legs[j] == "E" ? -1 : 2;
        masses[j] = j < charged ? 1 : 0;
    }
    auto handle = dlopen(argv[2], RTLD_LOCAL | RTLD_NOW);
    if (!handle) throw std::runtime_error(dlerror());
    const auto card = (std::filesystem::absolute(argv[2]).parent_path() / "process/Cards/param_card.dat").string();
    if (!std::filesystem::is_regular_file(card)) throw std::runtime_error("Missing parameter card " + card);
    int quiet = 0;
    load<void(*)(int*)>(handle, "setparamlog_")(&quiet);
    load<void(*)(const char*,size_t)>(handle, "setpara_")(card.c_str(),card.size());
    auto fixed = load<void(*)(double*,int*,double*)>(handle, "hepcat_mg_evaluate");
    const unsigned count = std::stoul(argv[3]);
    if (!count) throw std::invalid_argument("Supply a positive sample count");
    const auto first = std::stoull(argv[4]);
    const real norm = 1/(4*std::tgamma(1+input.nphot)*std::pow(std::tgamma(charged/2),2));
    std::cout << std::setprecision(16);
    std::cout << "seed physical_rate spurious_rate MG_rate maximum_polarized_error physical_s spurious_s MG_s\n";
    unsigned failures = 0;
    for (unsigned sample = 0; sample < count; ++sample) {
        auto point = sample_point(masses,first+sample);
        populate_spinors(point);
        std::vector<double> momenta;
        for (unsigned j = 0; j < n; ++j)
            for (auto x : point.momenta[j]) momenta.push_back(j < 2 ? x : -x);
        real rate[3]{}, seconds[3]{}, worst = 0;
        for (unsigned photons = 0; photons < (1u<<input.nphot); ++photons) {
            real values[3]{};
            for (unsigned k = 0; k < input.nphot; ++k) {
                const int sign = (photons >> k & 1) ? 1 : -1;
                roles[k+charged] = 2*sign;
                helicities[k+charged] = -sign; // all photons are crossed in this physical process
            }
            for (unsigned spins = 0; spins < (1u<<charged); ++spins) {
                for (unsigned j = 0; j < charged; ++j) {
                    unsigned col = spins >> j & 1;
                    point.angles[j] = point.angle_basis[j][col];
                    point.squares[j] = point.square_basis[j][col];
                    helicities[j] = col ? 1 : -1;
                }
                for (int backend = 0; backend < 3; ++backend) {
                    auto start = clock_type::now();
                    if (backend < 2) values[backend] += std::norm(amplitude(point,roles,backend==0,1.L/3,1));
                    else {
                        double value = 0;
                        fixed(momenta.data(),helicities.data(),&value);
                        values[2] += value;
                    }
                    seconds[backend] += std::chrono::duration<double>(clock_type::now()-start).count();
                }
            }
            for (int b=0;b<3;++b) rate[b]+=norm*values[b];
            if (!(values[2]>0) || !std::isfinite(values[2])) throw std::runtime_error("Invalid reference rate");
            for (int b=0;b<2;++b) {
                auto err=std::abs(values[b]-values[2])/values[2];
                if (!std::isfinite(err) || err>1e-6L) ++failures;
                worst=std::max(worst,err);
            }
        }
        std::cout << first+sample << " " << rate[0] << " " << rate[1] << " " << rate[2] << " " << worst;
        for (auto seconds_used : seconds) std::cout << " " << seconds_used;
        std::cout << std::endl;
    }
    dlclose(handle);
    return failures ? 2 : 0;
} catch (const std::exception &error) { std::cerr << error.what() << "\n"; return 1; }
