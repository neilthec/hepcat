// Replay the publication's four-photon sample on a Mathematica WriteSPINASProcess export.
#include "process.hpp"
#include <algorithm>
#include <array>
#include <charconv>
#include <cmath>
#include <cstdint>
#include <filesystem>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <limits>
#include <stdexcept>
#include <string>

namespace {
using Process = hepcat_generated::eeAAAA;
constexpr auto helicities = Process::helicity_count;
static_assert(Process::leg_count == 6 && helicities == 16, "Expected the full eeAAAA family");
static_assert(sizeof(double) == 8 && std::numeric_limits<double>::is_iec559, "Expected IEEE binary64");

double read_binary64(std::ifstream& input) {
    double value;
    char* bytes = reinterpret_cast<char*>(&value);
    if (!input.read(bytes, sizeof(value))) throw std::runtime_error("Incomplete binary record");
    const std::uint16_t one = 1;
    if (*reinterpret_cast<const unsigned char*>(&one) != 1) std::reverse(bytes, bytes + sizeof(value));
    return value;
}

struct Result {
    std::size_t points = 0, failed = 0, worst_point = 0;
    ldouble maximum_relative = 0;

    void check(ldouble actual, ldouble expected) {
        if (!std::isfinite(actual) || !std::isfinite(expected))
            throw std::runtime_error("Nonfinite squared amplitude");
        ++points;
        const auto scale = std::max(std::abs(actual), std::abs(expected));
        const auto relative = scale == 0 ? 0 : std::abs(actual - expected) / scale;
        if (relative > 1e-7L) ++failed;
        if (relative > maximum_relative) {
            maximum_relative = relative;
            worst_point = points;
        }
    }

    void print(const std::string& label) const {
        std::cout << label << ',' << points << ',' << failed << ',' << maximum_relative
                  << ',' << worst_point << '\n';
    }
};

int replay(const char* momenta_file, const char* reference_file, const char* point_limit) {
    const auto bytes = std::filesystem::file_size(momenta_file);
    constexpr std::size_t event_size = Process::leg_count * 4 * sizeof(double);
    if (bytes == 0 || bytes % event_size != 0) throw std::runtime_error("Invalid momentum file size");
    const auto events = bytes / event_size;
    if (std::filesystem::file_size(reference_file) != helicities * events * sizeof(double))
        throw std::runtime_error("Reference and momentum files have different event counts");
    auto count = events;
    if (point_limit) {
        const std::string arg = point_limit;
        const auto end = arg.data() + arg.size();
        const auto parsed = std::from_chars(arg.data(), end, count);
        if (parsed.ec != std::errc{} || parsed.ptr != end || count == 0 || count > events)
            throw std::runtime_error("Point limit must be from 1 through the input event count");
    }

    std::ifstream momenta(momenta_file, std::ios::binary);
    // MadGraph stores one contiguous event stream per labeled helicity.
    std::array<std::ifstream, helicities> references;
    for (std::size_t h = 0; h < helicities; ++h) {
        references[h].open(reference_file, std::ios::binary);
        references[h].seekg(static_cast<std::streamoff>(h * events * sizeof(double)));
        if (!references[h]) throw std::runtime_error("Cannot open the helicity reference");
    }

    // Preserve the original binary64 external parameters even in wider builds.
    Process process(ldouble(0.31333), ldouble(0.0005));
    process.set_widths_enabled(false);
    const auto configurations = Process::helicity_configurations();
    std::array<Result, helicities> fixed;
    Result summed, public_amp2;
    for (std::uintmax_t event = 0; event < count; ++event) {
        Process::Momenta point;
        for (auto& leg : point)
            for (auto& component : leg) component = read_binary64(momenta);
        process.set_momenta(point);
        ldouble observed_sum = 0, reference_sum = 0;
        for (std::size_t h = 0; h < helicities; ++h) {
            auto spins = configurations[h];
            ldouble rate = 0;
            for (int first : {-1, 1})
                for (int second : {-1, 1}) {
                    spins[0] = first;
                    spins[1] = second;
                    rate += std::norm(process.amp(spins));
                }
            rate /= 96; // Four initial spin states and 4! identical final photons.
            const auto reference = read_binary64(references[h]);
            fixed[h].check(rate, reference);
            observed_sum += rate;
            reference_sum += reference;
        }
        summed.check(observed_sum, reference_sum);
        public_amp2.check(process.amp2(), reference_sum);
    }
    std::cout << std::setprecision(std::numeric_limits<ldouble>::max_digits10)
              << "helicity,points,failed,max_relative,worst_point\n";
    bool passed = summed.failed == 0 && public_amp2.failed == 0;
    for (std::size_t h = 0; h < helicities; ++h) {
        fixed[h].print(std::to_string(h));
        passed = passed && fixed[h].failed == 0;
    }
    summed.print("summed");
    public_amp2.print("public_amp2");
    return passed ? 0 : 1;
}
} // namespace

int main(int argc, char** argv) {
    if (argc != 3 && argc != 4) {
        std::cerr << "Usage: replay-wolfram MOMENTA.dat MADGRAPH.dat [FIRST_N_POINTS]\n";
        return 2;
    }
    try {
        return replay(argv[1], argv[2], argc == 4 ? argv[3] : nullptr);
    } catch (const std::exception& e) {
        std::cerr << e.what() << '\n';
        return 2;
    }
}
