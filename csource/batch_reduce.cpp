// File bridge for the optional Mathematica front end. Parsing and output are
// deliberately outside the timer used to compare native reduction schedules.
#include "amplitude.hpp"
#include "reduce.hpp"
#include "process_memory.hpp"

#include <charconv>
#include <chrono>
#include <filesystem>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace {
namespace fs = std::filesystem;

template<class Integer> Integer read_integer(std::string_view text) {
    Integer value = 0;
    auto end = text.data() + text.size();
    auto parsed = std::from_chars(text.data(), end, value);
    if (parsed.ec != std::errc{} || parsed.ptr != end)
        throw std::invalid_argument("expected an integer, got " + std::string(text));
    return value;
}

std::size_t count(std::string_view text) { return read_integer<std::size_t>(text); }

std::string line(std::istream& input) {
    std::string result;
    if (!std::getline(input, result)) throw std::runtime_error("incomplete native amplitude file");
    if (!result.empty() && result.back() == '\r') result.pop_back();
    return result;
}

const std::vector<hepcat::Id>& list(const hepcat::Algebra& tree, hepcat::Id id) {
    const auto& node = tree.node(id);
    if (node.head != "List" || !node.compound) throw std::invalid_argument("expected List[...]");
    return node.args;
}

hepcat::numerator_shell shell(const hepcat::Algebra& tree, hepcat::Id id) {
    const auto& node = tree.node(id);
    if (node.head != "Shell" || node.args.size() != 2)
        throw std::invalid_argument("expected Shell[List[legs],mass]");
    std::vector<int> legs;
    for (auto leg : list(tree, node.args[0])) {
        int label = tree.leg(leg);
        if (label <= 0) throw std::invalid_argument("expected a positive external leg");
        legs.push_back(label);
    }
    return {std::move(legs), tree.format(node.args[1])};
}

hepcat::amplitude read_amplitude(std::istream& input, bool prepared) {
    hepcat::Algebra tree;
    std::vector<std::string> masses;
    for (auto mass : list(tree, tree.parse(line(input)))) masses.push_back(tree.format(mass));
    hepcat::amplitude amp(std::move(masses));
    for (bool diagrams : {true, false}) {
        auto rows = count(line(input));
        for (std::size_t i = 0; i < rows; ++i) {
            try {
                auto assumptions = tree.parse(line(input));
                std::string expression = line(input);
                std::string remainder = prepared ? line(input) : "0";
                if (diagrams) {
                    std::vector<hepcat::numerator_shell> shells;
                    for (auto id : list(tree, assumptions)) shells.push_back(shell(tree, id));
                    if (prepared) amp.add_prepared_diagram(expression, std::move(shells), remainder);
                    else amp.add_diagram(expression, std::move(shells));
                } else {
                    auto cut = shell(tree, assumptions);
                    if (prepared) amp.add_prepared_factorization_product(expression, std::move(cut), remainder);
                    else amp.add_factorization_product(expression, std::move(cut));
                }
            } catch (const std::exception& e) {
                throw std::runtime_error(std::string(diagrams ? "diagram " : "factorization product ") +
                                         std::to_string(i + 1) + ": " + e.what());
            }
        }
    }
    return amp;
}

// Publish completed output with one rename. A failed or concurrent run cannot
// leave a partial result in place of a previously completed one.
template<class Writer>
void write_result(const fs::path& path, Writer write) {
    fs::path stage;
    for (std::size_t suffix = 0; ; ++suffix) {
        stage = path.string() + ".writing-" + std::to_string(suffix);
        if (fs::create_directory(stage)) break;
    }
    struct Cleanup {
        fs::path path;
        ~Cleanup() { std::error_code ignored; fs::remove_all(path, ignored); }
    } cleanup{stage};
    const auto temporary = stage / "result";
    std::ofstream output(temporary);
    output.exceptions(std::ios::failbit | std::ios::badbit);
    output << std::setprecision(17);
    write(output);
    output.close();
    fs::rename(temporary, path);
}
void generate_from_models(std::istream& input, const fs::path& path, std::size_t workers) {
    struct source {
        std::string directory;
        unsigned number;
        std::string scope;
    };
    auto spec = [&] { return source{line(input), read_integer<unsigned>(line(input)), line(input)}; };
    const auto vertices = spec(), channels = spec();
    const auto mode = line(input), refs = line(input);
    if (refs != "Automatic" && refs != "Keep" && refs != "Physical")
        throw std::invalid_argument("unknown generation reference policy");
    if (mode != "Both" && mode != "Channels" && mode != "Diagrams")
        throw std::invalid_argument("unknown generation group");
    auto n = count(line(input));
    if (n < 3 || n > 64) throw std::invalid_argument("native generation requires 3 to 64 external legs");
    std::vector<std::string> names;
    names.reserve(n);
    for (std::size_t i = 0; i < n; ++i)
        names.push_back(line(input));
    if (input.peek() != std::char_traits<char>::eof() || input.bad())
        throw std::invalid_argument("unexpected generation input");
    using clock = std::chrono::steady_clock;
    auto begin = clock::now();
    hepcat::model vm(vertices.directory, vertices.number, vertices.scope);
    auto choice = refs == "Keep"       ? hepcat::generation_references::keep
                  : refs == "Physical" ? hepcat::generation_references::physical
                                       : hepcat::generation_references::automatic;
    auto amp = [&] {
        if (mode == "Diagrams") return hepcat::amplitude(std::move(names), vm, choice);
        auto fm = vertices.directory == channels.directory && vertices.number == channels.number &&
                          vertices.scope == channels.scope
                      ? vm
                      : hepcat::model(channels.directory, channels.number, channels.scope);
        return hepcat::amplitude(std::move(names), vm, fm, choice);
    }();
    auto start = clock::now();
    if (mode == "Both")
        amp.generate(workers);
    else if (mode == "Diagrams")
        amp.generate_diagrams(workers);
    else
        amp.generate_factorization_products(workers);
    auto finish = clock::now();
    auto peak = highWaterRSS();
    write_result(path, [&](std::ostream& out) {
        out << "HEPCAT_GENERATED_1\n"
            << amp.last_worker_count() << '\n'
            << std::chrono::duration<double>(start - begin).count() << '\n'
            << std::chrono::duration<double>(finish - start).count() << '\n'
            << peak << "\nList[";
        for (std::size_t i = 0; i < n; ++i) {
            if (i) out << ',';
            out << amp.external_masses()[i];
        }
        out << "]\n";
        for (const auto* rows : {&amp.diagrams(), &amp.factorization_products()}) {
            out << rows->size() << '\n';
            for (const auto& row : *rows) {
                out << "List[";
                for (std::size_t k = 0; k < row.shells.size(); ++k) {
                    auto& s = row.shells[k];
                    if (k) out << ',';
                    out << "List[List[";
                    for (std::size_t j = 0; j < s.legs.size(); ++j) {
                        if (j) out << ',';
                        out << s.legs[j];
                    }
                    out << "]," << s.mass << ']';
                }
                out << "]\n" << row.expression.str() << '\n';
                auto rest = row.expression.discarded_remainder();
                out << (rest ? rest->str() : "0") << '\n';
            }
        }
    });
}
} // namespace

int main(int argc, char** argv) {
    try {
        if (argc != 3 && argc != 4)
            throw std::invalid_argument("usage: hepcat-reduce-amplitude INPUT OUTPUT [WORKERS]");
        std::size_t workers = argc == 4 ? count(argv[3]) : 0;
        std::ifstream input(argv[1]);
        if (!input) throw std::runtime_error("cannot open input file");
        if (fs::exists(argv[2]) && fs::equivalent(argv[1], argv[2]))
            throw std::invalid_argument("input and output paths must differ");
        const auto format = line(input);
        if (format == "HEPCAT_GENERATE_1") {
            generate_from_models(input, argv[2], workers);
            return 0;
        }
        bool energy = format == "HEPCAT_HIGH_ENERGY_1";
        bool prepared = energy || format == "HEPCAT_FACTOR_PREPARED_1";
        bool complete = prepared || format == "HEPCAT_FACTOR_1";
        if (!complete && format != "HEPCAT_BATCH_1") throw std::invalid_argument("unknown native amplitude format");
        auto amp = read_amplitude(input, prepared);
        hepcat::factorization_options options;
        options.workers = workers;
        if (complete) {
            auto directions = count(line(input));
            for (std::size_t i = 0; i < directions; ++i) options.correction_basis.push_back(line(input));
        }
        hepcat::high_energy_options high_energy;
        std::optional<hepcat::amp_expression> seed;
        if (energy) {
            high_energy.workers = workers;
            high_energy.correction_basis = std::move(options.correction_basis);
            auto expression = line(input);
            if (!expression.empty()) seed.emplace(expression, amp.external_masses());
            auto bound = line(input);
            if (bound != "Automatic") high_energy.energy_bound = read_integer<int>(bound);
            auto points = count(line(input));
            for (std::size_t i = 0; i < points; ++i) {
                auto& point = high_energy.parameter_points.emplace_back();
                auto parameters = count(line(input));
                for (std::size_t j = 0; j < parameters; ++j) {
                    auto parameter = line(input);
                    auto value = line(input);
                    point.emplace_back(std::move(parameter), std::move(value));
                }
            }
        }
        if (input.peek() != std::char_traits<char>::eof()) throw std::invalid_argument("unexpected trailing records");
        if (input.bad()) throw std::runtime_error("error reading native amplitude file");
        using clock = std::chrono::steady_clock;
        if (energy) {
            // The benchmark clock starts with the factorized expression already
            // resident in C++. The chained entry point can acquire it here.
            if (!seed) amp.complete_factorization(workers);
            const auto before = highWaterRSS();
            const auto start = clock::now();
            const auto& result = seed ? amp.complete_high_energy(*seed, high_energy)
                                      : amp.complete_high_energy(high_energy);
            double seconds = std::chrono::duration<double>(clock::now() - start).count();
            const auto after = highWaterRSS();
            const auto& r = amp.last_high_energy();
            write_result(argv[2], [&](std::ostream& out) {
                out << "HEPCAT_HIGH_ENERGY_RESULT_1\n" << r.workers << '\n' << seconds << '\n'
                    << r.bound << '\n' << r.candidates << '\n' << r.rank << '\n'
                    << r.training_samples << '\n' << r.validation_samples << '\n'
                    << r.route << '\n' << r.validation_spin_states << '\n' << r.exhaustive_spin << '\n'
                    << before << '\n' << after << '\n' << result.str() << '\n';
            });
        } else if (complete) {
            if (!prepared) amp.reduce(workers);
            auto start = clock::now();
            const auto& result = amp.complete_factorization(options);
            double seconds = std::chrono::duration<double>(clock::now() - start).count();
            const auto& r = amp.last_factorization();
            write_result(argv[2], [&](std::ostream& out) {
                out << "HEPCAT_FACTOR_RESULT_1\n" << r.workers << '\n' << seconds << '\n'
                    << r.channels << '\n' << r.candidates << '\n' << r.rank << '\n'
                    << r.training_samples << '\n' << r.validation_samples << '\n'
                    << r.route << '\n' << result.str() << '\n';
            });
        } else {
            auto start = clock::now();
            amp.reduce(workers);
            double seconds = std::chrono::duration<double>(clock::now() - start).count();
            write_result(argv[2], [&](std::ostream& output) {
                output << "HEPCAT_BATCH_RESULT_1\n" << amp.last_worker_count() << '\n'
                       << seconds << '\n';
                for (auto items : {&amp.diagrams(), &amp.factorization_products()}) {
                    for (const auto& item : *items) {
                        auto remainder = item.expression.discarded_remainder();
                        output << item.expression.str() << '\n' << (remainder ? remainder->str() : "0") << '\n';
                    }
                }
            });
        }
        return 0;
    } catch (const std::exception& e) {
        std::cerr << e.what() << '\n';
        return 1;
    }
}
