// File bridge for the optional Mathematica front end. Parsing and output are
// deliberately outside the timer used to compare native reduction schedules.
#include "amplitude.hpp"
#include "reduce.hpp"

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

std::size_t count(std::string_view text) {
    std::size_t value = 0;
    auto end = text.data() + text.size();
    auto parsed = std::from_chars(text.data(), end, value);
    if (parsed.ec != std::errc{} || parsed.ptr != end)
        throw std::invalid_argument("expected a nonnegative integer, got " + std::string(text));
    return value;
}

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
        bool prepared = format == "HEPCAT_FACTOR_PREPARED_1";
        bool complete = prepared || format == "HEPCAT_FACTOR_1";
        if (!complete && format != "HEPCAT_BATCH_1") throw std::invalid_argument("unknown native amplitude format");
        auto amp = read_amplitude(input, prepared);
        hepcat::factorization_options options;
        options.workers = workers;
        if (complete) {
            auto directions = count(line(input));
            for (std::size_t i = 0; i < directions; ++i) options.correction_basis.push_back(line(input));
        }
        if (input.peek() != std::char_traits<char>::eof()) throw std::invalid_argument("unexpected trailing records");
        if (input.bad()) throw std::runtime_error("error reading native amplitude file");
        using clock = std::chrono::steady_clock;
        if (complete) {
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
