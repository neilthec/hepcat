// End-to-end model-based checks. Print the reduction and numerical residuals
// so a later change in algebra or evaluation precision is visible in CTest -V.
#include "amplitude.hpp"
#include <iostream>
#include <stdexcept>

using namespace hepcat;

int main(int argc, char** argv) {
    try {
        if (argc < 11)
            throw std::invalid_argument("expected model directories, model numbers, seed and external particles");
        model vertices(argv[1], std::stoul(argv[2])), channels(argv[1], std::stoul(argv[3]));
        model rules(argv[4], std::stoul(argv[5]));
        std::vector<std::string> external(argv + 7, argv + argc);
        amplitude amp(external, vertices, channels, rules);
        amp.generate(4).complete_high_energy(4);
        const auto& reduction = amp.last_high_energy();
        std::cout << "Final expression nodes " << reduction.nodes_before_final_reduction << " -> "
                  << reduction.nodes_after_final_reduction << '\n';
        validation_options options;
        options.seed = std::stoull(argv[6]);
        options.workers = 4;
        const auto report = amp.validate(options);
        for (const auto& c : report.checks)
            if (c.status != check_outcome::pass)
                std::cerr << c.kind << " " << c.detail << '\n';
        for (const auto& c : report.numerical) {
            std::cout << c.kind << " " << c.compared << " points; " << c.failures << " mismatches; "
                      << "max relative " << c.max_relative_error << ", replays " << c.replays
                      << " (max relative " << c.max_replay_relative_error << ") " << c.detail << '\n';
        }
        if (!report.passed())
            throw std::runtime_error("amplitude validation did not pass");
    } catch (const std::exception& e) {
        std::cerr << e.what() << '\n';
        return 1;
    }
}
