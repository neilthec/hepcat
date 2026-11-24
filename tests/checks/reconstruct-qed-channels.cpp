// Run hepcat-reconstruct-qed ROOT OUTPUT POINTS SQUARE ANGLE MULTIPLICITY LEG...
// Use 0 0 for the sum over massive cuts. OUTPUT is a new file for a verified
// FullForm expression. A diagnostic run needs at least 10,000 signed points.
#include "amplitude.hpp"
#include <filesystem>
#include <fstream>
#include <iostream>
using namespace hepcat;
namespace fs = std::filesystem;

int main(int argc, char** argv) try {
    if (argc < 11) throw std::invalid_argument("Expected root, output, points, shift legs, multiplicity and external particles");
    const fs::path root(argv[1]), output(argv[2]);
    if (fs::exists(output)) throw std::runtime_error("The destination already exists");
    validation_options settings;
    settings.points = std::stoul(argv[3]);
    settings.workers = 1;
    settings.seed = 20261124;
    const int i = std::stoi(argv[4]), j = std::stoi(argv[5]);
    const unsigned multiplicity = std::stoul(argv[6]);
    if ((i == 0) != (j == 0)) throw std::invalid_argument("Supply both shift legs");
    const std::optional<std::pair<int,int>> shift = i ? std::make_optional(std::make_pair(i,j)) : std::nullopt;
    const std::vector<std::string> legs(argv+7,argv+argc);
    amplitude process(legs, model(root/"models",3), model(root/"models",4), generation_references::physical);
    process.generate_factorization_products(1);
    const auto candidate = process.channel_sum_candidate(shift,multiplicity);
    std::cout << "Constructed from lower amplitudes. Validating the fixed candidate.\n" << std::flush;
    process.set_feynman_model(model(root/"tests/reference-models/calchep",2));
    process.generate_feynman_diagrams(1);
    auto result = process.validate(candidate,settings);
    for (const auto& row : result.checks)
        std::cout << row.kind << " " << row.label << " " << (row.status == check_outcome::pass ? "PASS" : "INCOMPLETE") << "\n";
    bool signed_check = false;
    for (const auto& row : result.numerical) {
        std::cout << row.kind << " compared " << row.compared << " failures " << row.failures
                  << " max relative " << row.max_relative_error << " replays " << row.replays
                  << " max replay relative " << row.max_replay_relative_error << "\n";
        if (row.kind == "Feynman" && row.compared == settings.points && row.nonzero && !row.failures)
            signed_check = true;
    }
    if (!result.passed() || !signed_check) return 2;
    std::ofstream file(output);
    file.exceptions(std::ios::badbit | std::ios::failbit);
    file << candidate.str() << '\n';
    file.close();
    return 0;
} catch (const std::exception& e) { std::cerr << e.what() << '\n'; return 1; }
