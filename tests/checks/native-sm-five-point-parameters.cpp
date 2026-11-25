// The same analytic five-point expression must work when the lepton masses
// coincide and when the weak mixing angle changes.
#include "amplitude.hpp"
#include <filesystem>
#include <iostream>

using namespace hepcat;

int main(int argc, char** argv) try {
    if (argc != 2) throw std::invalid_argument("Expected the HEPCAT repository root");
    const std::filesystem::path root(argv[1]);
    const model vertices(root / "models", 1, "SM"), channels(root / "models", 2, "SM");
    amplitude process({"e", "E", "m", "M", "h"}, vertices, channels);
    construct_options options;
    options.workers = 1;
    options.validation.seed = 52731;
    const auto amplitude = process.construct(options);
    process.set_feynman_model(model(root / "tests/reference-models/calchep", 1, "Feynman"));
    for (long double muon_mass : {0.0005L, 0.23L}) {
        validation_options check = options.validation;
        check.workers = 1;
        check.parameters = {{"Me", 0.0005L}, {"Mm", muon_mass}, {"SW", 0.41L}};
        const auto report = process.validate(amplitude, check);
        bool signed_match = false;
        for (const auto& row : report.numerical) {
            if (row.kind == "Feynman") {
                signed_match = row.status == check_outcome::pass && row.compared == check.points;
                std::cout << "Muon mass " << muon_mass << ", " << row.compared << " signed points, "
                          << row.failures << " mismatches.\n";
            }
        }
        if (!signed_match || !report.passed()) return 1;
    }
    return 0;
} catch (const std::exception& error) {
    std::cerr << error.what() << '\n';
    return 1;
}
