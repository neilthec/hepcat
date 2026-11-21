// Export the two photon helicities from the expanded paper formula and compare
// on common momenta. Build and run instructions are in seven-point-six-fermions.md.
#include "external_export.hpp"
#include "spinas_export.hpp"
#include <filesystem>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <sstream>

namespace fs = std::filesystem;
using namespace hepcat;

int main(int argc, char** argv) try {
    if (argc < 6 || argc > 8) {
        std::cerr << "Usage: replay-external FORMULA_DIR OUTPUT_DIR SPINAS_DIR "
                     "(--madgraph MG_DIR | --calchep CH_DIR) [POINTS [SEED]]\n";
        return 2;
    }
    const std::string engine = argv[4];
    if (engine != "--calchep" && engine != "--madgraph")
        throw std::invalid_argument("Choose --calchep or --madgraph");
    model qed("models", 3);
    std::vector<spinas_component> components;
    for (const std::string h : {"+", "-"}) {
        const auto file = fs::path(argv[1]) / (h + ".expr");
        std::ifstream source(file);
        if (!source) throw std::runtime_error("Cannot open " + file.string());
        std::ostringstream expression;
        expression << source.rdbuf();
        const std::vector<std::string> legs{"e", "E", "e", "E", "e", "E", "A." + h};
        std::vector<std::string> masses;
        for (const auto& leg : legs) masses.push_back(qed.particle(leg).mass);
        components.push_back({legs, amp_expression(expression.str(), masses)});
    }
    spinas_export_options out;
    out.name = "eeeeeeA";
    out.spinas_directory = fs::absolute(argv[3]);
    out.runtime_directory = fs::absolute("source/internal/export");
    const auto directory = fs::absolute(argv[2]);
    fs::create_directories(directory.parent_path());
    const auto process = write_spinas_process(directory, qed, components, out);

    external_options options;
    options.points = argc > 6 ? std::stoull(argv[6]) : 100000;
    options.seed = argc > 7 ? std::stoull(argv[7]) : 706181;
    if (!options.points) throw std::invalid_argument("Choose a positive point count");
    options.allowed_pdgs = {11, 22};
    options.runtime_directory = out.runtime_directory;
    options.run_calchep = engine == "--calchep";
    options.run_madgraph = engine == "--madgraph";
    if (options.run_calchep) {
        options.calchep_directory = fs::absolute(argv[5]);
        options.calchep_model_directory = fs::absolute("tests/reference-models/calchep");
        options.calchep_model_number = 2;
    } else {
        options.madgraph_directory = fs::absolute(argv[5]);
        options.madgraph_model = "sm-full";
        std::ifstream launcher(options.madgraph_directory / "bin/mg5_aMC");
        std::string shebang;
        if (std::getline(launcher, shebang) && shebang.rfind("#!/", 0) == 0 &&
            fs::is_regular_file(shebang.substr(2)))
            options.python_executable = shebang.substr(2);
    }
    options.progress = [](const auto& message) { std::cout << message << std::endl; };
    write_external_project(process, qed, options);
    return validate_external_project(process, options).passed() ? 0 : 1;
} catch (const std::exception& e) {
    std::cerr << e.what() << '\n';
    return 2;
}
