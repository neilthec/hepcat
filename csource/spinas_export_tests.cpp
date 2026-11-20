// Optional output path retains the representative eeAA project for external integrations.
#include "spinas_export.hpp"
#include "reduce.hpp"
#include "validation_color.hpp"
#include <chrono>
#include <fstream>
#include <regex>
#include <iostream>
#include <stdexcept>

using namespace hepcat;
namespace fs = std::filesystem;
namespace {
void check_color_tensors() {
    using namespace validation_detail;
    Algebra a;
    const std::vector<int> reps{3, -3, 8, 8};
    auto network = [&](std::initializer_list<const char*> factors) {
        std::vector<Id> ids;
        for (auto text : factors)
            ids.push_back(a.parse(text));
        return color_network(a, reps, ids);
    };
    auto ab = network(
        {"ColorGenerator[ColorIndex[1],k,ColorIndex[3]]", "ColorGenerator[k,ColorIndex[2],ColorIndex[4]]"});
    auto ba = network(
        {"ColorGenerator[ColorIndex[1],k,ColorIndex[4]]", "ColorGenerator[k,ColorIndex[2],ColorIndex[3]]"});
    auto comm = network(
        {"ColorStructure[ColorIndex[3],ColorIndex[4],c]", "ColorGenerator[ColorIndex[1],ColorIndex[2],c]"});
    mpq_class norm = 0, interference = 0;
    for (int i = 0; i < 3; ++i)
        for (int j = 0; j < 3; ++j)
            for (int x = 0; x < 8; ++x)
                for (int y = 0; y < 8; ++y) {
                    std::vector<int> colors{i, j, x, y};
                    auto first = ab(colors), second = ba(colors);
                    if (first - second != color_number(0, 1) * comm(colors))
                        throw std::runtime_error("Incorrect SU(3) commutator");
                    mpq_class metric(1, color_basis::metric(x) * color_basis::metric(y));
                    norm += metric * (first.real() * first.real() + first.imag() * first.imag());
                    interference += metric * (std::conj(first) * second).real();
                }
    // Tr(sum_ab T_b T_a T_a T_b) and Tr(sum_ab T_b T_a T_b T_a).
    if (norm != mpq_class(16, 3) || interference != -mpq_class(2, 3))
        throw std::runtime_error("Incorrect external SU(3) normalization or interference");
}
} // namespace
int main(int argc, char** argv) {
    try {
        if (argc < 3 || argc > 4)
            throw std::invalid_argument("usage: test_spinas_export MODELS EXPORT_TEMPLATES [OUTPUT]");
        check_color_tensors();
        model m(argv[1], 4);
        std::vector<spinas_component> family;
        std::vector<std::string> masses{m.particle("e").mass, m.particle("E").mass, "0", "0"};
        Algebra a(&m.algebra());
        for (auto& v : m.vertices()) {
            if (v.legs.size() == 4 && v.legs[2].substr(0, 2) == "A.")
                family.push_back({v.legs, amp_expression(a.format(a.mul({v.factor, v.expression})), masses)});
        }
        spinas_export_options opts;
        opts.name = "eeAA";
        opts.runtime_directory = argv[2];
        opts.overwrite = true;
        const fs::path directory =
            argc == 4 ? fs::path(argv[3])
                      : fs::temp_directory_path() /
                            ("hepcat-spinas-export-" +
                             std::to_string(std::chrono::steady_clock::now().time_since_epoch().count()));
        auto project = write_spinas_process(directory, m, family, opts);
        if (project.helicities.size() != 4 || !project.complete_helicities ||
            project.normalization != 0.125 || project.symmetry_factor != 0.5 ||
            project.pdgs != std::vector<int>{11, -11, 22, 22} || project.mass_parameter_pdgs.at("Me") != 11)
            throw std::runtime_error("incorrect eeAA process metadata");
        auto subset = family;
        subset.pop_back();
        try {
            write_spinas_process(directory / "invalid", m, subset, opts);
            throw std::runtime_error("accepted missing independent photon helicities");
        } catch (const std::invalid_argument&) {
        }
        opts.fixed_helicities = {{2, 2}, {3, 2}};
        auto selected = write_spinas_process(directory / "polarized", m, {family.front()}, opts);
        if (selected.complete_helicities || selected.helicities.size() != 1)
            throw std::runtime_error("incorrect fixed-helicity metadata");
        // A compound mass/width must compile in set_momenta, where the
        // propagator lives. It cannot reference a helicity-branch temporary.
        auto unstable = family.front();
        unstable.expression =
            amp_expression("Times[" + unstable.expression.str() +
                               ",Power[PropDen[Mom[Multiparticle[1,2]],Plus[1,Me],Times[2,Me]],-1]]",
                           masses);
        auto unstable_dir = directory / "finite-width";
        write_spinas_process(unstable_dir, m, {unstable}, opts);
        std::ifstream generated(unstable_dir / "process.cpp");
        std::string code((std::istreambuf_iterator<char>(generated)), {});
        auto before = code.find("::set_momenta("), after = code.find("\n}", before);
        if (before == std::string::npos || after == std::string::npos)
            throw std::runtime_error("generated set_momenta is missing");
        auto body = code.substr(before, after - before);
        if (body.find("widths_enabled ? cdouble(0, 1)") == std::string::npos ||
            std::regex_search(body, std::regex("\\bv[0-9]+\\b|\\bScalar\\b")))
            throw std::runtime_error("invalid finite-width cache expression");
        if (argc == 3)
            fs::remove_all(directory);
        std::cout << "Native SPINAS family emission passed.\n";
    } catch (const std::exception& e) {
        std::cerr << e.what() << '\n';
        return 1;
    }
}
