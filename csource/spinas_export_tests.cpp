// Optional output path retains the representative eeAA project for external integrations.
#include "spinas_export.hpp"
#include "spinas_open_current.hpp"
#include "spinas_spin_schedule.hpp"
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
void check_open_currents() {
    Algebra tree;
    for (bool probe_on_left : {false, true}) {
        std::vector<Id> sums;
        for (int probe = 6; probe <= 10; ++probe) {
            std::vector<Id> terms;
            for (int source = 1; source <= 4; ++source) {
                auto spinor = [&](int i) {
                    return tree.parse("Spinor[\"Spin\",\"Angle\"," + std::to_string(i) + "]");
                };
                auto left = spinor(probe_on_left ? probe : source);
                auto right = spinor(probe_on_left ? source : probe);
                terms.push_back(tree.parse("SpinorChain[" + tree.format(left) + ",Mom[2],Mom[3]," +
                                           tree.format(right) + "]"));
            }
            sums.push_back(tree.add(terms));
        }
        auto currents = spinas_detail::find_open_currents(tree, tree.mul(sums));
        if (currents.size() != sums.size())
            throw std::runtime_error("Missed repeated weighted Weyl sums");
        for (auto sum : sums)
            if (currents.at(sum).probe_on_left != probe_on_left || currents.at(sum).terms.size() != 4)
                throw std::runtime_error("Incorrect probe orientation or truncated Weyl sum");
        if (!spinas_detail::find_open_currents(tree, sums.front()).empty())
            throw std::runtime_error("Isolated Weyl sum has no reusable probes");
    }
}
void check_spin_schedule() {
    // v2 is a two-spin current. It must survive both choices of spin 5.
    const std::vector<std::string> rhs{
        "chains[0][indices[1][0] > 0]", "chains[1][indices[3][0] > 0]",
        "v0 + v1", "v2 * v1", "v3 + v2", "v4 * chains[2][indices[5][0] > 0]", "(v5)"};
    auto plan = spinas_detail::schedule_spins(rhs, {1,3,5});
    if (plan.legs.back() != 5 || plan.stage[2] != 2 || plan.stage[4] != 2 ||
        plan.shared[4] < 0 || plan.assignments >= rhs.size()*8 ||
        spinas_detail::scheduled_rhs(rhs[5], plan, 3).find("shared[") != 0)
        throw std::runtime_error("Failed to preserve a multi-spin current across the spectator loop");
}
} // namespace
int main(int argc, char** argv) {
    try {
        if (argc < 3 || argc > 4)
            throw std::invalid_argument("usage: test_spinas_export MODELS EXPORT_TEMPLATES [OUTPUT]");
        check_color_tensors();
        check_spin_schedule();
        check_open_currents();
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
