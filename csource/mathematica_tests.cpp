#include "amp_expression.hpp"
#include "model.hpp"
#include "reduce.hpp"

#include <fstream>
#include <iostream>
#include <stdexcept>
#include <string>

using namespace hepcat;

namespace {

void require(bool ok, const char* message) {
    if (!ok) throw std::runtime_error(message);
}

void expression_tests() {
    const std::string mass = R"(ModelParameter["Me","scope one","Real"])";
    const std::string explicit_mass = R"(ConstructiveDiagrams`ModelParameter["Me", "scope one", "Real"])";
    const std::string raw = R"(SpinorChain[Spinor["Spin","Upper","Square",1,MyModel`s],Mom[3],Spinor["Helicity","Angle",4]])";
    amp_expression expr(raw, {mass, mass, "0", "0"}, {{{1, 3}, mass}});
    const std::string chain = R"(ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor["Spin", "Upper", "Square", 1, MyModel`s], ConstructiveDiagrams`Mom[3], ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]])";
    require(expr.mathematica(false) == chain, "spinor chirality, insertion, or spin index");
    require(expr.mathematica() == "{{ConstructiveDiagrams`Mass[1] -> " + explicit_mass +
        ", ConstructiveDiagrams`Mass[2] -> " + explicit_mass +
        ", ConstructiveDiagrams`Mass[3] -> 0, ConstructiveDiagrams`Mass[4] -> 0, "
        "ConstructiveDiagrams`Mass[ConstructiveDiagrams`Multiparticle[1, 3]] -> " +
        explicit_mass + "},\n " + chain + "}", "external and diagram-local mass rules");
    require(expr.str() == raw, "export mutated the input");

    amp_expression color("ColorGenerator[ColorIndex[1],colorInternalIndex[List[2,3],3],Other`ColorIndex[4]]", {"0"});
    require(color.mathematica(false) ==
        "ConstructiveDiagrams`ColorGenerator[ConstructiveDiagrams`ColorIndex[1], "
        "ConstructiveDiagrams`Private`colorInternalIndex[{2, 3}, 3], Other`ColorIndex[4]]",
        "color index scope or argument order");
    amp_expression symbols("Plus[g,ModelParameter[1,\"EE\"],Rational[-1,7],1e-4,Abc`x]", {"0"});
    require(symbols.mathematica(false) ==
        "System`Plus[HEPCATNative`Symbols`g, ConstructiveDiagrams`ModelParameter[1, \"EE\"], "
        "System`Rational[-1, 7], 1.*^-4, Abc`x]", "source symbols or literal types");
    bool rejected = false;
    try { amp_expression("alpha_beta", {"0"}).mathematica(); }
    catch (const std::invalid_argument&) { rejected = true; }
    require(rejected, "a Wolfram pattern token was exported as a literal symbol");
}

// Write lower-point amplitudes and their tagged masses for the optional licensed
// comparison to ReadModel in mathematica_roundtrip.wls.
void export_model(const char* directory, unsigned number, const std::string& scope, const char* path) {
    model m(directory, number, scope);
    std::ofstream out(path);
    out.exceptions(std::ios::badbit | std::ios::failbit);
    out << "{";
    bool first = true;
    for (const auto& vertex : m.vertices()) {
        std::vector<std::string> masses;
        for (const auto& leg : vertex.legs) masses.push_back(m.particle(leg).mass);
        const auto& a = m.algebra();
        amp_expression expr("Times[" + a.format(vertex.factor) + "," + a.format(vertex.expression) + "]", masses);
        if (!first) out << ",\n";
        out << expr.mathematica();
        first = false;
    }
    out << "}\n";
    out.close();
}

} // namespace

int main(int argc, char** argv) {
    try {
        expression_tests();
        if (argc == 6 && std::string(argv[1]) == "--export-model")
            export_model(argv[2], std::stoul(argv[3]), argv[4], argv[5]);
        else if (argc != 1)
            throw std::invalid_argument("Expected --export-model DIRECTORY NUMBER SCOPE FILE");
        return 0;
    } catch (const std::exception& e) {
        std::cerr << e.what() << '\n';
        return 1;
    }
}
