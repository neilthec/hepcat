#include "model.hpp"
#include "reduce.hpp"
#include <chrono>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <map>
#include <stdexcept>

namespace fs = std::filesystem;
using namespace hepcat;
namespace {
void check(bool ok, const std::string& label) {
    if (!ok) throw std::runtime_error(label);
}
template <class F> void rejects(F f, const std::string& label) {
    try {
        f();
    } catch (const std::exception&) {
        return;
    }
    throw std::runtime_error(label);
}
struct fixture {
    fs::path dir =
        fs::temp_directory_path() /
        ("hepcat-model-tests-" + std::to_string(std::chrono::steady_clock::now().time_since_epoch().count()));
    fixture() {
        if (!fs::create_directory(dir)) throw std::runtime_error("could not create model fixture");
    }
    ~fixture() {
        std::error_code error;
        fs::remove_all(dir, error);
    }
    void write(const std::string& name, const std::string& body) {
        std::ofstream f(dir / name);
        if (!(f << body)) throw std::runtime_error("could not write fixture");
    }
};
void fixture_tests() {
    fixture test;
    test.write("vars1.mdl",
               "Name | Value | Comment\nmass | .05e+1 | pole mass\ng | -2*^-1 | coupling\n======\n");
    test.write(
        "func1.mdl",
        "Name | Expression\ny | 2*g^2 % ordinary arithmetic\nw | initQCD5(g,g,g,g)\na | b\nb | a\n======\n");
    test.write("prtcls1.mdl", "Fermion | q | Q | 1 | 1 | mass | 0 | 1 | | q | Q\n"
                              "Photon | A.+ | A.- | 22 | 2 | 0 | 0 | 1 | | A | A\n======\n");
    test.write("amps1.mdl", "P1 | P2 | P3 | P4 | Factor | Amplitude/Factor\n"
                            "% note | q | Q | | 0 | ???\nq | Q | A.+ | | -Sqrt[2]*g | x12<12>\n"
                            "P1 | P2 | P3 | P4 | P5 | Factor | Amplitude/Factor\n"
                            "q | Q | A.+ | A.+ | A.- | 2g^2 | <135]/(s13-mass^2)\n======\n");
    model m(test.dir, 1);
    check(m.particle("Q").spin2 == 1 && m.antiparticle("Q") == "q" && m.antiparticle("A.+") == "A.-",
          "particle lookup");
    check(m.vertices().size() == 2 && m.vertices()[1].legs.size() == 5,
          "variable-width vertex sections and comment handling");
    check(m.algebra().format(m.variables()[0].definition) == "Rational[1,2]" &&
              m.algebra().format(m.variables()[1].definition) == "Rational[-1,5]",
          "exact decimals");
    check(m.functions()[0].comment == "ordinary arithmetic", "function comment");
    auto param = [&](std::string n) {
        return "ModelParameter[\"" + n + "\",\"" + m.parameter_scope() + "\",\"Real\"]";
    };
    const auto ps = m.high_energy_parameter_points({param("g"), param("y")});
    check(ps.size() == 3 && ps[0] != ps[1], "independent high-energy model points");
    Algebra numeric;
    for (const auto& p : ps) {
        mpq_class g, y;
        check(numeric.number(numeric.parse(p[0].second), g) &&
                  numeric.number(numeric.parse(p[1].second), y) && y == 2 * g * g,
              "derived parameter preserved at sample");
    }
    rejects([&] { m.high_energy_parameter_points({param("unknown")}); }, "undeclared parameter accepted");
    rejects([&] { m.high_energy_parameter_points({param("a")}); }, "cyclic definition accepted");
    rejects([&] { m.high_energy_parameter_points({param("w")}); },
            "external parameter function treated as a free number");
    check(m.high_energy_parameter_points({"ModelParameter[\"g\",\"other\",\"Real\"]"}).empty(),
          "foreign model parameter adopted");
    check(model(test.dir / ".", 1).parameter_scope() == m.parameter_scope(), "equivalent path scopes");
    const std::vector<std::string> ordered{"q","Q","A.+","A.+","A.-"};
    auto exact = m.retrieve_amplitude(ordered);
    Algebra a(&m.algebra());
    check(exact && exact->str() == a.format(a.mul({m.vertices()[1].factor, m.vertices()[1].expression})),
          "exact catalog lookup omitted the factor or modified the row");
    const std::vector<std::string> permuted{"A.+","A.-","Q","A.+","q"};
    auto moved = m.retrieve_amplitude(permuted);
    test.write("amps1.mdl", "A.+|A.-|Q|A.+|q|2g^2|<512]/(s15-mass^2)\n======\n");
    auto oracle = model(test.dir, 1, m.parameter_scope()).retrieve_amplitude(permuted);
    check(moved && oracle && moved->str() == oracle->str() && moved->particle_count() == 5,
          "permuted lookup changed the momentum order, masses, or repeated photon slots");
    check(!m.retrieve_amplitude({"q","Q","A.-"}), "missing helicity returned a value");
    test.write("amps1.mdl", "q|Q|A.+||1|0\nQ|q|A.+||1|3\n======\n");
    model alternatives(test.dir, 1);
    check(alternatives.retrieve_amplitude({"q","Q","A.+"})->str() == "0", "stored zero lost");
    check(alternatives.retrieve_amplitude({"Q","q","A.+"})->str() == "3", "exact row lost priority");
    rejects([&] { alternatives.retrieve_amplitude({"A.+","q","Q"}); }, "ambiguous lookup accepted");
    // Typed external indices move with their particle. The spin slot, scalar
    // factor and summed color index each use integers with a different meaning.
    test.write("amps1.mdl",
        "q|Q|A.+||1|ColorGenerator[ColorIndex[1],ColorIndex[2],ColorInternalIndex[1,8]]"
        "*SpinorChain[Spinor[\"Spin\",\"Angle\",1,VectorSpinIndex[1,2]],Spinor[\"Spin\",\"Angle\",2]]\n======\n");
    model indexed(test.dir, 1, m.parameter_scope());
    auto with_indices = indexed.retrieve_amplitude({"A.+","q","Q"});
    test.write("amps1.mdl",
        "A.+|q|Q||1|ColorGenerator[ColorIndex[2],ColorIndex[3],ColorInternalIndex[1,8]]"
        "*SpinorChain[Spinor[\"Spin\",\"Angle\",2,VectorSpinIndex[2,2]],Spinor[\"Spin\",\"Angle\",3]]\n======\n");
    auto expected_indices = model(test.dir, 1, m.parameter_scope()).retrieve_amplitude({"A.+","q","Q"});
    check(with_indices->str() == expected_indices->str(), "indexed color or spin slot changed meaning");
    auto original_scope = m.parameter_scope();
    test.write("func1.mdl", "Name | Expression\ny | 3*g^2\n======\n");
    check(model(test.dir, 1).parameter_scope() != original_scope,
          "changed definition reused a parameter scope");
    test.write("amps1.mdl", "q|Q|A.+||1|Shared[<13>;g;*:1,2;+:3,3;2;^:4,5]\n"
                            "q|Q|A.-||1|(2*g*<13>)^2\n======\n");
    const model shared(test.dir, 1);
    check(shared.vertices()[0].expression == shared.vertices()[1].expression,
          "shared model expression differs from ordinary model algebra");
    for (const std::string bad : {"Shared[1;+:1,2]", "Shared[1;*:0,1]", "Shared[1;?:1,1]",
                                  "Shared[1;^:1,1,1]", "Shared[1;;1]", "Shared[1;]", "Shared[]"}) {
        test.write("amps1.mdl", "q|Q|A.+||1|" + bad + "\n======\n");
        rejects([&] { model invalid(test.dir, 1); }, "accepted invalid model expression " + bad);
    }
    test.write("amps1.mdl", "q|Q|A.+||1|<19>\n======\n");
    rejects([&] { model bad(test.dir, 1); }, "invalid compact spinor slot accepted");
    test.write("amps1.mdl", "q|Q|A.+||1|Run[123]\n======\n");
    rejects([&] { model bad(test.dir, 1); }, "unrecognized expression accepted");
    test.write("amps1.mdl", "q|Q|A.+||1|<12>\n");
    rejects([&] { model bad(test.dir, 1); }, "missing terminator accepted");

    test.write("vars2.mdl", "g|2|coupling\n");
    test.write("func2.mdl", "Name|Expression\n");
    test.write("prtcls2.mdl", "scalar|h|h|25|0|0|0|1||h|h\n");
    auto feynman_term = [&](const std::string& e) {
        test.write("lgrng2.mdl", "h|h|h||1|" + e + "\n");
        return model(test.dir, 2);
    };
    auto derivative = feynman_term("(p1.p2)^2");
    check(derivative.vertices().size() == 1, "contracted scalar-product power");
    for (auto invalid : {"1/G(p1)", "sqrt(p1.p2)", "2^G(p1)"})
        rejects([&] { feynman_term(invalid); }, "non-scalar Dirac operation accepted");

}
void repository_tests(const fs::path& repo) {
    auto dir = repo / "models";
    std::vector<model> models;
    for (unsigned i = 1; i <= 5; ++i)
        models.emplace_back(dir, i);
    check(models[0].parameter_scope() == models[1].parameter_scope() &&
              models[2].parameter_scope() == models[3].parameter_scope() &&
              models[3].parameter_scope() == models[4].parameter_scope() &&
              models[0].parameter_scope() != models[2].parameter_scope(),
          "shared and isolated model definitions");
    for (const auto& m : models)
        check(!m.is_feynman() && !m.vertices().empty(), "constructive model load");
    const auto& sm = models.front();
    std::vector<std::string> active;
    for (auto table : {&sm.variables(), &sm.functions()})
        for (const auto& p : *table)
            if (p.name == "MW" || p.name == "MZ" || p.name == "SW" || p.name == "CW")
                active.push_back(sm.algebra().format(p.symbol));
    check(active.size() == 4, "electroweak parameters");
    for (const auto& point : sm.high_energy_parameter_points(active)) {
        Algebra a;
        std::map<std::string, mpq_class> values;
        for (const auto& binding : point) {
            auto param = a.node(a.parse(binding.first));
            mpq_class v;
            check(a.number(a.parse(binding.second), v), "non-rational electroweak binding");
            values.emplace(a.node(param.args.front()).head, v);
        }
        auto sw = values.at("\"SW\""), cw = values.at("\"CW\"");
        check(sw * sw + cw * cw == 1 && values.at("\"MW\"") == values.at("\"MZ\"") * cw,
              "electroweak model identity");
    }
    auto ch = repo / "tests" / "reference-models" / "calchep";
    model ew(ch, 1);
    model qed(ch, 2);
    rejects([&] { qed.retrieve_amplitude({"e","E","A"}); }, "Feynman vertex returned as a catalog row");
    check(ew.is_feynman() && qed.is_feynman() && qed.vertices().size() == 1, "Feynman model detection");
    check(ew.particle("G.t").auxiliary == "T" && ew.particle("G.t").spin2 == 4 &&
              ew.antiparticle("G.t") == "G.t",
          "implicit contact auxiliary");
    check(qed.algebra().format(qed.vertices()[0].expression) ==
              "ConstructiveDiagrams`DiracWord[ConstructiveDiagrams`DiracGamma[ConstructiveDiagrams`"
              "LorentzIndex[3]]]",
          "ordered gamma matrix vertex");
}
} // namespace
int main(int argc, char** argv) {
    try {
        // The licensed model_reader_oracle.wls compares these exact expressions
        // against Mathematica using the model's existing parameter scope.
        if (argc == 6 && std::string(argv[1]) == "--export") {
            model m(argv[2], std::stoul(argv[3]), argv[4]);
            std::ofstream out(argv[5]);
            for (const auto& v : m.vertices()) {
                for (const auto& l : v.legs)
                    out << l << " ";
                out << '\t' << m.algebra().format(v.factor) << '\t' << m.algebra().format(v.expression)
                    << '\n';
            }
            if (!out) throw std::runtime_error("cannot export oracle comparison");
            return 0;
        }
        fixture_tests();
        auto env = std::getenv("HEPCAT_ROOT");
        repository_tests(env ? fs::path(env) : fs::path(__FILE__).parent_path().parent_path());
        std::cout << "Native model tests passed.\n";
    } catch (const std::exception& e) {
        std::cerr << e.what() << '\n';
        return 1;
    }
}
