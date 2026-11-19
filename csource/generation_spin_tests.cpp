#include "generation_spin.hpp"
#include "generation_sew.hpp"
#include <chrono>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <stdexcept>
using namespace hepcat;
using namespace hepcat::generation;

// A model may store an explicit polarization reference in a lower amplitude.
void stored_reference_test() {
    namespace fs = std::filesystem;
    const auto dir = fs::temp_directory_path() /
                     ("hepcat-stored-reference-" +
                      std::to_string(std::chrono::steady_clock::now().time_since_epoch().count()));
    if (!fs::create_directory(dir)) throw std::runtime_error("could not create model fixture");
    struct cleanup {
        fs::path path;
        ~cleanup() {
            std::error_code e;
            fs::remove_all(path, e);
        }
    } guard{dir};
    std::ofstream(dir / "vars1.mdl") << "m | 2 | mass\n======\n";
    std::ofstream(dir / "func1.mdl") << "======\n";
    std::ofstream(dir / "prtcls1.mdl") << "Fermion | q | Q | 1 | 1 | m | 0 | 1 | | q | Q\n"
                                          "Photon | A.+ | A.- | 22 | 2 | 0 | 0 | 1 | | A | A\n======\n";
    std::ofstream(dir / "amps1.mdl")
        << "q|Q|A.+|A.-|1|SpinorChain[Spinor[\"Xi\",\"Angle\",1],Mom[2],Spinor[\"Helicity\",\"Square\",3]]/"
           "SpinorChain[Spinor[\"Helicity\",\"Angle\",3],Spinor[\"Xi\",\"Angle\",1]]\n"
           "q|Q|A.+||1|SpinorChain[Spinor[\"Xi\",\"Angle\",1],Mom[2],Spinor[\"Helicity\",\"Square\",3]]/"
           "SpinorChain[Spinor[\"Helicity\",\"Angle\",3],Spinor[\"Xi\",\"Angle\",1]]\n======\n";
    model m(dir, 1);
    const std::vector<std::string> p{"q", "Q", "A.+", "A.-"};
    plan row{{term{{vertex{0, {1, 2, 3, 4}}}, {}, 1}}};
    Algebra a(&m.algebra());
    const std::vector<std::string> cubic{"q", "Q", "A.+"};
    plan bare{{term{{vertex{1, {1, 2, 3}}}, {}, 1}}};
    auto explicit_ref = sew(a, m, cubic, bare, false, false);
    if (explicit_ref != sew(a, m, cubic, bare, false, true) ||
        explicit_ref != sew(a, m, cubic, bare, false, true, true))
        throw std::runtime_error("singular external replacement for a stored three-point reference");
    auto old = sew(a, m, p, row, false, false);
    if (a.format(old).find("\"Xi\"") == std::string::npos)
        throw std::runtime_error("lost explicit reference in keep mode");
    auto cycle = sew(a, m, p, row, false, true);
    const auto form = a.format(cycle);
    if (form.find("\"Xi\"") != std::string::npos ||
        form.find("Spinor[\"Helicity\",\"Angle\",4]") == std::string::npos)
        throw std::runtime_error("stored reference was not mapped to the next photon");
    auto physical = sew(a, m, p, row, false, true, true);
    auto expected = a.parse("Times[-1,SpinorChain[Spinor[\"Helicity\",\"Square\",3],Mom[1],Mom[2],Spinor["
                            "\"Helicity\",\"Square\",3]],Power[SpinorChain[Spinor[\"Helicity\",\"Angle\",3]"
                            ",Mom[1],Spinor[\"Helicity\",\"Square\",3]],-1]]");
    if (physical != expected)
        throw std::runtime_error("wrong momentum or sign for a right-end physical reference: " +
                                 a.format(physical));
}
int main() {
    try {
        stored_reference_test();
        Algebra a;
        Id q = a.parse("Multiparticle[1,2]"), mass = a.atom("m");
        Id l = a.parse("Spinor[\"Spin\",\"Angle\",1]"), r = a.parse("Spinor[\"Spin\",\"Angle\",3]");
        Id rs = a.parse("Spinor[\"Spin\",\"Square\",3]");
        Id upper = a.parse("Spinor[\"Spin\",\"Upper\",\"Angle\",Multiparticle[1,2],1]");
        Id lower = a.parse("Spinor[\"Spin\",\"Lower\",\"Angle\",Multiparticle[1,2],1]");
        Id ls = a.parse("Spinor[\"Spin\",\"Lower\",\"Square\",Multiparticle[1,2],1]");
        auto chain = [&](std::initializer_list<Id> word) { return a.call("SpinorChain", word); };
        auto check = [&](Id actual, Id expected, const char* why) {
            if (actual != expected) throw std::runtime_error(std::string(why) + ": " + a.format(actual));
        };
        Id lq = chain({l, upper}), qr = chain({lower, r}), mr = chain({l, r});
        check(sew_spin(a, a.mul({lq, qr}), q, mass, 1), a.mul({mass, mr}), "mass contraction");
        check(sew_spin(a, a.mul({lq, chain({r, lower})}), q, mass, 1), a.mul({a.integer(-1), mass, mr}),
              "antisymmetry reversing a chain");
        check(sew_spin(a, a.pow(a.mul({lq, qr}), 2), q, mass, 2), a.mul({a.pow(mass, 2), a.pow(mr, 2)}),
              "normalization with two identical vector spin contractions");
        Id momentum = a.call("Mom", {q});
        check(sew_spin(a, a.mul({lq, chain({ls, rs})}), q, mass, 1), chain({l, momentum, rs}),
              "momentum sewing");
        Id qang = a.parse("Spinor[\"Helicity\",\"Angle\",Multiparticle[1,2]]");
        Id qsqr = a.parse("Spinor[\"Helicity\",\"Square\",Multiparticle[1,2]]");
        Id pair = a.mul({chain({l, qang}), chain({qsqr, rs})});
        auto pole_result = sew_null_spin(a, a.pow(pair, -2), q, {3, 4}, true);
        Id forward = a.pow(chain({l, momentum, rs}), -2), reverse = a.pow(chain({rs, momentum, l}), -2);
        if (pole_result != forward && pole_result != reverse)
            throw std::runtime_error("null denominator powers");
        bool rejected = false;
        try {
            sew_null_spin(a, chain({l, qang}), q, {3, 4}, true);
        } catch (const std::invalid_argument&) {
            rejected = true;
        }
        if (!rejected) throw std::runtime_error("unbalanced internal helicity was accepted");
        std::cout << "Massive spin and null helicity sewing tests passed.\n";
    } catch (const std::exception& e) {
        std::cerr << e.what() << '\n';
        return 1;
    }
}
