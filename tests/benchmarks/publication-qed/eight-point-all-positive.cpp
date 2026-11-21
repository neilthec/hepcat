// Signed replay of the published eight-point summands against the 720
// off-shell Feynman diagrams. Build and run as in eight-point-all-positive.md.
#include "generation_feynman.hpp"
#include "permutation_sum.hpp"
#include "madgraph-runtime.hpp"
#include "radiation_kinematics.hpp"
#include "validation_feynman.hpp"
#include "validation_numeric.hpp"
#include "validation_precision.hpp"

#include <algorithm>
#include <cmath>
#include <iostream>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

using namespace hepcat;
using namespace hepcat::validation;

namespace {
class paper : private detail::radiation_kinematics {
    Id e;
    Id coupling(unsigned denominator) {
        return ar.mul({ar.exact(mpq_class(1,denominator)),mass,ar.pow(e,6),chain(false,1,2)});
    }
    Id x(int i) { return over(chain(true,i,i,{{1},{2}}),{tau({2,i})}); }
    Id h(int i) { return chain(true,i,3,{{1}}); }
    Id b(int i,int j) { return ar.pow(ar.mul({mass,chain(true,i,j)}),2); }
public:
    paper(Algebra& a,Id m,Id charge) : radiation_kinematics(a,m,true,true),e(charge) {}
    Id massive_spin_factor() { return chain(false,1,2); }
    Id physical_seed() {
        const Id u = ar.mul({chain(true,5,7),chain(true,7,5,{{1}})});
        const Id v = chain(true,8,8,{{1},{5,6,7}});
        const Id bracket = ar.add({neg(ar.mul({x(7),x(8)})),over(b(7,8),{tau({2,3,4})}),
            over(ar.mul({ar.integer(4),u,minus(x(8),over(v,{tau({2,3,4})}))}),{tau({1,5,6,7})})});
        const Id leading = over(ar.mul({x(5),x(6),x(7),x(8)}),{tau({1,3}),tau({1,4})});
        const Id correction = over(ar.mul({ar.integer(6),b(5,6),bracket}),{tau({2,3}),tau({2,4}),tau({1,5,6})});
        return over(ar.mul({coupling(6),ar.pow(chain(true,3,4),2),ar.add({leading,correction})}),
                    {tau({1,5}),tau({1,6}),tau({1,7}),tau({1,8})});
    }
    Id spurious_seed() {
        const Id mass_pair = over(ar.mul({ar.integer(6),b(7,8)}),{tau({2,3,4})});
        const Id leading = over(ar.mul({h(5),h(6),minus(over(ar.mul({h(7),h(8)}),
                     {chain(false,3,7),chain(false,3,8)}),mass_pair)}),
                     {chain(false,3,4),tau({1,5}),tau({1,6})});
        const Id shift = ar.add({chain(true,4,6),over(chain(true,6,3,{{2}}),{chain(false,3,4)})});
        const Id emission = ar.add({over(h(5),{tau({1,5})}),
            over(minus(h(5),ar.mul({ar.integer(2),chain(false,3,7),chain(true,5,7)})),{tau({1,7,8})})});
        const Id correction = over(ar.mul({ar.integer(2),mass_pair,shift,emission}),{tau({1,5,7,8})});
        return over(ar.mul({coupling(3),chain(true,3,4),ar.add({leading,correction})}),
                    {tau({1,7}),tau({1,8}),chain(false,3,5),chain(false,3,6),tau({2,3})});
    }
    Id ordered_seed() {
        return over(ar.mul({ar.integer(8),mass,ar.pow(e,6),chain(false,1,2),chain(true,7,8),
                     chain(false,7,3,{{1}}),chain(false,7,4,{{1,3}}),
                     chain(false,7,5,{{1,3,4}}),neg(chain(false,7,6,{{2,8}}))}),
                    {chain(false,7,8),tau({2,7}),chain(false,7,3),chain(false,7,4),
                     chain(false,7,5),chain(false,7,6),tau({1,3}),tau({1,3,4}),tau({1,3,4,5}),tau({2,7,8})});
    }
};

struct comparison {
    const char* name;
    expression_evaluator evaluate;
    real raw = 0, replay = 0;
    unsigned rechecks = 0, failed = 0;
};
}

int main(int argc,char** argv) {
    try {
        if (argc > 3 && argc != 6)
            throw std::invalid_argument("usage: eight-point-replay [points [seed [MG-library mass charge]]]");
        const bool mg = argc == 6;
        const unsigned points = argc > 1 ? std::stoul(argv[1]) : 100000;
        const auto seed = argc > 2 ? std::stoull(argv[2]) : 806191;
        if (!points) throw std::invalid_argument("positive sample count required");
        model vertices("models",3), rules("tests/reference-models/calchep",2);
        Algebra a(&vertices.algebra()), fa(&rules.algebra());
        const auto pars = mg ? model_parameters(vertices,{{"Me",std::stold(argv[4])},{"EE",std::stold(argv[5])}})
                             : model_parameters(vertices);
        std::vector<std::string> names{"e","E","A.+","A.+","A.+","A.+","A.+","A.+"};
        std::vector<Id> masses, fm, trees;
        std::vector<real> numerical;
        for (auto& name : names) {
            masses.push_back(a.parse(vertices.particle(name).mass));
            numerical.push_back(evaluate_scalar(a,masses.back(),pars).real());
            fm.push_back(fa.parse(rules.particle(generation::feynman_external(rules,name)).mass));
        }
        a.setKinematics(masses,{});
        fa.setKinematics(fm,{});
        std::optional<expression_evaluator> feynman;
        std::optional<hepcat_spinas::madgraph_detail::Reference> madgraph;
        if (mg) madgraph.emplace(argv[3]);
        else {
            for (const auto& plan : generation::diagram_plans(rules,names))
                trees.push_back(feynman_to_spinors(fa,generation::sew_feynman(fa,rules,names,plan)));
            if (trees.size() != 720) throw std::runtime_error("expected 720 diagrams");
            feynman.emplace(fa,fa.add(trees),model_parameters(rules));
        }
        const auto charge = std::find_if(vertices.variables().begin(),vertices.variables().end(),
                                         [](const auto& p){return p.name=="EE";});
        if (charge==vertices.variables().end()) throw std::runtime_error("missing model charge");
        paper formula(a,masses[0],charge->symbol);
        std::vector<comparison> tests;
        for (auto [label,kernel] : {std::pair{"physical",formula.physical_seed()},
                                    std::pair{"spurious",formula.spurious_seed()},
                                    std::pair{"ordered spurious",formula.ordered_seed()}}) {
            // Each printed term has exactly one massive spin bracket. Its four
            // squared components can be summed once at each momentum.
            if (mg) kernel = a.mul({kernel,a.pow(formula.massive_spin_factor(),-1)});
            auto sum = expand_permutation_groups(a,kernel,{{3,4,5,6,7,8}});
            tests.push_back({label,expression_evaluator(a,sum,pars)});
        }
        std::cout << (mg ? "Comparing spin sums with live MadGraph at " : "Comparing signed Feynman amplitudes at ") << points << " points, seed " << seed << std::endl;
        for (unsigned i=0;i<points;++i) {
            const auto point = sample_point(numerical,seed+i);
            if (mg) {
                std::vector<double> momenta;
                for (unsigned leg=0; leg<names.size(); ++leg)
                    for (auto component : point.momenta[leg])
                        momenta.push_back(leg < 2 ? component : -component);
                std::vector<int> helicities(names.size(),1);
                real rate = 0, spin_sum = 0;
                for (int i : {0,1}) for (int j : {0,1}) {
                    helicities[0] = 2*i-1;
                    helicities[1] = 2*j-1;
                    const double term = madgraph->evaluate(momenta,helicities);
                    if (!(std::isfinite(term) && term >= 0))
                        throw std::runtime_error("invalid polarized MadGraph rate");
                    rate += term;
                    const auto& l = point.angle_basis[0][i];
                    const auto& r = point.angle_basis[1][j];
                    spin_sum += std::norm(l[0]*r[1]-l[1]*r[0]);
                }
                for (auto& test : tests) {
                    const auto value = spin_sum*std::norm(test.evaluate(point));
                    const auto scale = std::max(value,rate);
                    const auto error = scale ? std::abs(value-rate)/scale : 0;
                    test.raw = std::max(test.raw,error);
                    if (!(std::isfinite(value) && error<=1e-7L)) ++test.failed;
                }
                continue;
            }
            const auto expected = (*feynman)(point);
            for (auto& test : tests) {
                const auto value = test.evaluate(point);
                const auto scale = std::max(std::abs(expected),std::abs(value));
                const auto error = scale ? std::abs(value-expected)/scale : 0;
                test.raw = std::max(test.raw,error);
                if (std::isfinite(error) && error<=1e-8L) continue;
                ++test.rechecks;
                const auto chart = precision::reconstruct(point);
                const auto x = feynman->precise(point,chart), y = test.evaluate.precise(point,chart);
                const auto norm = std::max(x.magnitude(),y.magnitude());
                const auto relative = norm==0 ? 0. : precision::scalar((x-y).magnitude()/norm).get_d();
                test.replay = std::max(test.replay,static_cast<real>(relative));
                if (!(relative<=1e-8)) ++test.failed;
            }
        }
        bool passed = true;
        for (const auto& test : tests) {
            std::cout << test.name << ": maximum " << test.raw << ", replays " << test.rechecks
                      << ", replay maximum " << test.replay << ", failures " << test.failed << '\n';
            passed &= test.failed==0;
        }
        return passed ? 0 : 1;
    } catch (const std::exception& e) {
        std::cerr << e.what() << '\n';
        return 1;
    }
}
