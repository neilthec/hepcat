// Evaluate the returned permutation sum on relabeled phase-space points.
// This exercises the sign, stabilizer and momentum ordering independently
// of the symbolic relabeling used by the reducer.
#include "amp_expression.hpp"
#include "line_current.hpp"
#include "mixed_physical_current.hpp"
#include "permutation_sum.hpp"
#include "validation_numeric.hpp"

#include <algorithm>
#include <iostream>
#include <numeric>
#include <stdexcept>

using namespace hepcat;
using namespace hepcat::validation;

namespace {
void require(bool ok, const char* reason) {
    if (!ok) throw std::runtime_error(reason);
}

point permute(const point& p, const std::vector<int>& legs, const std::vector<int>& order) {
    auto q = p;
    std::vector<std::size_t> from(p.masses.size());
    std::iota(from.begin(),from.end(),0);
    for (std::size_t i = 0; i < legs.size(); ++i) from[legs[i]-1] = order[i]-1;
    for (std::size_t i = 0; i < from.size(); ++i) {
        const auto j = from[i];
        q.momenta[i] = p.momenta[j];
        q.angles[i] = p.angles[j];
        q.squares[i] = p.squares[j];
        q.angle_basis[i] = p.angle_basis[j];
        q.square_basis[i] = p.square_basis[j];
        for (std::size_t k = 0; k < from.size(); ++k)
            q.scalar_products[from.size()*i+k] = p.scalar_products[from.size()*j+from[k]];
    }
    return q;
}

void test_massive_scalar_legs() {
    Algebra a;
    const auto m = a.parse("m");
    a.setKinematics({a.parse("M1"),a.parse("M2"),m,m,m,m}, {});
    auto inverse_pole = [&](int spectator, int leg) {
        const auto labels = a.call("Multiparticle",{a.integer(spectator),a.integer(leg)});
        return a.pow(a.call("PropDen",{a.call("Mom",{labels}),m}),-1);
    };
    std::vector<Id> ordered;
    for (int i = 3; i <= 6; ++i)
        for (int j = 3; j <= 6; ++j)
            for (int k = 3; k <= 6; ++k)
                if (i != j && i != k && j != k)
                    ordered.push_back(a.mul({inverse_pole(1,i),inverse_pole(2,j),
                                             a.pow(inverse_pole(1,k),2)}));
    const auto sum = compact_permutations(a,a.add(ordered));
    require(sum && sum->legs == std::vector<int>({3,4,5,6}) &&
            sum->orbit_sizes == std::vector<std::size_t>{24} &&
            std::any_of(ordered.begin(),ordered.end(),[&](Id term) {
                return compact_commuting_products(a,term) == sum->kernel;
            }),
            "failed to collect identical massive scalar legs");
}

void test_single_minority(std::vector<int> majority, int minority, bool conjugate) {
    // Keep the fermion legs interleaved with the photons. At odd and even
    // multiplicity the center of the line is a vertex or a propagator.
    const int n = static_cast<int>(majority.size())+3;
    const int left = 2, right = 5;
    Algebra a;
    const auto mass = a.integer(1), zero = a.integer(0);
    std::vector<Id> masses(n,zero);
    masses[left-1] = masses[right-1] = mass;
    a.setKinematics(masses,{});
    std::vector<real> numerical_masses(n,0);
    numerical_masses[left-1] = numerical_masses[right-1] = 1;
    const std::vector<int> lone{minority};
    const auto& plus = conjugate ? lone : majority;
    const auto& minus = conjugate ? majority : lone;
    std::optional<permutation_sum> physical;
    const auto reference = physical_current_with_presentation(a,left,right,mass,plus,minus,mass,&physical);
    const auto ward = ordered_minority_sum(a,left,right,mass,plus,minus,mass);
    require(reference && ward && ward->legs == majority,"missing ordered minority-photon formula");
    if (majority.size() >= 4) require(physical.has_value(),"missing midpoint permutation sum");
    const expression_evaluator full(a,*reference,{});
    for (const auto& sum : {physical,ward}) {
        if (!sum) continue;
        const Id reduced = simplify_permutation_kernel(a,sum->kernel,majority);
        const expression_evaluator kernel(a,reduced,{});
        for (unsigned sample = 0; sample < 5; ++sample) {
            const auto p = sample_point(numerical_masses,553171+sample);
            const auto expected = full(p);
            complex total{};
            auto order = majority;
            do { total += kernel(permute(p,majority,order)); }
            while (std::next_permutation(order.begin(),order.end()));
            require(std::abs(total-expected) <= 1e-7L*std::abs(expected),
                    "the ordered minority-photon formula disagrees with the signed current");
        }
    }
}

void test_paired_weyl(bool physical, bool conjugate, bool shuffled) {
    Algebra a;
    const int left = shuffled ? 5 : 1, right = 2;
    const auto mass = a.atom("m"), zero = a.integer(0);
    std::vector<Id> shell(7,zero);
    std::vector<std::string> masses(7,"0");
    std::vector<real> numeric(7);
    for (int leg : {left,right}) {
        shell[leg-1] = mass;
        masses[leg-1] = "m";
        numeric[leg-1] = 1;
    }
    a.setKinematics(shell,{});
    const std::vector<int> major = shuffled ? std::vector<int>{6,1,3} : std::vector<int>{3,4,5};
    const std::vector<int> minor = shuffled ? std::vector<int>{7,4} : std::vector<int>{6,7};
    const auto build = physical ? mixed_physical_current : mixed_spurious_current;
    const auto scalar = a.mul({a.exact(mpq_class(-5,7)),a.pow(a.atom("g"),5)});
    const auto original = build(a,left,right,mass,conjugate ? minor : major,
                                conjugate ? major : minor,scalar).value();
    amp_expression input(a.format(original),masses);
    const auto grouped = input.reduce().grouped_permutation_form();
    auto expected = std::vector<std::vector<int>>{major,minor};
    for (auto& group : expected) std::sort(group.begin(),group.end());
    require(grouped && grouped->groups == expected,"lost independent Weyl-current permutations");
    const auto single = input.permutation_form();
    require(single && single->legs == expected[0],"single-group view changed its summation convention");
    require(input.latex().find("\\times S_")!=std::string::npos &&
            input.plain_text().find(") and (")!=std::string::npos,"missing independent groups in output");
    const parameters values{{"m",1},{"g",0.4L}};
    expression_evaluator kernel(a,a.parse(grouped->summand),values), legacy(a,a.parse(single->summand),values),
                         full(a,original,values);
    for (unsigned sample=0; sample<6; ++sample) {
        const auto point=sample_point(numeric,41792+sample);
        const auto reference=full(point);
        complex joint=0, old_api=0;
        auto first=expected[0];
        do {
            const auto q=permute(point,expected[0],first);
            old_api+=legacy(q);
            auto second=expected[1];
            do { joint+=kernel(permute(q,expected[1],second)); }
            while (std::next_permutation(second.begin(),second.end()));
        } while (std::next_permutation(first.begin(),first.end()));
        require(std::abs(joint-reference)<1e-8L*std::abs(reference) &&
                std::abs(old_api-reference)<1e-8L*std::abs(reference),"incorrect signed Weyl-current sum");
    }
    const auto changed = a.mul({original,a.call("MomProd",{a.integer(major[0]),a.integer(major[1])})});
    require(!recognize_ordered_current(a,changed),"stripped a momentum-dependent coefficient");
}

void test_reloaded_minority(bool physical, bool conjugate, bool shuffled) {
    Algebra a;
    const Id mass = a.atom("m"), zero = a.integer(0);
    std::vector<std::string> masses(7,"0");
    int left = shuffled ? 5 : 1, right = 2;
    masses[left-1] = masses[right-1] = "m";
    std::vector<Id> shell(7,zero);
    shell[left-1] = shell[right-1] = mass;
    a.setKinematics(shell,{});
    // Change the fixed square reference, fermion ordering, names and overall
    // normalization. No model, particles, graphs or saved kernel are passed
    // to amp_expression.
    const std::vector<int> majority = shuffled ? std::vector<int>{6,1,3,4} : std::vector<int>{3,4,5,6};
    const std::vector<int> minority{7};
    const auto& plus = conjugate ? minority : majority;
    const auto& minus = conjugate ? majority : minority;
    const auto build = physical ? mixed_physical_current : mixed_spurious_current;
    const Id normalization = a.mul({a.exact(mpq_class(-7,3)),a.pow(a.atom("g"),5)});
    const Id original = build(a,left,right,mass,plus,minus,normalization).value();
    amp_expression loaded(a.format(original),masses);
    const auto sum = loaded.reduce().permutation_form();
    auto sorted = majority;
    std::sort(sorted.begin(),sorted.end());
    require(sum && sum->legs == sorted && sum->orbit_sizes.empty(),
            "standalone reduction did not recover the ordered-current representation");
    if (!shuffled) {
        amp_expression kernel(sum->summand,masses);
        require(kernel.complexity() <= (physical ? 331u : 408u),"standalone summand regressed in complexity");
    }
    parameters pars{{"g",2},{"m",1}};
    expression_evaluator kernel(a,a.parse(sum->summand),pars), full(a,original,pars);
    std::vector<real> values(7,0);
    values[left-1] = values[right-1] = 1;
    for (unsigned i=0;i<4;++i) {
        const auto p = sample_point(values,132317+i);
        auto order = sorted;
        complex total{};
        do { total += kernel(permute(p,sorted,order)); } while (std::next_permutation(order.begin(),order.end()));
        const auto expected = full(p);
        require(std::abs(total-expected) <= 1e-7L*std::abs(expected),"reloaded signed amplitude differs");
    }
    if (shuffled) return;
    const Id spectator = a.call("PropDen",{a.call("Mom",{a.call("Multiparticle",{a.integer(left),a.integer(3)})}),mass});
    require(!recognize_ordered_current(a,a.mul({spectator,original})),
            "pulled a leg-dependent factor through a permutation sum");
    auto factors = a.node(original).args;
    auto body = std::find_if(factors.begin(),factors.end(),[&](Id f) { return a.node(f).head == "Plus"; });
    require(body != factors.end(),"missing sum of currents in negative control");
    auto terms = a.node(*body).args;
    terms.front() = a.mul({a.integer(-1),terms.front()});
    *body = a.add(std::move(terms));
    require(!recognize_ordered_current(a,reduceInto(a,a.mul(std::move(factors)),shell,{})),
            "accepted a current with a wrong relative sign");
}

void test_current(const model& qed, bool physical) {
    std::vector<std::string> particles{"e","E","A.+","A.+","A.+","A.+","A.+"};
    std::vector<std::string> masses;
    for (const auto& p : particles) masses.push_back(qed.particle(p).mass);
    Algebra a(&qed.algebra());
    std::vector<Id> mass_ids;
    for (const auto& mass : masses) mass_ids.push_back(a.parse(mass));
    a.setKinematics(mass_ids, {});
    const auto original = dirac_line_proposal(a,qed,particles,120,physical);
    require(original.has_value(),"missing seven-point amplitude");
    amp_expression amp(a.format(*original),masses);
    amp.reduce();
    const auto compact = amp.permutation_form();
    require(compact.has_value(),"missing compact permutation sum");
    auto lengths = compact->orbit_sizes;
    std::sort(lengths.begin(),lengths.end());
    require(lengths == (physical ? std::vector<std::size_t>{10,30,60}
                                 : std::vector<std::size_t>{20,60,60,60}), "wrong signed orbit sizes");
    require(amp.complexity() == (physical ? 820u : 920u), "expanded complexity changed");
    const amp_expression kernel(compact->summand,masses);
    require(kernel.complexity() == (physical ? 39u : 24u), "wrong summand complexity");
    require(amp.plain_text().find("Sum over all permutations") == 0 &&
            amp.latex().find("\\sum_") == 0, "missing typeset permutation sum");

    const auto parameters = model_parameters(qed);
    expression_evaluator full(a,*original,parameters), seed(a,a.parse(compact->summand),parameters);
    const auto mass = evaluate_scalar(a,a.parse(masses.front()),parameters).real();
    for (unsigned sample = 0; sample < 8; ++sample) {
        const auto p = sample_point({mass,mass,0,0,0,0,0},88031+sample);
        auto order = compact->legs;
        complex sum = 0;
        do { sum += seed(permute(p,compact->legs,order)); }
        while (std::next_permutation(order.begin(),order.end()));
        const auto expected = full(p);
        require(std::abs(expected) > 0 && std::abs(sum-expected) <= 1e-7L*std::abs(expected),
                "explicit permutation sum disagrees with the original signed amplitude");
    }

    amp_expression broken("Plus[" + amp.str() + ",SpinorChain[Spinor[\"Helicity\",\"Square\",3],"
                          "Spinor[\"Helicity\",\"Square\",4]]]",masses);
    const auto residual_symmetry = broken.reduce().permutation_form();
    require(!residual_symmetry || residual_symmetry->legs == std::vector<int>({5,6,7}),
            "permuted a leg whose signed orbit was broken");
    amp_expression on_cut(amp.str(),masses,{{{1,3},masses.front()}});
    require(!on_cut.reduce().permutation_form(),"permuted a diagram-local shell");
}
} // namespace

int main(int argc, char** argv) {
    if (argc != 2) return 2;
    try {
        test_massive_scalar_legs();
        test_single_minority({1,3,4},6,false);
        test_single_minority({1,3,4,6},7,false);
        test_single_minority({1,3,4,6},7,true);
        test_single_minority({1,3,4,6,7},8,true);
        test_reloaded_minority(true,false,false);
        test_reloaded_minority(false,false,false);
        test_reloaded_minority(false,true,true);
        test_paired_weyl(true,false,false);
        test_paired_weyl(false,false,false);
        test_paired_weyl(true,true,true);
        test_paired_weyl(false,true,true);
        model qed(argv[1],3);
        test_current(qed,true);
        test_current(qed,false);
        std::cout << "Both seven-point kernels reconstruct the signed amplitudes.\n";
    } catch (const std::exception& e) {
        std::cerr << e.what() << '\n';
        return 1;
    }
}
