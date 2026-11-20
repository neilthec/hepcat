// Focused comparison of the grouped endpoint currents with the independently
// planned CalcHEP rules. The public notebook driver runs the full checks.
#include "fermion_currents.hpp"
#include "amp_expression.hpp"
#include <algorithm>
#include "generation_feynman.hpp"
#include "generation_plan.hpp"
#include "validation_feynman.hpp"
#include "validation_numeric.hpp"

#include <iostream>
#include <stdexcept>

using namespace hepcat;
using namespace hepcat::validation;

namespace {
void agree(complex x, complex y, const char *description) {
  const auto scale = std::max(std::abs(x), std::abs(y));
  if (!scale || !std::isfinite(scale) || std::abs(x - y) > 1e-8L * scale)
    throw std::runtime_error(description);
}

void exchange_legs(point &p, size_t x, size_t y) {
  // The momenta and the chosen external spin state travel together.
  for (auto field : {&p.angles, &p.squares})
    std::swap((*field)[x], (*field)[y]);
  std::swap(p.angle_basis[x], p.angle_basis[y]);
  std::swap(p.square_basis[x], p.square_basis[y]);
  std::swap(p.momenta[x], p.momenta[y]);
  std::swap(p.masses[x], p.masses[y]);
  auto n = p.masses.size();
  for (size_t k = 0; k < n; ++k)
    std::swap(p.scalar_products[n * x + k], p.scalar_products[n * y + k]);
  for (size_t k = 0; k < n; ++k)
    std::swap(p.scalar_products[n * k + x], p.scalar_products[n * k + y]);
}

// Evaluate an explicitly signed product of permutation sums without using
// the symbolic relabeler. The momentum and massive spin state travel together.
complex sum_photons(const expression_evaluator& kernel, const point& p, const std::vector<int>& photons) {
  auto order = photons;
  complex result = 0;
  do {
    auto q = p;
    auto image = photons;
    for (std::size_t j = 0; j < photons.size(); ++j) {
      auto dest = std::find(image.begin()+j,image.end(),order[j]) - image.begin();
      exchange_legs(q,photons[j]-1,photons[dest]-1);
      std::swap(image[j], image[dest]);
    }
    result += kernel(q);
  } while (std::next_permutation(order.begin(),order.end()));
  return result;
}

complex sum_fermions(const expression_evaluator& seed, const point& p,
                     const std::vector<int>& photons, const std::vector<std::vector<int>>& alternating,
                     const signed_exchange& conjugate) {
  complex total = 0;
  for (unsigned mask = 0; mask < 4; ++mask) {
    auto q = p;
    int sign = 1;
    for (size_t i = 0; i < alternating.size(); ++i) {
      if (!(mask & (1u << i))) continue;
      exchange_legs(q,alternating[i][0]-1,alternating[i][1]-1);
      sign = -sign;
    }
    auto reversed = q;
    for (auto [left,right] : conjugate.swaps) exchange_legs(reversed,left-1,right-1);
    total += real(sign)*(sum_photons(seed,q,photons) + real(conjugate.sign)*sum_photons(seed,reversed,photons));
  }
  return total;
}

void check_presentation(Algebra& a, Id original, const permutation_sum& generated, const std::vector<Id>& masses,
                        const parameters& inputs, const std::vector<real>& values) {
  std::vector<std::string> mass_text;
  for (Id id : masses) mass_text.push_back(a.format(id));
  amp_expression amp(a.format(original),mass_text);
  const auto signed_sum = amp.reduce().signed_permutation_form();
  if (!signed_sum || signed_sum->symmetric_groups.size()!=1 || signed_sum->alternating_groups.size()!=2 ||
      !signed_sum->joint_exchange || signed_sum->joint_exchange->sign!=-1 ||
      !generated.joint_exchange || generated.joint_exchange->sign!=-1)
    throw std::runtime_error("Missing photon permutations, fermion antisymmetries or signed charge reversal");
  const auto& photons = signed_sum->symmetric_groups.front();
  const auto& electrons = signed_sum->alternating_groups[0];
  const auto& positrons = signed_sum->alternating_groups[1];
  if (photons.size()!=3 || electrons.size()!=2 || positrons.size()!=2) throw std::runtime_error("Bad leg groups");
  const std::vector<std::pair<int,int>> paired{{electrons[0],positrons[0]},{electrons[1],positrons[1]}};
  if (signed_sum->joint_exchange->swaps!=paired) throw std::runtime_error("Incorrect charge-reversed legs");
  const expression_evaluator full(a,original,inputs), seed(a,a.parse(signed_sum->summand),inputs);
  const expression_evaluator constructed(a,generated.kernel,inputs);
  const auto old = amp.permutation_form();
  const auto grouped = amp.grouped_permutation_form();
  if (!old || !grouped || old->legs!=photons || grouped->groups!=signed_sum->symmetric_groups)
    throw std::runtime_error("Unsigned API did not expand alternating groups");
  const expression_evaluator unsigned_seed(a,a.parse(old->summand),inputs);
  const auto tex = amp.latex(), plain = amp.plain_text();
  const auto conjugate = plain.find("simultaneous exchange");
  const auto alternate = plain.find("Antisymmetrize");
  const auto symmetrize = plain.find("Sum over all permutations");
  if (grouped->summand!=old->summand || tex.find("operatorname{sgn}") == std::string::npos ||
      tex.find("\\pi\\!\\left(1-\\mathsf P_") == std::string::npos ||
      !(conjugate < alternate && alternate < symmetrize && symmetrize != std::string::npos))
    throw std::runtime_error("Signed presentation lost an operator or changed their order");
  if (amp.permutation_summand_complexity() != expression_complexity(a,a.parse(signed_sum->summand)))
    throw std::runtime_error("Reported complexity differs from the displayed summand");
  for (unsigned sample = 0; sample < 3; ++sample) {
    auto p = sample_point(values, 81321+sample);
    const auto reference = full(p);
    agree(sum_fermions(seed,p,photons,signed_sum->alternating_groups,*signed_sum->joint_exchange),
          reference,"Recognized charge-reversed current mismatch");
    agree(sum_fermions(constructed,p,generated.legs,generated.alternating_groups,*generated.joint_exchange),
          reference,"Generated charge-reversed current mismatch");
    agree(sum_photons(unsigned_seed,p,photons),reference,"Unsigned API compatibility mismatch");
  }

  // Flip just one complete exchange allocation. It retains all poles and
  // little-group weights; the common signed coefficient check must reject it.
  auto root = a.node(original);
  if (root.head!="Plus" || root.args.size()!=16) throw std::runtime_error("Missing negative control allocations");
  root.args.front()=a.mul({a.integer(-1),root.args.front()});
  if (recognize_ordered_current(a,a.add(root.args)))
    throw std::runtime_error("Recognized a wrong relative sign in one exchange allocation");
}

void check(const model &vertices, const model &rules,
           const std::vector<std::string> &legs, bool physical, size_t diagrams,
           size_t exchange_first, size_t exchange_second, bool signed_orbits = false) {
  Algebra a, reference(&rules.algebra());
  auto inputs = model_parameters(vertices), finputs = model_parameters(rules);
  std::vector<Id> m, fm;
  std::vector<real> values;
  for (const auto &leg : legs) {
    m.push_back(a.parse(vertices.particle(leg).mass));
    fm.push_back(reference.parse(
        rules.particle(generation::feynman_external(rules, leg)).mass));
    values.push_back(evaluate_scalar(a, m.back(), inputs).real());
  }
  a.setKinematics(m, {});
  reference.setKinematics(fm, {});
  const auto trees = generation::diagram_plans(vertices, legs);
  if (trees.size() != diagrams ||
      fermion_current_proposal(a, vertices, legs, diagrams - 1, physical))
    throw std::runtime_error("Incomplete constructive topology inventory");
  std::optional<permutation_sum> presentation;
  auto current = fermion_current_proposal(a, vertices, legs, diagrams, physical, &presentation);
  if (presentation.has_value() != signed_orbits)
    throw std::runtime_error("Incorrect symmetry certificate for these particles");
  if (!current)
    throw std::runtime_error("Missing multi-line current");
  if (signed_orbits) check_presentation(a,*current,*presentation,m,inputs,values);
  std::vector<Id> feynman_terms;
  for (const auto &plan : generation::diagram_plans(rules, legs))
    feynman_terms.push_back(feynman_to_spinors(
        reference, generation::sew_feynman(reference, rules, legs, plan)));
  expression_evaluator constructive(a, *current, inputs);
  expression_evaluator feynman(
      reference, reference.add(std::move(feynman_terms)), finputs);
  for (unsigned j = 0; j < 6; ++j) {
    auto p = sample_point(values, 81321 + j);
    auto original = constructive(p);
    agree(original, feynman(p), "Signed Feynman current mismatch");
    exchange_legs(p, exchange_first, exchange_second);
    agree(-original, constructive(p),
          "Identical-fermion exchange lost its sign");
  }
}
} // namespace

int main(int argc, char **argv) {
  if (argc != 3)
    return 2;
  try {
    const model vertices(argv[1], 3), rules(argv[2], 2);
    check(vertices, rules, {"e", "E", "E", "e", "A.+", "A.+", "A.+"}, true, 240, 1, 2, true);
    check(vertices, rules, {"e", "E", "A.-", "e", "A.-", "E", "A.-"}, false, 240, 0, 3, true);
    check(vertices, rules, {"e", "E", "E", "e", "A.+", "A.+", "A.-"}, true, 240,
          1, 2);
    check(vertices, rules, {"e", "E", "E", "e", "A.-", "A.-", "A.+"}, false,
          240, 0, 3);
    check(vertices, rules, {"e", "E", "e", "E", "e", "E", "A.-"}, true, 252, 0,
          4);
    std::cout << "Seven-point signed Weyl currents and identical-fermion "
                 "exchanges passed.\n";
  } catch (const std::exception &e) {
    std::cerr << e.what() << '\n';
    return 1;
  }
}
