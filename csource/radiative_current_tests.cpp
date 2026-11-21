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
template<class Evaluator>
complex sum_group(const Evaluator& kernel, const point& p, const std::vector<int>& photons, bool alternating=false) {
  auto order = photons;
  complex result = 0;
  do {
    auto q = p;
    auto image = photons;
    int sign = 1;
    for (std::size_t j = 0; j < photons.size(); ++j) {
      auto dest = std::find(image.begin()+j,image.end(),order[j]) - image.begin();
      if (alternating && dest!=static_cast<std::ptrdiff_t>(j)) sign=-sign;
      exchange_legs(q,photons[j]-1,photons[dest]-1);
      std::swap(image[j], image[dest]);
    }
    result += real(sign)*kernel(q);
  } while (std::next_permutation(order.begin(),order.end()));
  return result;
}

template<class Evaluator>
complex sum_exchanges(const Evaluator& seed, const point& p, const std::vector<signed_exchange>& exchanges) {
  auto recurse = [&](auto& self, size_t count, const point& state)->complex {
    if (!count) return seed(state);
    auto image=state;
    const auto& op=exchanges[count-1];
    for (auto [l,r] : op.swaps) exchange_legs(image,l-1,r-1);
    return self(self,count-1,state)+real(op.sign)*self(self,count-1,image);
  };
  return recurse(recurse,exchanges.size(),p);
}

template<class Evaluator>
complex sum_fermions(const Evaluator& seed, const point& p,
                     const std::vector<int>& photons, const std::vector<std::vector<int>>& alternating,
                     const signed_exchange& conjugate, const std::vector<signed_exchange>& inner = {}) {
  auto local = [&](const point& q) {
    return sum_exchanges([&](const point& state) { return sum_group(seed,state,photons); },q,inner);
  };
  auto reverse = [&](const point& q) {
    auto reversed = q;
    for (auto [left,right] : conjugate.swaps) exchange_legs(reversed,left-1,right-1);
    return local(q) + real(conjugate.sign)*local(reversed);
  };
  auto second = [&](const point& q) { return sum_group(reverse,q,alternating.at(1),true); };
  return sum_group(second,p,alternating.at(0),true);
}

void check_presentation(Algebra& a, Id original, const permutation_sum& generated, const std::vector<Id>& masses,
                        const parameters& inputs, const std::vector<real>& values,
                        const std::vector<int>& expected_photons, size_t maximum_weight) {
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
  if (photons!=expected_photons || generated.legs!=expected_photons || electrons.size()!=2 || positrons.size()!=2)
    throw std::runtime_error("Bad leg groups");
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
  const auto weight = expression_complexity(a,a.parse(signed_sum->summand));
  if (weight > maximum_weight)
    throw std::runtime_error("Signed summand weight " + std::to_string(weight) +
                             " exceeds " + std::to_string(maximum_weight));
  if (amp.permutation_summand_complexity() != weight)
    throw std::runtime_error("Reported complexity differs from the displayed summand");
  for (unsigned sample = 0; sample < 3; ++sample) {
    auto p = sample_point(values, 81321+sample);
    const auto reference = full(p);
    agree(sum_fermions(seed,p,photons,signed_sum->alternating_groups,*signed_sum->joint_exchange),
          reference,"Recognized charge-reversed current mismatch");
    agree(sum_fermions(constructed,p,generated.legs,generated.alternating_groups,*generated.joint_exchange),
          reference,"Generated charge-reversed current mismatch");
    agree(sum_group(unsigned_seed,p,photons),reference,"Unsigned API compatibility mismatch");
  }

  // Flip just one complete exchange allocation. It retains all poles and
  // little-group weights; the common signed coefficient check must reject it.
  auto root = a.node(original);
  if (root.head!="Plus" || root.args.size()!=16) throw std::runtime_error("Missing negative control allocations");
  root.args.front()=a.mul({a.integer(-1),root.args.front()});
  amp_expression incorrect(a.format(a.add(root.args)),mass_text);
  const auto wrong = incorrect.reduce().signed_permutation_form();
  if (wrong && wrong->joint_exchange)
    throw std::runtime_error("Recognized a wrong relative sign in one exchange allocation");
}

// The two pole choices can share the same elementary expression when only
// one external photon is available. Select the requested displayed summand
// explicitly on reload, including after a previous view has been cached.
void check_three_pairs(Algebra& a, Id original, const permutation_sum& generated,
                       const std::vector<Id>& masses, const parameters& inputs,
                       const std::vector<real>& values, bool physical) {
  std::vector<std::string> mass_text;
  for (Id m : masses) mass_text.push_back(a.format(m));
  amp_expression amp(a.format(original),mass_text);
  const auto target = physical ? generation_references::physical : generation_references::cyclic;
  const auto view = amp.reduce(target).signed_permutation_form();
  if (!view || !view->symmetric_groups.empty() || !generated.legs.empty() ||
      !view->joint_exchange || !generated.joint_exchange ||
      view->joint_exchange->sign != 1 || generated.joint_exchange->sign != 1 ||
      view->alternating_groups.size()!=2 || generated.alternating_groups.size()!=2 ||
      view->inner_exchanges.size()!=1 || view->inner_exchanges[0].sign!=1 ||
      generated.inner_exchanges.size()!=1 || generated.inner_exchanges[0].sign!=1)
    throw std::runtime_error("Incorrect three-pair Fermi groups or mirror");
  for (const auto& block : view->alternating_groups)
    if (block.size()!=3) throw std::runtime_error("Missing identical charged legs");
  const expression_evaluator full(a,original,inputs), seed(a,a.parse(view->summand),inputs),
                             produced(a,generated.kernel,inputs);
  if (view->projected_terms.size()!=2 || generated.projected_terms.size()!=2)
    throw std::runtime_error("Missing leaf and center exchange operators");
  const auto& leaf=view->projected_terms[0];
  const auto& center=view->projected_terms[1];
  if (leaf.exchanges.size()!=1 || center.exchanges.size()!=1 || leaf.exchanges[0].sign!=-1 ||
      center.exchanges[0].sign!=1 || leaf.exchanges[0].swaps!=center.exchanges[0].swaps)
    throw std::runtime_error("Incorrect current orientation signs");
  const auto leaf_id=a.parse(leaf.summand), center_id=a.parse(center.summand);
  auto weight=expression_complexity(a,leaf_id)+expression_complexity(a,center_id);
  if (weight!=amp.permutation_summand_complexity() || weight>(physical ? 334u : 254u))
    throw std::runtime_error("Unreduced three-line summand of weight "+std::to_string(weight));
  const expression_evaluator leaf_value(a,leaf_id,inputs), center_value(a,center_id,inputs);
  auto compact=[&](const point& p) {
    return sum_exchanges(leaf_value,p,leaf.exchanges) + sum_exchanges(center_value,p,center.exchanges);
  };
  const auto plain=amp.plain_text(), tex=amp.latex();
  if (plain.find("(1 - P")==std::string::npos || plain.find("(1 + P")==std::string::npos ||
      tex.find("\\left(1-\\mathsf P_")==std::string::npos || tex.find("\\left(1+\\mathsf P_")==std::string::npos)
    throw std::runtime_error("Missing current projectors in the displayed formula");
  const auto unsigned_view = amp.permutation_form();
  const auto grouped = amp.grouped_permutation_form();
  if (!unsigned_view || !unsigned_view->legs.empty() || !grouped || !grouped->groups.empty() ||
      grouped->summand != unsigned_view->summand)
    throw std::runtime_error("Unsigned accessor left an unsummed charged-line permutation");
  const expression_evaluator expanded(a,a.parse(unsigned_view->summand),inputs);
  const auto sample=sample_point(values,81320);
  agree(expanded(sample),full(sample),"Incorrect application order in unsigned accessor");
  for (unsigned j=0; j<3; ++j) {
    auto p=sample_point(values,81321+j);
    agree(sum_fermions(compact,p,{},view->alternating_groups,*view->joint_exchange,view->inner_exchanges),full(p),
          "Reloaded factored three-pair current mismatch");
    agree(sum_fermions(seed,p,{},view->alternating_groups,*view->joint_exchange),full(p),
          "Reloaded three-pair signed orbit mismatch");
    agree(sum_fermions(produced,p,{},generated.alternating_groups,*generated.joint_exchange,generated.inner_exchanges),
          full(p),"Generated three-pair signed orbit mismatch");
  }
  const auto other = amp.reduce(physical ? generation_references::cyclic : generation_references::physical)
                        .signed_permutation_form();
  if (!other || !other->joint_exchange) throw std::runtime_error("Lost the current on changing pole preference");
  const auto p=sample_point(values,81521);
  const expression_evaluator alternative(a,a.parse(other->summand),inputs);
  agree(sum_fermions(alternative,p,{},other->alternating_groups,*other->joint_exchange),full(p),
        "Changing the requested pole form corrupted the signed sum");

  auto terms=a.node(original);
  if (terms.head!="Plus" || terms.args.size()!=54) throw std::runtime_error("Missing three-line allocations");
  terms.args.front()=a.mul({a.integer(-1),terms.args.front()});
  amp_expression wrong(a.format(a.add(terms.args)),mass_text);
  if (wrong.reduce().signed_permutation_form()) throw std::runtime_error("Accepted a wrong three-line sign");
}

// With two photons of each helicity the two Bose groups act independently.
// In particular, the balanced (1,1)|(1,1) allocation has a larger stabilizer.
void check_balanced_occupations(const model& vertices) {
  const std::vector<std::string> legs{"e","E","E","e","A.+","A.+","A.-","A.-"};
  Algebra a;
  const auto inputs = model_parameters(vertices);
  const auto m = a.parse(vertices.particle("e").mass), z = a.integer(0);
  a.setKinematics({m,m,m,m,z,z,z,z},{});
  std::optional<permutation_sum> form;
  const auto elementary = fermion_current_proposal(a,vertices,legs,1680,true,&form);
  if (!elementary || !form || form->joint_exchange || form->legs != std::vector<int>{5,6} ||
      form->other_groups != std::vector<std::vector<int>>{{7,8}} ||
      form->alternating_groups != std::vector<std::vector<int>>{{1,4},{2,3}} ||
      a.node(form->kernel).head != "Plus" || a.node(form->kernel).args.size() != 5)
    throw std::runtime_error("Expected five helicity occupations and two independent photon pairs");
  const expression_evaluator full(a,*elementary,inputs), seed(a,form->kernel,inputs);
  const auto mass = evaluate_scalar(a,m,inputs).real();
  for (unsigned sample = 0; sample < 3; ++sample) {
    const auto p = sample_point({mass,mass,mass,mass,0,0,0,0},704750+sample);
    complex sum{};
    for (unsigned mask = 0; mask < 8; ++mask) {
      auto image = p;
      if (mask & 1) exchange_legs(image,0,3);
      if (mask & 2) exchange_legs(image,1,2);
      if (mask & 4) exchange_legs(image,6,7);
      const bool odd = (mask & 3) == 1 || (mask & 3) == 2;
      sum += real(odd ? -1 : 1)*sum_group(seed,image,{5,6});
    }
    agree(sum,full(p),"Incorrect eight-point helicity-occupation weight");
  }
}

// In a star the permutation of three identical leaves already changes the
// ordering on the central line. In a path it pairs the orders related by
// reversal. Check the signed sum against the unrearranged charged currents.
void check_exchange_orderings(const model &vertices) {
  const std::vector<std::string> legs{"e","E","e","E","e","E","e","E"};
  Algebra a;
  const auto m = a.parse(vertices.particle("e").mass);
  const auto parameters = model_parameters(vertices);
  a.setKinematics(std::vector<Id>(8,m),{});
  std::optional<permutation_sum> form;
  const auto elementary = fermion_current_proposal(a,vertices,legs,1728,true,&form);
  if (!elementary || !form)
    throw std::runtime_error("Missing the complete eight-fermion tree sum");
  auto sizes = form->orbit_sizes;
  std::sort(sizes.begin(),sizes.end());
  if (sizes != std::vector<size_t>{288,288,576,576})
    throw std::runtime_error("Incorrect ordered star and path orbit sizes");
  const expression_evaluator full(a,*elementary,parameters);
  const expression_evaluator projected(a,expand_permutation_groups(a,form->kernel,form->alternating_groups,true),
                                       parameters);
  const std::vector<real> masses(8,evaluate_scalar(a,m,parameters).real());
  for (unsigned n=0; n<3; ++n) {
    const auto point = sample_point(masses,837871+n);
    agree(projected(point),full(point),"A virtual attachment order has the wrong sign or multiplicity");
  }
}

void check(const model &vertices, const model &rules,
           const std::vector<std::string> &legs, bool physical, size_t diagrams,
           size_t exchange_first, size_t exchange_second, const std::vector<int>& symmetric_photons = {},
           size_t maximum_weight = 0) {
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
  const bool three_pairs = diagrams == 252;
  if (presentation.has_value() != (three_pairs || !symmetric_photons.empty()))
    throw std::runtime_error("Incorrect symmetry certificate for these particles");
  if (!current)
    throw std::runtime_error("Missing multi-line current");
  if (three_pairs) check_three_pairs(a,*current,*presentation,m,inputs,values,physical);
  else if (presentation) check_presentation(a,*current,*presentation,m,inputs,values,symmetric_photons,maximum_weight);
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
    check(vertices, rules, {"e", "E", "E", "e", "A.+", "A.+", "A.+"}, true, 240, 1, 2, {5,6,7}, 388);
    check(vertices, rules, {"e", "E", "A.-", "e", "A.-", "E", "A.-"}, false, 240, 0, 3, {3,5,7}, 360);
    check(vertices, rules, {"e", "E", "E", "e", "A.+", "A.+", "A.-"}, true, 240,
          1, 2, {5,6}, 1188);
    check(vertices, rules, {"E", "A.-", "e", "A.+", "e", "E", "A.-"}, false,
          240, 2, 4, {2,7}, 1124);
    check(vertices, rules, {"e", "E", "e", "E", "e", "E", "A.-"}, true, 252, 0,
          4);
    check(vertices,rules,{"E","e","A.+","e","E","E","e"},false,252,1,6);
    check_balanced_occupations(vertices);
    check_exchange_orderings(vertices);
    std::cout << "Signed Weyl currents, Fermi exchange and photon occupations passed.\n";
  } catch (const std::exception &e) {
    std::cerr << e.what() << '\n';
    return 1;
  }
}
