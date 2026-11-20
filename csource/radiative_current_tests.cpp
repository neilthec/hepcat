// Focused comparison of the grouped endpoint currents with the independently
// planned CalcHEP rules. The public notebook driver runs the full checks.
#include "fermion_currents.hpp"
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

void check(const model &vertices, const model &rules,
           const std::vector<std::string> &legs, bool physical, size_t diagrams,
           size_t exchange_first, size_t exchange_second) {
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
  auto current =
      fermion_current_proposal(a, vertices, legs, diagrams, physical);
  if (!current)
    throw std::runtime_error("Missing multi-line current");
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
