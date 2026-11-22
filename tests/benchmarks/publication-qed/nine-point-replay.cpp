// Independent subset evaluation of the charged-tree formulas.
// See nine-point.md for compilation and the comparison modes.
#include "generation_feynman.hpp"
#include "madgraph-runtime.hpp"
#include "nine-point-tensors.hpp"
#include "validation_dirac_current.hpp"
#include "validation_feynman.hpp"
#include <iomanip>
#include <iostream>
#include <sstream>
using namespace hepcat;
using namespace hepcat::validation;
int main(int argc, char **argv) {
  if (argc < 6 || argc > 8 || (argc == 8 && std::string(argv[7]) != "tensor" && std::string(argv[7]) != "projected"))
    return 2;
  const int pairs = std::stoi(argv[1]), nphot = std::stoi(argv[2]);
  std::string helicities = argv[3];
  const bool explicit_helicities = !helicities.empty() &&
      helicities.find_first_not_of("+-") == std::string::npos;
  if (explicit_helicities && static_cast<int>(helicities.size()) != nphot)
    return 2;
  const int minus = explicit_helicities
      ? static_cast<int>(std::count(helicities.begin(), helicities.end(), '-'))
      : std::stoi(helicities);
  const unsigned points = std::stoul(argv[4]), seed = std::stoul(argv[5]);
  if (pairs < 1 || pairs > 4 || nphot < 0 || 2 * pairs + nphot > 9 ||
      minus < 0 || minus > nphot || !points)
    return 2;
  const bool symbolic = argc == 6 || std::string(argv[6]) == "signed";
  const bool tensor_rate = argc == 8;
  const bool project = std::string(argv[argc - 1]) == "projected";
  const bool paper = argc == 7 && (std::string(argv[6]) == "paper" || project);
  const bool gauge_only = argc == 7 && std::string(argv[6]) == "gauges";
  std::vector<int> types;
  for (int i = 0; i < pairs; ++i) {
    types.push_back(1);
    types.push_back(-1);
  }
  if (pairs == 2)
    std::swap(types[2], types[3]);
  if (!explicit_helicities)
    helicities = std::string(nphot - minus, '+') + std::string(minus, '-');
  for (char h : helicities)
    types.push_back(h == '+' ? 2 : -2);
  std::vector<real> masses(types.size(), 0);
  std::fill(masses.begin(), masses.begin() + 2 * pairs, 1);
  const real e = 1.L / 3;
  dirac_current::recursion evaluate(types);
  std::optional<expression_evaluator> feynman;
  std::optional<hepcat_spinas::madgraph_detail::Reference> mg;
  if (symbolic) {
    model rules("tests/reference-models/calchep", 2);
    Algebra a(&rules.algebra());
    std::vector<Id> m, diagrams;
    std::vector<std::string> names;
    for (int t : types) {
      names.emplace_back(t == 1 ? "e" : t == -1 ? "E" : t == 2 ? "A.+" : "A.-");
      m.push_back(a.parse(
          rules.particle(generation::feynman_external(rules, names.back()))
              .mass));
    }
    a.setKinematics(m, {});
    for (const auto &plan : generation::diagram_plans(rules, names))
      diagrams.push_back(a.factorSum(feynman_to_spinors(
          a, generation::sew_feynman(a, rules, names, plan))));
    feynman.emplace(a, a.add(diagrams),
                    model_parameters(rules, {{"Me", 1}, {"EE", e}}));
  } else if (!gauge_only && !paper)
    mg.emplace(argv[6]);
  real peak[3]{};
  unsigned point_seed = seed;
  const char* gauge_names[] = {"physical", "spurious", "independent"};
  auto check = [&](int index, complex x, complex y) {
    const auto scale = std::max(std::abs(x), std::abs(y));
    const auto delta = scale ? std::abs(x - y) / scale : 0;
    if (!std::isfinite(delta))
      throw std::runtime_error("Nonfinite comparison");
    if (delta > 1e-7L)
      std::cerr << std::setprecision(17) << "seed=" << point_seed
                << " gauge=" << gauge_names[index] << " reference=" << x
                << " evaluated=" << y << " relative=" << delta << '\n';
    peak[index] = std::max(peak[index], delta);
  };
  for (unsigned n = 0; n < points; ++n) {
    point_seed = seed + n;
    auto p = sample_point(masses, point_seed);
    if (mg) {
      std::vector<double> mom;
      for (unsigned i = 0; i < types.size(); ++i)
        for (auto x : p.momenta[i])
          mom.push_back(i < 2 ? x : -x);
      std::vector<int> hel = types;
      for (auto &h : hel)
        h = h > 0 ? 1 : -1;
      real rate = 0, physical = 0, spurious = 0;
      for (unsigned s = 0; s < (1u << (2 * pairs)); ++s) {
        for (int i = 0; i < 2 * pairs; ++i) {
          unsigned column = (s >> i) & 1;
          hel[i] = column ? 1 : -1;
          p.angles[i] = p.angle_basis[i][column];
          p.squares[i] = p.square_basis[i][column];
        }
        rate += mg->evaluate(mom, hel);
        auto amplitude = [&](dirac_current::gauge g) {
          return tensor_rate ? nine_point::tensors(p, types, e, g, project)()
                             : evaluate(p, e, g);
        };
        physical += std::norm(amplitude(dirac_current::gauge::physical));
        spurious += std::norm(amplitude(dirac_current::gauge::spurious));
      }
      check(0, rate, physical);
      check(1, rate, spurious);
    } else {
      const auto expected =
          feynman ? (*feynman)(p)
                  : evaluate(p, e, dirac_current::gauge::independent);
      if (paper) {
        check(
            0, expected,
            nine_point::tensors(p, types, e, dirac_current::gauge::physical, project)());
        check(
            1, expected,
            nine_point::tensors(p, types, e, dirac_current::gauge::spurious, project)());
      } else {
        check(0, expected, evaluate(p, e, dirac_current::gauge::physical));
        check(1, expected, evaluate(p, e, dirac_current::gauge::spurious));
      }
      if (feynman)
        check(2, expected, evaluate(p, e, dirac_current::gauge::independent));
    }
  }
  std::cout << std::setprecision(6) << "pairs=" << pairs << " photons=" << nphot
            << " negatives=" << minus << " helicities=" << helicities << " points=" << points
            << " seed=" << seed << " reference="
            << (symbolic ? "signed Feynman"
                : paper  ? "paper tensors vs independent gauge"
                : mg     ? "MadGraph"
                         : "independent gauge")
            << (tensor_rate ? " (paper tensors)" : "")
            << (project ? " (charge-line reversal)" : "")
            << " maxPhysical=" << peak[0] << " maxSpurious=" << peak[1]
            << " maxIndependent=" << peak[2] << '\n';
  return *std::max_element(peak, peak + 3) <= 1e-7L ? 0 : 1;
}
