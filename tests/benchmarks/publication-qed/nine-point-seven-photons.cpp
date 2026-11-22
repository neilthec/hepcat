// Expand a Bose summand numerically without materializing 7! symbolic copies.
// Usage from the repository root: nine-point-seven-photons OUTPUT_DIRECTORY [--paper|--ordered|--gaps|--central]
#include "line_current.hpp"
#include "mixed_physical_current.hpp"
#include "radiation_kinematics.hpp"
#include "validation_numeric.hpp"
#include <filesystem>
#include <fstream>
#include <iostream>
#include <numeric>
using namespace hepcat;
using namespace hepcat::validation;

// Independent transcription of the three seven-positive summands in the text.
class paper_formulas : private detail::radiation_kinematics {
  Id C;
  Id X(int i) { return chain(true, i, i, {{1}, {2}}); }
  Id E(int i, legs labels) {
    std::vector<Id> terms;
    for (int j : labels)
      terms.push_back(ar.mul({chain(true, i, j), chain(false, j, i, {{1}})}));
    return ar.add(terms);
  }
  Id Y(int i, legs labels) {
    std::vector<Id> terms;
    for (int j : labels) {
      terms.push_back(j <= 2 ? chain(false, 3, i, {{j}})
                             : ar.mul({chain(false, 3, j), chain(true, j, i)}));
    }
    return ar.add(terms);
  }
  Id pair(int i, int j) { return ar.pow(chain(true, i, j), 2); }
  Id num(int n) { return ar.integer(n); }

public:
  paper_formulas(Algebra &a, Id m, Id e)
      : radiation_kinematics(a, m, true, true),
        C(a.mul({a.pow(a.mul({a.parse("Power[2,Rational[1,2]]"), e}), 7), m,
                 chain(false, 1, 2)})) {}
  Id physical_seed() {
    const Id r89 =
        over(ar.mul({pair(8, 9), tau({1, 8}), tau({1, 9})}), {tau({2, 8, 9})});
    const Id r789 = over(
        ar.mul({num(3), pair(7, 8), tau({1, 7}), tau({1, 8}),
                ar.add({X(9),
                        over(ar.mul({tau({2, 9}), minus(X(9), E(9, {7, 8}))}),
                             {tau({2, 7, 8})})})}),
        {tau({2, 7, 8, 9})});
    const Id r6789 = over(
        ar.add(
            {ar.mul({num(6), pair(6, 7), tau({1, 6}), tau({1, 7}), X(8), X(9)}),
             ar.mul({num(4), E(6, {3, 4, 5}), tau({2, 6}), r789}),
             ar.mul({num(-6), ar.pow(mass, 2), pair(6, 7), tau({2, 6}),
                     tau({2, 7}), r89})}),
        {tau({1, 3, 4, 5})});
    const Id r56789 =
        over(ar.add({ar.mul({num(10), pair(5, 6), tau({1, 5}), tau({1, 6}),
                             X(7), X(8), X(9)}),
                     ar.mul({num(5), E(5, {3, 4}), tau({2, 5}), r6789}),
                     ar.mul({num(-10), ar.pow(mass, 2), pair(5, 6), tau({2, 5}),
                             tau({2, 6}), r789})}),
             {tau({1, 3, 4})});
    std::vector<Id> factors{C, pair(3, 4), ar.exact(mpq_class(1, 240)),
                            minus(ar.mul({X(5), X(6), X(7), X(8), X(9)}),
                                  ar.mul({ar.pow(mass, 2), r56789}))};
    for (int i = 3; i <= 9; ++i)
      factors.push_back(ar.pow(tau({1, i}), -1));
    for (int i = 5; i <= 9; ++i)
      factors.push_back(ar.pow(tau({2, i}), -1));
    return ar.mul(factors);
  }
  Id spurious_seed() {
    auto y = [&](int i) { return Y(i, {1}); };
    const Id z9 = ar.add(
        {y(9), over(ar.mul({tau({1, 9}), Y(9, {1, 5, 6})}), {tau({1, 5, 6})})});
    const Id z89 =
        minus(ar.mul({y(8), y(9)}),
              over(ar.mul({num(2), tau({1, 8}), Y(8, {2, 4, 7}), z9}),
                   {tau({1, 5, 6, 9})}));
    const Id z789 = minus(ar.mul({y(7), y(8), y(9)}),
                          over(ar.mul({num(3), tau({1, 7}), Y(7, {2, 4}), z89}),
                               {tau({2, 3, 4, 7})}));
    return over(
        ar.mul({neg(C), chain(true, 3, 4),
                minus(over(ar.mul({y(5), y(6), y(7), y(8), y(9)}),
                           {chain(false, 3, 5), chain(false, 3, 6)}),
                      over(ar.mul({num(10), ar.pow(mass, 2), pair(5, 6), z789}),
                           {tau({2, 3, 4})}))}),
        {num(120), chain(false, 3, 4), tau({2, 3}), tau({1, 5}), tau({1, 6}),
         tau({1, 7}), tau({1, 8}), tau({1, 9}),
         chain(false, 3, 7), chain(false, 3, 8), chain(false, 3, 9)});
  }
  Id one_negative_gaps() {
    const int h = 9, ref = 5;
    const Id spin = ar.mul({chain(false, 1, h), chain(false, h, 2)});
    // C already contains the endpoint bracket. Remove it only from the
    // common normalization, since the spin insertion carries both ends.
    const Id U = chain(false, 1, 2);
    std::vector<Id> factors{over(C, {U, ar.pow(mass, 2)})}, gaps;
    legs prefix{1};
    for (int j = 0; j <= 6; ++j) {
      std::vector<Id> term{
          ar.add({spin, ar.mul({U, over(chain(true, ref, h, {prefix}),
                                        {chain(true, ref, h)})})})};
      legs left{1}, right{2};
      for (int i = 3; i < 3 + j; ++i) {
        left.push_back(i);
        term.push_back(ar.pow(tau(left), -1));
      }
      for (int i = 8; i >= 3 + j; --i) {
        right.push_back(i);
        term.push_back(ar.pow(tau(right), -1));
      }
      gaps.push_back(ar.mul(term));
      if (j < 6) {
        const int i = 3 + j;
        factors.push_back(
            over(chain(true, i, h, {prefix}), {chain(false, h, i)}));
        prefix.push_back(i);
      }
    }
    factors.push_back(ar.add(gaps));
    return ar.mul(factors);
  }
  Id ordered_seed() {
    return over(
        ar.mul({neg(C), chain(true, 8, 9), chain(false, 8, 3, {{1}}),
                chain(false, 8, 4, {{1, 3}}), chain(false, 8, 5, {{1, 3, 4}}),
                chain(false, 8, 6, {{2, 7, 9}}), chain(false, 8, 7, {{2, 9}})}),
        {chain(false, 8, 9), chain(false, 8, 3), chain(false, 8, 4),
         chain(false, 8, 5), chain(false, 8, 6), chain(false, 8, 7),
         tau({2, 8}), tau({1, 3}), tau({1, 3, 4}), tau({1, 3, 4, 5}),
         tau({2, 7, 8, 9}), tau({2, 8, 9})});
  }
};

int main(int argc, char **argv) {
  if (argc < 2 || argc > 3 ||
      (argc == 3 && std::string(argv[2]) != "--central" &&
       std::string(argv[2]) != "--paper" &&
       std::string(argv[2]) != "--ordered" && std::string(argv[2]) != "--gaps"))
    return 2;
  model vertices("models", 3);
  std::filesystem::create_directories(argv[1]);
  for (int h = 0; h <= 3; ++h)
    for (bool physical : {true, false}) {
      const std::string mode = argc == 3 ? argv[2] : "selected";
      if ((mode == "--gaps" && (h != 1 || physical)) ||
          ((mode == "--paper" || mode == "--ordered") &&
           (h || (physical && mode == "--ordered"))))
        continue;
      Algebra a(&vertices.algebra());
      std::vector<std::string> legs(9, "A.+");
      std::fill(legs.end() - h, legs.end(), "A.-");
      legs[0] = "e";
      legs[1] = "E";
      std::vector<Id> ms;
      std::vector<std::string> masses;
      for (const auto &l : legs) {
        masses.push_back(vertices.particle(l).mass);
        ms.push_back(a.parse(masses.back()));
      }
      a.setKinematics(ms, {});
      std::optional<permutation_sum> form;
      const auto full =
          dirac_line_proposal(a, vertices, legs, 5040, physical, &form);
      std::vector<int> plus, minus;
      for (int i = 2; i < 9; ++i)
        (legs[i] == "A.+" ? plus : minus).push_back(i + 1);
      if (argc == 3 && std::string(argv[2]) == "--central" && h &&
          (physical || h > 1)) {
        auto candidate =
            paired_weyl_sum(a, 1, 2, ms[0], plus, minus,
                            a.pow(vertices.vertices()[0].factor, 7), physical);
        if (!candidate)
          throw std::runtime_error("Missing central photon decomposition");
        if (form)
          std::cout << "central vs selected "
                    << expression_complexity(a, candidate->kernel) << " / "
                    << expression_complexity(a, form->kernel) << std::endl;
        form = candidate;
      }
      if (!full || !form)
        throw std::runtime_error("Missing Bose summand");
      if (argc == 3 && std::string(argv[2]) != "--central") {
        const auto &vars = vertices.variables();
        const auto charge =
            std::find_if(vars.begin(), vars.end(),
                         [](const auto &p) { return p.name == "EE"; });
        if (charge == vars.end())
          throw std::runtime_error("Missing QED charge");
        paper_formulas paper(a, ms[0], charge->symbol);
        if (mode == "--gaps")
          form = permutation_sum{
              {3, 4, 5, 6, 7, 8}, paper.one_negative_gaps(), {}};
        else
          form->kernel = mode == "--ordered" ? paper.ordered_seed()
                         : physical          ? paper.physical_seed()
                                             : paper.spurious_seed();
      }
      const auto params =
          model_parameters(vertices, {{"Me", 1}, {"EE", 1.L / 3}});
      expression_evaluator full_value(a, *full, params),
          summand(a, expand_signed_operators(a, *form), params);
      real peak = 0;
      for (unsigned s = 0; s < 8; ++s) {
        const auto pt = sample_point({1, 1, 0, 0, 0, 0, 0, 0, 0}, 943027 + s);
        auto relabeled = pt;
        std::vector<int> idx(9);
        std::iota(idx.begin(), idx.end(), 0);
        complex sum = 0;
        auto groups = form->other_groups;
        groups.push_back(form->legs);
        auto visit = [&](auto &&self, size_t k) -> void {
          if (k < groups.size()) {
            const auto &source = groups[k];
            auto image = source;
            std::sort(image.begin(), image.end());
            do {
              for (size_t j = 0; j < source.size(); ++j)
                idx[source[j] - 1] = image[j] - 1;
              self(self, k + 1);
            } while (std::next_permutation(image.begin(), image.end()));
            return;
          }
          for (int i = 0; i < 9; ++i) {
            const auto j = idx[i];
            relabeled.momenta[i] = pt.momenta[j];
            relabeled.angles[i] = pt.angles[j];
            relabeled.squares[i] = pt.squares[j];
            relabeled.angle_basis[i] = pt.angle_basis[j];
            relabeled.square_basis[i] = pt.square_basis[j];
            for (int k = 0; k < 9; ++k)
              relabeled.scalar_products[9 * i + k] =
                  pt.scalar_products[9 * j + idx[k]];
          }
          sum += summand(relabeled);
        };
        visit(visit, 0);
        const auto expected = full_value(pt);
        const auto diff = std::abs(expected - sum) /
                          std::max(std::abs(sum), std::abs(expected));
        if (!(diff < 1e-8L))
          throw std::runtime_error(
              "Expanded Bose sum disagrees with the full expression");
        peak = std::max(diff, peak);
      }
      const std::string policy = physical ? "physical" : "spurious";
      auto out = std::filesystem::path(argv[1]) / (policy + std::to_string(h));
      std::ofstream(out.string() + ".expr") << a.format(form->kernel) << '\n';
      std::ofstream(out.string() + ".tex")
          << amp_expression(a.format(form->kernel), masses).latex() << '\n';
      std::cout << policy << " negatives " << h << " score "
                << expression_complexity(a, form->kernel) << " max relative "
                << peak << std::endl;
    }
}
