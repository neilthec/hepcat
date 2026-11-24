// Independently check frozen constructive spinor expressions and their printed
// reference choices. See constructive-reconstruction.md for compilation and
// use.
#include "model.hpp"
#include "validation_dirac_current.hpp"
#include "validation_numeric.hpp"
#include "validation_precision_parameters.hpp"
#include <filesystem>
#include <fstream>
#include <iostream>
#include <iterator>
#include <numeric>
#include <set>
#include <sstream>
using namespace hepcat;
using namespace hepcat::validation;
struct image {
  int sign;
  std::vector<unsigned> permutation;
};
struct seed {
  expression_evaluator evaluate;
  std::vector<image> images;
};
std::vector<seed> load(const std::filesystem::path &dir, const model &inputs,
                       const std::vector<Id> &masses, bool printed) {
  const auto parameters = model_parameters(inputs);
  const auto exact = precise_model_parameters(inputs);
  std::vector<seed> out;
  for (unsigned j = 1;
       std::filesystem::exists(dir / ("seed-" + std::to_string(j) + ".expr"));
       ++j) {
    const auto prefix = dir / ("seed-" + std::to_string(j));
    std::ifstream src(prefix.string() + ".expr");
    std::string txt((std::istreambuf_iterator<char>(src)), {});
    if (printed) {
      for (const std::string chirality : {"Angle", "Square"}) {
        const std::string from = "Spinor[\"Xi\",\"" + chirality + "\",901]",
                          to = "Spinor[\"Helicity\",\"" + chirality + "\",10]";
        for (size_t pos = 0; (pos = txt.find(from, pos)) != std::string::npos;
             pos += to.size())
          txt.replace(pos, from.size(), to);
      }
      if (txt.find("\"Xi\"") != std::string::npos)
        throw std::runtime_error("Unexpected reference in saved expression");
    }
    Algebra a(&inputs.algebra());
    a.setKinematics(masses, {});
    seed item{expression_evaluator(a, a.parse(txt), parameters, {}, &exact),
              {}};
    std::ifstream permutations(prefix.string() + ".permutations");
    int sign;
    while (permutations >> sign) {
      if (sign != 1 && sign != -1)
        throw std::runtime_error("Invalid permutation sign");
      image entry{sign, {}};
      std::set<unsigned> used;
      for (unsigned k = 0; k < masses.size(); ++k) {
        unsigned target;
        if (!(permutations >> target) || target == 0 ||
            target > masses.size() || !used.insert(target - 1).second)
          throw std::runtime_error("invalid permutation");
        entry.permutation.push_back(target - 1);
      }
      item.images.push_back(entry);
    }
    out.push_back(std::move(item));
  }
  if (out.empty())
    throw std::runtime_error("No frozen spinor seed");
  return out;
}
template <class Chart>
Chart image_of(const Chart &p, const std::vector<unsigned> &perm) {
  auto q = p;
  const auto n = perm.size();
  for (unsigned i = 0; i < n; ++i) {
    const auto j = perm[i];
    q.momenta[i] = p.momenta[j];
    q.masses[i] = p.masses[j];
    q.angles[i] = p.angles[j];
    q.squares[i] = p.squares[j];
    q.angle_basis[i] = p.angle_basis[j];
    q.square_basis[i] = p.square_basis[j];
    for (unsigned k = 0; k < n; ++k)
      q.scalar_products[i * p.masses.size() + k] =
          p.scalar_products[j * p.masses.size() + perm[k]];
  }
  return q;
}
template <class Chart, class Value, class Probe>
Chart with_printed_reference(const Chart &original,
                             const std::string &helicities,
                             const Probe &sandwich, const Value &one) {
  auto p = original;
  // The extra index holds two endpoint spinors only. It contributes no external
  // momentum.
  const auto n = p.masses.size();
  p.masses.push_back(0);
  p.angles.push_back({});
  p.squares.push_back({});
  p.scalar_products.assign((n + 1) * (n + 1), 0);
  for (size_t i = 0; i < n; ++i)
    for (size_t j = 0; j < n; ++j)
      p.scalar_products[i * (n + 1) + j] = original.scalar_products[i * n + j];
  if (helicities == "+++") {
    // <r a> = [9|(p1+p2)|a>. Probe the linear form on the two coordinate
    // spinors.
    p.angles.back() = {one, Value{}};
    const auto first = sandwich(p);
    p.angles.back() = {Value{}, one};
    const auto second = sandwich(p);
    p.angles.back() = {-second, first};
  } else if (helicities == "++-") {
    p.angles.back() = p.angles[8];
    p.squares.back() = p.squares[6];
  } else
    throw std::invalid_argument("Printed references are for +++ and ++-");
  return p;
}
int main(int argc, char **argv) try {
  if (argc != 7)
    throw std::invalid_argument(
        "Use MODEL_DIR seeds points firstseed helicities sampled|printed");
  model v(argv[1], 3, "qed-reconstruction");
  std::string helicities = argv[5];
  std::ifstream manifest(std::filesystem::path(argv[2]) / "checked-order.txt");
  std::string listed;
  for (std::string expected : {"e", "E", "e", "E", "e", "E"})
    if (!(manifest >> listed) || listed != expected)
      throw std::invalid_argument("Missing or incorrect seed inventory");
  for (char h : helicities)
    if (!(manifest >> listed) || listed != std::string("A.") + h)
      throw std::invalid_argument(
          "Helicity differs from the frozen construction");
  size_t expected_seeds, expected_sewings;
  if (!(manifest >> expected_seeds >> expected_sewings))
    throw std::invalid_argument("Incomplete seed inventory");
  std::vector<int> roles{1, -1, 1, -1, 1, -1};
  for (char h : helicities)
    roles.push_back(h == '+' ? 2 : -2);
  auto symbol = [&](const std::string &name) {
    for (const auto &var : v.variables())
      if (var.name == name)
        return var.symbol;
    throw std::runtime_error(name);
  };
  const auto params = model_parameters(v);
  const auto exact = precise_model_parameters(v);
  const auto exact_m = precise_scalar(v.algebra(), symbol("Me"), exact).x;
  const auto m = evaluate_scalar(v.algebra(), symbol("Me"), params).real(),
             e = evaluate_scalar(v.algebra(), symbol("EE"), params).real();
  std::vector<real> masses(roles.size());
  std::vector<Id> mids;
  Algebra a(&v.algebra());
  for (size_t j = 0; j < roles.size(); ++j) {
    if (abs(roles[j]) == 1)
      masses[j] = m;
    mids.push_back(abs(roles[j]) == 1 ? symbol("Me") : a.integer(0));
  }
  const std::string gauge = argv[6];
  if (gauge != "printed" && gauge != "sampled")
    throw std::invalid_argument("Unknown reference choice");
  const bool printed = gauge == "printed";
  auto fixed = load(argv[2], v, mids, printed);
  size_t sewings = 0;
  for (auto &term : fixed)
    sewings += term.images.size();
  if (fixed.size() != expected_seeds || sewings != expected_sewings ||
      sewings != 4536)
    throw std::runtime_error("Incomplete images of the fixed seed amplitudes");
  auto id = a.parse(
      R"(SpinorChain[Spinor["Helicity","Square",9],Mom[Multiparticle[1,2]],Spinor["Helicity","Angle",10]])");
  expression_evaluator sandwich(a, id, params, {}, &exact);
  std::cout << "Compiled " << fixed.size()
            << " frozen spinor seeds. Starting independent signed checks.\n"
            << std::flush;
  dirac_current::recursion independent(roles);
  real worst = 0;
  size_t fails = 0, replays = 0;
  real max_replay = 0;
  std::vector<precision::scalar> wide_masses;
  for (auto role : roles)
    wide_masses.push_back(std::abs(role) == 1 ? exact_m
                                              : precision::wide_real(0));
  const int count = std::stoi(argv[3]);
  if (count < 1)
    throw std::invalid_argument("At least one sampled point is required");
  auto start = std::stoull(argv[4]);
  for (int i = 0; i < count; ++i) {
    auto p = sample_point(masses, start + i);
    auto evaluation =
        printed ? with_printed_reference(p, helicities, sandwich, complex(1))
                : p;
    complex sum{}, compensation{};
    for (const auto &term : fixed)
      for (const auto &im : term.images) {
        auto value =
            real(im.sign) * term.evaluate(image_of(evaluation, im.permutation));
        auto y = value - compensation, t = sum + y;
        compensation = (t - sum) - y;
        sum = t;
      }
    auto ref = independent(p, e, dirac_current::gauge::independent);
    const auto scale = std::max(std::abs(ref), std::abs(sum)),
               err = std::abs(ref - sum) / scale;
    if (!std::isfinite(err))
      throw std::runtime_error("Nonfinite signed comparison");
    if (err > 1e-7L) {
      ++replays;
      auto chart = precision::reconstruct(p, wide_masses);
      const auto probe = [&](const auto &q) { return sandwich.precise(p, q); };
      if (printed)
        chart = with_printed_reference(chart, helicities, probe,
                                       precision::number(complex(1)));
      precision::number total;
      for (const auto &term : fixed)
        for (const auto &im : term.images) {
          const auto wide_value =
              term.evaluate.precise(p, image_of(chart, im.permutation));
          total += im.sign < 0 ? -wide_value : wide_value;
        }
      const auto narrowed = total.narrow();
      const auto replay = std::abs(narrowed - ref) /
                          std::max(std::abs(narrowed), std::abs(ref));
      if (!std::isfinite(replay) || replay > 1e-7L)
        ++fails;
      max_replay = std::max(max_replay, replay);
      std::cout << "Seed " << start + i
                << " 160-bit constructive vs native Feynman " << replay << "\n";
    }
    if (i < 3 || err > 1e-7L)
      std::cout << start + i << " constructive " << sum << " independent "
                << ref << " relative " << err << "\n"
                << std::flush;
    worst = std::max(worst, err);
  }
  std::cout << "Final worst relative " << worst << " candidate replays "
            << replays << " max replay vs native Feynman " << max_replay
            << " failures " << fails << "\n";
  return fails ? 2 : 0;
} catch (const std::exception &e) {
  std::cerr << e.what() << "\n";
  return 1;
}
