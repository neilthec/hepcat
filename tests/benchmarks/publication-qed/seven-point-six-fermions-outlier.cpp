// Replay compact six-fermion amplitudes at the actual external validation momenta.
// Build from the repository root against the native release library. On macOS,
// include the GMP headers and library directories from Homebrew if necessary.
// c++ -O2 -std=c++17 -Icsource tests/benchmarks/publication-qed/seven-point-six-fermions-outlier.cpp \
//     tests/results/native-reduce-build/libhepcat_reduce.a -lgmpxx -lgmp -o replay
// ./replay physical/ spurious/ outliers.csv > precise.tsv
//
// Each expression directory contains +.expr and -.expr. Generate each with
// seven-point-six-fermions.py. Its --raw-expanded option reproduces the direct
// orientation sum and --expanded groups equal spurious poles before division.
// The CSV must have a point or index field and p1_0 through p7_3 momentum
// components (E, px, py, pz) in GeV. The first two positive-energy legs are the
// incoming CM beams. MadGraph and CalcHEP outlier files use this layout.
#include "model.hpp"
#include "validation_numeric.hpp"
#include "validation_precision.hpp"
#include <algorithm>
#include <array>
#include <cmath>
#include <limits>
#include <filesystem>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>
namespace v = hepcat::validation;
namespace hp = v::precision;
using hp::scalar;
using Four = hp::four;
using Momenta = std::vector<Four>;
namespace fs = std::filesystem;

scalar spatial2(const Four &p) { return p[1] * p[1] + p[2] * p[2] + p[3] * p[3]; }
struct kinematic_errors {
  scalar shell_gev2{}, conservation_gev{};
};
kinematic_errors defects(const Momenta &p, const std::vector<scalar> &masses) {
  kinematic_errors result;
  Four total{};
  for (size_t i = 0; i < p.size(); ++i) {
    const auto &k = p[i];
    const scalar shell = abs(k[0] * k[0] - spatial2(k) - masses[i] * masses[i]);
    result.shell_gev2 = std::max(result.shell_gev2, shell);
    for (size_t j = 0; j < 4; ++j)
      total[j] += k[j];
  }
  for (const auto &component : total)
    result.conservation_gev = std::max(result.conservation_gev, scalar(abs(component)));
  return result;
}

std::vector<std::string> fields(std::string line) {
  if (!line.empty() && line.back() == '\r')
    line.pop_back();
  std::istringstream input(line);
  std::vector<std::string> result;
  std::string field;
  while (std::getline(input, field, ','))
    result.push_back(field);
  return result;
}
// Keep the sampled CM beam axis and magnitude. Enforce spatial closure on the
// hardest outgoing momentum, then rescale all outgoing three-vectors together
// to conserve energy with every final leg on shell.
Momenta conserved(Momenta p, const std::vector<scalar> &m) {
  for (size_t j = 1; j < 4; ++j)
    p[1][j] = -p[0][j];
  for (size_t i = 0; i < 2; ++i)
    p[i][0] = sqrt(spatial2(p[i]) + m[i] * m[i]);
  const scalar total = p[0][0] + p[1][0];
  size_t pivot = 2;
  for (size_t i = 3; i < 7; ++i)
    if (abs(p[i][0]) > abs(p[pivot][0]))
      pivot = i;
  for (size_t j = 1; j < 4; ++j) {
    p[pivot][j] = 0;
    for (size_t i = 2; i < 7; ++i)
      if (i != pivot)
        p[pivot][j] -= p[i][j];
  }
  std::array<scalar, 7> sq;
  for (size_t i = 2; i < 7; ++i)
    sq[i] = spatial2(p[i]);
  auto sum = [&](const scalar &lambda) {
    scalar e = 0;
    for (size_t i = 2; i < 7; ++i)
      e += sqrt(lambda * lambda * sq[i] + m[i] * m[i]);
    return e;
  };
  scalar lo = 0, hi = 2;
  if (sum(lo) > total || sum(hi) < total)
    throw std::runtime_error("Invalid physical CM point");
  for (size_t it = 0; it < hp::bits + 16; ++it) {
    scalar mid = (lo + hi) / 2;
    if (sum(mid) > total)
      hi = mid;
    else
      lo = mid;
  }
  for (size_t i = 2; i < 7; ++i) {
    for (size_t j = 1; j < 4; ++j)
      p[i][j] *= hi;
    p[i][0] = -sqrt(spatial2(p[i]) + m[i] * m[i]);
  }
  return p;
}

int main(int argc, char **argv) try {
  if (argc != 4)
    throw std::invalid_argument("Usage: replay PHYSICAL_DIR SPURIOUS_DIR OUTLIERS.csv");
  mpf_set_default_prec(hp::bits);
  hepcat::model model("models", 3);
  hepcat::Algebra a(&model.algebra());
  const auto mass = a.parse(model.particle("e").mass), zero = a.integer(0);
  a.setKinematics({mass, mass, mass, mass, mass, mass, zero}, {});
  const auto pars = v::model_parameters(model);
  std::vector<scalar> m(7, hp::wide_real(v::evaluate_scalar(a, mass, pars).real()));
  m[6] = 0;
  std::vector<v::expression_evaluator> eval;
  for (const auto &folder : {fs::path(argv[1]), fs::path(argv[2])})
    for (auto helicity : {"-", "+"}) {
      const auto path = folder / (std::string(helicity) + ".expr");
      std::ifstream in(path);
      if (!in)
        throw std::runtime_error("Missing " + path.string());
      std::ostringstream text;
      text << in.rdbuf();
      eval.emplace_back(a, a.parse(text.str()), pars);
    }
  std::ifstream data(argv[3]);
  std::string line;
  if (!std::getline(data, line))
    throw std::runtime_error("Missing CSV header");
  const auto header = fields(line);
  auto column = [&](const std::string &name) {
    const auto it = std::find(header.begin(), header.end(), name);
    if (it == header.end())
      throw std::runtime_error("Missing CSV field " + name);
    return size_t(it - header.begin());
  };
  const auto point_column =
      column(std::find(header.begin(), header.end(), "point") != header.end() ? "point" : "index");
  std::array<std::array<size_t, 4>, 7> momenta_columns{};
  for (size_t i = 0; i < 7; ++i)
    for (size_t j = 0; j < 4; ++j)
      momenta_columns[i][j] = column("p" + std::to_string(i + 1) + "_" + std::to_string(j));
  std::cout << "point\tchart\tmax_shift_GeV\tshell_error_GeV2\tconservation_error_GeV"
            << "\tphysical_minus\tphysical_plus\tspurious_minus\tspurious_plus"
            << "\trelative_minus\trelative_plus\n"
            << std::scientific << std::setprecision(23);
  while (std::getline(data, line)) {
    if (line.empty())
      continue;
    const auto row = fields(line);
    Momenta p(7);
    for (size_t i = 0; i < 7; ++i)
      for (size_t j = 0; j < 4; ++j) {
        const auto &token = row.at(momenta_columns[i][j]);
        size_t used = 0;
        const double x = std::stod(token, &used);
        if (used != token.size() || !std::isfinite(x) || (j == 0 && x <= 0))
          throw std::runtime_error("Invalid external momentum " + token);
        // Decode the saved binary64 point exactly, then cross outgoing legs.
        p[i][j] = hp::wide_real(i < 2 ? x : -x);
      }
    for (bool project : {false, true}) {
      const auto momenta = project ? conserved(p, m) : p;
      scalar shift = 0;
      for (size_t i = 0; i < 7; ++i)
        for (size_t j = 0; j < 4; ++j)
          shift = std::max(shift, scalar(abs(momenta[i][j] - p[i][j])));
      const scalar beam_energy = p[0][0] + p[1][0];
      if (shift > 4096 * std::numeric_limits<double>::epsilon() * beam_energy)
        throw std::runtime_error(
            "Projection exceeds component roundoff. Check the masses, units and beam order.");
      auto chart = hp::from_momenta(momenta, m, 0);
      v::point metadata;
      metadata.momenta.resize(7);
      std::array<scalar, 4> sum{};
      for (unsigned state = 0; state < 64; ++state) {
        for (size_t i = 0; i < 6; ++i) {
          const auto spin = (state >> i) & 1;
          chart.angles[i] = chart.angle_basis[i][spin];
          chart.squares[i] = chart.square_basis[i][spin];
        }
        for (size_t f = 0; f < eval.size(); ++f) {
          const auto value = eval[f].precise(metadata, chart);
          sum[f] += value.x * value.x + value.y * value.y;
        }
      }
      // The reference exporter averages the two initial spins and divides by
      // each of the two identical pairs in the final state: (1/4)(1/2!)(1/2!).
      for (auto &value : sum)
        value /= 16;
      const auto check = defects(momenta, m);
      std::cout << row.at(point_column) << '\t' << (project ? "conserved" : "unchanged") << '\t' << shift
                << '\t' << check.shell_gev2 << '\t' << check.conservation_gev;
      for (const auto &value : sum)
        std::cout << '\t' << value;
      for (size_t i = 0; i < 2; ++i) {
        const scalar scale = std::max(sum[i], sum[i + 2]);
        const scalar relative = scale == 0 ? scalar(0) : scalar(abs(sum[i] - sum[i + 2]) / scale);
        std::cout << '\t' << relative;
      }
      std::cout << '\n' << std::flush;
    }
  }
  if (data.bad())
    throw std::runtime_error("Failed to read the point file");
} catch (const std::exception &e) {
  std::cerr << e.what() << '\n';
  return 1;
}
