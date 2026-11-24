// From a HEPCAT checkout, prepare a new working directory with
//   python3 tests/checks/reconstruction-block-inputs.py . tests/results/charged
// Then run hepcat-reconstruct-charged WORK OUTPUT POINTS LEG...
// OUTPUT is a Shared[...] model expression. A completed run checks every cut
// and the independent signed Feynman amplitude before writing OUTPUT.
#include "amplitude.hpp"
#include "generation_sew.hpp"
#include "model_expression.hpp"
#include <algorithm>
#include <filesystem>
#include <fstream>
#include <functional>
#include <iostream>
#include <unordered_map>

using namespace hepcat;
namespace fs = std::filesystem;
namespace {

// A single reference for the whole sum, r = (p_first + p_second)|k] for
// angle endpoints, and its parity partner for square endpoints. The common
// null leg k can be used for every photon, including k itself.
Id common_reference(Algebra &a, Id root, int first, int second, int photon) {
  std::unordered_map<Id, Id> memo;
  const Id momentum = a.call(
      "Mom", {a.call("Multiparticle", {a.integer(first), a.integer(second)})});
  const Id xi = a.atom("\"Xi\""), square = a.atom("\"Square\""),
           angle = a.atom("\"Angle\"");
  auto visit = [&](auto &self, Id id) -> Id {
    if (auto found = memo.find(id); found != memo.end())
      return found->second;
    const auto &n = a.node(id);
    if (!n.compound || n.head == "ModelParameter")
      return id;
    std::vector<Id> args;
    if (n.head == "SpinorChain") {
      int sign = 1;
      for (std::size_t j = 0; j < n.args.size(); ++j) {
        const auto &sp = a.node(n.args[j]);
        if (sp.head != "Spinor" || sp.args[0] != xi) {
          args.push_back(n.args[j]);
          continue;
        }
        const auto h = a.call("Spinor", {a.atom("\"Helicity\""),
                                         sp.args[1] == angle ? square : angle,
                                         a.integer(photon)});
        if (j == 0) {
          args.push_back(h);
          args.push_back(momentum);
        } else {
          args.push_back(momentum);
          args.push_back(h);
          sign = -sign;
        }
      }
      return memo[id] =
                 a.mul({a.integer(sign), a.call(n.head, std::move(args))});
    }
    for (Id child : n.args)
      args.push_back(self(self, child));
    return memo[id] = a.call(n.head, std::move(args));
  };
  return visit(visit, root);
}

// Write the arithmetic tree in the existing model dialect, visiting every
// distinct subexpression once. Check read-back using the original model scope.
std::string encode(Algebra &a, Id root, const model &m,
                   const std::vector<bool> &massive) {
  std::unordered_map<Id, std::size_t> known;
  std::vector<std::string> records;
  std::function<std::string(Id)> leaf = [&](Id id) {
    const auto &n = a.node(id);
    mpq_class q;
    if (a.number(id, q))
      return q.get_den() == 1 ? q.get_str() : "(" + q.get_str() + ")";
    if (n.head == "ModelParameter") {
      auto name = a.node(n.args.front()).head;
      return name.substr(1, name.size() - 2);
    }
    if (!n.compound)
      return n.head;
    std::string out = n.head + "[";
    for (std::size_t j = 0; j < n.args.size(); ++j) {
      if (j)
        out += ",";
      out += leaf(n.args[j]);
    }
    return out + "]";
  };
  auto visit = [&](auto &self, Id id) -> std::size_t {
    if (auto found = known.find(id); found != known.end())
      return found->second;
    const auto &n = a.node(id);
    std::string record;
    if (n.head == "Times" || n.head == "Plus" || n.head == "Power") {
      record = n.head == "Times" ? "*:" : n.head == "Plus" ? "+:" : "^:";
      for (std::size_t j = 0; j < n.args.size(); ++j) {
        if (j)
          record += ",";
        record += std::to_string(self(self, n.args[j]));
      }
    } else
      record = leaf(id);
    records.push_back(std::move(record));
    return known[id] = records.size();
  };
  visit(visit, root);
  std::string result = "Shared[";
  for (std::size_t j = 0; j < records.size(); ++j) {
    if (j)
      result += ";";
    result += records[j];
  }
  result += "]";
  const Id read = model_detail::parse_expression(
      a, result, a.atom("\"" + m.parameter_scope() + "\""), massive);
  // Arithmetic read from FullForm can retain parenthesized products. The
  // model dialect normalizes these when it reads a shared instruction.
  std::unordered_map<Id, Id> cache;
  auto canonical = [&](auto &self, Id id) -> Id {
    if (auto found = cache.find(id); found != cache.end())
      return found->second;
    const auto &n = a.node(id);
    if (!n.compound)
      return id;
    std::vector<Id> args;
    for (auto c : n.args)
      args.push_back(self(self, c));
    if (n.head == "Times")
      return cache[id] = a.mul(std::move(args));
    if (n.head == "Plus")
      return cache[id] = a.add(std::move(args));
    return cache[id] = a.call(n.head, std::move(args));
  };
  if (canonical(canonical, read) != canonical(canonical, root))
    throw std::runtime_error("Model round trip changed the expression");
  return result;
}
} // namespace

int main(int argc, char **argv) try {
  if (argc < 8)
    throw std::invalid_argument("Expected working directory, new output file, "
                                "point count, and external particles");
  const fs::path work = argv[1], output = argv[2];
  if (fs::exists(output))
    throw std::runtime_error("Output already exists");
  validation_options options;
  options.points = std::stoul(argv[3]);
  options.workers = 1;
  options.seed = 20261124;
  if (options.points < 10000)
    throw std::invalid_argument("Supply at least 10000 validation points");
  std::vector<std::string> legs(argv + 4, argv + argc);
  const std::string scope = "qed-reconstruction";
  model blocks(work / "charged-blocks", 3, scope);
  amplitude process(legs, model(work / "models", 3, scope),
                    model(work / "models", 4, scope),
                    generation_references::physical);
  Algebra a(&blocks.algebra());
  std::vector<Id> masses, terms;
  std::vector<bool> massive;
  std::vector<int> charged, photons;
  for (const auto &mass : process.external_masses()) {
    masses.push_back(a.parse(mass));
    massive.push_back(mass != "0");
    (massive.back() ? charged : photons)
        .push_back(static_cast<int>(masses.size()));
  }
  if (charged.size() < 2)
    throw std::invalid_argument("A charged pair is required");
  a.setKinematics(masses, {});
  auto plans = generation::charged_block_plans(blocks, legs);
  for (const auto &graph : plans)
    terms.push_back(
        generation::sew(a, blocks, legs, graph, false, false, false));
  if (terms.empty())
    throw std::runtime_error("No on-shell sewings");
  Id result = a.add(std::move(terms));
  if (!photons.empty())
    result =
        common_reference(a, result, charged[0], charged[1], photons.back());
  std::cout << plans.size()
            << " on-shell sewings. Checking the fixed expression.\n"
            << std::flush;
  const amp_expression fixed(a, result, process.external_masses());
  process.generate_factorization_products(1);
  // The independent Feynman inputs are first attached here.
  process.set_feynman_model(
      model(work / "tests/reference-models/calchep", 2, scope));
  process.generate_feynman_diagrams(1);
  const auto report = process.validate(fixed, options);
  for (const auto &row : report.checks)
    std::cout << row.kind << " " << row.label << " "
              << (row.status == check_outcome::pass ? "PASS" : "INCOMPLETE")
              << "\n";
  bool compared = false;
  for (const auto &row : report.numerical) {
    std::cout << row.kind << " " << row.compared << " " << row.failures << " "
              << row.max_relative_error << " replays " << row.replays << " "
              << row.max_replay_relative_error << "\n";
    if (row.kind == "Feynman" && row.compared == options.points &&
        row.nonzero && !row.failures)
      compared = true;
  }
  if (!report.passed() || !compared)
    return 2;
  const auto body = encode(a, result, blocks, massive);
  std::ofstream out(output);
  out.exceptions(std::ios::badbit | std::ios::failbit);
  out << body << '\n';
  out.close();
  return 0;
} catch (const std::exception &e) {
  std::cerr << e.what() << '\n';
  return 1;
}
