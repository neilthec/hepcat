// Produce ordered charged lines independently from HEPCAT's CalcHEP tree
// planner. Run from the repository root and pipe stdout to nine-point-orbits.py
// --check-trees.
#include "generation_plan.hpp"
#include "model.hpp"
#include <algorithm>
#include <iostream>
#include <map>
#include <stdexcept>

int main() {
  const hepcat::model rules("tests/reference-models/calchep", 2);
  for (int pairs = 1; pairs <= 4; ++pairs) {
    std::vector<std::string> legs(9, "A");
    for (int i = 0; i < 2 * pairs; ++i)
      legs[i] = i % 2 ? "E" : "e";
    if (pairs == 2)
      std::swap(legs[2], legs[3]);
    for (const auto &plan : hepcat::generation::diagram_plans(rules, legs)) {
      if (plan.terms.size() != 1)
        throw std::logic_error("Unexpected QED internal species");
      const auto &tree = plan.terms.front();
      std::map<int, int> start, other;
      for (size_t v = 0; v < tree.vertices.size(); ++v) {
        const auto &vert = tree.vertices[v];
        if (rules.vertices().at(vert.row).legs !=
            std::vector<std::string>{"E", "e", "A"})
          throw std::logic_error("Unexpected QED vertex");
        start.emplace(vert.slots[0], v);
      }
      std::vector<int> ends;
      std::vector<std::vector<int>> words;
      for (int i = 0; i < 2 * pairs; ++i) {
        if (legs[i] != "E")
          continue;
        std::vector<int> word;
        int port = i + 1;
        do {
          const auto &vert = tree.vertices.at(start.at(port));
          word.push_back(vert.slots[2]);
          port = vert.slots[1];
        } while (port < 0);
        for (int link : word)
          if (link < 0)
            other[link] += i + 1;
        ends.push_back(port);
        words.push_back(word);
      }
      // Each exchanged photon is identified by the other line's positron.
      std::cout << pairs;
      int line = 0;
      for (int i = 0; i < 2 * pairs; ++i) {
        if (legs[i] != "E")
          continue;
        std::cout << ' ' << ends.at(line) << ':';
        for (int x : words.at(line))
          std::cout << (x > 0 ? x : i + 1 - other.at(x)) << ',';
        ++line;
      }
      std::cout << '\n';
    }
  }
}
