// Kinematic and summary invariants for the actual benchmark implementation.
/*
HEPCAT - High Energy Physics Constructive Amplitude Tool

Copyright (C) 2026 Neil Christensen

This file is part of HEPCAT.

HEPCAT is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

HEPCAT is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with HEPCAT.  If not, see <https://www.gnu.org/licenses/>.
*/

#include "benchmark.hpp"
#include <iostream>

int main() {
  using namespace hepcat_benchmark;
  try {
    std::mt19937_64 random(1729), replay(1729);
    const auto first = make_point<5>(100, Real(0.0005L), random);
    if (first != make_point<5>(100, Real(0.0005L), replay))
      throw std::runtime_error("Point generation cannot be replayed.");
    // Uniform massless three-body phase space: <E^2>/s = 1/8.
    Real energy = 0, squared = 0, z = 0;
    const int samples = 12000;
    for (int index = 0; index < samples; ++index) {
      const auto point = make_point<5>(100, Real(0.0005L), random);
      energy += point[2][0] / 100;
      squared += point[2][0] * point[2][0] / 10000;
      z += point[2][3] / point[2][0];
      for (const auto& leg : point)
        for (const auto component : leg)
          if (component != Real(double(component))) throw std::runtime_error("Point not double quantized.");
    }
    if (std::abs(energy / samples - Real(1) / 3) > .004L
        || std::abs(squared / samples - Real(1) / 8) > .003L
        || std::abs(z / samples) > .025L)
      throw std::runtime_error("RAMBO distribution/isotropy regression.");
    for (int index = 0; index < 100; ++index) {
      const auto two = make_point<4>(100, 1, random);
      if (std::abs(two[2][0] - 50) > 1e-9L) throw std::runtime_error("Two-body energy regression.");
      make_point<8>(100, 1, random);
    }
    if (median({4, 2, 3, 1}) != 2.5 || median({4, 1, 2}) != 2
        || relative_error(0, 0) != 0 || relative_error(2, 1) != .5L)
      throw std::runtime_error("Timing summary regression.");
    bool rejected = false;
    try { make_point<4>(1, 1, random); } catch (const std::invalid_argument&) { rejected = true; }
    if (!rejected) throw std::runtime_error("Below-threshold energy accepted.");
    rejected = false;
    try { relative_error(-1, 1); } catch (const std::runtime_error&) { rejected = true; }
    if (!rejected) throw std::runtime_error("Invalid squared amplitude accepted.");
    std::cout << "PASS: RAMBO shells/conservation, distribution, replay, precision and timing summaries\n";
    return 0;
  } catch (const std::exception& error) {
    std::cerr << error.what() << '\n';
    return 1;
  }
}
