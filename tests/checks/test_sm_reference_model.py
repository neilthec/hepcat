"""The independent massive-light-quark UFO retains its masses and Yukawas."""

# HEPCAT - High Energy Physics Constructive Amplitude Tool
#
# Copyright (C) 2026 Neil Christensen
#
# This file is part of HEPCAT.
#
# HEPCAT is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# HEPCAT is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with HEPCAT.  If not, see <https://www.gnu.org/licenses/>.


import json
from pathlib import Path
import subprocess
import sys
import unittest


ROOT = Path(__file__).resolve().parents[2]
MODEL = ROOT / "tests" / "reference-models" / "madgraph" / "sm"


class MassiveSMReferenceTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        # Import the UFO in a fresh interpreter: its standard module names must
        # not clash with another model imported by an unrelated test.
        probe = r'''
import cmath, json, sys
sys.path.insert(0, sys.argv[1])
import parameters, particles, vertices, couplings, function_library
from object_library import all_parameters, all_particles, all_vertices
values = {f.name: f for f in function_library.all_functions} if hasattr(function_library, 'all_functions') else {}
values.update({name: getattr(function_library,name) for name in dir(function_library)
               if callable(getattr(function_library,name))})
for p in all_parameters:
    if p.nature == 'external': values[p.name] = complex(p.value)
pending = [p for p in all_parameters if p.nature == 'internal']
while pending:
    progress = []
    for p in pending:
        try: values[p.name] = complex(eval(p.value, {'cmath': cmath, 'complex': complex}, values))
        except NameError: continue
        progress.append(p)
    if not progress: raise RuntimeError('Unresolved UFO parameter dependencies')
    pending = [p for p in pending if p not in progress]
rows = []
for pdg, label in [(1, 'd'), (2, 'u'), (3, 's')]:
    particle = next(p for p in all_particles if p.pdg_code == pdg)
    higgs = [v for v in all_vertices if sorted(p.pdg_code for p in v.particles) == [-pdg, pdg, 25]]
    coupling = next(iter(higgs[0].couplings.values()))
    actual = eval(coupling.value, {'cmath': cmath, 'complex': complex}, values)
    expected = -1j * values['y'+label]/cmath.sqrt(2)
    rows.append({'pdg': pdg, 'mass': values[particle.mass.name].real,
      'yukawa_mass': values['ym'+label].real, 'higgs_count': len(higgs),
      'structure': [x.name for x in higgs[0].lorentz],
      'yukawa_error': abs(actual-expected)})
print(json.dumps(rows))
'''
        completed = subprocess.run([sys.executable, "-c", probe, str(MODEL)],
                                   capture_output=True, text=True, check=True)
        cls.rows = json.loads(completed.stdout)

    def test_masses_and_yukawa_inputs_match_constructive_defaults(self):
        expected = {1: 0.00470, 2: 0.00216, 3: 0.095}
        for row in self.rows:
            with self.subTest(pdg=row["pdg"]):
                self.assertEqual(row["mass"], expected[row["pdg"]])
                self.assertEqual(row["yukawa_mass"], row["mass"])

    def test_each_light_flavor_has_one_standard_higgs_vertex(self):
        for row in self.rows:
            with self.subTest(pdg=row["pdg"]):
                self.assertEqual(row["higgs_count"], 1)
                self.assertEqual(row["structure"], ["FFS4"])
                self.assertEqual(row["yukawa_error"], 0)

    def test_only_unitary_gauge_is_advertised(self):
        self.assertIn("gauge = [0]", (MODEL / "__init__.py").read_text())
        self.assertFalse((MODEL / "restrict_default.dat").exists())

    def test_upstream_license_is_preserved(self):
        self.assertIn("MadTeam", (MODEL / "LICENSE").read_text())


if __name__ == "__main__":
    unittest.main()
