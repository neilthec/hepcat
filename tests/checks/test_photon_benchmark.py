"""Benchmark admission rejects incomplete or mismatched comparisons."""

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

import copy
import importlib.util
import itertools
import json
from pathlib import Path
import tempfile
import unittest

spec = importlib.util.spec_from_file_location('photon_benchmark', Path(__file__).resolve().parents[2] / 'tests/benchmarks/configure.py')
benchmark = importlib.util.module_from_spec(spec)
spec.loader.exec_module(benchmark)


class PhotonBenchmarkTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.directory = Path(self.temp.name)
        backend = {'Status': 'AVAILABLE', 'PhysicalPDGs': [11, -11, 22, 22],
                   'AllowedParticlePDGs': [11, 22], 'LegOrder': [1, 2, 3, 4],
                   'ParameterNames': ['Me', 'EE'], 'SPINASNormalizationFactor': 1}
        self.data = {'Class': 'eeAA', 'HelicityFamily': True, 'Masses': [1, 1, 0, 0],
                     'Spin2': [1, 1, 2, 2], 'IncomingLegs': [1, 2], 'Precision': 'LongDouble',
                     'ParameterOrder': ['Me', 'EE'], 'FixedHelicities': {},
                     'HelicityConfigurations': [[0, 0] + list(row) for row in itertools.product((-2, 2), repeat=2)],
                     'CalcHEPValidation': copy.deepcopy(backend), 'MadGraphValidation': copy.deepcopy(backend)}
        for name in ('process.cpp', 'process.hpp'):
            (self.directory / name).write_text('// fixture')

    def load(self):
        (self.directory / 'manifest.json').write_text(json.dumps(self.data))
        return benchmark.load_project(self.directory)

    def test_complete_family(self):
        self.assertEqual(self.load()[2], 2)

    def test_incomplete_family(self):
        self.data['HelicityConfigurations'].pop()
        with self.assertRaisesRegex(ValueError, 'all 4'):
            self.load()

    def test_duplicate_family(self):
        self.data['HelicityConfigurations'][-1] = self.data['HelicityConfigurations'][0]
        with self.assertRaisesRegex(ValueError, 'all 4'):
            self.load()

    def test_fixed_helicity(self):
        self.data['FixedHelicities'] = {'3': 2}
        with self.assertRaisesRegex(ValueError, 'all 4'):
            self.load()

    def test_requires_independent_references(self):
        self.data['MadGraphValidation']['Status'] = 'UNAVAILABLE'
        with self.assertRaisesRegex(ValueError, 'configure MadGraphValidation'):
            self.load()

    def test_requires_same_theory(self):
        self.data['CalcHEPValidation']['AllowedParticlePDGs'] = []
        with self.assertRaisesRegex(ValueError, 'explicitly restrict'):
            self.load()

    def test_no_fitted_normalization(self):
        self.data['CalcHEPValidation']['SPINASNormalizationFactor'] = 2
        with self.assertRaisesRegex(ValueError, 'normalization'):
            self.load()

    def test_parameter_order(self):
        self.data['MadGraphValidation']['ParameterNames'].reverse()
        with self.assertRaisesRegex(ValueError, 'parameter ordering'):
            self.load()


if __name__ == '__main__':
    unittest.main()
