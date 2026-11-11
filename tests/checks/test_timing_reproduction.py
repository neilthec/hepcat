"""Check timing admission, archived medians and portable figure replay.

These tests verify the reproduction tools. Synthetic benchmark files test
rejection paths; they are not substituted for the published physics records.
"""

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

from contextlib import redirect_stdout
import csv
import hashlib
import importlib.util
import itertools
import io
import json
import math
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
from types import SimpleNamespace
import unittest


BENCHMARK = Path(__file__).resolve().parents[1] / 'benchmarks'


def load(name):
    spec = importlib.util.spec_from_file_location(name.replace('-', '_'), BENCHMARK / name)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


PHOTON = load('plot-photon-timings.py')
FERMION = load('plot-four-fermion-timings.py')
COMPARE = load('compare-photon-timings.py')


def csv_rows(path):
    with path.open(newline='') as stream:
        return list(csv.DictReader(stream))


def write_csv(path, rows):
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open('w', newline='') as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)


class TimingReplayTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix='hepcat-timing-replay-')
        self.addCleanup(self.temp.cleanup)
        self.directory = Path(self.temp.name)

    def test_archived_one_pair_medians_match_raw_sweeps(self):
        rows, series, _, points, repeats = PHOTON.read_timings(BENCHMARK / 'data/timings/one-pair')
        self.assertEqual((points, repeats), (10000, 9))
        for index, row in enumerate(rows):
            for key, *_ in PHOTON.STYLES:
                self.assertEqual(float(row[key + '_seconds']), series[key]['medians'][index])

    def test_archived_four_fermion_medians_and_quartiles_match_sweeps(self):
        root = BENCHMARK / 'data/timings/two-pair'
        rows = FERMION.read_timings(root / 'timings.csv')
        FERMION.verify_repetitions(rows, root / 'timings-repetitions.csv')
        self.assertEqual([row['photons'] for row in rows], [0, 1, 2])

    def copied_one_pair(self):
        target = self.directory / 'inputs'
        shutil.copytree(BENCHMARK / 'data/timings/one-pair', target)
        return target

    def test_missing_or_duplicate_sweeps_cannot_be_plotted(self):
        root = self.copied_one_pair()
        path = root / 'physical/timings-repetitions.csv'
        rows = csv_rows(path)
        rows[-1] = rows[0]
        write_csv(path, rows)
        with self.assertRaisesRegex(ValueError, 'Incomplete or duplicate'):
            PHOTON.read_timings(root)

    def test_changed_timing_summary_is_rejected(self):
        root = self.copied_one_pair()
        path = root / 'fourway.csv'
        rows = csv_rows(path)
        rows[1]['CalcHEP_seconds'] = '1.0'
        write_csv(path, rows)
        with self.assertRaisesRegex(ValueError, 'Summary disagrees'):
            PHOTON.read_timings(root)

    def test_four_fermion_summary_must_agree_with_actual_sweeps(self):
        root = BENCHMARK / 'data/timings/two-pair'
        rows = FERMION.read_timings(root / 'timings.csv')
        rows[0]['physical_q25_seconds'] *= .9
        with self.assertRaisesRegex(ValueError, 'Summary disagrees'):
            FERMION.verify_repetitions(rows, root / 'timings-repetitions.csv')

    def test_small_runs_are_rescaled_without_claiming_more_points_were_timed(self):
        root = self.copied_one_pair()
        for path in root.rglob('*.csv'):
            rows = csv_rows(path)
            if 'points' in rows[0]:
                for row in rows:
                    row['points'] = '10'
                write_csv(path, rows)
        _, series, _, points, _ = PHOTON.read_timings(root)
        self.assertEqual(points, 10)
        self.assertAlmostEqual(series['CalcHEP']['medians'][0], .002974042 * 1000)

    def copied_archive(self):
        archive = self.directory / 'source' / 'tests/benchmarks'
        archive.mkdir(parents=True)
        shutil.copytree(BENCHMARK / 'data/timings', archive / 'data/timings')
        for name in ('plot-photon-timings.py', 'plot-four-fermion-timings.py'):
            shutil.copyfile(BENCHMARK / name, archive / name)
        return archive

    def test_analysis_replays_from_source_archive_without_git_or_original_paths(self):
        archive = self.copied_archive()
        for name in ('plot-photon-timings.py', 'plot-four-fermion-timings.py'):
            result = subprocess.run([sys.executable, '-I', str(archive / name), '--analysis-only',
                                     '--output-directory', str(self.directory / name)], cwd=self.directory,
                                    capture_output=True, text=True, timeout=20)
            self.assertEqual(result.returncode, 0, result.stderr)
        self.assertFalse((self.directory / 'source/.git').exists())

    @unittest.skipUnless(importlib.util.find_spec('matplotlib'), 'Optional Matplotlib plotting dependency not installed')
    def test_figures_are_drawn_from_the_archived_inputs(self):
        archive = self.copied_archive()
        for name, stem in (('plot-photon-timings.py', 'photon-timings'),
                           ('plot-four-fermion-timings.py', 'four-fermion-timings')):
            result = subprocess.run([sys.executable, str(archive / name), '--output-directory',
                                     str(self.directory)], cwd=self.directory,
                                    capture_output=True, text=True, timeout=30)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertTrue((self.directory / (stem + '.pdf')).read_bytes().startswith(b'%PDF'))
            self.assertGreater((self.directory / (stem + '.png')).stat().st_size, 10000)


class PairedBenchmarkTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix='hepcat-paired-timing-')
        self.addCleanup(self.temp.cleanup)
        self.directory = Path(self.temp.name)
        self.args = SimpleNamespace(physical=self.directory / 'physical/timings',
                                    spurious=self.directory / 'spurious/timings',
                                    output=self.directory / 'result/fourway', points=2, tolerance=1e-7,
                                    physical_exports=self.directory / 'physical/exports',
                                    spurious_exports=self.directory / 'spurious/exports')
        for representation, prefix, exports in (
                ('AllPhysical', self.args.physical, self.args.physical_exports),
                ('AllSpurious', self.args.spurious, self.args.spurious_exports)):
            self.make_run(representation, prefix, exports)

    def make_run(self, representation, prefix, exports):
        prefix.parent.mkdir(parents=True)
        model_number = 4 if representation == 'AllPhysical' else 5
        metadata = {'Status': 'COMPLETE', 'ValidatedRows': 'PASS',
                    'RequestedPhotonCounts': [2, 3, 4], 'CompletedPhotonCounts': [2, 3, 4],
                    'Points': 2, 'Repetitions': 1, 'Tolerance': 1e-7,
                    'SPINASScalarType': 'double', 'SPINASPrecisionBits': 53, 'ReferencePrecision': 'double',
                    'Build': {'Exports': []}}
        metadata.update({key: 'fixture' for key in (
            'Seed', 'MultiplicitySeed', 'Warmup', 'CenterOfMassEnergyGeV', 'Sampling', 'Normalization',
            'MadGraphSum', 'ParametersByMultiplicity', 'Timing', 'Excluded', 'System')})
        metadata['Build'].update({key: 'fixture' for key in (
            'Compiler', 'CompilerVersion', 'CXXFlags', 'SPINASLibrarySHA256', 'CalcHEPDirectory',
            'ReferenceOptimization', 'ModelSHA256', 'CalcHEPLibrarySHA256', 'CompilerConfigurations',
            'SupportSHA256')})
        repeated = []
        for n in (2, 3, 4):
            project = exports / ('ee' + 'A' * n)
            project.mkdir(parents=True)
            backend = {'AllowedParticlePDGs': [11, 22], 'PhysicalPDGs': [11, -11] + [22] * n,
                       'ParameterNames': ['Me', 'EE'], 'SPINASNormalizationFactor': 1,
                       'Normalization': 1 / (4 * math.factorial(n))}
            manifest = {'HelicityConfigurations': [[0, 0] + list(row)
                        for row in itertools.product((-2, 2), repeat=n)], 'HelicityFamily': True,
                        'FixedHelicities': {}, 'IncomingLegs': [1, 2], 'ParameterOrder': ['Me', 'EE'],
                        'ParameterDefaults': [1, 1], 'CalcHEPValidation': backend,
                        'MadGraphValidation': backend}
            (project / 'manifest.json').write_text(json.dumps(manifest))
            (project / 'process.cpp').write_text('// synthetic admission-test fixture\n')
            source = {'Representation': representation, 'ModelNumber': model_number,
                      'CompletePhotonHelicityCount': 2 ** n,
                      'ReferenceConfigurationSHA256': 'shared-fixture',
                      'InputHelicitySectors': [{'Particles': ['e', 'E'] + ['A.+'] * (n - neg) + ['A.-'] * neg,
                                               'ExpressionSHA256': 'fixture'} for neg in range(n + 1)],
                      'ModelFilesSHA256': {f'{stem}{model_number}.mdl': 'fixture'
                                          for stem in ('amps', 'prtcls', 'vars', 'func')}}
            metadata['Build']['Exports'].append({'N': n, 'Precision': 'Double', 'CodeStyle': 'Optimized',
                'AmplitudeSource': source, 'Export': '/original-machine/exports/' + project.name,
                'SHA256': {name: hashlib.sha256((project / name).read_bytes()).hexdigest()
                           for name in ('manifest.json', 'process.cpp')}})
            for backend_name in ('SPINAS', 'CalcHEP', 'MadGraph'):
                # Make second-run references faster: the merger must still use
                # the designated physical-run times rather than cherry-pick.
                seconds = n * .1
                if representation == 'AllSpurious' and backend_name != 'SPINAS':
                    seconds *= .5
                repeated.append({'photons': n, 'backend': backend_name, 'repetition': 1,
                                 'points': 2, 'seconds': seconds})
            point = COMPARE.file_for(prefix, f'-points-N{n}.csv')
            point.write_text('point,momentum\n1,1\n2,2\n')
            write_csv(COMPARE.file_for(prefix, f'-values-N{n}.csv'),
                      [{'point': i, 'spinas': i, 'calchep': i, 'madgraph': i} for i in (1, 2)])
            report = {key: 'fixture' for key in (
                'ExternalCount', 'Process', 'PhysicalPDGs', 'Masses', 'ExcludedParticles',
                'DiagramCount', 'MadGraphModel', 'MadGraphVersion', 'MadGraphModelPath',
                'MadGraphRestriction', 'FortranCompiler', 'ParameterMatching', 'MassMatching',
                'WidthPolicy', 'ZeroedWidths', 'PreservedWidths', 'ParameterRules', 'CouplingOrders',
                'Scope', 'Normalization', 'SpinSum', 'FortranFlags')}
            report.update(Status='READY', HelicityStates=4 * 2 ** n,
                          AllowedParticlePDGs=[11, 22], OptimizationLevel=3)
            COMPARE.file_for(prefix, f'-madgraph-N{n}.json').write_text(json.dumps(report))
        COMPARE.file_for(prefix, '-metadata.json').write_text(json.dumps(metadata))
        write_csv(COMPARE.file_for(prefix, '-repetitions.csv'), repeated)

    def test_verified_paired_runs_support_relocated_exports_and_fixed_reference_times(self):
        with redirect_stdout(io.StringIO()):
            COMPARE.compare(self.args)
        row = csv_rows(Path(str(self.args.output) + '.csv'))[0]
        self.assertEqual(float(row['CalcHEP_seconds']), .2)
        self.assertEqual(float(row['CalcHEP_relative_N2']), 1)

    def test_legacy_model_numbers_are_read_as_declared_historical_provenance(self):
        for prefix in (self.args.physical, self.args.spurious):
            path = COMPARE.file_for(prefix, '-metadata.json')
            metadata = json.loads(path.read_text())
            for item in metadata['Build']['Exports']:
                source = item['AmplitudeSource']
                source['ModelNumber'] -= 1
                source['ModelFilesSHA256'] = {
                    f"{stem}{source['ModelNumber']}.mdl": 'fixture'
                    for stem in ('amps', 'prtcls', 'vars', 'func')}
            path.write_text(json.dumps(metadata))
        with redirect_stdout(io.StringIO()):
            COMPARE.compare(self.args)
        self.assertTrue(Path(str(self.args.output) + '.csv').is_file())

    def test_declared_model_must_match_representation_and_hashed_filenames(self):
        path = COMPARE.file_for(self.args.physical, '-metadata.json')
        metadata = json.loads(path.read_text())
        source = metadata['Build']['Exports'][0]['AmplitudeSource']
        source['ModelNumber'] = 5
        path.write_text(json.dumps(metadata))
        with self.assertRaisesRegex(ValueError, 'wrong representation'):
            COMPARE.compare(self.args)
        source['ModelNumber'] = 3
        path.write_text(json.dumps(metadata))
        with self.assertRaisesRegex(ValueError, 'filenames disagree'):
            COMPARE.compare(self.args)
        del source['ModelNumber']
        path.write_text(json.dumps(metadata))
        with self.assertRaisesRegex(ValueError, 'declared model provenance'):
            COMPARE.compare(self.args)

    def test_different_points_are_rejected(self):
        COMPARE.file_for(self.args.spurious, '-points-N3.csv').write_text('different points')
        with self.assertRaisesRegex(ValueError, 'phase-space points differ'):
            COMPARE.compare(self.args)

    def test_changed_export_is_rejected(self):
        (self.args.spurious_exports / 'eeAA/process.cpp').write_text('changed')
        with self.assertRaisesRegex(ValueError, 'export changed'):
            COMPARE.compare(self.args)

    def test_disagreeing_squared_amplitude_is_rejected(self):
        path = COMPARE.file_for(self.args.spurious, '-values-N4.csv')
        rows = csv_rows(path)
        rows[0]['spinas'] = '3'
        write_csv(path, rows)
        with self.assertRaisesRegex(ValueError, 'relative error'):
            COMPARE.compare(self.args)

    def test_nonfinite_tolerance_is_rejected(self):
        self.args.tolerance = float('nan')
        with self.assertRaisesRegex(ValueError, 'finite and positive'):
            COMPARE.compare(self.args)


if __name__ == '__main__':
    unittest.main()
