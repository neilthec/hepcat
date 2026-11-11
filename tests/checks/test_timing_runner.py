"""Check portable timing plans, explicit prerequisites and failed-stage cleanup."""

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

from contextlib import redirect_stderr, redirect_stdout
import importlib.util
import io
import json
import os
from pathlib import Path
import shutil
import signal
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch


PATH = Path(__file__).resolve().parents[2] / 'tests/benchmarks/run-timings.py'
SPEC = importlib.util.spec_from_file_location('hepcat_timing_runner', PATH)
RUNNER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(RUNNER)


class TimingRunnerTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix='hepcat timing runner ')
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.configuration = self.root / 'configured export' / 'manifest.json'
        self.configuration.parent.mkdir()
        self.data = {
            'CalcHEPValidation': {'Status': 'AVAILABLE', 'AllowedParticlePDGs': [11, 22],
                'CalcHEPDirectory': str(self.root / 'CalcHEP'),
                'ModelDirectory': str(self.root / 'models'), 'ModelNumber': 2},
            'MadGraphValidation': {'Status': 'AVAILABLE', 'AllowedParticlePDGs': [11, 22],
                'MadGraphDirectory': str(self.root / 'MadGraph'),
                'MadGraphModel': 'sm-full', 'PythonExecutable': sys.executable}}
        self.configuration.write_text(json.dumps(self.data))

    def options(self, *extra):
        args = RUNNER.parser().parse_args(['--configuration', str(self.configuration),
            '--spinas', str(self.root / 'SPINAS'), '--kernel', str(self.root / 'WolframKernel'),
            '--cmake', str(self.root / 'cmake'), '--dry-run', *extra])
        return RUNNER.settings(args, self.root)

    def test_paper_defaults_and_smoke_defaults_are_explicit(self):
        options = self.options()
        self.assertEqual((options.points, options.repeats, options.warmup), (10000, 9, 100))
        short = self.options('--smoke')
        self.assertEqual((short.points, short.repeats, short.warmup), (10, 1, 1))
        self.assertNotEqual(short.output_directory, options.output_directory)
        explicit = self.options('--smoke', '--points', '23', '--warmup', '0')
        self.assertEqual((explicit.points, explicit.warmup), (23, 0))

    def test_one_private_double_library_and_serial_measurement_plan(self):
        stages = RUNNER.plan(self.options('--smoke'))
        self.assertEqual(sum('-DPRECISION_TYPE=DOUBLE' in s.command for s in stages), 1)
        measurements = [s for s in stages if s.metadata]
        self.assertEqual(len(measurements), 3)
        photon_seeds = [s.command[s.command.index('--seed') + 1] for s in measurements[:2]]
        self.assertEqual(photon_seeds, [str(RUNNER.PAPER_SEEDS['photons'])] * 2)
        self.assertEqual(measurements[2].command[measurements[2].command.index('--seed') + 1],
                         str(RUNNER.PAPER_SEEDS['four-fermions']))
        labels = [s.label for s in stages]
        self.assertLess(labels.index('Measure AllSpurious photons'), labels.index('Verify and combine paired photon timings'))
        self.assertLess(labels.index('Measure all four-fermion implementations'), labels.index('Plot four-fermion timings'))

    def test_seed_override_reaches_both_forms_and_families(self):
        stages = RUNNER.plan(self.options('--seed', '0'))
        self.assertEqual([s.command[s.command.index('--seed') + 1] for s in stages if s.metadata], ['0'] * 3)
        with self.assertRaisesRegex(ValueError, '64-bit'):
            self.options('--seed', str(2**64))

    def test_paths_with_spaces_remain_single_arguments(self):
        options = self.options('--family', 'photons', '--no-plots')
        stages = RUNNER.plan(options)
        export = next(s for s in stages if s.label.startswith('Export'))
        self.assertEqual(export.command[export.command.index('--configuration') + 1],
                         str(options.output_directory / 'reference-configuration.json'))
        self.assertFalse(any('Plot ' in s.label for s in stages))
        configure = next(s for s in stages if s.label == 'physical photons')
        selection = next(arg for arg in configure.command if arg.startswith('-DHEPCAT_PROCESS_DIRECTORIES='))
        self.assertIn('hepcat timing runner ', selection)
        self.assertEqual(selection.count(';'), 2)

    def test_missing_or_incomplete_configuration_is_actionable(self):
        self.configuration.unlink()
        with self.assertRaisesRegex(ValueError, 'Export eeAA or Export eeee'):
            self.options()
        for bad in (None, ['11', 22], [11], [11, 22, 23]):
            with self.subTest(restriction=bad):
                self.data['MadGraphValidation']['AllowedParticlePDGs'] = bad
                self.configuration.write_text(json.dumps(self.data))
                with self.assertRaisesRegex(ValueError, 'electron/photon restriction'):
                    self.options()

    def test_source_archive_dry_run_needs_no_git_or_original_machine(self):
        copied = self.root / 'copy without git' / 'tests' / 'benchmarks' / PATH.name
        copied.parent.mkdir(parents=True)
        shutil.copyfile(PATH, copied)
        result = subprocess.run([sys.executable, str(copied), '--configuration', str(self.configuration),
            '--kernel', str(self.root / 'chosen kernel'), '--cmake', str(self.root / 'chosen cmake'),
            '--spinas', str(self.root / 'chosen spinas'), '--dry-run', '--smoke'],
            cwd=self.root, capture_output=True, text=True, timeout=10)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn('Plan only', result.stdout)
        self.assertIn('compare-photon-timings.py', result.stdout)
        self.assertIn('export-four-fermions.wl', result.stdout)
        self.assertFalse((copied.parents[2] / 'tests/results/timings-smoke').exists())

    def test_old_results_are_retained_and_unowned_directories_are_protected(self):
        output = self.root / 'old results'
        output.mkdir()
        (output / 'precious.csv').write_text('keep me')
        with self.assertRaisesRegex(ValueError, 'already contains'):
            RUNNER.prepare_output(output, False)
        with self.assertRaisesRegex(ValueError, 'not identified'):
            RUNNER.prepare_output(output, True)
        (output / 'run-summary.json').write_text(json.dumps({'Generator': RUNNER.GENERATOR}))
        backup = RUNNER.prepare_output(output, True)
        self.assertEqual((backup / 'precious.csv').read_text(), 'keep me')
        self.assertEqual(list(output.iterdir()), [])

    def test_nonzero_stage_stops_pipeline_and_records_failure(self):
        options = self.options('--no-plots')
        stages = [RUNNER.Stage('Bad stage', (sys.executable, '-c', 'raise SystemExit(3)'), self.root),
                  RUNNER.Stage('Must not run', (sys.executable, '-c',
                    "from pathlib import Path; Path('unwanted').write_text('bad')"), self.root)]
        with redirect_stdout(io.StringIO()), self.assertRaisesRegex(RUNNER.StageFailure, 'status 3'):
            RUNNER.execute(options, stages)
        report = json.loads((options.output_directory / 'run-summary.json').read_text())
        self.assertEqual(report['Status'], 'FAILED')
        self.assertEqual(report['ActiveStage'], 'Bad stage')
        self.assertFalse((self.root / 'unwanted').exists())

    def test_zero_exit_without_completed_validation_is_rejected(self):
        metadata = self.root / 'incomplete.json'
        metadata.write_text(json.dumps({'Status': 'PARTIAL', 'ValidatedRows': 'PASS'}))
        stage = RUNNER.Stage('Incomplete benchmark', (sys.executable, '-c', 'pass'), self.root, metadata)
        with redirect_stdout(io.StringIO()), self.assertRaisesRegex(RUNNER.StageFailure, 'incomplete'):
            RUNNER.run_stage(stage, self.root / 'test.log', 5)

    @unittest.skipUnless(os.name == 'posix', 'Owned process groups require POSIX')
    def test_interrupt_during_spawn_keeps_ownership_and_restores_handlers(self):
        launch = subprocess.Popen
        old_handlers = {sig: signal.getsignal(sig) for sig in (signal.SIGINT, signal.SIGTERM)}
        stage = RUNNER.Stage('Interrupted spawn', (sys.executable, '-c', 'import time; time.sleep(60)'), self.root)
        for signum in (signal.SIGINT, signal.SIGTERM):
            with self.subTest(signal=signum):
                launched = []

                def record_and_interrupt(*args, **kwargs):
                    process = launch(*args, **kwargs)
                    launched.append(process)
                    os.kill(os.getpid(), signum)
                    return process

                try:
                    with patch.object(RUNNER.subprocess, 'Popen', side_effect=record_and_interrupt), \
                            redirect_stdout(io.StringIO()), self.assertRaises(KeyboardInterrupt):
                        RUNNER.run_stage(stage, self.root / 'interrupted.log', 5)
                    self.assertEqual(len(launched), 1)
                    self.assertIsNotNone(launched[0].returncode)
                    with self.assertRaises(ChildProcessError):
                        os.waitpid(launched[0].pid, os.WNOHANG)
                    self.assertEqual({sig: signal.getsignal(sig) for sig in old_handlers}, old_handlers)
                finally:
                    for process in launched:
                        if process.poll() is None:
                            RUNNER.stop_group(process)

    def test_failed_spawn_restores_signal_handlers(self):
        before = {sig: signal.getsignal(sig) for sig in (signal.SIGINT, signal.SIGTERM)}
        stage = RUNNER.Stage('Missing executable', ('does-not-exist',), self.root)
        with patch.object(RUNNER.subprocess, 'Popen', side_effect=FileNotFoundError), \
                redirect_stdout(io.StringIO()), self.assertRaises(FileNotFoundError):
            RUNNER.run_stage(stage, self.root / 'missing.log', 5)
        self.assertEqual({sig: signal.getsignal(sig) for sig in before}, before)

    def test_interrupted_run_is_recorded_without_success(self):
        options = self.options('--no-plots')
        stage = RUNNER.Stage('Interrupted stage', ('unused',), self.root)
        with patch.object(RUNNER, 'run_stage', side_effect=RUNNER.StageInterrupted(signal.SIGTERM)), \
                self.assertRaises(RUNNER.StageInterrupted):
            RUNNER.execute(options, [stage])
        report = json.loads((options.output_directory / 'run-summary.json').read_text())
        self.assertEqual(report['Status'], 'INTERRUPTED')
        self.assertEqual(report['Stages'], [])

    def test_main_sigterm_exit_is_nonzero_and_restores_handler(self):
        options = self.options('--no-plots')
        options.dry_run = False
        before = signal.getsignal(signal.SIGTERM)
        with patch.object(RUNNER, 'settings', return_value=options), \
                patch.object(RUNNER, 'execute', side_effect=RUNNER.StageInterrupted(signal.SIGTERM)), \
                redirect_stderr(io.StringIO()):
            self.assertEqual(RUNNER.main([]), 128 + signal.SIGTERM)
        self.assertEqual(signal.getsignal(signal.SIGTERM), before)

    @unittest.skipUnless(os.name == 'posix', 'Owned process groups require POSIX')
    def test_timeout_kills_owned_group_and_reaps_child(self):
        launched = []
        launch = subprocess.Popen

        def record(*args, **kwargs):
            process = launch(*args, **kwargs)
            launched.append(process)
            return process

        stage = RUNNER.Stage('Timeout', (sys.executable, '-c', 'import time; time.sleep(60)'), self.root)
        with patch.object(RUNNER.subprocess, 'Popen', side_effect=record), \
                patch.object(RUNNER.os, 'killpg', wraps=os.killpg) as killpg, \
                redirect_stdout(io.StringIO()), self.assertRaises(subprocess.TimeoutExpired):
            RUNNER.run_stage(stage, self.root / 'timeout.log', 0.1)
        self.assertEqual(len(launched), 1)
        killpg.assert_called_once_with(launched[0].pid, signal.SIGKILL)
        self.assertIsNotNone(launched[0].returncode)
        with self.assertRaises(ChildProcessError):
            os.waitpid(launched[0].pid, os.WNOHANG)


if __name__ == '__main__':
    unittest.main()
