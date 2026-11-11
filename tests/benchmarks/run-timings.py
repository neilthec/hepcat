#!/usr/bin/env python3
"""Rebuild the two QED timing studies from saved models and explicit references.

Read a notebook export's manifest for the user's CalcHEP/MadGraph selections,
build a private double-precision SPINAS library, export unchanged models 3 and
4, and run the independent comparisons before accepting any timing results.
All generated files stay in the chosen output directory. Use --smoke for a
short installation check, or --dry-run to inspect the commands without running
them. Existing results are never deleted.
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

import argparse
from contextlib import contextmanager
import hashlib
import importlib.util
import json
import math
import os
from pathlib import Path
import shlex
import shutil
import signal
import subprocess
import sys
import time
from typing import NamedTuple, Optional
import uuid


ROOT = Path(__file__).resolve().parents[2]
BENCHMARKS = ROOT / 'tests' / 'benchmarks'
GENERATOR = 'HEPCATTimingRunner'
PAPER_SEEDS = {'photons': 1793918752098091000, 'four-fermions': 1794010385397612000}


class Stage(NamedTuple):
    label: str
    command: tuple
    directory: Path
    metadata: Optional[Path] = None


class StageFailure(RuntimeError):
    pass


class StageInterrupted(KeyboardInterrupt):
    def __init__(self, signum):
        self.signum = signum
        super().__init__('Interrupted by ' + signal.Signals(signum).name)


def interrupt(signum, frame):
    raise StageInterrupted(signum)


@contextmanager
def defer_process_signals():
    """Keep ownership acquisition atomic without changing child signal masks."""
    previous, pending = {}, []

    def remember(signum, frame):
        pending.append(signum)

    try:
        for signum in (signal.SIGINT, signal.SIGTERM):
            previous[signum] = signal.signal(signum, remember)
        yield
    finally:
        for signum, handler in previous.items():
            signal.signal(signum, handler)
    for signum in pending:
        handler = previous[signum]
        if callable(handler):
            handler(signum, None)
        elif handler != signal.SIG_IGN:
            raise StageInterrupted(signum)


def positive_integer(text):
    value = int(text)
    if value <= 0:
        raise argparse.ArgumentTypeError('must be a positive integer')
    return value


def nonnegative_integer(text):
    value = int(text)
    if value < 0:
        raise argparse.ArgumentTypeError('must be a nonnegative integer')
    return value


def positive_real(text):
    value = float(text)
    if not math.isfinite(value) or value <= 0:
        raise argparse.ArgumentTypeError('must be finite and positive')
    return value


def parser():
    result = argparse.ArgumentParser(description=__doc__)
    result.add_argument('--configuration', type=Path,
                        help='manifest.json from a QED notebook export with both references configured')
    result.add_argument('--spinas', type=Path, default=ROOT.parent / 'spinas',
                        help='SPINAS checkout; the installed build is left unchanged')
    result.add_argument('--calchep', type=Path, help='override the configured CalcHEP installation')
    result.add_argument('--kernel', help='licensed WolframKernel executable; discovered when omitted')
    result.add_argument('--cmake', help='CMake executable; discovered when omitted')
    result.add_argument('--output-directory', type=Path,
                        help='new output directory; defaults to tests/results/timings[-smoke]')
    result.add_argument('--family', choices=('photons', 'four-fermions', 'all'), default='all')
    result.add_argument('--smoke', action='store_true', help='default to 10 points, one sweep and one warmup')
    result.add_argument('--points', type=positive_integer)
    result.add_argument('--repeats', type=positive_integer)
    result.add_argument('--warmup', type=nonnegative_integer)
    result.add_argument('--seed', type=nonnegative_integer,
                        help='override the recorded paper seed for each selected family')
    result.add_argument('--energy', type=positive_real, default=100, help='center-of-mass energy in GeV')
    result.add_argument('--jobs', type=positive_integer, default=os.cpu_count() or 1,
                        help='parallel compiler jobs; measurements themselves run serially')
    result.add_argument('--stage-timeout', type=positive_real, default=3600,
                        help='maximum seconds for any export/build/measurement stage')
    result.add_argument('--no-plots', action='store_true', help='omit plots and their Matplotlib/NumPy dependency')
    result.add_argument('--dry-run', action='store_true', help='print the plan without writing files or starting programs')
    result.add_argument('--overwrite', action='store_true',
                        help='retain an earlier runner-owned output as a sibling backup before a new run')
    return result


def resolve_tool(requested, name, candidates, dry_run):
    if requested:
        found = shutil.which(requested)
        path = Path(found or requested).expanduser().resolve()
        if dry_run or path.is_file() and os.access(path, os.X_OK):
            return str(path)
    else:
        found = shutil.which(name)
        if found:
            return str(Path(found).resolve())
        for path in candidates:
            if path.is_file() and os.access(path, os.X_OK):
                return str(path.resolve())
    raise ValueError(f'Cannot find {name}; set --{name.lower().replace("wolframkernel", "kernel")} explicitly.')


def settings(arguments, root=ROOT):
    arguments.root = Path(root).resolve()
    arguments.benchmarks = arguments.root / 'tests' / 'benchmarks'
    arguments.spinas = arguments.spinas.expanduser().resolve()
    default_exports = arguments.root / 'tests' / 'results' / 'spinas-output'
    if arguments.configuration is None:
        choices = [default_exports / name / 'manifest.json' for name in
                   (('eeee', 'eeAA') if arguments.family == 'four-fermions' else ('eeAA', 'eeee'))]
        arguments.configuration = next((p for p in choices if p.is_file()), choices[0])
    arguments.configuration = arguments.configuration.expanduser().resolve()
    try:
        arguments.config = json.loads(arguments.configuration.read_text())
    except (OSError, ValueError) as error:
        raise ValueError('First configure the reference installations in the QED notebook and evaluate an '
                         'Export eeAA or Export eeee cell. Then pass its manifest.json with --configuration. '
                         f'Cannot read {arguments.configuration}: {error}') from error
    if not isinstance(arguments.config, dict):
        raise ValueError('The reference configuration must be an export manifest object.')
    for backend in ('CalcHEPValidation', 'MadGraphValidation'):
        config = arguments.config.get(backend, {})
        pdgs = config.get('AllowedParticlePDGs') if isinstance(config, dict) else None
        if not isinstance(config, dict) or config.get('Status') != 'AVAILABLE' or \
                not isinstance(pdgs, list) or any(type(x) is not int for x in pdgs) or sorted(pdgs) != [11, 22]:
            raise ValueError(f'{backend} must be AVAILABLE with the explicit electron/photon restriction {{11,22}}.')
    ch = arguments.config['CalcHEPValidation']
    mg = arguments.config['MadGraphValidation']
    for data, keys in ((ch, ('CalcHEPDirectory', 'ModelDirectory', 'ModelNumber')),
                       (mg, ('MadGraphDirectory', 'MadGraphModel', 'PythonExecutable'))):
        if any(key not in data for key in keys):
            raise ValueError('Reference installation/model information is incomplete; refresh the notebook export.')
        if any(not isinstance(data[key], str) or not data[key].strip() for key in keys if key != 'ModelNumber'):
            raise ValueError('Reference paths and model names must be nonempty strings; refresh the notebook export.')
    if type(ch['ModelNumber']) is not int or ch['ModelNumber'] < 1:
        raise ValueError('The CalcHEP model number must be a positive integer.')
    if arguments.calchep:
        ch['CalcHEPDirectory'] = str(arguments.calchep.expanduser().resolve())
    arguments.calchep = Path(ch['CalcHEPDirectory']).expanduser().resolve()
    arguments.output_directory = (arguments.output_directory or arguments.root / 'tests' / 'results' /
                                  ('timings-smoke' if arguments.smoke else 'timings')).expanduser().resolve()
    if arguments.output_directory == arguments.root or arguments.output_directory in arguments.root.parents:
        raise ValueError('Choose a dedicated output directory, not the checkout or one of its parent directories.')
    arguments.points = arguments.points if arguments.points is not None else (10 if arguments.smoke else 10000)
    arguments.repeats = arguments.repeats if arguments.repeats is not None else (1 if arguments.smoke else 9)
    arguments.warmup = arguments.warmup if arguments.warmup is not None else (1 if arguments.smoke else 100)
    if arguments.seed is not None and arguments.seed >= 2**64:
        raise ValueError('--seed must fit an unsigned 64-bit integer.')
    candidates = list(Path('/Applications').glob('*.app/Contents/MacOS/WolframKernel'))
    candidates += list(Path('/usr/local/Wolfram').glob('*/*/Executables/WolframKernel'))
    arguments.kernel = resolve_tool(arguments.kernel or os.environ.get('WOLFRAM_KERNEL'),
                                    'WolframKernel', candidates, arguments.dry_run)
    arguments.cmake = resolve_tool(arguments.cmake, 'cmake',
                                  [Path('/Applications/CMake.app/Contents/bin/cmake')], arguments.dry_run)
    if not arguments.dry_run:
        if os.name != 'posix':
            raise ValueError('The independent CalcHEP benchmark requires a POSIX build environment.')
        if not (arguments.spinas / 'CMakeLists.txt').is_file() or not (arguments.spinas / 'include/spinas.h').is_file():
            raise ValueError('Set --spinas to a SPINAS source checkout containing CMakeLists.txt and include/spinas.h.')
        if not (arguments.calchep / 'lib/dynamic_me.a').is_file():
            raise ValueError('CalcHEP is not built at the configured location; build it or set --calchep.')
        if not Path(ch['ModelDirectory']).expanduser().is_dir():
            raise ValueError('The selected CalcHEP model directory is missing; refresh the notebook export configuration.')
        if not Path(mg['MadGraphDirectory']).expanduser().is_dir():
            raise ValueError('The selected MadGraph installation is missing; refresh the notebook export configuration.')
        if not arguments.no_plots and any(importlib.util.find_spec(name) is None for name in ('matplotlib', 'numpy')):
            raise ValueError('Plotting needs Matplotlib and NumPy in this Python environment. Install them or use --no-plots.')
    return arguments


def plan(options):
    root, source, out = options.root, options.benchmarks, options.output_directory
    cmake, python, kernel = options.cmake, sys.executable, options.kernel
    config = out / 'reference-configuration.json'
    library_build = out / 'spinas-build'
    library = library_build / 'libspinas.a'
    stages = []

    def add(label, command, directory=root, metadata=None):
        stages.append(Stage(label, tuple(str(x) for x in command), directory, metadata))

    add('Configure private Double SPINAS', [cmake, '-S', options.spinas, '-B', library_build,
        '-DCMAKE_BUILD_TYPE=Release', '-DPRECISION_TYPE=DOUBLE', '-DBUILD_TESTING=OFF'])
    add('Build private Double SPINAS', [cmake, '--build', library_build, '--target', 'spinas_static',
                                       '--parallel', options.jobs])

    def configure(label, directory, build, exports):
        add(label, [cmake, '-S', directory, '-B', build, '-DCMAKE_BUILD_TYPE=Release',
            '-DSPINAS_ROOT=' + str(options.spinas), '-DSPINAS_LIBRARY=' + str(library),
            '-DCALCHEP_ROOT=' + str(options.calchep), '-DPython3_EXECUTABLE=' + python, *exports])
        add('Build ' + label.lower(), [cmake, '--build', build, '--parallel', options.jobs])

    def measurement(label, executable, directory, family):
        seed = options.seed if options.seed is not None else PAPER_SEEDS[family]
        add(label, [executable, '--points', options.points, '--repeats', options.repeats,
            '--warmup', options.warmup, '--energy', options.energy, '--seed', seed,
            '--output', directory / 'timings'], directory,
            directory / 'timings-metadata.json')

    if options.family in ('photons', 'all'):
        base = out / 'photons'
        for name, representation in (('physical', 'AllPhysical'), ('spurious', 'AllSpurious')):
            directory = base / name
            exports, build = directory / 'exports', directory / 'build'
            add('Export saved ' + representation + ' photon amplitudes', [kernel, '-script',
                source / 'export-physical.wl', '--configuration', config, '--spinas', options.spinas,
                '--output', exports, '--representation', representation])
            configure(name + ' photons', source, build,
                      ['-DHEPCAT_PROCESS_DIRECTORIES=' + ';'.join(str(exports / ('ee' + 'A' * n)) for n in (2, 3, 4))])
            add('Check photon benchmark kinematics', [cmake, '--build', build, '--target', 'test'])
            measurement('Measure ' + representation + ' photons', build / 'qed_photon_benchmark', directory, 'photons')
        add('Verify and combine paired photon timings', [python, source / 'compare-photon-timings.py',
            '--physical', base / 'physical/timings', '--spurious', base / 'spurious/timings',
            '--output', base / 'fourway', '--points', options.points])
        if not options.no_plots:
            add('Plot photon timings', [python, source / 'plot-photon-timings.py', '--input-directory', base,
                                        '--output-directory', out / 'figures'])
    if options.family in ('four-fermions', 'all'):
        base = out / 'four-fermions'
        exports, build = base / 'exports', base / 'build'
        add('Export saved four-fermion amplitudes', [kernel, '-script', source / 'export-four-fermions.wl',
            '--configuration', config, '--spinas', options.spinas, '--output', exports])
        configure('four-fermion benchmark', source / 'four-fermion', build, [
            '-DHEPCAT_PHYSICAL_PROCESS_DIRECTORIES=' + ';'.join(str(exports / 'physical' / ('ffffPhysical' + str(n))) for n in (0, 1, 2)),
            '-DHEPCAT_SPURIOUS_PROCESS_DIRECTORIES=' + ';'.join(str(exports / 'spurious' / ('ffffSpurious' + str(n))) for n in (0, 1, 2))])
        measurement('Measure all four-fermion implementations', build / 'qed_four_fermion_benchmark', base, 'four-fermions')
        if not options.no_plots:
            add('Plot four-fermion timings', [python, source / 'plot-four-fermion-timings.py',
                '--input', base / 'timings.csv', '--output-directory', out / 'figures'])
    return stages


def prepare_output(directory, overwrite):
    backup = None
    if directory.exists() and any(directory.iterdir()):
        try:
            owned = json.loads((directory / 'run-summary.json').read_text()).get('Generator') == GENERATOR
        except (OSError, ValueError, AttributeError):
            owned = False
        if not overwrite:
            raise ValueError(f'{directory} already contains results. Choose a new --output-directory or use --overwrite to retain the old run.')
        if not owned:
            raise ValueError('Refusing to move an output directory not identified as an earlier HEPCAT timing run.')
        backup = directory.with_name(directory.name + '.previous-' + time.strftime('%Y%m%d-%H%M%S') + '-' + uuid.uuid4().hex[:8])
        directory.rename(backup)
    directory.mkdir(parents=True, exist_ok=True)
    return backup


def stop_group(process):
    try:
        if os.name == 'posix':
            os.killpg(process.pid, signal.SIGKILL)
        else:
            process.kill()
    except ProcessLookupError:
        pass
    process.wait()


def run_stage(stage, logfile, timeout):
    started = time.monotonic()
    print('\n' + stage.label, flush=True)
    print(shlex.join(stage.command), flush=True)
    with logfile.open('w') as log:
        log.write(shlex.join(stage.command) + '\n')
        log.flush()
        process = None
        try:
            # Ctrl-C can arrive after a child starts but before Popen returns.
            # Deliver it only once this scope owns the process and its group.
            with defer_process_signals():
                process = subprocess.Popen(stage.command, cwd=str(stage.directory), stdout=log,
                                           stderr=subprocess.STDOUT, start_new_session=(os.name == 'posix'))
            code = process.wait(timeout=timeout)
        except BaseException:
            if process is not None:
                stop_group(process)
            raise
    elapsed = time.monotonic() - started
    if code:
        raise StageFailure(f'{stage.label} exited with status {code}. See {logfile}')
    if stage.metadata:
        try:
            report = json.loads(stage.metadata.read_text())
        except (OSError, ValueError) as error:
            raise StageFailure(f'{stage.label} did not write a complete validation report: {error}') from error
        if report.get('Status') != 'COMPLETE' or report.get('ValidatedRows') != 'PASS':
            raise StageFailure(f'{stage.label} was incomplete or failed validation. See {stage.metadata}')
    print(f'Completed in {elapsed:.1f} s; log: {logfile}', flush=True)
    return {'Stage': stage.label, 'Status': 'PASS', 'ElapsedSeconds': elapsed, 'Log': str(logfile)}


def execute(options, stages):
    out = options.output_directory
    backup = prepare_output(out, options.overwrite)
    if backup:
        print('Previous run retained at:', backup, flush=True)
    (out / 'logs').mkdir()
    (out / 'reference-configuration.json').write_text(json.dumps(options.config, indent=2) + '\n')
    summary = {'Generator': GENERATOR, 'Status': 'RUNNING', 'Family': options.family,
        'PointsPerMultiplicity': options.points, 'Repetitions': options.repeats,
        'Warmup': options.warmup, 'EnergyGeV': options.energy,
        'Seeds': {name: options.seed if options.seed is not None else seed for name, seed in PAPER_SEEDS.items()},
        'SourceConfiguration': str(options.configuration),
        'SourceConfigurationSHA256': hashlib.sha256(options.configuration.read_bytes()).hexdigest(),
        'RunnerSHA256': hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
        'AmplitudeSource': 'Unchanged complete amplitudes saved in constructive models3and4; no construction or completion is run.',
        'Scope': 'Fresh measurements; wall times depend on this machine, toolchain and load.', 'Stages': []}
    summary_path = out / 'run-summary.json'
    started = time.monotonic()
    try:
        for index, stage in enumerate(stages, 1):
            summary['ActiveStage'] = stage.label
            summary_path.write_text(json.dumps(summary, indent=2) + '\n')
            logfile = out / 'logs' / f'{index:02d}.log'
            summary['Stages'].append(run_stage(stage, logfile, options.stage_timeout))
        summary['Status'] = 'COMPLETE'
        summary.pop('ActiveStage', None)
    except BaseException as error:
        summary['Status'] = 'INTERRUPTED' if isinstance(error, KeyboardInterrupt) else 'FAILED'
        summary['FailureReason'] = str(error) or type(error).__name__
        raise
    finally:
        summary['ElapsedSeconds'] = time.monotonic() - started
        summary_path.write_text(json.dumps(summary, indent=2) + '\n')
    print('\nComplete. Results:', out, flush=True)
    return summary


def main(argv=None):
    arguments = parser().parse_args(argv)
    try:
        options = settings(arguments)
        stages = plan(options)
        if options.dry_run:
            print('Plan only. No files are written and external installations are not verified.')
            print('Source: saved complete amplitudes in models3and4; no constructor is run.')
            for index, stage in enumerate(stages, 1):
                print(f'{index:02d}. {stage.label}\n    {shlex.join(stage.command)}')
            return 0
        previous = signal.signal(signal.SIGTERM, interrupt)
        try:
            execute(options, stages)
        finally:
            signal.signal(signal.SIGTERM, previous)
        return 0
    except StageInterrupted as error:
        print(str(error) + '; the active owned process group was terminated.', file=sys.stderr)
        return 128 + error.signum
    except KeyboardInterrupt:
        print('Interrupted; the active owned process group was terminated.', file=sys.stderr)
        return 130
    except subprocess.TimeoutExpired as error:
        print(f'Stage timed out after {error.timeout}s. The owned process group was terminated.', file=sys.stderr)
        return 124
    except (ValueError, OSError, StageFailure) as error:
        print('Timing run stopped: ' + str(error), file=sys.stderr)
        return 1


if __name__ == '__main__':
    sys.exit(main())
