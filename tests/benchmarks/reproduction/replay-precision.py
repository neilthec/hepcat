#!/usr/bin/env python3
"""Re-evaluate the frozen precision fixtures using isolated SPINAS copies.

The references are the original saved MadGraph binary64 rates. This command
recomputes SPINAS amplitudes; it does not claim to run a new MadGraph process.
Every installed source tree and the published data bundle remain unchanged.
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
import csv
import hashlib
import io
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import time
import zipfile

HERE = Path(__file__).resolve().parent
REPOSITORY = HERE.parents[2]
MODES = {'float': ('float', 24), 'double': ('double', 53), 'extended': ('long double', 64)}
GROUPS = ('eeAA', 'eeAAA', 'eeAAAA', 'four-fermion', 'six-fermion', 'high-n', 'rounding')


def require(condition, message):
    if not condition:
        raise ValueError(message)


def digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def safe_extract(archive, destination):
    destination = destination.resolve()
    for item in archive.infolist():
        path = (destination / item.filename).resolve()
        require(path.is_relative_to(destination) and not item.filename.startswith('/'),
                'Unsafe path in the evidence ZIP: ' + item.filename)
        require((item.external_attr >> 16) & 0o170000 != 0o120000,
                'Symlinks are not supported inside the evidence ZIP')
    archive.extractall(destination)


def run(command, log):
    started = time.monotonic()
    result = subprocess.run([str(x) for x in command], text=True,
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    with log.open('a') as stream:
        stream.write(json.dumps([str(x) for x in command]) + '\n' + result.stdout + '\n')
    require(result.returncode == 0, 'Command failed; inspect ' + str(log))
    return time.monotonic() - started


def prepare_inputs(output, evidence):
    inputs = output / 'frozen-inputs'
    inputs.mkdir(parents=True, exist_ok=True)
    with zipfile.ZipFile(evidence) as outer:
        with zipfile.ZipFile(io.BytesIO(outer.read('precision-replay.zip'))) as archive:
            safe_extract(archive, inputs)
        records = json.loads(outer.read('replay-source-records.json'))
    for name, canonical in records.get('aliases', {}).items():
        target = inputs / name
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(inputs / canonical, target)
    for row in records['files']:
        require(digest(inputs / row['path']) == row['sha256'],
                'Frozen replay input mismatch: ' + row['path'])
    return inputs


def build_library(spinas, destination, mode, cxx, flags):
    scalar, expected_bits = MODES[mode]
    destination.mkdir(parents=True, exist_ok=True)
    for name in ('include', 'source'):
        shutil.copytree(spinas / name, destination / name, dirs_exist_ok=True)
    types = destination / 'include/types.h'
    content = types.read_text()
    selection = '#if defined WITH_LONG_DOUBLE\ntypedef long double ldouble;\n#else\ntypedef double ldouble;\n#endif'
    require(content.count(selection) == 1,
            'Unsupported SPINAS scalar declaration. Use the recorded compatible source revision c8c69ff.')
    types.write_text(content.replace(selection, f'typedef {scalar} ldouble;'))
    particle = destination / 'source/particle.cpp'
    content = particle.read_text()
    original = '''      const ldouble epsilon = std::numeric_limits<ldouble>::epsilon();
      const ldouble tolerance = (sum==0 ? ldouble(0) : epsilon*1000000000/sum)
        + 8*epsilon*std::max(absolute,expected);'''
    adapted = '''      const ldouble inputEpsilon = std::numeric_limits<double>::epsilon();
      const ldouble epsilon = std::max(inputEpsilon, std::numeric_limits<ldouble>::epsilon());
      const ldouble tolerance = (sum==0 ? ldouble(0) : inputEpsilon*1000000000/sum)
        + 8*epsilon*std::max(absolute,expected);'''
    require(content.count(original) == 1,
            'Unsupported SPINAS external-shell guard. Use the recorded compatible source revision c8c69ff.')
    particle.write_text(content.replace(original, adapted))
    log = destination / 'build.log'
    probe = destination / 'probe.cpp'
    probe.write_text('#include "types.h"\n#include <limits>\n#include <iostream>\n'
                     'int main(){std::cout<<std::numeric_limits<ldouble>::digits<<"\\n";}\n')
    run([cxx, *flags, '-I', destination / 'include', probe, '-o', destination / 'probe'], log)
    result = subprocess.run([str(destination / 'probe')], capture_output=True, text=True, check=True)
    measured = int(result.stdout.strip())
    require(measured == expected_bits,
            f'{mode} provides {measured} bits; paper study used {expected_bits}. '
            'On Apple Silicon use --target x86_64 with an installed compatible compiler and Rosetta.')
    objects = []
    for source in sorted((destination / 'source').glob('*.cpp')):
        obj = destination / (source.stem + '.o')
        run([cxx, *flags, '-I', destination / 'include', '-c', source, '-o', obj], log)
        objects.append(obj)
    library = destination / 'libspinas.a'
    if library.exists():
        library.unlink()
    run(['ar', 'rcs', library, *objects], log)
    return {'scalar': scalar, 'mantissa_bits': measured, 'library': str(library),
            'sha256': digest(library),
            'input_policy': 'Private study guard recognizes binary64 input rounding; momenta and amplitudes unchanged.'}


def read_csv(path):
    with path.open(newline='') as stream:
        return list(csv.DictReader(stream))


def reference_helicities(evidence, member, keys):
    with zipfile.ZipFile(evidence) as outer:
        with zipfile.ZipFile(io.BytesIO(outer.read('precision-results.zip'))) as archive:
            with archive.open(member) as stream:
                return {tuple(r[k] for k in keys): r['madgraph']
                        for r in csv.DictReader(io.TextIOWrapper(stream))}


def join_helicities(evidence, output, family, mode, four=False):
    prefix = 'fermions' if four else 'one-pair'
    if four:
        path = output / prefix / 'results' / mode / (family + '-helicities.csv')
        member = f'fermions/results/double/{family}-helicities-compared.csv'
        fields = ('point', 'photon_helicity_mask')
        target = path.with_name(family + '-helicities-compared.csv')
    else:
        path = output / prefix / 'results' / (family + '-' + mode + '-helicities.csv')
        member = f'one-pair/results/{family}-double-helicity-comparison.csv'
        fields = ('point', 'helicity')
        target = path.with_name(family + '-' + mode + '-helicity-comparison.csv')
    references = reference_helicities(evidence, member, fields)
    rows = read_csv(path)
    seen = set()
    with target.open('w', newline='') as stream:
        writer = csv.DictWriter(stream, fieldnames=[*fields, 'physical', 'spurious', 'madgraph'])
        writer.writeheader()
        for row in rows:
            mask = int(row['photon_helicity_mask'])
            helicity = ''.join('+' if mask & (1 << i) else '-' for i in range(len(family) - 2))
            key = (row['point'], row['photon_helicity_mask'] if four else helicity)
            require(key not in seen and key in references, 'Incorrect helicity reference inventory')
            seen.add(key)
            writer.writerow({fields[0]: key[0], fields[1]: key[1],
                             'physical': row['physical' if four else 'physical_massive_spin_sum'],
                             'spurious': row['spurious' if four else 'spurious_massive_spin_sum'],
                             'madgraph': references[key]})
    require(seen == references.keys(), 'Incomplete fixed-helicity replay')


def replay_group(group, mode, inputs, output, library, cxx, flags, evidence):
    build = output / 'build' / mode
    build.mkdir(parents=True, exist_ok=True)
    include = ['-I', Path(library['library']).parent / 'include']
    log = build / (group + '.log')
    executable = build / group
    if group in ('eeAA', 'eeAAA', 'eeAAAA'):
        source = inputs / 'one-pair/sources' / group
        run([cxx, *flags, '-DPROCESS_CLASS=' + group, '-I', source, *include,
             inputs / 'one-pair/replay.cpp', source / 'physical/process.cpp',
             source / 'spurious/process.cpp', library['library'], '-o', executable], log)
        directory = output / 'one-pair/results'; directory.mkdir(parents=True, exist_ok=True)
        n = len(group) - 2
        run([executable, inputs / f'one-pair/inputs/timings-points-N{n}.csv',
             inputs / f'one-pair/inputs/timings-values-N{n}.csv', directory / (group + '-' + mode)], log)
        join_helicities(evidence, output, group, mode)
        runtime = json.loads((directory / (group + '-' + mode + '-runtime.json')).read_text())
        require(runtime['failed_evaluations'] == 0, 'Amplitude evaluation failure: ' + str(log))
    elif group in ('four-fermion', 'six-fermion'):
        directory = output / 'fermions/results' / mode; directory.mkdir(parents=True, exist_ok=True)
        if group == 'four-fermion':
            names = [f'ffff{p}{n}' for n in range(3) for p in ('Physical', 'Spurious')]
            driver = 'replay-four.cpp'
            input_directory = inputs / 'fermions/inputs'
        else:
            names = ['sixPhysical', 'sixSpurious']; driver = 'replay-six.cpp'
            input_directory = inputs / 'fermions'
        sources = [inputs / 'fermions/exports' / name / 'process.cpp' for name in names]
        run([cxx, *flags, *include, inputs / 'fermions' / driver,
             *sources, library['library'], '-o', executable], log)
        run([executable, input_directory, directory], log)
        if group == 'four-fermion':
            hel = build / 'four-helicities'
            run([cxx, *flags, *include, inputs / 'fermions/replay-four-helicities.cpp',
                 *sources[2:], library['library'], '-o', hel], log)
            run([hel, input_directory, directory], log)
            for family in ('eeeeA', 'eeeeAA'):
                join_helicities(evidence, output, family, mode, four=True)
        for family in ('eeee', 'eeeeA', 'eeeeAA') if group == 'four-fermion' else ('eeeeee',):
            rows = read_csv(directory / (family + '.csv'))
            require(all(not r['physical_error'] and not r['spurious_error'] for r in rows),
                    'Amplitude evaluation failures: ' + str(log))
    elif group == 'rounding':
        run([cxx, *flags, *include, inputs / 'rounding/compare-n8.cpp', library['library'],
             '-o', executable], log)
        directory = output / 'rounding'; directory.mkdir(parents=True, exist_ok=True)
        for projected in (False, True):
            prefix = directory / (mode + ('-projected' if projected else '-original'))
            command = [executable, '--input', inputs / 'rounding/original-points.csv', '--output', prefix]
            if projected: command.append('--project-shells')
            run(command, log)
    else:
        run([cxx, *flags, *include, inputs / 'high-n/replay.cpp', library['library'],
             '-o', executable], log)
        directory = output / 'high-n'; directory.mkdir(parents=True, exist_ok=True)
        for n in range(5, 9):
            prefix = 'high-n/inputs-N5' if n == 5 else 'high-n/inputs'
            run([executable, n, inputs / prefix / f'N{n}-points.csv',
                 inputs / prefix / f'N{n}-values.csv', directory / f'N{n}-{mode}',
                 '1.5' if n == 8 else '1'], log)
            metadata = json.loads((directory / f'N{n}-{mode}.json').read_text())
            require(metadata['EvaluationErrors'] == metadata['NonfinitePoints'] == 0,
                    'High-multiplicity evaluation failure: ' + str(log))
    if group in ('eeAA', 'eeAAA', 'eeAAAA'):
        rate_files = [(directory / (group + '-' + mode + '.csv'), 10000)]
    elif group == 'four-fermion':
        rate_files = [(directory / (family + '.csv'), 10000)
                      for family in ('eeee', 'eeeeA', 'eeeeAA')]
    elif group == 'six-fermion':
        rate_files = [(directory / 'eeeeee.csv', 1000)]
    elif group == 'rounding':
        rate_files = [(directory / (mode + '-' + suffix + '-values.csv'), 1000)
                      for suffix in ('original', 'projected')]
    else:
        rate_files = [(directory / f'N{n}-{mode}.csv', 1000) for n in range(5, 9)]
    counts = {str(path.relative_to(output)): len(read_csv(path)) for path, _ in rate_files}
    require(all(counts[str(path.relative_to(output))] == expected for path, expected in rate_files),
            'Incomplete numerical replay: ' + str(log))
    return {'group': group, 'mode': mode, 'status': 'COMPLETE', 'rate_rows': counts,
            'executable_sha256': digest(executable), 'log': str(log)}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--spinas', type=Path, required=True, help='Compatible SPINAS source checkout; never edited')
    parser.add_argument('--evidence-archive', type=Path, default=HERE / 'paper-evidence.zip')
    parser.add_argument('--output-directory', type=Path,
                        default=REPOSITORY / 'tests/results/reproduction/numerical-precision')
    parser.add_argument('--modes', nargs='+', choices=tuple(MODES), default=list(MODES))
    parser.add_argument('--families', nargs='+', choices=GROUPS, default=list(GROUPS))
    parser.add_argument('--cxx', default=os.environ.get('CXX', 'c++'))
    parser.add_argument('--target', choices=('native', 'x86_64'), default='native')
    args = parser.parse_args()
    require(args.spinas.joinpath('source').is_dir() and args.spinas.joinpath('include').is_dir(),
            '--spinas must point to a source checkout with include/ and source/')
    require(args.target == 'native' or sys.platform == 'darwin',
            '--target x86_64 is the Apple compiler cross-target option; use native on x86 Linux')
    flags = ['-std=c++17', '-O2', '-ffp-contract=off']
    if args.target == 'x86_64': flags = ['-arch', 'x86_64', *flags]
    args.output_directory.mkdir(parents=True, exist_ok=True)
    inputs = prepare_inputs(args.output_directory, args.evidence_archive)
    report = {'Status': 'RUNNING', 'Scope': __doc__.strip(), 'Compiler': args.cxx, 'Flags': flags,
              'SPINASSource': str(args.spinas.resolve()), 'Modes': {}, 'Runs': [],
              'Reference': 'Original archived MadGraph binary64 rates; no new MG calculation.'}
    for mode in args.modes:
        print('Building isolated SPINAS mode:', mode, flush=True)
        library = build_library(args.spinas, args.output_directory / 'spinas' / mode, mode, args.cxx, flags)
        report['Modes'][mode] = library
        for group in args.families:
            if group == 'rounding' and mode == 'float':
                continue  # The input-projection experiment compared only 53/64-bit arithmetic.
            print('Re-evaluating:', group, mode, flush=True)
            report['Runs'].append(replay_group(group, mode, inputs, args.output_directory,
                                               library, args.cxx, flags, args.evidence_archive))
            (args.output_directory / 'numerical-replay.json').write_text(json.dumps(report, indent=2) + '\n')
    report['Status'] = 'COMPLETE'
    report['CompletionMeaning'] = ('All requested evaluations completed with the recorded row counts. '
                                   'This is not a precision-tolerance PASS; analyze the resulting rates.')
    (args.output_directory / 'numerical-replay.json').write_text(json.dumps(report, indent=2) + '\n')
    print(args.output_directory / 'numerical-replay.json')


if __name__ == '__main__':
    main()
