"""Compare regenerated QED operand tables with MadGraph and archived complex amplitudes.

Run from the HEPCAT root. The baseline archive is the output of the original
source inventory. Only one process family's extracted or generated files are
needed on disk at a time. Each completed comparison is checkpointed as JSON.
"""
import argparse
import gzip
import json
import math
import re
from pathlib import Path
import shutil
import subprocess
import sys
import tarfile
import tempfile

HERE = Path(__file__).resolve().parent


def run(*args):
    return subprocess.run([str(arg) for arg in args], check=True, text=True,
                          stdout=subprocess.PIPE).stdout


def compile_baseline(archive, family, pole, directory, source, include, library):
    prefix = f'{family}/table_{pole}/'
    for name in ('plan.cpp', 'process.hpp', 'operands.i32'):
        with archive.extractfile(prefix + name) as src, (directory / name).open('wb') as dst:
            shutil.copyfileobj(src, dst)
    darwin = sys.platform == 'darwin'
    section = '.section __TEXT,__const' if darwin else '.section .rodata'
    label = ('_' if darwin else '') + 'hepcat_rate_operands'
    asm = directory / 'baseline.S'
    data = f'{section}\n.p2align 2\n.globl {label}\n{label}:\n.incbin "{directory}/operands.i32"'
    layout = re.search(r'^// Narrow operand stream byte offset = (\d+)$',
                       (directory / 'plan.cpp').read_text(), re.MULTILINE)
    if layout:
        split = int(layout[1])
        if split % 4 or not 0 < split < (directory / 'operands.i32').stat().st_size:
            raise ValueError('Invalid narrow operand stream offset')
        narrow_label = ('_' if darwin else '') + 'hepcat_rate_narrow'
        data += f',0,{split}\n.p2align 1\n.globl {narrow_label}\n{narrow_label}:\n.incbin "{directory}/operands.i32",{split}'
    asm.write_text(data + '\n')
    binary = directory / 'baseline.dylib'
    run('clang++', '-O2', '-std=c++17', '-fPIC', '-dynamiclib' if darwin else '-shared',
        '-I'+str(directory), '-I'+str(source), '-I'+str(include),
        directory/'plan.cpp', asm, HERE/'rate-spinas-bridge.cpp', library, '-o', binary)
    return binary


def compile_direct(directory, runtime, include, library):
    binary = directory/'direct.dylib'
    run('clang++', '-O2', '-std=c++17', '-fPIC', '-dynamiclib' if sys.platform == 'darwin' else '-shared',
        '-I'+str(directory), '-I'+str(runtime), '-I'+str(include),
        directory/'process.cpp', HERE/'rate-spinas-bridge.cpp', library, '-o', binary)
    return binary


def measure(timer, backend, lib, row, output, points):
    run(timer, backend, lib, row['pairs'], row['photons'], points, 3, row['seed'], output)
    fields = {}
    for line in output.read_text().splitlines():
        key, val = line.split()
        fields.setdefault(key, []).append(float(val))
    if len(fields['value']) != points or len(fields['seconds_per_point']) != 3:
        raise ValueError(f'Incomplete measurement in {output}')
    return dict(times=fields['seconds_per_point'], weights=fields['value'],
                peak_rss=int(fields['peak_rss_bytes'][0]))


def max_error(sample, reference):
    if len(sample) != len(reference) or not all(math.isfinite(x) and x > 0 for x in [*sample, *reference]):
        raise ValueError('Different point sets')
    worst = max(abs(x-y)/max(abs(x), abs(y)) for x,y in zip(sample, reference))
    if not worst <= 1e-7:
        raise ValueError(f'Complete rates disagree by {worst}')
    return worst


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    for name in ('exporter', 'timer', 'polarized_checker', 'include', 'library', 'manifest',
                 'baseline_archive', 'output'):
        ap.add_argument(name, type=Path)
    ap.add_argument('--representatives', type=Path,
                    help='Optional JSON mapping family, pole form and helicity word to native .expr or .expr.gz files')
    ap.add_argument('--family', action='append', help='Repeat to select multiple families')
    ap.add_argument('--slot-layout', choices=['compact','sequential'], default='compact',
                    help='Layout under test in an operand-table replay (the cache study measured compact)')
    ap.add_argument('--direct', action='store_true',
                    help='Compile the direct C++ code in both generations. Use a separate output directory.')
    ap.add_argument('--points', type=int, default=24, help='Matched phase-space points for each complete rate')
    ap.add_argument('--polarized-points', type=int, default=4)
    args = ap.parse_args()
    if args.points < 1 or args.polarized_points < 1:
        ap.error('Point counts must be positive')
    rows = json.loads((HERE/'rate-study.json').read_text())
    manifest = json.loads(args.manifest.read_text())
    representatives = json.loads(args.representatives.read_text()) if args.representatives else {}
    output = args.output.resolve()
    output.mkdir(parents=True, exist_ok=True)
    with tarfile.open(args.baseline_archive) as archive:
        for row in rows:
            family = row['family']
            if args.family and family not in args.family:
                continue
            for pole, model in [('physical', 4), ('spurious', 5)]:
                checkpoint = output / f'{family}-{pole}.json'
                if checkpoint.exists():
                    existing = json.loads(checkpoint.read_text())
                    if (existing.get('build', 'table') != ('direct' if args.direct else 'table') or
                        (not args.direct and existing.get('slot_layout', 'compact') != args.slot_layout) or
                        len(existing['after']['weights']) != args.points or
                        int(existing['polarized']['amplitudes']) != args.polarized_points*(1 << row['legs'])):
                        raise ValueError(f'Incompatible checkpoint {checkpoint}. Choose a new output directory.')
                    continue
                print(f'{family} {pole}', flush=True)
                with tempfile.TemporaryDirectory(dir=output) as temp:
                    directory = Path(temp)
                    source = directory/'new'
                    inputs = []
                    if family in representatives:
                        folder = directory/'expressions'
                        folder.mkdir()
                        for word, filename in representatives[family][pole].items():
                            path = Path(filename)
                            opener = gzip.open if path.suffix == '.gz' else open
                            with opener(path, 'rb') as src, (folder/(word+'.expr')).open('wb') as dst:
                                shutil.copyfileobj(src, dst)
                        inputs.append(folder)
                    run(args.exporter, row['pairs'], row['photons'], model, source, *inputs)
                    if inputs:
                        shutil.rmtree(folder)
                    plan = directory/'plan'
                    if args.direct:
                        for name in ('process.cpp', 'process.hpp'):
                            with archive.extractfile(f'{family}/{pole}/{name}') as src, (directory/name).open('wb') as dst:
                                shutil.copyfileobj(src, dst)
                        baseline = compile_direct(directory, source, args.include, args.library)
                        updated = compile_direct(source, source, args.include, args.library)
                    else:
                        run(sys.executable, HERE/'build-rate-plan.py', source, plan, args.include, args.library,
                            '--slot-layout', args.slot_layout)
                        baseline = compile_baseline(archive, family, pole, directory, source,
                                                    args.include, args.library)
                        updated = plan/'rate.dylib'
                    polarized = run(args.polarized_checker, baseline, updated, row['pairs'],
                                    row['photons'], args.polarized_points, row['seed'])
                    result = dict(family=family, pole=pole, seed=row['seed'],
                                  polarized=dict(line.split() for line in polarized.splitlines()))
                    for key, backend, lib in [('mg', 'MadGraph', manifest[family]['mg']),
                                              ('before', 'SPINAS', baseline),
                                              ('after', 'SPINAS', updated)]:
                        result[key] = measure(args.timer, backend, lib, row, directory/(key+'.txt'), args.points)
                        result[key]['mg_error'] = max_error(result[key]['weights'], result['mg']['weights'])
                    common = min(args.points, len(row['rate']['mg']['point_weights']))
                    max_error(result['mg']['weights'][:common], row['rate']['mg']['point_weights'][:common])
                    result['mg_error'] = max_error(result['after']['weights'], result['mg']['weights'])
                    if 'ch' in row['rate']:
                        result['archived_ch_error'] = max_error(result['after']['weights'][:common],
                                                                row['rate']['ch']['point_weights'][:common])
                    result['source_bytes'] = sum((source/name).stat().st_size for name in ('process.cpp', 'process.hpp'))
                    result['build'] = 'direct' if args.direct else 'table'
                    if not args.direct:
                        result['slot_layout'] = args.slot_layout
                        calls = re.findall(
                            r'return rate_plan<[^>]+>\(indices, \d+, (\d+)(?:, (\d+), \d+(?:, shared(?:, \d+)?)?)?\);',
                            (directory/'plan.cpp').read_text())
                        result['before_slots'] = max(int(peak or count) for count, peak in calls)
                        result['stats'] = json.loads((plan/'plan-stats.json').read_text())
                        result['table_bytes'] = sum((plan/name).stat().st_size for name in ('plan.cpp', 'process.hpp', 'operands.i32'))
                    checkpoint.write_text(json.dumps(result, indent=1)+'\n')


if __name__ == '__main__':
    main()
