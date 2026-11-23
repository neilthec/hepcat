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
    asm.write_text(f'{section}\n.p2align 2\n.globl {label}\n{label}:\n.incbin "{directory}/operands.i32"\n')
    binary = directory / 'baseline.dylib'
    run('clang++', '-O2', '-std=c++17', '-fPIC', '-dynamiclib' if darwin else '-shared',
        '-I'+str(directory), '-I'+str(source), '-I'+str(include),
        directory/'plan.cpp', asm, HERE/'rate-spinas-bridge.cpp', library, '-o', binary)
    return binary


def measure(timer, backend, lib, row, output):
    run(timer, backend, lib, row['pairs'], row['photons'], 24, 3, row['seed'], output)
    fields = {}
    for line in output.read_text().splitlines():
        key, val = line.split()
        fields.setdefault(key, []).append(float(val))
    if len(fields['value']) != 24 or len(fields['seconds_per_point']) != 3:
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
    ap.add_argument('--polarized-points', type=int, default=4)
    args = ap.parse_args()
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
                    plan = directory/'plan'
                    run(sys.executable, HERE/'build-rate-plan.py', source, plan, args.include, args.library)
                    baseline = compile_baseline(archive, family, pole, directory, source,
                                                args.include, args.library)
                    polarized = run(args.polarized_checker, baseline, plan/'rate.dylib', row['pairs'],
                                    row['photons'], args.polarized_points, row['seed'])
                    result = dict(family=family, pole=pole, seed=row['seed'],
                                  polarized=dict(line.split() for line in polarized.splitlines()))
                    for key, backend, lib in [('mg', 'MadGraph', manifest[family]['mg']),
                                              ('before', 'SPINAS', baseline),
                                              ('after', 'SPINAS', plan/'rate.dylib')]:
                        result[key] = measure(args.timer, backend, lib, row, directory/(key+'.txt'))
                        result[key]['mg_error'] = max_error(result[key]['weights'], row['rate']['mg']['point_weights'])
                    result['mg_error'] = max_error(result['after']['weights'], result['mg']['weights'])
                    if 'ch' in row['rate']:
                        result['archived_ch_error'] = max_error(result['after']['weights'],
                                                                row['rate']['ch']['point_weights'])
                    counts = re.findall(r'return rate_plan<[^>]+>\(indices, \d+, (\d+)\);',
                                        (directory/'plan.cpp').read_text())
                    result['before_slots'] = max(map(int, counts))
                    result['stats'] = json.loads((plan/'plan-stats.json').read_text())
                    result['source_bytes'] = sum((source/name).stat().st_size for name in ('process.cpp', 'process.hpp'))
                    result['table_bytes'] = sum((plan/name).stat().st_size for name in ('plan.cpp', 'process.hpp', 'operands.i32'))
                    checkpoint.write_text(json.dumps(result, indent=1)+'\n')


if __name__ == '__main__':
    main()
