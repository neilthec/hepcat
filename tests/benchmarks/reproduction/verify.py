#!/usr/bin/env python3
"""Verify the saved paper evidence and regenerate tables from retained data.

This uses exact catalog transcription tests, complete serialized precision
rates and original validation reports. It does not rerun Mathematica or an
independent amplitude generator and does not turn sampled evidence into proof.
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
from collections import Counter
import csv
from decimal import Decimal, localcontext
import hashlib
import importlib.util
import io
import json
from pathlib import Path
import subprocess
import sys
import zipfile

HERE = Path(__file__).resolve().parent
REPOSITORY = HERE.parents[2]


def load(path):
    return json.loads(path.read_text(encoding='utf-8'))


def require(condition, message):
    if not condition:
        raise ValueError(message)


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def read_csv(path):
    with path.open(newline='') as stream:
        return list(csv.DictReader(stream))


def matches(displayed, actual):
    value = Decimal(displayed)
    require(value == Decimal(actual).quantize(value),
            f'Published value {value} differs from replayed {actual}')


def check_manifest(data):
    manifest = load(data / 'manifest.json')
    for item in manifest['files']:
        path = data / item['path']
        require(path.is_file() and path.stat().st_size == item['bytes'] and
                sha(path) == item['sha256'], f'Evidence checksum mismatch: {path}')
    with zipfile.ZipFile(data / 'precision-results.zip') as archive:
        expected = load(data / 'precision-source-records.json')['files']
        require(set(archive.namelist()) == {r['path'] for r in expected},
                'Pointwise precision archive has missing or unexpected entries')
        for item in expected:
            content = archive.read(item['path'])
            require(len(content) == item['bytes'] and
                    hashlib.sha256(content).hexdigest() == item['sha256'],
                    f'Pointwise precision content mismatch: {item["path"]}')
    return {'files': len(manifest['files']), 'precision_result_files': len(expected)}


def check_sm(data):
    record = load(data / 'sm-four-point-results.json')
    summary, index, reports = record['Summary'], record['Index'], record['Reports']
    require(len(index) == len(reports) == 74, 'Expected 74 distinct SM export families')
    require(len({row['report'] for row in index}) == 74, 'Duplicate SM report')
    helicity_rows = []
    for row in index:
        report = reports[Path(row['report']).name]
        require(row['calchep_status'] == row['madgraph_status'] == report['Status'] == 'PASS',
                f'SM family did not pass: {row["export"]}')
        helicity_rows.extend(report['Rows'])
    require(len(helicity_rows) == 132 and all(
        r['Status'] == 'PASS' and r['RequestedPoints'] == r['ComparedPoints'] == 1000 and
        r['Mismatches'] == r['EvaluationErrors'] == 0 for r in helicity_rows),
        'SM MadGraph rows contain incomplete or failed comparisons')
    feynman, exports, captured_amplitudes = [], [], 0
    for status in record['StatusRecords'].values():
        require(all(not r.get('Failure') for r in status['Amplitudes']),
                'An archived SM construction failed')
        for cell in status.get('Cells', {}).values():
            feynman.extend(cell['Feynman'])
            captured_amplitudes += bool(cell['Feynman'])
        exports.extend(status['Exports'])
    require(captured_amplitudes == 115 and len(feynman) == 133 and set(feynman) == {'PASS'},
            'Unexpected or failed captured Feynman component comparisons')
    require(len(exports) == 74 and all(
        not r.get('Failure') and r['CalcHEP'] == r['MadGraph'] == 'PASS' for r in exports),
        'SM raw export captures do not establish 74 independent-reference passes')
    require(summary['Constructed'] == summary['FeynmanCasesPassed'] == 132 and
            summary['CalcHEP'] == summary['MadGraph'] == {'PASS': 74},
            'SM summary disagrees with the underlying records')
    zeros = [r for r in helicity_rows if r.get('ExpectedExactZero')]
    require(len(zeros) == 10 and all(r['ZeroAbsoluteTolerance'] == 1e-22 and
            r['MaximumAbsoluteError'] <= 1e-22 for r in zeros),
            'The recorded exact-zero four-gluon policy was not followed')
    return {'constructed': 132, 'Feynman_passes': 132, 'CalcHEP_family_passes': 74,
            'MadGraph_family_passes': 74, 'MadGraph_helicity_rows': 132,
            'Feynman_scope': 'The saved run summary reports 132 passes. Detailed capture files cover 115 amplitudes with 133 passing color-component rows; sections 71 and 73 retain their totals in the run summary. Rerun the public notebook for a fresh complete check.',
            'MadGraph_compared_points': sum(r['ComparedPoints'] for r in helicity_rows)}


def check_formula_records(data):
    result = {}
    for folder, total in [('one-minus-study', 5200), ('four-fermion-all-plus-study', 6000)]:
        report = load(data / folder / 'validation-summary.json')
        require(report['TotalMadGraphPoints'] == total == sum(r['Points'] for r in report['Runs']),
                f'Incorrect completed comparison count: {folder}')
        require(all(r['Status'] == 'PASS' for r in report['Runs']),
                f'An archived formula comparison failed: {folder}')
        result[folder] = {'completed_MadGraph_points': total}
    for n in range(2, 7):
        r = load(data / 'literature-study/all-positive' / f'native-N{n}.json')
        require(r['Samples'] == 1000 and r['Failures'] == 0,
                f'Published all-positive formula comparison failed at N={n}')
    public = load(data / 'all-positive-notebook-validation.json')
    for n, count in [(5, 10000), (6, 100000), (7, 1000000)]:
        rows = [r for r in public['Rows'] if r['Photons'] == n]
        require(len(rows) == 2 and {r['Form'] for r in rows} == {'Physical', 'Spurious'} and
                all(r['Status'] == 'PASS' and r['Points'] == count for r in rows),
                f'Completed public API evidence missing at N={n}')
    result['notebook_public_API'] = public
    result['scope'] = ('These checks inspect completed saved reports. They do not rerun '
                       'the generators and do not establish an all-multiplicity theorem.')
    return result


def check_timing_values(data):
    records = load(data / 'replay-source-records.json')
    aliases = records.get('aliases', {})
    maxima = {'physical_spurious': Decimal(0), 'CalcHEP': Decimal(0), 'MadGraph': Decimal(0)}

    def discrepancy(a, b):
        a, b = Decimal(a), Decimal(b)
        require(a.is_finite() and b.is_finite() and min(a, b) >= 0, 'Invalid saved benchmark rate')
        return abs(a - b) / max(a, b) if a or b else Decimal(0)

    with zipfile.ZipFile(data / 'precision-replay.zip') as archive:
        def content(name):
            return archive.read(aliases.get(name, name))

        def rows(name):
            return list(csv.DictReader(io.StringIO(content(name).decode())))

        for n in (2, 3, 4):
            p = f'timings/one-pair/physical/timings-points-N{n}.csv'
            s = f'timings/one-pair/spurious/timings-points-N{n}.csv'
            require(content(p) == content(s), f'One-pair N={n}: point streams differ')
            physical = rows(f'timings/one-pair/physical/timings-values-N{n}.csv')
            spurious = rows(f'timings/one-pair/spurious/timings-values-N{n}.csv')
            require(len(physical) == len(spurious) == 10000, 'Incomplete one-pair benchmark')
            for number, (a, b) in enumerate(zip(physical, spurious), 1):
                require(int(a['point']) == int(b['point']) == number, 'Mismatched timing point IDs')
                pairs = [('physical_spurious', a['spinas'], b['spinas'])]
                pairs += [(name, r['spinas'], r[col]) for r in (a, b)
                          for name, col in [('CalcHEP', 'calchep'), ('MadGraph', 'madgraph')]]
                for name, left, right in pairs:
                    error = discrepancy(left, right)
                    require(error <= Decimal('1e-7'), f'One-pair N={n} point={number}: {name} differs')
                    maxima[name] = max(maxima[name], error)
        for n in (0, 1, 2):
            values = rows(f'fermions/inputs/timings-values-N{n}.csv')
            require(len(values) == 10000, 'Incomplete four-fermion benchmark')
            for number, row in enumerate(values, 1):
                require(int(row['point']) == number, 'Mismatched four-fermion point IDs')
                pairs = [('physical_spurious', row['physical'], row['spurious'])]
                pairs += [(name, row[form], row[column]) for form in ('physical', 'spurious')
                          for name, column in [('CalcHEP', 'calchep'), ('MadGraph', 'madgraph')]]
                for name, left, right in pairs:
                    error = discrepancy(left, right)
                    require(error <= Decimal('1e-7'), f'Four-fermion N={n} point={number}: {name} differs')
                    maxima[name] = max(maxima[name], error)
    return {'one_pair_common_points': 30000, 'two_pair_common_points': 30000,
            'maximum_recorded_rate_discrepancies': {k: str(v) for k, v in maxima.items()},
            'scope': 'Recompares all four archived backends at the original matched points; no new evaluations.'}


def check_rounding(data):
    records = {}
    with zipfile.ZipFile(data / 'rounding-results.zip') as archive:
        for case in ('arm-long-double-saved', 'x86-double-saved', 'x86-long-double-saved',
                     'x86-double-projected', 'x86-long-double-projected'):
            with archive.open(case + '-values.csv') as binary:
                values = list(csv.DictReader(io.TextIOWrapper(binary)))
            with archive.open(case + '-spins.csv') as binary:
                spins = list(csv.DictReader(io.TextIOWrapper(binary)))
            require(len(values) == 1000 and len(spins) == 4000 and
                    all(r['status'] == 'PASS' for r in values),
                    f'Incomplete rounding comparison: {case}')
            require({int(r['point']) for r in values} == set(range(1, 1001)),
                    f'Invalid point inventory: {case}')
            complex_errors = []
            with localcontext() as context:
                context.prec = 60
                rate_errors = []
                for row in values:
                    a, b = Decimal(row['physical_spin_sum']), Decimal(row['spurious_spin_sum'])
                    require(a.is_finite() and b.is_finite() and min(a, b) >= 0,
                            f'Invalid squared amplitude in {case}')
                    rate_errors.append(abs(a - b) / max(a, b) if a or b else Decimal(0))
                seen = set()
                for row in spins:
                    key = (int(row['point']), int(row['spin1']), int(row['spin2']))
                    require(key not in seen and key[0] in range(1, 1001) and
                            key[1] in (-1, 1) and key[2] in (-1, 1),
                            f'Invalid or duplicate spin inventory: {case}')
                    seen.add(key)
                    ar, ai = Decimal(row['physical_real']), Decimal(row['physical_imag'])
                    br, bi = Decimal(row['spurious_real']), Decimal(row['spurious_imag'])
                    require(all(x.is_finite() for x in (ar, ai, br, bi)),
                            f'Nonfinite complex amplitude: {case}')
                    scale = max(ar * ar + ai * ai, br * br + bi * bi)
                    complex_errors.append((((ar - br)**2 + (ai - bi)**2) / scale).sqrt()
                                          if scale else Decimal(0))
            records[case] = {'points': len(values), 'complex_maximum': str(max(complex_errors)),
                             'squared_maximum': str(max(rate_errors)),
                             'scope': 'Recomputed from serialized full-precision amplitude and spin-sum columns.'}
    return records


def check_precision_tables(output, data, rounding):
    claims = load(data / 'published-values.json')
    rows = read_csv(output / 'summary.csv')
    indexed = {(r['family'], r['mode'], r['form']): r for r in rows if r['comparison'] == 'madgraph'}
    modes, forms = ('float', 'double', 'extended'), ('physical', 'spurious')
    table = []
    for claim in claims['catalog'] + [dict(family=f'N{r["photons"]}', values=r['values']) for r in claims['high_n']]:
        actual = [indexed[claim['family'], m, f]['maximum'] for m in modes for f in forms]
        require(len(actual) == len(claim['values']) == 6, 'Incomplete precision table')
        for expected, value in zip(claim['values'], actual):
            matches(expected, value)
        table.append({'family': claim['family'], **dict(zip(
            (m + '_' + f for m in modes for f in forms), actual))})
    for claim in claims['rounding']:
        values = rounding[claim['case']]
        for expected, key in zip(claim['values'], ('complex_maximum', 'squared_maximum')):
            matches(expected, values[key])
    with (output / 'paper-precision-tables.csv').open('w', newline='') as stream:
        writer = csv.DictWriter(stream, fieldnames=list(table[0]))
        writer.writeheader(); writer.writerows(table)
    return {'printed_values_checked': 72, 'catalog_families': 7,
            'catalog_common_points': sum(int(indexed[f['family'], 'double', 'physical']['points'])
                                         for f in claims['catalog']),
            'photon_helicity_sectors': 34, 'photon_free_sectors': 2}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--data-directory', type=Path, help='Previously extracted evidence inputs')
    parser.add_argument('--evidence-archive', type=Path, default=HERE / 'paper-evidence.zip')
    parser.add_argument('--models', type=Path, default=REPOSITORY / 'models')
    parser.add_argument('--output-directory', type=Path,
                        default=REPOSITORY / 'tests/results/reproduction')
    args = parser.parse_args()
    args.output_directory.mkdir(parents=True, exist_ok=True)
    if args.data_directory is None:
        spec = importlib.util.spec_from_file_location('precision_replay', HERE / 'replay-precision.py')
        replay = importlib.util.module_from_spec(spec); spec.loader.exec_module(replay)
        args.data_directory = args.output_directory / 'evidence-inputs'
        with zipfile.ZipFile(args.evidence_archive) as archive:
            replay.safe_extract(archive, args.data_directory)
    report = {'scope': __doc__.strip(), 'checksums': check_manifest(args.data_directory)}
    print('PASS: all curated evidence checksums and raw precision content hashes', flush=True)
    report['SM'] = check_sm(args.data_directory)
    print('PASS: saved 132-case SM summary and 74 complete CalcHEP/MadGraph family reports', flush=True)
    report['formula_records'] = check_formula_records(args.data_directory)
    print('PASS: completed formula-validation reports and notebook sample counts', flush=True)
    report['timing_comparisons'] = check_timing_values(args.data_directory)
    print('PASS: all four archived backends on 60,000 matched timing points', flush=True)
    subprocess.run([sys.executable, str(HERE / 'catalog-check.py'), '--models', str(args.models),
                    '--output-directory', str(args.output_directory / 'catalog'),
                    '--paper-model-labels'], check=True)
    report['catalog_sources'] = {
        'scope': 'Frozen paper labels 3/4 identify the historical catalog only. Current model files 4/5 supply the unchanged expressions.',
        'models': [
            {'preference': preference, 'current_model': number,
             'paper_model_label': number - 1,
             'source_file': str(args.models / f'amps{number}.mdl'),
             'source_sha256': sha(args.models / f'amps{number}.mdl')}
            for number, preference in ((4, 'AllPhysical'), (5, 'AllSpurious'))]}
    claims = load(args.data_directory / 'published-values.json')
    for filename, expected in claims['catalog_outputs'].items():
        require(sha(args.output_directory / 'catalog' / filename) == expected,
                f'Regenerated catalog differs from the paper: {filename}')
    with zipfile.ZipFile(args.data_directory / 'precision-results.zip') as archive:
        spec = importlib.util.spec_from_file_location('precision_analysis', HERE / 'analyze-precision.py')
        analyzer = importlib.util.module_from_spec(spec); spec.loader.exec_module(analyzer)
        analyzer.analyze(None, args.output_directory / 'precision', archive)
    rounding = check_rounding(args.data_directory)
    report['precision'] = check_precision_tables(args.output_directory / 'precision', args.data_directory, rounding)
    report['rounding'] = rounding
    print('PASS: all 72 printed precision values from complete pointwise numerical results', flush=True)
    report['Status'] = 'PASS'
    target = args.output_directory / 'evidence-verification.json'
    target.write_text(json.dumps(report, indent=2) + '\n')
    print(target)


if __name__ == '__main__':
    main()
