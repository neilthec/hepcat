#!/usr/bin/env python3
"""Summarize matched SPINAS/MadGraph replays without rounding extended results."""

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
from decimal import Decimal, InvalidOperation, localcontext
import hashlib
import json
import gzip
import zipfile
import io
from contextlib import ExitStack
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPOSITORY = HERE.parents[2]
MODES = {'float': 24, 'double': 53, 'extended': 64}
FAMILIES = {
    **{name: 'one-pair/results/{family}-{mode}.csv' for name in ('eeAA', 'eeAAA', 'eeAAAA')},
    **{name: 'fermions/results/{mode}/{family}.csv' for name in ('eeee', 'eeeeA', 'eeeeAA', 'eeeeee')},
    **{f'N{n}': 'high-n/{family}-{mode}.csv' for n in (5, 6, 7, 8)},
}
HELICITY_FAMILIES = {
    **{name: ('one-pair/results/{family}-{mode}-helicity-comparison.csv', count, 'helicity')
       for name, count in (('eeAA', 4), ('eeAAA', 8), ('eeAAAA', 16))},
    **{name: ('fermions/results/{mode}/{family}-helicities-compared.csv', count, 'photon_helicity_mask')
       for name, count in (('eeeeA', 2), ('eeeeAA', 4))},
}
THRESHOLDS = tuple(Decimal(text) for text in ('1e-3', '1e-5', '1e-7', '1e-9', '1e-11', '1e-13'))


def read_rows(path, keys):
    opener = gzip.open(path, 'rt', newline='') if str(path).endswith('.gz') else path.open('r', newline='')
    with opener as stream:
        reader = csv.DictReader(stream)
        fields = (*keys, 'physical', 'spurious', 'madgraph')
        if not all(field in reader.fieldnames for field in fields):
            raise ValueError(f'Missing required columns in {path}: {reader.fieldnames}')
        values = {}
        for row in reader:
            key = tuple(row[field] for field in keys)
            if key in values:
                raise ValueError(f'Duplicate {key} in {path}')
            try:
                values[key] = {field: Decimal(row[field]) for field in fields[len(keys):]}
            except InvalidOperation as exc:
                raise ValueError(f'Invalid value for {key} in {path}') from exc
    if not values:
        raise ValueError(f'Empty results: {path}')
    return values


def percentile(sorted_values, fraction):
    if not sorted_values:
        return None
    index = (len(sorted_values) - 1) * Decimal(fraction)
    lower = int(index)
    upper = min(lower + 1, len(sorted_values) - 1)
    return sorted_values[lower] + (index - lower) * (sorted_values[upper] - sorted_values[lower])


def summary(pairs):
    comparisons, failures, zeros = [], [], 0
    for key, left, right in pairs:
        if not left.is_finite() or not right.is_finite() or min(left, right) < 0:
            failures.append('/'.join(key))
            continue
        absolute = abs(left - right)
        scale = max(left, right)
        zeros += scale == 0
        error = absolute / scale if scale else Decimal(0)
        comparisons.append((error, key, absolute, left, right))
    comparisons.sort(key=lambda record: record[0])
    errors = [record[0] for record in comparisons]
    result = {'points': len(comparisons) + len(failures), 'finite_pairs': len(comparisons),
              'invalid_pairs': len(failures), 'both_zero': zeros,
              'invalid_point_ids': failures,
              'median': percentile(errors, '0.5'), 'p95': percentile(errors, '0.95'),
              'p99': percentile(errors, '0.99'), 'maximum': errors[-1] if errors else None,
              'above': {str(t): sum(error > t for error in errors) for t in THRESHOLDS}}
    if comparisons:
        _, key, absolute, left, right = comparisons[-1]
        result.update({'worst_point': '/'.join(key), 'absolute_error_at_worst': absolute,
                       'value_at_worst': left, 'reference_at_worst': right,
                       'maximum_absolute_error': max(row[2] for row in comparisons)})
    return result


def process(paths, keys):
    datasets = {mode: read_rows(path, keys) for mode, path in paths.items()}
    reference = datasets['double']
    for mode, values in datasets.items():
        if values.keys() != reference.keys():
            raise ValueError(f'Mismatched point identities in {paths[mode]}')
        for key, row in values.items():
            baseline = reference[key]['madgraph']
            if row['madgraph'] != baseline:
                raise ValueError(f'MadGraph reference changed with precision for {key} in {paths[mode]}')
    records = []
    helicities = sorted({key[1] for key in reference}) if len(keys) == 2 else [None]
    for helicity in helicities:
        for mode, values in datasets.items():
            relevant = [key for key in values if helicity is None or key[1] == helicity]
            targets = [('physical', 'madgraph', values), ('spurious', 'madgraph', values),
                       ('spurious', 'physical', values)]
            if mode != 'extended':
                targets += [(form, f'{form}_extended', datasets['extended']) for form in ('physical', 'spurious')]
            for form, target, others in targets:
                column = target.removesuffix('_extended')
                stats = summary((key, values[key][form], others[key][column]) for key in relevant)
                records.append({'helicity': helicity, 'mode': mode, 'bits': MODES[mode],
                                'form': form, 'comparison': target, **stats})
    return records


def write_csv(path, rows):
    fields = ['family', 'helicity', 'mode', 'bits', 'form', 'comparison', 'points', 'finite_pairs', 'invalid_pairs',
              'both_zero', 'median', 'p95', 'p99', 'maximum', 'worst_point', 'absolute_error_at_worst',
              'value_at_worst', 'reference_at_worst', 'maximum_absolute_error',
              *(f'above_{t}' for t in THRESHOLDS)]
    with path.open('w', newline='') as stream:
        writer = csv.DictWriter(stream, fields, extrasaction='ignore')
        writer.writeheader()
        for row in rows:
            writer.writerow({**row, **{f'above_{t}': count for t, count in row['above'].items()}})


def input_file(directory, archive, relative):
    if archive is not None:
        return archive / relative
    path = directory / relative
    compressed = Path(str(path) + '.gz')
    return path if path.is_file() else compressed


def analyze(directory, output, archive=None, helicities=True):
    """Recompute statistics from all retained rates, never from saved maxima."""
    # Keep one ZIP path root alive. Python 3.9 otherwise creates independent
    # wrapper objects sharing a file handle that an older wrapper can close.
    archive = zipfile.Path(archive) if archive is not None else None
    output.mkdir(parents=True, exist_ok=True)
    records, sources = [], {}
    for family, template in FAMILIES.items():
        paths = {mode: input_file(directory, archive, template.format(family=family, mode=mode))
                 for mode in MODES}
        if not all(path.is_file() for path in paths.values()):
            raise FileNotFoundError(f'Missing pointwise data for {family}: {paths}')
        sources[family] = {mode: {'file': template.format(family=family, mode=mode),
                                  'sha256': hashlib.sha256(path.read_bytes()).hexdigest()}
                           for mode, path in paths.items()}
        with localcontext() as context:
            context.prec = 40
            family_records = process(paths, ['point'])
        expected = 1000 if family == 'eeeeee' or family.startswith('N') else 10000
        if any(row['points'] != expected or row['invalid_pairs'] for row in family_records):
            raise ValueError(f'{family}: expected {expected} valid comparisons, with no dropped points')
        records.extend({'family': family, **row} for row in family_records)
    report = {'scope': 'Statistics independently recomputed from serialized pointwise rates. '
                        'This does not rerun any amplitude evaluator or MadGraph.',
              'relative_error': 'abs(value-reference)/max(abs(value),abs(reference)); zero if both zero',
              'quantiles': 'Linear interpolation over all finite nonnegative rates. Invalid data is an error.',
              'sources': sources, 'statistics': records}
    (output / 'summary.json').write_text(json.dumps(report, default=str, indent=2) + '\n')
    write_csv(output / 'summary.csv', records)
    if helicities:
        helicity_records = []
        for family, (template, count, key) in HELICITY_FAMILIES.items():
            paths = {mode: input_file(directory, archive, template.format(family=family, mode=mode))
                     for mode in MODES}
            if not all(path.is_file() for path in paths.values()):
                raise FileNotFoundError(f'Missing fixed-helicity data for {family}: {paths}')
            with localcontext() as context:
                context.prec = 40
                family_records = process(paths, ['point', key])
            if len({row['helicity'] for row in family_records}) != count or any(
                    row['points'] != 10000 or row['invalid_pairs'] for row in family_records):
                raise ValueError(f'{family}: incomplete or invalid fixed-helicity comparisons')
            helicity_records.extend({'family': family, **row} for row in family_records)
        write_csv(output / 'helicity-summary.csv', helicity_records)
    return report


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    source = parser.add_mutually_exclusive_group()
    source.add_argument('--data-archive', type=Path, help='ZIP of unchanged pointwise CSVs')
    source.add_argument('--data-directory', type=Path, help='Matching result tree from a numerical replay')
    parser.add_argument('--output-directory', type=Path,
                        default=REPOSITORY / 'tests/results/reproduction/precision')
    parser.add_argument('--totals-only', action='store_true')
    args = parser.parse_args()
    with ExitStack() as stack:
        if args.data_archive:
            archive = stack.enter_context(zipfile.ZipFile(args.data_archive))
        elif not args.data_directory:
            evidence = stack.enter_context(zipfile.ZipFile(HERE / 'paper-evidence.zip'))
            archive = stack.enter_context(zipfile.ZipFile(io.BytesIO(evidence.read('precision-results.zip'))))
        else:
            archive = None
        report = analyze(args.data_directory, args.output_directory, archive, not args.totals_only)
    for row in report['statistics']:
        if row['comparison'] == 'madgraph':
            print(f"{row['family']:8s} {row['bits']:2d} {row['form']:8s} "
                  f"{row['finite_pairs']:6d}/{row['points']:<6d} max={row['maximum']:.3g}")
    print(args.output_directory / 'summary.csv')


if __name__ == '__main__':
    main()
