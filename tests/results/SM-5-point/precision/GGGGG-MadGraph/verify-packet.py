"""Independently recompute every recorded precision comparison. This does not qualify a canonical result."""
from pathlib import Path
from decimal import Decimal, localcontext
import csv
import gzip
import hashlib
import itertools
import json
import struct
import sys

HERE = Path(__file__).resolve().parent
report = json.loads((HERE / 'precision-summary.json').read_text())
for relative, expected in report['Files'].items():
    path = (HERE / relative).resolve()
    assert path.is_relative_to(HERE.resolve()) and path.is_file()
    assert hashlib.sha256(path.read_bytes()).hexdigest() == expected
raw = json.loads((HERE / report['RawReportFile']).read_text())
states = {tuple(row['Helicities']) for row in raw['Rows']}
zero_states = {tuple(row['Helicities']) for row in raw['Rows'] if row['ExpectedExactZero']}
assert len(states) == 32 and states == set(itertools.product((-2, 2), repeat=5))

def read_csv(name):
    with gzip.open(HERE / name, 'rt', newline='') as stream:
        return list(csv.DictReader(stream))

native_rows = read_csv(report['NativeValuesFile'])
native = {}
for row in native_rows:
    key = (int(row['point']), tuple(int(row[f'h{i}']) for i in range(1, 6)))
    assert key not in native
    native[key] = float(row['normalized'])
expected_keys = {(point, state) for point in range(1, 1001) for state in states}
assert set(native) == expected_keys

binary = (HERE / report['Binary64PointsFile']).read_bytes()
assert len(binary) == 160000
with (HERE / report['PointsFile']).open(newline='') as stream:
    points = list(csv.reader(stream))[1:]
assert len(points) == 1000
for index, row in enumerate(points):
    assert int(row[0]) == index + 1
    assert struct.pack('<20d', *map(float, row[1:])) == binary[index*160:(index+1)*160]
saved_point = json.loads((HERE / report['Point719File']).read_text())
assert struct.pack('<20d', *(value for row in saved_point['Momenta'] for value in row)) == binary[718*160:719*160]
for failed in saved_point['FailedRows']:
    key = (719, tuple(failed['Helicities']))
    assert struct.pack('d', native[key]) == struct.pack('d', failed['FirstFailure']['HEPCATValue'])

with gzip.open(HERE / report['Reference113ValuesFile'], 'rt') as stream:
    precision = next(stream)
    assert '113' in precision and '128' in precision and '64' in precision
    quad = {}
    for line in stream:
        fields = line.split()
        assert len(fields) == 7
        key = (int(fields[0]), tuple(map(int, fields[1:6])))
        assert key not in quad
        quad[key] = Decimal(fields[6])
assert set(quad) == expected_keys
comparisons = read_csv(report['IndependentComparisonFile'])
assert len(comparisons) == 32000
seen = set()
mismatches = 0
maximum_nonzero_relative = Decimal(0)
maximum_zero_absolute = Decimal(0)
with localcontext() as context:
    context.prec = 90
    normalization = Decimal.from_float(float.fromhex(report['NormalizationBinary64Hex']))
    relative_tolerance = Decimal.from_float(1e-7)
    zero_tolerance = Decimal.from_float(1e-22)
    for row in comparisons:
        key = (int(row['Point']), tuple(int(row[f'H{i}']) for i in range(1, 6)))
        assert key not in seen
        seen.add(key)
        assert Decimal(row['Native53Normalized']) == Decimal.from_float(native[key])
        assert Decimal(row['MadGraph113Raw']) == quad[key]
        reference = normalization * quad[key]
        value = Decimal.from_float(native[key])
        error = abs(reference - value)
        scale = max(abs(reference), abs(value))
        relative = error / scale if scale else Decimal(0)
        exact_zero = key[1] in zero_states
        accepted = error <= (zero_tolerance if exact_zero else relative_tolerance * scale)
        assert int(row['Passed']) == int(accepted)
        mismatches += not accepted
        if exact_zero:
            maximum_zero_absolute = max(maximum_zero_absolute, error)
        else:
            maximum_nonzero_relative = max(maximum_nonzero_relative, relative)
assert seen == expected_keys and mismatches == 0
print(json.dumps({'Status': 'ALL_32000_COMPARISONS_RECOMPUTED_PASS',
                  'Mismatches': mismatches, 'ComparedHelicityPoints': len(seen),
                  'MaximumNonzeroRelativeError': str(maximum_nonzero_relative),
                  'MaximumZeroAbsoluteError': str(maximum_zero_absolute),
                  'CanonicalQualificationPerformed': False}, indent=2))
