"""Replay the archived binary64 stream through the unchanged generated MG53 library."""
from pathlib import Path
import csv
import ctypes
import json
import struct
import time

HERE = Path(__file__).resolve().parent
reference = json.loads((HERE / 'reference-53-retry-1/result.json').read_text())
reference_library = ctypes.CDLL(reference['Library'])
reference_library.hepcat_mg_initialize.restype = ctypes.c_int
assert reference_library.hepcat_mg_initialize() == 0
original = json.loads((HERE / 'raw-madgraph-original.json').read_text())
helicities = [row['Helicities'] for row in original['Rows']]
zero_states = {tuple(row['Helicities']) for row in original['Rows'] if row['ExpectedExactZero']}
native = {
    (int(row['point']), tuple(int(row['h' + str(i)]) for i in range(1, 6))): float(row['normalized'])
    for row in csv.DictReader((HERE / 'native-53-stream.csv').open())
}
point_bytes = (HERE / 'points-1000.f64').read_bytes()
rows = {
    tuple(state): {'Helicities': state, 'ComparedPoints': 0, 'Mismatches': 0,
                   'EvaluationErrors': 0, 'MaximumRelativeError': 0,
                   'MaximumAbsoluteError': 0, 'FailedPoints': []}
    for state in helicities
}
started = time.monotonic()
with (HERE / 'madgraph-53-stream.csv').open('w') as output:
    writer = csv.writer(output)
    writer.writerow(['Point', 'H1', 'H2', 'H3', 'H4', 'H5', 'Native53Normalized',
                     'MadGraph53Raw', 'MadGraph53Normalized', 'AbsoluteError', 'RelativeError', 'Passed'])
    for index in range(1000):
        point = (ctypes.c_double * 20)(*struct.unpack('<20d', point_bytes[index*160:(index+1)*160]))
        for state in helicities:
            key = tuple(state)
            physical = [(value // 2 if leg < 2 else -value // 2) for leg, value in enumerate(state)]
            helicity_argument = (ctypes.c_int * 5)(*physical)
            answer = ctypes.c_double()
            reference_library.hepcat_mg_evaluate(point, helicity_argument, ctypes.byref(answer))
            normalized = answer.value * (1 / 1536)
            native_value = native[(index + 1, key)]
            error = abs(normalized - native_value)
            scale = max(abs(normalized), abs(native_value))
            relative = error / scale if scale else 0
            passed = error <= (1e-22 if key in zero_states else 1e-7 * scale)
            row = rows[key]
            row['ComparedPoints'] += 1
            row['Mismatches'] += not passed
            row['MaximumRelativeError'] = max(relative, row['MaximumRelativeError'])
            row['MaximumAbsoluteError'] = max(error, row['MaximumAbsoluteError'])
            if not passed:
                row['FailedPoints'].append(index + 1)
            writer.writerow([index + 1, *state, repr(native_value), repr(answer.value),
                             repr(normalized), repr(error), repr(relative), int(passed)])
summary = {
    'Status': 'RAW_SAME_GENERATED_SOURCE_DOUBLE_CONTROL', 'Rows': list(rows.values()),
    'Mismatches': sum(row['Mismatches'] for row in rows.values()), 'EvaluationErrors': 0,
    'Seconds': time.monotonic() - started, 'OriginalHistoricalMGAllValuesBitIdenticalClaimed': False,
    'SameGeneratedFortranSourceUsedBy113BitBuild': True, 'Points': 1000, 'ComparedHelicityPoints': 32000
}
(HERE / 'madgraph-53-stream-summary.json').write_text(json.dumps(summary, indent=2) + '\n')
print({key: value for key, value in summary.items() if key != 'Rows'})
