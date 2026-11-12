"""Metadata-only audit of nine actual ordinary constructor/export checkpoints.

The projection was imported from the small export WXF capsules without loading
HEPCAT or calculating amplitudes. This script checks canonical file bytes,
preserves raw backend reports, and extracts their actual numerical counts.
"""
from pathlib import Path
import hashlib
import json
import math
import re
import shutil

ROOT = Path('/Users/neil/code/hepcat')
HERE = Path(__file__).resolve().parent
RESULTS = ROOT / 'tests/results/SM-5-point'
CURRENT = '55701e7a9f6d674b7f37aa1ced637de0e9e1934badd54be9dce03244084725f9'


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def read(path):
    return json.loads(path.read_text())


def relative(path):
    return str(Path(path).relative_to(ROOT))


def store(path, data):
    path.parent.mkdir(parents=True, exist_ok=True)
    assert not path.exists(), 'Audit files are immutable.'
    path.write_text(json.dumps(data, indent=2, sort_keys=True) + '\n')
    return {'Path': relative(path), 'SHA256': digest(path)}


def copy(path, destination):
    destination.parent.mkdir(parents=True, exist_ok=True)
    assert not destination.exists(), 'Audit files are immutable.'
    shutil.copyfile(path, destination)
    assert digest(path) == digest(destination)
    return {'Path': relative(destination), 'SHA256': digest(destination), 'OriginalPath': str(path)}


def one(pattern, text):
    matches = re.findall(pattern, text, re.MULTILINE)
    assert len(matches) == 1, (pattern, len(matches))
    return matches[0]


rows = []
projection = read(HERE / 'backend-projection.json')
assert len(projection) == 9
for projected in projection:
    family = projected['FamilyID']
    export_path = RESULTS / (family + '-export.json')
    export_capsule = RESULTS / (family + '-export.wxf')
    export = read(export_path)
    assert export['Status'] == export['CalcHEPStatus'] == export['MadGraphStatus'] == 'PASS'
    assert export['Points'] == 1000 and export['ContextSHA256'] == export['ExporterContextSHA256'] == CURRENT
    assert digest(export_capsule) == export['CapsuleSHA256'] == projected['CapsuleSHA256']
    assert projected['Record']['CaseCapsulesSHA256'] == export['CaseCapsulesSHA256']
    assert projected['Record']['ContextSHA256'] == CURRENT
    native = []
    for case in export['CaseIDs']:
        path = RESULTS / (case + '.json')
        capsule = RESULTS / (case + '.wxf')
        item = read(path)
        assert item['ID'] == case and item['Status'] == 'COMPLETE' and item['ContextSHA256'] == CURRENT
        assert item['FeynmanRows'] > 0 and item['Rows'] > 0 and item['CheckStatuses'] == {'PASS': item['Rows']}
        assert digest(capsule) == item['CapsuleSHA256'] == export['CaseCapsulesSHA256'][case]
        assert export['NativeCaseContexts'][case] == CURRENT
        native.append({'ID': case, 'Particles': item['Particles'], 'ContextSHA256': CURRENT,
            'Status': item['Status'], 'TopLevelRows': item['Rows'], 'FeynmanRows': item['FeynmanRows'],
            'CheckStatuses': item['CheckStatuses'], 'Seconds': item['Seconds'], 'Complexity': item['Complexity'],
            'Capsule': {'Path': relative(capsule), 'SHA256': digest(capsule)},
            'Summary': copy(path, HERE / 'native-summaries' / path.name)})
    backend = projected['BackendResult']
    ch = backend['CalcHEPValidation']
    stdout = ch['StandardOutput']
    counts = tuple(map(int, one(r'^Points requested/generated/compared: (\d+)/(\d+)/(\d+)\s*$', stdout)))
    mismatch, errors = map(int, one(r'^Mismatches/errors: (\d+)/(\d+)\s*$', stdout))
    rel, absolute = map(float, one(r'^Maximum relative/absolute error: ([^/\r\n]+)/([^\r\n]+)\s*$', stdout))
    seed = int(one(r'^Seed: ([0-9]+)\s*$', stdout))
    assert counts == (1000, 1000, 1000) and mismatch == errors == 0
    assert ch['Status'] == 'PASS' and ch['ExitCode'] == 0 and ch['Points'] == 1000 and ch['Seed'] == seed
    assert all(math.isfinite(value) and value >= 0 for value in (rel, absolute))
    ch_raw = store(HERE / 'raw' / (family + '-calchep.json'), {
        'SourceExportCapsuleSHA256': digest(export_capsule), 'BackendResult': ch})
    mg = backend['MadGraphValidation']
    mg_original = Path(mg['ReportFile'])
    mg_raw = read(mg_original)
    assert mg_raw['Rows'] == mg['Rows'] and mg_raw['Status'] == mg['Status'] == 'PASS'
    assert mg_raw['Passed'] is True and mg['Passed'] is True
    mg_rows = mg['Rows']
    assert mg_rows and all(row['Status'] == 'PASS' and row['Passed'] is True
        and row['RequestedPoints'] == row['GeneratedPoints'] == row['ComparedPoints'] == row['Points'] == 1000
        and row['Mismatches'] == row['EvaluationErrors'] == 0 and row['FirstFailure'] is None
        for row in mg_rows)
    mg_reference = json.loads(mg_raw['ReferenceBuildJSON'])
    assert mg_reference == mg['Reference']
    assert mg_reference['ZeroedWidths'] and all(row['Value'] == 0 for row in mg_reference['ZeroedWidths'])
    assert mg_reference['PreservedWidths'] == []
    assert mg_reference['ParameterRules']['lamWS'] == 0
    manifest_path = Path(backend['Files']['Manifest'])
    manifest = read(manifest_path)
    assert manifest['CalcHEPValidation']['WidthPolicy'] == 'ZeroWidthsForIndependentValidation'
    assert manifest['MadGraphValidation']['Points'] == manifest['CalcHEPValidation']['Points'] == 1000
    generated_ch = Path(backend['Files']['CalcHEPSource'])
    assert 'set_widths_enabled(false)' in generated_ch.read_text()
    code_hashes = {str(Path(backend['Files'][key]).relative_to(ROOT)): digest(Path(backend['Files'][key]))
                   for key in ('Source', 'Header', 'Runtime', 'CalcHEPSource', 'CalcHEPRuntime')}
    rows.append({'FamilyID': family, 'Status': 'PASS', 'ContextSHA256': CURRENT,
        'Native': native, 'CaseCapsulesSHA256': export['CaseCapsulesSHA256'],
        'NativeCaseContexts': export['NativeCaseContexts'], 'ExporterContextSHA256': export['ExporterContextSHA256'],
        'ExportCapsule': {'Path': relative(export_capsule), 'SHA256': digest(export_capsule)},
        'ExportSummary': copy(export_path, HERE / 'export-summaries' / export_path.name),
        'ExportSeconds': export['Seconds'],
        'CalcHEP': {'Status': 'PASS', 'Seed': seed, 'RequestedPoints': counts[0], 'GeneratedPoints': counts[1],
            'ComparedPoints': counts[2], 'Mismatches': mismatch, 'EvaluationErrors': errors,
            'MaximumRelativeError': rel, 'MaximumAbsoluteError': absolute, 'RawReport': ch_raw,
            'WidthPolicy': manifest['CalcHEPValidation']['WidthPolicy'], 'SPINASWidthsEnabled': False},
        'MadGraph': {'Status': 'PASS', 'Seed': mg_raw['Seed'], 'Rows': mg_rows,
            'RequestedPoints': sum(row['RequestedPoints'] for row in mg_rows),
            'GeneratedPoints': sum(row['GeneratedPoints'] for row in mg_rows),
            'ComparedPoints': sum(row['ComparedPoints'] for row in mg_rows),
            'Mismatches': sum(row['Mismatches'] for row in mg_rows),
            'EvaluationErrors': sum(row['EvaluationErrors'] for row in mg_rows),
            'MaximumRelativeError': max(row['MaximumRelativeError'] for row in mg_rows),
            'MaximumAbsoluteError': max(row['MaximumAbsoluteError'] for row in mg_rows),
            'RawReport': copy(mg_original, HERE / 'raw' / (family + '-madgraph.json')),
            'WidthPolicy': mg_reference['WidthPolicy'], 'ZeroedWidths': mg_reference['ZeroedWidths'],
            'PreservedWidths': mg_reference['PreservedWidths']},
        'Manifest': copy(manifest_path, HERE / 'manifests' / (family + '.json')),
        'GeneratedSourceSHA256': code_hashes})
audit = {'SchemaVersion': 1, 'Status': 'NINE_CANONICAL_ORDINARY_EXPORTS_PASS',
    'ContextSHA256': CURRENT, 'Families': len(rows), 'BackendRuns': 2 * len(rows),
    'CalcHEPComparedPoints': sum(row['CalcHEP']['ComparedPoints'] for row in rows),
    'MadGraphComparedPoints': sum(row['MadGraph']['ComparedPoints'] for row in rows),
    'CalcHEPMaximumRelativeError': max(row['CalcHEP']['MaximumRelativeError'] for row in rows),
    'MadGraphMaximumRelativeError': max(row['MadGraph']['MaximumRelativeError'] for row in rows),
    'Mismatches': 0, 'EvaluationErrors': 0, 'Records': rows,
    'MetadataProjection': {'Path': relative(HERE / 'backend-projection.json'), 'SHA256': digest(HERE / 'backend-projection.json')},
    'AuditScriptSHA256': digest(Path(__file__)), 'PhysicsRerunPerformedForAudit': False,
    'CanonicalRecordsModifiedForAudit': False}
store(HERE / 'audit.json', audit)
print(json.dumps({key: value for key, value in audit.items() if key != 'Records'}, indent=2))
