"""Read-only qualification of a separately reviewed MadGraph precision replay.

Raw export results never change. Numerical acceptance is recomputed from the
retained native-binary64 and reference streams, independently of PASS fields.
"""
# HEPCAT - High Energy Physics Constructive Amplitude Tool
# Copyright (C) 2026 Neil Christensen
# SPDX-License-Identifier: GPL-3.0-or-later
from __future__ import annotations
import csv
from decimal import Decimal, InvalidOperation, localcontext
import gzip
import hashlib
import importlib.util
import json
import math
from pathlib import Path
import re
import shlex
import struct

ROOT = Path(__file__).resolve().parents[2]
HERE = Path(__file__).resolve().parent
_spec = importlib.util.spec_from_file_location('sm5_mg_precision_constructor_context', HERE / 'constructor_context.py')
constructor_context = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(constructor_context)
_spec = importlib.util.spec_from_file_location('sm5_mg_precision_stage_context', HERE / 'stage_context.py')
stage_context = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(stage_context)
REVIEW_ASSERTIONS = {
    'AllCoreObjects113Bit', 'UnpromotedDriverExplicitBinary64ToQuad',
    'Generated53And113SourcesAndCardIdentical', 'OriginalFixedBinary64NormalizationReviewed',
    'NoKinematicRepair', 'NoHistoricalFullStreamBitIdentityClaim',
    'RawStatusesRemainUnchanged', 'AllRawComparisonsRequireIndependentRecomputation',
    'NoTolerancesEnlarged'}


def _require(value):
    if not value:
        raise ValueError('Incomplete or inconsistent MadGraph precision evidence.')


def _hash(path):
    h = hashlib.sha256()
    with path.open('rb') as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b''):
            h.update(block)
    return h.hexdigest()


def _object(path):
    data = json.loads(path.read_text())
    _require(isinstance(data, dict))
    return data


def _inside(base, value):
    _require(isinstance(value, str) and value and not Path(value).is_absolute()
             and '..' not in Path(value).parts)
    path = (base / value).resolve()
    path.relative_to(base.resolve())
    return path


def _matches(path, digest):
    return isinstance(digest, str) and re.fullmatch('[a-f0-9]{64}', digest) and path.is_file() and _hash(path) == digest


def _integer(value, minimum=0):
    return type(value) is int and value >= minimum


def _seed(value):
    if isinstance(value, str) and re.fullmatch(r'0|[1-9][0-9]*', value):
        value = int(value)
    _require(type(value) is int and 0 <= value < 2**64)
    return value


def _number(text):
    number = Decimal(str(text))
    _require(number.is_finite() and number >= 0)
    return number


def _float(text):
    value = float(text)
    _require(math.isfinite(value))
    return value


def _bits(value):
    return struct.pack('<d', _float(value))


def _state(value, legs):
    _require(isinstance(value, (list, tuple)) and len(value) == legs
             and all(type(v) is int for v in value))
    return tuple(value)


def _csv(path):
    with (gzip.open(path, 'rt', newline='') if path.suffix == '.gz' else path.open(newline='')) as stream:
        reader = csv.DictReader(stream)
        _require(reader.fieldnames and len(set(reader.fieldnames)) == len(reader.fieldnames))
        return list(reader)


def _key(row, legs, upper=True):
    return (int(row['Point' if upper else 'point']),
            tuple(int(row[('H' if upper else 'h') + str(i)]) for i in range(1, legs + 1)))


def _mapping(rows, legs, expected, upper=True):
    result = {}
    for row in rows:
        key = _key(row, legs, upper)
        _require(key not in result)
        result[key] = row
    _require(result.keys() == expected)
    return result


def _replay(report, folder, raw, manifest, family):
    """Recompute coverage, input bits, native controls, and every acceptance."""
    count = report['Points']; legs = len(manifest['Masses'])
    _require(_integer(count, 1) and legs >= 4 and report['InputStorageBits'] == 64
             and report['CorePrecisionBits'] == 113 and report['CoreStorageBits'] == 128
             and report['InputKinematicsCorrected'] is False and report['OtherAbsoluteTolerance'] == 0)
    seed = _seed(report['Seed']); _require(_seed(raw['Seed']) == seed)
    control = _object(_inside(folder, report['NativeDoubleControlFile']))
    _require(control.get('HistoricalAllPointNativeValuesWereSaved') is False
             and control.get('NoHistoricalAllPointBitIdentityClaim') is True
             and _seed(control.get('Seed')) == seed
             and control.get('Points') == count
             and report.get('OriginalKnownFailedPointAndNativeBits') == control)
    raw_rows = raw['Rows']; _require(isinstance(raw_rows, list) and raw_rows)
    states = {}; failed = {}
    for row in raw_rows:
        state = _state(row['Helicities'], legs); _require(state not in states)
        _require(type(row.get('ExpectedExactZero')) is bool)
        _require(all(row.get(k) == count for k in ('Points', 'RequestedPoints', 'GeneratedPoints', 'ComparedPoints'))
                 and row.get('EvaluationErrors') == 0)
        states[state] = row['ExpectedExactZero']
        if row['ExpectedExactZero']:
            _require(_number(row['ZeroAbsoluteTolerance']) == Decimal(str(1e-22)))
        if row.get('Status') == 'FAIL':
            f = row['FirstFailure']; key = (f['Point'], state)
            _require(_integer(f['Point'], 1) and f['Point'] <= count and row['Mismatches'] >= 1)
            failed[key] = f
        else:
            _require(row.get('Status') == 'PASS' and row.get('Mismatches') == 0)
    configured = {_state(v, legs) for v in manifest['HelicityConfigurations']}
    _require(len(configured) == len(manifest['HelicityConfigurations'])
             and configured == states.keys() and len(states) == family['LabeledHelicityAssignmentCount']
             and report['HelicityRows'] == len(states) and report['ComparedHelicityPoints'] == count * len(states)
             and control.get('Rows') == count * len(states)
             and control.get('HelicitiesPerPoint') == len(states)
             and failed and raw.get('Status') == 'FAIL')
    expected = {(point, state) for point in range(1, count + 1) for state in states}
    binary = _inside(folder, report['Binary64PointsFile']).read_bytes()
    _require(len(binary) == count * 4 * legs * 8)
    points = _csv(_inside(folder, report['PointsFile'])); _require(len(points) == count)
    point_bytes = {}
    for row in points:
        point = int(row['Point']); _require(1 <= point <= count and point not in point_bytes)
        data = b''.join(_bits(row[f'p{i}_{j}']) for i in range(1, legs + 1) for j in range(4))
        _require(data == binary[(point - 1) * 32 * legs:point * 32 * legs])
        point_bytes[point] = data
    native_rows = _mapping(_csv(_inside(folder, report['NativeValuesFile'])), legs, expected, False)
    native = {key: _float(row['normalized']) for key, row in native_rows.items()}
    _require(all(value == 0.0 for key,value in native.items() if states[key[1]]))
    # Only first-failure values were archived originally. Never infer identity
    # for historical values that were not saved.
    for (point, state), f in failed.items():
        _require(len(f['Momenta']) == legs and all(len(v) == 4 for v in f['Momenta']))
        _require(b''.join(_bits(v) for line in f['Momenta'] for v in line) == point_bytes[point])
        _require(_bits(f['HEPCATValue']) == _bits(native[(point, state)]))
    saved = _object(_inside(folder, report.get('KnownFailedPointFile', report.get('Point719File'))))
    _require(_seed(saved['Seed']) == seed and _integer(saved['Point'], 1))
    _require(b''.join(_bits(v) for line in saved['Momenta'] for v in line) == point_bytes[saved['Point']])
    _require(len(saved['FailedRows']) == len(failed)
             and {(saved['Point'], _state(v['Helicities'], legs)) for v in saved['FailedRows']} == failed.keys())
    for v in saved['FailedRows']:
        _require(v['FirstFailure'] == failed[(saved['Point'], tuple(v['Helicities']))])
    single = _csv(_inside(folder, 'native-53.csv')); _require(len(single) == len(states))
    single = {_state([int(row[f'h{i}']) for i in range(1, legs + 1)], legs): row for row in single}
    _require(single.keys() == states.keys())
    _require(all(_bits(row['normalized']) == _bits(native[(saved['Point'], state)]) for state, row in single.items()))
    relative_tol = Decimal.from_float(1e-7); zero_tol = Decimal.from_float(1e-22)
    _require(_number(report['RelativeTolerance']) == relative_tol
             and _number(report['GeneralExactZeroAbsoluteTolerance']) == zero_tol)
    norm = _float(float.fromhex(report['NormalizationBinary64Hex']))
    # The normalization is fixed by the original project, not fitted to data.
    expected_norm = float(manifest['SymmetryFactor']) / (manifest['InitialSpinAverage'] * manifest['InitialColorAverage'])
    _require(_bits(norm) == _bits(expected_norm) and norm > 0)
    double_rows = _mapping(_csv(_inside(folder, report['Reference53ValuesFile'])), legs, expected)
    double_failures = set()
    for key, row in double_rows.items():
        value = native[key]; reference = _float(row['MadGraph53Raw']) * norm
        error = abs(reference - value); scale = max(abs(reference), abs(value)); relative = error / scale if scale else 0.
        _require(_bits(row['Native53Normalized']) == _bits(value)
                 and _bits(row['MadGraph53Normalized']) == _bits(reference)
                 and _bits(row['AbsoluteError']) == _bits(error) and _bits(row['RelativeError']) == _bits(relative))
        passed = error <= (1e-22 if states[key[1]] else 1e-7 * scale)
        _require(int(row['Passed']) == int(passed))
        if not passed: double_failures.add(key)
    _require(double_failures == failed.keys())
    quad = {}
    with gzip.open(_inside(folder, report['Reference113ValuesFile']), 'rt') as stream:
        header = next(stream)
        _require(re.fullmatch(r'\s*CORE_PRECISION_BITS\s+113\s+CORE_STORAGE_BITS\s+128\s+INPUT_STORAGE_BITS\s+64\s*', header))
        for line in stream:
            fields = line.split(); _require(len(fields) == legs + 2)
            key = (int(fields[0]), tuple(map(int, fields[1:legs + 1]))); _require(key not in quad)
            quad[key] = _number(fields[-1])
    _require(quad.keys() == expected)
    comparisons = _mapping(_csv(_inside(folder, report['IndependentComparisonFile'])), legs, expected)
    maxima = {s: [Decimal(0), Decimal(0)] for s in states}; maximum_nonzero = Decimal(0); maximum_zero = Decimal(0)
    for key, row in comparisons.items():
        value = Decimal.from_float(native[key]); reference_raw = quad[key]
        _require(_number(row['Native53Normalized']) == value and _number(row['MadGraph113Raw']) == reference_raw)
        with localcontext() as context:
            context.prec = 80
            reference = reference_raw * Decimal.from_float(norm); error = abs(reference - value)
            scale = max(abs(reference), abs(value)); relative = error / scale if scale else Decimal(0)
            _require(_number(row['MadGraph113Normalized']) == reference and _number(row['AbsoluteError']) == error
                     and _number(row['RelativeError']) == relative)
            maxima[key[1]][0] = max(maxima[key[1]][0], error); maxima[key[1]][1] = max(maxima[key[1]][1], relative)
        with localcontext() as context:
            context.prec = 90
            exact_reference = reference_raw * Decimal.from_float(norm); exact_error = abs(exact_reference - value)
            scale = max(abs(exact_reference), abs(value))
            accepted = exact_error <= (zero_tol if states[key[1]] else relative_tol * scale)
            _require(accepted and int(row['Passed']) == int(accepted))
            if states[key[1]]: maximum_zero = max(maximum_zero, exact_error)
            elif scale: maximum_nonzero = max(maximum_nonzero, exact_error / scale)
    rows = report['Rows']; _require(len(rows) == len(states))
    seen = set()
    for row in rows:
        state = _state(row['Helicities'], legs); _require(state in states and state not in seen); seen.add(state)
        _require(row['Status'] == 'PASS' and row['ComparedPoints'] == count and row['Mismatches'] == 0
                 and row['EvaluationErrors'] == 0 and row['ExpectedExactZero'] is states[state]
                 and _number(row['MaximumAbsoluteError']) == maxima[state][0]
                 and _number(row['MaximumRelativeError']) == maxima[state][1])
    return {'Points': count, 'HelicityRows': len(states), 'ComparedHelicityPoints': len(expected),
            'MaximumNonzeroRelativeError': str(maximum_nonzero), 'MaximumZeroAbsoluteError': str(maximum_zero),
            'OriginalFailedHelicityPointsBitMatched': len(failed), 'OriginalFirstFailureMomentaBitMatched': True,
            'HistoricalAllPointNativeIdentityClaim': False, 'Seed': seed}


def _qualification_from_link(link, family, export_summary, cases, context_sha, results_dir,
                             export_root, required_points=1000, context_compatibility=None):
    """Verify a prospective binding without writing or admitting a campaign record."""
    try:
        _require(_integer(required_points, 1) and isinstance(family, dict) and isinstance(cases, dict))
        identifier = family['ID']; _require(re.fullmatch(r'[A-Za-z_][A-Za-z_0-9]*', identifier))
        results = Path(results_dir).resolve(); _require(isinstance(link, dict))
        raw_context = export_summary['ContextSHA256']
        _require(link['Backend'] == 'MadGraph' and link['FamilyID'] == identifier and link['ContextSHA256'] == raw_context
                 and export_summary['FamilyID'] == identifier and export_summary['Status'] == 'FAIL'
                 and export_summary['CalcHEPStatus'] == 'PASS' and export_summary['MadGraphStatus'] == 'FAIL'
                 and _integer(export_summary['Points'], required_points)
                 and constructor_context.admits_record(results / (identifier + '-export.json'), export_summary,
                    context_sha, context_compatibility))
        _require(_object(results / (identifier + '-export.json')) == {k:v for k,v in export_summary.items() if k not in ('PrecisionQualification',)})
        _require(_matches(results / (identifier + '-export.wxf'), link['ExportCapsuleSHA256'])
                 and export_summary['CapsuleSHA256'] == link['ExportCapsuleSHA256'])
        members = family['CaseIDs']; _require(isinstance(members, list) and members and len(set(members)) == len(members)
                 and all(isinstance(v, str) and re.fullmatch(r'[A-Za-z_][A-Za-z_0-9]*', v) for v in members)
                 and export_summary['CaseIDs'] == members)
        hashes = {}; contexts = {}
        for member in members:
            case = cases[member]
            _require(case['ID'] == member and case['Status'] == 'COMPLETE' and _integer(case['Rows'], 1)
                     and _integer(case['FeynmanRows'], 1) and case['FeynmanRows'] <= case['Rows']
                     and case['CheckStatuses'] == {'PASS':case['Rows']}
                     and constructor_context.admits_record(results / (member + '.json'), case, context_sha, context_compatibility)
                     and _matches(results / (member + '.wxf'), case['CapsuleSHA256']))
            saved_case = _object(results / (member + '.json'))
            _require(all(case.get(key) == value for key,value in saved_case.items())
                     and case.get('SupplementalParameterProbes', {}).get('Status', 'PASS') == 'PASS')
            hashes[member] = case['CapsuleSHA256']; contexts[member] = case['ContextSHA256']
        _require(all(value == hashes for value in (link['CaseCapsulesSHA256'], export_summary['CaseCapsulesSHA256']))
                 and link['NativeCaseContexts'] == contexts and export_summary['NativeCaseContexts'] == contexts
                 and export_summary['ExporterContextSHA256'] == raw_context)
        report_path = _inside(results, link['Report']); _require(_matches(report_path, link['ReportSHA256']))
        report = _object(report_path); folder = report_path.parent
        _require(report['Backend'] == 'MadGraph' and report['Status'] == 'PRECISION_REPLAY_PASS'
                 and report['ContextSHA256'] == raw_context and report['FamilyID'] == identifier
                 and report['ExportCapsuleSHA256'] == link['ExportCapsuleSHA256'] and report['CaseCapsulesSHA256'] == hashes
                 and report['NativeCaseContexts'] == contexts and report['Points'] == export_summary['Points']
                 and report['RawCalcHEPStatus'] == 'PASS' and report['RawMadGraphStatus'] == 'FAIL'
                 and report['RawOriginalExportStatusUnchanged'] == 'FAIL' and report['QualificationOrCanonicalPromotionPerformed'] is False
                 and report['AllGeneratedFortranSourceAndCardFilesIdentical53And113'] is True
                 and type(report['Mismatches']) is int and report['Mismatches'] == 0
                 and type(report['EvaluationErrors']) is int and report['EvaluationErrors'] == 0)
        files = report['Files']; _require(isinstance(files, dict) and files)
        _require(all(_matches(_inside(folder, name), digest) for name,digest in files.items()))
        for field in ('PointsFile','Binary64PointsFile','NativeValuesFile','Reference53ValuesFile','Reference113ValuesFile',
                      'IndependentComparisonFile','RawReportFile','SourceRuntimeManifestFile',
                      'ProjectHashesFile','GeneratedSourceHashesFile','NativeDoubleControlFile'):
            _require(report[field] in files)
        _require(report.get('KnownFailedPointFile', report.get('Point719File')) in files)
        review_path = _inside(results, link['SemanticReview']); _require(_matches(review_path, link['SemanticReviewSHA256']))
        review = _object(review_path)
        _require(review['Status'] == 'REVIEWED_FULL_SAME_INPUT_MADGRAPH_PRECISION_REPLAY'
                 and review['ReportSHA256'] == link['ReportSHA256'] and review['ContextSHA256'] == raw_context
                 and review['ExportCapsuleSHA256'] == link['ExportCapsuleSHA256']
                 and review['CompilerSHA256'] == report['CompilerSHA256']
                 and all(review.get(key) is True for key in REVIEW_ASSERTIONS)
                 and isinstance(review.get('ReviewedBy'), str) and review['ReviewedBy']
                 and isinstance(review.get('ReviewedUTC'), str) and review['ReviewedUTC']
                 and isinstance(review.get('EvidenceFiles'), dict)
                 and {'quad-stream.f90','quad-same-source-build.json','build-same-source-quad.py',
                      report['GeneratedSourceHashesFile'], report['ProjectHashesFile'], report['RawReportFile']}.issubset(review['EvidenceFiles'])
                 and all(files.get(name) == digest for name, digest in review['EvidenceFiles'].items()))
        compiler = Path(report['CompilerPath']); _require(compiler.is_absolute() and _matches(compiler, report['CompilerSHA256']))
        runtime = _object(_inside(folder, report['SourceRuntimeManifestFile']))
        configuration = _object(results / 'configuration.json')
        actual_runtime = stage_context.manifest(ROOT, HERE, configuration)
        _require(stage_context.fingerprint(runtime) == raw_context
                 and stage_context.fingerprint(actual_runtime) == context_sha)
        if raw_context == context_sha:
            _require(runtime == actual_runtime)
        else:
            # The exact historical export and native capsules were admitted
            # above by the existing finite reviewed context bridge. That is
            # permission to retain their actual old validation, not a rerun.
            # Models, installations and runtime libraries must still agree;
            # only reviewed package-source differences can cross this path.
            old_files, new_files = runtime['Files'], actual_runtime['Files']
            _require(runtime['Programs'] == actual_runtime['Programs']
                     and all(old_files.get(name) == new_files.get(name)
                             for name in old_files.keys() | new_files.keys()
                             if not name.startswith('source/')))
        export = Path(export_root).resolve()
        if not (export / 'manifest.json').is_file(): export = _inside(export, family['ExportName'])
        project = _object(_inside(folder, report['ProjectHashesFile']))
        _require({'manifest.json','process.cpp','process.hpp','madgraph-validation.cpp','madgraph-runtime.py',
                  'madgraph-runtime.hpp','spinas-runtime.hpp','spinas-validation.hpp'}.issubset(project)
                 and all(_matches(_inside(export, name), digest) for name,digest in project.items()))
        manifest = _object(export / 'manifest.json'); _require(len(manifest['Masses']) == len(family['Particles']))
        native_library = _inside(export, 'build/lib' + manifest['Class'] + '_process.a')
        _require(native_library.resolve() == Path(report['NativeGeneratedLibraryPath']).resolve()
                 and _matches(native_library, report['NativeGeneratedLibrarySHA256'])
                 and project.get(str(native_library.relative_to(export))) == report['NativeGeneratedLibrarySHA256'])
        spin_library = Path(report['SPINASLibraryPath']); _require(spin_library.is_absolute() and _matches(spin_library, report['SPINASLibrarySHA256']))
        command_path = _inside(export, review['LinkCommandPath'])
        _require(command_path == _inside(export, 'build/CMakeFiles/' + manifest['Class'] + '_validate_madgraph.dir/link.txt')
                 and _matches(command_path, review['LinkCommandSHA256']))
        command = shlex.split(command_path.read_text())
        linked = {(Path(v) if Path(v).is_absolute() else export / 'build' / v).resolve()
                  for v in command if Path(v).suffix in ('.a','.so','.dylib')}
        library_hashes = review['LinkedLibrariesSHA256']
        _require(isinstance(library_hashes, dict) and library_hashes
                 and all(Path(name).is_absolute() and '..' not in Path(name).parts for name in library_hashes)
                 and len({Path(name).resolve() for name in library_hashes}) == len(library_hashes)
                 and {Path(name).resolve() for name in library_hashes} == linked
                 and all(_matches(Path(name), digest) for name,digest in library_hashes.items())
                 and native_library.resolve() in linked and spin_library.resolve() in linked)
        generated = _object(_inside(folder, report['GeneratedSourceHashesFile']))
        _require(len(generated) == report['GeneratedSourceFileCount']
                 and generated == {name[len('generated-source/'):]:digest for name,digest in files.items() if name.startswith('generated-source/')})
        raw = _object(_inside(folder, report['RawReportFile'])); _require(files[report['RawReportFile']] == report['RawReportSHA256'])
        _require('raw-export-summary.json' in files and 'native-53.csv' in files)
        original = _object(_inside(folder, 'raw-export-summary.json')); _require(original == _object(results / (identifier + '-export.json')))
        replay = _replay(report, folder, raw, manifest, family)
        result = {'FamilyID':identifier,'Backend':'MadGraph','Status':'PRECISION_QUALIFIED','ContextSHA256':raw_context,
                  'CaseIDs':members,'CaseCapsulesSHA256':hashes,'NativeCaseContexts':contexts,
                  'ExportCapsuleSHA256':link['ExportCapsuleSHA256'],'Report':link['Report'],'ReportSHA256':link['ReportSHA256'],
                  'SemanticReview':link['SemanticReview'],'SemanticReviewSHA256':link['SemanticReviewSHA256'],
                  'MantissaBits':113,'REALBytes':16,'RawCalcHEPStatus':'PASS','RawMadGraphStatus':'FAIL',
                  'RawExportStatus':'FAIL','RelativeTolerance':1e-7,'ExactZeroAbsoluteTolerance':1e-22,**replay}
        if raw_context != context_sha:
            result['ActiveExporterContextSHA256'] = context_sha
            result['ContextCompatibility'] = constructor_context.public_proof(context_compatibility, 'PrecisionEvidence')
        return result
    except (OSError, ValueError, TypeError, KeyError, AttributeError, OverflowError,
            InvalidOperation, struct.error, csv.Error, StopIteration, EOFError, UnicodeError):
        return None


def qualification(family, export_summary, cases, context_sha, results_dir,
                  export_root, required_points=1000, context_compatibility=None):
    """Verify only an explicitly saved binding; never rewrite raw statuses."""
    try:
        link = _object(Path(results_dir) / (family['ID'] + '-precision.json'))
        return _qualification_from_link(link, family, export_summary, cases, context_sha,
            results_dir, export_root, required_points, context_compatibility)
    except (OSError, ValueError, TypeError, KeyError):
        return None
