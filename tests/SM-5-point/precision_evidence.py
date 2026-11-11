"""Verify a separate CalcHEP precision qualification without rewriting raw results.

The optional link binds a replay to the current construction capsules, export,
reference libraries and archived numerical inputs. Missing or stale evidence is
not a qualification. This module performs no amplitude calculation or writes.
"""
# HEPCAT - High Energy Physics Constructive Amplitude Tool
# Copyright (C) 2026 Neil Christensen
# SPDX-License-Identifier: GPL-3.0-or-later
from __future__ import annotations
import csv
import hashlib
import json
import math
from pathlib import Path
import re
import shlex


def _digest(path):
    checksum = hashlib.sha256()
    with path.open('rb') as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b''):
            checksum.update(block)
    return checksum.hexdigest()


def _json(path):
    value = json.loads(path.read_text())
    if not isinstance(value, dict):
        raise ValueError('Expected a JSON object.')
    return value


def _identifier(value):
    return isinstance(value, str) and re.fullmatch(r'[A-Za-z_][A-Za-z_0-9]*', value) is not None


def _inside(base, relative):
    if not isinstance(relative, str):
        raise ValueError('Expected a relative artifact path.')
    path = Path(relative)
    if path.is_absolute() or '..' in path.parts or not path.parts:
        raise ValueError('Artifact paths cannot traverse outside their evidence directory.')
    result = (base / path).resolve()
    result.relative_to(base.resolve())
    return result


def _matches(path, expected):
    return (isinstance(expected, str) and re.fullmatch('[0-9a-f]{64}', expected) is not None
            and path.is_file() and _digest(path) == expected)


def _positive_integer(value):
    return isinstance(value, int) and not isinstance(value, bool) and value > 0


def _zero_count(value):
    return isinstance(value, int) and not isinstance(value, bool) and value == 0


def _finite_nonnegative(value):
    return (isinstance(value, (int, float)) and not isinstance(value, bool)
            and math.isfinite(value) and value >= 0)


def _linked_libraries(export, class_name):
    command_file = export / 'build/CMakeFiles' / (class_name + '_validate_calchep.dir/link.txt')
    command = shlex.split(command_file.read_text())
    return {(Path(value) if Path(value).is_absolute() else export / 'build' / value).resolve()
            for value in command if Path(value).suffix in ('.a', '.so', '.dylib')}


def _points_match(path, count, legs):
    seen = set()
    columns = [f'p{leg}_{component}' for leg in range(1, legs + 1) for component in range(4)]
    with path.open(newline='') as stream:
        rows = csv.DictReader(stream)
        if not {'point', 'hepcat', 'calchep', *columns}.issubset(rows.fieldnames or []):
            return False
        for row in rows:
            point = int(row['point'])
            if point <= 0 or point in seen:
                return False
            seen.add(point)
            if not all(math.isfinite(float(row[key])) for key in ['hepcat', 'calchep', *columns]):
                return False
    return len(seen) == count


def qualification(family, export_summary, cases, context_sha, results_dir,
                  export_root, required_points=1000):
    """Return verified precision evidence, or None. Raw statuses are untouched.

    Report paths are relative to results_dir. export_root is the common export
    directory containing family['ExportName'], or that specific export itself.
    """
    try:
        if (not _positive_integer(required_points) or not isinstance(family, dict)
                or not isinstance(context_sha, str) or re.fullmatch('[0-9a-f]{64}', context_sha) is None):
            return None
        identifier = family.get('ID')
        if not _identifier(identifier):
            return None
        results = Path(results_dir).resolve()
        link = _json(results / (identifier + '-precision.json'))
        if link.get('FamilyID') != identifier or link.get('ContextSHA256') != context_sha:
            return None
        if (not isinstance(export_summary, dict)
                or export_summary.get('FamilyID') != identifier
                or export_summary.get('ContextSHA256') != context_sha
                or export_summary.get('MadGraphStatus') != 'PASS'
                or export_summary.get('Status') not in ('PASS', 'FAIL')
                or not _positive_integer(export_summary.get('Points'))
                or export_summary['Points'] < required_points):
            return None
        export_capsule = results / (identifier + '-export.wxf')
        export_hash = link.get('ExportCapsuleSHA256')
        if export_summary.get('CapsuleSHA256') != export_hash or not _matches(export_capsule, export_hash):
            return None
        members = family.get('CaseIDs')
        if (not isinstance(members, list) or not members or len(set(members)) != len(members)
                or not all(_identifier(member) for member in members)
                or export_summary.get('CaseIDs') != members):
            return None
        member_hashes = {}
        for member in members:
            case = cases.get(member, {})
            if (case.get('ID') != member or case.get('Status') != 'COMPLETE'
                    or case.get('ContextSHA256') != context_sha
                    or not _positive_integer(case.get('Rows'))
                    or not _positive_integer(case.get('FeynmanRows'))
                    or case['FeynmanRows'] > case['Rows']
                    or case.get('CheckStatuses') != {'PASS': case['Rows']}
                    or not _matches(results / (member + '.wxf'), case.get('CapsuleSHA256'))):
                return None
            member_hashes[member] = case['CapsuleSHA256']
        if (link.get('CaseCapsulesSHA256') != member_hashes
                or export_summary.get('CaseCapsulesSHA256') != member_hashes):
            return None
        report_path = _inside(results, link.get('Report'))
        if not _matches(report_path, link.get('ReportSHA256')):
            return None
        report = _json(report_path)
        if type(report.get('SchemaVersion')) is not int or report['SchemaVersion'] != 1 or report.get('Status') != 'PRECISION_REPLAY_PASS':
            return None
        count = report.get('Points')
        tolerance = report.get('RelativeTolerance')
        if (not _positive_integer(count) or count < required_points
                or not _finite_nonnegative(tolerance) or tolerance > 1e-7
                or report.get('NativeControlReproducesCapture') is not True):
            return None
        native = report.get('Results', {}).get('Native', {})
        extended = report.get('Results', {}).get('GCC128', {})
        if (native.get('Points') != count or native.get('OriginalBitMatches') != count
                or not _zero_count(native.get('Errors')) or not _positive_integer(native.get('MantissaBits'))
                or extended.get('Status') != 'PASS' or extended.get('Points') != count
                or extended.get('MantissaBits') != 113 or extended.get('REALBytes') != 16
                or not _zero_count(extended.get('Mismatches')) or not _zero_count(extended.get('Errors'))
                or not _finite_nonnegative(extended.get('MaximumRelativeError'))
                or not _finite_nonnegative(extended.get('MaximumAbsoluteError'))
                or extended['MaximumRelativeError'] > tolerance):
            return None
        export = Path(export_root).resolve()
        if not (export / 'manifest.json').is_file():
            name = family.get('ExportName')
            if not _identifier(name):
                return None
            export = _inside(export, name)
        manifest = _json(export / 'manifest.json')
        if not _matches(export / 'manifest.json', report.get('ManifestSHA256')):
            return None
        legs = len(manifest.get('Masses', []))
        if legs < 4 or report.get('ExternalCount') != legs or not _identifier(manifest.get('Class')):
            return None
        sources = report.get('ExportSourcesSHA256')
        if not isinstance(sources, dict) or not {'manifest.json', 'process.cpp', 'process.hpp'}.issubset(sources):
            return None
        if not all(_matches(_inside(export, name), digest) for name, digest in sources.items()):
            return None
        libraries = report.get('LinkedLibrariesSHA256')
        if not isinstance(libraries, dict) or not libraries:
            return None
        expected_libraries = _linked_libraries(export, manifest['Class'])
        actual_libraries = set()
        for name, digest in libraries.items():
            path = Path(name)
            if not path.is_absolute() or '..' in path.parts:
                return None
            path = path.resolve()
            if not _matches(path, digest):
                return None
            actual_libraries.add(path)
        if actual_libraries != expected_libraries:
            return None
        for field, filename in (('PointsSHA256', 'points.csv'), ('ParametersSHA256', 'parameters.csv'),
                                ('ToolSHA256', 'replay-tool.py')):
            if field in report and not _matches(_inside(report_path.parent, filename), report[field]):
                return None
        if 'PointsSHA256' in report and not _points_match(report_path.parent / 'points.csv', count, legs):
            return None
        return {'FamilyID': identifier, 'Status': 'PRECISION_QUALIFIED', 'ContextSHA256': context_sha,
                'CaseIDs': list(members), 'CaseCapsulesSHA256': member_hashes,
                'ExportCapsuleSHA256': export_hash, 'Report': link['Report'],
                'ReportSHA256': link['ReportSHA256'], 'Points': count,
                'MantissaBits': 113, 'REALBytes': 16, 'RelativeTolerance': tolerance,
                'MaximumRelativeError': extended['MaximumRelativeError'],
                'MaximumAbsoluteError': extended['MaximumAbsoluteError'],
                'RawCalcHEPStatus': export_summary.get('CalcHEPStatus', 'NOT RUN'),
                'MadGraphStatus': 'PASS'}
    except (OSError, ValueError, TypeError, KeyError, AttributeError, csv.Error):
        return None


def _current_context(configuration):
    import importlib.util
    specification = importlib.util.spec_from_file_location(
        'hepcat_sm5_precision_context', Path(__file__).with_name('run.py'))
    workflow = importlib.util.module_from_spec(specification)
    specification.loader.exec_module(workflow)
    return workflow.context_sha256(configuration)


def main():
    import argparse
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--family', required=True)
    parser.add_argument('--results', required=True, type=Path)
    parser.add_argument('--export-root', required=True, type=Path)
    parser.add_argument('--inventory', type=Path, default=Path(__file__).with_name('inventory.json'))
    parser.add_argument('--required-points', type=int, default=1000)
    args = parser.parse_args()
    verified = None
    try:
        configuration = _json(args.results / 'configuration.json')
        inventory = _json(args.inventory)
        family = next(row for row in inventory['Families'] if row.get('ID') == args.family)
        export = _json(args.results / (args.family + '-export.json'))
        cases = {member: _json(args.results / (member + '.json')) for member in family['CaseIDs']}
        verified = qualification(family, export, cases, _current_context(configuration),
                                 args.results, args.export_root, args.required_points)
    except (OSError, ValueError, TypeError, KeyError, StopIteration):
        pass
    print(json.dumps(verified or {'FamilyID': args.family, 'Status': 'UNQUALIFIED'}, indent=2))
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
