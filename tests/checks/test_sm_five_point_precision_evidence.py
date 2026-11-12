"""Strict, synthetic precision-qualification metadata tests. No physics runs."""
# HEPCAT - High Energy Physics Constructive Amplitude Tool
# Copyright (C) 2026 Neil Christensen
# SPDX-License-Identifier: GPL-3.0-or-later
from contextlib import redirect_stdout
import copy
import csv
import hashlib
import importlib.util
import io
import json
from pathlib import Path
import sys
import tempfile
import unittest
from unittest import mock

SCRIPT = Path(__file__).resolve().parents[1] / 'SM-5-point/precision_evidence.py'
SPEC = importlib.util.spec_from_file_location('sm5_precision_evidence', SCRIPT)
EVIDENCE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(EVIDENCE)


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def write(path, value):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value))


class PrecisionQualificationTests(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory(prefix='hepcat-precision-evidence-')
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)
        self.results = self.root / 'results'
        self.exports = self.root / 'exports'
        self.export = self.exports / 'Demo'
        self.audit = self.results / 'precision/Demo'
        self.export.mkdir(parents=True)
        self.audit.mkdir(parents=True)
        self.context = 'a' * 64
        self.family = {'ID': 'family_demo', 'CaseIDs': ['case_a', 'case_b'], 'ExportName': 'Demo'}
        self.cases = {}
        for member in self.family['CaseIDs']:
            capsule = self.results / (member + '.wxf')
            capsule.write_bytes(('synthetic capsule ' + member).encode())
            self.cases[member] = {'ID': member, 'Status': 'COMPLETE', 'ContextSHA256': self.context,
                'Rows': 3, 'FeynmanRows': 1, 'CheckStatuses': {'PASS': 3}, 'CapsuleSHA256': digest(capsule)}
            write(self.results / (member + '.json'), self.cases[member])
        raw = self.results / 'family_demo-export.wxf'
        raw.write_bytes(b'synthetic raw export retaining CalcHEP FAIL and MadGraph PASS')
        case_hashes = {member: case['CapsuleSHA256'] for member, case in self.cases.items()}
        self.export_summary = {'FamilyID': 'family_demo', 'ContextSHA256': self.context,
            'Status': 'FAIL', 'CalcHEPStatus': 'FAIL', 'MadGraphStatus': 'PASS', 'Points': 1000,
            'CaseIDs': list(self.family['CaseIDs']), 'CaseCapsulesSHA256': case_hashes,
            'CapsuleSHA256': digest(raw)}
        write(self.results / 'family_demo-export.json', self.export_summary)
        manifest = {'Class': 'Demo', 'Masses': [1, 1, 0, 0]}
        write(self.export / 'manifest.json', manifest)
        (self.export / 'process.cpp').write_text('// synthetic exported amplitude source')
        (self.export / 'process.hpp').write_text('// synthetic header')
        process_library = self.export / 'build/libDemo_process.a'
        process_library.parent.mkdir()
        process_library.write_bytes(b'synthetic compiled amplitude')
        reference_library = self.root / 'runtime/libspinas.a'
        reference_library.parent.mkdir()
        reference_library.write_bytes(b'synthetic SPINAS library')
        link = self.export / 'build/CMakeFiles/Demo_validate_calchep.dir/link.txt'
        link.parent.mkdir(parents=True)
        link.write_text(f'c++ main.o -o validate libDemo_process.a {reference_library}')
        with (self.audit / 'points.csv').open('w', newline='') as stream:
            columns = ['point', 'hepcat', 'calchep'] + [f'p{i}_{j}' for i in range(1, 5) for j in range(4)]
            writer = csv.writer(stream)
            writer.writerow(columns)
            for point in range(1, 1001):
                writer.writerow([point, 1., 1.0000002, *([0.] * 16)])
        (self.audit / 'parameters.csv').write_text('name,hex_value\ng,0x1p0\n')
        (self.audit / 'replay-tool.py').write_text('# synthetic archived replay method')
        self.report = {'SchemaVersion': 1, 'Status': 'PRECISION_REPLAY_PASS', 'Points': 1000,
            'ExternalCount': 4, 'RelativeTolerance': 1e-7, 'NativeControlReproducesCapture': True,
            'Seed': 2**63 + 123,
            'Results': {'Native': {'MantissaBits': 53, 'REALBytes': 8, 'Points': 1000,
                'OriginalBitMatches': 1000, 'Mismatches': 29, 'Errors': 0, 'Status': 'FAIL'},
                'GCC128': {'MantissaBits': 113, 'REALBytes': 16, 'Points': 1000,
                    'Mismatches': 0, 'Errors': 0, 'Status': 'PASS',
                    'MaximumRelativeError': 1e-10, 'MaximumAbsoluteError': 1e-16}},
            'ManifestSHA256': digest(self.export / 'manifest.json'),
            'ExportSourcesSHA256': {name: digest(self.export / name)
                                   for name in ('manifest.json', 'process.cpp', 'process.hpp')},
            'LinkedLibrariesSHA256': {str(path): digest(path) for path in (process_library, reference_library)},
            'PointsSHA256': digest(self.audit / 'points.csv'),
            'ParametersSHA256': digest(self.audit / 'parameters.csv'),
            'ToolSHA256': digest(self.audit / 'replay-tool.py')}
        self.link = {'FamilyID': 'family_demo', 'ContextSHA256': self.context,
            'ExportCapsuleSHA256': digest(raw), 'CaseCapsulesSHA256': dict(case_hashes),
            'Report': 'precision/Demo/precision-summary.json'}
        self.raw_path = self.results / 'diagnostics/current-raw-failure.json'
        self.control_path = self.audit / 'current-failure-control.json'
        seed = self.report['Seed']
        self.raw_failure = {'FamilyID': self.family['ID'], 'ContextSHA256': self.context,
            'ExportCapsuleSHA256': self.export_summary['CapsuleSHA256'], 'Seed': seed,
            'CommandResult': {'ExitCode': 1, 'StandardOutput':
                f'Amplitude validation: FAIL\nSeed: {seed}\n'
                'Points requested/generated/compared: 1000/1000/1000\n'
                'Mismatches/errors: 29/0\nFirst failing point: 3\n' +
                ''.join(f'  p{leg} = 0 0 0 0\n' for leg in range(1, 5))}}
        self.control = {'Seed': seed, 'CurrentFirstFailedPoint': 3,
            'MomentumComponents': 16, 'MomentumComponentsBitMatched': 16,
            'AllSavedMomentumComponentsBitMatched': True,
            'ExportCapsuleSHA256': self.export_summary['CapsuleSHA256'],
            'RawFailureReport': 'diagnostics/current-raw-failure.json'}
        self.seal_failure()
        self.seal()

    def seal_failure(self):
        write(self.raw_path, self.raw_failure)
        self.control['RawFailureReportSHA256'] = digest(self.raw_path)
        write(self.control_path, self.control)
        self.link['CurrentFailureControl'] = 'precision/Demo/current-failure-control.json'
        self.link['CurrentFailureControlSHA256'] = digest(self.control_path)

    def seal(self):
        write(self.audit / 'precision-summary.json', self.report)
        self.link['ReportSHA256'] = digest(self.audit / 'precision-summary.json')
        write(self.results / 'family_demo-precision.json', self.link)

    def qualify(self, **changes):
        options = {'family': self.family, 'export_summary': self.export_summary, 'cases': self.cases,
                   'context_sha': self.context, 'results_dir': self.results, 'export_root': self.exports,
                   'required_points': 1000}
        options.update(changes)
        return EVIDENCE.qualification(**options)

    def test_valid_precision_evidence_is_separate_from_raw_failures(self):
        before = copy.deepcopy(self.export_summary)
        verified = self.qualify()
        self.assertEqual(verified['Status'], 'PRECISION_QUALIFIED')
        self.assertEqual(verified['RawCalcHEPStatus'], 'FAIL')
        self.assertEqual(verified['MadGraphStatus'], 'PASS')
        self.assertEqual(self.export_summary, before)

    def test_specific_export_directory_is_also_supported(self):
        self.assertIsNotNone(self.qualify(export_root=self.export))

    def test_missing_link_and_malformed_link_are_unqualified(self):
        path = self.results / 'family_demo-precision.json'
        path.unlink()
        self.assertIsNone(self.qualify())
        path.write_text('{broken')
        self.assertIsNone(self.qualify())

    def test_context_must_bind_every_layer(self):
        self.assertIsNone(self.qualify(context_sha='b' * 64))
        self.assertIsNone(self.qualify(context_sha=None))
        self.link['ContextSHA256'] = 'b' * 64
        self.seal()
        self.assertIsNone(self.qualify())

    def test_wrong_link_identity_is_rejected(self):
        self.link['FamilyID'] = 'other_family'
        self.seal()
        self.assertIsNone(self.qualify())

    def test_report_hash_and_capsule_hashes_are_verified(self):
        (self.audit / 'precision-summary.json').write_text('{}')
        self.assertIsNone(self.qualify())
        self.seal()
        (self.results / 'family_demo-export.wxf').write_bytes(b'new export')
        self.assertIsNone(self.qualify())

    def test_new_case_capsule_invalidates_old_precision_link(self):
        path = self.results / 'case_a.wxf'
        path.write_bytes(b'new independently complete expression')
        self.cases['case_a']['CapsuleSHA256'] = digest(path)
        self.assertIsNone(self.qualify())

    def test_all_independent_members_must_be_complete(self):
        self.cases['case_b']['Status'] = 'FAIL'
        self.assertIsNone(self.qualify())

    def test_native_feynman_check_cannot_be_omitted(self):
        self.cases['case_b']['FeynmanRows'] = 0
        self.assertIsNone(self.qualify())

    def test_madgraph_must_pass_independently(self):
        self.export_summary['MadGraphStatus'] = 'FAIL'
        self.assertIsNone(self.qualify())

    def test_complete_case_and_export_bindings_must_match(self):
        self.export_summary['CaseCapsulesSHA256']['case_a'] = '0' * 64
        self.assertIsNone(self.qualify())

    def test_insufficient_point_counts_are_rejected(self):
        self.assertIsNone(self.qualify(required_points=1001))
        self.report['Points'] = 999
        self.seal()
        self.assertIsNone(self.qualify())

    def test_looser_tolerance_and_nonfinite_tolerance_are_rejected(self):
        for tolerance in (2e-7, float('nan'), float('inf'), -1):
            with self.subTest(tolerance=tolerance):
                self.report['RelativeTolerance'] = tolerance
                self.seal()
                self.assertIsNone(self.qualify())

    def test_native_control_must_exist_and_reproduce_all_points(self):
        self.report['Results']['Native']['OriginalBitMatches'] = 999
        self.seal()
        self.assertIsNone(self.qualify())
        del self.report['Results']['Native']
        self.seal()
        self.assertIsNone(self.qualify())

    def test_native_replay_boolean_is_required(self):
        self.report['NativeControlReproducesCapture'] = False
        self.seal()
        self.assertIsNone(self.qualify())

    def test_extended_precision_must_be_genuine_and_complete(self):
        for field, value in (('MantissaBits', 64), ('REALBytes', 8), ('Errors', 1),
                             ('Mismatches', 1), ('Points', 999), ('Status', 'FAIL')):
            with self.subTest(field=field):
                original = self.report['Results']['GCC128'][field]
                self.report['Results']['GCC128'][field] = value
                self.seal()
                self.assertIsNone(self.qualify())
                self.report['Results']['GCC128'][field] = original

    def test_counts_must_be_integers_rather_than_boolean_success_flags(self):
        for backend, field in (('Native', 'Errors'), ('GCC128', 'Errors'), ('GCC128', 'Mismatches')):
            with self.subTest(backend=backend, field=field):
                original = self.report['Results'][backend][field]
                self.report['Results'][backend][field] = False
                self.seal()
                self.assertIsNone(self.qualify())
                self.report['Results'][backend][field] = original

    def test_extended_residual_must_satisfy_the_recorded_tolerance(self):
        self.report['Results']['GCC128']['MaximumRelativeError'] = 1.1e-7
        self.seal()
        self.assertIsNone(self.qualify())

    def test_changed_manifest_export_source_and_linked_libraries_are_rejected(self):
        for path in (self.export / 'process.cpp', self.export / 'manifest.json', self.root / 'runtime/libspinas.a'):
            with self.subTest(path=path.name):
                original = path.read_bytes()
                path.write_bytes(original + b'changed')
                self.assertIsNone(self.qualify())
                path.write_bytes(original)

    def test_omitted_source_and_library_bindings_are_rejected(self):
        del self.report['ExportSourcesSHA256']['process.cpp']
        self.seal()
        self.assertIsNone(self.qualify())
        self.report['ExportSourcesSHA256']['process.cpp'] = digest(self.export / 'process.cpp')
        self.report['LinkedLibrariesSHA256'].pop(next(iter(self.report['LinkedLibrariesSHA256'])))
        self.seal()
        self.assertIsNone(self.qualify())

    def test_numerical_inputs_and_archived_tool_hashes_are_verified(self):
        for filename in ('points.csv', 'parameters.csv', 'replay-tool.py'):
            with self.subTest(filename=filename):
                path = self.audit / filename
                original = path.read_bytes()
                path.write_bytes(original + b'changed')
                self.assertIsNone(self.qualify())
                path.write_bytes(original)

    def test_truncated_point_set_is_not_accepted_even_with_updated_hash(self):
        path = self.audit / 'points.csv'
        path.write_text('\n'.join(path.read_text().splitlines()[:-1]) + '\n')
        self.report['PointsSHA256'] = digest(path)
        self.seal()
        self.assertIsNone(self.qualify())

    def test_report_path_cannot_traverse_or_escape_via_symlink(self):
        self.link['Report'] = '../precision-summary.json'
        self.seal()
        self.assertIsNone(self.qualify())
        outside = self.root / 'outside'
        outside.mkdir()
        (outside / 'precision-summary.json').write_text(json.dumps(self.report))
        (self.results / 'alias').symlink_to(outside, target_is_directory=True)
        self.link['Report'] = 'alias/precision-summary.json'
        self.link['ReportSHA256'] = digest(outside / 'precision-summary.json')
        write(self.results / 'family_demo-precision.json', self.link)
        self.assertIsNone(self.qualify())

    def test_export_source_path_cannot_traverse(self):
        outside = self.root / 'outside.cpp'
        outside.write_text('// unrelated')
        self.report['ExportSourcesSHA256']['../../outside.cpp'] = digest(outside)
        self.seal()
        self.assertIsNone(self.qualify())

    def test_family_filename_cannot_traverse(self):
        self.assertIsNone(self.qualify(family={**self.family, 'ID': '../family_demo'}))


    def test_current_failed_sample_is_recomputed_rather_than_trusting_success_flags(self):
        verified = self.qualify()
        self.assertEqual(verified['CurrentFailureControl']['MomentumComponents'], 16)
        self.assertTrue(verified['CurrentFailureControl']['BitwiseMatchVerified'])
        points = self.audit / 'points.csv'
        text = points.read_text().splitlines()
        row = text[3].split(',')
        row[3] = '1e-300'
        text[3] = ','.join(row)
        points.write_text('\n'.join(text) + '\n')
        self.report['PointsSHA256'] = digest(points)
        self.seal()
        self.assertTrue(self.control['AllSavedMomentumComponentsBitMatched'])
        self.assertIsNone(self.qualify())

    def test_old_or_missing_current_failure_controls_are_not_qualifications(self):
        for field in ('CurrentFailureControl', 'CurrentFailureControlSHA256'):
            with self.subTest(field=field):
                value = self.link.pop(field)
                self.seal()
                self.assertIsNone(self.qualify())
                self.link[field] = value
        self.control_path.write_text('{}')
        self.assertIsNone(self.qualify())

    def test_raw_failure_and_control_must_bind_the_current_export(self):
        for target, field in ((self.raw_failure, 'ExportCapsuleSHA256'),
                              (self.raw_failure, 'ContextSHA256'),
                              (self.raw_failure, 'FamilyID'),
                              (self.control, 'ExportCapsuleSHA256')):
            with self.subTest(field=field, target=target is self.control):
                original = target[field]
                target[field] = 'b' * 64
                self.seal_failure()
                self.seal()
                self.assertIsNone(self.qualify())
                target[field] = original

    def test_raw_failure_json_checksum_is_verified(self):
        self.raw_path.write_text('{}')
        self.assertIsNone(self.qualify())

    def test_seed_stays_exact_above_the_binary64_integer_range(self):
        self.assertIsNotNone(self.qualify())
        self.raw_failure['Seed'] = str(self.report['Seed'])
        self.seal_failure()
        self.seal()
        self.assertIsNotNone(self.qualify())
        self.raw_failure['Seed'] = float(self.report['Seed'])
        self.seal_failure()
        self.seal()
        self.assertIsNone(self.qualify())

    def test_replay_control_and_raw_seeds_must_all_match(self):
        for target in (self.report, self.control, self.raw_failure):
            with self.subTest(target=target is self.report):
                original = target['Seed']
                target['Seed'] = original + 1
                self.seal_failure()
                self.seal()
                self.assertIsNone(self.qualify())
                target['Seed'] = original

    def test_invalid_seed_types_and_bounds_are_rejected(self):
        for seed in (None, True, -1, 2**64, float(2**63 + 123), '1.5'):
            with self.subTest(seed=seed):
                self.report['Seed'] = seed
                self.seal()
                self.assertIsNone(self.qualify())

    def test_failed_point_index_and_component_counts_are_not_boolean_claims(self):
        for field, value in (('CurrentFirstFailedPoint', 4), ('CurrentFirstFailedPoint', True),
                             ('MomentumComponents', 20), ('MomentumComponentsBitMatched', 15),
                             ('AllSavedMomentumComponentsBitMatched', False)):
            with self.subTest(field=field, value=value):
                original = self.control[field]
                self.control[field] = value
                self.seal_failure()
                self.seal()
                self.assertIsNone(self.qualify())
                self.control[field] = original

    def test_incomplete_or_duplicated_raw_momenta_are_rejected(self):
        original = self.raw_failure['CommandResult']['StandardOutput']
        for text in (original.replace('  p4 = 0 0 0 0\n', ''),
                     original + '  p1 = 0 0 0 0\n',
                     original.replace('  p1 = 0 0 0 0', '  p1 = 0 0 0'),
                     original.replace('  p1 = 0 0 0 0', '  p1 = nan 0 0 0')):
            with self.subTest(text=text):
                self.raw_failure['CommandResult']['StandardOutput'] = text
                self.seal_failure()
                self.seal()
                self.assertIsNone(self.qualify())

    def test_bitwise_comparison_preserves_signed_zero(self):
        self.raw_failure['CommandResult']['StandardOutput'] = self.raw_failure['CommandResult']['StandardOutput'].replace(
            '  p1 = 0 0 0 0', '  p1 = -0 0 0 0')
        self.seal_failure()
        self.seal()
        self.assertIsNone(self.qualify())

    def test_current_raw_stream_counts_and_first_failure_must_be_present(self):
        original = self.raw_failure['CommandResult']['StandardOutput']
        for old, new in (('1000/1000/1000', '1000/999/999'), ('29/0', '28/0'),
                         ('29/0', '29/1'), ('First failing point: 3', 'First failing point: 1001'),
                         ('First failing point: 3', 'No saved failure')):
            with self.subTest(old=old, new=new):
                self.raw_failure['CommandResult']['StandardOutput'] = original.replace(old, new)
                self.seal_failure()
                self.seal()
                self.assertIsNone(self.qualify())

    def test_current_failure_artifact_paths_cannot_traverse(self):
        self.link['CurrentFailureControl'] = '../current-failure-control.json'
        self.seal()
        self.assertIsNone(self.qualify())
        self.seal_failure()
        self.control['RawFailureReport'] = '../current-raw-failure.json'
        self.seal_failure()
        self.seal()
        self.assertIsNone(self.qualify())

    def test_a_failed_raw_stream_requires_the_hashed_full_point_set(self):
        del self.report['PointsSHA256']
        self.seal()
        self.assertIsNone(self.qualify())

    def test_unrun_reference_backend_is_not_precision_qualified(self):
        self.export_summary['CalcHEPStatus'] = 'NOT RUN'
        self.assertIsNone(self.qualify())

    def test_metadata_cli_rechecks_context_without_running_physics(self):
        write(self.results / 'configuration.json', {'Points': 1000, 'ContextSHA256': self.context})
        inventory = self.root / 'inventory.json'
        write(inventory, {'Families': [self.family]})
        command = ['precision_evidence.py', '--family', 'family_demo', '--results', str(self.results),
                   '--export-root', str(self.exports), '--inventory', str(inventory)]
        for context, expected in ((self.context, 'PRECISION_QUALIFIED'), ('b' * 64, 'UNQUALIFIED')):
            output = io.StringIO()
            with mock.patch.object(sys, 'argv', command), mock.patch.object(EVIDENCE, '_current_context', return_value=context), redirect_stdout(output):
                self.assertEqual(EVIDENCE.main(), 0)
            self.assertEqual(json.loads(output.getvalue())['Status'], expected)


if __name__ == '__main__':
    unittest.main()
