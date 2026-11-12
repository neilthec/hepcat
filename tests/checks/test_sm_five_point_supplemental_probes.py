"""Supplemental bookkeeping tests. All reports and capsule bytes are synthetic."""
# HEPCAT - High Energy Physics Constructive Amplitude Tool
# Copyright (C) 2026 Neil Christensen
# SPDX-License-Identifier: GPL-3.0-or-later
import copy
import importlib.util
import json
from pathlib import Path
import tempfile
import unittest

SPEC = importlib.util.spec_from_file_location('sm5_supplemental_probe_checks',
    Path(__file__).resolve().parents[1] / 'SM-5-point/supplemental_probes.py')
PROBES = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(PROBES)


class SupplementalProbeTests(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory(prefix='hepcat-supplemental-probe-')
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)
        self.summary = {'SourceCaseID': 'case', 'SourceCapsuleSHA256': 'source-sha',
                        'ContextSHA256': 'context', 'Status': 'CHECKS_NOT_ALL_PASS',
                        'StatusCounts': {'PASS': 1, 'FAIL': 1},
                        'Rows': [{'Status': 'PASS'}, {'Status': 'FAIL'}],
                        'SourceAmplitudeUnchanged': True,
                        'NoExpressionMassSlotOrFlavorTransport': True}
        self.entry = {'ID': 'probe', 'CaseID': 'case', 'Kind': 'ParameterCheck',
                      'SourceCapsuleSHA256': 'source-sha', 'ProbeContextSHA256': 'context',
                      'Status': 'CHECKS_NOT_ALL_PASS', 'StatusCounts': {'PASS': 1, 'FAIL': 1},
                      'Files': {}}
        for name in ('Capsule', 'Summary', 'Script', 'ConversionScript'):
            path = self.root / name
            path.write_text(json.dumps(self.summary) if name == 'Summary' else 'synthetic ' + name)
            self.entry['Files'][name] = {'Path': name, 'SHA256': PROBES.digest(path)}

    def report(self, *, entry=None, source='source-sha', context='context'):
        return PROBES.case_report('case', source, [entry or self.entry], self.root, context)

    def update_summary(self, **values):
        self.summary.update(values)
        path = self.root / 'Summary'
        path.write_text(json.dumps(self.summary))
        self.entry['Files']['Summary']['SHA256'] = PROBES.digest(path)

    def test_verified_counterexample_is_not_a_pass(self):
        value = self.report()
        self.assertEqual(value['Status'], 'FAIL')
        self.assertEqual(value['Probes'][0]['StatusCounts'], {'PASS': 1, 'FAIL': 1})

    def test_unrelated_cases_have_no_probe(self):
        self.assertIsNone(PROBES.case_report('other', 'source-sha', [self.entry], self.root, 'context'))

    def test_missing_or_changed_artifact_cannot_clear_a_known_probe(self):
        for name in self.entry['Files']:
            with self.subTest(name=name):
                path = self.root / name
                original = path.read_bytes()
                path.write_bytes(b'changed')
                self.assertEqual(self.report()['Status'], 'UNAVAILABLE')
                path.unlink()
                self.assertEqual(self.report()['Status'], 'UNAVAILABLE')
                path.write_bytes(original)

    def test_new_expression_or_context_needs_a_probe_recheck(self):
        self.assertEqual(self.report(source='new-source')['Status'], 'UNAVAILABLE')
        self.assertEqual(self.report(context='new-context')['Status'], 'UNAVAILABLE')

    def test_counts_must_match_actual_rows_and_registry(self):
        self.update_summary(StatusCounts={'PASS': 2})
        self.assertEqual(self.report()['Status'], 'UNAVAILABLE')
        self.entry['StatusCounts'] = {'PASS': 2}
        self.assertEqual(self.report()['Status'], 'UNAVAILABLE')

    def test_summary_identity_and_unchanged_expression_are_required(self):
        for field, value in [('SourceCaseID', 'other'), ('ContextSHA256', 'other'),
                             ('SourceCapsuleSHA256', 'other'), ('SourceAmplitudeUnchanged', False),
                             ('NoExpressionMassSlotOrFlavorTransport', False)]:
            with self.subTest(field=field):
                old = self.summary[field]
                self.update_summary(**{field: value})
                self.assertEqual(self.report()['Status'], 'UNAVAILABLE')
                self.update_summary(**{field: old})

    def test_path_escape_and_symlink_escape_are_rejected(self):
        entry = copy.deepcopy(self.entry)
        for path in ('../outside', str(self.root / 'Capsule')):
            entry['Files']['Capsule']['Path'] = path
            self.assertEqual(self.report(entry=entry)['Status'], 'UNAVAILABLE')
        (self.root / 'escape').symlink_to(self.root.parent)
        entry['Files']['Capsule']['Path'] = 'escape/outside'
        self.assertEqual(self.report(entry=entry)['Status'], 'UNAVAILABLE')

    def test_unresolved_rows_are_not_counterexample_or_pass(self):
        self.entry['StatusCounts'] = {'UNRESOLVED': 1}
        self.update_summary(StatusCounts={'UNRESOLVED': 1}, Rows=[{'Status': 'UNRESOLVED'}])
        self.assertEqual(self.report()['Status'], 'UNAVAILABLE')

    def test_a_bound_actual_all_pass_probe_is_recognized(self):
        self.entry.update(Status='ALL_PASS', StatusCounts={'PASS': 2})
        self.update_summary(Status='ALL_PASS', StatusCounts={'PASS': 2}, Rows=[{'Status': 'PASS'}] * 2)
        self.assertEqual(self.report()['Status'], 'PASS')

    def test_malformed_or_duplicate_registry_is_rejected(self):
        path = self.root / 'registry.json'
        for value in ([], {}, {'SchemaVersion': 1, 'Probes': [self.entry] * 2}):
            path.write_text(json.dumps(value))
            with self.assertRaises(ValueError):
                PROBES.read_registry(path)
        path.write_text(json.dumps({'SchemaVersion': 1, 'Probes': [self.entry]}))
        self.assertEqual(PROBES.read_registry(path), [self.entry])


if __name__ == '__main__':
    unittest.main()
