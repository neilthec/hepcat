"""Generation transport admission tests with synthetic checkpoints."""
# HEPCAT - High Energy Physics Constructive Amplitude Tool
# Copyright (C) 2026 Neil Christensen
# SPDX-License-Identifier: GPL-3.0-or-later

import importlib.util
import os
from pathlib import Path
import sys
import unittest
from unittest import mock

sys.dont_write_bytecode = True
HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
CAMPAIGN = ROOT / 'tests/SM-5-point'

def load(path, name):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module

wrapper = load(CAMPAIGN / 'transport-generation.py', 'sm5_generation_launcher')
fixtures = load(ROOT / 'tests/checks/test_sm_five_point_workflow.py', 'original_generation_workflow_fixtures')

class SourceAdmissionTests(unittest.TestCase):
    def setUp(self):
        self.fixture = fixtures.FivePointWorkflowTests('test_complete_family_requires_every_independent_case_and_both_backends')
        self.fixture.setUp()
        self.addCleanup(self.fixture.doCleanups)
        self.fixture.valid_family()
        self.workflow = fixtures.WORKFLOW
        self.configuration = {'ContextSHA256': self.fixture.context, 'Points': 1000}
        for name, value in [('context_sha256', self.fixture.context), ('context_compatibility', None)]:
            patch = mock.patch.object(self.workflow, name, return_value=value)
            patch.start()
            self.addCleanup(patch.stop)

    def check(self, case='case_a'):
        return wrapper.source_admission(self.workflow, 'family_ab', case, self.configuration)

    def test_pass_requires_native_both_backends_and_literal_source_binding(self):
        row = self.check()
        self.assertEqual(row['Status'], 'APPROVED')
        self.assertEqual(row['GenerationSourceCaseID'], 'case_a')
        self.assertEqual(row['GenerationSourceExportCapsuleSHA256'],
            self.workflow.stage_context.digest(self.fixture.results / 'family_ab-export.wxf'))

    def test_each_external_backend_must_pass(self):
        for backend in ['CalcHEPStatus', 'MadGraphStatus']:
            with self.subTest(backend=backend):
                self.fixture.record('family_ab', export=True, **{backend: 'FAIL'})
                self.assertEqual(self.check()['Status'], 'REJECTED')

    def test_raw_fail_with_precision_label_is_not_upgraded(self):
        self.fixture.record('family_ab', export=True, Status='FAIL',
            PrecisionQualification={'Status': 'PRECISION_QUALIFIED'})
        self.assertEqual(self.check()['Status'], 'REJECTED')

    def test_insufficient_points_or_wrong_member_reject(self):
        self.fixture.record('family_ab', export=True, Points=999)
        self.assertEqual(self.check()['Status'], 'REJECTED')
        self.fixture.record('family_ab', export=True)
        self.assertEqual(self.check(case='not_a_member')['Status'], 'REJECTED')

    def test_native_tamper_uses_original_export_input_guard(self):
        (self.fixture.results / 'case_a.wxf').write_bytes(b'tampered native')
        self.assertEqual(self.check()['Status'], 'REJECTED')

    def test_failed_supplement_uses_original_probe_guard(self):
        self.fixture.supplemental_probe(status='FAIL')
        self.assertEqual(self.check()['Status'], 'REJECTED')

    def test_missing_export_capsule_rejects(self):
        (self.fixture.results / 'family_ab-export.wxf').unlink()
        self.assertEqual(self.check()['Status'], 'REJECTED')

    def test_source_export_change_during_readback_rejects(self):
        original = self.workflow.checked_summary
        def changed(identifier, suffix, *args, **kwargs):
            result = original(identifier, suffix, *args, **kwargs)
            if suffix == '-export':
                path = self.fixture.results / 'family_ab-export.json'
                path.write_text(path.read_text() + ' ')
            return result
        with mock.patch.object(self.workflow, 'checked_summary', side_effect=changed):
            self.assertEqual(self.check()['Status'], 'REJECTED')

    def test_launcher_forwards_all_normal_flags_and_sets_inherited_opt_in(self):
        flags = ['--family', 'family_ab', '--points', '1000', '--retry-failed']
        with mock.patch.dict(os.environ, {}, clear=True), mock.patch.object(wrapper.os, 'execv') as execute:
            self.assertEqual(wrapper.main(flags), 0)
            self.assertEqual(os.environ['HEPCAT_SM5_TRANSPORT_GENERATION'], '1')
            execute.assert_called_once_with(sys.executable,
                [sys.executable, str(CAMPAIGN / 'run.py'), *flags])

if __name__ == '__main__':
    unittest.main(verbosity=2)
