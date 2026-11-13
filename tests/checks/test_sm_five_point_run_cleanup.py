"""Runner admission for generated-cache cleanup, without physics or deletion."""
# HEPCAT - High Energy Physics Constructive Amplitude Tool
# Copyright (C) 2026 Neil Christensen
# SPDX-License-Identifier: GPL-3.0-or-later
from pathlib import Path
import importlib.util
from types import SimpleNamespace
import unittest
from unittest.mock import Mock, patch

ROOT = Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location('sm5_run_cleanup_test', ROOT/'tests/SM-5-point/run.py')
run = importlib.util.module_from_spec(spec); spec.loader.exec_module(run)


class RunCleanupTests(unittest.TestCase):
    def setUp(self):
        self.family = {'ID': 'family', 'CaseIDs': ['case']}
        self.state = {'Cases': {'case': {}}}
        self.export = {'Status': 'PASS'}
        self.case = {'ID': 'case', 'Status': 'COMPLETE'}
        self.cleanup = Mock(return_value={'Status': 'CLEANED'})
        self.module = SimpleNamespace(cleanup_completed_family=self.cleanup)
        self.spec = SimpleNamespace(loader=SimpleNamespace(exec_module=Mock()))

    def invoke(self):
        with patch.object(run, 'context_sha256', return_value='context'), \
             patch.object(run, 'context_compatibility', return_value='proof'), \
             patch.object(run, 'checked_summary', side_effect=[self.export, self.case]) as checked, \
             patch.object(run.importlib.util, 'spec_from_file_location', return_value=self.spec), \
             patch.object(run.importlib.util, 'module_from_spec', return_value=self.module):
            result = run.cleanup_family_generated(self.family, self.state, {}, runner_lock='held-lock')
        return result, checked

    def test_fresh_checked_records_and_runner_lock_reach_cleanup(self):
        result, checked = self.invoke()
        self.assertEqual(result, {'Status': 'CLEANED'})
        self.assertEqual(checked.call_args_list[0].args, ('family', '-export', 'context', 'proof'))
        self.assertEqual(checked.call_args_list[1].args, ('case', '', 'context', 'proof'))
        self.cleanup.assert_called_once_with(run.ROOT, run.RESULTS, self.family,
            {'case': self.case}, self.export, runner_lock='held-lock')

    def test_raw_backend_failure_is_retained(self):
        self.export = {'Status': 'FAIL', 'PrecisionQualification': {'Status': 'PRECISION_QUALIFIED'}}
        result, checked = self.invoke()
        self.assertEqual(result['Status'], 'RETAINED')
        self.assertEqual(checked.call_count, 1)
        self.cleanup.assert_not_called()

    def test_missing_export_is_retained(self):
        self.export = None
        result, _ = self.invoke()
        self.assertEqual(result['Status'], 'RETAINED')
        self.cleanup.assert_not_called()

    def test_supplemental_failure_or_unavailable_is_retained(self):
        for status in ('FAIL', 'UNAVAILABLE'):
            with self.subTest(status=status):
                self.state['Cases']['case'] = {'SupplementalParameterProbes': {'Status': status}}
                result, _ = self.invoke()
                self.assertEqual(result['Status'], 'RETAINED')
                self.cleanup.assert_not_called()


if __name__ == '__main__': unittest.main()
