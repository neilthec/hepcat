"""Checkpoint admission and worker lifecycle tests, using no physics kernels.

Every capsule in this suite is an explicitly synthetic byte fixture. These
checks test workflow accounting and stale-result rejection, not amplitudes.
"""

# HEPCAT - High Energy Physics Constructive Amplitude Tool
# Copyright (C) 2026 Neil Christensen
# SPDX-License-Identifier: GPL-3.0-or-later

from contextlib import redirect_stderr, redirect_stdout
import hashlib
import importlib.util
import io
import json
from pathlib import Path
import subprocess
import sys
import tempfile
from types import SimpleNamespace
import unittest
from unittest import mock


SCRIPT = Path(__file__).resolve().parents[1] / 'SM-5-point' / 'run.py'
SPEC = importlib.util.spec_from_file_location('hepcat_sm_five_point_workflow', SCRIPT)
WORKFLOW = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(WORKFLOW)


class FivePointWorkflowTests(unittest.TestCase):
    context = 'synthetic-current-input-digest'

    def setUp(self):
        temporary = tempfile.TemporaryDirectory(prefix='hepcat-sm-five-workflow-')
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)
        self.here = self.root / 'tests/SM-5-point'
        self.results = self.root / 'tests/results/SM-5-point'
        self.here.mkdir(parents=True)
        self.results.mkdir(parents=True)
        self.inventory = {
            'Cases': [{'ID': 'case_a', 'Particles': ['h'] * 5, 'FamilyID': 'family_ab'},
                      {'ID': 'case_b', 'Particles': ['h', 'h', 'h', 'A.+', 'A.-'],
                       'FamilyID': 'family_ab'}],
            'Families': [{'ID': 'family_ab', 'CaseIDs': ['case_a', 'case_b'],
                          'Sector': 'SyntheticFixtures'}],
        }
        self.write_json(self.here / 'inventory.json', self.inventory)
        self.write_json(self.here / 'supplemental-probes.json', {'SchemaVersion': 1, 'Probes': []})
        self.configuration = self.results / 'configuration.json'
        self.write_json(self.configuration, {'ContextSHA256': self.context, 'Points': 1000})
        for key, value in {'ROOT': self.root, 'HERE': self.here,
                           'RESULTS': self.results, 'PROGRESS': self.here / 'progress.json'}.items():
            patcher = mock.patch.object(WORKFLOW, key, value)
            patcher.start()
            self.addCleanup(patcher.stop)
        self.args = SimpleNamespace(kernel='synthetic-kernel', timeout=1,
                                    export_timeout=1, points=1000)

    @staticmethod
    def write_json(path, value):
        path.write_text(json.dumps(value) + '\n')

    def record(self, identifier, *, export=False, **changes):
        stem = identifier + ('-export' if export else '')
        capsule = self.results / (stem + '.wxf')
        capsule.write_bytes(('synthetic capsule: ' + stem).encode())
        value = {'FamilyID' if export else 'ID': identifier,
                 'Status': 'PASS' if export else 'COMPLETE',
                 'ContextSHA256': self.context,
                 'CapsuleSHA256': hashlib.sha256(capsule.read_bytes()).hexdigest()}
        if export:
            value.update(Points=1000, CalcHEPStatus='PASS', MadGraphStatus='PASS',
                         CaseIDs=['case_a', 'case_b'],
                         CaseCapsulesSHA256={cid: hashlib.sha256((self.results / (cid + '.wxf')).read_bytes()).hexdigest()
                             if (self.results / (cid + '.wxf')).is_file() else None
                             for cid in ('case_a', 'case_b')})
        else:
            value.update(Particles=next(row['Particles'] for row in self.inventory['Cases']
                                        if row['ID'] == identifier),
                         Rows=3, FeynmanRows=1, CheckStatuses={'PASS': 3})
        value.update(changes)
        self.write_json(self.results / (stem + '.json'), value)
        return value

    def refresh(self, *, points=1000, context=None):
        return WORKFLOW.refresh(self.inventory, context_sha=context or self.context, points=points)

    def valid_family(self):
        self.record('case_a')
        self.record('case_b')
        self.record('family_ab', export=True)

    def assert_not_fully_validated(self, state):
        self.assertEqual(state['Counts']['FullyValidatedCases'], 0)
        self.assertEqual(state['Counts']['ValidatedFamilies'], 0)

    def worker(self, kind='construct', identifier='case_a'):
        with redirect_stdout(io.StringIO()):
            return WORKFLOW.run_worker(self.args, kind, identifier, self.configuration)

    @staticmethod
    def fake_process(code=0):
        process = mock.Mock(pid=12345, returncode=code)
        process.wait.return_value = code
        return process

    def test_complete_family_requires_every_independent_case_and_both_backends(self):
        self.valid_family()
        counts = self.refresh()['Counts']
        self.assertEqual(counts['Constructed'], 2)
        self.assertEqual(counts['FullyValidatedCases'], 2)
        self.assertEqual(counts['ValidatedFamilies'], 1)

    def supplemental_probe(self, status='FAIL'):
        source = json.loads((self.results / 'case_a.json').read_text())
        counts = {status: 1}
        summary = {'SourceCaseID': 'case_a', 'SourceCapsuleSHA256': source['CapsuleSHA256'],
                   'ContextSHA256': self.context,
                   'Status': 'ALL_PASS' if status == 'PASS' else 'CHECKS_NOT_ALL_PASS',
                   'StatusCounts': counts, 'Rows': [{'Status': status}],
                   'SourceAmplitudeUnchanged': True,
                   'NoExpressionMassSlotOrFlavorTransport': True}
        entry = {'ID': 'synthetic-probe', 'CaseID': 'case_a', 'Kind': 'ParameterCheck',
                 'SourceCapsuleSHA256': source['CapsuleSHA256'],
                 'ProbeContextSHA256': self.context, 'Status': summary['Status'],
                 'StatusCounts': counts, 'Files': {}}
        for name in ('Capsule', 'Summary', 'Script', 'ConversionScript'):
            path = self.results / ('probe-' + name)
            path.write_text(json.dumps(summary) if name == 'Summary' else 'synthetic ' + name)
            entry['Files'][name] = {'Path': path.name,
                                   'SHA256': hashlib.sha256(path.read_bytes()).hexdigest()}
        self.write_json(self.here / 'supplemental-probes.json', {'SchemaVersion': 1, 'Probes': [entry]})
        return entry

    def test_known_parameter_failure_qualifies_counts_without_rewriting_raw_pass(self):
        self.valid_family()
        self.supplemental_probe()
        state = self.refresh()
        self.assertEqual(state['Counts']['Constructed'], 2)
        self.assertEqual(state['Counts']['RecordedInputPassCases'], 2)
        self.assertEqual(state['Counts']['SupplementalParameterProbeFailedCases'], 1)
        self.assertEqual(state['Families']['family_ab']['Status'], 'PASS')
        self.assertEqual(state['Families']['family_ab']['RecordedInputStatus'], 'PASS')
        self.assertEqual(state['Cases']['case_a']['Status'], 'COMPLETE')
        self.assertEqual(state['Cases']['case_a']['SupplementalParameterProbes']['Status'], 'FAIL')
        self.assert_not_fully_validated(state)
        self.assertNotIn('SupplementalParameterProbes', json.loads((self.results / 'case_a.json').read_text()))

    def test_unavailable_probe_blocks_full_validation_without_inventing_a_failure(self):
        self.valid_family()
        entry = self.supplemental_probe()
        (self.results / entry['Files']['Capsule']['Path']).unlink()
        state = self.refresh()
        self.assertEqual(state['Counts']['SupplementalParameterProbeFailedCases'], 0)
        self.assertEqual(state['Counts']['SupplementalParameterProbeUnavailableCases'], 1)
        self.assert_not_fully_validated(state)

    def test_bound_passing_supplement_does_not_block_a_complete_family(self):
        self.valid_family()
        self.supplemental_probe('PASS')
        state = self.refresh()
        self.assertEqual(state['Counts']['FullyValidatedCases'], 2)
        self.assertEqual(state['Counts']['ValidatedFamilies'], 1)
        self.assertEqual(state['Counts']['SupplementalParameterProbeFailedCases'], 0)

    def test_malformed_probe_registry_cannot_write_a_new_green_ledger(self):
        self.valid_family()
        self.write_json(self.here / 'supplemental-probes.json', [])
        with self.assertRaises(ValueError):
            self.refresh()
        self.assertFalse((self.here / 'progress.json').exists())

    def test_known_parameter_failure_blocks_automatic_new_export(self):
        self.valid_family()
        config = {'ContextSHA256': self.context}
        with mock.patch.object(WORKFLOW, 'context_sha256', return_value=self.context), \
             mock.patch.object(WORKFLOW, 'context_compatibility', return_value=None):
            self.assertEqual(WORKFLOW.export_input_admission('family_ab', config)['Status'], 'APPROVED')
            entry = self.supplemental_probe()
            rejected = WORKFLOW.export_input_admission('family_ab', config)
            self.assertEqual(rejected['Status'], 'REJECTED')
            self.assertIn('supplemental parameter', rejected['Reason'])
            (self.results / entry['Files']['Capsule']['Path']).unlink()
            self.assertEqual(WORKFLOW.export_input_admission('family_ab', config)['Status'], 'REJECTED')

    def test_family_pass_alone_does_not_imply_constructed_members(self):
        self.record('family_ab', export=True)
        self.assert_not_fully_validated(self.refresh())

    def test_missing_member_is_not_counted_as_a_validated_family(self):
        self.valid_family()
        (self.results / 'case_b.json').unlink()
        self.assert_not_fully_validated(self.refresh())

    def test_failed_member_invalidates_old_successful_family(self):
        self.valid_family()
        self.record('case_b', Status='FAIL')
        state = self.refresh()
        self.assertEqual(state['Counts']['Constructed'], 1)
        self.assert_not_fully_validated(state)

    def test_nonpass_native_rows_cannot_be_admitted_as_complete(self):
        self.valid_family()
        self.record('case_b', CheckStatuses={'PASS': 2, 'FAIL': 1})
        state = self.refresh()
        self.assertEqual(state['Counts']['Constructed'], 1)
        self.assert_not_fully_validated(state)

    def test_missing_native_feynman_comparison_cannot_be_admitted(self):
        self.valid_family()
        self.record('case_b', FeynmanRows=0)
        self.assert_not_fully_validated(self.refresh())

    def test_empty_check_set_cannot_claim_complete(self):
        self.valid_family()
        self.record('case_b', Rows=0, CheckStatuses={})
        self.assert_not_fully_validated(self.refresh())

    def test_missing_case_capsule_invalidates_summary(self):
        self.valid_family()
        (self.results / 'case_b.wxf').unlink()
        self.assert_not_fully_validated(self.refresh())

    def test_modified_case_capsule_invalidates_summary(self):
        self.valid_family()
        (self.results / 'case_b.wxf').write_bytes(b'modified synthetic capsule')
        self.assert_not_fully_validated(self.refresh())

    def test_missing_family_capsule_invalidates_summary(self):
        self.valid_family()
        (self.results / 'family_ab-export.wxf').unlink()
        self.assert_not_fully_validated(self.refresh())

    def test_modified_family_capsule_invalidates_summary(self):
        self.valid_family()
        (self.results / 'family_ab-export.wxf').write_bytes(b'modified synthetic export')
        self.assert_not_fully_validated(self.refresh())

    def test_changed_construction_context_invalidates_case(self):
        self.valid_family()
        self.record('case_b', ContextSHA256='old package or model digest')
        state = self.refresh()
        self.assertEqual(state['Counts']['Constructed'], 1)
        self.assert_not_fully_validated(state)

    def test_changed_export_context_invalidates_reference_validation(self):
        self.valid_family()
        self.record('family_ab', export=True, ContextSHA256='old numerical runtime digest')
        state = self.refresh()
        self.assertEqual(state['Counts']['Constructed'], 2)
        self.assert_not_fully_validated(state)

    def test_global_context_change_rejects_all_old_capsules(self):
        self.valid_family()
        state = self.refresh(context='updated sources and models')
        self.assertEqual(state['Counts']['Constructed'], 0)
        self.assert_not_fully_validated(state)

    def test_legacy_unfingerprinted_records_are_not_reused(self):
        self.valid_family()
        path = self.results / 'case_b.json'
        row = json.loads(path.read_text())
        del row['ContextSHA256']
        del row['CapsuleSHA256']
        self.write_json(path, row)
        self.assert_not_fully_validated(self.refresh())

    def test_summary_identity_must_match_its_inventory_slot(self):
        self.valid_family()
        self.record('case_b', ID='different_case')
        self.assert_not_fully_validated(self.refresh())

    def test_family_identity_must_match_its_inventory_slot(self):
        self.valid_family()
        self.record('family_ab', export=True, FamilyID='different_family')
        self.assert_not_fully_validated(self.refresh())

    def test_each_independent_backend_must_pass(self):
        for field in ('CalcHEPStatus', 'MadGraphStatus'):
            with self.subTest(field=field):
                self.valid_family()
                self.record('family_ab', export=True, **{field: 'FAIL'})
                self.assert_not_fully_validated(self.refresh())

    def test_prior_smaller_phase_space_run_does_not_meet_requested_coverage(self):
        self.valid_family()
        self.assert_not_fully_validated(self.refresh(points=2000))

    def test_more_validation_points_satisfy_a_smaller_request(self):
        self.valid_family()
        self.record('family_ab', export=True, Points=2000)
        self.assertEqual(self.refresh(points=1000)['Counts']['FullyValidatedCases'], 2)

    def test_malformed_summary_does_not_break_refresh_or_claim_completion(self):
        self.valid_family()
        (self.results / 'case_b.json').write_text('{truncated')
        self.assert_not_fully_validated(self.refresh())

    def test_zero_or_inconsistent_pass_counts_do_not_claim_complete(self):
        for counts in ({'PASS': 0}, {'PASS': 1}):
            with self.subTest(counts=counts):
                self.valid_family()
                self.record('case_b', Rows=3, CheckStatuses=counts)
                self.assert_not_fully_validated(self.refresh())

    def context_fixture(self):
        files = ['source/HEPCAT.wl', 'source/internal/export/runtime.py',
                 'source/internal/export/validation.hpp',
                 'tests/reference-models/calchep/lgrng1.mdl',
                 'tests/reference-models/madgraph/sm/parameters.py']
        files += [f'models/{stem}{number}.mdl' for number in (1, 2)
                  for stem in ('amps', 'prtcls', 'vars', 'func')]
        for name in files:
            path = self.root / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text('synthetic input ' + name + '\n')
        self.spinas = self.root / 'runtime/spinas'
        for name in ('include/particle.h', 'build/libspinas.a'):
            path = self.spinas / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_bytes(('synthetic runtime ' + name).encode())
        return {'SPINASDirectory': str(self.spinas),
                'CalcHEPDirectory': str(self.root / 'runtime/calchep'),
                'MadGraphDirectory': str(self.root / 'runtime/madgraph'),
                'Points': 1000, 'MemoryGiB': 4}

    def test_source_models_references_inventory_and_export_templates_bind_context(self):
        configuration = self.context_fixture()
        baseline = WORKFLOW.context_sha256(configuration)
        for name in ('source/HEPCAT.wl', 'source/internal/export/runtime.py',
                     'source/internal/export/validation.hpp', 'models/amps1.mdl',
                     'models/amps2.mdl', 'models/vars2.mdl',
                     'tests/reference-models/calchep/lgrng1.mdl',
                     'tests/reference-models/madgraph/sm/parameters.py',
                     'tests/SM-5-point/inventory.json'):
            with self.subTest(name=name):
                path = self.root / name
                original = path.read_bytes()
                path.write_bytes(original + b'\nchanged synthetic input')
                self.assertNotEqual(WORKFLOW.context_sha256(configuration), baseline)
                path.write_bytes(original)
                self.assertEqual(WORKFLOW.context_sha256(configuration), baseline)

    def test_spinas_header_and_library_updates_at_same_path_invalidate_context(self):
        configuration = self.context_fixture()
        baseline = WORKFLOW.context_sha256(configuration)
        for name in ('include/particle.h', 'build/libspinas.a'):
            with self.subTest(name=name):
                path = self.spinas / name
                original = path.read_bytes()
                path.write_bytes(original + b' changed runtime')
                self.assertNotEqual(WORKFLOW.context_sha256(configuration), baseline)
                path.write_bytes(original)

    def test_progress_results_and_git_commit_do_not_invalidate_physics_context(self):
        configuration = self.context_fixture()
        baseline = WORKFLOW.context_sha256(configuration)
        for name in ('tests/results/SM-5-point/new.json', 'tests/SM-5-point/progress.json', '.git/HEAD'):
            path = self.root / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text('synthetic changing bookkeeping')
        self.assertEqual(WORKFLOW.context_sha256(configuration), baseline)
        self.assertEqual(WORKFLOW.context_sha256({**configuration, 'MemoryGiB': 2, 'Points': 2000}), baseline)

    def test_rerun_expression_with_same_context_invalidates_old_compiled_export(self):
        self.valid_family()
        path = self.results / 'case_b.wxf'
        path.write_bytes(path.read_bytes() + b' new synthetic validated expression')
        summary_path = self.results / 'case_b.json'
        summary = json.loads(summary_path.read_text())
        summary['CapsuleSHA256'] = hashlib.sha256(path.read_bytes()).hexdigest()
        self.write_json(summary_path, summary)
        state = self.refresh()
        self.assertEqual(state['Counts']['Constructed'], 2)
        self.assert_not_fully_validated(state)

    def test_read_json_uses_default_for_missing_or_truncated_files(self):
        missing = self.results / 'missing.json'
        self.assertEqual(WORKFLOW.read_json(missing, {'default': True}), {'default': True})
        missing.write_text('{')
        self.assertEqual(WORKFLOW.read_json(missing, {'default': True}), {'default': True})

    def test_atomic_json_preserves_complete_document(self):
        path = self.results / 'atomic.json'
        WORKFLOW.atomic_json(path, {'Version': 1})
        WORKFLOW.atomic_json(path, {'Version': 2, 'Rows': list(range(5))})
        self.assertEqual(json.loads(path.read_text()), {'Version': 2, 'Rows': list(range(5))})
        self.assertFalse(path.with_suffix('.json.tmp').exists())

    def test_failed_worker_cannot_reuse_an_old_complete_summary(self):
        self.record('case_a')
        with mock.patch.object(WORKFLOW.subprocess, 'Popen', return_value=self.fake_process(1)):
            self.assertEqual(self.worker()['Status'], 'ERROR')

    def test_failed_export_cannot_reuse_an_old_pass_summary(self):
        self.record('family_ab', export=True)
        with mock.patch.object(WORKFLOW.subprocess, 'Popen', return_value=self.fake_process(1)):
            self.assertEqual(self.worker('export', 'family_ab')['Status'], 'ERROR')

    def test_worker_without_new_summary_cannot_reuse_old_success(self):
        self.record('case_a')
        with mock.patch.object(WORKFLOW.subprocess, 'Popen', return_value=self.fake_process(0)):
            self.assertEqual(self.worker()['Status'], 'ERROR')

    def test_old_summary_is_removed_before_worker_launch(self):
        self.record('case_a')
        def launch(*args, **kwargs):
            self.assertFalse((self.results / 'case_a.json').exists())
            return self.fake_process(1)
        with mock.patch.object(WORKFLOW.subprocess, 'Popen', side_effect=launch):
            self.assertEqual(self.worker()['Status'], 'ERROR')

    def test_nonzero_exit_rejects_even_a_new_successful_summary(self):
        def launch(*args, **kwargs):
            self.record('case_a')
            return self.fake_process(3)
        with mock.patch.object(WORKFLOW.subprocess, 'Popen', side_effect=launch):
            self.assertEqual(self.worker()['Status'], 'ERROR')

    def test_worker_startup_failure_is_checkpointed_as_error(self):
        self.record('case_a')
        with mock.patch.object(WORKFLOW.subprocess, 'Popen', side_effect=OSError('synthetic missing kernel')):
            self.assertEqual(self.worker()['Status'], 'ERROR')

    def test_external_timeout_replaces_old_success_and_stops_owned_worker(self):
        self.record('case_a')
        process = self.fake_process()
        process.wait.side_effect = subprocess.TimeoutExpired('synthetic kernel', 1)
        with mock.patch.object(WORKFLOW.subprocess, 'Popen', return_value=process), \
                mock.patch.object(WORKFLOW, 'stop_process') as stop:
            self.assertEqual(self.worker()['Status'], 'TIMEOUT')
            stop.assert_called_once_with(process)

    def test_worker_success_requires_a_capsule_not_only_a_json_claim(self):
        def launch(*args, **kwargs):
            row = self.record('case_a')
            (self.results / 'case_a.wxf').unlink()
            return self.fake_process(0)
        with mock.patch.object(WORKFLOW.subprocess, 'Popen', side_effect=launch):
            self.assertNotEqual(self.worker()['Status'], 'COMPLETE')

    def test_successful_worker_is_admitted_with_matching_new_capsule(self):
        def launch(*args, **kwargs):
            self.record('case_a')
            return self.fake_process(0)
        with mock.patch.object(WORKFLOW.subprocess, 'Popen', side_effect=launch):
            self.assertEqual(self.worker()['Status'], 'COMPLETE')

    def invoke_main(self, *options):
        output=io.StringIO()
        with mock.patch.object(sys, 'argv', ['run.py', '--family', 'family_ab', *options]), \
                mock.patch.object(WORKFLOW, 'cleanup_family_generated',
                    return_value={'Status': 'ALREADY_ABSENT'}) as cleanup, \
                redirect_stdout(output), redirect_stderr(io.StringIO()):
            self.last_cleanup = cleanup
            WORKFLOW.main()
        self.last_output=output.getvalue()
        return json.loads((self.here / 'progress.json').read_text())

    def test_absent_generated_root_skips_expensive_context_checks(self):
        family={**self.inventory['Families'][0],'ExportName':'ToyProcess'}
        with mock.patch.object(WORKFLOW,'context_sha256')as context, \
                mock.patch.object(WORKFLOW,'context_compatibility')as proof, \
                mock.patch.object(WORKFLOW,'checked_summary')as checked:
            result=WORKFLOW.cleanup_family_generated(family,{}, {})
        self.assertEqual(result['Status'],'ALREADY_ABSENT')
        self.assertEqual(result['FilesRemoved'],0)
        context.assert_not_called();proof.assert_not_called();checked.assert_not_called()

    def assert_cleanup_still_uses_context(self,family):
        with mock.patch.object(WORKFLOW,'context_sha256',return_value=self.context)as context, \
                mock.patch.object(WORKFLOW,'context_compatibility',return_value=None), \
                mock.patch.object(WORKFLOW,'checked_summary',return_value=None):
            result=WORKFLOW.cleanup_family_generated(family,{}, {})
        context.assert_called_once();self.assertEqual(result['Status'],'RETAINED')

    def test_existing_generated_root_keeps_all_admission_guards(self):
        family={**self.inventory['Families'][0],'ExportName':'ToyProcess'}
        target=self.root/'tests/results/spinas-output/SM-5-point/ToyProcess';target.mkdir(parents=True)
        self.assert_cleanup_still_uses_context(family);self.assertTrue(target.is_dir())

    def test_broken_target_symlink_is_not_treated_as_absent(self):
        family={**self.inventory['Families'][0],'ExportName':'ToyProcess'}
        target=self.root/'tests/results/spinas-output/SM-5-point/ToyProcess';target.parent.mkdir(parents=True)
        target.symlink_to(self.root/'missing-outside')
        self.assert_cleanup_still_uses_context(family);self.assertTrue(target.is_symlink())

    def test_symlink_ancestor_is_not_treated_as_absent(self):
        family={**self.inventory['Families'][0],'ExportName':'ToyProcess'}
        parent=self.root/'tests/results/spinas-output';parent.symlink_to(self.root/'missing-outside')
        self.assert_cleanup_still_uses_context(family);self.assertTrue(parent.is_symlink())

    def test_file_ancestor_is_not_treated_as_absent(self):
        family={**self.inventory['Families'][0],'ExportName':'ToyProcess'}
        parent=self.root/'tests/results/spinas-output';parent.write_text('manual file retained')
        self.assert_cleanup_still_uses_context(family);self.assertEqual(parent.read_text(),'manual file retained')

    def test_unknown_or_unsafe_export_name_keeps_old_guards(self):
        for name in (None,'../manual','path/name','a-b'):
            with self.subTest(name=name):
                family={**self.inventory['Families'][0],'ExportName':name}
                self.assert_cleanup_still_uses_context(family)

    def test_default_cleans_already_validated_generated_projects(self):
        self.valid_family()
        with mock.patch.object(WORKFLOW, 'context_sha256', return_value=self.context):
            self.invoke_main()
        self.last_cleanup.assert_called_once()
        self.assertNotIn('ALREADY_ABSENT',self.last_output)

    def test_keep_generated_preserves_completed_projects(self):
        self.valid_family()
        with mock.patch.object(WORKFLOW, 'context_sha256', return_value=self.context):
            self.invoke_main('--keep-generated')
        self.last_cleanup.assert_not_called()

    def test_refresh_only_does_not_remove_generated_files(self):
        self.valid_family()
        with mock.patch.object(WORKFLOW, 'context_sha256', return_value=self.context):
            self.invoke_main('--refresh-only')
        self.last_cleanup.assert_not_called()

    def test_cleanup_only_runs_no_physics_workers(self):
        self.valid_family()
        with mock.patch.object(WORKFLOW, 'context_sha256', return_value=self.context), \
                mock.patch.object(WORKFLOW, 'run_worker') as worker:
            self.invoke_main('--cleanup-only')
        self.last_cleanup.assert_called_once()
        worker.assert_not_called()

    def test_deleted_results_do_not_retain_stale_validation_counts(self):
        self.valid_family()
        previous = self.refresh()
        for path in self.results.iterdir():
            if path.is_file(): path.unlink()
        state = WORKFLOW.refresh(self.inventory, previous, self.context)
        self.assert_not_fully_validated(state)
        self.assertEqual(state['Counts']['Constructed'], 0)
        self.assertEqual(state['Cases'], {})
        self.assertEqual(state['Families'], {})

    def test_context_change_before_worker_stops_batch_without_running_it(self):
        calls = [0]
        def fingerprint(configuration):
            calls[0] += 1
            return self.context if calls[0] == 1 else 'modified source inputs'
        with mock.patch.object(WORKFLOW, 'context_sha256', side_effect=fingerprint), \
                mock.patch.object(WORKFLOW, 'run_worker') as worker:
            state = self.invoke_main()
        worker.assert_not_called()
        self.assertIn('Inputs changed', state['StopReason'])
        self.assertEqual(state['Counts']['Constructed'], 0)
        self.assert_not_fully_validated(state)

    def test_context_change_during_worker_rejects_result_and_stops_next_case(self):
        context = [self.context]
        def run(*args):
            summary = self.record('case_a')
            context[0] = 'modified model after kernel started'
            return summary
        with mock.patch.object(WORKFLOW, 'context_sha256', side_effect=lambda configuration: context[0]), \
                mock.patch.object(WORKFLOW, 'run_worker', side_effect=run) as worker:
            state = self.invoke_main()
        self.assertEqual(worker.call_count, 1)
        self.assertIn('Inputs changed', state['StopReason'])
        self.assertEqual(state['Counts']['Constructed'], 0)
        self.assert_not_fully_validated(state)

    def test_resume_reuses_complete_cases_but_still_runs_missing_independent_export(self):
        self.record('case_a')
        self.record('case_b')
        def export(args, kind, identifier, config):
            self.assertEqual((kind, identifier), ('export', 'family_ab'))
            return self.record(identifier, export=True)
        with mock.patch.object(WORKFLOW, 'context_sha256', return_value=self.context), \
                mock.patch.object(WORKFLOW, 'run_worker', side_effect=export) as worker:
            state = self.invoke_main()
        self.assertEqual(worker.call_count, 1)
        self.assertEqual(state['Counts']['FullyValidatedCases'], 2)
        self.assertEqual(state['Counts']['ValidatedFamilies'], 1)

    def test_failed_case_is_retried_explicitly_without_recomputing_good_sibling(self):
        self.record('case_a', Status='FAIL')
        self.record('case_b')
        attempted = []
        def run(args, kind, identifier, config):
            attempted.append((kind, identifier))
            return self.record(identifier, export=(kind == 'export'))
        with mock.patch.object(WORKFLOW, 'context_sha256', return_value=self.context), \
                mock.patch.object(WORKFLOW, 'run_worker', side_effect=run):
            state = self.invoke_main('--retry-failed')
        self.assertEqual(attempted, [('construct', 'case_a'), ('export', 'family_ab')])
        self.assertEqual(state['Counts']['FullyValidatedCases'], 2)

    def test_cli_rejects_nonfinite_deadlines_and_nonpositive_limits(self):
        for option, value in (('--timeout', 'nan'), ('--timeout', 'inf'),
                              ('--timeout', '0'), ('--export-timeout', 'nan'),
                              ('--export-timeout', 'inf'), ('--limit', '0'), ('--limit', '-1')):
            with self.subTest(option=option, value=value), \
                    mock.patch.object(sys, 'argv', ['run.py', '--refresh-only', option, value]), \
                    redirect_stdout(io.StringIO()), redirect_stderr(io.StringIO()):
                with self.assertRaises(SystemExit) as raised:
                    WORKFLOW.main()
                self.assertEqual(raised.exception.code, 2)


class PrecisionReplayInputTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        path = SCRIPT.with_name('replay-calchep-precision.py')
        spec = importlib.util.spec_from_file_location('sm_five_precision_replay', path)
        cls.tool = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(cls.tool)

    def setUp(self):
        temporary = tempfile.TemporaryDirectory(prefix='hepcat-precision-inputs-')
        self.addCleanup(temporary.cleanup)
        self.directory = Path(temporary.name)

    def manifest(self, count=5, **changes):
        value = {'Class': 'SyntheticProcess', 'Masses': [0] * count,
                 'IncomingLegs': [1, 2], 'ParameterOrder': ['coupling'],
                 'CalcHEPValidation': {'Status': 'AVAILABLE', 'Supported': True,
                    'PhysicalPDGs': list(range(1, count + 1)),
                    'LegOrder': list(range(1, count + 1)), 'ParameterNames': ['coupling']}}
        value.update(changes)
        (self.directory / 'manifest.json').write_text(json.dumps(value))
        return value

    def points(self, count=5, rows=2):
        path = self.directory / 'input.csv'
        import csv
        columns = ['point', 'hepcat', 'calchep'] + [f'p{i}_{j}' for i in range(1, count + 1) for j in range(4)]
        with path.open('w', newline='') as stream:
            writer = csv.writer(stream)
            writer.writerow(columns)
            for number in range(1, rows + 1):
                writer.writerow([number, 12345, 67890, *[float(i) + .125 for i in range(4 * count)]])
        return path

    def test_leg_count_and_parameters_come_from_any_supported_export(self):
        for count in (4, 5, 6, 8):
            with self.subTest(count=count):
                self.manifest(count)
                data, config, actual = self.tool.load_manifest(self.directory)
                self.assertEqual(actual, count)
                self.assertEqual(config['ParameterNames'], ['coupling'])
                self.assertEqual(data['Class'], 'SyntheticProcess')

    def test_bad_class_and_inconsistent_particle_counts_are_rejected(self):
        self.manifest(Class='bad();class')
        with self.assertRaises(ValueError):
            self.tool.load_manifest(self.directory)
        value = self.manifest()
        value['CalcHEPValidation']['PhysicalPDGs'].pop()
        (self.directory / 'manifest.json').write_text(json.dumps(value))
        with self.assertRaises(ValueError):
            self.tool.load_manifest(self.directory)

    def test_unsupported_or_polarized_reference_is_not_silently_compared(self):
        value = self.manifest()
        value['CalcHEPValidation']['Supported'] = False
        (self.directory / 'manifest.json').write_text(json.dumps(value))
        with self.assertRaises(ValueError):
            self.tool.load_manifest(self.directory)

    def test_nonstandard_input_order_is_explicitly_outside_replay_support(self):
        self.manifest(IncomingLegs=[2, 4])
        with self.assertRaisesRegex(ValueError, 'incoming legs first'):
            self.tool.load_manifest(self.directory)

    def test_saved_values_are_not_reused_as_the_new_candidate_amplitude(self):
        source = self.points()
        target = self.directory / 'normalized.csv'
        self.assertEqual(self.tool.normalize_points(source, target, 5), 2)
        import csv
        with target.open(newline='') as stream:
            rows = list(csv.DictReader(stream))
        self.assertNotIn('hepcat', rows[0])
        self.assertNotIn('calchep', rows[0])
        self.assertEqual(float(rows[0]['p5_3']), 19.125)

    def test_saved_momenta_must_be_finite_and_have_unique_positive_ids(self):
        for replacement in ('nan', 'inf', '-inf'):
            with self.subTest(value=replacement):
                source = self.points()
                source.write_text(source.read_text().replace(',0.125,', ',' + replacement + ','))
                with self.assertRaisesRegex(ValueError, 'finite'):
                    self.tool.normalize_points(source, self.directory / 'out.csv', 5)
        source = self.points()
        text = source.read_text().splitlines()
        text[2] = text[2].replace('2,', '1,', 1)
        source.write_text('\n'.join(text) + '\n')
        with self.assertRaisesRegex(ValueError, 'distinct positive'):
            self.tool.normalize_points(source, self.directory / 'out.csv', 5)

    def test_incomplete_and_empty_saved_point_sets_are_rejected(self):
        source = self.directory / 'invalid.csv'
        source.write_text('point,p1_0\n1,1\n')
        with self.assertRaises(ValueError):
            self.tool.normalize_points(source, self.directory / 'out.csv', 5)
        source = self.points(rows=0)
        with self.assertRaisesRegex(ValueError, 'empty'):
            self.tool.normalize_points(source, self.directory / 'out.csv', 5)

    def test_generated_sources_require_all_placeholders_resolved(self):
        with self.assertRaises(ValueError):
            self.tool.render('@CLASS@ @OTHER@', {'CLASS': 'Synthetic'})
        self.assertEqual(self.tool.render('@CLASS@', {'CLASS': 'Synthetic'}), 'Synthetic')

    def test_cli_rejects_unbounded_or_invalid_replay_limits(self):
        for option, value in (('--timeout', 'nan'), ('--timeout', 'inf'), ('--timeout', '0'),
                              ('--points', '0'), ('--seed', '-1'), ('--relative-tolerance', 'nan')):
            with self.subTest(option=option, value=value), \
                    mock.patch.object(sys, 'argv', ['replay.py', '.', '--gcc', 'synthetic-gcc', option, value]), \
                    redirect_stderr(io.StringIO()):
                with self.assertRaises(SystemExit) as raised:
                    self.tool.main()
                self.assertEqual(raised.exception.code, 2)


if __name__ == '__main__':
    unittest.main()
