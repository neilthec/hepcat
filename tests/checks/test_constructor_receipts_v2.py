"""Portable synthetic controls for the separate schema-2 receipt reader.

No research artifacts, kernel, physics, registry activation or canonical writes.
Adapter fixtures are explicitly marked test objects, not admitted v2 receipts.
The unchanged schema-1 Fixture is imported rather than copied or modified.
"""
from pathlib import Path
from types import SimpleNamespace
import copy
import hashlib
import importlib.util
import json
import shutil
import sys
import tempfile
import unittest
from unittest.mock import patch

sys.dont_write_bytecode = True
HERE = Path(__file__).resolve().parent
WORKFLOW = HERE.parent / 'SM-5-point'

def load(name, path):
    spec = importlib.util.spec_from_file_location(name, path)
    result = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(result)
    return result

F = load('literal_schema1_fixture_for_v2_tests', HERE / 'test_constructor_receipts.py')
V = load('portable_schema2_receipt_reader', WORKFLOW / 'constructor_receipts_v2.py')
C = F.C
S = F.S

class AdapterTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.f = F.Fixture(self.temp.name)

    def marked(self):
        proof = self.f.proof()
        proof.update(_ScopedV2ReceiptVerified=True, _Here=WORKFLOW)
        return proof

    def test_literal_fixture_and_primitive_are_unchanged(self):
        self.assertEqual(hashlib.sha256((HERE / 'test_constructor_receipts.py').read_bytes()).hexdigest(), V.LEGACY_FIXTURE_SHA)
        self.assertEqual(hashlib.sha256((WORKFLOW / 'constructor_receipts.py').read_bytes()).hexdigest(), V.LEGACY_SHA)
        self.assertIs(Fixture, F.Fixture)

    def test_actual_producer_and_raw_precision_failure_are_preserved(self):
        proof = self.marked()
        native = V.record_evidence(self.f.native_path, self.f.native, self.f.after_context, proof)
        export = V.record_evidence(self.f.export_path, self.f.export, self.f.after_context, proof)
        self.assertEqual(native['ActualProducerContextSHA256'], 'c' * 64)
        self.assertEqual(native['TransitionSchemaVersion'], 2)
        self.assertEqual(export['RecordedStatus'], 'FAIL')
        self.assertEqual(export['ExistingPrecisionQualification']['RawCalcHEPStatus'], 'FAIL')
        self.assertFalse(export['CurrentValidatorPassClaimed'])
        self.assertFalse(export['CurrentConstructorSuccessClaimed'])
        self.assertTrue(native['PriorHistoricalAdmission']['C11MessageStateUnreconstructed'])

    def test_missing_native_blocks_only_it_and_its_dependent_export(self):
        self.f.add_independent_strict_pair()
        proof = self.marked()
        self.f.native_path.with_suffix('.wxf').unlink()
        self.assertIsNone(V.record_evidence(self.f.native_path, self.f.native, self.f.after_context, proof))
        self.assertIsNone(V.record_evidence(self.f.export_path, self.f.export, self.f.after_context, proof))
        self.assertIsNotNone(V.record_evidence(self.f.native_b_path, self.f.native_b, self.f.after_context, proof))
        self.assertIsNotNone(V.record_evidence(self.f.export_b_path, self.f.export_b, self.f.after_context, proof))

    def test_missing_precision_artifact_blocks_only_its_export(self):
        self.f.add_independent_strict_pair()
        proof = self.marked()
        dependency = self.f.root / 'proof/family-only.json'
        dependency.write_text('{}')
        proof['_Records'][self.f.export_path.resolve()]['PrecisionBindings'] = [
            {'Path': str(dependency.relative_to(self.f.root)), 'SHA256': F.R.digest(dependency)}]
        dependency.unlink()
        self.assertIsNone(V.record_evidence(self.f.export_path, self.f.export, self.f.after_context, proof))
        self.assertIsNotNone(V.record_evidence(self.f.native_path, self.f.native, self.f.after_context, proof))
        self.assertIsNotNone(V.record_evidence(self.f.export_b_path, self.f.export_b, self.f.after_context, proof))

    def test_changed_live_summary_and_unknown_member_are_rejected(self):
        proof = self.marked()
        self.f.native_path.write_text('{}')
        self.assertIsNone(V.record_evidence(self.f.native_path, self.f.native, self.f.after_context, proof))
        self.assertIsNone(V.record_evidence(self.f.root / 'unknown.json', self.f.native, self.f.after_context, proof))

    def test_unmarked_or_false_marked_adapter_proof_cannot_pass(self):
        for proof in [None, {}, {'_ScopedV2ReceiptVerified': False}]:
            with self.subTest(proof=proof):
                self.assertIsNone(V.record_evidence(self.f.native_path, self.f.native, self.f.after_context, proof))
                self.assertIsNone(V.public_proof(proof))

    def test_retained_probe_keeps_original_context_and_current_failures(self):
        entry = self.f.make_probe()
        self.f.retain_probe(entry)
        proof = self.marked()
        report = lambda entries: S.case_report('case', self.f.native['CapsuleSHA256'], entries,
            self.f.root / 'tests/results/SM-5-point', self.f.after_context, proof)
        old = report([entry])
        self.assertEqual(old['Status'], 'PASS')
        self.assertEqual(old['Probes'][0]['ProbeContextSHA256'], self.f.before_context)
        self.assertFalse(old['Probes'][0]['CurrentValidatorPassClaimed'])
        unrelated = self.f.make_probe('unrelated', 'newcase', self.f.after_context, 'FAIL')
        self.assertEqual(report([entry, unrelated])['Status'], 'PASS')
        current = self.f.make_probe('current-failure', 'case', self.f.after_context, 'FAIL')
        self.assertEqual(report([entry, current])['Status'], 'FAIL')
        self.assertEqual(report([])['Status'], 'UNAVAILABLE')

    def test_pending_or_replaced_prior_probe_remains_authoritative(self):
        entry = self.f.make_probe()
        self.f.retain_probe(entry)
        proof = self.marked()
        report = lambda entries: S.case_report('case', self.f.native['CapsuleSHA256'], entries,
            self.f.root / 'tests/results/SM-5-point', self.f.after_context, proof)
        pending = self.f.make_probe('pending', 'case', self.f.after_context)
        (self.f.root / 'tests/results/SM-5-point' / pending['Files']['Summary']['Path']).unlink()
        self.assertEqual(report([entry, pending])['Status'], 'UNAVAILABLE')
        changed = copy.deepcopy(entry)
        changed['Status'] = 'CHECKS_FAILED'
        self.assertEqual(report([changed])['Status'], 'UNAVAILABLE')
        replaced = self.f.make_probe('prior', 'case', self.f.after_context, 'FAIL')
        self.assertEqual(report([replaced])['Status'], 'FAIL')

# This is the exact existing class object, with its original source bytes.
Fixture = F.Fixture

class RegistrationTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.here = self.root / 'tests/SM-5-point'
        self.here.mkdir(parents=True)

    def status(self, registry):
        return V.receipt_status(self.root, self.here, {}, 'target', registry=registry, core=C)

    def test_null_and_malformed_registered_entries_block_legacy_fallback(self):
        cases = [
            {'SchemaVersion': 2, 'Receipts': {'target': None}},
            {'SchemaVersion': 2, 'Receipts': {'target': []}},
            {'SchemaVersion': 999, 'Receipts': {'target': {}}},
            {'SchemaVersion': 2, 'Receipts': {'target': {}}, 'extra': True},
            {'SchemaVersion': 1, 'Receipts': {'target': None}}, []]
        for registry in cases:
            with self.subTest(registry=registry):
                value = self.status(registry)
                self.assertIs(value['Registered'], True)
                self.assertIsNone(value['Proof'])

    def test_registration_is_known_before_primitive_validation(self):
        with patch.object(V, 'primitive', side_effect=ValueError('Changed primitive')):
            value = self.status({'SchemaVersion': 2, 'Receipts': {'target': None}})
        self.assertIs(value['Registered'], True)
        self.assertIsNone(value['Proof'])

    def test_absent_unrelated_or_missing_registry_preserves_absence(self):
        for registry in [{'SchemaVersion': 2, 'Receipts': {}},
                         {'SchemaVersion': 2, 'Receipts': {'other': None}},
                         {'SchemaVersion': 1, 'Receipts': {}}]:
            with self.subTest(registry=registry):
                value = self.status(registry)
                self.assertIs(value['Registered'], False)
                self.assertIsNone(value['Proof'])
        self.assertIs(V.receipt_status(self.root, self.here, {}, 'target', core=C)['Registered'], False)

    def test_registered_rejection_cannot_fall_back_to_schema1(self):
        with patch.object(C, 'finite_receipt_v2_status', return_value={'Registered': True, 'Proof': None}), \
             patch.object(C, 'finite_receipt_status', side_effect=AssertionError('Legacy fallback forbidden')):
            self.assertIsNone(C.resolve(self.root, self.here, {}, 'target'))

    def test_absent_versioned_entry_keeps_schema1_resolver_branch(self):
        sentinel = object()
        with patch.object(C, 'finite_receipt_v2_status', return_value={'Registered': False, 'Proof': None}), \
             patch.object(C, 'finite_receipt_status', return_value={'Proof': sentinel}):
            self.assertIs(C.resolve(self.root, self.here, {}, 'target'), sentinel)

    def test_versioned_proof_dispatch_precedes_legacy_marker(self):
        sentinel = object()
        shim = SimpleNamespace(public_proof=lambda *a: sentinel, record_evidence=lambda *a: sentinel)
        with patch.object(C, 'receipt_v2_module', return_value=shim):
            marked = {'_ScopedV2ReceiptVerified': True, '_FiniteConstructorReceiptVerified': True}
            self.assertIs(C.public_proof(marked), sentinel)
            self.assertIs(C.record_evidence('none', {'ContextSHA256': 'old'}, 'new', marked), sentinel)

    def test_explicit_schema1_delegates_to_literal_legacy_verifier(self):
        f = Fixture(self.root / 'legacy')
        target = f.root / 'tests/checks/test_constructor_receipts.py'
        target.parent.mkdir(parents=True)
        shutil.copyfile(HERE / 'test_constructor_receipts.py', target)
        stage = SimpleNamespace(fingerprint=F.R.canonical_digest, manifest=lambda *a: f.after)
        registry = {'SchemaVersion': 1, 'Receipts': f.registry}
        with patch.object(V, 'primitive', return_value=F.R), patch.object(F.R, '_stage', return_value=stage):
            value = V.receipt_status(f.root, f.here, {}, f.after_context, registry=registry, core=C)
        self.assertEqual(value['Status'], 'AVAILABLE_FINITE_HISTORICAL_RETENTION')
        self.assertEqual(value['DelegatedSchemaVersion'], 1)
        self.assertIs(value['Registered'], True)
        self.assertTrue(value['Proof']['_FiniteConstructorReceiptVerified'])
        self.assertNotIn('_ScopedV2ReceiptVerified', value['Proof'])


def synthetic_full_snapshot():
    """Index-only875-record/13-producer fixture; never an admitted receipt."""
    producers = [hashlib.sha256(('synthetic-producer-' + str(i)).encode()).hexdigest() for i in range(12)]
    rows = []
    counts = {}
    for index in range(875):
        native = index < 523
        identifier = ('case' if native else 'family') + str(index if native else index - 523)
        status = 'COMPLETE' if native else ('FAIL' if index < 530 else 'PASS')
        producer = producers[index % 12] if index < 645 else V.BEFORE_CONTEXT
        digest = hashlib.sha256(('synthetic-capsule-' + identifier).encode()).hexdigest()
        raw = {'ID' if native else 'FamilyID': identifier, 'Status': status,
               'ContextSHA256': producer, 'CapsuleSHA256': digest}
        if not native:
            member = rows[(index - 523) % 523]['RawSummary']
            raw.update(CaseIDs=[member['ID']], CaseCapsulesSHA256={member['ID']: member['CapsuleSHA256']},
                       NativeCaseContexts={member['ID']: member['ContextSHA256']})
        precision = {'Status': 'PRECISION_QUALIFIED', 'RawCalcHEPStatus': 'FAIL'} if status == 'FAIL' else None
        prior_history = {'SyntheticOlderConditionalHistory': True} if index < 645 else None
        history = None if producer == V.BEFORE_CONTEXT else {
            'HistoricalEvidenceOnly': True, 'RecordedStatus': status,
            'ActualProducerContextSHA256': producer, 'CurrentContextSHA256': V.BEFORE_CONTEXT,
            **{key: False for key in F.R.FALSE_CLAIMS},
            'PriorHistoricalAdmission': copy.deepcopy(prior_history),
            'ExistingPrecisionQualification': copy.deepcopy(precision)}
        path = 'tests/results/SM-5-point/' + identifier + ('' if native else '-export')
        row = {'Kind': 'Native' if native else 'Export', 'ID': identifier,
               'SummaryPath': path + '.json', 'CapsulePath': path + '.wxf',
               'SummarySHA256': hashlib.sha256(('synthetic-summary-' + identifier).encode()).hexdigest(),
               'CapsuleSHA256': digest, 'RawSummary': raw, 'RecordedStatus': status,
               'ActualProducerContextSHA256': producer, 'AdmittedAtContextSHA256': V.BEFORE_CONTEXT,
               'PriorHistoricalAdmission': history, 'PriorSupplementalEntries': [],
               'ExistingPrecisionQualification': precision}
        if not native:
            row['NativeCaseInputBindings'] = {key: raw.get(key) for key in ('CaseIDs', 'CaseCapsulesSHA256', 'NativeCaseContexts')}
            if precision:
                row.update(PrecisionBindings=[{'Path': 'proof/' + identifier + '.json', 'SHA256': 'a' * 64}],
                           PrecisionExternalBindings=[])
        rows.append(row)
        counts[producer] = counts.get(producer, 0) + 1
    prior = copy.deepcopy(rows[:645])
    for row in prior:
        row['PriorHistoricalAdmission'] = {'SyntheticOlderConditionalHistory': True}
    snapshot = {'SchemaVersion': 2, 'Status': 'UNSEALED_VERIFIED_HISTORICAL_SNAPSHOT',
                'AdmissionContextSHA256': V.BEFORE_CONTEXT, 'Records': rows,
                **dict(zip(('NativeCaseCount', 'ExportCompanionCount', 'ArtifactCount', 'ActualProducerContextCount'), V.COUNTS)),
                **{key: True for key in ('ActualProducerContextsPreserved', 'PriorConditionsPreserved',
                    'Prior645RawRowsPreservedLiterally', 'PerNativePriorProbeEntriesBound',
                    'CurrentNewProbeFailuresRemainAuthoritative', 'CaseAndExportCacheChecksAreLazy',
                    'PrecisionCacheChecksArePerExport', 'PrecisionVerifiersRemainAuthoritative')},
                **{key: False for key in ('NewValidationPassClaimed', 'CanonicalWritesPerformed', 'CapsulesCopied',
                    'UnrelatedNewCaseRegistryAdditionsInvalidateHistoricalCases')},
                'ProducerRecordCounts': counts,
                'QualifiedFamilyIDs': [row['ID'] for row in rows if row['RecordedStatus'] == 'FAIL']}
    return snapshot, {'ArtifactCount': 645, 'Records': prior}

class FiniteInventoryTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.snapshot, self.prior = synthetic_full_snapshot()

    def test_full_synthetic875_and13producers_are_accepted_as_index_only(self):
        self.assertEqual(V.COUNTS, (523, 352, 875, 13))
        index = V.exact_snapshot(F.R, self.root, self.snapshot, self.prior)
        self.assertEqual(len(index), 875)
        self.assertEqual(len({row['ActualProducerContextSHA256'] for row in index.values()}), 13)
        self.assertEqual(sum(row['RecordedStatus'] == 'FAIL' for row in index.values()), 7)

    def test_missing_duplicate_wrong_count_or_producer_are_rejected(self):
        for label in ['missing', 'duplicate', 'count', 'producer']:
            snapshot = copy.deepcopy(self.snapshot)
            if label == 'missing': snapshot['Records'].pop()
            elif label == 'duplicate': snapshot['Records'][-1] = copy.deepcopy(snapshot['Records'][0])
            elif label == 'count': snapshot['ActualProducerContextCount'] += 1
            else: snapshot['ProducerRecordCounts']['foreign'] = 1
            with self.subTest(label=label), self.assertRaises((ValueError, KeyError, TypeError)):
                V.exact_snapshot(F.R, self.root, snapshot, self.prior)

    def test_raw_failure_history_probe_and_dependency_upgrades_are_rejected(self):
        for label in ['raw-failure', 'history', 'prior-row', 'global-pq']:
            snapshot, prior = copy.deepcopy(self.snapshot), copy.deepcopy(self.prior)
            if label == 'raw-failure': snapshot['Records'][523]['RecordedStatus'] = 'PASS'
            elif label == 'history': snapshot['Records'][0]['PriorHistoricalAdmission']['CurrentValidatorPassClaimed'] = True
            elif label == 'prior-row': prior['Records'][0]['RawSummary']['Seconds'] = -1
            else: snapshot['AuxiliaryBindings'] = snapshot['Records'][523]['PrecisionBindings']
            with self.subTest(label=label), self.assertRaises((ValueError, KeyError, TypeError)):
                V.exact_snapshot(F.R, self.root, snapshot, prior)

if __name__ == '__main__':
    unittest.main()
