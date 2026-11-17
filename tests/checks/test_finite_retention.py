"""Portable synthetic checks for the generic finite-history retention adapter.

No kernel, physics calculation, research capsule, registry activation or
canonical write. Adapter fixtures are explicitly marked test objects, not
admitted schema-3 receipts. The existing schema-1 Fixture is imported intact.
"""
from pathlib import Path
import copy
import hashlib
import importlib.util
import json
import shutil
import sys
import tempfile
import unittest
from unittest.mock import patch
from types import SimpleNamespace
import os

sys.dont_write_bytecode = True
HERE = Path(__file__).resolve().parent
WORKFLOW = HERE.parent / 'SM-5-point'

def load(name, path):
    spec = importlib.util.spec_from_file_location(name, path)
    result = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(result)
    return result

F = load('literal_legacy_fixture_for_generic_retention', HERE / 'test_constructor_receipts.py')
V = load('portable_generic_finite_retention', WORKFLOW / 'finite_retention.py')
Fixture = F.Fixture

class AdapterTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.f = Fixture(self.temp.name)

    def marked(self):
        proof = self.f.proof()
        proof.update(_FiniteRetentionV3Verified=True, _Here=WORKFLOW,
            PriorReceipt={"Path": "proof/synthetic-prior-receipt.json", "SHA256": "d" * 64})
        return proof

    def test_existing_fixture_class_is_reused(self):
        self.assertIs(Fixture, F.Fixture)
        self.assertEqual(hashlib.sha256((HERE / 'test_constructor_receipts.py').read_bytes()).hexdigest(),
            '3cb89713c3fe9716d86ddd1c34d4b598f7dad90e5fb8500bc96e0bbb2542a0f9')

    def test_actual_producer_failure_and_prior_conditions_are_preserved(self):
        proof = self.marked()
        native = V.record_evidence(self.f.native_path, self.f.native, self.f.after_context, proof)
        export = V.record_evidence(self.f.export_path, self.f.export, self.f.after_context, proof)
        self.assertEqual(native['ActualProducerContextSHA256'], 'c' * 64)
        self.assertEqual(export['RecordedStatus'], 'FAIL')
        self.assertEqual(export['ExistingPrecisionQualification']['RawCalcHEPStatus'], 'FAIL')
        self.assertFalse(export['CurrentValidatorPassClaimed'])
        self.assertFalse(export['CurrentConstructorSuccessClaimed'])
        self.assertTrue(native['PriorHistoricalAdmission']['C11MessageStateUnreconstructed'])

    def test_deleted_native_cache_affects_only_it_and_its_export(self):
        self.f.add_independent_strict_pair()
        proof = self.marked()
        self.f.native_path.with_suffix('.wxf').unlink()
        self.assertIsNone(V.record_evidence(self.f.native_path, self.f.native, self.f.after_context, proof))
        self.assertIsNone(V.record_evidence(self.f.export_path, self.f.export, self.f.after_context, proof))
        self.assertIsNotNone(V.record_evidence(self.f.native_b_path, self.f.native_b, self.f.after_context, proof))
        self.assertIsNotNone(V.record_evidence(self.f.export_b_path, self.f.export_b, self.f.after_context, proof))

    def test_deleted_precision_cache_does_not_block_native_or_other_family(self):
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

    def test_changed_live_summary_and_unknown_record_are_unavailable(self):
        proof = self.marked()
        self.f.native_path.write_text('{}')
        self.assertIsNone(V.record_evidence(self.f.native_path, self.f.native, self.f.after_context, proof))
        self.assertIsNone(V.record_evidence(self.f.root / 'unknown.json', self.f.native, self.f.after_context, proof))

    def test_unmarked_adapter_object_cannot_claim_admission(self):
        for proof in [None, {}, {'_FiniteRetentionV3Verified': False}]:
            with self.subTest(proof=proof):
                self.assertIsNone(V.record_evidence(self.f.native_path, self.f.native, self.f.after_context, proof))
                self.assertIsNone(V.public_proof(proof))

    def test_retained_probe_keeps_old_context_and_current_failures(self):
        entry = self.f.make_probe()
        self.f.retain_probe(entry)
        proof = self.marked()
        report = lambda entries: F.S.case_report('case', self.f.native['CapsuleSHA256'], entries,
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

    def test_missing_new_probe_and_replaced_prior_probe_stay_authoritative(self):
        entry = self.f.make_probe()
        self.f.retain_probe(entry)
        proof = self.marked()
        report = lambda entries: F.S.case_report('case', self.f.native['CapsuleSHA256'], entries,
            self.f.root / 'tests/results/SM-5-point', self.f.after_context, proof)
        pending = self.f.make_probe('pending', 'case', self.f.after_context)
        (self.f.root / 'tests/results/SM-5-point' / pending['Files']['Summary']['Path']).unlink()
        self.assertEqual(report([entry, pending])['Status'], 'UNAVAILABLE')
        changed = copy.deepcopy(entry)
        changed['Status'] = 'CHECKS_FAILED'
        self.assertEqual(report([changed])['Status'], 'UNAVAILABLE')
        replaced = self.f.make_probe('prior', 'case', self.f.after_context, 'FAIL')
        self.assertEqual(report([replaced])['Status'], 'FAIL')


class RegistrationTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.here = self.root / 'tests/SM-5-point'
        self.here.mkdir(parents=True)

    def status(self, registry):
        return V.receipt_status(self.root, self.here, {}, 'target', registry=registry, core=F.C)

    def test_registered_null_malformed_or_unknown_schema_cannot_pass(self):
        cases = [
            {'SchemaVersion': 3, 'Receipts': {'target': None}},
            {'SchemaVersion': 3, 'Receipts': {'target': []}},
            {'SchemaVersion': 3, 'Receipts': {'target': {}}},
            {'SchemaVersion': 3, 'Receipts': {'target': {}}, 'extra': True},
            {'SchemaVersion': 999, 'Receipts': {'target': {}}},
            {'SchemaVersion': True, 'Receipts': {'target': {}}},
            {'SchemaVersion': 999, 'Receipts': {}},
            {'SchemaVersion': 3, 'Receipts': {}, 'extra': True},
            {'SchemaVersion': True, 'Receipts': {}}, []]
        for registry in cases:
            with self.subTest(registry=registry):
                value = self.status(registry)
                self.assertIs(value['Registered'], True)
                self.assertIsNone(value['Proof'])

    def test_registration_is_known_before_primitive_validation(self):
        with patch.object(V, 'primitive', side_effect=ValueError('Changed primitive')):
            value = self.status({'SchemaVersion': 3, 'Receipts': {'target': None}})
        self.assertIs(value['Registered'], True)
        self.assertIsNone(value['Proof'])

    def test_absent_or_unrelated_current_entry_is_not_fabricated(self):
        for registry in [{'SchemaVersion': 3, 'Receipts': {}},
                         {'SchemaVersion': 3, 'Receipts': {'other': None}}]:
            with self.subTest(registry=registry):
                value = self.status(registry)
                self.assertIs(value['Registered'], False)
                self.assertIsNone(value['Proof'])
        self.assertIs(V.receipt_status(self.root, self.here, {}, 'target', core=F.C)['Registered'], False)

    def test_registered_refusal_cannot_fall_back_to_older_dispatchers(self):
        with patch.object(F.C, 'finite_receipt_v3_status', return_value={'Registered': True, 'Proof': None}), \
             patch.object(F.C, 'finite_receipt_v2_status', side_effect=AssertionError('No v2 fallback')), \
             patch.object(F.C, 'finite_receipt_status', side_effect=AssertionError('No v1 fallback')):
            self.assertIsNone(F.C.resolve(self.root, self.here, {}, 'target'))

    def test_absent_v3_entry_preserves_explicit_older_dispatch(self):
        sentinel = object()
        with patch.object(F.C, 'finite_receipt_v3_status', return_value={'Registered': False, 'Proof': None}), \
             patch.object(F.C, 'finite_receipt_v2_status', return_value={'Registered': True, 'Proof': sentinel}):
            self.assertIs(F.C.resolve(self.root, self.here, {}, 'target'), sentinel)

    def test_v3_verified_marker_precedes_both_older_markers(self):
        sentinel = object()
        shim = SimpleNamespace(public_proof=lambda *a: sentinel, record_evidence=lambda *a: sentinel)
        marked = {'_FiniteRetentionV3Verified': True, '_ScopedV2ReceiptVerified': True,
                  '_FiniteConstructorReceiptVerified': True}
        with patch.object(F.C, 'receipt_v3_module', return_value=shim):
            self.assertIs(F.C.public_proof(marked), sentinel)
            self.assertIs(F.C.record_evidence('none', {'ContextSHA256': 'old'}, 'new', marked), sentinel)

    def test_explicit_schema1_uses_the_unchanged_legacy_reader(self):
        fixture = Fixture(self.root / 'legacy')
        target = fixture.root / 'tests/checks/test_constructor_receipts.py'
        target.parent.mkdir(parents=True)
        shutil.copy2(HERE / 'test_constructor_receipts.py', target)
        stage = SimpleNamespace(fingerprint=F.R.canonical_digest, manifest=lambda *a: fixture.after)
        registry = {'SchemaVersion': 1, 'Receipts': fixture.registry}
        with patch.object(V, 'primitive', return_value=F.R), patch.object(F.R, '_stage', return_value=stage):
            value = V.receipt_status(fixture.root, fixture.here, {}, fixture.after_context,
                                     registry=registry, core=F.C)
        self.assertTrue(value['Registered'])
        self.assertEqual(value['DelegatedSchemaVersion'], 1)
        self.assertTrue(value['Proof']['_FiniteConstructorReceiptVerified'])
        self.assertNotIn('_FiniteRetentionV3Verified', value['Proof'])

    def test_explicit_schema2_delegates_without_upgrade(self):
        fixture = Fixture(self.root / 'version2')
        target = fixture.root / 'tests/checks/test_constructor_receipts.py'
        target.parent.mkdir(parents=True)
        shutil.copy2(HERE / 'test_constructor_receipts.py', target)
        registry = {'SchemaVersion': 2, 'Receipts': {fixture.after_context: None}}
        calls = []
        def old_reader(*args, **kwargs):
            calls.append((args, kwargs))
            return {'Status': 'SYNTHETIC_DELEGATION_ONLY', 'Registered': True, 'Proof': None}
        with patch.object(V, 'primitive', return_value=F.R), \
             patch.object(V, 'version2', return_value=SimpleNamespace(receipt_status=old_reader)):
            value = V.receipt_status(fixture.root, fixture.here, {}, fixture.after_context,
                                     registry=registry, core=F.C)
        self.assertEqual(value['DelegatedSchemaVersion'], 2)
        self.assertEqual(len(calls), 1)
        self.assertEqual(calls[0][1]['registry'], registry)
        self.assertIsNone(value['Proof'])


def synthetic_snapshot(native_count=3, export_count=2, producer_count=2, prior_count=1, qualified_count=1):
    """Index-only data, with no real capsules or admitted receipt."""
    before = hashlib.sha256(b'synthetic-before-context').hexdigest()
    producers = [hashlib.sha256(('synthetic-producer-' + str(i)).encode()).hexdigest()
                 for i in range(producer_count)]
    rows = []
    counts = {}
    for index in range(native_count + export_count):
        native = index < native_count
        identifier = ('case' + str(index)) if native else ('family' + str(index - native_count))
        status = 'COMPLETE' if native else ('FAIL' if index - native_count < qualified_count else 'PASS')
        producer = producers[index % producer_count]
        digest = hashlib.sha256(('synthetic-capsule-' + identifier).encode()).hexdigest()
        raw = {'ID' if native else 'FamilyID': identifier, 'Status': status,
               'ContextSHA256': producer, 'CapsuleSHA256': digest}
        if not native:
            member = rows[(index - native_count) % native_count]['RawSummary']
            raw.update(CaseIDs=[member['ID']], CaseCapsulesSHA256={member['ID']: member['CapsuleSHA256']},
                       NativeCaseContexts={member['ID']: member['ContextSHA256']})
        precision = {'Status': 'PRECISION_QUALIFIED', 'RawCalcHEPStatus': 'FAIL'} if status == 'FAIL' else None
        history = {'HistoricalEvidenceOnly': True, 'RecordedStatus': status,
                   'ActualProducerContextSHA256': producer, 'CurrentContextSHA256': before,
                   **{key: False for key in F.R.FALSE_CLAIMS}}
        path = 'tests/results/SM-5-point/' + identifier + ('' if native else '-export')
        row = {'Kind': 'Native' if native else 'Export', 'ID': identifier,
               'SummaryPath': path + '.json', 'CapsulePath': path + '.wxf',
               'SummarySHA256': hashlib.sha256(('summary-' + identifier).encode()).hexdigest(),
               'CapsuleSHA256': digest, 'RawSummary': raw, 'RecordedStatus': status,
               'ActualProducerContextSHA256': producer, 'AdmittedAtContextSHA256': before,
               'PriorHistoricalAdmission': history, 'PriorSupplementalEntries': [],
               'ExistingPrecisionQualification': precision}
        if not native:
            row['NativeCaseInputBindings'] = {key: raw[key] for key in
                ('CaseIDs', 'CaseCapsulesSHA256', 'NativeCaseContexts')}
            if precision:
                row.update(PrecisionBindings=[{'Path': 'proof/family-only.json', 'SHA256': 'b' * 64}],
                           PrecisionExternalBindings=[])
        rows.append(row)
        counts[producer] = counts.get(producer, 0) + 1
    prior = copy.deepcopy(rows[:prior_count])
    for current, old in zip(rows, prior):
        old['PriorHistoricalAdmission'] = {'C11MessageStateUnreconstructed': True}
        current['PriorHistoricalAdmission']['PriorHistoricalAdmission'] = copy.deepcopy(old['PriorHistoricalAdmission'])
        current['PriorHistoricalAdmission']['ExistingPrecisionQualification'] = copy.deepcopy(old['ExistingPrecisionQualification'])
    qualified = sorted(r['ID'] for r in rows if r['Kind'] == 'Export' and r['RecordedStatus'] == 'FAIL')
    spec = {'Counts': {'Native': native_count, 'Export': export_count,
                      'Total': len(rows), 'ActualProducers': len(counts)},
            'ProducerRecordCounts': counts, 'QualifiedFamilyIDs': qualified,
            'PriorRecordCount': prior_count, 'PriorSchemaVersion': 2,
            'PriorReceipt': {'Path': 'proof/prior.json', 'SHA256': 'c' * 64},
            'PriorSnapshot': {'Path': 'proof/prior-snapshot.json', 'SHA256': 'd' * 64}}
    snapshot = {'SchemaVersion': 3, 'Status': 'UNSEALED_VERIFIED_HISTORICAL_SNAPSHOT',
                'AdmissionContextSHA256': before, 'NativeCaseCount': native_count,
                'ExportCompanionCount': export_count, 'ArtifactCount': len(rows),
                'ActualProducerContextCount': len(counts), 'Records': rows,
                'ProducerRecordCounts': counts, 'QualifiedFamilyIDs': qualified,
                'PriorReceipt': spec['PriorReceipt'], 'PriorRecordSnapshot': spec['PriorSnapshot'],
                **{key: True for key in ('ActualProducerContextsPreserved', 'PriorConditionsPreserved',
                    'PerNativePriorProbeEntriesBound', 'CurrentNewProbeFailuresRemainAuthoritative',
                    'CaseAndExportCacheChecksAreLazy', 'PrecisionCacheChecksArePerExport',
                    'PrecisionVerifiersRemainAuthoritative', 'Prior' + str(prior_count) + 'RawRowsPreservedLiterally')},
                **{key: False for key in ('NewValidationPassClaimed', 'CanonicalWritesPerformed',
                    'CapsulesCopied', 'UnrelatedNewCaseRegistryAdditionsInvalidateHistoricalCases')}}
    return snapshot, {'ArtifactCount': prior_count, 'Records': prior}, {
        'BeforeContextSHA256': before, 'Snapshot': spec}


class IndexTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)

    def verify(self, snapshot, prior, policy):
        return V.exact_snapshot(F.R, self.root, snapshot, prior, policy)

    def test_counts_and_producers_come_from_the_finite_policy(self):
        for native, export, producers, prior in [(1, 0, 1, 0), (3, 2, 2, 1),
                                                (7, 4, 3, 4), (542, 363, 14, 875)]:
            with self.subTest(native=native, export=export, producers=producers):
                snapshot, old, policy = synthetic_snapshot(native, export, producers, prior)
                index = self.verify(snapshot, old, policy)
                self.assertEqual(len(index), native + export)
                self.assertFalse(any(self.root.rglob('*.wxf')))

    def test_synthetic905_preserves542_native363_export14_producers_and7_raw_failures(self):
        snapshot, prior, policy = synthetic_snapshot(542, 363, 14, 875, 7)
        index = self.verify(snapshot, prior, policy)
        self.assertEqual(len(index), 905)
        self.assertEqual(sum(r['Kind'] == 'Native' for r in index.values()), 542)
        self.assertEqual(sum(r['Kind'] == 'Export' for r in index.values()), 363)
        self.assertEqual(len(snapshot['ProducerRecordCounts']), 14)
        failed = [r for r in index.values() if r['RecordedStatus'] == 'FAIL']
        self.assertEqual(len(failed), 7)
        self.assertTrue(all(r['ExistingPrecisionQualification']['RawCalcHEPStatus'] == 'FAIL' for r in failed))
        self.assertFalse(any(self.root.rglob('*.wxf')))

    def test_duplicate_identity_changed_path_or_boolean_count_is_rejected(self):
        for mutation in [lambda s: s['Records'].__setitem__(1, copy.deepcopy(s['Records'][0])),
                         lambda s: s['Records'][0].update(SummaryPath='../case.json'),
                         lambda s: s.update(NativeCaseCount=True),
                         lambda s: s.update(SchemaVersion=True)]:
            snapshot, prior, policy = synthetic_snapshot()
            mutation(snapshot)
            with self.assertRaises((ValueError, KeyError, TypeError)):
                self.verify(snapshot, prior, policy)

    def test_prior_raw_history_and_precision_failures_cannot_be_upgraded(self):
        mutations = [lambda s: s['Records'][0]['RawSummary'].update(Status='FAIL'),
                     lambda s: s['Records'][0]['PriorHistoricalAdmission'].update(PriorHistoricalAdmission=None),
                     lambda s: s['Records'][3].update(ExistingPrecisionQualification=None),
                     lambda s: s.update(NewValidationPassClaimed=True),
                     lambda s: s.update(AuxiliaryBindings=[{'Path': 'live-pq-cache'}])]
        for mutation in mutations:
            snapshot, prior, policy = synthetic_snapshot()
            mutation(snapshot)
            with self.assertRaises((ValueError, KeyError, TypeError)):
                self.verify(snapshot, prior, policy)

    def test_missing_or_duplicate_prior_rows_and_changed_producer_distribution_are_rejected(self):
        for kind in ['missing', 'duplicate', 'producer']:
            snapshot, prior, policy = synthetic_snapshot(prior_count=2)
            if kind == 'missing':
                prior['Records'].pop()
            elif kind == 'duplicate':
                prior['Records'][1] = copy.deepcopy(prior['Records'][0])
            else:
                snapshot['ProducerRecordCounts'] = {'e' * 64: len(snapshot['Records'])}
            with self.assertRaises((ValueError, KeyError, TypeError)):
                self.verify(snapshot, prior, policy)


class PolicyFixture:
    """Synthetic cold tables; not an actual Root capture or a physics proof."""
    def __init__(self, root):
        self.root = Path(root)
        self.usage = self.root / 'Documentation/English/SymbolUsage.wl'
        self.usage.parent.mkdir(parents=True)
        self.usage.write_text('(* synthetic unchanged usage *)')
        self.helper = V.P + 'syntheticConstructorHelper'
        self.checker = 'ConstructiveDiagrams`CheckAmplitudes'
        self.public = 'ConstructiveDiagrams`ConstructAmplitude'
        self.before = {'Files': {'source/internal/HEPCAT-MixedNullPublic.wl': 'a' * 64,
            'source/HEPCAT-Base.wl': 'c' * 64, 'models/vars1.mdl': 'd' * 64,
            'numerical-library.so': 'e' * 64}, 'Programs': {'Kernel': 'Synthetic'}}
        self.after = copy.deepcopy(self.before)
        self.after['Files']['source/internal/HEPCAT-MixedNullPublic.wl'] = 'b' * 64
        self.old = {name: {field: '{}' for field in V.FIELDS} for name in
            [self.helper, self.checker, self.public, V.LOADER, V.SIGNATURE]}
        self.old[self.helper]['DownValues'] = '{beforeHelper}'
        self.old[V.LOADER]['OwnValues'] = '{HoldPattern[' + V.LOADER + '] :> 1}'
        self.old[V.SIGNATURE]['DownValues'] = '{unchangedSyntheticLoaderFormula}'
        self.new = copy.deepcopy(self.old)
        self.new[self.helper]['DownValues'] = '{afterHelper}'
        self.new[V.LOADER]['OwnValues'] = '{HoldPattern[' + V.LOADER + '] :> 2}'
        self.patterns = list(F.R.CAPTURE_PATTERNS)
        self.raw = lambda definitions: {'PackageLoadedOnce': True, 'ModelsRead': [],
            'PhysicsCalculationsPerformed': False, 'WolframVersion': 'Synthetic', 'SystemID': 'Synthetic',
            'CapturedNamePatterns': self.patterns, 'Definitions': definitions, 'SymbolCount': len(definitions)}
        self.left = [self.raw(copy.deepcopy(self.old)) for _ in range(2)]
        self.right = [self.raw(copy.deepcopy(self.new)) for _ in range(2)]
        _, _, index_policy = synthetic_snapshot()
        self.policy = {'SchemaVersion': 3, 'Kind': V.KIND,
            'BeforeManifest': self.before, 'AfterManifest': self.after,
            'BeforeContextSHA256': V.canonical(self.before), 'AfterContextSHA256': V.canonical(self.after),
            'ChangedSourcePaths': ['source/internal/HEPCAT-MixedNullPublic.wl'], 'AddedSourcePaths': [],
            'NamespacePatterns': self.patterns, 'WolframVersion': 'Synthetic', 'SystemID': 'Synthetic',
            'BeforeDefinitionsSHA256': V.canonical(self.old), 'AfterDefinitionsSHA256': V.canonical(self.new),
            'ReviewedExistingFields': {self.helper: ['DownValues'], V.LOADER: ['OwnValues']},
            'ReviewedFieldTables': {n: {'Before': self.old[n], 'After': self.new[n]} for n in [self.helper, V.LOADER]},
            'MechanicalSourceInventories': {},
            'NewSymbolInventorySHA256': V.canonical({}),
            'LoadingInputs': {side: {V.USAGE: {'Path': str(self.usage), 'SHA256': V.digest(self.usage)}}
                              for side in ['Before', 'After']},
            'CaptureSourceDirectories': [str(self.root / 'source')] * 2,
            'Loader': {'RequiredNames': ['HEPCAT-Base.wl'], 'OptionalNames': [],
                       'ScriptText': 'synthetic not executed', 'Sides': {}},
            'Snapshot': index_policy['Snapshot'],
            'VerifierTools': {'tests/SM-5-point/finite_retention.py': 'f' * 64}}

    def inspect(self):
        return V.inspect_transition(F.C, self.policy, V.canonical(self.policy), self.before,
            self.after, self.left, self.right, {}, installed_root_captures=True)


class PolicyTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.f = PolicyFixture(self.temp.name)

    def test_exact_constructor_only_ten_field_delta_is_inspected_not_admitted(self):
        result, _, _ = self.f.inspect()
        self.assertEqual(result['Status'], 'EXACT_POLICY_DELTA_INSPECTED_NOT_SEALED')
        self.assertTrue(result['NoAdmission'])
        self.assertEqual(result['ChangedExistingFields'], self.f.policy['ReviewedExistingFields'])

    def test_boolean_schema_and_finite_counts_are_not_integers(self):
        for field in ['SchemaVersion', 'Native', 'PriorRecordCount']:
            policy = copy.deepcopy(self.f.policy)
            if field == 'SchemaVersion':
                policy[field] = True
            elif field == 'Native':
                policy['Snapshot']['Counts'][field] = True
            else:
                policy['Snapshot'][field] = True
            with self.assertRaises(ValueError):
                V.validate_policy(policy, V.canonical(policy))

    def test_changed_policy_pin_model_program_or_runtime_cannot_pass(self):
        with self.assertRaises(ValueError):
            V.validate_policy(self.f.policy, '0' * 64)
        for key in ['models/vars1.mdl', 'numerical-library.so']:
            policy = copy.deepcopy(self.f.policy)
            policy['AfterManifest']['Files'][key] = 'f' * 64
            with self.assertRaises(ValueError):
                V.validate_policy(policy, V.canonical(policy))
        policy = copy.deepcopy(self.f.policy)
        policy['AfterManifest']['Programs']['Kernel'] = 'Changed'
        with self.assertRaises(ValueError):
            V.validate_policy(policy, V.canonical(policy))

    def test_public_checker_signature_and_unreviewed_field_changes_rejected(self):
        for name, field in [(self.f.public, 'DownValues'), (self.f.checker, 'DownValues'),
                            (V.SIGNATURE, 'DownValues'), (self.f.helper, 'Options')]:
            self.setUp_fixture_for_subtest()
            self.f.new[name][field] = '{changed}'
            self.f.right = [self.f.raw(copy.deepcopy(self.f.new)) for _ in range(2)]
            self.f.policy['AfterDefinitionsSHA256'] = V.canonical(self.f.new)
            with self.assertRaises(ValueError):
                self.f.inspect()

    def setUp_fixture_for_subtest(self):
        directory = tempfile.TemporaryDirectory()
        self.addCleanup(directory.cleanup)
        self.f = PolicyFixture(directory.name)

    def test_all_ten_fields_and_literal_usage_must_remain_bound(self):
        policy = copy.deepcopy(self.f.policy)
        del policy['ReviewedFieldTables'][self.f.helper]['After']['Messages']
        with self.assertRaises(ValueError):
            V.validate_policy(policy, V.canonical(policy))
        self.f.usage.write_text('changed literal usage')
        with self.assertRaises(ValueError):
            self.f.inspect()

    def test_namespace_model_reads_and_joint_platform_changes_rejected(self):
        for mutation in ['namespace', 'model', 'version', 'platform']:
            self.setUp_fixture_for_subtest()
            for row in self.f.left + self.f.right:
                if mutation == 'namespace':
                    row['CapturedNamePatterns'] = ['Missing`*']
                elif mutation == 'model':
                    row['ModelsRead'] = ['model1']
                elif mutation == 'version':
                    row['WolframVersion'] = 'Other'
                else:
                    row['SystemID'] = 'Other'
            with self.assertRaises(ValueError):
                self.f.inspect()

    def test_private_capture_directories_cannot_claim_installed_root(self):
        self.f.policy['CaptureSourceDirectories'][1] = str(self.f.root / 'private-source')
        with self.assertRaises(ValueError):
            self.f.inspect()

    def test_loader_rejects_boolean_record_index_and_incomplete_bundle(self):
        with self.assertRaises((ValueError, KeyError)):
            V.verify_loader(F.R, self.f.root, self.f.policy, self.f.before, self.f.after,
                self.f.old, self.f.new, lambda binding: None)


class HistoryFixture:
    """Two immutable byte versions at one logical path; no source is executed."""
    def __init__(self, root, schema=2):
        self.root = Path(root).resolve()
        self.prefix = self.root / 'new-archive'
        self.prefix.mkdir(parents=True)
        self.logical = self.root / 'tests/SM-5-point/example-reader.py'
        self.logical.parent.mkdir(parents=True)
        self.logical.write_text('new source bytes, not executed')
        self.old = self.root / 'prior-archive/example-reader.py'
        self.old.parent.mkdir(parents=True)
        self.old.write_text('old source bytes, not executed')
        self.old_sha = V.digest(self.old)
        def put(name, value):
            path = self.root / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(json.dumps(value))
            return {'Path': name, 'SHA256': V.digest(path)}
        self.prior_map = put('proof/prior-map.json', {'CanonicalRoot': str(self.root),
            'OriginalToArchivedFiles': {str(self.logical): {
                'ArchivedRelativePath': 'example-reader.py', 'SHA256': self.old_sha}}})
        snapshot = put('proof/prior-snapshot.json', {'ArtifactCount': 0, 'Records': []})
        self.prior = put('proof/prior.json', {'SchemaVersion': schema,
            'Status': 'SYNTHETIC_HISTORY_ONLY_NOT_SEALED', 'RootMapping': self.prior_map,
            'Snapshot': snapshot, 'ArtifactPrefix': 'prior-archive',
            'VerifierTools': [{'Path': str(self.logical), 'SHA256': self.old_sha}]})
        copies = {}
        for relative in ['proof/prior.json', 'proof/prior-map.json', 'proof/prior-snapshot.json',
                         'prior-archive/example-reader.py']:
            source = self.root / relative
            target = self.prefix / relative
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(source, target)
            copies[str(source)] = {'ArchivedRelativePath': relative, 'SHA256': V.digest(target)}
        self.receipt_copy = self.prefix / 'proof/prior.json'
        self.mapping = {'CanonicalRoot': str(self.root), 'NoMutableVerifierAliases': True,
            'NoPrivateToInstalledAdmissionMapping': True, 'OriginalToArchivedFiles': copies,
            'HashReferencesOnly': {}, 'VersionedHistoricalBindingResolutions': [{
                'OriginalLogicalPath': str(self.logical), 'SHA256': self.old_sha,
                'ActualPreimagePath': str(self.old), 'PriorReceiptSHA256': self.prior['SHA256'],
                'PriorRootMapping': self.prior_map}]}

    def resolve(self):
        return V.history_resolver(F.R, self.root, self.prefix, self.mapping, self.prior)


class HistoryTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.f = HistoryFixture(self.temp.name)

    def test_exact_historical_path_and_sha_version_is_retained_for_prior2_or3(self):
        for schema in [2, 3]:
            with tempfile.TemporaryDirectory() as directory:
                fixture = HistoryFixture(directory, schema)
                resolver, prior, _ = fixture.resolve()
                path = resolver({'Path': str(fixture.logical), 'SHA256': fixture.old_sha})
                self.assertEqual(V.digest(path), fixture.old_sha)
                self.assertNotEqual(V.digest(fixture.logical), fixture.old_sha)
                self.assertEqual(prior['SchemaVersion'], schema)

    def test_wrong_sha_or_rebound_actual_preimage_is_rejected(self):
        resolver, _, _ = self.f.resolve()
        with self.assertRaises(ValueError):
            resolver({'Path': str(self.f.logical), 'SHA256': 'f' * 64})
        self.f.mapping['VersionedHistoricalBindingResolutions'][0]['ActualPreimagePath'] = str(self.f.logical)
        with self.assertRaises(ValueError):
            self.f.resolve()

    def test_historical_symlink_and_hardlink_aliases_are_rejected(self):
        for kind in ['symlink', 'hardlink']:
            with tempfile.TemporaryDirectory() as directory:
                fixture = HistoryFixture(directory)
                fixture.receipt_copy.unlink()
                original = fixture.root / fixture.prior['Path']
                if kind == 'symlink':
                    fixture.receipt_copy.symlink_to(original)
                else:
                    os.link(original, fixture.receipt_copy)
                with self.assertRaises(ValueError):
                    fixture.resolve()

    def test_mutation_during_historical_json_readback_is_rejected(self):
        original = Path.read_text
        target = self.f.receipt_copy.resolve()
        def mutate(path, *args, **kwargs):
            result = original(path, *args, **kwargs)
            if path.resolve() == target:
                path.write_text(result + '\n')
            return result
        with patch.object(Path, 'read_text', mutate), self.assertRaises(ValueError):
            self.f.resolve()

    def test_recursive_index_preserves_prior_schema2_and3_conditions(self):
        for schema in [2, 3]:
            snapshot, prior, policy = synthetic_snapshot(4, 2, 2, 3)
            policy['Snapshot']['PriorSchemaVersion'] = schema
            index = V.exact_snapshot(F.R, Path(self.temp.name), snapshot, prior, policy)
            self.assertEqual(len(index), 6)
            for row in list(index.values())[:3]:
                self.assertTrue(row['PriorHistoricalAdmission']['PriorHistoricalAdmission']['C11MessageStateUnreconstructed'])


class LoaderFixture:
    """Tiny real files and mocked capture records; no Wolfram execution."""
    def __init__(self, root):
        self.root = Path(root).resolve()
        self.names = ['HEPCAT.wl', 'HEPCAT-Base.wl', 'HEPCAT-Model.wl'] + [
            'internal/input' + str(i) + '.wl' for i in range(18)]
        self.required, self.optional = self.names[:17], self.names[17:]
        self.old_paths = {}
        self.before = {'Files': {}, 'Programs': {}}
        for name in self.names:
            path = self.root / 'source' / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text('before bytes: ' + name)
            old = self.root / 'archive' / name
            old.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(path, old)
            self.old_paths[(str(path), V.digest(path))] = old
            self.before['Files']['source/' + name] = V.digest(path)
        changed = self.root / 'source' / self.names[-1]
        changed.write_text('after bytes')
        self.after = {'Files': {'source/' + n: V.digest(self.root / 'source' / n) for n in self.names}, 'Programs': {}}
        text = lambda names: '{' + ', '.join('"' + n + '"' for n in names) + '}'
        formula = '{syntheticSignature[' + text(self.required) + ', ' + text(self.optional) + ']}'
        self.old = {V.SIGNATURE: {'DownValues': formula}, V.LOADER: {
            'OwnValues': '{HoldPattern[' + V.LOADER + '] :> 1}'}}
        self.new = {V.SIGNATURE: {'DownValues': formula}, V.LOADER: {
            'OwnValues': '{HoldPattern[' + V.LOADER + '] :> 2}'}}
        script = self.root / 'proof/bare.wl'
        script.parent.mkdir(parents=True)
        script.write_text('synthetic collector never executed')
        def put(path, value):
            path.write_text(json.dumps(value))
            return {'Path': str(path.relative_to(self.root)), 'SHA256': V.digest(path)}
        sides = {}
        self.raws = {}
        self.controllers = {}
        for side, manifest, value in [('Before', self.before, 1), ('After', self.after, 2)]:
            paths = [self.resolve({'Path': str(self.root / 'source' / n),
                'SHA256': manifest['Files']['source/' + n]}) if side == 'Before'
                else self.root / 'source' / n for n in self.names]
            row = {'Status': 'INDEPENDENT_BARE_LOADER_RECOMPUTATION',
                'SourceDirectory': str(self.root / 'source'), 'NoModelOrPhysics': True,
                'WolframVersion': 'Synthetic', 'SystemID': 'Synthetic',
                'RequiredNames': self.required, 'OptionalNames': self.optional, 'InputCount': 21,
                'SourceSHA256': {n: manifest['Files']['source/' + n] for n in self.names},
                'BareInputsInputForm': '{' + ', '.join(str(int(hashlib.md5(q.read_bytes()).hexdigest(), 16)) for q in paths) + '}',
                'Bare': value, 'Loaded': value, 'BareMatchesLoaded': True, 'BareMatchesFunction': True,
                'ActualFunctionDownValues': formula}
            raw_path = self.root / ('proof/raw-' + side + '.json')
            raw_binding = put(raw_path, row)
            record = {'Status': 'CAPTURED', 'Pass': 1, 'RawSHA256': raw_binding['SHA256'],
                'RawFile': str(raw_path), 'KernelExitCode': 0, 'Bound': None, 'PinsUnchanged': True,
                'OwnedCleanup': {'Status': 'COMPLETE', 'AllObservedOwnedGroupsGone': True,
                    'AllOwnedPopenLeadersReaped': True, 'CleanupDeadlineSatisfied': True, 'InspectionFailed': False}}
            inputs = {str(self.root / 'source' / n): manifest['Files']['source/' + n] for n in self.names}
            inputs[str(script)] = V.digest(script)
            controller = {'Status': 'BARE_LOADER_INPUTS_VERIFIED_NOT_SEALED', 'Inputs': inputs, 'Records': [record]}
            control_path = self.root / ('proof/controller-' + side + '.json')
            sides[side] = {'Controller': put(control_path, controller), 'Raw': raw_binding,
                'Script': {'Path': str(script.relative_to(self.root)), 'SHA256': V.digest(script)}, 'RecordIndex': 0}
            self.raws[side] = row
            self.controllers[side] = controller
        self.policy = {'WolframVersion': 'Synthetic', 'SystemID': 'Synthetic', 'Loader': {
            'RequiredNames': self.required, 'OptionalNames': self.optional,
            'ScriptText': script.read_text(), 'Sides': sides}}

    def resolve(self, binding):
        return self.old_paths.get((binding['Path'], binding['SHA256']), Path(binding['Path']))

    def verify(self):
        return V.verify_loader(F.R, self.root, self.policy, self.before, self.after, self.old, self.new, self.resolve)

    def repin_raw(self, side):
        binding = self.policy['Loader']['Sides'][side]['Raw']
        path = self.root / binding['Path']
        path.write_text(json.dumps(self.raws[side]))
        binding['SHA256'] = V.digest(path)
        controller = self.controllers[side]
        controller['Records'][0]['RawSHA256'] = binding['SHA256']
        control_binding = self.policy['Loader']['Sides'][side]['Controller']
        path = self.root / control_binding['Path']
        path.write_text(json.dumps(controller))
        control_binding['SHA256'] = V.digest(path)


class LoaderTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.f = LoaderFixture(self.temp.name)

    def test_ordered21_inputs_and_literal_formula_are_bound_without_a_kernel(self):
        result = self.f.verify()
        self.assertEqual(result['Before']['InputCount'], 21)
        self.assertEqual(result['After']['InputCount'], 21)
        self.assertNotEqual(result['Before']['Value'], result['After']['Value'])

    def test_changed_count_platform_root_or_source_inventory_is_rejected(self):
        for key, value in [('InputCount', True), ('InputCount', 20), ('SystemID', 'Other'),
                           ('SourceDirectory', str(Path(self.temp.name) / 'private'))]:
            with tempfile.TemporaryDirectory() as directory:
                fixture = LoaderFixture(directory)
                fixture.raws['After'][key] = value
                fixture.repin_raw('After')
                with self.assertRaises(ValueError):
                    fixture.verify()

    def test_changed_ordered_byte_input_or_loaded_value_is_rejected(self):
        for key, value in [('BareInputsInputForm', '{1}'), ('Bare', 999), ('BareMatchesFunction', False)]:
            with tempfile.TemporaryDirectory() as directory:
                fixture = LoaderFixture(directory)
                fixture.raws['After'][key] = value
                fixture.repin_raw('After')
                with self.assertRaises(ValueError):
                    fixture.verify()

    def test_boolean_record_index_or_signature_formula_change_is_rejected(self):
        self.f.policy['Loader']['Sides']['Before']['RecordIndex'] = True
        with self.assertRaises(ValueError):
            self.f.verify()
        self.f.policy['Loader']['Sides']['Before']['RecordIndex'] = 0
        self.f.new[V.SIGNATURE]['DownValues'] += 'changed'
        with self.assertRaises(ValueError):
            self.f.verify()



class MechanicalInventoryTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.f = PolicyFixture(self.temp.name)
        self.name = 'HEPCATMinimalQCDCurrentSources`Private`$syntheticLiveCodeFiles'
        self.paths = sorted(n for n in self.f.before['Files'] if n.startswith('source/'))
        self.f.policy['MechanicalSourceInventories'] = {self.name: {
            'OrderedPaths': self.paths, 'AdditionalSourceFiles': {}}}
        for table, manifest in [(self.f.old, self.f.before), (self.f.new, self.f.after)]:
            fields = {field: '{}' for field in V.FIELDS}
            source = Path(self.f.policy['CaptureSourceDirectories'][0])
            entries = ', '.join(json.dumps(str(source.parent / n)) + ' -> ' +
                json.dumps(manifest['Files'][n]) for n in self.paths)
            fields['OwnValues'] = '{HoldPattern[' + self.name + '] :> <|' + entries + '|>}'
            table[self.name] = fields
        self.f.policy['ReviewedExistingFields'][self.name] = ['OwnValues']
        self.repin()

    def repin(self):
        self.f.left = [self.f.raw(copy.deepcopy(self.f.old)) for _ in range(2)]
        self.f.right = [self.f.raw(copy.deepcopy(self.f.new)) for _ in range(2)]
        self.f.policy['BeforeDefinitionsSHA256'] = V.canonical(self.f.old)
        self.f.policy['AfterDefinitionsSHA256'] = V.canonical(self.f.new)
        self.f.policy['ReviewedFieldTables'][self.name] = {
            'Before': self.f.old[self.name], 'After': self.f.new[self.name]}

    def test_exact_full_source_inventory_ownvalue_is_mechanical_only(self):
        result, _, _ = self.f.inspect()
        self.assertEqual(result['ChangedExistingFields'][self.name], ['OwnValues'])
        self.assertTrue(result['NoAdmission'])
        self.assertEqual(result['PrivatePathArtifacts'], {})
        self.assertTrue(all(self.f.old[self.name][field] == self.f.new[self.name][field]
                            for field in V.FIELDS if field != 'OwnValues'))

    def test_extra_own_symbol_is_not_a_prefix_exemption(self):
        extra = self.name + 'Extra'
        self.f.old[extra] = {field: '{}' for field in V.FIELDS}
        self.f.new[extra] = copy.deepcopy(self.f.old[extra])
        self.f.new[extra]['OwnValues'] = '{unreviewed}'
        self.f.policy['ReviewedExistingFields'][extra] = ['OwnValues']
        self.f.policy['ReviewedFieldTables'][extra] = {'Before': self.f.old[extra], 'After': self.f.new[extra]}
        self.repin()
        with self.assertRaises(ValueError):
            self.f.inspect()

    def test_missing_source_file_from_descriptor_is_rejected(self):
        self.f.policy['MechanicalSourceInventories'][self.name]['OrderedPaths'] = self.paths[1:]
        with self.assertRaises(ValueError):
            self.f.inspect()

    def test_changed_source_digest_or_wrong_own_table_is_rejected(self):
        for replacement in ['f' * 64, 'not-the-exact-ordered-association']:
            old = self.f.new[self.name]['OwnValues']
            if len(replacement) == 64:
                self.f.new[self.name]['OwnValues'] = old.replace('b' * 64, replacement)
            else:
                self.f.new[self.name]['OwnValues'] = replacement
            self.repin()
            with self.assertRaises(ValueError):
                self.f.inspect()
            self.f.new[self.name]['OwnValues'] = old
            self.repin()

    def cold_bundle(self, directory):
        fixture = LoaderFixture(directory)
        root = fixture.root
        usage = root / V.USAGE
        usage.parent.mkdir(parents=True)
        usage.write_text('synthetic unchanged Usage')
        usage_binding = {'Path': str(usage), 'SHA256': V.digest(usage)}
        policy = {'NamespacePatterns': list(F.R.CAPTURE_PATTERNS),
            'MechanicalSourceInventories': {self.name: {'OrderedPaths': [
                'source/' + n for n in fixture.names], 'AdditionalSourceFiles': {}}},
            'LoadingInputs': {side: {V.USAGE: usage_binding} for side in ['Before', 'After']}}
        inputs = {'ActualInstalledRootCaptures': True, 'BeforePair': [], 'AfterPair': [],
            'CaptureControllers': {}, 'NewSymbolInventory': {}}
        for side, manifest in [('Before', fixture.before), ('After', fixture.after)]:
            pins = {str(root / name): digest for name, digest in manifest['Files'].items()}
            pins[str(usage)] = V.digest(usage)
            records = []
            for index in [1, 2]:
                raw = root / ('proof/cold-' + side + '-' + str(index) + '.json')
                raw.write_text('{"SyntheticControlOnly": true, "NoKernelExecuted": true}')
                binding = {'Path': str(raw.relative_to(root)), 'SHA256': V.digest(raw)}
                inputs[side + 'Pair'].append(binding)
                record = copy.deepcopy(fixture.controllers[side]['Records'][0])
                record.update(Pass=index, RawFile=str(raw), RawSHA256=binding['SHA256'])
                records.append(record)
            controller = {'SourceRoot': str(root / 'source'),
                'NamespacePatterns': policy['NamespacePatterns'], 'NoModelReadsOrPhysics': True,
                'NoCompatibilityAdmissionOrRootEdits': True,
                'UsageRegistryPinnedBeforeAndAfterBothCaptures': True, 'Inputs': pins, 'Records': records}
            path = root / ('proof/cold-controller-' + side + '.json')
            path.write_text(json.dumps(controller))
            inputs['CaptureControllers'][side] = {'Path': str(path.relative_to(root)), 'SHA256': V.digest(path)}
        return fixture, inputs, policy

    def test_cold_boundary_requires_exact_complete_source_tree(self):
        with tempfile.TemporaryDirectory() as directory:
            fixture, inputs, policy = self.cold_bundle(directory)
            self.assertIsNone(V.verify_cold_controls(F.R, fixture.root, inputs, fixture.before,
                fixture.after, policy, fixture.resolve))

    def test_missing_altered_or_extra_actual_source_file_is_rejected(self):
        for change in ['missing', 'altered', 'extra']:
            with tempfile.TemporaryDirectory() as directory:
                fixture, inputs, policy = self.cold_bundle(directory)
                path = fixture.root / 'source' / fixture.names[0]
                if change == 'missing':
                    path.unlink()
                elif change == 'altered':
                    path.write_text('unreviewed change')
                else:
                    (fixture.root / 'source/unreviewed.txt').write_text('extra')
                with self.assertRaises((ValueError, OSError)):
                    V.verify_cold_controls(F.R, fixture.root, inputs, fixture.before,
                        fixture.after, policy, fixture.resolve)

    def test_any_other_inventory_symbol_field_change_is_rejected(self):
        self.f.new[self.name]['DownValues'] = '{not-mechanical}'
        self.repin()
        with self.assertRaises(ValueError):
            self.f.inspect()


if __name__ == '__main__':
    unittest.main()
