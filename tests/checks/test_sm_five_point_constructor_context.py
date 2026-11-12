"""Fail-closed provenance tests, with no amplitude calculations or status promotion."""
from pathlib import Path
import copy
import importlib.util
import json
import tempfile
import unittest
from unittest import mock

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location('sm5_constructor_context_test', ROOT / 'tests/SM-5-point/constructor_context.py')
C = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(C)


def fields(**updates):
    result = dict.fromkeys(C.FIELDS, '{}')
    result.update(updates)
    return result


def capture(rows, post=False):
    result = {'PackageLoadedOnce': True, 'ModelsRead': [], 'PhysicsCalculationsPerformed': False,
              'WolframVersion': 'test-version', 'SystemID': 'test-system', 'SymbolCount': len(rows),
              'Definitions': rows}
    if post:
        result['CapturedNamePatterns'] = list(C.CAPTURE_PATTERNS)
    return result


def fixtures():
    before = {'Files': {name: 'old-' + name for name in C.CHANGED_FILES} | {'source/HEPCAT-Check.wl': 'checker', 'models/vars1.mdl': 'model', 'library.a': 'runtime'},
              'Programs': {'Wolfram': '/test/Wolfram', 'CalcHEP': '/test/CH'}}
    after = copy.deepcopy(before)
    after['Files'].update({name: 'new-' + name for name in C.CHANGED_FILES + C.ADDED_FILES})
    rows = {C.CONSTRUCTOR: fields(DownValues='{oldConstructor}', Messages='{HoldPattern[ConstructAmplitude::usage] :> "Old, usage", HoldPattern[ConstructAmplitude::arg] :> "Keep"}'),
            'ConstructiveDiagrams`CheckAmplitudes': fields(DownValues='{unchangedCheck}'),
            'ConstructiveDiagrams`Private`worker': fields(DownValues='{unchangedPrivate}')}
    new_rows = copy.deepcopy(rows)
    new_rows[C.CONSTRUCTOR]['DownValues'] = '{newConstructor}'
    new_rows[C.CONSTRUCTOR]['Messages'] = '{HoldPattern[ConstructAmplitude::usage] :> "New usage", HoldPattern[ConstructAmplitude::arg] :> "Keep"}'
    new_rows[C.PRIVATE_PREFIXES[0] + 'helper'] = fields(DownValues='{newPrivateHelper}')
    new_rows[C.PRIVATE_PREFIXES[1] + 'local$'] = fields()
    return before, after, [capture(rows), capture(copy.deepcopy(rows))], [capture(new_rows, True), capture(copy.deepcopy(new_rows), True)]


class ExactTransitionTests(unittest.TestCase):
    def setUp(self):
        self.before, self.after, self.old, self.new = fixtures()

    def report(self):
        return C.compare_transition(self.before, self.after, self.old, self.new)

    def mutate_new(self, callback):
        for raw in self.new:
            callback(raw['Definitions'])

    def test_exact_constructor_change_is_analysis_only(self):
        report = self.report()
        self.assertEqual(report['Status'], 'ANALYZED_NOT_SEALED')
        self.assertFalse(report['ValidationRerunPerformed'])
        self.assertFalse(report['ConstructorRerunPerformed'])
        self.assertEqual(report['ExistingSymbolsChecked'], 3)
        self.assertEqual(report['ChangedDefinitionFields'][C.CONSTRUCTOR], ['DownValues', 'Messages'])

    def test_every_other_math_field_is_protected(self):
        for field in C.FIELDS:
            with self.subTest(field=field):
                _, _, _, new = fixtures()
                for raw in new:
                    raw['Definitions']['ConstructiveDiagrams`CheckAmplitudes'][field] = '{changed}'
                with self.assertRaises(ValueError):
                    C.compare_transition(self.before, self.after, self.old, new)

    def test_constructor_protected_fields(self):
        for field in set(C.FIELDS) - {'DownValues', 'Messages'}:
            with self.subTest(field=field):
                _, _, _, new = fixtures()
                for raw in new:
                    raw['Definitions'][C.CONSTRUCTOR][field] = '{changed}'
                with self.assertRaises(ValueError):
                    C.compare_transition(self.before, self.after, self.old, new)

    def test_nonusage_constructor_message_change_rejected(self):
        self.mutate_new(lambda rows: rows[C.CONSTRUCTOR].update(Messages='{HoldPattern[ConstructAmplitude::usage] :> "New", HoldPattern[ConstructAmplitude::arg] :> "Changed"}'))
        with self.assertRaises(ValueError):
            self.report()

    def test_new_public_symbol_is_rejected_even_when_empty(self):
        for raw in self.new:
            raw['Definitions']['HEPCATCovariantCurrentSources`PublicHelper'] = fields()
            raw['SymbolCount'] += 1
        with self.assertRaises(ValueError):
            self.report()

    def test_defined_module_local_is_rejected(self):
        for raw in self.new:
            raw['Definitions']['ConstructiveDiagrams`Private`temporary$'] = fields(OwnValues='{1}')
            raw['SymbolCount'] += 1
        with self.assertRaises(ValueError):
            self.report()

    def test_empty_module_local_is_permitted(self):
        for raw in self.new:
            raw['Definitions']['ConstructiveDiagrams`Private`temporary$'] = fields()
            raw['SymbolCount'] += 1
        self.assertIn('ConstructiveDiagrams`Private`temporary$', self.report()['NewEmptyPrivateSymbols'])

    def test_unapproved_private_namespace_rejected(self):
        for raw in self.new:
            raw['Definitions']['Other`Private`helper'] = fields()
            raw['SymbolCount'] += 1
        with self.assertRaises(ValueError):
            self.report()

    def test_missing_existing_symbol_rejected(self):
        for raw in self.new:
            del raw['Definitions']['ConstructiveDiagrams`Private`worker']
            raw['SymbolCount'] -= 1
        with self.assertRaises(ValueError):
            self.report()

    def test_no_constructor_change_rejected(self):
        self.mutate_new(lambda rows: rows[C.CONSTRUCTOR].update(DownValues='{oldConstructor}'))
        with self.assertRaises(ValueError):
            self.report()

    def test_second_cold_capture_must_match_exactly(self):
        self.new[1]['Definitions'][C.CONSTRUCTOR]['DownValues'] = '{newConstructor$changed}'
        with self.assertRaises(ValueError):
            self.report()

    def test_generated_suffixes_are_not_normalized(self):
        self.new[0]['Definitions'][C.CONSTRUCTOR]['DownValues'] = '{newConstructor$1}'
        self.new[1]['Definitions'][C.CONSTRUCTOR]['DownValues'] = '{newConstructor$2}'
        with self.assertRaises(ValueError):
            self.report()

    def test_all_namespaces_required(self):
        for raw in self.new:
            raw['CapturedNamePatterns'].remove('HEPCATCovariantContactCalibration`*')
        with self.assertRaises(ValueError):
            self.report()

    def test_complete_ten_fields_required(self):
        for raw in self.new:
            del raw['Definitions'][C.CONSTRUCTOR]['NValues']
        with self.assertRaises(ValueError):
            self.report()

    def test_version_and_system_are_protected(self):
        for key in ('WolframVersion', 'SystemID'):
            with self.subTest(key=key):
                _, _, _, new = fixtures()
                for raw in new:
                    raw[key] = 'changed'
                with self.assertRaises(ValueError):
                    C.compare_transition(self.before, self.after, self.old, new)

    def test_models_or_physics_in_capture_rejected(self):
        for key, value in [('ModelsRead', ['SM']), ('PhysicsCalculationsPerformed', True), ('PackageLoadedOnce', False)]:
            raw = copy.deepcopy(self.new[0])
            raw[key] = value
            with self.subTest(key=key), self.assertRaises(ValueError):
                C.definitions(raw, require_post=True)

    def test_model_runtime_and_other_source_bytes_protected(self):
        for name in ('source/HEPCAT-Check.wl', 'models/vars1.mdl', 'library.a'):
            with self.subTest(name=name):
                after = copy.deepcopy(self.after)
                after['Files'][name] = 'changed'
                with self.assertRaises(ValueError):
                    C.compare_transition(self.before, after, self.old, self.new)

    def test_extra_missing_files_and_programs_rejected(self):
        for action in ('extra', 'missing', 'program'):
            after = copy.deepcopy(self.after)
            if action == 'extra':
                after['Files']['source/internal/Unexpected.wl'] = 'x'
            elif action == 'missing':
                del after['Files']['library.a']
            else:
                after['Programs']['Wolfram'] = '/different/Wolfram'
            with self.subTest(action=action), self.assertRaises(ValueError):
                C.compare_transition(self.before, after, self.old, self.new)

    def test_full_names_preserve_symbol_identity(self):
        raw = capture({'helper': fields()}, True)
        raw['FullNames'] = {'helper': C.PRIVATE_PREFIXES[0] + 'helper'}
        self.assertEqual(list(C.definitions(raw, True)), [C.PRIVATE_PREFIXES[0] + 'helper'])
        raw['Definitions']['second'] = fields()
        raw['FullNames']['second'] = C.PRIVATE_PREFIXES[0] + 'helper'
        raw['SymbolCount'] += 1
        with self.assertRaises(ValueError):
            C.definitions(raw, True)


class MechanicalExceptionTests(unittest.TestCase):
    def setUp(self):
        self.before, self.after, self.old, self.new = fixtures()
        for raw in self.old:
            raw['Definitions'][C.LOADER_CACHE] = fields(OwnValues='{HoldPattern[ConstructiveDiagrams`Private`$amplitudeParallelLoadedSignature] :> 100}')
            raw['SymbolCount'] += 1
        for raw in self.new:
            raw['Definitions'][C.LOADER_CACHE] = fields(OwnValues='{HoldPattern[ConstructiveDiagrams`Private`$amplitudeParallelLoadedSignature] :> 200}')
            raw['SymbolCount'] += 1
        self.evidence = {'Before': 100, 'After': 200, 'InputCount': 13, '_VerifiedInputHashes': True}

    def test_cache_difference_requires_exact_bound_metadata(self):
        with self.assertRaises(ValueError):
            C.compare_transition(self.before, self.after, self.old, self.new)
        report = C.compare_transition(self.before, self.after, self.old, self.new, self.evidence)
        self.assertEqual(report['ChangedDefinitionFields'][C.LOADER_CACHE], ['OwnValues'])
        self.assertNotIn('_VerifiedInputHashes', report['MechanicalLoaderSignatureEvidence'])

    def test_wrong_recomputed_integer_or_unverified_inputs_rejected(self):
        for key, value in (('Before', 999), ('After', 999), ('_VerifiedInputHashes', False)):
            evidence = dict(self.evidence)
            evidence[key] = value
            with self.subTest(key=key), self.assertRaises(ValueError):
                C.compare_transition(self.before, self.after, self.old, self.new, evidence)

    def test_loader_definition_attributes_and_messages_never_exempted(self):
        for field in set(C.FIELDS) - {'OwnValues'}:
            new = copy.deepcopy(self.new)
            for raw in new:
                raw['Definitions'][C.LOADER_CACHE][field] = '{changed}'
            with self.subTest(field=field), self.assertRaises(ValueError):
                C.compare_transition(self.before, self.after, self.old, new, self.evidence)

    def test_cache_ownvalue_must_be_single_exact_integer_rule(self):
        for text in ('{100}', '{HoldPattern[ConstructiveDiagrams`Private`$amplitudeParallelLoadedSignature] :> f[200]}',
                     '{HoldPattern[ConstructiveDiagrams`Private`$amplitudeParallelLoadedSignature] :> 200, extraRule}'):
            new = copy.deepcopy(self.new)
            for raw in new:
                raw['Definitions'][C.LOADER_CACHE]['OwnValues'] = text
            with self.subTest(text=text), self.assertRaises(ValueError):
                C.compare_transition(self.before, self.after, self.old, new, self.evidence)

    def test_temporary_local_is_listed_not_normalized(self):
        for raw in self.new:
            raw['Definitions']['ConstructiveDiagrams`Private`local$'] = fields(Attributes='{Temporary}')
            raw['SymbolCount'] += 1
        report = C.compare_transition(self.before, self.after, self.old, self.new, self.evidence)
        self.assertEqual(report['NewTemporaryConstructorSymbols'], ['ConstructiveDiagrams`Private`local$'])

    def test_temporary_local_any_other_field_rejected(self):
        for field in set(C.FIELDS) - {'Attributes'}:
            new = copy.deepcopy(self.new)
            for raw in new:
                row = fields(Attributes='{Temporary}')
                row[field] = '{changed}'
                raw['Definitions']['ConstructiveDiagrams`Private`local$'] = row
                raw['SymbolCount'] += 1
            with self.subTest(field=field), self.assertRaises(ValueError):
                C.compare_transition(self.before, self.after, self.old, new, self.evidence)

    def test_temporary_other_attribute_or_nonlocal_name_rejected(self):
        for name, attrs in (('local$', '{Temporary, Protected}'), ('notLocal', '{Temporary}')):
            new = copy.deepcopy(self.new)
            for raw in new:
                raw['Definitions']['ConstructiveDiagrams`Private`' + name] = fields(Attributes=attrs)
                raw['SymbolCount'] += 1
            with self.subTest(name=name), self.assertRaises(ValueError):
                C.compare_transition(self.before, self.after, self.old, new, self.evidence)

    def test_review_must_explicitly_cover_both_mechanical_exceptions(self):
        for raw in self.new:
            raw['Definitions']['ConstructiveDiagrams`Private`local$'] = fields(Attributes='{Temporary}')
            raw['SymbolCount'] += 1
        report = C.compare_transition(self.before, self.after, self.old, self.new, self.evidence)
        review = {'Status': 'REVIEWED_CONSTRUCTOR_ONLY_LOADER_PRIVATE_MODULES', 'ReviewedBy': 'reviewer',
                  'AnalysisSHA256': C.canonical_digest(report), **dict.fromkeys(C.REVIEW_FLAGS, True),
                  'LoaderSignatureInputsAndExactRecomputedValuesReviewed': True,
                  'ListedConstructorTemporarySymbolsReviewed': True}
        self.assertEqual(C.reviewed_payload(report, review)['Status'], 'SEALED')
        for key in ('LoaderSignatureInputsAndExactRecomputedValuesReviewed', 'ListedConstructorTemporarySymbolsReviewed'):
            bad = dict(review)
            del bad[key]
            with self.subTest(key=key), self.assertRaises(ValueError):
                C.reviewed_payload(report, bad)


class MessageAndReviewTests(unittest.TestCase):
    def test_usage_text_can_change_with_escaped_quotes_and_nested_commas(self):
        a = r'{HoldPattern[ConstructAmplitude::usage] :> "Old \\"quoted\\", usage", HoldPattern[ConstructAmplitude::arg] :> f[1, {2, 3}]}'
        b = r'{HoldPattern[ConstructAmplitude::usage] :> "New usage", HoldPattern[ConstructAmplitude::arg] :> f[1, {2, 3}]}'
        # Use the actual InputForm one-backslash quote escape.
        a = a.replace('\\\\"', '\\"')
        self.assertTrue(C.usage_only(a, b))

    def test_malformed_or_multiple_usage_messages_rejected(self):
        good = '{HoldPattern[ConstructAmplitude::usage] :> "Old"}'
        for bad in ('{broken]', '{HoldPattern[ConstructAmplitude::usage] :> "Unclosed}',
                    '{HoldPattern[ConstructAmplitude::usage] :> "A", HoldPattern[ConstructAmplitude::usage] :> "B"}'):
            with self.subTest(bad=bad):
                self.assertFalse(C.usage_only(good, bad))

    def test_seal_requires_review_of_exact_analysis(self):
        analysis = C.compare_transition(*fixtures())
        review = {'Status': 'REVIEWED_CONSTRUCTOR_ONLY_LOADER_PRIVATE_MODULES',
                  'AnalysisSHA256': C.canonical_digest(analysis), 'ReviewedBy': 'independent reviewer',
                  **dict.fromkeys(C.REVIEW_FLAGS, True)}
        payload = C.reviewed_payload(analysis, review)
        self.assertTrue(payload['HistoricalEvidenceOnly'])
        self.assertFalse(payload['CurrentConstructorSuccessClaimed'])
        self.assertTrue(payload['SupplementalCounterexampleChecksStillRequired'])
        self.assertTrue(payload['PrecisionQualificationUnchangedRequired'])
        for key in ('AnalysisSHA256', 'ReviewedBy', *C.REVIEW_FLAGS):
            bad = dict(review)
            bad[key] = False
            with self.subTest(key=key), self.assertRaises(ValueError):
                C.reviewed_payload(analysis, bad)


class FiniteCheckpointTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.summary_path = self.root / 'sm5_test.json'
        self.capsule = self.root / 'sm5_test.wxf'
        self.capsule.write_bytes(b'exact historical capsule')
        self.summary = {'Status': 'COMPLETE', 'ContextSHA256': C.BASE_CONTEXT,
                        'CapsuleSHA256': C.digest(self.capsule), 'Seconds': 123, 'RequestedOptions': {'TimeConstraint': 600}}
        self.summary_path.write_text(json.dumps(self.summary))
        row = {'RecordedContextSHA256': C.BASE_CONTEXT, 'SummaryValidJSON': True,
               'SummarySHA256': C.digest(self.summary_path), 'Status': 'COMPLETE',
               'CapsuleExists': True, 'DeclaredCapsuleMatches': True, 'CapsuleSHA256': C.digest(self.capsule)}
        self.baseline = {'Snapshot': {self.summary_path.name: row}, 'Record': {'ExistingExporterBridgeSHA256': 'old-bridge-sha'}}
        self.proof = {'_VerifiedCurrentContext': True, 'Status': 'SEALED', 'BeforeContextSHA256': C.BASE_CONTEXT,
                      'AfterContextSHA256': 'c3', 'Kind': 'ExactConstructorOnlyHistoricalValidationTransition',
                      'HistoricalEvidenceOnly': True, 'CurrentConstructorSuccessClaimed': False, 'AnalysisSHA256': 'analysis-sha'}

    def evidence(self):
        return C.checkpoint_evidence(self.summary_path, self.summary, self.capsule, self.proof, self.baseline)

    def test_historical_validation_keeps_context_options_and_times(self):
        before = copy.deepcopy(self.summary)
        report = self.evidence()
        self.assertEqual(self.summary, before)
        self.assertEqual(report['RecordedContextSHA256'], C.BASE_CONTEXT)
        self.assertEqual(report['Stage'], 'HistoricalNativeValidation')
        self.assertFalse(report['ConstructorRerunPerformed'])
        self.assertFalse(report['ValidationRerunPerformed'])
        self.assertFalse(report['CurrentConstructorSuccessClaimed'])
        self.assertTrue(report['SupplementalCounterexampleChecksStillRequired'])
        self.assertTrue(report['PrecisionQualificationUnchangedRequired'])

    def test_old_context_newly_written_record_is_not_admitted(self):
        self.summary_path.write_text(json.dumps(self.summary) + ' ')
        self.assertIsNone(self.evidence())

    def test_unknown_snapshot_record_is_not_admitted(self):
        self.baseline['Snapshot'] = {}
        self.assertIsNone(self.evidence())

    def test_capsule_actual_bytes_and_declared_hash_required(self):
        self.capsule.write_bytes(b'changed')
        self.assertIsNone(self.evidence())

    def test_unverified_or_current_constructor_claim_rejected(self):
        for key, value in (('_VerifiedCurrentContext', False), ('Status', 'ANALYZED_NOT_SEALED'),
                           ('CurrentConstructorSuccessClaimed', True), ('HistoricalEvidenceOnly', False)):
            proof = dict(self.proof)
            proof[key] = value
            with self.subTest(key=key):
                self.assertIsNone(C.checkpoint_evidence(self.summary_path, self.summary, self.capsule, proof, self.baseline))

    def test_missing_capsule_cannot_be_complete(self):
        self.capsule.unlink()
        self.baseline['Snapshot'][self.summary_path.name]['CapsuleExists'] = False
        self.assertIsNone(self.evidence())

    def test_c1_chain_requires_the_original_exact_snapshot(self):
        old = 'c1'
        self.summary['ContextSHA256'] = old
        self.summary_path.write_text(json.dumps(self.summary))
        row = self.baseline['Snapshot'][self.summary_path.name]
        row.update(RecordedContextSHA256=old, SummarySHA256=C.digest(self.summary_path))
        self.baseline.update(ExporterBridge={'NativeContextSHA256': old},
                             ExporterArchives={'CheckpointSnapshot': {self.summary_path.name: copy.deepcopy(row)}})
        result = self.evidence()
        self.assertEqual(len(result['CompatibilityChain']), 2)
        self.assertEqual(result['RecordedContextSHA256'], old)
        self.baseline['ExporterArchives']['CheckpointSnapshot'][self.summary_path.name]['SummarySHA256'] = 'wrong'
        self.assertIsNone(self.evidence())

    def test_timed_out_no_capsule_is_historical_attempt_only(self):
        self.capsule.unlink()
        self.summary.update(Status='TIMEOUT', CapsuleSHA256=None)
        self.summary_path.write_text(json.dumps(self.summary))
        row = self.baseline['Snapshot'][self.summary_path.name]
        row.update(Status='TIMEOUT', CapsuleExists=False, SummarySHA256=C.digest(self.summary_path))
        result = self.evidence()
        self.assertIsNotNone(result)
        self.assertNotIn('PASS', result.values())

    def test_escaping_proof_paths_rejected(self):
        for name in ('../outside', '/private/tmp/outside'):
            with self.subTest(name=name), self.assertRaises(ValueError):
                C.inside(self.root, name)
        (self.root / 'escape').symlink_to(self.root.parent)
        with self.assertRaises(ValueError):
            C.inside(self.root, 'escape/file')


class SealedProofArchiveTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.here = self.root / 'tests/SM-5-point'
        self.here.mkdir(parents=True)
        self.before, self.after, self.old, self.new = fixtures()
        self.paths = [self.here / name for name in ('manifest.json', 'one.json', 'two.json', 'review.json')]
        for path, value in zip(self.paths[:3], (self.after, *self.new)):
            path.write_text(json.dumps(value))
        self.baseline = {'Manifest': self.before, 'Raw': self.old}
        self.stage = C.stage_module()
        actual_fingerprint = self.stage.fingerprint
        self.stage.fingerprint = lambda value: C.BASE_CONTEXT if value == self.before else actual_fingerprint(value)
        self.stage.manifest = lambda *args: self.after
        self.addCleanup(mock.patch.stopall)
        mock.patch.object(C, 'load_baseline', return_value=self.baseline).start()
        mock.patch.object(C, 'stage_module', return_value=self.stage).start()
        analysis = C.analyze(self.root, *self.paths[:3])
        review = {'Status': 'REVIEWED_CONSTRUCTOR_ONLY_LOADER_PRIVATE_MODULES',
                  'AnalysisSHA256': C.canonical_digest(analysis), 'ReviewedBy': 'reviewer',
                  **dict.fromkeys(C.REVIEW_FLAGS, True)}
        self.paths[3].write_text(json.dumps(review))
        self.payload = C.seal_payload(self.root, self.here, {}, *self.paths)
        self.proof = self.here / 'proof.json'
        self.proof.write_text(json.dumps(self.payload))

    def verify(self):
        return C.verified_proof(self.root, self.here, {}, self.payload['AfterContextSHA256'], self.proof)

    def test_exact_immutable_archives_and_current_manifest_verify(self):
        result = self.verify()
        self.assertTrue(result['_VerifiedCurrentContext'])
        self.assertFalse(result['CurrentConstructorSuccessClaimed'])
        self.assertNotIn('_Baseline', C.public_proof(result))

    def test_postcapture_byte_change_invalidates_proof(self):
        self.paths[1].write_text(self.paths[1].read_text() + ' ')
        self.assertIsNone(self.verify())

    def test_review_file_byte_change_invalidates_proof(self):
        self.paths[3].write_text(self.paths[3].read_text() + ' ')
        self.assertIsNone(self.verify())

    def test_current_runtime_manifest_change_invalidates_proof(self):
        changed = copy.deepcopy(self.after)
        changed['Files']['library.a'] = 'new-runtime'
        self.stage.manifest = lambda *args: changed
        self.assertIsNone(self.verify())

    def test_current_context_mismatch_invalidates_proof(self):
        self.assertIsNone(C.verified_proof(self.root, self.here, {}, 'wrong-current-context', self.proof))

    def test_unsealed_proof_is_not_admission(self):
        self.payload['Status'] = 'ANALYZED_NOT_SEALED'
        self.proof.write_text(json.dumps(self.payload))
        self.assertIsNone(self.verify())

    def test_archive_traversal_rejected_before_read(self):
        self.payload['AfterManifest'] = '../outside.json'
        self.proof.write_text(json.dumps(self.payload))
        self.assertIsNone(self.verify())

    def test_current_manifest_change_does_not_reseal_implicitly(self):
        self.stage.manifest = lambda *args: self.before
        with self.assertRaises(ValueError):
            C.seal_payload(self.root, self.here, {}, *self.paths)


class WorkflowHistoricalAdmissionTests(FiniteCheckpointTests):
    def setUp(self):
        super().setUp()
        spec = importlib.util.spec_from_file_location('ctor_workflow_integration', ROOT / 'tests/SM-5-point/run.py')
        self.workflow = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(self.workflow)
        self.summary.update(ID='sm5_test', Rows=7, FeynmanRows=1, CheckStatuses={'PASS': 7})
        self.summary_path.write_text(json.dumps(self.summary))
        self.baseline['Snapshot'][self.summary_path.name]['SummarySHA256'] = C.digest(self.summary_path)
        self.proof.update(_Baseline=self.baseline, _ProofSHA256='proof-hash', CodeReviewFileSHA256='review-hash')
        self.patch = mock.patch.object(self.workflow, 'RESULTS', self.root)
        self.patch.start()
        self.addCleanup(self.patch.stop)

    def test_native_admission_preserves_actual_context_and_checks(self):
        result = self.workflow.checked_summary('sm5_test', '', 'c3', self.proof)
        self.assertEqual(result['Status'], 'COMPLETE')
        self.assertEqual(result['ContextSHA256'], C.BASE_CONTEXT)
        self.assertFalse(result['ContextCompatibility']['CurrentConstructorSuccessClaimed'])
        self.assertEqual(result['Seconds'], 123)

    def test_compatibility_does_not_bypass_native_check_failure(self):
        self.summary['CheckStatuses'] = {'PASS': 6, 'FAIL': 1}
        self.summary_path.write_text(json.dumps(self.summary))
        self.baseline['Snapshot'][self.summary_path.name]['SummarySHA256'] = C.digest(self.summary_path)
        self.assertEqual(self.workflow.checked_summary('sm5_test', '', 'c3', self.proof)['Status'], 'STALE')

    def test_current_export_can_use_exact_original_native_context(self):
        family = {'FamilyID': 'sm5f_test', 'Status': 'PASS', 'ContextSHA256': 'c3',
                  'ExporterContextSHA256': 'c3', 'CaseIDs': ['sm5_test'],
                  'CaseCapsulesSHA256': {'sm5_test': self.summary['CapsuleSHA256']},
                  'NativeCaseContexts': {'sm5_test': C.BASE_CONTEXT}}
        capsule = self.root / 'sm5f_test-export.wxf'
        capsule.write_bytes(b'new export with exact old amplitude input')
        family['CapsuleSHA256'] = C.digest(capsule)
        path = self.root / 'sm5f_test-export.json'
        path.write_text(json.dumps(family))
        result = self.workflow.checked_summary('sm5f_test', '-export', 'c3', self.proof)
        self.assertEqual(result['Status'], 'PASS')
        self.assertEqual(result['NativeCaseContexts'], {'sm5_test': C.BASE_CONTEXT})
        family['NativeCaseContexts'] = {'sm5_test': 'c3'}
        path.write_text(json.dumps(family))
        self.assertEqual(self.workflow.checked_summary('sm5f_test', '-export', 'c3', self.proof)['Status'], 'STALE')


class ActualBaselineTests(unittest.TestCase):
    def test_reviewed_prepatch_archive_reconstructs_exactly(self):
        baseline = C.load_baseline(ROOT)
        self.assertEqual(len(baseline['Snapshot']), 310)
        self.assertEqual(len(C.cold_pair(*baseline['Raw'])), 7784)
        self.assertEqual(C.stage_module().fingerprint(baseline['Manifest']), C.BASE_CONTEXT)


if __name__ == '__main__':
    unittest.main()
