"""Synthetic data-only controls for finite historical retention.

These are not amplitude, constructor, validator or precision replay tests.
"""
from pathlib import Path
from types import SimpleNamespace
import copy
import importlib.util
import json
import tempfile
import unittest
from unittest.mock import patch

WORKFLOW = Path(__file__).resolve().parents[1] / 'SM-5-point'
def load(name, filename):
    spec = importlib.util.spec_from_file_location(name, WORKFLOW / filename)
    result = importlib.util.module_from_spec(spec); spec.loader.exec_module(result); return result
R = load('finite_receipt_test_engine', 'constructor_receipts.py')
C = load('finite_receipt_test_core', 'constructor_context.py')
S = load('finite_receipt_test_supplemental', 'supplemental_probes.py')
K = load('finite_receipt_test_collector', 'capture-constructor-definitions.py')

class Fixture:
    def __init__(self, root):
        self.root=Path(root); self.here=self.root/'tests/SM-5-point'; self.here.mkdir(parents=True)
        self.before={'Files':{key:'a'*64 for key in R.SOURCE_PATHS},'Programs':{}}
        self.after=copy.deepcopy(self.before)
        for key in R.SOURCE_PATHS:self.after['Files'][key]='b'*64
        self.old={name:{field:'{}'for field in R.FIELDS}for name in R.CONSTRUCTOR_FIELDS}
        for name in self.old:self.old[name]['DownValues']='{before['+name+']}'
        self.old[R.LOADER]={field:'{}'for field in R.FIELDS};self.old[R.LOADER]['OwnValues']='{HoldPattern['+R.LOADER+'] :> 1}'
        self.old[R.LOADER_FORMULA]={field:'{}'for field in R.FIELDS};self.old[R.LOADER_FORMULA]['DownValues']='{theSameLiteralFormula}'
        self.checker='ConstructiveDiagrams`CheckAmplitudes';self.old[self.checker]={field:'{}'for field in R.FIELDS};self.old[self.checker]['DownValues']='{unchangedChecker}'
        self.new=copy.deepcopy(self.old)
        for name in R.CONSTRUCTOR_FIELDS:self.new[name]['DownValues']='{after['+name+']}'
        self.new[R.LOADER]['OwnValues']='{HoldPattern['+R.LOADER+'] :> 2}'
        self.raw=lambda defs:{'PackageLoadedOnce':True,'ModelsRead':[],'PhysicsCalculationsPerformed':False,'WolframVersion':'Synthetic','SystemID':'Synthetic','CapturedNamePatterns':list(R.CAPTURE_PATTERNS),'Definitions':defs,'SymbolCount':len(defs)}
        self.raw_before=[self.raw(self.old),self.raw(copy.deepcopy(self.old))];self.raw_after=[self.raw(self.new),self.raw(copy.deepcopy(self.new))]
        self.loader={'Before':1,'After':2,'InputCountBefore':18,'InputCountAfter':18,'IndependentBareRecomputationMatches':True,'FormulaAllTenFieldsUnchanged':True}
        self.analysis=R.analyze_transition(C,self.before,self.after,self.raw_before,self.raw_after,{},self.loader,installed_root_captures=True)
        self.before_context=R.canonical_digest(self.before);self.after_context=R.canonical_digest(self.after)
        producer='c'*64
        self.native={'ID':'case','Status':'COMPLETE','ContextSHA256':producer,'Rows':1,'FeynmanRows':1,'CheckStatuses':{'PASS':1}}
        self.native_path=self.root/'tests/results/SM-5-point/case.json';self.native_path.parent.mkdir(parents=True);self.native_path.with_suffix('.wxf').write_bytes(b'synthetic-native-capsule');self.native['CapsuleSHA256']=R.digest(self.native_path.with_suffix('.wxf'));self.put('tests/results/SM-5-point/case.json',self.native)
        self.export={'FamilyID':'family','Status':'FAIL','CalcHEPStatus':'FAIL','MadGraphStatus':'PASS','ContextSHA256':producer,'CaseIDs':['case'],'CaseCapsulesSHA256':{'case':self.native['CapsuleSHA256']},'Points':1000}
        self.export_path=self.root/'tests/results/SM-5-point/family-export.json';self.export_path.with_suffix('.wxf').write_bytes(b'synthetic-export-capsule');self.export['CapsuleSHA256']=R.digest(self.export_path.with_suffix('.wxf'));self.put('tests/results/SM-5-point/family-export.json',self.export)
        history={'Status':'OLDER_CONDITIONAL_RETENTION','CurrentValidatorPassClaimed':False,'ValidationRerunPerformed':False,'C11MessageStateUnreconstructed':True}
        def row(kind,id,path,raw):return{'Kind':kind,'ID':id,'SummaryPath':str(path.relative_to(self.root)),'SummarySHA256':R.digest(path),'CapsulePath':str(path.with_suffix('.wxf').relative_to(self.root)),'CapsuleSHA256':raw['CapsuleSHA256'],'RawSummary':raw,'ActualProducerContextSHA256':raw['ContextSHA256'],'RecordedStatus':raw['Status'],'AdmittedAtContextSHA256':self.before_context,'PriorHistoricalAdmission':copy.deepcopy(history),'PriorSupplementalEntries':[], 'ExistingPrecisionQualification':{'Status':'PRECISION_QUALIFIED','RawCalcHEPStatus':'FAIL'}if kind=='Export'else None}
        self.records=[row('Native','case',self.native_path,self.native),row('Export','family',self.export_path,self.export)]
        self.probes={'SchemaVersion':1,'Probes':[]};self.probe_binding=self.put('tests/SM-5-point/supplemental-probes.json',self.probes)
        self.snapshot={'Status':'UNSEALED_VERIFIED_HISTORICAL_SNAPSHOT','AdmissionContextSHA256':self.before_context,'NativeCaseCount':1,'ExportCompanionCount':1,'ArtifactCount':2,'ActualProducerContextsPreserved':True,'PriorConditionsPreserved':True,'NewValidationPassClaimed':False,'SharedProofBindings':[],'Records':self.records,'SupplementalRegistry':self.probe_binding}
        self.receipt={'SchemaVersion':1,'Kind':R.KIND,'Status':'SEALED_REVIEWED','BeforeContextSHA256':self.before_context,'AfterContextSHA256':self.after_context,'ScopeFlags':{'HistoricalEvidenceOnly':True,**{k:False for k in R.FALSE_CLAIMS}},'BeforeManifest':self.put('proof/before.json',self.before),'AfterManifest':self.put('proof/after.json',self.after),'DefinitionInputs':{'ActualInstalledRootCaptures':True,'BeforePair':[self.put('proof/before1.json',self.raw_before[0]),self.put('proof/before2.json',self.raw_before[1])],'AfterPair':[self.put('proof/after1.json',self.raw_after[0]),self.put('proof/after2.json',self.raw_after[1])],'NewSymbolInventory':self.put('proof/new.json',{}),'LoaderRecomputation':self.put('proof/loader.json',self.loader)},'Analysis':self.put('proof/analysis.json',self.analysis),'Snapshot':self.put('proof/snapshot.json',self.snapshot),'VerifierTools':[]}
        for name in ('constructor_receipts.py','constructor_context.py','stage_context.py','supplemental_probes.py','run.py','generated_cleanup.py','precision_evidence.py','madgraph_precision_evidence.py','capture-constructor-definitions.wl','capture-constructor-definitions.py'):
            dest=self.here/name;dest.write_bytes((WORKFLOW/name).read_bytes());self.receipt['VerifierTools'].append({'Path':str(dest.relative_to(self.root)),'SHA256':R.digest(dest)})
        self.receipt['HistoricalVerifierTools']=[self.put('proof/old-verifier-preimage.json',{'OriginalVerifierBytes':'synthetic predecessor tool preimage'})]
        self.pin()
    def put(self,name,value):
        path=self.root/name;path.parent.mkdir(parents=True,exist_ok=True);path.write_text(json.dumps(value,sort_keys=True,indent=2)+'\n');return{'Path':name,'SHA256':R.digest(path)}
    def pin(self):
        self.registry={self.after_context:self.put('proof/receipt.json',self.receipt)}
    def repin_snapshot(self):self.receipt['Snapshot']=self.put('proof/snapshot.json',self.snapshot);self.pin()
    def add_independent_strict_pair(self):
        raw=copy.deepcopy(self.native);raw['ID']='caseB'
        path=self.native_path.with_name('caseB.json');path.with_suffix('.wxf').write_bytes(b'independent-native-B')
        raw['CapsuleSHA256']=R.digest(path.with_suffix('.wxf'));self.put(str(path.relative_to(self.root)),raw)
        row=copy.deepcopy(self.records[0]);row.update(ID='caseB',SummaryPath=str(path.relative_to(self.root)),SummarySHA256=R.digest(path),CapsulePath=str(path.with_suffix('.wxf').relative_to(self.root)),CapsuleSHA256=raw['CapsuleSHA256'],RawSummary=raw,PriorSupplementalEntries=[])
        self.records.append(row);self.native_b=raw;self.native_b_path=path
        raw=copy.deepcopy(self.export);raw.update(FamilyID='familyB',Status='PASS',CalcHEPStatus='PASS',CaseIDs=['caseB'],CaseCapsulesSHA256={'caseB':self.native_b['CapsuleSHA256']})
        path=self.export_path.with_name('familyB-export.json');path.with_suffix('.wxf').write_bytes(b'independent-export-B')
        raw['CapsuleSHA256']=R.digest(path.with_suffix('.wxf'));self.put(str(path.relative_to(self.root)),raw)
        row=copy.deepcopy(self.records[1]);row.update(ID='familyB',SummaryPath=str(path.relative_to(self.root)),SummarySHA256=R.digest(path),CapsulePath=str(path.with_suffix('.wxf').relative_to(self.root)),CapsuleSHA256=raw['CapsuleSHA256'],RawSummary=raw,RecordedStatus=raw['Status'],ExistingPrecisionQualification=None)
        self.records.append(row);self.export_b=raw;self.export_b_path=path
        self.snapshot.update(NativeCaseCount=2,ExportCompanionCount=2,ArtifactCount=4);self.repin_snapshot()
    def assert_b_available(self, tester, proof):
        tester.assertIsNotNone(R.record_evidence(self.native_b_path,self.native_b,self.after_context,proof))
        tester.assertIsNotNone(R.record_evidence(self.export_b_path,self.export_b,self.after_context,proof))
    def make_probe(self, identifier='prior', case_id='case', context=None, status='PASS'):
        context = context or self.before_context
        results = self.root/'tests/results/SM-5-point'
        folder = results/'diagnostics'/identifier; folder.mkdir(parents=True,exist_ok=True)
        counts = {status:1}
        summary = {'SourceCaseID':case_id,'SourceCapsuleSHA256':self.native['CapsuleSHA256'],
            'ContextSHA256':context,'Status':'ALL_PASS' if status=='PASS' else 'CHECKS_FAILED',
            'Rows':[{'Status':status}],'StatusCounts':counts,'SourceAmplitudeUnchanged':True,
            'NoExpressionMassSlotOrFlavorTransport':True}
        artifacts = {'Capsule':folder/'probe.wxf','Summary':folder/'probe.json',
                     'Script':folder/'probe.wl','ConversionScript':folder/'convert.wl'}
        for key,path in artifacts.items():
            path.write_text(json.dumps(summary) if key=='Summary' else 'synthetic '+key)
        return {'ID':identifier,'CaseID':case_id,'Kind':'ParameterCheck',
            'SourceCapsuleSHA256':self.native['CapsuleSHA256'],'ProbeContextSHA256':context,
            'Status':summary['Status'],'StatusCounts':counts,
            'Files':{key:{'Path':str(path.relative_to(results)),'SHA256':R.digest(path)}
                     for key,path in artifacts.items()}}
    def retain_probe(self, entry):
        self.records[0]['PriorSupplementalEntries']=[copy.deepcopy(entry)]
        self.repin_snapshot()
    def probe_report(self, entries, proof=None, case_id='case'):
        return S.case_report(case_id,self.native['CapsuleSHA256'],entries,
            self.root/'tests/results/SM-5-point',self.after_context,proof or self.proof())
    def status(self):
        stage=SimpleNamespace(fingerprint=R.canonical_digest,manifest=lambda *a:self.after)
        with patch.object(R,'_stage',return_value=stage):return R.receipt_status(self.root,self.here,{},self.after_context,registry=self.registry,core=C)
    def proof(self):
        status=self.status();assert status['Proof']is not None,status;return status['Proof']

class ReceiptTests(unittest.TestCase):
    def setUp(self):self.temp=tempfile.TemporaryDirectory();self.addCleanup(self.temp.cleanup);self.f=Fixture(self.temp.name)
    def test_exact_finite_fixture_keeps_actual_producer(self):
        result=R.record_evidence(self.f.native_path,self.f.native,self.f.after_context,self.f.proof());self.assertEqual(result['ActualProducerContextSHA256'],'c'*64);self.assertFalse(result['CurrentValidatorPassClaimed'])
    def test_native_ch_failure_is_not_upgraded(self):
        result=R.record_evidence(self.f.export_path,self.f.export,self.f.after_context,self.f.proof());self.assertEqual(result['RecordedStatus'],'FAIL');self.assertEqual(result['ExistingPrecisionQualification']['Status'],'PRECISION_QUALIFIED');self.assertFalse(result['CurrentConstructorSuccessClaimed'])
    def test_prior_c11_conditions_preserved(self):
        result=R.record_evidence(self.f.native_path,self.f.native,self.f.after_context,self.f.proof());self.assertTrue(result['PriorHistoricalAdmission']['C11MessageStateUnreconstructed'])
    def test_unregistered_receipt_is_unavailable(self):self.f.registry={};self.assertEqual(self.f.status()['Status'],'UNAVAILABLE')
    def test_draft_cannot_self_admit(self):self.f.receipt['Status']='DRAFT_NO_ADMISSION';self.f.pin();self.assertEqual(self.f.status()['Status'],'UNAVAILABLE')
    def test_changed_receipt_digest_rejected(self):self.f.put('proof/receipt.json',{'tampered':True});self.assertIsNone(self.f.status()['Proof'])
    def test_missing_receipt_explicitly_unavailable(self):(self.f.root/'proof/receipt.json').unlink();self.assertEqual(self.f.status()['Status'],'UNAVAILABLE')
    def test_missing_capsule_explicitly_unavailable_for_its_record(self):
        self.f.native_path.with_suffix('.wxf').unlink();proof=self.f.proof();self.assertEqual(R.record_status(self.f.native_path,self.f.native,self.f.after_context,proof)['Status'],'UNAVAILABLE')
    def test_mutated_capsule_record_unavailable(self):
        proof=self.f.proof();self.f.native_path.with_suffix('.wxf').write_bytes(b'changed');self.assertEqual(R.record_status(self.f.native_path,self.f.native,self.f.after_context,proof)['Status'],'UNAVAILABLE')
    def test_mutated_summary_rejected(self):
        proof=self.f.proof();self.f.put('tests/results/SM-5-point/case.json',{**self.f.native,'Seconds':99});self.assertIsNone(R.record_evidence(self.f.native_path,self.f.native,self.f.after_context,proof))
    def test_unknown_record_rejected(self):self.assertIsNone(R.record_evidence(self.f.root/'unknown.json',self.f.native,self.f.after_context,self.f.proof()))
    def test_derived_compatibility_allowed_only_in_memory(self):
        v={**self.f.native,'ContextCompatibility':{'HistoricalEvidenceOnly':True}};self.assertIsNotNone(R.record_evidence(self.f.native_path,v,self.f.after_context,self.f.proof()))
    def test_unapproved_extra_field_rejected(self):self.assertIsNone(R.record_evidence(self.f.native_path,{**self.f.native,'FabricatedPass':True},self.f.after_context,self.f.proof()))
    def test_stale_export_input_rejected(self):
        self.f.export['CaseCapsulesSHA256']['case']='0'*64;self.f.put('tests/results/SM-5-point/family-export.json',self.f.export);self.f.records[1]['SummarySHA256']=R.digest(self.f.export_path);self.f.repin_snapshot();self.assertIsNone(R.record_evidence(self.f.export_path,self.f.export,self.f.after_context,self.f.proof()))
    def test_missing_proof_artifact_rejected(self):(self.f.root/'proof/before1.json').unlink();self.assertIsNone(self.f.status()['Proof'])
    def test_engine_tool_change_rejected(self):(self.f.here/'constructor_receipts.py').write_text('changed');self.assertIsNone(self.f.status()['Proof'])
    def test_checker_field_change_rejected_even_if_new_receipt_is_pinned(self):
        new=copy.deepcopy(self.f.raw_after);new[0]['Definitions'][self.f.checker]['DownValues']='{changedChecker}';new[1]['Definitions'][self.f.checker]['DownValues']='{changedChecker}'
        with self.assertRaisesRegex(ValueError,'Unreviewed'):R.analyze_transition(C,self.f.before,self.f.after,self.f.raw_before,new,{},self.f.loader,installed_root_captures=True)
    def test_constructor_messages_not_exempt(self):
        new=copy.deepcopy(self.f.raw_after);name=next(iter(R.CONSTRUCTOR_FIELDS));new[0]['Definitions'][name]['Messages']='{changed}';new[1]['Definitions'][name]['Messages']='{changed}'
        with self.assertRaises(ValueError):R.analyze_transition(C,self.f.before,self.f.after,self.f.raw_before,new,{},self.f.loader,installed_root_captures=True)
    def test_model_change_not_constructor_scope(self):
        after=copy.deepcopy(self.f.after);after['Files']['models/amps2.mdl']='changed'
        with self.assertRaises(ValueError):R.analyze_transition(C,self.f.before,after,self.f.raw_before,self.f.raw_after,{},self.f.loader,installed_root_captures=True)
    def test_numerical_program_change_rejected(self):
        after=copy.deepcopy(self.f.after);after['Programs']['CalcHEPDirectory']='changed'
        with self.assertRaises(ValueError):R.analyze_transition(C,self.f.before,after,self.f.raw_before,self.f.raw_after,{},self.f.loader,installed_root_captures=True)
    def test_missing_installed_root_capture_rejected(self):self.f.receipt['DefinitionInputs']['ActualInstalledRootCaptures']=False;self.f.pin();self.assertIsNone(self.f.status()['Proof'])
    def test_no_current_constructor_equivalence_claim(self):self.f.receipt['ScopeFlags']['ConstructorEquivalenceClaimed']=True;self.f.pin();self.assertIsNone(self.f.status()['Proof'])
    def test_no_current_validator_pass_claim(self):self.f.receipt['ScopeFlags']['CurrentValidatorPassClaimed']=True;self.f.pin();self.assertIsNone(self.f.status()['Proof'])
    def test_prior_conditional_history_cannot_upgrade(self):self.f.records[0]['PriorHistoricalAdmission']['CurrentValidatorPassClaimed']=True;self.f.repin_snapshot();self.assertIsNone(self.f.status()['Proof'])
    def test_one_changed_constructor_missing_is_rejected(self):
        new=copy.deepcopy(self.f.raw_after);name=next(iter(R.CONSTRUCTOR_FIELDS));new[0]['Definitions'][name]['DownValues']=self.f.old[name]['DownValues'];new[1]['Definitions'][name]['DownValues']=self.f.old[name]['DownValues']
        with self.assertRaises(ValueError):R.analyze_transition(C,self.f.before,self.f.after,self.f.raw_before,new,{},self.f.loader,installed_root_captures=True)

    def test_collector_current_ten_patterns_use_legacy_reader_without_legacy_gate(self):
        self.assertEqual(len(C.CAPTURE_PATTERNS),6)
        self.assertEqual(K.validate_current_pair(C,R,*self.f.raw_after),self.f.new)
        self.assertEqual(len(C.CAPTURE_PATTERNS),6)
    def test_collector_rejects_missing_qcd_or_adjoint_namespace(self):
        pair=copy.deepcopy(self.f.raw_after);pair[0]['CapturedNamePatterns']=list(C.CAPTURE_PATTERNS);pair[1]['CapturedNamePatterns']=list(C.CAPTURE_PATTERNS)
        with self.assertRaises(ValueError):K.validate_current_pair(C,R,*pair)
    def test_collector_still_requires_exact_cold_definition_pair(self):
        pair=copy.deepcopy(self.f.raw_after);pair[1]['Definitions'][self.f.checker]['DownValues']='changed'
        with self.assertRaises(ValueError):K.validate_current_pair(C,R,*pair)
    def test_missing_case_a_capsule_only_blocks_a_and_its_family(self):
        self.f.add_independent_strict_pair();self.f.native_path.with_suffix('.wxf').unlink();proof=self.f.proof()
        self.assertIsNone(R.record_evidence(self.f.native_path,self.f.native,self.f.after_context,proof))
        self.assertIsNone(R.record_evidence(self.f.export_path,self.f.export,self.f.after_context,proof))
        self.f.assert_b_available(self,proof)
    def test_missing_case_a_summary_only_blocks_a_and_its_family(self):
        self.f.add_independent_strict_pair();self.f.native_path.unlink();proof=self.f.proof()
        self.assertIsNone(R.record_evidence(self.f.native_path,self.f.native,self.f.after_context,proof))
        self.assertIsNone(R.record_evidence(self.f.export_path,self.f.export,self.f.after_context,proof))
        self.f.assert_b_available(self,proof)
    def test_missing_export_a_capsule_does_not_block_native_a_or_pair_b(self):
        self.f.add_independent_strict_pair();self.f.export_path.with_suffix('.wxf').unlink();proof=self.f.proof()
        self.assertIsNotNone(R.record_evidence(self.f.native_path,self.f.native,self.f.after_context,proof))
        self.assertIsNone(R.record_evidence(self.f.export_path,self.f.export,self.f.after_context,proof))
        self.f.assert_b_available(self,proof)
    def test_missing_pq_a_artifact_only_blocks_export_a(self):
        self.f.add_independent_strict_pair();dependency=self.f.put('tests/results/SM-5-point/precision/a-packet.json',{'genuine':'synthetic packet'})
        self.f.records[1]['PrecisionBindings']=[dependency];self.f.repin_snapshot();(self.f.root/dependency['Path']).unlink();proof=self.f.proof()
        self.assertIsNotNone(R.record_evidence(self.f.native_path,self.f.native,self.f.after_context,proof))
        self.assertIsNone(R.record_evidence(self.f.export_path,self.f.export,self.f.after_context,proof))
        self.f.assert_b_available(self,proof)
    def test_missing_replay_compiler_only_blocks_its_pq_export(self):
        self.f.add_independent_strict_pair();dependency=self.f.put('replay-only-compiler',{'synthetic':'compiler'})
        self.f.records[1]['PrecisionExternalBindings']=[{'Path':str(self.f.root/dependency['Path']),'SHA256':dependency['SHA256']}]
        self.f.repin_snapshot();(self.f.root/dependency['Path']).unlink();proof=self.f.proof()
        self.assertIsNotNone(R.record_evidence(self.f.native_path,self.f.native,self.f.after_context,proof))
        self.assertIsNone(R.record_evidence(self.f.export_path,self.f.export,self.f.after_context,proof));self.f.assert_b_available(self,proof)
    def test_missing_shared_immutable_proof_blocks_all(self):
        binding=self.f.put('proof/immutable-prerequisite.json',{'actual':'historical proof'})
        self.f.snapshot['SharedProofBindings']=[binding];self.f.repin_snapshot();(self.f.root/binding['Path']).unlink();self.assertIsNone(self.f.status()['Proof'])
    def test_unscoped_global_pq_bindings_are_rejected(self):
        self.f.snapshot['AuxiliaryBindings']=[self.f.put('tests/results/SM-5-point/precision/a.json',{})];self.f.repin_snapshot();self.assertIsNone(self.f.status()['Proof'])
    def test_historical_symlink_to_current_tool_is_rejected(self):
        target=self.f.here/'constructor_context.py';alias=self.f.root/'proof/historical-link.py';alias.symlink_to(target)
        self.f.receipt['HistoricalVerifierTools']=[{'Path':'proof/historical-link.py','SHA256':R.digest(target)}];self.f.pin();self.assertIsNone(self.f.status()['Proof'])
    def test_historical_hardlink_to_current_tool_is_rejected(self):
        import os
        target=self.f.here/'constructor_context.py';alias=self.f.root/'proof/historical-link.py';os.link(target,alias)
        self.f.receipt['HistoricalVerifierTools']=[{'Path':'proof/historical-link.py','SHA256':R.digest(target)}];self.f.pin();self.assertIsNone(self.f.status()['Proof'])
    def test_archived_actual_copy_with_same_bytes_is_allowed(self):
        import shutil
        target=self.f.here/'constructor_context.py';copy_path=self.f.root/'proof/historical-copy.py';shutil.copy2(target,copy_path)
        self.f.receipt['HistoricalVerifierTools']=[{'Path':'proof/historical-copy.py','SHA256':R.digest(target)}];self.f.pin();self.assertIsNotNone(self.f.status()['Proof'])
    def test_receipt_mutation_during_readback_is_rejected(self):
        original=R.object_at;path=self.f.root/'proof/receipt.json'
        def mutate(p):
            result=original(p)
            if Path(p).resolve()==path.resolve():path.write_text(json.dumps({**result,'Mutation':'during read'}))
            return result
        with patch.object(R,'object_at',side_effect=mutate):self.assertIsNone(self.f.status()['Proof'])
    def test_receipt_mutation_after_read_before_return_is_rejected(self):
        original=R.analyze_transition;path=self.f.root/'proof/receipt.json'
        def mutate(*args,**kwargs):
            result=original(*args,**kwargs);path.write_text(json.dumps({**self.f.receipt,'Mutation':'during analysis'}));return result
        with patch.object(R,'analyze_transition',side_effect=mutate):self.assertIsNone(self.f.status()['Proof'])
    def test_returned_proof_digest_is_exact_registry_pin(self):
        self.assertEqual(self.f.proof()['_ProofSHA256'],self.f.registry[self.f.after_context]['SHA256'])
    def test_in_memory_summary_types_are_exact(self):
        changed={**self.f.native,'Rows':True}
        self.assertIsNone(R.record_evidence(self.f.native_path,changed,self.f.after_context,self.f.proof()))
    def test_missing_old_verifier_archive_is_unavailable(self):
        (self.f.root/'proof/old-verifier-preimage.json').unlink();self.assertIsNone(self.f.status()['Proof'])
    def test_archived_tool_cannot_substitute_for_actual_adapter_binding(self):
        current=next(r for r in self.f.receipt['VerifierTools'] if Path(r['Path']).name=='supplemental_probes.py')
        path=self.f.root/'archive/supplemental_probes.py';path.parent.mkdir();path.write_bytes((self.f.here/'supplemental_probes.py').read_bytes());current['Path']='archive/supplemental_probes.py';self.f.pin();self.assertIsNone(self.f.status()['Proof'])
    def test_historical_tools_cannot_point_at_mutable_current_tools(self):
        self.f.receipt['HistoricalVerifierTools']=[self.f.receipt['VerifierTools'][0]];self.f.pin();self.assertIsNone(self.f.status()['Proof'])
    def test_missing_namespace_cannot_hide_checker_fields(self):
        before=copy.deepcopy(self.f.raw_before);before[0]['CapturedNamePatterns']=[];before[1]['CapturedNamePatterns']=[]
        with self.assertRaises(ValueError):R.analyze_transition(C,self.f.before,self.f.after,before,self.f.raw_after,{},self.f.loader,installed_root_captures=True)
    def test_separate_data_registry_has_no_self_hash_cycle(self):
        engine=self.f.here/'constructor_receipts.py';eh=R.digest(engine);rh=R.digest(self.f.root/'proof/receipt.json')
        self.f.put('tests/SM-5-point/'+R.REGISTRY_FILENAME,{'SchemaVersion':1,'Receipts':self.f.registry})
        stage=SimpleNamespace(fingerprint=R.canonical_digest,manifest=lambda *a:self.f.after)
        with patch.object(R,'_stage',return_value=stage):
            result=R.receipt_status(self.f.root,self.f.here,{},self.f.after_context,core=C)
        self.assertIsNotNone(result['Proof']);self.assertEqual(eh,R.digest(engine));self.assertEqual(rh,R.digest(self.f.root/'proof/receipt.json'))
        self.assertNotIn(R.REGISTRY_FILENAME,[Path(r['Path']).name for r in self.f.receipt['VerifierTools']])
    def test_missing_data_registry_is_explicit_unavailable(self):
        stage=SimpleNamespace(fingerprint=R.canonical_digest,manifest=lambda *a:self.f.after)
        with patch.object(R,'_stage',return_value=stage):result=R.receipt_status(self.f.root,self.f.here,{},self.f.after_context,core=C)
        self.assertEqual(result['Status'],'UNAVAILABLE')
    def test_registry_cannot_contain_executable_or_extra_fields(self):
        self.f.put('tests/SM-5-point/'+R.REGISTRY_FILENAME,{'SchemaVersion':1,'Receipts':self.f.registry,'Executable':'no'})
        stage=SimpleNamespace(fingerprint=R.canonical_digest,manifest=lambda *a:self.f.after)
        with patch.object(R,'_stage',return_value=stage):result=R.receipt_status(self.f.root,self.f.here,{},self.f.after_context,core=C)
        self.assertEqual(result['Status'],'UNAVAILABLE')
    def test_exact_old_probe_retains_actual_context(self):
        entry=self.f.make_probe();self.f.retain_probe(entry);report=self.f.probe_report([entry]);self.assertEqual(report['Status'],'PASS');self.assertEqual(report['Probes'][0]['ProbeContextSHA256'],self.f.before_context);self.assertFalse(report['Probes'][0]['CurrentValidatorPassClaimed'])
    def test_new_other_case_probe_does_not_invalidate_old_case(self):
        entry=self.f.make_probe();self.f.retain_probe(entry);other=self.f.make_probe('unrelated','newcase',self.f.after_context,'FAIL');self.assertEqual(self.f.probe_report([entry,other])['Status'],'PASS')
    def test_new_current_failure_blocks_same_old_case(self):
        entry=self.f.make_probe();self.f.retain_probe(entry);new=self.f.make_probe('newfailure','case',self.f.after_context,'FAIL');self.assertEqual(self.f.probe_report([entry,new])['Status'],'FAIL')
    def test_new_current_unavailable_blocks_same_old_case(self):
        entry=self.f.make_probe();self.f.retain_probe(entry);new=self.f.make_probe('newpending','case',self.f.after_context);(self.f.root/'tests/results/SM-5-point'/new['Files']['Summary']['Path']).unlink();self.assertEqual(self.f.probe_report([entry,new])['Status'],'UNAVAILABLE')
    def test_deleted_prior_id_is_unavailable_even_with_no_selected_probes(self):
        entry=self.f.make_probe();self.f.retain_probe(entry);self.assertEqual(self.f.probe_report([])['Status'],'UNAVAILABLE')
    def test_changed_old_probe_cannot_reuse_admission(self):
        entry=self.f.make_probe();self.f.retain_probe(entry);changed=copy.deepcopy(entry);changed['Status']='CHECKS_FAILED';self.assertEqual(self.f.probe_report([changed])['Status'],'UNAVAILABLE')
    def test_missing_old_probe_artifact_is_unavailable(self):
        entry=self.f.make_probe();self.f.retain_probe(entry);(self.f.root/'tests/results/SM-5-point'/entry['Files']['Capsule']['Path']).unlink();self.assertEqual(self.f.probe_report([entry])['Status'],'UNAVAILABLE')
    def test_current_replacement_for_prior_id_is_checked_normally(self):
        entry=self.f.make_probe();self.f.retain_probe(entry);proof=self.f.proof();current=self.f.make_probe('prior','case',self.f.after_context,'FAIL');self.assertEqual(self.f.probe_report([current],proof)['Status'],'FAIL')
    def test_new_current_case_probe_works_without_historical_shortcut(self):
        new=self.f.make_probe('newcaseprobe','newcase',self.f.after_context);self.assertEqual(self.f.probe_report([new],case_id='newcase')['Status'],'PASS')
    def test_live_registry_addition_does_not_change_receipt_admission(self):
        self.f.put('tests/SM-5-point/supplemental-probes.json',{'SchemaVersion':1,'Probes':[{'ID':'new','CaseID':'newcase'}]});self.assertIsNotNone(self.f.status()['Proof'])
    def test_supplemental_adapter_is_hash_bound(self):
        (self.f.here/'supplemental_probes.py').write_text('unreviewed adapter');self.assertIsNone(self.f.status()['Proof'])

if __name__=='__main__':unittest.main()
