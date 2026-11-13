# HEPCAT - High Energy Physics Constructive Amplitude Tool
# Copyright (C) 2026 Neil Christensen
# SPDX-License-Identifier: GPL-3.0-or-later
"""Synthetic fail-closed controls for the private historical-retention draft."""
from copy import deepcopy
from pathlib import Path
import importlib.util
import json
import re
import tempfile
import unittest

HERE=Path(__file__).resolve().parents[1]/'SM-5-point'
spec=importlib.util.spec_from_file_location('c11', HERE/'validation_context_c11.py')
m=importlib.util.module_from_spec(spec);spec.loader.exec_module(m)

class Core:
    @staticmethod
    def cold_pair(a,b,require_post=False):
        if a!=b:raise ValueError('Independent captures differ')
        return a['Definitions']
    @staticmethod
    def _loader_value(text):return int(re.search(r':> (\d+)\}',text).group(1))

def blank():return {name:'{}' for name in m.FIELDS}
def fixture():
    old={};new={}
    for name,fields in m.CHANGED.items():
        old[name]=blank();new[name]=blank()
        for f in fields:old[name][f]='old '+name;new[name][f]='new '+name
    old[m.NATIVE]['DownValues']='{HoldPattern['+m.NATIVE+'[x_]] :> x}'
    old[m.LOADER]=blank();new[m.LOADER]=blank()
    old[m.LOADER]['OwnValues']='{HoldPattern['+m.LOADER+'] :> 1}'
    new[m.LOADER]['OwnValues']='{HoldPattern['+m.LOADER+'] :> 2}'
    for name in m.NEW_HEADS:
        new[name]=blank();new[name]['DownValues']='new '+name
    new[m.NATIVE_ALIAS]['DownValues']=old[m.NATIVE]['DownValues'].replace('highEnergyLaurentJetCoefficients','highEnergyLaurentJetCoefficientsNative')
    new[m.P+'highEnergyJetCaptureZeroAssumptions']['Attributes']='{HoldAll}'
    for name,options in ((m.C+'calibratedCovariantSource','{"TimeConstraint" -> 60, "AugmentFourVectorContacts" -> False, "ProgressFunction" -> None}'),(m.C+'fourVectorMetricCalibration','{"TimeConstraint" -> 486, "ProgressFunction" -> None}')):
        new[name]['Options']=options;new[name]['DefaultValues']='{HoldPattern[Options['+name+']] :> '+options+'}'
    name=m.P+'unchangedChecker';old[name]=blank();new[name]=blank()
    old[name]['DownValues']='exact';new[name]['DownValues']='exact'
    for name in m.PATH_CACHES:
        old[name]=blank();new[name]=blank()
        old[name]['OwnValues']='{"/repo/source"}'
        new[name]['OwnValues']='{"/shadow/source"}'
    for name in (m.P+'newEmpty',m.P+'local$'):
        new[name]=blank()
    new[m.P+'local$']['Attributes']='{Temporary}'
    before={'Files':{n:'old' for n in m.FILES}|{'source/other.wl':'same'},'Programs':{'compiler':'same'}}
    after=deepcopy(before)
    for n in m.FILES:after['Files'][n]='new'
    raw=lambda defs:{'Definitions':defs,'WolframVersion':'same','SystemID':'same'}
    return before,after,raw(old),raw(new),{'Before':1,'After':2,'InputCount':17,'_VerifiedInputHashes':True}

class Transition(unittest.TestCase):
    def setUp(self):self.a,self.b,self.o,self.n,self.e=fixture()
    def run_analysis(self):return m.compare_transition(Core,self.a,self.b,(self.o,self.o),(self.n,self.n),self.e,shadow_paths=('/repo/source','/shadow/source'))
    def test_exact_declared_transition_analyzed_not_sealed(self):
        r=self.run_analysis();self.assertEqual(r['Status'],'ANALYZED_NOT_SEALED');self.assertTrue(r['ValidationImplementationChanged']);self.assertFalse(r['ValidationRerunPerformed']);self.assertFalse(r['UniversalCheckerEquivalenceClaimed'])
    def test_other_model_file_change_rejected(self):
        self.b['Files']['source/other.wl']='changed'
        with self.assertRaises(ValueError):self.run_analysis()
    def test_new_manifest_path_rejected(self):
        self.b['Files']['new']='x'
        with self.assertRaises(ValueError):self.run_analysis()
    def test_runtime_program_change_rejected(self):
        self.b['Programs']['compiler']='changed'
        with self.assertRaises(ValueError):self.run_analysis()
    def test_public_options_rejected(self):
        self.n['Definitions']['ConstructiveDiagrams`ConstructAmplitude']['Options']='new'
        with self.assertRaises(ValueError):self.run_analysis()
    def test_public_message_rejected(self):
        self.n['Definitions']['ConstructiveDiagrams`ConstructAmplitude']['Messages']='deleted usage'
        with self.assertRaises(ValueError):self.run_analysis()
    def test_unchanged_checker_change_rejected(self):
        self.n['Definitions'][m.P+'unchangedChecker']['DownValues']='different'
        with self.assertRaises(ValueError):self.run_analysis()
    def test_missing_old_symbol_rejected(self):
        del self.n['Definitions'][m.P+'unchangedChecker']
        with self.assertRaises(ValueError):self.run_analysis()
    def test_missing_declared_change_rejected(self):
        n=m.S+'liftVertex';self.n['Definitions'][n]=deepcopy(self.o['Definitions'][n])
        with self.assertRaises(ValueError):self.run_analysis()
    def test_unverified_loader_rejected(self):
        self.e['_VerifiedInputHashes']=False
        with self.assertRaises(ValueError):self.run_analysis()
    def test_old_thirteen_loader_input_assumption_rejected(self):
        self.e['InputCount']=13
        with self.assertRaises(ValueError):self.run_analysis()
    def test_wrong_loader_value_rejected(self):
        self.e['After']=3
        with self.assertRaises(ValueError):self.run_analysis()
    def test_native_alias_body_change_rejected(self):
        self.n['Definitions'][m.NATIVE_ALIAS]['DownValues']+='changed'
        with self.assertRaises(ValueError):self.run_analysis()
    def test_unlisted_new_helper_rejected(self):
        self.n['Definitions'][m.P+'unexpected']=blank();self.n['Definitions'][m.P+'unexpected']['DownValues']='x'
        with self.assertRaises(ValueError):self.run_analysis()
    def test_new_temporary_state_rejected(self):
        self.n['Definitions'][m.P+'local$']['OwnValues']='1'
        with self.assertRaises(ValueError):self.run_analysis()
    def test_capture_hold_required(self):
        self.n['Definitions'][m.P+'highEnergyJetCaptureZeroAssumptions']['Attributes']='{}'
        with self.assertRaises(ValueError):self.run_analysis()
    def test_private_options_exact(self):
        self.n['Definitions'][m.C+'calibratedCovariantSource']['Options']='{}'
        with self.assertRaises(ValueError):self.run_analysis()
    def test_unknown_path_cache_change_rejected(self):
        self.n['Definitions'][m.PATH_CACHES[0]]['OwnValues']='different'
        with self.assertRaises(ValueError):self.run_analysis()
    def test_kernel_version_change_rejected(self):
        self.n['WolframVersion']='different'
        with self.assertRaises(ValueError):self.run_analysis()
    def test_independent_capture_disagreement_rejected(self):
        raw=deepcopy(self.n);raw['SystemID']='other'
        with self.assertRaises(ValueError):m.compare_transition(Core,self.a,self.b,(self.o,self.o),(self.n,raw),self.e)

class History(unittest.TestCase):
    def setUp(self):
        self.temp=tempfile.TemporaryDirectory();self.root=Path(self.temp.name)
        self.summary={'ContextSHA256':'original-context','CapsuleSHA256':'','Status':'COMPLETE'}
        self.cap=self.root/'cap.wxf';self.cap.write_bytes(b'original exact capsule')
        self.summary['CapsuleSHA256']=m.file_digest(self.cap)
        self.path=self.root/'summary.json';self.path.write_text(json.dumps(self.summary))
        self.frozen={'RawSummary':deepcopy(self.summary),'ActualProducerContextSHA256':'original-context','CapsuleSHA256':m.file_digest(self.cap),'SummarySHA256':m.file_digest(self.path)}
        self.manifest={'Files':{'new':'x'}}
        self.analysis={'AfterManifestSHA256':m.digest_object(self.manifest),'ValidationImplementationChanged':True,'HistoricalEvidenceOnly':True,'ValidationRerunPerformed':False}
        self.review={'Status':'SEALED_C11_HISTORICAL_RETENTION','AnalysisSHA256':m.digest_object(self.analysis),'CurrentManifestSHA256':m.digest_object(self.manifest),'FiniteSnapshotRowDigests':[m.digest_object(self.frozen)],**{f:True for f in m.REVIEW_FLAGS}}
    def tearDown(self):self.temp.cleanup()
    def run_history(self,**kw):return m.historical_record(self.path,self.summary,self.cap,self.frozen,self.review,self.analysis,current_manifest=self.manifest,**kw)
    def test_history_keeps_actual_context_and_never_current_validation(self):
        r=self.run_history();self.assertEqual(r['ActualProducerContextSHA256'],'original-context');self.assertFalse(r['CurrentValidatorPassClaimed']);self.assertFalse(r['NewConstructorSuccessClaimed']);self.assertFalse(r['ValidationRerunPerformed'])
    def test_deleted_disposable_cache_rejected_without_writes(self):
        self.cap.unlink();self.assertIsNone(self.run_history());self.assertFalse(self.cap.exists())
    def test_changed_capsule_rejected(self):
        self.cap.write_bytes(b'changed');self.assertIsNone(self.run_history())
    def test_changed_summary_bytes_rejected(self):
        self.path.write_text(' ');self.assertIsNone(self.run_history())
    def test_producer_context_relabel_rejected(self):
        self.summary['ContextSHA256']='current';self.assertIsNone(self.run_history())
    def test_derived_precision_and_probe_annotations_preserve_raw_record(self):
        self.summary.update(ContextCompatibility={'Status':'historical'},PrecisionQualification={'Status':'PRECISION_QUALIFIED'},SupplementalParameterProbes={'Status':'PASS'})
        self.assertIsNotNone(self.run_history())
    def test_annotation_cannot_override_raw_failure(self):
        self.summary['Status']='PASS';self.assertIsNone(self.run_history())
    def test_unknown_extra_raw_claim_rejected(self):
        self.summary['NewCurrentValidation']='PASS';self.assertIsNone(self.run_history())
    def test_missing_finite_row_rejected(self):
        self.review['FiniteSnapshotRowDigests']=[];self.assertIsNone(self.run_history())
    def test_missing_conditional_review_flag_rejected(self):
        del self.review[m.REVIEW_FLAGS[0]];self.assertIsNone(self.run_history())
    def test_known_counterexample_requires_narrow_replay(self):self.assertIsNone(self.run_history(known_counterexamples=('actual ordinary-worker false PASS',)))
    def test_unreviewed_analysis_rejected(self):
        self.analysis['ValidationRerunPerformed']=True;self.assertIsNone(self.run_history())
    def test_current_manifest_change_rejected(self):
        self.manifest['Files']['new']='changed';self.assertIsNone(self.run_history())

class Runtime(unittest.TestCase):
    def setUp(self):
        import types
        self.temp=tempfile.TemporaryDirectory();self.root=Path(self.temp.name)
        self.here=self.root/'tests/SM-5-point';self.here.mkdir(parents=True)
        self.cache=self.root/m.CACHE;self.cache.mkdir(parents=True)
        self.tools={}
        for name in ('validation_context_c11.py','constructor_context.py','stage_context.py'):
            path=self.here/name;path.write_text('synthetic immutable code '+name)
            self.tools[str(path.relative_to(self.root))]=m.file_digest(path)
        self.before={'Programs':{'same':'same'},'Files':{n:'old' for n in m.FILES}|{'model':'same'}}
        self.after=deepcopy(self.before)
        for n in m.FILES:self.after['Files'][n]='new'
        self.saved_stage=m._stage
        m._stage=lambda root:types.SimpleNamespace(manifest=lambda *a:self.after,fingerprint=lambda x:m.BASE_CONTEXT if x==self.before else m.AFTER_CONTEXT)
        results=self.root/'tests/results/SM-5-point';results.mkdir(parents=True,exist_ok=True)
        self.cap=results/'case.wxf';self.cap.write_bytes(b'actual finite old capsule')
        self.summary={'ID':'case','ContextSHA256':'older producer','Status':'COMPLETE','CapsuleSHA256':m.file_digest(self.cap)}
        self.summary_path=results/'case.json';self.summary_path.write_text(json.dumps(self.summary))
        self.row={'ID':'case','Kind':'Native','SummaryPath':str(self.summary_path.relative_to(self.root)),'CapsulePath':str(self.cap.relative_to(self.root)),'RawSummary':deepcopy(self.summary),'SummarySHA256':m.file_digest(self.summary_path),'CapsuleSHA256':m.file_digest(self.cap),'ActualProducerContextSHA256':'older producer','AdmittedAtContextSHA256':m.BASE_CONTEXT}
        self.analysis={'Status':'ANALYZED_NOT_SEALED','ChangedExistingFields':{**m.CHANGED,m.LOADER:('OwnValues',)},'NewSymbols':{name:'Defined' for name in m.NEW_HEADS},'NativeCoreExactHeadRename':True,'ShadowPathArtifacts':{},'BeforeManifestSHA256':m.digest_object(self.before),'AfterManifestSHA256':m.digest_object(self.after),'ValidationImplementationChanged':True,'HistoricalEvidenceOnly':True,'ValidationRerunPerformed':False,'UniversalCheckerEquivalenceClaimed':False}
        self.review={'Status':'SEALED_C11_HISTORICAL_RETENTION','ReviewedBy':'synthetic fixture only','AnalysisSHA256':m.digest_object(self.analysis),'CurrentManifestSHA256':m.digest_object(self.after),'FiniteSnapshotRowDigests':[m.digest_object(self.row)],**{f:True for f in m.REVIEW_FLAGS}}
        self.snapshot={'Status':'FINAL_C10_ADMITTED_RECORD_SNAPSHOT','AdmissionContextSHA256':m.BASE_CONTEXT,'ActualProducerContextsPreserved':True,'CurrentC11ValidationClaimed':False,'CapsulesCopied':False,'Records':[self.row]}
        self.seal={'SchemaVersion':1,'Kind':m.KIND,'Status':'SEALED','BeforeContextSHA256':m.BASE_CONTEXT,'AfterContextSHA256':m.AFTER_CONTEXT,'CurrentValidatorPassClaimed':False,'ValidationRerunPerformed':False,'VerifierToolsSHA256':self.tools}
        for name,value in (('BeforeManifest',self.before),('AfterManifest',self.after),('Analysis',self.analysis),('Review',self.review),('Snapshot',self.snapshot)):
            path=self.cache/(name+'.json');path.write_text(json.dumps(value));self.seal[name]={'Path':str(path.relative_to(self.root)),'SHA256':m.file_digest(path)}
        self.seal_path=self.root/m.REGISTRY;self.seal_path.write_text(json.dumps(self.seal))
    def tearDown(self):m._stage=self.saved_stage;self.temp.cleanup()
    def proof(self):return m.verified_proof(self.root,self.here,{},m.AFTER_CONTEXT)
    def rewrite(self,name,value):
        path=self.root/self.seal[name]['Path'];path.write_text(json.dumps(value));self.seal[name]['SHA256']=m.file_digest(path);self.seal_path.write_text(json.dumps(self.seal))
    def test_reviewed_finite_history_resolves_without_mutating_files(self):
        before=self.summary_path.read_bytes();proof=self.proof();self.assertIsNotNone(proof)
        evidence=m.record_evidence(self.summary_path,self.summary,m.AFTER_CONTEXT,proof)
        self.assertFalse(evidence['CurrentValidatorPassClaimed']);self.assertEqual(evidence['RecordedContextSHA256'],'older producer');self.assertEqual(self.summary_path.read_bytes(),before)
    def test_missing_history_cache_returns_unavailable(self):
        self.seal_path.unlink();self.assertIsNone(self.proof())
    def test_missing_snapshot_returns_unavailable(self):
        (self.root/self.seal['Snapshot']['Path']).unlink();self.assertIsNone(self.proof())
    def test_changed_bound_snapshot_rejected(self):
        (self.root/self.seal['Snapshot']['Path']).write_text('{}');self.assertIsNone(self.proof())
    def test_changed_verifier_rejected(self):
        (self.here/'constructor_context.py').write_text('changed');self.assertIsNone(self.proof())
    def test_additional_verifier_binding_rejected(self):
        self.seal['VerifierToolsSHA256']['unrelated']='x';self.seal_path.write_text(json.dumps(self.seal));self.assertIsNone(self.proof())
    def test_wrong_current_context_rejected(self):
        self.assertIsNone(m.verified_proof(self.root,self.here,{},'unadmitted future'))
    def test_unreviewed_usage_or_shadow_path_exception_rejected(self):
        self.analysis['ShadowPathArtifacts']={'one':'exception'};self.rewrite('Analysis',self.analysis);self.assertIsNone(self.proof())
    def test_missing_conditional_review_rejected(self):
        del self.review[m.REVIEW_FLAGS[0]];self.rewrite('Review',self.review);self.assertIsNone(self.proof())
    def test_unlisted_finite_record_rejected(self):
        proof=self.proof();other=self.summary_path.with_name('other.json');other.write_bytes(self.summary_path.read_bytes());other.with_suffix('.wxf').write_bytes(self.cap.read_bytes());self.assertIsNone(m.record_evidence(other,self.summary,m.AFTER_CONTEXT,proof))
    def test_tampered_capsule_rejected_by_record_check(self):
        proof=self.proof();self.cap.write_bytes(b'changed');self.assertIsNone(m.record_evidence(self.summary_path,self.summary,m.AFTER_CONTEXT,proof))
    def test_runtime_model_change_rejected(self):
        self.after['Files']['model']='changed';self.assertIsNone(self.proof())
    def test_outside_cache_path_rejected(self):
        self.seal['Analysis']['Path']='../outside';self.seal_path.write_text(json.dumps(self.seal));self.assertIsNone(self.proof())
    def test_nonfinite_snapshot_row_rejected(self):
        self.review['FiniteSnapshotRowDigests']=[];self.rewrite('Review',self.review);self.assertIsNone(self.proof())

if __name__=='__main__':unittest.main(verbosity=2)
