"""Pure metadata controls for the finite private C12 transition draft."""
from copy import deepcopy
from pathlib import Path
import importlib.util,os,unittest
HERE=Path(__file__).resolve().parent
ROOT=Path(os.environ.get('HEPCAT_TEST_REPOSITORY',Path(__file__).resolve().parents[2]))
def load(name,path):
 s=importlib.util.spec_from_file_location(name,path);m=importlib.util.module_from_spec(s);s.loader.exec_module(m);return m
M=load('c12_private_draft',Path(os.environ.get('HEPCAT_TEST_C12_HELPER',ROOT/'tests/SM-5-point/validation_context_c12.py')))
CORE=load('c12_existing_field_reader',ROOT/'tests/SM-5-point/constructor_context.py')
def blank():return {field:'{}' for field in M.FIELDS}
def raw(table):
 return {'Definitions':table,'SymbolCount':len(table),'CapturedNamePatterns':list(M.PATTERNS),'WolframVersion':'same','SystemID':'same','PackageLoadedOnce':True,'ModelsRead':[],'PhysicsCalculationsPerformed':False}
def fixture():
 old={};new={}
 for name in (M.COLOUR,M.SIGNATURE,M.LOADER,'ConstructiveDiagrams`CheckAmplitudes',*M.PATH_CACHES):old[name]=blank();new[name]=blank()
 old[M.COLOUR]['DownValues']='old colour constructor';new[M.COLOUR]['DownValues']='new colour proposal gate'
 old[M.SIGNATURE]['DownValues']='Hash[{"internal/HEPCAT-ColoredCurrents.wl", "next"}]'
 new[M.SIGNATURE]['DownValues']='Hash[{"internal/HEPCAT-ColoredCurrents.wl", "'+M.NEW_MODULE+'", "next"}]'
 old[M.LOADER]['OwnValues']='{HoldPattern['+M.LOADER+'] :> 1}';new[M.LOADER]['OwnValues']='{HoldPattern['+M.LOADER+'] :> 2}'
 old['ConstructiveDiagrams`CheckAmplitudes']['DownValues']='unchanged actual validator';new['ConstructiveDiagrams`CheckAmplitudes']['DownValues']='unchanged actual validator'
 for name in M.PATH_CACHES:old[name]['OwnValues']=new[name]['OwnValues']='{"/root/source"}'
 for name in M.NEW_HEADS:new[name]=blank();new[name]['DownValues']='defined '+name
 for i in range(171):new[M.Q+'empty'+str(i)]=blank()
 for i in range(152):new[M.Q+'local'+str(i)+'$']=blank();new[M.Q+'local'+str(i)+'$']['Attributes']='{Temporary}'
 inventory={'Counts':{'Defined':16,'Empty':171,'Temporary':152},'NamesAndFieldHashes':[]}
 for name in sorted(new.keys()-old.keys()):
  fields=new[name];nonempty={k:v for k,v in fields.items() if v!='{}'};kind='Defined' if name in M.NEW_HEADS else 'Temporary' if nonempty else 'Empty'
  inventory['NamesAndFieldHashes'].append({'Name':name,'Kind':kind,'FieldsSHA256':M.digest_object(fields),'NonemptyFields':nonempty})
 old_text={'source/HEPCAT.wl':'loader{"old"}end','source/HEPCAT-Model.wl':'model before\nmodel after\n','source/internal/HEPCAT-Color.wl':'old colour'}
 new_text={'source/HEPCAT.wl':'loader{"old",\n    "'+M.NEW_MODULE+'"}end','source/HEPCAT-Model.wl':'model before\n      "'+M.NEW_MODULE+'",\nmodel after\n','source/internal/HEPCAT-Color.wl':'new colour',M.ADDED_FILE:'new declared private source'}
 before={'Files':{k:M.sha_text(v) for k,v in old_text.items()}|{'models/amps2.mdl':'same','/native/lib.a':'same'},'Programs':{'spinas':'same'}}
 after={'Files':{k:M.sha_text(v) for k,v in new_text.items()}|{'models/amps2.mdl':'same','/native/lib.a':'same'},'Programs':{'spinas':'same'}}
 evidence={'_VerifiedInputHashes':True,'BeforeInputCount':17,'AfterInputCount':18,'Before':1,'After':2}
 return before,after,raw(old),raw(new),inventory,evidence,old_text,new_text

class C12MetadataTests(unittest.TestCase):
 def setUp(self):self.before,self.after,self.old,self.new,self.inventory,self.evidence,self.old_text,self.new_text=fixture()
 def analyze(self,**kwargs):return M.analyze(CORE,self.before,self.after,(self.old,self.old),(self.new,self.new),self.inventory,self.evidence,self.old_text,self.new_text,**kwargs)
 def reject(self):
  with self.assertRaises(ValueError):self.analyze()
 def test_exact_declared_transition(self):
  value=self.analyze();self.assertEqual(value['Status'],'ANALYZED_NOT_SEALED');self.assertTrue(value['AllExistingValidatorFieldsLiterallyUnchanged']);self.assertTrue(value['ExistingC11ConditionalHistoryMustRemain']);self.assertFalse(value['CurrentC12ValidationRerunClaimed']);self.assertFalse(value['OldC10QCDPilotHEUpgradeImplied']);self.assertFalse(value['PrivatePilotsOutsideFiniteSnapshotAdmitted'])
 def test_old_six_patterns_alone_reject(self):self.new['CapturedNamePatterns']=list(M.PATTERNS[:6]);self.reject()
 def test_reordered_original_patterns_reject(self):self.new['CapturedNamePatterns'][:2]=reversed(self.new['CapturedNamePatterns'][:2]);self.reject()
 def test_extra_global_namespace_reject(self):self.new['CapturedNamePatterns'].append('Global`*');self.reject()
 def test_models_read_reject(self):self.new['ModelsRead']=['model1'];self.reject()
 def test_physics_capture_reject(self):self.new['PhysicsCalculationsPerformed']=True;self.reject()
 def test_platform_change_reject(self):self.new['SystemID']='other';self.reject()
 def test_independent_capture_mismatch_reject(self):
  second=deepcopy(self.new);second['Definitions'][M.COLOUR]['DownValues']+='other'
  with self.assertRaises(ValueError):M.analyze(CORE,self.before,self.after,(self.old,self.old),(self.new,second),self.inventory,self.evidence,self.old_text,self.new_text)
 def test_old_validator_change_reject(self):self.new['Definitions']['ConstructiveDiagrams`CheckAmplitudes']['DownValues']='altered';self.reject()
 def test_old_message_change_reject(self):self.new['Definitions'][M.COLOUR]['Messages']='deleted usage';self.reject()
 def test_old_options_change_reject(self):self.new['Definitions'][M.COLOUR]['Options']='new default';self.reject()
 def test_old_attribute_change_reject(self):self.new['Definitions'][M.COLOUR]['Attributes']='{Protected}';self.reject()
 def test_old_symbol_disappears_reject(self):del self.new['Definitions']['ConstructiveDiagrams`CheckAmplitudes'];self.new['SymbolCount']-=1;self.reject()
 def test_missing_expected_ctor_change_reject(self):self.new['Definitions'][M.COLOUR]=deepcopy(self.old['Definitions'][M.COLOUR]);self.reject()
 def test_hash_formula_change_reject(self):self.new['Definitions'][M.SIGNATURE]['DownValues']=self.new['Definitions'][M.SIGNATURE]['DownValues'].replace('Hash','OtherHash');self.reject()
 def test_mechanical_old_input_count_reject(self):self.evidence['BeforeInputCount']=13;self.reject()
 def test_mechanical_new_input_count_reject(self):self.evidence['AfterInputCount']=17;self.reject()
 def test_unverified_loader_inputs_reject(self):self.evidence['_VerifiedInputHashes']=False;self.reject()
 def test_wrong_loader_integer_reject(self):self.evidence['After']=3;self.reject()
 def test_new_helper_ownvalue_reject(self):self.new['Definitions'][M.NEW_HEADS[0]]['OwnValues']='1';self.reject()
 def test_new_helper_options_reject(self):self.new['Definitions'][M.NEW_HEADS[0]]['Options']='{} new';self.reject()
 def test_new_empty_state_reject(self):self.new['Definitions'][M.Q+'empty0']['OwnValues']='7';self.reject()
 def test_new_temporary_extra_attribute_reject(self):self.new['Definitions'][M.Q+'local0$']['Attributes']='{Temporary,Protected}';self.reject()
 def test_uninventoried_temporary_reject(self):self.new['Definitions'][M.Q+'unexpected$']=blank();self.new['Definitions'][M.Q+'unexpected$']['Attributes']='{Temporary}';self.new['SymbolCount']+=1;self.reject()
 def test_new_public_symbol_reject_even_matching_inventory(self):
  name=M.Q+'empty0';fields=self.new['Definitions'].pop(name);replacement='HEPCATMinimalQCDCurrentSources`publicLeak';self.new['Definitions'][replacement]=fields
  row=next(r for r in self.inventory['NamesAndFieldHashes'] if r['Name']==name);row['Name']=replacement;self.inventory['NamesAndFieldHashes'].sort(key=lambda r:r['Name']);self.reject()
 def test_inventory_hash_mutation_reject(self):self.inventory['NamesAndFieldHashes'][0]['FieldsSHA256']='wrong';self.reject()
 def test_unsorted_name_inventory_reject(self):self.inventory['NamesAndFieldHashes'].reverse();self.reject()
 def test_wrong_inventory_kind_reject(self):self.inventory['NamesAndFieldHashes'][0]['Kind']='Empty';self.reject()
 def test_suffix_alone_does_not_authorize_new_local(self):
  old=M.Q+'local0$';self.new['Definitions'][M.Q+'foreign$']=self.new['Definitions'].pop(old);self.reject()
 def test_current_model_file_change_reject(self):self.after['Files']['models/amps2.mdl']='changed';self.reject()
 def test_current_runtime_library_change_reject(self):self.after['Files']['/native/lib.a']='changed';self.reject()
 def test_program_location_change_reject(self):self.after['Programs']['spinas']='other';self.reject()
 def test_second_added_source_file_reject(self):self.after['Files']['source/extra.wl']='x';self.reject()
 def test_existing_input_removed_reject(self):del self.after['Files']['models/amps2.mdl'];self.reject()
 def test_wrong_added_module_reject(self):self.after['Files']['source/other.wl']=self.after['Files'].pop(M.ADDED_FILE);self.reject()
 def test_source_text_not_manifest_bound_reject(self):self.new_text[M.ADDED_FILE]+='changed';self.reject()
 def test_model_body_change_beyond_registry_reject(self):
  self.new_text['source/HEPCAT-Model.wl']+='bad';self.after['Files']['source/HEPCAT-Model.wl']=M.sha_text(self.new_text['source/HEPCAT-Model.wl']);self.reject()
 def test_loader_body_change_beyond_get_reject(self):
  self.new_text['source/HEPCAT.wl']+='bad';self.after['Files']['source/HEPCAT.wl']=M.sha_text(self.new_text['source/HEPCAT.wl']);self.reject()
 def test_private_path_artifacts_explicit_and_not_installable(self):
  for name in M.PATH_CACHES:self.new['Definitions'][name]['OwnValues']='{"/private/source"}'
  value=self.analyze(shadow_paths=('/root/source','/private/source'));self.assertEqual(len(value['ShadowPathArtifacts']),3);self.assertFalse(value['ReadyForInstalledSeal'])
 def test_private_path_artifacts_reject_without_opt_in(self):self.new['Definitions'][M.PATH_CACHES[0]]['OwnValues']='{"/private/source"}';self.reject()
 def test_foreign_path_change_reject(self):
  self.new['Definitions'][M.PATH_CACHES[0]]['OwnValues']='{"/foreign/source"}'
  with self.assertRaises(ValueError):self.analyze(shadow_paths=('/root/source','/private/source'))


from types import SimpleNamespace
import json,tempfile
from unittest.mock import patch

class C12RuntimeTests(unittest.TestCase):
    def setUp(self):
        self.tmp=tempfile.TemporaryDirectory();self.addCleanup(self.tmp.cleanup)
        self.root=Path(self.tmp.name);self.here=self.root/'tests/SM-5-point';self.here.mkdir(parents=True)
        self.cache=self.root/'cache';self.cache.mkdir();self.seal_path=self.cache/'seal.json'
        self.before={'Files':{name:'old-'+name for name in M.CHANGED_FILES}|{'models/amps2.mdl':'unchanged'},'Programs':{'compiler':'unchanged'}}
        self.after=deepcopy(self.before)
        for name in M.CHANGED_FILES:self.after['Files'][name]='new-'+name
        self.after['Files'][M.ADDED_FILE]='new-module'
        self.analysis={'Status':'ANALYZED_NOT_SEALED','ChangedExistingFields':M.EXPECTED_CHANGED,'NewSymbolCounts':{'Defined':16,'Empty':171,'Temporary':152},'BeforeManifestSHA256':M.digest_object(self.before),'AfterManifestSHA256':M.digest_object(self.after),'ShadowPathArtifacts':{},'ReadyForInstalledSeal':True,'ActualInstalledRootCaptures':True,'AllExistingValidatorFieldsLiterallyUnchanged':True,'ExistingC11ConditionalHistoryMustRemain':True,'HistoricalProducerContextsMustRemain':True,'CurrentC12ValidationRerunClaimed':False,'CurrentC12ConstructorRerunClaimed':False,'PrivatePilotsOutsideFiniteSnapshotAdmitted':False,'OldC10QCDPilotHEUpgradeImplied':False}
        self.tools={}
        for name in M.VERIFIER_TOOLS:
            path=self.root/name;path.parent.mkdir(parents=True,exist_ok=True);path.write_text(name);self.tools[name]=M.file_digest(path)
        self.raw={'Status':'FAIL','ContextSHA256':'actual-old-producer','CapsuleSHA256':''}
        self.target=self.root/'records/0.json';self.target.parent.mkdir();self.target.with_suffix('.wxf').write_bytes(b'exact saved capsule');self.raw['CapsuleSHA256']=M.file_digest(self.target.with_suffix('.wxf'));self.target.write_text(json.dumps(self.raw))
        self.rows=[]
        for i in range(609):
            self.rows.append({'ID':str(i),'Kind':'Native' if i<356 else 'Export','SummaryPath':f'records/{i}.json','CapsulePath':f'records/{i}.wxf','SummarySHA256':M.file_digest(self.target),'CapsuleSHA256':self.raw['CapsuleSHA256'],'ActualProducerContextSHA256':'actual-old-producer','RawSummary':deepcopy(self.raw),'AdmittedAtContextSHA256':M.BASE_CONTEXT,'C12ValidationRerunClaimed':False,'ExistingC11HistoricalAdmission':{'CurrentValidatorPassClaimed':False,'ValidationRerunPerformed':False,'ConditionalNormalFreshWorkerContractOnly':True}})
        self.snapshot={'Status':'FINAL_C11_ADMITTED_RECORD_SNAPSHOT','AdmissionContextSHA256':M.BASE_CONTEXT,'ActualProducerContextsPreserved':True,'ExistingC11ConditionalHEHistoryPreserved':True,'ExistingC11HistoryNotUpgradedToLiteralCheckerEquivalence':True,'C12ValidationRerunClaimed':False,'CapsulesCopied':False,'Records':self.rows}
        self.review={'Status':'SEALED_C12_CONSTRUCTOR_ONLY_HISTORICAL_RETENTION','BeforeContextSHA256':M.BASE_CONTEXT,'AfterContextSHA256':M.AFTER_CONTEXT,'ReviewedBy':'independent root','VerifierToolsSHA256':self.tools,'FiniteSnapshotRowDigests':[M.digest_object(row) for row in self.rows],**dict.fromkeys(M.REVIEW_FLAGS,True)}
        self.seal={'SchemaVersion':1,'Status':'SEALED','Kind':M.KIND,'BeforeContextSHA256':M.BASE_CONTEXT,'AfterContextSHA256':M.AFTER_CONTEXT,'CurrentValidatorPassClaimed':False,'ValidationRerunPerformed':False,'CurrentConstructorSuccessClaimed':False,'VerifierToolsSHA256':self.tools}
        self.stage=SimpleNamespace(manifest=lambda *args:deepcopy(self.after),fingerprint=lambda value:M.BASE_CONTEXT if value==self.before else M.AFTER_CONTEXT)
        self.save()
    def bind(self,name,value):
        path=self.cache/(name+'.json');path.write_text(json.dumps(value));return {'Path':str(path.relative_to(self.root)),'SHA256':M.file_digest(path)}
    def save(self):
        for key,value in [('BeforeManifest',self.before),('AfterManifest',self.after),('Analysis',self.analysis),('Snapshot',self.snapshot)]:self.seal[key]=self.bind(key,value)
        self.review['AnalysisSHA256']=self.seal['Analysis']['SHA256'];self.seal['Review']=self.bind('Review',self.review);self.seal_path.write_text(json.dumps(self.seal))
    def verify(self):
        with patch.object(M,'_stage',return_value=self.stage):return M.verified_proof(self.root,self.here,{},M.AFTER_CONTEXT,self.seal_path)
    def reject(self):self.save();self.assertIsNone(self.verify())
    def evidence(self,proof=None):return M.record_evidence(self.target,self.raw,M.AFTER_CONTEXT,proof or self.verify())
    def test_exact_finite_proof(self):self.assertTrue(self.verify()['_C12VerifiedCurrentContext'])
    def test_raw_fail_retained_without_validation_claim(self):
        e=self.evidence();self.assertEqual(e['RecordedStatus'],'FAIL');self.assertFalse(e['CurrentValidatorPassClaimed']);self.assertFalse(e['ValidationRerunPerformed']);self.assertEqual(e['ActualProducerContextSHA256'],'actual-old-producer')
    def test_prior_conditional_annotation_exact(self):self.assertEqual(self.evidence()['ExistingC11HistoricalAdmission'],self.rows[0]['ExistingC11HistoricalAdmission'])
    def test_missing_cache_unavailable(self):self.seal_path.unlink();self.assertIsNone(self.verify())
    def test_unknown_context_reject(self):self.assertIsNone(M.verified_proof(self.root,self.here,{},'unadmitted',self.seal_path))
    def test_altered_bound_file_reject(self):(self.cache/'Analysis.json').write_text('{}');self.assertIsNone(self.verify())
    def test_unreviewed_flag_reject(self):self.review[M.REVIEW_FLAGS[0]]=False;self.reject()
    def test_current_validation_claim_reject(self):self.seal['CurrentValidatorPassClaimed']=True;self.reject()
    def test_old_he_upgrade_reject(self):self.analysis['OldC10QCDPilotHEUpgradeImplied']=True;self.reject()
    def test_shadow_path_artifact_reject(self):self.analysis['ShadowPathArtifacts']={'a':['OwnValues']};self.reject()
    def test_source_model_change_reject(self):
        actual=deepcopy(self.after);actual['Files']['models/amps2.mdl']='changed';self.stage.manifest=lambda *args:actual;self.assertIsNone(self.verify())
    def test_second_unreviewed_source_change_reject(self):self.after['Files']['models/amps2.mdl']='changed';self.reject()
    def test_runtime_program_change_reject(self):self.after['Programs']['compiler']='changed';self.reject()
    def test_added_unknown_file_reject(self):self.after['Files']['source/other.wl']='changed';self.reject()
    def test_missing_added_qcd_module_reject(self):del self.after['Files'][M.ADDED_FILE];self.reject()
    def test_verifier_mutation_reject(self):(self.root/next(iter(M.VERIFIER_TOOLS))).write_text('changed');self.assertIsNone(self.verify())
    def test_unreviewed_verifier_hash_reject(self):self.review['VerifierToolsSHA256']={};self.reject()
    def test_missing_finite_row_reject(self):self.rows.pop();self.reject()
    def test_duplicate_finite_row_reject(self):self.rows[1]=deepcopy(self.rows[0]);self.review['FiniteSnapshotRowDigests']=[M.digest_object(row) for row in self.rows];self.reject()
    def test_old_history_removed_reject(self):del self.rows[0]['ExistingC11HistoricalAdmission'];self.review['FiniteSnapshotRowDigests']=[M.digest_object(row) for row in self.rows];self.reject()
    def test_prior_history_relabel_current_pass_reject(self):self.rows[0]['ExistingC11HistoricalAdmission']['CurrentValidatorPassClaimed']=True;self.review['FiniteSnapshotRowDigests']=[M.digest_object(row) for row in self.rows];self.reject()
    def test_raw_capsule_mutation_reject(self):proof=self.verify();self.target.with_suffix('.wxf').write_bytes(b'changed');self.assertIsNone(self.evidence(proof))
    def test_raw_summary_mutation_reject(self):proof=self.verify();self.target.write_text('{}');self.assertIsNone(self.evidence(proof))
    def test_relabel_producer_reject(self):proof=self.verify();self.raw['ContextSHA256']=M.AFTER_CONTEXT;self.assertIsNone(self.evidence(proof))
    def test_private_pilot_outside_snapshot_reject(self):self.assertIsNone(M.record_evidence(self.root/'outside.json',self.raw,M.AFTER_CONTEXT,self.verify()))
    def test_declared_derived_summary_fields_allowed(self):self.raw['PrecisionQualification']={'Status':'unverified external qualifier'};self.assertIsNotNone(self.evidence())
    def test_unknown_derived_summary_field_reject(self):self.raw['InventedPass']=True;self.assertIsNone(self.evidence())
    def test_snapshot_cannot_upgrade_he_history(self):self.snapshot['ExistingC11HistoryNotUpgradedToLiteralCheckerEquivalence']=False;self.reject()


if __name__=='__main__':unittest.main(verbosity=2)
