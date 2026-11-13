"""Portable fail-closed C7/C8 provenance controls. No Wolfram or amplitude jobs."""
from pathlib import Path
import copy
import importlib.util
import json
import tempfile
import unittest
from unittest.mock import patch
from types import SimpleNamespace

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location('sm5_c8_context_test', ROOT/'tests/SM-5-point/validation_context_c8.py')
V = importlib.util.module_from_spec(SPEC); SPEC.loader.exec_module(V)
CORE_SPEC = importlib.util.spec_from_file_location('sm5_c8_core_test', ROOT/'tests/SM-5-point/constructor_context.py')
C = importlib.util.module_from_spec(CORE_SPEC); CORE_SPEC.loader.exec_module(C)


def fields(**updates):
    result = dict.fromkeys(V.FIELDS, '{}'); result.update(updates); return result


def capture(rows):
    return {'PackageLoadedOnce': True, 'ModelsRead': [], 'PhysicsCalculationsPerformed': False,
            'WolframVersion': 'test-version', 'SystemID': 'test-system', 'SymbolCount': len(rows),
            'Definitions': rows, 'CapturedNamePatterns': list(C.CAPTURE_PATTERNS)}


def fixture():
    before = {'Files': {name: 'old-'+name for name in V.CHANGED_FILES} |
              {'source/HEPCAT-Base.wl': 'base', 'models/vars1.mdl': 'model', '/runtime.a': 'runtime'},
              'Programs': {'SPINAS': '/test/library'}}
    after = copy.deepcopy(before)
    after['Files'].update({name: 'new-'+name for name in V.CHANGED_FILES})
    old = {name: fields(DownValues='{old-'+name+'}') for name in V.IMPLEMENTATIONS}
    for name in (V.CONSTRUCTOR,):
        short = name.split('`')[-1]
        old[name]['Messages'] = ('{HoldPattern['+short+'::usage] :> "Old", HoldPattern['+short+'::args] :> "Keep"}')
    old[V.EXACT_MATCH] = fields(DownValues='{unchangedLocalThreePointMatcher}')
    old[C.LOADER_CACHE] = fields(OwnValues='{HoldPattern['+C.LOADER_CACHE+'] :> 1}')
    old['ConstructiveDiagrams`CheckAmplitudes'] = fields(DownValues='{unchangedChecks}')
    new = copy.deepcopy(old)
    for name in V.IMPLEMENTATIONS: new[name]['DownValues'] = '{new-'+name+'}'
    new[C.LOADER_CACHE]['OwnValues'] = '{HoldPattern['+C.LOADER_CACHE+'] :> 2}'
    new[V.PREFIX+'localWork'] = fields()
    new[V.PREFIX+'localWork$'] = fields(Attributes='{Temporary}')
    cache = {'Before': 1, 'After': 2, '_VerifiedInputHashes': True, 'InputCount': 13}
    return before, after, [capture(old), capture(copy.deepcopy(old))], [capture(new), capture(copy.deepcopy(new))], cache


class ExactFieldTests(unittest.TestCase):
    def setUp(self): self.before, self.after, self.old, self.new, self.cache = fixture()
    def analyze(self): return V.compare_transition(C,self.before,self.after,self.old,self.new,self.cache)
    def edit(self,name,field,value):
        for raw in self.new: raw['Definitions'][name][field] = value
    def test_candidate_is_analysis_only(self):
        r=self.analyze(); self.assertEqual(r['Status'],'ANALYZED_NOT_SEALED'); self.assertTrue(r['HistoricalEvidenceOnly'])
        self.assertFalse(r['ValidationRerunPerformed']); self.assertFalse(r['CurrentConstructorSuccessClaimed'])
    def test_all_checker_definition_fields_are_exact(self):
        for field in V.FIELDS:
            with self.subTest(field=field):
                before,after,old,new,cache=fixture()
                for raw in new: raw['Definitions']['ConstructiveDiagrams`CheckAmplitudes'][field]='{changed}'
                with self.assertRaises(ValueError): V.compare_transition(C,before,after,old,new,cache)
    def test_model_change_is_not_a_source_exception(self):
        self.after['Files']['models/vars1.mdl']='new-model'
        with self.assertRaises(ValueError): self.analyze()
    def test_numerical_runtime_change_is_not_an_exception(self):
        self.after['Files']['/runtime.a']='changed'
        with self.assertRaises(ValueError): self.analyze()
    def test_unrelated_base_change_rejects(self):
        self.after['Files']['source/HEPCAT-Base.wl']='changed'
        with self.assertRaises(ValueError): self.analyze()
    def test_new_module_is_not_implicitly_allowed(self):
        self.after['Files']['source/internal/HEPCAT-Color-Orbits.wl']='new'
        with self.assertRaises(ValueError): self.analyze()
    def test_both_declared_implementation_differences_are_required(self):
        self.edit(V.CURRENT_SOURCE,'DownValues',self.old[0]['Definitions'][V.CURRENT_SOURCE]['DownValues'])
        with self.assertRaises(ValueError): self.analyze()
    def test_defaultvalues_and_options_of_old_functions_cannot_change(self):
        for name in V.IMPLEMENTATIONS:
            for field in ('Options','DefaultValues'):
                with self.subTest(name=name,field=field):
                    before,after,old,new,cache=fixture()
                    for raw in new: raw['Definitions'][name][field]='{changed}'
                    with self.assertRaises(ValueError): V.compare_transition(C,before,after,old,new,cache)
    def test_color_projection_math_is_not_exempted(self):
        name=V.PREFIX+'colorProjectionData'
        for raw in self.old: raw['Definitions'][name]=fields(DownValues='{unchangedProjection}'); raw['SymbolCount']+=1
        for raw in self.new: raw['Definitions'][name]=fields(DownValues='{changedProjection}'); raw['SymbolCount']+=1
        with self.assertRaises(ValueError): self.analyze()
    def test_an_empty_source_local_is_allowed_but_stored_data_is_not(self):
        name=V.CURRENT_PREFIX+'newLocal'
        for raw in self.new: raw['Definitions'][name]=fields(); raw['SymbolCount']+=1
        self.assertEqual(self.analyze()['Status'],'ANALYZED_NOT_SEALED')
        self.edit(name,'OwnValues','{state}')
        with self.assertRaises(ValueError): self.analyze()
    def test_new_private_defined_helper_is_not_an_exception(self):
        name=V.CURRENT_PREFIX+'newHelper'
        for raw in self.new: raw['Definitions'][name]=fields(DownValues='{new}');raw['SymbolCount']+=1
        with self.assertRaises(ValueError): self.analyze()
    def test_any_nonempty_field_on_new_private_symbol_rejects(self):
        for field in V.FIELDS:
            with self.subTest(field=field):
                before,after,old,new,cache=fixture()
                name=V.CURRENT_PREFIX+'newState'
                for raw in new: raw['Definitions'][name]=fields(**{field:'{unreviewed}'});raw['SymbolCount']+=1
                with self.assertRaises(ValueError): V.compare_transition(C,before,after,old,new,cache)
    def test_preexisting_colour_constructor_is_not_exempted(self):
        name=V.COLOUR_CONSTRUCTOR
        for raw in self.old:raw['Definitions'][name]=fields(DownValues='{oldColour}');raw['SymbolCount']+=1
        for raw in self.new:raw['Definitions'][name]=fields(DownValues='{newColour}');raw['SymbolCount']+=1
        with self.assertRaises(ValueError): self.analyze()
    def test_preexisting_spin_weight_metadata_helper_is_not_exempted(self):
        name=V.EXTERNAL_WEIGHTS
        for raw in self.old:raw['Definitions'][name]=fields(DownValues='{oldWeights}');raw['SymbolCount']+=1
        for raw in self.new:raw['Definitions'][name]=fields(DownValues='{newWeights}');raw['SymbolCount']+=1
        with self.assertRaises(ValueError): self.analyze()
    def test_existing_symbol_cannot_disappear(self):
        for raw in self.new:
            del raw['Definitions']['ConstructiveDiagrams`CheckAmplitudes']; raw['SymbolCount']-=1
        with self.assertRaises(ValueError): self.analyze()
    def test_both_cold_captures_must_match(self):
        self.new[1]['Definitions'][V.CONSTRUCTOR]['DownValues']='{different}'
        with self.assertRaises(ValueError): self.analyze()
    def test_capture_without_full_namespace_inventory_rejects(self):
        for raw in self.new: raw['CapturedNamePatterns']=[]
        with self.assertRaises(ValueError): self.analyze()
    def test_platform_or_kernel_version_change_rejects(self):
        for raw in self.new: raw['SystemID']='different-platform'
        with self.assertRaises(ValueError): self.analyze()
    def test_defined_public_symbol_rejects(self):
        for raw in self.new:
            raw['Definitions']['ConstructiveDiagrams`NewPublic']=fields(DownValues='{new}'); raw['SymbolCount']+=1
        with self.assertRaises(ValueError): self.analyze()
    def test_unrelated_defined_private_symbol_rejects(self):
        for raw in self.new:
            raw['Definitions'][V.PREFIX+'otherPhysics']=fields(DownValues='{new}'); raw['SymbolCount']+=1
        with self.assertRaises(ValueError): self.analyze()
    def test_existing_spectator_helper_may_not_gain_state(self):
        name=V.PREFIX+'colorSpectatorPath'
        for raw in self.old:raw['Definitions'][name]=fields(DownValues='{sameSpectator}');raw['SymbolCount']+=1
        for raw in self.new:raw['Definitions'][name]=fields(DownValues='{sameSpectator}');raw['SymbolCount']+=1
        self.edit(name,'OwnValues','{state}')
        with self.assertRaises(ValueError): self.analyze()
    def test_temporary_name_with_data_rejects(self):
        self.edit(V.PREFIX+'localWork$','OwnValues','{state}')
        with self.assertRaises(ValueError): self.analyze()
    def test_loader_cache_needs_verified_exact_recomputation(self):
        self.cache['_VerifiedInputHashes']=False
        with self.assertRaises(ValueError): self.analyze()
    def test_wrong_loader_integer_rejects(self):
        self.cache['After']=3
        with self.assertRaises(ValueError): self.analyze()
    def test_constructor_usage_cannot_change_in_this_transition(self):
        for name in (V.CONSTRUCTOR,):
            self.edit(name,'Messages',self.old[0]['Definitions'][name]['Messages'].replace('"Old"','"New"'))
        with self.assertRaises(ValueError): self.analyze()
    def test_private_core_messages_cannot_change(self):
        self.edit(V.EXACT_MATCH,'Messages','{HoldPattern[completionFactorizationCore::usage] :> \"Unreviewed\"}')
        with self.assertRaises(ValueError): self.analyze()
    def test_error_message_change_is_not_usage_only(self):
        self.edit(V.CONSTRUCTOR,'Messages',self.old[0]['Definitions'][V.CONSTRUCTOR]['Messages'].replace('"Keep"','"Changed"'))
        with self.assertRaises(ValueError): self.analyze()


    def test_local_vertex_matchers_are_not_part_of_the_class_extension(self):
        self.edit(V.EXACT_MATCH,'DownValues','{changedMatcher}')
        with self.assertRaises(ValueError): self.analyze()
    def test_metric_contact_calibrator_stays_exact(self):
        name='HEPCATCovariantContactCalibration`Private`calibratedCovariantSource'
        for raw in self.old: raw['Definitions'][name]=fields(DownValues='{sameContact}');raw['SymbolCount']+=1
        for raw in self.new: raw['Definitions'][name]=fields(DownValues='{changedContact}');raw['SymbolCount']+=1
        with self.assertRaises(ValueError): self.analyze()


class LoaderFormulaTests(unittest.TestCase):
    def test_exact_formula_is_reused_from_frozen_c4_owner_not_c5(self):
        with tempfile.TemporaryDirectory() as temporary:
            root=Path(temporary).resolve(); archive=root/'archive'; output=root/'evidence'
            output.mkdir(); old_files={}; new_files={}
            names=('required.wl','optional.wl')
            for name in names:
                before=archive/'source'/name; after=root/'source'/name
                before.parent.mkdir(parents=True,exist_ok=True); after.parent.mkdir(parents=True,exist_ok=True)
                before.write_text('old '+name); after.write_text('new '+name)
                old_files['source/'+name]=V.digest(before); new_files['source/'+name]=V.digest(after)
            formula=lambda paths,destination: json.dumps({'Paths':paths,'Destination':str(destination)},sort_keys=True)
            baseline={'Root':root,'Directory':root,'Archive':archive,'Manifest':{'Files':old_files},
                'Core':SimpleNamespace(SIGNATURE_REQUIRED=(names[0],),SIGNATURE_OPTIONAL=(names[1],)),
                'PriorVerifier':SimpleNamespace(),
                'Record':{'SourceFiles':{key:{'Path':str((archive/key).relative_to(root)),'SHA256':value}for key,value in old_files.items()}},
                'PriorProof':{'_Baseline':{'PriorVerifier':SimpleNamespace(), 'PriorProof':{'_Baseline':{'PriorVerifier':SimpleNamespace(loader_script=formula)}}}}}
            manifest=root/'manifest.json'; manifest.write_text(json.dumps({'Files':new_files}))
            with patch.object(V,'load_baseline',return_value=baseline):
                script=V.prepare_loader_evidence(root,manifest,output)
            recorded=json.loads((output/'loader-signature-inputs.json').read_text())
            paths={key:recorded[key] for key in ('BeforeRequired','BeforeOptional','AfterRequired','AfterOptional')}
            self.assertEqual(script.read_text(),formula(paths,output/'loader-signature-values.json'))


class ExactReviewTests(unittest.TestCase):
    def setUp(self):
        before,after,old,new,cache=fixture()
        self.analysis=V.compare_transition(C,before,after,old,new,cache)
        self.analysis['SourceDiffsSHA256']='literal-source-diff'
        self.review={'Status':'REVIEWED_EXACT_MASSIVE_FOUR_FERMION_VECTOR_SOURCE_CLASS_EXTENSION',
          'AnalysisSHA256':V.canonical_digest(self.analysis),'SourceDiffsSHA256':'literal-source-diff',
          'ExpectedChangedDefinitionFields':self.analysis['ChangedDefinitionFields'],
          'ExpectedNewDefinedPrivateSymbols':self.analysis['NewDefinedPrivateSymbols'],'ReviewedBy':'independent reviewer',
          **dict.fromkeys(V.REVIEW_FLAGS,True)}
        self.evidence={'controls':{'Path':'tests/actual-controls.json','SHA256':'exact-file'}}
    def test_review_seals_only_the_exact_analysis(self):
        r=V.reviewed_payload(self.analysis,self.review,self.evidence)
        self.assertEqual(r['Status'],'SEALED'); self.assertFalse(r['ConstructorRerunPerformed'])
        self.assertTrue(r['SupplementalCounterexampleChecksStillRequired']); self.assertTrue(r['PrecisionQualificationUnchangedRequired'])
    def test_unreviewed_or_missing_review_flag_rejects(self):
        for flag in V.REVIEW_FLAGS:
            with self.subTest(flag=flag):
                r=copy.deepcopy(self.review); r[flag]=False
                with self.assertRaises(ValueError): V.reviewed_payload(self.analysis,r,self.evidence)
    def test_new_helpers_need_an_exact_reviewed_list(self):
        self.review['ExpectedNewDefinedPrivateSymbols']=['ConstructiveDiagrams`Private`unexpectedNewHelper']
        with self.assertRaises(ValueError): V.reviewed_payload(self.analysis,self.review,self.evidence)
    def test_changed_functions_need_an_exact_reviewed_field_map(self):
        self.review['ExpectedChangedDefinitionFields']={}
        with self.assertRaises(ValueError): V.reviewed_payload(self.analysis,self.review,self.evidence)
    def test_review_cannot_rebind_a_different_source_diff(self):
        self.review['SourceDiffsSHA256']='different'
        with self.assertRaises(ValueError): V.reviewed_payload(self.analysis,self.review,self.evidence)
    def test_empty_evidence_rejects(self):
        with self.assertRaises(ValueError): V.reviewed_payload(self.analysis,self.review,{})
    def test_actual_evidence_bytes_are_checked(self):
        with tempfile.TemporaryDirectory() as temporary:
            root=Path(temporary); p=root/'controls.json';p.write_text('{"Checks":7}')
            r={'EvidenceFiles':{'control':{'Path':'controls.json','SHA256':V.digest(p)}}}
            self.assertEqual(V.review_evidence(root,r)['control']['SHA256'],V.digest(p))
            p.write_text('{"Checks":8}')
            with self.assertRaises(ValueError): V.review_evidence(root,r)
    def test_evidence_cannot_escape_the_checkout(self):
        with tempfile.TemporaryDirectory() as temporary:
            with self.assertRaises(ValueError): V.review_evidence(Path(temporary),{'EvidenceFiles':{'control':{'Path':'../outside','SHA256':'x'}}})


class FiniteRecordTests(unittest.TestCase):
    def setUp(self):
        self.temporary=tempfile.TemporaryDirectory();self.addCleanup(self.temporary.cleanup)
        self.root=Path(self.temporary.name);self.path=self.root/'sm5_example.json';self.capsule=self.path.with_suffix('.wxf');self.capsule.write_bytes(b'actual capsule')
        self.summary={'ID':'sm5_example','Status':'COMPLETE','ContextSHA256':V.BASE_CONTEXT,'CapsuleSHA256':V.digest(self.capsule),'Seconds':37,'RandomSeed':23}
        self.path.write_text(json.dumps(self.summary));self.prior_calls=[]
        class Prior:
            def record_evidence(inner,path,summary,current,proof):
                self.prior_calls.append(summary['ContextSHA256'])
                return {'CurrentContextSHA256':V.BASE_CONTEXT,'CompatibilityChain':[{'FromContextSHA256':summary['ContextSHA256'],'ToContextSHA256':V.BASE_CONTEXT}]}
        self.row={'SummarySHA256':V.digest(self.path),'Status':'COMPLETE','ContextSHA256':V.BASE_CONTEXT,'Identity':'sm5_example',
          'Capsule':str(self.capsule),'ActualCapsuleSHA256':V.digest(self.capsule),'DeclaredCapsuleSHA256':V.digest(self.capsule)}
        self.proof={'_C8VerifiedCurrentContext':True,'Status':'SEALED','Kind':V.KIND,'BeforeContextSHA256':V.BASE_CONTEXT,
          'AfterContextSHA256':'actual-current','HistoricalEvidenceOnly':True,'ConstructorRerunPerformed':False,
          'ValidationRerunPerformed':False,'CurrentConstructorSuccessClaimed':False,'AnalysisSHA256':'a','CodeReviewSHA256':'r','ReviewEvidenceSHA256':'e',
          '_Baseline':{'Snapshot':{self.path.name:self.row},'PriorVerifier':Prior(),'PriorProof':{'actual':'sealed prior'}}}
    def check(self): return V.record_evidence(self.path,self.summary,'actual-current',self.proof)
    def resave(self): self.path.write_text(json.dumps(self.summary));self.row['SummarySHA256']=V.digest(self.path)
    def test_exact_c7_record_retains_original_context_without_new_run_claim(self):
        before=self.path.read_bytes();r=self.check();self.assertIsNotNone(r);self.assertEqual(self.path.read_bytes(),before)
        self.assertEqual(r['RecordedContextSHA256'],V.BASE_CONTEXT);self.assertFalse(r['ConstructorRerunPerformed']);self.assertFalse(r['ValidationRerunPerformed']);self.assertFalse(r['CurrentConstructorSuccessClaimed'])
    def test_already_historical_records_require_the_exact_prior_chain(self):
        self.summary['ContextSHA256']='actual-old-c4';self.row['ContextSHA256']='actual-old-c4';self.resave()
        r=self.check();self.assertEqual(len(r['CompatibilityChain']),2);self.assertEqual(self.prior_calls,['actual-old-c4'])
    def test_missing_prior_chain_rejects(self):
        self.summary['ContextSHA256']='unreviewed-old';self.row['ContextSHA256']='unreviewed-old';self.resave()
        self.proof['_Baseline']['PriorVerifier'].record_evidence=lambda *args:None
        self.assertIsNone(self.check())
    def test_unlisted_future_record_is_not_admitted(self):
        self.proof['_Baseline']['Snapshot']={};self.assertIsNone(self.check())
    def test_edited_capsule_rejects(self):
        self.capsule.write_bytes(b'changed');self.assertIsNone(self.check())
    def test_edited_summary_or_timing_rejects(self):
        self.path.write_text(self.path.read_text()+' ');self.assertIsNone(self.check())
    def test_identity_context_or_status_change_rejects(self):
        for key,value in (('ID','different'),('ContextSHA256','different'),('Status','PASS')):
            with self.subTest(key=key):
                original=self.summary[key];self.summary[key]=value;self.assertIsNone(self.check());self.summary[key]=original
    def test_proof_cannot_claim_constructor_or_validator_rerun(self):
        for key in ('ConstructorRerunPerformed','ValidationRerunPerformed','CurrentConstructorSuccessClaimed'):
            with self.subTest(key=key):
                self.proof[key]=True;self.assertIsNone(self.check());self.proof[key]=False
    def test_unsealed_proof_rejects(self):
        self.proof['Status']='ANALYZED_NOT_SEALED';self.assertIsNone(self.check())
    def test_wrong_current_context_rejects(self):
        self.proof['AfterContextSHA256']='other';self.assertIsNone(self.check())
    def test_supplemental_and_precision_requirements_are_retained(self):
        r=self.check();self.assertTrue(r['SupplementalCounterexampleChecksStillRequired']);self.assertTrue(r['PrecisionQualificationUnchangedRequired'])


if __name__=='__main__': unittest.main()
