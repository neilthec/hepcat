"""Portable fail-closed C8/C9 provenance controls. No Wolfram or amplitude jobs."""
from pathlib import Path
import copy
import importlib.util
import json
import tempfile
import unittest
from unittest.mock import patch
from types import SimpleNamespace

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location('sm5_c9_context_test', ROOT/'tests/SM-5-point/validation_context_c9.py')
V = importlib.util.module_from_spec(SPEC); SPEC.loader.exec_module(V)
CORE_SPEC = importlib.util.spec_from_file_location('sm5_c9_core_test', ROOT/'tests/SM-5-point/constructor_context.py')
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
    after['Files'][V.ADDED_FILE] = 'new-reviewed-module'
    old = {name: fields(DownValues='{old-'+name+'}') for name in V.IMPLEMENTATIONS}
    anchor = '"internal/HEPCAT-SPINAS.wl", "internal/HEPCAT-SPINAS-Optimize.wl"'
    old[V.SIGNATURE_FUNCTION]['DownValues'] = '{Signature[' + anchor + ', Hash[Join[required, optional]] ]}'
    old[V.CONSTRUCTOR] = fields(DownValues='{unchangedConstructor}',
        Messages='{HoldPattern[ConstructAmplitude::usage] :> "Old", HoldPattern[ConstructAmplitude::args] :> "Keep"}')
    old[V.CURRENT_SOURCE] = fields(DownValues='{unchangedCurrentSource}')
    old[V.EXACT_MATCH] = fields(DownValues='{unchangedLocalThreePointMatcher}')
    old[C.LOADER_CACHE] = fields(OwnValues='{HoldPattern['+C.LOADER_CACHE+'] :> 1}')
    old['ConstructiveDiagrams`CheckAmplitudes'] = fields(DownValues='{unchangedChecks}')
    new = copy.deepcopy(old)
    new[V.COLOUR_CONSTRUCTOR]['DownValues'] = '{newColourConstructor}'
    added = '"internal/HEPCAT-SPINAS.wl", ' + ', '.join(json.dumps(x) for x in V.ADDED_REQUIRED) + ', "internal/HEPCAT-SPINAS-Optimize.wl"'
    new[V.SIGNATURE_FUNCTION]['DownValues'] = old[V.SIGNATURE_FUNCTION]['DownValues'].replace(anchor,added)
    new[C.LOADER_CACHE]['OwnValues'] = '{HoldPattern['+C.LOADER_CACHE+'] :> 2}'
    for name in V.NEW_FUNCTIONS: new[name] = fields(DownValues='{reviewedPrivateHelper}')
    new[V.PREFIX+'localWork'] = fields()
    new[V.PREFIX+'localWork$'] = fields(Attributes='{Temporary}')
    cache = {'Before': 1, 'After': 2, '_VerifiedInputHashes': True,
             'InputCountBefore':13,'InputCountAfter':17,'RequiredCountBefore':9,'RequiredCountAfter':13,'OptionalCount':4}
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
        self.edit(V.COLOUR_CONSTRUCTOR,'DownValues',self.old[0]['Definitions'][V.COLOUR_CONSTRUCTOR]['DownValues'])
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
    def test_preexisting_constructor_is_not_exempted(self):
        self.edit(V.CONSTRUCTOR,'DownValues','{changedConstructor}')
        with self.assertRaises(ValueError): self.analyze()
    def test_preexisting_current_source_is_not_exempted(self):
        self.edit(V.CURRENT_SOURCE,'DownValues','{changedCurrentSource}')
        with self.assertRaises(ValueError): self.analyze()
    def test_signature_formula_or_optional_policy_cannot_change(self):
        self.edit(V.SIGNATURE_FUNCTION,'DownValues',self.new[0]['Definitions'][V.SIGNATURE_FUNCTION]['DownValues'].replace('Hash[','ChangedHash['))
        with self.assertRaises(ValueError): self.analyze()
    def test_signature_cannot_omit_or_reorder_new_required_paths(self):
        for old,new in ((V.ADDED_REQUIRED[0], 'unreviewed.wl'), (V.ADDED_REQUIRED[2], V.ADDED_REQUIRED[0])):
            with self.subTest(path=old):
                before,after,a,b,cache=fixture()
                for raw in b: raw['Definitions'][V.SIGNATURE_FUNCTION]['DownValues']=raw['Definitions'][V.SIGNATURE_FUNCTION]['DownValues'].replace(old,new)
                with self.assertRaises(ValueError): V.compare_transition(C,before,after,a,b,cache)
    def test_added_module_is_required(self):
        del self.after['Files'][V.ADDED_FILE]
        with self.assertRaises(ValueError): self.analyze()
    def test_required_private_helper_cannot_be_missing(self):
        for raw in self.new:
            del raw['Definitions'][V.NEW_FUNCTIONS[-1]];raw['SymbolCount']-=1
        with self.assertRaises(ValueError): self.analyze()
    def test_new_helpers_cannot_have_other_definition_fields(self):
        for field in V.FIELDS:
            if field=='DownValues':continue
            with self.subTest(field=field):
                before,after,a,b,cache=fixture()
                for raw in b:raw['Definitions'][V.NEW_FUNCTIONS[0]][field]='{state}'
                with self.assertRaises(ValueError): V.compare_transition(C,before,after,a,b,cache)
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
    def setUp(self):
        self.temporary=tempfile.TemporaryDirectory();self.addCleanup(self.temporary.cleanup)
        self.root=Path(self.temporary.name).resolve();self.output=self.root/'evidence';self.output.mkdir()
        self.archive=self.root/'before';old_files={};new_files={};sources={}
        for name in V.AFTER_REQUIRED+V.OPTIONAL:
            key='source/'+name
            after=self.root/key;after.parent.mkdir(parents=True,exist_ok=True);after.write_text('after '+name)
            new_files[key]=V.digest(after)
            if name not in V.BEFORE_REQUIRED+V.OPTIONAL:continue
            before=self.archive/key;before.parent.mkdir(parents=True,exist_ok=True);before.write_text('before '+name)
            old_files[key]=V.digest(before);sources[key]={'Path':str(before.relative_to(self.root))}
        self.baseline={'Root':self.root,'Manifest':{'Files':old_files},'Record':{'SourceFiles':sources},
            'Core':SimpleNamespace(SIGNATURE_REQUIRED=V.BEFORE_REQUIRED,SIGNATURE_OPTIONAL=V.OPTIONAL)}
        self.after={'Files':new_files};self.manifest=self.root/'manifest.json';self.manifest.write_text(json.dumps(self.after))
        with patch.object(V,'load_baseline',return_value=self.baseline):
            self.script=V.prepare_loader_evidence(self.root,self.manifest,self.output)
        self.values=self.output/'loader-signature-values.json'
        self.values.write_text(json.dumps({'Before':123,'After':456,'PhysicsCalculationsPerformed':False,
            'Method':'ExactLoaderHashWithExplicitBeforeAndAfterRequiredLists'}))
    def evidence(self): return V.loader_evidence(self.baseline,self.after,self.output)
    def test_explicit_before9_after13_plus4_optional(self):
        r=self.evidence();self.assertEqual((r['RequiredCountBefore'],r['RequiredCountAfter'],r['OptionalCount']),(9,13,4))
        self.assertEqual((r['InputCountBefore'],r['InputCountAfter']),(13,17));self.assertTrue(r['_VerifiedInputHashes'])
        m=V.object_at(self.output/'loader-signature-inputs.json')
        self.assertEqual([r['Name']for r in m['BeforeInputs'] if r['Kind']=='Required'],list(V.BEFORE_REQUIRED))
        self.assertEqual([r['Name']for r in m['AfterInputs'] if r['Kind']=='Required'],list(V.AFTER_REQUIRED))
    def test_script_is_exact_and_optional_absence_has_named_missing(self):
        m=V.object_at(self.output/'loader-signature-inputs.json')
        self.assertEqual(self.script.read_text(),V.loader_script(m,self.values))
        self.assertIn('Missing["OptionalProofSourceAbsent", #2]',self.script.read_text())
    def test_missing_optional_keeps_exact_named_missing_input(self):
        key='source/'+V.OPTIONAL[-1]
        (self.root/key).unlink();(self.archive/key).unlink()
        del self.after['Files'][key];del self.baseline['Manifest']['Files'][key]
        self.manifest.write_text(json.dumps(self.after))
        with patch.object(V,'load_baseline',return_value=self.baseline):V.prepare_loader_evidence(self.root,self.manifest,self.output)
        r=self.evidence();self.assertEqual(r['InputCountAfter'],17)
        m=V.object_at(self.output/'loader-signature-inputs.json')
        self.assertIsNone(m['BeforeInputs'][-1]['SHA256']);self.assertIsNone(m['AfterInputs'][-1]['SHA256'])
    def test_missing_required_rejects(self):
        key='source/'+V.ADDED_REQUIRED[-1];del self.after['Files'][key]
        with self.assertRaises(ValueError): self.evidence()
    def test_changed_actual_input_rejects(self):
        (self.root/'source'/V.ADDED_REQUIRED[0]).write_text('changed')
        with self.assertRaises(ValueError):self.evidence()
    def test_forged_count_input_hash_or_order_rejects(self):
        path=self.output/'loader-signature-inputs.json';m=V.object_at(path)
        for alter in (lambda x:x.update(RequiredCountAfter=9),lambda x:x['AfterInputs'][0].update(SHA256='forged'),lambda x:x['AfterRequired'].reverse()):
            with self.subTest(alter=alter):
                candidate=copy.deepcopy(m);alter(candidate);path.write_text(json.dumps(candidate))
                with self.assertRaises(ValueError):self.evidence()
        path.write_text(json.dumps(m));self.evidence()
    def test_wrong_script_rejects(self):
        self.script.write_text(self.script.read_text().replace('Hash[Join[','Hash[Reverse[Join['))
        with self.assertRaises(ValueError):self.evidence()
    def test_boolean_loader_integer_rejects(self):
        value=V.object_at(self.values);value['After']=True;self.values.write_text(json.dumps(value))
        with self.assertRaises(ValueError):self.evidence()
    def test_old_core_list_cannot_silently_gain_modules(self):
        self.baseline['Core'].SIGNATURE_REQUIRED=V.AFTER_REQUIRED
        with self.assertRaises(ValueError):self.evidence()
    def test_loader_source_append_is_exact(self):
        old='Get[{"internal/HEPCAT-CovariantContacts.wl"}]; existing[]'
        new=old.replace('"internal/HEPCAT-CovariantContacts.wl"','"internal/HEPCAT-CovariantContacts.wl", "internal/HEPCAT-ColoredCurrents.wl"')
        self.assertTrue(V.loader_delta_exact(old,new))
        self.assertFalse(V.loader_delta_exact(old,new+' hiddenSideEffect[]'))
    def test_wolfram_literal_does_not_rewrite_brackets_in_path(self):
        self.assertEqual(V.wl_literal(['a[b]','quote"x']),'{"a[b]", "quote\\"x"}')


class ExactReviewTests(unittest.TestCase):
    def setUp(self):
        before,after,old,new,cache=fixture()
        self.analysis=V.compare_transition(C,before,after,old,new,cache)
        self.analysis['SourceDiffsSHA256']='literal-source-diff'
        self.review={'Status':'REVIEWED_EXACT_MASSIVE_COLOURED_FOUR_FERMION_SCALAR_SOURCE_EXTENSION',
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
        self.proof={'_C9VerifiedCurrentContext':True,'Status':'SEALED','Kind':V.KIND,'BeforeContextSHA256':V.BASE_CONTEXT,
          'AfterContextSHA256':'actual-current','HistoricalEvidenceOnly':True,'ConstructorRerunPerformed':False,
          'ValidationRerunPerformed':False,'CurrentConstructorSuccessClaimed':False,'AnalysisSHA256':'a','CodeReviewSHA256':'r','ReviewEvidenceSHA256':'e',
          '_Baseline':{'Snapshot':{self.path.name:self.row},'PriorVerifier':Prior(),'PriorProof':{'actual':'sealed prior'}}}
    def check(self): return V.record_evidence(self.path,self.summary,'actual-current',self.proof)
    def resave(self): self.path.write_text(json.dumps(self.summary));self.row['SummarySHA256']=V.digest(self.path)
    def test_exact_c8_record_retains_original_context_without_new_run_claim(self):
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
