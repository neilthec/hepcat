"""Finite fail-closed C3/C4 provenance controls. No Wolfram or amplitude jobs."""
from pathlib import Path
import copy
import importlib.util
import json
import tempfile
import unittest
from unittest import mock

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location('sm5_c4_context_test', ROOT/'tests/SM-5-point/validation_context_c4.py')
V = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(V)
C = V.legacy()


def fields(**updates):
    result = dict.fromkeys(C.FIELDS, '{}')
    result.update(updates)
    return result


def capture(rows):
    return {'PackageLoadedOnce': True, 'ModelsRead': [], 'PhysicsCalculationsPerformed': False,
            'WolframVersion': 'test-version', 'SystemID': 'test-system', 'SymbolCount': len(rows),
            'Definitions': rows, 'CapturedNamePatterns': list(C.CAPTURE_PATTERNS)}


def fixture():
    before = {'Files': {name: 'old-'+name for name in V.CHANGED_FILES} |
              {'source/HEPCAT-Check.wl': 'checker', 'models/vars1.mdl': 'model', '/lib/runtime.a': 'runtime'},
              'Programs': {'SPINAS': '/test/library'}}
    after = copy.deepcopy(before)
    after['Files'].update({name: 'new-'+name for name in V.CHANGED_FILES})
    vector = V.COLOR_FUNCTIONS[0]
    basis = V.COLOR_FUNCTIONS[1]
    old = {V.CONSTRUCTOR: fields(DownValues='{oldCtor}', Messages='{HoldPattern[ConstructAmplitude::usage] :> "Old", HoldPattern[ConstructAmplitude::args] :> "Keep"}'),
           V.CONTACT: fields(DownValues='{oldContactGuard}'),
           vector: fields(DownValues='{HoldPattern['+vector+'[x_]] :> '+vector+'[x]}'),
           basis: fields(DownValues='{HoldPattern['+basis+'[t_List,r_List,maxStates_Integer:65536]] :> oldBody}'),
           V.COLOR_FUNCTIONS[2]: fields(DownValues='{oldProjection}'),
           C.LOADER_CACHE: fields(OwnValues='{HoldPattern['+C.LOADER_CACHE+'] :> 1}'),
           'ConstructiveDiagrams`CheckAmplitudes': fields(DownValues='{unchangedChecks}')}
    new = copy.deepcopy(old)
    for name in (V.CONSTRUCTOR,V.CONTACT,*V.COLOR_FUNCTIONS):new[name]['DownValues']='{new-'+name+'}'
    new[basis]['DownValues']='{HoldPattern['+basis+'[t_List,r_List,maxStates_Integer:65536,materializeOriginalGram_:True]] :> newBody}'
    new[C.LOADER_CACHE]['OwnValues']='{HoldPattern['+C.LOADER_CACHE+'] :> 2}'
    for name in V.NEW_FUNCTIONS:new[name]=fields(DownValues='{newPrivateHelper}')
    new[V.NEW_FUNCTIONS[0]]['DownValues']=old[vector]['DownValues'].replace(vector,V.NEW_FUNCTIONS[0])
    cache={'Before':1,'After':2,'_VerifiedInputHashes':True,'InputCount':13}
    return before,after,[capture(old),capture(copy.deepcopy(old))],[capture(new),capture(copy.deepcopy(new))],cache


class ExactC4FieldTests(unittest.TestCase):
    def setUp(self):self.before,self.after,self.old,self.new,self.cache=fixture()
    def analyze(self):return V.compare_transition(self.before,self.after,self.old,self.new,self.cache)
    def edit(self,name,field,value):
        for raw in self.new:raw['Definitions'][name][field]=value
    def test_the_narrow_change_is_analysis_only(self):
        r=self.analyze();self.assertEqual(r['Status'],'ANALYZED_NOT_SEALED');self.assertTrue(r['RetainedEnumeratorExactRename']);self.assertFalse(r['ValidationRerunPerformed'])
    def test_all_other_checker_fields_stay_exact(self):
        for field in C.FIELDS:
            with self.subTest(field=field):
                before,after,old,new,cache=fixture()
                for raw in new:raw['Definitions']['ConstructiveDiagrams`CheckAmplitudes'][field]='{changed}'
                with self.assertRaises(ValueError):V.compare_transition(before,after,old,new,cache)
    def test_no_defaultvalues_exception_is_granted(self):
        self.edit(V.COLOR_FUNCTIONS[1],'DefaultValues','{fourthDefaultTrue}')
        with self.assertRaises(ValueError):self.analyze()
    def test_colour_options_cannot_change(self):
        self.edit(V.COLOR_FUNCTIONS[0],'Options','{newPublicOption}')
        with self.assertRaises(ValueError):self.analyze()
    def test_contact_changes_are_downvalues_only(self):
        self.edit(V.CONTACT,'OwnValues','{unexpectedMutation}')
        with self.assertRaises(ValueError):self.analyze()
    def test_usage_only_constructor_message_is_permitted(self):
        self.edit(V.CONSTRUCTOR,'Messages','{HoldPattern[ConstructAmplitude::usage] :> "New", HoldPattern[ConstructAmplitude::args] :> "Keep"}')
        self.assertIn('Messages',self.analyze()['ChangedDefinitionFields'][V.CONSTRUCTOR])
    def test_argument_messages_are_not_exempt(self):
        self.edit(V.CONSTRUCTOR,'Messages','{HoldPattern[ConstructAmplitude::usage] :> "New", HoldPattern[ConstructAmplitude::args] :> "Changed"}')
        with self.assertRaises(ValueError):self.analyze()
    def test_legacy_enumerator_requires_literal_rename(self):
        self.edit(V.NEW_FUNCTIONS[0],'DownValues','{differentEnumerator}')
        with self.assertRaises(ValueError):self.analyze()
    def test_old_third_default_cannot_change(self):
        value=self.new[0]['Definitions'][V.COLOR_FUNCTIONS[1]]['DownValues'].replace('65536','32768')
        self.edit(V.COLOR_FUNCTIONS[1],'DownValues',value)
        with self.assertRaises(ValueError):self.analyze()
    def test_fourth_default_cannot_silently_omit_original_gram(self):
        value=self.new[0]['Definitions'][V.COLOR_FUNCTIONS[1]]['DownValues'].replace('_:True','_:False')
        self.edit(V.COLOR_FUNCTIONS[1],'DownValues',value)
        with self.assertRaises(ValueError):self.analyze()
    def test_new_private_definitions_are_exactly_named(self):
        for raw in self.new:raw['Definitions'][V.COLOR_PREFIX+'unreviewed']=fields(DownValues='{new}');raw['SymbolCount']+=1
        with self.assertRaises(ValueError):self.analyze()
    def test_a_new_public_empty_symbol_is_rejected(self):
        for raw in self.new:raw['Definitions']['ConstructiveDiagrams`NewPublic']=fields();raw['SymbolCount']+=1
        with self.assertRaises(ValueError):self.analyze()
    def test_empty_and_temporary_module_locals_are_allowed(self):
        for raw in self.new:
            raw['Definitions'][V.COLOR_PREFIX+'local']=fields();raw['Definitions'][V.COLOR_PREFIX+'local$']=fields(Attributes='{Temporary}');raw['SymbolCount']+=2
        self.assertEqual(len(self.analyze()['NewEmptyPrivateSymbols']),1)
    def test_temporary_ownvalues_are_rejected(self):
        for raw in self.new:raw['Definitions'][V.COLOR_PREFIX+'local$']=fields(Attributes='{Temporary}',OwnValues='{1}');raw['SymbolCount']+=1
        with self.assertRaises(ValueError):self.analyze()
    def test_cold_capture_mismatch_is_rejected(self):
        self.new[1]['Definitions'][V.CONSTRUCTOR]['DownValues']='{different}'
        with self.assertRaises(ValueError):self.analyze()
    def test_model_runtime_and_installation_paths_are_exact(self):
        for key in ('models/vars1.mdl','/lib/runtime.a'):
            before,after,old,new,cache=fixture();after['Files'][key]='changed'
            with self.assertRaises(ValueError):V.compare_transition(before,after,old,new,cache)
        self.after['Programs']['SPINAS']='/other'
        with self.assertRaises(ValueError):self.analyze()
    def test_manifest_membership_is_exact(self):
        self.after['Files']['source/unreviewed.wl']='new'
        with self.assertRaises(ValueError):self.analyze()
    def test_loader_cache_requires_actual_verified_hashes(self):
        self.cache.pop('_VerifiedInputHashes')
        with self.assertRaises(ValueError):self.analyze()
    def test_review_cannot_be_inferred_from_control_counts(self):
        a=self.analyze();a['SourceDiffsSHA256']='diff';review={'Status':'REVIEWED_CANDIDATE_AND_FINITE_COLOUR_VALIDATION_TRANSITION','AnalysisSHA256':V.canonical_digest(a),'SourceDiffsSHA256':'diff','ReviewedBy':'fixture reviewer'}
        colour={'ControlChecks':65,'UniqueTensors':45,'OriginalOccurrences':3960,'Rank':6}
        with self.assertRaises(ValueError):V.reviewed_payload(a,review,colour)
        review.update(dict.fromkeys(V.REVIEW_FLAGS,True));proof=V.reviewed_payload(a,review,colour)
        self.assertFalse(proof['CurrentConstructorSuccessClaimed']);self.assertFalse(proof['ConstructorRerunPerformed'])
    def test_every_explicit_colour_proof_review_flag_is_required(self):
        a=self.analyze();a['SourceDiffsSHA256']='diff';review={'Status':'REVIEWED_CANDIDATE_AND_FINITE_COLOUR_VALIDATION_TRANSITION','AnalysisSHA256':V.canonical_digest(a),'SourceDiffsSHA256':'diff','ReviewedBy':'fixture',**dict.fromkeys(V.REVIEW_FLAGS,True)}
        for key in V.REVIEW_FLAGS:
            altered={**review,key:False}
            with self.subTest(key=key),self.assertRaises(ValueError):V.reviewed_payload(a,altered,{'ControlChecks':65,'UniqueTensors':45,'OriginalOccurrences':3960,'Rank':6})


class FiniteRecordTests(unittest.TestCase):
    def setUp(self):
        self.temp=tempfile.TemporaryDirectory();self.addCleanup(self.temp.cleanup);self.path=Path(self.temp.name)/'sm5_123456789abc.json';self.capsule=self.path.with_suffix('.wxf');self.capsule.write_bytes(b'fixture-not-an-amplitude')
        self.summary={'ID':'sm5_123456789abc','Status':'COMPLETE','ContextSHA256':V.BASE_CONTEXT,'CapsuleSHA256':V.digest(self.capsule)};self.path.write_text(json.dumps(self.summary))
        row={'Identity':self.summary['ID'],'Status':'COMPLETE','ContextSHA256':V.BASE_CONTEXT,'SummarySHA256':V.digest(self.path),'Capsule':'relative.wxf','ActualCapsuleSHA256':V.digest(self.capsule),'DeclaredCapsuleSHA256':V.digest(self.capsule)}
        self.proof={'Kind':V.KIND,'Status':'SEALED','BeforeContextSHA256':V.BASE_CONTEXT,'AfterContextSHA256':'new-context','_C4VerifiedCurrentContext':True,'HistoricalEvidenceOnly':True,'CurrentConstructorSuccessClaimed':False,'AnalysisSHA256':'analysis','ColourSemanticEvidence':{'fixture':True},'_Baseline':{'Snapshot':{self.path.name:row}}}
    def evidence(self):return V.record_evidence(self.path,self.summary,'new-context',self.proof)
    def test_only_the_exact_frozen_record_is_historical_evidence(self):
        value=self.evidence();self.assertIsNotNone(value);self.assertFalse(value['ValidationRerunPerformed']);self.assertFalse(value['CurrentConstructorSuccessClaimed'])
    def test_changed_summary_bytes_reject(self):
        self.path.write_text(self.path.read_text()+' ');self.assertIsNone(self.evidence())
    def test_changed_capsule_rejects(self):
        self.capsule.write_bytes(b'different');self.assertIsNone(self.evidence())
    def test_unlisted_record_rejects(self):
        self.proof['_Baseline']['Snapshot']={};self.assertIsNone(self.evidence())
    def test_unknown_recorded_context_cannot_skip_the_prior_chain(self):
        self.summary['ContextSHA256']='unproven';self.path.write_text(json.dumps(self.summary));row=self.proof['_Baseline']['Snapshot'][self.path.name];row['ContextSHA256']='unproven';row['SummarySHA256']=V.digest(self.path);self.assertIsNone(self.evidence())
    def test_unverified_or_unsealed_proof_rejects(self):
        self.proof['_C4VerifiedCurrentContext']=False;self.assertIsNone(self.evidence())
    def test_status_promotion_is_not_a_compatibility_operation(self):
        self.summary['Status']='PASS';self.assertIsNone(self.evidence())


class ColourArtifactTests(unittest.TestCase):
    def setUp(self):
        self.temp=tempfile.TemporaryDirectory();self.addCleanup(self.temp.cleanup);self.root=Path(self.temp.name)
        self.values={
          'FormalExplanation':'Exact finite sum proof fixture; not actual physics.',
          'FiveGluonWXF':b'fixture tensor data', 'OneVectorWXF':b'fixture reference vector',
          'DedupController':{'Status':'PASS','ExitCode':0,'FilesSHA256':{'source/internal/HEPCAT-Color.wl':'old-colour'}},
          'NetworkController':{'Status':'PASS','ExitCode':0,'FilesSHA256':{'source/internal/HEPCAT-Color.wl':'old-colour'}},
          'FiveGluonSummary':{'Status':'COMPLETE','OriginalOccurrences':3960,'UniqueTensors':45,'OccurrenceReconstructionVerified':True,'SourceSHA256':{'source/internal/HEPCAT-Color.wl':'old-colour'}},
          'ProductionTierSummary':{'Status':'PASS','ExitCode':0,'SourceAndModelsUnchanged':True,'RequestedSuites':['fixture'],'CompletedSuites':['fixture'],'SourceAndModelSHA256':{'source/internal/HEPCAT-Color.wl':'new-colour'},'DeclaredFrozenContextSHA256':'new-context'}}
        self.paths={}
        for key,value in self.values.items():
            path=self.root/(key+('.wxf' if isinstance(value,bytes) else '.md' if isinstance(value,str) else '.json'))
            path.write_bytes(value if isinstance(value,bytes) else value.encode() if isinstance(value,str) else json.dumps(value).encode());self.paths[key]=path
        self.summary={'LiteralDedupSmallTests':{'Checks':23,'Status':'PASS'},'SparseNetworkSmallTests':{'Checks':42,'Status':'PASS'},
          'SparseUniqueMetric':{'Status':'COMPLETE','OriginalOccurrences':3960,'UniqueTensors':45,'Rank':6,'ExpandedCoordinateDimensions':[3960,6],'OccurrenceReconstructionVerified':True,'UniqueGramReconstructionVerified':True,'SourceFilesUnchanged':True},
          'FreshActualFiveGluonVector':{'OneVectorComponentwiseSameQ':True,'OneVectorComponents':32768,'OneVectorArtifactSHA256':V.digest(self.paths['OneVectorWXF'])},
          'ArtifactsSHA256':{'one-vector.wxf':V.digest(self.paths['OneVectorWXF']),'five-gluon.wxf':V.digest(self.paths['FiveGluonWXF'])},'UnchangedColorSourceSHA256':'old-colour'}
        self.paths['ReviewSummary']=self.root/'ReviewSummary.json';self.paths['ReviewSummary'].write_text(json.dumps(self.summary))
        self.review={'ColourArtifacts':{key:{'Path':path.name,'SHA256':V.digest(path)} for key,path in self.paths.items()}}
    def evidence(self):return V.colour_evidence(self.root,self.review)
    def change_summary(self):
        p=self.paths['ReviewSummary'];p.write_text(json.dumps(self.summary));self.review['ColourArtifacts']['ReviewSummary']['SHA256']=V.digest(p)
    def test_pinned_proof_data_remains_support_not_automatic_review(self):
        result=self.evidence();self.assertEqual(result['ControlChecks'],65);self.assertTrue(result['FiniteSumProofRequiresExplicitReview']);self.assertTrue(result['NoAmplitudeConstructionOrValidationClaimed'])
    def test_unpinned_tensor_bytes_cannot_supply_equivalence(self):
        self.paths['FiveGluonWXF'].write_bytes(b'changed');self.assertRaises(ValueError,self.evidence)
    def test_rank_or_incidence_reconstruction_failure_rejects(self):
        self.summary['SparseUniqueMetric']['UniqueGramReconstructionVerified']=False;self.change_summary();self.assertRaises(ValueError,self.evidence)
    def test_counts_cannot_replace_actual_componentwise_reference(self):
        self.summary['FreshActualFiveGluonVector']['OneVectorComponentwiseSameQ']=False;self.change_summary();self.assertRaises(ValueError,self.evidence)
    def test_missing_formal_explanation_is_not_ignored(self):
        self.review['ColourArtifacts'].pop('FormalExplanation');self.assertRaises(ValueError,self.evidence)
    def test_production_tier_must_complete_and_preserve_source(self):
        p=self.paths['ProductionTierSummary'];value=self.values['ProductionTierSummary'];value['SourceAndModelsUnchanged']=False;p.write_text(json.dumps(value));self.review['ColourArtifacts']['ProductionTierSummary']['SHA256']=V.digest(p);self.assertRaises(ValueError,self.evidence)
    def test_escaping_artifact_path_is_rejected(self):
        self.review['ColourArtifacts']['FormalExplanation']['Path']='../outside.md';self.assertRaises(ValueError,self.evidence)


if __name__=='__main__':unittest.main()
