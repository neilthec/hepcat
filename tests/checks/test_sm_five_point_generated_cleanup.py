"""Safety tests for disposable SM5 generated projects; no physics kernels."""
import copy
import fcntl
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest
from unittest import mock

ROOT = Path(__file__).resolve().parents[2]
MODULE = ROOT / 'tests/SM-5-point/generated_cleanup.py'
SPEC = importlib.util.spec_from_file_location('sm5_generated_cleanup_tests', MODULE)
CLEANUP = importlib.util.module_from_spec(SPEC); SPEC.loader.exec_module(CLEANUP)


class GeneratedCleanupTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix='hepcat-cleanup-tests-')
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name).resolve()
        subprocess.run(['git', 'init', '-q', str(self.root)], check=True)
        self.results = self.root / 'tests/results/SM-5-point'; self.results.mkdir(parents=True)
        self.family = {'ID':'family_a', 'ExportName':'toyFive', 'CaseIDs':['case_a','case_b'],
                       'Incoming':['e','E'], 'Outgoing':['A','A','A']}
        self.cases = {}
        for cid, particles in [('case_a',['e','E','A.+','A.+','A.+']),('case_b',['e','E','A.+','A.+','A.-'])]:
            capsule = self.results / (cid + '.wxf'); capsule.write_bytes(('native ' + cid).encode())
            value = {'ID':cid,'Status':'COMPLETE','Particles':particles,'Rows':4,'FeynmanRows':1,
                     'CheckStatuses':{'PASS':4},'ContextSHA256':'1'*64,'CapsuleSHA256':self.sha(capsule)}
            self.cases[cid] = value; self.write(cid+'.json',value)
        cap = self.results / 'family_a-export.wxf'; cap.write_bytes(b'export native CH MG results')
        self.export = {'FamilyID':'family_a','Status':'PASS','CalcHEPStatus':'PASS','MadGraphStatus':'PASS',
                       'Points':1000,'CaseIDs':['case_a','case_b'],'ContextSHA256':'1'*64,
                       'CapsuleSHA256':self.sha(cap),'CaseCapsulesSHA256':{cid:d['CapsuleSHA256'] for cid,d in self.cases.items()}}
        self.write('family_a-export.json',self.export)
        self.write('ledger.json',{'Preserved':True})
        self.target = self.root / 'tests/results/spinas-output/SM-5-point/toyFive'
        self.regenerate()
        patcher = mock.patch.object(CLEANUP,'_active_jobs',return_value=[]);patcher.start();self.addCleanup(patcher.stop)
        self.canonical = {p.name:p.read_bytes() for p in self.results.iterdir() if p.is_file()}

    @staticmethod
    def sha(path): return hashlib.sha256(path.read_bytes()).hexdigest()

    def write(self,name,value): (self.results/name).write_text(json.dumps(value))

    def regenerate(self):
        self.target.mkdir(parents=True,exist_ok=True)
        self.manifest = {'Generator':'HEPCAT','Class':'sm5toyFive',
                         'PhysicalProcess':{'Incoming':['e','E'],'Outgoing':['A','A','A']},
                         'InputProcesses':[v['Particles'] for v in self.cases.values()]}
        (self.target/'manifest.json').write_text(json.dumps(self.manifest))
        (self.target/'build').mkdir(exist_ok=True)
        (self.target/'build/cache.o').write_bytes(b'disposable compiled object')
        (self.target/'validation-report.json').write_text('{"Status":"PASS","Points":1000}')
        (self.target/'point.csv').write_text('point,value\n1,0\n')
        (self.target/'native.log').write_text('raw native validator output\n')
        (self.target/'param_card.dat').write_text('raw model parameters\n')

    def call(self,**kwargs):
        return CLEANUP.cleanup_completed_family(self.root,self.results,self.family,self.cases,self.export,**kwargs)

    def assert_kept(self,result):
        self.assertEqual(result['Status'],'RETAINED',result)
        self.assertTrue(self.target.exists())

    def change_export(self,**changes):
        self.export.update(changes);self.write('family_a-export.json',self.export)

    def change_case(self,cid='case_a',**changes):
        self.cases[cid].update(changes);self.write(cid+'.json',self.cases[cid])

    def test_strict_cleanup_preserves_canonical_artifacts_and_raw_evidence(self):
        result=self.call();self.assertEqual(result['Status'],'CLEANED',result)
        self.assertFalse(self.target.exists())
        for name,value in self.canonical.items():self.assertEqual((self.results/name).read_bytes(),value)
        journal=json.loads(Path(result['Journal']).read_text())
        self.assertEqual(journal['Status'],'CLEANED')
        kept=Path(result['Journal']).parent/'evidence'
        for name in ('manifest.json','validation-report.json','point.csv','native.log','param_card.dat'):
            self.assertTrue((kept/name).is_file())
        self.assertFalse((kept/'build/cache.o').exists())
        self.assertTrue(journal['BoundaryInventoryIsNotAHistoricalExecutionHash'])
        for row in journal['RetainedEvidence']:
            self.assertEqual(self.sha(kept/row['Path']),row['SHA256'])

    def test_plan_is_read_only(self):
        value=self.call(dry_run=True);self.assertEqual(value['Status'],'PLANNED',value)
        self.assertTrue(self.target.exists());self.assertFalse((self.results/'generated-cleanup').exists())

    def test_already_absent_is_idempotent(self):
        self.assertEqual(self.call()['Status'],'CLEANED')
        self.assertEqual(self.call()['Status'],'ALREADY_ABSENT')

    def test_new_validated_regeneration_uses_new_evidence_journal(self):
        first=self.call();self.regenerate()
        cap=self.results/'family_a-export.wxf';cap.write_bytes(b'new validation export result')
        self.change_export(CapsuleSHA256=self.sha(cap))
        second=self.call();self.assertEqual(second['Status'],'CLEANED',second)
        self.assertNotEqual(first['Journal'],second['Journal']);self.assertTrue(Path(first['Journal']).exists())

    def test_raw_failure_even_precision_qualified_is_retained(self):
        self.change_export(Status='FAIL',CalcHEPStatus='FAIL',PrecisionQualification={'Status':'PRECISION_QUALIFIED'})
        self.assert_kept(self.call())

    def test_incomplete_independent_backend_is_retained(self):
        for backend in ('CalcHEPStatus','MadGraphStatus'):
            with self.subTest(backend=backend):
                self.change_export(**{backend:'FAIL'});self.assert_kept(self.call());self.change_export(**{backend:'PASS'})

    def test_insufficient_points_are_retained(self):
        self.change_export(Points=999);self.assert_kept(self.call())

    def test_boolean_points_are_not_counts(self):
        self.change_export(Points=True);self.assert_kept(self.call())

    def test_missing_native_feynman_check_is_retained(self):
        self.change_case(FeynmanRows=0);self.assert_kept(self.call())

    def test_native_failed_or_partial_rows_are_retained(self):
        self.change_case(CheckStatuses={'PASS':3,'FAIL':1});self.assert_kept(self.call())

    def test_supplemental_failure_is_retained(self):
        self.cases['case_a']['SupplementalParameterProbes']={'Status':'FAIL'}
        self.assert_kept(self.call())

    def test_mutated_native_capsule_is_retained(self):
        (self.results/'case_a.wxf').write_bytes(b'changed');self.assert_kept(self.call())

    def test_mutated_export_capsule_is_retained(self):
        (self.results/'family_a-export.wxf').write_bytes(b'changed');self.assert_kept(self.call())

    def test_checked_export_disagreeing_with_disk_is_retained(self):
        self.export['Points']=2000;self.assert_kept(self.call())

    def test_changed_member_map_is_retained(self):
        self.change_export(CaseCapsulesSHA256={'case_a':'f'*64,'case_b':'e'*64});self.assert_kept(self.call())

    def test_historical_native_contexts_are_preserved(self):
        self.change_case(ContextSHA256='2'*64)
        contexts={cid:d['ContextSHA256'] for cid,d in self.cases.items()}
        self.change_export(NativeCaseContexts=contexts,ExporterContextSHA256='1'*64)
        self.cases['case_a']['ContextCompatibility']={'Status':'checked by workflow'}
        result=self.call();self.assertEqual(result['Status'],'CLEANED',result)
        journal=json.loads(Path(result['Journal']).read_text());self.assertEqual(journal['RawExportContextSHA256'],'1'*64)

    def test_mixed_context_without_binding_is_retained(self):
        self.change_case(ContextSHA256='2'*64);self.assert_kept(self.call())

    def test_exporter_context_contradiction_is_retained(self):
        self.change_export(ExporterContextSHA256='2'*64);self.assert_kept(self.call())

    def test_manifest_wrong_family_is_retained(self):
        self.manifest['Class']='sm5different';(self.target/'manifest.json').write_text(json.dumps(self.manifest));self.assert_kept(self.call())

    def test_manifest_wrong_crossing_is_retained(self):
        self.manifest['PhysicalProcess']['Incoming']=['A','A'];(self.target/'manifest.json').write_text(json.dumps(self.manifest));self.assert_kept(self.call())

    def test_manifest_wrong_amplitude_members_are_retained(self):
        self.manifest['InputProcesses']=[];(self.target/'manifest.json').write_text(json.dumps(self.manifest));self.assert_kept(self.call())

    def test_export_directory_traversal_is_retained(self):
        self.family['ExportName']='../outside';self.assert_kept(self.call())

    def test_noncanonical_results_directory_is_retained(self):
        result=CLEANUP.cleanup_completed_family(self.root,self.root,self.family,self.cases,self.export)
        self.assert_kept(result)

    def test_root_symlink_is_never_followed(self):
        alternate=self.target.with_name('external');self.target.rename(alternate);self.target.symlink_to(alternate,target_is_directory=True)
        result=self.call();self.assertEqual(result['Status'],'RETAINED');self.assertTrue((alternate/'manifest.json').exists())

    def test_ancestor_symlink_is_never_followed(self):
        parent=self.target.parent;alternate=parent.with_name('elsewhere');parent.rename(alternate);parent.symlink_to(alternate,target_is_directory=True)
        result=self.call();self.assertEqual(result['Status'],'RETAINED');self.assertTrue((alternate/'toyFive/manifest.json').exists())

    def test_inner_generated_symlinks_are_unlinked_without_following(self):
        outside=self.root/'outside';outside.mkdir();(outside/'keep.f').write_bytes(b'original external source')
        (self.target/'build/model.f').symlink_to(outside/'keep.f');(self.target/'build/modeldir').symlink_to(outside,target_is_directory=True)
        result=self.call();self.assertEqual(result['Status'],'CLEANED',result)
        self.assertEqual((outside/'keep.f').read_bytes(),b'original external source')
        journal=json.loads(Path(result['Journal']).read_text());self.assertEqual(sum(x['Kind']=='Symlink' for x in journal['GeneratedBoundaryInventory']),2)

    def test_symlink_raw_report_is_retained(self):
        outside=self.root/'secret.json';outside.write_text('do not follow')
        (self.target/'validation-report.json').unlink();(self.target/'validation-report.json').symlink_to(outside)
        self.assert_kept(self.call());self.assertEqual(outside.read_text(),'do not follow')

    def test_tracked_generated_file_is_retained(self):
        subprocess.run(['git','-C',str(self.root),'add','-f',str(self.target/'build/cache.o')],check=True)
        self.assert_kept(self.call())

    def test_active_family_or_case_is_retained(self):
        for identifier in ('family_a','case_a'):
            with self.subTest(identifier=identifier):self.assert_kept(self.call(active_ids={identifier}))

    def test_unknown_activity_state_is_retained(self):self.assert_kept(self.call(active_ids=None))

    def test_detected_worker_is_retained(self):
        with mock.patch.object(CLEANUP,'_active_jobs',return_value=[1234]):self.assert_kept(self.call())

    def test_unavailable_process_snapshot_is_retained(self):
        with mock.patch.object(CLEANUP,'_active_jobs',side_effect=OSError('no process inventory')):self.assert_kept(self.call())

    def test_another_runner_lock_prevents_cleanup(self):
        with (self.results/'run.lock').open('w') as lease:
            fcntl.flock(lease,fcntl.LOCK_EX|fcntl.LOCK_NB);self.assert_kept(self.call())

    def test_borrowed_runner_lock_stays_locked(self):
        with (self.results/'run.lock').open('w') as lease:
            fcntl.flock(lease,fcntl.LOCK_EX|fcntl.LOCK_NB)
            self.assertEqual(self.call(runner_lock=lease)['Status'],'CLEANED')
            with (self.results/'run.lock').open('r+') as other:
                with self.assertRaises(BlockingIOError):fcntl.flock(other,fcntl.LOCK_EX|fcntl.LOCK_NB)

    def test_failed_evidence_copy_does_not_delete_generated_root(self):
        with mock.patch.object(CLEANUP.shutil,'copyfile',side_effect=OSError('disk full')):self.assert_kept(self.call())

    def test_changed_tree_before_deletion_is_retained(self):
        original=CLEANUP._scan(self.target);changed=copy.deepcopy(original);changed[0]['ModifiedNS']+=1
        with mock.patch.object(CLEANUP,'_scan',side_effect=[original,changed]):self.assert_kept(self.call())

    def test_large_raw_report_is_retained_without_huge_copy(self):
        inventory=CLEANUP._scan(self.target)
        next(x for x in inventory if x['Path']=='point.csv')['Bytes']=65*1024**2
        with mock.patch.object(CLEANUP,'_scan',return_value=inventory):self.assert_kept(self.call())

    def test_compressed_raw_points_are_preserved(self):
        data=b'raw compressed exact point bytes'
        (self.target/'native-values.csv.gz').write_bytes(data)
        result=self.call();self.assertEqual(result['Status'],'CLEANED',result)
        self.assertEqual((Path(result['Journal']).parent/'evidence/native-values.csv.gz').read_bytes(),data)

    def test_failed_removal_is_reported_as_partial_and_not_hidden_on_retry(self):
        with mock.patch.object(CLEANUP.shutil,'rmtree',side_effect=OSError('removal interrupted')):
            result=self.call()
        self.assertEqual(result['Status'],'PARTIAL_CLEANUP',result)
        self.assertTrue(Path(result['Quarantine']).is_dir())
        self.assertTrue(Path(result['Journal']).is_file())
        self.assertEqual(self.call()['Status'],'PARTIAL_CLEANUP')
        for name,value in self.canonical.items():self.assertEqual((self.results/name).read_bytes(),value)

    def test_invalid_required_point_count_is_retained(self):self.assert_kept(self.call(required_points=0))


if __name__=='__main__':unittest.main()
