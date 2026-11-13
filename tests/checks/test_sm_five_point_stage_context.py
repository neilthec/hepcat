"""Synthetic exact exporter-transition proofs; no physics kernels or exports."""
# HEPCAT - High Energy Physics Constructive Amplitude Tool
# Copyright (C) 2026 Neil Christensen
# SPDX-License-Identifier: GPL-3.0-or-later
import copy
import hashlib
import importlib.util
import json
from pathlib import Path
import tempfile
import unittest
from unittest import mock

HERE = Path(__file__).resolve().parents[1] / 'SM-5-point'
def load(name, path):
    spec = importlib.util.spec_from_file_location(name, path)
    value = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(value)
    return value
STAGE = load('sm5_stage_transition_test', HERE / 'stage_context.py')
WORKFLOW = load('sm5_stage_workflow_test', HERE / 'run.py')

class ExactExporterTransitionTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix='hepcat-sm5-transition-')
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.here = self.root / 'tests/SM-5-point'
        self.results = self.root / 'tests/results/SM-5-point'
        self.here.mkdir(parents=True)
        self.results.mkdir(parents=True)
        self.config = {'SPINASDirectory': str(self.root/'runtime/spinas'),
                       'MadGraphDirectory': str(self.root/'runtime/madgraph'),
                       'CalcHEPDirectory': str(self.root/'runtime/calchep')}
        self.inventory = {'Cases':[{'ID':'case_a','Particles':['h']*5}],
            'Families':[{'ID':'family_a','CaseIDs':['case_a']}]}
        self.write(self.here/'inventory.json', self.inventory)
        self.write(self.here/'supplemental-probes.json', {'SchemaVersion':1,'Probes':[]})
        for relative in (STAGE.EXPORTER, 'source/HEPCAT.wl','models/amps2.mdl',
             'tests/reference-models/calchep/lgrng1.mdl',
             'tests/reference-models/madgraph/sm/parameters.py',
             'runtime/spinas/include/particle.h','runtime/spinas/libspinas.a',
             'runtime/madgraph/VERSION','runtime/calchep/VERSION'):
            path=self.root/relative;path.parent.mkdir(parents=True,exist_ok=True)
            path.write_text('synthetic original '+relative)
        self.before=STAGE.manifest(self.root,self.here,self.config)
        self.old=STAGE.fingerprint(self.before)
        self.case=self.record('case_a', ID='case_a',Status='COMPLETE',
             Particles=['h']*5,Rows=3,FeynmanRows=1,CheckStatuses={'PASS':3})
        self.export=self.record('family_a-export',FamilyID='family_a',Status='PASS',
             CaseIDs=['case_a'],CaseCapsulesSHA256={'case_a':self.case['CapsuleSHA256']},
             CalcHEPStatus='PASS',MadGraphStatus='PASS',Points=1000)
        snapshots={path.name:{'RecordedContextSHA256':self.old,
            'SummarySHA256':STAGE.digest(path),'CapsuleSHA256':json.loads(path.read_text())['CapsuleSHA256']}
            for path in self.results.glob('*.json')}
        directory=self.root/'archive';directory.mkdir()
        self.write(directory/'before.json',self.before)
        self.write(directory/'checkpoints.json',snapshots)
        self.bridge={'SchemaVersion':1,'Kind':'ExactExporterOnlyTransition','Status':'PENDING',
            'ChangedFile':STAGE.EXPORTER,'NativeContextSHA256':self.old,
            'BeforeExporterSHA256':self.before['Files'][STAGE.EXPORTER],
            'BeforeManifest':'archive/before.json','BeforeManifestSHA256':STAGE.digest(directory/'before.json'),
            'CheckpointSnapshot':'archive/checkpoints.json',
            'CheckpointSnapshotSHA256':STAGE.digest(directory/'checkpoints.json')}
        self.write(self.here/STAGE.BRIDGE_NAME,self.bridge)
        (self.root/STAGE.EXPORTER).write_text('synthetic reviewed exporter')
        self.new=STAGE.fingerprint(STAGE.manifest(self.root,self.here,self.config))
        self.config['ContextSHA256']=self.new
        for key,value in {'ROOT':self.root,'HERE':self.here,'RESULTS':self.results}.items():
            patcher=mock.patch.object(WORKFLOW,key,value);patcher.start();self.addCleanup(patcher.stop)

    @staticmethod
    def write(path, value):
        path.write_text(json.dumps(value,sort_keys=True)+'\n')
    def record(self,stem,**fields):
        capsule=self.results/(stem+'.wxf');capsule.write_bytes(('synthetic '+stem).encode())
        row=dict(ContextSHA256=self.old,CapsuleSHA256=STAGE.digest(capsule),**fields)
        self.write(self.results/(stem+'.json'),row)
        return row
    def seal(self):
        STAGE.seal(self.root,self.here,self.config,STAGE.digest(self.root/STAGE.EXPORTER))
        return self.proof()
    def proof(self):
        return STAGE.verified_bridge(self.root,self.here,self.config,self.new)
    def checked(self, stem='case_a', suffix='', proof=None):
        return WORKFLOW.checked_summary(stem,suffix,self.new,proof or self.proof())

    def test_pending_bridge_does_not_admit_old_context(self):
        self.assertIsNone(self.proof())
        self.assertEqual(self.checked()['Status'],'STALE')
    def test_one_exact_pinned_change_admits_original_native_without_relabel(self):
        proof=self.seal();row=self.checked(proof=proof)
        self.assertEqual(row['Status'],'COMPLETE')
        self.assertEqual(row['ContextSHA256'],self.old)
        self.assertFalse(row['ContextCompatibility']['NativeRevalidationPerformed'])
        self.assertEqual(row['ContextCompatibility']['Stage'],'NativeCalculation')
        self.assertEqual(json.loads((self.results/'case_a.json').read_text()),self.case)
    def test_old_export_is_historical_report_with_original_context(self):
        self.seal();row=self.checked('family_a','-export')
        self.assertEqual(row['Status'],'PASS');self.assertEqual(row['ContextSHA256'],self.old)
        self.assertEqual(row['ContextCompatibility']['Stage'],'HistoricalExportReport')
    def test_native_summary_mutation_even_same_capsule_is_rejected(self):
        self.seal();self.case['Seconds']=12;self.write(self.results/'case_a.json',self.case)
        self.assertEqual(self.checked()['Status'],'STALE')
    def test_new_old_context_record_after_snapshot_is_rejected(self):
        self.seal();self.record('case_late',ID='case_late',Status='COMPLETE',Rows=3,
                              FeynmanRows=1,CheckStatuses={'PASS':3})
        self.assertEqual(self.checked('case_late')['Status'],'STALE')
    def test_native_capsule_mutation_is_rejected(self):
        self.seal();(self.results/'case_a.wxf').write_bytes(b'changed')
        self.assertEqual(self.checked()['Status'],'STALE')
    def test_old_export_summary_mutation_is_rejected(self):
        self.seal();self.export['Points']=1001;self.write(self.results/'family_a-export.json',self.export)
        self.assertEqual(self.checked('family_a','-export')['Status'],'STALE')
    def test_current_export_requires_exact_actual_native_context_map(self):
        self.seal();self.export.update(ContextSHA256=self.new,ExporterContextSHA256=self.new)
        for contexts in (None,{}, {'case_a':self.new},{'case_other':self.old}):
            with self.subTest(contexts=contexts):
                self.export.pop('NativeCaseContexts',None)
                if contexts is not None:self.export['NativeCaseContexts']=contexts
                self.write(self.results/'family_a-export.json',self.export)
                self.assertEqual(self.checked('family_a','-export')['Status'],'STALE')
        self.export['NativeCaseContexts']={'case_a':self.old}
        self.write(self.results/'family_a-export.json',self.export)
        self.assertEqual(self.checked('family_a','-export')['Status'],'PASS')
    def test_mixed_export_requires_consistent_explicit_producer_context(self):
        self.seal();self.export.update(ContextSHA256=self.new,NativeCaseContexts={'case_a':self.old})
        for producer in (None, self.old, {}, 'unrelated'):
            with self.subTest(producer=producer):
                self.export.pop('ExporterContextSHA256',None)
                if producer is not None:self.export['ExporterContextSHA256']=producer
                self.write(self.results/'family_a-export.json',self.export)
                self.assertEqual(self.checked('family_a','-export')['Status'],'STALE')

    def test_current_export_hashes_must_still_match_native_capsules(self):
        self.seal();self.export.update(ContextSHA256=self.new,ExporterContextSHA256=self.new,NativeCaseContexts={'case_a':self.old},
                                      CaseCapsulesSHA256={'case_a':'0'*64})
        self.write(self.results/'family_a-export.json',self.export)
        self.assertEqual(self.checked('family_a','-export')['Status'],'STALE')
    def test_admission_exposes_current_exporter_and_actual_native_contexts(self):
        self.seal();row=WORKFLOW.export_input_admission('family_a',self.config)
        self.assertEqual(row['Status'],'APPROVED')
        self.assertEqual(row['ExporterContextSHA256'],self.new)
        self.assertEqual(row['NativeCaseContexts'],{'case_a':self.old})
        self.assertEqual(row['CaseCapsulesSHA256'],{'case_a':self.case['CapsuleSHA256']})
    def test_export_admission_rejects_stale_configuration(self):
        self.seal();self.config['ContextSHA256']=self.old
        self.assertEqual(WORKFLOW.export_input_admission('family_a',self.config)['Status'],'REJECTED')
    def test_any_second_source_model_reference_runtime_change_closes_bridge(self):
        self.seal()
        for relative in ('source/HEPCAT.wl','models/amps2.mdl',
            'tests/reference-models/calchep/lgrng1.mdl',
            'tests/reference-models/madgraph/sm/parameters.py',
            'runtime/spinas/include/particle.h','runtime/spinas/libspinas.a','runtime/madgraph/VERSION'):
            with self.subTest(relative=relative):
                path=self.root/relative;old=path.read_bytes();path.write_bytes(old+b'changed')
                self.assertIsNone(self.proof());path.write_bytes(old)
    def test_added_and_removed_tracked_path_close_bridge(self):
        self.seal();path=self.root/'source/new.wl';path.write_text('added')
        self.assertIsNone(self.proof());path.unlink()
        path=self.root/'source/HEPCAT.wl';old=path.read_bytes();path.unlink()
        self.assertIsNone(self.proof());path.write_bytes(old)
    def test_program_path_change_closes_bridge(self):
        self.seal();self.config['MadGraphDirectory']='/another/install'
        self.assertIsNone(self.proof())
    def test_tampered_manifest_or_snapshot_closes_bridge(self):
        self.seal()
        for relative in ('archive/before.json','archive/checkpoints.json'):
            path=self.root/relative;old=path.read_bytes();path.write_bytes(old+b' ')
            self.assertIsNone(self.proof());path.write_bytes(old)
    def test_wrong_post_exporter_hash_closes_bridge(self):
        self.seal();bridge=json.loads((self.here/STAGE.BRIDGE_NAME).read_text())
        bridge['AfterExporterSHA256']='0'*64;self.write(self.here/STAGE.BRIDGE_NAME,bridge)
        self.assertIsNone(self.proof())
    def test_seal_rejects_wrong_requested_exporter_hash(self):
        with self.assertRaises(ValueError):STAGE.seal(self.root,self.here,self.config,'0'*64)
    def test_seal_cannot_admit_second_transition(self):
        self.seal()
        with self.assertRaises(ValueError):self.seal()
    def test_archive_paths_cannot_traverse(self):
        self.bridge['BeforeManifest']='../outside.json';self.write(self.here/STAGE.BRIDGE_NAME,self.bridge)
        with self.assertRaises(ValueError):self.seal()
    def test_worker_checks_inputs_before_and_after_numerical_export(self):
        worker=(HERE/'evaluate.wl').read_text()
        self.assertLess(worker.index('launchAdmission=If['),worker.index('Get['))
        self.assertIn('admission=!=launchAdmission',worker)
        self.assertLess(worker.index('admission=readAdmission[]'),worker.index('WriteSPINASProcess['))
        export_call = worker.index('WriteSPINASProcess[')
        projection_guard = worker.index('If[readAdmission[]=!=admission')
        self.assertGreater(projection_guard, worker.index('entries=MapThread[projectNativeForExport'))
        self.assertLess(projection_guard, export_call)
        self.assertGreater(worker.index('If[readAdmission[]=!=admission', export_call), export_call)
        self.assertIn('"NativeCaseContexts"->admission["NativeCaseContexts"]',worker)
        self.assertIn('"ExporterContextSHA256"->admission["ExporterContextSHA256"]',worker)

if __name__=='__main__':unittest.main()
