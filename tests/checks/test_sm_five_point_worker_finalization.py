"""Run the full worker with real WXF checkpoints and a synthetic exporter.

Package loading and serialization are real. Construction, model reads and all
external numerical programs are stubbed, and every output is temporary.
"""
# HEPCAT - High Energy Physics Constructive Amplitude Tool
# Copyright (C) 2026 Neil Christensen
# SPDX-License-Identifier: GPL-3.0-or-later
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

ROOT=Path(__file__).resolve().parents[2]
WORKER=ROOT/'tests/SM-5-point/evaluate.wl'
KERNEL=shutil.which('WolframKernel') or '/Applications/Wolfram.app/Contents/MacOS/WolframKernel'

@unittest.skipUnless(Path(KERNEL).is_file(),'A licensed Wolfram kernel is required.')
class WorkerFinalizationTests(unittest.TestCase):
    def run_export(self, *, mixed=False, break_json=False, schema="Particles", inventory_mismatch=False, rejected=False, corrupt_projection=False):
        with tempfile.TemporaryDirectory(prefix='hepcat-sm5-worker-finalization-') as directory:
            output=Path(directory)
            configuration={'ContextSHA256':'a'*64,'Points':1000,'MemoryGiB':4,
                'SPINASDirectory':'/synthetic/spinas','CalcHEPDirectory':'/synthetic/calchep',
                'MadGraphDirectory':'/synthetic/madgraph'}
            family={'ID':'synthetic_family','CaseIDs':['synthetic_case'],'ExportName':'Synthetic',
                'Particles':['h']*5,'Incoming':['h']*2,'Outgoing':['h']*3}
            admission={'Status':'APPROVED','FamilyID':'synthetic_family',
                'ExporterContextSHA256':'a'*64,'CaseIDs':['synthetic_case'],
                'NativeCaseContexts':{'synthetic_case':('b' if mixed else 'a')*64},
                'CaseCapsulesSHA256':{'synthetic_case':'REPLACE_WITH_ACTUAL_WXF_HASH'}}
            if mixed:
                admission['NativeContextCompatibility']={'Status':'EXACT_EXPORTER_ONLY_COMPATIBILITY',
                    'NativeContextSHA256':'b'*64,'ExporterContextSHA256':'a'*64,
                    'NativeRevalidationPerformed':False}
            fixture_entry={'Status':'COMPLETE','Expression':1}
            if schema in ('Particles','Both','Conflict','Malformed'):
                fixture_entry['Particles']=42 if schema=='Malformed' else ['h']*5
            if schema in ('Process','Both','Conflict'):
                fixture_entry['Process']=(['Z']+['h']*4) if schema=='Conflict' else ['h']*5
            expected_particles=(['Z']+['h']*4) if inventory_mismatch else ['h']*5
            literal=lambda value: 'ImportString['+json.dumps(json.dumps(value))+',"RawJSON"]'
            prelude='''fixtureDirectory=FIXTURE_DIRECTORY;
fixtureEntry=FIXTURE_ENTRY;
fixtureFile=FileNameJoin[{fixtureDirectory,"synthetic_case.wxf"}];
fixtureStream=OpenWrite[fixtureFile,BinaryFormat->True];
BinaryWrite[fixtureStream,Normal[BinarySerialize[fixtureEntry,PerformanceGoal->"Size"]],"Byte"];Close[fixtureStream];
Export[FileNameJoin[{fixtureDirectory,"native-before.sha256"}],IntegerString[FileHash[fixtureFile,"SHA256"],16,64],"Text"];
fixtureAdmission=FIXTURE_ADMISSION;
fixtureAdmission["CaseCapsulesSHA256"]=<|"synthetic_case"->IntegerString[FileHash[fixtureFile,"SHA256"],16,64]|>;
'''.replace('FIXTURE_DIRECTORY',json.dumps(str(output))).replace('FIXTURE_ADMISSION',literal(admission)).replace('FIXTURE_ENTRY',literal(fixture_entry))
            # Stub only admission metadata. Leave the actual RunProcess primitive
            # intact so projection invokes the real bounded Python/C++ helper.
            admission_body='''readAdmission[]:=Module[{command=RunProcess[admissionCommand],value},
    value=Quiet[Check[ImportString[command["StandardOutput"],"RawJSON"],$Failed]];
    If[command["ExitCode"]===0&&AssociationQ[value]&&Lookup[value,"Status",None]==="APPROVED",value,$Failed]];'''
            worker=WORKER.read_text()
            self.assertEqual(worker.count(admission_body),1)
            worker=worker.replace(admission_body,'readAdmission[]:=Global`fixtureAdmission;')
            worker=worker.replace('root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];',
                                              'root='+json.dumps(str(ROOT))+';')
            worker=worker.replace('configuration=Import[Environment["HEPCAT_SM5_CONFIGURATION"],"RawJSON"];',
                                  'configuration='+literal(configuration)+';')
            worker=worker.replace('inventory=Import[FileNameJoin[{root,"tests","SM-5-point","inventory.json"}],"RawJSON"];',
                                  'inventory='+literal({'Families':[family],'Cases':[{'ID':'synthetic_case','Particles':expected_particles}]})+';')
            worker=worker.replace('outputDirectory=FileNameJoin[{root,"tests","results","SM-5-point"}];',
                                  'outputDirectory='+json.dumps(str(output))+';')
            worker=worker.replace('kind=Environment["HEPCAT_SM5_KIND"];id=Environment["HEPCAT_SM5_ID"];',
                                  'kind="export";id="synthetic_family";')
            worker=worker.replace('seed=FromDigits[Environment["HEPCAT_SM5_SEED"]];','seed=17;')
            stubs='''
Unprotect[ReadModel,WriteSPINASProcess];Clear[ReadModel,WriteSPINASProcess];
ReadModel[___]:={"synthetic model"};
WriteSPINASProcess[___]:=<|"CalcHEPValidation"-><|"Status"->"PASS"|>,"MadGraphValidation"-><|"Status"->"PASS"|>|>;
Protect[ReadModel,WriteSPINASProcess];
'''
            if break_json:
                # Preserve real WXF writing, but make the following RawJSON summary invalid.
                stubs+='summarySerializationFailure=indeterminateSyntheticSymbol;\n'
                worker=worker.replace('summary=summary/._Missing->Null;',
                    'summary=summary/._Missing->Null;AssociateTo[summary,"SyntheticUnserializable"->summarySerializationFailure];')
            if corrupt_projection:
                worker=worker.replace('response=RunProcess[command];',
                    'response=RunProcess[command];Export[FileNameJoin[{directory,"input.wxf"}],"corrupt copied payload","Text"];')
            worker=worker.replace('$HistoryLength=0;','$HistoryLength=0;\n'+stubs)
            script=output/'full-worker.wl';script.write_text(prelude+worker)
            process=subprocess.run([KERNEL,'-script',str(script)],cwd=ROOT,capture_output=True,text=True,
                timeout=60,env=dict(os.environ,OMP_NUM_THREADS='1',OPENBLAS_NUM_THREADS='1'))
            self.assertEqual(hashlib.sha256((output/'synthetic_case.wxf').read_bytes()).hexdigest(),
                (output/'native-before.sha256').read_text().strip())
            projection_parent=output/'temporary'/'export-inputs'
            if projection_parent.is_dir():
                self.assertEqual(list(projection_parent.iterdir()),[],process.stdout+process.stderr)
            capsule=output/'synthetic_family-export.wxf'
            if rejected:
                self.assertEqual(process.returncode,2,process.stdout+process.stderr)
                self.assertIn('Native record external particles disagree with inventory',process.stdout)
                self.assertFalse(capsule.exists())
                self.assertFalse((output/'synthetic_family-export.json').exists())
                return
            self.assertTrue(capsule.is_file(),process.stdout+process.stderr)
            summary_path=output/'synthetic_family-export.json'
            if break_json:
                self.assertEqual(process.returncode,2,process.stdout+process.stderr)
                self.assertFalse(summary_path.is_file())
                self.assertIn('Checkpoint summary JSON could not be written',process.stdout)
                return
            self.assertEqual(process.returncode,0,process.stdout+process.stderr)
            self.assertTrue(summary_path.is_file(),process.stdout+process.stderr)
            summary=json.loads(summary_path.read_text())
            self.assertEqual(summary['Status'],'PASS')
            self.assertEqual(summary['CalcHEPStatus'],'PASS')
            self.assertEqual(summary['MadGraphStatus'],'PASS')
            self.assertEqual(summary['Points'],1000)
            self.assertEqual(summary['ContextSHA256'],'a'*64)
            self.assertEqual(summary['ExporterContextSHA256'],'a'*64)
            self.assertEqual(summary['NativeCaseContexts'],admission['NativeCaseContexts'])
            self.assertEqual(summary['CaseCapsulesSHA256']['synthetic_case'],
                hashlib.sha256((output/'synthetic_case.wxf').read_bytes()).hexdigest())
            self.assertEqual(summary['CapsuleSHA256'],hashlib.sha256(capsule.read_bytes()).hexdigest())
            self.assertEqual(summary['NativeContextCompatibility'],
                admission.get('NativeContextCompatibility'))
    def test_current_context_export_completes_actual_wxf_and_json_finalization(self):
        self.run_export()
    def test_mixed_context_export_retains_actual_input_context_in_both_checkpoints(self):
        self.run_export(mixed=True)
    def test_summary_serialization_failure_exits_nonzero_and_retains_wxf(self):
        self.run_export(break_json=True)
    def test_process_spelling_exports_without_rewriting_native_capsule(self):
        self.run_export(schema='Process')
    def test_agreeing_particles_and_process_are_accepted(self):
        self.run_export(schema='Both')
    def test_conflicting_particle_spellings_are_rejected(self):
        self.run_export(schema='Conflict',rejected=True)
    def test_missing_particle_spellings_are_rejected(self):
        self.run_export(schema='Missing',rejected=True)
    def test_nonlist_particle_spelling_is_rejected(self):
        self.run_export(schema='Malformed',rejected=True)
    def test_payload_checksum_failure_cleans_private_projection(self):
        self.run_export(corrupt_projection=True,rejected=True)
    def test_inventory_particle_disagreement_is_rejected(self):
        self.run_export(schema='Process',inventory_mismatch=True,rejected=True)


if __name__=='__main__':unittest.main()
