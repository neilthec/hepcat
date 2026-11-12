"""Exercise real HEPCAT loading with the worker's synthetic startup controls.

Only the metadata subprocess is stubbed. No construction, model read, numerical
export or canonical checkpoint write is executed by these licensed-kernel tests.
"""
# HEPCAT - High Energy Physics Constructive Amplitude Tool
# Copyright (C) 2026 Neil Christensen
# SPDX-License-Identifier: GPL-3.0-or-later
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]
WORKER = ROOT / 'tests/SM-5-point/evaluate.wl'
KERNEL = shutil.which('WolframKernel') or '/Applications/Wolfram.app/Contents/MacOS/WolframKernel'

@unittest.skipUnless(Path(KERNEL).is_file(), 'A licensed Wolfram kernel is required.')
class WorkerStartupContextTests(unittest.TestCase):
    def startup(self, kind):
        prefix = WORKER.read_text().split('model=ReadModel[', 1)[0]
        prefix = prefix.replace('root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];',
                                'root=' + json.dumps(str(ROOT)) + ';')
        prefix = prefix.replace('configuration=Import[Environment["HEPCAT_SM5_CONFIGURATION"],"RawJSON"];',
                                'configuration=<|"SyntheticConfiguration"->17|>;')
        prefix = prefix.replace('inventory=Import[FileNameJoin[{root,"tests","SM-5-point","inventory.json"}],"RawJSON"];',
                                'inventory=<|"SyntheticInventory"->23|>;')
        prefix = prefix.replace('kind=Environment["HEPCAT_SM5_KIND"];id=Environment["HEPCAT_SM5_ID"];',
                                'kind=' + json.dumps(kind) + ';id="synthetic_case";')
        prefix = prefix.replace('seed=FromDigits[Environment["HEPCAT_SM5_SEED"]];', 'seed=31;')
        stub = ('Unprotect[RunProcess];Clear[RunProcess];'
                'RunProcess[_List]:=<|"ExitCode"->0,"StandardOutput"->'
                '"{\\"Status\\":\\"APPROVED\\",\\"SyntheticAdmission\\":29}"|>;Protect[RunProcess];\n')
        # A file Get parses each startup expression in its actual current context.
        # ToExpression on the whole prefix would bind every name before Get.
        tail = r'''
smoke=<|"Kind"->kind,"ID"->id,"Configuration"->configuration,
 "Inventory"->inventory,"Root"->root,"OutputDirectory"->outputDirectory,"Seed"->seed,
 "Admission"->readAdmission[],"LaunchAdmission"->If[launchAdmission===None,"None",launchAdmission],
 "Contexts"->{Context[kind],Context[id],Context[configuration],Context[inventory],
   Context[root],Context[outputDirectory],Context[seed],Context[readAdmission],
   Context[admissionCommand],Context[launchAdmission]},
 "PublicReadModelContext"->Context[ReadModel],"PublicReadModelAvailable"->(Length[DownValues[ReadModel]]>0)|>;
Print["SM5_STARTUP_JSON:",ExportString[smoke,"RawJSON","Compact"->True]];
Quit[0];
'''
        with tempfile.TemporaryDirectory(prefix='hepcat-sm5-worker-startup-') as directory:
            path = Path(directory) / 'startup.wl';path.write_text(stub + prefix + tail)
            process = subprocess.run([KERNEL,'-script',str(path)],cwd=ROOT,
                capture_output=True,text=True,timeout=60,
                env=dict(os.environ,OMP_NUM_THREADS='1',OPENBLAS_NUM_THREADS='1'))
        self.assertEqual(process.returncode,0,process.stdout + process.stderr)
        lines=[line.split('SM5_STARTUP_JSON:',1)[1] for line in process.stdout.splitlines()
               if 'SM5_STARTUP_JSON:' in line]
        self.assertEqual(len(lines),1,process.stdout + process.stderr)
        result=json.loads(lines[0])
        self.assertEqual(result['Kind'],kind)
        self.assertEqual(result['ID'],'synthetic_case')
        self.assertEqual(result['Configuration'],{'SyntheticConfiguration':17})
        self.assertEqual(result['Inventory'],{'SyntheticInventory':23})
        self.assertEqual(result['Root'],str(ROOT))
        self.assertEqual(result['OutputDirectory'],str(ROOT/'tests/results/SM-5-point'))
        self.assertEqual(result['Seed'],31)
        self.assertEqual(result['Admission'],{'Status':'APPROVED','SyntheticAdmission':29})
        self.assertEqual(result['LaunchAdmission'],result['Admission'] if kind=='export' else 'None')
        self.assertEqual(result['Contexts'],['SMFivePointRun`Private`']*10)
        self.assertEqual(result['PublicReadModelContext'],'ConstructiveDiagrams`')
        self.assertTrue(result['PublicReadModelAvailable'])
    def test_construct_controls_survive_actual_package_loading(self):
        self.startup('construct')
    def test_export_admission_and_controls_survive_actual_package_loading(self):
        self.startup('export')

if __name__=='__main__':unittest.main()
