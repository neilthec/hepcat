"""Render tiny synthetic notebooks with absent scratch data; no physics calls."""
# HEPCAT - High Energy Physics Constructive Amplitude Tool
# Copyright (C) 2026 Neil Christensen
# SPDX-License-Identifier: GPL-3.0-or-later
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import signal
import struct
import subprocess
import tempfile
import time
import unittest
import zlib

ROOT=Path(__file__).resolve().parents[2];HERE=ROOT/'tests/SM-5-point';BUILDER=HERE/'build-notebook.wl'
KERNEL=Path('/Applications/Wolfram.app/Contents/MacOS/WolframKernel')
spec=importlib.util.spec_from_file_location('missing_results_wire',HERE/'prime-notebook-display-cache.py')
WIRE=importlib.util.module_from_spec(spec);spec.loader.exec_module(WIRE)
S=WIRE.string_payload;A=WIRE.association_payload;V=WIRE.varint
F=lambda name,values:b'f'+V(len(values))+b's'+V(len(name))+name.encode()+b''.join(values)
L=lambda values:F('List',values)
I=lambda value:b'L'+struct.pack('<q',value)

@unittest.skipUnless(KERNEL.is_file(),'Licensed Wolfram kernel required')
class MissingResultsNotebookTests(unittest.TestCase):
    def render(self,mode):
        with tempfile.TemporaryDirectory(prefix='hepcat-missing-notebook-results-')as temp:
            base=Path(temp);results=base/'results';output=base/'worked.nb';inventory=base/'inventory.json';progress=base/'progress.json'
            case_id='sm5_fixture';family_id='sm5f_fixture';particles=['h']*5
            inventory.write_text(json.dumps({'Cases':[{'ID':case_id,'Particles':particles,'FamilyID':family_id,'Sector':'PureHiggs'}],
                'Families':[{'ID':family_id,'Particles':particles,'CaseIDs':[case_id],'Incoming':['h']*2,'Outgoing':['h']*3,'ExportName':'toyHiggs','Sector':'PureHiggs'}]}))
            native={'ID':case_id,'Status':'COMPLETE','CapsuleSHA256':'0'*64}
            family={'FamilyID':family_id,'Status':'PASS','CaseIDs':[case_id],'CapsuleSHA256':'0'*64}
            if mode!='absent':
                results.mkdir();cap=results/(case_id+'.wxf')
                cap.write_bytes(b'8C:'+zlib.compress(A([('ID',S(case_id)),('Status',S('COMPLETE')),('Expression',I(7)),('Prints',L([S('actual synthetic captured formula')]))])))
                native['CapsuleSHA256']=hashlib.sha256(cap.read_bytes()).hexdigest()
                if mode in ('complete','changed'):
                    exp=results/(family_id+'-export.wxf')
                    exp.write_bytes(b'8C:'+zlib.compress(A([('Status',S('PASS')),('Prints',L([S('actual synthetic comparison')])) ,('Result',A([('Directory',S('toy-directory'))]))])))
                    family['CapsuleSHA256']=hashlib.sha256(exp.read_bytes()).hexdigest()
                if mode=='changed':cap.write_bytes(cap.read_bytes()+b'changed')
            counts={'Constructed':1,'RecordedInputPassCases':1,'FullyValidatedCases':1,'PrecisionQualifiedCases':0,
                'ValidatedIncludingPrecisionQualifiedCases':1,'SupplementalParameterProbeFailedCases':0}
            progress.write_text(json.dumps({'Counts':counts,'Cases':{case_id:native},'Families':{family_id:family}}))
            args=['--inventory',str(inventory),'--results',str(results),'--progress',str(progress),
                '--families',family_id,'--probes',str(base/'no-probes.json'),'--output',str(output)]
            started=time.monotonic();peak=0
            with (base/'kernel.log').open('w')as log:
                proc=subprocess.Popen([str(KERNEL),'-script',str(BUILDER),*args],cwd=ROOT,stdout=log,stderr=subprocess.STDOUT,start_new_session=True)
                try:
                    while proc.poll()is None:
                        rss=subprocess.run(['ps','-o','rss=','-p',str(proc.pid)],text=True,capture_output=True).stdout.strip()
                        peak=max(peak,int(rss or'0')*1024)
                        if peak>512<<20 or time.monotonic()-started>30:
                            os.killpg(proc.pid,signal.SIGKILL);proc.wait();self.fail('Tiny formatter exceeded512MiB/30s')
                        time.sleep(.1)
                finally:
                    if proc.poll()is None:os.killpg(proc.pid,signal.SIGKILL);proc.wait()
            text=(base/'kernel.log').read_text();self.assertEqual(proc.returncode,0,text);self.assertNotIn('::shdw',text)
            self.assertIn('with 1 families',text);self.assertLess(output.stat().st_size,1<<20)
            return output.read_text(),results.exists()

    def test_absent_results_does_not_repeat_durable_pass_counts(self):
        text,exists=self.render('absent');self.assertFalse(exists)
        self.assertIn('sm5-progress-unavailable',text);self.assertNotIn('Recorded constructions: 1',text)
        self.assertIn('to rebuild them',text)

    def test_partial_cache_keeps_formula_but_not_missing_backend_pass_total(self):
        text,_=self.render('partial');self.assertIn('actual synthetic captured formula',text)
        self.assertIn('sm5-progress-unavailable',text);self.assertNotIn('current validated total is 1',text)

    def test_changed_capsule_cannot_retain_the_old_qualification_total(self):
        text,_=self.render('changed');self.assertIn('sm5-progress-unavailable',text)
        self.assertNotIn('current validated total is 1',text)

    def test_complete_matching_cache_can_display_its_recorded_count(self):
        text,_=self.render('complete');self.assertIn('Recorded constructions: 1',text)
        self.assertIn('current validated total is 1',text);self.assertNotIn('sm5-progress-unavailable',text)

if __name__=='__main__':unittest.main()
