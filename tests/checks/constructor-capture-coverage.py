#!/usr/bin/env python3
"""Bounded behavioral test of the metadata capture's complete namespace coverage.

Load a tiny synthetic package, never HEPCAT models or physics. This explicit
integration check is separate from the fast data-only receipt unit tests.
"""
from pathlib import Path
import argparse
import hashlib
import importlib.util
import json
import os
import shutil
import signal
import subprocess
import time


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--kernel',default=shutil.which('WolframKernel') or '/Applications/Wolfram.app/Contents/MacOS/WolframKernel')
    parser.add_argument('--output',required=True,type=Path)
    args=parser.parse_args();output=args.output.resolve();output.mkdir(parents=True,exist_ok=False)
    workflow=Path(__file__).resolve().parents[1]/'SM-5-point'
    spec=importlib.util.spec_from_file_location('capture_coverage_receipts',workflow/'constructor_receipts.py');receipts=importlib.util.module_from_spec(spec);spec.loader.exec_module(receipts)
    script=workflow/'capture-constructor-definitions.wl';sha=lambda p:hashlib.sha256(p.read_bytes()).hexdigest();before=sha(script)
    names=[pattern[:-1]+('captureSentinel' if '`Private`'in pattern else 'CaptureSentinel')for pattern in receipts.CAPTURE_PATTERNS]
    fixture=output/'synthetic-package.wl';fixture.write_text('\n'.join(name+'[] := '+str(i)+';'for i,name in enumerate(names))+'\n')
    env=dict(os.environ,HEPCAT_FINGERPRINT_PACKAGE=str(fixture),HEPCAT_FINGERPRINT_DIRECTORY=str(output),HEPCAT_FINGERPRINT_PASS='1')
    started=time.monotonic();peak=0;reason=None
    with(output/'capture.log').open('w')as log:
        child=subprocess.Popen([args.kernel,'-script',str(script)],env=env,stdout=log,stderr=subprocess.STDOUT,start_new_session=True)
        try:
            while child.poll()is None:
                check=subprocess.run(['ps','-o','rss=','-p',str(child.pid)],capture_output=True,text=True,check=True)
                rss=check.stdout.strip();peak=max(peak,int(rss or'0')*1024)
                if peak>512*1024**2:reason='RESIDENT_MEMORY_LIMIT';break
                if time.monotonic()-started>30:reason='WALL_TIME_LIMIT';break
                try:child.wait(timeout=.1)
                except subprocess.TimeoutExpired:pass
        finally:
            if child.poll()is None:
                os.killpg(child.pid,signal.SIGTERM)
                try:child.wait(timeout=5)
                except subprocess.TimeoutExpired:os.killpg(child.pid,signal.SIGKILL);child.wait()
    actual=json.loads((output/'definitions-1-raw.json').read_text())if(output/'definitions-1-raw.json').is_file()else{}
    success=(child.returncode==0 and reason is None and actual.get('CapturedNamePatterns')==list(receipts.CAPTURE_PATTERNS)
        and all(name in actual.get('Definitions',{})and actual['Definitions'][name]['DownValues']!='{}'for name in names)
        and actual.get('ModelsRead')==[] and actual.get('PhysicsCalculationsPerformed')is False and sha(script)==before)
    result={'Status':'PASS'if success else'FAIL','Scope':'Synthetic namespace metadata capture, not a physics or HEPCAT validation',
        'AllTenOrderedPatternsCaptured':actual.get('CapturedNamePatterns')==list(receipts.CAPTURE_PATTERNS),
        'AllTenSyntheticSentinelsCaptured':all(name in actual.get('Definitions',{})for name in names),
        'CaptureToolSHA256':before,'ScriptUnchanged':sha(script)==before,'Seconds':time.monotonic()-started,
        'PeakKernelResidentBytes':peak,'ExitCode':child.returncode,'LimitReason':reason,'OwnedKernelCleanedUp':child.poll()is not None,
        'NoModelsOrPhysics':True}
    (output/'coverage-summary.json').write_text(json.dumps(result,indent=2)+'\n');print(json.dumps(result));return 0 if success else 1


if __name__=='__main__':raise SystemExit(main())
