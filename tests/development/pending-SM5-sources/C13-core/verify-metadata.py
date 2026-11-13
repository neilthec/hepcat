#!/usr/bin/env python3
"""Run only the bounded metadata tier. Raw results must stay outside the repo."""
from pathlib import Path
import argparse,hashlib,importlib.util,json,os,signal,subprocess,sys,time
sys.dont_write_bytecode=True
A=Path(__file__).resolve().parent
spec=importlib.util.spec_from_file_location('c13restore',A/'restore.py');restore=importlib.util.module_from_spec(spec);spec.loader.exec_module(restore)
sha=lambda p:hashlib.sha256(Path(p).read_bytes()).hexdigest()
def main():
 p=argparse.ArgumentParser(description=__doc__);p.add_argument('--baseline',required=True,type=Path);p.add_argument('--candidate',required=True,type=Path);p.add_argument('--output',required=True,type=Path);p.add_argument('--kernel',default='/Applications/Wolfram.app/Contents/MacOS/WolframKernel');x=p.parse_args();base=x.baseline.resolve(strict=True);candidate=x.candidate.resolve(strict=True);out=x.output.resolve();m=json.loads((A/'manifest.json').read_text());restore.verify_baseline(base,m)
 if out.exists()or restore.inside(out,base)or restore.inside(out,A):raise ValueError('Results must be new and outside baseline/archive')
 for item in m['Files']:assert sha(candidate/item['Path'])==item['CandidateSHA256']
 reference=candidate/'reference-extension.wl';assert sha(reference)==m['AppendedQCDDefinitionsSHA256']
 for path,digest in m['CodeFilesSHA256'].items():assert sha(A/path)==digest
 out.mkdir(parents=True);inputs={str(v):sha(v)for v in(candidate/'source').rglob('*')if v.is_file()};env=dict(os.environ,HEPCAT_C13_BASE_SOURCE=str(base/'source'),HEPCAT_C13_SOURCE=str(candidate/'source'),HEPCAT_C13_REFERENCE_EXTENSION=str(reference),HEPCAT_C13_TEST_OUTPUT=str(out),HEPCAT_TEST_ROOT=str(base),OMP_NUM_THREADS='1',OPENBLAS_NUM_THREADS='1');rows=[]
 for name in('definition-isolation','metadata-dispatch'):
  script=A/'tests'/(name+'.wl');entry=out/(name+'-entry.wl');entry.write_text('MemoryConstrained[TimeConstrained[Get['+json.dumps(str(script))+'],25,Quit[2]],512 2^20,Quit[2]];\n');start=time.monotonic();peak=0;reason=None
  with(out/(name+'.log')).open('w')as log:
   child=subprocess.Popen([x.kernel,'-script',str(entry)],cwd=base,env=env,stdout=log,stderr=subprocess.STDOUT,start_new_session=True)
   def stop():
    if child.poll()is None:
     os.killpg(child.pid,signal.SIGTERM)
     try:child.wait(.5)
     except subprocess.TimeoutExpired:os.killpg(child.pid,signal.SIGKILL);child.wait()
   try:
    while child.poll()is None:
     q=subprocess.run(['ps','-o','rss=','-p',str(child.pid)],capture_output=True,text=True)
     if q.returncode and child.poll()is None:raise RuntimeError('Owned-process inspection failed')
     if q.stdout.strip():peak=max(peak,int(q.stdout.strip())*1024)
     if time.monotonic()-start>29 or peak>512*2**20:reason='ClockOrMemory';stop();break
     time.sleep(.05)
   except BaseException as error:reason=str(error);stop()
   code=child.wait()
  result=json.loads((out/(name+'.json')).read_text())if(out/(name+'.json')).exists()else{};row={'Suite':name,'ExitCode':code,'Reason':reason,'Seconds':time.monotonic()-start,'PeakRSSBytes':peak,'Passed':result.get('Passed',0),'Failed':result.get('Failed'),'Status':result.get('Status','NO_SUMMARY'),'OwnedKernelExited':child.poll()is not None};rows.append(row);(out/'summary.json').write_text(json.dumps({'Results':rows},indent=2)+'\n')
  if code or reason or row['Status']!='PASS':raise RuntimeError('Stopped on non-PASS metadata suite')
 restore.verify_baseline(base,m);assert all(sha(v)==h for v,h in inputs.items());(out/'summary.json').write_text(json.dumps({'Status':'PASS','Results':rows,'InputsUnchanged':True,'Scope':'MetadataOnlyNoPhysics'},indent=2)+'\n');print('Metadata PASS',sum(r['Passed']for r in rows))
if __name__=='__main__':main()
