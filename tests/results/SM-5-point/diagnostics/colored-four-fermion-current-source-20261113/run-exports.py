from pathlib import Path
import hashlib,importlib.util,json,os,signal,subprocess,time
ROOT=Path('/Users/neil/code/hepcat');HERE=ROOT/'tests/SM-5-point';D=Path(__file__).resolve().parent
spec=importlib.util.spec_from_file_location('hepcat_chiral_export_guard',HERE/'run.py');run=importlib.util.module_from_spec(spec);spec.loader.exec_module(run)
def sha(p):return hashlib.sha256(p.read_bytes()).hexdigest()
cfg=run.read_json(ROOT/'tests/results/SM-5-point/configuration.json');manifest=run.stage_context.manifest(ROOT,HERE,cfg);assert run.stage_context.fingerprint(manifest)=='48d82ad2adaf29f9a338c9fb6b9cf3da37ed7570f6de5852ee50c798fd0d8363'
guards={str(p):sha(p) for p in (D/'source.wl',D/'export-pilots.wl',D/'pilot-1/result.wxf',D/'pilot-2/result.wxf')};records=[]
for index in (1,2):
 out=D/('pilot-'+str(index));assert run.read_json(out/'result.json')['Status']=='ALL_NATIVE_CHECKS_PASS';assert run.stage_context.manifest(ROOT,HERE,cfg)==manifest;assert all(sha(Path(p))==h for p,h in guards.items());start=time.monotonic()
 with (out/'export.log').open('w') as stream:
  p=subprocess.Popen(['/Applications/Wolfram.app/Contents/MacOS/WolframKernel','-script',str(D/'export-pilots.wl')],cwd=ROOT,stdout=stream,stderr=subprocess.STDOUT,env=dict(os.environ,HEPCAT_COLORED_EXPORT_INDEX=str(index),HEPCAT_COLORED_EXPORT_CONTEXT=run.stage_context.fingerprint(manifest),OMP_NUM_THREADS='1',OPENBLAS_NUM_THREADS='1'),start_new_session=True)
  try:code=p.wait(timeout=600)
  except subprocess.TimeoutExpired:
   os.killpg(p.pid,signal.SIGTERM)
   try:p.wait(timeout=5)
   except subprocess.TimeoutExpired:os.killpg(p.pid,signal.SIGKILL);p.wait()
   code=124
 assert run.stage_context.manifest(ROOT,HERE,run.read_json(ROOT/'tests/results/SM-5-point/configuration.json'))==manifest;assert all(sha(Path(p))==h for p,h in guards.items())
 summary=run.read_json(out/'export.json',{});record={'Pilot':index,'ExitCode':code,'Status':summary.get('Status','NO_RESULT'),'Seconds':time.monotonic()-start,'SourceAndNativeCapsulesUnchanged':True,'GuardedSHA256':guards,'Summary':summary,'CanonicalWritten':False};records.append(record);run.atomic_json(D/'export-controller-summary.json',{'Records':records,'ProductionChanged':False,'CanonicalWritten':False});print(json.dumps(record,indent=2),flush=True)
 if code!=0 or summary.get('Status')!='PASS':break
