import hashlib,importlib.util,json,os,secrets,signal,subprocess,time
from pathlib import Path
ROOT=Path('/Users/neil/code/hepcat');HERE=ROOT/'tests/SM-5-point';D=Path(__file__).resolve().parent
spec=importlib.util.spec_from_file_location('hepcat_chiral_pilot_guard',HERE/'run.py');run=importlib.util.module_from_spec(spec);spec.loader.exec_module(run)
def sha(p):return hashlib.sha256(p.read_bytes()).hexdigest()
cfg=run.read_json(ROOT/'tests/results/SM-5-point/configuration.json');manifest=run.stage_context.manifest(ROOT,HERE,cfg)
assert run.stage_context.fingerprint(manifest)=='b672a8945605ad07e3f24e47a783f071a54c9b057e44b65048267ec057d55afd'
guards={str(p):sha(p) for p in (D/'draft-sources.wl',D/'draft-contacts.wl',D/'pilot.wl')}
records=[]
for index in (1,2):
 assert run.stage_context.manifest(ROOT,HERE,run.read_json(ROOT/'tests/results/SM-5-point/configuration.json'))==manifest
 assert all(sha(Path(p))==h for p,h in guards.items())
 out=D/('pilot-'+str(index));out.mkdir(exist_ok=False);seed=secrets.randbelow(2**31-1)+1;started=time.monotonic()
 with (out/'worker.log').open('w') as stream:
  process=subprocess.Popen(['/Applications/Wolfram.app/Contents/MacOS/WolframKernel','-script',str(D/'pilot.wl')],cwd=ROOT,stdout=stream,stderr=subprocess.STDOUT,env=dict(os.environ,HEPCAT_CHIRAL_PILOT_INDEX=str(index),HEPCAT_CHIRAL_PILOT_SEED=str(seed),OMP_NUM_THREADS='1',OPENBLAS_NUM_THREADS='1'),start_new_session=True)
  try:code=process.wait(timeout=310)
  except subprocess.TimeoutExpired:
   os.killpg(process.pid,signal.SIGTERM)
   try:process.wait(timeout=5)
   except subprocess.TimeoutExpired:os.killpg(process.pid,signal.SIGKILL);process.wait()
   code=124
 assert run.stage_context.manifest(ROOT,HERE,run.read_json(ROOT/'tests/results/SM-5-point/configuration.json'))==manifest
 assert all(sha(Path(p))==h for p,h in guards.items())
 summary=run.read_json(out/'result.json',{})
 row={'Pilot':index,'ExitCode':code,'Seconds':time.monotonic()-started,'Seed':seed,'Status':summary.get('Status','NO_RESULT'),'AllSourceModelRuntimeFilesUnchanged':True,'DraftAndWorkerHashesUnchanged':True,'DraftAndWorkerSHA256':guards,'CanonicalWritten':False,'ProductionChanged':False,'Summary':summary}
 records.append(row);run.atomic_json(D/'pilot-controller-summary.json',{'Records':records,'ProductionChanged':False,'CanonicalWritten':False});print(json.dumps(row,indent=2),flush=True)
 if code!=0 or summary.get('Status')!='ALL_NATIVE_CHECKS_PASS':break
