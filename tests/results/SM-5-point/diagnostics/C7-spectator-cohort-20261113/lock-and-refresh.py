"""Hold the normal campaign lock for the already running sole cohort writer and refresh completed-family checkpoints."""
from pathlib import Path
import datetime,fcntl,hashlib,importlib.util,json,os,subprocess,time
ROOT=Path('/Users/neil/code/hepcat');HERE=ROOT/'tests/SM-5-point';RESULTS=ROOT/'tests/results/SM-5-point';D=Path(__file__).resolve().parent
spec=importlib.util.spec_from_file_location('c7_spectator_refresh_run',HERE/'run.py');run=importlib.util.module_from_spec(spec);spec.loader.exec_module(run)
load=lambda p:json.loads(p.read_text());sha=lambda p:hashlib.sha256(p.read_bytes()).hexdigest()
cohort=load(D/'cohort.json');config=load(RESULTS/'configuration.json');ctx=cohort['ContextSHA256'];manifest=cohort['Manifest']
assert run.stage_context.fingerprint(run.stage_context.manifest(ROOT,HERE,config))==ctx
command=str((D/'controller.py').relative_to(ROOT))
lines=subprocess.check_output(['ps','-Ao','pid=,command='],text=True).splitlines()
pids=[int(line.strip().split(None,1)[0])for line in lines if command in line and 'Python.app/Contents/MacOS/Python '+command in line]
assert len(pids)==1,pids
pid=pids[0];lock=(RESULTS/'run.lock').open('a+');fcntl.flock(lock,fcntl.LOCK_EX|fcntl.LOCK_NB)
audit={'Status':'LOCK_HELD','ControllerPID':pid,'Lock':'tests/results/SM-5-point/run.lock','ContextSHA256':ctx,'CompanionScriptSHA256':sha(Path(__file__)),'StartedUTC':run.timestamp(),'NoScatteringKernelLaunched':True,'NoConfigurationOrSourceWrites':True,'Refreshes':[]}
run.atomic_json(D/'ledger-refresh-audit.json',audit);seen=set()
def guard():
 assert load(RESULTS/'configuration.json')==config
 assert run.stage_context.manifest(ROOT,HERE,config)==manifest
 assert all(sha(ROOT/p)==h for p,h in cohort['WorkflowHashes'].items())
while True:
 guard();progress=run.read_json(D/'progress.json',{})
 exports={r['ID']:r for r in progress.get('Records',[])if r.get('Kind')=='export'}
 fresh=sorted(set(exports)-seen);finished=progress.get('Status')in {'PASS','STOPPED_ON_FAILURE'}
 alive=True
 try:os.kill(pid,0)
 except ProcessLookupError:alive=False
 if fresh or finished or not alive:
  state=run.refresh(run.read_json(HERE/'inventory.json'),run.read_json(run.PROGRESS,{}),ctx,1000)
  state['ActiveCohort']='tests/results/SM-5-point/diagnostics/C7-spectator-cohort-20261113/cohort.json'
  state['StopReason']='C7SpectatorCohortRunning'if alive and not finished else progress.get('Status','ControllerStopped')
  if finished or not alive:state['CurrentCase']=None;state['CurrentFamily']=None
  run.atomic_json(run.PROGRESS,state);guard()
  audit['Refreshes'].append({'UTC':run.timestamp(),'NewExportFamilies':fresh,'Counts':state['Counts'],'ProgressSHA256':sha(run.PROGRESS),'SourceModelRuntimeGuard':True})
  seen.update(fresh);run.atomic_json(D/'ledger-refresh-audit.json',audit)
 if finished or not alive:
  audit['Status']='COMPLETE'if progress.get('Status')=='PASS'else 'STOPPED_WITH_CHECKPOINTS';audit['FinalUTC']=run.timestamp();run.atomic_json(D/'ledger-refresh-audit.json',audit);break
 time.sleep(2)
