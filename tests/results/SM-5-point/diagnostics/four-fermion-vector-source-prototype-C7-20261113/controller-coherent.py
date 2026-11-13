"""Bounded isolated source-class experiment. It never edits production or campaign state."""
from pathlib import Path
import argparse,datetime,hashlib,importlib.util,json,os,signal,subprocess,time
ROOT=Path(__file__).resolve().parents[5];HERE=Path(__file__).resolve().parent
CONTEXT='4165f24c7d5501c40c03ea91f850a5cde77389a1236e3af397fb6d7e2344a167'
p=argparse.ArgumentParser();p.add_argument('--case',choices=['distinct-z','identical-z'],required=True);args=p.parse_args()
spec=importlib.util.spec_from_file_location('fourfv_context',ROOT/'tests/SM-5-point/run.py');w=importlib.util.module_from_spec(spec);spec.loader.exec_module(w)
cfg=json.loads((ROOT/'tests/results/SM-5-point/configuration.json').read_text());assert w.context_sha256(cfg)==CONTEXT
source=ROOT/'source/internal/HEPCAT-CovariantSources.wl';derivation=json.loads((HERE/'derivation.json').read_text());sha=lambda p:hashlib.sha256(p.read_bytes()).hexdigest();assert sha(source)==derivation['OriginalSourceSHA256'];assert sha(HERE/'prototype-source.wl')==derivation['PrototypeSourceSHA256']
inv=json.loads((ROOT/'tests/SM-5-point/inventory.json').read_text());wanted=['e','E','m','M','Z']if args.case=='distinct-z'else['e','E','E','e','Z'];family=next(f for f in inv['Families']if sorted(f['Particles'])==sorted(wanted));out=HERE/(args.case+'-coherent');out.mkdir(exist_ok=False)
plan={'FamilyID':family['ID'],'Particles':family['Particles'],'Incoming':family['Incoming'],'Outgoing':family['Outgoing'],'Class':'fourfv_'+args.case.replace('-','_')};(out/'plan.json').write_text(json.dumps(plan,indent=2)+'\n')
manifest=w.stage_context.manifest(ROOT,ROOT/'tests/SM-5-point',cfg);(out/'source-runtime-manifest.json').write_text(json.dumps(manifest,indent=2)+'\n');started=time.monotonic();env=dict(os.environ,HEPCAT_4FV_ROOT=str(ROOT),HEPCAT_4FV_OUTPUT=str(out),HEPCAT_4FV_PLAN=str(out/'plan.json'),OMP_NUM_THREADS='1',OPENBLAS_NUM_THREADS='1')
record={'Status':'RUNNING','Case':args.case,'Plan':plan,'ContextSHA256':CONTEXT,'WorkerSHA256':sha(HERE/'worker-coherent.wl'),'PrototypeSHA256':sha(HERE/'prototype-source.wl'),'StartedUTC':datetime.datetime.now(datetime.timezone.utc).isoformat(),'ExternalSeconds':2400,'MemoryGiB':4,'NoCanonicalOrModelWrite':True};(out/'controller.json').write_text(json.dumps(record,indent=2)+'\n')
with(out/'worker.log').open('w')as log:
 process=subprocess.Popen(['/Applications/Wolfram.app/Contents/MacOS/WolframKernel','-script',str(HERE/'worker-coherent.wl')],cwd=ROOT,env=env,stdout=log,stderr=subprocess.STDOUT,start_new_session=True)
 try:code=process.wait(timeout=2400)
 except subprocess.TimeoutExpired:
  os.killpg(process.pid,signal.SIGTERM)
  try:process.wait(timeout=5)
  except subprocess.TimeoutExpired:os.killpg(process.pid,signal.SIGKILL);process.wait()
  code=process.returncode;record['Timeout']=True
record.update(ExitCode=code,Seconds=time.monotonic()-started,InputsUnchanged=w.context_sha256(cfg)==CONTEXT and sha(HERE/'worker-coherent.wl')==record['WorkerSHA256']and sha(HERE/'prototype-source.wl')==record['PrototypeSHA256']);summary=out/'summary.json';record['WorkerSummary']=json.loads(summary.read_text())if summary.exists()else None;record['Status']='PASS'if record['InputsUnchanged']and code==0 and record['WorkerSummary']and record['WorkerSummary']['Status']=='PROTOTYPE_NATIVE_AND_EXTERNAL_PASS'else'NOT_ALL_PASS';(out/'controller.json').write_text(json.dumps(record,indent=2)+'\n');print(json.dumps(record,indent=2));raise SystemExit(0 if record['Status']=='PASS'else 1)
