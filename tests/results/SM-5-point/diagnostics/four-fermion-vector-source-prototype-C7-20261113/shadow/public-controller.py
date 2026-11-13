"""Run actual public default constructors using a fully isolated two-file source shadow."""
from pathlib import Path
import argparse, datetime, hashlib, importlib.util, json, os, signal, subprocess, time
HERE=Path(__file__).resolve().parent; ROOT=HERE.parents[5]
CONTEXT='4165f24c7d5501c40c03ea91f850a5cde77389a1236e3af397fb6d7e2344a167'
p=argparse.ArgumentParser();p.add_argument('--case',choices=['distinct-z','identical-z'],required=True);args=p.parse_args()
spec=importlib.util.spec_from_file_location('fourfv_public_context',ROOT/'tests/SM-5-point/run.py');w=importlib.util.module_from_spec(spec);spec.loader.exec_module(w)
cfg=json.loads((ROOT/'tests/results/SM-5-point/configuration.json').read_text());assert w.context_sha256(cfg)==CONTEXT
sha=lambda p:hashlib.sha256(p.read_bytes()).hexdigest();before=w.stage_context.manifest(ROOT,ROOT/'tests/SM-5-point',cfg);shadow=json.loads(json.dumps(before));candidate=json.loads((HERE/'candidate-manifest.json').read_text());expected={x['Path']:x for x in candidate['ChangedFiles']}
for path,value in list(shadow['Files'].items()):
 if path.startswith('source/'):
  source=HERE/path;assert source.is_file();shadow['Files'][path]=sha(source)
changed={p for p in before['Files'] if before['Files'][p]!=shadow['Files'][p]};assert changed==set(expected)
for path,item in expected.items():assert before['Files'][path]==item['BeforeSHA256']and shadow['Files'][path]==item['CandidateSHA256']
shadow_context=w.stage_context.fingerprint(shadow); inv=json.loads((ROOT/'tests/SM-5-point/inventory.json').read_text());wanted=['e','E','m','M','Z']if args.case=='distinct-z'else['e','E','e','E','Z'];family=next(f for f in inv['Families']if sorted(f['Particles'])==sorted(wanted));out=HERE/(args.case+'-public');out.mkdir(exist_ok=False)
plan={'FamilyID':family['ID'],'Particles':family['Particles']};(out/'plan.json').write_text(json.dumps(plan,indent=2)+'\n');(out/'shadow-runtime-manifest.json').write_text(json.dumps(shadow,indent=2)+'\n')
record={'Status':'RUNNING','Plan':plan,'OriginalProductionContextSHA256':CONTEXT,'ShadowSourceContextSHA256':shadow_context,'ChangedFiles':candidate['ChangedFiles'],'ScriptSHA256':sha(HERE/'public-worker.wl'),'StartedUTC':datetime.datetime.now(datetime.timezone.utc).isoformat(),'PublicOptions':['FeynmanModel','Output'],'OtherOptionsDefault':True,'ExternalSeconds':7200,'MemoryGiB':4,'CanonicalOrProductionWrites':False};(out/'controller.json').write_text(json.dumps(record,indent=2)+'\n');started=time.monotonic();env=dict(os.environ,HEPCAT_4FV_ROOT=str(ROOT),HEPCAT_4FV_SHADOW=str(HERE),HEPCAT_4FV_OUTPUT=str(out),HEPCAT_4FV_PLAN=str(out/'plan.json'),HEPCAT_4FV_SHADOW_CONTEXT=shadow_context,OMP_NUM_THREADS='1',OPENBLAS_NUM_THREADS='1')
with(out/'worker.log').open('w')as log:
 proc=subprocess.Popen(['/Applications/Wolfram.app/Contents/MacOS/WolframKernel','-script',str(HERE/'public-worker.wl')],cwd=ROOT,env=env,stdout=log,stderr=subprocess.STDOUT,start_new_session=True)
 try:code=proc.wait(timeout=7200)
 except subprocess.TimeoutExpired:
  os.killpg(proc.pid,signal.SIGTERM)
  try:proc.wait(timeout=5)
  except subprocess.TimeoutExpired:os.killpg(proc.pid,signal.SIGKILL);proc.wait()
  code=proc.returncode;record['Timeout']=True
unchanged=w.stage_context.manifest(ROOT,ROOT/'tests/SM-5-point',cfg)==before and sha(HERE/'public-worker.wl')==record['ScriptSHA256']and all(sha(HERE/p)==value for p,value in shadow['Files'].items()if p.startswith('source/')); summary=out/'summary.json';result=json.loads(summary.read_text())if summary.exists()else None
record.update(ExitCode=code,Seconds=time.monotonic()-started,InputsUnchanged=unchanged,WorkerSummary=result,Status='PASS'if unchanged and code==0 and result and result['Status']=='COMPLETE'else'NOT_ALL_PASS');(out/'controller.json').write_text(json.dumps(record,indent=2)+'\n');print(json.dumps(record,indent=2));raise SystemExit(0 if record['Status']=='PASS'else 1)
