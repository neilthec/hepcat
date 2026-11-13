from pathlib import Path
import hashlib,importlib.util,json,os,signal,subprocess,time
ROOT=Path('/Users/neil/code/hepcat');D=Path(__file__).resolve().parent
spec=importlib.util.spec_from_file_location('c7_shadow_stage',ROOT/'tests/SM-5-point/stage_context.py');stage=importlib.util.module_from_spec(spec);spec.loader.exec_module(stage)
sha=lambda p:hashlib.sha256(p.read_bytes()).hexdigest()
configuration=json.loads((ROOT/'tests/results/SM-5-point/configuration.json').read_text());manifest=stage.manifest(ROOT,ROOT/'tests/SM-5-point',configuration)
assert stage.fingerprint(manifest)=='4165f24c7d5501c40c03ea91f850a5cde77389a1236e3af397fb6d7e2344a167'
guards={ROOT/name:digest for name,digest in manifest['Files'].items() if not Path(name).is_absolute()};guards[D/'construct.wl']=sha(D/'construct.wl')
assert all(sha(p)==h for p,h in guards.items())
(D/'source-runtime-manifest.json').write_text(json.dumps(manifest,indent=2,sort_keys=True)+'\n')
running=[]
for index,label in [(1,'public-muAHH'),(2,'public-uUZhh')]:
 output=D/label;output.mkdir(exist_ok=False);stream=(output/'construct.log').open('x');began=time.monotonic()
 process=subprocess.Popen(['/Applications/Wolfram.app/Contents/MacOS/WolframKernel','-script',str(D/'construct.wl')],cwd=ROOT,stdout=stream,stderr=subprocess.STDOUT,start_new_session=True,
 env=dict(os.environ,OMP_NUM_THREADS='1',OPENBLAS_NUM_THREADS='1',HEPCAT_C7_SHADOW_OUTPUT=str(output),HEPCAT_C7_SHADOW_INDEX=str(index),HEPCAT_C7_PROSPECTIVE_CONTEXT='4165f24c7d5501c40c03ea91f850a5cde77389a1236e3af397fb6d7e2344a167'))
 running.append((index,output,process,stream,began))
results=[]
while running:
 for item in list(running):
  index,output,process,stream,began=item;code=process.poll()
  if code is None and time.monotonic()-began>960:
   os.killpg(process.pid,signal.SIGTERM)
   try:process.wait(timeout=5)
   except subprocess.TimeoutExpired:os.killpg(process.pid,signal.SIGKILL);process.wait()
   code=124
  if code is None:continue
  stream.close();same=stage.manifest(ROOT,ROOT/'tests/SM-5-point',configuration)==manifest and all(sha(p)==h for p,h in guards.items())
  summary=json.loads((output/'construction.json').read_text())if(output/'construction.json').exists()else{}
  record={'Index':index,'ExitCode':code,'Seconds':time.monotonic()-began,'SourceRuntimeAndScriptUnchanged':same,'SourceRuntimeManifestSHA256':sha(D/'source-runtime-manifest.json'),'PublicOptionsSupplied':['FeynmanModel','Output'],'Summary':summary,'ProductionChanged':False,'CanonicalWritten':False}
  (output/'controller.json').write_text(json.dumps(record,indent=2)+'\n');results.append(record);running.remove(item)
  (D/'controller-summary.json').write_text(json.dumps({'Records':results,'ProductionChanged':False,'CanonicalWritten':False},indent=2)+'\n');print(json.dumps(record,indent=2),flush=True)
 if running:time.sleep(.25)
raise SystemExit(0 if len(results)==2 and all(r['ExitCode']==0 and r['SourceRuntimeAndScriptUnchanged']and r['Summary'].get('Status')=='COMPLETE'for r in results)else 1)
