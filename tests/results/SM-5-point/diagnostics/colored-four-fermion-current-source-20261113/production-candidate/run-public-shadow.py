from pathlib import Path
import hashlib,importlib.util,json,os,signal,subprocess,time
ROOT=Path('/Users/neil/code/hepcat');D=Path(__file__).resolve().parent;P=D.parent;HERE=ROOT/'tests/SM-5-point'
sp=importlib.util.spec_from_file_location('coloured_public_shadow_stage',HERE/'stage_context.py');stage=importlib.util.module_from_spec(sp);sp.loader.exec_module(stage)
sha=lambda p:hashlib.sha256(p.read_bytes()).hexdigest();load=lambda p:json.loads(p.read_text())
cfg=load(ROOT/'tests/results/SM-5-point/configuration.json');baseline=stage.manifest(ROOT,HERE,cfg);assert stage.fingerprint(baseline)=='48d82ad2adaf29f9a338c9fb6b9cf3da37ed7570f6de5852ee50c798fd0d8363'
shadow=load(D/'shadow-source-manifest.json');prospective=load(D/'prospective-manifest.json');ctx=prospective['ProspectiveContextSHA256'];guards={str(p):sha(p)for p in [P/'equal-mass-probe.wl',D/'public-shadow.wl',Path(__file__)]}
def guard():
 assert stage.manifest(ROOT,HERE,cfg)==baseline
 assert all(sha(D/'shadow'/p)==h for p,h in shadow['Files'].items())
 assert all(sha(Path(p))==h for p,h in guards.items())
records=[]
for index in [0,1,2,3,4]:
 guard();target=P/'equal-mass-probe.wl'if index==0 else D/'public-shadow.wl';out=P/'equal-mass-probe'if index==0 else D/f'public-{index}';out.mkdir(exist_ok=True);start=time.monotonic()
 with (out/'kernel.log').open('w')as log:
  proc=subprocess.Popen(['/Applications/Wolfram.app/Contents/MacOS/WolframKernel','-script',str(target)],cwd=ROOT,stdout=log,stderr=subprocess.STDOUT,env=dict(os.environ,HEPCAT_COLOR_SHADOW_INDEX=str(index),HEPCAT_COLOR_SHADOW_CONTEXT=ctx),start_new_session=True)
  try:code=proc.wait(timeout=300 if index==0 else 1260)
  except subprocess.TimeoutExpired:os.killpg(proc.pid,signal.SIGTERM);proc.wait();code=124
 guard();summary=load(out/'result.json')if(out/'result.json').is_file()else{}
 row={'Index':index,'Kind':'EqualMassNativeProbe'if index==0 else'ActualPublicDefaultFullShadow','ExitCode':code,'Seconds':time.monotonic()-start,'Summary':summary,'InstalledSourceModelsRuntimeAndShadowUnchanged':True};records.append(row);(D/'public-controller-summary.json').write_text(json.dumps({'Records':records,'ProspectiveShadowContextSHA256':ctx,'InstalledProductionChanged':False,'CanonicalWritten':False},indent=2)+'\n');print(json.dumps(row),flush=True)
 if code or summary.get('Status')not in('ALL_PASS','COMPLETE'):raise SystemExit(code or 2)
