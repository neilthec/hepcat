from pathlib import Path
import hashlib,importlib.util,json,os,signal,subprocess,sys,time
ROOT=Path('/Users/neil/code/hepcat');D=Path(__file__).resolve().parent;HERE=ROOT/'tests/SM-5-point';index=int(sys.argv[1]);assert index in (1,2)
sp=importlib.util.spec_from_file_location('colored_current_stage',HERE/'stage_context.py');stage=importlib.util.module_from_spec(sp);sp.loader.exec_module(stage)
c=json.loads((ROOT/'tests/results/SM-5-point/configuration.json').read_text());before=stage.manifest(ROOT,HERE,c);assert stage.fingerprint(before)=='48d82ad2adaf29f9a338c9fb6b9cf3da37ed7570f6de5852ee50c798fd0d8363'
sha=lambda p:hashlib.sha256(p.read_bytes()).hexdigest();scripts={p.name:sha(p)for p in [D/'source.wl',D/'pilot-C8.wl',Path(__file__)]}
out=D/f'pilot-{index}';out.mkdir(exist_ok=True);env=dict(os.environ,HEPCAT_COLORED_PILOT_INDEX=str(index),HEPCAT_COLORED_PILOT_SEED=str(161803399+index))
started=time.monotonic()
with (out/'kernel.log').open('w')as log:
 p=subprocess.Popen(['/Applications/Wolfram.app/Contents/MacOS/WolframKernel','-script',str(D/'pilot-C8.wl')],cwd=ROOT,stdout=log,stderr=subprocess.STDOUT,env=env,start_new_session=True)
 try:code=p.wait(timeout=600)
 except subprocess.TimeoutExpired:os.killpg(p.pid,signal.SIGTERM);p.wait();code=124
same=stage.manifest(ROOT,HERE,c)==before and all(sha(D/n)==h for n,h in scripts.items());assert same
summary={'Index':index,'ExitCode':code,'Seconds':time.monotonic()-started,'ProductionSourceModelsRuntimeAndScriptsUnchanged':same,'ProductionContextSHA256':stage.fingerprint(before),'DraftHashes':scripts,'CanonicalWritten':False,'Result':json.loads((out/'result.json').read_text())if(out/'result.json').exists()else None}
(out/'controller.json').write_text(json.dumps(summary,indent=2)+'\n');print(json.dumps(summary,indent=2));raise SystemExit(code)
