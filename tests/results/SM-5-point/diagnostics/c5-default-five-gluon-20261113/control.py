from pathlib import Path
import hashlib,importlib.util,json,os,signal,subprocess,time
ROOT=Path('/Users/neil/code/hepcat');HERE=Path(__file__).resolve().parent
spec=importlib.util.spec_from_file_location('c5_color_stage',ROOT/'tests/SM-5-point/stage_context.py');stage=importlib.util.module_from_spec(spec);spec.loader.exec_module(stage)
configuration=json.loads((ROOT/'tests/results/SM-5-point/configuration.json').read_text())
manifest=stage.manifest(ROOT,ROOT/'tests/SM-5-point',configuration);expected=stage.fingerprint(manifest)
assert expected==os.environ['HEPCAT_C5_CONTEXT']
(HERE/'source-runtime-manifest.json').write_text(json.dumps(manifest,indent=2,sort_keys=True)+'\n')
sha=lambda p:hashlib.sha256(p.read_bytes()).hexdigest();loaded=sha(HERE/'construct.wl');began=time.monotonic()
with(HERE/'construct.log').open('x')as out:
 p=subprocess.Popen(['/Applications/Wolfram.app/Contents/MacOS/WolframKernel','-script',str(HERE/'construct.wl')],cwd=ROOT,stdout=out,stderr=subprocess.STDOUT,start_new_session=True,env=dict(os.environ,OMP_NUM_THREADS='1',OPENBLAS_NUM_THREADS='1'))
 try:code=p.wait(timeout=960)
 except subprocess.TimeoutExpired:
  os.killpg(p.pid,signal.SIGTERM)
  try:p.wait(timeout=5)
  except subprocess.TimeoutExpired:os.killpg(p.pid,signal.SIGKILL);p.wait()
  code=124
same=stage.fingerprint(stage.manifest(ROOT,ROOT/'tests/SM-5-point',configuration))==expected and sha(HERE/'construct.wl')==loaded
(HERE/'construct-controller.json').write_text(json.dumps({'ExitCode':code,'Seconds':time.monotonic()-began,'ContextSHA256':expected,'SourceAndScriptUnchanged':same,'LoadedScriptSHA256':loaded,'CanonicalRecordsModified':False,'CallType':'ActualPublicDefaultC5'},indent=2)+'\n')
assert same
print('ACTUAL_DEFAULT_C5_FIVE_GLUON',code,time.monotonic()-began);raise SystemExit(code)
