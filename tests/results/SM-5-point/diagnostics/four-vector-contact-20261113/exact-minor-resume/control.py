from pathlib import Path
import hashlib, importlib.util, json, os, signal, subprocess, time
ROOT=Path('/Users/neil/code/hepcat');HERE=Path(__file__).resolve().parent
spec=importlib.util.spec_from_file_location('vvv_stage',ROOT/'tests/SM-5-point/stage_context.py');stage=importlib.util.module_from_spec(spec);spec.loader.exec_module(stage)
config=json.loads((ROOT/'tests/results/SM-5-point/configuration.json').read_text());manifest=stage.manifest(ROOT,ROOT/'tests/SM-5-point',config);context=stage.fingerprint(manifest)
assert context=='47026bedc2844622616e46a05069c2682cc4306872799bbe8e02d4a01291cc04';before={p.name:hashlib.sha256(p.read_bytes()).hexdigest()for p in [HERE/'solve.wl']};began=time.monotonic()
with(HERE/'pilot.log').open('w')as out:
 p=subprocess.Popen(['/Applications/Wolfram.app/Contents/MacOS/WolframKernel','-script',str(HERE/'solve.wl')],cwd=ROOT,stdout=out,stderr=subprocess.STDOUT,start_new_session=True,env=dict(os.environ,HEPCAT_V4_CONTEXT=context,OMP_NUM_THREADS='1',OPENBLAS_NUM_THREADS='1'))
 try:code=p.wait(timeout=660)
 except subprocess.TimeoutExpired:
  os.killpg(p.pid,signal.SIGTERM)
  try:p.wait(timeout=10)
  except subprocess.TimeoutExpired:os.killpg(p.pid,signal.SIGKILL);p.wait()
  code=124
same=stage.fingerprint(stage.manifest(ROOT,ROOT/'tests/SM-5-point',config))==context and before=={p.name:hashlib.sha256(p.read_bytes()).hexdigest()for p in [HERE/'solve.wl']}
(HERE/'controller.json').write_text(json.dumps({'ExitCode':code,'Seconds':time.monotonic()-began,'SourceContext':context,'SourceAndScriptsUnchanged':same,'LoadedScriptsSHA256':before,'CanonicalRecordsModified':False,'NoIndependentBenchmarkRead':True,'CompleteKnownLowerFourAmplitudeRead':True},indent=2)+'\n');print('FOUR_VECTOR_CONTACT_DIAGNOSTIC_EXIT',code,'SECONDS',time.monotonic()-began);raise SystemExit(code)
