from pathlib import Path
import hashlib,importlib.util,json,os,signal,subprocess,time
ROOT=Path('/Users/neil/code/hepcat');HERE=Path(__file__).resolve().parent
EXPECTED='a91edec8580c2721ccbe8d5304442e9e506f9fd89dc911c250a550c31723a238'
s=importlib.util.spec_from_file_location('orbit_amp_stage',ROOT/'tests/SM-5-point/stage_context.py');stage=importlib.util.module_from_spec(s);s.loader.exec_module(stage)
c=json.loads((ROOT/'tests/results/SM-5-point/configuration.json').read_text());fingerprint=lambda:stage.fingerprint(stage.manifest(ROOT,ROOT/'tests/SM-5-point',c))
assert fingerprint()==EXPECTED
bench=ROOT/'tests/results/SM-5-point/diagnostics/five-gluon-construction-C4/current-helicity-feynman-benchmark.wxf'
assert hashlib.sha256(bench.read_bytes()).hexdigest()=='9fd34adeb136b96c5d52aa09e65c374a3631f3992376f44477bf07f8993110de'
loaded={name:hashlib.sha256((HERE/name).read_bytes()).hexdigest()for name in ('pilot.wl','reconstruct.wl')};started=time.monotonic()
with(HERE/'pilot.log').open('x')as out:
 p=subprocess.Popen(['/Applications/Wolfram.app/Contents/MacOS/WolframKernel','-script',str(HERE/'pilot.wl')],cwd=ROOT,stdout=out,stderr=subprocess.STDOUT,start_new_session=True,env=dict(os.environ,OMP_NUM_THREADS='1',OPENBLAS_NUM_THREADS='1'))
 try:code=p.wait(timeout=2400)
 except subprocess.TimeoutExpired:
  os.killpg(p.pid,signal.SIGTERM)
  try:p.wait(timeout=5)
  except subprocess.TimeoutExpired:os.killpg(p.pid,signal.SIGKILL);p.wait()
  code=124
assert fingerprint()==EXPECTED and all(hashlib.sha256((HERE/n).read_bytes()).hexdigest()==v for n,v in loaded.items())
(HERE/'controller.json').write_text(json.dumps({'ExitCode':code,'Seconds':time.monotonic()-started,'SourceContext':EXPECTED,'SourceContextUnchanged':True,'LoadedScriptSHA256':loaded,'NoKnownAmplitudeInput':True,'CanonicalRecordsModified':False},indent=2)+'\n')
print('COLOR_ORBIT_AMPLITUDE_PILOT_EXIT',code,time.monotonic()-started)
raise SystemExit(code)
