"""Actual default public C4 pilot in a diagnostic directory; no canonical writes."""
from pathlib import Path
import argparse,hashlib,importlib.util,json,os,signal,subprocess,time
ROOT=Path('/Users/neil/code/hepcat');HERE=Path(__file__).resolve().parent
EXPECTED='a91edec8580c2721ccbe8d5304442e9e506f9fd89dc911c250a550c31723a238'
spec=importlib.util.spec_from_file_location('c4_default_stage_guard',ROOT/'tests/SM-5-point/stage_context.py');stage=importlib.util.module_from_spec(spec);spec.loader.exec_module(stage)
configuration=json.loads((ROOT/'tests/results/SM-5-point/configuration.json').read_text())
def context():return stage.fingerprint(stage.manifest(ROOT,ROOT/'tests/SM-5-point',configuration))
def digest(p):return hashlib.sha256(p.read_bytes()).hexdigest()
p=argparse.ArgumentParser(description=__doc__);p.add_argument('kind',choices=('construct','export'));args=p.parse_args()
assert context()==EXPECTED
script=HERE/(args.kind+'.wl');loaded=digest(script);started=time.monotonic();limit=2400 if args.kind=='construct' else 1200
log=HERE/(args.kind+'.log');assert not log.exists(),'Preserve prior actual pilot.'
with log.open('x')as out:
 proc=subprocess.Popen(['/Applications/Wolfram.app/Contents/MacOS/WolframKernel','-script',str(script)],cwd=ROOT,stdout=out,stderr=subprocess.STDOUT,start_new_session=True,env=dict(os.environ,OMP_NUM_THREADS='1',OPENBLAS_NUM_THREADS='1'))
 try:code=proc.wait(timeout=limit)
 except subprocess.TimeoutExpired:
  os.killpg(proc.pid,signal.SIGTERM)
  try:proc.wait(timeout=5)
  except subprocess.TimeoutExpired:os.killpg(proc.pid,signal.SIGKILL);proc.wait()
  code=124
assert context()==EXPECTED and digest(script)==loaded
record={'Kind':args.kind,'ExitCode':code,'Seconds':time.monotonic()-started,'ExternalDeadline':limit,'SourceContextSHA256':EXPECTED,'SourceContextUnchanged':True,'LoadedScriptSHA256':loaded,'CanonicalRecordsModified':False}
(HERE/(args.kind+'-controller.json')).write_text(json.dumps(record,indent=2)+'\n');print(record,flush=True)
raise SystemExit(code)
