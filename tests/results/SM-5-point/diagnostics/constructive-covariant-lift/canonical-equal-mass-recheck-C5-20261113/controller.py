import hashlib,importlib.util,json,os,signal,subprocess,time
from pathlib import Path
ROOT=Path('/Users/neil/code/hepcat');HERE=ROOT/'tests/SM-5-point';RESULTS=ROOT/'tests/results/SM-5-point';D=Path(__file__).resolve().parent
CONTEXT='b672a8945605ad07e3f24e47a783f071a54c9b057e44b65048267ec057d55afd'
def sha(p):return hashlib.sha256(p.read_bytes()).hexdigest()
spec=importlib.util.spec_from_file_location('sm5_c5_equalmass_guard',HERE/'run.py');run=importlib.util.module_from_spec(spec);spec.loader.exec_module(run)
cfg=run.read_json(RESULTS/'configuration.json');manifest=run.stage_context.manifest(ROOT,HERE,cfg);assert run.stage_context.fingerprint(manifest)==CONTEXT
source=RESULTS/'sm5_46dff720d04e.wxf';assert sha(source)=='33b43a0075b17bd081d1fa8123ca09007e1cf3becef9c861c6771069c5b1afde'
guards={str(p):sha(p) for p in (source,D/'canonical-Mm-override.wl',D/'fresh-feynman-conversion.wl',RESULTS/'configuration.json',HERE/'supplemental-probes.json')}
started=time.monotonic()
with (D/'check.log').open('w') as output:
 process=subprocess.Popen(['/Applications/Wolfram.app/Contents/MacOS/WolframKernel','-script',str(D/'canonical-Mm-override.wl')],cwd=ROOT,env=dict(os.environ,OMP_NUM_THREADS='1',OPENBLAS_NUM_THREADS='1'),stdout=output,stderr=subprocess.STDOUT,start_new_session=True)
 try:code=process.wait(timeout=240)
 except subprocess.TimeoutExpired:
  os.killpg(process.pid,signal.SIGTERM)
  try:process.wait(timeout=5)
  except subprocess.TimeoutExpired:os.killpg(process.pid,signal.SIGKILL);process.wait()
  code=124
assert run.stage_context.manifest(ROOT,HERE,run.read_json(RESULTS/'configuration.json'))==manifest
assert all(sha(Path(name))==value for name,value in guards.items())
summary=run.read_json(D/'canonical-Mm-override.json',{})
record={'Status':'PASS' if code==0 and summary.get('Status')=='ALL_PASS' and summary.get('StatusCounts')=={'PASS':13} else 'NOT_PASS','ExitCode':code,'Seconds':time.monotonic()-started,'ContextSHA256':CONTEXT,'SourceCapsuleSHA256':guards[str(source)],'AllSourceModelRuntimeFilesUnchanged':True,'GuardedFilesUnchanged':True,'GuardedSHA256':guards,'OriginalSourceNativeContext':summary.get('SourceNativeContextSHA256'),'ActualStatusCounts':summary.get('StatusCounts'),'SourceAmplitudeUnchanged':summary.get('SourceAmplitudeUnchanged'),'NoExpressionMassSlotOrFlavorTransport':summary.get('NoExpressionMassSlotOrFlavorTransport'),'RegistryWritten':False,'SharedProgressWritten':False,'ConfigurationWritten':False}
if (D/'canonical-Mm-override.wxf').exists():record['ProbeCapsuleSHA256']=sha(D/'canonical-Mm-override.wxf')
if (D/'canonical-Mm-override.json').exists():record['ProbeSummarySHA256']=sha(D/'canonical-Mm-override.json')
run.atomic_json(D/'controller-summary.json',record);print(json.dumps(record,indent=2,sort_keys=True));raise SystemExit(0 if record['Status']=='PASS' else 1)
