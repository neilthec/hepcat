from pathlib import Path
import datetime,hashlib,importlib.util,json,os,signal,shutil,subprocess,time
root=Path('/Users/neil/code/hepcat');here=root/'tests/SM-5-point'
d=Path('/private/tmp/hepcat-current-constructor-baseline-path').read_text().strip();d=Path(d)
shutil.copyfile('/private/tmp/hepcat-cold-definition-fingerprint.wl',d/'fingerprint-definitions.wl')
shutil.copyfile(__file__,d/'capture-definitions.py')
spec=importlib.util.spec_from_file_location('sm5_fingerprint_context',here/'stage_context.py');stage=importlib.util.module_from_spec(spec);spec.loader.exec_module(stage)
config=json.loads((d/'configuration.json').read_text());expected=json.loads((d/'baseline.json').read_text())['ContextSHA256']
assert stage.fingerprint(stage.manifest(root,here,config))==expected
records=[]
for index in (1,2):
 assert stage.fingerprint(stage.manifest(root,here,config))==expected
 start=time.monotonic();status={'Pass':index,'StartedUTC':datetime.datetime.now(datetime.timezone.utc).isoformat(),'ExternalSeconds':120,'PackageLoadedOnce':True,'PhysicsCalculationsPerformed':False}
 with (d/f'definitions-{index}.log').open('x') as output:
  process=subprocess.Popen(['/Applications/Wolfram.app/Contents/MacOS/WolframKernel','-script',str(d/'fingerprint-definitions.wl')],cwd=root,stdout=output,stderr=subprocess.STDOUT,start_new_session=True,env=dict(os.environ,HEPCAT_FINGERPRINT_DIRECTORY=str(d),HEPCAT_FINGERPRINT_PASS=str(index),OMP_NUM_THREADS='1',OPENBLAS_NUM_THREADS='1'))
  try:status['ExitCode']=process.wait(timeout=120)
  except subprocess.TimeoutExpired:
   os.killpg(process.pid,signal.SIGTERM)
   try:process.wait(timeout=5)
   except subprocess.TimeoutExpired:os.killpg(process.pid,signal.SIGKILL);process.wait()
   status['Timeout']=True
 status['Seconds']=time.monotonic()-start;status['ContextUnchanged']=stage.fingerprint(stage.manifest(root,here,config))==expected
 path=d/f'definitions-{index}-raw.json';status['OutputExists']=path.is_file()
 if path.is_file():status['RawSHA256']=hashlib.sha256(path.read_bytes()).hexdigest();status['RawBytes']=path.stat().st_size
 records.append(status);(d/'definition-capture-status.json').write_text(json.dumps(records,indent=2)+'\n')
 print(json.dumps(status),flush=True)
 assert status['ExitCode']==0 and status['ContextUnchanged'] and status['OutputExists']
