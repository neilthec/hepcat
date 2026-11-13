import hashlib,json,re,subprocess,time,os
from pathlib import Path
root=Path(__file__).resolve().parents[5]
out=Path(__file__).resolve().parent
prospective=root/'tests/results/SM-5-point/diagnostics/colored-four-fermion-current-source-20261113/production-candidate/prospective-manifest.json'
manifest=json.loads(prospective.read_text());expected=manifest['Manifest']['Files']
sha=lambda p:hashlib.sha256(p.read_bytes()).hexdigest()
suites=['color-current-source-tests.wl','parallel-source-signature-tests.wl','color-basis-tests.wl','color-reduction-tests.wl','color-orbit-construction-tests.wl','color-handedness-tests.wl','covariant-constructor-tests.wl','construct-amplitude-tests.wl','sm-factorization-model-tests.wl','qed-five-point-simplification-tests.wl','six-point-completion-tests.wl']
summary={'ContextSHA256':manifest['ProspectiveContextSHA256'],'SuiteOrder':suites,'Records':[],'Status':'RUNNING','OneKernelAtATime':True}
def guard():
 changed=[r for r,h in expected.items() if not (root/r).is_file() or sha(root/r)!=h]
 if changed:raise RuntimeError('Frozen source/model/runtime changed: '+repr(changed))
def save():
 p=out/'summary.json';t=p.with_suffix('.json.tmp');t.write_text(json.dumps(summary,indent=2)+'\n');os.replace(t,p)
guard();save();start=time.monotonic()
for suite in suites:
 guard();t=time.monotonic();source=root/'tests/checks'/suite
 log=out/(suite+'.log')
 env=dict(os.environ);env['HEPCAT_TEST_REPOSITORY']=str(root)
 with log.open('w') as stream:
  result=subprocess.run(['python3','tests/checks/run-wolfram-tests.py','--timeout','900','--suite',suite],cwd=root,env=env,stdout=stream,stderr=subprocess.STDOUT)
 guard();record={'Suite':suite,'ExitCode':result.returncode,'Seconds':time.monotonic()-t,'SuiteSHA256':sha(source),'LogSHA256':sha(log),'SourceModelsRuntimeUnchanged':True}
 summary['Records'].append(record);summary['ElapsedSeconds']=time.monotonic()-start;save();print(json.dumps(record),flush=True)
 if result.returncode:
  summary['Status']='STOPPED_ON_FAILURE';save();raise SystemExit(result.returncode)
summary['Status']='ALL_PASS';summary['ElapsedSeconds']=time.monotonic()-start;save()
