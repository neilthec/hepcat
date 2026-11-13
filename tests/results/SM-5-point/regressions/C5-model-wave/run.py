import hashlib,json,pathlib,re,subprocess,time
root=pathlib.Path('/Users/neil/code/hepcat');out=pathlib.Path(__file__).resolve().parent
suites=['factorization-fitted-point-tests.wl','construct-amplitude-tests.wl','completion-input-fit-tests.wl','amplitude-completion-tests.wl','construct-benchmark-recovery-tests.wl','retained-high-energy-validation-tests.wl','covariant-constructor-tests.wl','covariant-contact-calibration-tests.wl']
def manifest():return {str(p.relative_to(root)):hashlib.sha256(p.read_bytes()).hexdigest() for p in sorted((root/'source').rglob('*.wl'))}
def parse_counts(text):
 found=re.findall(r'CHECKS: (\d+); FAILURES: (\d+)',text)
 if not found:found=re.findall(r'(\d+) [^\n]*checks;\s*(\d+) failures\.',text,re.IGNORECASE)
 return (int(found[-1][0]),int(found[-1][1])) if found else (None,None)
records=[];started=time.monotonic();before=manifest()
if (out/'summary.json').exists():
 previous=json.loads((out/'summary.json').read_text());before=previous['SourceBefore']
 for record in previous['Suites']:
  text=(out/(record['Suite']+'.log')).read_text();checks,failures=parse_counts(text)
  record.update(Checks=checks,Failures=failures,LogSHA256=hashlib.sha256(text.encode()).hexdigest())
  record['Status']='PASS' if record['ExitCode']==0 and record['CompletionMarker'] and failures==0 else 'FAIL_OR_INCOMPLETE'
  if record['Status']!='PASS':raise SystemExit('Cannot resume an actual failed/incomplete suite')
  records.append(record)
for name in suites:
 if any(r['Suite']==name for r in records):continue
 record={'Suite':name,'SourceBefore':manifest()};tic=time.monotonic();print('BEGIN '+name,flush=True)
 with (out/(name+'.log')).open('w') as log:
  process=subprocess.run(['python3','tests/checks/run-wolfram-tests.py','--suite',name,'--timeout','600'],cwd=root,stdout=log,stderr=subprocess.STDOUT)
 record.update(ExitCode=process.returncode,Seconds=time.monotonic()-tic,SourceAfter=manifest())
 text=(out/(name+'.log')).read_text();checks,failures=parse_counts(text)
 record.update(Checks=checks,Failures=failures,LogSHA256=hashlib.sha256(text.encode()).hexdigest(),CompletionMarker=('HEPCAT_TEST_COMPLETED:'+name) in text.splitlines())
 record['Status']='PASS' if record['ExitCode']==0 and record['CompletionMarker'] and record['Failures']==0 else 'FAIL_OR_INCOMPLETE'
 records.append(record);(out/'summary.json').write_text(json.dumps({'Status':'IN_PROGRESS','Suites':records,'SourceBefore':before},indent=2)+'\n')
 print(json.dumps({k:record[k] for k in ['Suite','Status','Seconds','Checks','Failures','ExitCode']}),flush=True)
 if record['Status']!='PASS':break
summary={'Status':'PASS' if len(records)==len(suites) and all(r['Status']=='PASS' for r in records) else 'FAIL_OR_INCOMPLETE','Seconds':time.monotonic()-started,'Suites':records,'SourceBefore':before,'SourceAfter':manifest(),'ContextScope':'C5 development source, per-suite source hashes recorded; no historical validation context relabeling'}
(out/'summary.json').write_text(json.dumps(summary,indent=2)+'\n');print('WAVE '+summary['Status'],flush=True)
raise SystemExit(0 if summary['Status']=='PASS' else 1)
