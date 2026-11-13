from pathlib import Path
from types import SimpleNamespace
import argparse,datetime,hashlib,importlib.util,json,os,shutil,subprocess,sys,time
ROOT=Path('/Users/neil/code/hepcat');HERE=ROOT/'tests/SM-5-point';RESULTS=ROOT/'tests/results/SM-5-point';D=RESULTS/'diagnostics/c6-default-five-gluon-20261113'
CONTEXT='47026bedc2844622616e46a05069c2682cc4306872799bbe8e02d4a01291cc04';ID='sm5_7e2e9f69dd9c';FAMILY='sm5f_3ca11bcd7f40';ORIGINAL='a343c8dfb8b721d787e5f0f5f0b0216b116701b2c30fff9baa35ea65aed6aa3f'
p=argparse.ArgumentParser();p.add_argument('--configuration',type=Path,required=True);args=p.parse_args()
spec=importlib.util.spec_from_file_location('c6_family_runner',HERE/'run.py');run=importlib.util.module_from_spec(spec);spec.loader.exec_module(run)
configuration=run.read_json(args.configuration);assert configuration['ContextSHA256']==CONTEXT and configuration['Points']>=1000
sha=lambda p:hashlib.sha256(p.read_bytes()).hexdigest();runner_sha=sha(HERE/'run.py');worker_sha=sha(HERE/'evaluate.wl')
def guard():
 assert run.context_sha256(configuration)==CONTEXT and sha(HERE/'run.py')==runner_sha and sha(HERE/'evaluate.wl')==worker_sha
 return run.context_compatibility(configuration,CONTEXT)
def checked(cid,suffix=''):return run.checked_summary(cid,suffix,CONTEXT,guard())
assert sha(D/'construction.wxf')==ORIGINAL
inventory=run.read_json(HERE/'inventory.json');family=next(f for f in inventory['Families']if f['ID']==FAMILY);members=[c for c in inventory['Cases']if c['FamilyID']==FAMILY]
assert len(members)==6 and next(c for c in members if c['ID']==ID)['Particles']==['G.+','G.+','G.-','G.-','G.-']
existing=checked(ID)
if not isinstance(existing,dict)or existing.get('Status')!='COMPLETE':
 stamp=datetime.datetime.now(datetime.timezone.utc).strftime('%Y%m%dT%H%M%S%f');history=RESULTS/'history';history.mkdir(exist_ok=True);archived=[]
 for ext in ['.json','.wxf','.log','.live.txt']:
  old=RESULTS/(ID+ext)
  if old.exists():
   dest=history/(ID+'-'+stamp+ext);record={'Original':str(old.relative_to(ROOT)),'Archived':str(dest.relative_to(ROOT)),'SHA256':sha(old)};old.replace(dest);archived.append(record)
 run.atomic_json(D/'canonical-import-history.json',{'Archived':archived,'ConstructorRerun':False,'SourceContext':CONTEXT})
 script=D/'import-canonical.wl';assert script.is_file()
 with(D/'canonical-import.log').open('w')as out:
  code=subprocess.run(['/Applications/Wolfram.app/Contents/MacOS/WolframKernel','-script',str(script)],cwd=ROOT,stdout=out,stderr=subprocess.STDOUT,timeout=120).returncode
 assert code==0
 for ext in ['.log','.live.txt']:
  origin=D/('construct'+ext)
  if origin.exists():shutil.copyfile(origin,RESULTS/(ID+ext))
 assert checked(ID)['Status']=='COMPLETE'
 print('ACTUAL_DEFAULT_C6_CANONICAL_ADOPTED',ID,flush=True)
worker_args=SimpleNamespace(kernel='/Applications/Wolfram.app/Contents/MacOS/WolframKernel',timeout=2400,export_timeout=1800)
records=[]
for cid in ['sm5_00d6634687d5','sm5_83fbb268bbf3']:
 guard();summary=checked(cid)
 if not isinstance(summary,dict)or summary.get('Status')!='COMPLETE':summary=run.run_worker(worker_args,'construct',cid,args.configuration)
 guard();records.append(summary);run.atomic_json(D/'remaining-family-controller.json',{'ContextSHA256':CONTEXT,'Records':records,'SharedLedgerModified':False})
 if summary.get('Status')!='COMPLETE':sys.exit(1)
assert all(checked(c['ID'])['Status']=='COMPLETE'for c in members)
summary=checked(FAMILY,'-export')
if not isinstance(summary,dict)or summary.get('Status')!='PASS':summary=run.run_worker(worker_args,'export',FAMILY,args.configuration)
guard();run.atomic_json(D/'family-export-controller.json',{'ContextSHA256':CONTEXT,'Result':summary,'SharedLedgerModified':False})
print('COMPLETE_FIVE_GLUON_FAMILY',summary.get('Status'),flush=True);sys.exit(0 if summary.get('Status')=='PASS'else 1)
