"""Sequential ordinary C7 public construction and independent validation of metadata-selected spectator families."""
from pathlib import Path
from types import SimpleNamespace
import datetime,hashlib,importlib.util,json,shutil,sys,time
ROOT=Path('/Users/neil/code/hepcat');HERE=ROOT/'tests/SM-5-point';RESULTS=ROOT/'tests/results/SM-5-point';D=Path(__file__).resolve().parent
CTX='4165f24c7d5501c40c03ea91f850a5cde77389a1236e3af397fb6d7e2344a167'
spec=importlib.util.spec_from_file_location('c7_spectator_cohort_run',HERE/'run.py');run=importlib.util.module_from_spec(spec);spec.loader.exec_module(run)
sha=lambda p:hashlib.sha256(p.read_bytes()).hexdigest();load=lambda p:json.loads(p.read_text())
config_path=RESULTS/'configuration.json';config=load(config_path);manifest=run.stage_context.manifest(ROOT,HERE,config)
assert config['ContextSHA256']==run.stage_context.fingerprint(manifest)==CTX
scripts={str(p.relative_to(ROOT)):sha(p)for p in [HERE/'run.py',HERE/'evaluate.wl',HERE/'inventory.json',config_path,Path(__file__)]}
compat=run.context_compatibility(config,CTX);assert compat is not None
inventory=load(HERE/'inventory.json');metadata=inventory['Particles'];selected=[]
shapes={('A',1,'h',2):'PhotonScalarPair',('Z',1,'h',2):'MassiveNeutralScalarPair',('Z',2,'h',1):'MassiveNeutralVectorPair',('W+',1,'h',2):'ChargedScalarPair',('W-',1,'h',2):'ChargedScalarPair'}
for family in inventory['Families']:
 if family['FermionCount']!=2:continue
 fermions=[p for p in family['Particles']if p in metadata and metadata[p]['Spin2']%2]
 if len(fermions)!=2 or not all(metadata[p]['Spin2']==1 and metadata[p]['Mass']!='0'and metadata[p]['ColorDimension']==3 and metadata[p]['Chirality']==''for p in fermions):continue
 content=family['BosonContent'];category=None
 for shape,name in shapes.items():
  if content=={shape[0]:shape[1],shape[2]:shape[3]}:category=name
 if category is None:continue
 selected.append({'FamilyID':family['ID'],'CaseIDs':family['CaseIDs'],'ExportName':family['ExportName'],'Particles':family['Particles'],'Sector':family['Sector'],'Category':category})
assert len(selected)==24 and sum(len(f['CaseIDs'])for f in selected)==30
first={'sm5f_caa5ca0e3ee5':0,'sm5f_10a73b4e70cc':1};order={'MassiveNeutralScalarPair':2,'MassiveNeutralVectorPair':3,'PhotonScalarPair':4,'ChargedScalarPair':5}
selected.sort(key=lambda f:(first.get(f['FamilyID'],order[f['Category']]),f['ExportName']))
cohort={'ContextSHA256':CTX,'Selection':'Two massive Dirac fundamental fermions, three color-singlet bosons; exactly A+2S, Z+2S, 2Z+S, or charged-vector+2S declared by existing inventory. No multi-massless-vector or WZ/WW scalar class.','Families':selected,'FamilyCount':24,'NativeCaseCount':30,'Manifest':manifest,'WorkflowHashes':scripts,'PerCaseExternalSeconds':900,'PerExportExternalSeconds':600,'MemoryGiB':4,'PointsPerBackend':1000,'NoGlobalProgressNotebookOrConfigurationWrites':True}
run.atomic_json(D/'cohort.json',cohort)
args=SimpleNamespace(kernel='/Applications/Wolfram.app/Contents/MacOS/WolframKernel',timeout=900,export_timeout=600)
records=[];started=time.monotonic()
def guard():
 assert load(config_path)==config,'Configuration changed.'
 assert run.stage_context.manifest(ROOT,HERE,config)==manifest,'Source/model/runtime changed.'
 assert all(sha(ROOT/p)==h for p,h in scripts.items()),'Workflow changed.'
 assert shutil.disk_usage(ROOT).free>512*1024**2,'Low disk space; no artifacts deleted.'
def save(status):
 run.atomic_json(D/'progress.json',{'Status':status,'ContextSHA256':CTX,'Seconds':time.monotonic()-started,'Records':records,'Counts':{k:sum(r.get('Status')==k for r in records)for k in ['COMPLETE','PASS','SKIPPED_VALIDATED','FAIL','TIMEOUT','ERROR']},'NoGlobalProgressNotebookOrConfigurationWrites':True})
for family in selected:
 guard();fid=family['FamilyID'];existing=run.checked_summary(fid,'-export',CTX,compat)
 if existing and existing.get('Status')=='PASS':
  records.append({'Kind':'family','ID':fid,'Status':'SKIPPED_VALIDATED','ExistingCapsuleSHA256':existing['CapsuleSHA256']});save('RUNNING');continue
 for cid in family['CaseIDs']:
  guard();existing=run.checked_summary(cid,'',CTX,compat)
  if existing and existing.get('Status')=='COMPLETE':
   records.append({'Kind':'construct','ID':cid,'Status':'SKIPPED_VALIDATED','ExistingCapsuleSHA256':existing['CapsuleSHA256']});save('RUNNING');continue
  before=time.monotonic();row=run.run_worker(args,'construct',cid,config_path);guard()
  records.append({'Kind':'construct','ID':cid,'FamilyID':fid,'Status':row['Status'],'Seconds':time.monotonic()-before,'Summary':row});save('RUNNING')
  if row['Status']!='COMPLETE':save('STOPPED_ON_FAILURE');raise SystemExit(2)
 guard();before=time.monotonic();row=run.run_worker(args,'export',fid,config_path);guard()
 records.append({'Kind':'export','ID':fid,'Status':row['Status'],'Seconds':time.monotonic()-before,'Summary':row});save('RUNNING')
 if row['Status']!='PASS':save('STOPPED_ON_FAILURE');raise SystemExit(3)
save('PASS');print(json.dumps({'Status':'PASS','Records':len(records),'Seconds':time.monotonic()-started}),flush=True)
