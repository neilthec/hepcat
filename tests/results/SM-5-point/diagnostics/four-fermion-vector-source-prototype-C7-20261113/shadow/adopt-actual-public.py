"""Import the two reviewed actual public-call capsules without changing a WXF byte."""
from pathlib import Path
import datetime,fcntl,hashlib,importlib.util,json,os,shutil,uuid
HERE=Path(__file__).resolve().parent;ROOT=HERE.parents[5];RESULTS=ROOT/'tests/results/SM-5-point';TOOLS=ROOT/'tests/SM-5-point';CONTEXT='48d82ad2adaf29f9a338c9fb6b9cf3da37ed7570f6de5852ee50c798fd0d8363'
sha=lambda p:hashlib.sha256(p.read_bytes()).hexdigest()
def read(p):return json.loads(p.read_text())
def write(p,v):
 q=p.with_name(p.name+'.'+uuid.uuid4().hex+'.tmp');q.write_text(json.dumps(v,indent=2)+'\n');os.replace(q,p)
def module(name,p):
 s=importlib.util.spec_from_file_location(name,p);v=importlib.util.module_from_spec(s);s.loader.exec_module(v);return v
w=module('public4fv_adopt_run',TOOLS/'run.py');v=module('public4fv_adopt_context',TOOLS/'validation_context_c8.py');cfg=read(RESULTS/'configuration.json');assert cfg['ContextSHA256']==w.context_sha256(cfg)==CONTEXT
proof=w.context_compatibility(cfg,CONTEXT);assert proof and proof.get('_C8VerifiedCurrentContext')is True
post=RESULTS/'compatibility/constructor-C8-postpatch-48d82ad2adaf29f9a338c9fb6b9cf3da37ed7570f6de5852ee50c798fd0d8363-20261113';review=read(post/'root-code-review.json');assert sha(post/'root-code-review.json')=='f15845a4552a6e9a3f4212323ebcb2a36691b2d721e09d327973e8b5c7666fb0'
bound={r['Path']:r['SHA256']for r in review['EvidenceFiles'].values()};audit=read(HERE/'public-native-audit.json');assert audit['Status']=='PASS'and sha(HERE/'public-native-audit.json')==bound[str((HERE/'public-native-audit.json').relative_to(ROOT))]
inv=read(TOOLS/'inventory.json');before=w.stage_context.manifest(ROOT,TOOLS,cfg);current_hashes={};stamp=datetime.datetime.now(datetime.timezone.utc).strftime('%Y%m%dT%H%M%SZ');records=[]
with(RESULTS/'run.lock').open('a')as lock:
 fcntl.flock(lock,fcntl.LOCK_EX|fcntl.LOCK_NB)
 for name,groups in [('distinct-z',4),('identical-z',3)]:
  source=HERE/(name+'-public');summary=read(source/'summary.json');controller=read(source/'controller.json');meta=next(x for x in audit['Records']if x['Case']==name);family=next(f for f in inv['Families']if f['ID']==summary['FamilyID']);assert len(family['CaseIDs'])==1;cid=family['CaseIDs'][0];case=next(c for c in inv['Cases']if c['ID']==cid)
  assert summary['Status']=='COMPLETE'and controller['Status']=='PASS'and controller['InputsUnchanged']is True
  assert summary['ShadowSourceContextSHA256']==CONTEXT and summary['AllOtherOptionsDefault']is True and summary['Particles']==case['Particles']
  assert read(source/'shadow-runtime-manifest.json')==before
  assert meta['RepresentativeSelected']is True and meta['ValidationParameterGroups']==groups and meta['BenchmarkUsedForConstruction']is False
  assert all(g['Statuses']=={'PASS':g['Rows']}and g['FeynmanStatus']=='PASS'and g['FeynmanCompletedSamples']==4 for g in meta['Groups'])
  cap=source/'result.wxf';capsha=sha(cap);assert capsha==meta['ResultCapsuleSHA256']==bound[str(cap.relative_to(ROOT))]
  target=RESULTS/(cid+'.wxf');archived=[]
  for suffix in('.wxf','.json','.log','.live.txt'):
   old=RESULTS/(cid+suffix)
   if old.exists():
    archive=HERE/'pre-adoption-history'/stamp/old.name;archive.parent.mkdir(parents=True,exist_ok=True);shutil.copy2(old,archive);assert sha(old)==sha(archive);archived.append({'Original':str(old.relative_to(ROOT)),'Archive':str(archive.relative_to(ROOT)),'SHA256':sha(old)})
  tmp=target.with_name(target.name+'.'+uuid.uuid4().hex+'.tmp');shutil.copyfile(cap,tmp);assert sha(tmp)==capsha;os.replace(tmp,target);assert sha(target)==capsha
  for srcname,suffix in [('worker.log','.log'),('worker.live.txt','.live.txt')]:
   dest=RESULTS/(cid+suffix);temp=dest.with_name(dest.name+'.'+uuid.uuid4().hex+'.tmp');shutil.copyfile(source/srcname,temp);os.replace(temp,dest)
  native={'ID':cid,'Particles':case['Particles'],'Status':'COMPLETE','Seconds':summary['Seconds'],'Complexity':summary['Complexity'],'RandomSeed':meta['AutomaticSeed'],'Points':cfg['Points'],'ContextSHA256':CONTEXT,'CapsuleSHA256':capsha,'Rows':summary['Rows'],'FeynmanRows':summary['FeynmanRows'],'CheckStatuses':summary['RowStatuses'],'ActualPublicCall':summary['PublicCall'],'RequestedOptions':{},'ImportedActualPublicShadowRun':True,'OriginalPublicCapsule':str(cap.relative_to(ROOT)),'OriginalPublicCapsuleSHA256':capsha,'PublicCapsuleBytesUnchanged':True,'OriginalPublicSourceContextSHA256':CONTEXT,'OriginalPublicSourceManifest':str((source/'shadow-runtime-manifest.json').relative_to(ROOT)),'OriginalPublicSourceManifestSHA256':sha(source/'shadow-runtime-manifest.json'),'CapturedCapsuleMetadataDescribesOriginalExecution':True,'NativeValidationRerunOnImport':False,'ConstructorRerunOnImport':False,'IndependentBackendsStillRequiredForThisActualOutput':True,'ImportScriptSHA256':sha(Path(__file__))}
  write(RESULTS/(cid+'.json'),native);checked=w.checked_summary(cid,'',CONTEXT,proof);assert checked['Status']=='COMPLETE';records.append({'CaseID':cid,'FamilyID':family['ID'],'Source':str(cap.relative_to(ROOT)),'Canonical':str(target.relative_to(ROOT)),'SourceSHA256':capsha,'CanonicalSHA256':sha(target),'ByteExact':True,'SummarySHA256':sha(RESULTS/(cid+'.json')),'OriginalArchivedFiles':archived,'NativeGroups':groups,'BackendRerun':False})
 assert w.stage_context.manifest(ROOT,TOOLS,read(RESULTS/'configuration.json'))==before
record={'Status':'TWO_BYTE_EXACT_PUBLIC_CAPSULES_IMPORTED','ContextSHA256':CONTEXT,'Records':records,'NoExpressionOrReportRewrite':True,'NoConstructorOrNativeValidationRerunOnImport':True,'SourceModelRuntimeUnchanged':True,'SharedProgressOrNotebookWritten':False}
write(HERE/'canonical-import-audit.json',record);print(json.dumps(record,indent=2))
