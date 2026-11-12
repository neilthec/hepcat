from pathlib import Path
import datetime,hashlib,importlib.util,json,re,shutil
root=Path('/Users/neil/code/hepcat');here=root/'tests/SM-5-point';results=root/'tests/results/SM-5-point'
d=results/'compatibility/constructor-prepatch-370fa1eabb7d41f56e45477db581572ca0d96bed2f4b2c98106c61eb5237706a-20261112T211458Z'
spec=importlib.util.spec_from_file_location('sm5_constructor_baseline_stage',here/'stage_context.py');stage=importlib.util.module_from_spec(spec);spec.loader.exec_module(stage)
config=json.loads((results/'configuration.json').read_text());manifest=stage.manifest(root,here,config);context=stage.fingerprint(manifest)
expected='370fa1eabb7d41f56e45477db581572ca0d96bed2f4b2c98106c61eb5237706a';assert context==expected,(context,expected)
def sha(p):return hashlib.sha256(p.read_bytes()).hexdigest()
def write(p,v):p.write_text(json.dumps(v,indent=2,sort_keys=True)+'\n')
assert not (d/'baseline.json').exists()
write(d/'manifest.json',manifest);write(d/'configuration.json',config)
shutil.copyfile(here/'inventory.json',d/'inventory.json');shutil.copyfile(here/'stage_context.py',d/'stage_context.py')
snapshot={};out=d/'summaries';out.mkdir(exist_ok=True)
paths=[p for p in sorted(results.glob('*.json')) if re.fullmatch(r'(?:sm5_[0-9a-f]{12}|sm5f_[0-9a-f]{12}-export)\.json',p.name)]
invalid=[]
for path in paths:
 raw=path.read_bytes();shutil.copyfile(path,out/path.name)
 try:summary=json.loads(raw)
 except (ValueError,UnicodeError):summary={};invalid.append(path.name)
 capsule=path.with_suffix('.wxf');entry={'SummarySHA256':sha(path),'RecordedContextSHA256':summary.get('ContextSHA256'),'Status':summary.get('Status'),'ID':summary.get('ID'),'FamilyID':summary.get('FamilyID'),'CapsuleExists':capsule.is_file(),'DeclaredCapsuleSHA256':summary.get('CapsuleSHA256'),'SummaryValidJSON':bool(summary)}
 if capsule.is_file():
  entry['CapsuleSHA256']=sha(capsule);entry['DeclaredCapsuleMatches']=entry['CapsuleSHA256']==entry['DeclaredCapsuleSHA256'];entry['CapsuleBytes']=capsule.stat().st_size
 snapshot[path.name]=entry
write(d/'checkpoint-snapshot.json',snapshot)
for name in manifest['Files']:
 if name.startswith('source/') and Path(name).suffix=='.wl':
  dest=d/name;dest.parent.mkdir(parents=True,exist_ok=True);shutil.copyfile(root/name,dest);assert sha(dest)==manifest['Files'][name]
bridge=json.loads((here/'exporter-context-bridge.json').read_text());shutil.copyfile(here/'exporter-context-bridge.json',d/'existing-exporter-context-bridge.json')
bridge_archive={}
for key in ('BeforeManifest','AfterManifest','CheckpointSnapshot'):
 p=root/bridge[key];dest=d/('existing-exporter-'+key+'.json');shutil.copyfile(p,dest);bridge_archive[key]={'OriginalPath':bridge[key],'ArchiveFile':dest.name,'SHA256':sha(dest)}
assert stage.fingerprint(stage.manifest(root,here,config))==expected
for name,entry in snapshot.items():
 assert sha(results/name)==entry['SummarySHA256'],name
 if entry['CapsuleExists']:assert sha((results/name).with_suffix('.wxf'))==entry['CapsuleSHA256'],name
record={'Kind':'FrozenPreConstructorChangeEvidence','Status':'FILES_AND_CHECKPOINTS_CAPTURED_DEFINITIONS_PENDING','CreatedUTC':datetime.datetime.now(datetime.timezone.utc).isoformat(),'ContextSHA256':context,'ManifestSHA256':sha(d/'manifest.json'),'ManifestFiles':len(manifest['Files']),'CheckpointSnapshotSHA256':sha(d/'checkpoint-snapshot.json'),'SummaryCount':len(snapshot),'CapsuleCount':sum(v['CapsuleExists'] for v in snapshot.values()),'InvalidJSONSummaries':invalid,'DeclaredCapsuleMismatches':[k for k,v in snapshot.items() if v.get('DeclaredCapsuleMatches') is False],'ExistingExporterBridgeSHA256':sha(d/'existing-exporter-context-bridge.json'),'ExistingExporterBridgeArchives':bridge_archive,'ConfigurationSHA256':sha(d/'configuration.json'),'InventorySHA256':sha(d/'inventory.json'),'ValidationRerunPerformed':False,'ConstructorRerunPerformed':False,'AdmissionPolicyChanged':False,'CanonicalArtifactsChanged':False,'CheckpointSetVerifiedStableBeforeAfter':True,'UnverifiedEntriesIneligibleForCompatibility':True}
write(d/'baseline.json',record);shutil.copyfile(Path(__file__),d/'capture-baseline.py')
Path('/private/tmp/hepcat-current-constructor-baseline-path').write_text(str(d)+'\n')
print(json.dumps({'Directory':str(d),**record},indent=2))
