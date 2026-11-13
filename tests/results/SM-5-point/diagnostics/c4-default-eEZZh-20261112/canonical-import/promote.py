"""Archive a controlled stop and import one exact completed actual default run."""
import datetime as dt
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import shutil

ROOT=Path('/Users/neil/code/hepcat')
HERE=ROOT/'tests/SM-5-point'
RESULTS=ROOT/'tests/results/SM-5-point'
DIRECTORY=RESULTS/'diagnostics/c4-default-eEZZh-20261112'
IMPORT=DIRECTORY/'canonical-import'
ID='sm5_f13b4237bc07'
CONTEXT='a91edec8580c2721ccbe8d5304442e9e506f9fd89dc911c250a550c31723a238'
def sha(path):return hashlib.sha256(path.read_bytes()).hexdigest()
def load(path):return json.loads(path.read_text())
def atomic_copy(source,target):
 temporary=target.with_suffix(target.suffix+'.import.tmp')
 assert not temporary.exists()
 shutil.copyfile(source,temporary)
 assert sha(temporary)==sha(source)
 os.replace(temporary,target)
def atomic_json(path,value):
 temporary=path.with_suffix(path.suffix+'.tmp')
 temporary.write_text(json.dumps(value,indent=2,sort_keys=True)+'\n')
 os.replace(temporary,path)

spec=importlib.util.spec_from_file_location('hepcat_c4_canonical_import_run',HERE/'run.py')
run=importlib.util.module_from_spec(spec);spec.loader.exec_module(run)
configuration=load(RESULTS/'configuration.json')
manifest=run.stage_context.manifest(ROOT,HERE,configuration)
assert run.stage_context.fingerprint(manifest)==CONTEXT
preconditions=load(IMPORT/'preconditions.json')
prepared=load(IMPORT/'preparation-audit.json')
assert preconditions['Status']=='METADATA_PRECONDITIONS_PASS'
assert prepared['Status']=='VERIFIED_READY_FOR_CANONICAL_IMPORT'
assert prepared['ExactSourceFieldPreservation'] is True
assert prepared['ActualParameterGroupPassRows']==[13,13,13]
assert prepared['ConstructorRerunPerformed'] is False and prepared['CanonicalWritten'] is False
assert sha(DIRECTORY/'construction.wxf')==preconditions['SourceCapsuleSHA256']==prepared['SourceCapsuleSHA256']
assert sha(IMPORT/'canonical.wxf')==prepared['PreparedCapsuleSHA256']
assert sha(IMPORT/'canonical.json')==prepared['PreparedSummarySHA256']
summary=load(IMPORT/'canonical.json')
assert summary['ID']==ID and summary['Status']=='COMPLETE' and summary['ContextSHA256']==CONTEXT
assert summary['CapsuleSHA256']==prepared['PreparedCapsuleSHA256']
assert summary['RandomSeed']==6782590582154204676 and summary['Seconds']==228.841378
assert summary['Rows']==13 and summary['CheckStatuses']=={'PASS':13} and summary['FeynmanRows']==1
assert summary['Provenance']['FullParameterValidationRows']==39
assert summary['Provenance']['ImportedActualDefaultRun'] is True
old=load(RESULTS/(ID+'.json'))
assert old['Status']=='ABORTED' and old['StopKind']=='CONTROLLED_STOP'
assert not (RESULTS/(ID+'.wxf')).exists()
assert old['ContextSHA256']=='55701e7a9f6d674b7f37aa1ced637de0e9e1934badd54be9dce03244084725f9'
assert sha(HERE/'inventory.json')==preconditions['InventorySHA256']
for name,h in preconditions['DiagnosticScripts'].items():assert sha(DIRECTORY/name)==h
stamp=dt.datetime.now(dt.timezone.utc).strftime('%Y%m%dT%H%M%S%f')
history=RESULTS/'history';history.mkdir(exist_ok=True)
existing=[RESULTS/(ID+ext) for ext in ('.json','.wxf','.log','.live.txt') if (RESULTS/(ID+ext)).exists()]
archived={}
for path in existing:
 target=history/(ID+'-'+stamp+path.name[len(ID):])
 assert not target.exists()
 h=sha(path);path.replace(target);assert sha(target)==h
 archived[str(target.relative_to(ROOT))]={'SHA256':h,'Original':str(path.relative_to(ROOT))}
atomic_copy(IMPORT/'canonical.wxf',RESULTS/(ID+'.wxf'))
atomic_copy(DIRECTORY/'construct.log',RESULTS/(ID+'.log'))
atomic_copy(DIRECTORY/'construct.live.txt',RESULTS/(ID+'.live.txt'))
atomic_copy(IMPORT/'canonical.json',RESULTS/(ID+'.json'))
assert sha(RESULTS/(ID+'.wxf'))==summary['CapsuleSHA256']
assert load(RESULTS/(ID+'.json'))==summary
assert run.stage_context.manifest(ROOT,HERE,load(RESULTS/'configuration.json'))==manifest
accepted=run.checked_summary(ID,'',CONTEXT,run.context_compatibility(configuration,CONTEXT))
assert accepted['Status']=='COMPLETE'
audit={'Status':'IMPORTED_ACTUAL_DEFAULT_RUN','ID':ID,'ContextSHA256':CONTEXT,
 'CanonicalCapsule':str((RESULTS/(ID+'.wxf')).relative_to(ROOT)),
 'CanonicalCapsuleSHA256':sha(RESULTS/(ID+'.wxf')),
 'CanonicalSummarySHA256':sha(RESULTS/(ID+'.json')),
 'OriginalCapsule':str((DIRECTORY/'construction.wxf').relative_to(ROOT)),
 'OriginalCapsuleSHA256':sha(DIRECTORY/'construction.wxf'),
 'PreparationAuditSHA256':sha(IMPORT/'preparation-audit.json'),
 'ImportScriptSHA256':sha(Path(__file__)),
 'MetadataPreparationScriptSHA256':sha(IMPORT/'prepare.wl'),
 'PriorArchived':archived,'PreviousStatus':'ABORTED','PreviousStopKind':'CONTROLLED_STOP',
 'OriginalPrintsDetailsRowsAndExpressionPreserved':True,'OriginalRandomSeed':summary['RandomSeed'],
 'OriginalSeconds':summary['Seconds'],'FullParameterValidationGroups':3,
 'FullParameterValidationRows':39,'ImportedActualDefaultRun':True,
 'ConstructorRerunPerformed':False,'ExpressionUnchanged':True,
 'SharedProgressWritten':False,'ConfigurationWritten':False,'CanonicalBackendExportReused':False}
atomic_json(IMPORT/'promotion-audit.json',audit)
print(json.dumps(audit,indent=2,sort_keys=True))
