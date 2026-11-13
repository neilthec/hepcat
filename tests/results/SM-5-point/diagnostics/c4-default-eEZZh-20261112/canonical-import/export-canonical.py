"""One guarded ordinary canonical export; run only after the parent releases a slot."""
import argparse
import hashlib
import importlib.util
import json
from pathlib import Path
import time

ROOT=Path('/Users/neil/code/hepcat'); HERE=ROOT/'tests/SM-5-point'; RESULTS=ROOT/'tests/results/SM-5-point'
DIRECTORY=RESULTS/'diagnostics/c4-default-eEZZh-20261112/canonical-import'
CONTEXT='a91edec8580c2721ccbe8d5304442e9e506f9fd89dc911c250a550c31723a238'
CASE='sm5_f13b4237bc07'; FAMILY='sm5f_f13b4237bc07'
def sha(path):return hashlib.sha256(path.read_bytes()).hexdigest()
spec=importlib.util.spec_from_file_location('hepcat_c4_normal_export',HERE/'run.py');run=importlib.util.module_from_spec(spec);spec.loader.exec_module(run)
config=RESULTS/'configuration.json'; cfg=run.read_json(config)
assert cfg['ContextSHA256']==CONTEXT and run.context_sha256(cfg)==CONTEXT
assert cfg['Points']==1000 and cfg['MemoryGiB']==4
native=run.checked_summary(CASE,'',CONTEXT,run.context_compatibility(cfg,CONTEXT));assert native['Status']=='COMPLETE'
assert native['CapsuleSHA256']=='c3c00756aec6b3ede1470ffee10925c90628a7bdb8bf77d4fa77214d71cb2755'
guarded={str(p):sha(p) for p in (config,HERE/'evaluate.wl',HERE/'run.py',HERE/'stage_context.py',HERE/'constructor_context.py',HERE/'validation_context_c4.py',RESULTS/(CASE+'.wxf'),RESULTS/(CASE+'.json'))}
started=time.monotonic()
args=argparse.Namespace(kernel='/Applications/Wolfram.app/Contents/MacOS/WolframKernel',timeout=7200,export_timeout=1800)
summary=run.run_worker(args,'export',FAMILY,config)
assert run.context_sha256(run.read_json(config))==CONTEXT
assert all(sha(Path(p))==h for p,h in guarded.items())
audit={'Status':summary['Status'],'FamilyID':FAMILY,'ContextSHA256':CONTEXT,'Seconds':time.monotonic()-started,'PointsPerBackend':1000,'GuardedFilesUnchanged':True,'GuardedSHA256':guarded,'Summary':summary,'SummarySHA256':sha(RESULTS/(FAMILY+'-export.json')),'SourceExpressionUnchanged':True,'ConstructorRerunPerformed':False,'DiagnosticBackendExportReused':False,'SharedProgressWritten':False}
run.atomic_json(DIRECTORY/'canonical-export-audit.json',audit)
print(json.dumps(audit,indent=2,sort_keys=True))
raise SystemExit(0 if summary['Status']=='PASS' else 1)
