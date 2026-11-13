"""Capture an explicit same-seed replay without changing validator mathematics."""
from pathlib import Path
import csv, hashlib, json, os, shutil, subprocess, time
ROOT=Path('/Users/neil/code/hepcat');HERE=Path(__file__).resolve().parent
PROJECT=HERE/'spinas-output/fivegluonfixed'; sha=lambda p:hashlib.sha256(p.read_bytes()).hexdigest()
original_reports=sorted(PROJECT.glob('madgraph-validation-report-*.json'));assert len(original_reports)==1
original=json.loads(original_reports[0].read_text());assert original['Status']=='PASS' and original['Rows'][0]['ComparedPoints']==1000
seed=original['Seed'];(HERE/'raw-madgraph-original.json').write_bytes(original_reports[0].read_bytes())
header=PROJECT/'spinas-validation.hpp';before=header.read_text();assert header.read_bytes().rstrip(b'\n')==(ROOT/'source/internal/export/spinas-validation.hpp').read_bytes().rstrip(b'\n')
(HERE/'spinas-validation-original.hpp').write_text(before)
paths=[PROJECT/n for n in ['process.cpp','process.hpp','spinas-validation.hpp','madgraph-runtime.hpp','madgraph-validation.cpp','manifest.json','build/fivegluonfixed_validate_madgraph']]
pretrace={'OriginalReportSHA256':sha(original_reports[0]),'Seed':seed,'HistoricalPerPointCSVExisted':False,'Files':[{'Path':str(p.relative_to(ROOT)),'SHA256':sha(p)}for p in paths]}
(HERE/'pretrace-project.json').write_text(json.dumps(pretrace,indent=2)+'\n')
trace='''    // Diagnostic-only I/O; no generation, evaluation or acceptance changes.
    if (const char* trace_path = std::getenv("HEPCAT_DIAGNOSTIC_POINT_CSV")) {
      std::ofstream trace(trace_path, std::ios::app);
      if (!trace) throw std::runtime_error("Could not open diagnostic point CSV.");
      trace << std::setprecision(std::numeric_limits<ldouble>::max_digits10)
            << options.seed << ',' << point_index + 1;
      for (const auto& momentum : point)
        for (const auto component : momentum) trace << ',' << component;
      trace << ',' << hepcat_squared << ',' << reference_squared
            << ',' << (point_error.empty() ? 0 : 1) << '\\n';
    }
'''
marker='    // Step 4: report progress without changing which points are tested.';assert before.count(marker)==1
instrumented=before.replace('#define HEPCAT_SPINAS_VALIDATION_HPP','#define HEPCAT_SPINAS_VALIDATION_HPP\n#include <fstream>\n#include <iomanip>\n#include <cstdlib>',1).replace(marker,trace+marker)
(HERE/'spinas-validation-trace.hpp').write_text(instrumented);header.write_text(instrumented)
cmake=shutil.which('cmake');assert cmake
began=time.monotonic()
with(HERE/'raw-replay.log').open('x')as out:
 try:
  subprocess.run([cmake,'--build',str(PROJECT/'build'),'--target','fivegluonfixed_validate_madgraph','-j2'],cwd=ROOT,stdout=out,stderr=subprocess.STDOUT,check=True,timeout=120)
  subprocess.run([str(PROJECT/'build/fivegluonfixed_validate_madgraph'),'--points','1000','--seed',str(seed),'--report',str(HERE/'raw-madgraph-replay.json')],cwd=ROOT,env=dict(os.environ,HEPCAT_DIAGNOSTIC_POINT_CSV=str(HERE/'raw-points-replay.csv')),stdout=out,stderr=subprocess.STDOUT,check=True,timeout=180)
 finally:
  header.write_text(before)
  subprocess.run([cmake,'--build',str(PROJECT/'build'),'--target','fivegluonfixed_validate_madgraph','-j2'],cwd=ROOT,stdout=out,stderr=subprocess.STDOUT,check=True,timeout=120)
replay=json.loads((HERE/'raw-madgraph-replay.json').read_text());rows=list(csv.reader((HERE/'raw-points-replay.csv').open()))
assert len(rows)==1000 and all(len(row)==25 and int(row[0])==seed and int(row[1])==i+1 and int(row[-1])==0 for i,row in enumerate(rows))
assert replay['Status']=='PASS' and replay['Seed']==seed
keys=['Helicities','Status','Passed','Points','RequestedPoints','GeneratedPoints','ComparedPoints','Mismatches','EvaluationErrors','MinimumCenterOfMassEnergy','MaximumCenterOfMassEnergy']
assert all(original['Rows'][0][key]==replay['Rows'][0][key]for key in keys)
summary={'Status':'RAW_REPLAY_PASS','Seed':seed,'Rows':1000,'OriginalReport':original,'ReplayReport':replay,'AllRowFieldsExactlyMatch':original['Rows']==replay['Rows'],'OriginalAndReplayCountsAndEnergyRangeExactlyMatch':True,'OriginalHeaderRestoredExactly':header.read_text()==before,'GeneratedProcessSourceSHA256':sha(PROJECT/'process.cpp'),'RawPointsSHA256':sha(HERE/'raw-points-replay.csv'),'TraceHeaderSHA256':sha(HERE/'spinas-validation-trace.hpp'),'OriginalHeaderSHA256':sha(HERE/'spinas-validation-original.hpp'),'Seconds':time.monotonic()-began,'HistoricalFirstRunHadPerPointCSV':False,'CaptureScope':'New same-seed replay of unchanged generated process/reference mathematics, with I/O-only trace inserted after comparison. No historical bit-identical-value claim; any recompile rounding differences remain visible in both raw reports.','RawCSVColumns':['Seed','PointIndex']+[f'p{i}_{j}'for i in range(1,6)for j in range(4)]+['HEPCAT','MadGraph','EvaluationError']}
(HERE/'raw-replay-summary.json').write_text(json.dumps(summary,indent=2,sort_keys=True)+'\n');print('C5_RAW_REPLAY_PASS',seed,len(rows),replay['Rows'][0]['MaximumRelativeError'],summary['AllRowFieldsExactlyMatch'])
