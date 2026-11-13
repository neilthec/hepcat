from pathlib import Path
import datetime,hashlib,json,re
ROOT=Path('/Users/neil/code/hepcat');R=ROOT/'tests/results/SM-5-point';D=R/'diagnostics/C7-spectator-cohort-20261113'
load=lambda p:json.loads(p.read_text());sha=lambda p:hashlib.sha256(p.read_bytes()).hexdigest()
cohort=load(D/'cohort.json');progress=load(D/'progress.json');ledger=load(D/'ledger-refresh-audit.json');ctx=cohort['ContextSHA256']
assert progress['Status']=='PASS'and ledger['Status']=='COMPLETE'
assert progress['Counts']=={'COMPLETE':29,'PASS':24,'SKIPPED_VALIDATED':1,'FAIL':0,'TIMEOUT':0,'ERROR':0}
assert ledger['Refreshes'][-1]['Counts']['Constructed']==245 and ledger['Refreshes'][-1]['Counts']['ValidatedIncludingPrecisionQualifiedCases']==245
assert ledger['Refreshes'][-1]['Counts']['FullyValidatedCases']==237 and ledger['Refreshes'][-1]['Counts']['PrecisionQualifiedCases']==8
cases=[];families=[];all_paths=[];remaining_paths=[]
for f in cohort['Families']:
 current=[]
 for cid in f['CaseIDs']:
  n=load(R/(cid+'.json'));assert n['Status']=='COMPLETE'and n['ContextSHA256']==ctx and n['CheckStatuses']=={'PASS':n['Rows']}and n['FeynmanRows']>=1
  assert sha(R/(cid+'.wxf'))==n['CapsuleSHA256']
  groups=re.findall(r'Validating color symmetry coefficient 1/1; parameter set ([123])/3',(R/(cid+'.live.txt')).read_text());assert groups==['1','2','3'],cid
  item={'ID':cid,'FamilyID':f['FamilyID'],'Particles':n['Particles'],'ContextSHA256':ctx,'Status':'COMPLETE','Rows':n['Rows'],'StatusCounts':n['CheckStatuses'],'FeynmanRows':n['FeynmanRows'],'ValidationGroupLiveMarkers':groups,'Seconds':n['Seconds'],'Complexity':n['Complexity'],'CapsuleSHA256':n['CapsuleSHA256'],'ImportedActualDefaultRun':n.get('ImportedActualDefaultRun',False)};cases.append(item);current.append(n)
 e=load(R/(f['FamilyID']+'-export.json'));assert e['Status']=='PASS'and e['CalcHEPStatus']==e['MadGraphStatus']=='PASS'and e['Points']==1000 and e['ContextSHA256']==ctx
 assert e['CaseIDs']==f['CaseIDs']and e['CaseCapsulesSHA256']=={n['ID']:n['CapsuleSHA256']for n in current};assert sha(R/(f['FamilyID']+'-export.wxf'))==e['CapsuleSHA256']
 families.append({'ID':f['FamilyID'],'ExportName':f['ExportName'],'Category':f['Category'],'CaseIDs':f['CaseIDs'],'Status':'PASS','CalcHEPStatus':'PASS','MadGraphStatus':'PASS','Points':1000,'ContextSHA256':ctx,'CapsuleSHA256':e['CapsuleSHA256']})
 selected=[]
 for stem in f['CaseIDs']+[f['FamilyID']+'-export']:
  selected.extend(p for p in R.glob(stem+'.*')if p.is_file()and p.suffix in {'.wxf','.json','.log','.txt'})
 all_paths+=selected
 if f['Category']not in {'MassiveNeutralScalarPair','MassiveNeutralVectorPair'}:remaining_paths+=selected
assert len(cases)==30 and len(families)==24
summary={'Status':'PASS','CreatedUTC':datetime.datetime.now(datetime.timezone.utc).isoformat(),'ContextSHA256':ctx,'Families':families,'Cases':cases,'FamilyCount':24,'NativeCaseCount':30,'OrdinaryConstructorRuns':29,'ImportedExactActualPublicDefaultRuns':1,'TopLevelNativePassRows':sum(c['Rows']for c in cases),'NormalParameterValidationGroups':90,'CalcHEPFamilyPasses':24,'MadGraphFamilyPasses':24,'PointsPerFamilyPerBackend':1000,'Failures':0,'FinalGlobalCounts':ledger['Refreshes'][-1]['Counts'],'FrozenSourceModelsRuntimeAndWorkflowUnchanged':True,'HistoricalC6RecordsRerunOrRelabeled':False,'CompiledBinariesIncluded':False,'CohortSeconds':progress['Seconds']}
summary_path=D/'summary.json';summary_path.write_text(json.dumps(summary,indent=2)+'\n')
for name,paths in [('all-artifacts',all_paths),('remaining-12-families-artifacts',remaining_paths)]:
 paths=sorted(set(paths+[D/'controller.py',D/'cohort.json',D/'controller.log',D/'progress.json',D/'lock-and-refresh.py',D/'lock-and-refresh.log',D/'ledger-refresh-audit.json',D/'execution-amendment.json',summary_path]))
 items=[{'Path':str(p.relative_to(ROOT)),'SHA256':sha(p),'Bytes':p.stat().st_size}for p in paths]
 manifest=D/(name+'-manifest.json');manifest.write_text(json.dumps({'ContextSHA256':ctx,'Files':items,'FileCount':len(items),'Bytes':sum(x['Bytes']for x in items),'ImmutableCompletedRecordsOnly':True,'CompiledBinariesIncluded':False},indent=2)+'\n')
 force=D/(name+'-force-add.txt');force.write_text('\n'.join([x['Path']for x in items]+[str(manifest.relative_to(ROOT)),str(force.relative_to(ROOT))])+'\n')
readme=ROOT/'tests/SM-5-point/README.md';text=readme.read_text();assert '## Recorded C7 coverage'not in text
text+='''\n\n## Recorded C7 coverage\n\nThe C7 checkpoint uses source context\n`4165f24c7d5501c40c03ea91f850a5cde77389a1236e3af397fb6d7e2344a167`.\nIt records **245 native-complete cases**: **237** with strict ordinary\nCalcHEP/MadGraph validation and **8** with the separately reviewed precision\nqualification. The latter retain their raw comparison results. This is partial\ncoverage of the 1,422-case inventory, not a claim that every SM five-point\namplitude has been completed.\n\nC7 adds 24 quark families, containing 30 explicit amplitudes: six quark-pair\nZhh families, six quark-pair ZZh families, six photon/hh families with both\nphoton helicities constructed independently, and six charged-vector/hh\nfamilies. Every new case uses the current source, retains all three normal\nparameter validation groups and passes physical residues, automatic spurious\npoles, high energy, reference independence and the independent full Feynman\ncomparison. Each family also passes 1,000 phase-space points against both\nCalcHEP and MadGraph in the normal canonical export.\n\nThere were 29 fresh ordinary worker constructions and one exact adoption of\nan already completed actual public default uUZhh call. The adoption preserves\nits expression, reports, prints and original timing; no constructor rerun is\nclaimed. Earlier C6 records remain historical evidence admitted through the\nreviewed finite compatibility chain. They are not relabeled as fresh C7 runs.\nThe frozen cohort summary and per-record hashes are in\n`tests/results/SM-5-point/diagnostics/C7-spectator-cohort-20261113/summary.json`.\n'''
readme.write_text(text)
print(json.dumps({'Status':'PASS','Cases':30,'Families':24,'GlobalCounts':summary['FinalGlobalCounts'],'SummarySHA256':sha(summary_path),'SourceContext':ctx},indent=2))
