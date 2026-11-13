(* Isolated source candidate and independent native validation. No production edits. *)
Get["/Users/neil/code/hepcat/source/HEPCAT.wl"];
Begin["HEPCATC8ColoredCurrentPilot`Private`"];
$ContextPath=Prepend[DeleteCases[$ContextPath,"HEPCATC8ColoredCurrentPilot`Private`"],"HEPCATC8ColoredCurrentPilot`Private`"];
$HistoryLength=0;root="/Users/neil/code/hepcat";directory=DirectoryName[$InputFileName];
index=FromDigits[Environment["HEPCAT_COLORED_PILOT_INDEX"]];seed=FromDigits[Environment["HEPCAT_COLORED_PILOT_SEED"]];
process=Switch[index,1,{"u","U","c","C","h"},2,{"u","U","u","U","h"},_,Quit[2]];
output=FileNameJoin[{directory,"pilot-"<>ToString[index]}];If[!DirectoryQ[output],CreateDirectory[output]];
sha[path_]:=IntegerString[FileHash[path,"SHA256"],16,64];
save[name_,value_]:=Module[{p=FileNameJoin[{output,name<>".wxf"}],s},s=OpenWrite[p<>".tmp",BinaryFormat->True];BinaryWrite[s,Normal[BinarySerialize[value,PerformanceGoal->"Size"]],"Byte"];Close[s];RenameFile[p<>".tmp",p,OverwriteTarget->True]];
started=AbsoluteTime[];stage="Setup";timings={};printed={};live=OpenWrite[FileNameJoin[{output,"live.txt"}]];
SetAttributes[capture,HoldAll];capture[body_]:=Internal`InheritedBlock[{Print},Unprotect[Print];Print[]:=Null;Print[args__]:=(AppendTo[printed,If[Length[{args}]===1,First[{args}],SequenceForm[args]]];WriteString[live,ToString[Last[printed],OutputForm],"\n"];FlushWrite[live];Null);Protect[Print];body];
SetAttributes[run,HoldRest];run[name_,body_]:=Module[{t=AbsoluteTime[],value},stage=name;WriteString[live,DateString[{"Hour",":","Minute",":","Second"}]," ",name,"\n"];FlushWrite[live];value=capture[body];AppendTo[timings,<|"Stage"->name,"Seconds"->N[AbsoluteTime[]-t],"TotalSeconds"->N[AbsoluteTime[]-started]|>];value];
remaining[]:=Max[0.001,540-(AbsoluteTime[]-started)];
model=ReadModel[FileNameJoin[{root,"models"}],1];lower=ReadModel[FileNameJoin[{root,"models"}],2];
modelSHA=HEPCATCovariantCurrentSources`Private`objectSHA[model];lowerSHA=HEPCATCovariantCurrentSources`Private`objectSHA[lower];
draftHashes=<|"source.wl"->sha[FileNameJoin[{directory,"source.wl"}]]|>;
Get[FileNameJoin[{directory,"source.wl"}]];
coloredChecksPass[report_,pair_,originalChannels_List,originalModel_List,reference_List]:=Module[{projection,children,checks,rank},
 If[!AssociationQ[report]||Lookup[report,"Scope",None]=!="EveryIndependentColorCoefficient"||
   Lookup[Lookup[report,"FeynmanPhaseAgreement",<||>],"Status",None]=!="PASS",Return[False]];
 checks=Lookup[report,"Checks",{}];If[checks==={}||!AllTrue[checks,AssociationQ[#]&&Lookup[#,"Status",None]==="PASS"&],Return[False]];
 projection=ConstructiveDiagrams`Private`colorProjectionData[{pair},originalChannels,
   candidate["OriginalColorProjection"]["Representations"],reference];
 If[!AssociationQ[projection],Return[False]];rank=Length[projection["Basis"]];
 children=Lookup[report,"ColorCoefficientReports",{}];
 rank===Length[candidate["OriginalColorProjection"]["Basis"]]&&Length[children]===rank&&AllTrue[Range[rank],Function[coefficient,
   ConstructiveDiagrams`Private`constructAllChecksPassQ[children[[coefficient]],
    First[projection["ProjectedDiagrams"][[coefficient]]],projection["ProjectedChannels"][[coefficient]],
    originalModel,projection["ProjectedFeynmanAmplitudes"][[coefficient]]]]]
];
records={};candidate=Missing["NotBuilt"];prepared=Missing["NotPrepared"];preparedPair=Missing["NotPrepared"];
channels=Missing["NotGenerated"];benchmark=Missing["NotConverted"];feynmanPairs=Missing["NotBuilt"];status="UNFINISHED";
work[]:=Catch[Module[{fail,pair,sets,overrides,report,ok,rows,k,graphCertificates},
 fail[tag_,data_:<||>]:=(status="FAIL";Throw[Failure[tag,data]]);
 candidate=run["Building source-only per-graph colored-current candidate",HEPCATColoredCurrentSources`Private`source[process,model]];
 If[!AssociationQ[candidate]||Lookup[candidate,"Status",None]=!="UNVALIDATED_CANDIDATE",fail["SourceCandidateUnavailable",<|"Candidate"->candidate|>]];
 If[!AllTrue[{"EveryOriginalOrderedGraphAndFermionSignMatched","CompleteOffShellConversionCoverage","RequiresCheckAmplitudes","ModelParametersUnchanged"},TrueQ[Lookup[candidate,#,False]]&]||
   !AllTrue[{"NumericalCoefficientFit","IndependentBenchmarkRead","NativeValidationPerformed"},Lookup[candidate,#,None]===False&]||candidate["InternalOnShellConditions"]=!={}||candidate["SourceModelSHA256"]=!=modelSHA,fail["SourceCertificateGuard"]];
 If[Length[candidate["PerGraphCorrespondence"]]=!=candidate["ResolvedOriginalDiagramCount"]||
  Sort[Lookup[candidate["PerGraphCorrespondence"],"OriginalIndex"]]=!=Range[candidate["ResolvedOriginalDiagramCount"]]||
  !AllTrue[candidate["PerGraphCorrespondence"],#["OriginalFermionSign"]===#["LiftedFermionSign"]&],fail["PerGraphBijectiveSignGuard"]];
 pair=candidate["Amplitude"];
 save["proposal",<|"Status"->"UNVALIDATED_CANDIDATE","Process"->process,"Candidate"->candidate,"RawCoherentPair"->pair,"RawTensorExpression"->candidate["Expression"],"DraftFileSHA256"->draftHashes,"Seed"->seed,"IndependentFeynmanReadAfterThisRecord"->True|>];
 proposalSHA=sha[FileNameJoin[{output,"proposal.wxf"}]];
 prepared=run["Preparing candidate with external identities only",PrepareAmplitudes[pair,model,"ExternalParticles"->process,"InternalOnShell"->False,"PolePreference"->"AllPhysical","ReturnDetails"->True,"TimeConstraint"->Min[60,remaining[]]]];
 If[!TrueQ[ConstructiveDiagrams`Private`constructPreparationUsableQ[prepared]],fail["ExternalPreparationUnavailable",<|"Preparation"->prepared|>]];
 preparedPair=First[prepared["PreparedAmplitudes"]];
 If[First[preparedPair]=!=First[pair],fail["ExternalMassRulesChanged"]];
 save["preparation",<|"RawPair"->pair,"PreparedPair"->preparedPair,"PreparationDetails"->prepared,"InternalOnShell"->False,"ProposalSHA256"->proposalSHA|>];
 channels=run["Generating all original lower-amplitude channels",FactorizationAmplitudeChannels[process,lower]];
 (* The independent reference is read only after the source candidate is saved. *)
 feynman=ReadModel[FileNameJoin[{root,"tests","reference-models","calchep"}],1];
 graphs=run["Generating fresh independent Feynman diagrams",CreateDiagrams[process,feynman]];
 feynmanPairs=run["Building fresh independent Feynman amplitudes",DiagramAmplitudes[graphs]];
 benchmark=run["Converting complete off-shell Feynman benchmark",FeynmanToSpinorProducts[feynmanPairs,"ReferenceSpinors"->Automatic,"ReduceSpinorProducts"->False,"TimeConstraint"->Min[60,remaining[]]]];
 If[!TrueQ[ConstructiveDiagrams`Private`constructBenchmarkConversionCompleteQ[feynmanPairs,benchmark]],fail["IndependentBenchmarkIncomplete"]];
 sets=ConstructiveDiagrams`Private`completionValidationRuleSets[lower,Automatic,Automatic];
 If[!ListQ[sets]||Length[sets]=!=3,fail["StandardParameterSetsUnavailable"]];
 Do[
  overrides=ConstructiveDiagrams`Private`constructValidationOverrides[lower,Automatic,sets[[k]]];
  options={"ParameterRules"->overrides,"RandomSeed"->(seed+104729(k-1)),"Samples"->4,"SpuriousPoles"->Automatic,"HighEnergyCheck"->True,"ReferenceCheck"->True,
   "FeynmanAmplitudes"->benchmark,"FeynmanModel"->feynman,"TimeConstraint"->Min[150,remaining[]],"FeynmanTimeConstraint"->Min[150,remaining[]],"Output"->"Details"};
  report=run["Checking all native tests at standard assignment "<>ToString[k]<>"/3",Block[{ConstructiveDiagrams`Private`$amplitudeParallelActive=True},CheckAmplitudes[preparedPair,channels,lower,Sequence@@options]]];
  ok=TrueQ[coloredChecksPass[report,preparedPair,channels,lower,benchmark]];
  rows=If[AssociationQ[report],Lookup[report,"Checks",{}],{}];
  AppendTo[records,<|"ParameterIndex"->k,"ResolvedParameterRules"->sets[[k]],"CheckParameterOverrides"->overrides,"Options"->options,"Report"->report,"AllChecksPass"->ok,"StatusCounts"->Counts[Lookup[rows,"Status",{}]]|>];
  save["native-progress",<|"Status"->If[ok,"IN_PROGRESS","CHECKS_NOT_ALL_PASS"],"Process"->process,"PreparedPair"->preparedPair,"Records"->records,"DraftFileSHA256"->draftHashes,"ProposalSHA256"->proposalSHA|>];
  If[!ok,fail["NativeChecksNotAllPass",<|"ParameterIndex"->k|>]],{k,Length[sets]}];
 status="ALL_NATIVE_CHECKS_PASS";True
]];
outcome=MemoryConstrained[TimeConstrained[work[],540,Failure["PilotOverallTimeConstraint",<|"Stage"->stage|>]],4 2^30,Failure["PilotMemoryLimit",<|"Stage"->stage|>]];
If[FailureQ[outcome]&&status==="UNFINISHED",status="UNRESOLVED"];
record=<|"Status"->status,"Process"->process,"RuntimeSourceMode"->"IsolatedPerGraphColoredCurrentSource","ProductionContextSHA256"->"48d82ad2adaf29f9a338c9fb6b9cf3da37ed7570f6de5852ee50c798fd0d8363",
 "DraftFileSHA256"->draftHashes,"Candidate"->candidate,"PreparedPair"->preparedPair,"PreparationDetails"->prepared,"OriginalChannels"->channels,
 "IndependentFeynmanPairs"->feynmanPairs,"IndependentFeynmanBenchmark"->benchmark,"Records"->records,"RandomSeed"->seed,"Prints"->printed,"StageTimings"->timings,
 "Seconds"->N[AbsoluteTime[]-started],"Outcome"->outcome,"LastStage"->stage,"PublicDefaultConstructionSuccessClaimed"->False,"ProductionChanged"->False|>;
save["result",record];
summary=KeyTake[record,{"Status","Process","RuntimeSourceMode","ProductionContextSHA256","DraftFileSHA256","RandomSeed","StageTimings","Seconds","LastStage","PublicDefaultConstructionSuccessClaimed","ProductionChanged"}];
AssociateTo[summary,{"ParameterGroupsCompleted"->Length[records],"AllGroupStatusCounts"->Lookup[records,"StatusCounts",{}],"OriginalDiagramCount"->If[AssociationQ[candidate],Lookup[candidate,"OriginalDiagramCount",Null],Null],
 "ContactCalibrationCount"->If[AssociationQ[candidate],Length[Lookup[candidate,"ContactCalibrations",{}]],Null],"ResultCapsuleSHA256"->sha[FileNameJoin[{output,"result.wxf"}]],"Failure"->If[FailureQ[outcome],ToString[outcome,InputForm],Null]}];
Export[FileNameJoin[{output,"result.json"}],summary,"RawJSON"];
Close[live];Print[summary];
End[];Quit[If[summary["Status"]==="ALL_NATIVE_CHECKS_PASS",0,1]];
