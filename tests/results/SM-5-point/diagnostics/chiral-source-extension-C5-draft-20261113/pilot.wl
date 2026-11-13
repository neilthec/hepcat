(* Isolated source candidate and independent native validation. No production edits. *)
Get["/Users/neil/code/hepcat/source/HEPCAT.wl"];
Begin["HEPCATChiralSourcePilot`Private`"];
$ContextPath=Prepend[DeleteCases[$ContextPath,"HEPCATChiralSourcePilot`Private`"],"HEPCATChiralSourcePilot`Private`"];
$HistoryLength=0;root="/Users/neil/code/hepcat";directory=DirectoryName[$InputFileName];
index=FromDigits[Environment["HEPCAT_CHIRAL_PILOT_INDEX"]];seed=FromDigits[Environment["HEPCAT_CHIRAL_PILOT_SEED"]];
process=Switch[index,1,{"ne","Ne","Z","h","h"},2,{"ne","Ne","Z","Z","h"},_,Quit[2]];
output=FileNameJoin[{directory,"pilot-"<>ToString[index]}];If[!DirectoryQ[output],CreateDirectory[output]];
sha[path_]:=IntegerString[FileHash[path,"SHA256"],16,64];
save[name_,value_]:=Module[{p=FileNameJoin[{output,name<>".wxf"}],s},s=OpenWrite[p<>".tmp",BinaryFormat->True];BinaryWrite[s,Normal[BinarySerialize[value,PerformanceGoal->"Size"]],"Byte"];Close[s];RenameFile[p<>".tmp",p,OverwriteTarget->True]];
started=AbsoluteTime[];stage="Setup";timings={};printed={};live=OpenWrite[FileNameJoin[{output,"live.txt"}]];
SetAttributes[capture,HoldAll];capture[body_]:=Internal`InheritedBlock[{Print},Unprotect[Print];Print[]:=Null;Print[args__]:=(AppendTo[printed,If[Length[{args}]===1,First[{args}],SequenceForm[args]]];WriteString[live,ToString[Last[printed],OutputForm],"\n"];FlushWrite[live];Null);Protect[Print];body];
SetAttributes[run,HoldRest];run[name_,body_]:=Module[{t=AbsoluteTime[],value},stage=name;WriteString[live,DateString[{"Hour",":","Minute",":","Second"}]," ",name,"\n"];FlushWrite[live];value=capture[body];AppendTo[timings,<|"Stage"->name,"Seconds"->N[AbsoluteTime[]-t],"TotalSeconds"->N[AbsoluteTime[]-started]|>];value];
remaining[]:=Max[0.001,280-(AbsoluteTime[]-started)];
model=ReadModel[FileNameJoin[{root,"models"}],1];lower=ReadModel[FileNameJoin[{root,"models"}],2];
modelSHA=HEPCATCovariantCurrentSources`Private`objectSHA[model];lowerSHA=HEPCATCovariantCurrentSources`Private`objectSHA[lower];
draftHashes=AssociationThread[{"draft-sources.wl","draft-contacts.wl"},sha[FileNameJoin[{directory,#}]]&/@{"draft-sources.wl","draft-contacts.wl"}];
Get[FileNameJoin[{directory,"draft-sources.wl"}]];Get[FileNameJoin[{directory,"draft-contacts.wl"}]];
records={};candidate=Missing["NotBuilt"];prepared=Missing["NotPrepared"];preparedPair=Missing["NotPrepared"];
channels=Missing["NotGenerated"];benchmark=Missing["NotConverted"];feynmanPairs=Missing["NotBuilt"];status="UNFINISHED";
work[]:=Catch[Module[{fail,pair,sets,overrides,report,ok,rows,k,graphCertificates},
 fail[tag_,data_:<||>]:=(status="FAIL";Throw[Failure[tag,data]]);
 candidate=run["Building source-only chiral draft candidate",HEPCATCovariantContactCalibration`Private`calibratedCovariantSource[process,model,lower,"TimeConstraint"->Min[60,remaining[]]]];
 If[!AssociationQ[candidate]||Lookup[candidate,"Status",None]=!="UNVALIDATED_CANDIDATE",fail["SourceCandidateUnavailable",<|"Candidate"->candidate|>]];
 If[!AllTrue[{"EveryOriginalDiagramCovered","EveryUsedOrderedVertexCertified","CompleteOffShellConversionCoverage","RequiresCheckAmplitudes"},TrueQ[Lookup[candidate,#,False]]&]||
   !AllTrue[{"NumericalCoefficientFit","IndependentBenchmarkRead","NativeValidationPerformed","NewIndependentContacts"},Lookup[candidate,#,None]===False&]||candidate["InternalOnShellConditions"]=!={}||
   candidate["SourceModelSHA256"]=!=modelSHA||candidate["LowerModelSHA256"]=!=lowerSHA,fail["SourceCertificateGuard"]];
 If[Sort[candidate["OrderedGraphCertificates"]]=!=Sort[candidate["LiftedGraphCertificates"]],fail["SourceGraphGuard"]];
 If[index===2&&candidate["ContactCalibrations"]=!={},fail["ExpectedCubicOnlyPilotHasQuartic"]];
 pair=candidate["Amplitude"];
 save["proposal",<|"Status"->"UNVALIDATED_CANDIDATE","Process"->process,"Candidate"->candidate,"RawCoherentPair"->pair,"RawTensorExpression"->candidate["RawTensorExpression"],"DraftFileSHA256"->draftHashes,"Seed"->seed,"IndependentFeynmanReadAfterThisRecord"->True|>];
 proposalSHA=sha[FileNameJoin[{output,"proposal.wxf"}]];
 prepared=run["Preparing candidate with external identities only",PrepareAmplitudes[{pair},model,"ExternalParticles"->process,"InternalOnShell"->False,"PolePreference"->"AllPhysical","ReturnDetails"->True,"TimeConstraint"->Min[40,remaining[]]]];
 If[!TrueQ[ConstructiveDiagrams`Private`constructPreparationUsableQ[prepared]],fail["ExternalPreparationUnavailable",<|"Preparation"->prepared|>]];
 preparedPair=First[prepared["PreparedAmplitudes"]];
 If[First[preparedPair]=!=candidate["ExternalMassRules"],fail["ExternalMassRulesChanged"]];
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
   "FeynmanAmplitudes"->benchmark,"FeynmanModel"->feynman,"TimeConstraint"->Min[60,remaining[]],"Output"->"Details"};
  report=run["Checking all native tests at standard assignment "<>ToString[k]<>"/3",Block[{ConstructiveDiagrams`Private`$amplitudeParallelActive=True},CheckAmplitudes[preparedPair,channels,lower,Sequence@@options]]];
  ok=TrueQ[ConstructiveDiagrams`Private`constructAllChecksPassQ[report,preparedPair,channels,lower,benchmark]];
  rows=If[AssociationQ[report],Lookup[report,"Checks",{}],{}];
  AppendTo[records,<|"ParameterIndex"->k,"ResolvedParameterRules"->sets[[k]],"CheckParameterOverrides"->overrides,"Options"->options,"Report"->report,"AllChecksPass"->ok,"StatusCounts"->Counts[Lookup[rows,"Status",{}]]|>];
  save["native-progress",<|"Status"->If[ok,"IN_PROGRESS","CHECKS_NOT_ALL_PASS"],"Process"->process,"PreparedPair"->preparedPair,"Records"->records,"DraftFileSHA256"->draftHashes,"ProposalSHA256"->proposalSHA|>];
  If[!ok,fail["NativeChecksNotAllPass",<|"ParameterIndex"->k|>]],{k,Length[sets]}];
 status="ALL_NATIVE_CHECKS_PASS";True
]];
outcome=MemoryConstrained[TimeConstrained[work[],280,Failure["PilotOverallTimeConstraint",<|"Stage"->stage|>]],4 2^30,Failure["PilotMemoryLimit",<|"Stage"->stage|>]];
If[FailureQ[outcome]&&status==="UNFINISHED",status="UNRESOLVED"];
record=<|"Status"->status,"Process"->process,"RuntimeSourceMode"->"IsolatedChiralDraftOverrides","ProductionContextSHA256"->"b672a8945605ad07e3f24e47a783f071a54c9b057e44b65048267ec057d55afd",
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
