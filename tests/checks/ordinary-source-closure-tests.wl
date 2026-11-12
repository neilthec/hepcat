(* Failure-only ordinary source-slot recovery preserves the original proposal. *)
(* HEPCAT, Copyright (C) 2026 Neil Christensen. GPL-3.0-or-later. *)
Begin["OrdinarySourceClosureTests`Private`"];
root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[root<>"/source/HEPCAT.wl"];
checks=0;failures={};SetAttributes[check,HoldRest];
check[name_,value_]:=(checks++;If[!TrueQ[value],AppendTo[failures,name];Print["FAIL: ",name]]);
retry=ConstructiveDiagrams`Private`completionRetryTimedOutOrdinarySources;
records=Table[<|"Expression"->osSource[i],"Source"->"SyntheticOrdinarySource"|>,{i,3}];
recordsHash=Hash[records,"SHA256"];
run[indices_,body_,seconds_:3,supported_:Function[value,True]]:=Internal`InheritedBlock[
 {ConstructiveDiagrams`Private`completionExternalReduce},
 Clear[ConstructiveDiagrams`Private`completionExternalReduce];
 calls={};
 ConstructiveDiagrams`Private`completionExternalReduce[value_,external_]:=(AppendTo[calls,value];body[value]);
 retry[records,indices,{},AbsoluteTime[]+seconds,supported]];
result=run[{2},Function[value,value+osOrdinaryTerm]];
check["only the indices explicitly recorded as timed out are retried",calls==={osSource[2]} && Lookup[result["Reports"],"SourceIndex"]==={2}];
check["an admitted ordinary source retains its original catalog ownership",result["Sources"]==={<|"SourceIndex"->2,"Expression"->osSource[2]+osOrdinaryTerm|>} && result["Status"]==="COMPLETE"];
check["the ledger records the first-pass timeout and successful second pass",result["Reports"][[1,"FirstPassStatus"]]==="TIMEOUT" && result["Reports"][[1,"Status"]]==="ADMITTED"];
check["each source retry has the unchanged finite one-second maximum",AllTrue[result["Reports"],NumericQ[#["TimeConstraint"]]&&0<#["TimeConstraint"]<=1&]];
check["retry memory is independently bounded",AllTrue[result["Reports"],IntegerQ[#["MemoryConstraintBytes"]]&&0<#["MemoryConstraintBytes"]<2^30&]];
none=run[{},Function[value,value]];
check["completed first-pass slots require no retry work",calls==={} && none["Status"]==="NOT_NEEDED" && none["Sources"]==={} && !TrueQ[none["Attempted"]]];
do[bad_]:=retry[records,bad,{},AbsoluteTime[]+3,Function[value,True]];
check["duplicate timeout indices cannot duplicate a source contribution",do[{1,1}]["Reason"]==="InvalidSourceSlots"];
check["out-of-range timeout indices are rejected before indexing",do[{4}]["Reason"]==="InvalidSourceSlots"];
check["string indices do not masquerade as source slots",do[{"2"}]["Reason"]==="InvalidSourceSlots"];
check["negative timeout indices are rejected",do[{-1}]["Reason"]==="InvalidSourceSlots"];
check["a malformed source record cannot enter the ordinary closure",retry[{<||>},{1},{},AbsoluteTime[]+3,Function[value,True]]["Reason"]==="InvalidSourceSlots"];
check["an infinite deadline does not grant an unbounded closure",retry[records,{1},{},Infinity,Function[value,True]]["Reason"]==="NoFiniteBuildDeadline"];
expired=retry[records,{1,2},{},AbsoluteTime[]-1,Function[value,True]];
check["an exhausted build clock starts no new source reduction",expired["Status"]==="DEADLINE" && expired["Reports"]==={} && expired["Sources"]==={}];
timed=run[{1,2},Function[value,Pause[0.15];value],0.03];
check["a timed-out retry preserves the source and cannot start a later slot after the deadline",timed["Sources"]==={} && Length[timed["Reports"]]===1 && timed["Reports"][[1,"Status"]]==="TIMEOUT" && calls==={osSource[1]}];
check["a short shared clock is passed literally to the source slot",0<timed["Reports"][[1,"TimeConstraint"]]<=0.03];
rejected=run[{1},Function[value,osUndeclaredPole],3,Function[value,FreeQ[value,osUndeclaredPole]]];
check["the existing support predicate can reject a new denominator",rejected["Sources"]==={} && rejected["Reports"][[1,"Status"]]==="REJECTED"];
Do[badResult=run[{1},Function[value,bad]];
 check["a failed or non-scalar rewrite preserves its literal source "<>ToString[bad,InputForm],badResult["Sources"]==={} && badResult["Reports"][[1,"Status"]]==="REJECTED"],
 {bad,{$Failed,Indeterminate,{osSource[1]},<|"Unsafe"->1|>,osSource[1]->0,Missing["UnsupportedIdentity"]}}];
memoryResult=Internal`InheritedBlock[{MemoryConstrained},
 Unprotect[MemoryConstrained];Clear[MemoryConstrained];SetAttributes[MemoryConstrained,HoldAll];
 MemoryConstrained[body_,bytes_,fallback_]:=(memoryCaptured=bytes;fallback);
 run[{1},Function[value,value]]];
check["the actual memory-guard sentinel supplies no correction direction",memoryResult["Sources"]==={} && memoryResult["Reports"][[1,"Status"]]==="MEMORY_LIMIT"];
check["the memory-bound ledger matches the guard actually invoked",memoryCaptured===memoryResult["Reports"][[1,"MemoryConstraintBytes"]]];
check["source expressions and caller-owned records are never modified",Hash[records,"SHA256"]===recordsHash];
(* This is an ordinary identity fixture, never a production process branch. *)
model=ReadModel[root<>"/models",2];
fixture=(Get[root<>"/tests/checks/fixtures/ordinary-source-closure.wl"])[model];
check["the real fixture binds parameters to the supplied model",AssociationQ[fixture] && AllTrue[DeleteDuplicates[Cases[fixture,_ModelParameter,Infinity]],
 MemberQ[First/@ModelParameterRules[model],#]&]];
actualRecord=<|"Expression"->fixture["SourceExpression"],"Source"->"PreparedChannel","CorrelatedRegularQuotient"->False|>;
allowed=DeleteDuplicates[Cases[fixture["SourceExpression"],_PropDen,Infinity]];
supportedReal=Function[value,FreeQ[value,Spinor["Xi",___]] &&
 AllTrue[Cases[value,_PropDen,Infinity],MemberQ[allowed,#]&]];
actual=retry[{actualRecord},{1},fixture["ExternalMassRules"],AbsoluteTime[]+4,supportedReal];
check["the general source-slot helper admits the real missing ordinary identity",actual["Status"]==="COMPLETE" && Length[actual["Sources"]]===1];
actualExpression=If[actual["Sources"]==={},$Failed,actual["Sources"][[1,"Expression"]]];
check["the source reduction equals the independently recorded external-only endpoint",actualExpression===fixture["ExpectedReducedSource"]];
plan=ConstructiveDiagrams`Private`completionBosePlan[{{4,5}},fixture["ExternalMassRules"]];
terms=ConstructiveDiagrams`Private`withProtectedPropagators[actualExpression,Expand];
terms=If[Head[terms]===Plus,List@@terms,{terms}];
columns=ConstructiveDiagrams`Private`completionNormalizeBasis[
 ConstructiveDiagrams`Private`completionCompactExchangeProject[#,plan]&/@
 ConstructiveDiagrams`Private`completionNormalizeBasis[terms]];
check["the recovered atoms contain the previously missing signed exchange projection",MemberQ[columns,fixture["ExpectedDirection"]]];
check["the recovered real source introduces no new typed physical propagator",AllTrue[Cases[actualExpression,_PropDen,Infinity],MemberQ[allowed,#]&]];
Do[left=SpinorChain[Spinor["Helicity","Square",3],Mom[2],Spinor["Spin","Angle",1]];
 right=SpinorChain[Spinor["Helicity","Square",3],Mom[h],Spinor["Spin","Angle",2]];
 expected=-(Mass[2] SpinorChain[Spinor["Helicity","Square",3],Spinor["Spin","Square",2]]*
  SpinorChain[Spinor["Helicity","Square",3],Mom[h],Spinor["Spin","Angle",1]])-
  SpinorChain[Spinor["Spin","Angle",1],Spinor["Spin","Angle",2]]*
  SpinorChain[Spinor["Helicity","Square",3],Mom[2],Mom[h],Spinor["Helicity","Square",3]];
 difference=Block[{ConstructiveDiagrams`Private`$rspCanonicalProductBasis=True},
  ReduceSpinorProducts[SchoutenRewrite[left,right,{1,1}]-expected]]/.fixture["ExternalMassRules"];
 check["the missing atom has an exact Schouten and external-Dirac derivation "<>ToString[h],difference===0],{h,{4,5}}];
(* Actual collector integration: regular quotients stay correlated. *)
gRules=Thread[(Mass/@Range[5])->{1,1,0,2,2}];gProp=PropDen[Mom[Multiparticle[1,3]],1];
gExpressions=Join[{(gProp+gSlowCoefficient)/gProp},Table[(gProp+gCoefficient[i])/gProp,{i,9}]];
gNormalized=<|"ExternalMassRules"->gRules,"Records"->{},"Targets"->{<|"Cut"->{1,3},"Complement"->{2,4,5},
 "ExternalLegs"->Range[5],"ExternalMassRules"->gRules,"Mass"->1,"Propagator"->gProp,
 "StateSum"->1,"ChannelExpression"->1/(z gProp),"Complete"->False|>}|>;
gPlan=ConstructiveDiagrams`Private`completionBosePlan[{},gRules];
gRun[enabled_]:=Internal`InheritedBlock[
 {ConstructiveDiagrams`Private`completionInputProposals,ConstructiveDiagrams`Private`completionExternalReduce},
 Clear[ConstructiveDiagrams`Private`completionInputProposals,ConstructiveDiagrams`Private`completionExternalReduce];
 ConstructiveDiagrams`Private`completionInputProposals[args___]:=<|"Status"->"AVAILABLE","RegularExpression"->0,"PoleBearingByRecord"->gExpressions|>;
 ConstructiveDiagrams`Private`completionExternalReduce[value_,rules_]:=(If[!FreeQ[value,gSlowCoefficient],Pause[0.08]];Together[value]);
 Block[{ConstructiveDiagrams`Private`$completionOrdinarySourceClosure=enabled,
  ConstructiveDiagrams`Private`$completionAutomaticCorrectionTermCeiling=None},
  ConstructiveDiagrams`Private`completionPhysicalChannelBasis[gNormalized,None,gPlan,128,128,1,Missing["NoReducedChannels"]]]];
gActive=gRun[True];gOrdinary=gRun[False];
check["the actual collector retries only its measured failed ordinary slot",
 gActive["Status"]==="AVAILABLE" && gActive["OrdinarySourceClosure"]["TimedOutSourceIndices"]==={1} &&
 Lookup[gActive["OrdinarySourceClosure"]["Reports"],"Status"]==={"ADMITTED"}];
check["the collector retains every original correlated source expression",
 Length[gActive["Sources"]]===10 && AllTrue[gActive["Sources"],TrueQ[#["CorrelatedRegularQuotient"]]&]];
check["regular quotient pieces do not become independent contact coordinates",
 gActive["RegularExpression"]===0 && Length[gActive["Basis"]]===10 && !MemberQ[gActive["Basis"],1] &&
 AllTrue[gActive["Basis"],!FreeQ[#,_PropDen]&]];
check["successful ordinary identity resumption preserves the entire correlated proposal",
 gActive["Basis"]===gOrdinary["Basis"]];
check["a disabled collector keeps its historical report format and source path",
 !KeyExistsQ[gOrdinary,"OrdinarySourceClosure"] && !KeyExistsQ[gOrdinary,"OrdinarySourceReductionReports"]];
check["the collector does not allocate a new clock for the second pass",
 gActive["OrdinarySourceClosure"]["OriginalBuildTimeConstraint"]===1 &&
 AllTrue[gActive["OrdinarySourceClosure"]["Reports"],0<#["TimeConstraint"]<=1&]];
(* Reproduce the actual missing-source mechanism through the real collector.
   A deterministic pause makes the first-pass deadline independent of CPU speed;
   the second pass executes the unmodified real ordinary-identity reducer. *)
rRules=fixture["ExternalMassRules"];rLegs=Range[5];rProps=allowed;
rTargets=Map[Function[prop,With[{key=ConstructiveDiagrams`Private`residuePropagatorKey[prop,rLegs]},
 <|"Cut"->First[key],"Complement"->Complement[rLegs,First[key]],"ExternalLegs"->rLegs,
 "ExternalMassRules"->rRules,"Mass"->Last[key],"Propagator"->prop,"StateSum"->1,
 "ChannelExpression"->1/(z prop),"Complete"->False|>]],rProps];
rExpressions=Join[{fixture["SourceExpression"]},Table[rCoefficient[i]/First[rProps],{i,9}]];
rNormalized=<|"ExternalMassRules"->rRules,"Records"->{},"Targets"->rTargets|>;
rOriginalReduction=DownValues[ConstructiveDiagrams`Private`completionExternalReduce];
rRun[enabled_]:=Internal`InheritedBlock[
 {ConstructiveDiagrams`Private`completionInputProposals,ConstructiveDiagrams`Private`completionExternalReduce},
 Clear[ConstructiveDiagrams`Private`completionInputProposals,ConstructiveDiagrams`Private`completionExternalReduce];
 ConstructiveDiagrams`Private`completionInputProposals[args___]:=<|"Status"->"AVAILABLE","RegularExpression"->0,"PoleBearingByRecord"->rExpressions|>;
 ConstructiveDiagrams`Private`completionExternalReduce[value_,rules_]:=(
  If[!FreeQ[value,_SpinorChain],Pause[0.15]];
  Internal`InheritedBlock[{ConstructiveDiagrams`Private`completionExternalReduce},
   DownValues[ConstructiveDiagrams`Private`completionExternalReduce]=rOriginalReduction;
   ConstructiveDiagrams`Private`completionExternalReduce[value,rules]]);
 Block[{ConstructiveDiagrams`Private`$completionOrdinarySourceClosure=enabled,
  ConstructiveDiagrams`Private`$rspCanonicalProductBasis=True,
  ConstructiveDiagrams`Private`$completionAutomaticCorrectionTermCeiling=None},
  ConstructiveDiagrams`Private`completionPhysicalChannelBasis[rNormalized,None,plan,1024,1024,2,Missing["NoReducedChannels"]]]];
rOrdinary=rRun[False];rActive=rRun[True];
check["the real source fixture times out on the narrow first pass and succeeds only on its resumed slot",
 rActive["Status"]==="AVAILABLE" && MemberQ[rActive["OrdinarySourceClosure"]["TimedOutSourceIndices"],1] &&
 MemberQ[Lookup[rActive["OrdinarySourceClosure"]["Reports"],"Status"],"ADMITTED"]];
check["the actual collector recovers the missing ordinary identity direction",
 MemberQ[rActive["Basis"],fixture["ExpectedDirection"]] && !MemberQ[rOrdinary["Basis"],fixture["ExpectedDirection"]]];
check["ordinary-source closure preserves the complete original correction span",
 Length[ConstructiveDiagrams`Private`completionIndependentBasis[Join[rActive["Basis"],rOrdinary["Basis"]],2]]===Length[rActive["Basis"]]];
check["the resumed real source retains only declared physical propagator types",
 AllTrue[Cases[rActive["Basis"],_PropDen,Infinity],
 MemberQ[rProps,#/.Mom[index_Multiparticle]:>Mom[Multiparticle@@Sort[List@@index]]]&]];

(* Dispatch permission belongs only to the existing automatic recovery. *)
input=<|"Amplitude"->SpinorChain[Spinor["Spin","Angle",1],Spinor["Spin","Angle",2]],
 "ExternalMassRules"->fixture["ExternalMassRules"],"RemainderData"->{<|"HasInternalConditions"->True,
 "ShellRemainder"->fixture["SourceExpression"],"FixedReferenceExpression"->fixture["SourceExpression"],
 "ReferenceInputExpression"->fixture["SourceExpression"]|>}|>;
channelInput=<|"OriginalAmplitudes"->{},"PreparedAmplitudes"->{},"RemainderData"->{}|>;
dispatch[extra_List:{},completeFirst_:False]:=Internal`InheritedBlock[
 {ConstructiveDiagrams`Private`completionFactorizationCore,ConstructiveDiagrams`Private`completionNormalizeInput,
 ConstructiveDiagrams`Private`completionPhysicalDenominatorOnlyQ},
 Clear[ConstructiveDiagrams`Private`completionFactorizationCore,ConstructiveDiagrams`Private`completionNormalizeInput,
 ConstructiveDiagrams`Private`completionPhysicalDenominatorOnlyQ];flags={};
 ConstructiveDiagrams`Private`completionNormalizeInput[args___]:=<||>;
 ConstructiveDiagrams`Private`completionPhysicalDenominatorOnlyQ[args___]:=True;
 ConstructiveDiagrams`Private`completionFactorizationCore[i_,ch_,m_,opts___Rule]:=(
  AppendTo[flags,TrueQ[ConstructiveDiagrams`Private`$completionOrdinarySourceClosure]];
  <|"Status"->If[TrueQ[completeFirst]||Length[flags]>1,"COMPLETE","UNRESOLVED"],
   "PreparationRemainderFit"->Missing["RemainderFitTimeout"],"Amplitude"->input["Amplitude"],
   "FamilyAmplitude"->input["Amplitude"],"ExternalMassRules"->fixture["ExternalMassRules"],
   "EffectiveMaxCorrectionTerms"->128,"FitSamples"->3,"ElapsedTime"->0.001,"Diagnostics"->{}|>);
 CompleteFactorization[input,channelInput,model,"ReturnDetails"->True,"RandomSeed"->271823,
  "PolePreference"->"AllPhysical","ProgressReporting"->False,Sequence@@extra]];
automatic=dispatch[];
check["only the renewed failure-only attempt enables ordinary-source closure",flags==={False,True} && automatic["Status"]==="COMPLETE"];
complete=dispatch[{},True];
check["successful initial paths do not enable the closure permission",flags==={False} && !KeyExistsQ[complete,"AutomaticTimeoutRecovery"]];
explicit=Block[{ConstructiveDiagrams`Private`$completionOrdinarySourceClosure=True},dispatch[{"TimeConstraint"->13}]];
check["an explicit public clock resets inherited closure permission and is never retried",flags==={False} && !KeyExistsQ[explicit,"AutomaticTimeoutRecovery"]];
check["ordinary source closure adds no public option",!MemberQ[First/@Options[CompleteFactorization],"OrdinarySourceClosure"]];
If[checks=!=46,AppendTo[failures,"Not every ordinary-source assertion executed."]];
Print["ORDINARY SOURCE CLOSURE CHECKS: ",checks,"; FAILURES: ",Length[failures]];
If[failures==={},Print["HEPCAT_TEST_COMPLETED:ordinary-source-closure-tests.wl"],Print[failures]];
End[];Quit[If[OrdinarySourceClosureTests`Private`failures==={},0,1]];
