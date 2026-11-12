(* Automatic effort follows the actual job while explicit controls stay exact.
   Construction regressions receive no numerical tuning options. *)

(*
HEPCAT - High Energy Physics Constructive Amplitude Tool

Copyright (C) 2026 Neil Christensen

This file is part of HEPCAT.

HEPCAT is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

HEPCAT is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with HEPCAT.  If not, see <https://www.gnu.org/licenses/>.
*)


(* A resource-only retry cannot change residue targets or explicit controls. *)
root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root,"source","HEPCAT.wl"}]];
checks=0;failures={};SetAttributes[check,HoldRest];
check[name_,value_]:=(checks++;If[!TrueQ[value],AppendTo[failures,name];Print["FAIL: ",name]]);
frModel=ReadModel[FileNameJoin[{root,"models"}],3];
frRules=Join[{Mass[1]->frMass,Mass[2]->frMass},Thread[(Mass/@Range[3,5])->0]];
frExpression=SpinorChain[Spinor["Spin","Angle",1],Spinor["Spin","Angle",2]];
frSource=Total[Table[frCoefficient[i] MomProd[1,3],{i,900}]];
frRecord=<|"HasInternalConditions"->True,"ShellRemainder"->frSource,
 "FixedReferenceExpression"->frExpression,"ReferenceInputExpression"->frSource|>;
frInput=<|"Amplitude"->frExpression,"PreparedAmplitudes"->{{frRules,frExpression}},
 "ExternalMassRules"->frRules,"RemainderData"->{frRecord}|>;
frChannels=<|"OriginalAmplitudes"->{},"PreparedAmplitudes"->{},"RemainderData"->{}|>;
frRequest=Join[Association[Options[CompleteFactorization]],<|"RandomSeed"->719384,
 "PolePreference"->"AllPhysical","ProgressReporting"->False|>];
frFailure=<|"Status"->"UNRESOLVED","PreparationRemainderFit"->Missing["RemainderFitTimeout"]|>;
frRecovery[input_:frInput,request_:frRequest,report_:frFailure]:=
 ConstructiveDiagrams`Private`completionFactorizationTimeoutRecovery[input,frChannels,frModel,request,report];
frPlan=frRecovery[];
check["a typed retained-fit timeout admits the actual larger five-leg source work",TrueQ[frPlan["Eligible"]]];
frMeasured=ConstructiveDiagrams`Private`completionFactorizationResourceMetrics[frInput,frChannels,frModel,frRequest,True];
check["the recovery clock is exactly the existing source-aware finite heuristic",
 frPlan["TimeConstraint"]===ConstructiveDiagrams`Private`amplitudeAutomaticSettings[
  "Factorization",frRequest,frMeasured]["Settings"]["TimeConstraint"] &&
 NumericQ[frPlan["TimeConstraint"]] && 0<frPlan["TimeConstraint"]<Infinity];
check["the admission certificate records the typed timeout and consumed sources",
 frPlan["FailureEvidence"]===Missing["RemainderFitTimeout"] &&
 frPlan["ResourcePolicy"]["Metrics"]["FactorizationRetainedSourceSlots"]===3 &&
 frPlan["ResourcePolicy"]["Metrics"]["FactorizationRetainedSourceLeaves"]===
  2 LeafCount[frSource]+LeafCount[frExpression]];
check["an explicit clock is authoritative before any source work is read",
 frRecovery[frInput,Join[frRequest,<|"TimeConstraint"->17/3|>]]["Reason"]==="ExplicitTimeConstraint"];
check["an explicit infinity clock is not silently replaced by a finite retry",
 !TrueQ[frRecovery[frInput,Join[frRequest,<|"TimeConstraint"->Infinity|>]]["Eligible"]]];
check["an explicit basis retains its literal correction-space contract",
 frRecovery[frInput,Join[frRequest,<|"CorrectionBasis"->{frExpression}|>]]["Reason"]==="ExplicitCorrectionBasis"];
check["a complete result never receives timeout recovery",
 !TrueQ[frRecovery[frInput,frRequest,Join[frFailure,<|"Status"->"COMPLETE"|>]]["Eligible"]]];
check["a missing fit is not by itself evidence of a timeout",
 !TrueQ[frRecovery[frInput,frRequest,KeyDrop[frFailure,"PreparationRemainderFit"]]["Eligible"]]];
check["strings resembling diagnostics are not accepted as typed timeout evidence",
 !TrueQ[frRecovery[frInput,frRequest,Join[frFailure,<|"PreparationRemainderFit"->"RemainderFitTimeout"|>]]["Eligible"]]];
check["an unrelated construction timeout is not a retained-fit timeout",
 !TrueQ[frRecovery[frInput,frRequest,Join[frFailure,<|"PreparationRemainderFit"->Missing["RemainderConstructionTimeout"]|>]]["Eligible"]]];
check["an unclassified report status cannot authorize a new clock",
 !TrueQ[frRecovery[frInput,frRequest,Join[frFailure,<|"Status"->"UNKNOWN"|>]]["Eligible"]]];
frFour=Join[frInput,<|"ExternalMassRules"->Most[frRules],
 "PreparedAmplitudes"->{{Most[frRules],frExpression}}|>];
check["four-point defaults retain their original scope even after a timeout",!TrueQ[frRecovery[frFour]["Eligible"]]];
check["no retained source history means no recovery",!TrueQ[frRecovery[KeyDrop[frInput,"RemainderData"]]["Eligible"]]];
check["a retained source smaller than the displayed expression cannot extend the clock",
 !TrueQ[frRecovery[Join[frInput,<|"RemainderData"->{Join[frRecord,<|
  "ShellRemainder"->0,"FixedReferenceExpression"->0,"ReferenceInputExpression"->0|>]}|>]]["Eligible"]]];
frInitialMetrics=ConstructiveDiagrams`Private`completionFactorizationResourceMetrics[frInput,frChannels,frModel,frRequest];
check["the successful degree-one policy remains source-ineligible upfront",
 frInitialMetrics["FactorizationSourceWorkAdmitted"]===False &&
 frInitialMetrics["FactorizationSourceWorkLeaves"]===0];
check["only literal True enables private source measurement",
 ConstructiveDiagrams`Private`completionFactorizationResourceMetrics[
  frInput,frChannels,frModel,frRequest,"True"]===frInitialMetrics];
frBefore=Hash[{frInput,frChannels,frModel},"SHA256"];
frRun[extra_List:{},succeed_:True,firstComplete_:False,validFallback_:False]:=Internal`InheritedBlock[
 {ConstructiveDiagrams`Private`completionFactorizationCore,
  ConstructiveDiagrams`Private`completionNormalizeInput,
  ConstructiveDiagrams`Private`completionPhysicalDenominatorOnlyQ},
 Clear[ConstructiveDiagrams`Private`completionFactorizationCore,
  ConstructiveDiagrams`Private`completionNormalizeInput,
  ConstructiveDiagrams`Private`completionPhysicalDenominatorOnlyQ];
 ConstructiveDiagrams`Private`completionNormalizeInput[args___]:=<||>;
 ConstructiveDiagrams`Private`completionPhysicalDenominatorOnlyQ[args___]:=!TrueQ[validFallback];
 frCalls={};
 ConstructiveDiagrams`Private`completionFactorizationCore[i_,ch_,m_,options___Rule]:=Module[
  {settings=Association[{options}],complete},
  AppendTo[frCalls,<|"Input"->i,"Channels"->ch,"Model"->m,"Options"->settings|>];
  complete=TrueQ[firstComplete] && Length[frCalls]===1 || Length[frCalls]>1 && TrueQ[succeed];
  Join[frFailure,<|"Status"->If[complete,"COMPLETE","UNRESOLVED"],
   "Amplitude"->frExpression,"FamilyAmplitude"->frExpression,
   "ExternalMassRules"->frRules,"EffectiveMaxCorrectionTerms"->512,
   "FitSamples"->settings["Samples"],"ElapsedTime"->0.001,
   "CorrectionSpace"->"ResourceDispatchFixture",
   "Diagnostics"->If[Length[frCalls]>1 && !TrueQ[validFallback],{"correction-term cap"},{}]|>]];
 CompleteFactorization[frInput,frChannels,frModel,"ReturnDetails"->True,
  "RandomSeed"->719384,"PolePreference"->If[TrueQ[validFallback],Automatic,"AllPhysical"],
  "ProgressReporting"->False,"Samples"->3,"ValidationSamples"->5,"MaxRefinements"->2,Sequence@@extra]];
frResult=frRun[];frSavedCalls=frCalls;
check["one failed resource attempt is followed by a complete retry",frResult["Status"]==="COMPLETE" && Length[frSavedCalls]===2];
check["the next attempt carries the already discovered automatic cap",
 frSavedCalls[[1,"Options","MaxCorrectionTerms"]]===128 &&
 frSavedCalls[[2,"Options","MaxCorrectionTerms"]]===512 && frResult["EffectiveMaxCorrectionTerms"]===512];
check["both attempts use exactly the same inputs, targets and model",
 AllTrue[frSavedCalls,#["Input"]===frInput && #["Channels"]===frChannels && #["Model"]===frModel&]];
check["the seed and explicit fit/held-out/refinement settings remain unchanged",
 AllTrue[frSavedCalls,KeyTake[#["Options"],{"RandomSeed","Samples","ValidationSamples","MaxRefinements"}]===
  <|"RandomSeed"->719384,"Samples"->3,"ValidationSamples"->5,"MaxRefinements"->2|>&]];
check["history retains the original failed report and both finite clock policies",
 frResult["AutomaticTimeoutRecovery"]["InitialFailedReport"]["Status"]==="UNRESOLVED" &&
 frResult["AutomaticTimeoutRecovery"]["InitialResourcePolicy"]["Resolved"]["TimeConstraint"]===
  frResult["AutomaticTimeoutRecovery"]["InitialTimeConstraint"] &&
 frResult["AutomaticTimeoutRecovery"]["RetryTimeConstraint"]===frPlan["TimeConstraint"] &&
 frResult["AutomaticTimeoutRecovery"]["MaximumCombinedTimeConstraint"]===
  Max[frResult["AutomaticTimeoutRecovery"]["InitialTimeConstraint"],
   frResult["AutomaticTimeoutRecovery"]["InitialElapsedTime"]]+frPlan["TimeConstraint"]];
check["adaptive attempt history preserves the failed attempt before the complete one",
 Lookup[frResult["AdaptiveAttempts"],"Status"]==={"UNRESOLVED","COMPLETE"}];
check["the renewed core receives at most its one measured source allowance",
 0<frSavedCalls[[2,"Options","TimeConstraint"]]<=frPlan["TimeConstraint"]];
frStillFailing=frRun[{},False];
check["a second timeout stays unresolved instead of creating an unbounded retry loop",
 frStillFailing["Status"]==="UNRESOLVED" && Length[frCalls]===2 &&
 Length[frStillFailing["AdaptiveAttempts"]]===2];
frCompleteFirst=frRun[{},True,True];
check["an immediately complete result keeps its original single attempt and report format",
 frCompleteFirst["Status"]==="COMPLETE" && Length[frCalls]===1 &&
 !KeyExistsQ[frCompleteFirst,"AutomaticTimeoutRecovery"]];
frBestFallback=frRun[{},False,True,True];
check["a previously complete representation fallback prevents a later failed fit from extending its clock",
 frBestFallback["Status"]==="COMPLETE" && Length[frCalls]===2 &&
 !KeyExistsQ[frBestFallback,"AutomaticTimeoutRecovery"] &&
 Lookup[frBestFallback["AdaptiveAttempts"],"Status"]==={"COMPLETE","UNRESOLVED"}];
frExplicit=frRun[{"TimeConstraint"->17/3}];
check["an explicit stage clock is neither increased nor retried",
 frExplicit["Status"]==="UNRESOLVED" && Length[frCalls]===1 &&
 frCalls[[1,"Options","TimeConstraint"]]<=17/3 && !KeyExistsQ[frExplicit,"AutomaticTimeoutRecovery"]];
frExplicitCap=frRun[{"MaxCorrectionTerms"->16}];
check["a literal correction cap remains literal through resource recovery",
 Length[frCalls]===2 && AllTrue[frCalls,#["Options"]["MaxCorrectionTerms"]===16&] &&
 frExplicitCap["EffectiveMaxCorrectionTerms"]===16];
check["resource recovery leaves all caller input and model data unchanged",
 Hash[{frInput,frChannels,frModel},"SHA256"]===frBefore];
check["this change adds no public completion option",
 !MemberQ[First/@Options[CompleteFactorization],"AutomaticTimeoutRecovery"]];
If[checks=!=30,AppendTo[failures,"Not every source-timeout-retry assertion executed."]];
Print["FACTORIZATION SOURCE TIMEOUT RETRY CHECKS: ",checks,"; FAILURES: ",Length[failures]];
If[failures=!={},Print[failures],Print["HEPCAT_TEST_COMPLETED:factorization-source-timeout-retry-tests.wl"]];
Quit[If[failures==={},0,1]];
