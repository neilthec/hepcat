(* A rejected point-first family cannot hide ordinary sources behind exact zeros. *)
(* HEPCAT - High Energy Physics Constructive Amplitude Tool
   Copyright (C) 2026 Neil Christensen
   SPDX-License-Identifier: GPL-3.0-or-later *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root,"source","HEPCAT.wl"}]];
$HistoryLength = 0; checks = 0; failures = {};
SetAttributes[check,HoldRest];
check[name_,value_] := (checks++;If[!TrueQ[value],AppendTo[failures,name];Print["FAIL: ",name]]);
zModel = ReadModel[FileNameJoin[{root,"models"}],3]; zOriginalModel = zModel;
zMass = ModelParameter[zModel,"Me"]; zParameter = Unique["zeroInherited"];
zRules = {Mass[1]->zMass,Mass[2]->zMass,Mass[3]->0,Mass[4]->0,Mass[5]->0};
zZero = Total[MomProd[1,#]& /@ Range[5]];
zCenter = 1/PropDen[Mom[Multiparticle[1,3]],zMass];
zNormalized = <|"Expression"->zCenter,"OriginalExpression"->zCenter,
 "FamilyAmplitude"->zCenter+zParameter zZero,"FreeParameters"->{zParameter},
 "RemainingBasis"->{zZero},"ExternalMassRules"->zRules,
 "RemainderData"->{<|"Witness"->"diagram source"|>},
 "ChannelRemainderData"->{<|"Witness"->"channel source"|>},"PolePreference"->"AllPhysical"|>;
zBefore = <|"CachedWitness"->"unchanged center checks"|>;
zChartWitness = <|"Method"->"ExactAffineLinearContradiction","OriginalCoordinates"->{zParameter},
 "WorkingCoordinates"->{zParameter},"PriorAffineRestrictionRules"->{},"ReachedEquations"->{1},
 "LinearSystem"->ConstructiveDiagrams`Private`completionSolveLinear[{1},{zParameter}],"UncheckedChartsClaimed"->False|>;
zRejected = <|"Status"->"UNRESOLVED","Amplitude"->zCenter,"BeforeReport"->zBefore,
 "HighEnergyInconsistencyCertificate"->zChartWitness,
 "ConstraintData"-><|"HighEnergy"-><|"Status"->"INCONSISTENT"|>|>|>;
zSettings = Join[Association[Options[CompleteHighEnergy]],<|"Output"->"Details",
 "ParameterRules"->ModelParameterRules[zModel],"RandomSeed"->1729,"TimeConstraint"->30,
 "ProgressReporting"->False,"Parallelization"->False,"IdentityProofModel"->zModel|>];
check["only an exact point-first contradiction is eligible",
 ConstructiveDiagrams`Private`completionHEZeroFamilyRetryEligibleQ[zNormalized,zRejected,zSettings,True]];
check["successful point-first paths do not pay for identity recovery",
 !ConstructiveDiagrams`Private`completionHEZeroFamilyRetryEligibleQ[zNormalized,Join[zRejected,<|"Status"->"COMPLETE"|>],zSettings,True]];
check["a generic unresolved child is not mistaken for an exact contradiction",
 !ConstructiveDiagrams`Private`completionHEZeroFamilyRetryEligibleQ[zNormalized,KeyDrop[zRejected,"HighEnergyInconsistencyCertificate"],zSettings,True]];
check["an empty contradiction association is not a certificate",
 !ConstructiveDiagrams`Private`completionHEZeroFamilyRetryEligibleQ[zNormalized,Join[zRejected,<|"HighEnergyInconsistencyCertificate"-><||>|>],zSettings,True]];
check["an unresolved linear solve cannot certify a contradiction",
 !ConstructiveDiagrams`Private`completionHEExactInconsistencyWitnessQ[Join[zChartWitness,<|"LinearSystem"-><|"Status"->"UNRESOLVED"|>|>]]];
check["an absent reached-equation witness cannot trigger recovery",
 !ConstructiveDiagrams`Private`completionHEExactInconsistencyWitnessQ[KeyDrop[zChartWitness,"ReachedEquations"]]];
check["the exact parameter-family wrapper retains its underlying chart witness",
 ConstructiveDiagrams`Private`completionHEExactInconsistencyWitnessQ[<|"Method"->"ExactParameterFamilyIntersectionContradiction",
 "OriginalCoordinates"->{zParameter},"PriorAffineRestrictionRules"->{},"ReachedEquations"->{1},
 "ChartContradiction"->zChartWitness,"UncheckedParameterAssignmentsClaimed"->False|>]];
check["an unfinished HE constraint group cannot enter recovery",
 !ConstructiveDiagrams`Private`completionHEZeroFamilyRetryEligibleQ[zNormalized,Join[zRejected,<|"ConstraintData"-><|"HighEnergy"-><|"Status"->"UNRESOLVED"|>|>|>],zSettings,True]];
check["explicit user correction bases remain bounded exactly as requested",
 !ConstructiveDiagrams`Private`completionHEZeroFamilyRetryEligibleQ[zNormalized,zRejected,Join[zSettings,<|"CorrectionBasis"->{zCenter}|>],True]];
check["direct strict completion does not take a point-only recovery",
 !ConstructiveDiagrams`Private`completionHEZeroFamilyRetryEligibleQ[zNormalized,zRejected,zSettings,False]];
check["an already empty inherited family needs no recovery",
 !ConstructiveDiagrams`Private`completionHEZeroFamilyRetryEligibleQ[Join[zNormalized,<|"FreeParameters"->{}|>],zRejected,zSettings,True]];
zCalls = 0; zSeen = Missing["NotCalled"];
zRecovered = Internal`InheritedBlock[{ConstructiveDiagrams`Private`completionHighEnergyWork},
 Clear[ConstructiveDiagrams`Private`completionHighEnergyWork];
 ConstructiveDiagrams`Private`completionHighEnergyWork[input_,channels_,model_,settings_,normalized_,retain_] := (
  zCalls++; zSeen = <|"Input"->input,"Channels"->channels,"Model"->model,"Settings"->settings,"Normalized"->normalized,
   "AutomaticFitSamples"->TrueQ[ConstructiveDiagrams`Private`$completionAutomaticFitSamples]|>;
  <|"Status"->"UNRESOLVED","PointReady"->True,"Amplitude"->normalized["Expression"],"FreeParameters"->{}|>);
 ConstructiveDiagrams`Private`completionHERecoverZeroInheritedFamily[zNormalized,zRejected,{},zModel,zSettings,30,True,True]];
check["an actual external momentum identity restores the ordinary-source policy",
 zCalls===1 && zRecovered["Retried"] && zRecovered["SelectedRetryReport"] && zRecovered["OrdinaryBasisReenabled"]];
zProof = zRecovered["IdentityReduction"];
check["the original nonliteral direction is certified before its coordinate is removed",
 zProof["Certificates"][[1,"Status"]]==="PROVED_ZERO" && zProof["RemovedParameters"]==={zParameter} &&
  ConstructiveDiagrams`Private`completionHEEntireInheritedFamilyZeroQ[zProof,{zParameter}]];
check["no incomplete affine family is passed to the retry",
 zSeen["Normalized"]["FreeParameters"]==={} && zSeen["Normalized"]["RemainingBasis"]==={} &&
 FreeQ[zSeen["Normalized"]["FamilyAmplitude"],zParameter]];
check["the retry preserves both retained source catalogs and the original center",
 zSeen["Normalized"]["RemainderData"]===zNormalized["RemainderData"] &&
 zSeen["Normalized"]["ChannelRemainderData"]===zNormalized["ChannelRemainderData"] &&
 zSeen["Normalized"]["Expression"]===zCenter && zSeen["Model"]===zModel];
check["the cached checks and automatic source choice survive without a benchmark",
 zSeen["Settings"]["BeforeReport"]===zBefore && zSeen["Settings"]["CorrectionBasis"]===Automatic &&
 zSeen["AutomaticFitSamples"] && !zRecovered["BenchmarkUsedForConstruction"] && !zRecovered["IndependentInteractionAdded"]];
check["the retry consumes only the original finite allowance",
 0<zSeen["Settings"]["TimeConstraint"]<=30 && zRecovered["ElapsedSeconds"]<31];
check["the same seed, numerical model assignment and validation policy are retained",
 zSeen["Settings"]["RandomSeed"]===zSettings["RandomSeed"] &&
 zSeen["Settings"]["ParameterRules"]===zSettings["ParameterRules"] &&
 zSeen["Settings"]["ValidationParameterRules"]===zSettings["ValidationParameterRules"]];
check["the exact failed child and all zero certificates remain inspectable",
 zRecovered["OriginalReport"]===zRejected && zRecovered["OriginalInheritedParameters"]==={zParameter} &&
 zRecovered["IdentityReduction"]===zProof];
check["the restored ordinary amplitude terms actually form an available correction source",
 ConstructiveDiagrams`Private`completionHEAutomaticBasis[zSeen["Normalized"]["Expression"],<||>,zSettings,-1]==={zCenter}];
check["a missing or unfinished proof cannot remove inherited coordinates",
 !ConstructiveDiagrams`Private`completionHEEntireInheritedFamilyZeroQ[Missing["Timeout"],{zParameter}] &&
 !ConstructiveDiagrams`Private`completionHEEntireInheritedFamilyZeroQ[Join[zProof,<|"Certificates"->{<|"Parameter"->zParameter,"Status"->"NOT_PROVED"|>}|>],{zParameter}]];
check["a certificate for a different coordinate is never substituted",
 !ConstructiveDiagrams`Private`completionHEEntireInheritedFamilyZeroQ[Join[zProof,<|"Certificates"->{<|"Parameter"->zOther,"Status"->"PROVED_ZERO"|>}|>],{zParameter}]];
check["a retained nonzero coordinate cannot be hidden by a zero-status label",
 !ConstructiveDiagrams`Private`completionHEEntireInheritedFamilyZeroQ[Join[zProof,<|"FamilyAmplitude"->zCenter+zParameter|>],{zParameter}] &&
 !ConstructiveDiagrams`Private`completionHEEntireInheritedFamilyZeroQ[Join[zProof,<|"FreeParameters"->{zParameter}|>],{zParameter}]];
check["a whole-family replay certificate is required",
 !ConstructiveDiagrams`Private`completionHEEntireInheritedFamilyZeroQ[KeyDrop[zProof,"FamilyRepresentationCertificate"],{zParameter}]];
zStopped = ConstructiveDiagrams`Private`completionHERecoverZeroInheritedFamily[zNormalized,zRejected,{},zModel,zSettings,0,True,True];
check["an exhausted stage cannot launch proof or broaden the source family",
 !zStopped["Attempted"] && !zStopped["Retried"] && zStopped["OriginalReport"]===zRejected];
zBad = Join[zNormalized,<|"FamilyAmplitude"->zCenter+zParameter,"RemainingBasis"->{1}|>];
zNonzero = ConstructiveDiagrams`Private`completionHERecoverZeroInheritedFamily[zBad,zRejected,{},zModel,zSettings,12,True,True];
check["a genuine constant contact direction is retained and never relabeled zero",
 !zNonzero["Retried"] && zNonzero["OriginalReport"]===zRejected &&
 zNonzero["IdentityReduction"]["FreeParameters"]==={zParameter}];
zFailure = Internal`InheritedBlock[{ConstructiveDiagrams`Private`completionHEPruneIdentityFamily},
 Clear[ConstructiveDiagrams`Private`completionHEPruneIdentityFamily];
 ConstructiveDiagrams`Private`completionHEPruneIdentityFamily[___] := Failure["ProofMemoryLimit",<||>];
 ConstructiveDiagrams`Private`completionHERecoverZeroInheritedFamily[zNormalized,zRejected,{},zModel,zSettings,8,True,True]];
check["a proof memory failure cannot publish a partial replacement family",
 !zFailure["Retried"] && zFailure["OriginalReport"]===zRejected];
zWorkerFailure = Internal`InheritedBlock[{ConstructiveDiagrams`Private`completionHighEnergyWork},
 Clear[ConstructiveDiagrams`Private`completionHighEnergyWork];
 ConstructiveDiagrams`Private`completionHighEnergyWork[___] := Failure["FitMemoryLimit",<||>];
 ConstructiveDiagrams`Private`completionHERecoverZeroInheritedFamily[zNormalized,zRejected,{},zModel,zSettings,8,True,True]];
check["a retry memory failure preserves the original failure rather than selecting a partial candidate",
 zWorkerFailure["Retried"] && !zWorkerFailure["SelectedRetryReport"] &&
 zWorkerFailure["Report"]===zRejected && FailureQ[zWorkerFailure["RetryReport"]]];
zAbort = CheckAbort[Internal`InheritedBlock[{ConstructiveDiagrams`Private`completionHEPruneIdentityFamily},
 Clear[ConstructiveDiagrams`Private`completionHEPruneIdentityFamily];
 ConstructiveDiagrams`Private`completionHEPruneIdentityFamily[___] := Abort[];
 ConstructiveDiagrams`Private`completionHERecoverZeroInheritedFamily[zNormalized,zRejected,{},zModel,zSettings,8,True,True]],$Aborted];
check["a user abort propagates rather than accepting a partial family",zAbort===$Aborted];
check["all checks leave the supplied model unchanged",zModel===zOriginalModel];
Print[checks," high-energy zero-family retry checks; ",Length[failures]," failures."];
If[failures=!={},Print[failures];Exit[1]];
Print["HEPCAT_TEST_COMPLETED:high-energy-zero-family-retry-tests.wl"];Exit[0];
