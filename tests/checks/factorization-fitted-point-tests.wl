(* A retained sparse-fit point is distinct from an unfinished affine-family proof.
   Dispatch controls below keep the real preparation and independent checks. *)
root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root,"source","HEPCAT.wl"}]];
checks=0;failures={};SetAttributes[check,HoldRest];
check[name_,value_]:=(checks++;If[!TrueQ[value],AppendTo[failures,name];Print["FAIL: ",name]]);
fpVars=Table[Unique["fitCoordinate"],{2}];fpExternal=Thread[(Mass/@Range[4])->{1,1,0,0}];
fpSolution=<|"Status"->"UNDERDETERMINED","SolutionRules"->{fpVars[[1]]->2+fpVars[[2]]},"FreeParameters"->{fpVars[[2]]},"Rank"->1|>;
fpBasis={MomProd[1,3],MomProd[2,3]};fpSeed=7;fpTrial=fpSeed+fpVars.fpBasis;
fpCheckpoint=ConstructiveDiagrams`Private`completionFactorizationPointCheckpoint[fpTrial,fpVars,fpSolution,fpExternal,<|"Round"->0|>];
fpSource=<|"Status"->"UNRESOLVED","Amplitude"->fpSeed,"FamilyAmplitude"->fpSeed,"FreeParameters"->{},"RemainingBasis"->{},
 "ExternalMassRules"->fpExternal,"CorrectionBasis"->fpBasis,"FittedPointCheckpoint"->fpCheckpoint,
 "ConstraintData"-><|"FitSeed"->fpSeed,"FitVariables"->fpVars,"LinearSolution"->fpSolution|>|>;
fpBefore=fpSource;fpPoint=ConstructiveDiagrams`Private`constructFactorizationFittedPoint[fpSource,5];
check["checkpoint retains the exact sparse-validation coordinate choice without claiming validation",
 fpCheckpoint["TrialAmplitude"]===fpTrial&&fpCheckpoint["FitVariables"]===fpVars&&fpCheckpoint["SolutionRules"]===fpSolution["SolutionRules"]&&
 fpCheckpoint["ParticularAssignment"]==={Last[fpVars]->0}&&!TrueQ[fpCheckpoint["IndependentPointValidationPerformed"]]&&
 !TrueQ[fpCheckpoint["FamilyUniquenessEstablished"]]&&!TrueQ[fpCheckpoint["BenchmarkUsedForConstruction"]]];
check["materialization recovers the fitted point rather than the returned original amplitude",
 AssociationQ[fpPoint]&&fpPoint["Amplitude"]===7+2MomProd[1,3]&&fpPoint["Amplitude"]=!=fpSource["Amplitude"]&&
 !TrueQ[fpPoint["FittedPointCheckpoint"]["ReturnedAmplitudeWasRecordedPoint"]]];
check["every formal unclassified affine direction and coordinate is preserved",
 fpPoint["FreeParameters"]==={Last[fpVars]}&&fpPoint["RemainingBasis"]==={Total[fpBasis]}&&
 Expand[fpPoint["FamilyAmplitude"]-fpTrial/.fpSolution["SolutionRules"]]===0&&
 !TrueQ[fpPoint["FittedPointCheckpoint"]["FormalAffineDirectionsClassified"]]];
check["materialization leaves the original unresolved report unchanged",fpSource===fpBefore&&fpSource["Status"]==="UNRESOLVED"];
Do[change=one[[1]];i=one[[2]];fpBad=Join[fpSource,<|"FittedPointCheckpoint"->Join[fpCheckpoint,change]|>];
 check["a changed recorded recipe is rejected "<>ToString[i],MissingQ[ConstructiveDiagrams`Private`constructFactorizationFittedPoint[fpBad,5]]],
 {one,{{<|"ParticularAssignment"->{Last[fpVars]->1}|>,1},{<|"FitVariables"->Reverse[fpVars]|>,2},
 {<|"TrialAmplitude"->-fpTrial|>,3},{<|"ExternalMassRules"->Reverse[fpExternal]|>,4},
 {<|"SolutionRules"->{First[fpVars]->-2+Last[fpVars]}|>,5}}}];
check["a changed correction-basis order cannot reuse the old checkpoint",
 MissingQ[ConstructiveDiagrams`Private`constructFactorizationFittedPoint[Join[fpSource,<|"CorrectionBasis"->Reverse[fpBasis]|>],5]]];
check["an incomplete coordinate solution cannot create a checkpoint",
 MissingQ[ConstructiveDiagrams`Private`completionFactorizationPointCheckpoint[fpTrial,fpVars,Join[fpSolution,<|"FreeParameters"->{}|>],fpExternal]]];
check["duplicate correction coordinates cannot create a checkpoint",
 MissingQ[ConstructiveDiagrams`Private`completionFactorizationPointCheckpoint[fpTrial,{First[fpVars],First[fpVars]},fpSolution,fpExternal]]];
check["contradictory fit status cannot create a candidate",
 MissingQ[ConstructiveDiagrams`Private`completionFactorizationPointCheckpoint[fpTrial,fpVars,Join[fpSolution,<|"Status"->"INCONSISTENT"|>],fpExternal]]];
check["nonfinite fit expressions cannot create a candidate",
 MissingQ[ConstructiveDiagrams`Private`completionFactorizationPointCheckpoint[Indeterminate,fpVars,fpSolution,fpExternal]]];

Do[fpKnownFailure=Join[fpSource,<|"FittedPointCheckpoint"->Join[fpCheckpoint,<|"SparseValidationReports"->{
 <|"Report"-><|"Checks"->{<|"Type"->"Physical","Status"->status|>}|>|>}|>]|>];
 check["known failed held-out point evidence cannot be overridden "<>status,
  ConstructiveDiagrams`Private`constructFactorizationFittedPoint[fpKnownFailure,5]===Missing["KnownFailedFittedPoint"]],
 {status,{"FAIL","Opp Sign"}}];
fpUnprovedFamily=Join[fpSource,<|"FittedPointCheckpoint"->Join[fpCheckpoint,<|"FamilyPass"->False,
 "SparseValidationReports"->{<|"Report"-><|"Checks"->{<|"Type"->"Physical","Status"->"PASS"|>},
  "AffineDirectionDiagnostic"-><|"Status"->"FAIL"|>|>|>}|>]|>];
check["unfinished or failed affine-direction classification is not mistaken for a failed point",
 AssociationQ[ConstructiveDiagrams`Private`constructFactorizationFittedPoint[fpUnprovedFamily,5]]];

(* Real four-point amplitudes and Feynman checks, with only the unfinished
   completion snapshot controlled to exercise the recovery deterministically. *)
fpModel=ReadModel[FileNameJoin[{root,"models"}],3];fpLower=ReadModel[FileNameJoin[{root,"models"}],4];
fpFeynman=ReadModel[FileNameJoin[{root,"tests","reference-models","calchep"}],2];
fpProcess={"e","E","A.+","A.+"};fpMasses=Thread[(Mass/@Range[4])->ConstructiveDiagrams`Private`constructExternalMasses[fpProcess,fpModel]];
fpKnown=Block[{ConstructiveDiagrams`Private`$colorFactorizationEnabled=False},LookupAmplitude[MapIndexed[{#1,First[#2]}&,fpProcess],fpLower,False,Table[Unique["spin"],{4}]]];
(* Resolve known lower-model parameters by declared name into the model used
   for this controlled constructor, as the ordinary public path does. *)
fpKnown=fpKnown/.(p_ModelParameter:>ModelParameter[fpModel,ModelParameterName[p]]);
fpMakeSource[expr_,status_:"UNRESOLVED"]:=Module[{v=Table[Unique["retainedFit"],{2}],sol,b,tr,cp},
 sol=<|"Status"->"UNDERDETERMINED","SolutionRules"->{First[v]->1},"FreeParameters"->{Last[v]},"Rank"->1|>;
 b={expr,1};tr=v.b;cp=ConstructiveDiagrams`Private`completionFactorizationPointCheckpoint[tr,v,sol,fpMasses];
 <|"Status"->status,"Amplitude"->0,"OriginalExpression"->0,"FamilyAmplitude"->0,"FreeParameters"->{},"RemainingBasis"->{},
 "ExternalMassRules"->fpMasses,"CorrectionBasis"->b,"FittedPointCheckpoint"->cp,"Diagnostics"->{"Controlled unfinished affine-family proof"},
 "ConstraintData"-><|"FitSeed"->0,"FitVariables"->v,"LinearSolution"->sol|>|>];
fpRun[source_,benchmark_:True]:=Module[{result},fpCFCount=0;fpHECount=0;
 result=Internal`InheritedBlock[{CompleteFactorization,CompleteHighEnergy},Unprotect[CompleteFactorization,CompleteHighEnergy];
 DownValues[CompleteFactorization]={HoldPattern[CompleteFactorization[___]]:>(fpCFCount++;source)};
 DownValues[CompleteHighEnergy]={HoldPattern[CompleteHighEnergy[___]]:>(fpHECount++;Failure["UnexpectedEnergyFit",<||>])};
 ConstructAmplitude[fpProcess,fpModel,fpModel,"FeynmanModel"->If[TrueQ[benchmark],fpFeynman,None],
 "Output"->"Details","Parallelization"->False,"ProgressReporting"->False,"PrintReport"->False,
 "RandomSeed"->927573,"TimeConstraint"->180,"Samples"->1,"ValidationSamples"->1,
 "PreparationOptions"->{"MaxSteps"->0,"MaxReferenceChoices"->1},"ChannelPreparationOptions"->{"MaxSteps"->0,"MaxReferenceChoices"->1},
 "FeynmanPreparationOptions"->{"MaxSteps"->0},"FinalPreparationOptions"->{"MaxSteps"->0}]];result];
fpRecovery=fpRun[fpMakeSource[fpKnown]];
check["an unresolved fit point can pass every real independent check without finishing its affine proof",
 AssociationQ[fpRecovery]&&fpRecovery["Status"]==="COMPLETE"&&fpCFCount===1&&fpHECount===0&&
 fpRecovery["RepresentativeSelection"]["SourceStage"]==="Retained factorization point"&&
 AllTrue[fpRecovery["RepresentativeSelection"]["ValidationReports"],AllTrue[#["Report"]["Checks"],#["Status"]==="PASS"&]&]];
check["retained-point acceptance keeps every ordinary parameter assignment and the independent benchmark",
 Lookup[fpRecovery["RepresentativeSelection"]["ValidationReports"],"ParameterRules"]===
 ConstructiveDiagrams`Private`completionValidationRuleSets[fpModel,Automatic,Automatic]&&
 AllTrue[fpRecovery["RepresentativeSelection"]["ValidationReports"],#["Report"]["FeynmanCheck"]["Status"]==="PASS"&]&&
 !TrueQ[fpRecovery["RepresentativeSelection"]["UniquenessEstablished"]]];
check["the old unresolved expression and coordinate recipe remain visible after successful point recovery",
 fpRecovery["Stages"]["Completing factorization"]["Amplitude"]===0&&
 fpRecovery["Stages"]["Completing factorization"]["Status"]==="UNRESOLVED"&&
 fpRecovery["RepresentativeSelection"]["RetainedFreeParameters"]=!={}&&
 fpRecovery["RepresentativeSelection"]["FittedPointCheckpoint"]["Materialized"]===True];
fpWrong=fpRun[fpMakeSource[-fpKnown]];
check["a physical or strict-phase Feynman disagreement cannot promote an unfinished point",
 fpWrong["Status"]==="FAILED"&&fpHECount===0&&Length[fpWrong["RepresentativeSelectionAttempts"]]===1&&
 !TrueQ[First[fpWrong["RepresentativeSelectionAttempts"]]["Selected"]]&&
 fpWrong["RepresentativeSelection"]===<|"Attempted"->False,"Selected"->False|>&&
 KeyExistsQ[fpWrong["Stages"],"Rejected retained factorization point"]];
fpNoBenchmark=fpRun[fpMakeSource[fpKnown],False];
check["without an independent benchmark unresolved completion retains its strict failure contract",
 fpNoBenchmark["Status"]==="FAILED"&&!KeyExistsQ[fpNoBenchmark["Stages"],"Materializing retained factorization point"]&&fpHECount===0];
fpComplete=fpRun[Join[fpMakeSource[fpKnown,"COMPLETE"],<|"Amplitude"->fpKnown|>]];
check["already complete factorization does not pay for the new recovery checks",
 fpComplete["Status"]==="FAILED"&&fpHECount===1&&!KeyExistsQ[fpComplete["Stages"],"Materializing retained factorization point"]];
(* Preserve a real solved fit when only classification of its remaining
   affine directions is interrupted. The physical samples remain genuine. *)
fpChannels=FactorizationAmplitudeChannels[fpProcess,fpModel];
fpCaptured=Internal`InheritedBlock[{ConstructiveDiagrams`Private`completionSparseValidation},
 DownValues[fpOriginalSparse]=DownValues[ConstructiveDiagrams`Private`completionSparseValidation]/.
  HoldPattern[ConstructiveDiagrams`Private`completionSparseValidation]->fpOriginalSparse;
 Clear[ConstructiveDiagrams`Private`completionSparseValidation];
 ConstructiveDiagrams`Private`completionSparseValidation[args___]:=Module[{r=fpOriginalSparse[args]},
  Join[r,<|"FamilyPass"->False,"Feedback"->Join[r["Feedback"],<|"Status"->"UNRESOLVED",
   "Diagnostics"->{"Controlled unfinished affine-direction classification"}|>]|>]];
 CompleteFactorization[{fpMasses,0},fpChannels,fpModel,"CorrectionBasis"->{fpKnown,1},
  "InputReweighting"->None,"ChannelReferences"->None,"Samples"->1,"ValidationSamples"->1,
  "Parallelization"->False,"ProgressReporting"->False,"RandomSeed"->821753,"TimeConstraint"->30,"Output"->"Details"]];
fpCapturedPoint=ConstructiveDiagrams`Private`constructFactorizationFittedPoint[fpCaptured,5];
check["actual completion checkpoints its fit before unfinished family classification",
 AssociationQ[fpCaptured]&&fpCaptured["Status"]==="UNRESOLVED"&&fpCaptured["Amplitude"]===0&&
 AssociationQ[fpCaptured["FittedPointCheckpoint"]]&&AssociationQ[fpCapturedPoint]&&
 Together[fpCapturedPoint["Amplitude"]-fpKnown]===0];
check["actual sparse point validation stays bound to its recipe without claiming full validation",
 Length[fpCaptured["FittedPointCheckpoint"]["SparseValidationReports"]]>0&&
 AllTrue[fpCaptured["FittedPointCheckpoint"]["SparseValidationReports"],AllTrue[#["Report"]["Channels"],#["Status"]==="PASS"&]&]&&
 fpCaptured["FittedPointCheckpoint"]["FitEquationCount"]===Length[fpCaptured["ConstraintData"]["Fit"]["Equations"]]&&
 !TrueQ[fpCaptured["FittedPointCheckpoint"]["IndependentPointValidationPerformed"]]];

fpMassParameter=ModelParameter[fpModel,"Me"];
fpDefaultMass=fpMassParameter/.ConstructiveDiagrams`Private`residueParameterRules[fpModel,Automatic];
fpSpecialized=fpRun[fpMakeSource[fpKnown fpMassParameter/fpDefaultMass]];
check["a default-parameter-only point fails the unchanged alternate-parameter obligations",
 fpSpecialized["Status"]==="FAILED"&&Length[fpSpecialized["RepresentativeSelectionAttempts"]]===1&&
 With[{r=First[fpSpecialized["RepresentativeSelectionAttempts"]]["ValidationReports"]},
  Length[r]>1&&AllTrue[First[r]["Report"]["Checks"],#["Status"]==="PASS"&]&&
  AnyTrue[Last[r]["Report"]["Checks"],#["Status"]=!="PASS"&]]];

(* A shared literal reference is not split. This rational zero cancels by
   ordinary scalar algebra before any reference choice or kinematic check. *)
fpXi=Spinor["Xi","Angle",779];fpSp[i_]:=Spinor[If[i<3,"Spin","Helicity"],"Angle",i];
fpBracket[a_,b_]:=SpinorChain[a,b];
fpU=fpBracket[fpXi,fpSp[3]];fpV=fpBracket[fpXi,fpSp[4]];
fpZero=fpU/fpV+fpV/fpU-(fpU^2+fpV^2)/(fpU fpV);
fpSharedRaw=fpKnown+fpZero;
fpSharedBefore=ConstructiveDiagrams`Private`preparationReferences[fpSharedRaw,fpMasses,fpModel,
 ConstructiveDiagrams`Private`preparationInputPair[#,"FactorizationChannel"]&/@fpChannels,"AllPhysical",{},False,True];
check["an exact rational zero still exposes the unchanged shared-Xi guard before cancellation",
 Together[fpZero]===0&&fpSharedBefore["Status"]==="UNRESOLVED"&&fpSharedBefore["Assignments"]==={}];
fpSharedRecovered=fpRun[fpMakeSource[fpSharedRaw]];
check["external-only preparation can remove a shared Xi before the unchanged resolver and full checks",
 fpSharedRecovered["Status"]==="COMPLETE"&&FreeQ[fpSharedRecovered["Amplitude"],Spinor["Xi",___]]&&
 AllTrue[fpSharedRecovered["RepresentativeSelection"]["ValidationReports"],AllTrue[#["Report"]["Checks"],#["Status"]==="PASS"&]&]&&
 fpSharedRecovered["Stages"]["Preparing retained factorization point representative details"]["Options"]["InternalOnShell"]===False];

Print["CHECKS: ",checks,"; FAILURES: ",Length[failures]];If[failures=!={},Print[InputForm[failures]]];If[failures==={},Print["HEPCAT_TEST_COMPLETED:factorization-fitted-point-tests.wl"]];Quit[If[failures==={},0,1]];
