(* Dispatch/acceptance controls only. Native integration is a separate opt-in suite. *)
root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root,"source","HEPCAT.wl"}]];
checks=0;failures={};SetAttributes[check,HoldRest];
check[name_,test_]:=(checks++;If[!TrueQ[test],AppendTo[failures,name];Print["FAIL: ",name]]);
cvModel=ReadModel[FileNameJoin[{root,"models"}],1];
cvLower=ReadModel[FileNameJoin[{root,"models"}],2];
cvFeynman=ReadModel[FileNameJoin[{root,"tests","reference-models","calchep"}],1];
cvCase={"e","E","m","M","h"};
cvRules[p_]:=Thread[(Mass/@Range[Length[p]])->ConstructiveDiagrams`Private`constructExternalMasses[p,cvModel]];
cvOriginalDefinitions={DownValues[ConstructAmplitude],DownValues[CheckAmplitudes],DownValues[CompleteFactorization],
 DownValues[PrepareAmplitudes],DownValues[HEPCATCovariantCurrentSources`Private`covariantCurrentSource]};
cvRun[process_,kind_,extra_:<||>,benchmark_:True]:=Module[{answer,external=cvRules[process],proposal,settings},
 cvSources=0;cvFits=0;cvChecks={};cvPreparations={};cvChannels={"original-target"};
 proposal=<|"Status"->"UNVALIDATED_CANDIDATE","Amplitude"->{external,3},"Expression"->3,
 "ExternalMassRules"->external,"EveryOriginalDiagramCovered"->True,"EveryUsedOrderedVertexCertified"->True,
 "CompleteOffShellConversionCoverage"->True,"RequiresCheckAmplitudes"->True,
 "NumericalCoefficientFit"->False,"IndependentBenchmarkRead"->False,"NativeValidationPerformed"->False,
 "NewIndependentContacts"->False,"InternalOnShellConditions"->{},"NumericalParameterAssignments"->{},
 "SourceModelSHA256"->HEPCATCovariantCurrentSources`Private`objectSHA[cvModel],
 "LowerModelSHA256"->HEPCATCovariantCurrentSources`Private`objectSHA[cvLower],
 "OriginalDiagramCount"->1,"LiftedDiagramCount"->1,
 "OrderedGraphCertificates"->{<|"operator"->1|>},"LiftedGraphCertificates"->{<|"operator"->1|>},
 "VertexCertificates"->{<|"Status"->"exact fixture witness"|>}|>;
 If[kind==="bad certificate",proposal["NumericalCoefficientFit"]=True];
 If[kind==="wrong scope",proposal["SourceModelSHA256"]="foreign"];
 settings=Join[<|"Output"->"Details","ProgressReporting"->False,"PrintReport"->False,
 "Parallelization"->False,"RandomSeed"->123457,"TimeConstraint"->20,
 "FeynmanModel"->If[benchmark,cvFeynman,None],
 "PreparationOptions"->{"MaxSteps"->0},"ChannelPreparationOptions"->{"MaxSteps"->0},
 "FinalPreparationOptions"->{"MaxSteps"->0}|>,extra];
 answer=Internal`InheritedBlock[{CreateDiagrams,DiagramAmplitudes,PrepareAmplitudes,FactorizationAmplitudeChannels,
 CompleteFactorization,CheckAmplitudes,HEPCATCovariantCurrentSources`Private`covariantCurrentSource,
 HEPCATCovariantContactCalibration`Private`calibratedCovariantSource,
 ConstructiveDiagrams`Private`constructPreparationUsableQ,ConstructiveDiagrams`Private`constructAllChecksPassQ,
 ConstructiveDiagrams`Private`completionNormalizeInput,ConstructiveDiagrams`Private`completionPhysicalDenominatorOnlyQ},
 Unprotect[CreateDiagrams,DiagramAmplitudes,PrepareAmplitudes,FactorizationAmplitudeChannels,CompleteFactorization,CheckAmplitudes];
 Clear[CreateDiagrams,DiagramAmplitudes,PrepareAmplitudes,FactorizationAmplitudeChannels,CompleteFactorization,CheckAmplitudes,
 HEPCATCovariantCurrentSources`Private`covariantCurrentSource,HEPCATCovariantContactCalibration`Private`calibratedCovariantSource,
 ConstructiveDiagrams`Private`constructPreparationUsableQ,ConstructiveDiagrams`Private`constructAllChecksPassQ,
 ConstructiveDiagrams`Private`completionNormalizeInput,ConstructiveDiagrams`Private`completionPhysicalDenominatorOnlyQ];
 CreateDiagrams[_,m_]:={<|"Backend"->If[Head[m]===FeynmanModel,"benchmark","constructive"]|>};
 DiagramAmplitudes[d_]:={{external,If[First[d]["Backend"]==="benchmark",17,11]}};
 FactorizationAmplitudeChannels[___]:=cvChannels;
 PrepareAmplitudes[input_,model_,opts___Rule]:=Module[{pair,oo=Association[{opts}]},
  AppendTo[cvPreparations,<|"Input"->input,"Options"->oo|>];
  pair=Which[MatchQ[input,{{__Rule},_}],input,MatchQ[input,{{{__Rule},_}}],First[input],True,{external,13}];
  <|"Status"->"PREPARED","OriginalAmplitudes"->input,"PreparedAmplitudes"->{pair}|>];
 ConstructiveDiagrams`Private`constructPreparationUsableQ[_]:=True;
 HEPCATCovariantCurrentSources`Private`covariantCurrentSource[___]:=(cvSources++;If[kind==="unsupported",Failure["unsupported",<||>],proposal]);
 HEPCATCovariantContactCalibration`Private`calibratedCovariantSource[___]:=(cvSources++;proposal);
 CompleteFactorization[args___]:=(cvFits++;cvFitInput=First[{args}];Failure["OriginalCompletionPath",<||>]);
 CheckAmplitudes[pair_,channels_,model_,opts___Rule]:=Module[{oo=Association[{opts}],status},
  AppendTo[cvChecks,<|"Pair"->pair,"Channels"->channels,"Options"->oo|>];
  status=If[kind==="failed validation","FAIL","PASS"];
  <|"Checks"->{<|"Status"->status|>},"Table"->Dataset[{<|"Status"->status|>}],"Summary"-><|status->1|>|>];
 ConstructiveDiagrams`Private`constructAllChecksPassQ[report_,___]:=AllTrue[report["Checks"],#["Status"]==="PASS"&];
 ConstructiveDiagrams`Private`completionNormalizeInput[___]:=<|"Targets"->{}|>;
 ConstructiveDiagrams`Private`completionPhysicalDenominatorOnlyQ[___]:=True;
 ConstructAmplitude[process,cvModel,cvLower,Sequence@@Normal[settings]]];
 <|"Result"->answer,"Sources"->cvSources,"Fits"->cvFits,"Checks"->cvChecks,"Preparations"->cvPreparations,
 "FitInput"->If[cvFits>0,cvFitInput,None]|>
];
cvGood=cvRun[cvCase,"good"];
check["a certified source is independently checked before numerical completion",
 cvGood["Result"]["Status"]==="COMPLETE"&&cvGood["Sources"]===1&&cvGood["Fits"]===0&&cvGood["Result"]["Amplitude"]===3];
check["the source is never used as its independent benchmark",
 AllTrue[cvGood["Checks"],#["Pair"][[2]]===3&&#["Options"]["FeynmanAmplitudes"][[1,2]]===17&&#["Channels"]===cvChannels&]];
check["default distinct independent masses receive all three standard assignments plus equal masses",
 Length[cvGood["Checks"]]===4&&Length[cvGood["Result"]["RepresentativeSelection"]["ValidationReports"]]===4&&
 With[{rules=ConstructiveDiagrams`Private`residueParameterRules[cvLower,Last[cvGood["Checks"]]["Options"]["ParameterRules"]]},
  (ModelParameter[cvLower,"Me"]/.rules)===(ModelParameter[cvLower,"Mm"]/.rules)]];
check["source preparation is final external-only preparation",
 AnyTrue[cvGood["Preparations"],#["Input"]==={cvRules[cvCase],3}&&#["Options"]["InternalOnShell"]===False&]];
check["details preserve original channels and the unvalidated source certificates",
 cvGood["Result"]["Stages"]["Generating factorization channels"]===cvChannels&&
 cvGood["Result"]["Stages"]["Covariant source proposal"]["Status"]==="UNVALIDATED_CANDIDATE"];
cvNoBenchmark=cvRun[cvCase,"good",<||>,False];
check["without an independent benchmark the old completion path stays authoritative",
 cvNoBenchmark["Sources"]===0&&cvNoBenchmark["Fits"]===1];
cvFour=cvRun[{"e","E","h","h"},"good"];
check["four-point processes never enter the new source stage",cvFour["Sources"]===0&&cvFour["Fits"]===1];
cvPhotons=cvRun[{"e","E","A.+","A.+","A.+"},"good"];
check["QED photon processes retain the old completion path",cvPhotons["Sources"]===0&&cvPhotons["Fits"]===1];
cvBad=cvRun[cvCase,"bad certificate"];
check["a numerical-fit certificate cannot authorize source validation",cvBad["Sources"]===1&&cvBad["Fits"]===1&&cvBad["Checks"]==={}];
cvForeign=cvRun[cvCase,"wrong scope"];
check["a foreign model hash cannot authorize source validation",cvForeign["Fits"]===1&&cvForeign["Checks"]==={}];
cvUnsupported=cvRun[cvCase,"unsupported"];
check["unsupported sources fall through to the original prepared diagrams",
 cvUnsupported["Fits"]===1&&cvUnsupported["FitInput"]["PreparedAmplitudes"]==={{cvRules[cvCase],11}}];
cvRejected=cvRun[cvCase,"failed validation"];
check["a failed source restores the previous pair selection and check report",
 cvRejected["Fits"]===1&&cvRejected["Result"]["CandidateAmplitude"]===Missing["NotConstructed"]&&
 cvRejected["Result"]["Checks"]===Missing["NotRun"]&&cvRejected["Result"]["RepresentativeSelection"]===<|"Attempted"->False,"Selected"->False|>];
check["a failed source retains its actual validation evidence without changing the original targets",
 Length[cvRejected["Result"]["RepresentativeSelectionAttempts"]]===1&&
 cvRejected["Result"]["RepresentativeSelectionAttempts"][[1,"Selected"]]===False&&
 cvRejected["Result"]["Stages"]["Generating factorization channels"]===cvChannels];
cvFixed=cvRun[cvCase,"good",<|"ParameterRules"->{"Me"->1,"Mm"->2}|>];
check["explicit parameter inputs do not acquire an additional default equal-mass probe",
 cvFixed["Result"]["Stages"]["Covariant source validation policy"]["AdditionalResolvedParameterRules"]==={}];
cvValidation=cvRun[cvCase,"good",<|"HighEnergyOptions"->{"ValidationParameterRules"->{{"Me"->1,"Mm"->2}}}|>];
check["explicit validation assignments preserve the existing base-plus-requested validation policy",
 Length[cvValidation["Checks"]]===2&&
 Lookup[cvValidation["Result"]["RepresentativeSelection"]["ValidationReports"],"ParameterRules"]===
 ConstructiveDiagrams`Private`completionValidationRuleSets[cvLower,Automatic,{{"Me"->1,"Mm"->2}}]&&
 cvValidation["Result"]["Stages"]["Covariant source validation policy"]["AdditionalResolvedParameterRules"]==={}];
cvContact=cvRun[{"e","E","Z","h","h"},"good"];
check["the calibrated-contact class uses the same independent three-assignment acceptance",
 cvContact["Result"]["Status"]==="COMPLETE"&&cvContact["Sources"]===1&&Length[cvContact["Checks"]]===3];
check["controlled dispatch probes restore all existing production and source definitions",
 cvOriginalDefinitions==={DownValues[ConstructAmplitude],DownValues[CheckAmplitudes],DownValues[CompleteFactorization],
 DownValues[PrepareAmplitudes],DownValues[HEPCATCovariantCurrentSources`Private`covariantCurrentSource]}];
If[checks=!=17,AppendTo[failures,"Not every constructor-source assertion executed"]];
Print["COVARIANT CONSTRUCTOR CHECKS: ",checks,"; FAILURES: ",Length[failures]];
If[failures=!={},Print[failures]];
If[failures==={},Print["HEPCAT_TEST_COMPLETED:covariant-constructor-tests.wl"]];
Quit[If[failures==={},0,1]];
