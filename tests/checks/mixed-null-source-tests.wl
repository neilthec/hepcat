(* Fast dispatch, metadata and native-reference unit controls for mixed-null
   current sources. This suite does not calibrate contacts, construct a real
   five-point amplitude, read a saved answer, or certify a physics result.
   HEPCAT_TEST_SOURCE_ENTRYPOINT and HEPCAT_TEST_DATA_ROOT are optional. *)
HEPCATMixedNullRegressionSetup`root=Replace[Environment["HEPCAT_TEST_DATA_ROOT"],Except[_String?(StringLength[#]>0&)]->DirectoryName[DirectoryName[DirectoryName[$InputFileName]]]];
HEPCATMixedNullRegressionSetup`entry=Replace[Environment["HEPCAT_TEST_SOURCE_ENTRYPOINT"],Except[_String?(StringLength[#]>0&)]->FileNameJoin[{HEPCATMixedNullRegressionSetup`root,"source","HEPCAT.wl"}]];
HEPCATMixedNullRegressionSetup`messages=CreateTemporary[];
HEPCATMixedNullRegressionSetup`stream=OpenWrite[HEPCATMixedNullRegressionSetup`messages];
Global`$HEPCATpath=DirectoryName[ExpandFileName[HEPCATMixedNullRegressionSetup`entry]];
Block[{$Messages={HEPCATMixedNullRegressionSetup`stream}},Get[HEPCATMixedNullRegressionSetup`entry]];
Close[HEPCATMixedNullRegressionSetup`stream];
Begin["HEPCATMixedNullRegression`Private`"];
$HistoryLength=0;root=HEPCATMixedNullRegressionSetup`root;
heavyHeads={HEPCATMasslessVectorCalibration`Private`calibrate,HEPCATMasslessVectorIntegration`Private`calibrateAndVerify,HEPCATMasslessVectorIntegration`Private`candidate,HEPCATMasslessVectorIntegration`Private`buildCandidate};
fields[symbol_]:={DownValues[symbol],OwnValues[symbol],SubValues[symbol],UpValues[symbol],NValues[symbol],Options[symbol],Attributes[symbol],DefaultValues[symbol],FormatValues[symbol],Messages[symbol]};
heavyBefore=fields/@heavyHeads;heavyCalls=0;
bodyResult=Internal`InheritedBlock[{HEPCATMasslessVectorCalibration`Private`calibrate,HEPCATMasslessVectorIntegration`Private`calibrateAndVerify,HEPCATMasslessVectorIntegration`Private`candidate,HEPCATMasslessVectorIntegration`Private`buildCandidate},
 Clear[HEPCATMasslessVectorCalibration`Private`calibrate,HEPCATMasslessVectorIntegration`Private`calibrateAndVerify,HEPCATMasslessVectorIntegration`Private`candidate,HEPCATMasslessVectorIntegration`Private`buildCandidate];
 HEPCATMasslessVectorCalibration`Private`calibrate[___]:=(heavyCalls++;Failure["UnexpectedExpensiveCalibrationInUnitSuite",<||>]);
 HEPCATMasslessVectorIntegration`Private`calibrateAndVerify[___]:=(heavyCalls++;Failure["UnexpectedExpensiveCalibrationInUnitSuite",<||>]);
 HEPCATMasslessVectorIntegration`Private`candidate[___]:=(heavyCalls++;Failure["UnexpectedExpensiveGraphBuildInUnitSuite",<||>]);
 HEPCATMasslessVectorIntegration`Private`buildCandidate[___]:=(heavyCalls++;Failure["UnexpectedExpensiveGraphBuildInUnitSuite",<||>]);
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
 HEPCATCovariantCurrentSources`Private`covariantCurrentSource[___]:=(cvSources++;If[MemberQ[{"unsupported","unsupported used quartic","unsupported used operator"},kind],Failure[Switch[kind,"unsupported used quartic","UnsupportedUsedQuartic","unsupported used operator","UnsupportedUsedOperator",_,"unsupported"],<|"FixtureScope"->"StubbedProviderRefusal"|>],proposal]);
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
cvTwoVectors=cvRun[{"e","E","Z","Z","h"},"good"];
check["two massive vectors and one scalar use the same certified metric-current route",
 cvTwoVectors["Result"]["Status"]==="COMPLETE"&&cvTwoVectors["Sources"]===1&&cvTwoVectors["Fits"]===0];
cvFourVectors=cvRun[{"Z","Z","Z","Z","h"},"good"];
check["four massive vectors and one scalar use the same certified metric-current route",
 cvFourVectors["Result"]["Status"]==="COMPLETE"&&cvFourVectors["Sources"]===1&&cvFourVectors["Fits"]===0&&Length[cvFourVectors["Checks"]]===3];
cvMasslessVector=cvRun[{"e","E","Z","A.+","h"},"good"];
check["the C13 mixed massless-vector 2V1S class uses its certified cubic provider",
 cvMasslessVector["Sources"]===1&&cvMasslessVector["Fits"]===0&&cvMasslessVector["Result"]["Status"]==="COMPLETE"];
cvPhotons=cvRun[{"e","E","A.+","A.+","A.+"},"good"];
check["QED photon processes retain the old completion path",cvPhotons["Sources"]===0&&cvPhotons["Fits"]===1];
cvSix=cvRun[{"e","E","A.+","A.+","A.+","A.-"},"good"];
check["QED six-point input never enters the five-point source provider",
 cvSix["Sources"]===0&&cvSix["Fits"]===1&&cvSix["FitInput"]["PreparedAmplitudes"]==={{cvRules[{"e","E","A.+","A.+","A.+","A.-"}],11}}];
cvBad=cvRun[cvCase,"bad certificate"];
check["a numerical-fit certificate cannot authorize source validation",cvBad["Sources"]===1&&cvBad["Fits"]===1&&cvBad["Checks"]==={}];
cvForeign=cvRun[cvCase,"wrong scope"];
check["a foreign model hash cannot authorize source validation",cvForeign["Fits"]===1&&cvForeign["Checks"]==={}];
cvUnsupported=cvRun[cvCase,"unsupported"];
check["unsupported sources fall through to the original prepared diagrams",
 cvUnsupported["Fits"]===1&&cvUnsupported["FitInput"]["PreparedAmplitudes"]==={{cvRules[cvCase],11}}];
cvUnsupportedQuartic=cvRun[cvCase,"unsupported used quartic"];
check["a provider refusal for an unsupported used quartic preserves the ordinary fallback and original targets",
 cvUnsupportedQuartic["Sources"]===1&&cvUnsupportedQuartic["Fits"]===1&&cvUnsupportedQuartic["Checks"]==={}&&
 cvUnsupportedQuartic["FitInput"]["PreparedAmplitudes"]==={{cvRules[cvCase],11}}&&
 cvUnsupportedQuartic["Result"]["Stages"]["Generating factorization channels"]===cvChannels];
cvUnsupportedOperator=cvRun[cvCase,"unsupported used operator"];
check["a provider refusal for an unsupported used operator preserves the ordinary fallback and original targets",
 cvUnsupportedOperator["Sources"]===1&&cvUnsupportedOperator["Fits"]===1&&cvUnsupportedOperator["Checks"]==={}&&
 cvUnsupportedOperator["FitInput"]["PreparedAmplitudes"]==={{cvRules[cvCase],11}}&&
 cvUnsupportedOperator["Result"]["Stages"]["Generating factorization channels"]===cvChannels];
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

check["accepted early sources never prepare unused ordinary diagrams or channels",
 !AnyTrue[cvGood["Preparations"],MemberQ[{ {{cvRules[cvCase],11}}, cvChannels}, #["Input"]]&]];
check["early source starts only after original raw diagrams and channels exist",
 With[{stages=Lookup[cvGood["Result"]["StageTimings"],"Stage",{}]},
  FirstPosition[stages,"Generating diagrams"][[1]]<FirstPosition[stages,"Generating factorization channels"][[1]]<
  FirstPosition[stages,"Building certified covariant source"][[1]]]];
check["rejected early source resumes ordinary diagram and channel preparation",
 KeyExistsQ[cvRejected["Result"]["Stages"],"Preparing diagrams"]&&KeyExistsQ[cvRejected["Result"]["Stages"],"Preparing factorization channels"]&&cvRejected["Fits"]===1];
check["existing QED5 guard preserves preparation before channels",
 With[{stages=Lookup[cvPhotons["Result"]["StageTimings"],"Stage",{}]},
  FirstPosition[stages,"Preparing diagrams"][[1]]<FirstPosition[stages,"Generating factorization channels"][[1]]]];
cvQED=ReadModel[FileNameJoin[{root,"models"}],3];
cvQEDRows=Select[cvQED[[2]],Length[HEPCATCovariantCurrentSources`Private`rowParticles[#]]===3&];
cvQEDGraphs={<|"VertexStructures"-><|0->(<|"ModelRow"->#|>&/@cvQEDRows)|>|>};
check["early metadata excludes three-photon QED and used-row pure-Abelian four-fermion radiation",
 !AssociationQ[ConstructiveDiagrams`Private`constructCovariantSourceMetadata[{"e","E","A.+","A.+","A.+"},cvQED,cvQEDGraphs,False]]&&
 !AssociationQ[ConstructiveDiagrams`Private`constructCovariantSourceMetadata[{"e","E","E","e","A.+"},cvQED,cvQEDGraphs,False]]];

If[checks=!=28,AppendTo[failures,"Not every constructor-source assertion executed"]];


legacyCount=checks;
model=cvModel;lower=cvLower;modelsBefore={model,lower};
process={"A.+","A.-","W+","W-","h"};
metaClass=ConstructiveDiagrams`Private`constructCovariantSourceMetadata[process,model,{},False];
plan=ConstructiveDiagrams`Private`constructMixedNullPublicPlan[process,model,lower,3];
check["source entrypoint loads without messages",FileByteCount[HEPCATMixedNullRegressionSetup`messages]===0];
check["public constructor is loaded",Length[DownValues[ConstructAmplitude]]>0];
check["two-null metadata enters the typed mixed-null source class",AssociationQ[metaClass]&&metaClass["SourceClass"]==="Calibrated mixed-null metric currents"];
check["one-null contact integration remains outside the public provider",!ConstructiveDiagrams`Private`constructMixedNullPublicClassQ[{"A.+","Z","W-","W+","h"},model]];
check["five vectors remain outside the public mixed-null provider",!ConstructiveDiagrams`Private`constructMixedNullPublicClassQ[{"A.+","A.-","Z","W-","W+"},model]];
check["fermions remain outside the public mixed-null provider",!ConstructiveDiagrams`Private`constructMixedNullPublicClassQ[{"e","E","A.+","Z","Z"},model]];
check["colored null vectors remain outside this colorless provider",!ConstructiveDiagrams`Private`constructMixedNullPublicClassQ[{"G.+","G.-","W-","W+","h"},model]];
check["all-massive source classification stays on its established route",ConstructiveDiagrams`Private`constructCovariantSourceMetadata[{"W+","W-","W+","W-","h"},model,{},False]["SourceClass"]==="Calibrated metric currents"];
check["typed planning retains36coherent components and the existing budget",AssociationQ[plan]&&Lookup[plan["Quartets"],"CoherentComponents"]==={36}&&ConstructiveDiagrams`Private`constructFourVectorSourceBudget[plan,1000]===216];
metadataCount=checks-legacyCount;

pcCalls={};pcProcess={"A.+","A.-","W+","W-","h"};
pcOriginal=Hash[{DownValues[ConstructiveDiagrams`Private`constructMixedNullPublicCandidate],Options[ConstructiveDiagrams`Private`constructMixedNullPublicCandidate],Attributes[ConstructiveDiagrams`Private`constructMixedNullPublicCandidate]},"SHA256"];
pcBoundary=Internal`InheritedBlock[{ConstructiveDiagrams`Private`constructMixedNullPublicCandidate},
 Clear[ConstructiveDiagrams`Private`constructMixedNullPublicCandidate];
 ConstructiveDiagrams`Private`constructMixedNullPublicCandidate[p_List,actualModel_List,actualLower_List,budget_,callback_]:=(AppendTo[pcCalls,<|"Process"->p,"SourceExactSameQ"->(actualModel===cvModel),"LowerExactSameQ"->(actualLower===cvLower),"Budget"->budget,"CallbackHead"->ToString[Head[callback],InputForm]|>];Failure["CanaryAtFreshMixedNullSourceBoundary",<||>]);
 pcWithBenchmark=cvRun[pcProcess,"good"];pcWithoutBenchmark=cvRun[pcProcess,"good",<||>,False];pcWithBenchmark];
pcAfter=Hash[{DownValues[ConstructiveDiagrams`Private`constructMixedNullPublicCandidate],Options[ConstructiveDiagrams`Private`constructMixedNullPublicCandidate],Attributes[ConstructiveDiagrams`Private`constructMixedNullPublicCandidate]},"SHA256"];
check["actual public constructor reaches the typed source boundary exactly once",Length[pcCalls]===1&&First[pcCalls]["Process"]===pcProcess&&First[pcCalls]["SourceExactSameQ"]===True&&First[pcCalls]["LowerExactSameQ"]===True];
check["typed source budget is capped by the original small parent budget",Length[pcCalls]===1&&0<First[pcCalls]["Budget"]<=20];
check["the mixed-null source is unavailable without an independent benchmark",Length[pcCalls]===1&&pcWithoutBenchmark["Fits"]===1&&pcWithoutBenchmark["Checks"]==={}];
check["distinctive provider refusal remains visible and does not bypass ordinary fallback",pcBoundary["Result"]["Status"]==="FAILED"&&pcBoundary["Result"]["Stages"]["Mixed-null vector source proposal"]===Failure["CanaryAtFreshMixedNullSourceBoundary",<||>]&&pcBoundary["Fits"]===1&&pcBoundary["Checks"]==={}];
check["original raw channels and diagram input survive the typed refusal",pcBoundary["Result"]["Stages"]["Generating factorization channels"]===cvChannels&&pcBoundary["FitInput"]["PreparedAmplitudes"]==={{cvRules[pcProcess],11}}];
check["new source boundary is reached before unused preparation",With[{stages=Lookup[pcBoundary["Result"]["StageTimings"],"Stage",{}]},FirstPosition[stages,"Generating factorization channels"][[1]]<FirstPosition[stages,"Building mixed-null lower-derived vector source"][[1]]<FirstPosition[stages,"Preparing diagrams"][[1]]]];
check["public source definitions are restored after the boundary canary",pcOriginal===pcAfter];

boundaryCount=checks-legacyCount-metadataCount;
external=Thread[(Mass/@Range[5])->ConstructiveDiagrams`Private`constructExternalMasses[process,model]];
rows=HEPCATCovariantCurrentSources`Private`particleRow[#,model]&/@process;
wave[leg_]:=PolarizationVector[leg,LorentzIndex[leg],rows[[leg,6]],If[rows[[leg,6]]===0,If[process[[leg]]===rows[[leg,2]],1,-1],Automatic]];
raw=LorentzMetric[LorentzIndex[1],LorentzIndex[2]]LorentzMetric[LorentzIndex[3],LorentzIndex[4]]Times@@Table[wave[i],{i,4}];
pairs={{external,raw},{external,2raw}};before=HEPCATCovariantContactCalibration`Private`coherentConversionNativeFields[];
conversion=HEPCATCovariantContactCalibration`Private`coherentSourceConversion[pairs,process,model,10];
certificate=ConstructiveDiagrams`Private`constructMixedNullPublicReferenceCertificate[<|"SourceConversion"->conversion|>,process,model];
wrongReference=ReplacePart[conversion,"ConvertedPairs"->(conversion["ConvertedPairs"]/.Spinor["Xi","Angle",1]->Spinor["Xi","Angle",2])];
wrongChirality=ReplacePart[conversion,"ConvertedPairs"->(conversion["ConvertedPairs"]/.Spinor["Xi","Angle",1]->Spinor["Xi","Square",1])];
referenceChecks=<|"OneCompletePairListConvertedWithNativeDefinitions"->(AssociationQ[conversion]&&conversion["Status"]==="COMPLETE"&&conversion["CompleteRawPairOrderRetained"]===True),
 "SameExternalReferenceUsedAcrossBothLorentzMonomials"->(AssociationQ[certificate]&&certificate["Status"]==="SHARED_PER_EXTERNAL_LEG_REFERENCE_ASSIGNMENT"&&Sort[certificate["ActualReferences"]]===Sort[{Spinor["Xi","Angle",1],Spinor["Xi","Square",2]}]),
 "AnotherLegsReferenceCannotMasqueradeAsSharedAssignment"->FailureQ[ConstructiveDiagrams`Private`constructMixedNullPublicReferenceCertificate[<|"SourceConversion"->wrongReference|>,process,model]],
 "WrongReferenceChiralityRejected"->FailureQ[ConstructiveDiagrams`Private`constructMixedNullPublicReferenceCertificate[<|"SourceConversion"->wrongChirality|>,process,model]],
 "NativeConverterAllTenFieldsRestored"->(before===HEPCATCovariantContactCalibration`Private`coherentConversionNativeFields[])
|>;
sourceFixture=<|"SourceConversion"->conversion,"RawSourceLorentzPairs"->pairs,"ExternalParticles"->process,"ExternalMassRules"->external,"InternalOnShellConditions"->{},"CoherentExternalVectorEncoding"-><|"OffShellKinematicsChanged"->False|>|>;
binding=ConstructiveDiagrams`Private`constructMixedNullPublicExternalBinding[sourceFixture,process,model];
AssociateTo[referenceChecks,{"NativeExternalMassAndCrossingBindingCertified"->AssociationQ[binding],
 "InternalMassShellRuleCannotEnterFinalSourceHeaders"->FailureQ[ConstructiveDiagrams`Private`constructMixedNullPublicExternalBinding[Join[sourceFixture,<|"RawSourceLorentzPairs"->({Append[First[#],Mass[Multiparticle[1,2]]->1],Last[#]}&/@pairs)|>],process,model]],
 "ChangedExternalOrderingCannotPassMassAndCrossingBinding"->FailureQ[ConstructiveDiagrams`Private`constructMixedNullPublicExternalBinding[sourceFixture,Reverse[process],model]],
 "InternalOnShellAssumptionIsRejected"->FailureQ[ConstructiveDiagrams`Private`constructMixedNullPublicExternalBinding[Join[sourceFixture,<|"InternalOnShellConditions"->{Mass[Multiparticle[1,2]]->1}|>],process,model]],
 "ChangedOffShellKinematicsFlagIsRejected"->FailureQ[ConstructiveDiagrams`Private`constructMixedNullPublicExternalBinding[Join[sourceFixture,<|"CoherentExternalVectorEncoding"-><|"OffShellKinematicsChanged"->True|>|>],process,model]]}];

KeyValueMap[check[#1,#2]&,referenceChecks];
referenceCount=Length[referenceChecks];

(* Synthetic ordered graph fixture only. Real graphUnion bookkeeping runs;
   tensor-calibration and primitive graph-certificate providers are scoped stubs. *)
originalGraph=<|"InternalVertices"->{0,1},"ControlSignature"-><|"OrderedPorts"->{1,2,3,4,5},"Kind"->"original"|>,"ControlUsedRows"->{}|>;
entry=First[plan["Quartets"]];attachment=First[entry["Attachments"]];cubicRow=attachment["CubicRow"];
aliases=HEPCATMasslessVectorIntegration`Private`declaredAliases[model];
physical=Lookup[aliases,#,#]&/@entry["Particles"];
contactRow=Join[physical,{1,0}];
controlCalibration=<|"ControlFixtureTag"->"SYNTHETIC_NO_LOWER_PROOF","Metadata"-><|"NullLegs"->{1,2}|>,"AllCoherentResiduals"->ConstantArray[0,36]|>;
contact=<|"Class"->"VectorVectorVectorVector","Particles"->physical,"LiftedRow"->contactRow,"LowerAmplitudeCalibration"->controlCalibration,"CertificateID"->"control-contact"|>;
cubic=<|"Class"->"ControlCubic","LiftedRow"->cubicRow,"SourceRows"->{cubicRow},"CertificateID"->"control-cubic"|>;
catalog=<|"Certificates"->{contact,cubic},"Aliases"->aliases|>;
addedGraph=<|"InternalVertices"->{0,1},"ControlSignature"-><|"OrderedPorts"->{1,2,3,4,5},"Kind"->"derived-contact"|>,"ControlUsedRows"->{contactRow,cubicRow}|>;
original={originalGraph};lifted={originalGraph,addedGraph};fixtureHash=Hash[{original,lifted,catalog,entry},"SHA256"];
graphChecks=Internal`InheritedBlock[{HEPCATCovariantCurrentSources`Private`diagramCertificate,HEPCATCovariantCurrentSources`Private`usedRows,HEPCATMasslessVectorIntegration`Private`completeContactQ,ConstructiveDiagrams`Private`constructMixedNullPublicReferenceCertificate,ConstructiveDiagrams`Private`constructMixedNullPublicExternalBinding},
 Clear[HEPCATCovariantCurrentSources`Private`diagramCertificate,HEPCATCovariantCurrentSources`Private`usedRows,HEPCATMasslessVectorIntegration`Private`completeContactQ,ConstructiveDiagrams`Private`constructMixedNullPublicReferenceCertificate,ConstructiveDiagrams`Private`constructMixedNullPublicExternalBinding];
 HEPCATCovariantCurrentSources`Private`diagramCertificate[g_Association,___]:=g["ControlSignature"];
 HEPCATCovariantCurrentSources`Private`usedRows[g_List]:=DeleteDuplicates[Flatten[Lookup[g,"ControlUsedRows",{}],1]];
 HEPCATMasslessVectorIntegration`Private`completeContactQ[c_Association,___]:=Lookup[c,"ControlFixtureTag",None]==="SYNTHETIC_NO_LOWER_PROOF";
 referenceFixture=<|"Status"->"STUBBED_REFERENCE_BOUNDARY_ONLY"|>;externalFixture=<|"Status"->"STUBBED_EXTERNAL_BOUNDARY_ONLY"|>;
 ConstructiveDiagrams`Private`constructMixedNullPublicReferenceCertificate[___]:=referenceFixture;
 ConstructiveDiagrams`Private`constructMixedNullPublicExternalBinding[___]:=externalFixture;
 union=HEPCATMasslessVectorIntegration`Private`graphUnion[original,lifted,catalog,model,<|"Quartets"->{entry}|>,lower];
 assembly=<|"Catalog"->catalog,"Diagrams"->lifted|>;
 proposal=<|"ExternalParticles"->process,"SourceModelSHA256"->HEPCATCovariantCurrentSources`Private`objectSHA[model],"LowerModelSHA256"->HEPCATCovariantCurrentSources`Private`objectSHA[lower],"OrderedGraphAssembly"->assembly,"FourVectorAugmentationPlan"-><|"Quartets"->{entry}|>,"SourceGraphUnion"->union,"VertexCertificates"->catalog["Certificates"],"OriginalDiagramCount"->Length[original],"LiftedDiagramCount"->Length[lifted],"OrderedGraphCertificates"->Lookup[union,"OriginalGraphCertificates",None],"LiftedGraphCertificates"->Lookup[union,"LiftedGraphCertificates",None],"MasslessReferenceAssignmentCertificate"->referenceFixture,"ExternalLegMassAndCrossingCertificate"->externalFixture,"InternalOnShellConditions"->{},"NumericalParameterAssignments"->{},"NewIndependentContacts"->False,"NoSavedCalibrationInput"->True|>;
 unsupported=Join[entry,<|"Attachments"->{<|"CubicRow"->{"undeclared","attachment","row",1,0}|>}|>];
 unsupportedAttachmentRejected=FailureQ[HEPCATMasslessVectorIntegration`Private`graphUnion[original,lifted,catalog,model,<|"Quartets"->{unsupported}|>,lower]];
 orderedMultiplicityRejected=FailureQ[HEPCATMasslessVectorIntegration`Private`graphUnion[{originalGraph,originalGraph},lifted,catalog,model,<|"Quartets"->{entry}|>,lower]];
 <|"synthetic ordered fixture passes only the isolated graph gate"->ConstructiveDiagrams`Private`constructMixedNullPublicGraphCoverageQ[proposal,original,model,lower],
 "dropping an original graph cannot pass"->(!ConstructiveDiagrams`Private`constructMixedNullPublicGraphCoverageQ[proposal,{},model,lower]),
 "foreign source owner cannot pass"->(!ConstructiveDiagrams`Private`constructMixedNullPublicGraphCoverageQ[Join[proposal,<|"SourceModelSHA256"->"foreign"|>],original,model,lower]),
 "wrong native graph count cannot pass"->(!ConstructiveDiagrams`Private`constructMixedNullPublicGraphCoverageQ[Join[proposal,<|"LiftedDiagramCount"->1|>],original,model,lower]),
 "internal shell condition cannot pass"->(!ConstructiveDiagrams`Private`constructMixedNullPublicGraphCoverageQ[Join[proposal,<|"InternalOnShellConditions"->{Mass[Multiparticle[1,2]]->0}|>],original,model,lower]),
 "source and lower model objects remain unchanged"->(modelsBefore==={model,lower}),
 "synthetic graph fixture remains unchanged"->(fixtureHash===Hash[{original,lifted,catalog,entry},"SHA256"])|>];
KeyValueMap[check[#1,#2]&,graphChecks];
graphCount=Length[graphChecks];
check["undeclared attachment is rejected by real graph-union bookkeeping",unsupportedAttachmentRejected];
check["ordered graph multiplicity is preserved by real graph-union bookkeeping",orderedMultiplicityRejected];

(* Property-based names/roles and small exact operator/frame controls. *)
particleNames=DeleteDuplicates[Flatten[model[[1,All,{2,3}]]]];
renamedNames=MapIndexed[Function[{name,index},If[StringEndsQ[name,".+"],"renamedNull"<>StringDrop[name,-2]<>".+",If[StringEndsQ[name,".-"],"renamedNull"<>StringDrop[name,-2]<>".-","renamedParticle"<>ToString[First[index]]]]],particleNames];
renameRules=Thread[particleNames->renamedNames];
renameModel[input_List]:=Module[{copy=input},copy[[1]]=ReplacePart[#,{2->(#[[2]]/.renameRules),3->(#[[3]]/.renameRules)}]&/@input[[1]];copy[[2]]=(Join[Replace[Drop[#,-2],renameRules,{1}],Take[#,-2]]&/@input[[2]]);copy];
renamedModel=renameModel[model];renamedLower=renameModel[lower];renamedProcess=process/.renameRules;
renamedPlan=ConstructiveDiagrams`Private`constructMixedNullPublicPlan[renamedProcess,renamedModel,renamedLower,3];
check["renaming particles preserves typed public classification",ConstructiveDiagrams`Private`constructMixedNullPublicClassQ[renamedProcess,renamedModel]];
check["renaming particles preserves metadata component counts",AssociationQ[renamedPlan]&&Lookup[renamedPlan["Quartets"],"CoherentComponents"]==={36}];
check["renamed particle and antiparticle endpoints stay distinct",DuplicateFreeQ[renamedNames]&&AllTrue[renamedModel[[1]],If[#[[2]]===#[[3]],True,MemberQ[renamedNames,#[[2]]]&&MemberQ[renamedNames,#[[3]]]]&]];
parts=entry["Particles"];masslessMeta=HEPCATMasslessVectorCalibration`Private`metadata[parts,model,lower];
check["two-null metadata has all4declared words with9components each",Length[masslessMeta["HelicityWords"]]===4&&masslessMeta["CoherentComponentsByWord"]==={9,9,9,9}];
oneNullMeta=HEPCATMasslessVectorCalibration`Private`metadata[{"A.+","Z","W+","W-"},model,lower];
check["one-null lower metadata has2words with27components each without public admission",AssociationQ[oneNullMeta]&&oneNullMeta["CoherentComponentsByWord"]==={27,27}&&!ConstructiveDiagrams`Private`constructMixedNullPublicClassQ[{"A.+","Z","W+","W-","h"},model]];
wrongRoles=lower;roleIndex=First[FirstPosition[wrongRoles[[1,All,2]],"W+"]];wrongRoles[[1,roleIndex,{2,3}]]=Reverse[wrongRoles[[1,roleIndex,{2,3}]]];
check["reversing a declared lower endpoint role is rejected",FailureQ[HEPCATMasslessVectorCalibration`Private`metadata[parts,model,wrongRoles]]];
external4=masslessMeta["ExternalMassRules"];
check["typed covariant scalar guard accepts a scalar unit",TrueQ[HEPCATMasslessVectorCalibration`Private`covariantInputs[1,1,{1,2,3},external4]["AllTypedCovariantScalars"]]];
check["typed covariant scalar guard rejects a free momentum",!TrueQ[HEPCATMasslessVectorCalibration`Private`covariantInputs[Mom[1],1,{1,2,3},external4]["AllTypedCovariantScalars"]]];
nullWitness=HEPCATMasslessVectorCalibration`Private`nullFrame[{{2,4},{3,6}}];
check["rank-one null frame reconstructs exactly without division by mass",AssociationQ[nullWitness]&&nullWitness["ExactOuterProductRemainder"]==={{0,0},{0,0}}&&nullWitness["DivisionByDeclaredMasslessMass"]===False];
check["zero null momentum cannot obtain a frame",FailureQ[HEPCATMasslessVectorCalibration`Private`nullFrame[ConstantArray[0,{2,2}]]]];
metricBasis={LorentzMetric[LorentzIndex[1],LorentzIndex[2]]LorentzMetric[LorentzIndex[3],LorentzIndex[4]],LorentzMetric[LorentzIndex[1],LorentzIndex[3]]LorentzMetric[LorentzIndex[2],LorentzIndex[4]],LorentzMetric[LorentzIndex[1],LorentzIndex[4]]LorentzMetric[LorentzIndex[2],LorentzIndex[3]]};
boseProof=HEPCATMasslessVectorIntegration`Private`boseTensorProof[{"charged1","neutral","charged2","neutral"},Total[metricBasis],model,2];
check["arbitrary identical-particle slots transport all3metric pairings",AssociationQ[boseProof]&&boseProof["Generators"]==={{2,4}}&&boseProof["Proofs"][[1,"MetricPairingPermutation"]]==={3,2,1}];
check["a nonsymmetric metric tensor fails the actual Bose proof",FailureQ[HEPCATMasslessVectorIntegration`Private`boseTensorProof[{"charged1","neutral","charged2","neutral"},First[metricBasis],model,2]]];
publicNames[ctx_String]:=Select[Names[ctx<>"*"],StringCount[#,"`"]===StringCount[ctx,"`"]&];
check["new calibration module exposes no public functions",publicNames["HEPCATMasslessVectorCalibration`"]==={}];
check["new integration module exposes no public functions",publicNames["HEPCATMasslessVectorIntegration`"]==={}];
check["new public bridge helpers stay in package Private context",AllTrue[Names["ConstructiveDiagrams`*MixedNull*"],StringContainsQ[#,"`Private`"]&]];
check["no expensive calibrator or full graph builder was invoked",heavyCalls===0];
subsetCounts=<|"ExistingConstructorControls"->legacyCount,"MetadataControls"->metadataCount,"BoundaryControls"->7,"NativeReferenceControls"->referenceCount,"GraphAdmissionControls"->graphCount,"BaseTotal"->legacyCount+metadataCount+7+referenceCount+graphCount|>;
check["all61base controls remain represented",subsetCounts["BaseTotal"]===61];
Null];
check["all expensive-provider definition fields restored after unit stubs",heavyBefore===(fields/@heavyHeads)];
output=Environment["HEPCAT_TEST_OUTPUT"];
report=<|"Status"->If[failures==={},"PASS","FAIL"],"Checks"->checks,"Failures"->failures,"Subsets"->subsetCounts,"NoCalibrationOrRealFivePointConstruction"->True,"NoSavedAmplitudeOrResearchCapsuleInput"->True,"SyntheticGraphProofProvidersStubbed"->True,"NativeReferenceConversionOnly"->True,"Scope"->"UnitAndDispatchControlsNotPhysicsAdmission"|>;
If[StringQ[output]&&StringLength[output]>0,If[!DirectoryQ[output],CreateDirectory[output,CreateIntermediateDirectories->True]];Export[FileNameJoin[{output,"mixed-null-source-controls.json"}],report,"RawJSON"]];
Print["MIXED-NULL SOURCE CHECKS: ",checks,"; FAILURES: ",Length[failures]];
If[failures=!={},Print[failures]];
If[failures==={},Print["HEPCAT_TEST_COMPLETED:mixed-null-source-tests.wl"]];
End[];
Quit[If[HEPCATMixedNullRegression`Private`failures==={},0,1]];
