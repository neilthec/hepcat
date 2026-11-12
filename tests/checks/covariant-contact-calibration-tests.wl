(* Portable exact lower-amplitude contact matching. No saved WXF or reference model. *)
root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root,"source","HEPCAT.wl"}]];
Get[FileNameJoin[{root,"tests","SM-5-point","covariant-current-sources.wl"}]];
checks=0;failures={};$HistoryLength=0;
SetAttributes[check,HoldRest];check[name_,condition_]:=(checks++;If[!TrueQ[condition],AppendTo[failures,name];Print["FAIL: ",name]]);
Global`metricContactCalibration[___]:=Failure["AmbientCollision",<||>];
Global`parameterTransport="ambient parameter transport";Global`rowParticles="ambient row helper";
qcAmbientBefore={DownValues[Global`metricContactCalibration],OwnValues[Global`parameterTransport],OwnValues[Global`rowParticles]};
qcNames=Sort[DeleteDuplicates[Join[Names["ConstructiveDiagrams`*"],Names["ConstructiveDiagrams`Private`*"]]]];
qcDefinitions=ToExpression[#,InputForm,Function[s,{DownValues[Unevaluated[s]],UpValues[Unevaluated[s]],SubValues[Unevaluated[s]],OwnValues[Unevaluated[s]],Options[Unevaluated[s]],Attributes[Unevaluated[s]],Messages[Unevaluated[s]]},HoldAllComplete]]&/@qcNames;
Get[FileNameJoin[{root,"tests","SM-5-point","covariant-contact-calibration.wl"}]];
check["loading the helper preserves ambient names and every existing package definition",
 qcAmbientBefore==={DownValues[Global`metricContactCalibration],OwnValues[Global`parameterTransport],OwnValues[Global`rowParticles]}&&
 qcDefinitions===(ToExpression[#,InputForm,Function[s,{DownValues[Unevaluated[s]],UpValues[Unevaluated[s]],SubValues[Unevaluated[s]],OwnValues[Unevaluated[s]],Options[Unevaluated[s]],Attributes[Unevaluated[s]],Messages[Unevaluated[s]]},HoldAllComplete]]&/@qcNames)];
qcCalibrate=HEPCATCovariantContactCalibration`Private`metricContactCalibration;
qcTransport=HEPCATCovariantContactCalibration`Private`parameterTransport;
qcCoherent=HEPCATCovariantContactCalibration`Private`coherent;
qcToy[scope_String]:=Module[{parameters,mv,ms,g,h3,bare,known,particles,unit3,unit4,source,cubic,graphs,pairs,full,lowerRules,lower},
 parameters=ModelParameter[#,scope,"Real"]&/@{"vectorMass","scalarMass","vectorScalar","scalarCubic","bareMetric","knownMetric"};
 {mv,ms,g,h3,bare,known}=parameters;
 particles={{"scalar","phi","phi",1,0,ms,0,1,"","phi","phi"},{"vector","V","V",2,2,mv,0,1,"G","V","V"}};
 unit3=qcCoherent[FeynmanToSpinorProducts[LorentzMetric[LorentzIndex[1],LorentzIndex[2]]PolarizationVector[1,LorentzIndex[1],mv,Automatic]PolarizationVector[2,LorentzIndex[2],mv,Automatic],"ReduceSpinorProducts"->False]];
 unit4=qcCoherent[FeynmanToSpinorProducts[LorentzMetric[LorentzIndex[3],LorentzIndex[4]]PolarizationVector[3,LorentzIndex[3],mv,Automatic]PolarizationVector[4,LorentzIndex[4],mv,Automatic],"ReduceSpinorProducts"->False]];
 source={particles,{{"V","V","phi","",g,unit3},{"phi","phi","phi","",h3,1},{"phi","phi","V","V",bare,unit4}},
  MapThread[{#1,#2}&,{parameters,{2,3,5/7,7/11,-13/17,19/23}}],{}};
 cubic=FeynmanModel[particles,{{"V","V","phi","",g,LorentzMetric[LorentzIndex[1],LorentzIndex[2]]},{"phi","phi","phi","",h3,1}},source[[3]],source[[4]]];
 graphs=CreateDiagrams[{"phi","phi","V","V"},cubic];pairs=FeynmanToSpinorProducts[DiagramAmplitudes[graphs],"ReduceSpinorProducts"->False];
 full=qcCoherent[Total[Last/@pairs]]+known unit4;
 lowerRules=(#->ModelParameter[ModelParameterName[#],scope<>"-lower","Real"]&)/@parameters;
 lower=ReplacePart[source,2->Append[Select[source[[2]],Length[DeleteCases[Drop[#,-2],""]]===3&],{"phi","phi","V","V",1,full}]]/.lowerRules;
 <|"Source"->source,"Lower"->lower,"Parameters"->parameters,"ScopeMap"->lowerRules,"MetricUnit"->unit4|>
];
qcData=qcToy["portable-contact"];qcSource=qcData["Source"];qcLower=qcData["Lower"];
qcSourceBefore=qcSource;qcLowerBefore=qcLower;
qcResult=MemoryConstrained[TimeConstrained[qcCalibrate[qcSource,qcLower,{"V","V"},"phi"],40,$TimedOut],2*2^30,$Aborted];
If[FailureQ[qcResult],Print["TOY RESULT ",qcResult]];
check["the known full lower amplitude determines the existing metric coefficient symbolically",
 AssociationQ[qcResult]&&qcResult["Status"]==="EXACT_SYMBOLIC_CONTACT_CALIBRATION"&&qcResult["MetricCoefficient"]===Last[qcData["Parameters"]]];
check["the coefficient is derived rather than copied from the bare quartic",
 AssociationQ[qcResult]&&qcResult["MetricCoefficient"]=!=qcData["Parameters"][[5]]];
check["every coherent component including identically zero monomials is certified",
 AssociationQ[qcResult]&&qcResult["CoherentComponentCount"]===9&&Length[qcResult["CoherentMonomials"]]===9&&
 qcResult["AllCoherentCoefficientRemainders"]===ConstantArray[0,9]&&qcResult["CompleteCoherentCoefficientCoverage"]];
check["the exact chart conserves momentum and has every external mass shell",
 AssociationQ[qcResult]&&qcResult["MomentumConservationVerified"]&&qcResult["ExternalMassShellRemainders"]==={0,0,0,0}&&
 AllTrue[qcResult["DenominatorRemainders"],#=!=0&]];
check["separate lower-model scopes are rebased only through declared names and definitions",
 AssociationQ[qcResult]&&Length[qcResult["ParameterTransport"]["ParameterMap"]]>0&&
 Complement[DeleteDuplicates[Cases[qcResult["MetricCoefficient"],_ModelParameter,{0,Infinity}]],First/@qcSource[[3]]]==={}&&
 !qcResult["ParameterTransport"]["NumericalDefaultsUsed"]];
check["cubic source vertices and ordered diagrams retain complete exact certificates",
 AssociationQ[qcResult]&&qcResult["CubicDiagramCount"]===3&&qcResult["CompleteOffShellConversionCoverage"]&&
 Sort[qcResult["OriginalGraphCertificates"]]===Sort[qcResult["LiftedGraphCertificates"]]&&
 AllTrue[qcResult["CubicVertexCertificates"],#["ExactDifference"]===0&]];
check["calibration adds no independent interaction and uses no parameter fit or benchmark",
 AssociationQ[qcResult]&&!qcResult["NewIndependentContact"]&&!qcResult["NumericalCoefficientFit"]&&
 !qcResult["IndependentBenchmarkRead"]&&qcResult["NumericalParameterAssignments"]==={}&&
 qcResult["NumericalKinematicSamples"]===0&&qcResult["InternalOnShellConditions"]==={}&&qcResult["RequiresFullAmplitudeValidation"]];
qcRenames={"V"->"Omega","phi"->"chi"};
qcParamRenames=(#->ModelParameter["new"<>ModelParameterName[#],"renamed-contact","Real"]&)/@qcData["Parameters"];
qcLowerParamRenames=((#/.qcData["ScopeMap"])->ModelParameter["new"<>ModelParameterName[#],"renamed-lower","Real"]&)/@qcData["Parameters"];
qcRenamed=qcCalibrate[qcSource/.Join[qcRenames,qcParamRenames],qcLower/.Join[qcRenames,qcLowerParamRenames],{"Omega","Omega"},"chi"];
check["renaming particles and every model input preserves the symbolic calibration",
 AssociationQ[qcRenamed]&&qcRenamed["MetricCoefficient"]===(qcResult["MetricCoefficient"]/.qcParamRenames)];
qcDifferentDefaults=qcLower;qcDifferentDefaults[[3,All,2]]=Range[6]+10;
check["symbolic calibration does not borrow independent numerical defaults from the lower model",
 AssociationQ[qcCalibrate[qcSource,qcDifferentDefaults,{"V","V"},"phi"]]];
qcDependentSource=qcSource;qcDependentLower=qcLower;
qcKnown=Last[qcData["Parameters"]];qcBare=qcData["Parameters"][[5]];
qcDependentSource[[3]]=Most[qcDependentSource[[3]]];qcDependentSource[[4]]={{qcKnown,2qcBare}};
qcDependentLower[[3]]=Most[qcDependentLower[[3]]];qcDependentLower[[4]]={{qcKnown/.qcData["ScopeMap"],2(qcBare/.qcData["ScopeMap"])}};
qcDependentResult=qcCalibrate[qcDependentSource,qcDependentLower,{"V","V"},"phi"];
check["matching dependent definitions are certified across model scopes",
 AssociationQ[qcDependentResult]&&AllTrue[Last/@qcDependentResult["ParameterTransport"]["DependentDefinitionDifferences"],#===0&]];
qcBadDefinition=qcDependentLower;qcBadDefinition[[4,1,2]]=3(qcBare/.qcData["ScopeMap"]);
check["conflicting dependent definitions cannot be silently rebased",
 FailureQ[qcCalibrate[qcDependentSource,qcBadDefinition,{"V","V"},"phi"]]];
qcMissing=qcLower;qcMissing[[2]]=Most[qcMissing[[2]]];
check["an absent complete lower amplitude is rejected rather than interpreted as known zero",
 FailureQ[qcCalibrate[qcSource,qcMissing,{"V","V"},"phi"]]];
qcNonminimal=qcSource;qcNonminimal[[2,3,-1]]*=Mandelstahm[1,2];
check["an existing nonminimal quartic is not changed into a metric contact",
 FailureQ[qcCalibrate[qcNonminimal,qcLower,{"V","V"},"phi"]]];
qcDerivativeLower=qcLower;qcDerivativeLower[[2,-1,-1]]+=MomProd[1,2](qcData["MetricUnit"]/.qcData["ScopeMap"]);
check["a momentum-dependent regular remainder is not declared a constant metric coefficient",
 FailureQ[qcCalibrate[qcSource,qcDerivativeLower,{"V","V"},"phi"]]];
qcForeignLower=qcLower;qcForeignLower[[2,-1,-1]]*=ModelParameter["foreign","unrelated","Real"];
check["foreign scalar ownership is rejected even when its printed form is simple",
 FailureQ[qcCalibrate[qcSource,qcForeignLower,{"V","V"},"phi"]]];
check["unsupported spin color mass and input classes fail without a candidate",
 FailureQ[qcCalibrate[ReplacePart[qcSource,{1,2,8}->3],qcLower,{"V","V"},"phi"]]&&
 FailureQ[qcCalibrate[ReplacePart[qcSource,{1,2,6}->0],qcLower,{"V","V"},"phi"]]&&
 FailureQ[qcCalibrate[qcSource,qcLower,{"V","unknown"},"phi"]]&&
 FailureQ[qcCalibrate[qcSource,qcLower,{"V","V","V"},"phi"]]];
check["the source and complete lower models remain unchanged",qcSource===qcSourceBefore&&qcLower===qcLowerBefore];
(* Fresh ordinary SM lower amplitudes exercise both self-conjugate and charged vectors. *)
qcSM=ReadModel[FileNameJoin[{root,"models"}],1];qcSMLower=ReadModel[FileNameJoin[{root,"models"}],2];
qcZZ=qcCalibrate[qcSM,qcSMLower,{"Z","Z"},"h"];
qcWW=qcCalibrate[qcSM,qcSMLower,{"W+","W-"},"h"];
qcE=ModelParameter[qcSM,"EE"];qcSW=ModelParameter[qcSM,"SW"];qcMW=ModelParameter[qcSM,"MW"];qcMZ=ModelParameter[qcSM,"MZ"];
If[FailureQ[qcZZ],Print["ZZ RESULT ",qcZZ]];If[FailureQ[qcWW],Print["WW RESULT ",qcWW]];
check["fresh HHZZ is exactly reconstructed from its complete constructive lower amplitude",
 AssociationQ[qcZZ]&&Cancel[Together[qcZZ["MetricCoefficient"]-qcE^2qcMZ^2/(2qcMW^2qcSW^2)]]===0&&qcZZ["AllCoherentCoefficientRemainders"]===ConstantArray[0,9]];
check["fresh HHWW is exactly reconstructed from its complete constructive lower amplitude",
 AssociationQ[qcWW]&&Cancel[Together[qcWW["MetricCoefficient"]-qcE^2/(2qcSW^2)]]===0&&qcWW["AllCoherentCoefficientRemainders"]===ConstantArray[0,9]];
qcCandidateBuild=HEPCATCovariantContactCalibration`Private`calibratedCovariantSource;
qcFFSource=qcSource;qcFFLower=qcLower;
qcFMass=ModelParameter["fermionMass","portable-contact","Real"];
qcFV=ModelParameter["fermionVector","portable-contact","Real"];
qcFS=ModelParameter["fermionScalar","portable-contact","Real"];
AppendTo[qcFFSource[[1]],{"fermion","f","F",3,1,qcFMass,0,1,"","f","F"}];
qcFFSource[[3]]=Join[qcFFSource[[3]],{{qcFMass,1},{qcFV,2/5},{qcFS,3/7}}];
qcVmass=First[qcData["Parameters"]];
qcFFV=qcCoherent[FeynmanToSpinorProducts[DiracChain[FermionBar[2,qcFMass,"Dirac"],DiracWord[DiracGamma[LorentzIndex[3]]],FermionKet[1,qcFMass,"Dirac"]]PolarizationVector[3,LorentzIndex[3],qcVmass,Automatic],"ReduceSpinorProducts"->False]];
qcFFS=FeynmanToSpinorProducts[DiracChain[FermionBar[2,qcFMass,"Dirac"],DiracWord[],FermionKet[1,qcFMass,"Dirac"]],"ReduceSpinorProducts"->False];
qcFFSource[[2]]=Join[qcFFSource[[2]],{{"f","F","V","",qcFV,qcFFV},{"f","F","phi","",qcFS,qcFFS}}];
qcFFLower[[1]]=qcFFSource[[1]]/.qcData["ScopeMap"];
qcFFLower[[3]]=qcFFSource[[3]]/.qcData["ScopeMap"];
qcFFCandidate=qcCandidateBuild[{"f","F","V","phi","phi"},qcFFSource,qcFFLower];
If[FailureQ[qcFFCandidate],Print["TOY FIVE RESULT ",qcFFCandidate]];
check["the generic five-leg wrapper returns a source-only unvalidated candidate",
 AssociationQ[qcFFCandidate]&&qcFFCandidate["Status"]==="UNVALIDATED_CANDIDATE"&&!qcFFCandidate["NativeValidationPerformed"]&&qcFFCandidate["RequiresCheckAmplitudes"]];
check["the five-leg wrapper preserves every ordered graph port physical pole and fermion flow",
 AssociationQ[qcFFCandidate]&&qcFFCandidate["EveryOriginalDiagramCovered"]&&qcFFCandidate["EveryUsedOrderedVertexCertified"]&&
 Sort[qcFFCandidate["OrderedGraphCertificates"]]===Sort[qcFFCandidate["LiftedGraphCertificates"]]];
check["the wrapper preserves raw vector tensors and documents their full coherent encoding",
 AssociationQ[qcFFCandidate]&&!FreeQ[qcFFCandidate["RawTensorExpression"],_VectorSpinIndex]&&FreeQ[qcFFCandidate["Expression"],_VectorSpinIndex]&&
 qcFFCandidate["CoherentExternalVectorEncoding"]["CompleteSpinDegrees"]==={1,1,2,0,0}&&
 !qcFFCandidate["CoherentExternalVectorEncoding"]["OffShellKinematicsChanged"]];
check["all used quartics have complete lower-amplitude calibration rather than a bare-row equality claim",
 AssociationQ[qcFFCandidate]&&Length[qcFFCandidate["ContactCalibrations"]]===1&&
 AllTrue[Select[qcFFCandidate["VertexCertificates"],#["Class"]==="VectorVectorScalarScalar"&],
  #["Status"]==="EXACT_COMPLETE_LOWER_AMPLITUDE_CONTACT_CONTINUATION"&&!#["BareVertexOffShellEqualityClaimed"]&]];
check["unsupported wrapper classes remain unavailable without a candidate",
 FailureQ[qcCandidateBuild[{"f","F","phi","phi","phi"},qcFFSource,qcFFLower]]&&
 FailureQ[qcCandidateBuild[{"f","F","V","phi","phi"},ReplacePart[qcFFSource,{1,3,9}->"L"],qcFFLower]]&&
 FailureQ[qcCandidateBuild[{"f","F","V","phi","phi"},ReplacePart[qcFFSource,{1,3,8}->3],qcFFLower]]];
qcSMCandidate=qcCandidateBuild[{"e","E","Z","h","h"},qcSM,qcSMLower];
If[FailureQ[qcSMCandidate],Print["SM FIVE RESULT ",qcSMCandidate]];
check["the fresh SM metadata class obtains all sixteen source-derived graphs without a saved answer",
 AssociationQ[qcSMCandidate]&&qcSMCandidate["OriginalDiagramCount"]===16&&qcSMCandidate["LiftedDiagramCount"]===16&&
 qcSMCandidate["CompleteOffShellConversionCoverage"]&&!qcSMCandidate["IndependentBenchmarkRead"]];
check["the fresh SM wrapper keeps source ownership and all nine lower-contact component certificates",
 AssociationQ[qcSMCandidate]&&Complement[DeleteDuplicates[Cases[qcSMCandidate["Expression"],_ModelParameter,Infinity]],First/@Join[qcSM[[3]],qcSM[[4]]]]==={}&&
 AllTrue[qcSMCandidate["ContactCalibrations"],#["AllCoherentCoefficientRemainders"]===ConstantArray[0,9]&]];
Print["COVARIANT CONTACT CALIBRATION CHECKS: ",checks,"; FAILURES: ",Length[failures]];
If[failures=!={},Print[failures]];
Print["HEPCAT_TEST_COMPLETED:covariant-contact-calibration-tests.wl"];
Quit[If[failures==={},0,1]];
