Global`qcdTestRoot=Replace[Environment["HEPCAT_QCD_REPOSITORY_ROOT"],{""->DirectoryName[DirectoryName[DirectoryName[$InputFileName]]],Except[_String]->DirectoryName[DirectoryName[DirectoryName[$InputFileName]]]}];
Global`$HEPCATpath=Replace[Environment["HEPCAT_QCD_TEST_SOURCE"],{""->FileNameJoin[{Global`qcdTestRoot,"source"}],Except[_String]->FileNameJoin[{Global`qcdTestRoot,"source"}]}];
Get[FileNameJoin[{Global`$HEPCATpath,"HEPCAT.wl"}]];
Begin["HEPCATQCDAdmissionControls`Private`"];$ContextPath=Prepend[DeleteCases[$ContextPath,"HEPCATQCDAdmissionControls`Private`"],"HEPCATQCDAdmissionControls`Private`"];$HistoryLength=0;
passed=0;failed=0;records={};SetAttributes[test,HoldRest];
test[name_,condition_]:=Module[{ok=TrueQ[condition]},AppendTo[records,<|"Test"->name,"Pass"->ok|>];If[ok,passed++,failed++];Print[If[ok,"PASS ","FAIL "],name]];
model=ReadModel[FileNameJoin[{Global`qcdTestRoot,"models"}],1];modelBefore=model;scope=HEPCATCovariantCurrentSources`Private`objectSHA[model];
trueFlags={"EveryOriginalDiagramCovered","EveryOriginalOrderedGraphAndFermionSignMatched","ActualDifferentiatedFeynmanPairingAndWickParityVerified","EveryUsedOrderedVertexCertified","CompleteOffShellConversionCoverage","ModelParametersUnchanged","RequiresCheckAmplitudes"};
falseFlags={"NumericalCoefficientFit","IndependentBenchmarkRead","NativeValidationPerformed","NewIndependentContacts"};
(* Synthetic metadata stops before any positive certificate can be admitted.
   Only the independently replayed witness checks are exercised below. *)
baseline=Join[AssociationThread[trueFlags,ConstantArray[True,Length[trueFlags]]],AssociationThread[falseFlags,ConstantArray[False,Length[falseFlags]]],
 <|"Status"->"UNVALIDATED_CANDIDATE","SourceModelSHA256"->scope,"Process"->{"u","U","c","C","G.+"},"ExternalMassRules"->{},"CompleteSpinWeights"->{1,1,1,1,-2},"OriginalDiagramCount"->1,"LiftedDiagramCount"->1,
 "OriginalGraphCertificates"->{<|"OrderedPorts"->{1,2,3},"WickParity"->1|>},"LiftedGraphCertificates"->{<|"OrderedPorts"->{1,2,3},"WickParity"->1|>},"PerGraphCorrespondence"->{<|"OriginalIndex"->1,"ActualWickParity"->1|>},
 "Catalogue"-><|"Model"->model,"Certificates"->{<|"CertificateID"->"unit-local-identity"|>},"OrdinaryOrderedColourCertificates"->{<|"OrderedPorts"->{1,2,3},"ColourTensor"->ColorGenerator[ColorIndex[3],ColorIndex[1],ColorIndex[2]]|>},
 "ThreeAdjointFullColourCertificates"->{<|"Coefficient"->ModelParameter[model,"GG"],"SourceRowSHA256"->{"unit-row"},"DerivedParticleRow"->{"adjoint"},"DerivedFeynmanRow"->{"unit-row"},"ColourPhaseIncludedBeforeMatching"->True,"NoPosthocPhaseAdjustment"->True,"BothLegalComplexBranchesProved"->True,"EveryOrderedBosePermutationProved"->True,"OrderedBoseCertificates"->{1}|>}|>,
 "InternalOnShellConditions"->{},"NumericalParameterAssignments"->{}|>];
SetAttributes[fields,HoldAll];fields[s_]:=Hash[{OwnValues[s],DownValues[s],UpValues[s],SubValues[s],NValues[s],DefaultValues[s],FormatValues[s],Options[s],Attributes[s],Messages[s]},"SHA256"];
sourceBefore=fields[HEPCATMinimalQCDCurrentSources`Private`source];calls=0;
SetAttributes[checkRejected,HoldRest];checkRejected[name_,proposal_,tag_,expectedCalls_]:=Module[{result,before=calls},result=HEPCATMinimalQCDCurrentSources`Private`sourceCertificate[proposal,model];test[name,FailureQ[result]&&First[result]===tag&&calls-before===expectedCalls]];
Internal`InheritedBlock[{HEPCATMinimalQCDCurrentSources`Private`source},
 Clear[HEPCATMinimalQCDCurrentSources`Private`source];HEPCATMinimalQCDCurrentSources`Private`source[___]:=(calls++;baseline);
 checkRejected["noncandidate cannot claim certificate",ReplacePart[baseline,"Status"->"COMPLETE"],"UnvalidatedProposalRequired",0];
 checkRejected["foreign source model rejects before replay",ReplacePart[baseline,"SourceModelSHA256"->"foreign"],"SourceModelOwnership",0];
 Do[checkRejected["missing mandatory flag "<>flag,KeyDrop[baseline,flag],"IncompleteSourceProofFlags",0];checkRejected["false mandatory flag "<>flag,ReplacePart[baseline,flag->False],"IncompleteSourceProofFlags",0],{flag,trueFlags}];
 Do[checkRejected["forbidden claim "<>flag,ReplacePart[baseline,flag->True],"UnexpectedFitBenchmarkOrInternalShell",0],{flag,falseFlags}];
 checkRejected["internal shell assumptions rejected",ReplacePart[baseline,"InternalOnShellConditions"->{Mass[1]->0}],"UnexpectedFitBenchmarkOrInternalShell",0];
 checkRejected["sampled theory points rejected",ReplacePart[baseline,"NumericalParameterAssignments"->{1}],"UnexpectedFitBenchmarkOrInternalShell",0];
 checkRejected["omitted original graph rejected",ReplacePart[baseline,"OriginalDiagramCount"->0],"OrderedGraphOrActualWickWitnessMismatch",1];
 checkRejected["fermion pairing parity mutation rejected",ReplacePart[baseline,{"PerGraphCorrespondence",1,"ActualWickParity"}->-1],"OrderedGraphOrActualWickWitnessMismatch",1];
 checkRejected["ordered source ports mutation rejected",ReplacePart[baseline,{"OriginalGraphCertificates",1,"OrderedPorts"}->{2,1,3}],"OrderedGraphOrActualWickWitnessMismatch",1];
 checkRejected["local operator coefficient mutation rejected",ReplacePart[baseline,{"Catalogue","Model",3}->{}],"LocalOperatorOrOrderedColourWitnessMismatch",1];
 checkRejected["ordered local colour-port mutation rejected",ReplacePart[baseline,{"Catalogue","OrdinaryOrderedColourCertificates",1,"OrderedPorts"}->{2,1,3}],"LocalOperatorOrOrderedColourWitnessMismatch",1];
 checkRejected["three-adjoint coefficient mutation rejected",ReplacePart[baseline,{"Catalogue","ThreeAdjointFullColourCertificates",1,"Coefficient"}->2ModelParameter[model,"GG"]],"FullColourAdjointPrimitiveMismatch",1];
 checkRejected["three-adjoint colour-phase claim mutation rejected",ReplacePart[baseline,{"Catalogue","ThreeAdjointFullColourCertificates",1,"NoPosthocPhaseAdjustment"}->False],"FullColourAdjointPrimitiveMismatch",1];
];
test["all ten source definition fields restored",fields[HEPCATMinimalQCDCurrentSources`Private`source]===sourceBefore];
test["original elementary model expression untouched",model===modelBefore];
result=<|"Status"->If[failed===0,"ALL_PASS","FAIL"],"Passed"->passed,"Failed"->failed,"Records"->records,"NoPositiveSyntheticCertificateClaim"->True,"NoNativePhysicsJob"->True|>;
Export[CreateDirectory[FileNameJoin[{$TemporaryDirectory,CreateUUID["hepcat-qcd-unit-"]}]]<>"/qcd-source-admission-controls.json",result,"RawJSON"];Print[passed," controls; ",failed," failures."];Print["HEPCAT_TEST_COMPLETED:qcd-source-admission-tests.wl"];End[];Quit[If[failed===0,0,1]];
