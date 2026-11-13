(* HEPCAT - High Energy Physics Constructive Amplitude Tool
   Copyright (C) 2026 Neil Christensen
   SPDX-License-Identifier: GPL-3.0-or-later

   Synthetic graph-certificate and metadata controls only. The81-entry lists
   below test certificate integrity; they are not amplitude proofs or solver
   outputs. No model file, research WXF, diagram or calibration is evaluated. *)
root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
repositoryOverride=Environment["HEPCAT_TEST_REPOSITORY"];
If[StringQ[repositoryOverride]&&repositoryOverride=!="",root=repositoryOverride];
entry=Environment["HEPCAT_TEST_PACKAGE_ENTRYPOINT"];
If[!StringQ[entry]||entry==="",entry=FileNameJoin[{root,"source","HEPCAT.wl"}]];
Global`$HEPCATpath=DirectoryName[entry];Get[entry];
Begin["HEPCATLowerVectorGraphGuardTests`Private`"];
$ContextPath=Prepend[DeleteCases[$ContextPath,"HEPCATLowerVectorGraphGuardTests`Private`"],
 "HEPCATLowerVectorGraphGuardTests`Private`"];
$HistoryLength=0;
productionBefore={DownValues[ConstructiveDiagrams`Private`constructCovariantGraphCoverageQ],
 DownValues[HEPCATCovariantContactCalibration`Private`calibratedGraphUnion],
 DownValues[HEPCATCovariantCurrentSources`Private`diagramCertificate],
 DownValues[HEPCATCovariantCurrentSources`Private`usedRows]};
rows={};test[name_,value_]:=AppendTo[rows,<|"Test"->name,"Passed"->TrueQ[value]|>];
good[record_]:=Quiet[Check[ConstructiveDiagrams`Private`constructCovariantGraphCoverageQ[record,{<|"OriginalGraph"->1|>}],False]];
original={<|"Signature"->1|>};additional=<|"Signature"->2|>;
proof=<|"AllEightyOneComponentsProvedZero"->True,"NumericalCoefficientFit"->False,"IndependentBenchmarkRead"->False,
 "Checks"->Table[<|"Component"->i,"Status"->"PROVED_ZERO","Proof"-><|"Value"->0|>|>,{i,81}]|>;
certs={<|"CertificateID"->"cubic","Class"->"VectorVectorVector","LiftedRow"->{"v1","v2","v3",1,1}|>,
 <|"CertificateID"->"contact","Class"->"VectorVectorVectorVector","LowerAmplitudeCalibration"->proof|>};
added=<|"GraphCertificate"->additional,"ContactCertificateID"->"contact","CubicCertificateIDs"->{"cubic"},"EveryVertexExactlyCertified"->True|>;
union=<|"Status"->"COMPLETE","AddedLowerGraphCertificates"->{added},"OriginalOrderedSubsetVerified"->True,"ExactOrderedUnionVerified"->True,"EveryAddedVertexCertified"->True,
 "OriginalGraphCertificates"->original,"LiftedGraphCertificates"->Append[original,additional],"AdditionalLowerDerivedGraphCount"->1|>;
proposal=<|"OrderedGraphCertificates"->original,"LiftedGraphCertificates"->Append[original,additional],"OriginalDiagramCount"->1,"LiftedDiagramCount"->2,
 "SourceGraphUnion"->union,"VertexCertificates"->certs|>;
test["CompleteExactGraphUnionAccepted",good[proposal]];
test["MissingUnionRejected",!good[KeyDrop[proposal,"SourceGraphUnion"]]];
test["MalformedUnionRejected",!good[Join[proposal,<|"SourceGraphUnion"->None|>]]];
test["AddedMultiplicityCannotBeDropped",!good[Join[proposal,<|"LiftedDiagramCount"->1|>]]];
test["MissingOriginalGraphRejected",!good[Join[proposal,<|"SourceGraphUnion"->Join[union,<|"OriginalGraphCertificates"->{}|>]|>]]];
test["UncertifiedAddedGraphRejected",!good[Join[proposal,<|"SourceGraphUnion"->Join[union,<|"EveryAddedVertexCertified"->False|>]|>]]];
test["NoCubicEmitterCertificateRejected",!good[Join[proposal,<|"SourceGraphUnion"->Join[union,<|"AddedLowerGraphCertificates"->{KeyDrop[added,"CubicCertificateIDs"]}|>]|>]]];
test["EmptyCubicEmitterCertificateRejected",!good[Join[proposal,<|"SourceGraphUnion"->Join[union,<|"AddedLowerGraphCertificates"->{Join[added,<|"CubicCertificateIDs"->{}|>]}|>]|>]]];
test["UnknownCubicEmitterRejected",!good[Join[proposal,<|"SourceGraphUnion"->Join[union,<|"AddedLowerGraphCertificates"->{Join[added,<|"CubicCertificateIDs"->{"unknown"}|>]}|>]|>]]];
test["MalformedAddedEntryRejected",!good[Join[proposal,<|"SourceGraphUnion"->Join[union,<|"AddedLowerGraphCertificates"->{Null}|>]|>]]];
badProof=Join[proof,<|"Checks"->ReplacePart[proof["Checks"],81-><|"Component"->81,"Status"->"NONZERO","Proof"-><|"Value"->1|>|>]|>];
badCert=Join[Last[certs],<|"LowerAmplitudeCalibration"->badProof|>];test["NonzeroComponentRejected",!good[Join[proposal,<|"VertexCertificates"->{First[certs],badCert}|>]]];
badProof=Join[proof,<|"Checks"->ReplacePart[proof["Checks"],81->First[proof["Checks"]]]|>];badCert=Join[Last[certs],<|"LowerAmplitudeCalibration"->badProof|>];
test["DuplicateAndMissingComponentRejected",!good[Join[proposal,<|"VertexCertificates"->{First[certs],badCert}|>]]];
badCert=Join[Last[certs],<|"LowerAmplitudeCalibration"->Join[proof,<|"NumericalCoefficientFit"->True|>]|>];
test["NumericallyFittedContactRejected",!good[Join[proposal,<|"VertexCertificates"->{First[certs],badCert}|>]]];
badCert=Join[Last[certs],<|"LowerAmplitudeCalibration"->Join[proof,<|"IndependentBenchmarkRead"->True|>]|>];
test["BenchmarkConstructedContactRejected",!good[Join[proposal,<|"VertexCertificates"->{First[certs],badCert}|>]]];
old=Join[proposal,<|"LiftedGraphCertificates"->original,"LiftedDiagramCount"->1|>];test["ExistingExactOriginalGraphPathPreserved",good[KeyDrop[old,"SourceGraphUnion"]]];

(* Metadata gates use renamed toy particles and literal exact masses. They do
   not call local-operator matching or the81-component contact calibrator. *)
particle[name_,anti_,spin_,mass_,color_:1,hand_:""] :=
 {name,name,anti,0,spin,mass,0,color,hand,name,anti};
particles={particle["a","a",2,1],particle["b","b",2,2],
 particle["c","c",2,3],particle["d","d",2,4],particle["photon","photon",2,0],
 particle["colored","colored",2,1,3],particle["phi","phi",0,5],
 particle["psi","psibar",1,3],particle["nu","nubar",1,0,1,"L"]};
emitter={"a","a","phi",1,1};quartic={"a","a","b","b",1,1};
model={particles,{emitter},{},{}};lower={particles,{quartic},{},{}};
paired[process_,m_:model]:=HEPCATCovariantContactCalibration`Private`fourVectorPairedLegOrders[process,m];
test["TwoPairedMassesKeepEveryLegalLabeledOrder",Length[paired[{"a","a","b","b"}]]===8];
test["CoincidentPairMassesKeepAll24Orders",Length[paired[{"a","a","a","a"}]]===24];
test["UnpairedFourMassesReject",MissingQ[HEPCATCovariantContactCalibration`Private`fourVectorPairedOrder[{"a","b","c","d"},model]]];
test["MasslessVectorCannotUseFourMassiveVectorChart",MissingQ[paired[{"a","a","photon","photon"}]]];
test["ColoredVectorCannotUseColorlessMetricChart",MissingQ[paired[{"a","a","colored","colored"}]]];
test["ScalarCannotUseVectorChart",MissingQ[paired[{"a","a","phi","phi"}]]];
test["WrongFourVectorArityRejects",MissingQ[paired[{"a","a","b"}]]];
plan=HEPCATCovariantContactCalibration`Private`fourVectorAugmentationPlan[{"a","a","b","b","phi"},model,lower,1];
test["KnownLowerContactAndDeclaredEmitterGiveMetadataPlan",AssociationQ[plan]&&plan["Status"]==="AVAILABLE"&&Length[plan["Quartets"]]===1&&
 Length[First[plan["Quartets"]]["Attachments"]]>0&&plan["NoNumericalKinematicsOrCoefficients"]&&!plan["IndependentBenchmarkRead"]];
noLower=HEPCATCovariantContactCalibration`Private`fourVectorAugmentationPlan[{"a","a","b","b","phi"},model,ReplacePart[lower,2->{}],1];
test["MissingCompleteLowerRowCannotCreateContact",AssociationQ[noLower]&&noLower["Status"]==="NOT_APPLICABLE"];
noEmitter=HEPCATCovariantContactCalibration`Private`fourVectorAugmentationPlan[{"a","a","b","b","phi"},ReplacePart[model,2->{}],lower,1];
test["MissingDeclaredCubicCannotCreateEmitter",AssociationQ[noEmitter]&&noEmitter["Status"]==="NOT_APPLICABLE"];
duplicateLower=ReplacePart[lower,2->{quartic,quartic}];
test["DuplicateKnownLowerRowsAreAmbiguous",MatchQ[HEPCATCovariantContactCalibration`Private`fourVectorAugmentationPlan[{"a","a","b","b","phi"},model,duplicateLower,1],Failure["AmbiguousCompleteLowerFourVectorRow",_Association]]];
test["WrongExternalWordHasNoInventedAttachment",HEPCATCovariantContactCalibration`Private`fourVectorAugmentationPlan[{"a","b","b","b","phi"},model,lower,1]["Status"]==="NOT_APPLICABLE"];
vvv={"a","a","b",1,1};ffv={"psi","psibar","a",1,1};
requiresLower[process_,used_]:=HEPCATCovariantCurrentSources`Private`minimalVectorLowerContactRequiredQ[process,model,used];
test["TwoDiracAndThreeVectorVVVRequiresLowerContact",requiresLower[{"psi","psibar","a","a","b"},{vvv}]];
test["DeclaredChiralEndpointsAlsoRequireLowerContact",requiresLower[{"nu","nubar","a","a","b"},{vvv}]];
test["CubicFermionCurrentsAloneDoNotInventGaugeQuartic",!requiresLower[{"psi","psibar","a","a","b"},{ffv}]];
test["VVSAloneDoesNotInventGaugeQuartic",!requiresLower[{"psi","psibar","a","a","b"},{emitter}]];
test["FourPointWordDoesNotEnterFivePointGate",!requiresLower[{"psi","psibar","a","b"},{vvv}]];
test["FourFermionWordRetainsItsEstablishedGate",!requiresLower[{"psi","psibar","psi","psibar","a"},{vvv}]];
test["UnknownExternalMetadataCannotRequireContact",!requiresLower[{"unknown","psibar","a","a","b"},{vvv}]];
test["FourVectorSourceBudgetRespectsAvailableParentTime",ConstructiveDiagrams`Private`constructFourVectorSourceBudget[<|"Quartets"->{<|"CoherentComponents"->81|>}|>,10]===10];
test["FourVectorSourceBudgetHasFinite600Cap",ConstructiveDiagrams`Private`constructFourVectorSourceBudget[<|"Quartets"->{<|"CoherentComponents"->81|>,<|"CoherentComponents"->81|>}|>,1000]===600];

(* Exercise the actual multiset-union builder using opaque, already-typed
   certificate fixtures. Only the two certificate-extraction callbacks are
   scoped stubs; the union matching and attachment checks are production code. *)
cubicRow={"a","a","phi",1,1};contactRow={"a","a","b","b",1,1};
originalGraph=<|"FixtureSignature"-><|"OrderedVertices"->1,"Phase"->1|>,"FixtureRows"->{},"InternalVertices"->{1}|>;
addedGraph=<|"FixtureSignature"-><|"OrderedVertices"->2,"Phase"->1|>,"FixtureRows"->{cubicRow,contactRow},"InternalVertices"->{1,2}|>;
catalog=<|"Certificates"->{<|"CertificateID"->"cubic","Class"->"VectorVectorScalar","LiftedRow"->cubicRow,"SourceRows"->{cubicRow}|>,
 <|"CertificateID"->"contact","Class"->"VectorVectorVectorVector","LiftedRow"->contactRow,"Particles"->{"a","a","b","b"},"LowerAmplitudeCalibration"->proof|>}|>;
attachmentPlan=<|"Quartets"->{<|"Particles"->{"a","a","b","b"},"Attachments"->{<|"CubicRow"->cubicRow|>}|>}|>;
runUnion[originalGraphs_,liftedGraphs_,cat_:catalog,attachments_:attachmentPlan]:=Internal`InheritedBlock[
 {HEPCATCovariantCurrentSources`Private`diagramCertificate,HEPCATCovariantCurrentSources`Private`usedRows},
 Clear[HEPCATCovariantCurrentSources`Private`diagramCertificate,HEPCATCovariantCurrentSources`Private`usedRows];
 HEPCATCovariantCurrentSources`Private`diagramCertificate[graph_Association,___]:=graph["FixtureSignature"];
 HEPCATCovariantCurrentSources`Private`usedRows[graphs_List]:=DeleteDuplicates[Flatten[Lookup[graphs,"FixtureRows",{}],1]];
 HEPCATCovariantContactCalibration`Private`calibratedGraphUnion[originalGraphs,liftedGraphs,cat,model,attachments]];
built=runUnion[{originalGraph},{originalGraph,addedGraph}];
test["ActualUnionBuilderAcceptsExactOriginalPlusCertifiedAttachment",AssociationQ[built]&&built["AdditionalLowerDerivedGraphCount"]===1&&built["OriginalOrderedSubsetVerified"]&&built["ExactOrderedUnionVerified"]];
test["ActualUnionBuilderRejectsMissingOriginal",FailureQ[runUnion[{originalGraph},{addedGraph}]]];
test["ActualUnionBuilderDoesNotReuseOneOccurrenceForTwoOriginals",FailureQ[runUnion[{originalGraph,originalGraph},{originalGraph,addedGraph}]]];
repeated=runUnion[{originalGraph},{originalGraph,addedGraph,addedGraph}];
test["ActualUnionBuilderPreservesRepeatedContributions",AssociationQ[repeated]&&repeated["AdditionalLowerDerivedGraphCount"]===2&&Length[repeated["AddedLowerGraphCertificates"]]===2];
test["ActualUnionBuilderRejectsChangedOrderedPhase",FailureQ[runUnion[{originalGraph},{Join[originalGraph,<|"FixtureSignature"-><|"OrderedVertices"->1,"Phase"->-1|>|>],addedGraph}]]];
test["ActualUnionBuilderRejectsUnknownContact",FailureQ[runUnion[{originalGraph},{originalGraph,addedGraph},Join[catalog,<|"Certificates"->{First[catalog["Certificates"]]}|>]]]];
test["ActualUnionBuilderRequiresExactlyTwoInternalVertices",FailureQ[runUnion[{originalGraph},{originalGraph,Join[addedGraph,<|"InternalVertices"->{1,2,3}|>]}]]];
test["ActualUnionBuilderRejectsUnplannedEmitter",FailureQ[runUnion[{originalGraph},{originalGraph,addedGraph},catalog,<|"Quartets"->{<|"Particles"->{"a","a","b","b"},"Attachments"->{<|"CubicRow"->{"b","b","phi",1,1}|>}|>}|>]]];
test["ActualUnionBuilderRejectsMissingQuartetPlan",FailureQ[runUnion[{originalGraph},{originalGraph,addedGraph},catalog,<|"Quartets"->{}|>]]];
test["ScopedMetadataCallbacksRestoreProductionDefinitions",productionBefore==={DownValues[ConstructiveDiagrams`Private`constructCovariantGraphCoverageQ],
 DownValues[HEPCATCovariantContactCalibration`Private`calibratedGraphUnion],
 DownValues[HEPCATCovariantCurrentSources`Private`diagramCertificate],
 DownValues[HEPCATCovariantCurrentSources`Private`usedRows]}];

failures=Select[rows,!TrueQ[#["Passed"]]&];
Print["CHECKS: ",Length[rows],"; FAILURES: ",Length[failures]];
If[failures=!={},Print[Lookup[failures,"Test"]];Quit[1]];
Print["HEPCAT_TEST_COMPLETED:lower-vector-graph-union-tests.wl"];
End[];Quit[0];
