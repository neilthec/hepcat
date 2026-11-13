Global`qcdTestRoot=Replace[Environment["HEPCAT_QCD_REPOSITORY_ROOT"],{""->DirectoryName[DirectoryName[DirectoryName[$InputFileName]]],Except[_String]->DirectoryName[DirectoryName[DirectoryName[$InputFileName]]]}];
Global`$HEPCATpath=Replace[Environment["HEPCAT_QCD_TEST_SOURCE"],{""->FileNameJoin[{Global`qcdTestRoot,"source"}],Except[_String]->FileNameJoin[{Global`qcdTestRoot,"source"}]}];
Get[FileNameJoin[{Global`$HEPCATpath,"HEPCAT.wl"}]];
Begin["HEPCATQCDLibraryControls`Private`"];$ContextPath=Prepend[DeleteCases[$ContextPath,"HEPCATQCDLibraryControls`Private`"],"HEPCATQCDLibraryControls`Private`"];$HistoryLength=0;
out=CreateDirectory[FileNameJoin[{$TemporaryDirectory,CreateUUID["hepcat-qcd-unit-"]}]];passed=0;failed=0;records={};
SetAttributes[test,HoldRest];test[name_,value_]:=Module[{ok=TrueQ[value]},AppendTo[records,<|"Test"->name,"Pass"->ok|>];If[ok,passed++,failed++];Print[If[ok,"PASS ","FAIL "],name]];
model=ReadModel[FileNameJoin[{Global`qcdTestRoot,"models"}],1];before=model;
adjoint=SelectFirst[model[[1]],#[[5]]===2&&#[[6]]===0&&#[[8]]===8&];fund=SelectFirst[model[[1]],#[[5]]===1&&#[[6]]=!=0&&Abs[#[[8]]]===3&];
ffg=Select[model[[2]],Length[HEPCATCovariantCurrentSources`Private`rowParticles[#]]===3&&Take[#,2]===fund[[{2,3}]]&&MemberQ[adjoint[[{2,3}]],#[[3]]]&];
ggg=Select[model[[2]],Length[HEPCATCovariantCurrentSources`Private`rowParticles[#]]===3&&AllTrue[HEPCATCovariantCurrentSources`Private`rowParticles[#],MemberQ[adjoint[[{2,3}]],#]&]&];
used=Join[ffg,ggg];catalog=HEPCATMinimalQCDCurrentSources`Private`catalog[model,used];
test["minimal fullcolour catalogue covers every used elementary row",AssociationQ[catalog]&&catalog["Status"]==="EXACT_MODEL_ONLY_LOCAL_CATALOGUE"&&catalog["EveryUsedElementaryRowCertified"]];
If[failed>0,Print[ToString[catalog,InputForm]];Quit[2]];
test["fullcolour catalogue preserves fundamental and adjoint representations",Sort[DeleteDuplicates[(List@@catalog["Model"])[[1,All,8]]]]==={3,8}&&catalog["OriginalColourDimensionsRetained"]];
test["derived local Lorentz bodies never attach colour twice",catalog["LorentzBodiesContainNoDuplicateColour"]];
test["ordinary local ordered colour factors exactly match",Length[catalog["OrdinaryOrderedColourCertificates"]]===2&&AllTrue[catalog["OrdinaryOrderedColourCertificates"],#["ExactColourEquality"]&]];
test["both complex3G proofs remain bound to the local catalogue",Length[catalog["ThreeAdjointFullColourCertificates"]]===1&&First[catalog["ThreeAdjointFullColourCertificates"]]["BothLegalComplexBranchesProved"]&&First[catalog["ThreeAdjointFullColourCertificates"]]["NoPosthocPhaseAdjustment"]];
local=First[catalog["OrdinaryOrderedColourCertificates"]];tensor=local["SourceColourTensor"];swap=ConstructiveDiagrams`Private`completionBoseCycle[tensor,{1,2}];
test["swapping fundamental colour ports is distinguished by all72components",Length[ConstructiveDiagrams`Private`colorTensorVector[tensor,{3,-3,8}]]===72&&ConstructiveDiagrams`Private`colorTensorVector[tensor,{3,-3,8}]=!=ConstructiveDiagrams`Private`colorTensorVector[swap,{3,-3,8}]];
unowned=ReplacePart[First[ffg],-2->7 First[ffg][[-2]]];bad=HEPCATMinimalQCDCurrentSources`Private`catalog[model,{unowned}];test["unowned elementary rows reject",FailureQ[bad]&&First[bad]==="UnownedOrEmptyUsedVertexRows"];
quartic={fund[[2]],fund[[3]],fund[[2]],fund[[3]],1,1};bad=HEPCATMinimalQCDCurrentSources`Private`catalog[ReplacePart[model,2->Append[model[[2]],quartic]],{quartic}];test["used quartic cannot disappear from the local library",FailureQ[bad]&&First[bad]==="QuarticOrOtherUsedArityNotSupported"];
explicit=ReplacePart[First[ffg],-1->Last[First[ffg]]ColorGenerator[ColorIndex[2],ColorIndex[1],ColorIndex[3]]];bad=HEPCATMinimalQCDCurrentSources`Private`catalog[ReplacePart[model,2->Append[model[[2]],explicit]],{explicit}];test["explicit stored colour is never silently stripped",FailureQ[bad]&&First[bad]==="ExplicitStoredColourRequiresSeparateFactorizationProof"];
nonminimal=ReplacePart[First[ffg],-2->First[ffg][[-2]]Mandelstahm[1,2]];bad=HEPCATMinimalQCDCurrentSources`Private`catalog[ReplacePart[model,2->(model[[2]]/.First[ffg]->nonminimal)],{nonminimal,Last[ffg]}];test["a kinematic prefactor rejects instead of fitting a local scalar",FailureQ[bad]];
foreign=ModelParameter["extraStrong","unownedTestScope","Real"];foreignRow=ReplacePart[First[ffg],-2->First[ffg][[-2]]foreign];bad=HEPCATMinimalQCDCurrentSources`Private`catalog[ReplacePart[model,2->(model[[2]]/.First[ffg]->foreignRow)],{foreignRow,Last[ffg]}];test["foreign model-scoped coefficients reject",FailureQ[bad]];
missing=ReplacePart[model,2->DeleteCases[model[[2]],Last[ffg]]];bad=HEPCATMinimalQCDCurrentSources`Private`catalog[missing,{First[ffg]}];test["missing conjugate massless-current counterpart is unsupported",FailureQ[bad]];
position=First[FirstPosition[model[[1]],fund]];chiral=ReplacePart[model,{1,position,6}->0];chiral=ReplacePart[chiral,{1,position,9}->"L"];bad=HEPCATMinimalQCDCurrentSources`Private`catalog[chiral,ffg];test["declared chiralFFG remains explicitly unsupported until a matcher exists",FailureQ[bad]&&First[bad]==="DeclaredMasslessFermionMasslessVectorCurrentNotYetMatched"];
neutralRow=SelectFirst[model[[2]],With[{rr=HEPCATCovariantCurrentSources`Private`particleRow[#,model]&/@HEPCATCovariantCurrentSources`Private`rowParticles[#]},Length[rr]===3&&Count[rr[[All,5]],1]===2&&Count[rr[[All,5]],2]===1&&Count[rr[[All,6]],0]===2&&AllTrue[rr,#[[8]]===1&]]&];
neutral=HEPCATMinimalQCDCurrentSources`Private`catalog[model,{neutralRow}];test["declared chiralneutral current retains exact projector match",AssociationQ[neutral]&&AnyTrue[neutral["Certificates"],Lookup[#,"IdenticallyZeroTemplatePositions",{}]=!={}&]];
(* A topology-only5leaf witness. No fivepoint amplitude or graph generation. *)
ffplus=SelectFirst[ffg,#[[3]]===adjoint[[2]]&];gminus=SelectFirst[ggg,Count[Take[#,3],adjoint[[3]]]===2&];
process={fund[[2]],fund[[3]],fund[[2]],fund[[3]],adjoint[[2]]};
edge1=UndirectedEdge[6,7];edge2=UndirectedEdge[7,8];
external[i_]:=<|"Particle"->process[[i]],"Index"->i,"Kind"->"External","Source"->i|>;
internal[name_,edge_]:=<|"Particle"->name,"Index"->Multiparticle[1,2],"Kind"->"Internal","Source"->edge|>;
make[order_List]:=Module[{graph=Graph[Join[Table[UndirectedEdge[order[[i]],If[i<=2,6,8]],{i,4}],{UndirectedEdge[5,7],edge1,edge2}]],structures},
 structures=<|6->{<|"ModelRow"->ffplus,"IncidentLegs"->{external[order[[1]]],external[order[[2]]],internal[adjoint[[2]],edge1]}|>},
 8->{<|"ModelRow"->ffplus,"IncidentLegs"->{external[order[[3]]],external[order[[4]]],internal[adjoint[[2]],edge2]}|>},
 7->{<|"ModelRow"->gminus,"IncidentLegs"->{internal[adjoint[[3]],edge1],internal[adjoint[[3]],edge2],external[5]}|>}|>;
 <|"Graph"->graph,"InternalVertices"->{6,7,8},"LeafVertices"->Range[5],"ExternalParticles"->process,"CanonicalExternalParticles"->process,
 "Spin2"->AssociationThread[DeleteDuplicates[process],{1,1,2}],"FermionRole"-><|fund[[2]]->"Ket",fund[[3]]->"Bar"|>,"VertexStructures"->structures,
 "Propagators"-><|edge1-><|"Particle"->adjoint[[2]],"Antiparticle"->adjoint[[3]],"Mass"->0,"Spin2"->2|>,edge2-><|"Particle"->adjoint[[2]],"Antiparticle"->adjoint[[3]],"Mass"->0,"Spin2"->2|>|>|>];
first=make[{1,2,3,4}];changed=make[{1,4,3,2}];firstCertificate=HEPCATCovariantCurrentSources`Private`diagramCertificate[first,catalog,model,False];changedCertificate=HEPCATCovariantCurrentSources`Private`diagramCertificate[changed,catalog,model,False];
test["changed fermionpairing changes the ordered graph certificate",AssociationQ[firstCertificate]&&AssociationQ[changedCertificate]&&firstCertificate=!=changedCertificate&&firstCertificate["OrientedFermionEndpoints"]=!=changedCertificate["OrientedFermionEndpoints"]];
firstPaths=ConstructiveDiagrams`Private`feynmanFermionPaths[first];changedPaths=ConstructiveDiagrams`Private`feynmanFermionPaths[changed];firstPairs=({First[#],Last[#]}&/@firstPaths);changedPairs=({First[#],Last[#]}&/@changedPaths);
test["actual differentiated Wick parity detects the endpoint transposition",Sort[firstPairs]===Sort[Reverse/@firstCertificate["OrientedFermionEndpoints"]]&&Sort[changedPairs]===Sort[Reverse/@changedCertificate["OrientedFermionEndpoints"]]&&ConstructiveDiagrams`Private`feynmanFermionPairSign[firstPairs]===-ConstructiveDiagrams`Private`feynmanFermionPairSign[changedPairs]];
test["original elementary model remains byte-identical as an expression",model===before];
result=<|"Status"->If[failed===0,"ALL_PASS","FAIL"],"Passed"->passed,"Failed"->failed,"Records"->records,"NoFivePointAmplitudeJob"->True,"OnlyExactLocalOrTopologyMetadataControls"->True,"ProductionEdited"->False|>;
Export[out<>"/qcd-library-controls.json",result,"RawJSON"];Print[passed," controls; ",failed," failures."];Print["HEPCAT_TEST_COMPLETED:qcd-local-library-tests.wl"];End[];Quit[If[failed===0,0,1]];
