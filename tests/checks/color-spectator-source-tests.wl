Get[FileNameJoin[{DirectoryName[DirectoryName[DirectoryName[$InputFileName]]],"source","HEPCAT.wl"}]];
Begin["HEPCATColorSpectatorTests`Private`"];
$ContextPath=Prepend[DeleteCases[$ContextPath,"HEPCATColorSpectatorTests`Private`"],"HEPCATColorSpectatorTests`Private`"];
c7Directory=DirectoryName[$InputFileName];
c7Passed=0;c7Failed=0;c7Records={};
c7Assert[c7Name_String,value_]:=Module[{ok=TrueQ[value]},AppendTo[c7Records,<|"Test"->c7Name,"Pass"->ok|>];If[ok,c7Passed++;Print["PASS ",c7Name],c7Failed++;Print["FAIL ",c7Name]]];
c7Model={{{"quark","q","Q",1,1,1,0,3,"","q","qbar"},{"scalar","h","h",25,0,1,0,1,"","h","h"},
 {"gluon","g","g",21,2,0,0,8,"","g","g"},{"vector","Z","Z",23,2,2,0,1,"","Z","Z"}},{{"q","Q","h","",1,1},{"h","h","h","",1,1}},{},{}};
c7Leg[c7P_String]:=<|"Particle"->c7P|>;
c7Structure[c7P_List,c7Row_List]:=<|"IncidentLegs"->(c7Leg/@c7P),"ModelRow"->c7Row|>;
c7Diagram=<|"Graph"->Graph[{UndirectedEdge[1,6],UndirectedEdge[6,7],UndirectedEdge[7,2],UndirectedEdge[6,3],UndirectedEdge[7,8],UndirectedEdge[8,4],UndirectedEdge[8,5]}],
 "ExternalParticles"->{"q","Q","h","h","Z"},"InternalVertices"->{6,7,8},"LeafVertices"->Range[5],
 "Propagators"-><|(UndirectedEdge[6,7])-><|"Particle"->"q","Antiparticle"->"Q","Mass"->1,"Spin2"->1|>,
 (UndirectedEdge[7,8])-><|"Particle"->"h","Antiparticle"->"h","Mass"->1,"Spin2"->0|>|>,
 "VertexStructures"-><|6->{c7Structure[{"q","Q","h"},c7Model[[2,1]]]},
 7->{c7Structure[{"Q","q","h"},c7Model[[2,1]]]},8->{c7Structure[{"h","h","Z"},c7Model[[2,2]]]}|>|>;
c7ChangeVertex[c7Input_Association,c7Vertex_Integer,c7Replacement_Association]:=Module[{c7Structures=c7Input["VertexStructures"]},
 c7Structures[c7Vertex]={Join[First[c7Structures[c7Vertex]],c7Replacement]};Join[c7Input,<|"VertexStructures"->c7Structures|>]];
c7Path[c7Input_,c7M_:c7Model]:=ConstructiveDiagrams`Private`colorSpectatorPath[c7Input,c7M,{1,2}];
c7ExpectedRejections=<|
 "explicit color in second vertex aborts whole certificate"->"ExplicitVertexColorRequiresSeparateTransportCertificate",
 "explicit color in last vertex cannot emit partial certificate"->"ExplicitVertexColorRequiresSeparateTransportCertificate",
 "two fundamental ports reject"->"NonDeltaVertexColorFlow","one colored port rejects"->"NonDeltaVertexColorFlow",
 "unknown vertex particle rejects"->"UnsupportedVertexColorRepresentation",
 "unsupported particle representation rejects"->"NonSpectatorInternalColorRepresentation",
 "colored scalar vertex ports reject"->"NonFermionColoredPath",
 "colored external spectator boson rejects"->"NonSpectatorInternalColorRepresentation",
 "internal adjoint rejects"->"NonSpectatorInternalColorRepresentation",
 "internal fundamental scalar rejects"->"NonSpectatorInternalColorRepresentation",
 "unknown internal representation rejects"->"NonSpectatorInternalColorRepresentation",
 "split colored path rejects"->"ColoredEdgesAreNotOneOpenFermionPath",
 "closed color loop rejects"->"ColoredEdgesAreNotOneOpenFermionPath",
 "disconnected extra color flow rejects"->"ColoredEdgesAreNotOneOpenFermionPath"|>;
c7Rejects[c7Name_,c7Input_,c7M_:c7Model]:=Module[{c7Result=c7Path[c7Input,c7M]},
 c7Assert[c7Name,FailureQ[c7Result]&&c7Result[[1]]===c7ExpectedRejections[c7Name]];
 c7Records[[-1]]=Append[Last[c7Records],"FailureTag"->If[FailureQ[c7Result],c7Result[[1]],Null]]];
c7Positive=c7Path[c7Diagram];c7Assert["ordinary fundamental path certifies",AssociationQ[c7Positive]];
c7Assert["positive path has both external endpoints",c7Positive["ColoredPath"]==={1,6,7,2}];
c7Assert["all three ordered vertex certificates retained",Length[c7Positive["VertexCertificates"]]===3];
c7Assert["positive path declares no adjoint or closed loop",c7Positive["InternalAdjointPresent"]===False&&c7Positive["ClosedColorLoopPresent"]===False];
c7Rejects["explicit color in second vertex aborts whole certificate",c7ChangeVertex[c7Diagram,7,<|"ModelRow"->ReplacePart[c7Model[[2,1]],-1->ColorDelta[ColorIndex[1],ColorIndex[2]]]|>]];
c7Rejects["explicit color in last vertex cannot emit partial certificate",c7ChangeVertex[c7Diagram,8,<|"ModelRow"->ReplacePart[c7Model[[2,2]],-1->ColorGenerator[ColorIndex[1],ColorIndex[2],1]]|>]];
c7Rejects["two fundamental ports reject",c7ChangeVertex[c7Diagram,7,<|"IncidentLegs"->(c7Leg/@{"q","q","h"})|>]];
c7Rejects["one colored port rejects",c7ChangeVertex[c7Diagram,7,<|"IncidentLegs"->(c7Leg/@{"q","h","h"})|>]];
c7Rejects["unknown vertex particle rejects",c7ChangeVertex[c7Diagram,7,<|"IncidentLegs"->(c7Leg/@{"q","unknown","h"})|>]];
c7Rejects["unsupported particle representation rejects",c7Diagram,ReplacePart[c7Model,{1,1,8}->6]];
c7Rejects["colored scalar vertex ports reject",c7Diagram,ReplacePart[c7Model,{1,1,5}->0]];
c7Rejects["colored external spectator boson rejects",c7Diagram,ReplacePart[c7Model,{1,2,8}->3]];
c7Props=c7Diagram["Propagators"];c7Props[UndirectedEdge[6,7]]=<|"Particle"->"g","Antiparticle"->"g","Mass"->0,"Spin2"->2|>;
c7Rejects["internal adjoint rejects",Join[c7Diagram,<|"Propagators"->c7Props|>]];
c7Props=c7Diagram["Propagators"];c7Props[UndirectedEdge[6,7]]=Join[c7Props[UndirectedEdge[6,7]],<|"Spin2"->0|>];
c7Rejects["internal fundamental scalar rejects",Join[c7Diagram,<|"Propagators"->c7Props|>]];
c7Props=c7Diagram["Propagators"];c7Props[UndirectedEdge[6,7]]=Join[c7Props[UndirectedEdge[6,7]],<|"Particle"->"unknown"|>];
c7Rejects["unknown internal representation rejects",Join[c7Diagram,<|"Propagators"->c7Props|>]];
c7Rejects["split colored path rejects",Join[c7Diagram,<|"Graph"->EdgeDelete[c7Diagram["Graph"],UndirectedEdge[6,7]]|>]];
c7Props=c7Diagram["Propagators"];c7Props[UndirectedEdge[6,8]]=c7Props[UndirectedEdge[6,7]];c7Props[UndirectedEdge[7,8]]=c7Props[UndirectedEdge[6,7]];
c7Rejects["closed color loop rejects",Join[c7Diagram,<|"Graph"->EdgeAdd[c7Diagram["Graph"],UndirectedEdge[6,8]],"Propagators"->c7Props|>]];
c7Props=c7Diagram["Propagators"];c7Props[UndirectedEdge[8,9]]=c7Props[UndirectedEdge[6,7]];
c7Rejects["disconnected extra color flow rejects",Join[c7Diagram,<|"Graph"->EdgeAdd[c7Diagram["Graph"],UndirectedEdge[8,9]],"Propagators"->c7Props|>]];
c7Assert["unknown external field rejects before generation",FailureQ[ConstructiveDiagrams`Private`colorSpectatorSource[{"unknown","Q","h","h","h"},c7Model,c7Model]]];
c7Assert["two fundamental pairs reject before generation",FailureQ[ConstructiveDiagrams`Private`colorSpectatorSource[{"q","Q","q","Q","h"},c7Model,c7Model]]];
c7SourceBefore=DownValues[CreateDiagrams];c7BasisBefore=DownValues[ConstructiveDiagrams`Private`colorDiagramBasis];
Do[c7Tensor=First[c7Entry];c7Index=Last[c7Entry];c7Result=Internal`InheritedBlock[{CreateDiagrams,ConstructiveDiagrams`Private`colorDiagramBasis},
 Unprotect[CreateDiagrams];Clear[CreateDiagrams];CreateDiagrams[___]:={c7Diagram};
 Clear[ConstructiveDiagrams`Private`colorDiagramBasis];
 ConstructiveDiagrams`Private`colorDiagramBasis[___]:=<|"DiagramTensors"->{c7Tensor}|>;
 ConstructiveDiagrams`Private`colorSpectatorSource[{"q","Q","h","h","Z"},c7Model,c7Model]];
 c7Assert["nonunit or extra color tensor rejects: "<>ToString[c7Index],FailureQ[c7Result]&&c7Result[[1]]==="NonCommonOrMisnormalizedSpectatorTensor"],
 {c7Entry, {{0,1},{2 ColorDelta[ColorIndex[2],ColorIndex[1]],2},
 {-ColorDelta[ColorIndex[2],ColorIndex[1]],3},{Sqrt[3] ColorDelta[ColorIndex[2],ColorIndex[1]],4},
 {1+ColorDelta[ColorIndex[2],ColorIndex[1]],5}}}];
c7Assert["all twenty-five path and scope cases executed",Length[c7Records]===25];
c7Assert["mocked production definitions restore exactly",c7SourceBefore===DownValues[CreateDiagrams]&&c7BasisBefore===DownValues[ConstructiveDiagrams`Private`colorDiagramBasis]];
c7Summary=<|"Status"->If[c7Failed===0,"PASS","FAIL"],"Passed"->c7Passed,"Failed"->c7Failed,"Records"->c7Records,
 "ProductionDefinitionsRestored"->(c7SourceBefore===DownValues[CreateDiagrams]&&c7BasisBefore===DownValues[ConstructiveDiagrams`Private`colorDiagramBasis]),
 "FivePointSourceConstructionRun"->False,"IndependentBenchmarkRead"->False|>;
Print[c7Summary];
Print["CHECKS: ",c7Passed+c7Failed,"; FAILURES: ",c7Failed];
If[c7Failed===0,Print["HEPCAT_TEST_COMPLETED:color-spectator-source-tests.wl"]];
End[];Quit[If[c7Failed===0,0,1]];
