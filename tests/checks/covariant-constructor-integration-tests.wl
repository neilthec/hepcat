(* Actual public constructor calls. No candidate/benchmark injection or saved answer. *)
root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root,"source","HEPCAT.wl"}]];
checks=0;failures={};SetAttributes[check,HoldRest];
check[name_,test_]:=(checks++;If[!TrueQ[test],AppendTo[failures,name];Print["FAIL: ",name]]);
ciModel=ReadModel[FileNameJoin[{root,"models"}],1];
ciLower=ReadModel[FileNameJoin[{root,"models"}],2];
ciIndependent=ReadModel[FileNameJoin[{root,"tests","reference-models","calchep"}],1];
ciCases={{{"e","E","m","M","h"},4},{{"e","E","e","E","h"},3},{{"e","E","Z","h","h"},3}};
ciSelection=Environment["HEPCAT_COVARIANT_INTEGRATION_CASE"];
If[StringQ[ciSelection]&&MemberQ[{"1","2","3"},ciSelection],ciCases=ciCases[[{ToExpression[ciSelection]}]]];
ciDirectory=Environment["HEPCAT_COVARIANT_INTEGRATION_OUTPUT"];
If[!StringQ[ciDirectory]||ciDirectory==="",ciDirectory=CreateDirectory[],
 If[!DirectoryQ[ciDirectory],CreateDirectory[ciDirectory,CreateIntermediateDirectories->True]]];
ciSHA[file_]:=IntegerString[FileHash[file,"SHA256"],16,64];
ciGuardFiles=Join[Sort[FileNames["*.wl",FileNameJoin[{root,"source"}],Infinity]],
 Flatten[Table[FileNameJoin[{root,"models",stem<>ToString[number]<>".mdl"}],
 {number,{1,2}},{stem,{"amps","prtcls","vars","func","extlib"}}]],
 Sort[FileNames["*1.mdl",FileNameJoin[{root,"tests","reference-models","calchep"}]]]];
ciHashes[]:=Association[(StringDrop[#,StringLength[root]+1]->ciSHA[#])&/@ciGuardFiles];
ciBeforeHashes=ciHashes[];ciReports={};
Print["Covariant constructor integration records: ",ciDirectory];
Do[
 ciReport=MemoryConstrained[ConstructAmplitude[First[one],ciModel,ciLower,
   "FeynmanModel"->ciIndependent,"Output"->"Details"],4*2^30,Missing["MemoryLimit"]];
 AppendTo[ciReports,<|"Particles"->First[one],"Report"->ciReport,
  "SourceAndModelHashesBefore"->ciBeforeHashes,"SourceAndModelHashesAfter"->ciHashes[],
  "ActualPublicOptions"->{"FeynmanModel"->"MatchedIndependentModel","Output"->"Details"}|>];
 Export[FileNameJoin[{ciDirectory,"case-"<>ToString[Length[ciReports]]<>".wxf"}],Last[ciReports],"WXF"];
 check["actual public construction completed "<>ToString[First[one],InputForm],
  AssociationQ[ciReport]&&Lookup[ciReport,"Status",None]==="COMPLETE"];
 check["the complete source candidate was accepted only by independent native validation",
  AssociationQ[ciReport]&&Lookup[Lookup[ciReport,"RepresentativeSelection",<||>],"SourceStage",None]==="Certified covariant source"&&
  Length[Lookup[ciReport["RepresentativeSelection"],"ValidationReports",{}]]===Last[one]&&
  AllTrue[ciReport["RepresentativeSelection"]["ValidationReports"],
   AssociationQ[#["Report"]]&&AllTrue[#["Report"]["Checks"],Lookup[#,"Status",None]==="PASS"&]&&
   Lookup[Lookup[#["Report"],"FeynmanCheck",<||>],"Status",None]==="PASS"&]];
 check["candidate provenance retains no internal shells sampled fitting or independent source",
  With[{proposal=Lookup[Lookup[ciReport,"Stages",<||>],"Covariant source proposal",<||>]},
   Lookup[proposal,"Status",None]==="UNVALIDATED_CANDIDATE"&&
   Lookup[proposal,"InternalOnShellConditions",None]==={}&&
   Lookup[proposal,"NumericalCoefficientFit",None]===False&&
   Lookup[proposal,"IndependentBenchmarkRead",None]===False&&
   TrueQ[Lookup[proposal,"EveryOriginalDiagramCovered",False]]]];
 If[!AssociationQ[ciReport]||Lookup[ciReport,"Status",None]=!="COMPLETE",Break[]],{one,ciCases}];
If[ciBeforeHashes=!=ciHashes[],AppendTo[failures,"Source or model changed during public construction"]];
If[Length[ciReports]=!=Length[ciCases]||checks=!=3Length[ciCases],AppendTo[failures,"Not every public integration case completed"]];
Print["COVARIANT CONSTRUCTOR INTEGRATION CHECKS: ",checks,"; FAILURES: ",Length[failures]];
If[failures=!={},Print[failures]];
If[failures==={},Print["HEPCAT_TEST_COMPLETED:covariant-constructor-integration-tests.wl"]];
Quit[If[failures==={},0,1]];
