(* Opt-in notebook-equivalent massive-vector constructions. No amplitude,
   correction basis or numerical effort override is supplied. *)

(*
HEPCAT - High Energy Physics Constructive Amplitude Tool

Copyright (C) 2026 Neil Christensen

This file is part of HEPCAT.

HEPCAT is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

HEPCAT is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with HEPCAT.  If not, see <https://www.gnu.org/licenses/>.
*)


svciRoot=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{svciRoot,"source","HEPCAT.wl"}]];
$HistoryLength=0;svciChecks=0;svciFailures={};svciResults=<||>;
SetAttributes[svciCheck,HoldRest];
svciCheck[name_,assertion_]:=(svciChecks++;
 If[!TrueQ[assertion],AppendTo[svciFailures,name];Print["FAIL: ",name]]);
svciCases=<|"ZZWW"->{"Z","Z","W-","W+"},"WWWW"->{"W+","W+","W-","W-"}|>;
svciBounds=<|"ZZWW"->63,"WWWW"->216|>;
svciChoice=Environment["HEPCAT_SM_VECTOR_CASES"];
svciChoice=If[MemberQ[{$Failed,""},svciChoice],Keys[svciCases],
 DeleteDuplicates[StringTrim/@StringSplit[svciChoice,","]]];
If[svciChoice==={}||!AllTrue[svciChoice,KeyExistsQ[svciCases,#]&],Quit[2]];
svciModel=ReadModel[FileNameJoin[{svciRoot,"models"}],1];
svciFeynman=ReadModel[FileNameJoin[{svciRoot,"tests", "reference-models", "calchep"}],1];
svciBefore={svciModel,svciFeynman};svciDirectory=CreateDirectory[];
Print["SM VECTOR CONSTRUCTION REPORTS: ",svciDirectory];
Do[
 svciStarted=AbsoluteTime[];
 svciReport=ConstructAmplitude[svciCases[svciName],svciModel,svciModel,
  "FeynmanModel"->svciFeynman,"RandomSeed"->2026110617,
  "Output"->"Details","ProgressReporting"->False,"PrintReport"->False];
 svciComplete=AssociationQ[svciReport]&&Lookup[svciReport,"Status",None]==="COMPLETE";
 svciCheck[svciName<>" default construction completes",svciComplete];
 If[AssociationQ[svciReport],Export[FileNameJoin[{svciDirectory,svciName<>".wxf"}],
  KeyTake[svciReport,{"Status","Process","Amplitude","AmplitudePair","Complexity","Checks","StageTimings","Failure"}],"WXF"]];
 If[svciComplete,
  svciPair=svciReport["AmplitudePair"];svciChannels=FactorizationAmplitudeChannels[svciCases[svciName],svciModel];
  svciRows=svciReport["Checks"]["Checks"];
  svciPhysical=Select[svciRows,Lookup[#,"Type",None]==="Physical"&];
  svciBenchmark=Select[svciRows,Lookup[#,"Type",None]==="Feynman"&];
  svciHE=Lookup[Lookup[svciReport,"Stages",<||>],"Completing high-energy behavior",<||>];
  svciCheck[svciName<>" every original physical channel passes",
   Length[svciPhysical]===Length[svciChannels]&&AllTrue[svciPhysical,
    Lookup[#,"Status",None]==="PASS"&&Lookup[#,"CompletedSamples",0]>=4&]];
  svciCheck[svciName<>" every mandatory final row passes",
   svciRows=!={}&&AllTrue[svciRows,Lookup[#,"Status",None]==="PASS"&]];
  svciCheck[svciName<>" independent all-spin Feynman comparison passes with unchanged sign",
   svciBenchmark=!={}&&AllTrue[svciBenchmark,Lookup[#,"Status",None]==="PASS"&&
    MemberQ[Lookup[#,"MatchingSigns",{}],1]&&Lookup[#,"OverallSign",None]===1&&
    Lookup[#,"CompletedSamples",0]>=4&&Lookup[#,"NormalizationFitted",None]===False&]];
  svciSelection=Lookup[svciReport,"RepresentativeSelection",<||>];
  svciValidations=Lookup[svciSelection,"ValidationReports",{}];
  svciCheck[svciName<>" a selected affine point passes all three independent parameter assignments",
   !TrueQ[Lookup[svciSelection,"Selected",False]]||
    (Lookup[svciSelection,"BenchmarkUsedForConstruction",True]===False&&
     Lookup[svciSelection,"UniquenessEstablished",True]===False&&
     Length[svciValidations]===3&&
     AllTrue[svciValidations,With[{rows=Lookup[Lookup[#,"Report",<||>],"Checks",{}]},
      rows=!={}&&AllTrue[rows,Lookup[#,"Status",None]==="PASS"&]]&])];
  svciCheck[svciName<>" high-energy stage does not expire",
   AssociationQ[svciHE]&&!MemberQ[Lookup[svciHE,"Diagnostics",{}],
    "High-energy completion exceeded TimeConstraint. The best retained candidate is provided for inspection, not claimed complete."]];
  svciCheck[svciName<>" final preparation achieves the known compact automatic score",
   svciReport["Complexity"]===SpinorExpressionComplexity[Last[svciPair]]&&
    svciReport["Complexity"]<=svciBounds[svciName]];
  svciCheck[svciName<>" physical pole preference is achieved",
   TrueQ[svciReport["AchievedPhysicalDenominators"]]];
  svciCheck[svciName<>" final mass metadata is external only and references are resolved",
   MatchQ[svciPair,{{__Rule},_}]&&FreeQ[First[svciPair],_Multiparticle]&&
    FreeQ[Last[svciPair],Spinor["Xi"|"Zeta",___]|_SpinorHat|_MomHat|_xFactor|_xTildeFactor]];
  AssociateTo[svciResults,svciName-><|"Status"->"COMPLETE","Complexity"->svciReport["Complexity"],
   "Seconds"->N[AbsoluteTime[]-svciStarted],"Summary"->svciReport["Checks"]["Summary"]|>]];
 Print["SM VECTOR RESULT ",svciName," ",If[AssociationQ[svciReport],
  KeyTake[svciReport,{"Status","Complexity","TotalTime"}],Head[svciReport]]],
 {svciName,svciChoice}];
svciCheck["supplied model lists are unchanged",svciBefore==={svciModel,svciFeynman}];
Print["SM VECTOR CONSTRUCTION RESULTS: ",InputForm[svciResults]];
Print["CHECKS: ",svciChecks,"; FAILURES: ",Length[svciFailures]];
If[svciFailures=!={},Print[svciFailures]];
Print["HEPCAT_TEST_COMPLETED:sm-vector-construction-integration-tests.wl"];
Quit[If[svciFailures==={},0,1]];
