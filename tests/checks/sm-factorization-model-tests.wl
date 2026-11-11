(* Complete SM four-point coverage, exact flavor certificates and export readback. *)

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


root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root,"source"}];
Get[FileNameJoin[{$HEPCATpath,"HEPCAT.wl"}]];
$HistoryLength = 0;
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value],AppendTo[failures,name];Print["FAIL: ",name]]);
elementary = ReadModel[FileNameJoin[{root,"models"}],1];
full = ReadModel[FileNameJoin[{root,"models"}],2];
provenance = Import[FileNameJoin[{root,"tests","checks","fixtures","sm-factorization-catalog.json"}],"RawJSON"];
parts[row_] := DeleteCases[Drop[row,-2],""];
cubics = Select[elementary[[2]],Length[parts[#]]===3&];
fours = Select[full[[2]],Length[parts[#]]===4&];
check["model 2 retains the elementary particles, parameters and functions",
 full[[{1,3,4}]] === elementary[[{1,3,4}]]];
check["every elementary cubic is retained exactly once",
 Select[full[[2]],Length[parts[#]]===3&] === cubics && Length[cubics]===71];
check["model 1 retains its original elementary quartics",
 Sort[Sort[parts[#]]&/@Select[elementary[[2]],Length[parts[#]]===4&]] ===
 Sort[Sort/@{{"h","h","h","h"},{"W+","W-","h","h"},{"Z","Z","h","h"}}]];
check["model 2 contains only cubics and complete four-point amplitudes",
 Length[full[[2]]]===Length[cubics]+Length[fours] && Length[fours]===357];
check["all four-point multisets are unique",
 Length[DeleteDuplicates[Sort[parts[#]]&/@fours]]===Length[fours]];
anti = Association[Flatten[({#[[2]]->#[[3]],#[[3]]->#[[2]]}&/@elementary[[1]]),1]];
triples = parts /@ cubics;
tree = Last[Reap[
 Do[If[v[[i]] === anti[w[[j]]],Sow[Sort[Join[Delete[v,i],Delete[w,j]]]]],
 {v,triples},{w,triples},{i,3},{j,3}]]];
tree = DeleteDuplicates[Join[Flatten[tree,1],
 Sort[parts[#]]&/@Select[elementary[[2]],Length[parts[#]]===4&]]];
zeros = Sort /@ provenance["KnownZeroMultisets"];
check["the exhaustive elementary tree inventory has 359 external multisets",Length[tree]===359];
check["all supported trees are present without redundant zero rows",
 Sort[Sort[parts[#]]&/@fours]===Sort[Complement[tree,zeros]]];
check["unlisted helicity zeros use the ordinary missing-amplitude semantics",
 Select[fours,MemberQ[zeros,Sort[parts[#]]]&]==={} && Length[zeros]===4 &&
 AllTrue[zeros, LookupAmplitude[Thread[{#,Range[4]}],full,False,{i,j,k,l}]===0&]];
check["complete rows contain no leftover reference spinors or x factors",
 FreeQ[fours,Spinor["Xi",___]|_xFactor|_xTildeFactor]];
check["all parameters belong to the returned model",
 Complement[DeleteDuplicates[Cases[fours,_ModelParameter,Infinity]],
 DeleteDuplicates[First/@Join[full[[3]],full[[4]]]]] === {}];
(* Certify each generation transformation on the complete elementary model,
   including all equality/distinct-flavor patterns. Widths are disabled in
   these bare-tree validations, so no width substitution is claimed here. *)
certifiedRules = Table[
 names=Normal[mapping["ParticleMap"]];
 parameters=KeyValueMap[ModelParameter[elementary,#1]->ModelParameter[elementary,#2]&,mapping["MassMap"]];
 vertexOK=AllTrue[elementary[[2]],Function[row,Module[{target},
  target=Select[elementary[[2]],Drop[#,-2]===(Drop[row,-2]/.names)&];
  Length[target]===1 && SameQ[(row[[-2]]row[[-1]])/.parameters,First[target][[-2]]First[target][[-1]]]]]];
 particleOK=AllTrue[elementary[[1]],Function[row,Module[{target},
  target=SelectFirst[elementary[[1]],#[[2]]===(row[[2]]/.names)&];
  ListQ[target] && SameQ[row[[{5,6,8,9}]]/.parameters,target[[{5,6,8,9}]]]]]];
 check["exact elementary flavor certificate "<>ToString[index],vertexOK&&particleOK];
 parameters,
 {index,Length[provenance["FlavorMaps"]]},
 {mapping,{provenance["FlavorMaps"][[index]]}}];
certifiedRules=Flatten[certifiedRules,1];
check["all 36 simultaneous generation permutations were certified",Length[certifiedRules]===36];
(* Read only saved Output expressions. Never evaluate construction inputs. *)
held=ToExpression[Import[FileNameJoin[{root,"tests","SM-4-point-construction.nb"}],"Text"],InputForm,HoldComplete];
groups=Cases[held,Cell[CellGroupData[{Cell[source_String,"Input",___],children___},_],___]/;
 StringContainsQ[source,"ConstructAmplitude["]:>{source,HoldComplete[children]},Infinity];
sourceRecords=Table[
 id=First[StringCases[g[[1]],StartOfString~~name:("sm"~~DigitCharacter..~~LetterCharacter...)~~"Particles":>name]];
 outputs=Cases[g[[2]],Cell[BoxData[boxes_],"Output",___]:>HoldComplete[boxes],Infinity];
 id->If[Length[outputs]===1,
  ReleaseHold[Replace[First[outputs],HoldComplete[boxes_]:>ToExpression[boxes,StandardForm,HoldComplete]]],$Failed],
 {g,groups}];
sources=Association[sourceRecords];
check["the live worked notebook contains all 138 saved construction results",
 Length[sourceRecords]===138 && FreeQ[sources,$Failed|$Aborted|_Failure]];
Do[
 source=Lookup[sources,entry["SourceID"],$Failed];
 expected=source/.certifiedRules[[entry["FlavorMap"]+1]];
 stored=Select[fours,parts[#]===entry["Particles"]&];
 check["saved amplitude matches certified notebook source "<>ToString[entry["Particles"],InputForm],
  source =!= $Failed && Length[stored]===1 && SameQ[First[stored][[-2]]First[stored][[-1]],expected]],
 {entry,provenance["Entries"]}];
Print["SM FACTORIZATION MODEL CHECKS: ",checks,"; FAILURES: ",Length[failures]];
If[failures==={},Print["HEPCAT_TEST_COMPLETED:sm-factorization-model-tests.wl"],Print[failures]];
Quit[If[failures==={},0,1]];
