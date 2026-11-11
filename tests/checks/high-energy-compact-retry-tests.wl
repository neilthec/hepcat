(* Exact ordinary compaction competes within the existing identity-proof clock. *)

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
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0; checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ",name]]);
hcModel = ReadModel[FileNameJoin[{root,"models"}],3];
hcRules = {Mass[1]->hcMass,Mass[2]->hcMass,Mass[3]->hcMass,Mass[4]->hcMass,Mass[5]->0,Mass[6]->0};
(* These are two retained coordinates, not a stored physical amplitude. Only
   their common charge factor was removed; mass and spin dependence remain. *)
hcDirections = Get[FileNameJoin[{root,"tests","checks","fixtures","high-energy-four-fermion-zero.wl"}]][hcMass];
check["the real fixture retains two nonliteral affine directions", Length[hcDirections]===2 && AllTrue[hcDirections,Length[DeleteDuplicates[Cases[#,_SpinorChain,Infinity]]]>10&&LeafCount[#]>1000&]];
hcProofs = Table[ConstructiveDiagrams`Private`completionHEZeroDirection[direction,hcRules,hcModel,30],{direction,hcDirections}];
Do[hcProof=hcProofs[[i]];hcRetry=hcProof["OrdinaryCompactionRetry"];
 check["a real direction has an exact generic certificate "<>ToString[i], hcProof["Status"]==="PROVED_ZERO"&&hcProof["Method"]==="DenseSymbolicExternalChart"&&hcProof["NumericalKinematicSamples"]===0];
 check["the original direction remains in its certificate "<>ToString[i], hcProof["OriginalDirection"]===hcDirections[[i]]];
 check["the ordinary compact retry is measured and exact "<>ToString[i], hcProof["ProofInput"]==="OrdinaryCompacted"&&AssociationQ[hcRetry]&&hcRetry["Status"]==="IMPROVED"&&hcRetry["Route"]=!={}&&hcRetry["NumericalKinematicSamples"]===0&&hcRetry["InternalOnShellConditions"]==={}];
 check["the compact replay reaches the symbolic proof input "<>ToString[i], hcProof["ReducedLeafCount"]===LeafCount[hcRetry["Amplitude"]]&&hcRetry["Input"]=!=hcRetry["Amplitude"]&&AllTrue[hcRetry["Route"],Lookup[#,"InternalOnShellConditions",{}]==={}&]];
 check["no internal on-shell condition entered the final certificate "<>ToString[i], hcProof["InternalOnShellConditions"]==={}&&hcRetry["CoherentComponentEstimate"]===16];
 check["the direction proof stays inside its original finite allowance "<>ToString[i], hcProof["ElapsedTime"]<32],{i,2}];

hcTensor=SpinorChain[Spinor["Spin","Angle",1],Spinor["Spin","Angle",2]] SpinorChain[Spinor["Spin","Square",3],Spinor["Spin","Square",4]] SpinorChain[Spinor["Helicity","Square",5],Mom[1],Spinor["Helicity","Angle",6]]^2;
hcBad=ConstructiveDiagrams`Private`completionHEZeroDirection[First[hcDirections]+hcTensor/10^30,hcRules,hcModel,30];
check["a tiny genuine coherent deformation is not deleted", hcBad["Status"]==="NOT_PROVED"];
check["the genuine deformation retains its original affine coordinate", hcBad["OriginalDirection"]===First[hcDirections]+hcTensor/10^30];
hcStarted=AbsoluteTime[];hcStopped=ConstructiveDiagrams`Private`completionHEZeroDirection[First[hcDirections],hcRules,hcModel,1/1000];
check["an expired proof retains the direction rather than certifying zero", hcStopped["Status"]==="NOT_PROVED"&&hcStopped["OriginalDirection"]===First[hcDirections]];
check["a tiny finite deadline does not start an unbounded retry", AbsoluteTime[]-hcStarted<2];
hcUnsupported=ConstructiveDiagrams`Private`completionHEZeroDirection[hcTensor SpinorChain[Spinor["Xi","Angle",7],Spinor["Helicity","Angle",6]],hcRules,hcModel,10];
check["unsupported references never get an identity certificate", hcUnsupported["Status"]==="NOT_PROVED"&&hcUnsupported["Method"]==="PreservedDirection"];

hcTwoRules={Mass[1]->hcMass,Mass[2]->hcMass,Mass[3]->0,Mass[4]->0,Mass[5]->0,Mass[6]->0};
hcTwo=Get[FileNameJoin[{root,"tests","checks","fixtures","high-energy-six-point-zero.wl"}]][hcMass];
hcTwoProof=ConstructiveDiagrams`Private`completionHEZeroDirection[hcTwo,hcTwoRules,hcModel,40];
check["the established two-fermion workflow retains its original chart path", hcTwoProof["Status"]==="PROVED_ZERO"&&hcTwoProof["Method"]==="DenseSymbolicExternalChart"&&MissingQ[hcTwoProof["OrdinaryCompactionRetry"]]];
Print[checks," high-energy compact-retry checks; ",Length[failures]," failures."];
If[failures=!={},Print[failures];Exit[1]];Print["HEPCAT_TEST_COMPLETED:high-energy-compact-retry-tests.wl"];Exit[0];
