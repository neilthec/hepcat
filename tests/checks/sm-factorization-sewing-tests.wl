(* Five-point sewing of exported SM four-point amplitudes against Feynman residues. *)

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


Begin["SMFactorizationSewingTests`Private`"];
root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root,"source","HEPCAT.wl"}]];
$HistoryLength=0;
model=ReadModel[FileNameJoin[{root,"models"}],2];
feynman=ReadModel[FileNameJoin[{root,"tests","reference-models","calchep"}],1];
processes={{"e","E","A.+","A.+","A.-"},
 {"u","U","A.+","A.+","A.-"},
 {"h","h","h","h","h"},
 {"u","U","G.+","A.+","A.-"}};
If[Environment["HEPCAT_SM_FACTOR_SEWING_CASES"]==="higgs",processes={processes[[3]]}];
checks=0;failures={};
check[name_,value_]:=(checks++;If[!TrueQ[value],AppendTo[failures,name];Print["FAIL: ",name]]);
Do[
 Print["Checking five-point sewing: ",process];
 channels=FactorizationAmplitudeChannels[process,model];
 pairs=PrepareAmplitudes[DiagramAmplitudes[CreateDiagrams[process,feynman]],feynman,
  "PolePreference"->"IndependentXi","MaxSteps"->0,"ProgressReporting"->False];
 (* Rebind model parameters by their declared names. The reference model has
    its own independent scope; all parameters used in these cases have the
    same matched physical definitions as the constructive SM model. *)
 parameters=Map[Function[p,p->ModelParameter[model,ModelParameterName[p]]],
  DeleteDuplicates[Cases[pairs,_ModelParameter,Infinity]]];
 amplitude=Total[Last/@pairs]/.parameters;
 report=CheckAmplitudes[amplitude,channels,model,
  "HighEnergyCheck"->False,"ReferenceCheck"->False,"SpuriousPoles"->{},
  "Samples"->1,"RandomSeed"->20261111,"TimeConstraint"->60,
  "Parallelization"->False,"Output"->"Details"];
 rows=If[AssociationQ[report],If[KeyExistsQ[report,"Checks"],report["Checks"],report["Channels"]],{}];
 check["nonempty lower-point channels "<>ToString[process,InputForm],Length[channels]>0];
 check["all independent Feynman residues match exported lower amplitudes "<>ToString[process,InputForm],
  AssociationQ[report]&&Length[rows]>0&&AllTrue[rows,#["Status"]==="PASS"&]];
 Print[If[AssociationQ[report],report["Summary"],report]],
 {process,processes}];
Print["SM FACTORIZATION SEWING CHECKS: ",checks,"; FAILURES: ",Length[failures]];
If[failures==={},Print["HEPCAT_TEST_COMPLETED:sm-factorization-sewing-tests.wl"],Print[failures]];
End[];
Quit[If[SMFactorizationSewingTests`Private`failures==={},0,1]];
