(* Symbolic SU(3) contractions are checked against explicit lambda/2 matrices. *)

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
Get[FileNameJoin[{root,"source","HEPCAT.wl"}]];
$HistoryLength=0;
checks=0;failures={};
SetAttributes[check,HoldRest];
check[name_,assertion_] := (checks++;If[!TrueQ[assertion],AppendTo[failures,name];Print["FAIL: ",name]]);
crT={
 {{0,1,0},{1,0,0},{0,0,0}}/2,
 {{0,-I,0},{I,0,0},{0,0,0}}/2,
 DiagonalMatrix[{1,-1,0}]/2,
 {{0,0,1},{0,0,0},{1,0,0}}/2,
 {{0,0,-I},{0,0,0},{I,0,0}}/2,
 {{0,0,0},{0,0,1},{0,1,0}}/2,
 {{0,0,0},{0,0,-I},{0,I,0}}/2,
 DiagonalMatrix[{1,1,-2}]/(2 Sqrt[3])};
crF=Table[Simplify[-2 I Tr[(crT[[a]].crT[[b]]-crT[[b]].crT[[a]]).crT[[c]]]],
 {a,8},{b,8},{c,8}];
crInternal[label_,dimension_] := ConstructiveDiagrams`Private`colorInternalIndex[label,dimension];
check["color public heads have the package context",
 Context[ColorTrace]==="ConstructiveDiagrams`" && Context[ReduceColor]==="ConstructiveDiagrams`"];
check["the trace of the identity has three fundamental states",ReduceColor[ColorTrace[]]===3];
check["a single generator is traceless",ReduceColor[ColorTrace[crA]]===0];
check["the two-generator trace has the standard normalization",
 ReduceColor[ColorTrace[crA,crB]]===ColorDelta[crA,crB]/2];
check["a summed adjoint generator trace has value four",ReduceColor[ColorTrace[crA,crA]]===4];
check["a powered trace retains both repeated adjoint sums",ReduceColor[ColorTrace[crA,crB]^2]===2];
check["a generator pair separated in one trace has the negative Fierz term",
 ReduceColor[ColorTrace[crA,crB,crA,crB]]===-2/3];
crLongReference=Simplify[Sum[Tr[crT[[a]].crT[[b]].crT[[c]].crT[[a]].crT[[b]].crT[[c]]],
 {a,8},{b,8},{c,8}]];
check["a six-generator trace agrees with direct matrices",
 ReduceColor[ColorTrace[crA,crB,crC,crA,crB,crC]]===crLongReference];
crCycle=Times[ColorGenerator[crI1,crI2,crA],ColorGenerator[crI2,crI3,crB],
 ColorGenerator[crI3,crI4,crC],ColorGenerator[crI4,crI5,crA],
 ColorGenerator[crI5,crI6,crB],ColorGenerator[crI6,crI1,crC]];
check["closed indexed generator cycles are traced beyond the former length-four limit",
 ReduceColor[crCycle]===crLongReference];
check["cyclic order is equivalent without reversing a trace",
 ReduceColor[ColorTrace[crA,crB,crC]-ColorTrace[crB,crC,crA]]===0];
check["a real structure constant uses the correct imaginary trace coefficient",
 ReduceColor[ColorStructure[crA,crB,crC]+2 I(ColorTrace[crA,crB,crC]-ColorTrace[crA,crC,crB])]===0];
check["the squared structure constant sums to twenty-four",
 ReduceColor[ColorStructure[crA,crB,crC]^2]===24 && Total[Flatten[crF^2]]===24];
check["an adjoint contraction has Casimir three",
 ReduceColor[ColorStructure[crA,crB,crC] ColorStructure[crD,crB,crC]]===3 ColorDelta[crA,crD]];
check["antisymmetric repeated external structure slots vanish",
 ReduceColor[ColorStructure[ColorIndex[1],ColorIndex[1],ColorIndex[2]]]===0];
crMixed=ReduceColor[ColorStructure[crA,crB,ColorIndex[3]]
 ColorGenerator[ColorIndex[1],crJ,crA] ColorGenerator[crJ,ColorIndex[2],crB]];
check["structure constants and ordered open generators retain their phase",
 crMixed===(3 I/2) ColorGenerator[ColorIndex[1],ColorIndex[2],ColorIndex[3]]];
check["the mixed contraction phase agrees independently with explicit matrices",
 And @@ Table[Simplify[Sum[crF[[a,b,c]] crT[[a]].crT[[b]],{a,8},{b,8}]-
   (3 I/2) crT[[c]]]===ConstantArray[0,{3,3}],{c,8}]];
check["the fundamental completeness identity retains its crossed color tensor",
 ReduceColor[ColorGenerator[ColorIndex[1],ColorIndex[2],crA]
  ColorGenerator[ColorIndex[3],ColorIndex[4],crA]]===
  ColorDelta[ColorIndex[1],ColorIndex[4]] ColorDelta[ColorIndex[2],ColorIndex[3]]/2-
  ColorDelta[ColorIndex[1],ColorIndex[2]] ColorDelta[ColorIndex[3],ColorIndex[4]]/6];
check["Hermitian conjugation swaps generator matrix slots",
 Conjugate[ColorGenerator[crI,crJ,crA]]===ColorGenerator[crJ,crI,crA]];
check["complex conjugation reverses an ordered trace",
 Conjugate[ColorTrace[crA,crB,crC]]===ColorTrace[crC,crB,crA]];
check["deltas and real structure constants are real under conjugation",
 Conjugate[ColorDelta[crI,crJ]]===ColorDelta[crI,crJ] &&
 Conjugate[ColorStructure[crA,crB,crC]]===ColorStructure[crA,crB,crC]];
check["the summed generator norm is four with conjugation",
 ReduceColor[ColorGenerator[crI,crJ,crA] Conjugate[ColorGenerator[crI,crJ,crA]]]===4 &&
 Total[Flatten[Table[Abs[crT[[a,i,j]]]^2,{a,8},{i,3},{j,3}]]]===4];
check["an unconjugated generator square is not mistaken for its norm",
 ReduceColor[ColorGenerator[crI,crJ,crA]^2]===1 &&
 Total[Flatten[Table[crT[[a,i,j]]^2,{a,8},{i,3},{j,3}]]]===1];
check["external color slots are not silently summed",
 ReduceColor[ColorGenerator[ColorIndex[1],ColorIndex[2],ColorIndex[3]]
  Conjugate[ColorGenerator[ColorIndex[1],ColorIndex[2],ColorIndex[3]]]]===
  ColorGenerator[ColorIndex[1],ColorIndex[2],ColorIndex[3]]
  ColorGenerator[ColorIndex[2],ColorIndex[1],ColorIndex[3]]];
check["external generator colors can be explicitly summed in a squared tensor",
 ReduceColor[ColorGenerator[ColorIndex[1],ColorIndex[2],ColorIndex[3]]
  Conjugate[ColorGenerator[ColorIndex[1],ColorIndex[2],ColorIndex[3]]],
  "SumExternalColors"->True]===4];
check["external delta norm uses a declared representation rather than a guess",
 ReduceColor[ColorDelta[ColorIndex[1],ColorIndex[2]]^2,"SumExternalColors"->True,
  "IndexDimensions"->{ColorIndex[1]->3}]===3 &&
 ReduceColor[ColorDelta[ColorIndex[1],ColorIndex[2]]^2,"SumExternalColors"->True,
  "IndexDimensions"->{ColorIndex[1]->8}]===8];
check["an unspecified external delta dimension is reported",
 MatchQ[ReduceColor[ColorDelta[ColorIndex[1],ColorIndex[2]]^2,"SumExternalColors"->True],
 Failure["ColorDimensions",_Association]]];
check["an unsquared external tensor cannot pretend to be a color trace",
 MatchQ[ReduceColor[ColorGenerator[ColorIndex[1],ColorIndex[2],ColorIndex[3]],
 "SumExternalColors"->True],Failure["ExternalColorMultiplicity",_Association]]];
check["an isolated delta loop retains its dimension ambiguity",ReduceColor[ColorDelta[crI,crI]]===ColorDelta[crI,crI]];
check["explicit fundamental and adjoint delta dimensions are supported",
 ReduceColor[ColorDelta[crI,crI],"IndexDimensions"->{crI->3}]===3 &&
 ReduceColor[ColorDelta[crA,crA],"IndexDimensions"->{crA->8}]===8];
check["generated internal delta loops retain their own representation dimensions",
 ReduceColor[ColorDelta[crInternal[1,3],crInternal[1,3]]]===3 &&
 ReduceColor[ColorDelta[crInternal[1,8],crInternal[1,8]]]===8];
check["incompatible dimensions are rejected",FailureQ[ReduceColor[
 ColorGenerator[crA,crI,crA]]]];
check["overused dummy labels are not contracted as independent pairs",
 MatchQ[ReduceColor[ColorTrace[crA,crA,crA,crA]],Failure["ColorMultiplicity",_Association]]];
check["conflicting explicit dimensions are rejected",
 FailureQ[ReduceColor[ColorDelta[crI,crI],"IndexDimensions"->{crI->3,crI->8}]]];
check["fixed integer components are not treated as summed dummy labels",
 ReduceColor[ColorTrace[1,1]]===1/2 && ReduceColor[ColorGenerator[1,2,2]]===-I/2 &&
 ReduceColor[ColorStructure[1,2,3]]===1];
check["repeated fixed components retain their component values",
 ReduceColor[ColorGenerator[1,2,2]^2]===-1/4 && ReduceColor[ColorDelta[1,1]]===1];
check["out-of-range and noninteger fixed components are rejected",
 FailureQ[ReduceColor[ColorGenerator[4,1,1]]] && FailureQ[ReduceColor[ColorTrace[0,1]]] &&
 FailureQ[ReduceColor[ColorDelta[1/2,1/2]]]];
check["all fixed generator components match independent matrices",
 And @@ Flatten[Table[ReduceColor[ColorGenerator[i,j,a]]===crT[[a,i,j]],{i,3},{j,3},{a,8}]]];
check["numeric traces use the written generator order",
 ReduceColor[ColorTrace[1,2,3]]===Tr[crT[[1]].crT[[2]].crT[[3]]] &&
 ReduceColor[ColorTrace[1,3,2]]===Tr[crT[[1]].crT[[3]].crT[[2]]]];
check["a color-free scalar expression is unchanged",ReduceColor[(crX+crY)^2]===(crX+crY)^2];
check["a noncolor spectator is not expanded",ReduceColor[(crX+crY)^2 ColorTrace[crA,crA]]===4 (crX+crY)^2];
check["nonpolynomial color input is reported",FailureQ[ReduceColor[1/ColorTrace[crA,crB]]]];
check["the expansion allowance reports failure instead of a partial result",
 MatchQ[ReduceColor[ColorDelta[crI,crJ]+ColorDelta[crK,crL],"MaxTerms"->1],Failure["ColorBudget",_Association]]];
check["a scaled contraction-budget failure remains a bare Failure",
 MatchQ[ReduceColor[7 ColorTrace[crA,crB,crC] ColorGenerator[crI,crJ,crA],"MaxTerms"->1],
 Failure["ColorBudget",_Association]]];
check["a scaled invalid tensor remains a bare Failure",
 MatchQ[ReduceColor[7 ColorTrace[crA,crA,crA,crA]],Failure["ColorMultiplicity",_Association]]];
check["invalid option values are rejected",FailureQ[ReduceColor[1,"MaxTerms"->0]] &&
 FailureQ[ReduceColor[1,"IndexDimensions"->{crI->4}]] && FailureQ[ReduceColor[1,"SumExternalColors"->Automatic]]];
crTraceBasis=ConstructiveDiagrams`Private`colorTensorBasis[{ColorTrace[ColorIndex[1],ColorIndex[2]]},{8,8}];
check["the exact tensor-basis backend accepts the same public trace representation",
 AssociationQ[crTraceBasis] && crTraceBasis["GramMatrix"]==={{2}}];
crTwoTrace=ColorTrace[ColorIndex[1],ColorIndex[2]];
crPowerReference=Flatten[Table[Tr[crT[[a]].crT[[b]]]^2,{a,8},{b,8}]];
crPowerVector=ConstructiveDiagrams`Private`colorTensorVector[crTwoTrace^2,{8,8}];
check["each factor of a trace power has an independent fundamental sum",
 crPowerVector===crPowerReference];
crPowerBasis=ConstructiveDiagrams`Private`colorTensorBasis[{crTwoTrace^2},{8,8}];
check["the squared two-generator trace has Gram norm one half",
 AssociationQ[crPowerBasis] && crPowerBasis["GramMatrix"]==={{1/2}}];
crTraceInterference=ConstructiveDiagrams`Private`colorTensorBasis[
 {crTwoTrace,crTwoTrace^2,crTwoTrace+I crTwoTrace^2},{8,8}];
check["trace powers preserve complex coherent interference with trace sums",
 AssociationQ[crTraceInterference] && crTraceInterference["DiagramGramMatrix"]===
 {{2,1,2+I},{1,1/2,1+I/2},{2-I,1-I/2,5/2}}];
check["term-local trace dummies are not summed over other polynomial terms",
 ConstructiveDiagrams`Private`colorTensorVector[crTwoTrace+crTwoTrace^2,{8,8}]===
 Flatten[Table[Tr[crT[[a]].crT[[b]]]+Tr[crT[[a]].crT[[b]]]^2,{a,8},{b,8}]]];
check["conjugated trace powers retain independent fundamental sums",
 ConstructiveDiagrams`Private`colorTensorVector[Conjugate[crTwoTrace^2],{8,8}]===
 Conjugate[crPowerReference]];
check["a trace times its conjugate has its independent component product",
 ConstructiveDiagrams`Private`colorTensorVector[crTwoTrace Conjugate[crTwoTrace],{8,8}]===
 Flatten[Table[Tr[crT[[a]].crT[[b]]] Conjugate[Tr[crT[[a]].crT[[b]]]],{a,8},{b,8}]]];
crOrderedWord=Times[ColorGenerator[ColorIndex[2],crInternal["word",3],ColorIndex[3]],
 ColorGenerator[crInternal["word",3],ColorIndex[1],ColorIndex[4]]];
crOrderedSquare=SquareAmplitude[crOrderedWord,{0,0,0,0}];
check["SquareAmplitude gives the conjugate copy an independent typed internal color sum",
 ReduceColor[crOrderedSquare,"SumExternalColors"->True]===16/3];
crBareWord=crOrderedWord /. crInternal["word",3]->crBareColor;
check["SquareAmplitude also freshens paired bare internal color symbols",
 ReduceColor[SquareAmplitude[crBareWord,{0,0,0,0}],"SumExternalColors"->True]===16/3];
check["color-dummy freshening never changes a same-named scalar coefficient",
 ReduceColor[SquareAmplitude[crBareColor crBareWord,{0,0,0,0}],"SumExternalColors"->True]===
 (16/3) crBareColor Conjugate[crBareColor]];
Print["Color reduction: ",checks," checks, ",Length[failures]," failures."];
If[failures =!= {},Print[failures]];
Print["HEPCAT_TEST_COMPLETED:color-reduction-tests.wl"];
Quit[If[failures==={},0,1]];
