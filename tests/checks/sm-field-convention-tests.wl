(* The constructive SM uses the original CalcHEP W-field basis. Check the
   amplitude phases themselves, including every coherent massive-spin
   coefficient. No fitted normalization or process-level sign is used. *)

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
check[name_, value_] := (checks++;
  If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);

fcConstructiveModel = ReadModel[FileNameJoin[{root, "models"}], 1];
fcDiagramModel = ReadModel[FileNameJoin[{root, "tests", "checks", "fixtures", "models"}], 6];
fcFeynmanModel = ReadModel[FileNameJoin[{root, "tests", "reference-models", "calchep"}], 1];
fcOriginalConstructiveModel = fcConstructiveModel;
fcOriginalDiagramModel = fcDiagramModel;
fcOriginalFeynmanModel = fcFeynmanModel;
fcOverrides = {"Mu" -> 4, "Md" -> 7, "Me" -> 1, "Mm" -> 2,
  "Ml" -> 3, "Mc" -> 5, "Ms" -> 8, "Mtp" -> 6, "Mb" -> 9,
  "MW" -> 20, "Mh" -> 30, "SW" -> 3/5, "EE" -> 1/3};
fcConstructiveRules = ConstructiveDiagrams`Private`residueParameterRules[
  fcConstructiveModel, fcOverrides];
fcDiagramRules = ConstructiveDiagrams`Private`residueParameterRules[
  fcDiagramModel, fcOverrides];
fcFeynmanRules = ConstructiveDiagrams`Private`residueParameterRules[
  List @@ fcFeynmanModel, fcOverrides];

(* A rational complex three-point chart exists even when real decay
   kinematics do not. Two charts vary the matrix entries independently.
   Massive spinors carry two symbolic components; massless spinors factor
   their exact rank-one momentum matrix. *)
fcThreePointChart[masses_List, sample_Integer] := Module[
  {mass = AssociationThread[Range[3], masses], epsilon = {{0, 1}, {-1, 0}},
   momenta = <||>, angleBasis = <||>, squareBasis = <||>, vectors = <||>,
   angles = <||>, squares = <||>, order, first, second, third, trace,
   diagonal, p, m, pivot, row, column, references},
  order = RotateLeft[Range[3],
    SelectFirst[Range[3], masses[[#]] =!= 0 &] - 1];
  {first, second, third} = order;
  AssociateTo[momenta, first -> mass[first] IdentityMatrix[2]];
  trace = (mass[third]^2 - mass[first]^2 - mass[second]^2)/mass[first];
  diagonal = sample - 1;
  AssociateTo[momenta, second -> {{diagonal, sample},
    {(diagonal (trace - diagonal) - mass[second]^2)/sample, trace - diagonal}}];
  AssociateTo[momenta, third -> -momenta[first] - momenta[second]];
  Do[
    m = mass[leg]; p = momenta[leg];
    If[m === 0,
      pivot = SelectFirst[Tuples[Range[2], 2], Extract[p, #] =!= 0 &];
      {row, column} = pivot;
      AssociateTo[angles, leg -> p[[All, column]]];
      AssociateTo[squares, leg -> p[[row, All]]/p[[row, column]]],
      AssociateTo[angleBasis, leg -> {{m, 0}, {0, 1}}];
      AssociateTo[squareBasis, leg -> epsilon.
        ConstructiveDiagrams`Private`residueAdjugate[p].angleBasis[leg]/m];
      AssociateTo[vectors, leg -> {Unique["fcSpin"], Unique["fcSpin"]}];
      AssociateTo[angles, leg -> angleBasis[leg].vectors[leg]];
      AssociateTo[squares, leg -> squareBasis[leg].vectors[leg]]],
    {leg, 3}];
  references = <|"Angle" -> AssociationThread[Range[12], ConstantArray[{2, 3}, 12]],
    "Square" -> AssociationThread[Range[12], ConstantArray[{3, 5}, 12]]|>;
  <|"Momenta" -> momenta, "Masses" -> mass, "AngleBasis" -> angleBasis,
    "SquareBasis" -> squareBasis, "ExternalSpinVectors" -> vectors,
    "Angle" -> angles, "Square" -> squares,
    "CoherentVectorSpinIndices" -> True, "AllowCutSpinors" -> False,
    "SpinVariables" -> Flatten[Values[vectors]], "ReferenceOverrides" -> references|>
];
fcPolynomialZeroQ[value_, variables_List] :=
  FreeQ[value, _Missing | _Failure] &&
    And @@ (Together[Last[#]] === 0 & /@ CoefficientRules[Expand[value], variables]);

fcCubicProcesses = {
  {"d", "U", "W+"}, {"u", "D", "W-"},
  {"s", "C", "W+"}, {"c", "S", "W-"},
  {"b", "T", "W+"}, {"t", "B", "W-"},
  {"e", "Ne", "W+"}, {"ne", "E", "W-"},
  {"m", "Nm", "W+"}, {"nm", "M", "W-"},
  {"l", "Nl", "W+"}, {"nl", "L", "W-"},
  {"U", "d", "W+"}, {"D", "u", "W-"},
  {"u", "U", "Z"}, {"d", "D", "Z"},
  {"e", "E", "Z"}, {"ne", "Ne", "Z"},
  {"W+", "W-", "h"}, {"W+", "W-", "Z"},
  {"W+", "W-", "A.+"}, {"W+", "W-", "A.-"},
  {"Z", "Z", "h"}, {"e", "E", "h"}, {"u", "U", "h"}};
Do[
  fcFeynmanExpression = Total[Last /@ FeynmanToSpinorProducts[
    DiagramAmplitudes[CreateDiagrams[fcProcess, fcFeynmanModel]],
    "ReduceSpinorProducts" -> False, "TimeConstraint" -> 30]];
  fcMasses = ConstructiveDiagrams`Private`physicalParticleMass[
    #, fcConstructiveModel[[1]]] & /@ fcProcess;
  fcConstructiveExpression = ExpandXFactors[Total[Last /@ DiagramAmplitudes[
    CreateDiagrams[fcProcess, fcConstructiveModel]]], 3] /.
      Thread[Mass /@ Range[3] -> fcMasses];
  fcCheckDiagramModel = MemberQ[Take[fcCubicProcesses, 12], fcProcess];
  If[fcCheckDiagramModel,
    fcDiagramExpression = ExpandXFactors[Total[Last /@ DiagramAmplitudes[
      CreateDiagrams[fcProcess, fcDiagramModel]]], 3] /.
        Thread[Mass /@ Range[3] -> fcMasses]];
  Do[
    fcChart = fcThreePointChart[fcMasses /. fcConstructiveRules, fcSample];
    check[{"three-point chart is exactly conserved and on shell", fcProcess, fcSample},
      Total[Values[fcChart["Momenta"]]] === ConstantArray[0, {2, 2}] &&
      And @@ Table[Together[Det[fcChart["Momenta"][leg]] -
        fcChart["Masses"][leg]^2] === 0, {leg, 3}]];
    fcFeynmanValue = ConstructiveDiagrams`Private`residueEvaluateChart[
      fcFeynmanExpression /. _ColorDelta -> 1, fcChart, fcFeynmanRules];
    fcConstructiveValue = ConstructiveDiagrams`Private`residueEvaluateChart[
      fcConstructiveExpression /. _ColorDelta -> 1, fcChart, fcConstructiveRules];
    check[{"aligned SM cubic agrees for every coherent spin coefficient", fcProcess, fcSample},
      !fcPolynomialZeroQ[fcFeynmanValue, fcChart["SpinVariables"]] &&
      fcPolynomialZeroQ[fcFeynmanValue - fcConstructiveValue, fcChart["SpinVariables"]]];
    If[fcCheckDiagramModel,
      fcDiagramValue = ConstructiveDiagrams`Private`residueEvaluateChart[
        fcDiagramExpression /. _ColorDelta -> 1, fcChart, fcDiagramRules];
      check[{"diagram-model charged cubic has the same aligned spin coefficients", fcProcess, fcSample},
        fcPolynomialZeroQ[fcFeynmanValue - fcDiagramValue, fcChart["SpinVariables"]]]],
    {fcSample, 2}],
  {fcProcess, fcCubicProcesses}];

(* A field basis change belongs to every vertex, rather than to a completed
   process. The antiparticle phases are reciprocal. Consequently each sewn
   internal pair cancels and any tree keeps only its external field phase. *)
fcFieldPhase[name_] := If[MemberQ[{"W+", "W-"}, name], -1, 1];
fcFields[row_] := DeleteCases[Take[row, 4], ""];
check["charged-vector particle and antiparticle phases cancel on an internal edge",
  fcFieldPhase["W+"] fcFieldPhase["W-"] === 1];
check["all supported particle-antiparticle edge phases cancel",
  AllTrue[fcConstructiveModel[[1]],
    fcFieldPhase[#[[2]]] fcFieldPhase[#[[3]]] === 1 &]];
Do[
  fcLeft = Times @@ (fcFieldPhase /@ Join[fcLeftExternal, {fcInternal[[1]]}]);
  fcRight = Times @@ (fcFieldPhase /@ Join[{fcInternal[[2]]}, fcRightExternal]);
  check[{"sewn lower-point phase equals the full external phase", fcInternal,
      fcLeftExternal, fcRightExternal},
    fcLeft fcRight === Times @@ (fcFieldPhase /@ Join[fcLeftExternal, fcRightExternal])],
  {fcInternal, {{"W+", "W-"}, {"W-", "W+"}, {"e", "E"}, {"Z", "Z"}}},
  {fcLeftExternal, {{"d", "U"}, {"W+", "Z"}}},
  {fcRightExternal, {{"W-", "h"}, {"W+", "W-", "h"}}}];

(* Undo the same field change in an in-memory constructive model, then compare
   complete raw five-point diagrams. The equality is before any kinematic
   restriction, so it also holds on each distinct factorization channel.
   This test never writes either model back to disk. *)
fcUnalignedConstructiveModel = ReplacePart[fcConstructiveModel, 2 -> Map[
  Function[row, ReplacePart[row, -2 ->
    ((Times @@ (fcFieldPhase /@ fcFields[row])) row[[-2]])]], fcConstructiveModel[[2]]]];
Do[
  fcAlignedDiagrams = CreateDiagrams[fcProcess, fcConstructiveModel];
  fcUnalignedDiagrams = CreateDiagrams[fcProcess, fcUnalignedConstructiveModel];
  fcAlignedPairs = DiagramAmplitudes[fcAlignedDiagrams];
  fcUnalignedPairs = DiagramAmplitudes[fcUnalignedDiagrams];
  fcExpectedPhase = Times @@ (fcFieldPhase /@ fcProcess);
  fcCuts = DeleteDuplicates[Flatten[Cases[First /@ fcAlignedPairs,
    HoldPattern[Mass[Multiparticle[legs__]] -> _] :> {{legs}}, Infinity], 1]];
  check[{"five-point fixture includes several internal channels", fcProcess},
    Length[fcAlignedDiagrams] >= 3 && Length[fcCuts] >= 3 &&
      Length[fcAlignedPairs] === Length[fcUnalignedPairs]];
  check[{"field rephasing preserves every complete five-point diagram and its residues", fcProcess},
    And @@ MapThread[Expand[Last[#1] - fcExpectedPhase Last[#2]] === 0 &,
      {fcAlignedPairs, fcUnalignedPairs}]];
  check[{"crossing preserves the same field phase", fcProcess},
    Expand[ReverseMomentum[Total[Last /@ fcAlignedPairs], {3, 4, 5}] -
      fcExpectedPhase ReverseMomentum[Total[Last /@ fcUnalignedPairs], {3, 4, 5}]] === 0],
  {fcProcess, {{"d", "U", "W+", "A.+", "h"},
    {"u", "D", "W-", "A.-", "h"}}}];
check["the convention checks do not mutate either loaded model",
  fcConstructiveModel === fcOriginalConstructiveModel &&
    fcDiagramModel === fcOriginalDiagramModel &&
    fcFeynmanModel === fcOriginalFeynmanModel];

Print["CHECKS: ", checks, "; FAILURES: ", Length[failures]];
If[failures =!= {}, Print[failures]; Quit[1]];
Print["SM FIELD CONVENTION TESTS PASSED"];
Print["HEPCAT_TEST_COMPLETED:sm-field-convention-tests.wl"];
Quit[0];
