(* Same-chirality Dirac currents use the supplied topology and model vertices. *)

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
Get[FileNameJoin[{root, "source", "HEPCAT.wl"}]];
$HistoryLength = 0;
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, test_] := (checks++; If[!TrueQ[test], AppendTo[failures, name]; Print["FAIL: ", name]]);
model = ReadModel[FileNameJoin[{root, "models"}], 4];
diagramModel = ReadModel[FileNameJoin[{root, "models"}], 3];
mass = ModelParameter[model, "Me"]; charge = ModelParameter[model, "EE"];
current[photons_, kind_: 1] := ConstructiveDiagrams`Private`constructPhysicalChiralCurrent[
  {1, 2}, photons, kind, mass, ConstantArray[-Sqrt[2] charge, Length[photons]]]["Expression"];
Do[
  process = Join[{"e", "E"}, ConstantArray["A.+", n]];
  pair = {Thread[Mass /@ Range[n + 2] -> Join[{mass, mass}, ConstantArray[0, n]]], current[Range[3, n + 2]]};
  report = CheckAmplitudes[pair, FactorizationAmplitudeChannels[process, model], model,
    "Output" -> "Details", "RandomSeed" -> 2026110500 + n, "TimeConstraint" -> 30];
  check["all original cuts and final conditions for " <> ToString[n] <> " vector emissions",
    report["Summary"] === <|"PASS" -> 2^n|>];
  check["known compact lower-point score is preserved for " <> ToString[n] <> " emissions",
    SpinorExpressionComplexity[Last[pair]] === {1, 27, 108}[[n - 1]]],
  {n, 2, 4}];
photons = Range[3, 7];
expression = current[photons];
check["seven-point subset current has the derived compact score", SpinorExpressionComplexity[expression] === 820];
check["the scalar current introduces only physical propagators and no reference denominator",
  FreeQ[expression, (Spinor | SpinorHat)["Xi", ___]] &&
  AllTrue[Cases[expression, Power[_, _Integer?Negative], Infinity],
    FreeQ[First[#], _SpinorChain | _SpinorTrace | _MomProd] &]];
permutation = AssociationThread[Range[7], {4, 2, 7, 3, 6, 1, 5}];
permuted = ConstructiveDiagrams`Private`constructPhysicalChiralCurrent[
  Lookup[permutation, {1, 2}], Lookup[permutation, photons], 1,
  mass, ConstantArray[-Sqrt[2] charge, 5]]["Expression"];
check["current generation uses typed external labels rather than special particle numbers",
  Expand[ConstructiveDiagrams`Private`canonicalChains[
    ConstructiveDiagrams`Private`residueCanonicalPropagators[
      permuted - ConstructiveDiagrams`Private`relabelLegs[expression, permutation], Range[7]]]] === 0];
check["angle and square duals follow the same recursion",
  current[photons, 2] === (expression /. {"Angle" -> "Square", "Square" -> "Angle"})];
process = Join[{"e", "E"}, ConstantArray["A.+", 5]];
graphs = CreateDiagrams[process, diagramModel];
external = Thread[Mass /@ Range[7] -> Join[{ModelParameter[diagramModel, "Me"], ModelParameter[diagramModel, "Me"]}, ConstantArray[0, 5]]];
proposal = ConstructiveDiagrams`Private`constructVectorLineForm[graphs, process, diagramModel, external, "AllPhysical"];
check["complete cubic topology supports the physical current", AssociationQ[proposal] &&
  proposal["Method"] === "PhysicalChiralSubsetCurrent"];
check["omitting an ordered diagram rejects the direct current",
  MissingQ[ConstructiveDiagrams`Private`constructVectorLineForm[Most[graphs], process, diagramModel, external, "AllPhysical"]]];
check["duplicating an ordered diagram rejects the direct current",
  MissingQ[ConstructiveDiagrams`Private`constructVectorLineForm[Append[Most[graphs], First[graphs]], process, diagramModel, external, "AllPhysical"]]];
(* A dense rest-frame certificate of the ordinary two-emission identity.
   Both massive matrices have determinant proofMass^2; the two null momenta
   and spinors are independent and total momentum conservation is not used. *)
proofP = proofMass IdentityMatrix[2];
proofQ = {{qa, qb}, {qc, (proofMass^2 + qb qc)/qa}};
proofAngles = <|3 -> {ax, ay}, 4 -> {bx, by}|>;
proofSquares = <|3 -> {au, av}, 4 -> {bu, bv}|>;
proofMomenta = <|1 -> proofP, 2 -> proofQ,
  3 -> Outer[Times, proofAngles[3], proofSquares[3]],
  4 -> Outer[Times, proofAngles[4], proofSquares[4]]|>;
proofChart = <|"Momenta" -> proofMomenta, "Masses" -> <|1 -> proofMass, 2 -> proofMass, 3 -> 0, 4 -> 0|>,
  "Angle" -> proofAngles, "Square" -> proofSquares|>;
proofSpin[kind_, i_] := Spinor["Helicity", kind, i];
proofBracket[i_, j_] := SpinorChain[proofSpin["Square", i], proofSpin["Square", j]];
proofK[i_] := SpinorChain[proofSpin["Square", i], Mom[1], Mom[2], proofSpin["Square", i]];
proofL[i_, j_] := proofBracket[i, j] SpinorChain[proofSpin["Angle", j], Mom[1], proofSpin["Square", i]];
proofU[i_] := PropDen[Mom[Multiparticle[1, i]], proofMass];
proofF[i_] := PropDen[Mom[Multiparticle[2, i]], proofMass];
proofIdentity = proofF[3] proofL[3, 4] proofK[4] + proofF[4] proofL[4, 3] proofK[3] +
  PropDen[Mom[Multiparticle[3, 4]], 0] proofK[3] proofK[4] -
  proofMass^2 proofBracket[3, 4]^2 (proofU[3] proofU[4] - proofF[3] proofF[4]);
check["the paired eikonal contraction is an exact external determinant identity",
  Cancel[Together[ConstructiveDiagrams`Private`residueEvaluateChart[proofIdentity, proofChart, {}]]] === 0];
primitiveCalls = 0;
Block[{ConstructiveDiagrams`Private`residueChartChain},
  ConstructiveDiagrams`Private`residueChartChain[_, chart_] := (primitiveCalls++; chart["Value"]);
  primitive = SpinorChain[Spinor["Helicity", "Angle", 3], Spinor["Helicity", "Angle", 4]];
  first = ConstructiveDiagrams`Private`residueEvaluateChart[primitive + primitive^2, <|"Value" -> 2|>, {}];
  check["one exact chart evaluation reuses repeated primitive spinor chains", first === 6 && primitiveCalls === 1];
  second = ConstructiveDiagrams`Private`residueEvaluateChart[primitive + primitive^2, <|"Value" -> 3|>, {}];
  check["primitive values are never reused across different charts", second === 12 && primitiveCalls === 2]
];
Print["CHECKS ", checks, "; FAILURES ", Length[failures]];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:chiral-current-tests.wl"], Print[failures]];
Quit[If[failures === {}, 0, 1]];
