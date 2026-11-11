(* Reusable QED all-plus five-point residue pilot. Get does not run tests or quit.
   q5ResidueReport[] compares H, S2 and the old permutation continuation.
   q5ResidueReport[expression] checks a supplied all-incoming expression.
   q5FullResidueReport[] also checks every channel returned by the public
   factorization API, using the process-independent automatic kinematics.
   Results are exact on the supplied rational sample charts, not a generic
   symbolic proof over every external phase-space point. *)

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


prRoot = Nest[DirectoryName, ExpandFileName[$InputFileName], 4];
If[!MemberQ[$Packages, "ConstructiveDiagrams`"] ||
    DownValues[ConstructiveDiagrams`PhysicalPoleResidue] === {},
  Global`$HEPCATpath = FileNameJoin[{prRoot, "source"}];
  Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]]
];

(* Model 3 supplies the three-point diagram vertices. Model 4 also contains
   the known four-point amplitudes needed for five-point factorization. *)
prModel = ReadModel[FileNameJoin[{prRoot, "models"}], 3];
prFixtureModel = ReadModel[FileNameJoin[{prRoot, "models"}], 4];
prElectronMass = ModelParameter[prModel, "Me"];
prElectricCharge = ModelParameter[prModel, "EE"];
prAngle[i_, j_] := SpinorChain[Spinor["Helicity", "Angle", i],
  Spinor["Helicity", "Angle", j]];
prSquare[i_, j_] := SpinorChain[Spinor["Helicity", "Square", i],
  Spinor["Helicity", "Square", j]];
prSpinAngle[i_, j_] := SpinorChain[Spinor["Spin", "Angle", i],
  Spinor["Spin", "Angle", j]];
prMixed[i_, p_, j_] := SpinorChain[Spinor["Helicity", "Angle", i], Mom[p],
  Spinor["Helicity", "Square", j]];
prK[i_] := SpinorChain[Spinor["Helicity", "Square", i], Mom[1], Mom[2],
  Spinor["Helicity", "Square", i]];
prDen[i_, j_] := PropDen[Mom[Multiparticle @@ Sort[{i, j}]], prElectronMass];
prPhotons = {3, 4, 5};
prPrefactor = 2 Sqrt[2] prElectricCharge^3 prElectronMass prSpinAngle[1, 2];
prH = prPrefactor Total[Function[c, With[{ab = Complement[prPhotons, {c}]},
      prSquare @@ ab]^2 prK[c]/prDen[2, c]] /@ prPhotons]/
  (Times @@ (prDen[1, #] & /@ prPhotons));
prS2 = prPrefactor Total[Function[order, With[
    {a = order[[1]], b = order[[2]], c = order[[3]]},
    prSquare[a, b] prMixed[a, 2, c]/
      (prAngle[a, b] prAngle[a, c] prDen[1, a] prDen[2, c])]] /@
  Permutations[prPhotons]];
prOldPermutation = prPrefactor Total[Function[order, With[
    {a = order[[1]], b = order[[2]], c = order[[3]]},
    prSquare[b, c] prMixed[b, 1, a]/
      (prAngle[a, b] prAngle[b, c] prDen[1, a] prDen[2, c])]] /@
  Permutations[prPhotons]];

prA4Row = SelectFirst[prFixtureModel[[2]],
  Sort[Drop[#, -2]] === Sort[{"e", "E", "A.+", "A.+"}] &];
prParticles = {"e", "E", "A.+", "A.+", "A.+"};
prChannels = FactorizationAmplitudeChannels[prParticles, prFixtureModel];
prTargets = ConstructiveDiagrams`Private`residueSimpleInputs[
  0, prChannels, prFixtureModel]["Targets"];
prElectronTargets = Select[prTargets,
  Length[#["Cut"]] === 2 && Count[#["Cut"], 1 | 2] === 1 &&
    Count[#["Cut"], 3 | 4 | 5] === 1 && #["Mass"] === prElectronMass &];
prJ = {{0, -1}, {1, 0}}; prL = -prJ;
prAdj[p_] := {{p[[2, 2]], -p[[1, 2]]}, {-p[[2, 1]], p[[1, 1]]}};
prZero[expr_] := AllTrue[Flatten[{expr}], Together[#] === 0 &];

(* q is the selected physical propagator at every point of this chart.
   Both external massive shells and total momentum conservation hold for
   generic q. Only q is sent to zero when extracting the residue. *)
prChart[cut_List, q_, spin1_Integer, spin2_Integer] := Module[
  {photon = First[Intersection[cut, prPhotons]], endpoint,
   ordering, labels, angle, square, momenta, t, base, other},
  endpoint = First[Intersection[cut, {1, 2}]];
  ordering = Prepend[Complement[prPhotons, {photon}], photon];
  labels = AssociationThread[{3, 4, 5}, ordering];
  angle = AssociationThread[ordering, {{1, 0}, {0, 1}, {1, 1}}];
  t = 3 (-29 - 8 q)/(10 - 2 q);
  square = AssociationThread[ordering, {{1, 0}, {0, 5}, {7, t}}];
  momenta = Association@Table[i -> Outer[Times, angle[i], square[i]],
    {i, ordering}];
  base = {{2, 3}, {(2 q - 1)/3, q}};
  other = -base - Total[Values[momenta]];
  AssociateTo[momenta, {endpoint -> base, 3 - endpoint -> other}];
  AssociateTo[angle, {1 -> UnitVector[2, spin1], 2 -> UnitVector[2, spin2]}];
  AssociateTo[square, {1 -> prL.prAdj[momenta[1]].angle[1],
    2 -> prL.prAdj[momenta[2]].angle[2]}];
  <|"Momenta" -> momenta, "Angle" -> angle, "Square" -> square|>
];
prMomentum[index_Integer, chart_] := chart["Momenta"][index];
prMomentum[index_Multiparticle, chart_] :=
  Total[prMomentum[#, chart] & /@ List @@ index];
prSpinorVector[Spinor["Xi", "Angle", ___], _] := {2, 3};
prSpinorVector[Spinor["Xi", "Square", ___], _] := {5, 7};
prSpinorVector[Spinor[_, kind_, index_Integer, ___], chart_] := chart[kind][index];
prChainValue[chain_SpinorChain, chart_] := Module[
  {parts = List @@ chain, first, last, insertions, row, matrices, kind},
  first = First[parts]; last = Last[parts];
  insertions = parts[[2 ;; -2]];
  kind = first[[2]];
  row = If[kind === "Angle", prSpinorVector[first, chart].prJ,
    prSpinorVector[first, chart]];
  matrices = MapIndexed[Function[{insertion, position}, With[
      {p = prMomentum[insertion[[1]], chart]},
      If[OddQ[First[position]] === (kind === "Angle"), p, prAdj[p]]]],
    insertions];
  Fold[Dot, row, matrices].If[last[[2]] === "Square",
    prL.prSpinorVector[last, chart], prSpinorVector[last, chart]]
];
prMapOnChart[expr_, chart_] := Module[{mapped},
  mapped = expr /. chain_SpinorChain :> prChainValue[chain, chart];
  mapped = mapped /. {
    PropDen[Mom[index_], mass_] :> Det[prMomentum[index, chart]] - mass^2,
    MomProd[i_, j_] :> (Det[prMomentum[i, chart] + prMomentum[j, chart]] -
      Det[prMomentum[i, chart]] - Det[prMomentum[j, chart]])/2};
  mapped = mapped /. Mom[index_] :> prMomentum[index, chart];
  mapped /. {prElectronMass -> 1, prElectricCharge -> 1}
];
prMap[spin1_Integer, spin2_Integer][expr_, cuts_List, variables_List] :=
  prMapOnChart[expr, prChart[First[cuts]["Cut"], First[variables], spin1, spin2]];

(* A compatible two-cut chart: D13=q and D25=r are independent coordinates.
   Both external massive shells hold before either coordinate is set to zero.
   At their intersection the other electron denominators are -5,7,37,-35. *)
prDoubleChart[q_, r_, spin1_Integer, spin2_Integer] := Module[
  {angle, square, momenta, u, t, base},
  angle = AssociationThread[prPhotons, {{1, 0}, {0, 1}, {1, 1}}];
  u = (r - q - 5)/5;
  t = -15 (r + 7 q + 14)/((r - q - 5) (3 - q) + 20);
  square = AssociationThread[prPhotons, {{1, 0}, {0, 5}, {7, t}}];
  momenta = Association@Table[i -> Outer[Times, angle[i], square[i]],
    {i, prPhotons}];
  base = {{u, 3}, {(u q - 1)/3, q}};
  AssociateTo[momenta, {1 -> base, 2 -> -base - Total[Values[momenta]]}];
  AssociateTo[angle, {1 -> UnitVector[2, spin1], 2 -> UnitVector[2, spin2]}];
  AssociateTo[square, {1 -> prL.prAdj[momenta[1]].angle[1],
    2 -> prL.prAdj[momenta[2]].angle[2]}];
  <|"Momenta" -> momenta, "Angle" -> angle, "Square" -> square|>
];
prDoubleMap[spin1_Integer, spin2_Integer][expr_, cuts_List, variables_List] := Module[
  {cutLegs = Lookup[cuts, "Cut"], first, second},
  If[Length[variables] =!= 2 || Sort[cutLegs] =!= {{1, 3}, {2, 5}}, Return[$Failed]];
  first = First[FirstPosition[cutLegs, {1, 3}]];
  second = First[FirstPosition[cutLegs, {2, 5}]];
  prMapOnChart[expr,
    prDoubleChart[variables[[first]], variables[[second]], spin1, spin2]]
];

(* A separate four-point chart calibrates the sewing phase against the
   retained A4 amplitude using only the model's three-point amplitudes. *)
prFourPointChart[cut_List, q_, spin1_Integer, spin2_Integer] := Module[
  {photon = First[Intersection[cut, {3, 4}]], ordering, angle, square, momenta, base},
  ordering = {photon, 7 - photon};
  angle = AssociationThread[ordering, {{1, 0}, {0, 1}}];
  square = AssociationThread[ordering, {{1, 0}, {5, 5 - q/3}}];
  momenta = Association@Table[i -> Outer[Times, angle[i], square[i]], {i, ordering}];
  base = {{2, 3}, {(2 q - 1)/3, q}};
  AssociateTo[momenta, {1 -> base, 2 -> -base - Total[Values[momenta]]}];
  AssociateTo[angle, {1 -> UnitVector[2, spin1], 2 -> UnitVector[2, spin2]}];
  AssociateTo[square, {1 -> prL.prAdj[momenta[1]].angle[1],
    2 -> prL.prAdj[momenta[2]].angle[2]}];
  <|"Momenta" -> momenta, "Angle" -> angle, "Square" -> square|>
];
prFourPointMap[spin1_Integer, spin2_Integer][expr_, cuts_List, variables_List] :=
  prMapOnChart[expr, prFourPointChart[First[cuts]["Cut"], First[variables], spin1, spin2]];
prFourPointChannels = FactorizationAmplitudeChannels[
  {"e", "E", "A.+", "A.+"}, prModel];
prFourPointTargets = Select[
  ConstructiveDiagrams`Private`residueSimpleInputs[
    0, prFourPointChannels, prModel]["Targets"],
  #["Mass"] === prElectronMass && MemberQ[#["Cut"], 1] &&
    Count[#["Cut"], 3 | 4] === 1 &];
prFourPointAmplitude = Times @@ prA4Row[[-2 ;; -1]];
prUncutChannels[target_] := Select[prElectronTargets,
  #["Cut"] =!= target["Cut"] &];
prResidue[expr_, target_, i_, j_] := PhysicalPoleResidue[expr, target,
  "KinematicMap" -> prMap[i, j], "KinematicKind" -> "Sample",
  "UncutChannels" -> prUncutChannels[target], "TimeConstraint" -> 15];
prResidueValue[result_] := Lookup[result, "PhysicalResidue", Missing["NoResidue"]];
prSewnValue[target_, i_, j_] := Module[{q, value},
  value = TimeConstrained[Cancel[Together[prMapOnChart[target["StateSum"],
      prChart[target["Cut"], q, i, j]]]] /. q -> 0, 15, $TimedOut];
  <|"Status" -> If[FreeQ[value, Indeterminate | ComplexInfinity |
      _DirectedInfinity | $TimedOut | _SpinorChain | _Mom | _PropDen], "PASS", "UNRESOLVED"],
    "PhysicalResidue" -> value|>
];

(* Diagram vertices come only from model 3. Model 4's existing four-point
   amplitudes are used solely for five-point factorization targets. *)
prDiagramRecords[] := DiagramAmplitudeRecords[CreateDiagrams[prParticles, prModel]];
prDiagramReferenceRules[record_Association] := DeleteDuplicates[Cases[
  record["OriginalExpression"],
  SpinorChain[reference : Spinor["Xi", _, _Integer],
      Spinor["Helicity", _, leg_Integer]] :>
    (reference -> ReplacePart[reference, 3 -> leg]), Infinity]];
prDiagramReferenceAssignments[records_List] := Map[
  <|"DiagramIndex" -> #["DiagramIndex"],
    "Rules" -> prDiagramReferenceRules[#]|> &, records];
(* Xi10, Xi11, ... are local counters in each diagram and can name different
   photons in different orderings. Choose one reference per external photon
   before adding diagrams; retain all original expressions in the records. *)
prPreparedDiagramSum[records_List] := Total[
  (ReduceSpinContractions[#["OriginalExpression"] /. prDiagramReferenceRules[#]] /.
    #["MassRules"]) & /@ records];
q5ResidueReport[expression_] := CheckPhysicalResidues[expression, prChannels, prFixtureModel,
  "Output" -> "Details",
  "KinematicMap" -> prMap[1, 2], "KinematicKind" -> "Sample",
  "TimeConstraint" -> 15];
q5ResidueReport[] := Module[{records = prDiagramRecords[], diagramSum, compare},
  diagramSum = prPreparedDiagramSum[records];
  compare[expression_] := CheckPhysicalResidues[expression, prChannels, prFixtureModel,
    "Output" -> "Details", "KinematicMap" -> prMap[1, 2],
    "KinematicKind" -> "Sample", "TimeConstraint" -> 15];
  <|
  "Scope" -> "ExactOnProvidedRationalSampleCharts",
  "Mass" -> 1, "Coupling" -> 1, "ExternalSpinComponents" -> {1, 2},
  "ExternalParticles" -> prParticles,
  "DiagramRecords" -> records,
  "ReferenceAssignments" -> prDiagramReferenceAssignments[records],
  "Amplitudes" -> <|"H" -> prH, "S2" -> prS2, "OldPermutation" -> prOldPermutation,
    "ActualDiagrams" -> diagramSum|>,
  "IndependentFactorizationTargets" -> prElectronTargets,
  "Reports" -> <|"H" -> compare[prH], "S2" -> compare[prS2],
    "OldPermutation" -> compare[prOldPermutation], "ActualDiagrams" -> compare[diagramSum]|>|>
];

Options[q5FullResidueReport] = Options[CheckPhysicalResidues];
q5FullResidueReport[expression : Except[_Rule | _RuleDelayed], opts : OptionsPattern[]] :=
  CheckPhysicalResidues[expression, prChannels, prFixtureModel, opts,
    "Output" -> "Details"];
(* Distinct physical-photon references expose a failure in the raw diagram
   continuation. Keep it as a negative control and record, separately, the
   explicit common-reference choice that passes. The checker changes neither. *)
q5FullResidueReport[opts : OptionsPattern[]] := Module[
  {records = prDiagramRecords[], amplitudes, diagramSum, commonRules},
  diagramSum = prPreparedDiagramSum[records];
  commonRules = DeleteDuplicates[Cases[diagramSum,
    reference : Spinor["Xi", _, _Integer] :>
      (reference -> ReplacePart[reference, 3 -> 0]), Infinity]];
  amplitudes = <|"H" -> prH, "S2" -> prS2,
    "OldPermutation" -> prOldPermutation,
    "ActualDiagrams" -> diagramSum,
    "CommonReferenceDiagrams" -> (diagramSum /. commonRules)|>;
  <|"ExternalParticles" -> prParticles, "Amplitudes" -> amplitudes,
    "DiagramRecords" -> records,
    "ReferenceAssignments" -> prDiagramReferenceAssignments[records],
    "CommonReferenceRules" -> commonRules,
    "IndependentFactorizationTargets" -> prTargets,
    "Reports" -> Map[CheckPhysicalResidues[#, prChannels, prFixtureModel,
      opts, "Output" -> "Details"] &, amplitudes]|>
];

(* ::Section:: *)
(*Mixed-Helicity Diagram Reconstruction*)

(* A process-specific reconstruction for e1,E2,A3+,A4+,A5-.
   The completion below is explicit. It is not an identity on the raw
   off-shell diagram sum and is not a change to CheckPhysicalResidues. *)

ClearAll[qedFivePointMixedRoute];
qedFivePointMixedRoute[pairs_List] := Module[
  {spinor, angle, square, firstChain, secondChain, doubleChain,
   firstDen, secondDen, mixedDen, firstAngle, pairAngle, photonInvariant,
   referenceGap, raw, numerators, localSeed, seed,
   completionNumerator, completionNumerators, completedNumerators,
   completion, completed, final, denominator, photonOrder, onShellFactors,
   referenceHelicities, expectedMasses = {prElectronMass, prElectronMass, 0, 0, 0},
   expectedPairs, expectedRaw, canonicalReferences},
  If[Length[pairs] =!= 6 || !AllTrue[pairs,
      MatchQ[#, {_List, _}] && AllTrue[First[#], MatchQ[#, _Rule] &] &&
        (Mass /@ Range[5] /. First[#]) === expectedMasses &],
    Return[Failure["QEDMixedFivePointScope", <|
      "MessageTemplate" -> "Supply the six standard QED diagram amplitudes for e,E,A.+,A.+,A.-."|>]]];
  spinor[i_, chirality_] :=
    Spinor[If[MemberQ[{1, 2}, i], "Spin", "Helicity"], chirality, i];
  angle[i_, j_] := SpinorChain[spinor[i, "Angle"], spinor[j, "Angle"]];
  square[i_, j_] := SpinorChain[spinor[i, "Square"], spinor[j, "Square"]];
  firstChain[i_] := SpinorChain[spinor[5, "Angle"], Mom[1], spinor[i, "Square"]];
  secondChain[i_] := SpinorChain[spinor[5, "Angle"], Mom[2], spinor[i, "Square"]];
  doubleChain[i_] := SpinorChain[spinor[i, "Square"], Mom[1], Mom[2], spinor[i, "Square"]];
  firstDen[i_] := PropDen[Mom[Multiparticle @@ Sort[{1, i}]], prElectronMass];
  secondDen[i_] := PropDen[Mom[Multiparticle @@ Sort[{2, i}]], prElectronMass];
  mixedDen[i_, j_] := SpinorChain[spinor[i, "Square"], Mom[1],
    Mom[Multiparticle @@ Sort[{i, 5}]], spinor[j, "Square"]];
  firstAngle = angle[1, 2];
  pairAngle = angle[1, 5] angle[2, 5];
  photonInvariant = 2 (MomProd[3, 4] + MomProd[3, 5] + MomProd[4, 5]);
  referenceGap = angle[5, 3] angle[5, 4];
  raw = (ReduceSpinContractions[Last[#]] /. First[#] &) /@ pairs;
  referenceHelicities = DeleteDuplicates[Cases[raw,
    Power[SpinorChain[Spinor["Xi", kind_, _],
        Spinor["Helicity", kind_, leg_Integer]], power_Integer?Negative] :>
      {leg, kind}, Infinity]];
  If[Sort[referenceHelicities] =!= {{3, "Angle"}, {4, "Angle"}, {5, "Square"}},
    Return[Failure["QEDMixedFivePointScope", <|
      "MessageTemplate" -> "The mixed route requires positive photons 3,4 and a negative photon 5."|>]]];
  photonOrder[expression_] := Module[{propagators, a, c},
    propagators = DeleteDuplicates[Cases[expression, _PropDen, Infinity]];
    a = Cases[propagators, PropDen[Mom[Multiparticle[1, i_Integer]], prElectronMass] :> i];
    c = Cases[propagators, PropDen[Mom[Multiparticle[1, i_Integer, j_Integer]], prElectronMass] :>
      First[Complement[{3, 4, 5}, {i, j}]]];
    If[Length[a] =!= 1 || Length[c] =!= 1, Return[$Failed]];
    {First[a], First[Complement[{3, 4, 5}, {First[a], First[c]}]], First[c]}
  ];
  denominator[expression_] := Times @@ DeleteDuplicates[Cases[expression, _PropDen, Infinity]];
  If[Sort[photonOrder /@ raw] =!= Sort[Permutations[{3, 4, 5}]],
    Return[Failure["IncompleteQEDMixedFivePointDiagrams", <||>]]];
  (* This is a certified worked route, not a completion of arbitrary input.
     Verify the actual six expressions before selecting their representatives. *)
  expectedPairs = DiagramAmplitudes[CreateDiagrams[
    {"e", "E", "A.+", "A.+", "A.-"}, ReadModel[FileNameJoin[{prRoot, "models"}], 3]]];
  expectedRaw = (ReduceSpinContractions[Last[#]] /. First[#] &) /@ expectedPairs;
  canonicalReferences[expression_] := expression /.
    prDiagramReferenceRules[<|"OriginalExpression" -> expression|>];
  If[!And @@ MapThread[
      (Cancel[Together[canonicalReferences[#1] - canonicalReferences[#2]]] === 0) &,
      {SortBy[raw, photonOrder], SortBy[expectedRaw, photonOrder]}],
    Return[Failure["QEDMixedFivePointScope", <|
      "MessageTemplate" -> "This worked route accepts the standard model-3 raw diagrams, not arbitrary altered numerators."|>]]];
  localSeed[order_] := With[{ij = DeleteCases[order, 5]},
    With[{i = ij[[1]], j = ij[[2]]},
      2 Sqrt[2] prElectricCharge^3 firstChain[i] secondChain[j]/(prElectronMass referenceGap) *
        (pairAngle - firstAngle firstChain[i] secondChain[j]/mixedDen[i, j])]];
  numerators = localSeed[photonOrder[#]] & /@ raw;
  seed = Total[MapThread[#1/denominator[#2] &, {numerators, raw}]];
  completionNumerator[order_] := With[{ij = DeleteCases[order, 5]},
    With[{i = ij[[1]], j = ij[[2]]},
      -2 Sqrt[2] prElectricCharge^3 prElectronMass firstAngle square[i, j]^4 firstDen[i] secondDen[j]/
        (photonInvariant square[i, 5] square[5, j] mixedDen[i, j])]];
  completionNumerators = completionNumerator[photonOrder[#]] & /@ raw;
  completedNumerators = numerators + completionNumerators;
  completion = Total[MapThread[#1/denominator[#2] &, {completionNumerators, raw}]];
  completed = seed + completion;
  onShellFactors[order_] := With[{ij = DeleteCases[order, 5]},
    Intersection[{firstDen[First[ij]], secondDen[Last[ij]]},
      {firstDen[First[order]], secondDen[Last[order]]}]];
  final = 2 Sqrt[2] prElectricCharge^3/(prElectronMass firstDen[3] firstDen[4] firstDen[5]) *
    (firstChain[3] doubleChain[4] *
        (firstAngle firstChain[3] - pairAngle square[3, 5])/secondDen[4] +
     firstChain[4] doubleChain[3] *
        (firstAngle firstChain[4] - pairAngle square[4, 5])/secondDen[3] +
     prElectronMass^2 square[3, 4]^2 *
        (firstAngle SpinorChain[spinor[5, "Angle"], Mom[1], Mom[2], spinor[5, "Angle"]] +
          pairAngle (firstDen[5] + secondDen[5]))/secondDen[5]);
  <|"InputDiagrams" -> pairs, "RawDiagrams" -> raw,
    "PhotonOrders" -> (photonOrder /@ raw),
    "LocalSeedNumerators" -> numerators,
    "LocalCompletionNumerators" -> completionNumerators,
    "LocalCompletedNumerators" -> completedNumerators,
    "OnShellFactorUsed" -> (onShellFactors /@ (photonOrder /@ raw)), "Seed" -> seed,
    "Completion" -> completion, "CompletedSpuriousForm" -> completed,
    "Final" -> final,
    "CompletionMeaning" ->
      "A physical off-shell completion, not an exact identity on the raw sum.",
    "LocalAssumptions" ->
      "Each seed numerator is compared only on its own two internal shells; original propagators remain opaque.",
    "GlobalAssumptions" ->
      "External shells and momentum conservation only. No internal shell condition on the summed amplitude."|>
];

ClearAll[qedFivePointMixedLocalChecks];
qedFivePointMixedLocalChecks[route_Association] := Module[
  {j = {{0, -1}, {1, 0}}, eps, adj, perp, momentum, angleSpinor,
   squareSpinor, endpoint, referenceAngle, referenceSquare, matrixChain,
   evaluate, aa, bb, order, a, b, c, q1, q2, mass, propagators,
   numerator, candidate, completedCandidate, checks = {}, original, aSpin, bSpin},
  eps = -j;
  adj[p_] := {{p[[2, 2]], -p[[1, 2]]}, {-p[[2, 1]], p[[1, 1]]}};
  perp[v_] := {-v[[2]], v[[1]]};
  momentum[Multiparticle[x__]] := Total[momentum /@ {x}];
  endpoint[Spinor["Helicity", "Angle", i_]] := angleSpinor[i];
  endpoint[Spinor["Helicity", "Square", i_]] := squareSpinor[i];
  endpoint[Spinor["Xi", "Angle", _]] := referenceAngle;
  endpoint[Spinor["Xi", "Square", _]] := referenceSquare;
  endpoint[Spinor["Spin", "Angle", i_]] := If[i === 1, aa, bb];
  endpoint[Spinor["Spin", "Square", i_]] := eps.adj[momentum[i]].If[i === 1, aa, bb]/mass;
  matrixChain[chain_SpinorChain] := Module[{xs = List @@ chain, kind, left, right, matrices},
    kind = xs[[1, 2]]; left = endpoint[First[xs]];
    If[kind === "Angle", left = left.j];
    right = endpoint[Last[xs]];
    If[xs[[-1, 2]] === "Square", right = eps.right];
    matrices = MapIndexed[If[OddQ[First[#2]] === (kind === "Angle"),
        momentum[#[[1]]], adj[momentum[#[[1]]]]] &, xs[[2 ;; -2]]];
    Fold[Dot, left, matrices].right
  ];
  evaluate[e_] := Together[e /. x_SpinorChain :> matrixChain[x] /.
    {PropDen[Mom[q_], prElectronMass] :> Det[momentum[q]] - mass^2,
     MomProd[i_, h_] :> (Det[momentum[i] + momentum[h]] -
       Det[momentum[i]] - Det[momentum[h]])/2} /. {prElectronMass -> mass, prElectricCharge -> 1}];
  Do[
    order = route["PhotonOrders"][[index]]; {a, b, c} = order;
    Do[
      mass = sample;
      angleSpinor[3] = {1, sample + 1}; angleSpinor[4] = {sample + 2, 1};
      angleSpinor[5] = {2, 2 sample + 3};
      momentum[1] = {{sample + 1, sample - 2},
        {sample + 2, (mass^2 + (sample - 2) (sample + 2))/(sample + 1)}};
      squareSpinor[a] = (sample + 2) perp[adj[momentum[1]].angleSpinor[a]];
      momentum[a] = Outer[Times, angleSpinor[a], squareSpinor[a]];
      q1 = momentum[1] + momentum[a];
      squareSpinor[b] = (sample - 5) perp[adj[q1].angleSpinor[b]];
      momentum[b] = Outer[Times, angleSpinor[b], squareSpinor[b]];
      q2 = -q1 - momentum[b];
      squareSpinor[c] = (sample + 3) perp[adj[q2].angleSpinor[c]];
      momentum[c] = Outer[Times, angleSpinor[c], squareSpinor[c]];
      momentum[2] = q2 - momentum[c];
      referenceAngle = {17, 29}; referenceSquare = {31, 43};
      propagators = Times @@ DeleteDuplicates[Cases[route["RawDiagrams"][[index]], _PropDen, Infinity]];
      original = Cancel[propagators route["RawDiagrams"][[index]]];
      Do[
        aa = UnitVector[2, aSpin]; bb = UnitVector[2, bSpin];
        numerator = evaluate[original];
        candidate = evaluate[route["LocalSeedNumerators"][[index]]];
        completedCandidate = evaluate[route["LocalCompletedNumerators"][[index]]];
        AppendTo[checks, <|"Order" -> order, "Mass" -> mass, "Spins" -> {aSpin, bSpin},
          "Shells" -> ({Det[momentum[1]], Det[momentum[2]], Det[q1], Det[q2]} === ConstantArray[mass^2, 4]),
          "Finite" -> FreeQ[{numerator, candidate, completedCandidate}, Indeterminate | ComplexInfinity | DirectedInfinity[_]],
          "SeedEqual" -> (Together[numerator - candidate] === 0),
          "CompletedEqual" -> (Together[numerator - completedCandidate] === 0)|>],
        {aSpin, 2}, {bSpin, 2}],
      {sample, {1, 2, 3, 4}}], {index, Length[route["RawDiagrams"]]}];
  <|"ExactComparisons" -> Length[checks],
    "AllPass" -> AllTrue[checks, #["Shells"] && #["Finite"] && #["SeedEqual"] && #["CompletedEqual"] &],
    "Details" -> checks|>
];

ClearAll[qedFivePointMixedEqualityChecks];
qedFivePointMixedEqualityChecks[] := Module[
  {j = {{0, -1}, {1, 0}}, eps, adj, la, ti, p, an, sq, f, g, a, b,
   d, t, c, k, ell, mass2, s, gap, closure, zero, u, v, w, z, r, x, y,
   orders = {{3, 4}, {4, 3}}, localChecks, spuriousU, regularU, spuriousV,
   compactU, compactV, aProduct, removeDPoles, compareU, compareV,
   finalRegrouping},
  eps = -j;
  adj[q_] := {{q[[2, 2]], -q[[1, 2]]}, {-q[[2, 1]], q[[1, 1]]}};
  la[3] = {1, 0}; la[4] = {0, 1}; la[5] = {1, 1};
  ti[3] = {1, 0}; ti[4] = {0, r}; ti[5] = {x, y};
  p[1] = {{u, v}, {w, z}};
  Do[p[i] = Outer[Times, la[i], ti[i]], {i, {3, 4, 5}}];
  p[2] = -p[1] - Total[p /@ {3, 4, 5}];
  mass2 = Det[p[1]]; closure = Expand[Det[p[2]] - mass2];
  zero[q_] := Last[PolynomialReduce[Expand[Numerator[Together[q]]],
      {closure}, {z, w, v, u, y, x, r}]] === 0;
  an[i_, h_] := la[i].j.la[h]; sq[i_, h_] := ti[i].eps.ti[h];
  f[i_] := la[5].j.p[1].eps.ti[i]; g[i_] := la[5].j.p[2].eps.ti[i];
  a[i_] := ti[i].adj[p[1]].la[i]; b[i_] := ti[i].adj[p[2]].la[i];
  d[i_, h_] := ti[i].adj[p[1]].(p[i] + p[5]).eps.ti[h];
  t[i_, h_] := ti[i].adj[p[1]].p[5].eps.ti[h];
  c[i_, h_] := 1 + a[i]/a[5] + b[h]/b[5];
  k[i_] := ti[i].adj[p[1]].p[2].eps.ti[i];
  ell = la[5].j.p[1].adj[p[2]].la[5];
  s = Det[p[1] + p[2]]; gap = an[5, 3] an[5, 4];
  localChecks = Function[ij, With[{i = ij[[1]], h = ij[[2]]},
    <|"Order" -> ij,
      "DLeft" -> zero[d[i, h] - a[i] sq[i, h] - f[i] sq[5, h]],
      "DRight" -> zero[d[i, h] - b[h] sq[i, h] - g[h] sq[i, 5]],
      "PoleRemovalIdentity" -> zero[s f[i] g[h] - ell d[i, h] + mass2 gap sq[i, h]^2],
      "ProductIdentity" -> zero[a[i] b[h] sq[i, h]^2 +
        d[i, h] (t[i, h] - sq[i, h] b[h]) - sq[i, 5] sq[5, h] f[i] g[h]]|>]] /@ orders;
  spuriousU = Total[Function[ij, With[{i = ij[[1]], h = ij[[2]]},
    c[i, h] (-f[i]^2 g[h]^2/(a[i] b[h] gap d[i, h]) -
      mass2 sq[i, h]^4/(s sq[i, 5] sq[5, h] d[i, h]))]] /@ orders];
  regularU = Total[Function[ij, With[{i = ij[[1]], h = ij[[2]]},
    c[i, h] (-f[i] g[h] ell/(a[i] b[h] gap s) +
      mass2 sq[i, h]^2 (t[i, h] - sq[i, h] b[h])/(a[i] b[h] sq[i, 5] sq[5, h] s))]] /@ orders];
  spuriousV = Total[Function[ij, With[{i = ij[[1]], h = ij[[2]]},
    c[i, h] f[i] g[h]/(a[i] b[h] gap)]] /@ orders];
  aProduct = a[3] a[4] a[5];
  compactU = (f[3]^2 k[4]/b[4] + f[4]^2 k[3]/b[3] + mass2 sq[3, 4]^2 ell/b[5])/aProduct;
  compactV = (-f[3] sq[3, 5] k[4]/b[4] - f[4] sq[4, 5] k[3]/b[3] +
    mass2 sq[3, 4]^2 (a[5] + b[5])/b[5])/aProduct;
  removeDPoles = zero[spuriousU - regularU];
  compareU = zero[regularU - compactU];
  compareV = zero[spuriousV - compactV];
  (* Ordinary external identities regroup the three physical blocks into two.
     The latter is a display alternative, not a lower-complexity replacement. *)
  finalRegrouping = <|
    "MomentumChain" -> zero[ell + an[3, 5] f[3] + an[4, 5] f[4]],
    "PropagatorSum" -> zero[a[5] + b[5] - an[3, 5] sq[3, 5] -
      an[4, 5] sq[4, 5]]|>;
  <|"LocalIdentities" -> localChecks, "RemoveDPoles" -> removeDPoles,
    "UEqualsThreeTermForm" -> compareU, "VEqualsThreeTermForm" -> compareV,
    "FullEquality" -> (removeDPoles && compareU && compareV),
    "FinalRegroupingIdentities" -> finalRegrouping,
    "InternalShellConditions" -> {}|>
];


(* Each sewn target is valid on its own cut. Restoring that one explicit
   propagator lets the same two-cut extractor compare both sewing orders. *)
q5DoubleResidueReport[spin1_Integer : 1, spin2_Integer : 2] := Module[
  {first, second, channels, uncut, extract},
  first = SelectFirst[prElectronTargets, #["Cut"] === {1, 3} &];
  second = SelectFirst[prElectronTargets, #["Cut"] === {2, 5} &];
  channels = {first, second};
  uncut = Select[prElectronTargets,
    !MemberQ[{{1, 3}, {2, 5}}, #["Cut"]] &];
  extract[expr_] := PhysicalPoleResidue[expr, channels,
    "KinematicMap" -> prDoubleMap[spin1, spin2], "KinematicKind" -> "Sample",
    "UncutChannels" -> uncut, "TimeConstraint" -> 15];
  <|"Scope" -> "ExactOnProvidedTwoVariableRationalSampleChart",
    "ExternalSpinComponent" -> {spin1, spin2}, "Channels" -> channels,
    "UncutChannels" -> uncut,
    "H" -> extract[prH], "S2" -> extract[prS2],
    "OldPermutation" -> extract[prOldPermutation],
    "ExpectedFromFirst" -> extract[first["StateSum"]/first["Propagator"]],
    "ExpectedFromSecond" -> extract[second["StateSum"]/second["Propagator"]]|>
];

(* Four-Fermion Bremsstrahlung: Explicit Diagram Numerator Route

   This QED pilot uses the standard cubic electron vertex, two electrons,
   two positrons and photon 5 of positive helicity. It is not an automatic
   simplifier for other theories. All original diagram expressions remain
   available. The quotient step changes each numerator by a multiple of
   THAT diagram's electron propagator; physical denominators stay off shell.
   In particular, the old raw sum is not claimed equal to the final sum. *)

q5FourFermionAngle[i_Integer] := Spinor[If[i === 5, "Helicity", "Spin"], "Angle", i];
q5FourFermionSquare[i_Integer] := Spinor[If[i === 5, "Helicity", "Spin"], "Square", i];
q5FourFermionBracket[kind_, i_, j_] := SpinorChain[
  If[kind === "Angle", q5FourFermionAngle[i], q5FourFermionSquare[i]],
  If[kind === "Angle", q5FourFermionAngle[j], q5FourFermionSquare[j]]];
q5FourFermionCurrent[a_, b_, c_, d_] :=
  q5FourFermionBracket["Angle", a, c] q5FourFermionBracket["Square", b, d] +
  q5FourFermionBracket["Angle", a, d] q5FourFermionBracket["Square", b, c] +
  q5FourFermionBracket["Angle", b, c] q5FourFermionBracket["Square", a, d] +
  q5FourFermionBracket["Angle", b, d] q5FourFermionBracket["Square", a, c];
q5FourFermionInsertion[emitter_, partner_, c_, d_] :=
  q5FourFermionBracket["Angle", partner, d] q5FourFermionBracket["Square", 5, c] +
  q5FourFermionBracket["Angle", partner, c] q5FourFermionBracket["Square", 5, d];
q5FourFermionElectronPropagator[i_] :=
  PropDen[Mom[Multiparticle @@ Sort[{i, 5}]], prElectronMass];
q5FourFermionPhotonPropagator[c_, d_] :=
  PropDen[Mom[Multiparticle @@ Sort[{c, d}]], 0];

q5FourFermionPhotonRoute[diagrams_List] := Module[
  {particles, actual, reference = Spinor["Xi", "Angle", 0], prefactor,
   orient, routes = {}, paired, diagram, lines, vertex, emittingLeg, pair,
   spectator, a, b, c, d, sign, endpointSign, current, insertion, electron,
   photon, original, references, referenceRules, propagatorRules, raw,
   referenceDenominator, referenceMomentum, denominator, numerator,
   retainedNumerator, removedCoefficient, removedNumerator, remainder,
   groups, grouped, electronA, electronB, softA, softB, final, pairedCurrent},
  If[diagrams === {} || !AllTrue[diagrams, AssociationQ],
    Return[Failure["InvalidQEDFivePointDiagrams", <||>]]];
  particles = diagrams[[1]]["ExternalParticles"];
  If[Length[particles] =!= 5 || particles[[5]] =!= "A.+" ||
      Sort[Take[particles, 4]] =!= Sort[{"e", "e", "E", "E"}] ||
      !AllTrue[diagrams, #["ExternalParticles"] === particles &],
    Return[Failure["QEDFourFermionPhotonScope", <|
      "MessageTemplate" -> "Supply diagrams for two e, two E and a positive-helicity photon in slot 5."|>]]];
  actual = DiagramAmplitudes[diagrams];
  If[Length[actual] =!= Length[diagrams] || Length[diagrams] =!= 8,
    Return[Failure["IncompleteQEDFivePointDiagrams", <||>]]];
  orient[ends_List] := If[particles[[First[ends]]] === "e", ends, Reverse[ends]];
  Do[
    diagram = diagrams[[i]];
    prefactor = -Times @@ (Last[#]["ModelRow"][[-2]] & /@
      Values[diagram["VertexStructures"]]);
    lines = diagramFermionLines[diagram];
    If[!MatchQ[lines, {{_Integer, _Integer}, {_Integer, _Integer}}],
      Return[Failure["InvalidFermionEndpointMatching", <|"DiagramIndex" -> i|>]]];
    vertex = First[AdjacencyList[diagram["Graph"], 5]];
    emittingLeg = Cases[diagram["IncidentLegData"][vertex],
      leg_Association /; leg["Kind"] === "External" && leg["Index"] =!= 5 :>
        leg["Index"]];
    If[Length[emittingLeg] =!= 1,
      Return[Failure["InvalidPhotonAttachment", <|"DiagramIndex" -> i|>]]];
    emittingLeg = First[emittingLeg];
    pair = orient[SelectFirst[lines, MemberQ[#, emittingLeg] &]];
    spectator = orient[SelectFirst[lines, !MemberQ[#, emittingLeg] &]];
    {a, b} = pair; {c, d} = spectator;
    sign = diagramFermionSign[diagram];
    endpointSign = If[emittingLeg === a, 1, -1];
    current = q5FourFermionCurrent[a, b, c, d];
    insertion = q5FourFermionInsertion[emittingLeg,
      If[emittingLeg === a, b, a], c, d];
    electron = q5FourFermionElectronPropagator[emittingLeg];
    photon = q5FourFermionPhotonPropagator[c, d];
    original = ReduceSpinContractions[Last[actual[[i]]]] /. First[actual[[i]]];
    references = DeleteDuplicates[Cases[original, Spinor["Xi", "Angle", _], Infinity]];
    referenceRules = (# -> reference) & /@ references;
    propagatorRules = DeleteDuplicates[Cases[original,
      prop : PropDen[_, mass_] :> (prop -> If[mass === 0, photon, electron]), Infinity]];
    raw = original /. referenceRules /. propagatorRules;
    referenceDenominator = SpinorChain[reference, q5FourFermionAngle[5]];
    referenceMomentum = SpinorChain[reference, Mom[emittingLeg], q5FourFermionSquare[5]];
    denominator = prElectronMass photon electron referenceDenominator;
    numerator = prefactor sign endpointSign
      (prElectronMass current + q5FourFermionBracket["Angle", emittingLeg, 5] insertion)
        referenceMomentum;
    retainedNumerator = prefactor sign endpointSign prElectronMass (
      current referenceMomentum + q5FourFermionBracket["Square", emittingLeg, 5]
        insertion referenceDenominator);
    removedCoefficient = prefactor sign endpointSign insertion
      SpinorChain[q5FourFermionAngle[emittingLeg], reference];
    removedNumerator = electron removedCoefficient;
    remainder = removedCoefficient/(prElectronMass photon referenceDenominator);
    AppendTo[routes, <|"DiagramIndex" -> i, "FermionLines" -> (orient /@ lines),
      "RadiatingPair" -> pair, "SpectatorPair" -> spectator,
      "EmittingLeg" -> emittingLeg, "FermionSign" -> sign,
      "VertexPrefactor" -> prefactor,
      "OriginalExpression" -> original, "OriginalReferences" -> references,
      "ReferenceRules" -> referenceRules, "ReferenceSpinor" -> reference,
      "PropagatorRules" -> propagatorRules, "Raw" -> raw,
      "DiracReduced" -> numerator/denominator,
      "CommonDenominator" -> denominator, "UnreducedNumerator" -> numerator,
      "RetainedNumerator" -> retainedNumerator,
      "NumeratorRepresentative" -> retainedNumerator/denominator,
      "RemovedNumeratorMultiple" -> removedNumerator,
      "RemovedNumeratorCoefficient" -> removedCoefficient,
      "OffCutRemainder" -> remainder, "ElectronPropagator" -> electron,
      "PhotonPropagator" -> photon,
      "SchoutenIdentity" -> Inactive[Equal][
        q5FourFermionBracket["Angle", emittingLeg, 5] referenceMomentum,
        prElectronMass referenceDenominator q5FourFermionBracket["Square", emittingLeg, 5] +
          electron SpinorChain[q5FourFermionAngle[emittingLeg], reference]]|>],
    {i, Length[diagrams]}];
  groups = GatherBy[routes, {#["RadiatingPair"], #["SpectatorPair"]} &];
  If[Length[groups] =!= 4 || !AllTrue[groups, Length[#] === 2 &&
      Sort[Lookup[#, "EmittingLeg"]] === Sort[First[#]["RadiatingPair"]] &&
      Length[DeleteDuplicates[Lookup[#, "FermionSign"]]] === 1 &&
      Length[DeleteDuplicates[Lookup[#, "VertexPrefactor"]]] === 1 &],
    Return[Failure["IncompleteRadiatingFermionPairs", <||>]]];
  paired = Table[
    grouped = group;
    {a, b} = First[grouped]["RadiatingPair"];
    {c, d} = First[grouped]["SpectatorPair"];
    sign = First[grouped]["FermionSign"];
    prefactor = First[grouped]["VertexPrefactor"];
    current = q5FourFermionCurrent[a, b, c, d];
    electronA = q5FourFermionElectronPropagator[a];
    electronB = q5FourFermionElectronPropagator[b];
    photon = q5FourFermionPhotonPropagator[c, d];
    softA = SpinorChain[reference, Mom[a], q5FourFermionSquare[5]]/
      (SpinorChain[reference, q5FourFermionAngle[5]] electronA);
    softB = SpinorChain[reference, Mom[b], q5FourFermionSquare[5]]/
      (SpinorChain[reference, q5FourFermionAngle[5]] electronB);
    final = prefactor sign/photon (
      current SpinorChain[q5FourFermionSquare[5], Mom[b], Mom[a],
        q5FourFermionSquare[5]]/(electronA electronB) +
      q5FourFermionBracket["Square", a, 5] q5FourFermionInsertion[a, b, c, d]/electronA -
      q5FourFermionBracket["Square", b, 5] q5FourFermionInsertion[b, a, c, d]/electronB);
    <|"RadiatingPair" -> {a, b}, "SpectatorPair" -> {c, d},
      "DiagramIndices" -> Lookup[grouped, "DiagramIndex"],
      "ReferenceForm" -> Total[Lookup[grouped, "NumeratorRepresentative"]],
      "Final" -> final, "SoftIdentity" -> Inactive[Equal][softA - softB,
        SpinorChain[q5FourFermionSquare[5], Mom[b], Mom[a],
          q5FourFermionSquare[5]]/(electronA electronB)]|>,
    {group, groups}];
  <|"ExternalParticles" -> particles, "ActualDiagramAmplitudes" -> actual,
    "OriginalRawSum" -> Total[Lookup[routes, "OriginalExpression"]],
    "CommonReference" -> reference, "RawSum" -> Total[Lookup[routes, "Raw"]],
    "Routes" -> routes, "PairedCurrents" -> paired,
    "ReducedSum" -> Total[Lookup[routes, "NumeratorRepresentative"]],
    "Final" -> Total[Lookup[paired, "Final"]],
    "Scope" -> "DiagramLocalNumeratorRepresentativesThenExactReferenceElimination",
    "DenominatorsKeptOffShell" -> True|>
];

(* All independent nonzero five-point entries for the electron/photon model.
   The opposite helicities use parity of the checked diagram derivations;
   qed-model-export-tests.wl checks every ordered photon assignment against
   independent Feynman diagrams before these entries are used at six points. *)
qedFivePointModelAmplitudes[] := Module[
  {mixed, radiation, dual, amplitudes, particles, factor, removeFactor},
  mixed = qedFivePointMixedRoute[DiagramAmplitudes[CreateDiagrams[
    {"e", "E", "A.+", "A.+", "A.-"}, prModel]]];
  If[FailureQ[mixed], Return[mixed]];
  radiation = q5FourFermionPhotonRoute[CreateDiagrams[
    {"e", "E", "E", "e", "A.+"}, prModel]];
  If[FailureQ[radiation], Return[radiation]];
  dual[expression_] := expression /.
    Spinor[type_, kind : ("Angle" | "Square"), rest___] :>
      Spinor[type, If[kind === "Angle", "Square", "Angle"], rest];
  particles = {
    {"e", "E", "A.+", "A.+", "A.+"},
    {"e", "E", "A.+", "A.+", "A.-"},
    {"e", "E", "A.+", "A.-", "A.-"},
    {"e", "E", "A.-", "A.-", "A.-"},
    {"e", "E", "E", "e", "A.+"},
    {"e", "E", "E", "e", "A.-"}};
  amplitudes = {prH, mixed["Final"],
    PermuteParticles[dual[mixed["Final"]], {3, 4, 5}], dual[prH],
    radiation["Final"], dual[radiation["Final"]]};
  factor = 2 Sqrt[2] prElectricCharge^3;
  removeFactor[sum_Plus] := removeFactor /@ sum;
  removeFactor[term_] := term/factor;
  MapThread[{#1, factor, removeFactor[#2]} &, {particles, amplitudes}]
];

(* A worked minimal-QED example, not a general amplitude bootstrap.
   The [5,3> shift acts on every typed occurrence in the complete lower
   amplitudes, including inverse factors and their internal propagators. *)
qedBalancedSixPointShift[expression_, variable_Symbol] := Module[
  {shiftSquare = 5, shiftAngle = 3, external = Range[6], rankOne = Unique["qedShiftQ"],
   tag = Unique["qedShiftFailure"], fail, kind, opposite, helicity, members,
   eta, twiceQDot, linearTerms, linearEta, linearTwiceQDot, endpointChoices,
   momentumChoices, splitRankOne, shiftChain, shift},
  fail[reason_, data_: <||>] := Throw[Failure[reason, data], tag];
  kind[spinor_] := ConstructiveDiagrams`Private`rspSpinorType[spinor];
  opposite[type_] := If[type === "Angle", "Square", "Angle"];
  helicity[type_, leg_] := Spinor["Helicity", type, leg];
  members[label_Integer] /; MemberQ[external, label] := {label};
  members[label_Multiparticle] := Flatten[members /@ List @@ label];
  members[label_] := fail["UnsupportedMomentumLabel", <|"Label" -> label|>];
  eta[label_] := Count[members[label], shiftSquare] - Count[members[label], shiftAngle];
  twiceQDot[label_] := Total[Function[leg,
      If[MemberQ[{shiftSquare, shiftAngle}, leg], 0,
        SpinorChain[helicity["Angle", shiftSquare], Mom[leg],
          helicity["Square", shiftAngle]]]] /@ members[label]];
  linearTerms[momentum_] := Module[{terms},
    terms = ConstructiveDiagrams`Private`rspLinearMomentumTerms[momentum];
    If[terms === $Failed || !AllTrue[terms,
        MatchQ[Last[#], Mom[_Integer]] && MemberQ[external, Last[#][[1]]] &&
          FreeQ[First[#], variable | rankOne] &],
      fail["UnsupportedLinearMomentum", <|"Momentum" -> momentum|>]];
    terms];
  linearEta[momentum_] := Total[(First[#] eta[Last[#][[1]]]) & /@ linearTerms[momentum]];
  linearTwiceQDot[momentum_] := Total[(First[#] twiceQDot[Last[#][[1]]]) & /@ linearTerms[momentum]];
  endpointChoices[spinor_] := Which[
    spinor === helicity["Square", shiftSquare],
      {{1, spinor}, {variable, helicity["Square", shiftAngle]}},
    spinor === helicity["Angle", shiftAngle],
      {{1, spinor}, {-variable, helicity["Angle", shiftSquare]}},
    MatchQ[spinor, Spinor["Xi", "Angle" | "Square", _Integer]], {{1, spinor}},
    kind[spinor] =!= None && FreeQ[spinor, _SpinorHat], {{1, spinor}},
    True, fail["UnsupportedSpinorEndpoint", <|"Spinor" -> spinor|>]];
  momentumChoices[momentum_] := Flatten[Function[term,
      Join[{{First[term], Last[term]}},
        If[eta[Last[term][[1]]] === 0, {},
          {{First[term] variable eta[Last[term][[1]]], rankOne}}]]] /@ linearTerms[momentum], 1];
  splitRankOne[parts_List] := Module[{position, beforeKind, prefixEnd, suffixStart},
    position = FirstPosition[parts, rankOne, Missing["NoShiftMomentum"]];
    If[MissingQ[position], Return[SpinorChain @@ parts]];
    position = First[position];
    beforeKind = If[EvenQ[position - 2], kind[First[parts]], opposite[kind[First[parts]]]];
    {prefixEnd, suffixStart} = If[beforeKind === "Angle",
      {helicity["Angle", shiftSquare], helicity["Square", shiftAngle]},
      {helicity["Square", shiftAngle], helicity["Angle", shiftSquare]}];
    splitRankOne[Join[Take[parts, position - 1], {prefixEnd}]] *
      splitRankOne[Join[{suffixStart}, Drop[parts, position]]]];
  shiftChain[chain_SpinorChain] := Module[{parts = List @@ chain, slots, choices},
    If[Length[parts] < 2 || kind[First[parts]] === None || kind[Last[parts]] === None,
      fail["UnsupportedSpinorChain", <|"Chain" -> chain|>]];
    slots = Rest[Most[parts]];
    If[(kind[First[parts]] === kind[Last[parts]]) =!= EvenQ[Length[slots]],
      fail["UnsupportedSpinorChainParity", <|"Chain" -> chain|>]];
    choices = Join[{endpointChoices[First[parts]]}, momentumChoices /@ slots,
      {endpointChoices[Last[parts]]}];
    If[Times @@ (Length /@ choices) > 4096,
      fail["ShiftChainExpansionLimit", <|"Limit" -> 4096|>]];
    Total[(Times @@ (First /@ #)) splitRankOne[Last /@ #] & /@ Tuples[choices]]];
  shift[value_] := Which[
    AtomQ[value], value,
    Head[value] === SpinorChain, shiftChain[value],
    MatchQ[value, SpinorTrace[_, _]], shift[ReduceSpinorProducts[value]],
    Head[value] === SpinorTrace,
      fail["UnsupportedLongSpinorTrace", <|"Trace" -> value|>],
    Head[value] === PropDen && MemberQ[{2, 3}, Length[value]],
      value + variable linearEta[First[value]] linearTwiceQDot[First[value]],
    Head[value] === MomProd && Length[value] === 2,
      value + variable/2 (eta[value[[1]]] twiceQDot[value[[2]]] +
        eta[value[[2]]] twiceQDot[value[[1]]]),
    Head[value] === Mandelstahm && Length[value] === 2,
      value + variable (eta[value[[1]]] + eta[value[[2]]]) *
        (twiceQDot[value[[1]]] + twiceQDot[value[[2]]]),
    MatchQ[value, Power[_Mom, 2]],
      value + variable linearEta[First[value]] linearTwiceQDot[First[value]],
    FreeQ[value, _Mom | _MomHat | _MomQ | _MomProd | _Mandelstahm |
      _Spinor | _SpinorHat | _SpinorChain | _SpinorTrace | _PropDen |
      _xFactor | _xTildeFactor], value,
    MemberQ[{Plus, Times, Power}, Head[value]], Map[shift, value],
    True, fail["UnsupportedShiftedStructure", <|"Expression" -> value|>]];
  Catch[shift[expression], tag]
];

qedBalancedSixPointZeroBrackets[expression_] := expression /. HoldPattern[
  SpinorChain[spinor : Spinor["Helicity", "Angle" | "Square", _Integer], same_]] /;
    spinor === same :> 0;

Options[qedBalancedSixPointAmplitude] = {"ReturnDetails" -> False,
  "TimeConstraint" -> 120, "ProgressReporting" -> True, "ProgressFunction" -> Automatic};
qedBalancedSixPointAmplitude[model_, OptionsPattern[]] := Module[
  {particles = {"e", "E", "A.+", "A.+", "A.-", "A.-"}, external = Range[6],
   seconds = OptionValue["TimeConstraint"], details = OptionValue["ReturnDetails"],
   enabled = OptionValue["ProgressReporting"], callback = OptionValue["ProgressFunction"],
   started = AbsoluteTime[], tag = Unique["qedBalancedSixPointFailure"], fail, emit,
   variable = Unique["qedBalancedSixPointZ"], result},
  If[!NumericQ[seconds] || !TrueQ[0 < seconds <= 120] ||
      !MemberQ[{True, False}, details] || !MemberQ[{True, False}, enabled] ||
      !(callback === Automatic || MatchQ[callback, _Function | _Symbol]),
    Return[Failure["InvalidQEDExampleOptions", <|"MessageTemplate" ->
      "Use a positive TimeConstraint no greater than 120 seconds and Boolean ReturnDetails/ProgressReporting."|>]]];
  fail[reason_, data_: <||>] := Throw[Failure[reason, Join[<|
    "MessageTemplate" -> "The worked balanced-helicity QED recursion could not be completed.",
    "Reason" -> reason|>, data]], tag];
  emit[completed_, cut_: None] := ConstructiveDiagrams`Private`completionProgress[callback, enabled,
    <|"Stage" -> "QED six-point lower-amplitude recursion", "Completed" -> completed,
      "Total" -> 8, "Cut" -> cut, "Elapsed" -> AbsoluteTime[] - started|>];
  result = TimeConstrained[Catch[Module[
    {channels, externalRules, normalized, targets, separated, expectedCuts,
     rows = {}, target, cut, denominator, slope, root, assigned, shifted, term,
     referenceMode, eta, amplitude},
    If[!ListQ[model] || Length[model] =!= 4 || !AllTrue[model, ListQ] ||
        !AllTrue[model[[1]], ListQ[#] && Length[#] >= 6 &],
      fail["ExpectedConstructiveQEDModel"]];
    emit[0];
    channels = FactorizationAmplitudeChannels[particles, model];
    If[!ListQ[channels] || Length[channels] =!= 14,
      fail["ExpectedFourteenKnownPhysicalChannels"]];
    externalRules = Thread[Mass /@ external -> Take[First[channels], 6]];
    If[(Mass[1] /. externalRules) =!= (Mass[2] /. externalRules) ||
        (Mass[1] /. externalRules) === 0 ||
        !AllTrue[Range[3, 6], (Mass[#] /. externalRules) === 0 &],
      fail["ExpectedOneMassiveElectronPairAndFourPhotons"]];
    normalized = ConstructiveDiagrams`Private`residueSimpleInputs[
      {externalRules, 0}, channels, model];
    If[!AssociationQ[normalized], fail["ChannelNormalizationFailed"]];
    targets = normalized["Targets"];
    If[Length[targets] =!= 14 || !AllTrue[targets, TrueQ[Lookup[#, "Complete", False]] &],
      fail["IncompleteLowerPointAmplitudes"]];
    separated = Select[targets, Abs[Count[#["Cut"], 5] - Count[#["Cut"], 3]] === 1 &];
    expectedCuts = Sort[{{1, 3}, {1, 5}, {2, 3}, {2, 5},
      {1, 3, 4}, {1, 3, 6}, {1, 4, 5}, {1, 5, 6}}];
    If[Length[separated] =!= 8 || Sort[Lookup[separated, "Cut"]] =!= expectedCuts,
      fail["UnexpectedShiftedChannelInventory"]];
    Do[
      cut = target["Cut"]; eta = Count[cut, 5] - Count[cut, 3];
      denominator = PropDen[Mom[Multiparticle @@ cut], target["Mass"]];
      slope = eta Total[Function[leg, If[MemberQ[{3, 5}, leg], 0,
          SpinorChain[Spinor["Helicity", "Angle", 5], Mom[leg],
            Spinor["Helicity", "Square", 3]]]] /@ cut];
      If[slope === 0, fail["IdenticallyDegenerateShiftChannel", <|"Cut" -> cut|>]];
      root = -denominator/slope;
      referenceMode = Which[MemberQ[{{1, 3}, {2, 3}}, cut], "PositivePhotonReference5",
        MemberQ[{{1, 5}, {2, 5}}, cut], "NegativePhotonReference3", True, "XiFreeFourByFour"];
      assigned = Switch[referenceMode,
        "PositivePhotonReference5", target["StateSum"] /.
          Spinor["Xi", "Angle", _Integer] -> Spinor["Helicity", "Angle", 5],
        "NegativePhotonReference3", target["StateSum"] /.
          Spinor["Xi", "Square", _Integer] -> Spinor["Helicity", "Square", 3],
        _, target["StateSum"]];
      If[!FreeQ[assigned, Spinor["Xi", ___]], fail["UnsupportedLowerAmplitudeReference", <|"Cut" -> cut|>]];
      assigned = qedBalancedSixPointZeroBrackets[assigned];
      shifted = qedBalancedSixPointZeroBrackets[qedBalancedSixPointShift[assigned, variable]];
      If[FailureQ[shifted] || !ConstructiveDiagrams`Private`residueFiniteQ[shifted],
        fail["UnsupportedShiftedLowerAmplitude", <|"Cut" -> cut, "Cause" -> shifted|>]];
      term = qedBalancedSixPointZeroBrackets[(shifted /. variable -> root)/denominator];
      If[!FreeQ[term, variable] || !ConstructiveDiagrams`Private`residueFiniteQ[term] || LeafCount[term] > 250000,
        fail["IncompleteRecursionTerm", <|"Cut" -> cut|>]];
      AppendTo[rows, <|"Cut" -> cut, "LowerPointMultiplicities" -> {Length[cut] + 1, 7 - Length[cut]},
        "UnshiftedPropagator" -> denominator, "ShiftSlope" -> slope, "ShiftRoot" -> root,
        "ReferenceMode" -> referenceMode, "StateSum" -> target["StateSum"],
        "ReferenceAssignedStateSum" -> assigned, "ShiftedStateSum" -> shifted,
        "ShiftVariable" -> variable, "RecursionTerm" -> term|>];
      emit[Length[rows], cut], {target, separated}];
    amplitude = Total[Lookup[rows, "RecursionTerm"]];
    <|"Status" -> "CONSTRUCTED", "Particles" -> particles, "Amplitude" -> amplitude,
      "ExternalMassRules" -> externalRules, "Channels" -> rows, "PhysicalTargetCount" -> 14,
      "RecursionTermCount" -> 8, "Shift" -> "[5,3>: q=|5>[3|, p5(z)=p5+zq, p3(z)=p3-zq",
      "ReferenceChoice" -> "Choose angle spinor 5 for the three-point positive photon and square spinor 3 for the three-point negative photon before shifting; four-by-four terms are Xi-free.",
      "SewingMinusIncludedOnce" -> True, "NoFeynmanInput" -> True,
      "BoundaryAtInfinity" -> "The minimal-QED hard-line argument is explained in the notebook; not a generic-model boundary theorem.",
      "Scope" -> "A process-specific lower-amplitude recursion. CONSTRUCTED does not mean the independent checker has passed.",
      "Elapsed" -> AbsoluteTime[] - started|>], tag], seconds,
    Failure["QEDExampleTimeConstraint", <|"MessageTemplate" ->
      "The balanced-helicity QED example exceeded its bounded construction time.", "TimeConstraint" -> seconds|>]];
  If[FailureQ[result] || !AssociationQ[result], result, If[details, result, result["Amplitude"]]]
];

(* ::Section::Closed:: *)
(* Six-point +++- worked identities. These are examples, never production inputs. *)

qedSixPointPPPMExampleParts[model_] := Module[
  {mass = Quiet[ModelParameter[model, "Me"]], charge = Quiet[ModelParameter[model, "EE"]],
   rules, angleSpinor, squareSpinor, angle, square, mixed, diagonal, pole,
   labels = {{3, 4, 5}, {4, 5, 3}, {5, 3, 4}}, common, carrier, bracket12,
   chain6, beta, heavy, heavyFactored, mainTerms, pairTerms, poleTerm1,
   poleTerm2, pairRemainder, mainRemainder, remainderNumerator,
   previousTerms, compactTerms, mainOld, pairOld, mainNew, pairNew},
  If[!MatchQ[mass, ModelParameter[_String, _String, _String]] ||
      !MatchQ[charge, ModelParameter[_String, _String, _String]],
    Return[Failure["QEDExampleParameters", <|"MessageTemplate" ->
      "The worked QED example requires model parameters Me and EE."|>]]];
  rules = Thread[Mass /@ Range[6] -> {mass, mass, 0, 0, 0, 0}];
  angleSpinor[i_] := Spinor[If[i <= 2, "Spin", "Helicity"], "Angle", i];
  squareSpinor[i_] := Spinor[If[i <= 2, "Spin", "Helicity"], "Square", i];
  angle[i_, j_] := SpinorChain[angleSpinor[i], angleSpinor[j]];
  square[i_, j_] := SpinorChain[squareSpinor[i], squareSpinor[j]];
  mixed[p_, j_] := SpinorChain[angleSpinor[6], Mom[p], squareSpinor[j]];
  diagonal[j_] := SpinorChain[squareSpinor[j], Mom[1], Mom[2], squareSpinor[j]];
  pole[legs__Integer] := PropDen[Mom[Multiparticle @@ Sort[{legs}]], mass];
  common = pole[1, 3] pole[1, 4] pole[1, 5] pole[1, 6];
  carrier = angle[1, 6] angle[2, 6]; bracket12 = angle[1, 2];
  chain6 = SpinorChain[angleSpinor[6], Mom[1], Mom[2], angleSpinor[6]];
  beta[b_, c_] := mass MomProd[2, b] square[b, c]^2 -
    (square[b, c] SpinorChain[squareSpinor[c], Mom[1], angleSpinor[b]] diagonal[b] +
      diagonal[b] diagonal[c])/mass;
  (* The eleven-monomial numerator retained from the earlier validated result. *)
  heavy[j_] := -2 MomProd[1, 2] angle[6, 1] angle[6, 2] square[j, 6] mixed[1, j] -
    2 MomProd[1, 6] angle[6, 1] angle[6, 2] square[j, 6] mixed[1, j] -
    2 mass MomProd[1, 2] angle[6, 1] square[j, 2] mixed[1, j] +
    mass^2 bracket12 mixed[1, j]^2 -
    mass angle[6, 1] square[j, 6] mixed[1, j] mixed[1, 2] +
    mass^2 angle[6, 1] angle[6, 2] square[j, 6] mixed[2, j] +
    2 mass MomProd[1, 2] angle[6, 2] square[j, 1] mixed[2, j] +
    mass^2 bracket12 mixed[2, j]^2 +
    mass angle[6, 2] square[j, 6] square[j, 1] chain6 +
    2 MomProd[1, 2] angle[6, 1] angle[6, 2] diagonal[j] +
    2 MomProd[1, 6] angle[6, 1] angle[6, 2] diagonal[j];
  heavyFactored[j_] := (pole[1, 6] carrier + bracket12 chain6)
    (diagonal[j] - square[j, 6] mixed[1, j]);
  mainTerms[j_, b_, c_] := 4 charge^4 mass square[b, c]^2/common {
    heavy[j]/(pole[2, 6] pole[1, b, c]),
    -bracket12 (2 MomProd[2, j] mixed[1, j]^2 - angle[j, 6] mixed[1, j] diagonal[j] -
      chain6 diagonal[j])/(pole[2, j] pole[1, b, c]),
    carrier diagonal[j]/pole[2, j],
    pole[1, 6] carrier diagonal[j]/(pole[2, j] pole[1, b, c])};
  pairTerms[j_, b_, c_] := 4 charge^4 mixed[1, j]
    (square[j, 6] carrier - bracket12 mixed[1, j])/(common pole[1, j, 6]) {
      beta[b, c]/pole[2, b], beta[c, b]/pole[2, c]};
  poleTerm1[j_, b_, c_] := 4 charge^4 mass square[b, c]^2 mixed[1, j]
    (square[j, 6] carrier - bracket12 mixed[1, j])/
    (pole[1, j] pole[1, 6] pole[2, b] pole[2, c] pole[1, j, 6]);
  poleTerm2[j_, b_, c_] := -4 charge^4 mass square[b, c]^2 mixed[2, j]
    (square[j, 6] carrier + bracket12 mixed[2, j])/
    (pole[1, b] pole[1, c] pole[2, 6] pole[2, j] pole[1, b, c]);
  pairRemainder[j_, b_, c_] := -4 charge^4 mixed[1, j]
    (square[j, 6] carrier - bracket12 mixed[1, j]) diagonal[b] diagonal[c]/
    (mass common pole[2, b] pole[2, c]);
  mainRemainder[j_, b_, c_] := 4 charge^4 mass square[b, c]^2 diagonal[j]
    ((pole[1, 6] + pole[2, 6]) carrier + bracket12 chain6)/
    (common pole[2, 6] pole[2, j]);
  remainderNumerator = mass^2 ((pole[1, 6] + pole[2, 6]) carrier + bracket12 chain6)
    Total[Function[label, With[{j = label[[1]], b = label[[2]], c = label[[3]]},
      square[b, c]^2 diagonal[j] pole[2, b] pole[2, c]]] /@ labels] -
    pole[2, 6] Total[Function[label, With[{j = label[[1]], b = label[[2]], c = label[[3]]},
      pole[2, j] mixed[1, j] (square[j, 6] carrier - bracket12 mixed[1, j])
        diagonal[b] diagonal[c]]] /@ labels];
  mainOld = Total /@ (mainTerms @@@ labels); pairOld = Total /@ (pairTerms @@@ labels);
  mainNew = (poleTerm2 @@@ labels) + (mainRemainder @@@ labels);
  pairNew = (poleTerm1 @@@ labels) + (pairRemainder @@@ labels);
  previousTerms = Join[Flatten[mainTerms @@@ labels, 1], Flatten[pairTerms @@@ labels, 1]];
  compactTerms = Join[poleTerm1 @@@ labels, poleTerm2 @@@ labels,
    {4 charge^4 remainderNumerator/(mass common pole[2, 6] pole[2, 3] pole[2, 4] pole[2, 5])}];
  <|"ExternalMassRules" -> rules, "PreviousExpression" -> Total[previousTerms],
    "CompactExpression" -> Total[compactTerms], "PreviousTerms" -> previousTerms,
    "CompactTerms" -> compactTerms, "MainOldBlocks" -> mainOld, "MainNewBlocks" -> mainNew,
    "PairOldBlocks" -> pairOld, "PairNewBlocks" -> pairNew,
    "HeavyNumerators" -> (heavy /@ {3, 4, 5}),
    "FactoredHeavyNumerators" -> (heavyFactored /@ {3, 4, 5}),
    "RemainderNumerator" -> remainderNumerator,
    "Scope" -> "A worked QED +++- example. Production completion does not call these formulas.",
    "InternalOnShellConditions" -> {}|>
];

QEDSixPointPPPMCompactExample[model_] := Module[{parts = qedSixPointPPPMExampleParts[model]},
  If[FailureQ[parts], parts, {parts["ExternalMassRules"], parts["CompactExpression"]}]];
QEDSixPointPPPMPreviousExample[model_] := Module[{parts = qedSixPointPPPMExampleParts[model]},
  If[FailureQ[parts], parts, {parts["ExternalMassRules"], parts["PreviousExpression"]}]];

QEDSixPointPPPMIdentityRoute[model_] := Module[{parts = qedSixPointPPPMExampleParts[model]},
  If[FailureQ[parts], parts, Join[
    MapThread[<|"Identity" -> "Determinant pair and exact three-leg virtuality", "Before" -> #1,
      "After" -> #2, "InternalOnShellConditions" -> {}|> &,
      {parts["PairOldBlocks"], parts["PairNewBlocks"]}],
    MapThread[<|"Identity" -> "External Dirac and scalar Schouten", "Before" -> #1,
      "After" -> #2, "InternalOnShellConditions" -> {}|> &,
      {parts["MainOldBlocks"], parts["MainNewBlocks"]}]]]];

QEDSixPointPPPMIdentities[model_] := Module[
  {parts = qedSixPointPPPMExampleParts[model], mass, a, s, angle, square, x, y, k, pole, equal},
  If[FailureQ[parts], Return[parts]];
  mass = Mass[1] /. parts["ExternalMassRules"];
  a[i_] := Spinor[If[i <= 2, "Spin", "Helicity"], "Angle", i];
  s[i_] := Spinor[If[i <= 2, "Spin", "Helicity"], "Square", i];
  angle[i_, j_] := SpinorChain[a[i], a[j]]; square[i_, j_] := SpinorChain[s[i], s[j]];
  x[i_] := SpinorChain[s[i], Mom[1], Mom[2], s[i]];
  y[p_, i_, j_] := SpinorChain[a[i], Mom[p], s[j]];
  k = SpinorChain[a[6], Mom[1], Mom[2], a[6]];
  pole[legs__Integer] := PropDen[Mom[Multiparticle @@ Sort[{legs}]], mass];
  equal[left_, right_] := Inactive[Equal][left, right];
  {equal[square[4, 5] (2 MomProd[2, 5] y[1, 4, 5] x[4] -
      2 MomProd[2, 4] y[1, 5, 4] x[5]),
    square[4, 5]^2 mass^2 (4 MomProd[2, 4] MomProd[2, 5] -
      4 MomProd[1, 4] MomProd[1, 5]) + 2 MomProd[4, 5] x[4] x[5]],
   equal[square[3, 6] k, pole[1, 6] y[2, 6, 3] - pole[2, 6] y[1, 6, 3]],
   equal[angle[3, 6] x[3], pole[2, 3] y[1, 6, 3] - pole[1, 3] y[2, 6, 3]],
   equal[pole[1, 3, 6], pole[2, 4] + pole[2, 5] + 2 MomProd[4, 5]],
   equal[pole[1, 4, 5], pole[2, 3] + pole[2, 6] + 2 MomProd[3, 6]],
   equal[SpinorChain[s[3], Mom[1], a[6]], y[1, 6, 3]]}
];

QEDSixPointPPPMExampleProof[model_] := Module[{parts, route, rows, proof, formal},
  parts = qedSixPointPPPMExampleParts[model]; If[FailureQ[parts], Return[parts]];
  route = QEDSixPointPPPMIdentityRoute[model];
  rows = MapIndexed[Function[{step, index},
    proof = ConstructiveDiagrams`Private`completionHEZeroDirection[
      step["Before"] - step["After"], parts["ExternalMassRules"], model, 30];
    {First[index], step["Identity"], Lookup[proof, "Status", "NOT_PROVED"]}], route];
  formal = ConstructiveDiagrams`Private`withProtectedPropagators[
    Total[parts["MainNewBlocks"]] + Total[parts["PairNewBlocks"]] - parts["CompactExpression"], Together];
  AppendTo[rows, {7, "Collect the regular remainders", If[formal === 0, "PROVED_ZERO", "NOT_PROVED"]}];
  Grid[Prepend[rows, {"Step", "Identity", "Status"}], Frame -> All, Alignment -> Left]
];

(* ::Section::Closed:: *)
(* Six-point ++-- off-shell-current reference. Example only, never a completion input. *)

(* This is an independently derived minimal-QED reference, not the output of
   constructive factorization completion. The 24 ordered Dirac currents use
   the same p1 momentum reference for every external photon. Ordinary external
   Dirac, Clifford and spinor identities give the 38 factored groups below.
   All internal numerator momenta and all propagators stay off shell. *)
QEDSixPointPPMMPhysicalCurrentExample[model_] := Module[
  {mass = Quiet[ModelParameter[model, "Me"]], charge = Quiet[ModelParameter[model, "EE"]],
   rules, angleSpinor, squareSpinor, angle, square, pole, terms},
  If[!MatchQ[mass, ModelParameter[_String, _String, _String]] ||
      !MatchQ[charge, ModelParameter[_String, _String, _String]],
    Return[Failure["QEDExampleParameters", <|"MessageTemplate" ->
      "The worked QED example requires declared model parameters Me and EE."|>]]];
  rules = Thread[Mass /@ Range[6] -> {mass, mass, 0, 0, 0, 0}];
  angleSpinor[i_] := Spinor[If[i <= 2, "Spin", "Helicity"], "Angle", i];
  squareSpinor[i_] := Spinor[If[i <= 2, "Spin", "Helicity"], "Square", i];
  angle[i_, j_] := SpinorChain[angleSpinor[i], angleSpinor[j]];
  square[i_, j_] := SpinorChain[squareSpinor[i], squareSpinor[j]];
  pole[legs__Integer] := PropDen[Mom[Multiparticle @@ Sort[{legs}]], mass];
  terms = {
  (-8*mass^2*angle[1, 6]*angle[5, 6]^2*MomProd[1, 6]*square[2, 3]*square[3, 4]*square[4, 6])/
 (pole[1, 4]*pole[1, 5]*pole[1, 6]^2*pole[2, 3]*pole[1, 5, 6]),

  (-4*angle[1, 6]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*
  (mass^3*angle[2, 5]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*square[3, 4] +
   2*mass*angle[2, 5]*MomProd[1, 2]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*
    square[3, 4] + mass*angle[2, 5]*pole[1, 5]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*
    square[3, 4] + mass*angle[2, 5]*pole[1, 6]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*
    square[3, 4] - mass^2*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[2]]*
    SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*square[3, 4] -
   mass^3*angle[2, 5]*SpinorChain[angleSpinor[5], Mom[2], squareSpinor[4]]*square[3, 4] -
   mass^2*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[2]]*SpinorChain[angleSpinor[5], Mom[2],
     squareSpinor[4]]*square[3, 4] - pole[1, 6]*SpinorChain[angleSpinor[5], Mom[1],
     squareSpinor[2]]*SpinorChain[angleSpinor[5], Mom[2], squareSpinor[4]]*square[3, 4] +
   mass*angle[2, 5]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*
    SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*square[4, 6] -
   SpinorChain[angleSpinor[5], Mom[1], squareSpinor[2]]*SpinorChain[angleSpinor[5], Mom[2],
     squareSpinor[4]]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*square[4, 6]))/
 (pole[1, 3]*pole[1, 4]*pole[1, 5]*pole[1, 6]*pole[2, 5]*pole[1, 3, 6]),

  (-4*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]*square[1, 4]*
  (mass^2*angle[2, 5]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*
    SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]] + 2*angle[2, 5]*MomProd[1, 2]*
    SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[3]] + angle[2, 5]*pole[1, 5]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*
    SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]] -
   mass*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[2]]*SpinorChain[angleSpinor[5], Mom[1],
     squareSpinor[3]]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]] -
   mass^2*angle[2, 5]*SpinorChain[angleSpinor[5], Mom[2], squareSpinor[3]]*
    SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]] -
   mass*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[2]]*SpinorChain[angleSpinor[5], Mom[2],
     squareSpinor[3]]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]] +
   2*angle[2, 5]*angle[4, 6]*MomProd[1, 2]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*
    square[3, 4] + angle[2, 5]*angle[4, 6]*pole[1, 5]*SpinorChain[angleSpinor[5], Mom[1],
     squareSpinor[3]]*square[3, 4] - mass*angle[4, 6]*SpinorChain[angleSpinor[5], Mom[1],
     squareSpinor[2]]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*square[3, 4] -
   mass^2*angle[2, 5]*angle[4, 6]*SpinorChain[angleSpinor[5], Mom[2], squareSpinor[3]]*
    square[3, 4]))/(pole[1, 3]*pole[1, 4]*pole[1, 5]*pole[1, 6]*pole[2, 5]*pole[1, 4, 6]),

  (4*angle[1, 6]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]*
  (mass^3*angle[2, 5]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*square[3, 4] +
   2*mass*angle[2, 5]*MomProd[1, 2]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*
    square[3, 4] + mass*angle[2, 5]*pole[1, 5]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*
    square[3, 4] - mass^2*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[2]]*
    SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*square[3, 4] -
   mass^3*angle[2, 5]*SpinorChain[angleSpinor[5], Mom[2], squareSpinor[3]]*square[3, 4] -
   mass^2*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[2]]*SpinorChain[angleSpinor[5], Mom[2],
     squareSpinor[3]]*square[3, 4] - mass*angle[2, 5]*SpinorChain[angleSpinor[5], Mom[1],
     squareSpinor[3]]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*square[4, 6] +
   SpinorChain[angleSpinor[5], Mom[1], squareSpinor[2]]*SpinorChain[angleSpinor[5], Mom[2],
     squareSpinor[3]]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*square[4, 6]))/
 (pole[1, 3]*pole[1, 4]*pole[1, 5]*pole[1, 6]*pole[2, 5]*pole[1, 4, 6]),

  (4*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*square[1, 3]*
  (-(mass^2*angle[2, 5]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*
     SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]) - 2*angle[2, 5]*MomProd[1, 2]*
    SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[4]] - angle[2, 5]*pole[1, 5]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*
    SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]] +
   mass*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[2]]*SpinorChain[angleSpinor[5], Mom[1],
     squareSpinor[4]]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]] +
   mass^2*angle[2, 5]*SpinorChain[angleSpinor[5], Mom[2], squareSpinor[4]]*
    SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]] +
   mass*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[2]]*SpinorChain[angleSpinor[5], Mom[2],
     squareSpinor[4]]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]] +
   2*angle[2, 5]*angle[3, 6]*MomProd[1, 2]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*
    square[3, 4] + angle[2, 5]*angle[3, 6]*pole[1, 5]*SpinorChain[angleSpinor[5], Mom[1],
     squareSpinor[4]]*square[3, 4] - mass*angle[3, 6]*SpinorChain[angleSpinor[5], Mom[1],
     squareSpinor[2]]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*square[3, 4] -
   mass^2*angle[2, 5]*angle[3, 6]*SpinorChain[angleSpinor[5], Mom[2], squareSpinor[4]]*
    square[3, 4]))/(pole[1, 3]*pole[1, 4]*pole[1, 5]*pole[1, 6]*pole[2, 5]*pole[1, 3, 6]),

  (-4*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*square[1, 4]*
  (mass^2*angle[2, 6]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*
    SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]] + 2*angle[2, 6]*MomProd[1, 2]*
    SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[3]] + angle[2, 6]*pole[1, 6]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*
    SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]] -
   mass*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[2]]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]] -
   mass^2*angle[2, 6]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*
    SpinorChain[angleSpinor[6], Mom[2], squareSpinor[3]] -
   mass*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[2]]*SpinorChain[angleSpinor[6], Mom[2], squareSpinor[3]] +
   2*angle[2, 6]*angle[4, 5]*MomProd[1, 2]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*
    square[3, 4] + angle[2, 6]*angle[4, 5]*pole[1, 6]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[3]]*square[3, 4] - mass*angle[4, 5]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[2]]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*square[3, 4] -
   mass^2*angle[2, 6]*angle[4, 5]*SpinorChain[angleSpinor[6], Mom[2], squareSpinor[3]]*
    square[3, 4]))/(pole[1, 3]*pole[1, 4]*pole[1, 5]*pole[1, 6]*pole[2, 6]*pole[1, 4, 5]),

  (4*angle[1, 5]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*
  (mass^3*angle[2, 6]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*square[3, 4] +
   2*mass*angle[2, 6]*MomProd[1, 2]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*
    square[3, 4] + mass*angle[2, 6]*pole[1, 6]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*
    square[3, 4] - mass^2*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[2]]*
    SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*square[3, 4] -
   mass^3*angle[2, 6]*SpinorChain[angleSpinor[6], Mom[2], squareSpinor[3]]*square[3, 4] -
   mass^2*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[2]]*SpinorChain[angleSpinor[6], Mom[2],
     squareSpinor[3]]*square[3, 4] - mass*angle[2, 6]*SpinorChain[angleSpinor[5], Mom[1],
     squareSpinor[3]]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*square[4, 5] +
   SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[2]]*SpinorChain[angleSpinor[6], Mom[2], squareSpinor[3]]*square[4, 5]))/
 (pole[1, 3]*pole[1, 4]*pole[1, 5]*pole[1, 6]*pole[2, 6]*pole[1, 4, 5]),

  (-4*angle[1, 5]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*
  (mass^3*angle[2, 6]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]*square[3, 4] +
   2*mass*angle[2, 6]*MomProd[1, 2]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]*
    square[3, 4] + mass*angle[2, 6]*pole[1, 5]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]*
    square[3, 4] + mass*angle[2, 6]*pole[1, 6]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]*
    square[3, 4] - mass^2*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[2]]*
    SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]*square[3, 4] -
   mass^3*angle[2, 6]*SpinorChain[angleSpinor[6], Mom[2], squareSpinor[4]]*square[3, 4] -
   mass^2*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[2]]*SpinorChain[angleSpinor[6], Mom[2],
     squareSpinor[4]]*square[3, 4] - pole[1, 5]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[2]]*SpinorChain[angleSpinor[6], Mom[2], squareSpinor[4]]*square[3, 4] +
   mass*angle[2, 6]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*
    SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]*square[4, 5] -
   SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[2]]*SpinorChain[angleSpinor[6], Mom[2], squareSpinor[4]]*square[4, 5]))/
 (pole[1, 3]*pole[1, 4]*pole[1, 5]*pole[1, 6]*pole[2, 6]*pole[1, 3, 5]),

  (4*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*square[1, 3]*
  (-(mass^2*angle[2, 6]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*
     SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]) - 2*angle[2, 6]*MomProd[1, 2]*
    SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[4]] - angle[2, 6]*pole[1, 6]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*
    SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]] +
   mass*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[2]]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]] +
   mass^2*angle[2, 6]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*
    SpinorChain[angleSpinor[6], Mom[2], squareSpinor[4]] +
   mass*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[2]]*SpinorChain[angleSpinor[6], Mom[2], squareSpinor[4]] +
   2*angle[2, 6]*angle[3, 5]*MomProd[1, 2]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]*
    square[3, 4] + angle[2, 6]*angle[3, 5]*pole[1, 6]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[4]]*square[3, 4] - mass*angle[3, 5]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[2]]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]*square[3, 4] -
   mass^2*angle[2, 6]*angle[3, 5]*SpinorChain[angleSpinor[6], Mom[2], squareSpinor[4]]*
    square[3, 4]))/(pole[1, 3]*pole[1, 4]*pole[1, 5]*pole[1, 6]*pole[2, 6]*pole[1, 3, 5]),

  (8*mass^3*angle[1, 6]*angle[5, 6]^2*MomProd[1, 6]*SpinorChain[squareSpinor[3], Mom[1],
   angleSpinor[2]]*square[3, 4]*square[4, 6])/(pole[1, 3]*pole[1, 4]*pole[1, 5]*pole[1, 6]^2*
  pole[2, 3]*pole[1, 5, 6]),

  (-8*angle[1, 6]*angle[5, 6]*MomProd[1, 6]*SpinorChain[squareSpinor[4], Mom[1], angleSpinor[5]]*
  square[2, 3]*square[3, 4])/(pole[1, 4]*pole[1, 6]^2*pole[2, 3]*pole[1, 5, 6]),

  (-8*mass^2*angle[1, 6]*angle[5, 6]*MomProd[1, 6]*SpinorChain[squareSpinor[4], Mom[1],
   angleSpinor[5]]*square[2, 3]*square[3, 4])/(pole[1, 4]*pole[1, 5]*pole[1, 6]^2*pole[2, 3]*
  pole[1, 5, 6]),

  (-16*angle[1, 6]*angle[5, 6]*MomProd[1, 6]^2*SpinorChain[squareSpinor[4], Mom[1], angleSpinor[5]]*
  square[2, 3]*square[3, 4])/(pole[1, 4]*pole[1, 5]*pole[1, 6]^2*pole[2, 3]*pole[1, 5, 6]),

  (-8*mass^4*angle[1, 6]*angle[5, 6]*MomProd[1, 6]*SpinorChain[squareSpinor[4], Mom[1],
   angleSpinor[5]]*square[2, 3]*square[3, 4])/(pole[1, 3]*pole[1, 4]*pole[1, 5]*pole[1, 6]^2*
  pole[2, 3]*pole[1, 5, 6]),

  (8*mass*angle[1, 6]*angle[5, 6]*MomProd[1, 6]*SpinorChain[squareSpinor[3], Mom[1], angleSpinor[2]]*
  SpinorChain[squareSpinor[4], Mom[1], angleSpinor[5]]*square[3, 4])/
 (pole[1, 3]*pole[1, 4]*pole[1, 6]^2*pole[2, 3]*pole[1, 5, 6]),

  (8*mass^3*angle[1, 6]*angle[5, 6]*MomProd[1, 6]*SpinorChain[squareSpinor[3], Mom[1],
   angleSpinor[2]]*SpinorChain[squareSpinor[4], Mom[1], angleSpinor[5]]*square[3, 4])/
 (pole[1, 3]*pole[1, 4]*pole[1, 5]*pole[1, 6]^2*pole[2, 3]*pole[1, 5, 6]),

  (16*mass*angle[1, 6]*angle[5, 6]*MomProd[1, 2]*MomProd[1, 6]*SpinorChain[squareSpinor[3], Mom[1],
   angleSpinor[2]]*SpinorChain[squareSpinor[4], Mom[1], angleSpinor[5]]*square[3, 4])/
 (pole[1, 3]*pole[1, 4]*pole[1, 5]*pole[1, 6]^2*pole[2, 3]*pole[1, 5, 6]),

  (16*mass*angle[1, 6]*angle[5, 6]*MomProd[1, 6]^2*SpinorChain[squareSpinor[3], Mom[1],
   angleSpinor[2]]*SpinorChain[squareSpinor[4], Mom[1], angleSpinor[5]]*square[3, 4])/
 (pole[1, 3]*pole[1, 4]*pole[1, 5]*pole[1, 6]^2*pole[2, 3]*pole[1, 5, 6]),

  (8*angle[1, 6]*MomProd[1, 6]*SpinorChain[squareSpinor[4], Mom[1], angleSpinor[6]]*
  (mass*SpinorChain[squareSpinor[3], Mom[1], angleSpinor[2]]*SpinorChain[squareSpinor[3], Mom[1],
     angleSpinor[5]]*SpinorChain[squareSpinor[4], Mom[1], angleSpinor[5]] +
   mass*SpinorChain[squareSpinor[3], Mom[1], angleSpinor[2]]*SpinorChain[squareSpinor[3], Mom[2],
     angleSpinor[5]]*SpinorChain[squareSpinor[4], Mom[1], angleSpinor[5]] -
   mass^2*SpinorChain[squareSpinor[3], Mom[1], angleSpinor[5]]*SpinorChain[squareSpinor[4], Mom[1],
     angleSpinor[5]]*square[2, 3] - 2*MomProd[1, 2]*SpinorChain[squareSpinor[3], Mom[1],
     angleSpinor[5]]*SpinorChain[squareSpinor[4], Mom[1], angleSpinor[5]]*square[2, 3] -
   pole[1, 3]*SpinorChain[squareSpinor[3], Mom[1], angleSpinor[5]]*
    SpinorChain[squareSpinor[4], Mom[1], angleSpinor[5]]*square[2, 3] +
   mass^2*SpinorChain[squareSpinor[3], Mom[2], angleSpinor[5]]*SpinorChain[squareSpinor[4], Mom[1],
     angleSpinor[5]]*square[2, 3] - mass*angle[5, 6]*SpinorChain[squareSpinor[3], Mom[1],
     angleSpinor[2]]*SpinorChain[squareSpinor[3], Mom[1], angleSpinor[5]]*square[4, 6] +
   2*angle[5, 6]*MomProd[1, 2]*SpinorChain[squareSpinor[3], Mom[1], angleSpinor[5]]*square[2, 3]*
    square[4, 6] + angle[5, 6]*pole[1, 3]*SpinorChain[squareSpinor[3], Mom[1], angleSpinor[5]]*
    square[2, 3]*square[4, 6] - mass^2*angle[5, 6]*SpinorChain[squareSpinor[3], Mom[2],
     angleSpinor[5]]*square[2, 3]*square[4, 6]))/(pole[1, 3]*pole[1, 4]*pole[1, 5]*pole[1, 6]^2*
  pole[2, 3]*pole[1, 4, 6]),

  (4*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]*square[1, 4]*
  (-(mass^2*angle[5, 6]*SpinorChain[angleSpinor[2], Mom[1], squareSpinor[3]]*
     SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]) -
   mass^2*angle[5, 6]*SpinorChain[angleSpinor[2], Mom[1], squareSpinor[3]]*
    SpinorChain[angleSpinor[5], Mom[2], squareSpinor[3]] -
   angle[4, 6]*SpinorChain[angleSpinor[2], Mom[1], squareSpinor[3]]*
    SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*SpinorChain[angleSpinor[5], Mom[2],
     squareSpinor[3]] + mass^3*angle[5, 6]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*
    square[2, 3] + 2*mass*angle[5, 6]*MomProd[1, 2]*SpinorChain[angleSpinor[5], Mom[1],
     squareSpinor[3]]*square[2, 3] + mass*angle[5, 6]*pole[1, 3]*
    SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*square[2, 3] +
   mass*angle[4, 6]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*
    SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*square[2, 3] -
   mass^3*angle[5, 6]*SpinorChain[angleSpinor[5], Mom[2], squareSpinor[3]]*square[2, 3]))/
 (pole[1, 3]*pole[1, 4]*pole[1, 5]*pole[1, 6]*pole[2, 3]*pole[1, 4, 6]),

  (-4*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*square[1, 4]*
  (-(mass^2*angle[5, 6]*SpinorChain[angleSpinor[2], Mom[1], squareSpinor[3]]*
     SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]) -
   mass^2*angle[5, 6]*SpinorChain[angleSpinor[2], Mom[1], squareSpinor[3]]*
    SpinorChain[angleSpinor[6], Mom[2], squareSpinor[3]] -
   angle[5, 6]*pole[1, 4]*SpinorChain[angleSpinor[2], Mom[1], squareSpinor[3]]*
    SpinorChain[angleSpinor[6], Mom[2], squareSpinor[3]] +
   angle[4, 6]*SpinorChain[angleSpinor[2], Mom[1], squareSpinor[3]]*
    SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*SpinorChain[angleSpinor[6], Mom[2],
     squareSpinor[3]] + mass^3*angle[5, 6]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*
    square[2, 3] + 2*mass*angle[5, 6]*MomProd[1, 2]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[3]]*square[2, 3] + mass*angle[5, 6]*pole[1, 3]*
    SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*square[2, 3] +
   mass*angle[5, 6]*pole[1, 4]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*square[2, 3] -
   mass*angle[4, 6]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*
    SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*square[2, 3] -
   mass^3*angle[5, 6]*SpinorChain[angleSpinor[6], Mom[2], squareSpinor[3]]*square[2, 3]))/
 (pole[1, 3]*pole[1, 4]*pole[1, 5]*pole[1, 6]*pole[2, 3]*pole[1, 4, 5]),

  (-4*angle[1, 5]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*
  (-(mass*SpinorChain[angleSpinor[2], Mom[1], squareSpinor[3]]*SpinorChain[angleSpinor[6], Mom[1],
      squareSpinor[3]]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]) -
   mass*SpinorChain[angleSpinor[2], Mom[1], squareSpinor[3]]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[4]]*SpinorChain[angleSpinor[6], Mom[2], squareSpinor[3]] +
   mass^2*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[4]]*square[2, 3] + 2*MomProd[1, 2]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[3]]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]*square[2, 3] +
   pole[1, 3]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*
    SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]*square[2, 3] -
   mass^2*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]*SpinorChain[angleSpinor[6], Mom[2],
     squareSpinor[3]]*square[2, 3] - mass*angle[5, 6]*SpinorChain[angleSpinor[2], Mom[1],
     squareSpinor[3]]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*square[4, 5] +
   2*angle[5, 6]*MomProd[1, 2]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*square[2, 3]*
    square[4, 5] + angle[5, 6]*pole[1, 3]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*
    square[2, 3]*square[4, 5] - mass^2*angle[5, 6]*SpinorChain[angleSpinor[6], Mom[2],
     squareSpinor[3]]*square[2, 3]*square[4, 5]))/(pole[1, 3]*pole[1, 4]*pole[1, 5]*pole[1, 6]*
  pole[2, 3]*pole[1, 4, 5]),

  (4*angle[1, 6]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*
  (mass*SpinorChain[angleSpinor[2], Mom[1], squareSpinor[4]]*SpinorChain[angleSpinor[5], Mom[1],
     squareSpinor[3]]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]] +
   mass*SpinorChain[angleSpinor[2], Mom[1], squareSpinor[4]]*SpinorChain[angleSpinor[5], Mom[1],
     squareSpinor[3]]*SpinorChain[angleSpinor[5], Mom[2], squareSpinor[4]] -
   mass^2*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*SpinorChain[angleSpinor[5], Mom[1],
     squareSpinor[4]]*square[2, 4] - 2*MomProd[1, 2]*SpinorChain[angleSpinor[5], Mom[1],
     squareSpinor[3]]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*square[2, 4] -
   pole[1, 4]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*
    SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*square[2, 4] +
   mass^2*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*SpinorChain[angleSpinor[5], Mom[2],
     squareSpinor[4]]*square[2, 4] - mass*angle[5, 6]*SpinorChain[angleSpinor[2], Mom[1],
     squareSpinor[4]]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*square[3, 6] +
   2*angle[5, 6]*MomProd[1, 2]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*square[2, 4]*
    square[3, 6] + angle[5, 6]*pole[1, 4]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*
    square[2, 4]*square[3, 6] - mass^2*angle[5, 6]*SpinorChain[angleSpinor[5], Mom[2],
     squareSpinor[4]]*square[2, 4]*square[3, 6]))/(pole[1, 3]*pole[1, 4]*pole[1, 5]*pole[1, 6]*
  pole[2, 4]*pole[1, 3, 6]),

  (4*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*square[1, 3]*
  (-(mass^2*angle[5, 6]*SpinorChain[angleSpinor[2], Mom[1], squareSpinor[4]]*
     SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]) -
   mass^2*angle[5, 6]*SpinorChain[angleSpinor[2], Mom[1], squareSpinor[4]]*
    SpinorChain[angleSpinor[5], Mom[2], squareSpinor[4]] -
   angle[3, 6]*SpinorChain[angleSpinor[2], Mom[1], squareSpinor[4]]*
    SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*SpinorChain[angleSpinor[5], Mom[2],
     squareSpinor[4]] + mass^3*angle[5, 6]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*
    square[2, 4] + 2*mass*angle[5, 6]*MomProd[1, 2]*SpinorChain[angleSpinor[5], Mom[1],
     squareSpinor[4]]*square[2, 4] + mass*angle[5, 6]*pole[1, 4]*
    SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*square[2, 4] +
   mass*angle[3, 6]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*
    SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*square[2, 4] -
   mass^3*angle[5, 6]*SpinorChain[angleSpinor[5], Mom[2], squareSpinor[4]]*square[2, 4]))/
 (pole[1, 3]*pole[1, 4]*pole[1, 5]*pole[1, 6]*pole[2, 4]*pole[1, 3, 6]),

  (-4*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*square[1, 3]*
  (-(mass^2*angle[5, 6]*SpinorChain[angleSpinor[2], Mom[1], squareSpinor[4]]*
     SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]) -
   mass^2*angle[5, 6]*SpinorChain[angleSpinor[2], Mom[1], squareSpinor[4]]*
    SpinorChain[angleSpinor[6], Mom[2], squareSpinor[4]] -
   angle[5, 6]*pole[1, 3]*SpinorChain[angleSpinor[2], Mom[1], squareSpinor[4]]*
    SpinorChain[angleSpinor[6], Mom[2], squareSpinor[4]] +
   angle[3, 6]*SpinorChain[angleSpinor[2], Mom[1], squareSpinor[4]]*
    SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*SpinorChain[angleSpinor[6], Mom[2],
     squareSpinor[4]] + mass^3*angle[5, 6]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]*
    square[2, 4] + 2*mass*angle[5, 6]*MomProd[1, 2]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[4]]*square[2, 4] + mass*angle[5, 6]*pole[1, 3]*
    SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]*square[2, 4] +
   mass*angle[5, 6]*pole[1, 4]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]*square[2, 4] -
   mass*angle[3, 6]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*
    SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]*square[2, 4] -
   mass^3*angle[5, 6]*SpinorChain[angleSpinor[6], Mom[2], squareSpinor[4]]*square[2, 4]))/
 (pole[1, 3]*pole[1, 4]*pole[1, 5]*pole[1, 6]*pole[2, 4]*pole[1, 3, 5]),

  (-4*angle[1, 5]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*
  (-(mass*SpinorChain[angleSpinor[2], Mom[1], squareSpinor[4]]*SpinorChain[angleSpinor[6], Mom[1],
      squareSpinor[3]]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]) -
   mass*SpinorChain[angleSpinor[2], Mom[1], squareSpinor[4]]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[3]]*SpinorChain[angleSpinor[6], Mom[2], squareSpinor[4]] +
   mass^2*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[4]]*square[2, 4] + 2*MomProd[1, 2]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[3]]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]*square[2, 4] +
   pole[1, 4]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*
    SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]*square[2, 4] -
   mass^2*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*SpinorChain[angleSpinor[6], Mom[2],
     squareSpinor[4]]*square[2, 4] - mass*angle[5, 6]*SpinorChain[angleSpinor[2], Mom[1],
     squareSpinor[4]]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]*square[3, 5] +
   2*angle[5, 6]*MomProd[1, 2]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]*square[2, 4]*
    square[3, 5] + angle[5, 6]*pole[1, 4]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]*
    square[2, 4]*square[3, 5] - mass^2*angle[5, 6]*SpinorChain[angleSpinor[6], Mom[2],
     squareSpinor[4]]*square[2, 4]*square[3, 5]))/(pole[1, 3]*pole[1, 4]*pole[1, 5]*pole[1, 6]*
  pole[2, 4]*pole[1, 3, 5]),

  (4*square[1, 4]*square[3, 4]*
  (-(mass^4*angle[2, 5]*angle[5, 6]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]) -
   mass^2*angle[2, 5]*angle[5, 6]*pole[1, 5]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]] -
   angle[2, 5]*angle[5, 6]*pole[1, 3]*pole[1, 5]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[3]] - angle[2, 5]*angle[5, 6]*pole[1, 4]*pole[1, 5]*
    SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]] +
   mass^3*angle[5, 6]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[2]]*
    SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]] + 2*mass*angle[5, 6]*MomProd[1, 2]*
    SpinorChain[angleSpinor[5], Mom[1], squareSpinor[2]]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[3]] + mass*angle[5, 6]*pole[1, 3]*SpinorChain[angleSpinor[5], Mom[1],
     squareSpinor[2]]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]] +
   mass*angle[5, 6]*pole[1, 4]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[2]]*
    SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]] -
   mass^2*angle[2, 5]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*
    SpinorChain[angleSpinor[5], Mom[1], Mom[2], angleSpinor[6]] -
   angle[2, 5]*pole[1, 3]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*
    SpinorChain[angleSpinor[5], Mom[1], Mom[2], angleSpinor[6]] -
   angle[2, 5]*pole[1, 4]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*
    SpinorChain[angleSpinor[5], Mom[1], Mom[2], angleSpinor[6]] -
   mass*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[2]]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[3]]*SpinorChain[angleSpinor[5], Mom[1], Mom[2], angleSpinor[6]] +
   mass^2*angle[2, 5]*angle[4, 6]*angle[5, 6]*pole[1, 5]*square[3, 4] -
   mass^3*angle[4, 6]*angle[5, 6]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[2]]*
    square[3, 4] + mass^2*angle[2, 5]*angle[4, 6]*SpinorChain[angleSpinor[5], Mom[1], Mom[2],
     angleSpinor[6]]*square[3, 4]))/(pole[1, 3]*pole[1, 4]*pole[1, 5]*pole[1, 6]*pole[2, 5]*
  pole[1, 3, 4]),

  (4*square[1, 3]*square[3, 4]*(mass^4*angle[2, 5]*angle[5, 6]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[4]] + mass^2*angle[2, 5]*angle[5, 6]*pole[1, 5]*
    SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]] + angle[2, 5]*angle[5, 6]*pole[1, 3]*
    pole[1, 5]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]] +
   angle[2, 5]*angle[5, 6]*pole[1, 4]*pole[1, 5]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[4]] - mass^3*angle[5, 6]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[2]]*
    SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]] - 2*mass*angle[5, 6]*MomProd[1, 2]*
    SpinorChain[angleSpinor[5], Mom[1], squareSpinor[2]]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[4]] - mass*angle[5, 6]*pole[1, 3]*SpinorChain[angleSpinor[5], Mom[1],
     squareSpinor[2]]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]] -
   mass*angle[5, 6]*pole[1, 4]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[2]]*
    SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]] +
   mass^2*angle[2, 5]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]*
    SpinorChain[angleSpinor[5], Mom[1], Mom[2], angleSpinor[6]] +
   angle[2, 5]*pole[1, 3]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]*
    SpinorChain[angleSpinor[5], Mom[1], Mom[2], angleSpinor[6]] +
   angle[2, 5]*pole[1, 4]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]*
    SpinorChain[angleSpinor[5], Mom[1], Mom[2], angleSpinor[6]] +
   mass*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[2]]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[4]]*SpinorChain[angleSpinor[5], Mom[1], Mom[2], angleSpinor[6]] +
   mass^2*angle[2, 5]*angle[3, 6]*angle[5, 6]*pole[1, 5]*square[3, 4] -
   mass^3*angle[3, 6]*angle[5, 6]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[2]]*
    square[3, 4] + mass^2*angle[2, 5]*angle[3, 6]*SpinorChain[angleSpinor[5], Mom[1], Mom[2],
     angleSpinor[6]]*square[3, 4]))/(pole[1, 3]*pole[1, 4]*pole[1, 5]*pole[1, 6]*pole[2, 5]*
  pole[1, 3, 4]),

  (-4*square[1, 4]*square[3, 4]*
  (-(mass^4*angle[2, 6]*angle[5, 6]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]) -
   2*mass^2*angle[2, 6]*angle[5, 6]*MomProd[1, 2]*SpinorChain[angleSpinor[5], Mom[1],
     squareSpinor[3]] - 2*angle[2, 6]*angle[5, 6]*MomProd[1, 2]*pole[1, 3]*
    SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]] - 2*angle[2, 6]*angle[5, 6]*MomProd[1, 2]*
    pole[1, 4]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]] -
   mass^2*angle[2, 6]*angle[5, 6]*pole[1, 6]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]] -
   angle[2, 6]*angle[5, 6]*pole[1, 3]*pole[1, 6]*SpinorChain[angleSpinor[5], Mom[1],
     squareSpinor[3]] - angle[2, 6]*angle[5, 6]*pole[1, 4]*pole[1, 6]*
    SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]] +
   mass^3*angle[5, 6]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*
    SpinorChain[angleSpinor[6], Mom[1], squareSpinor[2]] + mass*angle[5, 6]*pole[1, 3]*
    SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[2]] + mass*angle[5, 6]*pole[1, 4]*SpinorChain[angleSpinor[5], Mom[1],
     squareSpinor[3]]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[2]] +
   mass^2*angle[2, 6]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*
    SpinorChain[angleSpinor[5], Mom[1], Mom[2], angleSpinor[6]] +
   angle[2, 6]*pole[1, 3]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*
    SpinorChain[angleSpinor[5], Mom[1], Mom[2], angleSpinor[6]] +
   angle[2, 6]*pole[1, 4]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*
    SpinorChain[angleSpinor[5], Mom[1], Mom[2], angleSpinor[6]] +
   mass*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[3]]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[2]]*SpinorChain[angleSpinor[5], Mom[1], Mom[2], angleSpinor[6]] +
   2*mass^2*angle[2, 6]*angle[4, 5]*angle[5, 6]*MomProd[1, 2]*square[3, 4] +
   mass^2*angle[2, 6]*angle[4, 5]*angle[5, 6]*pole[1, 6]*square[3, 4] -
   mass^3*angle[4, 5]*angle[5, 6]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[2]]*
    square[3, 4] - mass^2*angle[2, 6]*angle[4, 5]*SpinorChain[angleSpinor[5], Mom[1], Mom[2],
     angleSpinor[6]]*square[3, 4]))/(pole[1, 3]*pole[1, 4]*pole[1, 5]*pole[1, 6]*pole[2, 6]*
  pole[1, 3, 4]),

  (-4*square[1, 3]*square[3, 4]*(mass^4*angle[2, 6]*angle[5, 6]*SpinorChain[angleSpinor[5], Mom[1],
     squareSpinor[4]] + 2*mass^2*angle[2, 6]*angle[5, 6]*MomProd[1, 2]*
    SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]] + 2*angle[2, 6]*angle[5, 6]*MomProd[1, 2]*
    pole[1, 3]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]] +
   2*angle[2, 6]*angle[5, 6]*MomProd[1, 2]*pole[1, 4]*SpinorChain[angleSpinor[5], Mom[1],
     squareSpinor[4]] + mass^2*angle[2, 6]*angle[5, 6]*pole[1, 6]*
    SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]] + angle[2, 6]*angle[5, 6]*pole[1, 3]*
    pole[1, 6]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]] +
   angle[2, 6]*angle[5, 6]*pole[1, 4]*pole[1, 6]*SpinorChain[angleSpinor[5], Mom[1],
     squareSpinor[4]] - mass^3*angle[5, 6]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*
    SpinorChain[angleSpinor[6], Mom[1], squareSpinor[2]] - mass*angle[5, 6]*pole[1, 3]*
    SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[2]] - mass*angle[5, 6]*pole[1, 4]*SpinorChain[angleSpinor[5], Mom[1],
     squareSpinor[4]]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[2]] -
   mass^2*angle[2, 6]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*
    SpinorChain[angleSpinor[5], Mom[1], Mom[2], angleSpinor[6]] -
   angle[2, 6]*pole[1, 3]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*
    SpinorChain[angleSpinor[5], Mom[1], Mom[2], angleSpinor[6]] -
   angle[2, 6]*pole[1, 4]*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*
    SpinorChain[angleSpinor[5], Mom[1], Mom[2], angleSpinor[6]] -
   mass*SpinorChain[angleSpinor[5], Mom[1], squareSpinor[4]]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[2]]*SpinorChain[angleSpinor[5], Mom[1], Mom[2], angleSpinor[6]] +
   2*mass^2*angle[2, 6]*angle[3, 5]*angle[5, 6]*MomProd[1, 2]*square[3, 4] +
   mass^2*angle[2, 6]*angle[3, 5]*angle[5, 6]*pole[1, 6]*square[3, 4] -
   mass^3*angle[3, 5]*angle[5, 6]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[2]]*
    square[3, 4] - mass^2*angle[2, 6]*angle[3, 5]*SpinorChain[angleSpinor[5], Mom[1], Mom[2],
     angleSpinor[6]]*square[3, 4]))/(pole[1, 3]*pole[1, 4]*pole[1, 5]*pole[1, 6]*pole[2, 6]*
  pole[1, 3, 4]),

  (-8*mass^2*angle[1, 6]*angle[5, 6]^2*MomProd[1, 6]*SpinorChain[squareSpinor[3], Mom[1], Mom[2],
   squareSpinor[4]]*square[2, 3]*square[4, 6])/(pole[1, 3]*pole[1, 4]*pole[1, 5]*pole[1, 6]^2*
  pole[2, 3]*pole[1, 5, 6]),

  (-8*angle[1, 6]*angle[5, 6]*MomProd[1, 6]*SpinorChain[squareSpinor[4], Mom[1], angleSpinor[5]]*
  SpinorChain[squareSpinor[3], Mom[1], Mom[2], squareSpinor[4]]*square[2, 3])/
 (pole[1, 3]*pole[1, 4]*pole[1, 6]^2*pole[2, 3]*pole[1, 5, 6]),

  (-8*mass^2*angle[1, 6]*angle[5, 6]*MomProd[1, 6]*SpinorChain[squareSpinor[4], Mom[1],
   angleSpinor[5]]*SpinorChain[squareSpinor[3], Mom[1], Mom[2], squareSpinor[4]]*square[2, 3])/
 (pole[1, 3]*pole[1, 4]*pole[1, 5]*pole[1, 6]^2*pole[2, 3]*pole[1, 5, 6]),

  (-16*angle[1, 6]*angle[5, 6]*MomProd[1, 6]^2*SpinorChain[squareSpinor[4], Mom[1], angleSpinor[5]]*
  SpinorChain[squareSpinor[3], Mom[1], Mom[2], squareSpinor[4]]*square[2, 3])/
 (pole[1, 3]*pole[1, 4]*pole[1, 5]*pole[1, 6]^2*pole[2, 3]*pole[1, 5, 6]),

  (-8*mass*angle[1, 6]*angle[5, 6]*MomProd[1, 6]*SpinorChain[squareSpinor[3], Mom[1], angleSpinor[2]]*
  SpinorChain[squareSpinor[4], Mom[1], angleSpinor[5]]*SpinorChain[squareSpinor[3], Mom[1], Mom[2],
   squareSpinor[4]])/(pole[1, 3]*pole[1, 4]*pole[1, 5]*pole[1, 6]^2*pole[2, 3]*pole[1, 5, 6]),

  (-8*angle[1, 6]*angle[5, 6]*MomProd[1, 6]*
  (mass*SpinorChain[squareSpinor[3], Mom[1], angleSpinor[5]]*SpinorChain[squareSpinor[4], Mom[1],
     angleSpinor[2]]*SpinorChain[squareSpinor[3], Mom[1], Mom[2], squareSpinor[4]] +
   mass^2*SpinorChain[squareSpinor[3], Mom[1], angleSpinor[5]]*SpinorChain[squareSpinor[3], Mom[1],
     Mom[2], squareSpinor[4]]*square[2, 4] + 2*MomProd[1, 6]*SpinorChain[squareSpinor[3], Mom[1],
     angleSpinor[5]]*SpinorChain[squareSpinor[3], Mom[1], Mom[2], squareSpinor[4]]*square[2, 4] +
   pole[1, 5]*SpinorChain[squareSpinor[3], Mom[1], angleSpinor[5]]*
    SpinorChain[squareSpinor[3], Mom[1], Mom[2], squareSpinor[4]]*square[2, 4] +
   mass^3*SpinorChain[squareSpinor[3], Mom[1], angleSpinor[5]]*SpinorChain[squareSpinor[4], Mom[1],
     angleSpinor[2]]*square[3, 4] + 2*mass*MomProd[1, 6]*SpinorChain[squareSpinor[3], Mom[1],
     angleSpinor[5]]*SpinorChain[squareSpinor[4], Mom[1], angleSpinor[2]]*square[3, 4] +
   mass*pole[1, 5]*SpinorChain[squareSpinor[3], Mom[1], angleSpinor[5]]*
    SpinorChain[squareSpinor[4], Mom[1], angleSpinor[2]]*square[3, 4] -
   mass^4*SpinorChain[squareSpinor[3], Mom[1], angleSpinor[5]]*square[2, 4]*square[3, 4] -
   2*mass^2*MomProd[1, 2]*SpinorChain[squareSpinor[3], Mom[1], angleSpinor[5]]*square[2, 4]*
    square[3, 4] - 4*MomProd[1, 2]*MomProd[1, 6]*SpinorChain[squareSpinor[3], Mom[1],
     angleSpinor[5]]*square[2, 4]*square[3, 4] - mass^2*pole[1, 4]*
    SpinorChain[squareSpinor[3], Mom[1], angleSpinor[5]]*square[2, 4]*square[3, 4] -
   2*MomProd[1, 6]*pole[1, 4]*SpinorChain[squareSpinor[3], Mom[1], angleSpinor[5]]*square[2, 4]*
    square[3, 4] - 2*MomProd[1, 2]*pole[1, 5]*SpinorChain[squareSpinor[3], Mom[1], angleSpinor[5]]*
    square[2, 4]*square[3, 4] - pole[1, 4]*pole[1, 5]*SpinorChain[squareSpinor[3], Mom[1],
     angleSpinor[5]]*square[2, 4]*square[3, 4] + mass^2*angle[5, 6]*
    SpinorChain[squareSpinor[3], Mom[1], Mom[2], squareSpinor[4]]*square[2, 4]*square[3, 6] +
   mass^3*angle[5, 6]*SpinorChain[squareSpinor[4], Mom[1], angleSpinor[2]]*square[3, 4]*
    square[3, 6] - 2*mass^2*angle[5, 6]*MomProd[1, 2]*square[2, 4]*square[3, 4]*square[3, 6] -
   mass^2*angle[5, 6]*pole[1, 4]*square[2, 4]*square[3, 4]*square[3, 6]))/
 (pole[1, 3]*pole[1, 4]*pole[1, 5]*pole[1, 6]^2*pole[2, 4]*pole[1, 5, 6]),

  (-4*angle[1, 5]*angle[5, 6]*(-(mass*SpinorChain[angleSpinor[2], Mom[1], squareSpinor[3]]*
     SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]*SpinorChain[squareSpinor[3], Mom[1],
      Mom[2], squareSpinor[4]]) - mass^2*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]*
    SpinorChain[squareSpinor[3], Mom[1], Mom[2], squareSpinor[4]]*square[2, 3] -
   pole[1, 5]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]*
    SpinorChain[squareSpinor[3], Mom[1], Mom[2], squareSpinor[4]]*square[2, 3] -
   pole[1, 6]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]*
    SpinorChain[squareSpinor[3], Mom[1], Mom[2], squareSpinor[4]]*square[2, 3] +
   mass^3*SpinorChain[angleSpinor[2], Mom[1], squareSpinor[3]]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[4]]*square[3, 4] + 2*mass*MomProd[1, 2]*SpinorChain[angleSpinor[2], Mom[1],
     squareSpinor[3]]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]*square[3, 4] +
   mass*pole[1, 5]*SpinorChain[angleSpinor[2], Mom[1], squareSpinor[3]]*
    SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]*square[3, 4] +
   mass*pole[1, 6]*SpinorChain[angleSpinor[2], Mom[1], squareSpinor[3]]*
    SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]*square[3, 4] -
   mass^4*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]*square[2, 3]*square[3, 4] -
   mass^2*pole[1, 3]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]*square[2, 3]*
    square[3, 4] - pole[1, 3]*pole[1, 5]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[4]]*
    square[2, 3]*square[3, 4] - pole[1, 3]*pole[1, 6]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[4]]*square[2, 3]*square[3, 4] + mass^2*angle[5, 6]*
    SpinorChain[squareSpinor[3], Mom[1], Mom[2], squareSpinor[4]]*square[2, 3]*square[4, 5] -
   mass^3*angle[5, 6]*SpinorChain[angleSpinor[2], Mom[1], squareSpinor[3]]*square[3, 4]*
    square[4, 5] + mass^2*angle[5, 6]*pole[1, 3]*square[2, 3]*square[3, 4]*square[4, 5]))/
 (pole[1, 3]*pole[1, 4]*pole[1, 5]*pole[1, 6]*pole[2, 3]*pole[1, 5, 6]),

  (4*angle[1, 5]*angle[5, 6]*(mass*SpinorChain[angleSpinor[2], Mom[1], squareSpinor[4]]*
    SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*SpinorChain[squareSpinor[3], Mom[1],
     Mom[2], squareSpinor[4]] + mass^2*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*
    SpinorChain[squareSpinor[3], Mom[1], Mom[2], squareSpinor[4]]*square[2, 4] +
   pole[1, 5]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*
    SpinorChain[squareSpinor[3], Mom[1], Mom[2], squareSpinor[4]]*square[2, 4] +
   pole[1, 6]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*
    SpinorChain[squareSpinor[3], Mom[1], Mom[2], squareSpinor[4]]*square[2, 4] +
   mass^3*SpinorChain[angleSpinor[2], Mom[1], squareSpinor[4]]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[3]]*square[3, 4] + mass*pole[1, 5]*SpinorChain[angleSpinor[2], Mom[1],
     squareSpinor[4]]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*square[3, 4] +
   mass*pole[1, 6]*SpinorChain[angleSpinor[2], Mom[1], squareSpinor[4]]*
    SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*square[3, 4] -
   mass^4*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*square[2, 4]*square[3, 4] -
   2*mass^2*MomProd[1, 2]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*square[2, 4]*
    square[3, 4] - mass^2*pole[1, 4]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*
    square[2, 4]*square[3, 4] - 2*MomProd[1, 2]*pole[1, 5]*SpinorChain[angleSpinor[6], Mom[1],
     squareSpinor[3]]*square[2, 4]*square[3, 4] - pole[1, 4]*pole[1, 5]*
    SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*square[2, 4]*square[3, 4] -
   2*MomProd[1, 2]*pole[1, 6]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*square[2, 4]*
    square[3, 4] - pole[1, 4]*pole[1, 6]*SpinorChain[angleSpinor[6], Mom[1], squareSpinor[3]]*
    square[2, 4]*square[3, 4] - mass^2*angle[5, 6]*SpinorChain[squareSpinor[3], Mom[1], Mom[2],
     squareSpinor[4]]*square[2, 4]*square[3, 5] - mass^3*angle[5, 6]*
    SpinorChain[angleSpinor[2], Mom[1], squareSpinor[4]]*square[3, 4]*square[3, 5] +
   2*mass^2*angle[5, 6]*MomProd[1, 2]*square[2, 4]*square[3, 4]*square[3, 5] +
   mass^2*angle[5, 6]*pole[1, 4]*square[2, 4]*square[3, 4]*square[3, 5]))/
 (pole[1, 3]*pole[1, 4]*pole[1, 5]*pole[1, 6]*pole[2, 4]*pole[1, 5, 6])
};
  {rules, charge^4 Total[terms]}
];

(* Inactive equalities keep the input and output visible. These are ordinary
   identities and exact propagator definitions, not internal cut conditions. *)
QEDSixPointPPMMPhysicalCurrentIdentities[model_] := Module[
  {example, mass, a, s, pole, eq, p = 1, q = 2},
  example = QEDSixPointPPMMPhysicalCurrentExample[model];
  If[FailureQ[example], Return[example]];
  mass = Mass[1] /. First[example];
  a[i_] := Spinor[If[i <= 2, "Spin", "Helicity"], "Angle", i];
  s[i_] := Spinor[If[i <= 2, "Spin", "Helicity"], "Square", i];
  pole[legs__Integer] := PropDen[Mom[Multiparticle @@ Sort[{legs}]], mass];
  eq[left_, right_] := Inactive[Equal][left, right];
  {eq[SpinorChain[s[3], Mom[1], a[3]], pole[1, 3]],
   eq[SpinorChain[a[5], Mom[1], s[5]], pole[1, 5]],
   eq[SpinorChain[a[1], Mom[p], Mom[q], a[2]] +
       SpinorChain[a[1], Mom[q], Mom[p], a[2]],
     2 MomProd[p, q] SpinorChain[a[1], a[2]]],
   eq[SpinorChain[a[1], Mom[1], Mom[1], a[2]],
     mass^2 SpinorChain[a[1], a[2]]],
   eq[SpinorChain[a[1], Mom[3], Mom[4], a[2]],
     SpinorChain[a[1], a[3]] SpinorChain[s[3], s[4]] SpinorChain[a[4], a[2]]],
   eq[pole[1, 3, 5], pole[1, 3] + pole[1, 5] + 2 MomProd[3, 5]]}
];

(* Fixed six-point + + - - example in a common-reference photon gauge.
   Six two-emission blocks share their central propagators. The regular
   remainder comes from the single nonzero polarization pair contraction.
   This worked expression is independent of the construction algorithm. *)
QEDSixPointPPMMSpuriousExample[model_] := Module[
  {mass = ModelParameter[model, "Me"], charge = ModelParameter[model, "EE"],
   spin, angle, square, pole, aRef, sRef, refDen, sandwich, x, contact, emission,
   pair, contraction, split, regular, amplitude},
  spin[kind_, leg_] := Spinor[If[leg <= 2, "Spin", "Helicity"], kind, leg];
  angle[i_, j_] := If[i === j, 0, SpinorChain[spin["Angle", i], spin["Angle", j]]];
  square[i_, j_] := If[i === j, 0, SpinorChain[spin["Square", i], spin["Square", j]]];
  pole[legs__Integer] := PropDen[Mom[Multiparticle @@ Sort[{legs}]], mass];
  aRef = <|3 -> 5, 4 -> 5, 5 -> 5, 6 -> 6|>;
  sRef = <|3 -> 3, 4 -> 4, 5 -> 3, 6 -> 3|>;
  refDen = <|3 -> angle[3, 5], 4 -> angle[4, 5], 5 -> square[3, 5], 6 -> square[3, 6]|>;
  sandwich[i_, p_Integer, j_] := If[p <= 2,
    SpinorChain[spin["Square", i], Mom[p], spin["Angle", j]], square[i, p] angle[p, j]];
  x[i_, momenta__Integer] := Total[sandwich[sRef[i], #, aRef[i]] & /@ {momenta}]/refDen[i];
  contact[i_, j_] := -angle[aRef[i], aRef[j]] square[sRef[i], sRef[j]]/(refDen[i] refDen[j]);
  emission[p_, i_] := {x[i, p], -Boole[i === 5] angle[5, p], -Boole[i === 6] angle[6, p]};
  pair[p_, i_, j_] := Module[{center = pole[p, i, j],
     den = pole[p, i] pole[p, j], chi, scalar, si, sj},
    Which[
      j <= 4,
      {(x[i, p] x[j, p] center - mass^2 square[i, j]^2)/den, 0, 0},
      i <= 4,
      chi = sandwich[i, p, j];
      scalar = (x[i, p] x[j, p] center + chi^2)/den;
      sj = -angle[j, p] (x[i, p] center + square[i, j] chi)/den;
      {scalar, If[j === 5, sj, 0], If[j === 6, sj, 0]},
      True,
      scalar = (x[i, p] x[j, p] center - mass^2 angle[i, j]^2)/den;
      si = (-angle[i, p] x[j, p] center + mass angle[i, j]^2 square[i, p])/den;
      sj = (-angle[j, p] x[i, p] center + mass angle[i, j]^2 square[j, p])/den;
      {scalar, si, sj}]];
  contraction[left_List, right_List] := Sum[
    left[[i]] right[[j]] angle[{1, 5, 6}[[i]], {2, 5, 6}[[j]]], {i, 3}, {j, 3}];
  split[sub_List] := contraction[pair[1, Sequence @@ sub],
    pair[2, Sequence @@ Complement[{3, 4, 5, 6}, sub]]]/pole[1, Sequence @@ sub];
  regular = contact[4, 6] (contraction[emission[1, 3], emission[2, 5]]/(pole[1, 3] pole[2, 5]) +
    contraction[emission[1, 5], emission[2, 3]]/(pole[1, 5] pole[2, 3]));
  amplitude = -4 charge^4/mass (Total[split /@ Subsets[{3, 4, 5, 6}, {2}]] + regular);
  {Thread[Mass /@ Range[6] -> {mass, mass, 0, 0, 0, 0}], amplitude}
];

(* Worked six-point e,E,E,e examples. For each signed fermion matching,
   partition the two photons between its currents. Contract each pair of
   currents across the virtual photon. Adding the two orders on the same
   line uses external Dirac and Schouten identities; no cut is imposed. *)
QEDSixPointFourFermionSpuriousExample[model_, helicities : ("PP" | "PM" | "MM")] := Module[
  {mass = ModelParameter[model, "Me"], charge = ModelParameter[model, "EE"],
   photons = {5, 6}, spin, angle, square, sandwich, denominator, electronPole,
   contraction, exchange, amplitude,
   sameSoft, sameSpin, sameTriple, sameScalar, sameShift, sameCurrent,
   mixedSoft, mixedShift, mixedRemainder, mixedCurrent, mixedProduct},
  spin[kind_, leg_] := Spinor[If[leg <= 4, "Spin", "Helicity"], kind, leg];
  angle[i_, j_] := If[i === j, 0, SpinorChain[spin["Angle", i], spin["Angle", j]]];
  square[i_, j_] := If[i === j, 0, SpinorChain[spin["Square", i], spin["Square", j]]];
  sandwich[chirality_, i_, momenta_List, opposite_, j_] :=
    SpinorChain[spin[chirality, i], Sequence @@ (Mom /@ momenta), spin[opposite, j]];
  denominator[m_, legs_List] := PropDen[Mom[Multiparticle @@ Sort[legs]], m];
  electronPole[leg_, emitted__Integer] := denominator[mass, {leg, emitted}];

  (* A current entry {coefficient, angle leg, square leg} represents an
     outer product. Contract two such entries by closing both brackets. *)
  contraction[left_List, right_List] := Total[Flatten[Table[
    l[[1]] r[[1]] angle[l[[2]], r[[2]]] square[l[[3]], r[[3]]],
    {l, left}, {r, right}], 1]];

  (* Equal helicities: cancel the leg-5 reference bracket in every scalar
     current. Retain the leg-6 reciprocal denominator for same-line emissions. *)
  sameSoft[a_, b_, i_, split_] := If[i === 5 || split,
    -sandwich["Square", i, {b, a}, "Square", i]/(electronPole[a, i] electronPole[b, i]),
    (sandwich["Square", i, {a}, "Angle", 5]/electronPole[a, i] -
      sandwich["Square", i, {b}, "Angle", 5]/electronPole[b, i])/angle[i, 5]];
  sameSpin[p_, i_] := square[i, p]/electronPole[p, i];
  sameTriple[p_, i_, j_] := electronPole[p, i, j] electronPole[p, i] electronPole[p, j];
  sameScalar[a_, b_, {}] := 1;
  sameScalar[a_, b_, {i_}] := sameSoft[a, b, i, True];
  sameScalar[a_, b_, {i_, j_}] := sameSoft[a, b, i, False] sameSoft[a, b, j, False] -
    mass^2 square[i, j]^2 (1/sameTriple[a, i, j] + 1/sameTriple[b, i, j]);
  sameShift[p_, q_, {}] := {};
  sameShift[p_, q_, {i_}] := {{sameSpin[p, i], i}};
  sameShift[p_, q_, {i_, j_}] := {
    {sameSpin[p, i] sameSoft[p, q, j, False] + mass square[i, j]^2 angle[i, p]/sameTriple[p, i, j], i},
    {sameSpin[p, j] sameSoft[p, q, i, False] + mass square[j, i]^2 angle[j, p]/sameTriple[p, j, i], j}};
  sameCurrent[a_, b_, emitted_List] := Join[
    {{sameScalar[a, b, emitted], a, b}, {sameScalar[a, b, emitted], b, a}},
    ({#[[1]], b, #[[2]]} & /@ sameShift[a, b, emitted]),
    ({If[Length[emitted] === 1, -#[[1]], #[[1]]], a, #[[2]]} & /@ sameShift[b, a, emitted])];

  (* Mixed helicities: cancel the reference brackets in the scalar
     current product by Schouten. Keep them in the spin-shift terms. *)
  mixedSoft[a_, b_, photon_, physical_] := If[physical,
    If[photon === 5, -sandwich["Square", 5, {b, a}, "Square", 5],
      sandwich["Angle", 6, {b, a}, "Angle", 6]]/
      (electronPole[a, photon] electronPole[b, photon]),
    (sandwich["Square", 5, {a}, "Angle", 6]/electronPole[a, photon] -
      sandwich["Square", 5, {b}, "Angle", 6]/electronPole[b, photon])/
      If[photon === 5, angle[5, 6], square[5, 6]]];
  mixedShift[p_, 5, sign_] := sign square[5, p]/electronPole[p, 5];
  mixedShift[p_, 6, sign_] := -sign angle[6, p]/electronPole[p, 6];
  mixedCurrent[a_, b_, shifted_List] := Function[{i, j},
    {Times @@ (With[{p = If[# === 5, j, i]}, mixedShift[p, #, If[p === a, 1, -1]]] & /@ shifted),
     If[MemberQ[shifted, 6], 6, i], If[MemberQ[shifted, 5], 5, j]}] @@@ {{a, b}, {b, a}};
  mixedRemainder[a_, b_, c_, d_] := Total[Table[With[
    {chi = sandwich["Square", 5, {i}, "Angle", 6], j = If[i === a, b, a]},
    chi contraction[{{chi, i, j}, {chi, j, i}, {-square[5, 6] angle[6, i], 6, j},
        {angle[5, 6] square[5, i], j, 5}}, mixedCurrent[c, d, {}]]/
      (electronPole[i, 5] electronPole[i, 6] electronPole[i, 5, 6])], {i, {a, b}}]];
  mixedProduct[a_, b_, c_, d_, left_List, right_List] := Module[{physical, soft, leading},
    soft[x_, y_, unshifted_, cancel_] := Times @@ (mixedSoft[x, y, #, cancel] & /@ unshifted);
    leading = Total[Flatten[Table[
      physical = leftShift === {} && rightShift === {};
      soft[a, b, Complement[left, leftShift], physical] soft[c, d, Complement[right, rightShift], physical]
        contraction[mixedCurrent[a, b, leftShift], mixedCurrent[c, d, rightShift]],
      {leftShift, Subsets[left]}, {rightShift, Subsets[right]}], 1]];
    leading + Which[left === photons, mixedRemainder[a, b, c, d],
      right === photons, mixedRemainder[c, d, a, b], True, 0]];

  exchange[a_, b_, c_, d_] := Total[Function[left,
    With[{right = Complement[photons, left]},
      If[helicities === "PM", mixedProduct[a, b, c, d, left, right],
        contraction[sameCurrent[a, b, left], sameCurrent[c, d, right]]]/
        denominator[0, Join[{a, b}, left]]]] /@ Subsets[photons]];
  amplitude = If[helicities === "PM", 4, -4] charge^4
    (exchange[1, 2, 4, 3] - exchange[1, 3, 4, 2]);
  If[helicities === "MM", amplitude = amplitude /.
    Spinor[kind_, chirality : ("Angle" | "Square"), leg_] :>
      Spinor[kind, If[chirality === "Angle", "Square", "Angle"], leg]];
  {Thread[(Mass /@ Range[6]) -> {mass, mass, mass, mass, 0, 0}], amplitude}
];

(* ::Section::Closed:: *)
(* All-positive massless-reference formula. Prediction example, not a reference calculation. *)

(* The arbitrary-photon formula is described in tests/development/SIMPLIFICATION.md.
   This helper uses the production subset recurrence rather than maintaining
   a second implementation. Its result has not been validated by this call;
   compare it with independent diagrams and CheckAmplitudes/ValidateMadGraph.
   All momenta are ingoing, with e,E in slots 1,2 and photons after them. *)
QEDAllPositiveSpuriousExample[model_, photonCount_Integer] := Module[
  {mass, vertex, vertexFactor, proposal, photons, spinTarget},
  If[!MatchQ[model, {_List, _List, _List, _List}] || photonCount < 2,
    Return[Failure["QEDAllPositiveSpuriousArguments", <|"MessageTemplate" ->
      "Supply a constructive QED model and at least two positive-helicity photons."|>]]];
  mass = ModelParameter[model, "Me"];
  If[!MatchQ[mass, _ModelParameter],
    Return[Failure["QEDExampleParameters", <|"MessageTemplate" ->
      "The QED example requires a declared electron mass parameter Me."|>]]];
  spinTarget = xFactor[1, 2] SpinorChain[
    Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]];
  vertex = LookupAmplitude[{{"e", 1}, {"E", 2}, {"A.+", 3}}, model,
    False, Table[Unique["qedPositiveSpin"], {4}]];
  vertexFactor = Cancel[vertex/spinTarget];
  If[vertexFactor === 0 || !TrueQ[ConstructiveDiagrams`Private`rspScalarQ[vertexFactor]] ||
      !TrueQ[ConstructiveDiagrams`Private`residueFiniteQ[vertexFactor]] ||
      TrueQ[ConstructiveDiagrams`Private`residueUnmappedQ[vertexFactor]],
    Return[Failure["QEDExampleVertex", <|"MessageTemplate" ->
      "The model must contain the minimal positive-helicity e,E,A.+ cubic vertex."|>]]];
  photons = Range[3, photonCount + 2];
  proposal = ConstructiveDiagrams`Private`constructSameChiralSpuriousSubsetCurrent[
    {1, 2}, photons, 1, mass, ConstantArray[vertexFactor, photonCount]];
  If[!AssociationQ[proposal] || !KeyExistsQ[proposal, "Expression"],
    Return[Failure["QEDAllPositiveSpuriousAllowance", <|"MessageTemplate" ->
      "The example exceeds the supported bounded scalar-subset allowance.",
      "PhotonCount" -> photonCount|>]]];
  {Thread[Mass /@ Range[photonCount + 2] -> Join[{mass, mass}, ConstantArray[0, photonCount]]],
    proposal["Expression"]}
];
QEDAllPositiveSpuriousExample[___] := Failure["QEDAllPositiveSpuriousArguments", <|
  "MessageTemplate" -> "Supply a constructive QED model and an integer photon count of at least two."|>];

<|"Amplitudes" -> <|"H" -> prH, "S2" -> prS2, "OldPermutation" -> prOldPermutation|>,
  "FactorizationTargets" -> prElectronTargets, "ReportFunction" -> q5ResidueReport|>
