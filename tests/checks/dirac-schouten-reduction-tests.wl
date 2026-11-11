(* General endpoint identities are tested on independent external mass shells,
   without momentum conservation or any internal-line shell assumption. *)

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


dsRoot = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{dsRoot, "source", "HEPCAT.wl"}]];
$HistoryLength = 0; dsChecks = 0; dsFailures = {};
SetAttributes[dsCheck, HoldRest];
dsCheck[name_, assertion_] := (dsChecks++; If[!TrueQ[assertion],
  AppendTo[dsFailures, name]; Print["FAIL: ", name]]);
dsRules = {Mass[1] -> dsMa, Mass[2] -> dsMb, Mass[3] -> dsMr,
  Mass[4] -> dsMs, Mass[5] -> 0};
dsNumericMasses = <|1 -> 2, 2 -> 3, 3 -> 5, 4 -> 7, 5 -> 0|>;
dsParameterRules = {dsMa -> 2, dsMb -> 3, dsMr -> 5, dsMs -> 7};
dsEpsilon = {{0, 1}, {-1, 0}};
dsAdjugate[matrix_] := {{matrix[[2, 2]], -matrix[[1, 2]]},
  {-matrix[[2, 1]], matrix[[1, 1]]}};
dsChart[dsSample_, dsMasses_: dsNumericMasses] := Module[
  {momenta = <||>, angles = <||>, squares = <||>, angleBasis = <||>,
   squareBasis = <||>, aa, bb, cc, mass, basis, conjugate, spin},
  Do[
    {aa, bb, cc} = {dsSample + dsLeg + 1, 2 dsLeg + 3, dsSample dsLeg + 2};
    mass = dsMasses[dsLeg];
    AssociateTo[momenta, dsLeg -> {{aa, bb}, {cc, (mass^2 + bb cc)/aa}}];
    If[mass === 0,
      AssociateTo[angles, dsLeg -> momenta[dsLeg][[All, 1]]];
      AssociateTo[squares, dsLeg -> {1, bb/aa}],
      (* Distinct determinant-one spin frames keep indexed components generic;
         equal spin labels on different legs are not accidentally collinear. *)
      basis = {{mass, mass dsLeg}, {dsSample + dsLeg,
        1 + dsLeg (dsSample + dsLeg)}};
      conjugate = dsEpsilon.dsAdjugate[momenta[dsLeg]].basis/mass;
      spin = {1, (dsSample + 2 dsLeg)/(dsLeg + 1)};
      AssociateTo[angleBasis, dsLeg -> basis];
      AssociateTo[squareBasis, dsLeg -> conjugate];
      AssociateTo[angles, dsLeg -> basis.spin];
      AssociateTo[squares, dsLeg -> conjugate.spin]],
    {dsLeg, Range[5]}];
  <|"Momenta" -> momenta, "Masses" -> dsMasses,
    "Angle" -> angles, "Square" -> squares,
    "AngleBasis" -> angleBasis, "SquareBasis" -> squareBasis,
    "PoleVariable" -> dsUnusedPole, "ReferenceSeed" -> 31,
    "AllowCutSpinors" -> False|>
];
dsSpinor[kind_, leg_, mode_: "Coherent"] := If[leg === 5,
  Spinor["Helicity", kind, leg], Switch[mode,
    "Coherent", Spinor["Spin", kind, leg],
    "Upper", Spinor["Spin", "Upper", kind, leg, If[OddQ[leg], -1/2, 1/2]],
    "Lower", Spinor["Spin", "Lower", kind, leg, If[OddQ[leg], 1/2, -1/2]],
    "Mixed", Spinor["Spin", If[OddQ[leg], "Lower", "Upper"], kind, leg, 1/2]]];
dsFlip[spinor_] := ConstructiveDiagrams`Private`rspFlipSpinor[spinor];
dsValue[expression_, chart_, parameters_: dsParameterRules] :=
  ConstructiveDiagrams`Private`residueEvaluateChart[expression, chart, parameters];
dsZero[expression_] := TrueQ[Cancel[Together[expression]] === 0];
dsReduce[expression_, rules_: dsRules] :=
  ConstructiveDiagrams`Private`rspDiracSchoutenReduction[expression, rules, 5];
dsRank[expression_] := {ConstructiveDiagrams`Private`expressionComplexity[expression], LeafCount[expression]};
dsBetter[first_, second_] := first =!= second && OrderedQ[{first, second}];
dsReplayQ[record_, chart_] := record["InternalOnShellConditions"] === {} &&
  dsZero[dsValue[record["Subtract"] - record["Insert"], chart]] &&
  dsZero[dsValue[record["Before"] - record["After"], chart]] &&
  AllTrue[record["LiteralFactorWitnesses"],
    TrueQ[# ["ExactLiteralReconstruction"]] &&
      TrueQ[Expand[# ["Term"] - # ["Factor"] # ["Cofactor"]] === 0] &];

dsMakeTail[kind_, mode_, word_List, position_Integer] := Module[
  {a = dsSpinor[kind, 1, mode], b = dsSpinor[kind, 2, mode],
   r = dsSpinor[kind, 3, mode], s, prefix, suffix, tail, source, massTerm, dots},
  s = dsSpinor[If[EvenQ[Length[word]], kind,
    If[kind === "Angle", "Square", "Angle"]], 4, mode];
  prefix = Take[word, position - 1]; suffix = Drop[word, position]; tail = Join[prefix, suffix];
  source = SpinorChain[a, b] (SpinorChain @@ Join[{r}, word, {s}]);
  massTerm = (-1)^Length[prefix] dsMa SpinorChain[r, b] *
    (SpinorChain @@ Join[{dsFlip[a]}, tail, {s}]);
  dots = Total[Table[2 (-1)^(Length[prefix] - dsPivot) MomProd[prefix[[dsPivot, 1]], 1] *
    SpinorChain[a, b] (SpinorChain @@ Join[{r}, Delete[prefix, dsPivot], suffix, {s}]),
    {dsPivot, Length[prefix]}]];
  <|"Input" -> source - massTerm - dots,
    "Expected" -> (-1)^Length[prefix] SpinorChain[a, r] *
      (SpinorChain @@ Join[{b, Mom[1]}, tail, {s}])|>
];
dsMakeDouble[kind_, mode_] := Module[
  {a = dsSpinor[kind, 1, mode], b = dsSpinor[kind, 2, mode], r = dsSpinor[kind, 3, mode]},
  <|"Input" -> 2 MomProd[1, 2] SpinorChain[a, r] SpinorChain[b, r] -
      dsMa SpinorChain[b, r] SpinorChain[r, Mom[2], dsFlip[a]] -
      dsMb SpinorChain[a, r] SpinorChain[r, Mom[1], dsFlip[b]],
    "Expected" -> SpinorChain[a, b] SpinorChain[r, Mom[1], Mom[2], r]|>
];

Do[
  dsFrame = dsChart[dsSample];
  dsCheck[{"external shells are exact but the independent momenta are not conserved", dsSample},
    And @@ Table[Det[dsFrame["Momenta"][dsLeg]] === dsNumericMasses[dsLeg]^2, {dsLeg, 5}] &&
      !dsZero[Total[Values[dsFrame["Momenta"]]] . {1, 1} . {1, 1}]],
  {dsSample, {1, 3}}];
Do[
  dsTail = dsMakeTail[dsKind, dsMode, {Mom[1]}, 1];
  dsDouble = dsMakeDouble[dsKind, dsMode];
  Do[
    dsReport = dsReduce[dsFixture["Input"]];
    dsCheck[{"production reduction improves by an ordinary identity", dsKind, dsMode, dsMethod},
      dsReport["Route"] =!= {} && dsReport["InternalOnShellConditions"] === {} &&
        !TrueQ[dsReport["TimedOut"]] &&
        dsBetter[dsRank[dsReport["Amplitude"]], dsRank[dsFixture["Input"]]]];
    dsCheck[{"the intended endpoint operation is recorded", dsKind, dsMode, dsMethod},
      MemberQ[Lookup[dsReport["Route"], "Identity"], dsMethod]];
    Do[
      dsFrame = dsChart[dsSample];
      dsCheck[{"full result and exact replay preserve independent unequal-mass kinematics",
          dsKind, dsMode, dsMethod, dsSample},
        dsZero[dsValue[dsReport["Amplitude"] - dsFixture["Input"], dsFrame]] &&
          dsZero[dsValue[dsReport["Amplitude"] - dsFixture["Expected"], dsFrame]] &&
          AllTrue[dsReport["Route"], dsReplayQ[#, dsFrame] &]],
      {dsSample, {1, 3}}],
    {dsFixture, {dsTail, dsDouble}},
    {dsMethod, {If[dsFixture === dsTail, "TailDiracSchouten", "DoubleEndpointDiracSchouten"]}}],
  {dsKind, {"Angle", "Square"}}, {dsMode, {"Coherent", "Upper", "Lower", "Mixed"}}];

(* Positions two and three need the complete Clifford remainder. Position
   one is already a leading endpoint, so ordinary normalization can leave no
   strictly improving recorded edit when p.q costs two momentum insertions.
   Require its exact value and nonworsened rank rather than an invented route. *)
Do[
  dsWord = Insert[{Mom[2], Mom[5]}, Mom[1], dsPosition];
  dsFixture = dsMakeTail[dsKind, "Coherent", dsWord, dsPosition];
  dsReport = dsReduce[dsFixture["Input"]];
  dsCheck[{If[dsPosition === 1,
      "a leading pivot preserves its exact value without requiring a redundant edit",
      "an interior pivot keeps every anticommutation term"], dsKind, dsPosition},
    If[dsPosition === 1,
      !dsBetter[dsRank[dsFixture["Input"]], dsRank[dsReport["Amplitude"]]],
      dsReport["Route"] =!= {}] &&
      dsZero[dsValue[dsReport["Amplitude"] - dsFixture["Expected"], dsChart[2]]] &&
      AllTrue[dsReport["Route"], dsReplayQ[#, dsChart[2]] &]],
  {dsKind, {"Angle", "Square"}}, {dsPosition, 3}];

dsTail = dsMakeTail["Angle", "Coherent", {Mom[1]}, 1];
dsDouble = dsMakeDouble["Angle", "Coherent"];
dsCore = dsTail["Input"]; dsCoreReport = dsReduce[dsCore];
dsSymbolicChart = dsChart[2, <|1 -> dsMa, 2 -> dsMb, 3 -> dsMr, 4 -> dsMs, 5 -> 0|>];
dsCheck["both endpoint identities hold with independent symbolic unequal masses",
  dsZero[dsValue[dsTail["Input"] - dsTail["Expected"], dsSymbolicChart, {}]] &&
    dsZero[dsValue[dsDouble["Input"] - dsDouble["Expected"], dsSymbolicChart, {}]]];
dsCheck["the tail insertion is a legal existing generalized Schouten rewrite followed by external Dirac",
  With[{dsRecord = First[dsCoreReport["Route"]]},
    AnyTrue[SchoutenCandidates[dsRecord["Subtract"]], Function[dsEdit,
      ConstructiveDiagrams`Private`canonicalChains[
        (dsEdit["Insert"] /. dsChain_SpinorChain :> ReduceSpinorProducts[dsChain]) /. dsRules] ===
        ConstructiveDiagrams`Private`canonicalChains[dsRecord["Insert"]]]]]];
dsRepeat = dsReduce[dsCore];
dsCheck["completed output and identity choices are deterministic",
  dsRepeat["Amplitude"] === dsCoreReport["Amplitude"] &&
    dsRepeat["Route"] === dsCoreReport["Route"]];
dsPower = dsReduce[dsCore^2];
dsBracket = SpinorChain[dsSpinor["Angle", 1], dsSpinor["Angle", 2]];
dsSpectator = dsReduce[Expand[dsBracket^2 dsCore]];
dsCheck["positive powers of a reducible sum preserve the full product",
  dsPower["Route"] =!= {} &&
    dsZero[dsValue[dsPower["Amplitude"] - dsCoreReport["Amplitude"]^2, dsChart[2]]]];
dsCheck["positive chain powers remove exactly one literal factor per witness",
  dsSpectator["Route"] =!= {} &&
    dsZero[dsValue[dsSpectator["Amplitude"] - dsBracket^2 dsCoreReport["Amplitude"], dsChart[2]]] &&
    AllTrue[dsSpectator["Route"], dsReplayQ[#, dsChart[2]] &]];

dsPole = PropDen[Mom[Multiparticle[1, 3]], dsInternalMass];
dsInverse = SpinorChain[dsSpinor["Angle", 3], dsSpinor["Angle", 4]];
dsRational = Expand[dsCore/(dsPole dsInverse)];
dsProtected = dsReduce[dsRational, Append[dsRules, Mass[Multiparticle[1, 3]] -> dsInternalMass]];
dsCheck["physical and inverse-spinor denominator factors are preserved literally",
  dsProtected["Route"] =!= {} &&
    DeleteDuplicates[Cases[dsProtected["Amplitude"], _PropDen, Infinity]] === {dsPole} &&
    !FreeQ[dsProtected["Amplitude"], dsInverse^-1] &&
    TrueQ[Expand[dsProtected["Amplitude"] dsPole dsInverse - dsCoreReport["Amplitude"]] === 0]];
dsCheck["internal mass declarations do not enter the ordinary reduction scope",
  dsProtected["ExternalMassRules"] === dsRules &&
    AllTrue[dsProtected["Route"], # ["ExternalMassRules"] === dsRules &&
      # ["InternalOnShellConditions"] === {} &]];
dsCheck["inverse and propagator temporary tokens never escape into replay records",
  FreeQ[dsProtected, dsSymbol_Symbol /;
    StringStartsQ[SymbolName[Unevaluated[dsSymbol]], "diracSchoutenInverse"] ||
    StringStartsQ[SymbolName[Unevaluated[dsSymbol]], "propagator"]] &&
    AllTrue[dsProtected["Route"], dsReplayQ[#, dsChart[2]] &]];
dsNumeric = dsReduce[dsDouble["Input"] /. dsParameterRules, dsRules /. dsParameterRules];
dsCompound = dsReduce[dsDouble["Input"] /. dsMb -> (dsMx + dsMy),
  dsRules /. dsMb -> (dsMx + dsMy)];
dsCheck["numeric masses need not remain a literal positive Times factor",
  dsNumeric["Route"] =!= {} &&
    dsZero[dsValue[dsNumeric["Amplitude"] - (dsDouble["Input"] /. dsParameterRules), dsChart[2]]]];
dsCheck["a factored compound external mass is divided only with exact scalar reconstruction",
  dsCompound["Route"] =!= {} &&
    FreeQ[dsCompound["Amplitude"], Power[dsMx + dsMy, _Integer?Negative]] &&
    dsZero[dsValue[(dsCompound["Amplitude"] /. {dsMx -> 1, dsMy -> 2}) -
      dsDouble["Input"], dsChart[2]]]];
dsHuge = (dsX + dsY + dsZ)^80 + dsCore;
dsHugeResult = dsReduce[dsHuge];
dsCheck["prospective expansion is rejected before a huge sum is distributed",
  dsHugeResult["Amplitude"] === dsHuge && dsHugeResult["Route"] === {} &&
    !TrueQ[dsHugeResult["TimedOut"]]];
dsExpired = ConstructiveDiagrams`Private`rspDiracSchoutenReduction[dsCore, dsRules, 0];
dsCheck["an exhausted allowance preserves the valid original expression",
  dsExpired["Amplitude"] === dsCore && dsExpired["Route"] === {}];

(* Conservation is a separate preparation operation. Base reductions still
   work when it is disabled, and undeclared legs cannot license conservation. *)
dsModelFixture = dsMakeTail["Angle", "Coherent", {Mom[5], Mom[1]}, 2];
dsWithoutMC = ConstructiveDiagrams`Private`preparationDiracSchoutenCompaction[
  dsModelFixture["Input"], dsRules, False, 5];
dsMissingMass = ConstructiveDiagrams`Private`preparationDiracSchoutenCompaction[
  dsModelFixture["Input"], DeleteCases[dsRules, HoldPattern[Mass[5] -> _]], True, 5];
dsCheck["explicitly disabled conservation still permits ordinary endpoint identities",
  dsWithoutMC["Route"] =!= {} && !TrueQ[dsWithoutMC["MomentumConservationAllowed"]] &&
    AllTrue[dsWithoutMC["Route"], # ["Operation"] === "TailAndDoubleEndpointSchouten" &&
      !TrueQ[# ["MomentumConservation"]] &] &&
    dsZero[dsValue[dsWithoutMC["Amplitude"] - dsModelFixture["Input"], dsChart[2]]]];
dsCheck["a momentum or physical-spinor leg missing from metadata cannot license conservation",
  !TrueQ[dsMissingMass["MomentumConservationAllowed"]] &&
    AllTrue[dsMissingMass["Route"], !TrueQ[# ["MomentumConservation"]] &] &&
    dsZero[dsValue[dsMissingMass["Amplitude"] - dsModelFixture["Input"], dsChart[2]]]];
dsCheck["the orchestration keeps matching expression rank and ordinary replay checkpoints",
  dsWithoutMC["Rank"] === ConstructiveDiagrams`Private`preparationRank[dsWithoutMC["Amplitude"]] &&
    dsWithoutMC["InternalOnShellConditions"] === {} &&
    AllTrue[Flatten[Lookup[dsWithoutMC["Route"], "IdentityRoute"], 1],
      dsReplayQ[#, dsChart[2]] &]];

(* A complete sum of massless outer products can be rejoined before applying
   declared conservation. The exact test chart has no internal cut. *)
dsMCRules = {Mass[1] -> dsMa, Mass[2] -> dsMb, Mass[3] -> 0, Mass[4] -> 0, Mass[5] -> 0};
dsMCSpinor[kind_, leg_] := Spinor[If[leg <= 2, "Spin", "Helicity"], kind, leg];
dsMCChart = ConstructiveDiagrams`Private`completionHESymbolicExternalChart[
  dsMCRules /. dsParameterRules];
dsMCVariables = Join[dsMCChart["ExternalIdentityCoordinates"], dsMCChart["SpinVariables"]];
dsMCChart = dsMCChart /. Thread[dsMCVariables -> Range[2, Length[dsMCVariables] + 1]];
dsCheck["the conservation fixture has exact external shells and conserved momenta without an internal cut",
  And @@ Flatten[Map[dsZero, Total[Values[dsMCChart["Momenta"]]], {2}]] &&
    !TrueQ[Lookup[dsMCChart, "AllowCutSpinors", False]]];
Do[
  dsOppositeKind = If[dsKind === "Angle", "Square", "Angle"];
  dsMCSpectator = SpinorChain[dsMCSpinor[dsKind, 3], Mom[1], Mom[2], dsMCSpinor[dsKind, 4]];
  dsMCInput = dsMCSpectator Total[SpinorChain[dsMCSpinor[dsKind, 1], dsMCSpinor[dsKind, #]] *
    SpinorChain[dsMCSpinor[dsOppositeKind, #], dsMCSpinor[dsOppositeKind, 2]] & /@ {3, 4, 5}];
  dsPublished = None;
  dsMCReport = ConstructiveDiagrams`Private`preparationDiracSchoutenCompaction[
    dsMCInput/dsPole, dsMCRules, True, 5, Function[dsSnapshot, dsPublished = dsSnapshot]];
  dsCheck[{"complete massless sums use a conserved ordinary sewing identity", dsKind},
    TrueQ[dsMCReport["MomentumConservationAllowed"]] &&
      MemberQ[Lookup[dsMCReport["Route"], "Operation"], "ConservedMasslessStitch"] &&
      dsZero[dsValue[(dsMCReport["Amplitude"] dsPole) - dsMCInput, dsMCChart]] &&
      DeleteDuplicates[Cases[dsMCReport["Amplitude"], _PropDen, Infinity]] === {dsPole} &&
      dsMCReport["InternalOnShellConditions"] === {}];
  dsCheck[{"published conservation checkpoints contain the actual expression and replay", dsKind},
    AssociationQ[dsPublished] && dsPublished["Amplitude"] === dsMCReport["Amplitude"] &&
      dsPublished["Rank"] === dsMCReport["Rank"] && dsPublished["Route"] === dsMCReport["Route"]];
  dsMCDisabled = ConstructiveDiagrams`Private`preparationDiracSchoutenCompaction[
    dsMCInput, dsMCRules, False, 5];
  dsMCIncomplete = ConstructiveDiagrams`Private`preparationDiracSchoutenCompaction[
    dsMCInput, DeleteCases[dsMCRules, HoldPattern[Mass[5] -> _]], True, 5];
  dsMCScalarUndeclared = ConstructiveDiagrams`Private`preparationDiracSchoutenCompaction[
    dsMCInput MomProd[6, 7], dsMCRules, True, 5];
  dsCheck[{"scalar-product spectators also require declared external legs", dsKind},
    !TrueQ[dsMCScalarUndeclared["MomentumConservationAllowed"]] &&
      dsMCScalarUndeclared["Amplitude"] === dsMCInput MomProd[6, 7] &&
      dsMCScalarUndeclared["Route"] === {}];
  dsCheck[{"the same positive sewing example is forbidden without a complete conservation scope", dsKind},
    dsMCDisabled["Amplitude"] === dsMCInput && dsMCDisabled["Route"] === {} &&
      dsMCIncomplete["Amplitude"] === dsMCInput && dsMCIncomplete["Route"] === {} &&
      !TrueQ[dsMCDisabled["MomentumConservationAllowed"]] &&
      !TrueQ[dsMCIncomplete["MomentumConservationAllowed"]]],
  {dsKind, {"Angle", "Square"}}];
Do[
  dsOppositeKind = If[dsKind === "Angle", "Square", "Angle"];
  dsGordonInput = dsMa SpinorChain[dsMCSpinor[dsKind, 2], dsMCSpinor[dsKind, 3]] *
      SpinorChain[dsMCSpinor[dsOppositeKind, 1], dsMCSpinor[dsOppositeKind, 4]] +
    dsMb SpinorChain[dsMCSpinor[dsKind, 1], dsMCSpinor[dsKind, 3]] *
      SpinorChain[dsMCSpinor[dsOppositeKind, 2], dsMCSpinor[dsOppositeKind, 4]] +
    SpinorChain[dsMCSpinor[dsKind, 1], dsMCSpinor[dsKind, 2]] *
      SpinorChain[dsMCSpinor[dsKind, 3], Mom[1], dsMCSpinor[dsOppositeKind, 4]] +
    SpinorChain[dsMCSpinor[dsKind, 1], dsMCSpinor[dsKind, 3]] *
      SpinorChain[dsMCSpinor[dsKind, 2], dsMCSpinor[dsKind, 5]] *
      SpinorChain[dsMCSpinor[dsOppositeKind, 5], dsMCSpinor[dsOppositeKind, 4]];
  dsGordon = ConstructiveDiagrams`Private`preparationGordonConservationTrial[
    dsGordonInput/dsPole, dsMCRules, 5];
  dsCheck[{"conserved Gordon pairs expose a genuine unequal-mass cancellation", dsKind},
    dsGordon["History"] =!= {} &&
      ConstructiveDiagrams`Private`preparationBetterQ[dsGordon["Rank"],
        ConstructiveDiagrams`Private`preparationRank[dsGordonInput/dsPole]] &&
      dsZero[dsValue[dsGordon["Amplitude"] dsPole - dsGordonInput, dsMCChart]] &&
      DeleteDuplicates[Cases[dsGordon["Amplitude"], _PropDen, Infinity]] === {dsPole}];
  dsCheck[{"each accepted Gordon pair is independently valid on the external conservation chart", dsKind},
    AllTrue[dsGordon["History"],
      dsZero[dsValue[# ["Subtract"] - # ["Insert"], dsMCChart]] &]],
  {dsKind, {"Angle", "Square"}}];
Print[dsChecks, " Dirac-Schouten reduction checks; ", Length[dsFailures], " failures."];
If[dsFailures =!= {}, Print[dsFailures]; Exit[1]];
Print["HEPCAT_TEST_COMPLETED:dirac-schouten-reduction-tests.wl"];
Exit[0];
