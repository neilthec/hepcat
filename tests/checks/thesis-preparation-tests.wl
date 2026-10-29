(* Thesis Eq. (101): ordinary sandwich identities and diagram-local preparation. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0;
checks = 0; failures = {};
check[name_, value_] := (checks++;
  If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);

tpSpin[kind_, leg_] := Spinor["Helicity", kind, leg];
tpBracket[kind_, a_, b_] := SpinorChain[tpSpin[kind, a], tpSpin[kind, b]];
tpChain[kind_, a_, p_, q_, b_] :=
  SpinorChain[tpSpin[kind, a], Mom[p], Mom[q], tpSpin[kind, b]];
tpPair[kind_] := tpChain[kind, 3, 1, 2, 3] tpChain[kind, 4, 1, 3, 4];
tpIdentity[kind_] :=
  2 Mass[1]^2 MomProd[2, 3] tpBracket[kind, 3, 4]^2 +
  2 MomProd[1, 3] tpBracket[kind, 3, 4] tpChain[kind, 4, 1, 2, 3];

(* Exact two-by-two evaluation is independent of the symbolic reduction rules.
   Only leg 3 is required massless for the identity. Neither P nor Q is cut. *)
tpEvaluate[expression_, chart_] := Expand[
  expression /. chain_SpinorChain :>
      ConstructiveDiagrams`Private`residueChartChain[chain, chart] /.
    MomProd[a_, b_] :> With[{p = chart["Momenta"][a], q = chart["Momenta"][b]},
      (Det[p + q] - Det[p] - Det[q])/2] /.
    Power[Mass[label_Integer], exponent_Integer?EvenQ] /;
        KeyExistsQ[chart["Momenta"], label] :>
      Det[chart["Momenta"][label]]^(exponent/2)];
tpCharts = BlockRandom[SeedRandom[721093]; Table[
  tpP = RandomInteger[{-7, 7}, {2, 2}];
  tpQ = RandomInteger[{-7, 7}, {2, 2}];
  tpAngles = AssociationThread[{3, 4}, RandomInteger[{-7, 7}, {2, 2}]];
  tpSquares = AssociationThread[{3, 4}, RandomInteger[{-7, 7}, {2, 2}]];
  <|"Momenta" -> Join[<|1 -> tpP, 2 -> tpQ|>,
      AssociationMap[Outer[Times, tpAngles[#], tpSquares[#]] &, {3, 4}]],
    "Angle" -> tpAngles, "Square" -> tpSquares,
    "AngleBasis" -> <|4 -> Transpose[{tpAngles[4], {2, 3}}]|>,
    "SquareBasis" -> <|4 -> Transpose[{tpSquares[4], {3, 5}}]|>,
    "PoleVariable" -> tpUnusedPole, "ReferenceSeed" -> 1|>, {40}]];

Do[
  check[{"ordinary sandwich identity on an independent exact chart", tpKind, tpIndex},
    tpEvaluate[tpPair[tpKind] - tpIdentity[tpKind], tpCharts[[tpIndex]]] === 0],
  {tpKind, {"Angle", "Square"}}, {tpIndex, Length[tpCharts]}];

Do[
  tpReduced = ReduceSpinorProducts[tpPair[tpKind]];
  check[{"the ordinary reducer exposes but does not discard the internal-shell remainder", tpKind},
    !FreeQ[tpReduced, MomProd[1, 3] | MomProd[3, 1]] &&
      Count[tpReduced, _Mom, Infinity] < Count[tpPair[tpKind], _Mom, Infinity]];
  check[{"the ordinary sandwich reduction is idempotent", tpKind},
    ReduceSpinorProducts[tpReduced] === tpReduced];
  check[{"ordinary sandwich reduction agrees with independent matrices", tpKind},
    AllTrue[Take[tpCharts, 8], tpEvaluate[tpPair[tpKind] - tpReduced, #] === 0 &]];
  tpExplicit = tpPair[tpKind] /. tpSpin[tpKind, 4] ->
    Spinor["Spin", "Upper", tpKind, 4, -1/2];
  tpExplicitReduced = ReduceSpinorProducts[tpExplicit];
  check[{"sandwich reduction retains an explicit spectator-spin index", tpKind},
    !FreeQ[tpExplicitReduced, Spinor["Spin", "Upper", tpKind, 4, -1/2]] &&
      AllTrue[Take[tpCharts, 8], tpEvaluate[tpExplicit - tpExplicitReduced, #] === 0 &]];
  tpWeighted = 7 tpPair[tpKind]^2;
  tpWeightedReduced = TimeConstrained[ReduceSpinorProducts[tpWeighted], 5, $TimedOut];
  check[{"positive sandwich powers terminate and preserve value", tpKind},
    tpWeightedReduced =!= $TimedOut &&
      AllTrue[Take[tpCharts, 8], tpEvaluate[tpWeighted - tpWeightedReduced, #] === 0 &]],
  {tpKind, {"Angle", "Square"}}];

Do[
  tpFirstOrder = If[tpReverseFirst === 0, {1, 2}, {2, 1}];
  tpSecondOrder = If[tpReverseSecond === 0, {1, 3}, {3, 1}];
  tpReversedPair = tpChain[tpKind, 3, tpFirstOrder[[1]], tpFirstOrder[[2]], 3] *
    tpChain[tpKind, 4, tpSecondOrder[[1]], tpSecondOrder[[2]], 4];
  tpReversedReduced = ReduceSpinorProducts[tpReversedPair];
  tpReversedExpected = (-1)^(tpReverseFirst + tpReverseSecond) tpIdentity[tpKind];
  check[{"both sandwich momentum orientations reduce with the correct exact sign",
      tpKind, tpReverseFirst, tpReverseSecond},
    Count[tpReversedReduced, _Mom, Infinity] < Count[tpReversedPair, _Mom, Infinity] &&
      ReduceSpinorProducts[tpReversedReduced] === tpReversedReduced &&
      AllTrue[Take[tpCharts, 8],
        tpEvaluate[tpReversedPair - tpReversedReduced, #] === 0 &&
          tpEvaluate[tpReversedPair - tpReversedExpected, #] === 0 &]],
  {tpKind, {"Angle", "Square"}}, {tpReverseFirst, 0, 1}, {tpReverseSecond, 0, 1}];

tpOpaque = PropDen[Mom[Multiparticle[1, 3]], tpMass];
tpWithPole = ReduceSpinorProducts[tpPair["Square"]/tpOpaque];
check["ordinary sandwich reduction preserves the physical propagator exactly",
  DeleteDuplicates[Cases[tpWithPole, _PropDen, Infinity]] === {tpOpaque}];
check["ordinary reduction does not introduce a composite internal mass-shell assumption",
  FreeQ[tpWithPole, Mass[_Multiparticle]] && !FreeQ[tpWithPole, MomProd[1, 3]]];

tpLargeProduct = Times @@ Table[
  tpChain["Square", 100 + 4 tpIndex, 10 tpIndex, 10 tpIndex + 1, 100 + 4 tpIndex] *
    tpChain["Square", 101 + 4 tpIndex, 10 tpIndex, 100 + 4 tpIndex, 101 + 4 tpIndex],
  {tpIndex, 13}];
tpHugePower = tpChain["Square", 3, 1, 2, 3]^(10^9) *
  tpChain["Square", 4, 1, 3, 4]^(10^9);
check["the sandwich expansion guard retains an oversized original product atomically",
  ConstructiveDiagrams`Private`rspSandwichProducts[tpLargeProduct] === tpLargeProduct];
check["the sandwich expansion guard handles huge powers without materializing them",
  ConstructiveDiagrams`Private`rspSandwichProducts[tpHugePower] === tpHugePower];
tpLargeReduced = TimeConstrained[ReduceSpinorProducts[tpLargeProduct], 10, $TimedOut];
check["an oversized sandwich product retains the legacy reduction behavior",
  tpLargeReduced === tpLargeProduct];
check["oversized sandwich products remain fixed points",
  tpLargeReduced =!= $TimedOut && ReduceSpinorProducts[tpLargeReduced] === tpLargeReduced];
check["the branch estimate saturates instead of allocating an enormous sum",
  ConstructiveDiagrams`Private`rspSandwichExpansionCount[(tpAA + tpBB)^(10^9)] === 4097];

(* Derive the coupling normalization from the supplied model, never from a fit.
   The thesis uses e at each vertex; model 2 uses -Sqrt[2] EE. *)
tpModel = ReadModel[FileNameJoin[{root, "models"}], 2];
tpMass = ModelParameter[tpModel, "Me"];
tpCharge = ModelParameter[tpModel, "EE"];
tpVertex = SelectFirst[tpModel[[2]],
  Sort[DeleteCases[Take[#, 4], ""]] === Sort[{"e", "E", "A.+"}] &];
tpVertexFactor = tpVertex[[5]];
check["the thesis normalization is read from the cubic model vertex",
  Expand[tpVertexFactor^4 - 4 tpCharge^4] === 0];

tpParticles = {"e", "E", "A.+", "A.+", "A.+", "A.+"};
tpDiagrams = CreateDiagrams[tpParticles, tpModel];
tpRaw = diagramAmplitudes[tpDiagrams];
tpRawHash = Hash[tpRaw, "SHA256"];
tpCuts[pair_] := Sort[DeleteDuplicates[First /@ DeleteMissing[
  ConstructiveDiagrams`Private`residuePropagatorKey[#, Range[6]] & /@
    DeleteDuplicates[Cases[Last[pair], _PropDen, Infinity]]]]];
tpWantedCuts = Sort[{{1, 3}, {1, 3, 4}, {2, 6}}];
tpRaw3456 = SelectFirst[tpRaw, tpCuts[#] === tpWantedCuts &, Missing["No3456Diagram"]];
check["the worked path starts from an actual six-point diagramAmplitude pair",
  Length[tpRaw] === 24 && !MissingQ[tpRaw3456]];

tpExpected = -tpVertexFactor^4 tpMass^3 *
  SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] *
  tpBracket["Square", 3, 4]^2 tpBracket["Square", 5, 6]^2 /
  Times @@ (PropDen[Mom[Multiparticle @@ #], tpMass] & /@
    {{1, 3}, {1, 4}, {1, 3, 4}, {2, 5}, {2, 6}});
tpSame[left_, right_] := TrueQ[Cancel[Together[
  ConstructiveDiagrams`Private`canonicalChains[
    ConstructiveDiagrams`Private`residueCanonicalPropagators[left - right, Range[6]]]]] === 0];
tpForced = MapThread[<|"Leg" -> #1, "Mode" -> "AllPhysical", "Target" -> #2|> &,
  {{3, 4, 5, 6}, {2, 1, 2, 1}}];

If[!MissingQ[tpRaw3456],
  tpContracted = ReduceSpinContractions[Last[tpRaw3456]] /. First[tpRaw3456];
  tpReferences = ConstructiveDiagrams`Private`preparationReferences[
    tpContracted, First[tpRaw3456], tpModel, tpRaw, "AllPhysical", tpForced];
  check["the thesis reference assignment uses admissible external physical poles",
    AssociationQ[tpReferences] && tpReferences["Status"] === "COMPLETE" &&
      FreeQ[tpReferences["Expression"], Spinor["Xi", ___]]];
  If[AssociationQ[tpReferences] && tpReferences["Status"] === "COMPLETE",
    tpAssignedPair = {First[tpRaw3456], tpReferences["Expression"]};
    Do[
      tpTrialPair = Fold[PermuteParticles, tpAssignedPair, tpCycles];
      tpTrialExpected = Fold[PermuteParticles, tpExpected, tpCycles];
      tpTrialAuxiliary = Fold[PermuteParticles,
        tpReferences["AuxiliaryPhysicalPropagators"], tpCycles];
      tpPrepared = TimeConstrained[ConstructiveDiagrams`Private`preparationReferenceTrial[
        Last[tpTrialPair], First[tpTrialPair],
        ConstructiveDiagrams`Private`preparationDotRules[First[tpTrialPair], True],
        tpTrialAuxiliary, True], 20, $TimedOut];
      check[{"the prescribed physical references reach Eq101 with typed photon permutations", tpCycles},
        tpPrepared =!= $TimedOut && tpSame[tpPrepared, tpTrialExpected]];
      check[{"the prepared diagram is one term without momentum insertions", tpCycles},
        tpPrepared =!= $TimedOut && Head[tpPrepared] =!= Plus &&
          FreeQ[tpPrepared, Spinor["Xi", ___]] &&
          FreeQ[Cases[tpPrepared, _SpinorChain, Infinity], _Mom]],
      {tpCycles, {{}, {{3, 4}}, {{4, 5}}}}]];
  tpAutomaticOptions = Join[Association[Options[PrepareAmplitudes]],
    <|"PolePreference" -> "AllPhysical", "MaxSteps" -> 0,
      "TimeConstraint" -> 20, "ReferenceTimeConstraint" -> 15,
      "ProgressReporting" -> False, "ReturnDetails" -> True|>];
  Do[
    tpAutomaticPair = Fold[PermuteParticles, tpRaw3456, tpCycles];
    tpAutomatic = TimeConstrained[
      ConstructiveDiagrams`Private`preparationOneAmplitude[
        tpAutomaticPair, 1, Length[tpRaw], tpAutomaticOptions, tpModel, tpRaw, "Diagram"],
      25, $TimedOut];
    check[{"automatic AllPhysical reference selection reaches a single momentum-free term", tpCycles},
      AssociationQ[tpAutomatic] && tpAutomatic["Status"] === "PREPARED" &&
        tpAutomatic["PreparedExpression"] =!= 0 &&
        Head[tpAutomatic["PreparedExpression"]] =!= Plus &&
        FreeQ[tpAutomatic["PreparedExpression"], _Mass] &&
        tpSame[tpAutomatic["PreparedExpression"], Fold[PermuteParticles, tpExpected, tpCycles]] &&
        FreeQ[Cases[tpAutomatic["PreparedExpression"], _SpinorChain, Infinity], _Mom]];
    check[{"automatic physical preparation preserves every original diagram propagator", tpCycles},
      AssociationQ[tpAutomatic] &&
        ContainsAll[tpCuts[{First[tpAutomaticPair], tpAutomatic["PreparedExpression"]}],
          tpCuts[tpAutomaticPair]]],
    {tpCycles, {{}, {{3, 4}}, {{4, 5}}}}];
  check["preparation leaves the retained original diagram expressions unchanged",
    Hash[tpRaw, "SHA256"] === tpRawHash]
];

(* The six-point improvement must preserve the established five-point
   AllSpurious compactness bound, using the same cubic model as its fixture. *)
tpFiveRaw = diagramAmplitudes[CreateDiagrams[
  {"e", "E", "A.+", "A.+", "A.+"}, tpModel]];
tpFivePrepared = TimeConstrained[PrepareAmplitudes[tpFiveRaw, tpModel,
  "PolePreference" -> "AllSpurious", "ProgressReporting" -> False], 120, $TimedOut];
Do[
  check[{"five-point AllSpurious retains or improves the compact primary complexity", tpIndex},
    ListQ[tpFivePrepared] && Length[tpFivePrepared] === 6 &&
      With[{expression = Last[tpFivePrepared[[tpIndex]]]},
        SpinorExpressionComplexity[expression] <= 13 &&
          (SpinorExpressionComplexity[expression] < 13 ||
            ConstructiveDiagrams`Private`expressionTermCounts[expression]["Terms"] <= 2)]],
  {tpIndex, 6}];

tpOriginalPoleTerm = MomProd[1, 3]/PropDen[Mom[Multiparticle[1, 3]], tpMass];
tpExternalRules = Thread[Mass /@ Range[6] -> {tpMass, tpMass, 0, 0, 0, 0}];
check["a physical invariant never cancels an unrecorded input propagator",
  ConstructiveDiagrams`Private`preparationCancelReferencePoles[
    tpOriginalPoleTerm, tpExternalRules, {}] === tpOriginalPoleTerm];
check["an explicitly recorded reference propagator may cancel by an ordinary invariant identity",
  ConstructiveDiagrams`Private`preparationCancelReferencePoles[
    tpOriginalPoleTerm, tpExternalRules,
    {PropDen[Mom[Multiparticle[1, 3]], tpMass]}] === 1/2];
tpReferencePole = PropDen[Mom[Multiparticle[1, 3]], tpMass];
tpInverseSpectator = 1/(1 + MomProd[1, 3]);
check["auxiliary-pole cancellation does not rewrite invariants inside another inverse factor",
  ConstructiveDiagrams`Private`preparationCancelReferencePoles[
    tpInverseSpectator/tpReferencePole, tpExternalRules, {tpReferencePole}] ===
      tpInverseSpectator/tpReferencePole];
check["auxiliary-pole cancellation handles the reversed scalar-product order",
  ConstructiveDiagrams`Private`preparationCancelReferencePoles[
    MomProd[3, 1]/tpReferencePole, tpExternalRules, {tpReferencePole}] === 1/2];
tpWrongMassPole = PropDen[Mom[Multiparticle[1, 3]], tpOtherMass];
check["an unrelated exchanged mass cannot turn a scalar product into a propagator",
  ConstructiveDiagrams`Private`preparationCancelReferencePoles[
    MomProd[1, 3]/tpWrongMassPole, tpExternalRules, {tpWrongMassPole}] ===
      MomProd[1, 3]/tpWrongMassPole];

(* Reference denominators coinciding with an input pole or its conserved
   complement are not auxiliary, even when shared Feynman references allow them. *)
tpFourExternal = Thread[Mass /@ Range[4] -> {tpMass, tpMass, 0, 0}];
tpReferenceRatio = SpinorChain[Spinor["Xi", "Angle", 99], Mom[1], tpSpin["Square", 3]] /
  SpinorChain[Spinor["Xi", "Angle", 99], tpSpin["Angle", 3]];
tpDen14 = PropDen[Mom[Multiparticle[1, 4]], tpMass];
tpDen23 = PropDen[Mom[Multiparticle[2, 3]], tpMass];
tpFourPairs = {{Append[tpFourExternal, Mass[Multiparticle[1, 4]] -> tpMass],
    tpReferenceRatio/tpDen14},
  {Append[tpFourExternal, Mass[Multiparticle[1, 3]] -> tpMass],
    1/PropDen[Mom[Multiparticle[1, 3]], tpMass]}};
tpSharedForced = {<|"Leg" -> 3, "Mode" -> "AllPhysical", "Target" -> 2|>};
tpComplementReference = ConstructiveDiagrams`Private`preparationReferences[
  tpReferenceRatio/tpDen14, First[First[tpFourPairs]], tpModel,
  tpFourPairs, "AllPhysical", tpSharedForced, True];
tpSameReference = ConstructiveDiagrams`Private`preparationReferences[
  tpReferenceRatio/tpDen23, First[First[tpFourPairs]], tpModel,
  tpFourPairs, "AllPhysical", tpSharedForced, True];
tpNewReference = ConstructiveDiagrams`Private`preparationReferences[
  tpReferenceRatio, tpFourExternal, tpModel,
  tpFourPairs, "AllPhysical", tpSharedForced, True];
check["a shared reference overlapping an original complementary pole remains supported",
  tpComplementReference["Status"] === "COMPLETE"];
check["an original complementary propagator is never classified as an auxiliary reference pole",
  tpComplementReference["AuxiliaryPhysicalPropagators"] === {}];
check["an identical original propagator is never classified as an auxiliary reference pole",
  tpSameReference["AuxiliaryPhysicalPropagators"] === {}];
check["a genuinely new reference propagator is retained in the explicit auxiliary inventory",
  tpNewReference["AuxiliaryPhysicalPropagators"] === {tpDen23}];
check["overlapping original and reference propagators remain in the assigned expression",
  !FreeQ[tpComplementReference["Expression"], tpDen14] &&
    !FreeQ[tpComplementReference["Expression"], tpDen23]];
tpComplementRatio = 2 MomProd[2, 3]/(tpDen14 tpDen23);
check["an original complementary pole cannot cancel through reference provenance",
  ConstructiveDiagrams`Private`preparationCancelReferencePoles[
    tpComplementRatio, tpFourExternal,
    tpComplementReference["AuxiliaryPhysicalPropagators"]] === tpComplementRatio];

(* Conservation is process-dependent and applies only to existing numerator
   momentum chains, never inverse factors or standalone invariant squares. *)
tpSparseLabels = {2, 5, 8, 11};
tpSparseRules = Thread[Mass /@ tpSparseLabels -> ConstantArray[0, 4]];
tpConservedChain = SpinorChain[tpSpin["Square", 2], Mom[Multiparticle[2, 5, 8]],
  tpSpin["Angle", 11]];
tpConservedDenominator = PropDen[Mom[Multiparticle[2, 5]], tpMass];
tpConserve[value_] := ConstructiveDiagrams`Private`preparationConservedMomentumChains[
  value, tpSparseRules, True];
check["the preparation conservation pass handles nonconsecutive external labels",
  tpConserve[tpConservedChain] === 0];
check["ordinary ReduceSpinorProducts does not infer process momentum conservation",
  ReduceSpinorProducts[tpConservedChain] =!= 0];
check["numerator conservation leaves physical propagators untouched",
  tpConserve[(1 + tpConservedChain)/tpConservedDenominator] === 1/tpConservedDenominator];
check["numerator conservation leaves auxiliary inverse spinor expressions untouched",
  tpConserve[(1 + tpConservedChain)/(tpConservedDenominator (3 + tpConservedChain))] ===
    1/(tpConservedDenominator (3 + tpConservedChain))];
tpBareCompositeSquare = Mom[Multiparticle[2, 5, 8]]^2;
check["chain conservation never expands or changes a standalone momentum square",
  tpConserve[tpBareCompositeSquare] === tpBareCompositeSquare];
tpUnknownChain = SpinorChain[tpSpin["Square", 2], Mom[Multiparticle[2, 5, 12]],
  tpSpin["Angle", 11]];
check["an unknown external momentum label prevents conservation assumptions",
  tpConserve[tpUnknownChain] === tpUnknownChain];
check["the preparation conservation pass can be disabled explicitly",
  ConstructiveDiagrams`Private`preparationConservedMomentumChains[
    tpConservedChain, tpSparseRules, False] === tpConservedChain];
check["the bounded conservation cleanup is idempotent",
  tpConserve[tpConserve[(1 + tpConservedChain)/tpConservedDenominator]] ===
    tpConserve[(1 + tpConservedChain)/tpConservedDenominator]];
check["total-momentum insertions vanish for generic unchanged reference endpoints",
  tpConserve[SpinorChain[Spinor["Xi", "Square", 19], Mom[Multiparticle @@ tpSparseLabels],
    Spinor["Xi", "Angle", 23]]] === 0];

tpKnownCompositeRules = Join[tpFourExternal,
  {Mass[Multiparticle[1, 3]] -> tpMass}];
check["composite-mass normalization does not invent an unknown channel mass",
  ConstructiveDiagrams`Private`preparationSpinorProducts[
    Mass[Multiparticle[2, 3]], tpKnownCompositeRules] === Mass[Multiparticle[2, 3]]];
check["composite-mass normalization retains repeated momentum labels",
  ConstructiveDiagrams`Private`preparationSpinorProducts[
    Mass[Multiparticle[1, 1, 3]], tpKnownCompositeRules] === Mass[Multiparticle[1, 1, 3]]];
tpProtectedCompositeMass = 1/(1 + Mass[Multiparticle[3, 1]]);
check["diagram-local mass substitutions never act inside an unrelated inverse factor",
  ConstructiveDiagrams`Private`preparationSpinorProducts[
    tpProtectedCompositeMass, tpKnownCompositeRules] === tpProtectedCompositeMass];

(* Eq. (90) is the opposite physical-reference family to Eq. (72).
   The signed model vertex is f = -Sqrt[2] EE; the final coefficient is -f^3. *)
tpFiveCuts[pair_] := Sort[DeleteDuplicates[First /@ DeleteMissing[
  ConstructiveDiagrams`Private`residuePropagatorKey[#, Range[5]] & /@
    DeleteDuplicates[Cases[Last[pair], _PropDen, Infinity]]]]];
tpFiveSame[left_, right_] := TrueQ[Cancel[Together[
  ConstructiveDiagrams`Private`canonicalChains[
    ConstructiveDiagrams`Private`residueCanonicalPropagators[left - right, Range[5]]]]] === 0];
tpFiveEq90 = -tpVertexFactor^3 tpMass *
  SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] *
  tpBracket["Square", 4, 5]^2 tpChain["Square", 3, 1, 2, 3] /
  Times @@ (PropDen[Mom[Multiparticle @@ #], tpMass] & /@
    {{1, 3}, {2, 3}, {2, 4}, {2, 5}});
tpFivePhysical = TimeConstrained[PrepareAmplitudes[tpFiveRaw, tpModel,
  "PolePreference" -> "AllPhysical", "ProgressReporting" -> False], 90, $TimedOut];
check["five-point AllPhysical keeps the ordinary amplitude-pair output by default",
  ListQ[tpFivePhysical] && Length[tpFivePhysical] === 6 &&
    AllTrue[tpFivePhysical, MatchQ[#, {_List, _}] &]];
Do[
  check[{"every default five-point physical diagram is one term with one momentum sandwich", tpIndex},
    ListQ[tpFivePhysical] && Length[tpFivePhysical] === 6 &&
      With[{expression = Last[tpFivePhysical[[tpIndex]]]},
        expression =!= 0 && Head[expression] =!= Plus && FreeQ[expression, _Mass | Spinor["Xi", ___]] &&
          Count[Cases[expression, _SpinorChain, Infinity], _Mom, Infinity] === 2 &&
          Length[tpFiveCuts[tpFivePhysical[[tpIndex]]]] === 4 &&
          ContainsAll[tpFiveCuts[tpFivePhysical[[tpIndex]]], tpFiveCuts[tpFiveRaw[[tpIndex]]]]]],
  {tpIndex, 6}];

Do[
  tpFivePair = tpFiveRaw[[tpIndex]];
  tpFiveContracted = ReduceSpinContractions[Last[tpFivePair]] /. First[tpFivePair];
  tpFiveReferenceBase = ConstructiveDiagrams`Private`preparationReferences[
    tpFiveContracted, First[tpFivePair], tpModel, tpFiveRaw, "AllPhysical"];
  tpFiveSelections = ConstructiveDiagrams`Private`preparationReferenceSelections[
    tpFiveReferenceBase["Choices"], 64]["Selections"];
  Do[
    tpFiveReference = ConstructiveDiagrams`Private`preparationReferences[
      tpFiveContracted, First[tpFivePair], tpModel, tpFiveRaw,
      "AllPhysical", tpFiveSelections[[tpChoice]]];
    tpFiveTrial = TimeConstrained[ConstructiveDiagrams`Private`preparationReferenceTrial[
      tpFiveReference["Expression"], First[tpFivePair],
      ConstructiveDiagrams`Private`preparationDotRules[First[tpFivePair], True],
      tpFiveReference["AuxiliaryPhysicalPropagators"], True], 10, $TimedOut];
    check[{"both admissible physical-reference choices produce a compact five-point diagram",
        tpIndex, tpChoice},
      Length[tpFiveSelections] === 2 && tpFiveTrial =!= $TimedOut && tpFiveTrial =!= 0 &&
        Head[tpFiveTrial] =!= Plus && FreeQ[tpFiveTrial, _Mass | Spinor["Xi", ___]] &&
        Count[Cases[tpFiveTrial, _SpinorChain, Infinity], _Mom, Infinity] === 2 &&
        Length[tpFiveCuts[{First[tpFivePair], tpFiveTrial}]] === 4 &&
        ContainsAll[tpFiveCuts[{First[tpFivePair], tpFiveTrial}], tpFiveCuts[tpFivePair]]],
    {tpChoice, 2}],
  {tpIndex, 6}];

Do[
  tpFiveWantedCuts = If[tpOrdering === "345", {{1, 3}, {2, 5}}, {{1, 3}, {2, 4}}];
  tpFivePair = SelectFirst[tpFiveRaw, tpFiveCuts[#] === Sort[tpFiveWantedCuts] &];
  tpFiveContracted = ReduceSpinContractions[Last[tpFivePair]] /. First[tpFivePair];
  tpFiveTargets = If[tpOrdering === "345", {2, 2, 1}, {2, 1, 2}];
  tpFiveForced = MapThread[<|"Leg" -> #1, "Mode" -> "AllPhysical", "Target" -> #2|> &,
    {{3, 4, 5}, tpFiveTargets}];
  tpFiveReference = ConstructiveDiagrams`Private`preparationReferences[
    tpFiveContracted, First[tpFivePair], tpModel, tpFiveRaw, "AllPhysical", tpFiveForced];
  tpFiveRawCoefficient = tpMass (tpFiveReference["Expression"] /.
    {_SpinorChain -> 1, _PropDen -> 1});
  check[{"the three-photon raw coefficient follows the actual signed cubic vertex", tpOrdering},
    Expand[tpFiveRawCoefficient - tpVertexFactor^3] === 0];
  Do[
    tpFivePermutedPair = Fold[PermuteParticles,
      {First[tpFivePair], tpFiveReference["Expression"]}, tpCycles];
    tpFivePermutedAuxiliary = Fold[PermuteParticles,
      tpFiveReference["AuxiliaryPhysicalPropagators"], tpCycles];
    tpFivePermutedExpected = Fold[PermuteParticles, tpFiveEq90, tpCycles];
    tpFiveTrial = TimeConstrained[ConstructiveDiagrams`Private`preparationReferenceTrial[
      Last[tpFivePermutedPair], First[tpFivePermutedPair],
      ConstructiveDiagrams`Private`preparationDotRules[First[tpFivePermutedPair], True],
      tpFivePermutedAuxiliary, True], 10, $TimedOut];
    check[{"both forced Eq90 routes and their typed permutations agree exactly", tpOrdering, tpCycles},
      tpFiveTrial =!= $TimedOut && FreeQ[tpFiveTrial, _Mass | Spinor["Xi", ___]] &&
        Head[tpFiveTrial] =!= Plus && tpFiveSame[tpFiveTrial, tpFivePermutedExpected]],
    {tpCycles, {{}, {{3, 4}}, {{4, 5}}}}],
  {tpOrdering, {"345", "354"}}];

tpFiveOrdinaryPair = tpChain["Square", 4, 2, 3, 4] tpChain["Square", 3, 1, 2, 3];
tpFiveOrdinaryReduced = ReduceSpinorProducts[tpFiveOrdinaryPair];
check["the ordinary reversed Eq90 sandwich identity retains the selected-shell remainder",
  !FreeQ[tpFiveOrdinaryReduced, MomProd[2, 3] | MomProd[3, 2]] &&
    AllTrue[Take[tpCharts, 8], tpEvaluate[tpFiveOrdinaryPair - tpFiveOrdinaryReduced, #] === 0 &]];
tpFiveZeroProduct = tpChain["Square", 3, 2, 2, 3] *
  tpChain["Square", 4, 1, 2, 4] * tpBracket["Square", 3, 4];
check["ordinary factored-chain cleanup removes a zero factor before product rewriting",
  ReduceSpinorProducts[tpFiveZeroProduct] === 0];

tpFiveFactored = SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] *
  tpChain["Square", 3, 1, 2, 3] *
  SpinorChain[tpSpin["Square", 4], Mom[2], Mom[Multiparticle[2, 5]], tpSpin["Square", 4]] *
  tpChain["Square", 5, 1, 2, 5];
tpFiveFactoredReduced = ReduceSpinorProducts[tpFiveFactored];
tpFiveOffCutCharts = BlockRandom[SeedRandom[721097]; Table[
  tpShell[leg_] := Module[{a = RandomInteger[{1, 5}], b = RandomInteger[{-4, 4}],
      c = RandomInteger[{-4, 4}]}, {{a, b}, {c, (1 + b c)/a}}];
  tpMomenta = AssociationThread[{1, 2}, tpShell /@ {1, 2}];
  tpAngles = AssociationThread[Range[5], Table[RandomInteger[{1, 5}, 2], {5}]];
  tpSquares = AssociationThread[{3, 4, 5}, Table[RandomInteger[{-4, 4}, 2], {3}]];
  Do[AssociateTo[tpSquares, tpLeg -> {{0, 1}, {-1, 0}} .
      {{tpMomenta[tpLeg][[2, 2]], -tpMomenta[tpLeg][[1, 2]]},
        {-tpMomenta[tpLeg][[2, 1]], tpMomenta[tpLeg][[1, 1]]}} . tpAngles[tpLeg]],
    {tpLeg, {1, 2}}];
  Do[AssociateTo[tpMomenta, tpLeg -> Outer[Times, tpAngles[tpLeg], tpSquares[tpLeg]]],
    {tpLeg, {3, 4, 5}}];
  <|"Momenta" -> tpMomenta, "Angle" -> tpAngles, "Square" -> tpSquares,
    "PoleVariable" -> tpUnusedPole, "ReferenceSeed" -> 1|>, {6}]];
check["ordinary per-factor cleanup exposes the compact Eq90 product without imposing an internal shell",
  Head[tpFiveFactoredReduced] === Plus && Length[tpFiveFactoredReduced] <= 3 &&
    Count[Cases[tpFiveFactoredReduced, _SpinorChain, Infinity], _Mom, Infinity] <= 8 &&
    !FreeQ[tpFiveFactoredReduced, MomProd[2, 5] | MomProd[5, 2]] &&
    ReduceSpinorProducts[tpFiveFactoredReduced] === tpFiveFactoredReduced &&
    AllTrue[tpFiveOffCutCharts, tpEvaluate[tpFiveFactored - tpFiveFactoredReduced, #] === 0 &]];

(* Mixed helicities require the chirality-dual massless-endpoint contraction.
   Its selected-shell remainder is retained by ordinary reduction. P and Q
   are arbitrary, and the spectator spinor need not be a massless particle. *)
tpOppositeKind["Angle"] := "Square";
tpOppositeKind["Square"] := "Angle";
tpOppositePair[kind_] := tpChain[kind, 3, 1, 2, 3] *
  tpChain[tpOppositeKind[kind], 4, 1, 3, 4];
tpMixedSandwich[kind_, momentum_] := SpinorChain[
  tpSpin[tpOppositeKind[kind], 4], Mom[momentum], tpSpin[kind, 3]];
tpOppositeIdentity[kind_] :=
  2 MomProd[2, 3] tpMixedSandwich[kind, 1]^2 -
  2 MomProd[1, 3] tpMixedSandwich[kind, 1] tpMixedSandwich[kind, 2];
Do[
  check[{"opposite-chirality sandwich identity on an independent exact chart", tpKind, tpIndex},
    tpEvaluate[tpOppositePair[tpKind] - tpOppositeIdentity[tpKind], tpCharts[[tpIndex]]] === 0],
  {tpKind, {"Angle", "Square"}}, {tpIndex, Length[tpCharts]}];
Do[
  tpOppositeReduced = ReduceSpinorProducts[tpOppositePair[tpKind]];
  check[{"opposite-chirality reduction exposes the nonzero off-cut remainder", tpKind},
    !FreeQ[tpOppositeReduced, MomProd[1, 3] | MomProd[3, 1]] &&
      Count[tpOppositeReduced, _Mom, Infinity] < Count[tpOppositePair[tpKind], _Mom, Infinity] &&
      AllTrue[Take[tpCharts, 8],
        tpEvaluate[tpOppositePair[tpKind] - tpOppositeReduced, #] === 0 &]];
  check[{"opposite-chirality reduction is idempotent", tpKind},
    ReduceSpinorProducts[tpOppositeReduced] === tpOppositeReduced];
  tpOppositeExplicit = tpOppositePair[tpKind] /.
    tpSpin[tpOppositeKind[tpKind], 4] ->
      Spinor["Spin", "Upper", tpOppositeKind[tpKind], 4, -1/2];
  tpOppositeExplicitReduced = ReduceSpinorProducts[tpOppositeExplicit];
  check[{"opposite-chirality reduction preserves explicit spectator-spin indices", tpKind},
    !FreeQ[tpOppositeExplicitReduced,
        Spinor["Spin", "Upper", tpOppositeKind[tpKind], 4, -1/2]] &&
      AllTrue[Take[tpCharts, 8],
        tpEvaluate[tpOppositeExplicit - tpOppositeExplicitReduced, #] === 0 &]];
  Do[
    tpOppositePowered = 7 tpChain[tpKind, 3, 1, 2, 3]^tpPowers[[1]] *
      tpChain[tpOppositeKind[tpKind], 4, 1, 3, 4]^tpPowers[[2]];
    tpOppositePoweredReduced = TimeConstrained[ReduceSpinorProducts[tpOppositePowered], 5, $TimedOut];
    check[{"opposite-chirality positive powers terminate with exact value", tpKind, tpPowers},
      tpOppositePoweredReduced =!= $TimedOut &&
        AllTrue[Take[tpCharts, 8],
          tpEvaluate[tpOppositePowered - tpOppositePoweredReduced, #] === 0 &]],
    {tpPowers, {{2, 2}, {2, 3}}}],
  {tpKind, {"Angle", "Square"}}];
Do[
  tpFirstOrder = If[tpReverseFirst === 0, {1, 2}, {2, 1}];
  tpSecondOrder = If[tpReverseSecond === 0, {1, 3}, {3, 1}];
  tpOppositeReversed = tpChain[tpKind, 3, tpFirstOrder[[1]], tpFirstOrder[[2]], 3] *
    tpChain[tpOppositeKind[tpKind], 4, tpSecondOrder[[1]], tpSecondOrder[[2]], 4];
  tpOppositeReversedReduced = ReduceSpinorProducts[tpOppositeReversed];
  tpOppositeExpected = (-1)^(tpReverseFirst + tpReverseSecond) tpOppositeIdentity[tpKind];
  check[{"both opposite-chirality momentum orders preserve their exact signs",
      tpKind, tpReverseFirst, tpReverseSecond},
    Count[tpOppositeReversedReduced, _Mom, Infinity] < Count[tpOppositeReversed, _Mom, Infinity] &&
      ReduceSpinorProducts[tpOppositeReversedReduced] === tpOppositeReversedReduced &&
      AllTrue[Take[tpCharts, 8],
        tpEvaluate[tpOppositeReversed - tpOppositeReversedReduced, #] === 0 &&
          tpEvaluate[tpOppositeReversed - tpOppositeExpected, #] === 0 &]],
  {tpKind, {"Angle", "Square"}}, {tpReverseFirst, 0, 1}, {tpReverseSecond, 0, 1}];
tpOppositeWithPole = ReduceSpinorProducts[tpOppositePair["Square"]/tpOpaque];
check["the opposite-chirality contraction never cancels an original propagator or assumes its shell",
  DeleteDuplicates[Cases[tpOppositeWithPole, _PropDen, Infinity]] === {tpOpaque} &&
    !FreeQ[tpOppositeWithPole, MomProd[1, 3] | MomProd[3, 1]] &&
    FreeQ[tpOppositeWithPole, Mass[_Multiparticle]]];

(* The caller controls which numerator shell rules participate in ranking.
   The three-argument helper remains an ordinary external-shell operation. *)
tpScopeRules = Thread[Mass /@ Range[5] -> {tpMass, tpMass, 0, 0, 0}];
tpScopeLeft = Spinor["Xi", "Angle", 70];
tpScopeRight = Spinor["Xi", "Angle", 71];
tpScopeBracket = SpinorChain[tpScopeLeft, tpScopeRight];
tpScopeChain = SpinorChain[tpScopeLeft, Mom[Multiparticle[1, 3]],
  Mom[Multiparticle[1, 3]], tpScopeRight];
tpScopeDots = {MomProd[1, 3] -> 0, MomProd[3, 1] -> 0};
tpScopeReduce[value_, dots_] := ConstructiveDiagrams`Private`preparationConservedMomentumChains[
  value, tpScopeRules, True, dots];
tpScopeOrdinary = tpScopeReduce[tpScopeChain, {}];
check["per-chain preparation without authorized cuts retains its internal-shell remainder",
  !FreeQ[tpScopeOrdinary, MomProd[1, 3] | MomProd[3, 1]] &&
    Expand[tpScopeOrdinary - (tpMass^2 + 2 MomProd[1, 3]) tpScopeBracket] === 0];
check["per-chain ranking may use only the numerator cuts supplied by its caller",
  Expand[tpScopeReduce[tpScopeChain, tpScopeDots] - tpMass^2 tpScopeBracket] === 0];
check["disabling momentum conservation preserves the helper input even when cuts are supplied",
  ConstructiveDiagrams`Private`preparationConservedMomentumChains[
    tpScopeChain, tpScopeRules, False, tpScopeDots] === tpScopeChain];
tpScopeInverse = 1/(3 + tpScopeChain);
check["per-chain authorized cuts leave unrelated inverse expressions opaque",
  tpScopeReduce[tpScopeInverse, tpScopeDots] === tpScopeInverse];
check["per-chain authorized cuts preserve original propagators exactly",
  tpScopeReduce[(1 + tpScopeChain)/tpOpaque, tpScopeDots] ===
    (1 + tpMass^2 tpScopeBracket)/tpOpaque];

(* Exercise actual mixed-helicity diagrams before completion. These checks
   compare numerators only on each original diagram's own two cuts. They do
   not substitute the known completed amplitude for the diagram expressions. *)
tpMixedParticles = {"e", "E", "A.+", "A.+", "A.-"};
tpMixedRaw = diagramAmplitudes[CreateDiagrams[tpMixedParticles, tpModel]];
tpMixedRawHash = Hash[tpMixedRaw, "SHA256"];
tpMixedReport = TimeConstrained[PrepareAmplitudes[tpMixedRaw, tpModel,
  "PolePreference" -> "AllPhysical", "MaxSteps" -> 0,
  "ProgressReporting" -> False, "ReturnDetails" -> True,
  "TimeConstraint" -> 20, "ReferenceTimeConstraint" -> 15], 120, $TimedOut];
tpMixedOrder[pair_] := Module[{cuts = tpFiveCuts[pair], first, last},
  first = First[FirstCase[cuts, {1, leg_Integer} :> {leg}]];
  last = First[FirstCase[cuts, {2, leg_Integer} :> {leg}]];
  {first, First[Complement[{3, 4, 5}, {first, last}]], last}];
tpMixedChart[order_, sample_, spin1_, spin2_] := Module[
  {j = {{0, -1}, {1, 0}}, eps, adj, perpendicular, angles, squares, momenta,
   a, b, c, q1, q2, mass = sample},
  eps = -j;
  adj[p_] := {{p[[2, 2]], -p[[1, 2]]}, {-p[[2, 1]], p[[1, 1]]}};
  perpendicular[v_] := {-v[[2]], v[[1]]};
  {a, b, c} = order;
  angles = <|3 -> {1, sample + 1}, 4 -> {sample + 2, 1}, 5 -> {2, 2 sample + 3}|>;
  momenta = <|1 -> {{sample + 1, sample - 2},
    {sample + 2, (mass^2 + (sample - 2) (sample + 2))/(sample + 1)}}|>;
  squares = <|a -> (sample + 2) perpendicular[adj[momenta[1]].angles[a]]|>;
  AssociateTo[momenta, a -> Outer[Times, angles[a], squares[a]]];
  q1 = momenta[1] + momenta[a];
  AssociateTo[squares, b -> (sample - 5) perpendicular[adj[q1].angles[b]]];
  AssociateTo[momenta, b -> Outer[Times, angles[b], squares[b]]];
  q2 = -q1 - momenta[b];
  AssociateTo[squares, c -> (sample + 3) perpendicular[adj[q2].angles[c]]];
  AssociateTo[momenta, {c -> Outer[Times, angles[c], squares[c]], 2 -> q2 - Outer[Times, angles[c], squares[c]]}];
  AssociateTo[angles, {1 -> UnitVector[2, spin1], 2 -> UnitVector[2, spin2]}];
  AssociateTo[squares, {1 -> eps.adj[momenta[1]].angles[1]/mass,
    2 -> eps.adj[momenta[2]].angles[2]/mass}];
  <|"Momenta" -> momenta, "Angle" -> angles, "Square" -> squares,
    "PoleVariable" -> tpUnusedPole, "ReferenceSeed" -> 1,
    "MassValue" -> mass, "Shells" ->
      ({Det[momenta[1]], Det[momenta[2]], Det[q1], Det[q2]} === ConstantArray[mass^2, 4])|>
];
tpMixedEvaluate[expression_, chart_] := Together[
  expression /. chain_SpinorChain :>
      ConstructiveDiagrams`Private`residueChartChain[chain, chart] /.
    PropDen[Mom[label_], mass_] :>
      Det[ConstructiveDiagrams`Private`residueChartMomentum[label, chart]] - mass^2 /.
    MomProd[a_, b_] :> With[
      {p = ConstructiveDiagrams`Private`residueChartMomentum[a, chart],
       q = ConstructiveDiagrams`Private`residueChartMomentum[b, chart]},
      (Det[p + q] - Det[p] - Det[q])/2] /.
    {tpMass -> chart["MassValue"], tpCharge -> 1}];
tpMixedMiddleNegativeQ[pair_] := tpMixedOrder[pair][[2]] === 5;
tpMixedOwnCutChecks[report_, index_] := Module[
  {originalPoles, originalNumerator, preparedNumerator, chart, originalValue, preparedValue},
  If[!AssociationQ[report], Return[{False}]];
  originalPoles = Times @@ DeleteDuplicates[Cases[Last[tpMixedRaw[[index]]], _PropDen, Infinity]];
  originalNumerator = Cancel[Together[originalPoles *
    report["DiagramReports"][[index]]["PolePreference"]["Expression"]]];
  preparedNumerator = Cancel[Together[originalPoles *
    report["DiagramReports"][[index]]["PreparedExpression"]]];
  Flatten[Table[
    chart = tpMixedChart[tpMixedOrder[tpMixedRaw[[index]]], sample, spin1, spin2];
    originalValue = tpMixedEvaluate[originalNumerator, chart];
    preparedValue = tpMixedEvaluate[preparedNumerator, chart];
    chart["Shells"] &&
      FreeQ[{originalValue, preparedValue}, Indeterminate | ComplexInfinity | DirectedInfinity[_]] &&
      Together[originalValue - preparedValue] === 0,
    {sample, {1, 2}}, {spin1, 2}, {spin2, 2}]]
];
Do[
  check[{"actual mixed physical-reference preparation is bounded and preserves every original cut", tpIndex},
    AssociationQ[tpMixedReport] && Length[tpMixedReport["DiagramReports"]] === 6 &&
      With[{row = tpMixedReport["DiagramReports"][[tpIndex]]},
        row["Status"] === "PREPARED" &&
          With[{expression = row["PreparedExpression"]},
            expression =!= 0 && FreeQ[expression, _Mass | Spinor["Xi", ___]] &&
              ConstructiveDiagrams`Private`expressionTermCounts[expression]["Terms"] <=
                If[tpMixedMiddleNegativeQ[tpMixedRaw[[tpIndex]]], 3, 4] &&
              SpinorExpressionComplexity[expression] <=
                If[tpMixedMiddleNegativeQ[tpMixedRaw[[tpIndex]]], 63, 16] &&
              ContainsAll[tpFiveCuts[{First[tpMixedRaw[[tpIndex]]], expression}],
                tpFiveCuts[tpMixedRaw[[tpIndex]]]]]]];
  tpMixedScopedChecks = tpMixedOwnCutChecks[tpMixedReport, tpIndex];
  check[{"actual mixed prepared numerators agree on their own double cuts for all massive-spin components", tpIndex},
    Length[tpMixedScopedChecks] === 8 && And @@ tpMixedScopedChecks],
  {tpIndex, 6}];
tpMixedSpuriousReport = TimeConstrained[PrepareAmplitudes[tpMixedRaw, tpModel,
  "PolePreference" -> "AllSpurious", "MaxSteps" -> 0,
  "ProgressReporting" -> False, "ReturnDetails" -> True,
  "TimeConstraint" -> 20, "ReferenceTimeConstraint" -> 15], 120, $TimedOut];
Do[
  check[{"every actual mixed spurious-reference preparation keeps its compact factored winner", tpIndex},
    AssociationQ[tpMixedSpuriousReport] && Length[tpMixedSpuriousReport["DiagramReports"]] === 6 &&
      With[{row = tpMixedSpuriousReport["DiagramReports"][[tpIndex]]},
        row["Status"] === "PREPARED" &&
          Length[row["PolePreference"]["Assignments"]] === 3 &&
          AllTrue[row["PolePreference"]["Assignments"], #["Mode"] === "AllSpurious" &] &&
          With[{expression = row["PreparedExpression"]},
            expression =!= 0 && FreeQ[expression, _Mass | Spinor["Xi", ___]] &&
              ConstructiveDiagrams`Private`expressionTermCounts[expression]["Terms"] <=
                If[tpMixedMiddleNegativeQ[tpMixedRaw[[tpIndex]]], 3, 2] &&
              SpinorExpressionComplexity[expression] <=
                If[tpMixedMiddleNegativeQ[tpMixedRaw[[tpIndex]]], 51, 30] &&
              ContainsAll[tpFiveCuts[{First[tpMixedRaw[[tpIndex]]], expression}],
                tpFiveCuts[tpMixedRaw[[tpIndex]]]]]]];
  tpMixedScopedChecks = tpMixedOwnCutChecks[tpMixedSpuriousReport, tpIndex];
  check[{"mixed spurious-reference winners preserve their same-reference numerators on each own double cut", tpIndex},
    Length[tpMixedScopedChecks] === 8 && And @@ tpMixedScopedChecks],
  {tpIndex, 6}];
check["mixed preparation does not alter its retained original amplitude pairs",
  Hash[tpMixedRaw, "SHA256"] === tpMixedRawHash];

(* A complete external amplitude can be smaller than the reducer's normal
   form. Keep that certified input available after mandatory validation;
   raw spin sums and reference factors are never eligible incumbents. *)
tpKnownSpin[kind_, leg_] := Spinor[If[MemberQ[{1, 2}, leg], "Spin", "Helicity"], kind, leg];
tpKnownAngle[a_, b_] := SpinorChain[tpKnownSpin["Angle", a], tpKnownSpin["Angle", b]];
tpKnownSquare[a_, b_] := SpinorChain[tpKnownSpin["Square", a], tpKnownSpin["Square", b]];
tpKnownF[i_] := SpinorChain[tpKnownSpin["Angle", 5], Mom[1], tpKnownSpin["Square", i]];
tpKnownK[i_] := SpinorChain[tpKnownSpin["Square", i], Mom[1], Mom[2], tpKnownSpin["Square", i]];
tpKnownDen[a_, b_] := PropDen[Mom[Multiparticle @@ Sort[{a, b}]], tpMass];
tpKnownA = tpKnownAngle[1, 2];
tpKnownB = tpKnownAngle[1, 5] tpKnownAngle[2, 5];
tpKnownCompleted = -tpVertexFactor^3/(tpMass tpKnownDen[1, 3] tpKnownDen[1, 4] tpKnownDen[1, 5]) *
  (tpKnownF[3] tpKnownK[4] (tpKnownA tpKnownF[3] - tpKnownB tpKnownSquare[3, 5])/tpKnownDen[2, 4] +
   tpKnownF[4] tpKnownK[3] (tpKnownA tpKnownF[4] - tpKnownB tpKnownSquare[4, 5])/tpKnownDen[2, 3] +
   tpMass^2 tpKnownSquare[3, 4]^2 *
    (tpKnownA SpinorChain[tpKnownSpin["Angle", 5], Mom[1], Mom[2], tpKnownSpin["Angle", 5]] +
      tpKnownB (tpKnownDen[1, 5] + tpKnownDen[2, 5]))/tpKnownDen[2, 5]);
check["the completed mixed fixture is the known seven-term expression of complexity seventy-one",
  SpinorExpressionComplexity[tpKnownCompleted] === 71 &&
    ConstructiveDiagrams`Private`expressionTermCounts[tpKnownCompleted]["Terms"] === 7];
check["an already-contracted expression with only external spinors is eligible for best-form retention",
  ConstructiveDiagrams`Private`preparationCompleteExternalInputQ[tpKnownCompleted, tpScopeRules]];
tpIneligibleInputs = {
  "actual raw mixed diagram" -> Last[First[tpMixedRaw]],
  "unassigned reference" -> SpinorChain[Spinor["Xi", "Angle", 90], tpSpin["Angle", 3]],
  "uncontracted spin index" -> SpinorChain[Spinor["Spin", "Upper", "Angle", 1, tpIndexLabel], tpSpin["Angle", 3]],
  "internal spinor momentum" -> SpinorChain[Spinor["Spin", "Angle", Multiparticle[1, 3]], tpSpin["Angle", 3]],
  "x factor" -> xFactor[1, 2],
  "conjugate x factor" -> xTildeFactor[1, 2],
  "hatted spinor" -> SpinorHat["Helicity", "Angle", 3],
  "hatted momentum" -> SpinorChain[tpSpin["Square", 3], MomHat[1], tpSpin["Angle", 4]],
  "first hatted scalar product" -> MomProdHat1[1, 3],
  "second hatted scalar product" -> MomProdHat2[1, 3],
  "twice hatted scalar product" -> MomProdHat12[1, 3],
  "unresolved internal mass" -> Mass[Multiparticle[3, 1]] tpKnownAngle[1, 2],
  "unresolved external masses" -> Mass[1] tpKnownAngle[1, 2]/Mass[2]^2,
  "dummy spin epsilon" -> SpinorEps["Upper", tpIndexLabel, tpSecondIndexLabel],
  "dummy spin transport" -> SpinorZ["Both", tpIndexLabel, tpSecondIndexLabel],
  "auxiliary momentum" -> MomQ[1],
  "unprocessed sigma tensor" -> sigma[tpIndexLabel],
  "zeta reference" -> SpinorChain[Spinor["Zeta", "Angle", 90], tpSpin["Angle", 3]],
  "unfinished reduced spinor chain" -> SpinorChainReduced[
    tpSpin["Square", 3], Mom[1], Mom[2], tpSpin["Square", 3]],
  "unfinished reduced trace" -> SpinorTraceReduced[Mom[1], Mom[2], Mom[3], Mom[4]],
  "raw Feynman word" -> DiracChain[tpRawFeynmanWord],
  "undeclared external spinor" -> SpinorChain[tpSpin["Angle", 99], tpSpin["Angle", 3]],
  "malformed spinor arity" -> SpinorChain[Spinor["Spin", "Angle", 1, tpIndexLabel], tpSpin["Angle", 3]]};
Do[
  check[{"raw or auxiliary expressions cannot seed completed-form retention", First[tpIneligible]},
    !ConstructiveDiagrams`Private`preparationCompleteExternalInputQ[Last[tpIneligible], tpScopeRules]],
  {tpIneligible, tpIneligibleInputs}];
tpKnownPreparation = TimeConstrained[PrepareAmplitudes[{{tpScopeRules, tpKnownCompleted}}, tpModel,
  "PolePreference" -> None, "InternalOnShell" -> False, "MaxSteps" -> 0,
  "TimeConstraint" -> 15, "ProgressReporting" -> False, "ReturnDetails" -> True], 20, $TimedOut];
tpKnownRow = If[AssociationQ[tpKnownPreparation], First[tpKnownPreparation["DiagramReports"]], <||>];
check["external-only preparation never replaces the known completed amplitude with a worse normal form",
  AssociationQ[tpKnownPreparation] && tpKnownRow["Status"] === "PREPARED" &&
    !ConstructiveDiagrams`Private`preparationBetterQ[
      ConstructiveDiagrams`Private`preparationRank[tpKnownCompleted],
      ConstructiveDiagrams`Private`preparationRank[tpKnownRow["PreparedExpression"]]]];
check["retaining a completed input still runs spin contractions, reference assignment and ordinary reduction",
  AssociationQ[tpKnownPreparation] &&
    ContainsAll[Lookup[tpKnownRow["Reductions"], "Operation"],
      {"ReduceSpinContractions", "PolePreference", "ReduceSpinorProducts"}] &&
    tpKnownRow["PolePreference"]["Status"] === "COMPLETE" &&
    TrueQ[tpKnownRow["OriginalCandidateEligible"]] && TrueQ[tpKnownRow["OriginalCandidateAdmitted"]]];
(* Mandatory reductions may reach an equivalent equal-rank form without a
   restoration event. When a best candidate is restored, record selection
   rather than a new identity or an internal-shell substitution. *)
check["completed-input retention reports selection honestly and preserves the amplitude",
  AssociationQ[tpKnownPreparation] &&
    AllTrue[Select[tpKnownRow["Reductions"],
        Lookup[#, "Operation", None] === "RetainedBestCompletedCandidate" &],
      StringQ[Lookup[#, "SourceOperation", None]] &&
        TrueQ[Lookup[#, "SelectionOnly", False]] &&
        !TrueQ[Lookup[#, "InternalOnShellIdentity", True]] &] &&
    TimeConstrained[Module[{chart, difference},
      chart = ConstructiveDiagrams`Private`completionHESymbolicExternalChart[tpScopeRules];
      difference = ConstructiveDiagrams`Private`residueEvaluateChart[
        tpKnownRow["PreparedExpression"] - tpKnownCompleted, chart, {}];
      Cancel[Together[difference]] === 0], 20, False]];
check["completed external preparation does not introduce internal numerator conditions",
  AssociationQ[tpKnownPreparation] && tpKnownRow["InternalOnShellChannels"] === {} &&
    tpKnownRow["InternalOnShellConditions"] === {}];
check["every raw mixed diagram remains ineligible for the original-input incumbent",
  AssociationQ[tpMixedReport] && AllTrue[tpMixedReport["DiagramReports"],
    !TrueQ[#["OriginalCandidateEligible"]] && !TrueQ[#["OriginalCandidateAdmitted"]] &]];
tpKnownMassPreparation = PrepareAmplitudes[{
  {Append[tpScopeRules, Mass[Multiparticle[1, 3]] -> tpMass],
    Mass[Multiparticle[3, 1]] tpKnownAngle[1, 2]}}, tpModel,
  "PolePreference" -> None, "InternalOnShell" -> False, "MaxSteps" -> 0,
  "ProgressReporting" -> False, "ReturnDetails" -> True];
tpKnownMassRow = First[tpKnownMassPreparation["DiagramReports"]];
check["best-form retention cannot restore an unresolved declared internal mass over ordinary replacement",
  tpKnownMassRow["Status"] === "PREPARED" &&
    !TrueQ[tpKnownMassRow["OriginalCandidateEligible"]] &&
    !TrueQ[tpKnownMassRow["OriginalCandidateAdmitted"]] &&
    tpKnownMassRow["PreparedExpression"] === tpMass tpKnownAngle[1, 2] &&
    tpKnownMassRow["InternalOnShellChannels"] === {}];
tpExternalMassPreparation = PrepareAmplitudes[{
  {tpScopeRules, Mass[1] tpKnownAngle[1, 2]/Mass[2]^2}}, tpModel,
  "PolePreference" -> None, "InternalOnShell" -> False, "MaxSteps" -> 0,
  "ProgressReporting" -> False, "ReturnDetails" -> True];
tpExternalMassRow = First[tpExternalMassPreparation["DiagramReports"]];
check["best-form retention cannot undo external mass replacement in a numerator or denominator",
  tpExternalMassRow["Status"] === "PREPARED" &&
    !TrueQ[tpExternalMassRow["OriginalCandidateEligible"]] &&
    !TrueQ[tpExternalMassRow["OriginalCandidateAdmitted"]] &&
    FreeQ[tpExternalMassRow["PreparedExpression"], _Mass] &&
    tpExternalMassRow["PreparedExpression"] === tpKnownAngle[1, 2]/tpMass &&
    tpExternalMassRow["InternalOnShellChannels"] === {}];

(* Inverse and unknown-power spinor factors must pass through ordinary
   denominator reduction before they may compete as completed forms. *)
tpInverseSeeds = Table[
  tpOtherSeedKind = tpOppositeKind[tpSeedKind];
  tpSeedChain = SpinorChain[Spinor["Spin", tpOtherSeedKind, 1], Mom[2],
    Spinor["Spin", tpSeedKind, 2]];
  <|"Name" -> ("reciprocal external Dirac chain " <> tpSeedKind),
    "Expression" -> 1/(tpOpaque tpSeedChain),
    "Expected" -> -1/(tpMass tpOpaque SpinorChain[
      Spinor["Spin", tpOtherSeedKind, 1], Spinor["Spin", tpOtherSeedKind, 2]])|>,
  {tpSeedKind, {"Angle", "Square"}}];
AppendTo[tpInverseSeeds, <|"Name" -> "reciprocal trace",
  "Expression" -> 1/(tpOpaque SpinorTrace[Mom[1], Mom[1]]),
  "Expected" -> 1/(2 tpMass^2 tpOpaque)|>];
tpUnknownSeedChain = SpinorChain[Spinor["Spin", "Square", 1], Mom[2],
  Spinor["Spin", "Angle", 2]];
tpUnknownSeedExpression = (3 + tpUnknownSeedChain^(-tpSeedExponent))^2/tpOpaque;
AppendTo[tpInverseSeeds, <|"Name" -> "spinor factor with unknown exponent",
  "Expression" -> tpUnknownSeedExpression,
  "Expected" -> (ReduceSpinorProducts[tpUnknownSeedExpression] /. tpScopeRules)|>];
Do[
  tpInverseSeedPreparation = PrepareAmplitudes[{{tpScopeRules, tpInverseSeed["Expression"]}}, tpModel,
    "PolePreference" -> None, "InternalOnShell" -> False, "MaxSteps" -> 0,
    "TimeConstraint" -> 10, "ProgressReporting" -> False, "ReturnDetails" -> True];
  tpInverseSeedRow = First[tpInverseSeedPreparation["DiagramReports"]];
  check[{"raw inverse factors cannot bypass ordinary external normalization", tpInverseSeed["Name"]},
    !ConstructiveDiagrams`Private`preparationCompleteExternalInputQ[
      tpInverseSeed["Expression"], tpScopeRules] &&
      tpInverseSeedRow["Status"] === "PREPARED" &&
      !TrueQ[tpInverseSeedRow["OriginalCandidateEligible"]] &&
      !TrueQ[tpInverseSeedRow["OriginalCandidateAdmitted"]] &&
      FreeQ[tpInverseSeedRow["PreparedExpression"],
        SpinorChain[___, _Mom, ___] | _SpinorTrace | _Mass] &&
      Cancel[Together[tpInverseSeedRow["PreparedExpression"] - tpInverseSeed["Expected"]]] === 0 &&
      DeleteDuplicates[Cases[tpInverseSeedRow["PreparedExpression"], _PropDen, Infinity]] === {tpOpaque} &&
      tpInverseSeedRow["InternalOnShellChannels"] === {}],
  {tpInverseSeed, tpInverseSeeds}];
check["completed positive spinor powers and opaque physical propagator inverses remain eligible",
  ConstructiveDiagrams`Private`preparationCompleteExternalInputQ[
    tpKnownAngle[1, 2]^2/tpOpaque, tpScopeRules] &&
    ConstructiveDiagrams`Private`preparationCompleteExternalInputQ[1/tpOpaque, tpScopeRules] &&
    ConstructiveDiagrams`Private`preparationCompleteExternalInputQ[tpKnownCompleted, tpScopeRules]];

(* ::Section:: *)
(* Joint numerator-shell equations, with process conservation kept explicit. *)
tpJointExternal = Thread[Mass /@ Range[6] -> {tpMass, tpMass, 0, 0, 0, 0}];
tpJointRules = Join[tpJointExternal, {Mass[Multiparticle[1, 3]] -> tpMass,
  Mass[Multiparticle[1, 3, 4]] -> tpMass, Mass[Multiparticle[2, 6]] -> tpMass}];
tpJointDots = ConstructiveDiagrams`Private`preparationJointDotRules[tpJointRules, True];
tpJointNoMC = ConstructiveDiagrams`Private`preparationJointDotRules[tpJointRules, False];
check["adjacent declared cuts remove the middle scalar remainder jointly",
  Together[(MomProd[1, 4] + MomProd[3, 4]) /. tpJointDots] === 0];
check["the middle adjacent-cut identity does not require momentum conservation",
  Together[(MomProd[1, 4] + MomProd[3, 4]) /. tpJointNoMC] === 0];
check["enabled conservation supplies the complementary last forward cut",
  Together[(MomProd[1, 5] + MomProd[3, 5] + MomProd[4, 5]) /. tpJointDots] === 0];
check["disabled conservation does not invent a complementary forward cut",
  Together[(MomProd[1, 5] + MomProd[3, 5] + MomProd[4, 5]) /. tpJointNoMC] =!= 0];
check["joint cut rules have no dependencies on their own pivots",
  FreeQ[Last /@ tpJointDots, Alternatives @@ (First /@ tpJointDots)]];
tpJointInvariant[legs_, mass_] := Total[((Mass[#] /. tpJointExternal)^2) & /@ legs] +
  2 Total[MomProd @@@ Subsets[legs, {2}]] - mass^2;
check["the joint solution certifies every declared original cut equation",
  And @@ (Together[tpJointInvariant[First[#], Last[#]] /. tpJointDots] === 0 & /@
    {{{1, 3}, tpMass}, {{1, 3, 4}, tpMass}, {{2, 6}, tpMass}})];
check["the joint solution certifies every enabled external dot-conservation equation",
  And @@ Table[Together[((Mass[i] /. tpJointExternal)^2 +
    Total[MomProd[i, #] & /@ DeleteCases[Range[6], i]]) /. tpJointDots] === 0, {i, 6}]];
tpJointSparse = Join[Thread[Mass /@ {2, 5, 8, 11, 14, 17} ->
    {tpMass, tpMass, 0, 0, 0, 0}], {Mass[Multiparticle[2, 8]] -> tpMass,
    Mass[Multiparticle[2, 8, 11]] -> tpMass, Mass[Multiparticle[5, 17]] -> tpMass}];
check["joint invariant reduction uses sparse external labels literally",
  Together[(MomProd[2, 11] + MomProd[8, 11]) /.
    ConstructiveDiagrams`Private`preparationJointDotRules[tpJointSparse, True]] === 0];
tpJointContradictory = Join[Thread[Mass /@ Range[4] -> {0, 0, 0, 0}],
  {Mass[Multiparticle[1, 2]] -> 1, Mass[Multiparticle[3, 4]] -> 2}];
check["inconsistent simultaneous cut declarations return a failure",
  FailureQ[ConstructiveDiagrams`Private`preparationJointDotRules[tpJointContradictory, True]]];
check["approximate masses do not certify exact simultaneous cut identities",
  ConstructiveDiagrams`Private`preparationJointDotRules[tpJointRules /. tpMass -> 1.0, True] === {}];
tpJointPole = PropDen[Mom[Multiparticle[1, 3]], tpMass];
tpJointInverse = 2 + MomProd[1, 4] + MomProd[3, 4];
tpJointInput = (1 + MomProd[1, 4] + MomProd[3, 4])/(tpJointPole tpJointInverse);
check["joint numerator equations leave every original propagator and inverse untouched",
  ConstructiveDiagrams`Private`preparationReduceDots[tpJointInput, tpJointDots] ===
    1/(tpJointPole tpJointInverse)];
Do[
  {tpJointInternal, tpJointConserve} = tpJointPolicy;
  tpJointPrepared = PrepareAmplitudes[{{tpJointRules, tpJointInput}}, tpModel,
    "PolePreference" -> None, "InternalOnShell" -> tpJointInternal,
    "MomentumConservation" -> tpJointConserve, "MaxSteps" -> 0,
    "ProgressReporting" -> False, "ReturnDetails" -> True];
  tpJointRow = First[tpJointPrepared["DiagramReports"]];
  check[{"public preparation respects joint-cut policy and inverse scope",
      tpJointInternal, tpJointConserve},
    tpJointRow["Status"] === "PREPARED" &&
      DeleteDuplicates[Cases[tpJointRow["PreparedExpression"], _PropDen, Infinity]] === {tpJointPole} &&
      Together[tpJointRow["PreparedExpression"] -
        If[tpJointInternal, 1/(tpJointPole tpJointInverse), tpJointInput]] === 0],
  {tpJointPolicy, {{True, True}, {True, False}, {False, True}}}];

(* Keep the original first tuple, then examine bounded reciprocal reference
   pairs before enumerating the full Cartesian product. *)
tpReferenceChoices = Table[Table[<|"Leg" -> leg, "Mode" -> "AllSpurious", "Target" -> target|>,
  {target, DeleteCases[Range[3, 6], leg]}], {leg, Range[3, 6]}];
tpReferencePlan = ConstructiveDiagrams`Private`preparationReferenceSelections[tpReferenceChoices, 4];
tpReciprocalSelectionQ[selection_] := With[
  {mapping = AssociationThread[Lookup[selection, "Leg"], Lookup[selection, "Target"]]},
  AllTrue[Keys[mapping], Lookup[mapping, Lookup[mapping, #, None], None] === # &]];
check["bounded reference staging retains the legacy first choice",
  First[tpReferencePlan["Selections"]] === First /@ tpReferenceChoices];
check["the next three four-photon reference seeds are reciprocal pairs",
  AllTrue[Rest[tpReferencePlan["Selections"]], tpReciprocalSelectionQ]];
check["a one-choice reference budget retains exactly the legacy tuple",
  ConstructiveDiagrams`Private`preparationReferenceSelections[tpReferenceChoices, 1]["Selections"] ===
    {First /@ tpReferenceChoices}];
check["the full reference-space size is reported without enumeration",
  tpReferencePlan["AvailableChoices"] === 81];
tpLargeReferenceChoices = Table[Table[<|"Leg" -> leg, "Mode" -> "AllSpurious", "Target" -> target|>,
  {target, 10}], {leg, 10}];
tpLargeReferencePlan = TimeConstrained[
  ConstructiveDiagrams`Private`preparationReferenceSelections[tpLargeReferenceChoices, 3], 2, $TimedOut];
check["a huge Cartesian reference space still has a bounded small plan",
  AssociationQ[tpLargeReferencePlan] && tpLargeReferencePlan["AvailableChoices"] === 10^10 &&
    Length[tpLargeReferencePlan["Selections"]] === 3];

(* Non-search reference callbacks must not evaluate a symbol with an OwnValue
   as a Function parameter in an already-running user kernel. *)
Do[
  tpCallbackPair = If[tpCallbackMode === None,
    {tpJointExternal, SpinorChain[Spinor["Spin", "Angle", 1], Mom[1],
      Spinor["Spin", "Square", 2]]},
    {tpJointExternal, SpinorChain[Spinor["Xi", "Angle", 10], Mom[1],
      Spinor["Helicity", "Square", 3]]/
      SpinorChain[Spinor["Xi", "Angle", 10], Spinor["Helicity", "Angle", 3]]}];
  tpCallbackValid = Block[{ConstructiveDiagrams`Private`value = 23,
      ConstructiveDiagrams`Private`candidate = 29},
    Quiet[Check[
      tpCallbackReport = PrepareAmplitudes[{tpCallbackPair}, tpModel,
        "PolePreference" -> tpCallbackMode, "InternalOnShell" -> False,
        "MaxSteps" -> 0, "ProgressReporting" -> False, "ReturnDetails" -> True];
      AssociationQ[tpCallbackReport] &&
        First[tpCallbackReport["DiagramReports"]]["Status"] === "PREPARED" &&
        ConstructiveDiagrams`Private`residueFiniteQ[tpCallbackReport["Amplitude"]],
      False, {Function::flpar}], {Function::flpar}]];
  check[{"preparation checkpoint callbacks tolerate existing private OwnValues", tpCallbackMode},
    tpCallbackValid], {tpCallbackMode, {None, "IndependentXi"}}];

(* ::Section:: *)
(* Actual mixed six-point diagrams: independent exact own-three-cut charts.
   Each original propagator is stripped before evaluating its numerator.
   Auxiliary reference denominators retain their actual numerical values. *)
tpSixJ = {{0, -1}, {1, 0}}; tpSixEps = -tpSixJ;
tpSixAdj[p_] := {{p[[2, 2]], -p[[1, 2]]}, {-p[[2, 1]], p[[1, 1]]}};
tpSixOwnChart[order_List, mass_, sample_Integer, spins_List] := Module[
  {p1, angles, squares, coefficients, momenta, total, nullAngle, nullSquare, photon, conditions},
  p1 = {{sample + 1, sample - 2},
    {sample + 2, (mass^2 + (sample - 2) (sample + 2))/(sample + 1)}};
  angles = <|3 -> {1, sample + 1}, 4 -> {sample + 2, 1},
    5 -> {2, 2 sample + 3}, 6 -> {sample + 3, 2 sample + 5}|>;
  squares = <||>; coefficients = <|3 -> sample + 2, 4 -> sample - 5,
    5 -> sample + 3, 6 -> -(sample + 1)|>;
  momenta = <|1 -> p1|>; total = p1;
  Do[
    nullAngle = angles[leg];
    nullSquare = coefficients[leg] ({-#[[2]], #[[1]]} &)[tpSixAdj[total].nullAngle];
    photon = Outer[Times, nullAngle, nullSquare];
    AssociateTo[squares, leg -> nullSquare]; AssociateTo[momenta, leg -> photon];
    total = total + photon, {leg, order}];
  AssociateTo[momenta, 2 -> -total];
  Do[AssociateTo[angles, leg -> UnitVector[2, spins[[leg]]]];
    AssociateTo[squares, leg -> tpSixEps.tpSixAdj[momenta[leg]].angles[leg]/mass], {leg, {1, 2}}];
  conditions = Join[{Det[momenta[1]] === mass^2, Det[momenta[2]] === mass^2,
      Total[Values[momenta]] === ConstantArray[0, {2, 2}]},
    (Det[momenta[#]] === 0 &) /@ Range[3, 6],
    (Det[Total[Lookup[momenta, Join[{1}, Take[order, #]]]]] === mass^2 &) /@ Range[3]];
  <|"Momenta" -> momenta, "Angle" -> angles, "Square" -> squares,
    "MassValue" -> mass, "Conditions" -> conditions|>
];
tpSixMatrix[label_Integer, chart_] := chart["Momenta"][label];
tpSixMatrix[Multiparticle[labels__], chart_] := Total[tpSixMatrix[#, chart] & /@ {labels}];
tpSixVector[Spinor[_, kind_, label_], chart_] := chart[kind][label];
tpSixChain[chain_SpinorChain, chart_] := Module[
  {parts = List @@ chain, kind, row, right, matrix},
  kind = parts[[1, 2]]; row = tpSixVector[First[parts], chart];
  If[kind === "Angle", row = row.tpSixJ];
  Do[matrix = vector /. Mom[label_] :> tpSixMatrix[label, chart];
    row = row.If[kind === "Angle", matrix, tpSixAdj[matrix]];
    kind = If[kind === "Angle", "Square", "Angle"], {vector, Rest[Most[parts]]}];
  right = tpSixVector[Last[parts], chart];
  If[Last[parts][[2]] === "Square", right = tpSixEps.right]; row.right
];
tpSixValue[expression_, chart_, originalPropagators_List] := Together[
  expression /. Thread[originalPropagators -> ConstantArray[1, Length[originalPropagators]]] /.
    {tpMass -> chart["MassValue"], tpCharge -> 1} /.
    prop_PropDen :> (Det[tpSixMatrix[prop[[1, 1]], chart]] - prop[[2]]^2) /.
    chain_SpinorChain :> tpSixChain[chain, chart] /.
    MomProd[a_, b_] :> (Det[tpSixMatrix[a, chart] + tpSixMatrix[b, chart]] -
      Det[tpSixMatrix[a, chart]] - Det[tpSixMatrix[b, chart]])/2];
tpSixOrder[pair_] := Module[{sets, nested},
  sets = DeleteDuplicates[(List @@ #[[1, 1]]) & /@
    DeleteDuplicates[Cases[Last[pair], _PropDen, Infinity]]];
  nested = SortBy[If[MemberQ[#, 1], #, Complement[Range[6], #]] & /@ sets, Length];
  If[Length[nested] =!= 3, Return[Missing["ExpectedThreeCuts"]]];
  Join[Complement[nested[[1]], {1}], Complement[nested[[2]], nested[[1]]],
    Complement[nested[[3]], nested[[2]]], Complement[Range[3, 6], nested[[3]]]]
];
Do[
  tpSixParticles = Join[{"e", "E"}, tpSixHelicities];
  tpSixRaw = diagramAmplitudes[CreateDiagrams[tpSixParticles, tpModel]];
  tpSixOrders = tpSixOrder /@ tpSixRaw;
  tpSixWantedOrders = If[Count[tpSixHelicities, "A.-"] === 1,
    {{3, 4, 5, 6}, {3, 6, 4, 5}}, {{3, 4, 5, 6}, {3, 5, 4, 6}}];
  tpSixSelected = (tpSixRaw[[FirstPosition[tpSixOrders, #][[1]]]] &) /@ tpSixWantedOrders;
  check[{"mixed six-point fixtures contain every distinct photon ordering", tpSixHelicities},
    Length[tpSixRaw] === 24 && Sort[tpSixOrders] === Sort[Permutations[Range[3, 6]]]];
  tpSixPrepared = PrepareAmplitudes[tpSixSelected, tpModel, "PolePreference" -> Automatic,
    "MaxSteps" -> 0, "MaxReferenceChoices" -> 4, "ReferenceTimeConstraint" -> 2,
    "TimeConstraint" -> 12, "ProgressReporting" -> False, "ReturnDetails" -> True];
  Do[
    tpSixPair = tpSixSelected[[tpSixIndex]];
    tpSixRow = tpSixPrepared["DiagramReports"][[tpSixIndex]];
    tpSixOutput = tpSixRow["PreparedExpression"];
    tpSixOriginalProps = DeleteDuplicates[Cases[Last[tpSixPair], _PropDen, Infinity]];
    tpSixRules = ConstructiveDiagrams`Private`preparationCanonicalMassRules[First[tpSixPair]];
    tpSixContracted = ConstructiveDiagrams`Private`preparationNumerators[Last[tpSixPair],
      Function[num, ConstructiveDiagrams`Private`preparationReplaceMasses[
        ReduceSpinContractions[num], tpSixRules]]];
    tpSixAssigned = ConstructiveDiagrams`Private`preparationReferences[tpSixContracted,
      tpSixRules, tpModel, tpSixRaw, Automatic, (KeyTake[#, {"Leg", "Mode", "Target"}] &) /@
        tpSixRow["PolePreference"]["Assignments"]];
    check[{"automatic mixed six-point preparation chooses four references", tpSixHelicities, tpSixIndex},
      tpSixRow["Status"] === "PREPARED" && tpSixAssigned["Status"] === "COMPLETE" &&
        Length[tpSixRow["PolePreference"]["Assignments"]] === 4 &&
        FreeQ[tpSixOutput, Spinor["Xi", ___] | _Mass] &&
        ConstructiveDiagrams`Private`residueFiniteQ[tpSixOutput]];
    check[{"mixed six-point preparation preserves all three original propagators", tpSixHelicities, tpSixIndex},
      Length[tpSixOriginalProps] === 3 &&
        ContainsAll[DeleteDuplicates[Cases[tpSixOutput, _PropDen, Infinity]], tpSixOriginalProps]];
    tpSixCertificate = Table[
      tpSixChart = tpSixOwnChart[tpSixWantedOrders[[tpSixIndex]],
        tpSixSample[[1]], tpSixSample[[2]], tpSixSpins];
      And @@ tpSixChart["Conditions"] &&
        tpSixValue[tpSixAssigned["Expression"] - tpSixOutput,
          tpSixChart, tpSixOriginalProps] === 0,
      {tpSixSample, {{1, 1}, {3/7, 2}}}, {tpSixSpins, Tuples[{1, 2}, 2]}];
    check[{"mixed six-point preparation has an independent same-reference three-cut certificate",
        tpSixHelicities, tpSixIndex}, And @@ Flatten[tpSixCertificate]],
    {tpSixIndex, 2}],
  {tpSixHelicities, {{"A.+", "A.+", "A.+", "A.-"}, {"A.+", "A.+", "A.-", "A.-"}}}];

(* ::Section:: *)
(* Four massive fermions and two photons. Conservation alternatives must be
   ranked after the declared external massless momenta have been factored. *)
tpFourExternal = Thread[Mass /@ Range[6] -> {tpMass, tpMass, tpMass, tpMass, 0, 0}];
tpFourOpen = SpinorChain[Spinor["Spin", "Square", 4], Mom[Multiparticle[1, 2, 3]],
  Spinor["Spin", "Angle", 1]];
tpFourConserved = ConstructiveDiagrams`Private`preparationConservedMomentumChains[
  tpFourOpen, tpFourExternal, True, {}, False, True];
check["external massless factorization participates in ranking an open-chain complement",
  FreeQ[Cases[tpFourConserved, _SpinorChain, Infinity], _Mom] &&
    SpinorExpressionComplexity[tpFourConserved] <= 3];
check["ordinary reduction does not infer the four-fermion process conservation equation",
  !FreeQ[Cases[ReduceSpinorProducts[tpFourOpen], _SpinorChain, Infinity], _Mom]];
check["disabled conservation retains the original open composite chain",
  ConstructiveDiagrams`Private`preparationConservedMomentumChains[
    tpFourOpen, tpFourExternal, False, {}, False, True] === tpFourOpen];
tpFourPole = PropDen[Mom[Multiparticle[1, 2, 3]], tpMass];
tpFourInverse = 3 + tpFourOpen;
check["complement ranking protects original propagators and ordinary inverse factors",
  ConstructiveDiagrams`Private`preparationConservedMomentumChains[
    tpFourOpen/(tpFourPole tpFourInverse), tpFourExternal, True, {}, False, True] ===
      tpFourConserved/(tpFourPole tpFourInverse)];
check["complement ranking protects powers with an unknown exponent",
  ConstructiveDiagrams`Private`preparationConservedMomentumChains[
    tpFourOpen tpFourInverse^tpFourUnknownPower, tpFourExternal, True, {}, False, True] ===
      tpFourConserved tpFourInverse^tpFourUnknownPower];
tpFourUndeclared = tpFourOpen /. Mom[Multiparticle[1, 2, 3]] -> Mom[Multiparticle[1, 2, 99]];
check["conservation does not complement a momentum containing an undeclared leg",
  ConstructiveDiagrams`Private`preparationConservedMomentumChains[
    tpFourUndeclared, tpFourExternal, True, {}, False, True] === tpFourUndeclared];
tpFourCycles = Table[{i, 8 + 3 i}, {i, 6}];
tpFourSparseOpen = Fold[PermuteParticles, tpFourOpen, tpFourCycles];
tpFourSparseRules = Fold[PermuteParticles, tpFourExternal, tpFourCycles];
check["completed complement ranking is covariant under sparse typed relabeling",
  ConstructiveDiagrams`Private`preparationConservedMomentumChains[
    tpFourSparseOpen, tpFourSparseRules, True, {}, False, True] ===
      Fold[PermuteParticles, tpFourConserved, tpFourCycles]];
check["sparse labels do not enable conservation when its option is disabled",
  ConstructiveDiagrams`Private`preparationConservedMomentumChains[
    tpFourSparseOpen, tpFourSparseRules, False, {}, False, True] === tpFourSparseOpen];
tpFourHelicityChain = SpinorChain[tpSpin["Angle", 6], Mom[Multiparticle[1, 2, 3]],
  tpSpin["Square", 5]];
tpFourHelicityExpected = ReduceSpinorProducts[-SpinorChain[tpSpin["Angle", 6], Mom[4],
  tpSpin["Square", 5]]];
check["selective reference-chain normalization preserves the open massive chain",
  ConstructiveDiagrams`Private`preparationConservedMomentumChains[
    tpFourOpen tpFourHelicityChain, tpFourExternal, True, {}, "HelicityEndpoints"] ===
      tpFourOpen tpFourHelicityExpected];
tpFourHat = tpFourHelicityChain /. Mom[Multiparticle[1, 2, 3]] -> MomHat[Multiparticle[1, 2, 3]];
check["reference-chain conservation does not extend the new route to hatted momenta",
  ConstructiveDiagrams`Private`preparationConservedMomentumChains[
    tpFourHat, tpFourExternal, True, {}, "HelicityEndpoints"] === tpFourHat];
Do[
  tpFourPolicy = PrepareAmplitudes[{{Append[tpFourExternal,
      Mass[Multiparticle[1, 5]] -> tpMass], tpFourOpen + MomProd[1, 5]}}, tpModel,
    "PolePreference" -> None, "InternalOnShell" -> False,
    "MomentumConservation" -> tpFourMC, "MaxSteps" -> 0,
    "TimeConstraint" -> 20, "ProgressReporting" -> False, "ReturnDetails" -> True];
  tpFourPolicyRow = First[tpFourPolicy["DiagramReports"]];
  tpFourPolicyExpression = tpFourPolicyRow["PreparedExpression"];
  check[{"external complement reduction does not enable a declared internal shell", tpFourMC},
    tpFourPolicyRow["Status"] === "PREPARED" &&
      tpFourPolicyRow["InternalOnShellChannels"] === {} &&
      tpFourPolicyRow["JointOnShellDotReplacements"] === {} &&
      !FreeQ[tpFourPolicyExpression, MomProd[1, 5]] &&
      If[tpFourMC,
        FreeQ[Cases[tpFourPolicyExpression, _SpinorChain, Infinity], _Mom],
        !FreeQ[Cases[tpFourPolicyExpression, _SpinorChain, Infinity], _Mom]]],
  {tpFourMC, {True, False}}];

(* Root two oriented charged lines at their unique null exchanged photon.
   Every external photon is chosen orthogonal to its adjacent charged line.
   This chart construction uses matrices, not a symbolic preparation rule. *)
tpFourLineData[diagram_] := Module[
  {graph = diagram["Graph"], external = diagram["ExternalParticles"], edge, lines},
  edge = SelectFirst[Keys[diagram["Propagators"]],
    diagram["Propagators"][#]["Mass"] === 0 &&
      diagram["Propagators"][#]["Spin2"] === 2 &];
  lines = diagramFermionLines[diagram];
  Map[Function[ends, Module[{oriented, path, sequence},
    oriented = If[external[[First[ends]]] === "e", ends, Reverse[ends]];
    path = FindShortestPath[graph, First[oriented], Last[oriented]];
    sequence = Table[If[MemberQ[List @@ edge, path[[j]]], "Exchange",
      First[Complement[AdjacencyList[graph, path[[j]]], path[[{j - 1, j + 1}]]]]],
      {j, 2, Length[path] - 1}];
    <|"Endpoints" -> oriented, "Sequence" -> sequence|>]], lines]
];
tpFourTopology[diagram_] := Module[{lines = tpFourLineData[diagram], owners, emitters, position},
  owners = Table[SelectFirst[Range[2], MemberQ[lines[[#]]["Sequence"], leg] &], {leg, {5, 6}}];
  emitters = MapThread[Function[{owner, leg},
    position = FirstPosition[lines[[owner]]["Sequence"], "Exchange"][[1]];
    If[FirstPosition[lines[[owner]]["Sequence"], leg][[1]] < position,
      First[lines[[owner]]["Endpoints"]], Last[lines[[owner]]["Endpoints"]]]],
    {owners, {5, 6}}];
  Which[First[owners] =!= Last[owners], "SeparateLines",
    First[emitters] === Last[emitters], "SameEndpoint", True, "OppositeEndpoints"]
];
tpFourOwnChart[diagram_, mass_, sample_Integer] := Module[
  {lines = tpFourLineData[diagram], q, qa, angles = <||>, squares = <||>, momenta = <||>,
   exchangeAngle = {1, sample + 1}, exchangeSquare = {sample + 2, 1},
   u, v, w, z, current, sequence, position, ends, emit, coefficient, lambda, tilde,
   photon, lineNumber, conditions},
  q = Outer[Times, exchangeAngle, exchangeSquare];
  emit[leg_Integer, sign_Integer] := (
    lambda = {sample + leg + 1, sample^2 + leg^2 + 3};
    coefficient = (sample + lineNumber + leg)/(sample + 2 leg + 1);
    tilde = coefficient ({-#[[2]], #[[1]]} &)[tpSixAdj[current].lambda];
    photon = Outer[Times, lambda, tilde];
    AssociateTo[angles, leg -> lambda]; AssociateTo[squares, leg -> tilde];
    AssociateTo[momenta, leg -> photon]; current = current + sign photon);
  Do[
    {ends, sequence} = Lookup[lines[[lineNumber]], {"Endpoints", "Sequence"}];
    position = FirstPosition[sequence, "Exchange"][[1]];
    u = sample + 2 lineNumber + 2; v = sample^2 + 3 lineNumber + 4;
    w = (q[[1, 1]] mass^2 - u v q[[2, 1]] + u^2 q[[2, 2]])/(u q[[1, 2]] - q[[1, 1]] v);
    z = (v q[[2, 1]] + w q[[1, 2]] - u q[[2, 2]])/q[[1, 1]];
    qa = {{u, v}, {w, z}};
    current = qa;
    Do[emit[leg, -1], {leg, Reverse[Take[sequence, position - 1]]}];
    AssociateTo[momenta, First[ends] -> current];
    current = qa + If[lineNumber === 1, q, -q];
    Do[emit[leg, 1], {leg, Drop[sequence, position]}];
    AssociateTo[momenta, Last[ends] -> -current], {lineNumber, 2}];
  Do[
    lambda = {tpFourSpin[leg, 1], tpFourSpin[leg, 2]};
    AssociateTo[angles, leg -> lambda];
    AssociateTo[squares, leg -> tpSixEps.tpSixAdj[momenta[leg]].lambda/mass], {leg, 4}];
  conditions = Join[{Total[Values[momenta]] === ConstantArray[0, {2, 2}]},
    Table[Det[momenta[leg]] === If[leg <= 4, mass^2, 0], {leg, 6}],
    Table[Det[Total[Lookup[momenta, List @@ prop["Index"]]]] ===
      If[prop["Spin2"] === 1, mass^2, 0], {prop, Values[diagram["Propagators"]]}]];
  <|"Momenta" -> momenta, "Angle" -> angles, "Square" -> squares,
    "MassValue" -> mass, "Conditions" -> conditions|>
];

Do[
  tpFourParticles = {"e", "E", "E", "e", "A.+", tpFourSecondPhoton};
  tpFourDiagrams = CreateDiagrams[tpFourParticles, tpModel];
  tpFourRaw = diagramAmplitudes[tpFourDiagrams];
  tpFourRawHash = Hash[tpFourRaw, "SHA256"];
  tpFourClasses = tpFourTopology /@ tpFourDiagrams;
  tpFourSelectedIndices = (FirstPosition[tpFourClasses, #][[1]] &) /@
    {"SameEndpoint", "OppositeEndpoints", "SeparateLines"};
  tpFourSelected = tpFourRaw[[tpFourSelectedIndices]];
  check[{"four-fermion two-photon fixtures cover all three radiation topologies", tpFourSecondPhoton},
    Length[tpFourRaw] === 40 && KeySort[Counts[tpFourClasses]] ===
      KeySort[<|"SameEndpoint" -> 16, "OppositeEndpoints" -> 8, "SeparateLines" -> 16|>]];
  tpFourPrepared = PrepareAmplitudes[tpFourSelected, tpModel,
    "PolePreference" -> "AllSpurious", "MaxSteps" -> 0,
    "MaxReferenceChoices" -> 4, "ReferenceTimeConstraint" -> 5,
    "TimeConstraint" -> 20, "ProgressReporting" -> False, "ReturnDetails" -> True];
  Do[
    tpFourIndex = tpFourSelectedIndices[[tpFourPosition]];
    tpFourPair = tpFourSelected[[tpFourPosition]];
    tpFourRow = tpFourPrepared["DiagramReports"][[tpFourPosition]];
    tpFourExpression = tpFourRow["PreparedExpression"];
    tpFourProps = DeleteDuplicates[Cases[Last[tpFourPair], _PropDen, Infinity]];
    tpFourRules = ConstructiveDiagrams`Private`preparationCanonicalMassRules[First[tpFourPair]];
    tpFourContracted = ConstructiveDiagrams`Private`preparationNumerators[Last[tpFourPair],
      Function[num, ConstructiveDiagrams`Private`preparationReplaceMasses[
        ReduceSpinContractions[num], tpFourRules]]];
    tpFourReferences = ConstructiveDiagrams`Private`preparationReferences[tpFourContracted,
      tpFourRules, tpModel, tpFourRaw, "AllSpurious",
      (KeyTake[#, {"Leg", "Mode", "Target"}] &) /@ tpFourRow["PolePreference"]["Assignments"]];
    check[{"four-fermion two-photon preparation completes a finite reference-free result",
        tpFourSecondPhoton, tpFourClasses[[tpFourIndex]]},
      tpFourRow["Status"] === "PREPARED" && tpFourReferences["Status"] === "COMPLETE" &&
        Length[tpFourRow["PolePreference"]["Assignments"]] === 2 &&
        FreeQ[tpFourExpression, Spinor["Xi", ___] | _Mass | _SpinorChainReduced | _SpinorTraceReduced] &&
        ConstructiveDiagrams`Private`residueFiniteQ[tpFourExpression]];
    check[{"four-fermion two-photon preparation retains all three original propagators",
        tpFourSecondPhoton, tpFourClasses[[tpFourIndex]]},
      Length[tpFourProps] === 3 &&
        ContainsAll[DeleteDuplicates[Cases[tpFourExpression, _PropDen, Infinity]], tpFourProps]];
    (* These bounds were measured with the public preparation budgets above.
       A more compact manually derived current is not substituted into it. *)
    tpFourQualityLimit = If[tpFourSecondPhoton === "A.+",
      {8, 56, 59}[[tpFourPosition]], {56, 58, 57}[[tpFourPosition]]];
    check[{"four-fermion two-photon preparation retains its measured bounded quality",
        tpFourSecondPhoton, tpFourClasses[[tpFourIndex]]},
      SpinorExpressionComplexity[tpFourExpression] <= tpFourQualityLimit &&
        If[tpFourSecondPhoton === "A.+" && tpFourPosition === 1,
          FreeQ[Cases[tpFourExpression, _SpinorChain, Infinity], _Mom], True]];
    tpFourCertificate = Table[
      tpFourChart = tpFourOwnChart[tpFourDiagrams[[tpFourIndex]],
        tpFourSample[[1]], tpFourSample[[2]]];
      And @@ tpFourChart["Conditions"] &&
        tpSixValue[tpFourReferences["Expression"] - tpFourExpression,
          tpFourChart, tpFourProps] === 0,
      {tpFourSample, {{1, 1}, {3/7, 2}}}];
    check[{"four-fermion two-photon numerators have exact coherent sixteen-spin certificates",
        tpFourSecondPhoton, tpFourClasses[[tpFourIndex]]}, And @@ tpFourCertificate],
    {tpFourPosition, Length[tpFourSelected]}];
  check[{"four-fermion two-photon preparation leaves every raw diagram pair unchanged", tpFourSecondPhoton},
    Hash[tpFourRaw, "SHA256"] === tpFourRawHash],
  {tpFourSecondPhoton, {"A.+", "A.-"}}];

(* An additional normal-budget call protects the established alternating
   four-photon result while new optional conservation routes are explored. *)
tpBalancedOrder = {3, 5, 4, 6};
tpBalancedPair = SelectFirst[tpSixRaw, tpSixOrder[#] === tpBalancedOrder &];
tpBalancedHash = Hash[tpBalancedPair, "SHA256"];
tpBalancedPrepared = PrepareAmplitudes[{tpBalancedPair}, tpModel,
  "PolePreference" -> "AllSpurious", "MaxSteps" -> 0,
  "MaxReferenceChoices" -> 64, "ReferenceTimeConstraint" -> 5,
  "TimeConstraint" -> 20, "ProgressReporting" -> False, "ReturnDetails" -> True];
tpBalancedRow = First[tpBalancedPrepared["DiagramReports"]];
tpBalancedExpression = tpBalancedRow["PreparedExpression"];
tpBalancedProps = DeleteDuplicates[Cases[Last[tpBalancedPair], _PropDen, Infinity]];
tpBalancedRules = ConstructiveDiagrams`Private`preparationCanonicalMassRules[First[tpBalancedPair]];
tpBalancedContracted = ConstructiveDiagrams`Private`preparationNumerators[Last[tpBalancedPair],
  Function[num, ConstructiveDiagrams`Private`preparationReplaceMasses[
    ReduceSpinContractions[num], tpBalancedRules]]];
tpBalancedReferences = ConstructiveDiagrams`Private`preparationReferences[tpBalancedContracted,
  tpBalancedRules, tpModel, tpSixRaw, "AllSpurious",
  (KeyTake[#, {"Leg", "Mode", "Target"}] &) /@ tpBalancedRow["PolePreference"]["Assignments"]];
tpBalancedCertificate = Table[
  tpBalancedChart = tpSixOwnChart[tpBalancedOrder, tpBalancedSample[[1]], tpBalancedSample[[2]],
    tpBalancedSpins];
  And @@ tpBalancedChart["Conditions"] &&
    tpSixValue[tpBalancedReferences["Expression"] - tpBalancedExpression,
      tpBalancedChart, tpBalancedProps] === 0,
  {tpBalancedSample, {{1, 1}, {3/7, 2}}}, {tpBalancedSpins, Tuples[{1, 2}, 2]}];
check["normal-budget alternating four-photon preparation retains its established compact result",
  tpBalancedRow["Status"] === "PREPARED" &&
    ConstructiveDiagrams`Private`residueFiniteQ[tpBalancedExpression] &&
    SpinorExpressionComplexity[tpBalancedExpression] <= 52 &&
    Length[tpBalancedProps] === 3 &&
    ContainsAll[DeleteDuplicates[Cases[tpBalancedExpression, _PropDen, Infinity]], tpBalancedProps] &&
    Hash[tpBalancedPair, "SHA256"] === tpBalancedHash && And @@ Flatten[tpBalancedCertificate]];

(* ::Section:: *)
(* Six massive external fermions: two locally sewn vector currents.
   Each chart has six independent two-component spinors, so equality of the
   resulting polynomial checks all 64 spin components without a spin sum. *)
(* Independent rational three-cut charts for the six-fermion QED tree. *)
tpSFAdj[p_] := {{p[[2,2]],-p[[1,2]]},{-p[[2,1]],p[[1,1]]}};
tpSFJ={{0,-1},{1,0}};tpSFEps=-tpSFJ;
tpSFLineData[diagram_] := Module[
 {lines,graph=diagram["Graph"],external=diagram["ExternalParticles"],paths,
  massive,centralIndex,central,vertices,outer,photon,otherVertex,indices},
 lines=Map[If[external[[First[#]]]==="e",#,Reverse[#]]&,
   ConstructiveDiagrams`diagramFermionLines[diagram]];
 paths=FindShortestPath[graph,First[#],Last[#]]& /@ lines;
 massive=SelectFirst[Keys[diagram["Propagators"]],diagram["Propagators"][#]["Spin2"]===1&];
 centralIndex=SelectFirst[Range[3],ContainsAll[paths[[#]],List@@massive]&];
 central=lines[[centralIndex]];vertices=paths[[centralIndex,{2,-2}]];
 indices=Table[
   photon=SelectFirst[Keys[diagram["Propagators"]],
     diagram["Propagators"][#]["Spin2"]===2 && MemberQ[List@@#,vertex]&];
   otherVertex=First[DeleteCases[List@@photon,vertex]];
   SelectFirst[DeleteCases[Range[3],centralIndex],MemberQ[paths[[#]],otherVertex]&],
   {vertex,vertices}];
 <|"CentralLine"->central,"OuterLines"->lines[[indices]],"Lines"->lines|>
];
tpSFOwnChart[diagram_,mass_,sample_Integer,spins_List:{}] := Module[
 {top=tpSFLineData[diagram],p,q,photons,angles=<||>,squares=<||>,momenta=<||>,
  left,right,lambda,tilde,split,mat,u,v,w,z,den,prop,conditions,tag=Unique["tpSFSplit"]},
 p={{sample+1,sample-2},{sample+2,(mass^2+(sample-2)(sample+2))/(sample+1)}};
 photons=Table[
   lambda=If[j===1,{1,sample+1},{sample+2,1}];
   tilde=If[j===1,sample+1,-sample-3] ({-#[[2]],#[[1]]}&)[tpSFAdj[p].lambda];
   Outer[Times,lambda,tilde],{j,2}];
 AssociateTo[momenta,top["CentralLine"][[1]]->p-photons[[1]]];
 AssociateTo[momenta,top["CentralLine"][[2]]->-p-photons[[2]]];
 split[null_,j_] := Catch[
   Do[
     u=sample+2j+2+offset;v=sample^2+3j+4+offset^2;
     den=u null[[1,2]]-null[[1,1]]v;
     If[den===0 || null[[1,1]]===0,Continue[]];
     w=(null[[1,1]]mass^2-u v null[[2,1]]+u^2 null[[2,2]])/den;
     z=(v null[[2,1]]+w null[[1,2]]-u null[[2,2]])/null[[1,1]];
     mat={{u,v},{w,z}};
     If[Det[mat]===mass^2 && Det[null-mat]===mass^2,Throw[mat,tag]],
     {offset,0,8}];Failure["CannotSplitNullCurrent",<||>],tag];
 Do[
   mat=split[photons[[j]],j];
   If[FailureQ[mat],Return[mat]];
   AssociateTo[momenta,top["OuterLines"][[j,1]]->mat];
   AssociateTo[momenta,top["OuterLines"][[j,2]]->photons[[j]]-mat],{j,2}];
 Do[
   lambda=If[Length[spins]===6,UnitVector[2,spins[[leg]]],{tpSFSpin[leg,1],tpSFSpin[leg,2]}];
   AssociateTo[angles,leg->lambda];
   AssociateTo[squares,leg->tpSFEps.tpSFAdj[momenta[leg]].lambda/mass],{leg,6}];
 conditions=Join[{Total[Values[momenta]]===ConstantArray[0,{2,2}]},
   Table[Det[momenta[leg]]===mass^2,{leg,6}],
   Table[Det[Total[Lookup[momenta,List@@prop["Index"]]]]===
       If[prop["Spin2"]===1,mass^2,0],{prop,Values[diagram["Propagators"]]}]];
 <|"Momenta"->momenta,"Angle"->angles,"Square"->squares,"MassValue"->mass,
   "Conditions"->conditions,"Topology"->top|>
];
tpSFMatrix[label_Integer,chart_]:=chart["Momenta"][label];
tpSFMatrix[Multiparticle[legs__Integer],chart_]:=Total[tpSFMatrix[#,chart]& /@ {legs}];
tpSFVector[Spinor[_,kind_,leg_Integer],chart_]:=chart[kind][leg];
tpSFChain[chain_SpinorChain,chart_] := Module[{parts=List@@chain,kind,row,right,matrix},
 kind=parts[[1,2]];row=tpSFVector[First[parts],chart];If[kind==="Angle",row=row.tpSFJ];
 Do[matrix=vector/.Mom[label_]:>tpSFMatrix[label,chart];
   row=row.If[kind==="Angle",matrix,tpSFAdj[matrix]];
   kind=If[kind==="Angle","Square","Angle"],{vector,Rest[Most[parts]]}];
 right=tpSFVector[Last[parts],chart];If[Last[parts][[2]]==="Square",right=tpSFEps.right];row.right];
tpSFOwnValue[expression_,chart_,props_List,parameterRules_List] := Together[
 expression/.Thread[props->ConstantArray[1,Length[props]]]/.parameterRules/.
   denominator_PropDen:>(Det[tpSFMatrix[denominator[[1,1]],chart]]-denominator[[2]]^2)/.
   chain_SpinorChain:>tpSFChain[chain,chart]/.
   MomProd[a_,b_]:>(Det[tpSFMatrix[a,chart]+tpSFMatrix[b,chart]]-
     Det[tpSFMatrix[a,chart]]-Det[tpSFMatrix[b,chart]])/2];

(* Independent external-shell points with no total-momentum or internal-cut
   condition, for checking ordinary identities separately from preparation. *)
tpSFOffcutChart[mass_,sample_Integer] := Module[
 {momenta=<||>,angles=<||>,squares=<||>,u,v,w,p,angle},
 Do[
   u=sample+leg+1;v=sample-2leg;w=sample+leg^2;
   p={{u,v},{w,(mass^2+v w)/u}};
   angle={tpSFSpin[leg,1],tpSFSpin[leg,2]};
   AssociateTo[momenta,leg->p];AssociateTo[angles,leg->angle];
   AssociateTo[squares,leg->tpSFEps.tpSFAdj[p].angle/mass],{leg,6}];
 <|"Momenta"->momenta,"Angle"->angles,"Square"->squares,
   "MassValue"->mass,"Conditions"->Table[Det[momenta[leg]]===mass^2,{leg,6}],
   "ConservesMomentum"->(Total[Values[momenta]]===ConstantArray[0,{2,2}])|>
];

tpSFParticles = {"e", "E", "e", "E", "e", "E"};
tpSFDiagrams = CreateDiagrams[tpSFParticles, tpModel];
tpSFRaw = diagramAmplitudes[tpSFDiagrams];
tpSFRawHash = Hash[tpSFRaw, "SHA256"];
tpSFTopology = tpSFLineData /@ tpSFDiagrams;
tpSFSigns = ConstructiveDiagrams`diagramFermionSign /@ tpSFDiagrams;
check["six-fermion fixtures cover all 36 diagrams and both matching signs",
  Length[tpSFDiagrams] === 36 && Length[tpSFRaw] === 36 &&
    Count[tpSFSigns, 1] === 18 && Count[tpSFSigns, -1] === 18];
tpSFFirstMatching = Select[Range[Length[tpSFDiagrams]],
  Sort[tpSFTopology[[#]]["Lines"]] === {{1, 2}, {3, 4}, {5, 6}} &];
tpSFCentralGroups = GatherBy[tpSFFirstMatching, tpSFTopology[[#]]["CentralLine"] &];
check["one six-fermion matching has both attachment orders on each central line",
  Length[tpSFFirstMatching] === 6 && Length[tpSFCentralGroups] === 3 &&
    Sort[Length /@ tpSFCentralGroups] === {2, 2, 2} &&
    Length[DeleteDuplicates[KeyTake[tpSFTopology[[#]], {"CentralLine", "OuterLines"}] & /@
      tpSFFirstMatching]] === 6];
tpSFNegative = FirstPosition[tpSFSigns, -1][[1]];
tpSFRepresentativeIndices = Join[First /@ tpSFCentralGroups, {tpSFNegative}];
tpSFSelected = tpSFRaw[[tpSFRepresentativeIndices]];
tpSFRSC = ConstructiveDiagrams`Private`preparationNumerators[Last[#],
    Function[num, ConstructiveDiagrams`Private`preparationReplaceMasses[
      ReduceSpinContractions[num], First[#]]]] & /@ tpSFRaw;
tpSFPrepared = PrepareAmplitudes[tpSFSelected, tpModel,
  "PolePreference" -> Automatic, "MaxSteps" -> 0, "TimeConstraint" -> 20,
  "ProgressReporting" -> False, "ReturnDetails" -> True];
Do[
  tpSFActualIndex = tpSFRepresentativeIndices[[tpSFItem]];
  tpSFPair = tpSFRaw[[tpSFActualIndex]];
  tpSFRow = tpSFPrepared["DiagramReports"][[tpSFItem]];
  tpSFOutput = tpSFRow["PreparedExpression"];
  tpSFProps = DeleteDuplicates[Cases[Last[tpSFPair], _PropDen, Infinity]];
  tpSFTerms = If[Head[Expand[tpSFOutput]] === Plus, List @@ Expand[tpSFOutput], {tpSFOutput}];
  check[{"six-fermion preparation retains a compact complete numerator", tpSFActualIndex},
    tpSFRow["Status"] === "PREPARED" && SpinorExpressionComplexity[tpSFOutput] <= 88 &&
      Length[tpSFTerms] <= 24 && ConstructiveDiagrams`Private`residueFiniteQ[tpSFOutput] &&
      FreeQ[tpSFOutput, _Mass | Spinor["Xi", ___] | _xFactor | _xTildeFactor |
        Spinor[_, _, _Multiparticle] | Spinor[_, _, _, _Multiparticle, _]]];
  check[{"six-fermion preparation keeps exactly its three original propagators", tpSFActualIndex},
    Length[tpSFProps] === 3 &&
      Sort[DeleteDuplicates[Cases[tpSFOutput, _PropDen, Infinity]]] === Sort[tpSFProps]];
  tpSFCertificate = Table[
    tpSFChart = tpSFOwnChart[tpSFDiagrams[[tpSFActualIndex]],
      tpSFSample[[1]], tpSFSample[[2]], {}];
    And @@ tpSFChart["Conditions"] &&
      AllTrue[Range[6], (Together /@ (
        tpSFChart["Momenta"][#].tpSFEps.tpSFChart["Square"][#] +
          tpSFSample[[1]] tpSFChart["Angle"][#])) === {0, 0} &] &&
      tpSFOwnValue[tpSFRSC[[tpSFActualIndex]] - tpSFOutput, tpSFChart, tpSFProps,
        {tpMass -> tpSFSample[[1]], tpCharge -> 1}] === 0 &&
      tpSFOwnValue[tpSFRSC[[tpSFActualIndex]], tpSFChart, tpSFProps,
        {tpMass -> tpSFSample[[1]], tpCharge -> 1}] =!= 0,
    {tpSFSample, {{1, 1}, {3/7, 2}}}];
  check[{"six-fermion preparation has a nonvacuous independent 64-component three-cut certificate",
      tpSFActualIndex}, And @@ tpSFCertificate],
  {tpSFItem, Length[tpSFRepresentativeIndices]}];
check["six-fermion preparation leaves the complete raw diagram list unchanged",
  Hash[tpSFRaw, "SHA256"] === tpSFRawHash];

(* Ordinary preparation must not borrow the declared internal photon/electron
   cuts when disabled. This starts from the algebraic contracted numerator;
   the comparison is independent of how the diagram was originally sewn. *)
tpSFPolicyIndex = First[tpSFFirstMatching];
tpSFPolicyRaw = tpSFRaw[[tpSFPolicyIndex]];
tpSFPolicyPair = {First[tpSFPolicyRaw], tpSFRSC[[tpSFPolicyIndex]]};
tpSFPolicyProps = DeleteDuplicates[Cases[Last[tpSFPolicyRaw], _PropDen, Infinity]];
tpSFPolicyReport = PrepareAmplitudes[{tpSFPolicyPair}, tpModel,
  "PolePreference" -> None, "InternalOnShell" -> False, "MomentumConservation" -> False,
  "MaxSteps" -> 0, "TimeConstraint" -> 20, "ProgressReporting" -> False, "ReturnDetails" -> True];
tpSFPolicyRow = First[tpSFPolicyReport["DiagramReports"]];
tpSFOffcutCertificate = Table[
  tpSFOffcut = tpSFOffcutChart[tpSFSample[[1]], tpSFSample[[2]]];
  And @@ tpSFOffcut["Conditions"] && !tpSFOffcut["ConservesMomentum"] &&
    AllTrue[tpSFPolicyProps, Together[Det[tpSFMatrix[#[[1, 1]], tpSFOffcut]] -
      (#[[2]] /. tpMass -> tpSFSample[[1]])^2] =!= 0 &] &&
    tpSFOwnValue[Last[tpSFPolicyPair] - tpSFPolicyRow["PreparedExpression"],
      tpSFOffcut, tpSFPolicyProps, {tpMass -> tpSFSample[[1]], tpCharge -> 1}] === 0,
  {tpSFSample, {{1, 1}, {3/7, 2}}}];
check["disabled cut and conservation policies still allow ordinary six-fermion sum reduction",
  tpSFPolicyRow["Status"] === "PREPARED" &&
    SpinorExpressionComplexity[tpSFPolicyRow["PreparedExpression"]] <= 88 &&
    tpSFPolicyRow["InternalOnShellChannels"] === {} &&
    tpSFPolicyRow["InternalOnShellConditions"] === {} &&
    tpSFPolicyRow["JointOnShellDotReplacements"] === {} && And @@ tpSFOffcutCertificate];
check["ordinary six-fermion preparation retains protected propagators with cuts disabled",
  Sort[DeleteDuplicates[Cases[tpSFPolicyRow["PreparedExpression"], _PropDen, Infinity]]] ===
    Sort[tpSFPolicyProps]];

If[checks =!= 439, AppendTo[failures, {"UnexpectedCheckCount", checks, 439}]];
Print[ToString[checks] <> " thesis-preparation checks; " <> ToString[Length[failures]] <> " failures."];
If[failures =!= {}, Print[failures]];
Print["HEPCAT_TEST_COMPLETED:thesis-preparation-tests.wl"];
Quit[If[failures === {}, 0, 1]];
