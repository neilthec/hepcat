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
    <|"ReferenceSpinors" -> "AllPhysical", "MaxSteps" -> 0,
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
  "ReferenceSpinors" -> "AllSpurious", "ProgressReporting" -> False], 120, $TimedOut];
Do[
  check[{"five-point AllSpurious retains at most two terms and complexity eighteen", tpIndex},
    ListQ[tpFivePrepared] && Length[tpFivePrepared] === 6 &&
      With[{expression = Last[tpFivePrepared[[tpIndex]]]},
        If[Head[expression] === Plus, Length[expression], 1] <= 2 &&
          SpinorExpressionComplexity[expression] <= 18]],
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
  "ReferenceSpinors" -> "AllPhysical", "ProgressReporting" -> False], 90, $TimedOut];
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

If[checks =!= 180, AppendTo[failures, {"UnexpectedCheckCount", checks, 180}]];
Print[ToString[checks] <> " thesis-preparation checks; " <> ToString[Length[failures]] <> " failures."];
If[failures =!= {}, Print[failures]];
Print["HEPCAT_TEST_COMPLETED:thesis-preparation-tests.wl"];
Quit[If[failures === {}, 0, 1]];
