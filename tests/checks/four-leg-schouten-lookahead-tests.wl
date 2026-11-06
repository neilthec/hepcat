(* A bounded ordinary lookahead may grow before a completed contraction shrinks. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root, "source", "HEPCAT.wl"}]];
$HistoryLength = 0; checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, assertion_] := (checks++; If[!TrueQ[assertion],
  AppendTo[failures, name]; Print["FAIL: ", name]]);
slRules = {Mass[1] -> slM1, Mass[2] -> slM2, Mass[3] -> 0, Mass[4] -> 0};
slSpinor[type_, leg_] := Spinor[If[leg <= 2, "Spin", "Helicity"], type, leg];
slA[i_, j_] := SpinorChain[slSpinor["Angle", i], slSpinor["Angle", j]];
slS[i_, j_] := SpinorChain[slSpinor["Square", i], slSpinor["Square", j]];
slX = SpinorChain[slSpinor["Square", 3], Mom[2], slSpinor["Angle", 4]];
slNumerator = slS[3, 4] slA[1, 4] slA[2, 4] - slA[3, 4] slS[1, 3] slS[2, 3] +
  (slA[1, 2] + slS[1, 2]) slX;
slCompact = (slM1 + slM2) (slS[1, 3] slA[2, 4] + slS[2, 3] slA[1, 4]);
slDenominator = PropDen[Mom[Multiparticle[1, 3]], slMediator, slWidth] *
  PropDen[Mom[Multiparticle[1, 4]], slOtherMediator];
slInput = slCoupling^2 slX slNumerator/(slM1 slDenominator);
slTarget = slCoupling^2 slX slCompact/(slM1 slDenominator);
slResult = ConstructiveDiagrams`Private`rspFourLegSchoutenLookahead[slInput, slRules, True, 2];
check["a general two-step external identity contracts an unequal-mass numerator",
  slResult["Status"] === "IMPROVED" && Together[slResult["Amplitude"] - slTarget] === 0 &&
    SpinorExpressionComplexity[slResult["Amplitude"]] < SpinorExpressionComplexity[slInput]];
check["physical inverse factors including a finite width remain unchanged",
  Sort[DeleteDuplicates[Cases[slResult["Amplitude"], _PropDen, Infinity]]] ===
    Sort[DeleteDuplicates[Cases[slInput, _PropDen, Infinity]]]];
slReplayNormal[before_, record_] := Module[{working = before},
  Do[working = Expand[working /. step["ChainRules"]];
    If[step["Operation"] === "ChainwiseExternalDirac", working = working /. step["ExternalMassRules"]],
    {step, record["Route"]}];
  Expand[working - record["Amplitude"]] === 0];
slReplayStep[step_] := Module[{edit = step["SchoutenEdit"], raw, beforeConservation, changed,
    afterConservation, eliminated = step["EliminatedMomentum"]},
  raw = Expand[step["Before"] - edit["Subtract"] + edit["Insert"]];
  beforeConservation = step["BeforeConservation"];
  afterConservation = step["AfterConservation"];
  changed = beforeConservation["Amplitude"];
  If[eliminated =!= None, changed = changed /. chain_SpinorChain :>
    (SpinorChain @@ Join[{First[chain]}, Rest[Most[List @@ chain]] /.
      Mom[eliminated] -> -Total[Mom /@ DeleteCases[step["ExternalLegs"], eliminated]], {Last[chain]}])];
  raw === step["AfterSchouten"] && slReplayNormal[raw, beforeConservation] &&
    If[eliminated === None, afterConservation === beforeConservation,
      slReplayNormal[changed, afterConservation]] && step["InternalOnShellConditions"] === {}];
check["each exact edit and normalization replays from the recorded source",
  slResult["Route"] =!= {} && AllTrue[slResult["Route"],
    TrueQ[#["ExactFormalReplay"]] && AllTrue[#["Steps"], slReplayStep] &]];
slDual[value_] := value /. Spinor[kind_, type : "Angle" | "Square", leg_] :>
  Spinor[kind, If[type === "Angle", "Square", "Angle"], leg];
slDualResult = ConstructiveDiagrams`Private`rspFourLegSchoutenLookahead[slDual[slInput], slRules, True, 2];
check["angle-square duality has the same compact ordinary representative",
  Together[slDualResult["Amplitude"] - slDual[slTarget]] === 0];
slRelabel = {1 -> 2, 2 -> 4, 3 -> 7, 4 -> 9};
slRelabelExpression[value_] := value /. {
  Spinor[kind_, type : "Angle" | "Square", leg_Integer] :> Spinor[kind, type, leg /. slRelabel],
  Mom[leg_Integer] :> Mom[leg /. slRelabel],
  Mom[Multiparticle[legs__Integer]] :> Mom[Multiparticle @@ ({legs} /. slRelabel)]};
slRelabelRules = slRules /. Mass[leg_Integer] :> Mass[leg /. slRelabel];
slRelabelResult = ConstructiveDiagrams`Private`rspFourLegSchoutenLookahead[
  slRelabelExpression[slInput], slRelabelRules, True, 2];
check["external labels are metadata and do not assume positions 1 through 4",
  Together[slRelabelResult["Amplitude"] - slRelabelExpression[slTarget]] === 0];
slDeformation = slDeform MomProd[1, 3] slA[1, 2];
slDeformedInput = slCoupling^2 slX (slNumerator + slDeformation)/(slM1 slDenominator);
slDeformedTarget = slCoupling^2 slX (slCompact + slDeformation)/(slM1 slDenominator);
slDeformedResult = ConstructiveDiagrams`Private`rspFourLegSchoutenLookahead[
  slDeformedInput, slRules, True, 2];
check["an arbitrary off-cut scalar-product remainder is retained",
  Together[slDeformedResult["Amplitude"] - slDeformedTarget] === 0 &&
    !FreeQ[slDeformedResult["Amplitude"], slDeform MomProd[1, 3]]];
check["internal mass rules do not change the external-only lookahead",
  ConstructiveDiagrams`Private`rspFourLegSchoutenLookahead[slInput,
    Append[slRules, Mass[Multiparticle[1, 3]] -> slForbiddenCut], True, 2]["Amplitude"] ===
      slResult["Amplitude"]];
check["a fifth external leg leaves the established higher-point route untouched",
  ConstructiveDiagrams`Private`rspFourLegSchoutenLookahead[slInput,
    Append[slRules, Mass[5] -> 0], True, 2]["Amplitude"] === slInput];
check["disabled momentum conservation and zero time retain the original checkpoint",
  ConstructiveDiagrams`Private`rspFourLegSchoutenLookahead[slInput, slRules, False, 2]["Amplitude"] === slInput &&
    ConstructiveDiagrams`Private`rspFourLegSchoutenLookahead[slInput, slRules, True, 0]["Amplitude"] === slInput];
slInverse = SpinorChain[slSpinor["Square", 1], Mom[2], slSpinor["Angle", 4]]^-1;
slInverseResult = ConstructiveDiagrams`Private`rspFourLegSchoutenLookahead[
  slInverse slInput, slRules, True, 2];
check["an inverse spinor chain is never normalized as a positive numerator",
  Together[slInverseResult["Amplitude"] - slInverse slTarget] === 0];
slProof = ConstructiveDiagrams`Private`completionHEZeroDirection[slNumerator - slCompact, slRules, {}, 10];
check["an independent generic external chart proves the contraction without samples",
  slProof["Status"] === "PROVED_ZERO" && slProof["NumericalKinematicSamples"] === 0 &&
    slProof["InternalOnShellConditions"] === {}];
slWrapped = ConstructiveDiagrams`Private`preparationChannelCurrentCompaction[slInput, slRules, True, 2,
  ConstructiveDiagrams`Private`rspFourLegSchoutenLookahead];
check["the common model-stage wrapper retains the compact scalar result",
  Together[slWrapped["Amplitude"] - slTarget] === 0];
slColor = ColorDelta[ColorIndex[1], ColorIndex[2]];
slColored = ConstructiveDiagrams`Private`preparationChannelCurrentCompaction[slColor slInput, slRules, True, 2,
  ConstructiveDiagrams`Private`rspFourLegSchoutenLookahead];
check["an unchanged color tensor multiplies the same compact spinor coefficient",
  Together[slColored["Amplitude"] - slColor slTarget] === 0];
slModel = ReadModel[FileNameJoin[{root, "models"}], 1];
slPrepared = PrepareAmplitudes[{slRules, slInput}, slModel, "InternalOnShell" -> False,
  "PolePreference" -> None, "MaxSteps" -> 0, "TimeConstraint" -> 8,
  "Parallelization" -> False, "ProgressReporting" -> False];
check["public final-amplitude preparation reaches the compact ordinary form",
  ListQ[slPrepared] && Length[slPrepared] === 2 && Together[Last[slPrepared] - slTarget] === 0];
slIndexed[value_] := value /. Spinor["Spin", type : "Angle" | "Square", leg_] :>
  Spinor["Spin", "Upper", type, leg, slInertIndex[leg, Mom[2]]];
slIndexedResult = ConstructiveDiagrams`Private`rspFourLegSchoutenLookahead[
  slIndexed[slInput], slRules, True, 2];
check["momentum elimination never edits an inert external spin-index label",
  Together[slIndexedResult["Amplitude"] - slIndexed[slTarget]] === 0 &&
    DeleteDuplicates[Cases[slIndexedResult["Amplitude"], slInertIndex[__], Infinity]] ===
      DeleteDuplicates[Cases[slIndexed[slTarget], slInertIndex[__], Infinity]]];
Print["Checks: ", checks, "; failures: ", Length[failures]];
If[failures =!= {}, Print[failures]];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:four-leg-schouten-lookahead-tests.wl"]];
Quit[If[failures === {}, 0, 1]];
