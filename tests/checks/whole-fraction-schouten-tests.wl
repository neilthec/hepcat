(* Ordinary Schouten identities must see a complete original numerator. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0;
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);
wfA[i_] := Spinor[If[MemberQ[{1, 2}, i], "Spin", "Helicity"], "Angle", i];
wfS[i_] := Spinor[If[MemberQ[{1, 2}, i], "Spin", "Helicity"], "Square", i];
wfRules = {Mass[1] -> wfMa, Mass[2] -> wfMb, Mass[3] -> 0, Mass[4] -> 0};
wfChains = {SpinorChain[wfA[1], wfA[2]], SpinorChain[wfA[3], Mom[1], wfS[4]]};
wfLeft = Times @@ wfChains;
wfRewrite = SchoutenRewrite[wfChains[[1]], wfChains[[2]], {0, 0}];
wfRight = ConstructiveDiagrams`Private`canonicalChains[
  (wfRewrite /. chain_SpinorChain :> ReduceSpinorProducts[chain]) /. wfRules];
wfMassTerm = SelectFirst[List @@ Expand[wfRight], FreeQ[#, _Mom] &];
wfExpected = Expand[wfRight - wfMassTerm];
wfNumerator = wfLeft - wfMassTerm;
wfV = PropDen[Mom[Multiparticle[1, 3]], wfMa];
wfW = PropDen[Mom[Multiparticle[2, 4]], wfMb];
wfExpression = (wfV wfLeft - wfV wfMassTerm)/(wfV wfW);
wfResult = ConstructiveDiagrams`Private`preparationWholeFractionSchouten[wfExpression, wfRules, 5];
check["whole-fraction Schouten finds an external Dirac cancellation",
  wfResult["Status"] === "IMPROVED" &&
    TrueQ[Expand[wfResult["Amplitude"] - wfExpected/wfW] === 0]];
check["the ordinary identity strictly reduces the expression rank",
  ConstructiveDiagrams`Private`preparationBetterQ[wfResult["Rank"],
    ConstructiveDiagrams`Private`preparationRank[wfExpression]]];
check["internal propagators remain symbolic and are not put on shell",
  wfResult["InternalOnShellConditions"] === {} && !FreeQ[wfResult["Amplitude"], wfW] &&
    AllTrue[wfResult["Route"], #["InternalOnShellConditions"] === {} &]];
wfReplay[report_Association] := Module[{edit = report["Edit"], cofactor = report["Cofactor"], inserted, replayed},
  inserted = ConstructiveDiagrams`Private`canonicalChains[
    edit["Subtract"]/(Times @@ edit["Chains"])
      SchoutenRewrite[edit["Chains"][[1]], edit["Chains"][[2]], edit["Splits"]]];
  replayed = ConstructiveDiagrams`Private`canonicalChains[
    ConstructiveDiagrams`Private`preparationSpinorProducts[
      Expand[report["OriginalNumerator"] - cofactor edit["Subtract"] + cofactor edit["Insert"]],
      report["ExternalMassRules"]]];
  TrueQ[Expand[inserted - edit["Insert"]] === 0] &&
    TrueQ[Expand[replayed - report["Numerator"]] === 0]];
check["each accepted edit replays as generalized Schouten followed by external reduction",
  wfResult["Route"] =!= {} && AllTrue[wfResult["Route"], wfReplay]];
check["the saved numerator and denominator reconstruct the accepted amplitude",
  With[{record = First[wfResult["Route"]]}, record["Fraction"] === 1 &&
    TrueQ[Expand[record["Numerator"]/record["Denominator"] - wfResult["Amplitude"]] === 0]]];
check["scoped virtuality tokens do not escape into expressions or replay data",
  FreeQ[wfResult, symbol_Symbol /; StringStartsQ[SymbolName[Unevaluated[symbol]], "wholeFractionVirtuality"]]];

wfBracket = SpinorChain[wfA[3], wfA[4]];
wfInverse = ConstructiveDiagrams`Private`preparationWholeFractionSchouten[
  wfExpression/wfBracket, wfRules, 5];
check["an existing inverse spinor factor is held unchanged",
  wfInverse["Status"] === "IMPROVED" &&
    TrueQ[Expand[wfInverse["Amplitude"] - wfExpected/(wfW wfBracket)] === 0]];
wfScaled = ConstructiveDiagrams`Private`preparationWholeFractionSchouten[
  (3 + 2 I) wfExpression, wfRules, 5];
check["complex coefficients and unequal external masses are preserved",
  wfScaled["Status"] === "IMPROVED" &&
    TrueQ[Expand[wfScaled["Amplitude"] - (3 + 2 I) wfExpected/wfW] === 0]];

wfLabelRules = {1 -> 8, 2 -> 6, 3 -> 11, 4 -> 9};
wfRelabel[expression_] := expression /. {
  Spinor[kind_, type_, leg_Integer] :> Spinor[kind, type, leg /. wfLabelRules],
  Mom[Multiparticle[legs__Integer]] :> Mom[Multiparticle @@ ({legs} /. wfLabelRules)],
  Mom[leg_Integer] :> Mom[leg /. wfLabelRules],
  Mass[leg_Integer] :> Mass[leg /. wfLabelRules]};
wfPermuted = ConstructiveDiagrams`Private`preparationWholeFractionSchouten[
  wfRelabel[wfExpression], wfRelabel[wfRules], 5];
check["the same identity works with arbitrary external labels",
  wfPermuted["Status"] === "IMPROVED" &&
    TrueQ[Expand[wfPermuted["Amplitude"] - wfRelabel[wfExpected/wfW]] === 0]];
wfWithInternalRule = ConstructiveDiagrams`Private`preparationWholeFractionSchouten[
  wfExpression, Append[wfRules, Mass[Multiparticle[1, 3]] -> wfMa], 5];
check["extra internal mass declarations do not enter the ordinary identity scope",
  wfWithInternalRule["Amplitude"] === wfResult["Amplitude"] &&
    AllTrue[wfWithInternalRule["Route"], #["ExternalMassRules"] === wfRules &]];
wfZeroBudget = ConstructiveDiagrams`Private`preparationWholeFractionSchouten[wfExpression, wfRules, 0];
check["an exhausted budget retains the unchanged valid input",
  wfZeroBudget["Amplitude"] === wfExpression && wfZeroBudget["Route"] === {}];
wfInterrupted = ConstructiveDiagrams`Private`preparationWholeFractionSchouten[
  wfExpression, wfRules, 0.000001];
check["a bounded interruption keeps matching expression, rank and replay checkpoints",
  wfInterrupted["Rank"] === ConstructiveDiagrams`Private`preparationRank[wfInterrupted["Amplitude"]] &&
    (wfInterrupted["Route"] === {} && wfInterrupted["Amplitude"] === wfExpression ||
      wfInterrupted["Route"] =!= {} && AllTrue[wfInterrupted["Route"], wfReplay])];
wfConflicting = ConstructiveDiagrams`Private`preparationWholeFractionSchouten[
  wfExpression, Append[wfRules, Mass[1] -> wfMb], 5];
check["conflicting external masses cannot license a reduction",
  wfConflicting["Amplitude"] === wfExpression && wfConflicting["Route"] === {}];
check["the helper reports ordinary scope without creating model or numerical assumptions",
  AllTrue[wfResult["Route"], #["ForwardReduction"] === "SpinorProductsAndExternalDiracSchouten" &] &&
    wfResult["AttemptedFractions"] >= 1 && wfResult["InspectedCandidates"] >= 1];
Print["WHOLE FRACTION SCHOUTEN CHECKS: ", checks, "; FAILURES: ", Length[failures]];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:whole-fraction-schouten-tests.wl"], Print[failures]];
Quit[If[failures === {}, 0, 1]];
