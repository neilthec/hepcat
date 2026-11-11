(* Ordinary Schouten identities must see a complete original numerator. *)

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

wfContinuation = ConstructiveDiagrams`Private`preparationWholeFractionContinuation[
  wfExpression, wfRules, 5];
check["checkpoint continuation preserves the actual ordinary Dirac-Schouten result",
  wfContinuation["Status"] === "IMPROVED" &&
    TrueQ[Expand[wfContinuation["Amplitude"] - wfExpected/wfW] === 0]];
check["every continued real checkpoint retains a complete ordinary replay",
  wfContinuation["Route"] =!= {} &&
    First[wfContinuation["Route"]]["Before"] === wfExpression &&
    Last[wfContinuation["Route"]]["After"] === wfContinuation["Amplitude"] &&
    AllTrue[Flatten[Lookup[wfContinuation["Route"], "IdentityRoute"], 1], wfReplay] &&
    wfContinuation["InternalOnShellConditions"] === {}];
wfContinuationInverse = ConstructiveDiagrams`Private`preparationWholeFractionContinuation[
  wfExpression/wfBracket, wfRules, 5];
check["continued checkpoints keep the existing inverse factors unchanged",
  TrueQ[Expand[wfContinuationInverse["Amplitude"] - wfExpected/(wfW wfBracket)] === 0]];
wfContinuationZero = ConstructiveDiagrams`Private`preparationWholeFractionContinuation[
  wfExpression, wfRules, 0];
check["zero continuation budget leaves the complete input and route unchanged",
  wfContinuationZero["Amplitude"] === wfExpression && wfContinuationZero["Route"] === {} &&
    wfContinuationZero["StopReason"] === "NoSearchBudget"];

(* Controlled search checkpoints test scheduling and interrupted-state safety.
   They are not used as mathematical identities or physical validation. *)
wfControlPoints = {wfExpression + wfBracket^3 + wfBracket^2,
  wfExpression + wfBracket^2, wfExpression};
wfControlCalls = {};
wfControlResult = Block[{ConstructiveDiagrams`Private`preparationWholeFractionSchouten},
  ConstructiveDiagrams`Private`preparationWholeFractionSchouten[value_, scope_, budget_] := Module[{position},
    AppendTo[wfControlCalls, <|"Input" -> value, "Budget" -> budget|>];
    position = First[FirstPosition[wfControlPoints, value]];
    <|"Amplitude" -> wfControlPoints[[Min[3, position + 1]]],
      "Route" -> {<|"ControlledCheckpoint" -> position|>}, "Status" -> "IMPROVED"|>];
  ConstructiveDiagrams`Private`preparationWholeFractionContinuation[First[wfControlPoints], wfRules, 5]];
check["continuation starts each new search from its accepted predecessor",
  Lookup[wfControlCalls, "Input"] === wfControlPoints &&
    wfControlResult["Amplitude"] === Last[wfControlPoints] &&
    Length[wfControlResult["Route"]] === 2];
check["a nominal success without a strict improvement stops continuation",
  wfControlResult["StopReason"] === "NoImprovingCheckpoint" &&
    Length[wfControlResult["Iterations"]] === 3];
check["all continuation searches share a decreasing finite allowance",
  AllTrue[Lookup[wfControlCalls, "Budget"], 0 < # <= 5 &] &&
    OrderedQ[Reverse[Lookup[wfControlCalls, "Budget"]]]];
wfInterruptedCalls = 0;
wfContinuationInterrupted = Block[{ConstructiveDiagrams`Private`preparationWholeFractionSchouten},
  ConstructiveDiagrams`Private`preparationWholeFractionSchouten[value_, scope_, budget_] :=
    (wfInterruptedCalls++;
      If[wfInterruptedCalls > 1, Pause[1]];
      <|"Amplitude" -> wfControlPoints[[2]], "Route" -> {<|"ControlledCheckpoint" -> 1|>},
        "Status" -> "IMPROVED"|>);
  ConstructiveDiagrams`Private`preparationWholeFractionContinuation[First[wfControlPoints], wfRules, 0.15]];
check["an interrupted later search retains the complete earlier expression and replay",
  wfContinuationInterrupted["Amplitude"] === wfControlPoints[[2]] &&
    Length[wfContinuationInterrupted["Route"]] === 1 &&
    Last[wfContinuationInterrupted["Route"]]["After"] === wfContinuationInterrupted["Amplitude"] &&
    wfContinuationInterrupted["Rank"] ===
      ConstructiveDiagrams`Private`preparationRank[wfContinuationInterrupted["Amplitude"]] &&
    TrueQ[wfContinuationInterrupted["TimedOut"]]];
wfPublishedCheckpoint = None; wfInterruptedCalls = 0;
wfOuterInterrupted = Block[{ConstructiveDiagrams`Private`preparationWholeFractionSchouten},
  ConstructiveDiagrams`Private`preparationWholeFractionSchouten[value_, scope_, budget_] :=
    (wfInterruptedCalls++;
      If[wfInterruptedCalls > 1, Pause[1]];
      <|"Amplitude" -> wfControlPoints[[2]], "Route" -> {<|"ControlledCheckpoint" -> 1|>},
        "Status" -> "IMPROVED"|>);
  TimeConstrained[ConstructiveDiagrams`Private`preparationWholeFractionContinuation[
    First[wfControlPoints], wfRules, 5, Function[record, wfPublishedCheckpoint = record]],
    0.15, $TimedOut]];
check["an outer caller clock can recover the published complete expression and replay",
  wfOuterInterrupted === $TimedOut && AssociationQ[wfPublishedCheckpoint] &&
    wfPublishedCheckpoint["Amplitude"] === wfControlPoints[[2]] &&
    Length[wfPublishedCheckpoint["Route"]] === 1 &&
    Last[wfPublishedCheckpoint["Route"]]["After"] === wfPublishedCheckpoint["Amplitude"]];
Print["WHOLE FRACTION SCHOUTEN CHECKS: ", checks, "; FAILURES: ", Length[failures]];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:whole-fraction-schouten-tests.wl"], Print[failures]];
Quit[If[failures === {}, 0, 1]];
