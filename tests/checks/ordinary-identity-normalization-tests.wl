(* Sparse ordinary identities are proposals, followed by an exact symbolic replay.
   No model-specific amplitude or sampled kinematics is supplied to the optimizer. *)

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
check[name_, expression_] := (checks++;
  If[!TrueQ[expression], AppendTo[failures, name]; Print["FAIL: ", name]]);
oiSpace = ConstructiveDiagrams`Private`rspMultilinearIdentitySpace;
oiSystem = ConstructiveDiagrams`Private`rspIdentityPrepareSystem;
oiOptimize = ConstructiveDiagrams`Private`rspOptimizeIdentitySpace;
oiPrepare = ConstructiveDiagrams`Private`preparationMultilinearIdentityNormalize;
oiA[i_] := Spinor["Spin", "Angle", i]; oiS[i_] := Spinor["Spin", "Square", i];
oiAngle[i_, j_] := SpinorChain[oiA[i], oiA[j]];
oiSquare[i_, j_] := SpinorChain[oiS[i], oiS[j]];
oiSandwich[i_, k_, j_] := SpinorChain[oiA[i], Mom[k], oiS[j]];
oiRules = Table[Mass[i] -> Symbol["oiMass" <> ToString[i]], {i, 6}];
(* The first three terms are Schouten after using only <1|p1 = m1[1|. *)
oiIdentity = oiSquare[4, 5] (oiAngle[1, 2] oiSandwich[3, 1, 6] -
  oiMass1 oiAngle[3, 2] oiSquare[1, 6] - oiAngle[1, 3] oiSandwich[2, 1, 6]);
oiBasis = oiAngle[1, 2] oiSquare[4, 5] oiSandwich[3, 2, 6];
oiInput = oiIdentity + 7 oiBasis;
oiInputHash = Hash[{oiInput, oiRules}, "SHA256"];
oiData = oiSpace[oiInput, oiRules];
check["a general unequal-mass multilinear sector is admitted", AssociationQ[oiData]];
oiPreparedSystem = oiSystem[oiData];
check["the identity system is bounded and exactly rational",
  AssociationQ[oiPreparedSystem] && Length[oiPreparedSystem["Keys"]] <= 2048 &&
    Length[oiPreparedSystem["SystemIdentities"]] <= 2048 &&
    AllTrue[Cases[Normal[oiPreparedSystem["IdentityMatrix"]], _?NumberQ, Infinity],
      MatchQ[#, _Integer | _Rational] &]];
oiResult = oiOptimize[oiInput, oiPreparedSystem, 5];
check["the target-free ordinary optimizer reduces the complete numerator",
  oiResult["Status"] === "IMPROVED" && oiResult["ExactFormalReplay"] === True &&
    SpinorExpressionComplexity[oiResult["Amplitude"]] <= SpinorExpressionComplexity[7 oiBasis]];
check["every accepted proposal has an exact residual certificate",
  oiResult["Certificate"]["ExactResidual"] === 0 &&
    Length[oiResult["Certificate"]["Coefficients"]] ===
      Length[oiResult["Certificate"]["OrdinaryIdentities"]]];
check["the original numerator and scoped mass rules are unchanged",
  Hash[{oiInput, oiRules}, "SHA256"] === oiInputHash];
oiZero = oiOptimize[oiIdentity, oiPreparedSystem, 5];
check["a pure ordinary Dirac-Schouten identity reduces to exact zero", oiZero["Amplitude"] === 0];
oiRepeated = oiOptimize[oiResult["Amplitude"], oiPreparedSystem, 5];
check["a completed sparse representative is idempotent", oiRepeated["Amplitude"] === oiResult["Amplitude"]];
check["a zero optimization budget keeps the original atomically",
  oiOptimize[oiInput, oiPreparedSystem, 0]["Amplitude"] === oiInput];
oiTiny = oiOptimize[oiInput, oiPreparedSystem, 10^-12];
check["an early timeout cannot publish an unverified edit",
  oiTiny["Amplitude"] === oiInput || TrueQ[oiTiny["ExactFormalReplay"]]];
check["conflicting external masses cannot license an identity",
  MissingQ[oiSpace[oiInput, Append[oiRules, Mass[1] -> oiWrongMass]]]];
check["a valid declaration cannot hide a second nonscalar mass rule",
  MissingQ[oiSpace[oiInput, Append[oiRules, Mass[1] -> {oiMass1}]]]];
check["delayed external mass declarations are not evaluated as assumptions",
  MissingQ[oiSpace[oiInput, Append[oiRules, Mass[1] :> oiWrongMass]]]];
check["nonfinite external masses cannot license an identity",
  MissingQ[oiSpace[oiInput, ReplacePart[oiRules, 1 -> (Mass[1] -> Infinity)]]]];
check["consistent duplicate external mass rules remain valid",
  AssociationQ[oiSpace[oiInput, Append[oiRules, First[oiRules]]]]];
check["internal composite mass rules do not enter the identity system",
  oiSpace[oiInput, Append[oiRules, Mass[Multiparticle[1, 2]] -> oiInternalMass]]["ExternalMassRules"] === oiRules];
check["nonscalar external mass data is declined",
  MissingQ[oiSpace[oiInput, ReplacePart[oiRules, 1 -> (Mass[1] -> {oiMass1})]]]];
check["unmatched coefficient fields are retained instead of being fitted",
  MissingQ[oiSpace[oiInput + oiIndependentCoefficient oiBasis, oiRules]]];
check["inverse kinematic factors require a numerator-only caller",
  MissingQ[oiSpace[oiInput/PropDen[Mom[Multiparticle[1, 3]], oiMass1], oiRules]]];
check["large factored powers are declined before expansion",
  MissingQ[oiSpace[(oiInput + oiBasis)^100000, oiRules]]];
check["noncommutative factors are declined", MissingQ[oiSpace[oiInput Dot[oiX, oiY], oiRules]]];
check["a repeated external spinor is not silently treated as multilinear",
  MissingQ[oiSpace[oiInput /. oiA[2] -> oiA[1], oiRules]]];
oiLabelMap = AssociationThread[Range[6], {19, 7, 31, 11, 29, 23}];
oiRelabel[value_] := value /. {
  Spinor["Spin", variance : ("Upper" | "Lower"), type_, i_Integer, projection_] :>
    Spinor["Spin", variance, type, oiLabelMap[i], projection],
  Spinor["Spin", type_, i_Integer] :>
    Spinor["Spin", type, oiLabelMap[i]], Mom[i_Integer] :> Mom[oiLabelMap[i]],
    Mass[i_Integer] :> Mass[oiLabelMap[i]]};
oiRelabelData = oiSpace[oiRelabel[oiIdentity], oiRelabel[oiRules]];
oiRelabelResult = oiOptimize[oiRelabel[oiIdentity], oiSystem[oiRelabelData], 5];
check["arbitrary particle labels preserve the exact ordinary identity", oiRelabelResult["Amplitude"] === 0];
oiDual = oiIdentity /. {"Angle" -> "Square", "Square" -> "Angle"};
oiDualData = oiSpace[oiDual, oiRules];
check["the angle-square dual uses the exact transpose signs",
  oiOptimize[oiDual, oiSystem[oiDualData], 5]["Amplitude"] === 0];
oiExplicit[value_] := value /. Spinor["Spin", type_, i_Integer] :>
  Spinor["Spin", "Upper", type, i, If[OddQ[i], 1/2, -1/2]];
oiExplicitData = oiSpace[oiExplicit[oiInput], oiRules];
oiExplicitResult = oiOptimize[oiExplicit[oiInput], oiSystem[oiExplicitData], 5];
check["explicit spin projections and index variance are preserved",
  oiExplicitResult["ExactFormalReplay"] === True &&
    AllTrue[Cases[oiExplicitResult["Amplitude"], _Spinor, Infinity],
      MatchQ[#, Spinor["Spin", "Upper", "Angle" | "Square", i_Integer, projection_] /;
        projection === If[OddQ[i], 1/2, -1/2]] &]];
oiConservation = oiAngle[1, 2] oiSquare[4, 5] Total[oiSandwich[3, #, 6] & /@ Range[6]];
oiWithoutConservation = oiPrepare[oiConservation, oiRules, False, 5];
oiWithConservation = oiPrepare[oiConservation, oiRules, True, 5];
check["the ordinary base space never assumes total momentum conservation",
  oiWithoutConservation["Amplitude"] =!= 0];
check["preparation can explicitly add declared total momentum conservation",
  oiWithConservation["Amplitude"] === 0 && AllTrue[oiWithConservation["Route"],
    TrueQ[#["MomentumConservation"]] && #["InternalOnShellIdentity"] === False &]];
oiProps = {PropDen[Mom[Multiparticle[1, 3]], oiMass1],
  PropDen[Mom[Multiparticle[2, 4, 5]], oiOtherExchangeMass]};
oiFraction = (oiConservation + oiBasis)/Times @@ oiProps;
oiFractionResult = oiPrepare[oiFraction, oiRules, True, 5];
check["complete physical propagator products are preserved off shell",
  Sort[DeleteDuplicates[Cases[oiFractionResult["Amplitude"], _PropDen, Infinity]]] === Sort[oiProps] &&
    AllTrue[oiFractionResult["Route"], #["InternalOnShellIdentity"] === False &]];
check["zero preparation budget returns the untouched input",
  oiPrepare[oiFraction, oiRules, True, 0]["Amplitude"] === oiFraction];
(* Independently prove every generated Base row, including all unequal-mass
   endpoint reductions, on arbitrary momentum matrices. The six matrices are
   independent, so this certificate cannot accidentally use conservation. *)
oiJ = {{0, -1}, {1, 0}};
oiSymbolicMatrices = Association@Table[With[
  {a = Symbol["oiChartA" <> ToString[i]], b = Symbol["oiChartB" <> ToString[i]],
   c = Symbol["oiChartC" <> ToString[i]], m = Mass[i] /. oiRules},
  i -> {{a, b}, {c, (m^2 + b c)/a}}], {i, 6}];
oiSymbolicAngles = Association@Table[i ->
  {Symbol["oiChartU" <> ToString[i]], Symbol["oiChartV" <> ToString[i]]}, {i, 6}];
oiSymbolicSquares = Association@Table[i ->
  (-Transpose[oiSymbolicMatrices[i]].oiJ.oiSymbolicAngles[i]/(Mass[i] /. oiRules)), {i, 6}];
oiSymbolicChart = <|"Momenta" -> oiSymbolicMatrices,
  "Angle" -> oiSymbolicAngles, "Square" -> oiSymbolicSquares,
  "Masses" -> Association@Table[i -> (Mass[i] /. oiRules), {i, 6}]|>;
oiGeneratorCertificates = Table[TimeConstrained[Cancel[Together[
  ConstructiveDiagrams`Private`residueEvaluateChart[
    oiData["Restore"][identity], oiSymbolicChart, {}]]], 0.5, $TimedOut],
  {identity, oiData["Identities"]}];
check["every generator is a symbolic identity for arbitrary independent unequal-mass momenta",
  Length[oiGeneratorCertificates] === 900 && AllTrue[oiGeneratorCertificates, # === 0 &]];

(* Conversion is checked against the independent matrix evaluator too, so a
   mistaken chirality transpose cannot certify itself through a formal replay. *)
oiRoundTrip = oiData["Restore"][oiData["ToFormal"][oiInput]];
check["formalize and restore preserve a nonzero numerator independently",
  Cancel[Together[ConstructiveDiagrams`Private`residueEvaluateChart[
    oiInput - oiRoundTrip, oiSymbolicChart, {}]]] === 0];
oiReversedInput = oiInput /. HoldPattern[SpinorChain[
    a : Spinor["Spin", "Angle", _Integer], p_Mom,
    b : Spinor["Spin", "Square", _Integer]]] :> SpinorChain[b, p, a];
oiReversedData = oiSpace[oiReversedInput, oiRules];
oiReversedRoundTrip = oiReversedData["Restore"][oiReversedData["ToFormal"][oiReversedInput]];
check["Square-Angle sandwich reversal has an independent symbolic certificate",
  Cancel[Together[ConstructiveDiagrams`Private`residueEvaluateChart[
    oiReversedInput - oiReversedRoundTrip, oiSymbolicChart, {}]]] === 0];
oiMixedVariance[value_] := value /. Spinor["Spin", type_, i_Integer] :>
  Spinor["Spin", If[OddQ[i], "Upper", "Lower"], type, i, If[OddQ[i], 1/2, -1/2]];
oiRichInput = oiRelabel[oiMixedVariance[oiReversedInput]];
oiRichRules = oiRelabel[oiRules];
oiRichData = oiSpace[oiRichInput, oiRichRules];
oiRichRoundTrip = oiRichData["Restore"][oiRichData["ToFormal"][oiRichInput]];
oiAngleBases = AssociationThread[Range[6], ConstantArray[IdentityMatrix[2], 6]];
oiSquareBases = Association@Table[i ->
  (-Transpose[oiSymbolicMatrices[i]].oiJ/(Mass[i] /. oiRules)), {i, 6}];
oiRichChart = Join[Association@KeyValueMap[
    #1 -> Association@KeyValueMap[(oiLabelMap[#1] -> #2) &, #2] &, oiSymbolicChart],
  <|"AngleBasis" -> Association@KeyValueMap[(oiLabelMap[#1] -> #2) &, oiAngleBases],
    "SquareBasis" -> Association@KeyValueMap[(oiLabelMap[#1] -> #2) &, oiSquareBases]|>];
check["shuffled labels and mixed Upper-Lower spin metadata round-trip independently",
  AssociationQ[oiRichData] &&
    Cancel[Together[ConstructiveDiagrams`Private`residueEvaluateChart[
      oiRichInput - oiRichRoundTrip, oiRichChart, {}]]] === 0 &&
    FreeQ[oiRichRoundTrip, Spinor["Spin", "Angle" | "Square", _Integer]]];

(* A rejected group must not poison the reusable system cache for another
   numerator that has the same six typed spinor records. *)
oiMassOnly = oiMass1 oiAngle[1, 2] oiSquare[3, 4] oiSquare[5, 6];
oiCachePoles = {PropDen[Mom[Multiparticle[1, 3]], oiMass1],
  PropDen[Mom[Multiparticle[2, 4]], oiMass2]};
oiCacheInputs = Table[
  oiMassOnly/oiCachePoles[[order[[1]]]] + oiIdentity/oiCachePoles[[order[[2]]]],
  {order, {{1, 2}, {2, 1}}}];
oiCacheResults = oiPrepare[#, oiRules, True, 5] & /@ oiCacheInputs;
check["a momentum-free pole group cannot block a later eligible group",
  And @@ MapThread[TrueQ[Together[#1["Amplitude"] - #2] === 0] &,
    {oiCacheResults, {oiMassOnly/oiCachePoles[[1]], oiMassOnly/oiCachePoles[[2]]}}]];
oiBadField = oiIdentity + oiIndependentCoefficient oiBasis;
oiBadFieldInputs = Table[
  oiBadField/oiCachePoles[[order[[1]]]] + oiIdentity/oiCachePoles[[order[[2]]]],
  {order, {{1, 2}, {2, 1}}}];
oiBadFieldResults = oiPrepare[#, oiRules, True, 5] & /@ oiBadFieldInputs;
check["an unsupported scalar field cannot poison a later eligible pole group",
  And @@ MapThread[TrueQ[Together[#1["Amplitude"] - #2] === 0] &,
    {oiBadFieldResults, {oiBadField/oiCachePoles[[1]], oiBadField/oiCachePoles[[2]]}}]];

Print["Ordinary identity normalization: ", checks, " checks; ", Length[failures], " failures."];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:ordinary-identity-normalization-tests.wl"]; Quit[0], Quit[1]];
