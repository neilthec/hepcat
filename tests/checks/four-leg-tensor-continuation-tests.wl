(* A bounded continuation chooses between exact typed-label frames. Known
   process answers are never supplied to the reducer. These checks exercise
   route continuity, opaque inverse factors and completed timeout checkpoints. *)

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


rcRoot = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{rcRoot, "source", "HEPCAT.wl"}]];
$HistoryLength = 0; rcChecks = 0; rcFailures = {};
SetAttributes[rcCheck, HoldRest];
rcCheck[name_, assertion_] := (rcChecks++; If[!TrueQ[assertion],
  AppendTo[rcFailures, name]; Print["FAIL: ", name]]);
rcReduce = ConstructiveDiagrams`Private`rspFourLegTensorReduction;
rcCore = ConstructiveDiagrams`Private`rspFourLegTensorReductionCore;
rcRelabel = ConstructiveDiagrams`Private`relabelLegs;
rcA[i_, j_] := SpinorChain[Spinor["Spin", "Angle", i], Spinor["Spin", "Angle", j]];
rcS[i_, j_] := SpinorChain[Spinor["Spin", "Square", i], Spinor["Spin", "Square", j]];
rcRules = Thread[Mass /@ Range[4] -> 2];
rcPole = PropDen[Mom[Multiparticle[1, 3]], rcExchangeMass, rcWidth];
rcToy = (rcA[1, 2] rcA[3, 4] + rcA[1, 4] rcA[2, 3]) rcS[1, 3] rcS[2, 4]/rcPole;
rcExpected = rcA[1, 3] rcA[2, 4] rcS[1, 3] rcS[2, 4]/rcPole;
rcInverseBases[value_] := Sort[DeleteDuplicates[Cases[{value},
  Power[base_, _Integer?Negative] /; !FreeQ[base,
    _PropDen | _SpinorChain | _MomProd | _Mom | _Spinor] :> base, Infinity]]];
rcZero[value_] := ConstructiveDiagrams`Private`canonicalChains[
  ReduceSpinorProducts[value]] === 0;
rcSequenceQ[report_, original_, rules_] := Module[{previous = original, valid},
  valid = AllTrue[report["Route"], Function[record,
    record["Operation"] === "TypedFrameFourLegTensorIdentities" &&
    record["Before"] === previous &&
    rcRelabel[record["Before"], record["ForwardLabelMap"]] === record["RenamedBefore"] &&
    rcRelabel[record["RenamedBefore"], record["InverseLabelMap"]] === record["Before"] &&
    rcRelabel[rules, record["ForwardLabelMap"]] === record["RenamedExternalMassRules"] &&
    rcRelabel[record["RenamedAfter"], record["InverseLabelMap"]] === record["After"] &&
    record["CoreRoute"] =!= {} &&
    First[record["CoreRoute"]]["Before"] === record["RenamedBefore"] &&
    Last[record["CoreRoute"]]["After"] === record["RenamedAfter"] &&
    AllTrue[record["CoreRoute"], TrueQ[#["ExactFormalReplay"]] &&
      #["InternalOnShellConditions"] === {} &] &&
    TrueQ[record["ExactTypedRelabelingRoundTrip"]] &&
    TrueQ[record["InverseArgumentsPreserved"]] &&
    record["InternalOnShellConditions"] === {} &&
    (previous = record["After"]; True)]];
  valid && previous === report["Amplitude"]
];
rcReport = rcReduce[rcToy, rcRules, True, 1, None];
rcCheck["the ordinary Schouten packet contracts without a supplied answer",
  rcReport["Status"] === "IMPROVED" && SpinorExpressionComplexity[rcReport["Amplitude"]] === 1 &&
    rcZero[rcReport["Amplitude"] - rcExpected]];
rcCheck["every completed continuation endpoint has an exact typed-frame route",
  rcSequenceQ[rcReport, rcToy, rcRules]];
rcCheck["finite-width propagator mass and width arguments remain unchanged",
  rcInverseBases[rcReport["Amplitude"]] === {rcPole} &&
    !FreeQ[rcReport["Amplitude"], rcPole]];
rcCheck["a local minimum rolls back every unsuccessful label frame",
  With[{report = rcReduce[rcExpected, rcRules, True, 1, None]},
    report["Amplitude"] === rcExpected && report["Route"] === {} &&
      report["Status"] === "UNCHANGED" && report["StopReason"] === "NoImprovingFrame" &&
      Length[report["Iterations"]] <= 4]];
rcAuxiliaryInverse = 1/(5 + rcA[1, 4] + SpinorChain[
  Spinor["Spin", "Angle", 1], Mom[2], Spinor["Spin", "Square", 3]]);
rcWithInverse = rcReduce[rcAuxiliaryInverse rcToy, rcRules, True, 1, None];
rcCheck["complete spinor inverse arguments survive all attempted frames literally",
  rcWithInverse["Status"] === "IMPROVED" &&
    rcInverseBases[rcWithInverse["Amplitude"]] === rcInverseBases[rcAuxiliaryInverse rcToy] &&
    rcSequenceQ[rcWithInverse, rcAuxiliaryInverse rcToy, rcRules]];
rcPermutedRules = Reverse[rcRules];
rcPermutedReport = rcReduce[rcToy, rcPermutedRules, True, 1, None];
rcCheck["external rule order does not change the result or its mass assumptions",
  rcZero[rcPermutedReport["Amplitude"] - rcExpected] &&
    rcSequenceQ[rcPermutedReport, rcToy, rcPermutedRules]];
rcLabelMap = AssociationThread[Range[4], {17, 29, 31, 43}];
rcRelabeledToy = (7/3) rcCoupling^2 rcRelabel[rcToy, rcLabelMap];
rcRelabeledRules = rcRelabel[rcRules, rcLabelMap];
rcRelabeled = rcReduce[rcRelabeledToy, rcRelabeledRules, True, 1, None];
rcCheck["typed frames preserve numerical coefficients and unrelated couplings",
  rcZero[rcRelabeled["Amplitude"] - (7/3) rcCoupling^2 rcRelabel[rcExpected, rcLabelMap]] &&
    OwnValues[rcCoupling] === {} && rcSequenceQ[rcRelabeled, rcRelabeledToy, rcRelabeledRules]];
rcCheck["unequal numerical masses keep the existing safe decline",
  With[{report = rcReduce[rcToy, Thread[Mass /@ Range[4] -> {2, 3, 2, 3}], True, 1, None]},
    report["Amplitude"] === rcToy && report["Route"] === {} &&
      report["StopReason"] === "UnsupportedNumericMassAbstraction"]];
rcCheck["a numerical mass ordered after a symbolic scale cannot relabel a particle",
  With[{report = rcReduce[rcToy, Thread[Mass /@ Range[4] -> {rcMass, 2, rcMass, 2}], True, 1, None]},
    report["Amplitude"] === rcToy && report["Route"] === {} &&
      report["StopReason"] === "UnsupportedNumericMassAbstraction"]];
rcCheck["five- and six-leg inputs retain their original expressions",
  And @@ Table[With[{report = rcReduce[rcToy,
      Join[rcRules, Thread[Mass /@ Range[5, count] -> 2]], True, 1, None]},
    report["Amplitude"] === rcToy && report["Route"] === {}], {count, {5, 6}}]];
rcCheck["disabled momentum conservation does not start a frame portfolio",
  With[{report = rcReduce[rcToy, rcRules, False, 1, None]},
    report["Amplitude"] === rcToy && report["Route"] === {}]];
rcHuge = (rcA[1, 2] + rcA[3, 4])^(10^9);
rcHugeTime = First[AbsoluteTiming[rcHugeReport = rcReduce[rcHuge, rcRules, True, .03, None]]];
rcCheck["an unexpanded huge power is declined before eager rank expansion",
  rcHugeReport["Amplitude"] === rcHuge && rcHugeReport["Route"] === {} && rcHugeTime < .6];

(* Use an actual completed core proof, then interrupt its continuation. The
   accepted endpoint must survive with its matching route and unchanged poles. *)
rcCoreProof = rcCore[rcToy, rcRules, True, 1, None];
rcCheckpoint = None;
rcTimed = Internal`InheritedBlock[{ConstructiveDiagrams`Private`rspFourLegTensorReductionCore},
  Clear[ConstructiveDiagrams`Private`rspFourLegTensorReductionCore];
  ConstructiveDiagrams`Private`rspFourLegTensorReductionCore[_, _, _, _, callback_] :=
    (callback[rcCoreProof]; Pause[1]; rcCoreProof);
  AbsoluteTiming[rcTimedReport = rcReduce[rcToy, rcRules, True, .05,
    Function[record, rcCheckpoint = record]]]];
rcCheck["the shared deadline retains a complete published amplitude and proof",
  First[rcTimed] < .6 && rcTimedReport["TimedOut"] === True &&
    AssociationQ[rcCheckpoint] && rcTimedReport["Amplitude"] === rcCheckpoint["Amplitude"] &&
    rcTimedReport["Route"] === rcCheckpoint["Route"] &&
    rcSequenceQ[rcTimedReport, rcToy, rcRules] &&
    rcInverseBases[rcTimedReport["Amplitude"]] === {rcPole}];
rcScopedCheckpoint = None;
rcScopedCallback = Quiet[Check[rcReduce[rcToy, rcRules, True, 1,
  Function[ConstructiveDiagrams`Private`record,
    rcScopedCheckpoint = ConstructiveDiagrams`Private`record]], $Failed]];
rcCheck["a callback parameter may share the private record name without capture",
  AssociationQ[rcScopedCallback] && AssociationQ[rcScopedCheckpoint] &&
    rcScopedCallback["Amplitude"] === rcScopedCheckpoint["Amplitude"] &&
    rcSequenceQ[rcScopedCallback, rcToy, rcRules]];
rcCheck["zero budget preserves the supplied input exactly",
  With[{report = rcReduce[rcToy, rcRules, True, 0, None]},
    report["Amplitude"] === rcToy && report["Route"] === {}]];
(* The optimizer proposes support only. Replaying zeros must leave every
   surviving exact coordinate in the same identity span. *)
rcSupportMatrix = SparseArray[{{2, 1}, {1, -1}, {-3, 1}, {0, 0}, {0, 0}}];
rcSupportVector = {-1, -2, 5, 3, 0};
rcSupportProposal = {0., 0., 1., 3., 0., 0., 0., 0., 0., 0., 1., -1.};
rcSupportSeed = {999/1000, -999/1000};
rcSupport = ConstructiveDiagrams`Private`rspPolishExactIdentitySupport[
  rcSupportMatrix, rcSupportVector, rcSupportProposal, rcSupportSeed, 1];
rcCheck["the exact solve restores every proposed zero and retains exact nonzero coordinates",
  AssociationQ[rcSupport] && rcSupport["Coefficients"] === {1, -1} &&
    rcSupport["ZeroIndices"] === {1, 2, 5} && TrueQ[rcSupport["ExactSupportReplay"]] &&
    rcSupportVector + rcSupportMatrix.rcSupport["Coefficients"] === {0, 0, 1, 3, 0}];
rcImpossibleSupport = ConstructiveDiagrams`Private`rspPolishExactIdentitySupport[
  rcSupportMatrix, ReplacePart[rcSupportVector, 5 -> 1/10^12],
  rcSupportProposal, rcSupportSeed, 1];
rcCheck["a small but exact nonzero coordinate outside the identity span cannot be dropped",
  rcImpossibleSupport === Missing["InconsistentExactSupport"]];
rcDeclinedSupportAttempts = 0;
rcDeclinedSupport = Internal`InheritedBlock[
  {ConstructiveDiagrams`Private`rspPolishExactIdentitySupport},
  Clear[ConstructiveDiagrams`Private`rspPolishExactIdentitySupport];
  ConstructiveDiagrams`Private`rspPolishExactIdentitySupport[___] :=
    (rcDeclinedSupportAttempts++; rcImpossibleSupport);
  rcCore[rcToy, rcRules, True, 1, None]];
rcCheck["an inconsistent support proposal retains the existing rational identity endpoint",
  rcDeclinedSupportAttempts > 0 && rcDeclinedSupport["Status"] === "IMPROVED" &&
    rcDeclinedSupport["Amplitude"] === rcCoreProof["Amplitude"] &&
    FreeQ[rcDeclinedSupport["Route"], "ExactLPSupportPolish"] &&
    AllTrue[rcDeclinedSupport["Route"], TrueQ[#["ExactFormalReplay"]] &]];
Print[rcChecks, " four-leg-tensor-continuation checks; ", Length[rcFailures], " failures."];
If[rcFailures =!= {}, Print[rcFailures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:four-leg-tensor-continuation-tests.wl"];
Quit[0];
