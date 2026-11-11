(* Ordinary final-amplitude conservation must not assume any internal shell.
   Check the exact identity transcript, widths, color coefficients and deadlines. *)

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
check[name_, value_] := (checks++;
  If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);
fcCompact = ConstructiveDiagrams`Private`preparationFinalConservationCompaction;
fcRank = ConstructiveDiagrams`Private`preparationRank;
fcRules = Thread[Mass /@ Range[4] -> {fcMass, fcMass, fcVectorMass, fcScalarMass}];
fcChain[i_] := SpinorChain[Spinor["Spin", "Square", 1], Mom[i], Spinor["Spin", "Angle", 2]];
fcAngle = SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]];
fcSquare = SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]];
fcPole = PropDen[Mom[Multiparticle[1, 3]], fcExchangedMass, fcWidth];
fcInput = (fcChain[3] + fcChain[4])/fcPole;
fcExpected = fcMass (fcSquare - fcAngle)/fcPole;
fcReport = fcCompact[fcInput, fcRules, True, 1];
check["a conserved current becomes the external Dirac mass difference",
  fcReport["Status"] === "IMPROVED" && Expand[fcReport["Amplitude"] - fcExpected] === 0];
check["the weighted expression complexity decreases",
  First[fcRank[fcReport["Amplitude"]]] < First[fcRank[fcInput]]];
check["an exchanged mass and finite width stay inside the original off-shell propagator",
  Cases[fcReport["Amplitude"], _PropDen, Infinity] === {fcPole}];

fcReplay[entry_Association] := Module[{anchor, legs, before, conserved, nullReduced, reduced, residual},
  Switch[entry["Operation"],
    "ConservedSingleMomentumSandwich",
      anchor = entry["Anchor"]; legs = entry["ExternalLegs"]; before = entry["Before"];
      conserved = ConstructiveDiagrams`Private`preparationNumerators[before, Function[num,
        num /. (chain_SpinorChain /; Length[chain] === 3 && chain[[2]] === Mom[anchor]) :>
          SpinorChain[chain[[1]], -Total[Mom /@ DeleteCases[legs, anchor]], chain[[-1]]]]];
      nullReduced = ConstructiveDiagrams`Private`preparationMasslessMomentumTrial[
        conserved, entry["ExternalMassRules"], {}, {}];
      reduced = ConstructiveDiagrams`Private`preparationSpinorProducts[
        nullReduced, entry["ExternalMassRules"]];
      residual = ConstructiveDiagrams`Private`withProtectedPropagators[
        reduced - entry["After"], Function[value, Cancel[Together[value]]]];
      conserved === entry["AfterConservation"] &&
        nullReduced === entry["AfterNullMomentumReduction"] &&
        reduced === entry["AfterExternalSpinorProducts"] && residual === 0 &&
        entry["OnShellChannels"] === {} && entry["InternalOnShellIdentity"] === False,
    "ConservedPropagatorGroupNumerator",
      entry["ExactGroupedNumeratorResidual"] === 0 && AllTrue[entry["CoefficientRoute"], fcReplay],
    "UnchangedColorTensorCoefficientConservation",
      entry["ExactColorReconstructionResidual"] === 0 &&
        AllTrue[Flatten[entry["CoefficientRoutes"], 1], fcReplay],
    _, False]];
check["accepted reductions replay the exact conservation and external-only identity steps",
  fcReport["Route"] =!= {} && AllTrue[fcReport["Route"], fcReplay]];
check["accidentally supplied internal mass rules are discarded before ordinary reduction",
  Expand[fcCompact[fcInput, Append[fcRules, Mass[Multiparticle[1, 3]] -> 0], True, 1]["Amplitude"] - fcExpected] === 0];
check["disabling momentum conservation preserves the original amplitude",
  fcCompact[fcInput, fcRules, False, 1]["Amplitude"] === fcInput];
check["a zero budget preserves the original amplitude",
  fcCompact[fcInput, fcRules, True, 0]["Amplitude"] === fcInput];
check["an already short current is retained",
  fcCompact[fcExpected, fcRules, True, 1]["Amplitude"] === fcExpected];
fcColor = ColorDelta[ColorIndex[1], ColorIndex[2]];
fcColorReport = fcCompact[fcColor fcInput, fcRules, True, 1];
check["exact color coefficients simplify without changing a free color tensor",
  Expand[fcColorReport["Amplitude"] - fcColor fcExpected] === 0];
check["color reconstruction and all nested ordinary identities replay exactly",
  AllTrue[fcColorReport["Route"], fcReplay]];
fcOtherColor = ColorGenerator[ColorIndex[1], ColorIndex[2], ColorIndex[3]];
fcMixedColorReport = fcCompact[fcColor fcInput + fcOtherColor fcExpected, fcRules, True, 1];
check["independent color tensors keep their own coefficients",
  Expand[fcMixedColorReport["Amplitude"] - (fcColor + fcOtherColor) fcExpected] === 0];
fcOpaque = Dot[fcLeft, fcRight] fcInput;
check["unconverted noncommutative expressions are declined",
  fcCompact[fcOpaque, fcRules, True, 1]["Amplitude"] === fcOpaque];
fcRankCalls = 0;
Internal`InheritedBlock[{ConstructiveDiagrams`Private`preparationRank},
  Clear[ConstructiveDiagrams`Private`preparationRank];
  ConstructiveDiagrams`Private`preparationRank[___] := (fcRankCalls++; {1, 1, 1, "SpinorExpressionComplexity"});
  fcUnsupportedCurrent = ConstructiveDiagrams`Private`preparationChannelCurrentCompaction[
    fcInput, Append[fcRules, Mass[5] -> 0], True, 1];
  fcNoInsertion = fcCompact[fcExpected, fcRules, True, 1]];
check["a higher-point channel-current input is declined before any ranking",
  fcUnsupportedCurrent["Amplitude"] === fcInput && fcRankCalls === 0];
check["an amplitude without a single momentum insertion is declined before any ranking",
  fcNoInsertion["Amplitude"] === fcExpected && fcRankCalls === 0];
fcTimed = Internal`InheritedBlock[{ConstructiveDiagrams`Private`preparationRank},
  Clear[ConstructiveDiagrams`Private`preparationRank];
  ConstructiveDiagrams`Private`preparationRank[___] := (Pause[1]; {1, 1, 1, "SpinorExpressionComplexity"});
  First[AbsoluteTiming[fcTimedReport = fcCompact[fcInput, fcRules, True, 0.02]]]];
check["even initial ranking respects a short shared deadline",
  fcTimed < 0.5 && fcTimedReport["TimedOut"] === True && fcTimedReport["Amplitude"] === fcInput];
Print["Final conservation compaction: ", checks, " checks; ", Length[failures], " failures."];
If[failures =!= {}, Print[failures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:final-conservation-compaction-tests.wl"];
Quit[0];
