(* Existing positive spectators lift exact six-slot identities without changing
   a fraction, its off-shell propagators, or the external spin metadata. *)

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
psPrepare = ConstructiveDiagrams`Private`preparationPairedLiteralSpectators;
psSpace = ConstructiveDiagrams`Private`rspMultilinearIdentitySpace;
psA[i_] := Spinor["Spin", "Angle", i];
psS[i_] := Spinor["Spin", "Square", i];
psAngle[i_, j_] := SpinorChain[psA[i], psA[j]];
psSquare[i_, j_] := SpinorChain[psS[i], psS[j]];
psSandwich[i_, k_, j_] := SpinorChain[psA[i], Mom[k], psS[j]];
psRules = Table[Mass[i] -> Symbol["psMass" <> ToString[i]], {i, 6}];
psIdentity = psSquare[4, 5] (psAngle[1, 2] psSandwich[3, 1, 6] -
  psMass1 psAngle[3, 2] psSquare[1, 6] - psAngle[1, 3] psSandwich[2, 1, 6]);
psBasis = psAngle[1, 2] psSquare[4, 5] psSandwich[3, 2, 6];
psSpectator1 = psAngle[1, 3]; psSpectator2 = psSquare[2, 4];
psPole = PropDen[Mom[Multiparticle[1, 3]], psInternalMass];
psInput = 3 psCharge^2 psSpectator1 psSpectator2 (psIdentity + 7 psBasis)/psPole;
psInputHash = Hash[{psInput, psRules}, "SHA256"];
psResult = psPrepare[psInput, psRules, False, 10];
check["two actual positive factors expose a supported ordinary cofactor",
  psResult["Status"] === "IMPROVED" && psResult["Route"] =!= {}];
check["the complete candidate strictly improves weighted complexity",
  SpinorExpressionComplexity[psResult["Amplitude"]] < SpinorExpressionComplexity[psInput]];
check["all original inverse propagators remain off shell and unchanged",
  DeleteDuplicates[Cases[psResult["Amplitude"], _PropDen, Infinity]] === {psPole}];
psReplay[entry_, rules_] := Module[{core, space, rhs, spectator},
  core = Expand[Total[Lookup[entry["LiteralFactorWitnesses"], "Cofactor"]]];
  space = psSpace[core, rules];
  If[!AssociationQ[space], Return[False]];
  spectator = Times @@ entry["SpectatorFactors"];
  rhs = entry["CoefficientScalar"]
    Total[Map[Function[record, With[{certificate = record["Certificate"]},
      Total[MapThread[Times, {certificate["Coefficients"],
        certificate["OrdinaryIdentities"]}]]]], entry["CoefficientRoute"]]];
  TrueQ[Expand[space["ToFormal"][entry["AfterCoefficient"] - core - rhs]] === 0] &&
    TrueQ[Expand[entry["AfterNumerator"] - entry["BeforeNumerator"] -
      spectator (entry["AfterCoefficient"] - core)] === 0]
];
check["every lifted scalar and spectator coefficient replays exactly",
  AllTrue[psResult["Route"], psReplay[#, psRules] &]];
check["each published checkpoint carries exact literal and ordinary witnesses",
  AllTrue[psResult["Route"], #["ExactLiteralResidual"] === 0 &&
    #["ExactLiftedResidual"] === 0 && TrueQ[#["ExactFormalReplay"]] &]];
check["external-only ordinary normalization never enables an internal shell",
  AllTrue[psResult["Route"], #["InternalOnShellIdentity"] === False &&
    #["MomentumConservation"] === False &]];
check["all input expressions and unequal mass declarations are untouched",
  Hash[{psInput, psRules}, "SHA256"] === psInputHash];
psLabelMap = AssociationThread[Range[6], {19, 7, 31, 11, 29, 23}];
psRelabel[value_] := value /. {
  Spinor["Spin", variance : ("Upper" | "Lower"), type_, i_Integer, projection_] :>
    Spinor["Spin", variance, type, psLabelMap[i], projection],
  Spinor["Spin", type_, i_Integer] :> Spinor["Spin", type, psLabelMap[i]],
  Mom[i_Integer] :> Mom[psLabelMap[i]], Mass[i_Integer] :> Mass[psLabelMap[i]]};
psRelabeled = psPrepare[psRelabel[psInput], psRelabel[psRules], False, 10];
check["nonconsecutive labels retain the exact lifted identity",
  psRelabeled["Status"] === "IMPROVED" &&
    AllTrue[psRelabeled["Route"], psReplay[#, psRelabel[psRules]] &]];
psDual = psPrepare[psInput /. {"Angle" -> "Square", "Square" -> "Angle"}, psRules, False, 10];
check["the angle-square dual retains its ordinary signs",
  psDual["Status"] === "IMPROVED" && AllTrue[psDual["Route"], psReplay[#, psRules] &]];
psExplicit[value_] := value /. Spinor["Spin", type_, i_Integer] :>
  Spinor["Spin", If[OddQ[i], "Upper", "Lower"], type, i, If[OddQ[i], 1/2, -1/2]];
psExplicitResult = psPrepare[psExplicit[psInput], psRules, False, 10];
check["fixed Upper and Lower spin projections are retained",
  psExplicitResult["Status"] === "IMPROVED" &&
    AllTrue[Cases[psExplicitResult["Amplitude"], _Spinor, Infinity],
      MatchQ[#, Spinor["Spin", variance_, "Angle" | "Square", i_Integer, projection_] /;
        variance === If[OddQ[i], "Upper", "Lower"] &&
        projection === If[OddQ[i], 1/2, -1/2]] &]];
check["explicit spin-index candidates have exact ordinary replays",
  AllTrue[psExplicitResult["Route"], psReplay[#, psRules] &]];
psRepeatedInput = psInput /. psSpectator2 -> psSpectator1;
psRepeated = psPrepare[psRepeatedInput, psRules, False, 10];
check["two occurrences of a positive repeated factor are removed with multiplicity",
  psRepeated["Status"] === "IMPROVED" && AllTrue[psRepeated["Route"], psReplay[#, psRules] &]];
psLongSpectator = SpinorChain[psA[2], Mom[3], Mom[4], psA[4]];
psLongInput = psInput /. psSpectator2 -> psLongSpectator;
psLong = psPrepare[psLongInput, psRules, False, 10];
check["a higher-momentum spectator stays opaque to the six-slot coefficient proof",
  psLong["Status"] === "IMPROVED" && AllTrue[psLong["Route"], psReplay[#, psRules] &]];
check["all long-spectator checkpoints retain their separate numerator reconstruction",
  AllTrue[psLong["Route"], #["ExactNumeratorReconstructionResidual"] === 0 &] &&
    !FreeQ[psLong["Amplitude"], psLongSpectator]];
check["zero budget preserves the complete expression atomically",
  psPrepare[psInput, psRules, False, 0]["Amplitude"] === psInput];
psTiny = psPrepare[psInput, psRules, False, 10^-12];
check["early timeout exposes only the original or a completely witnessed checkpoint",
  psTiny["Amplitude"] === psInput ||
    AllTrue[psTiny["Route"], TrueQ[#["ExactFormalReplay"]] &]];
psFractional = psInput/Sqrt[psSpectator1];
check["fractional inverse powers are preserved without an attempted rewrite",
  psPrepare[psFractional, psRules, False, 5]["Amplitude"] === psFractional];
psUnknownPower = psInput psSpectator1^(-psUnknownExponent);
check["unknown inverse exponents are preserved without an attempted rewrite",
  psPrepare[psUnknownPower, psRules, False, 5]["Amplitude"] === psUnknownPower];
check["a conflicting external mass cannot license a cofactor identity",
  psPrepare[psInput, Append[psRules, Mass[1] -> psOtherMass], False, 5]["Amplitude"] === psInput];
check["nonscalar external mass data is declined",
  psPrepare[psInput, ReplacePart[psRules, 1 -> (Mass[1] -> {psMass1})], False, 5]["Amplitude"] === psInput];
check["noncommutative factors remain on the existing route",
  With[{input = psInput Dot[psX, psY]},
    psPrepare[input, psRules, False, 5]["Amplitude"] === input]];
check["a large factored power is rejected before Cartesian expansion",
  With[{input = (psInput + psBasis)^100000},
    psPrepare[input, psRules, False, 5]["Amplitude"] === input]];
psAgain = psPrepare[psResult["Amplitude"], psRules, False, 5];
check["the completed compact representative is idempotent",
  psAgain["Amplitude"] === psResult["Amplitude"]];
psOtherPole = PropDen[Mom[Multiparticle[2, 4]], psOtherInternalMass];
psTwoFractions = psInput + (psInput /. psPole -> psOtherPole);
psTwo = psPrepare[psTwoFractions, psRules, False, 10];
check["independent existing fractions keep their own inverse products",
  psTwo["Status"] === "IMPROVED" &&
    Sort[DeleteDuplicates[Cases[psTwo["Amplitude"], _PropDen, Infinity]]] === Sort[{psPole, psOtherPole}] &&
    AllTrue[psTwo["Route"], MemberQ[{1/psPole, 1/psOtherPole}, #["InverseFactors"]] &]];
Print["Paired spectator ordinary identity checks: ", checks - Length[failures], "/", checks, " passed."];
If[failures =!= {}, Print["Failures: ", failures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:" <> FileNameTake[$InputFileName]];
Quit[0];
