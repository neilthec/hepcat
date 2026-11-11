(* Conservative bounds cover complete coherent tensors without enumerating them. *)

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
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);
pcAngle[i_, j_] := SpinorChain[Spinor["Helicity", "Angle", i], Spinor["Helicity", "Angle", j]];
pcSpinAngle[i_, j_] := SpinorChain[Spinor["Spin", "Angle", i], Spinor["Spin", "Angle", j]];
pcDen[legs_, mass_] := PropDen[Mom[Multiparticle @@ legs], mass];
pcMetadata = <|"ExternalLegs" -> Range[6], "ExternalMassRules" -> Thread[(Mass /@ Range[6]) -> 1]|>;
pcSix = pcSpinAngle[1, 2] pcSpinAngle[5, 6] *
  SpinorChain[Spinor["Spin", "Angle", 3], Mom[2], Spinor["Spin", "Square", 4]]/
    (pcDen[{1, 2}, 0] pcDen[{3, 4}, 0] pcDen[{1, 3, 5}, 1]);
pcChart = ConstructiveDiagrams`Private`highEnergyAutomaticChart[pcSix, pcMetadata, 1, 1729, {}];
pcCertificate = ConstructiveDiagrams`Private`highEnergyUpperBoundCertificate[pcSix, pcChart, {}, -2];
check["the six-fermion tensor has a conservative E minus two certificate",
  pcCertificate["Status"] === "PASS" && pcCertificate["PowerUpperBound"] === -2];
check["every massive coherent basis column is bounded, covering all sixty-four components",
  pcCertificate["MassiveSpinBasisBoundVerified"] === True &&
    pcCertificate["MassiveSpinBasisLegs"] === Range[6] &&
    pcCertificate["CoherentSpinLegs"] === Range[6] &&
    Length[pcCertificate["SpinVariables"]] === 6 &&
    pcCertificate["CheckedExternalSpinors"] === 6];
pcMapped = ConstructiveDiagrams`Private`residueEvaluateChart[pcSix, pcChart, {}];
check["the complete supplied spin polynomial is multilinear in all six coherent variables",
  PolynomialQ[pcMapped, pcChart["SpinVariables"]] &&
    AllTrue[pcChart["SpinVariables"], Exponent[pcMapped, #] === 1 &]];
check["internal propagators are checked at generic high energy and never put on shell",
  pcCertificate["InternalOnShellConditions"] === {} &&
    Length[pcCertificate["GenericPropagators"]] === 3 &&
    AllTrue[pcCertificate["GenericPropagators"], #["LeadingInvariant"] =!= 0 &] &&
    AllTrue[Cases[pcSix, _PropDen, Infinity],
      !TrueQ[Together[ConstructiveDiagrams`Private`residueEvaluateChart[#, pcChart, {}] /.
        pcChart["PoleVariable"] -> 1/10] === 0] &]];
pcBadBasis = Join[pcChart, <|"SquareBasis" -> Join[pcChart["SquareBasis"],
  <|1 -> (ConstructiveDiagrams`Private`residueAdjugate[pcChart["Momenta"][1]])|>]|>];
pcBadBasisCertificate = ConstructiveDiagrams`Private`highEnergyUpperBoundCertificate[
  pcSix, pcBadBasis, {}, -2];
check["a cut-chart spin basis growing like E is rejected even if one endpoint could look bounded",
  pcBadBasisCertificate["Status"] === "NOT_CERTIFIED" &&
    pcBadBasisCertificate["Reason"] === "UnboundedMassiveSpinBasis"];
pcDegenerateMomenta = Join[pcChart["Momenta"], <|2 -> -pcChart["Momenta"][1],
  4 -> -pcChart["Momenta"][3], 6 -> -pcChart["Momenta"][5]|>];
pcDegenerate = ConstructiveDiagrams`Private`highEnergyUpperBoundCertificate[
  pcSix, Join[pcChart, <|"Momenta" -> pcDegenerateMomenta|>], {}, -2];
check["a zero leading physical invariant declines certification",
  pcDegenerate["Status"] === "NOT_CERTIFIED" &&
    pcDegenerate["Reason"] === "DegenerateLeadingPropagator"];
pcDecline[expression_] := ConstructiveDiagrams`Private`highEnergyUpperBoundCertificate[
  expression, pcChart, {}, -2]["Status"] === "NOT_CERTIFIED";
check["unknown coefficients are not counted as fixed numerical constants", pcDecline[pcUnknown pcSix]];
check["unknown or singular propagator masses are not certified",
  pcDecline[pcSix /. PropDen[p_, 1] :> PropDen[p, pcUnknownMass]] &&
    pcDecline[pcSix /. PropDen[p_, 1] :> PropDen[p, Infinity]]];
check["inverse massive spinor brackets and scalar invariants use the exact fallback",
  pcDecline[pcSix/pcSpinAngle[1, 3]] && pcDecline[pcSix/MomProd[1, 2]]];
check["negative powers of sums and unsupported heads use the exact fallback",
  pcDecline[pcSix/(1 + MomProd[1, 2])] && pcDecline[pcFunction[pcSix]]];
check["explicit references and composite spinor endpoints cannot be certified",
  pcDecline[pcSix /. Spinor["Spin", "Angle", 1] -> Spinor["Xi", "Angle", 1]] &&
    pcDecline[pcSix /. Spinor["Spin", "Angle", 1] ->
      Spinor["Spin", "Angle", Multiparticle[1, 2]]]];
check["a propagator with only one external leg on either side is excluded",
  pcDecline[pcSix pcDen[{1, 3, 5}, 1]/pcDen[{1, 2, 3, 4, 5}, 1]]];
pcVirtualityNumerator = ConstructiveDiagrams`Private`highEnergyUpperBoundCertificate[
  pcSix pcDen[{1, 4}, 3]/pcDen[{1, 4}, 0], pcChart, {}, -2];
check["a numerator virtuality is a bounded polynomial rather than an inverse pole obligation",
  pcVirtualityNumerator["Status"] === "PASS" &&
    pcVirtualityNumerator["PowerUpperBound"] === -2 &&
    pcVirtualityNumerator["InternalOnShellConditions"] === {}];
pcExternalNumerator = ConstructiveDiagrams`Private`highEnergyUpperBoundCertificate[
  pcSix PropDen[Mom[1], 0], pcChart, {}, 0];
check["a degenerate numerator invariant may be overbounded without certifying its inverse",
  pcExternalNumerator["Status"] === "PASS" && pcExternalNumerator["PowerUpperBound"] === 0 &&
    Length[pcExternalNumerator["GenericPropagators"]] === 3 &&
    pcDecline[pcSix/PropDen[Mom[1], 0]]];
pcMissingShellChart = Join[pcChart, <|"KinematicChecks" ->
  KeyDrop[pcChart["KinematicChecks"], {"ExternalMassShells"}]|>];
check["incomplete chart validation is not treated as a proven physical family",
  ConstructiveDiagrams`Private`highEnergyUpperBoundCertificate[
    pcSix, pcMissingShellChart, {}, -2]["Status"] === "NOT_CERTIFIED"];

(* Large but exactly homogeneous massless examples make the fallback cheap
   enough to test both acceptance and a genuine tiny bad-growth coefficient. *)
pcMasslessMetadata = <|"ExternalLegs" -> Range[4],
  "ExternalMassRules" -> Thread[(Mass /@ Range[4]) -> 0]|>;
pcLargeNumerator = Expand[(pcAngle[1, 2] pcAngle[3, 4] +
  2 pcAngle[1, 3] pcAngle[2, 4] + 7 pcAngle[1, 4] pcAngle[2, 3])^24];
pcLarge = pcLargeNumerator/pcDen[{1, 2}, 0]^24;
pcLargeChart = ConstructiveDiagrams`Private`highEnergyAutomaticChart[
  pcLarge, pcMasslessMetadata, 1, 1733, {}];
pcLargeGood = ConstructiveDiagrams`Private`highEnergyOneSample[pcLarge, pcLargeChart, {}, 0];
check["a large supported expression reports only a bound, not an invented observed power",
  LeafCount[pcLarge] >= 5000 && pcLargeGood["Status"] === "PASS" &&
    pcLargeGood["Method"] === "ConservativeExternalPowerCounting" &&
    MissingQ[pcLargeGood["ObservedPower"]] && pcLargeGood["ObservedPowerUpperBound"] === 0];
pcLargeBad = pcLarge (1 + MomProd[1, 2]/10^100);
pcLargeBadResult = TimeConstrained[
  ConstructiveDiagrams`Private`highEnergyOneSample[pcLargeBad, pcLargeChart, {}, 0], 30, $TimedOut];
check["above-bound counting falls back and a tiny genuine E squared term still fails",
  AssociationQ[pcLargeBadResult] && pcLargeBadResult["Status"] === "FAIL" &&
    pcLargeBadResult["Method"] === "ExactFixedAngleLaurentExpansion" &&
    pcLargeBadResult["ObservedPower"] === 2 &&
    pcLargeBadResult["UpperBoundAttempt"]["Status"] === "NOT_CERTIFIED"];
pcLargeSpurious = pcLarge/pcAngle[1, 3];
pcLargeReference = pcLarge /. Spinor["Helicity", "Angle", 1] -> Spinor["Xi", "Angle", 1];
check["large explicit-reference expressions decline the shortcut",
  LeafCount[pcLargeReference] >= 5000 &&
    ConstructiveDiagrams`Private`highEnergyUpperBoundCertificate[
      pcLargeReference, pcLargeChart, {}, 0]["Status"] === "NOT_CERTIFIED"];
pcSpuriousChart = ConstructiveDiagrams`Private`highEnergyAutomaticChart[
  pcLargeSpurious, pcMasslessMetadata, 1, 1733, {}];
pcSpuriousSample = TimeConstrained[ConstructiveDiagrams`Private`highEnergyOneSample[
  pcLargeSpurious, pcSpuriousChart, {}, -1], 30, $TimedOut];
check["a nondegenerate massless bracket contributes one inverse power of energy",
  AssociationQ[pcSpuriousSample] && pcSpuriousSample["Status"] === "PASS" &&
    pcSpuriousSample["Method"] === "ConservativeExternalPowerCounting" &&
    MissingQ[pcSpuriousSample["ObservedPower"]] &&
    pcSpuriousSample["ObservedPowerUpperBound"] === -1 &&
    MatchQ[pcSpuriousSample["UpperBoundCertificate"]["MasslessBracketDenominators"],
      {KeyValuePattern[{"Bracket" -> pcAngle[1, 3], "LeadingCoefficient" -> Except[0]}]}]];
pcSameAngle = Join[pcSpuriousChart["Angle"], <|3 -> pcSpuriousChart["Angle"][1]|>];
pcDegenerateBracket = ConstructiveDiagrams`Private`highEnergyUpperBoundCertificate[
  pcLargeSpurious, Join[pcSpuriousChart, <|"Angle" -> pcSameAngle|>], {}, -1];
check["a vanishing leading massless bracket is rejected",
  pcDegenerateBracket["Status"] === "NOT_CERTIFIED" &&
    pcDegenerateBracket["Reason"] === "DegenerateLeadingMasslessBracket"];
pcReferenceChart = ConstructiveDiagrams`Private`highEnergyAutomaticChart[
  pcLargeReference, pcMasslessMetadata, 1, 1733, {}];
pcReferenceFallback = TimeConstrained[ConstructiveDiagrams`Private`highEnergyOneSample[
  pcLargeReference, pcReferenceChart, {}, 0], 30, $TimedOut];
check["a large explicit-reference expression remains subject to the exact fallback",
  AssociationQ[pcReferenceFallback] && pcReferenceFallback["Status"] === "PASS" &&
    pcReferenceFallback["Method"] === "ExactFixedAngleLaurentExpansion"];
pcAggregate = ConstructiveDiagrams`Private`highEnergyCheckReport[
  pcLarge, pcMasslessMetadata, 2, 1733, {}, 30, 0];
check["report aggregation preserves bound-only semantics for every certified sample",
  pcAggregate["Status"] === "PASS" && MissingQ[pcAggregate["ObservedPower"]] &&
    pcAggregate["ObservedPowerUpperBound"] === 0 &&
    pcAggregate["Method"] === "ConservativeExternalPowerCounting"];
pcMockAggregate[samples_] := Module[{index = 0},
  Block[{ConstructiveDiagrams`Private`highEnergyOneSample =
      Function[{expression, chartArgument, ruleArgument, boundArgument}, index++; samples[[index]]]},
    ConstructiveDiagrams`Private`highEnergyCheckReport[
      1, pcMasslessMetadata, 2, 1733, {}, 30, 0]]];
pcBoundOnlySample = <|"Status" -> "PASS", "ObservedPower" -> Missing["UpperBoundCertificate"],
  "ObservedPowerUpperBound" -> 0, "Method" -> "ConservativeExternalPowerCounting",
  "UpperBoundCertificate" -> <|"Status" -> "PASS"|>, "Diagnostics" -> {}|>;
pcPartialAggregate = pcMockAggregate[{pcBoundOnlySample,
  <|"Status" -> "UNRESOLVED", "Diagnostics" -> {}|>}];
check["an unfinished direction prevents an overall energy-bound claim",
  pcPartialAggregate["Status"] === "UNRESOLVED" &&
    MissingQ[pcPartialAggregate["ObservedPower"]] &&
    MissingQ[pcPartialAggregate["ObservedPowerUpperBound"]]];
pcMixedAggregate = pcMockAggregate[{pcBoundOnlySample,
  <|"Status" -> "PASS", "ObservedPower" -> -1, "ObservedPowerUpperBound" -> -1,
    "Method" -> "ExactFixedAngleLaurentExpansion", "Diagnostics" -> {}|>}];
check["a mixture of measured and bound-only samples retains only the combined upper bound",
  pcMixedAggregate["Status"] === "PASS" && MissingQ[pcMixedAggregate["ObservedPower"]] &&
    pcMixedAggregate["ObservedPowerUpperBound"] === 0 &&
    pcMixedAggregate["Method"] === "ConservativePowerCountingWithLaurentFallback"];
Print[checks, " conservative high-energy checks; ", Length[failures], " failures."];
If[failures =!= {}, Print[failures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:high-energy-power-counting-tests.wl"];
Quit[0];
