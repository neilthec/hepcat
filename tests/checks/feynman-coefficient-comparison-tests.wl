(* Preserve exact full-spin comparison while reducing scalar coefficients first. *)

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
fcModel = ReadModel[FileNameJoin[{root, "models"}], 3];
fcRules = ModelParameterRules[fcModel, {"Me" -> 1, "EE" -> 1}];
fcChannels = FactorizationAmplitudeChannels[{"e", "E", "A.+", "A.+"}, fcModel];
fcTargets = ConstructiveDiagrams`Private`residueSimpleInputs[0, fcChannels, fcModel]["Targets"];
fcChart = ConstructiveDiagrams`Private`amplitudeRationalComparisonChart[fcTargets, 1, 1777, fcRules];
fcVariables = fcChart["SpinVariables"];
fcBase = SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] +
  2 SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] +
  SpinorChain[Spinor["Spin", "Angle", 1], Mom[3], Spinor["Spin", "Square", 2]];
fcNew[expression_, benchmark_, tolerance_] := ConstructiveDiagrams`Private`amplitudeComparisonOneSample[
  expression, benchmark, fcChart, 1, 1777, 0, fcRules, fcRules, tolerance];
(* The former implementation is retained only as a small independent regression
   oracle. No package definitions are replaced by this test. *)
fcOld[expression_, benchmark_, tolerance_] := Module[{a, b, difference, opposite, states, signs},
  a = Cancel[Together[ConstructiveDiagrams`Private`residueEvaluateChart[
    Rationalize[expression, 0], fcChart, fcRules]]];
  b = Cancel[Together[ConstructiveDiagrams`Private`residueEvaluateChart[
    Rationalize[benchmark, 0], fcChart, fcRules]]];
  difference = Cancel[Together[a - b]]; opposite = Cancel[Together[a + b]];
  states = ConstructiveDiagrams`Private`amplitudeComparisonState[#, fcVariables, tolerance] & /@
    {difference, opposite};
  signs = Pick[{1, -1}, states, "Zero"];
  <|"Status" -> Which[MemberQ[signs, 1], "PASS", MemberQ[signs, -1], "Opp Sign",
      states === {"Nonzero", "Nonzero"}, "FAIL", True, "UNRESOLVED"],
    "CalculatedAmplitude" -> a, "FeynmanAmplitude" -> b, "Difference" -> difference,
    "OppositeSignDifference" -> opposite, "MatchingSigns" -> signs|>];
Do[
  fcOldValue = fcOld[case[[2]], case[[3]], case[[4]]];
  fcNewValue = fcNew[case[[2]], case[[3]], case[[4]]];
  check["small-case classification preserves the old comparison: " <> case[[1]],
    fcNewValue["Status"] === fcOldValue["Status"] &&
      fcNewValue["MatchingSigns"] === fcOldValue["MatchingSigns"]];
  check["all four displayed polynomial values remain exact: " <> case[[1]],
    AllTrue[{"CalculatedAmplitude", "FeynmanAmplitude", "Difference", "OppositeSignDifference"},
      Cancel[Together[fcNewValue[#] - fcOldValue[#]]] === 0 &]],
  {case, {{"equal", fcBase, fcBase, 0}, {"opposite", -fcBase, fcBase, 0},
    {"different", 2 fcBase, fcBase, 0}, {"zero", 0, 0, 0},
    {"complex phase", I fcBase, fcBase, 0},
    {"tiny mixed coefficient", fcBase + Times @@ fcVariables/10^100, fcBase,
      "Tolerance" /. Options[CheckAmplitudes]}}}];
fcTiny = fcNew[fcBase + Times @@ fcVariables/10^100, fcBase,
  "Tolerance" /. Options[CheckAmplitudes]];
check["a genuine exact tiny mixed coefficient fails even below the numerical tolerance",
  fcTiny["Status"] === "FAIL" &&
    MemberQ[fcTiny["ComparedSpinMonomials"], ConstantArray[1, Length[fcVariables]]]];
fcScalar = ConstructiveDiagrams`Private`amplitudeComparisonCoefficientState[{1/10^100},
  "Tolerance" /. Options[CheckAmplitudes]];
check["the scalar numerator convention does not turn a tiny rational into zero", fcScalar === "Nonzero"];
fcSixVariables = Array[fcSpin, 6];
fcSixPolynomial = Times @@ (1 + # & /@ fcSixVariables);
fcSixCoefficients = ConstructiveDiagrams`Private`amplitudeComparisonPolynomial[fcSixPolynomial, fcSixVariables];
check["all sixty-four six-fermion coherent monomials are retained",
  Length[fcSixCoefficients] === 64 &&
    Expand[ConstructiveDiagrams`Private`amplitudeComparisonPolynomialValue[
      fcSixCoefficients, fcSixVariables] - fcSixPolynomial] === 0];
fcVectorPolynomial = Times @@ (1 + # + #^2 & /@ fcSixVariables);
fcVectorCoefficients = ConstructiveDiagrams`Private`amplitudeComparisonPolynomial[
  fcVectorPolynomial, fcSixVariables];
check["larger coherent tensors retain every monomial without a fixed coefficient cap",
  Length[fcVectorCoefficients] === 729 &&
    Expand[ConstructiveDiagrams`Private`amplitudeComparisonPolynomialValue[
      fcVectorCoefficients, fcSixVariables] - fcVectorPolynomial] === 0];
fcU = First[fcVariables]; fcV = Last[fcVariables];
fcRemovable = fcNew[(1 - fcU^2)/(1 - fcU), 1 + fcU, 0];
check["ordinary cancellation recovers a removable spin denominator before extraction",
  fcRemovable["Status"] === "PASS" && fcRemovable["Difference"] === 0];
fcSpinDenominator = fcNew[1/(1 + fcU fcV), 1/(1 + fcU fcV), 0];
check["genuinely spin-dependent denominators remain unsupported",
  fcSpinDenominator["Status"] === "UNRESOLVED" &&
    fcSpinDenominator["MatchingSigns"] === {}];
fcExpectedOnly = fcNew[0, Times @@ fcVariables/10^100, 0];
check["the comparison includes monomials appearing only in the benchmark",
  fcExpectedOnly["Status"] === "FAIL" &&
    Length[fcExpectedOnly["ComparedSpinMonomials"]] === 1];

(* The recorded default six-point seed produces an accidental spectator pole
   on its first rational geometry. Retry geometry, never an amplitude value. *)
fcSixExternal = Range[6];
fcSixMassRules = Thread[(Mass /@ fcSixExternal) -> fcMass];
fcSixRules = {fcMass -> 1/2000};
fcAllCuts = DeleteDuplicates[ConstructiveDiagrams`Private`canonicalPhysicalCut[#, fcSixExternal] & /@
  Join[Subsets[fcSixExternal, {2}], Subsets[fcSixExternal, {3}]]];
fcSixCuts = SortBy[Select[fcAllCuts, If[Length[#] === 2,
  Total[(-1)^#] === 0, Abs[Total[(-1)^#]] === 1] &], {Length[#], #} &];
fcSixChannels = ConstructiveDiagrams`Private`residueSimpleChannel[
  #, If[Length[#] === 2, 0, fcMass], fcSixExternal, fcSixMassRules, fcModel] & /@ fcSixCuts;
fcSixChannels = Join[fcSixChannels, (ConstructiveDiagrams`Private`residueSimpleChannel[
  #, 1/2000, fcSixExternal, fcSixMassRules, fcModel] &) /@ Select[fcSixCuts, Length[#] === 3 &]];
fcBadGeometry = ConstructiveDiagrams`Private`completionCutChart[
  First[fcSixChannels], 1, 3174522571359952913, fcSixRules];
fcBadPole = fcBadGeometry["Chart"]["PoleVariable"];
fcAccidentalPole = ConstructiveDiagrams`Private`residueEvaluateChart[
  PropDen[Mom[Multiparticle[4, 5]], 0], fcBadGeometry["Chart"], fcSixRules];
check["the replayed first geometry really lies on an unintended pole for every coordinate",
  Length[fcSixChannels] === 27 && Cancel[Together[fcAccidentalPole]] === 0];
fcRetriedChart = ConstructiveDiagrams`Private`amplitudeRationalComparisonChart[
  fcSixChannels, 1, 3174522571359952913, fcSixRules];
check["a fresh rational geometry passes all twenty-seven original pole obligations",
  AssociationQ[fcRetriedChart] && fcRetriedChart["SamplingConstruction"] === "ExactRationalComplexOffCut" &&
    fcRetriedChart["OffCutKinematicValidation"]["Valid"] === True &&
    Length[fcRetriedChart["OffCutKinematicValidation"]["PhysicalPropagatorsNonzero"]] === 27 &&
    And @@ fcRetriedChart["OffCutKinematicValidation"]["PhysicalPropagatorsNonzero"]];
check["the rejected first geometry and its unintended cut remain visible",
  First[fcRetriedChart["RationalSamplingAttempts"]]["Status"] === "REJECTED" &&
    MemberQ[First[fcRetriedChart["RationalSamplingAttempts"]]["RejectedPhysicalCuts"], {4, 5}] &&
    Last[fcRetriedChart["RationalSamplingAttempts"]]["Status"] === "ACCEPTED"];
check["accepted off-cut momenta and every massive spin basis obey exact kinematics",
  And @@ fcRetriedChart["OffCutKinematicValidation"]["ExternalMassShells"] &&
    fcRetriedChart["OffCutKinematicValidation"]["MomentumConservation"] === True &&
    And @@ fcRetriedChart["OffCutKinematicValidation"]["ExternalSpinorBases"] &&
    fcRetriedChart["OffCutKinematicValidation"]["InternalOnShellConditions"] === {}];
fcSixSpinVariables = fcRetriedChart["SpinVariables"];
fcBadAmplitude = ConstructiveDiagrams`Private`amplitudeComparisonOneSample[
  1 + (Times @@ fcSixSpinVariables)/10^100, 1, fcRetriedChart, 1, 1777, 0,
  fcSixRules, fcSixRules, "Tolerance" /. Options[CheckAmplitudes]];
check["a genuinely bad amplitude still fails on the accepted replacement geometry",
  fcBadAmplitude["Status"] === "FAIL" &&
    MemberQ[fcBadAmplitude["ComparedSpinMonomials"], ConstantArray[1, 6]]];
fcDamagedChart = Join[fcRetriedChart, <|"SquareBasis" -> Join[
  fcRetriedChart["SquareBasis"], <|1 -> IdentityMatrix[2]|>]|>];
check["a damaged external spinor basis cannot satisfy the replacement geometry guard",
  ConstructiveDiagrams`Private`amplitudeComparisonChartValidation[
    fcDamagedChart, fcSixChannels, fcSixRules]["Valid"] === False];
Print[checks, " Feynman coefficient-comparison checks; ", Length[failures], " failures."];
If[failures =!= {}, Print[failures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:feynman-coefficient-comparison-tests.wl"];
Quit[0];
