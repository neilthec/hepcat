(* Real five-point affine identities must remain cheap in both chiralities.
   The fixture retains formal model parameters and all spin dependence.
   No numerical parameter substitution is made in the affine directions.
   Prepared amplitudes below are regression inputs, never construction data. *)

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


fpRoot = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{fpRoot, "source", "HEPCAT.wl"}]];
$HistoryLength = 0; fpChecks = 0; fpFailures = {};
SetAttributes[fpCheck, HoldRest];
fpCheck[name_, assertion_] := (fpChecks++; If[!TrueQ[assertion],
  AppendTo[fpFailures, name]; Print["FAIL: ", name]]);
fpModel = ReadModel[FileNameJoin[{fpRoot, "models"}], 3];
fpFixture = Get[FileNameJoin[{fpRoot, "tests", "checks", "fixtures",
  "high-energy-five-point-four-fermion-zero.wl"}]];
(* Bind symbolic fixture parameters to this checkout's model scope, never
   to Global values or numerical constants. *)
fpFixture = fpFixture /. parameter_ModelParameter :>
  ModelParameter[fpModel, ModelParameterName[parameter]];
fpRules = fpFixture["ExternalMassRules"];
fpMass = Mass[1] /. fpRules;
fpCheck["the fixture covers both complete five-point coherent-spin families",
  Length[fpFixture["PositiveDirections"]] === 2 &&
    Length[fpFixture["NegativeDirections"]] === 2 &&
    Sort[fpRules] === Sort[{Mass[1] -> fpMass, Mass[2] -> fpMass, Mass[3] -> fpMass,
      Mass[4] -> fpMass, Mass[5] -> 0}]];
fpProofs = Association@Table[
  fpChirality -> Table[
    fpInput = fpFixture[fpChirality][[fpDirectionIndex]];
    fpProof = ConstructiveDiagrams`Private`completionHEZeroDirection[
      fpInput, fpRules, fpModel, 45];
    Print["Proof ", fpChirality, " ", fpDirectionIndex, ": ",
      KeyTake[fpProof, {"Status", "Method", "ElapsedTime", "ChartAttempts"}]];
    fpCheck["generic exact identity: " <> fpChirality <> ToString[fpDirectionIndex],
      fpProof["Status"] === "PROVED_ZERO" &&
        fpProof["OriginalDirection"] === fpInput &&
        fpProof["NumericalKinematicSamples"] === 0 &&
        fpProof["InternalOnShellConditions"] === {}];
    fpCheck["bounded ordinary compaction precedes the dense tensor proof: " <>
        fpChirality <> ToString[fpDirectionIndex],
      (fpDirectionIndex === 1 || AssociationQ[fpProof["OrdinaryCompactionRetry"]] &&
        fpProof["OrdinaryCompactionRetry"]["Status"] === "IMPROVED" &&
        fpProof["OrdinaryCompactionRetry"]["CoherentComponentEstimate"] === 16 &&
        fpProof["OrdinaryCompactionRetry"]["Route"] =!= {} &&
        AllTrue[fpProof["OrdinaryCompactionRetry"]["Route"],
          Lookup[#, "InternalOnShellConditions", {}] === {} &]) &&
        fpProof["ElapsedTime"] < 47];
    fpProof, {fpDirectionIndex, 2}],
  {fpChirality, {"PositiveDirections", "NegativeDirections"}}];
Do[
  fpProof = Last[fpProofs[fpChirality]];
  fpComponent = fpProof["ComponentProof"];
  fpCheck["the nontrivial direction proves all sixteen tensor components: " <> fpChirality,
    AssociationQ[fpComponent] && fpComponent["Status"] === "PROVED_ZERO" &&
      fpComponent["ComponentCount"] === 16 && fpComponent["ComponentsChecked"] === 16 &&
      TrueQ[fpComponent["DegreeBoundsVerified"]] &&
      TrueQ[fpComponent["FullCoefficientCoverage"]] &&
      TrueQ[fpComponent["ReconstructionVerified"]] &&
      TrueQ[fpComponent["AllComponentsZero"]]];
  fpCheck["representation planning ranks both massive basis chiralities: " <> fpChirality,
    Length[fpProof["ChartPlanning"]] >= 2 &&
      Sort[DeleteDuplicates[Lookup[Lookup[fpProof["ChartPlanning"], "Choice"],
        "MassiveSpinorBasis"]]] === {"Angle", "Square"} &&
      AllTrue[fpProof["ChartPlanning"],
        IntegerQ[#["MappedPrimitiveLeaves"]] && #["MappedPrimitiveLeaves"] >= 0 &]],
  {fpChirality, {"PositiveDirections", "NegativeDirections"}}];
fpSquareChart = Last[fpProofs["NegativeDirections"]]["Chart"];
fpCompactDirection = Last[fpProofs["PositiveDirections"]]["OrdinaryCompactionRetry"]["Amplitude"];
fpCheckEstimate = ConstructiveDiagrams`Private`amplitudeCheckWork[
  fpCompactDirection, <|"ExternalMassRules" -> fpRules|>];
fpCheck["automatic checking accounts for a compact sixteen-component tensor",
  LeafCount[fpCompactDirection] < 3000 &&
    fpCheckEstimate["CoherentComponents"] === 16 && fpCheckEstimate["WeightedLeaves"] > 3000];
fpCheck["the opposite fpChirality uses a fully validated square-constant basis",
  fpSquareChart["MassiveSpinorBasis"] === "Square" &&
    TrueQ[fpSquareChart["ExternalSpinorBasesValidated"]] &&
    AllTrue[Keys[fpSquareChart["AngleBasis"]], Function[leg,
      ConstructiveDiagrams`Private`residueExactZero[
        fpSquareChart["AngleBasis"][leg].{{0, 1}, {-1, 0}}.
          Transpose[fpSquareChart["SquareBasis"][leg]] - fpSquareChart["Momenta"][leg]]]]];

fpTensor = SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] *
  SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] *
  SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Mom[2],
    Spinor["Helicity", "Square", 5]];
fpParameterRules = ModelParameterRules[fpModel];
Do[
  fpCompactAmplitude = fpFixture["PreparedAmplitudes"][fpHelicity];
  fpEnergyChart = ConstructiveDiagrams`Private`highEnergyAutomaticChart[
    fpCompactAmplitude, <|"ExternalLegs" -> Range[5], "ExternalMassRules" -> fpRules|>,
    1, 410077, fpParameterRules];
  fpEnergySample = ConstructiveDiagrams`Private`highEnergyOneSample[
    fpCompactAmplitude, fpEnergyChart, fpParameterRules, -1];
  fpCheck["a compact four-spin amplitude uses its exact uniform growth bound: " <> fpHelicity,
    LeafCount[fpCompactAmplitude] < 5000 && fpEnergySample["Status"] === "PASS" &&
      fpEnergySample["Method"] === "ConservativeExternalPowerCounting" &&
      MissingQ[fpEnergySample["ObservedPower"]] &&
      fpEnergySample["ObservedPowerUpperBound"] === -1];
  fpBoundCertificate = fpEnergySample["UpperBoundCertificate"];
  fpCheck["the growth certificate covers every coherent spin and keeps internal lines off shell: " <> fpHelicity,
    TrueQ[fpBoundCertificate["CoherentSpinPolynomial"]] &&
      Length[fpEnergySample["SpinVariables"]] === 4 &&
      Sort[fpBoundCertificate["CoherentSpinLegs"]] === {1, 2, 3, 4} &&
      TrueQ[fpBoundCertificate["MassiveSpinBasisBoundVerified"]] &&
      fpBoundCertificate["InternalOnShellConditions"] === {} &&
      TrueQ[fpEnergyChart["KinematicChecks"]["MomentumConservation"]] &&
      AllTrue[fpEnergyChart["KinematicChecks"]["ExternalMassShells"], TrueQ]],
  {fpHelicity, {"Positive", "Negative"}}];
fpBadGrowthBound = ConstructiveDiagrams`Private`highEnergyUpperBoundCertificate[
  fpTensor, fpEnergyChart, fpParameterRules, -1];
fpBadGrowthSample = ConstructiveDiagrams`Private`highEnergyOneSample[
  fpTensor, fpEnergyChart, fpParameterRules, -1];
fpCheck["a genuine growing tensor falls back to the exact Laurent calculation and fails",
  fpBadGrowthBound["Status"] === "NOT_CERTIFIED" &&
    fpBadGrowthSample["Status"] === "FAIL" &&
    fpBadGrowthSample["Method"] === "ExactFixedAngleLaurentExpansion"];
fpDeformed = First[fpFixture["PositiveDirections"]] + fpTensor/10^30;
fpBadProof = ConstructiveDiagrams`Private`completionHEZeroDirection[
  fpDeformed, fpRules, fpModel, 20];
fpCheck["a tiny genuine tensor deformation is never discarded as zero",
  fpBadProof["Status"] === "NOT_PROVED" &&
    fpBadProof["OriginalDirection"] === fpDeformed];
fpTimed = ConstructiveDiagrams`Private`completionHEZeroDirection[
  Last[fpFixture["NegativeDirections"]], fpRules, fpModel, 1/1000];
fpCheck["an expired proof retains its exact original direction",
  fpTimed["Status"] === "NOT_PROVED" &&
    fpTimed["OriginalDirection"] === Last[fpFixture["NegativeDirections"]]];
(* The negative fixture is also the exact two-direction family generated by
   the user's failing notebook seed4515496156666242158. Test the fast proof
   itself as well as the older dense portfolio above. *)
fpNativeProofs = <||>; fpNativeBackendAvailable = True;
fpRelabelMap = <|1 -> 9, 2 -> 5, 3 -> 13, 4 -> 7, 5 -> 2|>;
fpRelabeledDirection = ConstructiveDiagrams`Private`relabelLegs[
  3 Last[fpFixture["NegativeDirections"]] - 5 First[fpFixture["NegativeDirections"]], fpRelabelMap];
fpRelabeledRules = ConstructiveDiagrams`Private`relabelLegs[fpRules, fpRelabelMap];
fpNativeCases = <|"PositiveHardDirection" -> {Last[fpFixture["PositiveDirections"]], fpRules},
  "FailedSeedHardDirection" -> {Last[fpFixture["NegativeDirections"]], fpRules},
  "RelabeledAffineCombination" -> {fpRelabeledDirection, fpRelabeledRules}|>;
Do[
  fpNativePair = fpNativeCases[fpNativeName];
  fpNativeProof = ConstructiveDiagrams`Private`completionHENativeZeroDirection[
    First[fpNativePair], Last[fpNativePair], fpModel, 20];
  AssociateTo[fpNativeProofs, fpNativeName -> fpNativeProof];
  Print["Single-null exact proof ", fpNativeName, ": ",
    KeyTake[fpNativeProof, {"Status", "Method", "ElapsedTime"}]];
  fpNativeOptionalReason = Lookup[Lookup[fpNativeProof, "NativeCertificate", <||>],
    "FailureReason", Lookup[fpNativeProof, "Method", ""]];
  fpNativeAvailable = !(fpNativeProof["Status"] =!= "PROVED_ZERO" && MemberQ[
    {"CompilerUnavailable", "BoostHeadersUnavailable", "NativeRuntimeUnavailable",
      "NativePythonUnavailable", "NativeProcessUnavailable"}, fpNativeOptionalReason]);
  fpNativeBackendAvailable = fpNativeBackendAvailable && fpNativeAvailable;
  If[fpNativeAvailable,
    fpCheck["a generic dense single-null proof certifies the full direction: " <> fpNativeName,
      fpNativeProof["Status"] === "PROVED_ZERO" &&
        fpNativeProof["Method"] === "DenseRestSingleNullNativePolynomialIdentity" &&
        fpNativeProof["OriginalDirection"] === First[fpNativePair] &&
        TrueQ[fpNativeProof["CompleteCoherentCoefficientCoverage"]] &&
        TrueQ[fpNativeProof["DenominatorFactorsProvedNonzero"]] &&
        fpNativeProof["NumericalKinematicSamples"] === 0 &&
        fpNativeProof["CoherentSpinSamples"] === 0 && fpNativeProof["InternalOnShellConditions"] === {}],
    Print["Optional native proof unavailable: ", fpNativeOptionalReason];
    fpCheck["an unavailable optional backend cannot produce a zero certificate: " <> fpNativeName,
      fpNativeProof["Status"] === "NOT_PROVED" &&
        !TrueQ[Lookup[fpNativeProof, "CompleteCoherentCoefficientCoverage", False]] &&
        fpNativeProof["OriginalDirection"] === First[fpNativePair]]];
  fpNativePrepared = Lookup[fpNativeProof, "PreparationCertificate", <||>];
  fpNativePolynomial = Lookup[fpNativePrepared, "PolynomialExtraction", <||>];
  fpCheck["sparse coefficient extraction includes all coherent spin states: " <> fpNativeName,
    Length[fpNativePrepared["Chart"]["SpinVariables"]] === 4 &&
      fpNativePolynomial["Status"] === "COMPLETE" &&
      TrueQ[fpNativePolynomial["FullCoefficientCoverage"]] &&
      TrueQ[fpNativePolynomial["ReconstructionVerified"]] &&
      fpNativePolynomial["ComponentCount"] > 0 &&
      fpNativePolynomial["ComponentCount"] <= 16 &&
      AllTrue[fpNativePolynomial["ComponentMonomials"],
        Length[#] === 4 && AllTrue[#, MemberQ[{0, 1}, #] &] &]];
  fpNativeScalars = fpNativePrepared["ScalarVariableCertificate"];
  fpNativeLift = fpNativePrepared["RationalLiftCertificate"];
  fpCheck["exact scalar and rational-lift witnesses accompany each proof: " <> fpNativeName,
    TrueQ[fpNativeScalars["ExactScalarRoundTrip"]] &&
      TrueQ[fpNativeScalars["SharedRationalRootUnits"]] &&
      AllTrue[fpNativeScalars["RuleWitnesses"], TrueQ[#["Verified"]] &] &&
      TrueQ[fpNativeLift["ReconstructionVerified"]] &&
      TrueQ[fpNativeLift["AllPowersNonnegative"]] &&
      TrueQ[fpNativeLift["AllVariablesDeclared"]] &&
      AllTrue[fpNativeLift["LocalWitnesses"],
        TrueQ[#["LocalFactorReconstruction"]] && TrueQ[#["PositiveDenominatorPowers"]] &] &&
      TrueQ[fpNativePrepared["ScalarSpecializationCertificate"]["EveryFactorNonzeroAfterNumericRestoration"]] &&
      fpNativePrepared["ScalarSpecializationCertificate"]["FactorCount"] ===
        Length[fpNativePrepared["DenominatorRootIndices"]] &&
      fpNativePrepared["ScalarSpecializationCertificate"]["WitnessCount"] ===
        Length[fpNativePrepared["DenominatorRootIndices"]] &&
      AllTrue[fpNativePrepared["ScalarSpecializationCertificate"]["Witnesses"],
        TrueQ[#["AlgebraicMembershipVerified"]] &] &&
      fpNativePrepared["MassNormalizationUsed"] === False &&
      fpNativePrepared["NumericalParameterSubstitutions"] === {}],
  {fpNativeName, Keys[fpNativeCases]}];
fpRelabeledChart = fpNativeProofs["RelabeledAffineCombination"]["PreparationCertificate"]["Chart"];
fpCheck["the chart discovers a lone null leg after nonconsecutive relabeling",
  fpRelabeledChart["NullAxisLegs"] === {2} &&
    Sort[Keys[fpRelabeledChart["Momenta"]]] === {2, 5, 7, 9, 13} &&
    TrueQ[fpRelabeledChart["MomentumConservationValidated"]] &&
    AllTrue[Values[fpRelabeledChart["ExternalMassShellChecks"]], TrueQ] &&
    TrueQ[fpRelabeledChart["DependentShellLinearReconstruction"]]];

fpNumericChartRules = {Mass[1] -> 2, Mass[2] -> 3, Mass[3] -> 5, Mass[4] -> 0};
Do[
  fpDenseChart = ConstructiveDiagrams`Private`completionHERestSingleNullSymbolicChart[
    fpNumericChartRules, Automatic, Automatic, Automatic, fpNullKind, fpMassiveKind];
  fpCheck["unequal-mass four-point chart reconstructs both spin-frame chiralities: " <>
      fpNullKind <> fpMassiveKind,
    AssociationQ[fpDenseChart] && TrueQ[fpDenseChart["MomentumConservationValidated"]] &&
      AllTrue[Values[fpDenseChart["ExternalMassShellChecks"]], TrueQ] &&
      AllTrue[Values[fpDenseChart["ExternalSpinorBasesValidated"]], TrueQ] &&
      TrueQ[fpDenseChart["DependentShellLinearReconstruction"]] &&
      fpDenseChart["NumericalKinematicSamples"] === 0 &&
      fpDenseChart["InternalOnShellConditions"] === {}],
  {fpNullKind, {"Angle", "Square"}}, {fpMassiveKind, {"Angle", "Square"}}];

fpOtherMass = ModelParameter[ModelParameterName[fpMass], "distinct-test-model", "Real"];
fpScalarExpression = Sqrt[2] fpTensor + fpOtherMass fpTensor/Sqrt[2];
fpScalarMap = ConstructiveDiagrams`Private`completionHEExactScalarVariables[
  fpScalarExpression, fpRules, 3];
fpScalarForward = fpScalarMap["ForwardRules"];
fpRadicalUnit = fpScalarMap["PrimeRootVariables"][2];
fpCheck["reciprocal radicals share one exact unit rather than independent variables",
  fpScalarMap["Status"] === "COMPLETE" &&
    TrueQ[fpScalarMap["ExactScalarRoundTrip"]] &&
    TrueQ[fpScalarMap["SharedRationalRootUnits"]] &&
    Cancel[(Sqrt[2] /. fpScalarForward) - 2 (1/Sqrt[2] /. fpScalarForward)] === 0 &&
    AllTrue[fpScalarMap["RuleWitnesses"], TrueQ[#["Verified"]] &]];
fpCheck["model-local parameters with the same name remain independent scalars",
  (fpMass /. fpScalarForward) =!= (fpOtherMass /. fpScalarForward) &&
    SameQ[(fpMass /. fpScalarForward) /. fpScalarMap["BackwardRules"], fpMass] &&
    SameQ[(fpOtherMass /. fpScalarForward) /. fpScalarMap["BackwardRules"], fpOtherMass]];
fpSpecializationBad = ConstructiveDiagrams`Private`completionHEScalarNonzeroWitnesses[
  {fpRadicalUnit^2 - 2}, fpScalarMap["Variables"], fpScalarMap, 2];
fpSpecializationGood = ConstructiveDiagrams`Private`completionHEScalarNonzeroWitnesses[
  {fpRadicalUnit}, fpScalarMap["Variables"], fpScalarMap, 2];
fpCheck["a denominator zero after exact radical restoration is rejected",
  fpSpecializationBad["Status"] === "UNSUPPORTED" &&
    fpSpecializationBad["Reason"] === "ScalarSpecializationMayVanish" &&
    fpSpecializationGood["Status"] === "COMPLETE" &&
    TrueQ[fpSpecializationGood["EveryFactorNonzeroAfterNumericRestoration"]]];
fpTranscendentalMap = ConstructiveDiagrams`Private`completionHEExactScalarVariables[Sin[1], {}, 2];
fpTranscendentalWitness = ConstructiveDiagrams`Private`completionHEScalarNonzeroWitnesses[
  fpTranscendentalMap["Variables"], fpTranscendentalMap["Variables"], fpTranscendentalMap, 2];
fpCheck["an unproved transcendental coefficient never becomes an approximate nonzero witness",
  fpTranscendentalWitness["Status"] === "UNSUPPORTED" &&
    fpTranscendentalWitness["Reason"] === "ScalarSpecializationMayVanish"];
fpRationalRoots = {(fpX + 1)/(fpX - fpY) - 1, (fpX^2 + fpY)/(fpX - fpY)^3};
fpRationalLift = ConstructiveDiagrams`Private`completionHEClearRationalRoots[
  fpRationalRoots, {fpX, fpY}, 3];
fpRationalDenominators = Times @@ KeyValueMap[Power, #] & /@ fpRationalLift["RootDenominatorPowers"];
fpCheck["fraction-free lifting preserves root multiplicities and exact reconstruction",
  fpRationalLift["Status"] === "COMPLETE" && TrueQ[fpRationalLift["ReconstructionVerified"]] &&
    And @@ MapThread[Together[#1 - #2 #3] === 0 &,
      {fpRationalLift["Numerators"], fpRationalRoots, fpRationalDenominators}] &&
    AllTrue[fpRationalLift["LocalWitnesses"],
      TrueQ[#["LocalFactorReconstruction"]] && TrueQ[#["PositiveDenominatorPowers"]] &]];
fpUndeclaredLift = ConstructiveDiagrams`Private`completionHEClearRationalRoots[
  {fpUndeclared/(fpX + 1)}, {fpX}, 1];
fpCheck["undeclared scalar variables cannot enter a polynomial certificate",
  fpUndeclaredLift["Status"] === "UNSUPPORTED"];
fpNativeNonzero = ConstructiveDiagrams`Private`completionHENativeZeroDirection[
  fpDeformed, fpRules, fpModel, 15];
fpCheck["the single-null exact backend preserves a genuinely nonzero tensor deformation",
  fpNativeNonzero["Status"] === "NOT_PROVED" && fpNativeNonzero["OriginalDirection"] === fpDeformed &&
    (!fpNativeBackendAvailable || AnyTrue[Take[
      fpNativeNonzero["NativeCertificate"]["Roots"],
      UpTo[fpNativeNonzero["PreparationCertificate"]["CoefficientRootCount"]]],
      Lookup[#, "Status", None] === "NONZERO" &])];

Print[fpChecks, " five-point exact identity proof checks; ", Length[fpFailures], " failures."];
If[fpFailures =!= {}, Print[fpFailures]; Exit[1]];
Print["HEPCAT_TEST_COMPLETED:five-point-identity-proof-tests.wl"];
Exit[0];
