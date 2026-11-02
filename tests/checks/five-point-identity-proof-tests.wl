(* Real five-point affine identities must remain cheap in both chiralities.
   The fixture retains formal model parameters and all spin dependence.
   No numerical parameter substitution is made in the affine directions.
   Prepared amplitudes below are regression inputs, never construction data. *)
fpRoot = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{fpRoot, "source", "HEPCAT.wl"}]];
$HistoryLength = 0; fpChecks = 0; fpFailures = {};
SetAttributes[fpCheck, HoldRest];
fpCheck[name_, assertion_] := (fpChecks++; If[!TrueQ[assertion],
  AppendTo[fpFailures, name]; Print["FAIL: ", name]]);
fpModel = ReadModel[FileNameJoin[{fpRoot, "models"}], 2];
fpFixture = Get[FileNameJoin[{fpRoot, "tests", "checks", "fixtures",
  "high-energy-five-point-four-fermion-zero.wl"}]];
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
Print[fpChecks, " five-point exact identity proof checks; ", Length[fpFailures], " failures."];
If[fpFailures =!= {}, Print[fpFailures]; Exit[1]];
Print["HEPCAT_TEST_COMPLETED:five-point-identity-proof-tests.wl"];
Exit[0];
