(* Fixed-mass, fixed-angle high-energy checks of complete amplitudes. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
SetOptions[CheckPhysicalResidues, "ReferenceCheck" -> False];
If[DownValues[ConstructiveDiagrams`Private`highEnergyCheckReport] === {},
  Print["High-energy diagnostics did not load."]; Quit[1]];
Get[FileNameJoin[{root, "tests", "QED-5-point-residues.wl"}]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name];
  Print["FAIL: ", name]]);

heScalarModel = {{{"Scalar", "s", "s", 1, 0, 1, 0, 1, 1, "", ""}},
  {}, {}, {}};
heMassRules[n_Integer] := Thread[(Mass /@ Range[n]) -> ConstantArray[1, n]];
heMetadata[n_Integer] := <|"ExternalLegs" -> Range[n],
  "ExternalMassRules" -> heMassRules[n]|>;
heReport[expression_, n_Integer, opts___] := Module[{settings},
  settings = Association[Join[{
    "HighEnergySamples" -> 2, "RandomSeed" -> 1729,
    "ParameterRules" -> {}, "HighEnergyTimeConstraint" -> 20,
    "HighEnergyBound" -> Automatic}, {opts}]];
  ConstructiveDiagrams`Private`highEnergyCheckReport[expression, heMetadata[n],
    settings["HighEnergySamples"], settings["RandomSeed"], settings["ParameterRules"],
    settings["HighEnergyTimeConstraint"], settings["HighEnergyBound"]]
];

check["high-energy checking is enabled by default",
  ("HighEnergyCheck" /. Options[CheckPhysicalResidues]) === Automatic];
check["the default bound is inferred from multiplicity",
  ("HighEnergyBound" /. Options[CheckPhysicalResidues]) === Automatic];
heFourContact = heReport[7, 4];
check["a momentum-independent four-scalar contact obeys the constant bound",
  AssociationQ[heFourContact] && heFourContact["Status"] === "PASS"];
check["the four-point automatic power bound is zero",
  heFourContact["ExpectedPower"] === 0];
heDerivativeContact = heReport[MomProd[1, 2], 4];
check["an extra scalar momentum product violates the four-point bound",
  AssociationQ[heDerivativeContact] && heDerivativeContact["Status"] === "FAIL"];
heMasslessMetadata[n_Integer] := <|"ExternalLegs" -> Range[n],
  "ExternalMassRules" -> Thread[(Mass /@ Range[n]) -> ConstantArray[0, n]]|>;
heMasslessDerivative = ConstructiveDiagrams`Private`highEnergyCheckReport[
  MomProd[1, 2], heMasslessMetadata[4], 2, 1729, {}, 20, Automatic];
check["all-massless external states still expose forbidden momentum-product growth",
  heMasslessDerivative["Status"] === "FAIL" && heMasslessDerivative["ObservedPower"] === 2];
heMasslessExchange = ConstructiveDiagrams`Private`highEnergyCheckReport[
  1/PropDen[Mom[Multiparticle[1, 2, 3]], 0], heMasslessMetadata[6],
  2, 1729, {}, 20, Automatic];
check["all-massless exchange amplitudes support rational energy dependence without spin variables",
  heMasslessExchange["Status"] === "PASS" && heMasslessExchange["ExpectedPower"] === -2];
heFiveContact = heReport[7, 5];
check["a constant five-point contribution violates the inverse-energy bound",
  AssociationQ[heFiveContact] && heFiveContact["Status"] === "FAIL"];
check["the five-point automatic power bound is minus one",
  heFiveContact["ExpectedPower"] === -1];
heSixExchange = heReport[1/PropDen[Mom[Multiparticle[1, 2, 3]], 1], 6];
check["two quartic scalar vertices joined by a propagator have allowed six-point falloff",
  AssociationQ[heSixExchange] && heSixExchange["Status"] === "PASS"];
check["the six-point automatic power bound is minus two",
  heSixExchange["ExpectedPower"] === -2];
heOverride = heReport[MomProd[1, 2], 4, "HighEnergyBound" -> 2];
check["an explicit bound supports a different theory hypothesis",
  AssociationQ[heOverride] && heOverride["Status"] === "PASS"];
heLogarithm = heReport[Log[MomProd[1, 2]], 4];
check["logarithmic growth is unresolved rather than mistaken for an allowed constant",
  AssociationQ[heLogarithm] && heLogarithm["Status"] === "UNRESOLVED"];
heFractionalPower = heReport[MomProd[1, 2]^(1/3), 4];
check["unsupported fractional Laurent powers are unresolved rather than passed",
  AssociationQ[heFractionalPower] && heFractionalPower["Status"] === "UNRESOLVED"];
heTwelve = 2 MomProd[1, 2] + 2 -
  PropDen[Mom[Multiparticle[1, 2]], 0];
heCancellation = heReport[heTwelve + 5, 4];
check["the complete sum is reduced before judging energy growth",
  AssociationQ[heCancellation] && heCancellation["Status"] === "PASS"];
heTinyBadGrowth = heReport[7 + MomProd[1, 2]/10^100, 4];
check["an arbitrarily small exact bad-growth coefficient is not hidden by a fit tolerance",
  AssociationQ[heTinyBadGrowth] && heTinyBadGrowth["Status"] === "FAIL"];
heUnknownParameter = heReport[heUnspecifiedCoefficient MomProd[1, 2], 4];
check["an unspecified parameter gives unresolved rather than an invented zero",
  AssociationQ[heUnknownParameter] && heUnknownParameter["Status"] === "UNRESOLVED"];
check["unknown coefficients cannot imply a falsely bounded or decaying amplitude",
  MissingQ[Lookup[heUnknownParameter, "ObservedPowerUpperBound", Missing["NotDetermined"]]] &&
    heUnknownParameter["ObservedGrowth"] === "Not determined"];
hePartlyUnknown = heReport[1 + heUnspecifiedCoefficient MomProd[1, 2], 4];
check["a known constant does not hide an unresolved faster-growing coefficient",
  hePartlyUnknown["Status"] === "UNRESOLVED" &&
    hePartlyUnknown["ObservedGrowth"] === "Not determined" &&
    MissingQ[hePartlyUnknown["ObservedPower"]] &&
    MissingQ[hePartlyUnknown["ObservedPowerUpperBound"]]];
heKnownParameter = heReport[heSpecifiedCoefficient MomProd[1, 2], 4,
  "ParameterRules" -> {heSpecifiedCoefficient -> 3}];
check["supplied parameters are applied without scaling masses or couplings",
  AssociationQ[heKnownParameter] && heKnownParameter["Status"] === "FAIL"];
heTimed = heReport[MomProd[1, 2], 4,
  "HighEnergySamples" -> 100, "HighEnergyTimeConstraint" -> 0.000001];
check["a high-energy timeout is unresolved", AssociationQ[heTimed] &&
  heTimed["Status"] === "UNRESOLVED"];

Do[
  check["invalid high-energy option is rejected " <> ToString[bad, InputForm],
    Quiet[CheckPhysicalResidues[{heMassRules[4], 1}, {}, heScalarModel,
      bad]] === $Failed],
  {bad, {"HighEnergyCheck" -> "Sometimes", "HighEnergyBound" -> "Fast",
    "HighEnergySamples" -> 0, "HighEnergySamples" -> 3/2,
    "HighEnergyTimeConstraint" -> 0}}
];
heDisabled = CheckPhysicalResidues[{heMassRules[4], 7}, {}, heScalarModel,
  "HighEnergyCheck" -> False, "Output" -> "Details"];
check["opting out preserves the previous empty-pole summary",
  AssociationQ[heDisabled] && heDisabled["Summary"] === <||>];
check["opting out avoids inventing a high-energy row",
  !KeyExistsQ[heDisabled, "HighEnergyCheck"]];
heDefault = CheckPhysicalResidues[{heMassRules[4], 7}, {}, heScalarModel,
  "Output" -> "Details", "HighEnergySamples" -> 2];
check["the default checker includes high-energy results with the pole diagnostics",
  AssociationQ[heDefault] && heDefault["Summary"] === <|"PASS" -> 1|> &&
    heDefault["HighEnergySummary"] === <|"PASS" -> 1|> &&
    heDefault["HighEnergyCheck"]["Status"] === "PASS"];
check["the default high-energy row names both observed and allowed growth",
  AllTrue[Normal[heDefault["Table"]],
    ContainsAll[Keys[#], {"Type", "Test", "Status", "ObservedGrowth", "ExpectedGrowth"}] &]];
hePublicDisplay = CheckPhysicalResidues[{heMassRules[4], 7}, {}, heScalarModel,
  "Output" -> "HighEnergy", "HighEnergySamples" -> 2];
check["high-energy output exposes one visible row per kinematic sample",
  Head[hePublicDisplay] === Dataset && Length[Normal[hePublicDisplay]] === 2 &&
    AllTrue[Normal[hePublicDisplay], ContainsAll[Keys[#],
      {"Sample", "Status", "ObservedGrowth", "ExpectedGrowth", "Diagnostics"}] &]];
heExplicitLegacy = CheckPhysicalResidues[{heMassRules[4], 7}, {}, heScalarModel,
  "SpuriousPoles" -> {}, "HighEnergyCheck" -> False];
check["disabling the check preserves the physical-only table API",
  Head[heExplicitLegacy] === Dataset && Normal[heExplicitLegacy] === {}];
heDisabledDisplay = CheckPhysicalResidues[{heMassRules[4], 7}, {}, heScalarModel,
  "HighEnergyCheck" -> False, "Output" -> "HighEnergy"];
check["asking for a disabled high-energy display returns no rows",
  Head[heDisabledDisplay] === Dataset && Normal[heDisabledDisplay] === {}];

(* Real, normalized external helicity states must reproduce known QED falloff;
   arbitrary massive little-group projections do not establish this bound. *)
heQEDFour = ConstructiveDiagrams`Private`highEnergyCheckReport[
  prFourPointAmplitude, KeyTake[First[prFourPointTargets],
    {"ExternalLegs", "ExternalMassRules"}], 2, 1729,
  {Me -> 1, EE -> 1}, 30, Automatic];
check["the known four-point QED amplitude obeys its physical-spin bound",
  AssociationQ[heQEDFour] && heQEDFour["Status"] === "PASS"];
heQEDH = ConstructiveDiagrams`Private`highEnergyCheckReport[
  prH, KeyTake[First[prTargets], {"ExternalLegs", "ExternalMassRules"}],
  2, 1729, {Me -> 1, EE -> 1}, 30, Automatic];
check["the reference-free H amplitude has allowed five-point falloff",
  AssociationQ[heQEDH] && heQEDH["Status"] === "PASS"];
heQEDS2 = ConstructiveDiagrams`Private`highEnergyCheckReport[
  prS2, KeyTake[First[prTargets], {"ExternalLegs", "ExternalMassRules"}],
  2, 1729, {Me -> 1, EE -> 1}, 45, Automatic];
check["the equivalent spurious-pole S2 form has the same allowed falloff",
  AssociationQ[heQEDS2] && heQEDS2["Status"] === "PASS"];

heVectorContact = prSpinAngle[1, 2]^2 prSpinAngle[3, 4]^2;
heVectorBad = heReport[heVectorContact, 4];
check["massive-vector growth is tested in all spin components including longitudinal",
  AssociationQ[heVectorBad] && heVectorBad["Status"] === "FAIL"];
heVectorCancellation = heReport[heTwelve heVectorContact, 4];
check["a massive-vector momentum identity cancels before polarization growth is judged",
  AssociationQ[heVectorCancellation] && heVectorCancellation["Status"] === "PASS"];
heExplicitSpin = heReport[SpinorChain[
  Spinor["Spin", "Upper", "Angle", 1, -1/2],
  Spinor["Spin", "Upper", "Angle", 2, -1/2]], 4];
check["explicit supplied spin components do not claim every polarization was tested",
  AssociationQ[heExplicitSpin] && StringContainsQ[heExplicitSpin["Scope"], "Implicit"]];

heThreePoint = heReport[7, 3];
check["three-point real fixed-angle kinematics are not silently fabricated",
  AssociationQ[heThreePoint] && heThreePoint["Status"] === "UNRESOLVED"];
heQEDMetadata = KeyTake[First[prTargets], {"ExternalLegs", "ExternalMassRules"}];
heChart = ConstructiveDiagrams`Private`highEnergyAutomaticChart[
  prH, heQEDMetadata, 1, 1729, {Me -> 1, EE -> 1}];
check["high-energy kinematics retain the exact fixed physical masses",
  AssociationQ[heChart] && heChart["Masses"] ===
    <|1 -> 1, 2 -> 1, 3 -> 0, 4 -> 0, 5 -> 0|>];
check["chart construction verifies shells, conservation, and noncollinear leading invariants",
  TrueQ[heChart["KinematicChecks"]["MomentumConservation"]] &&
    And @@ heChart["KinematicChecks"]["ExternalMassShells"] &&
    TrueQ[heChart["KinematicChecks"]["LeadingPairInvariantsNonzero"]]];
heChartVariable = heChart["PoleVariable"];
heMomentaAtSample = Values[heChart["Momenta"]] /. heChartVariable -> 1/2;
heExactZero[expression_] := AllTrue[Flatten[{expression}],
  RootReduce[Together[#]] === 0 &];
check["all-ingoing conventions cross precisely the outgoing three legs",
  And @@ Thread[Re[N[Tr /@ Take[heMomentaAtSample, 2], 40]] > 0] &&
    And @@ Thread[Re[N[Tr /@ Drop[heMomentaAtSample, 2], 40]] < 0]];
check["momentum is conserved independently away from the expansion point",
  heExactZero[Total[heMomentaAtSample]]];
check["external shells remain fixed independently away from the expansion point",
  heExactZero[(Det /@ heMomentaAtSample) - {1, 1, 0, 0, 0}]];
check["outgoing photon angle and square spinors reconstruct negative momentum matrices",
  AllTrue[{3, 4, 5}, Function[index,
    heExactZero[(Outer[Times, heChart["Angle"][index],
      heChart["Square"][index]] - heChart["Momenta"][index]) /.
        heChartVariable -> 1/2]]]];
check["coherent massive spin variables retain every physical-spin coefficient",
  Length[heChart["SpinVariables"]] === 2 &&
    Keys[heChart["ExternalSpinVectors"]] === {1, 2}];
heSecondChart = ConstructiveDiagrams`Private`highEnergyAutomaticChart[
  prH, heQEDMetadata, 1, 1729, {Me -> 1, EE -> 1}];
check["fixed seeds reproduce the same generic fixed-angle geometry",
  heChart["OutgoingSpatialDirections"] === heSecondChart["OutgoingSpatialDirections"]];
heReferenceRatio = SpinorChain[Spinor["Xi", "Angle", 71],
  Spinor["Helicity", "Angle", 3]]/
    SpinorChain[Spinor["Xi", "Angle", 71], Spinor["Helicity", "Angle", 4]];
heReference = ConstructiveDiagrams`Private`highEnergyCheckReport[
  heReferenceRatio, heQEDMetadata, 2, 1729, {Me -> 1}, 20, 0];
check["fixed reference spinors remain usable along the high-energy family",
  AssociationQ[heReference] && heReference["Status"] === "PASS"];
heHeavyMetadata = <|"ExternalLegs" -> Range[4],
  "ExternalMassRules" -> Thread[(Mass /@ Range[4]) -> {10, 10, 1, 1}]|>;
heHeavy = ConstructiveDiagrams`Private`highEnergyCheckReport[
  MomProd[1, 2], heHeavyMetadata, 2, 1729, {}, 20, Automatic];
check["heavy incoming particles are allowed when the asymptotic physical family exists",
  AssociationQ[heHeavy] && heHeavy["Status"] === "FAIL"];
heHeavyChart = ConstructiveDiagrams`Private`highEnergyAutomaticChart[
  MomProd[1, 2], heHeavyMetadata, 1, 1729, {}];
heHeavyMomenta = Values[heHeavyChart["Momenta"]] /.
  heHeavyChart["PoleVariable"] -> 1/10;
check["heavy-particle high-energy samples are real and on shell above threshold",
  And @@ (Element[#, Reals] & /@ (Tr /@ heHeavyMomenta)) &&
    AllTrue[heHeavyMomenta, heExactZero[# - ConjugateTranspose[#]] &] &&
    heExactZero[(Det /@ heHeavyMomenta) - {100, 100, 1, 1}] &&
    heExactZero[Total[heHeavyMomenta]]];
heHeavyFixedMass = ConstructiveDiagrams`Private`highEnergyCheckReport[
  MomProd[1, 1], heHeavyMetadata, 1, 1729, {}, 20, Automatic];
check["a heavy external self-contraction stays constant rather than scaling with energy",
  AssociationQ[heHeavyFixedMass] && heHeavyFixedMass["Status"] === "PASS" &&
    heHeavyFixedMass["ObservedPower"] === 0];

(* An actual SM cancellation, rather than an invented vector polynomial.
   The known h h Z Z contact is fixed model input and must not be omitted. *)
Get[FileNameJoin[{root, "tests", "simplification-paths.wl"}]];
heSMRows = Select[simplificationReferences,
  StringStartsQ[#["Name"], "SM-4-point-test.nb / h h -> Z Z / "] &];
heSMMetadata = <|"ExternalLegs" -> Range[4],
  "ExternalMassRules" -> First[heSMRows]["Masses"]|>;
heSMParameters = {Mh -> 3, MZ -> 1, MW -> 4/5, SW -> 3/5, CW -> 4/5, EE -> 1};
heSMFull = ConstructiveDiagrams`Private`highEnergyCheckReport[
  Total[Lookup[heSMRows, "Amplitude"]], heSMMetadata,
  2, 1729, heSMParameters, 30, Automatic];
check["the complete known SM h h Z Z amplitude cancels all growing vector components",
  Length[heSMRows] === 4 && heSMFull["Status"] === "PASS" &&
    heSMFull["ObservedPower"] === 0];
heSMWithoutContact = ConstructiveDiagrams`Private`highEnergyCheckReport[
  Total[Lookup[Select[heSMRows, !StringEndsQ[#["Name"], "contact"] &], "Amplitude"]],
  heSMMetadata, 2, 1729, heSMParameters, 30, Automatic];
check["omitting the known SM contact exposes longitudinal E squared growth",
  heSMWithoutContact["Status"] === "FAIL" &&
    heSMWithoutContact["ObservedPower"] === 2 &&
    AllTrue[heSMWithoutContact["Samples"], AnyTrue[#["Components"],
      Function[component, component["Power"] === 2 &&
        Take[Last /@ component["SpinMonomial"], -2] === {1, 1}]] &]];

Print[checks, " high-energy checks; ", Length[failures], " failures."];
If[failures =!= {}, Print[failures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:high-energy-tests.wl"];
Quit[0];
