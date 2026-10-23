(* Model-backed numerical parameters remain separate from symbolic amplitudes. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
Get[FileNameJoin[{root, "tests", "QED-5-point-residues.wl"}]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name];
  Print["FAIL: ", name]]);
mpResolve[model_, rules_: Automatic] :=
  ConstructiveDiagrams`Private`residueParameterRules[model, rules];
mpZero[expression_] := Together[expression] === 0;
mpAllPass[report_] := AssociationQ[report] &&
  AllTrue[Lookup[report, "Checks", {}], #["Status"] === "PASS" &];
mpOptions[opts___] := Normal[Join[<|"Samples" -> 1, "HighEnergySamples" -> 1,
  "ReferenceSamples" -> 1, "RandomSeed" -> 1729, "TimeConstraint" -> 30,
  "Output" -> "Details"|>, Association[{opts}]]];
mpCheck[input_, channels_, model_, opts___] := CheckPhysicalResidues[
  input, channels, model, Sequence @@ mpOptions[opts]];
mpCompletionOptions[opts___] := Normal[Join[<|"Samples" -> 2,
  "ValidationSamples" -> 2, "TimeConstraint" -> 60,
  "ProgressReporting" -> False, "Output" -> "Details"|>, Association[{opts}]]];
mpFactor[input_, channels_, model_, opts___] := CompleteFactorization[
  input, channels, model, Sequence @@ mpCompletionOptions[opts]];
mpEnergy[input_, channels_, model_, opts___] := CompleteHighEnergy[
  input, channels, model, Sequence @@ mpCompletionOptions[opts]];

mpModel = {{{"Scalar", "s", "s", 1, 0, mpMass, 0, 1, 1, "", ""}},
  {{"s", "s", "s", "", mpDerivedCoupling, 1}},
  {{mpMassBase, 2, "base mass"}, {mpCoupling, 3, "base coupling"}},
  {{mpDerivedCoupling, 2 mpCoupling, "derived coupling"},
    {mpMass, mpMassShift, "out-of-order dependency"},
    {mpMassShift, mpMassBase + 1, "derived mass"}}};
mpSavedModel = mpModel;
mpDefaults = mpResolve[mpModel];
check["model-based diagnostics and completion use automatic model parameters by default",
  AllTrue[{CheckPhysicalResidues, CompleteFactorization, CompleteHighEnergy},
    ("ParameterRules" /. Options[#]) === Automatic &]];
check["model variables and out-of-order function chains resolve to exact numerical values",
  ({mpMassBase, mpMassShift, mpMass, mpCoupling, mpDerivedCoupling} /. mpDefaults) ===
    {2, 3, 3, 3, 6}];
check["an explicit empty override list retains the model defaults",
  mpResolve[mpModel, {}] === mpDefaults];
mpPartial = mpResolve[mpModel, {mpMassBase -> 4}];
check["a base-variable override recomputes derived model functions",
  ({mpMassBase, mpMassShift, mpMass, mpDerivedCoupling} /. mpPartial) === {4, 5, 5, 6}];
check["a coupling override recomputes its derived coupling",
  (mpDerivedCoupling /. mpResolve[mpModel, {mpCoupling -> 4}]) === 8];
check["a directly overridden derived parameter wins over its model definition",
  ({mpMassBase, mpMassShift, mpMass} /.
    mpResolve[mpModel, {mpMass -> 7}]) === {2, 3, 7}];
check["base and derived overrides are applied together without restoring the model formula",
  ({mpMassBase, mpMassShift, mpMass} /.
    mpResolve[mpModel, {mpMassBase -> 4, mpMass -> 9}]) === {4, 5, 9}];
check["duplicate overrides follow the first-match semantics of ordinary replacement rules",
  (mpMass /. mpResolve[mpModel, {mpMassBase -> 4, mpMassBase -> 5}]) === 5];
mpDecimalModel = ReplacePart[mpModel, {3, 1, 2} -> 0.1];
check["machine model values are rationalized before dependent expressions are evaluated",
  (mpMass /. mpResolve[mpDecimalModel]) === 1 + Rationalize[0.1, 0]];
check["machine overrides are likewise rationalized for exact kinematic charts",
  (mpMass /. mpResolve[mpModel, {mpMassBase -> 0.2}]) === 1 + Rationalize[0.2, 0]];
check["invalid override syntax is rejected",
  mpResolve[mpModel, 42] === $Failed &&
    mpResolve[mpModel, {mpMassBase}] === $Failed];

mpMissingModel = ReplacePart[mpModel, 3 -> Rest[mpModel[[3]]]];
check["missing definitions are not replaced by fabricated numerical values",
  !NumericQ[mpMass /. mpResolve[mpMissingModel]]];
mpCyclicModel = ReplacePart[mpModel, 4 -> {
  {mpDerivedCoupling, 2 mpCoupling, ""},
  {mpMass, mpMassShift + 1, ""}, {mpMassShift, mpMass - 1, ""}}];
mpCyclicRules = mpResolve[mpCyclicModel];
check["cyclic function definitions remain unresolved without repeated-replacement loops",
  !NumericQ[mpMass /. mpCyclicRules] && !NumericQ[mpMassShift /. mpCyclicRules]];
check["an unresolved dependency does not discard unrelated numerical model parameters",
  (mpDerivedCoupling /. mpCyclicRules) === 6];
mpNonfiniteModel = ReplacePart[mpModel, {3, 1, 2} -> Infinity];
check["nonfinite model values cannot become numerical kinematic assignments",
  !NumericQ[mpMass /. mpResolve[mpNonfiniteModel]]];
check["resolving model defaults and overrides never assigns global parameter values",
  AllTrue[{mpMassBase, mpMassShift, mpMass, mpCoupling, mpDerivedCoupling},
    OwnValues[#] === {} &] && mpModel === mpSavedModel];

(* The same resolved rules feed every category of the public checker. *)
mpScalarPairs = diagramAmplitudes[CreateDiagrams[ConstantArray["s", 4], mpModel]];
mpScalarAmplitude = Total[(ReduceSpinContractions[Last[#]] /. First[#]) & /@ mpScalarPairs];
mpScalarChannels = FactorizationAmplitudeChannels[ConstantArray["s", 4], mpModel];
mpSavedAmplitude = mpScalarAmplitude; mpSavedChannels = mpScalarChannels;
mpScalarReport = mpCheck[mpScalarAmplitude, mpScalarChannels, mpModel];
check["physical residues use model masses and couplings without explicit ParameterRules",
  mpScalarReport["PhysicalSummary"] === <|"PASS" -> 3|>];
check["high-energy checks use those same model-backed numerical masses and couplings",
  mpScalarReport["HighEnergyCheck"]["Status"] === "PASS"];
check["the checker records the effective numerical parameter assignments",
  ({mpMass, mpDerivedCoupling} /. mpScalarReport["ParameterRules"]) === {3, 6}];
mpScalarOverride = mpCheck[mpScalarAmplitude, mpScalarChannels, mpModel,
  "ParameterRules" -> {mpMassBase -> 4, mpCoupling -> 5}];
check["partial checker overrides preserve unsupplied defaults and recompute dependents",
  mpAllPass[mpScalarOverride] &&
    ({mpMass, mpDerivedCoupling} /. mpScalarOverride["ParameterRules"]) === {5, 10}];
mpQEDDefault = mpCheck[prFourPointAmplitude, prFourPointChannels, prModel];
check["the actual QED four-point amplitude passes with its model-file defaults",
  mpAllPass[mpQEDDefault] && mpQEDDefault["PhysicalSummary"] === <|"PASS" -> 2|>];
mpQEDEmpty = mpCheck[prFourPointAmplitude, prFourPointChannels, prModel,
  "ParameterRules" -> {}];
check["explicit empty checker overrides have the same model-backed meaning as Automatic",
  mpAllPass[mpQEDEmpty] &&
    mpQEDEmpty["ParameterRules"] === mpQEDDefault["ParameterRules"]];
mpQEDPartial = mpCheck[prFourPointAmplitude, prFourPointChannels, prModel,
  "ParameterRules" -> {EE -> 1}];
check["a QED coupling override does not require repeating the model's electron mass",
  mpAllPass[mpQEDPartial] && (EE /. mpQEDPartial["ParameterRules"]) === 1 &&
    (Me /. mpQEDPartial["ParameterRules"]) ===
      (Me /. mpQEDDefault["ParameterRules"])];
mpXi = Spinor["Xi", "Angle", 71];
mpXiBracket = (SpinorChain[mpXi, Spinor["Spin", "Angle", 2]] *
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle", 3]] -
  SpinorChain[mpXi, Spinor["Spin", "Angle", 1]] *
    SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle", 3]])/
  SpinorChain[mpXi, Spinor["Helicity", "Angle", 3]];
mpQEDReferenceForm = prS2 /. prSpinAngle[1, 2] -> mpXiBracket;
mpQEDReferenceReport = mpCheck[mpQEDReferenceForm, prChannels, prFixtureModel];
check["model defaults also support the six physical residues of a reference-bearing QED five-point form",
  mpQEDReferenceReport["PhysicalSummary"] === <|"PASS" -> 6|>];
check["spurious-pole cancellation uses the resolved model parameters",
  mpQEDReferenceReport["SpuriousSummary"] =!= <||> &&
    AllTrue[mpQEDReferenceReport["SpuriousChecks"], #["Status"] === "PASS" &]];
check["reference independence uses the resolved model parameters and coherent external spins",
  mpQEDReferenceReport["ReferenceCheck"]["Status"] === "PASS" &&
    mpQEDReferenceReport["ReferenceCheck"]["References"] === {71}];
mpMissingReport = mpCheck[mpScalarAmplitude, mpScalarChannels, mpMissingModel];
check["a required mass missing from the model remains unresolved instead of passing",
  !mpAllPass[mpMissingReport] &&
    mpMissingReport["PhysicalSummary"] === <|"UNRESOLVED" -> 3|> &&
    mpMissingReport["HighEnergyCheck"]["Status"] === "UNRESOLVED"];
mpCyclicReport = mpCheck[mpScalarAmplitude, mpScalarChannels, mpCyclicModel];
check["required cyclic model parameters produce an honest unresolved report",
  !mpAllPass[mpCyclicReport] &&
    mpCyclicReport["HighEnergyCheck"]["Status"] === "UNRESOLVED"];
mpNonfiniteReport = mpCheck[mpScalarAmplitude, mpScalarChannels, mpNonfiniteModel];
check["required nonfinite model parameters are not silently used in physical checks",
  !mpAllPass[mpNonfiniteReport] &&
    mpNonfiniteReport["HighEnergyCheck"]["Status"] === "UNRESOLVED"];
check["numerical checking preserves the symbolic amplitude, channels and original model",
  mpScalarAmplitude === mpSavedAmplitude && mpScalarChannels === mpSavedChannels &&
    mpModel === mpSavedModel && !FreeQ[mpScalarAmplitude, mpMass | mpDerivedCoupling]];

(* Completion fits numerical constraints but returns symbolic expressions. *)
mpFactorDefault = mpFactor[0, mpScalarChannels, mpModel];
check["factorization completion uses model defaults without a repeated parameter list",
  mpFactorDefault["Status"] === "COMPLETE" &&
    mpZero[mpFactorDefault["Amplitude"] - mpScalarAmplitude]];
check["default completion returns symbolic masses and couplings rather than their test values",
  !FreeQ[mpFactorDefault["Amplitude"], mpMass] &&
    !FreeQ[mpFactorDefault["Amplitude"], mpDerivedCoupling]];
mpFactorOverride = mpFactor[0, mpScalarChannels, mpModel,
  "ParameterRules" -> {mpMassBase -> 4, mpCoupling -> 5},
  "ValidationParameterRules" -> {{mpMassBase -> 6, mpCoupling -> 7}}];
check["completion resolves partial fitting and held-out validation overrides independently",
  mpFactorOverride["Status"] === "COMPLETE" &&
    mpZero[mpFactorOverride["Amplitude"] - mpScalarAmplitude] &&
    AllTrue[mpFactorOverride["ValidationReports"], mpAllPass[#["Report"]] &]];
mpExternalMassRules = Thread[(Mass /@ Range[4]) -> ConstantArray[mpMass, 4]];
mpGrowthInput = {mpExternalMassRules, 7 + mpDerivedCoupling MomProd[1, 2]};
mpEnergyDefault = mpEnergy[mpGrowthInput, {}, mpModel];
check["high-energy completion resolves the model's derived coefficients automatically",
  mpEnergyDefault["Status"] === "COMPLETE" && mpEnergyDefault["Amplitude"] === 7];
mpQEDEnergy = mpEnergy[prFourPointAmplitude, prFourPointChannels, prModel];
check["a completed QED amplitude remains unchanged under model-backed final validation",
  mpQEDEnergy["Status"] === "COMPLETE" && mpQEDEnergy["Correction"] === 0 &&
    mpZero[mpQEDEnergy["Amplitude"] - prFourPointAmplitude]];
mpMissingCompletion = mpFactor[0, mpScalarChannels, mpMissingModel];
check["completion cannot certify a solution when a required model mass is undefined",
  mpMissingCompletion["Status"] === "UNRESOLVED"];
mpUnknownCoefficientModel = ReplacePart[mpModel, 3 -> {First[mpModel[[3]]]}];
mpUnknownEnergy = mpEnergy[mpGrowthInput, {}, mpUnknownCoefficientModel];
check["an undefined derived coupling cannot become a fake zero correction",
  mpUnknownEnergy["Status"] === "UNRESOLVED" &&
    mpZero[mpUnknownEnergy["Amplitude"] - Last[mpGrowthInput]]];
check["completion leaves all supplied model definitions and symbolic inputs unchanged",
  mpModel === mpSavedModel && mpScalarAmplitude === mpSavedAmplitude &&
    mpGrowthInput === {mpExternalMassRules, 7 + mpDerivedCoupling MomProd[1, 2]} &&
    AllTrue[{mpMassBase, mpMassShift, mpMass, mpCoupling, mpDerivedCoupling},
      OwnValues[#] === {} &]];

Print[checks, " model-parameter checks; ", Length[failures], " failures."];
If[failures =!= {}, Print[failures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:model-parameter-tests.wl"];
Quit[0];
