(* Source-derived null-pair continuation proposals, not a supplied amplitude. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0;
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name];
  Print["FAIL: ", name]]);

model = ReadModel[FileNameJoin[{root, "models"}], 3]; modelBefore = model;
process = {"e", "E", "A.+", "A.+", "A.-", "A.-"};
channels = FactorizationAmplitudeChannels[process, model];
normalized = ConstructiveDiagrams`Private`completionNormalizeInput[0, channels, model];
normalized = Join[normalized, <|"ExternalParticles" -> process|>];
parameters = ModelParameterRules[model];
alternate = Map[Function[rule, First[rule] -> Which[
  First[rule] === ModelParameter[model, "Me"], 3/7,
  First[rule] === ModelParameter[model, "EE"], 2, True, Last[rule]]], parameters];
support = ConstructiveDiagrams`Private`completionNullPairSupport[normalized, model, Automatic];
check["shifted channel continuations are enabled conservatively by default",
  ("ShiftedChannelContinuations" /. Options[CompleteFactorization]) === Automatic];
check["automatic eligibility follows external null spin weights",
  support["Status"] === "AVAILABLE" && First[support["Pairs"]] === {5, 3} &&
    Length[support["Pairs"]] <= 4];
catalog = ConstructiveDiagrams`Private`completionNullPairChannels[normalized,
  First[support["Pairs"]], parameters, 918761, 256, 20];
check["complete lower-point products generate eight source-derived continuations",
  catalog["Status"] === "AVAILABLE" && Length[catalog["Basis"]] === 8];
check["every retained source preserves its own authoritative cut at two fresh samples",
  Length[catalog["SourceChecks"]] === 8 && AllTrue[catalog["SourceChecks"],
    #["Verification"]["Status"] === "PASS" && Length[#["Verification"]["Samples"]] === 2 &]];
check["catalog construction assumes no vanishing shifted boundary or replacement targets",
  catalog["BoundaryAtInfinityAssumed"] === False && catalog["OriginalTargetsRetained"] === True];
fit = Block[{ConstructiveDiagrams`Private`$completionParallelSettings =
    <|"Parallelization" -> False, "MaxKernels" -> Automatic|>},
  ConstructiveDiagrams`Private`completionFitShiftedChannels[0, catalog["Basis"], normalized,
    2, 2, 918769, parameters, {parameters, alternate}, 2, 60, Automatic, False]];
check["all original residue equations determine a complete continuation fit",
  fit["Status"] === "COMPLETE" && fit["FreeParameters"] === {} &&
    fit["ConstraintData"]["LinearSolution"]["Rank"] === 8];
check["held-out validation covers every original channel and alternative model parameters",
  Length[fit["ValidationReports"]] >= 2 &&
    AllTrue[fit["ValidationReports"],
      ConstructiveDiagrams`Private`completionPhysicalPassQ[#["Report"], normalized["Targets"]] &] &&
    MemberQ[Lookup[fit["ValidationReports"], "ParameterRules"], alternate]];
check["the fitted source-derived amplitude preserves every external spin weight",
  ConstructiveDiagrams`Private`completionExternalSpinWeights[fit["Amplitude"],
    normalized["ExternalMassRules"]] ===
      ConstructiveDiagrams`Private`completionExternalSpinWeights[
        First[normalized["Targets"]]["StateSum"], normalized["ExternalMassRules"]]];
exchangePlan = ConstructiveDiagrams`Private`completionExchangePlan[
  normalized, model, Automatic, Automatic, process];
compactCandidate = ConstructiveDiagrams`Private`completionCompactExchangeProject[
  fit["Amplitude"], exchangePlan];
compactPrepared = PrepareAmplitudes[{normalized["ExternalMassRules"], compactCandidate}, model,
  "InternalOnShell" -> False, "PolePreference" -> None, "MaxSteps" -> 0,
  "TimeConstraint" -> 20, "Parallelization" -> False,
  "ProgressReporting" -> False, "ReturnDetails" -> True];
check["preparation admits a completed compact rational input before optional contraction expansion",
  compactPrepared["ItemReports"][[1]]["OriginalCandidateAdmitted"] === True &&
    LeafCount[compactPrepared["Amplitude"]] <= LeafCount[compactCandidate]];
compactChecked = ConstructiveDiagrams`Private`residueSimpleInputs[
  compactPrepared["Amplitude"], channels, model];
check["checker normalization retains the completed compact prepared representation",
  compactChecked["Expression"] === compactPrepared["Amplitude"]];
smallCatalog = ConstructiveDiagrams`Private`completionNullPairChannels[normalized,
  First[support["Pairs"]], parameters, 918773, 1, 5];
check["a source-count cap rejects the catalog rather than silently truncating it",
  smallCatalog["Status"] === "UNAVAILABLE" && smallCatalog["Basis"] === {}];
timedCatalog = ConstructiveDiagrams`Private`completionNullPairChannels[normalized,
  First[support["Pairs"]], parameters, 918773, 256, 0.000001];
check["a timed-out catalog cannot fabricate complete source checks",
  timedCatalog["Status"] =!= "AVAILABLE"];
vacuousFit = ConstructiveDiagrams`Private`completionFitShiftedChannels[0, {1},
  <|"Targets" -> {}|>, 1, 1, 1, {}, {{}}, 0, 1, Automatic, False];
check["an empty target catalog cannot yield a vacuous shifted-channel success",
  vacuousFit["Status"] === "UNRESOLVED"];
unvalidatedFit = ConstructiveDiagrams`Private`completionFitShiftedChannels[0, {1},
  normalized, 1, 1, 1, parameters, {}, 0, 1, Automatic, False];
check["held-out parameter validation cannot be omitted",
  unvalidatedFit["Status"] === "UNRESOLVED"];

sameHelicityChannels = FactorizationAmplitudeChannels[
  {"e", "E", "A.+", "A.+", "A.+"}, model];
sameHelicityInput = ConstructiveDiagrams`Private`completionNormalizeInput[
  0, sameHelicityChannels, model];
sameHelicitySupport = ConstructiveDiagrams`Private`completionNullPairSupport[
  sameHelicityInput, model, Automatic];
check["automatic continuation proposals leave established same-helicity routes unchanged",
  sameHelicitySupport["Status"] === "UNAVAILABLE" && sameHelicitySupport["Pairs"] === {}];
fourMassive = <|"ExternalMassRules" -> Thread[Mass /@ Range[4] -> {1, 1, 1, 1}],
  "Targets" -> {}|>;
check["no eligible null pair leaves the established completion route in control",
  ConstructiveDiagrams`Private`completionNullPairSupport[fourMassive, model, Automatic]["Status"] ===
    "UNAVAILABLE"];
twoNull = <|"ExternalMassRules" -> Thread[Mass /@ Range[4] -> {0, 0, 1, 1}],
  "Targets" -> {}|>;
check["an explicit generic attempt can exceed automatic preferences without claiming validation",
  ConstructiveDiagrams`Private`completionNullPairSupport[twoNull, model, Automatic]["Status"] ===
    "UNAVAILABLE" &&
    ConstructiveDiagrams`Private`completionNullPairSupport[twoNull, model, True]["Scope"] ===
      "ExplicitGenericAttempt"];
referencePair = ConstructiveDiagrams`Private`completionNullPairReferences[
  SpinorChain[Spinor["Xi", "Angle", 71], Spinor["Helicity", "Angle", 3]]/
    SpinorChain[Spinor["Xi", "Square", 71], Spinor["Helicity", "Square", 4]],
  {5, 6}, normalized["ExternalMassRules"]];
check["one literal Xi ID keeps the same physical reference for both chiralities",
  referencePair["Assignments"][71] === 5 &&
    !FreeQ[referencePair["Expression"], Spinor["Helicity", "Angle", 5]] &&
    !FreeQ[referencePair["Expression"], Spinor["Helicity", "Square", 5]]];

(* A small scalar theory isolates preservation of a pre-existing local term.
   The pole numerator is a supplied product of lower amplitudes, not QED data. *)
scalarModel = {{{"Null scalar", "s", "s", 1, 0, 0, 0, 1, 1, "", ""},
    {"Massive scalar", "h", "h", 1, 0, 1, 0, 1, 1, "", ""}}, {}, {}, {}};
scalarMasses = {0, 0, 0, 1};
scalarRules = Thread[Mass /@ Range[4] -> scalarMasses];
scalarPole = PropDen[Mom[Multiparticle[1, 4]], 1];
scalarChannels = {Join[scalarMasses, {Multiparticle[1, 4], 1, -1/scalarPole}]};
localResult = CompleteFactorization[{scalarRules, 7 + 2/scalarPole},
  scalarChannels, scalarModel, "InputReweighting" -> None,
  "PolePreference" -> None,
  "BoseSymmetryGroups" -> {}, "FermionSymmetryGroups" -> {},
  "Samples" -> 1, "ValidationSamples" -> 1, "TimeConstraint" -> 30,
  "MaxCorrectionTerms" -> 8, "Parallelization" -> False,
  "ProgressReporting" -> False, "ReturnDetails" -> True];
check["a non-QED scalar process can use the same general continuation stage",
  localResult["Status"] === "COMPLETE" && localResult["CorrectionSpace"] === "ShiftedChannels"];
check["the new stage preserves an existing local contribution while repairing the pole",
  Cancel[Together[localResult["Amplitude"] - 1/scalarPole]] === 7];
disabledResult = CompleteFactorization[{scalarRules, 7 + 2/scalarPole},
  scalarChannels, scalarModel, "ShiftedChannelContinuations" -> False,
  "PolePreference" -> None,
  "InputReweighting" -> None, "BoseSymmetryGroups" -> {}, "FermionSymmetryGroups" -> {},
  "Samples" -> 1, "ValidationSamples" -> 1, "TimeConstraint" -> 20,
  "MaxCorrectionTerms" -> 8, "Parallelization" -> False,
  "ProgressReporting" -> False, "ReturnDetails" -> True];
check["False disables only the new proposal route and retains the old fallback",
  AssociationQ[disabledResult] && MissingQ[disabledResult["ShiftedChannelContinuations"]] &&
    disabledResult["Status"] === "COMPLETE"];
check["unknown continuation settings are rejected",
  Quiet[CompleteFactorization[{scalarRules, 0}, scalarChannels, scalarModel,
    "ShiftedChannelContinuations" -> "Unsafe"]] === $Failed];
check["model data remain unchanged after proposal generation and completion", model === modelBefore];

If[Environment["HEPCAT_TEST_FULL_BALANCED_CONSTRUCTION"] === "1",
  diagramModel = ReadModel[FileNameJoin[{root, "models"}], 2];
  feynmanModel = ReadModel[FileNameJoin[{root, "model_feynman"}], 2];
  constructed = ConstructAmplitude[process, diagramModel, model,
    "PolePreference" -> "AllSpurious", "FeynmanModel" -> feynmanModel,
    "Parallelization" -> False, "PreparationOptions" -> {"MaxSteps" -> 0},
    "ChannelPreparationOptions" -> {"MaxSteps" -> 0},
    "FinalPreparationOptions" -> {"MaxSteps" -> 0},
    "FactorizationOptions" -> {"Samples" -> 2, "ValidationSamples" -> 1,
      "MaxCorrectionTerms" -> 256, "TimeConstraint" -> 180},
    "HighEnergyOptions" -> {"Samples" -> 1, "ValidationSamples" -> 1,
      "MaxCorrectionTerms" -> 128, "TimeConstraint" -> 180},
    "CheckOptions" -> {"Samples" -> 1, "HighEnergySamples" -> 1,
      "FeynmanSamples" -> 1, "ReferenceSamples" -> 1,
      "RandomSeed" -> 941609, "TimeConstraint" -> 180},
    "ProgressReporting" -> False, "PrintReport" -> False, "Output" -> "Details"];
  check["the real six-point diagram construction closes without a supplied correction basis",
    constructed["Status"] === "COMPLETE"];
  check["every final physical, spurious, high-energy, reference and Feynman check passes",
    AllTrue[constructed["Checks"]["Checks"], #["Status"] === "PASS" &]];
  check["the final constructed amplitude has the target helicity and massive-spin weights",
    ConstructiveDiagrams`Private`completionExternalSpinWeights[constructed["Amplitude"],
      normalized["ExternalMassRules"]] ===
        ConstructiveDiagrams`Private`completionExternalSpinWeights[
          First[normalized["Targets"]]["StateSum"], normalized["ExternalMassRules"]]],
  Print["SKIP: set HEPCAT_TEST_FULL_BALANCED_CONSTRUCTION=1 for the full six-point constructor regression."]];

Print["CHECKS: ", checks, "; FAILURES: ", Length[failures]];
If[failures =!= {}, Print[failures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:null-channel-completion-tests.wl"];
Quit[0];
