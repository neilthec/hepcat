(* Full process construction retains physical targets and validates its output. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++; If[!TrueQ[value],
  AppendTo[failures, name]; Print["FAIL: ", name]]);
caFailureQ[value_] := FailureQ[value] || value === $Failed;
caModel = ReadModel[FileNameJoin[{root, "models"}], 2];
caFactorizationModel = ReadModel[FileNameJoin[{root, "models"}], 3];
caOriginalModel = caModel; caOriginalFactorizationModel = caFactorizationModel;
caProcess4 = {"e", "E", "A.+", "A.+"};
caProcess5 = {"e", "E", "A.+", "A.+", "A.+"};
caRules = {"Me" -> 1, "EE" -> 1};
caCheckOptions = {"Samples" -> 1, "HighEnergySamples" -> 1,
  "ReferenceSamples" -> 1, "FeynmanSamples" -> 1, "TimeConstraint" -> 30};
caOptions = <|"PolePreference" -> "AllPhysical", "Parallelization" -> False,
  "TimeConstraint" -> 180, "ParameterRules" -> caRules,
  "Samples" -> 1, "ValidationSamples" -> 1, "RandomSeed" -> 892033,
  "ProgressReporting" -> False, "PrintReport" -> False,
  "PreparationOptions" -> {"MaxSteps" -> 0, "MaxReferenceChoices" -> 1,
    "TimeConstraint" -> 20},
  "ChannelPreparationOptions" -> {"MaxSteps" -> 0, "MaxReferenceChoices" -> 1,
    "TimeConstraint" -> 20},
  "FinalPreparationOptions" -> {"MaxSteps" -> 0, "TimeConstraint" -> 20},
  "FactorizationOptions" -> {"TimeConstraint" -> 60,
    "ValidationParameterRules" -> {caRules}},
  "HighEnergyOptions" -> {"TimeConstraint" -> 60,
    "ValidationParameterRules" -> {caRules}}, "CheckOptions" -> caCheckOptions|>;
caRun4[opts___Rule] := ConstructAmplitude[caProcess4, caModel, caModel,
  Sequence @@ Normal[Join[caOptions, Association[{opts}]]]];
check["the public constructor exposes simple output and parallel controls",
  DownValues[ConstructAmplitude] =!= {} &&
    ("Output" /. Options[ConstructAmplitude]) === "Expression" &&
    ("PolePreference" /. Options[ConstructAmplitude]) === "AllPhysical" &&
    ("Parallelization" /. Options[ConstructAmplitude]) === Automatic &&
    ("MaxKernels" /. Options[ConstructAmplitude]) === Automatic];
caDetails = caRun4["Output" -> "Details"];
check["actual four-point QED construction completes every stage",
  AssociationQ[caDetails] && caDetails["Status"] === "COMPLETE" &&
    caDetails["RequestedPolePreference"] === "AllPhysical" &&
    caDetails["AchievedPhysicalDenominators"] === True &&
    caDetails["PreferenceFallbackReason"] === None &&
    caDetails["Complexity"] === SpinorExpressionComplexity[caDetails["Amplitude"]]];
If[!AssociationQ[caDetails] || Lookup[caDetails, "Status", None] =!= "COMPLETE",
  Print["Cannot continue construction assertions: ", caDetails];
  If[checks =!= 82, AppendTo[failures, "Not every construction assertion executed."]];
If[failures =!= {} && ValueQ[caPolicySelected],
  caPolicyDiagnosticDirectory = CreateDirectory[];
  Put[<|"Early" -> caPolicySelected, "Late" -> caPolicyLate,
    "Unresolved" -> caPolicyUnresolved, "ExpectedValidationSets" -> caPolicyValidationSets,
    "Expression" -> caExpression, "Model" -> caModel, "FeynmanModel" -> caFeynmanModel|>,
    FileNameJoin[{caPolicyDiagnosticDirectory, "representative-policy-failure.wl"}]];
  Print["Representative policy diagnostics: ", caPolicyDiagnosticDirectory]];
Print["CHECKS: ", checks, "; FAILURES: ", Length[failures]]; Quit[1]];
check["details retain the same process and reusable external-mass pair",
  caDetails["Process"] === caProcess4 &&
    MatchQ[caDetails["AmplitudePair"], {{__Rule}, _}] &&
    Last[caDetails["AmplitudePair"]] === caDetails["Amplitude"] &&
    AllTrue[First[caDetails["AmplitudePair"]],
      MatchQ[#, HoldPattern[Mass[_Integer] -> _]] &]];
check["the final report includes physical, spurious, energy and reference checks",
  AssociationQ[caDetails["Checks"]] && caDetails["Checks"]["Summary"] === <|"PASS" -> 4|> &&
    Length[caDetails["Checks"]["Channels"]] === 2 &&
    KeyExistsQ[caDetails["Checks"], "SpuriousChecks"] &&
    caDetails["Checks"]["HighEnergyCheck"]["Status"] === "PASS" &&
    caDetails["Checks"]["ReferenceCheck"]["Status"] === "PASS"];
check["all reported stage and total times are nonnegative",
  NumericQ[caDetails["TotalTime"]] && caDetails["TotalTime"] >= 0 &&
    ListQ[caDetails["StageTimings"]] && Length[caDetails["StageTimings"]] >= 8 &&
    AllTrue[caDetails["StageTimings"], AssociationQ[#] &&
      NumericQ[#["ElapsedTime"]] && #["ElapsedTime"] >= 0 &&
      NumericQ[#["TotalElapsedTime"]] && #["TotalElapsedTime"] >= #["ElapsedTime"] &]];
caPreparationReports = Cases[caDetails["Stages"], report_Association /;
  KeyExistsQ[report, "PreparedAmplitudes"] && KeyExistsQ[report, "InputType"], Infinity];
check["diagram and factorization preparation retain originals and remainders",
  AnyTrue[caPreparationReports, #["InputType"] === "Diagrams" &&
    KeyExistsQ[#, "OriginalAmplitudes"] && KeyExistsQ[#, "RemainderData"] &] &&
  AnyTrue[caPreparationReports, #["InputType"] === "FactorizationChannels" &&
    KeyExistsQ[#, "OriginalAmplitudes"] && KeyExistsQ[#, "RemainderData"] &]];
caExternalPreparations = Select[caPreparationReports,
  Lookup[#, "InputForm", None] === "SinglePair" && #["InputType"] === "Amplitude" &];
check["completed amplitudes are prepared again with external identities only",
  Length[caExternalPreparations] >= 2 && AllTrue[caExternalPreparations,
    #["Options"]["InternalOnShell"] === False &&
      AllTrue[#["ItemReports"], #["InternalOnShellChannels"] === {} &] &]];
caFirstWins = ConstructAmplitude[caProcess4, caModel, caModel,
  "Output" -> "Details", "Output" -> "Expression", Sequence @@ Normal[caOptions],
  "PreparationOptions" -> {"MaxSteps" -> 1}];
check["constructor options retain ordinary first-rule precedence",
  AssociationQ[caFirstWins] && caFirstWins["Status"] === "COMPLETE" &&
    caFirstWins["Stages"]["Preparing diagrams"]["Options"]["MaxSteps"] === 0];
caNestedFirst = caRun4["Output" -> "Details", "PreparationOptions" -> {
  "MaxSteps" -> 0, "MaxSteps" -> 1, "MaxReferenceChoices" -> 1, "TimeConstraint" -> 20}];
check["per-stage option lists retain first-rule precedence too",
  caNestedFirst["Status"] === "COMPLETE" &&
    caNestedFirst["Stages"]["Preparing diagrams"]["Options"]["MaxSteps"] === 0];
caFamilyParameter = Unique["constructionFree"];
caMass = ModelParameter[caModel, "Me"];
caOrdinaryRedex = SpinorChain[Spinor["Spin", "Angle", 1], Mom[1], Mom[1],
    Spinor["Spin", "Angle", 2]] - caMass^2 SpinorChain[
      Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]];
(* This is a formal affine-transport probe, not an extra QED interaction. *)
caFormalFamily = <|"Amplitude" -> caDetails["Amplitude"] + caOrdinaryRedex,
  "FamilyAmplitude" -> caDetails["Amplitude"] + caOrdinaryRedex +
    caFamilyParameter caDetails["Amplitude"],
  "FreeParameters" -> {caFamilyParameter}, "RemainingBasis" -> {caDetails["Amplitude"]},
  "ExternalMassRules" -> First[caDetails["AmplitudePair"]], "Correction" -> 0|>;
caTransportedFamily = ConstructiveDiagrams`Private`constructPreparedFamily[
  caFormalFamily, caDetails["AmplitudePair"]];
check["ordinary preparation transports every affine family direction unchanged",
  (ReduceSpinorProducts[caOrdinaryRedex] /. First[caDetails["AmplitudePair"]]) === 0 &&
    caTransportedFamily["FreeParameters"] === {caFamilyParameter} &&
    caTransportedFamily["RemainingBasis"] === {caDetails["Amplitude"]} &&
    Together[caTransportedFamily["FamilyAmplitude"] - caDetails["Amplitude"] -
      caFamilyParameter caDetails["Amplitude"]] === 0];
caInitialChart = caDetails["Stages"]["Completing factorization"]["ValidationReport"]["Channels"][[1]]["Samples"][[1]]["Kinematics"];
caFinalChart = caDetails["Checks"]["Channels"][[1]]["Samples"][[1]]["Kinematics"];
check["final validation uses fresh conserved kinematics after completion validation",
  AssociationQ[caInitialChart] && AssociationQ[caFinalChart] &&
    caInitialChart["ReferenceSeed"] =!= caFinalChart["ReferenceSeed"] &&
    (caInitialChart["Momenta"] /. caInitialChart["PoleVariable"] -> 0) =!=
      (caFinalChart["Momenta"] /. caFinalChart["PoleVariable"] -> 0)];
caExpression = caRun4[];
caPair = caRun4["Output" -> "Amplitude"];
check["ordinary output is just the same validated expression",
  !AssociationQ[caExpression] && !FailureQ[caExpression] &&
    Together[caExpression - caDetails["Amplitude"]] === 0];
check["the optional amplitude output retains external masses for later preparation",
  MatchQ[caPair, {{__Rule}, _}] && First[caPair] === First[caDetails["AmplitudePair"]] &&
    Together[Last[caPair] - caDetails["Amplitude"]] === 0];
caFreshChannels = FactorizationAmplitudeChannels[caProcess4, caModel];
caFreshCheck = CheckAmplitudes[caPair, caFreshChannels, caModel,
  "ParameterRules" -> caRules, "Samples" -> 1, "HighEnergySamples" -> 1,
  "ReferenceSamples" -> 1, "Parallelization" -> False, "Output" -> "Details"];
check["returned amplitudes independently pass the original unsimplified channels",
  caFreshCheck["Summary"] === <|"PASS" -> 4|> &&
    Lookup[caFreshCheck["Channels"], "Status"] === {"PASS", "PASS"}];
caEvents = {};
caProgress = caRun4["Output" -> "Details", "ProgressReporting" -> True,
  "ProgressFunction" -> Function[event, AppendTo[caEvents, event]]];
caStarted = Select[caEvents, Lookup[#, "Event", None] === "StageStarted" &];
caFinished = Select[caEvents, Lookup[#, "Event", None] === "StageFinished" &];
check["progress announces each stage and its completion on the calling kernel",
  Length[caStarted] >= 8 && Length[caFinished] === Length[caStarted] &&
    Lookup[caStarted, "Stage"] === Lookup[caFinished, "Stage"] &&
    AllTrue[caEvents, StringQ[#["Stage"]] && NumericQ[#["StageElapsed"]] &&
      #["StageElapsed"] >= 0 && NumericQ[#["TotalElapsed"]] && #["TotalElapsed"] >= 0 &]];
check["total progress time never moves backwards",
  OrderedQ[Lookup[caEvents, "TotalElapsed"]] && caProgress["Status"] === "COMPLETE"];
caPrinted = {};
caPrintedResult = Block[{Print = (AppendTo[caPrinted, HoldComplete[##]] &)},
  caRun4["PrintReport" -> True]];
check["end-of-run output records seed, total time, complexity and tests before returning the expression",
  Length[caPrinted] >= 4 &&
    StringContainsQ[ToString[First[caPrinted], InputForm], "Random seed"] &&
    StringContainsQ[ToString[caPrinted[[-3]], InputForm], "Total"] &&
    With[{score = caDetails["Complexity"]},
      caPrinted[[-2]] === HoldComplete[Row[{"Complexity: ", score}]]] &&
    StringContainsQ[ToString[Last[caPrinted], InputForm], "PASS"] &&
    Together[caPrintedResult - caExpression] === 0];
caSpuriousDetails = caRun4["PolePreference" -> "AllSpurious", "Output" -> "Details"];
check["an explicit spurious-pole preference reaches preparation and completion",
  caSpuriousDetails["Status"] === "COMPLETE" &&
    caSpuriousDetails["RequestedPolePreference"] === "AllSpurious" &&
    AllTrue[{"Preparing diagrams", "Preparing factorization channels"},
      caSpuriousDetails["Stages"][#]["Options"]["PolePreference"] === "AllSpurious" &] &&
    caSpuriousDetails["Stages"]["Completing factorization"]["PolePreference"] === "AllSpurious" &&
    caSpuriousDetails["Checks"]["Summary"]["PASS"] >= 4];
caFeynmanModel = ReadModel[FileNameJoin[{root, "model_feynman"}], 2];
caWithFeynman = caRun4["FeynmanModel" -> caFeynmanModel, "Output" -> "Details",
  "FeynmanPreparationOptions" -> {"MaxSteps" -> 0, "TimeConstraint" -> 20}];
check["an optional independent Feynman model is generated and checked too",
  caWithFeynman["Status"] === "COMPLETE" &&
    caWithFeynman["Checks"]["Summary"] === <|"PASS" -> 5|> &&
    caWithFeynman["Checks"]["FeynmanCheck"]["Status"] === "PASS"];
(* Policy checks use real prepared snapshots, changing only the completion
   metadata to make each branch deterministic rather than racing a timer. *)
caPreparationSource = caDetails["Stages"]["Preparing diagrams"];
caPreparationVariant[changes_Association] := Join[caPreparationSource,
  <|"Status" -> "PARTIAL", "ItemReports" -> MapIndexed[
    If[First[#2] === 1, Join[#1,
      <|"Status" -> "PARTIAL", "StopReason" -> "TimeConstraint"|>, changes], #1] &,
    caPreparationSource["ItemReports"]]|>];
caBoundedPreparation = caPreparationVariant[<||>];
check["a bounded legal preparation snapshot can continue to strict validation",
  ConstructiveDiagrams`Private`constructPreparationUsableQ[caPreparationSource] &&
    ConstructiveDiagrams`Private`constructPreparationUsableQ[caBoundedPreparation]];
caMissingReference = caPreparationVariant[<|"PolePreference" -> <|
  "Status" -> "UNRESOLVED", "Diagnostics" -> {"Requested reference unavailable."}|>|>];
check["an unresolved requested reference cannot masquerade as a search timeout",
  !ConstructiveDiagrams`Private`constructPreparationUsableQ[caMissingReference]];
check["a rejected reduction cannot be accepted as a valid partial snapshot",
  !ConstructiveDiagrams`Private`constructPreparationUsableQ[
    caPreparationVariant[<|"StopReason" -> "InvalidReduction"|>]]];
caFeynmanPreparationSource = caWithFeynman["Stages"]["Preparing Feynman amplitudes"];
caFailedConversion = Join[caFeynmanPreparationSource, <|"Status" -> "PARTIAL",
  "ItemReports" -> MapIndexed[If[First[#2] === 1, Join[#1,
    <|"Status" -> "PARTIAL", "StopReason" -> "TimeConstraint",
      "FeynmanConversionFailed" -> True|>], #1] &,
    caFeynmanPreparationSource["ItemReports"]]|>];
check["a failed Feynman conversion remains a hard preparation failure",
  !ConstructiveDiagrams`Private`constructPreparationUsableQ[caFailedConversion]];
check["joint-invariant reduction failures cannot pass the preparation gate",
  !ConstructiveDiagrams`Private`constructPreparationUsableQ[
    caPreparationVariant[<|"JointInvariantFailure" -> Failure["RejectedIdentity", <||>]|>]]];
check["physical denominators must remain off shell in accepted snapshots",
  !ConstructiveDiagrams`Private`constructPreparationUsableQ[
    caPreparationVariant[<|"DenominatorsKeptOffShell" -> False|>]]];
caKeepReference = Join[caMissingReference, <|"Options" -> Join[
  caMissingReference["Options"], <|"PolePreference" -> None|>]|>];
check["leaving references explicitly unchanged remains a supported final preparation",
  ConstructiveDiagrams`Private`constructPreparationUsableQ[caKeepReference]];
caOppositeOptions = Normal[Join[Association[caCheckOptions],
  <|"FeynmanAmplitudes" -> -caExpression|>]];
caOpposite = caRun4["CheckOptions" -> caOppositeOptions];
check["an opposite-sign independent benchmark prevents ordinary amplitude output",
  FailureQ[caOpposite] && Lookup[SelectFirst[Last[caOpposite]["Diagnostics"],
    AssociationQ, <||>], "Opp Sign", 0] === 1];
caUnresolved = caRun4["CheckOptions" -> Normal[Join[Association[caCheckOptions],
  <|"TimeConstraint" -> 0.000001|>]]];
check["an unresolved final check is a failure rather than an accepted expression",
  FailureQ[caUnresolved] && Lookup[SelectFirst[Last[caUnresolved]["Diagnostics"],
    AssociationQ, <||>], "UNRESOLVED", 0] > 0];
caTimeout = caRun4["TimeConstraint" -> 0.000001];
check["the shared process deadline returns a structured failure",
  FailureQ[caTimeout] && KeyExistsQ[Last[caTimeout], "Stage"]];
Do[check["invalid constructor controls are rejected: " <> ToString[bad, InputForm],
  caFailureQ[Quiet[caRun4[bad]]]], {bad, {
  "Output" -> "Everything", "MaxKernels" -> 0, "Parallelization" -> "Sometimes",
  "TimeConstraint" -> 0, "PreparationOptions" -> {"UnknownPreparationKnob" -> 1},
  "CheckOptions" -> {"HighEnergyCheck" -> False},
  "CheckOptions" -> {"ReferenceCheck" -> False},
  "CheckOptions" -> {"SpuriousPoles" -> {}}}}];
check["unknown process particles fail before diagram generation",
  caFailureQ[Quiet[ConstructAmplitude[{"e", "E", "not-a-particle", "A.+"},
    caModel, caModel, "PrintReport" -> False, "ProgressReporting" -> False]]]];
caFive = ConstructAmplitude[caProcess5, caModel, caFactorizationModel,
  Sequence @@ Normal[Join[caOptions, <|"Output" -> "Details"|>]]];
check["five-point QED uses the supplied complete lower-point model",
  AssociationQ[caFive] && caFive["Status"] === "COMPLETE" &&
    caFive["Process"] === caProcess5 &&
    Length[caFive["Checks"]["Channels"]] === 6 &&
    caFive["Checks"]["Summary"] === <|"PASS" -> 8|>];
caZero = ConstructAmplitude[{"e", "e", "A.+", "A.+"}, caModel, caModel,
  Sequence @@ Normal[Join[caOptions, <|"Output" -> "Details"|>]]];
check["a genuinely forbidden process still validates the zero amplitude",
  AssociationQ[caZero] && caZero["Status"] === "COMPLETE" &&
    caZero["Amplitude"] === 0 && Length[First[caZero["AmplitudePair"]]] === 4 &&
    caZero["Checks"]["Channels"] === {} &&
    caZero["Checks"]["HighEnergyCheck"]["Status"] === "PASS" &&
    caZero["Checks"]["ReferenceCheck"]["Status"] === "PASS"];
(* Replay both failed notebook seeds with real diagram and factorization
   models. The independent Feynman expression is used only for the final check. *)
Do[
  caMixedProcess = Join[{"e", "E"}, First[helicities]];
  caMixed = ConstructAmplitude[caMixedProcess, caModel, caModel,
    "PolePreference" -> "AllSpurious", "FeynmanModel" -> caFeynmanModel,
    "RandomSeed" -> Last[helicities], "Output" -> "Details",
    "Parallelization" -> False, "ProgressReporting" -> False, "PrintReport" -> False];
  check["opposite photon helicities cancel the reciprocal bracket product " <> ToString[First[helicities]],
    caMixed["Status"] === "COMPLETE" && caMixed["AchievedPhysicalDenominators"] === True &&
      caMixed["Complexity"] <= 14 &&
      caMixed["Checks"]["Summary"] === <|"PASS" -> 5|> &&
      Length[caMixed["Checks"]["Channels"]] === 2 &&
      caMixed["Checks"]["SpuriousChecks"] === {} &&
      caMixed["Checks"]["FeynmanCheck"]["Status"] === "PASS"],
  {helicities, {{{"A.+", "A.-"}, 6478803515844867618},
    {{"A.-", "A.+"}, 4641830399587009873}}}];

caMixedPhysical = ConstructAmplitude[{"e", "E", "A.+", "A.-"}, caModel, caModel,
  "FeynmanModel" -> caFeynmanModel, "RandomSeed" -> 6478803515844867618,
  "Output" -> "Details", "Parallelization" -> False,
  "ProgressReporting" -> False, "PrintReport" -> False];
check["the same mixed-helicity process still validates with the default physical representation",
  caMixedPhysical["Status"] === "COMPLETE" &&
    caMixedPhysical["AchievedPhysicalDenominators"] === True &&
    caMixedPhysical["Checks"]["Summary"] === <|"PASS" -> 5|> &&
    caMixedPhysical["Checks"]["SpuriousChecks"] === {}];

(* Selecting one fully checked point is distinct from proving that its whole
   affine family is unique. Policy branches use real four-point expressions;
   only stage snapshots are controlled, while all amplitude checks remain real. *)
caPolicyParameter = Unique["representativeFreedom"];
caPolicyDirection = caOrdinaryRedex;
caPolicyCF = Join[caDetails["Stages"]["Completing factorization"], <|
  "Status" -> "COMPLETE", "Amplitude" -> caExpression,
  "ExternalMassRules" -> First[caDetails["AmplitudePair"]],
  "FamilyAmplitude" -> caExpression + caPolicyParameter caPolicyDirection,
  "FreeParameters" -> {caPolicyParameter}, "RemainingBasis" -> {caPolicyDirection},
  "CorrectionBasis" -> {caPolicyDirection}, "Correction" -> 0|>];
caPolicyUnfinishedHE = Join[caPolicyCF, <|"Status" -> "UNDERDETERMINED",
  "Diagnostics" -> {"Controlled unfinished ordinary identity proof."}|>];
caPolicyNoFreeCF = Join[caPolicyCF, <|"FamilyAmplitude" -> caExpression,
  "FreeParameters" -> {}, "RemainingBasis" -> {}|>];
caPolicyCompleteHE = Join[caPolicyNoFreeCF, <|"Status" -> "COMPLETE"|>];
caPolicyOriginalDefinitions = {DownValues[CompleteFactorization], DownValues[CompleteHighEnergy],
  Attributes[CompleteFactorization], Attributes[CompleteHighEnergy]};
caPolicyRun[cf_Association, he_Association, useBenchmark_, opts___Rule] := Module[{},
  caPolicyCFCalls = 0; caPolicyHECalls = 0;
  caPolicyCFSource = cf; caPolicyHESource = he;
  Internal`InheritedBlock[{CompleteFactorization, CompleteHighEnergy},
    Unprotect[CompleteFactorization, CompleteHighEnergy];
    DownValues[CompleteFactorization] = {
      HoldPattern[CompleteFactorization[___]] :>
        (caPolicyCFCalls++; caPolicyCFSource)};
    DownValues[CompleteHighEnergy] = {
      HoldPattern[CompleteHighEnergy[___]] :>
        (caPolicyHECalls++; caPolicyHESource)};
    caRun4[Sequence @@ Normal[Join[<|"Output" -> "Details",
      "FeynmanModel" -> If[TrueQ[useBenchmark], caFeynmanModel, None],
      "FeynmanPreparationOptions" -> {"MaxSteps" -> 0, "TimeConstraint" -> 20},
      "HighEnergyOptions" -> {"TimeConstraint" -> 60}|>, Association[{opts}]]]]
  ]
];
caPolicySelected = caPolicyRun[caPolicyCF, caPolicyUnfinishedHE, True];
caPolicySelectedHECalls = caPolicyHECalls;
caPolicySelection = caPolicySelected["RepresentativeSelection"];
check["an independent benchmark can validate a fixed factorization point before unfinished family proofs",
  caPolicySelected["Status"] === "COMPLETE" && caPolicySelectedHECalls === 0 &&
    caPolicySelection["Selected"] === True && caPolicySelection["SourceStage"] === "Factorization" &&
    AllTrue[caPolicySelected["Checks"]["Checks"], #["Status"] === "PASS" &]];
check["an already prepared unchanged factorization point is not prepared twice",
  Count[Lookup[caPolicySelected["StageTimings"], "Stage"],
    "Preparing factorization result"] === 1 &&
    Count[Lookup[caPolicySelected["StageTimings"], "Stage"],
      "Preparing factorization representative"] === 0];
check["sampled representative selection explicitly disclaims family uniqueness and benchmark fitting",
  caPolicySelection["Scope"] === "IndependentlySampledValidatedPointNotFamilyUniqueness" &&
    caPolicySelection["UniquenessEstablished"] === False &&
    caPolicySelection["BenchmarkUsedForConstruction"] === False];
check["successful point validation retains every original free coordinate and unproved direction",
  caPolicySelection["RetainedFreeParameters"] === {caPolicyParameter} &&
    caPolicySelection["RetainedDirections"] === {caPolicyDirection} &&
    Together[caPolicySelection["RetainedFamilyAmplitude"] - caExpression -
      caPolicyParameter caPolicyDirection] === 0 &&
    caPolicySelected["Stages"]["Completing factorization"]["RemainingBasis"] === {caPolicyDirection}];
caPolicyValidationSets = ConstructiveDiagrams`Private`completionValidationRuleSets[
  caModel, caRules, Automatic];
caPolicyValidationReports = caPolicySelection["ValidationReports"];
check["point selection preserves the normal independent parameter-validation assignments",
  Length[caPolicyValidationSets] === 2 &&
    Lookup[caPolicyValidationReports, "ParameterRules"] === caPolicyValidationSets &&
    AllTrue[caPolicyValidationReports, Function[entry,
      Length[entry["Report"]["Channels"]] === 2 &&
        AllTrue[entry["Report"]["Checks"], #["Status"] === "PASS" &] &&
        entry["Report"]["FeynmanCheck"]["Status"] === "PASS"]]];
check["the selected point is unchanged by choosing a representative and contains no free coefficient",
  Together[caPolicySelected["Amplitude"] - caExpression] === 0 &&
    FreeQ[caPolicySelected["Amplitude"], caPolicyParameter]];
caPolicyBaseRules = ConstructiveDiagrams`Private`residueParameterRules[caModel, Automatic];
caPolicyMassKey = ModelParameter[caModel, "Me"];
caPolicyChargeKey = ModelParameter[caModel, "EE"];
caPolicyAlternateRules = (First[#] -> If[First[#] === caPolicyMassKey, 2 Last[#], Last[#]]) & /@
  caPolicyBaseRules;
check["automatic validation does not copy constructive defaults into the independent benchmark",
  ConstructiveDiagrams`Private`constructValidationOverrides[caModel, Automatic, caPolicyBaseRules] === Automatic &&
    ConstructiveDiagrams`Private`constructValidationOverrides[caModel, Automatic, Reverse[caPolicyBaseRules]] === Automatic];
caPolicySharedOverride = ConstructiveDiagrams`Private`constructValidationOverrides[
  caModel, Automatic, caPolicyAlternateRules];
check["alternate assignments share only deliberately changed parameter values",
  caPolicySharedOverride === {"Me" -> 2 (caPolicyMassKey /. caPolicyBaseRules)} &&
    FreeQ[caPolicySharedOverride, caPolicyChargeKey | "EE"]];
caPolicyScopedOverride = {caPolicyChargeKey -> 17/13};
check["caller-supplied scoped overrides retain their original model identity",
  ConstructiveDiagrams`Private`constructValidationOverrides[caModel, caPolicyScopedOverride,
    ConstructiveDiagrams`Private`residueParameterRules[caModel, caPolicyScopedOverride]] === caPolicyScopedOverride];
caPolicyForeignMassKey = ModelParameter[caFeynmanModel, "Me"];
caPolicyForeignOverride = ConstructiveDiagrams`Private`constructValidationOverrides[
  caModel, Automatic, Append[caPolicyBaseRules, caPolicyForeignMassKey -> 9]];
check["a foreign-model validation assignment cannot silently change the constructive parameter",
  caPolicyForeignOverride === {caPolicyForeignMassKey -> 9} &&
    (caPolicyMassKey /. ConstructiveDiagrams`Private`residueParameterRules[caModel, caPolicyForeignOverride]) ===
      (caPolicyMassKey /. caPolicyBaseRules)];
check["recorded parameter overrides resolve each intended validation value in both models",
  AllTrue[caPolicyValidationReports, Function[entry,
    (caPolicyMassKey /. ConstructiveDiagrams`Private`residueParameterRules[
       caModel, entry["CheckParameterOverrides"]]) === (caPolicyMassKey /. entry["ParameterRules"]) &&
    (caPolicyForeignMassKey /. ConstructiveDiagrams`Private`residueParameterRules[
       List @@ caFeynmanModel, entry["CheckParameterOverrides"]]) === (caPolicyMassKey /. entry["ParameterRules"])]]];
caPolicyLate = caPolicyRun[caPolicyNoFreeCF, caPolicyUnfinishedHE, True];
check["an underdetermined high-energy stage can still yield an independently validated fixed point",
  caPolicyLate["Status"] === "COMPLETE" && caPolicyHECalls === 1 &&
    caPolicyLate["RepresentativeSelection"]["SourceStage"] === "HighEnergy" &&
    caPolicyLate["RepresentativeSelection"]["SourceStatus"] === "UNDERDETERMINED" &&
    caPolicyLate["RepresentativeSelection"]["RetainedDirections"] === {caPolicyDirection}];
(* A late solver point must receive the same ordinary external-shell cleanup
   as the normal completed-stage path before it can be returned early. The
   redex is an identity zero, not an added interaction or fitted benchmark. *)
caPolicyRedexHE = Join[caPolicyUnfinishedHE, <|
  "Amplitude" -> caExpression + caOrdinaryRedex,
  "FamilyAmplitude" -> caExpression + caOrdinaryRedex +
    caPolicyParameter caPolicyDirection|>];
caPolicyReprepared = caPolicyRun[caPolicyNoFreeCF, caPolicyRedexHE, True,
  "FinalPreparationOptions" -> {"MaxSteps" -> 0, "MomentumConservation" -> False,
    "TimeConstraint" -> 20}];
caPolicyRepreparedSelection = caPolicyReprepared["RepresentativeSelection"];
caPolicyRepreparedDetails = caPolicyReprepared["Stages"][
  "Preparing highenergy representative details"];
check["a late selected representative receives ordinary final preparation",
  caPolicyReprepared["Status"] === "COMPLETE" && caPolicyHECalls === 1 &&
    caPolicyRepreparedSelection["SourceStage"] === "HighEnergy" &&
    Last[First[caPolicyRepreparedDetails["OriginalAmplitudes"]]] ===
      caExpression + caOrdinaryRedex &&
    Together[caPolicyReprepared["Amplitude"] - caExpression] === 0];
check["selected-point cleanup honors explicit final-preparation controls",
  caPolicyRepreparedDetails["Options"]["MaxSteps"] === 0 &&
    caPolicyRepreparedDetails["Options"]["MomentumConservation"] === False &&
    caPolicyRepreparedDetails["Options"]["TimeConstraint"] === 20 &&
    caPolicyRepreparedDetails["Options"]["InternalOnShell"] === False &&
    AllTrue[caPolicyRepreparedDetails["ItemReports"],
      #["InternalOnShellChannels"] === {} &]];
check["selected-point final preparation precedes every independent validation",
  With[{stages = Lookup[caPolicyReprepared["StageTimings"], "Stage"]},
    First[FirstPosition[stages, "Preparing highenergy representative"]] <
      Min[Flatten[Position[stages, stage_String /;
        StringStartsQ[stage, "Validating highenergy representative"]]]]] &&
    AllTrue[caPolicyRepreparedSelection["ValidationReports"],
      AllTrue[#["Report"]["Checks"], #["Status"] === "PASS" &] &]];
check["ordinary selected-point cleanup retains the full original affine family",
  caPolicyRepreparedSelection["RetainedFreeParameters"] === {caPolicyParameter} &&
    caPolicyRepreparedSelection["RetainedDirections"] === {caPolicyDirection} &&
    Together[caPolicyRepreparedSelection["RetainedFamilyAmplitude"] - caExpression -
      caOrdinaryRedex - caPolicyParameter caPolicyDirection] === 0 &&
    FreeQ[caPolicyReprepared["Amplitude"], caPolicyParameter]];
caPolicyUnresolved = caPolicyRun[caPolicyNoFreeCF,
  Join[caPolicyUnfinishedHE, <|"Status" -> "UNRESOLVED"|>], True];
check["an unresolved proof status needs the same full fixed-point validation rather than silent promotion",
  caPolicyUnresolved["Status"] === "COMPLETE" &&
    caPolicyUnresolved["RepresentativeSelection"]["SourceStatus"] === "UNRESOLVED" &&
    caPolicyUnresolved["RepresentativeSelection"]["UniquenessEstablished"] === False &&
    AllTrue[caPolicyUnresolved["Checks"]["Checks"], #["Status"] === "PASS" &]];
caPolicyStrict = caPolicyRun[caPolicyCF, caPolicyUnfinishedHE, False];
check["without an explicit independent Feynman model unfinished high-energy completion still fails strictly",
  caPolicyStrict["Status"] === "FAILED" && caPolicyHECalls === 1 &&
    !TrueQ[caPolicyStrict["RepresentativeSelection"]["Attempted"]] &&
    FailureQ[caPolicyStrict["Failure"]]];
caPolicyNoFree = caPolicyRun[caPolicyNoFreeCF, caPolicyCompleteHE, True];
check["a factorization report without free coordinates retains the ordinary high-energy workflow",
  caPolicyNoFree["Status"] === "COMPLETE" && caPolicyHECalls === 1 &&
    !TrueQ[caPolicyNoFree["RepresentativeSelection"]["Attempted"]]];
caPolicyWrongCF = Join[caPolicyCF, <|"Amplitude" -> -caExpression,
  "FamilyAmplitude" -> -caExpression + caPolicyParameter caPolicyDirection|>];
caPolicyWrongHE = Join[caPolicyUnfinishedHE, <|"Amplitude" -> -caExpression,
  "FamilyAmplitude" -> -caExpression + caPolicyParameter caPolicyDirection|>];
caPolicyRejected = caPolicyRun[caPolicyWrongCF, caPolicyWrongHE, True];
check["a benchmark disagreement cannot turn an unfinished candidate into a successful amplitude",
  caPolicyRejected["Status"] === "FAILED" && caPolicyHECalls === 1 &&
    Length[caPolicyRejected["RepresentativeSelectionAttempts"]] === 2 &&
    AllTrue[caPolicyRejected["RepresentativeSelectionAttempts"], !TrueQ[#["Selected"]] &] &&
    caPolicyRejected["RepresentativeSelection"]["RetainedDirections"] === {caPolicyDirection}];
caPolicyRecovered = caPolicyRun[caPolicyWrongCF, caPolicyCompleteHE, True];
check["failed early point validation continues to ordinary completion instead of discarding its recovery path",
  caPolicyRecovered["Status"] === "COMPLETE" && caPolicyHECalls === 1 &&
    !TrueQ[caPolicyRecovered["RepresentativeSelection"]["Selected"]] &&
    AllTrue[caPolicyRecovered["Checks"]["Checks"], #["Status"] === "PASS" &]];
check["one independent Feynman benchmark is reused after a rejected early representative",
  Count[Lookup[caPolicyRecovered["StageTimings"], "Stage"], "Generating Feynman diagrams"] === 1 &&
    Count[Lookup[caPolicyRecovered["StageTimings"], "Stage"], "Preparing Feynman amplitudes"] === 1];
caPolicyChannels = FactorizationAmplitudeChannels[caProcess4, caModel];
caPolicyBenchmark = caWithFeynman["Stages"]["Preparing Feynman amplitudes"]["PreparedAmplitudes"];
caPolicyGate[changes_Association] := ConstructiveDiagrams`Private`constructAllChecksPassQ[
  Join[caWithFeynman["Checks"], changes], caWithFeynman["AmplitudePair"], caPolicyChannels,
  caModel, caPolicyBenchmark];
check["representative validation cannot omit an original physical channel",
  !caPolicyGate[<|"Channels" -> Rest[caWithFeynman["Checks"]["Channels"]]|>]];
check["a missing or unresolved independent Feynman comparison cannot validate a representative",
  !caPolicyGate[<|"FeynmanCheck" -> <|"Status" -> "UNRESOLVED"|>|>] &&
    !caPolicyGate[<|"FeynmanCheck" -> Missing["NotRun"]|>]];
check["high-energy, reference and automatic spurious-discovery obligations remain mandatory",
  !caPolicyGate[<|"HighEnergyCheck" -> <|"Status" -> "UNRESOLVED"|>|>] &&
    !caPolicyGate[<|"ReferenceCheck" -> <|"Status" -> "FAIL"|>|>] &&
    !caPolicyGate[<|"AutomaticPoleDiscovery" -> False|>]];
caPolicyScalarModel = caModel;
caPolicyScalarModel[[1]] = Append[caPolicyScalarModel[[1]],
  {"representative-policy scalar", "policy_scalar", "policy_scalar", 1000, 0, 0, 0, 1, 1, "h", "h"}];
caPolicyScalarRules = Thread[(Mass /@ Range[4]) -> ConstantArray[0, 4]];
caPolicyContact = <|"Amplitude" -> 7, "FamilyAmplitude" -> 7 + caPolicyParameter,
  "FreeParameters" -> {caPolicyParameter}, "RemainingBasis" -> {1},
  "ExternalMassRules" -> caPolicyScalarRules,
  "ExternalParticles" -> ConstantArray["policy_scalar", 4]|>;
caPolicyContactResult = CompleteHighEnergy[caPolicyContact, {}, caPolicyScalarModel,
  "Samples" -> 1, "ValidationSamples" -> 1, "Parallelization" -> False,
  "ProgressReporting" -> False, "TimeConstraint" -> 30, "Output" -> "Details"];
check["standalone high-energy completion still retains genuine acceptable-growth contact freedom",
  caPolicyContactResult["Status"] === "UNDERDETERMINED" &&
    caPolicyContactResult["FreeParameters"] === {caPolicyParameter} &&
    caPolicyContactResult["RemainingBasis"] === {1}];
check["controlled stage probes restore all public completion definitions and attributes",
  caPolicyOriginalDefinitions === {DownValues[CompleteFactorization], DownValues[CompleteHighEnergy],
    Attributes[CompleteFactorization], Attributes[CompleteHighEnergy]}];

(* A benchmark-backed constructor may propose one point before classifying
   unused affine freedom. Point validation remains mandatory and independent. *)
caPFStateBefore=OwnValues[ConstructiveDiagrams`Private`$completionHighEnergyPointFirst];
caPFClassificationCalls=0;caPFPruneCalls=0;
caPFCore=Internal`InheritedBlock[
 {ConstructiveDiagrams`Private`completionHEExactDerivativeClassification,
  ConstructiveDiagrams`Private`completionHEPruneIdentityFamily},
 Clear[ConstructiveDiagrams`Private`completionHEExactDerivativeClassification,
  ConstructiveDiagrams`Private`completionHEPruneIdentityFamily];
 ConstructiveDiagrams`Private`completionHEPruneIdentityFamily[___]:=
   (caPFPruneCalls++;Failure["UnexpectedPointFamilyPruning",<||>]);
 ConstructiveDiagrams`Private`completionHEExactDerivativeClassification[___]:=
   (caPFClassificationCalls++;Failure["UnexpectedPointClassification",<||>]);
 Block[{ConstructiveDiagrams`Private`$completionHighEnergyPointFirst=True},
  CompleteHighEnergy[caPolicyContact,{},caPolicyScalarModel,
   "CorrectionBasis"->{},"Samples"->1,"ValidationSamples"->1,
   "Parallelization"->False,"ProgressReporting"->False,"TimeConstraint"->30,"Output"->"Details"]]];
check["point-first retains an unresolved fitted point and every unclassified affine coordinate",
 caPFCore["Status"]==="UNRESOLVED"&&TrueQ[caPFCore["PointReady"]]&&
 caPFCore["Amplitude"]===7&&caPFCore["FreeParameters"]==={caPolicyParameter}&&
 caPFCore["RemainingBasis"]==={1}&&
 Together[caPFCore["FamilyAmplitude"]-7-caPolicyParameter]===0&&
 Length[caPFCore["ConstraintData"]]===4&&
 caPFCore["ConstraintData"]["Physical"]["Status"]==="COMPLETE"&&
 MissingQ[caPFCore["ConstraintData"]["Spurious"]]&&
 MissingQ[caPFCore["ConstraintData"]["Reference"]]&&
 caPFCore["PointReadyCheckpoint"]["CompletedEnergyCharts"]===1&&
 caPFCore["PointReadyCheckpoint"]["UnvisitedFitGroups"]==={"Spurious","Reference"}&&
 !TrueQ[caPFCore["PointReadyCheckpoint"]["AllFitGroupsComplete"]]&&
 caPFClassificationCalls===0&&caPFPruneCalls===0&&
 !TrueQ[caPFCore["PointReadyCheckpoint"]["FamilyUniquenessEstablished"]]&&
 !TrueQ[caPFCore["PointReadyCheckpoint"]["IndependentPointValidationPerformed"]]];

caPFIncomplete=Table[With[{fitStatus=status},Internal`InheritedBlock[
 {ConstructiveDiagrams`Private`completionHEEnergyConstraintSets},
 Clear[ConstructiveDiagrams`Private`completionHEEnergyConstraintSets];
 ConstructiveDiagrams`Private`completionHEEnergyConstraintSets[___]:=
  <|"Status"->fitStatus,"Equations"->{},"Samples"->{},"Diagnostics"->{"Controlled incomplete fit"}|>;
 Block[{ConstructiveDiagrams`Private`$completionHighEnergyPointFirst=True},
  CompleteHighEnergy[caPolicyContact,{},caPolicyScalarModel,
   "CorrectionBasis"->{},"Samples"->1,"ValidationSamples"->1,
   "Parallelization"->False,"ProgressReporting"->False,"TimeConstraint"->30,"Output"->"Details"]]]],
 {status,{"UNRESOLVED","INCONSISTENT"}}];
caPFIncompleteChart=Internal`InheritedBlock[
 {ConstructiveDiagrams`Private`completionHELaurentRows},
 Clear[ConstructiveDiagrams`Private`completionHELaurentRows];
 ConstructiveDiagrams`Private`completionHELaurentRows[___]:=
  Failure["UnverifiedFirstChartCoefficients",<||>];
 Block[{ConstructiveDiagrams`Private`$completionHighEnergyPointFirst=True},
  CompleteHighEnergy[caPolicyContact,{},caPolicyScalarModel,
   "CorrectionBasis"->{},"Samples"->1,"ValidationSamples"->1,
   "Parallelization"->False,"ProgressReporting"->False,"TimeConstraint"->30,"Output"->"Details"]]];
caPFIncompletePhysical=Internal`InheritedBlock[
 {ConstructiveDiagrams`Private`completionResidueConstraints},
 Clear[ConstructiveDiagrams`Private`completionResidueConstraints];
 ConstructiveDiagrams`Private`completionResidueConstraints[___]:=
  <|"Status"->"UNRESOLVED","Equations"->{},"Samples"->{},"Diagnostics"->{"Controlled physical fit deadline"}|>;
 Block[{ConstructiveDiagrams`Private`$completionHighEnergyPointFirst=True},
  CompleteHighEnergy[caPolicyContact,{},caPolicyScalarModel,
   "CorrectionBasis"->{},"Samples"->1,"ValidationSamples"->1,
   "Parallelization"->False,"ProgressReporting"->False,"TimeConstraint"->30,"Output"->"Details"]]];
check["incomplete or inconsistent fit groups cannot produce a ready-point checkpoint",
 AllTrue[Join[caPFIncomplete,{caPFIncompleteChart,caPFIncompletePhysical}],AssociationQ[#]&&#["Status"]==="UNRESOLVED"&&
  !TrueQ[Lookup[#,"PointReady",False]]&]];

caPFBadParameter=Unique["growingPointDirection"];
caPFNonemptyInput=Join[caPolicyContact,<|
 "FamilyAmplitude"->7+caPFBadParameter MomProd[1,2]+caPolicyParameter,
 "FreeParameters"->{caPFBadParameter,caPolicyParameter},
 "RemainingBasis"->{MomProd[1,2],1}|>];
caPFNonempty=Block[{ConstructiveDiagrams`Private`$completionHighEnergyPointFirst=True},
 CompleteHighEnergy[caPFNonemptyInput,{},caPolicyScalarModel,
  "CorrectionBasis"->{},"Samples"->4,"ValidationSamples"->1,
  "ParameterRules"->{"EE"->1/3},
  "ValidationParameterRules"->{{"EE"->1/2},{"EE"->2/3}},
  "Parallelization"->False,"ProgressReporting"->False,"TimeConstraint"->30,"Output"->"Details"]];
check["one nonempty energy chart fixes bad growth and retains the complete allowed affine freedom",
 caPFNonempty["Status"]==="UNRESOLVED"&&TrueQ[caPFNonempty["PointReady"]]&&
 caPFNonempty["Amplitude"]===7&&caPFNonempty["FamilyAmplitude"]===7+caPolicyParameter&&
 caPFNonempty["FreeParameters"]==={caPolicyParameter}&&caPFNonempty["RemainingBasis"]==={1}&&
 MemberQ[caPFNonempty["PointReadyCheckpoint"]["ReachedEnergySolutionRules"],caPFBadParameter->0]&&
 caPFNonempty["PointReadyCheckpoint"]["OriginalCorrectionCoordinates"]==={caPFBadParameter,caPolicyParameter}&&
 caPFNonempty["PointReadyCheckpoint"]["CompletedEnergyCharts"]===1&&
 caPFNonempty["PointReadyCheckpoint"]["UnvisitedEnergyCharts"]===3&&
 caPFNonempty["PointReadyCheckpoint"]["RequestedParameterAssignments"]===3&&
 caPFNonempty["PointReadyCheckpoint"]["UnvisitedParameterAssignments"]===2&&
 !TrueQ[caPFNonempty["PointReadyCheckpoint"]["AllRequestedParameterAssignmentsFit"]]&&
 !TrueQ[caPFNonempty["PointReadyCheckpoint"]["IndependentPointValidationPerformed"]]];

caPFReady=Join[caPolicyRedexHE,<|"Status"->"UNRESOLVED","PointReady"->True,
 "PointReadyCheckpoint"-><|"AllFitGroupsComplete"->True,
  "FamilyUniquenessEstablished"->False,"IndependentPointValidationPerformed"->False,
  "BenchmarkUsedForConstruction"->False|>|>];
caPFRun[ready_Association,strict_Association,useBenchmark_,action_String,opts___Rule]:=Module[{},
 caPFModeHistory={};caPFInputHistory={};caPFReadySource=ready;caPFStrictSource=strict;
 caPFAction=action;
 Internal`InheritedBlock[{CompleteFactorization,CompleteHighEnergy},
  Unprotect[CompleteFactorization,CompleteHighEnergy];
  DownValues[CompleteFactorization]={HoldPattern[CompleteFactorization[___]]:>caPolicyNoFreeCF};
  DownValues[CompleteHighEnergy]={HoldPattern[CompleteHighEnergy[args___]]:>
   (AppendTo[caPFModeHistory,TrueQ[ConstructiveDiagrams`Private`$completionHighEnergyPointFirst]];
    AppendTo[caPFInputHistory,First[{args}]];
    Which[caPFAction==="Abort",Abort[],caPFAction==="Wait",Pause[30];caPFReadySource,
      Last[caPFModeHistory],caPFReadySource,True,caPFStrictSource])};
  caRun4[Sequence@@Normal[Join[<|"Output"->"Details",
   "FeynmanModel"->If[TrueQ[useBenchmark],caFeynmanModel,None],
   "FeynmanPreparationOptions"->{"MaxSteps"->0,"TimeConstraint"->20},
   "HighEnergyOptions"->{"TimeConstraint"->60}|>,Association[{opts}]]]]]];
(* Run the existing PointReady case while recording the actual worker scope.
   Start preparation jobs with the opposite local setting, as a separate
   kernel would not inherit the constructor's dynamic state. *)
caSupportModes = {};
caSupportStage = None;
caSupportStateBefore = OwnValues[ConstructiveDiagrams`Private`$deferExactTensorSupportPolish];
caPFSelected = Internal`InheritedBlock[{
    ConstructiveDiagrams`Private`preparationOneAmplitude,
    ConstructiveDiagrams`Private`amplitudeParallelMap},
  DownValues[caSupportOriginalPrepare] =
    DownValues[ConstructiveDiagrams`Private`preparationOneAmplitude] /.
      HoldPattern[ConstructiveDiagrams`Private`preparationOneAmplitude] -> caSupportOriginalPrepare;
  DownValues[caSupportOriginalMap] =
    DownValues[ConstructiveDiagrams`Private`amplitudeParallelMap] /.
      HoldPattern[ConstructiveDiagrams`Private`amplitudeParallelMap] -> caSupportOriginalMap;
  Clear[ConstructiveDiagrams`Private`preparationOneAmplitude,
    ConstructiveDiagrams`Private`amplitudeParallelMap];
  ConstructiveDiagrams`Private`preparationOneAmplitude[arguments___] := (
    AppendTo[caSupportModes, {caSupportStage,
      TrueQ[ConstructiveDiagrams`Private`$deferExactTensorSupportPolish]}];
    caSupportOriginalPrepare[arguments]);
  ConstructiveDiagrams`Private`amplitudeParallelMap[worker_, jobs_List,
      settings_Association, callback_: None] :=
    If[MatchQ[jobs, {{_Integer, {_List, _}, _String, _Association} ..}],
      With[{localWorker = worker,
          initialMode = !TrueQ[ConstructiveDiagrams`Private`$deferExactTensorSupportPolish]},
        caSupportOriginalMap[Function[item,
          Block[{ConstructiveDiagrams`Private`$deferExactTensorSupportPolish = initialMode},
            localWorker[item]]], jobs, settings, callback]],
      caSupportOriginalMap[worker, jobs, settings, callback]];
  caSupportResult = caPFRun[caPFReady,caPolicyCompleteHE,True,"Return",
    "ProgressReporting" -> True,
    "ProgressFunction" -> Function[event, If[event["Event"] === "StageStarted",
      caSupportStage = event["Stage"]]],
    "FinalPreparationOptions"->{"MaxSteps"->0,"MomentumConservation"->False,"TimeConstraint"->20}];
  caSupportStage = "Standalone";
  PrepareAmplitudes[caPair, caModel, "PolePreference" -> None,
    "ProgressReporting" -> False, "Parallelization" -> False, "TimeConstraint" -> 1];
  caSupportResult
];
caSupportStageModes[stage_String] := Cases[caSupportModes, {stage, mode_} :> mode];
check["source and pre-energy preparation defer exact support on every worker",
  AllTrue[{"Preparing diagrams", "Preparing factorization channels",
      "Preparing factorization result", "Preparing Feynman amplitudes"},
    Function[stage, With[{modes = caSupportStageModes[stage]},
      modes =!= {} && AllTrue[modes, TrueQ]]]]];
check["final candidate and standalone workers enable exact support",
  caSupportStageModes["Preparing highenergy representative"] === {False} &&
  caSupportStageModes["Standalone"] === {False}];
check["private support policy preserves worker metadata and caller scope",
  AllTrue[caPFSelected["Stages"]["Preparing diagrams"]["ItemReports"],
    IntegerQ[Lookup[#, "WorkerKernel", None]] &] &&
  FreeQ[Options[PrepareAmplitudes], "DeferExactTensorSupportPolish"] &&
  OwnValues[ConstructiveDiagrams`Private`$deferExactTensorSupportPolish] === caSupportStateBefore];
caPFSelectedModes=caPFModeHistory;
check["an explicit benchmark validates the prepared fitted point with every normal independent obligation",
 caPFSelected["Status"]==="COMPLETE"&&caPFSelectedModes==={True}&&
 Together[caPFSelected["Amplitude"]-caExpression]===0&&
 caPFSelected["RepresentativeSelection"]["RetainedFreeParameters"]==={caPolicyParameter}&&
 caPFSelected["RepresentativeSelection"]["RetainedDirections"]==={caPolicyDirection}&&
 Lookup[caPFSelected["RepresentativeSelection"]["ValidationReports"],"ParameterRules"]===caPolicyValidationSets&&
 AllTrue[caPFSelected["RepresentativeSelection"]["ValidationReports"],
  AllTrue[#["Report"]["Checks"],#["Status"]==="PASS"&]&&
   #["Report"]["FeynmanCheck"]["Status"]==="PASS"&]&&
 caPFSelected["Stages"]["Preparing highenergy representative details"]["Options"]["InternalOnShell"]===False&&
 !TrueQ[caPFSelected["RepresentativeSelection"]["BenchmarkUsedForConstruction"]]];

caPFWrong=Join[caPFReady,<|"Amplitude"->-caExpression,
 "FamilyAmplitude"->-caExpression+caPolicyParameter caPolicyDirection|>];
caPFRecovered=caPFRun[caPFWrong,caPolicyCompleteHE,True,"Return"];
caPFRecoveredModes=caPFModeHistory;caPFRecoveredInputs=caPFInputHistory;
check["a rejected fitted point resumes strict completion from the complete original source",
 caPFRecovered["Status"]==="COMPLETE"&&caPFRecoveredModes==={True,False}&&
 Length[caPFRecoveredInputs]===2&&SameQ@@caPFRecoveredInputs&&
 !TrueQ[First[caPFRecovered["RepresentativeSelectionAttempts"]]["Selected"]]&&
 KeyExistsQ[caPFRecovered["Stages"],"Rejected fitted high-energy representative"]&&
 Together[caPFRecovered["Amplitude"]-caExpression]===0&&
 AllTrue[caPFRecovered["Checks"]["Checks"],#["Status"]==="PASS"&]];

caPFNoBenchmark=caPFRun[caPFReady,caPolicyUnfinishedHE,False,"Return"];
caPFNoBenchmarkModes=caPFModeHistory;
check["no-benchmark construction and standalone completion retain their strict contracts",
 caPFNoBenchmark["Status"]==="FAILED"&&caPFNoBenchmarkModes==={False}&&
 !TrueQ[caPFNoBenchmark["RepresentativeSelection"]["Selected"]]&&
 caPolicyContactResult["Status"]==="UNDERDETERMINED"&&
 !TrueQ[Lookup[caPolicyContactResult,"PointReady",False]]&&
 caPolicyContactResult["FreeParameters"]==={caPolicyParameter}];

caPFAbort=CheckAbort[caPFRun[caPFReady,caPolicyCompleteHE,True,"Abort"],"ABORTED"];
caPFAfterAbort=OwnValues[ConstructiveDiagrams`Private`$completionHighEnergyPointFirst];
caPFTimeout=TimeConstrained[caPFRun[caPFReady,caPolicyCompleteHE,True,"Wait"],8,"TIMED_OUT"];
check["the private point-first mode cannot leak after normal return abort or timeout",
 caPFAbort==="ABORTED"&&caPFTimeout==="TIMED_OUT"&&
 caPFAfterAbort===caPFStateBefore&&
 OwnValues[ConstructiveDiagrams`Private`$completionHighEnergyPointFirst]===caPFStateBefore];

(* Static source contract. The tensor hook's complete call expression,
   including its seed/rank arguments, must be in a disabled If branch for
   unsupported external multiplicity. A guard inside tensorTrial is too late.
   Inspect held definitions only; no amplitude or ranking function is called. *)
caTensorCallsGuardedQ[] := Module[
 {held, calls, assignments, guarded, gateConditionQ, ancestors, node, conditions},
 held=With[{definition=DownValues[ConstructiveDiagrams`Private`preparationOneAmplitude]},
   HoldComplete[definition]];
 calls=Position[held,
   HoldPattern[ConstructiveDiagrams`Private`tensorTrial[arguments___]] /;
     FreeQ[HoldComplete[arguments],_Pattern],{0,Infinity},Heads->False];
 assignments=Cases[held,
   HoldPattern[Set[ConstructiveDiagrams`Private`tensorEligible,rhs_]] :>
     HoldComplete[rhs],{0,Infinity}];
 gateConditionQ[value_]:=MatchQ[value,
   HoldComplete[TrueQ[ConstructiveDiagrams`Private`tensorEligible]] |
   HoldComplete[And[___,TrueQ[ConstructiveDiagrams`Private`tensorEligible],___]]];
 guarded[path_List]:=AnyTrue[Range[0,Length[path]-1],Function[length,
   ancestors=Take[path,length];
   If[path[[length+1]]=!=2,False,
    node=Extract[held,ancestors,HoldComplete];
    conditions=Cases[node,HoldComplete[If[condition_,_,___]] :>
      HoldComplete[condition],{0}];
    Length[conditions]===1&&gateConditionQ[First[conditions]]]]];
 Length[calls]===2&&AllTrue[calls,guarded]&&Length[assignments]===1&&
   !FreeQ[assignments,HoldPattern[SameQ[Length[_Symbol],4]]]&&
   !FreeQ[assignments,HoldPattern[ConstructiveDiagrams`Private`preparationExternalRules[_]]]&&
   FreeQ[assignments,_ConstructiveDiagrams`Private`snapshotRank |
     _ConstructiveDiagrams`Private`preparationRank |
     _ConstructiveDiagrams`Private`expressionComplexity |
     _ConstructiveDiagrams`Private`preparationNumerators |
     _ConstructiveDiagrams`Private`rspFourLegTensorReduction]
];

check["tensor eligibility guards complete eager seed and rank arguments",
 caTensorCallsGuardedQ[]];

check["the constructor never mutates either supplied model",
  caModel === caOriginalModel && caFactorizationModel === caOriginalFactorizationModel];
If[checks =!= 82, AppendTo[failures, "Not every construction assertion executed."]];
If[failures =!= {} && ValueQ[caPolicySelected],
  caPolicyDiagnosticDirectory = CreateDirectory[];
  Put[<|"Early" -> caPolicySelected, "Late" -> caPolicyLate,
    "Unresolved" -> caPolicyUnresolved, "ExpectedValidationSets" -> caPolicyValidationSets,
    "Expression" -> caExpression, "Model" -> caModel, "FeynmanModel" -> caFeynmanModel|>,
    FileNameJoin[{caPolicyDiagnosticDirectory, "representative-policy-failure.wl"}]];
  Print["Representative policy diagnostics: ", caPolicyDiagnosticDirectory]];
Print["CHECKS: ", checks, "; FAILURES: ", Length[failures]];
If[failures =!= {}, Print[failures]];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:construct-amplitude-tests.wl"]];
Quit[If[failures === {}, 0, 1]];
