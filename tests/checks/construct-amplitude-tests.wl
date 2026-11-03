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
    ("Parallelization" /. Options[ConstructAmplitude]) === Automatic &&
    ("MaxKernels" /. Options[ConstructAmplitude]) === Automatic];
caDetails = caRun4["Output" -> "Details"];
check["actual four-point QED construction completes every stage",
  AssociationQ[caDetails] && caDetails["Status"] === "COMPLETE" &&
    caDetails["RequestedPolePreference"] === "AllPhysical" &&
    caDetails["AchievedPhysicalDenominators"] === True &&
    caDetails["PreferenceFallbackReason"] === None];
If[!AssociationQ[caDetails] || Lookup[caDetails, "Status", None] =!= "COMPLETE",
  Print["Cannot continue construction assertions: ", caDetails];
  If[checks =!= 62, AppendTo[failures, "Not every construction assertion executed."]];
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
check["end-of-run output records the seed then total time and tests before returning the expression",
  Length[caPrinted] >= 3 &&
    StringContainsQ[ToString[First[caPrinted], InputForm], "Random seed"] &&
    StringContainsQ[ToString[caPrinted[[-2]], InputForm], "Total"] &&
    StringContainsQ[ToString[Last[caPrinted], InputForm], "PASS"] &&
    Together[caPrintedResult - caExpression] === 0];
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

check["the constructor never mutates either supplied model",
  caModel === caOriginalModel && caFactorizationModel === caOriginalFactorizationModel];
If[checks =!= 62, AppendTo[failures, "Not every construction assertion executed."]];
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
