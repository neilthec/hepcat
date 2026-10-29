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
  AssociationQ[caDetails] && caDetails["Status"] === "COMPLETE"];
If[!AssociationQ[caDetails] || Lookup[caDetails, "Status", None] =!= "COMPLETE",
  Print["Cannot continue construction assertions: ", caDetails];
  If[checks =!= 40, AppendTo[failures, "Not every construction assertion executed."]];
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
check["end-of-run output shows total time followed by tests and returns the expression",
  Length[caPrinted] >= 2 &&
    StringContainsQ[ToString[First[caPrinted], InputForm], "Total"] &&
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
check["the constructor never mutates either supplied model",
  caModel === caOriginalModel && caFactorizationModel === caOriginalFactorizationModel];
If[checks =!= 40, AppendTo[failures, "Not every construction assertion executed."]];
Print["CHECKS: ", checks, "; FAILURES: ", Length[failures]];
If[failures =!= {}, Print[failures]];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:construct-amplitude-tests.wl"]];
Quit[If[failures === {}, 0, 1]];
