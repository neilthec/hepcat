(* Opt-in: generate independent CalcHEP processes and compare shared random points. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0;
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);
ciDirectory = CreateDirectory[];
ciPath[name_] := FileNameJoin[{ciDirectory, name}];
ciRun[command_List] := Quiet[Check[RunProcess[command], <|"ExitCode" -> -1|>]];
ciSucceeded[report_] := AssociationQ[report] && Lookup[report, "ExitCode", -1] === 0;
ciSpinas = Replace[Environment["HEPCAT_SPINAS_DIRECTORY"],
  $Failed | "" -> FileNameJoin[{ParentDirectory[root], "spinas"}]];
ciCalcHEP = Replace[Environment["HEPCAT_CALCHEP_DIRECTORY"], $Failed | "" -> Automatic];
ciModel = ReadModel[FileNameJoin[{root, "models"}], 3];
ciModelBefore = ciModel;
ciRules = {"Me" -> 1, "EE" -> 1/3};
ciCounterpart = {FileNameJoin[{root, "model_feynman"}], 2};
ciOriginalFiles = FileNameJoin[{First[ciCounterpart], # <> "2.mdl"}] & /@
  {"prtcls", "vars", "func", "lgrng"};
ciOriginalBytes = BinaryReadList[#, "Byte"] & /@ ciOriginalFiles;
ciLookup[process_List] := LookupAmplitude[Thread[{process, Range[Length[process]]}],
  ciModel, False, {ciI, ciJ, ciK, ciL}];
ciRecords[processes_List] := {#, ciLookup[#]} & /@ processes;
ciFourProcesses = Join[{"e", "E"}, #] & /@
  {{"A.+", "A.+"}, {"A.+", "A.-"}, {"A.-", "A.-"}};
ciFour = ciRecords[ciFourProcesses];
ciExport[name_String, entries_List, opts___] := WriteSPINASProcess[ciPath[name], entries, ciModel,
  opts, "ProcessName" -> name, "SPINASDirectory" -> ciSpinas, "ParameterRules" -> ciRules,
  "CalcHEPDirectory" -> ciCalcHEP, "CalcHEPModel" -> ciCounterpart,
  "CalcHEPPoints" -> 1000, "CalcHEPSeed" -> 12345, "ReturnDetails" -> True];
ciExecutable[report_Association, name_String] := FileNameJoin[
  {report["Directory"], "build", name <> "_validate_calchep" <> If[$OperatingSystem === "Windows", ".exe", ""]}];
ciCompare[report_Association, name_String, points_Integer:1000] := ciRun[
  {ciExecutable[report, name], "--points", ToString[points], "--seed", "12345"}];
ciPassed[report_] := ciSucceeded[report] &&
  StringContainsQ[Lookup[report, "StandardOutput", ""], "Amplitude validation: PASS"] &&
  StringContainsQ[Lookup[report, "StandardOutput", ""], "Points requested/generated/compared: 1000/1000/1000"] &&
  StringContainsQ[Lookup[report, "StandardOutput", ""], "Mismatches/errors: 0/0"];

Print["Generating the massive-electron two-photon CalcHEP comparison."];
ciPositive = ciExport["calchep_positive", ciFour, "RunCalcHEPValidation" -> True];
check["the public export option builds and runs the complete 1000-point CalcHEP validation",
  AssociationQ[ciPositive] && DirectoryQ[ciPositive["Directory"]]];
If[!AssociationQ[ciPositive], Print["CALCHEP POSITIVE EXPORT: ", InputForm[ciPositive]]; Quit[1]];
ciManifest = Import[ciPositive["Files"]["Manifest"], "RawJSON"];
ciConfig = Lookup[ciManifest, "CalcHEPValidation", <||>];
check["the selected CalcHEP model is available and physical photon PDGs stay self-conjugate",
  Lookup[ciConfig, "Status", None] === "AVAILABLE" &&
    Lookup[ciConfig, "PhysicalPDGs", {}] === {11, -11, 22, 22}];
check["the generated standalone validator was built", FileExistsQ[ciExecutable[ciPositive, "calchep_positive"]]];
ciPositiveRun = ciCompare[ciPositive, "calchep_positive"];
check["1000 shared random points agree without a fitted normalization", ciPassed[ciPositiveRun]];
If[!ciPassed[ciPositiveRun], Print["CALCHEP POSITIVE COMPARISON: ", ciPositiveRun]];
check["the validation report records the requested reproducible seed",
  StringContainsQ[Lookup[ciPositiveRun, "StandardOutput", ""], "Seed: 12345"]];
ciReplayRun = ciCompare[ciPositive, "calchep_positive"];
ciReplayLines[text_String] := Select[StringSplit[text, "\n"],
  StringStartsQ[#, "Seed: "] || StringStartsQ[#, "Points requested/generated/compared: "] ||
    StringStartsQ[#, "Mismatches/errors: "] || StringStartsQ[#, "Maximum relative/absolute error: "] &];
check["the same seed reproduces the full compared-point and error summary",
  ciPassed[ciReplayRun] && ciReplayLines[ciPositiveRun["StandardOutput"]] ===
    ciReplayLines[ciReplayRun["StandardOutput"]]];

ciAutoIR = ConstructiveDiagrams`Private`spinasFamilyIR[ciFour, ciModel,
  <|"ParameterRules" -> ciRules, "IncomingLegs" -> {1, 2}, "FixedHelicities" -> Automatic|>];
ciAutomatic = ConstructiveDiagrams`Private`spinasCalcHEPConfiguration[ciAutoIR, ciModel,
  Join[Association[Options[WriteSPINASProcess]],
    <|"CalcHEPDirectory" -> ciCalcHEP, "ParameterRules" -> ciRules|>]];
check["recognized pure-QED model data selects the matching massive CalcHEP counterpart automatically",
  AssociationQ[ciAutomatic] && ciAutomatic["Status"] === "AVAILABLE" &&
    ciAutomatic["ModelNumber"] === 2 && ciAutomatic["PhysicalPDGs"] === {11, -11, 22, 22}];
ciComptonProcesses = Join[{"e", #[[1]], "E"}, {#[[2]]}] & /@ Tuples[{"A.+", "A.-"}, 2];
ciComptonIR = ConstructiveDiagrams`Private`spinasFamilyIR[ciRecords[ciComptonProcesses], ciModel,
  <|"ParameterRules" -> ciRules, "IncomingLegs" -> {1, 2}, "FixedHelicities" -> Automatic|>,
  {"e", "A"} -> {"e", "A"}];
ciComptonConfig = ConstructiveDiagrams`Private`spinasCalcHEPConfiguration[ciComptonIR, ciModel,
  Join[Association[Options[WriteSPINASProcess]], <|"CalcHEPDirectory" -> ciCalcHEP,
    "CalcHEPModel" -> ciCounterpart, "ParameterRules" -> ciRules|>]];
check["outgoing charged species are conjugated once when building the CalcHEP process",
  ciComptonConfig["PhysicalPDGs"] === {11, 22, 11, 22}];

Print["Checking that a deliberately doubled amplitude is rejected."];
ciWrong = ciExport["calchep_wrong", ({First[#], 2 Last[#]} & /@ ciFour), "Build" -> True];
check["the deliberately wrong amplitude still generates a runnable validation project", AssociationQ[ciWrong]];
If[AssociationQ[ciWrong],
  ciWrongRun = ciCompare[ciWrong, "calchep_wrong"];
  check["a factor-of-two amplitude error fails every squared-amplitude comparison rather than being fitted away",
    !ciSucceeded[ciWrongRun] && StringContainsQ[Lookup[ciWrongRun, "StandardOutput", ""],
      "Amplitude validation: FAIL"] &&
    StringContainsQ[Lookup[ciWrongRun, "StandardOutput", ""], "Mismatches/errors: 1000/0"]];
  check["a disagreement retains the first common physical phase-space point for replay",
    StringContainsQ[Lookup[ciWrongRun, "StandardOutput", ""], "First failing point: 1"] &&
      AllTrue[{"  p1 =", "  p2 =", "  p3 =", "  p4 ="},
        StringContainsQ[Lookup[ciWrongRun, "StandardOutput", ""], #] &]],
  Print["WRONG-AMPLITUDE EXPORT: ", InputForm[ciWrong]]];

ciMismatchDirectory = CreateDirectory[ciPath["massless_counterpart"]];
Do[CopyFile[file, FileNameJoin[{ciMismatchDirectory, FileNameTake[file]}]], {file, ciOriginalFiles}];
ciMismatchParticleFile = FileNameJoin[{ciMismatchDirectory, "prtcls2.mdl"}];
ciMismatchText = Import[ciMismatchParticleFile, "Text"];
check["the deliberate counterpart mass mismatch fixture changes only its copied electron mass field",
  StringContainsQ[ciMismatchText, "| Me |"]];
Export[ciMismatchParticleFile, StringReplace[ciMismatchText, "| Me |" -> "| 0 |"], "Text"];
ciMismatch = ciExport["calchep_mass_mismatch", ciFour,
  "CalcHEPModel" -> {ciMismatchDirectory, 2}, "Build" -> True];
check["a distinct counterpart model can be selected without changing the source theory", AssociationQ[ciMismatch]];
If[AssociationQ[ciMismatch],
  ciMismatchRun = ciCompare[ciMismatch, "calchep_mass_mismatch"];
  check["external mass disagreement is a configuration error before phase-space comparisons",
    !ciSucceeded[ciMismatchRun] &&
    (StringContainsQ[Lookup[ciMismatchRun, "StandardOutput", ""], "Amplitude validation: ERROR"] ||
      StringContainsQ[Lookup[ciMismatchRun, "StandardOutput", ""], "Amplitude validation: UNAVAILABLE"]) &&
    StringContainsQ[Lookup[ciMismatchRun, "StandardOutput", ""], "Points requested/generated/compared: 1000/0/0"]]];

If[Environment["HEPCAT_TEST_CALCHEP_FIVE_POINT"] =!= "0",
  Print["Generating the three-photon CalcHEP comparison."];
  ciFiveProcesses = Join[{"e", "E"}, Join[ConstantArray["A.+", 3 - #], ConstantArray["A.-", #]]] & /@ Range[0, 3];
  ciFiveEntries = ciRecords[ciFiveProcesses];
  check["all independent three-photon helicity sectors are present as known model amplitudes",
    FreeQ[ciFiveEntries, _LookupAmplitude | _ConstructiveAmplitude]];
  ciFive = ciExport["calchep_five", ciFiveEntries, "Build" -> True];
  check["the five-point family compiles with all eight photon-helicity arrangements",
    AssociationQ[ciFive] && Length[Lookup[ciFive, "HelicityConfigurations", {}]] === 8];
  If[AssociationQ[ciFive], ciFiveRun = ciCompare[ciFive, "calchep_five"];
    check["1000 shared five-point random points agree with CalcHEP", ciPassed[ciFiveRun]];
    If[!ciPassed[ciFiveRun], Print["CALCHEP FIVE-POINT COMPARISON: ", ciFiveRun]]],
  Print["SKIP: HEPCAT_TEST_CALCHEP_FIVE_POINT=0 omits the optional five-point CalcHEP construction."]];
check["validation leaves all original CalcHEP model files and the HEPCAT model unchanged",
  ciOriginalBytes === (BinaryReadList[#, "Byte"] & /@ ciOriginalFiles) && ciModel === ciModelBefore];
Print["SPINAS CALCHEP INTEGRATION CHECKS: ", checks, "; FAILURES: ", Length[failures]];
If[failures === {}, DeleteDirectory[ciDirectory, DeleteContents -> True];
  Print["HEPCAT_TEST_COMPLETED:spinas-calchep-integration-tests.wl"],
  Print[failures]; Print["CALCHEP INTEGRATION FILES: ", ciDirectory]];
Quit[If[failures === {}, 0, 1]];
