(* Opt-in: real public export, MadEvent integration and unweighted LHE output.
   Select this suite explicitly with an external timeout of at least 3600s.
   It creates a private export and never changes the installed MadGraph tree. *)
smeiRoot = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{smeiRoot, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0;
smeiChecks = 0; smeiFailures = {};
SetAttributes[smeiCheck, HoldRest];
smeiCheck[name_, value_] := (smeiChecks++; If[!TrueQ[value],
  AppendTo[smeiFailures, name]; Print["FAIL: ", name]]);
smeiTemporary = CreateDirectory[];
smeiSpinas = Replace[Environment["HEPCAT_SPINAS_DIRECTORY"],
  $Failed | "" -> FileNameJoin[{ParentDirectory[smeiRoot], "spinas"}]];
smeiMadGraph = Replace[Environment["HEPCAT_MADGRAPH_DIRECTORY"],
  $Failed | "" -> FileNameJoin[{ParentDirectory[smeiRoot], "MadGraph"}]];
smeiModel = ReadModel[FileNameJoin[{smeiRoot, "models"}], 3];
smeiModelBefore = smeiModel;
smeiProcesses = Join[{"e", "E"}, #] & /@
  {{"A.+", "A.+"}, {"A.+", "A.-"}, {"A.-", "A.+"}, {"A.-", "A.-"}};
smeiFamily = {#, LookupAmplitude[Thread[{#, Range[4]}], smeiModel,
    False, {smeiI, smeiJ, smeiK, smeiL}]} & /@ smeiProcesses;
Print["Building the public eeAA SPINAS export and its MadEvent numerical bridge."];
smeiWrite[name_String, entries_] := WriteSPINASProcess[FileNameJoin[{smeiTemporary, name}],
  {"e", "E"} -> {"A", "A"}, entries, smeiModel,
  "ProcessName" -> name, "ParameterRules" -> {"Me" -> 1, "EE" -> 1/3},
  "SPINASDirectory" -> smeiSpinas, "MadGraphDirectory" -> smeiMadGraph,
  "MadGraphModel" -> "sm-full", "MadGraphAllowedParticles" -> {11, 22},
  "CalcHEPDirectory" -> None, "CalcHEPModel" -> None,
  "Build" -> True, "ReturnDetails" -> True];
smeiExport = smeiWrite["eeAA", smeiFamily];
smeiCheck["the public exporter builds the combined numerical project", AssociationQ[smeiExport]];
If[!AssociationQ[smeiExport], Print["EXPORT FAILURE: ", InputForm[smeiExport]]; Exit[1]];
smeiConfiguration = smeiExport["MadEventNumerical"];
smeiCheck["the chosen numerical model and launcher are available",
  smeiConfiguration["Status"] === "AVAILABLE" && FileExistsQ[smeiConfiguration["Executable"]]];
If[!TrueQ[smeiConfiguration["Status"] === "AVAILABLE"],
  Print["CONFIGURATION FAILURE: ", InputForm[smeiConfiguration]]; Exit[1]];
smeiScaledFamily = {First[#], 2 Last[#]} & /@ smeiFamily;
smeiScaledExport = smeiWrite["eeAA_scaled", smeiScaledFamily];
smeiCheck["an independently built doubled HEPCAT amplitude preserves the same UFO theory",
  AssociationQ[smeiScaledExport] &&
    smeiScaledExport["MadEventNumerical"]["Status"] === "AVAILABLE" &&
    smeiScaledExport["MadEventNumerical"]["MadGraphModel"] === smeiConfiguration["MadGraphModel"]];
If[!AssociationQ[smeiScaledExport], Print["SCALED EXPORT FAILURE: ", InputForm[smeiScaledExport]]; Exit[1]];
smeiDriver = FileNameJoin[{smeiRoot, "tests", "checks", "madevent-integration-driver.py"}];
Print["Running native MadEvent integration, charge rebinding and mass rebinding checks."];
smeiRun = RunProcess[{smeiConfiguration["PythonExecutable"], smeiDriver,
  "--project", smeiExport["Directory"], "--launcher", smeiConfiguration["Executable"],
  "--scaled-project", smeiScaledExport["Directory"],
  "--scaled-launcher", smeiScaledExport["MadEventNumerical"]["Executable"],
  "--timeout", "900"}];
smeiCheck["native survey/refine, unweighted events and parameter changes succeed",
  smeiRun["ExitCode"] === 0 && StringContainsQ[smeiRun["StandardOutput"], "MADEVENT_INTEGRATION_PASS"]];
If[smeiRun["ExitCode"] =!= 0,
  Print["INTEGRATION FAILURE: ", InputForm[smeiRun]]];
smeiReportPath = FileNameJoin[{smeiExport["Directory"], "madevent-integration-test.json"}];
smeiCheck["integration success is backed by a saved numerical report", FileExistsQ[smeiReportPath]];
If[FileExistsQ[smeiReportPath],
  smeiReport = Import[smeiReportPath, "RawJSON"];
  smeiCheck["all four genuine numerical runs return their requested LHE events",
    smeiReport["Status"] === "PASS" && Length[smeiReport["Runs"]] === 4 &&
      AllTrue[smeiReport["Runs"], #["Events"] === 20 && NumericQ[#["CrossSectionPB"]] &&
        #["CrossSectionPB"] > 0 && FileExistsQ[#["EventFile"]] &]];
  smeiCheck["the binary writes its session and events beneath the directory it was launched from",
    smeiReport["WorkingDirectory"] === FileNameJoin[{smeiExport["Directory"], "build"}] &&
      smeiReport["MadEventDirectory"] === FileNameJoin[{smeiExport["Directory"], "build", "madevent"}] &&
      !DirectoryQ[FileNameJoin[{smeiExport["Directory"], "madevent"}]] &&
      smeiReport["ScaledWorkingDirectory"] === FileNameJoin[{smeiScaledExport["Directory"], "build"}]];
  smeiCheck["doubling the live QED charge produces the expected rate scaling",
    Abs[smeiReport["ChargeRateRatio"]/16 - 1] < 1/5];
  smeiCheck["the integration uses the supplied amplitude rather than falling back to native HELAS",
    Abs[smeiReport["ScaledAmplitudeRateRatio"]/4 - 1] < 1/5]];
smeiCheck["integration preserves the supplied HEPCAT model", smeiModel === smeiModelBefore];
Print["MadEvent integration artifacts: ", smeiTemporary];
Print["MadEvent integration: ", smeiChecks - Length[smeiFailures], " / ", smeiChecks, " checks passed."];
If[smeiFailures =!= {}, Print["Failures: ", smeiFailures]; Exit[1]];
DeleteDirectory[smeiTemporary, DeleteContents -> True];
Print["HEPCAT_TEST_COMPLETED:spinas-madevent-integration-tests.wl"];
Exit[0];
