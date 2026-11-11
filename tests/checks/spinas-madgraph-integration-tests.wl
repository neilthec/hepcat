(* Opt-in: build an independent MadGraph reference and compare explicit helicities. *)

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


miRoot = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{miRoot, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0;
miChecks = 0; miFailures = {};
SetAttributes[miCheck, HoldRest];
miCheck[name_, assertion_] := Module[{result},
  miChecks++;
  result = Quiet[Check[assertion, $Failed]];
  If[!TrueQ[result], AppendTo[miFailures, name]; Print["FAIL: ", name]]
];
miTemporary = CreateDirectory[];
miPath[name_] := FileNameJoin[{miTemporary, name}];
miSpinas = Replace[Environment["HEPCAT_SPINAS_DIRECTORY"],
  $Failed | "" -> FileNameJoin[{ParentDirectory[miRoot], "spinas"}]];
miMadGraph = Replace[Environment["HEPCAT_MADGRAPH_DIRECTORY"],
  $Failed | "" -> FileNameJoin[{ParentDirectory[miRoot], "MadGraph"}]];
miModel = ReadModel[FileNameJoin[{miRoot, "models"}], 3];
miModelBefore = miModel;
miRules = {"Me" -> 1, "EE" -> 1/3};
miProcesses = Join[{"e", "E"}, #] & /@
  {{"A.+", "A.+"}, {"A.+", "A.-"}, {"A.-", "A.+"}, {"A.-", "A.-"}};
miFamily = {#, LookupAmplitude[Thread[{#, Range[4]}], miModel,
    False, {miI, miJ, miK, miL}]} & /@ miProcesses;
miExport[name_String, entries_List, opts___] := WriteSPINASProcess[miPath[name],
  {"e", "E"} -> {"A", "A"}, entries, miModel, opts,
  "ProcessName" -> name, "ParameterRules" -> miRules,
  "SPINASDirectory" -> miSpinas, "MadGraphDirectory" -> miMadGraph,
  "MadGraphModel" -> "sm-full", "MadGraphAllowedParticles" -> {11, 22},
  "CalcHEPDirectory" -> None, "CalcHEPModel" -> None, "ReturnDetails" -> True];
miRows[report_] := Lookup[report, "Rows", {}];
miPassed[report_, rows_Integer, points_Integer] := AssociationQ[report] &&
  Lookup[report, "Status", None] === "PASS" &&
  Length[miRows[report]] === rows &&
  AllTrue[miRows[report], Lookup[#, "Status", None] === "PASS" &&
    Lookup[#, "Points", -1] === points && Lookup[#, "Mismatches", -1] === 0 &&
    Lookup[#, "EvaluationErrors", -1] === 0 &];

Print["Exporting the massive-electron QED helicity family for independent MadGraph validation."];
miPositive = miExport["madgraph_positive", miFamily];
miCheck["source export retains the available MadGraph counterpart",
  AssociationQ[miPositive] && miPositive["MadGraphValidation"]["Configuration"]["Status"] === "AVAILABLE"];
If[!AssociationQ[miPositive], Print["EXPORT FAILURE: ", InputForm[miPositive]]; Exit[1]];
miPositiveReport = ValidateMadGraph[miPositive["Directory"], "Output" -> "Details", "MadGraphSeed" -> 12345];
miCheck["1000 common random points pass independently for all four photon helicities",
  miPassed[miPositiveReport, 4, 1000]];
If[!miPassed[miPositiveReport, 4, 1000], Print["POSITIVE REPORT: ", InputForm[miPositiveReport]]];
miCheck["the complete photon-helicity coverage is explicit and no massive-spin basis is selected",
  Sort[Lookup[miRows[miPositiveReport], "Helicities", {}]] ===
    Sort[Join[{0, 0}, #] & /@ Tuples[{-2, 2}, 2]]];
miCheck["the numerical report records the actual replay seed",
  AssociationQ[miPositiveReport] && miPositiveReport["Seed"] === 12345];
miCheck["detailed results are backed by a saved JSON report",
  AssociationQ[miPositiveReport] && FileExistsQ[miPositiveReport["ReportFile"]] &&
    Import[miPositiveReport["ReportFile"], "RawJSON"]["Status"] === "PASS"];
miIgnoredChanges = {
  ValidateMadGraph[miPositive["Directory"], "ParameterRules" -> {"Me" -> 2}],
  ValidateMadGraph[miPositive["Directory"], "FixedHelicities" -> {3 -> 2}]};
miCheck["stored model and helicity changes are rejected rather than silently ignored",
  AllTrue[miIgnoredChanges, MatchQ[#, Failure["MadGraphProjectConfiguration", _Association]] &]];
miReference = Lookup[miPositiveReport, "Reference", <||>];
miCheck["the independent generator and model provenance are recorded",
  AssociationQ[miReference] && AssociationQ[Lookup[miReference, "MadGraphVersion", None]] &&
    StringQ[miReference["MadGraphVersion"]["Version"]] &&
    StringLength[miReference["MadGraphVersion"]["Version"]] > 0];
miCheck["bare-propagator validation explicitly records zero unmatched reference widths",
  AssociationQ[miReference] &&
    StringContainsQ[Lookup[miReference, "WidthPolicy", ""], "Bare tree"] &&
    ListQ[Lookup[miReference, "ZeroedWidths", None]] &&
    AllTrue[miReference["ZeroedWidths"], TrueQ[Lookup[#, "Value", None] == 0] &] &&
    AllTrue[{"WZ", "WW"}, MemberQ[Lookup[miReference["ZeroedWidths"], "Name"], #] &] &&
    ListQ[Lookup[miReference, "PreservedWidths", None]]];
miCheck["every external reference mass is verified against the exported process",
  AssociationQ[miReference] && Length[Lookup[miReference, "MassMatching", {}]] === 4 &&
    And @@ MapThread[TrueQ[Abs[#1 - #2] < 10^-12] &,
      {Lookup[miReference["MassMatching"], "RequestedMass"], {1, 1, 0, 0}}]];
miReplay = ValidateMadGraph[miPositive["Directory"], "Output" -> "Details", "MadGraphSeed" -> 12345];
miCheck["the same seed reproduces every per-helicity numerical summary",
  miPassed[miReplay, 4, 1000] &&
    (KeyTake[#, {"Helicities", "Points", "MaximumRelativeError", "MaximumAbsoluteError"}] & /@ miRows[miReplay]) ===
    (KeyTake[#, {"Helicities", "Points", "MaximumRelativeError", "MaximumAbsoluteError"}] & /@ miRows[miPositiveReport])];

Print["Checking that one wrong photon-helicity branch is reported as a physical mismatch."];
miWrongFamily = miFamily;
miWrongFamily[[1, 2]] = 2 miWrongFamily[[1, 2]];
miWrong = miExport["madgraph_wrong", miWrongFamily];
miCheck["a deliberately wrong amplitude still exports as a runnable numerical comparison", AssociationQ[miWrong]];
If[AssociationQ[miWrong],
  miWrongReport = ValidateMadGraph[miWrong["Directory"], "Output" -> "Details",
    "MadGraphPoints" -> 5, "MadGraphSeed" -> 12345];
  miBadRows = Select[miRows[miWrongReport], Lookup[#, "Status", None] === "FAIL" &];
  miCheck["a doubled amplitude cannot be hidden by fitting a normalization ratio",
    AssociationQ[miWrongReport] && miWrongReport["Status"] === "FAIL" &&
      Length[miBadRows] === 1 && miBadRows[[1]]["Helicities"] === {0, 0, 2, 2} &&
      miBadRows[[1]]["Mismatches"] === 5 &&
      Count[Lookup[miRows[miWrongReport], "Status"], "PASS"] === 3];
  miCheck["the first failing common momenta and both numerical values are retained",
    Length[miBadRows] === 1 && AssociationQ[miBadRows[[1]]["FirstFailure"]] &&
      miBadRows[[1]]["FirstFailure"]["Point"] === 1 &&
      Dimensions[miBadRows[[1]]["FirstFailure"]["Momenta"]] === {4, 4} &&
      NumericQ[miBadRows[[1]]["FirstFailure"]["HEPCATValue"]] &&
      NumericQ[miBadRows[[1]]["FirstFailure"]["MadGraphValue"]]]];

Print["Checking that a global amplitude phase is physically irrelevant."];
miPhaseFamily = {First[#], I Last[#]} & /@ miFamily;
miPhase = miExport["madgraph_global_phase", miPhaseFamily];
miCheck["a global imaginary phase exports without changing the reference model", AssociationQ[miPhase]];
If[AssociationQ[miPhase],
  miPhaseReport = ValidateMadGraph[miPhase["Directory"], "Output" -> "Details",
    "MadGraphPoints" -> 20, "MadGraphSeed" -> 54321];
  miCheck["multiplying every amplitude by I leaves all fixed-helicity comparisons passing",
    miPassed[miPhaseReport, 4, 20]]];

Print["Checking a fixed outgoing photon helicity without changing the normalization convention."];
miSelected = miExport["madgraph_selected", miFamily, "FixedHelicities" -> {3 -> 2}];
miCheck["the selected-helicity process exports successfully", AssociationQ[miSelected]];
If[AssociationQ[miSelected],
  miSelectedReport = ValidateMadGraph[miSelected["Directory"], "Output" -> "Details",
    "MadGraphPoints" -> 30, "MadGraphSeed" -> 6789];
  miCheck["only the two requested photon combinations are checked, and both agree",
    miPassed[miSelectedReport, 2, 30] &&
      AllTrue[miRows[miSelectedReport], #["Helicities"][[3]] === 2 &]]];
Print["Checking incoming and outgoing photon helicities in QED Compton scattering."];
miComptonProcesses = ({"e", #[[1]], "E", #[[2]]} &) /@
  Tuples[{"A.+", "A.-"}, 2];
miComptonEntries = {#, LookupAmplitude[Thread[{#, Range[4]}], miModel,
  False, {miI, miJ, miK, miL}]} & /@ miComptonProcesses;
miCompton = WriteSPINASProcess[miPath["madgraph_compton"],
  {"e", "A"} -> {"e", "A"}, miComptonEntries, miModel,
  "ProcessName" -> "madgraph_compton", "ParameterRules" -> miRules,
  "SPINASDirectory" -> miSpinas, "MadGraphDirectory" -> miMadGraph,
  "MadGraphModel" -> "sm-full", "MadGraphAllowedParticles" -> {11, 22},
  "CalcHEPDirectory" -> None, "CalcHEPModel" -> None, "ReturnDetails" -> True];
miCheck["the incoming/outgoing photon process exports all four polarized branches",
  AssociationQ[miCompton]];
If[AssociationQ[miCompton],
  miComptonReport = ValidateMadGraph[miCompton["Directory"], "Output" -> "Details",
    "MadGraphPoints" -> 40, "MadGraphSeed" -> 83927];
  miCheck["Compton helicities agree separately after outgoing-vector convention conversion",
    miPassed[miComptonReport, 4, 40]];
  If[!miPassed[miComptonReport, 4, 40], Print["COMPTON REPORT: ", InputForm[miComptonReport]]]];
Print["Checking photon-only QED exchange in a four-fermion process."];
miFermionProcess = {"e", "E", "E", "e"};
miFermionAmplitude = LookupAmplitude[Thread[{miFermionProcess, Range[4]}], miModel,
  False, {miI, miJ, miK, miL}];
miCheck["the factorization model supplies the known QED four-fermion amplitude",
  miFermionAmplitude =!= 0 && FreeQ[miFermionAmplitude, LookupAmplitude]];
miFermion = WriteSPINASProcess[miPath["madgraph_four_fermions"],
  {"e", "E"} -> {"e", "E"}, {{miFermionProcess, miFermionAmplitude}}, miModel,
  "ProcessName" -> "madgraph_four_fermions", "ParameterRules" -> miRules,
  "SPINASDirectory" -> miSpinas, "MadGraphDirectory" -> miMadGraph,
  "MadGraphModel" -> "sm-full", "MadGraphAllowedParticles" -> {11, 22},
  "CalcHEPDirectory" -> None, "CalcHEPModel" -> None, "ReturnDetails" -> True];
miCheck["the four-fermion caller explicitly selects electron/photon intermediate states",
  AssociationQ[miFermion] &&
    miFermion["MadGraphValidation"]["Configuration"]["AllowedParticlePDGs"] === {11, 22}];
If[AssociationQ[miFermion],
  miFermionReport = ValidateMadGraph[miFermion["Directory"], "Output" -> "Details", "MadGraphSeed" -> 24680];
  miCheck["the four-fermion QED amplitude agrees on 1000 points without unwanted weak exchange",
    miPassed[miFermionReport, 1, 1000] && miRows[miFermionReport][[1]]["Helicities"] === {0, 0, 0, 0}];
  If[!miPassed[miFermionReport, 1, 1000], Print["FOUR-FERMION REPORT: ", InputForm[miFermionReport]]]];

Print["Checking the direct Mathematica shortcut with explicit generator, model and particle sector."];
miShortcut = ValidateMadGraph[{"e", "E"} -> {"A", "A"}, miFamily, miModel,
  "MadGraphDirectory" -> miMadGraph, "MadGraphModel" -> "sm-full",
  "MadGraphAllowedParticles" -> {11, 22}, "SPINASDirectory" -> miSpinas,
  "ParameterRules" -> miRules, "MadGraphPoints" -> 20, "MadGraphSeed" -> 112233,
  "CalcHEPDirectory" -> None, "CalcHEPModel" -> None, "Output" -> "Details"];
miCheck["the direct public shortcut honors the explicitly selected reference setup",
  miPassed[miShortcut, 4, 20] && miShortcut["Seed"] === 112233];

miCompactTemporary = RegularExpression["\\bconst\\s+auto\\s+_hepcat_v[0-9]+\\b"];
miCheck["small direct validation exports retain the ordinary arithmetic",
  AssociationQ[miShortcut] && !StringContainsQ[
    Import[FileNameJoin[{miShortcut["Directory"], "process.cpp"}], "Text"], miCompactTemporary]];

Print["Checking compact validation arithmetic against independent MadGraph helicities."];
miCompact = Block[{ConstructiveDiagrams`Private`$spinasValidationArithmeticLeafLimit = 0},
  miExport["madgraph_compact", miFamily]];
miCheck["the compact export writes local arithmetic temporaries",
  AssociationQ[miCompact] && StringContainsQ[
    Import[FileNameJoin[{miCompact["Directory"], "process.cpp"}], "Text"], miCompactTemporary]];
If[AssociationQ[miCompact],
  miCompactReport = ValidateMadGraph[miCompact["Directory"], "Output" -> "Details",
    "MadGraphPoints" -> 20, "MadGraphSeed" -> 112233];
  miCheck["compact arithmetic agrees with native MadGraph in every helicity sector",
    miPassed[miCompactReport, 4, 20]]];

Print["Checking that a genuine CalcHEP model mismatch does not suppress the independent MadGraph check."];
miCalcHEP = Replace[Environment["HEPCAT_CALCHEP_DIRECTORY"], $Failed | "" ->
  FileNameJoin[{ParentDirectory[miRoot], "CalcHEP", "calchep_3.9.2"}]];
miCombined = WriteSPINASProcess[miPath["madgraph_combined_reference_mismatch"],
  {"e", "E"} -> {"e", "E"}, {{miFermionProcess, miFermionAmplitude}}, miModel,
  "ProcessName" -> "madgraph_combined_reference_mismatch", "ParameterRules" -> miRules,
  "SPINASDirectory" -> miSpinas,
  "CalcHEPDirectory" -> miCalcHEP, "CalcHEPModel" -> {FileNameJoin[{miRoot, "tests", "reference-models", "calchep"}], 1},
  "CalcHEPAllowedParticles" -> All, "CalcHEPPoints" -> 3, "CalcHEPSeed" -> 12345,
  "MadGraphDirectory" -> miMadGraph, "MadGraphModel" -> "sm-full",
  "MadGraphAllowedParticles" -> {11, 22}, "MadGraphPoints" -> 3, "MadGraphSeed" -> 12345,
  "RunCalcHEPValidation" -> True, "RunMadGraphValidation" -> True, "ReturnDetails" -> True];
miCheck["a real full-SM reference mismatch is returned as a failed requested comparison",
  MatchQ[miCombined, Failure["SPINASValidation", _Association]] &&
    miCombined[[2]]["FailedReferences"] === {"CalcHEP"}];
If[FailureQ[miCombined] && AssociationQ[Lookup[miCombined[[2]], "ExportResult", None]],
  miCombinedResult = miCombined[[2]]["ExportResult"];
  miCheck["MadGraph still completes after the genuine CalcHEP numerical failure",
    FailureQ[miCombinedResult["CalcHEPValidation"]] &&
      StringContainsQ[miCombinedResult["CalcHEPValidation"][[2]]["CommandResult"]["StandardOutput"],
        "Amplitude validation: FAIL"] &&
      miPassed[miCombinedResult["MadGraphValidation"], 1, 3]],
  miCheck["the combined failure retains both reports", False]];

Print["Checking that RunMadGraphValidation cannot disguise a numerical FAIL as ordinary export success."];
miExportWrong = miExport["madgraph_wrong_requested", miWrongFamily,
  "RunMadGraphValidation" -> True, "MadGraphPoints" -> 3, "MadGraphSeed" -> 12345];
miCheck["a genuinely wrong requested MadGraph comparison returns Failure with its numerical report",
  MatchQ[miExportWrong, Failure["SPINASValidation", _Association]] &&
    miExportWrong[[2]]["FailedReferences"] === {"MadGraph"} &&
    miExportWrong[[2]]["ExportResult"]["MadGraphValidation"]["Status"] === "FAIL" &&
    Length[miRows[miExportWrong[[2]]["ExportResult"]["MadGraphValidation"]]] === 4];

miCheck["independent numerical validation does not mutate the supplied HEPCAT model", miModel === miModelBefore];

Print["MadGraph integration artifacts: ", miTemporary];
Print["MadGraph integration: ", miChecks - Length[miFailures], " / ", miChecks, " checks passed."];
If[miFailures =!= {}, Print["Failures: ", miFailures]; Exit[1]];
DeleteDirectory[miTemporary, DeleteContents -> True];
Print["HEPCAT_TEST_COMPLETED:spinas-madgraph-integration-tests.wl"];
Exit[0];
