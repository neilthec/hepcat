(* Exact public notebook call: no supplied answer, correction basis or tuning. *)

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


root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root, "source", "HEPCAT.wl"}]];
$HistoryLength = 0;
diagramModel = ReadModel[FileNameJoin[{root, "models"}], 3];
model = ReadModel[FileNameJoin[{root, "models"}], 5];
benchmark = ReadModel[FileNameJoin[{root, "tests", "reference-models", "calchep"}], 2];
before = {diagramModel, model, benchmark};
process = Join[{"e", "E"}, ConstantArray["A.+", 5]];
report = ConstructAmplitude[process, diagramModel, model,
  "PolePreference" -> "AllSpurious", "FeynmanModel" -> benchmark,
  "RandomSeed" -> 202611050705, "Output" -> "Details", "PrintReport" -> False,
  "ProgressReporting" -> True,
  "ProgressFunction" -> (If[MemberQ[{"StageStarted", "StageFinished"}, Lookup[#, "Event", ""]],
    Print[Lookup[#, "Event", ""], " ", Lookup[#, "Stage", ""], " ",
      Lookup[#, "TotalElapsed", ""]]] &)];
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, test_] := (checks++; If[!TrueQ[test], AppendTo[failures, name]; Print["FAIL: ", name]]);
check["the ordinary AllSpurious seven-point call completes",
  AssociationQ[report] && report["Status"] === "COMPLETE"];
If[AssociationQ[report] && report["Status"] === "COMPLETE",
  finalChecks = report["Checks"];
  spuriousCount = Length[finalChecks["SpuriousChecks"]];
  check["model four supplies all thirty physical channels",
    Length[finalChecks["Channels"]] === 30];
  check["all physical, spurious, energy, reference and Feynman checks pass",
    spuriousCount === 10 && finalChecks["Summary"] === <|"PASS" -> 33 + spuriousCount|>];
  check["the automatic expression reaches the verified compactness benchmark",
    IntegerQ[report["Complexity"]] && report["Complexity"] <= 1160];
  check["the resolved reference choice really gives spurious poles without Xi",
    !TrueQ[report["AchievedPhysicalDenominators"]] &&
      FreeQ[report["Amplitude"], (Spinor | SpinorHat)["Xi", ___]]];
  selection = report["RepresentativeSelection"];
  check["independent validation uses two parameter assignments without fitting the benchmark",
    selection["BenchmarkUsedForConstruction"] === False &&
      Length[selection["ValidationReports"]] === 2 &&
      AllTrue[selection["ValidationReports"],
        #["Report"]["Summary"] === <|"PASS" -> 33 + spuriousCount|> &]];
  check["the result uses the topology-derived same-chiral subset route",
    Lookup[report["Stages"]["Compact spurious form"], "Method", None] ===
      "SameChiralSpuriousSubsetCurrent"];
  Print["SEVEN-POINT ALLSPURIOUS RESULT ",
    KeyTake[report, {"Status", "Complexity", "TotalTime", "AchievedPhysicalDenominators"}]]
];
check["the supplied model data are preserved", before === {diagramModel, model, benchmark}];
savedReport = Environment["HEPCAT_SEVEN_SPURIOUS_REPORT"];
If[StringQ[savedReport] && savedReport =!= "", Export[savedReport, report, "WXF"]];
Print["CHECKS ", checks, "; FAILURES ", Length[failures]];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:seven-point-spurious-construction-integration-tests.wl"],
  Print[failures]];
Quit[If[failures === {}, 0, 1]];
