(* Opt-in checks against independent native SM generators. Model chirality
   supplies the complete state set; no polarized restriction is requested. *)

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
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, assertion_] := (checks++; If[!TrueQ[assertion],
  AppendTo[failures, name]; Print["FAIL: ", name]]);
directory = CreateDirectory[];
model = ReadModel[FileNameJoin[{root, "models"}], 1];
spinas = Replace[Environment["HEPCAT_SPINAS_DIRECTORY"],
  $Failed | "" -> FileNameJoin[{ParentDirectory[root], "spinas"}]];
madgraph = Replace[Environment["HEPCAT_MADGRAPH_DIRECTORY"],
  $Failed | "" -> FileNameJoin[{ParentDirectory[root], "MadGraph"}]];
calchep = Replace[Environment["HEPCAT_CALCHEP_DIRECTORY"],
  $Failed | "" -> FileNameJoin[{ParentDirectory[root], "CalcHEP", "calchep_3.9.2"}]];
cases = {
  {"neutrino_annihilation", {"ne", "Ne", "nm", "Nm"}, {-1, 1, 1, -1}},
  {"neutrino_scattering", {"ne", "nm", "Ne", "Nm"}, {-1, -1, -1, -1}}};
Do[
  {name, process, physicalHelicities} = entry;
  prepared = PrepareAmplitudes[DiagramAmplitudes[CreateDiagrams[process, model]], model];
  amplitude = Total[Last /@ prepared];
  report = WriteSPINASProcess[FileNameJoin[{directory, name}], {{process, amplitude}}, model,
    "ProcessName" -> name, "SPINASDirectory" -> spinas,
    "CalcHEPDirectory" -> calchep,
    "CalcHEPModel" -> {FileNameJoin[{root, "tests", "reference-models", "calchep"}], 1},
    "CalcHEPAllowedParticles" -> {12, 14, 23},
    "CalcHEPPoints" -> 30, "CalcHEPSeed" -> 38191,
    "RunCalcHEPValidation" -> True,
    "MadGraphDirectory" -> madgraph, "MadGraphModel" -> "sm-full",
    "MadGraphAllowedParticles" -> {12, 14, 23},
    "MadGraphParameterRules" -> {"lamWS" -> 0, "AWS" -> 0},
    "MadGraphPoints" -> 30, "MadGraphSeed" -> 38191,
    "RunMadGraphValidation" -> True, "ReturnDetails" -> True];
  check[name <> ": export runs both independent generators", AssociationQ[report]];
  If[AssociationQ[report],
    check[name <> ": physical crossing has exactly one state and no incoming spin average",
      report["HelicityConfigurations"] === {physicalHelicities} &&
        report["InitialSpinAverage"] === 1];
    ch = report["CalcHEPValidation"];
    mg = report["MadGraphValidation"];
    check[name <> ": CalcHEP agrees at all 30 points",
      Lookup[ch, "Status", None] === "PASS" &&
        StringContainsQ[Lookup[ch, "StandardOutput", ""], "Mismatches/errors: 0/0"]];
    check[name <> ": MadGraph agrees at all 30 points for the physical helicity",
      Lookup[mg, "Status", None] === "PASS" &&
        Lookup[Lookup[mg, "Rows", {}], "Helicities", {}] === {physicalHelicities} &&
        AllTrue[Lookup[mg, "Rows", {}],
          Lookup[#, "Points", 0] === 30 && Lookup[#, "Mismatches", -1] === 0 &]],
    Print["EXPORT FAILURE: ", InputForm[report]]],
  {entry, cases}];
Print["SPINAS CHIRAL INTEGRATION CHECKS: ", checks, "; FAILURES: ", Length[failures]];
If[failures === {}, DeleteDirectory[directory, DeleteContents -> True];
  Print["HEPCAT_TEST_COMPLETED:spinas-chiral-integration-tests.wl"],
  Print[failures]; Print["CHIRAL TEST FILES: ", directory]];
Quit[If[failures === {}, 0, 1]];
