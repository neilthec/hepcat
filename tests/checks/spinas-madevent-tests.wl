(* Public combined MadGraph validation and MadEvent export surface.
   These checks generate source only. The genuine integration regression is
   separate, so writing source cannot masquerade as a numerical PASS. *)

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


smeRoot = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{smeRoot, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0;
smeChecks = 0; smeFailures = {};
SetAttributes[smeCheck, HoldRest];
smeCheck[name_, value_] := (smeChecks++; If[!TrueQ[value],
  AppendTo[smeFailures, name]; Print["FAIL: ", name]]);
smeTemporary = CreateDirectory[];
smePath[name_] := FileNameJoin[{smeTemporary, name}];
smeModel = ReadModel[FileNameJoin[{smeRoot, "models"}], 3];
smeModelBefore = smeModel;
smeGlobalsBefore = {OwnValues[Global`Me], OwnValues[Global`EE]};
smeSpinas = Replace[Environment["HEPCAT_SPINAS_DIRECTORY"],
  $Failed | "" -> FileNameJoin[{ParentDirectory[smeRoot], "spinas"}]];
smeProcesses = Join[{"e", "E"}, #] & /@
  {{"A.+", "A.+"}, {"A.+", "A.-"}, {"A.-", "A.+"}, {"A.-", "A.-"}};
smeFamily = {#, LookupAmplitude[Thread[{#, Range[4]}], smeModel,
    False, {smeI, smeJ, smeK, smeL}]} & /@ smeProcesses;
(* No interpreter is executed by a source-only export. Isolate its availability
   probe so this unit suite does not require an installed Python or MadGraph. *)
smeExport[name_String, physical_, opts___] := Block[{
  ConstructiveDiagrams`Private`spinasMadGraphPython = Function[{directory},
    If[directory === "", "", "/source-only/python3"]]},
  WriteSPINASProcess[smePath[name], physical, smeFamily, smeModel, opts,
    "ProcessName" -> "madevent_stub", "SPINASDirectory" -> smeSpinas, "ReturnDetails" -> True]];
smeManifest[report_Association] := Import[report["Files"]["Manifest"], "RawJSON"];

smeDefault = smeExport["unconfigured", {"e", "E"} -> {"A", "A"}];
smeCheck["ordinary SPINAS export remains usable without a MadGraph installation",
  AssociationQ[smeDefault]];
If[!AssociationQ[smeDefault], Print[smeDefault]; Exit[1]];
smeDefaultManifest = smeManifest[smeDefault];
smeCheck["unconfigured numerical integration has an honest availability report",
  smeDefaultManifest["MadEventNumerical"]["Status"] === "UNAVAILABLE" &&
    StringLength[smeDefaultManifest["MadEventNumerical"]["Reason"]] > 0];
smeCheck["unconfigured exports require no numerical MadEvent launcher",
  !FileExistsQ[FileNameJoin[{smeDefault["Directory"], "madevent-runtime.py"}]] &&
    !StringContainsQ[Import[smeDefault["Files"]["CMake"], "Text"], "add_executable(madevent_stub_madevent"]];

(* A minimal installation marker suffices for a source-only export. No UFO
   import, topology generation, compilation or launch is allowed here. *)
smeFakeMG = CreateDirectory[smePath["chosen-MadGraph"]];
CreateDirectory[FileNameJoin[{smeFakeMG, "bin"}]];
Export[FileNameJoin[{smeFakeMG, "bin", "mg5_aMC"}], "#!/usr/bin/env python3\n", "Text"];
smeSelected = smeExport["combined", {"e", "E"} -> {"A", "A"},
  "MadGraphDirectory" -> smeFakeMG, "MadGraphModel" -> "explicit-test-ufo",
  "MadGraphAllowedParticles" -> {11, 22},
  "MadGraphParameterMap" -> {"EE" -> "charge", "Me" -> "electron_mass"}];
smeCheck["the same public call emits both workflows", AssociationQ[smeSelected]];
If[AssociationQ[smeSelected],
  smeSelectedManifest = smeManifest[smeSelected];
  smeNumerical = smeSelectedManifest["MadEventNumerical"];
  If[smeNumerical["Status"] =!= "AVAILABLE", Print["NUMERICAL CONFIGURATION: ", InputForm[smeNumerical]]];
  smeReference = smeSelectedManifest["MadGraphValidation"];
  smeCheck["numerical sessions default to the compiled build directory",
    smeNumerical["SessionDirectory"] === "PROC_madevent_stub" &&
      smeNumerical["SessionDirectoryBase"] === "BuildDirectory" &&
      smeNumerical["Executable"] === FileNameJoin[{smeSelected["Directory"], "build",
        "PROC_madevent_stub", "bin", "madevent"}]];
  smeCheck["both workflows preserve the explicitly chosen model and particle sector",
    smeNumerical["Status"] === "AVAILABLE" &&
      And @@ (smeNumerical[#] === smeReference[#] & /@
        {"MadGraphDirectory", "MadGraphModel", "AllowedParticlePDGs", "ParameterMap"})];
  smeCheck["both workflows ship their independent runtime files together",
    AllTrue[FileNameJoin[{smeSelected["Directory"], #}] & /@
      {"madgraph-validation.cpp", "madgraph-runtime.py", "madevent-spinas.cpp",
       "madevent-runtime.py", "madevent-config.json"}, FileExistsQ]];
  smeCheck["source-only export does not emit an unused C++ launcher",
    !FileExistsQ[FileNameJoin[{smeSelected["Directory"], "madevent-main.cpp"}]]];
  smeCheck["topology generation remains lazy during source-only export",
    !DirectoryQ[FileNameJoin[{smeSelected["Directory"], smeNumerical["SessionDirectory"]}]] &&
      !DirectoryQ[FileNameJoin[{smeSelected["Directory"], "build", smeNumerical["SessionDirectory"]}]] &&
      FileNames["*", smeFakeMG, Infinity] === {FileNameJoin[{smeFakeMG, "bin"}],
        FileNameJoin[{smeFakeMG, "bin", "mg5_aMC"}]}];
  smeCheck["the numerical adapter keeps the raw matrix element separate from averaging",
    With[{source = Import[FileNameJoin[{smeSelected["Directory"], "madevent-spinas.cpp"}], "Text"]},
      StringContainsQ[source, "std::norm(process->amp(spins))"] &&
        !StringContainsQ[source, "process->amp2("]]];
  smeCheck["MadEvent outgoing-vector states are converted back to native SPINAS labels",
    With[{source = Import[FileNameJoin[{smeSelected["Directory"], "madevent-spinas.cpp"}], "Text"]},
      StringContainsQ[source, "massless[leg] && spin2[leg] == 2"] &&
        StringContainsQ[source, "mg_leg >= Process::incoming_legs().size()"] &&
        StringContainsQ[source, "spins[leg] = -spins[leg];"]]];
  smeCheck["normalization metadata retains one initial average and one photon factorial",
    smeNumerical["InitialSpinAverage"] == 4 && smeNumerical["SymmetryFactor"] == 1/2];
  smeCheck["the optional static matrix bridge shares the ordinary process library without a duplicate launcher",
    With[{cmake = Import[smeSelected["Files"]["CMake"], "Text"]},
      StringContainsQ[cmake, "madevent_stub_madevent_matrix STATIC"] &&
        !StringContainsQ[cmake, "add_executable(madevent_stub_madevent"] &&
        StringContainsQ[cmake, "madevent-link.json"]]];
  smeCheck["an ordinary build prepares the native process without generating events",
    With[{cmake = Import[smeSelected["Files"]["CMake"], "Text"]},
      StringContainsQ[cmake, "option(HEPCAT_PREPARE_MADEVENT"] &&
        StringContainsQ[cmake, "set(HEPCAT_MADEVENT_DEFAULT_TARGET ALL)"] &&
        StringContainsQ[cmake, "--bridge-link-file \"${CMAKE_CURRENT_BINARY_DIR}/madevent-link.json\" --prepare"] &&
        StringContainsQ[cmake, "DEPENDS madevent_stub_madevent_matrix madevent_stub_process"] &&
        !StringContainsQ[cmake, "--run " | "--refresh-generated"]]];
  smeCheck["the generated link metadata identifies headers and a static SPINAS archive for bundling",
    With[{cmake = Import[smeSelected["Files"]["CMake"], "Text"]},
      StringContainsQ[cmake, "SPINASIncludeDirectory"] &&
        StringContainsQ[cmake, "SPINASStaticLibrary"]]];
];

smeOverwrite = smeExport["combined", {"e", "E"} -> {"A", "A"},
  "MadGraphDirectory" -> smeFakeMG, "MadGraphModel" -> "explicit-test-ufo",
  "MadGraphAllowedParticles" -> {11, 22}, "Overwrite" -> True];
smeCheck["Overwrite True authorizes refresh of an owned stale MadEvent session",
  AssociationQ[smeOverwrite] &&
    StringContainsQ[Import[smeOverwrite["Files"]["CMake"], "Text"], "--prepare --refresh-generated"]];

smeFixed = smeExport["fixed-outgoing", {"e", "E"} -> {"A.+", "A"},
  "MadGraphDirectory" -> smeFakeMG, "MadGraphModel" -> "explicit-test-ufo"];
smeCheck["a fixed outgoing photon remains a selected contribution with the full identical factor",
  AssociationQ[smeFixed] && With[{c = smeManifest[smeFixed]["MadEventNumerical"]},
    c["InitialSpinAverage"] == 4 && c["SymmetryFactor"] == 1/2 &&
      Length[c["HelicityConfigurations"]] === 2 &&
      AllTrue[c["HelicityConfigurations"], #[[3]] === 2 &]]];

smeCrossed = Block[{
  ConstructiveDiagrams`Private`spinasMadGraphPython = Function[{directory},
    If[directory === "", "", "/source-only/python3"]]},
  WriteSPINASProcess[smePath["fixed-incoming"], smeFamily, smeModel,
    "IncomingLegs" -> {3, 1}, "FixedHelicities" -> {3 -> 2},
    "ProcessName" -> "madevent_stub", "SPINASDirectory" -> smeSpinas,
    "MadGraphDirectory" -> smeFakeMG, "MadGraphModel" -> "explicit-test-ufo",
    "ReturnDetails" -> True]];
smeCheck["a crossed fixed incoming photon preserves the explicit leg map and no helicity average",
  AssociationQ[smeCrossed] && With[{c = smeManifest[smeCrossed]["MadEventNumerical"]},
    c["LegOrder"] === {3, 1, 2, 4} && c["PhysicalPDGs"] === {22, 11, 11, 22} &&
      c["InitialSpinAverage"] == 2 && c["SymmetryFactor"] == 1]];
smeCheck["export does not mutate model parameters or global symbols",
  smeModel === smeModelBefore && {OwnValues[Global`Me], OwnValues[Global`EE]} === smeGlobalsBefore];
DeleteDirectory[smeTemporary, DeleteContents -> True];
Print["MadEvent public export: ", smeChecks - Length[smeFailures], " / ", smeChecks, " checks passed."];
If[smeFailures =!= {}, Print["Failures: ", smeFailures]; Exit[1]];
Print["HEPCAT_TEST_COMPLETED:spinas-madevent-tests.wl"];
Exit[0];
