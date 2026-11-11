(* Metadata, unavailable-backend behavior and shared-point validation need no CalcHEP installation. *)

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
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0;
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);
scDirectory = CreateDirectory[];
scPath[name_] := FileNameJoin[{scDirectory, name}];
scRun[command_List] := Quiet[Check[RunProcess[command], <|"ExitCode" -> -1|>]];
scSucceeded[report_] := AssociationQ[report] && Lookup[report, "ExitCode", -1] === 0;
scSpinas = Replace[Environment["HEPCAT_SPINAS_DIRECTORY"],
  $Failed | "" -> FileNameJoin[{ParentDirectory[root], "spinas"}]];
scModel = ReadModel[FileNameJoin[{root, "models"}], 4];
scModelBefore = scModel;
scGlobalsBefore = {OwnValues[Global`Me], OwnValues[Global`EE]};
scRules = {"Me" -> 1, "EE" -> 1/3};
scProcesses = Join[{"e", "E"}, #] & /@
  {{"A.+", "A.+"}, {"A.+", "A.-"}, {"A.-", "A.-"}};
scLookup[process_List] := LookupAmplitude[Thread[{process, Range[Length[process]]}],
  scModel, False, {scI, scJ, scK, scL}];
scFamily = {#, scLookup[#]} & /@ scProcesses;
scOptions = Join[Association[Options[WriteSPINASProcess]],
  <|"ParameterRules" -> scRules, "CalcHEPModel" -> None,
    "CalcHEPDirectory" -> scPath["not installed"]|>];
check["CalcHEP requires explicit installation and model selection, with an automatic time-based seed",
  ("CalcHEPDirectory" /. Options[WriteSPINASProcess]) === None &&
    ("CalcHEPModel" /. Options[WriteSPINASProcess]) === None &&
    ("CalcHEPAllowedParticles" /. Options[WriteSPINASProcess]) === All &&
    ("RunCalcHEPValidation" /. Options[WriteSPINASProcess]) === False &&
    ("CalcHEPPoints" /. Options[WriteSPINASProcess]) === 1000 &&
    ("CalcHEPSeed" /. Options[WriteSPINASProcess]) === Automatic];
scIR = ConstructiveDiagrams`Private`spinasFamilyIR[scFamily, scModel,
  KeyTake[scOptions, {"IncomingLegs", "ParameterRules", "FixedHelicities"}]];
scDisabled = ConstructiveDiagrams`Private`spinasCalcHEPConfiguration[scIR, scModel, scOptions];
check["an explicitly disabled CalcHEP counterpart is unavailable rather than a passing comparison",
  AssociationQ[scDisabled] && scDisabled["Status"] === "UNAVAILABLE" &&
    StringQ[Lookup[scDisabled, "Reason", None]] && StringLength[scDisabled["Reason"]] > 0];
check["opposite photon helicities remain one self-conjugate physical species",
  scDisabled["PhysicalPDGs"] === {11, -11, 22, 22}];
scExplicitOptions = Join[scOptions, <|"CalcHEPModel" -> {FileNameJoin[{root, "tests", "reference-models", "calchep"}], 2},
  "CalcHEPAllowedParticles" -> {22, 11, 22}|>];
scExplicit = ConstructiveDiagrams`Private`spinasCalcHEPConfiguration[scIR, scModel, scExplicitOptions];
check["explicit internal particle restrictions are normalized and retained in metadata",
  scExplicit["Supported"] && scExplicit["AllowedParticlePDGs"] === {11, 22} &&
    scExplicit["ParticleRestriction"] === "AllowedPDGs" && scExplicit["ModelNumber"] === 2];
check["the default particle choice preserves the complete explicitly selected theory",
  scDisabled["AllowedParticlePDGs"] === {} && scDisabled["ParticleRestriction"] === "All"];
scExcluded = ConstructiveDiagrams`Private`spinasCalcHEPConfiguration[scIR, scModel,
  Join[scExplicitOptions, <|"CalcHEPAllowedParticles" -> {22}|>]];
check["a restriction that excludes an external particle cannot run as a valid reference",
  scExcluded["Status"] === "UNAVAILABLE" && StringContainsQ[scExcluded["Reason"], "excludes an external"]];
check["invalid allowed particle lists are rejected before project export",
  AllTrue[{{}, {0}, {-11, 22}, {11.}, {True}, {2147483648}},
    !ConstructiveDiagrams`Private`spinasCalcHEPOptionsQ[
      Join[scOptions, <|"CalcHEPAllowedParticles" -> #|>]] &]];
check["explicitly disabled CalcHEP directories never trigger discovery",
  ConstructiveDiagrams`Private`spinasCalcHEPRoot[None] === "" &&
    ConstructiveDiagrams`Private`spinasCalcHEPNumericalRoot[None] === ""];
scSelectedSource = ConstructiveDiagrams`Private`spinasCalcHEPSource["calchep_selected", scIR, scExplicit];
check["the generated runtime receives the explicit restriction and reports excluded native names",
  StringContainsQ[scSelectedSource, "const std::vector<int> allowed_particle_pdgs = {11, 22};"] &&
    StringContainsQ[scSelectedSource, "false, allowed_particle_pdgs, 0, {"] &&
    StringContainsQ[scSelectedSource, "excluded_particle_names()"]];
scReordered = ConstructiveDiagrams`Private`spinasCalcHEPConfiguration[
  Join[scIR, <|"IncomingLegs" -> {2, 1}|>], scModel, scOptions];
check["the explicit incoming order determines both CalcHEP leg order and physical particle identities",
  scReordered["LegOrder"] === {2, 1, 3, 4} &&
    scReordered["PhysicalPDGs"] === {-11, 11, 22, 22}];
scExport = WriteSPINASProcess[scPath["unavailable"], {"e", "E"} -> {"A", "A"},
  scFamily, scModel, "ProcessName" -> "calchep_stub", "ParameterRules" -> scRules,
  "SPINASDirectory" -> scSpinas, "CalcHEPDirectory" -> scPath["not installed"],
  "CalcHEPModel" -> None, "ReturnDetails" -> True];
check["source export remains usable without a CalcHEP installation or selected model",
  AssociationQ[scExport] && DirectoryQ[scExport["Directory"]] &&
    Lookup[Lookup[scExport, "Build", <||>], "Status", None] === "NOT_REQUESTED"];
If[!AssociationQ[scExport], Print["CALCHEP STUB EXPORT: ", scExport]; Quit[1]];
scFiles = FileNameJoin[{scExport["Directory"], #}] & /@
  {"process.hpp", "process.cpp", "calchep-validation.cpp", "spinas-validation.hpp", "calchep-runtime.hpp"};
check["the generated project includes callable validation and its portable runtime helpers",
  AllTrue[scFiles, FileExistsQ]];
scHeader = Import[FileNameJoin[{scExport["Directory"], "process.hpp"}], "Text"];
scSource = Import[FileNameJoin[{scExport["Directory"], "calchep-validation.cpp"}], "Text"];
check["the public process class exposes validation while the stub explicitly reports unavailability",
  StringContainsQ[scHeader, "validate_calchep"] &&
    StringContainsQ[scSource, "hepcat_spinas::unavailable_validation("]];
scManifest = Import[scExport["Files"]["Manifest"], "RawJSON"];
check["the manifest identifies the missing benchmark without reporting a test pass",
  Lookup[Lookup[scManifest, "CalcHEPValidation", <||>], "Status", None] === "UNAVAILABLE"];
scCMake = Import[scExport["Files"]["CMake"], "Text"];
check["the generated project provides a standalone validation executable",
  StringContainsQ[scCMake, "calchep_stub_validate_calchep"]];

check["automatic seed metadata remains a runtime choice instead of a frozen export-time number",
  scManifest["CalcHEPValidation"]["Seed"] === "Automatic"];
scAutomaticMain = Import[scExport["Files"]["CalcHEPMain"], "Text"];
check["an ordinary generated validator keeps its time-seeded C++ default",
  !StringContainsQ[scAutomaticMain, "defaults.seed = UINT64_C("]];
scZeroSeedConfig = ConstructiveDiagrams`Private`spinasCalcHEPConfiguration[
  scIR, scModel, Join[scOptions, <|"CalcHEPSeed" -> 0|>]];
scMaxSeedConfig = ConstructiveDiagrams`Private`spinasCalcHEPConfiguration[
  scIR, scModel, Join[scOptions, <|"CalcHEPSeed" -> 18446744073709551615|>]];
check["explicit zero and maximum uint64 seeds are emitted as exact fixed choices",
  StringContainsQ[ConstructiveDiagrams`Private`spinasCalcHEPMain["calchep_stub", scZeroSeedConfig],
    "defaults.seed = UINT64_C(0);"] &&
  StringContainsQ[ConstructiveDiagrams`Private`spinasCalcHEPMain["calchep_stub", scMaxSeedConfig],
    "defaults.seed = UINT64_C(18446744073709551615);"]];
scCapturedCommand = {};
scAutomaticRun = Block[{ConstructiveDiagrams`Private`spinasProjectCommand = Function[arguments,
    scCapturedCommand = arguments;
    <|"ExitCode" -> 0, "StandardOutput" -> "Seed: 8675309\nAmplitude validation: PASS\n",
      "StandardError" -> ""|>]},
  ConstructiveDiagrams`Private`spinasRunCalcHEPValidation[scDirectory, "calchep_stub", scDisabled]];
check["the automatic execution helper records the concrete seed printed by the validator",
  AssociationQ[scAutomaticRun] && scAutomaticRun["Seed"] === 8675309 &&
    !MemberQ[scCapturedCommand, "--seed"]];
scExplicitRun = Block[{ConstructiveDiagrams`Private`spinasProjectCommand = Function[arguments,
    scCapturedCommand = arguments;
    <|"ExitCode" -> 0, "StandardOutput" -> "Seed: 0\nAmplitude validation: PASS\n",
      "StandardError" -> ""|>]},
  ConstructiveDiagrams`Private`spinasRunCalcHEPValidation[scDirectory, "calchep_stub", scZeroSeedConfig]];
check["an explicit zero seed is passed to the executable and retained in its returned report",
  AssociationQ[scExplicitRun] && scExplicitRun["Seed"] === 0 &&
    Take[scCapturedCommand, -2] === {"--seed", "0"}];
scMissingSeedRun = Block[{ConstructiveDiagrams`Private`spinasProjectCommand = Function[arguments,
    <|"ExitCode" -> 0, "StandardOutput" -> "Amplitude validation: PASS\n", "StandardError" -> ""|>]},
  ConstructiveDiagrams`Private`spinasRunCalcHEPValidation[scDirectory, "calchep_stub", scDisabled]];
scWrongSeedRun = Block[{ConstructiveDiagrams`Private`spinasProjectCommand = Function[arguments,
    <|"ExitCode" -> 0, "StandardOutput" -> "Seed: 7\nAmplitude validation: PASS\n", "StandardError" -> ""|>]},
  ConstructiveDiagrams`Private`spinasRunCalcHEPValidation[scDirectory, "calchep_stub", scZeroSeedConfig]];
check["a missing or incorrectly overridden replay seed cannot be reported as a successful run",
  FailureQ[scMissingSeedRun] && FailureQ[scWrongSeedRun]];

scSelectedOptions = Join[scOptions, <|"CalcHEPModel" -> Automatic, "FixedHelicities" -> {3 -> 2}|>];
scSelectedIR = ConstructiveDiagrams`Private`spinasFamilyIR[scFamily, scModel,
  KeyTake[scSelectedOptions, {"IncomingLegs", "ParameterRules", "FixedHelicities"}]];
scSelected = ConstructiveDiagrams`Private`spinasCalcHEPConfiguration[scSelectedIR, scModel, scSelectedOptions];
check["a selected photon helicity is never compared as an unpolarized CalcHEP amplitude",
  AssociationQ[scSelected] && scSelected["Status"] === "UNAVAILABLE"];
scFixedIR = ConstructiveDiagrams`Private`spinasProcessIR[First[scProcesses], Last[First[scFamily]], scModel,
  <|"ParameterRules" -> scRules|>];
scFixed = ConstructiveDiagrams`Private`spinasCalcHEPConfiguration[
  scFixedIR, scModel, Join[scOptions, <|"CalcHEPModel" -> Automatic|>]];
check["the legacy fixed-photon-helicity overload is explicitly unavailable for an unpolarized comparison",
  AssociationQ[scFixed] && scFixed["Status"] === "UNAVAILABLE"];
scColored = ConstructiveDiagrams`Private`spinasCalcHEPConfiguration[
  Join[scIR, <|"ColorStripped" -> True|>], scModel,
  Join[scOptions, <|"CalcHEPModel" -> Automatic|>]];
check["a stripped expression cannot be reported as matching CalcHEP's full color sum",
  scColored["Status"] === "UNAVAILABLE" && StringContainsQ[scColored["Reason"], "color"]];
scOtherModel = scModel;
scOtherModel[[1]] = Append[scOtherModel[[1]],
  {"additional scalar", "q", "q", 990001, 0, 0, 0, 1, "", "q", "q"}];
scOther = ConstructiveDiagrams`Private`spinasCalcHEPConfiguration[
  scIR, scOtherModel, Join[scOptions, <|"CalcHEPModel" -> Automatic|>]];
check["no theory silently selects a reference model from its external particle names",
  AssociationQ[scOther] && scOther["Status"] === "UNAVAILABLE" && scOther["ModelNumber"] === 0];
check["configuration and export never assign model globals or modify the supplied model",
  scModel === scModelBefore && {OwnValues[Global`Me], OwnValues[Global`EE]} === scGlobalsBefore];

scCompiler = scRun[{"c++", "--version"}];
scHeaders = FileExistsQ[FileNameJoin[{scSpinas, "include", "spinas.h"}]];
If[scSucceeded[scCompiler] && scHeaders,
  Do[
    scExecutable = scPath["validation_runtime_" <> scABI];
    scCompiled = scRun[Join[{"c++", "-std=c++11", "-O1"},
      If[scABI === "long_double", {"-DWITH_LONG_DOUBLE"}, {}],
      {"-I" <> FileNameJoin[{scSpinas, "include"}], "-I" <> FileNameJoin[{root, "source", "internal", "export"}],
       FileNameJoin[{root, "tests", "checks", "spinas-validation-runtime-tests.cpp"}],
       "-o", scExecutable}]];
    check["the shared sampler and comparison runtime compiles with the " <> scABI <> " ABI", scSucceeded[scCompiled]];
    If[scSucceeded[scCompiled],
      scRan = scRun[{scExecutable}];
      check["shared-point, replay and failure-report runtime checks pass with the " <> scABI <> " ABI",
        scSucceeded[scRan] && StringContainsQ[Lookup[scRan, "StandardOutput", ""],
          "SPINAS_VALIDATION_RUNTIME_CHECKS="] &&
        StringContainsQ[Lookup[scRan, "StandardOutput", ""], "FAILURES=0"]],
      Print["VALIDATION RUNTIME COMPILER: ", scCompiled]],
    {scABI, {"double", "long_double"}}],
  Print["SKIP: runtime compiler checks need C++ and the SPINAS headers."]];

scLibraries = Select[FileNameJoin[{scSpinas, #, "libspinas.a"}] & /@ {"build", "lib"}, FileExistsQ];
If[scSucceeded[scCompiler] && scHeaders && scLibraries =!= {},
  scOracleSource = scPath["unavailable_oracle.cpp"];
  Export[scOracleSource, StringRiffle[{
    "#include \"" <> scExport["Files"]["Header"] <> "\"", "#include <iostream>",
    "int main() {", "  hepcat_generated::calchep_stub process;",
    "  const auto report = process.validate_calchep();",
    "  if (report.passed || report.status != \"UNAVAILABLE\" || report.compared_points != 0) return 1;",
    "  std::cout << \"UNAVAILABLE_IS_NOT_PASS\\n\";", "  return 0;", "}"}, "\n"], "Text"];
  scPrecision = If[FileExistsQ[FileNameJoin[{scSpinas, "build", "CMakeCache.txt"}]] &&
      StringContainsQ[Import[FileNameJoin[{scSpinas, "build", "CMakeCache.txt"}], "Text"],
        "PRECISION_TYPE:STRING=LONG_DOUBLE"], {"-DWITH_LONG_DOUBLE"}, {}];
  scOracleExecutable = scPath["unavailable_oracle"];
  scOracleCompiled = scRun[Join[{"c++", "-std=c++11", "-O1"}, scPrecision,
    {"-I" <> FileNameJoin[{scSpinas, "include"}], scOracleSource,
      scExport["Files"]["Source"], FileNameJoin[{scExport["Directory"], "calchep-validation.cpp"}],
      First[scLibraries], "-o", scOracleExecutable}]];
  check["an unavailable validator compiles and links without any CalcHEP library", scSucceeded[scOracleCompiled]];
  If[scSucceeded[scOracleCompiled],
    scOracleRan = scRun[{scOracleExecutable}];
    check["calling the generated unavailable validator cannot produce a false pass",
      scSucceeded[scOracleRan] && StringContainsQ[Lookup[scOracleRan, "StandardOutput", ""], "UNAVAILABLE_IS_NOT_PASS"]],
    Print["UNAVAILABLE VALIDATOR COMPILER: ", scOracleCompiled]],
  Print["SKIP: generated stub compilation needs C++ and the built SPINAS library."]];

Print["SPINAS CALCHEP METADATA CHECKS: ", checks, "; FAILURES: ", Length[failures]];
If[failures === {}, DeleteDirectory[scDirectory, DeleteContents -> True];
  Print["HEPCAT_TEST_COMPLETED:spinas-calchep-tests.wl"],
  Print[failures]; Print["CALCHEP METADATA FILES: ", scDirectory]];
Quit[If[failures === {}, 0, 1]];
