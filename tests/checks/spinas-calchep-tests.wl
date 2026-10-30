(* Metadata, unavailable-backend behavior and shared-point validation need no CalcHEP installation. *)
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
scModel = ReadModel[FileNameJoin[{root, "models"}], 3];
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
check["CalcHEP validation has opt-in execution and reproducible ordinary defaults",
  ("CalcHEPDirectory" /. Options[WriteSPINASProcess]) === Automatic &&
    ("CalcHEPModel" /. Options[WriteSPINASProcess]) === Automatic &&
    ("RunCalcHEPValidation" /. Options[WriteSPINASProcess]) === False &&
    ("CalcHEPPoints" /. Options[WriteSPINASProcess]) === 1000 &&
    ("CalcHEPSeed" /. Options[WriteSPINASProcess]) === 12345];
scIR = ConstructiveDiagrams`Private`spinasFamilyIR[scFamily, scModel,
  KeyTake[scOptions, {"IncomingLegs", "ParameterRules", "FixedHelicities"}]];
scDisabled = ConstructiveDiagrams`Private`spinasCalcHEPConfiguration[scIR, scModel, scOptions];
check["an explicitly disabled CalcHEP counterpart is unavailable rather than a passing comparison",
  AssociationQ[scDisabled] && scDisabled["Status"] === "UNAVAILABLE" &&
    StringQ[Lookup[scDisabled, "Reason", None]] && StringLength[scDisabled["Reason"]] > 0];
check["opposite photon helicities remain one self-conjugate physical species",
  scDisabled["PhysicalPDGs"] === {11, -11, 22, 22}];
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
check["an unfamiliar theory cannot silently select pure QED from its external particle names alone",
  AssociationQ[scOther] && scOther["Status"] === "UNAVAILABLE"];
check["configuration and export never assign model globals or modify the supplied model",
  scModel === scModelBefore && {OwnValues[Global`Me], OwnValues[Global`EE]} === scGlobalsBefore];

scCompiler = scRun[{"c++", "--version"}];
scHeaders = FileExistsQ[FileNameJoin[{scSpinas, "include", "spinas.h"}]];
If[scSucceeded[scCompiler] && scHeaders,
  Do[
    scExecutable = scPath["validation_runtime_" <> scABI];
    scCompiled = scRun[Join[{"c++", "-std=c++11", "-O1"},
      If[scABI === "long_double", {"-DWITH_LONG_DOUBLE"}, {}],
      {"-I" <> FileNameJoin[{scSpinas, "include"}], "-I" <> FileNameJoin[{root, "source"}],
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
