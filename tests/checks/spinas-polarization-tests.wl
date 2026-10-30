(* Explicit physical roles, selected helicities and coherent complex amplitudes. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0;
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);
spDirectory = CreateDirectory[];
spSpinas = Replace[Environment["HEPCAT_SPINAS_DIRECTORY"],
  $Failed | "" -> FileNameJoin[{ParentDirectory[root], "spinas"}]];
spModel = ReadModel[FileNameJoin[{root, "models"}], 3];
spModelBefore = spModel;
spRules = {"Me" -> 1, "EE" -> 1/3};
spLookup[process_List] := LookupAmplitude[Thread[{process, Range[Length[process]]}],
  spModel, False, {spI, spJ, spK, spL}];
spEntries[processes_List] := {#, spLookup[#]} & /@ processes;
spFour = spEntries[Join[{"e", "E"}, #] & /@
  {{"A.+", "A.+"}, {"A.+", "A.-"}, {"A.-", "A.-"}}];
spIncoming = spEntries[Join[#, {"e", "E"}] & /@
  {{"A.+", "A.+"}, {"A.+", "A.-"}, {"A.-", "A.-"}}];
spCompton = spEntries[Join[{"e", #[[1]], "E"}, {#[[2]]}] & /@ Tuples[{"A.+", "A.-"}, 2]];
spPath[name_] := FileNameJoin[{spDirectory, name}];
spOptions = {"ParameterRules" -> spRules, "SPINASDirectory" -> spSpinas, "ReturnDetails" -> True};
spFlat[name_, records_, opts___] := WriteSPINASProcess[spPath[name], records, spModel,
  opts, Sequence @@ spOptions, "ProcessName" -> name];
spPhysical[name_, process_, records_, opts___] := WriteSPINASProcess[spPath[name], process,
  records, spModel, opts, Sequence @@ spOptions, "ProcessName" -> name];
spConfigurations[report_] := If[AssociationQ[report], Lookup[report, "HelicityConfigurations", {}], {}];
spSelected[report_, count_, average_, divisor_] := AssociationQ[report] &&
  Length[spConfigurations[report]] === count && report["InitialSpinAverage"] === average &&
  report["FinalStateSymmetryDivisor"] === divisor;

spAll = spPhysical["polarized_all", {"e", "E"} -> {"A", "A"}, spFour];
check["a physical incoming/outgoing process accepts all helicity representatives",
  spSelected[spAll, 4, 4, 2]];
If[!AssociationQ[spAll], Print["PHYSICAL EXPORT DETAILS: ", spAll]; Quit[1]];
spManifest = Import[spAll["Files"]["Manifest"], "RawJSON"];
check["physical process roles and selected-helicity metadata are retained",
  KeyExistsQ[spAll, "PhysicalProcess"] && KeyExistsQ[spAll, "FixedHelicities"] &&
    KeyExistsQ[spManifest, "PhysicalProcess"] && KeyExistsQ[spManifest, "FixedHelicities"]];
spFlatAll = spFlat["polarized_flat_all", spFour];
spOutPlus = spFlat["polarized_out_plus", spFour, "FixedHelicities" -> {3 -> 2}];
spOutMinus = spPhysical["polarized_out_minus", {"e", "E"} -> {"A.-", "A"}, spFour];
check["an outgoing selected helicity restricts only the requested labeled leg",
  spSelected[spOutPlus, 2, 4, 2] && AllTrue[spConfigurations[spOutPlus], #[[3]] === 2 &] &&
    spSelected[spOutMinus, 2, 4, 2] && AllTrue[spConfigurations[spOutMinus], #[[3]] === -2 &]];
spPermuted = spPhysical["polarized_permuted", {"e", "E"} -> {"A.-", "A.+"}, Take[spFour, {2}]];
check["a selected branch may use an identical-photon permutation of a supplied formula",
  spSelected[spPermuted, 1, 4, 2] && spPermuted["GeneratedBranchCount"] === 1 &&
    spConfigurations[spPermuted] === {{0, 0, -2, 2}}];
spIncomingAll = spPhysical["polarized_in_all", {"A", "A"} -> {"E", "e"}, spIncoming];
spIncomingFlat = spFlat["polarized_in_flat", spIncoming];
spInPlus = spPhysical["polarized_in_plus", {"A.+", "A"} -> {"E", "e"}, spIncoming];
spInMinus = spFlat["polarized_in_minus", spIncoming, "FixedHelicities" -> {1 -> -2}];
spInBoth = spPhysical["polarized_in_both", {"A.+", "A.-"} -> {"E", "e"}, Take[spIncoming, {2}]];
check["one fixed incoming helicity removes exactly one factor of two from averaging",
  spSelected[spIncomingAll, 4, 4, 1] && spSelected[spInPlus, 2, 2, 1] && spSelected[spInMinus, 2, 2, 1]];
check["fixing both incoming helicities leaves no helicity average",
  spSelected[spInBoth, 1, 1, 1]];
spComptonFlat = spFlat["polarized_compton_flat", spCompton];
spComptonBoth = spPhysical["polarized_compton_both", {"e", "A.+"} -> {"e", "A.-"},
  Take[spCompton, {2}]];
check["incoming and outgoing helicities can both be selected without exchanging their roles",
  spSelected[spComptonBoth, 1, 2, 1] && spConfigurations[spComptonBoth] === {{0, 2, 0, -2}}];

spInvalid = {
  {"massive leg", {"e", "E"} -> {"A", "A"}, spFour, {"FixedHelicities" -> {1 -> 1}}},
  {"bad sign", {"e", "E"} -> {"A", "A"}, spFour, {"FixedHelicities" -> {3 -> 0}}},
  {"duplicate selection", {"e", "E"} -> {"A", "A"}, spFour, {"FixedHelicities" -> {3 -> 2, 3 -> -2}}},
  {"unknown leg", {"e", "E"} -> {"A", "A"}, spFour, {"FixedHelicities" -> {5 -> 2}}},
  {"role conflict", {"e", "E"} -> {"A", "A"}, spFour, {"IncomingLegs" -> {1, 3}}},
  {"helicity conflict", {"e", "E"} -> {"A.+", "A"}, spFour, {"FixedHelicities" -> {3 -> -2}}},
  {"wrong charged outgoing particle", {"e", "A"} -> {"E", "A"}, spCompton, {}},
  {"unknown physical particle", {"e", "E"} -> {"unknown", "A"}, spFour, {}},
  {"wrong process length", {"e", "E"} -> {"A", "A", "A"}, spFour, {}},
  {"three incoming legs", {"e", "E", "A"} -> {"A"}, spFour, {}}
};
Do[spRejectedPath = spPath["invalid_" <> ToString[index]];
  spRejected = Quiet[WriteSPINASProcess[spRejectedPath,
    spInvalid[[index, 2]], spInvalid[[index, 3]], spModel,
    Sequence @@ spInvalid[[index, 4]], Sequence @@ spOptions]];
  check["invalid physical role or polarization fails before writing: " <> spInvalid[[index, 1]],
    (FailureQ[spRejected] || spRejected === $Failed) && !DirectoryQ[spRejectedPath]],
  {index, Length[spInvalid]}];
spMissing = spPhysical["missing_selected_sector", {"e", "E"} -> {"A.+", "A"}, Take[spFour, 1]];
check["coverage errors name only missing selected helicity sectors",
  FailureQ[spMissing] && Lookup[spMissing[[2]], "MissingHelicities", {}] === {{0, 0, 2, -2}} &&
    !DirectoryQ[spPath["missing_selected_sector"]]];

spReports = {spAll, spFlatAll, spOutPlus, spOutMinus, spPermuted, spIncomingAll,
  spIncomingFlat, spInPlus, spInMinus, spInBoth, spComptonFlat, spComptonBoth};
spRun[command_List] := Quiet[Check[RunProcess[command], <|"ExitCode" -> -1|>]];
spLibraries = Select[FileNameJoin[{spSpinas, #, "libspinas.a"}] & /@ {"build", "lib"}, FileExistsQ];
spNative = Lookup[spRun[{"c++", "--version"}], "ExitCode", -1] === 0 && spLibraries =!= {} &&
  FileExistsQ[FileNameJoin[{spSpinas, "SM", "eeAA.cpp"}]];
If[spNative && AllTrue[spReports, AssociationQ],
  spIncludes = ("#include \"" <> #["Files"]["Header"] <> "\"") & /@ spReports;
  spMain = StringRiffle[Join[spIncludes, {"#include \"spinas-polarization-reference.hpp\"",
    "int main() { try { using namespace hepcat_generated; using namespace hepcat_polarization_reference;",
    "if (!compare<polarized_all,polarized_flat_all>(4,4,2,true)) return 1;",
    "if (!compare<polarized_out_plus,polarized_flat_all>(2,4,2,true)) return 1;",
    "if (!compare<polarized_out_minus,polarized_flat_all>(2,4,2,true)) return 1;",
    "if (!compare<polarized_permuted,polarized_flat_all>(1,4,2,true)) return 1;",
    "if (!compare<polarized_in_all,polarized_in_flat>(4,4,1)) return 1;",
    "if (!compare<polarized_in_plus,polarized_in_flat>(2,2,1)) return 1;",
    "if (!compare<polarized_in_minus,polarized_in_flat>(2,2,1)) return 1;",
    "if (!compare<polarized_in_both,polarized_in_flat>(1,1,1)) return 1;",
    "if (!compare<polarized_compton_both,polarized_compton_flat>(1,2,1)) return 1;",
    "if (!recombine<polarized_all,polarized_out_plus,polarized_out_minus>(false)) return 1;",
    "if (!recombine<polarized_in_all,polarized_in_plus,polarized_in_minus>(true)) return 1;",
    "std::cout << \"POLARIZATION_COMPLEX_AND_SUM_CHECKS=PASS\\n\"; return 0;",
    "} catch (const std::exception& e) { std::cerr << e.what() << '\\n'; return 2; } }"}], "\n"];
  spSource = spPath["polarization.cpp"]; Export[spSource, spMain, "Text"];
  spPrecision = {};
  spCache = FileNameJoin[{spSpinas, "build", "CMakeCache.txt"}];
  If[FileExistsQ[spCache] && StringContainsQ[Import[spCache, "Text"], "PRECISION_TYPE:STRING=LONG_DOUBLE"],
    spPrecision = {"-DWITH_LONG_DOUBLE"}];
  spExecutable = spPath["polarization_test"];
  spCommand = Join[{"c++", "-std=c++11", "-O1"}, spPrecision,
    {"-I" <> FileNameJoin[{root, "tests", "checks"}], "-I" <> FileNameJoin[{spSpinas, "include"}],
      "-I" <> spSpinas, spSource}, (#["Files"]["Source"] & /@ spReports),
    {FileNameJoin[{spSpinas, "SM", "eeAA.cpp"}], First[spLibraries], "-o", spExecutable}];
  spCompiled = spRun[spCommand];
  check["all physical-role and polarized C++ classes compile together",
    Lookup[spCompiled, "ExitCode", -1] === 0];
  If[Lookup[spCompiled, "ExitCode", -1] === 0,
    spRan = spRun[{spExecutable}];
    check["complex amplitudes, allowed default spins, restricted sums and helicity recombination agree at three phase-space points",
      Lookup[spRan, "ExitCode", -1] === 0 && StringContainsQ[Lookup[spRan, "StandardOutput", ""],
        "POLARIZATION_COMPLEX_AND_SUM_CHECKS=PASS"]];
    If[Lookup[spRan, "ExitCode", -1] =!= 0, Print["POLARIZATION RUN: ", spRan]],
    Print["POLARIZATION COMPILE: ", spCompiled]],
  Print["SKIP: complex polarization checks need C++, built SPINAS and successful source exports."]];
check["physical-role and polarization export do not change model data", spModel === spModelBefore];
Print["SPINAS POLARIZATION CHECKS: ", checks, "; FAILURES: ", Length[failures]];
If[failures === {}, DeleteDirectory[spDirectory, DeleteContents -> True];
  Print["HEPCAT_TEST_COMPLETED:spinas-polarization-tests.wl"],
  Print[failures]; Print["POLARIZATION TEST FILES: ", spDirectory]];
Quit[If[failures === {}, 0, 1]];
