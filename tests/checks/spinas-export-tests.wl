(* Complete SPINAS projects must compile, link and preserve amplitude conventions. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0;
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++; If[!TrueQ[value],
  AppendTo[failures, name]; Print["FAIL: ", name]]);
seDirectory = CreateDirectory[];
seSpinas = Replace[Environment["HEPCAT_SPINAS_DIRECTORY"],
  $Failed | "" -> FileNameJoin[{ParentDirectory[root], "spinas"}]];
seGlobalValuesBefore = {OwnValues[Global`Me], OwnValues[Global`EE]};
seModel = ReadModel[FileNameJoin[{root, "models"}], 3];
seMass = ModelParameter[seModel, "Me"];
seCharge = ModelParameter[seModel, "EE"];
seRules = {"Me" -> 1, "EE" -> 1/3};
seA[i_, j_] := SpinorChain[Spinor["Helicity", "Angle", i], Spinor["Helicity", "Angle", j]];
seS[i_, j_] := SpinorChain[Spinor["Helicity", "Square", i], Spinor["Helicity", "Square", j]];
seF[type_, i_, j_] := SpinorChain[Spinor["Spin", type, i], Spinor["Spin", type, j]];
seD[labels_List] := PropDen[Mom[Multiparticle @@ labels], seMass];
seLookup[process_List] := LookupAmplitude[Thread[{process, Range[Length[process]]}],
  seModel, False, {seI, seJ, seK, seL}];
seFourPP = seLookup[{"e", "E", "A.+", "A.+"}];
sePath[name_] := FileNameJoin[{seDirectory, name}];
seRun[command_List] := Quiet[Check[RunProcess[command], $Failed]];
seFailed[report_] := !AssociationQ[report] || Lookup[report, "ExitCode", -1] =!= 0;
seExport[name_String, process_List, amplitude_, model_:Automatic, opts___] :=
  WriteSPINASProcess[sePath[name], process, amplitude,
    If[model === Automatic, seModel, model], opts,
    "ProcessName" -> name, "ParameterRules" -> seRules,
    "SPINASDirectory" -> seSpinas, "ReturnDetails" -> True];
seFiles = {"process.hpp", "process.cpp", "main.cpp", "spinas-runtime.hpp",
  "CMakeLists.txt", "README.md", "manifest.json"};

check["the public writer is loaded, documented and protected",
  StringQ[WriteSPINASProcess::usage] && Length[DownValues[WriteSPINASProcess]] > 0 &&
    MemberQ[Attributes[WriteSPINASProcess], Protected]];
check["exporting is separate from opt-in compilation and execution",
  ("Build" /. Options[WriteSPINASProcess]) === False &&
    ("RunExamples" /. Options[WriteSPINASProcess]) === False &&
    ("Overwrite" /. Options[WriteSPINASProcess]) === False &&
    ("ReturnDetails" /. Options[WriteSPINASProcess]) === False];
sePlainDirectory = WriteSPINASProcess[sePath["plain project with spaces"],
  {"e", "E", "A.+", "A.+"}, seFourPP, seModel,
  "ParameterRules" -> seRules, "SPINASDirectory" -> seSpinas];
check["ordinary output is only the generated project directory",
  StringQ[sePlainDirectory] && DirectoryQ[sePlainDirectory]];
check["a complete portable source project is written",
  AllTrue[seFiles, FileExistsQ[FileNameJoin[{sePlainDirectory, #}]] &]];
sePlainHeader = Import[FileNameJoin[{sePlainDirectory, "process.hpp"}], "Text"];
sePlainSource = Import[FileNameJoin[{sePlainDirectory, "process.cpp"}], "Text"];
check["generated code uses SPINAS without unresolved Wolfram notation",
  StringContainsQ[sePlainHeader <> sePlainSource, "spinas::particle"] &&
    !StringContainsQ[sePlainHeader <> sePlainSource,
      {"SpinorChain[", "ModelParameter[", "PropDen[", "Power["}]];
check["legacy expression formatting remains available",
  SPINAS[seA[3, 4]] === "a34a.v()" &&
    DeclareSPINASSpinorProducts[2 seA[3, 4]] === "//Spinor Products\n sproduct a34a;"];
check["writer keeps model parameters inert instead of assigning user globals",
  Head[seMass] === ModelParameter && Head[seCharge] === ModelParameter &&
    {OwnValues[Global`Me], OwnValues[Global`EE]} === seGlobalValuesBefore];
seBeforeFiles = AssociationMap[BinaryReadList[FileNameJoin[{sePlainDirectory, #}], "Byte"] &, seFiles];
seRefusedOverwrite = WriteSPINASProcess[sePlainDirectory,
  {"e", "E", "A.+", "A.+"}, 2 seFourPP, seModel,
  "ParameterRules" -> seRules, "SPINASDirectory" -> seSpinas];
check["existing generated files are not overwritten by default",
  FailureQ[seRefusedOverwrite] && seBeforeFiles ===
    AssociationMap[BinaryReadList[FileNameJoin[{sePlainDirectory, #}], "Byte"] &, seFiles]];

seBadInputs = {
  {{"e", "unknown", "A.+", "A.+"}, seFourPP, {}},
  {{"e", "E", "A.+", "A.+"}, seUnknown[seFourPP], {}},
  {{"e", "E", "A.+", "A.+"}, Infinity, {}},
  {{"e", "E", "A.+", "A.+"}, seFourPP xFactor[1, 2], {}},
  {{"e", "E", "A.+", "A.+"}, seFourPP SpinorChain[
    Spinor["Xi", "Angle", 100], Spinor["Helicity", "Angle", 3]], {}},
  {{"e", "E", "A.+", "A.+"}, seFourPP ModelParameter["EE", "foreign-model", "Real"], {}},
  {{"e", "E", "A.+", "A.+"}, seFourPP, {"IncomingLegs" -> {1, 1}}},
  {{"e", "E", "A.+", "A.+"}, seFourPP, {"IncomingLegs" -> {1}}},
  {{"e", "E", "A.+", "A.+"}, seFourPP, {"IncomingLegs" -> {1, 2, 3}}},
  {{"e", "E", "A.+", "A.+"}, seFourPP, {"ProcessName" -> "bad/name"}},
  {{"e", "E", "A.+", "A.+"}, seFourPP, {"ProcessName" -> "class"}},
  {{"e", "E", "A.+", "A.+"}, seFourPP, {"ProcessName" -> "amp"}},
  {{"e", "E", "A.+", "A.+"}, seFourPP, {"ProcessName" -> "Parameters"}}
};
Do[
  seRejectedDirectory = sePath["rejected_" <> ToString[index]];
  seRejected = WriteSPINASProcess[seRejectedDirectory,
    seBadInputs[[index, 1]], seBadInputs[[index, 2]], seModel,
    Sequence @@ seBadInputs[[index, 3]], "ParameterRules" -> seRules,
    "SPINASDirectory" -> seSpinas];
  check["invalid input fails before writing a partial project " <> ToString[index],
    FailureQ[seRejected] && !DirectoryQ[seRejectedDirectory]],
  {index, Length[seBadInputs]}];

(* Non-SM fixture species exercise model-driven spin, mass and scalar handling. *)
seGeneric = seModel;
seGeneric[[1]] = Join[seGeneric[[1]], {
  {"fixture scalar", "h", "h", 100, 0, 0, 0, 1, 1, "h", "h"},
  {"fixture vector", "V", "V", 101, 2, seMass, 0, 1, 1, "V", "V"}}];
seComplex = ModelParameter["gComplex", "spinas-export-tests", "Complex"];
seComplexModel = seModel;
seComplexModel[[3]] = Append[seComplexModel[[3]], {seComplex, I/7}];
seWidthExpression = (1/3 + seComplex) Sqrt[2] Pi (MomProd[1, 3] + seMass^2/5) *
  SpinorChain[Spinor["Spin", "Angle", 1], Mom[Multiparticle[1, 3]], Mom[2],
    Spinor["Spin", "Angle", 3]] *
  SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]] /
  PropDen[Mom[Multiparticle[1, 3]], seMass, 1/11];
seVectorExpression = seF["Angle", 1, 2] seF["Square", 1, 2];

(* Slot allocation must retain a complete multilinear tensor without expanding
   factored coefficients or conflating reserved and symbolic free indices. *)
seVectorMixedSlots = SpinorChain[
  Spinor["Spin", "Angle", 1, VectorSpinIndex[1, 1]],
  Spinor["Spin", "Angle", 2, VectorSpinIndex[2, 1]]] SpinorChain[
  Spinor["Spin", "Square", 1, seFreeSlot],
  Spinor["Spin", "Square", 2, VectorSpinIndex[2, 2]]];
seTensorIR[value_] := ConstructiveDiagrams`Private`spinasProcessIR[
  {"V", "V", "h", "h"}, value, seGeneric, <|"ParameterRules" -> seRules|>];
seMixedSlotIR = seTensorIR[seVectorMixedSlots];
check["symbolic spin indices fill unused reserved tensor slots",
  AssociationQ[seMixedSlotIR] &&
    StringCount[StringJoin[seMixedSlotIR["ChainStatements"]], "spinor_spins[0][0]"] === 1 &&
    StringCount[StringJoin[seMixedSlotIR["ChainStatements"]], "spinor_spins[0][1]"] === 1];
seRepeatedSlot = seVectorMixedSlots /. seFreeSlot -> VectorSpinIndex[1, 1];
check["a repeated massive tensor slot is rejected",
  FailureQ[seTensorIR[seRepeatedSlot]]];
check["powers count repeated explicit tensor indices",
  FailureQ[seTensorIR[SpinorChain[
    Spinor["Spin", "Angle", 1, seFreeA], Spinor["Spin", "Angle", 2, seFreeB]]^2]]];
check["incomplete tensor slots are rejected separately in additive terms",
  FailureQ[seTensorIR[seVectorMixedSlots + seRepeatedSlot]]];
check["mixed implicit and explicit slots on one leg fail safely",
  FailureQ[seTensorIR[seVectorMixedSlots /.
    Spinor["Spin", "Square", 1, seFreeSlot] -> Spinor["Spin", "Square", 1]]]];
sePowerIR = seTensorIR[seF["Angle", 1, 2]^2];
check["implicit spin-bearing powers allocate every tensor slot",
  AssociationQ[sePowerIR] &&
    StringContainsQ[StringJoin[sePowerIR["ChainStatements"]], "spinor_spins[0][1]"] &&
    StringContainsQ[StringJoin[sePowerIR["ChainStatements"]], "spinor_spins[1][1]"]];
seFactoredIR = seTensorIR[seVectorExpression (1 + seMass^2 + MomProd[1, 2])];
check["implicit tensor allocation preserves factored scalar sums",
  AssociationQ[seFactoredIR] && Head[seFactoredIR["IndexedExpression"]] === Times &&
    Count[List @@ seFactoredIR["IndexedExpression"], _Plus] === 1];
seBadTensorDirectory = sePath["rejected_tensor"];
check["invalid spin tensors fail before writing project files",
  FailureQ[WriteSPINASProcess[seBadTensorDirectory, {"V", "V", "h", "h"},
    seRepeatedSlot, seGeneric, "SPINASDirectory" -> seSpinas]] && !DirectoryQ[seBadTensorDirectory]];
seFourMassRules = {Mass[1] -> seMass, Mass[2] -> seMass, Mass[3] -> 0, Mass[4] -> 0};
seNestedReport = <|"Amplitude" -> <|"Amplitude" -> {seFourMassRules, seFourPP}|>|>;
seReportProject = WriteSPINASProcess[sePath["nested_report"],
  {"e", "E", "A.+", "A.+"}, seNestedReport, seModel,
  "ParameterRules" -> seRules, "SPINASDirectory" -> seSpinas];
check["nested reports normalize their amplitude pairs before export",
  StringQ[seReportProject] &&
    Import[FileNameJoin[{seReportProject, "process.cpp"}], "Text"] === sePlainSource];
sePreparedReportIR = ConstructiveDiagrams`Private`spinasProcessIR[
  {"e", "E", "A.+", "A.+"}, <|"PreparedAmplitudes" ->
    {{seFourMassRules, seFourPP/2}, {seFourMassRules, seFourPP/2}}|>, seModel,
  <|"ParameterRules" -> seRules|>];
check["prepared reports normalize every mass-expression pair",
  AssociationQ[sePreparedReportIR] && sePreparedReportIR["OriginalExpression"] === seFourPP];
seFiveParts = {"e", "E", "A.+", "A.+", "A.+"};
seFiveH = seLookup[seFiveParts];
seFiveS2 = 2 Sqrt[2] seCharge^3 seMass seF["Angle", 1, 2] Total[
  Function[order, With[{a = order[[1]], b = order[[2]], c = order[[3]]},
    seS[a, b] SpinorChain[Spinor["Helicity", "Angle", a], Mom[2],
      Spinor["Helicity", "Square", c]] /
      (seA[a, b] seA[a, c] seD[{1, a}] seD[{2, c}])]] /@ Permutations[{3, 4, 5}]];
sePhotons = Range[3, 6];
seK[i_] := SpinorChain[Spinor["Helicity", "Square", i], Mom[1], Mom[2],
  Spinor["Helicity", "Square", i]];
seSix = Total[Flatten[Table[With[{other = Complement[sePhotons, pair]}, {
  -4 seCharge^4 seMass^3 seF["Angle", 1, 2] (seS @@ pair)^2 (seS @@ other)^2 /
    ((Times @@ (seD[{1, #}] & /@ other)) (Times @@ (seD[{2, #}] & /@ pair))
      seD[Prepend[other, 1]]),
  4 seCharge^4 seMass seF["Angle", 1, 2] (seS @@ other)^2 (Times @@ (seK /@ pair)) /
    ((Times @@ (seD[{1, #}] & /@ sePhotons)) (Times @@ (seD[{2, #}] & /@ pair)))}],
  {pair, Subsets[sePhotons, {2}]}], 1]];

seCompiler = seRun[{"c++", "--version"}];
seLibraries = Select[FileNameJoin[{seSpinas, #, "libspinas.a"}] & /@ {"build", "lib"}, FileExistsQ];
seNativeAvailable = !seFailed[seCompiler] && seLibraries =!= {} &&
  FileExistsQ[FileNameJoin[{seSpinas, "include", "spinas.h"}]] &&
  FileExistsQ[FileNameJoin[{seSpinas, "SM", "eeAA.cpp"}]];
seCppMain[class_String, kind_Integer, h3_:2, h4_:2] := StringRiffle[{
  "#include \"process.hpp\"", "#include \"spinas-export-reference.hpp\"",
  "int main() { return hepcat_export_reference::run_process<hepcat_generated::" <>
    class <> ">(" <> StringRiffle[ToString /@ {kind, h3, h4}, ","] <> "); }"}, "\n"] <> "\n";
seOracle[report_Association, name_String, kind_Integer, h3_:2, h4_:2] := Module[
  {directory = report["Directory"], source, executable, command, compiled, ran,
   precision = {}, cache},
  source = FileNameJoin[{directory, "native_oracle.cpp"}];
  executable = FileNameJoin[{directory, "native_oracle"}];
  Export[source, seCppMain[name, kind, h3, h4], "Text"];
  cache = FileNameJoin[{seSpinas, "build", "CMakeCache.txt"}];
  If[FileExistsQ[cache] && StringContainsQ[Import[cache, "Text"], "PRECISION_TYPE:STRING=LONG_DOUBLE"],
    precision = {"-DWITH_LONG_DOUBLE"}];
  command = Join[{"c++", "-std=c++11", "-O1"}, precision,
    {"-I" <> directory, "-I" <> FileNameJoin[{root, "tests", "checks"}],
     "-I" <> FileNameJoin[{seSpinas, "include"}], "-I" <> seSpinas,
     source, report["Files"]["Source"], FileNameJoin[{seSpinas, "SM", "eeAA.cpp"}],
     First[seLibraries], "-o", executable}];
  compiled = seRun[command];
  If[seFailed[compiled], Print["ORACLE COMPILATION ", name, ": ", compiled]; Return[$Failed]];
  ran = seRun[{executable}];
  If[seFailed[ran], Print["ORACLE EXECUTION ", name, ": ", ran]];
  ran
];
seBuildAndCompare[name_String, process_List, amplitude_, model_, kind_Integer, h3_:2, h4_:2,
    extra_List:{}] := Module[{report, oracle, valid},
  Print["BUILDING ", name];
  report = Block[{Global`Me = 91, Global`EE = 149},
    seExport[name, process, amplitude, model, Sequence @@ extra,
      "Build" -> True, "RunExamples" -> True]];
  valid = AssociationQ[report] && Lookup[Lookup[report, "Build", <||>], "Status", None] === "BUILT" &&
    Lookup[Lookup[report, "Examples", <||>], "Status", None] === "RAN" &&
    Lookup[Lookup[report, "Examples", <||>], "ExitCode", -1] === 0;
  check["generated project compiles, links and runs conserved example points: " <> name, valid];
  If[!valid, Print["EXPORT DETAILS ", name, ": ", report]; Return[report]];
  oracle = seOracle[report, name, kind, h3, h4];
  check[If[kind === 1 && h3 =!= h4,
    "the specified mixed-helicity model sign is preserved relative to native SPINAS: ",
    "generated amplitudes agree with independent native SPINAS values: "] <> name,
    !seFailed[oracle] && StringContainsQ[oracle["StandardOutput"], "NUMERICAL_COMPARISONS="]];
  report
];

If[seNativeAvailable,
  seReports = {};
  Print["CONVENTION: models/amps3.mdl has -2 EE^2 for mixed 4-point helicities; native SM/eeAA.cpp has +2 e^2. The exporter preserves the supplied sign. Numerical tests use this analytically specified relative minus, not a fitted phase."];
  Do[
    seProcess = Join[{"e", "E"}, helicities];
    seName = "ee_" <> StringJoin[If[# === "A.+", "p", "m"] & /@ helicities];
    AppendTo[seReports, seBuildAndCompare[seName, seProcess, seLookup[seProcess], seModel,
      1, If[helicities[[1]] === "A.+", 2, -2], If[helicities[[2]] === "A.+", 2, -2]]],
    {helicities, Tuples[{"A.+", "A.-"}, 2]}];
  Do[
    seProcess = Join[{"e", "E"}, helicities];
    seName = "ee_native_" <> StringJoin[If[# === "A.+", "p", "m"] & /@ helicities];
    AppendTo[seReports, seBuildAndCompare[seName, seProcess, -seLookup[seProcess], seModel,
      8, If[helicities[[1]] === "A.+", 2, -2], If[helicities[[2]] === "A.+", 2, -2]]],
    {helicities, {{"A.+", "A.-"}, {"A.-", "A.+"}}}];
  AppendTo[seReports, seBuildAndCompare["ee_three_photons_h", seFiveParts, seFiveH, seModel, 2]];
  AppendTo[seReports, seBuildAndCompare["ee_three_photons_s2", seFiveParts, seFiveS2, seModel, 2]];
  AppendTo[seReports, seBuildAndCompare["ee_four_photons", Join[{"e", "E"}, ConstantArray["A.+", 4]],
    seSix, seModel, 3]];
  AppendTo[seReports, seBuildAndCompare["composite_width", {"e", "E", "E", "e"},
    seWidthExpression, seComplexModel, 4]];
  AppendTo[seReports, seBuildAndCompare["massive_vectors", {"V", "V", "h", "h"},
    seVectorExpression, seGeneric, 5]];
  AppendTo[seReports, seBuildAndCompare["massive_vectors_mixed_slots", {"V", "V", "h", "h"},
    seVectorMixedSlots, seGeneric, 5]];
  AppendTo[seReports, seBuildAndCompare["reordered_incoming", ConstantArray["h", 4],
    MomProd[1, 2], seGeneric, 6, 2, 2, {"IncomingLegs" -> {1, 3}}]];
  AppendTo[seReports, seBuildAndCompare["chiral_trace", ConstantArray["h", 4],
    SpinorTrace[Mom[1], Mom[Multiparticle[1, 3]]], seGeneric, 7]];
  seZero = seExport["zero_amplitude", ConstantArray["h", 4], 0, seGeneric,
    "Build" -> True, "RunExamples" -> True];
  check["a genuinely zero amplitude still exports a runnable process",
    AssociationQ[seZero] && seZero["Build"]["Status"] === "BUILT" &&
      seZero["Examples"]["Status"] === "RAN" && seZero["Examples"]["ExitCode"] === 0],
  Print["SKIP: native SPINAS compile/run checks require a C++ compiler, headers, static library and SM/eeAA.cpp."]];

Print["CHECKS: ", checks, "; FAILURES: ", Length[failures]];
If[failures =!= {}, Print[failures]; Print["EXPORT TEST FILES: ", seDirectory]];
If[failures === {}, DeleteDirectory[seDirectory, DeleteContents -> True];
  Print["HEPCAT_TEST_COMPLETED:spinas-export-tests.wl"]];
Quit[If[failures === {}, 0, 1]];
