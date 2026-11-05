(* Compare the two public export modes at the complex-amplitude level. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root, "source", "HEPCAT.wl"}]];
$HistoryLength = 0;
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);
soDirectory = CreateDirectory[];
soModel = ReadModel[FileNameJoin[{root, "models"}], 3];
soMass = ModelParameter[soModel, "Me"]; soCharge = ModelParameter[soModel, "EE"];
soSpinas = Replace[Environment["HEPCAT_SPINAS_DIRECTORY"],
  $Failed | "" -> FileNameJoin[{ParentDirectory[root], "spinas"}]];
soRules = {"Me" -> 1, "EE" -> 1/3};
soLookup[particles_List] := LookupAmplitude[MapIndexed[{#1, First[#2]} &, particles],
  soModel, False, {soI, soJ, soK, soL}];
soPath[name_String] := FileNameJoin[{soDirectory, name}];
soArray[items_List] := "{{" <> StringRiffle[items, ","] <> "}}";
soRecords = {};
soExportPair[label_String, process_, expression_, model_, default_:False, classStem_:Automatic] := Module[
  {reports, name, report, manifest, mode, options},
  reports = Table[
    name = If[classStem === Automatic, label, classStem] <>
      If[mode === "Readable", "_readable", If[classStem === Automatic, "_optimized", ""]];
    Print["Exporting ", label, " as ", mode];
    options = If[TrueQ[default] && mode === "Optimized", {}, {"CodeStyle" -> mode}];
    report = If[process === "Family",
      WriteSPINASProcess[soPath[name], expression, model, Sequence @@ options,
        "ProcessName" -> name, "ParameterRules" -> soRules, "SPINASDirectory" -> soSpinas,
        "CalcHEPDirectory" -> None, "CalcHEPModel" -> None, "ReturnDetails" -> True],
      WriteSPINASProcess[soPath[name], process, expression, model, Sequence @@ options,
        "ProcessName" -> name, "ParameterRules" -> soRules, "SPINASDirectory" -> soSpinas,
        "CalcHEPDirectory" -> None, "CalcHEPModel" -> None, "ReturnDetails" -> True]];
    check[label <> " exports " <> mode, AssociationQ[report]];
    If[AssociationQ[report],
      manifest = Import[report["Files"]["Manifest"], "RawJSON"];
      check[label <> " records the selected public style " <> mode,
        report["CodeStyle"] === mode && manifest["CodeStyle"] === mode];
      check[label <> " separates numerical layout from style " <> mode,
        MemberQ[{"NativeSPINAS", "CanonicalDispatch"}, manifest["CodeLayout"]]];
      If[mode === "Optimized" && StringStartsQ[label, "optimized_qed"],
        check[label <> " uses compiled product-component tables",
          Lookup[Lookup[manifest, "Optimization", <||>], "PointProductTables", 0] > 0]];
      If[mode === "Optimized" && label === "optimized_qed4",
        check["repeated six-point arithmetic has reusable point/spin factors",
          Total[Lookup[manifest["Optimization"], {"PointFactors", "SpinFactors"}, 0]] > 0]];
      AppendTo[soRecords, <|"Label" -> label, "Style" -> mode,
        "Class" -> name, "Report" -> report, "Manifest" -> manifest|>]],
    {mode, {"Readable", "Optimized"}}];
  reports];

check["optimized code is the public default", ("CodeStyle" /. Options[WriteSPINASProcess]) === "Optimized"];
soFour = soLookup[{"e", "E", "A.+", "A.+"}];
(* A bounded CSE failure must retain a complete direct-table evaluator, rather
   than leaving references to half-created arithmetic temporaries. Padding the
   otherwise valid expression stresses size admission without changing it. *)
soGuardIR = ConstructiveDiagrams`Private`spinasProcessIR[
  {"e", "E", "A.+", "A.+"}, soFour, soModel,
  <|"IncomingLegs" -> {1, 2}, "ParameterRules" -> soRules|>];
soGuardIR = ConstructiveDiagrams`Private`spinasMergeScalarCaches[soGuardIR, {"guard_fixture"}];
soGuardPlan = ConstructiveDiagrams`Private`spinasNativeCachePlan[soGuardIR, {"guard_fixture"}];
soGuardPlan["Branches"] = Map[Join[#, <|"ExpressionCpp" ->
  StringRepeat[" ", 260000] <> #["ExpressionCpp"]|>] &, soGuardPlan["Branches"]];
soGuardOptimized = ConstructiveDiagrams`Private`spinasOptimizeNativePlan[soGuardIR, soGuardPlan];
check["CSE size limits retain complete native product tables",
  TrueQ[soGuardOptimized["OptimizationReport"]["CSESkipped"]] &&
    soGuardOptimized["OptimizationReport"]["PointProductTables"] > 0 &&
    soGuardOptimized["PointUpdates"] =!= {}];
check["bounded fallback contains no incomplete arithmetic temporaries",
  FreeQ[Lookup[soGuardOptimized["Branches"], "LocalStatements", {}], _String] &&
    !AnyTrue[Lookup[soGuardOptimized["Branches"], "ExpressionCpp"],
      StringContainsQ[#, "point_factor_" | "spin_factor_"] &]];
soInvalid = Quiet[WriteSPINASProcess[soPath["invalid_style"],
  {"e", "E", "A.+", "A.+"}, soFour, soModel, "CodeStyle" -> "FastGuess",
  "SPINASDirectory" -> soSpinas]];
check["an unsupported style fails before writing any project",
  (FailureQ[soInvalid] || soInvalid === $Failed) && !DirectoryQ[soPath["invalid_style"]]];

Do[
  soEntries = Table[soParticles = Join[{"e", "E"}, ConstantArray["A.+", photons - negative],
      ConstantArray["A.-", negative]];
    {soParticles, soLookup[soParticles]}, {negative, 0, photons}];
  soExportPair["optimized_qed" <> ToString[photons], "Family", soEntries, soModel, photons === 2],
  {photons, 2, 4}];

soGeneric = soModel;
soComplex = ModelParameter["momentum_factor_1", "optimized-export-fixture", "Complex"];
soWidth = ModelParameter["width", "optimized-export-fixture", "Real"];
soInternalMass = ModelParameter["internal_mass", "optimized-export-fixture", "Real"];
soGeneric[[1]] = Join[soGeneric[[1]], {
  {"optimized massive scalar", "r", "r", 997, 0, soMass, 0, 1, 1, "r", "r"},
  {"optimized massless scalar", "h", "h", 998, 0, 0, 0, 1, 1, "h", "h"},
  {"optimized massive vector", "V", "V", 999, 2, soMass, 0, 1, 1, "V", "V"}}];
soGeneric[[3]] = Join[soGeneric[[3]], {{soComplex, I/7}, {soWidth, 1/30}, {soInternalMass, 7/5}}];
soConstant = Pi + Sqrt[2] + Exp[I/3];
soScalar = soConstant (soComplex + soCharge soMass)^2 Sqrt[soComplex] (1 + MomProd[1, 3]) /
  PropDen[Mom[Multiparticle[1, 3]], soMass, soWidth];
soExportPair["optimized_scalar", ConstantArray["r", 4], soScalar, soGeneric];
soTensor = soComplex * SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] *
  SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] /
  PropDen[Mom[Multiparticle[1, 3]], soMass, soWidth];
soExportPair["optimized_tensor", {"V", "V", "h", "h"}, soTensor, soGeneric];
soLong = SpinorChain @@ Join[{Spinor["Spin", "Angle", 1]},
  Mom /@ {1, 2, 1, 2, 1, 2, 1, 3}, {Spinor["Spin", "Angle", 2]}];
soComposite = SpinorChain[Spinor["Spin", "Angle", 1], Mom[Multiparticle[1, 3]],
  Mom[2], Spinor["Spin", "Angle", 2]];
soFallback = (soComplex soComposite + soCharge^2 soLong/soMass^6) /
  PropDen[Mom[Multiparticle[1, 3]], soMass, soWidth];
soExportPair["optimized_fallback", {"e", "E", "h", "h"}, soFallback, soGeneric];
soTrace = soComplex SpinorTrace[Mom[1], Mom[Multiparticle[1, 3]], Mom[2], Mom[4]] /
  PropDen[Mom[Multiparticle[1, 3]], soMass, soWidth];
soExportPair["optimized_trace", ConstantArray["r", 4], soTrace, soGeneric];
(* An internal-only model mass is not forced real merely by its default or
   declaration: the existing parameter setter permits complex values. *)
soInternal = soComplex (1 + MomProd[1, 3]) / PropDen[Mom[Multiparticle[1, 3]], soInternalMass] +
  (soComplex + soCharge) MomProd[1, 2] / PropDen[Mom[Multiparticle[1, 3]], 7/5];
soExportPair["optimized_internal", ConstantArray["r", 4], soInternal, soGeneric];
soTinyMass = 1/PropDen[Mom[Multiparticle[1, 3]], soMass];
soExportPair["optimized_tiny_mass", ConstantArray["r", 4], soTinyMass, soGeneric];

(* Optimizer-generated names must not collide with either a class name or
   model parameter aliases. Keep helicity-dependent dispatch in this fixture. *)
soReservedKernel = ModelParameter["amp_unchecked", "optimized-name-fixture", "Complex"];
soReservedDispatch = ModelParameter["helicity_key", "optimized-name-fixture", "Complex"];
soNameModel = soGeneric;
soNameModel[[3]] = Join[soNameModel[[3]], {{soReservedKernel, 2/7 + I/5},
  {soReservedDispatch, 3/11 - I/13}}];
soNameExpression = (soReservedKernel + soReservedDispatch MomProd[1, 3]) soFour;
soExportPair["optimized_product_name", {"e", "E", "A.+", "A.+"},
  soNameExpression, soNameModel, False, "point_product_1"];
soExportPair["optimized_factor_name", {"e", "E", "A.+", "A.+"},
  soNameExpression, soNameModel, False, "point_factor_1"];
soKernelName = WriteSPINASProcess[soPath["reserved_kernel"], {"e", "E", "A.+", "A.+"},
  soFour, soModel, "ProcessName" -> "amp_unchecked", "SPINASDirectory" -> soSpinas];
check["an internal kernel name is rejected before project output",
  FailureQ[soKernelName] && !DirectoryQ[soPath["reserved_kernel"]]];
check["generated builds default to optimized Release without overriding an explicit choice",
  StringContainsQ[Import[soRecords[[1]]["Report"]["Files"]["CMake"], "Text"],
    "if(NOT CMAKE_CONFIGURATION_TYPES AND NOT CMAKE_BUILD_TYPE)\n  set(CMAKE_BUILD_TYPE Release"]];

(* A class name overlapping p1 needs the established canonical layout. *)
soCanonical = WriteSPINASProcess[soPath["canonical_collision"], ConstantArray["r", 4], soScalar,
  soGeneric, "ProcessName" -> "p1", "CodeStyle" -> "Optimized", "ParameterRules" -> soRules,
  "SPINASDirectory" -> soSpinas, "CalcHEPDirectory" -> None, "CalcHEPModel" -> None,
  "ReturnDetails" -> True];
check["a canonical layout retains the optimized public request",
  AssociationQ[soCanonical] && soCanonical["CodeStyle"] === "Optimized" &&
    soCanonical["CodeLayout"] === "CanonicalDispatch"];
If[Length[soRecords] =!= 22 || !AssociationQ[soCanonical],
  Print["OPTIMIZED EXPORT SETUP FAILED: ", soDirectory]; Print[failures]; Quit[1]];

soReadables = Select[soRecords, #["Style"] === "Readable" &];
soOptimized = Select[soRecords, #["Style"] === "Optimized" &];
soAllReports = Join[Lookup[soRecords, "Report"], {soCanonical}];
soAllInventories = Join[soRecords, {
  <|"Class" -> "p1", "Manifest" -> Import[soCanonical["Files"]["Manifest"], "RawJSON"]|>}];
soIncludes = ("#include " <> ToString[#["Files"]["Header"], InputForm]) & /@ soAllReports;
soParameterInventories = Table[
  "template<> inline std::map<std::string,unsigned> parameter_indices<hepcat_generated::" <>
    entry["Class"] <> ">() { return {" <>
    StringRiffle[MapIndexed[
      "{" <> ToString[#1, InputForm] <> "," <> ToString[First[#2] - 1] <> "}" &,
      entry["Manifest"]["ParameterOrder"]], ","] <> "}; }", {entry, soAllInventories}];
Export[FileNameJoin[{soDirectory, "optimized-projects.hpp"}],
  StringRiffle[Join[{"#pragma once", "#include <map>", "#include <string>"},
    soIncludes, {"template<class P> std::map<std::string,unsigned> parameter_indices();"},
    soParameterInventories], "\n"], "Text"];
soCalls = Table[
  With[{a = soReadables[[i]], b = soOptimized[[i]]},
    "  compare<hepcat_generated::" <> a["Class"] <> ", hepcat_generated::" <> b["Class"] <>
      ">(" <> ToString[a["Label"], InputForm] <> ", " <>
      soArray[ToString /@ a["Manifest"]["Spin2"]] <> ");"],
  {i, Length[soReadables]}];
Export[FileNameJoin[{soDirectory, "optimized-comparisons.inc"}],
  StringRiffle[Join[{"void run_style_comparisons() {"}, soCalls, {"}"}], "\n"], "Text"];
soExecutable = FileNameJoin[{soDirectory, "optimized_comparison"}];
soLibrary = SelectFirst[FileNameJoin[{soSpinas, #, "libspinas.a"}] & /@ {"build", "lib"},
  FileExistsQ, Missing["Library"]];
soCompiler = "c++";
soCompilerReady = Quiet[Check[RunProcess[{soCompiler, "--version"}]["ExitCode"] === 0, False]];
If[!StringQ[soLibrary] || !TrueQ[soCompilerReady],
  Print["SKIP: optimized backend numerical tests require C++ and SPINAS."]; Quit[1]];
soDefinitions = If[StringContainsQ[Import[soAllReports[[1]]["Files"]["CMake"], "Text"], "PUBLIC WITH_LONG_DOUBLE"],
  {"-DWITH_LONG_DOUBLE"}, {}];
soCompiled = RunProcess[Join[{soCompiler, "-std=c++11", "-O3"}, soDefinitions,
  {"-I" <> FileNameJoin[{soSpinas, "include"}], "-I" <> soDirectory,
    FileNameJoin[{root, "tests", "checks", "spinas-optimized-oracle.cpp"}]},
  Lookup[Lookup[soAllReports, "Files"], "Source"], {soLibrary, "-o", soExecutable}]];
check["both styles compile for QED and independent generic fixtures", soCompiled["ExitCode"] === 0];
If[soCompiled["ExitCode"] =!= 0, Print[soCompiled],
  soRun = RunProcess[{soExecutable}];
  check["all complex amplitudes, sums, updates and copies agree", soRun["ExitCode"] === 0 &&
    StringContainsQ[soRun["StandardOutput"], "OPTIMIZED_COMPLEX_COMPARISONS="]];
  Print[soRun["StandardOutput"]]; If[soRun["ExitCode"] =!= 0, Print[soRun]]];
Print["SPINAS optimized checks: ", checks, "; failures: ", Length[failures]];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:spinas-optimized-tests.wl"], Print[failures]];
Print["Optimized test projects: ", soDirectory];
Quit[If[failures === {}, 0, 1]];
