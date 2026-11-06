(* Public MadGraph validation configuration and guards. No external benchmark runs. *)
smgRoot = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{smgRoot, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0;
smgChecks = 0; smgFailures = {};
SetAttributes[smgCheck, HoldRest];
smgCheck[name_, assertion_] := Module[{result},
  smgChecks++;
  result = TimeConstrained[Quiet[Check[assertion, $Failed]], 30, $TimedOut];
  If[!TrueQ[result], AppendTo[smgFailures, name]; Print["FAIL: ", name]]
];
smgRejectedQ[value_] := FailureQ[value] || value === $Failed;
smgTemporary = CreateDirectory[];
smgPath[name_] := FileNameJoin[{smgTemporary, name}];
smgModel = ReadModel[FileNameJoin[{smgRoot, "models"}], 3];
smgModelBefore = smgModel;
smgGlobalsBefore = {OwnValues[Global`Me], OwnValues[Global`EE]};
smgSpinas = Replace[Environment["HEPCAT_SPINAS_DIRECTORY"],
  $Failed | "" -> FileNameJoin[{ParentDirectory[smgRoot], "spinas"}]];
smgProcesses = Join[{"e", "E"}, #] & /@
  {{"A.+", "A.+"}, {"A.+", "A.-"}, {"A.-", "A.+"}, {"A.-", "A.-"}};
smgFamily = {#, LookupAmplitude[Thread[{#, Range[4]}], smgModel,
    False, {smgI, smgJ, smgK, smgL}]} & /@ smgProcesses;
smgExportCase[name_String, opts___] := WriteSPINASProcess[smgPath[name],
  {"e", "E"} -> {"A", "A"}, smgFamily, smgModel, opts,
  "ProcessName" -> "madgraph_stub", "SPINASDirectory" -> smgSpinas, "ReturnDetails" -> True];
smgMetadata[report_Association] := Import[FileNameJoin[{report["Directory"], "manifest.json"}], "RawJSON"]["MadGraphValidation"];

smgCheck["neither independent generator installation nor model is selected by default",
  And @@ ((# /. Options[WriteSPINASProcess]) === None & /@
    {"MadGraphDirectory", "MadGraphModel", "CalcHEPDirectory", "CalcHEPModel"})];
smgCheck["particle restrictions are explicit and retain the full chosen theory by default",
  ("MadGraphAllowedParticles" /. Options[WriteSPINASProcess]) === All &&
    ("CalcHEPAllowedParticles" /. Options[WriteSPINASProcess]) === All];
smgCheck["reference execution is optional while numerical parameter matching remains automatic",
  ("RunMadGraphValidation" /. Options[WriteSPINASProcess]) === False &&
    ("MadGraphParameterRules" /. Options[WriteSPINASProcess]) === Automatic &&
    ("MadGraphParameterMap" /. Options[WriteSPINASProcess]) === Automatic];
smgCheck["the normal comparison uses 1000 points and a runtime time-based seed",
  ("MadGraphPoints" /. Options[WriteSPINASProcess]) === 1000 &&
    ("MadGraphSeed" /. Options[WriteSPINASProcess]) === Automatic];
smgCheck["the Mathematica entry point provides help and sensible report defaults",
  StringQ[ValidateMadGraph::usage] && StringLength[ValidateMadGraph::usage] > 40 &&
    ("Output" /. Options[ValidateMadGraph]) === "Table" &&
    TrueQ[("RelativeTolerance" /. Options[ValidateMadGraph]) == 10^-7] &&
    ("AbsoluteTolerance" /. Options[ValidateMadGraph]) === 0 &&
    ("TimeConstraint" /. Options[ValidateMadGraph]) === 600];
smgCheck["a missing project is rejected before a numerical run",
  smgRejectedQ[ValidateMadGraph[smgPath["not-exported"]]]];
smgWrongMode = Quiet[ValidateMadGraph[smgPath["not-exported"], "Output" -> "invented"]];
smgCheck["an unknown report mode is rejected", smgRejectedQ[smgWrongMode]];

smgDefault = smgExportCase["no-reference-selected"];
smgCheck["ordinary SPINAS source export needs no selected external benchmark", AssociationQ[smgDefault]];
If[!AssociationQ[smgDefault], Print["SOURCE EXPORT FAILURE: ", InputForm[smgDefault]]; Exit[1]];
smgDefaultMetadata = smgMetadata[smgDefault];
smgCheck["even recognized QED data cannot silently select an installation, model or restriction",
  smgDefaultMetadata["Status"] === "UNAVAILABLE" &&
    smgDefaultMetadata["MadGraphDirectory"] === "" &&
    smgDefaultMetadata["MadGraphModel"] === "" &&
    smgDefaultMetadata["AllowedParticlePDGs"] === {}];
smgCheck["MadGraph export metadata states the private zero-width validation policy",
  KeyExistsQ[smgDefaultMetadata, "WidthParameterIndices"] &&
    smgDefaultMetadata["WidthParameterIndices"] === {} &&
    StringContainsQ[smgDefaultMetadata["WidthPolicy"], "private"]];
smgWidthParameter = ModelParameter["gamma", "madgraph-width-role-fixture", "Real"];
smgOtherParameter = ModelParameter["delta", "madgraph-width-role-fixture", "Real"];
smgWidthIR = <|"OriginalExpression" ->
    1/PropDen[Mom[Multiparticle[1, 3]], ModelParameter[smgModel, "Me"], smgWidthParameter],
  "ParameterSymbols" -> {ModelParameter[smgModel, "EE"], smgWidthParameter, smgOtherParameter}|>;
smgCheck["width roles use zero-based exported indices rather than parameter-name guesses",
  ConstructiveDiagrams`Private`spinasValidationWidthIndices[smgWidthIR, smgModel] === {1}];
smgCompoundWidthIR = Join[smgWidthIR, <|"OriginalExpression" ->
  1/PropDen[Mom[Multiparticle[1, 3]], ModelParameter[smgModel, "Me"],
    smgWidthParameter smgOtherParameter]|>];
smgCheck["compound widths are disabled by the process hook without zeroing unrelated scalar inputs",
  ConstructiveDiagrams`Private`spinasValidationWidthIndices[smgCompoundWidthIR, smgModel] === {}];

smgCheck["the missing reference setup has a helpful diagnostic",
  StringQ[smgDefaultMetadata["Reason"]] && StringContainsQ[smgDefaultMetadata["Reason"], "MadGraphModel"] &&
    StringContainsQ[smgDefaultMetadata["Reason"], "MadGraphDirectory"]];

smgExplicit = smgExportCase["explicit-QED-sector", "MadGraphDirectory" -> smgPath["missing-MadGraph"],
  "MadGraphModel" -> "sm-full", "MadGraphAllowedParticles" -> {11, 22}];
smgCheck["explicit reference model and particle sector are preserved",
  AssociationQ[smgExplicit] && With[{metadata = smgMetadata[smgExplicit]},
    metadata["MadGraphModel"] === "sm-full" && metadata["AllowedParticlePDGs"] === {11, 22} &&
      metadata["Status"] === "UNAVAILABLE"]];
smgFull = smgExportCase["full-explicit-theory", "MadGraphModel" -> "sm-full"];
smgCustom = smgExportCase["custom-explicit-theory", "MadGraphModel" -> "my_custom_ufo"];
smgCheck["explicit full and custom UFO models are not replaced by QED inference",
  AssociationQ[smgFull] && AssociationQ[smgCustom] &&
    smgMetadata[smgFull]["MadGraphModel"] === "sm-full" &&
    smgMetadata[smgFull]["AllowedParticlePDGs"] === {} &&
    smgMetadata[smgCustom]["MadGraphModel"] === "my_custom_ufo" &&
    smgMetadata[smgCustom]["AllowedParticlePDGs"] === {}];
smgAutomaticModel = smgExportCase["automatic-is-not-a-theory", "MadGraphModel" -> Automatic];
smgCheck["requesting Automatic for a model still cannot infer a theory",
  AssociationQ[smgAutomaticModel] && smgMetadata[smgAutomaticModel]["Status"] === "UNAVAILABLE" &&
    smgMetadata[smgAutomaticModel]["MadGraphModel"] === ""];

smgEnvironmentNames = {"MADGRAPH_ROOT", "MG5AMC_ROOT", "MG5AMCNLO_ROOT"};
smgEnvironmentBefore = Environment /@ smgEnvironmentNames;
SetEnvironment[Thread[smgEnvironmentNames -> ConstantArray["", Length[smgEnvironmentNames]]]];
smgAutomaticDirectory = smgExportCase["environment-only-discovery", "MadGraphDirectory" -> Automatic,
  "MadGraphModel" -> "sm-full"];
SetEnvironment[Thread[smgEnvironmentNames -> (smgEnvironmentBefore /. $Failed -> None)]];
smgCheck["explicit Automatic directory selection does not scan hard-coded local paths",
  AssociationQ[smgAutomaticDirectory] && smgMetadata[smgAutomaticDirectory]["MadGraphDirectory"] === "" &&
    smgMetadata[smgAutomaticDirectory]["Status"] === "UNAVAILABLE"];

smgMissingExternal = smgExportCase["omitted-external-particle", "MadGraphModel" -> "sm-full",
  "MadGraphAllowedParticles" -> {22}];
smgCheck["a particle filter omitting an external type cannot produce a passing benchmark",
  AssociationQ[smgMissingExternal] && smgMetadata[smgMissingExternal]["Status"] === "UNAVAILABLE" &&
    StringContainsQ[smgMetadata[smgMissingExternal]["Reason"], "external particle"]];
Do[
  smgBadFilterTarget = smgPath["invalid-filter-" <> ToString[filterCase[[1]]]];
  smgBadFilter = Quiet[WriteSPINASProcess[smgBadFilterTarget, {"e", "E"} -> {"A", "A"},
    smgFamily, smgModel, "SPINASDirectory" -> smgSpinas, "MadGraphAllowedParticles" -> filterCase[[2]]]];
  smgCheck["invalid PDG filter is rejected before writing project " <> ToString[filterCase[[1]]],
    smgRejectedQ[smgBadFilter] && !DirectoryQ[smgBadFilterTarget]],
  {filterCase, {{1, {}}, {2, {0}}, {3, {-11, 22}}, {4, {11, 11, 22}}, {5, {11, 22.0}}}}];

smgDirectory = smgExplicit["Directory"];
smgCheck["the optional backend includes a callable method and standalone executable",
  StringContainsQ[Import[FileNameJoin[{smgDirectory, "process.hpp"}], "Text"], "validate_madgraph"] &&
    StringContainsQ[Import[FileNameJoin[{smgDirectory, "CMakeLists.txt"}], "Text"], "madgraph_stub_validate_madgraph"]];
smgCheck["reference runtime and bridge sources are exported together",
  AllTrue[FileNameJoin[{smgDirectory, #}] & /@
    {"madgraph-validation.cpp", "madgraph-main.cpp", "madgraph-runtime.hpp", "madgraph-runtime.py"}, FileExistsQ]];
smgCheck["automatic seeds are not frozen into the generated executable",
  !StringContainsQ[Import[FileNameJoin[{smgDirectory, "madgraph-main.cpp"}], "Text"], "options.seed = UINT64_C("]];

Do[
  smgBadSeedTarget = smgPath["bad-seed-" <> ToString[seedCase[[1]]]];
  smgBadSeed = Quiet[WriteSPINASProcess[smgBadSeedTarget, {"e", "E"} -> {"A", "A"},
    smgFamily, smgModel, "SPINASDirectory" -> smgSpinas, "MadGraphSeed" -> seedCase[[2]]]];
  smgCheck["invalid seed is rejected before writing project " <> ToString[seedCase[[1]]],
    smgRejectedQ[smgBadSeed] && !DirectoryQ[smgBadSeedTarget]],
  {seedCase, {{1, -1}, {2, 18446744073709551616}, {3, 1/2}}}];
Do[
  smgSeed = smgExportCase["exact-seed-" <> ToString[seedCase[[1]]], "MadGraphSeed" -> seedCase[[2]]];
  smgCheck["explicit uint64 seed is retained exactly " <> ToString[seedCase[[1]]],
    AssociationQ[smgSeed] && smgMetadata[smgSeed]["Seed"] === seedCase[[2]] &&
      StringContainsQ[Import[FileNameJoin[{smgSeed["Directory"], "madgraph-main.cpp"}], "Text"],
        "options.seed = UINT64_C(" <> ToString[seedCase[[2]]] <> ");"]],
  {seedCase, {{1, 0}, {2, 18446744073709551615}}}];
smgBadPoints = Quiet[smgExportCase["bad-points", "MadGraphPoints" -> 0]];
smgCheck["a zero-point comparison cannot be an empty successful validation",
  smgRejectedQ[smgBadPoints] && !DirectoryQ[smgPath["bad-points"]]];

smgSelected = smgExportCase["selected-helicity", "FixedHelicities" -> {3 -> 2},
  "MadGraphModel" -> "sm-full", "MadGraphAllowedParticles" -> {11, 22}];
smgCheck["fixed photon helicities remain explicit and supported",
  AssociationQ[smgSelected] &&
    With[{manifest = Import[FileNameJoin[{smgSelected["Directory"], "manifest.json"}], "RawJSON"]},
      Length[manifest["HelicityConfigurations"]] === 2 &&
        AllTrue[manifest["HelicityConfigurations"], #[[3]] === 2 &]]];
smgCheck["source export preserves the supplied model and global parameters",
  smgModel === smgModelBefore && {OwnValues[Global`Me], OwnValues[Global`EE]} === smgGlobalsBefore];

smgPresentationReport = <|"Status" -> "PASS", "Rows" -> {
  <|"Helicities" -> {0, 0, -2, 2}, "Status" -> "PASS", "Points" -> 1000, "MaximumRelativeError" -> 0|>,
  <|"Helicities" -> {0, 0, 0, 0}, "Status" -> "PASS", "Points" -> 1000, "MaximumRelativeError" -> 0|>}|>;
smgPresentation = Normal[ConstructiveDiagrams`Private`spinasMadGraphValidationOutput[smgPresentationReport, "Table"]];
smgCheck["helicity labels are readable rather than hidden nested lists",
  smgPresentation[[1]]["Helicities"] === "3:-, 4:+" && smgPresentation[[2]]["Helicities"] === "No massless helicities"];
smgCheck["detailed output preserves the raw doubled-helicity vectors",
  ConstructiveDiagrams`Private`spinasMadGraphValidationOutput[smgPresentationReport, "Details"] === smgPresentationReport];

(* Exercise a real CMake build with an unrelated failing ALL target. *)
smgCMake = ConstructiveDiagrams`Private`spinasProjectCMakeExecutable[Automatic];
If[StringQ[smgCMake],
  smgBuildDirectory = CreateDirectory[smgPath["validation-target-only"]];
  Export[FileNameJoin[{smgBuildDirectory, "manifest.json"}],
    <|"Class" -> "validation_only", "MadGraphValidation" ->
      <|"Status" -> "AVAILABLE", "Reason" -> ""|>|>, "RawJSON"];
  Export[FileNameJoin[{smgBuildDirectory, "CMakeLists.txt"}], StringRiffle[{
    "cmake_minimum_required(VERSION 3.16)",
    "project(validation_target_only NONE)",
    "add_custom_target(validation_only_validate_madgraph",
    "  COMMAND \"${CMAKE_COMMAND}\" -E touch \"${CMAKE_BINARY_DIR}/comparison.built\")",
    "add_custom_target(unrelated_madevent ALL",
    "  COMMAND \"${CMAKE_COMMAND}\" -E touch \"${CMAKE_BINARY_DIR}/unrelated.started\"",
    "  COMMAND \"${CMAKE_COMMAND}\" -E false)"}, "\n"], "Text"];
  smgValidationOnly = Block[{
    ConstructiveDiagrams`Private`spinasRunMadGraphValidation =
      Function[{directory, name, config, supplied}, smgPresentationReport]},
    ValidateMadGraph[smgBuildDirectory, "CMakeExecutable" -> smgCMake,
      "Output" -> "Details"]];
  smgCheck["public validation builds its executable without unrelated event-generation targets",
    AssociationQ[smgValidationOnly] && smgValidationOnly["Status"] === "PASS" &&
      FileExistsQ[FileNameJoin[{smgBuildDirectory, "build", "comparison.built"}]] &&
      !FileExistsQ[FileNameJoin[{smgBuildDirectory, "build", "unrelated.started"}]]];
  smgOrdinaryBuild = ConstructiveDiagrams`Private`spinasProjectBuild[
    smgBuildDirectory, "validation_only", "", False, smgCMake];
  smgCheck["ordinary project builds retain all requested integration targets",
    FailureQ[smgOrdinaryBuild] &&
      FileExistsQ[FileNameJoin[{smgBuildDirectory, "build", "unrelated.started"}]]],
  Print["SKIP: validation target build needs an installed CMake executable."]];

smgCalls = {};
smgCombined = Block[{
  ConstructiveDiagrams`Private`spinasRunCalcHEPValidation = Function[{directory, name, config},
    AppendTo[smgCalls, "CalcHEP"];
    Failure["TestReferenceFailure", <|"CommandResult" -> <|
      "StandardOutput" -> "Amplitude validation: FAIL"|>|>]],
  ConstructiveDiagrams`Private`spinasRunMadGraphValidation = Function[{directory, name, config},
    AppendTo[smgCalls, "MadGraph"]; smgPresentationReport]},
  ConstructiveDiagrams`Private`spinasRunRequestedValidations[<|"Directory" -> smgDirectory|>,
    smgDirectory, "test", <|"Points" -> 1000|>, <||>,
    <|"RunCalcHEPValidation" -> True, "RunMadGraphValidation" -> True|>]];
smgCheck["a failed first reference never suppresses the second independent comparison",
  smgCalls === {"CalcHEP", "MadGraph"} && FailureQ[smgCombined] &&
    smgCombined[[2]]["FailedReferences"] === {"CalcHEP"} &&
    smgCombined[[2]]["ExportResult"]["MadGraphValidation"]["Status"] === "PASS"];
smgCalls = {};
smgMismatch = Block[{
  ConstructiveDiagrams`Private`spinasRunCalcHEPValidation = Function[{directory, name, config},
    AppendTo[smgCalls, "CalcHEP"]; <|"Status" -> "PASS"|>],
  ConstructiveDiagrams`Private`spinasRunMadGraphValidation = Function[{directory, name, config},
    AppendTo[smgCalls, "MadGraph"]; <|"Status" -> "FAIL", "Message" -> "Known mismatch", "Rows" -> {}|>]},
  ConstructiveDiagrams`Private`spinasRunRequestedValidations[<|"Directory" -> smgDirectory|>,
    smgDirectory, "test", <|"Points" -> 1000|>, <||>,
    <|"RunCalcHEPValidation" -> True, "RunMadGraphValidation" -> True|>]];
smgCheck["a completed but numerically failing MadGraph comparison remains a failed validation",
  smgCalls === {"CalcHEP", "MadGraph"} && FailureQ[smgMismatch] &&
    smgMismatch[[2]]["FailedReferences"] === {"MadGraph"} &&
    smgMismatch[[2]]["ExportResult"]["CalcHEPValidation"]["Status"] === "PASS"];
smgCheck["unrequested validation leaves an ordinary source export unchanged",
  ConstructiveDiagrams`Private`spinasRunRequestedValidations[<|"Directory" -> smgDirectory|>,
    smgDirectory, "test", <||>, <||>,
    <|"RunCalcHEPValidation" -> False, "RunMadGraphValidation" -> False|>] ===
      <|"Directory" -> smgDirectory|>];

(* Reference parameter matching must see the defining scalar arithmetic, not
   an unrelated coupling with a HEPCAT-only name. No external generator runs. *)
smgP[name_] := ModelParameter[name, "validator-model-test", "Real"];
smgDependencyModel = {{{"Scalar", "s", "s", 1, 0, smgP["mass"], 0, 1, "", "", ""}}, {},
  {{smgP["mass"], 2, ""}, {smgP["g"], 3, ""}},
  {{smgP["gSquared"], smgP["g"]^2, ""},
   {smgP["gFourth"], smgP["gSquared"]^2, ""}}};
smgExpand[value_, model_: smgDependencyModel, overrides_: Automatic] :=
  ConstructiveDiagrams`Private`modelExpandParameterDefinitions[value, model, overrides];
smgCheck["derived scalar parameters expand transitively without numerical substitution",
  smgExpand[smgP["gFourth"]] === smgP["g"]^4];
smgCheck["an explicitly overridden derived parameter remains an independent export input",
  smgExpand[smgP["gFourth"], smgDependencyModel, {"gSquared" -> 7}] === smgP["gSquared"]^2];
smgCyclicModel = ReplacePart[smgDependencyModel, {4, 1, 2} -> smgP["gFourth"]];
smgCheck["a relevant dependency cycle fails instead of looping or inventing a value",
  FailureQ[smgExpand[smgP["gFourth"], smgCyclicModel]] &&
    smgExpand[smgP["mass"], smgCyclicModel] === smgP["mass"]];
smgExternalFunctionModel = ReplacePart[smgDependencyModel,
  {4, 1, 2} -> ModelFunction["runningMass", "validator-model-test", {smgP["mass"]}]];
smgCheck["external model functions are retained rather than called by symbolic export",
  smgExpand[smgP["gSquared"], smgExternalFunctionModel] === smgP["gSquared"]];
smgDependencyIR = ConstructiveDiagrams`Private`spinasProcessIR[
  ConstantArray["s", 4], smgP["gFourth"], smgDependencyModel,
  <|"IncomingLegs" -> {1, 2}|>];
smgCheck["the actual SPINAS emitter uses primitive model inputs for derived scalar couplings",
  AssociationQ[smgDependencyIR] && Sort[smgDependencyIR["ParameterNames"]] === {"g", "mass"}];
smgSMModel = ReadModel[FileNameJoin[{smgRoot, "models"}], 1];
smgSMDefaults = ModelParameterRules[smgSMModel];
smgSMRoles = ConstructiveDiagrams`Private`spinasMadGraphMassRoles[smgSMModel];
smgCheck["reference mass matching uses model particle roles rather than hard-coded aliases",
  smgSMRoles["Mtp"] === {6} && smgSMRoles["Ml"] === {15} &&
    smgSMRoles["MW"] === {24} && smgSMRoles["MZ"] === {23} &&
    !KeyExistsQ[smgSMRoles, "EE"]];
smgSMOptions = Join[Association[Options[WriteSPINASProcess]], <|"MadGraphModel" -> "sm-full"|>];
smgSMRules = ConstructiveDiagrams`Private`spinasMadGraphSMInputRules[smgSMModel, smgSMOptions];
smgCheck["the selected SM reference obtains its EW input scheme from verified model constraints",
  Sort[Keys[smgSMRules]] === Sort[{"Gf", "MZ", "aEWM1"}] &&
    Abs[smgSMRules["Gf"] - N[ModelParameter[smgSMModel, "GF"] /. smgSMDefaults]] < 10^-15 &&
    Abs[smgSMRules["MZ"] - N[ModelParameter[smgSMModel, "MZ"] /. smgSMDefaults]] < 10^-10];
smgExplicitSMRules = ConstructiveDiagrams`Private`spinasMadGraphSMInputRules[smgSMModel,
  Join[smgSMOptions, <|"MadGraphParameterRules" -> {"gf" -> 0.01, "lamWS" -> 0}|>]];
smgCheck["explicit reference inputs remain authoritative without duplicate case aliases",
  smgExplicitSMRules["gf"] === 0.01 && !KeyExistsQ[smgExplicitSMRules, "Gf"] &&
    smgExplicitSMRules["lamWS"] === 0];
smgCheck["a custom UFO never acquires guessed standard-model inputs",
  ConstructiveDiagrams`Private`spinasMadGraphSMInputRules[smgSMModel,
    Join[smgSMOptions, <|"MadGraphModel" -> "custom-ufo"|>]] === <||>];
smgUndeclared = Complement[DeleteDuplicates[Cases[smgSMModel[[2]], _ModelParameter, Infinity]],
  First /@ Join[smgSMModel[[3]], smgSMModel[[4]]]];
smgCheck["all SM vertices including the top Yukawa use declared model parameters", smgUndeclared === {}];

DeleteDirectory[smgTemporary, DeleteContents -> True];
Print["MadGraph public surface: ", smgChecks - Length[smgFailures], " / ", smgChecks, " checks passed."];
If[smgFailures =!= {}, Print["Failures: ", smgFailures]; Exit[1]];
Print["HEPCAT_TEST_COMPLETED:spinas-madgraph-tests.wl"];
Exit[0];
