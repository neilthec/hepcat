(* ::Package:: *)

(* ::Section::Closed:: *)
(*Independent MadGraph Validation*)

Begin["ConstructiveDiagrams`"];
ValidateMadGraph::usage = "ValidateMadGraph[directory] independently checks an exported SPINAS process against MadGraph at shared random physical momenta, with a separate result for every exported massless-helicity combination. ValidateMadGraph[particles, amplitude, model] checks one fixed-helicity all-ingoing expression. ValidateMadGraph[incoming -> outgoing, amplitudeEntries, model] first exports and builds a private SPINAS project. Massive spins are summed, not compared in potentially different spin bases. MadGraphPoints defaults to 1000 per helicity and MadGraphSeed -> Automatic chooses and reports a fresh replayable seed. Output -> Table returns a readable per-helicity table; Summary gives result counts and Details retains the complete numerical report and failed momenta. Specify MadGraphDirectory and MadGraphModel explicitly. MadGraphAllowedParticles -> All retains the selected theory; a list of positive absolute PDG codes, such as {11,22}, restricts intermediate particle types and includes their antiparticles. No theory or particle restriction is inferred from the HEPCAT model. MadGraphParameterMap and MadGraphParameterRules handle different parameter names or reference inputs. Squared amplitudes are compared, so overall phases do not affect the result. Colorless tree processes are supported; unsupported or unavailable comparisons are never reported as passing. This is numerical validation, not an analytic proof.";
WriteSPINASProcess::usage = WriteSPINASProcess::usage <> " Generated projects also expose validate_madgraph(options) and a process_validate_madgraph executable for independent fixed-massless-helicity validation. Specify MadGraphDirectory and MadGraphModel to select the independent installation and UFO theory. MadGraphAllowedParticles -> All retains all model particles; {11,22}, for example, restricts to electrons, positrons and photons using absolute PDG codes. MadGraphParameterMap relates differently named HEPCAT and UFO parameters; MadGraphParameterRules supplies explicit UFO external input values. RunMadGraphValidation -> True builds and runs MadGraphPoints -> 1000 shared points per helicity with MadGraphSeed -> Automatic. Fixed incoming or outgoing helicities are supported, and massive external spins are summed. Numerical mismatches remain FAIL, including losses of floating-point precision. When both reference checks are requested, both run independently even if the first fails. A failed requested check returns Failure with ExportResult retaining both reports and the generated files. Ordinary SPINAS execution does not require MadGraph.";
WriteSPINASProcess::usage = WriteSPINASProcess::usage <> " The same explicit MadGraph configuration also writes a process_madevent launcher. It prepares a private MadEvent process on first use and supplies the exported HEPCAT/SPINAS matrix element to MadEvent integration, unweighting and event output. Build -> True compiles the launcher; --prepare builds its private process without running events. Run and parameter cards adjust cuts, beams, masses and couplings. Fixed massless helicities and summed massive spins retain their specified averaging and symmetry factors. Current numerical coverage is colorless leading-order tree amplitudes; coherent decay sewing, NLO matching and a MadGraph massive-spin basis are not provided. The independent MadGraph validator continues to use MadGraph's own amplitude.";
Options[WriteSPINASProcess] = Join[Options[WriteSPINASProcess], {
  "MadGraphDirectory" -> None, "MadGraphModel" -> None, "MadGraphAllowedParticles" -> All,
  "MadGraphParameterRules" -> Automatic, "MadGraphParameterMap" -> Automatic,
  "RunMadGraphValidation" -> False, "MadGraphPoints" -> 1000, "MadGraphSeed" -> Automatic}];
Options[ValidateMadGraph] = Join[Options[WriteSPINASProcess], {
  "Output" -> "Table", "RelativeTolerance" -> 10^-7, "AbsoluteTolerance" -> 0,
  "MinimumEnergy" -> Automatic, "MaximumEnergy" -> Automatic, "TimeConstraint" -> 600}];
ValidateMadGraph::args = "Expected a generated SPINAS project directory, or incoming particle names -> outgoing particle names, helicity amplitude entries and a model, with valid validation options.";

Begin["ConstructiveDiagrams`Private`"];

(* ::Subsection::Closed:: *)
(*Installation and Model Selection*)

spinasMadGraphRoot[option_] := Module[{candidates},
  If[option === None, Return[""]];
  candidates = If[StringQ[option], {ExpandFileName[option]},
    Select[Environment /@ {"MADGRAPH_ROOT", "MG5AMC_ROOT", "MG5AMCNLO_ROOT"},
      StringQ[#] && StringLength[#] > 0 &]];
  SelectFirst[DeleteDuplicates[candidates],
    DirectoryQ[#] && FileExistsQ[FileNameJoin[{#, "bin", "mg5_aMC"}]] &, ""]
];
spinasMadGraphPython[root_String] := Module[{firstLine = "", candidates, probe, path},
  If[root === "", Return[""]];
  firstLine = Quiet[Check[First[StringSplit[Import[FileNameJoin[{root, "bin", "mg5_aMC"}], "Text"], "\n"]], ""]];
  path = Replace[Environment["PATH"], Except[_String] -> ""];
  candidates = Join[
    If[StringStartsQ[firstLine, "#!/" ] && !StringContainsQ[firstLine, "/env "],
      {StringTrim[StringDrop[firstLine, 2]]}, {}],
    (FileNameJoin[{#, "python3"}] & /@ Select[StringSplit[path, If[$OperatingSystem === "Windows", ";", ":"]], # =!= "" &]),
    Flatten[Table[FileNameJoin[{prefix, "opt", "python@3." <> ToString[version], "bin", "python3." <> ToString[version]}],
      {prefix, {"/opt/homebrew", "/usr/local"}}, {version, {14, 13, 12, 11, 10}}]],
    {"/usr/local/bin/python3", "/usr/bin/python3"}];
  SelectFirst[DeleteDuplicates[candidates], Function[path,
    FileExistsQ[path] && !DirectoryQ[path] &&
      (probe = spinasProjectCommand[{path, "-c", "import sys;sys.exit(0 if sys.version_info >= (3,10) else 1)"}];
       Lookup[probe, "ExitCode", -1] === 0)], ""]
];
spinasMadGraphRulesQ[rules_, values_] := rules === Automatic ||
  (MatchQ[rules, {___Rule}] && DuplicateFreeQ[First /@ rules] &&
    AllTrue[rules, StringQ[First[#]] && StringLength[StringTrim[First[#]]] > 0 && values[Last[#]] &]);
spinasMadGraphOptionsQ[options_Association] :=
  (MemberQ[{Automatic, None}, options["MadGraphDirectory"]] || StringQ[options["MadGraphDirectory"]]) &&
  (MemberQ[{Automatic, None}, options["MadGraphModel"]] ||
    StringQ[options["MadGraphModel"]] && StringTrim[options["MadGraphModel"]] =!= "") &&
  (options["MadGraphAllowedParticles"] === All ||
    MatchQ[options["MadGraphAllowedParticles"], {__Integer}] &&
      AllTrue[options["MadGraphAllowedParticles"], 0 < # <= 2147483647 &] &&
      DuplicateFreeQ[options["MadGraphAllowedParticles"]]) &&
  spinasMadGraphRulesQ[options["MadGraphParameterRules"],
    NumberQ[#] && TrueQ[Im[#] == 0] && FreeQ[#, Indeterminate | DirectedInfinity] &] &&
  spinasMadGraphRulesQ[options["MadGraphParameterMap"], StringQ[#] && StringTrim[#] =!= "" &] &&
  MemberQ[{True, False}, options["RunMadGraphValidation"]] &&
  IntegerQ[options["MadGraphPoints"]] && 0 < options["MadGraphPoints"] <= 2147483647 &&
  (options["MadGraphSeed"] === Automatic || IntegerQ[options["MadGraphSeed"]] &&
    0 <= options["MadGraphSeed"] <= 18446744073709551615);

spinasMadGraphConfiguration[ir_Association, data_List, supplied_Association, physical_Association] := Module[
  {options = Join[Association[Options[WriteSPINASProcess]], supplied], root, python, model,
   reasons = {}, normalization, family = TrueQ[Lookup[ir, "Family", False]], helicities, allowed},
  root = spinasMadGraphRoot[options["MadGraphDirectory"]];
  python = spinasMadGraphPython[root];
  model = options["MadGraphModel"];
  allowed = Replace[options["MadGraphAllowedParticles"], All -> {}];
  If[StringQ[model] && DirectoryQ[model], model = ExpandFileName[model]];
  If[MemberQ[{Automatic, None}, model], AppendTo[reasons, "Specify the equivalent MadGraph UFO model with MadGraphModel."]];
  If[root === "", AppendTo[reasons, "Specify a usable MadGraph installation with MadGraphDirectory. Automatic only consults MADGRAPH_ROOT, MG5AMC_ROOT or MG5AMCNLO_ROOT."]];
  If[allowed =!= {} && Complement[Abs[physical["PhysicalPDGs"]], allowed] =!= {},
    AppendTo[reasons, "MadGraphAllowedParticles must include every external particle type (absolute PDG codes)."]];
  If[root =!= "" && python === "", AppendTo[reasons, "A supported Python 3.10 or later interpreter was not found."]];
  If[TrueQ[ir["ColorStripped"]], AppendTo[reasons, "A color-stripped expression is not a complete color-summed amplitude."]];
  If[AnyTrue[ir["ParameterDefaults"], !TrueQ[Im[#] == 0] &],
    AppendTo[reasons, "MadGraph validation currently requires real model inputs."]];
  If[AnyTrue[ir["Spin2"], # > 2 &], AppendTo[reasons, "MadGraph validation currently supports external spins through one."]];
  normalization = If[family, ir["SymmetryFactor"]/ir["InitialSpinAverage"],
    1/(Times @@ (If[TrueQ[ir["MassValues"][[#]] == 0], 1, ir["Spin2"][[#]] + 1] & /@ ir["IncomingLegs"]))];
  helicities = If[family, ir["HelicityConfigurations"], {ir["Helicities"]}];
  <|"Status" -> If[reasons === {}, "AVAILABLE", "UNAVAILABLE"], "Reason" -> StringRiffle[reasons, " "],
    "MadGraphDirectory" -> root, "PythonExecutable" -> python,
    "MadGraphModel" -> If[StringQ[model], model, ""],
    "ParameterRules" -> Association[Replace[options["MadGraphParameterRules"], Automatic -> {}]],
    "ParameterMap" -> Association[Replace[options["MadGraphParameterMap"], Automatic -> {}]],
    "PhysicalPDGs" -> physical["PhysicalPDGs"],
    "AllowedParticlePDGs" -> allowed, "LegOrder" -> physical["LegOrder"],
    "Spin2" -> ir["Spin2"], "HelicityConfigurations" -> helicities,
    "ParameterNames" -> ir["ParameterNames"], "Normalization" -> normalization,
    "Points" -> options["MadGraphPoints"], "Seed" -> Replace[options["MadGraphSeed"], Automatic -> "Automatic"]|>
];

(* ::Subsection::Closed:: *)
(*Readable C++ Validation Project*)

spinasMadGraphHeader[header_String] := StringReplace[header, {
  "#include \"spinas-validation.hpp\"" -> "#include \"spinas-validation.hpp\"\n#include \"madgraph-runtime.hpp\"",
  "private:" -> "  // Independent MadGraph comparison, resolved by the validation library.\n  hepcat_spinas::MadGraphValidationReport validate_madgraph(\n    const hepcat_spinas::MadGraphValidationOptions& options = hepcat_spinas::MadGraphValidationOptions());\n\nprivate:"}];
spinasMadGraphCppList[values_List] := "{" <> StringRiffle[ToString /@ values, ", "] <> "}";
spinasMadGraphSource[name_String, config_Association, dir_String] := StringRiffle[Join[{
  "#include \"process.hpp\"", "#include <iostream>", "", "namespace hepcat_generated {", "",
  "hepcat_spinas::MadGraphValidationReport " <> name <> "::validate_madgraph(",
  "    const hepcat_spinas::MadGraphValidationOptions& options) {",
  "  hepcat_spinas::MadGraphConfiguration config;",
  "  config.available = " <> If[config["Status"] === "AVAILABLE", "true", "false"] <> ";",
  "  config.reason = " <> spinasCppQuoted[config["Reason"]] <> ";",
  "  config.madgraph_directory = " <> spinasCppQuoted[config["MadGraphDirectory"]] <> ";",
  "  config.model = " <> spinasCppQuoted[config["MadGraphModel"]] <> ";",
  "  config.python_executable = " <> spinasCppQuoted[config["PythonExecutable"]] <> ";",
  "  config.helper_path = " <> spinasCppQuoted[FileNameJoin[{dir, "madgraph-runtime.py"}]] <> ";",
  "  config.physical_pdgs = " <> spinasMadGraphCppList[config["PhysicalPDGs"]] <> ";",
  "  config.allowed_particle_pdgs = " <> spinasMadGraphCppList[config["AllowedParticlePDGs"]] <> ";",
  "  config.leg_order = " <> spinasMadGraphCppList[config["LegOrder"] - 1] <> ";",
  "  config.spin2 = " <> spinasMadGraphCppList[config["Spin2"]] <> ";",
  "  config.parameter_names = {" <> StringRiffle[spinasCppQuoted /@ config["ParameterNames"], ", "] <> "};",
  "  config.helicities = {" <> StringRiffle[spinasMadGraphCppList /@ config["HelicityConfigurations"], ", "] <> "};",
  "  config.normalization = " <> spinasCppReal[config["Normalization"]] <> ";"},
  KeyValueMap["  config.parameter_rules[" <> spinasCppQuoted[#1] <> "] = " <> spinasCppReal[#2] <> ";" &, config["ParameterRules"]],
  KeyValueMap["  config.parameter_map[" <> spinasCppQuoted[#1] <> "] = " <> spinasCppQuoted[#2] <> ";" &, config["ParameterMap"]], {
  "  return hepcat_spinas::validate_madgraph(*this, config, options, &std::cout);",
  "}", "", "} // namespace hepcat_generated", ""}], "\n"];
spinasMadGraphMain[name_String, config_Association] := StringRiffle[{
  "#include \"process.hpp\"", "#include <fstream>", "#include <iostream>", "#include <vector>", "",
  "int main(int argc, char** argv) {", "  try {",
  "    if (hepcat_spinas::validation_help_requested(argc, argv)) {",
  "      hepcat_spinas::print_validation_usage(std::cout);",
  "      std::cout << \"--report FILE --min-energy E --max-energy E --timeout SECONDS\\n\";", "      return 0;", "    }",
  "    hepcat_spinas::MadGraphValidationOptions options;",
  "    options.points = " <> ToString[config["Points"]] <> ";",
  If[config["Seed"] === "Automatic", "    // A fresh time-based seed is reported for every run.",
    "    options.seed = UINT64_C(" <> ToString[config["Seed"]] <> ");"],
  "    std::string report_file;", "    std::vector<char*> ordinary(1, argv[0]);",
  "    for (int i = 1; i < argc; ++i) {", "      const std::string flag(argv[i]);",
  "      if (flag == \"--report\" || flag == \"--min-energy\" || flag == \"--max-energy\" || flag == \"--timeout\") {",
  "        if (++i >= argc) throw std::invalid_argument(\"Missing validation option value.\");",
  "        if (flag == \"--report\") report_file = argv[i];",
  "        else if (flag == \"--min-energy\") options.minimum_center_of_mass_energy = std::stold(argv[i]);",
  "        else if (flag == \"--max-energy\") options.maximum_center_of_mass_energy = std::stold(argv[i]);",
  "        else { const auto seconds = std::stoul(argv[i]);",
  "          if (seconds == 0 || seconds > 86400) throw std::invalid_argument(\"Timeout must be in [1,86400] seconds.\");",
  "          options.preparation_timeout_seconds = static_cast<unsigned>(seconds); }",
  "      } else ordinary.push_back(argv[i]);", "    }",
  "    const auto parsed = hepcat_spinas::parse_validation_arguments(static_cast<int>(ordinary.size()), ordinary.data(), options);",
  "    static_cast<hepcat_spinas::ValidationOptions&>(options) = parsed;",
  "    hepcat_generated::" <> name <> " process;",
  "    const auto report = process.validate_madgraph(options);",
  "    hepcat_spinas::print_madgraph_validation_report(report, std::cout);",
  "    if (!report_file.empty()) {", "      std::ofstream output(report_file);",
  "      if (!output) throw std::runtime_error(\"Could not write the validation report.\");",
  "      hepcat_spinas::write_madgraph_validation_report_json(report, output);", "    }",
  "    return report.passed ? 0 : 1;", "  } catch (const std::exception& error) {",
  "    std::cerr << \"MadGraph validation error: \" << error.what() << '\\n';", "    return 2;", "  }", "}", ""}, "\n"];
spinasMadGraphCMake[name_String] := StringRiffle[{
  "", "# Independent MadGraph validation; generation and Fortran compilation happen only when run.",
  "find_package(Threads REQUIRED)",
  "add_library(" <> name <> "_madgraph_validation STATIC madgraph-validation.cpp)",
  "target_link_libraries(" <> name <> "_madgraph_validation PUBLIC " <> name <> "_process ${CMAKE_DL_LIBS} Threads::Threads)",
  "add_executable(" <> name <> "_validate_madgraph madgraph-main.cpp)",
  "target_link_libraries(" <> name <> "_validate_madgraph PRIVATE " <> name <> "_madgraph_validation)", ""}, "\n"];
spinasMadGraphProject[dir_String, name_String, config_Association] := <|
  "Files" -> AssociationThread[{"MadGraphRuntime", "MadGraphBuilder", "MadGraphSource", "MadGraphMain"},
    FileNameJoin[{dir, #}] & /@ {"madgraph-runtime.hpp", "madgraph-runtime.py", "madgraph-validation.cpp", "madgraph-main.cpp"}],
  "Contents" -> <|"MadGraphRuntime" -> Import[FileNameJoin[{$spinasExportSourceDirectory, "madgraph-runtime.hpp"}], "Text"],
    "MadGraphBuilder" -> Import[FileNameJoin[{$spinasExportSourceDirectory, "madgraph-runtime.py"}], "Text"],
    "MadGraphSource" -> spinasMadGraphSource[name, config, dir], "MadGraphMain" -> spinasMadGraphMain[name, config]|>|>;
spinasMadGraphREADME[name_String, config_Association] := StringRiffle[{
  "", "## Independent MadGraph validation", "",
  "Run `./build/" <> name <> "_validate_madgraph` to compare the independent MadGraph matrix element at 1000 common random points for each exported massless-helicity combination. Massive spins are summed on both sides because their spin bases may differ. Use `--points`, `--seed`, `--relative-tolerance` and `--absolute-tolerance` to control a run; `--report result.json` preserves its full diagnostic data and failed momenta.",
  "The actual time-based seed is printed and can be replayed. No normalization ratio is fitted. Initial averages and identical-species factors are applied once, using the same convention on both sides. Photon helicities use doubled labels +/-2 in the report. Fixed incoming and outgoing massless helicities are supported.",
  "Select the installation and UFO model explicitly with MadGraphDirectory and MadGraphModel when exporting. MadGraphAllowedParticles -> All retains the full selected model; a list such as {11,22} restricts particle types by absolute PDG code and includes their antiparticles. No QED model or restriction is inferred. Regenerate the project to change these stored settings.",
  "The reference is generated and compiled once in a private temporary workspace. The installed MadGraph source and its model files are not edited. Current SPINAS parameter and mass values are matched to the UFO inputs; unknown mappings or mismatched dependent couplings stop the check.",
  "The base comparison is colorless tree level. The ordinary process remains usable without MadGraph. A numerical FAIL, including floating-point cancellation in an exported formula, is not changed into a pass by fitting a scale or relaxing tolerances.",
  "Configuration: " <> config["Status"] <> If[config["Reason"] === "", "", ". " <> config["Reason"]], ""}, "\n"];

(* ::Subsection::Closed:: *)
(*MadEvent Integration with the Exported Amplitude*)

(* Keep the ordinary MadGraph comparison independent.  This second project
   replaces only MadEvent's numerical matrix kernel, retaining its phase-space
   mappings, integration, unweighting and event output. *)
spinasMadEventConfiguration[dir_String, name_String, ir_Association, config_Association] := Module[
  {family = TrueQ[Lookup[ir, "Family", False]], average, symmetry, defaults},
  average = If[family, ir["InitialSpinAverage"],
    Times @@ (If[TrueQ[ir["MassValues"][[#]] == 0], 1, ir["Spin2"][[#]] + 1] & /@ ir["IncomingLegs"])];
  symmetry = If[family, ir["SymmetryFactor"],
    1/(Times @@ (Factorial /@ Values[Counts[Drop[config["PhysicalPDGs"], 2]]]))];
  defaults = N[Re[ir["ParameterDefaults"]]];
  Join[config, <|"Backend" -> "HEPCAT/SPINAS", "ProcessDirectory" -> dir,
    "Normalization" -> N[config["Normalization"]],
    "ParameterRules" -> Map[N, config["ParameterRules"]],
    "BridgeLibrary" -> FileNameJoin[{dir, "build", "lib" <> name <> "_madevent_matrix.a"}],
    "BridgeLinkFile" -> FileNameJoin[{dir, "build", "madevent-link.json"}],
    "Executable" -> FileNameJoin[{dir, "build", name <> "_madevent"}],
    "SessionDirectory" -> FileNameJoin[{dir, "madevent"}],
    "ParameterDefaults" -> defaults,
    "Parameters" -> MapThread[<|"Name" -> #1, "Value" -> #2|> &, {ir["ParameterNames"], defaults}],
    "Masses" -> N[ir["MassValues"]], "InitialSpinAverage" -> N[average],
    "SymmetryFactor" -> N[symmetry], "TimeConstraint" -> 600,
    "SpinConvention" -> "Raw squared amplitude for one complete external spin configuration; massive basis is SPINAS; no coherent decay sewing.",
    "Scope" -> "Colorless leading-order tree amplitudes; MadEvent integration and unweighted events."|>]
];

spinasMadEventSource[name_String, ir_Association, config_Association] := Module[
  {massless = TrueQ[# == 0] & /@ ir["MassValues"], helicities = config["HelicityConfigurations"]},
  StringRiffle[{
    "// Numerical MadEvent kernel. The independent MadGraph validator uses its own amplitude.",
    "#include \"process.hpp\"", "#include <cmath>", "#include <memory>", "#include <string>", "",
    "namespace {", "using Process = hepcat_generated::" <> name <> ";",
    "// Each numerical worker owns its parameters, particles and spinor-product cache.",
    "thread_local std::unique_ptr<Process> process;",
    "thread_local Process::Parameters previous_parameters;",
    "thread_local Process::Momenta previous_momenta;", "thread_local bool momenta_ready = false;",
    "thread_local std::string last_error;",
    "const std::array<std::size_t, Process::leg_count> leg_order = " <>
      spinasCppArray[ToString /@ (config["LegOrder"] - 1)] <> ";",
    "const std::array<int, Process::leg_count> spin2 = " <> spinasCppArray[ToString /@ ir["Spin2"]] <> ";",
    "const std::array<bool, Process::leg_count> massless = " <>
      spinasCppArray[If[#, "true", "false"] & /@ massless] <> ";",
    "const std::array<Process::Spins, " <> ToString[Length[helicities]] <> "> selected_helicities = {{",
    StringRiffle[("  Process::Spins" <> spinasCppArray[ToString /@ #]) & /@ helicities, ",\n"],
    "}};", "",
    "bool selected(const Process::Spins& spins) {",
    "  for (const auto& helicities : selected_helicities) {", "    bool match = true;",
    "    for (std::size_t leg = 0; leg < Process::leg_count; ++leg)",
    "      if (massless[leg] && spins[leg] != helicities[leg]) match = false;",
    "    if (match) return true;", "  }", "  return false;", "}", "} // namespace", "",
    "extern \"C\" int hepcat_madevent_leg_count() { return static_cast<int>(Process::leg_count); }",
    "extern \"C\" int hepcat_madevent_parameter_count() { return static_cast<int>(Process::parameter_count); }",
    "extern \"C\" const char* hepcat_madevent_error() { return last_error.c_str(); }", "",
    "// MadEvent supplies physical momenta in incoming-then-outgoing order.",
    "// The analytic exporter has already crossed outgoing momenta and spinors once.",
    "// Return raw |amp|^2; MadEvent performs the spin average and identical-state factor.",
    "extern \"C\" int hepcat_madevent_matrix(const double* momenta, const int* helicities,",
    "                                      const double* parameters, double* answer) {",
    "  last_error.clear();", "  if (answer) *answer = 0;", "  try {",
    "    if (!momenta || !helicities || !answer || (Process::parameter_count && !parameters))",
    "      throw std::invalid_argument(\"Missing MadEvent matrix-element input.\");",
    "    Process::Parameters values;",
    "    for (std::size_t parameter = 0; parameter < Process::parameter_count; ++parameter) {",
    "      if (!std::isfinite(parameters[parameter])) throw std::invalid_argument(\"Nonfinite MadEvent model parameter.\");",
    "      values[parameter] = cdouble(parameters[parameter], 0);", "    }",
    "    if (!process) {", "      process.reset(new Process(values)); previous_parameters = values; momenta_ready = false;",
    "    } else if (values != previous_parameters) {", "      process->set_parameters(values); previous_parameters = values; momenta_ready = false;", "    }",
    "    Process::Momenta physical;", "    Process::Spins spins;",
    "    for (std::size_t mg_leg = 0; mg_leg < Process::leg_count; ++mg_leg) {",
    "      const std::size_t leg = leg_order[mg_leg];", "      const int helicity = helicities[mg_leg];",
    "      if ((spin2[leg] == 0 && helicity != 0) ||",
    "          (spin2[leg] == 1 && helicity != -1 && helicity != 1) ||",
    "          (spin2[leg] == 2 && helicity != -1 && helicity != 1 && (massless[leg] || helicity != 0)))",
    "        throw std::invalid_argument(\"Invalid MadEvent external helicity.\");",
    "      spins[leg] = spin2[leg] == 2 ? 2*helicity : helicity;",
    "      if ((process->masses()[leg] == 0) != massless[leg])",
    "        throw std::invalid_argument(\"An external massless/massive category changed; regenerate this process.\");",
    "      for (std::size_t component = 0; component < 4; ++component) {",
    "        const double value = momenta[4*mg_leg + component];",
    "        if (!std::isfinite(value)) throw std::invalid_argument(\"Nonfinite MadEvent momentum.\");",
    "        physical[leg][component] = value;", "      }", "    }",
    "    // A fixed massless helicity is a selected contribution, not an extra average.",
    "    if (!selected(spins)) return 0;",
    "    // MadEvent visits many spins at one phase-space point. Reuse all products.",
    "    if (!momenta_ready || physical != previous_momenta) {",
    "      process->set_momenta(physical); previous_momenta = physical; momenta_ready = true;", "    }",
    "    const ldouble probability = std::norm(process->amp(spins));",
    "    *answer = static_cast<double>(probability);",
    "    if (!std::isfinite(*answer) || *answer < 0) throw std::runtime_error(\"Nonfinite HEPCAT squared amplitude.\");",
    "    return 0;", "  } catch (const std::exception& error) { last_error = error.what(); }",
    "  catch (...) { last_error = \"Unknown HEPCAT numerical matrix-element error.\"; }",
    "  // A failed update must not leave a partially rebound process in the next call.",
    "  process.reset(); momenta_ready = false;", "  if (answer) *answer = 0;", "  return 1;", "}", ""}, "\n"]
];

spinasMadEventMain[name_String, config_Association, dir_String] := StringRiffle[{
  "// Start the private MadEvent session with the exported HEPCAT/SPINAS matrix element.",
  "#include <cerrno>", "#include <cstring>", "#include <iostream>", "#include <string>",
  "#include <vector>", "#include <unistd.h>", "", "int main(int argc, char** argv) {",
  "  std::vector<std::string> arguments = {", "    " <> spinasCppQuoted[config["PythonExecutable"]] <> ",",
  "    " <> spinasCppQuoted[FileNameJoin[{dir, "madevent-runtime.py"}]] <> ",",
  "    \"--configuration\", " <> spinasCppQuoted[FileNameJoin[{dir, "madevent-config.json"}]] <> "};",
  "  for (int i = 1; i < argc; ++i) arguments.push_back(argv[i]);",
  "  std::vector<char*> command;", "  for (auto& argument : arguments) command.push_back(&argument[0]);",
  "  command.push_back(nullptr);", "  execv(command[0], command.data());",
  "  std::cerr << \"Could not start the MadEvent integration helper: \" << std::strerror(errno) << '\\n';",
  "  return 2;", "}", ""}, "\n"];

spinasMadEventCMake[name_String, config_Association] := If[config["Status"] =!= "AVAILABLE", "",
  StringRiffle[{
    "", "# MadEvent uses this raw fixed-spin kernel; the reference validator stays independent.",
    "# A static archive also supports an existing SPINAS library built without PIC.",
    "add_library(" <> name <> "_madevent_matrix STATIC madevent-spinas.cpp)",
    "target_link_libraries(" <> name <> "_madevent_matrix PUBLIC " <> name <> "_process)",
    "add_executable(" <> name <> "_madevent madevent-main.cpp)",
    "add_dependencies(" <> name <> "_madevent " <> name <> "_madevent_matrix)",
    "if(APPLE AND CMAKE_CXX_COMPILER_ID MATCHES \"Clang\")",
    "  set(HEPCAT_MADEVENT_CXX_RUNTIME \"c++\")", "else()",
    "  set(HEPCAT_MADEVENT_CXX_RUNTIME \"stdc++\")", "endif()",
    "get_filename_component(HEPCAT_MADEVENT_RUNTIME_DIRECTORY \"${SPINAS_LIBRARY}\" DIRECTORY)",
    "file(GENERATE OUTPUT \"${CMAKE_CURRENT_BINARY_DIR}/madevent-link.json\" CONTENT",
    "  \"{\\n  \\\"Libraries\\\": [\\\"$<TARGET_FILE:" <> name <> "_madevent_matrix>\\\", \\\"$<TARGET_FILE:" <>
      name <> "_process>\\\", \\\"${SPINAS_LIBRARY}\\\"],\\n  \\\"CXXRuntime\\\": \\\"${HEPCAT_MADEVENT_CXX_RUNTIME}\\\",\\n  \\\"RuntimeLibraryDirectories\\\": [\\\"${HEPCAT_MADEVENT_RUNTIME_DIRECTORY}\\\"]\\n}\\n\")", ""}, "\n"]];

spinasMadEventProject[dir_String, name_String, ir_Association, config_Association] :=
  If[config["Status"] =!= "AVAILABLE", <|"Files" -> <||>, "Contents" -> <||>|>, <|
    "Files" -> AssociationThread[{"MadEventBuilder", "MadEventSource", "MadEventMain", "MadEventConfiguration"},
      FileNameJoin[{dir, #}] & /@ {"madevent-runtime.py", "madevent-spinas.cpp", "madevent-main.cpp", "madevent-config.json"}],
    "Contents" -> <|"MadEventBuilder" -> Import[FileNameJoin[{$spinasExportSourceDirectory, "madevent-runtime.py"}], "Text"],
      "MadEventSource" -> spinasMadEventSource[name, ir, config],
      "MadEventMain" -> spinasMadEventMain[name, config, dir],
      "MadEventConfiguration" -> ExportString[config, "RawJSON"]|>|>];

spinasMadEventREADME[name_String, config_Association] := If[config["Status"] =!= "AVAILABLE", "", StringRiffle[{
  "", "## MadEvent integration using the HEPCAT amplitude", "",
  "The same MadGraphDirectory, MadGraphModel, particle restriction and parameter matching also configure a numerical MadEvent backend. Build the ordinary project, then run `./build/" <> name <> "_madevent`. The first run creates a private `madevent/` process and replaces its numerical matrix kernel with the supplied HEPCAT/SPINAS amplitude. MadGraph still supplies the phase-space mappings, integration, cuts, unweighting and Les Houches event output. Its independent reference validator remains unchanged.",
  "Use `./build/" <> name <> "_madevent --prepare` to create and compile without starting a run, `--command-file commands.txt` for standard MadEvent commands, or `--run run_name` to generate events. `--run-card FILE` and `--param-card FILE` select cards before a run; `--help` lists the launcher options. Edit the private process cards to change beams, cuts, event counts, masses and couplings as in an ordinary MadEvent process.",
  "The matrix kernel receives current evaluated UFO model parameters on every call, including parameter-card updates. It reuses one SPINAS cache per numerical thread and updates masses before momenta. Unsupported mappings, complex parameters, changed massless/massive categories or failed evaluations stop the run. The installed MadGraph source and models are not edited.",
  "The returned kernel is raw |amp|^2 for each selected helicity and each massive spin. MadEvent performs incoming averaging and final identical-species factors exactly once, with the fixed-incoming-helicity average adjusted. Selected outgoing helicities remain labeled contributions. Massive spins are summed in the SPINAS basis; this interface does not provide spin-correlated decays or an identified MadGraph massive-spin basis. Current coverage is colorless leading-order tree amplitudes, not NLO or matched showers. The generated `madevent-spinas.cpp` and `madevent-main.cpp` show the numerical interface explicitly.", ""}, "\n"]];

(* ::Subsection::Closed:: *)
(*Mathematica Entry Point*)

spinasMadGraphRunOptionNames = {"Output", "RelativeTolerance", "AbsoluteTolerance",
  "MinimumEnergy", "MaximumEnergy", "TimeConstraint", "MadGraphPoints", "MadGraphSeed", "CMakeExecutable"};
spinasMadGraphValidationOptionsQ[options_Association] := spinasMadGraphOptionsQ[options] &&
  MemberQ[{"Table", "Summary", "Details"}, options["Output"]] &&
  AllTrue[Lookup[options, {"RelativeTolerance", "AbsoluteTolerance"}],
    NumberQ[#] && TrueQ[Im[#] == 0 && # >= 0] && FreeQ[#, Indeterminate | DirectedInfinity] &] &&
  AllTrue[Lookup[options, {"MinimumEnergy", "MaximumEnergy"}],
    # === Automatic || NumberQ[#] && TrueQ[Im[#] == 0 && # > 0] && FreeQ[#, Indeterminate | DirectedInfinity] &] &&
  IntegerQ[options["TimeConstraint"]] && 0 < options["TimeConstraint"] <= 86400 &&
  (options["MinimumEnergy"] === Automatic || options["MaximumEnergy"] === Automatic ||
    TrueQ[options["MinimumEnergy"] <= options["MaximumEnergy"]]);
spinasMadGraphCLIReal[value_] := StringDrop[StringDrop[spinasCppReal[value], 8], -2];
spinasMadGraphHelicityLabel[helicities_List] := Module[{labels},
  labels = MapIndexed[If[#1 === 0, Nothing,
    ToString[First[#2]] <> ":" <> If[#1 > 0, "+", "-"]] &, helicities];
  If[labels === {}, "No massless helicities", StringRiffle[labels, ", "]]
];
spinasMadGraphValidationOutput[report_Association, output_String] := Switch[output,
  "Details", report,
  "Summary", If[Lookup[report, "Rows", {}] === {}, <|report["Status"] -> 1|>,
    Counts[Lookup[report["Rows"], "Status", {}]]],
  _, Dataset[If[Lookup[report, "Rows", {}] === {},
    {KeyTake[report, {"Status", "Message"}]},
    Map[Join[<|"Helicities" -> spinasMadGraphHelicityLabel[#["Helicities"]]|>,
      KeyTake[#, {"Status", "Points", "MaximumRelativeError"}]] &, report["Rows"]]],
    MaxItems -> {All, All}]];

(* Do not let one independent reference failure suppress the other comparison.
   A written project is not evidence that its requested physics checks passed. *)
spinasRunRequestedValidations[result_Association, directory_String, name_String,
    calchep_Association, madgraph_Association, options_Association] := Module[
  {updated = result, report, failures = {}, command, status},
  If[TrueQ[options["RunCalcHEPValidation"]],
    report = spinasRunCalcHEPValidation[directory, name, calchep];
    AssociateTo[updated, "CalcHEPValidation" -> report];
    status = If[AssociationQ[report], Lookup[report, "Status", "ERROR"],
      command = If[FailureQ[report], Lookup[report[[2]], "CommandResult", <||>], <||>];
      FirstCase[StringCases[Lookup[command, "StandardOutput", ""],
        "Amplitude validation: " ~~ value : ("FAIL" | "ERROR" | "UNAVAILABLE") :> value],
        _String, "ERROR"]];
    Print[Dataset[{<|"Reference" -> "CalcHEP", "Status" -> status,
      "RequestedPoints" -> calchep["Points"]|>}]];
    If[status =!= "PASS", AppendTo[failures, "CalcHEP"]]];
  If[TrueQ[options["RunMadGraphValidation"]],
    report = spinasRunMadGraphValidation[directory, name, madgraph];
    AssociateTo[updated, "MadGraphValidation" -> report];
    If[AssociationQ[report], Print[spinasMadGraphValidationOutput[report, "Table"]],
      Print[Dataset[{<|"Reference" -> "MadGraph", "Status" -> "ERROR"|>}]]];
    If[!AssociationQ[report] || Lookup[report, "Status", "ERROR"] =!= "PASS",
      AppendTo[failures, "MadGraph"]]];
  If[failures =!= {}, Return[Failure["SPINASValidation", <|
    "MessageTemplate" -> "The requested independent comparison did not pass for `1`. Every requested reference check was attempted; inspect ExportResult for the reports.",
    "MessageParameters" -> {StringRiffle[failures, ", "]}, "Directory" -> directory,
    "FailedReferences" -> failures, "ExportResult" -> updated|>]]];
  updated
];
spinasRunMadGraphValidation[directory_String, name_String, config_Association, supplied_Association : <||>] := Module[
  {options = Join[Association[Options[ValidateMadGraph]], supplied], reportFile, command, run, report, seed},
  reportFile = FileNameJoin[{directory, "madgraph-validation-report-" <> CreateUUID[] <> ".json"}];
  command = {FileNameJoin[{directory, "build", name <> "_validate_madgraph" <> If[$OperatingSystem === "Windows", ".exe", ""]}],
    "--points", ToString[Lookup[supplied, "MadGraphPoints", config["Points"]]], "--report", reportFile,
    "--relative-tolerance", spinasMadGraphCLIReal[options["RelativeTolerance"]],
    "--absolute-tolerance", spinasMadGraphCLIReal[options["AbsoluteTolerance"]],
    "--timeout", ToString[options["TimeConstraint"]]};
  seed = Lookup[supplied, "MadGraphSeed", config["Seed"]];
  If[seed =!= Automatic && seed =!= "Automatic", command = Join[command, {"--seed", ToString[seed]}]];
  If[options["MinimumEnergy"] =!= Automatic, command = Join[command, {"--min-energy", spinasMadGraphCLIReal[options["MinimumEnergy"]]}]];
  If[options["MaximumEnergy"] =!= Automatic, command = Join[command, {"--max-energy", spinasMadGraphCLIReal[options["MaximumEnergy"]]}]];
  Print["Comparing HEPCAT with independent MadGraph: ", Lookup[supplied, "MadGraphPoints", config["Points"]],
    " common points for each requested massless-helicity combination."];
  run = spinasProjectCommand[command];
  report = If[FileExistsQ[reportFile], Quiet[Check[Import[reportFile, "RawJSON"], $Failed]], $Failed];
  If[!AssociationQ[report], Return[Failure["MadGraphValidation", <|
    "MessageTemplate" -> "MadGraph validation did not produce a readable report.", "CommandResult" -> run|>]]];
  If[!MemberQ[{0, 1}, Lookup[run, "ExitCode", -1]] || !IntegerQ[Lookup[report, "Seed", None]] ||
      (IntegerQ[seed] && report["Seed"] =!= seed) ||
      !MemberQ[{"PASS", "FAIL", "ERROR", "UNAVAILABLE"}, Lookup[report, "Status", None]] ||
      (TrueQ[Lookup[report, "Passed", False]] =!= (report["Status"] === "PASS")) ||
      (report["Status"] === "PASS" && (Lookup[run, "ExitCode", -1] =!= 0 || Lookup[report, "Rows", {}] === {} ||
        !AllTrue[report["Rows"], Lookup[#, "Status", None] === "PASS" && TrueQ[Lookup[#, "Points", 0] > 0] &])),
    Return[Failure["MadGraphValidation", <|"MessageTemplate" -> "The validator returned an inconsistent report.", "CommandResult" -> run|>]]];
  If[StringQ[Lookup[report, "ReferenceBuildJSON", None]],
    With[{reference = Quiet[Check[ImportString[report["ReferenceBuildJSON"], "RawJSON"], $Failed]]},
      If[AssociationQ[reference], report = Join[KeyDrop[report, "ReferenceBuildJSON"], <|"Reference" -> reference|>]]]];
  Print["MadGraph random seed: ", report["Seed"]];
  Join[report, <|"Directory" -> directory, "ReportFile" -> reportFile,
    "StandardOutput" -> Lookup[run, "StandardOutput", ""], "StandardError" -> Lookup[run, "StandardError", ""]|>]
];

ValidateMadGraph[directory_String, opts : OptionsPattern[]] := Module[
  {supplied = Association[Reverse[{opts}]], options, dir = ExpandFileName[directory], manifest,
   config, name, build, report},
  options = Join[Association[Options[ValidateMadGraph]], supplied];
  If[Complement[Keys[supplied], First /@ Options[ValidateMadGraph]] =!= {} ||
      !spinasMadGraphValidationOptionsQ[options], Message[ValidateMadGraph::args]; Return[$Failed]];
  If[!FileExistsQ[FileNameJoin[{dir, "manifest.json"}]], Return[Failure["MadGraphProject", <|
    "MessageTemplate" -> "The directory is not a generated HEPCAT/SPINAS process project.", "Directory" -> dir|>]]];
  manifest = Quiet[Check[Import[FileNameJoin[{dir, "manifest.json"}], "RawJSON"], $Failed]];
  If[!AssociationQ[manifest] || !KeyExistsQ[manifest, "MadGraphValidation"], Return[Failure["MadGraphProject", <|
    "MessageTemplate" -> "Regenerate this project with WriteSPINASProcess to add MadGraph validation."|>]]];
  config = manifest["MadGraphValidation"]; name = Lookup[manifest, "Class", None];
  If[!AssociationQ[config] || !StringQ[name] || !TrueQ[spinasCppIdentifierQ[name]] ||
      !StringQ[Lookup[config, "Status", None]] || !StringQ[Lookup[config, "Reason", None]],
    Return[Failure["MadGraphProject", <|"MessageTemplate" -> "The generated project's validation metadata are malformed."|>]]];
  If[config["Status"] =!= "AVAILABLE", Return[Failure["MadGraphUnavailable", <|"MessageTemplate" -> config["Reason"]|>]]];
  If[Complement[Keys[supplied], spinasMadGraphRunOptionNames] =!= {},
    Return[Failure["MadGraphProjectConfiguration", <|"MessageTemplate" ->
      "Model or installation changes require regenerating the project with WriteSPINASProcess; sampling options can be changed directly."|>]]];
  build = spinasProjectBuild[dir, name, "", False, options["CMakeExecutable"]];
  If[FailureQ[build], Return[build]];
  report = spinasRunMadGraphValidation[dir, name, config, supplied];
  If[FailureQ[report], Return[report]];
  spinasMadGraphValidationOutput[report, options["Output"]]
];
ValidateMadGraph[process : (_Rule | {___String}), amplitude_, model_, opts : OptionsPattern[]] := Module[
  {supplied = Association[Reverse[{opts}]], options, directory, exported, exportOptions, report},
  options = Join[Association[Options[ValidateMadGraph]], supplied];
  If[Complement[Keys[supplied], First /@ Options[ValidateMadGraph]] =!= {} ||
      !spinasMadGraphValidationOptionsQ[options], Message[ValidateMadGraph::args]; Return[$Failed]];
  directory = CreateDirectory[];
  exportOptions = Join[KeyTake[supplied, First /@ Options[WriteSPINASProcess]],
    <|"Build" -> False, "RunExamples" -> False, "RunCalcHEPValidation" -> False,
      "RunMadGraphValidation" -> False, "ReturnDetails" -> True|>];
  exported = WriteSPINASProcess[directory, process, amplitude, model, Sequence @@ Normal[exportOptions]];
  If[FailureQ[exported] || exported === $Failed, Return[exported]];
  ValidateMadGraph[directory, Sequence @@ Normal[KeyTake[options, spinasMadGraphRunOptionNames]]]
];
ValidateMadGraph[___] := (Message[ValidateMadGraph::args]; $Failed);
End[];
End[];
