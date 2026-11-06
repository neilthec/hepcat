(* Complete helicity families preserve spin sums and physical symmetry factors. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0;
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++; If[!TrueQ[value],
  AppendTo[failures, name]; Print["FAIL: ", name]]);
sfDirectory = CreateDirectory[];
sfSpinas = Replace[Environment["HEPCAT_SPINAS_DIRECTORY"],
  $Failed | "" -> FileNameJoin[{ParentDirectory[root], "spinas"}]];
sfModel = ReadModel[FileNameJoin[{root, "models"}], 3];
sfMass = ModelParameter[sfModel, "Me"]; sfCharge = ModelParameter[sfModel, "EE"];
sfRules = {"Me" -> 1, "EE" -> 1/3};
sfModelBefore = sfModel;
sfGlobalsBefore = {OwnValues[Global`Me], OwnValues[Global`EE]};
sfA[i_, j_] := SpinorChain[Spinor["Helicity", "Angle", i], Spinor["Helicity", "Angle", j]];
sfS[i_, j_] := SpinorChain[Spinor["Helicity", "Square", i], Spinor["Helicity", "Square", j]];
sfF[type_, i_, j_] := SpinorChain[Spinor["Spin", type, i], Spinor["Spin", type, j]];
sfLookup[process_List] := LookupAmplitude[Thread[{process, Range[Length[process]]}],
  sfModel, False, {sfI, sfJ, sfK, sfL}];
sfRecords[processes_List] := {#, sfLookup[#]} & /@ processes;
sfPath[name_] := FileNameJoin[{sfDirectory, name}];
sfRun[command_List] := Quiet[Check[RunProcess[command], $Failed]];
sfFailed[report_] := !AssociationQ[report] || Lookup[report, "ExitCode", -1] =!= 0;
sfExport[name_String, records_List, model_:Automatic, opts___] := WriteSPINASProcess[
  sfPath[name], records, If[model === Automatic, sfModel, model], opts,
  "ProcessName" -> name, "ParameterRules" -> sfRules,
  "SPINASDirectory" -> sfSpinas, "ReturnDetails" -> True];

sfFourProcesses = Join[{"e", "E"}, #] & /@
  {{"A.+", "A.+"}, {"A.+", "A.-"}, {"A.-", "A.-"}};
sfFour = sfRecords[sfFourProcesses];
check["helicity symmetry uses the conservative automatic default",
  ("HelicitySymmetry" /. Options[WriteSPINASProcess]) === Automatic];
sfPlain = sfExport["family_plain", sfFour];
check["one project accepts three independent two-photon helicity representatives",
  AssociationQ[sfPlain] && DirectoryQ[sfPlain["Directory"]]];
If[!AssociationQ[sfPlain], Print["FAMILY EXPORT DETAILS: ", sfPlain]; Quit[1]];
check["identical-photon permutation closes all four ordered helicities",
  Sort[sfPlain["HelicityConfigurations"]] ===
    Sort[Join[{0, 0}, #] & /@ Tuples[{-2, 2}, 2]] &&
    sfPlain["InputBranchCount"] === 3 && sfPlain["GeneratedBranchCount"] === 1];
check["two photons receive a final 2! divisor and the incoming spin average",
  sfPlain["InitialSpinAverage"] === 4 && sfPlain["FinalStateSymmetryDivisor"] === 2 &&
    sfPlain["SymmetryFactor"] === 1/2];
sfManifest = Import[sfPlain["Files"]["Manifest"], "RawJSON"];
check["the manifest records complete helicities and separate normalization factors",
  Length[sfManifest["HelicityConfigurations"]] === 4 &&
    sfManifest["InitialSpinAverage"] === 4 && sfManifest["FinalStateSymmetryDivisor"] === 2];
sfHeader = Import[sfPlain["Files"]["Header"], "Text"];
sfReadableSource = Import[sfPlain["Files"]["Source"], "Text"];
check["native helicity formulas are directly readable amplitude branches",
  StringContainsQ[sfReadableSource, "::amp("] &&
    StringContainsQ[sfReadableSource, "if ("] && StringContainsQ[sfReadableSource, ".v("] &&
    !StringContainsQ[sfReadableSource, RegularExpression["::component_[0-9]+\\("]]];
check["the saved family source separates formulas and tables into readable lines",
  Max[StringLength /@ StringSplit[sfReadableSource, "\n"]] <= 120 &&
    StringContainsQ[sfReadableSource, "a12a"] &&
    StringContainsQ[sfReadableSource, "t13"]];
check["the generated header exposes spin-array and native-style amplitude calls",
  AllTrue[{"amp(const Spins&", "spin_sum()", "amp2()", "incoming_spin_average",
    "symmetry_factor", "helicity_configurations"}, StringContainsQ[sfHeader, #] &] &&
    StringCount[sfHeader, "amp("] >= 2];
sfNoSymmetry = sfExport["family_exhaustive", sfFour, sfModel, "HelicitySymmetry" -> None];
check["None retains an exhaustive complete helicity family",
  AssociationQ[sfNoSymmetry] && !TrueQ[sfNoSymmetry["HelicitySummation"]["Active"]] &&
    sfNoSymmetry["HelicitySummation"]["Method"] === "Exhaustive" &&
    Length[sfNoSymmetry["HelicityConfigurations"]] === 4];
sfConjugation = sfExport["family_conjugation", sfFour, sfModel,
  "HelicitySymmetry" -> "Conjugation"];
check["declared QED conjugation halves only the summed evaluation inventory",
  AssociationQ[sfConjugation] &&
    TrueQ[sfConjugation["HelicitySummation"]["CompletePairing"]] &&
    Length[sfConjugation["HelicitySummation"]["Pairs"]] === 2 &&
    Length[sfConjugation["HelicitySummation"]["RepresentativeConfigurations"]] === 2 &&
    Length[sfConjugation["HelicityConfigurations"]] === 4 &&
    sfConjugation["InputBranchCount"] === 3 && sfConjugation["GeneratedBranchCount"] === 1];
sfCanonicalConjugation = sfExport["p1", sfFour, sfModel,
  "HelicitySymmetry" -> "Conjugation"];
check["the canonical layout retains the declared conjugation shortcut",
  AssociationQ[sfCanonicalConjugation] &&
    sfCanonicalConjugation["CodeLayout"] === "CanonicalDispatch" &&
    TrueQ[sfCanonicalConjugation["HelicitySummation"]["CompletePairing"]]];
sfFixedConjugation = sfExport["family_fixed_conjugation", sfFour, sfModel,
  "HelicitySymmetry" -> "Conjugation", "FixedHelicities" -> {3 -> 2}];
check["a selected helicity set without its reversed partners stays exhaustive",
  AssociationQ[sfFixedConjugation] &&
    !TrueQ[sfFixedConjugation["HelicitySummation"]["Active"]] &&
    Length[sfFixedConjugation["HelicityConfigurations"]] === 2 &&
    StringContainsQ[sfFixedConjugation["HelicitySummation"]["Reason"], "not closed"]];
sfBadSymmetryPath = sfPath["invalid_helicity_symmetry"];
sfBadSymmetry = Quiet[WriteSPINASProcess[sfBadSymmetryPath, sfFour, sfModel,
  "HelicitySymmetry" -> "GuessFromSpins", "SPINASDirectory" -> sfSpinas]];
check["unsupported helicity symmetry fails before writing a project",
  FailureQ[sfBadSymmetry] && !DirectoryQ[sfBadSymmetryPath]];
sfFiles = Values[sfPlain["Files"]];
sfBefore = AssociationMap[BinaryReadList[#, "Byte"] &, sfFiles];
sfRefused = sfExport["family_plain", ({First[#], 2 Last[#]} & /@ sfFour)];
check["a valid family cannot overwrite existing generated files by default",
  FailureQ[sfRefused] && sfBefore === AssociationMap[BinaryReadList[#, "Byte"] &, sfFiles]];
sfReservedPath = sfPath["reserved_helicity_method"];
sfReservedName = WriteSPINASProcess[sfReservedPath, sfFour, sfModel,
  "ProcessName" -> "helicity_pp", "ParameterRules" -> sfRules, "SPINASDirectory" -> sfSpinas];
check["the process class name cannot collide with a descriptive helicity method",
  FailureQ[sfReservedName] && !DirectoryQ[sfReservedPath]];

sfIncompletePath = sfPath["missing_helicities"];
sfIncomplete = WriteSPINASProcess[sfIncompletePath, Take[sfFour, 1], sfModel,
  "ParameterRules" -> sfRules, "SPINASDirectory" -> sfSpinas];
check["missing helicity counts are rejected before creating a project",
  FailureQ[sfIncomplete] && !DirectoryQ[sfIncompletePath] &&
    Sort[Lookup[sfIncomplete[[2]], "MissingHelicities", {}]] ===
      Sort[{{0, 0, -2, -2}, {0, 0, -2, 2}, {0, 0, 2, -2}}]];
sfDuplicatePath = sfPath["conflicting_helicities"];
sfDuplicate = WriteSPINASProcess[sfDuplicatePath,
  Append[sfFour, {First[First[sfFour]], 2 Last[First[sfFour]]}], sfModel,
  "ParameterRules" -> sfRules, "SPINASDirectory" -> sfSpinas];
check["conflicting expressions for one helicity are not silently selected",
  FailureQ[sfDuplicate] && !DirectoryQ[sfDuplicatePath]];
sfBadPath = sfPath["unresolved_reference"];
sfBad = WriteSPINASProcess[sfBadPath,
  ReplacePart[sfFour, {1, 2} -> Last[First[sfFour]] SpinorChain[
    Spinor["Xi", "Angle", 100], Spinor["Helicity", "Angle", 3]]], sfModel,
  "ParameterRules" -> sfRules, "SPINASDirectory" -> sfSpinas];
check["one unsupported branch rejects the family atomically",
  FailureQ[sfBad] && !DirectoryQ[sfBadPath]];
sfExplicitZero = sfExport["explicit_zero_sectors", ({First[#], 0} & /@ sfFour)];
check["explicitly supplied zero sectors still form a complete massive-spin family",
  AssociationQ[sfExplicitZero] && Length[sfExplicitZero["HelicityConfigurations"]] === 4];
sfColoredModel = sfModel;
sfColoredModel[[1]] = Map[If[MemberQ[#[[{2, 3}]], "e"], ReplacePart[#, 8 -> 3], #] &,
  sfColoredModel[[1]]];
sfColoredPath = sfPath["missing_color_sum"];
sfColored = WriteSPINASProcess[sfColoredPath, sfFour, sfColoredModel,
  "ParameterRules" -> sfRules, "SPINASDirectory" -> sfSpinas];
check["an unavailable color sum is rejected rather than called unpolarized amp2",
  FailureQ[sfColored] && !DirectoryQ[sfColoredPath]];
sfComptonPP = sfF["Angle", 1, 3] sfS[2, 4]^2;
sfComptonPM = sfF["Angle", 1, 3] SpinorChain[
  Spinor["Helicity", "Angle", 4], Mom[1], Spinor["Helicity", "Square", 2]]^2;
sfComptonMM = sfF["Angle", 1, 3] sfA[2, 4]^2;
sfComptonPath = sfPath["separate_crossing_roles"];
sfCompton = WriteSPINASProcess[sfComptonPath, {
  {{"e", "A.+", "E", "A.+"}, sfComptonPP},
  {{"e", "A.+", "E", "A.-"}, sfComptonPM},
  {{"e", "A.-", "E", "A.-"}, sfComptonMM}}, sfModel,
  "ParameterRules" -> sfRules, "SPINASDirectory" -> sfSpinas];
check["helicity closure never exchanges an incoming and outgoing photon",
  FailureQ[sfCompton] && !DirectoryQ[sfComptonPath]];
check["family generation changes neither its model nor global parameter symbols",
  sfModel === sfModelBefore &&
    {OwnValues[Global`Me], OwnValues[Global`EE]} === sfGlobalsBefore];
Do[
  sfReportPath = sfPath["rejected_report_" <> sfReportStatus];
  sfReportEntries = ReplacePart[sfFour, {1, 2} ->
    <|"Status" -> sfReportStatus, "Amplitude" -> Last[First[sfFour]]|>];
  sfReportResult = WriteSPINASProcess[sfReportPath, sfReportEntries, sfModel,
    "ParameterRules" -> sfRules, "SPINASDirectory" -> sfSpinas];
  check["an explicit unsuccessful report cannot become a helicity branch: " <> sfReportStatus,
    FailureQ[sfReportResult] && !DirectoryQ[sfReportPath]],
  {sfReportStatus, {"UNRESOLVED", "UNDERDETERMINED", "PARTIAL", "FAIL", "UNKNOWN"}}];
sfLegacyReportPath = sfPath["rejected_legacy_report"];
sfLegacyReport = WriteSPINASProcess[sfLegacyReportPath, First[First[sfFour]],
  <|"Status" -> "UNRESOLVED", "Amplitude" -> Last[First[sfFour]]|>, sfModel,
  "ParameterRules" -> sfRules, "SPINASDirectory" -> sfSpinas];
check["the fixed-helicity exporter also rejects an explicit unresolved report",
  FailureQ[sfLegacyReport] && !DirectoryQ[sfLegacyReportPath]];
sfSuccessfulWrappers = MapIndexed[{First[#1], If[First[#2] === 1,
  <|"Status" -> "COMPLETE", "Amplitude" -> Last[#1]|>,
  <|"Amplitude" -> <|"Amplitude" -> Last[#1]|>|>]} &, sfFour];
sfWrapperReport = sfExport["successful_report_wrappers", sfSuccessfulWrappers];
check["successful reports and statusless legacy wrappers remain accepted",
  AssociationQ[sfWrapperReport] && Length[sfWrapperReport["HelicityConfigurations"]] === 4];
sfOptionsList = WriteSPINASProcess[sfPath["family_option_list"], sfFour, sfModel,
  {"ParameterRules" -> sfRules, "SPINASDirectory" -> sfSpinas, "ReturnDetails" -> True}];
check["a standard option list dispatches to the grouped exporter",
  AssociationQ[sfOptionsList] && Length[sfOptionsList["HelicityConfigurations"]] === 4 &&
    sfOptionsList["InitialSpinAverage"] === 4];
sfLegacyOptionsList = WriteSPINASProcess[sfPath["legacy_option_list"],
  First[First[sfFour]], Last[First[sfFour]], sfModel,
  {"ParameterRules" -> sfRules, "SPINASDirectory" -> sfSpinas, "ReturnDetails" -> True}];
check["standard option lists still work with the fixed-helicity overload",
  AssociationQ[sfLegacyOptionsList] && sfLegacyOptionsList["Process"] === First[First[sfFour]]];
sfMixedOptions = WriteSPINASProcess[sfPath["mixed_option_priority"], sfFour, sfModel,
  "ProcessName" -> "family_option_priority", "ReturnDetails" -> True,
  {"ProcessName" -> "bad/name", "ReturnDetails" -> False,
   "ParameterRules" -> sfRules, "SPINASDirectory" -> sfSpinas}];
check["mixed option lists retain first-rule precedence and nested parameter rules",
  AssociationQ[sfMixedOptions] &&
    StringContainsQ[Import[sfMixedOptions["Files"]["Header"], "Text"], "class family_option_priority"] &&
    Import[sfMixedOptions["Files"]["Manifest"], "RawJSON"]["Masses"] === {1., 1., 0., 0.}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
sfReloaded = WriteSPINASProcess[sfPath["reloaded_family_option_list"], sfFour, sfModel,
  {"ParameterRules" -> sfRules, "SPINASDirectory" -> sfSpinas, "ReturnDetails" -> True}];
check["normal package reload retains the unambiguous family overload",
  AssociationQ[sfReloaded] && Length[sfReloaded["HelicityConfigurations"]] === 4];

sfIncoming = sfRecords[Join[#, {"e", "E"}] & /@
  {{"A.+", "A.+"}, {"A.+", "A.-"}, {"A.-", "A.-"}}];
sfFive = sfRecords[Join[{"e", "E"}, #] & /@
  {{"A.+", "A.+", "A.+"}, {"A.+", "A.+", "A.-"},
   {"A.+", "A.-", "A.-"}, {"A.-", "A.-", "A.-"}}];
sfSixFermions = {{{"e", "E", "E", "e", "E", "e"},
  sfCharge^4 sfF["Angle", 1, 2] sfF["Angle", 3, 4] sfF["Angle", 5, 6]}};

sfScalarRow = {"family scalar", "r", "r", 901, 0, sfMass, 0, 1, 1, "r", "r"};
sfZeroScalarRow = {"family massless scalar", "z", "z", 902, 0, 0, 0, 1, 1, "z", "z"};
sfGeneric = sfModel;
sfGeneric[[1]] = Join[sfGeneric[[1]], {sfScalarRow, sfZeroScalarRow,
  {"family fermion plus", "n.+", "N.+", 903, 1, 0, 0, 1, 1, "n", "N"},
  {"family fermion minus", "n.-", "N.-", 903, 1, 0, 0, 1, 1, "n", "N"}}];
sfGP = ModelParameter["FamilyGP", "spinas-family-tests", "Complex"];
sfGM = ModelParameter["FamilyGM", "spinas-family-tests", "Complex"];
sfGeneric[[3]] = Join[sfGeneric[[3]], {{sfGP, 1/3 + I/5}, {sfGM, 2/7 - I/11}}];
sfCouplings = {
  {{"e", "E", "A.+", "z"}, sfGP SpinorChain[Spinor["Spin", "Angle", 1],
     Mom[2], Spinor["Helicity", "Square", 3]] SpinorChain[
     Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]]},
  {{"e", "E", "A.-", "z"}, sfGM SpinorChain[Spinor["Spin", "Square", 1],
     Mom[2], Spinor["Helicity", "Angle", 3]] SpinorChain[
     Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle", 3]]}};
sfAsymmetric = sfExport["family_asymmetric_symmetry", sfCouplings, sfGeneric];
check["automatic symmetry never merges independent complex helicity couplings",
  AssociationQ[sfAsymmetric] &&
    !TrueQ[sfAsymmetric["HelicitySummation"]["Active"]] &&
    Length[sfAsymmetric["HelicityConfigurations"]] === 2];
(* These conjugate prefactors have equal moduli only on the real-G domain.
   A complex parameter update makes the exhaustive fallback observable. *)
sfRealityCouplings = sfCouplings /. {sfGP -> sfGP + I, sfGM -> sfGP - I};
sfRealityConjugation = sfExport["family_reality_guard", sfRealityCouplings, sfGeneric,
  "HelicitySymmetry" -> "Conjugation"];
check["declared paired sums record the runtime real-parameter requirement",
  AssociationQ[sfRealityConjugation] &&
    TrueQ[sfRealityConjugation["HelicitySummation"]["CompletePairing"]] &&
    TrueQ[sfRealityConjugation["HelicitySummation"]["ParameterRealityRequired"]]];
sfWidth = ModelParameter["FamilyWidth", "spinas-family-tests", "Real"];
sfWidthModel = sfModel;
sfWidthModel[[3]] = Append[sfWidthModel[[3]], {sfWidth, 1/15}];
sfWidthEntries = sfFour /. HoldPattern[PropDen[p_, mass_]] :> PropDen[p, mass, sfWidth];
sfWidthConjugation = sfExport["family_width_conjugation", sfWidthEntries, sfWidthModel,
  "HelicitySymmetry" -> "Conjugation"];
check["nonzero or unresolved propagator widths retain exhaustive sums",
  AssociationQ[sfWidthConjugation] &&
    !TrueQ[sfWidthConjugation["HelicitySummation"]["Active"]] &&
    StringContainsQ[sfWidthConjugation["HelicitySummation"]["Reason"], "width"]];
sfMasslessFermions = {
  {{"r", "r", "n.+", "n.+"}, sfS[3, 4]},
  {{"r", "r", "n.+", "n.-"}, SpinorChain[
     Spinor["Helicity", "Angle", 4], Mom[1], Spinor["Helicity", "Square", 3]]},
  {{"r", "r", "n.-", "n.-"}, sfA[3, 4]}};

sfChiralModel = sfGeneric;
sfChiralModel[[1]] = Join[sfChiralModel[[1]], {
  {"left Weyl", "nL", "NL", 904, 1, 0, 0, 1, "L", "n_L", "N_L"},
  {"right Weyl", "nR", "NR", 905, 1, 0, 0, 1, "R", "n_R", "N_R"}}];
sfChiralAmplitude = SpinorChain[Spinor["Helicity", "Angle", 3],
  Mom[1], Spinor["Helicity", "Square", 4]];
sfChiralEntries = {{{"r", "r", "nL", "NL"}, sfChiralAmplitude}};
sfChiral = sfExport["family_chiral", sfChiralEntries, sfChiralModel];
check["a left Weyl species needs one physical outgoing particle-antiparticle branch",
  AssociationQ[sfChiral] && sfChiral["HelicityConfigurations"] === {{0, 0, 1, -1}} &&
    sfChiral["InitialSpinAverage"] === 1 && sfChiral["FinalStateSymmetryDivisor"] === 1];
sfChiralIncoming = sfExport["family_chiral_incoming", {
  {{"nL", "NL", "r", "r"}, SpinorChain[Spinor["Helicity", "Angle", 1],
    Mom[3], Spinor["Helicity", "Square", 2]]}}, sfChiralModel];
check["incoming Weyl particle and antiparticle retain opposite helicities without a spin average",
  AssociationQ[sfChiralIncoming] &&
    sfChiralIncoming["HelicityConfigurations"] === {{-1, 1, 0, 0}} &&
    sfChiralIncoming["InitialSpinAverage"] === 1 &&
    sfChiralIncoming["FinalStateSymmetryDivisor"] === 2];
sfRightChiral = sfExport["family_right_chiral", {
  {{"r", "r", "nR", "NR"}, SpinorChain[Spinor["Helicity", "Square", 3],
    Mom[1], Spinor["Helicity", "Angle", 4]]}}, sfChiralModel];
check["right Weyl species reverse both particle and antiparticle helicities",
  AssociationQ[sfRightChiral] && sfRightChiral["HelicityConfigurations"] === {{0, 0, -1, 1}}];
sfChiralZero = sfExport["family_chiral_zero", {{{"r", "r", "nL", "NL"}, 0}}, sfChiralModel];
check["an explicit zero chiral amplitude gets its unique helicities from the model",
  AssociationQ[sfChiralZero] && sfChiralZero["HelicityConfigurations"] === {{0, 0, 1, -1}}];
sfChiralWrongPath = sfPath["family_chiral_wrong"];
sfChiralWrong = WriteSPINASProcess[sfChiralWrongPath,
  {{{"r", "r", "nL", "NL"}, SpinorChain[Spinor["Helicity", "Square", 3],
    Mom[1], Spinor["Helicity", "Angle", 4]]}}, sfChiralModel,
  "ParameterRules" -> sfRules, "SPINASDirectory" -> sfSpinas];
check["a wrong-chirality amplitude is rejected before writing files",
  FailureQ[sfChiralWrong] && !DirectoryQ[sfChiralWrongPath]];
sfChiralOptions = Join[Association[Options[WriteSPINASProcess]],
  <|"ParameterRules" -> sfRules, "CalcHEPModel" -> {FileNameJoin[{root, "model_feynman"}], 1}|>];
sfChiralIR = ConstructiveDiagrams`Private`spinasFamilyIR[
  sfChiralEntries, sfChiralModel, sfChiralOptions];
sfChiralFixedIR = ConstructiveDiagrams`Private`spinasFamilyIR[
  sfChiralEntries, sfChiralModel, Join[sfChiralOptions, <|"FixedHelicities" -> {3 -> 1}|>]];
check["a complete one-state chiral family is eligible for unpolarized CalcHEP comparison",
  AssociationQ[sfChiralIR] && TrueQ[ConstructiveDiagrams`Private`spinasCalcHEPConfiguration[
    sfChiralIR, sfChiralModel, sfChiralOptions]["Supported"]]];
check["selecting the only physical chiral helicity is not a polarization restriction",
  AssociationQ[sfChiralFixedIR] && TrueQ[ConstructiveDiagrams`Private`spinasCalcHEPConfiguration[
    sfChiralFixedIR, sfChiralModel, sfChiralOptions]["Supported"]]];
sfChiralSingleIR = ConstructiveDiagrams`Private`spinasProcessIR[
  First[First[sfChiralEntries]], sfChiralAmplitude, sfChiralModel, sfChiralOptions];
check["the single-formula overload can compare a complete chiral process with CalcHEP",
  AssociationQ[sfChiralSingleIR] && TrueQ[ConstructiveDiagrams`Private`spinasCalcHEPConfiguration[
    sfChiralSingleIR, sfChiralModel, sfChiralOptions]["Supported"]]];

sfCompiler = sfRun[{"c++", "--version"}];
sfLibraries = Select[FileNameJoin[{sfSpinas, #, "libspinas.a"}] & /@ {"build", "lib"}, FileExistsQ];
sfNativeAvailable = !sfFailed[sfCompiler] && sfLibraries =!= {} &&
  FileExistsQ[FileNameJoin[{sfSpinas, "include", "spinas.h"}]] &&
  AllTrue[{"eeAA.cpp", "AAee.cpp"}, FileExistsQ[FileNameJoin[{sfSpinas, "SM", #}]] &];
sfOracle[report_Association, name_String, kind_Integer, ranks_List, helicities_Integer,
    average_Integer, divisor_Integer] := Module[
  {directory = report["Directory"], main, executable, source, command, compiled, ran,
   precision = {}, cache, manifest, names, plus = -1, minus = -1},
  manifest = Import[report["Files"]["Manifest"], "RawJSON"];
  names = manifest["ParameterOrder"];
  If[kind === 5,
    plus = First[FirstPosition[names, "FamilyGP", {0}]] - 1;
    minus = First[FirstPosition[names, "FamilyGM", {0}]] - 1];
  If[kind === 9, plus = First[FirstPosition[names, "FamilyGP", {0}]] - 1];
  main = StringRiffle[{
    "#include \"process.hpp\"", "#include \"spinas-family-reference.hpp\"",
    "int main() { try { return hepcat_family_reference::run<hepcat_generated::" <> name <> ">(" <>
      StringRiffle[Join[{ToString[kind], "std::array<int," <> ToString[Length[ranks]] <> ">{{" <>
        StringRiffle[ToString /@ ranks, ","] <> "}}"},
        ToString /@ {helicities, average, divisor, plus, minus}], ","] <> "); }",
    "catch (const std::exception& e) { std::cerr << e.what() << '\\n'; return 2; } }"}, "\n"] <> "\n";
  source = FileNameJoin[{directory, "family_oracle.cpp"}];
  executable = FileNameJoin[{directory, "family_oracle"}]; Export[source, main, "Text"];
  cache = FileNameJoin[{sfSpinas, "build", "CMakeCache.txt"}];
  If[FileExistsQ[cache] && StringContainsQ[Import[cache, "Text"], "PRECISION_TYPE:STRING=LONG_DOUBLE"],
    precision = {"-DWITH_LONG_DOUBLE"}];
  command = Join[{"c++", "-std=c++11", "-O1"}, precision,
    {"-I" <> directory, "-I" <> FileNameJoin[{root, "tests", "checks"}],
     "-I" <> FileNameJoin[{sfSpinas, "include"}], "-I" <> sfSpinas,
     source, report["Files"]["Source"], FileNameJoin[{sfSpinas, "SM", "eeAA.cpp"}],
     FileNameJoin[{sfSpinas, "SM", "AAee.cpp"}], First[sfLibraries], "-o", executable}];
  compiled = sfRun[command];
  If[sfFailed[compiled], Print["FAMILY ORACLE COMPILE ", name, ": ", compiled]; Return[$Failed]];
  ran = sfRun[{executable}]; If[sfFailed[ran], Print["FAMILY ORACLE RUN ", name, ": ", ran]]; ran
];
sfBuildAndCompare[name_String, records_List, model_, kind_Integer, ranks_List,
    helicities_Integer, average_Integer, divisor_Integer] := Module[{report, valid, oracle},
  Print["BUILDING FAMILY ", name];
  report = sfExport[name, records, model, "Build" -> True, "RunExamples" -> True];
  valid = AssociationQ[report] && Lookup[Lookup[report, "Build", <||>], "Status", None] === "BUILT" &&
    Lookup[Lookup[report, "Examples", <||>], "Status", None] === "RAN" &&
    Lookup[Lookup[report, "Examples", <||>], "ExitCode", -1] === 0;
  check["family project compiles, links and runs physical examples: " <> name, valid];
  If[!valid, Print["FAMILY EXPORT DETAILS: ", report]; Return[report]];
  check["family normalization metadata matches native conventions: " <> name,
    Length[report["HelicityConfigurations"]] === helicities &&
      report["InitialSpinAverage"] === average && report["FinalStateSymmetryDivisor"] === divisor &&
      report["SymmetryFactor"] === 1/divisor];
  If[kind === 5, check["parameter arrays contain the union across both helicity branches",
    With[{order = Import[report["Files"]["Manifest"], "RawJSON"]["ParameterOrder"]},
      Count[order, "FamilyGP"] === 1 && Count[order, "FamilyGM"] === 1 &&
        Count[order, "Me"] === 1]]];
  oracle = sfOracle[report, name, kind, ranks, helicities, average, divisor];
  check["independent complete spin sums and complex amplitudes agree: " <> name,
    !sfFailed[oracle] && StringContainsQ[oracle["StandardOutput"], "FAMILY_COMPARISONS="]]; report
];

If[sfNativeAvailable,
  sfConjugationOracle = sfOracle[sfConjugation, "family_conjugation", 1,
    {1, 1, 2, 2}, 4, 4, 2];
  check["paired native sums equal exhaustive public amplitudes for every helicity",
    !sfFailed[sfConjugationOracle] &&
      StringContainsQ[sfConjugationOracle["StandardOutput"], "FAMILY_COMPARISONS=48"]];
  sfCanonicalOracle = sfOracle[sfCanonicalConjugation, "p1", 1,
    {1, 1, 2, 2}, 4, 4, 2];
  check["paired canonical sums equal exhaustive public amplitudes for every helicity",
    !sfFailed[sfCanonicalOracle] &&
      StringContainsQ[sfCanonicalOracle["StandardOutput"], "FAMILY_COMPARISONS=48"]];
  sfRealityOracle = sfOracle[sfRealityConjugation, "family_reality_guard", 9,
    {1, 1, 2, 0}, 2, 4, 1];
  check["real parameter pairing and complex parameter fallback both match the full sum",
    !sfFailed[sfRealityOracle] &&
      StringContainsQ[sfRealityOracle["StandardOutput"], "FAMILY_COMPARISONS=48"]];
  sfZeroOracle = sfOracle[sfExplicitZero, "explicit_zero_sectors", 7, {1, 1, 2, 2}, 4, 4, 2];
  check["every explicit zero helicity compiles and has zero amplitude and spin sum",
    !sfFailed[sfZeroOracle] &&
      StringContainsQ[sfZeroOracle["StandardOutput"], "FAMILY_COMPARISONS=48"]];
  sfReports = {
    sfBuildAndCompare["family_eeAA", sfFour, sfModel, 1, {1, 1, 2, 2}, 4, 4, 2],
    sfBuildAndCompare["family_AAee", sfIncoming, sfModel, 2, {2, 2, 1, 1}, 4, 4, 1],
    sfBuildAndCompare["family_eeAAA", sfFive, sfModel, 3, {1, 1, 2, 2, 2}, 8, 4, 6],
    sfBuildAndCompare["family_six_fermions", sfSixFermions, sfModel, 4,
      ConstantArray[1, 6], 1, 4, 4],
    sfBuildAndCompare["family_parameters", sfCouplings, sfGeneric, 5, {1, 1, 2, 0}, 2, 4, 1],
    sfBuildAndCompare["family_massless_fermions", sfMasslessFermions, sfGeneric, 6,
      {0, 0, 1, 1}, 4, 1, 2]},
  Print["SKIP: native family compile/run checks need a C++ compiler and built SPINAS library."]];
check["compiled family export leaves user models and global parameters unchanged",
  sfModel === sfModelBefore && {OwnValues[Global`Me], OwnValues[Global`EE]} === sfGlobalsBefore];
Print["SPINAS HELICITY FAMILY CHECKS: ", checks, "; FAILURES: ", Length[failures]];
If[failures === {}, DeleteDirectory[sfDirectory, DeleteContents -> True];
  Print["HEPCAT_TEST_COMPLETED:spinas-helicity-family-tests.wl"],
  Print[failures]; Print["FAMILY TEST FILES: ", sfDirectory]];
Quit[If[failures === {}, 0, 1]];
