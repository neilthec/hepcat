(* Expensive opt-in public SM constructions, with independent Feynman rules.
   No amplitude or correction basis is supplied to the constructor.
   HEPCAT_SM_CHARGED_CURRENT_CASES accepts one, both, UdZWPlus, DuZWMinus,
   or comma-separated case names. Default: both.
   HEPCAT_SM_CHARGED_CURRENT_SEEDS accepts up to eight comma-separated
   nonnegative integer seeds. Default: the documented reproducer seed.
   The external test runner controls the timeout for the selected batch. *)
scciRoot = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{scciRoot, "source", "HEPCAT.wl"}]];
$HistoryLength = 0;
scciChecks = 0; scciFailures = {}; scciMetrics = {};
SetAttributes[scciCheck, HoldRest];
scciCheck[name_, assertion_] := (scciChecks++;
  If[!TrueQ[assertion], AppendTo[scciFailures, name]; Print["FAIL: ", name]]);
scciCases = <|"UdZWPlus" -> {"U", "d", "Z", "W+"},
  "DuZWMinus" -> {"D", "u", "Z", "W-"}|>;
scciSelection = Environment["HEPCAT_SM_CHARGED_CURRENT_CASES"];
scciSelection = Which[
  MemberQ[{$Failed, "", "both"}, scciSelection], Keys[scciCases],
  scciSelection === "one", {"UdZWPlus"},
  StringQ[scciSelection], DeleteDuplicates[StringTrim /@ StringSplit[scciSelection, ","]],
  True, {}];
If[scciSelection === {} || !AllTrue[scciSelection, KeyExistsQ[scciCases, #] &],
  Print["Choose one, both, UdZWPlus or DuZWMinus for HEPCAT_SM_CHARGED_CURRENT_CASES."]; Quit[2]];
scciSeedText = Environment["HEPCAT_SM_CHARGED_CURRENT_SEEDS"];
scciSeeds = If[MemberQ[{$Failed, ""}, scciSeedText], {"2026110611"},
  StringTrim /@ StringSplit[scciSeedText, ","]];
If[scciSeeds === {} || Length[scciSeeds] > 8 ||
    !AllTrue[scciSeeds, StringMatchQ[#, DigitCharacter ..] &],
  Print["Provide at most eight nonnegative integer charged-current seeds."]; Quit[2]];
scciSeeds = DeleteDuplicates[FromDigits /@ scciSeeds];
scciModel = ReadModel[FileNameJoin[{scciRoot, "models"}], 1];
scciFeynmanModel = ReadModel[FileNameJoin[{scciRoot, "model_feynman"}], 1];
scciModelsBefore = {scciModel, scciFeynmanModel};
scciDiagnosticDirectory = CreateDirectory[];
Print["SM CHARGED-CURRENT REPORTS: ", scciDiagnosticDirectory];
scciValidate[report_, process_, seed_, label_] := Module[
  {complete, checks, physical, benchmark, ordinary, coefficients, pair, phase, validBenchmark},
  complete = AssociationQ[report] && Lookup[report, "Status", None] === "COMPLETE";
  scciCheck[label <> ": public construction completes with automatic resources", complete];
  If[!complete, Return[Null]];
  checks = Lookup[Lookup[report, "Checks", <||>], "Checks", {}];
  physical = Select[checks, Lookup[#, "Type", None] === "Physical" &];
  benchmark = Select[checks, Lookup[#, "Type", None] === "Feynman" &];
  ordinary = Select[checks, Lookup[#, "Type", None] =!= "Feynman" &];
  coefficients = Lookup[report, "ColorCoefficientReports", {report}];
  pair = Lookup[report, "AmplitudePair", Missing["NoAmplitudePair"]];
  phase = Lookup[report["Checks"], "FeynmanPhaseAgreement", Missing["NoColorPhase"]];
  validBenchmark[row_] := Lookup[row, "Status", None] === "PASS" &&
    MemberQ[Lookup[row, "MatchingSigns", {}], 1] &&
    Lookup[row, "OverallSign", None] === 1;
  scciCheck[label <> ": the reported process and reusable amplitude pair are intact",
    report["Process"] === process && MatchQ[pair, {{__Rule}, _}] &&
      Last[pair] === report["Amplitude"] && FreeQ[First[pair], _Multiparticle] &&
      AllTrue[First[pair], MatchQ[#, HoldPattern[Mass[_Integer] -> _]] &]];
  scciCheck[label <> ": every physical channel passes without a sign adjustment",
    Length[physical] === 3 && AllTrue[physical, Lookup[#, "Status", None] === "PASS" &] &&
      AllTrue[physical, Lookup[#, "CompletedSamples", 0] >= 4 &]];
  scciCheck[label <> ": high-energy and reference checks are present and pass",
    AnyTrue[ordinary, Lookup[#, "Type", None] === "HighEnergy" && Lookup[#, "Status", None] === "PASS" &] &&
      AnyTrue[ordinary, Lookup[#, "Type", None] === "Reference" && Lookup[#, "Status", None] === "PASS" &]];
  scciCheck[label <> ": no mandatory non-Feynman check is failed or unresolved",
    ordinary =!= {} && AllTrue[ordinary, Lookup[#, "Status", None] === "PASS" &]];
  scciCheck[label <> ": independent Feynman comparison is complete in the same model-wide convention",
    Length[benchmark] === Length[coefficients] && benchmark =!= {} && AllTrue[benchmark,
      validBenchmark[#] && Lookup[#, "CompletedSamples", 0] >= 4 &&
        Lookup[#, "NormalizationFitted", None] === False &&
        Lookup[#, "Method", None] === "ExactSampledFullAmplitudeSpinPolynomialComparison" &]];
  scciCheck[label <> ": all color coefficients share one overall Feynman convention",
    AssociationQ[phase] && Lookup[phase, "Status", None] === "PASS" &&
      Intersection @@ Lookup[benchmark, "MatchingSigns"] === phase["MatchingSigns"]];
  scciCheck[label <> ": automatic pole discovery and every discovered spurious test are retained",
    AllTrue[coefficients, TrueQ[Lookup[#["Checks"], "AutomaticPoleDiscovery", False]] &&
      ListQ[Lookup[#["Checks"], "SpuriousChecks", None]] &&
      AllTrue[#["Checks"]["SpuriousChecks"], Lookup[#, "Status", None] === "PASS" &] &]];
  scciCheck[label <> ": the seed remains available for exact replay",
    AllTrue[coefficients, Lookup[#, "RandomSeed", None] === seed &]];
  scciCheck[label <> ": the expression is finite and its complexity is measured consistently",
    FreeQ[report["Amplitude"], _Missing | _Failure | $Failed | $Aborted | Indeterminate |
      ComplexInfinity | _DirectedInfinity] &&
      report["Complexity"] === SpinorExpressionComplexity[report["Amplitude"]]];
  AppendTo[scciMetrics, <|"Case" -> label, "Complexity" -> report["Complexity"],
    "LeafCount" -> LeafCount[report["Amplitude"]], "Summary" -> report["Checks"]["Summary"],
    "FeynmanSigns" -> phase["MatchingSigns"]|>];
];
Do[
  scciLabel = scciCase <> "@" <> ToString[scciSeed]; scciStarted = AbsoluteTime[];
  Print["SM CHARGED-CURRENT BEGIN: ", scciLabel];
  scciReport = ConstructAmplitude[scciCases[scciCase], scciModel, scciModel,
    "FeynmanModel" -> scciFeynmanModel, "RandomSeed" -> scciSeed, "Output" -> "Details",
    "Parallelization" -> False, "ProgressReporting" -> False, "PrintReport" -> False];
  scciElapsed = AbsoluteTime[] - scciStarted;
  Export[FileNameJoin[{scciDiagnosticDirectory, scciLabel <> ".wxf"}], scciReport, "WXF"];
  scciValidate[scciReport, scciCases[scciCase], scciSeed, scciLabel];
  Print["SM CHARGED-CURRENT RESULT: ", InputForm[<|"Case" -> scciLabel,
    "WallSeconds" -> scciElapsed, "Status" -> If[AssociationQ[scciReport],
      Lookup[scciReport, "Status", None], Head[scciReport]]|>]],
  {scciCase, scciSelection}, {scciSeed, scciSeeds}];
scciCheck["all model data remain unchanged", scciModelsBefore === {scciModel, scciFeynmanModel}];
Export[FileNameJoin[{scciDiagnosticDirectory, "metrics.wxf"}], scciMetrics, "WXF"];
Print["SM CHARGED-CURRENT INTEGRATION CHECKS: ", scciChecks, "; FAILURES: ", Length[scciFailures]];
If[scciFailures === {}, Print["HEPCAT_TEST_COMPLETED:sm-charged-current-integration-tests.wl"], Print[scciFailures]];
Quit[If[scciFailures === {}, 0, 1]];
