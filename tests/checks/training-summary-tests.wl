root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root, "tests", "training-notebook-helpers.wl"}]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]]);
check["helper does not create global key symbols", Names["Global`key"] === {} && Names["Global`key$"] === {}];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
loadOK = Check[Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]]; True, False];
check["physics loads after helper without warnings", loadOK];
reloadOK = Check[Get[FileNameJoin[{root, "tests", "training-notebook-helpers.wl"}]]; True, False];
check["helper loads after physics without warnings", reloadOK];
check["helper internals remain private", Names["Global`key"] === {} && Names["Global`key$"] === {}];
SetEnvironment["HEPCAT_TRAIN_SCRAMBLES" -> "12"];
check["environment override", trainingIntegerSetting["SCRAMBLES", 4] === 12];
SetEnvironment["HEPCAT_TRAIN_SCRAMBLES" -> "invalid"];
check["invalid environment uses default", trainingIntegerSetting["SCRAMBLES", 4] === 4];
SetEnvironment["HEPCAT_TRAIN_HOLDOUT" -> "0"];
check["zero holdout", trainingIntegerSetting["HOLDOUT", 1] === 0];
settings = <|"Amplitudes" -> 4, "Scrambles" -> 12, "Steps" -> 5, "Rounds" -> 2|>;
report = <|"TrainingBatchSize" -> 1, "Kernels" -> 2, "TrainingStates" -> 100,
  "TrainingRows" -> 400, "ReverseSteps" -> 99, "SkippedReverseSteps" -> 1,
  "CacheHits" -> 2, "GenerationJobs" -> {1, 2, 3}, "TotalWallSeconds" -> 90,
  "WorkerSetupSeconds" -> 2, "DataGenerationSeconds" -> 10, "TrainingSeconds" -> 60,
  "ValidationSeconds" -> 15, "TrainingCPUSeconds" -> 120, "HoldoutScrambles" -> 12,
  "HoldoutSimpler" -> 6, "HoldoutRecovered" -> 3, "ModelPath" -> "/tmp/example.wlnet"|>;
summary = trainingRunSummary[report, settings];
check["validation percentages", StringContainsQ[summary, "6 / 12 (50.0%)"] && StringContainsQ[summary, "3 / 12 (25.0%)"]];
check["wall time in seconds and minutes", StringContainsQ[summary, "90.0 s (1.5 min)"]];
check["disabled validation", StringContainsQ[trainingRunSummary[Join[report, <|"HoldoutScrambles" -> 0|>], settings], "not measured"]];
check["failed run is explicit", StringStartsQ[trainingRunSummary[$Failed, settings], "TRAINING FAILED"]];
notebook = Get[FileNameJoin[{root, "tests", "train-unscrambling.nb"}]];
inputs = Cases[notebook, Cell[s_String, "Input", ___] :> s, Infinity];
SetEnvironment[{"HEPCAT_TRAIN_SCRAMBLES" -> "12", "HEPCAT_TRAIN_ROUNDS" -> "3",
  "HEPCAT_TRAIN_ROOT" -> root,
  "HEPCAT_TRAIN_BATCH_SIZE" -> "2", "HEPCAT_TRAIN_AMPLITUDES" -> "6"}];
ToExpression[First[inputs]];
check["notebook loads overrides", {nScrambles, nRounds, trainingBatchSize, nAmplitudes, nHoldOut} === {12, 3, 2, 6, 0}];
SetEnvironment["HEPCAT_TRAIN_AMPLITUDES" -> "all"];
check["all amplitude setting", trainingAmplitudeSetting[] === All];
ToExpression[First[inputs]];
ToExpression[inputs[[2]]];
ToExpression[inputs[[3]]];
check["notebook loads all fifteen curated channels without failures",
  Unscrambling`Private`validSimplificationRoutesQ[simplificationRoutes] &&
    Length[simplificationRoutes] === 15 && DuplicateFreeQ[Lookup[simplificationRoutes, "Name"]] &&
    AllTrue[simplificationRoutes, Length[#["States"]] > 1 &] &&
    FreeQ[simplificationRoutes, $Failed]];
check["handwritten Z identities are decomposed into atomic teaching steps",
  Length[First[simplificationRoutes]["States"]] === 97 &&
    Counts[First[simplificationRoutes]["Identities"]] ===
      <|"MomentumSum" -> 12, "Mass" -> 52, "Schouten" -> 16,
        "Anticommutation" -> 4, "OnShell" -> 4, "MomentumConservation" -> 8|>];
check["handwritten eeZh channels retain their individual elementary paths",
  Length /@ Lookup[Take[simplificationRoutes, {2, 4}], "States"] === {53, 11, 5} &&
    (Counts[#["Identities"]] &) /@ Take[simplificationRoutes, {2, 4}] === {
      <|"MomentumSum" -> 6, "Mass" -> 26, "Schouten" -> 8,
        "Anticommutation" -> 2, "OnShell" -> 2, "MomentumConservation" -> 8|>,
      <|"MomentumSum" -> 2, "Mass" -> 6, "MomentumConservation" -> 2|>,
      <|"MomentumSum" -> 2, "Mass" -> 2|>}];
check["eeZZ electron channels each teach two splits and two mass contractions",
  Length /@ Lookup[Take[simplificationRoutes, {5, 6}], "States"] === {5, 5} &&
    (Counts[#["Identities"]] &) /@ Take[simplificationRoutes, {5, 6}] ===
      ConstantArray[<|"MomentumSum" -> 2, "Mass" -> 2|>, 2]];
eewwRoute = SelectFirst[simplificationRoutes, #["Name"] === "e, E, W-, W+ / Z exchange" &, $Failed];
check["eeWW Z exchange retains its full elementary simplification route",
  AssociationQ[eewwRoute] && Length[eewwRoute["States"]] === 303 &&
    Counts[eewwRoute["Identities"]] ===
      <|"MomentumSum" -> 32, "Mass" -> 172, "Schouten" -> 40,
        "Anticommutation" -> 8, "OnShell" -> 8, "MomentumConservation" -> 42|>];
chargedCurrentRoutes = Select[simplificationRoutes,
  MemberQ[{"ne, E, W-, h / W+ exchange", "u, D, W-, h / W+ exchange"}, #["Name"]] &];
check["charged-current channels retain their complete elementary routes",
  Length /@ Lookup[chargedCurrentRoutes, "States"] === {19, 27} &&
    (Counts[#["Identities"]] &) /@ chargedCurrentRoutes === {
      <|"MomentumSum" -> 3, "Mass" -> 8, "Schouten" -> 3, "Anticommutation" -> 1,
        "OnShell" -> 1, "MomentumConservation" -> 2|>,
      <|"MomentumSum" -> 3, "Mass" -> 13, "Schouten" -> 4, "Anticommutation" -> 1,
        "OnShell" -> 1, "MomentumConservation" -> 4|>}];
zhwwRoute = SelectFirst[simplificationRoutes, #["Name"] === "Z, h, W-, W+ / Z exchange" &, $Failed];
check["bosonic Z exchange retains its full route and the source's accepted sign",
  AssociationQ[zhwwRoute] && Length[zhwwRoute["States"]] === 173 &&
    zhwwRoute["TemplateSign"] === -1 && Counts[zhwwRoute["Identities"]] ===
      <|"MomentumSum" -> 16, "Mass" -> 108, "MomentumConservation" -> 28,
        "Schouten" -> 12, "Anticommutation" -> 4, "OnShell" -> 4|>];
newCuratedRoutes = Take[simplificationRoutes, {11, 15}];
check["new curated channels follow the original ten in their declared order",
  Lookup[newCuratedRoutes, "Name"] === {
    "u, D, T, b / W+ exchange", "E, ne, Z, W- / W+ exchange",
    "U, d, Z, W+ / W+ exchange", "Z, h, W-, W+ / T-channel W exchange",
    "Z, h, W-, W+ / U-channel W exchange"}];
check["four-fermion W exchange retains every elementary teaching step",
  Length[newCuratedRoutes[[1]]["States"]] === 25 &&
    Counts[newCuratedRoutes[[1]]["Identities"]] ===
      <|"MomentumSum" -> 3, "Mass" -> 13, "Schouten" -> 4,
        "Anticommutation" -> 1, "OnShell" -> 1, "MomentumConservation" -> 2|>];
check["charged-current ZW channels retain their complete elementary routes",
  Length /@ Lookup[Take[newCuratedRoutes, {2, 3}], "States"] === {110, 156} &&
    (Counts[#["Identities"]] &) /@ Take[newCuratedRoutes, {2, 3}] === {
      <|"MomentumSum" -> 16, "Mass" -> 58, "Schouten" -> 14,
        "Anticommutation" -> 4, "OnShell" -> 4, "MomentumConservation" -> 13|>,
      <|"MomentumSum" -> 16, "Mass" -> 86, "Schouten" -> 24,
        "Anticommutation" -> 4, "OnShell" -> 4, "MomentumConservation" -> 21|>}];
check["bosonic W channels retain their routes and the source's accepted sign",
  Length /@ Lookup[Take[newCuratedRoutes, {4, 5}], "States"] === {73, 73} &&
    Lookup[Take[newCuratedRoutes, {4, 5}], "TemplateSign"] === {-1, -1} &&
    (Counts[#["Identities"]] &) /@ Take[newCuratedRoutes, {4, 5}] === ConstantArray[
      <|"MomentumSum" -> 16, "Mass" -> 36, "Schouten" -> 12,
        "Anticommutation" -> 4, "OnShell" -> 4|>, 2]];
allCuratedJobs = Unscrambling`Private`makeSimplificationRouteJobs[simplificationRoutes, All];
check["fifteen curated channels each receive all twenty-four particle permutations",
  Length[allCuratedJobs] === 360 &&
    Sort[Values[Counts[Lookup[allCuratedJobs, "Name"]]]] === ConstantArray[24, 15] &&
    DuplicateFreeQ[Lookup[allCuratedJobs, {"Name", "LegPermutation"}]]];
independentRoutes = Block[{ReduceSpinorProducts, UnscrambleSpinorAmplitudes, spPr, rsp,
    mReplacments = {Mass[1] -> 999}},
  Clear[ReduceSpinorProducts, UnscrambleSpinorAmplitudes, spPr, rsp];
  ReduceSpinorProducts[___] := $Failed;
  UnscrambleSpinorAmplitudes[___] := $Failed;
  spPr[___] := $Failed; rsp[___] := $Failed;
  trainingSimplificationRoutes
];
check["curated recipe does not depend on the NN, legacy reducer, or notebook globals",
  independentRoutes === simplificationRoutes];
check["notebook selects complete corpus", nAmplitudes === 243 && Length[trainingAmplitudes] === 243];
check["exact pairs deduplicated", DuplicateFreeQ[knownAmplitudes]];
check["reference external mass rules retained", AllTrue[Take[knownAmplitudes, 195],
  MatchQ[#[[1]], {Mass[1] -> _, Mass[2] -> _, Mass[3] -> _, Mass[4] -> _}] &]];
corpusSources = Flatten[Lookup[amplitudeCatalog, "Sources"]];
check["both notebook reference collections retained", Length[corpusSources] === 385 &&
  Count[corpusSources, s_String /; StringStartsQ[s, "SM-4-point.nb / "]] === 189 &&
  Count[corpusSources, s_String /; StringStartsQ[s, "SM-4-point-test.nb / "]] === 148];
syntheticFivePoint = Drop[amplitudeCatalog, 195];
check["synthetic five-point provenance is explicit", Length[syntheticFivePoint] === 48 &&
  AllTrue[syntheticFivePoint, TrueQ[#["Synthetic"]] && #["ExternalLegCount"] === 5 &&
    StringStartsQ[#["Name"], "Synthetic5Point / "] &]];
check["synthetic five-point external mass rules", AllTrue[syntheticFivePoint,
  Cases[#["Masses"], HoldPattern[Mass[i_Integer] -> _] :> i] === Range[5] &]];
check["synthetic five-point labels occur in numerators", AllTrue[syntheticFivePoint,
  Keys[Unscrambling`Private`stateLegIDs[#["Amplitude"] /. _PropDen -> 1, {}]] === Range[5] &]];
check["synthetic five-point chains have legal chirality", AllTrue[syntheticFivePoint,
  AllTrue[Cases[#["Amplitude"], _SpinorChain, Infinity], Unscrambling`Private`validChainQ] &]];
check["synthetic five-point mass families are diverse",
  Sort[Values[Counts[Lookup[syntheticFivePoint, "Family"]]]] === ConstantArray[8, 6]];
check["synthetic five-point algebraic templates are diverse",
  Sort[Values[Counts[Lookup[syntheticFivePoint, "Template"]]]] === ConstantArray[6, 8]];
(* Check real legal moves and teacher labels without constructing or training a net. *)
syntheticMoveAudit = TimeConstrained[Module[
  {pairs = Lookup[syntheticFivePoint, {"Masses", "Amplitude"}], families = {},
    starts = {}, steps = {}, scrambles = {}, before, rules, legal, trajectory,
    labels, rows, row, reachable, exact, count},
  Do[
    rules = pairs[[i, 1]];
    before = Unscrambling`Private`canonicalChains[pairs[[i, 2]] /. rules];
    legal = Unscrambling`Private`legalMoves[before, rules];
    families = Union[families, Lookup[legal, "Name"]];
    AppendTo[starts, Count[Lookup[legal, "Name"], Except["Stop"]] > 0];
    trajectory = Unscrambling`Private`scramblePair[pairs[[i]], 3, 1000 + i, True]["Trajectory"];
    AppendTo[scrambles, Length[trajectory]];
    Do[
      legal = Unscrambling`Private`legalMoves[step["After"], rules];
      count = Length[legal];
      labels = Boole[Unscrambling`Private`samePoly[
        Unscrambling`Private`applyCandidate[step["After"], #], step["Before"]]] & /@ legal;
      reachable = MemberQ[labels, 1];
      rows = Block[{Unscrambling`Private`$policyMethod = "ReverseMoves"},
        Unscrambling`Private`trainingRows[step["After"], rules, step["Before"]]];
      exact = If[reachable,
        If[MatchQ[rows, {_Association}], row = First[rows];
          row["ActionCount"] === count &&
          row["Target"] === N[PadRight[labels, Unscrambling`Private`$moveSlots]/Total[labels]] &&
          row["LegalMask"] === N[PadRight[ConstantArray[1, count], Unscrambling`Private`$moveSlots]], False],
        rows === {}];
      AppendTo[steps, <|"Reference" -> i, "Reachable" -> reachable, "ExactLabel" -> exact|>],
      {step, trajectory}],
    {i, Length[pairs]}];
  <|"MoveFamilies" -> families, "NonStopStarts" -> starts, "ScrambleSteps" -> scrambles,
    "RecordedSteps" -> Length[steps], "ReachableSteps" -> Count[Lookup[steps, "Reachable"], True],
    "SkippedSteps" -> Count[Lookup[steps, "Reachable"], False],
    "EveryReferenceHasLabels" -> Sort[DeleteDuplicates[Lookup[Select[steps, TrueQ[#["Reachable"]] &], "Reference"]]] === Range[Length[pairs]],
    "ExactTeacherLabels" -> AllTrue[Lookup[steps, "ExactLabel"], TrueQ]|>
], 60, $Failed];
check["synthetic five-point move audit finishes within one minute", AssociationQ[syntheticMoveAudit]];
If[AssociationQ[syntheticMoveAudit],
  check["synthetic five-point starts have non-stop actions", And @@ syntheticMoveAudit["NonStopStarts"]];
  check["synthetic five-point starts cover all move families",
    syntheticMoveAudit["MoveFamilies"] === Sort[{"Schouten", "Stop", "MomentumConservation",
      "Anticommutation", "MomentumSquare", "MomentumSum", "OnShell", "Mass", "ChainSquare"}]];
  check["synthetic five-point three-step scrambles complete", syntheticMoveAudit["ScrambleSteps"] === ConstantArray[3, 48]];
  check["synthetic five-point teacher labels exactly match legal moves", syntheticMoveAudit["ExactTeacherLabels"]];
  check["synthetic five-point references supply reachable teacher labels", syntheticMoveAudit["EveryReferenceHasLabels"]];
  Print[KeyDrop[syntheticMoveAudit, {"NonStopStarts", "ScrambleSteps"}]]
];
check["main-only ZZWW channels retained", MemberQ[corpusSources, "SM-4-point.nb / input 1103 / target 1"] &&
  MemberQ[corpusSources, "SM-4-point.nb / input 1112 / target 1"]];
check["no unresolved reference functions", FreeQ[knownAmplitudes,
  s_Symbol /; StringMatchQ[SymbolName[s], "amp*Tested*" | "spPr" | "rsp"]]];
check["no failed reference evaluations", FreeQ[knownAmplitudes, $Failed | $Aborted | Indeterminate | ComplexInfinity]];
nAmplitudes = 6;
ToExpression[inputs[[3]]];
check["numeric subset still works", Length[trainingAmplitudes] === 6];
check["notebook prints readable summary", StringContainsQ[Last[inputs], "Print[trainingRunSummary["] && !StringContainsQ[Last[inputs], "Print[report]"]];
Print[summary];
Print[<|"Checks" -> checks, "Failed" -> Length[failures], "Failures" -> failures|>];
Quit[If[failures === {}, 0, 1]];
