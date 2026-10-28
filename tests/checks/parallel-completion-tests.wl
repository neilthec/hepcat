(* Parallel exact sample construction leaves simultaneous fitting unchanged. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
Get[FileNameJoin[{root, "tests", "QED-5-point-residues.wl"}]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value],
  AppendTo[failures, name]; Print["FAIL: ", name]]);
pcRules = ConstructiveDiagrams`Private`residueParameterRules[prFixtureModel,
  {prElectronMass -> 1, prElectricCharge -> 1}];
pcNormalized = ConstructiveDiagrams`Private`completionNormalizeInput[
  prH, prChannels, prFixtureModel];
pcVariables = {Unique["parallelFitA"], Unique["parallelFitB"]};
pcBasis = {prH, First[List @@ Expand[prH]]};
pcTrial = 2 prH + pcVariables.pcBasis;
pcSettings[parallel_] := <|"Parallelization" -> parallel, "MaxKernels" -> 2|>;
pcConstraints[parallel_] := Block[
  {ConstructiveDiagrams`Private`$completionParallelSettings = pcSettings[parallel]},
  ConstructiveDiagrams`Private`completionResidueConstraints[pcTrial, pcVariables,
    pcNormalized, 2, 829017, pcRules, 60, Automatic, False]];
check["both completion APIs expose the same worker controls",
  AllTrue[{CompleteFactorization, CompleteHighEnergy},
    ("Parallelization" /. Options[#]) === Automatic &&
      ("MaxKernels" /. Options[#]) === Automatic &]];
pcSerial = pcConstraints[False];
pcParallel = pcConstraints[True];
check["serial and parallel constraints complete every channel and sample",
  pcSerial["Status"] === "COMPLETE" && pcParallel["Status"] === "COMPLETE" &&
    Length[pcParallel["Samples"]] === 2 Length[pcNormalized["Targets"]]];
check["independent jobs return the identical exact simultaneous equations",
  pcSerial["Equations"] === pcParallel["Equations"]];
check["fit samples preserve the original channel and sample ordering",
  (KeyTake[#, {"Cut", "Sample"}] & /@ pcSerial["Samples"]) ===
    (KeyTake[#, {"Cut", "Sample"}] & /@ pcParallel["Samples"])];
pcChartData[samples_List] := Map[Function[entry, With[{chart = entry["Chart"]},
  KeyTake[chart, {"Momenta", "AngleBasis", "SquareBasis", "References", "Masses",
    "MasslessBranchChoices", "MasslessLittleGroupScales", "Cut", "Complement",
    "CutMass", "ReferenceSeed"}] /. chart["PoleVariable"] -> parallelTestPole]], samples];
check["workers use exactly the original deterministic cut charts",
  pcChartData[pcSerial["Samples"]] === pcChartData[pcParallel["Samples"]]];
check["a requested two-worker fit uses two real workers",
  pcParallel["Execution"]["Mode"] === "Parallel" &&
    pcParallel["Execution"]["KernelCount"] === 2];
pcSolution = <|"SolutionRules" -> Thread[pcVariables -> {-1, 0}], "FreeParameters" -> {}|>;
pcValidate[parallel_, solution_] := Block[
  {ConstructiveDiagrams`Private`$completionParallelSettings = pcSettings[parallel]},
  ConstructiveDiagrams`Private`completionSparseValidation[pcTrial, pcVariables,
    solution, {}, {}, pcNormalized, 2, 1829020, pcRules, 60, Automatic, False]];
pcSerialValidation = pcValidate[False, pcSolution];
pcParallelValidation = pcValidate[True, pcSolution];
check["held-out validation checks all original targets and coherent spins",
  pcParallelValidation["FamilyPass"] === True &&
    pcParallelValidation["Feedback"]["Status"] === "COMPLETE" &&
    Counts[Lookup[pcParallelValidation["Report"]["Channels"], "Status"]] ===
      <|"PASS" -> Length[pcNormalized["Targets"]]|>];
check["held-out feedback retains identical raw equations for later refinements",
  pcSerialValidation["Feedback"]["Equations"] ===
    pcParallelValidation["Feedback"]["Equations"]];
check["held-out charts and sample seeds do not depend on scheduling",
  pcChartData[pcSerialValidation["Feedback"]["Samples"]] ===
    pcChartData[pcParallelValidation["Feedback"]["Samples"]] &&
  Lookup[pcSerialValidation["Feedback"]["Samples"], "RandomSeed"] ===
    Lookup[pcParallelValidation["Feedback"]["Samples"], "RandomSeed"]];
check["zero-fitted directions remain in the raw validation constraints",
  !FreeQ[pcParallelValidation["Feedback"]["Equations"], Last[pcVariables]]];
pcWrong = pcValidate[True, <|"SolutionRules" -> Thread[pcVariables -> {0, 0}],
  "FreeParameters" -> {}|>];
check["parallel validation rejects a wrong amplitude while retaining refinement equations",
  pcWrong["FamilyPass"] === False &&
    MemberQ[Lookup[pcWrong["Report"]["Channels"], "Status"], "FAIL"] &&
    pcWrong["Feedback"]["Status"] === "COMPLETE" &&
    pcWrong["Feedback"]["Equations"] === pcParallelValidation["Feedback"]["Equations"]];
pcPublic[parallel_] := CompleteFactorization[2 prH, prChannels, prFixtureModel,
  "CorrectionBasis" -> {prH}, "ChannelReferences" -> None,
  "BoseSymmetryGroups" -> {}, "Samples" -> 1, "ValidationSamples" -> 1,
  "ParameterRules" -> pcRules, "ValidationParameterRules" -> {pcRules},
  "Parallelization" -> parallel, "MaxKernels" -> 2,
  "TimeConstraint" -> 60, "ProgressReporting" -> False, "ReturnDetails" -> True];
pcSerialPublic = pcPublic[False]; pcParallelPublic = pcPublic[True];
check["public completion preserves the exactly fitted amplitude and all channel checks",
  pcSerialPublic["Status"] === "COMPLETE" && pcParallelPublic["Status"] === "COMPLETE" &&
    Together[pcSerialPublic["Amplitude"] - pcParallelPublic["Amplitude"]] === 0 &&
    Together[pcParallelPublic["Amplitude"] - prH] === 0 &&
    AllTrue[pcParallelPublic["ValidationReport"]["Channels"], #["Status"] === "PASS" &]];
check["disabling public parallelism also keeps internal checks on the master",
  pcSerialPublic["InitialReport"]["Execution"]["Mode"] === "Serial" &&
    pcSerialPublic["ConstraintData"]["Fit"]["Execution"]["Mode"] === "Serial"];
check["invalid factorization worker counts are rejected before fitting",
  Quiet[CompleteFactorization[prH, prChannels, prFixtureModel, "MaxKernels" -> 0]] === $Failed];
check["invalid high-energy worker choices are rejected before checking",
  FailureQ[CompleteHighEnergy[prH, prChannels, prFixtureModel, "Parallelization" -> "invalid"]]];
pcComponents = ConstructiveDiagrams`Private`completionLinearComponents[pcTrial, pcVariables, False];
pcExpired = Quiet[ConstructiveDiagrams`Private`completionConstraintSample[
  pcComponents, pcVariables, ConstructiveDiagrams`Private`completionTargets[pcNormalized["Targets"]],
  {1, 1}, 829017, pcRules, 60, AbsoluteTime[] - 1]];
check["a sample that missed the shared deadline is unresolved rather than accepted",
  AssociationQ[pcExpired] && pcExpired["Status"] === "UNRESOLVED" &&
    !KeyExistsQ[pcExpired, "Equations"]];
Print["CHECKS: ", checks, "; FAILURES: ", Length[failures]];
If[failures =!= {}, Print[failures]];
CloseKernels[ConstructiveDiagrams`Private`$amplitudeParallelKernels];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:parallel-completion-tests.wl"]];
Quit[If[failures === {}, 0, 1]];
