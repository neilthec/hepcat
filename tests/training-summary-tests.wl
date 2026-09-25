root = DirectoryName[DirectoryName[$InputFileName]];
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
check["notebook prints readable summary", StringContainsQ[Last[inputs], "Print[trainingRunSummary["] && !StringContainsQ[Last[inputs], "Print[report]"]];
Print[summary];
Print[<|"Checks" -> checks, "Failed" -> Length[failures], "Failures" -> failures|>];
Quit[If[failures === {}, 0, 1]];
