root = DirectoryName[DirectoryName[$InputFileName]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
Get[FileNameJoin[{root, "tests", "unscrambling.wl"}]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]]);
ang[i_, j_] := SpinorChain[Spinor["Helicity", "Angle", i], Spinor["Helicity", "Angle", j]];
pair = {Table[Mass[i] -> 0, {i, 6}], ang[1, 2]*ang[3, 4]*ang[5, 6]};
settings = <|"OnShellChannels" -> Automatic, "MomentumConservation" -> False,
  "JobTimeLimit" -> 10, "ValidationTimeLimit" -> 1|>;
jobs = Unscrambling`Private`makeTrainingJobs[{pair, pair}, 2, 2];
check["independent original and scramble jobs", Length[jobs] === 6 && Lookup[jobs, "Seed"] === {0, 1, 2, 0, 1, 2}];
chunks = Unscrambling`Private`partitionTrainingJobs[jobs, 2];
check["partition loses no jobs", SortBy[Flatten[chunks, 1], #["Order"] &] === jobs];
check["both workers assigned work", AllTrue[chunks, Length[#] > 0 &]];
serial = Unscrambling`Private`mapTrainingJobs[jobs, {}, settings];
again = Unscrambling`Private`mapTrainingJobs[jobs, {}, settings];
check["generation deterministic", Lookup[serial, "Result"] === Lookup[again, "Result"]];
check["all generation jobs succeed", Unscrambling`Private`validJobResultsQ[serial]];
check["job timing recorded", AllTrue[serial, #["CPUSeconds"] >= 0 && #["WallSeconds"] >= 0 &]];
cache = CreateDirectory[];
first = Unscrambling`Private`cachedTrainingJobs[jobs, {}, settings, cache, 123];
Block[{Unscrambling`Private`mapTrainingJobs},
  Unscrambling`Private`mapTrainingJobs[{}, _, _] := {};
  Unscrambling`Private`mapTrainingJobs[__] := $Failed;
  second = Unscrambling`Private`cachedTrainingJobs[jobs, {}, settings, cache, 123];
  check["cache hit skips all generation", AllTrue[Lookup[second, "CacheHit"], TrueQ]];
  check["cache preserves dataset", Lookup[first, "Result"] === Lookup[second, "Result"]];
  check["source changes invalidate cache", Unscrambling`Private`cachedTrainingJobs[jobs, {}, settings, cache, 124] === $Failed];
  check["conditions invalidate cache", Unscrambling`Private`cachedTrainingJobs[jobs, {}, Join[settings, <|"MomentumConservation" -> True|>], cache, 123] === $Failed];
];
DeleteDirectory[cache, DeleteContents -> True];
cache = CreateDirectory[];
Block[{Unscrambling`Private`mapTrainingJobs},
  Unscrambling`Private`mapTrainingJobs[_, _, _] := {serial[[1]], <|"Success" -> False|>};
  partial = Unscrambling`Private`cachedTrainingJobs[Take[jobs, 2], {}, settings, cache, 123];
  check["failed job rejects partial dataset", partial === $Failed];
  Unscrambling`Private`mapTrainingJobs[pending_, _, _] := (
    check["retry computes only failed job", Length[pending] === 1 && First[pending]["Order"] === 2];
    {serial[[2]]});
  retried = Unscrambling`Private`cachedTrainingJobs[Take[jobs, 2], {}, settings, cache, 123];
  check["successful job reused after failure", Lookup[retried, "CacheHit"] === {True, False}];
];
DeleteDirectory[cache, DeleteContents -> True];
vjobs = Unscrambling`Private`makeValidationJobs[{pair}, 2, 2, 2, 3];
check["holdout seeds separate", Lookup[vjobs, "Seed"] === {3, 4}];
Block[{Unscrambling`Private`generationJob},
  Unscrambling`Private`generationJob[_] := (Pause[1]; <||>);
  timed = Unscrambling`Private`evaluateTrainingJob[First[jobs], Join[settings, <|"JobTimeLimit" -> 0.05|>]];
  check["timed out job rejected", !TrueQ[timed["Success"]]];
];
check["empty holdout valid", Unscrambling`Private`mapTrainingJobs[{}, {}, settings] === {}];
check["invalid batch size rejected", Quiet[TrainUnscrambleNet[{pair}, "TrainingBatchSize" -> 0]] === $Failed];
check["invalid device rejected", Quiet[TrainUnscrambleNet[{pair}, TargetDevice -> "Unknown"]] === $Failed];
check["invalid workers rejected", Quiet[TrainUnscrambleNet[{pair}, Kernels -> 0]] === $Failed];
Block[{Unscrambling`Private`trainingKernelSnapshot, Unscrambling`Private`launchTrainingKernels,
    Unscrambling`Private`closeTrainingKernels, Unscrambling`Private`initializeTrainingWorkers,
    current = {99}, closed = {}},
  Unscrambling`Private`trainingKernelSnapshot[] := current;
  Unscrambling`Private`launchTrainingKernels[n_] := (current = Join[current, Range[n]]);
  Unscrambling`Private`closeTrainingKernels[workers_] := (closed = Join[closed, workers]; current = Complement[current, workers]);
  Unscrambling`Private`initializeTrainingWorkers[___] := True;
  result = Unscrambling`Private`withTrainingWorkers[2, "unused", 1, 0, Function[w, w]];
  check["only owned workers closed", result === {1, 2} && closed === {1, 2} && current === {99}];
  closed = {};
  result = Unscrambling`Private`withTrainingWorkers[{99}, "unused", 1, 0, Function[w, w]];
  check["borrowed pool preserved", result === {99} && closed === {} && current === {99}];
  result = CheckAbort[Unscrambling`Private`withTrainingWorkers[2, "unused", 1, 0, Function[w, Abort[]]], "Aborted"];
  check["abort closes only owned pool", result === "Aborted" && current === {99} && closed === {1, 2}];
  closed = {};
  Unscrambling`Private`initializeTrainingWorkers[___] := False;
  result = Quiet[Unscrambling`Private`withTrainingWorkers[2, "unused", 1, 0, Function[w, True]]];
  check["initialization failure cleans up", result === $Failed && current === {99} && closed === {1, 2}];
  closed = {};
  Unscrambling`Private`launchTrainingKernels[n_] := (current = Append[current, 1]);
  result = Quiet[Unscrambling`Private`withTrainingWorkers[2, "unused", 1, 0, Function[w, True]]];
  check["partial startup cleans up", result === $Failed && current === {99} && closed === {1}];
];
work = Unscrambling`Private`trainingWorkEstimate[Flatten[Lookup[Lookup[serial, "Result"], "Rows"], 1]];
check["shared state saves encoding work", work["StateBytesPerEpoch"] < work["StateBytesWithoutSharing"]];
check["padding overhead reported", work["EditBytesWithoutPadding"] <= work["EditBytesWithTrainingPadding"]];
Print[<|"Checks" -> checks, "Failed" -> Length[failures], "Failures" -> failures|>];
Quit[If[failures === {}, 0, 1]];
