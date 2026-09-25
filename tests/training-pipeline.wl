(* Independent deterministic jobs; only the coordinator writes model files. *)
TrainUnscrambleNet::job = "A `1` job failed or exceeded its time limit. No partial training dataset will be used.";
TrainUnscrambleNet::cache = "Could not read or write the training-data cache at `1`.";
TrainUnscrambleNet::parallelopts = "WorkerThreads and TrainingBatchSize must be positive integers; time limits must be positive numbers; cache and worker paths must be strings or their documented defaults.";
TrainUnscrambleNet::device = "TargetDevice must be CPU, GPU, CUDA, or {GPU or CUDA, a positive device index or All}. GPU training requires a supported CUDA installation.";

pipelineSourceFiles[root_] := Join[Sort[FileNames["*.wl", FileNameJoin[{root, "source"}]]],
  FileNameJoin[{root, "tests", #}] & /@ {"unscrambling.wl", "schouten.wl", "candidate-policy.wl", "training-pipeline.wl"}];
pipelineFingerprint[root_] := Hash[FileHash[#, "SHA256"] & /@ pipelineSourceFiles[root], "SHA256"];

makeTrainingJobs[amplitudes_, steps_, count_] := MapIndexed[Join[#1, <|"Order" -> First[#2]|>] &,
  Flatten[MapIndexed[Function[{pair, index},
    Table[<|"Mode" -> If[seed === 0, "Original", "Scramble"], "Pair" -> pair,
      "AmplitudeIndex" -> First[index], "Seed" -> seed, "Steps" -> steps|>, {seed, 0, count}]], amplitudes], 1]];
makeValidationJobs[amplitudes_, steps_, count_, hold_, episode_] := MapIndexed[Join[#1, <|"Order" -> First[#2]|>] &,
  Flatten[MapIndexed[Function[{pair, index},
    Table[<|"Mode" -> "Validate", "Pair" -> pair, "AmplitudeIndex" -> First[index],
      "Seed" -> count + seed, "Steps" -> steps, "Episode" -> episode|>, {seed, hold}]], amplitudes], 1]];

generationJob[job_] := Module[{pair = job["Pair"], scr, batches, targets, skipped},
  If[job["Mode"] === "Original",
    Return[<|"Rows" -> trainingRows[canonicalChains[pair[[2]] /. pair[[1]]], pair[[1]], pair[[2]] /. pair[[1]]],
      "Targets" -> 0, "Skipped" -> 0|>]];
  scr = scramblePair[pair, job["Steps"], job["Seed"], True];
  (* Collect once instead of repeatedly copying the growing dataset with Join. *)
  batches = trainingRows[#["After"], pair[[1]], #["Before"]] & /@ Reverse[scr["Trajectory"]];
  targets = Length[batches]; skipped = Count[batches, {}];
  <|"Rows" -> Flatten[batches, 1], "Targets" -> targets, "Skipped" -> skipped|>
];

validationJob[job_] := Module[{pair = job["Pair"], scr, trace, got},
  scr = scramblePair[pair, job["Steps"], job["Seed"], False];
  (* Keep stochastic attempts reproducible regardless of worker assignment. *)
  trace = BlockRandom[SeedRandom[Hash[{job["AmplitudeIndex"], job["Seed"], "Validation"}]];
    runUnscramble[{pair[[1]], scr["Expression"]}, False, job["Episode"], 5, $loadedModelDirectory]];
  If[!TrueQ[trace["ModelLoaded"]] || TrueQ[Lookup[trace, "ScoringFailed", False]], Return[$Failed]];
  got = trace["Result"][[2]];
  <|"Simpler" -> Boole[expressionComplexity[got] < expressionComplexity[scr["Expression"]]],
    "Recovered" -> Boole[samePoly[got, scr["Original"]]],
    "TerminationReason" -> Lookup[trace, "TerminationReason", "Unknown"]|>
];

evaluateTrainingJob[job_, settings_] := Block[
  {$onShellChannels = settings["OnShellChannels"], $useMomentumConservation = settings["MomentumConservation"],
    $timeLimit = settings["ValidationTimeLimit"]},
  Module[{start = AbsoluteTime[], cpu = TimeUsed[], result},
    result = TimeConstrained[If[job["Mode"] === "Validate", validationJob[job], generationJob[job]],
      settings["JobTimeLimit"], $Failed];
    <|"Order" -> job["Order"], "Success" -> AssociationQ[result], "Result" -> result,
      "WallSeconds" -> (AbsoluteTime[] - start), "CPUSeconds" -> (TimeUsed[] - cpu), "Worker" -> $KernelID|>
  ]
];

(* Greedy cost assignment avoids concentrating large amplitudes on one worker.
   Each worker receives only its own jobs, not the entire training dataset. *)
partitionTrainingJobs[jobs_, n_] := Module[{chunks = ConstantArray[{}, n], loads = ConstantArray[0, n], slot, job},
  Do[
    slot = First[Ordering[loads, 1]];
    AppendTo[chunks[[slot]], job];
    loads[[slot]] += LeafCount[job["Pair"]]*Max[1, job["Steps"]],
    {job, Reverse[SortBy[jobs, LeafCount[#["Pair"]]*Max[1, #["Steps"]] &]]}];
  chunks
];

mapTrainingJobs[{}, _, _] := {};
mapTrainingJobs[jobs_, {}, settings_] := evaluateTrainingJob[#, settings] & /@ jobs;
mapTrainingJobs[jobs_, workers_List, settings_] := Module[{chunks, raw, i},
  chunks = partitionTrainingJobs[jobs, Length[workers]];
  Do[With[{chunk = chunks[[i]]}, ParallelEvaluate[$workerJobs = chunk; Null, {workers[[i]]}, DistributedContexts -> None]],
    {i, Length[workers]}];
  raw = With[{config = settings}, ParallelEvaluate[
    Module[{results = evaluateTrainingJob[#, config] & /@ $workerJobs}, $workerJobs = {}; results],
    workers, DistributedContexts -> None]];
  If[!ListQ[raw] || !AllTrue[raw, ListQ], Return[$Failed]];
  raw = Flatten[raw, 1];
  If[Length[raw] =!= Length[jobs] || !AllTrue[raw, AssociationQ], Return[$Failed]];
  SortBy[raw, #["Order"] &]
];

validJobResultsQ[results_] := ListQ[results] && AllTrue[results, AssociationQ[#] && TrueQ[Lookup[#, "Success", False]] &];

trainingKernelSnapshot[] := Kernels[];
launchTrainingKernels[n_] := LaunchKernels[n];
closeTrainingKernels[workers_] := CloseKernels[workers];
initializeTrainingWorkers[workers_, workerRoot_, workerThreads_, fingerprint_] := With[
  {root = workerRoot, threads = ToString[workerThreads], requested = workerThreads, expected = fingerprint},
  TimeConstrained[ParallelEvaluate[
    (* Evaluate the platform on each worker, which may be a remote host. *)
    SetEnvironment[{"OMP_NUM_THREADS" -> ToString[If[$SystemID === "MacOSX-ARM64", Max[4, requested], requested]],
      "OMP_DYNAMIC" -> "FALSE", "OPENBLAS_NUM_THREADS" -> threads,
      "MKL_NUM_THREADS" -> threads, "MXNET_CPU_WORKER_NTHREADS" -> "1"}];
    Global`$HEPCATpath = FileNameJoin[{root, "source"}];
    Get[FileNameJoin[{Global`$HEPCATpath, "HEPCAT.wl"}]];
    Get[FileNameJoin[{root, "tests", "unscrambling.wl"}]];
    pipelineFingerprint[root] === expected,
    workers, DistributedContexts -> None], 60, $Failed] === ConstantArray[True, Length[workers]]
];

(* Only kernels launched here are closed. Explicit caller-supplied pools,
   including remote cluster kernels, remain connected after the call. *)
withTrainingWorkers[spec_, workerRoot_, workerThreads_, fingerprint_, body_] := Module[
  {before = trainingKernelSnapshot[], owned = {}, workers = {}, requested, result, initialized, launching = False,
    started = AbsoluteTime[], setupSeconds},
  CheckAbort[
    result = Catch[
      Which[
        spec === 1, Null,
        ListQ[spec] && spec =!= {} && DuplicateFreeQ[spec] && AllTrue[spec, MemberQ[before, #] &], workers = spec,
        spec === Automatic || (IntegerQ[spec] && spec > 1),
          requested = If[spec === Automatic, Max[1, $ProcessorCount - 1], spec];
          launching = True;
          TimeConstrained[launchTrainingKernels[requested], 60, Null];
          owned = Complement[trainingKernelSnapshot[], before]; workers = owned; launching = False;
          If[Length[workers] =!= requested, Message[TrainUnscrambleNet::kernels]; Throw[$Failed]],
        True, Message[TrainUnscrambleNet::kernels]; Throw[$Failed]
      ];
      If[workers =!= {},
        initialized = initializeTrainingWorkers[workers, workerRoot, workerThreads, fingerprint];
        If[!TrueQ[initialized], Message[TrainUnscrambleNet::kernels]; Throw[$Failed]]
      ];
      setupSeconds = AbsoluteTime[] - started;
      body[workers]
    ];
    If[owned =!= {}, closeTrainingKernels[owned]];
    If[AssociationQ[result], Join[result, <|"WorkerSetupSeconds" -> setupSeconds,
      "TotalWallSeconds" -> (AbsoluteTime[] - started)|>], result],
    If[launching, owned = Complement[trainingKernelSnapshot[], before]];
    If[owned =!= {}, closeTrainingKernels[owned]]; Abort[]
  ]
];

cachedTrainingJobs[jobs_, workers_, settings_, cache_, fingerprint_] := Module[
  {keys, results, misses, computed, i, path, entry, tmp, key, writeOK},
  keys = IntegerString[Hash[{fingerprint, KeyDrop[settings, {"JobTimeLimit", "ValidationTimeLimit"}], #}, "SHA256"], 16, 64] & /@ jobs;
  results = ConstantArray[Missing["Cache"], Length[jobs]];
  If[StringQ[cache],
    If[!DirectoryQ[cache], CreateDirectory[cache, CreateIntermediateDirectories -> True]];
    Do[
      path = FileNameJoin[{cache, keys[[i]] <> ".wxf"}];
      If[FileExistsQ[path],
        entry = Quiet[Check[Import[path, "WXF"], $Failed]];
        If[AssociationQ[entry] && Lookup[entry, "Key", None] === keys[[i]] &&
            validJobResultsQ[{Lookup[entry, "Value", $Failed]}],
          results[[i]] = Join[entry["Value"], <|"CacheHit" -> True, "CPUSeconds" -> 0., "WallSeconds" -> 0.|>]]
      ], {i, Length[jobs]}]
  ];
  misses = Select[Range[Length[jobs]], MissingQ[results[[#]]] &];
  computed = mapTrainingJobs[jobs[[misses]], workers, settings];
  If[!ListQ[computed] || Length[computed] =!= Length[misses], Return[$Failed]];
  Do[
    i = misses[[key]];
    If[!validJobResultsQ[{computed[[key]]}], Continue[]];
    results[[i]] = Join[computed[[key]], <|"CacheHit" -> False|>];
    If[StringQ[cache],
      path = FileNameJoin[{cache, keys[[i]] <> ".wxf"}]; tmp = path <> "." <> CreateUUID[] <> ".tmp";
      writeOK = Quiet[Check[Export[tmp, <|"Key" -> keys[[i]], "Value" -> results[[i]]|>, "WXF"] =!= $Failed &&
        RenameFile[tmp, path, OverwriteTarget -> True] =!= $Failed, False]];
      If[FileExistsQ[tmp], DeleteFile[tmp]];
      If[!TrueQ[writeOK], Message[TrainUnscrambleNet::cache, cache]; Return[$Failed]]
    ], {key, Length[misses]}];
  If[validJobResultsQ[results], results, $Failed]
];

installValidationModel[{}, _, _] := True;
installValidationModel[workers_, net_, directory_] := With[{model = net, dir = directory},
  ParallelEvaluate[$modelNet = model; $loadedModelDirectory = dir; True, workers, DistributedContexts -> None] ===
    ConstantArray[True, Length[workers]]
];

TrainUnscrambleNet[amplitudes_List, OptionsPattern[]] := Block[
  {$onShellChannels = normalizeOnShellChannels[OptionValue["OnShellChannels"]],
    $useMomentumConservation = OptionValue["MomentumConservation"], $trainingBatchSize = OptionValue["TrainingBatchSize"],
    $trainingTargetDevice = OptionValue[TargetDevice]},
  Module[{steps = OptionValue[Steps], count = OptionValue[Scrambles], hold = OptionValue[HoldOut],
      rounds = OptionValue[MaxTrainingRounds], episode = OptionValue[EpisodeLength], root = DirectoryName[$modelDirectory],
      workerRoot, cache = OptionValue["DataCacheDirectory"], settings, fingerprint, jobs,
      dir = resolveModelDirectory[OptionValue["ModelDirectory"]], workerThreads = OptionValue["WorkerThreads"]},
    If[!AllTrue[{steps, hold}, IntegerQ[#] && # >= 0 &] ||
        !AllTrue[{count, rounds, episode}, IntegerQ[#] && # > 0 &], Message[TrainUnscrambleNet::options]; Return[$Failed]];
    If[!MemberQ[{"CPU", "GPU", "CUDA"}, $trainingTargetDevice] &&
        !MatchQ[$trainingTargetDevice, {("GPU" | "CUDA"), (All | _Integer?Positive)}],
      Message[TrainUnscrambleNet::device]; Return[$Failed]];
    If[!AllTrue[{workerThreads, $trainingBatchSize}, IntegerQ[#] && # > 0 &] ||
        !AllTrue[{OptionValue["JobTimeLimit"], OptionValue["ValidationTimeLimit"]}, NumberQ[#] && TrueQ[# > 0] &] ||
        !(cache === None || StringQ[cache]) || !(OptionValue["WorkerRoot"] === Automatic || StringQ[OptionValue["WorkerRoot"]]),
      Message[TrainUnscrambleNet::parallelopts]; Return[$Failed]];
    workerRoot = Replace[OptionValue["WorkerRoot"], Automatic -> root];
    If[StringQ[cache], cache = ExpandFileName[cache]];
    settings = <|"OnShellChannels" -> $onShellChannels, "MomentumConservation" -> $useMomentumConservation,
      "JobTimeLimit" -> OptionValue["JobTimeLimit"], "ValidationTimeLimit" -> OptionValue["ValidationTimeLimit"]|>;
    fingerprint = pipelineFingerprint[root]; jobs = makeTrainingJobs[amplitudes, steps, count];
    withTrainingWorkers[OptionValue[Kernels], workerRoot, workerThreads, fingerprint, Function[workers,
      runTrainingPipeline[amplitudes, jobs, workers, settings, cache, fingerprint, steps, count, hold, episode, rounds, dir]]]
  ]
];

runTrainingPipeline[amplitudes_, jobs_, workers_, settings_, cache_, fingerprint_, steps_, count_, hold_, episode_, rounds_, dir_] := Module[
  {generated, rows, targets, skipped, net, paths, validation, dataSeconds, trainingSeconds, trainingCPU, validationSeconds,
    validationJobs, candidateRows, work},
  {dataSeconds, generated} = AbsoluteTiming[cachedTrainingJobs[jobs, workers, settings, cache, fingerprint]];
  If[!validJobResultsQ[generated], Message[TrainUnscrambleNet::job, "data generation"]; Return[$Failed]];
  rows = Flatten[Lookup[Lookup[generated, "Result"], "Rows"], 1];
  targets = Total[Lookup[Lookup[generated, "Result"], "Targets"]];
  skipped = Total[Lookup[Lookup[generated, "Result"], "Skipped"]];
  generated = KeyDrop[#, "Result"] & /@ generated;
  If[rows === {} || targets === skipped, Message[TrainUnscrambleNet::nodata]; Return[$Failed]];
  trainingCPU = TimeUsed[];
  {trainingSeconds, net} = AbsoluteTiming[trainCandidateNetwork[rows, rounds]];
  trainingCPU = TimeUsed[] - trainingCPU;
  If[!MatchQ[net, _NetChain | _NetGraph], Return[$Failed]];
  $modelNet = net; $loadedModelDirectory = dir;
  If[!DirectoryQ[dir], CreateDirectory[dir, CreateIntermediateDirectories -> True]];
  paths = modelPaths[dir];
  If[Export[paths["Net"], net] === $Failed, Message[TrainUnscrambleNet::save, dir]; Return[$Failed]];
  Put[<|"Version" -> $policyVersion, "Encoding" -> "SplitFullFormUTF8", "Architecture" -> "SharedStateGRU",
    "OnShellChannels" -> $onShellChannels, "MomentumConservation" -> $useMomentumConservation|>, paths["Metadata"]];
  validationJobs = makeValidationJobs[amplitudes, steps, count, hold, episode];
  {validationSeconds, validation} = AbsoluteTiming[
    If[validationJobs =!= {} && !installValidationModel[workers, net, dir], $Failed,
      mapTrainingJobs[validationJobs, workers, settings]]];
  If[!validJobResultsQ[validation], Message[TrainUnscrambleNet::job, "validation (model already saved)"]; Return[$Failed]];
  candidateRows = Total[Length /@ Lookup[rows, "Target"]];
  work = trainingWorkEstimate[rows];
  <|"Kernels" -> Max[1, Length[workers]], "Starts" -> Length[amplitudes]*count,
    "TrainingStates" -> Length[rows], "TrainingRows" -> candidateRows, "TrainingBatchSize" -> $trainingBatchSize,
    "TrainingTargetDevice" -> $trainingTargetDevice,
    "ReverseSteps" -> targets - skipped, "SkippedReverseSteps" -> skipped,
    "CacheHits" -> Count[Lookup[generated, "CacheHit"], True],
    "HoldoutScrambles" -> Length[validation],
    "HoldoutSimpler" -> Total[Lookup[Lookup[validation, "Result"], "Simpler"]],
    "HoldoutRecovered" -> Total[Lookup[Lookup[validation, "Result"], "Recovered"]],
    "DataGenerationSeconds" -> dataSeconds, "TrainingSeconds" -> trainingSeconds,
    "TrainingCPUSeconds" -> trainingCPU, "TrainingDataBytes" -> ByteCount[rows],
    "ValidationSeconds" -> validationSeconds,
    "GenerationWorkerCPUSeconds" -> Total[Lookup[generated, "CPUSeconds"]],
    "ValidationWorkerCPUSeconds" -> Total[Lookup[validation, "CPUSeconds"]],
    "EncodingWork" -> work,
    "GenerationJobs" -> (KeyDrop[#, "Result"] & /@ generated),
    "ValidationJobs" -> (KeyDrop[#, "Result"] & /@ validation),
    "ModelPath" -> paths["Net"]|>
];

trainingWorkEstimate[rows_] := Module[{counts, widths, width, stateBytes, edits},
  counts = Length /@ Lookup[rows, "Candidates"];
  edits = Flatten[Lookup[rows, "Candidates"], 1];
  width = Max[Length /@ edits];
  stateBytes = Length /@ Lookup[rows, "State"];
  <|"StateBytesPerEpoch" -> Total[stateBytes],
    "StateBytesWithoutSharing" -> Total[stateBytes*counts],
    "EditBytesWithoutPadding" -> Total[Count[#, Except[257]] & /@ edits],
    "EditBytesWithTrainingPadding" -> (width*Total[counts]),
    "MaximumEditWidth" -> width|>
];
