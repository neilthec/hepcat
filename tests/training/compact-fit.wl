(* ::Section::Closed:: *)
(*Compact Numerical Validation and Persistence*)
(* Loaded in Unscrambling`Private`; staged as compact-scale-training.wl. *)
compactScaleFiniteQ[x_] := NumberQ[x] && TrueQ[Im[x] == 0] && TrueQ[Abs[x] < Infinity];
compactScaleFiniteVectorQ[x_] := Module[{value = x, bounds},
  If[!ListQ[value] || !VectorQ[value], Return[False]];
  If[value === {}, Return[True]];
  If[Developer`PackedArrayQ[value] && MemberQ[{Integer, Real}, Head[First[value]]],
    bounds = MinMax[value]; Return[AllTrue[bounds, compactScaleFiniteQ]]];
  VectorQ[value, compactScaleFiniteQ]
];
compactScaleByteVectorQ[x_] := Module[{value = x, bounds},
  If[!ListQ[value] || !VectorQ[value] || value === {}, Return[False]];
  If[Developer`PackedArrayQ[value] && Head[First[value]] === Integer,
    bounds = MinMax[value]; Return[TrueQ[1 <= First[bounds] && Last[bounds] <= 256]]];
  VectorQ[value, IntegerQ[#] && 1 <= # <= 256 &]
];
compactScaleFiniteNetQ[net_] := MatchQ[net, _NetGraph | _NetChain] &&
  AllTrue[Information[net, "ArraysPositionList"],
    compactScaleFiniteVectorQ[Flatten[Normal[NetExtract[net, #]]]] &];

compactScaleRowQ[row_, width_, count_] := Module[{value = row, n},
  If[!AssociationQ[value], Return[False]]; n = value["ActionCount"];
  IntegerQ[n] && 0 < n <= count && Dimensions[value["Candidates"]] === {n, width} &&
    compactScaleFiniteVectorQ[Flatten[value["Candidates"]]] && compactScaleByteVectorQ[value["State"]] &&
    Length[value["Target"]] === n && Length[value["LegalMask"]] === n &&
    compactScaleFiniteVectorQ[value["Target"]] && compactScaleFiniteVectorQ[value["LegalMask"]] &&
    Min[value["Target"]] >= 0. && Abs[Total[value["Target"]] - 1.] < 0.00001 &&
    Min[value["LegalMask"]] > 0.
];

compactScaleTrainRow[row_, width_, count_] := <|"State" -> row["State"],
  "Candidates" -> Developer`ToPackedArray[PadRight[row["Candidates"], {count, width}, 0.]],
  "Target" -> Developer`ToPackedArray[PadRight[row["Target"], count, 0.]],
  "LegalMask" -> Developer`ToPackedArray[PadRight[row["LegalMask"], count, 0.]]|>;

compactScaleAssessment[policy_, rr_] := Module[{records, scores, n, target, maximum, loss},
  If[rr === {}, Return[<|"Rows" -> 0, "Correct" -> 0, "Accuracy" -> Null,
    "MeanLegalCrossEntropy" -> Null|>]];
  records = Map[Function[row,
    n = row["ActionCount"]; target = row["Target"];
    scores = Flatten[policy[KeyTake[row, {"State", "Candidates"}], TargetDevice -> "CPU"]];
    If[Length[scores] =!= n || !compactScaleFiniteVectorQ[scores], Return[$Failed, Module]];
    maximum = Max[scores]; loss = Log[Total[Exp[scores - maximum]]] - Total[target (scores - maximum)];
    <|"Correct" -> Boole[target[[First[Ordering[scores, -1]]]] > 0.], "Loss" -> loss|>
  ], rr];
  If[!FreeQ[records, $Failed] || !AllTrue[Lookup[records, "Loss"], compactScaleFiniteQ], Return[$Failed]];
  <|"Rows" -> Length[rr], "Correct" -> Total[Lookup[records, "Correct", {}]],
    "Accuracy" -> If[rr === {}, Null, N[Mean[Lookup[records, "Correct"]]]],
    "MeanLegalCrossEntropy" -> If[rr === {}, Null, Mean[Lookup[records, "Loss"]]]|>
];

compactScaleReadoutAudit[policy_, rr_] := Module[{encoder, rawEncoder, records},
  encoder = NetExtract[policy, "StateEncoder"];
  rawEncoder = NetReplacePart[encoder, {
    "MeanUnit" -> FunctionLayer[# &, "Input" -> 64],
    "MaxUnit" -> FunctionLayer[# &, "Input" -> 64]}];
  records = Map[Function[row, Module[{actual, raw, parts, expected, difference},
    actual = encoder[row["State"], TargetDevice -> "CPU"];
    raw = rawEncoder[row["State"], TargetDevice -> "CPU"];
    If[Length[actual] =!= 128 || Length[raw] =!= 128 ||
        !compactScaleFiniteVectorQ[actual] || !compactScaleFiniteVectorQ[raw],
      Return[<|"Passed" -> False|>]];
    parts = Partition[raw, 64];
    expected = Flatten[#/Sqrt[Total[#^2] + 1.*^-12] & /@ parts];
    difference = Max[Abs[actual - expected]];
    <|"Passed" -> (compactScaleFiniteVectorQ[expected] && difference < 0.00001),
      "Difference" -> difference, "ZeroBranches" -> Count[Norm /@ parts, 0.],
      "BothBranchesZero" -> Boole[AllTrue[parts, Norm[#] == 0. &]]|>
  ]], rr];
  <|"Passed" -> AllTrue[records, TrueQ[#["Passed"]] &],
    "MaximumDifference" -> If[records === {}, 0., Max[Lookup[records, "Difference", Infinity]]],
    "ZeroRawBranches" -> Total[Lookup[records, "ZeroBranches", 0]],
    "StatesWithBothBranchesZero" -> Total[Lookup[records, "BothBranchesZero", 0]]|>
];

compactScaleAtomicExport[path_, expression_, format_:Automatic] := Module[{temp, result},
  temp = FileNameJoin[{DirectoryName[path], ".pending-" <> FileNameTake[path]}];
  result = If[format === Automatic, Export[temp, expression], Export[temp, expression, format]];
  If[result === $Failed, Return[False]];
  TrueQ[Quiet[Check[RenameFile[temp, path, OverwriteTarget -> True]; True, False]]]
];
compactScaleAtomicPut[path_, expression_] := Module[{temp},
  temp = FileNameJoin[{DirectoryName[path], ".pending-" <> FileNameTake[path]}];
  Quiet[Check[Put[expression, temp]; RenameFile[temp, path, OverwriteTarget -> True]; True, False]]
];
compactScaleFileHash[path_] := IntegerString[FileHash[path, "SHA256"], 16, 64];
compactScaleArraysHash[net_] := IntegerString[Hash[
  With[{positions = Information[net, "ArraysPositionList"]},
    {positions, NetExtract[net, #] & /@ positions}], "SHA256"], 16, 64];

Clear[compactScaleTrain];
(* ::Section::Closed:: *)
(*Continuous Compact Optimization*)
compactScaleTrain[rows_List, config_Association, run_String, metadata_Association] := Module[
  {corpus = rows, rowCount, width, count, maximumActions, seconds, checkpointEvery, progressEvery, maxRounds, seed,
    learningRate, fitSize, heldout, fitRows, auditRows, trainer, order = {}, cursor = 1, epoch = 0,
    generator, guard, progress, checkpoint, observed = <|"AbsoluteBatch" -> 0|>, history = {},
    invalidLoss = False, diverged = False, checkpointFailed = False, lastCheckpointBatch = -1,
    lastSafeNet = None, start, elapsed, result, properties, stats, failure, current, actualUpdates,
    actualRounds, stopped, totalTrainTime, checkpointsDirectory, baseMetadata, callbackStatus,
    lastCheckpointTime = 0., progressFile, policyFile, metadataFile, resumeFile, historyFile, manifestFile,
    rowAuditSeconds, validRows},
  rowCount = Length[corpus];
  If[rowCount === 0, Return[<|"Net" -> $Failed, "Status" -> "NoTrainingRows"|>]];
  width = Lookup[config, "FeatureWidth", compactActionFeatureWidth];
  count = Max[Lookup[corpus, "ActionCount"]]; maximumActions = Lookup[config, "MaxActions", 1024];
  seconds = N[Lookup[config, "NNTimeSeconds", 36000]];
  checkpointEvery = N[Lookup[config, "CheckEverySeconds", 900]];
  progressEvery = N[Lookup[config, "ProgressEverySeconds", 30]];
  maxRounds = Lookup[config, "MaxTrainingRounds", 1000000]; seed = Lookup[config, "RandomSeed", 1];
  learningRate = N[Lookup[config, "LearningRate", 0.001]];
  fitSize = Min[rowCount, Lookup[config, "FitSampleSize", 32]];
  heldout = Lookup[config, "ValidationRows", {}];
  If[!IntegerQ[width] || width <= 0 || !IntegerQ[count] || !IntegerQ[maximumActions] ||
      maximumActions <= 0 || count > maximumActions || !IntegerQ[seed] || !ListQ[heldout] ||
      !AllTrue[{seconds, checkpointEvery, progressEvery, learningRate}, compactScaleFiniteQ[#] && # > 0. &] ||
      !IntegerQ[maxRounds] || maxRounds <= 0 || !IntegerQ[fitSize] || fitSize <= 0,
    Return[<|"Net" -> $Failed, "Status" -> "InvalidTrainingRowsOrConfiguration"|>]];
  Print[DateString["ISODateTime"], " TRAINING INPUT AUDIT START: ",
    <|"Rows" -> rowCount, "PackedDatasetBytes" -> ByteCount[corpus]|>];
  {rowAuditSeconds, validRows} = AbsoluteTiming[AllTrue[corpus, compactScaleRowQ[#, width, count] &] &&
    AllTrue[heldout, compactScaleRowQ[#, width, maximumActions] &]];
  Print[DateString["ISODateTime"], " TRAINING INPUT AUDIT COMPLETE: ",
    <|"Passed" -> validRows, "Seconds" -> rowAuditSeconds, "WolframMemoryBytes" -> MemoryInUse[]|>];
  If[!TrueQ[validRows], Return[<|"Net" -> $Failed, "Status" -> "InvalidTrainingRows"|>]];
  If[!DirectoryQ[run], CreateDirectory[run, CreateIntermediateDirectories -> True]];
  checkpointsDirectory = FileNameJoin[{run, "checkpoints"}];
  If[!DirectoryQ[checkpointsDirectory], CreateDirectory[checkpointsDirectory]];
  progressFile = FileNameJoin[{run, "training-progress.json"}];
  policyFile = FileNameJoin[{run, "unscramble-moves.wlnet"}];
  metadataFile = FileNameJoin[{run, "unscramble-moves.m"}];
  resumeFile = FileNameJoin[{run, "training-checkpoint.wxf"}];
  historyFile = FileNameJoin[{run, "fit-history.json"}];
  manifestFile = FileNameJoin[{run, "checkpoint-manifest.json"}];
  fitRows = If[fitSize === 1, Take[corpus, 1],
    corpus[[DeleteDuplicates[Round[Subdivide[1, rowCount, fitSize - 1]]]]]];
  auditRows = Take[fitRows, UpTo[4]];
  baseMetadata = Join[metadata, <|"Encoding" -> "StateUTF8AndNumericActions",
    "Architecture" -> "CompactReverseMoveActions", "DescriptorEncoding" -> "CompactStructuredActions",
    "DescriptorFeatureWidth" -> width, "Objective" -> "LegalMoveCrossEntropy",
    "ReadoutNormalization" -> "SeparateMeanMaxL2", "ReadoutSquaredNormEpsilon" -> 1.*^-12,
    "TrainingActionCount" -> count, "TrainingBatchSize" -> 1, "LearningRate" -> learningRate,
    "Optimizer" -> "ADAM", "OptimizerContinuity" -> "SingleNetTrainSession",
    "CheckpointResumeSemantics" -> "WeightsOnly; restarting NetTrain resets ADAM state",
    "Initialization" -> "Fresh", "RandomSeed" -> seed|>];
  trainer = NetInitialize[compactActionTrainer[width, count], RandomSeeding -> seed];
  If[!compactScaleFiniteNetQ[trainer], Return[<|"Net" -> $Failed, "Status" -> "InvalidInitialArrays"|>]];
  start = AbsoluteTime[];
  callbackStatus[p_] := Module[{keys},
    keys = Intersection[Keys[p], {"AbsoluteBatch", "Round", "BatchLoss", "RoundLoss", "TimeElapsed", "TimeRemaining"}];
    Join[AssociationThread[keys, Replace[Lookup[p, #], None | _Missing -> Null] & /@ keys],
      <|"Timestamp" -> DateString["ISODateTime"], "WallSeconds" -> N[AbsoluteTime[] - start],
        "BudgetSecondsRemaining" -> Max[0., seconds - Lookup[p, "TimeElapsed", 0.]],
        "BudgetProgressFraction" -> Min[1., Max[0., Lookup[p, "TimeElapsed", 0.]/seconds]],
        "CompletedFullRounds" -> Quotient[Lookup[p, "AbsoluteBatch", 0], rowCount],
        "TrainingRows" -> rowCount, "TrainingActionCount" -> count,
        "WolframMemoryBytes" -> MemoryInUse[]|>]
  ];
  checkpoint[net_, status_] := Module[
    {policy, fit, validation, audit, entry, checkpointPath, savedMeta, manifest, trainerHash, policyHash},
    If[!compactScaleFiniteNetQ[net], checkpointFailed = True; Return[False]];
    policy = variableCompactActionPolicy[NetExtract[net, "Policy"], width];
    fit = compactScaleAssessment[policy, fitRows];
    validation = compactScaleAssessment[policy, heldout];
    audit = compactScaleReadoutAudit[policy, auditRows];
    If[fit === $Failed || validation === $Failed || !TrueQ[audit["Passed"]],
      checkpointFailed = True; Return[False]];
    entry = Join[status, <|"FitSample" -> fit, "ValidationSample" -> validation,
      "ReadoutAudit" -> audit, "OptimizerStateReset" -> False|>];
    trainerHash = compactScaleArraysHash[net]; policyHash = compactScaleArraysHash[policy];
    savedMeta = Join[baseMetadata, <|"TrainingProgress" -> entry,
      "TrainerArraysSHA256" -> trainerHash, "PolicyArraysSHA256" -> policyHash|>];
    checkpointPath = FileNameJoin[{checkpointsDirectory,
      "trainer-" <> IntegerString[Lookup[status, "AbsoluteBatch", 0], 10, 9] <> ".wxf"}];
    If[!compactScaleAtomicExport[checkpointPath, net] ||
        !compactScaleAtomicExport[resumeFile, net] || !compactScaleAtomicExport[policyFile, policy] ||
        !compactScaleAtomicPut[metadataFile, savedMeta] ||
        !compactScaleAtomicExport[historyFile, Append[history, entry], "RawJSON"],
      checkpointFailed = True; Return[False]];
    manifest = <|"AbsoluteBatch" -> Lookup[status, "AbsoluteBatch", 0],
      "Timestamp" -> DateString["ISODateTime"], "Algorithm" -> "SHA256",
      "TrainerFile" -> FileNameTake[resumeFile], "TrainerSHA256" -> compactScaleFileHash[resumeFile],
      "PolicyFile" -> FileNameTake[policyFile], "PolicySHA256" -> compactScaleFileHash[policyFile],
      "MetadataFile" -> FileNameTake[metadataFile], "MetadataSHA256" -> compactScaleFileHash[metadataFile],
      "HistoryFile" -> FileNameTake[historyFile], "HistorySHA256" -> compactScaleFileHash[historyFile],
      "SnapshotFile" -> FileNameJoin[{"checkpoints", FileNameTake[checkpointPath]}],
      "SnapshotSHA256" -> compactScaleFileHash[checkpointPath],
      "TrainerArraysSHA256" -> trainerHash, "PolicyArraysSHA256" -> policyHash|>;
    (* Publish the manifest last so interrupted multi-file writes are detectable. *)
    If[!compactScaleAtomicExport[manifestFile, manifest, "RawJSON"],
      checkpointFailed = True; Return[False]];
    AppendTo[history, entry]; lastSafeNet = net;
    lastCheckpointBatch = Lookup[status, "AbsoluteBatch", 0]; lastCheckpointTime = AbsoluteTime[];
    Print[DateString["ISODateTime"], " CHECKPOINT: ", entry]; True
  ];
  generator = Function[info, Module[{view},
    If[info["BatchSize"] =!= 1, Print["Unexpected generator batch size."]; Abort[]];
    If[cursor > Length[order], epoch++;
      order = BlockRandom[SeedRandom[Hash[{seed, epoch, "CompactTrainingRows"}]];
        RandomSample[Range[rowCount]]]; cursor = 1];
    view = compactScaleTrainRow[corpus[[order[[cursor++]]]], width, count];
    AssociationThread[Keys[view], List /@ Values[view]]
  ]];
  guard = Function[p,
    observed = callbackStatus[p];
    If[AnyTrue[Lookup[observed, {"BatchLoss", "RoundLoss"}, None],
        # =!= None && # =!= Null && !MissingQ[#] && !compactScaleFiniteQ[#] &], invalidLoss = True; Abort[]];
  ];
  progress = Function[p,
    observed = callbackStatus[p];
    If[!compactScaleAtomicExport[progressFile, observed, "RawJSON"], checkpointFailed = True; Abort[]];
    Print[DateString["ISODateTime"], " NN PROGRESS: ", observed];
    If[AbsoluteTime[] - lastCheckpointTime >= checkpointEvery &&
        Lookup[observed, "AbsoluteBatch", 0] > lastCheckpointBatch,
      If[!checkpoint[p["Net"], observed], Abort[]]];
  ];
  If[!checkpoint[trainer, <|"AbsoluteBatch" -> 0, "CompletedFullRounds" -> 0,
      "Timestamp" -> DateString["ISODateTime"], "WallSeconds" -> 0., "TrainingRows" -> rowCount|>],
    Return[<|"Net" -> $Failed, "Status" -> "InitialCheckpointFailed"|>]];
  Print["CONTINUOUS COMPACT TRAINING: ", <|"Rows" -> rowCount, "FeatureWidth" -> width,
    "FixedActionCount" -> count, "UnpaddedDatasetBytes" -> ByteCount[corpus],
    "GeneratorExpressionBytes" -> ByteCount[generator],
    "ProgressStatusDefinitionBytes" -> ByteCount[DownValues[callbackStatus]],
    "OnePaddedCandidateMatrixBytes" -> 8 count width, "NNTimeSeconds" -> seconds,
    "CheckpointSeconds" -> checkpointEvery, "OptimizerContinuity" -> "Single ADAM session"|>];
  properties = {"TrainedNet", "TotalRounds", "TotalBatches", "ReasonTrainingStopped", "TotalTrainingTime", "RoundLoss"};
  {elapsed, result} = AbsoluteTiming[TimeConstrained[CheckAbort[Check[
    NetTrain[trainer, {generator, "RoundLength" -> rowCount}, properties,
      BatchSize -> 1, MaxTrainingRounds -> maxRounds, TimeGoal -> seconds, TargetDevice -> "CPU",
      TrainingProgressReporting -> None, TrainingProgressMeasurements -> None,
      TrainingProgressFunction -> {{guard, "Interval" -> Quantity[1, "Batches"]},
        {progress, "Interval" -> Quantity[progressEvery, "Seconds"]}},
      Method -> {"ADAM", "LearningRate" -> learningRate}],
    diverged = True; $Failed, {NetTrain::arrdiv}], $Aborted], seconds + 120., $Aborted]];
  failure = Which[diverged, "ArrayDivergence", invalidLoss, "NonfiniteProgressLoss",
    checkpointFailed, "CheckpointOrValidationFailure", result === $Aborted, "HardTimeoutOrAbort",
    !ListQ[result] || Length[result] =!= Length[properties], "MissingTrainingResults", True, None];
  actualUpdates = actualRounds = stopped = totalTrainTime = Null;
  If[failure === None,
    current = First[result]; actualRounds = result[[2]]; actualUpdates = result[[3]];
    stopped = If[StringQ[result[[4]]], result[[4]], ToString[result[[4]], InputForm]];
    totalTrainTime = result[[5]];
    failure = Which[!compactScaleFiniteNetQ[current], "NonfiniteFinalArrays",
      !IntegerQ[actualUpdates] || actualUpdates < 1 || !compactScaleFiniteQ[actualRounds], "InvalidActualCounters",
      !TrueQ[actualRounds == Quotient[actualUpdates, rowCount]], "InconsistentActualCounters",
      !compactScaleFiniteQ[totalTrainTime] || !(compactScaleFiniteQ[result[[6]]] ||
        (actualRounds === 0 && result[[6]] === None)), "NonfiniteFinalLossOrTime",
      !MemberQ[{"TimeGoal", "MaxTrainingRounds"}, stopped], "UnexpectedStopReason", True, None];
  ];
  stats = <|"Status" -> If[failure === None, "Complete", failure], "ActualRounds" -> actualRounds,
    "ActualUpdates" -> actualUpdates, "ObservedUpdates" -> Lookup[observed, "AbsoluteBatch", 0],
    "ReasonTrainingStopped" -> stopped, "TrainingSeconds" -> totalTrainTime, "WallSeconds" -> elapsed,
    "NNTimeSeconds" -> seconds, "CompletedFullRounds" -> If[IntegerQ[actualUpdates],
      Quotient[actualUpdates, rowCount], Null], "OptimizerStatePreservedDuringRun" -> True,
    "CheckpointResumePreservesOptimizer" -> False|>;
  If[failure === None && !checkpoint[current, Join[stats, <|"AbsoluteBatch" -> actualUpdates,
      "Timestamp" -> DateString["ISODateTime"], "BatchLoss" -> Lookup[observed, "BatchLoss", Null],
      "RoundLoss" -> Replace[result[[6]], None -> Null]|>]],
    failure = "FinalCheckpointFailure"; stats["Status"] = failure];
  compactScaleAtomicExport[FileNameJoin[{run, "training-summary.json"}], stats, "RawJSON"];
  If[failure =!= None, Print["TRAINING STOPPED WITH FAILURE: ", stats];
    Return[<|"Net" -> $Failed, "LastSafeNet" -> lastSafeNet, "Summary" -> stats, "History" -> history|>]];
  <|"Net" -> current, "Policy" -> variableCompactActionPolicy[NetExtract[current, "Policy"], width],
    "Summary" -> stats, "History" -> history|>
];
