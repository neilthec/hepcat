(* Opt-in integration test: two owned symbolic workers, no neural evaluation. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
Get[FileNameJoin[{root, "tests", "unscrambling.wl"}]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]]);
ang[i_, j_] := SpinorChain[Spinor["Helicity", "Angle", i], Spinor["Helicity", "Angle", j]];
pair = {Table[Mass[i] -> 0, {i, 6}], ang[1, 2]*ang[3, 4]*ang[5, 6]};
settings = <|"OnShellChannels" -> Automatic, "MomentumConservation" -> False,
  "JobTimeLimit" -> 10, "ValidationTimeLimit" -> 1|>;
jobs = Unscrambling`Private`makeTrainingJobs[{pair, pair}, 4, 12];
{serialSeconds, serial} = AbsoluteTiming[Unscrambling`Private`mapTrainingJobs[jobs, {}, settings]];
before = Kernels[];
Print["Launching exactly two owned test workers"];
result = Unscrambling`Private`withTrainingWorkers[2, root, 1, Unscrambling`Private`pipelineFingerprint[root],
  Function[workers, Module[{parallel, elapsed, validation, vjobs},
    {elapsed, parallel} = AbsoluteTiming[Unscrambling`Private`mapTrainingJobs[jobs, workers, settings]];
    check["serial parallel data identical", Lookup[parallel, "Result"] === Lookup[serial, "Result"]];
    check["two workers used", Length[DeleteDuplicates[Lookup[parallel, "Worker"]]] === 2];
    borrowed = Unscrambling`Private`withTrainingWorkers[workers, root, 1,
      Unscrambling`Private`pipelineFingerprint[root], Function[pool, Length[pool]]];
    check["borrowed workers remain available", borrowed === 2 && AllTrue[workers, MemberQ[Kernels[], #] &]];
    check["model broadcast to validation workers", Unscrambling`Private`installValidationModel[workers,
      NetInitialize[Unscrambling`Private`makePolicy[], RandomSeeding -> 1], "test-model"]];
    check["workers hold initialized model", ParallelEvaluate[
      MatchQ[Unscrambling`Private`$modelNet, _NetGraph] && Unscrambling`Private`$loadedModelDirectory === "test-model",
      workers, DistributedContexts -> None] === {True, True}];
    (* Stub only numerical inference; retain real validation scrambling and dispatch. *)
    ParallelEvaluate[
      Clear[Unscrambling`Private`runUnscramble];
      Unscrambling`Private`runUnscramble[p_, ___] := <|"ModelLoaded" -> True,
        "Result" -> p, "TerminationReason" -> "Test"|>, workers, DistributedContexts -> None];
    vjobs = Unscrambling`Private`makeValidationJobs[{pair, pair}, 1, 3, 2, 2];
    validation = Unscrambling`Private`mapTrainingJobs[vjobs, workers, settings];
    check["parallel validation jobs complete", Unscrambling`Private`validJobResultsQ[validation] && Length[validation] === 4];
    Print[<|"SerialGenerationSeconds" -> serialSeconds, "ParallelGenerationSeconds" -> elapsed|>];
    True
  ]]];
check["worker run succeeded", result === True];
check["owned workers closed", Kernels[] === before];

(* Real pinned worker channels and legal algebra; only numerical scores are replaced. *)
beforeInference = Kernels[];
inferencePair = {{Mass[1] -> m1, Mass[2] -> m2},
  SpinorChain[Spinor["Spin", "Angle", 1], Mom[1], Spinor["Spin", "Square", 2]]};
Block[{Unscrambling`Private`initializeInferenceWorkers, Unscrambling`Private`loadUnscrambleModel},
  Unscrambling`Private`loadUnscrambleModel[_] := True;
  Unscrambling`Private`initializeInferenceWorkers[workers_, _, settings_, _] := With[{testRoot = root},
    ParallelEvaluate[
      Global`$HEPCATpath = FileNameJoin[{testRoot, "source"}];
      Block[{$Output = {}}, Get[FileNameJoin[{Global`$HEPCATpath, "HEPCAT.wl"}]];
        Get[FileNameJoin[{testRoot, "tests", "unscrambling.wl"}]]];
      Unscrambling`Private`$policyMethod = "CandidateScorer";
      Unscrambling`Private`$onShellChannels = Automatic;
      Unscrambling`Private`$useMomentumConservation = False;
      Clear[Unscrambling`Private`scoreCandidates];
      Unscrambling`Private`scoreCandidates[expression_, rules_, candidates_] := (Pause[0.2];
        (-Unscrambling`Private`expressionComplexity[
          Unscrambling`Private`applyCandidate[expression, #]] &) /@ candidates);
      True, workers, DistributedContexts -> None] === ConstantArray[True, Length[workers]]
  ];
  inferenceResult = Unscrambling`Private`withInferenceWorkers[2, "test-model", <||>, 45.,
    Function[workers, Module[{requests, pending, replies = {}, received, timedOut = False, start, slow, next, checked},
      requests = Table[<|"Expression" -> inferencePair[[2]], "Rules" -> inferencePair[[1]],
        "RouteID" -> route, "Generation" -> 1, "Seed" -> 20261006,
        "DrawIndex" -> 0, "FirstChoice" -> route, "Seconds" -> 10.,
        "Temperature" -> Unscrambling`Private`inferenceRouteTemperature["CandidateScorer", route, 2],
        "VisitedStateKeys" -> {Unscrambling`Private`inferenceStateKey[inferencePair[[2]]]},
        "MoveVisits" -> <||>, "ExcludedMoveKeys" -> {}|>, {route, 2}];
      pending = MapThread[Unscrambling`Private`inferenceSubmit, {workers, requests}];
      start = AbsoluteTime[];
      While[pending =!= {} && AbsoluteTime[] - start < 20.,
        received = Unscrambling`Private`inferenceReceive[pending, 0.1];
        If[TrueQ[received["Ready"]],
          AppendTo[replies, received["Result"]];
          pending = DeleteCases[pending, received["Handle"]]]
      ];
      If[pending =!= {}, timedOut = True; Unscrambling`Private`cancelInference[pending, workers]];
      check["two pinned inference workers return actual legal edits",
        !timedOut && Length[replies] === 2 &&
          AllTrue[replies, MemberQ[{"Edited", "Stop"}, #["Status"]] &]];
      check["worker results retain independent route and generation tags",
        Sort[Lookup[replies, "RouteID"]] === {1, 2} &&
          Lookup[replies, "Generation"] === {1, 1} &&
          Length[DeleteDuplicates[Lookup[replies, "WorkerID"]]] === 2];
      check["independent inference worker edits overlap in wall time",
        Length[replies] === 2 && Max[Lookup[replies, "StartedAt"]] < Min[Lookup[replies, "FinishedAt"]]];
      check["distinct initial ranks explore different actual moves",
        Length[DeleteDuplicates[Lookup[Lookup[replies, "Selection"], "Index"]]] === 2];
      check["real worker selections carry local-loop and shared-fallback metadata",
        AllTrue[replies, IntegerQ[#["LoopAvoided"]] &&
          MemberQ[{True, False}, #["SharedMoveFallback"]] &&
          !KeyExistsQ[#["Selection"], "After"] &]];
      check["legal worker edit reduces the contraction",
        Min[Lookup[replies, "ComplexityAfter"]] < Unscrambling`Private`expressionComplexity[inferencePair[[2]]]];
      (* Cancellation must drain the old result before the same channel is reused. *)
      ParallelEvaluate[
        Clear[Unscrambling`Private`selectInferenceCandidate];
        Unscrambling`Private`selectInferenceCandidate[expression_, _, request_] := (
          If[request["Generation"] === 2, Pause[5]];
          <|"Candidate" -> Unscrambling`Private`stopMove[], "Index" -> 1, "Count" -> 1, "Score" -> 0.|>),
        workers, DistributedContexts -> None];
      slow = Unscrambling`Private`inferenceSubmit[First[workers], Join[First[requests], <|"Generation" -> 2|>]];
      Pause[0.1];
      check["targeted inference cancellation succeeds",
        Unscrambling`Private`cancelInference[{slow}, workers]];
      next = Unscrambling`Private`inferenceSubmit[First[workers], Join[First[requests], <|"Generation" -> 3|>]];
      received = Unscrambling`Private`inferenceReceive[{next}, 10.];
      check["cancelled worker channel returns only the new generation",
        TrueQ[received["Ready"]] && received["Result"]["Generation"] === 3 &&
          received["Result"]["Status"] === "Stop"];
      True
    ]]];
  check["owned inference worker run completes", inferenceResult === True];
  callerPool = LaunchKernels[1, ProgressReporting -> False];
  If[Length[callerPool] === 1,
    CheckAbort[
      ParallelEvaluate[Global`hepcatInferenceCallerMarker = 20261006, callerPool, DistributedContexts -> None];
      beforePublicInference = Kernels[];
      publicRequests = {};
      publicSubmitDefinitions = DownValues[Unscrambling`Private`inferenceSubmit];
      Block[{Unscrambling`Private`inferenceSubmit},
        Unscrambling`Private`inferenceSubmit[worker_, request_] := (
          AppendTo[publicRequests, request];
          Block[{Unscrambling`Private`inferenceSubmit},
            DownValues[Unscrambling`Private`inferenceSubmit] = publicSubmitDefinitions;
            Unscrambling`Private`inferenceSubmit[worker, request]]
        );
        publicResult = UnscrambleTrace[inferencePair, "Kernels" -> 2, "Model" -> "CandidateScorer",
          "MomentumConservation" -> False, "MaxSteps" -> 1, "Attempts" -> 1,
          "MaxStagnantSteps" -> 1, "TimeLimit" -> 45., "ProgressReporting" -> False,
          RandomSeed -> 20261006];
      ];
      check["real public parallel inference accepts a legal global record",
        AssociationQ[publicResult] && publicResult["Restarts"] >= 1 &&
          Unscrambling`Private`expressionComplexity[publicResult["Result"][[2]]] <
            Unscrambling`Private`expressionComplexity[inferencePair[[2]]]];
      publicGroups = GroupBy[publicRequests, #["Generation"] &];
      check["real public inference dispatches a greedy anchor and distinctly tempered exploration",
        Lookup[Take[First[Values[publicGroups]], 2], "Greedy"] === {True, False} &&
          Lookup[Take[First[Values[publicGroups]], 2], "Temperature"] === {0.1, 0.4}];
      If[AssociationQ[publicResult] && publicResult["Checkpoints"] =!= {} && Length[publicGroups] >= 2,
        firstRestartGeneration = First[Keys[publicGroups]] + 1;
        firstSharedBest = First[publicResult["Checkpoints"]]["After"];
        check["real coordinator restarts both workers at the same new checkpoint",
          Lookup[publicGroups[firstRestartGeneration], "Expression"] === {firstSharedBest, firstSharedBest}];
        check["real record restart clears histories and shared usage in both worker requests",
          AllTrue[publicGroups[firstRestartGeneration],
            #["VisitedStateKeys"] === {Unscrambling`Private`inferenceStateKey[firstSharedBest]} &&
              #["MoveVisits"] === <||> && #["ExcludedMoveKeys"] === {} &]],
        check["real coordinator restarts both workers at the same new checkpoint", False];
        check["real record restart clears histories and shared usage in both worker requests", False]
      ];
      check["public inference preserves a connected caller worker and its state",
        Kernels[] === beforePublicInference &&
          ParallelEvaluate[Global`hepcatInferenceCallerMarker, callerPool,
            DistributedContexts -> None] === {20261006}];
      CloseKernels[callerPool],
      CloseKernels[callerPool]; Abort[]
    ],
    check["separate caller worker available for preservation test", False]
  ];
];
check["inference workers are closed after the test", Kernels[] === beforeInference];
Print[<|"Checks" -> checks, "Failed" -> Length[failures], "Failures" -> failures|>];
Quit[If[failures === {}, 0, 1]];
