(* Opt-in integration test: two owned symbolic workers, no neural evaluation. *)
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
Print[<|"Checks" -> checks, "Failed" -> Length[failures], "Failures" -> failures|>];
Quit[If[failures === {}, 0, 1]];
