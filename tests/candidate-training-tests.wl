(* Training orchestration / checkpoint tests. The optimizer and inference
   scores are stubbed; this does not claim a successful numerical NN run. *)
root = DirectoryName[DirectoryName[$InputFileName]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
Get[FileNameJoin[{root, "tests", "unscrambling.wl"}]];
checks = 0; failures = {};
check[name_, result_] := (checks++; If[!TrueQ[result], AppendTo[failures, name]]);
ang[i_, j_] := SpinorChain[Spinor["Spin", "Angle", i], Spinor["Spin", "Angle", j]];
sq[i_, j_] := SpinorChain[Spinor["Spin", "Square", i], Spinor["Spin", "Square", j]];
rules = Table[Mass[i] -> m, {i, 6}];
amp = ang[1, 2]*sq[5, 6];
pair = {rules, amp};
massEdits = Select[UnscrambleCandidates[pair, "MomentumConservation" -> False], #["Name"] === "Mass" &];
check["mixed chirality amplitudes can scramble", Length[massEdits] > 0];
massState = Unscrambling`Private`applyCandidate[amp, First[massEdits]];
check["symbolic mass not captured by split iterator", FreeQ[
  UnscrambleCandidates[{rules, massState}, "MomentumConservation" -> False], Indeterminate | ComplexInfinity]];
check["every mass insertion has inverse", AllTrue[massEdits,
  Module[{state = Unscrambling`Private`applyCandidate[amp, #], candidates},
    candidates = UnscrambleCandidates[{rules, state}, "MomentumConservation" -> False];
    AnyTrue[candidates, Unscrambling`Private`samePoly[Unscrambling`Private`applyCandidate[state, #], amp] &]
  ] &]];
temp = CreateDirectory[];
Block[{Unscrambling`Private`trainCandidateNetwork, Unscrambling`Private`scoreCandidates},
  Unscrambling`Private`trainCandidateNetwork[rows_, _] := (
    check["optimizer batch size propagated", Unscrambling`Private`$trainingBatchSize === 2];
    check["optimizer device propagated", Unscrambling`Private`$trainingTargetDevice === {"GPU", All}];
    check["optimizer receives both labels", MemberQ[Flatten[Lookup[rows, "Target"], 1], {1.}] && MemberQ[Flatten[Lookup[rows, "Target"], 1], {0.}]];
    check["optimizer receives varying lengths", Length[DeleteDuplicates[Length /@ Lookup[rows, "State"]]] > 1];
    NetInitialize[Unscrambling`Private`makePolicy[], RandomSeeding -> 1]
  );
  Unscrambling`Private`scoreCandidates[ex_, _, edits_] :=
    N[-SpinorExpressionComplexity[Unscrambling`Private`applyCandidate[ex, #]] & /@ edits];
  report = TrainUnscrambleNet[{pair}, Steps -> 1, Scrambles -> 1, HoldOut -> 1,
    MaxTrainingRounds -> 1, EpisodeLength -> 2, Kernels -> 1,
    "ModelDirectory" -> temp, "MomentumConservation" -> False, "TrainingBatchSize" -> 2,
    TargetDevice -> {"GPU", All}];
  check["training returns report", AssociationQ[report]];
  check["batch size reported", report["TrainingBatchSize"] === 2];
  check["phase timings reported", AllTrue[
    Lookup[report, {"DataGenerationSeconds", "TrainingSeconds", "ValidationSeconds"}],
    NumberQ[#] && # >= 0 &]];
  check["reverse labels collected", report["ReverseSteps"] > 0];
  check["holdout executed", report["HoldoutScrambles"] === 1];
  check["saved versioned net", FileExistsQ[report["ModelPath"]]];
  Unscrambling`Private`$modelNet = None; Unscrambling`Private`$loadedModelDirectory = None;
  check["model reload", Unscrambling`Private`loadUnscrambleModel[temp]];
  trace = UnscrambleTrace[pair, "ModelDirectory" -> temp, "MaxSteps" -> 1, "Attempts" -> 1,
    "MomentumConservation" -> False];
  check["loaded model used by inference", trace["ModelLoaded"] && Length[trace["Trace"]] === 1];
  Put[<|"Version" -> 2, "Encoding" -> "FullFormUTF8"|>, Unscrambling`Private`modelPaths[temp]["Metadata"]];
  Unscrambling`Private`$modelNet = None; Unscrambling`Private`$loadedModelDirectory = None;
  check["incompatible metadata rejected", !Unscrambling`Private`loadUnscrambleModel[temp]];
];
DeleteDirectory[temp, DeleteContents -> True];
Print[<|"Checks" -> checks, "Failed" -> Length[failures], "Failures" -> failures|>];
Quit[If[failures === {}, 0, 1]];
