(* Real numerical training and worker validation; models live only in a temporary directory. *)
root = DirectoryName[DirectoryName[$InputFileName]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
Get[FileNameJoin[{root, "tests", "unscrambling.wl"}]];
Get[FileNameJoin[{root, "tests", "training-notebook-helpers.wl"}]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]]);
ang[i_, j_] := SpinorChain[Spinor["Helicity", "Angle", i], Spinor["Helicity", "Angle", j]];
pair = {Table[Mass[i] -> 0, {i, 6}], ang[1, 2]*ang[3, 4]*ang[5, 6]};
directory = CreateDirectory[];
before = Kernels[];
Print["Training six-leg example with two workers, then real numerical validation"];
report = TrainUnscrambleNet[{pair}, Steps -> 1, Scrambles -> 1, HoldOut -> 1,
  MaxTrainingRounds -> 1, EpisodeLength -> 2, Kernels -> 2,
  "TrainingBatchSize" -> 2, "ModelDirectory" -> directory,
  "MomentumConservation" -> False, "ValidationTimeLimit" -> 30];
check["pipeline returned report", AssociationQ[report]];
check["owned workers closed", Kernels[] === before];
If[AssociationQ[report],
  Print[trainingRunSummary[report, <|"Amplitudes" -> 1, "Scrambles" -> 1, "Steps" -> 1, "Rounds" -> 1|>]];
  Print[KeyTake[report, {"TrainingStates", "TrainingRows", "ReverseSteps", "HoldoutScrambles",
    "DataGenerationSeconds", "TrainingSeconds", "ValidationSeconds", "TotalWallSeconds"}]];
  check["reverse training examples", report["ReverseSteps"] > 0];
  check["real validation succeeded", Length[report["ValidationJobs"]] === 1 &&
    AllTrue[report["ValidationJobs"], TrueQ[#["Success"]] &]];
  Unscrambling`Private`$modelNet = None;
  Unscrambling`Private`$loadedModelDirectory = None;
  check["saved model reloaded", Unscrambling`Private`loadUnscrambleModel[directory]];
  values = Unscrambling`Private`$modelNet[
    <|"State" -> Range[10], "Candidates" -> {{1, 2, 3}, {257, 4, 5}}|>, TargetDevice -> "CPU"];
  check["reloaded model produces numerical scores", Dimensions[values] === {2, 1} && VectorQ[Flatten[values], NumericQ]];
];
DeleteDirectory[directory, DeleteContents -> True];
Print[<|"Checks" -> checks, "Failed" -> Length[failures], "Failures" -> failures|>];
Quit[If[failures === {}, 0, 1]];
