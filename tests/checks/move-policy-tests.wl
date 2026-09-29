(* Reverse-path supervision and state-only neural execution. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
Get[FileNameJoin[{root, "tests", "unscrambling.wl"}]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]]);
ang[i_, j_] := SpinorChain[Spinor["Helicity", "Angle", i], Spinor["Helicity", "Angle", j]];
rules = Table[Mass[i] -> 0, {i, 4}];
amp = ang[1, 2] ang[3, 4];
pair = {rules, amp};
dir = CreateDirectory[];
Block[{Unscrambling`Private`$policyMethod = "ReverseMoves",
    Unscrambling`Private`$trainingBatchSize = 1, Unscrambling`Private`$trainingTargetDevice = "CPU"},
  scr = ComplicateAmplitude[pair, 2, RandomSeed -> 1, "RecordSteps" -> True];
  rows = Flatten[Unscrambling`Private`trainingRows[#["After"], rules, #["Before"]] & /@ Reverse[scr["Trajectory"]], 1];
  check["reverse path produces labels", Length[rows] > 0];
  check["no resulting expression encodings", AllTrue[rows, !KeyExistsQ[#, "Candidates"] &]];
  check["each target has a legal positive", AllTrue[rows, Total[Flatten[#["Target"]]] >= 1 &]];
  Do[
    cands = UnscrambleCandidates[{rules, step["After"]}];
    row = Unscrambling`Private`trainingRows[step["After"], rules, step["Before"]];
    If[row =!= {},
      positives = Flatten[Position[Flatten[First[row]["Target"]], 1.]];
      check["positive actions undo recorded step", AllTrue[positives,
        Unscrambling`Private`samePoly[Unscrambling`Private`applyCandidate[step["After"], cands[[#]]], step["Before"]] &]]],
    {step, scr["Trajectory"]}];
  stop = First[Unscrambling`Private`trainingRows[amp, rules, amp]];
  check["stop label", stop["Target"][[stop["ActionCount"], 1]] == 1];
  check["unused slots do not contribute to loss", Total[Flatten[Drop[stop["Weights"], stop["ActionCount"]]]] == 0];
  Block[{Unscrambling`Private`$moveSlots = 1},
    check["overflow fails instead of truncating", Quiet[Unscrambling`Private`trainingRows[amp, rules, amp]] === $Failed]];
  net = Unscrambling`Private`trainCandidateNetwork[Append[rows, stop], 1];
  check["state-only network trains", Head[net] === NetGraph];
  check["no candidate encoder", FreeQ[Information[net, "ArraysPositionList"], "CandidateEncoder"]];
  Unscrambling`Private`$modelNet = net;
  cands = UnscrambleCandidates[pair];
  scores = Unscrambling`Private`scoreCandidates[amp, rules, cands];
  check["only legal slots scored", Length[scores] === Length[cands] && VectorQ[scores, NumericQ]];
  paths = Unscrambling`Private`modelPaths[dir];
  Export[paths["Net"], net];
  Put[<|"Encoding" -> "SplitFullFormUTF8", "Architecture" -> "ReverseMoveSlots", "MoveSlots" -> 512,
    "MoveSourceHash" -> FileHash[FileNameJoin[{root, "tests", "unscrambling.wl"}], "SHA256"]|>, paths["Metadata"]];
  Unscrambling`Private`$loadedModelDirectory = None;
  check["move model reloads", Unscrambling`Private`loadUnscrambleModel[dir]];
  check["reload scores stable", Max[Abs[scores - Unscrambling`Private`scoreCandidates[amp, rules, cands]]] < 10^-6];
  trace = UnscrambleTrace[pair, "PolicyMethod" -> "ReverseMoves", "ModelDirectory" -> dir, "MaxSteps" -> 2];
  check["public move inference", TrueQ[trace["ModelLoaded"]] && !TrueQ[trace["ScoringFailed"]]];
  Block[{Unscrambling`Private`$policyMethod = "CandidateScorer"},
    check["separate files", Unscrambling`Private`modelPaths[dir] =!= paths];
    check["no fallback to wrong method", !Unscrambling`Private`loadUnscrambleModel[dir]]];
];
check["default unchanged", ("PolicyMethod" /. Options[TrainUnscrambleNet]) === "CandidateScorer"];
check["invalid method rejected", Quiet[TrainUnscrambleNet[{pair}, "PolicyMethod" -> "Unknown"]] === $Failed];
report = TrainUnscrambleNet[{pair}, "PolicyMethod" -> "ReverseMoves", Steps -> 2,
  Scrambles -> 1, HoldOut -> 1, MaxTrainingRounds -> 1, EpisodeLength -> 2,
  Kernels -> 1, "ModelDirectory" -> dir];
check["public training pipeline", AssociationQ[report] && report["PolicyMethod"] === "ReverseMoves" &&
  report["ReverseSteps"] > 0 && report["HoldoutScrambles"] === 1];
check["scorer model not overwritten", !FileExistsQ[FileNameJoin[{dir, "unscramble.wlnet"}]]];
check["separate validation report", FileExistsQ[FileNameJoin[{dir, "move-validation-results.m"}]]];
DeleteDirectory[dir, DeleteContents -> True];
Print[<|"Checks" -> checks, "Failed" -> Length[failures], "Failures" -> failures|>];
Quit[If[failures === {}, 0, 1]];
