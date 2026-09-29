root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
Get[FileNameJoin[{root, "tests", "unscrambling.wl"}]];
Unscrambling`Private`$maskCandidatePadding = True;
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]]);
check["masking is opt in", ("MaskPadding" /. Options[TrainUnscrambleNet]) === False];
check["invalid masking rejected", Quiet[TrainUnscrambleNet[{}, "MaskPadding" -> "yes"], TrainUnscrambleNet::mask] === $Failed];
close[a_, b_] := VectorQ[Flatten[{a}], NumericQ] && VectorQ[Flatten[{b}], NumericQ] &&
  Max[Abs[Flatten[{a - b}]]] < 10^-6;
encoder = NetInitialize[Unscrambling`Private`makeMaskedEncoder[], RandomSeeding -> 1];
reference = encoder[{1, 2, 256}];
sequences = {{257, 1, 2, 256}, {1, 2, 256, 257}, {1, 257, 2, 257, 256},
  Join[ConstantArray[257, 20], {1, 2, 256}]};
Do[check["padding placement " <> ToString[i], close[reference, encoder[sequences[[i]]]]], {i, Length[sequences]}];
check["all padding leaves zero state", close[encoder[{257, 257}], ConstantArray[0., 64]]];
check["real content still matters", !close[reference, encoder[{1, 3, 256}]]];
rows = {
  <|"State" -> {1, 2}, "Candidates" -> {{257, 1, 2}, {3, 4, 5}}, "Target" -> {{1.}, {0.}}, "Weights" -> {{1.}, {1.}}|>,
  <|"State" -> {3, 4}, "Candidates" -> {{1, 2, 3, 4, 5}}, "Target" -> {{1.}}, "Weights" -> {{1.}}|>};
batch = Unscrambling`Private`prepareTrainingBatch[rows];
trained = NetTrain[NetInitialize[batch["Net"], RandomSeeding -> 1], batch["Rows"],
  MaxTrainingRounds -> 2, BatchSize -> 1, TrainingProgressReporting -> None,
  Method -> {"ADAM", "LearningRate" -> .001}];
check["masked graph trains", MatchQ[trained, _NetGraph]];
If[MatchQ[trained, _NetGraph],
  policy = Unscrambling`Private`variableWidthPolicy[NetExtract[trained, "Policy"]];
  input = KeyTake[First[rows], {"State", "Candidates"}];
  padded = Join[input, <|"Candidates" -> (PadLeft[#, 25, 257] & /@ input["Candidates"])|>];
  check["trained scores invariant", close[policy[input], policy[padded]]];
  check["trained rankings invariant", Ordering[Flatten[policy[input]]] === Ordering[Flatten[policy[padded]]]];
  path = FileNameJoin[{$TemporaryDirectory, "masked-test-" <> CreateUUID[] <> ".wlnet"}];
  Export[path, policy]; restored = Import[path]; DeleteFile[path];
  check["serialized model invariant", close[restored[input], restored[padded]]];
];
Print[<|"Checks" -> checks, "Failed" -> Length[failures], "Failures" -> failures|>];
Quit[If[failures === {}, 0, 1]];
