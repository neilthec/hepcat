(* Optional native-backend smoke test. Use run-wolfram-tests.py --neural
   so a stalled native call is terminated outside the Wolfram kernel. *)
root = DirectoryName[DirectoryName[$InputFileName]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
Get[FileNameJoin[{root, "tests", "unscrambling.wl"}]];
Print["Initializing variable-length candidate scorer"];
net = NetInitialize[Unscrambling`Private`makePolicy[], RandomSeeding -> 1];
inputs = {<|"State" -> Range[10], "Candidates" -> {{1, 2, 3}, {257, 4, 5}}|>,
  <|"State" -> Table[1 + Mod[i, 256], {i, 1500}], "Candidates" -> {{1, 2}, {3, 4}, {257, 5}}|>};
Print["Training unequal state lengths and candidate counts with fixed padded edit width"];
rows = {Join[inputs[[1]], <|"Target" -> {{1.}, {0.}}, "Weights" -> {{1.}, {1.}}|>],
  Join[inputs[[2]], <|"Target" -> {{0.}, {1.}, {0.}}, "Weights" -> List /@ Unscrambling`Private`balanceWeights[{0, 1, 0}]|>]};
trained = Block[{Unscrambling`Private`$trainingBatchSize = 2}, Unscrambling`Private`trainCandidateNetwork[rows, 1]];
If[!MatchQ[trained, _NetChain | _NetGraph], Quit[1]];
values = trained[#, TargetDevice -> "CPU"] & /@ inputs;
Print[<|"VariableLengthInference" -> values, "Training" -> Head[trained]|>];
Quit[If[Dimensions /@ values === {{2, 1}, {3, 1}} && VectorQ[Flatten[values], NumericQ], 0, 1]];
