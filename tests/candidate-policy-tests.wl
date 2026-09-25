(* Symbolic tests only: never starts the neural backend or writes model files. *)
root = DirectoryName[DirectoryName[$InputFileName]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{Global`$HEPCATpath, "HEPCAT.wl"}]];
Get[FileNameJoin[{root, "tests", "unscrambling.wl"}]];
failures = {}; checks = 0;
check[name_, result_] := (checks++; If[!TrueQ[result], AppendTo[failures, name]]);
ang[a_, b_] := SpinorChain[Spinor["Helicity", "Angle", a], Spinor["Helicity", "Angle", b]];
sixRules = Table[Mass[i] -> 0, {i, 6}];
amp = ang[1, 2]*ang[3, 4]*ang[5, 6];
pair = {sixRules, amp};
cands = UnscrambleCandidates[pair, "MomentumConservation" -> False];
check["six leg candidates", Length[cands] >= 4];
check["leg 6 edits", AnyTrue[cands, !FreeQ[#["Subtract"], Spinor[_, _, 6]] && #["Name"] =!= "Stop" &]];
check["stop", Last[cands]["Name"] === "Stop"];

sc1 = SpinorChain[Spinor["Spin", "Upper", "Angle", 1, ii], Mom[7], Mom[8], Mom[9], Mom[10], Spinor["Helicity", "Angle", 2]];
sc2 = SpinorChain[Spinor["Helicity", "Angle", 5], Mom[11], Mom[12], Mom[13], Mom[14], Spinor["Spin", "Lower", "Angle", 6, jj]];
splitCands = UnscrambleCandidates[{sixRules, sc1*sc2}, "MomentumConservation" -> False];
check["more than four split choices", Length[Select[splitCands, #["Name"] === "Schouten" &]] > 4];
check["late split retained", AnyTrue[splitCands, Lookup[#, "Splits", None] === {4, 4} &]];

encoded = UnscrambleEncoding[pair, First[cands]];
decoded = FromCharacterCode[encoded["State"] - 1, "UTF8"];
check["valid bytes", AllTrue[Values[encoded], VectorQ[#, IntegerQ[#] && 1 <= # <= 256 &] &]];
check["tree boundaries", StringContainsQ[decoded, "SpinorChain["] && StringContainsQ[decoded, "Rule["]];
renamedRules = Table[Mass[i + 100] -> 0, {i, 6}];
rename[ex_] := ex /. {Spinor[k_, t_, i_Integer] :> Spinor[k, t, i + 100], Mass[i_Integer] :> Mass[i + 100]};
renamed = UnscrambleEncoding[{renamedRules, rename[amp]}, rename[First[cands]]];
check["sparse label invariance", encoded === renamed];

longAmp = Total[Table[z[i]*ang[1, 2]*ang[3, 4]*ang[5, 6], {i, 90}]];
stop = Unscrambling`Private`stopMove[];
longEncoded = UnscrambleEncoding[{sixRules, longAmp}, stop];
longText = FromCharacterCode[longEncoded["State"] - 1, "UTF8"];
check["no sequence cutoff", Length[longEncoded["State"]] > 5000];
check["late expression content", StringContainsQ[longText, "z[90]"]];
changed = UnscrambleEncoding[{sixRules, longAmp /. z[90] -> tailMarker}, stop];
check["tail changes input", longEncoded =!= changed && StringContainsQ[FromCharacterCode[changed["State"] - 1, "UTF8"], "tailMarker"]];
check["masses affect input", encoded =!= UnscrambleEncoding[{sixRules /. (Mass[6] -> 0) -> (Mass[6] -> m6), amp}, First[cands]]];
check["split sites affect input", UnscrambleEncoding[{sixRules, sc1*sc2}, First[splitCands]] =!=
  UnscrambleEncoding[{sixRules, sc1*sc2}, Join[First[splitCands], <|"Splits" -> {99, 99}|>]]];

shellPair = {Join[sixRules, {Mass[Multiparticle[1, 2]] -> mz}], MomProd[1, 2]*amp};
check["automatic on shell channel", MemberQ[Lookup[UnscrambleCandidates[shellPair], "Name"], "OnShell"]];
check["optional on shell override", FreeQ[Lookup[UnscrambleCandidates[shellPair, "OnShellChannels" -> {}], "Name"], "OnShell"]];
shellCandidate = First[UnscrambleCandidates[shellPair]];
check["automatic conditions encoded", UnscrambleEncoding[shellPair, shellCandidate] ===
  UnscrambleEncoding[shellPair, shellCandidate, "OnShellChannels" -> {{1, 2}}]];
check["disabled conditions encoded distinctly", UnscrambleEncoding[shellPair, shellCandidate] =!=
  UnscrambleEncoding[shellPair, shellCandidate, "OnShellChannels" -> {}]];
check["explicit on shell channel", MemberQ[Lookup[UnscrambleCandidates[shellPair, "OnShellChannels" -> {{1, 2}}], "Name"], "OnShell"]];
momAmp = SpinorChain[Spinor["Helicity", "Angle", 5], Mom[6], Spinor["Helicity", "Square", 2]];
check["six leg momentum conservation", AnyTrue[UnscrambleCandidates[{sixRules, momAmp}],
  #["Name"] === "MomentumConservation" && !FreeQ[#["Insert"], Mom[5]] &]];
check["momentum assumption switch", FreeQ[Lookup[UnscrambleCandidates[{sixRules, momAmp}, "MomentumConservation" -> False], "Name"], "MomentumConservation"]];

(* Select any position in the dynamic list, not an encoded particle-label tuple. *)
Block[{Unscrambling`Private`scoreCandidates, Unscrambling`Private`$useMomentumConservation = False},
  Unscrambling`Private`scoreCandidates[_, _, moves_] := Table[Boole[i === Length[moves] - 1], {i, Length[moves]}];
  selected = Unscrambling`Private`selectCandidate[Unscrambling`Private`canonicalChains[sc1*sc2], sixRules, False];
  check["dynamic selection", selected["Index"] > 4 && selected["Index"] === Length[splitCands] - 1 && selected["Candidate"] === splitCands[[-2]]]
];

scr = ComplicateAmplitude[pair, 2, RandomSeed -> 3, "RecordSteps" -> True, "MomentumConservation" -> False];
check["trajectory recorded", Length[scr["Trajectory"]] === scr["Steps"] && scr["Steps"] === 2];
check["same scrambling actions", AllTrue[scr["Trajectory"],
  MemberQ[UnscrambleCandidates[{sixRules, #["Before"]}, "MomentumConservation" -> False], #["Candidate"]] &]];
rows = Block[{Unscrambling`Private`$useMomentumConservation = False},
  Unscrambling`Private`trainingRows[scr["Trajectory"][[1, "After"]], sixRules, amp]];
check["reverse supervised rows", rows =!= {} && MemberQ[First[rows]["Target"], {1.}] && MemberQ[First[rows]["Target"], {0.}]];
check["training input not truncated", AllTrue[Lookup[rows, "State"], Length[#] > 64 &]];
check["versioned model files", StringContainsQ[Unscrambling`Private`modelPaths["tmp"]["Net"], "shared-v3"]];
missingModelDirectory = CreateDirectory[];
check["missing model rejected", !Unscrambling`Private`loadUnscrambleModel[missingModelDirectory]];
DeleteDirectory[missingModelDirectory];
check["invalid inference options", Quiet[UnscrambleTrace[pair, "MaxSteps" -> -1]] === $Failed];
check["invalid training options", Quiet[TrainUnscrambleNet[{pair}, Steps -> -1]] === $Failed];
Print[<|"Checks" -> checks, "Failed" -> Length[failures], "Failures" -> failures|>];
Quit[If[failures === {}, 0, 1]];
