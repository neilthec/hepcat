root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
SetEnvironment["HEPCAT_TEST_ROOT" -> root];
notebook = Get[FileNameJoin[{root, "tests", "SM-4-point-unscramble-test.nb"}]];
inputs = Cases[notebook, Cell[benchmarkCellText_String, "Input", ___] :> benchmarkCellText, Infinity];
Scan[ToExpression, Take[inputs, 3]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]]);
check["same process and reference coverage", Length[ubCases] === 73 && Total[Length /@ Lookup[ubCases, "Channels"]] === 148];
check["all references resolve", And @@ Flatten[Table[
  !MissingQ[ubReference[test["SourceName"], channel]], {test, ubCases}, {channel, test["Channels"]}]]];
check["all permutations enabled", particlePermutations === Permutations[Range[4]]];
check["compact model selected by default", unscrambleModel === "CompactActions"];
check["all ingoing labels", AllTrue[ubCases, #["Name"] === StringRiffle[#["Particles"], ", "] &]];
check["status colors", ubStatus["PASS"] === Style["PASS", Bold, Darker[Green]] &&
  ubStatus["Unresolved"] === Style["Unresolved", Bold, Red]];
check["channel label follows permutation", ubChannel["Z:T", {2, 1, 3, 4}] === "Z:U"];
permutation = {3, 1, 4, 2};
probe = {Mass[Multiparticle[1, 2]] -> MZ,
  7 Spinor["Spin", "Angle", 1, 2]^2 MomHat[2] xFactor[Multiparticle[1, 4], 3] +
  Mandelstahm[2, 4] MomProd[1, 3] + Mom[Multiparticle[2, 3]] + Spinor["Xi", "Angle", 10]};
check["typed relabeling", ubRelabel[probe, permutation] === {
  Mass[Multiparticle[3, 1]] -> MZ,
  7 Spinor["Spin", "Angle", 3, 2]^2 MomHat[1] xFactor[Multiparticle[3, 2], 4] +
  Mandelstahm[1, 2] MomProd[3, 4] + Mom[Multiparticle[1, 4]] + Spinor["Xi", "Angle", 10]}];
check["all permutations invert", AllTrue[particlePermutations,
  ubRelabel[ubRelabel[probe, #], Ordering[#]] === probe &]];
check["hatted objects relabeled", ubRelabel[
  SpinorHat["Spin", "Up", "Angle", 1, 2] MomProdHat12[2, 4], permutation] ===
  SpinorHat["Spin", "Up", "Angle", 3, 2] MomProdHat12[1, 2]];
check["no legacy simplifier in runner", !StringContainsQ[inputs[[3]], "SimplifySpinorProducts"]];
verificationSeconds = 1;
check["same expression accepted", ubCompare[EE, EE, {0, 0, 0, 0}]["Equivalence"] === "Exact"];
check["algebraically equivalent accepted", ubCompare[(EE + GG)^2, EE^2 + 2 EE GG + GG^2,
  {0, 0, 0, 0}]["Equivalence"] === "Proved"];
check["wrong sign is not a pass", ubCompare[EE, -EE, {0, 0, 0, 0}]["Equivalence"] === "OppositeSign"];
check["unproved comparison stays unresolved", ubCompare[EE, GG, {0, 0, 0, 0}]["Equivalence"] === "Unresolved"];
test = First[ubCases];
diagrams = CreateDiagrams[test["Particles"], ubModel];
photon = ubSelect[diagrams, "A", False];
check["real diagram selection", Length[photon] > 0 && AllTrue[photon, ubMediator[#] === "A" &]];
check["sum all selection", ubSelect[diagrams, "unused", True] === diagrams];
Block[{ubSolve},
  Clear[ubSolve];
  sentPairs = {};
  ubSolve[p_] := (AppendTo[sentPairs, p]; <|"Result" -> p, "ModelLoaded" -> True, "ScoringFailed" -> False,
    "TerminationReason" -> "Test"|>);
  solved = ubSimplifyDiagrams[photon, test["Masses"]];
  check["raw diagrams retained", AssociationQ[solved] && KeyExistsQ[solved, "Raw"] && KeyExistsQ[solved, "Pieces"]];
  contractSpinIndices = False;
  rawSolved = ubSimplifyDiagrams[photon, test["Masses"]];
  check["raw input option", rawSolved["Raw"] === rawSolved["Prepared"]];
  permutedSolved = ubSimplifyDiagrams[photon, test["Masses"], permutation];
  check["solver input permuted", permutedSolved["Prepared"] === ubRelabel[rawSolved["Prepared"], permutation]];
  contractSpinIndices = True;
  multiDiagrams = ubSelect[CreateDiagrams[ubCases[[2, "Particles"]], ubModel], "A", False];
  sentPairs = {};
  multiSolved = ubSimplifyDiagrams[multiDiagrams, ubCases[[2, "Masses"]]];
  check["channel sums have only external mass rules", Length[sentPairs] > 1 &&
    Last[sentPairs][[1]] === Thread[(Mass /@ Range[4]) -> ubCases[[2, "Masses"]]]];
  ubSolve[p_] := $Failed;
  check["solver failure not accepted", FailureQ[ubSimplifyDiagrams[photon, test["Masses"]]]];
];
Block[{UnscrambleTrace, unscrambleModel},
  Clear[UnscrambleTrace];
  sentModelOptions = {};
  UnscrambleTrace[p_, opts___Rule] := (AppendTo[sentModelOptions, Association[{opts}]];
    <|"Result" -> p, "ModelLoaded" -> True, "ScoringFailed" -> False,
      "TerminationReason" -> "Test"|>);
  Do[
    unscrambleModel = choice;
    sentModelOptions = {};
    selectedSolved = ubSimplifyDiagrams[photon, test["Masses"]];
    check[choice <> " used for single-group diagrams", AssociationQ[selectedSolved] &&
      Length[sentModelOptions] === Length[selectedSolved["Pieces"]] &&
      AllTrue[sentModelOptions, Lookup[#, "Model"] === choice &]];
    check[choice <> " preserves solver limits and model directory", AllTrue[sentModelOptions,
      Lookup[#, {"MaxSteps", "Attempts", "TimeLimit", "ModelDirectory"}] ===
        {maxSteps, attempts, solverSeconds, FileNameJoin[{hepcatDirectory, "tests"}]} &]];
    sentModelOptions = {};
    selectedSolved = ubSimplifyDiagrams[multiDiagrams, ubCases[[2, "Masses"]], permutation];
    check[choice <> " used for every permuted piece and channel sum", AssociationQ[selectedSolved] &&
      Length[sentModelOptions] === Length[selectedSolved["Pieces"]] + 1 &&
      AllTrue[sentModelOptions, Lookup[#, "Model"] === choice &]],
    {choice, {"CandidateScorer", "CompactActions"}}
  ];
];
check["missing diagrams not zero success", ubResult[test, First[test["Channels"]], {}]["Status"] === "MissingDiagrams"];
Block[{ubResult, ubCases = {First[ubCases]}, particlePermutations = Permutations[Range[4]], Print},
  Clear[ubResult];
  ubResult[t_, k_, d_, p_] := <|"Process" -> t["Name"], "Channel" -> k,
    "Permutation" -> p, "Status" -> "PASS"|>;
  ubRun[];
  check["every channel and permutation visited", Length[ubResults] === 72 &&
    Length[DeleteDuplicates[Lookup[ubResults, "Permutation"]]] === 24];
  check["stored statuses remain strings", AllTrue[Lookup[ubResults, "Status"], StringQ]];
];
Print[<|"Checks" -> checks, "Failed" -> Length[failures], "Failures" -> failures|>];
Quit[If[failures === {}, 0, 1]];
