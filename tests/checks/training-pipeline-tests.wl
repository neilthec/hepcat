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
jobs = Unscrambling`Private`makeTrainingJobs[{pair, pair}, 2, 2];
check["independent original and scramble jobs", Length[jobs] === 6 && Lookup[jobs, "Seed"] === {0, 1, 2, 0, 1, 2}];
check["original numbering retained", jobs[[1, "Pair"]] === pair];
check["permutations deterministic", jobs === Unscrambling`Private`makeTrainingJobs[{pair, pair}, 2, 2]];
check["permutations are bijections", AllTrue[jobs,
  Sort[Keys[#["LegPermutation"]]] === Sort[Values[#["LegPermutation"]]] &]];
manyJobs = Unscrambling`Private`makeTrainingJobs[{pair}, 1, 24];
check["training has label diversity", Length[DeleteDuplicates[Lookup[manyJobs, "LegPermutation"]]] > 1];
check["permuting jobs preserves caller random stream",
  BlockRandom[SeedRandom[77]; Unscrambling`Private`makeTrainingJobs[{pair}, 1, 3]; RandomInteger[100000]] ===
  BlockRandom[SeedRandom[77]; RandomInteger[100000]]];
indexedPair = {{Mass[10] -> Me, Mass[20] -> Mm, Mass[30] -> MW,
    Mass[Multiparticle[10, 20]] -> MZ},
  7 SpinorChain[Spinor["Spin", "Angle", 10, 2], Mom[20], Spinor["Spin", "Square", 30, 1]]^2 +
    MomProd[10, 20] Mandelstahm[20, 30]/PropDen[Mom[Multiparticle[10, 20]], MZ]};
ids = <|10 -> 30, 20 -> 10, 30 -> 20|>;
relabeled = Unscrambling`Private`relabelLegs[indexedPair, ids];
check["consistent typed relabeling", relabeled === {
  {Mass[30] -> Me, Mass[10] -> Mm, Mass[20] -> MW, Mass[Multiparticle[30, 10]] -> MZ},
  7 SpinorChain[Spinor["Spin", "Angle", 30, 2], Mom[10], Spinor["Spin", "Square", 20, 1]]^2 +
    MomProd[30, 10] Mandelstahm[10, 20]/PropDen[Mom[Multiparticle[30, 10]], MZ]}];
check["relabeling round trip", Unscrambling`Private`relabelLegs[relabeled,
  AssociationThread[Values[ids], Keys[ids]]] === indexedPair];
Block[{Unscrambling`Private`$onShellChannels = {{10, 20}}, Unscrambling`Private`trainingRows},
  Unscrambling`Private`trainingRows[___] := {Unscrambling`Private`$onShellChannels};
  conditionResult = Unscrambling`Private`generationJob[<|"Pair" -> relabeled,
    "Mode" -> "Original", "LegPermutation" -> ids|>];
  check["explicit channels follow permutation", conditionResult["Rows"] === {{{10, 30}}}];
  check["channel settings restored", Unscrambling`Private`$onShellChannels === {{10, 20}}];
];
routeMasses = {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> Mm, Mass[4] -> Mm,
  Mass[Multiparticle[1, 2]] -> MZ};
routeStart = SpinorChain[Spinor["Spin", "Angle", 1], Mom[Multiparticle[1, 2]], Spinor["Spin", "Square", 2]];
routeStates = {routeStart};
Block[{Unscrambling`Private`$onShellChannels = Automatic, Unscrambling`Private`$useMomentumConservation = True},
  routeMove = First[Select[Unscrambling`Private`legalMoves[Last[routeStates], routeMasses],
    #["Name"] === "MomentumSum" && Lookup[#, "Identity", None] === "Split" &]];
  AppendTo[routeStates, Unscrambling`Private`applyCandidate[Last[routeStates], routeMove]];
  Do[routeMove = First[Select[Unscrambling`Private`legalMoves[Last[routeStates], routeMasses],
      #["Name"] === "Mass" && #["Direction"] === "Contract" &]];
    AppendTo[routeStates, Unscrambling`Private`applyCandidate[Last[routeStates], routeMove]], {2}];
];
routes = {<|"Name" -> "composite endpoint mass reduction", "Masses" -> routeMasses,
  "States" -> routeStates, "OnShellChannels" -> {{1, 2}}, "MomentumConservation" -> True|>};
routeJobs = Unscrambling`Private`makeSimplificationRouteJobs[routes, 2];
fullBoardSimplificationRouteRows[job_] := Module[{verified, states, batches},
  verified = Unscrambling`Private`verifiedSimplificationRoute[job];
  If[verified === $Failed, Return[$Failed]];
  states = verified["States"];
  batches = MapThread[Unscrambling`Private`trainingRows[#1[[1]], job["Pair"][[1]], #1[[2]], #2, #3] &,
    {Partition[states, 2, 1], verified["CandidateLists"], verified["LabelLists"]}];
  If[MemberQ[batches, {} | $Failed], Return[$Failed]];
  <|"Rows" -> Join[Flatten[batches, 1], Unscrambling`Private`trainingRows[
      Last[states], job["Pair"][[1]], Last[states]]],
    "Targets" -> Length[states] - 1, "Skipped" -> 0, "RouteName" -> job["Name"]|>
];
Do[
  Block[{Unscrambling`Private`$policyMethod = method,
      Unscrambling`Private`$onShellChannels = Unscrambling`Private`trainingJobChannels[routeJob],
      Unscrambling`Private`$useMomentumConservation = Lookup[routeJob, "MomentumConservation", True]},
    check["streamed route rows exactly match full boards " <> method <> " seed " <> ToString[routeJob["Seed"]],
      Unscrambling`Private`simplificationRouteRows[routeJob] === fullBoardSimplificationRouteRows[routeJob]]],
  {method, {"CompactActions", "ReverseMoves", "CandidateScorer"}}, {routeJob, routeJobs}];
streamingDefaultVerification = Unscrambling`Private`verifiedSimplificationRoute[First[routeJobs]];
check["default route verification still returns complete boards",
  AssociationQ[streamingDefaultVerification] &&
    KeyExistsQ[streamingDefaultVerification, "CandidateLists"] &&
    KeyExistsQ[streamingDefaultVerification, "LabelLists"] &&
    !KeyExistsQ[streamingDefaultVerification, "Rows"]];
streamingSmallJob = Join[First[routeJobs], <|"States" -> Take[routeStates, 2], "Steps" -> 1|>];
streamingExpectedContext = {streamingSmallJob["Name"], streamingSmallJob["Seed"],
  streamingSmallJob["LegPermutation"], 2};
Block[{Unscrambling`Private`legalMoves, streamingFailures = {}, streamingEncoded = {}},
  Unscrambling`Private`legalMoves[_, _] := (Pause[0.1]; {});
  streamingResult = Unscrambling`Private`verifiedSimplificationRoute[streamingSmallJob, <|
    "StepTimeLimit" -> 0.05,
    "FailureFunction" -> Function[detail, AppendTo[streamingFailures, detail]],
    "RowFunction" -> Function[{state, target, candidates, labels, step},
      AppendTo[streamingEncoded, step]; <|"Step" -> step|>]|>];
  check["route legal timeout reports exact context without partial rows",
    streamingResult === $Failed && streamingEncoded === {} && Length[streamingFailures] === 1 &&
      Lookup[First[streamingFailures], {"RouteName", "Sequence", "LegPermutation", "Steps"}] ===
        streamingExpectedContext &&
      Lookup[First[streamingFailures], {"Reason", "Step", "Phase"}] ===
        {"CuratedStepTimeout", 1, "LegalEnumeration"} && First[streamingFailures]["Seconds"] > 0];
];
Block[{streamingFailures = {}, streamingEncoded = {}},
  streamingResult = Unscrambling`Private`verifiedSimplificationRoute[streamingSmallJob, <|
    "StepTimeLimit" -> 0.05,
    "FailureFunction" -> Function[detail, AppendTo[streamingFailures, detail]],
    "RowFunction" -> Function[{state, target, candidates, labels, step},
      AppendTo[streamingEncoded, step]; Pause[0.1]; <|"Step" -> step|>]|>];
  check["route encoding timeout reports its phase without partial rows",
    streamingResult === $Failed && streamingEncoded === {1} && Length[streamingFailures] === 1 &&
      Lookup[First[streamingFailures], {"RouteName", "Sequence", "LegPermutation", "Steps"}] ===
        streamingExpectedContext &&
      Lookup[First[streamingFailures], {"Reason", "Step", "Phase"}] ===
        {"CuratedStepTimeout", 1, "Encoding"} && First[streamingFailures]["Seconds"] > 0];
];
Block[{streamingFailures = {}, streamingEncoded = {}},
  streamingResult = Unscrambling`Private`verifiedSimplificationRoute[streamingSmallJob, <|
    "FailureFunction" -> Function[detail, AppendTo[streamingFailures, detail]],
    "RowFunction" -> Function[{state, target, candidates, labels, step},
      AppendTo[streamingEncoded, step]; If[step === 2, $Failed, <|"Step" -> step|>]]|>];
  check["invalid terminal callback discards previously completed route rows",
    streamingResult === $Failed && streamingEncoded === {1, 2} && Length[streamingFailures] === 1 &&
      Lookup[First[streamingFailures], {"Reason", "Step", "Phase"}] ===
        {"InvalidCuratedRow", 2, "Encoding"}];
];
Block[{streamingProgress = {}, streamingCallbacks = {}, streamingFailures = {}},
  streamingResult = Unscrambling`Private`verifiedSimplificationRoute[streamingSmallJob, <|
    "ProgressFunction" -> Function[detail, AppendTo[streamingProgress, detail]],
    "FailureFunction" -> Function[detail, AppendTo[streamingFailures, detail]],
    "RowFunction" -> Function[{state, target, candidates, labels, step},
      AppendTo[streamingCallbacks, {step, Lookup[Pick[candidates, labels, 1], "Name"]}];
      <|"Step" -> step, "Terminal" -> Unscrambling`Private`samePoly[state, target]|>]|>];
  check["streaming progress and terminal Stop callback occur in order",
    AssociationQ[streamingResult] && streamingResult["Rows"] === {
      <|"Step" -> 1, "Terminal" -> False|>, <|"Step" -> 2, "Terminal" -> True|>} &&
      streamingFailures === {} && streamingCallbacks[[All, 1]] === {1, 2} &&
      FreeQ[First[streamingCallbacks][[2]], "Stop"] && Last[streamingCallbacks][[2]] === {"Stop"} &&
      Lookup[streamingProgress, "Phase"] === {"Canonicalization", "Canonicalization",
        "LegalEnumeration", "TeacherMatch", "Encoding", "RowComplete",
        "LegalEnumeration", "TeacherMatch", "Encoding", "RowComplete"} &&
      Lookup[streamingProgress, "Step"] === {1, 2, 1, 1, 1, 1, 2, 2, 2, 2} &&
      AllTrue[streamingProgress,
        Lookup[#, {"RouteName", "Sequence", "LegPermutation", "Steps"}] === streamingExpectedContext &]];
];
Do[
  Block[{Unscrambling`Private`trainingRows},
    Unscrambling`Private`trainingRows[___] := rejectedBatch;
    check["streamed route rejects invalid batch " <> ToString[rejectedBatch],
      Quiet[Unscrambling`Private`simplificationRouteRows[First[routeJobs]]] === $Failed]],
  {rejectedBatch, {{}, $Failed}}];
check["curated routes and permutations are separate jobs", Length[routeJobs] === 3 &&
  AllTrue[routeJobs, #["Mode"] === "SimplificationRoute" &]];
check["integer route augmentation preserves its original seeds",
  Lookup[routeJobs, "Seed"] === {0, 1, 2} && Lookup[routeJobs, "LegPermutation"] ===
    DeleteDuplicates[Table[Unscrambling`Private`trainingLegPermutation[
      {routeMasses, routeStates}, -1, seed], {seed, 0, 2}]]];
allRouteJobs = Unscrambling`Private`makeSimplificationRouteJobs[routes, All];
check["exhaustive four-particle augmentation contains every permutation",
  Length[allRouteJobs] === 24 && DuplicateFreeQ[Lookup[allRouteJobs, "LegPermutation"]] &&
    Sort[Values /@ Lookup[allRouteJobs, "LegPermutation"]] === Sort[Permutations[Range[4]]]];
check["exhaustive augmentation starts with original numbering",
  First[allRouteJobs]["Pair"] === {routeMasses, routeStart} &&
    Lookup[allRouteJobs, "Seed"] === Range[0, 23]];
check["exhaustive route permutations are deterministic",
  allRouteJobs === Unscrambling`Private`makeSimplificationRouteJobs[routes, All]];
check["exhaustive augmentation preserves caller random stream",
  BlockRandom[SeedRandom[77]; Unscrambling`Private`makeSimplificationRouteJobs[routes, All]; RandomInteger[100000]] ===
    BlockRandom[SeedRandom[77]; RandomInteger[100000]]];
check["every exhaustive job relabels states masses and channels together", AllTrue[allRouteJobs,
  #["States"] === Unscrambling`Private`relabelLegs[routeStates, #["LegPermutation"]] &&
    #["Pair"][[1]] === Unscrambling`Private`relabelLegs[routeMasses, #["LegPermutation"]] &&
    Unscrambling`Private`trainingJobChannels[#] === {Sort[Lookup[#["LegPermutation"], {1, 2}]]} &]];
fixtureMasses = {Me, Mu, Mm, MW};
fourRoutes = Table[Join[First[routes], <|"Name" -> "independent route " <> ToString[i],
  "Masses" -> (routeMasses /. Me -> fixtureMasses[[i]]),
  "States" -> (routeStates /. Me -> fixtureMasses[[i]]),
  "OnShellChannels" -> If[OddQ[i], {{1, 2}}, {{3, 4}}]|>], {i, 4}];
fourRouteJobs = Unscrambling`Private`makeSimplificationRouteJobs[fourRoutes, All];
check["four routes retain 96 distinct route permutation jobs", Length[fourRouteJobs] === 96 &&
  DuplicateFreeQ[({#["AmplitudeIndex"], #["LegPermutation"]} &) /@ fourRouteJobs] &&
    Counts[Lookup[fourRouteJobs, "AmplitudeIndex"]] === AssociationThread[-Range[4], ConstantArray[24, 4]]];
check["each route has its own complete permutation sequence", AllTrue[Values[GroupBy[fourRouteJobs,
  #["AmplitudeIndex"] &]], Lookup[#, "Seed"] === Range[0, 23] &&
    Sort[Values /@ Lookup[#, "LegPermutation"]] === Sort[Permutations[Range[4]]] &]];
check["multiple routes preserve their own masses states and channels", AllTrue[fourRouteJobs,
  Function[job, With[{source = fourRoutes[[-job["AmplitudeIndex"]]], ids = job["LegPermutation"]},
    job["Name"] === source["Name"] &&
      job["States"] === Unscrambling`Private`relabelLegs[source["States"], ids] &&
      job["Pair"] === Unscrambling`Private`relabelLegs[{source["Masses"], First[source["States"]]}, ids] &&
      Unscrambling`Private`trainingJobChannels[job] === (Sort[Lookup[ids, #]] & /@ source["OnShellChannels"])]]]];
fiveLegRoutes = {Join[First[routes], <|"Masses" -> Append[routeMasses, Mass[5] -> MW]|>]};
fiveLegJobs = Unscrambling`Private`makeSimplificationRouteJobs[fiveLegRoutes, All];
check["exhaustive five-particle augmentation contains 120 unique permutations",
  Length[fiveLegJobs] === 120 && DuplicateFreeQ[Lookup[fiveLegJobs, "LegPermutation"]]];
sixLegRoutes = {Join[First[routes], <|"Masses" -> Join[routeMasses, {Mass[5] -> MW, Mass[6] -> MW}]|>]};
check["exhaustive augmentation fails before larger factorial allocation", Quiet[
  Unscrambling`Private`makeSimplificationRouteJobs[sixLegRoutes, All]] === $Failed];
check["higher-point routes still support sampled augmentation", Length[
  Unscrambling`Private`makeSimplificationRouteJobs[sixLegRoutes, 0]] === 1];
check["invalid direct route augmentation rejected", Quiet[
  Unscrambling`Private`makeSimplificationRouteJobs[routes, "All"]] === $Failed];
Block[{Unscrambling`Private`withTrainingWorkers},
  Unscrambling`Private`withTrainingWorkers[___] := "WorkerSetupReached";
  check["public training accepts exhaustive route augmentation", TrainUnscrambleNet[
    {pair}, "SimplificationRoutes" -> routes, "RoutePermutations" -> All] === "WorkerSetupReached"];
];
manyRouteJobs = Unscrambling`Private`makeSimplificationRouteJobs[routes, 50];
check["duplicate route permutations are not repeated", Length[manyRouteJobs] <= 24 &&
  DuplicateFreeQ[Lookup[manyRouteJobs, "LegPermutation"]]];
check["curated transitions are permuted with their masses", AllTrue[routeJobs,
  #["States"] === Unscrambling`Private`relabelLegs[routeStates, #["LegPermutation"]] &&
    #["Pair"][[1]] === Unscrambling`Private`relabelLegs[routeMasses, #["LegPermutation"]] &]];
routeResults = Unscrambling`Private`mapTrainingJobs[routeJobs, {}, settings];
check["all curated transitions remain legal after relabeling", Unscrambling`Private`validJobResultsQ[routeResults]];
allRouteResults = Unscrambling`Private`mapTrainingJobs[allRouteJobs, {}, settings];
check["every exhaustive route permutation retains legal teacher transitions",
  Unscrambling`Private`validJobResultsQ[allRouteResults] && Length[allRouteResults] === 24];
fourRouteResults = Unscrambling`Private`mapTrainingJobs[fourRouteJobs, {}, settings];
check["all four routes retain legal teachers under every permutation",
  Unscrambling`Private`validJobResultsQ[fourRouteResults] && Length[fourRouteResults] === 96 &&
    AllTrue[fourRouteResults, #["Result"]["Targets"] === 3 && #["Result"]["Skipped"] === 0 &]];
routeRows = First[routeResults]["Result"]["Rows"];
check["only the curated endpoint is taught Stop", Length[routeRows] === 4 &&
  AllTrue[Most[routeRows], !Unscrambling`Private`trainingStopRowQ[#] &] &&
  Unscrambling`Private`trainingStopRowQ[Last[routeRows]]];
check["curated routes retain score-neutral composite splits",
  Unscrambling`Private`expressionComplexity[routeStates[[2]]] ===
    Unscrambling`Private`expressionComplexity[First[routeStates]] &&
  First[routeResults]["Result"]["Targets"] === 3 && First[routeResults]["Result"]["Skipped"] === 0];
growthStart = routeStart /. Mom[Multiparticle[1, 2]] -> Mom[1];
growthMove = First[Select[UnscrambleCandidates[{routeMasses, growthStart}],
  #["Name"] === "MomentumSum" && Lookup[#, "Identity", None] === "Introduce" &]];
growthEnd = Unscrambling`Private`applyCandidate[growthStart, growthMove];
growthJob = First[Unscrambling`Private`makeSimplificationRouteJobs[
  {<|"Name" -> "temporary momentum growth", "Masses" -> routeMasses,
    "States" -> {growthStart, growthEnd}|>}, 0]];
growthResult = Unscrambling`Private`evaluateTrainingJob[growthJob, settings];
check["curated routes still allow genuine temporary complexity growth",
  SpinorExpressionComplexity[growthEnd] > SpinorExpressionComplexity[growthStart] &&
    TrueQ[growthResult["Success"]] && growthResult["Result"]["Targets"] === 1 &&
    growthResult["Result"]["Skipped"] === 0];
check["malformed curated routes fail loudly", Quiet[Unscrambling`Private`makeSimplificationRouteJobs[
  {<|"Name" -> "missing states", "Masses" -> {}|>}, 0]] === $Failed];
badRouteJobs = Unscrambling`Private`makeSimplificationRouteJobs[
  {Join[First[routes], <|"States" -> {routeStart, EE}|>]}, 0];
check["illegal curated transitions reject the whole job", !TrueQ[
  Quiet[Unscrambling`Private`evaluateTrainingJob[First[badRouteJobs], settings]]["Success"]]];
repeatedRouteJobs = Unscrambling`Private`makeSimplificationRouteJobs[
  {Join[First[routes], <|"States" -> {routeStart, routeStart}|>]}, 0];
check["curated intermediate Stop is rejected", !TrueQ[
  Quiet[Unscrambling`Private`evaluateTrainingJob[First[repeatedRouteJobs], settings]]["Success"]]];
check["route conditions are sorted after permutation", Block[{Unscrambling`Private`$onShellChannels = Automatic},
  Unscrambling`Private`trainingJobChannels[<|"OnShellChannels" -> {{1, 2}},
    "LegPermutation" -> <|1 -> 3, 2 -> 1|>|>] === {{1, 3}}]];
Block[{Unscrambling`Private`$policyMethod = "ReverseMoves"},
  reverseRouteResult = Unscrambling`Private`evaluateTrainingJob[First[routeJobs],
    Join[settings, <|"PolicyMethod" -> "ReverseMoves"|>]];
  check["curated routes also supply direct-move targets", TrueQ[reverseRouteResult["Success"]] &&
    AllTrue[reverseRouteResult["Result"]["Rows"], KeyExistsQ[#, "LegalMask"] &]];
];
originalRouteJob = <|"Order" -> 4, "Mode" -> "Original", "Pair" -> {routeMasses, routeStart}, "Steps" -> 0|>;
originalRouteResult = Unscrambling`Private`evaluateTrainingJob[originalRouteJob,
  Join[settings, <|"MomentumConservation" -> True, "OnShellChannels" -> {{1, 2}}|>]];
collectedRoutes = Unscrambling`Private`collectTrainingRows[Join[routeResults, {originalRouteResult}],
  Join[routeJobs, {originalRouteJob}]];
check["a raw curated input is not counter-taught Stop", collectedRoutes["DiscardedStopExamples"] === 1];
overlappingRoutes = {Join[First[routes], <|"Name" -> "partial route", "States" -> Take[routeStates, 3]|>],
  Join[First[routes], <|"Name" -> "continued route", "States" -> Take[routeStates, -2]|>]};
overlapJobs = Unscrambling`Private`makeSimplificationRouteJobs[overlappingRoutes, 0];
overlapResults = Unscrambling`Private`mapTrainingJobs[overlapJobs, {}, settings];
overlapRows = Unscrambling`Private`collectTrainingRows[overlapResults, overlapJobs];
check["a route endpoint loses Stop when another route continues", overlapRows["DiscardedStopExamples"] === 1 &&
  Count[Unscrambling`Private`trainingStopRowQ /@ overlapRows["Rows"], True] === 1];
fourOverlappingRoutes = Join[Table[Join[First[routes], <|"Name" -> "route segment " <> ToString[i],
  "States" -> Take[routeStates, {i, i + 1}]|>], {i, 3}],
  {Join[First[routes], <|"Name" -> "complete overlapping route"|>]}];
fourOverlapJobs = Unscrambling`Private`makeSimplificationRouteJobs[fourOverlappingRoutes, 0];
fourOverlapResults = Unscrambling`Private`mapTrainingJobs[fourOverlapJobs, {}, settings];
fourOverlapRows = Unscrambling`Private`collectTrainingRows[fourOverlapResults, fourOverlapJobs];
check["four overlapping routes do not counter-teach intermediate Stop",
  Unscrambling`Private`validJobResultsQ[fourOverlapResults] &&
    fourOverlapRows["DiscardedStopExamples"] === 2 &&
    Count[Unscrambling`Private`trainingStopRowQ /@ fourOverlapRows["Rows"], True] === 2 &&
    Intersection[Unscrambling`Private`trainingRowStateKey /@
      Select[fourOverlapRows["Rows"], Unscrambling`Private`trainingStopRowQ],
      Unscrambling`Private`trainingRowStateKey /@
        Select[fourOverlapRows["Rows"], !Unscrambling`Private`trainingStopRowQ[#] &]] === {}];
check["invalid route permutation count rejected", Quiet[TrainUnscrambleNet[{pair}, "RoutePermutations" -> -1]] === $Failed];
chunks = Unscrambling`Private`partitionTrainingJobs[jobs, 2];
check["partition loses no jobs", SortBy[Flatten[chunks, 1], #["Order"] &] === jobs];
check["both workers assigned work", AllTrue[chunks, Length[#] > 0 &]];
serial = Unscrambling`Private`mapTrainingJobs[jobs, {}, settings];
again = Unscrambling`Private`mapTrainingJobs[jobs, {}, settings];
check["generation deterministic", Lookup[serial, "Result"] === Lookup[again, "Result"]];
check["all generation jobs succeed", Unscrambling`Private`validJobResultsQ[serial]];
zeroStepJobs = Unscrambling`Private`makeTrainingJobs[{pair}, 0, 1];
zeroStepResult = Last[Unscrambling`Private`mapTrainingJobs[zeroStepJobs, {}, settings]]["Result"];
check["relabeled originals supply Stop examples", Length[zeroStepResult["Rows"]] === 1 &&
  Count[First[zeroStepResult["Rows"]]["Target"], {1.} | {1}] === 1];
check["holdouts keep original numbering", AllTrue[
  Unscrambling`Private`makeValidationJobs[{pair}, 1, 2, 3, 2], #["Pair"] === pair &]];
check["job timing recorded", AllTrue[serial, #["CPUSeconds"] >= 0 && #["WallSeconds"] >= 0 &]];
routeCache = CreateDirectory[];
cachedRoutes = Unscrambling`Private`cachedTrainingJobs[routeJobs, {}, settings, routeCache, 123];
Block[{Unscrambling`Private`mapTrainingJobs},
  Unscrambling`Private`mapTrainingJobs[{}, _, _] := {};
  Unscrambling`Private`mapTrainingJobs[__] := $Failed;
  check["curated routes reuse existing cache", AllTrue[Lookup[
    Unscrambling`Private`cachedTrainingJobs[routeJobs, {}, settings, routeCache, 123], "CacheHit"], TrueQ]];
  check["route edits invalidate the cache", Unscrambling`Private`cachedTrainingJobs[
    ReplacePart[routeJobs, {1, "States", 2} -> EE], {}, settings, routeCache, 123] === $Failed];
  check["per-route conditions invalidate the cache", Unscrambling`Private`cachedTrainingJobs[
    ReplacePart[routeJobs, {1, "MomentumConservation"} -> False], {}, settings, routeCache, 123] === $Failed];
];
DeleteDirectory[routeCache, DeleteContents -> True];
cache = CreateDirectory[];
first = Unscrambling`Private`cachedTrainingJobs[jobs, {}, settings, cache, 123];
Block[{Unscrambling`Private`mapTrainingJobs},
  Unscrambling`Private`mapTrainingJobs[{}, _, _] := {};
  Unscrambling`Private`mapTrainingJobs[__] := $Failed;
  second = Unscrambling`Private`cachedTrainingJobs[jobs, {}, settings, cache, 123];
  check["cache hit skips all generation", AllTrue[Lookup[second, "CacheHit"], TrueQ]];
  check["cache preserves dataset", Lookup[first, "Result"] === Lookup[second, "Result"]];
  check["source changes invalidate cache", Unscrambling`Private`cachedTrainingJobs[jobs, {}, settings, cache, 124] === $Failed];
  check["conditions invalidate cache", Unscrambling`Private`cachedTrainingJobs[jobs, {}, Join[settings, <|"MomentumConservation" -> True|>], cache, 123] === $Failed];
];
DeleteDirectory[cache, DeleteContents -> True];
cache = CreateDirectory[];
Block[{Unscrambling`Private`mapTrainingJobs},
  Unscrambling`Private`mapTrainingJobs[_, _, _] := {serial[[1]], <|"Success" -> False|>};
  partial = Unscrambling`Private`cachedTrainingJobs[Take[jobs, 2], {}, settings, cache, 123];
  check["failed job rejects partial dataset", partial === $Failed];
  Unscrambling`Private`mapTrainingJobs[pending_, _, _] := (
    check["retry computes only failed job", Length[pending] === 1 && First[pending]["Order"] === 2];
    {serial[[2]]});
  retried = Unscrambling`Private`cachedTrainingJobs[Take[jobs, 2], {}, settings, cache, 123];
  check["successful job reused after failure", Lookup[retried, "CacheHit"] === {True, False}];
];
DeleteDirectory[cache, DeleteContents -> True];
vjobs = Unscrambling`Private`makeValidationJobs[{pair}, 2, 2, 2, 3];
check["holdout seeds separate", Lookup[vjobs, "Seed"] === {3, 4}];
Block[{Unscrambling`Private`generationJob},
  Unscrambling`Private`generationJob[_] := (Pause[1]; <||>);
  timed = Unscrambling`Private`evaluateTrainingJob[First[jobs], Join[settings, <|"JobTimeLimit" -> 0.05|>]];
  check["timed out job rejected", !TrueQ[timed["Success"]]];
];
check["empty holdout valid", Unscrambling`Private`mapTrainingJobs[{}, {}, settings] === {}];
check["invalid batch size rejected", Quiet[TrainUnscrambleNet[{pair}, "TrainingBatchSize" -> 0]] === $Failed];
check["invalid device rejected", Quiet[TrainUnscrambleNet[{pair}, TargetDevice -> "Unknown"]] === $Failed];
check["invalid workers rejected", Quiet[TrainUnscrambleNet[{pair}, Kernels -> 0]] === $Failed];
Block[{Unscrambling`Private`trainingKernelSnapshot, Unscrambling`Private`launchTrainingKernels,
    Unscrambling`Private`closeTrainingKernels, Unscrambling`Private`initializeTrainingWorkers,
    current = {99}, closed = {}},
  Unscrambling`Private`trainingKernelSnapshot[] := current;
  Unscrambling`Private`launchTrainingKernels[n_] := (current = Join[current, Range[n]]);
  Unscrambling`Private`closeTrainingKernels[workers_] := (closed = Join[closed, workers]; current = Complement[current, workers]);
  Unscrambling`Private`initializeTrainingWorkers[___] := True;
  result = Unscrambling`Private`withTrainingWorkers[2, "unused", 1, 0, Function[w, w]];
  check["only owned workers closed", result === {1, 2} && closed === {1, 2} && current === {99}];
  closed = {};
  result = Unscrambling`Private`withTrainingWorkers[{99}, "unused", 1, 0, Function[w, w]];
  check["borrowed pool preserved", result === {99} && closed === {} && current === {99}];
  result = CheckAbort[Unscrambling`Private`withTrainingWorkers[2, "unused", 1, 0, Function[w, Abort[]]], "Aborted"];
  check["abort closes only owned pool", result === "Aborted" && current === {99} && closed === {1, 2}];
  closed = {};
  Unscrambling`Private`initializeTrainingWorkers[___] := False;
  result = Quiet[Unscrambling`Private`withTrainingWorkers[2, "unused", 1, 0, Function[w, True]]];
  check["initialization failure cleans up", result === $Failed && current === {99} && closed === {1, 2}];
  closed = {};
  Unscrambling`Private`launchTrainingKernels[n_] := (current = Append[current, 1]);
  result = Quiet[Unscrambling`Private`withTrainingWorkers[2, "unused", 1, 0, Function[w, True]]];
  check["partial startup cleans up", result === $Failed && current === {99} && closed === {1}];
];
work = Unscrambling`Private`trainingWorkEstimate[Flatten[Lookup[Lookup[serial, "Result"], "Rows"], 1]];
check["shared state saves encoding work", work["StateBytesPerEpoch"] < work["StateBytesWithoutSharing"]];
check["padding overhead reported", work["EditBytesWithoutPadding"] <= work["EditBytesWithTrainingPadding"]];
Print[<|"Checks" -> checks, "Failed" -> Length[failures], "Failures" -> failures|>];
Quit[If[failures === {}, 0, 1]];
