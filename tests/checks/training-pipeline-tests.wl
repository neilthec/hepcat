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

(* Massless completeness and off-shell common-denominator algebra support the
   two-photon route without treating a propagator as an on-shell numerator. *)
photonMasses = {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> 0, Mass[4] -> 0};
photonSquare[i_, j_] := SpinorChain[Spinor["Helicity", "Square", i], Spinor["Helicity", "Square", j]];
photonSandwich[side_, i_, p_, j_] := SpinorChain[Spinor["Helicity", side, i], Mom[p],
  Spinor["Helicity", If[side === "Angle", "Square", "Angle"], j]];
photonEdits[expr_, identity_, masses_: photonMasses] := Select[
  Unscrambling`Private`helicityInvariantMoves[Unscrambling`Private`canonicalChains[expr], masses],
  Lookup[#, "Identity", None] === identity &];
Do[
  photonState = Unscrambling`Private`canonicalChains[7 photonSandwich[side, 3, 2, 3]];
  photonMoves = photonEdits[photonState, "HelicityTrace"];
  check["closed massless sandwich exact " <> side, Length[photonMoves] === 1 &&
    Unscrambling`Private`samePoly[Unscrambling`Private`applyCandidate[photonState, First[photonMoves]],
      14 MomProd[2, 3]]],
  {side, {"Angle", "Square"}}];
photonState = Unscrambling`Private`canonicalChains[photonSandwich["Angle", 3, 2, 3]^2];
photonMoves = photonEdits[photonState, "HelicityTrace"];
check["closed sandwich reduces one positive factor at a time", photonMoves =!= {} &&
  Unscrambling`Private`samePoly[Unscrambling`Private`applyCandidate[photonState, First[photonMoves]],
    2 MomProd[2, 3] photonSandwich["Angle", 3, 2, 3]]];
check["reciprocal sandwich is not a positive contraction", photonEdits[
  1/photonSandwich["Angle", 3, 2, 3], "HelicityTrace"] === {}];
check["different massless endpoints are not a closed trace", photonEdits[
  photonSandwich["Angle", 3, 2, 4], "HelicityTrace"] === {}];
check["massive spin endpoints are not helicity traces", photonEdits[
  SpinorChain[Spinor["Spin", "Angle", 1], Mom[2], Spinor["Spin", "Square", 1]],
  "HelicityTrace"] === {}];
check["nonzero mass helicity fixture is rejected", photonEdits[
  photonSandwich["Angle", 1, 2, 1], "HelicityTrace"] === {}];
check["unknown endpoint mass is rejected", photonEdits[
  photonSandwich["Angle", 9, 2, 9], "HelicityTrace"] === {}];
photonMoves = photonEdits[photonSandwich["Angle", 3, 3, 3], "HelicityTrace"];
check["own massless momentum does not introduce an unreduced self dot", AllTrue[photonMoves,
  Unscrambling`Private`samePoly[#["Insert"], 0] &]];
check["nonzero endpoint indices are not implicitly contracted", photonEdits[
  SpinorChain[Spinor["Helicity", "Angle", 3, 1], Mom[2], Spinor["Helicity", "Square", 3, 2]],
  "HelicityTrace"] === {}];
photonState = 6 MomProd[3, 4];
photonMoves = photonEdits[photonState, "MasslessBracket"];
check["massless invariant expands to opposite ordered brackets", AnyTrue[photonMoves,
  Unscrambling`Private`samePoly[Unscrambling`Private`applyCandidate[photonState, #],
    Unscrambling`Private`canonicalChains[3 ang[3, 4] photonSquare[4, 3]]] &]];
photonState = Unscrambling`Private`canonicalChains[3 ang[3, 4] photonSquare[4, 3]];
photonMoves = photonEdits[photonState, "MasslessBracket"];
check["massless bracket pair contracts to invariant", AnyTrue[photonMoves,
  Unscrambling`Private`samePoly[Unscrambling`Private`applyCandidate[photonState, #], 6 MomProd[3, 4]] &]];
check["massive invariant does not acquire massless bracket identity",
  photonEdits[MomProd[1, 3], "MasslessBracket"] === {}];
check["inverse invariant is not expanded", photonEdits[1/MomProd[3, 4], "MasslessBracket"] === {}];
photonDt = PropDen[Mom[Multiparticle[1, 3]], Me];
photonDu = PropDen[Mom[Multiparticle[1, 4]], Me];
photonState = ang[3, 4]/photonDt + 2 ang[3, 4]/photonDu;
photonMoves = Unscrambling`Private`propagatorNumeratorMoves[photonState, photonMasses];
photonSubsetSquare[ii_List] := Total[(Mass[#] /. photonMasses)^2 & /@ ii] +
  2 Total[(MomProd @@ Sort[#]) & /@ Subsets[ii, {2}]];
photonOffShell[expr_] := expr /. {
  HoldPattern[PropDen[Mom[Multiparticle[ii__Integer]], m_]] :> photonSubsetSquare[{ii}] - m^2,
  HoldPattern[Mom[Multiparticle[ii__Integer]]^2] :> photonSubsetSquare[{ii}]};
check["common-denominator numerator moves are available", photonMoves =!= {}];
check["propagator numerator moves are off-shell rational identities", AllTrue[photonMoves,
  Together[photonOffShell[Unscrambling`Private`applyCandidate[photonState, #] - photonState]] === 0 &]];
check["common-denominator moves retain both original propagator atoms", AllTrue[photonMoves,
  With[{result = Unscrambling`Private`applyCandidate[photonState, #]},
    !FreeQ[result, photonDt] && !FreeQ[result, photonDu] &&
      Sort[DeleteDuplicates[Cases[result, _PropDen, Infinity]]] === Sort[{photonDt, photonDu}]] &]];
check["one pole is not needlessly recombined", Unscrambling`Private`propagatorNumeratorMoves[
  ang[3, 4]/photonDt, photonMasses] === {}];
check["width-dependent propagators are not given the zero-width numerator identity",
  Unscrambling`Private`propagatorNumeratorMoves[
    ang[3, 4]/PropDen[Mom[Multiparticle[1, 3]], Me, width] +
      ang[3, 4]/PropDen[Mom[Multiparticle[1, 4]], Me, width], photonMasses] === {}];
check["mixed width groups remain unchanged unless every numerator pole is supported",
  Unscrambling`Private`propagatorNumeratorMoves[
    ang[3, 4]/photonDt + ang[3, 4]/PropDen[Mom[Multiparticle[1, 4]], Me, width],
    photonMasses] === {}];
check["unknown external masses do not produce an off-shell numerator",
  Unscrambling`Private`propagatorNumeratorMoves[
    ang[3, 4]/PropDen[Mom[Multiparticle[1, 9]], Me] +
      ang[3, 4]/PropDen[Mom[Multiparticle[1, 10]], Me], photonMasses] === {}];
Do[
  photonIds = AssociationThread[Range[4], permutation];
  photonPermuted = Unscrambling`Private`relabelLegs[{photonMasses,
    photonSandwich["Angle", 3, 2, 3]}, photonIds];
  photonMoves = photonEdits[photonPermuted[[2]], "HelicityTrace", photonPermuted[[1]]];
  photonExpected = Unscrambling`Private`relabelLegs[2 MomProd[2, 3], photonIds] /.
    MomProd[i_Integer, j_Integer] :> MomProd @@ Sort[{i, j}];
  check["massless completeness follows sparse labels " <> ToString[permutation],
    Length[photonMoves] === 1 && Unscrambling`Private`samePoly[
      Unscrambling`Private`applyCandidate[photonPermuted[[2]], First[photonMoves]], photonExpected]],
  {permutation, {{1, 2, 3, 4}, {4, 3, 2, 1}, {10, 30, 20, 40}}}];
photonEncodingCases = {
  {photonSandwich["Angle", 3, 2, 3], photonEdits[photonSandwich["Angle", 3, 2, 3], "HelicityTrace"]},
  {MomProd[3, 4], photonEdits[MomProd[3, 4], "MasslessBracket"]},
  {ang[3, 4]/photonDt + 2 ang[3, 4]/photonDu,
    Unscrambling`Private`propagatorNumeratorMoves[ang[3, 4]/photonDt + 2 ang[3, 4]/photonDu, photonMasses]}};
Do[
  photonFeatures = Unscrambling`Private`compactParticleFeatures[
    Unscrambling`Private`canonicalChains[fixture[[1]]], photonMasses, fixture[[2]]];
  check["new identity particle descriptors encode " <> ToString[index],
    ArrayQ[photonFeatures, 3, NumberQ] && Last[Dimensions[photonFeatures]] === 166],
  {index, Length[photonEncodingCases]}, {fixture, {photonEncodingCases[[index]]}}];
photonForwardOnly = photonEdits[photonSandwich["Angle", 3, 2, 3], "HelicityTrace"];
photonPropagatorOnly = Unscrambling`Private`propagatorNumeratorMoves[
  ang[3, 4]/photonDt + 2 ang[3, 4]/photonDu, photonMasses];
check["forward-only curated identities are excluded from random scrambling",
  photonForwardOnly =!= {} && photonPropagatorOnly =!= {} && AllTrue[
    Join[photonForwardOnly, photonPropagatorOnly],
    Lookup[#, "ScrambleEligible", True] === False &]];
check["forward-only identities remain legal during inference", AnyTrue[
  UnscrambleCandidates[{photonMasses, photonSandwich["Angle", 3, 2, 3]}],
  Lookup[#, "Identity", None] === "HelicityTrace" &] && AnyTrue[
  UnscrambleCandidates[{photonMasses, ang[3, 4]/photonDt + 2 ang[3, 4]/photonDu}],
  Lookup[#, "Identity", None] === "PropagatorNumerator" &]];
photonEligible = photonEdits[MomProd[3, 4], "MasslessBracket"];
Block[{Unscrambling`Private`legalMoves},
  Unscrambling`Private`legalMoves[_, _] := photonForwardOnly;
  photonScramble = ComplicateAmplitude[{photonMasses, photonSandwich["Angle", 3, 2, 3]}, 3,
    RandomSeed -> 123, "RecordSteps" -> True];
  check["scramble stops cleanly when only one-way edits remain",
    photonScramble["Steps"] === 0 && photonScramble["Trajectory"] === {} &&
      photonScramble["Expression"] === photonScramble["Original"]];
  Unscrambling`Private`legalMoves[_, _] := Join[photonForwardOnly, photonEligible];
  photonScramble = ComplicateAmplitude[{photonMasses, MomProd[3, 4]}, 1,
    RandomSeed -> 123, "RecordSteps" -> True];
  check["scramble still samples the reversible identity beside excluded edits",
    photonScramble["Steps"] === 1 && Length[photonScramble["Trajectory"]] === 1 &&
      photonScramble["Trajectory"][[1, "Candidate", "Identity"]] === "MasslessBracket"];
];
(* Numerator-only substitutions act on existing summands without expanding. *)
numeratorDt = s - mass^2;
numeratorDu = t - mass^2;
check["numerator substitutions do not combine separate poles",
  ConstructiveDiagrams`applyToNumerator[a/numeratorDt + b/numeratorDu,
    {s -> mass^2, t -> mass^2}] === a/numeratorDt + b/numeratorDu];
check["matching numerator and denominator symbols only changes numerator",
  ConstructiveDiagrams`applyToNumerator[x^2/(x + y), x -> z] === z^2/(x + y)];
check["factored numerator sums remain factored",
  ConstructiveDiagrams`applyToNumerator[c (x + y)/p,
    {x -> u, y -> v, p -> 0}] === c (u + v)/p];
check["rules can match a whole unexpanded numerator sum",
  ConstructiveDiagrams`applyToNumerator[(x + y)/p, x + y -> z] === z/p];
check["polynomial numerator powers are not expanded to expose new matches",
  ConstructiveDiagrams`applyToNumerator[(x + y)^2/p, x^2 -> z] ===
    (x + y)^2/p];
check["whole numerator identities apply to each existing top-level term",
  ConstructiveDiagrams`applyToNumerator[(x + y)^2/p + (x + y)/q,
    x + y -> z] === z^2/p + z/q];
check["rational numerical denominators are preserved",
  ConstructiveDiagrams`applyToNumerator[3 x/7 + 2 y/5, {x -> u, y -> v}] ===
    3 u/7 + 2 v/5];
check["numerator powers are replaced without touching denominator powers",
  ConstructiveDiagrams`applyToNumerator[x^3/(x + y)^2, x -> z] === z^3/(x + y)^2];
check["denominator-free expressions accept numerator identities",
  ConstructiveDiagrams`applyToNumerator[x^2 + y, x -> z] === z^2 + y];
check["zero has an ordinary zero numerator",
  ConstructiveDiagrams`applyToNumerator[0, x -> z] === 0];
check["delayed numerator rules are supported",
  ConstructiveDiagrams`applyToNumerator[MomProd[1, 2]/p + MomProd[2, 3]/q,
    MomProd[i_, j_] :> i + j] === 3/p + 5/q];
numeratorFactored = (x + y) (u + v)/(p + q);
check["empty numerator rules preserve the exact factored expression",
  ConstructiveDiagrams`applyToNumerator[numeratorFactored, {}] === numeratorFactored];
check["identities matching a whole denominator atom never apply to it",
  ConstructiveDiagrams`applyToNumerator[x/f[x], f[_] -> 0] === x/f[x]];
numeratorPropagator = PropDen[Mom[Multiparticle[1, 3]], Mass[1] + Mass[2]];
numeratorMomentum = Mom[Multiparticle[1, 3]];
check["composite momentum substitutions keep propagator contents unchanged",
  ConstructiveDiagrams`applyToNumerator[numeratorMomentum/numeratorPropagator^2,
    Mom[Multiparticle[i_, j_]] :> Mom[i] + Mom[j]] ===
    (Mom[1] + Mom[3])/numeratorPropagator^2];
check["numerator mass substitutions do not put propagators on shell",
  ConstructiveDiagrams`applyToNumerator[Mass[1] numeratorMomentum/numeratorPropagator,
    {Mass[_] -> Me, Mom[Multiparticle[i_, j_]] :> Mom[i] + Mom[j]}] ===
    Me (Mom[1] + Mom[3])/numeratorPropagator];
check["inverse sum contents are not expanded into new denominator factors",
  ConstructiveDiagrams`applyToNumerator[x/((p + q)^2 (r + s)),
    {p -> 0, q -> 0, r -> 0, s -> 0}] === x/((p + q)^2 (r + s))];
check["numerator API has the public package context",
  Context[ConstructiveDiagrams`applyToNumerator] === "ConstructiveDiagrams`"];
check["numerator API is protected after package loading",
  MemberQ[Attributes[ConstructiveDiagrams`applyToNumerator], Protected]];
Global`applyToNumerator[_, _] := "notebook helper";
Quiet[Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]]];
check["package reload retains its public numerator implementation",
  ConstructiveDiagrams`applyToNumerator[x/p + y/q, {x -> u, y -> v}] === u/p + v/q];
check["package reload does not alter an existing notebook helper",
  Global`applyToNumerator[x/p, {}] === "notebook helper"];
check["package reload restores numerator API protection",
  MemberQ[Attributes[ConstructiveDiagrams`applyToNumerator], Protected]];
ClearAll[Global`applyToNumerator];

complementEdits[expr_, rules_List] := Block[{Unscrambling`Private`$useMomentumConservation = True},
  Select[Unscrambling`Private`scalarMomentumConservationMoves[expr, rules],
    Lookup[#, "Identity", None] === "ScalarComplement" &]];
complementFourMasses = {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> 0, Mass[4] -> 0};
complementState = MomProd[1, 2];
complementMoves = complementEdits[complementState, complementFourMasses];
check["complementary invariant is exact for four external particles", Length[complementMoves] === 1 &&
  First[complementMoves]["Name"] === "MomentumConservation" &&
  First[complementMoves]["Legs"] === {1, 2} && Unscrambling`Private`samePoly[
    First[complementMoves]["Insert"], MomProd[3, 4] - Me^2]];
check["complementary invariant does not require internal on-shell channels", Block[
  {Unscrambling`Private`$onShellChannels = {}},
  complementEdits[complementState, complementFourMasses] === complementMoves]];
complementFiveMasses = {Mass[1] -> m1, Mass[2] -> m2, Mass[3] -> m3,
  Mass[4] -> m4, Mass[5] -> m5};
complementMoves = complementEdits[complementState, complementFiveMasses];
complementExpected = (m3^2 + m4^2 + m5^2 - m1^2 - m2^2)/2 +
  MomProd[3, 4] + MomProd[3, 5] + MomProd[4, 5];
check["five-particle complementary invariant includes every pair in its momentum sum",
  Length[complementMoves] === 1 && Unscrambling`Private`samePoly[
    First[complementMoves]["Insert"], complementExpected]];
complementPermutation = AssociationThread[Range[5], {10, 30, 40, 50, 80}];
complementPermutedMasses = Unscrambling`Private`relabelLegs[complementFiveMasses, complementPermutation];
complementPermutedState = Unscrambling`Private`relabelLegs[complementState, complementPermutation];
complementMoves = complementEdits[complementPermutedState, complementPermutedMasses];
check["complementary invariant supports sparse higher-point labels", Length[complementMoves] === 1 &&
  First[complementMoves]["Legs"] === {10, 30} && Unscrambling`Private`samePoly[
    First[complementMoves]["Insert"], Unscrambling`Private`relabelLegs[
      complementExpected, complementPermutation]]];
complementPropagator = PropDen[Mom[Multiparticle[1, 3]], Me];
complementPowered = 3 MomProd[1, 2]^2 ang[1, 2]/complementPropagator^2;
complementMoves = complementEdits[complementPowered, complementFourMasses];
check["complementary invariant replaces one positive dot factor and preserves powers",
  Length[complementMoves] === 1 && Unscrambling`Private`samePoly[
    First[complementMoves]["Insert"],
    3 MomProd[1, 2] (MomProd[3, 4] - Me^2) ang[1, 2]/complementPropagator^2]];
check["complementary invariant does not rewrite inverse dot factors",
  complementEdits[1/MomProd[1, 2], complementFourMasses] === {}];
check["complementary invariant does not enter propagator contents", complementEdits[
  1/PropDen[MomProd[1, 2] + Mom[Multiparticle[1, 3]], Me], complementFourMasses] === {}];
check["complementary invariant retains opaque propagator atoms", AllTrue[complementMoves,
  DeleteDuplicates[Cases[#["Insert"], _PropDen, Infinity]] === {complementPropagator} &]];
check["complementary invariant skips unknown external masses", complementEdits[
  complementState, ReplacePart[complementFourMasses, 4 -> (Mass[4] -> Missing["Mass"])]] === {}];
check["complementary invariant skips dots outside the external mass list", complementEdits[
  MomProd[1, 9], complementFourMasses] === {}];
check["complementary invariant respects disabled momentum conservation", Block[
  {Unscrambling`Private`$useMomentumConservation = False},
  Unscrambling`Private`scalarMomentumConservationMoves[complementState, complementFourMasses] === {}]];
complementMoves = complementEdits[complementPermutedState, complementPermutedMasses];
complementFeatures = Unscrambling`Private`compactParticleFeatures[
  complementPermutedState, complementPermutedMasses, complementMoves];
check["complementary invariant retains the existing particle descriptor width",
  ArrayQ[complementFeatures, 3, NumberQ] && Last[Dimensions[complementFeatures]] === 166];
check["complementary invariant is available through the public candidate board", AnyTrue[
  UnscrambleCandidates[{complementFourMasses, complementState}, "OnShellChannels" -> {},
    "MomentumConservation" -> True], Lookup[#, "Identity", None] === "ScalarComplement" &]];
check["complementary invariants are excluded from random scrambling",
  complementMoves =!= {} && AllTrue[complementMoves,
    Lookup[#, "ScrambleEligible", True] === False &]];
Block[{Unscrambling`Private`legalMoves},
  Unscrambling`Private`legalMoves[_, _] := complementMoves;
  complementScramble = ComplicateAmplitude[{complementPermutedMasses, complementPermutedState}, 3,
    RandomSeed -> 123, "RecordSteps" -> True];
  check["random scrambling skips a complementary-invariant-only board",
    complementScramble["Steps"] === 0 && complementScramble["Trajectory"] === {} &&
      complementScramble["Expression"] === complementScramble["Original"]];
];
(* Momentum conservation is linear in one chain/trace momentum slot, not in
   a power or reciprocal of the complete chain. Dot-product rows are linear. *)
conservationClosed[i_] := SpinorChain[Spinor["Helicity", "Angle", 4], Mom[i],
  Spinor["Helicity", "Square", 4]];
conservationMasslessRules = Table[Mass[i] -> 0, {i, 4}];
conservationCollected[expr_, rules_List] := Block[
  {Unscrambling`Private`$useMomentumConservation = True,
    Unscrambling`Private`$onShellChannels = {}},
  Select[Unscrambling`Private`kinematicMoves[Unscrambling`Private`canonicalChains[expr], rules],
    #["Name"] === "MomentumConservation" && KeyExistsQ[#, "Momentum"] &]];
conservationSquaredSum = Total[conservationClosed[#]^2 & /@ {1, 2, 3}];
conservationInverseSum = Total[1/conservationClosed[#] & /@ {1, 2, 3}];
check["conservation collection rejects squared whole-chain contexts",
  conservationCollected[conservationSquaredSum, conservationMasslessRules] === {}];
check["conservation collection rejects reciprocal whole-chain contexts",
  conservationCollected[conservationInverseSum, conservationMasslessRules] === {}];
conservationSquaredMoves = UnscrambleCandidates[
  {conservationMasslessRules, conservationSquaredSum}, "OnShellChannels" -> {}];
check["public candidates omit the false sum-of-squared-chains identity", !AnyTrue[
  conservationSquaredMoves, #["Name"] === "MomentumConservation" &&
    Unscrambling`Private`samePoly[#["Subtract"], conservationSquaredSum] &&
    Unscrambling`Private`samePoly[#["Insert"], -conservationClosed[4]^2] &]];
check["public candidates omit the false sum-of-reciprocal-chains identity", !AnyTrue[
  UnscrambleCandidates[{conservationMasslessRules, conservationInverseSum}, "OnShellChannels" -> {}],
  #["Name"] === "MomentumConservation" &&
    Unscrambling`Private`samePoly[#["Subtract"], conservationInverseSum] &&
    Unscrambling`Private`samePoly[#["Insert"], -1/conservationClosed[4]] &]];
conservationMomenta = {{1, 0, 0, 1}, {1, 0, 0, -1}, {-1, -1, 0, 0}, {-1, 1, 0, 0}};
conservationDot[a_, b_] := a[[1]] b[[1]] - Rest[a].Rest[b];
conservationNumericalChains = Table[conservationClosed[i] ->
  2 conservationDot[conservationMomenta[[4]], conservationMomenta[[i]]], {i, 4}];
conservationNumericalValue[expr_] := expr /. HoldPattern[
    SpinorChain[Spinor["Helicity", "Angle", 4], Mom[label_], Spinor["Helicity", "Square", 4]]] :>
  2 conservationDot[conservationMomenta[[4]],
    If[IntegerQ[label], conservationMomenta[[label]],
      Total[conservationMomenta[[List @@ label]]]]];
check["nonlinear regression fixture conserves all-ingoing massless momenta",
  Total[conservationMomenta] === {0, 0, 0, 0} &&
    (conservationDot[#, #] & /@ conservationMomenta) === {0, 0, 0, 0}];
check["independent physical fixture distinguishes linear and squared conservation",
  (Total[conservationClosed /@ Range[4]] /. conservationNumericalChains) === 0 &&
    (conservationSquaredSum /. conservationNumericalChains) === 24 &&
    (-conservationClosed[4]^2 /. conservationNumericalChains) === 0];
conservationScramble = ComplicateAmplitude[{conservationMasslessRules, conservationSquaredSum}, 1,
  RandomSeed -> 18, "RecordSteps" -> True, "OnShellChannels" -> {}];
check["former failing random seed now preserves the physical amplitude",
  Length[conservationScramble["Trajectory"]] === 1 &&
    Simplify[conservationNumericalValue[conservationScramble["Expression"]]] === 24];

conservationChain[i_] := SpinorChain[Spinor["Spin", "Angle", 1], Mom[i],
  Spinor["Spin", "Square", 2]];
conservationTrace[i_] := SpinorTrace[Mom[1], Mom[i]];
conservationFactor = 13 SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]];
conservationDenominator = PropDen[Mom[Multiparticle[1, 2]], MZ];
conservationSharedChain = conservationChain[1] Total[conservationChain /@ {1, 2, 3}];
conservationMoves = conservationCollected[conservationSharedChain, complementFourMasses];
check["linear collection preserves a shared positive chain multiplier",
  AnyTrue[conservationMoves, Unscrambling`Private`samePoly[
    #["Insert"], -conservationChain[1] conservationChain[4]] &]];
conservationSharedTrace = conservationTrace[1] Total[conservationTrace /@ {1, 2, 3}];
conservationMoves = conservationCollected[conservationSharedTrace, complementFourMasses];
check["linear collection preserves a shared positive trace multiplier",
  AnyTrue[conservationMoves, Unscrambling`Private`samePoly[
    #["Insert"], -conservationTrace[1] conservationTrace[4]] &]];
Do[
  conservationRules = Take[complementFiveMasses, legCount];
  conservationIDs = AssociationThread[Range[legCount], labels];
  conservationPermutedRules = Unscrambling`Private`relabelLegs[conservationRules, conservationIDs];
  conservationLinear = conservationFactor/conservationDenominator*
    Total[conservationChain /@ Range[legCount - 1]];
  conservationExpected = -conservationFactor/conservationDenominator*conservationChain[legCount];
  conservationMoves = conservationCollected[
    Unscrambling`Private`relabelLegs[conservationLinear, conservationIDs], conservationPermutedRules];
  conservationSuffix = " n=" <> ToString[legCount] <> " labels=" <> ToString[labels];
  check["linear chain sums retain conservation collection" <> conservationSuffix,
    AnyTrue[conservationMoves, Unscrambling`Private`samePoly[#["Insert"],
      Unscrambling`Private`canonicalChains[
        Unscrambling`Private`relabelLegs[conservationExpected, conservationIDs]]] &]];
  conservationLinear = conservationFactor/conservationDenominator*
    Total[conservationTrace /@ Range[legCount - 1]];
  conservationExpected = -conservationFactor/conservationDenominator*conservationTrace[legCount];
  conservationMoves = conservationCollected[
    Unscrambling`Private`relabelLegs[conservationLinear, conservationIDs], conservationPermutedRules];
  check["linear trace sums retain conservation collection" <> conservationSuffix,
    AnyTrue[conservationMoves, Unscrambling`Private`samePoly[#["Insert"],
      Unscrambling`Private`canonicalChains[
        Unscrambling`Private`relabelLegs[conservationExpected, conservationIDs]]] &]];
  conservationAnchor = conservationIDs[1];
  conservationRow = m1^2 + Total[MomProd[1, #] & /@ Range[2, legCount]];
  conservationRowState = Unscrambling`Private`relabelLegs[
    conservationFactor conservationRow/conservationDenominator, conservationIDs];
  conservationRowMoves = Block[{Unscrambling`Private`$useMomentumConservation = True,
      Unscrambling`Private`$onShellChannels = {}},
    Select[Unscrambling`Private`scalarMomentumConservationMoves[
      conservationRowState, conservationPermutedRules],
      Lookup[#, "Identity", None] === "ScalarRow" && #["Legs"] === {conservationAnchor} &]];
  check["complete dot-product row still contracts to zero" <> conservationSuffix,
    conservationRowMoves =!= {} && AllTrue[conservationRowMoves,
      Unscrambling`Private`applyCandidate[conservationRowState, #] === 0 &]];
  conservationRowSource = Unscrambling`Private`relabelLegs[
    -conservationFactor m1^2/conservationDenominator, conservationIDs];
  conservationRowExpected = Unscrambling`Private`relabelLegs[
    conservationFactor Total[MomProd[1, #] & /@ Range[2, legCount]]/conservationDenominator,
    conservationIDs];
  conservationRowMoves = Block[{Unscrambling`Private`$useMomentumConservation = True,
      Unscrambling`Private`$onShellChannels = {}},
    Select[Unscrambling`Private`scalarMomentumConservationMoves[
      conservationRowSource, conservationPermutedRules],
      Lookup[#, "Identity", None] === "ScalarRowExpand" && #["Legs"] === {conservationAnchor} &]];
  check["dot-product row expansion keeps every external momentum" <> conservationSuffix,
    AnyTrue[conservationRowMoves, Unscrambling`Private`samePoly[
      #["Insert"], conservationRowExpected] &]];
  check["row identities preserve opaque propagator atoms" <> conservationSuffix,
    conservationRowMoves =!= {} && AllTrue[conservationRowMoves,
      DeleteDuplicates[Cases[#["Insert"], _PropDen, Infinity]] ===
        {Unscrambling`Private`relabelLegs[conservationDenominator, conservationIDs]} &]],
  {legCount, {4, 5}}, {labels, {Range[legCount], Take[{40, 10, 80, 20, 50}, legCount]}}];

conservationSquareState = conservationClosed[1]^2;
conservationSquareInitial = conservationSquareState;
conservationSquareMoves[state_] := Block[{Unscrambling`Private`$useMomentumConservation = True},
  Select[Unscrambling`Private`chainMoves[state, conservationMasslessRules],
    #["Name"] === "MomentumConservation" && With[{site = #["Site"]},
      Unscrambling`Private`chainsIn[Unscrambling`Private`monomialsOf[state][[site[[1]]]]][[
        site[[2]], site[[3]] + 1]] === Mom[1]] &]];
conservationMoves = conservationSquareMoves[conservationSquareState];
check["squared chains keep legitimate single-slot conservation edits", conservationMoves =!= {}];
If[conservationMoves =!= {},
  conservationSquareState = Unscrambling`Private`applyCandidate[
    conservationSquareState, First[conservationMoves]]];
check["first conservation edit of a square keeps the unedited factor",
  Unscrambling`Private`samePoly[conservationSquareState,
    -conservationClosed[1] Total[conservationClosed /@ {2, 3, 4}]]];
Do[
  conservationMoves = conservationSquareMoves[conservationSquareState];
  If[conservationMoves === {}, Break[]];
  conservationSquareState = Unscrambling`Private`applyCandidate[
    conservationSquareState, First[conservationMoves]], {3}];
check["successive slot conservation edits retain all square cross terms",
  Unscrambling`Private`samePoly[conservationSquareState,
    Expand[Total[conservationClosed /@ {2, 3, 4}]^2]]];
check["squared slot conservation preserves the physical amplitude at both endpoints",
  (conservationSquareInitial /. conservationNumericalChains) === 4 &&
    (conservationSquareState /. conservationNumericalChains) === 4];
check["conservation fix still respects disabled momentum conservation", FreeQ[
  Lookup[UnscrambleCandidates[{conservationMasslessRules, conservationSquaredSum},
    "MomentumConservation" -> False, "OnShellChannels" -> {}], "Name"], "MomentumConservation"]];
Print[<|"Checks" -> checks, "Failed" -> Length[failures], "Failures" -> failures|>];
Quit[If[failures === {}, 0, 1]];
