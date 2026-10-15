(* Full legal-board diagnostics and independently verified learner recovery. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Block[{Print}, Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
  Get[FileNameJoin[{root, "tests", "unscrambling.wl"}]]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]]);
rules = Table[Mass[i] -> i + 1, {i, 4}];
den = PropDen[Mom[Multiparticle[1, 2]], Mass[Multiparticle[1, 2]], MomProd[1, 2]];
angle = SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]];
start = SpinorChain[Spinor["Spin", "Angle", 1], Mom[3], Mom[3], Spinor["Spin", "Angle", 2]]/den;
goal = 16 angle/den;
settings = Join[Unscrambling`Private`trainingContinuationDefaults,
  <|"MomentumConservation" -> False, "OnShellChannels" -> {}, "Seconds" -> 15,
    "StepTimeLimit" -> 5, "AnchorSteps" -> {1}, "MaxAttempts" -> 1|>];
Block[{Unscrambling`Private`$useMomentumConservation = False,
    Unscrambling`Private`$onShellChannels = {}},
  candidates = Unscrambling`Private`legalMoves[start, rules]];
labels = Boole[Unscrambling`Private`samePoly[
  Unscrambling`Private`applyCandidate[start, #], goal]] & /@ candidates;
teacher = First[FirstPosition[labels, 1]];
competing = First[Select[Range[Length[candidates]], candidates[[#]]["Name"] === "Mass" &]];
scores = ConstantArray[-5., Length[candidates]];
scores[[teacher]] = 2.; scores[[competing]] = 7.;
audit = Unscrambling`Private`trainingFullBoardAudit[candidates, labels, scores];
check["audit ranks teacher against all legal alternatives", audit["Passed"] &&
  audit["Scope"] === "FullLegalBoard" && audit["TeacherRank"] === 2 &&
  audit["FullActionCount"] === Length[candidates] && !audit["TopChoiceTeacher"] && audit["TeacherMargin"] == -5.];
check["audit probabilities normalized over full board", 0. < audit["TeacherProbability"] < 0.01];
order = Reverse[Range[Length[candidates]]];
reordered = Unscrambling`Private`trainingFullBoardAudit[candidates[[order]], labels[[order]], scores[[order]]];
check["audit invariant under board enumeration order", KeyDrop[audit, "TopChoiceIdentity"] ===
  KeyDrop[reordered, "TopChoiceIdentity"]];
check["audit rejects missing scores", !Unscrambling`Private`trainingFullBoardAudit[candidates, labels, Most[scores]]["Passed"]];
check["audit rejects nonfinite scores", !Unscrambling`Private`trainingFullBoardAudit[
  candidates, labels, ReplacePart[scores, 1 -> Infinity]]["Passed"]];
check["audit rejects absent verified teacher", !Unscrambling`Private`trainingFullBoardAudit[
  candidates, ConstantArray[0, Length[labels]], scores]["Passed"]];

(* The learner favors an expansion; the teacher must prove its way back. *)
learner = Function[{expression, masses, board}, N[Map[Which[
  #["Name"] === "Mass" && Lookup[#, "Direction", None] === "Expand", 10,
  #["Name"] === "ChainSquare" && Lookup[#, "Direction", None] === "Contract", 5,
  #["Name"] === "Stop", -10, True, 0] &, board]]];
learnerState = Unscrambling`Private`applyCandidate[start,
  candidates[[First[Ordering[learner[start, rules, candidates], -1]]]]];
continuation = Unscrambling`Private`trainingRecoverContinuation[
  {rules, learnerState}, {start, goal}, learner, settings];
check["off-path learner expression has verified continuation", TrueQ[continuation["Passed"]] &&
  TrueQ[continuation["EndpointVerified"]] && Unscrambling`Private`samePoly[Last[continuation["States"]], goal]];
check["continuation preserves opaque denominators", AllTrue[continuation["States"],
  DeleteDuplicates[Cases[#, _PropDen, Infinity]] === {den} &]];
check["continuation records only atomic legal identities", Length[continuation["RouteIdentities"]] ===
  Length[continuation["States"]] - 1 && FreeQ[continuation["RouteIdentities"], "Stop"]];
job = <|"Name" -> "Test route", "ReferenceName" -> "Test route", "Mode" -> "SimplificationRoute",
  "Kind" -> "Training", "Curated" -> True, "Pair" -> {rules, start}, "States" -> {start, goal},
  "OnShellChannels" -> {}, "MomentumConservation" -> False,
  "LegPermutation" -> AssociationThread[Range[4], Range[4]]|>;
examples = Unscrambling`Private`trainingLearnerRecoveryExamples[job, learner, settings];
check["learner recovery creates complete proven route", Length[examples["Routes"]] === 1 &&
  First[examples["Routes"]]["ModelRecovery"] && First[examples["Routes"]]["EndpointVerified"]];
check["recovery starts at learner state not its failed choice", Unscrambling`Private`samePoly[
  First[First[examples["Routes"]]["States"]], learnerState]];
check["recovery never teaches a return to the visited source state", AllTrue[
  First[examples["Routes"]]["States"], !Unscrambling`Private`samePoly[#, start] &]];
check["recovery preserves source legality metadata", KeyTake[First[examples["Routes"]],
  {"OnShellChannels", "MomentumConservation", "LegPermutation", "Kind"}] === KeyTake[job,
  {"OnShellChannels", "MomentumConservation", "LegPermutation", "Kind"}]];
stopScorer = Function[{expression, masses, board}, N[Boole[#["Name"] === "Stop"] & /@ board]];
stopped = Unscrambling`Private`trainingLearnerRecoveryExamples[job, stopScorer, settings];
check["premature learner Stop never creates teacher rows", stopped["Routes"] === {} &&
  First[stopped["Reports"]]["Reason"] === "PrematureLearnerStop"];
held = Unscrambling`Private`trainingLearnerRecoveryExamples[Join[job, <|"Kind" -> "Heldout"|>], learner, settings];
check["heldout sources excluded from recovery", held["Routes"] === {} &&
  First[held["Reports"]]["Reason"] === "InvalidRecoverySource"];
badScores = Function[{expression, masses, board}, ConstantArray[Infinity, Length[board]]];
bad = Unscrambling`Private`trainingLearnerRecoveryExamples[job, badScores, settings];
check["invalid model output cannot create recovery rows", bad["Routes"] === {} &&
  First[bad["Reports"]]["Reason"] === "LearnerScoringFailed"];
check["illegal suffix cannot become training target", !Quiet[Unscrambling`Private`trainingVerifyContinuation[
  {start, goal + 1}, rules, settings]]["Passed"]];
check["cycles after mass substitution are rejected", !Unscrambling`Private`trainingVerifyContinuation[
  {Mass[1] angle/den, start, 2 angle/den}, rules, settings]["Passed"]];
check["expression limits apply to direct rejoins", !Unscrambling`Private`trainingRecoverContinuation[
  {rules, learnerState}, {start, goal}, learner, Join[settings, <|"MaxExpressionLeaves" -> 1|>]]["Passed"]];
check["invalid node budget rejected", !Unscrambling`Private`trainingRecoverContinuation[
  {rules, learnerState}, {start, goal}, learner, Join[settings, <|"MaxNodes" -> 0|>]]["Passed"]];
slowScorer = Function[{expression, masses, board}, Pause[1]; ConstantArray[0., Length[board]]];
timed = Unscrambling`Private`trainingLearnerRecoveryExamples[job, slowScorer,
  Join[settings, <|"Seconds" -> 0.01|>]];
check["native preparation deadline never yields partial recovery", timed["Routes"] === {} &&
  First[timed["Reports"]]["Reason"] === "LearnerRecoveryTimeout"];

(* Shell scope is local to the diagram and never applied inside PropDen. *)
shellRules = Join[rules, {Mass[Multiparticle[1, 2]] -> z}];
shellStart = MomProd[1, 2]/den;
shellGoal = (z^2 - 13)/(2 den);
offShellProof = Block[{Unscrambling`Private`$onShellChannels = Automatic},
  Quiet[Unscrambling`Private`trainingVerifyContinuation[{shellStart, shellGoal}, shellRules, settings]]];
onShellProof = Block[{Unscrambling`Private`$onShellChannels = {}},
  Unscrambling`Private`trainingVerifyContinuation[{shellStart, shellGoal}, shellRules,
    Join[settings, <|"OnShellChannels" -> Automatic|>]]];
check["verification obeys explicit empty channel scope", !offShellProof["Passed"]];
check["verification obeys explicit diagram channel scope", onShellProof["Passed"]];

metadata = Get[FileNameJoin[{root, "tests", "unscramble-compact.m"}]];
check["training-only helpers preserve installed model compatibility",
  Unscrambling`Private`compactParticleModelCompatibleQ[metadata]];
weights = Unscrambling`Private`compactParticlePolicyArrays[
  Import[FileNameJoin[{root, "tests", "unscramble-compact.wlnet"}]]];
fullScores = Unscrambling`Private`trainingCompactCandidateScores[weights, start, rules, candidates, 3];
wholeScores = Unscrambling`Private`trainingCompactCandidateScores[weights, start, rules, candidates, 128];
check["portable full-board scoring returns every finite score", Unscrambling`Private`trainingFiniteScoresQ[
  fullScores, Length[candidates]]];
check["full-board scoring is chunk-size invariant", Max[Abs[fullScores - wholeScores]] < 1.*^-6];
check["invalid scoring chunk rejected", Unscrambling`Private`trainingCompactCandidateScores[
  weights, start, rules, candidates, 0] === $Failed];
Print["Training guidance checks: ", checks, "; failures: ", failures];
Quit[If[failures === {}, 0, 1]];
