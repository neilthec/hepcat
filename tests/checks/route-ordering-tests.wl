root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
Get[FileNameJoin[{root, "tests", "unscrambling.wl"}]];
Get[FileNameJoin[{root, "tests", "training-amplitudes.wl"}]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]]);
chain[i_, momenta_List, j_] := SpinorChain @@ Join[
  {Spinor["Spin", "Angle", i]}, Mom /@ momenta,
  {Spinor["Spin", If[EvenQ[Length[momenta]], "Angle", "Square"], j]}];
rules = {Mass[10] -> Me, Mass[20] -> Me, Mass[30] -> Mm, Mass[40] -> Mm, Mass[50] -> Mh};
denominator = PropDen[Mom[Multiparticle[10, 20]], MZ];
state = Unscrambling`Private`canonicalChains[(chain[10, {}, 20] + chain[30, {30}, 40])/denominator];
states = {state}; moves = {};
record[selector_] := Module[{move},
  move = SelectFirst[Unscrambling`Private`legalMoves[state, rules], selector, None];
  If[move === None, Print["Could not construct route-ordering fixture."]; Exit[2]];
  state = Unscrambling`Private`applyCandidate[state, move];
  AppendTo[states, state]; AppendTo[moves, move]
];
Block[{Unscrambling`Private`$onShellChannels = {}, Unscrambling`Private`$useMomentumConservation = False},
 record[Function[#["Name"] === "Mass" && Lookup[#, "Direction", ""] === "Expand" &&
   !FreeQ[#["Subtract"], SpinorChain[Spinor["Spin", "Angle", 10], Spinor["Spin", "Angle", 20]]]]];
 record[Function[#["Name"] === "Mass" && Lookup[#, "Direction", ""] === "Contract" &&
   !FreeQ[#["Subtract"], Mom[30]]]];
 record[Function[#["Name"] === "Mass" && Lookup[#, "Direction", ""] === "Contract" &&
   !FreeQ[#["Subtract"], Mom[10]]]]
];
route = <|"Name" -> "five-leg independent mass fixture", "Masses" -> rules, "States" -> states,
 "Identities" -> Lookup[moves, "Name"], "Moves" -> moves,
 "OnShellChannels" -> {}, "MomentumConservation" -> False, "Sources" -> {"route-ordering-tests.wl"}|>;
options = <|"Seconds" -> 10, "TotalSeconds" -> 20|>;
result = trainingOptimizeSimplificationRoutes[{route}, options];
optimized = First[result["Routes"]]; report = First[result["Reports"]];
check["bounded optimizer finds a lower peak in a generic five-leg route", TrueQ[report["Applied"]] &&
 report["Result"]["PeakComplexity"] < report["Original"]["PeakComplexity"]];
check["no directed rewrite changes in strict mode", report["DirectedRewriteChanges"] === 0];
check["all elementary deltas are preserved once", Sort[report["Order"]] === Range[Length[moves]] &&
 And @@ MapThread[Unscrambling`Private`samePoly,
   {Unscrambling`Private`canonicalChains /@ Differences[optimized["States"]],
    (Unscrambling`Private`canonicalChains /@ Differences[states])[[report["Order"]]]}]];
check["unchanged exact endpoint and mass/condition metadata",
 Unscrambling`Private`samePoly[Last[optimized["States"]], Last[states]] &&
 KeyDrop[optimized, {"States", "Identities", "Moves"}] === KeyDrop[route, {"States", "Identities", "Moves"}]];
check["unchanged propagators in every state", AllTrue[optimized["States"],
 DeleteDuplicates[Cases[#, _PropDen, Infinity]] === {denominator} &]];
job = First[Unscrambling`Private`makeSimplificationRouteJobs[{optimized}, 0]];
verified = Block[{Unscrambling`Private`$onShellChannels = {}, Unscrambling`Private`$useMomentumConservation = False},
 Unscrambling`Private`verifiedSimplificationRoute[job, <|"StepTimeLimit" -> 5|>]];
check["optimized route passes the training teacher verifier", AssociationQ[verified] &&
 Length[verified["Moves"]] === Length[moves]];
again = trainingOptimizeSimplificationRoutes[{route}, options];
check["ordering is deterministic", again["Routes"] === result["Routes"] &&
 First[again["Reports"]]["Order"] === report["Order"]];
check["optimizer does not consume caller randomness", BlockRandom[SeedRandom[77];
 trainingOptimizeSimplificationRoutes[{route}, options]; RandomInteger[100000]] ===
 BlockRandom[SeedRandom[77]; RandomInteger[100000]]];
fallBack[opts_, reason_] := Module[{attempt = trainingOptimizeSimplificationRoutes[{route}, Join[options, opts]]},
 attempt["Routes"] === {route} && First[attempt["Reports"]]["Reason"] === reason];
check["invalid options preserve the exact original route", fallBack[<|"Seconds" -> 0|>, "InvalidOptions"]];
check["state limit preserves the exact original route", fallBack[<|"MaxStates" -> 2|>, "StateLimit"]];
check["candidate limit preserves the exact original route", fallBack[<|"MaxCandidates" -> 1|>, "CandidateLimit"]];
check["expression size limit preserves the exact original route",
 fallBack[<|"MaxExpressionSize" -> 1|>, "ExpressionSizeLimit"]];
illegal = Join[route, <|"States" -> ReplacePart[states, 2 -> EE]|>];
illegalResult = trainingOptimizeSimplificationRoutes[{illegal}, options];
check["illegal input cannot yield partially optimized training states", illegalResult["Routes"] === {illegal} &&
 MemberQ[{"ChangingPropagators", "IllegalOriginalStep"}, First[illegalResult["Reports"]]["Reason"]]];
Block[{Unscrambling`Private`legalMoves},
 Unscrambling`Private`legalMoves[_, _] := (Pause[0.1]; {});
 check["time limit preserves the original and records the bounded failure",
   fallBack[<|"Seconds" -> 0.01|>, "TimeLimit"]];
 totalTimeout = trainingOptimizeSimplificationRoutes[{route, route},
   <|"Seconds" -> 0.1, "TotalSeconds" -> 0.01|>];
 check["total time budget bounds the entire route list", totalTimeout["Routes"] === {route, route} &&
   Lookup[totalTimeout["Reports"], "Reason"] === {"TimeLimit", "TotalTimeLimit"}]
];
minimal = Join[route, <|"States" -> Take[states, -2], "Identities" -> {Last[route["Identities"]]},
 "Moves" -> {Last[moves]}|>];
noChange = trainingOptimizeSimplificationRoutes[{minimal}, options];
check["already minimal order is returned exactly", noChange["Routes"] === {minimal} &&
 First[noChange["Reports"]]["Reason"] === "NoImprovement"];
check["empty route lists are supported", trainingOptimizeSimplificationRoutes[{}, options]["Routes"] === {}];
Print[<|"Checks" -> checks, "Failures" -> failures|>];
Exit[If[failures === {}, 0, 1]];
