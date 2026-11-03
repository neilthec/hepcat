(* Ranking measures completed expressions independently of reduction-search
   expansion budgets. Equal weighted scores prefer expression size, then
   additive groups; the diagnostic tuple keeps its existing field order. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0; checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);
rkRank = ConstructiveDiagrams`Private`preparationRank;
rkBetter = ConstructiveDiagrams`Private`preparationBetterQ;
rkPair = Get[FileNameJoin[{root, "tests", "checks", "fixtures", "preparation-rank-physical.wl"}]][rkMass, rkCharge];
rkPreferred = rkPair["Preferred"]; rkLeafSmaller = rkPair["LeafSmaller"];
rkPreferredRank = rkRank[rkPreferred]; rkLeafSmallerRank = rkRank[rkLeafSmaller];
check["the real preferred expression lies beyond the identity-expansion leaf budget",
  !ConstructiveDiagrams`Private`rspExpansionBudgetQ[rkPreferred] &&
  ConstructiveDiagrams`Private`rspExpansionProfile[rkPreferred, 4096, 200000][[2]] > 24000];
check["both real alternatives receive bounded weighted-complexity measurements",
  Last[rkPreferredRank] === "SpinorExpressionComplexity" && Last[rkLeafSmallerRank] === "SpinorExpressionComplexity"];
check["the actual lower-complexity form keeps its expected score", First[rkPreferredRank] === 2478];
check["the leaf-smaller alternative really has worse weighted complexity",
  First[rkLeafSmallerRank] === 2552 && LeafCount[rkLeafSmaller] < LeafCount[rkPreferred]];
check["smaller leaves cannot replace the actual better physical expression", !rkBetter[rkLeafSmallerRank, rkPreferredRank]];
check["the better physical expression is accepted in the opposite direction", rkBetter[rkPreferredRank, rkLeafSmallerRank]];
Do[rkValue = value; rkMeasured = rkRank[rkValue];
  check[{"normal polynomial ranks preserve their first three coordinates", value},
    Take[rkMeasured, 3] === {SpinorExpressionComplexity[rkValue],
      If[Head[rkValue] === Plus, Length[rkValue], If[rkValue === 0, 0, 1]], LeafCount[rkValue]} &&
    Last[rkMeasured] === "SpinorExpressionComplexity"],
  {value, {0, 1, rkX + 2 rkY, (rkX + rkY)^3,
    SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity", "Square", 4]]^2/
      PropDen[Mom[Multiparticle[1, 3]], rkMass]}}];
check["tagged weighted ties are unchanged", !rkBetter[{2, 3, 4, "SpinorExpressionComplexity"}, {2, 3, 4, "SpinorExpressionComplexity"}]];
check["equal-complexity candidates prefer smaller expressions before fewer additive groups",
  rkBetter[{2, 3, 4, "SpinorExpressionComplexity"}, {2, 2, 9, "SpinorExpressionComplexity"}] &&
  rkBetter[{2, 2, 8, "SpinorExpressionComplexity"}, {2, 2, 9, "SpinorExpressionComplexity"}] &&
  rkBetter[{2, 2, 8, "SpinorExpressionComplexity"}, {2, 3, 8, "SpinorExpressionComplexity"}]];
rkOpaque = (rkX + rkY + rkZ)^1000000;
rkOpaqueTime = First[AbsoluteTiming[rkOpaqueRank = rkRank[rkOpaque]]];
check["an enormous unexpanded power retains a bounded opaque fallback",
  Last[rkOpaqueRank] === "LeafCountFallback" && First[rkOpaqueRank] === LeafCount[rkOpaque] && rkOpaqueTime < 2];
check["an unmeasured fallback never wins through an artificially small leaf number",
  !rkBetter[{1, 1, 1, "LeafCountFallback"}, {100, 1, 100, "SpinorExpressionComplexity"}]];
check["a measured number is not compared with an unmeasured leaf score",
  !rkBetter[{1, 1, 1, "SpinorExpressionComplexity"}, {100, 1, 100, "LeafCountFallback"}]];
check["fallback candidates can still compare the same representation-size metric",
  rkBetter[{8, 2, 8, "LeafCountFallback"}, {9, 2, 9, "LeafCountFallback"}]];
check["private legacy three-coordinate comparisons retain their behavior",
  rkBetter[{1, 2, 3}, {2, 1, 1}] && !rkBetter[{2, 1, 1}, {1, 2, 3}] && !rkBetter[{1, 2, 3}, {1, 2, 3}]];
check["legacy untagged and measured ranks never mix",
  !rkBetter[{1, 2, 3}, {2, 3, 4, "SpinorExpressionComplexity"}] &&
  !rkBetter[{1, 2, 3, "SpinorExpressionComplexity"}, {2, 3, 4}]];
check["unsupported tags cannot establish an improvement",
  !rkBetter[{1, 1, 1, "Unknown"}, {2, 2, 2, "Unknown"}]];
rkTimed = Internal`InheritedBlock[{ConstructiveDiagrams`Private`expressionComplexity},
  Clear[ConstructiveDiagrams`Private`expressionComplexity];
  ConstructiveDiagrams`Private`expressionComplexity[_] := (Pause[1]; 1);
  First[AbsoluteTiming[rkTimedRank = rkRank[rkX + rkY]]]];
check["a slow weighted measurement keeps a tagged completed fallback",
  Last[rkTimedRank] === "LeafCountFallback" && First[rkTimedRank] === LeafCount[rkX + rkY] && rkTimed < 0.8];
rkOuter = Internal`InheritedBlock[{ConstructiveDiagrams`Private`expressionComplexity},
  Clear[ConstructiveDiagrams`Private`expressionComplexity];
  ConstructiveDiagrams`Private`expressionComplexity[_] := (Pause[1]; 1);
  TimeConstrained[rkRank[rkX + rkY], 0.02, Missing["OuterDeadline"]]];
check["the enclosing operation's earlier deadline remains authoritative", rkOuter === Missing["OuterDeadline"]];
rkModel = ReadModel[FileNameJoin[{root, "models"}], 3];
rkReal = Get[FileNameJoin[{root, "tests", "checks", "fixtures", "preparation-rank-physical.wl"}]][
  ModelParameter[rkModel, "Me"], ModelParameter[rkModel, "EE"]];
rkRules = Thread[Mass /@ Range[6] -> {ModelParameter[rkModel, "Me"], ModelParameter[rkModel, "Me"], 0, 0, 0, 0}];
rkPrepared = PrepareAmplitudes[{rkRules, rkReal["Preferred"]}, rkModel,
  "InternalOnShell" -> False, "PolePreference" -> None, "TimeConstraint" -> 0.05,
  "Parallelization" -> False, "ProgressReporting" -> False, "ReturnDetails" -> True];
check["element snapshots no longer impose the reducer's smaller ranking budget",
  AssociationQ[rkPrepared] && Last[First[rkPrepared["ItemReports"]]["PreparedRank"]] === "SpinorExpressionComplexity" &&
  First[First[rkPrepared["ItemReports"]]["PreparedRank"]] <= 2478];
Print[checks, " preparation-ranking checks; ", Length[failures], " failures."];
If[failures =!= {}, Print[failures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:preparation-ranking-tests.wl"]; Quit[0];
