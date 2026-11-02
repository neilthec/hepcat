(* Exact contradiction certificates must cover every original coefficient. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0; checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);
lnProof = ConstructiveDiagrams`Private`completionExactLeftNullContradiction;
lnVerify = ConstructiveDiagrams`Private`completionExactLeftNullCertificate;
lnCRT = ConstructiveDiagrams`Private`completionCertifiedModularReduce;
lnBlock = ConstructiveDiagrams`Private`completionExactBlockReduce;
lnSolve = ConstructiveDiagrams`Private`completionSolveLinear;
lnMatrix = {{1, 0, 2, 0}, {0, 1, 3, 0}, {1, 1, 5, 0}};
lnConstant = {0, 0, 1};
lnResult = lnProof[lnMatrix, lnConstant, 5];
lnCertificateQ[result_, matrix_, constant_] := AssociationQ[result] &&
  Lookup[result, "Status", None] === "INCONSISTENT" &&
  result["CheckedRows"] === Length[matrix] && result["CheckedColumns"] === Length[First[matrix]] &&
  result["LeftNullVector"].matrix === ConstantArray[0, Length[First[matrix]]] &&
  result["LeftNullVector"].constant === result["NonzeroConstantResidual"] &&
  result["NonzeroConstantResidual"] =!= 0 && TrueQ[result["ExactAllColumnCertificate"]];
check["rank-deficient exact contradiction is verified on every original row and column",
  lnCertificateQ[lnResult, lnMatrix, lnConstant]];
check["a contradiction reports only the rank information its certificate proves",
  lnResult["Rank"] === Missing["NotComputedFromContradictionCertificate"] &&
  lnResult["RankLowerBound"] === 2 && lnResult["CoefficientRankLowerBound"] === 2];
check["a contradiction never fabricates affine RREF rows or a free nullspace",
  !KeyExistsQ[lnResult, "ReducedRows"] && !KeyExistsQ[lnResult, "SolutionRules"]];
lnRationalMatrix = MapIndexed[#1/(First[#2] + 1) &, lnMatrix, {2}];
lnRationalConstant = {0, 0, 1/13};
lnRational = lnProof[lnRationalMatrix, lnRationalConstant, 5];
check["row denominator clearing preserves the original rational contradiction",
  lnCertificateQ[lnRational, lnRationalMatrix, lnRationalConstant]];
check["the independently replayable scaled witness is integral",
  VectorQ[lnRational["RowScales"], IntegerQ] &&
  VectorQ[lnRational["RowScaledLeftNullVector"], IntegerQ] &&
  lnRational["LeftNullVector"] === lnRational["RowScales"] lnRational["RowScaledLeftNullVector"]];
lnWitness = {-1, -1, 1};
check["a valid external witness is independently accepted",
  AssociationQ[lnVerify[lnMatrix, lnConstant, {1, 1, 1}, lnWitness]]];
check["a forged witness with a nonzero coefficient residual is rejected",
  MissingQ[lnVerify[lnMatrix, lnConstant, {1, 1, 1}, {0, -1, 1}]]];
check["changing a previously covered column invalidates the old witness",
  MissingQ[lnVerify[ReplacePart[lnMatrix, {3, 3} -> 6], lnConstant, {1, 1, 1}, lnWitness]]];
check["adding a formerly untested coefficient column invalidates the witness",
  MissingQ[lnVerify[MapThread[Append, {lnMatrix, {0, 0, 1}}],
    lnConstant, {1, 1, 1}, lnWitness]]];
check["zero contradiction residual cannot certify inconsistency",
  MissingQ[lnVerify[lnMatrix, {0, 0, 0}, {1, 1, 1}, lnWitness]]];
check["the zero witness cannot certify inconsistency",
  MissingQ[lnVerify[lnMatrix, lnConstant, {1, 1, 1}, {0, 0, 0}]]];
check["invalid row clearing cannot masquerade as an integer proof",
  MissingQ[lnVerify[lnRationalMatrix, lnRationalConstant, {1, 1, 1}, lnWitness]]];
check["singular row scaling and incorrect witness dimensions decline",
  MissingQ[lnVerify[lnMatrix, lnConstant, {1, 0, 1}, lnWitness]] &&
  MissingQ[lnVerify[lnMatrix, lnConstant, {1, 1, 1}, {-1, 1}]]];
lnConsistentConstant = -lnMatrix.{2, 3, 0, 0};
check["consistent exact equations do not yield a contradiction",
  MissingQ[lnProof[lnMatrix, lnConsistentConstant, 5]]];
check["a homogeneous nullspace remains available to the unchanged solver",
  MissingQ[lnProof[lnMatrix, {0, 0, 0}, 5]] &&
  AssociationQ[lnCRT[lnMatrix, {0, 0, 0}, 5]]];
lnBadPrime = lnProof[{{1, 0}, {0, 5}, {1, 0}}, {0, 0, 1}, 5, 5];
check["an unlucky modular rank does not undermine an exact all-column contradiction",
  lnCertificateQ[lnBadPrime, {{1, 0}, {0, 5}, {1, 0}}, {0, 0, 1}]];
check["a rank-dropping prime is not promoted to an exact coefficient rank",
  lnBadPrime["RankLowerBound"] === 1 && MissingQ[lnBadPrime["Rank"]] &&
  MatrixRank[{{1, 0}, {0, 5}, {1, 0}}] === 2];
check["the direct block API retains its rank-dropping-prime fallback contract",
  MissingQ[lnBlock[{{1, 0}, {0, 5}, {1, 0}}, {0, 0, 1}, 5]]];
check["a bad-prime false contradiction on a consistent system declines",
  MissingQ[lnProof[{{3}}, {1}, 5, 3]] &&
  lnCRT[{{3}}, {1}, 5, {3, 5, 7, 11}]["ReducedRows"] === {{1, -1/3}}];
lnZero = lnProof[{{0, 0}, {0, 0}}, {0, 1}, 5];
check["an entirely zero coefficient matrix has certified exact rank zero",
  lnCertificateQ[lnZero, {{0, 0}, {0, 0}}, {0, 1}] && lnZero["Rank"] === 0];
lnConstantRow = lnProof[{{1, 0}, {0, 0}}, {0, 1}, 5];
check["a constant-row contradiction does not infer the rank of unrelated rows",
  lnCertificateQ[lnConstantRow, {{1, 0}, {0, 0}}, {0, 1}] && MissingQ[lnConstantRow["Rank"]]];
check["invalid and exhausted clocks preserve fallback",
  AllTrue[{0, -1, Infinity, 1 + I, "invalid", 10^-9},
    MissingQ[lnProof[lnMatrix, lnConstant, #]] &]];
check["unsupported coefficient types never enter the exact rational certificate",
  AllTrue[{{{1., 0}}, {{Sqrt[2], 0}}, {{I, 0}}},
    MissingQ[lnProof[#, {1}, 5]] &]];
check["malformed dimensions and nonprime modular arithmetic decline",
  MissingQ[lnProof[lnMatrix, {1}, 5]] && MissingQ[lnProof[{{1}, {1, 2}}, {0, 1}, 5]] &&
  MissingQ[lnProof[lnMatrix, lnConstant, 5, 4]]];
lnWide = PadRight[lnMatrix, {3, 70}]; lnVariables = Array[lnVariable, 70];
lnWideResult = lnCRT[lnWide, lnConstant, 5];
check["the bounded CRT boundary consumes rank-deficient exact contradictions",
  lnCertificateQ[lnWideResult, lnWide, lnConstant] &&
  lnWideResult["Backend"] === "CertifiedExactLeftNullContradiction"];
lnFit = lnSolve[lnWide.lnVariables + lnConstant, lnVariables, 5];
check["the simultaneous solve terminates on the exact contradiction without a rank computation",
  lnFit["Status"] === "INCONSISTENT" && lnFit["SolutionRules"] === {} &&
  lnFit["FreeParameters"] === lnVariables && MissingQ[lnFit["Rank"]] &&
  lnFit["RankLowerBound"] === 2 &&
  lnFit["InconsistencyCertificate"]["ExactAllColumnCertificate"] === True];
lnConsistentFit = lnSolve[lnWide.lnVariables + lnConsistentConstant, lnVariables, 5];
lnFamily = lnVariables /. lnConsistentFit["SolutionRules"];
check["successful affine outcomes retain exact numeric rank and every free direction",
  lnConsistentFit["Status"] === "UNDERDETERMINED" && lnConsistentFit["Rank"] === 2 &&
  Length[lnConsistentFit["FreeParameters"]] === 68 &&
  Expand[lnWide.lnFamily + lnConsistentConstant] === {0, 0, 0}];
lnPrevious = <|"Rank" -> 3, "SolutionRules" -> {}, "FreeParameters" -> lnVariables|>;
lnRefined = ConstructiveDiagrams`Private`completionRefineLinear[
  lnPrevious, lnWide.lnVariables + lnConstant, 5];
check["affine refinement propagates an unknown exact contradiction rank honestly",
  lnRefined["Status"] === "INCONSISTENT" && MissingQ[lnRefined["Rank"]] &&
  lnRefined["RankLowerBound"] === 5];
lnObservedBudgets = {};
lnBudgetProbe = Block[{ConstructiveDiagrams`Private`completionExactLeftNullContradiction =
    Function[{matrix, constant, seconds, prime}, AppendTo[lnObservedBudgets, seconds];
      Missing["ControlledCertificateDecline"]]}, lnCRT[lnWide, lnConstant, 1/10]];
check["the optional precheck receives only the remaining existing solver deadline",
  Length[lnObservedBudgets] === 1 && 0 < First[lnObservedBudgets] <= 1/10 && MissingQ[lnBudgetProbe]];
lnObservedBudgets = {};
lnLargeBudgetProbe = Block[{ConstructiveDiagrams`Private`completionExactLeftNullContradiction =
    Function[{matrix, constant, seconds, prime}, AppendTo[lnObservedBudgets, seconds];
      Missing["ControlledCertificateDecline"]]},
  lnCRT[PadRight[lnMatrix, {3, 877}], lnConstant, 60, {3, 5, 7}]];
check["a wide modular contradiction can use more than twenty seconds without extending its clock",
  MissingQ[lnLargeBudgetProbe] && Length[lnObservedBudgets] === 1 &&
    20 < First[lnObservedBudgets] <= 30];
lnObservedBudgets = {};
lnCappedBudgetProbe = Block[{ConstructiveDiagrams`Private`completionExactLeftNullContradiction =
    Function[{matrix, constant, seconds, prime}, AppendTo[lnObservedBudgets, seconds];
      Missing["ControlledCertificateDecline"]]},
  lnCRT[PadRight[lnMatrix, {3, 1600}], lnConstant, 600, {3, 5, 7}]];
check["the contradiction attempt retains its finite independent ceiling",
  MissingQ[lnCappedBudgetProbe] && lnObservedBudgets === {120.}];
lnObservedBudgets = {};
lnExpired = Block[{ConstructiveDiagrams`Private`completionExactLeftNullContradiction =
    Function[{matrix, constant, seconds, prime}, AppendTo[lnObservedBudgets, seconds];
      Missing["UnexpectedCertificateCall"]]}, lnCRT[PadRight[IdentityMatrix[96], {96, 128}],
    ConstantArray[1, 96], 10^-9]];
check["an expired overall solver clock cannot launch a fresh certificate clock",
  MissingQ[lnExpired] && lnObservedBudgets === {}];
Print["Exact left-null contradiction checks: ", checks, "; failures: ", Length[failures]];
Print["HEPCAT_TEST_COMPLETED:exact-left-null-contradiction-tests.wl"];
If[failures =!= {}, Print[failures]; Exit[1], Exit[0]];
