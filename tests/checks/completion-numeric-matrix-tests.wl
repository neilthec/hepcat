(* Exact numeric assembly must preserve every affine row and coordinate. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root, "source", "HEPCAT.wl"}]];
checks = 0; failures = {};
check[name_, test_] := (checks++; If[!TrueQ[test], AppendTo[failures, name]; Print["FAIL: ", name]]);
normalize[matrix_, seconds_: 3] := ConstructiveDiagrams`Private`completionCanonicalNumericCoefficientMatrix[matrix, seconds];

cancel = (Sqrt[2] + Sqrt[3])^2 - 5 - 2 Sqrt[6];
raw = {{cancel, 1 + cancel, -2}, {1 + cancel, cancel, 3},
  {1/10^80, 0, 0}, {cancel, 1 + cancel, -2}};
report = normalize[raw];
check["exact ordinary radical cancellation", AssociationQ[report] &&
  report["Matrix"] === {{0, 1, -2}, {1, 0, 3}, {1/10^80, 0, 0}, {0, 1, -2}}];
check["duplicate constraints and all original columns retained", AssociationQ[report] &&
  report["Dimensions"] === {4, 3} && report["OriginalDimensions"] === {4, 3}];
check["tiny nonzero coefficient is not rounded away", AssociationQ[report] &&
  report["Matrix"][[3, 1]] === 1/10^80];
check["no numerical approximation in matrix certificate", AssociationQ[report] &&
  report["NumericalApproximation"] === False && report["GaussianRationalMatrix"] === True &&
  And @@ Values[report["Certificate"]]];
check["each reused number has one normalization entry", AssociationQ[report] &&
  report["DistinctCoefficients"] === Length[DeleteDuplicates[Flatten[raw]]]];

complex = normalize[{{(Sqrt[2] + I Sqrt[3])^2 + 1 - 2 I Sqrt[6], 2 I/7}, {0, -3/11}}];
check["exact complex coefficient cancellations", AssociationQ[complex] &&
  complex["Matrix"] === {{0, 2 I/7}, {0, -3/11}}];
algebraic = normalize[{{Sqrt[2], 1}, {1, -Sqrt[3]}}];
check["genuine algebraic field entries remain exact", AssociationQ[algebraic] &&
  algebraic["Matrix"] === {{Sqrt[2], 1}, {1, -Sqrt[3]}} &&
  algebraic["GaussianRationalMatrix"] === False];
check["approximate input uses established solver route", MissingQ[normalize[{{1., 2}}]]];
check["symbolic fit coordinate is not mistaken for a numerical coefficient",
  MissingQ[normalize[{{unfixedCoefficient, 2}}]]];
check["ragged matrix rejected", MissingQ[normalize[{{1}, {2, 3}}]]];
check["exhausted allowance returns no claimed normalization", MissingQ[normalize[raw, 0]]];
check["singular coefficient is not accepted", MissingQ[normalize[{{ComplexInfinity, 1}}]]];

(* An independently solvable affine fixture checks that normalization changes
   neither a coupled equation nor its free-coordinate family. *)
aa = Unique["numericMatrixA"]; bb = Unique["numericMatrixB"]; cc = Unique["numericMatrixC"];
coupled = normalize[{{-3 + cancel, 1 + cancel, 1, 0}, {-6, 2, 2 + cancel, 0}}];
fit = ConstructiveDiagrams`Private`completionSolveLinear[
  (First[#] + Rest[#].{aa, bb, cc} &) /@ coupled["Matrix"], {aa, bb, cc}];
check["coupled exact affine solve preserves two free coordinates",
  AssociationQ[fit] && fit["Status"] === "UNDERDETERMINED" && fit["Rank"] === 1 &&
  Length[fit["FreeParameters"]] === 2];
point = {aa, bb, cc} /. fit["SolutionRules"];
check["full original affine equations replay after solve",
  And @@ (Together[#] === 0 & /@ ((First[#] + Rest[#].point &) /@
    {{-3 + cancel, 1 + cancel, 1, 0}, {-6, 2, 2 + cancel, 0}}))];

Print["CHECKS: ", checks, "; FAILURES: ", Length[failures]];
If[failures =!= {}, Print[failures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:completion-numeric-matrix-tests.wl"];
Quit[0];
