(* Exact chart-unit normalization preserves complete affine solution spaces. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root, "source", "HEPCAT.wl"}]];
$HistoryLength = 0;
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, assertion_] := (checks++; If[!TrueQ[assertion], AppendTo[failures, name]; Print["FAIL: ", name]]);
zero[value_] := AllTrue[Flatten[{value}], RootReduce[Expand[#]] === 0 &];
solve = ConstructiveDiagrams`Private`completionSolveLinear;
normalize = ConstructiveDiagrams`Private`completionAlgebraicRowUnits;
variables = {rowUnitU, rowUnitV, rowUnitW};
matrix = {{1, 2, -3}, {2, -1, 4}, {-2, 4, 1}};
point = {2, -3, 5}; constant = -matrix.point;
units = {Sqrt[2] + I Sqrt[3], Sqrt[5] - Sqrt[7], Root[#^3 - 2 &, 1]};
sourceMatrix = MapThread[Times, {matrix, units}]; sourceConstant = units constant;
normalized = normalize[sourceMatrix, sourceConstant, 5];
check["independent algebraic chart units are removed before the global matrix reduction",
  normalized["Normalization"]["AcceptedRows"] === 3 &&
    AllTrue[Flatten[{normalized["Matrix"], normalized["Constant"]}],
      MatchQ[#, _Integer | _Rational | _Complex] &]];
check["every accepted divisor has an exact nonzero proof and a replay of its original row",
  AllTrue[normalized["Normalization"]["Rows"],
    ConstructiveDiagrams`Private`highEnergyZeroState[#["ProvedNonzeroScale"]] === "Nonzero" &&
      zero[#["ExactReplay"]] && TrueQ[#["ExactRowEquivalence"]] &&
      !TrueQ[#["UnknownCoordinatesChanged"]] &]];
check["normalization records every original affine row and changes no unknown coordinates",
  normalized["Normalization"]["AllOriginalRowsRetained"] &&
    normalized["Normalization"]["OriginalRowCount"] === 3 &&
    AllTrue[normalized["Normalization"]["Rows"],
      #["OriginalAffineRow"] === Append[sourceMatrix[[#["Row"]]], sourceConstant[[#["Row"]]]] &]];
rows = sourceMatrix.variables + sourceConstant;
full = solve[rows, variables];
check["the full exact solution is expressed in the original coefficient coordinates",
  full["Status"] === "COMPLETE" && full["Rank"] === 3 &&
    zero[(variables /. full["SolutionRules"]) - point] && zero[rows /. full["SolutionRules"]]];
partial = solve[Take[rows, 2], variables];
check["a partial solution retains the original free coefficient without rescaling it",
  partial["Status"] === "UNDERDETERMINED" && partial["FreeParameters"] === {rowUnitW} &&
    (rowUnitW /. partial["SolutionRules"]) === rowUnitW &&
    zero[Take[rows, 2] /. partial["SolutionRules"]]];
refined = ConstructiveDiagrams`Private`completionRefineLinear[partial, {Last[rows]}];
check["incremental refinement and a batch solve give the same exact point",
  refined["Status"] === "COMPLETE" && zero[(variables /. refined["SolutionRules"]) - point]];
contradiction = solve[Append[rows, First[rows] + 1/1000000000000000000], variables];
check["algebraic row units cannot hide a tiny exact contradiction",
  contradiction["Status"] === "INCONSISTENT"];
zeroUnit = Root[#^2 - 2 &, 2] - Sqrt[2];
guard = normalize[{{zeroUnit, zeroUnit, zeroUnit}}, {zeroUnit}, 2];
check["an exactly zero algebraic unit is never used as a divisor",
  guard["Normalization"]["AcceptedRows"] === 0 &&
    FreeQ[{guard["Matrix"], guard["Constant"]}, Indeterminate | ComplexInfinity | _DirectedInfinity]];
unresolved = solve[{rowUnitUnresolved rowUnitU - 1}, {rowUnitU}];
check["an unresolved model coefficient remains unresolved rather than obtaining a numerical fit",
  unresolved["Status"] === "UNRESOLVED"];
inexact = normalize[N[sourceMatrix], N[sourceConstant], 2];
check["inexact values receive no exact nonzero or row-equivalence certificates",
  inexact["Normalization"]["AcceptedRows"] === 0];
stopped = normalize[sourceMatrix, sourceConstant, 0];
check["an exhausted normalization budget retains the complete original system",
  stopped["Matrix"] === sourceMatrix && stopped["Constant"] === sourceConstant &&
    stopped["Normalization"]["AcceptedRows"] === 0];
previous = <|"Status" -> "COMPLETE", "Equations" -> {rowUnitU + rowUnitV - 3},
  "Samples" -> {"original physical cut"}|>;
step1 = ConstructiveDiagrams`Private`completionSubstituteConstraintFamily[
  previous, {rowUnitU -> 3 - rowUnitV}];
step2 = ConstructiveDiagrams`Private`completionSubstituteConstraintFamily[
  step1, {rowUnitV -> 2}];
check["successive exact coefficient eliminations update earlier constraints without losing their original rows",
  step2["Equations"] === {0} && step2["UnreducedEquations"] === previous["Equations"] &&
    step2["Samples"] === previous["Samples"]];
familyReport = ConstructiveDiagrams`Private`completionHEFamilyReport[
  <|"FreeParameters" -> variables, "RemainingBasis" -> {1, 2, 3}|>,
  7 + 2 rowUnitV, {rowUnitV}, 4];
check["retained affine metadata immediately describes the current reduced family",
  familyReport["FamilyAmplitude"] === 7 + 2 rowUnitV &&
    familyReport["FreeParameters"] === {rowUnitV} && familyReport["RemainingBasis"] === {2} &&
    familyReport["Amplitude"] === 7 && familyReport["Correction"] === 3];
fixedReport = ConstructiveDiagrams`Private`completionHEFamilyReport[familyReport, 9, {}, 4];
check["a fully fixed family cannot retain stale free coordinates on an unresolved later check",
  fixedReport["FreeParameters"] === {} && fixedReport["RemainingBasis"] === {} &&
    fixedReport["FamilyAmplitude"] === fixedReport["Amplitude"] === 9 &&
    fixedReport["Correction"] === 5];
blockRows = Flatten[Table[(Sqrt[k + 1] + I Sqrt[k + 2]) matrix, {k, 2, 9}], 1];
blockConstant = -blockRows.point;
block = ConstructiveDiagrams`Private`completionExactAlgebraicBlockSolve[
  blockRows, blockConstant, variables, 8];
check["a small algebraic block certifies the complete solution against every original chart row",
  AssociationQ[block] && block["Status"] === "COMPLETE" &&
    zero[(variables /. block["SolutionRules"]) - point] &&
    block["AlgebraicBlockCertificate"]["AllOriginalRowsVerified"] &&
    block["AlgebraicBlockCertificate"]["CheckedRows"] === Length[blockRows]];
blockPartialRows = Flatten[Table[(Sqrt[k + 1] + I Sqrt[k + 2]) Take[matrix, 2], {k, 2, 9}], 1];
blockPartial = ConstructiveDiagrams`Private`completionExactAlgebraicBlockSolve[
  blockPartialRows, -blockPartialRows.point, variables, 8];
check["an algebraic block verifies every independent null direction in the original coordinates",
  AssociationQ[blockPartial] && blockPartial["Status"] === "UNDERDETERMINED" &&
    blockPartial["FreeParameters"] === {rowUnitW} &&
    blockPartial["AlgebraicBlockCertificate"]["FullNullspaceVerified"] &&
    zero[(blockPartialRows.variables - blockPartialRows.point) /. blockPartial["SolutionRules"]]];
blockBad = ConstructiveDiagrams`Private`completionExactAlgebraicBlockSolve[
  Append[blockRows, First[blockRows]],
  Append[blockConstant, First[blockConstant] + 10^-100], variables, 8];
check["an algebraic block cannot hide an exact contradiction below its numerical rank-proposal precision",
  AssociationQ[blockBad] && blockBad["Status"] === "INCONSISTENT" &&
    IntegerQ[blockBad["AlgebraicBlockCertificate"]["ContradictionRow"]]];
check["an exhausted algebraic-block certificate budget requests the unchanged native fallback",
  MissingQ[ConstructiveDiagrams`Private`completionExactAlgebraicBlockSolve[
    blockRows, blockConstant, variables, 0]]];
referenceMassRules = {Mass[1] -> 2, Mass[2] -> 2, Mass[3] -> 0, Mass[4] -> 3};
referenceMetadata = <|"ExternalMassRules" -> referenceMassRules, "ExternalLegs" -> Range[4]|>;
referenceChart = ConstructiveDiagrams`Private`completionHEReferenceFitChart[
  referenceMetadata, {}, 1, 11939, {}];
check["rational reference-fit samples conserve momentum and keep every external shell exactly",
  AssociationQ[referenceChart] && referenceChart["MomentumConservationValidated"] &&
    referenceChart["ExternalMassShellsValidated"] &&
    zero[Total[Values[referenceChart["Momenta"]]]] &&
    AllTrue[Range[4], zero[Det[referenceChart["Momenta"][#]] - (Mass[#] /. referenceMassRules)^2] &] &&
    referenceChart["InternalOnShellConditions"] === {}];
check["rational reference fitting retains complete coherent massive-spin bases without normalization roots",
  Length[referenceChart["SpinVariables"]] === 3 &&
    referenceChart["ExternalSpinorBasesValidated"] &&
    AllTrue[Flatten[Values[referenceChart["Momenta"]]], MatchQ[#, _Integer | _Rational] &] &&
    AllTrue[Keys[referenceChart["AngleBasis"]], Function[leg,
      zero[referenceChart["AngleBasis"][leg].{{0, 1}, {-1, 0}}.
        Transpose[referenceChart["SquareBasis"][leg]] - referenceChart["Momenta"][leg]]]]];
referenceReplay = ConstructiveDiagrams`Private`completionHEReferenceFitChart[
  referenceMetadata, {}, 1, 11939, {}];
check["an explicit seed reproduces the external kinematics without sharing mutable chart state",
  referenceReplay["Momenta"] === referenceChart["Momenta"] &&
    referenceReplay["SpinVariables"] =!= referenceChart["SpinVariables"]];
check["unsupported rational reference geometry requests the established physical-chart fallback",
  MissingQ[ConstructiveDiagrams`Private`completionHEReferenceFitChart[
    <|"ExternalMassRules" -> Take[referenceMassRules, 3]|>, {}, 1, 11939, {}]]];
manyVariables = Array[linearRefinementVariable, 128];
initialRows = Take[manyVariables, 32] - Range[32];
initialFit = solve[initialRows, manyVariables];
feedbackRows = Table[With[{left = 32 + index, right = 97 + Mod[index - 1, 32],
    chartUnit = If[EvenQ[index], Sqrt[2] + I Sqrt[3], Sqrt[5] - I Sqrt[7]]},
  chartUnit (manyVariables[[left]] + 2 manyVariables[[right]] - left - 2 right)], {index, 64}];
affineRows = ConstructiveDiagrams`Private`completionRefinementAffineRows[initialFit, feedbackRows, 10];
check["large feedback normalizes each raw chart before sparse affine substitution",
  AssociationQ[affineRows] && affineRows["Certificate"]["AffineMapReconstructionVerified"] &&
    affineRows["Certificate"]["OriginalRowCount"] === 64 &&
    affineRows["Certificate"]["RawRowNormalization"]["AcceptedRows"] === 64 &&
    affineRows["Certificate"]["UnknownCoordinatesChanged"] === False];
largeRefinement = ConstructiveDiagrams`Private`completionRefineLinear[initialFit, feedbackRows];
largeBatch = solve[Join[initialRows, feedbackRows], manyVariables];
check["sparse affine feedback preserves every original pivot and free-coordinate solution",
  largeRefinement["Status"] === "UNDERDETERMINED" && largeRefinement["Rank"] === 96 &&
    largeRefinement["FreeParameters"] === largeBatch["FreeParameters"] &&
    zero[(manyVariables /. largeRefinement["SolutionRules"]) -
      (manyVariables /. largeBatch["SolutionRules"])] &&
    zero[Join[initialRows, feedbackRows] /. largeRefinement["SolutionRules"]]];
oneFeedbackRow = ConstructiveDiagrams`Private`completionRefinementAffineRows[
  initialFit, Take[feedbackRows, 1], 5];
check["large affine families retain exact matrix composition even for a single held-out row",
  AssociationQ[oneFeedbackRow] && Length[oneFeedbackRow["Equations"]] === 1 &&
    oneFeedbackRow["Certificate"]["OriginalRowCount"] === 1 &&
    zero[oneFeedbackRow["Equations"] -
      (Take[feedbackRows, 1] /. initialFit["SolutionRules"])/
        First[oneFeedbackRow["Certificate"]["ExactRowUnits"]]["Scale"]]];
check["an exhausted sparse affine composition budget retains the native substitution route",
  MissingQ[ConstructiveDiagrams`Private`completionRefinementAffineRows[initialFit, feedbackRows, 0]]];
check["nonlinear original feedback is left to the established post-substitution check",
  MissingQ[ConstructiveDiagrams`Private`completionRefinementAffineRows[initialFit,
    ConstantArray[First[manyVariables]^2 - 1, 16], 2]]];
energyVariables = {energyChartU, energyChartV, energyChartW};
energyInitial = <|"Status" -> "UNDERDETERMINED", "SolutionRules" -> {},
  "FreeParameters" -> energyVariables, "Rank" -> 0|>;
energyFirstRows = {energyChartU + energyChartV - 1};
energyRestriction = ConstructiveDiagrams`Private`completionHEEnergyAffineRestriction[
  energyChartU MomProd[1, 2] + energyChartV MomProd[1, 3] + energyChartW,
  energyVariables, energyInitial, energyFirstRows, energyFirstRows, 2];
check["per-chart restriction retains the full original-coordinate affine nullspace",
  AssociationQ[energyRestriction] &&
    energyRestriction["FreeParameters"] === {energyChartV, energyChartW} &&
    energyRestriction["Certificate"]["EveryRetainedEquationVerified"] &&
    energyRestriction["Certificate"]["FullNullspaceRetained"] &&
    zero[energyFirstRows /. energyRestriction["LinearSystem"]["SolutionRules"]]];
check["unsupported or exhausted per-chart restriction keeps the established whole-family route",
  MissingQ[ConstructiveDiagrams`Private`completionHEEnergyAffineRestriction[
    energyChartU, energyVariables, energyInitial, {energyChartU^2 - 1},
    {energyChartU^2 - 1}, 2]] &&
    MissingQ[ConstructiveDiagrams`Private`completionHEEnergyAffineRestriction[
      energyChartU, energyVariables, energyInitial, energyFirstRows, energyFirstRows, 0]]];
energyMetadata = <|"ExternalLegs" -> Range[4],
  "ExternalMassRules" -> Thread[Mass /@ Range[4] -> 1]|>;
energyChartRows = ConstructiveDiagrams`Private`completionHEEnergyConstraints[
  energyChartU MomProd[1, 2] + energyChartV MomProd[1, 3] + energyChartW,
  energyVariables, energyMetadata, 3, 93191, {}, 0, None, False];
energyChartFit = solve[energyChartRows["Equations"], energyVariables];
check["sequential high-energy fitting still evaluates every independently seeded requested chart",
  energyChartRows["Status"] === "COMPLETE" &&
    Lookup[energyChartRows["Samples"], "Sample"] === Range[3] &&
    TrueQ[energyChartRows["AllRequestedChartsRetained"]] &&
    Length[energyChartRows["SequentialAffineReductions"]] >= 1 &&
    First[energyChartRows["Samples"]]["WorkingCorrectionCoordinates"] === energyVariables &&
    Length[energyChartRows["Samples"][[2]]["WorkingCorrectionCoordinates"]] < 3];
check["later energy charts constrain surviving directions while retaining a regular contact coefficient",
  energyChartFit["Status"] === "UNDERDETERMINED" &&
    energyChartFit["Rank"] === 2 && energyChartFit["FreeParameters"] === {energyChartW} &&
    zero[{energyChartU, energyChartV} /. energyChartFit["SolutionRules"]]];
energyUnrestrictedRows = Block[
  {ConstructiveDiagrams`Private`completionHEEnergyAffineRestriction = Function[Null,
    Missing["DiagnosticFallback"], HoldAll]},
  ConstructiveDiagrams`Private`completionHEEnergyConstraints[
    energyChartU MomProd[1, 2] + energyChartV MomProd[1, 3] + energyChartW,
    energyVariables, energyMetadata, 3, 93191, {}, 0, None, False]];
energyUnrestrictedFit = solve[energyUnrestrictedRows["Equations"], energyVariables];
check["exact per-chart restriction and its unsupported fallback yield the same complete affine solution",
  energyUnrestrictedRows["SequentialAffineReductions"] === {} &&
    energyChartFit["FreeParameters"] === energyUnrestrictedFit["FreeParameters"] &&
    zero[(energyVariables /. energyChartFit["SolutionRules"]) -
      (energyVariables /. energyUnrestrictedFit["SolutionRules"])]];
energyAssignments = {{energyChartScale -> 1}, {energyChartScale -> 2},
  {energyChartScale -> 3}};
energyAllSets = ConstructiveDiagrams`Private`completionHEEnergyConstraintSets[
  energyChartU MomProd[1, 2] + energyChartV MomProd[1, 3] + energyChartW,
  energyVariables, energyMetadata, 3, 93191, energyAssignments, 0, None, False];
check["certified energy restrictions carry across parameter assignments without dropping any chart",
  energyAllSets["Status"] === "COMPLETE" &&
    Length[energyAllSets["ParameterAssignments"]] === 3 && Length[energyAllSets["Samples"]] === 9 &&
    TrueQ[energyAllSets["AllRequestedParameterAssignmentsRetained"]] &&
    Length[energyAllSets["SequentialParameterAffineReductions"]] >= 1 &&
    energyAllSets["ParameterAssignments"][[2]]["WorkingCorrectionCoordinates"] === {energyChartW}];
energyVaryingSets = ConstructiveDiagrams`Private`completionHEEnergyConstraintSets[
  (energyChartU + energyChartScale energyChartV) MomProd[1, 2] + energyChartW,
  energyVariables, energyMetadata, 2, 93191, energyAssignments, 0, None, False];
energyVaryingFit = solve[energyVaryingSets["Equations"], energyVariables];
check["later model assignments still constrain coefficients left free at the first assignment",
  energyVaryingSets["Status"] === "COMPLETE" &&
    Length[energyVaryingSets["Samples"]] === 6 &&
    energyVaryingFit["Rank"] === 2 && energyVaryingFit["FreeParameters"] === {energyChartW} &&
    zero[{energyChartU, energyChartV} /. energyVaryingFit["SolutionRules"]]];
energyVaryingFallback = Block[
  {ConstructiveDiagrams`Private`completionHEEnergyAffineRestriction = Function[Null,
    Missing["DiagnosticFallback"], HoldAll]},
  ConstructiveDiagrams`Private`completionHEEnergyConstraintSets[
    (energyChartU + energyChartScale energyChartV) MomProd[1, 2] + energyChartW,
    energyVariables, energyMetadata, 2, 93191, energyAssignments, 0, None, False]];
energyVaryingFallbackFit = solve[energyVaryingFallback["Equations"], energyVariables];
check["unsupported cross-assignment restrictions preserve the unrestricted full affine solution",
  energyVaryingFallback["SequentialParameterAffineReductions"] === {} &&
    AllTrue[energyVaryingFallback["ParameterAssignments"],
      #["WorkingCorrectionCoordinates"] === energyVariables &] &&
    energyVaryingFit["FreeParameters"] === energyVaryingFallbackFit["FreeParameters"] &&
    zero[(energyVariables /. energyVaryingFit["SolutionRules"]) -
      (energyVariables /. energyVaryingFallbackFit["SolutionRules"])]];
Print[checks, " algebraic row-normalization checks; ", Length[failures], " failures."];
If[failures =!= {}, Exit[1]];
Print["HEPCAT_TEST_COMPLETED:linear-row-normalization-tests.wl"];
Exit[0];
