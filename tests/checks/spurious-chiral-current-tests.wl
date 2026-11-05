(* Equal-chirality currents: ordinary determinant algebra and typed topology. *)
scRoot = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{scRoot, "source", "HEPCAT.wl"}]];
$HistoryLength = 0;
scChecks = 0; scFailures = {};
SetAttributes[scCheck, HoldRest];
scCheck[name_, test_] := (scChecks++;
  If[!TrueQ[test], AppendTo[scFailures, name]; Print["FAIL: ", name]]);
scModel = ReadModel[FileNameJoin[{scRoot, "models"}], 3];
scDiagramModel = ReadModel[FileNameJoin[{scRoot, "models"}], 2];
scMass = ModelParameter[scModel, "Me"]; scCharge = ModelParameter[scModel, "EE"];
scExpression[vectors_List, kind_:1] :=
  ConstructiveDiagrams`Private`constructSameChiralSpuriousSubsetCurrent[
    {1, 2}, vectors, kind, scMass, ConstantArray[-Sqrt[2] scCharge, Length[vectors]]]["Expression"];
scPhysical[vectors_List, kind_:1] :=
  ConstructiveDiagrams`Private`constructPhysicalChiralCurrent[
    {1, 2}, vectors, kind, scMass, ConstantArray[-Sqrt[2] scCharge, Length[vectors]]]["Expression"];

(* Clear the reference denominators before evaluating a dense polynomial
   certificate. P^2=m^2 and each external vector is null; total conservation
   and internal-shell identities are not used. The three-emission identity
   is the sum of two-emission determinants times their untouched spectators. *)
scProofAngles = AssociationThread[Range[3, 6], Array[scProofAngle, {4, 2}]];
scProofSquares = AssociationThread[Range[3, 6], Array[scProofSquare, {4, 2}]];
scProofMomenta = Join[<|1 -> scProofMass IdentityMatrix[2]|>,
  Association@Table[i -> Outer[Times, scProofAngles[i], scProofSquares[i]], {i, 3, 6}]];
scProofChart = <|"Momenta" -> scProofMomenta,
  "Masses" -> <|1 -> scProofMass, 3 -> 0, 4 -> 0, 5 -> 0, 6 -> 0|>,
  "Angle" -> scProofAngles, "Square" -> scProofSquares|>;
Do[
  scProofKind = {"Angle", "Square"}[[scKind]];
  scProofDual = {"Square", "Angle"}[[scKind]];
  scProofSpin[type_, i_] := Spinor["Helicity", type, i];
  scProofBracket[type_, i_, j_] := SpinorChain[scProofSpin[type, i], scProofSpin[type, j]];
  scProofInsert[i_, indices_List] := Total[SpinorChain[scProofSpin[scProofKind, 6], Mom[#],
      scProofSpin[scProofDual, i]] & /@ indices];
  scProofPole[indices_List] := PropDen[Mom[Multiparticle @@ Prepend[indices, 1]], scProofMass];
  Do[
    scProofLegs = Take[Range[3, 5], scCount];
    scProofIdentity = Total[Function[i,
        scProofPole[{i}] scProofInsert[i, Prepend[DeleteCases[scProofLegs, i], 1]] *
          (Times @@ (scProofInsert[#, {1}] & /@ DeleteCases[scProofLegs, i]))] /@ scProofLegs] -
      scProofPole[scProofLegs] (Times @@ (scProofInsert[#, {1}] & /@ scProofLegs)) +
      scProofMass^2 Total[Function[pair,
        scProofBracket[scProofDual, Sequence @@ pair]^2 *
          (Times @@ (scProofBracket[scProofKind, 6, #] & /@ pair)) *
          (Times @@ (scProofInsert[#, {1}] & /@ Complement[scProofLegs, pair]))] /@
            Subsets[scProofLegs, {2}]];
    scCheck["exact " <> ToString[scCount] <> "-emission determinant, chirality " <> ToString[scKind],
      Cancel[Together[ConstructiveDiagrams`Private`residueEvaluateChart[
        scProofIdentity, scProofChart, {}]]] === 0],
    {scCount, 2, 3}],
  {scKind, 2}];

(* Existing four-, five-, six-point currents remain the reference fixtures. *)
Do[
  scVectors = Range[3, scN + 2];
  scCandidate = scExpression[scVectors];
  scExpected = scPhysical[scVectors];
  scExternal = Thread[Mass /@ Range[scN + 2] -> Join[{scMass, scMass}, ConstantArray[0, scN]]];
  scProcess = Join[{"e", "E"}, ConstantArray["A.+", scN]];
  scChannels = ConstructiveDiagrams`Private`residueSimpleInputs[{scExternal, 0},
    FactorizationAmplitudeChannels[scProcess, scModel], scModel]["Targets"];
  scChart = ConstructiveDiagrams`Private`amplitudeRationalComparisonChart[
    scChannels, 1, 20261106, {scMass -> 1, scCharge -> 1}];
  scCheck["regular exact lower-point comparison for " <> ToString[scN] <> " emissions",
    TrueQ[scChart["OffCutKinematicValidation"]["Valid"]] &&
      Cancel[Together[ConstructiveDiagrams`Private`residueEvaluateChart[
        scCandidate - scExpected, scChart, {scMass -> 1, scCharge -> 1}]]] === 0];
  If[scN === 3 || scN === 4,
    scCheck["existing spurious compactness for " <> ToString[scN] <> " emissions",
      SpinorExpressionComplexity[scCandidate] === If[scN === 3, 30, 120]]];
  If[scN === 4,
    scOldPair = ConstructiveDiagrams`Private`constructSameChiralPairSplit[
      {1, 2}, scVectors, 1, scMass, ConstantArray[-Sqrt[2] scCharge, scN]]["Expression"];
    scCheck["the generalized recurrence preserves the previous six-point pair split",
      Expand[ConstructiveDiagrams`Private`canonicalChains[
        ConstructiveDiagrams`Private`residueCanonicalPropagators[
          scCandidate - scOldPair, Range[6]]]] === 0]],
  {scN, 2, 4}];

scVectors = Range[3, 7];
scCandidates = {scExpression[scVectors, 1], scExpression[scVectors, 2]};
scReferences = {scPhysical[scVectors, 1], scPhysical[scVectors, 2]};
scCheck["seven-point compactness is bounded in either chirality",
  (SpinorExpressionComplexity /@ scCandidates) === {1160, 1160}];
scCheck["opposite helicity is the exact angle/square dual",
  Expand[ConstructiveDiagrams`Private`canonicalChains[
    Last[scCandidates] - (First[scCandidates] /. {"Angle" -> "Square", "Square" -> "Angle"})]] === 0];
scCheck["the compact current has actual spurious brackets and no auxiliary reference",
  FreeQ[scCandidates, (Spinor | SpinorHat)["Xi", ___]] &&
    AllTrue[scCandidates, ConstructiveDiagrams`Private`constructSpuriousBracketFormQ]];
scCheck["all ten pair reference brackets are represented",
  Length[DeleteDuplicates[Cases[First[scCandidates],
    Power[SpinorChain[Spinor["Helicity", "Angle", _Integer],
      Spinor["Helicity", "Angle", _Integer]], _Integer?Negative], Infinity]]] === 10];
scExternal = Thread[Mass /@ Range[7] -> {scMass, scMass, 0, 0, 0, 0, 0}];
scProcess = Join[{"e", "E"}, ConstantArray["A.+", 5]];
scChannels = ConstructiveDiagrams`Private`residueSimpleInputs[{scExternal, 0},
  FactorizationAmplitudeChannels[scProcess, scModel], scModel]["Targets"];
Do[
  scRules = {scMass -> scMassValue, scCharge -> 2/7}; scRegular = False;
  Do[
    scChart = ConstructiveDiagrams`Private`amplitudeRationalComparisonChart[
      scChannels, scSample + 7 scRetry, 20261106 + scSample + scRetry, scRules];
    scValues = Quiet[ConstructiveDiagrams`Private`residueEvaluateChart[#, scChart, scRules] & /@
      Join[scCandidates, scReferences]];
    If[!AllTrue[scValues, ConstructiveDiagrams`Private`residueFiniteQ], Continue[]];
    scRegular = True; Break[], {scRetry, 0, 4}];
  scCheck["both chiralities at generic mass " <> ToString[scMassValue] <> ", chart " <> ToString[scSample],
    scRegular && TrueQ[scChart["OffCutKinematicValidation"]["Valid"]] &&
      AllTrue[Table[Cancel[Together[scValues[[kind]] - scValues[[kind + 2]]]], {kind, 2}], # === 0 &]],
  {scMassValue, {1, 3/2}}, {scSample, 1, 3}];

scPermutation = AssociationThread[Range[7], {4, 2, 7, 3, 6, 1, 5}];
scPermuted = ConstructiveDiagrams`Private`constructSameChiralSpuriousSubsetCurrent[
  Lookup[scPermutation, {1, 2}], Lookup[scPermutation, scVectors], 1, scMass,
  ConstantArray[-Sqrt[2] scCharge, 5]]["Expression"];
scCheck["typed external labels rather than special particle numbers",
  Expand[ConstructiveDiagrams`Private`canonicalChains[
    ConstructiveDiagrams`Private`residueCanonicalPropagators[
      scPermuted - ConstructiveDiagrams`Private`relabelLegs[First[scCandidates], scPermutation], Range[7]]]] === 0];
scGraphs = CreateDiagrams[scProcess, scDiagramModel];
scProposal = ConstructiveDiagrams`Private`constructVectorLineForm[
  scGraphs, scProcess, scDiagramModel, scExternal, "AllSpurious"];
scCheck["the certified full ordered inventory admits the new current",
  AssociationQ[scProposal] && scProposal["Method"] === "SameChiralSpuriousSubsetCurrent"];
scCheck["missing ordered diagrams reject current replacement",
  MissingQ[ConstructiveDiagrams`Private`constructVectorLineForm[
    Most[scGraphs], scProcess, scDiagramModel, scExternal, "AllSpurious"]]];
scCheck["duplicated orderings cannot replace a missing ordered diagram",
  MissingQ[ConstructiveDiagrams`Private`constructVectorLineForm[
    Append[Most[scGraphs], First[scGraphs]], scProcess, scDiagramModel, scExternal, "AllSpurious"]]];
scCheck["the scalar subset bound rejects eight external vectors before expansion",
  MissingQ[ConstructiveDiagrams`Private`constructSameChiralSpuriousSubsetCurrent[
    {1, 2}, Range[3, 10], 1, scMass, ConstantArray[-Sqrt[2] scCharge, 8]]]];
(* The readable QED example must follow its supplied model rather than
   hard-coded values or globals, without certifying its own prediction. *)
Get[FileNameJoin[{scRoot, "tests", "QED-5-point-residues.wl"}]];
scExample = QEDAllPositiveSpuriousExample[scDiagramModel, 3];
scCheck["the formula example retains external mass declarations",
  MatchQ[scExample, {_List, _}] &&
    First[scExample] === Thread[Mass /@ Range[5] -> {scMass, scMass, 0, 0, 0}]];
scCheck["the formula example preserves the known three-photon score",
  SpinorExpressionComplexity[Last[scExample]] === 30];
scScopedParameters = (# -> ModelParameter[ModelParameterName[#], "SpuriousFormulaTest", #[[3]]]) & /@
  DeleteDuplicates[Cases[scDiagramModel, _ModelParameter, Infinity]];
scCheck["formula examples preserve distinct model parameter scopes",
  QEDAllPositiveSpuriousExample[scDiagramModel /. scScopedParameters, 3] ===
    (scExample /. scScopedParameters)];
scCheck["unrelated globals do not affect the example prediction",
  Block[{Global`Me = "other mass", Global`EE = "other charge"},
    QEDAllPositiveSpuriousExample[scDiagramModel, 3]] === scExample];
scScaledModel = scDiagramModel;
scScaledModel[[2, All, -2]] = 2 scScaledModel[[2, All, -2]];
scScaled = QEDAllPositiveSpuriousExample[scScaledModel, 3];
scCheck["the cubic coefficient is obtained from the model at every emission",
  First[scScaled] === First[scExample] && Expand[Last[scScaled] - 8 Last[scExample]] === 0];
scCheck["unsupported examples fail instead of returning a purported amplitude",
  AllTrue[{QEDAllPositiveSpuriousExample[scDiagramModel, 1],
    QEDAllPositiveSpuriousExample[scDiagramModel, 8],
    QEDAllPositiveSpuriousExample[scDiagramModel, "six"],
    QEDAllPositiveSpuriousExample[{{}, {}, {}, {}}, 3]}, FailureQ]];
Print["SPURIOUS CHIRAL CURRENT CHECKS: ", scChecks, "; FAILURES: ", Length[scFailures]];
If[scFailures === {}, Print["HEPCAT_TEST_COMPLETED:spurious-chiral-current-tests.wl"], Print[scFailures]];
Quit[If[scFailures === {}, 0, 1]];
