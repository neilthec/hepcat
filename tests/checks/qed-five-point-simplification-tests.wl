(* Exact checks of the diagram-local QED5 routes and their physical
   continuations. A local on-shell numerator identity is not represented as
   an off-shell identity of the full raw diagram sum. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
Get[FileNameJoin[{root, "tests", "QED-5-point-residues.wl"}]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name];
  Print["FAIL: ", name]]);
qftModelDirectory = FileNameJoin[{root, "models"}];
qftDiagramModel = ReadModel[qftModelDirectory, 2];
qftFactorizationModel = ReadModel[qftModelDirectory, 3];
qftParity[expression_] := expression /.
  Spinor[type_, kind : ("Angle" | "Square"), rest___] :>
    Spinor[type, If[kind === "Angle", "Square", "Angle"], rest];
qftNoAuxiliaryDenominatorsQ[expression_] := AllTrue[
  Cases[expression, Power[base_, exponent_Integer?Negative] :> base, Infinity],
  FreeQ[#, _SpinorChain | _SpinorTrace] &];

(* The reference-elimination step is a polynomial Schouten identity. These
   matrices and spinors are independent, with no internal shell equations. *)
qftJ = {{0, -1}, {1, 0}}; qftL = -qftJ;
qftMatrixA = {{qftA11, qftA12}, {qftA21, qftA22}};
qftMatrixB = {{qftB11, qftB12}, {qftB21, qftB22}};
qftXi = {qftX1, qftX2}; qftLambda = {qftZ1, qftZ2};
qftSquare = {qftW1, qftW2};
qftAdj[m_] := {{m[[2, 2]], -m[[1, 2]]}, {-m[[2, 1]], m[[1, 1]]}};
qftRA = qftXi.qftJ.qftMatrixA.qftL.qftSquare;
qftRB = qftXi.qftJ.qftMatrixB.qftL.qftSquare;
qftTA = qftLambda.qftJ.qftMatrixA.qftL.qftSquare;
qftTB = qftLambda.qftJ.qftMatrixB.qftL.qftSquare;
qftReference = qftXi.qftJ.qftLambda;
qftK = qftSquare.qftAdj[qftMatrixA].qftMatrixB.qftL.qftSquare;
check["the eikonal reference cancels by Schouten without any shell assumptions",
  Expand[qftRA qftTB - qftRB qftTA + qftReference qftK] === 0];

(* Four-fermion radiation. All candidate amplitudes below start with actual
   CreateDiagrams output, not a saved answer or a fitted correction table. *)
qftParts = {"e", "E", "E", "e", "A.+"};
qftDiagrams = CreateDiagrams[qftParts, qftDiagramModel];
qftRoute = q5FourFermionPhotonRoute[qftDiagrams];
check["the radiation route rejects a duplicated diagram instead of missing an attachment",
  FailureQ[q5FourFermionPhotonRoute[ReplacePart[qftDiagrams,
    -1 -> First[qftDiagrams]]]]];
check["the radiation route accepts all eight generated diagrams",
  Length[qftDiagrams] === 8 && AssociationQ[qftRoute] &&
    Length[qftRoute["Routes"]] === 8];
check["the original local expressions remain inspectable",
  And @@ (KeyExistsQ[#, "Raw"] && KeyExistsQ[#, "DiracReduced"] &&
    KeyExistsQ[#, "NumeratorRepresentative"] &&
    KeyExistsQ[#, "RemovedNumeratorMultiple"] &&
    KeyExistsQ[#, "OffCutRemainder"] && KeyExistsQ[#, "ElectronPropagator"] &&
    KeyExistsQ[#, "PhotonPropagator"] & /@ qftRoute["Routes"])];
check["the radiating-current result has no reference spinors or x factors",
  FreeQ[qftRoute["Final"], Spinor["Xi", ___] | _xFactor | _xTildeFactor]];
check["the radiating-current result has no auxiliary spinor denominators",
  qftNoAuxiliaryDenominatorsQ[qftRoute["Final"]]];
qftPhysicalPropagators = Join[
  PropDen[Mom[Multiparticle @@ #], 0] & /@ {{1, 2}, {1, 3}, {2, 4}, {3, 4}},
  PropDen[Mom[Multiparticle[#, 5]], Me] & /@ Range[4]];
check["the final radiation amplitude contains only its eight physical poles",
  Sort[DeleteDuplicates[Cases[qftRoute["Final"], _PropDen, Infinity]]] ===
    Sort[qftPhysicalPropagators]];
check["the two emission attachments form each of four neutral radiating currents",
  Length[qftRoute["PairedCurrents"]] === 4 &&
    AllTrue[qftRoute["PairedCurrents"], Length[#["DiagramIndices"]] === 2 &]];
check["the discarded numerator is explicitly a multiple of its own electron cut",
  AllTrue[qftRoute["Routes"], Expand[#["RemovedNumeratorMultiple"] -
    #["ElectronPropagator"] #["RemovedNumeratorCoefficient"]] === 0 &] &&
    TrueQ[qftRoute["DenominatorsKeptOffShell"]]];

qftChannels = FactorizationAmplitudeChannels[qftParts, qftFactorizationModel];
qftTargets = ConstructiveDiagrams`Private`residueSimpleInputs[
  0, qftChannels, qftFactorizationModel]["Targets"];
qftMap[expression_, chart_, value_] := Together[
  ConstructiveDiagrams`Private`residueEvaluateChart[expression, chart,
    {Me -> 1, EE -> 1}] /. chart["PoleVariable"] -> value];
qftAuto = ConstructiveDiagrams`Private`residueAutomaticChart[
  First[qftTargets], 2, 9839, {Me -> 1, EE -> 1}];
qftChart = qftAuto["Chart"];
Do[
  qftLocal = qftRoute["Routes"][[qftIndex]];
  check["the actual numerator reduces to its Dirac form off cut " <> ToString[qftIndex],
    qftMap[qftLocal["Raw"] - qftLocal["DiracReduced"], qftChart, 1] === 0];
  check["the off-cut remainder is retained explicitly " <> ToString[qftIndex],
    qftMap[qftLocal["DiracReduced"] - qftLocal["NumeratorRepresentative"] -
      qftLocal["OffCutRemainder"], qftChart, 1] === 0];
  qftCut = Sort[List @@ qftLocal["ElectronPropagator"][[1, 1]]];
  qftTarget = SelectFirst[qftTargets, #["Cut"] === qftCut && #["Mass"] === Me &];
  qftCutChart = ConstructiveDiagrams`Private`residueAutomaticChart[
    qftTarget, 2, 5817, {Me -> 1, EE -> 1}]["Chart"];
  check["the representative change vanishes on that diagram's electron cut " <>
      ToString[qftIndex],
    qftMap[qftLocal["UnreducedNumerator"] - qftLocal["RetainedNumerator"],
      qftCutChart, 0] === 0],
  {qftIndex, Length[qftRoute["Routes"]]}];
Do[
  qftReferenceChart = Join[qftChart, <|"ReferenceOverrides" ->
    <|"Angle" -> <|0 -> qftReferenceVector|>|>|>];
  check["the paired reference form equals the final form off cut for Xi " <>
      ToString[qftReferenceVector, InputForm],
    qftMap[qftRoute["ReducedSum"] - qftRoute["Final"], qftReferenceChart, 1] === 0],
  {qftReferenceVector, {{1, 2}, {5, 11}, {-7, 3}}}];

qftCases = {
  {qftParts, qftRoute["Final"]},
  {{"e", "E", "e", "E", "A.+"}, q5FourFermionPhotonRoute[
      CreateDiagrams[{"e", "E", "e", "E", "A.+"}, qftDiagramModel]]["Final"]},
  {{"E", "e", "e", "E", "A.+"}, q5FourFermionPhotonRoute[
      CreateDiagrams[{"E", "e", "e", "E", "A.+"}, qftDiagramModel]]["Final"]},
  {ReplacePart[qftParts, 5 -> "A.-"], qftParity[qftRoute["Final"]]}};
Do[
  qftChannels = FactorizationAmplitudeChannels[qftCase[[1]], qftFactorizationModel];
  qftReport = CheckPhysicalResidues[qftCase[[2]], qftChannels, qftFactorizationModel,
    "Samples" -> 2, "RandomSeed" -> 8091,
    "ParameterRules" -> {Me -> 1, EE -> 1}, "Output" -> "Details"];
  check["all eight physical residues agree " <> ToString[qftCase[[1]], InputForm],
    Length[qftChannels] === 8 && qftReport["Summary"] === <|"PASS" -> 8|>],
  {qftCase, qftCases}];
qftMassReport = CheckPhysicalResidues[qftRoute["Final"],
  FactorizationAmplitudeChannels[qftParts, qftFactorizationModel], qftFactorizationModel,
  "Samples" -> 2, "RandomSeed" -> 9812,
  "ParameterRules" -> {Me -> 3/7, EE -> 2}, "Output" -> "Summary"];
check["the radiation residue agreement is not confined to unit mass or coupling",
  qftMassReport === <|"PASS" -> 8|>];
qftSpuriousReport = CheckPhysicalResidues[qftRoute["Final"],
  FactorizationAmplitudeChannels[qftParts, qftFactorizationModel], qftFactorizationModel,
  "Samples" -> 2, "RandomSeed" -> 137,
  "ParameterRules" -> {Me -> 1, EE -> 1}, "Output" -> "Details",
  "SpuriousPoles" -> {SpinorChain[Spinor["Xi", "Angle", 10],
    Spinor["Helicity", "Angle", 5]]}];
check["the original photon reference bracket has a finite limit",
  qftSpuriousReport["SpuriousSummary"] === <|"PASS" -> 1|> &&
    AllTrue[qftSpuriousReport["SpuriousChecks"], #["PoleOrder"] === 0 &]];

(* Mixed photon helicities: compare all six actual local numerators on
   their own two internal shells, then prove the global equality without
   using any internal shell conditions. *)
qftMixedParts = {"e", "E", "A.+", "A.+", "A.-"};
qftMixedPairs = diagramAmplitudes[CreateDiagrams[qftMixedParts, qftDiagramModel]];
qftMixedRoute = qedFivePointMixedRoute[qftMixedPairs];
check["the mixed route rejects an empty input",
  FailureQ[qedFivePointMixedRoute[{}]]];
check["the mixed route rejects the all-positive photon sector",
  FailureQ[qedFivePointMixedRoute[diagramAmplitudes[CreateDiagrams[
    {"e", "E", "A.+", "A.+", "A.+"}, qftDiagramModel]]]]];
check["the mixed route rejects duplicate orderings instead of omitting a diagram",
  FailureQ[qedFivePointMixedRoute[ReplacePart[qftMixedPairs,
    -1 -> First[qftMixedPairs]]]]];
check["the mixed route rejects inconsistent external mass metadata",
  FailureQ[qedFivePointMixedRoute[ReplacePart[qftMixedPairs,
    {1, 1, 1} -> (Mass[1] -> 0)]]]];
check["the worked mixed route does not replace altered numerators with a known answer",
  FailureQ[qedFivePointMixedRoute[ReplacePart[qftMixedPairs,
    {1, 2} -> (qftMixedPairs[[1, 2]] + 1)]]]];
check["the mixed route retains its original six diagram amplitudes",
  qftMixedRoute["InputDiagrams"] === qftMixedPairs &&
    Length[qftMixedRoute["RawDiagrams"]] === 6 &&
    Sort[qftMixedRoute["PhotonOrders"]] === Sort[Permutations[{3, 4, 5}]]];
check["each mixed numerator completion uses a shell of that same diagram",
  AllTrue[qftMixedRoute["OnShellFactorUsed"], Length[#] >= 1 &]];
qftMixedLocal = qedFivePointMixedLocalChecks[qftMixedRoute];
check["all 96 mixed local numerators match both shell representatives",
  qftMixedLocal["ExactComparisons"] === 96 && TrueQ[qftMixedLocal["AllPass"]] &&
    AllTrue[qftMixedLocal["Details"],
      #["Shells"] && #["Finite"] && #["SeedEqual"] && #["CompletedEqual"] &]];
check["the mixed final form is reference free with physical denominators only",
  FreeQ[qftMixedRoute["Final"], Spinor["Xi", ___] | _xFactor | _xTildeFactor] &&
    qftNoAuxiliaryDenominatorsQ[qftMixedRoute["Final"]]];
qftMixedReport = CheckPhysicalResidues[qftMixedRoute["Final"],
  FactorizationAmplitudeChannels[qftMixedParts, qftFactorizationModel],
  qftFactorizationModel, "Samples" -> 3, "RandomSeed" -> 5817,
  "ParameterRules" -> {Me -> 2, EE -> 1}, "Output" -> "Summary"];
check["all six mixed-helicity physical residues agree",
  qftMixedReport === <|"PASS" -> 6|>];
qftMixedProof = qedFivePointMixedEqualityChecks[];
check["all mixed local polynomial identities vanish",
  AllTrue[qftMixedProof["LocalIdentities"],
    #["DLeft"] && #["DRight"] && #["PoleRemovalIdentity"] && #["ProductIdentity"] &]];
check["the mixed spurious form equals the three-term form globally",
  qftMixedProof["RemoveDPoles"] && qftMixedProof["UEqualsThreeTermForm"] &&
    qftMixedProof["VEqualsThreeTermForm"] && qftMixedProof["FullEquality"]];
check["the mixed global equality uses no internal shell conditions",
  qftMixedProof["InternalShellConditions"] === {}];

Print[<|"Checks" -> checks, "Failed" -> Length[failures], "Failures" -> failures,
  "Scope" -> "Exact local identities, symbolic reference cancellation and sampled physical residues"|>];
Quit[If[failures === {}, 0, 1]];
