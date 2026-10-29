(* Exact checks of the diagram-local QED5 routes and their physical
   continuations. A local on-shell numerator identity is not represented as
   an off-shell identity of the full raw diagram sum. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
SetOptions[CheckPhysicalResidues, "HighEnergyCheck" -> False, "ReferenceCheck" -> False];
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
  PropDen[Mom[Multiparticle[#, 5]], prElectronMass] & /@ Range[4]];
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
    {prElectronMass -> 1, prElectricCharge -> 1}] /. chart["PoleVariable"] -> value];
qftAuto = ConstructiveDiagrams`Private`residueAutomaticChart[
  First[qftTargets], 2, 9839, {prElectronMass -> 1, prElectricCharge -> 1}];
qftChart = qftAuto["Chart"];
Do[
  qftLocal = qftRoute["Routes"][[qftIndex]];
  check["the actual numerator reduces to its Dirac form off cut " <> ToString[qftIndex],
    qftMap[qftLocal["Raw"] - qftLocal["DiracReduced"], qftChart, 1] === 0];
  check["the off-cut remainder is retained explicitly " <> ToString[qftIndex],
    qftMap[qftLocal["DiracReduced"] - qftLocal["NumeratorRepresentative"] -
      qftLocal["OffCutRemainder"], qftChart, 1] === 0];
  qftCut = Sort[List @@ qftLocal["ElectronPropagator"][[1, 1]]];
  qftTarget = SelectFirst[qftTargets, #["Cut"] === qftCut && #["Mass"] === prElectronMass &];
  qftCutChart = ConstructiveDiagrams`Private`residueAutomaticChart[
    qftTarget, 2, 5817, {prElectronMass -> 1, prElectricCharge -> 1}]["Chart"];
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
    "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1}, "Output" -> "Details"];
  check["all eight physical residues agree " <> ToString[qftCase[[1]], InputForm],
    Length[qftChannels] === 8 && qftReport["Summary"] === <|"PASS" -> 8|>],
  {qftCase, qftCases}];
qftMassReport = CheckPhysicalResidues[qftRoute["Final"],
  FactorizationAmplitudeChannels[qftParts, qftFactorizationModel], qftFactorizationModel,
  "Samples" -> 2, "RandomSeed" -> 9812,
  "ParameterRules" -> {prElectronMass -> 3/7, prElectricCharge -> 2}, "Output" -> "Summary"];
check["the radiation residue agreement is not confined to unit mass or coupling",
  qftMassReport === <|"PASS" -> 8|>];
qftSpuriousReport = CheckPhysicalResidues[qftRoute["Final"],
  FactorizationAmplitudeChannels[qftParts, qftFactorizationModel], qftFactorizationModel,
  "Samples" -> 2, "RandomSeed" -> 137,
  "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1}, "Output" -> "Details",
  "SpuriousPoles" -> {SpinorChain[Spinor["Xi", "Angle", 10],
    Spinor["Helicity", "Angle", 5]]}];
check["the original photon reference bracket has a finite limit",
  qftSpuriousReport["SpuriousSummary"] === <|"PASS" -> 1|> &&
    AllTrue[qftSpuriousReport["SpuriousChecks"], #["PoleOrder"] === 0 &]];

(* Exact five-point chart for a radiating fermion line and a massless
   exchanged line. Evaluate numerators after stripping both original poles. *)
qftPreparationOwnChart[emitter_Integer, partner_Integer, spectators : {_Integer, _Integer},
    photon_Integer, mass_, sample_Integer, spins_List] := Module[
  {j = {{0, -1}, {1, 0}}, eps, adjugate, perpendicular, r, lq, tq, q,
   lk, tk, k, a, b, c, d, u, v, z, w, pc, pd, massive, momenta,
   angles, squares, angleBasis, squareBasis, checks},
  eps = -j;
  adjugate[p_] := {{p[[2, 2]], -p[[1, 2]]}, {-p[[2, 1]], p[[1, 1]]}};
  perpendicular[x_] := {-x[[2]], x[[1]]};
  r = {{sample + 1, sample - 2},
    {sample + 2, (mass^2 + (sample - 2) (sample + 2))/(sample + 1)}};
  lq = {sample + 3, 2 sample + 1};
  tq = (sample + 2) perpendicular[adjugate[r].lq];
  q = Outer[Times, lq, tq];
  lk = {2, 2 sample + 3};
  tk = (sample - 5) perpendicular[adjugate[r].lk];
  k = Outer[Times, lk, tk];
  {{a, b}, {c, d}} = q;
  u = sample + 4; v = sample - 1;
  z = (b mass^2 + v u d - v^2 c)/(b u - v a);
  w = (z a + u d - v c)/b;
  pc = {{u, v}, {w, z}}; pd = q - pc;
  massive = Sort[Join[{emitter, partner}, spectators]];
  momenta = AssociationThread[{emitter, partner, spectators[[1]], spectators[[2]], photon},
    {r - k, -r - q, pc, pd, k}];
  angles = AssociationThread[massive, UnitVector[2, #] & /@ spins];
  squares = AssociationMap[eps.adjugate[momenta[#]].angles[#]/mass &, massive];
  angleBasis = AssociationMap[IdentityMatrix[2] &, massive];
  squareBasis = AssociationMap[eps.adjugate[momenta[#]]/mass &, massive];
  AssociateTo[angles, photon -> lk]; AssociateTo[squares, photon -> tk];
  checks = {
    (Det /@ Lookup[momenta, massive]) === ConstantArray[mass^2, 4],
    Det[k] === 0, Total[Values[momenta]] === ConstantArray[0, {2, 2}],
    Det[momenta[emitter] + momenta[photon]] === mass^2,
    Det[momenta[spectators[[1]]] + momenta[spectators[[2]]]] === 0,
    And @@ ((!TrueQ[Det[momenta[#] + k] - mass^2 == 0]) & /@ Complement[massive, {emitter}])};
  <|"Momenta" -> momenta, "Angle" -> angles, "Square" -> squares,
    "AngleBasis" -> angleBasis, "SquareBasis" -> squareBasis,
    "PoleVariable" -> qftPreparationUnusedPole, "ReferenceSeed" -> 8181,
    "ReferenceOverrides" -> <|"Angle" -> <|0 -> {17, 29}|>, "Square" -> <|0 -> {31, 43}|>|>,
    "MassValue" -> mass, "ChartChecks" -> checks|>
];


(* General preparation starts with the actual eight diagrams in either photon
   helicity. No curated final expression is substituted for a diagram. *)
qftPreparationExternal = Thread[Mass /@ Range[5] ->
  {prElectronMass, prElectronMass, prElectronMass, prElectronMass, 0}];
qftPreparationGeometry[diagram_] := Module[{photon, vertex, emitter, lines, radiating},
  photon = First[First[Position[diagram["ExternalParticles"], "A.+" | "A.-"]]];
  vertex = First[AdjacencyList[diagram["Graph"], photon]];
  emitter = First[Cases[diagram["IncidentLegData"][vertex],
    leg_Association /; leg["Kind"] === "External" && leg["Index"] =!= photon :> leg["Index"]]];
  lines = diagramFermionLines[diagram];
  radiating = SelectFirst[lines, MemberQ[#, emitter] &];
  <|"Emitter" -> emitter, "Partner" -> First[Complement[radiating, {emitter}]],
    "Spectators" -> SelectFirst[lines, !MemberQ[#, emitter] &], "Photon" -> photon|>
];
qftPreparationOwnValue[expression_, chart_] := Together[
  ConstructiveDiagrams`Private`residueEvaluateChart[expression, chart,
    {prElectronMass -> chart["MassValue"], prElectricCharge -> 2}]];
qftPreparationOwnChecks[assigned_, prepared_, poles_List, geometry_, massSamples_List] := Module[
  {originalNumerator, preparedNumerator, chart, before, after},
  originalNumerator = Cancel[Together[assigned (Times @@ poles)]];
  preparedNumerator = Cancel[Together[prepared (Times @@ poles)]];
  Flatten[Table[
    chart = qftPreparationOwnChart[geometry["Emitter"], geometry["Partner"],
      geometry["Spectators"], geometry["Photon"], massSample[[1]], massSample[[2]], spins];
    before = qftPreparationOwnValue[originalNumerator, chart];
    after = qftPreparationOwnValue[preparedNumerator, chart];
    And @@ chart["ChartChecks"] &&
      FreeQ[{before, after}, _Missing | Indeterminate | ComplexInfinity | DirectedInfinity[_]] &&
      Together[before - after] === 0,
    {massSample, massSamples}, {spins, Tuples[{1, 2}, 4]}]]
];
qftPreparationFixtures = Table[Module[{particles, diagrams, pairs},
  particles = ReplacePart[qftParts, 5 -> helicity];
  diagrams = If[helicity === "A.+", qftDiagrams, CreateDiagrams[particles, qftDiagramModel]];
  pairs = If[helicity === "A.+", qftRoute["ActualDiagramAmplitudes"], diagramAmplitudes[diagrams]];
  <|"Helicity" -> helicity, "Particles" -> particles, "Diagrams" -> diagrams,
    "Pairs" -> pairs, "Hash" -> Hash[pairs, "SHA256"],
    "Geometry" -> (qftPreparationGeometry /@ diagrams)|>],
  {helicity, {"A.+", "A.-"}}];
check["the opposite-helicity preparation fixture also contains all eight actual diagrams",
  Length[qftPreparationFixtures[[2]]["Diagrams"]] === 8 &&
    Length[qftPreparationFixtures[[2]]["Pairs"]] === 8];
qftPreparationModes = {"Xi", "AllPhysical"};
qftPreparationReports = Association[];
Do[
  qftPreparedReport = TimeConstrained[PrepareAmplitudes[qftFixture["Pairs"], qftDiagramModel,
    "PolePreference" -> qftMode, "MaxSteps" -> 0, "TimeConstraint" -> 15,
    "ReferenceTimeConstraint" -> 10, "ProgressReporting" -> False, "ReturnDetails" -> True],
    120, $TimedOut];
  AssociateTo[qftPreparationReports, {qftFixture["Helicity"], qftMode} -> qftPreparedReport];
  check[{"public four-fermion preparation completes every actual diagram", qftFixture["Helicity"], qftMode},
    AssociationQ[qftPreparedReport] && qftPreparedReport["Status"] === "PREPARED" &&
      Length[qftPreparedReport["DiagramReports"]] === 8];
  Do[
    qftOriginalPair = qftFixture["Pairs"][[qftPreparationIndex]];
    qftOriginalPoles = DeleteDuplicates[Cases[Last[qftOriginalPair], _PropDen, Infinity]];
    qftPreparedRow = If[AssociationQ[qftPreparedReport],
      qftPreparedReport["DiagramReports"][[qftPreparationIndex]], <||>];
    qftPreparedExpression = Lookup[qftPreparedRow, "PreparedExpression", $Failed];
    qftExpectedXiKind = If[qftFixture["Helicity"] === "A.+", "Angle", "Square"];
    check[{"automatic four-fermion preparation reaches the compact quotient and protects original poles",
        qftFixture["Helicity"], qftMode, qftPreparationIndex},
      qftPreparedExpression =!= $Failed && qftPreparedRow["Status"] === "PREPARED" &&
        FreeQ[qftPreparedExpression, _Mass | _xFactor | _xTildeFactor |
          _SpinorChainReduced | _SpinorTraceReduced] &&
        SpinorExpressionComplexity[qftPreparedExpression] <= If[qftMode === "Xi", 22, 38] &&
        ConstructiveDiagrams`Private`expressionTermCounts[qftPreparedExpression]["Terms"] <= 6 &&
        ContainsAll[DeleteDuplicates[Cases[qftPreparedExpression, _PropDen, Infinity]], qftOriginalPoles] &&
        If[qftMode === "AllPhysical", FreeQ[qftPreparedExpression, Spinor["Xi", ___]],
          AllTrue[Cases[qftPreparedExpression, Spinor["Xi", kind_, id_] :> {kind, id}, Infinity],
            # === {qftExpectedXiKind, 0} &]]];
    qftOwnChecks = If[AssociationQ[qftPreparedReport],
      qftPreparationOwnChecks[qftPreparedRow["PolePreference"]["Expression"], qftPreparedExpression,
        qftOriginalPoles, qftFixture["Geometry"][[qftPreparationIndex]], {{1, 1}, {3/7, 2}}], {False}];
    check[{"same-reference prepared numerators agree on both own cuts for all sixteen fermion spin components",
        qftFixture["Helicity"], qftMode, qftPreparationIndex},
      Length[qftOwnChecks] === 32 && And @@ qftOwnChecks],
    {qftPreparationIndex, 8}];
  (* Move the photon as well as the massive endpoints. These are typed
     identity tests; no sign convention is inferred from a new process. *)
  If[AssociationQ[qftPreparedReport],
    qftLabelCycle = {1, 5, 2};
    qftLabels = AssociationThread[qftLabelCycle, RotateLeft[qftLabelCycle]];
    qftMapLabel[index_Integer] := Lookup[qftLabels, index, index];
    qftMapLabel[indices_List] := qftMapLabel /@ indices;
    qftTypedPair = PermuteParticles[{First[qftFixture["Pairs"][[1]]],
      qftPreparedReport["DiagramReports"][[1]]["PolePreference"]["Expression"]}, qftLabelCycle];
    qftTypedAuxiliary = PermuteParticles[Lookup[
      qftPreparedReport["DiagramReports"][[1]]["PolePreference"], "AuxiliaryPhysicalPropagators", {}], qftLabelCycle];
    qftTypedRules = ConstructiveDiagrams`Private`preparationCanonicalMassRules[First[qftTypedPair]];
    qftTypedResult = ConstructiveDiagrams`Private`preparationReferenceTrial[
      Last[qftTypedPair], qftTypedRules,
      ConstructiveDiagrams`Private`preparationDotRules[qftTypedRules, True], qftTypedAuxiliary, True];
    qftTypedPoles = PermuteParticles[DeleteDuplicates[Cases[
      Last[qftFixture["Pairs"][[1]]], _PropDen, Infinity]], qftLabelCycle];
    qftTypedChecks = qftPreparationOwnChecks[Last[qftTypedPair], qftTypedResult, qftTypedPoles,
      Map[qftMapLabel, qftFixture["Geometry"][[1]]], {{3/7, 3}}],
    qftTypedChecks = {False}];
  check[{"four-fermion preparation is covariant under moving the photon and fermion labels",
      qftFixture["Helicity"], qftMode},
    Length[qftTypedChecks] === 16 && And @@ qftTypedChecks],
  {qftFixture, qftPreparationFixtures}, {qftMode, qftPreparationModes}];
Do[check[{"preparation leaves the retained raw four-fermion pairs unchanged", qftFixture["Helicity"]},
  Hash[qftFixture["Pairs"], "SHA256"] === qftFixture["Hash"]],
  {qftFixture, qftPreparationFixtures}];

(* The massless bridge is licensed by a declared external zero mass, not by
   an arbitrary inserted momentum or an internal massless cut. *)
qftNoWitness[kind_] := SpinorChain[Spinor["Spin", kind, 1], Mom[5],
  Spinor["Xi", If[kind === "Angle", "Square", "Angle"], 77]];
qftMasslessTrial[value_, rules_] := ConstructiveDiagrams`Private`preparationMasslessMomentumTrial[
  value, rules, {}, {}];
Do[
  qftNoWitnessInput = qftNoWitness[qftKind];
  qftNoWitnessOutput = qftMasslessTrial[qftNoWitnessInput, qftPreparationExternal];
  check[{"a declared external massless momentum factors without a separate helicity witness", qftKind},
    SpinorExpressionComplexity[qftNoWitnessInput] === 5 &&
      SpinorExpressionComplexity[qftNoWitnessOutput] === 1 && FreeQ[qftNoWitnessOutput, _Mom] &&
      qftMap[qftNoWitnessInput - qftNoWitnessOutput, qftChart, 1] === 0],
  {qftKind, {"Angle", "Square"}}];
check["an unknown or nonzero external mass never licenses rank-one factorization",
  And @@ Flatten[Table[qftMasslessTrial[qftNoWitness[kind],
      qftPreparationExternal /. HoldPattern[Mass[5] -> 0] -> (Mass[5] -> mass)] === qftNoWitness[kind],
    {kind, {"Angle", "Square"}}, {mass, {qftUnknownExternalMass, prElectronMass}}]]];
qftCompositeNoWitness = SpinorChain[Spinor["Spin", "Angle", 1], Mom[Multiparticle[2, 3]],
  Spinor["Xi", "Square", 77]];
check["an internal massless declaration does not license an external rank-one spinor",
  qftMasslessTrial[qftCompositeNoWitness,
    Append[qftPreparationExternal, Mass[Multiparticle[2, 3]] -> 0]] === qftCompositeNoWitness];
qftNoWitnessInput = qftNoWitness["Angle"];
qftMasslessOpaque = PropDen[qftOpaqueArgument[qftNoWitnessInput, MomProd[2, 5]], qftOpaqueMass];
qftMasslessScoped = qftMasslessTrial[(1 + qftNoWitnessInput)/
  (qftMasslessOpaque (3 + qftNoWitnessInput)), qftPreparationExternal];
qftMasslessOrdinaryInverse = ReduceSpinorProducts[1/(3 + qftNoWitnessInput)];
check["the massless bridge preserves original propagators and inverse kinematics after ordinary normalization",
  DeleteDuplicates[Cases[qftMasslessScoped, _PropDen, Infinity]] === {qftMasslessOpaque} &&
    !FreeQ[qftMasslessScoped, qftMasslessOrdinaryInverse] &&
    Count[qftMasslessOrdinaryInverse, Mom[5], Infinity] === 1 &&
    FreeQ[qftMasslessOrdinaryInverse, Spinor["Helicity", _, 5]] &&
    qftMasslessTrial[1/(3 + qftNoWitnessInput), qftPreparationExternal] === 1/(3 + qftNoWitnessInput)];

(* The compact complete amplitude remains a separate off-cut representative. *)
check["the known complete four-fermion amplitude has thirty-two terms and complexity one hundred sixty",
  SpinorExpressionComplexity[qftRoute["Final"]] === 160 &&
    ConstructiveDiagrams`Private`expressionTermCounts[qftRoute["Final"]]["Terms"] === 32];
qftCompletePreparation = PrepareAmplitudes[{{qftPreparationExternal, qftRoute["Final"]}},
  qftDiagramModel, "PolePreference" -> None, "InternalOnShell" -> False,
  "MaxSteps" -> 0, "TimeConstraint" -> 15, "ProgressReporting" -> False, "ReturnDetails" -> True];
check["ordinary preparation preserves the best known complete amplitude without assuming its internal cuts",
  qftCompletePreparation["Status"] === "PREPARED" &&
    SpinorExpressionComplexity[qftCompletePreparation["Amplitude"]] <= 160 &&
    qftMap[qftCompletePreparation["Amplitude"] - qftRoute["Final"], qftChart, 1] === 0 &&
    First[qftCompletePreparation["DiagramReports"]]["InternalOnShellChannels"] === {}];


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
  "ParameterRules" -> {prElectronMass -> 2, prElectricCharge -> 1}, "Output" -> "Summary"];
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
check["the optional two-block regrouping follows from ordinary external identities",
  AllTrue[Values[qftMixedProof["FinalRegroupingIdentities"]], TrueQ]];

Print[<|"Checks" -> checks, "Failed" -> Length[failures], "Failures" -> failures,
  "Scope" -> "Exact local identities, symbolic reference cancellation and sampled physical residues"|>];
Print["HEPCAT_TEST_COMPLETED:qed-five-point-simplification-tests.wl"];
Quit[If[failures === {}, 0, 1]];
