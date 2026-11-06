(* Bounded ordinary reduction of four coherent massive-vector spin tensors.
   The fixture stores original inputs only; no compact process answer is used. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root, "source", "HEPCAT.wl"}]];
$HistoryLength = 0; checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, assertion_] := (checks++;
  If[!TrueQ[assertion], AppendTo[failures, name]; Print["FAIL: ", name]]);
ftReduce = ConstructiveDiagrams`Private`rspFourLegTensorReduction;
ftModel = ReadModel[FileNameJoin[{root, "models"}], 1];
ftInputs = Get[FileNameJoin[{root, "tests", "checks", "fixtures",
  "massive-four-vector-inputs.wl"}]][ModelParameter[ftModel, #] &];
ftParameters = ModelParameterRules[ftModel,
  {"MW" -> 20, "SW" -> 3/5, "EE" -> 1/3, "Mh" -> 30}];
ftInverses[expression_] := Sort[DeleteDuplicates[Cases[{expression},
  inverse : Power[base_, exponent_] /; !TrueQ[exponent >= 0] &&
    !FreeQ[base, _PropDen | _SpinorChain | _MomProd | _Mom | _Spinor] :> inverse,
  Infinity]]];
ftNoNewPoles[input_, output_] := Module[{original = ftInverses[input], retained},
  retained = ftInverses[output];
  AllTrue[retained, Function[inverse,
    AnyTrue[original, First[#] === First[inverse] &&
      TrueQ[Abs[Last[inverse]] <= Abs[Last[#]]] &]]] &&
    AllTrue[DeleteDuplicates[Cases[{output}, _PropDen, Infinity]],
      MemberQ[DeleteDuplicates[Cases[{input}, _PropDen, Infinity]], #] &]
];

(* Recompute the exact algebraic certificates, without repeating the much
   slower symbolic proof of every row already used during development. *)
ftReplay[report_] := Module[{route, groups, sectors, mass, restore, sourceChecks,
    linearChecks, numeratorChecks, sectorValues},
  If[!AssociationQ[report] || report["Route"] === {}, Return[False]];
  AllTrue[report["Route"], Function[record,
    route = record; groups = route["GroupCertificates"]; mass = route["MassScale"];
    restore[value_] := (value /. {chain_SpinorChain :>
      chain/mass^(Length[chain] - 2), product_MomProd :> product/mass^2}) /.
        route["RatioDefinitions"];
    sourceChecks = Map[Function[group,
      Expand[Total[(#["Coefficient"] restore[#["Polynomial"]]) & /@
          group["CoefficientSectors"]] -
        (group["OriginalNumerator"] /. route["ExternalMassRules"])] === 0 &&
      group["ExactMassScalingReconstructionResidual"] === 0], groups];
    sectors = Select[Flatten[Lookup[groups, "SectorCertificates"], 1], AssociationQ];
    linearChecks = Map[Function[sector,
      Expand[sector["Candidate"] - sector["NormalizedSource"] -
        Total[MapThread[Times, {sector["IdentityCoefficients"],
          sector["OrdinaryIdentities"]}]]] === 0 &&
      sector["ExactLinearCombinationResidual"] === 0 &&
      sector["ExactFactorizationResidual"] === 0 &&
      sector["InternalOnShellConditions"] === {}], sectors];
    numeratorChecks = Map[Function[group,
      sectorValues = MapThread[If[AssociationQ[#2], #2["Candidate"], #1["Polynomial"]] &,
        {group["CoefficientSectors"], group["SectorCertificates"]}];
      Expand[Total[MapThread[#1 restore[#2] &,
        {Lookup[group["CoefficientSectors"], "Coefficient"], sectorValues}]] -
          group["NewNumerator"]] === 0], groups];
    AllTrue[sourceChecks, TrueQ] && AllTrue[linearChecks, TrueQ] &&
      AllTrue[numeratorChecks, TrueQ] && TrueQ[route["ExactFormalReplay"]] &&
      route["InternalOnShellConditions"] === {}]]
];
ftMassRoundTrip[report_] := AllTrue[report["Route"], Function[route,
  AllTrue[route["OtherMassDefinitions"],
    Cancel[(Last[#] /. route["RatioDefinitions"]) - First[#]] === 0 &] &&
  FreeQ[report["Amplitude"], Alternatives @@ (First /@ route["RatioDefinitions"])]]];

(* These are two independent conserved exact kinematic points, retaining all
   four coherent spin variables. Literal polynomial-coefficient equality checks
   every massive-spin component; no phase or normalization is fitted. *)
ftChartChecks[input_, output_, externalRules_] := Module[
  {chart, coordinates, spins, actualRules, sampled, left, right, poles, results = {}},
  actualRules = externalRules /. ftParameters;
  chart = ConstructiveDiagrams`Private`completionHESymbolicExternalChart[
    actualRules, Automatic, Automatic];
  If[!AssociationQ[chart], Return[{False, False}]];
  coordinates = chart["ExternalIdentityCoordinates"]; spins = chart["SpinVariables"];
  poles = DeleteDuplicates[Cases[{input, output}, _PropDen, Infinity]];
  Do[
    sampled = chart /. Thread[coordinates -> point];
    left = ConstructiveDiagrams`Private`spuriousEvaluateCurve[input, sampled, ftParameters];
    right = ConstructiveDiagrams`Private`spuriousEvaluateCurve[output, sampled, ftParameters];
    AppendTo[results, Length[spins] === 4 &&
      TrueQ[chart["ExternalMassShellsValidated"]] &&
      TrueQ[chart["MomentumConservationValidated"]] &&
      chart["InternalOnShellConditions"] === {} &&
      AllTrue[poles, With[{value = ConstructiveDiagrams`Private`spuriousEvaluateCurve[
        #, sampled, ftParameters]}, NumberQ[value] && value =!= 0] &] &&
      FreeQ[{left, right}, _Real | Indeterminate | ComplexInfinity | _DirectedInfinity |
        _SpinorChain | _PropDen | _MomProd | _ModelParameter] &&
      PolynomialQ[left, spins] && PolynomialQ[right, spins] &&
      CoefficientRules[Expand[left], spins] === CoefficientRules[Expand[right], spins]],
    {point, {{3, 5, 7}, {5, 7, 11}}}];
  results
];
ftReports = <||>;
Do[
  ftCase = ftInputs[ftName];
  ftReport = ftReduce[ftCase["Amplitude"], ftCase["ExternalMassRules"], True,
    If[ftName === "WWWW", 6, 8], None];
  AssociateTo[ftReports, ftName -> ftReport];
  check[ftName <> ": bounded search finds a strictly smaller input-derived expression",
    ftReport["Status"] === "IMPROVED" &&
      SpinorExpressionComplexity[ftReport["Amplitude"]] <
        SpinorExpressionComplexity[ftCase["Amplitude"]]];
  check[ftName <> ": retained inverse arguments stay unchanged and no new poles appear",
    ftNoNewPoles[ftCase["Amplitude"], ftReport["Amplitude"]]];
  check[ftName <> ": exact source scaling, coefficient reconstruction and factoring replay",
    ftReplay[ftReport]];
  check[ftName <> ": temporary mass ratios are reversible and do not escape",
    ftMassRoundTrip[ftReport]];
  check[ftName <> ": no internal on-shell conditions enter the ordinary reduction",
    ftReport["InternalOnShellConditions"] === {} &&
      AllTrue[ftReport["Route"], #["InternalOnShellConditions"] === {} &]];
  ftChartResults = ftChartChecks[ftCase["Amplitude"], ftReport["Amplitude"],
    ftCase["ExternalMassRules"]];
  Do[check[ftName <> ": literal coherent coefficients agree at exact point " <>
    ToString[ftPoint], ftChartResults[[ftPoint]]], {ftPoint, 2}],
  {ftName, {"WWWW", "ZZWW"}}];

ftA[i_, j_] := SpinorChain[Spinor["Spin", "Angle", i], Spinor["Spin", "Angle", j]];
ftS[i_, j_] := SpinorChain[Spinor["Spin", "Square", i], Spinor["Spin", "Square", j]];
ftRules = Thread[Mass /@ Range[4] -> 2];
ftWidthPole = PropDen[Mom[Multiparticle[1, 3]], ftExchangeMass, ftWidth];
ftToy = (ftA[1, 2] ftA[3, 4] + ftA[1, 4] ftA[2, 3]) ftS[1, 3] ftS[2, 4]/ftWidthPole;
ftWidthReport = ftReduce[ftToy, ftRules, True, 1, None];
check["a finite-width propagator stays opaque while its numerator contracts",
  ftWidthReport["Status"] === "IMPROVED" &&
    ftNoNewPoles[ftToy, ftWidthReport["Amplitude"]] &&
    ftInverses[ftWidthReport["Amplitude"]] === ftInverses[ftToy] &&
    !FreeQ[ftWidthReport["Amplitude"], ftWidthPole] && ftReplay[ftWidthReport]];
ftPreserved[expression_, rules_, conserve_:True, seconds_:1] := Module[{report},
  report = ftReduce[expression, rules, conserve, seconds, None];
  report["Amplitude"] === expression && report["Route"] === {} &&
    report["InternalOnShellConditions"] === {}];
check["five external legs retain the established route",
  ftPreserved[ftToy, Append[ftRules, Mass[5] -> 2]]];
check["six external legs retain the established route",
  ftPreserved[ftToy, Join[ftRules, {Mass[5] -> 2, Mass[6] -> 2}]]];
check["four-fermion tensors with four spinor slots are not vector tensors",
  ftPreserved[ftA[1, 2] ftS[3, 4]/ftWidthPole, ftRules]];
check["disabled momentum conservation keeps the original amplitude",
  ftPreserved[ftToy, ftRules, False]];
check["a massless leg keeps the existing reduction route",
  ftPreserved[ftToy, ReplacePart[ftRules, 4 -> (Mass[4] -> 0)]]];
ftIndexed = ftToy /. Spinor["Spin", type_, leg_] :>
  Spinor["Spin", "Upper", type, leg, ftIndex[leg, Mom[2]]];
check["explicit spin-index metadata are declined without edits",
  ftPreserved[ftIndexed, ftRules]];
check["unprojected color tensors are declined without edits",
  ftPreserved[ColorDelta[ColorIndex[1], ColorIndex[2]] ftToy, ftRules]];
check["a zero time budget retains the exact supplied expression",
  ftPreserved[ftToy, ftRules, True, 0]];



ftComposite = ftA[1, 2] ftA[2, 3] ftS[3, 4] *
  SpinorChain[Spinor["Spin", "Square", 1], Mom[2] + Mom[3],
    Spinor["Spin", "Angle", 4]]/PropDen[Mom[Multiparticle[1, 3]], 3];
ftCompositeReport = ftReduce[ftComposite, ftRules, True, 2, None];
check["bounded ordinary momentum-slot linearity admits composite-vector numerators",
  ftCompositeReport["Status"] === "IMPROVED" &&
    SpinorExpressionComplexity[ftCompositeReport["Amplitude"]] <
      SpinorExpressionComplexity[ftComposite] &&
    AllTrue[ftCompositeReport["Route"], Function[route,
      route["MomentumSlotLinearityRules"] =!= {} &&
        AllTrue[route["MomentumSlotLinearityRules"],
          Last[#] === ConstructiveDiagrams`Private`rspLinearChain[First[#]] &]]] &&
    ftNoNewPoles[ftComposite, ftCompositeReport["Amplitude"]]];
check["composite-slot reduction retains every exact coherent spin coefficient",
  And @@ ftChartChecks[ftComposite, ftCompositeReport["Amplitude"], ftRules]];
ftBroadSlot = 2 Mom[1] + 3 Mom[2] + Mom[3] + Mom[4];
ftBroadInput = Times @@ Table[SpinorChain[Spinor["Spin", "Angle", leg],
  ftBroadSlot, ftBroadSlot, Spinor["Spin", "Angle", Mod[leg, 4] + 1]], {leg, 4}];
ftBroadReport = ftReduce[ftBroadInput, ftRules, True, 1, None];
check["prospective slot expansion is bounded before a large tensor is distributed",
  ftBroadReport["Amplitude"] === ftBroadInput && ftBroadReport["Route"] === {} &&
    ftBroadReport["StopReason"] === "LinearityExpansionLimit"];
check["unsupported huge chain powers return before materializing repeated factors",
  TimeConstrained[ftPreserved[ftA[1, 2]^100000000, ftRules], 0.5, False]];


(* Numeric masses can coincide with leg labels. The optional ratio-based
   route declines this unsupported case rather than rewriting typed labels. *)
ftNumericMassToy = (ftA[1, 2] ftA[3, 4] + ftA[1, 4] ftA[2, 3]) ftS[1, 2] *
  SpinorChain[Spinor["Spin", "Square", 3], Mom[3], Spinor["Spin", "Angle", 4]]/ftWidthPole;
check["unequal numeric masses do not substitute into particle or momentum labels",
  ftPreserved[ftNumericMassToy, Thread[Mass /@ Range[4] -> {2, 3, 2, 3}]]];
check["mixed symbolic and numeric masses do not rewrite a coinciding leg label",
  ftPreserved[ftNumericMassToy,
    Thread[Mass /@ Range[4] -> {ftSymbolicMass, 3, ftSymbolicMass, 3}]]];

(* A complex-width propagator is not the real momentum virtuality. This checks
   the actual preparation route, separately from opaque inverse protection. *)
ftVirtualRules = {Mass[1] -> ftVirtualMass, Mass[2] -> ftOtherMass,
  Mass[3] -> 0, Mass[4] -> ftLastMass};
ftVirtualPole = PropDen[Mom[Multiparticle[1, 3]], ftVirtualMass, ftVirtualWidth];
ftVirtualInput = MomProd[1, 3]/ftVirtualPole;
ftVirtualOutput = ConstructiveDiagrams`Private`preparationExternalVirtualities[
  ftVirtualInput, ftVirtualRules];
check["external virtuality reduction retains the imaginary width remainder",
  Together[ftVirtualOutput -
    (ftVirtualPole - I ftVirtualMass ftVirtualWidth)/(2 ftVirtualPole)] === 0 &&
    DeleteDuplicates[Cases[ftVirtualOutput, _PropDen, Infinity]] === {ftVirtualPole}];
ftZeroWidthOutput = ConstructiveDiagrams`Private`preparationExternalVirtualities[
  ftVirtualInput /. ftVirtualWidth -> 0, ftVirtualRules];
check["zero-width external virtuality reduction keeps its original one-half limit",
  ftZeroWidthOutput === 1/2];


(* Exercise the public complete-amplitude checkpoint, without rebuilding a
   large process or enabling the general identity-route search. *)
ftPublicPrepared = PrepareAmplitudes[{ftRules, ftToy}, ftModel,
  "MaxSteps" -> 0, "InternalOnShell" -> False, "PolePreference" -> None,
  "TimeConstraint" -> 4, "ReturnDetails" -> True,
  "Parallelization" -> False, "ProgressReporting" -> False];
ftPublicItem = If[AssociationQ[ftPublicPrepared] &&
    ListQ[Lookup[ftPublicPrepared, "ItemReports", None]],
  First[ftPublicPrepared["ItemReports"]], <||>];
ftPublicTensor = SelectFirst[Lookup[ftPublicItem, "Reductions", {}],
  Lookup[#, "Operation", None] === "OriginalFourLegTensorIdentities" &,
  Missing["NoPublicTensorCheckpoint"]];
check["public completed preparation keeps the improved original tensor checkpoint",
  AssociationQ[ftPublicPrepared] && AssociationQ[ftPublicTensor] &&
    ftPublicPrepared["Status"] === "PREPARED" &&
    SpinorExpressionComplexity[ftPublicPrepared["Amplitude"]] <
      SpinorExpressionComplexity[ftToy] &&
    ftNoNewPoles[ftToy, ftPublicPrepared["Amplitude"]] &&
    ftPublicPrepared["Options"]["InternalOnShell"] === False &&
    ftPublicItem["InternalOnShellChannels"] === {} &&
    ftPublicTensor["InternalOnShellIdentity"] === False &&
    ftPublicTensor["OnShellChannels"] === {} &&
    ftReplay[<|"Route" -> ftPublicTensor["IdentityRoute"]|>]];

Print["Four-leg tensor reduction: ", checks, " checks; ", Length[failures], " failures."];
If[failures =!= {}, Print[failures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:four-leg-tensor-reduction-tests.wl"];
Quit[0];
