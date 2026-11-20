(* Bounded ordinary reduction of four coherent massive-vector spin tensors.
   The fixture stores original inputs only; no compact process answer is used. *)

(*
HEPCAT - High Energy Physics Constructive Amplitude Tool

Copyright (C) 2026 Neil Christensen

This file is part of HEPCAT.

HEPCAT is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

HEPCAT is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with HEPCAT.  If not, see <https://www.gnu.org/licenses/>.
*)


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

(* Typed coordinate frames are reversible ordinary relabelings. Recurse into
   their completed core certificates rather than treating their metadata as
   another direct quotient record. *)
ftCoreRoutes[records_List] := Flatten[Map[Function[record,
  Switch[Lookup[record, "Operation", None],
    "TypedFrameFourLegTensorIdentities", ftCoreRoutes[record["CoreRoute"]],
    "ExactInverseSpectatorFourLegTensorGroups",
      Flatten[ftCoreRoutes[#["IdentityRoute"]] & /@ record["GroupCertificates"], 1],
    _, {record}]], records], 1];
ftCoreRoutes[report_Association] := ftCoreRoutes[report["Route"]];
ftFrameReplay[record_Association] := Module[
  {rename = ConstructiveDiagrams`Private`relabelLegs,
   forward = record["ForwardLabelMap"], backward = record["InverseLabelMap"]},
  TrueQ[record["ExactTypedRelabelingRoundTrip"]] &&
    TrueQ[record["ExactRestoredEndpoint"]] &&
    TrueQ[record["InverseArgumentsPreserved"]] &&
    rename[record["Before"], forward] === record["RenamedBefore"] &&
    rename[record["RenamedBefore"], backward] === record["Before"] &&
    rename[record["ExternalMassRules"], forward] === record["RenamedExternalMassRules"] &&
    rename[record["RenamedAfter"], backward] === record["After"] &&
    ftNoNewPoles[record["Before"], record["After"]] &&
    record["InternalOnShellConditions"] === {} &&
    ftReplay[<|"Route" -> record["CoreRoute"]|>]
];

(* A larger complete expression may have reached the same ordinary core
   through exact inverse-spectator groups. Replay the bookkeeping independently
   before following each group's original core identity certificate. *)
ftGroupReplay[record_Association] := Module[
  {forward = record["ProtectedInverseMap"], restore, certificates,
   reconstructedGroups},
  restore = Reverse /@ forward;
  certificates = record["GroupCertificates"];
  reconstructedGroups = Fold[ReplacePart[#1, #2["GroupIndex"] -> #2["Amplitude"]] &,
    record["OriginalGroups"], certificates];
  (record["Before"] /. forward) === record["ProtectedExpression"] &&
    Expand[record["ProtectedExpression"] - Total[record["ProtectedGroups"]]] === 0 &&
    record["ExactProtectedGroupReconstructionResidual"] === 0 &&
    (record["ProtectedGroups"] /. restore) === record["OriginalGroups"] &&
    reconstructedGroups === record["UpdatedGroups"] &&
    Total[record["UpdatedGroups"]] === record["After"] &&
    AllTrue[certificates, Function[certificate,
      certificate["OriginalGroup"] ===
        record["OriginalGroups"][[certificate["GroupIndex"]]] &&
      ftReplay[<|"Route" -> certificate["IdentityRoute"]|>]]] &&
    TrueQ[record["InverseArgumentsPreserved"]] &&
    TrueQ[record["StrictWholeExpressionRank"]] &&
    ftNoNewPoles[record["Before"], record["After"]] &&
    record["InternalOnShellConditions"] === {}
];

(* Recompute the exact algebraic certificates, without repeating the much
   slower symbolic proof of every row already used during development. *)
ftReplay[report_] := Module[{route, groups, sectors, mass, restore, sourceChecks,
    linearChecks, numeratorChecks, sectorValues},
  If[!AssociationQ[report] || report["Route"] === {}, Return[False]];
  AllTrue[report["Route"], Function[record,
    If[Lookup[record, "Operation", None] === "TypedFrameFourLegTensorIdentities",
      ftFrameReplay[record],
    If[Lookup[record, "Operation", None] === "ExactInverseSpectatorFourLegTensorGroups",
      ftGroupReplay[record],
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
      route["InternalOnShellConditions"] === {}]]]]
];
ftMassRoundTrip[report_] := AllTrue[ftCoreRoutes[report], Function[route,
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
  (* Allow the reducer its full ten-second search budget for both fixtures.
     The zero- and exhausted-budget cases below check deadline behavior. *)
  ftReport = ftReduce[ftCase["Amplitude"], ftCase["ExternalMassRules"], True, 10, None];
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
    AllTrue[ftCoreRoutes[ftCompositeReport], Function[route,
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

(* Existing-term dependence compaction uses a sampled proposal followed by a
   generic symbolic identity certificate. No stored process answer is used. *)
ftDependence = ConstructiveDiagrams`Private`preparationExistingTermCompaction;
ftDependenceMass = ModelParameter[ftModel, "MW"];
ftDependenceRules = Thread[Mass /@ Range[4] -> ftDependenceMass];
ftDependenceTensor = ftA[1, 3]^2 ftA[2, 4]^2;
ftDependencePole13 = PropDen[Mom[Multiparticle[1, 3]], ftDependenceMass];
ftDependencePole14 = PropDen[Mom[Multiparticle[1, 4]], ftDependenceMass];
ftDependenceInput = Expand[
  (ftDependenceMass^2 + 2 MomProd[1, 3]) ftDependenceTensor/ftDependencePole13 +
  2 (ftDependenceMass^2 + 2 MomProd[1, 4]) ftDependenceTensor/ftDependencePole14];
ftDependenceReport = ftDependence[ftDependenceInput, ftDependenceRules, 4];
ftDependenceRoute = First[Lookup[ftDependenceReport, "Route", {<||>}]];
check["ordinary term dependence compacts regular pieces across physical pole sectors",
  ftDependenceReport["Status"] === "IMPROVED" &&
  SpinorExpressionComplexity[ftDependenceReport["Amplitude"]] <
    SpinorExpressionComplexity[ftDependenceInput] &&
  ftNoNewPoles[ftDependenceInput, ftDependenceReport["Amplitude"]]];
check["the compactor reweights only its own terms and certifies all parameters symbolically",
  ftDependenceRoute["After"] === Expand[
    ftDependenceRoute["ConstantReweights"].ftDependenceRoute["OriginalTerms"]] &&
  AllTrue[ftDependenceRoute["ConstantReweights"], NumberQ[#] && FreeQ[#, _Real] &] &&
  ftDependenceRoute["ExactGenericResidual"] === 0 &&
  ftDependenceRoute["InternalOnShellConditions"] === {} &&
  ftDependenceRoute["KnownAmplitudeOrAddedTensorUsed"] === False &&
  AllTrue[First /@ ftDependenceRoute["AbstractParameterRules"], MatchQ[#, _ModelParameter] &]];
(* PermuteParticles takes a cycle, so apply the same nontrivial cycle to the
   expression and its mass metadata. *)
ftDependencePermutation = {3, 1, 4, 2};
ftDependencePermuted = PermuteParticles[ftDependenceInput, ftDependencePermutation];
ftDependencePermutedRules = PermuteParticles[ftDependenceRules, ftDependencePermutation];
ftDependencePermutedReport = ftDependence[ftDependencePermuted, ftDependencePermutedRules, 4];
check["the same compactor handles ordinary relabeled four-vector expressions",
  ftDependencePermuted =!= ftDependenceInput &&
  ftDependencePermutedReport["Status"] === "IMPROVED" &&
  First[ftDependencePermutedReport["Route"]]["ExactGenericResidual"] === 0 &&
  ftNoNewPoles[ftDependencePermuted, ftDependencePermutedReport["Amplitude"]]];
ftDependenceWidthInput = ftDependenceInput /. PropDen[p_, m_] :> PropDen[p, m, ftWidth];
check["finite-width inverse objects are preserved by the unsupported-input fallback",
  ftDependence[ftDependenceWidthInput, ftDependenceRules, 1]["Amplitude"] === ftDependenceWidthInput];
check["higher multiplicity and non-vector inputs bypass term dependence compaction",
  ftDependence[ftDependenceInput, Append[ftDependenceRules, Mass[5] -> ftDependenceMass], 1]["Amplitude"] === ftDependenceInput &&
  ftDependence[ftA[1, 2] ftS[3, 4], ftDependenceRules, 1]["Amplitude"] === ftA[1, 2] ftS[3, 4]];
check["zero budget returns the original expression",
  ftDependence[ftDependenceInput, ftDependenceRules, 0]["Amplitude"] === ftDependenceInput];
check["a huge powered chain is rejected before repeated-chain allocation",
  TimeConstrained[
    ftDependence[ftA[1, 2]^100000000, ftDependenceRules, 1]["Amplitude"] ===
      ftA[1, 2]^100000000, 2, False]];
check["prospective expansion is guarded before a huge power is distributed",
  TimeConstrained[
    ftDependence[(ftA[1, 2] + ftA[1, 3])^100000000, ftDependenceRules, 1]["Amplitude"] ===
      (ftA[1, 2] + ftA[1, 3])^100000000, 2, False]];
(* This polynomial vanishes on every deliberately small parameter sample,
   but is not identically zero. Numeric fitting alone would drop it. *)
ftDependenceCharge = ModelParameter[ftModel, "EE"];
ftAdversarialPolynomial = Expand[Times @@ (ftDependenceCharge - Prime[#] & /@ Range[1, 12])];
ftAdversarialInput = Expand[ftAdversarialPolynomial ftDependenceTensor/ftDependencePole13];
ftAdversarialReport = ftDependence[ftAdversarialInput, ftDependenceRules, 4];
check["a sample-only cancellation is rejected by the generic symbolic proof",
  ftAdversarialReport["Status"] === "UNCHANGED" &&
  ftAdversarialReport["StopReason"] === "GenericOrdinaryProofDidNotClose" &&
  ftAdversarialReport["Amplitude"] === ftAdversarialInput];
check["the exact certificate records a dense chart and every coherent spin-one component",
  ftDependenceRoute["DenseFourMassiveExternalChart"] === True &&
  ftDependenceRoute["CovariantTypedScalar"] === True &&
  ftDependenceRoute["FullTypedMapCoverage"] === True &&
  ftDependenceRoute["CoherentDegreeBounds"] === {2, 2, 2, 2} &&
  ftDependenceRoute["SymmetricSpinOneTensorInterpretation"] === True &&
  ftDependenceRoute["NumericalSamplesProposeConstantsOnly"] === True &&
  AllTrue[ftDependenceRoute["MassiveBasisCertificates"], TrueQ[#["Validated"]] &] &&
  AllTrue[ftDependenceRoute["OriginalDenominatorRegularityWitnesses"],
    TrueQ[#["EveryOriginalDenominatorRegular"]] &]];
ftBadBasisChart = ConstructiveDiagrams`Private`completionHESymbolicExternalChart[ftDependenceRules];
ftBadBasisChart["AngleBasis"] = Join[ftBadBasisChart["AngleBasis"], <|1 -> ConstantArray[0, {2, 2}]|>];
ftBadBasisReport = Internal`InheritedBlock[
  {ConstructiveDiagrams`Private`completionHESymbolicExternalChart},
  Clear[ConstructiveDiagrams`Private`completionHESymbolicExternalChart];
  ConstructiveDiagrams`Private`completionHESymbolicExternalChart[___] := ftBadBasisChart;
  ftDependence[ftDependenceInput, ftDependenceRules, 1]];
check["a singular or malformed massive-spin basis fails closed",
  ftBadBasisReport["Status"] === "UNCHANGED" &&
  ftBadBasisReport["StopReason"] === "InvalidMassiveSpinBasis" &&
  ftBadBasisReport["Amplitude"] === ftDependenceInput];
ftExcessSpinDegree = ftA[1, 2]^3 ftA[3, 4] + 2 ftA[1, 3]^3 ftA[2, 4];
ftExcessSpinReport = ftDependence[ftExcessSpinDegree, ftDependenceRules, 1];
check["non-spin-one slot degrees are rejected before coherent fitting",
  ftExcessSpinReport["Status"] === "UNCHANGED" &&
  ftExcessSpinReport["StopReason"] === "NotFourCoherentVectorSlots"];
ftUnsupportedMapReport = Internal`InheritedBlock[
  {ConstructiveDiagrams`Private`spuriousEvaluateCurve},
  Clear[ConstructiveDiagrams`Private`spuriousEvaluateCurve];
  ConstructiveDiagrams`Private`spuriousEvaluateCurve[___] := Missing["ForcedUnsupportedMap"];
  ftDependence[ftDependenceInput, ftDependenceRules, 1]];
check["unsupported typed maps are rejected rather than converted to zero rows",
  ftUnsupportedMapReport["Status"] === "UNCHANGED" &&
  ftUnsupportedMapReport["StopReason"] === "InsufficientRegularCharts" &&
  ftUnsupportedMapReport["Amplitude"] === ftDependenceInput];

(* Source and factorization preparation deliberately defer optional final
   polishing. Observe the caller's scope without performing a second fit. *)
ftCompactionScopeCalls = {};
ftCompactionScope[deferred_] := Internal`InheritedBlock[
  {ConstructiveDiagrams`Private`preparationExistingTermCompaction},
  Clear[ConstructiveDiagrams`Private`preparationExistingTermCompaction];
  ConstructiveDiagrams`Private`preparationExistingTermCompaction[body_, _, _] := (
    AppendTo[ftCompactionScopeCalls,
      TrueQ[ConstructiveDiagrams`Private`$deferExactTensorSupportPolish]];
    <|"Status" -> "UNCHANGED", "Amplitude" -> body, "Route" -> {}|>);
  Block[{ConstructiveDiagrams`Private`$deferExactTensorSupportPolish = deferred},
    PrepareAmplitudes[{ftDependenceRules, ftDependenceInput}, ftModel,
      "MaxSteps" -> 0, "InternalOnShell" -> False, "PolePreference" -> None,
      "TimeConstraint" -> 6, "ReturnDetails" -> True,
      "Parallelization" -> False, "ProgressReporting" -> False]]];
ftCompactionDeferred = ftCompactionScope[True];
check["deferred source preparation skips the final term-dependence fit",
  ftCompactionScopeCalls === {} && AssociationQ[ftCompactionDeferred]];
ftCompactionFinal = ftCompactionScope[False];
check["normal final preparation still invokes exactly one term-dependence fit",
  ftCompactionScopeCalls === {False} && AssociationQ[ftCompactionFinal]];

(* A retained original rational input used to consume its entire proof cap
   while testing polynomiality of an unsimplified rational zero. Model values
   remain symbolic in the certificate; numerical charts only propose a point. *)
ftRationalCertificateCase = ftInputs["ZZWWRationalCertificateInput"];
ftRationalCertificateReport = ftDependence[
  ftRationalCertificateCase["Amplitude"], ftRationalCertificateCase["ExternalMassRules"], 15];
check["rational cancellation precedes expensive generic coherent classification",
  ftRationalCertificateReport["Status"] === "IMPROVED" &&
  SpinorExpressionComplexity[ftRationalCertificateReport["Amplitude"]] <
    SpinorExpressionComplexity[ftRationalCertificateCase["Amplitude"]] &&
  First[ftRationalCertificateReport["Route"]]["ExactGenericResidual"] === 0 &&
  First[ftRationalCertificateReport["Route"]]["InternalOnShellConditions"] === {} &&
  First[ftRationalCertificateReport["Route"]]["KnownAmplitudeOrAddedTensorUsed"] === False];
ftRationalTinyBudget = ftDependence[ftRationalCertificateCase["Amplitude"],
  ftRationalCertificateCase["ExternalMassRules"], 0.001];
check["a positive exhausted proof budget preserves the complete original input",
  ftRationalTinyBudget["Status"] === "UNCHANGED" && TrueQ[ftRationalTinyBudget["TimedOut"]] &&
  ftRationalTinyBudget["Amplitude"] === ftRationalCertificateCase["Amplitude"]];
ftIndependentTerms = ftDependenceTensor/ftDependencePole13 +
  ftA[1, 2]^2 ftA[3, 4]^2/ftDependencePole14;
ftIndependentReport = ftDependence[ftIndependentTerms, ftDependenceRules, 2];
check["independent existing monomials remain unchanged without a strict improvement",
  ftIndependentReport["Status"] === "UNCHANGED" &&
  ftIndependentReport["StopReason"] === "NoStrictRankImprovement" &&
  ftIndependentReport["Amplitude"] === ftIndependentTerms];

Print["Four-leg tensor reduction: ", checks, " checks; ", Length[failures], " failures."];
If[failures =!= {}, Print[failures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:four-leg-tensor-reduction-tests.wl"];
Quit[0];
