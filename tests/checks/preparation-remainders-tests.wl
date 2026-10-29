(* Preparation provenance is explicit data, not an off-cut identity claim. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++;
  If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);
rrZero[value_] := Together[value] === 0;
rrModel = {{{"Scalar", "s", "s", 1, 0, 0, 0, 1, 1, "", ""}}, {}, {}, {}};
rrExternal = Thread[(Mass /@ Range[4]) -> 0];
rrRules = Append[rrExternal, Mass[Multiparticle[1, 2]] -> 1];
rrDen = PropDen[Mom[Multiparticle[1, 2]], 1];
rrPair = {rrRules, (3 + MomProd[1, 2])/rrDen};
rrSaved = rrPair;
rrPrepare[input_, opts___] := PrepareAmplitudes[input, rrModel, opts,
  "MaxSteps" -> 0, "ProgressReporting" -> False, "ReturnDetails" -> True];
rrPrepared = rrPrepare[{rrPair}, "PolePreference" -> None];
rrItem = First[rrPrepared["ItemReports"]]; rrData = rrItem["RemainderData"];
check["preparation exposes one remainder record per input item only in details",
  rrPrepared["RemainderData"] === {rrData} &&
    rrPrepared["PreparedAmplitudes"] === {{rrRules, 7/(2 rrDen)}} &&
    PrepareAmplitudes[{rrPair}, rrModel, "PolePreference" -> None,
      "MaxSteps" -> 0, "ProgressReporting" -> False] === rrPrepared["PreparedAmplitudes"]];
check["the retained fixed-reference remainder exactly reconstructs its source",
  rrData["Status"] === "COMPLETE" &&
    rrZero[rrData["PreparedExpression"] + rrData["ShellRemainder"] -
      rrData["FixedReferenceExpression"]] &&
    rrZero[rrData["ShellRemainder"] - (MomProd[1, 2] - 1/2)/rrDen]];
check["preparation provenance does not mutate the original pair or propagator",
  rrPair === rrSaved && rrPrepared["OriginalAmplitudes"] === {rrSaved} &&
    rrData["DenominatorsKeptOffShell"] && !FreeQ[rrData["ShellRemainder"], rrDen]];
check["each completed stage retains its literal grouped difference",
  rrData["Stages"] =!= {} && AllTrue[rrData["Stages"],
    rrZero[#["After"] + #["Remainder"] - #["Before"]] &] &&
    AllTrue[rrData["Stages"], !TrueQ[#["OffCutIdentityClaimed"]] &]];
check["the shell provenance identifies only declared internal conditions",
  rrData["OnShellChannels"] === {{1, 2}} && rrData["HasInternalConditions"] &&
    rrData["ExternalMassRules"] === rrExternal &&
    rrData["MassRules"] === rrRules];
rrOff = rrPrepare[{rrPair}, "PolePreference" -> None, "InternalOnShell" -> False];
rrOffData = First[rrOff["RemainderData"]];
check["disabling internal shells retains a source without introducing shell assumptions",
  rrOffData["OnShellChannels"] === {} && !rrOffData["HasInternalConditions"] &&
    rrZero[rrOffData["ShellRemainder"]] &&
    rrZero[rrOffData["PreparedExpression"] - Last[rrPair]]];
rrNestedPair = {rrRules, (3 + MomProd[1, 2])/(rrDen (5 + MomProd[1, 2]))};
rrNested = First[rrPrepare[{rrNestedPair}, "PolePreference" -> None]["RemainderData"]];
check["retained numerator reductions preserve nested inverse factors",
  rrZero[rrNested["PreparedExpression"] - 7/(2 rrDen (5 + MomProd[1, 2]))] &&
    rrZero[rrNested["ShellRemainder"] - (MomProd[1, 2] - 1/2)/(rrDen (5 + MomProd[1, 2]))]];

rrChannel = {0, 0, 0, 0, Multiparticle[1, 2], 1,
  (3 + MomProd[1, 2] + MomProd[1, 3])/(rrDen PropDen[Mom[Multiparticle[1, 3]], 2])};
rrChannelPrepared = rrPrepare[{rrChannel}, "PolePreference" -> None];
rrChannelData = First[rrChannelPrepared["RemainderData"]];
check["prepared factorization rows retain only their selected cut",
  rrChannelPrepared["InputType"] === "FactorizationChannels" &&
    rrChannelData["InputType"] === "FactorizationChannel" &&
    rrChannelData["OnShellChannels"] === {{1, 2}} &&
    FreeQ[rrChannelData["MassRules"], Mass[Multiparticle[1, 3]]] &&
    !FreeQ[rrChannelData["PreparedExpression"], MomProd[1, 3]]];
check["factorization preparation preserves row metadata and original targets",
  Drop[First[rrChannelPrepared["PreparedAmplitudes"]], -1] === Drop[rrChannel, -1] &&
    rrChannelPrepared["OriginalAmplitudes"] === {rrChannel} &&
    rrZero[rrChannelData["PreparedExpression"] + rrChannelData["ShellRemainder"] -
      rrChannelData["FixedReferenceExpression"]]];
check["the factorization shell remainder still records other off-shell propagators",
  !FreeQ[rrChannelData["ShellRemainder"], PropDen[Mom[Multiparticle[1, 3]], 2]] &&
    !TrueQ[rrChannelData["OffCutIdentityClaimed"]]];

rrReferenceRatio[id_, leg_] :=
  SpinorChain[Spinor["Xi", "Angle", id], Mom[1], Spinor["Helicity", "Square", leg]]/
    SpinorChain[Spinor["Xi", "Angle", id], Spinor["Helicity", "Angle", leg]];
rrReferencePair = {rrRules, rrReferenceRatio[71, 3] rrReferenceRatio[72, 4]/rrDen};
rrReferencePrepared = rrPrepare[{rrReferencePair}, "PolePreference" -> "IndependentXi"];
rrReferenceData = First[rrReferencePrepared["RemainderData"]];
rrCommon = SelectFirst[rrReferenceData["ReferenceContinuations"], #["Mode"] === "Xi" &];
rrIndependent = SelectFirst[rrReferenceData["ReferenceContinuations"], #["Mode"] === "IndependentXi" &];
check["one common-Xi source is retained before reference and shell reductions",
  DeleteDuplicates[Cases[rrCommon["Expression"], Spinor["Xi", _, id_] :> id, Infinity]] === {0} &&
    rrCommon["RequiresFullAmplitudeValidation"] && !rrCommon["OffCutIdentityClaimed"] &&
    rrCommon["Expression"] === (rrReferenceData["ReferenceInputExpression"] /.
      Spinor["Xi", kind_, _] :> Spinor["Xi", kind, 0])];
check["unambiguous references retain a shared per-external-leg continuation",
  Sort[DeleteDuplicates[Cases[rrIndependent["Expression"],
      Spinor["Xi", _, id_] :> id, Infinity]]] === {3, 4} &&
    rrReferenceData["Completeness"]["ReferenceContinuationsComplete"]];
check["reference-change differences are separate from fixed-reference remainders",
  rrZero[rrReferenceData["ReferenceInputExpression"] -
    rrReferenceData["ReferenceChangeRemainder"] - rrReferenceData["FixedReferenceExpression"]] &&
    rrZero[rrReferenceData["FixedReferenceExpression"] -
      rrReferenceData["ShellRemainder"] - rrReferenceData["PreparedExpression"]] &&
    AnyTrue[rrReferenceData["Stages"], #["Scope"] === "ReferenceContinuationChange" &]];
rrCommonPrepared = rrPrepare[{rrReferencePair}, "PolePreference" -> "Xi"];
rrCommonData = First[rrCommonPrepared["RemainderData"]];
check["an unassigned per-leg mapping is diagnosed without inventing one",
  Lookup[rrCommonData["ReferenceContinuations"], "Mode"] === {"Xi"} &&
    !rrCommonData["Completeness"]["ReferenceContinuationsComplete"] &&
    rrCommonData["Diagnostics"] =!= {}];
rrAlternativePrepared = rrPrepare[{rrReferencePair}, "PolePreference" -> "AllSpurious",
  "MaxReferenceChoices" -> 1, "ReferenceTimeConstraint" -> 2];
rrAlternativeItem = First[rrAlternativePrepared["ItemReports"]];
rrAlternatives = rrAlternativeItem["RemainderData"]["ReferenceAlternatives"];
check["completed reference alternatives retain their own source and discarded differences",
  rrAlternatives =!= {} && rrAlternatives === rrAlternativeItem["PolePreference"]["Alternatives"] &&
    AllTrue[rrAlternatives, AssociationQ[#] &&
      rrZero[#["FixedReferenceExpression"] - #["ReducedExpression"] - #["ShellRemainder"]] &&
      #["Expression"] === #["ReducedExpression"] && !#["OffCutIdentityClaimed"] &]];
check["non-search reference choices expose an empty alternative list explicitly",
  rrCommonData["ReferenceAlternatives"] === {} && rrData["ReferenceAlternatives"] === {}];

rrSingleRules = {Mass[1] -> 1, Mass[2] -> 1, Mass[3] -> 1, Mass[4] -> 0,
  Mass[Multiparticle[1, 2]] -> 1};
rrSinglePair = {rrSingleRules, rrReferenceRatio[71, 4]/rrDen};
rrUnavailable = rrPrepare[{rrSinglePair}, "PolePreference" -> "AllSpurious"];
rrUnavailableData = First[rrUnavailable["RemainderData"]];
check["an unavailable reference remains partial instead of asserting a completed shell source",
  rrUnavailable["Status"] === "PARTIAL" && rrUnavailableData["Status"] === "PARTIAL" &&
    !rrUnavailableData["Completeness"]["FixedReferenceAvailable"] &&
    MissingQ[rrUnavailableData["ShellRemainder"]] &&
    rrUnavailableData["Diagnostics"] =!= {}];
check["capturing a failed reference still preserves the completed contracted source",
  rrUnavailableData["Completeness"]["ReferenceInputAvailable"] &&
    Lookup[rrUnavailableData["ReferenceContinuations"], "Mode"] === {"Xi"}];
check["empty preparations return aligned empty remainder data",
  rrPrepare[{}, "PolePreference" -> None]["RemainderData"] === {}];

(* A cubic scalar tree supplies actual independent physical poles. Rewrite
   one genuine diagram using an ordinary pair-virtuality identity, then let
   preparation remove it on a different diagram's numerator cut. Completion
   must recover that lost pole from the recorded difference, not an answer. *)
rrCubicModel = {{{"Scalar", "s", "s", 1, 0, rrMass, 0, 1, 1, "", ""}},
  {{"s", "s", "s", "", rrCoupling, 1}}, {}, {}};
rrCubicPairs = diagramAmplitudes[CreateDiagrams[ConstantArray["s", 4], rrCubicModel]];
rrCubicChannels = FactorizationAmplitudeChannels[ConstantArray["s", 4], rrCubicModel];
rrCubicTerms = Last /@ rrCubicPairs;
rrCubicTree = Total[rrCubicTerms];
rrCut = First[First[ConstructiveDiagrams`Private`knownInternalChannels[First[First[rrCubicPairs]]]]];
rrCutDen = PropDen[Mom[Multiparticle @@ rrCut], rrMass];
rrVirtuality = 2 (MomProd @@ rrCut) + rrMass^2;
rrCubicInput = {First[First[rrCubicPairs]],
  rrCubicTerms[[1]] + rrCubicTerms[[3]] + rrCubicTerms[[2]] rrVirtuality/rrCutDen};
rrCubicPrepared = PrepareAmplitudes[{rrCubicInput}, rrCubicModel,
  "PolePreference" -> None, "MaxSteps" -> 0,
  "ProgressReporting" -> False, "ReturnDetails" -> True];
rrCubicRules = {rrMass -> 1, rrCoupling -> 2};
(* This fixture isolates reconstruction from remainders without a symmetry fit. *)
rrFactor[input_, channels_, opts___] := CompleteFactorization[input, channels, rrCubicModel,
  opts, "BoseSymmetryGroups" -> {}, "Samples" -> 2, "ValidationSamples" -> 2, "MaxCorrectionTerms" -> 16,
  "RandomSeed" -> 728891, "ParameterRules" -> rrCubicRules,
  "ProgressReporting" -> False, "ReturnDetails" -> True, "TimeConstraint" -> 60];
rrCompleted = rrFactor[rrCubicPrepared, rrCubicChannels];
check["actual preparation records a reduction that removes a different scalar-tree pole",
  rrZero[rrCubicPrepared["Amplitude"] - rrCubicTerms[[1]] - rrCubicTerms[[3]]] &&
    First[rrCubicPrepared["RemainderData"]]["ShellRemainder"] =!= 0];
check["completion first recovers a missing physical pole from retained preparation data",
  rrCompleted["Status"] === "COMPLETE" &&
    AssociationQ[rrCompleted["PreparationRemainderFit"]] &&
    rrCompleted["PreparationRemainderFit"]["Status"] === "COMPLETE" &&
    rrZero[ConstructiveDiagrams`Private`residueCanonicalPropagators[
      rrCompleted["Amplitude"] - rrCubicTree, Range[4]]]];
check["the recovered remainder fit independently validates every original channel",
  AllTrue[rrCompleted["ValidationReport"]["Channels"], #["Status"] === "PASS" &] &&
    rrCompleted["OriginalExpression"] === rrCubicPrepared["Amplitude"]];
rrChannelsPrepared = PrepareAmplitudes[rrCubicChannels, rrCubicModel,
  "PolePreference" -> None, "MaxSteps" -> 0,
  "ProgressReporting" -> False, "ReturnDetails" -> True];
rrZeroPreparedChannels = Join[rrChannelsPrepared, <|"PreparedAmplitudes" ->
  (ReplacePart[#, -1 -> 0] & /@ rrChannelsPrepared["PreparedAmplitudes"])|>];
rrFromChannels = rrFactor[0, rrZeroPreparedChannels];
check["prepared channel associations retain original nonzero target residues",
  rrFromChannels["Status"] === "COMPLETE" &&
    rrFromChannels["ChannelPreparation"]["OriginalChannelsAreTargets"] &&
    rrFromChannels["ChannelPreparation"]["PreparedChannels"] ===
      rrZeroPreparedChannels["PreparedAmplitudes"] &&
    rrZero[ConstructiveDiagrams`Private`residueCanonicalPropagators[
      rrFromChannels["Amplitude"] - rrCubicTree, Range[4]]]];
rrDisabled = rrFactor[rrCubicPrepared, rrCubicChannels, "PreparationRemainders" -> None];
check["PreparationRemainders None disables the new trial while preserving completion",
  rrDisabled["Status"] === "COMPLETE" &&
    MissingQ[rrDisabled["PreparationRemainders"]] &&
    rrZero[ConstructiveDiagrams`Private`residueCanonicalPropagators[
      rrDisabled["Amplitude"] - rrCubicTree, Range[4]]]];
rrExplicit = rrFactor[rrCubicPrepared, rrCubicChannels,
  "CorrectionBasis" -> {rrCubicTerms[[2]]}];
rrLiteral = rrFactor[rrCubicPrepared, rrCubicChannels, "ChannelReferences" -> None];
check["explicit correction bases and literal-channel mode bypass preparation remainder trials",
  rrExplicit["Status"] === "COMPLETE" && rrLiteral["Status"] === "COMPLETE" &&
    MissingQ[rrExplicit["PreparationRemainders"]] && MissingQ[rrLiteral["PreparationRemainders"]] &&
    Length[rrExplicit["CorrectionBasis"]] === 1];
check["a prepared channel report without a model is rejected rather than partly interpreted",
  Quiet[rrFactor[0, KeyDrop[rrChannelsPrepared, "Model"]]] === $Failed];
check["a prepared channel report with changed cut metadata is rejected",
  Quiet[rrFactor[0, Join[rrChannelsPrepared, <|"PreparedAmplitudes" ->
    ReplacePart[rrChannelsPrepared["PreparedAmplitudes"], {1, -2} -> 2 rrMass]|>]]] === $Failed];
check["a prepared channel report with unaligned remainder records is rejected",
  Quiet[rrFactor[0, Join[rrChannelsPrepared, <|"RemainderData" -> {<||>}|>]]] === $Failed &&
    Quiet[rrFactor[0, Join[rrChannelsPrepared, <|"RemainderData" ->
      ConstantArray[0, Length[rrCubicChannels]]|>]]] === $Failed];
rrTargets = ConstructiveDiagrams`Private`completionTargets[
  ConstructiveDiagrams`Private`residueSimpleInputs[0, rrCubicChannels, rrCubicModel]["Targets"]];
rrCubicExternal = Select[First[First[rrCubicPairs]], MatchQ[#, HoldPattern[Mass[_Integer] -> _]] &];
rrOtherDen = First[Cases[rrCubicTerms[[2]], _PropDen, Infinity]];
rrVirtualityInput = rrVirtuality/(rrOtherDen (5 + (MomProd @@ rrCut)));
rrExposed = ConstructiveDiagrams`Private`completionExposePairVirtualities[
  rrVirtualityInput, rrTargets, rrCubicExternal];
check["virtuality exposure protects reciprocal scalar products and keeps the virtuality off shell",
  rrZero[rrExposed - rrCutDen/(rrOtherDen (5 + (MomProd @@ rrCut)))] &&
    !FreeQ[rrExposed, (MomProd @@ rrCut)] && !FreeQ[rrExposed, rrCutDen]];
rrVirtualityChecks = Table[
  rrChart = ConstructiveDiagrams`Private`amplitudeRationalComparisonChart[
    rrTargets, sample, 718351, rrCubicRules];
  rrZero[ConstructiveDiagrams`Private`residueEvaluateChart[
    rrVirtualityInput - rrExposed, rrChart, rrCubicRules]], {sample, 2}];
check["exposed pair virtualities agree at independent off-cut conserved kinematics",
  And @@ rrVirtualityChecks];
rrZeroChannels = ReplacePart[rrCubicChannels, {2, -1} -> 0];
rrZeroResult = rrFactor[rrCubicPrepared, rrZeroChannels];
check["retained discarded directions are not restored when their target residue is zero",
  rrZeroResult["Status"] === "COMPLETE" &&
    rrZero[ConstructiveDiagrams`Private`residueCanonicalPropagators[
      rrZeroResult["Amplitude"] - rrCubicTerms[[1]] - rrCubicTerms[[3]], Range[4]]] &&
    AllTrue[rrZeroResult["ValidationReport"]["Channels"], #["Status"] === "PASS" &]];

rrProbeNormalized = ConstructiveDiagrams`Private`completionNormalizeInput[
  0, rrCubicChannels, rrCubicModel];
rrProbeSaved = rrProbeNormalized;
rrProbeBad = ConstructiveDiagrams`Private`completionPreparationProbe[
  0, {0}, rrProbeNormalized, rrCubicRules, 64891, 5];
check["the exact preparation preflight rejects an inconsistent correction space",
  rrProbeBad["Status"] === "INCONSISTENT" &&
    rrProbeBad["LinearSolution"]["Status"] === "INCONSISTENT" &&
    rrProbeBad["EquationCount"] > 0];
rrProbeGood = ConstructiveDiagrams`Private`completionPreparationProbe[
  0, {rrCubicTree}, rrProbeNormalized, rrCubicRules, 64891, 5];
check["a consistent preparation preflight never certifies completion",
  rrProbeGood["Status"] === "NOT_REJECTED" &&
    rrProbeGood["Status"] =!= "COMPLETE" && rrProbeGood["Samples"] =!= {} &&
    rrProbeGood["Scope"] === "ExactEarlyRejectionOnlyFullValidationStillRequired"];
rrFirstProbeTarget = SelectFirst[SortBy[rrTargets, Length[#["Cut"]] &],
  TrueQ[Lookup[#, "Complete", False]] && !TrueQ[#["StateSum"] === 0] &];
rrOneCutSource = rrFirstProbeTarget["StateSum"]/rrFirstProbeTarget["Propagator"];
rrProbeOneCut = ConstructiveDiagrams`Private`completionPreparationProbe[
  0, {rrOneCutSource}, rrProbeNormalized, rrCubicRules, 64891, 5];
rrOneCutCompletion = rrFactor[0, rrCubicChannels, "CorrectionBasis" -> {rrOneCutSource}];
check["passing the sampled preflight cut cannot bypass other physical channels",
  rrProbeOneCut["Status"] === "NOT_REJECTED" &&
    rrOneCutCompletion["Status"] === "UNRESOLVED" &&
    rrOneCutCompletion["ConstraintData"]["LinearSolution"]["Status"] === "INCONSISTENT"];
rrProbeZeroNormalized = ConstructiveDiagrams`Private`completionNormalizeInput[
  0, rrZeroChannels, rrCubicModel];
rrProbeZero = ConstructiveDiagrams`Private`completionPreparationProbe[
  0, {rrCubicTree}, rrProbeZeroNormalized, rrCubicRules, 64891, 5];
rrZeroObligationCompletion = rrFactor[0, rrZeroChannels, "CorrectionBasis" -> {rrCubicTree}];
check["zero-residue obligations remain mandatory after a nonzero-cut preflight passes",
  rrProbeZero["Status"] === "NOT_REJECTED" &&
    rrZeroObligationCompletion["Status"] === "UNRESOLVED" &&
    rrZeroObligationCompletion["ConstraintData"]["LinearSolution"]["Status"] === "INCONSISTENT"];
rrProbeTimed = ConstructiveDiagrams`Private`completionPreparationProbe[
  0, {rrCubicTree}, rrProbeNormalized, rrCubicRules, 64891, 0.000001];
check["a preflight timeout retains its undecided status without weakening the target data",
  rrProbeTimed["Status"] === "NOT_REJECTED" && rrProbeTimed["TimedOut"] &&
    rrProbeNormalized === rrProbeSaved];
rrProbeInexact = ConstructiveDiagrams`Private`completionPreparationProbe[
  0, {0}, rrProbeNormalized, {rrMass -> 1, rrCoupling -> 2.0}, 64891, 5];
check["inexact equations cannot supply a claimed exact preflight disproof",
  rrProbeInexact["Status"] === "NOT_REJECTED" && rrProbeInexact["EquationCount"] > 0 &&
    Lookup[rrProbeInexact["LinearSolution"], "Status", None] =!= "INCONSISTENT"];
rrProbeInexactComplex = ConstructiveDiagrams`Private`completionPreparationProbe[
  0, {0}, rrProbeNormalized, {rrMass -> 1, rrCoupling -> 2.0 + 0.1 I}, 64891, 5];
check["atomic inexact complex coefficients also leave the exact preflight undecided",
  rrProbeInexactComplex["Status"] === "NOT_REJECTED" &&
    rrProbeInexactComplex["EquationCount"] > 0 &&
    Lookup[rrProbeInexactComplex["LinearSolution"], "Status", None] =!= "INCONSISTENT"];

(* Force the new final continuation route using genuine five-point diagrams,
   not a supplied amplitude answer. Earlier correction spaces are disabled
   only inside this test; PolePreference -> None also bypasses the earlier
   preferred-source attempt while retaining all preparation provenance. The
   retained common-reference direction still undergoes the ordinary full
   six-channel fit and held-out validation. *)
Get[FileNameJoin[{root, "tests", "QED-5-point-residues.wl"}]];
rrQEDRaw = diagramAmplitudes[CreateDiagrams[prParticles, prModel]];
rrQEDPrepared = PrepareAmplitudes[rrQEDRaw, prModel,
  "PolePreference" -> "AllPhysical", "MaxReferenceChoices" -> 1,
  "ReferenceTimeConstraint" -> 5, "MaxSteps" -> 0,
  "ProgressReporting" -> False, "ReturnDetails" -> True];
rrQEDNormalized = ConstructiveDiagrams`Private`completionNormalizeInput[
  rrQEDPrepared, prChannels, prFixtureModel];
rrQEDPlan = ConstructiveDiagrams`Private`completionBosePlan[
  {{3, 4, 5}}, rrQEDNormalized["ExternalMassRules"]];
rrQEDRetainedSpace = ConstructiveDiagrams`Private`completionPreparationBasis[
  rrQEDPrepared, <||>, rrQEDNormalized, rrQEDPlan, 1, 3];
rrQEDFallback = Block[{
  ConstructiveDiagrams`Private`completionPreparationBasis =
    Function[{input, channels, normalized, plan, limit, seconds},
      Join[rrQEDRetainedSpace, <|"Basis" -> Missing["TestDisabled"]|>]],
  ConstructiveDiagrams`Private`completionPeeledChannelBasis =
    Function[{normalized, continuations, plan, limit, rules, seed, seconds}, Missing["TestDisabled"]],
  ConstructiveDiagrams`Private`completionReducedChannelBasis =
    Function[{normalized, continuations, limit, rules, seed}, <|"Basis" -> {}|>],
  ConstructiveDiagrams`Private`completionAutomaticBasis =
    Function[{normalized, limit, references}, {}]},
  CompleteFactorization[rrQEDPrepared, prChannels, prFixtureModel,
    "PolePreference" -> None, "InputReweighting" -> None, "MaxCorrectionTerms" -> 1,
    "BoseSymmetryGroups" -> {{3, 4, 5}}, "Samples" -> 1, "ValidationSamples" -> 1,
    "ParameterRules" -> {"Me" -> 1, "EE" -> 1},
    "TimeConstraint" -> 30, "ProgressReporting" -> False, "ReturnDetails" -> True]];
check["a retained common-reference QED continuation is validated only as the final fallback",
  rrQEDFallback["Status"] === "COMPLETE" &&
    rrQEDFallback["CorrectionSpace"] === "RetainedReferences" &&
    rrQEDFallback["PreparationReferenceAttempts"] =!= {} &&
    AllTrue[rrQEDFallback["ValidationReport"]["Channels"], #["Status"] === "PASS" &] &&
    Length[rrQEDFallback["ValidationReport"]["Channels"]] === 6];
check["a retained-reference child preserves the parent's Bose declaration and original input",
  rrQEDFallback["BoseSymmetry"] === <|"Groups" -> {{3, 4, 5}}, "Order" -> 6|> &&
    rrQEDFallback["OriginalExpression"] === rrQEDPrepared["Amplitude"] &&
    rrZero[rrQEDFallback["Amplitude"] - rrQEDFallback["Correction"] -
      rrQEDFallback["OriginalExpression"]] &&
    !FreeQ[rrQEDFallback["Amplitude"], Spinor["Xi", "Angle", 0]]];

(* A notebook kernel may still contain the older, more-specific List-only
   signature when the user reloads HEPCAT. Reloading must replace it rather
   than leave that old definition shadowing the report-aware entry point. *)
Unprotect[CompleteFactorization];
CompleteFactorization[rrLegacyInput_, rrLegacyChannels_List, rrLegacyModel_List,
    rrLegacyOptions : OptionsPattern[]] := rrLegacySentinel;
Protect[CompleteFactorization];
rrLegacyInstalled = CompleteFactorization[0, rrCubicChannels, rrCubicModel] === rrLegacySentinel;
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
rrReloadedCompletion = rrFactor[rrCubicTree, rrCubicChannels];
check["reloading clears the legacy List-only completion definition in an existing kernel",
  rrLegacyInstalled && FreeQ[DownValues[CompleteFactorization], rrLegacySentinel] &&
    rrReloadedCompletion["Status"] === "COMPLETE" &&
    rrZero[ConstructiveDiagrams`Private`residueCanonicalPropagators[
      rrReloadedCompletion["Amplitude"] - rrCubicTree, Range[4]]]];

Print[checks, " preparation-remainders checks; ", Length[failures], " failures."];
If[failures =!= {}, Print[failures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:preparation-remainders-tests.wl"];
Quit[0];
