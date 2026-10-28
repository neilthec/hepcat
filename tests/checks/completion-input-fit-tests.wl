(* Small input-derived completion proposals are hypotheses, not identities.
   Every accepted proposal must satisfy the full independent residue checks. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
Get[FileNameJoin[{root, "tests", "QED-5-point-residues.wl"}]];
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++;
  If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);
ciZero[value_] := Together[value] === 0;
ciCanonical[value_, n_Integer : 4] :=
  ConstructiveDiagrams`Private`residueCanonicalPropagators[value, Range[n]];
ciSettings[opts___] := Normal[Join[<|"Samples" -> 1, "ValidationSamples" -> 1,
  "RandomSeed" -> 11807, "TimeConstraint" -> 30, "MaxCorrectionTerms" -> 32,
  "ProgressReporting" -> False, "ReturnDetails" -> True|>, Association[{opts}]]];
ciFactor[input_, channels_, model_, opts___] := CompleteFactorization[
  input, channels, model, Sequence @@ ciSettings[opts]];
ciPass[report_] := AssociationQ[report] &&
  AllTrue[Lookup[report, "Channels", {}], #["Status"] === "PASS" &];
ciComplete[report_] := AssociationQ[report] && report["Status"] === "COMPLETE";
ciAttempts[report_] := Lookup[report, "InputFitAttempts", {}];
ciAttemptSpaces[report_] := Lookup[ciAttempts[report], "Mode", {}];

(* A real cubic scalar model supplies three independent simple poles. The
   overlapping records deliberately share exactly one signed, weighted term. *)
ciScalarModel = {{{"Scalar", "s", "s", 1, 0, ciMass, 0, 1, 1, "", ""}},
  {{"s", "s", "s", "", ciCoupling, 1}}, {}, {}};
ciScalarPairs = diagramAmplitudes[CreateDiagrams[ConstantArray["s", 4], ciScalarModel]];
ciScalarChannels = FactorizationAmplitudeChannels[ConstantArray["s", 4], ciScalarModel];
ciScalarTerms = Last /@ ciScalarPairs;
ciScalarTree = Total[ciScalarTerms];
ciExternalRules = Thread[(Mass /@ Range[4]) -> ConstantArray[ciMass, 4]];
ciRules = {ciMass -> 1, ciCoupling -> 2, ciContact -> 7};
ciPair[expression_] := {ciExternalRules, expression};
ciA = ciScalarTerms[[1]]; ciB = ciScalarTerms[[2]]; ciC = ciScalarTerms[[3]];
ciOverlapInput = ciPair /@ {ciA + ciB, ciB + ciC};
ciNormalized[input_] := ConstructiveDiagrams`Private`completionNormalizeInput[
  input, ciScalarChannels, ciScalarModel];
ciProposal[input_, opts___] := ConstructiveDiagrams`Private`completionInputProposals[
  ciNormalized[input], opts];
ciDirection[proposal_, mode_] := Lookup[SelectFirst[proposal["Proposals"],
  #["Mode"] === mode &, <||>], "Direction", Missing["NoProposal"]];
ciProposals = ciProposal[ciOverlapInput];
check["bounded proposal collection is available for ordinary diagram records",
  ciProposals["Status"] === "AVAILABLE" &&
    Lookup[ciProposals["Proposals"], "Mode"] === {"InputRescaling", "InputOverlap"}];
check["the first proposal preserves the entire pole-bearing input",
  ciZero[ciCanonical[ciDirection[ciProposals, "InputRescaling"] -
    (ciA + 2 ciB + ciC)]] && ciProposals["RegularExpression"] === 0];
check["the overlap proposal forms a union of complete signed terms across records",
  ciZero[ciCanonical[ciDirection[ciProposals, "InputOverlap"] - ciScalarTree]] &&
    Lookup[Last[ciProposals["Proposals"]], "RepeatedTermGroups"] === 1 &&
    Lookup[Last[ciProposals["Proposals"]], "RemovedCopies"] === 1];
ciOneRecord = ciProposal[{ciPair[ciA + 2 ciB + ciC]}];
check["multiplicity inside one diagram is never reinterpreted as an overlap",
  Lookup[ciOneRecord["Proposals"], "Mode"] === {"InputRescaling"} &&
    ciZero[ciCanonical[ciDirection[ciOneRecord, "InputRescaling"] - (ciA + 2 ciB + ciC)]]];
ciPlainProposals = ciProposal[ciA + 2 ciB + ciC];
check["a plain summed expression does not invent cross-diagram provenance",
  Lookup[ciPlainProposals["Proposals"], "Mode"] === {"InputRescaling"}];
ciContactProposals = ciProposal[ciPair /@ {ciA + ciB + ciContact, ciB + ciC + ciContact}];
check["repeated explicit regular terms keep their original multiplicity",
  ciContactProposals["RegularExpression"] === 2 ciContact &&
    ciZero[ciCanonical[ciDirection[ciContactProposals, "InputOverlap"] - ciScalarTree]]];
ciDistinctProposals = ciProposal[ciPair /@ {ciA + ciB, ciA + 2 ciB + ciC}];
check["overlap comparison includes the full signed coefficient rather than a stripped monomial",
  ciZero[ciCanonical[ciDirection[ciDistinctProposals, "InputOverlap"] -
    (ciA + 3 ciB + ciC)]]];
ciBoundedProposals = ciProposal[ciOverlapInput, 1];
check["an exceeded proposal collection bound is reported without truncating terms",
  ciBoundedProposals["Status"] === "UNRESOLVED" &&
    ciBoundedProposals["Proposals"] === {} && ciBoundedProposals["Diagnostics"] =!= {}];
check["a zero input supplies no nonzero input direction",
  ciProposal[0]["Proposals"] === {}];
ciSavedOverlap = ciOverlapInput; ciSavedModel = ciScalarModel;
ciOverlap = ciFactor[ciOverlapInput, ciScalarChannels, ciScalarModel,
  "ParameterRules" -> ciRules];
check["an input overlap proposal solves all three physical poles simultaneously",
  ciComplete[ciOverlap] && ciOverlap["CorrectionSpace"] === "InputOverlap" &&
    ciZero[ciCanonical[ciOverlap["Amplitude"] - ciScalarTree]]];
check["overlap repair is reported rather than presented as an algebraic identity",
  ciAttempts[ciOverlap] =!= {} && MemberQ[ciAttemptSpaces[ciOverlap], "InputOverlap"] &&
    ciPass[ciOverlap["ValidationReport"]]];
check["a rejected global fit is recorded before a successful overlap fit",
  ciAttemptSpaces[ciOverlap] === {"InputRescaling", "InputOverlap"} &&
    First[ciAttempts[ciOverlap]]["Status"] === "UNRESOLVED" &&
    Last[ciAttempts[ciOverlap]]["Status"] === "COMPLETE" &&
    Last[ciAttempts[ciOverlap]]["NormalizationFactor"] === 1];
check["the overlap shortcut does not mutate the diagram records or model",
  ciOverlapInput === ciSavedOverlap && ciScalarModel === ciSavedModel &&
    ciZero[ciCanonical[ciOverlap["OriginalExpression"] - (ciA + 2 ciB + ciC)]]];

ciDoubleBChannels = ReplacePart[ciScalarChannels,
  {2, -1} -> 2 Last[ciScalarChannels[[2]]]];
ciLegitimate = ciFactor[ciOverlapInput, ciDoubleBChannels, ciScalarModel,
  "ParameterRules" -> ciRules];
check["a repeated term is retained when the original physical amplitude already passes",
  ciComplete[ciLegitimate] && ciLegitimate["CorrectionSpace"] === "NotRequired" &&
    ciAttempts[ciLegitimate] === {} &&
    ciZero[ciCanonical[ciLegitimate["Amplitude"] - (ciA + 2 ciB + ciC)]]];

ciNegativeChannels = ReplacePart[ciScalarChannels, {2, -1} -> -Last[ciScalarChannels[[2]]]];
ciNegative = ciFactor[ciPair /@ {ciA - ciB, -ciB + ciC},
  ciNegativeChannels, ciScalarModel, "ParameterRules" -> ciRules];
check["shared negative terms retain their actual sign in an overlap candidate",
  ciComplete[ciNegative] && ciNegative["CorrectionSpace"] === "InputOverlap" &&
    ciZero[ciCanonical[ciNegative["Amplitude"] - (ciA - ciB + ciC)]]];
ciWeighted = ciFactor[ciPair /@ {ciA + 2 ciB, 2 ciB + ciC},
  ciDoubleBChannels, ciScalarModel, "ParameterRules" -> ciRules];
check["an exact repeated weighted term retains its coefficient",
  ciComplete[ciWeighted] && ciWeighted["CorrectionSpace"] === "InputOverlap" &&
    ciZero[ciCanonical[ciWeighted["Amplitude"] - (ciA + 2 ciB + ciC)]]];
ciTripleBChannels = ReplacePart[ciScalarChannels, {2, -1} -> 3 Last[ciScalarChannels[[2]]]];
ciDifferentWeights = ciFactor[ciPair /@ {ciA + ciB, 2 ciB + ciC},
  ciTripleBChannels, ciScalarModel, "ParameterRules" -> ciRules];
check["different signed or weighted terms are not treated as identical occurrences",
  ciComplete[ciDifferentWeights] && ciDifferentWeights["CorrectionSpace"] === "NotRequired" &&
    ciZero[ciCanonical[ciDifferentWeights["Amplitude"] - (ciA + 3 ciB + ciC)]]];

ciRescale = ciFactor[2 ciScalarTree + ciContact, ciScalarChannels, ciScalarModel,
  "ParameterRules" -> ciRules];
check["input rescaling changes only pole-bearing terms and preserves an explicit contact",
  ciComplete[ciRescale] && ciRescale["CorrectionSpace"] === "InputRescaling" &&
    ciZero[ciCanonical[ciRescale["Amplitude"] - ciScalarTree - ciContact]]];
check["a successful scalar input fit retains its independently checked affine family",
  ciRescale["FreeParameters"] === {} && ciRescale["RemainingBasis"] === {} &&
    ciZero[ciRescale["FamilyAmplitude"] - ciRescale["Amplitude"]] &&
    ciPass[ciRescale["ValidationReport"]]];
ciRescaleSolution = ciRescale["ConstraintData"]["LinearSolution"]["SolutionRules"];
check["input-fit coefficients reconstruct the amplitude from the retained regular seed",
  ciRescale["ConstraintData"]["FitSeed"] === ciContact && Length[ciRescaleSolution] === 1 &&
    ciZero[ciCanonical[ciRescale["ConstraintData"]["FitSeed"] +
      ((First /@ ciRescaleSolution) /. ciRescaleSolution).ciRescale["CorrectionBasis"] -
      ciRescale["Amplitude"]]]];
ciOpposite = ciFactor[-ciScalarTree, ciScalarChannels, ciScalarModel,
  "ParameterRules" -> ciRules];
check["a uniform opposite sign is fitted from physical data rather than presumed",
  ciComplete[ciOpposite] && ciOpposite["CorrectionSpace"] === "InputRescaling" &&
    ciZero[ciCanonical[ciOpposite["Amplitude"] - ciScalarTree]]];
ciZeroThirdChannels = ReplacePart[ciScalarChannels, {3, -1} -> 0];
ciArbitraryFactor = ciFactor[7 (ciA + ciB)/3, ciZeroThirdChannels, ciScalarModel,
  "ParameterRules" -> ciRules, "MaxCorrectionTerms" -> 1];
check["an arbitrary rational input normalization is derived from all supplied residues",
  ciComplete[ciArbitraryFactor] && ciArbitraryFactor["CorrectionSpace"] === "InputRescaling" &&
    Last[ciAttempts[ciArbitraryFactor]]["NormalizationFactor"] === 3/7 &&
    ciZero[ciCanonical[ciArbitraryFactor["Amplitude"] - ciA - ciB]]];
ciZeroThirdCut = ConstructiveDiagrams`Private`canonicalPhysicalCut[
  List @@ ciScalarChannels[[3, -3]], Range[4]];
ciZeroThirdRow = SelectFirst[ciArbitraryFactor["ValidationReport"]["Channels"],
  #["Cut"] === ciZeroThirdCut &, <||>];
check["a declared zero-residue channel remains in independent input-fit validation",
  Length[ciArbitraryFactor["ValidationReport"]["Channels"]] === 3 &&
    ciPass[ciArbitraryFactor["ValidationReport"]] && ciZeroThirdRow["Status"] === "PASS" &&
    AllTrue[Flatten[{ciZeroThirdRow["CalculatedResidue"], ciZeroThirdRow["ExpectedResidue"]}], ciZero]];

(* A single scalar rescaling cannot repair different wrong channel weights.
   Rejected proposals must leave the existing general reconstruction available. *)
ciNonuniform = ciFactor[ciA + 3 ciB + 2 ciC, ciScalarChannels, ciScalarModel,
  "ParameterRules" -> ciRules];
check["a failed global fit falls through to the general correction space",
  ciComplete[ciNonuniform] && ciNonuniform["CorrectionSpace"] =!= "InputRescaling" &&
    ciNonuniform["CorrectionSpace"] =!= "InputOverlap" &&
    ciZero[ciCanonical[ciNonuniform["Amplitude"] - ciScalarTree]]];
ciFromZero = ciFactor[0, ciScalarChannels, ciScalarModel, "ParameterRules" -> ciRules];
check["a zero seed still uses channel reconstruction instead of a meaningless rescaling",
  ciComplete[ciFromZero] && ciFromZero["CorrectionSpace"] =!= "InputRescaling" &&
    ciZero[ciCanonical[ciFromZero["Amplitude"] - ciScalarTree]]];
ciExplicit = ciFactor[2 ciScalarTree, ciScalarChannels, ciScalarModel,
  "ParameterRules" -> ciRules, "CorrectionBasis" -> {ciScalarTree}];
check["an explicit correction basis bypasses input-derived proposal fitting",
  ciComplete[ciExplicit] && ciAttempts[ciExplicit] === {} &&
    ciExplicit["CorrectionSpace"] === "SuppliedTerms" &&
    ciZero[ciCanonical[ciExplicit["Amplitude"] - ciScalarTree]]];
ciNoReferences = ciFactor[2 ciScalarTree, ciScalarChannels, ciScalarModel,
  "ParameterRules" -> ciRules, "ChannelReferences" -> None];
check["literal channel mode bypasses the automatic input-fit stage",
  ciComplete[ciNoReferences] && ciAttempts[ciNoReferences] === {} &&
    !MemberQ[{"InputRescaling", "InputOverlap"}, ciNoReferences["CorrectionSpace"]] &&
    ciZero[ciCanonical[ciNoReferences["Amplitude"] - ciScalarTree]]];
ciNoBasis = ciFactor[2 ciScalarTree, ciScalarChannels, ciScalarModel,
  "ParameterRules" -> ciRules, "CorrectionBasis" -> {}];
check["an explicit empty correction basis is not silently replaced by an input fit",
  AssociationQ[ciNoBasis] && ciNoBasis["Status"] === "UNRESOLVED" &&
    ciAttempts[ciNoBasis] === {} &&
    ciZero[ciCanonical[ciNoBasis["Amplitude"] - 2 ciScalarTree]]];
ciDisabled = ciFactor[2 ciScalarTree, ciScalarChannels, ciScalarModel,
  "ParameterRules" -> ciRules, "InputReweighting" -> None];
check["the user can disable input reweighting while retaining general completion",
  ciComplete[ciDisabled] && ciAttempts[ciDisabled] === {} &&
    !MemberQ[{"InputRescaling", "InputOverlap"}, ciDisabled["CorrectionSpace"]] &&
    ciZero[ciCanonical[ciDisabled["Amplitude"] - ciScalarTree]]];
check["unsupported input reweighting settings are rejected",
  Quiet[CompleteFactorization[ciScalarTree, ciScalarChannels, ciScalarModel,
    "InputReweighting" -> True]] === $Failed];

ciParameterDependent = ciFactor[(1 + ciMass) ciScalarTree,
  ciScalarChannels, ciScalarModel, "ParameterRules" -> ciRules,
  "ValidationParameterRules" -> {{ciMass -> 2, ciCoupling -> 3, ciContact -> 7}}];
check["a coefficient fitted at one mass cannot bypass additional parameter validation",
  AssociationQ[ciParameterDependent] &&
    ciParameterDependent["CorrectionSpace"] =!= "InputRescaling" &&
    (!ciComplete[ciParameterDependent] ||
      ciZero[ciCanonical[ciParameterDependent["Amplitude"] - ciScalarTree]])];
check["the parameter-dependent input-fit rejection exposes its failed held-out validation",
  AnyTrue[ciAttempts[ciParameterDependent],
    #["Mode"] === "InputRescaling" && #["Status"] === "UNRESOLVED" &&
      StringContainsQ[Lookup[#, "Reason", ""], "validation"] &&
      Length[Lookup[#, "ValidationReports", {}]] >= 2 &]];
ciTiny = ciFactor[ciOverlapInput, ciScalarChannels, ciScalarModel,
  "ParameterRules" -> ciRules, "TimeConstraint" -> 0.000001];
check["an exhausted clock never promotes an unvalidated input proposal to success",
  AssociationQ[ciTiny] && ciTiny["Status"] === "UNRESOLVED" &&
    ciZero[ciCanonical[ciTiny["Amplitude"] - (ciA + 2 ciB + ciC)]] &&
    Lookup[ciTiny, "Diagnostics", {}] =!= {}];
ciTimedProposal = Block[
  {ConstructiveDiagrams`Private`completionResidueConstraints =
    Function[Pause[1]; <|"Status" -> "UNRESOLVED"|>]},
  ConstructiveDiagrams`Private`completionFitInputProposal[
    First[ciProposals["Proposals"]], 0, ciNormalized[ciOverlapInput],
    1, 1, 11807, ciRules, {ciRules}, 0.02, Automatic, False]];
check["an interrupted input fit retains the original amplitude with an honest timeout reason",
  ciTimedProposal["Status"] === "UNRESOLVED" &&
    StringContainsQ[ciTimedProposal["Reason"], "time"] &&
    ciZero[ciCanonical[ciTimedProposal["Amplitude"] - (ciA + 2 ciB + ciC)]] &&
    ciTimedProposal["ValidationReports"] === {}];

(* Independent five-point QED examples exercise the actual six physical
   channels, including complete implicit massive-spin component checks. *)
ciQEDRules = {prElectronMass -> 1, prElectricCharge -> 1};
ciQEDOffCutZero[expression_] := AllTrue[
  Flatten[Table[prMapOnChart[expression, prDoubleChart[coords[[1]], coords[[2]], s1, s2]],
    {coords, {{1/7, 2/9}, {2/5, 3/11}}}, {s1, 2}, {s2, 2}]], ciZero];
ciQEDDoubled = ciFactor[2 prH, prChannels, prFixtureModel,
  "ParameterRules" -> ciQEDRules, "Samples" -> 2, "ValidationSamples" -> 2,
  "TimeConstraint" -> 60, "MaxCorrectionTerms" -> 1];
check["the doubled known QED amplitude is repaired by a one-direction input fit",
  ciComplete[ciQEDDoubled] && ciQEDDoubled["CorrectionSpace"] === "InputRescaling" &&
    Last[ciAttempts[ciQEDDoubled]]["NormalizationFactor"] === 1/2 &&
    ciQEDOffCutZero[ciQEDDoubled["Amplitude"] - prH]];
check["the QED rescaling succeeds before constructing channel continuations",
  Length[ciQEDDoubled["CorrectionBasis"]] === 1 &&
    ciQEDDoubled["ChannelContinuations"] === <||>];
check["the QED input fit checks every original physical channel independently",
  Length[ciQEDDoubled["ValidationReport"]["Channels"]] === 6 &&
    ciPass[ciQEDDoubled["ValidationReport"]]];
ciQEDRaw = diagramAmplitudes[CreateDiagrams[prParticles, prModel]];
ciQEDPrepared = PrepareAmplitudes[ciQEDRaw, prModel,
  "ReferenceSpinors" -> "AllPhysical", "MaxSteps" -> 0,
  "TimeConstraint" -> 20, "ReferenceTimeConstraint" -> 5,
  "ProgressReporting" -> False];
ciQEDNormalized = ConstructiveDiagrams`Private`completionNormalizeInput[
  ciQEDPrepared, prChannels, prFixtureModel];
ciQEDCanonical[value_] := ConstructiveDiagrams`Private`completionCanonicalReduce[
  value, ciQEDNormalized["ExternalMassRules"]];
check["the six actual prepared physical-reference diagrams sum algebraically to twice H",
  ciZero[ciQEDCanonical[ciQEDNormalized["Expression"] - 2 prH]]];
ciQEDProposals = ConstructiveDiagrams`Private`completionInputProposals[ciQEDNormalized];
ciQEDOverlap = SelectFirst[ciQEDProposals["Proposals"],
  #["Mode"] === "InputOverlap" &, <||>];
check["the real diagram overlap has three repeated groups and removes three duplicate copies",
  ciQEDOverlap["RepeatedTermGroups"] === 3 && ciQEDOverlap["RemovedCopies"] === 3 &&
    ciZero[ciQEDCanonical[ciQEDOverlap["Direction"] - prH]]];
ciQEDCompleted = ciFactor[ciQEDPrepared, prChannels, prFixtureModel,
  "ParameterRules" -> ciQEDRules, "Samples" -> 2, "ValidationSamples" -> 2,
  "TimeConstraint" -> 90, "MaxCorrectionTerms" -> 1];
check["six real prepared diagram records admit a validated small input completion",
  Length[ciQEDPrepared] === 6 && ciComplete[ciQEDCompleted] &&
    ciQEDCompleted["CorrectionSpace"] === "InputRescaling" &&
    Last[ciAttempts[ciQEDCompleted]]["NormalizationFactor"] === 1/2 &&
    ciQEDOffCutZero[ciQEDCompleted["Amplitude"] - prH]];
check["the actual six-diagram repair uses one direction and no channel continuation construction",
  Length[ciQEDCompleted["CorrectionBasis"]] === 1 &&
    ciQEDCompleted["ChannelContinuations"] === <||>];
check["the prepared-diagram result passes all six held-out physical residues",
  Length[ciQEDCompleted["ValidationReport"]["Channels"]] === 6 &&
    ciPass[ciQEDCompleted["ValidationReport"]]];

check["the complete input-fit regression inventory ran", checks === 43];
Print[checks, " completion-input-fit checks; ", Length[failures], " failures."];
If[failures =!= {}, Print[failures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:completion-input-fit-tests.wl"];
Quit[0];
