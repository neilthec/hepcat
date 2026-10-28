(* Reference-free sewn channels must remain available beyond five points. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
checks = 0; failures = {};
check[name_, value_] := (checks++;
  If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);

sixParticles = {"e", "E", "A.+", "A.+", "A.+", "A.+"};
sixModel = ReadModel[FileNameJoin[{root, "models"}], 3];
sixChannels = FactorizationAmplitudeChannels[sixParticles, sixModel];
sixNormalized = ConstructiveDiagrams`Private`completionNormalizeInput[0, sixChannels, sixModel];
sixRules = ConstructiveDiagrams`Private`residueParameterRules[sixModel, Automatic];
sixBasis = ConstructiveDiagrams`Private`completionReducedChannelBasis[
  sixNormalized, <|"Variants" -> {}|>, 128, sixRules, 1729];
check["six-point QED has eight three-by-five and six four-by-four channels",
  Length[sixChannels] === 14 && Counts[Length[List @@ #[[-3]]] & /@ sixChannels] ===
    <|2 -> 8, 3 -> 6|>];
check["all six reference-free four-by-four channels enter the compact correction space",
  AssociationQ[sixBasis] && Length[sixBasis["Verification"]] === 6 &&
    AllTrue[sixBasis["Verification"], Length[#["Cut"]] === 3 &&
      #["Mode"] === "ReferenceFree" && #["Verification"]["Status"] === "PASS" &]];
check["six-point compact channel sources and directions contain no reference spinors",
  AssociationQ[sixBasis] && sixBasis["Basis"] =!= {} &&
    FreeQ[{sixBasis["Sources"], sixBasis["Basis"]}, Spinor["Xi", ___]]];
check["reference-free channel directions still obey the complete correction-term cap",
  ConstructiveDiagrams`Private`completionReducedChannelBasis[
    sixNormalized, <|"Variants" -> {}|>, 1, sixRules, 1729] ===
      Missing["ReducedChannelSpaceLimit"]];
sixContinuations = ConstructiveDiagrams`Private`completionChannelContinuations[
  sixNormalized, sixModel, sixRules, 1729];
sixSpuriousVariants = Select[sixContinuations["Variants"],
  MemberQ[{"AllSpurious", "ReverseCyclic", "RotatedCyclic"}, #["Mode"]] &];
check["every six-point reference-dependent channel has three certified massless choices",
  Length[sixSpuriousVariants] === 24 &&
    AllTrue[GatherBy[sixSpuriousVariants, #["Cut"] &], Function[group,
      Length[DeleteDuplicates[Lookup[Flatten[Lookup[group, "Assignments"], 1], "Target"]]] === 3 &&
        AllTrue[group, TrueQ[#["Accepted"]] && #["Verification"]["Status"] === "PASS" &]]]];
check["new cyclic rotations retain their assignment and rotation metadata",
  Count[Lookup[sixSpuriousVariants, "Mode"], "RotatedCyclic"] === 8 &&
    AllTrue[Select[sixSpuriousVariants, #["Mode"] === "RotatedCyclic" &],
      #["Rotation"] === 1 && Length[#["Assignments"]] === 1 &]];
sixFiveParticles = {"e", "E", "A.+", "A.+", "A.+"};
sixFiveChannels = FactorizationAmplitudeChannels[sixFiveParticles, sixModel];
sixFiveNormalized = ConstructiveDiagrams`Private`completionNormalizeInput[
  0, sixFiveChannels, sixModel];
sixFiveContinuations = ConstructiveDiagrams`Private`completionChannelContinuations[
  sixFiveNormalized, sixModel, sixRules, 1729];
check["five-point continuation counts and forward-reverse modes remain unchanged",
  Counts[Lookup[sixFiveContinuations["Variants"], "Mode"]] ===
    <|"AllSpurious" -> 6, "ReverseCyclic" -> 6, "AllPhysical" -> 6|>];
(* Ordinary sandwich reductions expose 266 rather than 230 atomic directions.
   Inspect the full space here, and separately require refusal at the old cap. *)
sixExtendedBasis = ConstructiveDiagrams`Private`completionReducedChannelBasis[
  sixNormalized, sixContinuations, 512, sixRules, 1729];
sixSelectedChecks = Select[sixExtendedBasis["Verification"],
  #["Mode"] === "SelectedCutReduction" &];
check["six-point selected-cut reductions add eight grouped, certified directions",
  AssociationQ[sixExtendedBasis] && Length[sixExtendedBasis["GroupedDirections"]] === 8 &&
    Length[sixSelectedChecks] === 8 &&
    AllTrue[sixSelectedChecks, #["Verification"]["Status"] === "PASS" &]];
check["the selected-cut extension stays within the declared correction-space cap",
  AssociationQ[sixExtendedBasis] && Length[sixExtendedBasis["Basis"]] <= 512 &&
    ConstructiveDiagrams`Private`completionReducedChannelBasis[
      sixNormalized, sixContinuations, 1, sixRules, 1729] ===
        Missing["ReducedChannelSpaceLimit"]];
check["stronger ordinary reductions never silently truncate an oversized selected-cut space",
  ConstructiveDiagrams`Private`completionReducedChannelBasis[
    sixNormalized, sixContinuations, 256, sixRules, 1729] ===
      Missing["ReducedChannelSpaceLimit"]];

(* This behavior is not special to QED or photon references. *)
sixScalarModel = {{{"Scalar", "s", "s", 1, 0, sixScalarMass, 0, 1, 1, "", ""}},
  {{"s", "s", "s", "", sixScalarCoupling, 1}}, {}, {}};
sixScalarChannels = FactorizationAmplitudeChannels[ConstantArray["s", 4], sixScalarModel];
sixScalarNormalized = ConstructiveDiagrams`Private`completionNormalizeInput[
  0, sixScalarChannels, sixScalarModel];
sixScalarRules = {sixScalarMass -> 1, sixScalarCoupling -> 2};
sixScalarBasis = ConstructiveDiagrams`Private`completionReducedChannelBasis[
  sixScalarNormalized, <|"Variants" -> {}|>, 8, sixScalarRules, 1729];
check["already reference-free scalar channels receive the same compact treatment",
  AssociationQ[sixScalarBasis] && Length[sixScalarBasis["Basis"]] === 3 &&
    Length[sixScalarBasis["Verification"]] === 3 &&
    AllTrue[sixScalarBasis["Verification"], #["Verification"]["Status"] === "PASS" &]];
sixScalarTarget = First[ConstructiveDiagrams`Private`completionTargets[sixScalarNormalized["Targets"]]];
sixHigherTarget = Join[sixScalarTarget,
  <|"StateSum" -> 1/sixScalarTarget["Propagator"]|>];
sixHigherNormalized = Join[sixScalarNormalized, <|"Targets" -> {sixHigherTarget}|>];
check["reference-free sources with genuine higher-order physical poles are not admitted",
  ConstructiveDiagrams`Private`completionReducedChannelBasis[
    sixHigherNormalized, <|"Variants" -> {}|>, 8, sixScalarRules, 1729] ===
      Missing["NoVerifiedReducedChannels"]];
check["a reference-free correction source is never confused with an unknown channel",
  AssociationQ[sixScalarBasis] && AllTrue[sixScalarBasis["Verification"],
    MemberQ[Lookup[sixScalarNormalized["Targets"], "Cut"], #["Cut"]] &]];
sixScalarTargets = ConstructiveDiagrams`Private`completionTargets[sixScalarNormalized["Targets"]];
sixSelectedTarget = SelectFirst[sixScalarTargets, #["Cut"] === {1, 2} &];
sixOtherTarget = SelectFirst[sixScalarTargets, #["Cut"] === {1, 3} &];
sixTwoPoleSource = (1 + MomProd[1, 2] + MomProd[1, 3])/
  (sixSelectedTarget["Propagator"] sixOtherTarget["Propagator"]);
sixSelectedTarget = Join[sixSelectedTarget,
  <|"StateSum" -> sixTwoPoleSource sixSelectedTarget["Propagator"]|>];
sixSelectedBasis = ConstructiveDiagrams`Private`completionReducedChannelBasis[
  Join[sixScalarNormalized, <|"Targets" -> {sixSelectedTarget}|>],
  <|"Variants" -> {}|>, 16, sixScalarRules, 1729];
sixSelectedExpression = ConstructiveDiagrams`Private`withProtectedPropagators[
  sixTwoPoleSource + First[sixSelectedBasis["GroupedDirections"]], Together];
check["a selected-cut reduction never puts the other source propagators on shell",
  ConstructiveDiagrams`Private`residueExactZero[sixSelectedExpression -
    (1 - sixScalarMass^2/2 + MomProd[1, 3])/
      (sixSelectedTarget["Propagator"] sixOtherTarget["Propagator"])]];
check["selected-cut diagnostics expose only the selected internal mass rule",
  Cases[Last[sixSelectedBasis["Verification"]]["MassRules"],
    HoldPattern[Mass[p_Multiparticle] -> _] :> p] === {Multiparticle[1, 2]}];
sixSelectedAtomic = ConstructiveDiagrams`Private`completionSelectedChannelBasis[
  Join[sixScalarNormalized, <|"Targets" -> {sixSelectedTarget}|>],
  <|"Variants" -> {}|>, 16, sixScalarRules, 1729, 10];
check["selected-channel atomic terms retain the selected-only prepared source",
  AssociationQ[sixSelectedAtomic] && Length[sixSelectedAtomic["Basis"]] > 1 &&
    ConstructiveDiagrams`Private`residueExactZero[First[sixSelectedAtomic["Sources"]] -
      (1 - sixScalarMass^2/2 + MomProd[1, 3])/
        (sixSelectedTarget["Propagator"] sixOtherTarget["Propagator"])]];
check["atomic selected-channel sources retain their selected residue certificate",
  AllTrue[sixSelectedAtomic["Verification"], #["Verification"]["Status"] === "PASS" &&
    Cases[#["MassRules"], HoldPattern[Mass[p_Multiparticle] -> _] :> p] ===
      {Multiparticle[1, 2]} &]];
check["selected-channel atomic construction never silently truncates an oversized space",
  FailureQ[ConstructiveDiagrams`Private`completionSelectedChannelBasis[
    Join[sixScalarNormalized, <|"Targets" -> {sixSelectedTarget}|>],
    <|"Variants" -> {}|>, 1, sixScalarRules, 1729, 10]]];

(* Additive mapping uses the same exact restriction, only on smaller sums. *)
sixAdditiveChart = ConstructiveDiagrams`Private`completionCutChart[
  sixScalarTarget, 1, 1729, sixScalarRules];
sixAdditivePole = sixScalarTarget["Propagator"];
sixAdditiveComponents = {0, 1/sixAdditivePole + MomProd[1, 3]/sixAdditivePole,
  3/sixAdditivePole};
sixAdditiveResult = ConstructiveDiagrams`Private`completionLinearResidue[
  sixAdditiveComponents, {sixAdditiveA, sixAdditiveB}, sixScalarTarget,
  sixScalarTargets, sixAdditiveChart, 10];
sixWholeResult = ConstructiveDiagrams`Private`completionLinearResidue[
  sixAdditiveComponents, {sixAdditiveA, sixAdditiveB}, sixScalarTarget,
  sixScalarTargets, KeyDrop[sixAdditiveChart, "AutomaticCoherentChart"], 10];
check["automatic charts mark the guaranteed additive kinematic map",
  TrueQ[sixAdditiveChart["AutomaticCoherentChart"]]];
check["additive mapping matches whole-expression residues with coherent spin metadata",
  sixAdditiveResult["Status"] === "PASS" && sixWholeResult["Status"] === "PASS" &&
    ConstructiveDiagrams`Private`residueExactZero[
      sixAdditiveResult["PhysicalResidue"] - sixWholeResult["PhysicalResidue"]] &&
    TrueQ[sixAdditiveResult["KinematicChecks"]["Valid"]]];
sixAdditiveInvariant = Mandelstahm @@ sixScalarTarget["Cut"];
sixCancelledHigherPoles = Expand[
  (sixAdditiveInvariant - sixScalarMass^2)/sixAdditivePole^2];
sixCancellationResult = ConstructiveDiagrams`Private`completionLinearResidue[
  {0, sixCancelledHigherPoles}, {sixAdditiveA}, sixScalarTarget,
  sixScalarTargets, sixAdditiveChart, 10];
check["singular summands fall back to the whole component and retain pole cancellations",
  sixCancellationResult["Status"] === "PASS" &&
    sixCancellationResult["PhysicalResidue"] === sixAdditiveA];
check["a hidden inverse-invariant higher pole cannot be hidden by a zero fitted coefficient",
  ConstructiveDiagrams`Private`completionLinearResidue[
    {0, 1/sixAdditivePole + 1/(sixAdditiveInvariant - sixScalarMass^2)^2},
    {sixAdditiveA}, sixScalarTarget, sixScalarTargets, sixAdditiveChart, 10]["Status"] ===
      "UNRESOLVED"];
sixMasslessScalarModel = ReplacePart[sixScalarModel, {1, 1, 6} -> 0];
sixMasslessChannels = FactorizationAmplitudeChannels[ConstantArray["s", 4], sixMasslessScalarModel];
sixMasslessNormalized = ConstructiveDiagrams`Private`completionNormalizeInput[
  0, sixMasslessChannels, sixMasslessScalarModel];
sixMasslessTargets = ConstructiveDiagrams`Private`completionTargets[sixMasslessNormalized["Targets"]];
sixMasslessTarget = SelectFirst[sixMasslessTargets, #["Cut"] === {1, 2} &];
sixMasslessChart = ConstructiveDiagrams`Private`completionCutChart[
  sixMasslessTarget, 1, 1729, {sixScalarCoupling -> 2}];
sixVanishingBracket = SelectFirst[
  (SpinorChain[Spinor["Helicity", #, 1], Spinor["Helicity", #, 2]] & /@
    {"Angle", "Square"}), Function[bracket,
      ConstructiveDiagrams`Private`residueExactZero[
        ConstructiveDiagrams`Private`residueRegularRestriction[
          sixMasslessChart["Map"][bracket, {sixMasslessTarget}, {sixBracketVariable}],
          {sixBracketVariable}]]]];
check["an inverse bracket singular at the cut still undergoes full mapped regularity checks",
  ConstructiveDiagrams`Private`completionLinearResidue[
    {0, 1/(sixMasslessTarget["Propagator"] sixVanishingBracket)}, {sixAdditiveA},
    sixMasslessTarget, sixMasslessTargets, sixMasslessChart, 10]["Status"] === "UNRESOLVED"];
check["exact numeric cache normalization preserves scaled singular Missing results",
  ConstructiveDiagrams`Private`completionLinearResidue[
    {0, 3/sixAdditivePole^2}, {sixAdditiveA}, sixScalarTarget,
    sixScalarTargets, sixAdditiveChart, 10]["Status"] === "UNRESOLVED"];
sixCustomExpression = 1/sixAdditivePole + MomProd[1, 3]/sixAdditivePole;
sixCustomMap = With[{original = sixAdditiveChart["Map"], special = sixCustomExpression},
  Function[{expression, channels, poles}, original[expression, channels, poles] +
    If[expression === special, 1/First[poles], 0]]];
sixCustomChart = Join[KeyDrop[sixAdditiveChart, "AutomaticCoherentChart"],
  <|"Map" -> sixCustomMap|>];
sixCustomResult = ConstructiveDiagrams`Private`completionLinearResidue[
  {sixCustomExpression}, {}, sixScalarTarget, sixScalarTargets, sixCustomChart, 10];
check["custom maps retain whole-expression behavior without assuming additivity",
  sixCustomResult["Status"] === "PASS" &&
    ConstructiveDiagrams`Private`residueExactZero[sixCustomResult["PhysicalResidue"] -
      sixWholeResult["PhysicalResidue"] /. {sixAdditiveA -> 1, sixAdditiveB -> 0}] === False &&
    ConstructiveDiagrams`Private`residueExactZero[sixCustomResult["PhysicalResidue"] - 1 -
      (sixWholeResult["PhysicalResidue"] /. {sixAdditiveA -> 1, sixAdditiveB -> 0})]];

sixFrozenEvaluator = ConstructiveDiagrams`Private`completionSimpleCutEvaluator[
  sixScalarTarget, sixAdditiveChart];
check["automatic coherent charts retain their model-local parameter rules",
  sixAdditiveChart["ParameterRules"] === sixScalarRules];
check["certified rational simple poles use frozen automatic restrictions",
  sixAdditiveResult["ComponentRestrictionMethods"] ===
    ConstantArray["FrozenAutomaticSimplePole", Length[sixAdditiveComponents]]];
check["the selected complement is canonicalized to the same simple pole",
  sixFrozenEvaluator[1/PropDen[Mom[Multiparticle @@ sixScalarTarget["Complement"]],
    sixScalarTarget["Mass"]]] === 1];
check["a hidden invariant pole declines the frozen shortcut before cancellation",
  MissingQ[sixFrozenEvaluator[1/(sixAdditiveInvariant - sixScalarMass^2)^2]] &&
    sixCancellationResult["ComponentRestrictionMethods"][[2]] === "ExactMappedRestriction"];
check["doubled explicit poles and unsupported functions decline the frozen shortcut",
  MissingQ[sixFrozenEvaluator[1/sixAdditivePole^2]] &&
    MissingQ[sixFrozenEvaluator[Sin[MomProd[1, 3]]/sixAdditivePole]]];
check["unassigned symbolic couplings retain the ordinary exact mapping",
  MissingQ[sixFrozenEvaluator[sixUnassignedCoupling/sixAdditivePole]]];
sixMasslessFrozenEvaluator = ConstructiveDiagrams`Private`completionSimpleCutEvaluator[
  sixMasslessTarget, sixMasslessChart];
check["a hidden inverse bracket pole declines the frozen shortcut",
  MissingQ[sixMasslessFrozenEvaluator[
    1/(sixMasslessTarget["Propagator"] sixVanishingBracket)]]];
check["custom maps never enter the frozen automatic evaluator",
  MissingQ[ConstructiveDiagrams`Private`completionSimpleCutEvaluator[
    sixScalarTarget, sixCustomChart]] &&
    sixCustomResult["ComponentRestrictionMethods"] === {"WholeCustomMap"}];

(* Implicit references and surviving internal spinors are not off-cut sources. *)
sixFourFermionParts = {"e", "E", "E", "e", "A.+"};
sixFourFermionChannels = FactorizationAmplitudeChannels[sixFourFermionParts, sixModel];
sixFourFermionNormalized = ConstructiveDiagrams`Private`completionNormalizeInput[
  0, sixFourFermionChannels, sixModel];
sixFourFermionTargets = ConstructiveDiagrams`Private`completionTargets[
  sixFourFermionNormalized["Targets"]];
sixCutOnlyTargets = Select[sixFourFermionTargets,
  FreeQ[#["StateSum"], Spinor["Xi", ___]] &];
check["the four-fermion photon channels retain implicit references and cut-only spinors",
  Length[sixCutOnlyTargets] === 4 && AllTrue[sixCutOnlyTargets,
    !FreeQ[#["StateSum"], _xFactor | _xTildeFactor] &&
      !FreeQ[#["StateSum"], Spinor["Helicity", _, _Multiparticle]] &]];
check["cut-only four-fermion photon channels do not enter the reference-free compact space",
  ConstructiveDiagrams`Private`completionReducedChannelBasis[
    sixFourFermionNormalized, <|"Variants" -> {}|>, 128, sixRules, 1729] ===
      Missing["NoReducedPhysicalChannels"]];
check["cut-only four-fermion sources are also excluded from selected-channel atomics",
  ConstructiveDiagrams`Private`completionSelectedChannelBasis[
    sixFourFermionNormalized, <|"Variants" -> {}|>, 128, sixRules, 1729, 10] ===
      Missing["NoSelectedChannelSources"]];
sixContextTarget = First[sixCutOnlyTargets];
sixContextChart = ConstructiveDiagrams`Private`completionCutChart[
  sixContextTarget, 1, 1729, sixRules];
sixContextCut = Multiparticle @@ sixContextTarget["Cut"];
sixContextExpression = Expand[(
  (xFactor @@ sixContextTarget["Cut"]) +
    SpinorChain[Spinor["Helicity", "Angle", sixContextCut],
      Spinor["Helicity", "Angle", 5]])/sixContextTarget["Propagator"]];
sixWholeContextMapped = False; sixMissingContextMapped = False;
sixContextMap = With[{original = sixContextChart["Map"]},
  Function[{expression, channels, poles},
    If[!FreeQ[expression, _xFactor | _xTildeFactor],
      If[FreeQ[expression, Spinor["Helicity", _, _Multiparticle, ___]],
        sixMissingContextMapped = True, sixWholeContextMapped = True]];
    original[expression, channels, poles]]];
sixContextResult = ConstructiveDiagrams`Private`completionLinearResidue[
  {0, sixContextExpression}, {sixAdditiveA}, sixContextTarget,
  sixFourFermionTargets, Join[sixContextChart, <|"Map" -> sixContextMap|>], 10];
check["implicit x factors retain the whole internal-spinor inventory across addends",
  sixContextResult["Status"] === "PASS" && sixWholeContextMapped &&
    !sixMissingContextMapped];
sixCutOnlyTerms = {xFactor[1, 2], xTildeFactor[1, 2],
  Spinor["Helicity", "Angle", Multiparticle[1, 2]],
  Spinor["Spin", "Upper", "Square", Multiparticle[1, 2], 1/2],
  SpinorHat["Helicity", "Square", Multiparticle[1, 2]]};
check["all supported internal spinor shapes and implicit x references are excluded",
  AllTrue[sixCutOnlyTerms, Function[term,
    ConstructiveDiagrams`Private`completionReducedChannelBasis[
      Join[sixScalarNormalized, <|"Targets" -> {
        Join[sixScalarTarget, <|"StateSum" -> term|>]}|>],
      <|"Variants" -> {}|>, 128, sixScalarRules, 1729] ===
        Missing["NoReducedPhysicalChannels"]]]];

(* Ordinary failures name the actionable cause without hiding an amplitude in them. *)
sixLimitReason = "The channel-derived correction space exceeds MaxCorrectionTerms.";
sixLimitReport = <|"Status" -> "UNRESOLVED", "Amplitude" -> sixUncheckedAmplitude,
  "Diagnostics" -> {<|"Stage" -> "Correction space"|>, sixLimitReason,
    "Additional diagnostic information."}|>;
sixLimitFailure = ConstructiveDiagrams`Private`completionFormat[
  sixLimitReport, "Expression", False];
check["ordinary completion failure exposes the first diagnostic string as its reason",
  FailureQ[sixLimitFailure] && sixLimitFailure[[2]]["Reason"] === sixLimitReason &&
    StringContainsQ[sixLimitFailure[[2]]["MessageTemplate"], sixLimitReason]];
check["ordinary completion failures contain neither a candidate amplitude nor a full report",
  !KeyExistsQ[sixLimitFailure[[2]], "Amplitude"] &&
    !KeyExistsQ[sixLimitFailure[[2]], "Report"] &&
    FreeQ[sixLimitFailure, sixUncheckedAmplitude]];
check["detailed completion formatting still returns the supplied report unchanged",
  ConstructiveDiagrams`Private`completionFormat[sixLimitReport, "Details", False] ===
    sixLimitReport];
sixRetainedFailure = ConstructiveDiagrams`Private`completionFormat[
  sixLimitReport, "Expression", True];
check["the existing retained-report failure format is unchanged",
  sixRetainedFailure === Failure["IncompleteAmplitude", <|"MessageTemplate" ->
    "Amplitude completion did not establish all required constraints. Inspect the retained report.",
    "Amplitude" -> sixUncheckedAmplitude, "Report" -> sixLimitReport|>]];
check["successful completion still returns only its amplitude",
  ConstructiveDiagrams`Private`completionFormat[
    Join[sixLimitReport, <|"Status" -> "COMPLETE"|>], "Expression", False] ===
      sixUncheckedAmplitude];
sixNoReasonFailure = ConstructiveDiagrams`Private`completionFormat[
  KeyDrop[sixLimitReport, "Diagnostics"], "Expression", False];
check["a failure without diagnostic strings retains a useful generic message",
  FailureQ[sixNoReasonFailure] && sixNoReasonFailure[[2]]["Reason"] ===
    "Factorization completion did not establish agreement with every channel." &&
    StringContainsQ[sixNoReasonFailure[[2]]["MessageTemplate"], "Request ReturnDetails"]];

sixScalarProgress = {};
sixScalarCompletion = CompleteFactorization[0, sixScalarChannels, sixScalarModel,
  "ParameterRules" -> sixScalarRules, "Samples" -> 1, "ValidationSamples" -> 1,
  "MaxCorrectionTerms" -> 8, "TimeConstraint" -> 30, "ReturnDetails" -> True,
  "ProgressFunction" -> (AppendTo[sixScalarProgress, #] &)];
check["reference-free channels complete a generic scalar amplitude end to end",
  sixScalarCompletion["Status"] === "COMPLETE" &&
    sixScalarCompletion["CorrectionSpace"] === "ReducedChannels" &&
    AllTrue[sixScalarCompletion["ValidationReport"]["Channels"], #["Status"] === "PASS" &]];
check["progress distinguishes exact solving from cut equation construction",
  AnyTrue[sixScalarProgress, Lookup[#, "Stage", None] === "Solving factorization" &&
    Lookup[#, "EquationCount", 0] > 0 && Lookup[#, "Unknowns", 0] === 3 &]];

(* Bose symmetry is an explicit state declaration, never inferred from masses. *)
sixBoseRules = Join[{Mass[1] -> sixScalarMass, Mass[2] -> sixScalarMass},
  Table[Mass[i] -> 0, {i, 3, 8}]];
sixBosePlan = ConstructiveDiagrams`Private`completionBosePlan[{{3, 4, 5, 6}}, sixBoseRules];
sixBoseEmpty = ConstructiveDiagrams`Private`completionBosePlan[{}, sixBoseRules];
sixBoseTwo = ConstructiveDiagrams`Private`completionBosePlan[{{3, 4}}, sixBoseRules];
sixBoseProduct = ConstructiveDiagrams`Private`completionBosePlan[{{3, 4}, {5, 6}}, sixBoseRules];
sixBoseProject[expr_, plan_:sixBosePlan] :=
  ConstructiveDiagrams`Private`completionBoseProject[expr, plan];
sixBoseCycle[expr_, labels_] := ConstructiveDiagrams`Private`completionBoseCycle[expr, labels];
check["empty Bose grouping is the identity",
  sixBoseEmpty["Order"] === 1 && sixBoseProject[Mom[3], sixBoseEmpty] === Mom[3]];
check["four declared identical bosons give exactly twenty-four typed permutations",
  sixBosePlan["Order"] === 24 && Length[DeleteDuplicates[sixBosePlan["Permutations"]]] === 24];
sixMixedParticles = {"e", "E", "A.+", "A.+", "A.+", "A.-"};
sixMixedChannels = FactorizationAmplitudeChannels[sixMixedParticles, sixModel];
sixMixedBosePlan = ConstructiveDiagrams`Private`completionBosePlan[{{3, 4, 5}}, sixBoseRules];
check["three identical positive-helicity photons give exactly six declared permutations",
  sixMixedBosePlan["Groups"] === {{3, 4, 5}} && sixMixedBosePlan["Order"] === 6 &&
    Length[DeleteDuplicates[sixMixedBosePlan["Permutations"]]] === 6];
sixMixedBoseState = {Mom[3], Mom[4], Mom[5], Mom[6],
  Spinor["Helicity", "Angle", 6], Spinor["Helicity", "Square", 6]};
sixMixedBoseImages = Fold[ConstructiveDiagrams`Private`completionBoseCycle,
  sixMixedBoseState, #] & /@ sixMixedBosePlan["Permutations"];
check["every mixed-helicity permutation fixes the negative-helicity leg while averaging the other three",
  AllTrue[sixMixedBoseImages, Sort[Take[#, 3]] === (Mom /@ Range[3, 5]) &&
    Drop[#, 3] === Drop[sixMixedBoseState, 3] &] &&
    And @@ MapThread[Expand[#1 - #2] === 0 &,
      {Take[Total[sixMixedBoseImages]/6, 3],
        ConstantArray[Total[Mom /@ Range[3, 5]]/3, 3]}]];
check["the mixed six-point QED model supplies all fourteen factorization channels",
  Length[sixMixedChannels] === 14];
check["equal photon masses do not infer an opposite-helicity Bose grouping",
  (Mass /@ Range[3, 6] /. sixBoseRules) === {0, 0, 0, 0} &&
    ("BoseSymmetryGroups" /. Options[CompleteFactorization]) === {} &&
    sixBoseEmpty["Groups"] === {} && sixBoseEmpty["Order"] === 1 &&
    ConstructiveDiagrams`Private`completionBoseProject[sixMixedBoseState, sixBoseEmpty] ===
      sixMixedBoseState];
check["mixed six-point channels retain eight three-by-five and six four-by-four sewings",
  Length[sixMixedChannels] === 14 &&
    Counts[Length[List @@ #[[-3]]] & /@ sixMixedChannels] === <|2 -> 8, 3 -> 6|>];
check["changing one photon helicity does not change the external mass inventory",
  AllTrue[sixMixedChannels, Take[#, 6] === Take[First[sixChannels], 6] &]];
check["all six mixed four-by-four sewings are reference-free before continuation",
  Length[Select[sixMixedChannels, Length[List @@ #[[-3]]] === 3 &]] === 6 &&
    FreeQ[Select[sixMixedChannels, Length[List @@ #[[-3]]] === 3 &], Spinor["Xi", ___]]];
check["Bose averaging preserves constants and has exact normalization",
  sixBoseProject[7] === 7 &&
    Expand[sixBoseProject[Mom[3]] - Total[Mom /@ Range[3, 6]]/4] === 0];
check["disjoint Bose groups use the product group",
  sixBoseProduct["Order"] === 4 &&
    Expand[sixBoseProject[Mom[3] Mom[5], sixBoseProduct] -
      (Mom[3] + Mom[4]) (Mom[5] + Mom[6])/4] === 0];
sixBoseExpression = Mom[3]/PropDen[Mom[Multiparticle[1, 4]], sixScalarMass] +
  3 Mom[5] MomProd[1, 6];
sixBoseAveraged = sixBoseProject[sixBoseExpression];
check["Bose projection is idempotent and invariant under generating swaps",
  sixBoseProject[sixBoseAveraged] === sixBoseAveraged &&
    AllTrue[{{3, 4}, {4, 5}, {5, 6}}, sixBoseCycle[sixBoseAveraged, #] === sixBoseAveraged &]];
check["antisymmetric directions vanish under the declared bosonic swap",
  sixBoseProject[Mom[3] - Mom[4], sixBoseTwo] === 0];
check["Bose permutations preserve literal integers, Xi IDs and spin projection numbers",
  sixBoseCycle[{3, Spinor["Xi", "Angle", 3],
    Spinor["Spin", "Upper", "Angle", 3, 4], sixUntyped[3, 4],
    Spinor["Helicity", "Square", 3]}, {3, 4}] ===
    {3, Spinor["Xi", "Angle", 3], Spinor["Spin", "Upper", "Angle", 4, 4],
      sixUntyped[3, 4], Spinor["Helicity", "Square", 4]}];
check["Bose permutations relabel only the leg field of massive-vector spin slots",
  sixBoseCycle[Spinor["Spin", "Upper", "Angle", 3, VectorSpinIndex[3, 1]], {3, 4}] ===
    Spinor["Spin", "Upper", "Angle", 4, VectorSpinIndex[4, 1]]];
check["Bose permutations preserve hatted reference IDs",
  sixBoseCycle[{SpinorHat["Xi", "Angle", 3],
    SpinorHat["Spin", "Upper", "Square", 3, 1/2]}, {3, 4}] ===
    {SpinorHat["Xi", "Angle", 3], SpinorHat["Spin", "Upper", "Square", 4, 1/2]}];
check["Bose permutations relabel composite momenta without shell substitutions",
  sixBoseCycle[{Mom[Multiparticle[1, 3]], MomHat[Multiparticle[2, 4]], MomQ[3],
    1/PropDen[Mom[Multiparticle[1, 3]], sixScalarMass]}, {3, 4}] ===
    {Mom[Multiparticle[1, 4]], MomHat[Multiparticle[2, 3]], MomQ[4],
      1/PropDen[Mom[Multiparticle[1, 4]], sixScalarMass]}];
check["invalid, overlapping, undeclared or unequal-mass Bose groups are rejected",
  AllTrue[{{{3, 4}, {4, 5}}, {{3, 3}}, {{3}}, {{3, 9}}, {{1, 3}},
    {{3, sixNonintegerLabel}}, Automatic},
    FailureQ[ConstructiveDiagrams`Private`completionBosePlan[#, sixBoseRules]] &]];
check["Bose group enumeration is bounded before constructing permutations",
  ConstructiveDiagrams`Private`completionBosePlan[{{3, 4, 5, 6, 7}}, sixBoseRules]["Order"] === 120 &&
    MatchQ[ConstructiveDiagrams`Private`completionBosePlan[
      {{3, 4, 5, 6, 7, 8}}, sixBoseRules], Failure["BoseSymmetryLimit", _Association]]];
sixBoseScalar = CompleteFactorization[0, sixScalarChannels, sixScalarModel,
  "BoseSymmetryGroups" -> {{1, 2, 3, 4}}, "ParameterRules" -> sixScalarRules,
  "Samples" -> 1, "ValidationSamples" -> 1, "MaxCorrectionTerms" -> 1,
  "TimeConstraint" -> 30, "ProgressReporting" -> False, "ReturnDetails" -> True];
check["the correction cap applies after Bose projection, with every target still validated",
  sixBoseScalar["Status"] === "COMPLETE" && Length[sixBoseScalar["CorrectionBasis"]] === 1 &&
    Length[sixBoseScalar["ValidationReport"]["Channels"]] === 3 &&
    AllTrue[sixBoseScalar["ValidationReport"]["Channels"], #["Status"] === "PASS" &]];
check["Bose completion retains the original input and its complete correction",
  sixBoseScalar["OriginalExpression"] === 0 &&
    sixBoseScalar["BoseSymmetry"] === <|"Groups" -> {{1, 2, 3, 4}}, "Order" -> 24|> &&
    sixBoseScalar["Amplitude"] === sixBoseScalar["Correction"]];
sixBoseSeed = sixScalarCompletion["Amplitude"] + MomProd[1, 2] - MomProd[1, 3];
sixBoseFastPath = CompleteFactorization[sixBoseSeed, sixScalarChannels, sixScalarModel,
  "BoseSymmetryGroups" -> {{1, 2, 3, 4}}, "ParameterRules" -> sixScalarRules,
  "Samples" -> 1, "ValidationSamples" -> 1, "TimeConstraint" -> 30,
  "ProgressReporting" -> False, "ReturnDetails" -> True];
check["Bose averaging also acts on an already residue-correct seed before the fast path",
  sixBoseFastPath["Status"] === "COMPLETE" &&
    sixBoseFastPath["OriginalExpression"] === sixBoseSeed &&
    ConstructiveDiagrams`Private`residueExactZero[
      sixBoseFastPath["Amplitude"] - sixScalarCompletion["Amplitude"]]];

(* Keep the full automatic span when it fits; group whole input diagrams only
   as an explicit, independently validated alternative to an exceeded cap. *)
sixBudgetMassRules = Table[Mass[i] -> sixScalarMass, {i, 4}];
sixBudgetAtoms = Table[MomProd[1, 2]^power/
  PropDen[Mom[Multiparticle[1, 2]], sixScalarMass], {power, 1, 4}];
sixBudgetInput = {{sixBudgetMassRules, Total[sixBudgetAtoms]}};
sixBudgetPlan = ConstructiveDiagrams`Private`completionBosePlan[
  {{1, 2, 3, 4}}, sixBudgetMassRules];
sixBudgetProject[expression_] := Expand[ConstructiveDiagrams`Private`canonicalChains[
  ConstructiveDiagrams`Private`residueCanonicalPropagators[
    ConstructiveDiagrams`Private`completionBoseProject[expression, sixBudgetPlan], Range[4]]]];
sixBudgetChannelAtoms = ConstructiveDiagrams`Private`completionAutomaticBasis[
  sixScalarNormalized, 16, None];
sixBudgetContains[basis_List, directions_List] := Length[
  ConstructiveDiagrams`Private`completionIndependentBasis[
    ConstructiveDiagrams`Private`completionNormalizeBasis[Join[basis, directions]]]] === Length[basis];
sixBudgetReports = Association@Table[cap -> CompleteFactorization[
  sixBudgetInput, sixScalarChannels, sixScalarModel,
  "BoseSymmetryGroups" -> {{1, 2, 3, 4}}, "ParameterRules" -> sixScalarRules,
  "ValidationParameterRules" -> {sixScalarRules,
    {sixScalarMass -> 2, sixScalarCoupling -> 2}},
  "Samples" -> 3, "ValidationSamples" -> 1, "MaxRefinements" -> 3,
  "MaxCorrectionTerms" -> cap, "TimeConstraint" -> 45,
  "ProgressReporting" -> False, "ReturnDetails" -> True], {cap, {8, 2, 1}}];
sixBudgetFull = sixBudgetReports[8];
sixBudgetGrouped = sixBudgetReports[2];
sixBudgetTooSmall = sixBudgetReports[1];
check["a fitting full Bose correction span is not replaced by the grouped budget alternative",
  sixBudgetFull["Status"] === "COMPLETE" &&
    sixBudgetFull["CorrectionSpace"] === "BoseSelectedChannelUnion" &&
    Length[sixBudgetFull["CorrectionBasis"]] === 5 &&
    sixBudgetFull["CorrectionSpaceCandidates"] === {}];
check["the full Bose correction span retains every input and channel atom",
  sixBudgetContains[sixBudgetFull["CorrectionBasis"],
    sixBudgetProject /@ Join[sixBudgetAtoms, sixBudgetChannelAtoms]]];
check["an exceeded full space tries a named grouped input space without silent truncation",
  sixBudgetGrouped["Status"] === "COMPLETE" &&
    sixBudgetGrouped["CorrectionSpace"] === "BoseGroupedInputChannelUnion" &&
    Length[sixBudgetGrouped["CorrectionBasis"]] === 2 &&
    sixBudgetGrouped["CorrectionSpaceCandidates"] === {
      <|"CorrectionSpace" -> "BoseSelectedChannelUnion", "Terms" -> 5, "WithinLimit" -> False|>,
      <|"CorrectionSpace" -> "BoseGroupedInputChannelUnion", "Terms" -> 2, "WithinLimit" -> True|>}];
check["grouped budget directions retain the whole input diagram and every channel atom",
  sixBudgetContains[sixBudgetGrouped["CorrectionBasis"],
    sixBudgetProject /@ Join[{Total[sixBudgetAtoms]}, sixBudgetChannelAtoms]] &&
    sixBudgetGrouped["OriginalExpression"] === Total[sixBudgetAtoms]];
sixBudgetLastRound = Max[Lookup[sixBudgetGrouped["ValidationReports"], "Round", -1]];
sixBudgetFinalReports = Select[sixBudgetGrouped["ValidationReports"],
  Lookup[#, "Round", -1] === sixBudgetLastRound &];
check["grouped budget completion validates every channel at both declared mass scales",
  Length[sixBudgetFinalReports] === 2 &&
    Length[DeleteDuplicates[Lookup[sixBudgetFinalReports, "ParameterRules"]]] === 2 &&
    AllTrue[sixBudgetFinalReports, Function[entry,
      Length[entry["Report"]["Channels"]] === 3 &&
        AllTrue[entry["Report"]["Channels"], #["Status"] === "PASS" &]]]];
check["independent mass validation cancels the full grouped scalar contamination",
  ConstructiveDiagrams`Private`residueExactZero[
    sixBudgetGrouped["Amplitude"] - sixScalarCompletion["Amplitude"]]];
check["a still-oversized grouped space remains unresolved with both exact sizes recorded",
  sixBudgetTooSmall["Status"] === "UNRESOLVED" &&
    sixBudgetTooSmall["CorrectionSpaceCandidates"] === {
      <|"CorrectionSpace" -> "BoseSelectedChannelUnion", "Terms" -> 5, "WithinLimit" -> False|>,
      <|"CorrectionSpace" -> "BoseGroupedInputChannelUnion", "Terms" -> 2, "WithinLimit" -> False|>} &&
    sixBudgetTooSmall["RefinementHistory"] === {} &&
    sixBudgetTooSmall["ConstraintData"]["LinearSolution"] === <||>];
sixBudgetOrdinaryFailure = ConstructiveDiagrams`Private`completionFormat[
  sixBudgetTooSmall, "Expression", False];
check["an oversized budget alternative cannot return an unchecked ordinary amplitude",
  FailureQ[sixBudgetOrdinaryFailure] &&
    !KeyExistsQ[sixBudgetOrdinaryFailure[[2]], "Amplitude"] &&
    StringContainsQ[sixBudgetOrdinaryFailure[[2]]["MessageTemplate"], "MaxCorrectionTerms"]];

(* Pruning changes coordinates, never the formal correction span. *)
sixPruneBasis = {sixPruneA (sixPruneX + sixPruneY),
  sixPruneA (sixPruneX - sixPruneY), sixPruneA sixPruneX};
check["Gaussian pruning keeps the first typed factored independent directions",
  ConstructiveDiagrams`Private`completionIndependentBasis[sixPruneBasis] === Take[sixPruneBasis, 2]];
check["exact complex multiples use the sparse Gaussian path",
  ConstructiveDiagrams`Private`completionIndependentBasis[
    {I sixPruneX + sixPruneY, 2 I sixPruneX + 2 sixPruneY, sixPruneX - sixPruneY}] ===
      {I sixPruneX + sixPruneY, sixPruneX - sixPruneY}];
check["zero and repeated directions retain deterministic first representatives",
  ConstructiveDiagrams`Private`completionIndependentBasis[
    {0, sixPruneX, sixPruneX, 2 sixPruneX, sixPruneX + sixPruneY, sixPruneY, 0}] ===
      {sixPruneX, sixPruneX + sixPruneY}];
check["approximate coefficients retain the complete unpruned input",
  ConstructiveDiagrams`Private`completionIndependentBasis[{sixPruneX, 1. sixPruneX}] ===
    {sixPruneX, 1. sixPruneX}];
check["unsupported exact numeric coefficient fields retain the legacy fallback",
  ConstructiveDiagrams`Private`completionGaussianIndependentBasis[
    {Sqrt[2] sixPruneX, sixPruneX}]["Mode"] === "LegacyExactFieldFallback" &&
  ConstructiveDiagrams`Private`completionIndependentBasis[
    {Sqrt[2] sixPruneX, sixPruneX}] === {Sqrt[2] sixPruneX}];
check["formal rational-denominator keys keep the legacy cancellation convention",
  ConstructiveDiagrams`Private`completionIndependentBasis[
    {sixPruneX/(sixPruneA - sixPruneB)^2,
      sixPruneX/(sixPruneA^2 - 2 sixPruneA sixPruneB + sixPruneB^2), sixPruneY}] ===
    {sixPruneX/(sixPruneA - sixPruneB)^2, sixPruneY}];
check["an infinite budget supports both exact coefficient paths",
  ConstructiveDiagrams`Private`completionIndependentBasis[{sixPruneX, 2 sixPruneX}, Infinity] === {sixPruneX} &&
  ConstructiveDiagrams`Private`completionIndependentBasis[{Sqrt[2] sixPruneX, sixPruneX}, Infinity] === {Sqrt[2] sixPruneX}];
check["a nonpositive pruning budget retains every direction",
  ConstructiveDiagrams`Private`completionIndependentBasis[sixPruneBasis, 0] === sixPruneBasis &&
  ConstructiveDiagrams`Private`completionIndependentBasis[sixPruneBasis, -1] === sixPruneBasis];
sixPruneTimed = Block[{ConstructiveDiagrams`Private`completionGaussianRationalQ},
  ConstructiveDiagrams`Private`completionGaussianRationalQ[value_] := (Pause[0.03]; True);
  ConstructiveDiagrams`Private`completionIndependentBasis[{sixPruneX, sixPruneY}, 0.04]];
check["a timeout discards any partial independent subset atomically",
  sixPruneTimed === {sixPruneX, sixPruneY}];
check["formal pruning never substitutes a physical momentum-product identity",
  Length[ConstructiveDiagrams`Private`completionIndependentBasis[
    {MomProd[1, 2], (Mandelstahm[1, 2] - Mass[1]^2 - Mass[2]^2)/2}]] === 2];
check["all-zero input directions have the established empty-span result",
  ConstructiveDiagrams`Private`completionIndependentBasis[{0, 0}] === {}];
(* The balanced-helicity case is an explicit lower-amplitude recursion example,
   not a claim that the bounded automatic correction space is universal. *)
Get[FileNameJoin[{root, "tests", "QED-5-point-residues.wl"}]];
sixBalancedParticles = {"e", "E", "A.+", "A.+", "A.-", "A.-"};
sixBalancedEvents = {};
sixBalancedExample = qedBalancedSixPointAmplitude[sixModel, "ReturnDetails" -> True,
  "ProgressFunction" -> Function[event, AppendTo[sixBalancedEvents, event]]];
check["the balanced worked example regenerates fourteen targets and eight recursion terms",
  AssociationQ[sixBalancedExample] && sixBalancedExample["Status"] === "CONSTRUCTED" &&
    sixBalancedExample["PhysicalTargetCount"] === 14 && Length[sixBalancedExample["Channels"]] === 8];
check["the balanced example retains its model-local masses and no explicit reference spinors",
  AssociationQ[sixBalancedExample] && FreeQ[sixBalancedExample["Amplitude"], Spinor["Xi", ___]] &&
    (Mass[1] /. sixBalancedExample["ExternalMassRules"]) === ModelParameter[sixModel, "Me"] &&
    TrueQ[sixBalancedExample["SewingMinusIncludedOnce"]] && TrueQ[sixBalancedExample["NoFeynmanInput"]]];
check["the balanced constructor reports eight steps and defaults to an ordinary expression",
  Lookup[sixBalancedEvents, "Completed"] === Range[0, 8] &&
    qedBalancedSixPointAmplitude[sixModel, "ProgressReporting" -> False] === sixBalancedExample["Amplitude"]];
check["the balanced example rejects incomplete lower-point data",
  FailureQ[qedBalancedSixPointAmplitude[ReadModel[FileNameJoin[{root, "models"}], 2],
    "ProgressReporting" -> False]]];
sixBalancedZ = Unique["sixBalancedShiftTest"];
sixBalancedDenominator = PropDen[Mom[Multiparticle[1, 5]], ModelParameter[sixModel, "Me"]];
sixBalancedSlope = SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1], Spinor["Helicity", "Square", 3]];
check["the example shift traverses lower-amplitude inverse propagators",
  qedBalancedSixPointShift[1/sixBalancedDenominator, sixBalancedZ] ===
    1/(sixBalancedDenominator + sixBalancedZ sixBalancedSlope)];
check["the example shift treats repeated typed momentum occurrences coherently",
  qedBalancedSixPointShift[MomProd[5, 1] + MomProd[1, 3], sixBalancedZ] ===
    MomProd[5, 1] + MomProd[1, 3]];
check["the example shift rejects unsupported labels rather than dropping them",
  FailureQ[qedBalancedSixPointShift[Mom[7]^2, sixBalancedZ]]];

sixBalancedChannels = FactorizationAmplitudeChannels[sixBalancedParticles, sixModel];
sixBalancedNormalized = ConstructiveDiagrams`Private`residueSimpleInputs[
  {sixBalancedExample["ExternalMassRules"], sixBalancedExample["Amplitude"]}, sixBalancedChannels, sixModel];
sixBalancedTargets = ConstructiveDiagrams`Private`completionTargets[sixBalancedNormalized["Targets"]];
sixBalancedSelectedTargets = Select[sixBalancedTargets, MemberQ[{{1, 3}, {1, 3, 4}}, #["Cut"]] &];
sixBalancedSelectedNormalized = Join[sixBalancedNormalized, <|"Targets" -> sixBalancedSelectedTargets|>];
sixBalancedPoleProbe = ConstructiveDiagrams`Private`completionSparseValidation[
  sixBalancedExample["Amplitude"], {}, <|"SolutionRules" -> {}, "FreeParameters" -> {}|>, {}, {},
  sixBalancedSelectedNormalized, 1, 881371, ModelParameterRules[sixModel], 60, Automatic, False];
check["the balanced example passes fresh coherent-spin two-leg and three-leg cuts",
  AssociationQ[sixBalancedPoleProbe] && TrueQ[sixBalancedPoleProbe["FamilyPass"]] &&
    Length[sixBalancedPoleProbe["Report"]["Channels"]] === 2 &&
    AllTrue[sixBalancedPoleProbe["Report"]["Channels"], #["Status"] === "PASS" &]];

(* Generated only after the constructive expression, and used only as a check. *)
sixBalancedFeynmanModel = ReadModel[FileNameJoin[{root, "model_feynman"}], 2];
sixBalancedFeynmanRaw = diagramAmplitudes[CreateDiagrams[sixBalancedParticles, sixBalancedFeynmanModel]];
sixBalancedFeynmanPrepared = PrepareAmplitudes[sixBalancedFeynmanRaw, sixBalancedFeynmanModel,
  "ReferenceSpinors" -> "AllSpurious", "MaxSteps" -> 0, "TimeConstraint" -> 60,
  "ProgressReporting" -> False];
sixBalancedFeynmanProbe = ConstructiveDiagrams`Private`amplitudeFeynmanReport[
  sixBalancedNormalized, sixBalancedFeynmanPrepared, sixModel, 1, 881399,
  ModelParameterRules[sixModel], ModelParameterRules[sixBalancedFeynmanModel], 60, 0];
check["the balanced example agrees with an independent fresh Feynman point in every massive-spin component",
  AssociationQ[sixBalancedFeynmanProbe] && sixBalancedFeynmanProbe["Status"] === "PASS" &&
    sixBalancedFeynmanProbe["CompletedSamples"] === 1 &&
    sixBalancedFeynmanProbe["InternalOnShell"] === False &&
    sixBalancedFeynmanProbe["NormalizationFitted"] === False];

(* This default end-to-end regression follows the user's physical-reference
   diagram route. It supplies only generated diagrams, channels and the model;
   neither a final amplitude nor a correction basis enters construction. *)
sixCompactOldChecks = checks;
check["the original default six-point regression inventory remains intact", sixCompactOldChecks === 96];
sixCompactChecks = 0;
sixCompactCheck[name_, value_] := (sixCompactChecks++; check[name, value]);
Print["Preparing twenty-four fresh all-positive physical-reference diagrams."];
sixCompactDiagramModel = ReadModel[FileNameJoin[{root, "models"}], 2];
sixCompactDiagrams = CreateDiagrams[sixParticles, sixCompactDiagramModel];
sixCompactRaw = diagramAmplitudes[sixCompactDiagrams];
sixCompactRawSaved = sixCompactRaw;
sixCompactPreparation = PrepareAmplitudes[sixCompactRaw, sixCompactDiagramModel,
  "ReferenceSpinors" -> "AllPhysical", "MaxSteps" -> 0,
  "ProgressReporting" -> False, "ReturnDetails" -> True];
sixCompactPrepared = sixCompactPreparation["PreparedAmplitudes"];
sixCompactNormalized = ConstructiveDiagrams`Private`completionNormalizeInput[
  sixCompactPreparation, sixChannels, sixModel];
sixCompactGroups = GatherBy[Last /@ sixCompactPrepared,
  ConstructiveDiagrams`Private`completionCanonicalReduce[
    #, sixCompactNormalized["ExternalMassRules"]] &];
sixCompactCheck["all twenty-four fresh diagrams prepare to one-term physical-reference numerators",
  Length[sixCompactDiagrams] === 24 && Length[sixCompactPrepared] === 24 &&
    sixCompactPreparation["Status"] === "PREPARED" &&
    AllTrue[sixCompactPrepared,
      First[ConstructiveDiagrams`Private`preparationRank[Last[#]]] === 1 &]];
sixCompactCheck["the generated physical-reference diagrams have six groups of four identical terms",
  Length[sixCompactGroups] === 6 && (Length /@ sixCompactGroups) === ConstantArray[4, 6]];
sixCompactCheck["preparation keeps the raw diagrams intact and eliminates only internal spin sums and references",
  sixCompactRaw === sixCompactRawSaved &&
    FreeQ[Last /@ sixCompactPrepared, _Mass | Spinor["Xi", ___] |
      Spinor[___, _Multiparticle, ___] | _xFactor | _xTildeFactor]];
sixCompactPlan = ConstructiveDiagrams`Private`completionBosePlan[
  {{3, 4, 5, 6}}, sixCompactNormalized["ExternalMassRules"]];
Print["Completing all fourteen channels using source-derived companions."];
sixCompactCompletion = CompleteFactorization[sixCompactPreparation, sixChannels, sixModel,
  "BoseSymmetryGroups" -> {{3, 4, 5, 6}}, "Samples" -> 2, "ValidationSamples" -> 2,
  "MaxCorrectionTerms" -> 8, "TimeConstraint" -> 180,
  "ProgressReporting" -> False, "ReturnDetails" -> True];
sixCompactCheck["automatic completion finds a compact amplitude from fresh physical-reference diagrams",
  AssociationQ[sixCompactCompletion] && sixCompactCompletion["Status"] === "COMPLETE" &&
    sixCompactCompletion["CorrectionSpace"] === "PeeledChannels"];
sixCompactCheck["the small source-derived fit has four coefficients and no discarded free family",
  Length[sixCompactCompletion["CorrectionBasis"]] === 4 &&
    sixCompactCompletion["FreeParameters"] === {} && sixCompactCompletion["RemainingBasis"] === {} &&
    sixCompactCompletion["FamilyAmplitude"] === sixCompactCompletion["Amplitude"]];
sixCompactCheck["the completed six-point amplitude retains twelve terms at complexity at most 108",
  First[ConstructiveDiagrams`Private`preparationRank[sixCompactCompletion["Amplitude"]]] <= 108 &&
    ConstructiveDiagrams`Private`expressionTermCounts[sixCompactCompletion["Amplitude"]]["Terms"] === 12];
sixCompactCheck["the compact result is independent of reference labels and retains ordinary external spinors",
  FreeQ[sixCompactCompletion["Amplitude"], _Mass | Spinor["Xi", ___] |
    Spinor[___, _Multiparticle, ___] | _xFactor | _xTildeFactor | _Return | _Module]];
sixCompactLastRound = Max[Lookup[sixCompactCompletion["ValidationReports"], "Round", -1]];
sixCompactValidations = Select[sixCompactCompletion["ValidationReports"],
  Lookup[#, "Round", -1] === sixCompactLastRound &];
sixCompactCheck["all fourteen channels pass independent validation at both model mass scales",
  Length[sixCompactValidations] === 2 &&
    Length[DeleteDuplicates[Lookup[sixCompactValidations, "ParameterRules"]]] === 2 &&
    AllTrue[sixCompactValidations, Function[entry,
      Length[entry["Report"]["Channels"]] === 14 &&
        AllTrue[entry["Report"]["Channels"], #["Status"] === "PASS" &]]]];
sixCompactCheck["the companion fit records all declared coefficients and a separate fit seed",
  Length[sixCompactCompletion["ConstraintData"]["FitVariables"]] === 4 &&
    sixCompactCompletion["ConstraintData"]["LinearSolution"]["Rank"] === 4 &&
    sixCompactCompletion["ConstraintData"]["FitSeed"] === 0];
sixCompactCompanions = sixCompactCompletion["ChannelCompanions"];
sixCompactCheck["the companion builder reports completed construction within the declared cap",
  AssociationQ[sixCompactCompanions] &&
    sixCompactCompanions["ConstructionStatus"] === "COMPLETE" &&
    Length[sixCompactCompanions["Basis"]] === 4 &&
    sixCompactCompanions["RawDirectionCount"] === 17 &&
    sixCompactCompanions["Diagnostics"] === {}];
sixCompactCheck["all eight ordinary selected-cut source normalizations are independently certified",
  Length[sixCompactCompanions["SourceChecks"]] === 8 &&
    AllTrue[sixCompactCompanions["SourceChecks"],
      #["Verification"]["Status"] === "PASS" &&
        Length[#["Verification"]["Samples"]] === 2 &&
        #["Scope"] === "SelectedCutSourceNormalizationOnly" &]];
sixCompactCheck["a momentum-peeling companion is explicitly distinguished from an identity",
  Length[sixCompactCompanions["Mappings"]] === 16 &&
    AllTrue[sixCompactCompanions["Mappings"], #["PeelingIsIdentity"] === False &]];
sixCompactCheck["source components of every available momentum degree are retained before fitting",
  Sort[DeleteDuplicates[Lookup[sixCompactCompanions["Mappings"], "MomentumDegree"]]] === {3, 4}];
sixCompactCheck["the selected physical propagator is never changed by a typed peeling map",
  AllTrue[sixCompactCompanions["Mappings"], Function[mapping,
    AllTrue[mapping["PropagatorMap"], Function[replacement,
      MatchQ[replacement, _PropDen -> _PropDen] &&
        First[ConstructiveDiagrams`Private`residuePropagatorKey[First[replacement], Range[6]]] =!=
          mapping["Cut"]]]]]];
sixCompactCheck["typed peeling preserves the external spinors and their labels exactly",
  AllTrue[sixCompactCompanions["Mappings"],
    Sort[Cases[#["SourceExpression"], _Spinor, Infinity]] ===
      Sort[Cases[#["CompanionExpression"], _Spinor, Infinity]] &]];
sixCompactOpaque[value_] := value /. {_PropDen -> sixOpaquePropagator, _Mom -> sixOpaqueMomentum};
sixCompactCheck["typed peeling leaves signs, coefficients and untyped integer payloads untouched",
  AllTrue[sixCompactCompanions["Mappings"],
    sixCompactOpaque[#["SourceExpression"]] === sixCompactOpaque[#["CompanionExpression"]] &]];
sixCompactCheck["every generated companion uses known simple physical poles",
  AllTrue[sixCompactCompanions["Basis"],
    ConstructiveDiagrams`Private`completionHigherPhysicalPoles[#,
      ConstructiveDiagrams`Private`completionTargets[sixCompactNormalized["Targets"]], Range[6]] === {} &]];
sixCompactCapped = ConstructiveDiagrams`Private`completionPeeledChannelBasis[
  sixCompactNormalized, sixCompactCompletion["ChannelContinuations"], sixCompactPlan,
  1, sixRules, 726113, 15];
sixCompactCheck["a companion space exceeding the requested cap is declined rather than truncated",
  sixCompactCapped["Basis"] === Missing["PeeledChannelSpaceLimit"] &&
    sixCompactCapped["ConstructionStatus"] === "PARTIAL" && sixCompactCapped["Diagnostics"] =!= {}];
sixCompactScalarPlan = ConstructiveDiagrams`Private`completionBosePlan[{},
  sixScalarNormalized["ExternalMassRules"]];
sixCompactIneligibleSource = <|"Variants" -> {<|"Mode" -> "AllPhysical", "Accepted" -> True,
  "Cut" -> sixScalarTarget["Cut"], "Mass" -> sixScalarTarget["Mass"],
  "Expression" -> sixScalarTarget["StateSum"]/sixScalarTarget["Propagator"]|>}|>;
sixCompactCheck["a channel without a massive-anchor and null-emission pair supplies no peeling ansatz",
  ConstructiveDiagrams`Private`completionPeeledChannelBasis[
    sixScalarNormalized, sixCompactIneligibleSource, sixCompactScalarPlan, 8,
    sixScalarRules, 726119, 2] === Missing["NoEligibleSources"]];
sixCompactContactTarget = SelectFirst[sixNormalized["Targets"], Length[#["Cut"]] === 2 &];
sixCompactContactSource = <|"Variants" -> {<|"Mode" -> "AllPhysical", "Accepted" -> True,
  "Cut" -> sixCompactContactTarget["Cut"], "Mass" -> sixCompactContactTarget["Mass"],
  "Expression" -> 1|>}|>;
sixCompactContactSpace = ConstructiveDiagrams`Private`completionPeeledChannelBasis[
  sixNormalized, sixCompactContactSource, sixCompactPlan, 8, sixRules, 726127, 15];
sixCompactCheck["even an eligible normalized source cannot introduce an independent pole-free direction",
  AssociationQ[sixCompactContactSpace] && sixCompactContactSpace["Core"] === 0 &&
    sixCompactContactSpace["RegularExpression"] === 0 &&
    sixCompactContactSpace["Basis"] === {} && sixCompactContactSpace["Mappings"] === {} &&
    Length[sixCompactContactSpace["SourceChecks"]] === 1 &&
    First[sixCompactContactSpace["SourceChecks"]]["Verification"]["Status"] === "PASS"];
sixCompactOtherPole = SelectFirst[sixNormalized["Targets"],
  Length[#["Cut"]] === 2 && #["Cut"] =!= sixCompactContactTarget["Cut"] &];
sixCompactPoleSum = 1/sixCompactContactTarget["Propagator"] + 1/sixCompactOtherPole["Propagator"];
sixCompactPoleSumSource = <|"Variants" -> {<|"Mode" -> "AllPhysical", "Accepted" -> True,
  "Cut" -> sixCompactContactTarget["Cut"], "Mass" -> sixCompactContactTarget["Mass"],
  "Expression" -> sixCompactPoleSum|>}|>;
sixCompactPoleSumSpace = ConstructiveDiagrams`Private`completionPeeledChannelBasis[
  sixNormalized, sixCompactPoleSumSource, sixCompactPlan, 8, sixRules, 726129, 15];
sixCompactCheck["a sum of physical pole terms is retained when its ordinary denominator is one",
  Denominator[sixCompactPoleSum] === 1 && AssociationQ[sixCompactPoleSumSpace] &&
    Length[sixCompactPoleSumSpace["Basis"]] === 1 &&
    Length[sixCompactPoleSumSpace["Mappings"]] === 1 &&
    sixCompactPoleSumSpace["RawDirectionCount"] === 1 &&
    Denominator[First[sixCompactPoleSumSpace["Mappings"]]["ProjectedExpression"]] === 1];
sixCompactCheck["channel companions are enabled by default and support an explicit opt-out",
  ("ChannelCompanions" /. Options[CompleteFactorization]) === Automatic &&
    Quiet[CompleteFactorization[0, sixScalarChannels, sixScalarModel,
      "ChannelCompanions" -> True]] === $Failed];
sixCompactDisabled = CompleteFactorization[0, sixScalarChannels, sixScalarModel,
  "ChannelCompanions" -> None, "ParameterRules" -> sixScalarRules,
  "Samples" -> 1, "ValidationSamples" -> 1, "TimeConstraint" -> 30,
  "ProgressReporting" -> False, "ReturnDetails" -> True];
sixCompactCheck["disabling companions preserves the existing general channel reconstruction",
  sixCompactDisabled["Status"] === "COMPLETE" &&
    sixCompactDisabled["ChannelCompanions"] === Missing["NotRequested"] &&
    sixCompactDisabled["CorrectionSpace"] =!= "PeeledChannels" &&
    AllTrue[sixCompactDisabled["ValidationReport"]["Channels"], #["Status"] === "PASS" &]];

(* This independent benchmark is generated after the constructive solve and
   never supplied as fitting data. Internal Feynman lines remain off shell. *)
sixCompactFeynmanModel = sixBalancedFeynmanModel;
sixCompactFeynmanRaw = diagramAmplitudes[CreateDiagrams[sixParticles, sixCompactFeynmanModel]];
sixCompactFeynmanPrepared = PrepareAmplitudes[sixCompactFeynmanRaw, sixCompactFeynmanModel,
  "ReferenceSpinors" -> "AllSpurious", "MaxSteps" -> 0, "TimeConstraint" -> 60,
  "ProgressReporting" -> False];
sixCompactCheck["the independent all-positive Feynman benchmark contains all twenty-four diagrams",
  ListQ[sixCompactFeynmanPrepared] && Length[sixCompactFeynmanPrepared] === 24];
sixCompactFullCheck = CheckAmplitudes[sixCompactCompletion["Amplitude"], sixChannels, sixModel,
  "FeynmanAmplitudes" -> sixCompactFeynmanPrepared, "FeynmanModel" -> sixCompactFeynmanModel,
  "FeynmanSamples" -> 1, "FeynmanTimeConstraint" -> 120,
  "Samples" -> 3, "HighEnergySamples" -> 2, "ReferenceSamples" -> 2,
  "RandomSeed" -> 726133, "TimeConstraint" -> 90, "Output" -> "Details"];
sixCompactCheck["the compact automatic result passes fresh full channel-by-channel checks",
  Length[sixCompactFullCheck["Channels"]] === 14 &&
    AllTrue[sixCompactFullCheck["Channels"], #["Status"] === "PASS" &]];
sixCompactCheck["the compact automatic amplitude passes the expected high-energy bound",
  sixCompactFullCheck["HighEnergyCheck"]["Status"] === "PASS"];
sixCompactCheck["the compact automatic result retains no unresolved reference or spurious-pole check",
  sixCompactFullCheck["ReferenceCheck"]["Status"] === "PASS" &&
    AllTrue[sixCompactFullCheck["SpuriousChecks"], #["Status"] === "PASS" &]];
sixCompactCheck["the compact result agrees with fresh Feynman amplitudes in every coherent massive-spin component",
  sixCompactFullCheck["FeynmanCheck"]["Status"] === "PASS" &&
    sixCompactFullCheck["FeynmanCheck"]["CompletedSamples"] === 1 &&
    sixCompactFullCheck["FeynmanCheck"]["Scope"] ===
      "SampledOffCutMomentaAllImplicitMassiveSpinComponents" &&
    sixCompactFullCheck["FeynmanCheck"]["InternalOnShell"] === False &&
    sixCompactFullCheck["FeynmanCheck"]["NormalizationFitted"] === False];
check["the complete compact six-point regression inventory ran", sixCompactChecks === 28];

If[Environment["HEPCAT_TEST_FULL_BALANCED_SIX_POINT"] === "1",
  sixBalancedFullCheck = CheckAmplitudes[sixBalancedExample["Amplitude"], sixBalancedChannels, sixModel,
    "FeynmanAmplitudes" -> sixBalancedFeynmanPrepared, "FeynmanModel" -> sixBalancedFeynmanModel,
    "FeynmanSamples" -> 3, "Samples" -> 2, "HighEnergySamples" -> 2, "ReferenceSamples" -> 2,
    "RandomSeed" -> 881417, "TimeConstraint" -> 600, "Output" -> "Details"];
  check["the full balanced example passes all fourteen physical channels",
    AssociationQ[sixBalancedFullCheck] && Length[sixBalancedFullCheck["Channels"]] === 14 &&
      AllTrue[sixBalancedFullCheck["Channels"], #["Status"] === "PASS" &]];
  check["the full balanced example passes three independent Feynman samples",
    sixBalancedFullCheck["FeynmanCheck"]["Status"] === "PASS" &&
      sixBalancedFullCheck["FeynmanCheck"]["CompletedSamples"] === 3];
  check["the full balanced example passes its expected high-energy bound",
    sixBalancedFullCheck["HighEnergyCheck"]["Status"] === "PASS"];
  check["the full balanced example has no remaining external reference dependence",
    sixBalancedFullCheck["ReferenceCheck"]["Status"] === "PASS"];
  check["the two classified spurious bracket poles pass their sampled limits",
    With[{rows = Select[sixBalancedFullCheck["SpuriousChecks"],
        Lookup[Lookup[#, "Discovery", <||>], "Classification", None] === "Spurious" &]},
      Length[rows] === 2 && AllTrue[rows, #["Status"] === "PASS" &]]];
  check["unsupported discovered denominator factors stay unresolved instead of silently passing",
    AllTrue[Select[sixBalancedFullCheck["SpuriousChecks"],
      Lookup[Lookup[#, "Discovery", <||>], "Classification", None] === "Unclassified" &],
      #["Status"] === "UNRESOLVED" &]];
];

If[Environment["HEPCAT_TEST_FULL_SIX_POINT"] === "1",
  Print["Preparing the actual six-point electron/four-photon diagrams."];
  sixFullDiagramModel = ReadModel[FileNameJoin[{root, "models"}], 2];
  sixFullPairs = diagramAmplitudes[CreateDiagrams[sixParticles, sixFullDiagramModel]];
  sixFullPrepared = PrepareAmplitudes[sixFullPairs, sixFullDiagramModel,
    "ReferenceSpinors" -> "AllSpurious", "MaxSteps" -> 12,
    "ProgressReporting" -> False];
  check["the full six-point regression prepares all twenty-four actual diagrams",
    ListQ[sixFullPrepared] && Length[sixFullPrepared] === 24 &&
      FreeQ[sixFullPrepared, Spinor["Xi", ___]]];
  Print["Completing all fourteen six-point factorization channels."];
  sixFullCompletion = CompleteFactorization[sixFullPrepared, sixChannels, sixModel,
    "BoseSymmetryGroups" -> {{3, 4, 5, 6}},
    "Samples" -> 12, "ValidationSamples" -> 1, "MaxCorrectionTerms" -> 256,
    "MaxRefinements" -> 5, "TimeConstraint" -> 1200,
    "ProgressReporting" -> False, "ReturnDetails" -> True];
  check["actual six-point diagrams complete without a supplied answer or correction basis",
    AssociationQ[sixFullCompletion] && sixFullCompletion["Status"] === "COMPLETE"];
  sixFullValidations = Lookup[sixFullCompletion, "ValidationReports", {}];
  sixFullLastRound = If[sixFullValidations === {}, -1,
    Max[Lookup[sixFullValidations, "Round", -1]]];
  sixFullFinalValidations = Select[sixFullValidations,
    Lookup[#, "Round", -1] === sixFullLastRound &];
  check["the completed six-point result passes every held-out channel at both mass scales",
    Length[sixFullFinalValidations] === 2 &&
      Length[DeleteDuplicates[Lookup[sixFullFinalValidations, "ParameterRules"]]] === 2 &&
      AllTrue[sixFullFinalValidations, Function[entry,
        Length[entry["Report"]["Channels"]] === 14 &&
          AllTrue[entry["Report"]["Channels"], #["Status"] === "PASS" &]]]];
  check["the full six-point affine family also passes its final independent validation",
    Lookup[sixFullCompletion, "RefinementHistory", {}] =!= {} &&
      Last[sixFullCompletion["RefinementHistory"]]["Status"] === "COMPLETE"];
];

(* This fresh end-to-end case is deliberately opt-in: its full automatic fit
   costs much more than the structural probes above. No saved answer or cached
   equations enter construction, and the Feynman result is generated only later. *)
If[Environment["HEPCAT_TEST_FULL_MIXED_SIX_POINT"] === "1",
  Print["Preparing twenty-four mixed-helicity six-point diagrams."];
  sixMixedDiagramModel = ReadModel[FileNameJoin[{root, "models"}], 2];
  sixMixedRaw = diagramAmplitudes[CreateDiagrams[sixMixedParticles, sixMixedDiagramModel]];
  sixMixedRawBefore = sixMixedRaw;
  sixMixedPrepared = PrepareAmplitudes[sixMixedRaw, sixMixedDiagramModel,
    "ReferenceSpinors" -> "AllSpurious", "MaxSteps" -> 12,
    "ProgressReporting" -> False];
  check["the mixed end-to-end regression prepares twenty-four actual diagrams",
    ListQ[sixMixedPrepared] && Length[sixMixedPrepared] === 24 &&
      FreeQ[sixMixedPrepared, Spinor["Xi", ___]] && sixMixedRaw === sixMixedRawBefore];
  sixMixedCompletion = CompleteFactorization[sixMixedPrepared, sixMixedChannels, sixModel,
    "BoseSymmetryGroups" -> {{3, 4, 5}}, "Samples" -> 28,
    "ValidationSamples" -> 2, "RandomSeed" -> 1729,
    "MaxCorrectionTerms" -> 1024, "MaxRefinements" -> 5,
    "TimeConstraint" -> 7200, "ProgressReporting" -> False, "ReturnDetails" -> True];
  check["mixed six-point diagrams complete through the public automatic algorithm",
    AssociationQ[sixMixedCompletion] && sixMixedCompletion["Status"] === "COMPLETE"];
  If[AssociationQ[sixMixedCompletion] && sixMixedCompletion["Status"] === "COMPLETE",
    sixMixedLastRound = Max[Lookup[sixMixedCompletion["ValidationReports"], "Round", -1]];
    sixMixedFinalReports = Select[sixMixedCompletion["ValidationReports"],
      #["Round"] === sixMixedLastRound &];
    check["the mixed result passes all channels at both model-mass assignments",
      Length[sixMixedFinalReports] === 2 &&
        Length[DeleteDuplicates[Lookup[sixMixedFinalReports, "ParameterRules"]]] === 2 &&
        AllTrue[sixMixedFinalReports, Function[entry,
          Length[entry["Report"]["Channels"]] === 14 &&
            AllTrue[entry["Report"]["Channels"], #["Status"] === "PASS" &]]]];
    check["the public mixed affine family passes independent validation",
      sixMixedCompletion["RefinementHistory"] =!= {} &&
        Last[sixMixedCompletion["RefinementHistory"]]["Status"] === "COMPLETE"];
    sixMixedFeynmanModel = ReadModel[FileNameJoin[{root, "model_feynman"}], 2];
    sixMixedFeynmanRaw = diagramAmplitudes[
      CreateDiagrams[sixMixedParticles, sixMixedFeynmanModel]];
    sixMixedFeynmanPrepared = PrepareAmplitudes[sixMixedFeynmanRaw, sixMixedFeynmanModel,
      "ReferenceSpinors" -> "AllSpurious", "MaxSteps" -> 0,
      "TimeConstraint" -> 120, "ProgressReporting" -> False];
    check["the independent mixed Feynman benchmark retains twenty-four diagrams",
      ListQ[sixMixedFeynmanPrepared] && Length[sixMixedFeynmanPrepared] === 24];
    (* This probe certifies sampled off-cut agreement, not every optional check. *)
    sixMixedFeynmanReport = CheckAmplitudes[sixMixedCompletion["Amplitude"],
      sixMixedChannels, sixModel, "FeynmanAmplitudes" -> sixMixedFeynmanPrepared,
      "FeynmanModel" -> sixMixedFeynmanModel, "FeynmanSamples" -> 3,
      "FeynmanTimeConstraint" -> 240, "Samples" -> 1, "RandomSeed" -> 874031,
      "SpuriousPoles" -> {}, "HighEnergyCheck" -> False, "ReferenceCheck" -> False,
      "TimeConstraint" -> 90, "Output" -> "Details"];
    check["the public mixed result agrees with independently generated Feynman diagrams off cut",
      AssociationQ[sixMixedFeynmanReport] &&
        sixMixedFeynmanReport["FeynmanCheck"]["Status"] === "PASS" &&
        sixMixedFeynmanReport["FeynmanCheck"]["CompletedSamples"] === 3 &&
        sixMixedFeynmanReport["FeynmanCheck"]["Scope"] ===
          "SampledOffCutMomentaAllImplicitMassiveSpinComponents" &&
        sixMixedFeynmanReport["FeynmanCheck"]["InternalOnShell"] === False &&
        sixMixedFeynmanReport["FeynmanCheck"]["NormalizationFitted"] === False];
  ];
];

Print[checks, " six-point-completion checks; ", Length[failures], " failures."];
If[failures =!= {}, Print[failures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:six-point-completion-tests.wl"];
Quit[0];
