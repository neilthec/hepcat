(* Infer exchange symmetry only for identified, compatible bosonic states. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0;
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++;
  If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);
bsInfer = ConstructiveDiagrams`Private`completionInferBoseGroups;
bsNormalize[expression_, masses_List, targets_List : {}] := <|
  "Expression" -> expression,
  "ExternalMassRules" -> Thread[(Mass /@ Range[Length[masses]]) -> masses],
  "Targets" -> targets, "Records" -> {}|>;
bsBracket[type_, i_, j_] := SpinorChain[
  Spinor["Helicity", type, i], Spinor["Helicity", type, j]];
bsReferenceFactor[type_, leg_] := SpinorChain[
  Spinor["Helicity", type, leg], Spinor["Xi", type, 100 + leg]]^2;
bsElectronPair = SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]];
bsQEDModel = {{{"electron", "e", "E", 11, 1, bsMass, 0, 1, 1, "", ""},
  {"photon", "A.+", "A.-", 22, 2, 0, 0, 1, 1, "", ""}}, {}, {}, {}};
bsAllPlus = bsElectronPair bsBracket["Square", 3, 4] bsBracket["Square", 4, 5] *
  bsBracket["Square", 5, 6] bsBracket["Square", 6, 3];
bsMasses = {bsMass, bsMass, 0, 0, 0, 0};
bsAllPlusNormalized = bsNormalize[bsAllPlus, bsMasses];
bsAllPlusInfo = bsInfer[bsAllPlusNormalized, bsQEDModel];
check["automatic Bose inference is the public default",
  ("BoseSymmetryGroups" /. Options[CompleteFactorization]) === Automatic];
check["all four equal-helicity photons are inferred without a manual group",
  bsAllPlusInfo["Groups"] === {{3, 4, 5, 6}}];
check["automatic inference returns compact diagnostics separately from the amplitude",
  AssociationQ[bsAllPlusInfo] && ContainsAll[Keys[bsAllPlusInfo],
    {"Groups", "ExternalParticles", "Legs", "Diagnostics", "Source"}] &&
    bsAllPlusNormalized["Expression"] === bsAllPlus];
check["the electron and positron are excluded from bosonic groups",
  FreeQ[Flatten[bsAllPlusInfo["Groups"]], 1 | 2]];
bsOneMinus = bsElectronPair bsBracket["Square", 3, 4] *
  bsBracket["Square", 4, 5] bsBracket["Square", 5, 3] *
  bsReferenceFactor["Angle", 6];
bsOneMinusInfo = bsInfer[bsNormalize[bsOneMinus, bsMasses], bsQEDModel];
check["one negative-helicity photon is not averaged with the three positive-helicity photons",
  bsOneMinusInfo["Groups"] === {{3, 4, 5}}];
bsTwoMinus = bsElectronPair bsBracket["Square", 3, 4]^2 bsBracket["Angle", 5, 6]^2;
bsTwoMinusInfo = bsInfer[bsNormalize[bsTwoMinus, bsMasses], bsQEDModel];
check["two positive and two negative photons form two independent exchange groups",
  bsTwoMinusInfo["Groups"] === {{3, 4}, {5, 6}}];
bsFactored = (1 + MomProd[1, 2]) bsAllPlus/
  PropDen[Mom[Multiparticle[1, 3]], bsMass];
check["factored sums and propagators do not alter external spinor weights",
  bsInfer[bsNormalize[bsFactored, bsMasses], bsQEDModel]["Groups"] === {{3, 4, 5, 6}}];
bsEquivalentSum = bsAllPlus + 3 MomProd[1, 3] bsAllPlus;
check["a homogeneous sum retains its common external state",
  bsInfer[bsNormalize[bsEquivalentSum, bsMasses], bsQEDModel]["Groups"] === {{3, 4, 5, 6}}];
bsInhomogeneous = bsAllPlus + bsTwoMinus;
bsInhomogeneousInfo = bsInfer[bsNormalize[bsInhomogeneous, bsMasses], bsQEDModel];
check["incompatible external helicities in one expression are not silently averaged",
  !MemberQ[bsInhomogeneousInfo["Groups"], group_ /; MemberQ[group, 5] || MemberQ[group, 6]]];
check["a scalar expression is not guessed to carry photon helicities",
  bsInfer[bsNormalize[3, bsMasses], bsQEDModel]["Groups"] === {}];
check["zero alone provides no photon-state evidence",
  bsInfer[bsNormalize[0, bsMasses], bsQEDModel]["Groups"] === {}];
bsUnknown = bsOpaque[bsAllPlus];
check["an unsupported spinor-containing head is not assigned a fictitious homogeneous weight",
  bsInfer[bsNormalize[bsUnknown, bsMasses], bsQEDModel]["Groups"] === {}];
bsChannelInfo = bsInfer[bsNormalize[0, bsMasses,
  {<|"Complete" -> True, "StateSum" -> bsAllPlus|>}], bsQEDModel];
check["complete nonzero channel data can identify states of a zero seed",
  bsChannelInfo["Groups"] === {{3, 4, 5, 6}}];
bsScalarModel = {{{"real scalar", "s", "s", 1, 0, bsScalarMass, 0, 1, 1, "", ""}}, {}, {}, {}};
check["a unique scalar species with declared masses can be identified without spinors",
  bsInfer[bsNormalize[7, ConstantArray[bsScalarMass, 4]], bsScalarModel]["Groups"] === {{1, 2, 3, 4}}];
bsSixScalarPlan = ConstructiveDiagrams`Private`completionAutomaticBosePlan[
  bsNormalize[7, ConstantArray[bsScalarMass, 6]], bsScalarModel];
check["larger identical-boson sets use a bounded exact subgroup instead of failing",
  bsSixScalarPlan["DetectedGroups"] === {Range[6]} &&
    bsSixScalarPlan["Groups"] === {Range[5]} && bsSixScalarPlan["Order"] === 120 &&
    bsSixScalarPlan["Diagnostics"] =!= {}];
bsScalarAmbiguousModel = bsScalarModel;
bsScalarAmbiguousModel[[1]] = Join[bsScalarModel[[1]],
  {{"other scalar", "r", "r", 2, 0, bsScalarMass, 0, 1, 1, "", ""}}];
bsScalarAmbiguousInfo = bsInfer[bsNormalize[7, ConstantArray[bsScalarMass, 4]], bsScalarAmbiguousModel];
check["equal scalar masses do not establish identical species",
  bsScalarAmbiguousInfo["Groups"] === {} && bsScalarAmbiguousInfo["Diagnostics"] =!= {}];
check["ordered process labels resolve otherwise degenerate scalar species",
  bsInfer[bsNormalize[7, ConstantArray[bsScalarMass, 4]], bsScalarAmbiguousModel,
    {"s", "r", "s", "r"}]["Groups"] === {{1, 3}, {2, 4}}];
bsVectorModel = {{{"charged vector", "v", "V", 24, 2, bsVectorMass, 0, 1, 1, "", ""}}, {}, {}, {}};
bsVectorExpression = Times @@ Table[SpinorChain[Spinor["Spin", "Angle", leg],
  Spinor["Xi", "Angle", 200 + leg]]^2, {leg, 4}];
bsVectorNormalized = bsNormalize[bsVectorExpression, ConstantArray[bsVectorMass, 4]];
bsVectorAmbiguousInfo = bsInfer[bsVectorNormalized, bsVectorModel];
check["a massive vector and its antiparticle are not merged from mass and spin alone",
  bsVectorAmbiguousInfo["Groups"] === {} && bsVectorAmbiguousInfo["Diagnostics"] =!= {}];
check["explicit particle and antiparticle labels form separate vector groups",
  bsInfer[bsVectorNormalized, bsVectorModel, {"v", "V", "v", "V"}]["Groups"] === {{1, 3}, {2, 4}}];
bsQCDModel = bsQEDModel;
bsQCDModel[[1]] = Join[bsQEDModel[[1]],
  {{"gluon", "G.+", "G.-", 21, 2, 0, 0, 8, 1, "", ""}}];
bsColorAmbiguousInfo = bsInfer[bsAllPlusNormalized, bsQCDModel];
check["a photon and gluon degeneracy is not resolved from their equal masses and spins",
  bsColorAmbiguousInfo["Groups"] === {} && bsColorAmbiguousInfo["Diagnostics"] =!= {}];
check["explicit photon species labels resolve the massless-vector degeneracy",
  bsInfer[bsAllPlusNormalized, bsQCDModel,
    {"e", "E", "A.+", "A.+", "A.+", "A.+"}]["Groups"] === {{3, 4, 5, 6}}];
check["color-stripped gluons are conservatively left unprojected",
  bsInfer[bsAllPlusNormalized, bsQCDModel,
    {"e", "E", "G.+", "G.+", "G.+", "G.+"}]["Groups"] === {}];
bsMasslessFermionModel = {{{"massless fermion", "f.+", "f.-", 1, 1, 0, 0, 1, 1, "", ""}}, {}, {}, {}};
bsMasslessFermions = bsBracket["Square", 1, 2] bsBracket["Square", 3, 4];
check["massless fermions are never treated as Bose states",
  bsInfer[bsNormalize[bsMasslessFermions, ConstantArray[0, 4]], bsMasslessFermionModel]["Groups"] === {}];
bsSpinFixed = SpinorChain[Spinor["Spin", "Angle", 1, 1], Spinor["Xi", "Angle", 201]]^2 *
  SpinorChain[Spinor["Spin", "Angle", 2, 2], Spinor["Xi", "Angle", 202]]^2;
bsNeutralVectorModel = {{{"neutral vector", "v", "v", 23, 2, bsVectorMass, 0, 1, 1, "", ""}}, {}, {}, {}};
check["different explicit massive-spin states are not averaged together",
  bsInfer[bsNormalize[bsSpinFixed, ConstantArray[bsVectorMass, 2]], bsNeutralVectorModel,
    {"v", "v"}]["Groups"] === {}];
bsSpinVariance = SpinorChain[Spinor["Spin", "Upper", "Angle", 1, 1],
  Spinor["Xi", "Angle", 201]]^2 SpinorChain[
  Spinor["Spin", "Lower", "Angle", 2, 1], Spinor["Xi", "Angle", 202]]^2;
check["upper and lower fixed massive-spin slots do not become the same external state",
  bsInfer[bsNormalize[bsSpinVariance, ConstantArray[bsVectorMass, 2]], bsNeutralVectorModel,
    {"v", "v"}]["Groups"] === {}];
bsFixedFactor[leg_, index_] := SpinorChain[Spinor["Spin", "Angle", leg, index],
  Spinor["Xi", "Angle", 300 + leg]];
bsSpinTwoExpression = bsFixedFactor[1, 1]^3 bsFixedFactor[1, 2] *
  bsFixedFactor[2, 1] bsFixedFactor[2, 2]^3;
bsSpinTwoModel = {{{"spin two", "t", "t", 39, 4, bsTensorMass, 0, 1, 1, "", ""}}, {}, {}, {}};
check["spin-two states retain their spin-index multiplicities",
  bsInfer[bsNormalize[bsSpinTwoExpression, ConstantArray[bsTensorMass, 2]], bsSpinTwoModel,
    {"t", "t"}]["Groups"] === {}];
bsEmptyPlan = ConstructiveDiagrams`Private`completionBosePlan[{},
  bsAllPlusNormalized["ExternalMassRules"]];
check["an explicit empty symmetry group is still an exact opt-out",
  bsEmptyPlan["Groups"] === {} && bsEmptyPlan["Order"] === 1 &&
    ConstructiveDiagrams`Private`completionBoseProject[bsAllPlus, bsEmptyPlan] === bsAllPlus];
check["typed color labels follow particle relabeling without changing bare integers or Xi IDs",
  ConstructiveDiagrams`Private`completionBoseCycle[
    {ColorIndex[3], ColorIndex[4], 3, Spinor["Xi", "Angle", 3]}, {3, 4}] ===
    {ColorIndex[4], ColorIndex[3], 3, Spinor["Xi", "Angle", 3]}];

(* A real cubic tree checks the public automatic path and explicit opt-out. *)
bsCubicModel = bsScalarModel;
bsCubicModel[[2]] = {{"s", "s", "s", "", bsCoupling, 1}};
bsCubicChannels = FactorizationAmplitudeChannels[ConstantArray["s", 4], bsCubicModel];
bsCubicRules = Thread[(Mass /@ Range[4]) -> ConstantArray[bsScalarMass, 4]];
bsCubicPrepared = PrepareAmplitudes[{bsCubicRules, 0}, bsCubicModel,
  "ExternalParticles" -> ConstantArray["s", 4], "ReferenceSpinors" -> None,
  "MaxSteps" -> 0, "ProgressReporting" -> False, "ReturnDetails" -> True];
bsCubicSettings = {"Samples" -> 1, "ValidationSamples" -> 1, "TimeConstraint" -> 30,
  "MaxCorrectionTerms" -> 8, "ParameterRules" -> {bsScalarMass -> 1, bsCoupling -> 2},
  "ProgressReporting" -> False, "ReturnDetails" -> True};
bsCubicAutomatic = CompleteFactorization[bsCubicPrepared, bsCubicChannels, bsCubicModel,
  Sequence @@ bsCubicSettings];
check["completion inherits ordered process labels and validates an automatically projected scalar tree",
  bsCubicAutomatic["Status"] === "COMPLETE" &&
    bsCubicAutomatic["BoseSymmetry"]["Groups"] === {{1, 2, 3, 4}} &&
    bsCubicAutomatic["BoseSymmetry"]["Source"] === "ExternalParticles" &&
    bsCubicAutomatic["ExternalParticles"] === ConstantArray["s", 4] &&
    AllTrue[bsCubicAutomatic["ValidationReport"]["Channels"], #["Status"] === "PASS" &]];
bsCubicDisabled = CompleteFactorization[bsCubicPrepared, bsCubicChannels, bsCubicModel,
  "BoseSymmetryGroups" -> {}, Sequence @@ bsCubicSettings];
check["explicit empty groups preserve successful legacy completion with no averaging",
  bsCubicDisabled["Status"] === "COMPLETE" &&
    bsCubicDisabled["BoseSymmetry"]["Groups"] === {} &&
    bsCubicDisabled["BoseSymmetry"]["Order"] === 1 &&
    AllTrue[bsCubicDisabled["ValidationReport"]["Channels"], #["Status"] === "PASS" &]];
bsConflictModel = bsCubicModel;
bsConflictModel[[1]] = bsScalarAmbiguousModel[[1]];
bsConflictChannels = PrepareAmplitudes[bsCubicChannels, bsConflictModel,
  "ExternalParticles" -> {"r", "s", "s", "s"}, "ReferenceSpinors" -> None,
  "MaxSteps" -> 0, "ProgressReporting" -> False, "ReturnDetails" -> True];
bsConflict = CompleteFactorization[bsCubicPrepared, bsConflictChannels, bsConflictModel,
  "BoseSymmetryGroups" -> {}, Sequence @@ bsCubicSettings];
check["contradictory amplitude and channel process labels are rejected even with projection disabled",
  FailureQ[bsConflict] && bsConflict[[1]] === "InconsistentExternalParticles"];
bsMalformedNames = CompleteFactorization[0, bsCubicChannels, bsCubicModel,
  "ExternalParticles" -> "s", "BoseSymmetryGroups" -> {}, Sequence @@ bsCubicSettings];
check["a malformed direct process option is rejected before inference despite an empty-group opt-out",
  FailureQ[bsMalformedNames] && bsMalformedNames[[1]] === "InvalidExternalParticles"];

(* Process metadata is retained only in requested preparation reports. *)
mdModel = {{{"a", "a", "A", 1, 0, mdMassA, 0, 1, 1, "", ""},
  {"b", "b", "b", 2, 0, mdMassB, 0, 1, 1, "", ""},
  {"v", "v.+", "v.-", 3, 2, 0, 0, 1, 1, "", ""}}, {}, {}, {}};
mdRules = {Mass[1] -> mdMassA, Mass[2] -> mdMassA, Mass[3] -> 0, Mass[4] -> 0};
mdPair = {mdRules, 3}; mdNames = {"a", "A", "v.+", "v.-"};
mdPrepare[input_, opts___] := PrepareAmplitudes[input, mdModel, opts,
  "ReferenceSpinors" -> None, "MaxSteps" -> 0, "ProgressReporting" -> False];
mdDefault = mdPrepare[mdPair, "ReturnDetails" -> True];
mdDetail = mdPrepare[mdPair, "ExternalParticles" -> mdNames, "ReturnDetails" -> True];
check["Automatic process labels remain unspecified", !KeyExistsQ[mdDefault, "ExternalParticles"]];
check["ordered process labels are retained", mdDetail["ExternalParticles"] === mdNames];
check["antiparticle and helicity names are retained exactly",
  mdDetail["ExternalParticles"][[{2, 3, 4}]] === {"A", "v.+", "v.-"}];
check["metadata does not change the single-pair output shape",
  mdPrepare[mdPair, "ExternalParticles" -> mdNames] === mdPair];
check["metadata does not change the list-of-pairs output shape",
  mdPrepare[{mdPair}, "ExternalParticles" -> mdNames] === {mdPair}];
check["each preparation item carries the ordered process",
  First[mdDetail["ItemReports"]]["ExternalParticles"] === mdNames];
check["each retained remainder carries the ordered process",
  First[mdDetail["RemainderData"]]["ExternalParticles"] === mdNames];
check["process metadata does not alter declared masses",
  mdDetail["ExternalMassRules"] === mdRules && mdDetail["PreparedAmplitudes"] === {mdPair}];
check["process validation does not assign or alter model parameters",
  mdModel[[1, 1, 6]] === mdMassA && OwnValues[mdMassA] === {}];
check["a process list that is too short is rejected",
  FailureQ[mdPrepare[mdPair, "ExternalParticles" -> Most[mdNames]]]];
check["a process list that is too long is rejected",
  FailureQ[mdPrepare[mdPair, "ExternalParticles" -> Append[mdNames, "b"]]]];
check["an unknown particle name is rejected",
  FailureQ[mdPrepare[mdPair, "ExternalParticles" -> ReplacePart[mdNames, 3 -> "not-known"]]]];
check["a non-string particle name is rejected",
  FailureQ[mdPrepare[mdPair, "ExternalParticles" -> ReplacePart[mdNames, 3 -> 3]]]];
check["a non-list process value is rejected",
  FailureQ[mdPrepare[mdPair, "ExternalParticles" -> None]]];
check["a known particle with an incompatible mass is rejected",
  FailureQ[mdPrepare[mdPair, "ExternalParticles" -> ReplacePart[mdNames, 1 -> "b"]]]];
check["symbolically equal declared masses are accepted",
  mdPrepare[{mdRules /. mdMassA -> (mdMassA + mdMassB - mdMassB), 3},
    "ExternalParticles" -> mdNames] === mdPair];
mdChannel = {mdMassA, mdMassA, 0, 0, Multiparticle[1, 3], mdMassA, 3};
mdChannelDetail = mdPrepare[{mdChannel}, "ExternalParticles" -> mdNames, "ReturnDetails" -> True];
check["factorization-channel preparation retains the same process labels",
  mdChannelDetail["ExternalParticles"] === mdNames &&
    mdChannelDetail["PreparedAmplitudes"] === {mdChannel}];
check["empty process metadata is accepted on empty input",
  mdPrepare[{}, "ExternalParticles" -> {}, "ReturnDetails" -> True]["ExternalParticles"] === {}];
check["nonempty process metadata is rejected on empty input",
  FailureQ[mdPrepare[{}, "ExternalParticles" -> {"a"}]]];
mdFeynman = FeynmanModel @@ mdModel;
mdFeynmanDetail = PrepareAmplitudes[mdPair, mdFeynman, "ExternalParticles" -> mdNames,
  "ReferenceSpinors" -> None, "ProgressReporting" -> False, "ReturnDetails" -> True];
check["Feynman-model preparation retains ordered particle metadata",
  mdFeynmanDetail["ExternalParticles"] === mdNames &&
    mdFeynmanDetail["PreparedAmplitudes"] === {mdPair}];
mdP1 = ModelParameter["same", "metadata-one", "Real"];
mdP2 = ModelParameter["same", "metadata-two", "Real"];
mdForeignModel = mdModel /. mdMassA -> mdP1;
mdForeign = PrepareAmplitudes[{mdRules /. mdMassA -> mdP2, 3}, mdForeignModel,
  "ExternalParticles" -> mdNames, "ReferenceSpinors" -> None,
  "MaxSteps" -> 0, "ProgressReporting" -> False];
check["same printed mass names in different model scopes are not identified",
  FailureQ[mdForeign]];

If[checks =!= 53, AppendTo[failures, "The suite did not execute all 53 checks."]];
Print[checks, " Bose-state and particle-metadata checks; ", Length[failures], " failures."];
If[failures =!= {}, Print[failures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:bose-symmetry-tests.wl"];
Quit[0];
