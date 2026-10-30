(* Small physical source proposals retain the seed and every validation target. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0;
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);
csModel = ReadModel[FileNameJoin[{root, "models"}], 3];
csMass = ModelParameter[csModel, "Me"]; csCharge = ModelParameter[csModel, "EE"];
csParticles = {"e", "E", "A.+", "A.+"};
csChannels = FactorizationAmplitudeChannels[csParticles, csModel];
csRules = Thread[Mass /@ Range[4] -> {csMass, csMass, 0, 0}];
csAngle = SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]];
csSquare = SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity", "Square", 4]];
csCarrier = csCharge^2 csMass csAngle csSquare^2;
csPole13 = PropDen[Mom[Multiparticle[1, 3]], csMass];
csPole14 = PropDen[Mom[Multiparticle[1, 4]], csMass];
csSource = csCarrier/(csPole13 csPole14);
csOther = csSource MomProd[1, 3];
csRegular = csCarrier;
csInput = ConstructiveDiagrams`Private`completionNormalizeInput[
  {csRules, csRegular + csSource}, csChannels, csModel];
csBefore = csInput; csModelBefore = csModel;
csPlan = ConstructiveDiagrams`Private`completionExchangePlan[
  Join[csInput, <|"ExternalParticles" -> csParticles|>], csModel, Automatic, Automatic, csParticles];
csBuild[sources_, cap_: 16, bound_: 64] :=
  ConstructiveDiagrams`Private`completionReducedPhysicalSourceBasis[
    csInput, <|"Sources" -> sources|>, csPlan, bound, cap, 10];
csReport = csBuild[{csSource, csOther}];
check["reduced physical channel sources form a named finite proposal",
  csReport["Status"] === "AVAILABLE" && csReport["CandidateMode"] === "ReducedPhysicalSourceAtoms" &&
    Length[csReport["Basis"]] === 2 && csReport["SourceCount"] === 2];
check["explicit regular input stays fixed rather than becoming a correction direction",
  csReport["RegularExpression"] === csRegular &&
    AllTrue[csReport["Basis"], !FreeQ[#, Power[_PropDen, _Integer?Negative]] &]];
check["every proposed direction contains only original physical denominators",
  AllTrue[csReport["Basis"], ConstructiveDiagrams`Private`completionPhysicalDenominatorOnlyQ[#, csInput] &]];
check["every proposed direction has supported simple physical residues",
  AllTrue[csReport["Basis"], ConstructiveDiagrams`Private`completionHigherPhysicalPoles[
    #, ConstructiveDiagrams`Private`completionTargets[csInput["Targets"]], Range[4]] === {} &]];
check["source construction changes neither model nor original residue obligations",
  csInput === csBefore && csModel === csModelBefore];
csCap = csBuild[{csSource, csOther}, 1];
check["an insufficient correction cap does not truncate the proposal",
  csCap["Status"] === "UNAVAILABLE" && csCap["Basis"] === {} &&
    csCap["ProjectedDirectionCount"] === 2 && !TrueQ[csCap["WithinLimit"]]];
csSpurious = csBuild[{csSource, csSource/SpinorChain[
  Spinor["Helicity", "Angle", 3], Spinor["Helicity", "Angle", 4]]}];
check["an apparent spinor-bracket pole cannot enter the physical-only source proposal",
  csSpurious["Status"] === "AVAILABLE" && csSpurious["SourceCount"] === 1];
csHigher = csBuild[{csSource, csSource/csPole13}];
check["higher physical pole powers are kept outside the simple-residue proposal",
  csHigher["Status"] === "AVAILABLE" && csHigher["SourceCount"] === 1];
csXi = csBuild[{csSource, csSource SpinorChain[
  Spinor["Xi", "Angle", 90], Spinor["Helicity", "Angle", 3]]}];
check["unresolved auxiliary spinors cannot enter a closed physical source proposal",
  csXi["Status"] === "AVAILABLE" && csXi["SourceCount"] === 1];
csEmpty = csBuild[{}];
check["an absent reduced source catalog stays explicitly unavailable",
  csEmpty["Status"] === "UNAVAILABLE" && csEmpty["Basis"] === {}];
(* The first selected-cut source proposal can defer optional invariant pivots. *)
csTarget = First[ConstructiveDiagrams`Private`completionTargets[csInput["Targets"]]];
csReferences = <|"Variants" -> {<|"Accepted" -> True, "Cut" -> csTarget["Cut"],
  "Mass" -> csTarget["Mass"], "Mode" -> "AllPhysical", "Expression" -> csSource|>}|>;
csParameterRules = ConstructiveDiagrams`Private`residueParameterRules[csModel, Automatic];
csInvariantCalls = 0;
csOriginalSelected = Block[{ConstructiveDiagrams`Private`completionCutInvariantTrials =
    Function[{expression, cut, mass, rules}, csInvariantCalls++; {}]},
  ConstructiveDiagrams`Private`completionSelectedChannelBasis[
    csInput, csReferences, 128, csParameterRules, 1729, 10, False]];
check["the first selected-cut proposal can omit invariant pivots without claiming they were tested",
  AssociationQ[csOriginalSelected] && csInvariantCalls === 0 &&
    csOriginalSelected["InvariantContinuationsRequested"] === False &&
    MissingQ[csOriginalSelected["InvariantContinuationsComplete"]]];
check["deferring invariant pivots retains whole original sources and their own-cut certificates",
  AssociationQ[csOriginalSelected] && Length[csOriginalSelected["Sources"]] > 0 &&
    AllTrue[csOriginalSelected["Verification"], #["Verification"]["Status"] === "PASS" &]];
csInvariantCalls = 0;
csDefaultSelected = Block[{ConstructiveDiagrams`Private`completionCutInvariantTrials =
    Function[{expression, cut, mass, rules}, csInvariantCalls++; {}]},
  ConstructiveDiagrams`Private`completionSelectedChannelBasis[
    csInput, csReferences, 128, csParameterRules, 1729, 10]];
check["the established selected-cut behavior still includes invariant pivots by default",
  AssociationQ[csDefaultSelected] && csInvariantCalls > 0 &&
    csDefaultSelected["InvariantContinuationsRequested"] === True];
csGrouped = ConstructiveDiagrams`Private`completionReducedPhysicalSourceBasis[
  csInput, <|"Sources" -> {csSource, csOther}|>, csPlan, 128, 16, 10, csOriginalSelected];
check["verified selected-cut whole sources are leading correlated fit coordinates",
  csGrouped["Status"] === "AVAILABLE" && csGrouped["CandidateMode"] === "GroupedSelectedPhysicalSources" &&
    Length[csGrouped["GroupedSelectedControls"]] > 0 &&
    First[csGrouped["Basis"]] === First[csGrouped["GroupedSelectedControls"]]];
check["the grouped source proposal retains selected-cut verification and fixed regular input",
  AssociationQ[csGrouped["SourceVerification"]] &&
    csGrouped["SourceVerification"]["InvariantContinuationsRequested"] === False &&
    csGrouped["RegularExpression"] === csRegular &&
    AllTrue[csGrouped["Basis"], ConstructiveDiagrams`Private`completionPhysicalDenominatorOnlyQ[#, csInput] &]];
csGroupedCap = ConstructiveDiagrams`Private`completionReducedPhysicalSourceBasis[
  csInput, <|"Sources" -> {csSource, csOther}|>, csPlan, 128, 1, 10, csOriginalSelected];
check["the complete grouped-plus-atomic proposal is refused rather than truncated under a cap",
  csGroupedCap["Status"] === "UNAVAILABLE" && csGroupedCap["Basis"] === {} &&
    csGroupedCap["ProjectedDirectionCount"] === 2 && csInput === csBefore];
csCorrelated = csSource (1 + csTarget["Propagator"]/csMass^2);
csCorrelatedCheck = ConstructiveDiagrams`Private`completionContinuationCheck[
  csSource, csCorrelated, csTarget,
  ConstructiveDiagrams`Private`completionTargets[csInput["Targets"]], csParameterRules, 1729];
check["the correlated whole-source fixture preserves its selected residue by exact cut checks",
  csCorrelatedCheck["Status"] === "PASS"];
csCorrelatedSelected = <|"Sources" -> {csCorrelated},
  "Verification" -> {<|"Cut" -> csTarget["Cut"], "Verification" -> csCorrelatedCheck|>},
  "OriginalSourcesComplete" -> True, "InvariantContinuationsRequested" -> False,
  "InvariantContinuationsComplete" -> Missing["NotRequested"],
  "Scope" -> "SelectedCutPreparedChannelTermsWithUnchangedTargets"|>;
csCorrelatedProposal = ConstructiveDiagrams`Private`completionReducedPhysicalSourceBasis[
  csInput, <|"Sources" -> {csSource, csOther}|>, csPlan, 128, 16, 10, csCorrelatedSelected];
check["a correlated multi-term continuation remains one leading coordinate rather than separate monomial weights",
  csCorrelatedProposal["Status"] === "AVAILABLE" &&
    Length[csCorrelatedProposal["GroupedSelectedControls"]] === 1 &&
    ConstructiveDiagrams`Private`rspSandwichExpansionCount[
      First[csCorrelatedProposal["GroupedSelectedControls"]]] > 1 &&
    First[csCorrelatedProposal["Basis"]] === First[csCorrelatedProposal["GroupedSelectedControls"]]];
Print["COMPACT PHYSICAL SOURCE CHECKS: ", checks, "; FAILURES: ", Length[failures]];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:compact-physical-source-tests.wl"], Print[failures]];
Quit[If[failures === {}, 0, 1]];
