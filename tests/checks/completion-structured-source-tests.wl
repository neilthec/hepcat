(* Completion-only support certificates must never hide another singularity,
   remove an affine coordinate, or change a custom-map restriction. *)

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
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
checks = 0; failures = {}; SetAttributes[check, HoldRest];
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);
csModel = {{{"Scalar", "s", "s", 1, 0, csMass, 0, 1, 1, "", ""}},
  {{"s", "s", "s", "", csCoupling, 1}}, {{csMass, 1, "mass"}, {csCoupling, 2, "coupling"}}, {}};
csChannels = FactorizationAmplitudeChannels[ConstantArray["s", 4], csModel];
csNorm = ConstructiveDiagrams`Private`completionNormalizeInput[0, csChannels, csModel];
csTargets = ConstructiveDiagrams`Private`completionTargets[csNorm["Targets"]];
csSelected = SelectFirst[csTargets, #["Cut"] === {1, 2} &];
csOther = SelectFirst[csTargets, #["Cut"] === {1, 3} &];
csPole = csSelected["Propagator"]; csOtherPole = csOther["Propagator"];
csRules = {csMass -> 1, csCoupling -> 2};
csChart = ConstructiveDiagrams`Private`completionCutChart[csSelected, 1, 1729, csRules];
csProfile = ConstructiveDiagrams`Private`completionComponentPoleProfile;
csRegular = ConstructiveDiagrams`Private`completionCutRegularityEvaluator[csSelected, csChart];
check["the test chart validates the selected cut", AssociationQ[csChart] && TrueQ[csChart["AutomaticCoherentChart"]]];
check["diagonal exact-cut frames retain massive normalization and reconstruct every external momentum",
  AllTrue[Keys[csChart["Chart"]["AngleBasis"]],Function[leg,
    ConstructiveDiagrams`Private`residueExactZero[Det[csChart["Chart"]["AngleBasis"][leg]]-csChart["Chart"]["Masses"][leg]] &&
    ConstructiveDiagrams`Private`residueExactZero[Det[csChart["Chart"]["SquareBasis"][leg]]-csChart["Chart"]["Masses"][leg]] &&
    ConstructiveDiagrams`Private`residueExactZero[csChart["Chart"]["AngleBasis"][leg].{{0,1},{-1,0}}.
      Transpose[csChart["Chart"]["SquareBasis"][leg]]-csChart["Chart"]["Momenta"][leg]]]]];
check["a different explicit propagator is regular on this chart", csRegular[csProfile[2/csOtherPole, Range[4]]]];
check["a polynomial numerator cannot create a missing pole", csRegular[csProfile[(1 + MomProd[1, 3]^2)/csOtherPole, Range[4]]]];
check["a declared selected pole cannot use the zero shortcut", !csRegular[csProfile[1/csPole, Range[4]]]];
check["the selected complement is still the selected pole", !csRegular[csProfile[1/PropDen[Mom[Multiparticle @@ csSelected["Complement"]], csMass], Range[4]]]];
check["a hidden scalar-invariant inverse is unsupported", !TrueQ[csProfile[1/(csOtherPole MomProd[1, 2]), Range[4]]["Supported"]]];
csBracket = SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]];
check["a hidden spinor inverse is unsupported", !TrueQ[csProfile[1/(csOtherPole csBracket), Range[4]]["Supported"]]];
check["an unknown function around a numerator is unsupported", !TrueQ[csProfile[csUnknown[csBracket]/csOtherPole, Range[4]]["Supported"]]];
check["reference spinors remain on the original evaluator", !TrueQ[csProfile[SpinorChain[Spinor["Xi", "Angle", 1], Spinor["Spin", "Angle", 2]]/csOtherPole, Range[4]]["Supported"]]];
check["unknown external labels are unsupported", !TrueQ[csProfile[MomProd[1, 9]/csOtherPole, Range[4]]["Supported"]]];
check["internal composite spinors are unsupported", !TrueQ[csProfile[SpinorChain[Spinor["Helicity", "Angle", Multiparticle[1, 2]], Spinor["Helicity", "Angle", 3]]/csOtherPole, Range[4]]["Supported"]]];
check["propagators with unsupported width data remain on the original path", !TrueQ[csProfile[1/PropDen[Mom[Multiparticle[1, 3]], csMass, csWidth], Range[4]]["Supported"]]];
check["an unresolved inverse coefficient cannot establish regularity", !csRegular[csProfile[1/(csOtherPole csUnknownMass), Range[4]]]];
csKnownChart = Join[csChart, <|"ParameterRules" -> Append[csRules, csUnknownMass -> 3]|>];
check["an exactly nonzero coefficient is verified at its chart", ConstructiveDiagrams`Private`completionCutRegularityEvaluator[csSelected, csKnownChart][csProfile[1/(csOtherPole csUnknownMass), Range[4]]]];
csZeroChart = Join[csChart, <|"ParameterRules" -> Append[csRules, csUnknownMass -> 0]|>];
check["a vanishing coefficient inverse cannot fabricate zero", !ConstructiveDiagrams`Private`completionCutRegularityEvaluator[csSelected, csZeroChart][csProfile[1/(csOtherPole csUnknownMass), Range[4]]]];
csCoincident = PropDen[Mom[Multiparticle @@ csSelected["Cut"]], csAliasMass];
csCoincidentChart = Join[csChart, <|"ParameterRules" -> Append[csRules, csAliasMass -> 1]|>];
check["another mass label coinciding on the cut falls back", !ConstructiveDiagrams`Private`completionCutRegularityEvaluator[csSelected, csCoincidentChart][csProfile[1/csCoincident, Range[4]]]];
csCustomChart = KeyDrop[csChart, "AutomaticCoherentChart"];
check["custom maps never inherit structural regularity", !ConstructiveDiagrams`Private`completionCutRegularityEvaluator[csSelected, csCustomChart][csProfile[1/csOtherPole, Range[4]]]];
csSingularChart = Join[csChart, <|"Chart" -> Join[csChart["Chart"],
  <|"Angle" -> Join[csChart["Chart"]["Angle"], <|1 -> {1/csChart["Chart"]["PoleVariable"], 1}|>]|>]|>];
Quiet[check["saved automatic charts with singular frozen spinors decline the shortcut",
  !ConstructiveDiagrams`Private`completionCutRegularityEvaluator[csSelected, csSingularChart][csProfile[csBracket/csOtherPole, Range[4]]]], {Power::infy, Infinity::indet}];
csUnmappedChart = Join[csChart, <|"Chart" -> Join[csChart["Chart"],
  <|"Square" -> Join[csChart["Chart"]["Square"], <|1 -> {Missing["UnmappedSpinor"], 1}|>]|>]|>];
check["unmapped frozen spinors remain on the exact evaluator",
  !ConstructiveDiagrams`Private`completionCutRegularityEvaluator[csSelected, csUnmappedChart][csProfile[csBracket/csOtherPole, Range[4]]]];
csComponents = {0, 1/csPole, 2/csOtherPole, csBracket/csOtherPole};
csUnknowns = {csA, csB, csC};
csProfiles = csProfile[#, Range[4]] & /@ csComponents;
csFast = ConstructiveDiagrams`Private`completionLinearResidue[csComponents, csUnknowns, csSelected, csTargets, csChart, 10, csProfiles];
csExact = ConstructiveDiagrams`Private`completionLinearResidue[csComponents, csUnknowns, csSelected, csTargets, csCustomChart, 10];
check["support-aware restrictions equal the full mapped residue", csFast["Status"] === "PASS" && csExact["Status"] === "PASS" && ConstructiveDiagrams`Private`residueExactZero[csFast["PhysicalResidue"] - csExact["PhysicalResidue"]]];
check["zero-residue coordinates remain represented", TrueQ[csFast["ComponentCoordinatesPreserved"]] && csFast["PhysicalResidue"] === csA && Length[csFast["ComponentRestrictionMethods"]] === Length[csComponents]];
check["proven regular components have explicit provenance", csFast["StructurallyRegularComponents"] === 3 && Count[csFast["ComponentRestrictionMethods"], "TypedRegularNoSelectedPole"] === 3];
csHidden = 1/((Mandelstahm @@ csSelected["Cut"]) - csMass^2);
csHiddenComponents = {0, csHidden};
csHiddenResult = ConstructiveDiagrams`Private`completionLinearResidue[csHiddenComponents, {csA}, csSelected, csTargets, csChart, 10, csProfile[#, Range[4]] & /@ csHiddenComponents];
check["a hidden selected pole reaches the original exact evaluator", csHiddenResult["Status"] === "PASS" && csHiddenResult["PhysicalResidue"] === csA && Last[csHiddenResult["ComponentRestrictionMethods"]] =!= "TypedRegularNoSelectedPole"];
csBatch = Join[csComponents, Table[(i + 1)/csOtherPole, {i, 6}]];
csStopped = ConstructiveDiagrams`Private`completionComponentPoleProfiles[csBatch, Range[4], 0];
check["expired profile construction preserves every coordinate", Length[csStopped] === Length[csBatch] && AllTrue[csStopped, !TrueQ[#["Supported"]] &]];
csComplete = ConstructiveDiagrams`Private`completionComponentPoleProfiles[csBatch, Range[4], 5];
check["a completed profile batch keeps every supported column", Length[csComplete] === Length[csBatch] && AllTrue[csComplete, TrueQ[#["Supported"]] &]];
(* The grouped constructor keeps complete catalogs and authoritative limits. *)
csStructuredSeed = 7 + 1/csPole + (2 + MomProd[1, 3])/csOtherPole;
csStructuredNorm = ConstructiveDiagrams`Private`completionNormalizeInput[csStructuredSeed, csChannels, csModel];
csStructuredNorm = Join[csStructuredNorm, <|"RemainderData" -> {
  <|"ShellRemainder" -> (3 + MomProd[1, 4])/csPole|>}|>];
csPlan = ConstructiveDiagrams`Private`completionExchangePlan[csStructuredNorm, csModel, {}, {}, Automatic];
csConservationSeed = <|"Basis"->{MomProd[1,3]/csPole},"RegularExpression"->7|>;
csConservation = Block[{ConstructiveDiagrams`Private`$completionAutomaticCorrectionTermCeiling=None},
  ConstructiveDiagrams`Private`completionMomentumConservationBasis[
    csConservationSeed,csStructuredNorm,csPlan,32,32,5]];
check["ordinary conservation closure exposes new source-derived numerator directions",
  csConservation["Status"] === "AVAILABLE" && Length[csConservation["Basis"]]>1 &&
    TrueQ[csConservation["OrdinaryRelationQuotient"]["OriginalSourceSpanPreserved"]]];
check["conservation closure preserves fixed regular input and every physical denominator",
  csConservation["RegularExpression"] === 7 &&
    AllTrue[csConservation["Basis"],ConstructiveDiagrams`Private`completionPhysicalDenominatorOnlyQ[#,csStructuredNorm]&]];
check["the conservation source catalog retains ordinary numerator-only replay provenance",
  csConservation["RewriteRoutes"] =!= {} && AllTrue[csConservation["RewriteRoutes"],
    TrueQ[#["NumeratorOnly"]] && #["InternalOnShellIdentity"] === False &]];
csConservationQuotient=csConservation["OrdinaryRelationQuotient"];
check["known construction identities remove redundant source coordinates without sampled pruning",
  csConservationQuotient["Status"] === "COMPLETE" && csConservationQuotient["PrunedDirections"]>0 &&
    TrueQ[csConservationQuotient["ReconstructionConstraintsVerified"]] &&
    csConservationQuotient["NumericalKinematicsUsed"] === False &&
    csConservationQuotient["ModelParameterSpecializationUsed"] === False];
csConservationReplay=csConservationQuotient["OriginalBasis"] -
  csConservationQuotient["ReconstructionMatrix"].csConservationQuotient["Basis"];
check["a separate exact conserved chart verifies every retained source reconstruction",
  ConstructiveDiagrams`Private`residueExactZero[
    csChart["Map"][csConservationReplay,{csSelected},{csConservationTestPole}]]];
check["the completion proposal certifies reconstruction of every original source direction",
  TrueQ[csConservation["OriginalSourceSpanPreserved"]] &&
    TrueQ[csConservation["SourceSpanCertificate"]["SourceColumnsInOriginalUnion"]] &&
    TrueQ[csConservation["SourceSpanCertificate"]["ReconstructionConstraintsVerified"]]];
csConservationAdmission=<|"Status"->"AVAILABLE","Basis"->{csA},
  "RawDirectionCount"->8,"InitialDirections"->4,"OriginalSourceSpanPreserved"->True,
  "SourceSpanCertificate"-><|"ReconstructionConstraintsVerified"->True|>|>;
check["an exact quotient may shrink the basis while exposing new correction directions",
  ConstructiveDiagrams`Private`completionConservationSpaceUsableQ[csConservationAdmission]];
check["missing ordinary reconstruction prevents a conservation fallback",
  !ConstructiveDiagrams`Private`completionConservationSpaceUsableQ[
    KeyDrop[csConservationAdmission,"SourceSpanCertificate"]]];
check["a false source-span certificate cannot admit the fallback",
  !ConstructiveDiagrams`Private`completionConservationSpaceUsableQ[
    Join[csConservationAdmission,<|"OriginalSourceSpanPreserved"->False|>]]];
check["an unchanged source union does not retry an already inconsistent space",
  !ConstructiveDiagrams`Private`completionConservationSpaceUsableQ[
    Join[csConservationAdmission,<|"RawDirectionCount"->4|>]]];
csConservationSmall = Block[{ConstructiveDiagrams`Private`$completionAutomaticCorrectionTermCeiling=None},
  ConstructiveDiagrams`Private`completionMomentumConservationBasis[
    csConservationSeed,csStructuredNorm,csPlan,2,2,5]];
check["an explicit conservation-space cap rejects the whole proposal without trimming",
  csConservationSmall["Status"] === "UNAVAILABLE" && csConservationSmall["Basis"] === {} &&
    csConservationSmall["EffectiveMaxCorrectionTerms"] === 2];
csConservationAutomatic = Block[{ConstructiveDiagrams`Private`$completionAutomaticCorrectionTermCeiling=32},
  ConstructiveDiagrams`Private`completionMomentumConservationBasis[
    csConservationSeed,csStructuredNorm,csPlan,2,2,5]];
check["automatic admission reuses the complete conservation union inside its granted ceiling",
  csConservationAutomatic["Status"] === "AVAILABLE" &&
    csConservationAutomatic["Basis"] === csConservation["Basis"] &&
    csConservationAutomatic["EffectiveMaxCorrectionTerms"] >= Length[csConservation["Basis"]] &&
    Lookup[csConservationAutomatic["CapChanges"],"Operation"] === {"ReuseMomentumConservationClosure"}];
csPreparedChannels = <|"Model" -> csModel, "OriginalAmplitudes" -> csChannels, "PreparedAmplitudes" -> csChannels|>;
csStructured = Block[{ConstructiveDiagrams`Private`$completionAutomaticCorrectionTermCeiling = None},
  ConstructiveDiagrams`Private`completionStructuredPhysicalSourceBasis[
    csStructuredNorm, csPreparedChannels, csPlan, 64, 32, 10]];
check["complete prepared source catalogs produce an available finite proposal", csStructured["Status"] === "AVAILABLE" && TrueQ[csStructured["SourceCatalogComplete"]]];
check["explicit regular input remains fixed at its original coefficient", csStructured["RegularExpression"] === 7];
check["every original channel remains an independent target obligation", csStructured["OriginalTargetCount"] === Length[csTargets] && TrueQ[csStructured["AllOriginalTargetsRetained"]]];
check["the insertion grouping is not mislabeled as full momentum degree", csStructured["Grouping"] === "ChainMomentumInsertionDegree"];
check["ordinary structured normalization adds no internal shell conditions", csStructured["InternalOnShellConditions"] === {} && csStructured["ColumnNormalization"]["InternalOnShellConditions"] === {}];
check["retained source remainders remain distinguishable", AnyTrue[Lookup[csStructured["Sources"], "Source"], AssociationQ[#] && Lookup[#, "Type", None] === "RetainedShellRemainder" &]];
csNoHistory = ConstructiveDiagrams`Private`completionStructuredPhysicalSourceBasis[
  KeyDrop[csStructuredNorm, "RemainderData"], csPreparedChannels, csPlan, 64, 32, 10];
check["excluding preparation history excludes every retained shell source", !AnyTrue[Lookup[csNoHistory["Sources"], "Source"], AssociationQ[#] && Lookup[#, "Type", None] === "RetainedShellRemainder" &]];
csEmptyHistoryCheck = Quiet[Check[
  ConstructiveDiagrams`Private`completionStructuredPhysicalSourceBasis[
    KeyDrop[csStructuredNorm, "RemainderData"], csPreparedChannels, csPlan, 64, 32, 10],
  $Failed, {Do::iterb}], {Do::iterb}];
check["an absent remainder history remains an empty iterable without messages",
  AssociationQ[csEmptyHistoryCheck] && csEmptyHistoryCheck["Status"] === "AVAILABLE"];
csOriginalNorm = csStructuredNorm;
csExpired = ConstructiveDiagrams`Private`completionStructuredPhysicalSourceBasis[
  csStructuredNorm, csPreparedChannels, csPlan, 64, 32, 0];
check["an expired source-catalog budget cannot claim a completed prefix", csExpired["Status"] === "UNAVAILABLE" && !TrueQ[csExpired["SourceCatalogComplete"]] && csExpired["Basis"] === {}];
check["failed source attempts leave the supplied input untouched", csStructuredNorm === csOriginalNorm];
csConstructionOverflow = ConstructiveDiagrams`Private`completionStructuredPhysicalSourceBasis[
  csStructuredNorm, csPreparedChannels, csPlan, 1, 32, 5];
check["construction-cap overflow rejects the complete proposal", csConstructionOverflow["Status"] === "UNAVAILABLE" && csConstructionOverflow["Basis"] === {} && !TrueQ[csConstructionOverflow["WithinLimit"]]];
csFinalOverflow = Block[{ConstructiveDiagrams`Private`$completionAutomaticCorrectionTermCeiling = None},
  ConstructiveDiagrams`Private`completionStructuredPhysicalSourceBasis[
    csStructuredNorm, csPreparedChannels, csPlan, 64, 1, 10]];
check["an explicit final cap cannot grow or admit a truncated basis", csFinalOverflow["Status"] === "UNAVAILABLE" && csFinalOverflow["Basis"] === {} && csFinalOverflow["EffectiveMaxCorrectionTerms"] === 1 && csFinalOverflow["CapChanges"] === {}];
csAutoAdmission = Block[{ConstructiveDiagrams`Private`$completionAutomaticCorrectionTermCeiling = 32},
  ConstructiveDiagrams`Private`completionStructuredPhysicalSourceBasis[
    csStructuredNorm, csPreparedChannels, csPlan, 64, 1, 10]];
check["automatic admission reuses all completed columns", csAutoAdmission["Status"] === "AVAILABLE" && Length[csAutoAdmission["Basis"]] === Length[csStructured["Basis"]] && csAutoAdmission["EffectiveMaxCorrectionTerms"] >= Length[csAutoAdmission["Basis"]] && csAutoAdmission["CapChanges"] =!= {}];
csOptionalTimeout = Internal`InheritedBlock[{ConstructiveDiagrams`Private`completionNormalizePhysicalColumns},
  DownValues[ConstructiveDiagrams`Private`completionNormalizePhysicalColumns] = {};
  ConstructiveDiagrams`Private`completionNormalizePhysicalColumns[args___] := Pause[3];
  ConstructiveDiagrams`Private`completionStructuredPhysicalSourceBasis[
    csStructuredNorm, csPreparedChannels, csPlan, 64, 32, 1, "Normalized"]];
check["optional reduction timeout retains the complete source checkpoint", csOptionalTimeout["Status"] === "AVAILABLE" && TrueQ[csOptionalTimeout["SourceCatalogComplete"]] && !TrueQ[csOptionalTimeout["OptionalReductionCompleted"]] && Length[csOptionalTimeout["Basis"]] >= Length[csStructured["Basis"]]];
(* Original presentations stay distinct until the ordinary fit-time
   canonicalizer; eligibility is checked on their physical canonical copies. *)
csComplementPole = PropDen[Mom[Multiparticle @@ csSelected["Complement"]], csMass];
csPresentationExpression = (1 + csBracket)/csComplementPole;
csPresentationNorm = ConstructiveDiagrams`Private`completionNormalizeInput[
  csPresentationExpression, csChannels, csModel];
csPresentationNorm = Join[csPresentationNorm, <|"Records" -> {
  <|"Expression" -> csPresentationExpression|>}, "RemainderData" -> {<|"ShellRemainder" -> csPresentationExpression|>}|>];
csPresentation = ConstructiveDiagrams`Private`completionStructuredPhysicalSourceBasis[
  csPresentationNorm, <||>, csPlan, 64, 32, 10];
csRetainedPresentation = SelectFirst[csPresentation["Sources"], AssociationQ[#["Source"]] &];
check["retained remainder presentation survives canonical eligibility", csPresentation["Status"] === "AVAILABLE" &&
  csPresentation["SourcePresentation"] === "Original" &&
  csRetainedPresentation["Expression"] === csPresentationExpression &&
  csRetainedPresentation["CanonicalExpression"] =!= csPresentationExpression &&
  !FreeQ[csPresentation["Basis"], csComplementPole]];
check["prepared whole sources keep their established physical canonical form",
  First[csPresentation["Sources"]]["Expression"] === First[csPresentation["Sources"]]["CanonicalExpression"] &&
  First[csPresentation["Sources"]]["SuppliedExpression"] === csPresentationExpression];
check["original proposal performs no eager ordinary column normalization",
  csPresentation["ColumnNormalization"]["Status"] === "NOT_REQUESTED" &&
  csPresentation["ColumnNormalization"]["Processed"] === 0];
csReusableCatalog = csPresentation["CompleteSourceCatalog"];
csNormalizedPresentation = ConstructiveDiagrams`Private`completionStructuredPhysicalSourceBasis[
  csPresentationNorm, <||>, csPlan, 64, 32, 10, "Normalized", csReusableCatalog];
check["normalized fallback reuses the complete verified source catalog",
  csNormalizedPresentation["Status"] === "AVAILABLE" && csNormalizedPresentation["SourceCatalogReused"] &&
  csNormalizedPresentation["SourcePresentation"] === "Normalized" &&
  csNormalizedPresentation["OriginalTargetCount"] === csPresentation["OriginalTargetCount"]];
check["both source presentations retain the same physical functional directions",
  AllTrue[csPresentation["Basis"], Function[originalColumn, AnyTrue[csNormalizedPresentation["Basis"],
    Function[normalizedColumn, ConstructiveDiagrams`Private`residueExactZero[
      ConstructiveDiagrams`Private`completionCanonicalReduce[originalColumn - normalizedColumn,
        csPresentationNorm["ExternalMassRules"]]]]]]]];
csPresentationCapped = Block[{ConstructiveDiagrams`Private`$completionAutomaticCorrectionTermCeiling = None},
  ConstructiveDiagrams`Private`completionStructuredPhysicalSourceBasis[
    csPresentationNorm, <||>, csPlan, 64, 1, 10]];
check["an explicit cap rejects the whole original coordinate proposal while retaining its catalog",
  csPresentationCapped["Status"] === "UNAVAILABLE" && csPresentationCapped["Basis"] === {} &&
  csPresentationCapped["EffectiveMaxCorrectionTerms"] === 1 && csPresentationCapped["CapChanges"] === {} &&
  csPresentationCapped["CompleteSourceCatalog"]["Status"] === "COMPLETE"];
csNormalizedCapped = Block[{ConstructiveDiagrams`Private`$completionAutomaticCorrectionTermCeiling = None},
  ConstructiveDiagrams`Private`completionStructuredPhysicalSourceBasis[
    csPresentationNorm, <||>, csPlan, 64, 1, 10, "Normalized", csPresentationCapped["CompleteSourceCatalog"]]];
check["a normalized fallback may fit the same explicit cap without admitting or truncating the original proposal",
  csNormalizedCapped["Status"] === "AVAILABLE" && Length[csNormalizedCapped["Basis"]] === 1 &&
  csNormalizedCapped["EffectiveMaxCorrectionTerms"] === 1 && csNormalizedCapped["CapChanges"] === {} &&
  csNormalizedCapped["SourceCatalogReused"]];
(* Catalog identity depends on consumed source data, not the unrelated
   preparation histories that can be far larger than all usable remainders. *)
csHistoryOnlyNorm = Join[csPresentationNorm, <|"RemainderData" ->
  (Join[#, <|"ReferenceAlternatives" -> Range[100000],
      "UnrelatedDiagnostic" -> "Changed retained history"|>] & /@
    csPresentationNorm["RemainderData"])|>];
csHistoryReuse = ConstructiveDiagrams`Private`completionStructuredPhysicalSourceBasis[
  csHistoryOnlyNorm, <||>, csPlan, 64, 32, 10, "Original", csReusableCatalog];
check["unconsumed preparation history cannot invalidate the complete source catalog",
  csHistoryReuse["Status"] === "AVAILABLE" && csHistoryReuse["SourceCatalogReused"] &&
  csHistoryReuse["Basis"] === csPresentation["Basis"] &&
  csHistoryReuse["CompleteSourceCatalog"]["InputSignature"] === csReusableCatalog["InputSignature"]];
check["the catalog fingerprint explicitly names its consumed source scope",
  csHistoryReuse["CatalogInputFingerprintScope"] === "PreparedRecordsTargetsAndOrderedConsumedShellRemainders"];
csChangedShellNorm = Join[csHistoryOnlyNorm, <|"RemainderData" ->
  {<|"ShellRemainder" -> 2 csPresentationExpression|>}|>];
csChangedShellCatalog = ConstructiveDiagrams`Private`completionStructuredPhysicalSourceBasis[
  csChangedShellNorm, <||>, csPlan, 64, 32, 10, "Original", csReusableCatalog];
check["a changed consumed shell remainder invalidates the reusable catalog",
  csChangedShellCatalog["Status"] === "AVAILABLE" && !csChangedShellCatalog["SourceCatalogReused"] &&
  csChangedShellCatalog["CompleteSourceCatalog"]["InputSignature"] =!= csReusableCatalog["InputSignature"] &&
  csChangedShellCatalog["AllOriginalTargetsRetained"]];
csMissingShellNorm = Join[csPresentationNorm, <|"RemainderData" ->
  {<|"HistoryOnly" -> 1|>, First[csPresentationNorm["RemainderData"]]}|>];
csMissingShellCatalog = ConstructiveDiagrams`Private`completionStructuredPhysicalSourceBasis[
  csMissingShellNorm, <||>, csPlan, 64, 32, 10, "Original", csReusableCatalog];
check["missing shell records preserve the retained source origin index",
  csMissingShellCatalog["Status"] === "AVAILABLE" &&
  SelectFirst[csMissingShellCatalog["Sources"], AssociationQ[#["Source"]] &]["Source"]["Item"] === 2];
csCorruptCatalog = Join[csReusableCatalog, <|"Sources" -> {}|>];
csRebuiltCatalog = ConstructiveDiagrams`Private`completionStructuredPhysicalSourceBasis[
  csPresentationNorm, <||>, csPlan, 64, 32, 10, "Normalized", csCorruptCatalog];
check["a damaged cached catalog is recollected rather than trusted",
  csRebuiltCatalog["Status"] === "AVAILABLE" && !csRebuiltCatalog["SourceCatalogReused"] &&
  csRebuiltCatalog["SourceCount"] === csPresentation["SourceCount"]];
csUnsupportedPresentation = Join[csPresentationNorm, <|"Records" -> {
  <|"Expression" -> csPresentationExpression|>,
  <|"Expression" -> SpinorChain[Spinor["Xi", "Angle", 0], Spinor["Helicity", "Angle", 3]]/csPole|>}|>];
csSupportedSubset = ConstructiveDiagrams`Private`completionStructuredPhysicalSourceBasis[
  csUnsupportedPresentation, <||>, csPlan, 64, 32, 10];
check["unsupported source presentation remains an explicit rejection and does not hide obligations",
  csSupportedSubset["Status"] === "AVAILABLE" && Length[csSupportedSubset["RejectedSources"]] === 1 &&
  csSupportedSubset["OriginalTargetCount"] === Length[csTargets] && csSupportedSubset["AllOriginalTargetsRetained"]];
(* A real four-fermion input makes the structural gate eligible without a
   particle-name exception. Repetition here is a deliberate normalization test. *)
csQED = ReadModel[FileNameJoin[{root, "models"}], 3];
csQEDProcess = {"e", "E", "E", "e"};
csQEDPairs = PrepareAmplitudes[DiagramAmplitudes[CreateDiagrams[csQEDProcess, csQED]], csQED,
  "PolePreference" -> None, "MaxSteps" -> 0, "Parallelization" -> False, "ProgressReporting" -> False];
csQEDChannels = FactorizationAmplitudeChannels[csQEDProcess, csQED];
csRepeatedPairs = Flatten[ConstantArray[csQEDPairs, 6], 1];
csRepeatedExpression = Total[Last /@ csRepeatedPairs];
csRepeated = <|"PreparedAmplitudes" -> csRepeatedPairs, "ExternalParticles" -> csQEDProcess,
  "RemainderData" -> ConstantArray[<|"ShellRemainder" -> csRepeatedExpression|>, Length[csRepeatedPairs]]|>;
csCFOptions = {"PolePreference" -> "AllPhysical", "InputReweighting" -> None,
  "PreparationRemainders" -> None, "ChannelCompanions" -> None, "Samples" -> 1,
  "ValidationSamples" -> 1, "Parallelization" -> False, "TimeConstraint" -> 30,
  "ProgressReporting" -> False, "ReturnDetails" -> True, "RandomSeed" -> 1729};
csGroupedCF = CompleteFactorization[csRepeated, csQEDChannels, csQED, Sequence @@ csCFOptions];
check["the public structural gate is based on actual coherent work", csGroupedCF["StructuredPhysicalEligibility"]["Eligible"] === True && csGroupedCF["StructuredPhysicalEligibility"]["CoherentComponents"] === 16];
check["PreparationRemainders None remains authoritative in the public path", csGroupedCF["Status"] === "COMPLETE" && AssociationQ[csGroupedCF["StructuredPhysicalSpace"]] && !AnyTrue[Lookup[csGroupedCF["StructuredPhysicalSpace"]["Sources"], "Source"], AssociationQ[#] && Lookup[#, "Type", None] === "RetainedShellRemainder" &]];
check["a completed original proposal does not eagerly construct or fit the normalized fallback",
  csGroupedCF["Status"] === "COMPLETE" &&
  Lookup[Select[csGroupedCF["PhysicalSourceAttempts"],
    Lookup[#, "CandidateMode", None] === "StructuredPreparedPhysicalBlocks" &], "SourcePresentation"] === {"Original"} &&
  csGroupedCF["StructuredPhysicalSpace"]["ColumnNormalization"]["Status"] === "NOT_REQUESTED"];
csRepeatedExpression = Total[Last /@ csRepeatedPairs];
csExplicitCF = CompleteFactorization[csRepeated, csQEDChannels, csQED,
  "CorrectionBasis" -> {csRepeatedExpression}, Sequence @@ csCFOptions];
check["an explicit correction basis bypasses structured source generation", MatchQ[csExplicitCF["StructuredPhysicalSpace"], _Missing]];
csLiteralCF = CompleteFactorization[csRepeated, csQEDChannels, csQED,
  "ChannelReferences" -> None, Sequence @@ csCFOptions];
check["literal channel mode bypasses structured source generation", MatchQ[csLiteralCF["StructuredPhysicalSpace"], _Missing]];
csAdmissionEvent = <|"From" -> 128, "To" -> 256, "ObservedProjectedDirections" -> 129,
  "Operation" -> "RegressionStructuredAdmission", "ElapsedTime" -> 0|>;
csFailedAdmission = Internal`InheritedBlock[{ConstructiveDiagrams`Private`completionStructuredPhysicalSourceBasis},
  DownValues[ConstructiveDiagrams`Private`completionStructuredPhysicalSourceBasis] = {};
  ConstructiveDiagrams`Private`completionStructuredPhysicalSourceBasis[args___] :=
    <|"Status" -> "AVAILABLE", "Basis" -> {1}, "RegularExpression" -> 0,
      "EffectiveMaxCorrectionTerms" -> 256, "CapChanges" -> {csAdmissionEvent},
      "CandidateMode" -> "StructuredPreparedPhysicalBlocks", "WithinLimit" -> True|>;
  CompleteFactorization[csRepeated, csQEDChannels, csQED, Sequence @@ csCFOptions]];
check["a failed admitted proposal preserves its cap event exactly once", Count[csFailedAdmission["CapChanges"], csAdmissionEvent] === 1 && csFailedAdmission["EffectiveMaxCorrectionTerms"] >= 256];

csPairs = DiagramAmplitudes[CreateDiagrams[ConstantArray["s", 4], csModel]];
csAmplitude = Total[(ReduceSpinContractions[Last[#]] /. First[#]) & /@ csPairs];
(* Exercise the projected-seed hook without a large six-fermion benchmark.
   The mock performs the exact identity expr==expr; only gating, deadline and
   replay propagation are controlled here. Scalar physical checks still run. *)
csMetricValues = DownValues[ConstructiveDiagrams`Private`amplitudeResourceMetrics];
DownValues[csUncappedMetrics] = csMetricValues /.
  HoldPattern[ConstructiveDiagrams`Private`amplitudeResourceMetrics] -> csUncappedMetrics;
csSeedCalls = {}; csSyntheticLegs = Range[6];
csSeedHookResult = Internal`InheritedBlock[{ConstructiveDiagrams`Private`amplitudeResourceMetrics,
    ConstructiveDiagrams`Private`preparationMultilinearIdentityNormalize},
  DownValues[ConstructiveDiagrams`Private`amplitudeResourceMetrics] = {};
  ConstructiveDiagrams`Private`amplitudeResourceMetrics[args___] :=
    Join[csUncappedMetrics[args], <|"CoherentSpinLegs" -> csSyntheticLegs|>];
  DownValues[ConstructiveDiagrams`Private`preparationMultilinearIdentityNormalize] = {};
  ConstructiveDiagrams`Private`preparationMultilinearIdentityNormalize[expr_, rules_, conserve_, seconds_] := (
    AppendTo[csSeedCalls, <|"Seconds" -> seconds, "ExternalRules" -> rules, "Conserve" -> conserve|>];
    <|"Status" -> "IMPROVED", "Amplitude" -> expr, "Route" -> {
      <|"Operation" -> "ExactUnitIdentity", "Before" -> expr, "After" -> expr,
        "InternalOnShellIdentity" -> False, "ExactFormalReplay" -> True|>}|>);
  CompleteFactorization[csAmplitude, csChannels, csModel, "CorrectionBasis" -> {},
    "BoseSymmetryGroups" -> {Range[4]}, "FermionSymmetryGroups" -> {}, "Samples" -> 1,
    "ValidationSamples" -> 1, "TimeConstraint" -> 30, "Parallelization" -> False,
    "ProgressReporting" -> False, "ReturnDetails" -> True, "RandomSeed" -> 1729]];
check["projected-seed normalization uses actual coherent-leg eligibility", Length[csSeedCalls] === 1];
check["projected-seed normalization shares the explicit stage clock", Length[csSeedCalls] === 1 &&
  0 < First[csSeedCalls]["Seconds"] <= 10];
check["ordinary projected-seed replay remains in the completed report", csSeedHookResult["Status"] === "COMPLETE" &&
  csSeedHookResult["ProjectedSeedNormalization"]["Route"][[1]]["ExactFormalReplay"] === True &&
  csSeedHookResult["ProjectedSeedNormalization"]["Route"][[1]]["InternalOnShellIdentity"] === False];
csSeedCalls = {}; csSyntheticLegs = Range[4];
csSkippedSeed = Internal`InheritedBlock[{ConstructiveDiagrams`Private`amplitudeResourceMetrics,
    ConstructiveDiagrams`Private`preparationMultilinearIdentityNormalize},
  DownValues[ConstructiveDiagrams`Private`amplitudeResourceMetrics] = {};
  ConstructiveDiagrams`Private`amplitudeResourceMetrics[args___] :=
    Join[csUncappedMetrics[args], <|"CoherentSpinLegs" -> csSyntheticLegs|>];
  DownValues[ConstructiveDiagrams`Private`preparationMultilinearIdentityNormalize] = {};
  ConstructiveDiagrams`Private`preparationMultilinearIdentityNormalize[args___] :=
    (AppendTo[csSeedCalls, "UnexpectedCall"]; $Failed);
  CompleteFactorization[csAmplitude, csChannels, csModel, "CorrectionBasis" -> {},
    "BoseSymmetryGroups" -> {Range[4]}, "FermionSymmetryGroups" -> {}, "Samples" -> 1,
    "ValidationSamples" -> 1, "TimeConstraint" -> 30, "Parallelization" -> False,
    "ProgressReporting" -> False, "ReturnDetails" -> True, "RandomSeed" -> 1729]];
check["the projected-seed gate preserves smaller coherent systems", csSeedCalls === {} &&
  MatchQ[csSkippedSeed["ProjectedSeedNormalization"], _Missing] && csSkippedSeed["Status"] === "COMPLETE"];
csSeedSaved = csAmplitude;
csNoSeedBudget = ConstructiveDiagrams`Private`preparationMultilinearIdentityNormalize[
  csAmplitude, csNorm["ExternalMassRules"], True, 0];
check["an expired ordinary normalization keeps the exact seed", csNoSeedBudget["Status"] === "UNCHANGED" &&
  csNoSeedBudget["Amplitude"] === csSeedSaved && csNoSeedBudget["Route"] === {}];

csMessages = {};
Block[{$MessageList = {}}, csEnergy = CompleteHighEnergy[csAmplitude + 1, csChannels, csModel,
  "CorrectionBasis" -> {1}, "HighEnergyBound" -> -1, "Samples" -> 1, "ValidationSamples" -> 1,
  "Parallelization" -> False,
  "TimeConstraint" -> 30, "ProgressReporting" -> False, "Output" -> "Details", "RandomSeed" -> 1729];
  csMessages = $MessageList];
check["a uniquely completed public high-energy result has no retained certificate", AssociationQ[csEnergy] && csEnergy["Status"] === "COMPLETE" && csEnergy["FreeParameters"] === {}];
check["empty certificate diagnostics emit no Counts warning", FreeQ[csMessages, HoldPattern[MessageName[Counts, _]]]];
check["empty retained proof counts are a proper empty association", AssociationQ[csEnergy] && csEnergy["RetainedDirectionProofSummary"]["ProofReasons"] === <||>];
Print[checks, " completion structured-source checks; ", Length[failures], " failures."];
If[failures =!= {}, Print[failures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:completion-structured-source-tests.wl"];Quit[0];
