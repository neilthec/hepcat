(* Representation preference must preserve all physical residue targets and
   ordinary explicit regular input; a smaller source space is only a proposal. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
checks=0;failures={}; SetAttributes[check,HoldRest];
check[name_,value_]:=(checks++;If[!TrueQ[value],AppendTo[failures,name];Print["FAIL: ",name]]);
pref=ConstructiveDiagrams`Private`completionPolePreference;
phys=ConstructiveDiagrams`Private`completionPhysicalDenominatorOnlyQ;
check["Automatic inherits diagram AllPhysical",pref[Automatic,<|"Options"-><|"PolePreference"->"AllPhysical"|>|>,{}]==="AllPhysical"];
check["Automatic inherits channel AllPhysical",pref[Automatic,{},<|"Options"-><|"PolePreference"->"AllPhysical"|>|>]==="AllPhysical"];
check["Automatic inherits detailed completion preference",pref[Automatic,<|"PolePreference"->"AllPhysical"|>]==="AllPhysical"];
check["Explicit None overrides preparation",pref[None,<|"Options"-><|"PolePreference"->"AllPhysical"|>|>]===None];
check["Explicit Physical works for plain inputs",pref["AllPhysical",{}]==="AllPhysical"];
check["Plain inputs have no inferred preference",pref[Automatic,{}]===None];
check["a spurious preference stays distinct from AllPhysical",pref[Automatic,<|"Options"-><|"PolePreference"->"AllSpurious"|>|>]==="AllSpurious"];
rules={Mass[1]->m,Mass[2]->m,Mass[3]->0,Mass[4]->0};
channel=<|"Cut"->{1,3},"Complement"->{2,4},"Mass"->m,"ExternalLegs"->Range[4],"ExternalMassRules"->rules,
 "Propagator"->PropDen[Mom[Multiparticle[1,3]],m],"ChannelExpression"->1,"Complete"->True,"StateSum"->1|>;
normalized=<|"ExternalMassRules"->rules,"Targets"->{channel}|>;
p=PropDen[Mom[Multiparticle[1,3]],m];q=PropDen[Mom[Multiparticle[2,4]],m];
r=PropDen[Mom[Multiparticle[1,4]],m];a=SpinorChain[Spinor["Helicity","Angle",3],Spinor["Helicity","Angle",4]];
check["Known physical denominator admitted",phys[1/p,normalized]];
check["Complement denominator admitted",phys[1/q,normalized]];
check["Negative mass same physical mass square",phys[1/PropDen[Mom[Multiparticle[1,3]],-m],normalized]];
check["Wrong internal mass rejected",!phys[1/PropDen[Mom[Multiparticle[1,3]],2m],normalized]];
check["Unknown cut rejected",!phys[1/r,normalized]];
check["All physical factors inside grouped inverse accepted",phys[1/(p q),normalized]];
check["Numerator spinors are permitted",phys[a/p,normalized]];
check["Bracket inverse rejected",!phys[1/(p a),normalized]];
check["Sum containing kinematic inverse rejected",!phys[1/(p+q),normalized]];
check["Invariant inverse rejected",!phys[1/(p MomProd[1,3]),normalized]];
check["Model coefficient inverse admitted",phys[1/(p (m^2+coupling^2)),normalized]];
check["Regular terms admitted",phys[m+coupling a,normalized]];
check["Mass declared zero inverse rejected",!phys[1/(p Mass[3]),normalized]];
check["Symbolic inverse powers rejected",!phys[p^exponent,normalized]];
check["Reciprocal kinematic functions rejected",!phys[Csc[MomProd[1,3]]/p,normalized]];
(* Reducing a correction column must preserve that one coordinate, even when
   ordinary external identities rewrite its numerator as a multi-term sum. *)
ppNormalizeColumns = ConstructiveDiagrams`Private`completionNormalizePhysicalColumns;
ppColumnRules = {Mass[7] -> ppMassA, Mass[11] -> ppMassB, Mass[13] -> 0, Mass[17] -> 0};
ppColumnProp = PropDen[Mom[Multiparticle[7,13]], ppInternalMass];
ppColumnLeft = Spinor["Helicity", "Angle", 13];
ppColumnRight = Spinor["Helicity", "Angle", 17];
ppColumnBracket = SpinorChain[ppColumnLeft, ppColumnRight];
ppColumnInput = {
  (SpinorChain[ppColumnLeft, Mom[7], Mom[7], ppColumnRight] +
   SpinorChain[ppColumnLeft, Mom[11], Mom[11], ppColumnRight])/ppColumnProp,
  1/(1 + Mass[7]) + 1/(1 + Mass[11]),
  (MomProd[7,13] + ppUnrelated)/ppColumnProp};
ppColumnSaved = ppColumnInput;
ppColumnFlag = OwnValues[ConstructiveDiagrams`Private`$rspCanonicalProductBasis];
ppColumnReport = ppNormalizeColumns[ppColumnInput,
  Append[ppColumnRules, Mass[Multiparticle[7,13]] -> ppInternalMass], 5];
check["ordinary column normalization preserves the input and number of coordinates",
  ppColumnInput === ppColumnSaved && Length[ppColumnReport["Basis"]] === 3 &&
    ppColumnReport["InputColumns"] === 3 && ppColumnReport["OutputColumns"] === 3];
check["a reduced sum remains one whole correction direction",
  Together[ppColumnReport["Basis"][[1]] -
    (ppMassA^2 + ppMassB^2) ppColumnBracket/ppColumnProp] === 0 &&
    !TrueQ[ppColumnReport["Reatomized"]] && !TrueQ[ppColumnReport["SourceBasisChanged"]]];
check["unequal external masses stay independent in the normalized column",
  ppColumnReport["Basis"][[2]] === 1/(1 + ppMassA) + 1/(1 + ppMassB) &&
    !FreeQ[ppColumnReport["Basis"][[1]], ppMassA] &&
    !FreeQ[ppColumnReport["Basis"][[1]], ppMassB]];
check["whole-column normalization does not impose the internal mass shell",
  Together[ppColumnReport["Basis"][[3]] - ppColumnInput[[3]]] === 0 &&
    !FreeQ[ppColumnReport["Basis"][[3]], MomProd[7,13]] &&
    ppColumnReport["InternalOnShellConditions"] === {} &&
    FreeQ[ppColumnReport["ExternalMassRules"], Mass[_Multiparticle]]];
check["every whole-column route has a completed ordinary identity checkpoint",
  ppColumnReport["NormalizedColumns"] === 3 &&
    AllTrue[ppColumnReport["ColumnReports"],
      #1["Status"] === "NORMALIZED" && TrueQ[#1["ExactForwardIdentity"]] &&
      #1["NumericalKinematicSamples"] === 0 && #1["InternalOnShellConditions"] === {} &]];
check["the column normalizer restores the private canonical-basis scope",
  OwnValues[ConstructiveDiagrams`Private`$rspCanonicalProductBasis] === ppColumnFlag];
ppColumnPermutation = {7 -> 21, 11 -> 37, 13 -> 41, 17 -> 53};
ppColumnPermuted = ppNormalizeColumns[ppColumnInput /. ppColumnPermutation,
  ppColumnRules /. ppColumnPermutation, 5];
check["whole-column normalization is independent of the particle numbering",
  Together[ppColumnPermuted["Basis"] - (ppColumnReport["Basis"] /. ppColumnPermutation)] === {0,0,0}];
ppColumnScopedRules = ppColumnRules /. {ppMassA -> ModelParameter["m", "left-model", "Real"],
  ppMassB -> ModelParameter["m", "right-model", "Real"]};
ppColumnScoped = ppNormalizeColumns[ppColumnInput, ppColumnScopedRules, 5];
check["model-scoped parameters are not identified by their display names",
  !FreeQ[ppColumnScoped["Basis"][[1]], ModelParameter["m", "left-model", "Real"]] &&
    !FreeQ[ppColumnScoped["Basis"][[1]], ModelParameter["m", "right-model", "Real"]]];
ppColumnConflicting = ppNormalizeColumns[ppColumnInput,
  Append[ppColumnRules, Mass[7] -> ppOtherMass], 5];
check["conflicting external mass data preserves the complete original basis",
  ppColumnConflicting["Basis"] === ppColumnInput && ppColumnConflicting["Processed"] === 0];
ppColumnNonscalar = ppNormalizeColumns[ppColumnInput,
  ReplacePart[ppColumnRules, 1 -> (Mass[7] -> {ppMassA, ppMassB})], 5];
check["nonscalar external mass data cannot license a column identity",
  ppColumnNonscalar["Basis"] === ppColumnInput && ppColumnNonscalar["Processed"] === 0];
ppColumnDuplicate = ppNormalizeColumns[ppColumnInput, Append[ppColumnRules, First[ppColumnRules]], 5];
check["identical external mass duplicates leave the same whole-column span",
  ppColumnDuplicate["Basis"] === ppColumnReport["Basis"]];
ppColumnInternal = ppNormalizeColumns[ppColumnInput, Join[ppColumnRules,
  {Mass[Multiparticle[7,13]] -> {ppMassA, ppMassB},
   Mass[Multiparticle[7,13]] -> ppOtherMass}], 5];
check["even malformed internal mass declarations do not enter external column reduction",
  ppColumnInternal["Basis"] === ppColumnReport["Basis"] &&
    ppColumnInternal["ExternalMassRules"] === ppColumnRules];
ppColumnNoBudget = ppNormalizeColumns[ppColumnInput, ppColumnRules, 0];
check["a zero normalization budget keeps every untouched direction",
  ppColumnNoBudget["Basis"] === ppColumnInput && ppColumnNoBudget["UntouchedTail"] === Range[3]];
ppColumnInterrupted = Block[{ConstructiveDiagrams`Private`completionExternalReduce},
  ConstructiveDiagrams`Private`completionExternalReduce[value_, masses_List] := (Pause[0.05]; value);
  ppNormalizeColumns[ppColumnInput, ppColumnRules, 0.001]];
check["a shared deadline cannot publish a partial column or truncate its tail",
  ppColumnInterrupted["Basis"] === ppColumnInput &&
    ppColumnInterrupted["OutputColumns"] === 3 && ppColumnInterrupted["Processed"] === 0 &&
    ppColumnInterrupted["ColumnReports"] === {}];
ppColumnFailed = Block[{ConstructiveDiagrams`Private`completionExternalReduce},
  ConstructiveDiagrams`Private`completionExternalReduce[value_, masses_List] := $Failed;
  ppNormalizeColumns[ppColumnInput, ppColumnRules, 5]];
check["failed reductions retain each literal direction instead of dropping it",
  ppColumnFailed["Basis"] === ppColumnInput && ppColumnFailed["Processed"] === 3 &&
    AllTrue[ppColumnFailed["ColumnReports"], #1["Status"] === "PRESERVED" &]];
ppColumnEmpty = ppNormalizeColumns[{}, ppColumnRules, 5];
check["an empty physical basis remains empty with a complete span report",
  ppColumnEmpty["Basis"] === {} && ppColumnEmpty["OutputColumns"] === 0 &&
    TrueQ[ppColumnEmpty["FunctionalSpanPreserved"]]];

builder=ConstructiveDiagrams`Private`completionPhysicalChannelBasis;
scalarModel={{{"Scalar","s","s",1,0,m,0,1,1,"",""}},{{"s","s","s","",g,1}},{},{}};
scalarPairs=diagramAmplitudes[CreateDiagrams[ConstantArray["s",4],scalarModel]];
scalarChannels=FactorizationAmplitudeChannels[ConstantArray["s",4],scalarModel];
scalarNorm=ConstructiveDiagrams`Private`completionNormalizeInput[scalarPairs,scalarChannels,scalarModel];
identityPlan=<|"Order"->1,"Permutations"->{{}}|>;
scalarPrep=<|"PreparedAmplitudes"->scalarChannels,"Model"->scalarModel|>;
space=builder[scalarNorm,scalarPrep,identityPlan,64,32,10];
check["physical-only basis available from prepared sources",space["Status"]==="AVAILABLE" && Length[space["Basis"]]>0];
check["every built direction has only catalogued physical denominators",AllTrue[space["Basis"],phys[#,scalarNorm]&]];
check["legacy factorization contour does not enter the correction directions",
 FreeQ[space["Basis"], ConstructiveDiagrams`z]];
check["the physical source proposal excludes unsupported higher-order propagators",
 AllTrue[space["Basis"], ConstructiveDiagrams`Private`completionHigherPhysicalPoles[#,scalarNorm["Targets"],Range[4]] === {} &]];
check["no independent regular direction invented",AllTrue[space["Basis"],!FreeQ[#,Power[_PropDen,_Integer?Negative]]&]];
check["source metadata retains prepared-input provenance",MemberQ[Lookup[space["Sources"],"Source",{}],"PreparedInput"]];
contactPairs=Append[scalarPairs,{First[First[scalarPairs]],contact}];
contactNorm=ConstructiveDiagrams`Private`completionNormalizeInput[contactPairs,scalarChannels,scalarModel];
contactSpace=builder[contactNorm,scalarPrep,identityPlan,64,32,10];
check["explicit regular input retained as fixed seed",contactSpace["RegularExpression"]===contact];
check["final cap never truncates sources",builder[scalarNorm,scalarPrep,identityPlan,64,1,10]["Status"]==="UNAVAILABLE"];
noInputsNorm=Join[scalarNorm,<|"Expression"->0,"Records"->{<|"Expression"->0|>}|>];
channelOnlySpace=builder[noInputsNorm,scalarPrep,identityPlan,64,32,10];
check["prepared/original channels work independently of input diagrams",channelOnlySpace["Status"]==="AVAILABLE"];
augmented=Join[channel,<|"Cut"->{1,4},"Complement"->{2,3},"StateSum"->0,"Propagator"->r|>];
augmented=KeyDrop[augmented,"ChannelExpression"];
check["absence zero-residue obligations are not declared physical poles",!phys[1/r,Join[normalized,<|"Targets"->{channel,augmented}|>]]];
reducedOnlyNorm=Join[scalarNorm,<|"Expression"->0,"Records"->{<|"Expression"->0|>},"Targets"->scalarNorm["Targets"]|>];
reducedSources=<|"Sources"->{First[Last /@ scalarPairs]},"Basis"->Rest[Last /@ scalarPairs]|>;
reducedOnlySpace=builder[reducedOnlyNorm,<||>,identityPlan,64,32,10,reducedSources];
check["optional reduced channel continuation sources admitted",reducedOnlySpace["Status"]==="AVAILABLE" &&
 MemberQ[Lookup[reducedOnlySpace["Sources"],"Source",{}],"ReducedPhysicalChannelSource"]];

(* Exercise the public completion entry points on model-generated amplitudes.
   Unequal wrong channel weights cannot be fixed by a single normalization. *)
ppSettings[opts___] := Normal[Join[<|"Samples" -> 1, "ValidationSamples" -> 1,
  "RandomSeed" -> 110729, "TimeConstraint" -> 40, "MaxCorrectionTerms" -> 32,
  "ParameterRules" -> {m -> 1, g -> 2, contact -> 7},
  "Parallelization" -> False, "ProgressReporting" -> False, "ReturnDetails" -> True,
  "InputReweighting" -> None, "PreparationRemainders" -> None,
  "ChannelCompanions" -> None, "ShiftedChannelContinuations" -> None,
  "BoseSymmetryGroups" -> {}, "FermionSymmetryGroups" -> {}|>, Association[{opts}]]];
ppComplete[value_] := AssociationQ[value] && value["Status"] === "COMPLETE";
ppPhysicalPass[value_] := AssociationQ[value] && Length[value["Channels"]] === 3 &&
  AllTrue[value["Channels"], #["Status"] === "PASS" &];
ppCanonical[value_] := ConstructiveDiagrams`Private`residueCanonicalPropagators[value, Range[4]];
ppZero[value_] := Together[ppCanonical[value]] === 0;
ppWrongPairs = MapIndexed[{First[#1], First[#2] Last[#1]} &, scalarPairs];
ppSavedInput = ppWrongPairs; ppSavedChannels = scalarChannels; ppSavedModel = scalarModel;
ppDiagramPreparation = PrepareAmplitudes[ppWrongPairs, scalarModel,
  "PolePreference" -> "AllPhysical", "ExternalParticles" -> ConstantArray["s",4],
  "MaxSteps" -> 0, "Parallelization" -> False, "ProgressReporting" -> False,
  "TimeConstraint" -> 10, "ReturnDetails" -> True];
ppChannelPreparation = PrepareAmplitudes[scalarChannels, scalarModel,
  "PolePreference" -> "AllPhysical", "ExternalParticles" -> ConstantArray["s",4],
  "MaxSteps" -> 0, "Parallelization" -> False, "ProgressReporting" -> False,
  "TimeConstraint" -> 10, "ReturnDetails" -> True];
ppTree = Total[Last /@ scalarPairs];
ppPhysical = CompleteFactorization[ppDiagramPreparation, ppChannelPreparation,
  scalarModel, Sequence @@ ppSettings[]];
check["AllPhysical preparation chooses the physical-only completion proposal",
  ppComplete[ppPhysical] && ppPhysical["CorrectionSpace"] === "PhysicalChannels" &&
    ppPhysical["PolePreference"] === "AllPhysical"];
check["the physical proposal reconstructs the model amplitude without normalization guesses",
  ppZero[ppPhysical["Amplitude"] - ppTree] && phys[ppPhysical["Amplitude"], scalarNorm]];
check["all original channel residues receive independent validation",
  ppPhysicalPass[ppPhysical["ValidationReport"]] &&
    ppPhysical["ValidationReports"] =!= {}];
check["physical preference leaves model, input records and original channels unchanged",
  scalarChannels === ppSavedChannels && scalarModel === ppSavedModel && ppWrongPairs === ppSavedInput];
ppOrdinary = CompleteFactorization[ppDiagramPreparation, ppChannelPreparation,
  scalarModel, Sequence @@ ppSettings["PolePreference" -> None]];
check["disabling the representation preference retains the same physical answer",
  ppComplete[ppOrdinary] && ppOrdinary["PolePreference"] === None &&
    ppZero[ppOrdinary["Amplitude"] - ppPhysical["Amplitude"]]];
check["disabled preference still validates every original channel",
  ppPhysicalPass[ppOrdinary["ValidationReport"]]];
ppPlain = CompleteFactorization[ppWrongPairs, scalarChannels, scalarModel,
  Sequence @@ ppSettings["PolePreference" -> "AllPhysical"]];
check["plain amplitude inputs can explicitly prefer physical poles",
  ppComplete[ppPlain] && ppPlain["PolePreference"] === "AllPhysical" && ppZero[ppPlain["Amplitude"] - ppTree]];
ppContactInput = Append[ppWrongPairs, {First[First[scalarPairs]], contact}];
ppContact = CompleteFactorization[ppContactInput, scalarChannels, scalarModel,
  Sequence @@ ppSettings["PolePreference" -> "AllPhysical"]];
check["physical-only fitting retains an explicit regular seed term",
  ppComplete[ppContact] && ppZero[ppContact["Amplitude"] - ppTree - contact]];
check["regular preservation does not change the independently validated residues",
  ppPhysicalPass[ppContact["ValidationReport"]]];
ppCapped = CompleteFactorization[ppDiagramPreparation, ppChannelPreparation,
  scalarModel, Sequence @@ ppSettings["MaxCorrectionTerms" -> 1]];
check["a physical-space cap cannot silently delete required channel directions",
  AssociationQ[ppCapped] && ppCapped["Status"] =!= "COMPLETE" &&
    Lookup[ppCapped["PhysicalPoleSpace"], "Status", None] === "UNAVAILABLE"];
check["a rejected physical-only space leaves the ordinary fallback visible",
  AssociationQ[ppCapped] && ppCapped["CorrectionSpace"] =!= "PhysicalChannels" &&
    ppCapped["Diagnostics"] =!= {}];
ppExplicit = CompleteFactorization[ppWrongPairs, scalarChannels, scalarModel,
  Sequence @@ ppSettings["PolePreference" -> "AllPhysical", "CorrectionBasis" -> Join[Last /@ scalarPairs, {1}]]];
check["an explicit supplied correction space is not restricted by the representation preference",
  ppComplete[ppExplicit] && ppExplicit["CorrectionSpace"] === "SuppliedTerms" &&
    Length[ppExplicit["RemainingBasis"]] === 1 && Length[ppExplicit["FreeParameters"]] === 1];
check["the complete affine freedom remains in the supplied-space result",
  ppZero[ppExplicit["FamilyAmplitude"] - ppExplicit["Amplitude"] -
    ppExplicit["FreeParameters"].ppExplicit["RemainingBasis"]]];
ppEnergy = CompleteHighEnergy[ppPhysical, scalarChannels, scalarModel,
  "Samples" -> 1, "ValidationSamples" -> 1, "ParameterRules" -> {m -> 1, g -> 2},
  "Parallelization" -> False, "ProgressReporting" -> False, "TimeConstraint" -> 30,
  "Output" -> "Details"];
check["high-energy completion inherits the physical-pole representation preference",
  AssociationQ[ppEnergy] && ppEnergy["PolePreference"] === "AllPhysical"];
check["the valid scalar completion remains unchanged through high-energy checks",
  ppComplete[ppEnergy] && ppZero[ppEnergy["Amplitude"] - ppPhysical["Amplitude"]]];
check["unsupported physical-pole preferences are rejected",
  Quiet[CompleteFactorization[ppWrongPairs, scalarChannels, scalarModel,
    "PolePreference" -> True]] === $Failed];

check["Automatic inherits a prepared spurious preference",pref[Automatic,<|"Options"-><|"PolePreference"->"AllSpurious"|>|>]==="AllSpurious"];
check["Automatic inherits a prepared mixed preference",pref[Automatic,<|"Options"-><|"PolePreference"->"Mixed"|>|>]==="Mixed"];
check["the input prescription wins over a differently prepared channel report",pref[Automatic,<|"Options"-><|"PolePreference"->"AllSpurious"|>|>,<|"Options"-><|"PolePreference"->"AllPhysical"|>|>]==="AllSpurious"];
check["an explicit preparation None remains None during completion",pref[Automatic,<|"Options"-><|"PolePreference"->None|>|>,<|"Options"-><|"PolePreference"->"AllPhysical"|>|>]===None];
check["all public workflow stages use the same preference option",AllTrue[{PrepareAmplitudes,CompleteFactorization,CompleteHighEnergy,ConstructAmplitude},MemberQ[First/@Options[#],"PolePreference"]&]];

(* A reference-free scalar amplitude still exercises the public preferred
   source route: reference choices do not remove its true propagators. *)
ppSpurious = CompleteFactorization[ppWrongPairs, scalarChannels, scalarModel,
  Sequence @@ ppSettings["PolePreference" -> "AllSpurious"]];
check["a spurious preference enters its preferred source attempt",
  ppComplete[ppSpurious] && ppSpurious["CorrectionSpace"] === "PreferredReferences" &&
    ppSpurious["PolePreference"] === "AllSpurious"];
check["a preferred spurious source fit preserves all true physical residues",
  ppZero[ppSpurious["Amplitude"] - ppTree] && ppPhysicalPass[ppSpurious["ValidationReport"]]];
ppMixed = CompleteFactorization[ppWrongPairs, scalarChannels, scalarModel,
  Sequence @@ ppSettings["PolePreference" -> "Mixed"]];
check["a mixed preference enters its preferred source attempt",
  ppComplete[ppMixed] && ppMixed["CorrectionSpace"] === "PreferredReferences" &&
    ppMixed["PolePreference"] === "Mixed"];
check["changing the auxiliary prescription cannot change the scalar physical amplitude",
  ppZero[ppMixed["Amplitude"] - ppSpurious["Amplitude"]] && ppPhysicalPass[ppMixed["ValidationReport"]]];

(* Auxiliary-reference choices retain the same true propagators and targets. *)
match=ConstructiveDiagrams`Private`completionPreferredReferenceVariantQ;
builder=ConstructiveDiagrams`Private`completionPreferredReferenceBasis;
v[mode_,kinds_:{}]:=<|"Accepted"->True,"Mode"->mode,"Assignments"->(<|"Mode"->#|>&/@kinds)|>;
check["allspurious accepts forward reverse rotated",AllTrue[{"AllSpurious","ReverseCyclic","RotatedCyclic"},match[v[#],"AllSpurious"]&]];
check["allspurious excludes physical",!match[v["AllPhysical"],"AllSpurious"]];
check["mixed includes both reference types",AllTrue[{"AllSpurious","AllPhysical","Mixed"},match[v[#],"Mixed"]&]];
check["cyclic keeps forward",match[v["AllSpurious"],"Cyclic"]&&!match[v["ReverseCyclic"],"Cyclic"]];
check["reverse keeps reverse",match[v["ReverseCyclic"],"ReverseCyclic"]&&!match[v["AllSpurious"],"ReverseCyclic"]];
check["recorded mixed requires allowed assignments",match[v["PreparedReferences",{"AllSpurious","AllPhysical"}],"Mixed"]&&!match[v["PreparedReferences",{"Xi"}],"Mixed"]];
check["recordedXi staysXi",match[v["PreparedReferences",{"Xi"}],"Xi"]&&!match[v["PreparedReferences",{"IndependentXi"}],"Xi"]];
check["independentXi stays independent",match[v["IndependentXi"],"IndependentXi"]&&!match[v["Xi"],"IndependentXi"]];
check["unverified sources rejected",!match[Join[v["AllSpurious"],<|"Accepted"->False|>],"AllSpurious"]];
rules={Mass[1]->m,Mass[2]->m,Mass[3]->0,Mass[4]->0};p=PropDen[Mom[Multiparticle[1,3]],m];
ch=<|"Cut"->{1,3},"Complement"->{2,4},"Mass"->m,"ExternalLegs"->Range[4],"ExternalMassRules"->rules,"ChannelExpression"->1/p,"Propagator"->p,"Complete"->True,"StateSum"->1|>;
norm=<|"Expression"->0,"Records"->{<|"Expression"->0|>},"ExternalMassRules"->rules,"Targets"->{ch}|>;
a=SpinorChain[Spinor["Helicity","Angle",3],Spinor["Helicity","Angle",4]];
x=SpinorChain[Spinor["Xi","Angle",0],Spinor["Helicity","Angle",3]];
variants={Join[v["AllSpurious"],<|"Expression"->1/(p a),"Cut"->{1,3}|>],Join[v["ReverseCyclic"],<|"Expression"->1/(p a^2),"Cut"->{1,3}|>],Join[v["AllPhysical"],<|"Expression"->2/p,"Cut"->{1,3}|>],Join[v["Xi"],<|"Expression"->1/(p x),"Cut"->{1,3}|>]};
space=builder[norm,<||>,<|"Order"->1,"Permutations"->{{}}|>,64,32,10,<|"Variants"->variants|>,"AllSpurious"];
check["spurious preferred space available",space["Status"]==="AVAILABLE"&&Length[space["Basis"]]>0];
check["spurious preferred space contains auxiliary brackets",!FreeQ[space["Basis"],Power[_SpinorChain,_Integer?Negative]]];
check["spurious mode does not remove true physical propagators",AllTrue[space["Basis"],!FreeQ[#,Power[_PropDen,_Integer?Negative]]&]];
check["spurious mode does not introduce genericXi",FreeQ[space["Basis"],Spinor["Xi",___]]];
xiSpace=builder[norm,<||>,<|"Order"->1,"Permutations"->{{}}|>,64,32,10,<|"Variants"->variants|>,"Xi"];
check["Xi prescription permits external commonXi references",xiSpace["Status"]==="AVAILABLE"&&!FreeQ[xiSpace["Basis"],Spinor["Xi",___]]];
noneSpace=builder[norm,<||>,<|"Order"->1,"Permutations"->{{}}|>,64,32,10,<|"Variants"->variants|>,None];
check["None does not create a preferred-space proposal",noneSpace["Status"]==="UNAVAILABLE"];
(* A budget fallback selects a whole named source space, not the first
   few atoms of an oversized union. This grouped direction carries all cuts. *)
ppGroupedFallback = ConstructiveDiagrams`Private`completionPhysicalChannelBasis[
  scalarNorm, scalarPrep, identityPlan, 64, 1, 10,
  <|"Basis" -> {ppTree}, "Sources" -> {}|>];
check["the physical cap uses a complete named grouped source fallback",
  ppGroupedFallback["Status"] === "AVAILABLE" &&
    ppGroupedFallback["CandidateMode"] === "ReducedPhysicalChannelsBudgetFallback" &&
    ppGroupedFallback["OversizedUnionDirections"] > 1 &&
    Length[ppGroupedFallback["Basis"]] === 1];
check["the grouped budget fallback retains every pole in its chosen source",
  ppZero[First[ppGroupedFallback["Basis"]] - ppTree]];
ppGroupedFit = CompleteFactorization[{First[First[scalarPairs]], 0},
  scalarChannels, scalarModel, Sequence @@ ppSettings[
    "PolePreference" -> "AllPhysical", "MaxCorrectionTerms" -> 1,
    "CorrectionBasis" -> ppGroupedFallback["Basis"]]];
check["the one-direction grouped fallback still matches every original channel",
  ppComplete[ppGroupedFit] && ppZero[ppGroupedFit["Amplitude"] - ppTree] &&
    ppPhysicalPass[ppGroupedFit["ValidationReport"]]];

(* End-to-end regression for the original mixed-helicity failure. Generate
   every input afresh through the public wrapper, including an independent
   Feynman sum. No saved answer, notebook output, cache or helper supplies it. *)
Print["Constructing the real five-point mixed-helicity QED regression."];
ppQEDDiagramModel = ReadModel[FileNameJoin[{root, "models"}], 2];
ppQEDFactorizationModel = ReadModel[FileNameJoin[{root, "models"}], 3];
ppQEDFeynmanModel = ReadModel[FileNameJoin[{root, "model_feynman"}], 2];
ppQEDModelsBefore = {ppQEDDiagramModel, ppQEDFactorizationModel, ppQEDFeynmanModel};
ppQEDProcess = {"e", "E", "A.+", "A.+", "A.-"};
ppQEDRules = {"Me" -> 1, "EE" -> 1};
ppQEDConstruction = ConstructAmplitude[ppQEDProcess, ppQEDDiagramModel,
  ppQEDFactorizationModel,
  "PolePreference" -> "AllPhysical", "FeynmanModel" -> ppQEDFeynmanModel,
  "Samples" -> 1, "ValidationSamples" -> 1, "ParameterRules" -> ppQEDRules,
  "Parallelization" -> False, "RandomSeed" -> 1729,
  "TimeConstraint" -> 240, "ProgressReporting" -> False, "PrintReport" -> False,
  "PreparationOptions" -> {"MaxSteps" -> 0, "TimeConstraint" -> 60},
  "ChannelPreparationOptions" -> {"MaxSteps" -> 0, "TimeConstraint" -> 60},
  "FinalPreparationOptions" -> {"MaxSteps" -> 0, "TimeConstraint" -> 60},
  "FeynmanPreparationOptions" -> {"MaxSteps" -> 0, "TimeConstraint" -> 60},
  "FactorizationOptions" -> {"MaxCorrectionTerms" -> 128, "TimeConstraint" -> 180,
    "ValidationParameterRules" -> {ppQEDRules}},
  "HighEnergyOptions" -> {"MaxCorrectionTerms" -> 128, "TimeConstraint" -> 180,
    "ValidationParameterRules" -> {ppQEDRules}},
  "CheckOptions" -> {"Samples" -> 3, "HighEnergySamples" -> 3,
    "ReferenceSamples" -> 3, "FeynmanSamples" -> 3, "TimeConstraint" -> 60},
  "Output" -> "Details"];
check["the mixed-helicity five-point QED wrapper completes every stage",
  AssociationQ[ppQEDConstruction] && ppQEDConstruction["Status"] === "COMPLETE"];
If[AssociationQ[ppQEDConstruction] && ppQEDConstruction["Status"] === "COMPLETE",
  ppQEDChecks = ppQEDConstruction["Checks"];
  ppQEDFactorization = ppQEDConstruction["Stages"]["Completing factorization"];
  ppQEDChannels = ppQEDConstruction["Stages"]["Generating factorization channels"];
  ppQEDNormalized = ConstructiveDiagrams`Private`completionNormalizeInput[
    ppQEDConstruction["AmplitudePair"], ppQEDChannels, ppQEDFactorizationModel];
  check["all mixed-helicity final physical, spurious, energy and reference checks pass",
    ppQEDChecks["Checks"] =!= {} &&
      AllTrue[ppQEDChecks["Checks"], #["Status"] === "PASS" &] &&
      Length[ppQEDChecks["Channels"]] === 6 &&
      ppQEDChecks["HighEnergyCheck"]["Status"] === "PASS" &&
      ppQEDChecks["ReferenceCheck"]["Status"] === "PASS"];
  check["the independently generated mixed-helicity Feynman sum agrees",
    ppQEDChecks["FeynmanCheck"]["Status"] === "PASS"];
  check["the mixed-helicity final expression contains only declared physical denominators",
    phys[ppQEDConstruction["Amplitude"], ppQEDNormalized] &&
      Lookup[ppQEDChecks, "SpuriousChecks", {}] === {}];
  check["mixed-helicity completion avoids the former large spurious-pole expression",
    LeafCount[ppQEDConstruction["Amplitude"]] < 1500 &&
      If[Head[ppQEDFactorization["Amplitude"]] === Plus,
        Length[ppQEDFactorization["Amplitude"]], 1] < 10];
  check["the wrapper carries AllPhysical through preparation and factorization",
    ppQEDConstruction["Stages"]["Preparing diagrams"]["Options"]["PolePreference"] === "AllPhysical" &&
      ppQEDConstruction["Stages"]["Preparing factorization channels"]["Options"]["PolePreference"] === "AllPhysical" &&
      ppQEDFactorization["PolePreference"] === "AllPhysical"],
  Print["Mixed-helicity construction diagnostics: ",
    If[AssociationQ[ppQEDConstruction],
      KeyTake[ppQEDConstruction, {"Status", "FailureStage", "Diagnostics"}], ppQEDConstruction]]];
check["construction leaves all three QED models unchanged",
  {ppQEDDiagramModel, ppQEDFactorizationModel, ppQEDFeynmanModel} === ppQEDModelsBefore];
Print[checks, " physical-pole completion checks; ", Length[failures], " failures."];
If[failures =!= {}, Print[failures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:physical-pole-completion-tests.wl"];
Quit[0];
