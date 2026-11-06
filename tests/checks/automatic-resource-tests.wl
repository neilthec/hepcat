(* Automatic effort follows the actual job while explicit controls stay exact.
   Construction regressions receive no numerical tuning options. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, expression_] := (checks++;
  If[!TrueQ[expression], AppendTo[failures, name]; Print["FAIL: ", name]]);
arMetrics = ConstructiveDiagrams`Private`amplitudeResourceMetrics;
arPolicy = ConstructiveDiagrams`Private`amplitudeAutomaticSettings;
arFinite[value_] := NumericQ[value] && TrueQ[0 < value < Infinity];
arRules4 = {Mass[1] -> arMass, Mass[2] -> arMass, Mass[3] -> 0, Mass[4] -> 0};
arRules5 = Append[arRules4, Mass[5] -> 0];
arRules6 = Append[arRules5, Mass[6] -> 0];
arExpression = (1 + SpinorChain[Spinor["Helicity", "Angle", 3],
  Spinor["Helicity", "Angle", 4]])^20;
arFour = arMetrics[{arRules4, arExpression}];
arFive = arMetrics[{arRules5, arExpression}];
arSix = arMetrics[{arRules6, arExpression}];
check["ordinary pairs supply four-, five- and six-point multiplicity",
  Lookup[{arFour, arFive, arSix}, "ExternalParticles"] === {4, 5, 6}];
check["size estimates preserve factored expressions without expansion",
  arFour["ExpressionLeaves"] === LeafCount[arExpression] &&
    arFour["LargestItemLeaves"] === LeafCount[arExpression] && Head[arExpression] === Power];
check["coherent-spin rows follow actual spinor states rather than masses alone",
  arFour["CoherentComponents"] === 1 &&
    arMetrics[{arRules4, arExpression SpinorChain[
      Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]}]["CoherentComponents"] === 4 &&
    arMetrics[{arRules4, SpinorChain[
      Spinor["Spin", "Upper", "Angle", 1, -1/2],
      Spinor["Spin", "Upper", "Angle", 2, 1/2]]}]["CoherentComponents"] === 1];
arVectorRules = {Mass[1]->0,Mass[2]->arMass,Mass[3]->arMass,Mass[4]->arMass};
arThreeVectors = (SpinorChain[Spinor["Spin","Angle",2],Spinor["Spin","Angle",3]] +
  2 SpinorChain[Spinor["Spin","Square",2],Spinor["Spin","Square",3]])^2 *
  SpinorChain[Spinor["Spin","Square",4],Spinor["Helicity","Square",1]]^2;
arVectorMetrics = arMetrics[{arVectorRules,arThreeVectors}];
check["three shorthand massive vectors retain all twenty-seven coherent components",
  arVectorMetrics["CoherentComponents"] === 27 &&
    Lookup[arVectorMetrics["CoherentSpinDegrees"],{2,3,4}] === {2,2,2}];
arVectorChannel = <|"Cut"->{1,3},"Complement"->{2,4},"ExternalLegs"->Range[4],
  "ExternalMassRules"->{Mass[1]->0,Mass[2]->1,Mass[3]->2,Mass[4]->3},"Mass"->1,
  "Propagator"->PropDen[Mom[Multiparticle[1,3]],1]|>;
arVectorChart = ConstructiveDiagrams`Private`completionCutChart[arVectorChannel,1,9127,{}];
arVectorMapped = arVectorChart["Map"][arThreeVectors,{arVectorChannel},{arVectorPole}];
check["the exact checker maps all twenty-seven vector polynomial components independently of its resource estimate",
  Length[ConstructiveDiagrams`Private`completionCoefficientEquations[arVectorMapped,
    arVectorChart["SpinVariables"]]] === 27];
arPrepared = <|"PreparedAmplitudes" -> {{arRules5, arExpression}, {arRules5, arExpression/7}},
  "ExternalMassRules" -> arRules5, "Amplitude" -> 8 arExpression/7,
  "RemainingBasis" -> Table[arExpression, 17], "CorrectionBasis" -> Table[arExpression, 9]|>;
arPreparedHash = Hash[arPrepared, "SHA256"];
arDetailed = arMetrics[arPrepared];
check["preparation and affine reports retain independent workload measurements",
  arDetailed["Items"] === 2 && arDetailed["RetainedDirections"] === 17 &&
    arDetailed["BasisDirections"] === 9 && arDetailed["ExternalParticles"] === 5];
check["an actual correction basis count replaces an earlier estimate",
  arMetrics[arPrepared, {}, Automatic, 300]["BasisDirections"] === 300];
check["resource measurement does not modify caller reports or expressions",
  Hash[arPrepared, "SHA256"] === arPreparedHash &&
    arDetailed["ExpressionLeaves"] === LeafCount[arPrepared["Amplitude"]]];
arLongDirection = Total[Table[arScalarCoefficient[index] SpinorChain[
  Spinor["Spin", "Angle", 1], Mom[3], Spinor["Spin", "Square", 2]], {index, 1800}]];
arRetainedReport = <|"Amplitude" -> arExpression, "ExternalMassRules" -> arRules4,
  "FamilyAmplitude" -> arExpression + arFamilyCoefficient arLongDirection,
  "RemainingBasis" -> {arLongDirection}, "FreeParameters" -> {arFamilyCoefficient}|>;
arRetainedReportHash = Hash[arRetainedReport, "SHA256"];
arCenterMetrics = arMetrics[arRetainedReport];
arFamilyMetrics = ConstructiveDiagrams`Private`completionHighEnergyResourceMetrics[arRetainedReport];
check["high-energy resource sizing measures the supplied family instead of only its fixed center",
  arFamilyMetrics["ExpressionLeaves"] >= LeafCount[arRetainedReport["FamilyAmplitude"]] &&
    arFamilyMetrics["RetainedFamilyExpressionLeaves"] === LeafCount[arRetainedReport["FamilyAmplitude"]] &&
    arFamilyMetrics["FixedCenterExpressionLeaves"] === LeafCount[arExpression] &&
    arFamilyMetrics["ResourceExpressionSource"] === "RetainedAffineFamily"];
check["family-aware automatic time grows with measured work without changing ordinary stage metrics",
  ConstructiveDiagrams`Private`amplitudeAutomaticSettings["HighEnergy",
    <|"TimeConstraint" -> Automatic|>, arFamilyMetrics]["Settings"]["TimeConstraint"] >
  ConstructiveDiagrams`Private`amplitudeAutomaticSettings["HighEnergy",
    <|"TimeConstraint" -> Automatic|>, arCenterMetrics]["Settings"]["TimeConstraint"] &&
    arMetrics[arRetainedReport]["ExpressionLeaves"] === LeafCount[arExpression]];
arDirectionOnlyMetrics = ConstructiveDiagrams`Private`completionHighEnergyResourceMetrics[
  KeyDrop[arRetainedReport, "FamilyAmplitude"]];
check["retained direction work is measured even when no explicit full family field is supplied",
  arDirectionOnlyMetrics["RetainedDirectionLeaves"] === LeafCount[arLongDirection] &&
    ConstructiveDiagrams`Private`amplitudeAutomaticSettings["HighEnergy",
      <|"TimeConstraint" -> Automatic|>, arDirectionOnlyMetrics]["Settings"]["TimeConstraint"] >
    ConstructiveDiagrams`Private`amplitudeAutomaticSettings["HighEnergy",
      <|"TimeConstraint" -> Automatic|>, arCenterMetrics]["Settings"]["TimeConstraint"]];
arExplicitBasisMetrics = ConstructiveDiagrams`Private`completionHighEnergyResourceMetrics[
  {arRules4, arExpression}, {}, {arLongDirection, 2 arLongDirection}];
check["an explicit high-energy correction basis contributes its actual supplied work",
  arExplicitBasisMetrics["BasisDirections"] === 2 &&
    arExplicitBasisMetrics["ExplicitCorrectionBasisLeaves"] ===
      Total[LeafCount /@ {arLongDirection, 2 arLongDirection}]];
arFixedResourceRequest = <|"TimeConstraint" -> 11/3, "Samples" -> 1, "ValidationSamples" -> 2|>;
check["larger retained families never change explicit high-energy clocks or sample controls",
  ConstructiveDiagrams`Private`amplitudeAutomaticSettings["HighEnergy", arFixedResourceRequest,
    arFamilyMetrics]["Settings"] === arFixedResourceRequest];
check["family-aware measurement neither mutates its input nor changes plain amplitude estimates",
  Hash[arRetainedReport, "SHA256"] === arRetainedReportHash &&
    KeyTake[ConstructiveDiagrams`Private`completionHighEnergyResourceMetrics[{arRules4, arExpression}],
      Keys[arFour]] === arFour];
(* Generated HE sources are small input slots even when their derivation
   records contain large histories. Measure only slots actually read later. *)
arSourceRules=Thread[(Mass/@Range[4])->ConstantArray[arMass,4]];
arSourceExpression=SpinorChain[Spinor["Spin","Angle",1],Spinor["Spin","Angle",2]]*
  SpinorChain[Spinor["Spin","Square",3],Spinor["Spin","Square",4]]^2;
arShellSlot=(1+MomProd[1,3])^9;
arReferenceSlot=Total[Table[arSourceScalar[i] MomProd[1,3],{i,220}]];
arXiSlot=SpinorChain[Spinor["Xi","Angle",7],Spinor["Helicity","Angle",3]];
arUnusedHistory=Array[arUnconsumedHistory,5000];
arSourceRecord=<|"HasInternalConditions"->True,"ShellRemainder"->arShellSlot,
  "FixedReferenceExpression"->arXiSlot,"ReferenceInputExpression"->arReferenceSlot,
  "DerivationHistory"->arUnusedHistory,
  "ReferenceContinuations"->{<|"Mode"->"AllPhysical","Expression"->arUnusedHistory|>,
    <|"Mode"->"Xi","Expression"->arXiSlot|>,<|"Mode"->"Xi","Expression"->arUnusedHistory|>}|>;
arNoShellRecord=<|"HasInternalConditions"->True,"ShellRemainder"->Missing["NoShellDifference"],
  "FixedReferenceExpression"->arUnusedHistory,"ReferenceInputExpression"->arUnusedHistory,
  "ReferenceContinuations"->{<|"Mode"->"Xi","Expression"->arXiSlot|>}|>;
arChannelSourceRecord=<|"HasInternalConditions"->True,"ShellRemainder"->arShellSlot,
  "FixedReferenceExpression"->arXiSlot,
  "ReferenceContinuations"->{<|"Mode"->"Xi","Expression"->arXiSlot|>,
    <|"Mode"->"Xi","Expression"->2 arXiSlot|>,<|"Mode"->"Mixed","Expression"->arUnusedHistory|>}|>;
arChannelOverrideRecord=Join[arChannelSourceRecord,<|"ReferenceInputExpression"->arXiSlot|>];
arSourceReport=<|"Amplitude"->arSourceExpression,"FamilyAmplitude"->arSourceExpression,
  "ExternalMassRules"->arSourceRules,"RemainingBasis"->{},"FreeParameters"->{},
  "RemainderData"->{arSourceRecord,arNoShellRecord},
  "ChannelRemainderData"->{Join[arChannelSourceRecord,<|"ShellRemainder"->arUnusedHistory|>]}|>;
arSourcePreparedChannels=<|"OriginalAmplitudes"->{},"RemainderData"->{arChannelSourceRecord,arChannelOverrideRecord}|>;
arSourceReportHash=Hash[arSourceReport,"SHA256"];
arSourceSlotMetrics=ConstructiveDiagrams`Private`completionHERetainedSourceMetrics[arSourceReport,arSourcePreparedChannels];
arExpectedSourceLeaves=3 LeafCount[arShellSlot]+LeafCount[arReferenceSlot]+7 LeafCount[arXiSlot]+LeafCount[2 arXiSlot];
check["HE source sizing counts only the shell and reference expressions consumed by its generators",
  arSourceSlotMetrics["RetainedSourceExpressionLeaves"]===arExpectedSourceLeaves &&
    arSourceSlotMetrics["RetainedSourceExpressionSlots"]===12];
check["source sizing respects common-Xi selection and the channel reference-input override",
  arSourceSlotMetrics["RetainedSourceSlotCounts"]===<|"ShellRemainder"->3,
    "FixedReferenceExpression"->3,"ReferenceInputExpression"->2,"XiReferenceContinuation"->4|>];
check["unconsumed histories and unused continuation modes do not enlarge the source estimate",
  ConstructiveDiagrams`Private`completionHERetainedSourceMetrics[
    arSourceReport/.arUnconsumedHistory->arDifferentUnusedHistory,arSourcePreparedChannels]===arSourceSlotMetrics];
check["current prepared channels override stale channel provenance in an earlier completion report",
  ConstructiveDiagrams`Private`completionHERetainedSourceMetrics[
    Join[arSourceReport,<|"ChannelRemainderData"->{}|>],arSourcePreparedChannels]===arSourceSlotMetrics &&
    ConstructiveDiagrams`Private`completionHERetainedSourceMetrics[arSourceReport,{}]["RetainedSourceExpressionLeaves"]>
      ConstructiveDiagrams`Private`completionHERetainedSourceMetrics[
        Join[arSourceReport,<|"ChannelRemainderData"->{}|>],{}]["RetainedSourceExpressionLeaves"]];
arGeneratedSourceMetrics=ConstructiveDiagrams`Private`completionHighEnergyResourceMetrics[arSourceReport,arSourcePreparedChannels];
check["generated source work uses the complete coherent tensor with the four-state QED baseline",
  arGeneratedSourceMetrics["CoherentComponents"]===36 &&
    arGeneratedSourceMetrics["RetainedSourceCoherentWeight"]===9 &&
    arGeneratedSourceMetrics["RetainedSourceWorkLeaves"]===9 arExpectedSourceLeaves];
check["large pending source families receive a larger finite allowance before their deadline is created",
  arPolicy["HighEnergy",<|"TimeConstraint"->Automatic|>,arGeneratedSourceMetrics]["Settings"]["TimeConstraint"]>
    arPolicy["HighEnergy",<|"TimeConstraint"->Automatic|>,
      Join[arGeneratedSourceMetrics,<|"RetainedSourceWorkLeaves"->0|>]]["Settings"]["TimeConstraint"]];
arExplicitSourceMetrics=ConstructiveDiagrams`Private`completionHighEnergyResourceMetrics[
  arSourceReport,arSourcePreparedChannels,{arExpression}];
check["an explicit correction basis disables unused automatic retained-source sizing",
  arExplicitSourceMetrics["RetainedSourceExpressionLeaves"]===0 &&
    arExplicitSourceMetrics["RetainedSourceExpressionSlots"]===0 &&
    arExplicitSourceMetrics["RetainedSourceWorkLeaves"]===0];
arQEDSourceMetrics=ConstructiveDiagrams`Private`completionHighEnergyResourceMetrics[
  Join[arSourceReport,<|"Amplitude"->SpinorChain[Spinor["Spin","Angle",1],Spinor["Spin","Angle",2]],
    "FamilyAmplitude"->SpinorChain[Spinor["Spin","Angle",1],Spinor["Spin","Angle",2]],"ExternalMassRules"->arRules4|>],arSourcePreparedChannels];
check["the two-fermion QED spin tensor has unit source weight",
  arQEDSourceMetrics["CoherentComponents"]===4 && arQEDSourceMetrics["RetainedSourceCoherentWeight"]===1];
check["source measurement neither expands a factored source nor mutates caller history",
  Head[arSourceReport["RemainderData"][[1,"ShellRemainder"]]]===Power &&
    Hash[arSourceReport,"SHA256"]===arSourceReportHash];
check["retained-source work affects HE only and leaves every other stage policy unchanged",
  AllTrue[{"Construction","Preparation","Factorization","Checks"},
    arPolicy[#,<|"TimeConstraint"->Automatic|>,arGeneratedSourceMetrics]["Settings"]===
    arPolicy[#,<|"TimeConstraint"->Automatic|>,Join[arGeneratedSourceMetrics,<|"RetainedSourceWorkLeaves"->0|>]]["Settings"]&]];
arGeneratedWithFamily=ConstructiveDiagrams`Private`completionHighEnergyResourceMetrics[
  Join[arRetainedReport,<|"RemainderData"->{<|"HasInternalConditions"->True,"ShellRemainder"->1|>}|>]];
check["the larger actual supplied family still dominates a tiny generated-source estimate",
  arGeneratedWithFamily["RetainedDirectionLeaves"]===LeafCount[arLongDirection] &&
    arPolicy["HighEnergy",<|"TimeConstraint"->Automatic|>,arGeneratedWithFamily]["Settings"]===
    arPolicy["HighEnergy",<|"TimeConstraint"->Automatic|>,Join[arGeneratedWithFamily,<|"RetainedSourceWorkLeaves"->0|>]]["Settings"]];
arSM43Resource=Join[arGeneratedSourceMetrics,<|"Channels"->4,"ExpressionLeaves"->1306,
  "LargestItemLeaves"->1306,"RetainedDirections"->0,"RetainedSourceRecords"->8,
  "RetainedSourceWorkLeaves"->8948*9|>];
check["the measured 36-state SM source workload gets a bounded allowance commensurate with its pending family",
  arPolicy["HighEnergy",<|"TimeConstraint"->Automatic|>,arSM43Resource]["Settings"]["TimeConstraint"]===442 &&
    arPolicy["HighEnergy",<|"TimeConstraint"->Automatic|>,arSM43Resource]["ResourcePolicy"]["PostFitReserve"]===100];
arSM71Resource=Join[arGeneratedSourceMetrics,<|"Channels"->3,"CoherentComponents"->81,
  "ExpressionLeaves"->15910,"LargestItemLeaves"->15910,"RetainedDirections"->0,
  "RetainedSourceRecords"->6,"RetainedSourceWorkLeaves"->46648*21|>];
check["the four-vector source catalog remains inside the existing finite Automatic work ceiling",
  600<=arPolicy["HighEnergy",<|"TimeConstraint"->Automatic|>,arSM71Resource]["Settings"]["TimeConstraint"]<=900];
check["generated-source measurements cannot extend any explicit clock, cap or sample setting",
  arPolicy["HighEnergy",arFixedResourceRequest,arGeneratedSourceMetrics]["Settings"]===arFixedResourceRequest];
arSpinModel = {{{"vector", "v", "v", 1, 2, arMass},
  {"fermion", "f", "F", 2, 1, arMass}, {"photon", "a+", "a-", 22, 2, 0},
  {"scalar", "s", "s", 25, 0, arMass}}, {}, {}, {}};
arSpinModelHash = Hash[arSpinModel, "SHA256"];
arConstructionVectors = ConstructiveDiagrams`Private`amplitudeConstructionResourceMetrics[
  {"v", "v", "v", "v"}, arSpinModel];
arConstructionQED = ConstructiveDiagrams`Private`amplitudeConstructionResourceMetrics[
  {"f", "F", "a+", "a-"}, arSpinModel];
arConstructionMixedSpins = ConstructiveDiagrams`Private`amplitudeConstructionResourceMetrics[
  {"f", "F", "v", "v"}, arSpinModel];
check["construction estimates all massive model spin states before diagram generation",
  arConstructionVectors["CoherentComponents"] === 81 &&
    arConstructionMixedSpins["CoherentComponents"] === 36 &&
    arConstructionQED["CoherentComponents"] === 4 &&
    arConstructionVectors["CoherentSpinDegrees"] === AssociationThread[Range[4], {2, 2, 2, 2}]];
check["fixed-helicity massless particles and massive scalars add no artificial coherent multiplicity",
  ConstructiveDiagrams`Private`amplitudeConstructionResourceMetrics[
    {"a+", "a-", "s", "s"}, arSpinModel]["CoherentComponents"] === 1];
check["model-aware wrapper effort grows for vector tensors while the QED default and explicit clocks stay unchanged",
  arPolicy["Construction", <|"TimeConstraint" -> Automatic|>, arConstructionVectors]["Settings"]["TimeConstraint"] >
    arPolicy["Construction", <|"TimeConstraint" -> Automatic|>, arFour]["Settings"]["TimeConstraint"] &&
  arPolicy["Construction", <|"TimeConstraint" -> Automatic|>, arConstructionQED]["Settings"]["TimeConstraint"] ===
    arPolicy["Construction", <|"TimeConstraint" -> Automatic|>, arFour]["Settings"]["TimeConstraint"] &&
  arPolicy["Construction", <|"TimeConstraint" -> 17|>, arConstructionVectors]["Settings"]["TimeConstraint"] === 17];
check["model-aware resource measurement is read-only and unknown species retain the ordinary fallback",
  Hash[arSpinModel, "SHA256"] === arSpinModelHash &&
    ConstructiveDiagrams`Private`amplitudeConstructionResourceMetrics[
      {"unknown", "f", "F", "s"}, arSpinModel] === arMetrics[{}, {}, {"unknown", "f", "F", "s"}]];
arChannels = Table[<|"ExternalLegs" -> Range[6]|>, 14];
check["channel metadata can supply multiplicity before diagram preparation",
  arMetrics[0, arChannels]["ExternalParticles"] === 6 &&
    arMetrics[0, arChannels]["Channels"] === 14];
arRequests = <|"TimeConstraint" -> Automatic, "MaxCorrectionTerms" -> Automatic,
  "MaxSteps" -> Automatic, "Samples" -> Automatic, "ValidationSamples" -> Automatic,
  "MaxCandidates" -> Automatic, "MaxReferenceChoices" -> Automatic,
  "MaxRefinements" -> Automatic, "ProgressReporting" -> False|>;
check["automatic effort accounts for coherent tensor work while explicit clocks stay authoritative",
  arPolicy["Factorization",arRequests,arVectorMetrics]["Settings"]["TimeConstraint"] >
    arPolicy["Factorization",arRequests,arFour]["Settings"]["TimeConstraint"] &&
  arPolicy["Factorization",<|"TimeConstraint"->17|>,arVectorMetrics]["Settings"]["TimeConstraint"] === 17];
arProfiles = arPolicy["Construction", arRequests, #] & /@ {arFour, arFive, arSix};
check["all automatic construction profiles resolve to finite positive limits",
  AllTrue[arProfiles, arFinite[#["Settings"]["TimeConstraint"]] &&
    IntegerQ[#["Settings"]["MaxCorrectionTerms"]] && #["Settings"]["MaxCorrectionTerms"] > 0 &]];
check["larger multiplicity receives more time without changing the request",
  OrderedQ[#["Settings"]["TimeConstraint"] & /@ arProfiles] &&
    arProfiles[[3]]["Settings"]["TimeConstraint"] > arProfiles[[1]]["Settings"]["TimeConstraint"] &&
    arRequests["TimeConstraint"] === Automatic];
check["automatic validation never lowers its independent sample standard",
  AllTrue[arProfiles, #["Settings"]["ValidationSamples"] >= 4 &&
    #["ResourcePolicy"]["ValidationSamplesReduced"] === False &]];
check["automatic fit sampling retains a minimum of four samples",
  AllTrue[arProfiles, #["Settings"]["Samples"] >= 4 &]];
check["optional identity enumeration starts with ordinary reductions only",
  AllTrue[arProfiles, #["Settings"]["MaxSteps"] === 0 &]];
arLarge = Join[arSix, <|"ExpressionLeaves" -> 90000, "LargestItemLeaves" -> 30000,
  "Channels" -> 14, "BasisDirections" -> 1000, "RetainedDirections" -> 250|>];
arLargeProfile = arPolicy["Factorization", arRequests, arLarge];
check["large expressions and correction spaces receive a larger finite budget",
  arFinite[arLargeProfile["Settings"]["TimeConstraint"]] &&
    arLargeProfile["Settings"]["TimeConstraint"] > arPolicy["Factorization", arRequests, arSix]["Settings"]["TimeConstraint"] &&
    arLargeProfile["Settings"]["MaxCorrectionTerms"] >= arLarge["BasisDirections"]];
arManyUnknowns = Join[arFive, <|"Channels" -> 6, "BasisDirections" -> 1000|>];
check["automatic sample counts respond to the number of fitted directions",
  arPolicy["Factorization", arRequests, arManyUnknowns]["Settings"]["Samples"] >
    arPolicy["Factorization", arRequests, Join[arManyUnknowns, <|"BasisDirections" -> 8|>]]["Settings"]["Samples"]];
check["more retained freedom increases the high-energy budget",
  arPolicy["HighEnergy", arRequests, arLarge]["Settings"]["TimeConstraint"] >
    arPolicy["Factorization", arRequests, arLarge]["Settings"]["TimeConstraint"]];
check["retained shell-source work receives a finite automatic high-energy allowance",
  arPolicy["HighEnergy", arRequests, Join[arFive, <|"RetainedSourceRecords" -> 12|>]]["Settings"]["TimeConstraint"] >
    arPolicy["HighEnergy", arRequests, arFive]["Settings"]["TimeConstraint"] &&
    arPolicy["HighEnergy", <|"TimeConstraint" -> 17|>,
      Join[arFive, <|"RetainedSourceRecords" -> 12|>]]["Settings"]["TimeConstraint"] === 17];
arOversized = arPolicy["Factorization", arRequests,
  Join[arSix, <|"BasisDirections" -> 100000|>]];
check["an exceeded automatic correction ceiling remains explicit",
  arOversized["ResourcePolicy"]["BasisExceedsAutomaticCeiling"] === True &&
    arOversized["Settings"]["MaxCorrectionTerms"] === arOversized["ResourcePolicy"]["CorrectionTermCeiling"]];
arExplicit = <|"TimeConstraint" -> 7/2, "MaxCorrectionTerms" -> 17,
  "MaxSteps" -> 9, "Samples" -> 3, "ValidationSamples" -> 2,
  "MaxCandidates" -> 19, "MaxReferenceChoices" -> 7, "MaxRefinements" -> 1,
  "ProgressReporting" -> False|>;
check["explicit numerical choices remain exactly unchanged at every stage",
  AllTrue[{"Construction", "Preparation", "Factorization", "HighEnergy", "Checks"},
    arPolicy[#, arExplicit, arLarge]["Settings"] === arExplicit &]];
check["an explicitly unlimited time is not replaced by an automatic timeout",
  arPolicy["Factorization", <|"TimeConstraint" -> Infinity|>, arSix]["Settings"]["TimeConstraint"] === Infinity];
check["mixed explicit and automatic options preserve the explicit sample override",
  With[{arMixed = arPolicy["Factorization", Join[arRequests, <|"Samples" -> 2|>], arManyUnknowns]},
    arMixed["Settings"]["Samples"] === 2 && arMixed["Settings"]["ValidationSamples"] >= 4 &&
      !MemberQ[arMixed["ResourcePolicy"]["AutomaticOptions"], "Samples"]]];
check["resource policy never changes unrelated settings",
  arLargeProfile["Settings"]["ProgressReporting"] === False &&
    arPolicy["Checks", <|"Output" -> "Details", "TimeConstraint" -> Automatic|>, arSix]["Settings"]["Output"] === "Details"];
check["preparation's public effort controls default to Automatic",
  AllTrue[{"MaxSteps", "TimeConstraint", "MaxReferenceChoices", "MaxCandidates"},
    (# /. Options[PrepareAmplitudes]) === Automatic &]];
check["factorization's public resource controls default to Automatic",
  AllTrue[{"TimeConstraint", "MaxCorrectionTerms", "Samples", "ValidationSamples"},
    (# /. Options[CompleteFactorization]) === Automatic &]];
check["high-energy completion's public resource controls default to Automatic",
  AllTrue[{"TimeConstraint", "MaxCorrectionTerms", "Samples", "ValidationSamples"},
    (# /. Options[CompleteHighEnergy]) === Automatic &]];
check["the constructor defaults to physical poles with automatic resource settings",
  ("PolePreference" /. Options[ConstructAmplitude]) === "AllPhysical" &&
    AllTrue[{"TimeConstraint", "Samples", "ValidationSamples", "Parallelization", "MaxKernels"},
      (# /. Options[ConstructAmplitude]) === Automatic &]];

(* Discover an actual projected physical basis before falling back to a
   smaller space. This generic source catalog has two exact formal directions. *)
arCapPropagator = PropDen[Mom[Multiparticle[1, 2]], 0];
arCapRules = Table[Mass[i] -> 0, {i, 4}];
arCapTarget = <|"ExternalLegs" -> Range[4], "Cut" -> {1, 2}, "Mass" -> 0,
  "ChannelExpression" -> 0, "Complete" -> True, "StateSum" -> 0|>;
arCapNormalized = <|"Expression" -> (arDirectionA + arDirectionB)/arCapPropagator,
  "ExternalMassRules" -> arCapRules, "Targets" -> {arCapTarget},
  "Records" -> {<|"Expression" -> (arDirectionA + arDirectionB)/arCapPropagator|>}|>;
arCapPlan = <|"Order" -> 1, "Permutations" -> {{}}, "Signs" -> {1}|>;
arCapHash = Hash[arCapNormalized, "SHA256"];
arCapAutomatic = Block[{ConstructiveDiagrams`Private`$completionAutomaticCorrectionTermCeiling = 8},
  ConstructiveDiagrams`Private`completionPhysicalChannelBasis[
    arCapNormalized, None, arCapPlan, 1, 1, 5]];
arCapExplicit = Block[{ConstructiveDiagrams`Private`$completionAutomaticCorrectionTermCeiling = None},
  ConstructiveDiagrams`Private`completionPhysicalChannelBasis[
    arCapNormalized, None, arCapPlan, 4, 1, 5]];
arCapCeiling = Block[{ConstructiveDiagrams`Private`$completionAutomaticCorrectionTermCeiling = 1},
  ConstructiveDiagrams`Private`completionPhysicalChannelBasis[
    arCapNormalized, None, arCapPlan, 4, 1, 5]];
check["an automatic cap reuses the complete discovered projected physical basis",
  arCapAutomatic["Status"] === "AVAILABLE" && Length[arCapAutomatic["Basis"]] === 2 &&
    arCapAutomatic["EffectiveMaxCorrectionTerms"] === 2 &&
    arCapAutomatic["CandidateMode"] === "PreparedAndReducedPhysicalUnion"];
check["early cap changes and bounded raw-catalog construction are explicit",
  MatchQ[arCapAutomatic["CapChanges"], {_Association}] &&
    KeyTake[First[arCapAutomatic["CapChanges"]], {"From", "To", "ObservedProjectedDirections"}] ===
      <|"From" -> 1, "To" -> 2, "ObservedProjectedDirections" -> 2|> &&
    2 <= arCapAutomatic["CatalogConstructionLimit"] <= 4096];
check["a fixed cap never silently truncates or expands the physical basis",
  arCapExplicit["Status"] === "UNAVAILABLE" && arCapExplicit["Basis"] === {} &&
    arCapExplicit["EffectiveMaxCorrectionTerms"] === 1 && arCapExplicit["CapChanges"] === {}];
check["the automatic hard ceiling is never exceeded",
  arCapCeiling["Status"] === "UNAVAILABLE" && arCapCeiling["EffectiveMaxCorrectionTerms"] === 1 &&
    arCapCeiling["CapChanges"] === {}];
check["early-cap discovery leaves the original source catalog unchanged",
  Hash[arCapNormalized, "SHA256"] === arCapHash];

arUnionBasis = Table[arUnionDirection[i], {i, 907}];
arUnionHash = Hash[arUnionBasis, "SHA256"];
arUnionAdmission = ConstructiveDiagrams`Private`completionMaterializedBasisCap[
  arUnionBasis, Automatic, 256, 2048];
check["an already materialized exchange union grows only its automatic effective cap",
  arUnionAdmission["EffectiveMaxCorrectionTerms"] === 1024 &&
    First[arUnionAdmission["CapChanges"]]["Operation"] === "ReuseCompleteExchangeUnion"];
check["automatic admission preserves every original column without normalization or trimming",
  Hash[arUnionBasis, "SHA256"] === arUnionHash && Length[arUnionBasis] === 907];
check["an explicit cap has no inherited permission to grow a materialized union",
  ConstructiveDiagrams`Private`completionMaterializedBasisCap[
    arUnionBasis, Automatic, 256, None] ===
      <|"EffectiveMaxCorrectionTerms" -> 256, "CapChanges" -> {}|>];
check["a supplied correction basis never gains implicit cap admission",
  ConstructiveDiagrams`Private`completionMaterializedBasisCap[
    arUnionBasis, arUnionBasis, 256, 2048]["EffectiveMaxCorrectionTerms"] === 256];
check["an oversized union is retained intact but never admitted beyond the automatic ceiling",
  ConstructiveDiagrams`Private`completionMaterializedBasisCap[
    arUnionBasis, Automatic, 256, 512] ===
      <|"EffectiveMaxCorrectionTerms" -> 256, "CapChanges" -> {}|> && Length[arUnionBasis] === 907];
check["an automatic ceiling need not be a power of two",
  ConstructiveDiagrams`Private`completionMaterializedBasisCap[
    arUnionBasis, Automatic, 256, 1000]["EffectiveMaxCorrectionTerms"] === 1000];
check["an incomplete source checkpoint cannot raise a cap",
  ConstructiveDiagrams`Private`completionMaterializedBasisCap[
    Append[arUnionBasis, $TimedOut], Automatic, 256, 2048]["CapChanges"] === {}];

arHEUnion=Table[arHEBasis[i],{i,300}];arHEHash=Hash[arHEUnion,"SHA256"];
arHEAdmission=ConstructiveDiagrams`Private`completionHEMaterializedBasisAdmission[
  arHEUnion,{},128,1024,2];
check["the complete high-energy union can grow an Automatic cap inside its granted ceiling",
  arHEAdmission["EffectiveMaxCorrectionTerms"]===512 && Length[arHEAdmission["Basis"]]===300];
check["high-energy admission never samples, trims or changes the original sources",
  Hash[arHEUnion,"SHA256"]===arHEHash && arHEAdmission["NumericalKinematicsUsed"]===False &&
    arHEAdmission["TruncatedDirections"]===0];
check["an explicit high-energy cap remains authoritative",
  ConstructiveDiagrams`Private`completionHEMaterializedBasisAdmission[
    arHEUnion,{},128,None,2]["EffectiveMaxCorrectionTerms"]===128];
check["the high-energy union cannot exceed the Automatic hard ceiling",
  ConstructiveDiagrams`Private`completionHEMaterializedBasisAdmission[
    arHEUnion,{},128,256,2]["EffectiveMaxCorrectionTerms"]===128];
check["inherited high-energy family coordinates count toward the same complete cap",
  ConstructiveDiagrams`Private`completionHEMaterializedBasisAdmission[
    Take[arHEUnion,200],Array[arHEInherited,100],128,1024,2]["EffectiveMaxCorrectionTerms"]===512];
check["an exact formal duplicate can be removed before high-energy cap admission",
  With[{admission=ConstructiveDiagrams`Private`completionHEMaterializedBasisAdmission[
      {arHEA,arHEB,arHEA+arHEB},{},2,8,2]},
    Length[admission["Basis"]]===2 && admission["EffectiveMaxCorrectionTerms"]===2 &&
      admission["OriginalNewDirections"]===3 && admission["TruncatedDirections"]===0]];

(* The controller must not rebuild a complete union merely because its initial
   heuristic cap was too small. This inert core never claims a fitted result. *)
arUnionBuilds = 0;
arUnionController = Internal`InheritedBlock[
  {ConstructiveDiagrams`Private`completionFactorizationCore},
  Clear[ConstructiveDiagrams`Private`completionFactorizationCore];
  ConstructiveDiagrams`Private`completionFactorizationCore[input_, channels_, model_, opts___] := Module[
    {settings = Association[{opts}], admission},
    arUnionBuilds++;
    admission = ConstructiveDiagrams`Private`completionMaterializedBasisCap[
      arUnionBasis, settings["CorrectionBasis"], settings["MaxCorrectionTerms"],
      ConstructiveDiagrams`Private`$completionAutomaticCorrectionTermCeiling];
    Join[<|"Status" -> "UNRESOLVED", "Amplitude" -> 0, "ExternalMassRules" -> arRules4,
      "CorrectionBasis" -> arUnionBasis, "CorrectionSpace" -> "ExchangeSelectedChannelUnion",
      "CorrectionSpaceCandidates" -> {<|"Terms" -> Length[arUnionBasis],
        "WithinLimit" -> True|>}, "ElapsedTime" -> 0,
      "Diagnostics" -> {"An inert source-space fixture does not certify an amplitude."}|>, admission]];
  CompleteFactorization[{arRules4, 0}, {}, {{}, {}, {}, {}},
    "PolePreference" -> "AllPhysical", "ReturnDetails" -> True,
    "TimeConstraint" -> 17/2, "ProgressReporting" -> False, "Parallelization" -> False]];
check["the automatic controller builds an admitted complete union once and preserves all columns",
  arUnionBuilds === 1 && Length[arUnionController["AdaptiveAttempts"]] === 1 &&
    arUnionController["EffectiveMaxCorrectionTerms"] === 1024 &&
    arUnionController["CorrectionBasis"] === arUnionBasis &&
    arUnionController["Status"] === "UNRESOLVED"];
check["admitting an existing union does not extend the original stage clock",
  arUnionController["ResourcePolicy"]["Resolved"]["TimeConstraint"] === 17/2];

arLiteralTerms = Table[arLiteralDirection[i]/arCapPropagator, {i, 1048}];
arLiteralNormalized = Join[arCapNormalized, <|"Expression" -> Total[arLiteralTerms], "Targets" -> {}|>];
arOldSourceLimit = ConstructiveDiagrams`Private`completionSourceConstructionLimit[256, 4];
arAdmittedSourceLimit = ConstructiveDiagrams`Private`completionSourceConstructionLimit[2048, 4];
arStaleLiteralBasis = ConstructiveDiagrams`Private`completionAutomaticBasis[
  arLiteralNormalized, arOldSourceLimit, <|"Sources" -> {}|>];
arAdmittedLiteralBasis = ConstructiveDiagrams`Private`completionAutomaticBasis[
  arLiteralNormalized, arAdmittedSourceLimit, <|"Sources" -> {}|>];
check["a stale pre-admission source bound reproduces the literal-space rejection",
  arOldSourceLimit === 1024 && FailureQ[arStaleLiteralBasis] &&
    arStaleLiteralBasis[[2]]["Terms"] === 1048];
check["a raised effective cap supplies the same established construction policy to later fallbacks",
  arAdmittedSourceLimit === 4096 && ListQ[arAdmittedLiteralBasis] &&
    Length[arAdmittedLiteralBasis] === 1048 && Total[arAdmittedLiteralBasis] === Total[arLiteralTerms]];
check["source construction retains the existing bounds for explicit and unsymmetrized spaces",
  ConstructiveDiagrams`Private`completionSourceConstructionLimit[17, 4] === 68 &&
    ConstructiveDiagrams`Private`completionSourceConstructionLimit[17, 1] === 17 &&
    ConstructiveDiagrams`Private`completionSourceConstructionLimit[5000, 8] === 5000];

(* An inert core report isolates the retry controller. No mocked report is
   accepted as an amplitude or used to bypass a physical check. *)
arMeasureCalls = 0;
arMeasurePhysical = <|"Status" -> "AVAILABLE", "WithinLimit" -> True,
  "CandidateMode" -> "PreparedAndReducedPhysicalUnion",
  "RawDirectionCount" -> 512, "ProjectedDirectionCount" -> 2|>;
arMeasureFallback = Join[arMeasurePhysical,
  <|"CandidateMode" -> "ReducedPhysicalChannelsBudgetFallback", "OversizedUnionDirections" -> 512|>];
arMeasureRun[space_, suppliedBasis_ : Automatic] := Internal`InheritedBlock[
  {ConstructiveDiagrams`Private`completionFactorizationCore},
  Clear[ConstructiveDiagrams`Private`completionFactorizationCore];
  ConstructiveDiagrams`Private`completionFactorizationCore[args___] := (
    arMeasureCalls++;
    <|"Status" -> "UNRESOLVED", "Amplitude" -> 0,
      "ExternalMassRules" -> arRules4, "Diagnostics" -> {"The supplied source space did not close."},
      "CorrectionSpace" -> "RetryControlFixture", "PhysicalPoleSpace" -> space,
      "EffectiveMaxCorrectionTerms" -> 128, "ElapsedTime" -> 0|>);
  CompleteFactorization[{arRules4, 0}, {}, {{}, {}, {}, {}},
    "CorrectionBasis" -> suppliedBasis,
    "PolePreference" -> "AllPhysical", "ReturnDetails" -> True,
    "ProgressReporting" -> False, "Parallelization" -> False]];
arMeasureComplete = arMeasureRun[arMeasurePhysical];
check["a complete projected physical space does not retry its larger raw atom count",
  arMeasureCalls === 1 && Length[arMeasureComplete["AdaptiveAttempts"]] === 1 &&
    First[arMeasureComplete["AdaptiveAttempts"]]["ObservedDirections"] === 2 &&
    arMeasureComplete["EffectiveMaxCorrectionTerms"] === 128];
arMeasureCalls = 0;
arMeasureReduced = arMeasureRun[arMeasureFallback];
check["an oversized reduced fallback still requests a bounded larger cap",
  arMeasureCalls === 2 && Length[arMeasureReduced["AdaptiveAttempts"]] === 2 &&
    First[arMeasureReduced["AdaptiveAttempts"]]["ObservedDirections"] === 512 &&
    arMeasureReduced["EffectiveMaxCorrectionTerms"] === 512];

arMeasureCalls = 0;
arMeasuredExplicitBasis = arMeasureRun[arMeasurePhysical, Table[arBasisDirection[i], {i, 258}]];
check["an explicit basis informs the initial automatic cap before any fit attempt",
  arMeasureCalls === 1 &&
    arMeasuredExplicitBasis["ResourcePolicy"]["Metrics"]["BasisDirections"] === 258 &&
    arMeasuredExplicitBasis["ResourcePolicy"]["Resolved"]["MaxCorrectionTerms"] === 512 &&
    First[arMeasuredExplicitBasis["AdaptiveAttempts"]]["MaxCorrectionTerms"] === 512];

(* Fresh public calls exercise the defaults without resource tuning, temporary
   caches, notebook outputs, known final amplitudes or global SetOptions. *)
(* Factorization must size the retained source and parameter workload before
   its single shared core deadline, not only the displayed fixed center. *)
arFactorSourceMetrics=ConstructiveDiagrams`Private`completionFactorizationResourceMetrics;
arFactorRequests=Association[Options[CompleteFactorization]];
arFactorSM=ReadModel[FileNameJoin[{root,"models"}],1];
arFactorMeasured=arFactorSourceMetrics[arSourceReport,arSourcePreparedChannels,arFactorSM,arFactorRequests];
check["higher massive-spin factorization measures retained source slots before its deadline",
 arFactorMeasured["FactorizationSourceWorkAdmitted"] &&
 arFactorMeasured["FactorizationRetainedSourceLeaves"]===arExpectedSourceLeaves &&
 arFactorMeasured["FactorizationRetainedSourceSlots"]===12 &&
 arFactorMeasured["FactorizationResourceResolutionPhase"]==="BeforeCatalogConstructionAndCoreDeadline"];
check["factorization weighting follows the actual coherent tensor and unchanged model assignments",
 arFactorMeasured["FactorizationSourceCoherentWeight"]===9 &&
 arFactorMeasured["FactorizationParameterAssignmentCount"]===Length[
  ConstructiveDiagrams`Private`completionValidationRuleSets[arFactorSM,Automatic,Automatic]] &&
 arFactorMeasured["FactorizationSourceAssignmentWeight"]===3/2 &&
 arFactorMeasured["FactorizationSourceWorkLeaves"]===Ceiling[9*3/2*arExpectedSourceLeaves]];
arFactorResolved=arPolicy["Factorization",arFactorRequests,arFactorMeasured];
arFactorBaseline=arPolicy["Factorization",arFactorRequests,arMetrics[arSourceReport,arSourcePreparedChannels]];
check["a pending coupled source family gets a larger finite automatic factorization allowance",
 arFinite[arFactorResolved["Settings"]["TimeConstraint"]] &&
 arFactorResolved["Settings"]["TimeConstraint"]>arFactorBaseline["Settings"]["TimeConstraint"]];
check["the factorization source estimate does not change caps or fit and held-out samples",
 KeyTake[arFactorResolved["Settings"],{"MaxCorrectionTerms","Samples","ValidationSamples","MaxRefinements"}]===
 KeyTake[arFactorBaseline["Settings"],{"MaxCorrectionTerms","Samples","ValidationSamples","MaxRefinements"}]];
check["an explicit factorization clock is unchanged despite large available sources",
 arPolicy["Factorization",Join[arFactorRequests,<|"TimeConstraint"->17/3|>],arFactorMeasured]["Settings"]["TimeConstraint"]===17/3];
check["the new factorization metric cannot change the wrapper's overall time policy",
 arPolicy["Construction",arRequests,arFactorMeasured]["Settings"]["TimeConstraint"]===
 arPolicy["Construction",arRequests,arMetrics[arSourceReport,arSourcePreparedChannels]]["Settings"]["TimeConstraint"]];
check["unused derivation histories do not enter the factorization resource inventory",
 arFactorSourceMetrics[arSourceReport/.arUnconsumedHistory->arDifferentUnusedHistory,
 arSourcePreparedChannels,arFactorSM,arFactorRequests]===arFactorMeasured];
check["explicit prepared channels remain authoritative over stale channel source metadata",
 arFactorSourceMetrics[Join[arSourceReport,<|"ChannelRemainderData"->{}|>],
 arSourcePreparedChannels,arFactorSM,arFactorRequests]===arFactorMeasured];
arFactorExplicit=arFactorSourceMetrics[arSourceReport,arSourcePreparedChannels,arFactorSM,
 Join[arFactorRequests,<|"CorrectionBasis"->{arExpression}|>]];
check["an explicit correction basis disables unused automatic factorization source slots",
 arFactorExplicit["FactorizationRetainedSourceLeaves"]===0 &&
 arFactorExplicit["FactorizationRetainedSourceSlots"]===0 &&
 arFactorExplicit["FactorizationPreparedChannelLeaves"]===0 &&
 arFactorExplicit["FactorizationExplicitBasisLeaves"]===LeafCount[arExpression]];
arFactorSingle=arFactorSourceMetrics[arSourceReport,arSourcePreparedChannels,arFactorSM,
 Join[arFactorRequests,<|"ValidationParameterRules"->{}|>]];
check["factorization counts actual requested parameter assignments rather than assuming three",
 arFactorSingle["FactorizationParameterAssignmentCount"]===1 &&
 arFactorSingle["FactorizationSourceAssignmentWeight"]===1 &&
 arFactorSingle["FactorizationSourceWorkLeaves"]===9 arExpectedSourceLeaves];
arFactorRawRows={Join[ConstantArray[arMass,4],{Multiparticle[1,3],arMass,arReferenceSlot}]};
arFactorPreparedRows={Join[ConstantArray[arMass,4],{Multiparticle[1,3],arMass,arShellSlot}]};
arFactorChannels=Join[arSourcePreparedChannels,<|"OriginalAmplitudes"->arFactorRawRows,
 "PreparedAmplitudes"->arFactorPreparedRows|>];
arFactorChannelMetrics=arFactorSourceMetrics[arSourceReport,arFactorChannels,arFactorSM,arFactorRequests];
check["original residue targets and prepared channel sources are sized separately",
 arFactorChannelMetrics["FactorizationOriginalChannelLeaves"]===LeafCount[arReferenceSlot] &&
 arFactorChannelMetrics["FactorizationPreparedChannelLeaves"]===LeafCount[arShellSlot] &&
 arFactorChannelMetrics["FactorizationSourceWorkLeaves"]===Ceiling[
  9*3/2*(arExpectedSourceLeaves+LeafCount[arReferenceSlot]+LeafCount[arShellSlot])]];
arFactorFermionLegs=Range[6];arFactorFermionRules=Thread[(Mass/@arFactorFermionLegs)->ConstantArray[arMass,6]];
arFactorFermionExpr=Times@@(SpinorChain[Spinor["Spin","Angle",First[#]],Spinor["Spin","Square",Last[#]]]&/@Partition[arFactorFermionLegs,2]);
arFactorFermionReport=Join[arSourceReport,<|"Amplitude"->arFactorFermionExpr,
 "FamilyAmplitude"->arFactorFermionExpr,"ExternalMassRules"->arFactorFermionRules|>];
arFactorFermionMetrics=arFactorSourceMetrics[arFactorFermionReport,{},arFactorSM,arFactorRequests];
check["sixty-four-component fermion-only tensors keep the established factorization policy",
 arFactorFermionMetrics["CoherentComponents"]===64 && !arFactorFermionMetrics["FactorizationSourceWorkAdmitted"] &&
 arFactorFermionMetrics["FactorizationSourceWorkLeaves"]===0 &&
 arPolicy["Factorization",arFactorRequests,arFactorFermionMetrics]["Settings"]===
 arPolicy["Factorization",arFactorRequests,arMetrics[arFactorFermionReport]]["Settings"]];
arFactorQEDReport=Join[arSourceReport,<|"Amplitude"->SpinorChain[Spinor["Spin","Angle",1],Spinor["Spin","Square",2]],
 "ExternalMassRules"->arRules4|>];
arFactorQEDMetrics=arFactorSourceMetrics[arFactorQEDReport,{},arFactorSM,arFactorRequests];
check["primitive QED spin tensors do not acquire additional factorization source work",
 arFactorQEDMetrics["CoherentComponents"]===4 && !arFactorQEDMetrics["FactorizationSourceWorkAdmitted"] &&
 arPolicy["Factorization",arFactorRequests,arFactorQEDMetrics]["Settings"]===
 arPolicy["Factorization",arFactorRequests,arMetrics[arFactorQEDReport]]["Settings"]];
check["the source counter uses current channel flags without deduplicating complete histories",
 arFactorMeasured["RetainedSourceRecords"]===4 &&
 arMetrics[Join[arSourceReport,<|"ChannelRemainderData"->ConstantArray[arSourceRecord,99]|>],
 arSourcePreparedChannels]["RetainedSourceRecords"]===4];
check["factorization source measurement leaves caller models and reports unchanged",
 Hash[arSourceReport,"SHA256"]===arSourceReportHash &&
 arFactorMeasured["ExpressionLeaves"]===LeafCount[arSourceExpression]];

(* Exact classification and held-out validation remain work after fitting. *)
arVectorReservePolicy=arPolicy["HighEnergy",<|"TimeConstraint"->Automatic|>,arSM71Resource];
check["four-vector automatic HE includes the measured finite post-fit reserve",
 arVectorReservePolicy["ResourcePolicy"]["PostFitReserve"]===160 &&
 arVectorReservePolicy["Settings"]["TimeConstraint"]===868];
check["constructor and HE reserve classification and validation before their clocks begin",
 arPolicy["Construction",<|"TimeConstraint"->Automatic|>,arConstructionVectors]["ResourcePolicy"]["PostFitReserve"]===320 &&
 arPolicy["Construction",<|"TimeConstraint"->Automatic|>,arConstructionVectors]["Settings"]["TimeConstraint"]===1250];
check["three-vector coherent work receives the same structural reserve rule",
 arPolicy["HighEnergy",<|"TimeConstraint"->Automatic|>,arVectorMetrics]["ResourcePolicy"]["PostFitReserve"]===90];
check["a 64-component degree-one-only tensor receives no new reserve",
 AllTrue[{"Construction","HighEnergy"},arPolicy[#,<|"TimeConstraint"->Automatic|>,arFactorFermionMetrics]["ResourcePolicy"]["PostFitReserve"]===0&]];
check["explicit clocks do not inherit an automatic post-fit reserve",
 AllTrue[{"Construction","HighEnergy"},arPolicy[#,<|"TimeConstraint"->41/7|>,arConstructionVectors]["Settings"]["TimeConstraint"]===41/7 &&
 arPolicy[#,<|"TimeConstraint"->41/7|>,arConstructionVectors]["ResourcePolicy"]["PostFitReserve"]===0&]];
check["factorization retains its separate prefit deadline without the HE reserve",
 arFactorResolved["ResourcePolicy"]["PostFitReserve"]===0 &&
 arPolicy["Factorization",<|"TimeConstraint"->Automatic|>,arConstructionVectors]["ResourcePolicy"]["PostFitReserve"]===0];
arThreeVectorTail=Join[arVectorMetrics,<|"Channels"->2,"RetainedDirections"->96,
 "RetainedSourceRecords"->4,"RetainedDirectionLeaves"->152000|>];
arThreeVectorOuter=Join[arConstructionVectors,<|"CoherentComponents"->27,
 "CoherentSpinDegrees"-><|2->2,3->2,4->2|>|>];
check["three-vector overall allowance leaves time after prefix and full HE child",
 arPolicy["Construction",<|"TimeConstraint"->Automatic|>,arThreeVectorOuter]["Settings"]["TimeConstraint"]===900 &&
 arPolicy["HighEnergy",<|"TimeConstraint"->Automatic|>,arThreeVectorTail]["Settings"]["TimeConstraint"]===733 &&
 900-78-733>0];
arFourVectorTail=Join[arSM71Resource,<|"RetainedSourceRecords"->12|>];
check["four-vector overall allowance leaves a separate final-check margin",
 arPolicy["HighEnergy",<|"TimeConstraint"->Automatic|>,arFourVectorTail]["Settings"]["TimeConstraint"]===916 &&
 arPolicy["Construction",<|"TimeConstraint"->Automatic|>,arConstructionVectors]["Settings"]["TimeConstraint"]-70-916>0];
arHugeTensor=Join[arConstructionVectors,<|"ExternalParticles"->10,"CoherentComponents"->10^9|>];
check["automatic post-fit reserves have a finite process-independent ceiling",
 AllTrue[{"Construction","HighEnergy"},arPolicy[#,<|"TimeConstraint"->Automatic|>,arHugeTensor]["ResourcePolicy"]["PostFitReserve"]===If[#==="Construction",1800,900] &&
 arFinite[arPolicy[#,<|"TimeConstraint"->Automatic|>,arHugeTensor]["Settings"]["TimeConstraint"]]&]];
check["post-fit reservation does not change caps or sampling obligations",
 KeyTake[arPolicy["HighEnergy",arFactorRequests,arConstructionVectors]["Settings"],
  {"MaxCorrectionTerms","Samples","ValidationSamples","MaxRefinements"}]===
 KeyTake[arPolicy["HighEnergy",Join[arFactorRequests,<|"TimeConstraint"->41/7|>],arConstructionVectors]["Settings"],
  {"MaxCorrectionTerms","Samples","ValidationSamples","MaxRefinements"}]];

arDiagramModel = ReadModel[FileNameJoin[{root, "models"}], 2];
arFactorModel = ReadModel[FileNameJoin[{root, "models"}], 3];
arModelHashes = Hash[#, "SHA256"] & /@ {arDiagramModel, arFactorModel};
arProcess4 = {"e", "E", "A.+", "A.+"};
arProcess5 = {"e", "E", "A.+", "A.+", "A.+"};
arDefault4 = ConstructAmplitude[arProcess4, arDiagramModel, arDiagramModel];
check["a real four-point QED amplitude constructs with no options",
  !FailureQ[arDefault4] && arDefault4 =!= $Failed && !AssociationQ[arDefault4]];
arDefault5 = ConstructAmplitude[arProcess5, arDiagramModel, arFactorModel];
check["a real five-point QED amplitude constructs with no options",
  !FailureQ[arDefault5] && arDefault5 =!= $Failed && !AssociationQ[arDefault5]];
arProcess6Zero = {"e", "e", "A.+", "A.+", "A.+", "A.+"};
arDefault6Zero = ConstructAmplitude[arProcess6Zero, arDiagramModel, arFactorModel];
check["a genuinely forbidden six-point QED process still validates with no options",
  arDefault6Zero === 0];
arFreshCheck[expression_, process_, model_] := Module[{arRows, arReport},
  arRows = FactorizationAmplitudeChannels[process, model];
  arReport = CheckAmplitudes[expression, arRows, model,
    "Samples" -> 2, "HighEnergySamples" -> 2, "ReferenceSamples" -> 2,
    "RandomSeed" -> 472581, "Output" -> "Details", "TimeConstraint" -> 60];
  AssociationQ[arReport] && arReport["Checks"] =!= {} &&
    AllTrue[arReport["Checks"], #["Status"] === "PASS" &]];
check["the option-free four-point result passes fresh independent checks",
  arFreshCheck[arDefault4, arProcess4, arDiagramModel]];
check["the option-free five-point result passes fresh independent checks",
  arFreshCheck[arDefault5, arProcess5, arFactorModel]];
arOverride = ConstructAmplitude[arProcess4, arDiagramModel, arDiagramModel,
  "Output" -> "Details", "Output" -> "Expression",
  "TimeConstraint" -> 90, "TimeConstraint" -> 180,
  "Samples" -> 1, "ValidationSamples" -> Automatic,
  "PreparationOptions" -> {"MaxSteps" -> 0, "TimeConstraint" -> 15},
  "FactorizationOptions" -> {"MaxCorrectionTerms" -> 32, "TimeConstraint" -> 30, "TimeConstraint" -> 80},
  "HighEnergyOptions" -> {"MaxCorrectionTerms" -> 32, "TimeConstraint" -> 30},
  "PrintReport" -> False, "ProgressReporting" -> False, "Parallelization" -> False];
check["explicit constructor choices retain ordinary first-rule precedence",
  AssociationQ[arOverride] && arOverride["Status"] === "COMPLETE"];
check["an explicit identity-search override reaches preparation unchanged",
  arOverride["Stages"]["Preparing diagrams"]["Options"]["MaxSteps"] === 0];
check["explicit nested resource choices retain their values and first-rule precedence",
  With[{arResources = arOverride["Stages"]["Completing factorization"]["ResourcePolicy"]},
    arResources["Requested"]["MaxCorrectionTerms"] === 32 &&
      arResources["Resolved"]["MaxCorrectionTerms"] === 32 &&
      arResources["Requested"]["TimeConstraint"] === 30 &&
      arResources["Resolved"]["TimeConstraint"] === 30]];
check["automatic final-check clocks are finite and explicit clocks stay exact",
  arFinite[arPolicy["Checks", <|"TimeConstraint" -> Automatic|>, arSix]["Settings"]["TimeConstraint"]] &&
    arPolicy["Checks", <|"TimeConstraint" -> 41/7|>, arLarge]["Settings"]["TimeConstraint"] === 41/7];
arCheckDeadline = ConstructAmplitude[arProcess4, arDiagramModel, arDiagramModel,
  "PrintReport" -> False, "ProgressReporting" -> False, "Parallelization" -> False,
  "CheckOptions" -> {"TimeConstraint" -> 0.000001}];
check["a user's explicit final-check deadline is honored instead of replaced by Automatic",
  FailureQ[arCheckDeadline] && Last[arCheckDeadline]["Stage"] === "Checking final amplitude"];
arExplicitFit = Block[{ConstructiveDiagrams`Private`$completionAutomaticFitSamples = True},
  CompleteFactorization[
    {arOverride["AmplitudePair"][[1]], 0},
    FactorizationAmplitudeChannels[arProcess4, arDiagramModel], arDiagramModel,
    "CorrectionBasis" -> (Last /@ arOverride["Stages"]["Preparing diagrams"]["PreparedAmplitudes"]),
    "Samples" -> 1, "ValidationSamples" -> 1, "TimeConstraint" -> Automatic,
    "ProgressReporting" -> False, "Parallelization" -> False, "ReturnDetails" -> True]];
check["automatic time selection never overrides an explicit fit sample count in a nested call",
  AssociationQ[arExplicitFit] && arExplicitFit["Status"] === "COMPLETE" &&
    arExplicitFit["FitSamples"] === 1 && arExplicitFit["ResourcePolicy"]["Resolved"]["Samples"] === 1];
arNumericFit = Block[{ConstructiveDiagrams`Private`$completionAutomaticFitSamples = True,
  ConstructiveDiagrams`Private`$completionAutomaticCorrectionTermCeiling = 1024},
  CompleteFactorization[{arOverride["AmplitudePair"][[1]], 0},
    FactorizationAmplitudeChannels[arProcess4, arDiagramModel], arDiagramModel,
    "CorrectionBasis" -> (Last /@ arOverride["Stages"]["Preparing diagrams"]["PreparedAmplitudes"]),
    "Samples" -> 1, "ValidationSamples" -> 1, "TimeConstraint" -> 30, "MaxCorrectionTerms" -> 32,
    "ProgressReporting" -> False, "Parallelization" -> False, "ReturnDetails" -> True]];
check["fully numeric public calls isolate their samples from an enclosing automatic fit",
  AssociationQ[arNumericFit] && arNumericFit["Status"] === "COMPLETE" && arNumericFit["FitSamples"] === 1];
check["an explicit numeric public cap isolates inherited automatic permissions",
  arNumericFit["EffectiveMaxCorrectionTerms"] === 32 &&
    arNumericFit["CapChanges"] === {} && Length[arNumericFit["CorrectionBasis"]] <= 32];

(* The controller must distinguish a validated expression from its pole
   presentation. Multiply a real checked amplitude by an exact quotient 1;
   mock only private dispatch so the fallback branch is inexpensive. *)
arPreferenceU = SpinorChain[Spinor["Helicity", "Angle", 3], Spinor["Helicity", "Angle", 4]];
arPreferenceV = SpinorChain[Spinor["Helicity", "Angle", 3], Mom[1], Mom[2],
  Spinor["Helicity", "Angle", 4]];
arPreferenceIdentity = (arPreferenceU^2 - arPreferenceV^2)/
  ((arPreferenceU - arPreferenceV) (arPreferenceU + arPreferenceV));
arPreferenceBase = arOverride["Stages"]["Completing factorization"];
arPreferenceExpression = arPreferenceBase["Amplitude"] arPreferenceIdentity;
arPreferenceEvents = {};
arPreferenceRun[expression_, extra_List : {}] := Internal`InheritedBlock[
  {ConstructiveDiagrams`Private`completionFactorizationCore},
  Clear[ConstructiveDiagrams`Private`completionFactorizationCore];
  ConstructiveDiagrams`Private`completionFactorizationCore[args___] :=
    Join[KeyDrop[arPreferenceBase, {"RequestedPolePreference", "AchievedPhysicalDenominators",
      "PreferenceFallbackReason"}], <|"Amplitude" -> expression,
      "FamilyAmplitude" -> expression, "CorrectionSpace" -> "ShiftedChannels"|>];
  CompleteFactorization[{arPreferenceBase["ExternalMassRules"], expression},
    FactorizationAmplitudeChannels[arProcess4, arDiagramModel], arDiagramModel,
    "PolePreference" -> "AllPhysical", "ReturnDetails" -> True,
    "ProgressReporting" -> True, "Parallelization" -> False,
    "ProgressFunction" -> Function[event, AppendTo[arPreferenceEvents, event]],
    Sequence @@ extra]];
arPreferenceFallback = arPreferenceRun[arPreferenceExpression];
check["the fallback presentation fixture is exactly the same checked amplitude",
  Cancel[arPreferenceIdentity - 1] === 0 &&
    arPreferenceBase["Status"] === "COMPLETE" &&
    AllTrue[arPreferenceBase["ValidationReport"]["Channels"], #["Status"] === "PASS" &]];
check["a validated AllPhysical fallback stays complete but reports the unmet representation",
  arPreferenceFallback["Status"] === "COMPLETE" &&
    arPreferenceFallback["RequestedPolePreference"] === "AllPhysical" &&
    arPreferenceFallback["AchievedPhysicalDenominators"] === False &&
    StringQ[arPreferenceFallback["PreferenceFallbackReason"]]];
check["pole-preference reporting changes neither the fitted amplitude nor residue validation",
  arPreferenceFallback["Amplitude"] === arPreferenceExpression &&
    arPreferenceFallback["ValidationReport"] === arPreferenceBase["ValidationReport"] &&
    arPreferenceFallback["CorrectionSpace"] === "ShiftedChannels"];
check["the completion progress event discloses a valid representation fallback",
  Count[arPreferenceEvents, event_Association /;
    Lookup[event, "Stage", None] === "Factorization representation" &&
      Lookup[event, "Status", None] === "COMPLETE" &&
      Lookup[event, "AchievedPhysicalDenominators", True] === False] === 1];
arPreferenceNumeric = arPreferenceRun[arPreferenceExpression,
  {"TimeConstraint" -> 30, "Samples" -> 1, "ValidationSamples" -> 1, "MaxCorrectionTerms" -> 32}];
check["fully explicit resource calls retain the same truthful pole-preference details",
  arPreferenceNumeric["Status"] === "COMPLETE" &&
    arPreferenceNumeric["RequestedPolePreference"] === "AllPhysical" &&
    arPreferenceNumeric["AchievedPhysicalDenominators"] === False];
arPreferenceExhausted = arPreferenceRun[arPreferenceExpression,
  {"TimeConstraint" -> 10^-9, "Samples" -> 1, "ValidationSamples" -> 1, "MaxCorrectionTerms" -> 32}];
check["representation reporting respects an exhausted explicit shared clock",
  arPreferenceExhausted["Status"] === "COMPLETE" &&
    MissingQ[arPreferenceExhausted["AchievedPhysicalDenominators"]] &&
    arPreferenceExhausted["PreferenceFallbackReason"] === None];
arPreferencePhysicalDiagrams = PrepareAmplitudes[
  DiagramAmplitudes[CreateDiagrams[arProcess4, arDiagramModel]], arDiagramModel,
  "PolePreference" -> "AllPhysical", "ProgressReporting" -> False, "Parallelization" -> False];
arPreferencePhysicalSource = CompleteFactorization[arPreferencePhysicalDiagrams,
  FactorizationAmplitudeChannels[arProcess4, arDiagramModel], arDiagramModel,
  "PolePreference" -> "AllPhysical", "ReturnDetails" -> True,
  "ProgressReporting" -> False, "Parallelization" -> False];
arPreferencePhysical = arPreferenceRun[arPreferencePhysicalSource["Amplitude"]];
check["a physical-only result reports success without a fallback reason",
  arPreferencePhysicalSource["Status"] === "COMPLETE" &&
    arPreferencePhysicalSource["AchievedPhysicalDenominators"] === True &&
    arPreferencePhysical["Status"] === "COMPLETE" &&
    arPreferencePhysical["RequestedPolePreference"] === "AllPhysical" &&
    arPreferencePhysical["AchievedPhysicalDenominators"] === True &&
    arPreferencePhysical["PreferenceFallbackReason"] === None];
check["small automatic sample batches avoid cold worker startup",
  ConstructiveDiagrams`Private`completionWarmOnlyQ[{{1, 1}}, {arParameter}, {1, arParameter}]];
check["large exact sample batches may rebuild a cold pool after a short probe",
  !ConstructiveDiagrams`Private`completionWarmOnlyQ[Tuples[{Range[14], Range[4]}],
    Table[Unique["sampleCoefficient"], 54], ConstantArray[1, 55]]];
check["the automatic and explicit workflows leave both supplied models unchanged",
  (Hash[#, "SHA256"] & /@ {arDiagramModel, arFactorModel}) === arModelHashes];
If[Environment["HEPCAT_TEST_FULL_AUTOMATIC_CONSTRUCTION"] === "1",
  arDefault6 = ConstructAmplitude[{"e", "E", "A.+", "A.+", "A.+", "A.+"}, arDiagramModel, arFactorModel];
  check["a nonzero six-point QED amplitude constructs with no options",
    !FailureQ[arDefault6] && arDefault6 =!= $Failed && !AssociationQ[arDefault6]];
  check["the option-free nonzero six-point result passes fresh independent checks",
    arFreshCheck[arDefault6, {"e", "E", "A.+", "A.+", "A.+", "A.+"}, arFactorModel]],
  Print["SKIP: set HEPCAT_TEST_FULL_AUTOMATIC_CONSTRUCTION=1 for the nonzero six-point automatic constructor regression."]];
If[Environment["HEPCAT_TEST_FULL_PHYSICAL_MIXED_CONSTRUCTION"] === "1",
  arMixedProcess = {"e", "E", "A.+", "A.+", "A.+", "A.-"};
  arMixedChannels = FactorizationAmplitudeChannels[arMixedProcess, arFactorModel];
  arMixedFeynmanModel = ReadModel[FileNameJoin[{root, "model_feynman"}], 2];
  arMixedReport = ConstructAmplitude[arMixedProcess, arDiagramModel, arFactorModel,
    "PolePreference" -> "AllPhysical", "FeynmanModel" -> arMixedFeynmanModel,
    "Output" -> "Details", "PrintReport" -> False, "ProgressReporting" -> False];
  If[AssociationQ[arMixedReport] && arMixedReport["Status"] === "COMPLETE",
    Print["MIXED AUTOMATIC BENCHMARK: ", <|"Status" -> arMixedReport["Status"],
      "TotalTime" -> arMixedReport["TotalTime"], "Stages" -> arMixedReport["StageTimings"],
      "Leaves" -> LeafCount[Last[arMixedReport["AmplitudePair"]]],
      "Checks" -> Counts[Lookup[arMixedReport["Checks"]["Checks"], "Status"]]|>]];
  check["mixed six-point construction completes with only semantic choices",
    AssociationQ[arMixedReport] && arMixedReport["Status"] === "COMPLETE" &&
      arMixedReport["Stages"]["Completing high-energy behavior"]["FreeParameters"] === {}];
  check["the automatic physical mixed result is compact and has only catalogued physical poles",
    AssociationQ[arMixedReport] && arMixedReport["Status"] === "COMPLETE" &&
      LeafCount[Last[arMixedReport["AmplitudePair"]]] < 10000 &&
      ConstructiveDiagrams`Private`completionPhysicalDenominatorOnlyQ[
        Last[arMixedReport["AmplitudePair"]],
        ConstructiveDiagrams`Private`completionNormalizeInput[
          arMixedReport["AmplitudePair"], arMixedChannels, arFactorModel]]];
  check["the mixed physical result passes every mandatory check and the independent Feynman benchmark",
    AssociationQ[arMixedReport] && arMixedReport["Status"] === "COMPLETE" &&
      Length[arMixedReport["Checks"]["Channels"]] === 14 &&
      AllTrue[arMixedReport["Checks"]["Checks"], #["Status"] === "PASS" &] &&
      arMixedReport["Checks"]["FeynmanCheck"]["Status"] === "PASS"],
  Print["SKIP: set HEPCAT_TEST_FULL_PHYSICAL_MIXED_CONSTRUCTION=1 for the full AllPhysical mixed six-point constructor regression."]];
arExpected = 126 + If[Environment["HEPCAT_TEST_FULL_AUTOMATIC_CONSTRUCTION"] === "1", 2, 0] +
  If[Environment["HEPCAT_TEST_FULL_PHYSICAL_MIXED_CONSTRUCTION"] === "1", 3, 0];
If[checks =!= arExpected, AppendTo[failures, "Not every automatic-resource assertion executed."]];
Print["AUTOMATIC RESOURCE CHECKS: ", checks, "; FAILURES: ", Length[failures]];
If[failures =!= {}, Print[failures]];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:automatic-resource-tests.wl"]];
Quit[If[failures === {}, 0, 1]];
