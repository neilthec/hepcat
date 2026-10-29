(* Exact transverse finiteness checks on the full amplitude. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
SetOptions[CheckPhysicalResidues, "HighEnergyCheck" -> False, "ReferenceCheck" -> False];
Get[FileNameJoin[{root, "tests", "QED-5-point-residues.wl"}]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name];
  Print["FAIL: ", name]]);
spReport[expression_, factors_, opts___] :=
  ConstructiveDiagrams`Private`spuriousCheckReport[expression, factors, prTargets,
    "Samples" -> 2, "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1},
    "TimeConstraint" -> 20, opts];
spAngle34 = prAngle[3, 4]; spSquare34 = prSquare[3, 4];
spXi3 = SpinorChain[Spinor["Xi", "Angle", 0], Spinor["Helicity", "Angle", 3]];
spXi4 = SpinorChain[Spinor["Xi", "Angle", 0], Spinor["Helicity", "Angle", 4]];

Do[
  expression = {prH, prS2}[[index]];
  spRows = spReport[expression, {spAngle34, spSquare34}];
  check["H and S2 finite on both complex branches " <> ToString[index],
    Lookup[spRows, "Status"] === {"PASS", "PASS"} &&
      AllTrue[spRows, #["CompletedSamples"] === 2 &]],
  {index, 2}
];
spOld = First[spReport[prOldPermutation, {spAngle34}]];
check["old permutation continuation retains its spurious angle pole",
  spOld["Status"] === "FAIL" && spOld["PoleOrder"] >= 1];
Do[
  spRows = spReport[1/factor^power, {factor}];
  check["negative Laurent power " <> ToString[{factor, power}, InputForm],
    First[spRows]["Status"] === "FAIL" && First[spRows]["PoleOrder"] === power],
  {factor, {spAngle34, spSquare34, spXi3}}, {power, {1, 2, 3}}
];
check["zero amplitude is regular", First[spReport[0, {spAngle34}]]["Status"] === "PASS"];
check["pole cancellation is performed on the full sum",
  First[spReport[(spAngle34 + spSquare34)/spAngle34 - spSquare34/spAngle34,
    {spAngle34}]]["Status"] === "PASS"];
check["shared reference label is evaluated consistently",
  First[spReport[(spXi3 + spXi4)/spXi3 - spXi4/spXi3,
    {spXi3}]]["Status"] === "PASS"];
spSquareXi3 = SpinorChain[Spinor["Xi", "Square", 7],
  Spinor["Helicity", "Square", 3]];
check["square-reference bracket poles are also sampled",
  First[spReport[1/spSquareXi3^2, {spSquareXi3}]]["PoleOrder"] === 2];
check["unsupported factor is unresolved",
  First[spReport[1/spAngle34, {MomProd[3, 4]}]]["Status"] === "UNRESOLVED"];
DownValues[spWarningEvaluateDelegate] = DownValues[
  ConstructiveDiagrams`Private`residueEvaluateChart] /.
    HoldPattern[ConstructiveDiagrams`Private`residueEvaluateChart] -> spWarningEvaluateDelegate;
spSingularTrialCount = 0;
spSingularTrialReport = Check[Block[
  {ConstructiveDiagrams`Private`residueEvaluateChart = Function[{value, chart, rules},
    If[value === spSingularTrialAmplitude,
      spSingularTrialCount++;
      Message[Power::infy, HoldForm[1/0]];
      Message[Infinity::indet, HoldForm[0 ComplexInfinity]];
      ComplexInfinity,
      spWarningEvaluateDelegate[value, chart, rules]]]},
  First[spReport[spSingularTrialAmplitude, {spAngle34}]]],
  spUnexpectedSingularWarning, {Power::infy, Infinity::indet}];
check["expected singular-trial warnings stay local without fabricating a finite pass",
  AssociationQ[spSingularTrialReport] && spSingularTrialReport["Status"] === "UNRESOLVED" &&
    spSingularTrialCount > 1 &&
    AllTrue[spSingularTrialReport["Samples"], #["Status"] === "UNRESOLVED" &]];
spUnrelatedTrialEvaluator::probe = "An unrelated trial-evaluator diagnostic.";
spUnrelatedTrialReport = Missing["NotRun"];
spUnrelatedTrialCaught = Block[{$Messages = {}}, Check[Block[
  {ConstructiveDiagrams`Private`residueEvaluateChart = Function[{value, chart, rules},
    If[value === spSingularTrialAmplitude,
      Message[spUnrelatedTrialEvaluator::probe]; ComplexInfinity,
      spWarningEvaluateDelegate[value, chart, rules]]]},
  spUnrelatedTrialReport = First[spReport[spSingularTrialAmplitude, {spAngle34}]]],
  spExpectedUnrelatedWarning, {spUnrelatedTrialEvaluator::probe}]];
check["unrelated evaluator diagnostics are not silenced by singular-chart handling",
  spUnrelatedTrialCaught === spExpectedUnrelatedWarning &&
    AssociationQ[spUnrelatedTrialReport] && spUnrelatedTrialReport["Status"] === "UNRESOLVED"];
check["massive angle pairs are not silently treated as massless collinear poles",
  First[spReport[1/prSpinAngle[1, 2], {prSpinAngle[1, 2]}]]["Status"] === "UNRESOLVED"];
check["supplied physical maps are not silently reused as spurious charts",
  First[spReport[0, {spAngle34}, "KinematicMap" -> Function[{x, c, v}, x]]]["Status"] === "UNRESOLVED"];
spPhysical = Join[First[prTargets], <|"Cut" -> {3, 4}, "Complement" -> {1, 2, 5},
  "Mass" -> 0, "Propagator" -> PropDen[Mom[Multiparticle[3, 4]], 0]|>];
spCoincident = First[ConstructiveDiagrams`Private`spuriousCheckReport[
  1/spAngle34, {spAngle34}, Append[prTargets, spPhysical], "Samples" -> 1,
  "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1}, "TimeConstraint" -> 20]];
check["coincident physical poles are not mistaken for spurious failures",
  spCoincident["Status"] === "UNRESOLVED" &&
    AnyTrue[spCoincident["Diagnostics"], StringContainsQ[#, "physical pole"] &]];
spTimed = First[ConstructiveDiagrams`Private`spuriousCheckReport[
  prS2, {spAngle34}, prTargets, "Samples" -> 100,
  "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1}, "TimeConstraint" -> 0.00001]];
check["time limit is reported as unresolved", spTimed["Status"] === "UNRESOLVED"];
spLaurent = ConstructiveDiagrams`Private`spuriousLaurentData[
  ((1 + Sqrt[2])^2 - (3 + 2 Sqrt[2]))/z^2 + 7, z];
check["algebraic coefficients cancel exactly", AssociationQ[spLaurent] &&
  spLaurent["Order"] === 0 && spLaurent["Limit"] === 7];
check["unknown parameters cannot produce a fabricated numerical pass",
  MissingQ[ConstructiveDiagrams`Private`spuriousLaurentData[c/z, z]]];
spSixMetadata = <|"Cut" -> {1, 2}, "Complement" -> {3, 4, 5, 6},
  "ExternalLegs" -> Range[6], "Mass" -> 2,
  "Propagator" -> PropDen[Mom[Multiparticle[1, 2]], 2],
  "ExternalMassRules" -> Thread[(Mass /@ Range[6]) -> ConstantArray[0, 6]]|>;
spSixRows = ConstructiveDiagrams`Private`spuriousCheckReport[1/spAngle34^2,
  {spAngle34, spSquare34}, {spSixMetadata}, "Samples" -> 2,
  "TimeConstraint" -> 20];
check["six-point charts distinguish both complex branches",
  Lookup[spSixRows, "Status"] === {"FAIL", "PASS"} &&
    Lookup[spSixRows, "PoleOrder"] === {2, 0}];
spChart = First[spOld["Samples"]]["Chart"];
check["sample momenta conserve momentum away from the spurious pole",
  ConstructiveDiagrams`Private`residueExactZero[Total[Values[spChart["Momenta"]]]]];
check["every external mass shell is preserved throughout the Laurent chart",
  AllTrue[Keys[spChart["Momenta"]],
    ConstructiveDiagrams`Private`residueExactZero[
      Det[spChart["Momenta"][#]] - spChart["Masses"][#]^2] &]];
check["all physical propagators remain generic on the spurious pole",
  ConstructiveDiagrams`Private`spuriousPhysicalGenericQ[
    prOldPermutation, prTargets, spChart, {prElectronMass -> 1, prElectricCharge -> 1}]];
spPublicDefault = CheckPhysicalResidues[prH, prChannels, prFixtureModel,
  "Samples" -> 1, "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1}];
check["default public table has one compact schema even without spurious poles",
  Head[spPublicDefault] === Dataset && AllTrue[Normal[spPublicDefault],
    Sort[Keys[#]] === Sort[{"Type", "Pole", "Status"}] && #["Type"] === "Physical" &]];
spPublic = CheckPhysicalResidues[prS2, prChannels, prFixtureModel,
  "Samples" -> 1, "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1},
  "SpuriousPoles" -> {spAngle34, spSquare34}, "Output" -> "Details"];
check["public details preserve physical and spurious rows separately",
  Length[spPublic["Channels"]] === 6 && Length[spPublic["SpuriousChecks"]] === 2 &&
    spPublic["PhysicalSummary"] === <|"PASS" -> 6|> &&
    spPublic["SpuriousSummary"] === <|"PASS" -> 2|> &&
    spPublic["Summary"] === <|"PASS" -> 8|>];
spPublicTable = CheckPhysicalResidues[prH, prChannels, prFixtureModel,
  "Samples" -> 1, "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1},
  "SpuriousPoles" -> {spAngle34}];
check["combined public table is compact and labels each pole type",
  Head[spPublicTable] === Dataset && Length[Normal[spPublicTable]] === 7 &&
    AllTrue[Normal[spPublicTable], Sort[Keys[#]] === Sort[{"Type", "Pole", "Status"}] &] &&
    Counts[Lookup[Normal[spPublicTable], "Type"]] ===
      <|"Physical" -> 6, "Spurious" -> 1|>];
spPublicResidues = CheckPhysicalResidues[prH, prChannels, prFixtureModel,
  "Samples" -> 1, "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1},
  "SpuriousPoles" -> {spAngle34}, "Output" -> "Residues"];
check["public residue view displays scalar spurious limits without collapsed lists",
  Head[spPublicResidues] === Dataset && Length[Normal[spPublicResidues]] === 7 &&
    AllTrue[Normal[spPublicResidues], ContainsAll[Keys[#],
      {"Type", "Pole", "Sample", "Status", "Calculated", "Expected", "Difference", "PoleOrder", "Limit"}] &&
      AllTrue[Lookup[#, {"Calculated", "Expected", "Difference", "Limit"}], !ListQ[#] &] &] &&
    Counts[Lookup[Normal[spPublicResidues], "Type"]] === <|"Physical" -> 6, "Spurious" -> 1|>];
check["a finite spurious sample compares zero singular coefficient with zero",
  With[{row = SelectFirst[Normal[spPublicResidues], #["Type"] === "Spurious" &]},
    row["Calculated"] === 0 && row["Expected"] === 0 && row["Difference"] === 0 &&
      row["PoleOrder"] === 0 && NumericQ[row["Limit"]]]];
spPublicUnsupported = CheckPhysicalResidues[prH, prChannels, prFixtureModel,
  "Samples" -> 1, "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1},
  "SpuriousPoles" -> {MomProd[3, 4]}, "Output" -> "Details"];
check["unsupported public spurious factors are explicitly unresolved",
  spPublicUnsupported["SpuriousSummary"] === <|"UNRESOLVED" -> 1|> &&
    spPublicUnsupported["PhysicalSummary"] === <|"PASS" -> 6|>];
spInternalPhoton = SpinorChain[
  Spinor["Helicity", "Angle", Multiparticle[3, 4]],
  Spinor["Helicity", "Angle", 5]];
spInternalResult = First[spReport[spInternalPhoton/spAngle34, {spAngle34}]];
check["spurious charts never project an internal photon onto their synthetic cut",
  spInternalResult["Status"] === "UNRESOLVED"];
spPairChart = First[First[spPublic["SpuriousChecks"]]["Samples"]]["Chart"];
spReferenceResult = First[spReport[0, {spXi3}]];
spReferenceChart = First[spReferenceResult["Samples"]]["Chart"];
check["both spurious chart types explicitly disable internal cut spinors",
  spPairChart["AllowCutSpinors"] === False &&
    spReferenceChart["AllowCutSpinors"] === False];
check["synthetic pair cuts reject cut-only x-factor expansion",
  !FreeQ[ConstructiveDiagrams`Private`residueExpandCutXFactors[
    xFactor[3, 4] spInternalPhoton, spPairChart], _Missing]];
spNoPhysicalReport = CheckPhysicalResidues[
  {Thread[(Mass /@ Range[5]) -> {prElectronMass, prElectronMass, 0, 0, 0}], 1/spAngle34^2},
  {}, prModel, "Samples" -> 1, "ParameterRules" -> {prElectronMass -> 1},
  "SpuriousPoles" -> {spAngle34}, "Output" -> "Details"];
check["spurious tests can use external mass rules without any physical channels",
  spNoPhysicalReport["Channels"] === {} &&
    spNoPhysicalReport["PhysicalSummary"] === <||> &&
    spNoPhysicalReport["SpuriousSummary"] === <|"FAIL" -> 1|> &&
    First[spNoPhysicalReport["SpuriousChecks"]]["PoleOrder"] === 2];
spNoKinematicsReport = CheckPhysicalResidues[{}, {}, prModel,
  "SpuriousPoles" -> {spAngle34}, "Output" -> "Details"];
check["an empty process cannot fabricate external kinematics for a spurious pass",
  spNoKinematicsReport["SpuriousSummary"] === <|"UNRESOLVED" -> 1|>];

(* Automatic discovery uses the unsimplified input expression. The supplied
   factorization rows, not candidate denominators, define the physical poles. *)
spAutoDetails[expression_, channels_ : prChannels] := CheckPhysicalResidues[
  expression, channels, prFixtureModel, "Samples" -> 1,
  "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1}, "TimeConstraint" -> 20,
  "Output" -> "Details"];
spAutoS2 = spAutoDetails[prS2];
check["automatic spurious discovery is the public default",
  ("SpuriousPoles" /. Options[CheckPhysicalResidues]) === Automatic];
check["S2 automatically checks its three cancelled angle poles",
  spAutoS2["PhysicalSummary"] === <|"PASS" -> 6|> &&
    spAutoS2["SpuriousSummary"] === <|"PASS" -> 3|> &&
    spAutoS2["Summary"] === <|"PASS" -> 9|> &&
    Length[spAutoS2["SpuriousChecks"]] === 3];
check["automatic discovery retains pole provenance and multiplicity",
  ListQ[spAutoS2["PoleDiscovery"]] &&
    AllTrue[spAutoS2["PoleDiscovery"], ContainsAll[Keys[#],
      {"Factor", "CanonicalFactor", "Classification", "Multiplicity",
        "PhysicalChannels", "Diagnostics"}] &]];
spAutoOld = spAutoDetails[prOldPermutation];
check["the old permutation formula automatically exposes all three spurious failures",
  spAutoOld["SpuriousSummary"] === <|"FAIL" -> 3|> &&
    Length[spAutoOld["SpuriousChecks"]] === 3];
spAutoH = spAutoDetails[prH];
check["a physical-pole-only form has no invented spurious checks",
  spAutoH["PhysicalSummary"] === <|"PASS" -> 6|> &&
    spAutoH["SpuriousChecks"] === {} && spAutoH["SpuriousSummary"] === <||> &&
    spAutoH["Summary"] === <|"PASS" -> 6|>];
check["details make both families of checks inspectable",
  Length[spAutoS2["Checks"]] === 9 && Length[spAutoH["Checks"]] === 6];

spExternalRules = Thread[(Mass /@ Range[5]) -> {prElectronMass, prElectronMass, 0, 0, 0}];
spAutoNoPhysical[expression_] := CheckPhysicalResidues[
  {spExternalRules, expression}, {}, prModel, "Samples" -> 2,
  "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1}, "TimeConstraint" -> 20,
  "Output" -> "Details"];
spNested = spAutoNoPhysical[1/(prElectronMass^3 (spXi3 spXi4)^2)];
spNestedDiscovered = Select[spNested["PoleDiscovery"],
  #["Classification"] === "Spurious" &];
check["nested denominator products retain their multiplicities",
  Length[spNestedDiscovered] === 2 &&
    Lookup[spNestedDiscovered, "Multiplicity"] === {2, 2} &&
    spNested["SpuriousSummary"] === <|"FAIL" -> 2|> &&
    Lookup[spNested["SpuriousChecks"], "PoleOrder"] === {2, 2}];
spConstant = spAutoNoPhysical[1/prElectronMass^3];
check["inverse masses and numerical coefficients are not kinematic poles",
  spConstant["SpuriousChecks"] === {} &&
    !AnyTrue[spConstant["PoleDiscovery"],
      MemberQ[{"Spurious", "Unclassified"}, #["Classification"]] &]];
spRawCancelled = spAutoNoPhysical[
  (spAngle34 + spSquare34)/spAngle34 - spSquare34/spAngle34];
check["raw cancelled denominators remain visible and are checked on the full sum",
  Length[spRawCancelled["SpuriousChecks"]] === 1 &&
    spRawCancelled["SpuriousSummary"] === <|"PASS" -> 1|>];
spMixedChain = SpinorChain[Spinor["Helicity", "Angle", 3], Mom[1],
  Spinor["Helicity", "Square", 4]];
spMixedAuto = spAutoNoPhysical[1/spMixedChain];
check["mixed momentum-chain denominators receive actual cancellation checks",
  spMixedAuto["SpuriousSummary"] === <|"FAIL" -> 1|> &&
    First[spMixedAuto["SpuriousChecks"]]["PoleOrder"] === 1 &&
    AnyTrue[spMixedAuto["PoleDiscovery"], #["Classification"] === "Spurious" &]];
spUnsupportedChain = SpinorChain[Spinor["Helicity", "Angle", 3], Mom[7],
  Spinor["Helicity", "Square", 4]];
spUnsupportedAuto = spAutoNoPhysical[1/spUnsupportedChain];
check["unknown momentum-chain denominator surfaces instead of disappearing",
  spUnsupportedAuto["SpuriousSummary"] === <|"UNRESOLVED" -> 1|> &&
    AnyTrue[spUnsupportedAuto["PoleDiscovery"],
      #["Classification"] === "Unclassified" && #["Diagnostics"] =!= {} &]];
spUnsupportedValues = CheckPhysicalResidues[{spExternalRules, 1/spUnsupportedChain}, {}, prModel,
  "Samples" -> 1, "ParameterRules" -> {prElectronMass -> 1}, "Output" -> "Residues"];
check["uncomputed spurious values remain missing rather than fabricated zeros",
  Head[spUnsupportedValues] === Dataset &&
    AllTrue[Normal[spUnsupportedValues], #["Status"] === "UNRESOLVED" &&
      MissingQ[#["Calculated"]] && MissingQ[#["Difference"]] &]];
spUnknownPolynomial = spAutoNoPhysical[
  1/(spUnknownScale + MomProd[1, 3] MomProd[1, 4])];
check["an incomplete classification is explicitly unresolved",
  spUnknownPolynomial["SpuriousSummary"] === <|"UNRESOLVED" -> 1|> &&
    AnyTrue[spUnknownPolynomial["PoleDiscovery"], #["Classification"] === "Unclassified" &]];
spZeroNoChannels = CheckPhysicalResidues[0, {}, prModel, "Output" -> "Details"];
check["zero without channels needs neither invented poles nor fake kinematics",
  AssociationQ[spZeroNoChannels] && spZeroNoChannels["Checks"] === {} &&
    spZeroNoChannels["Summary"] === <||>];
check["a denominator constant on external mass shells does not invent a pole",
  ConstructiveDiagrams`Private`poleDiscoverDenominators[
    1/(1 + MomProd[3, 3]), prTargets, {}] === {}];
check["an identically zero external-mass denominator is not silently discarded",
  With[{rows = ConstructiveDiagrams`Private`poleDiscoverDenominators[
      Mass[3]^-2, prTargets, {}]},
    Length[rows] === 1 && First[rows]["Classification"] === "Unclassified"]];

(* Exact duplicates do not add a new apparent-pole obligation. Keep distinct
   powers and source forms while avoiding repeated factor classification. *)
spDedupPhysical = PropDen[Mom[Multiparticle[1, 3]], prElectronMass];
spDedupPhysicalSum = PropDen[Mom[1] + Mom[3], prElectronMass];
spDedupExpression = 1/spXi3 + 2/spXi3^3 + 1/(spXi3^2 - spXi4^2) +
  1/(spXi3 - spXi4) + 1/(2 MomProd[1, 3]) +
  1/spDedupPhysical + 1/spDedupPhysicalSum^2;
spDedupRecord = <|"Expression" -> spDedupExpression,
  "OriginalExpression" -> spDedupExpression, "MassRules" -> {}|>;
spDedupMassRecord = <|"Expression" -> 1/(spXi3 + Mass[3]),
  "OriginalExpression" -> 1/(spXi3 + Mass[3])^3, "MassRules" -> {Mass[3] -> 0}|>;
spDedupMassRecordOther = <|"Expression" -> 1/(spXi3 + Mass[4]),
  "OriginalExpression" -> 1/(spXi3 + Mass[4])^3, "MassRules" -> {Mass[4] -> 0}|>;
spDedupInventory = ConstructiveDiagrams`Private`poleDiscoverDenominators[
  spDedupExpression, prTargets, {spDedupRecord, spDedupMassRecord}];
spRepeatedRecords = Join[ConstantArray[spDedupRecord, 40],
  ConstantArray[spDedupMassRecord, 40], ConstantArray[spDedupMassRecordOther, 40]];
spRepeatedInventory = ConstructiveDiagrams`Private`poleDiscoverDenominators[
  spDedupExpression, prTargets, spRepeatedRecords];
check["repeated expressions and records retain exactly the complete apparent-pole inventory",
  spRepeatedInventory === spDedupInventory];
check["record duplicates are removed only after their individual mass rules are applied",
  ConstructiveDiagrams`Private`poleDiscoverDenominators[0, prTargets,
    {spDedupMassRecord, spDedupMassRecordOther}] ===
  ConstructiveDiagrams`Private`poleDiscoverDenominators[1/spXi3 + 1/spXi3^3, prTargets, {}]];
spDedupXiRow = SelectFirst[spRepeatedInventory, MemberQ[#["ApparentForms"], spXi3] &];
check["distinct denominator powers retain their existing maximum multiplicity",
  spDedupXiRow["Multiplicity"] === 3];
spDedupPhysicalRow = SelectFirst[spRepeatedInventory,
  #["CanonicalFactor"] === spDedupPhysical &];
check["canonical pole grouping retains every distinct original denominator form",
  spDedupPhysicalRow["Classification"] === "Physical" &&
    spDedupPhysicalRow["Multiplicity"] === 2 &&
    ContainsAll[spDedupPhysicalRow["ApparentForms"],
      {spDedupPhysical, spDedupPhysicalSum, MomProd[1, 3]}]];
DownValues[spPoleClassifyDelegate] = DownValues[
  ConstructiveDiagrams`Private`poleClassifyFactor] /.
    HoldPattern[ConstructiveDiagrams`Private`poleClassifyFactor] -> spPoleClassifyDelegate;
spPoleClassificationCalls = {};
spCountedInventory = Block[
  {ConstructiveDiagrams`Private`poleClassifyFactor = Function[{factor, power, channels, metadata},
    AppendTo[spPoleClassificationCalls, {factor, power}];
    spPoleClassifyDelegate[factor, power, channels, metadata]]},
  ConstructiveDiagrams`Private`poleDiscoverDenominators[
    spDedupExpression, prTargets, spRepeatedRecords]];
check["each distinct factored base and power is classified once despite repeated input records",
  spCountedInventory === spDedupInventory && spPoleClassificationCalls =!= {} &&
    DuplicateFreeQ[spPoleClassificationCalls]];
check["deduplication does not merge distinct powers before classification",
  ContainsAll[spPoleClassificationCalls, {{spXi3, 1}, {spXi3, 3}}]];
check["overlapping factorizations preserve all factors without repeated classification",
  MemberQ[spPoleClassificationCalls, {spXi3 - spXi4, 1}] &&
    MemberQ[spPoleClassificationCalls, {spXi3 + spXi4, 1}] &&
    Count[spPoleClassificationCalls, {spXi3 - spXi4, 1}] === 1];
spSymbolicPowerInventory = ConstructiveDiagrams`Private`poleDiscoverDenominators[
  1/spXi3^spSymbolicPolePower + 1/spXi3^3, prTargets, {}];
check["symbolic denominator powers retain their unresolved multiplicity and diagnostics",
  Length[spSymbolicPowerInventory] === 1 &&
    First[spSymbolicPowerInventory]["Multiplicity"] === Missing["SymbolicPower"] &&
    First[spSymbolicPowerInventory]["Classification"] === "Unclassified" &&
    First[spSymbolicPowerInventory]["Diagnostics"] =!= {}];

(* Rank-one momentum factorization is an ordinary external massless identity,
   used only to discover the zero sets of denominator factors. *)
spFactorMetadata = <|"ExternalLegs" -> Range[6],
  "ExternalMassRules" -> Thread[(Mass /@ Range[6]) ->
    {prElectronMass, prElectronMass, 0, 0, 0, 0}]|>;
spFactorChain[kind_, middle_List, right_, left_ : 3] := SpinorChain @@ Join[
  {Spinor["Helicity", kind, left]}, Mom /@ middle,
  {Spinor["Helicity", If[EvenQ[Length[middle]], kind,
    If[kind === "Angle", "Square", "Angle"]], right]}];
spFactorFixtures = Join[Flatten[Table[{
    spFactorChain[kind, {4}, 5], spFactorChain[kind, {4, 5}, 6],
    spFactorChain[kind, {6, 4}, 6]}, {kind, {"Angle", "Square"}}]],
  {SpinorChain[Spinor["Xi", "Angle", 7], Mom[4],
    Spinor["Helicity", "Square", 5]], SpinorTrace[Mom[3], Mom[4]]}];
spFactoredFixtures = ConstructiveDiagrams`Private`poleExternalMasslessFactor[
    #, spFactorMetadata] & /@ spFactorFixtures;
check["external massless denominator factors obey exact ordinary spinor identities",
  AllTrue[MapThread[ReduceSpinorProducts[ReduceSpinContractions[#1] - #2] /.
      spFactorMetadata["ExternalMassRules"] &, {spFactoredFixtures, spFactorFixtures}],
    # === 0 &]];
check["both starting chiralities and coinciding endpoint labels factor into simple brackets",
  AllTrue[Take[spFactoredFixtures, 6], FreeQ[#, chain_SpinorChain /; Length[chain] > 2] &] &&
    Last[spFactoredFixtures] === 2 MomProd[3, 4]];
spOneMomentum = First[spFactorFixtures];
spTwoMomenta = spFactorFixtures[[2]];
spFactorGuards = {
  spFactorChain["Angle", {1}, 5], spFactorChain["Angle", {7}, 5],
  SpinorChain[Spinor["Helicity", "Angle", 3], Mom[Multiparticle[4, 5]],
    Spinor["Helicity", "Square", 6]],
  SpinorChain[Spinor["Helicity", "Angle", 3], Mom[4] + Mom[5],
    Spinor["Helicity", "Square", 6]],
  SpinorChain[Spinor["Helicity", "Angle", 3], Mom[4],
    Spinor["Helicity", "Angle", 5]],
  SpinorChain[Spinor["UnknownKind", "Angle", 3], Mom[4],
    Spinor["Helicity", "Square", 5]],
  SpinorTrace[Mom[1], Mom[4]], SpinorTrace[Mom[3], Mom[4], Mom[5], Mom[6]]};
check["massive missing composite and invalid-chirality factors stay unchanged",
  (ConstructiveDiagrams`Private`poleExternalMasslessFactor[#, spFactorMetadata] & /@
    spFactorGuards) === spFactorGuards];
check["missing external metadata cannot authorize momentum factorization",
  ConstructiveDiagrams`Private`poleExternalMasslessFactor[
    spOneMomentum, Missing["ExternalMassMetadata"]] === spOneMomentum];
check["internal mass replacements never authorize an external momentum factorization",
  ConstructiveDiagrams`Private`poleExternalMasslessFactor[
    spFactorChain["Angle", {7}, 5], Join[spFactorMetadata, <|"ExternalMassRules" ->
      Append[spFactorMetadata["ExternalMassRules"], Mass[Multiparticle[3, 4]] -> 0]|>]] ===
    spFactorChain["Angle", {7}, 5]];
spFactorRecords = {<|"ExternalMassRules" -> spFactorMetadata["ExternalMassRules"]|>};
spChainInventory = ConstructiveDiagrams`Private`poleDiscoverDenominators[
  1/spTwoMomenta^3, {}, spFactorRecords];
check["two massless momentum insertions expose all three bracket zero sets and powers",
  Length[spChainInventory] === 3 &&
    Lookup[spChainInventory, "Classification"] === ConstantArray["Spurious", 3] &&
    Lookup[spChainInventory, "Multiplicity"] === {3, 3, 3}];
Do[
  spBranchInventory = ConstructiveDiagrams`Private`poleDiscoverDenominators[
    spOneMomentum^-spBranchPower, {}, spFactorRecords];
  check["branch-sensitive massless chains remain unresolved " <> ToString[spBranchPower],
    Length[spBranchInventory] === 1 &&
      First[spBranchInventory]["Factor"] === spOneMomentum &&
      First[spBranchInventory]["Classification"] === "Unclassified" &&
      First[spBranchInventory]["Diagnostics"] =!= {}],
  {spBranchPower, {1/2, spSymbolicPolePower}}];
Do[
  spZeroFactorInventory = ConstructiveDiagrams`Private`poleDiscoverDenominators[
    1/spZeroFactor, {}, spFactorRecords];
  check["identically zero massless denominators are retained " <> ToString[spZeroFactor, InputForm],
    Length[spZeroFactorInventory] === 1 &&
      First[spZeroFactorInventory]["Classification"] === "Unclassified" &&
      AnyTrue[First[spZeroFactorInventory]["Diagnostics"],
        StringContainsQ[#, "vanishes identically"] &]],
  {spZeroFactor, {spFactorChain["Angle", {3}, 4],
    spFactorChain["Angle", {4, 4}, 5], SpinorTrace[Mom[3], Mom[3]]}}];
spAutoFactoredChain = spAutoNoPhysical[1/spOneMomentum];
check["automatic finiteness checks test every massless-chain bracket factor on the full amplitude",
  spAutoFactoredChain["SpuriousSummary"] === <|"FAIL" -> 2|> &&
    AllTrue[spAutoFactoredChain["SpuriousChecks"], #["PoleOrder"] === 1 &]];
spAutoCancelledChain = spAutoNoPhysical[
  (spOneMomentum + 7)/spOneMomentum - 7/spOneMomentum];
check["massless-chain apparent poles cancel on the full sum without rewriting its amplitude",
  spAutoCancelledChain["SpuriousSummary"] === <|"PASS" -> 2|>];
spAutoTrace = spAutoNoPhysical[1/SpinorTrace[Mom[3], Mom[4]]^2];
check["two-momentum traces use the scalar invariant checker and both complex branches",
  spAutoTrace["SpuriousSummary"] === <|"FAIL" -> 1|> &&
    First[spAutoTrace["SpuriousChecks"]]["PoleOrder"] === 2 &&
    First[spAutoTrace["SpuriousChecks"]]["CompletedSamples"] >= 2];

spMasslessPhysicalRow = ReplacePart[First[prChannels], {
  -3 -> Multiparticle[3, 4], -2 -> 0, -1 -> 0}];
spPhysicalBracket = spAutoDetails[1/spAngle34, {spMasslessPhysicalRow}];
check["a bracket representing a supplied massless channel is physical, not spurious",
  spPhysicalBracket["SpuriousChecks"] === {} &&
    AnyTrue[spPhysicalBracket["PoleDiscovery"], #["Classification"] === "Physical" &&
      !FreeQ[#["Factor"], _SpinorChain] &]];
spExpandedPhysical = spAutoDetails[1/(2 MomProd[1, 3])];
check["an expanded scalar denominator matches its supplied physical propagator",
  spExpandedPhysical["SpuriousChecks"] === {} &&
    AnyTrue[spExpandedPhysical["PoleDiscovery"], #["Classification"] === "Physical" &&
      !FreeQ[#["Factor"], _MomProd] &]];
spD34 = PropDen[Mom[Multiparticle[3, 4]], 0];
spUnknownPropagator = spAutoDetails[1/spD34^2];
check["a candidate propagator absent from the supplied channels is spurious",
  spUnknownPropagator["SpuriousSummary"] === <|"FAIL" -> 1|> &&
    !AnyTrue[spUnknownPropagator["Channels"], #["Cut"] === {3, 4} &] &&
    First[spUnknownPropagator["SpuriousChecks"]]["PoleOrder"] === 2];

(* An invariant pole is sampled on both complex massless branches. *)
spInvariant = First[spReport[1/spD34^2, {spD34}]];
check["an unphysical scalar propagator has the expected Laurent order",
  spInvariant["Status"] === "FAIL" && spInvariant["PoleOrder"] === 2];
check["an apparent invariant pole cancels in the full expression",
  First[spReport[(spD34 + 7)/spD34 - 7/spD34, {spD34}]]["Status"] === "PASS"];
Do[
  spBranchReport = First[spReport[1/spBranch, {spD34}]];
  check["invariant sampling catches either chiral branch " <> ToString[spBranch, InputForm],
    spBranchReport["Status"] === "FAIL" &&
      Sort[Lookup[spBranchReport["Samples"], "Status"]] === {"FAIL", "PASS"}],
  {spBranch, {spAngle34, spSquare34}}];
spPromotedBranch = First[ConstructiveDiagrams`Private`spuriousCheckReport[
  1/spAngle34, {spD34}, prTargets, "Samples" -> 1,
  "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1}, "TimeConstraint" -> 20]];
check["one requested invariant sample is promoted to cover both complex branches",
  spPromotedBranch["Status"] === "FAIL" &&
    spPromotedBranch["UserRequestedSamples"] === 1 &&
    spPromotedBranch["CompletedSamples"] >= 2 &&
    Sort[Lookup[spPromotedBranch["Samples"], "Status"]] === {"FAIL", "PASS"}];
spWrongMass = PropDen[Mom[1] + Mom[2], 7];
check["a wrong-mass propagator cannot inherit a physical classification",
  With[{row = First[spReport[1/spWrongMass, {spWrongMass}]]},
    row["Status"] === "FAIL" && row["PoleOrder"] === 1]];
spPhysicalPropagator = First[prTargets]["Propagator"];
check["an explicitly requested physical propagator is not tested as a generic spurious pole",
  First[spReport[1/spPhysicalPropagator, {spPhysicalPropagator}]]["Status"] === "UNRESOLVED"];
spMetadataOnly = Join[First[prTargets], <|"KinematicMetadataOnly" -> True|>];
spMetadataResult = First[ConstructiveDiagrams`Private`spuriousCheckReport[
  1/spD34, {spD34}, {spMetadataOnly}, "Samples" -> 2,
  "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1}, "TimeConstraint" -> 20]];
check["a metadata-only chart descriptor does not invent a physical pole guard",
  spMetadataResult["Status"] === "FAIL"];

(* Compound factors use conserved rational curves and exact polynomial gcds.
   Every root is checked at once, without solving for one selected branch. *)
spScalarCompound = 1 + MomProd[1, 3] MomProd[1, 4];
spCompoundRows = ConstructiveDiagrams`Private`spuriousCheckReport[
  1/spMixedChain^2 + 1/spScalarCompound^3,
  {spMixedChain, spScalarCompound}, prTargets, "Samples" -> 2,
  "RandomSeed" -> 887113, "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1},
  "TimeConstraint" -> 20];
check["uncancelled mixed and scalar compound poles fail with their actual orders",
  Lookup[spCompoundRows, "Status"] === {"FAIL", "FAIL"} &&
    Lookup[spCompoundRows, "PoleOrder"] === {2, 3}];
check["compound certificates check every polynomial root away from physical poles",
  AllTrue[spCompoundRows, AllTrue[#["Samples"],
    TrueQ[#["AllPolynomialRootsChecked"]] &&
      TrueQ[#["PhysicalPropagatorsNonzeroAtPoleRoots"]] &] &]];
spCompoundCancelled = ConstructiveDiagrams`Private`spuriousCheckReport[
  (spMixedChain + 7)/spMixedChain - 7/spMixedChain +
    (spScalarCompound + 11)/spScalarCompound - 11/spScalarCompound,
  {spMixedChain, spScalarCompound}, prTargets, "Samples" -> 2,
  "RandomSeed" -> 887117, "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1},
  "TimeConstraint" -> 20];
check["compound apparent poles cancel only after checking the complete sum",
  Lookup[spCompoundCancelled, "Status"] === {"PASS", "PASS"} &&
    Lookup[spCompoundCancelled, "CompletedSamples"] === {2, 2}];
spCompoundChart = First[First[spCompoundCancelled]["Samples"]]["Chart"];
check["compound sample curves conserve momentum and every external mass shell identically",
  ConstructiveDiagrams`Private`residueExactZero[Total[Values[spCompoundChart["Momenta"]]]] &&
    AllTrue[Keys[spCompoundChart["Momenta"]],
      ConstructiveDiagrams`Private`residueExactZero[
        Det[spCompoundChart["Momenta"][#]] - spCompoundChart["Masses"][#]^2] &]];
check["the massless spinor mixing is exactly nilpotent and never activates internal cut spinors",
  spCompoundChart["SpinorMixingU"].spCompoundChart["SpinorMixingV"] === 0 &&
    spCompoundChart["AllowCutSpinors"] === False &&
    AllTrue[spCompoundChart["SpinorMixingLegs"],
      ConstructiveDiagrams`Private`residueExactZero[
        Outer[Times, spCompoundChart["Angle"][#], spCompoundChart["Square"][#]] -
          spCompoundChart["Momenta"][#]] &]];
check["compound curves vary the massive-pair invariant and have no coordinate denominator boundaries",
  Exponent[ConstructiveDiagrams`Private`residueEvaluateChart[MomProd[1, 2],
      spCompoundChart, {prElectronMass -> 1}], spCompoundChart["PoleVariable"]] > 0 &&
    AllTrue[Flatten[Join[Values[spCompoundChart["Momenta"]], Values[spCompoundChart["Angle"]],
      Values[spCompoundChart["Square"]]]], PolynomialQ[#, spCompoundChart["PoleVariable"]] &]];
spHiddenMassiveFactor = SpinorChain[Spinor["Helicity", "Angle", 3], Mom[1], Mom[2],
    Spinor["Helicity", "Angle", 4]] +
  SpinorChain[Spinor["Helicity", "Angle", 3], Mom[2], Mom[1],
    Spinor["Helicity", "Angle", 4]] - 7 spAngle34;
spHiddenMetadata = Join[First[prTargets], <|"Cut" -> {1, 2}, "Complement" -> {3, 4, 5},
  "Mass" -> 0, "KinematicMetadataOnly" -> True|>];
spHiddenMassiveCheck = First[ConstructiveDiagrams`Private`spuriousCompoundCheck[
  spAngle34/spHiddenMassiveFactor, {spHiddenMassiveFactor}, {spHiddenMetadata}, 20, 2, 887117,
  {prElectronMass -> 1, prElectricCharge -> 1}, Automatic]["Rows"]];
check["a Clifford-hidden massive invariant pole cannot pass through a frozen massless curve",
  spHiddenMassiveCheck["Status"] === "FAIL" && spHiddenMassiveCheck["PoleOrder"] === 1];
check["inhomogeneous little-group factors remain unsupported rather than receiving a partial certificate",
  !ConstructiveDiagrams`Private`poleRationalCurveFactorQ[
    spMixedChain + 1, First[prTargets]]];
spThreeMassiveMetadata = <|"ExternalLegs" -> Range[7],
  "ExternalMassRules" -> Thread[(Mass /@ Range[7]) ->
    {prElectronMass, prElectronMass, prElectronMass, 0, 0, 0, 0}]|>;
check["compound curves decline more than two massive legs conservatively",
  !ConstructiveDiagrams`Private`poleRationalCurveFactorQ[
    SpinorChain[Spinor["Helicity", "Angle", 4], Mom[1], Spinor["Helicity", "Square", 5]],
    spThreeMassiveMetadata]];
check["compound curves decline only two massless legs without changing ordinary bracket charts",
  !ConstructiveDiagrams`Private`poleRationalCurveFactorQ[spMixedChain,
    <|"ExternalLegs" -> Range[4], "ExternalMassRules" -> Thread[(Mass /@ Range[4]) ->
      {prElectronMass, prElectronMass, 0, 0}]|>]];
spBothBranches = ConstructiveDiagrams`Private`spuriousCompoundCheck[
  1/spAngle34, {spAngle34 spSquare34}, prTargets, 20, 1, 887123,
  {prElectronMass -> 1, prElectricCharge -> 1}, Automatic]["Rows"];
check["a compound product cannot pass by checking only its harmless chiral component",
  First[spBothBranches]["Status"] === "FAIL" &&
    First[spBothBranches]["PoleOrder"] === 1];
spCoincidentCompound = ConstructiveDiagrams`Private`spuriousCompoundCheck[
  0, {spPhysicalPropagator}, prTargets, 20, 1, 887129,
  {prElectronMass -> 1, prElectricCharge -> 1}, Automatic]["Rows"];
check["compound checking cannot certify a supplied physical pole as spurious",
  First[spCoincidentCompound]["Status"] === "UNRESOLVED" &&
    AnyTrue[First[spCoincidentCompound]["Diagnostics"], StringContainsQ[#, "physical pole"] &]];
Do[
  spConstantCompound = ConstructiveDiagrams`Private`spuriousCompoundCheck[
    0, {constant}, prTargets, 20, 1, 887131,
    {prElectronMass -> 1, prElectricCharge -> 1}, Automatic]["Rows"];
  check["a constant or zero curve polynomial cannot fabricate a pole pass " <> ToString[constant],
    First[spConstantCompound]["Status"] === "UNRESOLVED" &&
      First[spConstantCompound]["CompletedSamples"] === 0], {constant, {0, 1}}];
check["unknown and inexact polynomial coefficients do not receive exact gcd certificates",
  MissingQ[ConstructiveDiagrams`Private`spuriousCurveRationalData[spUnknownCoefficient z, z]] &&
    MissingQ[ConstructiveDiagrams`Private`spuriousCurveRationalData[0.1 z, z]]];
spMixedOrdering = ConstructiveDiagrams`Private`spuriousCheckReport[0,
  {spMixedChain, spAngle34, spScalarCompound, spUnsupportedChain}, prTargets,
  "Samples" -> 1, "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1},
  "TimeConstraint" -> 20];
check["grouped compound jobs preserve the supplied pole order and unresolved rows",
  Lookup[spMixedOrdering, "Pole"] === {spMixedChain, spAngle34, spScalarCompound, spUnsupportedChain} &&
    Lookup[spMixedOrdering, "Status"] === {"PASS", "PASS", "PASS", "UNRESOLVED"}];

spBalancedModel = ReadModel[FileNameJoin[{root, "models"}], 3];
spBalancedExample = qedBalancedSixPointAmplitude[spBalancedModel,
  "ReturnDetails" -> True, "ProgressReporting" -> False];
spBalancedChannels = FactorizationAmplitudeChannels[spBalancedExample["Particles"], spBalancedModel];
spBalancedNormalized = ConstructiveDiagrams`Private`residueSimpleInputs[
  {spBalancedExample["ExternalMassRules"], spBalancedExample["Amplitude"]},
  spBalancedChannels, spBalancedModel];
spBalancedInventory = ConstructiveDiagrams`Private`poleDiscoverDenominators[
  spBalancedNormalized["Expression"], spBalancedNormalized["Targets"], spBalancedNormalized["Records"]];
spBalancedCompoundFactors = Lookup[Select[spBalancedInventory,
  ConstructiveDiagrams`Private`poleRationalCurveFactorQ[#["CanonicalFactor"],
    First[spBalancedNormalized["Targets"]]] &], "CanonicalFactor"];
check["the actual balanced six-point expression has no unclassified denominator factors",
  Counts[Lookup[spBalancedInventory, "Classification"]] === <|"Physical" -> 14, "Spurious" -> 30|> &&
    Length[spBalancedCompoundFactors] === 28];
spBalancedCompoundBefore = spBalancedNormalized["Expression"];
{spBalancedCompoundSeconds, spBalancedCompoundChecks} = AbsoluteTiming[
  ConstructiveDiagrams`Private`spuriousCheckReport[spBalancedNormalized["Expression"],
    spBalancedCompoundFactors, spBalancedNormalized["Targets"], "Samples" -> 2,
    "RandomSeed" -> 887147, "ParameterRules" -> ModelParameterRules[spBalancedModel],
    "TimeConstraint" -> 30]];
Print["Balanced six-point compound-pole checks: ", spBalancedCompoundSeconds, " s."];
check["all twenty-eight actual compound denominators cancel on two fresh conserved curves",
  Length[spBalancedCompoundChecks] === 28 &&
    AllTrue[spBalancedCompoundChecks, #["Status"] === "PASS" && #["CompletedSamples"] === 2 &]];
check["the actual full amplitude stays unchanged and every compound root is certified",
  spBalancedNormalized["Expression"] === spBalancedCompoundBefore &&
    AllTrue[spBalancedCompoundChecks, AllTrue[#["Samples"],
      TrueQ[#["AllPolynomialRootsChecked"]] &&
        TrueQ[#["PhysicalPropagatorsNonzeroAtPoleRoots"]] &] &]];
spBalancedNegativeChart = First[First[spBalancedCompoundChecks]["Samples"]]["Chart"];
spBalancedNegativeVariable = spBalancedNegativeChart["PoleVariable"];
spBalancedNegativeRules = ModelParameterRules[spBalancedModel];
spBalancedCurveData = ConstructiveDiagrams`Private`spuriousCurveRationalData[
  ConstructiveDiagrams`Private`spuriousEvaluateCurve[spBalancedCompoundBefore,
    spBalancedNegativeChart, spBalancedNegativeRules], spBalancedNegativeVariable];
spBalancedPhysicalData = ConstructiveDiagrams`Private`spuriousCurveRationalData[
  ConstructiveDiagrams`Private`residueEvaluateChart[#["Propagator"],
    spBalancedNegativeChart, spBalancedNegativeRules], spBalancedNegativeVariable] & /@
      spBalancedNormalized["Targets"];
spBalancedInjectedPoles = Function[factor, Module[{mappedFactor, modified, certificate},
  mappedFactor = ConstructiveDiagrams`Private`residueEvaluateChart[
    factor, spBalancedNegativeChart, spBalancedNegativeRules];
  modified = ConstructiveDiagrams`Private`spuriousCurveRationalData[
    spBalancedCurveData["Numerator"]/spBalancedCurveData["Denominator"] + 1/mappedFactor^2,
    spBalancedNegativeVariable];
  certificate = ConstructiveDiagrams`Private`spuriousCurveCertificate[factor, modified,
    spBalancedPhysicalData, spBalancedNegativeChart, spBalancedNegativeRules];
  AssociationQ[certificate] && certificate["Status"] === "FAIL" && certificate["PoleOrder"] >= 2
]] /@ spBalancedCompoundFactors;
check["injecting an uncancelled double pole at each actual compound factor fails every certificate",
  spBalancedInjectedPoles === ConstantArray[True, 28]];
check["compound sampling restores original physical masses after choosing dimensionless units",
  spBalancedNegativeChart["KinematicScale"] === Abs[ModelParameter[spBalancedModel, "Me"] /. spBalancedNegativeRules] &&
    spBalancedNegativeChart["NumericalParametersUnchanged"] === True &&
    spBalancedNegativeChart["Masses"][1] === (ModelParameter[spBalancedModel, "Me"] /. spBalancedNegativeRules) &&
    spBalancedNegativeChart["DimensionlessExternalMasses"][1] === 1 &&
    ModelParameterRules[spBalancedModel] === spBalancedNegativeRules];
check["scaled massive angle and square bases retain their physical mass determinants",
  AllTrue[Keys[spBalancedNegativeChart["AngleBasis"]],
    ConstructiveDiagrams`Private`residueExactZero[
      Det[spBalancedNegativeChart["AngleBasis"][#]] - spBalancedNegativeChart["Masses"][#]] &&
      ConstructiveDiagrams`Private`residueExactZero[
        Det[spBalancedNegativeChart["SquareBasis"][#]] - spBalancedNegativeChart["Masses"][#]] &]];
spScaledCoherentChart = ConstructiveDiagrams`Private`completionHECoherentChart[spBalancedNegativeChart];
check["mass-normalized curves preserve the full coherent massive Dirac equation",
  AllTrue[{1, 2}, Function[leg, ConstructiveDiagrams`Private`residueExactZero[
    ConstructiveDiagrams`Private`residueEvaluateChart[
      SpinorChain[Spinor["Spin", "Angle", leg], Mom[leg], Spinor["Helicity", "Square", 3]] -
        spScaledCoherentChart["Masses"][leg] SpinorChain[Spinor["Spin", "Square", leg],
          Spinor["Helicity", "Square", 3]], spScaledCoherentChart, spBalancedNegativeRules]]]]];
spPrimitiveCacheTrial = spCacheCoefficient prSpinAngle[1, 2]/spMixedChain^2 +
  spCacheCoefficient (MomProd[1, 3] + 2 MomProd[1, 4])/spScalarCompound;
check["call-local primitive reduction preserves exact curve values and every coherent spin component",
  ConstructiveDiagrams`Private`residueExactZero[
    ConstructiveDiagrams`Private`spuriousEvaluateCurve[
      spPrimitiveCacheTrial, spScaledCoherentChart, spBalancedNegativeRules] -
    ConstructiveDiagrams`Private`residueEvaluateChart[
      spPrimitiveCacheTrial, spScaledCoherentChart, spBalancedNegativeRules]]];
{spSharedBudgetSeconds, spSharedBudget} = AbsoluteTiming[Block[
  {ConstructiveDiagrams`Private`spuriousEvaluateCurve = Function[{expression, chart, rules},
    Pause[0.025]; Missing["DeliberatelySlowCurve"]]},
  ConstructiveDiagrams`Private`spuriousCompoundCheck[0, ConstantArray[spMixedChain, 100],
    prTargets, 0.02, 100, 887191, {prElectronMass -> 1, prElectricCharge -> 1}, Automatic]]];
check["compound groups share one clock instead of multiplying it by their pole count",
  spSharedBudgetSeconds < 1 && Length[spSharedBudget["Rows"]] === 100 &&
    AllTrue[spSharedBudget["Rows"], #["Status"] === "UNRESOLVED" &&
      #["TimeConstraint"] === 0.02 && #["TimeConstraintScope"] === "SharedCompoundPoleGroup" &]];

spAffineUnknowns = {spRegularCoefficient, spDoubleCoefficient, spSimpleCoefficient, spSpinCoefficient};
spAffineTrial = spRegularCoefficient + spDoubleCoefficient prSpinAngle[1, 2]/spMixedChain^2 +
  spSimpleCoefficient/spMixedChain + spSpinCoefficient spSquare34/spMixedChain;
spAffineReport = ConstructiveDiagrams`Private`spuriousCompoundConstraints[
  spAffineTrial, spAffineUnknowns, {spMixedChain}, prTargets, First[prTargets],
  2, 887201, {prElectronMass -> 1, prElectricCharge -> 1}, 20];
check["compound completion generates exact affine equations without fitting a reference amplitude",
  spAffineReport["Status"] === "COMPLETE" && Length[spAffineReport["Samples"]] === 2 &&
    spAffineReport["Equations"] =!= {} &&
    AllTrue[spAffineReport["Equations"] /. Thread[Rest[spAffineUnknowns] -> 0],
      ConstructiveDiagrams`Private`residueExactZero]];
check["regular affine directions are retained rather than invented or silently fixed",
  FreeQ[spAffineReport["Equations"], spRegularCoefficient]];
check["every pole-producing affine direction is constrained in all massive-spin components",
  AllTrue[Rest[spAffineUnknowns], Function[unknown, With[
    {assignment = Thread[spAffineUnknowns -> (Boole[# === unknown] & /@ spAffineUnknowns)]},
    AnyTrue[spAffineReport["Equations"] /. assignment,
      !ConstructiveDiagrams`Private`residueExactZero[#] &]]]] &&
    AllTrue[spAffineReport["Samples"], Length[#["Chart"]["SpinVariables"]] === 2 &]];
check["affine compound constraints eliminate curve and coherent-spin variables coefficientwise",
  AllTrue[spAffineReport["Samples"], Function[sample,
    AllTrue[Join[{sample["Chart"]["PoleVariable"]}, sample["Chart"]["SpinVariables"]],
      FreeQ[spAffineReport["Equations"], #] &]]]];
check["compound remainders retain the full double-pole denominator multiplicity",
  AllTrue[spAffineReport["Samples"], Function[sample,
    With[{row = First[sample["CompoundPoles"]], variable = sample["Chart"]["PoleVariable"]},
      row["AllPolynomialRootsConstrained"] === True &&
        row["PhysicalPropagatorsNonzeroAtPoleRoots"] === True &&
        Exponent[row["SupportedDenominator"], variable] === 2 Exponent[row["PolePolynomial"], variable]]]]];
Do[
  spBadAffineReport = ConstructiveDiagrams`Private`spuriousCompoundConstraints[
    bad, {spSimpleCoefficient}, {spMixedChain}, prTargets, First[prTargets],
    1, 887211, {prElectronMass -> 1, prElectricCharge -> 1}, 20];
  check["non-affine compound fitting stays unresolved " <> ToString[bad, InputForm],
    spBadAffineReport["Status"] === "UNRESOLVED"],
  {bad, {1/(spMixedChain + spSimpleCoefficient), spSimpleCoefficient^2/spMixedChain}}];
spAffineTimed = ConstructiveDiagrams`Private`spuriousCompoundConstraints[
  spAffineTrial, spAffineUnknowns, {spMixedChain}, prTargets, First[prTargets],
  2, 887213, {prElectronMass -> 1, prElectricCharge -> 1}, 0.000001];
check["compound coefficient generation reports timeouts without accepting a partial fit",
  spAffineTimed["Status"] === "UNRESOLVED"];
spIntegratedCompound = ConstructiveDiagrams`Private`completionHESpuriousConstraints[
  spAffineTrial, spAffineUnknowns, <|"Targets" -> prTargets, "Records" -> {}|>,
  First[prTargets], 2, 887201, {prElectronMass -> 1, prElectricCharge -> 1},
  Automatic, False, 20];
check["the completion dispatcher groups compound poles into exact coefficient constraints",
  spIntegratedCompound["Status"] === "COMPLETE" &&
    spIntegratedCompound["Equations"] === spAffineReport["Equations"] &&
    AllTrue[spIntegratedCompound["Samples"],
      #["Method"] === "ExactConservedCurvePolynomialRemainder" &]];
spIntegratedCompoundFit = ConstructiveDiagrams`Private`completionSolveLinear[
  spIntegratedCompound["Equations"], spAffineUnknowns];
check["the integrated exact solve removes all pole directions and retains regular freedom",
  spIntegratedCompoundFit["Status"] === "UNDERDETERMINED" &&
    spIntegratedCompoundFit["Rank"] === 3 &&
    spIntegratedCompoundFit["FreeParameters"] === {spRegularCoefficient} &&
    (Rest[spAffineUnknowns] /. spIntegratedCompoundFit["SolutionRules"]) === {0, 0, 0}];

Print[checks, " spurious-pole checks; ", Length[failures], " failures."];
If[failures =!= {}, Print[failures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:spurious-pole-tests.wl"];
Quit[0];
