(* Exact transverse finiteness checks on the full amplitude. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
Get[FileNameJoin[{root, "tests", "QED-5-point-residues.wl"}]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name];
  Print["FAIL: ", name]]);
spReport[expression_, factors_, opts___] :=
  ConstructiveDiagrams`Private`spuriousCheckReport[expression, factors, prTargets,
    "Samples" -> 2, "ParameterRules" -> {Me -> 1, EE -> 1},
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
check["massive angle pairs are not silently treated as massless collinear poles",
  First[spReport[1/prSpinAngle[1, 2], {prSpinAngle[1, 2]}]]["Status"] === "UNRESOLVED"];
check["supplied physical maps are not silently reused as spurious charts",
  First[spReport[0, {spAngle34}, "KinematicMap" -> Function[{x, c, v}, x]]]["Status"] === "UNRESOLVED"];
spPhysical = Join[First[prTargets], <|"Cut" -> {3, 4}, "Complement" -> {1, 2, 5},
  "Mass" -> 0, "Propagator" -> PropDen[Mom[Multiparticle[3, 4]], 0]|>];
spCoincident = First[ConstructiveDiagrams`Private`spuriousCheckReport[
  1/spAngle34, {spAngle34}, Append[prTargets, spPhysical], "Samples" -> 1,
  "ParameterRules" -> {Me -> 1, EE -> 1}, "TimeConstraint" -> 20]];
check["coincident physical poles are not mistaken for spurious failures",
  spCoincident["Status"] === "UNRESOLVED" &&
    AnyTrue[spCoincident["Diagnostics"], StringContainsQ[#, "physical pole"] &]];
spTimed = First[ConstructiveDiagrams`Private`spuriousCheckReport[
  prS2, {spAngle34}, prTargets, "Samples" -> 100,
  "ParameterRules" -> {Me -> 1, EE -> 1}, "TimeConstraint" -> 0.00001]];
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
    prOldPermutation, prTargets, spChart, {Me -> 1, EE -> 1}]];
spPublicDefault = CheckPhysicalResidues[prH, prChannels, prFixtureModel,
  "Samples" -> 1, "ParameterRules" -> {Me -> 1, EE -> 1}];
check["default public table has one compact schema even without spurious poles",
  Head[spPublicDefault] === Dataset && AllTrue[Normal[spPublicDefault],
    Sort[Keys[#]] === Sort[{"Type", "Pole", "Status"}] && #["Type"] === "Physical" &]];
spPublic = CheckPhysicalResidues[prS2, prChannels, prFixtureModel,
  "Samples" -> 1, "ParameterRules" -> {Me -> 1, EE -> 1},
  "SpuriousPoles" -> {spAngle34, spSquare34}, "Output" -> "Details"];
check["public details preserve physical and spurious rows separately",
  Length[spPublic["Channels"]] === 6 && Length[spPublic["SpuriousChecks"]] === 2 &&
    spPublic["PhysicalSummary"] === <|"PASS" -> 6|> &&
    spPublic["SpuriousSummary"] === <|"PASS" -> 2|> &&
    spPublic["Summary"] === <|"PASS" -> 8|>];
spPublicTable = CheckPhysicalResidues[prH, prChannels, prFixtureModel,
  "Samples" -> 1, "ParameterRules" -> {Me -> 1, EE -> 1},
  "SpuriousPoles" -> {spAngle34}];
check["combined public table is compact and labels each pole type",
  Head[spPublicTable] === Dataset && Length[Normal[spPublicTable]] === 7 &&
    AllTrue[Normal[spPublicTable], Sort[Keys[#]] === Sort[{"Type", "Pole", "Status"}] &] &&
    Counts[Lookup[Normal[spPublicTable], "Type"]] ===
      <|"Physical" -> 6, "Spurious" -> 1|>];
spPublicResidues = CheckPhysicalResidues[prH, prChannels, prFixtureModel,
  "Samples" -> 1, "ParameterRules" -> {Me -> 1, EE -> 1},
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
  "Samples" -> 1, "ParameterRules" -> {Me -> 1, EE -> 1},
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
  {Thread[(Mass /@ Range[5]) -> {Me, Me, 0, 0, 0}], 1/spAngle34^2},
  {}, prModel, "Samples" -> 1, "ParameterRules" -> {Me -> 1},
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
  "ParameterRules" -> {Me -> 1, EE -> 1}, "TimeConstraint" -> 20,
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

spExternalRules = Thread[(Mass /@ Range[5]) -> {Me, Me, 0, 0, 0}];
spAutoNoPhysical[expression_] := CheckPhysicalResidues[
  {spExternalRules, expression}, {}, prModel, "Samples" -> 2,
  "ParameterRules" -> {Me -> 1, EE -> 1}, "TimeConstraint" -> 20,
  "Output" -> "Details"];
spNested = spAutoNoPhysical[1/(Me^3 (spXi3 spXi4)^2)];
spNestedDiscovered = Select[spNested["PoleDiscovery"],
  #["Classification"] === "Spurious" &];
check["nested denominator products retain their multiplicities",
  Length[spNestedDiscovered] === 2 &&
    Lookup[spNestedDiscovered, "Multiplicity"] === {2, 2} &&
    spNested["SpuriousSummary"] === <|"FAIL" -> 2|> &&
    Lookup[spNested["SpuriousChecks"], "PoleOrder"] === {2, 2}];
spConstant = spAutoNoPhysical[1/Me^3];
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
spUnsupportedAuto = spAutoNoPhysical[1/spMixedChain];
check["unsupported momentum-chain denominator surfaces instead of disappearing",
  spUnsupportedAuto["SpuriousSummary"] === <|"UNRESOLVED" -> 1|> &&
    AnyTrue[spUnsupportedAuto["PoleDiscovery"],
      #["Classification"] === "Unclassified" && #["Diagnostics"] =!= {} &]];
spUnsupportedValues = CheckPhysicalResidues[{spExternalRules, 1/spMixedChain}, {}, prModel,
  "Samples" -> 1, "ParameterRules" -> {Me -> 1}, "Output" -> "Residues"];
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
  "ParameterRules" -> {Me -> 1, EE -> 1}, "TimeConstraint" -> 20]];
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
  "ParameterRules" -> {Me -> 1, EE -> 1}, "TimeConstraint" -> 20]];
check["a metadata-only chart descriptor does not invent a physical pole guard",
  spMetadataResult["Status"] === "FAIL"];

Print[checks, " spurious-pole checks; ", Length[failures], " failures."];
If[failures =!= {}, Print[failures]; Quit[1]];
Quit[0];
