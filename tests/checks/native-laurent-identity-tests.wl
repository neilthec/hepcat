(* Dense-chart, typed-normalization and optional exact native proof contracts. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0; checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);
niRules = {Mass[1] -> niMass, Mass[2] -> niMass, Mass[3] -> niMass, Mass[4] -> niMass, Mass[5] -> 0, Mass[6] -> 0};
niUnitRules = niRules /. niMass -> 1;
niChart = ConstructiveDiagrams`Private`completionHERestNullSymbolicChart[niUnitRules];
check["rest/null chart verifies every external shell and total momentum", AssociationQ[niChart] &&
  niChart["ExternalMassShellsValidated"] && niChart["MomentumConservationValidated"] &&
  AllTrue[Values[niChart["ExternalMassShellChecks"]], TrueQ]];
check["both null outer products and every massive basis are independently checked",
  AllTrue[Values[niChart["NullSpinorProductsValidated"]], TrueQ] &&
  AllTrue[Values[niChart["ExternalSpinorBasesValidated"]], TrueQ]];
check["chart has independent coordinates and coherent variables, without internal shells",
  Length[niChart["ExternalIdentityCoordinates"]] === 9 &&
  DuplicateFreeQ[Join[niChart["ExternalIdentityCoordinates"], niChart["SpinVariables"]]] &&
  Length[niChart["SpinVariables"]] === 4 && niChart["InternalOnShellConditions"] === {} &&
  niChart["NumericalKinematicSamples"] === 0 && niChart["ResidualGaugeDimension"] === 1];
check["only the two chosen null rows are dependent and their angle lines are fixed",
  niChart["DependentShellLegs"] === {5, 6} && niChart["Angle"][5] === {1, 0} && niChart["Angle"][6] === {0, 1}];
niRelabelRules = niUnitRules /. {Mass[1] -> Mass[2], Mass[2] -> Mass[4], Mass[3] -> Mass[7], Mass[4] -> Mass[9], Mass[5] -> Mass[10], Mass[6] -> Mass[12]};
niRelabelChart = ConstructiveDiagrams`Private`completionHERestNullSymbolicChart[niRelabelRules, {12, 10}];
check["chart selection uses labels and masses rather than a fixed process",
  AssociationQ[niRelabelChart] && niRelabelChart["RestFrameLeg"] === 2 && niRelabelChart["NullAxisLegs"] === {12, 10}];
niExtraNull = ConstructiveDiagrams`Private`completionHERestNullSymbolicChart[Append[niUnitRules, Mass[7] -> 0]];
check["additional massless momenta remain independent shell matrices", AssociationQ[niExtraNull] &&
  niExtraNull["ExternalMassShellsValidated"] && Length[niExtraNull["ExternalIdentityCoordinates"]] === 12 &&
  niExtraNull["NullSpinorProductsValidated"][7]];
check["all-massless inputs decline this optional chart",
  ConstructiveDiagrams`Private`completionHERestNullSymbolicChart[niUnitRules /. (Mass[i_] -> 1) :> (Mass[i] -> 0)] === $Failed];
check["one null leg and duplicate external metadata decline",
  ConstructiveDiagrams`Private`completionHERestNullSymbolicChart[niUnitRules /. (Mass[6] -> 0) -> (Mass[6] -> 1)] === $Failed &&
  ConstructiveDiagrams`Private`completionHERestNullSymbolicChart[Append[niUnitRules, Mass[1] -> 1]] === $Failed];
niA[i_] := Spinor["Spin", "Angle", i];
niBracket[i_, j_] := SpinorChain[niA[i], niA[j]];
niPhoton = SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Helicity", "Square", 6]];
niP15 = PropDen[Mom[Multiparticle[1, 5]], niMass];
niP26 = PropDen[Mom[Multiparticle[2, 6]], niMass];
niTensor = niBracket[1, 2] niBracket[3, 4] - niBracket[1, 3] niBracket[2, 4] + niBracket[1, 4] niBracket[2, 3];
niZero = niCoupling^4 niPhoton^2 niTensor/(niP15 niP26);
niNormalized = ConstructiveDiagrams`Private`completionHECommonMassNormalization[niZero, niRules];
check["typed common-mass dilation and literal coupling power are certified exactly",
  niNormalized["Status"] === "COMPLETE" && niNormalized["HomogeneousMassDimension"] === 0 &&
  niNormalized["ScalarFactorRules"] === {niCoupling -> 4} && niNormalized["ScalarFactor"] === niCoupling^4 &&
  niNormalized["CommonMassRequiredNonzero"]];
check["normalization retains all external spin weights and no internal shell assumptions",
  niNormalized["ExternalSpinWeights"] === {1, 1, 1, 1, -2, -2} && niNormalized["InternalOnShellConditions"] === {}];
check["unequal formal masses decline unit-mass lifting",
  ConstructiveDiagrams`Private`completionHECommonMassNormalization[niZero, niRules /. (Mass[3] -> niMass) -> (Mass[3] -> niOtherMass)]["Status"] === "UNSUPPORTED"];
check["an internal mass unrelated to the common external mass declines",
  ConstructiveDiagrams`Private`completionHECommonMassNormalization[niZero /. niP15 -> PropDen[Mom[Multiparticle[1, 5]], niOtherMass], niRules]["Status"] === "UNSUPPORTED"];
check["nonhomogeneous mass dependence cannot be erased by unit substitution",
  ConstructiveDiagrams`Private`completionHECommonMassNormalization[(1 + niMass) niZero, niRules]["Status"] === "UNSUPPORTED"];
check["a coupling with different powers is not set to one",
  ConstructiveDiagrams`Private`completionHECommonMassNormalization[(1 + niCoupling) niZero, niRules]["Status"] === "UNSUPPORTED"];
check["machine coefficients and nonlinear mass functions are unsupported",
  ConstructiveDiagrams`Private`completionHECommonMassNormalization[0.5 niZero, niRules]["Status"] === "UNSUPPORTED" &&
  ConstructiveDiagrams`Private`completionHECommonMassNormalization[Sin[niMass] niZero, niRules]["Status"] === "UNSUPPORTED"];
niClear = ConstructiveDiagrams`Private`completionHEClearTypedDenominators[niNormalized["Amplitude"]];
check["typed denominator clearing has an exact opaque rational reconstruction",
  niClear["Status"] === "COMPLETE" && niClear["ExactRationalReconstruction"] &&
  niClear["PrimitiveRestorationExact"] && Length[niClear["DenominatorFactors"]] === 2 &&
  niClear["DenominatorNonzeroProofRequired"]];
check["inverse brackets cannot enter as cleared physical propagators",
  ConstructiveDiagrams`Private`completionHEClearTypedDenominators[1/niPhoton]["Status"] === "UNSUPPORTED"];
check["a nonzero-width denominator declines the strict typed route",
  ConstructiveDiagrams`Private`completionHEClearTypedDenominators[1/PropDen[Mom[Multiparticle[1, 5]], 1, 1]]["Status"] === "UNSUPPORTED"];
niInhomogeneous = SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity", "Angle", 6]] - 1;
check["the normalized null angle bracket cannot hide inhomogeneous spin weights",
  ConstructiveDiagrams`Private`completionHEPrepareNativeIdentity[niInhomogeneous, niRules, 5]["Status"] === "NOT_PROVED"];
niMassiveHelicity = SpinorChain[Spinor["Helicity", "Angle", 1], niA[2]];
check["massive helicity tags cannot be reinterpreted as coherent massive spinors",
  ConstructiveDiagrams`Private`completionHEPrepareNativeIdentity[niMassiveHelicity, niRules, 5]["Status"] === "NOT_PROVED" &&
  ConstructiveDiagrams`Private`completionHEZeroDirection[niMassiveHelicity, niRules, {}, 5]["Status"] === "NOT_PROVED"];
check["massless tag validation rejects indexed and unresolved massive variants conservatively",
  !ConstructiveDiagrams`Private`completionHEMasslessHelicityTagsQ[Spinor["Helicity", "Upper", "Angle", 1], niRules] &&
  !ConstructiveDiagrams`Private`completionHEMasslessHelicityTagsQ[Spinor["Helicity", "Angle", 1, 1/2], niRules] &&
  !ConstructiveDiagrams`Private`completionHEMasslessHelicityTagsQ[Spinor["Helicity", "Angle", 99], niRules] &&
  ConstructiveDiagrams`Private`completionHEMasslessHelicityTagsQ[niPhoton, niRules]];
niFixedSpin = SpinorChain[Spinor["Spin", "Upper", "Angle", 1, 1/2], niA[2]];
check["fixed spin and auxiliary reference spinors are excluded",
  ConstructiveDiagrams`Private`completionHEPrepareNativeIdentity[niFixedSpin, niRules, 5]["Status"] === "NOT_PROVED" &&
  ConstructiveDiagrams`Private`completionHEPrepareNativeIdentity[SpinorChain[Spinor["Xi", "Angle", 0], niA[1]], niRules, 5]["Status"] === "NOT_PROVED"];
check["opposite-chirality untyped scalar cannot receive a frame certificate",
  ConstructiveDiagrams`Private`completionHEPrepareNativeIdentity[SpinorChain[niA[1], Spinor["Spin", "Square", 2]], niRules, 5]["Status"] === "NOT_PROVED"];
niDAG = ConstructiveDiagrams`Private`completionHELaurentDAG[{(niX + niY)^2 - niX^2 - 2 niX niY - niY^2, niX niY}, {niX, niY}, 5];
check["DAG preserves every node and ordered coefficient root exactly", niDAG["Status"] === "COMPLETE" &&
  niDAG["EveryNodeRoundTrip"] && niDAG["ExactStructuralRoundTrip"] && niDAG["RootOrderPreserved"] && niDAG["RootCount"] === 2];
check["unknown functions and real coefficients decline export",
  ConstructiveDiagrams`Private`completionHELaurentDAG[{Sin[niX]}, {niX}, 5]["Status"] === "UNSUPPORTED" &&
  ConstructiveDiagrams`Private`completionHELaurentDAG[{0.5 niX}, {niX}, 5]["Status"] === "UNSUPPORTED"];
check["export enforces integer coefficient bits and exponent caps",
  ConstructiveDiagrams`Private`completionHELaurentDAG[{2^4096 niX}, {niX}, 5]["Status"] === "UNSUPPORTED" &&
  ConstructiveDiagrams`Private`completionHELaurentDAG[{niX^1000001}, {niX}, 5]["Status"] === "UNSUPPORTED"];
check["duplicate variables and zero clock fail closed",
  ConstructiveDiagrams`Private`completionHELaurentDAG[{niX}, {niX, niX}, 5]["Status"] === "UNSUPPORTED" &&
  ConstructiveDiagrams`Private`completionHEPrepareNativeIdentity[niZero, niRules, 0]["Status"] === "NOT_PROVED"];
niPrepared = <|"Status" -> "PREPARED", "DAG" -> niDAG, "DenominatorRootIndices" -> {2}|>;
niRun = ConstructiveDiagrams`Private`completionHENativeLaurentRun[niPrepared, 30];
niOptionalReason = Lookup[Lookup[niRun, "NativeCertificate", <||>], "FailureReason", Lookup[niRun, "Method", ""]];
niOptionalAvailable = !(niRun["Status"] =!= "PROVED_ZERO" && MemberQ[
  {"CompilerUnavailable", "BoostHeadersUnavailable", "NativeRuntimeUnavailable", "NativePythonUnavailable", "NativeProcessUnavailable"}, niOptionalReason]);
If[niOptionalAvailable,
check["native result proves zero coefficients and nonzero denominator roots separately", niRun["Status"] === "PROVED_ZERO" &&
  niRun["NativeCertificate"]["RunStatus"] === "COMPLETE" && !niRun["NativeCertificate"]["AllRootsZero"] &&
  niRun["NativeCertificate"]["AllExpectedStatuses"]];
niNative = niRun["NativeCertificate"]; niArtifact = niRun["ArtifactDirectory"];
niInputPath = FileNameJoin[{niArtifact, "input.dag"}];
niHashes = ConstructiveDiagrams`Private`completionHENativeRuntimeSources[];
niValid[report_] := ConstructiveDiagrams`Private`completionHENativeRuntimeResultQ[report, niDAG, {2}, niHashes, niArtifact, niInputPath];
check["unmodified root order, hashes and exact factor witnesses validate", niValid[niNative]];
check["a truncated root list cannot receive a certificate", !niValid[Join[niNative, <|"Roots" -> Take[niNative["Roots"], 1]|>]]];
check["a changed original root mapping cannot receive a certificate", !niValid[Join[niNative, <|"OriginalRoots" -> Reverse[niNative["OriginalRoots"]]|>]]];
check["source and input hashes are mandatory", !niValid[Join[niNative, <|"InputSHA256" -> StringRepeat["0", 64]|>]] &&
  !niValid[Join[niNative, <|"SourceHashes" -> <||>|>]]];
check["source fingerprint and independent factor verification are mandatory", !niValid[Join[niNative, <|"SourceFingerprint" -> StringRepeat["0", 64]|>]] &&
  !niValid[Join[niNative, <|"ExactFactorVerification" -> <|"Status" -> "FAIL"|>|>]]];
niWrongRows = niNative["Roots"]; niWrongRows[[1, "Terms"]] = 1;
check["zero status with nonzero coefficient inventory is rejected", !niValid[Join[niNative, <|"Roots" -> niWrongRows|>]]];
check["partial traversal cannot masquerade as complete", !niValid[Join[niNative, <|"RunStatus" -> "NOT_PROVED"|>]]];
niTinyDAG = ConstructiveDiagrams`Private`completionHELaurentDAG[{1/10^40 niX + (niX + niY)^2 - niX^2 - 2 niX niY - niY^2}, {niX, niY}, 5];
niTinyRun = ConstructiveDiagrams`Private`completionHENativeLaurentRun[<|"Status" -> "PREPARED", "DAG" -> niTinyDAG, "DenominatorRootIndices" -> {}|>, 20];
check["a tiny genuine rational spin coefficient remains nonzero", niTinyRun["Status"] === "NOT_PROVED" &&
  niTinyRun["NativeCertificate"]["Roots"][[1, "Status"]] === "NONZERO"];
niInverseDAG = ConstructiveDiagrams`Private`completionHELaurentDAG[{1/(niX + niY)}, {niX, niY}, 5];
niInverseRun = ConstructiveDiagrams`Private`completionHENativeLaurentRun[<|"Status" -> "PREPARED", "DAG" -> niInverseDAG, "DenominatorRootIndices" -> {}|>, 20];
check["non-monomial inverses preserve an unsupported result", niInverseRun["Status"] === "NOT_PROVED"];
niZeroNative = ConstructiveDiagrams`Private`completionHENativeZeroDirection[niZero, niRules, {}, 30];
check["full generic helper proves an ordinary four-spin tensor identity", niZeroNative["Status"] === "PROVED_ZERO" &&
  niZeroNative["Method"] === "DenseRestNullNativeLaurentIdentity" &&
  niZeroNative["CompleteCoherentCoefficientCoverage"] && niZeroNative["DenominatorFactorsProvedNonzero"] &&
  niZeroNative["NumericalKinematicSamples"] === 0 && niZeroNative["InternalOnShellConditions"] === {}];
  , Print["SKIP: optional native compiler/runtime is unavailable: ", niOptionalReason]];
niEmptyDirectory = CreateDirectory[];
check["missing optional runtime preserves the direction", Block[{Global`$HEPCATpath = niEmptyDirectory},
  ConstructiveDiagrams`Private`completionHENativeLaurentRun[niPrepared, 10]["Status"] === "NOT_PROVED"]];
DeleteDirectory[niEmptyDirectory];
check["the native backend cannot launch inside a worker", Block[{$KernelID = 1},
  ConstructiveDiagrams`Private`completionHENativeLaurentRun[niPrepared, 10]["Method"] === "UnsupportedSerialNativeLaunch"]];
niLarge = Total[Array[niOpaqueDirection, 600]]; niParameter = Unique["nativeAxis"];
niBudgets = {};
niFamily = Block[{ConstructiveDiagrams`Private`$completionParallelSettings = <|"Parallelization" -> False, "MaxKernels" -> 1|>,
    ConstructiveDiagrams`Private`completionHEZeroDirection, ConstructiveDiagrams`Private`completionHENativeZeroDirection},
  ConstructiveDiagrams`Private`completionHEZeroDirection[___] := <|"Status" -> "NOT_PROVED", "Method" -> "TestUnavailable"|>;
  ConstructiveDiagrams`Private`completionHENativeZeroDirection[direction_, rules_, model_, seconds_] :=
    (AppendTo[niBudgets, seconds]; <|"Status" -> "PROVED_ZERO", "Method" -> "TestCertifiedSerialZero"|>);
  ConstructiveDiagrams`Private`completionHEPruneIdentityFamily[1 + niParameter niLarge, {niParameter}, niRules, {}, 2, {niLarge}]];
check["small shared deadlines never start the optional native fallback", niFamily["FreeParameters"] === {niParameter} && niBudgets === {}];
niFamily = Block[{ConstructiveDiagrams`Private`$completionParallelSettings = <|"Parallelization" -> False, "MaxKernels" -> 1|>,
    ConstructiveDiagrams`Private`completionHEZeroDirection, ConstructiveDiagrams`Private`completionHENativeZeroDirection},
  ConstructiveDiagrams`Private`completionHEZeroDirection[___] := <|"Status" -> "NOT_PROVED", "Method" -> "TestUnavailable"|>;
  ConstructiveDiagrams`Private`completionHENativeZeroDirection[direction_, rules_, model_, seconds_] :=
    (AppendTo[niBudgets, seconds]; <|"Status" -> "NOT_PROVED", "Method" -> "TestNativeTimeConstraint"|>);
  ConstructiveDiagrams`Private`completionHEPruneIdentityFamily[1 + niParameter niLarge, {niParameter}, niRules, {}, 5, {niLarge}]];
check["unproved late native work preserves the complete original axis", niFamily["FreeParameters"] === {niParameter} &&
  niFamily["RemainingBasis"] === {niLarge} && SameQ[niFamily["FamilyAmplitude"], 1 + niParameter niLarge] &&
  Length[niBudgets] === 1 && 0 < First[niBudgets] <= 5 && niFamily["NativeLaurentRetry"]["Execution"] === "SerialMasterOnly"];
niFixture = Get[FileNameJoin[{root, "tests", "checks", "fixtures", "high-energy-four-fermion-native-zero.wl"}]][niMass, niCoupling];
niFixturePrepared = ConstructiveDiagrams`Private`completionHEPrepareNativeIdentity[niFixture, niRules, 20];
check["the actual retained direction has complete typed, coherent and reversible preparation", niFixturePrepared["Status"] === "PREPARED" &&
  niFixturePrepared["PolynomialExtraction"]["FullCoefficientCoverage"] &&
  niFixturePrepared["PolynomialExtraction"]["ReconstructionVerified"] &&
  niFixturePrepared["DAG"]["EveryNodeRoundTrip"] &&
  Which[niFixturePrepared["Method"] === "DenseRestNullTypedLaurentIdentity",
    niFixturePrepared["CoefficientRootCount"] === 15 &&
      niFixturePrepared["DenominatorRootIndices"] === Range[16, 27] &&
      niFixturePrepared["MassNormalization"]["HomogeneousMassDimension"] === -2 &&
      niFixturePrepared["MassNormalization"]["ScalarFactorRules"] === {niCoupling -> 4},
    niFixturePrepared["Method"] === "DenseRestMixedNullTypedPolynomialIdentity",
    niFixturePrepared["CoefficientRootCount"] === 16 &&
      niFixturePrepared["MassNormalizationUsed"] === False &&
      niFixturePrepared["ScalarVariableCertificate"]["ExactScalarRoundTrip"] &&
      niFixturePrepared["ScalarSpecializationCertificate"]["EveryFactorNonzeroAfterNumericRestoration"],
    True, False]];
If[Environment["HEPCAT_TEST_NATIVE_FULL_DIRECTION"] === "1" && niOptionalAvailable,
  niFixtureProof = ConstructiveDiagrams`Private`completionHENativeZeroDirection[niFixture, niRules, {}, 120];
  check["actual difficult affine identity is proved with every coefficient and denominator root", niFixtureProof["Status"] === "PROVED_ZERO" &&
    niFixtureProof["NativeCertificate"]["RootCount"] === niFixturePrepared["DAG"]["RootCount"] &&
    niFixtureProof["NativeCertificate"]["RunStatus"] === "COMPLETE" &&
    niFixtureProof["NativeCertificate"]["AllExpectedStatuses"] && niFixtureProof["ElapsedTime"] <= 120],
  Print["SKIP: set HEPCAT_TEST_NATIVE_FULL_DIRECTION=1 for the actual bounded 90-second native proof."]];
(* The saved six-point failure contained ordinary identities that were hard
   in a same-chirality chart. Mixed axes must prove them without selecting a
   numerical phase-space or parameter point, and must preserve genuine terms. *)
niMixedFixture = Get[FileNameJoin[{root, "tests", "checks", "fixtures",
  "high-energy-six-point-mixed-null-zero.wl"}]][niMass, niCoupling];
niMixedRules = niMixedFixture["ExternalMassRules"];
niMixedChoice = ConstructiveDiagrams`Private`completionHEMixedNullAxisChoice[
  First[niMixedFixture["Directions"]], niMixedRules];
check["mixed axes are chosen from homogeneous spin weights rather than labels",
  niMixedChoice["Status"] === "AVAILABLE" && niMixedChoice["NullAxisLegs"] === {4, 3}];
niMixedRelabel = <|1 -> 2, 2 -> 7, 3 -> 4, 4 -> 9, 5 -> 11, 6 -> 13|>;
niMixedRelabeledRules = ConstructiveDiagrams`Private`relabelLegs[niMixedRules, niMixedRelabel];
niMixedCases = <|
  "ActualLargeDirection" -> {First[niMixedFixture["Directions"]], niMixedRules},
  "RelabeledNonhomogeneousScalarMultiple" -> {
    ConstructiveDiagrams`Private`relabelLegs[
      (1 + niMass + niCoupling^2) Last[niMixedFixture["Directions"]], niMixedRelabel],
    niMixedRelabeledRules}|>;
If[niOptionalAvailable,
  KeyValueMap[Function[{name, data},
    niMixedProof = ConstructiveDiagrams`Private`completionHENativeZeroDirection[
      First[data], Last[data], {}, 45];
    check[name <> " has a complete exact mixed-null identity certificate",
      niMixedProof["Status"] === "PROVED_ZERO" &&
      niMixedProof["Method"] === "DenseRestMixedNullNativePolynomialIdentity" &&
      TrueQ[niMixedProof["CompleteCoherentCoefficientCoverage"]] &&
      TrueQ[niMixedProof["DenominatorFactorsProvedNonzero"]] &&
      niMixedProof["NumericalKinematicSamples"] === 0 &&
      niMixedProof["CoherentSpinSamples"] === 0 &&
      niMixedProof["InternalOnShellConditions"] === {}];
    niMixedCertificate = Lookup[niMixedProof, "PreparationCertificate", <||>];
    check[name <> " retains complete scalar and coherent reconstruction",
      niMixedCertificate["MassNormalizationUsed"] === False &&
      niMixedCertificate["NumericalParameterSubstitutions"] === {} &&
      TrueQ[niMixedCertificate["ScalarVariableCertificate"]["ExactScalarRoundTrip"]] &&
      TrueQ[niMixedCertificate["PolynomialExtraction"]["FullCoefficientCoverage"]] &&
      TrueQ[niMixedCertificate["PolynomialExtraction"]["ReconstructionVerified"]] &&
      niMixedCertificate["CoefficientRootCount"] === 4 &&
      Length[niMixedCertificate["CoefficientMonomials"]] === 4];
    check[name <> " verifies every rational-clearing and denominator obligation",
      TrueQ[niMixedCertificate["RationalLiftCertificate"]["ReconstructionVerified"]] &&
      TrueQ[niMixedCertificate["ScalarSpecializationCertificate"]["EveryFactorNonzeroAfterNumericRestoration"]] &&
      niMixedCertificate["ScalarSpecializationCertificate"]["FactorCount"] ===
        Length[niMixedCertificate["DenominatorRootIndices"]] &&
      niMixedCertificate["ScalarSpecializationCertificate"]["WitnessCount"] ===
        Length[niMixedCertificate["DenominatorRootIndices"]]];
    check[name <> " preserves the original exact direction and bounded clock",
      niMixedProof["OriginalDirection"] === First[data] &&
      niMixedProof["NativeProofBudget"] === 45 && niMixedProof["ElapsedTime"] < 46]
  ], niMixedCases];
  niMixedDeformation = SpinorChain[niA[1], niA[2]] *
    SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
      Spinor["Helicity", "Angle", 4]]^2 *
    SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity", "Angle", 6]]^2/10^30;
  niMixedNonzero = ConstructiveDiagrams`Private`completionHENativeZeroDirection[
    Last[niMixedFixture["Directions"]] + niMixedDeformation, niMixedRules, {}, 45];
  check["an actual deformed direction is retained when exact expansion does not close",
    niMixedNonzero["Status"] === "NOT_PROVED" &&
    niMixedNonzero["OriginalDirection"] === Last[niMixedFixture["Directions"]] + niMixedDeformation &&
    ConstructiveDiagrams`Private`completionExternalSpinWeights[
      Last[niMixedFixture["Directions"]] + niMixedDeformation, niMixedRules] ===
      ConstructiveDiagrams`Private`completionExternalSpinWeights[Last[niMixedFixture["Directions"]], niMixedRules]];
  niMixedSmallNonzero = ConstructiveDiagrams`Private`completionHENativeZeroDirection[
    niMixedDeformation, niMixedRules, {}, 20];
  check["a tiny same-weight six-point tensor is explicitly proved nonzero",
    niMixedSmallNonzero["Status"] === "NOT_PROVED" &&
    AnyTrue[Lookup[Lookup[niMixedSmallNonzero, "NativeCertificate", <||>], "Roots", {}],
      Lookup[#, "ExpectedStatus", None] === "ZERO" && Lookup[#, "Status", None] === "NONZERO" &]],
  Print["SKIP: optional exact mixed-null runtime is unavailable: ", niOptionalReason]];

Print["CHECKS: ", checks, "; FAILURES: ", Length[failures]];
If[failures =!= {}, Print[InputForm[failures]]; Exit[1]];
Print["HEPCAT_TEST_COMPLETED:native-laurent-identity-tests.wl"];
Exit[0];
