(* Exact auxiliary-reference independence at sampled off-cut physical momenta. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
If[DownValues[ConstructiveDiagrams`Private`referenceCheckReport] === {},
  Print["Reference diagnostics did not load."]; Quit[1]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name];
  Print["FAIL: ", name]]);
riMassRules[n_Integer] := Thread[(Mass /@ Range[n]) -> ConstantArray[0, n]];
riMetadata[n_Integer] := <|"ExternalLegs" -> Range[n],
  "ExternalMassRules" -> riMassRules[n]|>;
riScalarModel = {{{"Scalar", "s", "s", 1, 0, 0, 0, 1, 1, "", ""}},
  {}, {}, {}};
riAngle[i_, j_] := SpinorChain[Spinor["Helicity", "Angle", i],
  Spinor["Helicity", "Angle", j]];
riSquare[i_, j_] := SpinorChain[Spinor["Helicity", "Square", i],
  Spinor["Helicity", "Square", j]];
riXiAngle[x_, i_] := SpinorChain[Spinor["Xi", "Angle", x],
  Spinor["Helicity", "Angle", i]];
riXiSquare[x_, i_] := SpinorChain[Spinor["Xi", "Square", x],
  Spinor["Helicity", "Square", i]];
riSchouten[x_] := riXiAngle[x, 1] riAngle[2, 3] +
  riXiAngle[x, 2] riAngle[3, 1] + riXiAngle[x, 3] riAngle[1, 2];
riSquareSchouten[x_] := riXiSquare[x, 1] riSquare[2, 3] +
  riXiSquare[x, 2] riSquare[3, 1] + riXiSquare[x, 3] riSquare[1, 2];
riReport[expression_, opts___] := Module[{settings},
  settings = Association[Join[{"ExternalLegs" -> 4, "Samples" -> 2,
    "RandomSeed" -> 1729, "ParameterRules" -> {}, "TimeConstraint" -> 20,
    "PhysicalChannels" -> {}}, {opts}]];
  ConstructiveDiagrams`Private`referenceCheckReport[expression,
    riMetadata[settings["ExternalLegs"]], settings["PhysicalChannels"],
    settings["Samples"], settings["RandomSeed"], settings["ParameterRules"],
    settings["TimeConstraint"]]
];

check["reference checks are enabled by default",
  ("ReferenceCheck" /. Options[CheckPhysicalResidues]) === Automatic];
riIndependent = riReport[1 + riSchouten[71]/riXiAngle[71, 4]];
check["an exact angle Schouten cancellation is reference independent",
  AssociationQ[riIndependent] && riIndependent["Status"] === "PASS"];
riSquareIndependent = riReport[3 + riSquareSchouten[72]/riXiSquare[72, 4]];
check["an exact square Schouten cancellation is reference independent",
  riSquareIndependent["Status"] === "PASS"];
riHomogeneous = riXiAngle[71, 1]/riXiAngle[71, 2];
riDirectionDependent = riReport[riHomogeneous];
check["reference direction dependence is detected despite homogeneity under rescaling",
  riDirectionDependent["Status"] === "FAIL"];
riMixedDependent = riReport[
  riXiAngle[71, 1] riXiSquare[71, 1]/
    (riXiAngle[71, 2] riXiSquare[71, 2])];
check["linked angle and square references expose mixed direction dependence",
  riMixedDependent["Status"] === "FAIL"];
riMixedIndependent = riReport[1 +
  riSchouten[71] riXiSquare[71, 2]/(riXiAngle[71, 4] riXiSquare[71, 4]) +
  riSquareSchouten[71] riXiAngle[71, 1]/(riXiAngle[71, 4] riXiSquare[71, 4])];
check["mixed angle-square identities cancel with one coherent null reference",
  riMixedIndependent["Status"] === "PASS"];
riTwoReferences = riReport[riHomogeneous - riXiAngle[72, 1]/riXiAngle[72, 2]];
check["distinct literal Xi labels are varied independently rather than silently identified",
  riTwoReferences["Status"] === "FAIL" && Sort[riTwoReferences["References"]] === {71, 72}];
riShared = riReport[riHomogeneous + riXiAngle[71, 3]/riXiAngle[71, 4]];
check["one shared reference may occur at several external legs",
  riShared["Status"] === "FAIL" && riShared["References"] === {71}];
check["potential sharing is reported as cooccurrence rather than a proven photon assignment",
  Keys[riShared["PotentialSharedReferences"]] === {71} &&
    riShared["ReferenceLegAssociationScope"] === "SpinorChainCooccurrenceOnly"];
riManyIndependent = riReport[riSchouten[71]/riXiAngle[71, 4] +
  riSquareSchouten[72]/riXiSquare[72, 4] + 5];
check["all reference variables are checked together after exact cancellations",
  riManyIndependent["Status"] === "PASS" &&
    Sort[riManyIndependent["References"]] === {71, 72}];
riNoReferences = riReport[MomProd[1, 2]];
check["a reference-free expression is explicitly scoped rather than a Ward-identity proof",
  riNoReferences["Status"] === "PASS" && riNoReferences["Scope"] === "NoExplicitReferences"];
riHiddenX = riReport[xFactor[1, 2]];
riHiddenXTilde = riReport[xTildeFactor[1, 2]];
check["unexpanded x factors are unresolved even without an explicit Xi",
  riHiddenX["Status"] === "UNRESOLVED" && riHiddenXTilde["Status"] === "UNRESOLVED"];
riUnsupportedXi = riReport[SpinorChain[
  Spinor["Xi", "UnsupportedChirality", 71], Spinor["Helicity", "Angle", 1]]];
check["unsupported explicit reference chiralities cannot masquerade as no references",
  riUnsupportedXi["Status"] === "UNRESOLVED" &&
    riUnsupportedXi["Scope"] =!= "NoExplicitReferences"];
riBareXi = riReport[Spinor["Xi", "Angle", 71]];
check["a bare top-level Xi spinor is unresolved rather than a reference-free scalar amplitude",
  riBareXi["Status"] === "UNRESOLVED" && riBareXi["References"] === {71} &&
    riBareXi["Scope"] =!= "NoExplicitReferences"];
riPhysical = <|"Cut" -> {1, 3}, "Complement" -> {2, 4},
  "ExternalLegs" -> Range[4], "ExternalMassRules" -> riMassRules[4],
  "Mass" -> 0, "Propagator" -> PropDen[Mom[Multiparticle[1, 3]], 0]|>;
riOffCut = riReport[riPhysical["Propagator"] riHomogeneous,
  "PhysicalChannels" -> {riPhysical}];
check["reference dependence proportional to a physical propagator is not hidden on its cut",
  riOffCut["Status"] === "FAIL"];
riPoleExpression = riReport[riHomogeneous/riPhysical["Propagator"],
  "PhysicalChannels" -> {riPhysical}];
check["ordinary physical denominators remain off shell during reference comparisons",
  riPoleExpression["Status"] === "FAIL"];
riOffCutSample = First[riOffCut["Samples"]];
riOffCutChart = riOffCutSample["Chart"];
riExactZero[expression_] := AllTrue[Flatten[{expression}], RootReduce[Together[#]] === 0 &];
check["recorded reference samples conserve momentum and satisfy every external shell",
  riExactZero[Total[Values[riOffCutChart["Momenta"]]]] &&
    riExactZero[Det /@ Values[riOffCutChart["Momenta"]]]];
check["recorded samples keep the actual physical denominator nonzero",
  !riExactZero[ConstructiveDiagrams`Private`residueEvaluateChart[
    riPhysical["Propagator"], riOffCutChart, {}]]];
check["failure diagnostics retain an exact nonzero difference coefficient",
  riOffCutSample["NonzeroCoefficientCount"] > 0 &&
    riOffCutSample["OffendingCoefficients"] =!= {}];
riJointCoordinates = First[riTwoReferences["Samples"]]["ReferenceCoordinates"];
check["each literal reference has four independent linked real coordinates",
  Sort[Keys[riJointCoordinates]] === {71, 72} &&
    AllTrue[Values[riJointCoordinates], Length[#] === 4 &] &&
    DuplicateFreeQ[Flatten[Values[riJointCoordinates]]]];
riSix = riReport[1 + riSchouten[71]/riXiAngle[71, 6], "ExternalLegs" -> 6];
check["reference comparisons are not restricted to four-point kinematics",
  riSix["Status"] === "PASS"];
riUnknown = riReport[riUnspecifiedCoupling riHomogeneous];
check["unspecified coefficients cannot produce a false reference-independence pass",
  riUnknown["Status"] === "UNRESOLVED"];
riKnown = riReport[riSpecifiedCoupling riHomogeneous,
  "ParameterRules" -> {riSpecifiedCoupling -> 1}];
check["known coupling values are applied consistently in reference comparisons",
  riKnown["Status"] === "FAIL"];
riTimed = riReport[riHomogeneous, "Samples" -> 100,
  "TimeConstraint" -> 0.000001];
check["reference-check timeouts remain unresolved", riTimed["Status"] === "UNRESOLVED"];

riMassiveMetadata = <|"ExternalLegs" -> Range[5],
  "ExternalMassRules" -> Thread[(Mass /@ Range[5]) -> {1, 1, 0, 0, 0}]|>;
riMassiveSpinFactor = SpinorChain[Spinor["Spin", "Angle", 1],
  Spinor["Spin", "Angle", 2]];
riMassiveSchouten = riXiAngle[71, 3] riAngle[4, 5] +
  riXiAngle[71, 4] riAngle[5, 3] + riXiAngle[71, 5] riAngle[3, 4];
riMassiveIndependent = ConstructiveDiagrams`Private`referenceCheckReport[
  riMassiveSpinFactor (1 + riMassiveSchouten/riXiAngle[71, 3]),
  riMassiveMetadata, {}, 2, 1729, {}, 20];
riMassiveDependent = ConstructiveDiagrams`Private`referenceCheckReport[
  riMassiveSpinFactor riXiAngle[71, 3]/riXiAngle[71, 4],
  riMassiveMetadata, {}, 2, 1729, {}, 20];
check["reference independence holds for every coefficient of an implicit massive-spin amplitude",
  riMassiveIndependent["Status"] === "PASS" &&
    AllTrue[riMassiveIndependent["Samples"], Length[#["SpinVariables"]] === 2 &]];
check["massive coherent-spin reference dependence is not hidden by one spin projection",
  riMassiveDependent["Status"] === "FAIL" &&
    AllTrue[riMassiveDependent["Samples"], #["NonzeroCoefficientCount"] > 0 &]];
riMassiveSample = First[riMassiveDependent["Samples"]];
riReferenceVariables = Flatten[Values[riMassiveSample["ReferenceCoordinates"]]];
check["external massive-spin bases and momenta do not vary with reference coordinates",
  FreeQ[KeyTake[riMassiveSample["Chart"],
    {"Momenta", "AngleBasis", "SquareBasis", "ExternalSpinVectors"}],
      Alternatives @@ riReferenceVariables]];

(* Verify the frozen physical spin bases independently against the package's
   external Dirac identities, including an outgoing crossed massive particle. *)
riDiracMetadata = <|"ExternalLegs" -> Range[5],
  "ExternalMassRules" -> Thread[(Mass /@ Range[5]) -> {1, 1, 1, 0, 0}]|>;
riDiracChart = ConstructiveDiagrams`Private`highEnergyAutomaticChart[
  0, riDiracMetadata, 1, 1729, {}];
riDiracChart = Join[riDiracChart /. riDiracChart["PoleVariable"] ->
  riDiracChart["KinematicChecks"]["ValidationParameter"],
  <|"PoleVariable" -> Unique["unusedDiracPole"]|>];
riDiracIdentityQ[chain_] := Module[{mapped, polynomial, coefficients},
  mapped = ConstructiveDiagrams`Private`residueEvaluateChart[
    chain - ReduceSpinorProducts[chain], riDiracChart, {}];
  If[!ConstructiveDiagrams`Private`residueFiniteQ[mapped] ||
      ConstructiveDiagrams`Private`residueUnmappedQ[mapped], Return[False]];
  polynomial = Expand[Numerator[Cancel[Together[mapped]]]];
  coefficients = CoefficientRules[polynomial, riDiracChart["SpinVariables"]];
  AllTrue[coefficients,
    ConstructiveDiagrams`Private`highEnergyZeroState[Last[#]] === "Zero" &]
];
check["frozen incoming coherent spinors obey the right external Dirac identity",
  riDiracIdentityQ[SpinorChain[Spinor["Helicity", "Square", 4],
    Mom[1], Spinor["Spin", "Angle", 1]]]];
check["frozen explicit upper spin components obey the left external Dirac identity",
  riDiracIdentityQ[SpinorChain[Spinor["Spin", "Upper", "Square", 2, -1/2],
    Mom[2], Spinor["Helicity", "Angle", 4]]]];
check["crossed outgoing coherent spinors obey the right external Dirac identity",
  riDiracIdentityQ[SpinorChain[Spinor["Helicity", "Square", 4],
    Mom[3], Spinor["Spin", "Angle", 3]]]];
check["crossed explicit lower spin components obey the left external Dirac identity",
  riDiracIdentityQ[SpinorChain[Spinor["Spin", "Lower", "Angle", 3, 1/2],
    Mom[3], Spinor["Helicity", "Square", 4]]]];

(* Give a complete physical amplitude a genuine auxiliary-reference form:
   this rewrites its actual <12> factor, rather than adding a vanishing term. *)
Get[FileNameJoin[{root, "tests", "QED-5-point-residues.wl"}]];
riQEDReference = Spinor["Xi", "Angle", 71];
riQEDAngle12 = (
  SpinorChain[riQEDReference, Spinor["Spin", "Angle", 2]]
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle", 3]] -
  SpinorChain[riQEDReference, Spinor["Spin", "Angle", 1]]
    SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle", 3]])/
  SpinorChain[riQEDReference, Spinor["Helicity", "Angle", 3]];
riQEDWithReference = prH /. prSpinAngle[1, 2] -> riQEDAngle12;
riQEDReport = ConstructiveDiagrams`Private`referenceCheckReport[
  riQEDWithReference, KeyTake[First[prTargets], {"ExternalLegs", "ExternalMassRules"}],
  prTargets, 2, 1729, {Me -> 1, EE -> 1}, 30];
check["the complete physical five-point QED amplitude passes in a nontrivial Xi representation",
  !FreeQ[riQEDWithReference, Spinor["Xi", ___]] &&
    riQEDReport["Status"] === "PASS" && riQEDReport["References"] === {71} &&
    AllTrue[riQEDReport["Samples"], Length[#["SpinVariables"]] === 2 &]];
riQEDFourRecords = DiagramAmplitudeRecords[CreateDiagrams[
  {"e", "E", "A.+", "A.+"}, prModel]];
riQEDFourRaw = prPreparedDiagramSum[riQEDFourRecords];
(* A deliberate common-reference input is a separate representation. The
   checker must not make this choice implicitly for a distinct-reference sum. *)
riQEDFourCommon = riQEDFourRaw /.
  Spinor["Xi", kind_, _Integer] :> Spinor["Xi", kind, 0];
riQEDFourReport = ConstructiveDiagrams`Private`referenceCheckReport[
  riQEDFourCommon,
  KeyTake[First[prFourPointTargets], {"ExternalLegs", "ExternalMassRules"}],
  prFourPointTargets, 2, 1729, {Me -> 1, EE -> 1}, 30];
check["actual all-plus four-point QED diagrams pass for an explicitly chosen common Xi",
  riQEDFourReport["Status"] === "PASS" && riQEDFourReport["References"] === {0}];
riQEDFourRawReport = ConstructiveDiagrams`Private`referenceCheckReport[
  riQEDFourRaw,
  KeyTake[First[prFourPointTargets], {"ExternalLegs", "ExternalMassRules"}],
  prFourPointTargets, 1, 1729, {Me -> 1, EE -> 1}, 30];
check["the same raw diagrams with distinct photon references are not silently merged",
  riQEDFourRawReport["Status"] === "FAIL" &&
    Sort[riQEDFourRawReport["References"]] === {3, 4}];
riRawQEDFive = prPreparedDiagramSum[prDiagramRecords[]];
riRawQEDReport = ConstructiveDiagrams`Private`referenceCheckReport[
  riRawQEDFive, KeyTake[First[prTargets], {"ExternalLegs", "ExternalMassRules"}],
  prTargets, 1, 1729, {Me -> 1, EE -> 1}, 30];
check["the known unfinished raw five-point continuation remains an honest negative control",
  riRawQEDReport["Status"] === "FAIL" &&
    Sort[riRawQEDReport["References"]] === {3, 4, 5}];

Do[
  check["invalid reference option is rejected " <> ToString[bad, InputForm],
    Quiet[CheckPhysicalResidues[{riMassRules[4], 1}, {}, riScalarModel,
      bad]] === $Failed],
  {bad, {"ReferenceCheck" -> "Sometimes", "ReferenceSamples" -> 0,
    "ReferenceSamples" -> 3/2, "ReferenceTimeConstraint" -> 0}}
];
riDefault = CheckPhysicalResidues[{riMassRules[4], 7}, {}, riScalarModel,
  "Output" -> "Details", "HighEnergySamples" -> 1];
check["the ordinary report combines reference and high-energy checks by default",
  riDefault["Summary"] === <|"PASS" -> 2|> &&
    riDefault["ReferenceSummary"] === <|"PASS" -> 1|> &&
    riDefault["ReferenceCheck"]["Status"] === "PASS"];
check["the unified table names reference diagnostics alongside the existing checks",
  Sort[Lookup[Normal[riDefault["Table"]], "Type"]] === {"HighEnergy", "Reference"}];
riPublicSamples = CheckPhysicalResidues[
  {riMassRules[4], 1 + riSchouten[71]/riXiAngle[71, 4]}, {}, riScalarModel,
  "Output" -> "References", "HighEnergyCheck" -> False,
  "SpuriousPoles" -> {}, "ReferenceSamples" -> 2];
check["reference sample diagnostics have their own visible public table",
  Head[riPublicSamples] === Dataset && Length[Normal[riPublicSamples]] === 2 &&
    AllTrue[Normal[riPublicSamples], ContainsAll[Keys[#], {"Sample", "Status"}] &]];
riDisabled = CheckPhysicalResidues[{riMassRules[4], 7}, {}, riScalarModel,
  "HighEnergyCheck" -> False, "ReferenceCheck" -> False, "Output" -> "Details"];
check["reference opt-out preserves legacy status counts and avoids an invented row",
  riDisabled["Summary"] === <||> && !KeyExistsQ[riDisabled, "ReferenceCheck"]];
riDisabledDisplay = CheckPhysicalResidues[{riMassRules[4], 7}, {}, riScalarModel,
  "HighEnergyCheck" -> False, "ReferenceCheck" -> False, "Output" -> "References"];
check["asking for a disabled reference display gives an empty table",
  Head[riDisabledDisplay] === Dataset && Normal[riDisabledDisplay] === {}];

Print[checks, " reference-independence checks; ", Length[failures], " failures."];
If[failures =!= {}, Print[failures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:reference-independence-tests.wl"];
Quit[0];
