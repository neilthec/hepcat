(* Identity-zero correction directions are not undetermined interactions. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0;
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);
heDiagramModel = ReadModel[FileNameJoin[{root, "models"}], 2];
heModel = ReadModel[FileNameJoin[{root, "models"}], 3];
heFeynmanModel = ReadModel[FileNameJoin[{root, "model_feynman"}], 2];
heOriginalModels = {heDiagramModel, heModel};
heMass = ModelParameter[heModel, "Me"];
heRules = {Mass[1] -> heMass, Mass[2] -> heMass, Mass[3] -> 0, Mass[4] -> 0, Mass[5] -> 0};
heChart = ConstructiveDiagrams`Private`completionHESymbolicExternalChart[heRules];
check["symbolic identity charts have no sampled momentum assignments or internal cuts",
  AssociationQ[heChart] && heChart["InternalOnShellConditions"] === {} &&
    heChart["ExternalMassShellsValidated"] && heChart["MomentumConservationValidated"] &&
    Length[heChart["ExternalIdentityCoordinates"]] === 5 &&
    AllTrue[Values[heChart["Momenta"]], MatrixQ] &&
    And @@ (ConstructiveDiagrams`Private`residueExactZero[
      Det[heChart["Momenta"][#]] - (Mass[#] /. heRules)^2] & /@ Range[5]) &&
    ConstructiveDiagrams`Private`residueExactZero[Total[Values[heChart["Momenta"]]]]];
heConservationZero = Total[MomProd[1, #] & /@ Range[5]];
heZeroProof = ConstructiveDiagrams`Private`completionHEZeroDirection[
  heConservationZero, heRules, heModel, 10];
check["external momentum conservation can prove a correction direction zero",
  heZeroProof["Status"] === "PROVED_ZERO"];
heWrongChirality = SpinorChain[Spinor["Helicity", "Angle", 3], Spinor["Helicity", "Square", 4]];
check["a noncovariant angle-square contraction cannot receive a frame-based zero certificate",
  !ConstructiveDiagrams`Private`completionHECovariantScalarQ[heWrongChirality, Range[5]] &&
    ConstructiveDiagrams`Private`completionHEZeroDirection[
      heWrongChirality, heRules, heModel, 10]["Status"] === "NOT_PROVED"];
check["untyped momentum traces and odd chiral traces are not invariant certificates",
  !ConstructiveDiagrams`Private`completionHECovariantScalarQ[Tr[Mom[1]], Range[5]] &&
    !ConstructiveDiagrams`Private`completionHECovariantScalarQ[SpinorTrace[Mom[1]], Range[5]]];
heInhomogeneous = 1 + SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity", "Square", 4]];
check["fixing massless spinor phases cannot hide inhomogeneous external weights",
  ConstructiveDiagrams`Private`completionHEZeroDirection[
    heInhomogeneous, heRules, heModel, 10]["Status"] === "NOT_PROVED"];
hePhaseBracket = SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity", "Square", 4]];
heBranchArtifact = Sqrt[hePhaseBracket^2] - hePhaseBracket;
check["kinematic square-root branches cannot be fixed by the identity chart",
  !ConstructiveDiagrams`Private`completionHECovariantScalarQ[heBranchArtifact, Range[5]] &&
    !ConstructiveDiagrams`Private`completionHECovariantScalarQ[Sqrt[MomProd[3, 4]], Range[5]] &&
    ConstructiveDiagrams`Private`completionHECovariantScalarQ[Sqrt[heMass], Range[5]] &&
    ConstructiveDiagrams`Private`completionHEZeroDirection[
      heBranchArtifact, heRules, heModel, 10]["Status"] === "NOT_PROVED"];
heFixedSpin = SpinorChain[Spinor["Spin", "Upper", "Angle", 1, -1/2],
  Spinor["Spin", "Upper", "Angle", 2, -1/2]];
check["fixed spin components cannot be certified only in a chosen rest frame",
  ConstructiveDiagrams`Private`completionHEZeroDirection[
    heFixedSpin, heRules, heModel, 10]["Status"] === "NOT_PROVED"];
heParameter = Unique["identityFreedom"];
heUnproved = ConstructiveDiagrams`Private`completionHEPruneIdentityFamily[
  7 + heParameter heConservationZero, {heParameter}, heRules, heModel, 0];
check["an exhausted proof budget retains the complete correction direction",
  heUnproved["FreeParameters"] === {heParameter} && heUnproved["RemovedParameters"] === {}];
heNonlinearFamily = 7 + heParameter^2 MomProd[1, 2];
heNonlinearResult = ConstructiveDiagrams`Private`completionHEPruneIdentityFamily[
  heNonlinearFamily, {heParameter}, heRules, heModel, 1];
check["non-affine input cannot be changed by reconstructing its first derivatives",
  heNonlinearResult["AffineFamily"] === False &&
    heNonlinearResult["FamilyAmplitude"] === heNonlinearFamily &&
    heNonlinearResult["FreeParameters"] === {heParameter} &&
    heNonlinearResult["RemovedParameters"] === {}];
heScalarModel = heDiagramModel;
heScalarModel[[1]] = Append[heScalarModel[[1]],
  {"identity-test scalar", "identity_scalar", "identity_scalar", 1000, 0, 0, 0, 1, 1, "h", "h"}];
heScalarRules = Thread[(Mass /@ Range[4]) -> ConstantArray[0, 4]];
heContact = <|"Amplitude" -> 7, "FamilyAmplitude" -> 7 + heParameter,
  "FreeParameters" -> {heParameter}, "RemainingBasis" -> {1},
  "ExternalMassRules" -> heScalarRules, "ExternalParticles" -> ConstantArray["identity_scalar", 4]|>;
heContactResult = CompleteHighEnergy[heContact, {}, heScalarModel,
  "Samples" -> 1, "ValidationSamples" -> 1, "Parallelization" -> False,
  "ProgressReporting" -> False, "TimeConstraint" -> 30, "Output" -> "Details"];
check["genuine acceptable-growth local freedom remains underdetermined",
  heContactResult["Status"] === "UNDERDETERMINED" &&
    heContactResult["FreeParameters"] === {heParameter} &&
    heContactResult["RemainingBasis"] === {1} &&
    AllTrue[heContactResult["AfterReport"]["Checks"], #["Status"] === "PASS" &]];

(* Use the actual notebook's numerical defaults, reference choices and budgets.
   The independent Feynman calculation is only a final benchmark. No known
   higher-point expression is supplied to construction or completion. *)
heRun[process_List] := ConstructAmplitude[process, heDiagramModel, heModel,
  "PolePreference" -> "AllPhysical", "Output" -> "Details",
  "Samples" -> 2, "FeynmanModel" -> heFeynmanModel,
  "ProgressReporting" -> False, "PrintReport" -> False,
  "PreparationOptions" -> If[Count[process, "A.+" | "A.-"] === 1, {"MaxSteps" -> 0}, {}],
  "FactorizationOptions" -> If[Count[process, "A.+" | "A.-"] === 1,
    {"Samples" -> 3, "ValidationSamples" -> 1, "MaxCorrectionTerms" -> 128, "TimeConstraint" -> 300},
    {"Samples" -> 1, "ValidationSamples" -> 1, "MaxCorrectionTerms" -> 128, "TimeConstraint" -> 180}],
  "HighEnergyOptions" -> {"Samples" -> 1, "ValidationSamples" -> 1,
    "MaxCorrectionTerms" -> 128, "TimeConstraint" -> 180},
  "CheckOptions" -> {"Samples" -> 2, "TimeConstraint" -> 180}];
heFullConstruction = Environment["HEPCAT_TEST_FULL_HELICITY_CONSTRUCTION"] === "1";
If[heFullConstruction, Do[
  Print["CONSTRUCTING ", process];
  heResult = heRun[process];
  check["five-point construction completes every stage: " <> StringRiffle[process, ","],
    AssociationQ[heResult] && heResult["Status"] === "COMPLETE" &&
      heResult["Checks"]["Checks"] =!= {} &&
      AllTrue[heResult["Checks"]["Checks"], #["Status"] === "PASS" &] &&
      heResult["Checks"]["FeynmanCheck"]["Status"] === "PASS"];
  If[AssociationQ[heResult] && AssociationQ[Lookup[heResult["Stages"], "Completing high-energy behavior", None]],
    heReduction = Lookup[heResult["Stages"]["Completing high-energy behavior"],
      "FamilyIdentityReduction", <|"Certificates" -> {}|>];
    check["removed family directions carry exact identity proofs: " <> StringRiffle[process, ","],
      AllTrue[Select[heReduction["Certificates"], #["Status"] === "PROVED_ZERO" &],
        MemberQ[{"ExactRationalAlgebra", "OrdinaryExternalIdentities", "DenseSymbolicExternalChart"}, #["Method"]] &&
          Lookup[#, "NumericalKinematicSamples", 0] === 0 &&
          Lookup[#, "InternalOnShellConditions", {}] === {} &]]],
  {process, {{"e", "E", "A.-", "A.+", "A.+"}, {"e", "E", "A.+", "A.-", "A.+"},
    {"e", "E", "E", "e", "A.-"}, {"e", "E", "E", "e", "A.+"}}}],
  Print["SKIP: set HEPCAT_TEST_FULL_HELICITY_CONSTRUCTION=1 for the four full notebook constructor regressions."]];
check["identity pruning and construction preserve both supplied models",
  heOriginalModels === {heDiagramModel, heModel}];
If[checks =!= If[heFullConstruction, 19, 11], AppendTo[failures, "Not every enabled assertion executed."]];
Print["CHECKS: ", checks, "; FAILURES: ", Length[failures]];
If[failures =!= {}, Print[failures]];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:high-energy-family-tests.wl"]];
Quit[If[failures === {}, 0, 1]];
