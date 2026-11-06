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
heBudgetParameter = Unique["proofBudgetParameter"];
heSmallDirection = heBudgetTerm[1] + heBudgetTerm[2];
heLargeDirection = Total[Array[heBudgetTerm, 7000]];
heSmallBudget = ConstructiveDiagrams`Private`completionHEIdentityProofBudget[
  1 + heBudgetParameter heSmallDirection, {heBudgetParameter}, {heSmallDirection}, 300];
heLargeBudget = ConstructiveDiagrams`Private`completionHEIdentityProofBudget[
  1 + heBudgetParameter heLargeDirection, {heBudgetParameter}, {heLargeDirection}, 300];
check["identity-proof effort grows for a large direction even when the family has one coordinate",
  heLargeBudget["AllocatedSeconds"] > heSmallBudget["AllocatedSeconds"] &&
    heSmallBudget["AllocatedSeconds"] >= 45];
check["proof-budget diagnostics record the actual direction size and count",
  heLargeBudget["DirectionCount"] === 1 &&
    heLargeBudget["LargestDirectionLeafCount"] === LeafCount[heLargeDirection]];
heCappedBudget = ConstructiveDiagrams`Private`completionHEIdentityProofBudget[
  1 + heBudgetParameter heLargeDirection, {heBudgetParameter}, {heLargeDirection}, 20];
check["a large direction cannot extend an explicitly limited stage clock",
  0 < heCappedBudget["AllocatedSeconds"] <= 15];
heManyBudget = ConstructiveDiagrams`Private`completionHEIdentityProofBudget[
  1, Array[heBudgetVariable, 10], ConstantArray[heSmallDirection, 10], 300];
check["complexity-aware allocation preserves the existing direction-count allowance",
  heManyBudget["AllocatedSeconds"] >= 100];
heFallbackBudget = ConstructiveDiagrams`Private`completionHEIdentityProofBudget[
  1 + heBudgetParameter heLargeDirection, {heBudgetParameter}, Automatic, 300];
check["an unavailable coefficient list uses the full family size conservatively",
  heFallbackBudget["LargestDirectionLeafCount"] === LeafCount[1 + heBudgetParameter heLargeDirection] &&
    heFallbackBudget["AllocatedSeconds"] >= heLargeBudget["AllocatedSeconds"]];
check["empty families need no proof clock and an unlimited stage still gets a bounded workload allowance",
  ConstructiveDiagrams`Private`completionHEIdentityProofBudget[1, {}, {}, 300]["AllocatedSeconds"] === 0 &&
    0 < ConstructiveDiagrams`Private`completionHEIdentityProofBudget[
      1 + heBudgetParameter heLargeDirection, {heBudgetParameter}, {heLargeDirection}, Infinity]["AllocatedSeconds"] < Infinity];
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
heXi[kind_, leg_] := SpinorChain[Spinor["Xi", kind, 91], Spinor["Helicity", kind, leg]];
heBracket[kind_, left_, right_] := SpinorChain[Spinor["Helicity", kind, left],
  Spinor["Helicity", kind, right]];
heXiSchouten = heXi["Angle", 3] heBracket["Angle", 4, 5] -
  heXi["Angle", 4] heBracket["Angle", 3, 5] + heXi["Angle", 5] heBracket["Angle", 3, 4];
heXiProof = ConstructiveDiagrams`Private`completionHEReferenceIdentityCertificate[
  heXiSchouten, heRules, 5];
check["reference identities retain all independent symbolic auxiliary coordinates",
  heXiProof["Status"] === "PROVED_ZERO" && heXiProof["FullAuxiliaryReferenceCoordinates"] &&
    Keys[heXiProof["ReferenceCoordinates"]] === {91} &&
    Length[heXiProof["ReferenceCoordinates"][91]] === 4 &&
    AllTrue[heXiProof["ReferenceCoordinates"][91], MatchQ[#, _Symbol] &]];
check["an auxiliary-reference identity certificate uses no numerical kinematics or internal shells",
  heXiProof["NumericalKinematicSamples"] === 0 && heXiProof["NumericalParameterAssignments"] === {} &&
    heXiProof["InternalOnShellConditions"] === {} && heXiProof["ExactRationalNumerator"] === 0];
check["the public identity-zero route can certify a shared-reference Schouten identity",
  ConstructiveDiagrams`Private`completionHEZeroDirection[
    heXiSchouten, heRules, heModel, 5]["Status"] === "PROVED_ZERO"];
check["a genuine reference-dependent ratio is not declared zero",
  ConstructiveDiagrams`Private`completionHEReferenceIdentityCertificate[
    heXi["Angle", 3]/heXi["Angle", 4], heRules, 5]["Status"] === "NOT_PROVED"];
heBadXiContraction = SpinorChain[Spinor["Xi", "Angle", 91], Spinor["Helicity", "Square", 3]];
check["auxiliary coordinates cannot certify a noncovariant angle-square contraction",
  !ConstructiveDiagrams`Private`completionHECovariantScalarQ[heBadXiContraction, Range[5], True] &&
    ConstructiveDiagrams`Private`completionHEReferenceIdentityCertificate[
      heBadXiContraction, heRules, 5]["Status"] === "NOT_PROVED"];
check["an exhausted reference identity budget retains the unproved direction",
  ConstructiveDiagrams`Private`completionHEReferenceIdentityCertificate[
    heXiSchouten, heRules, 0]["Status"] === "NOT_PROVED"];
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
(* Affine zero proofs must not expand a compact shared-source expression
   into one copy of the source per remaining derivative coordinate. *)
heFactoredParameters=Array[heFactoredCoordinate,3];
heFactoredSource=Total[Array[heOpaqueSourceTerm,40]];
heFactoredFamily=7+(2 heFactoredParameters[[1]]+3 heFactoredParameters[[2]]+
  5 heFactoredParameters[[3]]) heFactoredSource+heFactoredParameters[[3]] MomProd[1,2];
heFactoredUnproved=ConstructiveDiagrams`Private`completionHEPruneIdentityFamily[
  heFactoredFamily,heFactoredParameters,heRules,heModel,0];
heDerivativeRebuild=7+heFactoredParameters.(D[heFactoredFamily,#]&/@heFactoredParameters);
check["an unproved affine family preserves its original shared-source factorization exactly",
  heFactoredUnproved["FamilyAmplitude"]===heFactoredFamily &&
    LeafCount[heFactoredUnproved["FamilyAmplitude"]]<LeafCount[heDerivativeRebuild]];
check["expression preservation retains every unproved coordinate and its exact derivative",
  heFactoredUnproved["FreeParameters"]===heFactoredParameters &&
    heFactoredUnproved["RemainingBasis"]===(D[heFactoredFamily,#]&/@heFactoredParameters)];
heFactoredIdentityFamily=7+Total[MapThread[Times,{{1,2,3},heFactoredParameters}]] MomProd[1,1]+
  heFactoredParameters[[1]] Total[MomProd[1,#]&/@Range[2,5]]+
  heFactoredParameters[[3]] MomProd[1,2];
heFactoredCertified=Block[
  {ConstructiveDiagrams`Private`$completionParallelSettings=<|"Parallelization"->False,"MaxKernels"->1|>,
   ConstructiveDiagrams`Private`completionHEZeroDirection=Function[{expression,rules,model,seconds},
     If[expression===heConservationZero,<|"Status"->"PROVED_ZERO","Method"->"OrdinaryExternalIdentities"|>,
       <|"Status"->"NOT_PROVED","Method"->"NotAnIdentity"|>]]},
  ConstructiveDiagrams`Private`completionHEPruneIdentityFamily[
    heFactoredIdentityFamily,heFactoredParameters,heRules,heModel,2]];
heFactoredExpected=heFactoredIdentityFamily/.First[heFactoredParameters]->0;
check["a proved zero coordinate is removed by substitution in the original expression",
  heFactoredCertified["RemovedParameters"]==={First[heFactoredParameters]} &&
    heFactoredCertified["FamilyAmplitude"]===heFactoredExpected &&
    heFactoredCertified["FreeParameters"]===Rest[heFactoredParameters]];
check["kept derivative coordinates agree with the preserved factored family",
  heFactoredCertified["RemainingBasis"]===(D[heFactoredExpected,#]&/@Rest[heFactoredParameters]) &&
    Expand[heFactoredCertified["FamilyAmplitude"]-7-
      Rest[heFactoredParameters].heFactoredCertified["RemainingBasis"]]===0];
check["the factoring certificate references only proved-zero removed coordinates",
  heFactoredCertified["FamilyRepresentationCertificate"]["RemovedCoordinateRules"]==={First[heFactoredParameters]->0} &&
    heFactoredCertified["FamilyRepresentationCertificate"]["RemovedCertificatePositions"]==={1} &&
    heFactoredCertified["Certificates"][[1,"Status"]]==="PROVED_ZERO"];
check["an exhausted proof budget records no invented coordinate substitutions",
  heFactoredUnproved["FamilyRepresentationCertificate"]["RemovedCoordinateRules"]==={} &&
    TrueQ[heFactoredUnproved["FamilyRepresentationCertificate"]["OriginalAffineFactoringPreserved"]]];
(* A small correlated source proposal is a search choice, never a change
   to the original center, inherited family, factorization targets or model. *)
heWholeCharge=ModelParameter[heDiagramModel,"EE"];
heWholeValue=heWholeCharge^2 (MomProd[1,2]+MomProd[1,3])^5/PropDen[Mom[Multiparticle[1,2]],heMass];
heWholeRecord=<|"InputType"->"Diagram","HasInternalConditions"->True,
  "ShellRemainder"->heWholeValue,"OnShellChannels"->{{1,2}},"OffCutIdentityClaimed"->False|>;
heWholeChannel=Join[heWholeRecord,<|"InputType"->"FactorizationChannel",
  "ShellRemainder"->heWholeValue/ConstructiveDiagrams`z,"FixedReferenceExpression"->ConstructiveDiagrams`z|>];
heWholeNormalized=<|"Expression"->1,"ExternalMassRules"->heRules,
  "ExternalParticles"->{"e","E","A.+","A.-","A.+"},"Targets"->{},"Records"->{},
  "RemainderData"->{heWholeRecord},"ChannelRemainderData"->{heWholeChannel}|>;
heWholeOriginalHash=Hash[heWholeNormalized,"SHA256"];
heWholeProposal=ConstructiveDiagrams`Private`completionHEWholeSourceRemainders[heWholeNormalized,heDiagramModel,5];
check["whole-source proposals preserve model couplings and do not introduce independent interactions",
  heWholeProposal["Status"]==="AVAILABLE" && heWholeProposal["Basis"]=!={} &&
    !FreeQ[heWholeProposal["Basis"],heWholeCharge] &&
    AllTrue[heWholeProposal["Sources"],TrueQ[#["ModelCouplingsPreserved"]] &&
      #["NewIndependentInteraction"]===False && #["InternalConditionsReapplied"]===False&]];
check["whole channel remainders use the same sewing contour as the complete source path",
  AnyTrue[heWholeProposal["Sources"],#["InputType"]==="FactorizationChannel" &&
    #["ContourMultiplier"]===-ConstructiveDiagrams`z&] && FreeQ[heWholeProposal["Basis"],ConstructiveDiagrams`z]];
check["source proposal generation leaves every retained original record unchanged",
  Hash[heWholeNormalized,"SHA256"]===heWholeOriginalHash && TrueQ[heWholeProposal["SourceCatalogComplete"]]];
heWholeSynthetic=Join[heWholeProposal,<|"Basis"->{heWholeValue},"EstimatedAtomicDirections"->40|>];
check["correlated proposals require a complete catalog and a substantially smaller finite space",
  ConstructiveDiagrams`Private`completionHEWholeSourceEligibleQ[heWholeSynthetic,<|"CoherentComponents"->16|>] &&
    !ConstructiveDiagrams`Private`completionHEWholeSourceEligibleQ[heWholeSynthetic,<|"CoherentComponents"->4|>] &&
    !ConstructiveDiagrams`Private`completionHEWholeSourceEligibleQ[
      Join[heWholeSynthetic,<|"SourceCatalogComplete"->False|>],<|"CoherentComponents"->81|>] &&
    !ConstructiveDiagrams`Private`completionHEWholeSourceEligibleQ[
      Join[heWholeSynthetic,<|"EstimatedAtomicDirections"->2|>],<|"CoherentComponents"->81|>]];
check["an exhausted whole-source build cannot expose a partial correction catalog",
  MissingQ[ConstructiveDiagrams`Private`completionHEWholeSourceRemainders[heWholeNormalized,heDiagramModel,0]]];
heWholeInput=<|"Amplitude"->1,"FamilyAmplitude"->1+heParameter MomProd[1,2],
  "FreeParameters"->{heParameter},"RemainingBasis"->{MomProd[1,2]},
  "ExternalMassRules"->heRules,"ExternalParticles"->{"e","E","A.+","A.-","A.+"},
  "RemainderData"->{heWholeRecord}|>;
heWholeRun[basis_,pass_,preferredStatus_]:=Block[
 {ConstructiveDiagrams`Private`completionHECubicBilinearSource=Function[Null,Missing["NotApplicable"],HoldAll],
  ConstructiveDiagrams`Private`completionHEWholeSourcePreflightQ=Function[Null,True,HoldAll],
  ConstructiveDiagrams`Private`completionHEWholeSourceEligibleQ=Function[{proposal,metrics},
    AssociationQ[proposal] && Lookup[proposal,"Status",None]==="AVAILABLE"],
  ConstructiveDiagrams`Private`completionHEWholeSourceRemainders=Function[Null,
    heWholeBuilds++;heWholeSynthetic,HoldAll],
  ConstructiveDiagrams`Private`completionHECheck=Function[Null,heWholeInitialChecks++;
    <|"Checks"->{<|"Status"->Which[pass===True,"PASS",pass===False,"FAIL",True,pass]|>},"HighEnergyCheck"-><||>,"ReferenceCheck"-><||>|>,HoldAll],
  ConstructiveDiagrams`Private`completionHighEnergyWork=Function[{input,channels,model,settings,normalized,retain},
    AppendTo[heWholeCalls,<|"Basis"->settings["CorrectionBasis"],"TimeConstraint"->settings["TimeConstraint"],
      "MaxCorrectionTerms"->settings["MaxCorrectionTerms"],"ReusedInitialCheck"->KeyExistsQ[settings,"BeforeReport"],
      "IdentityProofStageTimeConstraint"->Lookup[settings,"IdentityProofStageTimeConstraint",Missing["NotNeeded"]],"OriginalInput"->input,
      "FamilyAmplitude"->normalized["FamilyAmplitude"],"FreeParameters"->normalized["FreeParameters"]|>];
    <|"Status"->If[settings["CorrectionBasis"]===Automatic,"COMPLETE",preferredStatus],
      "Amplitude"->normalized["Expression"],"FamilyAmplitude"->normalized["FamilyAmplitude"],
      "FreeParameters"->normalized["FreeParameters"],"RemainingBasis"->normalized["RemainingBasis"],
      "ExternalMassRules"->normalized["ExternalMassRules"],"Diagnostics"->{}|>]},
 CompleteHighEnergy[heWholeInput,{},heDiagramModel,"CorrectionBasis"->basis,
   "TimeConstraint"->90,"MaxCorrectionTerms"->7,"Samples"->1,"ValidationSamples"->1,
   "ProgressReporting"->False,"Output"->"Details","RandomSeed"->1729]];
heWholeCalls={};heWholeBuilds=0;heWholeInitialChecks=0;
heWholeFallback=heWholeRun[Automatic,False,"INCONSISTENT"];
check["an inconsistent correlated proposal falls back to the original complete Automatic space",
  Lookup[heWholeCalls,"Basis"]==={heWholeSynthetic["Basis"],Automatic} &&
    heWholeFallback["Status"]==="COMPLETE" && heWholeInitialChecks===1];
check["both attempts preserve the original center and every inherited family coordinate",
  AllTrue[heWholeCalls,#["OriginalInput"]===heWholeInput &&
    #["FamilyAmplitude"]===heWholeInput["FamilyAmplitude"] && #["FreeParameters"]==={heParameter}&] &&
    heWholeFallback["FamilyAmplitude"]===heWholeInput["FamilyAmplitude"]];
check["the small proposal shares the original clock and never overrides an explicit cap",
  First[heWholeCalls]["TimeConstraint"]<=30 && Last[heWholeCalls]["TimeConstraint"]<=90 &&
    Lookup[heWholeCalls,"MaxCorrectionTerms"]==={7,7} &&
    First[heWholeFallback["AutomaticSourceAttempts"]]["FullOriginalSourcesRetainedForFallback"]===True];
heWholeCalls={};heWholeBuilds=0;heWholeInitialChecks=0;
heWholeAlreadyPass=heWholeRun[Automatic,True,"INCONSISTENT"];
check["an already-passing point skips correlated source work and retains unresolved inherited freedom",
  heWholeBuilds===0 && Lookup[heWholeCalls,"Basis"]==={Automatic} && heWholeInitialChecks===1 &&
    heWholeAlreadyPass["FreeParameters"]==={heParameter} &&
    heWholeAlreadyPass["FamilyAmplitude"]===heWholeInput["FamilyAmplitude"]];
check["a passing baseline preserves proof effort while the outer shared clock remains authoritative",
  0<First[heWholeCalls]["IdentityProofStageTimeConstraint"]<=90 &&
    First[heWholeCalls]["IdentityProofStageTimeConstraint"]>=First[heWholeCalls]["TimeConstraint"] &&
    First[heWholeCalls]["TimeConstraint"]<=90];
heWholeCalls={};heWholeBuilds=0;heWholeInitialChecks=0;
heWholeExplicit=heWholeRun[{1},False,"UNRESOLVED"];
check["an explicitly supplied correction space bypasses every Automatic source proposal",
  heWholeBuilds===0 && Lookup[heWholeCalls,"Basis"]==={{1}} && heWholeInitialChecks===0];
heWholeCalls={};heWholeBuilds=0;heWholeInitialChecks=0;
heWholeAccepted=heWholeRun[Automatic,False,"COMPLETE"];
check["a fully completed correlated proposal avoids constructing the larger fallback",
  heWholeAccepted["Status"]==="COMPLETE" && Length[heWholeCalls]===1 &&
    First[heWholeAccepted["AutomaticSourceAttempts"]]["Status"]==="COMPLETE"];
heWholeResolvedReport=<|"Checks"->{<|"Status"->"PASS"|>,<|"Status"->"FAIL"|>,<|"Status"->"Opp Sign"|>},
  "HighEnergyCheck"-><||>,"ReferenceCheck"-><||>|>;
check["only fully resolved original-point reports can be reused across source proposals",
  ConstructiveDiagrams`Private`completionHEReusableInitialReportQ[heWholeResolvedReport]];
check["an inconclusive or empty check report cannot become a definitive cached failure",
  !ConstructiveDiagrams`Private`completionHEReusableInitialReportQ[
    Join[heWholeResolvedReport,<|"Checks"->{<|"Status"->"UNRESOLVED"|>}|>]] &&
    !ConstructiveDiagrams`Private`completionHEReusableInitialReportQ[
      Join[heWholeResolvedReport,<|"Checks"->{}|>]]];
heWholeCalls={};heWholeBuilds=0;heWholeInitialChecks=0;
heWholeInconclusive=heWholeRun[Automatic,"UNRESOLVED","INCONSISTENT"];
check["an inconclusive preferred baseline is retried by the full fallback without a stale cache",
  heWholeBuilds===0 && Lookup[heWholeCalls,"Basis"]==={Automatic} &&
    Lookup[heWholeCalls,"ReusedInitialCheck"]==={False} &&
    heWholeInconclusive["FamilyAmplitude"]===heWholeInput["FamilyAmplitude"]];
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
(* A stopped proof retains its coordinate, but is not evidence of a new
   interaction. Force only the proof result, leaving every constraint and
   independent validation in the public high-energy stage active. *)
heTimedProofResult = Block[
  {ConstructiveDiagrams`Private`completionHEZeroDirection =
    Function[{expression, rules, model, seconds},
      <|"Status" -> "NOT_PROVED", "Method" -> "TimeConstraint"|>]},
  CompleteHighEnergy[heContact, {}, heScalarModel,
    "Samples" -> 1, "ValidationSamples" -> 1, "Parallelization" -> False,
    "ProgressReporting" -> False, "TimeConstraint" -> 30, "Output" -> "Details"]];
check["an unfinished identity proof retains the family and underdetermined status",
  heTimedProofResult["Status"] === "UNDERDETERMINED" &&
    heTimedProofResult["FreeParameters"] === {heParameter} &&
    heTimedProofResult["RemainingBasis"] === {1}];
check["proof diagnostics distinguish unfinished certification from established nonzero freedom",
  heTimedProofResult["RetainedDirectionProofSummary"] === <|
    "RetainedDirections" -> 1, "EstablishedNonzeroDirections" -> 0,
    "IdentityProofUnfinished" -> 1, "ProofReasons" -> <|"TimeConstraint" -> 1|>|> &&
    MemberQ[heTimedProofResult["Diagnostics"], heTimedProofResult["RetainedDirectionProofSummary"]]];
check["a timed-out proof does not claim a missing interaction or require extra model information",
  StringContainsQ[StringRiffle[Cases[heTimedProofResult["Diagnostics"], _String], " "],
    "An unfinished identity proof does not establish a missing interaction"] &&
    !StringContainsQ[StringRiffle[Cases[heTimedProofResult["Diagnostics"], _String], " "],
      "additional model information is required"]];

(* Explicit null axes are generic coordinate choices, not extra kinematic
   constraints. Opposite homogeneous weights select a useful default pair. *)
hePivotRules = {Mass[1] -> heMass, Mass[2] -> heMass,
  Mass[3] -> 0, Mass[4] -> 0, Mass[5] -> 0, Mass[6] -> 0};
hePivotAngle[i_, j_] := SpinorChain[Spinor["Helicity", "Angle", i], Spinor["Helicity", "Angle", j]];
hePivotSquare[i_, j_] := SpinorChain[Spinor["Helicity", "Square", i], Spinor["Helicity", "Square", j]];
hePivotMixed = hePivotSquare[3, 4] hePivotSquare[4, 5] hePivotSquare[3, 5] hePivotAngle[3, 6] hePivotAngle[4, 6]/hePivotAngle[3, 4];
hePivotUniform = hePivotSquare[3, 4]^2 hePivotSquare[5, 6]^2;
check["Opposite homogeneous weights choose null3/null6",
  ConstructiveDiagrams`Private`completionHEPreferredNullPivots[hePivotMixed, hePivotRules] === {3, 6}];
check["Uniform weights retain first two nulls",
  ConstructiveDiagrams`Private`completionHEPreferredNullPivots[hePivotUniform, hePivotRules] === {3, 4}];
check["Unknown weights retain first two nulls",
  ConstructiveDiagrams`Private`completionHEPreferredNullPivots[hePivotSquare[3, 4] + hePivotAngle[3, 4], hePivotRules] === {3, 4}];
check["No null pair retains automatic massive frame",
  ConstructiveDiagrams`Private`completionHEPreferredNullPivots[hePivotMixed,
    {Mass[1] -> heMass, Mass[2] -> heMass, Mass[3] -> heMass, Mass[4] -> 0}] === Automatic];
Do[check["Reject invalid pivots " <> ToString[hePivotPivots, InputForm],
  ConstructiveDiagrams`Private`completionHESymbolicExternalChart[hePivotRules, hePivotPivots] === $Failed],
  {hePivotPivots, {{3, 3}, {1, 3}, {3, 99}, {3}, {3, 4, 5}, {3, "4"}, None}}];
hePivotEpsilon = {{0, 1}, {-1, 0}};
Do[
 hePivotChart = ConstructiveDiagrams`Private`completionHESymbolicExternalChart[hePivotRules, hePivotPivots];
 check["Axes preserved " <> ToString[hePivotPivots, InputForm], hePivotChart["NullAxisLegs"] === hePivotPivots];
 check["Conservation " <> ToString[hePivotPivots, InputForm],
   ConstructiveDiagrams`Private`residueExactZero[Total[Values[hePivotChart["Momenta"]]]]];
 check["All external mass shells " <> ToString[hePivotPivots, InputForm],
   AllTrue[Range[6], ConstructiveDiagrams`Private`residueExactZero[
     Det[hePivotChart["Momenta"][#]] - hePivotChart["Masses"][#]^2] &]];
 check["Null momentum spinor decomposition " <> ToString[hePivotPivots, InputForm],
   AllTrue[Range[3, 6], ConstructiveDiagrams`Private`residueExactZero[
     Outer[Times, hePivotChart["Angle"][#], hePivotChart["Square"][#]] - hePivotChart["Momenta"][#]] &]];
 check["Full coherent massive spin bases " <> ToString[hePivotPivots, InputForm],
   Length[hePivotChart["SpinVariables"]] === 2 && AllTrue[{1, 2},
     !ConstructiveDiagrams`Private`residueExactZero[Det[hePivotChart["AngleBasis"][#]]] &&
     ConstructiveDiagrams`Private`residueExactZero[hePivotChart["AngleBasis"][#].hePivotEpsilon.
       Transpose[hePivotChart["SquareBasis"][#]] - hePivotChart["Momenta"][#]] &]];
 hePivotCuts = Prepend[#, 1] & /@ Subsets[Range[3, 6], {1, 3}];
 check["No internal shell imposed on fourteen physical cuts " <> ToString[hePivotPivots, InputForm],
   hePivotChart["InternalOnShellConditions"] === {} && Length[hePivotCuts] === 14 &&
   AllTrue[hePivotCuts, !ConstructiveDiagrams`Private`residueExactZero[
     Det[Total[Lookup[hePivotChart["Momenta"], #]]] - heMass^2] &]],
 {hePivotPivots, {{3, 4}, {3, 6}, {6, 3}}}];

(* Solving the massive pair instead of the massless pair is a different
   rational coordinate chart on the same generic conserved external data. *)
heDependentChart = ConstructiveDiagrams`Private`completionHESymbolicExternalChart[
  hePivotRules, {3, 6}, {1, 2}];
check["explicit dependent shells retain the requested null axes and massive pair",
  AssociationQ[heDependentChart] && heDependentChart["NullAxisLegs"] === {3, 6} &&
    heDependentChart["DependentShellLegs"] === {1, 2}];
check["massive-dependent charts preserve conservation and every external mass shell",
  ConstructiveDiagrams`Private`residueExactZero[Total[Values[heDependentChart["Momenta"]]]] &&
    AllTrue[Range[6], ConstructiveDiagrams`Private`residueExactZero[
      Det[heDependentChart["Momenta"][#]] - heDependentChart["Masses"][#]^2] &]];
check["massive-dependent charts retain full coherent spin bases and null spinor decompositions",
  Length[heDependentChart["SpinVariables"]] === 2 &&
    AllTrue[{1, 2}, !ConstructiveDiagrams`Private`residueExactZero[
      Det[heDependentChart["AngleBasis"][#]]] &&
      ConstructiveDiagrams`Private`residueExactZero[
        heDependentChart["AngleBasis"][#].hePivotEpsilon.
          Transpose[heDependentChart["SquareBasis"][#]] - heDependentChart["Momenta"][#]] &] &&
    AllTrue[Range[3, 6], ConstructiveDiagrams`Private`residueExactZero[
      Outer[Times, heDependentChart["Angle"][#], heDependentChart["Square"][#]] -
        heDependentChart["Momenta"][#]] &]];
check["massive-dependent charts impose no shell on any of the fourteen internal cuts",
  heDependentChart["InternalOnShellConditions"] === {} &&
    AllTrue[hePivotCuts, !ConstructiveDiagrams`Private`residueExactZero[
      Det[Total[Lookup[heDependentChart["Momenta"], #]]] - heMass^2] &]];
Do[check["reject invalid dependent shells " <> ToString[heBadDependents, InputForm],
  ConstructiveDiagrams`Private`completionHESymbolicExternalChart[
    hePivotRules, {3, 6}, heBadDependents] === $Failed],
  {heBadDependents, {{1, 1}, {1, 3}, {1, 99}, {1}, {1, 2, 4}, {1, "2"}}}];
heChartChoices = ConstructiveDiagrams`Private`completionHEIdentityChartChoices[
  hePivotMixed, hePivotRules];
check["the identity portfolio offers a massive-dependent chart and preserves the legacy chart",
  First[heChartChoices] === <|"NullAxisLegs" -> {3, 6}, "DependentShellLegs" -> {1, 2}|> &&
    Last[heChartChoices] === <|"NullAxisLegs" -> {3, 6}, "DependentShellLegs" -> Automatic|>];

(* A retained direction from the actual six-point mixed-helicity fit formerly
   exhausted its proof budget. The fixture removes only its common charge
   factor, retains symbolic mass dependence, and supplies no amplitude answer. *)
heRealIdentity = Get[FileNameJoin[{root, "tests", "checks", "fixtures",
  "high-energy-six-point-zero.wl"}]][heMass];
heRealProof = ConstructiveDiagrams`Private`completionHEZeroDirection[
  heRealIdentity, hePivotRules, heModel, 40];
check["an actual retained six-point direction has an exact generic external identity certificate",
  FreeQ[heRealIdentity, Global`heIdentityMass] &&
    heRealProof["Status"] === "PROVED_ZERO" &&
    heRealProof["Method"] === "DenseSymbolicExternalChart" &&
    heRealProof["Chart"]["DependentShellLegs"] === {1, 2} &&
    heRealProof["NumericalKinematicSamples"] === 0 &&
    heRealProof["InternalOnShellConditions"] === {}];
heRealDeformation = heRealIdentity + hePivotMixed SpinorChain[
  Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]];
heNonzeroProof = ConstructiveDiagrams`Private`completionHEZeroDirection[
  heRealDeformation, hePivotRules, heModel, 40];
check["a genuine covariant deformation of that direction is never certified zero",
  heNonzeroProof["Status"] === "NOT_PROVED" &&
    heNonzeroProof["Method"] === "NonzeroSymbolicExternalChart"];
heRealParameter = Unique["realIdentityDirection"];
heRealExpired = ConstructiveDiagrams`Private`completionHEPruneIdentityFamily[
  7 + heRealParameter heRealIdentity, {heRealParameter}, hePivotRules, heModel, 0,
  {heRealIdentity}];
check["an expired real-direction proof preserves its exact affine family",
  heRealExpired["FreeParameters"] === {heRealParameter} &&
    heRealExpired["RemainingBasis"] === {heRealIdentity} &&
    heRealExpired["RemovedParameters"] === {}];

(* Coherent-spin coefficients are checked independently, including the mixed
   component. Built-in CoefficientRules provides a separate small-input oracle
   for native Taylor coefficient checks on large rational external charts. *)
heCoefficientVariables = {Unique["coherentOne"], Unique["coherentTwo"]};
{heCoefficientU, heCoefficientV} = heCoefficientVariables;
heKinematicA = Unique["kinematicA"]; heKinematicB = Unique["kinematicB"];
heRationalZero = (heKinematicA^2 - heKinematicB^2)/(heKinematicA - heKinematicB) -
  heKinematicA - heKinematicB;
heMappedPolynomial = heRationalZero (1 + heCoefficientU) (1 + heCoefficientV);
heMappedRules = CoefficientRules[heMappedPolynomial, heCoefficientVariables];
heMappedProof = ConstructiveDiagrams`Private`completionHECoherentComponentCertificate[
  heMappedPolynomial, heCoefficientVariables, 10];
check["all four factored coherent-spin components receive exact zero certificates",
  Length[heMappedRules] === 4 &&
    AllTrue[Last /@ heMappedRules, TrueQ[Cancel[Together[#]] === 0] &] &&
    heMappedProof["Status"] === "PROVED_ZERO" &&
    heMappedProof["ComponentCount"] === Length[heMappedRules] &&
    Sort[heMappedProof["ComponentMonomials"]] === Sort[First /@ heMappedRules] &&
    heMappedProof["ComponentsChecked"] === Length[heMappedRules] &&
    heMappedProof["ReconstructionVerified"] === True &&
    heMappedProof["DegreeBoundsVerified"] === True &&
    heMappedProof["FullCoefficientCoverage"] === True &&
    heMappedProof["AllComponentsZero"] === True &&
    AllTrue[heMappedProof["ComponentProofs"], TrueQ[#["ExactZero"]] &]];
Do[
  heComponentTrial = heMappedPolynomial + heComponentMonomial/(heKinematicA + heKinematicB);
  heComponentRules = CoefficientRules[heComponentTrial, heCoefficientVariables];
  heComponentProof = ConstructiveDiagrams`Private`completionHECoherentComponentCertificate[
    heComponentTrial, heCoefficientVariables, 10];
  check["a nonzero coherent component is never dropped: " <> ToString[heComponentMonomial, InputForm],
    Count[TrueQ[Cancel[Together[#]] === 0] & /@ (Last /@ heComponentRules), False] === 1 &&
      heComponentProof["Status"] === "NOT_PROVED" &&
      heComponentProof["Method"] === "NonzeroCoherentPolynomialComponent" &&
      heComponentProof["AllComponentsZero"] === False &&
      AnyTrue[heComponentProof["ComponentProofs"], #["ExactZero"] === False &]],
  {heComponentMonomial, {1, heCoefficientU, heCoefficientV, heCoefficientU heCoefficientV}}];
heHigherSpinPolynomial = heRationalZero (1 + heCoefficientU^3) (1 - heCoefficientV^2);
heHigherSpinRules = CoefficientRules[heHigherSpinPolynomial, heCoefficientVariables];
heHigherSpinProof = ConstructiveDiagrams`Private`completionHECoherentComponentCertificate[
  heHigherSpinPolynomial, heCoefficientVariables, 10];
check["coherent coefficient certificates preserve higher external-spin powers",
  Sort[First /@ heHigherSpinRules] === Sort[{{0, 0}, {0, 2}, {3, 0}, {3, 2}}] &&
    heHigherSpinProof["Status"] === "PROVED_ZERO" &&
    heHigherSpinProof["ComponentCount"] === 12 &&
    Sort[heHigherSpinProof["ComponentMonomials"]] === Sort[Tuples[{Range[0, 3], Range[0, 2]}]] &&
    heHigherSpinProof["ComponentsChecked"] === 12 &&
    heHigherSpinProof["ReconstructionVerified"] === True &&
    heHigherSpinProof["DegreeBoundsVerified"] === True &&
    heHigherSpinProof["FullCoefficientCoverage"] === True];
heCollectorPolynomial = (heKinematicA + heCoefficientU) (heKinematicB + heCoefficientV)/
  (heKinematicA + heKinematicB) + heCoefficientU^2 heCoefficientV +
  (heKinematicA - heKinematicB) heCoefficientV^2;
heExpectedCoefficient[powers_] := Last[SelectFirst[heHigherSpinRules,
  First[#] === powers &, powers -> 0]];
check["every dense Taylor component agrees with the independent sparse coefficient oracle",
  heHigherSpinProof["CoefficientMethod"] === "ExactSymbolicTaylorCoefficients" &&
    AllTrue[heHigherSpinProof["ComponentProofs"],
      #["ExactZero"] === TrueQ[Cancel[Together[heExpectedCoefficient[#["Monomial"]]]] === 0] &]];
heBadDegreeBound = Block[
  {ConstructiveDiagrams`Private`completionHECoherentPolynomialDegreeBounds =
    Function[{expression, variables}, {0, 0}]},
  ConstructiveDiagrams`Private`completionHECoherentComponentCertificate[
    heCollectorPolynomial, heCoefficientVariables, 10]];
check["underestimated polynomial degrees are rejected by independent exact derivative guards",
  heBadDegreeBound["Status"] === "NOT_PROVED" &&
    heBadDegreeBound["Method"] === "CoherentPolynomialDegreeBoundFailed"];
heManySpinVariables = Table[Unique["componentLimitSpin"], {13}];
heTooManyComponents = ConstructiveDiagrams`Private`completionHECoherentComponentCertificate[
  Times @@ (1 + # & /@ heManySpinVariables), heManySpinVariables, 10];
check["an over-budget coefficient grid is retained without an incomplete zero certificate",
  heTooManyComponents["Status"] === "NOT_PROVED" &&
    heTooManyComponents["Method"] === "CoherentComponentLimit"];
heNonPolynomialProof = ConstructiveDiagrams`Private`completionHECoherentComponentCertificate[
  1/(heKinematicA + heCoefficientU) + heCoefficientV, heCoefficientVariables, 10];
check["a spin-dependent denominator cannot receive a polynomial zero certificate",
  heNonPolynomialProof["Status"] === "NOT_PROVED" &&
    heNonPolynomialProof["Method"] === "NonPolynomialCoherentSpins"];
heCoefficientExpired = ConstructiveDiagrams`Private`completionHECoherentComponentCertificate[
  heMappedPolynomial, heCoefficientVariables, 0];
check["an exhausted component-proof budget does not certify an unfinished polynomial",
  heCoefficientExpired["Status"] === "NOT_PROVED" &&
    heCoefficientExpired["Method"] === "TimeConstraint"];

(* Reuse only already certified external identities. Formal subtraction is
   independently checked before a smaller remainder can receive its own proof. *)
heSpanZeros = {heConservationZero, Total[MomProd[2, #] & /@ Range[5]]};
heSpanTarget = 2 First[heSpanZeros] + 3 Last[heSpanZeros];
heSpanReduction = ConstructiveDiagrams`Private`completionHECertifiedZeroRemainder[
  heSpanTarget, heSpanZeros, 10];
check["certified zero-span subtraction has an exact independently reconstructed relation",
  heSpanReduction["Status"] === "REDUCED" &&
    heSpanReduction["ReducedInput"] === 0 &&
    heSpanReduction["ExactFormalReconstruction"] === True &&
    heSpanReduction["ExactGroupingReconstruction"] === True &&
    Expand[heSpanTarget - heSpanReduction["KnownZeroCoefficients"].heSpanZeros -
      heSpanReduction["ReducedInput"]] === 0];
heSpanContact = ConstructiveDiagrams`Private`completionHECertifiedZeroRemainder[
  heSpanTarget + 1, heSpanZeros, 10];
check["certified zero-span subtraction retains a genuine nonzero contact remainder",
  heSpanContact["Status"] === "REDUCED" &&
    heSpanContact["ReducedInput"] === 1 &&
    Expand[heSpanTarget + 1 - heSpanContact["KnownZeroCoefficients"].heSpanZeros -
      heSpanContact["ReducedInput"]] === 0 &&
    ConstructiveDiagrams`Private`completionHEZeroDirection[
      heSpanContact["ReducedInput"], heRules, heModel, 10]["Status"] === "NOT_PROVED"];
heSpanBadGrouping = Block[
  {ConstructiveDiagrams`Private`preparationGroupedPropagatorForm =
    Function[{expression, seconds}, 0]},
  ConstructiveDiagrams`Private`completionHECertifiedZeroRemainder[
    heSpanTarget + 1, heSpanZeros, 10]];
check["a corrupted grouped remainder fails exact original-expression reconstruction",
  heSpanBadGrouping["Status"] === "UNCHANGED" &&
    heSpanBadGrouping["ReducedInput"] === heSpanTarget + 1 &&
    heSpanBadGrouping["ExactGroupingReconstruction"] === False];
heSpanExpired = ConstructiveDiagrams`Private`completionHECertifiedZeroRemainder[
  heSpanTarget, heSpanZeros, 0];
check["an expired certified-span budget preserves the original direction",
  heSpanExpired["Status"] === "UNCHANGED" && heSpanExpired["ReducedInput"] === heSpanTarget];
heSpanParameters = Table[Unique["spanCoordinate"], {4}];
heSpanDirections = Join[heSpanZeros, {heSpanTarget, heSpanTarget + 1}];
heSpanFamily = 7 + heSpanParameters.heSpanDirections;
heSpanEvents = {};
heSpanResult = Block[
  {ConstructiveDiagrams`Private`$completionParallelSettings =
      <|"Parallelization" -> False, "MaxKernels" -> 1|>,
   ConstructiveDiagrams`Private`completionHEZeroDirection =
     Function[{expression, rules, model, seconds}, Which[
       expression === 0, <|"Status" -> "PROVED_ZERO", "Method" -> "ExactRationalAlgebra"|>,
       MemberQ[heSpanZeros, expression],
         <|"Status" -> "PROVED_ZERO", "Method" -> "OrdinaryExternalIdentities"|>,
       True, <|"Status" -> "NOT_PROVED", "Method" -> "TimeConstraint"|>]]},
  ConstructiveDiagrams`Private`completionHEPruneIdentityFamily[
    heSpanFamily, heSpanParameters, heRules, heModel, 30, heSpanDirections,
    Function[event, AppendTo[heSpanEvents, event]]]];
check["a certified-span retry removes only the independently proved zero remainder",
  heSpanResult["RemovedParameters"] === Most[heSpanParameters] &&
    heSpanResult["FreeParameters"] === {Last[heSpanParameters]} &&
    heSpanResult["RemainingBasis"] === {Last[heSpanDirections]} &&
    heSpanResult["FamilyAmplitude"] === 7 + Last[heSpanParameters] Last[heSpanDirections]];
heSpanCertificate = heSpanResult["Certificates"][[3]];
check["reused certificates retain exact reconstruction and only initially proved prerequisites",
  heSpanResult["CertifiedZeroSpanRetry"]["PrerequisitesFromInitialBatch"] === {1, 2} &&
    heSpanCertificate["Method"] === "CertifiedZeroSpanRemainder" &&
    heSpanCertificate["ExactFormalReconstruction"] === True &&
    heSpanCertificate["ExactGroupingReconstruction"] === True &&
    heSpanCertificate["ReusedCertificate"]["Status"] === "PROVED_ZERO" &&
    AllTrue[heSpanCertificate["PrerequisiteCertifiedDirections"], #["Status"] === "PROVED_ZERO" &] &&
    heSpanResult["Certificates"][[4]]["CertifiedZeroSpanRetry"]["OriginalDirectionRetained"] === True];
heSpanRetryEvents = Select[heSpanEvents,
  Lookup[#, "Operation", None] === "Reusing certified ordinary identities" &];
check["certified-span progress counts both successful and retained retries",
  Lookup[heSpanRetryEvents, "Completed"] === {1, 2} &&
    Lookup[heSpanRetryEvents, "Total"] === {2, 2} &&
    Last[heSpanRetryEvents]["ProvedZero"] === 1 &&
    Last[heSpanRetryEvents]["RetainedDirections"] === 1];
heUncertifiedSpan = Block[
  {ConstructiveDiagrams`Private`$completionParallelSettings =
      <|"Parallelization" -> False, "MaxKernels" -> 1|>,
   ConstructiveDiagrams`Private`completionHEZeroDirection =
     Function[{expression, rules, model, seconds},
       If[expression === First[heSpanZeros],
         <|"Status" -> "PROVED_ZERO", "Method" -> "OrdinaryExternalIdentities"|>,
         <|"Status" -> "NOT_PROVED", "Method" -> "TimeConstraint"|>]]},
  ConstructiveDiagrams`Private`completionHEPruneIdentityFamily[
    heSpanFamily, heSpanParameters, heRules, heModel, 30, heSpanDirections]];
check["an unproved prerequisite cannot enter the certified span or erase a pending direction",
  heUncertifiedSpan["CertifiedZeroSpanRetry"]["PrerequisitesFromInitialBatch"] === {1} &&
    heUncertifiedSpan["FreeParameters"] === Rest[heSpanParameters] &&
    heUncertifiedSpan["RemainingBasis"] === Rest[heSpanDirections]];
heExpiredSpanFamily = ConstructiveDiagrams`Private`completionHEPruneIdentityFamily[
  heSpanFamily, heSpanParameters, heRules, heModel, 0, heSpanDirections];
check["an expired family deadline never starts a fresh certified-span proof budget",
  heExpiredSpanFamily["FreeParameters"] === heSpanParameters &&
    heExpiredSpanFamily["FamilyAmplitude"] === heSpanFamily &&
    heExpiredSpanFamily["CertifiedZeroSpanRetry"]["Attempted"] === 0];

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
        MemberQ[{"ExactRationalAlgebra", "OrdinaryExternalIdentities", "DenseSymbolicExternalChart",
          "CertifiedZeroSpanRemainder"}, #["Method"]] &&
          If[#["Method"] === "CertifiedZeroSpanRemainder",
            TrueQ[#["ExactFormalReconstruction"]] && TrueQ[#["ExactGroupingReconstruction"]] &&
              #["ReusedCertificate"]["Status"] === "PROVED_ZERO" &&
              AllTrue[#["PrerequisiteCertifiedDirections"], #["Status"] === "PROVED_ZERO" &], True] &&
          Lookup[#, "NumericalKinematicSamples", 0] === 0 &&
          Lookup[#, "InternalOnShellConditions", {}] === {} &]]],
  {process, {{"e", "E", "A.-", "A.+", "A.+"}, {"e", "E", "A.+", "A.-", "A.+"},
    {"e", "E", "E", "e", "A.-"}, {"e", "E", "E", "e", "A.+"}}}],
  Print["SKIP: set HEPCAT_TEST_FULL_HELICITY_CONSTRUCTION=1 for the four full notebook constructor regressions."]];
(* Exact notebook calls, with only replay and presentation controls added.
   No cached amplitude or process-specific correction basis is supplied. *)
heFreshNotebook = Environment["HEPCAT_TEST_NOTEBOOK_FOUR_FERMION"] === "1";
heFreshExportDirectory = Environment["HEPCAT_NOTEBOOK_VALIDATION_DIRECTORY"];
If[heFreshNotebook, Do[
  heFreshProcess = {"e", "E", "E", "e", heFreshCase[[1]]};
  heFreshResult = ConstructAmplitude[heFreshProcess, heDiagramModel, heModel,
    "FeynmanModel" -> heFeynmanModel, "RandomSeed" -> heFreshCase[[2]],
    "Output" -> "Details", "ProgressReporting" -> False, "PrintReport" -> False];
  check["the exact four-fermion photon notebook call completes with its replay seed: " <> heFreshCase[[1]],
    AssociationQ[heFreshResult] && heFreshResult["Status"] === "COMPLETE" &&
      heFreshResult["Checks"]["FeynmanCheck"]["Status"] === "PASS" &&
      AllTrue[heFreshResult["Checks"]["Checks"], #["Status"] === "PASS" &]];
  heFreshReduction = Lookup[Lookup[Lookup[heFreshResult, "Stages", <||>],
    "Completing high-energy behavior", <||>], "FamilyIdentityReduction", <||>];
  heFreshCertified = Select[Lookup[heFreshReduction, "Certificates", {}],
    Lookup[#, "Status", None] === "PROVED_ZERO" &];
  check["notebook family directions are removed only with exact external-identity certificates: " <> heFreshCase[[1]],
    ContainsAll[Lookup[heFreshCertified, "Parameter", {}], Lookup[heFreshReduction, "RemovedParameters", {}]] &&
      AllTrue[heFreshCertified,
      Lookup[#, "NumericalKinematicSamples", 0] === 0 &&
        Lookup[#, "InternalOnShellConditions", {}] === {} &]];
  If[StringQ[heFreshExportDirectory] && DirectoryQ[heFreshExportDirectory],
    heFreshTag = "qed5FourFermions" <> If[heFreshCase[[1]] === "A.+", "P", "M"];
    Export[FileNameJoin[{heFreshExportDirectory, heFreshTag <> "-fixed-report.wxf"}],
      KeyTake[heFreshResult, {"Status", "Amplitude", "Checks", "RandomSeed", "StageTimings", "Failure"}], "WXF"];
    If[Lookup[heFreshResult, "Status", None] === "COMPLETE",
      Export[FileNameJoin[{heFreshExportDirectory, heFreshTag <> "-fixed.wxf"}],
        heFreshResult["Amplitude"], "WXF"]]],
  {heFreshCase, {{"A.+", 724106912298138516}, {"A.-", 9158220563237508691}}}],
  Print["SKIP: set HEPCAT_TEST_NOTEBOOK_FOUR_FERMION=1 for the exact replayed notebook calls."]];
check["identity pruning and construction preserve both supplied models",
  heOriginalModels === {heDiagramModel, heModel}];
If[checks =!= 110 + If[heFullConstruction, 8, 0] + If[heFreshNotebook, 4, 0],
  AppendTo[failures, "Not every enabled assertion executed."]];
Print["CHECKS: ", checks, "; FAILURES: ", Length[failures]];
If[failures =!= {}, Print[failures]];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:high-energy-family-tests.wl"]];
Quit[If[failures === {}, 0, 1]];
