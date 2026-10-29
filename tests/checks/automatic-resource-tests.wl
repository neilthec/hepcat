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
arChannels = Table[<|"ExternalLegs" -> Range[6]|>, 14];
check["channel metadata can supply multiplicity before diagram preparation",
  arMetrics[0, arChannels]["ExternalParticles"] === 6 &&
    arMetrics[0, arChannels]["Channels"] === 14];
arRequests = <|"TimeConstraint" -> Automatic, "MaxCorrectionTerms" -> Automatic,
  "MaxSteps" -> Automatic, "Samples" -> Automatic, "ValidationSamples" -> Automatic,
  "MaxCandidates" -> Automatic, "MaxReferenceChoices" -> Automatic,
  "MaxRefinements" -> Automatic, "ProgressReporting" -> False|>;
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
check["the constructor's sample, time, pole and worker settings are automatic",
  AllTrue[{"TimeConstraint", "Samples", "ValidationSamples", "PolePreference", "Parallelization", "MaxKernels"},
    (# /. Options[ConstructAmplitude]) === Automatic &]];

(* Fresh public calls exercise the defaults without resource tuning, temporary
   caches, notebook outputs, known final amplitudes or global SetOptions. *)
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
arNumericFit = Block[{ConstructiveDiagrams`Private`$completionAutomaticFitSamples = True},
  CompleteFactorization[{arOverride["AmplitudePair"][[1]], 0},
    FactorizationAmplitudeChannels[arProcess4, arDiagramModel], arDiagramModel,
    "CorrectionBasis" -> (Last /@ arOverride["Stages"]["Preparing diagrams"]["PreparedAmplitudes"]),
    "Samples" -> 1, "ValidationSamples" -> 1, "TimeConstraint" -> 30, "MaxCorrectionTerms" -> 32,
    "ProgressReporting" -> False, "Parallelization" -> False, "ReturnDetails" -> True]];
check["fully numeric public calls isolate their samples from an enclosing automatic fit",
  AssociationQ[arNumericFit] && arNumericFit["Status"] === "COMPLETE" && arNumericFit["FitSamples"] === 1];
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
arExpected = 39 + If[Environment["HEPCAT_TEST_FULL_AUTOMATIC_CONSTRUCTION"] === "1", 2, 0] +
  If[Environment["HEPCAT_TEST_FULL_PHYSICAL_MIXED_CONSTRUCTION"] === "1", 3, 0];
If[checks =!= arExpected, AppendTo[failures, "Not every automatic-resource assertion executed."]];
Print["AUTOMATIC RESOURCE CHECKS: ", checks, "; FAILURES: ", Length[failures]];
If[failures =!= {}, Print[failures]];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:automatic-resource-tests.wl"]];
Quit[If[failures === {}, 0, 1]];
