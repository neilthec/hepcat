<|"Encoding" -> "StateUTF8AndNumericActions", 
 "Architecture" -> "CompactParticleActions", "DescriptorEncoding" -> 
  "ParticleRoleTokens", "DescriptorFeatureWidth" -> 166, 
 "NativeTokenCount" -> 14, "RuntimeEvaluation" -> 
  "PortableVariableParticleTokens", "CompactFeatureSchema" -> 
  <|"Encoding" -> "ParticleRoleTokens", "Width" -> 166, 
   "OverviewWidth" -> 146, "TokenKinds" -> {"Overview", "LegRole", 
     "ChainElement"}, "RoleCount" -> 12, "Coordinates" -> 
    {"CanonicalParticleID", "SubsetSize", "ChainPosition", "ChainLength", 
     "ParticleCount"}, "MaximumExternalLegs" -> Infinity, 
   "ParticleLabels" -> "ExistingStateLegIDs", "MomentumSubsets" -> 
    "EveryMember", "FocusedChains" -> "CompleteOrderedMomenta", 
   "Padding" -> "ZeroTokensMaskedBeforePooling", "WholeResultExpression" -> 
    False, "CandidateIndex" -> False, "LossyLocalSummaries" -> True|>, 
 "Objective" -> "LegalMoveCrossEntropy", "ReadoutNormalization" -> 
  "SeparateMeanMaxL2", "ReadoutSquaredNormEpsilon" -> 1.*^-12, 
 "RuntimeCoreSourceEncoding" -> "SectionedLegalMathAndParticleTokens", 
 "RuntimeCoreSourceSHA256" -> 
  "62ed48a7d3049905b07fd3cc4a28b2c05dc8d89d1a820440ca8e4bae4cde773a", 
 "OnShellChannels" -> Automatic, "MomentumConservation" -> True, 
 "TrainingProgress" -> <|"Status" -> "Complete", "ActualRounds" -> 20, 
   "ActualUpdates" -> 3740, "CompletedFullRounds" -> 20, 
   "ReasonTrainingStopped" -> "MaxTrainingRounds", 
   "TrainingSeconds" -> 93.255425, "WallSeconds" -> 93.379947, 
   "TrainingRows" -> 187, "TrainingActionCount" -> 80, 
   "TrainingTokenCount" -> 14, "FinalRoundLoss" -> 0.7158457899444679, 
   "OptimizerContinuity" -> "SingleNetTrainSession", 
   "CheckpointResumeSemantics" -> "WeightsOnly", 
   "Fit" -> <|"Rows" -> 187, "Correct" -> 164, 
     "Accuracy" -> 0.8770053475935828, "MeanLegalCrossEntropy" -> 
      0.6217988652880881, "NativeAssessmentTokenCount" -> 14, 
     "PortableUnpaddedAgreement" -> True|>, "HeldoutTeacherFit" -> 
    <|"Rows" -> 13, "Correct" -> 9, "Accuracy" -> 0.6923076923076923, 
     "MeanLegalCrossEntropy" -> 2.3419666183007593, 
     "NativeAssessmentTokenCount" -> 15, "PortableUnpaddedAgreement" -> 
      True|>|>, "SourceHashes" -> 
  <|"HEPCAT-Base.wl" -> 
    "86a5eca713a8e4d14d2f93e9db8d5d7a14b0f3d1bb66262d5396648143e53915", 
   "HEPCAT-Model.wl" -> 
    "26c4fb2f4faca5ebad1c527bcdbb29e3d357587b10b21f2269e50e5881ee5050", 
   "HEPCAT.wl" -> 
    "9750f767bbcb491b6f4aa852b7ecd743c4fd8f1ecf597ecd88575fa708b686ed", 
   "unscrambling.wl" -> 
    "94225760d80785f7a32e33c384eb6817b83ea8999bdab8747cf01066f13b17db", 
   "training-amplitudes.wl" -> 
    "0800e46676dfa4ebaa8acd84fa2ea75b4f61b1fceb857646237d93d0955218f0", 
   "compact-scale-data.wl" -> 
    "da4f85164a2ddf299eff0b5124ac49879c418b629e36cf8f86ea98167de6ca4c", 
   "compact-scale-training.wl" -> 
    "60edaad003d8555ee976d0d83912309636538dea54ff8296a2f22b730fdd57a2", 
   "compact-scale-run.wl" -> 
    "09f93e72de881733a57e8d4ef6ea7b09ebe3c7b7652c26b04c514e9418426d13"|>, 
 "DatasetHashEncoding" -> "SHA256File", 
 "DatasetHash" -> 
  "e040388ed4ac4df80da774b28df02d3fe5fa130cdd49b3e0ebd86f5ff7d5df25", 
 "Initialization" -> "Fresh", "RandomSeed" -> 1, 
 "ReferenceSplit" -> <|"TrainingFivePointIndices" -> 
    {196, 197, 198, 199, 200, 201, 204, 205, 206, 207, 208, 209, 212, 213, 
     214, 215, 216, 217, 220, 221, 222, 223, 224, 225}, 
   "HeldoutFivePointIndices" -> {232, 234, 240, 242}, 
   "ReplayFourPointIndices" -> {121, 125, 173, 118}, 
   "TrainingFamilies" -> {"Synthetic5Point / Massless", 
     "Synthetic5Point / FermionPair", "Synthetic5Point / TwoFermionPairs", 
     "Synthetic5Point / FourVectors"}, "HeldoutFamilies" -> 
    {"Synthetic5Point / MixedMassive", "Synthetic5Point / FiveMassive"}, 
   "FamilyDisjoint" -> True, "TemplateDisjoint" -> False, 
   "SyntheticInterpretation" -> 
    "Algebraic fixtures, not physical five-point amplitudes", 
   "SelectedReferences" -> 
    <|"Name" -> "Synthetic5Point / FiveMassive / InternalMomentumChain", 
     "Family" -> "Synthetic5Point / FiveMassive", 
     "Template" -> "InternalMomentumChain", "Synthetic" -> True, 
     "ExternalLegCount" -> 5|>|>, "TrainingActionCount" -> 80, 
 "TrainingTokenCount" -> 14|>
