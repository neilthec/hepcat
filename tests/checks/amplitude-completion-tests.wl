(* Bounded diagram/channel preparation and simultaneous amplitude completion. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
If[AnyTrue[{PrepareAmplitudes, CompleteFactorization, CompleteHighEnergy},
    DownValues[#] === {} &], Print["Amplitude-completion APIs did not load."]; Quit[1]];
Get[FileNameJoin[{root, "tests", "QED-5-point-residues.wl"}]];
checks = 0; failures = {};
check[name_, value_] := (checks++;
  If[Mod[checks, 25] === 0, Print["Completed ", checks, " amplitude-completion checks."]];
  If[!TrueQ[value], AppendTo[failures, name];
  Print["FAIL: ", name]]);
acZero[expression_] := Together[expression] === 0;
acInvalid[value_] := value === $Failed || FailureQ[value];
acSettings[opts___] := Normal[Join[<|"Samples" -> 2, "ValidationSamples" -> 2,
  "RandomSeed" -> 1729, "TimeConstraint" -> 60, "MaxCorrectionTerms" -> 64,
  "ProgressReporting" -> False, "Output" -> "Details"|>, Association[{opts}]]];
acFactor[input_, channels_, model_, opts___] := CompleteFactorization[
  input, channels, model, Sequence @@ acSettings[opts]];
acEnergy[input_, channels_, model_, opts___] := CompleteHighEnergy[
  input, channels, model, Sequence @@ acSettings[opts]];
acCheck[expression_, channels_, model_, rules_, seed_] := CheckPhysicalResidues[
  expression, channels, model, "Output" -> "Details", "Samples" -> 3,
  "RandomSeed" -> seed, "ParameterRules" -> rules, "TimeConstraint" -> 30,
  "HighEnergyCheck" -> False, "ReferenceCheck" -> False];
acPhysicalPass[report_] := AssociationQ[report] &&
  AllTrue[report["Channels"], #["Status"] === "PASS" &];
acAllPass[report_] := AssociationQ[report] &&
  AllTrue[report["Checks"], #["Status"] === "PASS" &];
acCanonical[expression_, n_] :=
  ConstructiveDiagrams`Private`residueCanonicalPropagators[expression, Range[n]];
check["all preparation and completion stages enable progress reporting by default",
  AllTrue[{PrepareAmplitudes, CompleteFactorization, CompleteHighEnergy},
    ("ProgressReporting" /. Options[#]) === True &]];

(* Preparation retains diagram-local assumptions and off-shell denominators. *)
acRules = {Mass[1] -> prElectronMass, Mass[2] -> prElectronMass, Mass[3] -> 0, Mass[4] -> 0,
  Mass[Multiparticle[1, 3]] -> prElectronMass};
acDen = PropDen[Mom[Multiparticle[1, 3]], prElectronMass];
acDiracPair = {acRules, SpinorChain[Spinor["Spin", "Angle", 1], Mom[1],
  Spinor["Spin", "Square", 2]]/acDen};
acSavedPair = acDiracPair; acSavedModel = prModel;
acPrepEvents = {};
acPrepared = PrepareAmplitudes[{acDiracPair}, prModel,
  "MaxSteps" -> 0, "ProgressReporting" -> True,
  "ProgressFunction" -> (AppendTo[acPrepEvents, #] &), "Output" -> "Details"];
check["preparation reduces an actual external Dirac chain before identity search",
  acPrepared["Status"] === "PREPARED" && acPrepared["Amplitude"] ===
    prElectronMass SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]/acDen];
check["preparation retains original amplitudes and the supplied model",
  acPrepared["OriginalAmplitudes"] === {acSavedPair} &&
    acDiracPair === acSavedPair && prModel === acSavedModel];
check["preparation records the external mass rules separately from numerator shells",
  acPrepared["ExternalMassRules"] === Take[acRules, 4] &&
    First[acPrepared["DiagramReports"]]["InternalOnShellChannels"] === {{1, 3}}];
check["preparation does not put the physical denominator on shell",
  !FreeQ[acPrepared["Amplitude"], acDen] &&
    TrueQ[First[acPrepared["DiagramReports"]]["DenominatorsKeptOffShell"]]];
check["preparation progress callbacks receive structured live events",
  Length[acPrepEvents] >= 3 && AllTrue[acPrepEvents,
    AssociationQ[#] && ContainsAll[Keys[#], {"Stage", "Diagram", "Complexity", "Terms"}] &]];
acPreparedPairs = PrepareAmplitudes[{acDiracPair}, prModel,
  "MaxSteps" -> 0, "ProgressReporting" -> False];
check["the default preparation output remains ordinary amplitude pairs",
  acPreparedPairs === acPrepared["PreparedAmplitudes"]];
acExplicitPreparedPairs = PrepareAmplitudes[{acDiracPair}, prModel,
  "MaxSteps" -> 0, "ProgressReporting" -> False, "Output" -> "Amplitudes"];
check["explicit Amplitudes output retains compatible prepared pairs",
  acExplicitPreparedPairs === acPreparedPairs];
check["explicit Expression output remains the ordinary summed expression",
  PrepareAmplitudes[{acDiracPair}, prModel, "MaxSteps" -> 0,
    "ProgressReporting" -> False, "Output" -> "Expression"] === acPrepared["Amplitude"]];
check["detailed preparation output is opt-in by default",
  ("ReturnDetails" /. Options[PrepareAmplitudes]) === False &&
    ("Output" /. Options[PrepareAmplitudes]) === "Amplitudes"];
check["the previous preparation name delegates to the same implementation and options",
  Options[PrepareDiagramAmplitudes] === Options[PrepareAmplitudes] &&
    PrepareDiagramAmplitudes[{acDiracPair}, prModel, "MaxSteps" -> 0,
      "ProgressReporting" -> False] === acPreparedPairs];
check["ReturnDetails False returns ordinary prepared pairs",
  PrepareAmplitudes[{acDiracPair}, prModel, "MaxSteps" -> 0,
    "ProgressReporting" -> False, "ReturnDetails" -> False] === acPreparedPairs];
acReturnDetails = PrepareAmplitudes[{acDiracPair}, prModel, "MaxSteps" -> 0,
  "ProgressReporting" -> False, "ReturnDetails" -> True];
check["ReturnDetails True exposes the retained inputs and preparation history",
  AssociationQ[acReturnDetails] && acReturnDetails["Amplitude"] === acPrepared["Amplitude"] &&
    acReturnDetails["OriginalAmplitudes"] === {acDiracPair} &&
    First[acReturnDetails["DiagramReports"]]["Reductions"] =!= {}];
check["ReturnDetails True takes precedence over legacy Output choices",
  AssociationQ[PrepareAmplitudes[{acDiracPair}, prModel, "MaxSteps" -> 0,
    "ProgressReporting" -> False, "ReturnDetails" -> True, "Output" -> "Amplitudes"]]];
acXiPair = {acRules, SpinorChain[Spinor["Xi", "Angle", 71],
  Spinor["Helicity", "Angle", 3]]/
    (SpinorChain[Spinor["Xi", "Angle", 72], Spinor["Helicity", "Angle", 3]] acDen)};
acXiPrepared = Last[First[PrepareAmplitudes[{acXiPair}, prModel,
  "ReferenceSpinors" -> None, "MaxSteps" -> 0, "ProgressReporting" -> False]]];
check["explicit None preserves distinct reference spinors during preparation",
  Sort[DeleteDuplicates[Cases[acXiPrepared, Spinor["Xi", kind_, id_] :> {kind, id}, Infinity]]] ===
    {{"Angle", 71}, {"Angle", 72}} &&
    ReduceSpinorProducts[acXiPrepared - Last[acXiPair]] === 0];
acReferencePrepare[pairs_, choice_, opts___] := PrepareAmplitudes[pairs, prModel,
  opts, "ReferenceSpinors" -> choice, "MaxSteps" -> 0,
  "ProgressReporting" -> False, "Output" -> "Details"];
acReferenceAssignments[report_] := First[report["DiagramReports"]]["ReferenceSpinors"]["Assignments"];
acReferenceRatio[kind_, id_, leg_, momentum_] := SpinorChain[
  Spinor["Xi", kind, id], Mom[momentum],
  Spinor["Helicity", If[kind === "Angle", "Square", "Angle"], leg]]/
    SpinorChain[Spinor["Xi", kind, id], Spinor["Helicity", kind, leg]];
acReferenceRules = {Mass[1] -> prElectronMass, Mass[2] -> prElectronMass,
  Mass[3] -> 0, Mass[4] -> 0, Mass[5] -> 0,
  Mass[Multiparticle[1, 3]] -> prElectronMass};
acReferenceRaw = acReferenceRatio["Angle", 10, 5, 1] *
  acReferenceRatio["Angle", 11, 3, 1] acReferenceRatio["Square", 12, 4, 2]/acDen;
acReferencePair = {acReferenceRules, acReferenceRaw};
acReferenceSpurious = acReferencePrepare[{acReferencePair}, "AllSpurious"];
acReferenceCyclic = acReferencePrepare[{acReferencePair}, "Cyclic"];
acReferenceCycle = {Spinor["Xi", "Angle", 10] -> Spinor["Helicity", "Angle", 3],
  Spinor["Xi", "Angle", 11] -> Spinor["Helicity", "Angle", 4],
  Spinor["Xi", "Square", 12] -> Spinor["Helicity", "Square", 5]};
check["cyclic reference choices follow diagram-local vertex order rather than sorted particle labels",
  Sort[({#["Leg"], #["Target"]} &) /@ acReferenceAssignments[acReferenceCyclic]] ===
    {{3, 4}, {4, 5}, {5, 3}}];
check["massless reference substitution preserves both angle and square chirality",
  acReferenceCyclic["Status"] === "PREPARED" &&
    FreeQ[acReferenceCyclic["Amplitude"], Spinor["Xi", ___]] &&
    ReduceSpinorProducts[acReferenceCyclic["Amplitude"] -
      (acReferenceRaw /. acReferenceCycle)] === 0];
acReferenceAutomatic = acReferencePrepare[{acReferencePair}, Automatic];
check["Automatic uses supported spurious choices first",
  acReferenceAutomatic["Amplitude"] === acReferenceSpurious["Amplitude"] &&
    AllTrue[acReferenceAssignments[acReferenceAutomatic], #["Mode"] === "AllSpurious" &]];
check["the default reference-spinor choice is Automatic",
  ("ReferenceSpinors" /. Options[PrepareAmplitudes]) === Automatic];
acReferenceAlias = acReferencePrepare[{acReferencePair}, "Cyclic"];
check["Cyclic retains the deterministic forward reference assignment",
  acReferenceAlias["Amplitude"] === acReferenceCyclic["Amplitude"]];
check["optimized AllSpurious is no more complex than the fixed cyclic assignment",
  SpinorExpressionComplexity[acReferenceSpurious["Amplitude"]] <=
    SpinorExpressionComplexity[acReferenceCyclic["Amplitude"]] &&
    AllTrue[acReferenceAssignments[acReferenceSpurious], #["Mode"] === "AllSpurious" &] &&
    FreeQ[acReferenceSpurious["Amplitude"], Spinor["Xi", ___]]];
acReferenceReverse = acReferencePrepare[{acReferencePair}, "ReverseCyclic"];
check["the reverse cyclic choice takes the previous distinct massless external leg",
  Sort[({#["Leg"], #["Target"]} &) /@ acReferenceAssignments[acReferenceReverse]] ===
    {{3, 5}, {4, 3}, {5, 4}}];
acReferenceShared = acReferencePrepare[{acReferencePair}, "Xi"];
check["Xi chooses a shared generic reference without changing its chirality",
  Sort[DeleteDuplicates[Cases[acReferenceShared["Amplitude"],
    Spinor["Xi", kind_, id_] :> {kind, id}, Infinity]]] ===
      {{"Angle", 0}, {"Square", 0}}];
acReferenceIndependent = acReferencePrepare[{acReferencePair}, "IndependentXi"];
check["IndependentXi records one generic reference per external massless leg",
  Sort[({#["Leg"], #["Target"]} &) /@ acReferenceAssignments[acReferenceIndependent]] ===
    {{3, 3}, {4, 4}, {5, 5}} &&
    Sort[DeleteDuplicates[Cases[acReferenceIndependent["Amplitude"],
      Spinor["Xi", _, id_] :> id, Infinity]]] === {3, 4, 5}];
check["reference selection occurs between spin contraction and spinor-product reduction",
  With[{operations = Lookup[First[acReferenceSpurious["DiagramReports"]]["Reductions"], "Operation"]},
    ContainsAll[operations, {"ReduceSpinContractions", "ReferenceSpinors", "ReduceSpinorProducts"}] &&
      FirstPosition[operations, "ReduceSpinContractions"][[1]] <
        FirstPosition[operations, "ReferenceSpinors"][[1]] <
        FirstPosition[operations, "ReduceSpinorProducts"][[1]]]];
check["reference preparation retains its original input and off-shell physical denominator",
  acReferenceSpurious["OriginalAmplitudes"] === {acReferencePair} &&
    !FreeQ[acReferenceSpurious["Amplitude"], acDen] &&
    First[acReferenceSpurious["DiagramReports"]]["DenominatorsKeptOffShell"] === True];
acReferenceBlockedPairs = {acReferencePair,
  {Append[Take[acReferenceRules, 5], Mass[Multiparticle[3, 4]] -> 0], 1}};
acReferenceBlocked = acReferencePrepare[acReferenceBlockedPairs, "AllSpurious"];
check["a massless bracket belonging to a certified physical channel is not called spurious",
  SelectFirst[acReferenceAssignments[acReferenceBlocked], #["Leg"] === 3 &]["Target"] === 5];

(* Physical reference choices are certified by other supplied diagram cuts. *)
acReferenceExternal = Take[acRules, 4];
acReferencePhysicalPairs = {
  {Append[acReferenceExternal, Mass[Multiparticle[1, 3]] -> prElectronMass],
    acReferenceRatio["Angle", 31, 3, 1]/acDen},
  {Append[acReferenceExternal, Mass[Multiparticle[1, 4]] -> prElectronMass],
    acReferenceRatio["Square", 32, 4, 1]/PropDen[Mom[Multiparticle[1, 4]], prElectronMass]}};
acReferencePhysical = acReferencePrepare[acReferencePhysicalPairs, "AllPhysical"];
acPhysicalAssignments = Flatten[Lookup[
  Lookup[acReferencePhysical["DiagramReports"], "ReferenceSpinors"], "Assignments"], 1];
check["physical reference choices use other certified poles rather than each diagram's own cut",
  acReferencePhysical["Status"] === "PREPARED" &&
    Sort[({#["Leg"], #["Target"]} &) /@ acPhysicalAssignments] === {{3, 2}, {4, 2}} &&
    AllTrue[acPhysicalAssignments, #["Mode"] === "AllPhysical" &]];
check["physical choices replace Xi by opposite-chirality spinors and momentum insertions",
  FreeQ[acReferencePhysical["Amplitude"], Spinor["Xi", ___]] &&
    !FreeQ[First[acReferencePhysical["DiagramReports"]]["ReferenceSpinors"]["Expression"],
      SpinorChain[Spinor["Helicity", "Square", 3], Mom[2], Mom[1],
        Spinor["Helicity", "Square", 3]]] &&
    !FreeQ[Last[acReferencePhysical["DiagramReports"]]["ReferenceSpinors"]["Expression"],
      SpinorChain[Spinor["Helicity", "Angle", 4], Mom[2], Mom[1],
        Spinor["Helicity", "Angle", 4]]]];
check["only the newly inserted auxiliary self-sandwiches become certified physical denominators",
  And @@ MapThread[Function[{pair, auxiliary},
      !FreeQ[Last[pair], auxiliary]],
    {acReferencePhysical["PreparedAmplitudes"],
      {PropDen[Mom[Multiparticle[2, 3]], prElectronMass], PropDen[Mom[Multiparticle[2, 4]], prElectronMass]}}] &&
    And @@ MapThread[Function[{pair, originalDenominator}, !FreeQ[Last[pair], originalDenominator]],
      {acReferencePhysical["PreparedAmplitudes"],
        {acDen, PropDen[Mom[Multiparticle[1, 4]], prElectronMass]}}]];
acReferenceUncertified = acReferencePrepare[{First[acReferencePhysicalPairs]}, "AllPhysical"];
check["unsupported physical reference choices remain partial instead of inventing a pole",
  acReferenceUncertified["Status"] === "PARTIAL" &&
    First[acReferenceUncertified["DiagramReports"]]["ReferenceSpinors"]["Status"] === "UNRESOLVED"];
acReferenceSingleExternal = Join[Thread[(Mass /@ Range[4]) -> ConstantArray[prElectronMass, 4]],
  {Mass[5] -> 0}];
acReferenceSinglePairs = {
  {Append[acReferenceSingleExternal, Mass[Multiparticle[1, 5]] -> prElectronMass],
    acReferenceRatio["Angle", 41, 5, 3]/PropDen[Mom[Multiparticle[1, 5]], prElectronMass]},
  {Append[acReferenceSingleExternal, Mass[Multiparticle[2, 5]] -> prElectronMass],
    acReferenceRatio["Square", 42, 5, 4]/PropDen[Mom[Multiparticle[2, 5]], prElectronMass]}};
acReferenceSingle = acReferencePrepare[acReferenceSinglePairs, Automatic];
check["Automatic falls back to certified physical references when only one external leg is massless",
  acReferenceSingle["Status"] === "PREPARED" &&
    Flatten[Lookup[Lookup[acReferenceSingle["DiagramReports"], "ReferenceSpinors"], "Assignments"], 1][[All, "Target"]] ===
      {2, 1} && FreeQ[acReferenceSingle["Amplitude"], Spinor["Xi", ___]]];
acReferenceSingleSpurious = acReferencePrepare[acReferenceSinglePairs, "AllSpurious"];
check["AllSpurious with a single massless leg never creates a zero self-bracket",
  acReferenceSingleSpurious["Status"] === "PARTIAL" &&
    FreeQ[acReferenceSingleSpurious["Amplitude"], Indeterminate | ComplexInfinity | _DirectedInfinity]];
check["unsupported reference choices are rejected before preparation",
  acInvalid[Quiet[PrepareAmplitudes[{acReferencePair}, prModel,
    "ReferenceSpinors" -> "Random", "ProgressReporting" -> False]]]];

(* Reference-dependent toy numerators isolate assignment optimization. They are
   not assertions that arbitrary Xi-dependent inputs are physical amplitudes. *)
acOptimizedSpuriousRatio = SpinorChain[Spinor["Xi", "Angle", 61],
    Spinor["Helicity", "Angle", 5]]/
  SpinorChain[Spinor["Xi", "Angle", 61], Spinor["Helicity", "Angle", 3]];
acOptimizedSpuriousPair = {acReferenceRules, (1 + acOptimizedSpuriousRatio)/acDen};
acOptimizedCyclic = acReferencePrepare[{acOptimizedSpuriousPair}, "Cyclic"];
acOptimizedSpurious = acReferencePrepare[{acOptimizedSpuriousPair}, "AllSpurious"];
check["AllSpurious tests a noncyclic reference that reduces the actual numerator",
  acOptimizedSpurious["Status"] === "PREPARED" &&
    acOptimizedSpurious["Amplitude"] === 1/acDen &&
    First[acReferenceAssignments[acOptimizedSpurious]]["Target"] === 5 &&
    First[acReferenceAssignments[acOptimizedCyclic]]["Target"] === 4 &&
    SpinorExpressionComplexity[acOptimizedSpurious["Amplitude"]] <
      SpinorExpressionComplexity[acOptimizedCyclic["Amplitude"]]];
acOptimizedSpuriousAgain = acReferencePrepare[{acOptimizedSpuriousPair}, "AllSpurious"];
check["optimized reference selection is deterministic on repeated identical input",
  acOptimizedSpuriousAgain["Amplitude"] === acOptimizedSpurious["Amplitude"] &&
    acReferenceAssignments[acOptimizedSpuriousAgain] ===
      acReferenceAssignments[acOptimizedSpurious]];
check["reference optimization retains the original pair and its off-shell physical pole",
  acOptimizedSpurious["OriginalAmplitudes"] === {acOptimizedSpuriousPair} &&
    acOptimizedSpurious["PreparedAmplitudes"][[1, 1]] === First[acOptimizedSpuriousPair] &&
    !FreeQ[acOptimizedSpurious["Amplitude"], acDen]];
acReferenceSearch[report_] := First[report["DiagramReports"]]["ReferenceSpinors"]["Search"];
check["reference optimization has finite default choices and an automatic shared time sub-budget",
  ("MaxReferenceChoices" /. Options[PrepareAmplitudes]) === 64 &&
    ("ReferenceTimeConstraint" /. Options[PrepareAmplitudes]) === Automatic];
check["optimized reference details expose actual reduction scores and a bounded search inventory",
  With[{reference = First[acOptimizedSpurious["DiagramReports"]]["ReferenceSpinors"]},
    ContainsAll[Keys[reference], {"ReducedExpression", "ReducedRank", "Search", "Alternatives"}] &&
      reference["ReducedExpression"] === 1/acDen &&
      reference["ReducedRank"][[1]] === SpinorExpressionComplexity[reference["ReducedExpression"]] &&
      reference["Search"]["AvailableChoices"] === 2 &&
      reference["Search"]["CompletedChoices"] === reference["Search"]["TriedChoices"] &&
      TrueQ[reference["Search"]["Exhaustive"]] &&
      OrderedQ[Lookup[reference["Alternatives"], "Rank"]]]];
acOptimizedSpuriousLimited = acReferencePrepare[{acOptimizedSpuriousPair}, "AllSpurious",
  "MaxReferenceChoices" -> 1];
check["a reference-choice cap retains a valid best-so-far assignment without claiming exhaustive search",
  acOptimizedSpuriousLimited["Status"] === "PREPARED" &&
    acReferenceSearch[acOptimizedSpuriousLimited]["TriedChoices"] === 1 &&
    acReferenceSearch[acOptimizedSpuriousLimited]["CompletedChoices"] === 1 &&
    !TrueQ[acReferenceSearch[acOptimizedSpuriousLimited]["Exhaustive"]] &&
    acReferenceSearch[acOptimizedSpuriousLimited]["AvailableChoices"] === 2 &&
    FreeQ[acOptimizedSpuriousLimited["Amplitude"], Spinor["Xi", ___]]];
acLargeReferenceChoices = Table[Table[
  <|"Leg" -> leg, "Mode" -> "AllSpurious", "Target" -> target|>,
  {target, 10}], {leg, 10}];
acLargeReferencePlan = TimeConstrained[
  ConstructiveDiagrams`Private`preparationReferenceSelections[acLargeReferenceChoices, 3],
  2, $TimedOut];
check["a small reference cap never materializes the exponentially large Cartesian product",
  AssociationQ[acLargeReferencePlan] &&
    acLargeReferencePlan["AvailableChoices"] === 10^10 &&
    Length[acLargeReferencePlan["Selections"]] === 3 &&
    Length[DeleteDuplicates[acLargeReferencePlan["Selections"]]] === 3 &&
    First[acLargeReferencePlan["Selections"]] === First /@ acLargeReferenceChoices &&
    AllTrue[acLargeReferencePlan["Selections"], Function[selection,
      And @@ MapThread[MemberQ, {acLargeReferenceChoices, selection}]]]];
acOptimizedSharedLegPair = {acReferenceRules,
  (1 + acOptimizedSpuriousRatio + SpinorChain[Spinor["Xi", "Square", 65],
      Spinor["Helicity", "Square", 5]]/
    SpinorChain[Spinor["Xi", "Square", 65], Spinor["Helicity", "Square", 3]])/acDen};
acOptimizedSharedLeg = acReferencePrepare[{acOptimizedSharedLegPair}, "AllSpurious"];
check["different Xi labels and chiralities for one photon share one reference choice",
  acOptimizedSharedLeg["Amplitude"] === 1/acDen &&
    Length[acReferenceAssignments[acOptimizedSharedLeg]] === 2 &&
    DeleteDuplicates[({#["Leg"], #["Target"], #["Mode"]} &) /@
      acReferenceAssignments[acOptimizedSharedLeg]] === {{3, 5, "AllSpurious"}} &&
    acReferenceSearch[acOptimizedSharedLeg]["AvailableChoices"] === 2];

acOptimizedPhysicalExternal = Join[
  Thread[(Mass /@ Range[4]) -> ConstantArray[prElectronMass, 4]], {Mass[5] -> 0}];
acOptimizedPhysicalDen = PropDen[Mom[Multiparticle[1, 5]], prElectronMass];
acOptimizedPhysicalPair = {
  Append[acOptimizedPhysicalExternal, Mass[Multiparticle[1, 5]] -> prElectronMass],
  (1 + acReferenceRatio["Angle", 62, 5, 3])/acOptimizedPhysicalDen};
acOptimizedPhysicalPairs = Join[{acOptimizedPhysicalPair}, Table[{
  Append[acOptimizedPhysicalExternal, Mass[Multiparticle[leg, 5]] -> prElectronMass], 1}, {leg, {2, 3}}]];
acOptimizedPhysical = acReferencePrepare[acOptimizedPhysicalPairs, "AllPhysical"];
acOptimizedPhysicalLimited = acReferencePrepare[acOptimizedPhysicalPairs, "AllPhysical",
  "MaxReferenceChoices" -> 1];
check["AllPhysical compares allowed targets instead of always taking the first massive particle",
  acOptimizedPhysical["Status"] === "PREPARED" &&
    First[acReferenceAssignments[acOptimizedPhysical]]["Target"] === 3 &&
    First[acReferenceAssignments[acOptimizedPhysicalLimited]]["Target"] === 2 &&
    First[acOptimizedPhysical["DiagramReports"]]["PreparedExpression"] ===
      1/acOptimizedPhysicalDen &&
    SpinorExpressionComplexity[First[acOptimizedPhysical["DiagramReports"]]["PreparedExpression"]] <
      SpinorExpressionComplexity[First[acOptimizedPhysicalLimited["DiagramReports"]]["PreparedExpression"]]];
check["optimized physical references retain each original diagram's own shell context",
  First /@ acOptimizedPhysical["PreparedAmplitudes"] === First /@ acOptimizedPhysicalPairs &&
    First[acOptimizedPhysical["DiagramReports"]]["InternalOnShellChannels"] === {{1, 5}} &&
    !FreeQ[First[acOptimizedPhysical["DiagramReports"]]["PreparedExpression"], acOptimizedPhysicalDen]];

acOptimizedMixedRatio = acReferenceRatio["Angle", 64, 4, 2];
acOptimizedMixedPair = {acReferenceRules,
  (1 + acReferenceWeightA acOptimizedSpuriousRatio +
    acReferenceWeightB acOptimizedMixedRatio)/acDen};
acOptimizedMixedPairs = Join[{acOptimizedMixedPair}, Table[{
  Append[Take[acReferenceRules, 5], Mass[Multiparticle[2, leg]] -> prElectronMass], 1}, {leg, {3, 4}}]];
acOptimizedMixed = acReferencePrepare[acOptimizedMixedPairs, "Mixed"];
acOptimizedMixedSpurious = acReferencePrepare[acOptimizedMixedPairs, "AllSpurious"];
acOptimizedMixedPhysical = acReferencePrepare[acOptimizedMixedPairs, "AllPhysical"];
check["Mixed can choose a genuinely mixed assignment after reducing the whole numerator",
  acOptimizedMixed["Status"] === "PREPARED" &&
    First[acOptimizedMixed["DiagramReports"]]["PreparedExpression"] === 1/acDen &&
    Sort[Lookup[acReferenceAssignments[acOptimizedMixed], "Mode"]] ===
      {"AllPhysical", "AllSpurious"} &&
    First[acOptimizedMixedSpurious["DiagramReports"]]["PreparedExpression"] =!= 1/acDen &&
    First[acOptimizedMixedPhysical["DiagramReports"]]["PreparedExpression"] =!= 1/acDen];
acOptimizedAllSpuriousMixed = acReferencePrepare[{acOptimizedSpuriousPair}, "Mixed"];
check["Mixed also permits an all-spurious winner when that is best",
  acOptimizedAllSpuriousMixed["Amplitude"] === 1/acDen &&
    First[acReferenceAssignments[acOptimizedAllSpuriousMixed]]["Mode"] === "AllSpurious"];
acOptimizedAllPhysicalMixed = acReferencePrepare[acOptimizedPhysicalPairs, "Mixed"];
check["Mixed also permits an all-physical winner when no spurious partner exists",
  First[acOptimizedAllPhysicalMixed["DiagramReports"]]["PreparedExpression"] ===
    1/acOptimizedPhysicalDen &&
    First[acReferenceAssignments[acOptimizedAllPhysicalMixed]]["Mode"] === "AllPhysical"];
acOptimizedPhysicalCompetitionPairs = {
  {acReferenceRules, (1 + acReferenceRatio["Angle", 66, 3, 2])/acDen},
  {Append[Take[acReferenceRules, 5], Mass[Multiparticle[2, 3]] -> prElectronMass], 1}};
acOptimizedPhysicalCompetition = acReferencePrepare[acOptimizedPhysicalCompetitionPairs, "Mixed"];
check["Mixed can prefer an all-physical winner even when spurious partners are available",
  First[acOptimizedPhysicalCompetition["DiagramReports"]]["PreparedExpression"] === 1/acDen &&
    First[acReferenceAssignments[acOptimizedPhysicalCompetition]]["Mode"] === "AllPhysical" &&
    First[acReferenceAssignments[acOptimizedPhysicalCompetition]]["Target"] === 2];
check["a new reference mode does not change an input with no Xi spinors",
  PrepareAmplitudes[{acDiracPair}, prModel, "ReferenceSpinors" -> "Mixed",
    "MaxSteps" -> 0, "ProgressReporting" -> False] === acPreparedPairs];
check["an infinite reference sub-budget remains valid for an input with no Xi spinors",
  PrepareAmplitudes[{acDiracPair}, prModel, "ReferenceSpinors" -> "Mixed",
    "ReferenceTimeConstraint" -> Infinity, "MaxSteps" -> 0,
    "ProgressReporting" -> False] === acPreparedPairs];
acReferenceOptimizationEvents = {};
acReferenceProgress = PrepareAmplitudes[{acOptimizedSpuriousPair}, prModel,
  "ReferenceSpinors" -> "AllSpurious", "MaxSteps" -> 0,
  "ProgressReporting" -> True, "ProgressFunction" -> (AppendTo[acReferenceOptimizationEvents, #] &),
  "ReturnDetails" -> True];
acReferenceChoosingEvents = Select[acReferenceOptimizationEvents,
  Lookup[#, "Operation", ""] === "Choosing references" &];
check["reference optimization reports live tried-choice counts and best known complexity",
  acReferenceChoosingEvents =!= {} &&
    AllTrue[acReferenceChoosingEvents,
      ContainsAll[Keys[#], {"Completed", "Total", "Complexity", "Terms"}] &&
        #["Completed"] <= #["Total"] &] &&
    OrderedQ[Lookup[acReferenceChoosingEvents, "Completed"]] &&
    Last[acReferenceChoosingEvents]["Completed"] ===
      acReferenceSearch[acReferenceProgress]["TriedChoices"]];
acReferencePostSelectionPaused = False;
acReferencePostSelectionTimed = PrepareAmplitudes[{acOptimizedSpuriousPair}, prModel,
  "ReferenceSpinors" -> "AllSpurious", "MaxSteps" -> 0,
  "ReferenceTimeConstraint" -> Infinity, "TimeConstraint" -> 1,
  "ProgressReporting" -> True, "ReturnDetails" -> True,
  "ProgressFunction" -> (If[Lookup[#, "Operation", ""] === "ReduceSpinorProducts",
    acReferencePostSelectionPaused = True; Pause[1.1]] &)];
check["an overall timeout after reference selection retains the completed reduced winning trial",
  TrueQ[acReferencePostSelectionPaused] &&
    acReferencePostSelectionTimed["Status"] === "PARTIAL" &&
    acReferencePostSelectionTimed["Amplitude"] === acOptimizedSpurious["Amplitude"] &&
    acReferenceAssignments[acReferencePostSelectionTimed] ===
      acReferenceAssignments[acOptimizedSpurious] &&
    With[{diagram = First[acReferencePostSelectionTimed["DiagramReports"]]},
      diagram["StopReason"] === "TimeConstraint" &&
        diagram["PreparedExpression"] === diagram["ReferenceSpinors"]["ReducedExpression"] &&
        MemberQ[Lookup[diagram["Reductions"], "Operation"], "RetainedReferenceTrial"]] &&
    !FreeQ[acReferencePostSelectionTimed["Amplitude"], acDen]];
acReferenceSubTimed = acReferencePrepare[{acOptimizedSpuriousPair}, "AllSpurious",
  "ReferenceTimeConstraint" -> 0.000001];
check["reference sub-timeouts retain a valid fallback or an honestly partial original input",
  acReferenceSubTimed["OriginalAmplitudes"] === {acOptimizedSpuriousPair} &&
    !TrueQ[acReferenceSearch[acReferenceSubTimed]["Exhaustive"]] &&
    acReferenceSearch[acReferenceSubTimed]["CompletedChoices"] <=
      acReferenceSearch[acReferenceSubTimed]["TriedChoices"] &&
    FreeQ[acReferenceSubTimed["Amplitude"], Indeterminate | ComplexInfinity | _DirectedInfinity] &&
    If[acReferenceSubTimed["Status"] === "PREPARED",
      FreeQ[acReferenceSubTimed["Amplitude"], Spinor["Xi", ___]] &&
        First[acReferenceSubTimed["DiagramReports"]]["ReferenceSpinors"]["Status"] === "COMPLETE" &&
        If[acReferenceSearch[acReferenceSubTimed]["CompletedChoices"] === 0,
          First[acReferenceSubTimed["DiagramReports"]]["ReferenceSpinors"]["Diagnostics"] =!= {},
          True],
      acReferenceSubTimed["Status"] === "PARTIAL"]];
acMasslessScalarModel = {{{"Scalar", "s", "s", 1, 0, 0, 0, 1, 1, "", ""}},
  {}, {}, {}};
acExternalZero = Thread[(Mass /@ Range[4]) -> ConstantArray[0, 4]];
acLocalPairs = Table[{Append[acExternalZero, Mass[Multiparticle[1, 2]] -> mass],
  (3 + MomProd[1, 2])/PropDen[Mom[Multiparticle[1, 2]], mass]}, {mass, {1, 2}}];
acLocal = PrepareAmplitudes[acLocalPairs, acMasslessScalarModel,
  "Output" -> "Details", "MaxSteps" -> 4, "ProgressReporting" -> False];
check["the same numerator uses only its own diagram's internal mass condition",
  Last /@ acLocal["PreparedAmplitudes"] === {
    7/(2 PropDen[Mom[Multiparticle[1, 2]], 1]),
    5/PropDen[Mom[Multiparticle[1, 2]], 2]}];
check["each accepted internal-shell reduction is retained in the preparation history",
  AllTrue[acLocal["DiagramReports"], Function[report,
    AnyTrue[Join[report["Reductions"], report["IdentitySteps"]],
      TrueQ[Lookup[#, "InternalOnShellIdentity", False]] ||
        Lookup[#, "Name", None] === "OnShell" &]]]];
acLocalOff = PrepareAmplitudes[acLocalPairs, acMasslessScalarModel,
  "InternalOnShell" -> False, "MaxSteps" -> 4, "ProgressReporting" -> False,
  "Output" -> "Details"];
check["internal numerator-shell reductions can be disabled independently",
  AllTrue[Last /@ acLocalOff["PreparedAmplitudes"], !FreeQ[#, _MomProd | _SpinorChain] &] &&
    AllTrue[acLocalOff["DiagramReports"], #["InternalOnShellChannels"] === {} &&
      AllTrue[#["IdentitySteps"], #["Name"] =!= "OnShell" &] &]];
Do[
  acConflictingRules = Join[acExternalZero,
    {Mass[Multiparticle[1, 2]] -> 1, Mass[cut] -> 2}];
  check["contradictory diagram-shell masses are rejected before identity search " <> ToString[cut, InputForm],
    Quiet[PrepareAmplitudes[{{acConflictingRules,
      MomProd[1, 2]/PropDen[Mom[Multiparticle[1, 2]], 1]}}, acMasslessScalarModel,
      "ReferenceSpinors" -> None, "MaxSteps" -> 4,
      "ProgressReporting" -> False]] === $Failed],
  {cut, {Multiparticle[1, 2], Multiparticle[2, 1]}}];
check["contradictory external masses are rejected before applying external equations of motion",
  Quiet[PrepareAmplitudes[{{Append[acExternalZero, Mass[1] -> 1],
    SpinorChain[Spinor["Spin", "Angle", 1], Mom[1], Spinor["Spin", "Square", 2]]}},
    acMasslessScalarModel, "ReferenceSpinors" -> None,
    "ProgressReporting" -> False]] === $Failed];
acEquivalentShellRules = Join[acExternalZero,
  {Mass[Multiparticle[1, 2]] -> acShellParameter (acShellParameter + 1),
    Mass[Multiparticle[2, 1]] -> acShellParameter^2 + acShellParameter}];
acEquivalentShellPair = {acEquivalentShellRules,
  MomProd[1, 2]/PropDen[Mom[Multiparticle[1, 2]], acShellParameter (acShellParameter + 1)]};
acEquivalentShellPrepared = PrepareAmplitudes[{acEquivalentShellPair}, acMasslessScalarModel,
  "ReferenceSpinors" -> None, "MaxSteps" -> 0,
  "ProgressReporting" -> False, "ReturnDetails" -> True];
check["algebraically equal reversed-cut mass declarations are accepted without mutating input metadata",
  acEquivalentShellPrepared["Status"] === "PREPARED" &&
    acZero[acEquivalentShellPrepared["Amplitude"] - acShellParameter^2 (acShellParameter + 1)^2/
      (2 PropDen[Mom[Multiparticle[1, 2]], acShellParameter (acShellParameter + 1)])] &&
    First[First[acEquivalentShellPrepared["PreparedAmplitudes"]]] === acEquivalentShellRules &&
    acEquivalentShellPrepared["OriginalAmplitudes"] === {acEquivalentShellPair}];
acDuplicateExternalRules = Join[acExternalZero, {Mass[1] -> acShellParameter - acShellParameter}];
acDuplicateExternalPrepared = PrepareAmplitudes[{{acDuplicateExternalRules,
    SpinorChain[Spinor["Xi", "Square", 71], Total[Mom /@ Range[4]],
      Spinor["Xi", "Angle", 73]]}}, acMasslessScalarModel,
  "ReferenceSpinors" -> None, "InternalOnShell" -> False, "MaxSteps" -> 2,
  "ProgressReporting" -> False];
check["equivalent repeated external rules do not duplicate momenta in conservation",
  Last[First[acDuplicateExternalPrepared]] === 0 &&
    First[First[acDuplicateExternalPrepared]] === acDuplicateExternalRules];
acReorderedLocalPairs = {First[acLocalPairs], {Reverse[First[Last[acLocalPairs]]], Last[Last[acLocalPairs]]}};
acReorderedLocalPrepared = PrepareAmplitudes[acReorderedLocalPairs, acMasslessScalarModel,
  "ReferenceSpinors" -> None, "MaxSteps" -> 0, "ProgressReporting" -> False];
check["external rule order is immaterial while different diagrams retain their distinct internal shells",
  Last /@ acReorderedLocalPrepared === Last /@ acLocal["PreparedAmplitudes"] &&
    First /@ acReorderedLocalPrepared === First /@ acReorderedLocalPairs];

(* Declared pair masses determine scalar products without a heuristic search. *)
acDotExternal = {Mass[1] -> prElectronMass, Mass[2] -> prElectronMass,
  Mass[3] -> 0, Mass[4] -> 0, Mass[5] -> 0};
acDotRules = Append[acDotExternal, Mass[Multiparticle[2, 5]] -> prElectronMass];
acDotDen = PropDen[Mom[Multiparticle[2, 5]], prElectronMass];
acDotPair = {acDotRules, (3 + MomProd[2, 5])/acDotDen};
acDotPrepared = PrepareAmplitudes[{acDotPair}, prModel,
  "MaxSteps" -> 0, "ProgressReporting" -> False, "ReturnDetails" -> True];
check["a known internal pair removes its zero scalar product even when MaxSteps is zero",
  acDotPrepared["Status"] === "PREPARED" && acDotPrepared["Amplitude"] === 3/acDotDen];
check["deterministic pair-mass reduction is recorded as a diagram-local numerator identity",
  AnyTrue[First[acDotPrepared["DiagramReports"]]["Reductions"],
    TrueQ[Lookup[#, "InternalOnShellIdentity", False]] &&
      Lookup[#, "Before", None] =!= Lookup[#, "After", None] &]];
acDotReverse = PrepareAmplitudes[{{acDotRules,
    (3 + MomProd[5, 2])/acDotDen}}, prModel,
  "MaxSteps" -> 0, "ProgressReporting" -> False];
check["the reversed scalar-product ordering uses the same known pair mass",
  Last[First[acDotReverse]] === 3/acDotDen];
acDotReverseMass = PrepareAmplitudes[{{
    Append[acDotExternal, Mass[Multiparticle[5, 2]] -> prElectronMass], Last[acDotPair]}}, prModel,
  "MaxSteps" -> 0, "ProgressReporting" -> False];
check["reversed particle order in a declared pair mass is recognized",
  Last[First[acDotReverseMass]] === 3/acDotDen];
acDotLocalPairs = Table[{
    Append[acDotExternal, Mass[Multiparticle[2, 5]] -> mass],
    (3 + MomProd[2, 5])/PropDen[Mom[Multiparticle[2, 5]], mass]},
  {mass, {prElectronMass, 2 prElectronMass}}];
acDotLocal = PrepareAmplitudes[acDotLocalPairs, prModel,
  "MaxSteps" -> 0, "ProgressReporting" -> False];
check["identical scalar products use only each diagram's own symbolic internal mass",
  And @@ MapThread[acZero[#1 - #2] &,
    {Last /@ acDotLocal, {3/acDotDen,
      (3 + 3 prElectronMass^2/2)/PropDen[Mom[Multiparticle[2, 5]], 2 prElectronMass]}}]];
acDotUnknownPair = {acDotRules, (3 + MomProd[2, 4])/acDotDen};
acDotUnknown = PrepareAmplitudes[{acDotUnknownPair}, prModel,
  "MaxSteps" -> 0, "ProgressReporting" -> False];
check["unknown pair masses are neither assumed nor introduced into the expression",
  acZero[Last[First[acDotUnknown]] - Last[acDotUnknownPair]] &&
    FreeQ[Last[First[acDotUnknown]], Mass[_Multiparticle]]];
acDotComplementRules = Append[acDotExternal, Mass[Multiparticle[1, 3, 4]] -> prElectronMass];
acDotComplementDen = PropDen[Mom[Multiparticle[1, 3, 4]], prElectronMass];
acDotComplementPair = {acDotComplementRules, (3 + MomProd[2, 5])/acDotComplementDen};
acDotComplement = PrepareAmplitudes[{acDotComplementPair}, prModel,
  "MaxSteps" -> 0, "ProgressReporting" -> False];
check["a known larger cut determines its two-particle complement through momentum conservation",
  Last[First[acDotComplement]] === 3/acDotComplementDen];
acDotComplementOff = PrepareAmplitudes[{acDotComplementPair}, prModel,
  "MomentumConservation" -> False, "MaxSteps" -> 0, "ProgressReporting" -> False];
check["disabling momentum conservation disables inferred complementary pair masses",
  acZero[Last[First[acDotComplementOff]] - Last[acDotComplementPair]]];
acDotTripleRules = Append[Append[acDotExternal, Mass[6] -> 0],
  Mass[Multiparticle[1, 2, 3]] -> prElectronMass];
acDotTriplePair = {acDotTripleRules,
  (3 + MomProd[2, 5])/PropDen[Mom[Multiparticle[1, 2, 3]], prElectronMass]};
acDotTriple = PrepareAmplitudes[{acDotTriplePair}, prModel,
  "MaxSteps" -> 0, "ProgressReporting" -> False];
check["a three-particle cut and three-particle complement do not determine an individual pair",
  acZero[Last[First[acDotTriple]] - Last[acDotTriplePair]]];
acDotDisabled = PrepareAmplitudes[{acDotPair}, prModel,
  "InternalOnShell" -> False, "MaxSteps" -> 0, "ProgressReporting" -> False];
check["InternalOnShell False disables deterministic internal pair-mass reduction",
  acZero[Last[First[acDotDisabled]] - Last[acDotPair]]];
acDotDenominatorPair = {acDotRules,
  (3 + MomProd[2, 5])/(acDotDen (5 + MomProd[2, 5]))};
acDotDenominatorExpected = 3/(acDotDen (5 + MomProd[2, 5]));
acDotDenominatorPrepared = PrepareAmplitudes[{acDotDenominatorPair}, prModel,
  "MaxSteps" -> 0, "ProgressReporting" -> False];
check["pair-mass reductions preserve both physical propagators and other denominator factors",
  Last[First[acDotDenominatorPrepared]] === acDotDenominatorExpected];
acDotInversePair = {acDotRules, (3 + MomProd[2, 5])/(acDotDen MomProd[2, 5])};
acDotInversePrepared = PrepareAmplitudes[{acDotInversePair}, prModel,
  "MaxSteps" -> 0, "ProgressReporting" -> False];
check["an inverse scalar product is not incorrectly put on the numerator shell",
  acZero[Last[First[acDotInversePrepared]] - 3/(acDotDen MomProd[2, 5])]];
acDotNestedPair = {acDotRules,
  (3 + (1 + MomProd[2, 5])/(5 + MomProd[2, 5]))/acDotDen};
acDotNested = PrepareAmplitudes[{acDotNestedPair}, prModel,
  "MaxSteps" -> 0, "ProgressReporting" -> False];
check["nested inverse factors retain their off-shell scalar product while positive factors reduce",
  acZero[Last[First[acDotNested]] - (3 + 1/(5 + MomProd[2, 5]))/acDotDen] &&
    !FreeQ[Last[First[acDotNested]], MomProd[2, 5]]];
acDotDenominatorSearch = PrepareAmplitudes[{acDotDenominatorPair}, prModel,
  "MaxSteps" -> 4, "ProgressReporting" -> False];
check["subsequent identity search cannot reuse numerator shells inside denominators",
  Last[First[acDotDenominatorSearch]] === acDotDenominatorExpected];
Do[
  acUnknownPowerPair = {Append[acExternalZero, Mass[Multiparticle[1, 2]] -> 1],
    (3 + MomProd[1, 2]^power)^2/PropDen[Mom[Multiparticle[1, 2]], 1]};
  acUnknownPowerPrepared = PrepareAmplitudes[{acUnknownPowerPair}, acMasslessScalarModel,
    "ReferenceSpinors" -> None, "MaxSteps" -> 0, "ProgressReporting" -> False];
  check["an exponent with unknown sign protects its base from internal-shell substitution " <>
      ToString[power, InputForm],
    acZero[Last[First[acUnknownPowerPrepared]] - Expand[Last[acUnknownPowerPair]]] &&
      !FreeQ[Last[First[acUnknownPowerPrepared]], MomProd[1, 2]]],
  {power, {acShellPower, -acShellPower}}];
acPositivePowerPair = {Append[acExternalZero, Mass[Multiparticle[1, 2]] -> 1],
  (3 + MomProd[1, 2]^2)^2/PropDen[Mom[Multiparticle[1, 2]], 1]};
acPositivePowerPrepared = PrepareAmplitudes[{acPositivePowerPair}, acMasslessScalarModel,
  "ReferenceSpinors" -> None, "MaxSteps" -> 0, "ProgressReporting" -> False];
check["nonnegative integer powers still receive diagram-local numerator shell reductions",
  Last[First[acPositivePowerPrepared]] === 169/(16 PropDen[Mom[Multiparticle[1, 2]], 1])];
acExternalUnknownPower = (3 + SpinorChain[Spinor["Spin", "Square", 1], Mom[2],
    Spinor["Spin", "Angle", 2]]^(-acShellPower))^2/acDen;
acExternalUnknownPowerPrepared = PrepareAmplitudes[{{acRules, acExternalUnknownPower}}, prModel,
  "ReferenceSpinors" -> None, "MaxSteps" -> 0, "ProgressReporting" -> False];
check["ordinary external spinor reduction still acts inside an unknown-sign power",
  acZero[Last[First[acExternalUnknownPowerPrepared]] - (ReduceSpinorProducts[acExternalUnknownPower] /. acRules)] &&
    FreeQ[Last[First[acExternalUnknownPowerPrepared]], SpinorChain[___, _Mom, ___]]];

(* Ordinary spinor identities also act in reciprocal factors. Internal diagram
   shells remain restricted to positive numerator factors. *)
acWholeBracket = SpinorChain[Spinor["Helicity", "Angle", 3], Spinor["Xi", "Angle", 77]];
acWholeReverseBracket = SpinorChain[Spinor["Xi", "Angle", 77], Spinor["Helicity", "Angle", 3]];
acWholeBracketPrepared = PrepareAmplitudes[{{acRules,
    acWholeBracket/(acDen acWholeReverseBracket)}}, prModel,
  "ReferenceSpinors" -> None, "InternalOnShell" -> False, "MaxSteps" -> 0,
  "ProgressReporting" -> False];
check["whole-expression reduction cancels an auxiliary bracket with its reversed denominator",
  Last[First[acWholeBracketPrepared]] === -1/acDen];
Do[
  acWholeOtherKind = If[acWholeKind === "Angle", "Square", "Angle"];
  acWholeDiracChain = SpinorChain[Spinor["Spin", acWholeOtherKind, 1], Mom[2],
    Spinor["Spin", acWholeKind, 2]];
  acWholeDiracBracket = SpinorChain[Spinor["Spin", acWholeOtherKind, 1],
    Spinor["Spin", acWholeOtherKind, 2]];
  acWholeDiracPrepared = PrepareAmplitudes[{{acRules, 1/(acDen acWholeDiracChain)}},
    prModel, "ReferenceSpinors" -> None, "InternalOnShell" -> False, "MaxSteps" -> 0,
    "ProgressReporting" -> False];
  check["external Dirac identities reduce a reciprocal chain " <> acWholeKind,
    Last[First[acWholeDiracPrepared]] === -1/(prElectronMass acDen acWholeDiracBracket)],
  {acWholeKind, {"Angle", "Square"}}];
acWholeTracePrepared = PrepareAmplitudes[{{acRules,
    1/(acDen SpinorTrace[Mom[1], Mom[1]])}}, prModel,
  "ReferenceSpinors" -> None, "InternalOnShell" -> False, "MaxSteps" -> 0,
  "ProgressReporting" -> False];
check["ordinary trace identities reduce a reciprocal trace without touching its physical pole",
  Last[First[acWholeTracePrepared]] === 1/(2 prElectronMass^2 acDen)];
acWholeShellDirac = SpinorChain[Spinor["Spin", "Square", 1], Mom[2],
  Spinor["Spin", "Angle", 2]];
acWholeShellPrepared = PrepareAmplitudes[{{acDotRules,
    (3 + MomProd[2, 5])/(acDotDen (5 + MomProd[2, 5]) acWholeShellDirac)}}, prModel,
  "ReferenceSpinors" -> None, "MaxSteps" -> 0, "ProgressReporting" -> False];
check["ordinary denominator spinor reduction does not apply an internal scalar shell there",
  Last[First[acWholeShellPrepared]] === -3/(prElectronMass acDotDen (5 + MomProd[2, 5])
    SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])];
acKnownNumeratorDotCount[pair_] := Module[{declared, external, known, terms},
  external = Sort[Cases[First[pair], HoldPattern[Mass[i_Integer] -> _] :> i]];
  declared = Cases[First[pair], HoldPattern[Mass[cut_Multiparticle] -> _] :>
    Sort[List @@ cut]];
  known = Select[Join[declared, Complement[external, #] & /@ declared], Length[#] === 2 &];
  terms = If[Head[Last[pair]] === Plus, List @@ Last[pair], {Last[pair]}];
  Total[Function[term, Length[Select[Cases[Numerator[term], _MomProd, {0, Infinity}],
    Function[product, MemberQ[known, Sort[List @@ product]]]]]] /@ terms]
];
acDotQEDPairs = diagramAmplitudes[CreateDiagrams[
  {"e", "E", "A.+", "A.+", "A.+"}, prModel]];
acDotQEDPrepared = PrepareAmplitudes[acDotQEDPairs, prModel,
  "ReferenceSpinors" -> "AllSpurious", "MaxSteps" -> 0,
  "ProgressReporting" -> False, "ReturnDetails" -> True];
acDotQEDBeforePairs = ({#["MassRules"],
    ReduceSpinorProducts[#["ReferenceSpinors"]["Expression"]] /. #["MassRules"]} &) /@
  acDotQEDPrepared["DiagramReports"];
check["all six actual QED diagrams exhaust their declared-pair numerator scalar products",
  acDotQEDPrepared["Status"] === "PREPARED" &&
    Length[acDotQEDPrepared["PreparedAmplitudes"]] === 6 &&
    Total[acKnownNumeratorDotCount /@ acDotQEDBeforePairs] > 0 &&
    Total[acKnownNumeratorDotCount /@ acDotQEDPrepared["PreparedAmplitudes"]] === 0];
check["scalar-product preparation retains the original diagram inputs and mass-rule scope",
  acDotQEDPrepared["OriginalAmplitudes"] === acDotQEDPairs &&
    First /@ acDotQEDPrepared["PreparedAmplitudes"] === First /@ acDotQEDPairs];

(* Momentum conservation belongs to preparation, with the complete declared leg
   set and strictly improving trials on the whole numerator. *)
acConservationLabels = {2, 5, 8, 11};
acConservationRules = Thread[(Mass /@ acConservationLabels) -> ConstantArray[0, 4]];
acConservationRow = MomProd[2, 5] + MomProd[2, 8] + MomProd[2, 11];
acConservationDen = PropDen[Mom[Multiparticle[2, 5]], acShellMass];
acConservationPair = {acConservationRules,
  Expand[3 + acConservationRow^2]/(acConservationDen (5 + MomProd[2, 5]))};
acConservationPrepared = PrepareAmplitudes[{acConservationPair}, acMasslessScalarModel,
  "ReferenceSpinors" -> None, "InternalOnShell" -> False, "MaxSteps" -> 4,
  "ProgressReporting" -> False, "ReturnDetails" -> True];
check["whole-numerator conservation simplifies a squared row with noncontiguous particle labels",
  acConservationPrepared["Status"] === "PREPARED" &&
    acZero[acConservationPrepared["Amplitude"] - 3/(acConservationDen (5 + MomProd[2, 5]))]];
check["whole-numerator conservation preserves physical and other inverse factors literally",
  Denominator[acConservationPrepared["Amplitude"]] ===
    Denominator[3/(acConservationDen (5 + MomProd[2, 5]))]];
check["accepted whole-numerator conservation has a strict recorded rank improvement",
  AnyTrue[First[acConservationPrepared["DiagramReports"]]["IdentitySteps"],
    Lookup[Lookup[#, "Edit", <||>], "PreparationConservation", False] === True &&
      OrderedQ[{Lookup[#, "AfterRank", Infinity], Lookup[#, "BeforeRank", -Infinity]}] &&
      Lookup[#, "AfterRank", None] =!= Lookup[#, "BeforeRank", None] &]];
acConservationOff = PrepareAmplitudes[{acConservationPair}, acMasslessScalarModel,
  "ReferenceSpinors" -> None, "InternalOnShell" -> False, "MomentumConservation" -> False,
  "MaxSteps" -> 4, "ProgressReporting" -> False, "ReturnDetails" -> True];
check["disabling preparation conservation preserves the unconserved row-square value",
  Module[{angles, squares, momenta, epsilon = {{0, 1}, {-1, 0}}, adjugate, value},
    angles = AssociationThread[acConservationLabels, {{1, 2}, {2, -1}, {3, 1}, {-2, 3}}];
    squares = AssociationThread[acConservationLabels, {{2, 3}, {-1, 2}, {1, -3}, {4, 1}}];
    momenta = AssociationThread[acConservationLabels,
      -Outer[Times, angles[#], squares[#].epsilon] & /@ acConservationLabels];
    adjugate[p_] := {{p[[2, 2]], -p[[1, 2]]}, {-p[[2, 1]], p[[1, 1]]}};
    value[expression_] := Together[expression /. {
      SpinorChain[Spinor["Helicity", kind_, i_], Spinor["Helicity", kind_, j_]] :>
        If[kind === "Angle", angles[i].epsilon.angles[j], -squares[i].epsilon.squares[j]],
      MomProd[i_, j_] :> Tr[momenta[i].adjugate[momenta[j]]]/2}];
    Total[Values[momenta]] =!= ConstantArray[0, {2, 2}] &&
      value[acConservationOff["Amplitude"] - Last[acConservationPair]] === 0 &&
      AllTrue[First[acConservationOff["DiagramReports"]]["IdentitySteps"],
        Lookup[#, "Name", None] =!= "MomentumConservation" &]]];
acConservationZeroSteps = PrepareAmplitudes[{acConservationPair}, acMasslessScalarModel,
  "ReferenceSpinors" -> None, "InternalOnShell" -> False, "MaxSteps" -> 0,
  "ProgressReporting" -> False];
check["MaxSteps zero retains basic reduction without entering conservation search",
  acZero[Last[First[acConservationZeroSteps]] - Last[acConservationPair]]];
acConservationTwice = PrepareAmplitudes[acConservationPrepared["PreparedAmplitudes"],
  acMasslessScalarModel, "ReferenceSpinors" -> None, "InternalOnShell" -> False,
  "MaxSteps" -> 4, "ProgressReporting" -> False, "ReturnDetails" -> True];
check["conservation preparation is idempotent and records no cycling edits",
  acConservationTwice["PreparedAmplitudes"] === acConservationPrepared["PreparedAmplitudes"] &&
    First[acConservationTwice["DiagramReports"]]["IdentitySteps"] === {}];
acConservationChain = SpinorChain[Spinor["Xi", "Square", 19],
  Total[Mom /@ acConservationLabels], Spinor["Xi", "Angle", 23]];
acConservationChainPrepared = PrepareAmplitudes[{{acConservationRules, acConservationChain}},
  acMasslessScalarModel, "ReferenceSpinors" -> None, "InternalOnShell" -> False,
  "MaxSteps" -> 2, "ProgressReporting" -> False];
check["preparation knows the explicit conserved external set rather than a contiguous range",
  Last[First[acConservationChainPrepared]] === 0 && ReduceSpinorProducts[acConservationChain] =!= 0];
acPartial = PrepareAmplitudes[acLocalPairs, acMasslessScalarModel,
  "TimeConstraint" -> 0.000001, "ProgressReporting" -> False, "Output" -> "Details"];
check["preparation timeouts retain original inputs and the last valid expression",
  acPartial["Status"] === "PARTIAL" &&
    acPartial["OriginalAmplitudes"] === acLocalPairs &&
    Length[acPartial["PreparedAmplitudes"]] === Length[acLocalPairs]];
check["empty diagram preparation stays empty",
  PrepareAmplitudes[{}, prModel, "ProgressReporting" -> False] === {}];
check["empty preparation still supports explicit summed-expression output",
  PrepareAmplitudes[{}, prModel, "ProgressReporting" -> False,
    "Output" -> "Expression"] === 0];
Do[
  check["invalid preparation input or option is rejected " <> ToString[bad, InputForm],
    acInvalid[Quiet[PrepareAmplitudes[{acDiracPair}, prModel, bad]]]],
  {bad, {"MaxSteps" -> -1, "MaxCandidates" -> 0, "InternalOnShell" -> "Maybe",
    "MaxReferenceChoices" -> 0, "MaxReferenceChoices" -> 1.5,
    "ReferenceTimeConstraint" -> 0, "ReferenceTimeConstraint" -> "Forever",
    "TimeConstraint" -> 0, "Output" -> "Everything", "ReturnDetails" -> "Yes",
    "ReturnDetails" -> 1}}
];
check["a malformed preparation model is rejected",
  acInvalid[Quiet[PrepareAmplitudes[{acDiracPair}, {}]]]];
Do[
  check["failed or nonfinite diagram expressions are rejected " <> ToString[bad, InputForm],
    acInvalid[Quiet[PrepareAmplitudes[{{acRules, bad}}, prModel,
      "MaxSteps" -> 0, "ProgressReporting" -> False]]]],
  {bad, {$Failed, Missing["DiagramFailure"], Infinity}}
];
check["nonfinite diagram mass rules are rejected",
  acInvalid[Quiet[PrepareAmplitudes[{{{Mass[1] -> Infinity}, 1}}, prModel,
    "ProgressReporting" -> False]]]];
acInvalidReduction = Block[{ConstructiveDiagrams`Private`preparationNumerators},
  ConstructiveDiagrams`Private`preparationNumerators[_, _] := $Failed;
  PrepareAmplitudes[{acDiracPair}, prModel,
    "ProgressReporting" -> False, "Output" -> "Details"]];
check["failed reductions retain the previous expression and cannot become PREPARED",
  acInvalidReduction["Status"] === "PARTIAL" &&
    acInvalidReduction["Amplitude"] === Last[acDiracPair] &&
    First[acInvalidReduction["DiagramReports"]]["StopReason"] === "InvalidReduction"];

(* Compact factorization rows retain their metadata and declare only one cut.
   Other propagators inside a lower-point amplitude never add numerator shells. *)
acChannelDen = PropDen[Mom[Multiparticle[1, 2]], 2];
acChannelUncutDen = PropDen[Mom[Multiparticle[1, 3]], 3];
acChannelRow = Join[ConstantArray[0, 4], {Multiparticle[2, 1], 2,
  -(3 + MomProd[1, 2] + MomProd[1, 3])/(acChannelDen acChannelUncutDen)}];
acChannelSavedRow = acChannelRow;
acChannelPrepared = PrepareAmplitudes[{acChannelRow}, acMasslessScalarModel,
  "ReferenceSpinors" -> None, "MaxSteps" -> 0,
  "ProgressReporting" -> False, "ReturnDetails" -> True];
acChannelExpected = -(5 + MomProd[1, 3])/(acChannelDen acChannelUncutDen);
check["compact factorization rows use only their declared cut for numerator shells",
  acChannelPrepared["Status"] === "PREPARED" &&
    acZero[acChannelPrepared["Amplitude"] - acChannelExpected] &&
    !FreeQ[acChannelPrepared["Amplitude"], MomProd[1, 3]]];
check["optional channel diagnostics identify the input representation and selected channel",
  acChannelPrepared["InputType"] === "FactorizationChannels" &&
    acChannelPrepared["ElementTypes"] === {"FactorizationChannel"} &&
    acChannelPrepared["DiagramReports"] === {} &&
    acChannelPrepared["ChannelReports"] === acChannelPrepared["ItemReports"] &&
    With[{item = First[acChannelPrepared["ItemReports"]]},
      item["InputType"] === "FactorizationChannel" && item["ItemIndex"] === 1 &&
        item["Channel"] === acChannelRow[[-3]] &&
        item["Cut"] === List @@ acChannelRow[[-3]] && item["Mass"] === 2]];
check["channel preparation preserves every metadata entry, original sign and supplied row",
  Most[First[acChannelPrepared["PreparedAmplitudes"]]] === Most[acChannelSavedRow] &&
    acChannelPrepared["OriginalAmplitudes"] === {acChannelSavedRow} &&
    acChannelRow === acChannelSavedRow];
check["the selected and all unselected propagator denominators remain off shell",
  !FreeQ[acChannelPrepared["Amplitude"], acChannelDen] &&
    !FreeQ[acChannelPrepared["Amplitude"], acChannelUncutDen] &&
    First[acChannelPrepared["ItemReports"]]["InternalOnShellChannels"] === {{1, 2}} &&
    TrueQ[First[acChannelPrepared["ItemReports"]]["DenominatorsKeptOffShell"]]];
check["channel rows are returned directly by default rather than converted to diagram pairs",
  PrepareAmplitudes[{acChannelRow}, acMasslessScalarModel,
    "ReferenceSpinors" -> None, "MaxSteps" -> 0,
    "ProgressReporting" -> False] === acChannelPrepared["PreparedAmplitudes"] &&
    PrepareAmplitudes[{acChannelRow}, acMasslessScalarModel,
      "ReferenceSpinors" -> None, "MaxSteps" -> 0,
      "ProgressReporting" -> False, "Output" -> "Expression"] ===
        acChannelPrepared["Amplitude"]];
acExistingPoleXi = Spinor["Xi", "Angle", 5];
acExistingPolePhoton = Spinor["Helicity", "Angle", 5];
acExistingPole13 = PropDen[Mom[Multiparticle[1, 3]], prElectronMass];
acExistingPole25 = PropDen[Mom[Multiparticle[2, 5]], prElectronMass];
acExistingPoleSource = SpinorChain[acExistingPoleXi, Spinor["Spin", "Angle", 1]]/
  (SpinorChain[acExistingPoleXi, acExistingPolePhoton] acExistingPole13 acExistingPole25);
acExistingPoleRows = {
  {prElectronMass, prElectronMass, 0, 0, 0, Multiparticle[1, 3], prElectronMass, acExistingPoleSource},
  {prElectronMass, prElectronMass, 0, 0, 0, Multiparticle[2, 5], prElectronMass, 0}};
acExistingPolePrepared = PrepareAmplitudes[acExistingPoleRows, prModel,
  "ReferenceSpinors" -> "AllPhysical", "MaxSteps" -> 0,
  "ProgressReporting" -> False, "ReturnDetails" -> True];
check["physical reference selection does not duplicate an uncut propagator already present in a channel",
  acExistingPolePrepared["Status"] === "PARTIAL" &&
    First[acExistingPolePrepared["ItemReports"]]["ReferenceSpinors"]["Status"] === "UNRESOLVED" &&
    !FreeQ[First[acExistingPolePrepared["PreparedAmplitudes"]], Spinor["Xi", ___]] &&
    Exponent[Denominator[First[acExistingPolePrepared["PreparedAmplitudes"]][[-1]]],
      acExistingPole25] === 1];
check["excluding existing propagators from reference choices does not declare their numerators on shell",
  First[acExistingPolePrepared["ItemReports"]]["InternalOnShellChannels"] === {{1, 3}}];
acChannelContourRow = ReplacePart[acChannelRow,
  -1 -> Last[acChannelRow]/ConstructiveDiagrams`z];
acChannelContourPrepared = PrepareAmplitudes[{acChannelContourRow}, acMasslessScalarModel,
  "ReferenceSpinors" -> None, "MaxSteps" -> 0, "ProgressReporting" -> False];
check["preparation does not strip the channel contour factor or add a sewing sign",
  acZero[ConstructiveDiagrams`z Last[First[acChannelContourPrepared]] - acChannelExpected] &&
    !FreeQ[Last[First[acChannelContourPrepared]], ConstructiveDiagrams`z]];
acDiracChannelRow = {prElectronMass, prElectronMass, 0, 0, Multiparticle[1, 3], prElectronMass, Last[acDiracPair]};
acDiracChannelPrepared = PrepareAmplitudes[{acDiracChannelRow}, prModel,
  "ReferenceSpinors" -> None, "MaxSteps" -> 0, "ProgressReporting" -> False];
check["compact channels also receive ordinary external Dirac identities",
  Last[First[acDiracChannelPrepared]] === acPrepared["Amplitude"] &&
    Most[First[acDiracChannelPrepared]] === Most[acDiracChannelRow]];
check["the legacy preparation alias accepts the same compact channel rows",
  PrepareDiagramAmplitudes[{acChannelRow}, acMasslessScalarModel,
    "ReferenceSpinors" -> None, "MaxSteps" -> 0,
    "ProgressReporting" -> False] === acChannelPrepared["PreparedAmplitudes"]];
acChannelOff = PrepareAmplitudes[{acChannelRow}, acMasslessScalarModel,
  "ReferenceSpinors" -> None, "MaxSteps" -> 0, "InternalOnShell" -> False,
  "ProgressReporting" -> False, "ReturnDetails" -> True];
check["channel numerator shells can be disabled without discarding their metadata",
  acZero[acChannelOff["Amplitude"] - Last[acChannelRow]] &&
    Most[First[acChannelOff["PreparedAmplitudes"]]] === Most[acChannelRow] &&
    First[acChannelOff["ItemReports"]]["InternalOnShellChannels"] === {}];
acChannelProtected = ReplacePart[acChannelRow, -1 ->
  (3 + MomProd[1, 2])/(acChannelDen (5 + MomProd[1, 2]))];
acChannelProtectedPrepared = PrepareAmplitudes[{acChannelProtected}, acMasslessScalarModel,
  "ReferenceSpinors" -> None, "MaxSteps" -> 0, "ProgressReporting" -> False];
check["a factorization channel shell never substitutes into a reciprocal invariant",
  Last[First[acChannelProtectedPrepared]] === 5/(acChannelDen (5 + MomProd[1, 2]))];
acChannelComplementRow = ReplacePart[acChannelRow, -1 -> MomProd[3, 4]/acChannelDen];
acChannelComplement = PrepareAmplitudes[{acChannelComplementRow}, acMasslessScalarModel,
  "ReferenceSpinors" -> None, "MaxSteps" -> 0, "ProgressReporting" -> False];
acChannelComplementOff = PrepareAmplitudes[{acChannelComplementRow}, acMasslessScalarModel,
  "ReferenceSpinors" -> None, "MaxSteps" -> 0, "MomentumConservation" -> False,
  "ProgressReporting" -> False];
check["a channel's complementary cut is equivalent only when momentum conservation is enabled",
  Last[First[acChannelComplement]] === 2/acChannelDen &&
    Last[First[acChannelComplementOff]] === Last[acChannelComplementRow]];
acChannelLargeDen = PropDen[Mom[Multiparticle[1, 2, 3]], 3];
acChannelLargeRow = Join[ConstantArray[0, 5], {Multiparticle[1, 2, 3], 3,
  (1 + MomProd[4, 5])/acChannelLargeDen}];
acChannelLarge = PrepareAmplitudes[{acChannelLargeRow}, acMasslessScalarModel,
  "ReferenceSpinors" -> None, "MaxSteps" -> 0, "ProgressReporting" -> False];
check["a larger factorization cut uses its known two-particle complement at five points",
  Most[First[acChannelLarge]] === Most[acChannelLargeRow] &&
    Last[First[acChannelLarge]] === 11/(2 acChannelLargeDen)];
acChannelSixRow = Join[ConstantArray[0, 6], {Multiparticle[1, 2, 3], 3,
  (1 + MomProd[4, 5])/acChannelLargeDen}];
acChannelSix = PrepareAmplitudes[{acChannelSixRow}, acMasslessScalarModel,
  "ReferenceSpinors" -> None, "MaxSteps" -> 0, "ProgressReporting" -> False];
check["a three-by-three cut does not invent an individual pair-mass condition",
  acZero[Last[First[acChannelSix]] - Last[acChannelSixRow]] &&
    !FreeQ[Last[First[acChannelSix]], MomProd[4, 5]] &&
    Most[First[acChannelSix]] === Most[acChannelSixRow]];
acMixedPreparationInput = {First[acLocalPairs], acChannelRow};
acMixedPreparation = PrepareAmplitudes[acMixedPreparationInput, acMasslessScalarModel,
  "ReferenceSpinors" -> None, "MaxSteps" -> 0,
  "ProgressReporting" -> False, "ReturnDetails" -> True];
check["each mixed-list element retains its own input format and shell assumptions",
  First[acMixedPreparation["PreparedAmplitudes"]] === First[acLocal["PreparedAmplitudes"]] &&
    Last[acMixedPreparation["PreparedAmplitudes"]] ===
      First[acChannelPrepared["PreparedAmplitudes"]] &&
    Lookup[acMixedPreparation["ItemReports"], "InternalOnShellChannels"] ===
      {{{1, 2}}, {{1, 2}}} &&
    acMixedPreparation["OriginalAmplitudes"] === acMixedPreparationInput];
check["mixed preparation reports separate diagram and channel items without exposing extra default output",
  acMixedPreparation["InputType"] === "Mixed" &&
    acMixedPreparation["ElementTypes"] === {"Diagram", "FactorizationChannel"} &&
    Length[acMixedPreparation["DiagramReports"]] === 1 &&
    Length[acMixedPreparation["ChannelReports"]] === 1 &&
    Lookup[acMixedPreparation["ItemReports"], "ItemIndex"] === {1, 2}];
check["diagram diagnostics remain available through the legacy report-key alias",
  acPrepared["ItemReports"] === acPrepared["DiagramReports"]];
Do[
  check["malformed compact channel metadata is rejected " <> ToString[bad, InputForm],
    acInvalid[Quiet[PrepareAmplitudes[{bad}, acMasslessScalarModel,
      "ReferenceSpinors" -> None, "MaxSteps" -> 0, "ProgressReporting" -> False]]]],
  {bad, {
    {0, 0, 0, 0, Multiparticle[1, 1], 2, 1},
    {0, 0, 0, 0, Multiparticle[1, 5], 2, 1},
    {0, 0, 0, 0, Multiparticle[1], 2, 1},
    {0, 0, 0, 0, Multiparticle[1, 2, 3, 4], 2, 1},
    {0, 0, 0, 0, {1, 2}, 2, 1},
    {0, 0, 0, 0, Multiparticle[1, 2], Infinity, 1},
    {0, 0, 0, Infinity, Multiparticle[1, 2], 2, 1},
    {0, 0, 0, 0, Multiparticle[1, 2], 2, $Failed},
    {0, 0, 0, 0, Multiparticle[1, 2], 2, Missing["Amplitude"]}
  }}
];
check["mixed inputs with conflicting external masses fail before any simplification",
  acInvalid[Quiet[PrepareAmplitudes[{First[acLocalPairs],
    ReplacePart[acChannelRow, 1 -> 1]}, acMasslessScalarModel,
      "ReferenceSpinors" -> None, "MaxSteps" -> 0, "ProgressReporting" -> False]]]];
check["an unknown preparation option cannot be silently ignored",
  acInvalid[Quiet[PrepareAmplitudes[{acChannelRow}, acMasslessScalarModel,
    "ReferenceSpinors" -> None, "MaxSteps" -> 0,
    "ProgressReporting" -> False, "UnknownPreparationOption" -> True]]]];
acActualPreparedChannels = PrepareAmplitudes[prChannels, prFixtureModel,
  "ReferenceSpinors" -> None, "MaxSteps" -> 0,
  "ProgressReporting" -> False, "ReturnDetails" -> True];
check["actual five-point factorization channels keep all compact channel definitions",
  acActualPreparedChannels["Status"] === "PREPARED" &&
    Length[acActualPreparedChannels["PreparedAmplitudes"]] === 6 &&
    Most /@ acActualPreparedChannels["PreparedAmplitudes"] === Most /@ prChannels &&
    acActualPreparedChannels["OriginalAmplitudes"] === prChannels &&
    And @@ MapThread[Function[{row, item},
      item["InternalOnShellChannels"] === {Sort[List @@ row[[-3]]]}],
      {prChannels, acActualPreparedChannels["ItemReports"]}]];
acActualChannelResidues = Flatten[MapThread[Function[{original, prepared}, Module[{target},
    target = SelectFirst[prElectronTargets,
      #["Cut"] === ConstructiveDiagrams`Private`canonicalPhysicalCut[
        List @@ original[[-3]], Range[5]] &];
    Table[PhysicalPoleResidue[
      (Last[original] - Last[prepared]) /. ConstructiveDiagrams`z -> 1, target,
      "KinematicMap" -> prMap[i, j], "TimeConstraint" -> 10], {i, 2}, {j, 2}]
  ]], {prChannels, acActualPreparedChannels["PreparedAmplitudes"]}], 2];
check["external reduction and a channel's own numerator shell preserve all sampled QED residues",
  Length[acActualChannelResidues] === 24 &&
    AllTrue[acActualChannelResidues,
      AssociationQ[#] && #["Status"] === "PASS" && #["PhysicalResidue"] === 0 &]];

(* Independent cubic-scalar trees expose double counting of overlapping poles. *)
Print["Checking scalar-tree completion and exact linear refinement."];
acScalarModel = {{{"Scalar", "s", "s", 1, 0, acScalarMass, 0, 1, 1, "", ""}},
  {{"s", "s", "s", "", acCubicCoupling, 1}}, {}, {}};
acSum[pairs_] := Total[(ReduceSpinContractions[Last[#]] /. First[#]) & /@ pairs];
acScalarFourPairs = diagramAmplitudes[CreateDiagrams[ConstantArray["s", 4], acScalarModel]];
acScalarFivePairs = diagramAmplitudes[CreateDiagrams[ConstantArray["s", 5], acScalarModel]];
acScalarFour = acSum[acScalarFourPairs]; acScalarFive = acSum[acScalarFivePairs];
acScalarKnownModel = ReplacePart[acScalarModel, 2 -> Join[acScalarModel[[2]],
  {Join[ConstantArray["s", 4], {1, acScalarFour}]}]];
acScalarFourChannels = FactorizationAmplitudeChannels[ConstantArray["s", 4], acScalarModel];
acScalarFiveChannels = FactorizationAmplitudeChannels[ConstantArray["s", 5], acScalarKnownModel];
acScalarRules = {acScalarMass -> 1, acCubicCoupling -> 2};
acFiveTargets = ConstructiveDiagrams`Private`residueSimpleInputs[
  0, acScalarFiveChannels, acScalarKnownModel]["Targets"];
acNaiveLift = Total[(#["StateSum"]/#["Propagator"]) & /@ acFiveTargets];
check["the five-scalar pilot has fifteen diagrams and ten single-pole channels",
  Length[acScalarFivePairs] === 15 && Length[acScalarFiveChannels] === 10];
check["naively summing complete single-pole lifts double counts every scalar diagram",
  acZero[acCanonical[acNaiveLift - 2 acScalarFive, 5]]];
acFour = acFactor[0, acScalarFourChannels, acScalarModel, "ParameterRules" -> acScalarRules];
check["automatic factorization completion reconstructs a four-scalar tree from zero",
  acFour["Status"] === "COMPLETE" &&
    acZero[acCanonical[acFour["Amplitude"] - acScalarFour, 4]]];
acFive = acFactor[0, acScalarFiveChannels, acScalarKnownModel, "ParameterRules" -> acScalarRules];
check["automatic completion reconstructs double-pole scalar trees without a known-answer basis",
  acFive["Status"] === "COMPLETE" &&
    acZero[acCanonical[acFive["Amplitude"] - acScalarFive, 5]]];
check["simultaneous completion avoids the naive factor-of-two overcount",
  !acZero[acFive["Amplitude"]] &&
    acZero[acCanonical[acNaiveLift - 2 acFive["Amplitude"], 5]]];
acFiveHeldOut = acCheck[acFive["Amplitude"], acScalarFiveChannels, acScalarKnownModel,
  {acScalarMass -> 2, acCubicCoupling -> 3}, 5801];
check["the scalar reconstruction survives held-out masses, couplings and cut samples",
  acPhysicalPass[acFiveHeldOut] && Length[acFiveHeldOut["Channels"]] === 10];
acInconsistent = ConstructiveDiagrams`Private`completionSolveLinear[
  {acUnknown - 1, acUnknown - 2}, {acUnknown}];
check["contradictory exact linear equations cannot produce a spurious solution",
  acInconsistent["Status"] === "INCONSISTENT"];
acLinearUnknowns = {acLinearU, acLinearV, acLinearW};
acLinearInitial = {acLinearU + 2 acLinearV - acLinearW - 3};
acLinearAdded = {acLinearV + acLinearW - 2};
acLinearFirst = ConstructiveDiagrams`Private`completionSolveLinear[
  acLinearInitial, acLinearUnknowns];
acLinearIncrement = ConstructiveDiagrams`Private`completionRefineLinear[
  acLinearFirst, acLinearAdded];
acLinearBatch = ConstructiveDiagrams`Private`completionSolveLinear[
  Join[acLinearInitial, acLinearAdded], acLinearUnknowns];
check["incremental refinement preserves the exact affine solution of a batch linear solve",
  acLinearIncrement["Status"] === "UNDERDETERMINED" &&
    acLinearIncrement["Rank"] === acLinearBatch["Rank"] &&
    acLinearIncrement["FreeParameters"] === acLinearBatch["FreeParameters"] &&
    AllTrue[(acLinearUnknowns /. acLinearIncrement["SolutionRules"]) -
      (acLinearUnknowns /. acLinearBatch["SolutionRules"]), acZero]];
acLinearFinal = ConstructiveDiagrams`Private`completionRefineLinear[
  acLinearIncrement, {acLinearW - 7}];
acLinearFinalBatch = ConstructiveDiagrams`Private`completionSolveLinear[
  Join[acLinearInitial, acLinearAdded, {acLinearW - 7}], acLinearUnknowns];
check["successive refinements compose earlier pivot rules and retain the full rank",
  acLinearFinal["Status"] === "COMPLETE" &&
    acLinearFinal["Rank"] === 3 && acLinearFinal["FreeParameters"] === {} &&
    AllTrue[(acLinearUnknowns /. acLinearFinal["SolutionRules"]) -
      (acLinearUnknowns /. acLinearFinalBatch["SolutionRules"]), acZero]];
acLinearContradiction = ConstructiveDiagrams`Private`completionRefineLinear[
  acLinearFinal, {acLinearW - 8}];
check["an inconsistent extension of an exact incremental solution remains inconsistent",
  acLinearContradiction["Status"] === "INCONSISTENT"];
(* Exact row scaling must not change the affine family or hide a contradiction.
   Large common content mirrors the coherent-spin validation equations. *)
acScaledUnknowns = {acScaledU, acScaledV, acScaledW, acScaledX, acScaledY, acScaledZ};
acScaledInitial = {
  acScaledUnknowns[[1]] + 2 acScaledUnknowns[[2]] - acScaledUnknowns[[3]] - 3,
  acScaledUnknowns[[2]] - acScaledUnknowns[[4]] + Sqrt[2] acScaledUnknowns[[6]] - 1};
acScaledAdded = {
  2 acScaledUnknowns[[1]] - acScaledUnknowns[[2]] + acScaledUnknowns[[3]] +
    I acScaledUnknowns[[5]] - 2 - Sqrt[2],
  acScaledUnknowns[[4]] + 3 acScaledUnknowns[[5]] - 2 acScaledUnknowns[[6]] - Sqrt[3]};
acScaledRows[rows_List] := Flatten[Table[
  ((k + I)/(k + 1)) (10^90 + 39)^k rows, {k, 1, 3}]];
acScaledFirst = ConstructiveDiagrams`Private`completionSolveLinear[
  acScaledRows[acScaledInitial], acScaledUnknowns];
acScaledIncrement = ConstructiveDiagrams`Private`completionRefineLinear[
  acScaledFirst, acScaledRows[acScaledAdded]];
acScaledBatch = ConstructiveDiagrams`Private`completionSolveLinear[
  Join[acScaledRows[acScaledInitial], acScaledRows[acScaledAdded]], acScaledUnknowns];
check["large exact row contents retain the same batch and incremental affine family",
  acScaledIncrement["Status"] === "UNDERDETERMINED" &&
    acScaledIncrement["Rank"] === 4 && acScaledBatch["Rank"] === 4 &&
    acScaledIncrement["FreeParameters"] === acScaledBatch["FreeParameters"] &&
    AllTrue[(acScaledUnknowns /. acScaledIncrement["SolutionRules"]) -
      (acScaledUnknowns /. acScaledBatch["SolutionRules"]), RootReduce[Expand[#]] === 0 &]];
check["the scaled affine family satisfies every original algebraic and complex constraint",
  AllTrue[Join[acScaledInitial, acScaledAdded] /. acScaledIncrement["SolutionRules"],
    RootReduce[Expand[#]] === 0 &] && Length[acScaledIncrement["FreeParameters"]] === 2];
acScaledLateContradiction = ConstructiveDiagrams`Private`completionRefineLinear[
  acScaledIncrement, {First[acScaledInitial] + 1}];
check["large-row normalization cannot discard a late exact contradiction",
  acScaledLateContradiction["Status"] === "INCONSISTENT"];
acScaledZeroExtension = ConstructiveDiagrams`Private`completionRefineLinear[
  acScaledIncrement, {0, 0, 0}];
check["zero-row refinement preserves all remaining exact affine freedom",
  acScaledZeroExtension["Rank"] === 4 &&
    acScaledZeroExtension["FreeParameters"] === acScaledIncrement["FreeParameters"] &&
    AllTrue[(acScaledUnknowns /. acScaledZeroExtension["SolutionRules"]) -
      (acScaledUnknowns /. acScaledIncrement["SolutionRules"]), RootReduce[Expand[#]] === 0 &]];
check["row normalization still rejects nonlinear correction coefficients",
  ConstructiveDiagrams`Private`completionSolveLinear[
    {acScaledUnknowns[[1]]^2 - 1}, acScaledUnknowns]["Status"] === "UNRESOLVED"];
acRadicalRows = {acRadicalU + 2 acRadicalV - acRadicalW - 3 Sqrt[2],
  acRadicalV + acRadicalW - 2 Sqrt[2]};
acRadicalFit = ConstructiveDiagrams`Private`completionSolveLinear[
  acRadicalRows, {acRadicalU, acRadicalV, acRadicalW}];
acRadicalContradiction = ConstructiveDiagrams`Private`completionRefineLinear[
  acRadicalFit, {First[acRadicalRows] - Sqrt[2]}];
check["a common radical right-hand side is restored without rescaling free coefficients",
  acRadicalFit["Status"] === "UNDERDETERMINED" && acRadicalFit["Rank"] === 2 &&
    acRadicalFit["FreeParameters"] === {acRadicalW} &&
    AllTrue[({acRadicalU, acRadicalV} /. acRadicalFit["SolutionRules"]) -
      {-Sqrt[2] + 3 acRadicalW, 2 Sqrt[2] - acRadicalW}, RootReduce[Expand[#]] === 0 &] &&
    AllTrue[acRadicalRows /. acRadicalFit["SolutionRules"], RootReduce[Expand[#]] === 0 &] &&
    acRadicalContradiction["Status"] === "INCONSISTENT"];
(* Distinct algebraic column units must be restored in the original variables,
   including a free variable whose column was scaled during row reduction. *)
acColumnUnknowns = {acColumnU, acColumnV, acColumnW};
acColumnRows = {
  Sqrt[2] acColumnU + 2 I acColumnV - Sqrt[3] acColumnW - 2 Sqrt[5],
  2 Sqrt[2] acColumnU - I acColumnV + Sqrt[3] acColumnW - 3 Sqrt[5],
  -Sqrt[2] acColumnU + 3 I acColumnV + 2 Sqrt[3] acColumnW - 11 Sqrt[5]};
acColumnFit = ConstructiveDiagrams`Private`completionSolveLinear[
  acColumnRows, acColumnUnknowns];
check["distinct real and imaginary algebraic column scales preserve a full exact solution",
  acColumnFit["Status"] === "COMPLETE" && acColumnFit["Rank"] === 3 &&
    acColumnFit["FreeParameters"] === {} &&
    AllTrue[(acColumnUnknowns /. acColumnFit["SolutionRules"]) -
      {Sqrt[5]/Sqrt[2], -2 I Sqrt[5], Sqrt[15]}, RootReduce[Expand[#]] === 0 &] &&
    AllTrue[acColumnRows /. acColumnFit["SolutionRules"], RootReduce[Expand[#]] === 0 &]];
acColumnPartial = ConstructiveDiagrams`Private`completionSolveLinear[
  Take[acColumnRows, 2], acColumnUnknowns];
check["algebraic column scaling retains free coefficients in their original coordinates",
  acColumnPartial["Status"] === "UNDERDETERMINED" && acColumnPartial["Rank"] === 2 &&
    acColumnPartial["FreeParameters"] === {acColumnW} &&
    AllTrue[({acColumnU, acColumnV} /. acColumnPartial["SolutionRules"]) -
      {(8 Sqrt[5] - Sqrt[3] acColumnW)/(5 Sqrt[2]),
        (Sqrt[5] + 3 Sqrt[3] acColumnW)/(5 I)}, RootReduce[Expand[#]] === 0 &] &&
    AllTrue[Take[acColumnRows, 2] /. acColumnPartial["SolutionRules"],
      RootReduce[Expand[#]] === 0 &]];
acColumnContradiction = ConstructiveDiagrams`Private`completionSolveLinear[
  Append[acColumnRows, First[acColumnRows] - Sqrt[7]], acColumnUnknowns];
check["algebraic column scaling cannot hide an inconsistent radical equation",
  acColumnContradiction["Status"] === "INCONSISTENT"];
acColumnIncrement = ConstructiveDiagrams`Private`completionRefineLinear[
  acColumnPartial, {Last[acColumnRows]}];
check["refinement after algebraic column scaling agrees with a batch solve",
  acColumnIncrement["Status"] === "COMPLETE" && acColumnIncrement["Rank"] === 3 &&
    acColumnIncrement["FreeParameters"] === {} &&
    AllTrue[(acColumnUnknowns /. acColumnIncrement["SolutionRules"]) -
      (acColumnUnknowns /. acColumnFit["SolutionRules"]), RootReduce[Expand[#]] === 0 &] &&
    AllTrue[acColumnRows /. acColumnIncrement["SolutionRules"], RootReduce[Expand[#]] === 0 &]];
acUnfactoredColumnRows = {acUnfactoredU + Sqrt[3] acUnfactoredV - 1,
  Sqrt[2] acUnfactoredU + 2 Sqrt[3] acUnfactoredV - 3};
acUnfactoredColumnFit = ConstructiveDiagrams`Private`completionSolveLinear[
  acUnfactoredColumnRows, {acUnfactoredU, acUnfactoredV}];
check["a column without one rationalizing algebraic unit retains the exact fallback solver",
  acUnfactoredColumnFit["Status"] === "COMPLETE" &&
    acUnfactoredColumnFit["Rank"] === 2 && acUnfactoredColumnFit["FreeParameters"] === {} &&
    AllTrue[({acUnfactoredU, acUnfactoredV} /. acUnfactoredColumnFit["SolutionRules"]) -
      {-(2 + Sqrt[2])/2, (4 + Sqrt[2])/(2 Sqrt[3])}, RootReduce[Expand[#]] === 0 &] &&
    AllTrue[acUnfactoredColumnRows /. acUnfactoredColumnFit["SolutionRules"],
      RootReduce[Expand[#]] === 0 &]];
acFourTargets = ConstructiveDiagrams`Private`residueSimpleInputs[
  0, acScalarFourChannels, acScalarModel]["Targets"];
acHalfChannel = ReplacePart[First[acScalarFourChannels],
  -1 -> Last[First[acScalarFourChannels]]/2];
acSplitChannel = acFactor[0, {acHalfChannel, acHalfChannel}, acScalarModel,
  "ParameterRules" -> acScalarRules, "Samples" -> 1, "ValidationSamples" -> 1];
check["degenerate exchange rows on one cut contribute their summed residue to fitting and validation",
  acSplitChannel["Status"] === "COMPLETE" &&
    acZero[acCanonical[acSplitChannel["Amplitude"] -
      First[acFourTargets]["StateSum"]/First[acFourTargets]["Propagator"], 4]]];
check["an empty or partial physical report cannot certify nonempty channel targets",
  !ConstructiveDiagrams`Private`completionPhysicalPassQ[<|"Channels" -> {}|>, acFourTargets] &&
    !ConstructiveDiagrams`Private`completionPhysicalPassQ[
      <|"Channels" -> {Join[First[acFourTargets], <|"Status" -> "PASS"|>]}|>, acFourTargets] &&
    ConstructiveDiagrams`Private`completionPhysicalPassQ[<|"Channels" -> {}|>, {}]];
acFeedbackContext = ConstructiveDiagrams`Private`completionNormalizeInput[
  acScalarFour, acScalarFourChannels, acScalarModel];
acEmptyFeedback = Catch[Catch[
  ConstructiveDiagrams`Private`completionValidationConstraints[
    0, {}, acFeedbackContext, {}, 1], _,
  Function[{value, tag}, Missing["EscapedValidationThrow", tag]]]];
check["missing held-out data returns an unresolved diagnostic without escaping the completion function",
  AssociationQ[acEmptyFeedback] && acEmptyFeedback["Status"] === "UNRESOLVED" &&
    acEmptyFeedback["Diagnostics"] =!= {}];
acAllowedChannels = Rest[acScalarFourChannels];
acAllowedChannelsSaved = acAllowedChannels;
acForbidden = acFactor[acScalarFour, acAllowedChannels, acScalarModel,
  "ParameterRules" -> acScalarRules];
acForbiddenValidationChannels = Prepend[acAllowedChannels,
  ReplacePart[First[acScalarFourChannels], -1 -> 0]];
acForbiddenHeldOut = acCheck[acForbidden["Amplitude"], acForbiddenValidationChannels,
  acScalarModel, {acScalarMass -> 2, acCubicCoupling -> 3}, 6029];
check["repair preserves zero residues in channels absent from the supplied data",
  acForbidden["Status"] === "COMPLETE" && acPhysicalPass[acForbiddenHeldOut] &&
    Length[acForbiddenHeldOut["Channels"]] === 3];
acZeroObligationRow = SelectFirst[acForbidden["ValidationReport"]["Channels"],
  #["Cut"] === First[acFourTargets]["Cut"] &, Missing["ZeroChannelNotValidated"]];
check["a removed forbidden pole remains an explicit zero obligation in held-out validation",
  acAllowedChannels === acAllowedChannelsSaved &&
    Length[acForbidden["ValidationReport"]["Channels"]] === 3 &&
    AssociationQ[acZeroObligationRow] && acZeroObligationRow["Status"] === "PASS" &&
    Total[Lookup[acZeroObligationRow["FactorizationChannels"], "StateSum"]] === 0];
acWithoutForbiddenPole = Cancel[acCanonical[acScalarFour -
  First[acFourTargets]["StateSum"]/First[acFourTargets]["Propagator"], 4]];
acBasisOnlyForbidden = acFactor[acWithoutForbiddenPole, acAllowedChannels, acScalarModel,
  "CorrectionBasis" -> {1/First[acFourTargets]["Propagator"]},
  "ParameterRules" -> acScalarRules];
check["a forbidden physical pole appearing only in an explicit correction basis is constrained to zero",
  FreeQ[acWithoutForbiddenPole, First[acFourTargets]["Propagator"]] &&
    acAllowedChannels === acAllowedChannelsSaved &&
    acBasisOnlyForbidden["Status"] === "COMPLETE" &&
    acBasisOnlyForbidden["FreeParameters"] === {} &&
    acBasisOnlyForbidden["RemainingBasis"] === {} &&
    Length[acBasisOnlyForbidden["ValidationReport"]["Channels"]] === 3 &&
    acZero[acBasisOnlyForbidden["Amplitude"] - acWithoutForbiddenPole]];

(* Known QED trees and one genuinely missing pole contribution. *)
Print["Checking existing QED amplitudes and a missing physical pole."];
acQEDFour = acFactor[prFourPointAmplitude, prFourPointChannels, prModel,
  "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1}];
check["an already correct physical QED four-point amplitude is not changed",
  acQEDFour["Status"] === "COMPLETE" && acQEDFour["Correction"] === 0 &&
    acZero[acQEDFour["Amplitude"] - prFourPointAmplitude]];
acSavedCheckerOptions = Options[CheckPhysicalResidues]; acCustomMapUses = 0;
acHostileMap[___] := (acCustomMapUses++; Indeterminate);
SetOptions[CheckPhysicalResidues,
  "KinematicMap" -> acHostileMap, "KinematicKind" -> "Symbolic",
  "Tolerance" -> 1000000];
acIsolatedFactor = acFactor[prFourPointAmplitude, prFourPointChannels, prModel,
  "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1}, "Samples" -> 1, "ValidationSamples" -> 1];
SetOptions[CheckPhysicalResidues, Sequence @@ acSavedCheckerOptions];
check["completion validation does not inherit custom checker maps or kinematic modes",
  acCustomMapUses === 0 && acIsolatedFactor["Status"] === "COMPLETE" &&
    acPhysicalPass[acIsolatedFactor["ValidationReport"]]];
acQEDFive = acFactor[prH, prChannels, prFixtureModel,
  "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1}];
check["an already correct physical QED five-point amplitude is not changed",
  acQEDFive["Status"] === "COMPLETE" && acQEDFive["Correction"] === 0 &&
    acZero[acQEDFive["Amplitude"] - prH]];
acQEDMissingPole = First[prFourPointTargets]["StateSum"]/
  First[prFourPointTargets]["Propagator"];
acQEDRepair = acFactor[prFourPointAmplitude - acQEDMissingPole,
  prFourPointChannels, prModel, "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1}];
acQEDRepairHeldOut = acCheck[acQEDRepair["Amplitude"], prFourPointChannels, prModel,
  {prElectronMass -> 2, prElectricCharge -> 3}, 7919];
check["a missing QED pole is repaired using the supplied channels rather than a known-answer basis",
  acQEDRepair["Status"] === "COMPLETE" && acPhysicalPass[acQEDRepairHeldOut]];
check["QED fitting and held-out validation retain separate sample information",
  AssociationQ[acQEDRepair["ConstraintData"]] &&
    AssociationQ[acQEDRepair["ValidationReport"]]];
check["an exactly validated QED affine family is not rejected by unsimplified coefficient cancellations",
  acQEDRepair["Status"] === "COMPLETE" &&
    acQEDRepair["ConstraintData"]["LinearSolution"]["Rank"] === 2 &&
    Length[acQEDRepair["ConstraintData"]["LinearSolution"]["FreeParameters"]] === 3 &&
    acQEDRepair["FreeParameters"] =!= {} &&
    AllTrue[acQEDRepair["ValidationReports"], acPhysicalPass[#["Report"]] &]];
acRejectedHigherPoles = Select[acQEDRepair["ChannelContinuations"]["Variants"],
  !TrueQ[#["Accepted"]] &&
    Lookup[#["Verification"], "Reason", None] ===
      "UnsupportedHigherOrderPhysicalPoleContinuation" &];
check["reference choices introducing unsupported repeated physical poles are rejected explicitly",
  acRejectedHigherPoles =!= {} &&
    AllTrue[acRejectedHigherPoles,
      #["Verification"]["HigherOrderPropagators"] =!= {} &&
        #["Verification"]["Diagnostics"] =!= {} &]];
(* The caller explicitly chooses a shared generic reference through preparation. *)
acQEDCommonPairs = diagramAmplitudes[CreateDiagrams[{"e", "E", "A.+", "A.+"}, prModel]];
acQEDPipelinePrepared = PrepareAmplitudes[acQEDCommonPairs, prModel,
  "ReferenceSpinors" -> "Xi", "ProgressReporting" -> False, "Output" -> "Details"];
acQEDPipelinePairs = PrepareAmplitudes[acQEDCommonPairs, prModel,
  "ReferenceSpinors" -> "Xi", "ProgressReporting" -> False];
acQEDPlainFactor = acFactor[acQEDPipelinePairs, prFourPointChannels, prModel,
  "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1}];
check["ordinary prepared pairs feed factorization completion without extracting report fields",
  ListQ[acQEDPipelinePairs] && AllTrue[acQEDPipelinePairs, MatchQ[#, {_List, _}] &] &&
    acZero[Total[Last /@ acQEDPipelinePairs] - acQEDPipelinePrepared["Amplitude"]] &&
    acQEDPlainFactor["Status"] === "COMPLETE" &&
    acPhysicalPass[acQEDPlainFactor["ValidationReport"]]];
acQEDPipelineFactor = acFactor[acQEDPipelinePrepared, prFourPointChannels, prModel,
  "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1}];
acQEDPipelineComplete = acEnergy[acQEDPipelineFactor, prFourPointChannels, prModel,
  "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1}];
check["the full pipeline prepares actual QED diagramAmplitudes output",
  acQEDPipelinePrepared["Status"] === "PREPARED" &&
    Length[acQEDPipelinePrepared["PreparedAmplitudes"]] === 2];
check["the prepared QED diagrams pass simultaneous factorization completion",
  acQEDPipelineFactor["Status"] === "COMPLETE"];
check["actual QED diagrams pass all final conditions through the staged pipeline",
  acQEDPipelineComplete["Status"] === "COMPLETE" &&
    acAllPass[acQEDPipelineComplete["AfterReport"]] &&
    acQEDPipelineComplete["AfterReport"]["Summary"] === <|"PASS" -> 6|>];

(* Reproduce the notebook's five-point AllSpurious construction without a supplied answer. *)
Print["Preparing the actual five-point AllSpurious diagrams."];
acQEDFivePairs = diagramAmplitudes[CreateDiagrams[prParticles, prModel]];
acQEDFivePrepared = PrepareAmplitudes[acQEDFivePairs, prModel,
  "ReferenceSpinors" -> "AllSpurious", "ProgressReporting" -> False];
acQEDFiveSaved = acQEDFivePrepared;
check["whole-expression cleanup leaves at most two compact terms per actual QED diagram",
  AllTrue[acQEDFivePrepared, Function[pair,
    If[Head[Last[pair]] === Plus, Length[Last[pair]], 1] <= 2 &&
      SpinorExpressionComplexity[Last[pair]] <= 18]]];
check["actual prepared QED diagrams are fixed points of ordinary full-expression reduction",
  AllTrue[acQEDFivePrepared, Function[pair,
    (ReduceSpinorProducts[Last[pair]] /.
      Cases[First[pair], HoldPattern[Mass[_Integer] -> _]]) === Last[pair]]]];
check["the five-point regression starts from six actual AllSpurious prepared diagrams",
  Length[acQEDFivePrepared] === 6 &&
    FreeQ[acQEDFivePrepared, Spinor["Xi", ___]]];
acQEDFiveCompleted = acFactor[acQEDFivePrepared, prChannels, prFixtureModel,
  "Samples" -> 1, "ValidationSamples" -> 1,
  "MaxCorrectionTerms" -> 128, "TimeConstraint" -> 180];
Print["Checking the automatically reconstructed five-point amplitude."];
check["automatic channel continuations complete the notebook's five-point AllSpurious input",
  acQEDFiveCompleted["Status"] === "COMPLETE" &&
    acPhysicalPass[acQEDFiveCompleted["ValidationReport"]] &&
    acQEDFivePrepared === acQEDFiveSaved];
check["the five-point completion removes auxiliary references without rewriting the prepared input",
  FreeQ[acQEDFiveCompleted["Amplitude"], Spinor["Xi", ___]] &&
    AssociationQ[acQEDFiveCompleted["ChannelContinuations"]]];
acAcceptedContinuations = Select[
  acQEDFiveCompleted["ChannelContinuations"]["Variants"], TrueQ[#["Accepted"]] &];
check["additional reference continuations are independently certified on their own selected cuts",
  acAcceptedContinuations =!= {} &&
    AnyTrue[acAcceptedContinuations, #["Mode"] === "AllPhysical" &] &&
    AllTrue[acAcceptedContinuations,
      #["Verification"]["Status"] === "PASS" &&
        Length[#["Verification"]["Samples"]] >= 2 &&
        AllTrue[#["Verification"]["Samples"], #["ResidueDifference"] === 0 &] &]];
acQEDFiveFinalCheck = CheckPhysicalResidues[acQEDFiveCompleted["Amplitude"],
  prChannels, prFixtureModel, "Samples" -> 3, "HighEnergySamples" -> 3,
  "RandomSeed" -> 11617, "ParameterRules" -> {prElectronMass -> 2, prElectricCharge -> 3},
  "Output" -> "Details", "TimeConstraint" -> 60];
check["the generated five-point result passes every independent final check at new parameters",
  acAllPass[acQEDFiveFinalCheck] &&
    acQEDFiveFinalCheck["PhysicalSummary"] === <|"PASS" -> 6|>];
acQEDOffCutValue[expression_, chart_] := Together[prMapOnChart[
  expression /. trace_SpinorTrace :>
    ConstructiveDiagrams`Private`residueChartTrace[trace, chart], chart]];
(* q and r remain symbolic, so neither internal line is put on shell. *)
acQEDFiveOffCutDifferences = Table[TimeConstrained[
  acQEDOffCutValue[acQEDFiveCompleted["Amplitude"] - prH,
    prDoubleChart[acOffCutQ, acOffCutR, i, j]], 30, $TimedOut],
  {i, 2}, {j, 2}];
check["the reconstructed expression equals H on an external-shell chart away from both cuts",
  Flatten[acQEDFiveOffCutDifferences] === ConstantArray[0, 4]];

(* Mixed reference optimization must remain compatible with off-cut completion. *)
Print["Checking optimized Mixed references through five-point completion."];
acQEDMixedPrepared = PrepareAmplitudes[acQEDFivePairs, prModel,
  "ReferenceSpinors" -> "Mixed", "MaxSteps" -> 0,
  "ProgressReporting" -> False, "ReturnDetails" -> True];
acQEDMixedSaved = acQEDMixedPrepared["PreparedAmplitudes"];
check["Mixed prepares all six real QED diagrams without changing their mass assumptions",
  acQEDMixedPrepared["Status"] === "PREPARED" &&
    Length[acQEDMixedSaved] === 6 &&
    First /@ acQEDMixedSaved === First /@ acQEDFivePairs &&
    FreeQ[acQEDMixedSaved, Spinor["Xi", ___]]];
acQEDMixedCompleted = acFactor[acQEDMixedPrepared, prChannels, prFixtureModel,
  "Samples" -> 1, "ValidationSamples" -> 1,
  "MaxCorrectionTerms" -> 128, "TimeConstraint" -> 180,
  "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1}];
acQEDMixedFinalCheck = CheckPhysicalResidues[acQEDMixedCompleted["Amplitude"],
  prChannels, prFixtureModel, "Samples" -> 3, "HighEnergySamples" -> 2,
  "RandomSeed" -> 11617, "ParameterRules" -> {prElectronMass -> 2, prElectricCharge -> 3},
  "Output" -> "Details", "TimeConstraint" -> 60];
check["the Mixed pipeline completes and passes all final checks at independent parameters",
  acQEDMixedCompleted["Status"] === "COMPLETE" &&
    acAllPass[acQEDMixedFinalCheck] &&
    acQEDMixedFinalCheck["PhysicalSummary"] === <|"PASS" -> 6|> &&
    Length[acQEDMixedFinalCheck["Checks"]] === 8 &&
    acQEDMixedPrepared["PreparedAmplitudes"] === acQEDMixedSaved];
acQEDMixedOffCutDifferences = Table[TimeConstrained[
  acQEDOffCutValue[acQEDMixedCompleted["Amplitude"] - prH,
    prDoubleChart[acOffCutQ, acOffCutR, i, j]], 30, $TimedOut],
  {i, 2}, {j, 2}];
check["the Mixed result equals H without setting either internal denominator on shell",
  Flatten[acQEDMixedOffCutDifferences] === ConstantArray[0, 4]];

(* Mixed photon helicities are different from Mixed reference selection. *)
Print["Checking mixed photon helicities from cyclic diagram preparation."];
acMixedHelicityParticles = {"e", "E", "A.+", "A.+", "A.-"};
acMixedHelicityPairs = diagramAmplitudes[CreateDiagrams[acMixedHelicityParticles, prModel]];
acMixedHelicityChannels = FactorizationAmplitudeChannels[acMixedHelicityParticles, prFixtureModel];
acMixedHelicityPrepared = PrepareAmplitudes[acMixedHelicityPairs, prModel,
  "ReferenceSpinors" -> "Cyclic", "MaxSteps" -> 0,
  "ProgressReporting" -> False] /. MomProd[3, 4] -> Mom[Multiparticle[3, 4]]^2/2;
acMixedHelicitySaved = acMixedHelicityPrepared;
acMixedHelicityCompleted = acFactor[acMixedHelicityPrepared, acMixedHelicityChannels,
  prFixtureModel, "Samples" -> 1, "ValidationSamples" -> 1,
  "MaxCorrectionTerms" -> 128, "TimeConstraint" -> 180,
  "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1}];
check["mixed helicities complete automatically with old cyclic choices and scalar momentum squares",
  acMixedHelicityCompleted["Status"] === "COMPLETE" &&
    acMixedHelicityCompleted["CorrectionSpace"] === "ReducedChannels" &&
    Length[acMixedHelicityCompleted["CorrectionBasis"]] <= 128 &&
    !FreeQ[acMixedHelicitySaved, Mom[Multiparticle[3, 4]]^2] &&
    acMixedHelicityPrepared === acMixedHelicitySaved];
check["whole-channel reductions are checked before their correction terms are admitted",
  With[{rows = acMixedHelicityCompleted["ReducedChannelSpace"]["Verification"]},
    rows =!= {} && AllTrue[rows, #["Verification"]["Status"] === "PASS" &]]];
acMixedHelicityFinalCheck = CheckPhysicalResidues[acMixedHelicityCompleted["Amplitude"],
  acMixedHelicityChannels, prFixtureModel, "ParameterRules" -> {prElectronMass -> 2, prElectricCharge -> 3},
  "Samples" -> 3, "HighEnergySamples" -> 2, "RandomSeed" -> 11617,
  "TimeConstraint" -> 90, "Output" -> "Details"];
check["mixed helicities pass every final check at independent masses, coupling and momenta",
  acAllPass[acMixedHelicityFinalCheck] &&
    acMixedHelicityFinalCheck["PhysicalSummary"] === <|"PASS" -> 6|>];
acMixedHelicityKnown = qedFivePointMixedRoute[acMixedHelicityPairs]["Final"];
acMixedHelicityOffCutDifferences = Table[TimeConstrained[
  acQEDOffCutValue[acMixedHelicityCompleted["Amplitude"] - acMixedHelicityKnown /.
    Power[p_Mom, exponent_Integer?EvenQ] :>
      Det[prMomentum[p[[1]], prDoubleChart[acOffCutQ, acOffCutR, i, j]]]^(exponent/2),
    prDoubleChart[acOffCutQ, acOffCutR, i, j]], 30, $TimedOut], {i, 2}, {j, 2}];
check["automatic mixed-helicity completion equals the independently worked result away from cuts",
  Flatten[acMixedHelicityOffCutDifferences] === ConstantArray[0, 4]];

(* Prepared compact rows are direct completion inputs, just as in the notebook. *)
Print["Checking completion with prepared factorization-channel inputs."];
acPreparedCompletionChannels = PrepareAmplitudes[prChannels, prFixtureModel,
  "ReferenceSpinors" -> "AllSpurious", "MaxSteps" -> 0,
  "ProgressReporting" -> False];
check["alternative channel representations are opt-in rather than inferred from the model",
  ("AlternativeChannels" /. Options[CompleteFactorization]) === {}];
acPreparedChannelsAlone = acFactor[acQEDFivePrepared,
  acPreparedCompletionChannels, prFixtureModel,
  "Samples" -> 1, "ValidationSamples" -> 1,
  "MaxCorrectionTerms" -> 128, "TimeConstraint" -> 180];
check["a reduced-only correction space reports its missing directions instead of claiming success",
  acPreparedChannelsAlone["Status"] === "UNRESOLVED" &&
    acPreparedChannelsAlone["ConstraintData"]["LinearSolution"]["Status"] === "INCONSISTENT"];
acPreparedChannelCompletion = acFactor[acQEDFivePrepared,
  acPreparedCompletionChannels, prFixtureModel,
  "AlternativeChannels" -> prChannels,
  "Samples" -> 1, "ValidationSamples" -> 1,
  "MaxCorrectionTerms" -> 128, "TimeConstraint" -> 180];
check["actual AllSpurious diagrams complete against prepared factorization channels",
  acPreparedChannelCompletion["Status"] === "COMPLETE" &&
    acPhysicalPass[acPreparedChannelCompletion["ValidationReport"]] &&
    acQEDFivePrepared === acQEDFiveSaved];
check["original channel alternatives are certified before expanding the reduced correction space",
  acPreparedChannelCompletion["CorrectionSpace"] === "AlternativeChannels" &&
    acPreparedChannelCompletion["AlternativeChannelFallback"]["Status"] === "COMPLETE" &&
    acPreparedChannelCompletion["AlternativeChannelFallback"]["Scope"] ===
      "ExactSampledSelectedCutEquivalenceWithUnchangedResidueTargets" &&
    Length[acPreparedChannelCompletion["AlternativeChannelFallback"]["Verification"]] === 6 &&
    AllTrue[acPreparedChannelCompletion["AlternativeChannelFallback"]["Verification"],
      #["Verification"]["Status"] === "PASS" &] &&
    Length[acPreparedChannelCompletion["CorrectionBasis"]] <= 128];
acPreparedChannelHeldOut = acCheck[acPreparedChannelCompletion["Amplitude"],
  prChannels, prFixtureModel, {prElectronMass -> 2, prElectricCharge -> 3}, 11617];
check["completion using prepared channels also passes the original channels at independent parameters",
  acPhysicalPass[acPreparedChannelHeldOut]];
acMixedPreparedChannels = PrepareAmplitudes[acMixedHelicityChannels, prFixtureModel,
  "ReferenceSpinors" -> "AllSpurious", "MaxSteps" -> 0,
  "ProgressReporting" -> False];
acMixedPreparedChannelCompletion = acFactor[acMixedHelicityCompleted["Amplitude"],
  acMixedPreparedChannels, prFixtureModel,
  "AlternativeChannels" -> acMixedHelicityChannels,
  "Samples" -> 1, "ValidationSamples" -> 1,
  "MaxCorrectionTerms" -> 128, "TimeConstraint" -> 60];
check["prepared mixed-helicity channels accept an independently completed amplitude unchanged",
  acMixedPreparedChannelCompletion["Status"] === "COMPLETE" &&
    acMixedPreparedChannelCompletion["Correction"] === 0 &&
    acZero[acMixedPreparedChannelCompletion["Amplitude"] -
      acMixedHelicityCompleted["Amplitude"]]];
check["channel alternatives are not inspected when the prepared-only comparison already succeeds",
  !KeyExistsQ[acMixedPreparedChannelCompletion, "AlternativeChannelFallback"]];
acBadAlternativeChannels = ReplacePart[prChannels,
  {1, -1} -> 2 prChannels[[1, -1]]];
acBadAlternativeCompletion = acFactor[acQEDFivePrepared,
  acPreparedCompletionChannels, prFixtureModel,
  "AlternativeChannels" -> acBadAlternativeChannels,
  "Samples" -> 1, "ValidationSamples" -> 1,
  "MaxCorrectionTerms" -> 128, "TimeConstraint" -> 180];
check["an alternative with a different selected residue is rejected instead of changing the target",
  acBadAlternativeCompletion["Status"] === "UNRESOLVED" &&
    acBadAlternativeCompletion["CorrectionSpace"] =!= "AlternativeChannels" &&
    acBadAlternativeCompletion["AlternativeChannelFallback"]["Status"] === "UNRESOLVED" &&
    AnyTrue[acBadAlternativeCompletion["AlternativeChannelFallback"]["Verification"],
      #["Verification"]["Status"] === "FAIL" &]];
acExplicitAlternativeSpace = acFactor[acQEDFivePrepared,
  acPreparedCompletionChannels, prFixtureModel,
  "CorrectionBasis" -> {}, "AlternativeChannels" -> prChannels,
  "Samples" -> 1, "ValidationSamples" -> 1,
  "MaxCorrectionTerms" -> 128, "TimeConstraint" -> 30];
check["channel alternatives never broaden an explicitly supplied correction basis",
  acExplicitAlternativeSpace["Status"] === "UNRESOLVED" &&
    acExplicitAlternativeSpace["CorrectionSpace"] === "SuppliedTerms" &&
    !KeyExistsQ[acExplicitAlternativeSpace, "AlternativeChannelFallback"]];
Do[
  check["malformed alternative channels are rejected " <> ToString[bad, InputForm],
    acInvalid[Quiet[CompleteFactorization[prH, prChannels, prFixtureModel,
      "AlternativeChannels" -> bad, "ProgressReporting" -> False]]]],
  {bad, {True, "Original", {{}}, {{0, 0, 0, 0, Multiparticle[1, 2], Infinity, 1}}}}
];

acContactNormalized = ConstructiveDiagrams`Private`completionNormalizeInput[
  {{Select[First[First[acMixedHelicityPrepared]], MatchQ[First[#], Mass[_Integer]] &],
    acPreservedContact + Total[Last /@ acMixedHelicityPrepared]}},
  acMixedHelicityChannels, prFixtureModel];
acContactCompact = ConstructiveDiagrams`Private`completionReducedChannelBasis[
  acContactNormalized, acMixedHelicityCompleted["ChannelContinuations"], 128,
  {prElectronMass -> 1, prElectricCharge -> 1}, 1729];
check["compact channel construction does not include a known pole-free input as a cancellation direction",
  AssociationQ[acContactCompact] && FreeQ[acContactCompact["Basis"], acPreservedContact]];

acCompactFallback = Block[
  {ConstructiveDiagrams`Private`completionReducedChannelBasis = Function[
    {normalized, references, limit, rules, seed}, <|"Basis" -> {}|>]},
  acFactor[prH/2, prChannels, prFixtureModel,
    "Samples" -> 2, "ValidationSamples" -> 1,
    "MaxCorrectionTerms" -> 128, "TimeConstraint" -> 90,
    "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1}]];
check["an insufficient compact strategy falls back to the retained literal channel terms",
  acCompactFallback["Status"] === "COMPLETE" &&
    acCompactFallback["CorrectionSpace"] === "LiteralTerms" &&
    Length[acCompactFallback["CorrectionSpaceAttempts"]] === 2 &&
    acPhysicalPass[acCompactFallback["ValidationReport"]]];

acQEDFiveLegacy = acFactor[acQEDFivePrepared, prChannels, prFixtureModel,
  "ChannelReferences" -> None, "Samples" -> 4, "ValidationSamples" -> 1,
  "MaxCorrectionTerms" -> 128, "TimeConstraint" -> 90];
Print["Checking retained affine families, validation feedback, and bounded failures."];
check["the legacy literal channel-term space is honestly insufficient for the five-point example",
  acQEDFiveLegacy["Status"] === "UNRESOLVED" &&
    acQEDFiveLegacy["ConstraintData"]["LinearSolution"]["Status"] === "INCONSISTENT"];
check["compact completion uses verified channel structures rather than silently reusing the old space",
  acQEDFiveCompleted["CorrectionSpace"] === "ReducedChannels" &&
    acQEDFiveCompleted["CorrectionBasis"] =!= acQEDFiveLegacy["CorrectionBasis"] &&
    AnyTrue[acQEDFiveCompleted["ReducedChannelSpace"]["Sources"],
      MemberQ[Lookup[acAcceptedContinuations, "Expression"], #] &]];

acQEDDistinct = prPreparedDiagramSum[DiagramAmplitudeRecords[
  CreateDiagrams[{"e", "E", "A.+", "A.+"}, prModel]]];
acQEDDistinctCompletion = acEnergy[acQEDDistinct, prFourPointChannels, prModel,
  "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1}, "Samples" -> 1,
  "ValidationSamples" -> 1];
check["uniquely fixed zero corrections retain a raw diagram's failed reference report",
  acQEDDistinctCompletion["Status"] === "UNRESOLVED" &&
    acZero[acQEDDistinctCompletion["Amplitude"] - acQEDDistinct] &&
    acQEDDistinctCompletion["PhysicalLinearSystem"]["FreeParameters"] === {} &&
    acQEDDistinctCompletion["BeforeReport"]["ReferenceCheck"]["Status"] === "FAIL"];
acQEDNormalization = acEnergy[prFourPointAmplitude/2, prFourPointChannels, prModel,
  "CorrectionBasis" -> {prFourPointAmplitude}, "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1}];
check["a uniquely fixed nonzero correction survives all remaining zero-unknown checks",
  acQEDNormalization["Status"] === "COMPLETE" &&
    acZero[acQEDNormalization["Amplitude"] - prFourPointAmplitude] &&
    acQEDNormalization["PhysicalLinearSystem"]["FreeParameters"] === {} &&
    acAllPass[acQEDNormalization["AfterReport"]]];
acCompletionEvents = {};
acLiveFactor = acFactor[prFourPointAmplitude, prFourPointChannels, prModel,
  "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1}, "Samples" -> 1, "ValidationSamples" -> 1,
  "ProgressReporting" -> True, "ProgressFunction" -> (AppendTo[acCompletionEvents, #] &)];
acLiveEnergy = acEnergy[acLiveFactor, prFourPointChannels, prModel,
  "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1}, "Samples" -> 1, "ValidationSamples" -> 1,
  "ProgressReporting" -> True, "ProgressFunction" -> (AppendTo[acCompletionEvents, #] &)];
check["both completion stages expose structured progress callbacks",
  acLiveFactor["Status"] === "COMPLETE" && acLiveEnergy["Status"] === "COMPLETE" &&
    AllTrue[acCompletionEvents, AssociationQ[#] && KeyExistsQ[#, "Stage"] &] &&
    AnyTrue[acCompletionEvents, StringContainsQ[#["Stage"], "factorization", IgnoreCase -> True] &] &&
    MemberQ[Lookup[acCompletionEvents, "Stage"], "HighEnergy"]];
acDefaultFactor = CompleteFactorization[prFourPointAmplitude, prFourPointChannels, prModel,
  "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1}, "Samples" -> 1,
  "ValidationSamples" -> 1, "ProgressReporting" -> False];
check["the default successful factorization output is an ordinary expression",
  !AssociationQ[acDefaultFactor] && !FailureQ[acDefaultFactor] &&
    acZero[acDefaultFactor - prFourPointAmplitude]];
check["factorization diagnostics are disabled by default without changing expression output",
  ("ReturnDetails" /. Options[CompleteFactorization]) === False &&
    ("Output" /. Options[CompleteFactorization]) === "Expression"];
acFactorFalse = CompleteFactorization[prFourPointAmplitude, prFourPointChannels, prModel,
  "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1}, "Samples" -> 1, "ValidationSamples" -> 1,
  "ProgressReporting" -> False, "ReturnDetails" -> False];
check["ReturnDetails False keeps a successfully validated factorization result plain",
  !AssociationQ[acFactorFalse] && !FailureQ[acFactorFalse] &&
    acZero[acFactorFalse - acDefaultFactor]];
acFactorDetails = CompleteFactorization[prFourPointAmplitude, prFourPointChannels, prModel,
  "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1}, "Samples" -> 1, "ValidationSamples" -> 1,
  "ProgressReporting" -> False, "ReturnDetails" -> True];
check["ReturnDetails True exposes the factorization candidate, constraints and validation history",
  AssociationQ[acFactorDetails] && acFactorDetails["Status"] === "COMPLETE" &&
    acZero[acFactorDetails["Amplitude"] - acDefaultFactor] &&
    ContainsAll[Keys[acFactorDetails], {"InitialReport", "ValidationReport", "ConstraintData"}]];
acFactorDetailsOverride = CompleteFactorization[
  prFourPointAmplitude, prFourPointChannels, prModel,
  "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1}, "Samples" -> 1, "ValidationSamples" -> 1,
  "ProgressReporting" -> False, "ReturnDetails" -> True, "Output" -> "Expression"];
check["ReturnDetails True overrides the legacy factorization Expression output choice",
  AssociationQ[acFactorDetailsOverride] && acFactorDetailsOverride["Status"] === "COMPLETE"];
check["legacy factorization Output Details remains supported",
  AssociationQ[acQEDFour] && acQEDFour["Status"] === "COMPLETE" &&
    acZero[acQEDFour["Amplitude"] - acDefaultFactor]];
acPlainFinal = CompleteHighEnergy[acDefaultFactor, prFourPointChannels, prModel,
  "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1}, "Samples" -> 1,
  "ValidationSamples" -> 1, "ProgressReporting" -> False];
check["a plain factorization result feeds final completion without extracting report fields",
  !AssociationQ[acPlainFinal] && !FailureQ[acPlainFinal] &&
    acZero[acPlainFinal - prFourPointAmplitude]];
Do[
  check["invalid factorization ReturnDetails value is rejected " <> ToString[bad, InputForm],
    acInvalid[Quiet[CompleteFactorization[prFourPointAmplitude,
      prFourPointChannels, prModel, "ReturnDetails" -> bad, "ProgressReporting" -> False]]]],
  {bad, {"Yes", 1, Automatic}}
];
acFree = acFactor[acScalarFour, acScalarFourChannels, acScalarModel,
  "CorrectionBasis" -> {1}, "ParameterRules" -> acScalarRules];
check["residue-invisible local directions are retained rather than falsely fixed",
  acFree["Status"] === "COMPLETE" && acFree["FreeParameters"] =!= {} &&
    acFree["RemainingBasis"] === {1} &&
    !FreeQ[acFree["FamilyAmplitude"], Alternatives @@ acFree["FreeParameters"]]];
acComplementNull = 1/PropDen[Mom[Multiparticle[1, 2]], acScalarMass] -
  1/PropDen[Mom[Multiparticle[3, 4]], acScalarMass];
acCleanFamily = acFactor[acScalarFour, acScalarFourChannels, acScalarModel,
  "CorrectionBasis" -> {acComplementNull, 1}, "ParameterRules" -> acScalarRules];
check["an exactly zero complementary-propagator direction is removed but local contact freedom survives",
  acCleanFamily["Status"] === "COMPLETE" &&
    acCleanFamily["CorrectionBasis"] === {1} &&
    acCleanFamily["RemainingBasis"] === {1} &&
    Length[acCleanFamily["FreeParameters"]] === 1];

(* One cut sample cannot fix an arbitrary coefficient of a varying invariant. *)
acAdaptiveTarget = First[acFourTargets];
acAdaptiveDen = acAdaptiveTarget["Propagator"];
acAdaptivePair = {First[acAdaptiveTarget["Cut"]],
  First[Complement[Range[4], acAdaptiveTarget["Cut"]]]};
acAdaptiveBasis = {1/acAdaptiveDen,
  (MomProd @@ acAdaptivePair)/acAdaptiveDen};
acNoFeedback = acFactor[0, Take[acScalarFourChannels, 1], acScalarModel,
  "CorrectionBasis" -> acAdaptiveBasis, "ParameterRules" -> acScalarRules,
  "Samples" -> 1, "ValidationSamples" -> 1, "MaxRefinements" -> 0];
check["disabling feedback leaves an underfitted affine family unresolved",
  acNoFeedback["Status"] === "UNRESOLVED" &&
    Length[acNoFeedback["RefinementHistory"]] === 1];
acAdaptive = acFactor[0, Take[acScalarFourChannels, 1], acScalarModel,
  "CorrectionBasis" -> acAdaptiveBasis, "ParameterRules" -> acScalarRules,
  "Samples" -> 1, "ValidationSamples" -> 1];
check["held-out affine-family failures become simultaneous equations and repair an underfit",
  acAdaptive["Status"] === "COMPLETE" &&
    Length[acAdaptive["RefinementHistory"]] >= 2 &&
    acAdaptive["FreeParameters"] === {} &&
    acPhysicalPass[acAdaptive["ValidationReport"]]];
acAdaptiveHistory = acAdaptive["RefinementHistory"];
check["refinement records retain fresh independent validation seeds and expanding sample counts",
  ListQ[acAdaptiveHistory] &&
    DuplicateFreeQ[Lookup[acAdaptiveHistory, "ValidationSeed"]] &&
    AllTrue[acAdaptiveHistory,
      #["FitSeed"] =!= #["ValidationSeed"] &&
        ContainsAll[Keys[#], {"Round", "EquationCount", "Rank", "FreeParameters", "Status"}] &] &&
    OrderedQ[Lookup[acAdaptiveHistory, "ValidationSamples"]] &&
    Last[Lookup[acAdaptiveHistory, "ValidationSamples"]] >
      First[Lookup[acAdaptiveHistory, "ValidationSamples"]]];
check["validation feedback retains the exact additional constraints for inspection",
  AssociationQ[acAdaptive["ConstraintData"]["Validation"]] &&
    acAdaptive["ConstraintData"]["Validation"]["Status"] === "COMPLETE" &&
    Length[acAdaptive["ConstraintData"]["Fit"]["Equations"]] >
      First[acAdaptiveHistory]["EquationCount"] &&
    AnyTrue[acAdaptive["ConstraintData"]["Fit"]["Samples"],
      KeyExistsQ[#, "RandomSeed"] && AssociationQ[Lookup[#, "Chart", None]] &]];
acExplicitInsufficient = acFactor[0, Take[acScalarFourChannels, 1], acScalarModel,
  "CorrectionBasis" -> {(MomProd @@ acAdaptivePair)/acAdaptiveDen},
  "ParameterRules" -> acScalarRules, "Samples" -> 2, "ValidationSamples" -> 1];
check["adaptive validation never invents a correction outside an explicit insufficient basis",
  acExplicitInsufficient["Status"] === "UNRESOLVED" &&
    acExplicitInsufficient["ConstraintData"]["LinearSolution"]["Status"] === "INCONSISTENT" &&
    Length[acExplicitInsufficient["CorrectionBasis"]] === 1];
acLimited = acFactor[0, acScalarFiveChannels, acScalarKnownModel,
  "ParameterRules" -> acScalarRules, "MaxCorrectionTerms" -> 1];
check["an undersized correction space is unresolved rather than silently truncated",
  acLimited["Status"] === "UNRESOLVED" && acLimited["Amplitude"] === 0];
acNoDirections = CompleteFactorization[0, acScalarFourChannels, acScalarModel,
  "CorrectionBasis" -> {}, "ParameterRules" -> acScalarRules,
  "Samples" -> 1, "ValidationSamples" -> 1, "ProgressReporting" -> False];
check["default incomplete factorization returns a compact failure rather than an unvalidated candidate",
  FailureQ[acNoDirections] && acNoDirections[[1]] === "IncompleteAmplitude" &&
    !KeyExistsQ[acNoDirections[[2]], "Report"] &&
    !KeyExistsQ[acNoDirections[[2]], "Amplitude"]];
acNoDirectionsDetails = CompleteFactorization[0, acScalarFourChannels, acScalarModel,
  "CorrectionBasis" -> {}, "ParameterRules" -> acScalarRules,
  "Samples" -> 1, "ValidationSamples" -> 1, "ProgressReporting" -> False,
  "ReturnDetails" -> True];
check["opt-in incomplete factorization details retain the candidate and diagnostic reports",
  AssociationQ[acNoDirectionsDetails] && acNoDirectionsDetails["Status"] === "UNRESOLVED" &&
    acNoDirectionsDetails["Amplitude"] === 0 &&
    ContainsAll[Keys[acNoDirectionsDetails], {"InitialReport", "Correction", "Diagnostics"}]];
acFactorTimed = acFactor[0, acScalarFiveChannels, acScalarKnownModel,
  "ParameterRules" -> acScalarRules, "TimeConstraint" -> 0.000001];
check["factorization timeouts preserve the starting candidate",
  acFactorTimed["Status"] === "UNRESOLVED" && acFactorTimed["Amplitude"] === 0];
check["automatic channel continuations and bounded validation feedback are enabled by default",
  ("ChannelReferences" /. Options[CompleteFactorization]) === Automatic &&
    ("MaxRefinements" /. Options[CompleteFactorization]) === 3];
Do[
  check["invalid factorization continuation or refinement option is rejected " <>
      ToString[bad, InputForm],
    acInvalid[Quiet[CompleteFactorization[prH, prChannels, prFixtureModel,
      bad, "ProgressReporting" -> False]]]],
  {bad, {"ChannelReferences" -> "Random", "ChannelReferences" -> False,
    "MaxRefinements" -> -1, "MaxRefinements" -> 1/2,
    "MaxRefinements" -> Infinity}}
];

(* High-energy completion uses a bounded declared family, not arbitrary contacts. *)
Print["Checking high-energy completion and preserved physical residues."];
acScalarLocalRepair = CompleteHighEnergy[{acExternalZero, 7 + MomProd[1, 2]}, {},
  acMasslessScalarModel, "Samples" -> 2, "ValidationSamples" -> 2,
  "ProgressReporting" -> False];
check["automatic high-energy completion removes an existing inadmissible local term",
  acScalarLocalRepair === 7];
acReferenceArtifact = SpinorChain[Spinor["Xi", "Angle", 71],
  Spinor["Helicity", "Angle", 3]]/
    SpinorChain[Spinor["Xi", "Angle", 71], Spinor["Helicity", "Angle", 4]];
acReferenceRepair = acEnergy[{acExternalZero, 7 + acReferenceArtifact}, {},
  acMasslessScalarModel, "CorrectionBasis" -> {acReferenceArtifact}];
check["reference-dependent spurious artifacts can be removed despite acceptable energy scaling",
  acReferenceRepair["Status"] === "COMPLETE" && acReferenceRepair["Amplitude"] === 7 &&
    acAllPass[acReferenceRepair["AfterReport"]]];
check["the final stage jointly uses spurious-pole and reference-independence constraints",
  acReferenceRepair["ConstraintData"]["Spurious"]["Equations"] =!= {} &&
    acReferenceRepair["ConstraintData"]["Reference"]["Equations"] =!= {}];
acMassiveExternal = Thread[(Mass /@ Range[4]) -> ConstantArray[acScalarMass, 4]];
acWrongMassFit = acEnergy[{acMassiveExternal, 7 + acScalarMass^2 MomProd[1, 2]}, {},
  acScalarModel, "ParameterRules" -> {acScalarMass -> 1},
  "CorrectionBasis" -> {MomProd[1, 2]}];
check["held-out mass validation rejects a coefficient fitted only at one parameter point",
  acWrongMassFit["Status"] === "UNRESOLVED"];
acRightMassFit = acEnergy[{acMassiveExternal, 7 + acScalarMass^2 MomProd[1, 2]}, {},
  acScalarModel, "ParameterRules" -> {acScalarMass -> 1},
  "CorrectionBasis" -> {acScalarMass^2 MomProd[1, 2]}];
check["a basis with the correct analytic mass dependence survives independent validation",
  acRightMassFit["Status"] === "COMPLETE" && acRightMassFit["Amplitude"] === 7];
acQEDComplete = acEnergy[prH, prChannels, prFixtureModel,
  "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1}];
check["a complete QED amplitude passes the final stage unchanged",
  acQEDComplete["Status"] === "COMPLETE" &&
    acZero[acQEDComplete["Amplitude"] - prH] && acQEDComplete["Correction"] === 0];
check["final completion exposes every independent validation category",
  acAllPass[acQEDComplete["AfterReport"]] &&
    ContainsAll[Keys[acQEDComplete["AfterReport"]],
      {"PhysicalSummary", "SpuriousSummary", "HighEnergyCheck", "ReferenceCheck"}]];
acUnder = acEnergy[acFree, acScalarFourChannels, acScalarModel,
  "ParameterRules" -> acScalarRules];
check["acceptable-growth contact freedom is honestly underdetermined",
  acUnder["Status"] === "UNDERDETERMINED" && acUnder["FreeParameters"] =!= {} &&
    acAllPass[acUnder["AfterReport"]]];
Get[FileNameJoin[{root, "tests", "simplification-paths.wl"}]];
acSMModel = ReadModel[FileNameJoin[{root, "models"}], 1];
acSMReferenceParameters = {Mh -> ModelParameter[acSMModel, "Mh"],
  MZ -> ModelParameter[acSMModel, "MZ"], MW -> ModelParameter[acSMModel, "MW"],
  SW -> ModelParameter[acSMModel, "SW"], CW -> ModelParameter[acSMModel, "CW"],
  EE -> ModelParameter[acSMModel, "EE"]};
acSMRows = Select[simplificationReferences,
  StringStartsQ[#["Name"], "SM-4-point-test.nb / h h -> Z Z / "] &] /.
    acSMReferenceParameters;
acSMFull = Total[Lookup[acSMRows, "Amplitude"]];
acSMContact = SelectFirst[acSMRows, StringEndsQ[#["Name"], "contact"] &]["Amplitude"];
acSMChannels = FactorizationAmplitudeChannels[{"h", "h", "Z", "Z"}, acSMModel];
acSMRules = {"Mh" -> 3, "MZ" -> 1, "MW" -> 4/5,
  "SW" -> 3/5, "CW" -> 4/5, "EE" -> 1};
acSMRepair = acEnergy[acSMFull - acSMContact, acSMChannels, acSMModel,
  "CorrectionBasis" -> {acSMContact}, "ParameterRules" -> acSMRules];
check["the known SM contact is recovered by cancelling longitudinal high-energy growth",
  acSMRepair["Status"] === "COMPLETE" && acZero[acSMRepair["Amplitude"] - acSMFull] &&
    acZero[acSMRepair["Correction"] - acSMContact]];
check["the SM contact repair preserves all three independently known factorization channels",
  acAllPass[acSMRepair["AfterReport"]] &&
    acSMRepair["AfterReport"]["PhysicalSummary"] === <|"PASS" -> 3|>];
acSMHeldOut = CheckPhysicalResidues[acSMRepair["Amplitude"], acSMChannels, acSMModel,
  "ParameterRules" -> {"Mh" -> 5, "MZ" -> 2, "MW" -> 8/5,
    "SW" -> 3/5, "CW" -> 4/5, "EE" -> 2},
  "Samples" -> 3, "HighEnergySamples" -> 3, "RandomSeed" -> 8191,
  "Output" -> "Details", "TimeConstraint" -> 30];
check["the SM repair survives held-out masses, coupling, momenta and vector polarizations",
  acAllPass[acSMHeldOut]];
acSMNoBasis = acEnergy[acSMFull - acSMContact, acSMChannels, acSMModel,
  "CorrectionBasis" -> {}, "ParameterRules" -> acSMRules];
check["a failed amplitude is not repaired by inventing independent contact structures",
  acSMNoBasis["Status"] === "UNRESOLVED" &&
    acZero[acSMNoBasis["Amplitude"] - (acSMFull - acSMContact)]];
acEnergyFailure = CompleteHighEnergy[acSMFull - acSMContact, acSMChannels, acSMModel,
  "CorrectionBasis" -> {}, "ParameterRules" -> acSMRules,
  "Samples" -> 1, "ValidationSamples" -> 1, "ProgressReporting" -> False];
check["default incomplete high-energy completion returns an inspectable failure",
  FailureQ[acEnergyFailure] && acEnergyFailure[[1]] === "IncompleteAmplitude" &&
    KeyExistsQ[acEnergyFailure[[2]], "Report"]];
acOld = acEnergy[prOldPermutation, prChannels, prFixtureModel,
  "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1}, "Samples" -> 1,
  "ValidationSamples" -> 1, "TimeConstraint" -> 20];
check["the old permutation formula cannot be called complete while any check fails",
  If[acOld["Status"] === "COMPLETE", acAllPass[acOld["AfterReport"]],
    MemberQ[{"UNRESOLVED", "UNDERDETERMINED"}, acOld["Status"]]]];
acEnergyTimed = acEnergy[prH, prChannels, prFixtureModel,
  "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1}, "TimeConstraint" -> 0.000001];
check["high-energy timeouts retain the supplied amplitude and mandatory diagnostic fields",
  acEnergyTimed["Status"] === "UNRESOLVED" &&
    acZero[acEnergyTimed["Amplitude"] - prH] &&
    ContainsAll[Keys[acEnergyTimed],
      {"Amplitude", "Correction", "CorrectionBasis", "FamilyAmplitude", "FreeParameters"}]];
Do[
  check["invalid completion option is rejected " <> ToString[{function, bad}, InputForm],
    acInvalid[Quiet[function[prH, prChannels, prFixtureModel, bad]]]],
  {function, {CompleteFactorization, CompleteHighEnergy}},
  {bad, {"Samples" -> 0, "ValidationSamples" -> 0, "MaxCorrectionTerms" -> 0,
    "CorrectionBasis" -> "Everything", "TimeConstraint" -> 0, "Output" -> "Everything",
    "ParameterRules" -> 42, "ParameterRules" -> {acScalarMass},
    "ProgressFunction" -> 42, "UnknownCompletionOption" -> 1}}
];
Do[
  check["malformed completion calls are rejected " <> ToString[function, InputForm],
    acInvalid[Quiet[function[1]]]],
  {function, {CompleteFactorization, CompleteHighEnergy}}
];

Print[checks, " amplitude-completion checks; ", Length[failures], " failures."];
If[failures =!= {}, Print[failures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:amplitude-completion-tests.wl"];
Quit[0];
