root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
SetEnvironment["HEPCAT_TEST_ROOT" -> root];
notebook = ToExpression[Import[FileNameJoin[{root, "tests", "SM-4-point-unscramble-test.nb"}],
  "Text"], InputForm, HoldComplete];
inputs = Cases[notebook, Cell[benchmarkCellText_String, "Input", ___] :> benchmarkCellText, Infinity];
Scan[ToExpression, Take[inputs, 3]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]]);
check["same process and reference coverage", Length[ubCases] === 73 && Total[Length /@ Lookup[ubCases, "Channels"]] === 148];
check["all references resolve", And @@ Flatten[Table[
  !MissingQ[ubReference[test["SourceName"], channel]], {test, ubCases}, {channel, test["Channels"]}]]];
check["all permutations enabled", particlePermutations === Permutations[Range[4]]];
check["compact model selected by default", unscrambleModel === "CompactActions"];
check["benchmark exposes unlimited productive steps with configurable patience and live progress",
  maxSteps === Infinity && IntegerQ[maxStagnantSteps] && maxStagnantSteps > 0 && liveProgress === True];
check["benchmark defaults also apply to direct solver calls", AllTrue[
  {UnscrambleSpinorAmplitudes, UnscrambleTrace},
  (Lookup[Association[Options[#]], {"MaxSteps", "MaxStagnantSteps", "ProgressReporting"}] ===
    {maxSteps, maxStagnantSteps, liveProgress}) &]];
demoNotebook = ToExpression[Import[FileNameJoin[{root, "tests", "unscrambling.nb"}],
  "Text"], InputForm, HoldComplete];
demoLoadInputs = Cases[demoNotebook, Cell[code_String, "Input", opts___] /;
  MemberQ[{opts}, ExpressionUUID -> "08dbf5e5-9e48-48bf-96d6-d1623153cea0"] :> code, Infinity];
check["both notebooks parse without evaluating their cells",
  Count[notebook, _Notebook, Infinity] === 1 && Count[demoNotebook, _Notebook, Infinity] === 1];
check["demo load settings have valid held syntax", Length[demoLoadInputs] === 1 &&
  Quiet[Check[ToExpression[First[demoLoadInputs], InputForm, HoldComplete] =!= $Failed, False]]];
check["demo uses unlimited productive steps with ten-step stagnation and live progress",
  Length[demoLoadInputs] === 1 && AllTrue[
    {"maxSteps = Infinity;", "maxStagnantSteps = 10;", "liveProgress = True;"},
    StringContainsQ[First[demoLoadInputs], #] &] &&
  StringCount[First[demoLoadInputs], "\"MaxSteps\" -> maxSteps"] === 2 &&
  StringCount[First[demoLoadInputs], "\"MaxStagnantSteps\" -> maxStagnantSteps"] === 2 &&
  StringCount[First[demoLoadInputs], "\"ProgressReporting\" -> liveProgress"] === 2];
check["all ingoing labels", AllTrue[ubCases, #["Name"] === StringRiffle[#["Particles"], ", "] &]];
check["status colors", ubStatus["PASS"] === Style["PASS", Bold, Darker[Green]] &&
  ubStatus["Unresolved"] === Style["Unresolved", Bold, Red]];
check["channel label follows permutation", ubChannel["Z:T", {2, 1, 3, 4}] === "Z:U"];
permutation = {3, 1, 4, 2};
probe = {Mass[Multiparticle[1, 2]] -> MZ,
  7 Spinor["Spin", "Angle", 1, 2]^2 MomHat[2] xFactor[Multiparticle[1, 4], 3] +
  Mandelstahm[2, 4] MomProd[1, 3] + Mom[Multiparticle[2, 3]] + Spinor["Xi", "Angle", 10]};
check["typed relabeling", ubRelabel[probe, permutation] === {
  Mass[Multiparticle[3, 1]] -> MZ,
  7 Spinor["Spin", "Angle", 3, 2]^2 MomHat[1] xFactor[Multiparticle[3, 2], 4] +
  Mandelstahm[1, 2] MomProd[3, 4] + Mom[Multiparticle[1, 4]] + Spinor["Xi", "Angle", 10]}];
check["all permutations invert", AllTrue[particlePermutations,
  ubRelabel[ubRelabel[probe, #], Ordering[#]] === probe &]];
check["hatted objects relabeled", ubRelabel[
  SpinorHat["Spin", "Up", "Angle", 1, 2] MomProdHat12[2, 4], permutation] ===
  SpinorHat["Spin", "Up", "Angle", 3, 2] MomProdHat12[1, 2]];
check["no legacy simplifier in runner", !StringContainsQ[inputs[[3]], "SimplifySpinorProducts"]];
check["runner uses public paired diagram amplitudes",
  StringContainsQ[inputs[[3]], "diagramAmplitudes[diagrams]"] &&
    !StringContainsQ[inputs[[3]], "diagramAmplitude /@"]];
verificationSeconds = 1;
check["same expression accepted", ubCompare[EE, EE, {0, 0, 0, 0}]["Equivalence"] === "Exact"];
check["algebraically equivalent accepted", ubCompare[(EE + GG)^2, EE^2 + 2 EE GG + GG^2,
  {0, 0, 0, 0}]["Equivalence"] === "Proved"];
check["wrong sign is not a pass", ubCompare[EE, -EE, {0, 0, 0, 0}]["Equivalence"] === "OppositeSign"];
check["unproved comparison stays unresolved", ubCompare[EE, GG, {0, 0, 0, 0}]["Equivalence"] === "Unresolved"];
externalX = ConstructiveDiagrams`expandExternalXFactors;
fivePointX = xFactor[1, Multiparticle[2, 3, 4]];
fivePointExpanded =
  SpinorChain[Spinor["Xi", "Angle", 10], Mom[Multiparticle[2, 3, 4]],
    Spinor["Helicity", "Square", 5]]/
  (Mass[Multiparticle[2, 3, 4]] SpinorChain[Spinor["Xi", "Angle", 10],
    Spinor["Helicity", "Angle", 5]]);
check["external x identifies fifth leg from composite indices",
  externalX[fivePointX, 5] === fivePointExpanded];
sixPointTilde = xTildeFactor[Multiparticle[1, 2, 3], Multiparticle[4, 5]];
sixPointExpanded =
  SpinorChain[Spinor["Xi", "Square", 10], Mom[Multiparticle[4, 5]],
    Spinor["Helicity", "Angle", 6]]/
  (Mass[Multiparticle[4, 5]] SpinorChain[Spinor["Xi", "Square", 10],
    Spinor["Helicity", "Square", 6]]);
check["external tilde identifies sixth leg from composite indices",
  externalX[sixPointTilde, 6] === sixPointExpanded];
ambiguousX = xFactor[1, 2] + xTildeFactor[1, 2];
check["ambiguous external factors keep their original heads", externalX[ambiguousX, 4] === ambiguousX];
mixedExternalX = externalX[fivePointX xTildeFactor[Multiparticle[1, 2, 3], 4], 5];
check["mixed external factors use distinct reference spinors",
  FreeQ[mixedExternalX, _xFactor | _xTildeFactor] &&
    Sort[DeleteDuplicates[Cases[mixedExternalX, Spinor["Xi", _, i_Integer] :> i, Infinity]]] === {10, 11}];
denominatorX = fivePointX/PropDen[Mom[Multiparticle[1, 2, 5]], MZ];
check["external factor expansion preserves propagator denominators",
  Cases[externalX[denominatorX, 5], _PropDen, Infinity] === Cases[denominatorX, _PropDen, Infinity]];
check["legacy external expansion shares the same replacement", ExpandXFactors[denominatorX, 5] === externalX[denominatorX, 5]];
internalXIdentity =
  xFactor[1, 2] xTildeFactor[3, 4]*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] +
  xFactor[3, 4] xTildeFactor[1, 2]*
    SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]];
check["external helper does not apply internal exchange identities", externalX[internalXIdentity, 4] === internalXIdentity];
rawDiagramAmplitude[diag_] := Block[
  {ConstructiveDiagrams`expandExternalXFactors, ConstructiveDiagrams`diagramMassReplacements},
  Clear[ConstructiveDiagrams`expandExternalXFactors];
  Clear[ConstructiveDiagrams`diagramMassReplacements];
  ConstructiveDiagrams`expandExternalXFactors[expr_, n_] := expr;
  ConstructiveDiagrams`diagramMassReplacements[_] := {};
  diagramAmplitude[diag]
];
Do[
  xDiagrams = TimeConstrained[CreateDiagrams[xParticles, ubModel], 20, $Failed];
  check["external-factor fixture generates diagrams " <> ToString[xParticles, InputForm],
    ListQ[xDiagrams] && Length[xDiagrams] > 0];
  If[ListQ[xDiagrams] && Length[xDiagrams] > 0,
    rawXAmplitudes = rawDiagramAmplitude /@ xDiagrams;
    expandedXAmplitudes = diagramAmplitude /@ xDiagrams;
    unresolvedXAmplitudes = externalX[#, Length[xParticles]] & /@ rawXAmplitudes;
    expectedXAmplitudes = MapThread[
      (#1 /. ConstructiveDiagrams`diagramMassReplacements[#2]) &,
      {unresolvedXAmplitudes, xDiagrams}];
    check["external-factor fixture has raw x factors " <> ToString[xParticles, InputForm],
      !FreeQ[rawXAmplitudes, _xFactor | _xTildeFactor]];
    check["diagram amplitude expands external factors then its own mass rules " <> ToString[xParticles, InputForm],
      expandedXAmplitudes === expectedXAmplitudes &&
        FreeQ[expandedXAmplitudes, _xFactor | _xTildeFactor]];
    check["external-factor expansion introduces known mass placeholders " <> ToString[xParticles, InputForm],
      !FreeQ[unresolvedXAmplitudes, Mass[_Integer]] &&
        !FreeQ[unresolvedXAmplitudes, Mass[_Multiparticle]]];
    check["diagram amplitude resolves external and multiparticle masses " <> ToString[xParticles, InputForm],
      FreeQ[expandedXAmplitudes, _Mass]];
    bulkXAmplitudes = diagramAmplitudes[xDiagrams];
    check["unpaired bulk diagrams retain their rules and resolved expressions " <> ToString[xParticles, InputForm],
      bulkXAmplitudes === MapThread[
        {ConstructiveDiagrams`diagramMassReplacements[#1], #2} &, {xDiagrams, expectedXAmplitudes}]];
    check["diagram propagators survive external-factor expansion " <> ToString[xParticles, InputForm],
      Cases[expandedXAmplitudes, _PropDen, Infinity] === Cases[rawXAmplitudes, _PropDen, Infinity]]
  ],
  {xParticles, {{"e", "E", "A.+", "h"}, {"A.-", "E", "e", "h"},
    {"u", "U", "G.-", "h"}, {"e", "E", "A.+", "h", "h"}}}
];
mixedMassDiagrams = Join @@ (CreateDiagrams[#, ubModel] & /@
  {{"e", "E", "A.+", "h"}, {"u", "U", "A.+", "h"}});
mixedMassExpected = Function[diag, Module[{rules = ConstructiveDiagrams`diagramMassReplacements[diag]},
  {rules, externalX[rawDiagramAmplitude[diag], 4] /. rules}
]] /@ mixedMassDiagrams;
check["mixed diagram fixtures have different masses for the same leg",
  DeleteDuplicates[Mass[2] /. mixedMassExpected[[All, 1]]] === {Me, Mu}];
check["bulk amplitudes use per-diagram masses rather than a shared rule union",
  diagramAmplitudes[mixedMassDiagrams] === mixedMassExpected];
Do[
  pairedMassDiagrams = ubSelect[CreateDiagrams[pairedParticles[[pairedPermutation]], ubModel], "A", False];
  pairedMassGroups = Values[GroupBy[pairedMassDiagrams, ConstructiveDiagrams`masslessSpin1PairKey]];
  check["massless-vector mass fixture has paired diagrams " <> ToString[{pairedParticles, pairedPermutation}, InputForm],
    Length[pairedMassGroups] > 0 && AllTrue[pairedMassGroups, Length[#] === 2 &]];
  Do[
    pairedMassRules = ConstructiveDiagrams`diagramMassReplacements[First[pairedMassGroup]];
    unresolvedPairedMass = Block[{ConstructiveDiagrams`diagramMassReplacements},
      Clear[ConstructiveDiagrams`diagramMassReplacements];
      ConstructiveDiagrams`diagramMassReplacements[_] := {};
      ConstructiveDiagrams`masslessSpin1PairAmplitude[First[pairedMassGroup]]
    ];
    resolvedPairedMass = diagramAmplitudes[pairedMassGroup];
    check["paired vector shortcut introduces a known external mass " <> ToString[{pairedParticles, pairedPermutation, pairedMassRules}, InputForm],
      !FreeQ[unresolvedPairedMass, Mass[_Integer]]];
    check["paired vector shortcut resolves its mass and retains its rules " <> ToString[{pairedParticles, pairedPermutation, pairedMassRules}, InputForm],
      resolvedPairedMass === {{pairedMassRules, unresolvedPairedMass /. pairedMassRules}} &&
        FreeQ[resolvedPairedMass[[All, 2]], _Mass]];
    check["paired vector mass replacement preserves propagators " <> ToString[{pairedParticles, pairedPermutation, pairedMassRules}, InputForm],
      Cases[resolvedPairedMass[[All, 2]], _PropDen, Infinity] ===
        Cases[unresolvedPairedMass, _PropDen, Infinity]],
    {pairedMassGroup, pairedMassGroups}
  ],
  {pairedParticles, {{"e", "E", "W+", "W-"}, {"W+", "W-", "W+", "W-"}}},
  {pairedPermutation, {Range[4], permutation}}
];
(* The notebook's Z example needs explicit composite-momentum edits before
   endpoint mass identities can act. This tests the legal path, not a model. *)
zCompositeDiagrams = ubSelect[CreateDiagrams[{"e", "E", "M", "m"}, ubModel], "Z", False];
zCompositePairs = diagramAmplitudes[zCompositeDiagrams];
zCompositeRules = First[zCompositePairs][[1]];
zCompositeOriginal = Unscrambling`Private`canonicalAmplitude[
  ReduceSpinContractions[Total[zCompositePairs[[All, 2]]]], zCompositeRules];
zCompositePropagators = DeleteDuplicates[Cases[zCompositeOriginal, _PropDen, Infinity]];
allCuratedRoutes = trainingSimplificationRoutes;
zCuratedRoute = First[allCuratedRoutes];
check["fifteen curated routes remain separate from the reference catalog",
  Length[allCuratedRoutes] === 15 && DuplicateFreeQ[Lookup[allCuratedRoutes, "Name"]] &&
    Unscrambling`Private`validSimplificationRoutesQ[allCuratedRoutes]];
check["curated Z path begins at the public diagram and spin-contraction boundary",
  zCuratedRoute["Masses"] === zCompositeRules &&
    Unscrambling`Private`samePoly[First[zCuratedRoute["States"]], zCompositeOriginal]];
check["curated Z path preserves every propagator in every state", AllTrue[zCuratedRoute["States"],
  DeleteDuplicates[Cases[#, _PropDen, Infinity]] === zCompositePropagators &]];
check["curated Z endpoint needs no further momentum-chain reductions",
  FreeQ[Cases[Last[zCuratedRoute["States"]], _SpinorChain, Infinity], _Mom]];
check["curated Z endpoint agrees with the reference without a spinor reducer",
  Unscrambling`Private`samePoly[
    ExtractMandelstahm[Last[zCuratedRoute["States"]]] /. MW -> MZ CW,
    ubReference["e E -> M m", "Z"]]];
eezhCuratedPairs = ReduceSpinContractions[
  diagramAmplitudes[CreateDiagrams[{"e", "E", "Z", "h"}, ubModel]]];
check["fresh eeZh diagrams contain all three handwritten channels", Length[eezhCuratedPairs] === 3];
eezhCuratedStepCounts = <|"Z" -> 52, "e:T" -> 10, "e:U" -> 4|>;
Do[
  eezhCuratedPropagators = DeleteDuplicates[Cases[eezhCuratedPair[[2]], _PropDen, Infinity]];
  eezhCuratedChannel = Switch[First[eezhCuratedPropagators],
    PropDen[Mom[Multiparticle[1, 2]], MZ], "Z",
    PropDen[Mom[Multiparticle[1, 3]], Me], "e:T",
    PropDen[Mom[Multiparticle[1, 4]], Me], "e:U", _, "Unknown"];
  eezhCuratedRoute = SelectFirst[Rest[allCuratedRoutes],
    #["Masses"] === eezhCuratedPair[[1]] &&
      DeleteDuplicates[Cases[First[#["States"]], _PropDen, Infinity]] === eezhCuratedPropagators &, $Failed];
  check["curated eeZh route matches diagram masses and propagator " <> eezhCuratedChannel,
    AssociationQ[eezhCuratedRoute] && KeyExistsQ[eezhCuratedStepCounts, eezhCuratedChannel]];
  If[AssociationQ[eezhCuratedRoute] && KeyExistsQ[eezhCuratedStepCounts, eezhCuratedChannel],
    check["curated eeZh path starts at the public diagram boundary " <> eezhCuratedChannel,
      Unscrambling`Private`samePoly[First[eezhCuratedRoute["States"]],
        Unscrambling`Private`canonicalAmplitude[eezhCuratedPair[[2]], eezhCuratedPair[[1]]]]];
    check["curated eeZh elementary identities match every transition " <> eezhCuratedChannel,
      Length[eezhCuratedRoute["Identities"]] === eezhCuratedStepCounts[eezhCuratedChannel] &&
        Length[eezhCuratedRoute["States"]] === eezhCuratedStepCounts[eezhCuratedChannel] + 1];
    check["curated eeZh path preserves its propagator in every state " <> eezhCuratedChannel,
      AllTrue[eezhCuratedRoute["States"],
        DeleteDuplicates[Cases[#, _PropDen, Infinity]] === eezhCuratedPropagators &]];
    check["curated eeZh endpoint agrees with the accepted reference " <> eezhCuratedChannel,
      Unscrambling`Private`samePoly[
        Unscrambling`Private`canonicalChains[ExtractMandelstahm[Last[eezhCuratedRoute["States"]]]],
        Unscrambling`Private`canonicalChains[ubReference["e E -> Z h", eezhCuratedChannel]]]]
  ],
  {eezhCuratedPair, eezhCuratedPairs}
];
eezzCuratedPairs = ReduceSpinContractions[
  diagramAmplitudes[CreateDiagrams[{"e", "E", "Z", "Z"}, ubModel]]];
eezzElectronPairs = Select[eezzCuratedPairs,
  !FreeQ[#[[2]], PropDen[Mom[Multiparticle[1, 3] | Multiparticle[1, 4]], Me]] &];
eezzElectronRoutes = Select[allCuratedRoutes,
  StringStartsQ[#["Name"], "e, E, Z, Z / electron"] &];
check["fresh eeZZ diagrams contain both curated electron channels", Length[eezzElectronPairs] === 2];
eezzBrackets[text_] := Unscrambling`Private`canonicalChains[
  ConvertStringAmplitude[text, {Me, Me, MZ, MZ}]];
eezzElectronReference[channel_] := If[channel === "e:T",
  EE^2/(2 MW^2 SW^2 PropDen[Mom[Multiparticle[1, 3]], Me])*
    (gLe^2 eezzBrackets["[24]<13>"]*(MZ eezzBrackets["<34>"] + eezzBrackets["[314>"]) +
     gRe^2 eezzBrackets["[13]<24>"]*(MZ eezzBrackets["[34]"] + eezzBrackets["[413>"]) +
     gLe gRe Me eezzBrackets["[13][24]<34>+[34]<13><24>"]),
  -EE^2/(2 MW^2 SW^2 PropDen[Mom[Multiparticle[1, 4]], Me])*
    (gLe^2 eezzBrackets["[23]<14>"]*(MZ eezzBrackets["<34>"] - eezzBrackets["[413>"]) +
     gRe^2 eezzBrackets["[14]<23>"]*(MZ eezzBrackets["[34]"] - eezzBrackets["[314>"]) +
     gLe gRe Me eezzBrackets["[14][23]<34>+[34]<14><23>"])
];
Do[
  eezzCuratedPropagators = DeleteDuplicates[Cases[eezzCuratedPair[[2]], _PropDen, Infinity]];
  eezzCuratedChannel = Switch[First[eezzCuratedPropagators],
    PropDen[Mom[Multiparticle[1, 3]], Me], "e:T",
    PropDen[Mom[Multiparticle[1, 4]], Me], "e:U", _, "Unknown"];
  eezzCuratedRoute = SelectFirst[eezzElectronRoutes,
    #["Masses"] === eezzCuratedPair[[1]] &&
      DeleteDuplicates[Cases[First[#["States"]], _PropDen, Infinity]] === eezzCuratedPropagators &, $Failed];
  check["curated eeZZ route matches fresh diagram masses and propagator " <> eezzCuratedChannel,
    AssociationQ[eezzCuratedRoute] && MemberQ[{"e:T", "e:U"}, eezzCuratedChannel]];
  If[AssociationQ[eezzCuratedRoute] && MemberQ[{"e:T", "e:U"}, eezzCuratedChannel],
    check["curated eeZZ path starts at the mass-resolved spin-contraction boundary " <> eezzCuratedChannel,
      Unscrambling`Private`samePoly[First[eezzCuratedRoute["States"]],
        Unscrambling`Private`canonicalAmplitude[eezzCuratedPair[[2]], eezzCuratedPair[[1]]]]];
    check["curated eeZZ path has four elementary edits and five states " <> eezzCuratedChannel,
      Length[eezzCuratedRoute["States"]] === 5 &&
        Counts[eezzCuratedRoute["Identities"]] === <|"MomentumSum" -> 2, "Mass" -> 2|>];
    check["curated eeZZ path preserves its propagator in every state " <> eezzCuratedChannel,
      AllTrue[eezzCuratedRoute["States"],
        DeleteDuplicates[Cases[#, _PropDen, Infinity]] === eezzCuratedPropagators &]];
    check["curated eeZZ endpoint agrees with the notebook channel reference " <> eezzCuratedChannel,
      Unscrambling`Private`samePoly[Last[eezzCuratedRoute["States"]],
        Unscrambling`Private`canonicalChains[eezzElectronReference[eezzCuratedChannel]]]]
  ],
  {eezzCuratedPair, eezzElectronPairs}
];
check["both eeZZ electron endpoints reproduce the full accepted electron reference",
  (* The notebook writes the T denominator as Me^2 - s13 rather than s13 - Me^2. *)
  Together[Unscrambling`Private`canonicalChains[
    ExtractMandelstahm[Total[Last /@ Lookup[eezzElectronRoutes, "States"]]] -
      ubReference["e E -> Z Z", "e"]]] === 0];
eezzCuratedJobs = Unscrambling`Private`makeSimplificationRouteJobs[eezzElectronRoutes, All];
check["both eeZZ electron routes receive all particle-label permutations", Length[eezzCuratedJobs] === 48 &&
  Sort[Values[Counts[Lookup[eezzCuratedJobs, "Name"]]]] === {24, 24}];
Block[{Unscrambling`Private`$onShellChannels = Automatic,
    Unscrambling`Private`$useMomentumConservation = True},
  Do[
    eezzJobRules = eezzCuratedJob["Pair"][[1]];
    eezzJobStates = Unscrambling`Private`canonicalAmplitude[#, eezzJobRules] & /@ eezzCuratedJob["States"];
    eezzJobIdentities = SelectFirst[eezzElectronRoutes, #["Name"] === eezzCuratedJob["Name"] &]["Identities"];
    Do[
      eezzLegalCandidates = Unscrambling`Private`legalMoves[eezzJobStates[[eezzStep]], eezzJobRules];
      check["eeZZ permuted transition remains a legal elementary edit " <>
        ToString[{eezzCuratedJob["Name"], eezzCuratedJob["LegPermutation"], eezzStep}, InputForm],
        AnyTrue[eezzLegalCandidates, Function[candidate,
          candidate["Name"] === eezzJobIdentities[[eezzStep]] &&
            Unscrambling`Private`samePoly[
              Unscrambling`Private`applyCandidate[eezzJobStates[[eezzStep]], candidate],
              eezzJobStates[[eezzStep + 1]]]]]],
      {eezzStep, Length[eezzJobStates] - 1}],
    {eezzCuratedJob, eezzCuratedJobs}]
];
eewwCuratedRoute = SelectFirst[allCuratedRoutes,
  #["Name"] === "e, E, W-, W+ / Z exchange" &, $Failed];
eewwCuratedDiagrams = ubSelect[CreateDiagrams[{"e", "E", "W-", "W+"}, ubModel], "Z", False];
eewwCuratedPairs = ReduceSpinContractions[diagramAmplitudes[eewwCuratedDiagrams]];
check["fresh eeWW Z diagrams have one matching curated route",
  AssociationQ[eewwCuratedRoute] && Length[eewwCuratedPairs] === 1];
If[AssociationQ[eewwCuratedRoute] && Length[eewwCuratedPairs] === 1,
  eewwCuratedPair = First[eewwCuratedPairs];
  eewwCuratedPropagators = DeleteDuplicates[Cases[eewwCuratedPair[[2]], _PropDen, Infinity]];
  check["curated eeWW Z path starts at the mass-resolved public diagram boundary",
    eewwCuratedRoute["Masses"] === eewwCuratedPair[[1]] &&
      Unscrambling`Private`samePoly[First[eewwCuratedRoute["States"]],
        Unscrambling`Private`canonicalAmplitude[eewwCuratedPair[[2]], eewwCuratedPair[[1]]]]];
  check["curated eeWW Z path retains all 302 elementary edits",
    Length[eewwCuratedRoute["States"]] === 303 &&
      Counts[eewwCuratedRoute["Identities"]] ===
        <|"MomentumSum" -> 32, "Mass" -> 172, "Schouten" -> 40,
          "Anticommutation" -> 8, "OnShell" -> 8, "MomentumConservation" -> 42|>];
  check["curated eeWW Z path keeps its propagator opaque in every state",
    eewwCuratedPropagators === {PropDen[Mom[Multiparticle[1, 2]], MZ]} &&
      AllTrue[eewwCuratedRoute["States"],
        DeleteDuplicates[Cases[#, _PropDen, Infinity]] === eewwCuratedPropagators &]];
  check["curated eeWW Z states contain no unresolved mass placeholders", FreeQ[eewwCuratedRoute["States"], _Mass]];
  check["curated eeWW Z endpoint agrees with the accepted notebook reference",
    Together[Unscrambling`Private`canonicalChains[
      ExtractMandelstahm[Last[eewwCuratedRoute["States"]]] - ubReference["e E -> W- W+", "Z"]]] === 0];
  check["curated eeWW Z route reduces complexity from 376 to 22",
    SpinorExpressionComplexity /@ eewwCuratedRoute["States"][[{1, -1}]] === {376, 22}]
];
curatedPermutationReplayQ[permutedStates_, originalEdits_, ids_] := Module[{states, edits},
  If[Length[originalEdits] =!= Length[permutedStates] - 1, Return[False]];
  states = Unscrambling`Private`canonicalChains /@ permutedStates;
  edits = (Join[#, <|
    "Subtract" -> Unscrambling`Private`canonicalChains[Unscrambling`Private`relabelLegs[#["Subtract"], ids]],
    "Insert" -> Unscrambling`Private`canonicalChains[Unscrambling`Private`relabelLegs[#["Insert"], ids]]|>] &) /@
    originalEdits;
  And @@ MapThread[Unscrambling`Private`samePoly[Unscrambling`Private`applyCandidate[#1, #3], #2] &,
    {Most[states], Rest[states], edits}]
];
zhwwCuratedRoute = SelectFirst[allCuratedRoutes, #["Name"] === "Z, h, W-, W+ / Z exchange" &, $Failed];
zhwwCuratedDiagrams = ubSelect[CreateDiagrams[{"Z", "h", "W-", "W+"}, ubModel], "Z:S", False];
zhwwCuratedPairs = ReduceSpinContractions[diagramAmplitudes[zhwwCuratedDiagrams]];
check["fresh bosonic Z exchange has one matching curated route",
  AssociationQ[zhwwCuratedRoute] && Length[zhwwCuratedPairs] === 1];
If[AssociationQ[zhwwCuratedRoute] && Length[zhwwCuratedPairs] === 1,
  zhwwCuratedPair = First[zhwwCuratedPairs];
  zhwwCuratedStates = zhwwCuratedRoute["States"];
  zhwwCuratedPole = {PropDen[Mom[Multiparticle[1, 2]], MZ]};
  check["bosonic Z route begins at the mass-resolved public diagram boundary",
    zhwwCuratedRoute["Masses"] === zhwwCuratedPair[[1]] &&
      Unscrambling`Private`samePoly[First[zhwwCuratedStates],
        Unscrambling`Private`canonicalAmplitude[zhwwCuratedPair[[2]], zhwwCuratedPair[[1]]]]];
  check["bosonic Z endpoint preserves the source notebook's accepted opposite sign",
    zhwwCuratedRoute["TemplateSign"] === -1 &&
      Together[Unscrambling`Private`canonicalChains[ExtractMandelstahm[Last[zhwwCuratedStates]] +
        ubReference["Z h -> W- W+", "Z:S"]]] === 0];
  check["bosonic Z route reduces complexity from 188 to 14",
    SpinorExpressionComplexity /@ zhwwCuratedStates[[{1, -1}]] === {188, 14}];
  check["bosonic Z route preserves its off-shell propagator in every state",
    AllTrue[zhwwCuratedStates, DeleteDuplicates[Cases[#, _PropDen, Infinity]] === zhwwCuratedPole &]];
  zhwwRecordedEdits = {};
  Block[{Unscrambling`Private`$onShellChannels = Automatic,
    Unscrambling`Private`$useMomentumConservation = True},
    Do[
      zhwwCuratedFamily = zhwwCuratedRoute["Identities"][[step]];
      zhwwCuratedEdits = Switch[zhwwCuratedFamily,
        "MomentumSum", Unscrambling`Private`momentumSumMoves[zhwwCuratedStates[[step]], zhwwCuratedPair[[1]]],
        "Mass" | "MomentumConservation", Unscrambling`Private`chainMoves[zhwwCuratedStates[[step]], zhwwCuratedPair[[1]]],
        "Schouten", Unscrambling`SchoutenCandidates[zhwwCuratedStates[[step]]],
        "Anticommutation", Unscrambling`Private`interiorChainMoves[zhwwCuratedStates[[step]], zhwwCuratedPair[[1]]],
        "OnShell", Unscrambling`Private`kinematicMoves[zhwwCuratedStates[[step]], zhwwCuratedPair[[1]]],
        _, {}];
      zhwwMatchedEdit = SelectFirst[zhwwCuratedEdits, Function[edit,
        edit["Name"] === zhwwCuratedFamily &&
          Unscrambling`Private`samePoly[Unscrambling`Private`applyCandidate[zhwwCuratedStates[[step]], edit],
            zhwwCuratedStates[[step + 1]]]], $Failed];
      check["bosonic Z elementary identity " <> ToString[step], AssociationQ[zhwwMatchedEdit]];
      If[AssociationQ[zhwwMatchedEdit], AppendTo[zhwwRecordedEdits, zhwwMatchedEdit]],
      {step, Length[zhwwCuratedStates] - 1}]];
  zhwwCuratedJobs = Unscrambling`Private`makeSimplificationRouteJobs[{zhwwCuratedRoute}, All];
  check["bosonic Z route receives every particle permutation", Length[zhwwCuratedJobs] === 24 &&
    DuplicateFreeQ[Lookup[zhwwCuratedJobs, "LegPermutation"]]];
  Do[
    check["bosonic Z permuted boundary, target, and propagator " <>
      ToString[Values[zhwwCuratedJob["LegPermutation"]], InputForm],
      Unscrambling`Private`samePoly[First[zhwwCuratedJob["States"]],
        Unscrambling`Private`relabelLegs[First[zhwwCuratedStates], zhwwCuratedJob["LegPermutation"]]] &&
      Unscrambling`Private`samePoly[Last[zhwwCuratedJob["States"]],
        Unscrambling`Private`relabelLegs[Last[zhwwCuratedStates], zhwwCuratedJob["LegPermutation"]]] &&
      AllTrue[zhwwCuratedJob["States"], DeleteDuplicates[Cases[#, _PropDen, Infinity]] ===
        Unscrambling`Private`relabelLegs[zhwwCuratedPole, zhwwCuratedJob["LegPermutation"]] &]];
    check["bosonic Z permuted intermediate applications " <>
      ToString[Values[zhwwCuratedJob["LegPermutation"]], InputForm],
      curatedPermutationReplayQ[zhwwCuratedJob["States"], zhwwRecordedEdits, zhwwCuratedJob["LegPermutation"]]],
    {zhwwCuratedJob, zhwwCuratedJobs}]
];
chargedCurrentRoutes = Select[allCuratedRoutes,
  MemberQ[{"ne, E, W-, h / W+ exchange", "u, D, W-, h / W+ exchange"}, #["Name"]] &];
check["both charged-current W exchange routes are retained", Length[chargedCurrentRoutes] === 2];
Do[
  chargedCurrentDiagrams = ubSelect[CreateDiagrams[chargedCurrentSpecification[[2]], ubModel], "W+", False];
  chargedCurrentPairs = ReduceSpinContractions[diagramAmplitudes[chargedCurrentDiagrams]];
  chargedCurrentRoute = SelectFirst[chargedCurrentRoutes,
    #["Name"] === chargedCurrentSpecification[[1]] &, $Failed];
  check["fresh charged-current boundary " <> chargedCurrentSpecification[[1]],
    AssociationQ[chargedCurrentRoute] && Length[chargedCurrentPairs] === 1];
  If[AssociationQ[chargedCurrentRoute] && Length[chargedCurrentPairs] === 1,
    chargedCurrentPair = First[chargedCurrentPairs];
    chargedCurrentStates = chargedCurrentRoute["States"];
    chargedCurrentPole = {PropDen[Mom[Multiparticle[1, 2]], MW]};
    check["charged-current route begins at its public diagram boundary " <> chargedCurrentSpecification[[1]],
      chargedCurrentRoute["Masses"] === chargedCurrentPair[[1]] &&
        Unscrambling`Private`samePoly[First[chargedCurrentStates],
          Unscrambling`Private`canonicalAmplitude[chargedCurrentPair[[2]], chargedCurrentPair[[1]]]]];
    check["charged-current endpoint exactly matches the original notebook " <> chargedCurrentSpecification[[1]],
      Together[Unscrambling`Private`canonicalChains[ExtractMandelstahm[Last[chargedCurrentStates]] -
        ubReference[chargedCurrentSpecification[[3]], "W+"]]] === 0];
    check["charged-current complexity reduction " <> chargedCurrentSpecification[[1]],
      SpinorExpressionComplexity /@ chargedCurrentStates[[{1, -1}]] === chargedCurrentSpecification[[4]]];
    check["charged-current route preserves its off-shell propagator " <> chargedCurrentSpecification[[1]],
      AllTrue[chargedCurrentStates, DeleteDuplicates[Cases[#, _PropDen, Infinity]] === chargedCurrentPole &]];
    chargedCurrentRecordedEdits = {};
    Block[{Unscrambling`Private`$onShellChannels = Automatic,
      Unscrambling`Private`$useMomentumConservation = True},
      Do[
        chargedCurrentLegal = UnscrambleCandidates[{chargedCurrentRoute["Masses"], chargedCurrentStates[[step]]}];
        chargedCurrentMatchedEdit = SelectFirst[chargedCurrentLegal, Function[edit,
            edit["Name"] === chargedCurrentRoute["Identities"][[step]] &&
              Unscrambling`Private`samePoly[Unscrambling`Private`applyCandidate[chargedCurrentStates[[step]], edit],
                chargedCurrentStates[[step + 1]]]], $Failed];
        check["charged-current elementary edit " <> chargedCurrentSpecification[[1]] <> " / " <> ToString[step],
          AssociationQ[chargedCurrentMatchedEdit]];
        If[AssociationQ[chargedCurrentMatchedEdit], AppendTo[chargedCurrentRecordedEdits, chargedCurrentMatchedEdit]],
        {step, Length[chargedCurrentStates] - 1}]];
    chargedCurrentJobs = Unscrambling`Private`makeSimplificationRouteJobs[{chargedCurrentRoute}, All];
    check["charged-current route receives every particle permutation " <> chargedCurrentSpecification[[1]],
      Length[chargedCurrentJobs] === 24 && DuplicateFreeQ[Lookup[chargedCurrentJobs, "LegPermutation"]]];
    Do[
      check["charged-current permuted boundary and target " <> chargedCurrentSpecification[[1]] <> " / " <>
        ToString[Values[chargedCurrentJob["LegPermutation"]], InputForm],
        Unscrambling`Private`samePoly[First[chargedCurrentJob["States"]],
          Unscrambling`Private`relabelLegs[First[chargedCurrentStates], chargedCurrentJob["LegPermutation"]]] &&
        Unscrambling`Private`samePoly[Last[chargedCurrentJob["States"]],
          Unscrambling`Private`relabelLegs[Last[chargedCurrentStates], chargedCurrentJob["LegPermutation"]]] &&
        AllTrue[chargedCurrentJob["States"], DeleteDuplicates[Cases[#, _PropDen, Infinity]] ===
          Unscrambling`Private`relabelLegs[chargedCurrentPole, chargedCurrentJob["LegPermutation"]] &]];
      check["charged-current permuted intermediate applications " <> chargedCurrentSpecification[[1]] <> " / " <>
        ToString[Values[chargedCurrentJob["LegPermutation"]], InputForm],
        curatedPermutationReplayQ[chargedCurrentJob["States"], chargedCurrentRecordedEdits,
          chargedCurrentJob["LegPermutation"]]],
      {chargedCurrentJob, chargedCurrentJobs}]
  ],
  {chargedCurrentSpecification, {
    {"ne, E, W-, h / W+ exchange", {"ne", "E", "W-", "h"}, "ne E -> W- h", {37, 6}},
    {"u, D, W-, h / W+ exchange", {"u", "D", "W-", "h"}, "u D -> W- h", {37, 11}}}}];
(* The new massive-external channels use independently generated diagram
   boundaries and the notebook's existing channel references. *)
newCuratedSpecifications = {
  {"u, D, T, b / W+ exchange", {"u", "D", "T", "b"}, "u D -> T b", "W+", 1, 24},
  {"E, ne, Z, W- / W+ exchange", {"E", "ne", "Z", "W-"}, "E ne -> Z W-", "W+", 1, 109},
  {"U, d, Z, W+ / W+ exchange", {"U", "d", "Z", "W+"}, "U d -> Z W+", "W+", 1, 155},
  {"Z, h, W-, W+ / T-channel W exchange", {"Z", "h", "W-", "W+"}, "Z h -> W- W+", "W+:T", -1, 72},
  {"Z, h, W-, W+ / U-channel W exchange", {"Z", "h", "W-", "W+"}, "Z h -> W- W+", "W+:U", -1, 72}
};
check["new curated channels have no external photon or gluon",
  AllTrue[newCuratedSpecifications, FreeQ[#[[2]], "A.+" | "A.-" | "G.+" | "G.-"] &]];
Do[
  newCuratedName = newCuratedSpecification[[1]];
  newCuratedRoute = SelectFirst[allCuratedRoutes, #["Name"] === newCuratedName &, $Failed];
  newCuratedDiagrams = ubSelect[CreateDiagrams[newCuratedSpecification[[2]], ubModel],
    newCuratedSpecification[[4]], False];
  newCuratedPairs = ReduceSpinContractions[diagramAmplitudes[newCuratedDiagrams]];
  check["new curated route has one independent diagram boundary " <> newCuratedName,
    AssociationQ[newCuratedRoute] && Length[newCuratedPairs] === 1];
  If[AssociationQ[newCuratedRoute] && Length[newCuratedPairs] === 1,
    newCuratedPair = First[newCuratedPairs];
    newCuratedStates = newCuratedRoute["States"];
    newCuratedPoles = DeleteDuplicates[Cases[newCuratedPair[[2]], _PropDen, Infinity]];
    newCuratedReference = ubReference[newCuratedSpecification[[3]], newCuratedSpecification[[4]]];
    check["new curated route matches fresh mass rules " <> newCuratedName,
      Sort[newCuratedRoute["Masses"]] === Sort[newCuratedPair[[1]]]];
    check["new curated route starts at the public spin-contraction boundary " <> newCuratedName,
      Unscrambling`Private`samePoly[First[newCuratedStates],
        Unscrambling`Private`canonicalAmplitude[newCuratedPair[[2]], newCuratedPair[[1]]]]];
    check["new curated endpoint agrees with the independent notebook reference " <> newCuratedName,
      !MissingQ[newCuratedReference] &&
        Together[Unscrambling`Private`canonicalChains[ExtractMandelstahm[Last[newCuratedStates]] -
          newCuratedSpecification[[5]] newCuratedReference]] === 0];
    check["new curated endpoint retains its accepted overall sign " <> newCuratedName,
      Lookup[newCuratedRoute, "TemplateSign", 1] === newCuratedSpecification[[5]]];
    check["new curated route retains every elementary identity " <> newCuratedName,
      Length[newCuratedRoute["Identities"]] === newCuratedSpecification[[6]] &&
        Length[newCuratedStates] === newCuratedSpecification[[6]] + 1];
    check["new curated route improves complexity " <> newCuratedName,
      SpinorExpressionComplexity[Last[newCuratedStates]] < SpinorExpressionComplexity[First[newCuratedStates]]];
    check["new curated route preserves its off-shell propagator " <> newCuratedName,
      AllTrue[newCuratedStates, DeleteDuplicates[Cases[#, _PropDen, Infinity]] === newCuratedPoles &]];
    check["new curated states have no unresolved masses, hats, zetas, or x factors " <> newCuratedName,
      FreeQ[newCuratedStates, _Mass | _SpinorHat | _MomHat | _Zeta | _xFactor | _xTildeFactor]];
    newCuratedJobs = Unscrambling`Private`makeSimplificationRouteJobs[{newCuratedRoute}, All];
    check["new curated route receives all particle-label permutations " <> newCuratedName,
      Length[newCuratedJobs] === 24 && DuplicateFreeQ[Lookup[newCuratedJobs, "LegPermutation"]]]
  ],
  {newCuratedSpecification, newCuratedSpecifications}
];
check["real Z numerator contains composite momentum chains",
  !FreeQ[zCompositeOriginal, c_SpinorChain /; !FreeQ[c, Mom[_Multiparticle]]]];
check["real Z legal candidates expose composite splitting",
  AnyTrue[UnscrambleCandidates[{zCompositeRules, zCompositeOriginal}],
    #["Name"] === "MomentumSum" && Lookup[#, "Identity", None] === "Split" &]];
zCompositeSplit = zCompositeOriginal;
zCompositeSplitSteps = 0;
Do[
  zCompositeEdits = Select[Unscrambling`Private`momentumSumMoves[zCompositeSplit, zCompositeRules],
    Lookup[#, "Identity", None] === "Split" &];
  If[zCompositeEdits === {}, Break[]];
  zCompositeSplit = Unscrambling`Private`applyCandidate[zCompositeSplit, First[zCompositeEdits]];
  zCompositeSplitSteps++, {100}];
check["real Z staged splitting removes composite numerator momenta",
  zCompositeSplitSteps > 0 &&
    FreeQ[zCompositeSplit, c_SpinorChain /; !FreeQ[c, Mom[_Multiparticle]]]];
check["real Z splitting preserves its propagator",
  DeleteDuplicates[Cases[zCompositeSplit, _PropDen, Infinity]] === zCompositePropagators];
check["real Z split identities agree with independent spinor algebra",
  Expand[ReduceSpinorProducts[zCompositeOriginal - zCompositeSplit] /. zCompositeRules] === 0];
zCompositeContracted = zCompositeSplit;
zCompositeMassSteps = 0;
Do[
  zCompositeEdits = Select[Unscrambling`Private`chainMoves[zCompositeContracted, zCompositeRules],
    #["Name"] === "Mass" && #["Direction"] === "Contract" &];
  If[zCompositeEdits === {}, Break[]];
  zCompositeContracted = Unscrambling`Private`applyCandidate[zCompositeContracted, First[zCompositeEdits]];
  zCompositeMassSteps++, {100}];
check["real Z split chains expose endpoint mass contractions", zCompositeMassSteps > 0];
check["real Z mass contractions preserve algebra and propagator",
  Expand[ReduceSpinorProducts[zCompositeOriginal - zCompositeContracted] /. zCompositeRules] === 0 &&
    DeleteDuplicates[Cases[zCompositeContracted, _PropDen, Infinity]] === zCompositePropagators];
test = First[ubCases];
diagrams = CreateDiagrams[test["Particles"], ubModel];
photon = ubSelect[diagrams, "A", False];
check["real diagram selection", Length[photon] > 0 && AllTrue[photon, ubMediator[#] === "A" &]];
pairedPhotonAmplitudes = diagramAmplitudes[photon];
rawPairedPhotonAmplitudes = Block[{ConstructiveDiagrams`expandExternalXFactors},
  Clear[ConstructiveDiagrams`expandExternalXFactors];
  ConstructiveDiagrams`expandExternalXFactors[expr_, n_] := expr;
  diagramAmplitudes[photon]
];
check["paired internal massless-vector shortcut stays unchanged and x free",
  Length[pairedPhotonAmplitudes] === 1 && pairedPhotonAmplitudes === rawPairedPhotonAmplitudes &&
    FreeQ[pairedPhotonAmplitudes, _xFactor | _xTildeFactor]];
check["paired diagrams preserve their mass rules", pairedPhotonAmplitudes[[1, 1]] ===
  ConstructiveDiagrams`diagramMassReplacements[First[photon]]];
unsupportedPairAmplitudes = Block[{ConstructiveDiagrams`masslessSpin1PairAmplitude},
  Clear[ConstructiveDiagrams`masslessSpin1PairAmplitude];
  ConstructiveDiagrams`masslessSpin1PairAmplitude[_] := Missing["UnsupportedMasslessSpin1"];
  diagramAmplitudes[photon]
];
check["unsupported pairing keeps both helicity contributions",
  unsupportedPairAmplitudes ===
    ({ConstructiveDiagrams`diagramMassReplacements[#], diagramAmplitude[#]} & /@ photon)];
fivePointPairDiagrams = TimeConstrained[
  CreateDiagrams[{"e", "E", "M", "m", "h"}, ubModel], 20, $Failed];
check["five-point pairing fixture generates diagrams", ListQ[fivePointPairDiagrams] &&
  fivePointPairDiagrams =!= {}];
If[ListQ[fivePointPairDiagrams],
  fivePointPhotonDiagrams = Select[fivePointPairDiagrams,
    ConstructiveDiagrams`masslessSpin1Edge[#] =!= None &];
  check["five-point fixture contains internal photon helicities", Length[fivePointPhotonDiagrams] > 1];
  fivePointIndividualAmplitudes =
    {ConstructiveDiagrams`diagramMassReplacements[#], diagramAmplitude[#]} & /@ fivePointPhotonDiagrams;
  fivePointPairedAmplitudes = diagramAmplitudes[fivePointPhotonDiagrams];
  check["four-point shortcut rejects higher-point input", AllTrue[fivePointPhotonDiagrams,
    MissingQ[ConstructiveDiagrams`masslessSpin1PairAmplitude[#]] &]];
  check["five-point API preserves all amplitudes and mass rules",
    fivePointPairedAmplitudes === fivePointIndividualAmplitudes];
  check["higher-point retention does not depend on diagram order",
    diagramAmplitudes[Reverse[fivePointPhotonDiagrams]] === Reverse[fivePointIndividualAmplitudes]];
];
photonTest = First[Select[ubCases, #["Name"] === "e, E, M, m" &]];
photonTestDiagrams = CreateDiagrams[photonTest["Particles"], ubModel];
photonTestSelected = ubSelect[photonTestDiagrams, "A", False];
photonReference = ubReference[photonTest["SourceName"], "A"];
photonSingleSum = ReduceSpinContractions[Total[diagramAmplitude /@ photonTestSelected]];
photonPairedSum = ReduceSpinContractions[Total[diagramAmplitudes[photonTestSelected][[All, 2]]]];
check["single photon helicities retain the internal x-factor obstruction",
  !FreeQ[photonSingleSum, _xFactor | _xTildeFactor] &&
    ubCompare[photonSingleSum, photonReference, photonTest["Masses"]]["Equivalence"] === "Unresolved"];
check["paired photon matches the manual reference without the neural model",
  FreeQ[photonPairedSum, _xFactor | _xTildeFactor] &&
    Expand[ExtractMandelstahm[photonPairedSum] - photonReference] === 0];
Block[{ubSolve},
  Clear[ubSolve];
  ubSolve[p_] := <|"Result" -> p, "ModelLoaded" -> True, "ScoringFailed" -> False,
    "TerminationReason" -> "NoOpTest"|>;
  photonSpinByParticle = Association[Flatten[
    ({#[[2]] -> #[[5]], #[[3]] -> #[[5]]} &) /@ ubModel[[1]]]];
  fourFermionPhotonCases = Select[ubCases, MemberQ[#["Channels"], "A"] &&
    AllTrue[#["Particles"], Lookup[photonSpinByParticle, #, -1] === 1 &] &];
  check["all four-fermion photon processes covered", Length[fourFermionPhotonCases] === 10];
  Do[
    allPhotonDiagrams = TimeConstrained[CreateDiagrams[allPhotonTest["Particles"], ubModel], 20, $Failed];
    check["four-fermion photon diagrams exist " <> allPhotonTest["Name"],
      ListQ[allPhotonDiagrams] && allPhotonDiagrams =!= {}];
    If[ListQ[allPhotonDiagrams], Do[
      photonRow = ubResult[allPhotonTest, "A", allPhotonDiagrams, photonPermutation];
      check["photon benchmark agrees " <> allPhotonTest["Name"] <> " numbering " <>
          ToString[photonPermutation, InputForm],
        photonRow["Status"] === "PASS" && FreeQ[photonRow["Prepared"], _xFactor | _xTildeFactor]],
      {photonPermutation, particlePermutations}]],
    {allPhotonTest, fourFermionPhotonCases}];
  singlePhotonRow = ubResult[photonTest, "A", {First[photonTestSelected]}];
  check["unpaired photon helicity is not mistaken for the summed amplitude",
    singlePhotonRow["Status"] === "Unresolved"];
];
check["sum all selection", ubSelect[diagrams, "unused", True] === diagrams];
Block[{ubSolve},
  Clear[ubSolve];
  sentPairs = {};
  ubSolve[p_] := (AppendTo[sentPairs, p]; <|"Result" -> p, "ModelLoaded" -> True, "ScoringFailed" -> False,
    "TerminationReason" -> "Test"|>);
  solved = ubSimplifyDiagrams[photon, test["Masses"]];
  check["raw diagrams retained", AssociationQ[solved] && KeyExistsQ[solved, "Raw"] && KeyExistsQ[solved, "Pieces"]];
  contractSpinIndices = False;
  rawSolved = ubSimplifyDiagrams[photon, test["Masses"]];
  check["raw input option", rawSolved["Raw"] === rawSolved["Prepared"]];
  permutedSolved = ubSimplifyDiagrams[photon, test["Masses"], permutation];
  check["solver input permuted", permutedSolved["Prepared"] === ubRelabel[rawSolved["Prepared"], permutation]];
  contractSpinIndices = True;
  multiDiagrams = ubSelect[CreateDiagrams[ubCases[[2, "Particles"]], ubModel], "A", False];
  sentPairs = {};
  multiSolved = ubSimplifyDiagrams[multiDiagrams, ubCases[[2, "Masses"]]];
  check["channel sums have only external mass rules", Length[sentPairs] > 1 &&
    Last[sentPairs][[1]] === Thread[(Mass /@ Range[4]) -> ubCases[[2, "Masses"]]]];
  ubSolve[p_] := $Failed;
  check["solver failure not accepted", FailureQ[ubSimplifyDiagrams[photon, test["Masses"]]]];
];
Block[{UnscrambleTrace, unscrambleModel},
  Clear[UnscrambleTrace];
  sentModelOptions = {};
  UnscrambleTrace[p_, opts___Rule] := (AppendTo[sentModelOptions, Association[{opts}]];
    <|"Result" -> p, "ModelLoaded" -> True, "ScoringFailed" -> False,
      "TerminationReason" -> "Test"|>);
  Do[
    unscrambleModel = choice;
    sentModelOptions = {};
    selectedSolved = ubSimplifyDiagrams[photon, test["Masses"]];
    check[choice <> " used for single-group diagrams", AssociationQ[selectedSolved] &&
      Length[sentModelOptions] === Length[selectedSolved["Pieces"]] &&
      AllTrue[sentModelOptions, Lookup[#, "Model"] === choice &]];
    check[choice <> " preserves solver limits and model directory", AllTrue[sentModelOptions,
      Lookup[#, {"MaxSteps", "MaxStagnantSteps", "ProgressReporting", "Attempts", "TimeLimit", "ModelDirectory"}] ===
        {maxSteps, maxStagnantSteps, liveProgress, attempts, solverSeconds,
          FileNameJoin[{hepcatDirectory, "tests"}]} &]];
    sentModelOptions = {};
    selectedSolved = ubSimplifyDiagrams[multiDiagrams, ubCases[[2, "Masses"]], permutation];
    check[choice <> " used for every permuted piece and channel sum", AssociationQ[selectedSolved] &&
      Length[sentModelOptions] === Length[selectedSolved["Pieces"]] + 1 &&
      AllTrue[sentModelOptions, Lookup[#, "Model"] === choice &]],
    {choice, {"CandidateScorer", "CompactActions"}}
  ];
];
check["missing diagrams not zero success", ubResult[test, First[test["Channels"]], {}]["Status"] === "MissingDiagrams"];
Block[{ubResult, ubCases = {First[ubCases]}, particlePermutations = Permutations[Range[4]], Print},
  Clear[ubResult];
  ubResult[t_, k_, d_, p_] := <|"Process" -> t["Name"], "Channel" -> k,
    "Permutation" -> p, "Status" -> "PASS"|>;
  ubRun[];
  check["every channel and permutation visited", Length[ubResults] === 72 &&
    Length[DeleteDuplicates[Lookup[ubResults, "Permutation"]]] === 24];
  check["stored statuses remain strings", AllTrue[Lookup[ubResults, "Status"], StringQ]];
];
Print[<|"Checks" -> checks, "Failed" -> Length[failures], "Failures" -> failures|>];
Quit[If[failures === {}, 0, 1]];
