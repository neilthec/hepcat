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
check["one permutation enabled by default", testAllPermutations === False &&
  particlePermutations === {Range[4]}];
check["all permutations enabled by the switch", Block[{testAllPermutations = True},
  particlePermutations === Permutations[Range[4]]]];
check["single permutation restored after switching", particlePermutations === {Range[4]}];
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
check["all permutations invert", AllTrue[Permutations[Range[4]],
  ubRelabel[ubRelabel[probe, #], Ordering[#]] === probe &]];
check["hatted objects relabeled", ubRelabel[
  SpinorHat["Spin", "Up", "Angle", 1, 2] MomProdHat12[2, 4], permutation] ===
  SpinorHat["Spin", "Up", "Angle", 3, 2] MomProdHat12[1, 2]];
check["no legacy simplifier in runner", !StringContainsQ[inputs[[3]], "SimplifySpinorProducts"]];
check["runner uses public bulk diagram amplitudes",
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
  check["massless-vector mass fixture has generic diagrams " <> ToString[{pairedParticles, pairedPermutation}, InputForm],
    Length[pairedMassDiagrams] > 0 && AllTrue[pairedMassDiagrams,
      Length[Lookup[#, "HelicityAssignments", {}]] === 2 &&
        AllTrue[Values[#["Propagators"]], Lookup[#, "Particle"] === "A" &] &]];
  Do[
    pairedMassRules = ConstructiveDiagrams`diagramMassReplacements[pairedMassDiagram];
    unresolvedPairedMass = Block[{ConstructiveDiagrams`diagramMassReplacements},
      Clear[ConstructiveDiagrams`diagramMassReplacements];
      ConstructiveDiagrams`diagramMassReplacements[_] := {};
      diagramAmplitude[pairedMassDiagram]
    ];
    resolvedPairedMass = diagramAmplitudes[{pairedMassDiagram}];
    check["paired vector shortcut introduces a known external mass " <> ToString[{pairedParticles, pairedPermutation, pairedMassRules}, InputForm],
      !FreeQ[unresolvedPairedMass, Mass[_Integer]]];
    check["paired vector shortcut resolves its mass and retains its rules " <> ToString[{pairedParticles, pairedPermutation, pairedMassRules}, InputForm],
      resolvedPairedMass === {{pairedMassRules, unresolvedPairedMass /. pairedMassRules}} &&
        FreeQ[resolvedPairedMass[[All, 2]], _Mass]];
    check["paired vector mass replacement preserves propagators " <> ToString[{pairedParticles, pairedPermutation, pairedMassRules}, InputForm],
      Cases[resolvedPairedMass[[All, 2]], _PropDen, Infinity] ===
        Cases[unresolvedPairedMass, _PropDen, Infinity]],
    {pairedMassDiagram, pairedMassDiagrams}
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
check["twenty-four curated routes remain separate from the reference catalog",
  Length[allCuratedRoutes] === 24 && DuplicateFreeQ[Lookup[allCuratedRoutes, "Name"]] &&
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
quarkZPairs = diagramAmplitudes[ubSelect[
  CreateDiagrams[{"u", "U", "C", "c"}, ubModel], "Z", False]];
quarkZRoute = SelectFirst[allCuratedRoutes, #["Name"] === "u, U, C, c / Z exchange" &];
check["handwritten quark Z path begins at fresh public diagrams",
  Length[quarkZPairs] === 1 && quarkZRoute["Masses"] === First[quarkZPairs][[1]] &&
    Unscrambling`Private`samePoly[First[quarkZRoute["States"]],
      Unscrambling`Private`canonicalAmplitude[
        ReduceSpinContractions[First[quarkZPairs][[2]]], First[quarkZPairs][[1]]]]];
check["handwritten quark Z endpoint agrees without a spinor reducer",
  Unscrambling`Private`samePoly[
    ExtractMandelstahm[Last[quarkZRoute["States"]]] /. MW -> MZ CW,
    ubReference["u U -> C c", "Z"]]];
check["handwritten quark Z path preserves its off-shell denominator",
  AllTrue[quarkZRoute["States"], DeleteDuplicates[Cases[#, _PropDen, Infinity]] ===
    {PropDen[Mom[Multiparticle[1, 2]], MZ]} &]];
check["handwritten quark Z endpoint removes every momentum insertion",
  FreeQ[Cases[Last[quarkZRoute["States"]], _SpinorChain, Infinity], _Mom]];
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
(* Handwritten electron exchange with two external Higgs bosons. *)
eehhCuratedPairs = ReduceSpinContractions[
  diagramAmplitudes[CreateDiagrams[{"e", "E", "h", "h"}, ubModel]]];
eehhElectronPairs = Select[eehhCuratedPairs,
  !FreeQ[#[[2]], PropDen[Mom[Multiparticle[1, 3] | Multiparticle[1, 4]], Me]] &];
eehhElectronRoutes = Select[allCuratedRoutes,
  StringStartsQ[#["Name"], "e, E, h, h / electron"] &];
check["fresh eeHH diagrams contain both curated electron channels",
  Length[eehhElectronPairs] === 2 && Length[eehhElectronRoutes] === 2];
eehhBrackets[text_] := Unscrambling`Private`canonicalChains[
  ConvertStringAmplitude[text, {Me, Me, Mh, Mh}]];
Do[
  eehhDenominator = PropDen[Mom[Multiparticle[1, eehhLeg]], Me];
  eehhPair = SelectFirst[eehhElectronPairs, !FreeQ[#[[2]], eehhDenominator] &, $Failed];
  eehhRoute = SelectFirst[eehhElectronRoutes,
    DeleteDuplicates[Cases[First[#["States"]], _PropDen, Infinity]] === {eehhDenominator} &, $Failed];
  eehhChannel = If[eehhLeg === 3, "e:T", "e:U"];
  check["curated eeHH route matches fresh masses and propagator " <> eehhChannel,
    AssociationQ[eehhRoute] && ListQ[eehhPair] && eehhRoute["Masses"] === eehhPair[[1]]];
  If[AssociationQ[eehhRoute] && ListQ[eehhPair],
    check["curated eeHH path starts at the public spin-contraction boundary " <> eehhChannel,
      Unscrambling`Private`samePoly[First[eehhRoute["States"]],
        Unscrambling`Private`canonicalAmplitude[eehhPair[[2]], eehhPair[[1]]]]];
    check["curated eeHH path records four elementary identities " <> eehhChannel,
      Length[eehhRoute["States"]] === 5 &&
        eehhRoute["Identities"] === {"MomentumSum", "MomentumSum", "Mass", "Mass"}];
    check["curated eeHH path preserves its off-shell denominator " <> eehhChannel,
      AllTrue[eehhRoute["States"], DeleteDuplicates[Cases[#, _PropDen, Infinity]] === {eehhDenominator} &]];
    eehhEndpoint = -EE^2 Me^2/(4 MW^2 SW^2 eehhDenominator)*
      (2 Me eehhBrackets["<12>+[12]"] +
        eehhBrackets[If[eehhLeg === 3, "[132>+[231>", "[142>+[241>"]]);
    check["curated eeHH endpoint matches the known channel without a spinor reducer " <> eehhChannel,
      Unscrambling`Private`samePoly[Last[eehhRoute["States"]],
        Unscrambling`Private`canonicalChains[eehhEndpoint]]];
    check["curated eeHH complexity drops from twenty-two to twelve " <> eehhChannel,
      SpinorExpressionComplexity /@ eehhRoute["States"] === {22, 22, 22, 17, 12}]
  ],
  {eehhLeg, {3, 4}}];
check["both eeHH electron endpoints reproduce the full notebook electron reference",
  Unscrambling`Private`samePoly[
    Unscrambling`Private`canonicalChains[
      ExtractMandelstahm[Total[Last /@ Lookup[eehhElectronRoutes, "States"]]]],
    Unscrambling`Private`canonicalChains[ubReference["e E -> h h", "e"]]]];
eehhCuratedJobs = Unscrambling`Private`makeSimplificationRouteJobs[eehhElectronRoutes, All];
check["both eeHH electron paths receive all particle-label permutations", Length[eehhCuratedJobs] === 48 &&
  Sort[Values[Counts[Lookup[eehhCuratedJobs, "Name"]]]] === {24, 24}];
Do[
  eehhJobRules = eehhJob["Pair"][[1]];
  eehhJobStates = Unscrambling`Private`canonicalAmplitude[#, eehhJobRules] & /@ eehhJob["States"];
  eehhJobIdentities = SelectFirst[eehhElectronRoutes, #["Name"] === eehhJob["Name"] &]["Identities"];
  Block[{Unscrambling`Private`$onShellChannels = Unscrambling`Private`trainingJobChannels[eehhJob],
      Unscrambling`Private`$useMomentumConservation = eehhJob["MomentumConservation"]},
    Do[
      eehhLegalCandidates = Unscrambling`Private`legalMoves[eehhJobStates[[eehhStep]], eehhJobRules];
      check["eeHH permuted transition remains a legal elementary edit " <>
        ToString[{eehhJob["Name"], eehhJob["LegPermutation"], eehhStep}, InputForm],
        AnyTrue[eehhLegalCandidates, Function[candidate,
          candidate["Name"] === eehhJobIdentities[[eehhStep]] &&
            Unscrambling`Private`samePoly[
              Unscrambling`Private`applyCandidate[eehhJobStates[[eehhStep]], candidate],
              eehhJobStates[[eehhStep + 1]]]]]],
      {eehhStep, Length[eehhJobStates] - 1}]],
  {eehhJob, eehhCuratedJobs}];
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
resolvedHelicityDiagrams[diag_] :=
  ConstructiveDiagrams`resolveDiagramHelicities[diag, #] & /@
    Lookup[diag, "HelicityAssignments", {}];
exactGenericHelicitySum[diag_] := Block[
  {ConstructiveDiagrams`genericFermionCurrentAmplitude, ConstructiveDiagrams`genericMasslessSpin1Amplitude},
  Clear[ConstructiveDiagrams`genericFermionCurrentAmplitude, ConstructiveDiagrams`genericMasslessSpin1Amplitude];
  ConstructiveDiagrams`genericFermionCurrentAmplitude[_] := Missing["TestRawHelicitySum"];
  ConstructiveDiagrams`genericMasslessSpin1Amplitude[_, _] := Missing["TestRawHelicitySum"];
  diagramAmplitude[diag]
];
referenceAngle[Spinor[k_, _, index_]] := Spinor[k, "Angle", index];
referenceSquare[Spinor[k_, _, index_]] := Spinor[k, "Square", index];
referenceAngle[Spinor[k_, ul : ("Upper" | "Lower"), _, index_, rest___]] :=
  Spinor[k, ul, "Angle", index, rest];
referenceSquare[Spinor[k_, ul : ("Upper" | "Lower"), _, index_, rest___]] :=
  Spinor[k, ul, "Square", index, rest];
(* Extract the four matter spinors from the original vertex expressions,
   retaining their explicit index polarity and the original propagator factors. *)
independentFermionCurrentReference[diag_] := Module[
  {processed, expressions, edges, used = {}, currents = {}, ends, records,
   spinors, a, b, c, d, aa, ss, current, result},
  processed = ConstructiveDiagrams`makeDiagramSpinIndicesExplicit[First[resolvedHelicityDiagrams[diag]]];
  expressions = Association @ KeyValueMap[#1 -> Last[#2]["Expression"] &,
    processed["VertexStructures"]];
  edges = diag["HelicitySumEdges"];
  Do[
    ends = Keys[diag["InternalEdgeIndices"][edge]];
    If[!FreeQ[used, Alternatives @@ ends], Return[Missing["OverlappingVertices"]]];
    records = Last[processed["VertexStructures"][#]] & /@ ends;
    spinors = (List @@ First[Cases[#["Expression"], _SpinorChain, Infinity]]) & /@ records;
    If[Length /@ spinors =!= {2, 2} || !AllTrue[Flatten[spinors], MatchQ[#, _Spinor] &],
      Return[Missing["NonFermionCurrent"]]];
    {{a, b}, {c, d}} = spinors;
    aa[x_, y_] := SpinorChain[referenceAngle[x], referenceAngle[y]];
    ss[x_, y_] := SpinorChain[referenceSquare[x], referenceSquare[y]];
    current = aa[a, d] ss[b, c] + aa[a, c] ss[b, d] +
      aa[b, d] ss[a, c] + aa[b, c] ss[a, d];
    AppendTo[currents, -(Times @@ (#["ModelRow"][[-2]] & /@ records)) current];
    used = Join[used, ends],
    {edge, edges}];
  result = ConstructiveDiagrams`diagramFermionSign[diag] Times @@ Join[
    Values[KeyDrop[expressions, used]], currents,
    Lookup[Values[processed["Propagators"]], "Factor"]]/I;
  ConstructiveDiagrams`alignInternalMultiparticles[result,
    Values /@ Values[diag["InternalEdgeIndices"]]] /.
    ConstructiveDiagrams`diagramMassReplacements[diag]
];
photonComponents = Join @@ (resolvedHelicityDiagrams /@ photon);
check["photon exchange is one generic diagram with two correlated assignments",
  Length[photon] === 1 && Length[photonComponents] === 2 &&
    Lookup[First[photon], "ExternalParticles"] === test["Particles"] &&
    Lookup[Values[First[photon]["Propagators"]], "Particle"] === {"A"}];
rawPairedPhotonAmplitudes = Block[{ConstructiveDiagrams`expandExternalXFactors},
  Clear[ConstructiveDiagrams`expandExternalXFactors];
  ConstructiveDiagrams`expandExternalXFactors[expr_, n_] := expr;
  diagramAmplitudes[photon]
];
check["generic internal massless-vector shortcut stays unchanged and x free",
  Length[pairedPhotonAmplitudes] === 1 && pairedPhotonAmplitudes === rawPairedPhotonAmplitudes &&
    FreeQ[pairedPhotonAmplitudes, _xFactor | _xTildeFactor]];
check["generic singular and plural APIs give the same helicity sum",
  diagramAmplitude[First[photon]] === pairedPhotonAmplitudes[[1, 2]]];
check["generic diagrams preserve their mass rules", pairedPhotonAmplitudes[[1, 1]] ===
  ConstructiveDiagrams`diagramMassReplacements[First[photon]]];
unsupportedPairAmplitudes = Block[{ConstructiveDiagrams`masslessSpin1PairAmplitude},
  Clear[ConstructiveDiagrams`masslessSpin1PairAmplitude];
  ConstructiveDiagrams`masslessSpin1PairAmplitude[_] := Missing["UnsupportedMasslessSpin1"];
  diagramAmplitudes[photon]
];
check["unsupported shortcut retains both correlated helicity contributions in one entry",
  Length[unsupportedPairAmplitudes] === 1 &&
    unsupportedPairAmplitudes[[1, 1]] === pairedPhotonAmplitudes[[1, 1]] &&
    Unscrambling`Private`samePoly[unsupportedPairAmplitudes[[1, 2]],
      Total[diagramAmplitude /@ photonComponents]]];
fivePointPairDiagrams = TimeConstrained[
  CreateDiagrams[{"e", "E", "M", "m", "h"}, ubModel], 20, $Failed];
check["five-point pairing fixture generates diagrams", ListQ[fivePointPairDiagrams] &&
  fivePointPairDiagrams =!= {}];
If[ListQ[fivePointPairDiagrams],
  fivePointPhotonDiagrams = Select[fivePointPairDiagrams,
    ConstructiveDiagrams`masslessSpin1Edge[#] =!= None &];
  check["five-point fixture contains generic internal photons", Length[fivePointPhotonDiagrams] > 1 &&
    AllTrue[fivePointPhotonDiagrams, Length[Lookup[#, "HelicityAssignments", {}]] === 2 &]];
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
partialPhoton = Join[First[photon], <|"HelicityAssignments" ->
  Take[First[photon]["HelicityAssignments"], 1]|>];
partialPhotonAmplitude = diagramAmplitude[partialPhoton];
check["incomplete generic assignment cannot trigger the complete photon shortcut",
  Unscrambling`Private`samePoly[partialPhotonAmplitude, diagramAmplitude[First[photonComponents]]] &&
    !FreeQ[partialPhotonAmplitude, _xFactor | _xTildeFactor]];
check["legacy resolved list still combines the complete four-point photon exchange",
  diagramAmplitudes[photonComponents] === pairedPhotonAmplitudes];
check["legacy missing-partner list retains its one resolved component",
  diagramAmplitudes[Take[photonComponents, 1]] ===
    {{ConstructiveDiagrams`diagramMassReplacements[First[photonComponents]],
      diagramAmplitude[First[photonComponents]]}}];
massiveIndexEdgeA = UndirectedEdge[7, 8];
massiveIndexEdgeB = UndirectedEdge[8, 9];
massiveIndexFixture = <|
  "Propagators" -> <|
    massiveIndexEdgeA -> <|"Mass" -> Me, "Spin2" -> 1,
      "ParticleVertex" -> 8, "AntiparticleVertex" -> 7|>,
    massiveIndexEdgeB -> <|"Mass" -> MW, "Spin2" -> 2,
      "ParticleVertex" -> 9, "AntiparticleVertex" -> 8|>|>,
  "InternalEdgeIndices" -> <|
    massiveIndexEdgeA -> <|7 -> Multiparticle[3, 4, 5, 6], 8 -> Multiparticle[1, 2]|>,
    massiveIndexEdgeB -> <|8 -> Multiparticle[5, 6], 9 -> Multiparticle[1, 2, 3, 4]|>|>,
  "VertexStructures" -> <|
    7 -> {<|"Expression" -> SpinorChain[Spinor["Spin", "Angle", 1],
      Spinor["Spin", "Angle", Multiparticle[3, 4, 5, 6]]]|>},
    8 -> {<|"Expression" -> SpinorChain[Spinor["Spin", "Angle", 2],
      Spinor["Spin", "Angle", Multiparticle[1, 2]]]*
      SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", Multiparticle[5, 6]]]^2|>},
    9 -> {<|"Expression" -> SpinorChain[Spinor["Spin", "Angle", 4],
      Spinor["Spin", "Angle", Multiparticle[1, 2, 3, 4]]]^2|>}|>|>;
massiveIndicesExplicit = ConstructiveDiagrams`makeDiagramSpinIndicesExplicit[massiveIndexFixture];
massiveIndexLists = Values[massiveIndicesExplicit["SpinIndices"]];
check["distinct internal massive lines use disjoint spin dummy indices",
  Length /@ massiveIndexLists === {1, 2} && DuplicateFreeQ[Flatten[massiveIndexLists]]];
massiveSharedExpression = Last[massiveIndicesExplicit["VertexStructures"][8]]["Expression"];
check["making one antiparticle line explicit does not flip another line's upper index",
  Cases[massiveSharedExpression, Spinor["Spin", kind_, _, Multiparticle[1, 2], _] :> kind,
    Infinity] === {"Upper"} &&
    Cases[massiveSharedExpression, Spinor["Spin", kind_, _, Multiparticle[5, 6], _] :> kind,
      Infinity] === {"Lower", "Lower"}];
sixPointMasslessPartitions = {{Multiparticle[1, 2], Multiparticle[3, 4, 5, 6]},
  {Multiparticle[1, 2, 3, 4], Multiparticle[5, 6]}};
sixPointMasslessProduct =
  SpinorChain[Spinor["Helicity", "Angle", Multiparticle[1, 2]], Spinor["Helicity", "Square", 1]]*
  SpinorChain[Spinor["Helicity", "Angle", Multiparticle[3, 4, 5, 6]], Spinor["Helicity", "Square", 2]]*
  SpinorChain[Spinor["Helicity", "Square", Multiparticle[1, 2, 3, 4]], Spinor["Helicity", "Angle", 3]]*
  SpinorChain[Spinor["Helicity", "Square", Multiparticle[5, 6]], Spinor["Helicity", "Angle", 4]];
sixPointAligned = ConstructiveDiagrams`alignInternalMultiparticles[sixPointMasslessProduct,
  sixPointMasslessPartitions];
sixPointAlignmentExpected = -sixPointMasslessProduct /.
  {Multiparticle[3, 4, 5, 6] -> Multiparticle[1, 2],
    Multiparticle[1, 2, 3, 4] -> Multiparticle[5, 6]};
check["six-point massless alignment follows actual complementary edge partitions",
  sixPointAligned === sixPointAlignmentExpected &&
    DeleteDuplicates[Cases[sixPointAligned, Spinor["Helicity", _, mp_Multiparticle] :> mp,
      Infinity]] === {Multiparticle[1, 2], Multiparticle[5, 6]}];
check["generic photon drawing retains external arrows but has no helicity arrow",
  Count[CreateArrowDiagram[First[photon]], _Arrow, Infinity] === Length[test["Particles"]] &&
    AllTrue[First[photon]["HelicitySumEdges"],
      MissingQ[ConstructiveDiagrams`arrowDirectionData[#, First[photon]["InternalEdgeLabels"]]] &]];
genericAssignmentQ[diag_] := Module[{edges = Lookup[diag, "HelicitySumEdges", {}], assignments},
  assignments = Lookup[diag, "HelicityAssignments", {}];
  edges =!= {} && assignments =!= {} && DuplicateFreeQ[assignments] &&
    AllTrue[assignments, AssociationQ[#] && Sort[Keys[#]] === Sort[edges] &] &&
    AllTrue[edges, Function[edge, With[{prop = diag["Propagators"][edge]},
      MemberQ[{"A", "G"}, prop["Particle"]] && prop["Mass"] === 0 && prop["Spin2"] === 2]]]
];
resolvedAssignmentQ[diag_, assignment_] := Module[{resolved, props, vertexRecords},
  resolved = ConstructiveDiagrams`resolveDiagramHelicities[diag, assignment];
  props = Values[resolved["Propagators"]];
  vertexRecords = Values[resolved["VertexStructures"]];
  !KeyExistsQ[resolved, "HelicityAssignments"] && !KeyExistsQ[resolved, "HelicitySumEdges"] &&
    resolved["ExternalParticles"] === diag["ExternalParticles"] &&
    ConstructiveDiagrams`diagramMassReplacements[resolved] ===
      ConstructiveDiagrams`diagramMassReplacements[diag] &&
    AllTrue[vertexRecords, # =!= {} && AllTrue[#, Function[record,
      AllTrue[Normal[Lookup[record, "InternalHelicities", <||>]],
        Lookup[assignment, First[#], None] === Last[#] &] &&
      Lookup[record["IncidentLegs"], "Particle"] ===
        Take[record["ModelRow"], Length[record["IncidentLegs"]]]]] &]
];
Do[
  multiVectorDiagrams = TimeConstrained[CreateDiagrams[multiVectorFixture[[2]], ubModel], 20, $Failed];
  check["six-fermion fixture generates diagrams " <> multiVectorFixture[[1]], ListQ[multiVectorDiagrams]];
  If[ListQ[multiVectorDiagrams],
    multiVectorSelected = Select[multiVectorDiagrams, Function[diag,
      Length[Lookup[diag, "HelicitySumEdges", {}]] === 2 &&
        AllTrue[Lookup[diag, "HelicitySumEdges", {}],
          diag["Propagators"][#, "Particle"] === multiVectorFixture[[3]] &]]];
    check["six-fermion fixture has six two-vector topologies " <> multiVectorFixture[[1]],
      Length[multiVectorSelected] === 6];
    Do[
      multiVectorLabel = multiVectorFixture[[1]] <> " / " <> ToString[multiVectorIndex];
      multiVectorDiagram = multiVectorSelected[[multiVectorIndex]];
      multiVectorAssignments = multiVectorDiagram["HelicityAssignments"];
      check["two-vector generic metadata " <> multiVectorLabel,
        genericAssignmentQ[multiVectorDiagram] && Length[multiVectorAssignments] === 4];
      check["all two-vector assignments restrict the vertices coherently " <> multiVectorLabel,
        AllTrue[multiVectorAssignments, resolvedAssignmentQ[multiVectorDiagram, #] &]];
      multiVectorComponents = resolvedHelicityDiagrams[multiVectorDiagram];
      multiVectorAmplitude = diagramAmplitude[multiVectorDiagram];
      check["two-vector amplitude is the complete correlated sum " <> multiVectorLabel,
        Unscrambling`Private`samePoly[exactGenericHelicitySum[multiVectorDiagram],
          Total[diagramAmplitude /@ multiVectorComponents]]];
      check["two disjoint fermion currents remove internal x factors " <> multiVectorLabel,
        FreeQ[multiVectorAmplitude, _xFactor | _xTildeFactor]];
      multiVectorReference = independentFermionCurrentReference[multiVectorDiagram];
      check["two-vector sewing agrees with independently extracted matter currents " <> multiVectorLabel,
        FreeQ[multiVectorReference, _Missing] &&
          Expand[ReduceSpinorProducts[multiVectorAmplitude - multiVectorReference]] === 0];
      check["two-vector singular and bulk APIs agree " <> multiVectorLabel,
        diagramAmplitudes[{multiVectorDiagram}] ===
          {{ConstructiveDiagrams`diagramMassReplacements[multiVectorDiagram], multiVectorAmplitude}}];
      check["two-vector output retains every off-shell propagator " <> multiVectorLabel,
        Sort[DeleteDuplicates[Cases[multiVectorAmplitude, _PropDen, Infinity]]] ===
          Sort[DeleteDuplicates[Cases[Lookup[Values[multiVectorDiagram["Propagators"]], "Factor"],
            _PropDen, Infinity]]]],
      {multiVectorIndex, Length[multiVectorSelected]}]
  ],
  {multiVectorFixture, {{"photons", {"e", "E", "m", "M", "l", "L"}, "A"},
    {"gluons", {"u", "U", "d", "D", "c", "C"}, "G"}}}
];
Do[
  fixedHelicityParticles = {"e", "E", "M", "m", fixedHelicity};
  fixedHelicityDiagrams = TimeConstrained[CreateDiagrams[fixedHelicityParticles, ubModel], 20, $Failed];
  check["fixed external helicity fixture generates diagrams " <> fixedHelicity, ListQ[fixedHelicityDiagrams]];
  If[ListQ[fixedHelicityDiagrams],
    fixedHelicityPhotonDiagrams = Select[fixedHelicityDiagrams,
      Lookup[#, "HelicitySumEdges", {}] =!= {} &];
    check["fixed external helicity fixture has four generic photon topologies " <> fixedHelicity,
      Length[fixedHelicityPhotonDiagrams] === 4];
    check["internal grouping leaves the fifth external helicity exact " <> fixedHelicity,
      AllTrue[fixedHelicityDiagrams, #["ExternalParticles"] === fixedHelicityParticles &&
        #["ExternalAssignment"][5, "Particle"] === fixedHelicity &]];
    Do[
      check["fixed external helicity keeps both internal assignments " <>
          ToString[{fixedHelicity, fixedHelicityIndex}, InputForm],
        genericAssignmentQ[fixedHelicityPhotonDiagrams[[fixedHelicityIndex]]] &&
          Length[fixedHelicityPhotonDiagrams[[fixedHelicityIndex]]["HelicityAssignments"]] === 2 &&
          Unscrambling`Private`samePoly[exactGenericHelicitySum[fixedHelicityPhotonDiagrams[[fixedHelicityIndex]]],
            Total[diagramAmplitude /@ resolvedHelicityDiagrams[fixedHelicityPhotonDiagrams[[fixedHelicityIndex]]]]]],
      {fixedHelicityIndex, Length[fixedHelicityPhotonDiagrams]}]
  ],
  {fixedHelicity, {"A.+", "A.-"}}
];
gluonParticles = {"G.+", "G.+", "G.-", "G.-", "G.+"};
gluonDiagrams = TimeConstrained[CreateDiagrams[gluonParticles, ubModel], 20, $Failed];
check["triple-gluon fixture generates generic diagrams", ListQ[gluonDiagrams] && gluonDiagrams =!= {}];
If[ListQ[gluonDiagrams] && gluonDiagrams =!= {},
  check["triple-gluon model forbids some naive independent helicity combinations",
    AnyTrue[gluonDiagrams, Length[#["HelicityAssignments"]] < 2^Length[#["HelicitySumEdges"]] &]];
  check["triple-gluon assignments use only model-allowed coherent vertices",
    AllTrue[gluonDiagrams, genericAssignmentQ[#] &&
      With[{diag = #}, AllTrue[diag["HelicityAssignments"], resolvedAssignmentQ[diag, #] &]] &]];
  check["triple-gluon summation preserves all fixed external helicities",
    AllTrue[gluonDiagrams, #["ExternalParticles"] === gluonParticles &]];
  check["triple-gluon fallback equals its allowed resolved sum",
    AllTrue[Take[gluonDiagrams, UpTo[3]],
      Unscrambling`Private`samePoly[diagramAmplitude[#], Total[diagramAmplitude /@ resolvedHelicityDiagrams[#]]] &]]
];
sixGluonParticles = {"G.+", "G.-", "G.+", "G.-", "G.+", "G.-"};
sixGluonDiagrams = TimeConstrained[CreateDiagrams[sixGluonParticles, ubModel], 20, $Failed];
check["actual six-point massless alignment fixture generates diagrams",
  ListQ[sixGluonDiagrams] && sixGluonDiagrams =!= {}];
If[ListQ[sixGluonDiagrams] && sixGluonDiagrams =!= {},
  sixGluonDiagram = First[sixGluonDiagrams];
  sixGluonComponent = First[resolvedHelicityDiagrams[sixGluonDiagram]];
  sixGluonAmplitude = diagramAmplitude[sixGluonComponent];
  check["actual six-point component contains internal helicity spinors",
    !FreeQ[sixGluonAmplitude, Spinor["Helicity", _, _Multiparticle]]];
  check["actual six-point component aligns all three internal massless partitions",
    Length[sixGluonDiagram["HelicitySumEdges"]] === 3 &&
      AllTrue[Values[sixGluonDiagram["InternalEdgeIndices"]], Function[ends,
        With[{mps = Values[ends]}, With[{keep =
            ConstructiveDiagrams`preferredInternalMultiparticle @@ mps},
          FreeQ[sixGluonAmplitude, Spinor["Helicity", _, First[DeleteCases[mps, keep]]]]]]]]];
  check["actual six-point alignment preserves the three propagator denominators",
    Sort[DeleteDuplicates[Cases[sixGluonAmplitude, _PropDen, Infinity]]] ===
      Sort[DeleteDuplicates[Cases[Lookup[Values[sixGluonComponent["Propagators"]], "Factor"],
        _PropDen, Infinity]]]];
  check["generic gluon drawing has no internal helicity arrows",
    Count[CreateArrowDiagram[sixGluonDiagram], _Arrow, Infinity] === 6 &&
      AllTrue[sixGluonDiagram["HelicitySumEdges"],
        MissingQ[ConstructiveDiagrams`arrowDirectionData[#, sixGluonDiagram["InternalEdgeLabels"]]] &]]
];
cubicScalarModel = {{{"scalar", "s", "s", 0, 0, ms, 0, 1, "", "s", "s"}},
  {{"s", "s", "s", gs, 1}}, {}, {}};
Do[
  scalarDiagrams = TimeConstrained[CreateDiagrams[ConstantArray["s", scalarCount[[1]]],
    cubicScalarModel], 20, $Failed];
  check["complete cubic scalar topology count " <> ToString[scalarCount[[1]]],
    ListQ[scalarDiagrams] && Length[scalarDiagrams] === scalarCount[[2]]];
  If[scalarCount[[1]] === 6 && ListQ[scalarDiagrams],
    check["six-point cubic scalar diagrams include the three-internal-edge star",
      AnyTrue[scalarDiagrams, Count[VertexDegree[
        Graph[#["InternalVertices"], Keys[#["Propagators"]]]], 3] === 1 &]]],
  {scalarCount, {{3, 1}, {4, 3}, {5, 15}, {6, 105}}}
];
photonTest = First[Select[ubCases, #["Name"] === "e, E, M, m" &]];
photonTestDiagrams = CreateDiagrams[photonTest["Particles"], ubModel];
photonTestSelected = ubSelect[photonTestDiagrams, "A", False];
photonReference = ubReference[photonTest["SourceName"], "A"];
photonTestComponents = Join @@ (resolvedHelicityDiagrams /@ photonTestSelected);
photonSingleSum = ReduceSpinContractions[Total[diagramAmplitude /@ photonTestComponents]];
photonPairedSum = ReduceSpinContractions[Total[diagramAmplitudes[photonTestSelected][[All, 2]]]];
check["resolved photon components retain the internal x-factor obstruction",
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
      {photonPermutation, Permutations[Range[4]]}]],
    {allPhotonTest, fourFermionPhotonCases}];
  singlePhotonRow = ubResult[photonTest, "A", {First[photonTestComponents]}];
  check["unpaired photon helicity is not mistaken for the summed amplitude",
    singlePhotonRow["Status"] === "Unresolved"];
  genericPhotonRow = ubResult[photonTest, "A", {First[photonTestSelected]}];
  check["one generic photon diagram includes its complete helicity sum",
    genericPhotonRow["Status"] === "PASS"];
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
Block[{ubResult, ubCases = {First[ubCases]}, testAllPermutations = False, Print},
  Clear[ubResult];
  ubResult[t_, k_, d_, p_] := <|"Process" -> t["Name"], "Channel" -> k,
    "Permutation" -> p, "Status" -> "PASS"|>;
  ubRun[];
  check["every channel visited once by default", Length[ubResults] === 3 &&
    DeleteDuplicates[Lookup[ubResults, "Permutation"]] === {Range[4]}];
  testAllPermutations = True;
  ubRun[];
  check["every channel and permutation visited", Length[ubResults] === 72 &&
    Length[DeleteDuplicates[Lookup[ubResults, "Permutation"]]] === 24];
  testAllPermutations = False;
  ubRun[];
  check["runner returns to single permutation after switching", Length[ubResults] === 3 &&
    DeleteDuplicates[Lookup[ubResults, "Permutation"]] === {Range[4]}];
  check["stored statuses remain strings", AllTrue[Lookup[ubResults, "Status"], StringQ]];
];
obsoleteDiagramTemporaryPatterns = {"ConstructiveDiagrams`state", "ConstructiveDiagrams`state$*",
  "ConstructiveDiagrams`leg", "ConstructiveDiagrams`leg$*",
  "ConstructiveDiagrams`choices", "ConstructiveDiagrams`choices$*",
  "ConstructiveDiagrams`prop", "ConstructiveDiagrams`prop$*"};
Block[{Global`state = "notebook state", Global`leg = "notebook leg",
    Global`leg$ = "notebook scoped leg", Global`choices = "notebook choices",
    Global`prop = "notebook propagator"},
  Do[
    (* The second reload simulates an older package's leaked symbols. Only
       this deliberate collision fixture suppresses messages, not the reload. *)
    If[reloadIndex === 2,
      Quiet[Symbol /@ {"ConstructiveDiagrams`state", "ConstructiveDiagrams`state$123",
        "ConstructiveDiagrams`leg", "ConstructiveDiagrams`leg$",
        "ConstructiveDiagrams`choices", "ConstructiveDiagrams`choices$123",
        "ConstructiveDiagrams`prop", "ConstructiveDiagrams`prop$123"}]];
    Block[{$MessageList = {}},
      Get[FileNameJoin[{root, "source", "HEPCAT.wl"}]];
      check["package reload has no notebook-name collisions " <> ToString[reloadIndex],
        $MessageList === {}]];
    check["package reload preserves notebook variables " <> ToString[reloadIndex],
      {Global`state, Global`leg, Global`leg$, Global`choices, Global`prop} ===
        {"notebook state", "notebook leg", "notebook scoped leg", "notebook choices", "notebook propagator"}];
    check["package reload removes obsolete public diagram temporaries " <> ToString[reloadIndex],
      Flatten[Names /@ obsoleteDiagramTemporaryPatterns] === {}];
    reloadDiagrams = CreateDiagrams[{"e", "E", "M", "m"}, ubModel];
    check["diagram helpers still work while notebook variables have values " <> ToString[reloadIndex],
      Length[reloadDiagrams] === 3 && Length[diagramAmplitudes[reloadDiagrams]] === 3 &&
        FreeQ[diagramAmplitudes[reloadDiagrams], _Missing | _xFactor | _xTildeFactor]],
    {reloadIndex, 2}]
];
(* Equal photon helicities have two channel-local paths and an external-shell-
   only sum. Reference choices belong to the preparation boundary. *)
electronPhotonRoutes = trainingElectronPhotonPairRoute[];
check["equal-helicity photon example has three curated paths",
  ListQ[electronPhotonRoutes] && Length[electronPhotonRoutes] === 3 &&
    Unscrambling`Private`validSimplificationRoutesQ[electronPhotonRoutes]];
If[ListQ[electronPhotonRoutes] && Length[electronPhotonRoutes] === 3,
  electronPhotonRoute = Last[electronPhotonRoutes];
  electronPhotonStates = electronPhotonRoute["States"];
  electronPhotonDiagrams = CreateDiagrams[{"e", "E", "A.+", "A.+"}, ubModel];
  electronPhotonPairs = diagramAmplitudes[electronPhotonDiagrams];
  electronPhotonPoles = {
    PropDen[Mom[Multiparticle[1, 3]], Me],
    PropDen[Mom[Multiparticle[1, 4]], Me]};
  electronPhotonGauge[pair_] := Module[{otherLegs},
    otherLegs = Which[
      !FreeQ[pair[[2]], electronPhotonPoles[[1]]], {4, 3},
      !FreeQ[pair[[2]], electronPhotonPoles[[2]]], {3, 4},
      True, Return[$Failed]];
    Unscrambling`Private`canonicalAmplitude[
      ReduceSpinContractions[pair[[2]]] /. {
        Spinor["Xi", "Angle", 10] -> Spinor["Helicity", "Angle", otherLegs[[1]]],
        Spinor["Xi", "Angle", 11] -> Spinor["Helicity", "Angle", otherLegs[[2]]]}, pair[[1]]]
  ];
  electronPhotonPrepared = electronPhotonGauge /@ electronPhotonPairs;
  electronPhotonTarget = 2 EE^2 Me*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity", "Square", 4]]^2/
    (Times @@ electronPhotonPoles);
  electronPhotonTargets = Join[
    Table[2 EE^2 Me*
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
      SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity", "Square", 4]]/
      (SpinorChain[Spinor["Helicity", "Angle", 3], Spinor["Helicity", "Angle", 4]]*
        electronPhotonPoles[[index]]), {index, 2}], {electronPhotonTarget}];
  check["equal-helicity photon sum contains both electron exchange channels",
    Length[electronPhotonPairs] === 2 && FreeQ[electronPhotonPrepared, $Failed] &&
      Sort[DeleteDuplicates[Cases[electronPhotonPairs[[All, 2]], _PropDen, Infinity]]] ===
        Sort[electronPhotonPoles]];
  Do[
    electronPhotonPair = SelectFirst[electronPhotonPairs,
      !FreeQ[#[[2]], electronPhotonPoles[[index]]] &];
    check["equal-helicity photon channel starts at its own fresh gauge-fixed diagram " <> ToString[index],
      Unscrambling`Private`samePoly[First[electronPhotonRoutes[[index]]["States"]],
        electronPhotonGauge[electronPhotonPair]]];
    check["equal-helicity photon channel agrees with its separately reduced reference " <> ToString[index],
      Unscrambling`Private`samePoly[Last[electronPhotonRoutes[[index]]["States"]],
        electronPhotonTargets[[index]]]];
    check["equal-helicity photon channel keeps its own numerator shell conditions " <> ToString[index],
      electronPhotonRoutes[[index]]["OnShellChannels"] ===
        If[index === 1, {{1, 3}, {2, 4}}, {{1, 4}, {2, 3}}]];
    check["equal-helicity photon channel preserves only its own off-shell propagator " <> ToString[index],
      AllTrue[electronPhotonRoutes[[index]]["States"],
        DeleteDuplicates[Cases[#, _PropDen, Infinity]] === {electronPhotonPoles[[index]]} &]],
    {index, 2}];
  check["equal-helicity photon sum starts after the two channel-local reductions",
    Unscrambling`Private`samePoly[First[electronPhotonStates],
      Total[Last /@ Lookup[Take[electronPhotonRoutes, 2], "States"]]]];
  check["equal-helicity photon endpoint equals the notebook reference without a spinor reducer",
    Unscrambling`Private`samePoly[Last[electronPhotonStates], electronPhotonTarget]];
  check["equal-helicity photon sum has no internal shell assumptions",
    electronPhotonRoute["OnShellChannels"] === {} &&
      FreeQ[electronPhotonRoute["Masses"], Mass[_Multiparticle]] &&
      FreeQ[electronPhotonRoute["Identities"], "OnShell"]];
  check["equal-helicity photon states retain only the original off-shell propagators",
    AllTrue[electronPhotonStates, Function[expr,
      AllTrue[Cases[expr, _PropDen, Infinity], MemberQ[electronPhotonPoles, #] &]]] &&
      Sort[DeleteDuplicates[Cases[Last[electronPhotonStates], _PropDen, Infinity]]] ===
        Sort[electronPhotonPoles]];
  check["equal-helicity photon training states have no reference spinors or x factors",
    FreeQ[Lookup[electronPhotonRoutes, "States"], Spinor["Xi", ___] | _xFactor | _xTildeFactor | _Mass]];
  check["equal-helicity photon route records every elementary identity",
    AllTrue[electronPhotonRoutes, Length[#["Identities"]] === Length[#["States"]] - 1 &&
      FreeQ[#["Identities"], "ReduceSpinorProducts" | "SimplifySpinorProducts"] &]];
  electronPhotonJobs = Unscrambling`Private`makeSimplificationRouteJobs[electronPhotonRoutes, All];
  check["equal-helicity photon route receives all particle-label permutations",
    Length[electronPhotonJobs] === 72 &&
      AllTrue[GatherBy[electronPhotonJobs, #["Name"] &],
        Length[#] === 24 && DuplicateFreeQ[Lookup[#, "LegPermutation"]] &]];
  Do[
    Block[{Unscrambling`Private`$onShellChannels =
          Unscrambling`Private`trainingJobChannels[electronPhotonJob],
        Unscrambling`Private`$useMomentumConservation =
          Lookup[electronPhotonJob, "MomentumConservation", True]},
      electronPhotonVerified = Unscrambling`Private`verifiedSimplificationRoute[electronPhotonJob];
      check["equal-helicity photon permuted path consists entirely of legal edits " <>
          electronPhotonJob["Name"] <> " " <> ToString[Values[electronPhotonJob["LegPermutation"]], InputForm],
        AssociationQ[electronPhotonVerified] &&
          Length[electronPhotonVerified["Moves"]] === electronPhotonJob["Steps"]];
      check["equal-helicity photon permuted endpoint matches the reference " <>
          electronPhotonJob["Name"] <> " " <> ToString[Values[electronPhotonJob["LegPermutation"]], InputForm],
        Unscrambling`Private`samePoly[Last[electronPhotonJob["States"]],
          Unscrambling`Private`relabelLegs[electronPhotonTargets[[-electronPhotonJob["AmplitudeIndex"]]],
            electronPhotonJob["LegPermutation"]]]]
    ],
    {electronPhotonJob, electronPhotonJobs}]
];
(* Opposite photon helicities have two channel-local paths and one external-
   shell-only sum path. No reducer or neural inference supplies their edits. *)
oppositePhotonRoutes = trainingElectronOppositePhotonRoutes[];
check["opposite-helicity photon example has three curated paths",
  ListQ[oppositePhotonRoutes] && Length[oppositePhotonRoutes] === 3 &&
    Unscrambling`Private`validSimplificationRoutesQ[oppositePhotonRoutes]];
If[ListQ[oppositePhotonRoutes] && Length[oppositePhotonRoutes] === 3,
  oppositePhotonPoles = {PropDen[Mom[Multiparticle[1, 3]], Me], PropDen[Mom[Multiparticle[1, 4]], Me]};
  oppositePhotonPairs = diagramAmplitudes[CreateDiagrams[{"e", "E", "A.+", "A.-"}, ubModel]];
  oppositePhotonPrepared = Table[
    pair = SelectFirst[oppositePhotonPairs, !FreeQ[#[[2]], pole] &];
    Unscrambling`Private`canonicalAmplitude[ReduceSpinContractions[pair[[2]]] /.
      oppositePhotonRoutes[[1]]["ReferenceSpinorPreparation"], pair[[1]]],
    {pole, oppositePhotonPoles}];
  oppositePhotonTarget = Unscrambling`Private`canonicalChains[-2 EE^2*
    ConvertStringAmplitude["[13]<24>+[23]<14>", {Me, Me, 0, 0}]*
    ConvertStringAmplitude["[314>", {Me, Me, 0, 0}]/(Times @@ oppositePhotonPoles)];
  check["opposite-helicity photon public diagrams contain exactly the two electron channels",
    Length[oppositePhotonPairs] === 2 &&
      Sort[DeleteDuplicates[Cases[oppositePhotonPairs[[All, 2]], _PropDen, Infinity]]] === Sort[oppositePhotonPoles]];
  Do[
    check["opposite-helicity photon route starts at fresh gauge-fixed diagram " <> ToString[index],
      Unscrambling`Private`samePoly[First[oppositePhotonRoutes[[index]]["States"]], oppositePhotonPrepared[[index]]]];
    check["opposite-helicity photon channel keeps its own numerator shell conditions " <> ToString[index],
      oppositePhotonRoutes[[index]]["OnShellChannels"] ===
        If[index === 1, {{1, 3}, {2, 4}}, {{1, 4}, {2, 3}}]];
    check["opposite-helicity photon channel preserves its off-shell propagator " <> ToString[index],
      AllTrue[oppositePhotonRoutes[[index]]["States"],
        DeleteDuplicates[Cases[#, _PropDen, Infinity]] === {oppositePhotonPoles[[index]]} &]],
    {index, 2}];
  check["opposite-helicity photon sum starts at both channel endpoints",
    Unscrambling`Private`samePoly[First[oppositePhotonRoutes[[3]]["States"]],
      Total[Last /@ Lookup[Take[oppositePhotonRoutes, 2], "States"]]]];
  check["opposite-helicity photon sum has no internal shell assumptions",
    oppositePhotonRoutes[[3]]["OnShellChannels"] === {} &&
      FreeQ[oppositePhotonRoutes[[3]]["Masses"], _Multiparticle]];
  check["opposite-helicity photon sum preserves both off-shell propagators",
    AllTrue[oppositePhotonRoutes[[3]]["States"],
      Sort[DeleteDuplicates[Cases[#, _PropDen, Infinity]]] === Sort[oppositePhotonPoles] &]];
  check["opposite-helicity photon endpoint equals the handwritten expression",
    Unscrambling`Private`samePoly[Last[oppositePhotonRoutes[[3]]["States"]], oppositePhotonTarget]];
  check["opposite-helicity photon endpoint equals the independent reference catalog",
    Unscrambling`Private`samePoly[ExtractMandelstahm[Last[oppositePhotonRoutes[[3]]["States"]]],
      Unscrambling`Private`canonicalChains[ubReference["e E -> A.+ A.-", "e"]]]];
  check["opposite-helicity photon states have no reference spinors or x factors",
    FreeQ[Lookup[oppositePhotonRoutes, "States"], Spinor["Xi", ___] | _xFactor | _xTildeFactor | _Mass]];
  check["opposite-helicity photon paths record all 51 elementary identities",
    Length /@ Lookup[oppositePhotonRoutes, "Identities"] === {19, 18, 14} &&
      MapThread[Length[#1] === Length[#2] - 1 &,
        {Lookup[oppositePhotonRoutes, "Identities"], Lookup[oppositePhotonRoutes, "States"]}] ===
        {True, True, True} &&
      FreeQ[Lookup[oppositePhotonRoutes, "Identities"], "ReduceSpinorProducts" | "SimplifySpinorProducts"]];
  oppositePhotonJobs = Unscrambling`Private`makeSimplificationRouteJobs[oppositePhotonRoutes, All];
  check["opposite-helicity photon paths receive all 72 particle-label permutations",
    Length[oppositePhotonJobs] === 72 && AllTrue[GatherBy[oppositePhotonJobs, #["Name"] &],
      Length[#] === 24 && DuplicateFreeQ[Lookup[#, "LegPermutation"]] &]];
  Do[
    Block[{Unscrambling`Private`$onShellChannels = Unscrambling`Private`trainingJobChannels[job],
      Unscrambling`Private`$useMomentumConservation = Lookup[job, "MomentumConservation", True]},
      oppositePhotonVerified = Unscrambling`Private`verifiedSimplificationRoute[job];
      check["opposite-helicity photon permuted path consists entirely of legal edits " <>
        job["Name"] <> " " <> ToString[Values[job["LegPermutation"]], InputForm],
        AssociationQ[oppositePhotonVerified] && Length[oppositePhotonVerified["Moves"]] === job["Steps"]];
      If[job["Name"] === oppositePhotonRoutes[[3]]["Name"],
        check["opposite-helicity photon permuted sum matches the reference " <>
          ToString[Values[job["LegPermutation"]], InputForm],
          Unscrambling`Private`samePoly[Last[job["States"]],
            Unscrambling`Private`relabelLegs[oppositePhotonTarget, job["LegPermutation"]]]]]
    ], {job, oppositePhotonJobs}]
];
Print[<|"Checks" -> checks, "Failed" -> Length[failures], "Failures" -> failures|>];
Quit[If[failures === {}, 0, 1]];
