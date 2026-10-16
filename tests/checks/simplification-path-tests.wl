(* Curated elementary-identity paths, fresh diagram boundaries, and permutations. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
ubModel = ReadModel[FileNameJoin[{root, "models"}], 1];
ubCatalog = Get[FileNameJoin[{root, "tests", "simplification-paths.wl"}]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]]);
ubMediator[d_] := Module[{props = Values[d["Propagators"]], particle},
  If[props === {}, Return["contact"]];
  particle = First[props]["Particle"];
  Which[MemberQ[{"A", "A.+", "A.-"}, particle], "A",
    MemberQ[{"G", "G.+", "G.-"}, particle], "G", True, particle]
];
ubSelect[diagrams_, key_, sumAll_] := Module[{parts = StringSplit[key, ":"]},
  If[TrueQ[sumAll], Return[diagrams]];
  Select[diagrams, ubMediator[#] === First[parts] &&
    (Length[parts] === 1 || diagramChannel[#] === Last[parts]) &]
];
ubReference[name_, key_] := Module[{source, matches},
  source = "SM-4-point-test.nb / " <> name <> " / " <> key;
  matches = Select[ubCatalog, MemberQ[#["Sources"], source] &];
  If[Length[matches] === 1, First[matches]["Amplitude"], Missing["Reference", source]]
];
verificationSeconds = 1;
ubCompare[got_, expected_, masses_] := Module[{check, assumptions, difference, sum},
  assumptions = MW == MZ CW &&
    Mandelstahm[1, 2] + Mandelstahm[1, 3] + Mandelstahm[1, 4] == Total[masses^2];
  check[expr_] := TimeConstrained[
    FullSimplify[ExtractMandelstahm[ReduceSpinorProducts[expr]], Assumptions -> assumptions],
    verificationSeconds, $TimedOut];
  If[got === expected, Return[<|"Equivalence" -> "Exact", "Residual" -> 0|>]];
  difference = check[got - expected];
  If[difference === 0, Return[<|"Equivalence" -> "Proved", "Residual" -> 0|>]];
  sum = check[got + expected];
  <|"Equivalence" -> Which[sum === 0, "OppositeSign",
    difference === $TimedOut || sum === $TimedOut, "VerificationTimeout", True, "Unresolved"],
    "Residual" -> difference|>
];
permutation = {3, 1, 4, 2};

(* Enumerate original legal paths once, then replay their exact edits under
   particle relabeling instead of regenerating every permuted candidate list. *)
routeDataQ[route_] := AssociationQ[route] && StringQ[Lookup[route, "Name", None]] &&
  ListQ[Lookup[route, "Masses", None]] && ListQ[Lookup[route, "States", None]] &&
  Length[route["States"]] >= 2 &&
  Length[Lookup[route, "Identities", {}]] === Length[route["States"]] - 1;
validRoutesQ[routes_] := ListQ[routes] && AllTrue[routes, routeDataQ];
permutedRoutes[routes_, All] := Flatten[MapIndexed[
  Function[{route, position}, Map[Function[permutation, Module[{ids, masses, states, channels},
    ids = AssociationThread[Range[4], permutation];
    masses = ConstructiveDiagrams`Private`relabelLegs[route["Masses"], ids];
    states = ConstructiveDiagrams`Private`relabelLegs[route["States"], ids];
    channels = Lookup[route, "OnShellChannels", Automatic];
    If[ListQ[channels], channels = Sort /@ ((Lookup[ids, #, #] &) /@ # & /@ channels)];
    Join[route, <|"LegPermutation" -> ids, "States" -> states, "Masses" -> masses,
      "Pair" -> {masses, First[states]}, "OnShellChannels" -> channels,
      "AmplitudeIndex" -> -First[position], "Steps" -> Length[states] - 1|>]
  ]], Permutations[Range[4]]]], routes], 1];
routeChannels[route_] := Lookup[route, "OnShellChannels", Automatic];
routeEdits[state_, masses_, family_] := Switch[family,
  "MomentumSum", ConstructiveDiagrams`Private`momentumSumMoves[state, masses],
  "Mass" | "MomentumConservation", Join[
    ConstructiveDiagrams`Private`chainMoves[state, masses],
    ConstructiveDiagrams`Private`scalarMomentumConservationMoves[state, masses]],
  "Schouten", SchoutenCandidates[state],
  "Anticommutation" | "ChainSquare", Join[
    ConstructiveDiagrams`Private`interiorChainMoves[state, masses],
    ConstructiveDiagrams`Private`traceMoves[state, masses]],
  "OnShell", ConstructiveDiagrams`Private`kinematicMoves[state, masses],
  "MomentumSquare", Join[
    ConstructiveDiagrams`Private`kinematicMoves[state, masses],
    ConstructiveDiagrams`Private`helicityInvariantMoves[state, masses],
    ConstructiveDiagrams`Private`propagatorNumeratorMoves[state, masses],
    ConstructiveDiagrams`Private`traceMoves[state, masses]],
  _, ConstructiveDiagrams`Private`legalMoves[state, masses]];
verifiedRoute[route_] := cachedVerifiedRoute[KeyTake[route,
  {"Masses", "States", "Identities", "OnShellChannels", "MomentumConservation"}]];
cachedVerifiedRoute[route_] := cachedVerifiedRoute[route] = Module[
  {masses = route["Masses"], states, edits = {}, choices, chosen,
    transitionDelta, deltaTerms, preferred, matchesDelta},
  states = ConstructiveDiagrams`Private`canonicalAmplitude[#, masses] & /@ route["States"];
  Block[{ConstructiveDiagrams`Private`$onShellChannels = routeChannels[route],
    ConstructiveDiagrams`Private`$useMomentumConservation = Lookup[route, "MomentumConservation", True]},
    Do[
      choices = routeEdits[states[[step]], masses, route["Identities"][[step]]];
      (* Apply an edit by its local polynomial delta, not a full-state rewrite
         for every rejected candidate. The equality is exactly the same. *)
      transitionDelta = ConstructiveDiagrams`Private`canonicalChains[
        states[[step]] - states[[step + 1]]];
      deltaTerms = ConstructiveDiagrams`Private`monomialsOf[transitionDelta];
      matchesDelta[candidate_] := candidate["Name"] === route["Identities"][[step]] &&
          ConstructiveDiagrams`Private`samePoly[
            ConstructiveDiagrams`Private`canonicalChains[candidate["Subtract"] - candidate["Insert"]],
            transitionDelta];
      preferred = Select[choices, MemberQ[deltaTerms, #["Subtract"]] &];
      chosen = SelectFirst[preferred, matchesDelta, $Failed];
      (* A cancelled source monomial may be absent from the delta; never
         exclude that case from the complete legal candidate search. *)
      If[chosen === $Failed, chosen = SelectFirst[choices, matchesDelta, $Failed]];
      If[chosen === $Failed, Return[$Failed, Module]];
      AppendTo[edits, chosen],
      {step, Length[states] - 1}]];
  <|"Moves" -> edits|>
];
selectedPermutations[paths_] := Flatten[Map[
  #[[{1, 12, 24}]] &, GatherBy[paths, #["Name"] &]], 1];
replayPermutedRoute[path_, original_] := Module[{verified, states, edits, ids},
  verified = verifiedRoute[original];
  If[!AssociationQ[verified], Return[False]];
  ids = path["LegPermutation"];
  states = path["States"];
  edits = (Join[#, <|
    "Subtract" -> ConstructiveDiagrams`Private`canonicalChains[
      ConstructiveDiagrams`Private`relabelLegs[#["Subtract"], ids]],
    "Insert" -> ConstructiveDiagrams`Private`canonicalChains[
      ConstructiveDiagrams`Private`relabelLegs[#["Insert"], ids]]|>] &) /@ verified["Moves"];
  And @@ MapThread[ConstructiveDiagrams`Private`samePoly[
    ConstructiveDiagrams`Private`canonicalChains[#1 - #2],
    ConstructiveDiagrams`Private`canonicalChains[#3["Subtract"] - #3["Insert"]]] &,
    {Most[states], Rest[states], edits}]
];
(* The notebook's Z example needs explicit composite-momentum edits before
   endpoint mass identities can act. This tests the legal path, not a model. *)
zCompositeDiagrams = ubSelect[CreateDiagrams[{"e", "E", "M", "m"}, ubModel], "Z", False];
zCompositePairs = diagramAmplitudes[zCompositeDiagrams];
zCompositeRules = First[zCompositePairs][[1]];
zCompositeOriginal = ConstructiveDiagrams`Private`canonicalAmplitude[
  ReduceSpinContractions[Total[zCompositePairs[[All, 2]]]], zCompositeRules];
zCompositePropagators = DeleteDuplicates[Cases[zCompositeOriginal, _PropDen, Infinity]];
Print["Constructing the curated identity paths."];
allCuratedRoutes = simplificationRoutes;
check["reference catalog retains 195 physical SM examples", Length[ubCatalog] === 195 &&
  !AnyTrue[Lookup[ubCatalog, "Name"], StringStartsQ[#, "Synthetic5Point"] &]];
check["curated paths do not load a neural package", Names["Unscrambling`*"] === {}];
zCuratedRoute = First[allCuratedRoutes];
check["twenty-four curated identity paths remain separate from the reference catalog",
  Length[allCuratedRoutes] === 24 && DuplicateFreeQ[Lookup[allCuratedRoutes, "Name"]] &&
    validRoutesQ[allCuratedRoutes]];
check["curated Z path begins at the public diagram and spin-contraction boundary",
  zCuratedRoute["Masses"] === zCompositeRules &&
    ConstructiveDiagrams`Private`samePoly[First[zCuratedRoute["States"]], zCompositeOriginal]];
check["curated Z path preserves every propagator in every state", AllTrue[zCuratedRoute["States"],
  DeleteDuplicates[Cases[#, _PropDen, Infinity]] === zCompositePropagators &]];
check["curated Z endpoint needs no further momentum-chain reductions",
  FreeQ[Cases[Last[zCuratedRoute["States"]], _SpinorChain, Infinity], _Mom]];
check["curated Z endpoint agrees with the reference without a spinor reducer",
  ConstructiveDiagrams`Private`samePoly[
    ExtractMandelstahm[Last[zCuratedRoute["States"]]] /. MW -> MZ CW,
    ubReference["e E -> M m", "Z"]]];
quarkZPairs = diagramAmplitudes[ubSelect[
  CreateDiagrams[{"u", "U", "C", "c"}, ubModel], "Z", False]];
quarkZRoute = SelectFirst[allCuratedRoutes, #["Name"] === "u, U, C, c / Z exchange" &];
check["handwritten quark Z path begins at fresh public diagrams",
  Length[quarkZPairs] === 1 && quarkZRoute["Masses"] === First[quarkZPairs][[1]] &&
    ConstructiveDiagrams`Private`samePoly[First[quarkZRoute["States"]],
      ConstructiveDiagrams`Private`canonicalAmplitude[
        ReduceSpinContractions[First[quarkZPairs][[2]]], First[quarkZPairs][[1]]]]];
check["handwritten quark Z endpoint agrees without a spinor reducer",
  ConstructiveDiagrams`Private`samePoly[
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
      ConstructiveDiagrams`Private`samePoly[First[eezhCuratedRoute["States"]],
        ConstructiveDiagrams`Private`canonicalAmplitude[eezhCuratedPair[[2]], eezhCuratedPair[[1]]]]];
    check["curated eeZh elementary identities match every transition " <> eezhCuratedChannel,
      Length[eezhCuratedRoute["Identities"]] === eezhCuratedStepCounts[eezhCuratedChannel] &&
        Length[eezhCuratedRoute["States"]] === eezhCuratedStepCounts[eezhCuratedChannel] + 1];
    check["curated eeZh path preserves its propagator in every state " <> eezhCuratedChannel,
      AllTrue[eezhCuratedRoute["States"],
        DeleteDuplicates[Cases[#, _PropDen, Infinity]] === eezhCuratedPropagators &]];
    check["curated eeZh endpoint agrees with the accepted reference " <> eezhCuratedChannel,
      ConstructiveDiagrams`Private`samePoly[
        ConstructiveDiagrams`Private`canonicalChains[ExtractMandelstahm[Last[eezhCuratedRoute["States"]]]],
        ConstructiveDiagrams`Private`canonicalChains[ubReference["e E -> Z h", eezhCuratedChannel]]]]
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
eezzBrackets[text_] := ConstructiveDiagrams`Private`canonicalChains[
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
      ConstructiveDiagrams`Private`samePoly[First[eezzCuratedRoute["States"]],
        ConstructiveDiagrams`Private`canonicalAmplitude[eezzCuratedPair[[2]], eezzCuratedPair[[1]]]]];
    check["curated eeZZ path has four elementary edits and five states " <> eezzCuratedChannel,
      Length[eezzCuratedRoute["States"]] === 5 &&
        Counts[eezzCuratedRoute["Identities"]] === <|"MomentumSum" -> 2, "Mass" -> 2|>];
    check["curated eeZZ path preserves its propagator in every state " <> eezzCuratedChannel,
      AllTrue[eezzCuratedRoute["States"],
        DeleteDuplicates[Cases[#, _PropDen, Infinity]] === eezzCuratedPropagators &]];
    check["curated eeZZ endpoint agrees with the notebook channel reference " <> eezzCuratedChannel,
      ConstructiveDiagrams`Private`samePoly[Last[eezzCuratedRoute["States"]],
        ConstructiveDiagrams`Private`canonicalChains[eezzElectronReference[eezzCuratedChannel]]]]
  ],
  {eezzCuratedPair, eezzElectronPairs}
];
check["both eeZZ electron endpoints reproduce the full accepted electron reference",
  (* The notebook writes the T denominator as Me^2 - s13 rather than s13 - Me^2. *)
  Together[ConstructiveDiagrams`Private`canonicalChains[
    ExtractMandelstahm[Total[Last /@ Lookup[eezzElectronRoutes, "States"]]] -
      ubReference["e E -> Z Z", "e"]]] === 0];
eezzCuratedJobs = permutedRoutes[eezzElectronRoutes, All];
check["both eeZZ electron routes receive all particle-label permutations", Length[eezzCuratedJobs] === 48 &&
  Sort[Values[Counts[Lookup[eezzCuratedJobs, "Name"]]]] === {24, 24}];
Do[
  eezzOriginalRoute = SelectFirst[eezzElectronRoutes, #["Name"] === eezzCuratedJob["Name"] &];
  check["eeZZ permuted transitions replay exact legal identities " <>
    ToString[{eezzCuratedJob["Name"], eezzCuratedJob["LegPermutation"]}, InputForm],
    replayPermutedRoute[eezzCuratedJob, eezzOriginalRoute]],
  {eezzCuratedJob, eezzCuratedJobs}];
(* Handwritten electron exchange with two external Higgs bosons. *)
eehhCuratedPairs = ReduceSpinContractions[
  diagramAmplitudes[CreateDiagrams[{"e", "E", "h", "h"}, ubModel]]];
eehhElectronPairs = Select[eehhCuratedPairs,
  !FreeQ[#[[2]], PropDen[Mom[Multiparticle[1, 3] | Multiparticle[1, 4]], Me]] &];
eehhElectronRoutes = Select[allCuratedRoutes,
  StringStartsQ[#["Name"], "e, E, h, h / electron"] &];
check["fresh eeHH diagrams contain both curated electron channels",
  Length[eehhElectronPairs] === 2 && Length[eehhElectronRoutes] === 2];
eehhBrackets[text_] := ConstructiveDiagrams`Private`canonicalChains[
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
      ConstructiveDiagrams`Private`samePoly[First[eehhRoute["States"]],
        ConstructiveDiagrams`Private`canonicalAmplitude[eehhPair[[2]], eehhPair[[1]]]]];
    check["curated eeHH path records four elementary identities " <> eehhChannel,
      Length[eehhRoute["States"]] === 5 &&
        eehhRoute["Identities"] === {"MomentumSum", "MomentumSum", "Mass", "Mass"}];
    check["curated eeHH path preserves its off-shell denominator " <> eehhChannel,
      AllTrue[eehhRoute["States"], DeleteDuplicates[Cases[#, _PropDen, Infinity]] === {eehhDenominator} &]];
    eehhEndpoint = -EE^2 Me^2/(4 MW^2 SW^2 eehhDenominator)*
      (2 Me eehhBrackets["<12>+[12]"] +
        eehhBrackets[If[eehhLeg === 3, "[132>+[231>", "[142>+[241>"]]);
    check["curated eeHH endpoint matches the known channel without a spinor reducer " <> eehhChannel,
      ConstructiveDiagrams`Private`samePoly[Last[eehhRoute["States"]],
        ConstructiveDiagrams`Private`canonicalChains[eehhEndpoint]]];
    check["curated eeHH complexity drops from twenty-two to twelve " <> eehhChannel,
      SpinorExpressionComplexity /@ eehhRoute["States"] === {22, 22, 22, 17, 12}]
  ],
  {eehhLeg, {3, 4}}];
check["both eeHH electron endpoints reproduce the full notebook electron reference",
  ConstructiveDiagrams`Private`samePoly[
    ConstructiveDiagrams`Private`canonicalChains[
      ExtractMandelstahm[Total[Last /@ Lookup[eehhElectronRoutes, "States"]]]],
    ConstructiveDiagrams`Private`canonicalChains[ubReference["e E -> h h", "e"]]]];
eehhCuratedJobs = permutedRoutes[eehhElectronRoutes, All];
check["both eeHH electron paths receive all particle-label permutations", Length[eehhCuratedJobs] === 48 &&
  Sort[Values[Counts[Lookup[eehhCuratedJobs, "Name"]]]] === {24, 24}];
Do[
  eehhOriginalRoute = SelectFirst[eehhElectronRoutes, #["Name"] === eehhJob["Name"] &];
  check["eeHH permuted transitions replay exact legal identities " <>
    ToString[{eehhJob["Name"], eehhJob["LegPermutation"]}, InputForm],
    replayPermutedRoute[eehhJob, eehhOriginalRoute]],
  {eehhJob, eehhCuratedJobs}];
Print["Checking the massive-vector diagram boundaries and endpoints."];
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
      ConstructiveDiagrams`Private`samePoly[First[eewwCuratedRoute["States"]],
        ConstructiveDiagrams`Private`canonicalAmplitude[eewwCuratedPair[[2]], eewwCuratedPair[[1]]]]];
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
    Together[ConstructiveDiagrams`Private`canonicalChains[
      ExtractMandelstahm[Last[eewwCuratedRoute["States"]]] - ubReference["e E -> W- W+", "Z"]]] === 0];
  check["curated eeWW Z route reduces complexity from 376 to 22",
    SpinorExpressionComplexity /@ eewwCuratedRoute["States"][[{1, -1}]] === {376, 22}]
];
curatedPermutationReplayQ[permutedStates_, originalEdits_, ids_] := Module[{states, edits},
  If[Length[originalEdits] =!= Length[permutedStates] - 1, Return[False]];
  states = permutedStates;
  edits = (Join[#, <|
    "Subtract" -> ConstructiveDiagrams`Private`canonicalChains[ConstructiveDiagrams`Private`relabelLegs[#["Subtract"], ids]],
    "Insert" -> ConstructiveDiagrams`Private`canonicalChains[ConstructiveDiagrams`Private`relabelLegs[#["Insert"], ids]]|>] &) /@
    originalEdits;
  And @@ MapThread[ConstructiveDiagrams`Private`samePoly[
    ConstructiveDiagrams`Private`canonicalChains[#1 - #2],
    ConstructiveDiagrams`Private`canonicalChains[#3["Subtract"] - #3["Insert"]]] &,
    {Most[states], Rest[states], edits}]
];
Print["Checking the bosonic Z path and its exact permutation replays."];
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
      ConstructiveDiagrams`Private`samePoly[First[zhwwCuratedStates],
        ConstructiveDiagrams`Private`canonicalAmplitude[zhwwCuratedPair[[2]], zhwwCuratedPair[[1]]]]];
  check["bosonic Z endpoint preserves the source notebook's accepted opposite sign",
    zhwwCuratedRoute["TemplateSign"] === -1 &&
      Together[ConstructiveDiagrams`Private`canonicalChains[ExtractMandelstahm[Last[zhwwCuratedStates]] +
        ubReference["Z h -> W- W+", "Z:S"]]] === 0];
  check["bosonic Z route reduces complexity from 188 to 14",
    SpinorExpressionComplexity /@ zhwwCuratedStates[[{1, -1}]] === {188, 14}];
  check["bosonic Z route preserves its off-shell propagator in every state",
    AllTrue[zhwwCuratedStates, DeleteDuplicates[Cases[#, _PropDen, Infinity]] === zhwwCuratedPole &]];
  zhwwVerified = verifiedRoute[zhwwCuratedRoute];
  zhwwRecordedEdits = If[AssociationQ[zhwwVerified], zhwwVerified["Moves"], {}];
  Do[
    check["bosonic Z elementary identity " <> ToString[step],
      Length[zhwwRecordedEdits] === Length[zhwwCuratedStates] - 1 &&
        AssociationQ[zhwwRecordedEdits[[step]]] &&
        zhwwRecordedEdits[[step, "Name"]] === zhwwCuratedRoute["Identities"][[step]]],
    {step, Length[zhwwCuratedStates] - 1}];
  zhwwCuratedJobs = permutedRoutes[{zhwwCuratedRoute}, All];
  check["bosonic Z route receives every particle permutation", Length[zhwwCuratedJobs] === 24 &&
    DuplicateFreeQ[Lookup[zhwwCuratedJobs, "LegPermutation"]]];
  Do[
    check["bosonic Z permuted boundary, target, and propagator " <>
      ToString[Values[zhwwCuratedJob["LegPermutation"]], InputForm],
      ConstructiveDiagrams`Private`samePoly[First[zhwwCuratedJob["States"]],
        ConstructiveDiagrams`Private`relabelLegs[First[zhwwCuratedStates], zhwwCuratedJob["LegPermutation"]]] &&
      ConstructiveDiagrams`Private`samePoly[Last[zhwwCuratedJob["States"]],
        ConstructiveDiagrams`Private`relabelLegs[Last[zhwwCuratedStates], zhwwCuratedJob["LegPermutation"]]] &&
      AllTrue[zhwwCuratedJob["States"], DeleteDuplicates[Cases[#, _PropDen, Infinity]] ===
        ConstructiveDiagrams`Private`relabelLegs[zhwwCuratedPole, zhwwCuratedJob["LegPermutation"]] &]];
    check["bosonic Z permuted intermediate applications " <>
      ToString[Values[zhwwCuratedJob["LegPermutation"]], InputForm],
      curatedPermutationReplayQ[zhwwCuratedJob["States"], zhwwRecordedEdits, zhwwCuratedJob["LegPermutation"]]],
    {zhwwCuratedJob, zhwwCuratedJobs}]
];
Print["Checking the charged-current paths and their exact permutation replays."];
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
        ConstructiveDiagrams`Private`samePoly[First[chargedCurrentStates],
          ConstructiveDiagrams`Private`canonicalAmplitude[chargedCurrentPair[[2]], chargedCurrentPair[[1]]]]];
    check["charged-current endpoint exactly matches the original notebook " <> chargedCurrentSpecification[[1]],
      Together[ConstructiveDiagrams`Private`canonicalChains[ExtractMandelstahm[Last[chargedCurrentStates]] -
        ubReference[chargedCurrentSpecification[[3]], "W+"]]] === 0];
    check["charged-current complexity reduction " <> chargedCurrentSpecification[[1]],
      SpinorExpressionComplexity /@ chargedCurrentStates[[{1, -1}]] === chargedCurrentSpecification[[4]]];
    check["charged-current route preserves its off-shell propagator " <> chargedCurrentSpecification[[1]],
      AllTrue[chargedCurrentStates, DeleteDuplicates[Cases[#, _PropDen, Infinity]] === chargedCurrentPole &]];
    chargedCurrentVerified = verifiedRoute[chargedCurrentRoute];
    chargedCurrentRecordedEdits = If[AssociationQ[chargedCurrentVerified],
      chargedCurrentVerified["Moves"], {}];
    Do[
      check["charged-current elementary edit " <> chargedCurrentSpecification[[1]] <> " / " <> ToString[step],
        Length[chargedCurrentRecordedEdits] === Length[chargedCurrentStates] - 1 &&
          AssociationQ[chargedCurrentRecordedEdits[[step]]] &&
          chargedCurrentRecordedEdits[[step, "Name"]] === chargedCurrentRoute["Identities"][[step]]],
      {step, Length[chargedCurrentStates] - 1}];
    chargedCurrentJobs = permutedRoutes[{chargedCurrentRoute}, All];
    check["charged-current route receives every particle permutation " <> chargedCurrentSpecification[[1]],
      Length[chargedCurrentJobs] === 24 && DuplicateFreeQ[Lookup[chargedCurrentJobs, "LegPermutation"]]];
    Do[
      check["charged-current permuted boundary and target " <> chargedCurrentSpecification[[1]] <> " / " <>
        ToString[Values[chargedCurrentJob["LegPermutation"]], InputForm],
        ConstructiveDiagrams`Private`samePoly[First[chargedCurrentJob["States"]],
          ConstructiveDiagrams`Private`relabelLegs[First[chargedCurrentStates], chargedCurrentJob["LegPermutation"]]] &&
        ConstructiveDiagrams`Private`samePoly[Last[chargedCurrentJob["States"]],
          ConstructiveDiagrams`Private`relabelLegs[Last[chargedCurrentStates], chargedCurrentJob["LegPermutation"]]] &&
        AllTrue[chargedCurrentJob["States"], DeleteDuplicates[Cases[#, _PropDen, Infinity]] ===
          ConstructiveDiagrams`Private`relabelLegs[chargedCurrentPole, chargedCurrentJob["LegPermutation"]] &]];
      check["charged-current permuted intermediate applications " <> chargedCurrentSpecification[[1]] <> " / " <>
        ToString[Values[chargedCurrentJob["LegPermutation"]], InputForm],
        curatedPermutationReplayQ[chargedCurrentJob["States"], chargedCurrentRecordedEdits,
          chargedCurrentJob["LegPermutation"]]],
      {chargedCurrentJob, selectedPermutations[chargedCurrentJobs]}]
  ],
  {chargedCurrentSpecification, {
    {"ne, E, W-, h / W+ exchange", {"ne", "E", "W-", "h"}, "ne E -> W- h", {37, 6}},
    {"u, D, W-, h / W+ exchange", {"u", "D", "W-", "h"}, "u D -> W- h", {37, 11}}}}];
(* The new massive-external channels use independently generated diagram
   boundaries and the notebook's existing channel references. *)
Print["Checking the remaining massive-vector paths against fresh diagrams."];
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
      ConstructiveDiagrams`Private`samePoly[First[newCuratedStates],
        ConstructiveDiagrams`Private`canonicalAmplitude[newCuratedPair[[2]], newCuratedPair[[1]]]]];
    check["new curated endpoint agrees with the independent notebook reference " <> newCuratedName,
      !MissingQ[newCuratedReference] &&
        Together[ConstructiveDiagrams`Private`canonicalChains[ExtractMandelstahm[Last[newCuratedStates]] -
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
    newCuratedJobs = permutedRoutes[{newCuratedRoute}, All];
    check["new curated route receives all particle-label permutations " <> newCuratedName,
      Length[newCuratedJobs] === 24 && DuplicateFreeQ[Lookup[newCuratedJobs, "LegPermutation"]]]
  ],
  {newCuratedSpecification, newCuratedSpecifications}
];
check["real Z numerator contains composite momentum chains",
  !FreeQ[zCompositeOriginal, c_SpinorChain /; !FreeQ[c, Mom[_Multiparticle]]]];
check["real Z legal candidates expose composite splitting",
  AnyTrue[SpinorIdentityCandidates[{zCompositeRules, zCompositeOriginal}],
    #["Name"] === "MomentumSum" && Lookup[#, "Identity", None] === "Split" &]];
zCompositeSplit = zCompositeOriginal;
zCompositeSplitSteps = 0;
Do[
  zCompositeEdits = Select[ConstructiveDiagrams`Private`momentumSumMoves[zCompositeSplit, zCompositeRules],
    Lookup[#, "Identity", None] === "Split" &];
  If[zCompositeEdits === {}, Break[]];
  zCompositeSplit = ConstructiveDiagrams`Private`applyCandidate[zCompositeSplit, First[zCompositeEdits]];
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
  zCompositeEdits = Select[ConstructiveDiagrams`Private`chainMoves[zCompositeContracted, zCompositeRules],
    #["Name"] === "Mass" && #["Direction"] === "Contract" &];
  If[zCompositeEdits === {}, Break[]];
  zCompositeContracted = ConstructiveDiagrams`Private`applyCandidate[zCompositeContracted, First[zCompositeEdits]];
  zCompositeMassSteps++, {100}];
check["real Z split chains expose endpoint mass contractions", zCompositeMassSteps > 0];
check["real Z mass contractions preserve algebra and propagator",
  Expand[ReduceSpinorProducts[zCompositeOriginal - zCompositeContracted] /. zCompositeRules] === 0 &&
    DeleteDuplicates[Cases[zCompositeContracted, _PropDen, Infinity]] === zCompositePropagators];
Print["Checking the photon identity paths and label permutations."];
electronPhotonRoutes = simplificationElectronPhotonPairRoute[];
check["equal-helicity photon example has three curated paths",
  ListQ[electronPhotonRoutes] && Length[electronPhotonRoutes] === 3 &&
    validRoutesQ[electronPhotonRoutes]];
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
    ConstructiveDiagrams`Private`canonicalAmplitude[
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
      ConstructiveDiagrams`Private`samePoly[First[electronPhotonRoutes[[index]]["States"]],
        electronPhotonGauge[electronPhotonPair]]];
    check["equal-helicity photon channel agrees with its separately reduced reference " <> ToString[index],
      ConstructiveDiagrams`Private`samePoly[Last[electronPhotonRoutes[[index]]["States"]],
        electronPhotonTargets[[index]]]];
    check["equal-helicity photon channel keeps its own numerator shell conditions " <> ToString[index],
      electronPhotonRoutes[[index]]["OnShellChannels"] ===
        If[index === 1, {{1, 3}, {2, 4}}, {{1, 4}, {2, 3}}]];
    check["equal-helicity photon channel preserves only its own off-shell propagator " <> ToString[index],
      AllTrue[electronPhotonRoutes[[index]]["States"],
        DeleteDuplicates[Cases[#, _PropDen, Infinity]] === {electronPhotonPoles[[index]]} &]],
    {index, 2}];
  check["equal-helicity photon sum starts after the two channel-local reductions",
    ConstructiveDiagrams`Private`samePoly[First[electronPhotonStates],
      Total[Last /@ Lookup[Take[electronPhotonRoutes, 2], "States"]]]];
  check["equal-helicity photon endpoint equals the notebook reference without a spinor reducer",
    ConstructiveDiagrams`Private`samePoly[Last[electronPhotonStates], electronPhotonTarget]];
  check["equal-helicity photon sum has no internal shell assumptions",
    electronPhotonRoute["OnShellChannels"] === {} &&
      FreeQ[electronPhotonRoute["Masses"], Mass[_Multiparticle]] &&
      FreeQ[electronPhotonRoute["Identities"], "OnShell"]];
  check["equal-helicity photon states retain only the original off-shell propagators",
    AllTrue[electronPhotonStates, Function[expr,
      AllTrue[Cases[expr, _PropDen, Infinity], MemberQ[electronPhotonPoles, #] &]]] &&
      Sort[DeleteDuplicates[Cases[Last[electronPhotonStates], _PropDen, Infinity]]] ===
        Sort[electronPhotonPoles]];
  check["equal-helicity photon identity states have no reference spinors or x factors",
    FreeQ[Lookup[electronPhotonRoutes, "States"], Spinor["Xi", ___] | _xFactor | _xTildeFactor | _Mass]];
  check["equal-helicity photon route records every elementary identity",
    AllTrue[electronPhotonRoutes, Length[#["Identities"]] === Length[#["States"]] - 1 &&
      FreeQ[#["Identities"], "ReduceSpinorProducts" | "SimplifySpinorProducts"] &]];
  electronPhotonJobs = permutedRoutes[electronPhotonRoutes, All];
  check["equal-helicity photon route receives all particle-label permutations",
    Length[electronPhotonJobs] === 72 &&
      AllTrue[GatherBy[electronPhotonJobs, #["Name"] &],
        Length[#] === 24 && DuplicateFreeQ[Lookup[#, "LegPermutation"]] &]];
  Do[
    Block[{ConstructiveDiagrams`Private`$onShellChannels =
          routeChannels[electronPhotonJob],
        ConstructiveDiagrams`Private`$useMomentumConservation =
          Lookup[electronPhotonJob, "MomentumConservation", True]},
      electronPhotonOriginalRoute = SelectFirst[electronPhotonRoutes,
        #["Name"] === electronPhotonJob["Name"] &];
      electronPhotonVerified = If[replayPermutedRoute[electronPhotonJob, electronPhotonOriginalRoute],
        verifiedRoute[electronPhotonOriginalRoute], $Failed];
      check["equal-helicity photon permuted path consists entirely of legal edits " <>
          electronPhotonJob["Name"] <> " " <> ToString[Values[electronPhotonJob["LegPermutation"]], InputForm],
        AssociationQ[electronPhotonVerified] &&
          Length[electronPhotonVerified["Moves"]] === electronPhotonJob["Steps"]];
      check["equal-helicity photon permuted endpoint matches the reference " <>
          electronPhotonJob["Name"] <> " " <> ToString[Values[electronPhotonJob["LegPermutation"]], InputForm],
        ConstructiveDiagrams`Private`samePoly[Last[electronPhotonJob["States"]],
          ConstructiveDiagrams`Private`relabelLegs[electronPhotonTargets[[-electronPhotonJob["AmplitudeIndex"]]],
            electronPhotonJob["LegPermutation"]]]]
    ],
    {electronPhotonJob, selectedPermutations[electronPhotonJobs]}]
];
(* Opposite photon helicities have two channel-local paths and one external-
   shell-only sum path. Every transition is an explicit elementary identity. *)
oppositePhotonRoutes = simplificationElectronOppositePhotonRoutes[];
check["opposite-helicity photon example has three curated paths",
  ListQ[oppositePhotonRoutes] && Length[oppositePhotonRoutes] === 3 &&
    validRoutesQ[oppositePhotonRoutes]];
If[ListQ[oppositePhotonRoutes] && Length[oppositePhotonRoutes] === 3,
  oppositePhotonPoles = {PropDen[Mom[Multiparticle[1, 3]], Me], PropDen[Mom[Multiparticle[1, 4]], Me]};
  oppositePhotonPairs = diagramAmplitudes[CreateDiagrams[{"e", "E", "A.+", "A.-"}, ubModel]];
  oppositePhotonPrepared = Table[
    pair = SelectFirst[oppositePhotonPairs, !FreeQ[#[[2]], pole] &];
    ConstructiveDiagrams`Private`canonicalAmplitude[ReduceSpinContractions[pair[[2]]] /.
      oppositePhotonRoutes[[1]]["ReferenceSpinorPreparation"], pair[[1]]],
    {pole, oppositePhotonPoles}];
  oppositePhotonTarget = ConstructiveDiagrams`Private`canonicalChains[-2 EE^2*
    ConvertStringAmplitude["[13]<24>+[23]<14>", {Me, Me, 0, 0}]*
    ConvertStringAmplitude["[314>", {Me, Me, 0, 0}]/(Times @@ oppositePhotonPoles)];
  check["opposite-helicity photon public diagrams contain exactly the two electron channels",
    Length[oppositePhotonPairs] === 2 &&
      Sort[DeleteDuplicates[Cases[oppositePhotonPairs[[All, 2]], _PropDen, Infinity]]] === Sort[oppositePhotonPoles]];
  Do[
    check["opposite-helicity photon route starts at fresh gauge-fixed diagram " <> ToString[index],
      ConstructiveDiagrams`Private`samePoly[First[oppositePhotonRoutes[[index]]["States"]], oppositePhotonPrepared[[index]]]];
    check["opposite-helicity photon channel keeps its own numerator shell conditions " <> ToString[index],
      oppositePhotonRoutes[[index]]["OnShellChannels"] ===
        If[index === 1, {{1, 3}, {2, 4}}, {{1, 4}, {2, 3}}]];
    check["opposite-helicity photon channel preserves its off-shell propagator " <> ToString[index],
      AllTrue[oppositePhotonRoutes[[index]]["States"],
        DeleteDuplicates[Cases[#, _PropDen, Infinity]] === {oppositePhotonPoles[[index]]} &]],
    {index, 2}];
  check["opposite-helicity photon sum starts at both channel endpoints",
    ConstructiveDiagrams`Private`samePoly[First[oppositePhotonRoutes[[3]]["States"]],
      Total[Last /@ Lookup[Take[oppositePhotonRoutes, 2], "States"]]]];
  check["opposite-helicity photon sum has no internal shell assumptions",
    oppositePhotonRoutes[[3]]["OnShellChannels"] === {} &&
      FreeQ[oppositePhotonRoutes[[3]]["Masses"], _Multiparticle]];
  check["opposite-helicity photon sum preserves both off-shell propagators",
    AllTrue[oppositePhotonRoutes[[3]]["States"],
      Sort[DeleteDuplicates[Cases[#, _PropDen, Infinity]]] === Sort[oppositePhotonPoles] &]];
  check["opposite-helicity photon endpoint equals the handwritten expression",
    ConstructiveDiagrams`Private`samePoly[Last[oppositePhotonRoutes[[3]]["States"]], oppositePhotonTarget]];
  check["opposite-helicity photon endpoint equals the independent reference catalog",
    ConstructiveDiagrams`Private`samePoly[ExtractMandelstahm[Last[oppositePhotonRoutes[[3]]["States"]]],
      ConstructiveDiagrams`Private`canonicalChains[ubReference["e E -> A.+ A.-", "e"]]]];
  check["opposite-helicity photon states have no reference spinors or x factors",
    FreeQ[Lookup[oppositePhotonRoutes, "States"], Spinor["Xi", ___] | _xFactor | _xTildeFactor | _Mass]];
  check["opposite-helicity photon paths record all 51 elementary identities",
    Length /@ Lookup[oppositePhotonRoutes, "Identities"] === {19, 18, 14} &&
      MapThread[Length[#1] === Length[#2] - 1 &,
        {Lookup[oppositePhotonRoutes, "Identities"], Lookup[oppositePhotonRoutes, "States"]}] ===
        {True, True, True} &&
      FreeQ[Lookup[oppositePhotonRoutes, "Identities"], "ReduceSpinorProducts" | "SimplifySpinorProducts"]];
  oppositePhotonJobs = permutedRoutes[oppositePhotonRoutes, All];
  check["opposite-helicity photon paths receive all 72 particle-label permutations",
    Length[oppositePhotonJobs] === 72 && AllTrue[GatherBy[oppositePhotonJobs, #["Name"] &],
      Length[#] === 24 && DuplicateFreeQ[Lookup[#, "LegPermutation"]] &]];
  Do[
    Block[{ConstructiveDiagrams`Private`$onShellChannels = routeChannels[job],
      ConstructiveDiagrams`Private`$useMomentumConservation = Lookup[job, "MomentumConservation", True]},
      oppositePhotonOriginalRoute = SelectFirst[oppositePhotonRoutes, #["Name"] === job["Name"] &];
      oppositePhotonVerified = If[replayPermutedRoute[job, oppositePhotonOriginalRoute],
        verifiedRoute[oppositePhotonOriginalRoute], $Failed];
      check["opposite-helicity photon permuted path consists entirely of legal edits " <>
        job["Name"] <> " " <> ToString[Values[job["LegPermutation"]], InputForm],
        AssociationQ[oppositePhotonVerified] && Length[oppositePhotonVerified["Moves"]] === job["Steps"]];
      If[job["Name"] === oppositePhotonRoutes[[3]]["Name"],
        check["opposite-helicity photon permuted sum matches the reference " <>
          ToString[Values[job["LegPermutation"]], InputForm],
          ConstructiveDiagrams`Private`samePoly[Last[job["States"]],
            ConstructiveDiagrams`Private`relabelLegs[oppositePhotonTarget, job["LegPermutation"]]]]]
    ], {job, selectedPermutations[oppositePhotonJobs]}]
];

Do[
  Print["Verifying every original identity in ", route["Name"]];
  check["every stored transition is an exact legal identity: " <> route["Name"],
    AssociationQ[verifiedRoute[route]]],
  {route, allCuratedRoutes}];
Print[<|"Checks" -> checks, "Failed" -> Length[failures], "Failures" -> failures|>];
Quit[If[failures === {}, 0, 1]];
