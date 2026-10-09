(* ::Package:: *)

(* Reference Amplitudes
   Copied from SM-4-point.nb and SM-4-point-test.nb, without evaluating diagrams.
   337 reference occurrences, 195 distinct {mass rules, amplitude} pairs.
   Exact duplicates share Sources; algebraically equivalent forms are retained.
   Main-notebook input numbers index the extracted title/section/input cells
   from zero. Source mass/coupling conventions are intentionally unchanged.
   These are channel references as well as channel sums, not 195 processes.
   Only external masses are supplied: no extra internal on-shell assumptions.
   A separate synthetic five-point section supplies algebraic training fixtures,
   not diagram-derived physical amplitudes. Its internal-channel rules are explicit.
*)

(* ::Section::Closed:: *)
(* Curated Simplification Routes *)

(* Evaluated only by training, not by readers of the reference catalog below.
   The e, E, M, m and e, E, Z, h routes follow the handwritten derivations in
   SM-4-point-unscramble-test.nb, decomposing its reducer calls into legal edits.
   The e, E, Z, Z routes reconstruct the channel targets in SM-4-point-test.nb.
   The e, E, W-, W+ Z route also decomposes its legacy standard-form reduction.
   Charged-current four-fermion, W/h, and Z/W routes reconstruct the same notebook's
   channel simplifications, as do all three bosonic Z/h/W/W channels. The bosonic
   routes preserve the notebook's explicitly accepted overall-sign convention.
   Each starts at diagramAmplitudes plus ReduceSpinContractions. No trained net,
   notebook globals, or black-box spinor reducer are used to build the route.
   MW == MZ CW is only an endpoint comparison convention, never a route edit. *)
trainingSimplificationRoutes::build = "Could not construct a curated simplification route using the current legal identities.";

(* ::Subsection::Closed:: *)
(* Four-Fermion Z Exchange *)
trainingFourFermionZRoute[] := Module[
  {a, s, c, masses, state, states, expected, record, walk, choices, chainAt,
    brackets, identityMatches, schoutenOne, schoutenTwo, routeBuilt, identities = {}},
  If[!MemberQ[$Packages, "Unscrambling`"], Message[trainingSimplificationRoutes::build]; Return[$Failed]];
  a[i_, j_] := SpinorChain[Spinor["Spin", "Angle", i], Spinor["Spin", "Angle", j]];
  s[i_, j_] := SpinorChain[Spinor["Spin", "Square", i], Spinor["Spin", "Square", j]];
  c[i_, j_] := SpinorChain[Spinor["Spin", "Angle", i], Mom[Multiparticle[1, 2]], Spinor["Spin", "Square", j]];
  masses = {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> Mm, Mass[4] -> Mm,
    Mass[Multiparticle[1, 2]] -> MZ, Mass[Multiparticle[3, 4]] -> MZ};
  state = Unscrambling`Private`canonicalChains[-EE^2/(4 MW^2 SW^2 PropDen[Mom[Multiparticle[1, 2]], MZ])*
    (MZ^2 (gRe^2 a[2, 3] s[1, 4] + gLe^2 a[1, 4] s[2, 3] +
       gLe gRe (a[1, 3] s[2, 4] + a[2, 4] s[1, 3])) +
     gRe^2 c[2, 4] c[3, 1] + gLe gRe c[2, 3] c[4, 1] +
     gLe gRe c[1, 4] c[3, 2] + gLe^2 c[4, 2] c[1, 3])];
  expected = -EE^2/(4 MW^2 SW^2 PropDen[Mom[Multiparticle[1, 2]], MZ])*
    (2 MZ^2 (gRe^2 a[2, 3] s[1, 4] + gLe^2 a[1, 4] s[2, 3] +
       gLe gRe (a[1, 3] s[2, 4] + a[2, 4] s[1, 3])) +
     Me Mm (gLe - gRe)^2 (a[1, 2] - s[1, 2]) (a[3, 4] - s[3, 4]));
  states = {state};
  record[candidate_] := (state = Unscrambling`Private`applyCandidate[state, candidate];
    AppendTo[states, state]; AppendTo[identities, candidate["Name"]]);
  walk[generate_, select_, limit_] := (
    Do[choices = Select[generate[], select]; If[choices === {}, Break[]]; record[First[choices]], {limit}];
    If[choices =!= {} && AnyTrue[generate[], select], Throw[$Failed]]);
  chainAt[candidate_] := With[{site = candidate["Site"]},
    DeleteDuplicates[Select[Unscrambling`Private`chainsIn[
      Unscrambling`Private`monomialsOf[state][[site[[1]]]]],
      Unscrambling`Private`validChainQ]][[site[[2]]]]];
  brackets[text_] := Unscrambling`Private`canonicalChains[ConvertStringAmplitude[text, {Me, Me, Mm, Mm}]];
  schoutenOne = {
    {brackets["[23][124>"], brackets["-<423][12]+<422][13]"]},
    {brackets["[24][123>"], brackets["-<324][12]+<322][14]"]},
    {brackets["<23>[421>"], brackets["<13>[422>-<12>[423>"]},
    {brackets["<24>[321>"], brackets["<14>[322>-<12>[324>"]}};
  schoutenTwo = {
    {brackets["[14][213>"], brackets["-<314][21]+<311][24]"]},
    {brackets["[13][214>"], brackets["-<413][21]+<411][23]"]},
    {brackets["<14>[312>"], brackets["<24>[311>-<21>[314>"]},
    {brackets["<13>[412>"], brackets["<23>[411>-<21>[413>"]}};
  identityMatches[candidate_, block_] := AnyTrue[block, Function[identity,
    With[{scale = Together[candidate["Subtract"]/identity[[1]]]},
      FreeQ[scale, _SpinorChain] && Unscrambling`Private`samePoly[
        candidate["Insert"], scale identity[[2]]]]]];
  routeBuilt = Catch[Block[{Unscrambling`Private`$onShellChannels = Automatic,
      Unscrambling`Private`$useMomentumConservation = True},
    (* Reproduce the useful preprocessing algebra without calling its reducer. *)
    walk[Function[Unscrambling`Private`momentumSumMoves[state, masses]],
      Function[Lookup[#, "Identity", None] === "Split"], 100];
    walk[Function[Unscrambling`Private`chainMoves[state, masses]],
      Function[#["Name"] === "Mass" && #["Direction"] === "Contract"], 100];
    walk[Function[Unscrambling`SchoutenCandidates[state]],
      Function[#["Splits"] === {0, 0} && AllTrue[#["Chains"],
        Length[#] === 3 && !MemberQ[{{1, 2}, {3, 4}},
          Sort[Unscrambling`Private`spinorLeg /@ {First[#], Last[#]}]] &]], 100];
    walk[Function[Unscrambling`Private`interiorChainMoves[state, masses]],
      Function[#["Name"] === "Anticommutation" && With[{chain = chainAt[#]},
        Length[chain] === 4 &&
          chain[[2]] =!= Mom[Unscrambling`Private`spinorLeg[First[chain]]] &&
          chain[[3]] =!= Mom[Unscrambling`Private`spinorLeg[Last[chain]]] &&
          (chain[[3]] === Mom[Unscrambling`Private`spinorLeg[First[chain]]] ||
           chain[[2]] === Mom[Unscrambling`Private`spinorLeg[Last[chain]]])]], 100];
    walk[Function[Unscrambling`Private`chainMoves[state, masses]],
      Function[#["Name"] === "Mass" && #["Direction"] === "Contract"], 100];
    walk[Function[Unscrambling`Private`kinematicMoves[state, masses]],
      Function[#["Name"] === "OnShell" && !FreeQ[#["Subtract"], MomProd[1, 2]] &&
        FreeQ[#["Insert"], MomProd[1, 2]]], 100];
    (* The two handwritten Schouten blocks and their intervening conservation. *)
    walk[Function[Unscrambling`SchoutenCandidates[state]],
      Function[identityMatches[#, schoutenOne]], 100];
    walk[Function[Unscrambling`Private`chainMoves[state, masses]], Function[candidate, Module[{chain},
      If[candidate["Name"] =!= "MomentumConservation", Return[False]];
      chain = chainAt[candidate];
      Length[chain] === 3 && chain[[2]] === Mom[2] &&
        Sort[Unscrambling`Private`spinorLeg /@ {First[chain], Last[chain]}] === {3, 4}]], 100];
    walk[Function[Unscrambling`Private`chainMoves[state, masses]],
      Function[#["Name"] === "Mass" && #["Direction"] === "Contract"], 100];
    walk[Function[Unscrambling`SchoutenCandidates[state]],
      Function[identityMatches[#, schoutenTwo]], 100];
    walk[Function[Unscrambling`Private`chainMoves[state, masses]],
      Function[#["Name"] === "Mass" && #["Direction"] === "Contract"], 100];
    True
  ]];
  If[routeBuilt =!= True || Length[states] < 2 || !FreeQ[Cases[state, _SpinorChain, Infinity], _Mom] ||
      !Unscrambling`Private`samePoly[state, expected],
    Message[trainingSimplificationRoutes::build]; Return[$Failed]];
  {<|"Name" -> "e, E, M, m / Z exchange", "Masses" -> masses, "States" -> states,
    "Sources" -> {"SM-4-point-unscramble-test.nb / handwritten e, E, M, m Z derivation"},
    "Identities" -> identities,
    "OnShellChannels" -> Automatic, "MomentumConservation" -> True|>}
];

(* ::Subsection::Closed:: *)
(* Electron, Positron, Z, Higgs Channels *)

trainingElectronZHZRoute[] := Module[
 {a, s, c, masses, state, states, expected, record, walk, choices, chainAt,
  brackets, identityMatches, schoutenOne, schoutenTwo, routeBuilt, identities = {}},
 If[!MemberQ[$Packages, "Unscrambling`"], Message[trainingSimplificationRoutes::build]; Return[$Failed]];
 a[i_, j_] := SpinorChain[Spinor["Spin", "Angle", i], Spinor["Spin", "Angle", j]];
 s[i_, j_] := SpinorChain[Spinor["Spin", "Square", i], Spinor["Spin", "Square", j]];
 c[i_, j_] := SpinorChain[Spinor["Spin", "Angle", i], Mom[Multiparticle[1, 2]], Spinor["Spin", "Square", j]];
 masses = {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> MZ, Mass[4] -> Mh,
  Mass[Multiparticle[1, 2]] -> MZ, Mass[Multiparticle[3, 4]] -> MZ};
 state = Unscrambling`Private`canonicalChains[EE^2/(2 Sqrt[2] MW^2 SW^2 PropDen[Mom[Multiparticle[1, 2]], MZ])*
  (MZ^2 (gRe a[2, 3] s[1, 3] + gLe a[1, 3] s[2, 3]) +
   gRe c[2, 3] c[3, 1] + gLe c[1, 3] c[3, 2])];
 expected = EE^2/(2 Sqrt[2] MW^2 SW^2 PropDen[Mom[Multiparticle[1, 2]], MZ])*
  (Me (gLe - gRe) (a[1, 2] - s[1, 2])*
   SpinorChain[Spinor["Spin", "Angle", 3], Mom[4], Spinor["Spin", "Square", 3]] +
   2 MZ^2 (gLe a[1, 3] s[2, 3] + gRe a[2, 3] s[1, 3]));
 states = {state};
 record[candidate_] := (state = Unscrambling`Private`applyCandidate[state, candidate];
  AppendTo[states, state]; AppendTo[identities, candidate["Name"]]);
 walk[generate_, select_, limit_] := (
  Do[choices = Select[generate[], select]; If[choices === {}, Break[]]; record[First[choices]], {limit}];
  If[choices =!= {} && AnyTrue[generate[], select], Throw[$Failed]]);
 chainAt[candidate_] := With[{site = candidate["Site"]},
  DeleteDuplicates[Select[Unscrambling`Private`chainsIn[
   Unscrambling`Private`monomialsOf[state][[site[[1]]]]],
   Unscrambling`Private`validChainQ]][[site[[2]]]]];
 brackets[text_] := Unscrambling`Private`canonicalChains[ConvertStringAmplitude[text, {Me, Me, MZ, Mh}]];
 schoutenOne = {
  {brackets["<12>[313>"], brackets["<32>[311>"] - brackets["<31>[312>"]},
  {brackets["<12>[323>"], brackets["<32>[321>"] - brackets["<31>[322>"]}};
 (* The final angle identity uses the legal sign for the reversed bracket <21>. *)
 schoutenTwo = {
  {brackets["[13][243>"], -brackets["<343][21]"] + brackets["<341][23]"]},
  {brackets["<23>[341>"], brackets["<13>[342>"] + brackets["<21>[343>"]}};
 identityMatches[candidate_, block_] := AnyTrue[block, Function[identity,
  With[{scale = Together[candidate["Subtract"]/identity[[1]]]},
   FreeQ[scale, _SpinorChain] && Unscrambling`Private`samePoly[
    candidate["Insert"], scale identity[[2]]]]]];
 routeBuilt = Catch[Block[{Unscrambling`Private`$onShellChannels = Automatic,
    Unscrambling`Private`$useMomentumConservation = True},
  walk[Function[Unscrambling`Private`momentumSumMoves[state, masses]],
   Function[Lookup[#, "Identity", None] === "Split"], 100];
  walk[Function[Unscrambling`Private`chainMoves[state, masses]],
   Function[#["Name"] === "Mass" && #["Direction"] === "Contract"], 100];
  walk[Function[Unscrambling`SchoutenCandidates[state]],
   Function[#["Splits"] === {1, 1} && AllTrue[#["Chains"],
    Length[#] === 3 && !MemberQ[{{1, 2}, {3, 3}},
     Sort[Unscrambling`Private`spinorLeg /@ {First[#], Last[#]}]] &]], 100];
  walk[Function[Unscrambling`Private`interiorChainMoves[state, masses]],
   Function[#["Name"] === "Anticommutation" && With[{chain = chainAt[#]},
    Length[chain] === 4 &&
     chain[[2]] =!= Mom[Unscrambling`Private`spinorLeg[First[chain]]] &&
     chain[[3]] =!= Mom[Unscrambling`Private`spinorLeg[Last[chain]]] &&
     (chain[[3]] === Mom[Unscrambling`Private`spinorLeg[First[chain]]] ||
      chain[[2]] === Mom[Unscrambling`Private`spinorLeg[Last[chain]]])]], 100];
  walk[Function[Unscrambling`Private`chainMoves[state, masses]],
   Function[#["Name"] === "Mass" && #["Direction"] === "Contract"], 100];
  walk[Function[Unscrambling`Private`kinematicMoves[state, masses]],
   Function[#["Name"] === "OnShell" && !FreeQ[#["Subtract"], MomProd[1, 2]] &&
    FreeQ[#["Insert"], MomProd[1, 2]]], 100];
  walk[Function[Unscrambling`SchoutenCandidates[state]],
   Function[identityMatches[#, schoutenOne]], 100];
  walk[Function[Unscrambling`Private`chainMoves[state, masses]],
   Function[#["Name"] === "MomentumConservation" &&
    MemberQ[brackets /@ {"[123>", "[213>", "[312>", "[321>"}, chainAt[#]]], 100];
  walk[Function[Unscrambling`Private`chainMoves[state, masses]],
   Function[#["Name"] === "Mass" && #["Direction"] === "Contract"], 100];
  walk[Function[Unscrambling`SchoutenCandidates[state]],
   Function[identityMatches[#, schoutenTwo]], 100];
  walk[Function[Unscrambling`Private`chainMoves[state, masses]],
   Function[#["Name"] === "Mass" && #["Direction"] === "Contract"], 100];
  True
 ]];
 If[routeBuilt =!= True || Length[states] < 2 || !Unscrambling`Private`samePoly[state, expected],
  Message[trainingSimplificationRoutes::build]; Return[$Failed]];
 {<|"Name" -> "e, E, Z, h / Z exchange", "Masses" -> masses, "States" -> states,
   "Sources" -> {"SM-4-point-unscramble-test.nb / handwritten e, E, Z, h Z derivation"},
   "Identities" -> identities, "OnShellChannels" -> Automatic, "MomentumConservation" -> True|>}
];

trainingElectronZHElectronRoutes[] := Module[
 {a, s, x, y, index, masses, momentum, state, states, edits, expected, record, walk,
  choices, chainAt, routeBuilt, routes = {}},
 If[!MemberQ[$Packages, "Unscrambling`"], Message[trainingSimplificationRoutes::build]; Return[$Failed]];
 a[i_, j_] := SpinorChain[Spinor["Spin", "Angle", i], Spinor["Spin", "Angle", j]];
 s[i_, j_] := SpinorChain[Spinor["Spin", "Square", i], Spinor["Spin", "Square", j]];
 x[i_, q_, j_] := SpinorChain[Spinor["Spin", "Square", i], Mom[q], Spinor["Spin", "Angle", j]];
 y[i_, q_, j_] := SpinorChain[Spinor["Spin", "Angle", i], Mom[q], Spinor["Spin", "Square", j]];
 record[candidate_] := (state = Unscrambling`Private`applyCandidate[state, candidate];
  AppendTo[states, state]; AppendTo[edits, candidate]);
 walk[generate_, select_, limit_] := (
  Do[choices = Select[generate[], select]; If[choices === {}, Break[]]; record[First[choices]], {limit}];
  If[choices =!= {} && AnyTrue[generate[], select], Throw[$Failed]]);
 chainAt[candidate_] := With[{site = candidate["Site"]},
  DeleteDuplicates[Select[Unscrambling`Private`chainsIn[
   Unscrambling`Private`monomialsOf[state][[site[[1]]]]],
   Unscrambling`Private`validChainQ]][[site[[2]]]]];
 Do[
  masses = {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> MZ, Mass[4] -> Mh,
   Mass[Multiparticle[1, If[index === 2, 3, 4]]] -> Me,
   Mass[Multiparticle[2, If[index === 2, 4, 3]]] -> Me};
  momentum = If[index === 2, Multiparticle[1, 3], Multiparticle[1, 4]];
  state = Unscrambling`Private`canonicalChains[
   EE^2 Me/(2 Sqrt[2] MW^2 SW^2 PropDen[Mom[momentum], Me])*
    (gRe Me a[2, 3] s[1, 3] + gLe Me a[1, 3] s[2, 3] +
     If[index === 2,
      -gLe a[1, 3] y[2, momentum, 3] - gRe s[1, 3] y[3, momentum, 2],
       gRe a[2, 3] y[1, momentum, 3] + gLe s[2, 3] y[3, momentum, 1]])];
  states = {state}; edits = {};
  routeBuilt = Catch[Block[{Unscrambling`Private`$onShellChannels = Automatic,
    Unscrambling`Private`$useMomentumConservation = True},
   walk[Function[Unscrambling`Private`momentumSumMoves[state, masses]],
    Function[Lookup[#, "Identity", None] === "Split"], 100];
   walk[Function[Unscrambling`Private`chainMoves[state, masses]],
    Function[#["Name"] === "Mass" && #["Direction"] === "Contract"], 100];
   If[index === 2,
    walk[Function[Unscrambling`Private`chainMoves[state, masses]], Function[candidate,
     candidate["Name"] === "MomentumConservation" && With[{chain = chainAt[candidate]},
      Length[chain] === 3 && chain[[2]] === Mom[1]]], 100];
    walk[Function[Unscrambling`Private`chainMoves[state, masses]],
     Function[#["Name"] === "Mass" && #["Direction"] === "Contract"], 100]
   ];
   True
  ]];
  expected = If[index === 2,
   EE^2 Me/(2 Sqrt[2] MW^2 SW^2 PropDen[Mom[Multiparticle[1, 3]], Me])*
    (gRe s[1, 3] (2 Me a[2, 3] + x[2, 4, 3]) +
     gLe a[1, 3] (2 Me s[2, 3] + x[3, 4, 2])),
   EE^2 Me/(2 Sqrt[2] MW^2 SW^2 PropDen[Mom[Multiparticle[1, 4]], Me])*
    (gLe s[2, 3] (2 Me a[1, 3] + x[1, 4, 3]) +
     gRe a[2, 3] (2 Me s[1, 3] + x[3, 4, 1]))];
  If[routeBuilt =!= True || Length[states] < 2 ||
    !Unscrambling`Private`samePoly[state, Unscrambling`Private`canonicalChains[expected]],
   Message[trainingSimplificationRoutes::build]; Return[$Failed]];
  AppendTo[routes, <|"Name" -> If[index === 2,
    "e, E, Z, h / electron (1, 3) exchange", "e, E, Z, h / electron (1, 4) exchange"],
   "Masses" -> masses, "States" -> states, "Identities" -> Lookup[edits, "Name"],
   "Sources" -> {If[index === 2,
    "SM-4-point-unscramble-test.nb / In[666], In[670], Out[670]",
    "SM-4-point-unscramble-test.nb / In[672], Out[672]"]},
   "OnShellChannels" -> Automatic, "MomentumConservation" -> True|>],
 {index, {2, 3}}];
 routes
];

(* ::Subsection::Closed:: *)
(* Electron, Positron, Two Z Bosons *)

trainingElectronZZElectronRoutes[] := Module[
 {a, s, y, index, masses, momentum, denominator, state, states, identities,
  expected, record, walk, choices, routeBuilt, routes = {}},
 If[!MemberQ[$Packages, "Unscrambling`"], Message[trainingSimplificationRoutes::build]; Return[$Failed]];
 a[i_, j_] := SpinorChain[Spinor["Spin", "Angle", i], Spinor["Spin", "Angle", j]];
 s[i_, j_] := SpinorChain[Spinor["Spin", "Square", i], Spinor["Spin", "Square", j]];
 y[i_, q_, j_] := SpinorChain[Spinor["Spin", "Angle", i], Mom[q], Spinor["Spin", "Square", j]];
 record[candidate_] := (state = Unscrambling`Private`applyCandidate[state, candidate];
  AppendTo[states, state]; AppendTo[identities, candidate["Name"]]);
 walk[generate_, select_, limit_] := (
  Do[choices = Select[generate[], select]; If[choices === {}, Break[]]; record[First[choices]], {limit}];
  If[choices =!= {} && AnyTrue[generate[], select], Throw[$Failed]]);
 Do[
  momentum = Multiparticle[1, index]; denominator = PropDen[Mom[momentum], Me];
  masses = {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> MZ, Mass[4] -> MZ,
   Mass[momentum] -> Me, Mass[Multiparticle[2, 7 - index]] -> Me};
  state = Unscrambling`Private`canonicalAmplitude[
   EE^2/(2 MW^2 SW^2 denominator)*If[index === 3,
    gLe gRe Me (a[3, 4] s[1, 3] s[2, 4] + a[1, 3] a[2, 4] s[3, 4]) +
     gRe^2 a[2, 4] s[1, 3] y[3, momentum, 4] +
     gLe^2 a[1, 3] s[2, 4] y[4, momentum, 3],
    -gLe gRe Me (a[3, 4] s[1, 4] s[2, 3] + a[1, 4] a[2, 3] s[3, 4]) +
     gLe^2 a[1, 4] s[2, 3] y[3, momentum, 4] +
     gRe^2 a[2, 3] s[1, 4] y[4, momentum, 3]], masses];
  states = {state}; identities = {};
  routeBuilt = Catch[Block[{Unscrambling`Private`$onShellChannels = Automatic,
    Unscrambling`Private`$useMomentumConservation = True},
   (* Two sum splits expose the leg-3 or leg-4 endpoint contractions. *)
   walk[Function[Unscrambling`Private`momentumSumMoves[state, masses]],
    Function[Lookup[#, "Identity", None] === "Split"], 10];
   walk[Function[Unscrambling`Private`chainMoves[state, masses]],
    Function[#["Name"] === "Mass" && #["Direction"] === "Contract"], 10];
   True
  ]];
  expected = EE^2/(2 MW^2 SW^2 denominator)*If[index === 3,
   gLe gRe Me (a[3, 4] s[1, 3] s[2, 4] + a[1, 3] a[2, 4] s[3, 4]) +
    gRe^2 a[2, 4] s[1, 3] (MZ s[3, 4] + y[3, 1, 4]) +
    gLe^2 a[1, 3] s[2, 4] (MZ a[3, 4] + y[4, 1, 3]),
   -gLe gRe Me (a[3, 4] s[1, 4] s[2, 3] + a[1, 4] a[2, 3] s[3, 4]) +
    gLe^2 a[1, 4] s[2, 3] (-MZ a[3, 4] + y[3, 1, 4]) +
    gRe^2 a[2, 3] s[1, 4] (-MZ s[3, 4] + y[4, 1, 3])];
  If[routeBuilt =!= True || Length[states] =!= 5 ||
    Counts[identities] =!= <|"MomentumSum" -> 2, "Mass" -> 2|> ||
    !AllTrue[states, DeleteDuplicates[Cases[#, _PropDen, Infinity]] === {denominator} &] ||
    !Unscrambling`Private`samePoly[state, Unscrambling`Private`canonicalChains[expected]],
   Message[trainingSimplificationRoutes::build]; Return[$Failed]];
  AppendTo[routes, <|"Name" -> "e, E, Z, Z / electron (1, " <> ToString[index] <> ") exchange",
   "Masses" -> masses, "States" -> states, "Identities" -> identities,
   "Sources" -> {"SM-4-point-test.nb / " <> If[index === 3, "ampffZZfTTested", "ampffZZfUTested"]},
   "OnShellChannels" -> Automatic, "MomentumConservation" -> True|>],
 {index, {3, 4}}];
 routes
];

(* ::Subsection::Closed:: *)
(* Electron, Positron, W Pair: Z Exchange *)

trainingElectronWWZRoute[] := Module[
 {a, s, y, channel, masses, momentum, denominator, state, states, identities = {},
  expected, record, walk, choices, chainAt, internalQ, externalQ, chainCost,
  chainCostTotal, massClose, candidates, candidate, currentCost, routeBuilt},
 If[!MemberQ[$Packages, "Unscrambling`"], Message[trainingSimplificationRoutes::build]; Return[$Failed]];
 a[i_, j_] := SpinorChain[Spinor["Spin", "Angle", i], Spinor["Spin", "Angle", j]];
 s[i_, j_] := SpinorChain[Spinor["Spin", "Square", i], Spinor["Spin", "Square", j]];
 y[i_, q_, j_] := SpinorChain[Spinor["Spin", "Angle", i], Mom[q], Spinor["Spin", "Square", j]];
 momentum = Multiparticle[1, 2]; denominator = PropDen[Mom[momentum], MZ];
 masses = {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> MW, Mass[4] -> MW,
  Mass[momentum] -> MZ, Mass[Multiparticle[3, 4]] -> MZ};
 channel[i_, j_] := MW MZ^2 (a[i, 4] s[j, 3] + a[i, 3] s[j, 4]) (a[3, 4] + s[3, 4]) +
  MZ^2 a[3, 4] (s[j, 4] y[i, momentum, 3] + s[j, 3] y[i, momentum, 4]) +
  MZ^2 s[3, 4] (a[i, 4] y[3, momentum, j] + a[i, 3] y[4, momentum, j]) +
  MW (a[3, 4] + s[3, 4]) (y[i, momentum, 4] y[3, momentum, j] +
    y[i, momentum, 3] y[4, momentum, j]);
 state = Unscrambling`Private`canonicalAmplitude[
  EE^2/(4 MW^2 MZ^2 SW^2 denominator) (gLe channel[1, 2] + gRe channel[2, 1]), masses];
 expected = Unscrambling`Private`canonicalChains[EE^2/(2 MW^2 SW^2 denominator) (
  gLe (2 MW (a[1, 4] s[2, 3] + a[1, 3] s[2, 4]) (a[3, 4] + s[3, 4]) +
    a[3, 4] s[3, 4] (-Me a[1, 2] + Me s[1, 2] + 2 y[1, 3, 2])) +
  gRe (2 MW (a[2, 4] s[1, 3] + a[2, 3] s[1, 4]) (a[3, 4] + s[3, 4]) +
    a[3, 4] s[3, 4] (Me a[1, 2] - Me s[1, 2] + 2 y[2, 3, 1])))];
 states = {state};
 record[edit_] := (state = Unscrambling`Private`applyCandidate[state, edit];
  AppendTo[states, state]; AppendTo[identities, edit["Name"]]);
 walk[generate_, select_, limit_] := (
  Do[choices = Select[generate[], select]; If[choices === {}, Break[]]; record[First[choices]], {limit}];
  If[choices =!= {} && AnyTrue[generate[], select], Throw[$Failed]]);
 chainAt[edit_] := With[{site = edit["Site"]},
  DeleteDuplicates[Select[Unscrambling`Private`chainsIn[
   Unscrambling`Private`monomialsOf[state][[site[[1]]]]],
   Unscrambling`Private`validChainQ]][[site[[2]]]]];
 internalQ[leg_] := MemberQ[{1, 2}, leg]; externalQ[leg_] := MemberQ[{3, 4}, leg];
 (* Prefer a basis with the remaining insertion between the two fermion legs.
    Endpoint insertions have zero cost because their mass contraction follows. *)
 chainCost[chain_] := With[{parts = List @@ chain}, If[Length[parts] < 3, 0,
  With[{first = Unscrambling`Private`spinorLeg[First[parts]],
    last = Unscrambling`Private`spinorLeg[Last[parts]], momentumLeg = parts[[2, 1]]},
   Which[parts[[2]] === Mom[first] || parts[[-2]] === Mom[last], 0,
    Length[parts] =!= 3, 10,
    internalQ[first] && internalQ[last], If[momentumLeg === 3, 0, 1],
    externalQ[first] && externalQ[last], If[momentumLeg === 1, 6, 7],
    internalQ[momentumLeg], 5, True, 2]]]];
 chainCostTotal[expression_] := Total[Total[chainCost /@ Unscrambling`Private`chainsIn[#]] & /@
  Unscrambling`Private`monomialsOf[expression]];
 massClose[] := walk[Function[Unscrambling`Private`chainMoves[state, masses]],
  Function[#["Name"] === "Mass" && #["Direction"] === "Contract"], 100];
 routeBuilt = Catch[Block[{Unscrambling`Private`$onShellChannels = Automatic,
   Unscrambling`Private`$useMomentumConservation = True},
  (* Separate sums, contract endpoint insertions, then reduce the two-momentum chains. *)
  walk[Function[Unscrambling`Private`momentumSumMoves[state, masses]],
   Function[Lookup[#, "Identity", None] === "Split"], 100];
  massClose[];
  walk[Function[Unscrambling`SchoutenCandidates[state]],
   Function[#["Splits"] === {0, 0} && AllTrue[#["Chains"],
    Length[#] === 3 && !MemberQ[{{1, 2}, {3, 4}},
     Sort[Unscrambling`Private`spinorLeg /@ {First[#], Last[#]}]] &]], 100];
  walk[Function[Unscrambling`Private`interiorChainMoves[state, masses]],
   Function[#["Name"] === "Anticommutation" && With[{chain = chainAt[#]},
    Length[chain] === 4 &&
     chain[[2]] =!= Mom[Unscrambling`Private`spinorLeg[First[chain]]] &&
     chain[[3]] =!= Mom[Unscrambling`Private`spinorLeg[Last[chain]]] &&
     (chain[[3]] === Mom[Unscrambling`Private`spinorLeg[First[chain]]] ||
      chain[[2]] === Mom[Unscrambling`Private`spinorLeg[Last[chain]]])]], 100];
  massClose[];
  walk[Function[Unscrambling`Private`kinematicMoves[state, masses]],
   Function[#["Name"] === "OnShell" && !FreeQ[#["Subtract"], MomProd[1, 2]] &&
    FreeQ[#["Insert"], MomProd[1, 2]]], 100];
  (* Conservation and Schouten move single insertions into the chosen basis. *)
  Do[
   candidates = Select[Join[Unscrambling`SchoutenCandidates[state],
     Select[Unscrambling`Private`chainMoves[state, masses], #["Name"] === "MomentumConservation" &]],
    chainCostTotal[#["Insert"]] < chainCostTotal[#["Subtract"]] &];
   If[candidates === {}, Break[]];
   candidate = First[SortBy[candidates,
    {chainCostTotal[#["Insert"]] - chainCostTotal[#["Subtract"]],
     LeafCount[#["Insert"]] - LeafCount[#["Subtract"]]} &]];
   record[candidate]; massClose[], {100}];
  If[candidates =!= {}, Throw[$Failed]];
  (* Locally neutral Schouten rewrites expose cancellations with distant terms. *)
  Do[
   currentCost = chainCostTotal[state];
   candidates = Select[Unscrambling`SchoutenCandidates[state],
    chainCostTotal[#["Insert"]] <= chainCostTotal[#["Subtract"]] &];
   choices = Select[({#, Unscrambling`Private`applyCandidate[state, #]} &) /@ candidates,
    chainCostTotal[#[[2]]] < currentCost &];
   If[choices === {}, Break[]];
   candidate = First[First[SortBy[choices,
    {chainCostTotal[#[[2]]], LeafCount[#[[2]]]} &]]];
   record[candidate]; massClose[], {100}];
  If[choices =!= {}, Throw[$Failed]];
  True
 ]];
 If[routeBuilt =!= True || Length[states] =!= 303 ||
   Counts[identities] =!= <|"MomentumSum" -> 32, "Mass" -> 172, "Schouten" -> 40,
     "Anticommutation" -> 8, "OnShell" -> 8, "MomentumConservation" -> 42|> ||
   !AllTrue[states, DeleteDuplicates[Cases[#, _PropDen, Infinity]] === {denominator} &] ||
   !Unscrambling`Private`samePoly[state, expected],
  Message[trainingSimplificationRoutes::build]; Return[$Failed]];
 {<|"Name" -> "e, E, W-, W+ / Z exchange", "Masses" -> masses,
   "States" -> states, "Identities" -> identities,
   "Sources" -> {"SM-4-point-test.nb / ampffWWZTested; diagramAmplitudes plus ReduceSpinContractions"},
   "OnShellChannels" -> Automatic, "MomentumConservation" -> True|>}
];

(* ::Subsection::Closed:: *)
(* Charged-Current W and Higgs Channels *)
trainingChargedCurrentWhWRoutes[] := Module[
 {a, s, c, kinds, masses, state, states, expected, record, walk, choices, chainAt,
  brackets, identityMatches, schoutenOne, schoutenTwo, routeBuilt, identities,
  firstMass, secondMass, specification, expectedCounts, routes = {}},
 If[!MemberQ[$Packages, "Unscrambling`"], Message[trainingSimplificationRoutes::build]; Return[$Failed]];
 a[i_, j_] := SpinorChain[Spinor[kinds[i], "Angle", i], Spinor[kinds[j], "Angle", j]];
 s[i_, j_] := SpinorChain[Spinor[kinds[i], "Square", i], Spinor[kinds[j], "Square", j]];
 c[i_, j_] := SpinorChain[Spinor[kinds[i], "Angle", i], Mom[Multiparticle[1, 2]],
   Spinor[kinds[j], "Square", j]];
 record[candidate_] := (state = Unscrambling`Private`applyCandidate[state, candidate];
  AppendTo[states, state]; AppendTo[identities, candidate["Name"]]);
 walk[generate_, select_, limit_] := (
  Do[choices = Select[generate[], select]; If[choices === {}, Break[]]; record[First[choices]], {limit}];
  If[choices =!= {} && AnyTrue[generate[], select], Throw[$Failed]]);
 chainAt[candidate_] := With[{site = candidate["Site"]},
  DeleteDuplicates[Select[Unscrambling`Private`chainsIn[
   Unscrambling`Private`monomialsOf[state][[site[[1]]]]],
   Unscrambling`Private`validChainQ]][[site[[2]]]]];
 identityMatches[candidate_, block_] := AnyTrue[block, Function[identity,
  With[{scale = Together[candidate["Subtract"]/identity[[1]]]},
   FreeQ[scale, _SpinorChain] && Unscrambling`Private`samePoly[
    candidate["Insert"], scale identity[[2]]]]]];
 Do[
 firstMass = specification[[2]]; secondMass = specification[[3]];
 kinds = <|1 -> If[firstMass === 0, "Helicity", "Spin"], 2 -> "Spin", 3 -> "Spin", 4 -> "Spin"|>;
 masses = {Mass[1] -> firstMass, Mass[2] -> secondMass, Mass[3] -> MW, Mass[4] -> Mh,
  Mass[Multiparticle[1, 2]] -> MW, Mass[Multiparticle[3, 4]] -> MW};
 state = Unscrambling`Private`canonicalChains[-EE^2/(2 MW^2 SW^2 PropDen[Mom[Multiparticle[1, 2]], MW])*
  (MW^2 a[1, 3] s[2, 3] + c[1, 3] c[3, 2])];
 expected = -EE^2/(2 MW^2 SW^2 PropDen[Mom[Multiparticle[1, 2]], MW])*
  (2 MW^2 a[1, 3] s[2, 3] + (secondMass a[1, 2] - firstMass s[1, 2])*
   SpinorChain[Spinor["Spin", "Angle", 3], Mom[4], Spinor["Spin", "Square", 3]]);
 states = {state}; identities = {};
 brackets[text_] := Unscrambling`Private`canonicalChains[ConvertStringAmplitude[text, {firstMass, secondMass, MW, Mh}]];
 schoutenOne = {
  {brackets["<12>[313>"], brackets["<32>[311>"] - brackets["<31>[312>"]},
  {brackets["<12>[323>"], brackets["<32>[321>"] - brackets["<31>[322>"]}};
 schoutenTwo = {
  {brackets["[13][243>"], -brackets["<343][21]"] + brackets["<341][23]"]},
  {brackets["<23>[341>"], brackets["<13>[342>"] + brackets["<21>[343>"]}};
 routeBuilt = Catch[Block[{Unscrambling`Private`$onShellChannels = Automatic,
    Unscrambling`Private`$useMomentumConservation = True},
  walk[Function[Unscrambling`Private`momentumSumMoves[state, masses]],
   Function[Lookup[#, "Identity", None] === "Split"], 100];
  walk[Function[Unscrambling`Private`chainMoves[state, masses]],
   Function[#["Name"] === "Mass" && #["Direction"] === "Contract"], 100];
  walk[Function[Unscrambling`SchoutenCandidates[state]],
   Function[#["Splits"] === {1, 1} && AllTrue[#["Chains"],
    Length[#] === 3 && !MemberQ[{{1, 2}, {3, 3}},
     Sort[Unscrambling`Private`spinorLeg /@ {First[#], Last[#]}]] &]], 100];
  walk[Function[Unscrambling`Private`interiorChainMoves[state, masses]],
   Function[#["Name"] === "Anticommutation" && With[{chain = chainAt[#]},
    Length[chain] === 4 &&
     chain[[2]] =!= Mom[Unscrambling`Private`spinorLeg[First[chain]]] &&
     chain[[3]] =!= Mom[Unscrambling`Private`spinorLeg[Last[chain]]] &&
     (chain[[3]] === Mom[Unscrambling`Private`spinorLeg[First[chain]]] ||
      chain[[2]] === Mom[Unscrambling`Private`spinorLeg[Last[chain]]])]], 100];
  walk[Function[Unscrambling`Private`chainMoves[state, masses]],
   Function[#["Name"] === "Mass" && #["Direction"] === "Contract"], 100];
  walk[Function[Unscrambling`Private`kinematicMoves[state, masses]],
   Function[#["Name"] === "OnShell" && !FreeQ[#["Subtract"], MomProd[1, 2]] &&
    FreeQ[#["Insert"], MomProd[1, 2]]], 100];
  walk[Function[Unscrambling`SchoutenCandidates[state]],
   Function[identityMatches[#, schoutenOne]], 100];
  walk[Function[Unscrambling`Private`chainMoves[state, masses]],
   Function[#["Name"] === "MomentumConservation" &&
    MemberQ[brackets /@ {"[123>", "[213>", "[312>", "[321>"}, chainAt[#]]], 100];
  walk[Function[Unscrambling`Private`chainMoves[state, masses]],
   Function[#["Name"] === "Mass" && #["Direction"] === "Contract"], 100];
  walk[Function[Unscrambling`SchoutenCandidates[state]],
   Function[identityMatches[#, schoutenTwo]], 100];
  walk[Function[Unscrambling`Private`chainMoves[state, masses]],
   Function[#["Name"] === "Mass" && #["Direction"] === "Contract"], 100];
  True
 ]];
 expectedCounts = If[firstMass === 0,
  <|"MomentumSum" -> 3, "Mass" -> 8, "Schouten" -> 3, "Anticommutation" -> 1,
    "OnShell" -> 1, "MomentumConservation" -> 2|>,
  <|"MomentumSum" -> 3, "Mass" -> 13, "Schouten" -> 4, "Anticommutation" -> 1,
    "OnShell" -> 1, "MomentumConservation" -> 4|>];
 If[routeBuilt =!= True || Length[states] =!= Total[Values[expectedCounts]] + 1 ||
   Counts[identities] =!= expectedCounts || !Unscrambling`Private`samePoly[state, expected] ||
   !AllTrue[states, DeleteDuplicates[Cases[#, _PropDen, Infinity]] ===
     {PropDen[Mom[Multiparticle[1, 2]], MW]} &],
  Message[trainingSimplificationRoutes::build]; Return[$Failed]];
 AppendTo[routes, <|"Name" -> specification[[1]], "Masses" -> masses, "States" -> states,
   "Sources" -> {specification[[4]]}, "Identities" -> identities,
   "OnShellChannels" -> Automatic, "MomentumConservation" -> True|>],
 {specification, {
   {"ne, E, W-, h / W+ exchange", 0, Me, "SM-4-point-test.nb / ne E -> W- h / W+"},
   {"u, D, W-, h / W+ exchange", Mu, Md, "SM-4-point-test.nb / u D -> W- h / W+"}}}];
 routes
];

(* ::Subsection::Closed:: *)
(* Bosonic Z, Higgs, and W Channel *)
trainingZhWWZRoute[] := Module[
 {a, s, y, channel, masses, momentum, denominator, state, states, edits = {},
  expected, record, walk, choices, chainAt, chainCost, chainCostTotal,
  massClose, candidates, candidate, currentCost, routeBuilt},
 If[!MemberQ[$Packages, "Unscrambling`"], Message[trainingSimplificationRoutes::build]; Return[$Failed]];
 a[i_, j_] := SpinorChain[Spinor["Spin", "Angle", i], Spinor["Spin", "Angle", j]];
 s[i_, j_] := SpinorChain[Spinor["Spin", "Square", i], Spinor["Spin", "Square", j]];
 y[i_, q_, j_] := SpinorChain[Spinor["Spin", "Angle", i], Mom[q], Spinor["Spin", "Square", j]];
 momentum = Multiparticle[1, 2]; denominator = PropDen[Mom[momentum], MZ];
 masses = {Mass[1] -> MZ, Mass[2] -> Mh, Mass[3] -> MW, Mass[4] -> MW,
  Mass[momentum] -> MZ, Mass[Multiparticle[3, 4]] -> MZ};
 channel = MW MZ^2 (a[1, 4] s[1, 3] + a[1, 3] s[1, 4]) (a[3, 4] + s[3, 4]) +
  MZ^2 a[3, 4] (s[1, 4] y[1, momentum, 3] + s[1, 3] y[1, momentum, 4]) +
  MZ^2 s[3, 4] (a[1, 4] y[3, momentum, 1] + a[1, 3] y[4, momentum, 1]) +
  MW (a[3, 4] + s[3, 4]) (y[1, momentum, 4] y[3, momentum, 1] +
   y[1, momentum, 3] y[4, momentum, 1]);
 state = Unscrambling`Private`canonicalAmplitude[
  -EE^2 channel/(2 Sqrt[2] MW^2 MZ^2 SW^2 denominator), masses];
 (* The source notebook accepts this channel with the opposite template sign. *)
 expected = Unscrambling`Private`canonicalChains[-EE^2/(Sqrt[2] MW^2 SW^2 denominator) (
  a[3, 4] s[3, 4] (y[1, 3, 1] - y[1, 4, 1]) +
   2 MW (s[1, 3] a[1, 4] + s[1, 4] a[1, 3]) (s[3, 4] + a[3, 4]))];
 states = {state};
 record[edit_] := (state = Unscrambling`Private`applyCandidate[state, edit];
  AppendTo[states, state]; AppendTo[edits, edit]);
 walk[generate_, select_, limit_] := (
  Do[choices = Select[generate[], select]; If[choices === {}, Break[]]; record[First[choices]], {limit}];
  If[choices =!= {} && AnyTrue[generate[], select], Throw[$Failed]]);
 chainAt[edit_] := With[{site = edit["Site"]},
  DeleteDuplicates[Select[Unscrambling`Private`chainsIn[
   Unscrambling`Private`monomialsOf[state][[site[[1]]]]],
   Unscrambling`Private`validChainQ]][[site[[2]]]]];
 chainCost[chain_] := With[{parts = List @@ chain}, If[Length[parts] < 3, 0,
  With[{first = Unscrambling`Private`spinorLeg[First[parts]],
   last = Unscrambling`Private`spinorLeg[Last[parts]], momentumLeg = parts[[2, 1]]},
   Which[parts[[2]] === Mom[first] || parts[[-2]] === Mom[last], 0,
    Length[parts] =!= 3, 10,
    first === 1 && last === 1, If[MemberQ[{3, 4}, momentumLeg], 0, 1],
    MemberQ[{first, last}, 1], 2, True, 6]]]];
 chainCostTotal[expression_] := Total[Total[chainCost /@ Unscrambling`Private`chainsIn[#]] & /@
  Unscrambling`Private`monomialsOf[expression]];
 massClose[] := walk[Function[Unscrambling`Private`chainMoves[state, masses]],
  Function[#["Name"] === "Mass" && #["Direction"] === "Contract"], 100];
 routeBuilt = Catch[Block[{Unscrambling`Private`$onShellChannels = Automatic,
   Unscrambling`Private`$useMomentumConservation = True},
  walk[Function[Unscrambling`Private`momentumSumMoves[state, masses]],
   Function[Lookup[#, "Identity", None] === "Split"], 100];
  massClose[];
  walk[Function[Unscrambling`Private`chainMoves[state, masses]],
   Function[#["Name"] === "MomentumConservation" && MemberQ[List @@ chainAt[#], Mom[2]]], 100];
  massClose[];
  walk[Function[Unscrambling`SchoutenCandidates[state]],
   Function[#["Splits"] === {0, 0} && AllTrue[#["Chains"], Length[#] === 3 &&
    MemberQ[{{1, 3}, {1, 4}}, Sort[Unscrambling`Private`spinorLeg /@ {First[#], Last[#]}]] &]], 100];
  walk[Function[Unscrambling`Private`interiorChainMoves[state, masses]],
   Function[#["Name"] === "Anticommutation" && With[{chain = chainAt[#]},
    Length[chain] === 4 &&
     chain[[2]] =!= Mom[Unscrambling`Private`spinorLeg[First[chain]]] &&
     chain[[3]] =!= Mom[Unscrambling`Private`spinorLeg[Last[chain]]] &&
     (chain[[3]] === Mom[Unscrambling`Private`spinorLeg[First[chain]]] ||
      chain[[2]] === Mom[Unscrambling`Private`spinorLeg[Last[chain]]])]], 100];
  massClose[];
  walk[Function[Unscrambling`Private`interiorChainMoves[state, masses]],
   Function[#["Name"] === "ChainSquare" && #["Direction"] === "Contract"], 100];
  (* Move the remaining insertions to the external Z's two endpoints. *)
  Do[
   candidates = Select[Join[Unscrambling`SchoutenCandidates[state],
     Select[Unscrambling`Private`chainMoves[state, masses], #["Name"] === "MomentumConservation" &]],
    chainCostTotal[#["Insert"]] < chainCostTotal[#["Subtract"]] &];
   If[candidates === {}, Break[]];
   candidate = First[SortBy[candidates,
    {chainCostTotal[#["Insert"]] - chainCostTotal[#["Subtract"]],
     LeafCount[#["Insert"]] - LeafCount[#["Subtract"]]} &]];
   record[candidate]; massClose[], {100}];
  If[candidates =!= {}, Throw[$Failed]];
  Do[
   currentCost = chainCostTotal[state];
   candidates = Select[Unscrambling`SchoutenCandidates[state],
    chainCostTotal[#["Insert"]] <= chainCostTotal[#["Subtract"]] &];
   choices = Select[({#, Unscrambling`Private`applyCandidate[state, #]} &) /@ candidates,
    chainCostTotal[#[[2]]] < currentCost &];
   If[choices === {}, Break[]];
   candidate = First[First[SortBy[choices, {chainCostTotal[#[[2]]], LeafCount[#[[2]]]} &]]];
   record[candidate]; massClose[], {100}];
  If[choices =!= {}, Throw[$Failed]];
  walk[Function[Unscrambling`Private`kinematicMoves[state, masses]],
   Function[#["Name"] === "OnShell" && !FreeQ[#["Subtract"], MomProd[3, 4]] &&
    FreeQ[#["Insert"], MomProd[3, 4]]], 100];
  True
 ]];
 If[routeBuilt =!= True || Length[states] =!= 173 ||
   Counts[Lookup[edits, "Name"]] =!= <|"MomentumSum" -> 16, "Mass" -> 108,
    "MomentumConservation" -> 28, "Schouten" -> 12, "Anticommutation" -> 4, "OnShell" -> 4|> ||
   !AllTrue[states, DeleteDuplicates[Cases[#, _PropDen, Infinity]] === {denominator} &] ||
   !Unscrambling`Private`samePoly[state, expected],
  Message[trainingSimplificationRoutes::build]; Return[$Failed]];
 {<|"Name" -> "Z, h, W-, W+ / Z exchange", "Masses" -> masses,
   "States" -> states, "Identities" -> Lookup[edits, "Name"],
   "Sources" -> {"SM-4-point-test.nb / ampZhWWSTested; saved PASS (opposite overall sign)",
    "diagramAmplitudes plus ReduceSpinContractions"},
   "TemplateSign" -> -1, "OnShellChannels" -> Automatic, "MomentumConservation" -> True|>}
];

(* ::Subsection::Closed:: *)
(* Charged-Current Four-Fermion Channel *)
trainingFourFermionWRoute[] := Module[
 {a, s, c, masses, denominator, state, states, expected, record, walk, choices,
  chainAt, identityMatches, routeBuilt, expectedCounts, identities = {}},
 If[!MemberQ[$Packages, "Unscrambling`"], Message[trainingSimplificationRoutes::build]; Return[$Failed]];
 a[i_, j_] := SpinorChain[Spinor["Spin", "Angle", i], Spinor["Spin", "Angle", j]];
 s[i_, j_] := SpinorChain[Spinor["Spin", "Square", i], Spinor["Spin", "Square", j]];
 c[i_, p_, j_] := SpinorChain[Spinor["Spin", "Angle", i], Mom[p], Spinor["Spin", "Square", j]];
 denominator = PropDen[Mom[Multiparticle[1, 2]], MW];
 masses = {Mass[1] -> Mu, Mass[2] -> Md, Mass[3] -> Mtp, Mass[4] -> Mb,
  Mass[Multiparticle[1, 2]] -> MW, Mass[Multiparticle[3, 4]] -> MW};
 state = Unscrambling`Private`canonicalChains[-EE^2/(2 MW^2 SW^2 denominator)*
  (MW^2 a[1, 4] s[2, 3] + c[1, Multiparticle[1, 2], 3] c[4, Multiparticle[1, 2], 2])];
 expected = -EE^2/(2 MW^2 SW^2 denominator)*
  (2 MW^2 a[1, 4] s[2, 3] + Md Mtp a[1, 2] a[3, 4] - Mu Mtp s[1, 2] a[3, 4] -
   Mb Md a[1, 2] s[3, 4] + Mu Mb s[1, 2] s[3, 4]);
 states = {state};
 record[candidate_] := (state = Unscrambling`Private`applyCandidate[state, candidate];
  AppendTo[states, state]; AppendTo[identities, candidate["Name"]]);
 walk[generate_, select_, limit_] := (
  Do[choices = Select[generate[], select]; If[choices === {}, Break[]]; record[First[choices]], {limit}];
  If[choices =!= {} && AnyTrue[generate[], select], Throw[$Failed]]);
 chainAt[candidate_] := With[{site = candidate["Site"]},
  DeleteDuplicates[Select[Unscrambling`Private`chainsIn[
   Unscrambling`Private`monomialsOf[state][[site[[1]]]]], Unscrambling`Private`validChainQ]][[site[[2]]]]];
 identityMatches[candidate_, before_, after_] := With[
  {scale = Together[candidate["Subtract"]/Unscrambling`Private`canonicalChains[before]]},
  FreeQ[scale, _SpinorChain] && Unscrambling`Private`samePoly[candidate["Insert"],
   scale Unscrambling`Private`canonicalChains[after]]];
 routeBuilt = Catch[Block[{Unscrambling`Private`$onShellChannels = Automatic,
   Unscrambling`Private`$useMomentumConservation = True},
  walk[Function[Unscrambling`Private`momentumSumMoves[state, masses]],
   Function[Lookup[#, "Identity", None] === "Split"], 100];
  walk[Function[Unscrambling`Private`chainMoves[state, masses]],
   Function[#["Name"] === "Mass" && #["Direction"] === "Contract"], 100];
  walk[Function[Unscrambling`SchoutenCandidates[state]],
   Function[#["Splits"] === {0, 0} && AllTrue[#["Chains"], Length[#] === 3 &&
    !MemberQ[{{1, 2}, {3, 4}}, Sort[Unscrambling`Private`spinorLeg /@ {First[#], Last[#]}]] &]], 100];
  walk[Function[Unscrambling`Private`interiorChainMoves[state, masses]],
   Function[#["Name"] === "Anticommutation" && With[{chain = chainAt[#]}, Length[chain] === 4 &&
    chain[[2]] =!= Mom[Unscrambling`Private`spinorLeg[First[chain]]] &&
    chain[[3]] =!= Mom[Unscrambling`Private`spinorLeg[Last[chain]]] &&
    (chain[[3]] === Mom[Unscrambling`Private`spinorLeg[First[chain]]] ||
     chain[[2]] === Mom[Unscrambling`Private`spinorLeg[Last[chain]]])]], 100];
  walk[Function[Unscrambling`Private`chainMoves[state, masses]],
   Function[#["Name"] === "Mass" && #["Direction"] === "Contract"], 100];
  walk[Function[Unscrambling`Private`kinematicMoves[state, masses]],
   Function[#["Name"] === "OnShell" && !FreeQ[#["Subtract"], MomProd[1, 2]] &&
    FreeQ[#["Insert"], MomProd[1, 2]]], 100];
  walk[Function[Unscrambling`SchoutenCandidates[state]],
   Function[identityMatches[#, a[2, 4] c[1, 2, 3], a[1, 4] c[2, 2, 3] - a[1, 2] c[4, 2, 3]]], 100];
  walk[Function[Unscrambling`SchoutenCandidates[state]],
   Function[identityMatches[#, s[1, 3] c[4, 1, 2], s[1, 2] c[4, 1, 3] + s[2, 3] c[4, 1, 1]]], 100];
  walk[Function[Unscrambling`Private`chainMoves[state, masses]],
   Function[#["Name"] === "Mass" && #["Direction"] === "Contract"], 100];
  walk[Function[Unscrambling`Private`chainMoves[state, masses]],
   Function[#["Name"] === "MomentumConservation" && With[{chain = chainAt[#]}, Length[chain] === 3 &&
    chain[[2]] === Mom[2] && Sort[Unscrambling`Private`spinorLeg /@ {First[chain], Last[chain]}] === {3, 4}]], 100];
  walk[Function[Unscrambling`Private`chainMoves[state, masses]],
   Function[#["Name"] === "Mass" && #["Direction"] === "Contract"], 100];
  walk[Function[Unscrambling`SchoutenCandidates[state]],
   Function[identityMatches[#, a[1, 4] c[2, 1, 3], a[1, 2] c[4, 1, 3] + a[2, 4] c[1, 1, 3]]], 100];
  walk[Function[Unscrambling`Private`chainMoves[state, masses]],
   Function[#["Name"] === "Mass" && #["Direction"] === "Contract"], 100]; True]];
 expectedCounts = <|"MomentumSum" -> 3, "Mass" -> 13, "Schouten" -> 4,
  "Anticommutation" -> 1, "OnShell" -> 1, "MomentumConservation" -> 2|>;
 If[routeBuilt =!= True || Length[states] =!= 25 || Counts[identities] =!= expectedCounts ||
   !FreeQ[Cases[state, _SpinorChain, Infinity], _Mom] ||
   !AllTrue[states, DeleteDuplicates[Cases[#, _PropDen, Infinity]] === {denominator} &] ||
   !Unscrambling`Private`samePoly[state, expected],
  Message[trainingSimplificationRoutes::build]; Return[$Failed]];
 {<|"Name" -> "u, D, T, b / W+ exchange", "Masses" -> masses, "States" -> states,
  "Sources" -> {"SM-4-point-test.nb / u D -> T b / W+"}, "Identities" -> identities,
  "OnShellChannels" -> Automatic, "MomentumConservation" -> True|>}
];

(* ::Subsection::Closed:: *)
(* Charged-Current Z and W Channels *)
trainingChargedCurrentZWWRoutes[] := Module[
 {a, s, y, c, kinds, firstMass, secondMass, specification, masses, denominator,
  state, states, identities, expected, initial, record, walk, choices, chainAt,
  chainCost, chainCostTotal, massClose, candidates, candidate, currentCost,
  distance, scored, chosen, expectedCounts, built, routes = {}},
 If[!MemberQ[$Packages, "Unscrambling`"], Message[trainingSimplificationRoutes::build]; Return[$Failed]];
 a[i_, j_] := SpinorChain[Spinor[kinds[i], "Angle", i], Spinor[kinds[j], "Angle", j]];
 s[i_, j_] := SpinorChain[Spinor[kinds[i], "Square", i], Spinor[kinds[j], "Square", j]];
 y[i_, q_, j_] := SpinorChain[Spinor[kinds[i], "Angle", i], Mom[q], Spinor[kinds[j], "Square", j]];
 c[i_, j_] := y[i, Multiparticle[1, 2], j];
 record[edit_] := (state = Unscrambling`Private`applyCandidate[state, edit];
  AppendTo[states, state]; AppendTo[identities, edit["Name"]]);
 walk[generate_, select_, limit_] := (
  Do[choices = Select[generate[], select]; If[choices === {}, Break[]]; record[First[choices]], {limit}];
  If[choices =!= {} && AnyTrue[generate[], select], Throw[$Failed]]);
 chainAt[edit_] := With[{site = edit["Site"]}, DeleteDuplicates[Select[
  Unscrambling`Private`chainsIn[Unscrambling`Private`monomialsOf[state][[site[[1]]]]],
  Unscrambling`Private`validChainQ]][[site[[2]]]]];
 chainCost[chain_] := With[{parts = List @@ chain}, If[Length[parts] < 3, 0,
  With[{first = Unscrambling`Private`spinorLeg[First[parts]],
    last = Unscrambling`Private`spinorLeg[Last[parts]], momentumLeg = parts[[2, 1]]},
   Which[parts[[2]] === Mom[first] || parts[[-2]] === Mom[last], 0,
    Length[parts] =!= 3, 10, MemberQ[{1, 2}, first] && MemberQ[{1, 2}, last], If[momentumLeg === 3, 0, 1],
    MemberQ[{3, 4}, first] && MemberQ[{3, 4}, last], If[momentumLeg === 1, 6, 7],
    MemberQ[{1, 2}, momentumLeg], 5, True, 2]]]];
 chainCostTotal[expression_] := Total[Total[chainCost /@ Unscrambling`Private`chainsIn[#]] & /@
  Unscrambling`Private`monomialsOf[expression]];
 massClose[] := walk[Function[Unscrambling`Private`chainMoves[state, masses]],
  Function[#["Name"] === "Mass" && #["Direction"] === "Contract"], 100];
 distance[expression_] := If[Unscrambling`Private`samePoly[expression, expected], 0,
  Length[Unscrambling`Private`monomialsOf[Expand[Numerator[Together[
   Unscrambling`Private`canonicalChains[expression - expected]]]]]]];
 Do[
  firstMass = specification[[2]]; secondMass = specification[[3]];
  kinds = <|1 -> "Spin", 2 -> If[secondMass === 0, "Helicity", "Spin"], 3 -> "Spin", 4 -> "Spin"|>;
  denominator = PropDen[Mom[Multiparticle[1, 2]], MW];
  masses = {Mass[1] -> firstMass, Mass[2] -> secondMass, Mass[3] -> MZ, Mass[4] -> MW,
   Mass[Multiparticle[1, 2]] -> MW, Mass[Multiparticle[3, 4]] -> MW};
  initial = specification[[4]] EE^2/(2 Sqrt[2] MW^2 MZ^2 SW^2 denominator)*(
   MW^2 (a[2, 4] s[1, 3] (MW a[3, 4] + MZ s[3, 4]) +
    a[2, 3] s[1, 4] (MZ a[3, 4] + MW s[3, 4])) +
   MW^2 a[3, 4] (s[1, 4] c[2, 3] + s[1, 3] c[2, 4]) +
   MW^2 s[3, 4] (a[2, 4] c[3, 1] + a[2, 3] c[4, 1]) +
   (MZ a[3, 4] + MW s[3, 4]) c[2, 4] c[3, 1] +
   (MW a[3, 4] + MZ s[3, 4]) c[2, 3] c[4, 1]);
  state = Unscrambling`Private`canonicalAmplitude[initial, masses]; states = {state}; identities = {};
  expected = Unscrambling`Private`canonicalChains[specification[[4]] EE^2/
   (Sqrt[2] MW^2 MZ^2 SW^2 denominator)*(
    2 MW^2 a[2, 3] s[1, 4] (MZ a[3, 4] + MW s[3, 4]) +
    2 MW^2 a[2, 4] s[1, 3] (MW a[3, 4] + MZ s[3, 4]) +
    a[3, 4] s[3, 4] ((2 MW^2 - MZ^2) (firstMass a[1, 2] - secondMass s[1, 2]) +
      2 MW^2 y[2, 3, 1]))];
  built = Catch[Block[{Unscrambling`Private`$onShellChannels = Automatic,
    Unscrambling`Private`$useMomentumConservation = True},
   walk[Function[Unscrambling`Private`momentumSumMoves[state, masses]],
    Function[Lookup[#, "Identity", None] === "Split"], 100]; massClose[];
   walk[Function[Unscrambling`SchoutenCandidates[state]],
    Function[#["Splits"] === {0, 0} && AllTrue[#["Chains"], Length[#] === 3 &&
      !MemberQ[{{1, 2}, {3, 4}}, Sort[Unscrambling`Private`spinorLeg /@ {First[#], Last[#]}]] &]], 100];
   walk[Function[Unscrambling`Private`interiorChainMoves[state, masses]],
    Function[#["Name"] === "Anticommutation" && With[{chain = chainAt[#]}, Length[chain] === 4 &&
     chain[[2]] =!= Mom[Unscrambling`Private`spinorLeg[First[chain]]] &&
     chain[[3]] =!= Mom[Unscrambling`Private`spinorLeg[Last[chain]]] &&
     (chain[[3]] === Mom[Unscrambling`Private`spinorLeg[First[chain]]] ||
      chain[[2]] === Mom[Unscrambling`Private`spinorLeg[Last[chain]]])]], 100]; massClose[];
   walk[Function[Unscrambling`Private`kinematicMoves[state, masses]],
    Function[#["Name"] === "OnShell" && !FreeQ[#["Subtract"], MomProd[1, 2]] &&
     FreeQ[#["Insert"], MomProd[1, 2]]], 100];
   Do[
    candidates = Select[Join[Unscrambling`SchoutenCandidates[state],
      Select[Unscrambling`Private`chainMoves[state, masses], #["Name"] === "MomentumConservation" &]],
     chainCostTotal[#["Insert"]] < chainCostTotal[#["Subtract"]] &];
    If[candidates === {}, Break[]];
    candidate = First[SortBy[candidates, {chainCostTotal[#["Insert"]] - chainCostTotal[#["Subtract"]],
      LeafCount[#["Insert"]] - LeafCount[#["Subtract"]]} &]];
    record[candidate]; massClose[], {100}];
   If[candidates =!= {}, Throw[$Failed]];
   Do[
    currentCost = chainCostTotal[state];
    candidates = Select[Unscrambling`SchoutenCandidates[state],
     chainCostTotal[#["Insert"]] <= chainCostTotal[#["Subtract"]] &];
    choices = Select[({#, Unscrambling`Private`applyCandidate[state, #]} &) /@ candidates,
     chainCostTotal[#[[2]]] < currentCost &];
    If[choices === {}, Break[]]; candidate = First[First[SortBy[choices,
      {chainCostTotal[#[[2]]], LeafCount[#[[2]]]} &]]]; record[candidate]; massClose[], {100}];
   If[choices =!= {}, Throw[$Failed]];
   (* Ordinary bracket Schouten identities collect the remaining external mass terms. *)
   Do[
    candidates = Select[Unscrambling`SchoutenCandidates[state], AllTrue[#["Chains"], Length[#] === 2 &] &];
    scored = Select[({#, Unscrambling`Private`applyCandidate[state, #]} &) /@ candidates,
     distance[#[[2]]] < distance[state] &];
    If[scored === {}, Break[]]; chosen = First[SortBy[scored, distance[#[[2]]] &]];
    record[First[chosen]], {12}];
   True
  ]];
  expectedCounts = If[secondMass === 0,
   <|"MomentumSum" -> 16, "Mass" -> 58, "Schouten" -> 14, "Anticommutation" -> 4,
     "OnShell" -> 4, "MomentumConservation" -> 13|>,
   <|"MomentumSum" -> 16, "Mass" -> 86, "Schouten" -> 24, "Anticommutation" -> 4,
     "OnShell" -> 4, "MomentumConservation" -> 21|>];
  If[built =!= True || Length[states] =!= Total[Values[expectedCounts]] + 1 ||
    Counts[identities] =!= expectedCounts || !Unscrambling`Private`samePoly[state, expected] ||
    !AllTrue[states, DeleteDuplicates[Cases[#, _PropDen, Infinity]] === {denominator} &],
   Message[trainingSimplificationRoutes::build]; Return[$Failed]];
  AppendTo[routes, <|"Name" -> specification[[1]], "Masses" -> masses, "States" -> states,
    "Sources" -> {specification[[5]]}, "Identities" -> identities,
    "OnShellChannels" -> Automatic, "MomentumConservation" -> True|>],
 {specification, {
  {"E, ne, Z, W- / W+ exchange", Me, 0, -1, "SM-4-point-test.nb / ampffZWWSlepTested"},
  {"U, d, Z, W+ / W+ exchange", Mu, Md, 1, "SM-4-point-test.nb / ampffZWWSudTested"}}}];
 routes
];

(* ::Subsection::Closed:: *)
(* Bosonic Z, Higgs, and W Exchange Channels *)
trainingZhWWWRoutes[] := Module[
 {a, s, y, momentum, denominator, masses, channel, state, states, edits = {},
  expected, record, walk, choices, chainAt, massClose, chainCost, chainCostTotal,
  candidates, candidate, routeBuilt, route, permutation},
 If[!MemberQ[$Packages, "Unscrambling`"], Message[trainingSimplificationRoutes::build]; Return[$Failed]];
 a[i_, j_] := SpinorChain[Spinor["Spin", "Angle", i], Spinor["Spin", "Angle", j]];
 s[i_, j_] := SpinorChain[Spinor["Spin", "Square", i], Spinor["Spin", "Square", j]];
 y[i_, q_, j_] := SpinorChain[Spinor["Spin", "Angle", i], Mom[q], Spinor["Spin", "Square", j]];
 momentum = Multiparticle[1, 3]; denominator = PropDen[Mom[momentum], MW];
 masses = {Mass[1] -> MZ, Mass[2] -> Mh, Mass[3] -> MW, Mass[4] -> MW,
  Mass[momentum] -> MW, Mass[Multiparticle[2, 4]] -> MW};
 channel = MW^3 (a[1, 3] a[3, 4] s[1, 4] + a[1, 4] s[1, 3] s[3, 4]) +
  MW^2 MZ (a[3, 4] s[1, 3] s[1, 4] + a[1, 3] a[1, 4] s[3, 4]) +
  MW^2 (a[3, 4] s[1, 3] y[1, momentum, 4] +
   a[1, 4] s[1, 3] y[3, momentum, 4] + a[1, 3] s[3, 4] y[4, momentum, 1] +
   a[1, 3] s[1, 4] y[4, momentum, 3]) +
  MW (a[1, 3] y[3, momentum, 4] y[4, momentum, 1] +
   s[1, 3] y[1, momentum, 4] y[4, momentum, 3]) +
  MZ (s[1, 3] y[3, momentum, 4] y[4, momentum, 1] +
   a[1, 3] y[1, momentum, 4] y[4, momentum, 3]);
 state = Unscrambling`Private`canonicalAmplitude[-EE^2 channel/
  (2 Sqrt[2] MW^2 MZ^2 SW^2 denominator), masses];
 expected = Unscrambling`Private`canonicalChains[-Sqrt[2] EE^2/(MZ^2 SW^2 denominator) (
   MW a[1, 3] a[3, 4] s[1, 4] + MZ a[3, 4] s[1, 3] s[1, 4] +
   MZ a[1, 3] a[1, 4] s[3, 4] + MW a[1, 4] s[1, 3] s[3, 4]) -
  EE^2 a[1, 3] s[1, 3] (y[4, 1, 4] + y[4, 3, 4])/(Sqrt[2] MW^2 SW^2 denominator) +
  Sqrt[2] EE^2 a[1, 3] s[1, 3] y[4, 3, 4]/(MZ^2 SW^2 denominator)];
 states = {state};
 record[edit_] := (state = Unscrambling`Private`applyCandidate[state, edit];
  AppendTo[states, state]; AppendTo[edits, edit]);
 walk[generate_, select_, limit_] := (
  Do[choices = Select[generate[], select]; If[choices === {}, Break[]]; record[First[choices]], {limit}];
  If[choices =!= {} && AnyTrue[generate[], select], Throw[$Failed]]);
 chainAt[edit_] := With[{site = edit["Site"]},
  DeleteDuplicates[Select[Unscrambling`Private`chainsIn[
   Unscrambling`Private`monomialsOf[state][[site[[1]]]]],
   Unscrambling`Private`validChainQ]][[site[[2]]]]];
 massClose[] := walk[Function[Unscrambling`Private`chainMoves[state, masses]],
  Function[#["Name"] === "Mass" && #["Direction"] === "Contract"], 100];
 chainCost[chain_] := With[{parts = List @@ chain}, If[Length[parts] < 3, 0,
  With[{first = parts[[1, 3]], last = parts[[-1, 3]]},
   Which[parts[[2]] === Mom[first] || parts[[-2]] === Mom[last], 0,
    Length[parts] =!= 3, 10,
    first === 4 && last === 4 && MemberQ[{Mom[1], Mom[3]}, parts[[2]]], 0,
    True, 2]]]];
 chainCostTotal[expr_] := Total[Total[chainCost /@ Unscrambling`Private`chainsIn[#]] & /@
  Unscrambling`Private`monomialsOf[expr]];
 routeBuilt = Catch[Block[{Unscrambling`Private`$onShellChannels = Automatic,
   Unscrambling`Private`$useMomentumConservation = True},
  walk[Function[Unscrambling`Private`momentumSumMoves[state, masses]],
   Function[Lookup[#, "Identity", None] === "Split"], 100];
  massClose[];
  (* Join the two noncontractible insertions before moving an endpoint momentum. *)
  walk[Function[Unscrambling`SchoutenCandidates[state]],
   Function[#["Splits"] === {0, 0} && AllTrue[#["Chains"], Length[#] === 3 &&
     #[[2]] =!= Mom[#[[1, 3]]] && #[[2]] =!= Mom[#[[-1, 3]]] &] &&
    Count[Cases[#["Insert"], _SpinorChain, Infinity], c_ /; Length[c] === 4] >
     Count[Cases[#["Subtract"], _SpinorChain, Infinity], c_ /; Length[c] === 4]], 100];
  walk[Function[Unscrambling`Private`interiorChainMoves[state, masses]],
   Function[#["Name"] === "Anticommutation" && With[{chain = chainAt[#]},
    Length[chain] === 4 && chain[[2]] =!= Mom[chain[[1, 3]]] &&
     chain[[3]] =!= Mom[chain[[-1, 3]]] &&
     (chain[[3]] === Mom[chain[[1, 3]]] || chain[[2]] === Mom[chain[[-1, 3]]])]], 100];
  massClose[];
  walk[Function[Unscrambling`Private`kinematicMoves[state, masses]],
   Function[#["Name"] === "OnShell" && !FreeQ[#["Subtract"], MomProd[1, 3]] &&
    FreeQ[#["Insert"], MomProd[1, 3]]], 100];
  (* The final basis leaves the spectator W's two allowed single insertions. *)
  Do[
   candidates = Select[Unscrambling`SchoutenCandidates[state],
    chainCostTotal[#["Insert"]] < chainCostTotal[#["Subtract"]] &];
   If[candidates === {}, Break[]];
   candidate = First[SortBy[candidates,
    {chainCostTotal[#["Insert"]] - chainCostTotal[#["Subtract"]],
     LeafCount[#["Insert"]] - LeafCount[#["Subtract"]]} &]];
   record[candidate]; massClose[], {100}];
  If[candidates =!= {}, Throw[$Failed]];
  True
 ]];
 If[routeBuilt =!= True || Length[states] =!= 73 ||
   Counts[Lookup[edits, "Name"]] =!= <|"MomentumSum" -> 16, "Mass" -> 36,
    "Schouten" -> 12, "Anticommutation" -> 4, "OnShell" -> 4|> ||
   !AllTrue[states, DeleteDuplicates[Cases[#, _PropDen, Infinity]] === {denominator} &] ||
   !Unscrambling`Private`samePoly[state, expected],
  Message[trainingSimplificationRoutes::build]; Return[$Failed]];
 route = <|"Name" -> "Z, h, W-, W+ / T-channel W exchange", "Masses" -> masses,
  "States" -> states, "Identities" -> Lookup[edits, "Name"],
  "Sources" -> {"SM-4-point-test.nb / zhWWFinalize[ampZhWWTTested[], T]; saved PASS (opposite overall sign)",
   "diagramAmplitudes plus ReduceSpinContractions"},
  "TemplateSign" -> -1, "OnShellChannels" -> Automatic, "MomentumConservation" -> True|>;
 permutation = AssociationThread[Range[4], {1, 2, 4, 3}];
 {route, Join[route, <|"Name" -> "Z, h, W-, W+ / U-channel W exchange",
   "Masses" -> Join[Take[masses, 4], Unscrambling`Private`relabelLegs[Drop[masses, 4], permutation]],
   "States" -> (-Unscrambling`Private`canonicalChains[Unscrambling`Private`relabelLegs[#, permutation]] & /@ states),
   "Sources" -> {"SM-4-point-test.nb / zhWWFinalize[ampZhWWUTested[], U]; saved PASS (opposite overall sign)",
    "diagramAmplitudes plus ReduceSpinContractions; crossed T-channel legal identities"}|>]}
];

trainingSimplificationRoutes := Module[{routes},
  routes = {trainingFourFermionZRoute[], trainingElectronZHZRoute[],
   trainingElectronZHElectronRoutes[], trainingElectronZZElectronRoutes[], trainingElectronWWZRoute[],
   trainingChargedCurrentWhWRoutes[], trainingZhWWZRoute[], trainingFourFermionWRoute[],
   trainingChargedCurrentZWWRoutes[], trainingZhWWWRoutes[]};
  If[MemberQ[routes, $Failed], $Failed, Join @@ routes]
];

(* ::Section::Closed:: *)
(* Reference Catalog *)

Join[{
(* ::Subsection::Closed:: *)
(* Reference 1 *)
<|"Name" -> "SM-4-point-test.nb / u U -> C c / A", 
 "Sources" -> {"SM-4-point-test.nb / u U -> C c / A", "SM-4-point.nb / input 34 / target 1", 
   "SM-4-point.nb / input 38 / target 1"}, "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Mc, 
   Mass[4] -> Mc}, "Amplitude" -> 
  (-8*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/(9*Mandelstahm[1, 2])|>,

(* ::Subsection::Closed:: *)
(* Reference 2 *)
<|"Name" -> "SM-4-point-test.nb / u U -> C c / h", 
 "Sources" -> {"SM-4-point-test.nb / u U -> C c / h", "SM-4-point.nb / input 45 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Mc, Mass[4] -> Mc}, 
 "Amplitude" -> (EE^2*Mc*Mu*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
    (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
   (4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 3 *)
<|"Name" -> "SM-4-point-test.nb / u U -> C c / Z", 
 "Sources" -> {"SM-4-point-test.nb / u U -> C c / Z", "SM-4-point.nb / input 48 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Mc, Mass[4] -> Mc}, 
 "Amplitude" -> -1/2*(EE^2*(gLu*gRu*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
       gRu^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
       gLu^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
       gLu*gRu*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
     (CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) - 
   (EE^2*(gLu - gRu)^2*Mc*Mu*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 4 *)
<|"Name" -> "SM-4-point-test.nb / u U -> U u / A", "Sources" -> {"SM-4-point-test.nb / u U -> U u / A"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Mu, Mass[4] -> Mu}, 
 "Amplitude" -> (-8*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/(9*Mandelstahm[1, 2]) + 
   (8*EE^2*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] - 
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] - 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/(9*Mandelstahm[1, 3])|>,

(* ::Subsection::Closed:: *)
(* Reference 5 *)
<|"Name" -> "SM-4-point-test.nb / u U -> U u / G", "Sources" -> {"SM-4-point-test.nb / u U -> U u / G"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Mu, Mass[4] -> Mu}, 
 "Amplitude" -> (-2*GG^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/Mandelstahm[1, 2] + 
   (2*GG^2*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] - 
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] - 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/Mandelstahm[1, 3]|>,

(* ::Subsection::Closed:: *)
(* Reference 6 *)
<|"Name" -> "SM-4-point-test.nb / u U -> U u / h", "Sources" -> {"SM-4-point-test.nb / u U -> U u / h"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Mu, Mass[4] -> Mu}, 
 "Amplitude" -> -1/4*(EE^2*Mu^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]] + 
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])*
      (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]] + 
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
     (MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 3])) + 
   (EE^2*Mu^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 7 *)
<|"Name" -> "SM-4-point-test.nb / u U -> U u / Z", "Sources" -> {"SM-4-point-test.nb / u U -> U u / Z"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Mu, Mass[4] -> Mu}, 
 "Amplitude" -> (EE^2*(gLu - gRu)^2*Mu^2*(SpinorChain[Spinor["Spin", "Angle", 1], 
       Spinor["Spin", "Angle", 3]] - SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])*
     (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 3])) - 
   (EE^2*(gLu*gRu*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
      gRu^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
      gLu^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      gLu*gRu*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
    (2*CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) - 
   (EE^2*(gLu - gRu)^2*Mu^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) + 
   (EE^2*(-(gRu^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]) - 
      gLu^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      gLu*gRu*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
         SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] + 
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
         SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])))/
    (2*CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 8 *)
<|"Name" -> "SM-4-point-test.nb / u U -> S s / A", 
 "Sources" -> {"SM-4-point-test.nb / u U -> S s / A", "SM-4-point.nb / input 88 / target 1", 
   "SM-4-point.nb / input 93 / target 1"}, "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Ms, 
   Mass[4] -> Ms}, "Amplitude" -> 
  (4*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/(9*Mandelstahm[1, 2])|>,

(* ::Subsection::Closed:: *)
(* Reference 9 *)
<|"Name" -> "SM-4-point-test.nb / u U -> S s / h", 
 "Sources" -> {"SM-4-point-test.nb / u U -> S s / h", "SM-4-point.nb / input 96 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Ms, Mass[4] -> Ms}, 
 "Amplitude" -> (EE^2*Ms*Mu*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
    (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
   (4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 10 *)
<|"Name" -> "SM-4-point-test.nb / u U -> S s / Z", 
 "Sources" -> {"SM-4-point-test.nb / u U -> S s / Z", "SM-4-point.nb / input 99 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Ms, Mass[4] -> Ms}, 
 "Amplitude" -> -1/2*(EE^2*(gLd*gRu*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
       gRd*gRu*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
       gLd*gLu*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
       gLu*gRd*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
     (CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) - (EE^2*(gLd - gRd)*(gLu - gRu)*Ms*Mu*
     (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 11 *)
<|"Name" -> "SM-4-point-test.nb / u U -> D d / A", 
 "Sources" -> {"SM-4-point-test.nb / u U -> D d / A", "SM-4-point.nb / input 108 / target 1", 
   "SM-4-point.nb / input 112 / target 1"}, "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Md, 
   Mass[4] -> Md}, "Amplitude" -> 
  (4*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/(9*Mandelstahm[1, 2])|>,

(* ::Subsection::Closed:: *)
(* Reference 12 *)
<|"Name" -> "SM-4-point-test.nb / u U -> D d / h", 
 "Sources" -> {"SM-4-point-test.nb / u U -> D d / h", "SM-4-point.nb / input 115 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Md, Mass[4] -> Md}, 
 "Amplitude" -> (EE^2*Md*Mu*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
    (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
   (4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 13 *)
<|"Name" -> "SM-4-point-test.nb / u U -> D d / Z", 
 "Sources" -> {"SM-4-point-test.nb / u U -> D d / Z", "SM-4-point.nb / input 118 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Md, Mass[4] -> Md}, 
 "Amplitude" -> -1/2*(EE^2*(gLd*gRu*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
       gRd*gRu*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
       gLd*gLu*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
       gLu*gRd*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
     (CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) - (EE^2*(gLd - gRd)*(gLu - gRu)*Md*Mu*
     (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 14 *)
<|"Name" -> "SM-4-point-test.nb / u U -> D d / W+", 
 "Sources" -> {"SM-4-point-test.nb / u U -> D d / W+", "SM-4-point.nb / input 121 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Md, Mass[4] -> Md}, 
 "Amplitude" -> -1/2*(EE^2*(2*MW^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      (Md*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]] - 
        Mu*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])*
       (-(Mu*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]) + 
        Md*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])))/
    (MW^2*SW^2*(-MW^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 15 *)
<|"Name" -> "SM-4-point-test.nb / d D -> S s / A", 
 "Sources" -> {"SM-4-point-test.nb / d D -> S s / A", "SM-4-point.nb / input 132 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Ms, Mass[4] -> Ms}, 
 "Amplitude" -> (-2*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/(9*Mandelstahm[1, 2])|>,

(* ::Subsection::Closed:: *)
(* Reference 16 *)
<|"Name" -> "SM-4-point-test.nb / d D -> S s / h", 
 "Sources" -> {"SM-4-point-test.nb / d D -> S s / h", "SM-4-point.nb / input 139 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Ms, Mass[4] -> Ms}, 
 "Amplitude" -> (EE^2*Md*Ms*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
    (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
   (4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 17 *)
<|"Name" -> "SM-4-point-test.nb / d D -> S s / Z", 
 "Sources" -> {"SM-4-point-test.nb / d D -> S s / Z", "SM-4-point.nb / input 142 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Ms, Mass[4] -> Ms}, 
 "Amplitude" -> -1/2*(EE^2*(gLd*gRd*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
       gRd^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
       gLd^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
       gLd*gRd*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
     (CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) - 
   (EE^2*(gLd - gRd)^2*Md*Ms*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 18 *)
<|"Name" -> "SM-4-point-test.nb / d D -> D d / A", "Sources" -> {"SM-4-point-test.nb / d D -> D d / A"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Md, Mass[4] -> Md}, 
 "Amplitude" -> (-2*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/(9*Mandelstahm[1, 2]) + 
   (2*EE^2*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] - 
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] - 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/(9*Mandelstahm[1, 3])|>,

(* ::Subsection::Closed:: *)
(* Reference 19 *)
<|"Name" -> "SM-4-point-test.nb / d D -> D d / G", "Sources" -> {"SM-4-point-test.nb / d D -> D d / G"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Md, Mass[4] -> Md}, 
 "Amplitude" -> (-2*GG^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/Mandelstahm[1, 2] + 
   (2*GG^2*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] - 
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] - 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/Mandelstahm[1, 3]|>,

(* ::Subsection::Closed:: *)
(* Reference 20 *)
<|"Name" -> "SM-4-point-test.nb / d D -> D d / h", "Sources" -> {"SM-4-point-test.nb / d D -> D d / h"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Md, Mass[4] -> Md}, 
 "Amplitude" -> -1/4*(EE^2*Md^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]] + 
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])*
      (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]] + 
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
     (MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 3])) + 
   (EE^2*Md^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 21 *)
<|"Name" -> "SM-4-point-test.nb / d D -> D d / Z", "Sources" -> {"SM-4-point-test.nb / d D -> D d / Z"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Md, Mass[4] -> Md}, 
 "Amplitude" -> (EE^2*(gLd - gRd)^2*Md^2*(SpinorChain[Spinor["Spin", "Angle", 1], 
       Spinor["Spin", "Angle", 3]] - SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])*
     (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 3])) - 
   (EE^2*(gLd*gRd*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
      gRd^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
      gLd^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      gLd*gRd*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
    (2*CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) - 
   (EE^2*(gLd - gRd)^2*Md^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) + 
   (EE^2*(-(gRd^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]) - 
      gLd^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      gLd*gRd*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
         SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] + 
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
         SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])))/
    (2*CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 22 *)
<|"Name" -> "SM-4-point-test.nb / u U -> E e / A", 
 "Sources" -> {"SM-4-point-test.nb / u U -> E e / A", "SM-4-point.nb / input 179 / target 1", 
   "SM-4-point.nb / input 183 / target 1"}, "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Me, 
   Mass[4] -> Me}, "Amplitude" -> 
  (4*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/(3*Mandelstahm[1, 2])|>,

(* ::Subsection::Closed:: *)
(* Reference 23 *)
<|"Name" -> "SM-4-point-test.nb / u U -> E e / h", 
 "Sources" -> {"SM-4-point-test.nb / u U -> E e / h", "SM-4-point.nb / input 186 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Me, Mass[4] -> Me}, 
 "Amplitude" -> (EE^2*Me*Mu*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
    (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
   (4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 24 *)
<|"Name" -> "SM-4-point-test.nb / u U -> E e / Z", 
 "Sources" -> {"SM-4-point-test.nb / u U -> E e / Z", "SM-4-point.nb / input 189 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Me, Mass[4] -> Me}, 
 "Amplitude" -> -1/2*(EE^2*(gLe*gRu*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
       gRe*gRu*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
       gLe*gLu*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
       gLu*gRe*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
     (CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) - (EE^2*(gLe - gRe)*(gLu - gRu)*Me*Mu*
     (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 25 *)
<|"Name" -> "SM-4-point-test.nb / u U -> Ne ne / Z", 
 "Sources" -> {"SM-4-point-test.nb / u U -> Ne ne / Z", "SM-4-point.nb / input 197 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> 0, Mass[4] -> 0}, 
 "Amplitude" -> -1/2*(EE^2*(gRu*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square", 3]] + 
      gLu*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]]))/
    (CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 26 *)
<|"Name" -> "SM-4-point-test.nb / d D -> E e / A", 
 "Sources" -> {"SM-4-point-test.nb / d D -> E e / A", "SM-4-point.nb / input 204 / target 1", 
   "SM-4-point.nb / input 208 / target 1"}, "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Me, 
   Mass[4] -> Me}, "Amplitude" -> 
  (-2*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/(3*Mandelstahm[1, 2])|>,

(* ::Subsection::Closed:: *)
(* Reference 27 *)
<|"Name" -> "SM-4-point-test.nb / d D -> E e / h", 
 "Sources" -> {"SM-4-point-test.nb / d D -> E e / h", "SM-4-point.nb / input 211 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Me, Mass[4] -> Me}, 
 "Amplitude" -> (EE^2*Md*Me*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
    (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
   (4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 28 *)
<|"Name" -> "SM-4-point-test.nb / d D -> E e / Z", 
 "Sources" -> {"SM-4-point-test.nb / d D -> E e / Z", "SM-4-point.nb / input 214 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Me, Mass[4] -> Me}, 
 "Amplitude" -> -1/2*(EE^2*(gLe*gRd*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
       gRd*gRe*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
       gLd*gLe*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
       gLd*gRe*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
     (CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) - (EE^2*(gLd - gRd)*(gLe - gRe)*Md*Me*
     (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 29 *)
<|"Name" -> "SM-4-point-test.nb / d D -> Ne ne / Z", 
 "Sources" -> {"SM-4-point-test.nb / d D -> Ne ne / Z", "SM-4-point.nb / input 221 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> 0, Mass[4] -> 0}, 
 "Amplitude" -> -1/2*(EE^2*(gRd*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square", 3]] + 
      gLd*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]]))/
    (CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 30 *)
<|"Name" -> "SM-4-point-test.nb / e E -> M m / A", 
 "Sources" -> {"SM-4-point-test.nb / e E -> M m / A", "SM-4-point.nb / input 228 / target 1", 
   "SM-4-point.nb / input 232 / target 1"}, "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> Mm, 
   Mass[4] -> Mm}, "Amplitude" -> 
  (-2*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/Mandelstahm[1, 2]|>,

(* ::Subsection::Closed:: *)
(* Reference 31 *)
<|"Name" -> "SM-4-point-test.nb / e E -> M m / h", 
 "Sources" -> {"SM-4-point-test.nb / e E -> M m / h", "SM-4-point.nb / input 235 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> Mm, Mass[4] -> Mm}, 
 "Amplitude" -> (EE^2*Me*Mm*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
    (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
   (4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 32 *)
<|"Name" -> "SM-4-point-test.nb / e E -> M m / Z", 
 "Sources" -> {"SM-4-point-test.nb / e E -> M m / Z", "SM-4-point.nb / input 238 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> Mm, Mass[4] -> Mm}, 
 "Amplitude" -> -1/2*(EE^2*(gLe*gRe*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
       gRe^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
       gLe^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
       gLe*gRe*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
     (CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) - 
   (EE^2*(gLe - gRe)^2*Me*Mm*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 33 *)
<|"Name" -> "SM-4-point-test.nb / e E -> E e / A", "Sources" -> {"SM-4-point-test.nb / e E -> E e / A"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> Me, Mass[4] -> Me}, 
 "Amplitude" -> (-2*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/Mandelstahm[1, 2] + 
   (2*EE^2*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] - 
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] - 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/Mandelstahm[1, 3]|>,

(* ::Subsection::Closed:: *)
(* Reference 34 *)
<|"Name" -> "SM-4-point-test.nb / e E -> E e / h", "Sources" -> {"SM-4-point-test.nb / e E -> E e / h"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> Me, Mass[4] -> Me}, 
 "Amplitude" -> -1/4*(EE^2*Me^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]] + 
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])*
      (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]] + 
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
     (MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 3])) + 
   (EE^2*Me^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 35 *)
<|"Name" -> "SM-4-point-test.nb / e E -> E e / Z", "Sources" -> {"SM-4-point-test.nb / e E -> E e / Z"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> Me, Mass[4] -> Me}, 
 "Amplitude" -> (EE^2*(gLe - gRe)^2*Me^2*(SpinorChain[Spinor["Spin", "Angle", 1], 
       Spinor["Spin", "Angle", 3]] - SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])*
     (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 3])) - 
   (EE^2*(gLe*gRe*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
      gRe^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
      gLe^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      gLe*gRe*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
    (2*CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) - 
   (EE^2*(gLe - gRe)^2*Me^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) + 
   (EE^2*(-(gRe^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]) - 
      gLe^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      gLe*gRe*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
         SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] + 
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
         SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])))/
    (2*CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 36 *)
<|"Name" -> "SM-4-point-test.nb / e E -> Ne ne / Z", 
 "Sources" -> {"SM-4-point-test.nb / e E -> Ne ne / Z", "SM-4-point.nb / input 267 / target 1", 
   "SM-4-point.nb / input 272 / target 1"}, "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> 0, 
   Mass[4] -> 0}, "Amplitude" -> 
  -1/2*(EE^2*(gRe*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square", 3]] + 
      gLe*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]]))/
    (CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 37 *)
<|"Name" -> "SM-4-point-test.nb / e E -> Ne ne / W+", 
 "Sources" -> {"SM-4-point-test.nb / e E -> Ne ne / W+", "SM-4-point.nb / input 275 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> 0, Mass[4] -> 0}, 
 "Amplitude" -> -1/2*(EE^2*(Me^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square", 3]] + 
      2*MW^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]]))/
    (MW^2*SW^2*(-MW^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 38 *)
<|"Name" -> "SM-4-point-test.nb / ne Ne -> Nm nm / Z", 
 "Sources" -> {"SM-4-point-test.nb / ne Ne -> Nm nm / Z", "SM-4-point.nb / input 280 / target 1", 
   "SM-4-point.nb / input 285 / target 1"}, "Masses" -> {Mass[1] -> 0, Mass[2] -> 0, Mass[3] -> 0, 
   Mass[4] -> 0}, "Amplitude" -> 
  -1/2*(EE^2*SpinorChain[Spinor["Helicity", "Angle", 1], Spinor["Helicity", "Angle", 4]]*
     SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Helicity", "Square", 3]])/
    (CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 39 *)
<|"Name" -> "SM-4-point-test.nb / ne Ne -> Ne ne / Z", 
 "Sources" -> {"SM-4-point-test.nb / ne Ne -> Ne ne / Z"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> 0, Mass[3] -> 0, Mass[4] -> 0}, 
 "Amplitude" -> -1/2*(EE^2*SpinorChain[Spinor["Helicity", "Angle", 1], Spinor["Helicity", "Angle", 4]]*
      SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Helicity", "Square", 3]])/
     (CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) - 
   (EE^2*SpinorChain[Spinor["Helicity", "Angle", 1], Spinor["Helicity", "Angle", 4]]*
     SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Helicity", "Square", 3]])/
    (2*CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 40 *)
<|"Name" -> "SM-4-point-test.nb / u D -> T b / W+", "Sources" -> {"SM-4-point-test.nb / u D -> T b / W+"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Md, Mass[3] -> Mtp, Mass[4] -> Mb}, 
 "Amplitude" -> -1/2*(EE^2*(Md*Mtp*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
       SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      Mtp*Mu*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] + 
      2*MW^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] - 
      Mb*Md*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
      Mb*Mu*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (MW^2*SW^2*(-MW^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 41 *)
<|"Name" -> "SM-4-point-test.nb / e E -> h h / h", 
 "Sources" -> {"SM-4-point-test.nb / e E -> h h / h", "SM-4-point.nb / input 321 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> Mh, Mass[4] -> Mh}, 
 "Amplitude" -> (-3*EE^2*Me*Mh^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]))/
   (4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 42 *)
<|"Name" -> "SM-4-point-test.nb / e E -> h h / e", "Sources" -> {"SM-4-point-test.nb / e E -> h h / e"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> Mh, Mass[4] -> Mh}, 
 "Amplitude" -> 
  -1/4*(EE^2*Me^2*(2*Me*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
         SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]) + 
       SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 2]] + 
       SpinorChain[Spinor["Spin", "Square", 2], Mom[3], Spinor["Spin", "Angle", 1]]))/
     (MW^2*SW^2*(-Me^2 + Mandelstahm[1, 3])) - 
   (EE^2*Me^2*(2*Me*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]) + 
      SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 2]] + 
      SpinorChain[Spinor["Spin", "Square", 2], Mom[4], Spinor["Spin", "Angle", 1]]))/
    (4*MW^2*SW^2*(-Me^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 43 *)
<|"Name" -> "SM-4-point-test.nb / u U -> h h / h", 
 "Sources" -> {"SM-4-point-test.nb / u U -> h h / h", "SM-4-point.nb / input 331 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Mh, Mass[4] -> Mh}, 
 "Amplitude" -> (-3*EE^2*Mh^2*Mu*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]))/
   (4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 44 *)
<|"Name" -> "SM-4-point-test.nb / u U -> h h / u", "Sources" -> {"SM-4-point-test.nb / u U -> h h / u"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Mh, Mass[4] -> Mh}, 
 "Amplitude" -> 
  -1/4*(EE^2*Mu^2*(2*Mu*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
         SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]) + 
       SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 2]] + 
       SpinorChain[Spinor["Spin", "Square", 2], Mom[3], Spinor["Spin", "Angle", 1]]))/
     (MW^2*SW^2*(-Mu^2 + Mandelstahm[1, 3])) - 
   (EE^2*Mu^2*(2*Mu*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]) + 
      SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 2]] + 
      SpinorChain[Spinor["Spin", "Square", 2], Mom[4], Spinor["Spin", "Angle", 1]]))/
    (4*MW^2*SW^2*(-Mu^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 45 *)
<|"Name" -> "SM-4-point-test.nb / d D -> h h / h", 
 "Sources" -> {"SM-4-point-test.nb / d D -> h h / h", "SM-4-point.nb / input 341 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Mh, Mass[4] -> Mh}, 
 "Amplitude" -> (-3*EE^2*Md*Mh^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]))/
   (4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 46 *)
<|"Name" -> "SM-4-point-test.nb / d D -> h h / d", "Sources" -> {"SM-4-point-test.nb / d D -> h h / d"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Mh, Mass[4] -> Mh}, 
 "Amplitude" -> 
  -1/4*(EE^2*Md^2*(2*Md*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
         SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]) + 
       SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 2]] + 
       SpinorChain[Spinor["Spin", "Square", 2], Mom[3], Spinor["Spin", "Angle", 1]]))/
     (MW^2*SW^2*(-Md^2 + Mandelstahm[1, 3])) - 
   (EE^2*Md^2*(2*Md*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]) + 
      SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 2]] + 
      SpinorChain[Spinor["Spin", "Square", 2], Mom[4], Spinor["Spin", "Angle", 1]]))/
    (4*MW^2*SW^2*(-Md^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 47 *)
<|"Name" -> "SM-4-point-test.nb / e E -> Z h / Z", 
 "Sources" -> {"SM-4-point-test.nb / e E -> Z h / Z", "SM-4-point.nb / input 382 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> MZ, Mass[4] -> Mh}, 
 "Amplitude" -> (EE^2*gLe*(2*MZ^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      Me*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
       SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (2*Sqrt[2]*MW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) + 
   (EE^2*gRe*(2*MZ^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
      Me*(-SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
       SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (2*Sqrt[2]*MW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 48 *)
<|"Name" -> "SM-4-point-test.nb / e E -> Z h / e:T", 
 "Sources" -> {"SM-4-point-test.nb / e E -> Z h / e:T", "SM-4-point.nb / input 386 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> MZ, Mass[4] -> Mh}, 
 "Amplitude" -> (EE^2*gRe*Me*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     (2*Me*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]] + 
      SpinorChain[Spinor["Spin", "Square", 2], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (2*Sqrt[2]*MW^2*SW^2*(-Me^2 + Mandelstahm[1, 3])) + 
   (EE^2*gLe*Me*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     (2*Me*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 2]]))/
    (2*Sqrt[2]*MW^2*SW^2*(-Me^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 49 *)
<|"Name" -> "SM-4-point-test.nb / e E -> Z h / e:U", 
 "Sources" -> {"SM-4-point-test.nb / e E -> Z h / e:U", "SM-4-point.nb / input 388 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> MZ, Mass[4] -> Mh}, 
 "Amplitude" -> (EE^2*gLe*Me*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     (2*Me*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]] + 
      SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (2*Sqrt[2]*MW^2*SW^2*(-Me^2 + Mandelstahm[1, 4])) + 
   (EE^2*gRe*Me*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     (2*Me*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]]))/
    (2*Sqrt[2]*MW^2*SW^2*(-Me^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 50 *)
<|"Name" -> "SM-4-point-test.nb / u U -> Z h / Z", 
 "Sources" -> {"SM-4-point-test.nb / u U -> Z h / Z", "SM-4-point.nb / input 398 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> MZ, Mass[4] -> Mh}, 
 "Amplitude" -> (EE^2*gLu*(2*MZ^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      Mu*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
       SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (2*Sqrt[2]*MW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) + 
   (EE^2*gRu*(2*MZ^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
      Mu*(-SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
       SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (2*Sqrt[2]*MW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 51 *)
<|"Name" -> "SM-4-point-test.nb / u U -> Z h / u:T", 
 "Sources" -> {"SM-4-point-test.nb / u U -> Z h / u:T", "SM-4-point.nb / input 402 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> MZ, Mass[4] -> Mh}, 
 "Amplitude" -> (EE^2*gRu*Mu*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     (2*Mu*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]] + 
      SpinorChain[Spinor["Spin", "Square", 2], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (2*Sqrt[2]*MW^2*SW^2*(-Mu^2 + Mandelstahm[1, 3])) + 
   (EE^2*gLu*Mu*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     (2*Mu*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 2]]))/
    (2*Sqrt[2]*MW^2*SW^2*(-Mu^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 52 *)
<|"Name" -> "SM-4-point-test.nb / u U -> Z h / u:U", 
 "Sources" -> {"SM-4-point-test.nb / u U -> Z h / u:U", "SM-4-point.nb / input 404 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> MZ, Mass[4] -> Mh}, 
 "Amplitude" -> (EE^2*gLu*Mu*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     (2*Mu*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]] + 
      SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (2*Sqrt[2]*MW^2*SW^2*(-Mu^2 + Mandelstahm[1, 4])) + 
   (EE^2*gRu*Mu*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     (2*Mu*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]]))/
    (2*Sqrt[2]*MW^2*SW^2*(-Mu^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 53 *)
<|"Name" -> "SM-4-point-test.nb / d D -> Z h / Z", 
 "Sources" -> {"SM-4-point-test.nb / d D -> Z h / Z", "SM-4-point.nb / input 409 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> MZ, Mass[4] -> Mh}, 
 "Amplitude" -> (EE^2*gLd*(2*MZ^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      Md*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
       SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (2*Sqrt[2]*MW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) + 
   (EE^2*gRd*(2*MZ^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
      Md*(-SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
       SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (2*Sqrt[2]*MW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 54 *)
<|"Name" -> "SM-4-point-test.nb / d D -> Z h / d:T", 
 "Sources" -> {"SM-4-point-test.nb / d D -> Z h / d:T", "SM-4-point.nb / input 413 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> MZ, Mass[4] -> Mh}, 
 "Amplitude" -> (EE^2*gRd*Md*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     (2*Md*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]] + 
      SpinorChain[Spinor["Spin", "Square", 2], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (2*Sqrt[2]*MW^2*SW^2*(-Md^2 + Mandelstahm[1, 3])) + 
   (EE^2*gLd*Md*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     (2*Md*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 2]]))/
    (2*Sqrt[2]*MW^2*SW^2*(-Md^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 55 *)
<|"Name" -> "SM-4-point-test.nb / d D -> Z h / d:U", 
 "Sources" -> {"SM-4-point-test.nb / d D -> Z h / d:U", "SM-4-point.nb / input 415 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> MZ, Mass[4] -> Mh}, 
 "Amplitude" -> (EE^2*gLd*Md*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     (2*Md*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]] + 
      SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (2*Sqrt[2]*MW^2*SW^2*(-Md^2 + Mandelstahm[1, 4])) + 
   (EE^2*gRd*Md*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     (2*Md*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]]))/
    (2*Sqrt[2]*MW^2*SW^2*(-Md^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 56 *)
<|"Name" -> "SM-4-point-test.nb / e E -> Z Z / h", 
 "Sources" -> {"SM-4-point-test.nb / e E -> Z Z / h", "SM-4-point.nb / input 538 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> (EE^2*Me*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
    (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
    SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
   (2*MW^2*SW^2*(Mh^2 - Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 57 *)
<|"Name" -> "SM-4-point-test.nb / e E -> Z Z / e", "Sources" -> {"SM-4-point-test.nb / e E -> Z Z / e"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> -1/2*(EE^2*gLe*gRe*Me*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
     (MW^2*SW^2*(-Me^2 + Mandelstahm[1, 4])) - 
   (EE^2*gLe*gRe*Me*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (2*MW^2*SW^2*(Me^2 - Mandelstahm[1, 3])) - 
   (EE^2*gRe^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     (MZ*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]))/
    (2*MW^2*SW^2*(-Me^2 + Mandelstahm[1, 4])) - 
   (EE^2*gLe^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     (MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]))/
    (2*MW^2*SW^2*(Me^2 - Mandelstahm[1, 3])) - 
   (EE^2*gLe^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     (MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]]))/
    (2*MW^2*SW^2*(-Me^2 + Mandelstahm[1, 4])) - 
   (EE^2*gRe^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     (MZ*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]]))/
    (2*MW^2*SW^2*(Me^2 - Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 58 *)
<|"Name" -> "SM-4-point-test.nb / u U -> Z Z / h", 
 "Sources" -> {"SM-4-point-test.nb / u U -> Z Z / h", "SM-4-point.nb / input 555 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> (EE^2*Mu*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
    (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
    SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
   (2*MW^2*SW^2*(Mh^2 - Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 59 *)
<|"Name" -> "SM-4-point-test.nb / u U -> Z Z / u", "Sources" -> {"SM-4-point-test.nb / u U -> Z Z / u"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> -1/2*(EE^2*gLu*gRu*Mu*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
     (MW^2*SW^2*(-Mu^2 + Mandelstahm[1, 4])) - 
   (EE^2*gLu*gRu*Mu*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (2*MW^2*SW^2*(Mu^2 - Mandelstahm[1, 3])) - 
   (EE^2*gRu^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     (MZ*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]))/
    (2*MW^2*SW^2*(-Mu^2 + Mandelstahm[1, 4])) - 
   (EE^2*gLu^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     (MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]))/
    (2*MW^2*SW^2*(Mu^2 - Mandelstahm[1, 3])) - 
   (EE^2*gLu^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     (MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]]))/
    (2*MW^2*SW^2*(-Mu^2 + Mandelstahm[1, 4])) - 
   (EE^2*gRu^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     (MZ*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]]))/
    (2*MW^2*SW^2*(Mu^2 - Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 60 *)
<|"Name" -> "SM-4-point-test.nb / d D -> Z Z / h", 
 "Sources" -> {"SM-4-point-test.nb / d D -> Z Z / h", "SM-4-point.nb / input 567 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> (EE^2*Md*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
    (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
    SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
   (2*MW^2*SW^2*(Mh^2 - Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 61 *)
<|"Name" -> "SM-4-point-test.nb / d D -> Z Z / d", "Sources" -> {"SM-4-point-test.nb / d D -> Z Z / d"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> -1/2*(EE^2*gLd*gRd*Md*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
     (MW^2*SW^2*(-Md^2 + Mandelstahm[1, 4])) - 
   (EE^2*gLd*gRd*Md*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (2*MW^2*SW^2*(Md^2 - Mandelstahm[1, 3])) - 
   (EE^2*gRd^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     (MZ*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]))/
    (2*MW^2*SW^2*(-Md^2 + Mandelstahm[1, 4])) - 
   (EE^2*gLd^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     (MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]))/
    (2*MW^2*SW^2*(Md^2 - Mandelstahm[1, 3])) - 
   (EE^2*gLd^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     (MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]]))/
    (2*MW^2*SW^2*(-Md^2 + Mandelstahm[1, 4])) - 
   (EE^2*gRd^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     (MZ*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]]))/
    (2*MW^2*SW^2*(Md^2 - Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 62 *)
<|"Name" -> "SM-4-point-test.nb / e E -> W- W+ / h", 
 "Sources" -> {"SM-4-point-test.nb / e E -> W- W+ / h", "SM-4-point.nb / input 591 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (EE^2*Me*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
    (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
    SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
   (2*MW^2*SW^2*(Mh^2 - Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 63 *)
<|"Name" -> "SM-4-point-test.nb / e E -> W- W+ / Z", 
 "Sources" -> {"SM-4-point-test.nb / e E -> W- W+ / Z", "SM-4-point.nb / input 597 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> -1/2*(EE^2*gRe*(2*MW*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
        (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
         SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
       2*MW*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
        (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
         SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
       SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
        (Me*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
         Me*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] + 
         2*SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 2]])))/
     (MW^2*SW^2*(MZ^2 - Mandelstahm[1, 2])) - 
   (EE^2*gLe*(2*MW*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
       (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
      2*MW*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
       (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
      SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
       (-(Me*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]) + 
        Me*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] + 
        2*SpinorChain[Spinor["Spin", "Square", 2], Mom[3], Spinor["Spin", "Angle", 1]])))/
    (2*MW^2*SW^2*(MZ^2 - Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 64 *)
<|"Name" -> "SM-4-point-test.nb / e E -> W- W+ / A", 
 "Sources" -> {"SM-4-point-test.nb / e E -> W- W+ / A", "SM-4-point.nb / input 600 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (-2*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/(MW*Mandelstahm[1, 2]) - 
   (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
     (SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 2]] + 
      SpinorChain[Spinor["Spin", "Square", 2], Mom[3], Spinor["Spin", "Angle", 1]]))/
    (MW^2*Mandelstahm[1, 2])|>,

(* ::Subsection::Closed:: *)
(* Reference 65 *)
<|"Name" -> "SM-4-point-test.nb / e E -> W- W+ / ne:U", 
 "Sources" -> {"SM-4-point-test.nb / e E -> W- W+ / ne:U", "SM-4-point.nb / input 594 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> -((EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     (MW*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]]))/
    (MW^2*SW^2*Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 66 *)
<|"Name" -> "SM-4-point-test.nb / u U -> W- W+ / h", 
 "Sources" -> {"SM-4-point-test.nb / u U -> W- W+ / h", "SM-4-point.nb / input 617 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (EE^2*Mu*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
    (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
    SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
   (2*MW^2*SW^2*(Mh^2 - Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 67 *)
<|"Name" -> "SM-4-point-test.nb / u U -> W- W+ / Z", 
 "Sources" -> {"SM-4-point-test.nb / u U -> W- W+ / Z", "SM-4-point.nb / input 624 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> -1/2*(EE^2*gRu*(2*MW*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
        (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
         SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
       2*MW*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
        (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
         SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
       SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
        (Mu*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
         Mu*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] + 
         2*SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 2]])))/
     (MW^2*SW^2*(MZ^2 - Mandelstahm[1, 2])) - 
   (EE^2*gLu*(2*MW*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
       (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
      2*MW*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
       (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
      SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
       (-(Mu*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]) + 
        Mu*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] + 
        2*SpinorChain[Spinor["Spin", "Square", 2], Mom[3], Spinor["Spin", "Angle", 1]])))/
    (2*MW^2*SW^2*(MZ^2 - Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 68 *)
<|"Name" -> "SM-4-point-test.nb / u U -> W- W+ / A", 
 "Sources" -> {"SM-4-point-test.nb / u U -> W- W+ / A", "SM-4-point.nb / input 628 / target 1", 
   "SM-4-point.nb / input 636 / target 1"}, "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> MW, 
   Mass[4] -> MW}, "Amplitude" -> 
  (4*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/(3*MW*Mandelstahm[1, 2]) + 
   (4*EE^2*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
     (SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 2]] + 
      SpinorChain[Spinor["Spin", "Square", 2], Mom[3], Spinor["Spin", "Angle", 1]]))/
    (3*MW^2*Mandelstahm[1, 2])|>,

(* ::Subsection::Closed:: *)
(* Reference 69 *)
<|"Name" -> "SM-4-point-test.nb / u U -> W- W+ / d:T", 
 "Sources" -> {"SM-4-point-test.nb / u U -> W- W+ / d:T", "SM-4-point.nb / input 621 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
    (MW*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
     SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]))/
   (MW^2*SW^2*(-Md^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 70 *)
<|"Name" -> "SM-4-point-test.nb / d D -> W- W+ / h", 
 "Sources" -> {"SM-4-point-test.nb / d D -> W- W+ / h", "SM-4-point.nb / input 642 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (EE^2*Md*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
    (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
    SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
   (2*MW^2*SW^2*(Mh^2 - Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 71 *)
<|"Name" -> "SM-4-point-test.nb / d D -> W- W+ / Z", 
 "Sources" -> {"SM-4-point-test.nb / d D -> W- W+ / Z", "SM-4-point.nb / input 648 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> -1/2*(EE^2*gRd*(2*MW*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
        (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
         SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
       2*MW*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
        (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
         SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
       SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
        (Md*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
         Md*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] + 
         2*SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 2]])))/
     (MW^2*SW^2*(MZ^2 - Mandelstahm[1, 2])) - 
   (EE^2*gLd*(2*MW*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
       (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
      2*MW*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
       (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
      SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
       (-(Md*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]) + 
        Md*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] + 
        2*SpinorChain[Spinor["Spin", "Square", 2], Mom[3], Spinor["Spin", "Angle", 1]])))/
    (2*MW^2*SW^2*(MZ^2 - Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 72 *)
<|"Name" -> "SM-4-point-test.nb / d D -> W- W+ / A", 
 "Sources" -> {"SM-4-point-test.nb / d D -> W- W+ / A", "SM-4-point.nb / input 651 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (-2*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/(3*MW*Mandelstahm[1, 2]) - 
   (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
     (SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 2]] + 
      SpinorChain[Spinor["Spin", "Square", 2], Mom[3], Spinor["Spin", "Angle", 1]]))/
    (3*MW^2*Mandelstahm[1, 2])|>,

(* ::Subsection::Closed:: *)
(* Reference 73 *)
<|"Name" -> "SM-4-point-test.nb / d D -> W- W+ / u:U", 
 "Sources" -> {"SM-4-point-test.nb / d D -> W- W+ / u:U", "SM-4-point.nb / input 645 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> -((EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     (MW*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]]))/
    (MW^2*SW^2*(-Mu^2 + Mandelstahm[1, 4])))|>,

(* ::Subsection::Closed:: *)
(* Reference 74 *)
<|"Name" -> "SM-4-point-test.nb / ne Ne -> W- W+ / Z", 
 "Sources" -> {"SM-4-point-test.nb / ne Ne -> W- W+ / Z", "SM-4-point.nb / input 612 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> 0, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> -1/2*(EE^2*(2*MW*SpinorChain[Spinor["Helicity", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Spin", "Square", 3]]*
       (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
      2*MW*SpinorChain[Spinor["Helicity", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Spin", "Square", 4]]*
       (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
      2*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Helicity", "Square", 2], Mom[3], Spinor["Helicity", "Angle", 1]]))/
    (MW^2*SW^2*(MZ^2 - Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 75 *)
<|"Name" -> "SM-4-point-test.nb / ne Ne -> W- W+ / e:T", 
 "Sources" -> {"SM-4-point-test.nb / ne Ne -> W- W+ / e:T", "SM-4-point.nb / input 609 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> 0, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (EE^2*SpinorChain[Spinor["Helicity", "Angle", 1], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Spin", "Square", 4]]*
    (MW*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
     SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]))/
   (MW^2*SW^2*(-Me^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 76 *)
<|"Name" -> "SM-4-point-test.nb / ne E -> W- h / W+", 
 "Sources" -> {"SM-4-point-test.nb / ne E -> W- h / W+", "SM-4-point.nb / input 762 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> Me, Mass[3] -> MW, Mass[4] -> Mh}, 
 "Amplitude" -> -1/2*(EE^2*(2*MW^2*SpinorChain[Spinor["Helicity", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      Me*SpinorChain[Spinor["Helicity", "Angle", 1], Spinor["Spin", "Angle", 2]]*
       SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (MW^2*SW^2*(-MW^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 77 *)
<|"Name" -> "SM-4-point-test.nb / ne E -> W- h / e:T", 
 "Sources" -> {"SM-4-point-test.nb / ne E -> W- h / e:T", "SM-4-point.nb / input 766 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> Me, Mass[3] -> MW, Mass[4] -> Mh}, 
 "Amplitude" -> -1/2*(EE^2*Me*SpinorChain[Spinor["Helicity", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     (2*Me*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 2]]))/
    (MW^2*SW^2*(-Me^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 78 *)
<|"Name" -> "SM-4-point-test.nb / u D -> W- h / W+", 
 "Sources" -> {"SM-4-point-test.nb / u D -> W- h / W+", "SM-4-point.nb / input 771 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Md, Mass[3] -> MW, Mass[4] -> Mh}, 
 "Amplitude" -> -1/2*(EE^2*(2*MW^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      (Md*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
        Mu*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
       SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (MW^2*SW^2*(-MW^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 79 *)
<|"Name" -> "SM-4-point-test.nb / u D -> W- h / d:T", 
 "Sources" -> {"SM-4-point-test.nb / u D -> W- h / d:T", "SM-4-point.nb / input 775 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Md, Mass[3] -> MW, Mass[4] -> Mh}, 
 "Amplitude" -> -1/2*(EE^2*Md*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     (2*Md*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 2]]))/
    (MW^2*SW^2*(-Md^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 80 *)
<|"Name" -> "SM-4-point-test.nb / u D -> W- h / u:U", 
 "Sources" -> {"SM-4-point-test.nb / u D -> W- h / u:U", "SM-4-point.nb / input 778 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Md, Mass[3] -> MW, Mass[4] -> Mh}, 
 "Amplitude" -> -1/2*(EE^2*Mu*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     (2*Mu*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]] + 
      SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (MW^2*SW^2*(-Mu^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 81 *)
<|"Name" -> "SM-4-point-test.nb / ne Ne -> Z h / Z", "Sources" -> {"SM-4-point-test.nb / ne Ne -> Z h / Z"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> 0, Mass[3] -> MZ, Mass[4] -> Mh}, 
 "Amplitude" -> (EE^2*MZ^2*SpinorChain[Spinor["Helicity", "Angle", 1], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Spin", "Square", 3]])/
   (Sqrt[2]*MW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 82 *)
<|"Name" -> "SM-4-point-test.nb / ne Ne -> Z Z / ne", 
 "Sources" -> {"SM-4-point-test.nb / ne Ne -> Z Z / ne"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> 0, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> (EE^2*SpinorChain[Spinor["Helicity", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Spin", "Square", 4]]*
     (MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]))/
    (2*MW^2*SW^2*Mandelstahm[1, 3]) - (EE^2*SpinorChain[Spinor["Helicity", "Angle", 1], 
      Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Spin", "Square", 3]]*
     (MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]]))/
    (2*MW^2*SW^2*Mandelstahm[1, 4])|>,

(* ::Subsection::Closed:: *)
(* Reference 83 *)
<|"Name" -> "SM-4-point-test.nb / e E -> A.+ h / e:T", 
 "Sources" -> {"SM-4-point-test.nb / e E -> A.+ h / e:T", "SM-4-point.nb / input 354 / target 1", 
   "SM-4-point.nb / input 358 / target 1"}, "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> 0, 
   Mass[4] -> Mh}, "Amplitude" -> 
  (EE^2*Me*(SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square", 3]]*
      ((2*Me^2 - Mh^2)*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]] + 
       Me*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Spinor["Spin", "Angle", 2]]) + 
     Me*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]]*
      SpinorChain[Spinor["Helicity", "Square", 3], Mom[2], Spinor["Spin", "Angle", 1]] - 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
      SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2], Spinor["Helicity", "Square", 3]]))/
   (Sqrt[2]*MW*SW*(Me^2 - Mandelstahm[1, 3])*(Me^2 - Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 84 *)
<|"Name" -> "SM-4-point-test.nb / u U -> A.+ h / u:T", 
 "Sources" -> {"SM-4-point-test.nb / u U -> A.+ h / u:T", "SM-4-point.nb / input 363 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> 0, Mass[4] -> Mh}, 
 "Amplitude" -> 
  -1/3*(Sqrt[2]*EE^2*Mu*(SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square", 3]]*
       (-((Mh^2 - 2*Mu^2)*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]]) + 
        Mu*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Spinor["Spin", "Angle", 2]]) + 
      Mu*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]]*
       SpinorChain[Spinor["Helicity", "Square", 3], Mom[2], Spinor["Spin", "Angle", 1]] - 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
       SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2], Spinor["Helicity", "Square", 3]]))/
    (MW*SW*(Mu^2 - Mandelstahm[1, 3])*(Mu^2 - Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 85 *)
<|"Name" -> "SM-4-point-test.nb / d D -> A.+ h / d:T", 
 "Sources" -> {"SM-4-point-test.nb / d D -> A.+ h / d:T", "SM-4-point.nb / input 368 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> 0, Mass[4] -> Mh}, 
 "Amplitude" -> (EE^2*Md*(SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square", 3]]*
      ((2*Md^2 - Mh^2)*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]] + 
       Md*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Spinor["Spin", "Angle", 2]]) + 
     Md*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]]*
      SpinorChain[Spinor["Helicity", "Square", 3], Mom[2], Spinor["Spin", "Angle", 1]] - 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
      SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2], Spinor["Helicity", "Square", 3]]))/
   (3*Sqrt[2]*MW*SW*(Md^2 - Mandelstahm[1, 3])*(Md^2 - Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 86 *)
<|"Name" -> "SM-4-point-test.nb / e E -> A.+ A.+ / e:T", 
 "Sources" -> {"SM-4-point-test.nb / e E -> A.+ A.+ / e:T", "SM-4-point.nb / input 425 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> 0, Mass[4] -> 0}, 
 "Amplitude" -> (2*EE^2*Me*SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity", "Square", 4]]^2*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]])/
   ((-Me^2 + Mandelstahm[1, 3])*(-Me^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 87 *)
<|"Name" -> "SM-4-point-test.nb / e E -> A.+ A.- / e", 
 "Sources" -> {"SM-4-point-test.nb / e E -> A.+ A.- / e", "SM-4-point.nb / input 430 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> 0, Mass[4] -> 0}, 
 "Amplitude" -> (-2*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]])*
    SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle", 4]])/
   ((-Me^2 + Mandelstahm[1, 3])*(-Me^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 88 *)
<|"Name" -> "SM-4-point-test.nb / u U -> A.+ A.+ / u:T", 
 "Sources" -> {"SM-4-point-test.nb / u U -> A.+ A.+ / u:T", "SM-4-point.nb / input 435 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> 0, Mass[4] -> 0}, 
 "Amplitude" -> (8*EE^2*Mu*SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity", "Square", 4]]^2*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]])/
   (9*(-Mu^2 + Mandelstahm[1, 3])*(-Mu^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 89 *)
<|"Name" -> "SM-4-point-test.nb / u U -> A.+ A.- / u", 
 "Sources" -> {"SM-4-point-test.nb / u U -> A.+ A.- / u", "SM-4-point.nb / input 439 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> 0, Mass[4] -> 0}, 
 "Amplitude" -> (-8*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]])*
    SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle", 4]])/
   (9*(-Mu^2 + Mandelstahm[1, 3])*(-Mu^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 90 *)
<|"Name" -> "SM-4-point-test.nb / d D -> A.+ A.+ / d:T", 
 "Sources" -> {"SM-4-point-test.nb / d D -> A.+ A.+ / d:T", "SM-4-point.nb / input 444 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> 0, Mass[4] -> 0}, 
 "Amplitude" -> (2*EE^2*Md*SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity", "Square", 4]]^2*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]])/
   (9*(-Md^2 + Mandelstahm[1, 3])*(-Md^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 91 *)
<|"Name" -> "SM-4-point-test.nb / d D -> A.+ A.- / d", 
 "Sources" -> {"SM-4-point-test.nb / d D -> A.+ A.- / d", "SM-4-point.nb / input 448 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> 0, Mass[4] -> 0}, 
 "Amplitude" -> (-2*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]])*
    SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle", 4]])/
   (9*(-Md^2 + Mandelstahm[1, 3])*(-Md^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 92 *)
<|"Name" -> "SM-4-point-test.nb / u U -> G.+ G.+ / u:T", 
 "Sources" -> {"SM-4-point-test.nb / u U -> G.+ G.+ / u:T", "SM-4-point.nb / input 454 / target 1", 
   "SM-4-point.nb / input 457 / target 1"}, "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> 0, 
   Mass[4] -> 0}, "Amplitude" -> 
  (2*GG^2*Mu*SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity", "Square", 4]]^2*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]])/
   ((-Mu^2 + Mandelstahm[1, 3])*(-Mu^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 93 *)
<|"Name" -> "SM-4-point-test.nb / A.+ e -> Z E / e:S", 
 "Sources" -> {"SM-4-point-test.nb / A.+ e -> Z E / e:S", "SM-4-point.nb / input 499 / target 1", 
   "SM-4-point.nb / input 502 / target 1"}, "Masses" -> {Mass[1] -> 0, Mass[2] -> Me, Mass[3] -> MZ, 
   Mass[4] -> Me}, "Amplitude" -> 
  -((EE^2*gLe*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      (MZ*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 4]]*
        SpinorChain[Spinor["Helicity", "Square", 1], Mom[2], Spinor["Spin", "Angle", 3]] - 
       Me*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]]*
        SpinorChain[Spinor["Helicity", "Square", 1], Mom[3], Spinor["Spin", "Angle", 4]]))/
     (MW*SW*(Me^2 - Mandelstahm[1, 2])*(Me^2 - Mandelstahm[1, 4]))) - 
   (EE^2*gRe*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     (-(Me*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]]*
        SpinorChain[Spinor["Helicity", "Square", 1], Mom[3], Spinor["Spin", "Angle", 2]]) + 
      MZ*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 2]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (MW*SW*(Me^2 - Mandelstahm[1, 2])*(Me^2 - Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 94 *)
<|"Name" -> "SM-4-point-test.nb / A.+ u -> Z U / u:S", 
 "Sources" -> {"SM-4-point-test.nb / A.+ u -> Z U / u:S", "SM-4-point.nb / input 508 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> Mu, Mass[3] -> MZ, Mass[4] -> Mu}, 
 "Amplitude" -> (2*EE^2*gLu*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     (MZ*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Mom[2], Spinor["Spin", "Angle", 3]] - 
      Mu*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Mom[3], Spinor["Spin", "Angle", 4]]))/
    (3*MW*SW*(Mu^2 - Mandelstahm[1, 2])*(Mu^2 - Mandelstahm[1, 4])) + 
   (2*EE^2*gRu*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     (-(Mu*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]]*
        SpinorChain[Spinor["Helicity", "Square", 1], Mom[3], Spinor["Spin", "Angle", 2]]) + 
      MZ*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 2]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (3*MW*SW*(Mu^2 - Mandelstahm[1, 2])*(Mu^2 - Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 95 *)
<|"Name" -> "SM-4-point-test.nb / A.+ d -> Z D / d:S", 
 "Sources" -> {"SM-4-point-test.nb / A.+ d -> Z D / d:S", "SM-4-point.nb / input 524 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> Md, Mass[3] -> MZ, Mass[4] -> Md}, 
 "Amplitude" -> -1/3*(EE^2*gLd*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      (MZ*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 4]]*
        SpinorChain[Spinor["Helicity", "Square", 1], Mom[2], Spinor["Spin", "Angle", 3]] - 
       Md*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]]*
        SpinorChain[Spinor["Helicity", "Square", 1], Mom[3], Spinor["Spin", "Angle", 4]]))/
     (MW*SW*(Md^2 - Mandelstahm[1, 2])*(Md^2 - Mandelstahm[1, 4])) - 
   (EE^2*gRd*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     (-(Md*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]]*
        SpinorChain[Spinor["Helicity", "Square", 1], Mom[3], Spinor["Spin", "Angle", 2]]) + 
      MZ*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 2]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (3*MW*SW*(Md^2 - Mandelstahm[1, 2])*(Md^2 - Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 96 *)
<|"Name" -> "SM-4-point-test.nb / h G.+ -> U u / u:T", 
 "Sources" -> {"SM-4-point-test.nb / h G.+ -> U u / u:T", "SM-4-point.nb / input 659 / target 1"}, 
 "Masses" -> {Mass[1] -> Mh, Mass[2] -> 0, Mass[3] -> Mu, Mass[4] -> Mu}, 
 "Amplitude" -> -((EE*GG*Mu*(-(Mh^2*SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Spin", "Square", 3]]*
        SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Spin", "Square", 4]]) + 
      Mu*SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Helicity", "Square", 2], Mom[1], Spinor["Spin", "Angle", 3]] + 
      Mu*SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Helicity", "Square", 2], Mom[1], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Helicity", "Square", 2], Mom[1], Mom[3], Spinor["Helicity", "Square", 2]]))/
    (Sqrt[2]*MW*SW*(-Mu^2 + Mandelstahm[1, 3])*(-Mu^2 + Mandelstahm[1, 4])))|>,

(* ::Subsection::Closed:: *)
(* Reference 97 *)
<|"Name" -> "SM-4-point-test.nb / h G.+ -> D d / d:T", 
 "Sources" -> {"SM-4-point-test.nb / h G.+ -> D d / d:T", "SM-4-point.nb / input 667 / target 1"}, 
 "Masses" -> {Mass[1] -> Mh, Mass[2] -> 0, Mass[3] -> Md, Mass[4] -> Md}, 
 "Amplitude" -> -((EE*GG*Md*(-(Mh^2*SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Spin", "Square", 3]]*
        SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Spin", "Square", 4]]) + 
      Md*SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Helicity", "Square", 2], Mom[1], Spinor["Spin", "Angle", 3]] + 
      Md*SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Helicity", "Square", 2], Mom[1], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Helicity", "Square", 2], Mom[1], Mom[3], Spinor["Helicity", "Square", 2]]))/
    (Sqrt[2]*MW*SW*(-Md^2 + Mandelstahm[1, 3])*(-Md^2 + Mandelstahm[1, 4])))|>,

(* ::Subsection::Closed:: *)
(* Reference 98 *)
<|"Name" -> "SM-4-point-test.nb / G.+ u -> Z U / u:S", 
 "Sources" -> {"SM-4-point-test.nb / G.+ u -> Z U / u:S", "SM-4-point.nb / input 679 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> Mu, Mass[3] -> MZ, Mass[4] -> Mu}, 
 "Amplitude" -> (EE*GG*gLu*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     (MZ*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Mom[2], Spinor["Spin", "Angle", 3]] - 
      Mu*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Mom[3], Spinor["Spin", "Angle", 4]]))/
    (MW*SW*(Mu^2 - Mandelstahm[1, 2])*(Mu^2 - Mandelstahm[1, 4])) + 
   (EE*GG*gRu*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     (-(Mu*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]]*
        SpinorChain[Spinor["Helicity", "Square", 1], Mom[3], Spinor["Spin", "Angle", 2]]) + 
      MZ*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 2]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (MW*SW*(Mu^2 - Mandelstahm[1, 2])*(Mu^2 - Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 99 *)
<|"Name" -> "SM-4-point-test.nb / G.+ d -> Z D / d:S", 
 "Sources" -> {"SM-4-point-test.nb / G.+ d -> Z D / d:S", "SM-4-point.nb / input 687 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> Md, Mass[3] -> MZ, Mass[4] -> Md}, 
 "Amplitude" -> (EE*GG*gLd*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     (MZ*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Mom[2], Spinor["Spin", "Angle", 3]] - 
      Md*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Mom[3], Spinor["Spin", "Angle", 4]]))/
    (MW*SW*(Md^2 - Mandelstahm[1, 2])*(Md^2 - Mandelstahm[1, 4])) + 
   (EE*GG*gRd*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     (-(Md*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]]*
        SpinorChain[Spinor["Helicity", "Square", 1], Mom[3], Spinor["Spin", "Angle", 2]]) + 
      MZ*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 2]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (MW*SW*(Md^2 - Mandelstahm[1, 2])*(Md^2 - Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 100 *)
<|"Name" -> "SM-4-point-test.nb / G.+ u -> A.+ U / u", 
 "Sources" -> {"SM-4-point-test.nb / G.+ u -> A.+ U / u", "SM-4-point.nb / input 699 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> Mu, Mass[3] -> 0, Mass[4] -> Mu}, 
 "Amplitude" -> (4*EE*GG*Mu*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Helicity", "Square", 3]]^2*
    SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]])/
   (3*(-Mu^2 + Mandelstahm[1, 2])*(-Mu^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 101 *)
<|"Name" -> "SM-4-point-test.nb / G.+ u -> A.- U / u", 
 "Sources" -> {"SM-4-point-test.nb / G.+ u -> A.- U / u", "SM-4-point.nb / input 705 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> Mu, Mass[3] -> 0, Mass[4] -> Mu}, 
 "Amplitude" -> (4*EE*GG*(-(SpinorChain[Spinor["Helicity", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 2]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Mom[2], Spinor["Helicity", "Angle", 3]]) + 
     SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 4]]*
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle", 3]]*
      SpinorChain[Spinor["Helicity", "Square", 1], Mom[2], Spinor["Helicity", "Angle", 3]]))/
   (3*(-Mu^2 + Mandelstahm[1, 2])*(-Mu^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 102 *)
<|"Name" -> "SM-4-point-test.nb / G.+ d -> A.+ D / d", 
 "Sources" -> {"SM-4-point-test.nb / G.+ d -> A.+ D / d", "SM-4-point.nb / input 712 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> Md, Mass[3] -> 0, Mass[4] -> Md}, 
 "Amplitude" -> (-2*EE*GG*Md*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Helicity", "Square", 3]]^2*
    SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]])/
   (3*(-Md^2 + Mandelstahm[1, 2])*(-Md^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 103 *)
<|"Name" -> "SM-4-point-test.nb / G.+ u -> G.+ U / u", 
 "Sources" -> {"SM-4-point-test.nb / G.+ u -> G.+ U / u", "SM-4-point.nb / input 723 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> Mu, Mass[3] -> 0, Mass[4] -> Mu}, 
 "Amplitude" -> (2*GG^2*Mu*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Helicity", "Square", 3]]^2*
    SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]])/
   ((-Mu^2 + Mandelstahm[1, 2])*(-Mu^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 104 *)
<|"Name" -> "SM-4-point-test.nb / A.+ W+ -> U d / W+", 
 "Sources" -> {"SM-4-point-test.nb / A.+ W+ -> U d / W+", "SM-4-point.nb / input 818 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> MW, Mass[3] -> Mu, Mass[4] -> Md}, 
 "Amplitude" -> (Sqrt[2]*EE^2*(2/(3*(-Mu^2 + Mandelstahm[1, 3])) - 1/(3*(-Md^2 + Mandelstahm[1, 4])))*
    SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
    (-(MW*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Mom[4], Spinor["Spin", "Angle", 2]]) + 
     SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 2]]*
      (Md^2*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]] - 
       Mu*SpinorChain[Spinor["Helicity", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]])))/
   (MW*SW*(-MW^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 105 *)
<|"Name" -> "SM-4-point-test.nb / A.+ W+ -> Ne e / W+", 
 "Sources" -> {"SM-4-point-test.nb / A.+ W+ -> Ne e / W+", "SM-4-point-test.nb / A.+ W+ -> Ne e / e:U", 
   "SM-4-point.nb / input 825 / target 1", "SM-4-point.nb / input 828 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> MW, Mass[3] -> 0, Mass[4] -> Me}, 
 "Amplitude" -> -((Sqrt[2]*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     (Me^2*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Helicity", "Square", 3]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 2]] - 
      MW*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Helicity", "Square", 3]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Mom[4], Spinor["Spin", "Angle", 2]]))/
    (MW*SW*(-MW^2 + Mandelstahm[1, 2])*(-Me^2 + Mandelstahm[1, 4])))|>,

(* ::Subsection::Closed:: *)
(* Reference 106 *)
<|"Name" -> "SM-4-point-test.nb / E ne -> A.+ W- / W+", 
 "Sources" -> {"SM-4-point-test.nb / E ne -> A.+ W- / W+", "SM-4-point-test.nb / E ne -> A.+ W- / e:T", 
   "SM-4-point.nb / input 845 / target 1"}, "Masses" -> {Mass[1] -> Me, Mass[2] -> 0, Mass[3] -> 0, 
   Mass[4] -> MW}, "Amplitude" -> 
  (Sqrt[2]*EE^2*(Me*SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Spin", "Square", 4]]*
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle", 2]] + 
     MW*SpinorChain[Spinor["Helicity", "Angle", 2], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square", 3]])*
    SpinorChain[Spinor["Helicity", "Square", 3], Mom[2], Spinor["Spin", "Angle", 4]])/
   (MW*SW*(-MW^2 + Mandelstahm[1, 2])*(-Me^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 107 *)
<|"Name" -> "SM-4-point-test.nb / E ne -> Z W- / W+", 
 "Sources" -> {"SM-4-point-test.nb / E ne -> Z W- / W+", "SM-4-point.nb / input 869 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> 0, Mass[3] -> MZ, Mass[4] -> MW}, 
 "Amplitude" -> -((EE^2*(2*MW^2*SpinorChain[Spinor["Helicity", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
       (MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
        MW*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
      2*MW^2*SpinorChain[Spinor["Helicity", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
       (MW*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
        MZ*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
      SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
       (Me*(2*MW^2 - MZ^2)*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle", 2]] + 
        2*MW^2*SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Helicity", "Angle", 2]])))/
    (Sqrt[2]*MW^2*MZ^2*SW^2*(-MW^2 + Mandelstahm[1, 2])))|>,

(* ::Subsection::Closed:: *)
(* Reference 108 *)
<|"Name" -> "SM-4-point-test.nb / E ne -> Z W- / e:T", 
 "Sources" -> {"SM-4-point-test.nb / E ne -> Z W- / e:T", "SM-4-point.nb / input 872 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> 0, Mass[3] -> MZ, Mass[4] -> MW}, 
 "Amplitude" -> (EE^2*SpinorChain[Spinor["Helicity", "Angle", 2], Spinor["Spin", "Angle", 4]]*
    (gRe*Me*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
     gLe*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
      (MZ*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
       SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]])))/
   (Sqrt[2]*MW^2*SW^2*(-Me^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 109 *)
<|"Name" -> "SM-4-point-test.nb / E ne -> Z W- / ne:U", 
 "Sources" -> {"SM-4-point-test.nb / E ne -> Z W- / ne:U", "SM-4-point.nb / input 875 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> 0, Mass[3] -> MZ, Mass[4] -> MW}, 
 "Amplitude" -> -((EE^2*SpinorChain[Spinor["Helicity", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     (MW*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]))/
    (Sqrt[2]*MW^2*SW^2*Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 110 *)
<|"Name" -> "SM-4-point-test.nb / U d -> Z W+ / W+", 
 "Sources" -> {"SM-4-point-test.nb / U d -> Z W+ / W+", "SM-4-point.nb / input 887 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Md, Mass[3] -> MZ, Mass[4] -> MW}, 
 "Amplitude" -> (EE^2*(2*MW^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
      (MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
       MW*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
     2*MW^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
      (MW*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
       MZ*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
      ((2*MW^2 - MZ^2)*(Mu*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
         Md*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]) + 
       2*MW^2*SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 2]])))/
   (Sqrt[2]*MW^2*MZ^2*SW^2*(-MW^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 111 *)
<|"Name" -> "SM-4-point-test.nb / U d -> Z W+ / u:T", 
 "Sources" -> {"SM-4-point-test.nb / U d -> Z W+ / u:T"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Md, Mass[3] -> MZ, Mass[4] -> MW}, 
 "Amplitude" -> (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
    (gRu*Mu*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
     gLu*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
      (MZ*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
       SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]])))/
   (Sqrt[2]*MW^2*SW^2*(-Mu^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 112 *)
<|"Name" -> "SM-4-point-test.nb / U d -> Z W+ / d:U", 
 "Sources" -> {"SM-4-point-test.nb / U d -> Z W+ / d:U"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Md, Mass[3] -> MZ, Mass[4] -> MW}, 
 "Amplitude" -> -((EE^2*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     (gRd*Md*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      gLd*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       (MW*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] - 
        SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]])))/
    (Sqrt[2]*MW^2*SW^2*(-Md^2 + Mandelstahm[1, 4])))|>,

(* ::Subsection::Closed:: *)
(* Reference 113 *)
<|"Name" -> "SM-4-point-test.nb / G.+ W+ -> U d / u:T", 
 "Sources" -> {"SM-4-point-test.nb / G.+ W+ -> U d / u:T", "SM-4-point-test.nb / G.+ W+ -> U d / d:U", 
   "SM-4-point.nb / input 909 / target 1", "SM-4-point.nb / input 913 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> MW, Mass[3] -> Mu, Mass[4] -> Md}, 
 "Amplitude" -> -((Sqrt[2]*EE*GG*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     (-(MW*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]]*
        SpinorChain[Spinor["Helicity", "Square", 1], Mom[4], Spinor["Spin", "Angle", 2]]) + 
      SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 2]]*
       (Md^2*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]] - 
        Mu*SpinorChain[Spinor["Helicity", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]])))/
    (MW*SW*(-Mu^2 + Mandelstahm[1, 3])*(-Md^2 + Mandelstahm[1, 4])))|>,

(* ::Subsection::Closed:: *)
(* Reference 114 *)
<|"Name" -> "SM-4-point-test.nb / h h -> h h / h:S", 
 "Sources" -> {"SM-4-point-test.nb / h h -> h h / h:S", "SM-4-point.nb / input 929 / target 1"}, 
 "Masses" -> {Mass[1] -> Mh, Mass[2] -> Mh, Mass[3] -> Mh, Mass[4] -> Mh}, 
 "Amplitude" -> (-9*EE^2*Mh^4)/(4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 115 *)
<|"Name" -> "SM-4-point-test.nb / h h -> h h / h:T", 
 "Sources" -> {"SM-4-point-test.nb / h h -> h h / h:T", "SM-4-point.nb / input 932 / target 1"}, 
 "Masses" -> {Mass[1] -> Mh, Mass[2] -> Mh, Mass[3] -> Mh, Mass[4] -> Mh}, 
 "Amplitude" -> (-9*EE^2*Mh^4)/(4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 116 *)
<|"Name" -> "SM-4-point-test.nb / h h -> h h / h:U", 
 "Sources" -> {"SM-4-point-test.nb / h h -> h h / h:U", "SM-4-point.nb / input 935 / target 1"}, 
 "Masses" -> {Mass[1] -> Mh, Mass[2] -> Mh, Mass[3] -> Mh, Mass[4] -> Mh}, 
 "Amplitude" -> (-9*EE^2*Mh^4)/(4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 117 *)
<|"Name" -> "SM-4-point-test.nb / h h -> h h / contact", 
 "Sources" -> {"SM-4-point-test.nb / h h -> h h / contact", "SM-4-point.nb / input 939 / target 1"}, 
 "Masses" -> {Mass[1] -> Mh, Mass[2] -> Mh, Mass[3] -> Mh, Mass[4] -> Mh}, 
 "Amplitude" -> (-3*EE^2*Mh^2)/(4*MW^2*SW^2)|>,

(* ::Subsection::Closed:: *)
(* Reference 118 *)
<|"Name" -> "SM-4-point-test.nb / h h -> Z Z / h:S", 
 "Sources" -> {"SM-4-point-test.nb / h h -> Z Z / h:S", "SM-4-point.nb / input 950 / target 1"}, 
 "Masses" -> {Mass[1] -> Mh, Mass[2] -> Mh, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> (-3*EE^2*Mh^2*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
   (2*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 119 *)
<|"Name" -> "SM-4-point-test.nb / h h -> Z Z / Z:T", 
 "Sources" -> {"SM-4-point-test.nb / h h -> Z Z / Z:T", "SM-4-point.nb / input 954 / target 1"}, 
 "Masses" -> {Mass[1] -> Mh, Mass[2] -> Mh, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> -1/2*(EE^2*(2*MZ^2*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]] + 
      MZ*(SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
         SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]] + 
        SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
         SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]])))/
    (MW^2*SW^2*(-MZ^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 120 *)
<|"Name" -> "SM-4-point-test.nb / h h -> Z Z / Z:U", 
 "Sources" -> {"SM-4-point-test.nb / h h -> Z Z / Z:U", "SM-4-point.nb / input 957 / target 1"}, 
 "Masses" -> {Mass[1] -> Mh, Mass[2] -> Mh, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> -1/2*(EE^2*(2*MZ^2*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]] - 
      MZ*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
         SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]] + 
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
         SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]])))/
    (MW^2*SW^2*(-MZ^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 121 *)
<|"Name" -> "SM-4-point-test.nb / h h -> Z Z / contact", 
 "Sources" -> {"SM-4-point-test.nb / h h -> Z Z / contact", "SM-4-point.nb / input 946 / target 1"}, 
 "Masses" -> {Mass[1] -> Mh, Mass[2] -> Mh, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> (EE^2*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/(2*MW^2*SW^2)|>,

(* ::Subsection::Closed:: *)
(* Reference 122 *)
<|"Name" -> "SM-4-point-test.nb / h h -> W- W+ / h:S", 
 "Sources" -> {"SM-4-point-test.nb / h h -> W- W+ / h:S", "SM-4-point.nb / input 968 / target 1"}, 
 "Masses" -> {Mass[1] -> Mh, Mass[2] -> Mh, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (-3*EE^2*Mh^2*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
   (2*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 123 *)
<|"Name" -> "SM-4-point-test.nb / h h -> W- W+ / W+:T", 
 "Sources" -> {"SM-4-point-test.nb / h h -> W- W+ / W+:T", "SM-4-point.nb / input 972 / target 1"}, 
 "Masses" -> {Mass[1] -> Mh, Mass[2] -> Mh, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> -1/2*(EE^2*(2*MW^2*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]] + 
      MW*(SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
         SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]] + 
        SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
         SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]])))/
    (MW^2*SW^2*(-MW^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 124 *)
<|"Name" -> "SM-4-point-test.nb / h h -> W- W+ / W+:U", 
 "Sources" -> {"SM-4-point-test.nb / h h -> W- W+ / W+:U", "SM-4-point.nb / input 975 / target 1"}, 
 "Masses" -> {Mass[1] -> Mh, Mass[2] -> Mh, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> -1/2*(EE^2*(2*MW^2*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]] - 
      MW*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
         SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]] + 
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
         SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]])))/
    (MW^2*SW^2*(-MW^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 125 *)
<|"Name" -> "SM-4-point-test.nb / h h -> W- W+ / contact", 
 "Sources" -> {"SM-4-point-test.nb / h h -> W- W+ / contact", "SM-4-point.nb / input 964 / target 1"}, 
 "Masses" -> {Mass[1] -> Mh, Mass[2] -> Mh, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (EE^2*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/(2*MW^2*SW^2)|>,

(* ::Subsection::Closed:: *)
(* Reference 126 *)
<|"Name" -> "SM-4-point-test.nb / Z Z -> Z Z / h:S", 
 "Sources" -> {"SM-4-point-test.nb / Z Z -> Z Z / h:S", "SM-4-point.nb / input 1081 / target 1"}, 
 "Masses" -> {Mass[1] -> MZ, Mass[2] -> MZ, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> -((EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
    (MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2])))|>,

(* ::Subsection::Closed:: *)
(* Reference 127 *)
<|"Name" -> "SM-4-point-test.nb / Z Z -> Z Z / h:T", 
 "Sources" -> {"SM-4-point-test.nb / Z Z -> Z Z / h:T", "SM-4-point.nb / input 1084 / target 1"}, 
 "Masses" -> {Mass[1] -> MZ, Mass[2] -> MZ, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> -((EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 3])))|>,

(* ::Subsection::Closed:: *)
(* Reference 128 *)
<|"Name" -> "SM-4-point-test.nb / Z Z -> Z Z / h:U", 
 "Sources" -> {"SM-4-point-test.nb / Z Z -> Z Z / h:U", "SM-4-point.nb / input 1087 / target 1"}, 
 "Masses" -> {Mass[1] -> MZ, Mass[2] -> MZ, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> -((EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 4])))|>,

(* ::Subsection::Closed:: *)
(* Reference 129 *)
<|"Name" -> "SM-4-point-test.nb / Z Z -> W- W+ / h:S", 
 "Sources" -> {"SM-4-point-test.nb / Z Z -> W- W+ / h:S", "SM-4-point.nb / input 1093 / target 1"}, 
 "Masses" -> {Mass[1] -> MZ, Mass[2] -> MZ, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> -((EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
    (MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2])))|>,

(* ::Subsection::Closed:: *)
(* Reference 130 *)
<|"Name" -> "SM-4-point-test.nb / W+ W- -> W+ W- / h:S", 
 "Sources" -> {"SM-4-point-test.nb / W+ W- -> W+ W- / h:S"}, 
 "Masses" -> {Mass[1] -> MW, Mass[2] -> MW, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> -((EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
    (MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2])))|>,

(* ::Subsection::Closed:: *)
(* Reference 131 *)
<|"Name" -> "SM-4-point-test.nb / W+ W- -> W+ W- / h:U", 
 "Sources" -> {"SM-4-point-test.nb / W+ W- -> W+ W- / h:U", "SM-4-point-test.nb / W+ W+ -> W- W- / h:U", 
   "SM-4-point.nb / input 1121 / target 1"}, "Masses" -> {Mass[1] -> MW, Mass[2] -> MW, Mass[3] -> MW, 
   Mass[4] -> MW}, "Amplitude" -> -((EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 4])))|>,

(* ::Subsection::Closed:: *)
(* Reference 132 *)
<|"Name" -> "SM-4-point-test.nb / W+ W- -> W- W+ / h:T", 
 "Sources" -> {"SM-4-point-test.nb / W+ W- -> W- W+ / h:T", "SM-4-point-test.nb / W+ W+ -> W- W- / h:T", 
   "SM-4-point.nb / input 1118 / target 1"}, "Masses" -> {Mass[1] -> MW, Mass[2] -> MW, Mass[3] -> MW, 
   Mass[4] -> MW}, "Amplitude" -> -((EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 3])))|>,

(* ::Subsection::Closed:: *)
(* Reference 133 *)
<|"Name" -> "SM-4-point-test.nb / A.+ h -> W- W+ / W+:T", 
 "Sources" -> {"SM-4-point-test.nb / A.+ h -> W- W+ / W+:T", "SM-4-point.nb / input 982 / target 1", 
   "SM-4-point.nb / input 985 / target 1"}, "Masses" -> {Mass[1] -> 0, Mass[2] -> Mh, Mass[3] -> MW, 
   Mass[4] -> MW}, "Amplitude" -> (Sqrt[2]*EE^2*SpinorChain[Spinor["Spin", "Angle", 3], 
     Spinor["Spin", "Angle", 4]]*(Mh^2*SpinorChain[Spinor["Helicity", "Square", 1], 
       Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 1], 
       Spinor["Spin", "Square", 4]] - 
     MW*(SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 4]]*
        SpinorChain[Spinor["Helicity", "Square", 1], Mom[2], Spinor["Spin", "Angle", 3]] + 
       SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]]*
        SpinorChain[Spinor["Helicity", "Square", 1], Mom[2], Spinor["Spin", "Angle", 4]])))/
   (MW*SW*(-MW^2 + Mandelstahm[1, 3])*(-MW^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 134 *)
<|"Name" -> "SM-4-point-test.nb / Z h -> W- W+ / Z:S", 
 "Sources" -> {"SM-4-point-test.nb / Z h -> W- W+ / Z:S", "SM-4-point.nb / input 992 / target 1"}, 
 "Masses" -> {Mass[1] -> MZ, Mass[2] -> Mh, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> 
  (EE^2*(2*MW*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 1]] - 
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 1]]))/
   (Sqrt[2]*MW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 135 *)
<|"Name" -> "SM-4-point-test.nb / Z h -> W- W+ / W+:T", 
 "Sources" -> {"SM-4-point-test.nb / Z h -> W- W+ / W+:T"}, 
 "Masses" -> {Mass[1] -> MZ, Mass[2] -> Mh, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> -((Sqrt[2]*EE^2*MW*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/
     (MZ^2*SW^2*(MW^2 - Mandelstahm[1, 3]))) - 
   (Sqrt[2]*EE^2*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/
    (MZ*SW^2*(MW^2 - Mandelstahm[1, 3])) - 
   (Sqrt[2]*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
    (MZ*SW^2*(MW^2 - Mandelstahm[1, 3])) - 
   (Sqrt[2]*EE^2*MW*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
    (MZ^2*SW^2*(MW^2 - Mandelstahm[1, 3])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 4]])/
    (Sqrt[2]*MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 4], Mom[3], Spinor["Spin", "Angle", 4]])/
    (Sqrt[2]*MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (Sqrt[2]*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 4], Mom[3], Spinor["Spin", "Angle", 4]])/
    (MZ^2*SW^2*(MW^2 - Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 136 *)
<|"Name" -> "SM-4-point-test.nb / Z h -> W- W+ / W+:U", 
 "Sources" -> {"SM-4-point-test.nb / Z h -> W- W+ / W+:U"}, 
 "Masses" -> {Mass[1] -> MZ, Mass[2] -> Mh, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> -((Sqrt[2]*EE^2*MW*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])/
     (MZ^2*SW^2*(MW^2 - Mandelstahm[1, 4]))) - 
   (Sqrt[2]*EE^2*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/
    (MZ*SW^2*(MW^2 - Mandelstahm[1, 4])) - 
   (Sqrt[2]*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
    (MZ*SW^2*(MW^2 - Mandelstahm[1, 4])) - 
   (Sqrt[2]*EE^2*MW*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
    (MZ^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 3]])/
    (Sqrt[2]*MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 3]])/
    (Sqrt[2]*MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) - 
   (Sqrt[2]*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 3]])/
    (MZ^2*SW^2*(MW^2 - Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 137 *)
<|"Name" -> "SM-4-point-test.nb / A.+ A.+ -> W- W+ / W+", 
 "Sources" -> {"SM-4-point-test.nb / A.+ A.+ -> W- W+ / W+", "SM-4-point.nb / input 1009 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> 0, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (2*EE^2*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Helicity", "Square", 2]]^2*
    SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]^2)/
   ((-MW^2 + Mandelstahm[1, 3])*(-MW^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 138 *)
<|"Name" -> "SM-4-point-test.nb / A.+ A.- -> W- W+ / W+", 
 "Sources" -> {"SM-4-point-test.nb / A.+ A.- -> W- W+ / W+", "SM-4-point.nb / input 1019 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> 0, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (2*EE^2*(SpinorChain[Spinor["Helicity", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Helicity", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 4]])^2)/
   ((-MW^2 + Mandelstahm[1, 3])*(-MW^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 139 *)
<|"Name" -> "SM-4-point-test.nb / A.+ Z -> W- W+ / W+:U", 
 "Sources" -> {"SM-4-point-test.nb / A.+ Z -> W- W+ / W+:U", "SM-4-point.nb / input 1074 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> MZ, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (2*EE^2*(CW^2*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 4]]^2*
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]^2 + 
     (-1 + 2*CW^2)*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]]*
      SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 4]]*
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]] + 
     CW^2*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]]^2*
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]^2 - 
     CW*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 2]]*
      SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 4]]*
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
     CW*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 2]]*
      SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]]*
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
     CW^2*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 2]]^2*
      SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]^2))/
   (CW*SW*(-(CW^2*MZ^2) + Mandelstahm[1, 3])*(-(CW^2*MZ^2) + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 140 *)
<|"Name" -> "SM-4-point-test.nb / W+ W+ -> W- W- / Z:T", 
 "Sources" -> {"SM-4-point-test.nb / W+ W+ -> W- W- / Z:T", "SM-4-point.nb / input 1131 / target 1"}, 
 "Masses" -> {Mass[1] -> MW, Mass[2] -> MW, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])/
    (MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/
    (2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/
    (MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (2*CW^2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*CW^2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 3])) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 3])) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (CW^2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 3])) + 
   (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
    (MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 4]])/
    (2*CW*MZ^3*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 4]])/
    (2*CW*MZ^3*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 4]])/
    (2*CW*MZ^3*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 4]])/
    (2*CW*MZ^3*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 4], Mom[3], Spinor["Spin", "Angle", 1]])/
    (2*CW*MZ^3*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 4], Mom[3], Spinor["Spin", "Angle", 1]])/
    (2*CW*MZ^3*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 4], Mom[3], Spinor["Spin", "Angle", 1]])/
    (2*CW*MZ^3*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 4], Mom[3], Spinor["Spin", "Angle", 1]])/
    (2*CW*MZ^3*SW^2*(MZ^2 - Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 141 *)
<|"Name" -> "SM-4-point-test.nb / W+ W+ -> W- W- / Z:U", 
 "Sources" -> {"SM-4-point-test.nb / W+ W+ -> W- W- / Z:U", "SM-4-point.nb / input 1140 / target 1"}, 
 "Masses" -> {Mass[1] -> MW, Mass[2] -> MW, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (-3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])/
    (2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/
    (MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/
    (MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 4])) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (2*CW^2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 4])) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (CW^2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*CW^2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
    (MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]])/
    (2*CW*MZ^3*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]])/
    (2*CW*MZ^3*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]])/
    (2*CW*MZ^3*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]])/
    (2*CW*MZ^3*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]])/
    (2*CW*MZ^3*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]])/
    (2*CW*MZ^3*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]])/
    (2*CW*MZ^3*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]])/
    (2*CW*MZ^3*SW^2*(MZ^2 - Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 142 *)
<|"Name" -> "SM-4-point-test.nb / W+ W+ -> W- W- / A:T", 
 "Sources" -> {"SM-4-point-test.nb / W+ W+ -> W- W- / A:T", "SM-4-point.nb / input 1172 / target 1"}, 
 "Masses" -> {Mass[1] -> MW, Mass[2] -> MW, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> ((-2*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])/Mandelstahm[1, 3] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 3] + 
     (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 3] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/Mandelstahm[1, 3] + 
     (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/Mandelstahm[1, 3] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/Mandelstahm[1, 3] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 3] + 
     (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 3] - 
     (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 3] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
       SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 3] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 3] + 
     (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 3] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 3] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 3] - 
     (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 3])/MW^2 + 
   (((EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
         SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
         SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/Mandelstahm[1, 3] + 
       (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
         SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
         SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 3] + 
       (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
         SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
         SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 3] + 
       (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
         SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
         SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 3])*
      SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 4]] + 
     ((EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
         SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
         SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])/Mandelstahm[1, 3] + 
       (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
         SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
         SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/Mandelstahm[1, 3] + 
       (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
         SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
         SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/Mandelstahm[1, 3] + 
       (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
         SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
         SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 3])*
      SpinorChain[Spinor["Spin", "Square", 4], Mom[3], Spinor["Spin", "Angle", 1]])/MW^3|>,

(* ::Subsection::Closed:: *)
(* Reference 143 *)
<|"Name" -> "SM-4-point-test.nb / W+ W+ -> W- W- / A:U", 
 "Sources" -> {"SM-4-point-test.nb / W+ W+ -> W- W- / A:U", "SM-4-point.nb / input 1184 / target 1"}, 
 "Masses" -> {Mass[1] -> MW, Mass[2] -> MW, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> ((EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])/Mandelstahm[1, 4] + 
     (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 4] + 
     (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 4] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/Mandelstahm[1, 4] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/Mandelstahm[1, 4] - 
     (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/Mandelstahm[1, 4] + 
     (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/Mandelstahm[1, 4] - 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
       SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/Mandelstahm[1, 4] + 
     (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 4] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 4] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 4] + 
     (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 4] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 4] - 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 4] + 
     (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 4])/MW^2 + 
   ((EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]])/Mandelstahm[1, 4] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]])/Mandelstahm[1, 4] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]])/Mandelstahm[1, 4] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]])/Mandelstahm[1, 4] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]])/Mandelstahm[1, 4] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]])/Mandelstahm[1, 4] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]])/Mandelstahm[1, 4] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]])/Mandelstahm[1, 4])/MW^3|>,

(* ::Subsection::Closed:: *)
(* Reference 144 *)
<|"Name" -> "SM-4-point.nb / input 57 / target 1", 
 "Sources" -> {"SM-4-point.nb / input 57 / target 1", "SM-4-point.nb / input 63 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Mu, Mass[4] -> Mu}, 
 "Amplitude" -> (-8*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/(9*Mandelstahm[1, 2])|>,

(* ::Subsection::Closed:: *)
(* Reference 145 *)
<|"Name" -> "SM-4-point.nb / input 59 / target 1", 
 "Sources" -> {"SM-4-point.nb / input 59 / target 1", "SM-4-point.nb / input 66 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Mu, Mass[4] -> Mu}, 
 "Amplitude" -> (8*EE^2*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] - 
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] - 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/(9*Mandelstahm[1, 3])|>,

(* ::Subsection::Closed:: *)
(* Reference 146 *)
<|"Name" -> "SM-4-point.nb / input 69 / target 1", "Sources" -> {"SM-4-point.nb / input 69 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Mu, Mass[4] -> Mu}, 
 "Amplitude" -> (EE^2*Mu^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
    (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
   (4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 147 *)
<|"Name" -> "SM-4-point.nb / input 71 / target 1", "Sources" -> {"SM-4-point.nb / input 71 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Mu, Mass[4] -> Mu}, 
 "Amplitude" -> -1/4*(EE^2*Mu^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]] + 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])*
     (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
    (MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 148 *)
<|"Name" -> "SM-4-point.nb / input 74 / target 1", "Sources" -> {"SM-4-point.nb / input 74 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Mu, Mass[4] -> Mu}, 
 "Amplitude" -> -1/2*(EE^2*(gLu*gRu*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
       gRu^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
       gLu^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
       gLu*gRu*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
     (CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) - 
   (EE^2*(gLu - gRu)^2*Mu^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 149 *)
<|"Name" -> "SM-4-point.nb / input 76 / target 1", "Sources" -> {"SM-4-point.nb / input 76 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Mu, Mass[4] -> Mu}, 
 "Amplitude" -> (EE^2*(gLu - gRu)^2*Mu^2*(SpinorChain[Spinor["Spin", "Angle", 1], 
       Spinor["Spin", "Angle", 3]] - SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])*
     (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 3])) + 
   (EE^2*(-(gRu^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]) - 
      gLu^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      gLu*gRu*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
         SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] + 
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
         SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])))/
    (2*CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 150 *)
<|"Name" -> "SM-4-point.nb / input 136 / target 1", "Sources" -> {"SM-4-point.nb / input 136 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Ms, Mass[4] -> Ms}, 
 "Amplitude" -> (4*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/(9*Mandelstahm[1, 2])|>,

(* ::Subsection::Closed:: *)
(* Reference 151 *)
<|"Name" -> "SM-4-point.nb / input 151 / target 1", 
 "Sources" -> {"SM-4-point.nb / input 151 / target 1", "SM-4-point.nb / input 157 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Md, Mass[4] -> Md}, 
 "Amplitude" -> (-2*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/(9*Mandelstahm[1, 2])|>,

(* ::Subsection::Closed:: *)
(* Reference 152 *)
<|"Name" -> "SM-4-point.nb / input 153 / target 1", "Sources" -> {"SM-4-point.nb / input 153 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Md, Mass[4] -> Md}, 
 "Amplitude" -> (2*EE^2*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] - 
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] - 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/(9*Mandelstahm[1, 3])|>,

(* ::Subsection::Closed:: *)
(* Reference 153 *)
<|"Name" -> "SM-4-point.nb / input 160 / target 1", "Sources" -> {"SM-4-point.nb / input 160 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Md, Mass[4] -> Md}, 
 "Amplitude" -> (EE^2*Md^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
    (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
   (4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 154 *)
<|"Name" -> "SM-4-point.nb / input 162 / target 1", "Sources" -> {"SM-4-point.nb / input 162 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Md, Mass[4] -> Md}, 
 "Amplitude" -> -1/4*(EE^2*Md^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]] + 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])*
     (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
    (MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 155 *)
<|"Name" -> "SM-4-point.nb / input 165 / target 1", "Sources" -> {"SM-4-point.nb / input 165 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Md, Mass[4] -> Md}, 
 "Amplitude" -> -1/2*(EE^2*(gLd*gRd*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
       gRd^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
       gLd^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
       gLd*gRd*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
     (CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) - 
   (EE^2*(gLd - gRd)^2*Md^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 156 *)
<|"Name" -> "SM-4-point.nb / input 167 / target 1", "Sources" -> {"SM-4-point.nb / input 167 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Md, Mass[4] -> Md}, 
 "Amplitude" -> (EE^2*(gLd - gRd)^2*Md^2*(SpinorChain[Spinor["Spin", "Angle", 1], 
       Spinor["Spin", "Angle", 3]] - SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])*
     (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 3])) + 
   (EE^2*(-(gRd^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]) - 
      gLd^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      gLd*gRd*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
         SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] + 
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
         SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])))/
    (2*CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 157 *)
<|"Name" -> "SM-4-point.nb / input 243 / target 1", 
 "Sources" -> {"SM-4-point.nb / input 243 / target 1", "SM-4-point.nb / input 249 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> Me, Mass[4] -> Me}, 
 "Amplitude" -> (-2*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/Mandelstahm[1, 2]|>,

(* ::Subsection::Closed:: *)
(* Reference 158 *)
<|"Name" -> "SM-4-point.nb / input 245 / target 1", 
 "Sources" -> {"SM-4-point.nb / input 245 / target 1", "SM-4-point.nb / input 252 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> Me, Mass[4] -> Me}, 
 "Amplitude" -> (2*EE^2*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] - 
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] - 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/Mandelstahm[1, 3]|>,

(* ::Subsection::Closed:: *)
(* Reference 159 *)
<|"Name" -> "SM-4-point.nb / input 255 / target 1", "Sources" -> {"SM-4-point.nb / input 255 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> Me, Mass[4] -> Me}, 
 "Amplitude" -> (EE^2*Me^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
    (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
   (4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 160 *)
<|"Name" -> "SM-4-point.nb / input 257 / target 1", "Sources" -> {"SM-4-point.nb / input 257 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> Me, Mass[4] -> Me}, 
 "Amplitude" -> -1/4*(EE^2*Me^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]] + 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])*
     (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
    (MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 161 *)
<|"Name" -> "SM-4-point.nb / input 260 / target 1", "Sources" -> {"SM-4-point.nb / input 260 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> Me, Mass[4] -> Me}, 
 "Amplitude" -> -1/2*(EE^2*(gLe*gRe*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
       gRe^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
       gLe^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
       gLe*gRe*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
     (CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) - 
   (EE^2*(gLe - gRe)^2*Me^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 162 *)
<|"Name" -> "SM-4-point.nb / input 262 / target 1", "Sources" -> {"SM-4-point.nb / input 262 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> Me, Mass[4] -> Me}, 
 "Amplitude" -> (EE^2*(gLe - gRe)^2*Me^2*(SpinorChain[Spinor["Spin", "Angle", 1], 
       Spinor["Spin", "Angle", 3]] - SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])*
     (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 3])) + 
   (EE^2*(-(gRe^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]) - 
      gLe^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      gLe*gRe*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
         SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] + 
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
         SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])))/
    (2*CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 163 *)
<|"Name" -> "SM-4-point.nb / input 287 / target 1", "Sources" -> {"SM-4-point.nb / input 287 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> 0, Mass[3] -> 0, Mass[4] -> 0}, 
 "Amplitude" -> -1/2*(EE^2*SpinorChain[Spinor["Helicity", "Angle", 1], Spinor["Helicity", "Angle", 4]]*
     SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Helicity", "Square", 3]])/
    (CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 164 *)
<|"Name" -> "SM-4-point.nb / input 295 / target 1", "Sources" -> {"SM-4-point.nb / input 295 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Md, Mass[3] -> Mt, Mass[4] -> Mb}, 
 "Amplitude" -> -1/2*(EE^2*(Md*Mt*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
       SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      Mt*Mu*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] + 
      2*MW^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] - 
      Mb*Md*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
      Mb*Mu*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (MW^2*SW^2*(-MW^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 165 *)
<|"Name" -> "SM-4-point.nb / input 302 / target 1", "Sources" -> {"SM-4-point.nb / input 302 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Mu, Mass[3] -> Ml, Mass[4] -> 0}, 
 "Amplitude" -> -1/2*(EE^2*(Ml*Mu*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
       SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Helicity", "Angle", 4]] - 
      Md*Ml*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Helicity", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] + 
      2*MW^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]))/
    (MW^2*SW^2*(-MW^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 166 *)
<|"Name" -> "SM-4-point.nb / input 310 / target 1", "Sources" -> {"SM-4-point.nb / input 310 / target 1"}, 
 "Masses" -> {Mass[1] -> Mm, Mass[2] -> 0, Mass[3] -> Me, Mass[4] -> 0}, 
 "Amplitude" -> -1/2*(EE^2*(2*MW^2*SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle", 4]] - 
      Me*Mm*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Helicity", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square", 2]]))/
    (MW^2*SW^2*(-MW^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 167 *)
<|"Name" -> "SM-4-point.nb / input 324 / target 1", "Sources" -> {"SM-4-point.nb / input 324 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> Mh, Mass[4] -> Mh}, 
 "Amplitude" -> -1/4*(EE^2*Me^2*(2*Me*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]) + 
      SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 2]] + 
      SpinorChain[Spinor["Spin", "Square", 2], Mom[3], Spinor["Spin", "Angle", 1]]))/
    (MW^2*SW^2*(-Me^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 168 *)
<|"Name" -> "SM-4-point.nb / input 326 / target 1", "Sources" -> {"SM-4-point.nb / input 326 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> Mh, Mass[4] -> Mh}, 
 "Amplitude" -> -1/4*(EE^2*Me^2*(2*Me*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]) + 
      SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 2]] + 
      SpinorChain[Spinor["Spin", "Square", 2], Mom[4], Spinor["Spin", "Angle", 1]]))/
    (MW^2*SW^2*(-Me^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 169 *)
<|"Name" -> "SM-4-point.nb / input 334 / target 1", "Sources" -> {"SM-4-point.nb / input 334 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Mh, Mass[4] -> Mh}, 
 "Amplitude" -> -1/4*(EE^2*Mu^2*(2*Mu*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]) + 
      SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 2]] + 
      SpinorChain[Spinor["Spin", "Square", 2], Mom[3], Spinor["Spin", "Angle", 1]]))/
    (MW^2*SW^2*(-Mu^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 170 *)
<|"Name" -> "SM-4-point.nb / input 336 / target 1", "Sources" -> {"SM-4-point.nb / input 336 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Mh, Mass[4] -> Mh}, 
 "Amplitude" -> -1/4*(EE^2*Mu^2*(2*Mu*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]) + 
      SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 2]] + 
      SpinorChain[Spinor["Spin", "Square", 2], Mom[4], Spinor["Spin", "Angle", 1]]))/
    (MW^2*SW^2*(-Mu^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 171 *)
<|"Name" -> "SM-4-point.nb / input 344 / target 1", "Sources" -> {"SM-4-point.nb / input 344 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Mh, Mass[4] -> Mh}, 
 "Amplitude" -> -1/4*(EE^2*Md^2*(2*Md*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]) + 
      SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 2]] + 
      SpinorChain[Spinor["Spin", "Square", 2], Mom[3], Spinor["Spin", "Angle", 1]]))/
    (MW^2*SW^2*(-Md^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 172 *)
<|"Name" -> "SM-4-point.nb / input 346 / target 1", "Sources" -> {"SM-4-point.nb / input 346 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Mh, Mass[4] -> Mh}, 
 "Amplitude" -> -1/4*(EE^2*Md^2*(2*Md*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]) + 
      SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 2]] + 
      SpinorChain[Spinor["Spin", "Square", 2], Mom[4], Spinor["Spin", "Angle", 1]]))/
    (MW^2*SW^2*(-Md^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 173 *)
<|"Name" -> "SM-4-point.nb / input 393 / target 1", "Sources" -> {"SM-4-point.nb / input 393 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> 0, Mass[3] -> MZ, Mass[4] -> Mh}, 
 "Amplitude" -> (EE^2*SpinorChain[Spinor["Helicity", "Angle", 1], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Spin", "Square", 3]])/
   (Sqrt[2]*CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 174 *)
<|"Name" -> "SM-4-point.nb / input 463 / target 1", "Sources" -> {"SM-4-point.nb / input 463 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> 0, Mass[4] -> 0}, 
 "Amplitude" -> (-2*GG^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]])*
    SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle", 4]])/
   ((-Mu^2 + Mandelstahm[1, 3])*(-Mu^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 175 *)
<|"Name" -> "SM-4-point.nb / input 541 / target 1", "Sources" -> {"SM-4-point.nb / input 541 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> -1/2*(EE^2*gLe*gRe*Me*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]] + 
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
     (MW^2*SW^2*(Me^2 - Mandelstahm[1, 3])) - 
   (EE^2*gLe^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     (MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]))/
    (2*MW^2*SW^2*(Me^2 - Mandelstahm[1, 3])) - 
   (EE^2*gRe^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     (MZ*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]]))/
    (2*MW^2*SW^2*(Me^2 - Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 176 *)
<|"Name" -> "SM-4-point.nb / input 543 / target 1", "Sources" -> {"SM-4-point.nb / input 543 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> -1/2*(EE^2*gLe*gRe*Me*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
     (MW^2*SW^2*(-Me^2 + Mandelstahm[1, 4])) - 
   (EE^2*gRe^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     (MZ*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]))/
    (2*MW^2*SW^2*(-Me^2 + Mandelstahm[1, 4])) - 
   (EE^2*gLe^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     (MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]]))/
    (2*MW^2*SW^2*(-Me^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 177 *)
<|"Name" -> "SM-4-point.nb / input 548 / target 1", "Sources" -> {"SM-4-point.nb / input 548 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> 0, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> -1/2*(EE^2*SpinorChain[Spinor["Helicity", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Spin", "Square", 3]]*
     (MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]]))/
    (MW^2*SW^2*Mandelstahm[1, 4])|>,

(* ::Subsection::Closed:: *)
(* Reference 178 *)
<|"Name" -> "SM-4-point.nb / input 550 / target 1", "Sources" -> {"SM-4-point.nb / input 550 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> 0, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> (EE^2*SpinorChain[Spinor["Helicity", "Angle", 1], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Spin", "Square", 4]]*
    (MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
     SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]))/
   (2*MW^2*SW^2*Mandelstahm[1, 3])|>,

(* ::Subsection::Closed:: *)
(* Reference 179 *)
<|"Name" -> "SM-4-point.nb / input 559 / target 1", "Sources" -> {"SM-4-point.nb / input 559 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> -1/2*(EE^2*gLu*gRu*Mu*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]] + 
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
     (MW^2*SW^2*(Mu^2 - Mandelstahm[1, 3])) - 
   (EE^2*gLu^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     (MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]))/
    (2*MW^2*SW^2*(Mu^2 - Mandelstahm[1, 3])) - 
   (EE^2*gRu^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     (MZ*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]]))/
    (2*MW^2*SW^2*(Mu^2 - Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 180 *)
<|"Name" -> "SM-4-point.nb / input 562 / target 1", "Sources" -> {"SM-4-point.nb / input 562 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> -1/2*(EE^2*gLu*gRu*Mu*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
     (MW^2*SW^2*(-Mu^2 + Mandelstahm[1, 4])) - 
   (EE^2*gRu^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     (MZ*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]))/
    (2*MW^2*SW^2*(-Mu^2 + Mandelstahm[1, 4])) - 
   (EE^2*gLu^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     (MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]]))/
    (2*MW^2*SW^2*(-Mu^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 181 *)
<|"Name" -> "SM-4-point.nb / input 570 / target 1", "Sources" -> {"SM-4-point.nb / input 570 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> -1/2*(EE^2*gLd*gRd*Md*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]] + 
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
     (MW^2*SW^2*(Md^2 - Mandelstahm[1, 3])) - 
   (EE^2*gLd^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     (MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]))/
    (2*MW^2*SW^2*(Md^2 - Mandelstahm[1, 3])) - 
   (EE^2*gRd^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     (MZ*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]]))/
    (2*MW^2*SW^2*(Md^2 - Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 182 *)
<|"Name" -> "SM-4-point.nb / input 572 / target 1", "Sources" -> {"SM-4-point.nb / input 572 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> -1/2*(EE^2*gLd*gRd*Md*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
     (MW^2*SW^2*(-Md^2 + Mandelstahm[1, 4])) - 
   (EE^2*gRd^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     (MZ*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]))/
    (2*MW^2*SW^2*(-Md^2 + Mandelstahm[1, 4])) - 
   (EE^2*gLd^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     (MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]]))/
    (2*MW^2*SW^2*(-Md^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 183 *)
<|"Name" -> "SM-4-point.nb / input 716 / target 1", "Sources" -> {"SM-4-point.nb / input 716 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> Md, Mass[3] -> 0, Mass[4] -> Md}, 
 "Amplitude" -> (-2*EE*GG*(-(SpinorChain[Spinor["Helicity", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 2]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Mom[2], Spinor["Helicity", "Angle", 3]]) + 
     SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 4]]*
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle", 3]]*
      SpinorChain[Spinor["Helicity", "Square", 1], Mom[2], Spinor["Helicity", "Angle", 3]]))/
   (3*(-Md^2 + Mandelstahm[1, 2])*(-Md^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 184 *)
<|"Name" -> "SM-4-point.nb / input 727 / target 1", "Sources" -> {"SM-4-point.nb / input 727 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> Mu, Mass[3] -> 0, Mass[4] -> Mu}, 
 "Amplitude" -> (2*GG^2*(-(SpinorChain[Spinor["Helicity", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 2]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Mom[2], Spinor["Helicity", "Angle", 3]]) + 
     SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 4]]*
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle", 3]]*
      SpinorChain[Spinor["Helicity", "Square", 1], Mom[2], Spinor["Helicity", "Angle", 3]]))/
   ((-Mu^2 + Mandelstahm[1, 2])*(-Mu^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 185 *)
<|"Name" -> "SM-4-point.nb / input 739 / target 1", "Sources" -> {"SM-4-point.nb / input 739 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> Md, Mass[3] -> 0, Mass[4] -> Md}, 
 "Amplitude" -> (2*GG^2*Md*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Helicity", "Square", 3]]^2*
    SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]])/
   ((-Md^2 + Mandelstahm[1, 2])*(-Md^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 186 *)
<|"Name" -> "SM-4-point.nb / input 743 / target 1", "Sources" -> {"SM-4-point.nb / input 743 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> Md, Mass[3] -> 0, Mass[4] -> Md}, 
 "Amplitude" -> (2*GG^2*(-(SpinorChain[Spinor["Helicity", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 2]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Mom[2], Spinor["Helicity", "Angle", 3]]) + 
     SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 4]]*
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle", 3]]*
      SpinorChain[Spinor["Helicity", "Square", 1], Mom[2], Spinor["Helicity", "Angle", 3]]))/
   ((-Md^2 + Mandelstahm[1, 2])*(-Md^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 187 *)
<|"Name" -> "SM-4-point.nb / input 839 / target 1", "Sources" -> {"SM-4-point.nb / input 839 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> 0, Mass[3] -> 0, Mass[4] -> MW}, 
 "Amplitude" -> -((Sqrt[2]*EE^2*(Me*SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle", 2]] + 
      MW*SpinorChain[Spinor["Helicity", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square", 3]])*
     SpinorChain[Spinor["Helicity", "Square", 3], Mom[2], Spinor["Spin", "Angle", 4]])/
    (MW*SW*(-MW^2 + Mandelstahm[1, 2])*(-Me^2 + Mandelstahm[1, 3])))|>,

(* ::Subsection::Closed:: *)
(* Reference 188 *)
<|"Name" -> "SM-4-point.nb / input 893 / target 1", "Sources" -> {"SM-4-point.nb / input 893 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Mu, Mass[3] -> MZ, Mass[4] -> MW}, 
 "Amplitude" -> (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
    (gRu*Mu*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
     gLu*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
      (MZ*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
       SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]])))/
   (Sqrt[2]*MW^2*SW^2*(-Mu^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 189 *)
<|"Name" -> "SM-4-point.nb / input 898 / target 1", "Sources" -> {"SM-4-point.nb / input 898 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Mu, Mass[3] -> MZ, Mass[4] -> MW}, 
 "Amplitude" -> -((EE^2*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     (gRd*Md*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      gLd*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       (MW*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] - 
        SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]])))/
    (Sqrt[2]*MW^2*SW^2*(-Md^2 + Mandelstahm[1, 4])))|>,

(* ::Subsection::Closed:: *)
(* Reference 190 *)
<|"Name" -> "SM-4-point.nb / input 997 / target 1", "Sources" -> {"SM-4-point.nb / input 997 / target 1"}, 
 "Masses" -> {Mass[1] -> MZ, Mass[2] -> Mh, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (EE^2*(2*MW^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
     2*MW^2*MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
     2*MW^2*MZ*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] - 
     MZ^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
      SpinorChain[Spinor["Spin", "Square", 4], Mom[2], Spinor["Spin", "Angle", 4]] + 
     2*MW^2*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
      SpinorChain[Spinor["Spin", "Square", 4], Mom[3], Spinor["Spin", "Angle", 1]]))/
   (Sqrt[2]*MW^2*MZ^2*SW^2*(-MW^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 191 *)
<|"Name" -> "SM-4-point.nb / input 1002 / target 1", "Sources" -> {"SM-4-point.nb / input 1002 / target 1"}, 
 "Masses" -> {Mass[1] -> MZ, Mass[2] -> Mh, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (EE^2*(2*MW^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
     2*MW^2*MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
     2*MW^2*MZ*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
     MZ^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
      SpinorChain[Spinor["Spin", "Square", 3], Mom[2], Spinor["Spin", "Angle", 3]] + 
     2*MW^2*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
      SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]]))/
   (Sqrt[2]*MW^2*MZ^2*SW^2*(-MW^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 192 *)
<|"Name" -> "SM-4-point.nb / input 1103 / target 1", "Sources" -> {"SM-4-point.nb / input 1103 / target 1"}, 
 "Masses" -> {Mass[1] -> MZ, Mass[2] -> MZ, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (-2*CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])/
    (MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (3*CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (2*CW^2*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/
    (MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (3*CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (3*CW^2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (3*CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (3*CW^2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) - 
   (3*CW^2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (CW^2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) - 
   (CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (2*CW^2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) - 
   (2*CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
    (MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (3*CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 4]])/
    (2*MW^3*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (3*CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 4]])/
    (2*MW^3*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 4]])/
    (2*MW^3*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 4]])/
    (2*MW^3*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (3*CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 4], Mom[3], Spinor["Spin", "Angle", 1]])/
    (2*MW^3*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (3*CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 4], Mom[3], Spinor["Spin", "Angle", 1]])/
    (2*MW^3*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 4], Mom[3], Spinor["Spin", "Angle", 1]])/
    (2*MW^3*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 4], Mom[3], Spinor["Spin", "Angle", 1]])/
    (2*MW^3*SW^2*(MW^2 - Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 193 *)
<|"Name" -> "SM-4-point.nb / input 1112 / target 1", "Sources" -> {"SM-4-point.nb / input 1112 / target 1"}, 
 "Masses" -> {Mass[1] -> MZ, Mass[2] -> MZ, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (3*CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (2*CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/
    (MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (2*CW^2*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/
    (MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (3*CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) - 
   (3*CW^2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (3*CW^2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) - 
   (CW^2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (3*CW^2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (3*CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (2*CW^2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (2*CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
    (MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (3*CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]])/
    (2*MW^3*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]])/
    (2*MW^3*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (3*CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]])/
    (2*MW^3*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]])/
    (2*MW^3*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (3*CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]])/
    (2*MW^3*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]])/
    (2*MW^3*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (3*CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]])/
    (2*MW^3*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]])/
    (2*MW^3*SW^2*(MW^2 - Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 194 *)
<|"Name" -> "SM-4-point.nb / input 1150 / target 1", "Sources" -> {"SM-4-point.nb / input 1150 / target 1"}, 
 "Masses" -> {Mass[1] -> MW, Mass[2] -> MW, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (-2*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])/(CW^2*MZ^2*Mandelstahm[1, 3]) + 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/(2*CW^2*MZ^2*Mandelstahm[1, 3]) + 
   (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/(CW^2*MZ^2*Mandelstahm[1, 3]) + 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/(2*CW^2*MZ^2*Mandelstahm[1, 3]) + 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/(2*CW^2*MZ^2*Mandelstahm[1, 3]) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/(2*CW^2*MZ^2*Mandelstahm[1, 3]) + 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/(2*CW^2*MZ^2*Mandelstahm[1, 3]) + 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/(2*CW^2*MZ^2*Mandelstahm[1, 3]) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/(CW^2*MZ^2*Mandelstahm[1, 3]) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/(CW^2*MZ^2*Mandelstahm[1, 3]) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/(2*CW^2*MZ^2*Mandelstahm[1, 3]) + 
   (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/(CW^2*MZ^2*Mandelstahm[1, 3]) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/(2*CW^2*MZ^2*Mandelstahm[1, 3]) - 
   (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/(CW^2*MZ^2*Mandelstahm[1, 3]) + 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 4]])/
    (2*CW^3*MZ^3*Mandelstahm[1, 3]) + (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], 
      Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 4]])/
    (2*CW^3*MZ^3*Mandelstahm[1, 3]) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 4]])/
    (2*CW^3*MZ^3*Mandelstahm[1, 3]) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 4]])/
    (2*CW^3*MZ^3*Mandelstahm[1, 3]) + (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], 
      Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 4], Mom[3], Spinor["Spin", "Angle", 1]])/
    (2*CW^3*MZ^3*Mandelstahm[1, 3]) + (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], 
      Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 4], Mom[3], Spinor["Spin", "Angle", 1]])/
    (2*CW^3*MZ^3*Mandelstahm[1, 3]) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 4], Mom[3], Spinor["Spin", "Angle", 1]])/
    (2*CW^3*MZ^3*Mandelstahm[1, 3]) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 4], Mom[3], Spinor["Spin", "Angle", 1]])/
    (2*CW^3*MZ^3*Mandelstahm[1, 3])|>,

(* ::Subsection::Closed:: *)
(* Reference 195 *)
<|"Name" -> "SM-4-point.nb / input 1159 / target 1", "Sources" -> {"SM-4-point.nb / input 1159 / target 1"}, 
 "Masses" -> {Mass[1] -> MW, Mass[2] -> MW, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])/(2*CW^2*MZ^2*Mandelstahm[1, 4]) + 
   (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/(CW^2*MZ^2*Mandelstahm[1, 4]) + 
   (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/(CW^2*MZ^2*Mandelstahm[1, 4]) + 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/(2*CW^2*MZ^2*Mandelstahm[1, 4]) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/(2*CW^2*MZ^2*Mandelstahm[1, 4]) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/(CW^2*MZ^2*Mandelstahm[1, 4]) + 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/(2*CW^2*MZ^2*Mandelstahm[1, 4]) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/(CW^2*MZ^2*Mandelstahm[1, 4]) + 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/(2*CW^2*MZ^2*Mandelstahm[1, 4]) + 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/(2*CW^2*MZ^2*Mandelstahm[1, 4]) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/(2*CW^2*MZ^2*Mandelstahm[1, 4]) + 
   (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/(CW^2*MZ^2*Mandelstahm[1, 4]) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/(2*CW^2*MZ^2*Mandelstahm[1, 4]) + 
   (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/(CW^2*MZ^2*Mandelstahm[1, 4]) + 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]])/
    (2*CW^3*MZ^3*Mandelstahm[1, 4]) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]])/
    (2*CW^3*MZ^3*Mandelstahm[1, 4]) + (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], 
      Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]])/
    (2*CW^3*MZ^3*Mandelstahm[1, 4]) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]])/
    (2*CW^3*MZ^3*Mandelstahm[1, 4]) + (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], 
      Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]])/
    (2*CW^3*MZ^3*Mandelstahm[1, 4]) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]])/
    (2*CW^3*MZ^3*Mandelstahm[1, 4]) + (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], 
      Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]])/
    (2*CW^3*MZ^3*Mandelstahm[1, 4]) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]])/
    (2*CW^3*MZ^3*Mandelstahm[1, 4])|>
},

(* ::Subsection::Closed:: *)
(* Synthetic Five-Point Algebraic Fixtures *)
Module[{families, familyNames},
  families = {{0, 0, 0, 0, 0}, {Me, Me, 0, 0, 0}, {Me, Me, Mm, Mm, 0},
    {MW, MW, MW, MW, 0}, {Mh, MW, MZ, Me, Me}, {MW, MW, MZ, MZ, Mh}};
  familyNames = {"Massless", "FermionPair", "TwoFermionPairs", "FourVectors",
    "MixedMassive", "FiveMassive"};
  Flatten[Table[Module[{masses = families[[family]], spinor, bracket, chain, expressions,
      channelPairs, channelMasses, channelIndices, denominators, templateNames, externalRules},
    spinor[type_, leg_] := Spinor[If[masses[[leg]] === 0, "Helicity", "Spin"], type, leg];
    bracket[type_, left_, right_] := SpinorChain[spinor[type, left], spinor[type, right]];
    chain[leftType_, left_, momenta_List, rightType_, right_] :=
      SpinorChain @@ Join[{spinor[leftType, left]}, Mom /@ momenta, {spinor[rightType, right]}];
    expressions = {
      bracket["Angle", 1, 2] bracket["Angle", 3, 4] bracket["Square", 2, 5],
      bracket["Square", 1, 2] bracket["Square", 3, 4] bracket["Angle", 1, 5],
      chain["Angle", 1, {3}, "Square", 2] bracket["Angle", 4, 5],
      chain["Square", 1, {2, 3}, "Square", 4] bracket["Angle", 2, 5],
      chain["Angle", 1, {2, 5, 3}, "Square", 4],
      MomProd[2, 5] bracket["Angle", 1, 3] bracket["Square", 2, 4] +
        MomProd[3, 5] bracket["Angle", 1, 4] bracket["Square", 2, 3],
      chain["Angle", 1, {Multiparticle[2, 3]}, "Square", 4] bracket["Angle", 2, 5],
      (Mom[Multiparticle[2, 3]]^2 - Mass[2]^2 - Mass[3]^2)
        bracket["Angle", 1, 4] bracket["Square", 2, 5]
    };
    channelPairs = {{1, 2}, {2, 3}, {4, 5}}; channelMasses = {0, MZ, Mh};
    channelIndices = {{1}, {}, {2}, {3}, {1, 3}, {2}, {2, 3}, {2}};
    denominators = {PropDen[Mom[Multiparticle[1, 2]], 0], 1,
      PropDen[Mom[Multiparticle[2, 3]], MZ], PropDen[Mom[Multiparticle[4, 5]], Mh],
      PropDen[Mom[Multiparticle[1, 2]], 0] PropDen[Mom[Multiparticle[4, 5]], Mh],
      PropDen[Mom[Multiparticle[2, 3]], MZ],
      PropDen[Mom[Multiparticle[2, 3]], MZ] PropDen[Mom[Multiparticle[4, 5]], Mh],
      PropDen[Mom[Multiparticle[2, 3]], MZ]};
    templateNames = {"AngleBracketProduct", "SquareBracketProduct", "OneMomentumChain",
      "TwoMomentumChain", "ThreeMomentumChain", "DotProductSum", "InternalMomentumChain",
      "InternalMomentumSquare"};
    externalRules = Thread[(Mass /@ Range[5]) -> masses];
    Table[With[{name = "Synthetic5Point / " <> familyNames[[family]] <> " / " <> templateNames[[template]]},
      <|"Name" -> name, "Sources" -> {name}, "Synthetic" -> True, "ExternalLegCount" -> 5,
        "Family" -> "Synthetic5Point / " <> familyNames[[family]], "Template" -> templateNames[[template]],
        "Masses" -> Join[externalRules,
          (Mass[Multiparticle @@ channelPairs[[#]]] -> channelMasses[[#]] & /@ channelIndices[[template]])],
        "Amplitude" -> (-1)^(family + template) (family + template) EE^3
          expressions[[template]]/((1 + Mod[template, 3]) denominators[[template]])|>],
      {template, Length[expressions]}]
  ], {family, Length[families]}], 1]
]]
