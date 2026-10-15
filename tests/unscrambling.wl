(* ::Package:: *)

(* Neural-guided simplification, legal identity scrambling, and training.

   Stored numerators have their mass rules already applied, so Mass[1]
   is Me (or 0, MW, MZ, ...). PropDen factors remain opaque. After each identity
   the expression is expanded. RecordSteps can retain intermediate
   expressions and exact edit records for supervised reverse trajectories.
*)


(* ::Subsection::Closed:: *)
(*Package and Public Functions*)

Quiet[ClearAll["Unscrambling`*", "Unscrambling`Private`*"]];

BeginPackage["Unscrambling`", {"ConstructiveDiagrams`"}];

ComplicateAmplitude::usage = "ComplicateAmplitude[{massRules, amplitude}, nSteps] or ComplicateAmplitude[{massRules, amplitude}, nSteps, seed] applies legal identities. Returns MassRules, Original, Expression and Steps. The string option RecordSteps -> True also includes Trajectory; RandomSeed, OnShellChannels and MomentumConservation are supported.";
TrainUnscrambleNet::usage = "TrainUnscrambleNet[{amp1, amp2, ...}, opts] trains a shared-state candidate scorer. Kernels selects serial execution (1), owned local workers (n or Automatic), or an explicit connected worker list. Data generation and validation run in parallel; neural optimization remains on the coordinator. String options include DataCacheDirectory, WorkerRoot, WorkerThreads, TrainingBatchSize, JobTimeLimit, ValidationTimeLimit, ModelDirectory, OnShellChannels and MomentumConservation. Saves the current unscramble.wlnet and unscramble.m files.";
UnscrambleSpinorAmplitudes::usage = "UnscrambleSpinorAmplitudes[{massRules, amplitude}] takes one diagram amplitude, as returned by diagramAmplitudes, and rewrites it with the trained net until the expression stops simplifying. diagramAmplitude's expression is the second element; the mass rules are the first.";
SpinorExpressionComplexity::usage = "SpinorExpressionComplexity[expr] is the score used to decide whether an expression got simpler. Fewer terms, momentum insertions, and momentum products give a lower score. Composite momentum slots count their implicit expanded terms; PropDen factors remain opaque.";
SchoutenRewrite::usage = "SchoutenRewrite[c1, c2, {m, n}] rewrites a product of spinor chains using Decay Rates, Appendix B, Eq. (B11). m and n count momenta before the split in each chain. Invalid chirality or splits return $Failed.";
SchoutenCandidates::usage = "SchoutenCandidates[expr] returns exact generalized Schouten edits with Subtract, Insert, Chains and Splits. Chain reversal is canonicalized for cancellation. No mass or on-shell assumption is used.";
UnscrambleTrace::usage = "UnscrambleTrace[{massRules, amplitude}] runs the trained policy and returns its guesses, changed expressions, complexity scores and accepted checkpoints, together with Result.";
UnscrambleCandidates::usage = "UnscrambleCandidates[{massRules, amplitude}] lists legal concrete edits and Stop for the canonicalized expression. Supports arbitrary integer leg labels. Options: OnShellChannels (default Automatic, inferred from internal mass rules), MomentumConservation (default True), both string keys.";
UnscrambleEncoding::usage = "UnscrambleEncoding[{massRules, amplitude}, candidate] returns an association with State and Candidate UTF-8 byte-ID sequences for the candidate policy. Both are complete; no truncation or learned vocabulary cutoff is applied.";
TrainUnscrambleNet::usage = TrainUnscrambleNet::usage <> " PolicyMethod -> ReverseMoves (string keys and values) trains an experimental state-only reverse-action predictor in separate unscramble-moves model files; CandidateScorer is the default.";
TrainUnscrambleNet::usage = TrainUnscrambleNet::usage <> " SimplificationRoutes supplies curated associations with Name, Masses and States ordered from an input to its simplified endpoint. Every adjacent state must be one legal edit; temporary complexity growth is allowed. These routes are combined with random scramble targets.";
UnscrambleSpinorAmplitudes::usage = UnscrambleSpinorAmplitudes::usage <> " The string option Model selects CandidateScorer (scores symbolic legal edits) or CompactActions (scores compact legal-edit features). Model -> Automatic preserves PolicyMethod selection; ModelDirectory chooses a directory containing the selected model files. CandidateBatchSize (default 128) bounds compact candidate-feature batches without changing global selection.";
UnscrambleTrace::usage = UnscrambleTrace::usage <> " Supports the same Model and ModelDirectory options as UnscrambleSpinorAmplitudes.";
UnscrambleSpinorAmplitudes::usage = UnscrambleSpinorAmplitudes::usage <> " MaxSteps defaults to Infinity; MaxStagnantSteps defaults to 10. Score-neutral MomentumSum splits do not consume stagnation patience. ProgressReporting -> Automatic shows live term counts in notebooks; False disables the display. TimeLimit remains an overall safety limit.";
UnscrambleTrace::usage = UnscrambleTrace::usage <> " Progress contains the final term counts, complexity, step count and elapsed seconds.";
UnscrambleSpinorAmplitudes::usage = UnscrambleSpinorAmplitudes::usage <> " Kernels -> Automatic uses the available local parallel capacity. Kernels -> n limits concurrent routes; 1 keeps sequential attempts. Parallel routes all restart at every strict new global best, with different opening moves and independent random choices. RandomSeed can fix the route random streams; asynchronous completion order can still differ.";
UnscrambleTrace::usage = UnscrambleTrace::usage <> " Parallel runs also return Routes, Restarts and Generation, and include RouteID, WorkerID and Generation on trace edits.";

Begin["`Private`"];

$modelDirectory = DirectoryName[$InputFileName];


(* ::Subsection::Closed:: *)
(*Generalized Schouten Identities*)

(* Decay Rates, Appendix B, Eq. (B11).
   Endpoints, including explicit little-group indices, travel unchanged. *)
spinorType[Spinor[_, t: ("Angle" | "Square"), __]] := t;
spinorType[Spinor[_, _, t: ("Angle" | "Square"), __]] := t;
spinorType[_] := None;
spinorLeg[Spinor[_, "Angle" | "Square", i_, ___]] := i;
spinorLeg[Spinor[_, _, "Angle" | "Square", i_, ___]] := i;

validChainQ[c_SpinorChain] := Module[{xs = List @@ c, l, r},
  If[Length[xs] < 2, Return[False]];
  l = spinorType[First[xs]]; r = spinorType[Last[xs]];
  MemberQ[{"Angle", "Square"}, l] && MemberQ[{"Angle", "Square"}, r] &&
    AllTrue[Drop[Rest[xs], -1], MatchQ[#, _Mom] &] &&
    SameQ[l === r, EvenQ[Length[xs] - 2]]
];
validChainQ[_] := False;

SchoutenRewrite[c1_, c2_, {m_Integer, n_Integer}] := Module[
  {u, v, p, q, leftP, rightP, leftQ, rightQ, chains, sign},
  If[!validChainQ[c1] || !validChainQ[c2], Return[$Failed]];
  u = List @@ c1; v = List @@ c2;
  p = Drop[Rest[u], -1]; q = Drop[Rest[v], -1];
  If[! (0 <= m <= Length[p] && 0 <= n <= Length[q]), Return[$Failed]];
  leftP = Reverse[Take[p, m]]; rightP = Drop[p, m];
  leftQ = Take[q, n]; rightQ = Reverse[Drop[q, n]];
  chains = {
    SpinorChain @@ Join[{Last[v]}, rightQ, rightP, {Last[u]}],
    SpinorChain @@ Join[{First[v]}, leftQ, leftP, {First[u]}],
    SpinorChain @@ Join[{Last[v]}, rightQ, leftP, {First[u]}],
    SpinorChain @@ Join[{First[v]}, leftQ, rightP, {Last[u]}]
  };
  If[!AllTrue[chains, validChainQ], Return[$Failed]];
  sign = (-1)^(m + Length[q] - n);
  sign*(chains[[1]]*chains[[2]] - chains[[3]]*chains[[4]])
];
SchoutenRewrite[___] := $Failed;

(* Eq. (A28): reversal contributes (-1)^(number of momenta + 1). *)
canonicalChain[c_SpinorChain] := Module[{rev = Reverse[c]},
  If[validChainQ[c] && Length[c] === 2 && First[c] === Last[c], Return[0]];
  If[validChainQ[c] && !OrderedQ[{c, rev}], (-1)^(Length[c] - 1)*rev, c]
];
(* Treat propagators as opaque, including when they contain other algebraic heads. *)
withProtectedPropagators[expr_, function_] := Module[{denominators, tokens},
  denominators = DeleteDuplicates[Cases[{expr}, _PropDen, Infinity]];
  If[denominators === {}, Return[function[expr]]];
  tokens = Table[Unique["propagator"], {Length[denominators]}];
  function[expr /. Thread[denominators -> tokens]] /. Thread[tokens -> denominators]
];
(* Distribute only momentum slots, never arguments of a propagator or another head. *)
linearMomentumTerms[arg_] := Module[{terms = monomialsOf[arg], factors, momenta, coefficient, out = {}},
  If[arg === 0, Return[{}]];
  Do[
    factors = factorsOf[term]; momenta = Select[factors, MatchQ[#, _Mom] &];
    If[Length[momenta] =!= 1, Return[$Failed]];
    coefficient = term/First[momenta];
    If[!FreeQ[coefficient, _Mom | _SpinorChain | _Spinor], Return[$Failed]];
    AppendTo[out, {coefficient, First[momenta] /. Mom[Multiparticle[ii__Integer]] :>
      Mom[Multiparticle @@ Sort[{ii}]]}], {term, terms}];
  out
];
linearChain[c_SpinorChain] := Module[{xs = List @@ c, parts},
  If[Length[xs] < 2 || !MemberQ[{"Angle", "Square"}, spinorType[First[xs]]] ||
      !MemberQ[{"Angle", "Square"}, spinorType[Last[xs]]] ||
      SameQ[spinorType[First[xs]], spinorType[Last[xs]]] =!= EvenQ[Length[xs] - 2], Return[c]];
  parts = linearMomentumTerms /@ Drop[Rest[xs], -1];
  If[!FreeQ[parts, $Failed], Return[c]];
  Total[(Times @@ #[[All, 1]])*canonicalChain[
      SpinorChain @@ Join[{First[xs]}, #[[All, 2]], {Last[xs]}]] & /@ Tuples[parts]]
];
validTraceQ[t_SpinorTrace] := EvenQ[Length[t]] && AllTrue[List @@ t, MatchQ[#, _Mom] &];
validTraceQ[_] := False;
linearTrace[t_SpinorTrace] := Module[{parts},
  If[OddQ[Length[t]], Return[t]];
  parts = linearMomentumTerms /@ List @@ t;
  If[!FreeQ[parts, $Failed], Return[t]];
  Total[(Times @@ #[[All, 1]])*If[# === {}, 2, SpinorTrace @@ #[[All, 2]]] & /@ Tuples[parts]]
];
canonicalChains[expr_] := withProtectedPropagators[expr,
  Function[body, Expand[body /. {c_SpinorChain :> linearChain[c], t_SpinorTrace :> linearTrace[t],
    Mom[Multiparticle[ii__Integer]] :> Mom[Multiparticle @@ Sort[{ii}]]}]]];
canonicalAmplitude[expr_, rules_] := canonicalChains[
  withProtectedPropagators[expr, Function[body, body /. rules]]];

SchoutenCandidates[expr_] := Module[{moves = {}, mons, chains, rhs, sub, ins, mon, i, j, m, n},
  mons = monomialsOf[expr];
  Do[
    chains = Select[chainsIn[mon], validChainQ];
    Do[
      rhs = SchoutenRewrite[chains[[i]], chains[[j]], {m, n}];
      If[rhs === $Failed, Continue[]];
      sub = mon;
      ins = canonicalChains[mon/(chains[[i]]*chains[[j]])*rhs];
      If[!samePoly[canonicalChains[sub], ins],
        AppendTo[moves, <|"Name" -> "Schouten", "Subtract" -> sub,
          "Insert" -> ins, "Chains" -> chains[[{i, j}]], "Splits" -> {m, n}|>]
      ],
      {i, Length[chains]}, {j, i + 1, Length[chains]},
      {m, 0, Length[chains[[i]]] - 2}, {n, 0, Length[chains[[j]]] - 2}
    ],
    {mon, mons}
  ];
  DeleteDuplicatesBy[moves, {#["Subtract"], #["Insert"]} &]
];



(* ::Subsection::Closed:: *)
(*Expression Structure*)

expandAmplitude[expr_] := Expand[expr];

externalIndices[massRules_List] := Sort @ Cases[massRules, HoldPattern[Mass[i_Integer] -> _] :> i];

massValue[massRules_List, i_] := Replace[Mass[i], Join[massRules, {_ -> Missing["Mass"]}]];

monomialsOf[expr_] := Module[{ex = expandAmplitude[expr]},
  Which[
    ex === 0, {},
    Head[ex] === Plus, List @@ ex,
    True, {ex}
  ]
];

factorsOf[mon_] := If[Head[mon] === Times, List @@ mon, {mon}];

chainPairQ[SpinorChain[Spinor[_, _, _], Spinor[_, _, _]]] := True;
chainPairQ[_] := False;
chainKind[SpinorChain[Spinor[k_, _, _], ___]] := k;
chainType[SpinorChain[Spinor[_, tp_, _], Spinor[_, tp_, _]]] := tp;
chainEnds[SpinorChain[Spinor[_, _, i_], Spinor[_, _, j_]]] := {i, j};
threeChainQ[SpinorChain[Spinor[_, _, _], Mom[_], Spinor[_, _, _]]] := True;
threeChainQ[_] := False;

chainsIn[mon_] := Module[{factors = factorsOf[mon], out = {}, i},
  Do[
    Which[
      MatchQ[factors[[i]], _SpinorChain], AppendTo[out, factors[[i]]],
      MatchQ[factors[[i]], Power[_SpinorChain, _Integer?Positive]],
        Do[AppendTo[out, factors[[i, 1]]], {factors[[i, 2]]}]
    ],
    {i, Length[factors]}
  ];
  out
];

tracesIn[mon_] := Flatten[Function[factor, Which[
    MatchQ[factor, _SpinorTrace], {factor},
    MatchQ[factor, Power[_SpinorTrace, _Integer?Positive]], ConstantArray[factor[[1]], factor[[2]]],
    True, {}]] /@ factorsOf[mon]];
momentumSlots[c_SpinorChain] := Drop[Rest[List @@ c], -1];
momentumSlots[t_SpinorTrace] := List @@ t;
momentumSlotOffset[c_SpinorChain] := 1;
momentumSlotOffset[t_SpinorTrace] := 0;
traceMetadata[t_SpinorTrace] := <|"Trace" -> t|>;
traceMetadata[_] := <||>;

bracket[kind_, tp_, a_, b_] := SpinorChain[Spinor[kind, tp, a], Spinor[kind, tp, b]];
three[kind_, t1_, a_, p_, t2_, b_] :=
  SpinorChain[Spinor[kind, t1, a], Mom[p], Spinor[kind, t2, b]];

(* <ij><kl> = <ik><jl> - <il><jk>, and the same for square brackets.
   Checked against ReduceSpinorProducts. *)
schoutenSum[kind_, tp_, i_, j_, k_, l_] :=
  bracket[kind, tp, i, k]*bracket[kind, tp, j, l] -
  bracket[kind, tp, i, l]*bracket[kind, tp, j, k];

(* A bracket times a one-momentum chain. The contracted spinor is the chain
   end of the same chirality as the bracket. This is the epsilon identity
   <ab>(v·d) = <db>(v·a) - <da>(v·b). *)
schoutenChainSum[br_, sc_] := Module[{kind, a, b, c, p, d, tb, t1, t2},
  kind = chainKind[br];
  {a, b} = chainEnds[br];
  tb = chainType[br];
  t1 = sc[[1, 2]];
  t2 = sc[[3, 2]];
  c = sc[[1, 3]];
  p = sc[[2, 1]];
  d = sc[[3, 3]];
  Which[
    tb === "Angle" && t1 === "Square" && t2 === "Angle",
      bracket[kind, "Angle", d, b]*three[kind, "Square", c, p, "Angle", a] -
      bracket[kind, "Angle", d, a]*three[kind, "Square", c, p, "Angle", b],
    tb === "Square" && t1 === "Square" && t2 === "Angle",
      bracket[kind, "Square", c, b]*three[kind, "Square", a, p, "Angle", d] -
      bracket[kind, "Square", c, a]*three[kind, "Square", b, p, "Angle", d],
    tb === "Angle" && t1 === "Angle" && t2 === "Square",
      bracket[kind, "Angle", c, b]*three[kind, "Angle", a, p, "Square", d] -
      bracket[kind, "Angle", c, a]*three[kind, "Angle", b, p, "Square", d],
    tb === "Square" && t1 === "Angle" && t2 === "Square",
      bracket[kind, "Square", d, b]*three[kind, "Angle", c, p, "Square", a] -
      bracket[kind, "Square", d, a]*three[kind, "Angle", c, p, "Square", b],
    True, $Failed
  ]
];

structureQ[SpinorChain[__]] := True;
structureQ[SpinorTrace[___]] := True;
structureQ[MomProd[__]] := True;
structureQ[Mom[Multiparticle[__]]] := True;
structureQ[PropDen[__]] := True;
structureQ[Mandelstahm[__]] := True;
structureQ[Power[f_, _]] := structureQ[f];
structureQ[_] := False;

splitMon[mon_] := Module[{factors = factorsOf[mon], skeleton},
  skeleton = Times @@ Select[factors, structureQ];
  {Times @@ Select[factors, ! structureQ[#] &], skeleton}
];

groupsOf[expr_] := Module[{groups = <||>, mons, pair, i},
  mons = monomialsOf[expr];
  Do[
    pair = splitMon[mons[[i]]];
    groups[pair[[2]]] = Lookup[groups, pair[[2]], 0] + pair[[1]],
    {i, Length[mons]}
  ];
  groups
];


(* ::Subsection::Closed:: *)
(*Mass Rules and On-Shell Conditions*)

knownInternalChannels[massRules_List] := Select[Cases[massRules,
  HoldPattern[Mass[Multiparticle[ii__Integer]] -> m_] :> {Sort[{ii}], m}],
  Length[#[[1]]] >= 2 && DuplicateFreeQ[#[[1]]] &];
internalChannels[massRules_List] := Select[knownInternalChannels[massRules],
  $onShellChannels === Automatic || MemberQ[$onShellChannels, #[[1]]] &];
internalPairs[massRules_List] := Select[internalChannels[massRules], Length[#[[1]]] === 2 &];

shellOf[m_, ma_, mb_] := Expand[(m^2 - ma^2 - mb^2)/2];

(* coef = shell * quotient, with quotient free of the shell's variables. *)
shellQuotient[coef_, shell_] := Module[{vars, num, den, red},
  If[shell === 0, Return[None]];
  vars = Variables[shell];
  If[vars === {},
    Return[If[Expand[coef - shell] === 0, 1, None]]
  ];
  {num, den} = NumeratorDenominator[Together[coef]];
  If[! PolynomialQ[num, vars] || ! PolynomialQ[Expand[den], vars] || ! PolynomialQ[shell, vars],
    Return[None]
  ];
  red = PolynomialReduce[num, {shell}, vars];
  If[Expand[red[[2]]] =!= 0, None, Together[red[[1, 1]]/den]]
];

orderedDot[a_, b_] := MomProd @@ Sort[{a, b}];

lineMass[massRules_List, legs_List] := Module[{found},
  found = SelectFirst[internalChannels[massRules], #[[1]] === Sort[legs] &];
  If[MissingQ[found], None, found[[2]]]
];
lineMass[massRules_List, i_Integer, j_Integer] := lineMass[massRules, {i, j}];
momentumMass[rules_List, leg_Integer] := massValue[rules, leg];
momentumMass[rules_List, leg_Multiparticle] := lineMass[rules, List @@ leg];
momentumMass[_, _] := None;

scalarFactorPresentQ[sk_, atom_] := AnyTrue[factorsOf[sk], Function[factor,
  With[{f = If[MatchQ[factor, Power[_, _Integer?Positive]], List @@ factor, {factor, 1}],
      a = If[MatchQ[atom, Power[_, _Integer?Positive]], List @@ atom, {atom, 1}]},
    f[[1]] === a[[1]] && f[[2]] >= a[[2]]]]];
numeratorScalarAtoms[groups_] := DeleteDuplicates[Flatten[
  (Function[factor, If[MatchQ[factor, Power[_, _Integer?Positive]], factor[[1]], factor]] /@
    factorsOf[#]) & /@ Keys[groups]]];
scalarRelationContexts[groups_, atoms_List] := DeleteDuplicates[Flatten[Table[
  If[scalarFactorPresentQ[sk, atom], {sk/atom}, {}], {sk, Keys[groups]}, {atom, atoms}], 2]];

(* Match complete scalar relations in a shared numerator context. Structural
   factors, including entire propagators, never participate in the relation. *)
scalarRelationMoves[groups_, lhs_, rhs_, name_, legs_, identity_] := Module[
  {wanted = groupsOf[lhs], atoms, contexts, pivot, scale, moves = {}, subtract, insert},
  atoms = DeleteCases[Keys[wanted], 1];
  If[atoms === {}, Return[{}]];
  contexts = scalarRelationContexts[groups, atoms]; pivot = First[atoms];
  Do[
    scale = Lookup[groups, context pivot, 0]/wanted[pivot];
    If[samePoly[scale, 0] || !AllTrue[Keys[wanted],
        samePoly[Lookup[groups, context #, 0], scale wanted[#]] &], Continue[]];
    subtract = expandAmplitude[scale lhs context]; insert = expandAmplitude[scale rhs context];
    If[!samePoly[subtract, insert], AppendTo[moves, <|"Name" -> name,
      "Subtract" -> subtract, "Insert" -> insert, "Legs" -> legs, "Identity" -> identity|>]],
    {context, contexts}];
  moves
];

(* 2 p_i·p_j = (p_i+p_j)^2 - m_i^2 - m_j^2, written Mom[Multiparticle[i,j]]^2
   so Expand does not turn the square into ordinary products. An internal
   line also has (p_i+p_j)^2 = M^2. *)

(* ::Subsection::Closed:: *)
(*Momentum-Square Identities*)

momentumSquareMoves[expr_, massRules_List] := Module[{moves = {}, groups, keys, exts, atoms, pairs, pair, g},
  groups = groupsOf[expr];
  keys = Keys[groups];
  exts = externalIndices[massRules];
  atoms = numeratorScalarAtoms[groups];
  pairs = DeleteDuplicates[Join[Cases[atoms, MomProd[i_Integer, j_Integer] /; i =!= j :> Sort[{i, j}]],
    Cases[atoms, Mom[Multiparticle[i_Integer, j_Integer]] :> Sort[{i, j}]],
    First /@ internalPairs[massRules]]];
  Do[
    Module[{i = pair[[1]], j = pair[[2]], mi, mj, mp2, dot, kin, bigM, sk, coef, rest, subtract, insert},
      mi = massValue[massRules, i];
      mj = massValue[massRules, j];
      If[mi === Missing["Mass"] || mj === Missing["Mass"], Continue[]];
      mp2 = Mom[Multiparticle[i, j]]^2;
      dot = orderedDot[i, j];
      kin = mp2 - mi^2 - mj^2;
      bigM = lineMass[massRules, i, j];
      Do[
        sk = keys[[g]]; coef = groups[sk];
        If[MemberQ[factorsOf[sk], dot],
          rest = sk/dot;
          subtract = expandAmplitude[coef*sk];
          insert = expandAmplitude[coef*rest*kin/2];
          If[! samePoly[subtract, insert],
            AppendTo[moves, <|"Name" -> "MomentumSquare", "Subtract" -> subtract,
              "Insert" -> insert, "Legs" -> {i, j}|>]
          ]
        ];
        If[MemberQ[factorsOf[sk], mp2],
          rest = sk/mp2;
          subtract = expandAmplitude[coef*sk];
          insert = expandAmplitude[coef*rest*(2*dot + mi^2 + mj^2)];
          If[! samePoly[subtract, insert],
            AppendTo[moves, <|"Name" -> "MomentumSquare", "Subtract" -> subtract,
              "Insert" -> insert, "Legs" -> {i, j}|>]
          ];
          If[bigM =!= None && ! (bigM === 0 && mi === 0 && mj === 0),
            insert = expandAmplitude[coef*rest*bigM^2];
            If[! samePoly[subtract, insert],
              AppendTo[moves, <|"Name" -> "OnShell", "Subtract" -> subtract,
                "Insert" -> insert, "Legs" -> {i, j}, "Mass" -> bigM|>]
            ]
          ]
        ],
        {g, Length[keys]}
      ];
      If[bigM =!= None,
        Module[{shell2 = Expand[bigM^2 - mi^2 - mj^2], q},
          Do[
            sk = keys[[g]]; coef = groups[sk];
            If[withProtectedPropagators[sk, Function[body,
                FreeQ[body, Mom[Multiparticle[__]]] && FreeQ[body, dot]]],
              q = shellQuotient[coef, shell2];
              If[q =!= None && Expand[q] =!= 0,
                subtract = expandAmplitude[coef*sk];
                insert = expandAmplitude[q*kin*sk];
                If[! samePoly[subtract, insert],
                  AppendTo[moves, <|"Name" -> "MomentumSquare", "Subtract" -> subtract,
                    "Insert" -> insert, "Legs" -> {i, j}|>]
                ]
              ]
            ],
            {g, Length[keys]}
          ]
        ]
      ]
    ],
    {pair, pairs}
  ];
  Join[moves, subsetMomentumSquareMoves[expr, massRules]]
];

subsetMomentumSquareMoves[expr_, rules_List] := Module[
  {moves = {}, groups = groupsOf[expr], subsets, present, legs, label, square, masses,
   value, dots, mass, subtract, insert, rest, scale, coef},
  present = DeleteDuplicates[Cases[numeratorScalarAtoms[groups],
    Mom[q:Multiparticle[ii__Integer]] /; Length[{ii}] >= 3 && DuplicateFreeQ[{ii}] :> {q, Sort[{ii}]}]];
  subsets = DeleteDuplicates[Join[present,
    ({Multiparticle @@ #[[1]], #[[1]]} &) /@ Select[knownInternalChannels[rules], Length[#[[1]]] >= 3 &]]];
  Do[
    {label, legs} = subset; square = Mom[label]^2;
    masses = massValue[rules, #] & /@ legs; mass = lineMass[rules, legs];
    dots = orderedDot @@ # & /@ Subsets[legs, {2}];
    value = If[AnyTrue[masses, MissingQ], None, Total[masses^2] + 2 Total[dots]];
    Do[
      coef = groups[sk];
      If[scalarFactorPresentQ[sk, square],
        rest = sk/square; subtract = expandAmplitude[coef sk];
        If[value =!= None, AppendTo[moves, <|"Name" -> "MomentumSquare", "Subtract" -> subtract,
          "Insert" -> expandAmplitude[coef rest value], "Legs" -> legs,
          "Identity" -> "ExpandSubset"|>]];
        If[mass =!= None, AppendTo[moves, <|"Name" -> "OnShell", "Subtract" -> subtract,
          "Insert" -> expandAmplitude[coef rest mass^2], "Legs" -> legs,
          "Mass" -> mass, "Identity" -> "SubsetSquare"|>]]];
      If[mass =!= None && !TrueQ[mass == 0] && !FreeQ[coef, mass],
        scale = shellQuotient[coef, mass^2];
        If[scale =!= None && !samePoly[scale, 0],
          AppendTo[moves, <|"Name" -> "OnShell", "Subtract" -> expandAmplitude[coef sk],
            "Insert" -> expandAmplitude[scale square sk], "Legs" -> legs,
            "Mass" -> mass, "Identity" -> "ExpandSubsetMass"|>];
          If[value =!= None, AppendTo[moves, <|"Name" -> "OnShell", "Subtract" -> expandAmplitude[coef sk],
            "Insert" -> expandAmplitude[scale value sk], "Legs" -> legs,
            "Mass" -> mass, "Identity" -> "ExpandSubsetMassValue"|>]]]],
      {sk, Keys[groups]}];
    If[value =!= None,
      moves = Join[moves, scalarRelationMoves[groups, value, square,
        "MomentumSquare", legs, "CollectSubset"]];
      If[mass =!= None, moves = Join[moves, scalarRelationMoves[groups, value, mass^2,
        "OnShell", legs, "SubsetInvariant"]]]],
    {subset, subsets}];
  moves
];

scalarMomentumConservationMoves[expr_, rules_List] := Module[
  {moves = {}, groups = groupsOf[expr], exts = externalIndices[rules], dots,
   legs, mass, rhs, subtract, insert, scale, row, complement},
  If[!TrueQ[$useMomentumConservation] || Length[exts] < 2, Return[{}]];
  dots = Cases[numeratorScalarAtoms[groups], d:MomProd[i_Integer, j_Integer] /;
    i =!= j && MemberQ[exts, i] && MemberQ[exts, j] :> d];
  Do[
    legs = List @@ dot;
    Do[
      mass = massValue[rules, anchor];
      rhs = -mass^2 - Total[orderedDot[anchor, #] & /@ Complement[exts, legs]];
      Do[If[scalarFactorPresentQ[sk, dot],
        subtract = expandAmplitude[groups[sk] sk]; insert = expandAmplitude[groups[sk] sk/dot rhs];
        If[!samePoly[subtract, insert], AppendTo[moves, <|"Name" -> "MomentumConservation",
          "Subtract" -> subtract, "Insert" -> insert, "Legs" -> {anchor, First[DeleteCases[legs, anchor]]},
          "Identity" -> "ScalarDot"|>]]], {sk, Keys[groups]}],
      {anchor, legs}], {dot, dots}];
  (* The square of a pair equals the square of its complementary external
     momentum sum. Only external masses enter this off-shell relation. *)
  If[Length[exts] >= 4 && AllTrue[exts, !MissingQ[massValue[rules, #]] &],
    Do[
      legs = Sort[List @@ dot]; complement = Complement[exts, legs];
      rhs = (Total[massValue[rules, #]^2 & /@ complement] -
          Total[massValue[rules, #]^2 & /@ legs])/2 +
        Total[orderedDot @@ # & /@ Subsets[complement, {2}]];
      Do[If[scalarFactorPresentQ[sk, dot],
        subtract = expandAmplitude[groups[sk] sk];
        insert = expandAmplitude[groups[sk] sk/dot rhs];
        If[!samePoly[subtract, insert], AppendTo[moves, <|"Name" -> "MomentumConservation",
          "Subtract" -> subtract, "Insert" -> insert, "Legs" -> legs,
          "Identity" -> "ScalarComplement", "ScrambleEligible" -> False|>]]], {sk, Keys[groups]}],
      {dot, dots}]];
  Do[
    mass = massValue[rules, anchor]; row = Total[orderedDot[anchor, #] & /@ DeleteCases[exts, anchor]];
    moves = Join[moves, scalarRelationMoves[groups, row, -mass^2,
      "MomentumConservation", {anchor}, "ScalarRow"]];
    If[!TrueQ[mass == 0], Do[
      scale = shellQuotient[groups[sk], -mass^2];
      If[scale =!= None && !samePoly[scale, 0], AppendTo[moves, <|"Name" -> "MomentumConservation",
        "Subtract" -> expandAmplitude[groups[sk] sk], "Insert" -> expandAmplitude[scale row sk],
        "Legs" -> {anchor}, "Identity" -> "ScalarRowExpand"|>]], {sk, Keys[groups]}]],
    {anchor, exts}];
  moves
];

(* Massless helicity completeness: <i|p_j|i] = 2 p_i.p_j and
   <ij>[ji] = 2 p_i.p_j. Inverse brackets remain ordinary rational factors. *)
helicityInvariantMoves[expr_, rules_List] := Module[
  {moves = {}, mons = monomialsOf[expr], massless, chain, xs, dot, product, chains, rest},
  massless = Select[externalIndices[rules], TrueQ[massValue[rules, #] === 0] &];
  Do[
    chains = chainsIn[mon];
    Do[
      xs = List @@ chain;
      If[Length[xs] === 3 && validChainQ[chain] &&
          MatchQ[First[xs], Spinor["Helicity", "Angle" | "Square", _Integer]] &&
          Last[xs] === flipSpinor[First[xs]] && MemberQ[massless, spinorLeg[First[xs]]] &&
          MatchQ[xs[[2]], Mom[_Integer]],
        dot = If[spinorLeg[First[xs]] === xs[[2, 1]], 0,
          orderedDot[spinorLeg[First[xs]], xs[[2, 1]]] /. rules];
        AppendTo[moves, <|"Name" -> "MomentumSquare", "Identity" -> "HelicityTrace",
          "Direction" -> "Contract", "ScrambleEligible" -> False, "Subtract" -> mon,
          "Insert" -> canonicalChains[mon/chain*2 dot],
          "Legs" -> {spinorLeg[First[xs]], xs[[2, 1]]}|>]],
      {chain, DeleteDuplicates[chains]}];
    Do[
      dot = orderedDot @@ pair;
      product = canonicalChains[bracket["Helicity", "Angle", pair[[1]], pair[[2]]]*
        bracket["Helicity", "Square", pair[[2]], pair[[1]]]];
      If[scalarFactorPresentQ[mon, dot],
        AppendTo[moves, <|"Name" -> "MomentumSquare", "Identity" -> "MasslessBracket",
          "Direction" -> "Expand", "Subtract" -> mon,
          "Insert" -> canonicalChains[mon/dot*product/2], "Legs" -> pair|>]];
      If[AllTrue[Cases[product, _SpinorChain, Infinity], MemberQ[chains, #] &],
        rest = Cancel[mon/product];
        AppendTo[moves, <|"Name" -> "MomentumSquare", "Identity" -> "MasslessBracket",
          "Direction" -> "Contract", "Subtract" -> mon,
          "Insert" -> canonicalChains[rest*2 dot], "Legs" -> pair|>]],
      {pair, Subsets[massless, {2}]}],
    {mon, mons}];
  moves
];

(* Common-denominator algebra can introduce a propagator in the numerator.
   Expand that numerator factor off shell; never rewrite the denominator. *)
propagatorNumeratorMoves[expr_, rules_List] := Module[
  {moves = {}, groups = groupsOf[expr], buckets, terms, props, tokens, fraction, num, den,
   legs, masses, value, insert, subtract, replacements, usedLegs, valid},
  buckets = GroupBy[Keys[groups], # /. _PropDen -> 1 &];
  Do[
    terms = (groups[#] # &) /@ keys;
    props = DeleteDuplicates[Cases[terms, _PropDen, Infinity]];
    If[Length[props] < 2, Continue[]];
    tokens = Table[Unique["numeratorPropagator"], {Length[props]}];
    subtract = Total[terms];
    fraction = Together[subtract /. Thread[props -> tokens]];
    {num, den} = NumeratorDenominator[fraction];
    replacements = {}; usedLegs = {}; valid = True;
    Do[
      If[FreeQ[num, tokens[[i]]], Continue[]];
      If[!PolynomialQ[num, {tokens[[i]]}] ||
          !MatchQ[props[[i]], PropDen[Mom[_Integer | Multiparticle[__Integer]], _]],
        valid = False; Break[]];
      legs = Replace[props[[i, 1, 1]], {j_Integer :> {j}, Multiparticle[ii__Integer] :> {ii}}];
      If[!DuplicateFreeQ[legs], valid = False; Break[]];
      masses = massValue[rules, #] & /@ legs;
      If[AnyTrue[masses, MissingQ], valid = False; Break[]];
      value = Total[masses^2] + 2 Total[orderedDot @@ # & /@ Subsets[legs, {2}]] - props[[i, 2]]^2;
      AppendTo[replacements, tokens[[i]] -> value]; usedLegs = Union[usedLegs, legs],
      {i, Length[props]}];
    If[valid && replacements =!= {},
      insert = canonicalChains[((num /. replacements)/den) /. Thread[tokens -> props]];
      If[!samePoly[subtract, insert], AppendTo[moves, <|"Name" -> "MomentumSquare",
        "Identity" -> "PropagatorNumerator", "ScrambleEligible" -> False, "Subtract" -> subtract,
        "Insert" -> insert, "Legs" -> usedLegs|>]]],
    {keys, Values[buckets]}];
  moves
];


ComplicateAmplitude[pair:{_List, _}, nSteps_Integer, seed_Integer] :=
  ComplicateAmplitude[pair, nSteps, RandomSeed -> seed];

samePoly[a_, b_] := expandAmplitude[a - b] === 0;



(* ::Subsection::Closed:: *)
(*Kinematic Identities*)

kinematicMoves[expr_, massRules_List] := Module[
  {moves = {}, groups, keys, exts, buckets, r, g, n, momentumPlaceholder = Unique["conservedMomentum"]},
  groups = groupsOf[expr];
  keys = Keys[groups];
  exts = externalIndices[massRules];
  Do[
    Module[{legs = internalPairs[massRules][[r, 1]], m = internalPairs[massRules][[r, 2]],
        ma, mb, shell, dot, sk, coef, rest, subtract, insert, q},
      ma = massValue[massRules, legs[[1]]];
      mb = massValue[massRules, legs[[2]]];
      If[ma =!= Missing["Mass"] && mb =!= Missing["Mass"],
        shell = shellOf[m, ma, mb];
        dot = orderedDot @@ legs;
        Do[
          sk = keys[[g]]; coef = groups[sk];
          If[AnyTrue[factorsOf[sk], # === dot ||
              MatchQ[#, Power[base_, power_Integer?Positive] /; base === dot] &],
            rest = sk/dot;
            subtract = expandAmplitude[coef*sk];
            insert = expandAmplitude[coef*shell*rest];
            If[! samePoly[subtract, insert],
              AppendTo[moves, <|"Name" -> "OnShell", "Subtract" -> subtract, "Insert" -> insert,
                "Legs" -> Sort[legs]|>]
            ]
          ];
          (* Expand a scalar coefficient back into this diagram's on-shell dot product.
             PropDen remains a structural factor and is never rewritten. *)
          q = shellQuotient[coef, shell];
          If[q =!= None && !samePoly[q, 0],
            subtract = expandAmplitude[coef*sk];
            insert = expandAmplitude[q*dot*sk];
            AppendTo[moves, <|"Name" -> "OnShell", "Subtract" -> subtract,
              "Insert" -> insert, "Legs" -> Sort[legs]|>]
          ],
          {g, Length[keys]}
        ]
      ]
    ],
    {r, Length[internalPairs[massRules]]}
  ];
  buckets = <||>;
  Do[
    Module[{sk = keys[[g]], slots, offset, which, template},
      (* Extract one positive chain or trace factor before replacing a slot.
         Any remaining copies stay in the template, so a square itself is
         never treated as linear. Inverse factors are not selected. *)
      Do[
        If[!validChainQ[factor] && !validTraceQ[factor], Continue[]];
        slots = momentumSlots[factor]; offset = momentumSlotOffset[factor];
        Do[
          If[!MatchQ[slots[[n]], Mom[_Integer]], Continue[]];
          which = slots[[n, 1]];
          If[!IntegerQ[which] || !MemberQ[exts, which], Continue[]];
          template = sk/factor*ReplacePart[factor, n + offset -> momentumPlaceholder];
          buckets[template] = Append[Lookup[buckets, template, {}], {which, groups[sk], sk}],
          {n, Length[slots]}],
        {factor, DeleteDuplicates[Join[chainsIn[sk], tracesIn[sk]]]}]
    ],
    {g, Length[keys]}
  ];
  KeyValueMap[
    Function[{template, entries},
      Module[{byMom, present, missing, alpha, skJ, subtract, insert},
        byMom = <||>;
        Do[
          byMom[entries[[n, 1]]] = {entries[[n, 2]], entries[[n, 3]]},
          {n, Length[entries]}
        ];
        present = Keys[byMom];
        missing = Complement[exts, present];
        If[Length[missing] === 1 && Length[present] === Length[exts] - 1 && Length[exts] >= 2,
          alpha = byMom[present[[1]]][[1]];
          If[AllTrue[present, samePoly[byMom[#][[1]], alpha] &],
            skJ = template /. momentumPlaceholder -> Mom[missing[[1]]];
            subtract = expandAmplitude[Total[alpha*Table[byMom[present[[n]]][[2]], {n, Length[present]}]]];
            insert = expandAmplitude[-alpha*skJ];
            If[! samePoly[subtract, insert],
              AppendTo[moves, <|"Name" -> "MomentumConservation", "Subtract" -> subtract,
                "Insert" -> insert, "Momentum" -> missing[[1]]|>]
            ]
          ]
        ]
      ]
    ],
    buckets
  ];
  Do[AppendTo[moves, sq], {sq, momentumSquareMoves[expr, massRules]}];
  moves = Join[moves, scalarMomentumConservationMoves[expr, massRules]];
  DeleteDuplicatesBy[moves, {#["Name"], expandAmplitude[#["Subtract"]], expandAmplitude[#["Insert"]]} &]
];

(* One shared scorer evaluates every legal edit.
   UTF-8 serialization preserves the complete ordered expression tree. *)

(* ::Subsection::Closed:: *)
(*Policy Settings*)

$modelNet = None;
$compactWeights = None;
$loadedModelDirectory = None;
$onShellChannels = Automatic;
$useMomentumConservation = True;
$maxStagnantSteps = 10;
$timeLimit = 60;
$candidateBatchSize = 128;
$progressReporting = Automatic;
$trainingBatchSize = 1;
$trainingTargetDevice = "CPU";

normalizeOnShellChannels[Automatic] := Automatic;
normalizeOnShellChannels[channels_List] := Sort /@ channels;

Options[UnscrambleCandidates] = {"OnShellChannels" -> Automatic, "MomentumConservation" -> True};
Options[ComplicateAmplitude] = Join[{RandomSeed -> Automatic, "RecordSteps" -> False}, Options[UnscrambleCandidates]];
Options[UnscrambleSpinorAmplitudes] = Join[
  {"MaxSteps" -> Infinity, "Attempts" -> 5, "MaxStagnantSteps" -> 10,
    "TimeLimit" -> 60, "Model" -> Automatic, "ModelDirectory" -> Automatic,
    "CandidateBatchSize" -> 128,
    "PolicyMethod" -> "CandidateScorer", "ProgressReporting" -> Automatic,
    "Kernels" -> Automatic, RandomSeed -> Automatic}, Options[UnscrambleCandidates]];
Options[UnscrambleTrace] = Options[UnscrambleSpinorAmplitudes];


(* ::Subsection::Closed:: *)
(*Legal Candidate Edits*)

stopMove[] := <|"Name" -> "Stop", "Subtract" -> 0, "Insert" -> 0|>;

(* Relate both endpoints to their own momenta without changing the other
   endpoint's massive/massless kind or little-group indices. *)
flipSpinor[s_Spinor] := s /. {"Angle" -> "Square", "Square" -> "Angle"};
chainMoves[expr_, massRules_] := Module[{moves = {}, mons, chains, c, xs, ps, m, rhs, others, t, k, side, j},
  mons = monomialsOf[expr];
  Do[
    chains = Select[chainsIn[mons[[t]]], validChainQ];
    Do[
      c = chains[[k]]; xs = List @@ c; ps = Drop[Rest[xs], -1];
      Do[
        m = massValue[massRules, spinorLeg[xs[[side]]]];
        If[!MissingQ[m] && m =!= 0,
          rhs = If[side === 1,
            (SpinorChain @@ Join[{flipSpinor[First[xs]], Mom[spinorLeg[First[xs]]]}, ps, {Last[xs]}])/m,
            -(SpinorChain @@ Join[{First[xs]}, ps, {Mom[spinorLeg[Last[xs]]], flipSpinor[Last[xs]]}])/m];
          AppendTo[moves, <|"Name" -> "Mass", "Subtract" -> mons[[t]],
            "Insert" -> Expand[mons[[t]]/c*rhs], "Site" -> {t, k, side}, "Direction" -> "Expand"|>]
        ];
        If[ps === {}, Continue[]];
        If[If[side === 1, First[ps], Last[ps]] === Mom[spinorLeg[xs[[side]]]],
          m = massValue[massRules, spinorLeg[xs[[side]]]];
          If[!MissingQ[m],
            rhs = If[side === 1,
              m*(SpinorChain @@ Join[{flipSpinor[First[xs]]}, Rest[ps], {Last[xs]}]),
              -m*(SpinorChain @@ Join[{First[xs]}, Most[ps], {flipSpinor[Last[xs]]}])];
            AppendTo[moves, <|"Name" -> "Mass", "Subtract" -> mons[[t]],
              "Insert" -> Expand[mons[[t]]/c*rhs], "Site" -> {t, k, side}, "Direction" -> "Contract"|>]
          ]
        ], {side, {1, -1}}
      ];
      If[TrueQ[$useMomentumConservation],
        Do[
          If[!MemberQ[externalIndices[massRules], ps[[j, 1]]], Continue[]];
          others = DeleteCases[externalIndices[massRules], ps[[j, 1]]];
          If[others === {}, Continue[]];
          rhs = -Total[ReplacePart[c, j + 1 -> Mom[#]] & /@ others];
          AppendTo[moves, <|"Name" -> "MomentumConservation", "Subtract" -> mons[[t]],
            "Insert" -> Expand[mons[[t]]/c*rhs], "Site" -> {t, k, j}|>],
          {j, Length[ps]}
        ]
      ], {k, Length[chains]}
    ], {t, Length[mons]}
  ];
  moves
];

(* Interior Clifford identities preserve both endpoint spinors and their indices.
   Reverse square insertions are limited to the start of a chain, one per
   massive external leg, rather than every possible insertion site. *)
interiorChainMoves[expr_, rules_] := Module[
  {moves = {}, mons = monomialsOf[expr], chains, c, xs, ps, short, rhs, square, mass, leg, swapped},
  Do[
    chains = DeleteDuplicates[Select[chainsIn[mons[[t]]], validChainQ]];
    Do[
      c = chains[[k]]; xs = List @@ c; ps = Drop[Rest[xs], -1];
      Do[
        short = SpinorChain @@ Join[Take[xs, j], Drop[xs, j + 2]];
        If[ps[[j]] === ps[[j + 1]],
          leg = ps[[j, 1]];
          mass = momentumMass[rules, leg];
          square = If[MissingQ[mass] || mass === None, Mom[leg]^2, mass^2];
          rhs = square short;
          AppendTo[moves, <|"Name" -> "ChainSquare", "Subtract" -> mons[[t]],
            "Insert" -> Expand[mons[[t]]/c*rhs], "Site" -> {t, k, j}, "Direction" -> "Contract"|>],
          swapped = ReplacePart[c, {j + 1 -> ps[[j + 1]], j + 2 -> ps[[j]]}];
          rhs = 2 (orderedDot[ps[[j, 1]], ps[[j + 1, 1]]] /. rules) short - swapped;
          AppendTo[moves, <|"Name" -> "Anticommutation", "Subtract" -> mons[[t]],
            "Insert" -> Expand[mons[[t]]/c*rhs], "Site" -> {t, k, j}|>]
        ], {j, Length[ps] - 1}];
      (* Do not nest inverse square insertions in a chain already containing one. *)
      If[AnyTrue[Partition[ps, 2, 1], #[[1]] === #[[2]] &], Continue[]];
      Do[
        mass = massValue[rules, leg];
        If[MissingQ[mass] || TrueQ[mass == 0], Continue[]];
        rhs = (SpinorChain @@ Join[Take[xs, 1], {Mom[leg], Mom[leg]}, Rest[xs]])/mass^2;
        AppendTo[moves, <|"Name" -> "ChainSquare", "Subtract" -> mons[[t]],
          "Insert" -> Expand[mons[[t]]/c*rhs], "Site" -> {t, k, leg}, "Direction" -> "Expand"|>],
        {leg, externalIndices[rules]}],
      {k, Length[chains]}], {t, Length[mons]}];
  moves
];

(* A composite momentum is a sum, independently of momentum conservation or
   on-shell assumptions. Binary collection supplies reverse training moves. *)
momentumMembers[i_Integer] := {i};
momentumMembers[Multiparticle[ii__Integer]] := With[{members = {ii}},
  If[Length[members] >= 2 && DuplicateFreeQ[members], Sort[members], {}]];
momentumMembers[_] := {};
momentumLabel[{i_}] := i;
momentumLabel[ii_List] := Multiparticle @@ Sort[ii];

momentumSumMoves[expr_, rules_] := Module[
  {moves = {}, mons = monomialsOf[expr], exts = externalIndices[rules], buckets = <||>,
    hole = Unique["momentumSlot"], chains, c, ps, offset, members, rhs, pair, split, template, coefficient},
  Do[
    chains = Join[DeleteDuplicates[Select[chainsIn[mons[[t]]], validChainQ]],
      DeleteDuplicates[Select[tracesIn[mons[[t]]], validTraceQ]]];
    Do[
      c = chains[[k]]; ps = momentumSlots[c]; offset = momentumSlotOffset[c];
      Do[
        members = momentumMembers[ps[[j, 1]]];
        If[members === {}, Continue[]];
        If[Length[members] >= 2,
          rhs = ReplacePart[c, j + offset -> Mom[First[members]]] +
            ReplacePart[c, j + offset -> Mom[momentumLabel[Rest[members]]]];
          AppendTo[moves, Join[<|"Name" -> "MomentumSum", "Direction" -> "Expand",
            "Subtract" -> mons[[t]], "Insert" -> canonicalChains[mons[[t]]/c*rhs],
            "Site" -> {t, k, j}, "Legs" -> {ps[[j, 1]]}, "Identity" -> "Split"|>, traceMetadata[c]]],
          If[MemberQ[exts, First[members]],
            Do[
              pair = momentumLabel[{First[members], other}];
              rhs = ReplacePart[c, j + offset -> Mom[pair]] - ReplacePart[c, j + offset -> Mom[other]];
              AppendTo[moves, Join[<|"Name" -> "MomentumSum", "Direction" -> "Expand",
                "Subtract" -> mons[[t]], "Insert" -> canonicalChains[mons[[t]]/c*rhs],
                "Site" -> {t, k, j}, "Legs" -> {pair, other}, "Identity" -> "Introduce"|>, traceMetadata[c]]],
              {other, Complement[exts, members]}]]];
        split = splitMon[mons[[t]]];
        template = splitMon[canonicalChains[split[[2]]/c*ReplacePart[c, j + offset -> Mom[hole]]]];
        coefficient = split[[1]] template[[1]];
        buckets[template[[2]]] = Append[Lookup[buckets, template[[2]], {}],
          {ps[[j, 1]], coefficient, mons[[t]]}],
        {j, Length[ps]}], {k, Length[chains]}], {t, Length[mons]}];
  KeyValueMap[Function[{context, entries}, Module[
    {byMomentum, labels, a, b, ma, mb, ca, cb, target, signs, scales, subtract},
    byMomentum = GroupBy[DeleteDuplicates[entries], First]; labels = Keys[byMomentum];
    Do[
      a = labels[[i]]; b = labels[[j]];
      ma = momentumMembers[a]; mb = momentumMembers[b];
      ca = Total[byMomentum[a][[All, 2]]]; cb = Total[byMomentum[b][[All, 2]]];
      target = None;
      If[Intersection[ma, mb] === {}, target = momentumLabel[Join[ma, mb]]; signs = {1, 1}];
      If[Complement[mb, ma] === {} && Length[ma] > Length[mb],
        target = momentumLabel[Complement[ma, mb]]; signs = {1, -1}];
      If[Complement[ma, mb] === {} && Length[mb] > Length[ma],
        target = momentumLabel[Complement[mb, ma]]; signs = {-1, 1}];
      If[target =!= None,
        (* Collect a shared portion even when another term has changed one coefficient. *)
        scales = DeleteDuplicates[{signs[[1]] ca, signs[[2]] cb}];
        Do[
          subtract = canonicalChains[scale*((signs[[1]] context /. Mom[hole] -> Mom[a]) +
            (signs[[2]] context /. Mom[hole] -> Mom[b]))];
          AppendTo[moves, <|"Name" -> "MomentumSum", "Direction" -> "Contract",
            "Subtract" -> subtract, "Insert" -> canonicalChains[scale*(context /. Mom[hole] -> Mom[target])],
            "Legs" -> {a, b}, "Momentum" -> target, "Identity" -> "Collect"|>], {scale, scales}]],
      {i, Length[labels]}, {j, i + 1, Length[labels]}]]], buckets];
  moves
];

(* Trace moves use the same Clifford algebra, with no external spinor endpoints. *)
traceMoves[expr_, rules_] := Module[
  {moves = {}, mons = monomialsOf[expr], traces, trace, ps, short, swapped, rhs, mass, square, leg, dots},
  Do[
    traces = DeleteDuplicates[Select[tracesIn[mons[[t]]], validTraceQ]];
    Do[
      trace = traces[[k]]; ps = List @@ trace;
      Do[
        short = SpinorTrace @@ Join[Take[ps, j - 1], Drop[ps, j + 1]];
        If[ps[[j]] === ps[[j + 1]],
          mass = momentumMass[rules, ps[[j, 1]]];
          square = If[MissingQ[mass] || mass === None, ps[[j]]^2, mass^2];
          rhs = square short;
          AppendTo[moves, <|"Name" -> "ChainSquare", "Identity" -> "TraceSquare", "Direction" -> "Contract",
            "Subtract" -> mons[[t]], "Insert" -> canonicalChains[mons[[t]]/trace*rhs],
            "Trace" -> trace, "Site" -> {t, k, j}|>],
          swapped = ReplacePart[trace, {j -> ps[[j + 1]], j + 1 -> ps[[j]]}];
          rhs = 2 (orderedDot[ps[[j, 1]], ps[[j + 1, 1]]] /. rules) short - swapped;
          AppendTo[moves, <|"Name" -> "Anticommutation", "Identity" -> "TraceAnticommutation",
            "Subtract" -> mons[[t]], "Insert" -> canonicalChains[mons[[t]]/trace*rhs],
            "Trace" -> trace, "Site" -> {t, k, j}|>]
        ], {j, Length[ps] - 1}];
      If[Length[ps] > 2 && First[ps] === Last[ps],
        mass = momentumMass[rules, First[ps][[1]]];
        square = If[MissingQ[mass] || mass === None, First[ps]^2, mass^2];
        (* Crossing the trace boundary flips chirality; one rotation restores it. *)
        rhs = square (SpinorTrace @@ RotateLeft[Most[Rest[ps]]]);
        AppendTo[moves, <|"Name" -> "ChainSquare", "Identity" -> "TraceCyclicSquare", "Direction" -> "Contract",
          "Subtract" -> mons[[t]], "Insert" -> canonicalChains[mons[[t]]/trace*rhs],
          "Trace" -> trace, "Site" -> {t, k, 0}|>]];
      If[Length[ps] === 2 && First[ps] =!= Last[ps],
        AppendTo[moves, <|"Name" -> "MomentumSquare", "Identity" -> "TracePair", "Direction" -> "Contract",
          "Subtract" -> mons[[t]], "Insert" -> canonicalChains[mons[[t]]/trace*2*((orderedDot @@ (First /@ ps)) /. rules)],
          "Trace" -> trace, "Site" -> {t, k, 1}, "Legs" -> (First /@ ps)|>]];
      If[TrueQ[$useMomentumConservation],
        Do[
          leg = ps[[j, 1]];
          If[!MemberQ[externalIndices[rules], leg], Continue[]];
          rhs = -Total[ReplacePart[trace, j -> Mom[#]] & /@ Complement[externalIndices[rules], {leg}]];
          AppendTo[moves, <|"Name" -> "MomentumConservation", "Identity" -> "TraceConservation",
            "Subtract" -> mons[[t]], "Insert" -> canonicalChains[mons[[t]]/trace*rhs],
            "Trace" -> trace, "Site" -> {t, k, j}, "Momentum" -> leg|>], {j, Length[ps]}]];
      If[!AnyTrue[Partition[ps, 2, 1], #[[1]] === #[[2]] &],
        Do[
          mass = massValue[rules, leg];
          If[MissingQ[mass] || TrueQ[mass == 0], Continue[]];
          rhs = (SpinorTrace @@ Join[{Mom[leg], Mom[leg]}, ps])/mass^2;
          AppendTo[moves, <|"Name" -> "ChainSquare", "Identity" -> "TraceSquare", "Direction" -> "Expand",
            "Subtract" -> mons[[t]], "Insert" -> canonicalChains[mons[[t]]/trace*rhs],
            "Trace" -> trace, "Site" -> {t, k, leg}|>], {leg, externalIndices[rules]}]],
      {k, Length[traces]}];
    dots = DeleteDuplicates[Cases[factorsOf[mons[[t]]],
      dot_MomProd :> dot, {1}] ~Join~ Cases[factorsOf[mons[[t]]],
        Power[dot_MomProd, _Integer?Positive] :> dot, {1}]];
    Do[
      rhs = (SpinorTrace @@ (Mom /@ (List @@ dot)))/2;
      AppendTo[moves, <|"Name" -> "MomentumSquare", "Identity" -> "TracePair", "Direction" -> "Expand",
        "Subtract" -> mons[[t]], "Insert" -> canonicalChains[mons[[t]]/dot*rhs], "Legs" -> (List @@ dot)|>],
      {dot, dots}], {t, Length[mons]}];
  moves
];

applyCandidate[expr_, cand_Association] :=
  If[cand["Name"] === "Stop", expr, canonicalChains[expr - cand["Subtract"] + cand["Insert"]]];

legalMoves[expr_, massRules_List] := Module[{legacy, moves, normalizationDelta},
  legacy = Select[kinematicMoves[expr, massRules],
    MemberQ[If[TrueQ[$useMomentumConservation],
      {"MomentumConservation", "MomentumSquare", "OnShell"},
      {"MomentumSquare", "OnShell"}], #["Name"]] &];
  moves = Join[chainMoves[expr, massRules], interiorChainMoves[expr, massRules],
    momentumSumMoves[expr, massRules], traceMoves[expr, massRules],
    helicityInvariantMoves[expr, massRules], propagatorNumeratorMoves[expr, massRules],
    SchoutenCandidates[expr], legacy];
  moves = DeleteDuplicatesBy[moves, {#["Name"], #["Subtract"], #["Insert"]} &];
  (* Canonicalization is linear, so unchanged edits need only their local delta.
     Include the input correction for callers supplying noncanonical states. *)
  normalizationDelta = canonicalChains[expr] - expr;
  moves = Select[moves,
    !samePoly[canonicalChains[#["Insert"] - #["Subtract"]] + normalizationDelta, 0] &];
  Append[moves, stopMove[]]
];

UnscrambleCandidates[{massRules_List, amplitude_}, OptionsPattern[]] :=
  Block[{$onShellChannels = normalizeOnShellChannels[OptionValue["OnShellChannels"]],
      $useMomentumConservation = OptionValue["MomentumConservation"]},
    legalMoves[canonicalAmplitude[amplitude, massRules], massRules]
  ];

(* Labels are renumbered only where they denote particles, never in coefficients,
   exponents, spin indices, or split positions. Sparse and high labels are legal. *)

(* ::Subsection::Closed:: *)
(*Full-Expression Encoding*)

relabelLegs[expr_, ids_Association] := Module[{idx},
  idx[i_Integer] := Lookup[ids, i, i];
  idx[Multiparticle[ii__]] := Multiparticle @@ (idx /@ {ii});
  idx[x_] := x;
  expr /. {
    Spinor[k_, t: ("Angle" | "Square"), i_, rest___] :> Spinor[k, t, idx[i], rest],
    Spinor[k_, ud_, t: ("Angle" | "Square"), i_, rest___] :> Spinor[k, ud, t, idx[i], rest],
    Mom[i_] :> Mom[idx[i]], Mass[i_] :> Mass[idx[i]],
    MomProd[i_, j_] :> MomProd[idx[i], idx[j]],
    Mandelstahm[i_, j_] :> Mandelstahm[idx[i], idx[j]]
  }
];

stateLegIDs[expr_, rules_] := Module[{legs},
  legs = Flatten[Join[
    Cases[{expr, rules}, s_Spinor :> spinorLeg[s], Infinity],
    Cases[{expr, rules}, (Mom | Mass)[i_] :> i, Infinity],
    Cases[{expr, rules}, (MomProd | Mandelstahm)[i_, j_] :> {i, j}, Infinity]] /.
      Multiparticle[ii__] :> {ii}];
  legs = Union[Select[legs, IntegerQ]];
  AssociationThread[legs, Range[Length[legs]]]
];

encodePacket[packet_] := 1 + ToCharacterCode[ToString[FullForm[packet], PageWidth -> Infinity], "UTF8"];
(* Preserve the packet tags used to train the current weights. *)
stateFeature[expr_, rules_, ids_] := encodePacket[{
  "HEPCATStateV3", {"State", relabelLegs[expr, ids]}, {"Masses", relabelLegs[rules, ids]},
  {"Conditions", $useMomentumConservation,
    Map[Lookup[ids, #, #] &, Sort /@ (First /@ internalChannels[rules]), {2}]}}];
editFeature[cand_, ids_] := encodePacket[{
  "HEPCATEditV3", cand["Name"], relabelLegs[cand["Subtract"], ids],
  relabelLegs[cand["Insert"], ids], Lookup[cand, "Site", {}], Lookup[cand, "Splits", {}],
  relabelLegs[Lookup[cand, "Chains", {}], ids], Lookup[cand, "Direction", ""]}];

candidateFeature[expr_, rules_, cand_] := Module[{ids = stateLegIDs[expr, rules]},
  <|"State" -> stateFeature[expr, rules, ids], "Candidate" -> editFeature[cand, ids]|>
];

(* Left padding keeps the final recurrent position on actual edit content.
   257 is reserved for padding; no expression or edit bytes are dropped. *)
groupFeatures[expr_, rules_, cands_] := Module[{ids = stateLegIDs[expr, rules], edits},
  edits = editFeature[#, ids] & /@ cands;
  <|"State" -> stateFeature[expr, rules, ids],
    "Candidates" -> (PadLeft[#, Max[Length /@ edits], 257] & /@ edits)|>
];

UnscrambleEncoding[pair:{rules_List, amp_}, cand_Association, opts:OptionsPattern[UnscrambleCandidates]] :=
  Block[{$onShellChannels = normalizeOnShellChannels[OptionValue["OnShellChannels"]],
      $useMomentumConservation = OptionValue["MomentumConservation"]},
    candidateFeature[canonicalAmplitude[amp, rules], rules, cand]
  ];


(* ::Subsection::Closed:: *)
(*Compact Action Features*)

compactActionFeatureWidth = 146;
compactActionFeatureSchema = <|
  "Encoding" -> "StructuredLegalEdit", "Width" -> compactActionFeatureWidth,
  "Operations" -> {"Mass", "ChainSquare", "Anticommutation", "Schouten",
    "MomentumConservation", "MomentumSquare", "OnShell", "Stop"},
  "Blocks" -> <|"Operation" -> 8, "Direction" -> 3, "SiteKind" -> 7,
    "SiteCoordinates" -> 8, "ExplicitLegRoles" -> 28, "SchoutenSplits" -> 4,
    "FocusedChains" -> 54, "LocalSummaries" -> 30, "RewriteMarkers" -> 4|>,
  "MaximumExternalLegs" -> 4, "ParticleLabels" -> "ExistingStateLegIDs",
  "WholeResultExpression" -> False, "CandidateIndex" -> False,
  "Lossy" -> True|>;
compactActionFeatures::arity = "Compact four-point features support at most four external particle IDs, not `1`.";
compactActionFeatures::invalid = "A legal-edit descriptor is invalid or has an unsupported identity family.";

compactCount[x_] := Log[1. + Max[0., N[x]]];
compactOneHot[value_, choices_List] := N[Boole[# === value] & /@ choices];
compactMomLeg[Mom[leg_]] := leg;
compactMomLeg[_] := None;
compactLegList[leg_Integer] := {leg};
compactLegList[Multiparticle[legs__Integer]] := {legs};
compactLegList[_] := {};
compactLegVector[leg_, ids_, count_] := Module[{legs},
  legs = Sort[DeleteDuplicates[Lookup[ids, #, #] & /@ compactLegList[leg]]];
  If[legs === {}, Return[ConstantArray[0., 7]]];
  Join[N[Boole[MemberQ[legs, #]] & /@ Range[4]],
    {compactCount[Length[legs]], N[Min[legs]/Max[1, count]], N[Max[legs]/Max[1, count]]}]
];
compactLegBits[leg_, ids_] := N[Boole[MemberQ[
    Lookup[ids, #, #] & /@ compactLegList[leg], #]] & /@ Range[4]];
compactMassiveLeg[leg_, rules_] := With[{mass = massValue[rules, leg]},
  N[Boole[!MissingQ[mass] && mass =!= 0]]];
compactChirality[spinor_] := Switch[spinorType[spinor], "Angle", -1., "Square", 1., _, 0.];

(* An anchor is a split between momenta: anchor j exposes momenta j and j+1. *)
compactChainVector[None, _, _, _, _] := ConstantArray[0., 27];
compactChainVector[trace_SpinorTrace, anchor_, ids_, rules_, count_] := Module[
  {ps = List @@ trace, n = Length[trace], split, before, after},
  If[!validTraceQ[trace] || !IntegerQ[anchor], Return[$Failed]];
  split = Min[n, Max[0, anchor]];
  before = If[split > 0, compactMomLeg[ps[[split]]], None];
  after = If[split < n, compactMomLeg[ps[[split + 1]]], None];
  Join[{1., 0., 0., 0., 0.}, ConstantArray[0., 8],
    {compactCount[n], compactCount[split], N[split/Max[1, n]]},
    compactLegBits[before, ids], compactLegBits[after, ids],
    {0., N[Boole[before =!= None && before === after]], N[Mod[n, 2]]}]
];
compactChainVector[chain_, anchor_, ids_, rules_, count_] := Module[
  {xs, momenta, n, split, before, after, left, right},
  If[!validChainQ[chain] || !IntegerQ[anchor], Return[$Failed]];
  xs = List @@ chain; momenta = Drop[Rest[xs], -1]; n = Length[momenta];
  split = Min[n, Max[0, anchor]];
  before = If[split > 0, compactMomLeg[momenta[[split]]], None];
  after = If[split < n, compactMomLeg[momenta[[split + 1]]], None];
  left = spinorLeg[First[xs]]; right = spinorLeg[Last[xs]];
  Join[{1., compactMassiveLeg[left, rules], compactMassiveLeg[right, rules],
      compactChirality[First[xs]], compactChirality[Last[xs]]},
    compactLegBits[left, ids], compactLegBits[right, ids],
    {compactCount[n], compactCount[split], N[split/Max[1, n]]},
    compactLegBits[before, ids], compactLegBits[after, ids],
    {N[Boole[left === right]], N[Boole[before =!= None && before === after]], N[Mod[n, 2]]}]
];

compactNumericFactor[coefficient_] := Module[{factor},
  factor = Times @@ Select[factorsOf[coefficient], NumberQ];
  If[TrueQ[Im[N[factor]] == 0], N[Re[factor]], 0.]
];
compactLocalSummary[expr_, count_] := Module[
  {terms, chains, lengths, coefficients, numbers, symbols, factors, powers, legs, denominator},
  terms = monomialsOf[expr];
  chains = Flatten[Join[chainsIn[#], tracesIn[#]] & /@ terms];
  lengths = Length[momentumSlots[#]] & /@ chains;
  coefficients = First[splitMon[#]] & /@ terms;
  numbers = compactNumericFactor /@ coefficients;
  symbols = Length[Cases[{#}, s_Symbol /; Context[s] =!= "System`", Infinity]] & /@ coefficients;
  factors = Length[factorsOf[#]] & /@ coefficients;
  powers = Flatten[Cases[{#}, Power[_, exponent_Integer] :> Abs[exponent], Infinity] & /@ coefficients];
  legs = Keys[stateLegIDs[expr, {}]]; denominator = Max[1, Length[terms]];
  {compactCount[Length[terms]], compactCount[Length[chains]], compactCount[Total[lengths]],
    compactCount[Max[Join[{0}, lengths]]], compactCount[Count[{expr}, _MomProd, Infinity]],
    compactCount[Count[{expr}, Power[Mom[_Multiparticle], 2], Infinity]],
    compactCount[Count[{expr}, _PropDen, Infinity]], compactCount[LeafCount[expr]],
    N[Count[numbers, _?Negative]/denominator], N[Count[coefficients, _?NumberQ]/denominator],
    compactCount[Total[Abs[numbers]]], compactCount[Total[factors]/denominator],
    compactCount[Total[symbols]/denominator], compactCount[Max[Join[{0}, powers]]],
    N[Length[legs]/Max[1, count]]}
];

compactActionVector[expr_, rules_, candidate_, ids_, terms_, termAddresses_] := Module[
  {name, direction, site, siteKind, term = 0, chainIndex = 0, position = 0, side = 0,
    chains = {}, anchors = {0, 0}, chain, momenta, expanded = None, conserved = None,
    legs, splits, affected, sourceTerms, before, after, count = Length[ids], coords, focused,
    trace = Lookup[candidate, "Trace", None]},
  name = Lookup[candidate, "Name", None]; direction = Lookup[candidate, "Direction", ""];
  (* Composite linearity uses the existing momentum-rewrite feature block. *)
  If[name === "MomentumSum", name = "MomentumConservation"];
  If[!MemberQ[compactActionFeatureSchema["Operations"], name], Return[$Failed]];
  site = Lookup[candidate, "Site", {}]; splits = Lookup[candidate, "Splits", {}];
  legs = Lookup[candidate, "Legs", {}];
  If[!ListQ[site] || !ListQ[splits] || !ListQ[legs], Return[$Failed]];
  sourceTerms = monomialsOf[candidate["Subtract"]];
  affected = DeleteCases[Lookup[termAddresses, #, 0] & /@ sourceTerms, 0];
  If[Length[site] === 3,
    {term, chainIndex, position} = site;
    If[!IntegerQ[term] || !(1 <= term <= Length[terms]) || !IntegerQ[chainIndex], Return[$Failed]];
    If[trace =!= None,
      If[!validTraceQ[trace] || !MemberQ[tracesIn[terms[[term]]], trace], Return[$Failed]];
      chain = trace,
      chains = Select[chainsIn[terms[[term]]], validChainQ];
      If[MemberQ[{"ChainSquare", "Anticommutation"}, name] ||
          Lookup[candidate, "Name", None] === "MomentumSum", chains = DeleteDuplicates[chains]];
      If[!(1 <= chainIndex <= Length[chains]), Return[$Failed]];
      chain = chains[[chainIndex]]];
    chains = {chain}; momenta = momentumSlots[chain];
    Switch[name,
      "Mass", siteKind = "MassEndpoint"; side = position;
        If[!MemberQ[{1, -1}, side], Return[$Failed]];
        anchors[[1]] = If[side === 1, 0, Length[momenta]]; position = 0,
      "ChainSquare", If[direction === "Expand",
        siteKind = "ChainExpansion"; expanded = position; position = 0,
        siteKind = "Interior"; anchors[[1]] = position],
      "Anticommutation", siteKind = "Interior"; anchors[[1]] = position,
      "MomentumSquare", If[trace === None, Return[$Failed]];
        siteKind = "Interior"; anchors[[1]] = position,
      "MomentumConservation", siteKind = "ChainMomentum"; anchors[[1]] = position;
        If[!IntegerQ[position] || !(1 <= position <= Length[momenta]), Return[$Failed]];
        conserved = compactMomLeg[momenta[[position]]],
      _, Return[$Failed]],
    If[site =!= {}, Return[$Failed]];
    siteKind = Which[name === "Stop", "Stop", name === "Schouten", "Schouten", True, "Kinematic"];
    If[name === "Schouten",
      chains = Lookup[candidate, "Chains", {}];
      If[Length[chains] =!= 2 || Length[splits] =!= 2 || !VectorQ[splits, IntegerQ], Return[$Failed]];
      anchors = splits];
    conserved = Lookup[candidate, "Momentum", None];
    If[affected =!= {}, term = Min[affected]]
  ];
  If[name =!= "Schouten", splits = {0, 0}];
  chains = PadRight[chains, 2, None];
  focused = MapThread[compactChainVector[#1, #2, ids, rules, count] &, {chains, anchors}];
  If[!FreeQ[focused, $Failed], Return[$Failed]];
  before = compactLocalSummary[candidate["Subtract"], count];
  after = compactLocalSummary[candidate["Insert"], count];
  coords = {compactCount[term], N[term/Max[1, Length[terms]]], compactCount[chainIndex],
    N[chainIndex/Max[1, Length[chainsIn[If[term > 0, terms[[term]], 0]]]]],
    compactCount[position], N[position/Max[1, If[chains[[1]] === None, 0, Length[momentumSlots[chains[[1]]]]]]],
    N[Boole[side === 1]], N[Boole[side === -1]]};
  Join[compactOneHot[name, compactActionFeatureSchema["Operations"]],
    compactOneHot[direction, {"", "Contract", "Expand"}],
    compactOneHot[siteKind, {"Stop", "MassEndpoint", "Interior", "ChainExpansion",
      "ChainMomentum", "Schouten", "Kinematic"}], coords,
    Flatten[compactLegVector[#, ids, count] & /@ {expanded, conserved,
      If[Length[legs] >= 1, legs[[1]], None], If[Length[legs] >= 2, legs[[2]], None]}],
    {compactCount[splits[[1]]], compactCount[splits[[2]]],
      N[splits[[1]]/Max[1, If[chains[[1]] === None, 0, Length[momentumSlots[chains[[1]]]]]]],
      N[splits[[2]]/Max[1, If[chains[[2]] === None, 0, Length[momentumSlots[chains[[2]]]]]]]},
    Flatten[focused], before, after,
    N[Boole[!FreeQ[#, _MomProd]] & /@ {candidate["Subtract"], candidate["Insert"]}],
    N[Boole[!FreeQ[#, Power[Mom[_Multiparticle], 2]]] & /@
      {candidate["Subtract"], candidate["Insert"]}]]
];

compactActionFeatures[expr_, rules_, candidates_List] := Module[{ids, terms, addresses, matrix},
  ids = stateLegIDs[expr, rules];
  If[Length[ids] > 4, Message[compactActionFeatures::arity, Length[ids]]; Return[$Failed]];
  terms = monomialsOf[expr]; addresses = AssociationThread[terms, Range[Length[terms]]];
  matrix = compactActionVector[expr, rules, #, ids, terms, addresses] & /@ candidates;
  If[!FreeQ[matrix, $Failed] || !AllTrue[matrix,
      Length[#] === compactActionFeatureWidth && VectorQ[#, NumberQ] &&
        AllTrue[#, TrueQ[Im[#] == 0 && Abs[#] < Infinity] &] &],
    Message[compactActionFeatures::invalid]; Return[$Failed]];
  N[matrix]
];


(* ::Subsection::Closed:: *)
(*Neural Network*)

$maskCandidatePadding = False;
$policyMethod = "CandidateScorer";
$moveSlots = 512;

(* The experimental policy predicts slots in the deterministic legal move list.
   Only the current state enters the net. Never truncate an overflowing list. *)
(* Pool local features across the whole packet; a shared suffix must not erase
   differences in earlier expression bytes. Keep the scorer's GRU unchanged. *)
makeMoveEncoder[] := NetGraph[<|
  "Embed" -> EmbeddingLayer[24, 256],
  "Local" -> NetChain[{ConvolutionLayer[64, {5}, PaddingSize -> 2, Interleaving -> True], Ramp,
    ConvolutionLayer[64, {5}, PaddingSize -> 2, Interleaving -> True], Ramp}],
  "Mean" -> AggregationLayer[Mean, 1], "Max" -> AggregationLayer[Max, 1],
  "Join" -> CatenateLayer[]|>,
  {NetPort["Input"] -> "Embed" -> "Local", "Local" -> {"Mean", "Max"},
    {"Mean", "Max"} -> "Join"}, "Input" -> {"Varying"}];

makeMovePolicy[] := NetGraph[<|"StateEncoder" -> makeMoveEncoder[],
  "Actions" -> NetChain[{LinearLayer[128], Ramp, LinearLayer[$moveSlots]}]|>,
  {NetPort["State"] -> "StateEncoder" -> "Actions"}];

moveFeatures[expr_, rules_] := <|"State" -> stateFeature[expr, rules, stateLegIDs[expr, rules]]|>;
policyArchitecture[] := If[$policyMethod === "ReverseMoves", "ReverseMoveSlots",
  If[TrueQ[$maskCandidatePadding], "MaskedSharedStateGRU", "SharedStateGRU"]];

makeEncoder[classes_, width_:"Varying"] := NetChain[{
  EmbeddingLayer[24, classes], GatedRecurrentLayer[64], SequenceLastLayer[]
}, "Input" -> {width}];

makeMaskedEncoder[width_:"Varying"] := Module[{gru, cell},
  (* Unfold requires initialized dimensions; use the training pipeline's fixed seed. *)
  gru = NetUnfold[NetInitialize[GatedRecurrentLayer[64,
    "Input" -> {"Varying", 24}], RandomSeeding -> 1]];
  cell = NetGraph[<|"Embed" -> EmbeddingLayer[24, 257],
    "Mask" -> EmbeddingLayer[64, 257,
      "Weights" -> Join[ConstantArray[1., {256, 64}], {ConstantArray[0., 64]}],
      LearningRateMultipliers -> None],
    "GRU" -> gru, "Choose" -> ThreadingLayer[#1 #2 + (1 - #1) #3 &]|>,
    {NetPort["Input"] -> "Embed" -> NetPort["GRU", "Input"],
      NetPort["Input"] -> "Mask", NetPort["State"] -> NetPort["GRU", "State1"],
      {"Mask", NetPort["GRU", "Output"], NetPort["State"]} -> "Choose",
      "Choose" -> NetPort["Output"], NetPort["GRU", "OutState1"] -> None}];
  (* Padding freezes the recurrent state, including after learned biases change. *)
  NetChain[{NetFoldOperator[cell, {"Output" -> "State"}], SequenceLastLayer[]},
    "Input" -> {width, Restricted["Integer", 257]}]
];

makePolicy[width_:"Varying"] := Module[{head},
  If[$policyMethod === "ReverseMoves", Return[makeMovePolicy[]]];
  head = NetGraph[<|"Join" -> CatenateLayer[],
    "Score" -> NetChain[{LinearLayer[32], Ramp, LinearLayer[1], LogisticSigmoid}]|>,
    {{NetPort["State"], NetPort["Candidate"]} -> "Join" -> "Score"},
    "State" -> 64, "Candidate" -> 64];
  NetGraph[<|"StateEncoder" -> makeEncoder[256],
    "CandidateEncoder" -> NetMapOperator[If[TrueQ[$maskCandidatePadding],
      makeMaskedEncoder[width], makeEncoder[257, width]]],
    "Scorer" -> NetMapThreadOperator[head, <|"Candidate" -> 1|>]|>,
    {NetPort["State"] -> "StateEncoder" -> NetPort["Scorer", "State"],
      NetPort["Candidates"] -> "CandidateEncoder" -> NetPort["Scorer", "Candidate"]}]
];


compactParticleUnitLayer[n_] := FunctionLayer[#/Sqrt[Total[#^2] + 1.*^-12] &, "Input" -> n];

compactParticleStateEncoder[] := NetGraph[<|
  "Embed" -> EmbeddingLayer[24, 256],
  "Local" -> NetChain[{ConvolutionLayer[64, {5}, PaddingSize -> 2, Interleaving -> True], Ramp,
    ConvolutionLayer[64, {5}, PaddingSize -> 2, Interleaving -> True], Ramp}],
  "Mean" -> AggregationLayer[Mean, 1], "Max" -> AggregationLayer[Max, 1],
  "MeanUnit" -> compactParticleUnitLayer[64], "MaxUnit" -> compactParticleUnitLayer[64],
  "Join" -> CatenateLayer[]|>,
  {NetPort["Input"] -> "Embed" -> "Local", "Local" -> {"Mean", "Max"},
    "Mean" -> "MeanUnit", "Max" -> "MaxUnit", {"MeanUnit", "MaxUnit"} -> "Join"},
  "Input" -> {"Varying"}];

compactParticleMaskWeights[] := Table[N[Boole[147 <= j <= 149]], {64}, {j, 166}];

(* Mask after the token transform, so trained biases cannot affect padding. *)
compactParticleEncoder[tokens_:"Varying"] := NetGraph[<|
  "Tokens" -> NetMapOperator[NetChain[{LinearLayer[64], Ramp}]],
  "Mask" -> NetMapOperator[LinearLayer[64, "Weights" -> compactParticleMaskWeights[],
    "Biases" -> ConstantArray[0., 64], LearningRateMultipliers -> None]],
  "Masked" -> ThreadingLayer[Times], "Sum" -> AggregationLayer[Total, 1],
  "Count" -> AggregationLayer[Total, 1],
  "Mean" -> ThreadingLayer[#1/Max[#2, 1.] &], "Max" -> AggregationLayer[Max, 1],
  "MeanUnit" -> compactParticleUnitLayer[64], "MaxUnit" -> compactParticleUnitLayer[64],
  "Join" -> CatenateLayer[]|>,
  {NetPort["Input"] -> {"Tokens", "Mask"}, {"Tokens", "Mask"} -> "Masked",
    "Masked" -> {"Sum", "Max"}, "Mask" -> "Count",
    {"Sum", "Count"} -> "Mean" -> "MeanUnit", "Max" -> "MaxUnit",
    {"MeanUnit", "MaxUnit"} -> "Join"}, "Input" -> {tokens, 166}];

compactParticlePolicy[count_, tokens_Integer?Positive] := Module[{head},
  head = NetGraph[<|"Join" -> CatenateLayer[],
    "Score" -> NetChain[{LinearLayer[64], Ramp, LinearLayer[1], PartLayer[1]}]|>,
    {{NetPort["State"], NetPort["Candidate"]} -> "Join" -> "Score"},
    "State" -> 128, "Candidate" -> 128];
  NetGraph[<|"StateEncoder" -> compactParticleStateEncoder[],
    "CandidateEncoder" -> NetMapOperator[compactParticleEncoder[tokens]],
    "Scorer" -> NetMapThreadOperator[head, <|"Candidate" -> 1|>]|>,
    {NetPort["State"] -> "StateEncoder" -> NetPort["Scorer", "State"],
      NetPort["Candidates"] -> "CandidateEncoder" -> NetPort["Scorer", "Candidate"]},
    "Candidates" -> {count, tokens, 166}]
];
compactParticlePolicy[___] := $Failed;

compactParticleTrainer[count_Integer?Positive, tokens_Integer?Positive] := NetGraph[<|
  "Policy" -> compactParticlePolicy[count, tokens],
  "Mask" -> ThreadingLayer[If[#2 > 0., #1, -1.*^30] &],
  "Probabilities" -> SoftmaxLayer[], "Loss" -> CrossEntropyLossLayer["Probabilities"]|>,
  {NetPort["State"] -> NetPort["Policy", "State"],
    NetPort["Candidates"] -> NetPort["Policy", "Candidates"],
    {"Policy", NetPort["LegalMask"]} -> "Mask" -> "Probabilities" -> NetPort["Loss", "Input"],
    NetPort["Target"] -> NetPort["Loss", "Target"]},
  "Candidates" -> {count, tokens, 166}, "LegalMask" -> count, "Target" -> count];
compactParticleTrainer[___] := $Failed;

variableCompactParticlePolicy[net_NetGraph, tokens_:Automatic] := Module[{shape, tokenCount, policy},
  shape = Lookup[Information[net, "InputPorts"], "Candidates", None];
  If[!MatchQ[shape, {_, _Integer?Positive, 166}], Return[$Failed]];
  tokenCount = If[tokens === Automatic, shape[[2]], tokens];
  If[!IntegerQ[tokenCount] || tokenCount <= 0, Return[$Failed]];
  policy = compactParticlePolicy["Varying", tokenCount];
  NetReplacePart[policy, (# -> NetExtract[net, #] & /@ Information[net, "ArraysPositionList"])]
];
variableCompactParticlePolicy[___] := $Failed;


(* ::Subsection::Closed:: *)
(*Compact Policy Evaluation*)

(* Evaluate the saved compact network arrays without the native neural runtime. *)
compactPolicyArrays[net_NetGraph] := Module[{paths, dimensions, arrays},
  paths = <|"Embedding" -> {"StateEncoder", "Embed", "Weights"},
    "Conv1Weights" -> {"StateEncoder", "Local", 1, "Weights"},
    "Conv1Biases" -> {"StateEncoder", "Local", 1, "Biases"},
    "Conv2Weights" -> {"StateEncoder", "Local", 3, "Weights"},
    "Conv2Biases" -> {"StateEncoder", "Local", 3, "Biases"},
    "HiddenWeights" -> {"Scorer", "Net", "Score", 1, "Weights"},
    "HiddenBiases" -> {"Scorer", "Net", "Score", 1, "Biases"},
    "OutputWeights" -> {"Scorer", "Net", "Score", 3, "Weights"},
    "OutputBiases" -> {"Scorer", "Net", "Score", 3, "Biases"}|>;
  dimensions = <|"Embedding" -> {256, 24}, "Conv1Weights" -> {64, 24, 5},
    "Conv1Biases" -> {64}, "Conv2Weights" -> {64, 64, 5}, "Conv2Biases" -> {64},
    "HiddenWeights" -> {64, 274}, "HiddenBiases" -> {64},
    "OutputWeights" -> {1, 64}, "OutputBiases" -> {1}|>;
  arrays = Quiet[Check[Map[Developer`ToPackedArray[N[Normal[NetExtract[net, #]]]] &, paths], $Failed]];
  If[arrays === $Failed || !AllTrue[Keys[paths],
      Dimensions[arrays[#]] === dimensions[#] &&
        VectorQ[Flatten[arrays[#]], NumberQ] &&
        AllTrue[MinMax[Flatten[arrays[#]]], TrueQ[Im[#] == 0 && Abs[#] < Infinity] &] &], Return[$Failed]];
  arrays
];
compactPolicyArrays[_] := $Failed;

compactPortableConvolution[input_, weights_, biases_] := Module[{count, padded, output},
  count = Length[input];
  padded = Join[ConstantArray[0., {2, Length[First[input]]}], input,
    ConstantArray[0., {2, Length[First[input]]}]];
  output = Total[Table[
    padded[[Range[count] + offset - 1]] . Transpose[weights[[All, All, offset]]],
    {offset, 5}]];
  Ramp[(# + biases) & /@ output]
];

compactPolicyStateVector[arrays_Association, state_] := Module[{encoded, mean, maximum, vector},
  If[!VectorQ[state, IntegerQ] || state === {} || Min[state] < 1 || Max[state] > 256, Return[$Failed]];
  encoded = arrays["Embedding"][[state]];
  encoded = compactPortableConvolution[encoded, arrays["Conv1Weights"], arrays["Conv1Biases"]];
  encoded = compactPortableConvolution[encoded, arrays["Conv2Weights"], arrays["Conv2Biases"]];
  mean = Mean[encoded]; maximum = Max /@ Transpose[encoded];
  vector = Join[mean/Sqrt[Total[mean^2] + 1.*^-12],
    maximum/Sqrt[Total[maximum^2] + 1.*^-12]];
  If[VectorQ[vector, NumberQ] && Length[vector] === 128 &&
      AllTrue[MinMax[vector], TrueQ[Im[#] == 0 && Abs[#] < Infinity] &], vector, $Failed]
];
compactPolicyStateVector[___] := $Failed;

compactPolicyCandidateScores[arrays_Association, stateVector_List, candidates_] := Module[
  {features, weights, stateProjection, hidden, scores},
  If[Length[stateVector] =!= 128 || !VectorQ[stateVector, NumberQ] ||
      !MatrixQ[candidates, NumberQ] || Length[candidates] < 1 || Dimensions[candidates][[2]] =!= 146,
    Return[$Failed]];
  features = Developer`ToPackedArray[N[candidates]];
  If[!AllTrue[MinMax[Flatten[features]], TrueQ[Im[#] == 0 && Abs[#] < Infinity] &], Return[$Failed]];
  weights = arrays["HiddenWeights"];
  stateProjection = weights[[All, 1 ;; 128]] . stateVector + arrays["HiddenBiases"];
  hidden = features . Transpose[weights[[All, 129 ;; 274]]];
  hidden = Ramp[(# + stateProjection) & /@ hidden];
  scores = hidden . First[arrays["OutputWeights"]] + First[arrays["OutputBiases"]];
  If[VectorQ[scores, NumberQ] &&
      AllTrue[MinMax[scores], TrueQ[Im[#] == 0 && Abs[#] < Infinity] &], scores, $Failed]
];
compactPolicyCandidateScores[___] := $Failed;

compactPolicyScores[arrays_Association, input_Association] := Module[{stateVector},
  stateVector = compactPolicyStateVector[arrays, Lookup[input, "State", None]];
  If[stateVector === $Failed, Return[$Failed]];
  compactPolicyCandidateScores[arrays, stateVector, Lookup[input, "Candidates", None]]
];
compactPolicyScores[___] := $Failed;

compactParticlePolicyArrays[net_NetGraph] := Module[{paths, dimensions, arrays},
  paths = <|"Embedding" -> {"StateEncoder", "Embed", "Weights"},
    "Conv1Weights" -> {"StateEncoder", "Local", 1, "Weights"},
    "Conv1Biases" -> {"StateEncoder", "Local", 1, "Biases"},
    "Conv2Weights" -> {"StateEncoder", "Local", 3, "Weights"},
    "Conv2Biases" -> {"StateEncoder", "Local", 3, "Biases"},
    "TokenWeights" -> {"CandidateEncoder", "Net", "Tokens", "Net", 1, "Weights"},
    "TokenBiases" -> {"CandidateEncoder", "Net", "Tokens", "Net", 1, "Biases"},
    "TokenMaskWeights" -> {"CandidateEncoder", "Net", "Mask", "Net", "Weights"},
    "TokenMaskBiases" -> {"CandidateEncoder", "Net", "Mask", "Net", "Biases"},
    "HiddenWeights" -> {"Scorer", "Net", "Score", 1, "Weights"},
    "HiddenBiases" -> {"Scorer", "Net", "Score", 1, "Biases"},
    "OutputWeights" -> {"Scorer", "Net", "Score", 3, "Weights"},
    "OutputBiases" -> {"Scorer", "Net", "Score", 3, "Biases"}|>;
  dimensions = <|"Embedding" -> {256, 24}, "Conv1Weights" -> {64, 24, 5},
    "Conv1Biases" -> {64}, "Conv2Weights" -> {64, 64, 5}, "Conv2Biases" -> {64},
    "TokenWeights" -> {64, 166}, "TokenBiases" -> {64},
    "TokenMaskWeights" -> {64, 166}, "TokenMaskBiases" -> {64},
    "HiddenWeights" -> {64, 256}, "HiddenBiases" -> {64},
    "OutputWeights" -> {1, 64}, "OutputBiases" -> {1}|>;
  arrays = Quiet[Check[Map[Developer`ToPackedArray[N[Normal[NetExtract[net, #]]]] &, paths], $Failed]];
  If[arrays === $Failed || !AllTrue[Keys[paths],
      Dimensions[arrays[#]] === dimensions[#] &&
        VectorQ[Flatten[arrays[#]], NumberQ] &&
        AllTrue[MinMax[Flatten[arrays[#]]], TrueQ[Im[#] == 0 && Abs[#] < Infinity] &] &] ||
      arrays["TokenMaskWeights"] =!= compactParticleMaskWeights[] ||
      arrays["TokenMaskBiases"] =!= ConstantArray[0., 64], Return[$Failed]];
  arrays
];
compactParticlePolicyArrays[_] := $Failed;

compactParticleCandidateScores[arrays_Association, stateVector_List, candidates_] := Module[
  {features, dimensions, candidateVectors,
    weights, stateProjection, hidden, scores},
  dimensions = Dimensions[candidates];
  If[Length[stateVector] =!= 128 || !VectorQ[stateVector, NumberQ] ||
      Length[dimensions] =!= 3 || dimensions[[1]] < 1 || dimensions[[2]] < 1 ||
      dimensions[[3]] =!= 166 || !ArrayQ[candidates, 3, NumberQ], Return[$Failed]];
  features = Developer`ToPackedArray[N[candidates]];
  If[!AllTrue[MinMax[Flatten[features]], TrueQ[Im[#] == 0 && Abs[#] < Infinity] &], Return[$Failed]];
  candidateVectors = Map[Function[candidate, Module[{mask, values, tokenMean, tokenMax},
    mask = Total[Transpose[candidate[[All, 147 ;; 149]]]];
    If[!AllTrue[mask, TrueQ[# == 0. || # == 1.] &], Return[$Failed]];
    values = Ramp[(# + arrays["TokenBiases"]) & /@
      (candidate . Transpose[arrays["TokenWeights"]])];
    values = MapThread[#1 #2 &, {values, mask}];
    tokenMean = Total[values]/Max[Total[mask], 1.]; tokenMax = Max /@ Transpose[values];
    Join[tokenMean/Sqrt[Total[tokenMean^2] + 1.*^-12],
      tokenMax/Sqrt[Total[tokenMax^2] + 1.*^-12]]
  ]], features];
  If[!FreeQ[candidateVectors, $Failed], Return[$Failed]];
  weights = arrays["HiddenWeights"];
  stateProjection = weights[[All, 1 ;; 128]] . stateVector + arrays["HiddenBiases"];
  hidden = candidateVectors . Transpose[weights[[All, 129 ;; 256]]];
  hidden = Ramp[(# + stateProjection) & /@ hidden];
  scores = hidden . First[arrays["OutputWeights"]] + First[arrays["OutputBiases"]];
  If[VectorQ[scores, NumberQ] &&
      AllTrue[MinMax[scores], TrueQ[Im[#] == 0 && Abs[#] < Infinity] &], scores, $Failed]
];
compactParticleCandidateScores[___] := $Failed;

compactParticlePolicyScores[arrays_Association, input_Association] := Module[{stateVector},
  stateVector = compactPolicyStateVector[arrays, Lookup[input, "State", None]];
  If[stateVector === $Failed, Return[$Failed]];
  compactParticleCandidateScores[arrays, stateVector, Lookup[input, "Candidates", None]]
];
compactParticlePolicyScores[___] := $Failed;


(* ::Subsection::Closed:: *)
(*Particle-Aware Compact Features*)

compactParticleFeatureWidth = 166;
compactParticleFeatureSchema = <|
  "Encoding" -> "ParticleRoleTokens", "Width" -> compactParticleFeatureWidth,
  "OverviewWidth" -> 146, "TokenKinds" -> {"Overview", "LegRole", "ChainElement"},
  "RoleCount" -> 12,
  "Coordinates" -> {"CanonicalParticleID", "SubsetSize", "ChainPosition", "ChainLength", "ParticleCount"},
  "MaximumExternalLegs" -> Infinity, "ParticleLabels" -> "ExistingStateLegIDs",
  "MomentumSubsets" -> "EveryMember", "FocusedChains" -> "CompleteOrderedMomenta",
  "Padding" -> "ZeroTokensMaskedBeforePooling", "WholeResultExpression" -> False,
  "CandidateIndex" -> False, "LossyLocalSummaries" -> True|>;
compactParticleFeatures::invalid = "A particle-aware legal-edit descriptor is invalid.";

compactParticleToken[kind_, role_, particle_, size_, position_, length_, count_] :=
  Join[ConstantArray[0., 146], compactOneHot[kind, {"Overview", "LegRole", "ChainElement"}],
    compactOneHot[role, Range[12]], N[{particle, size, position, length, count}]];

compactParticleLegTokens[leg_, kind_, role_, position_, length_, ids_] := Module[{members},
  members = Sort[DeleteDuplicates[compactLegList[leg]]];
  If[members === {}, Return[{compactParticleToken[kind, role, 0, 0, position, length, Length[ids]]}]];
  If[!AllTrue[members, KeyExistsQ[ids, #] &], Return[$Failed]];
  compactParticleToken[kind, role, ids[#], Length[members], position, length, Length[ids]] & /@ members
];

(* Local statistics remain small; particle membership and chain order live in
   separate tokens, not in a fixed four- or five-particle one-hot vector. *)
compactParticleOverview[expr_, rules_, candidate_, ids_, terms_, addresses_] :=
  Block[{compactLegBits, compactLegVector},
    compactLegBits[_, _] := ConstantArray[0., 4];
    compactLegVector[leg_, labels_, count_] := Module[{members},
      members = Sort[DeleteDuplicates[Lookup[labels, #, #] & /@ compactLegList[leg]]];
      Join[ConstantArray[0., 4], If[members === {}, ConstantArray[0., 3],
        {compactCount[Length[members]], N[Min[members]/Max[1, count]], N[Max[members]/Max[1, count]]}]]
    ];
    compactActionVector[expr, rules, candidate, ids, terms, addresses]
  ];

compactParticleAction[expr_, rules_, candidate_, ids_, terms_, addresses_] := Module[
  {overview, site, name, chains = {}, termChains, expanded = None, conserved = None,
    legs, roles, tokens, parts, xs, momenta, left, right, index, position,
    trace = Lookup[candidate, "Trace", None]},
  overview = compactParticleOverview[expr, rules, candidate, ids, terms, addresses];
  If[overview === $Failed, Return[$Failed]];
  name = candidate["Name"]; site = Lookup[candidate, "Site", {}];
  legs = Lookup[candidate, "Legs", {}];
  If[Length[site] === 3,
    If[trace =!= None,
      chains = {trace},
      termChains = Select[chainsIn[terms[[site[[1]]]]], validChainQ];
      If[MemberQ[{"ChainSquare", "Anticommutation", "MomentumSum"}, name], termChains = DeleteDuplicates[termChains]];
      chains = {termChains[[site[[2]]]]}];
    If[name === "ChainSquare" && Lookup[candidate, "Direction", ""] === "Expand", expanded = site[[3]]];
    If[MemberQ[{"MomentumConservation", "MomentumSum"}, name],
      conserved = compactMomLeg[momentumSlots[First[chains]][[site[[3]]]]]],
    If[name === "Schouten", chains = Lookup[candidate, "Chains", {}]];
    conserved = Lookup[candidate, "Momentum", None]
  ];
  roles = {expanded, conserved, If[Length[legs] >= 1, legs[[1]], None],
    If[Length[legs] >= 2, If[Length[legs] > 2, Multiparticle @@ Rest[legs], legs[[2]]], None]};
  tokens = {Join[overview, compactOneHot["Overview", {"Overview", "LegRole", "ChainElement"}],
    ConstantArray[0., 12], N[{0, 0, 0, 0, Length[ids]}]]};
  Do[
    parts = compactParticleLegTokens[roles[[index]], "LegRole", index, 0, 0, ids];
    If[parts === $Failed, Return[$Failed]]; tokens = Join[tokens, parts],
    {index, Length[roles]}];
  Do[
    xs = List @@ chains[[index]]; momenta = momentumSlots[chains[[index]]];
    parts = {};
    If[Head[chains[[index]]] === SpinorChain,
      left = spinorLeg[First[xs]]; right = spinorLeg[Last[xs]];
      parts = Join[
        compactParticleLegTokens[left, "ChainElement", 5 + 3 (index - 1), 0, Length[momenta], ids],
        compactParticleLegTokens[right, "ChainElement", 6 + 3 (index - 1), Length[momenta] + 1, Length[momenta], ids]]];
    Do[
      parts = Join[parts, compactParticleLegTokens[compactMomLeg[momenta[[position]]],
        "ChainElement", 7 + 3 (index - 1), position, Length[momenta], ids]],
      {position, Length[momenta]}];
    If[!FreeQ[parts, $Failed], Return[$Failed]]; tokens = Join[tokens, parts],
    {index, Length[chains]}];
  If[!MatrixQ[tokens, NumberQ] || Dimensions[tokens][[2]] =!= compactParticleFeatureWidth ||
      !AllTrue[Flatten[tokens], TrueQ[Im[#] == 0 && Abs[#] < Infinity] &], Return[$Failed]];
  N[tokens]
];

compactParticleFeatures[expr_, rules_, candidates_List] := Module[{ids, terms, addresses, descriptors, length},
  If[candidates === {}, Return[$Failed]];
  ids = stateLegIDs[expr, rules]; terms = monomialsOf[expr];
  addresses = AssociationThread[terms, Range[Length[terms]]];
  descriptors = compactParticleAction[expr, rules, #, ids, terms, addresses] & /@ candidates;
  If[!FreeQ[descriptors, $Failed], Message[compactParticleFeatures::invalid]; Return[$Failed]];
  length = Max[Length /@ descriptors];
  Developer`ToPackedArray[PadRight[#, {length, compactParticleFeatureWidth}, 0.] & /@ descriptors]
];


(* ::Subsection::Closed:: *)
(*Model Files*)

(* Pin the legal identities and encodings, not unrelated notebook or training code. *)
compactSourceSection[text_String, first_String, last_String] := Module[
  {lines, starts, ends, body},
  lines = StringSplit[StringReplace[text, "\r\n" -> "\n"], "\n", All];
  starts = Flatten[Position[lines, first, {1}]];
  ends = Flatten[Position[lines, last, {1}]];
  If[Length[starts] =!= 1 || Length[ends] =!= 1 || First[starts] >= First[ends], Return[$Failed]];
  body = StringTrim[StringRiffle[Take[lines, {First[starts] + 1, First[ends] - 1}], "\n"]];
  If[StringEndsQ[body, "(* ::Subsection::Closed:: *)"],
    body = StringTrim[StringDrop[body, -StringLength["(* ::Subsection::Closed:: *)"]]]];
  body
];
compactCoreSourceFingerprint[source_String] := Module[{blocks},
  blocks = <|
    "LegalMath" -> compactSourceSection[source, "(*Generalized Schouten Identities*)", "(*Policy Settings*)"],
    "LegalEditsAndStateEncoding" -> compactSourceSection[source, "(*Legal Candidate Edits*)", "(*Compact Action Features*)"],
    "CompactActionEncoding" -> compactSourceSection[source, "(*Compact Action Features*)", "(*Neural Network*)"]|>;
  If[MemberQ[Values[blocks], $Failed], $Failed, IntegerString[Hash[blocks, "SHA256"], 16, 64]]
];
$compactSourceFingerprint = Module[{stream, source},
  stream = OpenRead[FileNameJoin[{$modelDirectory, "unscrambling.wl"}], BinaryFormat -> True];
  source = FromCharacterCode[BinaryReadList[stream, "Byte"], "UTF8"]; Close[stream];
  If[StringQ[source], compactCoreSourceFingerprint[source], $Failed]
];
compactModelCompatibleQ[meta_Association] :=
  Lookup[meta, "Encoding", None] === "StateUTF8AndNumericActions" &&
  Lookup[meta, "Architecture", None] === "CompactReverseMoveActions" &&
  Lookup[meta, "DescriptorEncoding", None] === "CompactStructuredActions" &&
  Lookup[meta, "DescriptorFeatureWidth", None] === compactActionFeatureWidth &&
  Lookup[meta, "CompactFeatureSchema", None] === compactActionFeatureSchema &&
  Lookup[meta, "Objective", None] === "LegalMoveCrossEntropy" &&
  Lookup[meta, "ReadoutNormalization", None] === "SeparateMeanMaxL2" &&
  Lookup[meta, "ReadoutSquaredNormEpsilon", None] === 1.*^-12 &&
  Lookup[meta, "RuntimeCoreSourceEncoding", None] === "SectionedLegalMathAndCompactActions" &&
  StringQ[$compactSourceFingerprint] &&
  (* Known earlier descriptors are unchanged; unknown source revisions still fail. *)
  MemberQ[{$compactSourceFingerprint,
    "cf001b379916e10aab582c97e2f8d08d566466163658a4009b4998de57b483a7"},
    Lookup[meta, "RuntimeCoreSourceSHA256", None]];

$compactDescriptorEncoding = None;
$compactParticleSourceFingerprint = Module[{stream, source, tokens},
  stream = OpenRead[FileNameJoin[{$modelDirectory, "unscrambling.wl"}], BinaryFormat -> True];
  source = FromCharacterCode[BinaryReadList[stream, "Byte"], "UTF8"]; Close[stream];
  tokens = compactSourceSection[source, "(*Particle-Aware Compact Features*)", "(*Model Files*)"];
  If[StringQ[$compactSourceFingerprint] && StringQ[tokens],
    IntegerString[Hash[{$compactSourceFingerprint, tokens}, "SHA256"], 16, 64], $Failed]
];
compactParticleModelCompatibleQ[meta_Association] :=
  Lookup[meta, "Encoding", None] === "StateUTF8AndNumericActions" &&
  Lookup[meta, "Architecture", None] === "CompactParticleActions" &&
  Lookup[meta, "DescriptorEncoding", None] === "ParticleRoleTokens" &&
  Lookup[meta, "DescriptorFeatureWidth", None] === compactParticleFeatureWidth &&
  Lookup[meta, "CompactFeatureSchema", None] === compactParticleFeatureSchema &&
  MemberQ[{"LegalMoveCrossEntropy", "SampledLegalMoveCrossEntropy"}, Lookup[meta, "Objective", None]] &&
  Lookup[meta, "ReadoutNormalization", None] === "SeparateMeanMaxL2" &&
  Lookup[meta, "ReadoutSquaredNormEpsilon", None] === 1.*^-12 &&
  IntegerQ[Lookup[meta, "NativeTokenCount", None]] && meta["NativeTokenCount"] > 0 &&
  Lookup[meta, "RuntimeEvaluation", None] === "PortableVariableParticleTokens" &&
  Lookup[meta, "RuntimeCoreSourceEncoding", None] === "SectionedLegalMathAndParticleTokens" &&
  StringQ[$compactParticleSourceFingerprint] &&
  MemberQ[{$compactParticleSourceFingerprint,
    "62ed48a7d3049905b07fd3cc4a28b2c05dc8d89d1a820440ca8e4bae4cde773a",
    "76ba389ad5345681abba758755754a20e48d9c91144c3e2cbe78533f8bd121d2",
    (* Local-delta no-op checks preserve this earlier model's complete boards. *)
    "2efe34e2701e18b9d95f66dff1a398ed13e94ca6c52647ed4ba0cb9cc29561f3",
    (* Photon identities expand legal boards without changing state or action encoders. *)
    "3b8907fa37e66726effa44cb64234697978c90a5af9d7c2d0c43d6119e149404",
    (* The linear-conservation correction removes invalid edits; encodings are unchanged. *)
    "ed0befdddb5f95f968e9b6abac4f1828495d18ce60b975fe5d4cedf62f37c615"},
    Lookup[meta, "RuntimeCoreSourceSHA256", None]];

modelPaths[dir_] := With[{stem = Switch[$policyMethod,
    "CompactActions", "unscramble-compact", "ReverseMoves", "unscramble-moves", _, "unscramble"]},
  <|"Net" -> FileNameJoin[{dir, stem <> ".wlnet"}],
    "Metadata" -> FileNameJoin[{dir, stem <> ".m"}]|>];
resolveModelDirectory[Automatic] := $modelDirectory;
resolveModelDirectory[dir_String] := ExpandFileName[dir];

loadUnscrambleModel[dir_] := Module[{paths, meta, net, arrays = None, encoding = None, ports},
  If[$loadedModelDirectory === dir && $loadedPolicyMethod === $policyMethod &&
      ($policyMethod =!= "CompactActions" || AssociationQ[$compactWeights]) &&
      MatchQ[$modelNet, _NetChain | _NetGraph], Return[True]];
  paths = modelPaths[dir];
  If[!AllTrue[Values[paths], FileExistsQ], Return[False]];
  meta = Get[paths["Metadata"]];
  If[!AssociationQ[meta], Return[False]];
  If[$policyMethod === "CompactActions",
    encoding = Lookup[meta, "DescriptorEncoding", None];
    If[!If[encoding === "ParticleRoleTokens", compactParticleModelCompatibleQ[meta],
        compactModelCompatibleQ[meta]], Return[False]],
    If[Lookup[meta, "Encoding", None] =!= "SplitFullFormUTF8" ||
        !MemberQ[If[$policyMethod === "ReverseMoves", {"ReverseMoveSlots"},
          {"SharedStateGRU", "MaskedSharedStateGRU"}], Lookup[meta, "Architecture", None]], Return[False]]];
  If[$policyMethod === "ReverseMoves" &&
      (Lookup[meta, "MoveSlots", None] =!= $moveSlots ||
       Lookup[meta, "Objective", None] =!= "LegalMoveCrossEntropy" ||
       Lookup[meta, "MoveSourceHash", None] =!= FileHash[FileNameJoin[{$modelDirectory, "unscrambling.wl"}], "SHA256"]),
    Return[False]];
  net = Import[paths["Net"]];
  If[!MatchQ[net, _NetChain | _NetGraph], Return[False]];
  If[$policyMethod === "CompactActions",
    ports = <|"State" -> {"Varying", Restricted["Integer", 256]},
      "Candidates" -> If[encoding === "ParticleRoleTokens",
        {"Varying", meta["NativeTokenCount"], compactParticleFeatureWidth}, {"Varying", compactActionFeatureWidth}]|>;
    If[Information[net, "InputPorts"] =!= ports ||
        Information[net, "OutputPorts"] =!= <|"Output" -> {"Varying"}|>, Return[False]];
    arrays = If[encoding === "ParticleRoleTokens", compactParticlePolicyArrays[net], compactPolicyArrays[net]];
    If[arrays === $Failed, Return[False]]];
  $modelNet = net; $loadedModelDirectory = dir; $loadedPolicyMethod = $policyMethod;
  $compactWeights = arrays; $compactDescriptorEncoding = encoding;
  True
];


(* ::Subsection::Closed:: *)
(*Candidate Scoring*)

scoreCompactCandidateChunks[expr_, rules_, cands_List] := Module[
  {stateVector, chunks, featureFunction, scoreFunction, features, scores, batches},
  If[cands === {} || !IntegerQ[$candidateBatchSize] || $candidateBatchSize < 1, Return[$Failed]];
  stateVector = compactPolicyStateVector[$compactWeights, stateFeature[expr, rules, stateLegIDs[expr, rules]]];
  If[stateVector === $Failed, Return[$Failed]];
  featureFunction = If[$compactDescriptorEncoding === "ParticleRoleTokens", compactParticleFeatures, compactActionFeatures];
  scoreFunction = If[$compactDescriptorEncoding === "ParticleRoleTokens", compactParticleCandidateScores, compactPolicyCandidateScores];
  chunks = Partition[cands, UpTo[$candidateBatchSize]];
  batches = Map[Function[chunk,
    features = featureFunction[expr, rules, chunk];
    If[features === $Failed, Return[$Failed, Module]];
    scores = scoreFunction[$compactWeights, stateVector, features];
    If[scores === $Failed || Length[scores] =!= Length[chunk], Return[$Failed, Module]];
    scores
  ], chunks];
  Flatten[batches]
];

scoreCandidates[expr_, rules_, cands_] := Module[{scores},
  Which[
    $policyMethod === "CompactActions",
    scores = scoreCompactCandidateChunks[expr, rules, cands],
    $policyMethod === "ReverseMoves",
    If[Length[cands] > $moveSlots, Message[TrainUnscrambleNet::slots, Length[cands], $moveSlots]; Return[$Failed]];
    scores = Flatten[$modelNet[moveFeatures[expr, rules], TargetDevice -> "CPU"]];
    If[Length[scores] =!= $moveSlots, Return[$Failed]];
    scores = Take[scores, Length[cands]],
    True,
    scores = Flatten[$modelNet[groupFeatures[expr, rules, cands], TargetDevice -> "CPU"]]];
  If[Length[scores] =!= Length[cands] || !VectorQ[scores, NumericQ], $Failed, scores]
];

selectCandidate[expr_, rules_, sample_] := Module[{cands, scores, probs, idx},
  cands = legalMoves[expr, rules];
  scores = scoreCandidates[expr, rules, cands];
  If[scores === $Failed, Return[$Failed]];
  probs = Exp[(scores - Max[scores])/If[MemberQ[{"ReverseMoves", "CompactActions"}, $policyMethod], 1., 0.2]];
  idx = If[TrueQ[sample], RandomChoice[probs -> Range[Length[cands]]], First[Ordering[scores, -1]]];
  <|"Candidate" -> cands[[idx]], "Index" -> idx, "Count" -> Length[cands], "Score" -> scores[[idx]]|>
];


(* ::Subsection::Closed:: *)
(*Complexity and Scrambling*)

SpinorExpressionComplexity[expr_] := expressionComplexity[expr];
complexityMomentumMultiplicity[Mom[label_]] := Max[1, Length[momentumMembers[label]]];
complexityMomentumMultiplicity[_] := 1;
momentumBranchCount[slots_List] := Times @@ (complexityMomentumMultiplicity /@ slots);

(* Count the branches hidden in composite slots without constructing them.
   A product's slots multiply branch counts, including repeated chain factors. *)
monomialComplexity[mon_] := Module[{slots, branches},
  slots = Flatten[momentumSlots /@ Join[chainsIn[mon], tracesIn[mon]], 1];
  branches = momentumBranchCount[slots];
  branches*(1 + 4*Length[slots] + 4*Count[{mon}, _MomProd, Infinity] +
    2*Count[{mon}, Power[Mom[Multiparticle[__]], 2], Infinity])
];
expressionComplexity[expr_] := withProtectedPropagators[expr,
  Function[body, Total[monomialComplexity /@ monomialsOf[body]]]];
expressionTermCounts[expr_] := withProtectedPropagators[expr, Function[body,
  With[{mons = monomialsOf[body]}, <|"Terms" -> Length[mons],
    "ExpandedTerms" -> Total[momentumBranchCount[
      Flatten[momentumSlots /@ Join[chainsIn[#], tracesIn[#]], 1]] & /@ mons]|>]]];

(* Random scrambles use the reversible subset of the inference vocabulary. *)
scramblePair[{rules_List, amp_}, n_, seed_, record_] := Module[
  {original = canonicalAmplitude[amp, rules], expr, moves, pick, next, history = {}},
  expr = original;
  BlockRandom[
    If[IntegerQ[seed], SeedRandom[seed]];
    Do[
      moves = Select[legalMoves[expr, rules],
        #["Name"] =!= "Stop" && TrueQ[Lookup[#, "ScrambleEligible", True]] &];
      If[moves === {}, Break[]];
      pick = RandomChoice[moves]; next = applyCandidate[expr, pick];
      AppendTo[history, <|"Before" -> expr, "After" -> next, "Candidate" -> pick|>];
      expr = next, {n}
    ]
  ];
  Join[<|"MassRules" -> rules, "Original" -> original, "Expression" -> expr, "Steps" -> Length[history]|>,
    If[TrueQ[record], <|"Trajectory" -> history|>, <||>]]
];

ComplicateAmplitude[pair:{_List, _}, nSteps_Integer:5, OptionsPattern[]] :=
  Block[{$onShellChannels = normalizeOnShellChannels[OptionValue["OnShellChannels"]],
      $useMomentumConservation = OptionValue["MomentumConservation"]},
    scramblePair[pair, Max[0, nSteps], OptionValue[RandomSeed], OptionValue["RecordSteps"]]
  ];

(* A reverse scramble is a label only when the inference candidate set can
   actually perform it. Skip unreachable targets rather than invent actions. *)

(* ::Subsection::Closed:: *)
(*Training Examples*)

trainingRows[expr_, rules_, target_, candidates_:Automatic, verifiedLabels_:Automatic] := Module[
  {cands, labels, delta, canonicalTarget = canonicalAmplitude[target, rules]},
  cands = If[candidates === Automatic, legalMoves[expr, rules], candidates];
  If[$policyMethod === "ReverseMoves" && Length[cands] > $moveSlots,
    Message[TrainUnscrambleNet::slots, Length[cands], $moveSlots]; Return[$Failed]];
  labels = If[verifiedLabels === Automatic,
    delta = canonicalChains[canonicalTarget - expr];
    If[samePoly[expr, canonicalTarget], Boole[#["Name"] === "Stop"] & /@ cands,
      Boole[#["Name"] =!= "Stop" && samePoly[canonicalChains[#["Insert"] - #["Subtract"]], delta] &&
        samePoly[applyCandidate[expr, #], canonicalTarget]] & /@ cands], verifiedLabels];
  If[!MemberQ[labels, 1], Return[{}]];
  If[$policyMethod === "ReverseMoves", Return[{Join[moveFeatures[expr, rules],
    <|"Target" -> N[PadRight[labels, $moveSlots]/Total[labels]],
      "LegalMask" -> N[PadRight[ConstantArray[1, Length[cands]], $moveSlots]],
      "ActionCount" -> Length[cands]|>]}]];
  {Join[groupFeatures[expr, rules, cands], <|"Target" -> List /@ N[labels],
    "Weights" -> List /@ balanceWeights[labels]|>]}
];

balanceWeights[labels_] := Module[{positive = Count[labels, 1], negative = Count[labels, 0]},
  If[positive === 0 || negative === 0, ConstantArray[1., Length[labels]],
    N[Sqrt[Length[labels]/(2*If[# === 1, positive, negative])] & /@ labels]]
];


(* ::Subsection::Closed:: *)
(*Neural Training*)

Options[TrainUnscrambleNet] = Join[
  {Steps -> 8, Scrambles -> 20, HoldOut -> 2, MaxTrainingRounds -> 6,
    EpisodeLength -> 12, Kernels -> 1, TargetDevice -> "CPU", "ModelDirectory" -> Automatic,
    "DataCacheDirectory" -> None, "WorkerRoot" -> Automatic,
    "WorkerThreads" -> 1, "TrainingBatchSize" -> 1, "MaskPadding" -> False, "PolicyMethod" -> "CandidateScorer",
    "JobTimeLimit" -> 300, "ValidationTimeLimit" -> 60, "SimplificationRoutes" -> {}, "RoutePermutations" -> 4}, Options[UnscrambleCandidates]];
TrainUnscrambleNet::kernels = "Kernels must be a positive integer, Automatic, or a nonempty list of connected kernel objects. Worker startup or initialization failed.";
TrainUnscrambleNet::nodata = "No reversible training examples were generated. No model was saved.";
TrainUnscrambleNet::options = "Steps, HoldOut must be nonnegative integers; Scrambles, MaxTrainingRounds, EpisodeLength must be positive integers.";
TrainUnscrambleNet::save = "Could not save the candidate model in `1`.";
TrainUnscrambleNet::mask = "MaskPadding must be True or False.";
TrainUnscrambleNet::method = "PolicyMethod must be CandidateScorer or ReverseMoves. MaskPadding applies only to CandidateScorer.";
TrainUnscrambleNet::slots = "The legal move list has `1` entries, exceeding the ReverseMoves limit of `2`. No moves were truncated.";
TrainUnscrambleNet::routes = "SimplificationRoutes must be a list of associations with a nonempty Name, a list of Masses rules, and at least two States ordered from the input to the simplified endpoint. Optional OnShellChannels must be Automatic or lists of integer legs; MomentumConservation must be True or False.";
TrainUnscrambleNet::route = "Simplification route `1`, transition `2`, is not a changed result of one current legal edit. No partial route will be used.";
TrainUnscrambleNet::routepermutations = "RoutePermutations must be All or a nonnegative integer.";
TrainUnscrambleNet::routepermutationsall = "Route `1` has `2` external legs. Exhaustive RoutePermutations -> All is limited to five legs; use an integer sampling count for larger routes.";

(* Replace illegal logits before softmax so they receive no probability or
   gradient. Targets share probability among all equivalent reverse moves. *)
makeMoveTrainingPolicy[] := NetGraph[<|"Policy" -> makeMovePolicy[],
  "Mask" -> ThreadingLayer[If[#2 > 0., #1, -1.*^30] &],
  "Probabilities" -> SoftmaxLayer[], "Loss" -> CrossEntropyLossLayer["Probabilities"]|>,
  {NetPort["State"] -> NetPort["Policy", "State"],
    {"Policy", NetPort["LegalMask"]} -> "Mask" -> "Probabilities" -> NetPort["Loss", "Input"],
    NetPort["Target"] -> NetPort["Loss", "Target"]},
  "LegalMask" -> $moveSlots, "Target" -> $moveSlots];

makeTrainingPolicy[width_:"Varying"] := If[$policyMethod === "ReverseMoves",
  makeMoveTrainingPolicy[], makeScorerTrainingPolicy[width]];

makeScorerTrainingPolicy[width_:"Varying"] := NetGraph[<|"Policy" -> makePolicy[width],
  "WeightedPrediction" -> ThreadingLayer[Times], "WeightedTarget" -> ThreadingLayer[Times],
  "Loss" -> MeanSquaredLossLayer[]|>, Join[{
  NetPort["State"] -> NetPort["Policy", "State"],
  {"Policy", NetPort["Weights"]} -> "WeightedPrediction" -> NetPort["Loss", "Input"],
  {NetPort["Target"], NetPort["Weights"]} -> "WeightedTarget" -> NetPort["Loss", "Target"]},
  If[$policyMethod === "ReverseMoves", {}, {NetPort["Candidates"] -> NetPort["Policy", "Candidates"]}]]];

prepareTrainingBatch[rows_] := Module[{width, padded, net},
  If[$policyMethod === "ReverseMoves", Return[<|"Net" -> makeTrainingPolicy[],
    "Rows" -> (KeyDrop[#, "ActionCount"] & /@ rows), "Width" -> 0|>]];
  width = Max[Length /@ Flatten[Lookup[rows, "Candidates"], 1]];
  padded = (Join[#, <|"Candidates" -> (PadLeft[#, width, 257] & /@ #["Candidates"])|>] &) /@ rows;
  net = makeTrainingPolicy[width];
  <|"Net" -> net, "Rows" -> padded, "Width" -> width|>
];

variableWidthPolicy[net_] := Module[{paths = Information[net, "ArraysPositionList"]},
  If[$policyMethod === "ReverseMoves", Return[net]];
  Block[{$maskCandidatePadding = !FreeQ[paths, "Mask"]},
    NetReplacePart[makePolicy[], Map[# -> NetExtract[net, #] &, paths]]]
];

progressNumber[value_] := If[NumberQ[value], ToString[NumberForm[N[value], {10, 3}]], "pending"];

reportTrainingProgress[progress_Association, total_] := Module[{status, done, elapsed, remaining, keys},
  done = Lookup[progress, "AbsoluteBatch", 0];
  elapsed = Lookup[progress, "TimeElapsed", 0];
  remaining = Lookup[progress, "TimeRemaining", Missing["NotAvailable"]];
  keys = Intersection[Keys[progress], {"AbsoluteBatch", "Batch", "BatchesPerRound", "Round",
      "TotalRounds", "BatchLoss", "RoundLoss", "TimeElapsed", "TimeRemaining"}];
  (* Evaluate NetTrain's delayed properties while the callback is active. *)
  status = Join[AssociationThread[keys, Lookup[progress, #] & /@ keys],
    <|"TotalBatches" -> total, "Timestamp" -> DateString["ISODateTime"],
      "WolframMemoryBytes" -> MemoryInUse[]|>];
  Print[status["Timestamp"], " NN progress: ", done, "/", total, " batches (",
    progressNumber[100. done/Max[1, total]], "%); round ", progressNumber[Lookup[progress, "Round", 0]],
    "/", Lookup[progress, "TotalRounds", "?"], "; batch loss ",
    progressNumber[Lookup[progress, "BatchLoss", Missing[]]], "; elapsed ",
    progressNumber[elapsed/60.], " min; remaining estimate ",
    progressNumber[If[NumberQ[remaining], remaining/60., remaining]], " min; Wolfram memory ",
    progressNumber[status["WolframMemoryBytes"]/2.^30], " GiB"];
  If[StringQ[$trainingProgressFile], Put[status, $trainingProgressFile]];
];

$trainingProgressFile = None;

moveTrainingAccuracy[net_, rows_] := Module[{correct, scores},
  correct = Count[Map[Function[row,
    scores = Take[Flatten[net[KeyTake[row, {"State"}], TargetDevice -> $trainingTargetDevice]], row["ActionCount"]];
    TrueQ[row["Target"][[First[Ordering[scores, -1]]]] > 0]], rows], True];
  <|"Correct" -> correct, "States" -> Length[rows], "Accuracy" -> N[correct/Length[rows]]|>
];

trainCandidateNetwork[rows_, rounds_] := Module[{trained, batch = prepareTrainingBatch[rows], total, callback, policy, accuracy},
  total = rounds Ceiling[Length[rows]/$trainingBatchSize];
  callback = Function[progress, reportTrainingProgress[progress, total]];
  Print["NN training: ", Length[rows], " states; ", rounds, " rounds; batch size ",
    $trainingBatchSize, "; ", total, " planned batches; encoded dataset ",
    progressNumber[ByteCount[batch["Rows"]]/2.^30], " GiB"];
  (* NetTrain permits only the first input dimension to vary. Fix the byte
     width to this dataset's longest edit, padding only, never truncating. *)
  trained = NetTrain[NetInitialize[batch["Net"], RandomSeeding -> 1], batch["Rows"],
    MaxTrainingRounds -> rounds, BatchSize -> $trainingBatchSize, TargetDevice -> $trainingTargetDevice,
    TrainingProgressReporting -> None,
    TrainingProgressFunction -> {{callback, "Interval" -> Quantity[30, "Seconds"]},
      {callback, "Interval" -> Quantity[1, "Rounds"]}},
    Method -> {"ADAM", "LearningRate" -> 0.001}];
  Print[If[MatchQ[trained, _NetGraph], "NN training finished.", "NN training failed."]];
  If[!MatchQ[trained, _NetGraph], Return[$Failed]];
  policy = variableWidthPolicy[NetExtract[trained, "Policy"]];
  If[$policyMethod === "ReverseMoves",
    accuracy = moveTrainingAccuracy[policy, rows];
    Print["Training move accuracy: ", accuracy["Correct"], "/", accuracy["States"],
      " (", progressNumber[100. accuracy["Accuracy"]], "%). This is training data, not validation."]];
  policy
];

(* Independent deterministic jobs; only the coordinator writes model files. *)
TrainUnscrambleNet::job = "A `1` job failed or exceeded its time limit. No partial training dataset will be used.";
TrainUnscrambleNet::cache = "Could not read or write the training-data cache at `1`.";
TrainUnscrambleNet::parallelopts = "WorkerThreads and TrainingBatchSize must be positive integers; time limits must be positive numbers; cache and worker paths must be strings or their documented defaults.";
TrainUnscrambleNet::device = "TargetDevice must be CPU, GPU, CUDA, or {GPU or CUDA, a positive device index or All}. GPU training requires a supported CUDA installation.";


(* ::Subsection::Closed:: *)
(*Deterministic Training and Validation Jobs*)

pipelineSourceFiles[root_] := Join[Sort[FileNames["*.wl", FileNameJoin[{root, "source"}]]],
  FileNameJoin[{root, "tests", #}] & /@ {"unscrambling.wl"}];
pipelineFingerprint[root_] := Hash[FileHash[#, "SHA256"] & /@ pipelineSourceFiles[root], "SHA256"];

trainingLegPermutation[pair_, index_, seed_] := Module[{legs = Keys[stateLegIDs[pair[[2]], pair[[1]]]]},
  AssociationThread[legs, If[seed === 0, legs,
    BlockRandom[SeedRandom[Hash[{pair, index, seed, "TrainingLegPermutation"}, "SHA256"]]; RandomSample[legs]]]]
];

makeTrainingJobs[amplitudes_, steps_, count_] := MapIndexed[Join[#1, <|"Order" -> First[#2]|>] &,
  Flatten[MapIndexed[Function[{pair, index},
    Table[With[{ids = trainingLegPermutation[pair, First[index], seed]},
      <|"Mode" -> If[seed === 0, "Original", "Scramble"], "Pair" -> relabelLegs[pair, ids],
        "LegPermutation" -> ids, "AmplitudeIndex" -> First[index], "Seed" -> seed, "Steps" -> steps|>],
      {seed, 0, count}]], amplitudes], 1]];

validSimplificationRoutesQ[routes_] := ListQ[routes] && AllTrue[routes, Function[route,
  AssociationQ[route] && StringQ[Lookup[route, "Name", None]] && StringLength[route["Name"]] > 0 &&
    MatchQ[Lookup[route, "Masses", None], {(_Rule | _RuleDelayed) ...}] &&
    ListQ[Lookup[route, "States", None]] && Length[route["States"]] >= 2 &&
    FreeQ[route["States"], $Failed | _Missing] &&
    (Lookup[route, "OnShellChannels", Automatic] === Automatic ||
      MatchQ[Lookup[route, "OnShellChannels"], {{_Integer ..} ...}]) &&
    MemberQ[{True, False}, Lookup[route, "MomentumConservation", True]]]];

(* Curated paths are inputs, not new reference amplitudes: only their final
   endpoint receives Stop. Their whole path and conditions are cache inputs. *)
validRoutePermutationCountQ[count_] := count === All || (IntegerQ[count] && count >= 0);

makeSimplificationRouteJobs[routes_, count_] := Module[{jobs},
  If[!validSimplificationRoutesQ[routes], Message[TrainUnscrambleNet::routes]; Return[$Failed]];
  If[!validRoutePermutationCountQ[count], Message[TrainUnscrambleNet::routepermutations]; Return[$Failed]];
  jobs = Catch[Flatten[MapIndexed[Function[{route, index},
    Module[{pair = {route["Masses"], route["States"]}, legs, permutations},
      legs = Keys[stateLegIDs[pair[[2]], pair[[1]]]];
      If[count === All && Length[legs] > 5,
        Message[TrainUnscrambleNet::routepermutationsall, route["Name"], Length[legs]]; Throw[$Failed]];
      permutations = If[count === All,
        AssociationThread[legs, #] & /@ Permutations[legs],
        Table[trainingLegPermutation[pair, -First[index], seed], {seed, 0, count}]];
      MapIndexed[Function[{ids, seed},
        Join[<|"Mode" -> "SimplificationRoute", "Name" -> route["Name"],
          "Pair" -> relabelLegs[{route["Masses"], First[route["States"]]}, ids],
          "States" -> relabelLegs[route["States"], ids], "LegPermutation" -> ids,
          "AmplitudeIndex" -> -First[index], "Seed" -> First[seed] - 1,
          "Steps" -> Length[route["States"]] - 1|>,
          KeyTake[route, {"OnShellChannels", "MomentumConservation"}]]], permutations]
    ]], routes], 1]];
  If[jobs === $Failed, Return[$Failed]];
  MapIndexed[Join[#1, <|"Order" -> First[#2]|>] &,
    DeleteDuplicatesBy[jobs, {#["AmplitudeIndex"], #["LegPermutation"]} &]]
];

trainingJobChannels[job_] := Replace[normalizeOnShellChannels[Lookup[job, "OnShellChannels", $onShellChannels]],
  channels_List :> normalizeOnShellChannels[Map[Lookup[Lookup[job, "LegPermutation", <||>], #, #] &, channels, {2}]]];

(* ::Subsection::Closed:: *)
(*Model-Guided Training Preparation*)

trainingFiniteScoresQ[scores_, count_Integer] := ListQ[scores] && Length[scores] === count &&
  AllTrue[scores, NumberQ[#] && TrueQ[Im[#] == 0 && Abs[#] < Infinity] &];

(* Encode the state once; full-board audits never require whole-result encoding. *)
trainingCompactCandidateScores[weights_Association, expression_, rules_List, candidates_List,
    batch_Integer:128] := Module[{state, scores},
  If[batch < 1 || candidates === {}, Return[$Failed]];
  state = compactPolicyStateVector[weights, stateFeature[expression, rules, stateLegIDs[expression, rules]]];
  If[state === $Failed, Return[$Failed]];
  scores = Flatten[Map[Function[chunk, Module[{features},
    features = compactParticleFeatures[expression, rules, chunk];
    If[features === $Failed, $Failed, compactParticleCandidateScores[weights, state, features]]]],
    Partition[candidates, UpTo[batch]]]];
  If[trainingFiniteScoresQ[scores, Length[candidates]], scores, $Failed]
];
trainingCompactCandidateScores[___] := $Failed;

trainingFullBoardAudit[candidates_List, labels_List, scores_List] := Module[
  {positive, alternative, best, other, probabilities, top},
  If[candidates === {} || !trainingFiniteScoresQ[scores, Length[candidates]] ||
      Length[labels] =!= Length[candidates] || !AllTrue[labels, MemberQ[{0, 1}, #] &] ||
      Total[labels] == 0 || !AllTrue[candidates, AssociationQ],
    Return[<|"Passed" -> False, "Reason" -> "InvalidFullBoardAudit"|>]];
  positive = Flatten[Position[labels, 1]]; alternative = Complement[Range[Length[labels]], positive];
  best = Max[scores[[positive]]]; other = If[alternative === {}, Null, Max[scores[[alternative]]]];
  probabilities = Exp[scores - Max[scores]]; probabilities /= Total[probabilities];
  top = First[Ordering[scores, -1]];
  <|"Passed" -> True, "Scope" -> "FullLegalBoard", "FullActionCount" -> Length[candidates],
    "TeacherCount" -> Length[positive], "TeacherRank" -> 1 + Count[scores, value_ /; value > best],
    "TeacherProbability" -> Total[probabilities[[positive]]],
    "TeacherMargin" -> If[other === Null, Null, best - other],
    "TopChoiceTeacher" -> MemberQ[positive, top], "TopChoiceIdentity" -> candidates[[top]]["Name"]|>
];
trainingFullBoardAudit[___] := <|"Passed" -> False, "Reason" -> "InvalidFullBoardAudit"|>;

trainingContinuationDefaults = <|"MaxAttempts" -> 2, "RolloutSteps" -> 2, "MaxDepth" -> 2,
  "BeamWidth" -> 4, "MaxNodes" -> 32, "MaxCandidatesPerNode" -> 256,
  "Seconds" -> 120, "StepTimeLimit" -> 30, "MaxExpressionLeaves" -> 20000,
  "OnShellChannels" -> Automatic, "MomentumConservation" -> True, "AnchorSteps" -> Automatic,
  "ForbiddenStates" -> {}|>;
trainingContinuationOptionsQ[settings_] := AllTrue[Lookup[settings,
  {"MaxAttempts", "RolloutSteps", "MaxDepth", "BeamWidth", "MaxNodes", "MaxCandidatesPerNode", "MaxExpressionLeaves"}],
    IntegerQ[#] && # > 0 &] && AllTrue[Lookup[settings, {"Seconds", "StepTimeLimit"}],
    NumberQ[#] && TrueQ[Im[#] == 0 && 0 < # < Infinity] &] &&
  MemberQ[{True, False}, settings["MomentumConservation"]] &&
  ListQ[settings["ForbiddenStates"]] && FreeQ[settings["ForbiddenStates"], $Failed | _Missing] &&
  (settings["OnShellChannels"] === Automatic || MatchQ[settings["OnShellChannels"], {{_Integer ..} ...}]);
trainingContinuationKey[state_] := Hash[canonicalChains[state], "SHA256"];

trainingVerifyContinuation[states_List, rules_List, settings_Association] := Module[{job, verified, normalized},
  If[!trainingContinuationOptionsQ[settings] || Length[states] < 2 || !FreeQ[states, $Failed | _Missing] ||
      !AllTrue[states, LeafCount[#] <= settings["MaxExpressionLeaves"] &],
    Return[<|"Passed" -> False, "Reason" -> "InvalidOrCyclicContinuation"|>]];
  verified = Block[{$onShellChannels = settings["OnShellChannels"],
      $useMomentumConservation = settings["MomentumConservation"]},
    TimeConstrained[
      normalized = canonicalAmplitude[#, rules] & /@ states;
      If[!DuplicateFreeQ[trainingContinuationKey /@ normalized] ||
          !AllTrue[normalized, LeafCount[#] <= settings["MaxExpressionLeaves"] &],
        Return[<|"Passed" -> False, "Reason" -> "InvalidOrCyclicContinuation"|>, Module]];
      job = <|"Name" -> "Learner recovery", "Pair" -> {rules, First[normalized]}, "States" -> normalized|>;
      verifiedSimplificationRoute[job, <|"StepTimeLimit" -> settings["StepTimeLimit"],
      "RowFunction" -> Function[{state, target, candidates, labels, step},
        <|"Identity" -> candidates[[First[FirstPosition[labels, 1]]]]["Name"]|>]|>],
      settings["Seconds"], $Failed]];
  If[verified === $Failed, <|"Passed" -> False, "Reason" -> "UnverifiedContinuation"|>,
    <|"Passed" -> True, "States" -> verified["States"],
      "RouteIdentities" -> Most[Lookup[verified["Rows"], "Identity"]], "EndpointVerified" -> True|>]
];

(* Search only a bounded bridge. The known suffix still has to pass the same
   streaming legal-edge verifier as the original curated route. *)
trainingRecoverContinuation[pair:{rules_List, _}, anchors_List, scorer_, options_Association:<||>] := Module[
  {settings = Join[trainingContinuationDefaults, options], result},
  If[!trainingContinuationOptionsQ[settings] || anchors === {} ||
      !FreeQ[anchors, $Failed | _Missing], Return[<|"Passed" -> False, "Reason" -> "InvalidRecoveryOptions"|>]];
  result = TimeConstrained[Block[{$onShellChannels = settings["OnShellChannels"],
      $useMomentumConservation = settings["MomentumConservation"]}, Module[
    {known, frontier, visited = <||>, nodes = 0, expression, path, depth, candidates, scores,
      selected, next, key, matches, states, proof, children, deltas, candidateDelta, targetIndices,
      test, tag = Unique[], denominatorSet, forbidden},
    known = canonicalAmplitude[#, rules] & /@ anchors;
    expression = canonicalAmplitude[pair[[2]], rules];
    forbidden = trainingContinuationKey[canonicalAmplitude[#, rules]] & /@ settings["ForbiddenStates"];
    If[!AllTrue[Prepend[known, expression], LeafCount[#] <= settings["MaxExpressionLeaves"] &],
      Return[<|"Passed" -> False, "Reason" -> "RecoveryExpressionLimit"|>, Module]];
    denominatorSet = DeleteDuplicates[Cases[expression, _PropDen, Infinity]];
    frontier = {<|"Expression" -> expression, "States" -> {expression}|>};
    Catch[Do[
      children = {};
      Do[
        If[nodes >= settings["MaxNodes"], Throw[<|"Passed" -> False, "Reason" -> "RecoveryNodeLimit",
          "VisitedNodes" -> nodes|>, tag]];
        expression = node["Expression"]; path = node["States"]; key = trainingContinuationKey[expression];
        If[KeyExistsQ[visited, key], Continue[]]; visited[key] = True; nodes++;
        test[index_, bridge_] := Module[{candidateStates, verified},
          candidateStates = Join[bridge, Drop[known, index]];
          If[Length[candidateStates] < 2, Return[False]];
          If[Intersection[trainingContinuationKey /@ candidateStates, forbidden] =!= {}, Return[False]];
          If[!AllTrue[candidateStates, LeafCount[#] <= settings["MaxExpressionLeaves"] &], Return[False]];
          If[!AllTrue[candidateStates, Complement[DeleteDuplicates[Cases[#, _PropDen, Infinity]],
              denominatorSet] === {} &], Return[False]];
          verified = trainingVerifyContinuation[candidateStates, rules, settings];
          If[TrueQ[verified["Passed"]], Throw[Join[verified, <|"ReachedAnchorIndex" -> index,
            "BridgeSteps" -> Length[bridge] - 1, "VisitedNodes" -> nodes|>], tag]]; False
        ];
        matches = Reverse[Flatten[Position[trainingContinuationKey /@ known, key]]];
        Do[If[samePoly[expression, known[[index]]], test[index, path]], {index, matches}];
        If[depth >= settings["MaxDepth"], Continue[]];
        candidates = legalMoves[expression, rules];
        If[candidates === {}, Continue[]];
        (* Match a one-edit rejoin using deltas before constructing trial results. *)
        deltas = GroupBy[Range[Length[known]], trainingContinuationKey[known[[#]] - expression] &];
        Do[If[candidate["Name"] === "Stop", Continue[]];
          candidateDelta = canonicalChains[candidate["Insert"] - candidate["Subtract"]];
          targetIndices = Reverse[Lookup[deltas, trainingContinuationKey[candidateDelta], {}]];
          If[targetIndices === {}, Continue[]];
          next = applyCandidate[expression, candidate];
          If[MemberQ[forbidden, trainingContinuationKey[next]], Continue[]];
          Do[If[samePoly[next, known[[index]]], test[index, Append[path, next]]], {index, targetIndices}],
          {candidate, candidates}];
        scores = scorer[expression, rules, candidates];
        If[!trainingFiniteScoresQ[scores, Length[candidates]],
          Throw[<|"Passed" -> False, "Reason" -> "RecoveryScoringFailed", "VisitedNodes" -> nodes|>, tag]];
        selected = Take[SortBy[Range[Length[candidates]], {-scores[[#]],
            Hash[KeyTake[candidates[[#]], {"Name", "Subtract", "Insert"}], "SHA256"]} &],
          UpTo[settings["MaxCandidatesPerNode"]]];
        Do[If[candidates[[index]]["Name"] === "Stop", Continue[]];
          next = applyCandidate[expression, candidates[[index]]];
          key = trainingContinuationKey[next];
          If[LeafCount[next] > settings["MaxExpressionLeaves"] || KeyExistsQ[visited, key] ||
              MemberQ[forbidden, key] || MemberQ[trainingContinuationKey /@ path, key], Continue[]];
          AppendTo[children, <|"Expression" -> next, "States" -> Append[path, next], "Score" -> scores[[index]]|>],
          {index, selected}],
        {node, frontier}];
      frontier = Take[DeleteDuplicatesBy[SortBy[children,
          {expressionComplexity[#["Expression"]], -Lookup[#, "Score", 0.], trainingContinuationKey[#["Expression"]]} &],
        trainingContinuationKey[#["Expression"]] &], UpTo[settings["BeamWidth"]]];
      If[frontier === {}, Break[]], {depth, 0, settings["MaxDepth"]}];
      <|"Passed" -> False, "Reason" -> "NoVerifiedRejoin", "VisitedNodes" -> nodes|>, tag]
  ]], settings["Seconds"], <|"Passed" -> False, "Reason" -> "RecoveryTimeout"|>];
  result
];
trainingRecoverContinuation[___] := <|"Passed" -> False, "Reason" -> "InvalidRecoveryInput"|>;

trainingLearnerRecoveryExamples[job_Association, scorer_, options_Association:<||>] := Module[
  {settings, original, steps, routes = {}, reports = {}, attempt, result, started},
  settings = Join[trainingContinuationDefaults, <|"OnShellChannels" -> trainingJobChannels[job],
    "MomentumConservation" -> Lookup[job, "MomentumConservation", True]|>, options];
  If[Lookup[job, "Mode", None] =!= "SimplificationRoute" ||
      !MemberQ[{"Training", "Train"}, Lookup[job, "Kind", None]] ||
      !MatchQ[Lookup[job, "Pair", None], {_List, _}] || !ListQ[Lookup[job, "States", None]] ||
      Length[job["States"]] < 2 || !trainingContinuationOptionsQ[settings],
    Return[<|"Routes" -> {}, "Reports" -> {<|"Passed" -> False, "Reason" -> "InvalidRecoverySource"|>}|>]];
  original = job["States"];
  steps = settings["AnchorSteps"];
  If[steps === Automatic, steps = DeleteDuplicates[Round[Subdivide[1, Length[original] - 1,
    Max[1, settings["MaxAttempts"] - 1]]]]];
  If[!ListQ[steps] || !AllTrue[steps, IntegerQ[#] && 1 <= # < Length[original] &],
    Return[<|"Routes" -> {}, "Reports" -> {<|"Passed" -> False, "Reason" -> "InvalidRecoveryAnchors"|>}|>]];
  Do[started = AbsoluteTime[];
    result = TimeConstrained[Block[{$onShellChannels = settings["OnShellChannels"],
        $useMomentumConservation = settings["MomentumConservation"]}, Module[
      {state, next, candidates, scores, index, expected, continuation, witness, sourceStep, chosen, learnerVisited},
      state = canonicalAmplitude[original[[anchor]], job["Pair"][[1]]];
      learnerVisited = {state};
      witness = <|"Passed" -> False, "Reason" -> "StayedOnTeacherPath"|>;
      Do[
        sourceStep = anchor + step - 1;
        If[sourceStep >= Length[original], Break[]];
        candidates = legalMoves[state, job["Pair"][[1]]]; scores = scorer[state, job["Pair"][[1]], candidates];
        If[!trainingFiniteScoresQ[scores, Length[candidates]],
          witness = <|"Passed" -> False, "Reason" -> "LearnerScoringFailed"|>; Break[]];
        index = First[Ordering[scores, -1]]; chosen = candidates[[index]];
        If[chosen["Name"] === "Stop", witness = <|"Passed" -> False,
          "Reason" -> "PrematureLearnerStop", "SourceStep" -> sourceStep|>; Break[]];
        next = applyCandidate[state, chosen]; expected = canonicalAmplitude[original[[sourceStep + 1]], job["Pair"][[1]]];
        If[LeafCount[next] > settings["MaxExpressionLeaves"],
          witness = <|"Passed" -> False, "Reason" -> "LearnerExpressionLimit"|>; Break[]];
        If[samePoly[next, expected], state = next; AppendTo[learnerVisited, next]; Continue[]];
        continuation = trainingRecoverContinuation[{job["Pair"][[1]], next}, Drop[original, sourceStep - 1],
          scorer, Join[settings, <|"ForbiddenStates" -> Join[settings["ForbiddenStates"], learnerVisited],
            "Seconds" -> Max[0.001, settings["Seconds"] - (AbsoluteTime[] - started)]|>]];
        witness = Join[KeyDrop[continuation, {"States", "RouteIdentities"}],
          <|"SourceStep" -> sourceStep, "LearnerIdentity" -> chosen["Name"]|>];
        If[TrueQ[continuation["Passed"]],
          witness = Join[witness, <|"Route" -> Join[job,
            <|"Name" -> job["Name"] <> " / learner recovery " <> ToString[anchor],
              "ReferenceName" -> Lookup[job, "ReferenceName", job["Name"]],
              "Pair" -> {job["Pair"][[1]], First[continuation["States"]]},
              "States" -> continuation["States"], "RouteIdentities" -> continuation["RouteIdentities"],
              "Steps" -> Length[continuation["States"]] - 1, "ModelRecovery" -> True,
              "Curated" -> True, "RecoverySourceStep" -> sourceStep,
              "EndpointVerified" -> True|>]|>]]; Break[], {step, settings["RolloutSteps"]}]; witness
    ]], settings["Seconds"], <|"Passed" -> False, "Reason" -> "LearnerRecoveryTimeout"|>];
    If[TrueQ[Lookup[result, "Passed", False]] && AssociationQ[Lookup[result, "Route", None]],
      AppendTo[routes, result["Route"]]];
    AppendTo[reports, Join[KeyDrop[result, "Route"], <|"AnchorStep" -> anchor,
      "Seconds" -> N[AbsoluteTime[] - started]|>]],
    {anchor, Take[steps, UpTo[settings["MaxAttempts"]]]}];
  <|"Routes" -> routes, "Reports" -> reports|>
];
trainingLearnerRecoveryExamples[___] := <|"Routes" -> {},
  "Reports" -> {<|"Passed" -> False, "Reason" -> "InvalidRecoverySource"|>}|>;

(* Streaming consumers encode a verified edge immediately, rather than keeping
   hundreds of full candidate boards alive until the entire path is checked. *)
verifiedSimplificationRoute[job_, options_Association:<||>] := Module[
  {states, results, state, target, candidates, choices, delta, labels, row,
    limit = Lookup[options, "StepTimeLimit", Infinity],
    rowFunction = Lookup[options, "RowFunction", Automatic],
    progress = Lookup[options, "ProgressFunction", None],
    failed = Lookup[options, "FailureFunction", None],
    step = 0, phase = "Canonicalization", started, report, fail, tag = Unique[], count},
  count = Length[job["States"]];
  report[] := If[progress =!= None, progress[<|"RouteName" -> job["Name"],
    "Sequence" -> Lookup[job, "Sequence", Lookup[job, "Seed", 0]],
    "LegPermutation" -> Lookup[job, "LegPermutation", <||>],
    "Step" -> step, "Steps" -> count, "Phase" -> phase,
    "Seconds" -> N[AbsoluteTime[] - started]|>]];
  fail[reason_] := Module[{details = <|"Reason" -> reason, "RouteName" -> job["Name"],
      "Sequence" -> Lookup[job, "Sequence", Lookup[job, "Seed", 0]],
      "LegPermutation" -> Lookup[job, "LegPermutation", <||>],
      "Step" -> step, "Steps" -> count, "Phase" -> phase,
      "Seconds" -> N[AbsoluteTime[] - started]|>},
    If[failed =!= None, failed[details]];
    If[reason === "IllegalSimplificationRoute", Message[TrainUnscrambleNet::route, job["Name"], step]];
    Throw[$Failed, tag]
  ];
  started = AbsoluteTime[];
  Catch[
    If[!(limit === Infinity || (NumericQ[limit] && TrueQ[limit > 0])),
      fail["InvalidRouteStepTimeLimit"]];
    states = Table[
      step = index; phase = "Canonicalization"; started = AbsoluteTime[]; report[];
      TimeConstrained[canonicalAmplitude[job["States"][[index]], job["Pair"][[1]]],
        limit, fail["CuratedStepTimeout"]], {index, count}];
    results = Table[
      step = index; started = AbsoluteTime[];
      state = states[[index]]; target = states[[Min[index + 1, count]]];
      row = TimeConstrained[
        phase = "LegalEnumeration"; report[];
        candidates = legalMoves[state, job["Pair"][[1]]];
        phase = "TeacherMatch"; report[];
        delta = canonicalChains[target - state];
        choices = If[index === count, Select[candidates, #["Name"] === "Stop" &],
          If[samePoly[state, target], {}, Select[candidates,
            #["Name"] =!= "Stop" && samePoly[canonicalChains[#["Insert"] - #["Subtract"]], delta] &&
              samePoly[applyCandidate[state, #], target] &]]];
        If[choices === {}, fail["IllegalSimplificationRoute"]];
        labels = Boole[MemberQ[choices, #]] & /@ candidates;
        If[rowFunction === Automatic, {choices, candidates, labels},
          phase = "Encoding"; report[];
          row = rowFunction[state, target, candidates, labels, index];
          If[!AssociationQ[row], fail["InvalidCuratedRow"]];
          If[KeyExistsQ[row, "Reason"], fail[row["Reason"]]];
          row], limit, fail["CuratedStepTimeout"]];
      phase = "RowComplete"; report[];
      row, {index, If[rowFunction === Automatic, count - 1, count]}];
    If[rowFunction === Automatic,
      <|"States" -> states, "Moves" -> results[[All, 1]],
        "CandidateLists" -> results[[All, 2]], "LabelLists" -> results[[All, 3]]|>,
      <|"States" -> states, "Rows" -> results|>], tag]
];

simplificationRouteRows[job_] := Module[{verified},
  verified = verifiedSimplificationRoute[job, <|"RowFunction" ->
    Function[{state, target, candidates, labels, step}, Module[{batch},
      batch = trainingRows[state, job["Pair"][[1]], target, candidates, labels];
      Which[
        MatchQ[batch, {} | $Failed], <|"Reason" -> "IllegalSimplificationRoute"|>,
        !ListQ[batch], <|"Reason" -> "InvalidCuratedRow"|>,
        True, <|"Batch" -> batch|>]
    ]]|>];
  If[verified === $Failed, Return[$Failed]];
  <|"Rows" -> Flatten[Lookup[verified["Rows"], "Batch"], 1],
    "Targets" -> Length[verified["States"]] - 1, "Skipped" -> 0, "RouteName" -> job["Name"]|>
];

trainingRowStateKey[row_] := Hash[KeyTake[row, {"State", "Candidates", "LegalMask"}], "SHA256"];
trainingStopRowQ[row_] := If[KeyExistsQ[row, "ActionCount"],
  TrueQ[row["Target"][[row["ActionCount"]]] > 0], TrueQ[First[Last[row["Target"]]] > 0]];

(* An earlier reference may be a curated path's intermediate state. Do not
   counter-teach Stop there merely because random scrambling started from it. *)
collectTrainingRows[generated_, jobs_] := Module[{modes, routeRows, moveKeys, kept, discarded = 0},
  modes = AssociationThread[Lookup[jobs, "Order"], Lookup[jobs, "Mode"]];
  routeRows = Flatten[Lookup[Lookup[Select[generated,
    modes[#["Order"]] === "SimplificationRoute" &], "Result", {}], "Rows", {}], 1];
  moveKeys = trainingRowStateKey /@ Select[routeRows, !trainingStopRowQ[#] &];
  kept = Map[Function[result,
    Select[result["Result"]["Rows"], Function[row,
      If[trainingStopRowQ[row] && MemberQ[moveKeys, trainingRowStateKey[row]], discarded++; False, True]]]], generated];
  <|"Rows" -> Flatten[kept, 1], "DiscardedStopExamples" -> discarded|>
];

makeValidationJobs[amplitudes_, steps_, count_, hold_, episode_] := MapIndexed[Join[#1, <|"Order" -> First[#2]|>] &,
  Flatten[MapIndexed[Function[{pair, index},
    Table[<|"Mode" -> "Validate", "Pair" -> pair, "AmplitudeIndex" -> First[index],
      "Seed" -> count + seed, "Steps" -> steps, "Episode" -> episode|>, {seed, hold}]], amplitudes], 1]];

generationJob[job_] := Block[{
  $onShellChannels = trainingJobChannels[job],
  $useMomentumConservation = Lookup[job, "MomentumConservation", $useMomentumConservation]},
 Module[{pair = job["Pair"], scr, batches, targets, skipped},
  If[job["Mode"] === "SimplificationRoute", Return[simplificationRouteRows[job]]];
  If[job["Mode"] === "Original",
    Return[<|"Rows" -> trainingRows[canonicalAmplitude[pair[[2]], pair[[1]]], pair[[1]], canonicalAmplitude[pair[[2]], pair[[1]]]],
      "Targets" -> 0, "Skipped" -> 0|>]];
  scr = scramblePair[pair, job["Steps"], job["Seed"], True];
  (* Collect once instead of repeatedly copying the growing dataset with Join. *)
  batches = trainingRows[#["After"], pair[[1]], #["Before"]] & /@ Reverse[scr["Trajectory"]];
  targets = Length[batches]; skipped = Count[batches, {}];
  (* Teach Stop in the same numbering as the reverse trajectory. *)
  <|"Rows" -> Join[Flatten[batches, 1],
      trainingRows[canonicalAmplitude[pair[[2]], pair[[1]]], pair[[1]], canonicalAmplitude[pair[[2]], pair[[1]]]]],
    "Targets" -> targets, "Skipped" -> skipped|>
 ]
];

validationJob[job_] := Module[{pair = job["Pair"], scr, trace, got},
  scr = scramblePair[pair, job["Steps"], job["Seed"], False];
  (* Keep stochastic attempts reproducible regardless of worker assignment. *)
  trace = BlockRandom[SeedRandom[Hash[{job["AmplitudeIndex"], job["Seed"], "Validation"}]];
    runUnscramble[{pair[[1]], scr["Expression"]}, False, job["Episode"], 5, $loadedModelDirectory]];
  If[!TrueQ[trace["ModelLoaded"]] || TrueQ[Lookup[trace, "ScoringFailed", False]], Return[$Failed]];
  got = trace["Result"][[2]];
  <|"Simpler" -> Boole[expressionComplexity[got] < expressionComplexity[scr["Expression"]]],
    "Recovered" -> Boole[samePoly[got, scr["Original"]]],
    "TerminationReason" -> Lookup[trace, "TerminationReason", "Unknown"]|>
];

evaluateTrainingJob[job_, settings_] := Block[
  {$onShellChannels = settings["OnShellChannels"], $useMomentumConservation = settings["MomentumConservation"],
    $timeLimit = settings["ValidationTimeLimit"], $policyMethod = Lookup[settings, "PolicyMethod", "CandidateScorer"]},
  Module[{start = AbsoluteTime[], cpu = TimeUsed[], result},
    result = TimeConstrained[If[job["Mode"] === "Validate", validationJob[job], generationJob[job]],
      settings["JobTimeLimit"], $Failed];
    <|"Order" -> job["Order"], "Success" -> (AssociationQ[result] && FreeQ[result, $Failed]), "Result" -> result,
      "WallSeconds" -> (AbsoluteTime[] - start), "CPUSeconds" -> (TimeUsed[] - cpu), "Worker" -> $KernelID|>
  ]
];

(* Greedy cost assignment avoids concentrating large amplitudes on one worker.
   Each worker receives only its own jobs, not the entire training dataset. *)

(* ::Subsection::Closed:: *)
(*Parallel Worker Scheduling*)

partitionTrainingJobs[jobs_, n_] := Module[{chunks = ConstantArray[{}, n], loads = ConstantArray[0, n], slot, job},
  Do[
    slot = First[Ordering[loads, 1]];
    AppendTo[chunks[[slot]], job];
    loads[[slot]] += LeafCount[job["Pair"]]*Max[1, job["Steps"]],
    {job, Reverse[SortBy[jobs, LeafCount[#["Pair"]]*Max[1, #["Steps"]] &]]}];
  chunks
];

mapTrainingJobs[{}, _, _] := {};
mapTrainingJobs[jobs_, {}, settings_] := evaluateTrainingJob[#, settings] & /@ jobs;
mapTrainingJobs[jobs_, workers_List, settings_] := Module[{chunks, raw, i},
  chunks = partitionTrainingJobs[jobs, Length[workers]];
  Do[With[{chunk = chunks[[i]]}, ParallelEvaluate[$workerJobs = chunk; Null, {workers[[i]]}, DistributedContexts -> None]],
    {i, Length[workers]}];
  raw = With[{config = settings}, ParallelEvaluate[
    Module[{results = evaluateTrainingJob[#, config] & /@ $workerJobs}, $workerJobs = {}; results],
    workers, DistributedContexts -> None]];
  If[!ListQ[raw] || !AllTrue[raw, ListQ], Return[$Failed]];
  raw = Flatten[raw, 1];
  If[Length[raw] =!= Length[jobs] || !AllTrue[raw, AssociationQ], Return[$Failed]];
  SortBy[raw, #["Order"] &]
];

validJobResultsQ[results_] := ListQ[results] && AllTrue[results, AssociationQ[#] && TrueQ[Lookup[#, "Success", False]] &];


(* ::Subsection::Closed:: *)
(*Concurrent Inference Workers*)

inferenceKernelSnapshot[] := Kernels[];
launchInferenceKernels[n_Integer] := LaunchKernels[
  System`KernelConfiguration[n, "UseKernelForking" -> False], ProgressReporting -> False];
closeInferenceKernels[workers_List] := CloseKernels[workers];
inferenceMoveKey[move_Association] := Hash[move, "SHA256"];
inferenceStateKey[expression_] := Hash[expression, "SHA256"];
inferenceProcessorCapacity[] := Module[{command, information, output},
  command = Switch[$OperatingSystem,
    "MacOSX", {"/usr/sbin/sysctl", "-n", "hw.logicalcpu"},
    "Unix", {"/usr/bin/env", "-u", "OMP_NUM_THREADS", "-u", "OMP_THREAD_LIMIT", "/usr/bin/nproc"},
    _, None];
  If[command =!= None,
    information = TimeConstrained[Quiet[Check[RunProcess[command], $Failed]], 2, $Failed];
    If[AssociationQ[information] && Lookup[information, "ExitCode", 1] === 0,
      output = StringTrim[Lookup[information, "StandardOutput", ""]];
      If[StringMatchQ[output, DigitCharacter ..] && FromDigits[output] > 0, Return[FromDigits[output]]]
    ]
  ];
  Max[1, $ProcessorCount]
];

(* Pinned channels keep inference outside the shared ParallelSubmit queue.
   Existing training or user kernels never receive or lose these jobs. *)
inferenceSend[worker_, expression_] := Catch[
  Parallel`Kernels`send[worker, expression], _, Function[{value, tag}, $Failed]];
SetAttributes[inferenceSend, HoldRest];
inferenceRead[worker_] := Catch[Parallel`Kernels`receive[worker, False], _,
  Function[{value, tag}, $Failed]];

inferenceSubmit[worker_, request_Association] := Module[{packet, sent},
  packet = Join[request, <|"RequestID" -> CreateUUID[]|>];
  sent = With[{job = packet}, inferenceSend[worker, evaluateInferenceEdit[job]]];
  If[sent === $Failed, Return[$Failed]];
  <|"Worker" -> worker, "RouteID" -> packet["RouteID"], "Generation" -> packet["Generation"],
    "RequestID" -> packet["RequestID"]|>
];

inferenceReceive[tickets_List, seconds_:0.1] := Module[{started = AbsoluteTime[], held, value, ticket, ready},
  Catch[While[True,
    Do[
      held = inferenceRead[ticket["Worker"]];
      If[held === $Failed, Throw[<|"Ready" -> True, "Handle" -> ticket,
        "Result" -> <|"Status" -> "WorkerFailed", "RouteID" -> ticket["RouteID"],
          "Generation" -> ticket["Generation"], "RequestID" -> ticket["RequestID"]|>|>, ready]];
      If[MatchQ[held, _HoldComplete],
        value = ReleaseHold[held];
        If[AssociationQ[value] && KeyExistsQ[value, "RequestID"] &&
            value["RequestID"] =!= ticket["RequestID"], Continue[]];
        If[!AssociationQ[value] || !KeyExistsQ[value, "RequestID"],
          value = <|"Status" -> "WorkerFailed", "RouteID" -> ticket["RouteID"],
            "Generation" -> ticket["Generation"], "RequestID" -> ticket["RequestID"]|>];
        Throw[<|"Ready" -> True, "Handle" -> ticket, "Result" -> value|>, ready]
      ], {ticket, tickets}];
    If[AbsoluteTime[] - started >= Max[0, seconds], Throw[<|"Ready" -> False|>, ready]];
    Pause[Min[0.01, Max[0, seconds - (AbsoluteTime[] - started)]]]
  ], ready]
];

cancelInference[tickets_List, workers_List] := Module[{busy, result, ok = True},
  busy = Select[DeleteDuplicates[Lookup[tickets, "Worker", {}]], MemberQ[workers, #] &];
  Do[
    (* Abort[kernel] flushes that kernel's pending result before returning. *)
    result = TimeConstrained[Quiet[Check[Abort[worker], $Failed]], 5, $Failed];
    If[result === $Failed || !MemberQ[inferenceKernelSnapshot[], worker] ||
        !TrueQ[Parallel`Developer`EvaluationCount[worker] === 0],
      ok = False; If[MemberQ[inferenceKernelSnapshot[], worker], closeInferenceKernels[{worker}]]
    ], {worker, busy}];
  ok
];

selectInferenceCandidate[expression_, rules_, request_Association] := Module[
  {candidates = legalMoves[expression, rules], scores, remaining, eligible, ranked, probabilities, selected,
    firstChoice = Lookup[request, "FirstChoice", None], temperature = Lookup[request, "Temperature", 1.],
    excluded, visited, visits, moveKeys, next, loopAvoided = 0, sharedFallback = False, ready},
  If[candidates === {}, Return[Missing["NoMoves"]]];
  scores = scoreCandidates[expression, rules, candidates];
  If[!ListQ[scores] || Length[scores] =!= Length[candidates] || !AllTrue[scores,
      NumberQ[#] && TrueQ[Im[#] == 0] && TrueQ[Abs[#] < Infinity] &], Return[$Failed]];
  excluded = Lookup[request, "ExcludedMoveKeys", {}];
  visited = Lookup[request, "VisitedStateKeys", {}];
  visits = Lookup[request, "MoveVisits", <||>];
  moveKeys = inferenceMoveKey /@ candidates;
  remaining = Range[Length[candidates]];
  eligible = Select[remaining, !MemberQ[excluded, moveKeys[[#]]] &];
  If[!NumberQ[temperature] || !TrueQ[temperature > 0], temperature = 1.];
  Catch[While[remaining =!= {},
    If[!AnyTrue[eligible, candidates[[#]]["Name"] =!= "Stop" &] &&
        AnyTrue[remaining, candidates[[#]]["Name"] =!= "Stop" &],
      sharedFallback = True; eligible = remaining];
    If[eligible === {}, Throw[Missing["NoMoves"], ready]];
    If[IntegerQ[firstChoice] && firstChoice > 0,
      ranked = Select[eligible, candidates[[#]]["Name"] =!= "Stop" &];
      If[ranked === {}, ranked = eligible];
      ranked = SortBy[ranked, {-scores[[#]], #} &];
      selected = ranked[[1 + Mod[firstChoice - 1, Length[ranked]]]],
      If[TrueQ[Lookup[request, "Greedy", False]],
        selected = First[SortBy[eligible, {-scores[[#]], #} &]],
        probabilities = Exp[(scores[[eligible]] - Max[scores[[eligible]]])/temperature]/
          (1 + Lookup[visits, moveKeys[[eligible]], 0]);
        If[sharedFallback, probabilities *= Map[If[MemberQ[excluded, moveKeys[[#]]], 0.25, 1.] &, eligible]];
        selected = RandomChoice[probabilities -> eligible]
      ]
    ];
    next = applyCandidate[expression, candidates[[selected]]];
    If[candidates[[selected]]["Name"] =!= "Stop" && MemberQ[visited, inferenceStateKey[next]],
      loopAvoided++; remaining = DeleteCases[remaining, selected]; eligible = DeleteCases[eligible, selected];
      Continue[]];
    Throw[<|"Candidate" -> candidates[[selected]], "Index" -> selected, "Count" -> Length[candidates],
      "Score" -> scores[[selected]], "MoveKey" -> moveKeys[[selected]], "After" -> next,
      "LoopAvoided" -> loopAvoided, "SharedMoveFallback" -> sharedFallback|>, ready]
  ]; Missing["NoMoves"], ready]
];

evaluateInferenceEdit[request_Association] := Module[
  {expression = request["Expression"], rules = request["Rules"], started = AbsoluteTime[], result,
    selection, candidate, next, beforeScore, afterScore, changed, neutral, base, seconds},
  base = KeyTake[request, {"RouteID", "Generation", "RequestID"}];
  seconds = Lookup[request, "Seconds", 60.];
  result = TimeConstrained[BlockRandom[
    SeedRandom[Hash[{Lookup[request, "Seed", 0], request["RouteID"], request["Generation"],
      Lookup[request, "DrawIndex", 0]}, "SHA256"]];
    selection = selectInferenceCandidate[expression, rules, request];
    Which[
      selection === $Failed, Join[base, <|"Status" -> "ScoringFailed"|>],
      MissingQ[selection], Join[base, <|"Status" -> "NoMoves"|>],
      True,
      candidate = selection["Candidate"];
      next = If[KeyExistsQ[selection, "After"], selection["After"], applyCandidate[expression, candidate]];
      beforeScore = expressionComplexity[expression]; afterScore = expressionComplexity[next];
      changed = !samePoly[expression, next];
      neutral = candidate["Name"] === "MomentumSum" && Lookup[candidate, "Identity", None] === "Split" &&
        beforeScore === afterScore && changed;
      Join[base, <|"Status" -> If[candidate["Name"] === "Stop", "Stop", "Edited"],
        "Selection" -> KeyDrop[selection, "After"], "Before" -> expression, "After" -> next,
        "ComplexityBefore" -> beforeScore, "ComplexityAfter" -> afterScore,
        "Counts" -> expressionTermCounts[next], "Changed" -> changed, "NeutralSplit" -> neutral,
        "LoopAvoided" -> Lookup[selection, "LoopAvoided", 0],
        "SharedMoveFallback" -> TrueQ[Lookup[selection, "SharedMoveFallback", False]]|>]
    ]
  ], Max[0.001, seconds], Join[base, <|"Status" -> "TimeLimit"|>]];
  Join[result, <|"WorkerID" -> $KernelID, "StartedAt" -> started, "FinishedAt" -> AbsoluteTime[],
    "ElapsedSeconds" -> (AbsoluteTime[] - started)|>]
];

initializeInferenceWorkers[workers_List, dir_String, settings_Association, seconds_] := Module[
  {source, tests = $modelDirectory, tickets, remaining, received, initialized = True, started = AbsoluteTime[], i},
  source = If[StringQ[Global`$HEPCATpath] && FileExistsQ[FileNameJoin[{Global`$HEPCATpath, "HEPCAT.wl"}]],
    Global`$HEPCATpath, FileNameJoin[{DirectoryName[tests], "source"}]];
  tickets = Table[<|"Worker" -> workers[[i]], "RouteID" -> i, "Generation" -> -1,
    "RequestID" -> CreateUUID[]|>, {i, Length[workers]}];
  Do[With[{worker = tickets[[i]]["Worker"], requestID = tickets[[i]]["RequestID"], sourcePath = source,
      testPath = tests, modelDirectory = dir, config = settings},
    If[inferenceSend[worker, Module[{loaded},
      SetEnvironment[{"OMP_NUM_THREADS" -> If[$SystemID === "MacOSX-ARM64", "4", "1"],
        "OMP_DYNAMIC" -> "FALSE", "OPENBLAS_NUM_THREADS" -> "1", "MKL_NUM_THREADS" -> "1",
        "MXNET_CPU_WORKER_NTHREADS" -> "1"}];
      Global`$HEPCATpath = sourcePath;
      Block[{$Output = {}}, Get[FileNameJoin[{sourcePath, "HEPCAT.wl"}]];
        Get[FileNameJoin[{testPath, "unscrambling.wl"}]]];
      Unscrambling`Private`$policyMethod = Lookup[config, "PolicyMethod", "CandidateScorer"];
      Unscrambling`Private`$onShellChannels = Lookup[config, "OnShellChannels", Automatic];
      Unscrambling`Private`$useMomentumConservation = Lookup[config, "MomentumConservation", True];
      Unscrambling`Private`$candidateBatchSize = Lookup[config, "CandidateBatchSize", 128];
      loaded = Unscrambling`Private`loadUnscrambleModel[modelDirectory];
      <|"RequestID" -> requestID, "Initialized" -> TrueQ[loaded], "WorkerID" -> $KernelID|>
    ]] === $Failed, initialized = False]
  ], {i, Length[tickets]}];
  If[!initialized, cancelInference[tickets, workers]; Return[False]];
  remaining = tickets;
  While[remaining =!= {} && AbsoluteTime[] - started < seconds,
    received = inferenceReceive[remaining, Min[0.05, Max[0, seconds - (AbsoluteTime[] - started)]]];
    If[TrueQ[received["Ready"]],
      remaining = DeleteCases[remaining, received["Handle"]];
      If[!TrueQ[Lookup[received["Result"], "Initialized", False]], initialized = False]
    ]
  ];
  If[remaining =!= {}, cancelInference[remaining, workers]; initialized = False];
  initialized
];

withInferenceWorkers[spec_, dir_String, settings_Association, remainingSeconds_, body_] := Module[
  {before = inferenceKernelSnapshot[], owned = {}, requested, capacity, licenseLimit, initialized,
    launching = False, launchLinks = {}, launchResult, started = AbsoluteTime[], budget, cleanup},
  cleanup[] := (If[launching, owned = Complement[inferenceKernelSnapshot[], before]];
    If[owned =!= {}, closeInferenceKernels[owned]];
    (* A timed-out bootstrap can own links before it registers KernelObjects. *)
    If[launching, Quiet[Scan[Function[link, LinkInterrupt[link, 3]; LinkClose[link]],
      Complement[Links[], launchLinks]]]]);
  Internal`WithLocalSettings[Null,
    Catch[
      capacity = inferenceProcessorCapacity[];
      requested = If[spec === Automatic, capacity, spec];
      licenseLimit = If[IntegerQ[$MaxLicenseSubprocesses], Max[0, $MaxLicenseSubprocesses - Length[before]], Infinity];
      requested = Min[requested, capacity, licenseLimit];
      If[!IntegerQ[requested] || requested < 1 || !TrueQ[remainingSeconds > 0],
        Message[UnscrambleSpinorAmplitudes::kernels]; Throw[$Failed]];
      budget = Min[60., remainingSeconds]; launchLinks = Links[]; launching = True;
      (* Bootstrap's global abort hook must not reset a caller's worker pool. *)
      launchResult = Block[{System`AbortKernels = Function[Null]},
        TimeConstrained[launchInferenceKernels[requested], budget, $Failed]];
      owned = Complement[inferenceKernelSnapshot[], before];
      If[launchResult === $Failed, Message[UnscrambleSpinorAmplitudes::kernels]; Throw[$Failed]];
      If[owned === {}, Message[UnscrambleSpinorAmplitudes::kernels]; Throw[$Failed]];
      launching = False;
      If[Length[owned] =!= If[spec === Automatic, capacity, spec],
        Message[UnscrambleSpinorAmplitudes::kernellimit, If[spec === Automatic, capacity, spec], Length[owned]]];
      budget = Min[60., Max[0., remainingSeconds - (AbsoluteTime[] - started)]];
      initialized = initializeInferenceWorkers[owned, dir, settings, budget];
      If[!TrueQ[initialized], Message[UnscrambleSpinorAmplitudes::workerinit]; Throw[$Failed]];
      body[owned]
    ],
    AbortProtect[cleanup[]]
  ]
];


(* ::Subsection::Closed:: *)
(*Worker Lifecycle*)

trainingKernelSnapshot[] := Kernels[];
launchTrainingKernels[n_] := LaunchKernels[n];
closeTrainingKernels[workers_] := CloseKernels[workers];
initializeTrainingWorkers[workers_, workerRoot_, workerThreads_, fingerprint_] := With[
  {root = workerRoot, threads = ToString[workerThreads], requested = workerThreads, expected = fingerprint},
  TimeConstrained[ParallelEvaluate[
    (* Evaluate the platform on each worker, which may be a remote host. *)
    SetEnvironment[{"OMP_NUM_THREADS" -> ToString[If[$SystemID === "MacOSX-ARM64", Max[4, requested], requested]],
      "OMP_DYNAMIC" -> "FALSE", "OPENBLAS_NUM_THREADS" -> threads,
      "MKL_NUM_THREADS" -> threads, "MXNET_CPU_WORKER_NTHREADS" -> "1"}];
    Global`$HEPCATpath = FileNameJoin[{root, "source"}];
    Get[FileNameJoin[{Global`$HEPCATpath, "HEPCAT.wl"}]];
    Get[FileNameJoin[{root, "tests", "unscrambling.wl"}]];
    pipelineFingerprint[root] === expected,
    workers, DistributedContexts -> None], 60, $Failed] === ConstantArray[True, Length[workers]]
];

(* Only kernels launched here are closed. Explicit caller-supplied pools,
   including remote cluster kernels, remain connected after the call. *)
withTrainingWorkers[spec_, workerRoot_, workerThreads_, fingerprint_, body_] := Module[
  {before = trainingKernelSnapshot[], owned = {}, workers = {}, requested, result, initialized, launching = False,
    started = AbsoluteTime[], setupSeconds},
  CheckAbort[
    result = Catch[
      Which[
        spec === 1, Null,
        ListQ[spec] && spec =!= {} && DuplicateFreeQ[spec] && AllTrue[spec, MemberQ[before, #] &], workers = spec,
        spec === Automatic || (IntegerQ[spec] && spec > 1),
          requested = If[spec === Automatic, Max[1, $ProcessorCount - 1], spec];
          launching = True;
          TimeConstrained[launchTrainingKernels[requested], 60, Null];
          owned = Complement[trainingKernelSnapshot[], before]; workers = owned; launching = False;
          If[Length[workers] =!= requested, Message[TrainUnscrambleNet::kernels]; Throw[$Failed]],
        True, Message[TrainUnscrambleNet::kernels]; Throw[$Failed]
      ];
      If[workers =!= {},
        initialized = initializeTrainingWorkers[workers, workerRoot, workerThreads, fingerprint];
        If[!TrueQ[initialized], Message[TrainUnscrambleNet::kernels]; Throw[$Failed]]
      ];
      setupSeconds = AbsoluteTime[] - started;
      body[workers]
    ];
    If[owned =!= {}, closeTrainingKernels[owned]];
    If[AssociationQ[result], Join[result, <|"WorkerSetupSeconds" -> setupSeconds,
      "TotalWallSeconds" -> (AbsoluteTime[] - started)|>], result],
    If[launching, owned = Complement[trainingKernelSnapshot[], before]];
    If[owned =!= {}, closeTrainingKernels[owned]]; Abort[]
  ]
];


(* ::Subsection::Closed:: *)
(*Generated-Data Cache*)

cachedTrainingJobs[jobs_, workers_, settings_, cache_, fingerprint_] := Module[
  {keys, results, misses, computed, i, path, entry, tmp, key, writeOK},
  keys = IntegerString[Hash[{fingerprint, KeyDrop[settings, {"JobTimeLimit", "ValidationTimeLimit"}], #}, "SHA256"], 16, 64] & /@ jobs;
  results = ConstantArray[Missing["Cache"], Length[jobs]];
  If[StringQ[cache],
    If[!DirectoryQ[cache], CreateDirectory[cache, CreateIntermediateDirectories -> True]];
    Do[
      path = FileNameJoin[{cache, keys[[i]] <> ".wxf"}];
      If[FileExistsQ[path],
        entry = Quiet[Check[Import[path, "WXF"], $Failed]];
        If[AssociationQ[entry] && Lookup[entry, "Key", None] === keys[[i]] &&
            validJobResultsQ[{Lookup[entry, "Value", $Failed]}],
          results[[i]] = Join[entry["Value"], <|"CacheHit" -> True, "CPUSeconds" -> 0., "WallSeconds" -> 0.|>]]
      ], {i, Length[jobs]}]
  ];
  misses = Select[Range[Length[jobs]], MissingQ[results[[#]]] &];
  computed = mapTrainingJobs[jobs[[misses]], workers, settings];
  If[!ListQ[computed] || Length[computed] =!= Length[misses], Return[$Failed]];
  Do[
    i = misses[[key]];
    If[!validJobResultsQ[{computed[[key]]}], Continue[]];
    results[[i]] = Join[computed[[key]], <|"CacheHit" -> False|>];
    If[StringQ[cache],
      path = FileNameJoin[{cache, keys[[i]] <> ".wxf"}]; tmp = path <> "." <> CreateUUID[] <> ".tmp";
      writeOK = Quiet[Check[Export[tmp, <|"Key" -> keys[[i]], "Value" -> results[[i]]|>, "WXF"] =!= $Failed &&
        RenameFile[tmp, path, OverwriteTarget -> True] =!= $Failed, False]];
      If[FileExistsQ[tmp], DeleteFile[tmp]];
      If[!TrueQ[writeOK], Message[TrainUnscrambleNet::cache, cache]; Return[$Failed]]
    ], {key, Length[misses]}];
  If[validJobResultsQ[results], results, $Failed]
];


(* ::Subsection::Closed:: *)
(*Training Pipeline*)

installValidationModel[{}, _, _] := True;
installValidationModel[workers_, net_, directory_] := With[{model = net, dir = directory, method = $policyMethod},
  ParallelEvaluate[$modelNet = model; $loadedModelDirectory = dir; $loadedPolicyMethod = method; True, workers, DistributedContexts -> None] ===
    ConstantArray[True, Length[workers]]
];

TrainUnscrambleNet[amplitudes_List, OptionsPattern[]] := Block[
  {$onShellChannels = normalizeOnShellChannels[OptionValue["OnShellChannels"]],
    $useMomentumConservation = OptionValue["MomentumConservation"], $trainingBatchSize = OptionValue["TrainingBatchSize"],
    $trainingTargetDevice = OptionValue[TargetDevice], $maskCandidatePadding = OptionValue["MaskPadding"],
    $policyMethod = OptionValue["PolicyMethod"]},
  Module[{steps = OptionValue[Steps], count = OptionValue[Scrambles], hold = OptionValue[HoldOut],
      rounds = OptionValue[MaxTrainingRounds], episode = OptionValue[EpisodeLength], root = DirectoryName[$modelDirectory],
      workerRoot, cache = OptionValue["DataCacheDirectory"], settings, fingerprint, jobs, routeJobs,
      dir = resolveModelDirectory[OptionValue["ModelDirectory"]], workerThreads = OptionValue["WorkerThreads"]},
    If[!MemberQ[{True, False}, $maskCandidatePadding], Message[TrainUnscrambleNet::mask]; Return[$Failed]];
    If[!MemberQ[{"CandidateScorer", "ReverseMoves"}, $policyMethod] ||
        ($policyMethod === "ReverseMoves" && TrueQ[$maskCandidatePadding]),
      Message[TrainUnscrambleNet::method]; Return[$Failed]];
    If[!AllTrue[{steps, hold}, IntegerQ[#] && # >= 0 &] ||
        !AllTrue[{count, rounds, episode}, IntegerQ[#] && # > 0 &], Message[TrainUnscrambleNet::options]; Return[$Failed]];
    If[!validRoutePermutationCountQ[OptionValue["RoutePermutations"]],
      Message[TrainUnscrambleNet::routepermutations]; Return[$Failed]];
    If[!MemberQ[{"CPU", "GPU", "CUDA"}, $trainingTargetDevice] &&
        !MatchQ[$trainingTargetDevice, {("GPU" | "CUDA"), (All | _Integer?Positive)}],
      Message[TrainUnscrambleNet::device]; Return[$Failed]];
    If[!AllTrue[{workerThreads, $trainingBatchSize}, IntegerQ[#] && # > 0 &] ||
        !AllTrue[{OptionValue["JobTimeLimit"], OptionValue["ValidationTimeLimit"]}, NumberQ[#] && TrueQ[# > 0] &] ||
        !(cache === None || StringQ[cache]) || !(OptionValue["WorkerRoot"] === Automatic || StringQ[OptionValue["WorkerRoot"]]),
      Message[TrainUnscrambleNet::parallelopts]; Return[$Failed]];
    workerRoot = Replace[OptionValue["WorkerRoot"], Automatic -> root];
    If[StringQ[cache], cache = ExpandFileName[cache]];
    settings = <|"OnShellChannels" -> $onShellChannels, "MomentumConservation" -> $useMomentumConservation,
      "PolicyMethod" -> $policyMethod,
      "JobTimeLimit" -> OptionValue["JobTimeLimit"], "ValidationTimeLimit" -> OptionValue["ValidationTimeLimit"]|>;
    routeJobs = makeSimplificationRouteJobs[OptionValue["SimplificationRoutes"], OptionValue["RoutePermutations"]];
    If[routeJobs === $Failed, Return[$Failed]];
    fingerprint = pipelineFingerprint[root];
    jobs = MapIndexed[Join[#1, <|"Order" -> First[#2]|>] &,
      Join[makeTrainingJobs[amplitudes, steps, count], routeJobs]];
    withTrainingWorkers[OptionValue[Kernels], workerRoot, workerThreads, fingerprint, Function[workers,
      runTrainingPipeline[amplitudes, jobs, workers, settings, cache, fingerprint, steps, count, hold, episode, rounds, dir]]]
  ]
];


(* ::Subsection::Closed:: *)
(*Progress and Saved Results*)

trainingStage[name_String] := Module[{path = Environment["HEPCAT_TRAIN_STAGE_FILE"]},
  Print["Training stage: ", name];
  If[StringQ[path], Export[path, name, "Text"]];
];

runTrainingPipeline[amplitudes_, jobs_, workers_, settings_, cache_, fingerprint_, steps_, count_, hold_, episode_, rounds_, dir_] := Module[
  {generated, rows, targets, skipped, net, paths, validation, dataSeconds, trainingSeconds, trainingCPU, validationSeconds,
    validationJobs, candidateRows, work, collected},
  trainingStage["Data generation"];
  {dataSeconds, generated} = AbsoluteTiming[cachedTrainingJobs[jobs, workers, settings, cache, fingerprint]];
  If[!validJobResultsQ[generated], Message[TrainUnscrambleNet::job, "data generation"]; Return[$Failed]];
  collected = collectTrainingRows[generated, jobs]; rows = collected["Rows"];
  targets = Total[Lookup[Lookup[generated, "Result"], "Targets"]];
  skipped = Total[Lookup[Lookup[generated, "Result"], "Skipped"]];
  generated = KeyDrop[#, "Result"] & /@ generated;
  If[rows === {} || targets === skipped, Message[TrainUnscrambleNet::nodata]; Return[$Failed]];
  trainingStage["NN training"];
  If[!DirectoryQ[dir], CreateDirectory[dir, CreateIntermediateDirectories -> True]];
  trainingCPU = TimeUsed[];
  {trainingSeconds, net} = AbsoluteTiming[Block[{$trainingProgressFile = FileNameJoin[{dir,
      If[$policyMethod === "ReverseMoves", "move-training-progress.m", "training-progress.m"]}]},
    trainCandidateNetwork[rows, rounds]]];
  trainingCPU = TimeUsed[] - trainingCPU;
  If[!MatchQ[net, _NetChain | _NetGraph], Return[$Failed]];
  $modelNet = net; $loadedModelDirectory = dir; $loadedPolicyMethod = $policyMethod;
  If[!DirectoryQ[dir], CreateDirectory[dir, CreateIntermediateDirectories -> True]];
  paths = modelPaths[dir];
  If[Export[paths["Net"], net] === $Failed, Message[TrainUnscrambleNet::save, dir]; Return[$Failed]];
  Put[Join[<|"Encoding" -> "SplitFullFormUTF8", "Architecture" -> policyArchitecture[],
    "MoveSlots" -> $moveSlots,
    "MoveSourceHash" -> FileHash[FileNameJoin[{$modelDirectory, "unscrambling.wl"}], "SHA256"],
    "OnShellChannels" -> $onShellChannels, "MomentumConservation" -> $useMomentumConservation|>,
    If[$policyMethod === "ReverseMoves", <|"Objective" -> "LegalMoveCrossEntropy"|>, <||>]], paths["Metadata"]];
  validationJobs = makeValidationJobs[amplitudes, steps, count, hold, episode];
  Put[<|"Status" -> "Model saved; validation pending", "ModelPath" -> paths["Net"],
    "TrainingSeconds" -> trainingSeconds, "TrainingCPUSeconds" -> trainingCPU,
    "TrainingStates" -> Length[rows], "ValidationCases" -> Length[validationJobs]|>,
    FileNameJoin[{dir, If[$policyMethod === "ReverseMoves", "move-training-checkpoint.m", "training-checkpoint.m"]}]];
  Print["Model saved: ", paths["Net"]];
  trainingStage["Validation"];
  {validationSeconds, validation} = AbsoluteTiming[
    If[validationJobs =!= {} && !installValidationModel[workers, net, dir], $Failed,
      mapTrainingJobs[validationJobs, workers, settings]]];
  If[!validJobResultsQ[validation], Message[TrainUnscrambleNet::job, "validation (model already saved)"]; Return[$Failed]];
  Put[validation, FileNameJoin[{dir, If[$policyMethod === "ReverseMoves", "move-validation-results.m", "validation-results.m"]}]];
  trainingStage["Complete"];
  candidateRows = If[$policyMethod === "ReverseMoves", Total[Lookup[rows, "ActionCount"]], Total[Length /@ Lookup[rows, "Target"]]];
  work = If[$policyMethod === "ReverseMoves", <|"StateBytesPerEpoch" -> Total[Length /@ Lookup[rows, "State"]],
    "EditBytesWithTrainingPadding" -> 0|>, trainingWorkEstimate[rows]];
  <|"PolicyMethod" -> $policyMethod, "Kernels" -> Max[1, Length[workers]], "Starts" -> Length[amplitudes]*count,
    "SimplificationRouteJobs" -> Count[Lookup[jobs, "Mode"], "SimplificationRoute"],
    "SimplificationRouteNames" -> DeleteDuplicates[Lookup[Select[jobs, #["Mode"] === "SimplificationRoute" &], "Name", {}]],
    "DiscardedConflictingStopExamples" -> collected["DiscardedStopExamples"],
    "TrainingStates" -> Length[rows], "TrainingRows" -> candidateRows, "TrainingBatchSize" -> $trainingBatchSize,
    "TrainingTargetDevice" -> $trainingTargetDevice,
    "ReverseSteps" -> targets - skipped, "SkippedReverseSteps" -> skipped,
    "CacheHits" -> Count[Lookup[generated, "CacheHit"], True],
    "HoldoutScrambles" -> Length[validation],
    "HoldoutSimpler" -> Total[Lookup[Lookup[validation, "Result"], "Simpler"]],
    "HoldoutRecovered" -> Total[Lookup[Lookup[validation, "Result"], "Recovered"]],
    "DataGenerationSeconds" -> dataSeconds, "TrainingSeconds" -> trainingSeconds,
    "TrainingCPUSeconds" -> trainingCPU, "TrainingDataBytes" -> ByteCount[rows],
    "ValidationSeconds" -> validationSeconds,
    "GenerationWorkerCPUSeconds" -> Total[Lookup[generated, "CPUSeconds"]],
    "ValidationWorkerCPUSeconds" -> Total[Lookup[validation, "CPUSeconds"]],
    "EncodingWork" -> work,
    "GenerationJobs" -> (KeyDrop[#, "Result"] & /@ generated),
    "ValidationJobs" -> (KeyDrop[#, "Result"] & /@ validation),
    "ModelPath" -> paths["Net"]|>
];

trainingWorkEstimate[rows_] := Module[{counts, widths, width, stateBytes, edits},
  counts = Length /@ Lookup[rows, "Candidates"];
  edits = Flatten[Lookup[rows, "Candidates"], 1];
  width = Max[Length /@ edits];
  stateBytes = Length /@ Lookup[rows, "State"];
  <|"StateBytesPerEpoch" -> Total[stateBytes],
    "StateBytesWithoutSharing" -> Total[stateBytes*counts],
    "EditBytesWithoutPadding" -> Total[Count[#, Except[257]] & /@ edits],
    "EditBytesWithTrainingPadding" -> (width*Total[counts]),
    "MaximumEditWidth" -> width|>
];



(* ::Subsection::Closed:: *)
(*Unscrambling and Trace*)

UnscrambleSpinorAmplitudes::nomodel = "No compatible model files for the selected model were found in `1`.";
UnscrambleSpinorAmplitudes::model = "Model must be Automatic, CandidateScorer, or CompactActions (string values).";
UnscrambleSpinorAmplitudes::method = "PolicyMethod must be CandidateScorer, ReverseMoves, or CompactActions (string values).";
UnscrambleSpinorAmplitudes::directory = "ModelDirectory must be Automatic or a directory path string.";
UnscrambleSpinorAmplitudes::scores = "The model did not return one numeric score per candidate.";
UnscrambleSpinorAmplitudes::options = "MaxSteps must be Infinity or a nonnegative integer; Attempts and MaxStagnantSteps must be positive integers; TimeLimit must be a positive number of seconds.";
UnscrambleSpinorAmplitudes::batchsize = "CandidateBatchSize must be a positive integer.";
UnscrambleSpinorAmplitudes::progress = "ProgressReporting must be Automatic, True, or False.";
UnscrambleSpinorAmplitudes::kernels = "Kernels must be Automatic or a positive integer specifying local inference workers.";
UnscrambleSpinorAmplitudes::kernellimit = "Requested `1` inference workers; using `2` available local workers.";
UnscrambleSpinorAmplitudes::workerinit = "Inference workers could not load the package and selected model.";
UnscrambleSpinorAmplitudes::seed = "RandomSeed must be Automatic or an integer.";

unscrambleProgressPanel[progress_, elapsed_] := If[Lookup[progress, "Mode", "Sequential"] === "Parallel",
  Column[{
    Grid[{
      {Style["Parallel unscrambling", Bold], progress["Stage"]},
      {"Routes / group restarts", Row[{progress["Workers"], " / ", progress["Restarts"]}]},
      {"Best terms / expanded terms / complexity", Row[{progress["BestTerms"], " / ",
        progress["BestExpandedTerms"], " / ", progress["BestComplexity"]}]},
      {"Total completed edits / elapsed seconds", Row[{progress["Steps"], " / ", NumberForm[elapsed, {10, 1}]}]}
    }, Alignment -> Left, Spacings -> {1.5, 0.3}],
    Grid[Prepend[(With[{r = #}, {r["RouteID"], Lookup[r, "WorkerID", "-"],
        r["Status"], Row[{r["Attempt"], " / ", progress["Attempts"]}],
        If[TrueQ[r["Greedy"]], "Greedy", NumberForm[r["Temperature"], {4, 2}]],
        r["Steps"], Row[{r["StagnantSteps"], " / ", progress["MaxStagnantSteps"]}],
        Row[{r["DuplicateAvoided"], " / ", r["LoopAvoided"]}],
        r["CurrentTerms"], r["CurrentComplexity"], NumberForm[r["ElapsedSeconds"], {10, 1}]}] &) /@
      Lookup[progress, "Routes", {}],
      Style[#, Bold] & /@ {"Route", "Kernel", "State", "Attempt", "Sampling", "Edits", "Patience",
        "Skipped dup / loop", "Terms", "Complexity", "Work (s)"}],
      Alignment -> Left, Spacings -> {1., 0.3}]
  }, Spacings -> 1.],
  Grid[{
  {Style["Unscrambling", Bold], progress["Stage"]},
  {"Attempt / total steps", Row[{progress["Attempt"], " / ", progress["Attempts"],
    "    |    ", progress["Steps"]}]},
  {"Terms (current / best)", Row[{progress["CurrentTerms"], " / ", progress["BestTerms"]}]},
  {"Expanded terms (current / best)", Row[{progress["CurrentExpandedTerms"], " / ", progress["BestExpandedTerms"]}]},
  {"Complexity (current / best)", Row[{progress["CurrentComplexity"], " / ", progress["BestComplexity"]}]},
  {"Neutral splits", progress["NeutralSplits"]},
  {"Without improvement", Row[{progress["StagnantSteps"], " / ", progress["MaxStagnantSteps"]}]},
  {"Elapsed seconds", NumberForm[elapsed, {10, 1}]}
}, Alignment -> Left, Spacings -> {1.5, 0.3}]];

(* The display reads cached numbers; dynamic refresh never traverses an amplitude. *)
SetAttributes[withUnscrambleProgress, HoldAll];
withUnscrambleProgress[body_, progress_, started_] := If[
  $progressReporting =!= False && TrueQ[$Notebooks] && MatchQ[$FrontEnd, _FrontEndObject],
  Monitor[body, Refresh[unscrambleProgressPanel[progress, AbsoluteTime[] - started],
    UpdateInterval -> 0.5, TrackedSymbols :> {progress}], 0.3], body];

runUnscramble[{rules_List, amplitude_}, record_, maxSteps_, attempts_, dir_] := Module[
  {expr = canonicalAmplitude[amplitude, rules], checkpoint, trial, before, selection, move, next,
    history = {}, checkpoints = {}, accepted, failed = False, attempt,
    stagnant = 0, reason = "StepLimit", loaded = False, bestScore, nextScore, trialScore,
    step, totalSteps = 0, neutralSplits = 0, neutralSplit, started = AbsoluteTime[], progress, currentCounts, bestCounts},
  checkpoint = expr; bestScore = expressionComplexity[checkpoint];
  bestCounts = expressionTermCounts[checkpoint]; currentCounts = bestCounts;
  progress = <|"Stage" -> "Loading model", "Attempt" -> 0, "Attempts" -> attempts, "Steps" -> 0,
    "CurrentTerms" -> currentCounts["Terms"], "BestTerms" -> bestCounts["Terms"],
    "CurrentExpandedTerms" -> currentCounts["ExpandedTerms"], "BestExpandedTerms" -> bestCounts["ExpandedTerms"],
    "CurrentComplexity" -> bestScore, "BestComplexity" -> bestScore,
    "StagnantSteps" -> 0, "MaxStagnantSteps" -> $maxStagnantSteps, "NeutralSplits" -> 0,
    "CandidateBatchSize" -> $candidateBatchSize|>;
  withUnscrambleProgress[TimeConstrained[
  loaded = loadUnscrambleModel[dir];
  If[!loaded, Message[UnscrambleSpinorAmplitudes::nomodel, dir];
    reason = "ModelUnavailable",
  Do[
    before = checkpoint; trial = checkpoint; trialScore = bestScore;
    stagnant = 0; step = 0; reason = "StepLimit";
    progress = Join[progress, <|"Attempt" -> attempt, "StagnantSteps" -> 0,
      "CurrentTerms" -> bestCounts["Terms"], "CurrentExpandedTerms" -> bestCounts["ExpandedTerms"],
      "CurrentComplexity" -> bestScore|>];
    While[maxSteps === Infinity || step < maxSteps,
      progress = Join[progress, <|"Stage" -> "Choosing edit"|>];
      selection = selectCandidate[trial, rules, attempt > 1];
      If[selection === $Failed, Message[UnscrambleSpinorAmplitudes::scores]; failed = True; reason = "ScoringFailed"; Break[]];
      progress = Join[progress, <|"Stage" -> "Applying edit"|>];
      move = selection["Candidate"]; next = applyCandidate[trial, move];
      nextScore = expressionComplexity[next];
      currentCounts = expressionTermCounts[next]; step++; totalSteps++;
      neutralSplit = move["Name"] === "MomentumSum" && Lookup[move, "Identity", None] === "Split" &&
        nextScore === trialScore && !samePoly[trial, next];
      If[neutralSplit, neutralSplits++];
      If[nextScore < bestScore,
        checkpoint = next; bestScore = nextScore; bestCounts = currentCounts; stagnant = 0,
        If[!neutralSplit, stagnant++]];
      progress = Join[progress, <|"Steps" -> totalSteps,
        "CurrentTerms" -> currentCounts["Terms"], "BestTerms" -> bestCounts["Terms"],
        "CurrentExpandedTerms" -> currentCounts["ExpandedTerms"], "BestExpandedTerms" -> bestCounts["ExpandedTerms"],
        "CurrentComplexity" -> nextScore, "BestComplexity" -> bestScore, "StagnantSteps" -> stagnant,
        "NeutralSplits" -> neutralSplits|>];
      If[TrueQ[record], AppendTo[history, Join[selection, <|"Attempt" -> attempt,
        "Before" -> trial, "After" -> next, "Changed" -> !samePoly[trial, next],
        "ComplexityBefore" -> trialScore, "ComplexityAfter" -> nextScore|>]]];
      trial = next; trialScore = nextScore;
      If[move["Name"] === "Stop", reason = "Stop"; Break[]];
      If[stagnant >= $maxStagnantSteps, reason = "Stagnation"; Break[]];
    ];
    accepted = expressionComplexity[checkpoint] < expressionComplexity[before];
    If[TrueQ[record], AppendTo[checkpoints, <|"Attempt" -> attempt, "Before" -> before,
      "Trial" -> trial, "Accepted" -> accepted, "After" -> checkpoint|>]];
    If[failed, Break[]], {attempt, attempts}
  ]], $timeLimit, reason = "TimeLimit"], progress, started];
  progress = Join[progress, <|"Stage" -> reason, "ElapsedSeconds" -> AbsoluteTime[] - started|>];
  If[reason === "ModelUnavailable", Return[<|"Result" -> {rules, amplitude}, "Trace" -> {},
    "Checkpoints" -> {}, "ModelLoaded" -> False, "TerminationReason" -> reason, "Progress" -> progress|>]];
  <|"PolicyMethod" -> $policyMethod, "Result" -> {rules, checkpoint}, "Trace" -> history, "Checkpoints" -> checkpoints,
    "ModelLoaded" -> loaded, "ScoringFailed" -> failed, "TerminationReason" -> reason,
    "StagnantSteps" -> stagnant, "Progress" -> progress|>
];

(* ::Subsection::Closed:: *)
(*Cooperative Parallel Search*)

publicInferenceRoute[route_Association] := KeyTake[route, {"RouteID", "WorkerID", "Status", "Attempt",
  "Steps", "StagnantSteps", "CurrentTerms", "CurrentExpandedTerms", "CurrentComplexity",
  "NeutralSplits", "ElapsedSeconds", "Generation", "Temperature", "Greedy", "DuplicateAvoided", "LoopAvoided"}];

inferenceRouteTemperature[method_, index_Integer, count_Integer] :=
  If[method === "CandidateScorer", 0.2, 1.] *
    If[count <= 1, 1., 0.5 Exp[Log[4.] (index - 1)/(count - 1)]];

runParallelUnscramble[{rules_List, amplitude_}, record_, maxSteps_, attempts_, dir_, spec_, seedSpec_] := Module[
  {started = AbsoluteTime[], checkpoint = amplitude, bestScore = Infinity, bestCounts,
    loaded = False, failed = False, reason = "RoutesExhausted", generation = 0, restarts = 0,
    routes = <||>, tickets = {}, history = {}, checkpoints = {}, totalSteps = 0, neutralSplits = 0,
    sharedMoves = <||>, duplicateAvoided = 0, loopAvoided = 0,
    progress, seed, settings, result, remaining, refresh, resetRoutes, submitRoute, finishAttempt,
    runPool, workers = {}, packet, response, ticket, route, id, counts, beforeBest, cancelled,
    stateKey, moveKey, stateMoves, moveVisit, rejectedLoops},
  remaining[] := Max[0., $timeLimit - (AbsoluteTime[] - started)];
  bestCounts = <|"Terms" -> 0, "ExpandedTerms" -> 0|>;
  progress = <|"Mode" -> "Parallel", "Stage" -> "Preparing expression", "Workers" -> 0,
    "RequestedWorkers" -> spec, "Attempts" -> attempts, "Steps" -> 0, "Restarts" -> 0,
    "Generation" -> generation, "MaxStagnantSteps" -> $maxStagnantSteps,
    "BestTerms" -> 0, "BestExpandedTerms" -> 0, "BestComplexity" -> Null,
    "Routes" -> {}, "NeutralSplits" -> 0, "DuplicateAvoided" -> 0, "LoopAvoided" -> 0,
    "CandidateBatchSize" -> $candidateBatchSize|>;
  refresh[stage_] := (progress = Join[progress, <|"Stage" -> stage, "Workers" -> Length[workers],
    "Generation" -> generation, "Restarts" -> restarts, "Steps" -> totalSteps,
    "BestTerms" -> bestCounts["Terms"], "BestExpandedTerms" -> bestCounts["ExpandedTerms"],
    "BestComplexity" -> If[bestScore === Infinity, Null, bestScore], "NeutralSplits" -> neutralSplits,
    "DuplicateAvoided" -> duplicateAvoided, "LoopAvoided" -> loopAvoided,
    "Routes" -> (publicInferenceRoute /@ Values[routes])|>]);
  resetRoutes[] := (sharedMoves = <||>; Do[
    route = Lookup[routes, index, <|"Steps" -> 0, "NeutralSplits" -> 0, "ElapsedSeconds" -> 0.,
      "DuplicateAvoided" -> 0, "LoopAvoided" -> 0|>];
    routes[index] = Join[route, <|"RouteID" -> index, "Worker" -> workers[[index]],
      "Generation" -> generation, "Expression" -> checkpoint, "Attempt" -> 1,
      "AttemptSteps" -> 0, "StagnantSteps" -> 0, "Status" -> "Ready",
      "Temperature" -> inferenceRouteTemperature[$policyMethod, index, Length[workers]],
      "Greedy" -> (index === 1), "VisitedStateKeys" -> {inferenceStateKey[checkpoint]},
      "DrawIndex" -> 0, "DiversityRetries" -> 0, "ExcludedMoveKeys" -> {},
      "CurrentTerms" -> bestCounts["Terms"], "CurrentExpandedTerms" -> bestCounts["ExpandedTerms"],
      "CurrentComplexity" -> bestScore|>], {index, Length[workers]}]);
  submitRoute[index_] := Module[{r = routes[index], request, handle},
    If[remaining[] <= 0., Return[False]];
    request = <|"Expression" -> r["Expression"], "Rules" -> rules, "RouteID" -> index,
      "Generation" -> generation, "Seed" -> Hash[{seed, index, generation, r["Attempt"]}],
      "DrawIndex" -> r["DrawIndex"] + 1, "Seconds" -> remaining[],
      "Temperature" -> r["Temperature"], "Greedy" -> r["Greedy"],
      "VisitedStateKeys" -> r["VisitedStateKeys"], "ExcludedMoveKeys" -> r["ExcludedMoveKeys"],
      "MoveVisits" -> Map[#["Count"] &, Lookup[sharedMoves, inferenceStateKey[r["Expression"]], <||>]]|>;
    If[r["AttemptSteps"] === 0 && r["DiversityRetries"] === 0,
      AssociateTo[request, "FirstChoice" -> (index + Length[workers]*(r["Attempt"] - 1))]];
    handle = inferenceSubmit[r["Worker"], request];
    If[!AssociationQ[handle], failed = True; reason = "WorkerFailed"; Return[False]];
    AppendTo[tickets, handle]; routes[index] = Join[r, <|"Status" -> "Choosing edit",
      "DrawIndex" -> request["DrawIndex"]|>]; True
  ];
  finishAttempt[index_, status_] := Module[{r = routes[index]},
    If[r["Attempt"] < attempts && remaining[] > 0.,
      routes[index] = Join[r, <|"Attempt" -> r["Attempt"] + 1, "AttemptSteps" -> 0,
        "StagnantSteps" -> 0, "Expression" -> checkpoint,
        "Greedy" -> False, "VisitedStateKeys" -> {inferenceStateKey[checkpoint]},
        "DrawIndex" -> 0, "DiversityRetries" -> 0, "ExcludedMoveKeys" -> {},
        "CurrentTerms" -> bestCounts["Terms"], "CurrentExpandedTerms" -> bestCounts["ExpandedTerms"],
        "CurrentComplexity" -> bestScore, "Status" -> "Ready"|>]; submitRoute[index],
      routes[index] = Join[r, <|"Status" -> status|>]]
  ];
  runPool[pool_] := Module[{outcome},
    workers = pool; resetRoutes[]; refresh["Starting routes"];
    outcome = TimeConstrained[
      Do[submitRoute[index], {index, Length[workers]}];
      While[tickets =!= {} && !failed && remaining[] > 0.,
        refresh["Searching"];
        response = inferenceReceive[tickets, Min[0.1, remaining[]]];
        If[!AssociationQ[response], failed = True; reason = "WorkerFailed"; Break[]];
        If[!TrueQ[Lookup[response, "Ready", False]], Pause[0.01]; Continue[]];
        ticket = response["Handle"]; packet = response["Result"];
        (* A completed old-generation edit cannot overwrite a newer checkpoint. *)
        If[!AssociationQ[packet] || !MemberQ[tickets, ticket] ||
            Lookup[packet, "Generation", -1] =!= generation,
          If[MemberQ[tickets, ticket], tickets = DeleteCases[tickets, ticket]]; Continue[]];
        tickets = DeleteCases[tickets, ticket];
        id = Lookup[packet, "RouteID", 0];
        If[!KeyExistsQ[routes, id] || ticket["RouteID"] =!= id,
          failed = True; reason = "WorkerFailed"; Break[]];
        route = routes[id];
        If[Lookup[packet, "Status", "ScoringFailed"] === "ScoringFailed",
          Message[UnscrambleSpinorAmplitudes::scores]; failed = True; reason = "ScoringFailed"; Break[]];
        rejectedLoops = Lookup[packet, "LoopAvoided", 0]; loopAvoided += rejectedLoops;
        route = Join[route, <|"WorkerID" -> Lookup[packet, "WorkerID", Lookup[route, "WorkerID", "-"]],
          "ElapsedSeconds" -> route["ElapsedSeconds"] + Lookup[packet, "ElapsedSeconds", 0.],
          "LoopAvoided" -> route["LoopAvoided"] + rejectedLoops|>]; routes[id] = route;
        If[packet["Status"] === "TimeLimit", finishAttempt[id, "TimeLimit"]; Continue[]];
        If[packet["Status"] === "NoMoves", finishAttempt[id, "NoMoves"]; Continue[]];
        If[!MemberQ[{"Edited", "Stop"}, packet["Status"]],
          failed = True; reason = "WorkerFailed"; Break[]];
        If[!SameQ[packet["Before"], route["Expression"]] ||
            !NumberQ[packet["ComplexityAfter"]] || !AssociationQ[packet["Counts"]],
          failed = True; reason = "WorkerFailed"; Break[]];
        stateKey = inferenceStateKey[packet["Before"]];
        moveKey = Lookup[packet["Selection"], "MoveKey", inferenceMoveKey[packet["Selection"]["Candidate"]]];
        stateMoves = Lookup[sharedMoves, stateKey, <||>];
        moveVisit = Lookup[stateMoves, moveKey, <|"Count" -> 0, "Routes" -> {}|>];
        (* Retry collisions briefly, then allow shared prefixes with different future choices. *)
        If[packet["Status"] === "Edited" && packet["ComplexityAfter"] >= bestScore &&
            AnyTrue[moveVisit["Routes"], # =!= id &] && route["DiversityRetries"] < 3 &&
            !TrueQ[Lookup[packet, "SharedMoveFallback", False]],
          duplicateAvoided++;
          routes[id] = Join[route, <|"DuplicateAvoided" -> route["DuplicateAvoided"] + 1,
            "DiversityRetries" -> route["DiversityRetries"] + 1,
            "ExcludedMoveKeys" -> Append[route["ExcludedMoveKeys"], moveKey]|>];
          submitRoute[id]; Continue[]
        ];
        If[packet["Status"] === "Edited",
          AssociateTo[stateMoves, moveKey -> <|"Count" -> moveVisit["Count"] + 1,
            "Routes" -> Union[moveVisit["Routes"], {id}]|>];
          AssociateTo[sharedMoves, stateKey -> stateMoves]];
        counts = packet["Counts"]; totalSteps++;
        If[TrueQ[packet["NeutralSplit"]], neutralSplits++];
        route = Join[route, <|"Expression" -> packet["After"], "WorkerID" -> packet["WorkerID"],
          "Steps" -> route["Steps"] + 1, "AttemptSteps" -> route["AttemptSteps"] + 1,
          "VisitedStateKeys" -> Append[route["VisitedStateKeys"], inferenceStateKey[packet["After"]]],
          "DiversityRetries" -> 0, "ExcludedMoveKeys" -> {},
          "CurrentTerms" -> counts["Terms"], "CurrentExpandedTerms" -> counts["ExpandedTerms"],
          "CurrentComplexity" -> packet["ComplexityAfter"],
          "StagnantSteps" -> route["StagnantSteps"] + Boole[!TrueQ[packet["NeutralSplit"]]],
          "NeutralSplits" -> route["NeutralSplits"] + Boole[TrueQ[packet["NeutralSplit"]]]|>];
        routes[id] = route;
        If[TrueQ[record], AppendTo[history, Join[packet["Selection"],
          KeyTake[packet, {"RouteID", "Generation", "WorkerID", "Before", "After", "Changed",
            "ComplexityBefore", "ComplexityAfter", "StartedAt", "FinishedAt", "ElapsedSeconds"}],
          <|"Attempt" -> route["Attempt"], "Temperature" -> route["Temperature"], "Greedy" -> route["Greedy"],
            "LoopAvoided" -> rejectedLoops, "DuplicateAvoided" -> route["DuplicateAvoided"]|>]]];
        If[packet["ComplexityAfter"] < bestScore,
          beforeBest = checkpoint; checkpoint = packet["After"];
          bestScore = packet["ComplexityAfter"]; bestCounts = counts;
          If[TrueQ[record], AppendTo[checkpoints, <|"RouteID" -> id, "WorkerID" -> packet["WorkerID"],
            "Generation" -> generation, "Attempt" -> route["Attempt"],
            "Before" -> beforeBest, "After" -> checkpoint, "Accepted" -> True|>]];
          generation++; restarts++; refresh["Restarting all routes"];
          cancelled = cancelInference[tickets, workers]; tickets = {};
          If[!TrueQ[cancelled], failed = True; reason = "WorkerFailed"; Break[]];
          resetRoutes[]; Do[submitRoute[index], {index, Length[workers]}]; Continue[]
        ];
        Which[
          packet["Status"] === "Stop", finishAttempt[id, "Stop"],
          route["StagnantSteps"] >= $maxStagnantSteps, finishAttempt[id, "Stagnation"],
          maxSteps =!= Infinity && route["AttemptSteps"] >= maxSteps, finishAttempt[id, "StepLimit"],
          True, submitRoute[id]
        ];
      ];
      If[remaining[] <= 0., reason = "TimeLimit"];
      True,
      Max[0.001, remaining[]], reason = "TimeLimit"; $TimedOut];
    cancelInference[tickets, workers]; tickets = {};
    outcome
  ];
  settings = <|"PolicyMethod" -> $policyMethod, "OnShellChannels" -> $onShellChannels,
    "MomentumConservation" -> $useMomentumConservation, "CandidateBatchSize" -> $candidateBatchSize|>;
  seed = If[IntegerQ[seedSpec], seedSpec, RandomInteger[{0, 2^31 - 1}]];
  withUnscrambleProgress[
    CheckAbort[
      result = TimeConstrained[
        checkpoint = canonicalAmplitude[amplitude, rules]; bestScore = expressionComplexity[checkpoint];
        bestCounts = expressionTermCounts[checkpoint]; refresh["Loading model"];
        If[maxSteps === 0, reason = "StepLimit", loaded = loadUnscrambleModel[dir];
          If[!TrueQ[loaded], Message[UnscrambleSpinorAmplitudes::nomodel, dir]; reason = "ModelUnavailable",
            refresh["Starting workers"];
            result = withInferenceWorkers[spec, dir, settings, remaining[], runPool];
            If[result === $Failed && reason === "RoutesExhausted", reason = "WorkerUnavailable"]]];
        True,
        $timeLimit, reason = "TimeLimit"; $TimedOut],
      cancelInference[tickets, workers]; tickets = {}; reason = "Aborted"
    ], progress, started];
  routes = Map[If[MemberQ[{"Ready", "Choosing edit"}, #["Status"]],
    Join[#, <|"Status" -> If[reason === "RoutesExhausted", "Finished", reason]|>], #] &, routes];
  refresh[reason]; progress = Join[progress, <|"ElapsedSeconds" -> (AbsoluteTime[] - started)|>];
  <|"PolicyMethod" -> $policyMethod, "Result" -> {rules, If[reason === "ModelUnavailable", amplitude, checkpoint]},
    "Trace" -> history, "Checkpoints" -> checkpoints, "ModelLoaded" -> TrueQ[loaded],
    "ScoringFailed" -> failed, "TerminationReason" -> reason, "Progress" -> progress,
    "Routes" -> progress["Routes"], "Workers" -> Length[workers], "Generation" -> generation,
    "Restarts" -> restarts, "DuplicateAvoided" -> duplicateAvoided, "LoopAvoided" -> loopAvoided,
    "StagnantSteps" -> If[routes === <||>, 0,
      Max[Lookup[Values[routes], "StagnantSteps"]]]|>
];

configuredRun[pair:{_List, _}, record_, OptionsPattern[UnscrambleTrace]] := Block[
  {$onShellChannels = normalizeOnShellChannels[OptionValue["OnShellChannels"]],
    $useMomentumConservation = OptionValue["MomentumConservation"],
    $maxStagnantSteps = OptionValue["MaxStagnantSteps"], $timeLimit = OptionValue["TimeLimit"],
    $candidateBatchSize = OptionValue["CandidateBatchSize"],
    $progressReporting = OptionValue["ProgressReporting"],
    $policyMethod = Switch[OptionValue["Model"], Automatic, OptionValue["PolicyMethod"],
      "CandidateScorer" | "Current", "CandidateScorer", "CompactActions", "CompactActions", _, $Failed]},
  If[$policyMethod === $Failed, Message[UnscrambleSpinorAmplitudes::model]; Return[$Failed]];
  If[!MemberQ[{"CandidateScorer", "ReverseMoves", "CompactActions"}, $policyMethod],
    Message[UnscrambleSpinorAmplitudes::method]; Return[$Failed]];
  If[!MatchQ[OptionValue["ModelDirectory"], Automatic | _String],
    Message[UnscrambleSpinorAmplitudes::directory]; Return[$Failed]];
  If[!IntegerQ[$candidateBatchSize] || $candidateBatchSize < 1,
    Message[UnscrambleSpinorAmplitudes::batchsize]; Return[$Failed]];
  If[!TrueQ[OptionValue["MaxSteps"] === Infinity ||
        (IntegerQ[OptionValue["MaxSteps"]] && OptionValue["MaxSteps"] >= 0)] ||
      !IntegerQ[OptionValue["Attempts"]] || OptionValue["Attempts"] < 1 ||
      !IntegerQ[$maxStagnantSteps] || $maxStagnantSteps < 1 ||
      !NumberQ[$timeLimit] || !TrueQ[$timeLimit > 0],
    Message[UnscrambleSpinorAmplitudes::options]; Return[$Failed]];
  If[!MemberQ[{Automatic, True, False}, $progressReporting],
    Message[UnscrambleSpinorAmplitudes::progress]; Return[$Failed]];
  If[!(OptionValue["Kernels"] === Automatic ||
      (IntegerQ[OptionValue["Kernels"]] && OptionValue["Kernels"] > 0)),
    Message[UnscrambleSpinorAmplitudes::kernels]; Return[$Failed]];
  If[!(OptionValue[RandomSeed] === Automatic || IntegerQ[OptionValue[RandomSeed]]),
    Message[UnscrambleSpinorAmplitudes::seed]; Return[$Failed]];
  If[OptionValue["Kernels"] === 1,
    If[IntegerQ[OptionValue[RandomSeed]], BlockRandom[SeedRandom[OptionValue[RandomSeed]];
      runUnscramble[pair, record, OptionValue["MaxSteps"], OptionValue["Attempts"],
        resolveModelDirectory[OptionValue["ModelDirectory"]]]],
      runUnscramble[pair, record, OptionValue["MaxSteps"], OptionValue["Attempts"],
        resolveModelDirectory[OptionValue["ModelDirectory"]]]],
    runParallelUnscramble[pair, record, OptionValue["MaxSteps"], OptionValue["Attempts"],
      resolveModelDirectory[OptionValue["ModelDirectory"]], OptionValue["Kernels"], OptionValue[RandomSeed]]]
];
UnscrambleTrace[pair:{_List, _}, opts:OptionsPattern[]] :=
  configuredRun[pair, True, opts, Sequence @@ Options[UnscrambleTrace]];
UnscrambleSpinorAmplitudes[pair:{_List, _}, opts:OptionsPattern[]] := Module[{result},
  result = configuredRun[pair, False, opts, Sequence @@ Options[UnscrambleSpinorAmplitudes]];
  If[AssociationQ[result], result["Result"], $Failed]
];


(* ::Subsection::Closed:: *)
(*End Package*)

End[];
EndPackage[];
