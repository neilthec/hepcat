(* ::Package:: *)

(* Neural-guided simplification, legal identity scrambling, and training.

   Every stored expression has the mass rules already applied, so Mass[1]
   is Me (or 0, MW, MZ, ...) and never the head Mass. After each identity
   the whole expression is expanded. RecordSteps can retain intermediate
   expressions and exact edit records for supervised reverse trajectories.
*)


(* ::Subsection::Closed:: *)
(*Package and Public Functions*)

Quiet[ClearAll["Unscrambling`*", "Unscrambling`Private`*"]];

BeginPackage["Unscrambling`", {"ConstructiveDiagrams`"}];

ComplicateAmplitude::usage = "ComplicateAmplitude[{massRules, amplitude}, nSteps] or ComplicateAmplitude[{massRules, amplitude}, nSteps, seed] applies legal identities. Returns MassRules, Original, Expression and Steps. The string option RecordSteps -> True also includes Trajectory; RandomSeed, OnShellChannels and MomentumConservation are supported.";
TrainUnscrambleNet::usage = "TrainUnscrambleNet[{amp1, amp2, ...}, opts] trains a shared-state candidate scorer. Kernels selects serial execution (1), owned local workers (n or Automatic), or an explicit connected worker list. Data generation and validation run in parallel; neural optimization remains on the coordinator. String options include DataCacheDirectory, WorkerRoot, WorkerThreads, TrainingBatchSize, JobTimeLimit, ValidationTimeLimit, ModelDirectory, OnShellChannels and MomentumConservation. Saves the current unscramble.wlnet and unscramble.m files.";
UnscrambleSpinorAmplitudes::usage = "UnscrambleSpinorAmplitudes[{massRules, amplitude}] takes one diagram amplitude, as returned by diagramAmplitudes, and rewrites it with the trained net until the expression stops simplifying. diagramAmplitude's expression is the second element; the mass rules are the first.";
SpinorExpressionComplexity::usage = "SpinorExpressionComplexity[expr] is the score used to decide whether an expression got simpler. Fewer terms, momentum insertions, and momentum products give a lower score.";
SchoutenRewrite::usage = "SchoutenRewrite[c1, c2, {m, n}] rewrites a product of spinor chains using Decay Rates, Appendix B, Eq. (B11). m and n count momenta before the split in each chain. Invalid chirality or splits return $Failed.";
SchoutenCandidates::usage = "SchoutenCandidates[expr] returns exact generalized Schouten edits with Subtract, Insert, Chains and Splits. Chain reversal is canonicalized for cancellation. No mass or on-shell assumption is used.";
UnscrambleTrace::usage = "UnscrambleTrace[{massRules, amplitude}] runs the trained policy and returns its guesses, changed expressions, complexity scores and accepted checkpoints, together with Result.";
UnscrambleCandidates::usage = "UnscrambleCandidates[{massRules, amplitude}] lists legal concrete edits and Stop for the canonicalized expression. Supports arbitrary integer leg labels. Options: OnShellChannels (default Automatic, inferred from internal mass rules), MomentumConservation (default True), both string keys.";
UnscrambleEncoding::usage = "UnscrambleEncoding[{massRules, amplitude}, candidate] returns an association with State and Candidate UTF-8 byte-ID sequences for the candidate policy. Both are complete; no truncation or learned vocabulary cutoff is applied.";

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
  If[validChainQ[c] && !OrderedQ[{c, rev}], (-1)^(Length[c] - 1)*rev, c]
];
canonicalChains[expr_] := Expand[expr /. c_SpinorChain :> canonicalChain[c]];

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

internalPairs[massRules_List] := Select[Cases[massRules,
  HoldPattern[Mass[Multiparticle[a_Integer, b_Integer]] -> m_] :> {{a, b}, m}],
  $onShellChannels === Automatic || MemberQ[$onShellChannels, Sort[#[[1]]]] &];

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

lineMass[massRules_List, i_Integer, j_Integer] := Module[{found},
  found = SelectFirst[internalPairs[massRules], Sort[#[[1]]] === Sort[{i, j}] &];
  If[MissingQ[found], None, found[[2]]]
];

(* 2 p_i·p_j = (p_i+p_j)^2 - m_i^2 - m_j^2, written Mom[Multiparticle[i,j]]^2
   so Expand does not turn the square into ordinary products. An internal
   line also has (p_i+p_j)^2 = M^2. *)

(* ::Subsection::Closed:: *)
(*Momentum-Square Identities*)

momentumSquareMoves[expr_, massRules_List] := Module[{moves = {}, groups, keys, exts, pair, g},
  groups = groupsOf[expr];
  keys = Keys[groups];
  exts = externalIndices[massRules];
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
            If[FreeQ[sk, Mom[Multiparticle[__]]] && FreeQ[sk, dot],
              q = shellQuotient[coef, shell2];
              If[q =!= None && Expand[q] =!= 0 && ! FreeQ[coef, bigM],
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
    {pair, Subsets[exts, {2}]}
  ];
  moves
];


ComplicateAmplitude[pair:{_List, _}, nSteps_Integer, seed_Integer] :=
  ComplicateAmplitude[pair, nSteps, RandomSeed -> seed];

samePoly[a_, b_] := expandAmplitude[a - b] === 0;



(* ::Subsection::Closed:: *)
(*Kinematic Identities*)

kinematicMoves[expr_, massRules_List] := Module[
  {moves = {}, groups, keys, exts, buckets, r, g, n},
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
    Module[{sk = keys[[g]], nMoms, count, which, template},
      nMoms = Length[Cases[sk, Mom[_Integer], Infinity]];
      Do[
        count = 0; which = 0;
        template = sk /. Mom[ii_Integer] :> (
          count++;
          If[count === n,
            which = ii; Mom[0],
            Mom[ii]
          ]
        );
        If[MemberQ[exts, which],
          buckets[template] = Append[Lookup[buckets, template, {}], {which, groups[sk], sk}]
        ],
        {n, nMoms}
      ]
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
            skJ = template /. Mom[0] -> Mom[missing[[1]]];
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
  DeleteDuplicatesBy[moves, {#["Name"], expandAmplitude[#["Subtract"]], expandAmplitude[#["Insert"]]} &]
];

(* One shared scorer evaluates every legal edit.
   UTF-8 serialization preserves the complete ordered expression tree. *)

(* ::Subsection::Closed:: *)
(*Policy Settings*)

$modelNet = None;
$loadedModelDirectory = None;
$onShellChannels = Automatic;
$useMomentumConservation = True;
$maxStagnantSteps = 3;
$timeLimit = 60;
$trainingBatchSize = 1;
$trainingTargetDevice = "CPU";

normalizeOnShellChannels[Automatic] := Automatic;
normalizeOnShellChannels[channels_List] := Sort /@ channels;

Options[UnscrambleCandidates] = {"OnShellChannels" -> Automatic, "MomentumConservation" -> True};
Options[ComplicateAmplitude] = Join[{RandomSeed -> Automatic, "RecordSteps" -> False}, Options[UnscrambleCandidates]];
Options[UnscrambleSpinorAmplitudes] = Join[
  {"MaxSteps" -> 12, "Attempts" -> 5, "MaxStagnantSteps" -> 3,
    "TimeLimit" -> 60, "ModelDirectory" -> Automatic}, Options[UnscrambleCandidates]];
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
          mass = If[IntegerQ[leg], massValue[rules, leg],
            If[MatchQ[leg, Multiparticle[_Integer, _Integer]], lineMass[rules, Sequence @@ leg], None]];
          square = If[MissingQ[mass] || mass === None, Mom[leg]^2, mass^2];
          rhs = square short;
          AppendTo[moves, <|"Name" -> "ChainSquare", "Subtract" -> mons[[t]],
            "Insert" -> Expand[mons[[t]]/c*rhs], "Site" -> {t, k, j}, "Direction" -> "Contract"|>],
          swapped = ReplacePart[c, {j + 1 -> ps[[j + 1]], j + 2 -> ps[[j]]}];
          rhs = 2 orderedDot[ps[[j, 1]], ps[[j + 1, 1]]] short - swapped;
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

applyCandidate[expr_, cand_Association] :=
  If[cand["Name"] === "Stop", expr, canonicalChains[expr - cand["Subtract"] + cand["Insert"]]];

legalMoves[expr_, massRules_List] := Module[{legacy, moves},
  legacy = Select[kinematicMoves[expr, massRules],
    MemberQ[If[TrueQ[$useMomentumConservation],
      {"MomentumConservation", "MomentumSquare", "OnShell"},
      {"MomentumSquare", "OnShell"}], #["Name"]] &];
  moves = Join[chainMoves[expr, massRules], interiorChainMoves[expr, massRules], SchoutenCandidates[expr], legacy];
  moves = Select[moves, !samePoly[applyCandidate[expr, #], expr] &];
  Append[DeleteDuplicatesBy[moves, {#["Name"], #["Subtract"], #["Insert"]} &], stopMove[]]
];

UnscrambleCandidates[{massRules_List, amplitude_}, OptionsPattern[]] :=
  Block[{$onShellChannels = normalizeOnShellChannels[OptionValue["OnShellChannels"]],
      $useMomentumConservation = OptionValue["MomentumConservation"]},
    legalMoves[canonicalChains[amplitude /. massRules], massRules]
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
    Map[Lookup[ids, #, #] &, Sort /@ (First /@ internalPairs[rules]), {2}]}}];
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
    candidateFeature[canonicalChains[amp /. rules], rules, cand]
  ];


(* ::Subsection::Closed:: *)
(*Neural Network*)

makeEncoder[classes_, width_:"Varying"] := NetChain[{
  EmbeddingLayer[24, classes], GatedRecurrentLayer[64], SequenceLastLayer[]
}, "Input" -> {width}];

makePolicy[width_:"Varying"] := Module[{head},
  head = NetGraph[<|"Join" -> CatenateLayer[],
    "Score" -> NetChain[{LinearLayer[32], Ramp, LinearLayer[1], LogisticSigmoid}]|>,
    {{NetPort["State"], NetPort["Candidate"]} -> "Join" -> "Score"},
    "State" -> 64, "Candidate" -> 64];
  NetGraph[<|"StateEncoder" -> makeEncoder[256],
    "CandidateEncoder" -> NetMapOperator[makeEncoder[257, width]],
    "Scorer" -> NetMapThreadOperator[head, <|"Candidate" -> 1|>]|>,
    {NetPort["State"] -> "StateEncoder" -> NetPort["Scorer", "State"],
      NetPort["Candidates"] -> "CandidateEncoder" -> NetPort["Scorer", "Candidate"]}]
];


(* ::Subsection::Closed:: *)
(*Current Model Files*)

modelPaths[dir_] := <|
  "Net" -> FileNameJoin[{dir, "unscramble.wlnet"}],
  "Metadata" -> FileNameJoin[{dir, "unscramble.m"}]
|>;
resolveModelDirectory[Automatic] := $modelDirectory;
resolveModelDirectory[dir_String] := ExpandFileName[dir];

loadUnscrambleModel[dir_] := Module[{paths, meta, net},
  If[$loadedModelDirectory === dir && MatchQ[$modelNet, _NetChain | _NetGraph], Return[True]];
  paths = modelPaths[dir];
  If[!AllTrue[Values[paths], FileExistsQ], Return[False]];
  meta = Get[paths["Metadata"]];
  If[!AssociationQ[meta] ||
      Lookup[meta, "Encoding", None] =!= "SplitFullFormUTF8" ||
      Lookup[meta, "Architecture", None] =!= "SharedStateGRU", Return[False]];
  net = Import[paths["Net"]];
  If[!MatchQ[net, _NetChain | _NetGraph], Return[False]];
  $modelNet = net; $loadedModelDirectory = dir;
  True
];


(* ::Subsection::Closed:: *)
(*Candidate Scoring*)

scoreCandidates[expr_, rules_, cands_] := Module[{scores},
  scores = Flatten[$modelNet[groupFeatures[expr, rules, cands], TargetDevice -> "CPU"]];
  If[Length[scores] =!= Length[cands] || !VectorQ[scores, NumericQ], $Failed, scores]
];

selectCandidate[expr_, rules_, sample_] := Module[{cands, scores, probs, idx},
  cands = legalMoves[expr, rules];
  scores = scoreCandidates[expr, rules, cands];
  If[scores === $Failed, Return[$Failed]];
  probs = Exp[(scores - Max[scores])/0.2];
  idx = If[TrueQ[sample], RandomChoice[probs -> Range[Length[cands]]], First[Ordering[scores, -1]]];
  <|"Candidate" -> cands[[idx]], "Index" -> idx, "Count" -> Length[cands], "Score" -> scores[[idx]]|>
];


(* ::Subsection::Closed:: *)
(*Complexity and Scrambling*)

SpinorExpressionComplexity[expr_] := expressionComplexity[expr];
expressionComplexity[expr_] := Module[{chains},
  chains = Flatten[chainsIn /@ monomialsOf[expr]];
  Length[monomialsOf[expr]] + 4*Total[Max[Length[#] - 2, 0] & /@ chains] +
    4*Count[{expr}, _MomProd, Infinity] +
    2*Count[{expr}, Power[Mom[Multiparticle[__]], 2], Infinity]
];

(* Scrambling and inference share exactly the same candidate vocabulary. *)
scramblePair[{rules_List, amp_}, n_, seed_, record_] := Module[
  {original = canonicalChains[amp /. rules], expr, moves, pick, next, history = {}},
  expr = original;
  BlockRandom[
    If[IntegerQ[seed], SeedRandom[seed]];
    Do[
      moves = Select[legalMoves[expr, rules], #["Name"] =!= "Stop" &];
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

trainingRows[expr_, rules_, target_] := Module[{cands, labels, canonicalTarget = canonicalChains[target]},
  cands = legalMoves[expr, rules];
  labels = Boole[samePoly[applyCandidate[expr, #], canonicalTarget]] & /@ cands;
  If[!MemberQ[labels, 1], Return[{}]];
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
    "WorkerThreads" -> 1, "TrainingBatchSize" -> 1,
    "JobTimeLimit" -> 300, "ValidationTimeLimit" -> 60}, Options[UnscrambleCandidates]];
TrainUnscrambleNet::kernels = "Kernels must be a positive integer, Automatic, or a nonempty list of connected kernel objects. Worker startup or initialization failed.";
TrainUnscrambleNet::nodata = "No reversible training examples were generated. No model was saved.";
TrainUnscrambleNet::options = "Steps, HoldOut must be nonnegative integers; Scrambles, MaxTrainingRounds, EpisodeLength must be positive integers.";
TrainUnscrambleNet::save = "Could not save the candidate model in `1`.";

makeTrainingPolicy[width_:"Varying"] := NetGraph[<|"Policy" -> makePolicy[width],
  "WeightedPrediction" -> ThreadingLayer[Times], "WeightedTarget" -> ThreadingLayer[Times],
  "Loss" -> MeanSquaredLossLayer[]|>, {
  NetPort["State"] -> NetPort["Policy", "State"],
  NetPort["Candidates"] -> NetPort["Policy", "Candidates"],
  {"Policy", NetPort["Weights"]} -> "WeightedPrediction" -> NetPort["Loss", "Input"],
  {NetPort["Target"], NetPort["Weights"]} -> "WeightedTarget" -> NetPort["Loss", "Target"]}];

prepareTrainingBatch[rows_] := Module[{width, padded, net},
  width = Max[Length /@ Flatten[Lookup[rows, "Candidates"], 1]];
  padded = (Join[#, <|"Candidates" -> (PadLeft[#, width, 257] & /@ #["Candidates"])|>] &) /@ rows;
  net = makeTrainingPolicy[width];
  <|"Net" -> net, "Rows" -> padded, "Width" -> width|>
];

variableWidthPolicy[net_] := Module[{paths = Information[net, "ArraysPositionList"]},
  NetReplacePart[makePolicy[], Map[# -> NetExtract[net, #] &, paths]]
];

trainCandidateNetwork[rows_, rounds_] := Module[{trained, batch = prepareTrainingBatch[rows]},
  (* NetTrain permits only the first input dimension to vary. Fix the byte
     width to this dataset's longest edit, padding only, never truncating. *)
  trained = NetTrain[NetInitialize[batch["Net"], RandomSeeding -> 1], batch["Rows"],
    MaxTrainingRounds -> rounds, BatchSize -> $trainingBatchSize, TargetDevice -> $trainingTargetDevice,
    TrainingProgressReporting -> None, Method -> {"ADAM", "LearningRate" -> 0.001}];
  If[MatchQ[trained, _NetGraph],
    variableWidthPolicy[NetExtract[trained, "Policy"]], $Failed]
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
makeValidationJobs[amplitudes_, steps_, count_, hold_, episode_] := MapIndexed[Join[#1, <|"Order" -> First[#2]|>] &,
  Flatten[MapIndexed[Function[{pair, index},
    Table[<|"Mode" -> "Validate", "Pair" -> pair, "AmplitudeIndex" -> First[index],
      "Seed" -> count + seed, "Steps" -> steps, "Episode" -> episode|>, {seed, hold}]], amplitudes], 1]];

generationJob[job_] := Block[{
  $onShellChannels = Replace[$onShellChannels, channels_List :>
    Map[Lookup[Lookup[job, "LegPermutation", <||>], #, #] &, channels, {2}]]},
 Module[{pair = job["Pair"], scr, batches, targets, skipped},
  If[job["Mode"] === "Original",
    Return[<|"Rows" -> trainingRows[canonicalChains[pair[[2]] /. pair[[1]]], pair[[1]], pair[[2]] /. pair[[1]]],
      "Targets" -> 0, "Skipped" -> 0|>]];
  scr = scramblePair[pair, job["Steps"], job["Seed"], True];
  (* Collect once instead of repeatedly copying the growing dataset with Join. *)
  batches = trainingRows[#["After"], pair[[1]], #["Before"]] & /@ Reverse[scr["Trajectory"]];
  targets = Length[batches]; skipped = Count[batches, {}];
  (* Teach Stop in the same numbering as the reverse trajectory. *)
  <|"Rows" -> Join[Flatten[batches, 1],
      trainingRows[canonicalChains[pair[[2]] /. pair[[1]]], pair[[1]], pair[[2]] /. pair[[1]]]],
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
    $timeLimit = settings["ValidationTimeLimit"]},
  Module[{start = AbsoluteTime[], cpu = TimeUsed[], result},
    result = TimeConstrained[If[job["Mode"] === "Validate", validationJob[job], generationJob[job]],
      settings["JobTimeLimit"], $Failed];
    <|"Order" -> job["Order"], "Success" -> AssociationQ[result], "Result" -> result,
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
installValidationModel[workers_, net_, directory_] := With[{model = net, dir = directory},
  ParallelEvaluate[$modelNet = model; $loadedModelDirectory = dir; True, workers, DistributedContexts -> None] ===
    ConstantArray[True, Length[workers]]
];

TrainUnscrambleNet[amplitudes_List, OptionsPattern[]] := Block[
  {$onShellChannels = normalizeOnShellChannels[OptionValue["OnShellChannels"]],
    $useMomentumConservation = OptionValue["MomentumConservation"], $trainingBatchSize = OptionValue["TrainingBatchSize"],
    $trainingTargetDevice = OptionValue[TargetDevice]},
  Module[{steps = OptionValue[Steps], count = OptionValue[Scrambles], hold = OptionValue[HoldOut],
      rounds = OptionValue[MaxTrainingRounds], episode = OptionValue[EpisodeLength], root = DirectoryName[$modelDirectory],
      workerRoot, cache = OptionValue["DataCacheDirectory"], settings, fingerprint, jobs,
      dir = resolveModelDirectory[OptionValue["ModelDirectory"]], workerThreads = OptionValue["WorkerThreads"]},
    If[!AllTrue[{steps, hold}, IntegerQ[#] && # >= 0 &] ||
        !AllTrue[{count, rounds, episode}, IntegerQ[#] && # > 0 &], Message[TrainUnscrambleNet::options]; Return[$Failed]];
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
      "JobTimeLimit" -> OptionValue["JobTimeLimit"], "ValidationTimeLimit" -> OptionValue["ValidationTimeLimit"]|>;
    fingerprint = pipelineFingerprint[root]; jobs = makeTrainingJobs[amplitudes, steps, count];
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
    validationJobs, candidateRows, work},
  trainingStage["Data generation"];
  {dataSeconds, generated} = AbsoluteTiming[cachedTrainingJobs[jobs, workers, settings, cache, fingerprint]];
  If[!validJobResultsQ[generated], Message[TrainUnscrambleNet::job, "data generation"]; Return[$Failed]];
  rows = Flatten[Lookup[Lookup[generated, "Result"], "Rows"], 1];
  targets = Total[Lookup[Lookup[generated, "Result"], "Targets"]];
  skipped = Total[Lookup[Lookup[generated, "Result"], "Skipped"]];
  generated = KeyDrop[#, "Result"] & /@ generated;
  If[rows === {} || targets === skipped, Message[TrainUnscrambleNet::nodata]; Return[$Failed]];
  trainingStage["NN training"];
  trainingCPU = TimeUsed[];
  {trainingSeconds, net} = AbsoluteTiming[trainCandidateNetwork[rows, rounds]];
  trainingCPU = TimeUsed[] - trainingCPU;
  If[!MatchQ[net, _NetChain | _NetGraph], Return[$Failed]];
  $modelNet = net; $loadedModelDirectory = dir;
  If[!DirectoryQ[dir], CreateDirectory[dir, CreateIntermediateDirectories -> True]];
  paths = modelPaths[dir];
  If[Export[paths["Net"], net] === $Failed, Message[TrainUnscrambleNet::save, dir]; Return[$Failed]];
  Put[<|"Encoding" -> "SplitFullFormUTF8", "Architecture" -> "SharedStateGRU",
    "OnShellChannels" -> $onShellChannels, "MomentumConservation" -> $useMomentumConservation|>, paths["Metadata"]];
  validationJobs = makeValidationJobs[amplitudes, steps, count, hold, episode];
  Put[<|"Status" -> "Model saved; validation pending", "ModelPath" -> paths["Net"],
    "TrainingSeconds" -> trainingSeconds, "TrainingCPUSeconds" -> trainingCPU,
    "TrainingStates" -> Length[rows], "ValidationCases" -> Length[validationJobs]|>,
    FileNameJoin[{dir, "training-checkpoint.m"}]];
  Print["Model saved: ", paths["Net"]];
  trainingStage["Validation"];
  {validationSeconds, validation} = AbsoluteTiming[
    If[validationJobs =!= {} && !installValidationModel[workers, net, dir], $Failed,
      mapTrainingJobs[validationJobs, workers, settings]]];
  If[!validJobResultsQ[validation], Message[TrainUnscrambleNet::job, "validation (model already saved)"]; Return[$Failed]];
  Put[validation, FileNameJoin[{dir, "validation-results.m"}]];
  trainingStage["Complete"];
  candidateRows = Total[Length /@ Lookup[rows, "Target"]];
  work = trainingWorkEstimate[rows];
  <|"Kernels" -> Max[1, Length[workers]], "Starts" -> Length[amplitudes]*count,
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

UnscrambleSpinorAmplitudes::nomodel = "No compatible unscramble.wlnet and unscramble.m found in `1`. Train with TrainUnscrambleNet.";
UnscrambleSpinorAmplitudes::scores = "The model did not return one numeric score per candidate.";
UnscrambleSpinorAmplitudes::options = "MaxSteps must be a nonnegative integer; Attempts and MaxStagnantSteps must be positive integers; TimeLimit must be a positive number of seconds.";

runUnscramble[{rules_List, amplitude_}, record_, maxSteps_, attempts_, dir_] := Module[
  {expr = canonicalChains[amplitude /. rules], checkpoint, trial, before, selection, move, next,
    history = {}, checkpoints = {}, accepted, failed = False, attempt,
    stagnant = 0, reason = "StepLimit", loaded = False, bestScore, nextScore},
  checkpoint = expr; bestScore = expressionComplexity[checkpoint];
  TimeConstrained[
  loaded = loadUnscrambleModel[dir];
  If[!loaded, Message[UnscrambleSpinorAmplitudes::nomodel, dir];
    Return[<|"Result" -> {rules, amplitude}, "Trace" -> {}, "Checkpoints" -> {}, "ModelLoaded" -> False|>]];
  Do[
    before = checkpoint; trial = checkpoint; reason = "StepLimit";
    Do[
      selection = selectCandidate[trial, rules, attempt > 1];
      If[selection === $Failed, Message[UnscrambleSpinorAmplitudes::scores]; failed = True; reason = "ScoringFailed"; Break[]];
      move = selection["Candidate"]; next = applyCandidate[trial, move];
      nextScore = expressionComplexity[next];
      If[nextScore < bestScore,
        checkpoint = next; bestScore = nextScore; stagnant = 0,
        stagnant++];
      If[TrueQ[record], AppendTo[history, Join[selection, <|"Attempt" -> attempt,
        "Before" -> trial, "After" -> next, "Changed" -> !samePoly[trial, next],
        "ComplexityBefore" -> expressionComplexity[trial], "ComplexityAfter" -> expressionComplexity[next]|>]]];
      trial = next;
      If[stagnant >= $maxStagnantSteps, reason = "Stagnation"; Break[]];
      If[move["Name"] === "Stop", reason = "Stop"; Break[]], {maxSteps}
    ];
    accepted = expressionComplexity[checkpoint] < expressionComplexity[before];
    If[TrueQ[record], AppendTo[checkpoints, <|"Attempt" -> attempt, "Before" -> before,
      "Trial" -> trial, "Accepted" -> accepted, "After" -> checkpoint|>]];
    If[failed || stagnant >= $maxStagnantSteps, Break[]], {attempt, attempts}
  ], $timeLimit, reason = "TimeLimit"];
  <|"Result" -> {rules, checkpoint}, "Trace" -> history, "Checkpoints" -> checkpoints,
    "ModelLoaded" -> loaded, "ScoringFailed" -> failed, "TerminationReason" -> reason,
    "StagnantSteps" -> stagnant|>
];

configuredRun[pair:{_List, _}, record_, OptionsPattern[UnscrambleTrace]] := Block[
  {$onShellChannels = normalizeOnShellChannels[OptionValue["OnShellChannels"]],
    $useMomentumConservation = OptionValue["MomentumConservation"],
    $maxStagnantSteps = OptionValue["MaxStagnantSteps"], $timeLimit = OptionValue["TimeLimit"]},
  If[!IntegerQ[OptionValue["MaxSteps"]] || OptionValue["MaxSteps"] < 0 ||
      !IntegerQ[OptionValue["Attempts"]] || OptionValue["Attempts"] < 1 ||
      !IntegerQ[$maxStagnantSteps] || $maxStagnantSteps < 1 ||
      !NumberQ[$timeLimit] || !TrueQ[$timeLimit > 0],
    Message[UnscrambleSpinorAmplitudes::options]; Return[$Failed]];
  runUnscramble[pair, record, OptionValue["MaxSteps"], OptionValue["Attempts"],
    resolveModelDirectory[OptionValue["ModelDirectory"]]]
];
UnscrambleTrace[pair:{_List, _}, opts:OptionsPattern[]] := configuredRun[pair, True, opts];
UnscrambleSpinorAmplitudes[pair:{_List, _}, opts:OptionsPattern[]] := Module[{result},
  result = configuredRun[pair, False, opts];
  If[AssociationQ[result], result["Result"], $Failed]
];


(* ::Subsection::Closed:: *)
(*End Package*)

End[];
EndPackage[];
