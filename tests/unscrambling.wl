(* Complicate a simplified amplitude by applying identities at random.

   Every stored expression has the mass rules already applied, so Mass[1]
   is Me (or 0, MW, MZ, ...) and never the head Mass. After each identity
   the whole expression is expanded. RecordSteps can retain intermediate
   expressions and exact edit records for supervised reverse trajectories.
*)

Quiet[ClearAll["Unscrambling`*", "Unscrambling`Private`*"]];

BeginPackage["Unscrambling`", {"ConstructiveDiagrams`"}];

ComplicateAmplitude::usage = "ComplicateAmplitude[{massRules, amplitude}, nSteps] or ComplicateAmplitude[{massRules, amplitude}, nSteps, seed] applies legal identities. Returns MassRules, Original, Expression and Steps. The string option RecordSteps -> True also includes Trajectory; RandomSeed, OnShellChannels and MomentumConservation are supported.";
TrainUnscrambleNet::usage = "TrainUnscrambleNet[{amp1, amp2, ...}, opts] trains a shared-state candidate scorer. Kernels selects serial execution (1), owned local workers (n or Automatic), or an explicit connected worker list. Data generation and validation run in parallel; neural optimization remains on the coordinator. String options include DataCacheDirectory, WorkerRoot, WorkerThreads, TrainingBatchSize, JobTimeLimit, ValidationTimeLimit, ModelDirectory, OnShellChannels and MomentumConservation. Saves version-3 files without replacing older networks.";
UnscrambleSpinorAmplitudes::usage = "UnscrambleSpinorAmplitudes[{massRules, amplitude}] takes one diagram amplitude, as returned by diagramAmplitudes, and rewrites it with the trained net until the expression stops simplifying. diagramAmplitude's expression is the second element; the mass rules are the first.";
SpinorExpressionComplexity::usage = "SpinorExpressionComplexity[expr] is the score used to decide whether an expression got simpler. Fewer terms, momentum insertions, and momentum products give a lower score.";
SchoutenRewrite::usage = "SchoutenRewrite[c1, c2, {m, n}] rewrites a product of spinor chains using Decay Rates, Appendix B, Eq. (B11). m and n count momenta before the split in each chain. Invalid chirality or splits return $Failed.";
SchoutenCandidates::usage = "SchoutenCandidates[expr] returns exact generalized Schouten edits with Subtract, Insert, Chains and Splits. Chain reversal is canonicalized for cancellation. No mass or on-shell assumption is used.";
UnscrambleTrace::usage = "UnscrambleTrace[{massRules, amplitude}] runs the trained policy and returns its guesses, changed expressions, complexity scores and accepted checkpoints, together with Result.";
UnscrambleCandidates::usage = "UnscrambleCandidates[{massRules, amplitude}] lists legal concrete edits and Stop for the canonicalized expression. Supports arbitrary integer leg labels. Options: OnShellChannels (default Automatic, inferred from internal mass rules), MomentumConservation (default True), both string keys.";
UnscrambleEncoding::usage = "UnscrambleEncoding[{massRules, amplitude}, candidate] returns an association with State and Candidate UTF-8 byte-ID sequences for the version-3 policy. Both are complete; no truncation or learned vocabulary cutoff is applied.";

Begin["`Private`"];

$modelDirectory = DirectoryName[$InputFileName];

Get[FileNameJoin[{$modelDirectory, "schouten.wl"}]];

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


kinematicMoves[expr_, massRules_List] := Module[
  {moves = {}, groups, keys, exts, buckets, r, g, n},
  groups = groupsOf[expr];
  keys = Keys[groups];
  exts = externalIndices[massRules];
  Do[
    Module[{legs = internalPairs[massRules][[r, 1]], m = internalPairs[massRules][[r, 2]],
        ma, mb, shell, dot, sk, coef, rest, subtract, insert},
      ma = massValue[massRules, legs[[1]]];
      mb = massValue[massRules, legs[[2]]];
      If[ma =!= Missing["Mass"] && mb =!= Missing["Mass"],
        shell = shellOf[m, ma, mb];
        dot = orderedDot @@ legs;
        Do[
          sk = keys[[g]]; coef = groups[sk];
          If[MemberQ[factorsOf[sk], dot] && ! samePoly[shell, dot],
            rest = sk/dot;
            subtract = expandAmplitude[coef*sk];
            insert = expandAmplitude[coef*shell*rest];
            If[! samePoly[subtract, insert],
              AppendTo[moves, <|"Name" -> "OnShell", "Subtract" -> subtract, "Insert" -> insert,
                "Legs" -> Sort[legs]|>]
            ]
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

Get[FileNameJoin[{$modelDirectory, "candidate-policy.wl"}]];

End[];
EndPackage[];
