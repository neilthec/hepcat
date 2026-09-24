(* Complicate a simplified amplitude by applying identities at random.

   Every stored expression has the mass rules already applied, so Mass[1]
   is Me (or 0, MW, MZ, ...) and never the head Mass. After each identity
   the whole expression is expanded. Only the final scrambled expression and
   the original expression are kept. The intermediate steps are not stored.
*)

Quiet[ClearAll["Unscrambling`*", "Unscrambling`Private`*"]];

BeginPackage["Unscrambling`", {"ConstructiveDiagrams`"}];

ComplicateAmplitude::usage = "ComplicateAmplitude[{massRules, amplitude}, nSteps] or ComplicateAmplitude[{massRules, amplitude}, nSteps, seed] applies nSteps identities. Mass rules are substituted first. The result is an association with MassRules, Original, Expression, and Steps. Original is the amplitude after mass substitution. Expression is the final scrambled amplitude. Intermediate steps are not stored.";
TrainUnscrambleNet::usage = "TrainUnscrambleNet[{amp1, amp2, ...}, opts] scrambles each {massRules, amplitude} and trains a small net to guess the next identity. Independent scrambles and episodes run in parallel across the available CPU kernels. Options: Steps, Scrambles, HoldOut, MaxTrainingRounds, EpisodeLength. The trained net is written next to this file.";
UnscrambleSpinorAmplitudes::usage = "UnscrambleSpinorAmplitudes[{massRules, amplitude}] takes one diagram amplitude, as returned by diagramAmplitudes, and rewrites it with the trained net until the expression stops simplifying. diagramAmplitude's expression is the second element; the mass rules are the first.";
SpinorExpressionComplexity::usage = "SpinorExpressionComplexity[expr] is the score used to decide whether an expression got simpler. Fewer terms, momentum insertions, and momentum products give a lower score.";

Begin["`Private`"];

$modelDirectory = DirectoryName[$InputFileName];

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

chainsIn[mon_] := Module[{factors = factorsOf[mon], out = {}},
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

groupsOf[expr_] := Module[{groups = <||>, mons, pair},
  mons = monomialsOf[expr];
  Do[
    pair = splitMon[mons[[i]]];
    groups[pair[[2]]] = Lookup[groups, pair[[2]], 0] + pair[[1]],
    {i, Length[mons]}
  ];
  groups
];

internalPairs[massRules_List] := Cases[massRules,
  HoldPattern[Mass[Multiparticle[a_Integer, b_Integer]] -> m_] :> {{a, b}, m}];

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
momentumSquareMoves[expr_, massRules_List] := Module[{moves = {}, groups, keys, exts},
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

candidateMoves[expr_, massRules_List] := Module[{moves = {}, mons, exts, groups, pairs},
  mons = monomialsOf[expr];
  exts = externalIndices[massRules];
  Do[
    Module[{chs = chainsIn[mons[[t]]]},
      Do[
        Module[{c1 = chs[[a]], c2 = chs[[b]], e1, e2},
          If[chainPairQ[c1] && chainPairQ[c2] && chainType[c1] === chainType[c2] &&
              chainKind[c1] === chainKind[c2],
            e1 = chainEnds[c1];
            e2 = chainEnds[c2];
            If[Length[DeleteDuplicates[Join[e1, e2]]] == 4,
              AppendTo[moves, <|"Kind" -> "Schouten", "Monomial" -> mons[[t]], "Chains" -> {c1, c2}|>]
            ]
          ]
        ],
        {a, Length[chs]}, {b, a + 1, Length[chs]}
      ];
      Do[
        Module[{sc = chs[[a]], br, ends, leg, m},
          If[chainPairQ[sc],
            AppendTo[moves, <|"Kind" -> "ReverseSpinor", "Monomial" -> mons[[t]], "Chain" -> sc|>];
            ends = chainEnds[sc];
            Do[
              leg = ends[[s]];
              m = massValue[massRules, leg];
              If[IntegerQ[leg] && m =!= 0 && m =!= Missing["Mass"],
                AppendTo[moves, <|"Kind" -> "Mass", "Monomial" -> mons[[t]],
                  "Chain" -> sc, "Leg" -> leg, "Mass" -> m|>]
              ],
              {s, 2}
            ]
          ];
          If[threeChainQ[sc],
            If[sc[[1, 2]] =!= sc[[3, 2]],
              AppendTo[moves, <|"Kind" -> "ReverseSpinor", "Monomial" -> mons[[t]], "Chain" -> sc|>]
            ];
            If[IntegerQ[sc[[2, 1]]] && MemberQ[exts, sc[[2, 1]]] && Length[exts] >= 2,
              AppendTo[moves, <|"Kind" -> "Momentum", "Monomial" -> mons[[t]],
                "Chain" -> sc, "Momentum" -> sc[[2, 1]]|>]
            ];
            Do[
              br = chs[[b]];
              If[chainPairQ[br] && chainKind[br] === chainKind[sc] && schoutenChainSum[br, sc] =!= $Failed,
                AppendTo[moves, <|"Kind" -> "Schouten", "Monomial" -> mons[[t]],
                  "Bracket" -> br, "Chain" -> sc|>]
              ],
              {b, Length[chs]}
            ]
          ]
        ],
        {a, Length[chs]}
      ]
    ],
    {t, Length[mons]}
  ];
  groups = groupsOf[expr];
  pairs = internalPairs[massRules];
  Do[
    Module[{legs = pairs[[r, 1]], m = pairs[[r, 2]], ma, mb, shell, keys, coef, q, dot},
      ma = massValue[massRules, legs[[1]]];
      mb = massValue[massRules, legs[[2]]];
      If[ma =!= Missing["Mass"] && mb =!= Missing["Mass"] && !(m === 0 && ma === 0 && mb === 0),
        shell = shellOf[m, ma, mb];
        keys = Keys[groups];
        Do[
          coef = groups[keys[[g]]];
          (* The internal mass has to actually appear, otherwise this is not
             the on-shell value of this line. A massless line uses the
             external masses instead. *)
          If[!(If[m === 0, FreeQ[coef, ma] && FreeQ[coef, mb], FreeQ[coef, m]]),
            q = shellQuotient[coef, shell];
            If[q =!= None && Expand[q] =!= 0,
              dot = orderedDot[legs[[1]], legs[[2]]];
              AppendTo[moves, <|"Kind" -> "OffShell",
                "Removed" -> expandAmplitude[coef*keys[[g]]],
                "Added" -> expandAmplitude[q*dot*keys[[g]]],
                "Legs" -> Sort[legs], "Mass" -> m|>]
            ]
          ],
          {g, Length[keys]}
        ]
      ]
    ],
    {r, Length[pairs]}
  ];
  Do[
    AppendTo[moves, <|"Kind" -> If[sq["Name"] === "OnShell", "OffShell", "MomentumSquare"],
      "Removed" -> sq["Subtract"], "Added" -> sq["Insert"], "Legs" -> sq["Legs"],
      "Mass" -> Lookup[sq, "Mass", None]|>],
    {sq, momentumSquareMoves[expr, massRules]}
  ];
  DeleteDuplicates[moves]
];

applyMove[move_Association, massRules_List] := Module[{mon, added},
  If[move["Kind"] === "OffShell" || move["Kind"] === "MomentumSquare",
    Return[{move["Removed"], move["Added"]}]
  ];
  mon = move["Monomial"];
  added = Switch[move["Kind"],
    "Schouten",
      If[KeyExistsQ[move, "Chains"],
        Module[{c1 = move["Chains"][[1]], c2 = move["Chains"][[2]], e1, e2},
          e1 = chainEnds[c1]; e2 = chainEnds[c2];
          expandAmplitude[mon/(c1*c2)*schoutenSum[chainKind[c1], chainType[c1], e1[[1]], e1[[2]], e2[[1]], e2[[2]]]]
        ],
        expandAmplitude[mon/(move["Bracket"]*move["Chain"])*schoutenChainSum[move["Bracket"], move["Chain"]]]
      ],
    "ReverseSpinor",
      Module[{sc = move["Chain"], flipped},
        flipped = If[chainPairQ[sc],
          -SpinorChain[sc[[2]], sc[[1]]],
          SpinorChain[sc[[3]], sc[[2]], sc[[1]]]
        ];
        expandAmplitude[mon/sc*flipped]
      ],
    "Mass",
      Module[{sc = move["Chain"], leg = move["Leg"], m = move["Mass"], ends, other, kind, tp, newChain, sign},
        ends = chainEnds[sc];
        other = If[ends[[1]] === leg, ends[[2]], ends[[1]]];
        kind = chainKind[sc];
        tp = chainType[sc];
        (* <ij> = [i|pi|j>/m_i, and <ij> = -[j|pj|i>/m_j. Square brackets match. *)
        sign = If[ends[[1]] === leg, 1, -1];
        newChain = If[tp === "Angle",
          three[kind, "Square", leg, leg, "Angle", other],
          three[kind, "Angle", leg, leg, "Square", other]
        ];
        expandAmplitude[mon/sc*sign*newChain/m]
      ],
    "Momentum",
      Module[{sc = move["Chain"], j = move["Momentum"], others},
        others = DeleteCases[externalIndices[massRules], j];
        expandAmplitude[mon/sc*(-Sum[ReplaceAll[sc, Mom[j] -> Mom[others[[r]]]], {r, Length[others]}])]
      ],
    _, mon
  ];
  {expandAmplitude[mon], added}
];

unscrambleOf[move_Association] := Switch[move["Kind"],
  "Mass", <|"Name" -> "Mass", "Leg" -> move["Leg"], "Mass" -> move["Mass"]|>,
  "Momentum", <|"Name" -> "MomentumConservation", "Momentum" -> move["Momentum"]|>,
  "Schouten", <|"Name" -> "Schouten"|>,
  "ReverseSpinor", <|"Name" -> "ReverseSpinor"|>,
  "OffShell", <|"Name" -> "OnShell", "Legs" -> move["Legs"], "Mass" -> move["Mass"]|>,
  "MomentumSquare", <|"Name" -> "MomentumSquare", "Legs" -> move["Legs"]|>,
  _, <|"Name" -> move["Kind"]|>
];

ComplicateAmplitude[{massRules_List, amplitude_}, nSteps_Integer: 5, OptionsPattern[]] :=
  Module[{original, expr, moves, kinds, pool, pick, removed, added, seed, found, nDone = 0},
    seed = OptionValue[RandomSeed];
    If[IntegerQ[seed], SeedRandom[seed]];
    original = expandAmplitude[amplitude /. massRules];
    expr = original;
    Do[
      moves = candidateMoves[expr, massRules];
      If[moves === {}, Break[]];
      (* One kind at a time, in random order, so a long list of mass
         insertions cannot hide Schouten, reversal, or on-shell. *)
      kinds = RandomSample[DeleteDuplicates[moves[[All, "Kind"]]]];
      found = False;
      Do[
        pool = Select[Select[moves, #["Kind"] === kinds[[kk]] &],
          Module[{parts = applyMove[#, massRules]},
            expandAmplitude[parts[[2]] - parts[[1]]] =!= 0
          ] &];
        If[pool =!= {},
          pick = RandomChoice[pool];
          {removed, added} = applyMove[pick, massRules];
          found = True;
          Break[]
        ],
        {kk, Length[kinds]}
      ];
      If[! found, Break[]];
      expr = expandAmplitude[(expr - removed + added) /. massRules];
      nDone++;
      removed =.; added =.; pick =.; moves =.; pool =.;
      ,
      {nSteps}
    ];
    <|"MassRules" -> massRules, "Original" -> original, "Expression" -> expr, "Steps" -> nDone|>
  ];

Options[ComplicateAmplitude] = {RandomSeed -> Automatic};

ComplicateAmplitude[pair:{_List, _}, nSteps_Integer, seed_Integer] :=
  ComplicateAmplitude[pair, nSteps, RandomSeed -> seed];

samePoly[a_, b_] := expandAmplitude[a - b] === 0;

spinorFactors[sk_] := Select[factorsOf[sk], MatchQ[#, _SpinorChain] &];
otherFactors[sk_] := Times @@ Select[factorsOf[sk], ! MatchQ[#, _SpinorChain] &];

(* <ij><kl> = <ik><jl> - <il><jk>. Both bracket orders are returned: the
   sum does not record which written order was expanded. *)
basicSchoutenRestores[skP_, skN_] := Module[{bP, bN, oP, oN, found = {}},
  bP = Select[spinorFactors[skP], chainPairQ];
  bN = Select[spinorFactors[skN], chainPairQ];
  If[Length[bP] =!= 2 || Length[bN] =!= 2, Return[{}]];
  If[Count[spinorFactors[skP], _?threeChainQ] =!= 0 || Count[spinorFactors[skN], _?threeChainQ] =!= 0,
    Return[{}]];
  oP = otherFactors[skP]; oN = otherFactors[skN];
  If[oP =!= oN, Return[{}]];
  If[! (chainType[bP[[1]]] === chainType[bP[[2]]] === chainType[bN[[1]]] === chainType[bN[[2]]]),
    Return[{}]];
  Do[
    Module[{B1 = order[[1]], B2 = order[[2]], i, k, j, l, kind, tp, need, restored},
      {i, k} = chainEnds[B1]; {j, l} = chainEnds[B2];
      kind = chainKind[B1]; tp = chainType[B1];
      need = Sort[{bracket[kind, tp, i, l], bracket[kind, tp, j, k]}];
      If[need === Sort[bN],
        restored = oP*bracket[kind, tp, i, j]*bracket[kind, tp, k, l];
        AppendTo[found, restored]
      ]
    ],
    {order, {bP, Reverse[bP]}}
  ];
  DeleteDuplicates[found]
];

(* Bracket times a one-momentum chain, undone back to one product. *)
chainSchoutenRestore[skP_, skN_] := Module[
  {brP, chP, brN, chN, oP, oN, kind, p, dP, dN, cP, cN, aP, aN},
  If[Count[spinorFactors[skP], _?chainPairQ] =!= 1 || Count[spinorFactors[skP], _?threeChainQ] =!= 1,
    Return[$Failed]];
  If[Count[spinorFactors[skN], _?chainPairQ] =!= 1 || Count[spinorFactors[skN], _?threeChainQ] =!= 1,
    Return[$Failed]];
  brP = SelectFirst[spinorFactors[skP], chainPairQ];
  chP = SelectFirst[spinorFactors[skP], threeChainQ];
  brN = SelectFirst[spinorFactors[skN], chainPairQ];
  chN = SelectFirst[spinorFactors[skN], threeChainQ];
  oP = otherFactors[skP]; oN = otherFactors[skN];
  If[oP =!= oN || chP[[2]] =!= chN[[2]], Return[$Failed]];
  If[chainKind[brP] =!= chainKind[chP] || chainKind[brN] =!= chainKind[chN], Return[$Failed]];
  kind = chainKind[brP];
  p = chP[[2, 1]];
  {cP, aP} = {chP[[1, 3]], chP[[3, 3]]};
  {cN, aN} = {chN[[1, 3]], chN[[3, 3]]};
  dP = chainEnds[brP]; dN = chainEnds[brN];
  Which[
    (* <d b>[c|p|a> - <d a>[c|p|b> = <a b>[c|p|d> *)
    chainType[brP] === "Angle" && chainType[brN] === "Angle" &&
        chP[[1, 2]] === "Square" && chP[[3, 2]] === "Angle" &&
        chN[[1, 2]] === "Square" && chN[[3, 2]] === "Angle" &&
        cP === cN && dP[[1]] === dN[[1]] && dP[[2]] === aN && dN[[2]] === aP,
      oP*bracket[kind, "Angle", aP, aN]*three[kind, "Square", cP, p, "Angle", dP[[1]]],
    (* [c b][a|p|d> - [c a][b|p|d> = [a b][c|p|d> *)
    chainType[brP] === "Square" && chainType[brN] === "Square" &&
        chP[[1, 2]] === "Square" && chP[[3, 2]] === "Angle" &&
        chN[[1, 2]] === "Square" && chN[[3, 2]] === "Angle" &&
        aP === aN && dP[[1]] === dN[[1]] && dP[[2]] === cN && dN[[2]] === cP,
      oP*bracket[kind, "Square", cP, cN]*three[kind, "Square", dP[[1]], p, "Angle", aP],
    (* <c b><a|p|d] - <c a><b|p|d] = <a b><c|p|d] *)
    chainType[brP] === "Angle" && chainType[brN] === "Angle" &&
        chP[[1, 2]] === "Angle" && chP[[3, 2]] === "Square" &&
        chN[[1, 2]] === "Angle" && chN[[3, 2]] === "Square" &&
        aP === aN && dP[[1]] === dN[[1]] && dP[[2]] === cN && dN[[2]] === cP,
      oP*bracket[kind, "Angle", cP, cN]*three[kind, "Angle", dP[[1]], p, "Square", aP],
    (* [d b]<c|p|a] - [d a]<c|p|b] = [a b]<c|p|d] *)
    chainType[brP] === "Square" && chainType[brN] === "Square" &&
        chP[[1, 2]] === "Angle" && chP[[3, 2]] === "Square" &&
        chN[[1, 2]] === "Angle" && chN[[3, 2]] === "Square" &&
        cP === cN && dP[[1]] === dN[[1]] && dP[[2]] === aN && dN[[2]] === aP,
      oP*bracket[kind, "Square", aP, aN]*three[kind, "Angle", cP, p, "Square", dP[[1]]],
    True, $Failed
  ]
];

schoutenRestores[skP_, skN_] := Module[{chain},
  chain = chainSchoutenRestore[skP, skN];
  Join[If[chain === $Failed, {}, {chain}], basicSchoutenRestores[skP, skN]]
];

applyCandidate[expr_, cand_Association] :=
  expandAmplitude[expr - cand["Subtract"] + cand["Insert"]];

inverseMoves[expr_, massRules_List] := Module[
  {moves = {}, mons, groups, keys, exts, buckets, i, j},
  mons = monomialsOf[expr];
  Do[
    Module[{chs = chainsIn[mons[[t]]]},
      Do[
        Module[{sc = chs[[a]], flipped, insert, leg, other, m, kind, tp},
          If[chainPairQ[sc] || (threeChainQ[sc] && sc[[1, 2]] =!= sc[[3, 2]]),
            flipped = If[chainPairQ[sc],
              -SpinorChain[sc[[2]], sc[[1]]],
              SpinorChain[sc[[3]], sc[[2]], sc[[1]]]
            ];
            insert = expandAmplitude[mons[[t]]/sc*flipped];
            If[! samePoly[mons[[t]], insert],
              AppendTo[moves, <|"Name" -> "ReverseSpinor", "Subtract" -> expandAmplitude[mons[[t]]],
                "Insert" -> insert|>]
            ]
          ];
          If[MatchQ[sc, SpinorChain[Spinor[_, _, ii_], Mom[ii_], Spinor[_, _, _]]] &&
              sc[[1, 2]] =!= sc[[3, 2]],
            leg = sc[[1, 3]]; other = sc[[3, 3]];
            m = massValue[massRules, leg];
            If[m =!= 0 && m =!= Missing["Mass"] && MemberQ[factorsOf[mons[[t]]], sc],
              kind = chainKind[sc];
              tp = If[sc[[1, 2]] === "Angle", "Square", "Angle"];
              insert = expandAmplitude[mons[[t]]*m*bracket[kind, tp, leg, other]/sc];
              If[! samePoly[mons[[t]], insert],
                AppendTo[moves, <|"Name" -> "Mass", "Subtract" -> expandAmplitude[mons[[t]]],
                  "Insert" -> insert, "Leg" -> leg|>]
              ];
              insert = expandAmplitude[-mons[[t]]*m*bracket[kind, tp, other, leg]/sc];
              If[! samePoly[mons[[t]], insert],
                AppendTo[moves, <|"Name" -> "Mass", "Subtract" -> expandAmplitude[mons[[t]]],
                  "Insert" -> insert, "Leg" -> leg|>]
              ]
            ]
          ]
        ],
        {a, Length[chs]}
      ]
    ],
    {t, Length[mons]}
  ];
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
  Do[
    If[samePoly[groups[keys[[i]]] + groups[keys[[j]]], 0],
      Module[{coefP, coefN, subtract, insert, restored},
        Do[
          coefP = groups[pair[[1]]]; coefN = groups[pair[[2]]];
          restored = schoutenRestores[pair[[1]], pair[[2]]];
          Do[
            subtract = expandAmplitude[coefP*pair[[1]] + coefN*pair[[2]]];
            insert = expandAmplitude[coefP*restored[[q]]];
            If[! samePoly[subtract, insert],
              AppendTo[moves, <|"Name" -> "Schouten", "Subtract" -> subtract, "Insert" -> insert|>]
            ],
            {q, Length[restored]}
          ],
          {pair, {{keys[[i]], keys[[j]]}, {keys[[j]], keys[[i]]}}}
        ]
      ]
    ],
    {i, Length[keys]}, {j, i + 1, Length[keys]}
  ];
  Do[AppendTo[moves, sq], {sq, momentumSquareMoves[expr, massRules]}];
  DeleteDuplicatesBy[moves, {#["Name"], expandAmplitude[#["Subtract"]], expandAmplitude[#["Insert"]]} &]
];

actionNames = {"Mass", "MomentumConservation", "Schouten", "ReverseSpinor", "OnShell", "MomentumSquare", "Stop"};

expressionTokens[expr_] := Module[{bag = {}},
  walk[SpinorChain[a___]] := (AppendTo[bag, "SpinorChain"]; Scan[walk, {a}]);
  walk[Spinor[k_, tp_, i_]] := (
    AppendTo[bag, "Spinor"]; AppendTo[bag, ToString[k]]; AppendTo[bag, ToString[tp]];
    AppendTo[bag, "idx:" <> ToString[i]]);
  walk[Mom[i_Integer]] := AppendTo[bag, "Mom:" <> ToString[i]];
  walk[Mom[Multiparticle[a__]]] := (AppendTo[bag, "MomMulti"]; Scan[walk, {a}]);
  walk[MomProd[a_, b_]] := (
    AppendTo[bag, "MomProd"]; AppendTo[bag, "idx:" <> ToString[a]]; AppendTo[bag, "idx:" <> ToString[b]]);
  walk[PropDen[p_, m_]] := (AppendTo[bag, "PropDen"]; walk[p]; walk[m]);
  walk[e_Plus] := (AppendTo[bag, "Plus"]; Scan[walk, List @@ e]);
  walk[e_Times] := (AppendTo[bag, "Times"]; Scan[walk, List @@ e]);
  walk[Power[a_, b_]] := (AppendTo[bag, "Power"]; walk[a]; walk[b]);
  walk[r_Rational] := AppendTo[bag, "rat:" <> ToString[InputForm[r]]];
  walk[n_Integer] := AppendTo[bag, "int:" <> ToString[n]];
  walk[s_Symbol] := AppendTo[bag, "sym:" <> SymbolName[s]];
  walk[other_] := AppendTo[bag, "h:" <> ToString[Head[other]]];
  walk[expr];
  bag
];

bagVector[tokens_List, vocab_List] := Log[1. + N[Lookup[Counts[tokens], vocab, 0]]];

idxHot[i_] := Module[{v = ConstantArray[0., 7]},
  If[IntegerQ[i] && 0 <= i <= 6, v[[i + 1]] = 1.];
  v
];

chainCode[sc_SpinorChain] := Module[{ends},
  If[chainPairQ[sc],
    ends = chainEnds[sc];
    {1., If[chainType[sc] === "Angle", 1., 0.], ends[[1]]/8., ends[[2]]/8., 0.},
    {2., If[sc[[1, 2]] === "Angle", 1., 0.], sc[[1, 3]]/8., sc[[3, 3]]/8., If[MatchQ[sc[[2]], _Mom], sc[[2, 1]]/8., 0.]}
  ]
];
chainCode[_] := {0., 0., 0., 0., 0.};

editCodes[expr_] := Module[{chains, coded},
  chains = Cases[expr, _SpinorChain, Infinity];
  If[chains === {}, Return[ConstantArray[0., 15]]];
  coded = chainCode /@ Take[chains, UpTo[3]];
  Flatten[PadRight[coded, {3, 5}, 0.]]
];

candidateFeature[expr_, cand_Association, vocab_List] := Module[{idxs, a, b, mom, leg},
  idxs = Cases[cand["Subtract"], (Spinor[_, _, i_] | Mom[i_Integer]) :> i, Infinity];
  a = If[idxs === {}, 0, idxs[[1]]];
  b = If[Length[idxs] < 2, 0, idxs[[2]]];
  mom = FirstCase[cand["Subtract"], Mom[i_Integer] :> i, 0, Infinity];
  leg = Lookup[cand, "Leg", Lookup[cand, "Momentum", 0]];
  Join[
    bagVector[expressionTokens[expr], vocab],
    bagVector[expressionTokens[cand["Subtract"]], vocab],
    bagVector[expressionTokens[cand["Insert"]], vocab],
    editCodes[cand["Subtract"]],
    editCodes[cand["Insert"]],
    N[Boole[# === cand["Name"]] & /@ actionNames],
    idxHot[a], idxHot[b], idxHot[mom], idxHot[If[IntegerQ[leg], leg, 0]],
    {
      N[Length[monomialsOf[expr]]]/20.,
      N[Length[monomialsOf[cand["Subtract"]]]]/6.,
      N[Length[monomialsOf[cand["Insert"]]]]/6.
    }
  ]
];

buildVocabulary[tokenLists_List] := Module[{counts, ranked, forced},
  counts = Counts[Flatten[tokenLists]];
  ranked = Keys[ReverseSort[counts]];
  forced = Join[
    {"SpinorChain", "Spinor", "Angle", "Square", "Spin", "Helicity", "Plus", "Times", "Power", "PropDen", "MomProd"},
    Table["idx:" <> ToString[i], {i, 0, 6}],
    Table["Mom:" <> ToString[i], {i, 0, 6}],
    {"sym:Me", "sym:Mm", "sym:MW", "sym:MZ", "sym:MH", "sym:EE"}
  ];
  Take[DeleteDuplicates[Join[forced, ranked]], UpTo[80]]
];

stateRows[expr_, massRules_List, target_] := Module[{cands, labels},
  cands = inverseMoves[expr, massRules];
  labels = Table[
    If[target =!= None && samePoly[applyCandidate[expr, cands[[n]]], target], 1, 0],
    {n, Length[cands]}
  ];
  <|"Candidates" -> cands, "Labels" -> labels, "Hit" -> MemberQ[labels, 1] || target === None|>
];

$modelNet = None;
$modelVocab = None;
$modelMaxLen = 64;
$actionIds = {"Stop", "ReverseSpinor", "Mass", "MomentumConservation", "Schouten", "OnShell", "MomentumSquare"};
$nAction = Length[$actionIds]*4*4*4;

modelPaths[] := <|
  "Net" -> FileNameJoin[{$modelDirectory, "unscramble-net.wlnet"}],
  "Vocab" -> FileNameJoin[{$modelDirectory, "unscramble-vocab.m"}]
|>;

loadUnscrambleModel[] := Module[{paths, meta},
  If[Head[$modelNet] =!= NetChain && Head[$modelNet] =!= NetGraph,
    paths = modelPaths[];
    If[FileExistsQ[paths["Net"]] && FileExistsQ[paths["Vocab"]],
      $modelNet = Import[paths["Net"]];
      meta = Get[paths["Vocab"]];
      If[AssociationQ[meta],
        $modelVocab = meta["Tokens"];
        $modelMaxLen = meta["MaxLen"],
        $modelVocab = meta
      ]
    ]
  ];
  Head[$modelNet] === NetChain || Head[$modelNet] === NetGraph
];

encodeAction[id_Integer, a_Integer, b_Integer, dir_Integer] :=
  1 + (id - 1)*64 + (a - 1)*16 + (b - 1)*4 + Mod[dir, 4];

decodeAction[idx_Integer] := Module[{z = idx - 1, dir, b, a, id},
  dir = Mod[z, 4]; z = Quotient[z, 4];
  b = Mod[z, 4] + 1; z = Quotient[z, 4];
  a = Mod[z, 4] + 1;
  id = Quotient[z, 4] + 1;
  <|"Id" -> $actionIds[[Clip[id, {1, Length[$actionIds]}]]], "A" -> a, "B" -> b, "Dir" -> dir|>
];

tokenVector[expr_, vocab_List] := Module[{ids},
  ids = Lookup[AssociationThread[vocab -> Range[Length[vocab]]], expressionTokens[expr], 1];
  PadRight[Take[ids, UpTo[$modelMaxLen]], $modelMaxLen, 1]
];

makePolicy[vocabSize_Integer] := NetChain[{
  EmbeddingLayer[16, Max[vocabSize, 1]],
  FlattenLayer[],
  LinearLayer[128], ElementwiseLayer[Ramp],
  LinearLayer[64], ElementwiseLayer[Ramp],
  LinearLayer[$nAction]
}, "Input" -> $modelMaxLen];

policyProbabilities[expr_] := Module[{logits, shifted},
  logits = $modelNet[tokenVector[expr, $modelVocab]];
  shifted = logits - Max[logits];
  Exp[shifted]/Total[Exp[shifted]]
];

(* The network names one identity and where it applies. Nothing else is built. *)
guessAction[expr_, sample_:False] := Module[{probs, idx},
  If[sample && RandomReal[] < 0.3,
    Return[decodeAction[RandomInteger[{1, $nAction}]]]
  ];
  probs = policyProbabilities[expr];
  idx = If[sample, RandomChoice[probs -> Range[$nAction]], Ordering[probs, -1][[1]]];
  decodeAction[idx]
];

replaceFirst[expr_, pattern_, value_] := Module[{pos},
  pos = Position[expr, pattern, Infinity, 1];
  If[pos === {}, expr, expandAmplitude[ReplacePart[expr, pos[[1]] -> value]]]
];

applyReverse[expr_, a_, b_] := Module[{pos, sc},
  pos = Position[expr, SpinorChain[Spinor[_, _, a], Spinor[_, _, b]] |
    SpinorChain[Spinor[_, _, a], Mom[_], Spinor[_, _, b]], Infinity, 1];
  If[pos === {}, Return[expr]];
  sc = Extract[expr, pos[[1]]];
  expandAmplitude[ReplacePart[expr, pos[[1]] -> If[chainPairQ[sc],
    -SpinorChain[sc[[2]], sc[[1]]],
    SpinorChain[sc[[3]], sc[[2]], sc[[1]]]
  ]]]
];

applyMass[expr_, rules_, leg_, other_, dir_] := Module[{pos, sc, m, kind, tp, br},
  pos = Position[expr, SpinorChain[Spinor[_, _, leg], Mom[leg], Spinor[_, _, other]], Infinity, 1];
  If[pos === {}, Return[expr]];
  sc = Extract[expr, pos[[1]]];
  m = massValue[rules, leg];
  If[m === 0 || m === Missing["Mass"], Return[expr]];
  kind = chainKind[sc];
  tp = If[sc[[1, 2]] === "Angle", "Square", "Angle"];
  br = If[dir < 2, m*bracket[kind, tp, leg, other], -m*bracket[kind, tp, other, leg]];
  expandAmplitude[ReplacePart[expr, pos[[1]] -> br]]
];

applyOnShell[expr_, rules_, a_, b_, dir_] := Module[{i, j, mp, dot, bigM, mi, mj, pos},
  {i, j} = Sort[{a, b}];
  bigM = lineMass[rules, i, j];
  If[bigM === None, Return[expr]];
  mi = massValue[rules, i]; mj = massValue[rules, j];
  mp = Mom[Multiparticle[i, j]];
  dot = orderedDot[i, j];
  If[EvenQ[dir],
    replaceFirst[expr, mp^2, bigM^2],
    replaceFirst[expr, dot, shellOf[bigM, mi, mj]]
  ]
];

applyMomentumSquare[expr_, rules_, a_, b_, dir_] := Module[{i, j, mp, dot, mi, mj},
  {i, j} = Sort[{a, b}];
  If[i === j, Return[expr]];
  mi = massValue[rules, i]; mj = massValue[rules, j];
  If[mi === Missing["Mass"] || mj === Missing["Mass"], Return[expr]];
  mp = Mom[Multiparticle[i, j]];
  dot = orderedDot[i, j];
  If[EvenQ[dir],
    replaceFirst[expr, dot, (mp^2 - mi^2 - mj^2)/2],
    replaceFirst[expr, mp^2, 2*dot + mi^2 + mj^2]
  ]
];

applySchouten[expr_, rules_, a_, b_, dir_] := Module[
  {exts, others, c, d, tp, kind, plusB, minusB, groups, keys, rest, coefP, coefN, skP, skN, found},
  exts = externalIndices[rules];
  others = Complement[exts, {a, b}];
  If[Length[others] != 2, Return[expr]];
  {c, d} = others;
  tp = If[EvenQ[dir], "Angle", "Square"];
  kind = "Spin";
  plusB = Sort[{bracket[kind, tp, a, c], bracket[kind, tp, b, d]}];
  minusB = Sort[{bracket[kind, tp, a, d], bracket[kind, tp, b, c]}];
  groups = groupsOf[expr];
  keys = Keys[groups];
  found = $Failed;
  Do[
    Module[{brs, o},
      brs = Sort[Select[spinorFactors[keys[[g]]], chainPairQ]];
      o = otherFactors[keys[[g]]];
      If[brs === plusB && Count[spinorFactors[keys[[g]]], _?threeChainQ] === 0,
        Do[
          If[Sort[Select[spinorFactors[keys[[h]]], chainPairQ]] === minusB && otherFactors[keys[[h]]] === o &&
              samePoly[groups[keys[[g]]] + groups[keys[[h]]], 0],
            found = {keys[[g]], keys[[h]], o, groups[keys[[g]]]}
          ],
          {h, Length[keys]}
        ]
      ]
    ],
    {g, Length[keys]}
  ];
  If[found === $Failed, Return[expr]];
  {skP, skN, rest, coefP} = found;
  coefN = groups[skN];
  expandAmplitude[expr - coefP*skP - coefN*skN + coefP*rest*bracket[kind, tp, a, b]*bracket[kind, tp, c, d]]
];

applyMomentumCollect[expr_, rules_, mom_] := Module[{groups, keys, exts, buckets, done = expr},
  exts = externalIndices[rules];
  If[! MemberQ[exts, mom] || Length[exts] < 2, Return[expr]];
  groups = groupsOf[expr];
  keys = Keys[groups];
  buckets = <||>;
  Do[
    Module[{sk = keys[[g]], nMoms, count, which, template},
      nMoms = Length[Cases[sk, Mom[_Integer], Infinity]];
      Do[
        count = 0; which = 0;
        template = sk /. Mom[ii_Integer] :> (count++; If[count === n, which = ii; Mom[0], Mom[ii]]);
        If[which =!= 0,
          buckets[template] = Append[Lookup[buckets, template, {}], {which, groups[sk], sk}]
        ],
        {n, Max[nMoms, 0]}
      ]
    ],
    {g, Length[keys]}
  ];
  Do[
    If[done =!= expr, Break[]];
    Module[{template = Keys[buckets][[k]], entries = buckets[Keys[buckets][[k]]], byMom = <||>, present, alpha, skJ},
      Do[byMom[entries[[n, 1]]] = {entries[[n, 2]], entries[[n, 3]]}, {n, Length[entries]}];
      present = Keys[byMom];
      If[Complement[exts, present] === {mom} && Length[present] === Length[exts] - 1,
        alpha = byMom[present[[1]]][[1]];
        If[AllTrue[present, samePoly[byMom[#][[1]], alpha] &],
          skJ = template /. Mom[0] -> Mom[mom];
          done = expandAmplitude[expr - Total[alpha*Table[byMom[present[[n]]][[2]], {n, Length[present]}]] - alpha*skJ]
        ]
      ]
    ],
    {k, Length[buckets]}
  ];
  done
];

(* Apply the single named identity. If that pattern is not present, leave the expression alone. *)
applyGuess[expr_, rules_List, guess_Association] := Module[{a = guess["A"], b = guess["B"], dir = guess["Dir"]},
  Switch[guess["Id"],
    "Stop", expr,
    "ReverseSpinor", If[a === b, expr, applyReverse[expr, a, b]],
    "Mass", applyMass[expr, rules, a, b, dir],
    "MomentumConservation", applyMomentumCollect[expr, rules, a],
    "Schouten", applySchouten[expr, rules, a, b, dir],
    "OnShell", applyOnShell[expr, rules, a, b, dir],
    "MomentumSquare", applyMomentumSquare[expr, rules, a, b, dir],
    _, expr
  ]
];

yesProbability[net_, feature_List] := Module[{p},
  p = net[feature, "Probabilities"];
  Which[
    AssociationQ[p], N[Lookup[p, 1, 0]],
    ListQ[p], N[Last[p]],
    True, 0.
  ]
];

(* Lower is simpler: fewer terms, fewer inserted momenta, fewer momentum products,
   and fewer off-shell (p_i+p_j)^2 factors. *)
SpinorExpressionComplexity[expr_] := expressionComplexity[expr];

expressionComplexity[expr_] := Module[{chains, pairs},
  chains = Cases[expr, _SpinorChain, Infinity];
  pairs = Select[chains, chainPairQ];
  Length[monomialsOf[expr]] +
    4*Count[chains, _?threeChainQ] +
    4*Count[{expr}, _MomProd, Infinity] +
    2*Count[{expr}, Power[Mom[Multiparticle[__]], 2], Infinity] +
    Count[pairs, c_ /; chainEnds[c][[1]] > chainEnds[c][[2]]]
];

cappedMoves[moves_List] := Module[{picked = {}},
  Do[
    picked = Join[picked, Take[Select[moves, #["Name"] === name &], UpTo[2]]],
    {name, DeleteCases[actionNames, "Stop"]}
  ];
  picked
];

stopMove[] := <|"Name" -> "Stop", "Subtract" -> 0, "Insert" -> 0|>;

(* {simplest complexity reachable, fewest legal moves to reach it}.
   A step may increase complexity when that is the shortest way down. *)
plan[expr_, massRules_List, depth_Integer] := Module[{here, moves, value, steps, child, v, s},
  here = expressionComplexity[expr];
  value = here; steps = 0;
  If[depth <= 0, Return[{value, steps}]];
  moves = cappedMoves[inverseMoves[expr, massRules]];
  Do[
    child = plan[applyCandidate[expr, moves[[n]]], massRules, depth - 1];
    v = child[[1]]; s = child[[2]] + 1;
    If[v < value || (v == value && v < here && s < steps),
      value = v; steps = s
    ],
    {n, Length[moves]}
  ];
  {value, steps}
];

(* Positive moves are those on a shortest path to the simplest expression
   the legal moves can reach in two steps. Stop is positive only when no
   legal move improves the score. *)
labelState[expr_, massRules_List] := Module[{all, cands, here, goal, child, labs},
  all = inverseMoves[expr, massRules];
  cands = DeleteDuplicates[Join[
    cappedMoves[all],
    Select[all, MemberQ[{"MomentumSquare", "OnShell"}, #["Name"]] &]
  ]];
  here = expressionComplexity[expr];
  goal = plan[expr, massRules, 2];
  labs = Table[
    child = plan[applyCandidate[expr, cands[[n]]], massRules, 1];
    Boole[goal[[1]] < here && child[[1]] == goal[[1]] && child[[2]] + 1 == goal[[2]]],
    {n, Length[cands]}
  ];
  AppendTo[cands, stopMove[]];
  AppendTo[labs, Boole[goal[[1]] == here]];
  {cands, labs}
];

chooseMove[expr_, massRules_List] := Module[{cands, probs, best},
  If[! loadUnscrambleModel[], Return[None]];
  cands = Append[inverseMoves[expr, massRules], stopMove[]];
  probs = Table[yesProbability[$modelNet, candidateFeature[expr, cands[[n]], $modelVocab]], {n, Length[cands]}];
  best = Ordering[probs, -1][[1]];
  If[cands[[best, "Name"]] === "Stop", None, cands[[best]]]
];

Options[TrainUnscrambleNet] = {Steps -> 8, Scrambles -> 20, HoldOut -> 2, MaxTrainingRounds -> 6, EpisodeLength -> 6, Kernels -> Automatic};

actionCode[guess_Association] := encodeAction[
  Position[$actionIds, guess["Id"]][[1, 1]], guess["A"], guess["B"], guess["Dir"]];

(* One guess, then do it. No list of alternative expressions is built. *)
runEpisode[start_, rules_List, len_Integer] := Module[{expr = start, hist = {}, guess, next},
  Do[
    guess = guessAction[expr, True];
    If[guess["Id"] === "Stop", Break[]];
    next = applyGuess[expr, rules, guess];
    If[! samePoly[next, expr],
      AppendTo[hist, {tokenVector[expr, $modelVocab], actionCode[guess]}];
      expr = next
    ],
    {len}
  ];
  {expr, hist}
];

workerPackage[] := FileNameJoin[{$modelDirectory, "unscrambling.wl"}];
workerHEPCAT[] := FileNameJoin[{DirectoryName[$modelDirectory], "source", "HEPCAT.wl"}];

(* Subkernels each load the package. LaunchKernels uses every licensed CPU. *)
startWorkers[requested_Integer] := Module[{pkg, hep, n},
  n = Max[requested, 0];
  If[n <= 1, Return[0]];
  If[$KernelCount > n, Quiet[CloseKernels[Take[$Kernels, $KernelCount - n]]]];
  If[$KernelCount < n, Quiet[LaunchKernels[n - $KernelCount]]];
  If[$KernelCount == 0, Return[0]];
  pkg = workerPackage[];
  hep = workerHEPCAT[];
  With[{pkg = pkg, hep = hep, path = DirectoryName[hep] <> "/"},
    ParallelEvaluate[
      Global`$HEPCATpath = path;
      If[! MemberQ[$Packages, "ConstructiveDiagrams`"], Get[hep]];
      If[! MemberQ[$Packages, "Unscrambling`"], Get[pkg]];
    ]
  ];
  $KernelCount
];

sharePolicy[] := With[{net = $modelNet, vocab = $modelVocab, len = $modelMaxLen},
  ParallelEvaluate[
    $modelNet = net;
    $modelVocab = vocab;
    $modelMaxLen = len;
  ]
];

TrainUnscrambleNet[amplitudes_List, OptionsPattern[]] := Module[
  {steps, scrambles, hold, rounds, episode, vocab, starts, kept, nKernels, jobs, outcomes,
    paths, holdN = 0, holdSimpler = 0, holdRec = 0, inputs, targets},
  steps = OptionValue[Steps];
  scrambles = OptionValue[Scrambles];
  hold = OptionValue[HoldOut];
  rounds = OptionValue[MaxTrainingRounds];
  episode = OptionValue[EpisodeLength];
  vocab = buildVocabulary[expressionTokens /@ Table[
    expandAmplitude[amplitudes[[i, 2]] /. amplitudes[[i, 1]]],
    {i, Length[amplitudes]}
  ]];
  $modelVocab = vocab;
  $modelNet = NetInitialize[makePolicy[Length[vocab]], RandomSeeding -> 1];
  nKernels = startWorkers[If[OptionValue[Kernels] === Automatic, $ProcessorCount, OptionValue[Kernels]]];
  WriteString["stdout", "kernels ", nKernels, "\n"];
  jobs = Flatten[Table[{amplitudes[[i]], seed}, {i, Length[amplitudes]}, {seed, scrambles}], 1];
  starts = If[nKernels > 0,
    With[{n = steps},
      sharePolicy[];
      ParallelMap[
        Function[job, {job[[1, 1]], ComplicateAmplitude[job[[1]], n, job[[2]]]["Expression"]}],
        jobs
      ]
    ],
    Table[{jobs[[s, 1, 1]], ComplicateAmplitude[jobs[[s, 1]], steps, jobs[[s, 2]]]["Expression"]}, {s, Length[jobs]}]
  ];
  Do[
    kept = {};
    outcomes = If[nKernels > 0,
      sharePolicy[];
      With[{len = episode, tag = r},
        ParallelMap[
          Function[job,
            SeedRandom[10007*tag + job[[3]]];
            Module[{final, hist},
              {final, hist} = runEpisode[job[[2]], job[[1]], len];
              If[expressionComplexity[final] < expressionComplexity[job[[2]]] && hist =!= {}, hist, {}]
            ]
          ],
          MapIndexed[{#[[1]], #[[2]], #2[[1]]} &, starts]
        ]
      ],
      Table[
        SeedRandom[10007*r + s];
        Module[{final, hist},
          {final, hist} = runEpisode[starts[[s, 2]], starts[[s, 1]], episode];
          If[expressionComplexity[final] < expressionComplexity[starts[[s, 2]]] && hist =!= {}, hist, {}]
        ],
        {s, Length[starts]}
      ]
    ];
    kept = Flatten[outcomes, 1];
    WriteString["stdout", "round ", r, " kept ", Length[kept], "\n"];
    If[kept === {}, Continue[]];
    inputs = kept[[All, 1]];
    targets = kept[[All, 2]];
    $modelNet = NetTrain[$modelNet, Thread[inputs -> targets],
      LossFunction -> CrossEntropyLossLayer["Index"],
      MaxTrainingRounds -> 8,
      BatchSize -> Min[64, Length[targets]],
      TargetDevice -> "CPU",
      TrainingProgressReporting -> None,
      Method -> {"ADAM", "LearningRate" -> 0.003}
    ];
    , {r, rounds}
  ];
  paths = modelPaths[];
  Export[paths["Net"], $modelNet];
  Put[<|"Tokens" -> $modelVocab, "MaxLen" -> $modelMaxLen|>, paths["Vocab"]];
  Do[
    Module[{rules = amplitudes[[i, 1]], original, scr, got},
      original = expandAmplitude[amplitudes[[i, 2]] /. rules];
      Do[
        scr = ComplicateAmplitude[amplitudes[[i]], steps, scrambles + seed];
        got = UnscrambleSpinorAmplitudes[{rules, scr["Expression"]}];
        holdN++;
        If[expressionComplexity[got[[2]]] < expressionComplexity[scr["Expression"]], holdSimpler++];
        If[samePoly[got[[2]], original], holdRec++],
        {seed, hold}
      ]
    ],
    {i, Length[amplitudes]}
  ];
  <|
    "Kernels" -> nKernels,
    "Starts" -> Length[starts],
    "HoldoutScrambles" -> holdN,
    "HoldoutSimpler" -> holdSimpler,
    "HoldoutRecovered" -> holdRec
  |>
];

UnscrambleSpinorAmplitudes::nomodel = "No trained net found. Evaluate TrainUnscrambleNet on a list of amplitudes first.";

(* Look, guess one identity, do it, and repeat a handful of times.
   Keep the result only when the expression got simpler; otherwise go back. *)
UnscrambleSpinorAmplitudes[{massRules_List, amplitude_}] := Module[
  {expr, checkpoint, trial, attempt, guess, next},
  If[! loadUnscrambleModel[],
    Message[UnscrambleSpinorAmplitudes::nomodel];
    Return[{massRules, amplitude}]
  ];
  expr = expandAmplitude[amplitude /. massRules];
  checkpoint = expr;
  Do[
    trial = checkpoint;
    Do[
      guess = guessAction[trial, attempt > 1];
      If[guess["Id"] === "Stop", Break[]];
      next = applyGuess[trial, massRules, guess];
      trial = next,
      {4}
    ];
    If[expressionComplexity[trial] < expressionComplexity[checkpoint],
      checkpoint = trial
    ],
    {attempt, 5}
  ];
  {massRules, checkpoint}
];

End[];
EndPackage[];
