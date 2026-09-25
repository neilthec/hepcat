(* Loaded inside Unscrambling`Private`. Decay Rates, Appendix B, Eq. (B11).
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
