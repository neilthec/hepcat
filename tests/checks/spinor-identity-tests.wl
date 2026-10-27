(* Exact algebra, numerator scoping, and higher-point identity regressions. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]]);
ang[i_, j_] := SpinorChain[Spinor["Helicity", "Angle", i], Spinor["Helicity", "Angle", j]];

(* ::Section:: *)
(* Generalized Schouten: independent exact two-component matrices. *)
sp[t_, i_] := Spinor["Helicity", t, i];
ch[t_, i_, ps_List, u_, j_] := SpinorChain @@ Join[{sp[t, i]}, Mom /@ ps, {sp[u, j]}];

(* Independent two-component evaluation, using arbitrary exact complex matrices.
   The alternating momentum matrices are P and adj(P), as in Eq. (A14).
   Opposite epsilon signs on the square and angle rows implement Eq. (A28). *)
eps = {{0, 1}, {-1, 0}};
adj[p_] := {{p[[2, 2]], -p[[1, 2]]}, {-p[[2, 1]], p[[1, 1]]}};
SeedRandom[20260924];
vectors = AssociationThread[Range[8], Table[RandomInteger[{-5, 5}, 2] + I*RandomInteger[{-5, 5}, 2], {8}]];
matrices = AssociationThread[Range[8], Table[RandomInteger[{-5, 5}, {2, 2}] + I*RandomInteger[{-5, 5}, {2, 2}], {8}]];
type[s_] := ConstructiveDiagrams`Private`spinorType[s];
leg[s_] := ConstructiveDiagrams`Private`spinorLeg[s];
evalChain[c_SpinorChain] := Module[{xs = List @@ c, angle, row, ps},
  angle = type[First[xs]] === "Angle";
  row = vectors[leg[First[xs]]].eps*If[angle, 1, -1];
  ps = Drop[Rest[xs], -1];
  Do[
    row = row.If[angle, matrices[p[[1]]], adj[matrices[p[[1]]]]];
    angle = !angle,
    {p, ps}
  ];
  row.vectors[leg[Last[xs]]]
];
numeric[ex_] := Expand[ex /. c_SpinorChain :> evalChain[c]];

(* All chiralities, chain lengths zero through four, and every split. *)
Do[
  c1 = ch[t1, 1, Take[{5, 6, 7, 8}, len1],
    If[EvenQ[len1], t1, If[t1 === "Angle", "Square", "Angle"]], 2];
  c2 = ch[t2, 3, Take[{8, 7, 6, 5}, len2],
    If[EvenQ[len2], t2, If[t2 === "Angle", "Square", "Angle"]], 4];
  rhs = SchoutenRewrite[c1, c2, {m, n}];
  allowed = Mod[Boole[t1 === "Square"] + m, 2] === Mod[Boole[t2 === "Square"] + n, 2];
  check[{"split", t1, t2, len1, len2, m, n},
    If[allowed, rhs =!= $Failed && numeric[c1*c2 - rhs] === 0, rhs === $Failed]],
  {t1, {"Angle", "Square"}}, {t2, {"Angle", "Square"}},
  {len1, 0, 4}, {len2, 0, 4}, {m, 0, len1}, {n, 0, len2}
];

c1 = ch["Angle", 1, {5, 6}, "Angle", 2];
c2 = ch["Square", 3, {}, "Square", 4];
check["B18", Expand[SchoutenRewrite[c1, c2, {1, 0}] -
  (-ch["Square", 4, {6}, "Angle", 2]*ch["Square", 3, {5}, "Angle", 1] +
    ch["Square", 4, {5}, "Angle", 1]*ch["Square", 3, {6}, "Angle", 2])] === 0];
check["invalid split", SchoutenRewrite[c1, c2, {3, 0}] === $Failed];
check["invalid chain", SchoutenRewrite[ch["Angle", 1, {5}, "Angle", 2], c2, {0, 0}] === $Failed];
mixed = c1 /. sp["Angle", 1] -> Spinor["Spin", "Upper", "Angle", 1, ii];
rhs = SchoutenRewrite[mixed, c2, {1, 0}];
check["explicit spin index preserved", !FreeQ[rhs, Spinor["Spin", "Upper", "Angle", 1, ii]] && numeric[mixed*c2 - rhs] === 0];

sample = 3*c1*c2*ch["Angle", 5, {}, "Angle", 6]/PropDen[Mom[Multiparticle[1, 2]], mass];
moves = SchoutenCandidates[sample];
check["candidate coverage", Length[moves] > 0];
check["spectators and denominator", AllTrue[moves, numeric[#["Subtract"] - #["Insert"]] === 0 &]];
check["powers", AllTrue[SchoutenCandidates[c1^2*c2], numeric[#["Subtract"] - #["Insert"]] === 0 &]];

(* A one-momentum Schouten scramble must be reachable through legal candidates. *)
original = ch["Angle", 1, {}, "Angle", 2]*ch["Square", 3, {5}, "Angle", 4];
scrambled = SchoutenRewrite[ch["Angle", 1, {}, "Angle", 2], ch["Square", 3, {5}, "Angle", 4], {0, 1}];
rules = Table[Mass[i] -> 0, {i, 4}];
state = ConstructiveDiagrams`Private`canonicalChains[scrambled];
guesses = Select[SpinorIdentityCandidates[{rules, scrambled}, "MomentumConservation" -> False],
  #["Name"] === "Schouten" &];
outputs = ConstructiveDiagrams`Private`applyCandidate[state, #] & /@ guesses;
check["legal edits can shorten momentum Schouten", AnyTrue[outputs,
  ConstructiveDiagrams`Private`expressionComplexity[#] < ConstructiveDiagrams`Private`expressionComplexity[scrambled] &]];
check["all Schouten actions preserve value", AllTrue[outputs, numeric[# - scrambled] === 0 &]];
check["longer chains are penalized", SpinorExpressionComplexity[ch["Angle", 1, {5, 6}, "Angle", 2]] >
  SpinorExpressionComplexity[ch["Angle", 1, {5}, "Square", 2]]];


(* ::Section:: *)
(* Interior chains, composite momenta, scalar relations, and opaque poles. *)
Clear[moves];
rules = {Mass[1] -> m1, Mass[2] -> m2, Mass[3] -> m3, Mass[4] -> m4};
chain[pp___] := SpinorChain[Spinor["Spin", "Angle", 1], pp,
  Spinor["Spin", If[EvenQ[Length[{pp}]], "Angle", "Square"], 4]];
moves[expr_, rr_:rules] := SpinorIdentityCandidates[{rr, expr}, "MomentumConservation" -> False];
apply[expr_, move_] := ConstructiveDiagrams`Private`applyCandidate[ConstructiveDiagrams`Private`canonicalChains[expr], move];
same[a_, b_] := Expand[ConstructiveDiagrams`Private`canonicalChains[a - b]] === 0;
expr = 3 chain[Mom[2], Mom[2]]^2;
contract = Select[moves[expr], #["Name"] === "ChainSquare" && #["Direction"] === "Contract" &];
check["one chain factor contracted", Length[contract] === 1 && same[apply[expr, First[contract]], 3 m2^2 chain[] chain[Mom[2], Mom[2]]]];
expr = chain[Mom[2], Mom[3]];
anti = Select[moves[expr], #["Name"] === "Anticommutation" &];
check["adjacent swap", Length[anti] === 1 && same[apply[expr, First[anti]],
  2 MomProd[2, 3] chain[] - chain[Mom[3], Mom[2]]]];
check["identity agrees with independent reducer", Expand[ReduceSpinorProducts[expr - apply[expr, First[anti]]] /. rules] === 0];
after = apply[expr, First[anti]];
check["swap reverse reachable", AnyTrue[moves[after], same[apply[after, #], expr] &]];
expansions = Select[moves[chain[]], #["Name"] === "ChainSquare" && #["Direction"] === "Expand" &];
check["bounded inverse insertions", Length[expansions] === 4];
check["every insertion can contract back", AllTrue[expansions, Function[move,
  With[{inserted = apply[chain[], move]}, AnyTrue[moves[inserted], same[apply[inserted, #], chain[]] &]]]]];
zeroRules = rules /. m2 -> 0;
zeroMoves = Select[moves[chain[Mom[2], Mom[2]], zeroRules], #["Name"] === "ChainSquare" && #["Direction"] === "Contract" &];
check["massless contraction", apply[chain[Mom[2], Mom[2]], First[zeroMoves]] === 0];
check["no division by massless mass", Length[Select[moves[chain[], zeroRules], #["Name"] === "ChainSquare" && #["Direction"] === "Expand" &]] === 3];
internal = chain[Mom[Multiparticle[2, 3]], Mom[Multiparticle[2, 3]]];
internalRules = Append[rules, Mass[Multiparticle[2, 3]] -> mz];
on = Select[SpinorIdentityCandidates[{internalRules, internal}], #["Name"] === "ChainSquare" && #["Direction"] === "Contract" &];
off = Select[SpinorIdentityCandidates[{internalRules, internal}, "OnShellChannels" -> {}], #["Name"] === "ChainSquare" && #["Direction"] === "Contract" &];
check["internal mass when on shell", same[apply[internal, First[on]], mz^2 chain[]]];
check["off shell square retained", same[apply[internal, First[off]], Mom[Multiparticle[2, 3]]^2 chain[]]];
long = SpinorChain[Spinor["Spin", "Angle", 1], Mom[2], Mom[3], Mom[2], Spinor["Spin", "Square", 4]];
target = 2 MomProd[2, 3] SpinorChain[Spinor["Spin", "Angle", 1], Mom[2], Spinor["Spin", "Square", 4]] -
  m2^2 SpinorChain[Spinor["Spin", "Angle", 1], Mom[3], Spinor["Spin", "Square", 4]];
check["three insertions reduce in two edits", AnyTrue[Select[moves[long], #["Name"] === "Anticommutation" &], Function[move,
  With[{next = apply[long, move]}, AnyTrue[moves[next], same[apply[next, #], target] &]]]]];
indexed = SpinorChain[Spinor["Spin", "Up", "Angle", 1, 7], Mom[2], Mom[2], Spinor["Spin", "Down", "Angle", 4, 9]];
indexedMoves = Select[moves[indexed], #["Name"] === "ChainSquare" && #["Direction"] === "Contract" &];
check["little group indices preserved", same[apply[indexed, First[indexedMoves]],
  m2^2 SpinorChain[Spinor["Spin", "Up", "Angle", 1, 7], Spinor["Spin", "Down", "Angle", 4, 9]]]];
Do[
  sample = SpinorChain @@ Join[{Spinor["Spin", chirality, 1]}, Mom /@ momenta,
    {Spinor["Spin", chirality, 4]}];
  forward = Select[moves[sample], #["Name"] === "Anticommutation" ||
    (#["Name"] === "ChainSquare" && #["Direction"] === "Contract") &];
  check["interior identity " <> ToString[{chirality, momenta}, InputForm],
    AllTrue[forward, Expand[ReduceSpinorProducts[sample - apply[sample, #]] /. rules] === 0 &]],
  {chirality, {"Angle", "Square"}},
  {momenta, {{2, 3}, {3, 2}, {2, 2}, {2, 3, 3, 2}, {2, 3, 2, 3}}}];
den = PropDen[Mom[Multiparticle[2, 3]], mz];
shell = (mz^2 - m2^2 - m3^2)/2;
expr = MomProd[2, 3] chain[]/den;
target = shell chain[]/den;
check["diagram numerator on shell", AnyTrue[moves[expr, internalRules], same[apply[expr, #], target] &]];
check["direct reverse on shell move", AnyTrue[Select[moves[target, internalRules], #["Name"] === "OnShell" &], same[apply[target, #], expr] &]];
expr = MomProd[2, 3]^2 chain[]/den;
check["dot power contracts one factor", AnyTrue[moves[expr, internalRules], same[apply[expr, #], shell MomProd[2, 3] chain[]/den] &]];
powerTarget = shell MomProd[2, 3] chain[]/den;
check["dot power reverse reachable", AnyTrue[moves[powerTarget, internalRules], same[apply[powerTarget, #], expr] &]];
check["all on shell moves preserve propagator", AllTrue[Select[moves[expr, internalRules], #["Name"] === "OnShell" &], !FreeQ[apply[expr, #], den] &]];
check["no internal rule no on shell move", FreeQ[Lookup[moves[expr], "Name"], "OnShell"]];
check["disabled internal channel", FreeQ[Lookup[SpinorIdentityCandidates[{internalRules, expr}, "OnShellChannels" -> {}], "Name"], "OnShell"]];
zeroInternal = {Mass[1] -> 0, Mass[2] -> 0, Mass[3] -> 0, Mass[4] -> 0, Mass[Multiparticle[2, 3]] -> 0};
expr = MomProd[2, 3] chain[]/den;
check["zero shell contraction", AnyTrue[moves[expr, zeroInternal], apply[expr, #] === 0 &]];
check["zero shell no inverse division", FreeQ[moves[chain[]/den, zeroInternal], Indeterminate | ComplexInfinity]];

(* Check composite-momentum identities against an independent multilinear
   expansion, without relying on candidate generation or canonicalChains. *)
momentumTerms[Mom[leg_Integer]] := {{1, Mom[leg]}};
momentumTerms[Mom[Multiparticle[legs__Integer]]] := ({1, Mom[#]} & /@ {legs});
momentumTerms[p_Plus] := Join @@ (momentumTerms /@ List @@ p);
momentumTerms[p_Times] := Module[{factors = List @@ p, momenta, coefficient},
  momenta = Select[factors, MatchQ[#, _Mom] &];
  If[Length[momenta] =!= 1, Return[{{1, p}}]];
  coefficient = Times @@ Select[factors, !MatchQ[#, _Mom] &];
  ({coefficient #[[1]], #[[2]]} & /@ momentumTerms[First[momenta]])
];
momentumTerms[p_] := {{1, p}};
explicitChain[c_SpinorChain] := Module[{parts = List @@ c, choices},
  choices = Tuples[momentumTerms /@ Drop[Rest[parts], -1]];
  Total[(Times @@ (First /@ #)) (SpinorChain @@
      Join[{First[parts]}, Last /@ #, {Last[parts]}]) & /@ choices]
];
explicitMomenta[expr_] := Expand[(expr /. c_SpinorChain :> explicitChain[c]) /.
  c_SpinorChain :> ConstructiveDiagrams`Private`canonicalChain[c]];
sameMomenta[a_, b_] := explicitMomenta[a - b] === 0;
sumMoves[expr_, rr_:rules] := Select[
  SpinorIdentityCandidates[{rr, expr}, "MomentumConservation" -> False,
    "OnShellChannels" -> {}], #["Name"] === "MomentumSum" &];
reverseSumQ[before_, move_, rr_:rules] := With[{after = apply[before, move]},
  AnyTrue[sumMoves[after, rr], same[apply[after, #], before] &]];

expr = chain[Mom[Multiparticle[2, 3]]];
split = Select[sumMoves[expr], #["Direction"] === "Expand" &];
check["composite chain splits without momentum conservation or on shell", AnyTrue[split,
  same[apply[expr, #], chain[Mom[2]] + chain[Mom[3]]] &]];
check["composite split is independently linear", split =!= {} &&
  AllTrue[split, sameMomenta[expr, apply[expr, #]] &]];
check["composite split retains local site", AllTrue[split,
  MatchQ[Lookup[#, "Site", None], {_Integer, _Integer, _Integer}] &]];
check["composite split has exact reverse", split =!= {} && AllTrue[split, reverseSumQ[expr, #] &]];

check["atomic complexity unchanged", SpinorExpressionComplexity[chain[Mom[2]]] === 5 &&
  SpinorExpressionComplexity[chain[Mom[2], Mom[3]]] === 9];
check["zero complexity unchanged", SpinorExpressionComplexity[0] === 0];
check["two-member momentum counts two implicit terms",
  SpinorExpressionComplexity[expr] === 10 &&
    SpinorExpressionComplexity[expr] === SpinorExpressionComplexity[explicitMomenta[expr]]];
check["three-member momentum counts three implicit terms",
  SpinorExpressionComplexity[chain[Mom[Multiparticle[1, 2, 3]]]] === 15];
check["pure composite splits do not increase complexity", AllTrue[split,
  SpinorExpressionComplexity[apply[expr, #]] === SpinorExpressionComplexity[expr] &]];
twoSlots = chain[Mom[Multiparticle[1, 2]], Mom[Multiparticle[2, 3, 4]]];
check["multiple composite slots multiply implicit terms",
  SpinorExpressionComplexity[twoSlots] === 54 &&
    SpinorExpressionComplexity[twoSlots] === SpinorExpressionComplexity[explicitMomenta[twoSlots]]];
twoChains = chain[Mom[Multiparticle[1, 2]]] chain[Mom[Multiparticle[2, 3, 4]]];
check["composite chain products multiply implicit terms",
  SpinorExpressionComplexity[twoChains] === 54 &&
    SpinorExpressionComplexity[twoChains] === SpinorExpressionComplexity[explicitMomenta[twoChains]]];
check["composite chain powers count repeated factors",
  SpinorExpressionComplexity[chain[Mom[Multiparticle[2, 3]]]^2] === 36 &&
    SpinorExpressionComplexity[explicitMomenta[chain[Mom[Multiparticle[2, 3]]]^2]] === 27];
traceSum = SpinorTrace[Mom[Multiparticle[1, 2]], Mom[3]];
check["trace composite momentum counts implicit terms",
  SpinorExpressionComplexity[traceSum] === 18 &&
    SpinorExpressionComplexity[traceSum] === SpinorExpressionComplexity[
      SpinorTrace[Mom[1], Mom[3]] + SpinorTrace[Mom[2], Mom[3]]]];
check["implicit branches also weight spectator scalar products",
  SpinorExpressionComplexity[MomProd[1, 4] expr] === 18 &&
    SpinorExpressionComplexity[MomProd[1, 4] expr] ===
      SpinorExpressionComplexity[explicitMomenta[MomProd[1, 4] expr]]];
cancellingSum = expr - chain[Mom[2]];
check["composite split exposes a complexity improvement", AllTrue[sumMoves[cancellingSum],
  If[Lookup[#, "Identity", None] === "Split",
    SpinorExpressionComplexity[apply[cancellingSum, #]] < SpinorExpressionComplexity[cancellingSum], True] &]];
check["composite complexity independent of particle labels",
  SpinorExpressionComplexity[twoSlots] === SpinorExpressionComplexity[
    ConstructiveDiagrams`Private`relabelLegs[twoSlots, <|1 -> 11, 2 -> 27, 3 -> 43, 4 -> 68|>]]];
opaqueDen = PropDen[Mom[Multiparticle[1, 2]], MomProd[1, 2] + chain[Mom[Multiparticle[2, 3]]]];
check["complexity excludes every propagator argument",
  SpinorExpressionComplexity[1/opaqueDen] === 1 &&
    SpinorExpressionComplexity[expr/opaqueDen] === SpinorExpressionComplexity[expr] &&
    SpinorExpressionComplexity[expr opaqueDen] === SpinorExpressionComplexity[expr]];

expr = chain[Mom[2]];
introduce = Select[sumMoves[expr], #["Direction"] === "Expand" &];
check["atomic momentum introduces bundle difference", AnyTrue[introduce,
  same[apply[expr, #], chain[Mom[Multiparticle[2, 3]]] - chain[Mom[3]]] &]];
check["bundle introduction is independently linear", introduce =!= {} &&
  AllTrue[introduce, sameMomenta[expr, apply[expr, #]] &]];
check["introduced bundles have exact reverse", introduce =!= {} &&
  AllTrue[introduce, reverseSumQ[expr, #] &]];
check["atomic introduction is linear in supplied legs", Length[introduce] === Length[rules] - 1];
overlapping = chain[Mom[Multiparticle[2, 3]]] + chain[Mom[2]];
overlapSplit = Select[sumMoves[overlapping], Lookup[#, "Identity", None] === "Split" &];
check["collection reverses a split after coefficients combine", overlapSplit =!= {} &&
  AllTrue[overlapSplit, reverseSumQ[overlapping, #] &]];
check["collection with unequal coefficients remains a linear identity", AllTrue[
  sumMoves[2 chain[Mom[2]] + chain[Mom[3]]],
  sameMomenta[2 chain[Mom[2]] + chain[Mom[3]], apply[2 chain[Mom[2]] + chain[Mom[3]], #]] &]];
check["permuted composite members normalize in chains", same[
  ConstructiveDiagrams`Private`canonicalChains[chain[Mom[Multiparticle[3, 2]]]],
  chain[Mom[Multiparticle[2, 3]]]]];

Do[
  expr = pair[[1]]; target = pair[[2]];
  collect = Select[sumMoves[expr], #["Direction"] === "Contract" &];
  targetMoves = Select[collect, same[apply[expr, #], target] &];
  check["collect composite identity " <> ToString[index], targetMoves =!= {}];
  check["independent collect identity " <> ToString[index], sameMomenta[expr, target]];
  check["collect reverse reachable " <> ToString[index], targetMoves =!= {} &&
    reverseSumQ[expr, First[targetMoves]]],
  {index, 3},
  {pair, {{chain[Mom[2]] + chain[Mom[3]], chain[Mom[Multiparticle[2, 3]]]},
    {chain[Mom[Multiparticle[2, 3]]] - chain[Mom[3]], chain[Mom[2]]},
    {3 chain[Mom[2]]/den + 3 chain[Mom[3]]/den, 3 chain[Mom[Multiparticle[2, 3]]]/den}}[[{index}]]}
];

Do[
  check["explicit momentum multilinearity " <> ToString[index],
    same[ConstructiveDiagrams`Private`canonicalChains[pair[[1]]], pair[[2]]]],
  {index, 3},
  {pair, {{chain[Mom[2] + Mom[3]], chain[Mom[2]] + chain[Mom[3]]},
    {chain[2 Mom[2] - 3 Mom[3]], 2 chain[Mom[2]] - 3 chain[Mom[3]]},
    {chain[Mom[2] + Mom[3], Mom[1] - 2 Mom[4]],
      chain[Mom[2], Mom[1]] - 2 chain[Mom[2], Mom[4]] +
      chain[Mom[3], Mom[1]] - 2 chain[Mom[3], Mom[4]]}}[[{index}]]}
];

manyRules = {Mass[11] -> m11, Mass[27] -> m27, Mass[43] -> m43,
  Mass[68] -> m68, Mass[92] -> m92, Mass[105] -> m105};
indexedSum = SpinorChain[Spinor["Spin", "Up", "Angle", 11, 7],
  Mom[Multiparticle[27, 43, 68, 92]], Spinor["Spin", "Down", "Square", 105, 9]];
manySplits = Select[sumMoves[indexedSum, manyRules], #["Direction"] === "Expand" &];
check["multi particle bundle has bounded splits", 0 < Length[manySplits] <= 4];
check["multi particle labels and little group indices retained", AllTrue[manySplits,
  sameMomenta[indexedSum, apply[indexedSum, #]] &&
    !FreeQ[apply[indexedSum, #], Spinor["Spin", "Up", "Angle", 11, 7]] &&
    !FreeQ[apply[indexedSum, #], Spinor["Spin", "Down", "Square", 105, 9]] &]];
check["multi particle splits have exact reverses", manySplits =!= {} &&
  AllTrue[manySplits, reverseSumQ[indexedSum, #, manyRules] &]];

Do[
  expr = sample;
  compositeMoves = sumMoves[expr];
  check["composite polynomial identity " <> ToString[index], compositeMoves =!= {} &&
    AllTrue[compositeMoves, sameMomenta[expr, apply[expr, #]] &]];
  check["composite polynomial reverses " <> ToString[index], compositeMoves =!= {} &&
    AllTrue[compositeMoves, reverseSumQ[expr, #] &]];
  check["composite polynomial retains denominator " <> ToString[index],
    AllTrue[compositeMoves, !FreeQ[apply[expr, #], den] &]],
  {index, 3},
  {sample, {3 chain[Mom[Multiparticle[2, 3]]]^2/den,
    chain[Mom[Multiparticle[2, 3]], Mom[1]]/den,
    chain[Mom[Multiparticle[2, 3]], Mom[Multiparticle[1, 4]]]/den}[[{index}]]}
];
check["propagator only produces no momentum sum edits", sumMoves[1/den] === {}];
check["canonical multilinearity leaves propagator untouched",
  ConstructiveDiagrams`Private`canonicalChains[1/PropDen[Mom[2] + Mom[3], mz]] ===
    1/PropDen[Mom[2] + Mom[3], mz]];

(* Scalar relations use the same supplied external legs and internal poles as
   chain edits. No relations are applied within a propagator. *)
threeRules = Append[rules, Mass[Multiparticle[1, 2, 3]] -> mq];
q3 = Mom[Multiparticle[1, 2, 3]];
qValue = m1^2 + m2^2 + m3^2 + 2 MomProd[1, 2] + 2 MomProd[1, 3] + 2 MomProd[2, 3];
qChain = chain[q3, q3];
qContract = Select[moves[qChain, threeRules], #["Name"] === "ChainSquare" && #["Direction"] === "Contract" &];
check["three-leg internal chain uses its supplied pole mass",
  AnyTrue[qContract, same[apply[qChain, #], mq^2 chain[]] &]];
qDisabled = SpinorIdentityCandidates[{threeRules, qChain}, "OnShellChannels" -> {}, "MomentumConservation" -> False];
check["three-leg chain stays off shell when channel disabled", AnyTrue[
  Select[qDisabled, #["Name"] === "ChainSquare" && #["Direction"] === "Contract" &],
  same[apply[qChain, #], q3^2 chain[]] &]];
check["three-leg shell selection accepts permuted channel labels", AnyTrue[
  SpinorIdentityCandidates[{threeRules, q3^2 chain[]}, "OnShellChannels" -> {{3, 1, 2}}, "MomentumConservation" -> False],
  same[apply[q3^2 chain[], #], mq^2 chain[]] &]];
qSquareMoves = moves[q3^2 chain[], threeRules];
check["three-leg scalar square expands into all pairwise invariants", AnyTrue[qSquareMoves,
  same[apply[q3^2 chain[], #], qValue chain[]] &]];
check["three-leg scalar square contracts on shell", AnyTrue[qSquareMoves,
  same[apply[q3^2 chain[], #], mq^2 chain[]] &]];
check["three-leg square expansion is reversible off shell", AnyTrue[
  SpinorIdentityCandidates[{threeRules, qValue chain[]}, "OnShellChannels" -> {}, "MomentumConservation" -> False],
  same[apply[qValue chain[], #], q3^2 chain[]] &]];
check["three-leg scalar sum can contract directly to pole mass", AnyTrue[moves[qValue chain[], threeRules],
  same[apply[qValue chain[], #], mq^2 chain[]] &]];
check["three-leg scalar mass has direct square and invariant reverses",
  AnyTrue[moves[mq^2 chain[], threeRules], same[apply[mq^2 chain[], #], q3^2 chain[]] &] &&
  AnyTrue[moves[mq^2 chain[], threeRules], same[apply[mq^2 chain[], #], qValue chain[]] &]];
check["three-leg scalar square power contracts one square", AnyTrue[moves[q3^4 chain[], threeRules],
  same[apply[q3^4 chain[], #], mq^2 q3^2 chain[]] &]];
check["three-leg zero-mass pole contracts without division", AnyTrue[moves[q3^2 chain[], threeRules /. mq -> 0],
  apply[q3^2 chain[], #] === 0 &] && FreeQ[moves[chain[], threeRules /. mq -> 0], Indeterminate | ComplexInfinity]];
check["disabled three-leg shell has no pole-mass substitution", FreeQ[
  Select[SpinorIdentityCandidates[{threeRules, q3^2 chain[]}, "OnShellChannels" -> {}, "MomentumConservation" -> False],
    #["Name"] === "OnShell" &], mq]];
manyInternal = Append[manyRules, Mass[Multiparticle[92, 27, 68, 43]] -> mq4];
manyQ = Mom[Multiparticle[27, 43, 68, 92]];
manySquareChain = SpinorChain[Spinor["Spin", "Up", "Angle", 11, 7], manyQ, manyQ,
  Spinor["Spin", "Down", "Angle", 105, 9]];
manyTarget = mq4^2 SpinorChain[Spinor["Spin", "Up", "Angle", 11, 7], Spinor["Spin", "Down", "Angle", 105, 9]];
check["four-leg subset mass works with sparse labels and explicit indices", AnyTrue[moves[manySquareChain, manyInternal],
  same[apply[manySquareChain, #], manyTarget] &]];

scalarDot = MomProd[1, 2] chain[];
scalarDotTarget = (-m1^2 - MomProd[1, 3] - MomProd[1, 4]) chain[];
scalarChoices = Select[SpinorIdentityCandidates[{rules, scalarDot}], #["Name"] === "MomentumConservation" &];
check["scalar dot momentum conservation", AnyTrue[scalarChoices, same[apply[scalarDot, #], scalarDotTarget] &]];
check["scalar dot conservation has exact reverse", AnyTrue[SpinorIdentityCandidates[{rules, scalarDotTarget}],
  same[apply[scalarDotTarget, #], scalarDot] &]];
check["scalar conservation respects disabled option", FreeQ[Lookup[moves[scalarDot], "Name"], "MomentumConservation"]];
scalarRow = (MomProd[1, 2] + MomProd[1, 3] + MomProd[1, 4]) chain[];
check["complete scalar row contracts", AnyTrue[SpinorIdentityCandidates[{rules, scalarRow}],
  same[apply[scalarRow, #], -m1^2 chain[]] &]];
check["scalar row contraction is reversible", AnyTrue[SpinorIdentityCandidates[{rules, -m1^2 chain[]}],
  same[apply[-m1^2 chain[], #], scalarRow] &]];
manyDot = MomProd[11, 27] chain[];
manyDotTarget = (-m11^2 - Total[MomProd[11, #] & /@ {43, 68, 92, 105}]) chain[];
check["scalar conservation supports six sparse external labels", AnyTrue[SpinorIdentityCandidates[{manyRules, manyDot}],
  same[apply[manyDot, #], manyDotTarget] &]];
unknownDen = PropDen[opaque[Mom[1], MomProd[1, 2], chain[Mom[2], Mom[3]]], unknownMass];
check["propagator-only scalar and chain identities are never proposed",
  SpinorIdentityCandidates[{threeRules, 1/unknownDen}] === {}];
protectedScalar = q3^2 chain[]/unknownDen;
check["all new scalar edits preserve unknown propagator contents", AllTrue[
  SpinorIdentityCandidates[{threeRules, protectedScalar}],
  DeleteDuplicates[Cases[apply[protectedScalar, #], _PropDen, Infinity]] === {unknownDen} &]];
protectedConservation = scalarDot/unknownDen;
check["scalar momentum conservation preserves unknown propagator contents", AllTrue[
  SpinorIdentityCandidates[{rules, protectedConservation}],
  DeleteDuplicates[Cases[apply[protectedConservation, #], _PropDen, Infinity]] === {unknownDen} &]];
placeholderDen = PropDen[opaque[Mom[0], Mom[1]], unknownMass];
protectedRow = (chain[Mom[1]] + chain[Mom[2]] + chain[Mom[3]])/placeholderDen;
rowChoices = Select[SpinorIdentityCandidates[{rules, protectedRow}],
  #["Name"] === "MomentumConservation" &];
check["chain collection placeholder never enters unknown propagator", rowChoices =!= {} && AllTrue[rowChoices,
  DeleteDuplicates[Cases[apply[protectedRow, #], _PropDen, Infinity]] === {placeholderDen} &]];
overlapDen = PropDen[opaque[Mom[1], Mass[1]], unknownMass];
overlapChain = chain[Mom[Multiparticle[1, 2]], Mom[Multiparticle[1, 3]]]/overlapDen;
overlapChoices = Select[moves[overlapChain], #["Name"] === "Anticommutation" &];
overlapTarget = (2 (m1^2 + MomProd[1, 2] + MomProd[1, 3] + MomProd[2, 3]) chain[] -
  chain[Mom[Multiparticle[1, 3]], Mom[Multiparticle[1, 2]]])/overlapDen;
check["overlapping composite anticommutation applies known external masses", overlapChoices =!= {} &&
  AnyTrue[overlapChoices, same[apply[overlapChain, #], overlapTarget] &] && AllTrue[overlapChoices,
    FreeQ[Expand[apply[overlapChain, #] overlapDen], _Mass] &]];
check["overlapping composite anticommutation preserves unknown propagator", AllTrue[overlapChoices,
  DeleteDuplicates[Cases[apply[overlapChain, #], _PropDen, Infinity]] === {overlapDen} &]];
vectors = <|1 -> {1, 2, 3}, 2 -> {2, -1, 1}, 3 -> {-1, 3, 2}, 4 -> {-2, -4, -6}|>;
scalarNumeric[ex_] := Expand[ex /. {
  MomProd[i_Integer, j_Integer] :> vectors[i].vectors[j],
  Power[Mom[Multiparticle[ii__Integer]], 2] :> With[{v = Total[vectors /@ {ii}]}, v.v],
  chain[] -> 1} /. Thread[{m1, m2, m3, m4} ->
    Sqrt[(#. #) & /@ Values[vectors]]] /. mq -> Sqrt[Total[Values[vectors][[1 ;; 3]]].Total[Values[vectors][[1 ;; 3]]]]];
check["scalar dot conservation independently agrees with exact conserved vectors", AllTrue[
  Select[scalarChoices, StringStartsQ[Lookup[#, "Identity", ""], "Scalar"] &],
  scalarNumeric[scalarDot - apply[scalarDot, #]] === 0 &]];
check["subset square identities independently agree with exact conserved vectors", AllTrue[
  Select[qSquareMoves, MemberQ[{"ExpandSubset", "SubsetSquare"}, Lookup[#, "Identity", ""]] &],
  scalarNumeric[q3^2 chain[] - apply[q3^2 chain[], #]] === 0 &]];

(* ::Section:: *)
(* Typed particle permutations, including sparse labels and explicit indices. *)
indexedPair = {{Mass[10] -> Me, Mass[20] -> Mm, Mass[30] -> MW,
    Mass[Multiparticle[10, 20]] -> MZ},
  7 SpinorChain[Spinor["Spin", "Angle", 10, 2], Mom[20], Spinor["Spin", "Square", 30, 1]]^2 +
    MomProd[10, 20] Mandelstahm[20, 30]/PropDen[Mom[Multiparticle[10, 20]], MZ]};
ids = <|10 -> 30, 20 -> 10, 30 -> 20|>;
relabeled = ConstructiveDiagrams`Private`relabelLegs[indexedPair, ids];
check["consistent typed relabeling", relabeled === {
  {Mass[30] -> Me, Mass[10] -> Mm, Mass[20] -> MW, Mass[Multiparticle[30, 10]] -> MZ},
  7 SpinorChain[Spinor["Spin", "Angle", 30, 2], Mom[10], Spinor["Spin", "Square", 20, 1]]^2 +
    MomProd[30, 10] Mandelstahm[10, 20]/PropDen[Mom[Multiparticle[30, 10]], MZ]}];
check["relabeling round trip", ConstructiveDiagrams`Private`relabelLegs[relabeled,
  AssociationThread[Values[ids], Keys[ids]]] === indexedPair];

(* ::Section:: *)
(* Massless invariants, rational numerator algebra, and momentum conservation. *)
(* Massless completeness and off-shell common-denominator algebra support the
   two-photon route without treating a propagator as an on-shell numerator. *)
photonMasses = {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> 0, Mass[4] -> 0};
photonSquare[i_, j_] := SpinorChain[Spinor["Helicity", "Square", i], Spinor["Helicity", "Square", j]];
photonSandwich[side_, i_, p_, j_] := SpinorChain[Spinor["Helicity", side, i], Mom[p],
  Spinor["Helicity", If[side === "Angle", "Square", "Angle"], j]];
photonEdits[expr_, identity_, masses_: photonMasses] := Select[
  ConstructiveDiagrams`Private`helicityInvariantMoves[ConstructiveDiagrams`Private`canonicalChains[expr], masses],
  Lookup[#, "Identity", None] === identity &];
Do[
  photonState = ConstructiveDiagrams`Private`canonicalChains[7 photonSandwich[side, 3, 2, 3]];
  photonMoves = photonEdits[photonState, "HelicityTrace"];
  check["closed massless sandwich exact " <> side, Length[photonMoves] === 1 &&
    ConstructiveDiagrams`Private`samePoly[ConstructiveDiagrams`Private`applyCandidate[photonState, First[photonMoves]],
      14 MomProd[2, 3]]],
  {side, {"Angle", "Square"}}];
photonState = ConstructiveDiagrams`Private`canonicalChains[photonSandwich["Angle", 3, 2, 3]^2];
photonMoves = photonEdits[photonState, "HelicityTrace"];
check["closed sandwich reduces one positive factor at a time", photonMoves =!= {} &&
  ConstructiveDiagrams`Private`samePoly[ConstructiveDiagrams`Private`applyCandidate[photonState, First[photonMoves]],
    2 MomProd[2, 3] photonSandwich["Angle", 3, 2, 3]]];
check["reciprocal sandwich is not a positive contraction", photonEdits[
  1/photonSandwich["Angle", 3, 2, 3], "HelicityTrace"] === {}];
check["different massless endpoints are not a closed trace", photonEdits[
  photonSandwich["Angle", 3, 2, 4], "HelicityTrace"] === {}];
check["massive spin endpoints are not helicity traces", photonEdits[
  SpinorChain[Spinor["Spin", "Angle", 1], Mom[2], Spinor["Spin", "Square", 1]],
  "HelicityTrace"] === {}];
check["nonzero mass helicity fixture is rejected", photonEdits[
  photonSandwich["Angle", 1, 2, 1], "HelicityTrace"] === {}];
check["unknown endpoint mass is rejected", photonEdits[
  photonSandwich["Angle", 9, 2, 9], "HelicityTrace"] === {}];
photonMoves = photonEdits[photonSandwich["Angle", 3, 3, 3], "HelicityTrace"];
check["own massless momentum does not introduce an unreduced self dot", AllTrue[photonMoves,
  ConstructiveDiagrams`Private`samePoly[#["Insert"], 0] &]];
check["nonzero endpoint indices are not implicitly contracted", photonEdits[
  SpinorChain[Spinor["Helicity", "Angle", 3, 1], Mom[2], Spinor["Helicity", "Square", 3, 2]],
  "HelicityTrace"] === {}];
photonState = 6 MomProd[3, 4];
photonMoves = photonEdits[photonState, "MasslessBracket"];
check["massless invariant expands to opposite ordered brackets", AnyTrue[photonMoves,
  ConstructiveDiagrams`Private`samePoly[ConstructiveDiagrams`Private`applyCandidate[photonState, #],
    ConstructiveDiagrams`Private`canonicalChains[3 ang[3, 4] photonSquare[4, 3]]] &]];
photonState = ConstructiveDiagrams`Private`canonicalChains[3 ang[3, 4] photonSquare[4, 3]];
photonMoves = photonEdits[photonState, "MasslessBracket"];
check["massless bracket pair contracts to invariant", AnyTrue[photonMoves,
  ConstructiveDiagrams`Private`samePoly[ConstructiveDiagrams`Private`applyCandidate[photonState, #], 6 MomProd[3, 4]] &]];
check["massive invariant does not acquire massless bracket identity",
  photonEdits[MomProd[1, 3], "MasslessBracket"] === {}];
check["inverse invariant is not expanded", photonEdits[1/MomProd[3, 4], "MasslessBracket"] === {}];
photonDt = PropDen[Mom[Multiparticle[1, 3]], Me];
photonDu = PropDen[Mom[Multiparticle[1, 4]], Me];
photonState = ang[3, 4]/photonDt + 2 ang[3, 4]/photonDu;
photonMoves = ConstructiveDiagrams`Private`propagatorNumeratorMoves[photonState, photonMasses];
photonSubsetSquare[ii_List] := Total[(Mass[#] /. photonMasses)^2 & /@ ii] +
  2 Total[(MomProd @@ Sort[#]) & /@ Subsets[ii, {2}]];
photonOffShell[expr_] := expr /. {
  HoldPattern[PropDen[Mom[Multiparticle[ii__Integer]], m_]] :> photonSubsetSquare[{ii}] - m^2,
  HoldPattern[Mom[Multiparticle[ii__Integer]]^2] :> photonSubsetSquare[{ii}]};
check["common-denominator numerator moves are available", photonMoves =!= {}];
check["propagator numerator moves are off-shell rational identities", AllTrue[photonMoves,
  Together[photonOffShell[ConstructiveDiagrams`Private`applyCandidate[photonState, #] - photonState]] === 0 &]];
check["common-denominator moves retain both original propagator atoms", AllTrue[photonMoves,
  With[{result = ConstructiveDiagrams`Private`applyCandidate[photonState, #]},
    !FreeQ[result, photonDt] && !FreeQ[result, photonDu] &&
      Sort[DeleteDuplicates[Cases[result, _PropDen, Infinity]]] === Sort[{photonDt, photonDu}]] &]];
check["one pole is not needlessly recombined", ConstructiveDiagrams`Private`propagatorNumeratorMoves[
  ang[3, 4]/photonDt, photonMasses] === {}];
check["width-dependent propagators are not given the zero-width numerator identity",
  ConstructiveDiagrams`Private`propagatorNumeratorMoves[
    ang[3, 4]/PropDen[Mom[Multiparticle[1, 3]], Me, width] +
      ang[3, 4]/PropDen[Mom[Multiparticle[1, 4]], Me, width], photonMasses] === {}];
check["mixed width groups remain unchanged unless every numerator pole is supported",
  ConstructiveDiagrams`Private`propagatorNumeratorMoves[
    ang[3, 4]/photonDt + ang[3, 4]/PropDen[Mom[Multiparticle[1, 4]], Me, width],
    photonMasses] === {}];
check["unknown external masses do not produce an off-shell numerator",
  ConstructiveDiagrams`Private`propagatorNumeratorMoves[
    ang[3, 4]/PropDen[Mom[Multiparticle[1, 9]], Me] +
      ang[3, 4]/PropDen[Mom[Multiparticle[1, 10]], Me], photonMasses] === {}];
Do[
  photonIds = AssociationThread[Range[4], permutation];
  photonPermuted = ConstructiveDiagrams`Private`relabelLegs[{photonMasses,
    photonSandwich["Angle", 3, 2, 3]}, photonIds];
  photonMoves = photonEdits[photonPermuted[[2]], "HelicityTrace", photonPermuted[[1]]];
  photonExpected = ConstructiveDiagrams`Private`relabelLegs[2 MomProd[2, 3], photonIds] /.
    MomProd[i_Integer, j_Integer] :> MomProd @@ Sort[{i, j}];
  check["massless completeness follows sparse labels " <> ToString[permutation],
    Length[photonMoves] === 1 && ConstructiveDiagrams`Private`samePoly[
      ConstructiveDiagrams`Private`applyCandidate[photonPermuted[[2]], First[photonMoves]], photonExpected]],
  {permutation, {{1, 2, 3, 4}, {4, 3, 2, 1}, {10, 30, 20, 40}}}];
photonForwardOnly = photonEdits[photonSandwich["Angle", 3, 2, 3], "HelicityTrace"];
photonPropagatorOnly = ConstructiveDiagrams`Private`propagatorNumeratorMoves[
  ang[3, 4]/photonDt + 2 ang[3, 4]/photonDu, photonMasses];
check["forward-only curated identities are excluded from random scrambling",
  photonForwardOnly =!= {} && photonPropagatorOnly =!= {} && AllTrue[
    Join[photonForwardOnly, photonPropagatorOnly],
    Lookup[#, "ScrambleEligible", True] === False &]];
check["forward-only identities remain legal in the public identity API", AnyTrue[
  SpinorIdentityCandidates[{photonMasses, photonSandwich["Angle", 3, 2, 3]}],
  Lookup[#, "Identity", None] === "HelicityTrace" &] && AnyTrue[
  SpinorIdentityCandidates[{photonMasses, ang[3, 4]/photonDt + 2 ang[3, 4]/photonDu}],
  Lookup[#, "Identity", None] === "PropagatorNumerator" &]];
photonEligible = photonEdits[MomProd[3, 4], "MasslessBracket"];
Block[{ConstructiveDiagrams`Private`legalMoves},
  ConstructiveDiagrams`Private`legalMoves[_, _] := photonForwardOnly;
  photonScramble = ComplicateAmplitude[{photonMasses, photonSandwich["Angle", 3, 2, 3]}, 3,
    RandomSeed -> 123, "RecordSteps" -> True];
  check["scramble stops cleanly when only one-way edits remain",
    photonScramble["Steps"] === 0 && photonScramble["Trajectory"] === {} &&
      photonScramble["Expression"] === photonScramble["Original"]];
  ConstructiveDiagrams`Private`legalMoves[_, _] := Join[photonForwardOnly, photonEligible];
  photonScramble = ComplicateAmplitude[{photonMasses, MomProd[3, 4]}, 1,
    RandomSeed -> 123, "RecordSteps" -> True];
  check["scramble still samples the reversible identity beside excluded edits",
    photonScramble["Steps"] === 1 && Length[photonScramble["Trajectory"]] === 1 &&
      photonScramble["Trajectory"][[1, "Candidate", "Identity"]] === "MasslessBracket"];
];
(* Numerator-only substitutions act on existing summands without expanding. *)
numeratorDt = s - mass^2;
numeratorDu = t - mass^2;
check["numerator substitutions do not combine separate poles",
  ConstructiveDiagrams`applyToNumerator[a/numeratorDt + b/numeratorDu,
    {s -> mass^2, t -> mass^2}] === a/numeratorDt + b/numeratorDu];
check["matching numerator and denominator symbols only changes numerator",
  ConstructiveDiagrams`applyToNumerator[x^2/(x + y), x -> z] === z^2/(x + y)];
check["factored numerator sums remain factored",
  ConstructiveDiagrams`applyToNumerator[c (x + y)/p,
    {x -> u, y -> v, p -> 0}] === c (u + v)/p];
check["rules can match a whole unexpanded numerator sum",
  ConstructiveDiagrams`applyToNumerator[(x + y)/p, x + y -> z] === z/p];
check["polynomial numerator powers are not expanded to expose new matches",
  ConstructiveDiagrams`applyToNumerator[(x + y)^2/p, x^2 -> z] ===
    (x + y)^2/p];
check["whole numerator identities apply to each existing top-level term",
  ConstructiveDiagrams`applyToNumerator[(x + y)^2/p + (x + y)/q,
    x + y -> z] === z^2/p + z/q];
check["rational numerical denominators are preserved",
  ConstructiveDiagrams`applyToNumerator[3 x/7 + 2 y/5, {x -> u, y -> v}] ===
    3 u/7 + 2 v/5];
check["numerator powers are replaced without touching denominator powers",
  ConstructiveDiagrams`applyToNumerator[x^3/(x + y)^2, x -> z] === z^3/(x + y)^2];
check["denominator-free expressions accept numerator identities",
  ConstructiveDiagrams`applyToNumerator[x^2 + y, x -> z] === z^2 + y];
check["zero has an ordinary zero numerator",
  ConstructiveDiagrams`applyToNumerator[0, x -> z] === 0];
check["delayed numerator rules are supported",
  ConstructiveDiagrams`applyToNumerator[MomProd[1, 2]/p + MomProd[2, 3]/q,
    MomProd[i_, j_] :> i + j] === 3/p + 5/q];
numeratorFactored = (x + y) (u + v)/(p + q);
check["empty numerator rules preserve the exact factored expression",
  ConstructiveDiagrams`applyToNumerator[numeratorFactored, {}] === numeratorFactored];
check["identities matching a whole denominator atom never apply to it",
  ConstructiveDiagrams`applyToNumerator[x/f[x], f[_] -> 0] === x/f[x]];
numeratorPropagator = PropDen[Mom[Multiparticle[1, 3]], Mass[1] + Mass[2]];
numeratorMomentum = Mom[Multiparticle[1, 3]];
check["composite momentum substitutions keep propagator contents unchanged",
  ConstructiveDiagrams`applyToNumerator[numeratorMomentum/numeratorPropagator^2,
    Mom[Multiparticle[i_, j_]] :> Mom[i] + Mom[j]] ===
    (Mom[1] + Mom[3])/numeratorPropagator^2];
check["numerator mass substitutions do not put propagators on shell",
  ConstructiveDiagrams`applyToNumerator[Mass[1] numeratorMomentum/numeratorPropagator,
    {Mass[_] -> Me, Mom[Multiparticle[i_, j_]] :> Mom[i] + Mom[j]}] ===
    Me (Mom[1] + Mom[3])/numeratorPropagator];
check["inverse sum contents are not expanded into new denominator factors",
  ConstructiveDiagrams`applyToNumerator[x/((p + q)^2 (r + s)),
    {p -> 0, q -> 0, r -> 0, s -> 0}] === x/((p + q)^2 (r + s))];
check["numerator API has the public package context",
  Context[ConstructiveDiagrams`applyToNumerator] === "ConstructiveDiagrams`"];
check["numerator API is protected after package loading",
  MemberQ[Attributes[ConstructiveDiagrams`applyToNumerator], Protected]];
(* Only this deliberately colliding notebook-helper fixture suppresses shadows. *)
Quiet[ToExpression["Global`applyToNumerator[_, _] := \"notebook helper\""]];
Quiet[Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]]];
check["package reload retains its public numerator implementation",
  ConstructiveDiagrams`applyToNumerator[x/p + y/q, {x -> u, y -> v}] === u/p + v/q];
check["package reload does not alter an existing notebook helper",
  Global`applyToNumerator[x/p, {}] === "notebook helper"];
check["package reload restores numerator API protection",
  MemberQ[Attributes[ConstructiveDiagrams`applyToNumerator], Protected]];
ClearAll[Global`applyToNumerator];

complementEdits[expr_, rules_List] := Block[{ConstructiveDiagrams`Private`$useMomentumConservation = True},
  Select[ConstructiveDiagrams`Private`scalarMomentumConservationMoves[expr, rules],
    Lookup[#, "Identity", None] === "ScalarComplement" &]];
complementFourMasses = {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> 0, Mass[4] -> 0};
complementState = MomProd[1, 2];
complementMoves = complementEdits[complementState, complementFourMasses];
check["complementary invariant is exact for four external particles", Length[complementMoves] === 1 &&
  First[complementMoves]["Name"] === "MomentumConservation" &&
  First[complementMoves]["Legs"] === {1, 2} && ConstructiveDiagrams`Private`samePoly[
    First[complementMoves]["Insert"], MomProd[3, 4] - Me^2]];
check["complementary invariant does not require internal on-shell channels", Block[
  {ConstructiveDiagrams`Private`$onShellChannels = {}},
  complementEdits[complementState, complementFourMasses] === complementMoves]];
complementFiveMasses = {Mass[1] -> m1, Mass[2] -> m2, Mass[3] -> m3,
  Mass[4] -> m4, Mass[5] -> m5};
complementMoves = complementEdits[complementState, complementFiveMasses];
complementExpected = (m3^2 + m4^2 + m5^2 - m1^2 - m2^2)/2 +
  MomProd[3, 4] + MomProd[3, 5] + MomProd[4, 5];
check["five-particle complementary invariant includes every pair in its momentum sum",
  Length[complementMoves] === 1 && ConstructiveDiagrams`Private`samePoly[
    First[complementMoves]["Insert"], complementExpected]];
complementPermutation = AssociationThread[Range[5], {10, 30, 40, 50, 80}];
complementPermutedMasses = ConstructiveDiagrams`Private`relabelLegs[complementFiveMasses, complementPermutation];
complementPermutedState = ConstructiveDiagrams`Private`relabelLegs[complementState, complementPermutation];
complementMoves = complementEdits[complementPermutedState, complementPermutedMasses];
check["complementary invariant supports sparse higher-point labels", Length[complementMoves] === 1 &&
  First[complementMoves]["Legs"] === {10, 30} && ConstructiveDiagrams`Private`samePoly[
    First[complementMoves]["Insert"], ConstructiveDiagrams`Private`relabelLegs[
      complementExpected, complementPermutation]]];
complementPropagator = PropDen[Mom[Multiparticle[1, 3]], Me];
complementPowered = 3 MomProd[1, 2]^2 ang[1, 2]/complementPropagator^2;
complementMoves = complementEdits[complementPowered, complementFourMasses];
check["complementary invariant replaces one positive dot factor and preserves powers",
  Length[complementMoves] === 1 && ConstructiveDiagrams`Private`samePoly[
    First[complementMoves]["Insert"],
    3 MomProd[1, 2] (MomProd[3, 4] - Me^2) ang[1, 2]/complementPropagator^2]];
check["complementary invariant does not rewrite inverse dot factors",
  complementEdits[1/MomProd[1, 2], complementFourMasses] === {}];
check["complementary invariant does not enter propagator contents", complementEdits[
  1/PropDen[MomProd[1, 2] + Mom[Multiparticle[1, 3]], Me], complementFourMasses] === {}];
check["complementary invariant retains opaque propagator atoms", AllTrue[complementMoves,
  DeleteDuplicates[Cases[#["Insert"], _PropDen, Infinity]] === {complementPropagator} &]];
check["complementary invariant skips unknown external masses", complementEdits[
  complementState, ReplacePart[complementFourMasses, 4 -> (Mass[4] -> Missing["Mass"])]] === {}];
check["complementary invariant skips dots outside the external mass list", complementEdits[
  MomProd[1, 9], complementFourMasses] === {}];
check["complementary invariant respects disabled momentum conservation", Block[
  {ConstructiveDiagrams`Private`$useMomentumConservation = False},
  ConstructiveDiagrams`Private`scalarMomentumConservationMoves[complementState, complementFourMasses] === {}]];
complementMoves = complementEdits[complementPermutedState, complementPermutedMasses];
check["complementary invariant is available through the public candidate board", AnyTrue[
  SpinorIdentityCandidates[{complementFourMasses, complementState}, "OnShellChannels" -> {},
    "MomentumConservation" -> True], Lookup[#, "Identity", None] === "ScalarComplement" &]];
check["complementary invariants are excluded from random scrambling",
  complementMoves =!= {} && AllTrue[complementMoves,
    Lookup[#, "ScrambleEligible", True] === False &]];
Block[{ConstructiveDiagrams`Private`legalMoves},
  ConstructiveDiagrams`Private`legalMoves[_, _] := complementMoves;
  complementScramble = ComplicateAmplitude[{complementPermutedMasses, complementPermutedState}, 3,
    RandomSeed -> 123, "RecordSteps" -> True];
  check["random scrambling skips a complementary-invariant-only board",
    complementScramble["Steps"] === 0 && complementScramble["Trajectory"] === {} &&
      complementScramble["Expression"] === complementScramble["Original"]];
];
(* Momentum conservation is linear in one chain/trace momentum slot, not in
   a power or reciprocal of the complete chain. Dot-product rows are linear. *)
conservationClosed[i_] := SpinorChain[Spinor["Helicity", "Angle", 4], Mom[i],
  Spinor["Helicity", "Square", 4]];
conservationMasslessRules = Table[Mass[i] -> 0, {i, 4}];
conservationCollected[expr_, rules_List] := Block[
  {ConstructiveDiagrams`Private`$useMomentumConservation = True,
    ConstructiveDiagrams`Private`$onShellChannels = {}},
  Select[ConstructiveDiagrams`Private`kinematicMoves[ConstructiveDiagrams`Private`canonicalChains[expr], rules],
    #["Name"] === "MomentumConservation" && KeyExistsQ[#, "Momentum"] &]];
conservationSquaredSum = Total[conservationClosed[#]^2 & /@ {1, 2, 3}];
conservationInverseSum = Total[1/conservationClosed[#] & /@ {1, 2, 3}];
check["conservation collection rejects squared whole-chain contexts",
  conservationCollected[conservationSquaredSum, conservationMasslessRules] === {}];
check["conservation collection rejects reciprocal whole-chain contexts",
  conservationCollected[conservationInverseSum, conservationMasslessRules] === {}];
conservationSquaredMoves = SpinorIdentityCandidates[
  {conservationMasslessRules, conservationSquaredSum}, "OnShellChannels" -> {}];
check["public candidates omit the false sum-of-squared-chains identity", !AnyTrue[
  conservationSquaredMoves, #["Name"] === "MomentumConservation" &&
    ConstructiveDiagrams`Private`samePoly[#["Subtract"], conservationSquaredSum] &&
    ConstructiveDiagrams`Private`samePoly[#["Insert"], -conservationClosed[4]^2] &]];
check["public candidates omit the false sum-of-reciprocal-chains identity", !AnyTrue[
  SpinorIdentityCandidates[{conservationMasslessRules, conservationInverseSum}, "OnShellChannels" -> {}],
  #["Name"] === "MomentumConservation" &&
    ConstructiveDiagrams`Private`samePoly[#["Subtract"], conservationInverseSum] &&
    ConstructiveDiagrams`Private`samePoly[#["Insert"], -1/conservationClosed[4]] &]];
conservationMomenta = {{1, 0, 0, 1}, {1, 0, 0, -1}, {-1, -1, 0, 0}, {-1, 1, 0, 0}};
conservationDot[a_, b_] := a[[1]] b[[1]] - Rest[a].Rest[b];
conservationNumericalChains = Table[conservationClosed[i] ->
  2 conservationDot[conservationMomenta[[4]], conservationMomenta[[i]]], {i, 4}];
conservationNumericalValue[expr_] := expr /. HoldPattern[
    SpinorChain[Spinor["Helicity", "Angle", 4], Mom[label_], Spinor["Helicity", "Square", 4]]] :>
  2 conservationDot[conservationMomenta[[4]],
    If[IntegerQ[label], conservationMomenta[[label]],
      Total[conservationMomenta[[List @@ label]]]]];
check["nonlinear regression fixture conserves all-ingoing massless momenta",
  Total[conservationMomenta] === {0, 0, 0, 0} &&
    (conservationDot[#, #] & /@ conservationMomenta) === {0, 0, 0, 0}];
check["independent physical fixture distinguishes linear and squared conservation",
  (Total[conservationClosed /@ Range[4]] /. conservationNumericalChains) === 0 &&
    (conservationSquaredSum /. conservationNumericalChains) === 24 &&
    (-conservationClosed[4]^2 /. conservationNumericalChains) === 0];
conservationScramble = ComplicateAmplitude[{conservationMasslessRules, conservationSquaredSum}, 1,
  RandomSeed -> 18, "RecordSteps" -> True, "OnShellChannels" -> {}];
check["former failing random seed now preserves the physical amplitude",
  Length[conservationScramble["Trajectory"]] === 1 &&
    Simplify[conservationNumericalValue[conservationScramble["Expression"]]] === 24];

conservationChain[i_] := SpinorChain[Spinor["Spin", "Angle", 1], Mom[i],
  Spinor["Spin", "Square", 2]];
conservationTrace[i_] := SpinorTrace[Mom[1], Mom[i]];
conservationFactor = 13 SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]];
conservationDenominator = PropDen[Mom[Multiparticle[1, 2]], MZ];
conservationSharedChain = conservationChain[1] Total[conservationChain /@ {1, 2, 3}];
conservationMoves = conservationCollected[conservationSharedChain, complementFourMasses];
check["linear collection preserves a shared positive chain multiplier",
  AnyTrue[conservationMoves, ConstructiveDiagrams`Private`samePoly[
    #["Insert"], -conservationChain[1] conservationChain[4]] &]];
conservationSharedTrace = conservationTrace[1] Total[conservationTrace /@ {1, 2, 3}];
conservationMoves = conservationCollected[conservationSharedTrace, complementFourMasses];
check["linear collection preserves a shared positive trace multiplier",
  AnyTrue[conservationMoves, ConstructiveDiagrams`Private`samePoly[
    #["Insert"], -conservationTrace[1] conservationTrace[4]] &]];
Do[
  conservationRules = Take[complementFiveMasses, legCount];
  conservationIDs = AssociationThread[Range[legCount], labels];
  conservationPermutedRules = ConstructiveDiagrams`Private`relabelLegs[conservationRules, conservationIDs];
  conservationLinear = conservationFactor/conservationDenominator*
    Total[conservationChain /@ Range[legCount - 1]];
  conservationExpected = -conservationFactor/conservationDenominator*conservationChain[legCount];
  conservationMoves = conservationCollected[
    ConstructiveDiagrams`Private`relabelLegs[conservationLinear, conservationIDs], conservationPermutedRules];
  conservationSuffix = " n=" <> ToString[legCount] <> " labels=" <> ToString[labels];
  check["linear chain sums retain conservation collection" <> conservationSuffix,
    AnyTrue[conservationMoves, ConstructiveDiagrams`Private`samePoly[#["Insert"],
      ConstructiveDiagrams`Private`canonicalChains[
        ConstructiveDiagrams`Private`relabelLegs[conservationExpected, conservationIDs]]] &]];
  conservationLinear = conservationFactor/conservationDenominator*
    Total[conservationTrace /@ Range[legCount - 1]];
  conservationExpected = -conservationFactor/conservationDenominator*conservationTrace[legCount];
  conservationMoves = conservationCollected[
    ConstructiveDiagrams`Private`relabelLegs[conservationLinear, conservationIDs], conservationPermutedRules];
  check["linear trace sums retain conservation collection" <> conservationSuffix,
    AnyTrue[conservationMoves, ConstructiveDiagrams`Private`samePoly[#["Insert"],
      ConstructiveDiagrams`Private`canonicalChains[
        ConstructiveDiagrams`Private`relabelLegs[conservationExpected, conservationIDs]]] &]];
  conservationAnchor = conservationIDs[1];
  conservationRow = m1^2 + Total[MomProd[1, #] & /@ Range[2, legCount]];
  conservationRowState = ConstructiveDiagrams`Private`relabelLegs[
    conservationFactor conservationRow/conservationDenominator, conservationIDs];
  conservationRowMoves = Block[{ConstructiveDiagrams`Private`$useMomentumConservation = True,
      ConstructiveDiagrams`Private`$onShellChannels = {}},
    Select[ConstructiveDiagrams`Private`scalarMomentumConservationMoves[
      conservationRowState, conservationPermutedRules],
      Lookup[#, "Identity", None] === "ScalarRow" && #["Legs"] === {conservationAnchor} &]];
  check["complete dot-product row still contracts to zero" <> conservationSuffix,
    conservationRowMoves =!= {} && AllTrue[conservationRowMoves,
      ConstructiveDiagrams`Private`applyCandidate[conservationRowState, #] === 0 &]];
  conservationRowSource = ConstructiveDiagrams`Private`relabelLegs[
    -conservationFactor m1^2/conservationDenominator, conservationIDs];
  conservationRowExpected = ConstructiveDiagrams`Private`relabelLegs[
    conservationFactor Total[MomProd[1, #] & /@ Range[2, legCount]]/conservationDenominator,
    conservationIDs];
  conservationRowMoves = Block[{ConstructiveDiagrams`Private`$useMomentumConservation = True,
      ConstructiveDiagrams`Private`$onShellChannels = {}},
    Select[ConstructiveDiagrams`Private`scalarMomentumConservationMoves[
      conservationRowSource, conservationPermutedRules],
      Lookup[#, "Identity", None] === "ScalarRowExpand" && #["Legs"] === {conservationAnchor} &]];
  check["dot-product row expansion keeps every external momentum" <> conservationSuffix,
    AnyTrue[conservationRowMoves, ConstructiveDiagrams`Private`samePoly[
      #["Insert"], conservationRowExpected] &]];
  check["row identities preserve opaque propagator atoms" <> conservationSuffix,
    conservationRowMoves =!= {} && AllTrue[conservationRowMoves,
      DeleteDuplicates[Cases[#["Insert"], _PropDen, Infinity]] ===
        {ConstructiveDiagrams`Private`relabelLegs[conservationDenominator, conservationIDs]} &]],
  {legCount, {4, 5}}, {labels, {Range[legCount], Take[{40, 10, 80, 20, 50}, legCount]}}];

conservationSquareState = conservationClosed[1]^2;
conservationSquareInitial = conservationSquareState;
conservationSquareMoves[state_] := Block[{ConstructiveDiagrams`Private`$useMomentumConservation = True},
  Select[ConstructiveDiagrams`Private`chainMoves[state, conservationMasslessRules],
    #["Name"] === "MomentumConservation" && With[{site = #["Site"]},
      ConstructiveDiagrams`Private`chainsIn[ConstructiveDiagrams`Private`monomialsOf[state][[site[[1]]]]][[
        site[[2]], site[[3]] + 1]] === Mom[1]] &]];
conservationMoves = conservationSquareMoves[conservationSquareState];
check["squared chains keep legitimate single-slot conservation edits", conservationMoves =!= {}];
If[conservationMoves =!= {},
  conservationSquareState = ConstructiveDiagrams`Private`applyCandidate[
    conservationSquareState, First[conservationMoves]]];
check["first conservation edit of a square keeps the unedited factor",
  ConstructiveDiagrams`Private`samePoly[conservationSquareState,
    -conservationClosed[1] Total[conservationClosed /@ {2, 3, 4}]]];
Do[
  conservationMoves = conservationSquareMoves[conservationSquareState];
  If[conservationMoves === {}, Break[]];
  conservationSquareState = ConstructiveDiagrams`Private`applyCandidate[
    conservationSquareState, First[conservationMoves]], {3}];
check["successive slot conservation edits retain all square cross terms",
  ConstructiveDiagrams`Private`samePoly[conservationSquareState,
    Expand[Total[conservationClosed /@ {2, 3, 4}]^2]]];
check["squared slot conservation preserves the physical amplitude at both endpoints",
  (conservationSquareInitial /. conservationNumericalChains) === 4 &&
    (conservationSquareState /. conservationNumericalChains) === 4];
check["conservation fix still respects disabled momentum conservation", FreeQ[
  Lookup[SpinorIdentityCandidates[{conservationMasslessRules, conservationSquaredSum},
    "MomentumConservation" -> False, "OnShellChannels" -> {}], "Name"], "MomentumConservation"]];

(* ::Section:: *)
(* Chiral traces: independent Weyl-matrix evaluation. *)
rules = Table[Mass[i] -> (i + 1), {i, 5}];
Do[
  zeroBracket = SpinorChain[endpoint, endpoint];
  check["identical endpoint is zero " <> ToString[endpoint, InputForm],
    ConstructiveDiagrams`Private`canonicalChains[zeroBracket] === 0],
  {endpoint, {Spinor["Helicity", "Angle", 1], Spinor["Spin", "Square", 1],
    Spinor["Spin", "Angle", 1, 2], Spinor["Spin", 2, "Angle", 1]}}];
indexedBracket = SpinorChain[Spinor["Spin", "Angle", 1, 1], Spinor["Spin", "Angle", 1, 2]];
check["distinct little-group indices are not zero", ConstructiveDiagrams`Private`canonicalChains[indexedBracket] =!= 0];
check["Schouten normalizes newly identical endpoints", ConstructiveDiagrams`Private`samePoly[
  ConstructiveDiagrams`Private`canonicalChains[SchoutenRewrite[ang[1, 2], ang[1, 3], {0, 0}]],
  ang[1, 2] ang[1, 3]]];
protectedDenominator = PropDen[Mom[Multiparticle[1, 2]], Mass[1],
  ang[1, 1], SpinorTrace[Mom[1], Mom[1]], MomProd[1, 2]];
check["normalization never descends into PropDen", ConstructiveDiagrams`Private`canonicalAmplitude[
  protectedDenominator, rules] === protectedDenominator];
check["empty chiral trace is two", ConstructiveDiagrams`Private`canonicalChains[SpinorTrace[]] === 2];
oddTrace = SpinorTrace[Mom[1], Mom[2], Mom[3]];
check["odd trace is left unsupported", ConstructiveDiagrams`Private`canonicalChains[oddTrace] === oddTrace &&
  SpinorIdentityCandidates[{rules, oddTrace}, "MomentumConservation" -> False] === {}];
check["explicit trace momentum slots distribute", ConstructiveDiagrams`Private`samePoly[
  ConstructiveDiagrams`Private`canonicalChains[SpinorTrace[2 Mom[1] - Mom[2], Mom[3]]],
  2 SpinorTrace[Mom[1], Mom[3]] - SpinorTrace[Mom[2], Mom[3]]]];
tracePair = SpinorTrace[Mom[1], Mom[2]];
tracePairCandidates = SpinorIdentityCandidates[{rules, tracePair}, "MomentumConservation" -> False];
check["trace pair contracts to twice its scalar product", AnyTrue[tracePairCandidates,
  Lookup[#, "Identity", None] === "TracePair" && ConstructiveDiagrams`Private`samePoly[
    ConstructiveDiagrams`Private`applyCandidate[tracePair, #], 2 MomProd[1, 2]] &]];
check["scalar product has a reverse trace label", AnyTrue[
  SpinorIdentityCandidates[{rules, 2 MomProd[1, 2]}, "MomentumConservation" -> False],
  Lookup[#, "Identity", None] === "TracePair" && ConstructiveDiagrams`Private`samePoly[
    ConstructiveDiagrams`Private`applyCandidate[2 MomProd[1, 2], #], tracePair] &]];
traceSum = SpinorTrace[Mom[Multiparticle[1, 2]], Mom[3]];
traceSumCandidates = SpinorIdentityCandidates[{rules, traceSum}, "MomentumConservation" -> False];
traceSplit = SelectFirst[traceSumCandidates, Lookup[#, "Identity", None] === "Split" &, Missing["Split"]];
traceAfterSplit = If[AssociationQ[traceSplit], ConstructiveDiagrams`Private`applyCandidate[traceSum, traceSplit], $Failed];
check["composite trace split has an exact reverse", traceAfterSplit =!= $Failed && AnyTrue[
  SpinorIdentityCandidates[{rules, traceAfterSplit}, "MomentumConservation" -> False],
  Lookup[#, "Identity", None] === "Collect" && ConstructiveDiagrams`Private`samePoly[
    ConstructiveDiagrams`Private`applyCandidate[traceAfterSplit, #], traceSum] &]];
traceOverlap = SpinorTrace[Mom[Multiparticle[1, 2]], Mom[Multiparticle[1, 3]], Mom[4], Mom[5]];
traceOverlapCandidates = SpinorIdentityCandidates[{rules, traceOverlap}, "MomentumConservation" -> False];
check["overlapping composite trace contractions apply numerator masses", AllTrue[
  Select[traceOverlapCandidates, #["Name"] === "Anticommutation" &], FreeQ[#["Insert"], _Mass] &]];
subsetRules = Append[rules, Mass[Multiparticle[1, 2, 3]] -> 9];
traceSubset = SpinorTrace[Mom[Multiparticle[1, 2, 3]], Mom[Multiparticle[1, 2, 3]], Mom[4], Mom[5]];
traceSubsetCandidates = SpinorIdentityCandidates[{subsetRules, traceSubset}, "MomentumConservation" -> False];
check["three-leg on-shell trace square contracts", AnyTrue[traceSubsetCandidates,
  Lookup[#, "Identity", None] === "TraceSquare" && Lookup[#, "Direction", None] === "Contract" &&
    ConstructiveDiagrams`Private`samePoly[ConstructiveDiagrams`Private`applyCandidate[traceSubset, #],
      81 SpinorTrace[Mom[4], Mom[5]]] &]];
traceOffShell = SpinorIdentityCandidates[{subsetRules, traceSubset}, "MomentumConservation" -> False,
  "OnShellChannels" -> {}];
check["three-leg trace square respects channel filter", AnyTrue[traceOffShell,
  Lookup[#, "Identity", None] === "TraceSquare" && ConstructiveDiagrams`Private`samePoly[
    ConstructiveDiagrams`Private`applyCandidate[traceSubset, #],
    Mom[Multiparticle[1, 2, 3]]^2 SpinorTrace[Mom[4], Mom[5]]] &]];
(* Independent Weyl matrices catch chirality errors that the legacy reducer shares. *)
Module[{traceMatrixVectors, traceMatrixVector, traceMatrixDot, traceMatrixSigma,
  traceMatrixTrace, traceMatrixValue, traceMatrixRules, traceMatrixExamples,
  traceMatrixBoards, traceMatrixFeatures, traceMatrixAudit, traceMatrixCyclic},
  traceMatrixVectors = <|1 -> {2, 1, 0, 0}, 2 -> {3, 0, 1, 0}, 3 -> {4, 0, 0, 1},
    4 -> {5, 1, 1, 0}, 5 -> {7, 2, 0, 1}|>;
  traceMatrixVector[i_Integer] := traceMatrixVectors[i];
  traceMatrixVector[Multiparticle[ii__Integer]] := Total[traceMatrixVector /@ {ii}];
  traceMatrixDot[a_, b_] := With[{v = traceMatrixVector[a], w = traceMatrixVector[b]},
    2 v[[1]] w[[1]] - v.w];
  traceMatrixSigma[leg_, sign_] := With[{v = traceMatrixVector[leg]},
    {{v[[1]] + sign v[[4]], sign (v[[2]] - I v[[3]])},
      {sign (v[[2]] + I v[[3]]), v[[1]] - sign v[[4]]}}];
  traceMatrixTrace[{}] := 2;
  traceMatrixTrace[legs_List] := Tr[Dot @@ MapIndexed[
    traceMatrixSigma[#1, (-1)^(First[#2] - 1)] &, legs]];
  traceMatrixValue[expr_] := Simplify[expr /. {
    t_SpinorTrace :> traceMatrixTrace[First /@ (List @@ t)],
    MomProd[a_, b_] :> traceMatrixDot[a, b],
    HoldPattern[Mom[leg_]^2] :> traceMatrixDot[leg, leg]}];
  traceMatrixRules = Join[Mass[#] -> Sqrt[traceMatrixDot[#, #]] & /@ Range[5],
    {Mass[Multiparticle[1, 2]] ->
      Sqrt[traceMatrixDot[Multiparticle[1, 2], Multiparticle[1, 2]]]}];
  traceMatrixExamples = {
    SpinorTrace[Mom[1], Mom[2], Mom[3], Mom[4], Mom[5], Mom[1]],
    SpinorTrace[Mom[1], Mom[1], Mom[2], Mom[3]],
    SpinorTrace[Mom[Multiparticle[1, 2]], Mom[3], Mom[4], Mom[5]],
    SpinorTrace[Mom[1], Mom[2]], MomProd[1, 2]};
  traceMatrixBoards = SpinorIdentityCandidates[{traceMatrixRules, #},
    "MomentumConservation" -> False] & /@ traceMatrixExamples;
  traceMatrixAudit = Flatten[MapThread[Function[{start, board},
    Map[Function[move, If[MemberQ[
        {"ChainSquare", "Anticommutation", "MomentumSum", "MomentumSquare", "Stop"}, move["Name"]],
      {move["Name"], Lookup[move, "Identity", None], traceMatrixValue[
        start - ConstructiveDiagrams`Private`applyCandidate[start, move]]}, Nothing]], board]],
    {traceMatrixExamples, traceMatrixBoards}], 1];
  check["trace independent audit covers broad legal boards", Length[traceMatrixAudit] >= 80 &&
    ContainsAll[traceMatrixAudit[[All, 2]],
      {"TraceSquare", "TraceAnticommutation", "TraceCyclicSquare", "TracePair", "Split", "Introduce"}]];
  check["trace identities agree exactly with independent matrices", AllTrue[traceMatrixAudit, Last[#] === 0 &]];
  traceMatrixCyclic = Select[First[traceMatrixBoards],
    Lookup[#, "Identity", None] === "TraceCyclicSquare" &];
  check["six-momentum boundary contraction retains chirality", Length[traceMatrixCyclic] === 1 &&
    traceMatrixValue[First[traceMatrixExamples]] === 2628 - 6 I &&
    traceMatrixValue[First[traceMatrixCyclic]["Insert"]] === 2628 - 6 I &&
    3 traceMatrixTrace[{2, 3, 4, 5}] === 2628 + 6 I];
];

(* ::Section:: *)
(* Public identity API and reversible symbolic trajectories. *)
Module[{ids, pair, start, poles, edits, next, inverse, family, labels},
  Do[
    ids = AssociationThread[Range[5], labels];
    pair = ConstructiveDiagrams`Private`relabelLegs[
      {Join[Thread[(Mass /@ Range[5]) -> family],
        {Mass[Multiparticle[1, 2, 5]] -> poleMass}],
       Mom[Multiparticle[5, 2, 1]]^2
         SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]/
           PropDen[Mom[Multiparticle[5, 2, 1]], poleMass]}, ids];
    start = ConstructiveDiagrams`Private`canonicalAmplitude[pair[[2]], pair[[1]]];
    poles = DeleteDuplicates[Cases[pair[[2]], _PropDen, Infinity]];
    edits = Select[SpinorIdentityCandidates[pair, "MomentumConservation" -> False],
      MemberQ[{"MomentumSquare", "OnShell"}, #["Name"]] &];
    check["five-point subset square keeps both identities " <> ToString[{family, labels}],
      Sort[Lookup[edits, "Name"]] === {"MomentumSquare", "OnShell"}];
    Do[
      next = ConstructiveDiagrams`Private`applyCandidate[start, edit];
      inverse = SpinorIdentityCandidates[{pair[[1]], next}, "MomentumConservation" -> False];
      check["five-point subset square has an exact inverse " <> ToString[{family, labels, edit["Name"]}],
        AnyTrue[inverse, ConstructiveDiagrams`Private`applyCandidate[next, #] === start &]];
      check["five-point subset square leaves its pole opaque " <> ToString[{family, labels, edit["Name"]}],
        DeleteDuplicates[Cases[next, _PropDen, Infinity]] === poles &&
          ConstructiveDiagrams`Private`canonicalAmplitude[next, pair[[1]]] === next],
      {edit, edits}],
    {family, {{0, 0, 0, 0, 0}, {m, m, m, m, m}, {0, m, 0, m, 0},
      {m1, m2, m3, m4, m5}, {0, m2, m3, 0, m5}, {m1, 0, m3, m4, 0}}},
    {labels, {{1, 2, 3, 4, 5}, {5, 4, 3, 2, 1}, {3, 5, 1, 4, 2}, {11, 27, 43, 68, 92}}}]
];
sixRules = Table[Mass[i] -> 0, {i, 6}];
sixAmplitude = ang[1, 2] ang[3, 4] ang[5, 6];
sixCandidates = SpinorIdentityCandidates[{sixRules, sixAmplitude},
  "MomentumConservation" -> False];
check["six-leg legal edits retain the sixth leg", Length[sixCandidates] >= 3 &&
  AnyTrue[sixCandidates, !FreeQ[#["Subtract"], Spinor[_, _, 6]] &]];
check["identity API has no policy stop action", FreeQ[Lookup[sixCandidates, "Name"], "Stop"]];
publicPair = {{Mass[1] -> 1, Mass[2] -> 2, Mass[3] -> 3, Mass[4] -> 4},
  chain[Mom[2], Mom[2]]};
publicEdits = SpinorIdentityCandidates[publicPair, "MomentumConservation" -> False];
publicEdit = SelectFirst[publicEdits,
  #["Name"] === "ChainSquare" && Lookup[#, "Direction", ""] === "Contract" &];
check["public identity application agrees with exact edit",
  ApplySpinorIdentity[publicPair, publicEdit, "MomentumConservation" -> False] === 4 chain[]];
check["public identity application agrees with canonical edit semantics",
  ApplySpinorIdentity[publicPair, publicEdit, "MomentumConservation" -> False] ===
    ConstructiveDiagrams`Private`applyCandidate[
      ConstructiveDiagrams`Private`canonicalAmplitude[publicPair[[2]], publicPair[[1]]], publicEdit]];
check["public identity application rejects stale edits", Quiet[
  ApplySpinorIdentity[{publicPair[[1]], chain[Mom[3], Mom[3]]}, publicEdit,
    "MomentumConservation" -> False]] === $Failed];
check["public identity application rejects fabricated edits", Quiet[
  ApplySpinorIdentity[publicPair, Join[publicEdit, <|"Insert" -> wrongResult|>],
    "MomentumConservation" -> False]] === $Failed];
publicOpaque = PropDen[opaque[MomProd[2, 3], chain[Mom[2], Mom[2]]], Mass[2]];
publicOpaquePair = {publicPair[[1]], publicPair[[2]]/publicOpaque};
publicOpaqueEdit = SelectFirst[SpinorIdentityCandidates[publicOpaquePair,
  "MomentumConservation" -> False],
  #["Name"] === "ChainSquare" && Lookup[#, "Direction", ""] === "Contract" &];
publicOpaqueResult = ApplySpinorIdentity[publicOpaquePair, publicOpaqueEdit,
  "MomentumConservation" -> False];
check["public identity application preserves every opaque propagator argument",
  publicOpaqueResult === 4 chain[]/publicOpaque &&
    Cases[publicOpaqueResult, _PropDen, Infinity] === {publicOpaque}];
symbolicScramble = ComplicateAmplitude[{sixRules, sixAmplitude}, 2,
  RandomSeed -> 3, "RecordSteps" -> True, "MomentumConservation" -> False];
check["symbolic scramble records every exact intermediate state",
  symbolicScramble["Steps"] === 2 && Length[symbolicScramble["Trajectory"]] === 2];
check["symbolic scramble uses only public legal identities",
  AllTrue[symbolicScramble["Trajectory"], MemberQ[
    SpinorIdentityCandidates[{sixRules, #["Before"]}, "MomentumConservation" -> False],
    #["Candidate"]] &]];
check["public identity application rejects Stop", Quiet[
  ApplySpinorIdentity[publicPair, <|"Name" -> "Stop", "Subtract" -> 0, "Insert" -> 0|>]] === $Failed];
shellPublicPair = {Append[First[publicPair], Mass[Multiparticle[2, 3]] -> shellMass],
  MomProd[2, 3] chain[]};
shellPublicEdit = SelectFirst[SpinorIdentityCandidates[shellPublicPair,
  "MomentumConservation" -> False], #["Name"] === "OnShell" &];
check["an internal-shell edit is rejected when its channel is disabled", Quiet[
  ApplySpinorIdentity[shellPublicPair, shellPublicEdit,
    "OnShellChannels" -> {}, "MomentumConservation" -> False]] === $Failed];
check["loading HEPCAT does not load a neural package",
  Names["Unscrambling`*"] === {} &&
    Names["ConstructiveDiagrams`TrainUnscrambleNet"] === {} &&
    Names["ConstructiveDiagrams`UnscrambleSpinorAmplitudes"] === {}];
(* A degenerate Schouten split must not rewrite a product into itself. *)
degenerateSchoutenResults = {};
Do[
  degenerateSchoutenInput = SpinorChain[Spinor[kind, chirality, 2],
      Spinor["Helicity", chirality, 5]] *
    SpinorChain[Spinor["Helicity", chirality, 5], Mom[1], Mom[2],
      Spinor["Helicity", chirality, 5]];
  degenerateSchoutenReduced = TimeConstrained[
    ReduceSpinorProducts[degenerateSchoutenInput], 3, $TimedOut];
  AppendTo[degenerateSchoutenResults, degenerateSchoutenReduced];
  check[{"degenerate Schouten terminates", kind, chirality},
    degenerateSchoutenReduced =!= $TimedOut &&
      FreeQ[degenerateSchoutenReduced, _SpinorChainReduced] &&
      If[kind === "Helicity",
        (* A declared helicity spinor fixes its own momentum to rank one.
           The old arbitrary matrix2 chart cannot test that identity. *)
        Module[{angles = <|2 -> {2, -1}, 5 -> {3, 5}|>,
            squares = <|2 -> {1, 4}, 5 -> {-2, 3}|>, momenta, chart},
          momenta = Join[<|1 -> matrices[1]|>,
            AssociationMap[Outer[Times, angles[#], squares[#]] &, {2, 5}]];
          chart = <|"Momenta" -> momenta, "Angle" -> angles, "Square" -> squares,
            "PoleVariable" -> degenerateSchoutenPole, "ReferenceSeed" -> 1|>;
          Expand[(degenerateSchoutenInput - degenerateSchoutenReduced) /.
            c_SpinorChain :> ConstructiveDiagrams`Private`residueChartChain[c, chart] /.
            MomProd[a_, b_] :> (Det[momenta[a] + momenta[b]] -
              Det[momenta[a]] - Det[momenta[b]])/2 /.
            Power[Mass[label_Integer], power_Integer?EvenQ] :>
              Det[momenta[label]]^(power/2)] === 0],
        numeric[degenerateSchoutenInput - degenerateSchoutenReduced] === 0]],
  {kind, {"Spin", "Helicity"}}, {chirality, {"Angle", "Square"}}];
check["all degenerate Schouten regressions completed", Length[degenerateSchoutenResults] === 4];
degenerateSandwichResults = {};
Do[
  degenerateSandwichInput = SpinorChain[Spinor[kind, "Square", 4], Mom[2],
      Spinor["Helicity", "Angle", 5]] *
    SpinorChain[Spinor["Helicity", "Angle", 5], Mom[2], Mom[4],
      Spinor["Helicity", "Angle", 5]];
  degenerateSandwichReduced = TimeConstrained[
    ReduceSpinorProducts[degenerateSandwichInput], 3, $TimedOut];
  AppendTo[degenerateSandwichResults, degenerateSandwichReduced];
  check[{"degenerate mixed sandwich terminates", kind},
    degenerateSandwichReduced === degenerateSandwichInput],
  {kind, {"Spin", "Helicity"}}];
check["all degenerate mixed sandwich regressions completed", Length[degenerateSandwichResults] === 2];

(* Xi identifiers are independent spinors, even when they equal momentum labels. *)
Module[{left, right, fixture, examples, results, spinors, spinorValues, matrices,
    vector, valueChain, value, physical, expected, targetSpinor, epsilon, adjugate},
  epsilon = {{0, 1}, {-1, 0}};
  adjugate[matrix_] := {{matrix[[2, 2]], -matrix[[1, 2]]},
    {-matrix[[2, 1]], matrix[[1, 1]]}};
  Do[
    left = SpinorChain[Spinor["Xi", "Angle", reference], Mom[3],
      Spinor["Helicity", "Square", 4]];
    right = SpinorChain[Spinor["Xi", "Angle", reference + 1], Mom[3],
      Spinor["Helicity", "Square", 5]];
    fixture = left right;
    examples = {7 fixture, 3 left^2 right, 5 left right^2, 11 left/right,
      13 fixture/PropDen[Mom[Multiparticle[1, 3]], xiTestMass]};
    results = Block[{$RecursionLimit = 160},
      TimeConstrained[ReduceSpinorProducts /@ examples, 3, $TimedOut]];
    check[{"Xi-targeted Schouten products terminate", reference},
      ListQ[results] && Length[results] === Length[examples] &&
        FreeQ[results, _SpinorChainReduced | _SpinorTraceReduced]];
    check[{"Xi-targeted Schouten products remain idempotent", reference},
      ListQ[results] && Block[{$RecursionLimit = 160},
        ReduceSpinorProducts /@ results] === results];
    spinors = DeleteDuplicates[Cases[examples, _Spinor, Infinity]];
    vector[spinor_] := Lookup[spinorValues, ToString[spinor, InputForm]];
    valueChain[chain_SpinorChain] := Module[{parts = List @@ chain, angle, row},
      angle = parts[[1, 2]] === "Angle";
      row = vector[First[parts]].epsilon If[angle, 1, -1];
      Do[row = row.If[angle, matrices[momentum[[1]]], adjugate[matrices[momentum[[1]]]]];
        angle = !angle, {momentum, Drop[Rest[parts], -1]}];
      row.vector[Last[parts]]];
    value[expression_] := Expand[expression /. chain_SpinorChain :> valueChain[chain] /.
      {_PropDen -> 7, Mass[3]^2 -> Det[matrices[3]],
        MomProd[i_, j_] :> Tr[matrices[i].adjugate[matrices[j]]]/2}];
    Do[
      BlockRandom[SeedRandom[seed];
        matrices = AssociationThread[{3, 5}, Table[
          RandomInteger[{-4, 4}, {2, 2}] + I RandomInteger[{-3, 3}, {2, 2}], {2}]];
        spinorValues = AssociationThread[ToString[#, InputForm] & /@ spinors,
          Table[RandomInteger[{-5, 5}, 2] + I RandomInteger[{-3, 3}, 2],
            {Length[spinors]}]]];
      check[{"Xi-targeted Schouten preserves independent exact spinors", reference, seed},
        ListQ[results] && And @@ MapThread[value[#1 - #2] === 0 &, {examples, results}]],
      {seed, {4507, 7919}}],
    {reference, {3, 13}}];

  targetSpinor = Spinor["Helicity", "Angle", 3];
  physical = SpinorChain[Spinor["Helicity", "Square", 4], Mom[3],
      Spinor["Xi", "Angle", 7]] *
    SpinorChain[Spinor["Xi", "Square", 8], Mom[5], targetSpinor];
  expected = -SpinorChain[targetSpinor, Spinor["Xi", "Angle", 7]] *
    SpinorChain[Spinor["Xi", "Square", 8], Mom[5], Mom[3],
      Spinor["Helicity", "Square", 4]];
  check["the physical-endpoint Schouten orientation still exposes its Dirac reduction",
    Expand[ReduceSpinorProducts[physical] - ReduceSpinorProducts[expected]] === 0];
];

(* Recreate the old conditions in an existing kernel, without loading old code
   or relying on a temporary fixture. Unrelated user product rules must survive. *)
xiReloadGuardedRules = Select[DownValues[Times],
  !FreeQ[#, HoldPattern[UnsameQ[_, "Xi"]]] && !FreeQ[#, _SpinorChainReduced] &];
xiReloadOldRules = DeleteCases[xiReloadGuardedRules,
  HoldPattern[UnsameQ[_, "Xi"]], Infinity];
check["the reload regression reconstructs exactly the five obsolete product orientations",
  Length[xiReloadGuardedRules] === 5 && Length[xiReloadOldRules] === 5 &&
    FreeQ[xiReloadOldRules, HoldPattern[UnsameQ[_, "Xi"]]]];
Unprotect[Times];
DownValues[Times] = Join[xiReloadOldRules, DownValues[Times]];
Global`xiReloadLeft[a_] Global`xiReloadRight[b_] := Global`xiReloadValue[a, b];
Protect[Times];
xiReloadBefore = Length[DownValues[Times]];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
check["package reload removes only the five obsolete unguarded product definitions",
  Length[DownValues[Times]] === xiReloadBefore - 5 &&
    Intersection[xiReloadOldRules, DownValues[Times]] === {}];
check["package reload preserves unrelated user definitions on Times",
  Global`xiReloadLeft[2] Global`xiReloadRight[3] === Global`xiReloadValue[2, 3]];
xiReloadFixture = SpinorChain[Spinor["Xi", "Angle", 3], Mom[3],
    Spinor["Helicity", "Square", 4]] *
  SpinorChain[Spinor["Xi", "Angle", 4], Mom[3], Spinor["Helicity", "Square", 5]];
xiReloadExpected = SpinorChain[Spinor["Helicity", "Square", 4], Mom[3],
    Spinor["Xi", "Angle", 3]] *
  SpinorChain[Spinor["Helicity", "Square", 5], Mom[3], Spinor["Xi", "Angle", 4]];
check["the Xi product cycle is repaired in a reloaded existing kernel",
  Block[{$RecursionLimit = 160},
    TimeConstrained[ReduceSpinorProducts[xiReloadFixture], 3, $TimedOut]] === xiReloadExpected];
xiReloadFirstRules = DownValues[Times];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
check["repeated package reloads neither duplicate product rules nor erase unrelated rules",
  DownValues[Times] === xiReloadFirstRules &&
    Global`xiReloadLeft[5] Global`xiReloadRight[7] === Global`xiReloadValue[5, 7] &&
    MemberQ[Attributes[Times], Protected]];
Unprotect[Times];
Unset[Global`xiReloadLeft[a_] Global`xiReloadRight[b_]];
Protect[Times];

(* ::Section:: *)
(* Radiating fermion currents: external Dirac, Schouten and rank-one momenta.
   Every chart is exact. P is arbitrary; no internal cut or momentum
   conservation is imposed. Explicit spin indices select independent columns. *)
qfiOther["Angle"] := "Square";
qfiOther["Square"] := "Angle";
qfiJ = {{0, -1}, {1, 0}}; qfiEps = -qfiJ;
qfiAdj[p_] := {{p[[2, 2]], -p[[1, 2]]}, {-p[[2, 1]], p[[1, 1]]}};
qfiChart[massive_, seed_] := BlockRandom[SeedRandom[119003 + seed]; Module[
  {m = If[massive, {1, 3/7, 2}[[1 + Mod[seed, 3]]], 0], u, v, w,
   pa, p, basis, squareBasis, la, ta, lk, tk, ra, rs, momenta, angles, squares},
  {u, v, w} = {RandomInteger[{1, 5}], RandomInteger[{-5, 5}], RandomInteger[{-5, 5}]};
  basis = {{u, v}, {w, (1 + v w)/u}};
  la = basis[[All, 1]];
  If[massive,
    {u, v, w} = {RandomInteger[{1, 5}], RandomInteger[{-5, 5}], RandomInteger[{-5, 5}]};
    pa = {{u, v}, {w, (m^2 + v w)/u}};
    squareBasis = qfiEps.qfiAdj[pa].basis/m; ta = squareBasis[[All, 1]],
    ta = RandomInteger[{-5, 5}, 2]; pa = Outer[Times, la, ta]; squareBasis = IdentityMatrix[2]];
  p = RandomInteger[{-5, 5}, {2, 2}];
  lk = {2, seed + 2}; tk = {seed + 1, -1};
  ra = {seed + 3, 2 seed - 1}; rs = {3 seed - 2, seed + 4};
  momenta = <|1 -> pa, 2 -> p, 3 -> Outer[Times, ra, rs],
    4 -> RandomInteger[{-5, 5}, {2, 2}], 5 -> Outer[Times, lk, tk]|>;
  angles = <|1 -> la, 3 -> ra, 5 -> lk|>; squares = <|1 -> ta, 3 -> rs, 5 -> tk|>;
  <|"Momenta" -> momenta, "Angle" -> angles, "Square" -> squares,
    "AngleBasis" -> <|1 -> basis|>, "SquareBasis" -> <|1 -> squareBasis|>,
    "References" -> <|"Angle" -> <|77 -> ra, 1 -> {seed - 2, seed + 5}|>,
      "Square" -> <|77 -> rs, 1 -> {2 seed + 3, -seed - 1}|>|>, "MassValue" -> m|>
]];
qfiMassiveCharts = Table[qfiChart[True, seed], {seed, 10}];
qfiMasslessCharts = Table[qfiChart[False, seed], {seed, 10}];
qfiKind[s_Spinor] := s[[If[Length[s] === 5, 3, 2]]];
qfiLeg[s_Spinor] := s[[If[Length[s] === 5, 4, 3]]];
qfiVector[s_Spinor, chart_] := Module[{kind = qfiKind[s], label = qfiLeg[s], column, basis},
  If[First[s] === "Xi", Return[chart["References"][kind][label]]];
  If[Length[s] === 3, Return[chart[kind][label]]];
  basis = chart[kind <> "Basis"][label]; column = If[Last[s] === -1/2, 1, 2];
  If[s[[2]] === "Upper", basis[[All, column]],
    If[column === 1, basis[[All, 2]], -basis[[All, 1]]]]
];
qfiMatrix[label_Integer, chart_] := chart["Momenta"][label];
qfiMatrix[Multiparticle[labels__], chart_] := Total[qfiMatrix[#, chart] & /@ {labels}];
qfiValueChain[c_SpinorChain, chart_] := Module[
  {parts = List @@ c, kind, row, right},
  kind = qfiKind[First[parts]]; row = qfiVector[First[parts], chart];
  If[kind === "Angle", row = row.qfiJ];
  Do[row = row.If[kind === "Angle", qfiMatrix[p[[1]], chart], qfiAdj[qfiMatrix[p[[1]], chart]]];
    kind = qfiOther[kind], {p, Rest[Most[parts]]}];
  right = qfiVector[Last[parts], chart];
  If[qfiKind[Last[parts]] === "Square", right = qfiEps.right];
  row.right
];
qfiValue[expression_, chart_] := Together[
  expression /. c_SpinorChain :> qfiValueChain[c, chart] /.
    MomProd[a_, b_] :> (Det[qfiMatrix[a, chart] + qfiMatrix[b, chart]] -
      Det[qfiMatrix[a, chart]] - Det[qfiMatrix[b, chart]])/2 /.
    Power[Mass[label_Integer], power_Integer?EvenQ] :> Det[qfiMatrix[label, chart]]^(power/2) /.
    {Mass[1] -> chart["MassValue"], Mass[5] -> 0}];
qfiFlip[s_Spinor] := s /. {
  Spinor[family_, position_, kind_, label_, index_] :>
    Spinor[family, position, qfiOther[kind], label, index],
  Spinor[family_, kind_, label_] :> Spinor[family, qfiOther[kind], label]};
qfiFactors[family_, a_, r_, reverseBracket_, reverseMomentum_] := Module[
  {kind = qfiKind[a], k, otherK, bare, diagonal},
  k = Spinor["Helicity", kind, 5]; otherK = Spinor["Helicity", qfiOther[kind], 5];
  bare = If[reverseBracket, SpinorChain[k, a], SpinorChain[a, k]];
  diagonal = If[family === "Fundamental",
    If[reverseMomentum, SpinorChain[otherK, Mom[1], r], SpinorChain[r, Mom[1], otherK]],
    SpinorChain @@ Join[{otherK}, Mom /@ If[reverseMomentum, {1, 2}, {2, 1}], {otherK}]];
  {bare, diagonal}
];
qfiExpected[family_, a_, r_] := Module[{kind = qfiKind[a], k, otherK},
  k = Spinor["Helicity", kind, 5]; otherK = Spinor["Helicity", qfiOther[kind], 5];
  If[family === "Fundamental",
    Mass[1] SpinorChain[r, k] SpinorChain[qfiFlip[a], otherK] +
      2 MomProd[1, 5] SpinorChain[a, r],
    2 Mass[1] MomProd[2, 5] SpinorChain[qfiFlip[a], otherK] -
      2 MomProd[1, 5] SpinorChain[a, Mom[2], otherK]]
];
Do[
  qfiA = Spinor[If[qfiMassive, "Spin", "Helicity"], qfiChirality, 1];
  qfiR = Spinor["Xi", qfiChirality, 77];
  qfiInput = Times @@ qfiFactors[qfiFamily, qfiA, qfiR, False, False];
  qfiRight = qfiExpected[qfiFamily, qfiA, qfiR];
  qfiBoard = If[qfiMassive, qfiMassiveCharts, qfiMasslessCharts];
  check[{"radiation identity agrees with independent exact Weyl matrices", qfiFamily, qfiChirality, qfiMassive},
    AllTrue[qfiBoard, qfiValue[qfiInput - qfiRight, #] === 0 &]];
  qfiReduced = ReduceSpinorProducts[qfiInput];
  check[{"ordinary radiation reduction retains its off-cut remainder and terminates", qfiFamily, qfiChirality, qfiMassive},
    Count[qfiReduced, _Mom, Infinity] < Count[qfiInput, _Mom, Infinity] &&
      !FreeQ[qfiReduced, MomProd[1, 5]] && FreeQ[qfiReduced, _SpinorChainReduced] &&
      ReduceSpinorProducts[qfiReduced] === qfiReduced &&
      AllTrue[qfiBoard, qfiValue[qfiInput - qfiReduced, #] === 0 &]],
  {qfiFamily, {"Fundamental", "Physical"}}, {qfiChirality, {"Angle", "Square"}},
  {qfiMassive, {True, False}}];
Do[
  qfiA = Spinor["Spin", qfiPosition, qfiChirality, 1, qfiSpin];
  qfiR = Spinor["Xi", qfiChirality, 77];
  qfiInput = Times @@ qfiFactors[qfiFamily, qfiA, qfiR, qfiReverseBracket, qfiReverseMomentum];
  qfiReduced = ReduceSpinorProducts[qfiInput];
  qfiSign = If[qfiReverseBracket, -1, 1] * If[qfiFamily === "Physical" && qfiReverseMomentum, -1, 1];
  qfiRight = qfiSign qfiExpected[qfiFamily, qfiA, qfiR];
  check[{"radiation contractions preserve indexed spin components and both orientations", qfiFamily,
      qfiChirality, qfiPosition, qfiSpin, qfiReverseBracket, qfiReverseMomentum},
    !FreeQ[qfiReduced, Spinor["Spin", qfiPosition, _, 1, qfiSpin]] &&
      Count[qfiReduced, _Mom, Infinity] < Count[qfiInput, _Mom, Infinity] &&
      AllTrue[Take[qfiMassiveCharts, 3],
        qfiValue[qfiInput - qfiRight, #] === 0 && qfiValue[qfiInput - qfiReduced, #] === 0 &]],
  {qfiFamily, {"Fundamental", "Physical"}}, {qfiChirality, {"Angle", "Square"}},
  {qfiPosition, {"Upper", "Lower"}}, {qfiSpin, {-1/2, 1/2}},
  {qfiReverseBracket, {False, True}}, {qfiReverseMomentum, {False, True}}];
Do[
  qfiA = Spinor["Spin", qfiChirality, 1]; qfiR = Spinor["Xi", qfiChirality, 77];
  qfiPair = qfiFactors[qfiFamily, qfiA, qfiR, False, False];
  qfiInput = 7 qfiPair[[1]]^qfiPowers[[1]] qfiPair[[2]]^qfiPowers[[2]];
  qfiReduced = TimeConstrained[ReduceSpinorProducts[qfiInput], 5, $TimedOut];
  check[{"radiation contractions support unequal positive powers", qfiFamily, qfiChirality, qfiPowers},
    qfiReduced =!= $TimedOut && AllTrue[Take[qfiMassiveCharts, 5], qfiValue[qfiInput - qfiReduced, #] === 0 &]],
  {qfiFamily, {"Fundamental", "Physical"}}, {qfiChirality, {"Angle", "Square"}},
  {qfiPowers, {{2, 3}, {3, 2}}}];
Do[
  qfiA = Spinor["Spin", qfiChirality, 1];
  qfiR = Spinor[qfiReferenceFamily, qfiChirality, If[qfiReferenceFamily === "Xi", 77, 3]];
  qfiInput = Times @@ qfiFactors["Fundamental", qfiA, qfiR, False, False];
  qfiReduced = ReduceSpinorProducts[qfiInput];
  check[{"the fundamental identity permits an arbitrary independent reference", qfiChirality, qfiReferenceFamily},
    Count[qfiReduced, _Mom, Infinity] < Count[qfiInput, _Mom, Infinity] &&
      AllTrue[Take[qfiMassiveCharts, 5], qfiValue[qfiInput - qfiReduced, #] === 0 &]],
  {qfiChirality, {"Angle", "Square"}}, {qfiReferenceFamily, {"Xi", "Spin", "Helicity"}}];
Do[
  qfiA = Spinor["Xi", qfiChirality, 1]; qfiR = Spinor["Xi", qfiChirality, 77];
  qfiInput = Times @@ qfiFactors[qfiFamily, qfiA, qfiR, False, False];
  qfiReduced = ReduceSpinorProducts[qfiInput];
  check[{"a coinciding Xi identifier does not license a Dirac equation", qfiFamily, qfiChirality},
    AllTrue[Take[qfiMassiveCharts, 5], qfiValue[qfiInput - qfiReduced, #] === 0 &]],
  {qfiFamily, {"Fundamental", "Physical"}}, {qfiChirality, {"Angle", "Square"}}];
qfiOpaque = PropDen[Mom[Multiparticle[1, 5]], qfiPoleMass];
Do[
  qfiA = Spinor["Spin", qfiChirality, 1]; qfiR = Spinor["Xi", qfiChirality, 77];
  qfiInput = Times @@ qfiFactors["Physical", qfiA, qfiR, False, False];
  qfiReduced = ReduceSpinorProducts[qfiInput/qfiOpaque];
  check[{"ordinary radiation identities preserve the original physical propagator", qfiChirality},
    DeleteDuplicates[Cases[qfiReduced, _PropDen, Infinity]] === {qfiOpaque} &&
      !FreeQ[qfiReduced, MomProd[1, 5]] &&
      AllTrue[Take[qfiMassiveCharts, 5], qfiValue[qfiInput - qfiOpaque qfiReduced, #] === 0 &]],
  {qfiChirality, {"Angle", "Square"}}];
Do[
  qfiA = Spinor["Spin", qfiChirality, 1]; qfiR = Spinor["Xi", qfiChirality, 77];
  qfiPair = qfiFactors[qfiFamily, qfiA, qfiR, False, False];
  qfiInput = qfiPair[[1]]/qfiPair[[2]]; qfiReduced = ReduceSpinorProducts[qfiInput];
  qfiBoard = Select[qfiMassiveCharts, qfiValue[qfiPair[[2]], #] =!= 0 &];
  check[{"an inverse chain is not consumed as a numerator contraction", qfiFamily, qfiChirality},
    Length[qfiBoard] >= 6 && AllTrue[qfiBoard, qfiValue[qfiInput - qfiReduced, #] === 0 &]],
  {qfiFamily, {"Fundamental", "Physical"}}, {qfiChirality, {"Angle", "Square"}}];

(* Rank-one insertion factorization: each slot and parity uses a genuinely
   massless pk. An Xi identifier or an unconstrained matrix is not a witness. *)
Do[
  qfiSlots = ReplacePart[Take[{2, 3, 4, 2, 3}, qfiLength], qfiSlot -> 5];
  qfiEndKind = If[EvenQ[qfiLength], qfiChirality, qfiOther[qfiChirality]];
  qfiInput = SpinorChain @@ Join[{Spinor["Spin", qfiChirality, 1]}, Mom /@ qfiSlots,
    {Spinor["Xi", qfiEndKind, 77]}];
  qfiAtKind = If[EvenQ[qfiSlot - 1], qfiChirality, qfiOther[qfiChirality]];
  qfiRight = (SpinorChain @@ Join[{First[qfiInput]}, Mom /@ Take[qfiSlots, qfiSlot - 1],
      {Spinor["Helicity", qfiAtKind, 5]}]) *
    (SpinorChain @@ Join[{Spinor["Helicity", qfiOther[qfiAtKind], 5]},
      Mom /@ Drop[qfiSlots, qfiSlot], {Last[qfiInput]}]);
  qfiSplit = ConstructiveDiagrams`Private`rspSplitMasslessMomentum[qfiInput, 5];
  check[{"rank-one splitting has the correct orientation at every chain slot", qfiChirality, qfiLength, qfiSlot},
    !MissingQ[qfiSplit] && Count[qfiSplit, _Mom, Infinity] === qfiLength - 1 &&
      AllTrue[Take[qfiMassiveCharts, 3],
        qfiValue[qfiInput - qfiRight, #] === 0 && qfiValue[qfiInput - qfiSplit, #] === 0 &]],
  {qfiChirality, {"Angle", "Square"}}, {qfiLength, 1, 5}, {qfiSlot, qfiLength}];
Do[
  qfiInput = SpinorChain[Spinor["Spin", qfiChirality, 1], Mom[5],
    Spinor["Xi", qfiOther[qfiChirality], 77]] *
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Xi", "Square", 77]];
  qfiReduced = ReduceSpinorProducts[qfiInput];
  check[{"a matching helicity witness removes an inserted massless momentum", qfiChirality},
    FreeQ[qfiReduced, _Mom] && ReduceSpinorProducts[qfiReduced] === qfiReduced &&
      AllTrue[Take[qfiMassiveCharts, 5], qfiValue[qfiInput - qfiReduced, #] === 0 &]];
  qfiInput = SpinorChain[Spinor["Helicity", qfiChirality, 5], Mom[2],
    Spinor["Helicity", qfiOther[qfiChirality], 5]];
  check[{"a diagonal helicity sandwich is its ordinary scalar invariant", qfiChirality},
    ReduceSpinorProducts[qfiInput] === 2 MomProd[2, 5] &&
      ReduceSpinorProducts[qfiInput /. Mom[2] -> Mom[5]] === 0 &&
      AllTrue[qfiMassiveCharts, qfiValue[qfiInput - 2 MomProd[2, 5], #] === 0 &]];
  qfiInput = SpinorChain[Spinor["Spin", qfiChirality, 1], Mom[5],
    Spinor["Xi", qfiOther[qfiChirality], 77]];
  qfiReduced = ReduceSpinorProducts[qfiInput];
  qfiBoard = Map[Join[#, <|"Momenta" -> Join[#["Momenta"], <|5 -> {{7, 2}, {3, 8}}|>]|>] &, Take[qfiMassiveCharts, 5]];
  check[{"an unlicensed momentum stays unconstrained when no helicity witness exists", qfiChirality},
    FreeQ[qfiReduced, Spinor["Helicity", _, 5]] &&
      AllTrue[qfiBoard, qfiValue[qfiInput - qfiReduced, #] === 0 &]],
  {qfiChirality, {"Angle", "Square"}}];

Print[<|"Checks" -> checks, "Failed" -> Length[failures], "Failures" -> failures|>];
Print["HEPCAT_TEST_COMPLETED:spinor-identity-tests.wl"];
Quit[If[failures === {}, 0, 1]];
