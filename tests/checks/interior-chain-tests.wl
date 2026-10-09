root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
Get[FileNameJoin[{root, "tests", "unscrambling.wl"}]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]]);
rules = {Mass[1] -> m1, Mass[2] -> m2, Mass[3] -> m3, Mass[4] -> m4};
chain[pp___] := SpinorChain[Spinor["Spin", "Angle", 1], pp,
  Spinor["Spin", If[EvenQ[Length[{pp}]], "Angle", "Square"], 4]];
moves[expr_, rr_:rules] := UnscrambleCandidates[{rr, expr}, "MomentumConservation" -> False];
apply[expr_, move_] := Unscrambling`Private`applyCandidate[Unscrambling`Private`canonicalChains[expr], move];
same[a_, b_] := Expand[Unscrambling`Private`canonicalChains[a - b]] === 0;
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
check["swap has reverse training label", Unscrambling`Private`trainingRows[after, rules, expr] =!= {}];
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
on = Select[UnscrambleCandidates[{internalRules, internal}], #["Name"] === "ChainSquare" && #["Direction"] === "Contract" &];
off = Select[UnscrambleCandidates[{internalRules, internal}, "OnShellChannels" -> {}], #["Name"] === "ChainSquare" && #["Direction"] === "Contract" &];
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
check["on shell reverse training label", Unscrambling`Private`trainingRows[target, internalRules, expr] =!= {}];
expr = MomProd[2, 3]^2 chain[]/den;
check["dot power contracts one factor", AnyTrue[moves[expr, internalRules], same[apply[expr, #], shell MomProd[2, 3] chain[]/den] &]];
powerTarget = shell MomProd[2, 3] chain[]/den;
check["dot power reverse reachable", AnyTrue[moves[powerTarget, internalRules], same[apply[powerTarget, #], expr] &]];
check["all on shell moves preserve propagator", AllTrue[Select[moves[expr, internalRules], #["Name"] === "OnShell" &], !FreeQ[apply[expr, #], den] &]];
check["no internal rule no on shell move", FreeQ[Lookup[moves[expr], "Name"], "OnShell"]];
check["disabled internal channel", FreeQ[Lookup[UnscrambleCandidates[{internalRules, expr}, "OnShellChannels" -> {}], "Name"], "OnShell"]];
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
  c_SpinorChain :> Unscrambling`Private`canonicalChain[c]];
sameMomenta[a_, b_] := explicitMomenta[a - b] === 0;
sumMoves[expr_, rr_:rules] := Select[
  UnscrambleCandidates[{rr, expr}, "MomentumConservation" -> False,
    "OnShellChannels" -> {}], #["Name"] === "MomentumSum" &];
reverseSumQ[before_, move_, rr_:rules] := With[{after = apply[before, move]},
  AnyTrue[sumMoves[after, rr], same[apply[after, #], before] &]];
sumLabelQ[before_, move_, rr_:rules] := Block[
  {Unscrambling`Private`$onShellChannels = {},
   Unscrambling`Private`$useMomentumConservation = False},
  Unscrambling`Private`trainingRows[apply[before, move], rr, before] =!= {}
];

expr = chain[Mom[Multiparticle[2, 3]]];
split = Select[sumMoves[expr], #["Direction"] === "Expand" &];
check["composite chain splits without momentum conservation or on shell", AnyTrue[split,
  same[apply[expr, #], chain[Mom[2]] + chain[Mom[3]]] &]];
check["composite split is independently linear", split =!= {} &&
  AllTrue[split, sameMomenta[expr, apply[expr, #]] &]];
check["composite split retains local site", AllTrue[split,
  MatchQ[Lookup[#, "Site", None], {_Integer, _Integer, _Integer}] &]];
check["composite split has exact reverse", split =!= {} && AllTrue[split, reverseSumQ[expr, #] &]];
check["composite split has reverse training target", split =!= {} && sumLabelQ[expr, First[split]]];

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
    Unscrambling`Private`relabelLegs[twoSlots, <|1 -> 11, 2 -> 27, 3 -> 43, 4 -> 68|>]]];
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
check["bundle introduction has reverse training target", introduce =!= {} && sumLabelQ[expr, First[introduce]]];
check["atomic introduction is linear in supplied legs", Length[introduce] === Length[rules] - 1];
overlapping = chain[Mom[Multiparticle[2, 3]]] + chain[Mom[2]];
overlapSplit = Select[sumMoves[overlapping], Lookup[#, "Identity", None] === "Split" &];
check["collection reverses a split after coefficients combine", overlapSplit =!= {} &&
  AllTrue[overlapSplit, reverseSumQ[overlapping, #] && sumLabelQ[overlapping, #] &]];
check["collection with unequal coefficients remains a linear identity", AllTrue[
  sumMoves[2 chain[Mom[2]] + chain[Mom[3]]],
  sameMomenta[2 chain[Mom[2]] + chain[Mom[3]], apply[2 chain[Mom[2]] + chain[Mom[3]], #]] &]];
check["permuted composite members normalize in chains", same[
  Unscrambling`Private`canonicalChains[chain[Mom[Multiparticle[3, 2]]]],
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
    same[Unscrambling`Private`canonicalChains[pair[[1]]], pair[[2]]]],
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
check["multi particle split has reverse training target", manySplits =!= {} &&
  sumLabelQ[indexedSum, First[manySplits], manyRules]];

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
  Unscrambling`Private`canonicalChains[1/PropDen[Mom[2] + Mom[3], mz]] ===
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
qDisabled = UnscrambleCandidates[{threeRules, qChain}, "OnShellChannels" -> {}, "MomentumConservation" -> False];
check["three-leg chain stays off shell when channel disabled", AnyTrue[
  Select[qDisabled, #["Name"] === "ChainSquare" && #["Direction"] === "Contract" &],
  same[apply[qChain, #], q3^2 chain[]] &]];
check["three-leg shell selection accepts permuted channel labels", AnyTrue[
  UnscrambleCandidates[{threeRules, q3^2 chain[]}, "OnShellChannels" -> {{3, 1, 2}}, "MomentumConservation" -> False],
  same[apply[q3^2 chain[], #], mq^2 chain[]] &]];
qSquareMoves = moves[q3^2 chain[], threeRules];
check["three-leg scalar square expands into all pairwise invariants", AnyTrue[qSquareMoves,
  same[apply[q3^2 chain[], #], qValue chain[]] &]];
check["three-leg scalar square contracts on shell", AnyTrue[qSquareMoves,
  same[apply[q3^2 chain[], #], mq^2 chain[]] &]];
check["three-leg square expansion is reversible off shell", AnyTrue[
  UnscrambleCandidates[{threeRules, qValue chain[]}, "OnShellChannels" -> {}, "MomentumConservation" -> False],
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
  Select[UnscrambleCandidates[{threeRules, q3^2 chain[]}, "OnShellChannels" -> {}, "MomentumConservation" -> False],
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
scalarChoices = Select[UnscrambleCandidates[{rules, scalarDot}], #["Name"] === "MomentumConservation" &];
check["scalar dot momentum conservation", AnyTrue[scalarChoices, same[apply[scalarDot, #], scalarDotTarget] &]];
check["scalar dot conservation has exact reverse", AnyTrue[UnscrambleCandidates[{rules, scalarDotTarget}],
  same[apply[scalarDotTarget, #], scalarDot] &]];
check["scalar conservation respects disabled option", FreeQ[Lookup[moves[scalarDot], "Name"], "MomentumConservation"]];
scalarRow = (MomProd[1, 2] + MomProd[1, 3] + MomProd[1, 4]) chain[];
check["complete scalar row contracts", AnyTrue[UnscrambleCandidates[{rules, scalarRow}],
  same[apply[scalarRow, #], -m1^2 chain[]] &]];
check["scalar row contraction is reversible", AnyTrue[UnscrambleCandidates[{rules, -m1^2 chain[]}],
  same[apply[-m1^2 chain[], #], scalarRow] &]];
manyDot = MomProd[11, 27] chain[];
manyDotTarget = (-m11^2 - Total[MomProd[11, #] & /@ {43, 68, 92, 105}]) chain[];
check["scalar conservation supports six sparse external labels", AnyTrue[UnscrambleCandidates[{manyRules, manyDot}],
  same[apply[manyDot, #], manyDotTarget] &]];
unknownDen = PropDen[opaque[Mom[1], MomProd[1, 2], chain[Mom[2], Mom[3]]], unknownMass];
check["propagator-only scalar and chain identities are never proposed",
  UnscrambleCandidates[{threeRules, 1/unknownDen}] === {Unscrambling`Private`stopMove[]}];
protectedScalar = q3^2 chain[]/unknownDen;
check["all new scalar edits preserve unknown propagator contents", AllTrue[
  UnscrambleCandidates[{threeRules, protectedScalar}],
  DeleteDuplicates[Cases[apply[protectedScalar, #], _PropDen, Infinity]] === {unknownDen} &]];
protectedConservation = scalarDot/unknownDen;
check["scalar momentum conservation preserves unknown propagator contents", AllTrue[
  UnscrambleCandidates[{rules, protectedConservation}],
  DeleteDuplicates[Cases[apply[protectedConservation, #], _PropDen, Infinity]] === {unknownDen} &]];
placeholderDen = PropDen[opaque[Mom[0], Mom[1]], unknownMass];
protectedRow = (chain[Mom[1]] + chain[Mom[2]] + chain[Mom[3]])/placeholderDen;
rowChoices = Select[UnscrambleCandidates[{rules, protectedRow}],
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
Print[<|"Checks" -> checks, "Failed" -> Length[failures], "Failures" -> failures|>];
Quit[If[failures === {}, 0, 1]];
