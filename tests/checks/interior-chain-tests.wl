root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
Get[FileNameJoin[{root, "tests", "unscrambling.wl"}]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]]);
rules = {Mass[1] -> m1, Mass[2] -> m2, Mass[3] -> m3, Mass[4] -> m4};
chain[pp___] := SpinorChain[Spinor["Spin", "Angle", 1], pp, Spinor["Spin", "Angle", 4]];
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
Print[<|"Checks" -> checks, "Failed" -> Length[failures], "Failures" -> failures|>];
Quit[If[failures === {}, 0, 1]];
