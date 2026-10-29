(* The contraction fast path must preserve factored input without hiding any
   supported completeness, trace or epsilon contraction. Exact signs and
   independent algebra checks remain in legacy-algebra-tests.wl. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value],
  AppendTo[failures, name]; Print["FAIL: ", name]]);
possible[value_] := ConstructiveDiagrams`Private`spinContractionsPossibleQ[value];
opposite[value_] := If[value === "Angle", "Square", "Angle"];
variance[value_] := If[value === "Upper", "Lower", "Upper"];
spectator[kind_, leg_] := Spinor["Spin", kind, leg];
indexed[head_, level_, kind_] := head["Spin", level, kind, 7, guardIndex];
helicity[head_, kind_, indexed_] := If[indexed,
  head["Helicity", kind, 7, guardIndex], head["Helicity", kind, 7]];
pairChains[first_, second_, placement_] := Switch[placement,
  "Adjacent", {SpinorChain[spectator["Angle", 91], first],
    SpinorChain[second, spectator["Square", 92]]},
  "Leading", {SpinorChain[first, spectator["Angle", 91]],
    SpinorChain[second, spectator["Square", 92]]},
  "Trailing", {SpinorChain[spectator["Angle", 91], first],
    SpinorChain[spectator["Square", 92], second]}];
fixtures = {};
add[name_, value_] := AppendTo[fixtures, {name, value}];

Do[
  pair = pairChains[indexed[head, level, firstKind],
    indexed[head, variance[level], secondKind], placement];
  add[{"massive completeness", head, level, firstKind, secondKind, placement}, Times @@ pair],
  {head, {Spinor, SpinorHat}}, {level, {"Upper", "Lower"}},
  {firstKind, {"Angle", "Square"}}, {secondKind, {"Angle", "Square"}},
  {placement, {"Adjacent", "Leading", "Trailing"}}];

Do[
  pair = pairChains[helicity[head, kind, hasIndex], helicity[head, opposite[kind], hasIndex], placement];
  add[{"massless completeness", head, kind, hasIndex, placement}, Times @@ pair],
  {head, {Spinor, SpinorHat}}, {kind, {"Angle", "Square"}}, {hasIndex, {False, True}},
  {placement, {"Adjacent", "Leading", "Trailing"}}];

Do[
  pair = pairChains[helicity[head, kind, False], helicity[head, opposite[kind], False], placement];
  add[{"massless powers", head, kind, placement, powers},
    pair[[1]]^First[powers] pair[[2]]^Last[powers]],
  {head, {Spinor, SpinorHat}}, {kind, {"Angle", "Square"}},
  {placement, {"Adjacent", "Leading", "Trailing"}},
  {powers, {{3, 3}, {1, 3}, {3, 1}, {-2, -2}, {guardPower, guardPower}}}];
Do[
  pair = pairChains[helicity[head, kind, True], helicity[head, opposite[kind], True], "Adjacent"];
  add[{"indexed helicity squared tail", head, kind}, pair[[1]] pair[[2]]^2],
  {head, {Spinor, SpinorHat}}, {kind, {"Angle", "Square"}}];

Do[
  add[{"closed massive trace", head, level, firstKind, secondKind, slots},
    SpinorChain @@ Join[{indexed[head, level, firstKind]}, Take[Mom /@ {8, 9}, slots],
      {indexed[head, variance[level], secondKind]}]],
  {head, {Spinor, SpinorHat}}, {level, {"Upper", "Lower"}},
  {firstKind, {"Angle", "Square"}}, {secondKind, {"Angle", "Square"}}, {slots, 0, 2}];
Do[
  add[{"closed bare helicity trace", head, kind, slots},
    SpinorChain @@ Join[{helicity[head, kind, False]}, Take[Mom /@ {8, 9}, slots],
      {helicity[head, opposite[kind], False]}]],
  {head, {Spinor, SpinorHat}}, {kind, {"Angle", "Square"}}, {slots, 0, 2}];

Do[
  spinor = indexed[head, level, kind];
  chain = Switch[placement,
    "Leading", SpinorChain[spinor, spectator["Square", 92]],
    "Middle", SpinorChain[spectator["Angle", 91], spinor, spectator["Square", 92]],
    "Trailing", SpinorChain[spectator["Angle", 91], spinor]];
  epsilon = If[order === 1, SpinorEps[variance[level], guardIndex, guardOtherIndex],
    SpinorEps[variance[level], guardOtherIndex, guardIndex]];
  add[{"epsilon raising and lowering", head, level, kind, placement, order}, chain epsilon],
  {head, {Spinor, SpinorHat}}, {level, {"Upper", "Lower"}}, {kind, {"Angle", "Square"}},
  {placement, {"Leading", "Middle", "Trailing"}}, {order, {1, 2}}];

check["every current replacement family has its fixture", Length[fixtures] === 244];
Do[
  check[{"a real contraction is never skipped", First[fixture]}, possible[Last[fixture]]];
  reduced = ReduceSpinContractions[Last[fixture]];
  check[{"the admitted contraction still reduces", First[fixture]}, reduced =!= Last[fixture]],
  {fixture, fixtures}];

pair = pairChains[helicity[Spinor, "Angle", False], helicity[Spinor, "Square", False], "Adjacent"];
product = Times @@ pair;
sewn = SpinorChain[spectator["Angle", 91], Mom[7], spectator["Square", 92]];
masks = {Function[value, value + guardOffset],
  Function[value, (guardA + guardB) value],
  Function[value, value/PropDen[Mom[Multiparticle[1, 2]], guardMass]],
  Function[value, 1/(guardA value)], Function[value, value^2], Function[value, value^-2],
  Function[value, (guardA + value)/(guardB + guardC)]};
Do[
  wrapped = masks[[index]][product]; expected = masks[[index]][sewn];
  check[{"contractions remain visible inside algebraic containers", index}, possible[wrapped]];
  check[{"container reductions preserve the sewn value", index},
    Cancel[Together[ReduceSpinContractions[wrapped] - expected]] === 0],
  {index, Length[masks]}];

onlyAngle = SpinorChain[helicity[Spinor, "Angle", False], spectator["Angle", 91]];
onlySquare = SpinorChain[Spinor["Helicity", "Square", 8], spectator["Square", 92]];
factored = Times @@ Table[(1 + guardCoefficient[index] onlyAngle + onlySquare^2), {index, 18}];
factored = factored/(guardDenominator + onlySquare)^3;
check["uncontracted rational factors do not trigger expansion", !possible[factored]];
unchanged = TimeConstrained[ReduceSpinContractions[factored], 3, $TimedOut];
check["the no-op result retains its exact factored form", unchanged === factored];
check["the no-op result retains its original expression size", LeafCount[unchanged] === LeafCount[factored]];
check["ordinary factored scalar expressions are also preserved",
  ReduceSpinContractions[(guardA + guardB)^8/(guardC + guardD)^3] ===
    (guardA + guardB)^8/(guardC + guardD)^3];
check["different spin indices do not form a signature pair", !possible[
  SpinorChain[spectator["Angle", 91], indexed[Spinor, "Upper", "Angle"]]
    SpinorChain[indexed[Spinor, "Lower", "Square"] /. guardIndex -> guardOtherIndex,
      spectator["Square", 92]]]];
check["different spinor heads do not form a signature pair", !possible[
  SpinorChain[spectator["Angle", 91], helicity[Spinor, "Angle", False]]
    SpinorChain[helicity[SpinorHat, "Square", False], spectator["Square", 92]]]];
check["bare and indexed helicities are distinct contraction signatures", !possible[
  SpinorChain[spectator["Angle", 91], helicity[Spinor, "Angle", False]]
    SpinorChain[helicity[Spinor, "Square", True], spectator["Square", 92]]]];

check["the complete contraction guard inventory ran", checks === 510];
Print[checks, " spin-contraction guard checks; ", Length[failures], " failures."];
If[failures =!= {}, Print[failures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:spin-contraction-guard-tests.wl"];
Quit[0];
