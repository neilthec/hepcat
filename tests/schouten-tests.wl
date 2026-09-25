(* Run with WolframKernel -script tests/schouten-tests.wl. No training or writes. *)
root = DirectoryName[DirectoryName[$InputFileName]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{Global`$HEPCATpath, "HEPCAT.wl"}]];
Get[FileNameJoin[{root, "tests", "unscrambling.wl"}]];

failures = {};
checks = 0;
check[name_, result_] := (checks++; If[!TrueQ[result], AppendTo[failures, name]]);
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
type[s_] := Unscrambling`Private`spinorType[s];
leg[s_] := Unscrambling`Private`spinorLeg[s];
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
state = Unscrambling`Private`canonicalChains[scrambled];
guesses = Select[UnscrambleCandidates[{rules, scrambled}, "MomentumConservation" -> False],
  #["Name"] === "Schouten" &];
outputs = Unscrambling`Private`applyCandidate[state, #] & /@ guesses;
check["policy can shorten momentum Schouten", AnyTrue[outputs,
  Unscrambling`Private`expressionComplexity[#] < Unscrambling`Private`expressionComplexity[scrambled] &]];
check["all policy Schouten actions preserve value", AllTrue[outputs, numeric[# - scrambled] === 0 &]];
check["longer chains are penalized", SpinorExpressionComplexity[ch["Angle", 1, {5, 6}, "Angle", 2]] >
  SpinorExpressionComplexity[ch["Angle", 1, {5}, "Square", 2]]];

(* Exercise trace/checkpoint plumbing with a deterministic policy, without
   importing the neural runtime or changing the saved trained network. *)
chosen = SelectFirst[guesses, SpinorExpressionComplexity[
  Unscrambling`Private`applyCandidate[state, #]] < SpinorExpressionComplexity[state] &];
Block[{Unscrambling`Private`loadUnscrambleModel, Unscrambling`Private`selectCandidate},
  Unscrambling`Private`loadUnscrambleModel[_] := True;
  Unscrambling`Private`selectCandidate[ex_, _, _] := <|"Candidate" ->
    If[ex === state, chosen, Unscrambling`Private`stopMove[]], "Index" -> 1, "Count" -> 1, "Score" -> 1.|>;
  trace = UnscrambleTrace[{rules, scrambled}, "MomentumConservation" -> False];
  check["trace matches ordinary inference", trace["Result"] === UnscrambleSpinorAmplitudes[{rules, scrambled}, "MomentumConservation" -> False]];
  check["trace accepted checkpoint", TrueQ[trace["Checkpoints"][[1, "Accepted"]]]];
  check["trace stop action", AnyTrue[trace["Trace"], #["Candidate"]["Name"] === "Stop" &]];
  check["trace result preserves value", numeric[trace["Result"][[2]] - scrambled] === 0]
];

Print[<|"Checks" -> checks, "Failed" -> Length[failures], "Failures" -> failures|>];
Quit[If[failures === {}, 0, 1]];
