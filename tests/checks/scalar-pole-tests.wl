(* Ordinary scalar Schouten/virtuality cluster regressions. No internal cut,
   fitted amplitude, saved certificate or process-specific rule is used. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]]);

scalarFixture[labels : {a_, b_, j_, k_}, {ma_, mb_}] := Module[
  {pa, pb, pc, qa, qb, qc, z1, z2, ja, jb, ka, kb, eps, adj, p, q,
   angleBasis, squareBasis, angles, squares, chart, ang, sq, ab, sb, ch,
   carrier, d, u, v, e, x, h, bracket, y, z, aj, bj, higher, inventory,
   expression, pole, regular, external},
  eps = {{0, 1}, {-1, 0}};
  adj[m_] := {{m[[2, 2]], -m[[1, 2]]}, {-m[[2, 1]], m[[1, 1]]}};
  p = {{pa, pb}, {pc, (ma^2 + pb pc)/pa}};
  q = {{qa, qb}, {qc, (mb^2 + qb qc)/qa}};
  angleBasis = AssociationThread[{a, b}, {{{ma, 0}, {0, 1}}, {{mb, 0}, {0, 1}}}];
  squareBasis = AssociationThread[{a, b},
    {eps.adj[p].angleBasis[a]/ma, eps.adj[q].angleBasis[b]/mb}];
  angles = AssociationThread[labels,
    {angleBasis[a].{1, z1}, angleBasis[b].{1, z2}, {ja, jb}, {1, 0}}];
  squares = AssociationThread[labels,
    {squareBasis[a].{1, z1}, squareBasis[b].{1, z2}, {0, 1}, {ka, kb}}];
  chart = <|"Momenta" -> AssociationThread[labels,
      {p, q, Outer[Times, angles[j], squares[j]], Outer[Times, angles[k], squares[k]]}],
    "Angle" -> angles, "Square" -> squares,
    "Masses" -> AssociationThread[labels, {ma, mb, 0, 0}],
    "AngleBasis" -> angleBasis, "SquareBasis" -> squareBasis,
    "SpinVariables" -> {z1, z2}, "PoleVariable" -> Unique["unused"],
    "ReferenceSeed" -> 0, "CoherentVectorSpinIndices" -> True|>;
  ang[i_] := Spinor[If[MemberQ[{a, b}, i], "Spin", "Helicity"], "Angle", i];
  sq[i_] := Spinor[If[MemberQ[{a, b}, i], "Spin", "Helicity"], "Square", i];
  ab[i_, l_] := SpinorChain[ang[i], ang[l]];
  sb[i_, l_] := SpinorChain[sq[i], sq[l]];
  ch[i_, ps_List, l_] := SpinorChain @@ Join[{ang[i]}, Mom /@ ps, {sq[l]}];
  carrier = ab[a, k] ab[b, k]; d = ab[a, b];
  u = ch[k, {a}, j]; v = ch[k, {b}, j];
  e = SpinorChain[ang[k], Mom[a], Mom[b], ang[k]];
  x = SpinorChain[sq[j], Mom[a], Mom[b], sq[j]];
  h = sb[j, k]; bracket = ab[j, k];
  y = PropDen[Mom[Multiparticle @@ Sort[{a, k}]], ma];
  z = PropDen[Mom[Multiparticle @@ Sort[{b, k}]], mb];
  aj = PropDen[Mom[Multiparticle @@ Sort[{a, j}]], ma];
  bj = PropDen[Mom[Multiparticle @@ Sort[{b, j}]], mb];
  higher = PropDen[Mom[Multiparticle @@ Sort[{b, j, k}]], mb];
  inventory = {y, z, aj, bj, higher};
  expression = (x - h u) (y carrier + d e)/(y z higher) +
    d (e x + bracket u x - bj u^2)/(y bj higher) + carrier x/(bj higher);
  pole = -aj v (h carrier + d v)/(bj z higher);
  regular = x (y carrier + d e)/(y bj z);
  external = Thread[(Mass /@ labels) -> {ma, mb, 0, 0}];
  <|"Amplitude" -> expression, "ExpectedPole" -> pole, "ExpectedRegular" -> regular,
    "Higher" -> higher, "Inventory" -> inventory, "MassRules" -> external, "Chart" -> chart|>
];

exactScalarZero[expression_, chart_] := TrueQ[Cancel[Together[
  ConstructiveDiagrams`Private`spuriousEvaluateCurve[expression, chart, {}]]] === 0];
noScopedVirtuality[value_] := FreeQ[value,
  symbol_Symbol /; StringStartsQ[SymbolName[Unevaluated[symbol]], "scalarPhysicalVirtuality"]];
replayScalarReport[report_] := Module[{current = report["InitialNumerator"]},
  Do[current = Expand[current - edit["Cofactor"] edit["Relation"]["Identity"]],
    {edit, report["Route"]}];
  Expand[current - report["Numerator"]] === 0 &&
    Expand[report["Numerator"] - report["Quotient"] report["HigherForTest"] - report["Remainder"]] === 0
];

Do[
  fixture = scalarFixture[labels, {scalarMass, scalarMass}];
  report = ConstructiveDiagrams`Private`preparationScalarPoleBlock[
    fixture["Amplitude"], fixture["Higher"], fixture["MassRules"], fixture["Inventory"], False, 8];
  check[{labels, "strict ordinary improvement"}, report["Status"] === "IMPROVED" &&
    ConstructiveDiagrams`Private`preparationBetterQ[report["Rank"],
      ConstructiveDiagrams`Private`preparationRank[fixture["Amplitude"]]]];
  check[{labels, "all contributions retained"}, exactScalarZero[
    report["DecompositionAmplitude"] - fixture["Amplitude"], fixture["Chart"]]];
  check[{labels, "expected compact pole"}, exactScalarZero[
    report["PoleContribution"] - fixture["ExpectedPole"], fixture["Chart"]]];
  check[{labels, "nonzero regular remainder retained"}, report["RegularRemainder"] =!= 0 &&
    FreeQ[report["RegularRemainder"], fixture["Higher"]] && exactScalarZero[
      report["RegularRemainder"] - fixture["ExpectedRegular"], fixture["Chart"]]];
  check[{labels, "formal replay and division"}, TrueQ[report["PolynomialDivisionCertificate"]] &&
    replayScalarReport[Append[report, "HigherForTest" -> fixture["Higher"]]]];
  check[{labels, "no scoped tokens escape"}, noScopedVirtuality[report]];
  check[{labels, "no internal shell used"}, report["InternalOnShellConditions"] === {}];
  check[{labels, "no new kinematic inverse"},
    AllTrue[Cases[report["DecompositionAmplitude"], Power[base_, power_Integer?Negative] :> base, Infinity],
      NumberQ[#] || MemberQ[fixture["Inventory"], #] || # === scalarMass &]],
  {labels, {{1, 2, 3, 4}, {7, 4, 9, 2}}}
];

fixture = scalarFixture[{1, 2, 3, 4}, {scalarMassA, scalarMassB}];
relations = ConstructiveDiagrams`Private`preparationScalarSchoutenRelations[
  fixture["Amplitude"], fixture["Higher"], fixture["MassRules"], fixture["Inventory"], False, 2];
check["at least three independently generated relations", Length[relations] >= 3];
check["unequal masses remain exact", AllTrue[relations,
  exactScalarZero[#["Identity"], fixture["Chart"]] &]];
check["all relations use only the selected physical virtuality",
  AllTrue[Select[relations, #["Origin"]["Operation"] === "MasslessBracketAndPhysicalVirtuality" &],
    #["Origin"]["Virtuality"]["Propagator"] === fixture["Higher"] &&
      !TrueQ[#["Origin"]["Virtuality"]["MomentumConservationUsed"]] &]];
check["higher propagator is not assumed zero", !exactScalarZero[fixture["Higher"], fixture["Chart"]]];

unequalReport = ConstructiveDiagrams`Private`preparationScalarPoleBlock[
  fixture["Amplitude"], fixture["Higher"], fixture["MassRules"], fixture["Inventory"], False, 8];
check["unequal-mass cluster improves without a selected cut", unequalReport["Status"] === "IMPROVED"];
check["unequal-mass pole and regular pieces are exact", exactScalarZero[
  unequalReport["PoleContribution"] - fixture["ExpectedPole"], fixture["Chart"]] &&
  exactScalarZero[unequalReport["RegularRemainder"] - fixture["ExpectedRegular"], fixture["Chart"]]];

zeroBudget = ConstructiveDiagrams`Private`preparationScalarPoleBlock[
  fixture["Amplitude"], fixture["Higher"], fixture["MassRules"], fixture["Inventory"], False, 0];
check["zero budget preserves input", zeroBudget["Amplitude"] === fixture["Amplitude"] &&
  zeroBudget["Route"] === {} && zeroBudget["RegularRemainder"] === 0];
tinyBudget = ConstructiveDiagrams`Private`preparationScalarPoleBlock[
  fixture["Amplitude"], fixture["Higher"], fixture["MassRules"], fixture["Inventory"], False, 0.000001];
check["tiny budget returns a complete checkpoint", noScopedVirtuality[tinyBudget] &&
  ConstructiveDiagrams`Private`residueFiniteQ[tinyBudget["Amplitude"]] &&
  TrueQ[tinyBudget["PolynomialDivisionCertificate"]]];

badRules = fixture["MassRules"] /. HoldPattern[Mass[1] -> _] :> (Mass[1] -> {1, 2});
check["nonscalar mass rejected before rule construction",
  ConstructiveDiagrams`Private`preparationScalarSchoutenRelations[
    fixture["Amplitude"], fixture["Higher"], badRules, fixture["Inventory"], False, 1] === {}];
badReport = ConstructiveDiagrams`Private`preparationScalarPoleBlock[
  fixture["Amplitude"], fixture["Higher"], badRules, fixture["Inventory"], False, 1];
check["nonscalar mass cannot alter amplitude", badReport["Amplitude"] === fixture["Amplitude"] &&
  badReport["Status"] === "UNCHANGED"];
massiveEndpoint = ReplacePart[fixture["MassRules"], 4 -> (Mass[4] -> scalarMassK)];
massiveRelations = ConstructiveDiagrams`Private`preparationScalarSchoutenRelations[
  fixture["Amplitude"], fixture["Higher"], massiveEndpoint, fixture["Inventory"], False, 1];
check["massive endpoint does not license massless bracket rule",
  FreeQ[Lookup[Lookup[massiveRelations, "Origin", {}], "Operation", {}],
    "MasslessBracketAndPhysicalVirtuality"]];

Print[<|"Checks" -> checks, "Failed" -> Length[failures], "Failures" -> failures|>];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:scalar-pole-tests.wl"]];
Quit[If[failures === {}, 0, 1]];
