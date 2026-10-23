(* Diagram construction, external factors, masses, helicity sums, and signs. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
ubModel = ReadModel[FileNameJoin[{root, "models"}], 1];
ubCatalog = Get[FileNameJoin[{root, "tests", "simplification-paths.wl"}]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]]);
vertexSlotRules = <|1 -> 7, 2 -> Multiparticle[3, 4], 3 -> 5|>;
vertexRelabelCases = {
  {Spinor["Spin", "Upper", "Angle", 1, 2], Spinor["Spin", "Upper", "Angle", 7, 2]},
  {SpinorHat["Spin", "Lower", "Square", 2, 3],
    SpinorHat["Spin", "Lower", "Square", Multiparticle[3, 4], 3]},
  {Spinor["Xi", "Angle", 1], Spinor["Xi", "Angle", 1]},
  {Mom[1], Mom[7]}, {MomHat[2], MomHat[Multiparticle[3, 4]]}, {MomQ[3], MomQ[5]},
  {Mom[Multiparticle[1, 2]], Mom[Multiparticle[7, 3, 4]]},
  {Mass[Multiparticle[1, 2]], Mass[Multiparticle[7, 3, 4]]},
  {MomProd[1, 2], MomProd[7, Multiparticle[3, 4]]},
  {Mandelstahm[1, 3], Mandelstahm[7, 5]},
  {xFactor[1, 2] xTildeFactor[3, 1],
    xFactor[7, Multiparticle[3, 4]] xTildeFactor[5, 7]}};
Do[
  check["diagram vertex relabels only typed particle positions " <> ToString[vertexCaseIndex],
    ConstructiveDiagrams`replaceVertexIndices[vertexRelabelCases[[vertexCaseIndex, 1]],
      vertexSlotRules] === vertexRelabelCases[[vertexCaseIndex, 2]]],
  {vertexCaseIndex, Length[vertexRelabelCases]}];
vertexMomentumModel = {
  MapIndexed[{#1, #1, #1, First[#2], 0, vertexMass[First[#2]], "0", 1, "", #1, #1} &,
    {"a", "b", "c", "d"}],
  {{"a", "b", "c", "d", 2, MomProd[1, 2]^2 + 3 Mass[1]^2}}, {}, {}};
vertexMomentumDiagrams = CreateDiagrams[{"b", "d", "a", "c"}, vertexMomentumModel];
check["permuted derivative contact generates exactly one diagram",
  Length[vertexMomentumDiagrams] === 1];
check["diagram amplitude relabels momenta and masses without changing coefficients or powers",
  Last[First[diagramAmplitudes[vertexMomentumDiagrams]]] ===
    2 (MomProd[3, 1]^2 + 3 vertexMass[1]^2)];
ubMediator[d_] := Module[{props = Values[d["Propagators"]], particle},
  If[props === {}, Return["contact"]];
  particle = First[props]["Particle"];
  Which[MemberQ[{"A", "A.+", "A.-"}, particle], "A",
    MemberQ[{"G", "G.+", "G.-"}, particle], "G", True, particle]
];
ubSelect[diagrams_, key_, sumAll_] := Module[{parts = StringSplit[key, ":"]},
  If[TrueQ[sumAll], Return[diagrams]];
  Select[diagrams, ubMediator[#] === First[parts] &&
    (Length[parts] === 1 || diagramChannel[#] === Last[parts]) &]
];
ubReference[name_, key_] := Module[{source, matches},
  source = "SM-4-point-test.nb / " <> name <> " / " <> key;
  matches = Select[ubCatalog, MemberQ[#["Sources"], source] &];
  If[Length[matches] === 1, First[matches]["Amplitude"], Missing["Reference", source]]
];
verificationSeconds = 1;
ubCompare[got_, expected_, masses_] := Module[{check, assumptions, difference, sum},
  assumptions = MW == MZ CW &&
    Mandelstahm[1, 2] + Mandelstahm[1, 3] + Mandelstahm[1, 4] == Total[masses^2];
  check[expr_] := TimeConstrained[
    FullSimplify[ExtractMandelstahm[ReduceSpinorProducts[expr]], Assumptions -> assumptions],
    verificationSeconds, $TimedOut];
  If[got === expected, Return[<|"Equivalence" -> "Exact", "Residual" -> 0|>]];
  difference = check[got - expected];
  If[difference === 0, Return[<|"Equivalence" -> "Proved", "Residual" -> 0|>]];
  sum = check[got + expected];
  <|"Equivalence" -> Which[sum === 0, "OppositeSign",
    difference === $TimedOut || sum === $TimedOut, "VerificationTimeout", True, "Unresolved"],
    "Residual" -> difference|>
];
permutation = {3, 1, 4, 2};
externalX = ConstructiveDiagrams`expandExternalXFactors;
fivePointX = xFactor[1, Multiparticle[2, 3, 4]];
fivePointExpanded =
  SpinorChain[Spinor["Xi", "Angle", 10], Mom[Multiparticle[2, 3, 4]],
    Spinor["Helicity", "Square", 5]]/
  (Mass[Multiparticle[2, 3, 4]] SpinorChain[Spinor["Xi", "Angle", 10],
    Spinor["Helicity", "Angle", 5]]);
check["external x identifies fifth leg from composite indices",
  externalX[fivePointX, 5] === fivePointExpanded];
sixPointTilde = xTildeFactor[Multiparticle[1, 2, 3], Multiparticle[4, 5]];
sixPointExpanded =
  SpinorChain[Spinor["Xi", "Square", 10], Mom[Multiparticle[4, 5]],
    Spinor["Helicity", "Angle", 6]]/
  (Mass[Multiparticle[4, 5]] SpinorChain[Spinor["Xi", "Square", 10],
    Spinor["Helicity", "Square", 6]]);
check["external tilde identifies sixth leg from composite indices",
  externalX[sixPointTilde, 6] === sixPointExpanded];
ambiguousX = xFactor[1, 2] + xTildeFactor[1, 2];
check["ambiguous external factors keep their original heads", externalX[ambiguousX, 4] === ambiguousX];
mixedExternalX = externalX[fivePointX xTildeFactor[Multiparticle[1, 2, 3], 4], 5];
check["mixed external factors use distinct reference spinors",
  FreeQ[mixedExternalX, _xFactor | _xTildeFactor] &&
    Sort[DeleteDuplicates[Cases[mixedExternalX, Spinor["Xi", _, i_Integer] :> i, Infinity]]] === {10, 11}];
denominatorX = fivePointX/PropDen[Mom[Multiparticle[1, 2, 5]], MZ];
check["external factor expansion preserves propagator denominators",
  Cases[externalX[denominatorX, 5], _PropDen, Infinity] === Cases[denominatorX, _PropDen, Infinity]];
check["legacy external expansion shares the same replacement", ExpandXFactors[denominatorX, 5] === externalX[denominatorX, 5]];
internalXIdentity =
  xFactor[1, 2] xTildeFactor[3, 4]*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] +
  xFactor[3, 4] xTildeFactor[1, 2]*
    SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]];
check["external helper does not apply internal exchange identities", externalX[internalXIdentity, 4] === internalXIdentity];
rawDiagramAmplitude[diag_] := Block[
  {ConstructiveDiagrams`expandExternalXFactors, ConstructiveDiagrams`diagramMassReplacements},
  Clear[ConstructiveDiagrams`expandExternalXFactors];
  Clear[ConstructiveDiagrams`diagramMassReplacements];
  ConstructiveDiagrams`expandExternalXFactors[expr_, n_] := expr;
  ConstructiveDiagrams`diagramMassReplacements[_] := {};
  diagramAmplitude[diag]
];
Do[
  xDiagrams = TimeConstrained[CreateDiagrams[xParticles, ubModel], 20, $Failed];
  check["external-factor fixture generates diagrams " <> ToString[xParticles, InputForm],
    ListQ[xDiagrams] && Length[xDiagrams] > 0];
  If[ListQ[xDiagrams] && Length[xDiagrams] > 0,
    rawXAmplitudes = rawDiagramAmplitude /@ xDiagrams;
    expandedXAmplitudes = diagramAmplitude /@ xDiagrams;
    unresolvedXAmplitudes = externalX[#, Length[xParticles]] & /@ rawXAmplitudes;
    expectedXAmplitudes = MapThread[
      (#1 /. ConstructiveDiagrams`diagramMassReplacements[#2]) &,
      {unresolvedXAmplitudes, xDiagrams}];
    check["external-factor fixture has raw x factors " <> ToString[xParticles, InputForm],
      !FreeQ[rawXAmplitudes, _xFactor | _xTildeFactor]];
    check["diagram amplitude expands external factors then its own mass rules " <> ToString[xParticles, InputForm],
      expandedXAmplitudes === expectedXAmplitudes &&
        FreeQ[expandedXAmplitudes, _xFactor | _xTildeFactor]];
    check["external-factor expansion introduces known mass placeholders " <> ToString[xParticles, InputForm],
      !FreeQ[unresolvedXAmplitudes, Mass[_Integer]] &&
        !FreeQ[unresolvedXAmplitudes, Mass[_Multiparticle]]];
    check["diagram amplitude resolves external and multiparticle masses " <> ToString[xParticles, InputForm],
      FreeQ[expandedXAmplitudes, _Mass]];
    bulkXAmplitudes = diagramAmplitudes[xDiagrams];
    check["unpaired bulk diagrams retain their rules and resolved expressions " <> ToString[xParticles, InputForm],
      bulkXAmplitudes === MapThread[
        {ConstructiveDiagrams`diagramMassReplacements[#1], #2} &, {xDiagrams, expectedXAmplitudes}]];
    check["diagram propagators survive external-factor expansion " <> ToString[xParticles, InputForm],
      Cases[expandedXAmplitudes, _PropDen, Infinity] === Cases[rawXAmplitudes, _PropDen, Infinity]]
  ],
  {xParticles, {{"e", "E", "A.+", "h"}, {"A.-", "E", "e", "h"},
    {"u", "U", "G.-", "h"}, {"e", "E", "A.+", "h", "h"}}}
];
mixedMassDiagrams = Join @@ (CreateDiagrams[#, ubModel] & /@
  {{"e", "E", "A.+", "h"}, {"u", "U", "A.+", "h"}});
mixedMassExpected = Function[diag, Module[{rules = ConstructiveDiagrams`diagramMassReplacements[diag]},
  {rules, externalX[rawDiagramAmplitude[diag], 4] /. rules}
]] /@ mixedMassDiagrams;
check["mixed diagram fixtures have different masses for the same leg",
  DeleteDuplicates[Mass[2] /. mixedMassExpected[[All, 1]]] === {Me, Mu}];
check["bulk amplitudes use per-diagram masses rather than a shared rule union",
  diagramAmplitudes[mixedMassDiagrams] === mixedMassExpected];
Do[
  pairedMassDiagrams = ubSelect[CreateDiagrams[pairedParticles[[pairedPermutation]], ubModel], "A", False];
  check["massless-vector mass fixture has generic diagrams " <> ToString[{pairedParticles, pairedPermutation}, InputForm],
    Length[pairedMassDiagrams] > 0 && AllTrue[pairedMassDiagrams,
      Length[Lookup[#, "HelicityAssignments", {}]] === 2 &&
        AllTrue[Values[#["Propagators"]], Lookup[#, "Particle"] === "A" &] &]];
  Do[
    pairedMassRules = ConstructiveDiagrams`diagramMassReplacements[pairedMassDiagram];
    unresolvedPairedMass = Block[{ConstructiveDiagrams`diagramMassReplacements},
      Clear[ConstructiveDiagrams`diagramMassReplacements];
      ConstructiveDiagrams`diagramMassReplacements[_] := {};
      diagramAmplitude[pairedMassDiagram]
    ];
    resolvedPairedMass = diagramAmplitudes[{pairedMassDiagram}];
    check["paired vector shortcut introduces a known external mass " <> ToString[{pairedParticles, pairedPermutation, pairedMassRules}, InputForm],
      !FreeQ[unresolvedPairedMass, Mass[_Integer]]];
    check["paired vector shortcut resolves its mass and retains its rules " <> ToString[{pairedParticles, pairedPermutation, pairedMassRules}, InputForm],
      resolvedPairedMass === {{pairedMassRules, unresolvedPairedMass /. pairedMassRules}} &&
        FreeQ[resolvedPairedMass[[All, 2]], _Mass]];
    check["paired vector mass replacement preserves propagators " <> ToString[{pairedParticles, pairedPermutation, pairedMassRules}, InputForm],
      Cases[resolvedPairedMass[[All, 2]], _PropDen, Infinity] ===
        Cases[unresolvedPairedMass, _PropDen, Infinity]],
    {pairedMassDiagram, pairedMassDiagrams}
  ],
  {pairedParticles, {{"e", "E", "W+", "W-"}, {"W+", "W-", "W+", "W-"}}},
  {pairedPermutation, {Range[4], permutation}}
];
test = <|"Particles" -> {"u", "U", "C", "c"}|>;
diagrams = CreateDiagrams[test["Particles"], ubModel];
photon = ubSelect[diagrams, "A", False];
check["real diagram selection", Length[photon] > 0 && AllTrue[photon, ubMediator[#] === "A" &]];
pairedPhotonAmplitudes = diagramAmplitudes[photon];
resolvedHelicityDiagrams[diag_] :=
  ConstructiveDiagrams`resolveDiagramHelicities[diag, #] & /@
    Lookup[diag, "HelicityAssignments", {}];
exactGenericHelicitySum[diag_] := Block[
  {ConstructiveDiagrams`genericFermionCurrentAmplitude, ConstructiveDiagrams`genericMasslessSpin1Amplitude},
  Clear[ConstructiveDiagrams`genericFermionCurrentAmplitude, ConstructiveDiagrams`genericMasslessSpin1Amplitude];
  ConstructiveDiagrams`genericFermionCurrentAmplitude[_] := Missing["TestRawHelicitySum"];
  ConstructiveDiagrams`genericMasslessSpin1Amplitude[_, _] := Missing["TestRawHelicitySum"];
  diagramAmplitude[diag]
];
referenceAngle[Spinor[k_, _, index_]] := Spinor[k, "Angle", index];
referenceSquare[Spinor[k_, _, index_]] := Spinor[k, "Square", index];
referenceAngle[Spinor[k_, ul : ("Upper" | "Lower"), _, index_, rest___]] :=
  Spinor[k, ul, "Angle", index, rest];
referenceSquare[Spinor[k_, ul : ("Upper" | "Lower"), _, index_, rest___]] :=
  Spinor[k, ul, "Square", index, rest];
(* Extract the four matter spinors from the original vertex expressions,
   retaining their explicit index polarity and the original propagator factors. *)
independentFermionCurrentReference[diag_] := Module[
  {processed, expressions, edges, used = {}, currents = {}, ends, records,
   spinors, a, b, c, d, aa, ss, current, result},
  processed = ConstructiveDiagrams`makeDiagramSpinIndicesExplicit[First[resolvedHelicityDiagrams[diag]]];
  expressions = Association @ KeyValueMap[#1 -> Last[#2]["Expression"] &,
    processed["VertexStructures"]];
  edges = diag["HelicitySumEdges"];
  Do[
    ends = Keys[diag["InternalEdgeIndices"][edge]];
    If[!FreeQ[used, Alternatives @@ ends], Return[Missing["OverlappingVertices"]]];
    records = Last[processed["VertexStructures"][#]] & /@ ends;
    spinors = (List @@ First[Cases[#["Expression"], _SpinorChain, Infinity]]) & /@ records;
    If[Length /@ spinors =!= {2, 2} || !AllTrue[Flatten[spinors], MatchQ[#, _Spinor] &],
      Return[Missing["NonFermionCurrent"]]];
    {{a, b}, {c, d}} = spinors;
    aa[x_, y_] := SpinorChain[referenceAngle[x], referenceAngle[y]];
    ss[x_, y_] := SpinorChain[referenceSquare[x], referenceSquare[y]];
    current = aa[a, d] ss[b, c] + aa[a, c] ss[b, d] +
      aa[b, d] ss[a, c] + aa[b, c] ss[a, d];
    AppendTo[currents, -(Times @@ (#["ModelRow"][[-2]] & /@ records)) current];
    used = Join[used, ends],
    {edge, edges}];
  result = ConstructiveDiagrams`diagramFermionSign[diag] Times @@ Join[
    Values[KeyDrop[expressions, used]], currents,
    Lookup[Values[processed["Propagators"]], "Factor"]]/I;
  ConstructiveDiagrams`alignInternalMultiparticles[result,
    Values /@ Values[diag["InternalEdgeIndices"]]] /.
    ConstructiveDiagrams`diagramMassReplacements[diag]
];
photonComponents = Join @@ (resolvedHelicityDiagrams /@ photon);
check["photon exchange is one generic diagram with two correlated assignments",
  Length[photon] === 1 && Length[photonComponents] === 2 &&
    Lookup[First[photon], "ExternalParticles"] === test["Particles"] &&
    Lookup[Values[First[photon]["Propagators"]], "Particle"] === {"A"}];
rawPairedPhotonAmplitudes = Block[{ConstructiveDiagrams`expandExternalXFactors},
  Clear[ConstructiveDiagrams`expandExternalXFactors];
  ConstructiveDiagrams`expandExternalXFactors[expr_, n_] := expr;
  diagramAmplitudes[photon]
];
check["generic internal massless-vector shortcut stays unchanged and x free",
  Length[pairedPhotonAmplitudes] === 1 && pairedPhotonAmplitudes === rawPairedPhotonAmplitudes &&
    FreeQ[pairedPhotonAmplitudes, _xFactor | _xTildeFactor]];
check["generic singular and plural APIs give the same helicity sum",
  diagramAmplitude[First[photon]] === pairedPhotonAmplitudes[[1, 2]]];
check["generic diagrams preserve their mass rules", pairedPhotonAmplitudes[[1, 1]] ===
  ConstructiveDiagrams`diagramMassReplacements[First[photon]]];
unsupportedPairAmplitudes = Block[{ConstructiveDiagrams`masslessSpin1PairAmplitude},
  Clear[ConstructiveDiagrams`masslessSpin1PairAmplitude];
  ConstructiveDiagrams`masslessSpin1PairAmplitude[_] := Missing["UnsupportedMasslessSpin1"];
  diagramAmplitudes[photon]
];
check["unsupported shortcut retains both correlated helicity contributions in one entry",
  Length[unsupportedPairAmplitudes] === 1 &&
    unsupportedPairAmplitudes[[1, 1]] === pairedPhotonAmplitudes[[1, 1]] &&
    ConstructiveDiagrams`Private`samePoly[unsupportedPairAmplitudes[[1, 2]],
      Total[diagramAmplitude /@ photonComponents]]];
fivePointPairDiagrams = TimeConstrained[
  CreateDiagrams[{"e", "E", "M", "m", "h"}, ubModel], 20, $Failed];
check["five-point pairing fixture generates diagrams", ListQ[fivePointPairDiagrams] &&
  fivePointPairDiagrams =!= {}];
If[ListQ[fivePointPairDiagrams],
  fivePointPhotonDiagrams = Select[fivePointPairDiagrams,
    ConstructiveDiagrams`masslessSpin1Edge[#] =!= None &];
  check["five-point fixture contains generic internal photons", Length[fivePointPhotonDiagrams] > 1 &&
    AllTrue[fivePointPhotonDiagrams, Length[Lookup[#, "HelicityAssignments", {}]] === 2 &]];
  fivePointIndividualAmplitudes =
    {ConstructiveDiagrams`diagramMassReplacements[#], diagramAmplitude[#]} & /@ fivePointPhotonDiagrams;
  fivePointPairedAmplitudes = diagramAmplitudes[fivePointPhotonDiagrams];
  check["four-point shortcut rejects higher-point input", AllTrue[fivePointPhotonDiagrams,
    MissingQ[ConstructiveDiagrams`masslessSpin1PairAmplitude[#]] &]];
  check["five-point API preserves all amplitudes and mass rules",
    fivePointPairedAmplitudes === fivePointIndividualAmplitudes];
  check["higher-point retention does not depend on diagram order",
    diagramAmplitudes[Reverse[fivePointPhotonDiagrams]] === Reverse[fivePointIndividualAmplitudes]];
];
partialPhoton = Join[First[photon], <|"HelicityAssignments" ->
  Take[First[photon]["HelicityAssignments"], 1]|>];
partialPhotonAmplitude = diagramAmplitude[partialPhoton];
check["incomplete generic assignment cannot trigger the complete photon shortcut",
  ConstructiveDiagrams`Private`samePoly[partialPhotonAmplitude, diagramAmplitude[First[photonComponents]]] &&
    !FreeQ[partialPhotonAmplitude, _xFactor | _xTildeFactor]];
check["legacy resolved list still combines the complete four-point photon exchange",
  diagramAmplitudes[photonComponents] === pairedPhotonAmplitudes];
check["legacy missing-partner list retains its one resolved component",
  diagramAmplitudes[Take[photonComponents, 1]] ===
    {{ConstructiveDiagrams`diagramMassReplacements[First[photonComponents]],
      diagramAmplitude[First[photonComponents]]}}];
massiveIndexEdgeA = UndirectedEdge[7, 8];
massiveIndexEdgeB = UndirectedEdge[8, 9];
massiveIndexFixture = <|
  "Propagators" -> <|
    massiveIndexEdgeA -> <|"Mass" -> Me, "Spin2" -> 1,
      "ParticleVertex" -> 8, "AntiparticleVertex" -> 7|>,
    massiveIndexEdgeB -> <|"Mass" -> MW, "Spin2" -> 2,
      "ParticleVertex" -> 9, "AntiparticleVertex" -> 8|>|>,
  "InternalEdgeIndices" -> <|
    massiveIndexEdgeA -> <|7 -> Multiparticle[3, 4, 5, 6], 8 -> Multiparticle[1, 2]|>,
    massiveIndexEdgeB -> <|8 -> Multiparticle[5, 6], 9 -> Multiparticle[1, 2, 3, 4]|>|>,
  "VertexStructures" -> <|
    7 -> {<|"Expression" -> SpinorChain[Spinor["Spin", "Angle", 1],
      Spinor["Spin", "Angle", Multiparticle[3, 4, 5, 6]]]|>},
    8 -> {<|"Expression" -> SpinorChain[Spinor["Spin", "Angle", 2],
      Spinor["Spin", "Angle", Multiparticle[1, 2]]]*
      SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", Multiparticle[5, 6]]]^2|>},
    9 -> {<|"Expression" -> SpinorChain[Spinor["Spin", "Angle", 4],
      Spinor["Spin", "Angle", Multiparticle[1, 2, 3, 4]]]^2|>}|>|>;
massiveIndicesExplicit = ConstructiveDiagrams`makeDiagramSpinIndicesExplicit[massiveIndexFixture];
massiveIndexLists = Values[massiveIndicesExplicit["SpinIndices"]];
check["distinct internal massive lines use disjoint spin dummy indices",
  Length /@ massiveIndexLists === {1, 2} && DuplicateFreeQ[Flatten[massiveIndexLists]]];
massiveSharedExpression = Last[massiveIndicesExplicit["VertexStructures"][8]]["Expression"];
check["making one antiparticle line explicit does not flip another line's upper index",
  Cases[massiveSharedExpression, Spinor["Spin", kind_, _, Multiparticle[1, 2], _] :> kind,
    Infinity] === {"Upper"} &&
    Cases[massiveSharedExpression, Spinor["Spin", kind_, _, Multiparticle[5, 6], _] :> kind,
      Infinity] === {"Lower", "Lower"}];
sixPointMasslessPartitions = {{Multiparticle[1, 2], Multiparticle[3, 4, 5, 6]},
  {Multiparticle[1, 2, 3, 4], Multiparticle[5, 6]}};
sixPointMasslessProduct =
  SpinorChain[Spinor["Helicity", "Angle", Multiparticle[1, 2]], Spinor["Helicity", "Square", 1]]*
  SpinorChain[Spinor["Helicity", "Angle", Multiparticle[3, 4, 5, 6]], Spinor["Helicity", "Square", 2]]*
  SpinorChain[Spinor["Helicity", "Square", Multiparticle[1, 2, 3, 4]], Spinor["Helicity", "Angle", 3]]*
  SpinorChain[Spinor["Helicity", "Square", Multiparticle[5, 6]], Spinor["Helicity", "Angle", 4]];
sixPointAligned = ConstructiveDiagrams`alignInternalMultiparticles[sixPointMasslessProduct,
  sixPointMasslessPartitions];
sixPointAlignmentExpected = -sixPointMasslessProduct /.
  {Multiparticle[3, 4, 5, 6] -> Multiparticle[1, 2],
    Multiparticle[1, 2, 3, 4] -> Multiparticle[5, 6]};
check["six-point massless alignment follows actual complementary edge partitions",
  sixPointAligned === sixPointAlignmentExpected &&
    DeleteDuplicates[Cases[sixPointAligned, Spinor["Helicity", _, mp_Multiparticle] :> mp,
      Infinity]] === {Multiparticle[1, 2], Multiparticle[5, 6]}];
check["generic photon drawing retains external arrows but has no helicity arrow",
  Count[CreateArrowDiagram[First[photon]], _Arrow, Infinity] === Length[test["Particles"]] &&
    AllTrue[First[photon]["HelicitySumEdges"],
      MissingQ[ConstructiveDiagrams`arrowDirectionData[#, First[photon]["InternalEdgeLabels"]]] &]];
genericAssignmentQ[diag_] := Module[{edges = Lookup[diag, "HelicitySumEdges", {}], assignments},
  assignments = Lookup[diag, "HelicityAssignments", {}];
  edges =!= {} && assignments =!= {} && DuplicateFreeQ[assignments] &&
    AllTrue[assignments, AssociationQ[#] && Sort[Keys[#]] === Sort[edges] &] &&
    AllTrue[edges, Function[edge, With[{prop = diag["Propagators"][edge]},
      MemberQ[{"A", "G"}, prop["Particle"]] && prop["Mass"] === 0 && prop["Spin2"] === 2]]]
];
resolvedAssignmentQ[diag_, assignment_] := Module[{resolved, props, vertexRecords},
  resolved = ConstructiveDiagrams`resolveDiagramHelicities[diag, assignment];
  props = Values[resolved["Propagators"]];
  vertexRecords = Values[resolved["VertexStructures"]];
  !KeyExistsQ[resolved, "HelicityAssignments"] && !KeyExistsQ[resolved, "HelicitySumEdges"] &&
    resolved["ExternalParticles"] === diag["ExternalParticles"] &&
    ConstructiveDiagrams`diagramMassReplacements[resolved] ===
      ConstructiveDiagrams`diagramMassReplacements[diag] &&
    AllTrue[vertexRecords, # =!= {} && AllTrue[#, Function[record,
      AllTrue[Normal[Lookup[record, "InternalHelicities", <||>]],
        Lookup[assignment, First[#], None] === Last[#] &] &&
      Lookup[record["IncidentLegs"], "Particle"] ===
        Take[record["ModelRow"], Length[record["IncidentLegs"]]]]] &]
];
Do[
  multiVectorDiagrams = TimeConstrained[CreateDiagrams[multiVectorFixture[[2]], ubModel], 20, $Failed];
  check["six-fermion fixture generates diagrams " <> multiVectorFixture[[1]], ListQ[multiVectorDiagrams]];
  If[ListQ[multiVectorDiagrams],
    multiVectorSelected = Select[multiVectorDiagrams, Function[diag,
      Length[Lookup[diag, "HelicitySumEdges", {}]] === 2 &&
        AllTrue[Lookup[diag, "HelicitySumEdges", {}],
          diag["Propagators"][#, "Particle"] === multiVectorFixture[[3]] &]]];
    check["six-fermion fixture has six two-vector topologies " <> multiVectorFixture[[1]],
      Length[multiVectorSelected] === 6];
    Do[
      multiVectorLabel = multiVectorFixture[[1]] <> " / " <> ToString[multiVectorIndex];
      multiVectorDiagram = multiVectorSelected[[multiVectorIndex]];
      multiVectorAssignments = multiVectorDiagram["HelicityAssignments"];
      check["two-vector generic metadata " <> multiVectorLabel,
        genericAssignmentQ[multiVectorDiagram] && Length[multiVectorAssignments] === 4];
      check["all two-vector assignments restrict the vertices coherently " <> multiVectorLabel,
        AllTrue[multiVectorAssignments, resolvedAssignmentQ[multiVectorDiagram, #] &]];
      multiVectorComponents = resolvedHelicityDiagrams[multiVectorDiagram];
      multiVectorAmplitude = diagramAmplitude[multiVectorDiagram];
      check["two-vector amplitude is the complete correlated sum " <> multiVectorLabel,
        ConstructiveDiagrams`Private`samePoly[exactGenericHelicitySum[multiVectorDiagram],
          Total[diagramAmplitude /@ multiVectorComponents]]];
      check["two disjoint fermion currents remove internal x factors " <> multiVectorLabel,
        FreeQ[multiVectorAmplitude, _xFactor | _xTildeFactor]];
      multiVectorReference = independentFermionCurrentReference[multiVectorDiagram];
      check["two-vector sewing agrees with independently extracted matter currents " <> multiVectorLabel,
        FreeQ[multiVectorReference, _Missing] &&
          Expand[ReduceSpinorProducts[multiVectorAmplitude - multiVectorReference]] === 0];
      check["two-vector singular and bulk APIs agree " <> multiVectorLabel,
        diagramAmplitudes[{multiVectorDiagram}] ===
          {{ConstructiveDiagrams`diagramMassReplacements[multiVectorDiagram], multiVectorAmplitude}}];
      check["two-vector output retains every off-shell propagator " <> multiVectorLabel,
        Sort[DeleteDuplicates[Cases[multiVectorAmplitude, _PropDen, Infinity]]] ===
          Sort[DeleteDuplicates[Cases[Lookup[Values[multiVectorDiagram["Propagators"]], "Factor"],
            _PropDen, Infinity]]]],
      {multiVectorIndex, Length[multiVectorSelected]}]
  ],
  {multiVectorFixture, {{"photons", {"e", "E", "m", "M", "l", "L"}, "A"},
    {"gluons", {"u", "U", "d", "D", "c", "C"}, "G"}}}
];
Do[
  fixedHelicityParticles = {"e", "E", "M", "m", fixedHelicity};
  fixedHelicityDiagrams = TimeConstrained[CreateDiagrams[fixedHelicityParticles, ubModel], 20, $Failed];
  check["fixed external helicity fixture generates diagrams " <> fixedHelicity, ListQ[fixedHelicityDiagrams]];
  If[ListQ[fixedHelicityDiagrams],
    fixedHelicityPhotonDiagrams = Select[fixedHelicityDiagrams,
      Lookup[#, "HelicitySumEdges", {}] =!= {} &];
    check["fixed external helicity fixture has four generic photon topologies " <> fixedHelicity,
      Length[fixedHelicityPhotonDiagrams] === 4];
    check["internal grouping leaves the fifth external helicity exact " <> fixedHelicity,
      AllTrue[fixedHelicityDiagrams, #["ExternalParticles"] === fixedHelicityParticles &&
        #["ExternalAssignment"][5, "Particle"] === fixedHelicity &]];
    Do[
      check["fixed external helicity keeps both internal assignments " <>
          ToString[{fixedHelicity, fixedHelicityIndex}, InputForm],
        genericAssignmentQ[fixedHelicityPhotonDiagrams[[fixedHelicityIndex]]] &&
          Length[fixedHelicityPhotonDiagrams[[fixedHelicityIndex]]["HelicityAssignments"]] === 2 &&
          ConstructiveDiagrams`Private`samePoly[exactGenericHelicitySum[fixedHelicityPhotonDiagrams[[fixedHelicityIndex]]],
            Total[diagramAmplitude /@ resolvedHelicityDiagrams[fixedHelicityPhotonDiagrams[[fixedHelicityIndex]]]]]],
      {fixedHelicityIndex, Length[fixedHelicityPhotonDiagrams]}]
  ],
  {fixedHelicity, {"A.+", "A.-"}}
];
gluonParticles = {"G.+", "G.+", "G.-", "G.-", "G.+"};
gluonDiagrams = TimeConstrained[CreateDiagrams[gluonParticles, ubModel], 20, $Failed];
check["triple-gluon fixture generates generic diagrams", ListQ[gluonDiagrams] && gluonDiagrams =!= {}];
If[ListQ[gluonDiagrams] && gluonDiagrams =!= {},
  check["triple-gluon model forbids some naive independent helicity combinations",
    AnyTrue[gluonDiagrams, Length[#["HelicityAssignments"]] < 2^Length[#["HelicitySumEdges"]] &]];
  check["triple-gluon assignments use only model-allowed coherent vertices",
    AllTrue[gluonDiagrams, genericAssignmentQ[#] &&
      With[{diag = #}, AllTrue[diag["HelicityAssignments"], resolvedAssignmentQ[diag, #] &]] &]];
  check["triple-gluon summation preserves all fixed external helicities",
    AllTrue[gluonDiagrams, #["ExternalParticles"] === gluonParticles &]];
  check["triple-gluon fallback equals its allowed resolved sum",
    AllTrue[Take[gluonDiagrams, UpTo[3]],
      ConstructiveDiagrams`Private`samePoly[diagramAmplitude[#], Total[diagramAmplitude /@ resolvedHelicityDiagrams[#]]] &]]
];
sixGluonParticles = {"G.+", "G.-", "G.+", "G.-", "G.+", "G.-"};
sixGluonDiagrams = TimeConstrained[CreateDiagrams[sixGluonParticles, ubModel], 20, $Failed];
check["actual six-point massless alignment fixture generates diagrams",
  ListQ[sixGluonDiagrams] && sixGluonDiagrams =!= {}];
If[ListQ[sixGluonDiagrams] && sixGluonDiagrams =!= {},
  sixGluonDiagram = First[sixGluonDiagrams];
  sixGluonComponent = First[resolvedHelicityDiagrams[sixGluonDiagram]];
  sixGluonAmplitude = diagramAmplitude[sixGluonComponent];
  check["actual six-point component contains internal helicity spinors",
    !FreeQ[sixGluonAmplitude, Spinor["Helicity", _, _Multiparticle]]];
  check["actual six-point component aligns all three internal massless partitions",
    Length[sixGluonDiagram["HelicitySumEdges"]] === 3 &&
      AllTrue[Values[sixGluonDiagram["InternalEdgeIndices"]], Function[ends,
        With[{mps = Values[ends]}, With[{keep =
            ConstructiveDiagrams`preferredInternalMultiparticle @@ mps},
          FreeQ[sixGluonAmplitude, Spinor["Helicity", _, First[DeleteCases[mps, keep]]]]]]]]];
  check["actual six-point alignment preserves the three propagator denominators",
    Sort[DeleteDuplicates[Cases[sixGluonAmplitude, _PropDen, Infinity]]] ===
      Sort[DeleteDuplicates[Cases[Lookup[Values[sixGluonComponent["Propagators"]], "Factor"],
        _PropDen, Infinity]]]];
  check["generic gluon drawing has no internal helicity arrows",
    Count[CreateArrowDiagram[sixGluonDiagram], _Arrow, Infinity] === 6 &&
      AllTrue[sixGluonDiagram["HelicitySumEdges"],
        MissingQ[ConstructiveDiagrams`arrowDirectionData[#, sixGluonDiagram["InternalEdgeLabels"]]] &]]
];
cubicScalarModel = {{{"scalar", "s", "s", 0, 0, ms, 0, 1, "", "s", "s"}},
  {{"s", "s", "s", gs, 1}}, {}, {}};
Do[
  scalarDiagrams = TimeConstrained[CreateDiagrams[ConstantArray["s", scalarCount[[1]]],
    cubicScalarModel], 20, $Failed];
  check["complete cubic scalar topology count " <> ToString[scalarCount[[1]]],
    ListQ[scalarDiagrams] && Length[scalarDiagrams] === scalarCount[[2]]];
  If[scalarCount[[1]] === 6 && ListQ[scalarDiagrams],
    check["six-point cubic scalar diagrams include the three-internal-edge star",
      AnyTrue[scalarDiagrams, Count[VertexDegree[
        Graph[#["InternalVertices"], Keys[#["Propagators"]]]], 3] === 1 &]]],
  {scalarCount, {{3, 1}, {4, 3}, {5, 15}, {6, 105}}}
];
photonTest = <|"Particles" -> {"e", "E", "M", "m"},
  "SourceName" -> "e E -> M m", "Masses" -> {Me, Me, Mm, Mm}|>;
photonTestDiagrams = CreateDiagrams[photonTest["Particles"], ubModel];
photonTestSelected = ubSelect[photonTestDiagrams, "A", False];
photonReference = ubReference[photonTest["SourceName"], "A"];
photonTestComponents = Join @@ (resolvedHelicityDiagrams /@ photonTestSelected);
photonSingleSum = ReduceSpinContractions[Total[diagramAmplitude /@ photonTestComponents]];
photonPairedSum = ReduceSpinContractions[Total[diagramAmplitudes[photonTestSelected][[All, 2]]]];
check["resolved photon components retain the internal x-factor obstruction",
  !FreeQ[photonSingleSum, _xFactor | _xTildeFactor] &&
    ubCompare[photonSingleSum, photonReference, photonTest["Masses"]]["Equivalence"] === "Unresolved"];
check["paired photon matches the manual reference using ordinary algebra",
  FreeQ[photonPairedSum, _xFactor | _xTildeFactor] &&
    Expand[ExtractMandelstahm[photonPairedSum] - photonReference] === 0];

qedSignModel = ReadModel[FileNameJoin[{root, "models"}], 2];
(* Independent Wick check: follow graph paths, not the helper's port graph.
   For e/E lines the sign is the determinant parity of positron partners. *)
qedWickSign[diag_] := Module[{electrons, positrons, partnerOrder, path},
  electrons = Flatten[Position[diag["ExternalParticles"], "e", {1}]];
  positrons = Flatten[Position[diag["ExternalParticles"], "E", {1}]];
  partnerOrder = Table[SelectFirst[positrons, Function[positron,
    path = Partition[FindShortestPath[diag["Graph"], electron, positron], 2, 1];
    AllTrue[path, Function[ends, With[{edge = UndirectedEdge @@ Sort[ends]},
      !KeyExistsQ[diag["Propagators"], edge] ||
        diag["Propagators"][edge, "Spin2"] === 1]]]]], {electron, electrons}];
  If[!AllTrue[partnerOrder, IntegerQ], Return[$Failed]];
  Signature[Ordering[partnerOrder]]
];
Do[
  signDiagrams = CreateDiagrams[signParticles, qedSignModel];
  check["four-point identical-fermion Wick parity " <> ToString[signParticles, InputForm],
    Length[signDiagrams] === 2 && AllTrue[signDiagrams,
      ConstructiveDiagrams`diagramFermionSign[#] === qedWickSign[#] &]],
  {signParticles, DeleteDuplicates[Permutations[{"e", "E", "e", "E"}]]}
];
Do[
  signDiagrams = CreateDiagrams[signParticles, ubModel];
  check["distinct-flavor four-point normalization unchanged " <> ToString[signParticles, InputForm],
    signDiagrams =!= {} && AllTrue[signDiagrams,
      ConstructiveDiagrams`diagramFermionSign[#] === 1 &]],
  {signParticles, Permutations[{"e", "E", "M", "m"}]}
];
Do[
  signDiagrams = CreateDiagrams[signParticles, qedSignModel];
  check["photon attachments retain fermion matching parity " <> ToString[signParticles, InputForm],
    Length[signDiagrams] === 8 && AllTrue[signDiagrams,
      ConstructiveDiagrams`diagramFermionSign[#] === qedWickSign[#] &]];
  signGroups = GatherBy[signDiagrams,
    Sort[Sort /@ ConstructiveDiagrams`diagramFermionLines[#]] &];
  check["all four attachments in each pairing have the same exchange parity " <>
      ToString[signParticles, InputForm],
    Length[signGroups] === 2 && Length /@ signGroups === {4, 4} &&
      AllTrue[signGroups, Length[DeleteDuplicates[
        ConstructiveDiagrams`diagramFermionSign /@ #]] === 1 &]];
  check["fermion parity does not depend on vertex association order " <>
      ToString[signParticles, InputForm],
    AllTrue[signDiagrams, ConstructiveDiagrams`diagramFermionSign[#] ===
      ConstructiveDiagrams`diagramFermionSign[Join[#, <|"VertexStructures" ->
        Association[Reverse[Normal[#["VertexStructures"]]]]|>]] &]];
  check["fermion parity multiplies a whole diagram exactly once " <>
      ToString[signParticles, InputForm],
    AllTrue[signDiagrams, Function[diag,
      diagramAmplitude[diag] === ConstructiveDiagrams`diagramFermionSign[diag] *
        Block[{ConstructiveDiagrams`diagramFermionSign},
          ConstructiveDiagrams`diagramFermionSign[_] := 1;
          diagramAmplitude[diag]]]]],
  {signParticles, {{"e", "E", "E", "e", "A.+"},
    {"e", "E", "E", "e", "A.-"}, {"e", "E", "e", "E", "A.+"},
    {"e", "E", "e", "E", "A.-"}, {"A.+", "E", "e", "e", "E"},
    {"e", "A.-", "E", "E", "e"}}}
];
sixFermionDiagrams = CreateDiagrams[{"e", "E", "e", "E", "e", "E"}, qedSignModel];
check["six-fermion trees use all six matching determinant signs",
  Length[sixFermionDiagrams] === 36 &&
    Length[DeleteDuplicates[Sort[Sort /@ ConstructiveDiagrams`diagramFermionLines[#]] & /@
      sixFermionDiagrams]] === 6 && AllTrue[sixFermionDiagrams,
      ConstructiveDiagrams`diagramFermionSign[#] === qedWickSign[#] &]];
Do[
  singleLineDiagrams = CreateDiagrams[Join[{"e", "E"}, photonHelicities], qedSignModel];
  check["one fermion line acquires no exchange sign " <> ToString[photonHelicities, InputForm],
    Length[singleLineDiagrams] === 6 && AllTrue[singleLineDiagrams,
      ConstructiveDiagrams`diagramFermionSign[#] === 1 &]],
  {photonHelicities, Tuples[{"A.+", "A.-"}, 3]}
];

obsoleteDiagramTemporaryPatterns = {"ConstructiveDiagrams`state", "ConstructiveDiagrams`state$*",
  "ConstructiveDiagrams`leg", "ConstructiveDiagrams`leg$*",
  "ConstructiveDiagrams`choices", "ConstructiveDiagrams`choices$*",
  "ConstructiveDiagrams`prop", "ConstructiveDiagrams`prop$*"};
Block[{Global`state = "notebook state", Global`leg = "notebook leg",
    Global`leg$ = "notebook scoped leg", Global`choices = "notebook choices",
    Global`prop = "notebook propagator"},
  Do[
    (* The second reload simulates an older package's leaked symbols. Only
       this deliberate collision fixture suppresses messages, not the reload. *)
    If[reloadIndex === 2,
      Quiet[Symbol /@ {"ConstructiveDiagrams`state", "ConstructiveDiagrams`state$123",
        "ConstructiveDiagrams`leg", "ConstructiveDiagrams`leg$",
        "ConstructiveDiagrams`choices", "ConstructiveDiagrams`choices$123",
        "ConstructiveDiagrams`prop", "ConstructiveDiagrams`prop$123"}]];
    Block[{$MessageList = {}},
      Get[FileNameJoin[{root, "source", "HEPCAT.wl"}]];
      check["package reload has no notebook-name collisions " <> ToString[reloadIndex],
        $MessageList === {}]];
    check["package reload preserves notebook variables " <> ToString[reloadIndex],
      {Global`state, Global`leg, Global`leg$, Global`choices, Global`prop} ===
        {"notebook state", "notebook leg", "notebook scoped leg", "notebook choices", "notebook propagator"}];
    check["package reload removes obsolete public diagram temporaries " <> ToString[reloadIndex],
      Flatten[Names /@ obsoleteDiagramTemporaryPatterns] === {}];
    reloadDiagrams = CreateDiagrams[{"e", "E", "M", "m"}, ubModel];
    check["diagram helpers still work while notebook variables have values " <> ToString[reloadIndex],
      Length[reloadDiagrams] === 3 && Length[diagramAmplitudes[reloadDiagrams]] === 3 &&
        FreeQ[diagramAmplitudes[reloadDiagrams], _Missing | _xFactor | _xTildeFactor]],
    {reloadIndex, 2}]
];
Print[<|"Checks" -> checks, "Failed" -> Length[failures], "Failures" -> failures|>];
Print["HEPCAT_TEST_COMPLETED:diagram-tests.wl"];
Quit[If[failures === {}, 0, 1]];
