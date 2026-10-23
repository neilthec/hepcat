(* Legacy algebra contracts, with explicit formulas and independent matrix checks. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
checks = 0; failures = {};
SetAttributes[legacyCheck, HoldRest];
legacyCheck[name_, assertion_] := Module[{answer},
  checks++;
  answer = Check[TimeConstrained[Block[{$IterationLimit = 10000, $RecursionLimit = 512},
    Check[assertion, $Failed]], 8, $TimedOut], $Failed];
  If[!TrueQ[answer], AppendTo[failures, name]; Print["FAILED: ", name, " -> ", InputForm[answer]]]
];
legacySame[a_, b_] := Expand[a - b] === 0;
legacySpin[h_, chirality_, i_] := h["Spin", chirality, i];
legacyHel[h_, chirality_, i_] := h["Helicity", chirality, i];
legacyIndexed[h_, level_, chirality_, i_] := h["Spin", level, chirality, i, legacyJ];
legacyBracket[h_, type_, i_, j_] := SpinorChain[legacySpin[h, type, i], legacySpin[h, type, j]];

(* ::Section:: *)
(* Massive completeness: index position, chirality and contracted endpoint. *)
Do[
  legacyLeft = legacySpin[legacyHead, legacyFirstType, 1];
  legacyRight = legacySpin[legacyHead, legacySecondType, 4];
  legacyUpper = legacyIndexed[legacyHead, "Upper", legacyFirstType, 2];
  legacyLower = legacyIndexed[legacyHead, "Lower", legacySecondType, 2];
  legacySign = If[legacyFirstType === "Angle", 1, -1];
  legacyMiddle = If[legacyFirstType === legacySecondType,
    Mass[2] SpinorChain[legacyLeft, legacyRight],
    SpinorChain[legacyLeft, If[legacyHead === Spinor, Mom[2], MomHat[2]], legacyRight]];
  legacyCheck[{"massive right-left", legacyHead, legacyFirstType, legacySecondType},
    legacySame[ReduceSpinContractions[SpinorChain[legacyLeft, legacyUpper] SpinorChain[legacyLower, legacyRight]],
      legacySign legacyMiddle]];
  legacyCheck[{"massive reversed variance", legacyHead, legacyFirstType, legacySecondType},
    legacySame[ReduceSpinContractions[SpinorChain[legacyLeft, legacyIndexed[legacyHead, "Lower", legacyFirstType, 2]]
      SpinorChain[legacyIndexed[legacyHead, "Upper", legacySecondType, 2], legacyRight]],
      -legacySign legacyMiddle]];
  legacyCheck[{"massive left-left", legacyHead, legacyFirstType, legacySecondType},
    legacySame[ReduceSpinContractions[SpinorChain[legacyUpper, legacyLeft] SpinorChain[legacyLower, legacyRight]],
      -legacySign legacyMiddle]];
  legacyCheck[{"massive right-right", legacyHead, legacyFirstType, legacySecondType},
    legacySame[ReduceSpinContractions[SpinorChain[legacyLeft, legacyUpper] SpinorChain[legacyRight, legacyLower]],
      -legacySign legacyMiddle]],
  {legacyHead, {Spinor, SpinorHat}}, {legacyFirstType, {"Angle", "Square"}}, {legacySecondType, {"Angle", "Square"}}
];
legacyCheck["distinct spin indices are not contracted",
  ReduceSpinContractions[SpinorChain[legacySpin[Spinor, "Angle", 1], Spinor["Spin", "Upper", "Angle", 2, legacyJ]]
    SpinorChain[Spinor["Spin", "Lower", "Angle", 2, legacyK], legacySpin[Spinor, "Angle", 4]]] ===
  SpinorChain[legacySpin[Spinor, "Angle", 1], Spinor["Spin", "Upper", "Angle", 2, legacyJ]]
    SpinorChain[Spinor["Spin", "Lower", "Angle", 2, legacyK], legacySpin[Spinor, "Angle", 4]]];
legacyCheck["contraction distributes over a sum and preserves a pole",
  legacySame[ReduceSpinContractions[(legacyA + legacyB) SpinorChain[legacySpin[Spinor, "Angle", 1],
    Spinor["Spin", "Upper", "Angle", 2, legacyJ]] SpinorChain[Spinor["Spin", "Lower", "Square", 2, legacyJ],
    legacySpin[Spinor, "Square", 4]]/PropDen[Mom[Multiparticle[1, 2]], legacyMass]],
    (legacyA + legacyB) SpinorChain[legacySpin[Spinor, "Angle", 1], Mom[2], legacySpin[Spinor, "Square", 4]]/
      PropDen[Mom[Multiparticle[1, 2]], legacyMass]]];

(* ::Section:: *)
(* Massless completeness, powers and epsilon raising/lowering. *)
Do[
  legacyOpposite = If[legacyType === "Angle", "Square", "Angle"];
  legacyOuterLeft = legacySpin[Spinor, legacyType, 1];
  legacyOuterRight = legacySpin[Spinor, legacyOpposite, 4];
  legacyL = SpinorChain[legacyOuterLeft, legacyHel[Spinor, legacyType, 3]];
  legacyR = SpinorChain[legacyHel[Spinor, legacyOpposite, 3], legacyOuterRight];
  legacySewn = SpinorChain[legacyOuterLeft, Mom[3], legacyOuterRight];
  legacyCheck[{"massless completeness", legacyType}, ReduceSpinContractions[legacyL legacyR] === legacySewn];
  legacyCheck[{"massless equal cubic powers", legacyType},
    ReduceSpinContractions[legacyL^3 legacyR^3] === legacySewn^3];
  legacyCheck[{"massless right powered spectator", legacyType},
    ReduceSpinContractions[7 legacyL legacyR^3] === 7 legacySewn legacyR^2];
  legacyCheck[{"massless left powered spectator", legacyType},
    ReduceSpinContractions[5 legacyL^3 legacyR] === 5 legacyL^2 legacySewn];
  legacyCheck[{"dummy-index helicity completeness", legacyType},
    ReduceSpinContractions[SpinorChain[legacyOuterLeft, Spinor["Helicity", legacyType, 3, legacyJ]]
      SpinorChain[Spinor["Helicity", legacyOpposite, 3, legacyJ], legacyOuterRight]] === legacySewn];
  legacyCheck[{"massless left-left", legacyType},
    ReduceSpinContractions[SpinorChain[legacyHel[Spinor, legacyType, 3], legacyOuterLeft]
      SpinorChain[legacyHel[Spinor, legacyOpposite, 3], legacyOuterRight]] ===
      If[legacyType === "Angle",
        -SpinorChain[legacyOuterLeft, Mom[3], legacyOuterRight],
        -SpinorChain[legacyOuterRight, Mom[3], legacyOuterLeft]]];
  legacyCheck[{"massless right-right", legacyType},
    ReduceSpinContractions[SpinorChain[legacyOuterLeft, legacyHel[Spinor, legacyType, 3]]
      SpinorChain[legacyOuterRight, legacyHel[Spinor, legacyOpposite, 3]]] ===
      If[legacyType === "Angle",
        -SpinorChain[legacyOuterRight, Mom[3], legacyOuterLeft],
        -SpinorChain[legacyOuterLeft, Mom[3], legacyOuterRight]]],
  {legacyType, {"Angle", "Square"}}
];
Do[
  legacyM = If[legacyHead === Spinor, Mom, MomHat];
  legacyL = SpinorChain[legacyHel[legacyHead, "Square", 3], legacySpin[legacyHead, "Square", 1]];
  legacyTail = Join[legacyM /@ Take[{5, 6, 7}, legacyMomentumCount],
    {legacySpin[legacyHead, If[EvenQ[legacyMomentumCount], "Angle", "Square"], 4]}];
  legacyR = SpinorChain @@ Prepend[legacyTail, legacyHel[legacyHead, "Angle", 3]];
  legacySewn = (-1)^Length[legacyTail] SpinorChain @@ Join[Reverse[legacyTail],
    {legacyM[3], legacySpin[legacyHead, "Square", 1]}];
  legacyCheck[{"powered left-left reversal sign", legacyHead, legacyPower, legacyMomentumCount},
    legacySame[ReduceSpinContractions[legacyL^legacyPower legacyR], legacyL^(legacyPower - 1) legacySewn]];
  legacyCheck[{"powered left-left preserves scalar and propagator", legacyHead, legacyPower, legacyMomentumCount},
    legacySame[ReduceSpinContractions[7 legacyL^legacyPower legacyR/PropDen[Mom[Multiparticle[1, 2]], legacyMass]],
      7 legacyL^(legacyPower - 1) legacySewn/PropDen[Mom[Multiparticle[1, 2]], legacyMass]]],
  {legacyHead, {Spinor, SpinorHat}}, {legacyPower, {2, 3}}, {legacyMomentumCount, {0, 1, 2, 3}}
];
Do[
  legacyNext = If[legacyLevel === "Lower", "Upper", "Lower"];
  legacyEpsilonLevel = If[legacyLevel === "Lower", "Upper", "Lower"];
  legacyCheck[{"epsilon", legacyLevel, legacyType, legacyOrder},
    ReduceSpinContractions[SpinorChain[legacySpin[Spinor, legacyType, 1],
      Spinor["Spin", legacyLevel, legacyType, 2, legacyJ]]
      If[legacyOrder === 1, SpinorEps[legacyEpsilonLevel, legacyJ, legacyK], SpinorEps[legacyEpsilonLevel, legacyK, legacyJ]]] ===
      If[legacyOrder === 1, 1, -1] SpinorChain[legacySpin[Spinor, legacyType, 1],
        Spinor["Spin", legacyNext, legacyType, 2, legacyK]]],
  {legacyLevel, {"Lower", "Upper"}}, {legacyType, {"Angle", "Square"}}, {legacyOrder, {1, 2}}
];
Do[
  legacyCheck[{"closed massive angle trace", legacyLevel},
    ReduceSpinContractions[SpinorChain[Spinor["Spin", legacyLevel, "Angle", 2, legacyJ], Mom[3], Mom[4],
      Spinor["Spin", If[legacyLevel === "Upper", "Lower", "Upper"], "Angle", 2, legacyJ]]] ===
      If[legacyLevel === "Upper", -1, 1] Mass[2] SpinorTrace[Mom[3], Mom[4]]];
  legacyCheck[{"closed massive mixed trace", legacyLevel},
    ReduceSpinContractions[SpinorChain[Spinor["Spin", legacyLevel, "Square", 2, legacyJ], Mom[3],
      Spinor["Spin", If[legacyLevel === "Upper", "Lower", "Upper"], "Angle", 2, legacyJ]]] ===
      If[legacyLevel === "Upper", -1, 1] SpinorTrace[Mom[2], Mom[3]]],
  {legacyLevel, {"Upper", "Lower"}}
];
legacyCheck["closed helicity trace", ReduceSpinContractions[SpinorChain[legacyHel[Spinor, "Square", 2],
  Mom[3], legacyHel[Spinor, "Angle", 2]]] === SpinorTrace[Mom[2], Mom[3]]];

(* ::Section:: *)
(* Symmetrization must commute with identifying initially distinct spin labels. *)
legacySymmetrizationFixtures = {
  {"spin 1 one chain", 2, SpinorChain[Spinor["Spin", "Angle", 3], Mom[7], Spinor["Spin", "Square", 3]]},
  {"spin 1 two chains", 2, SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 1]]
    SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]},
  {"spin 1 squared chain", 2, SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 1]]^2},
  {"spin 3/2 two chains", 3, SpinorChain[Spinor["Spin", "Angle", 3], Mom[7], Spinor["Spin", "Square", 3]]
    SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 1]]},
  {"spin 3/2 three chains", 3, SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 1]]
    SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]
    SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 5]]},
  {"spin 2 two chains", 4, SpinorChain[Spinor["Spin", "Angle", 3], Mom[7], Spinor["Spin", "Square", 3]]
    SpinorChain[Spinor["Spin", "Angle", 3], Mom[8], Spinor["Spin", "Square", 3]]},
  {"spin 2 three chains", 4, SpinorChain[Spinor["Spin", "Angle", 3], Mom[7], Spinor["Spin", "Square", 3]]
    SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 1]]
    SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]},
  {"spin 2 four chains", 4, SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 1]]
    SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]
    SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 5]]
    SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 6]]}
};
Do[
  legacyIndices = Take[{legacyI1, legacyI2, legacyI3, legacyI4}, legacyFixture[[2]]];
  legacySymmetrized = SymmetrizeSpin[legacyFixture[[3]], 3, legacyIndices];
  legacyCheck[{"distinct spin-label normalized average", legacyFixture[[1]]},
    (legacySymmetrized /. _SpinorChain -> 1) === 1];
  legacyCheck[{"distinct spin-label permutation symmetry", legacyFixture[[1]]},
    legacySame[legacySymmetrized, SymmetrizeSpin[legacyFixture[[3]], 3, Reverse[legacyIndices]]]];
  legacyPatterns = Join[Tuples[{legacyJ, legacyK}, legacyFixture[[2]]],
    If[legacyFixture[[2]] === 4, Permutations[{legacyJ, legacyJ, legacyK, legacyLIndex}], {}]];
  Do[
    legacyCheck[{"coinciding spin labels retain multiplicity", legacyFixture[[1]], legacyPattern},
      legacySame[SymmetrizeSpin[legacyFixture[[3]], 3, legacyPattern],
        legacySymmetrized /. Thread[legacyIndices -> legacyPattern]]],
    {legacyPattern, legacyPatterns}],
  {legacyFixture, legacySymmetrizationFixtures}
];
Do[
  legacyCheck[{"unsupported symmetrization rank leaves the expression unchanged", legacyRank},
    SymmetrizeSpin[legacySymmetrizationFixtures[[1, 3]], 3, Range[legacyRank]] ===
      legacySymmetrizationFixtures[[1, 3]]],
  {legacyRank, {0, 5, 20}}
];
legacyCheck["spin 1/2 keeps its direct index assignment",
  SymmetrizeSpin[SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 1]], 3, {legacyJ}] ===
    SpinorChain[Spinor["Spin", "Upper", "Angle", 3, legacyJ], Spinor["Spin", "Angle", 1]]];

(* ::Section:: *)
(* Ordering, multilinearity, external Dirac identities, and Clifford squares. *)
Do[
  legacyM = If[legacyHead === Spinor, Mom, MomHat];
  legacyDot = If[legacyHead === Spinor, MomProd, MomProdHat12];
  legacyOtherType = If[legacyType === "Angle", "Square", "Angle"];
  legacyCheck[{"equal bracket vanishes", legacyHead, legacyType},
    ReduceSpinorProducts[legacyBracket[legacyHead, legacyType, 2, 2]] === 0];
  legacyCheck[{"bracket antisymmetry", legacyHead, legacyType},
    ReduceSpinorProducts[legacyBracket[legacyHead, legacyType, 3, 1]] === -legacyBracket[legacyHead, legacyType, 1, 3]];
  legacyCheck[{"external mass right", legacyHead, legacyType},
    ReduceSpinorProducts[SpinorChain[legacySpin[legacyHead, legacyOtherType, 1], legacyM[4],
      legacySpin[legacyHead, legacyType, 4]]] ===
      -Mass[4] SpinorChain[legacySpin[legacyHead, legacyOtherType, 1],
        legacySpin[legacyHead, If[legacyType === "Angle", "Square", "Angle"], 4]]];
  legacyCheck[{"external mass left", legacyHead, legacyType},
    ReduceSpinorProducts[SpinorChain[legacySpin[legacyHead, legacyType, 1], legacyM[1],
      legacySpin[legacyHead, legacyOtherType, 4]]] ===
      Mass[1] SpinorChain[legacySpin[legacyHead, If[legacyType === "Angle", "Square", "Angle"], 1],
        legacySpin[legacyHead, legacyOtherType, 4]]];
  legacyCheck[{"indexed external mass right", legacyHead, legacyType},
    ReduceSpinorProducts[SpinorChain[legacySpin[legacyHead, legacyOtherType, 1], legacyM[4],
      legacyIndexed[legacyHead, "Upper", legacyType, 4]]] ===
      -Mass[4] SpinorChain[legacySpin[legacyHead, legacyOtherType, 1],
        legacyIndexed[legacyHead, "Upper", If[legacyType === "Angle", "Square", "Angle"], 4]]];
  legacyCheck[{"massless right annihilation", legacyHead, legacyType},
    ReduceSpinorProducts[SpinorChain[legacySpin[legacyHead, legacyOtherType, 1], legacyM[4],
      legacyHel[legacyHead, legacyType, 4]]] === 0];
  legacyCheck[{"massless left annihilation", legacyHead, legacyType},
    ReduceSpinorProducts[SpinorChain[legacyHel[legacyHead, legacyType, 1], legacyM[1],
      legacySpin[legacyHead, legacyOtherType, 4]]] === 0];
  legacyCheck[{"adjacent momentum square", legacyHead, legacyType},
    ReduceSpinorProducts[SpinorChain[legacySpin[legacyHead, legacyType, 1], legacyM[3], legacyM[3],
      legacySpin[legacyHead, legacyType, 4]]] === Mass[3]^2 legacyBracket[legacyHead, legacyType, 1, 4]];
  legacyCheck[{"two-momentum reversal", legacyHead, legacyType},
    ReduceSpinorProducts[SpinorChain[legacySpin[legacyHead, legacyType, 4], legacyM[2], legacyM[3],
      legacySpin[legacyHead, legacyType, 1]]] ===
      SpinorChain[legacySpin[legacyHead, legacyType, 1], legacyM[2], legacyM[3], legacySpin[legacyHead, legacyType, 4]] -
        2 legacyDot[2, 3] legacyBracket[legacyHead, legacyType, 1, 4]];
  legacyCheck[{"interior triple Clifford contraction", legacyHead, legacyType},
    legacySame[ReduceSpinorProducts[SpinorChain[legacySpin[legacyHead, legacyType, 1],
      legacyM[2], legacyM[3], legacyM[2], legacySpin[legacyHead, If[legacyType === "Angle", "Square", "Angle"], 4]]],
      If[legacyType === "Angle",
        2 legacyDot[2, 3] SpinorChain[legacySpin[legacyHead, "Square", 4], legacyM[2], legacySpin[legacyHead, "Angle", 1]] -
          Mass[2]^2 SpinorChain[legacySpin[legacyHead, "Square", 4], legacyM[3], legacySpin[legacyHead, "Angle", 1]],
        2 legacyDot[2, 3] SpinorChain[legacySpin[legacyHead, "Square", 1], legacyM[2], legacySpin[legacyHead, "Angle", 4]] -
          Mass[2]^2 SpinorChain[legacySpin[legacyHead, "Square", 1], legacyM[3], legacySpin[legacyHead, "Angle", 4]]]]],
  {legacyHead, {Spinor, SpinorHat}}, {legacyType, {"Angle", "Square"}}
];
legacyCheck["angle-square one-momentum reversal",
  ReduceSpinorProducts[SpinorChain[legacySpin[Spinor, "Angle", 1], Mom[3], legacySpin[Spinor, "Square", 4]]] ===
    SpinorChain[legacySpin[Spinor, "Square", 4], Mom[3], legacySpin[Spinor, "Angle", 1]]];
legacyCheck["bilinear composite momentum slots",
  legacySame[ReduceSpinorProducts[SpinorChain[legacySpin[Spinor, "Square", 1], Mom[Multiparticle[2, 3]],
    Mom[Multiparticle[5, 6, 7]], legacySpin[Spinor, "Square", 4]]],
    Total[Flatten[Table[SpinorChain[legacySpin[Spinor, "Square", 1], Mom[legacyI], Mom[legacyK],
      legacySpin[Spinor, "Square", 4]], {legacyI, {2, 3}}, {legacyK, {5, 6, 7}}]]]]];
legacyCheck["negative momentum slot and identity insertion",
  ReduceSpinorProducts[SpinorChain[legacySpin[Spinor, "Square", 1], -Mom[3], 1, legacySpin[Spinor, "Angle", 4]]] ===
    -SpinorChain[legacySpin[Spinor, "Square", 1], Mom[3], legacySpin[Spinor, "Angle", 4]]];
legacyCheck["no invented internal scalar shell",
  ReduceSpinorProducts[MomProd[1, 3] legacyBracket[Spinor, "Angle", 1, 4]] ===
    MomProd[1, 3] legacyBracket[Spinor, "Angle", 1, 4]];

(* ::Section:: *)
(* Basic Schouten reductions, traces, scalar products and Mandelstam conversion. *)
Do[
  legacyCheck[{"Schouten difference", legacyType},
    ReduceSpinorProducts[legacyBracket[Spinor, legacyType, 1, 4] legacyBracket[Spinor, legacyType, 2, 3] -
      legacyBracket[Spinor, legacyType, 1, 3] legacyBracket[Spinor, legacyType, 2, 4]] ===
      -legacyBracket[Spinor, legacyType, 1, 2] legacyBracket[Spinor, legacyType, 3, 4]];
  legacyCheck[{"Schouten sum", legacyType},
    ReduceSpinorProducts[legacyBracket[Spinor, legacyType, 1, 2] legacyBracket[Spinor, legacyType, 3, 4] +
      legacyBracket[Spinor, legacyType, 1, 4] legacyBracket[Spinor, legacyType, 2, 3]] ===
      legacyBracket[Spinor, legacyType, 1, 3] legacyBracket[Spinor, legacyType, 2, 4]],
  {legacyType, {"Angle", "Square"}}
];
Do[
  legacyDot = If[legacyM === Mom, MomProd, MomProdHat12];
  legacyCheck[{"empty trace", legacyM}, ReduceSpinorProducts[SpinorTrace[]] === 2];
  legacyCheck[{"trace two", legacyM}, ReduceSpinorProducts[SpinorTrace[legacyM[3], legacyM[1]]] === 2 legacyDot[1, 3]];
  legacyCheck[{"trace square", legacyM}, ReduceSpinorProducts[SpinorTrace[legacyM[2], legacyM[2]]] === 2 Mass[2]^2];
  legacyCheck[{"cyclic trace square", legacyM},
    ReduceSpinorProducts[SpinorTrace[legacyM[2], legacyM[3], legacyM[4], legacyM[2]]] === 2 Mass[2]^2 legacyDot[3, 4]];
  legacyCheck[{"trace composite three", legacyM},
    ReduceSpinorProducts[SpinorTrace[legacyM[Multiparticle[1, 2, 3]], legacyM[4]]] ===
      2 legacyDot[1, 4] + 2 legacyDot[2, 4] + 2 legacyDot[3, 4]];
  legacyCheck[{"trace negative", legacyM}, ReduceSpinorProducts[SpinorTrace[-legacyM[1], legacyM[4]]] === -2 legacyDot[1, 4]];
  legacyCheck[{"dot symmetry", legacyM}, legacyDot[4, 2] === legacyDot[2, 4]];
  legacyCheck[{"dot external square", legacyM}, legacyDot[3, 3] === Mass[3]^2];
  legacyCheck[{"dot composite four", legacyM}, legacyDot[Multiparticle[1, 2, 3, 4], 5] === Total[Table[legacyDot[legacyI, 5], {legacyI, 4}]]];
  legacyCheck[{"dot bilinear composite", legacyM},
    legacyDot[Multiparticle[1, 2], Multiparticle[3, 4]] === legacyDot[1, 3] + legacyDot[1, 4] + legacyDot[2, 3] + legacyDot[2, 4]];
  legacyCheck[{"dot two symbolic negative momenta", legacyM}, legacyDot[-legacyP, -legacyQ] === legacyDot[legacyP, legacyQ]];
  legacyCheck[{"propagator sign normalization", legacyM}, PropDen[-legacyM[1] - legacyM[3], legacyMass] === PropDen[legacyM[1] + legacyM[3], legacyMass]],
  {legacyM, {Mom, MomHat}}
];
legacyCheck["Mandelstam numerator conversion", ExtractMandelstahm[MomProd[1, 3]] ===
  (Mandelstahm[1, 3] - Mass[1]^2 - Mass[3]^2)/2];
legacyCheck["Mandelstam denominator conversion", ExtractMandelstahm[1/PropDen[Mom[Multiparticle[3, 4]], legacyMass]] ===
  1/(Mandelstahm[1, 2] - legacyMass^2)];
legacyCheck["higher-point Mandelstam does not assume four-point complements",
  ExtractMandelstahm[PropDen[Mom[Multiparticle[3, 4]], legacyMass], 5] === Mandelstahm[3, 4] - legacyMass^2];
legacyCheck["hatted Mandelstam numerator conversion", ExtractMandelstahm[MomProdHat12[2, 4], 5] ===
  (MandelstahmHat[2, 4] - Mass[2]^2 - Mass[4]^2)/2];

(* ::Section:: *)
(* Auxiliary Zeta identities remain supported, even though current diagrams do not need them. *)
Do[
  legacyCheck[{"Zeta normalization", legacyHead, legacyType},
    ReduceSpinorProducts[SpinorChain[legacyHel[legacyHead, legacyType, 2], legacyHead["Zeta", legacyType, 2]]] === Sqrt[2 En[2]]];
  legacyCheck[{"Zeta reversed normalization", legacyHead, legacyType},
    ReduceSpinorProducts[SpinorChain[legacyHead["Zeta", legacyType, 2], legacyHel[legacyHead, legacyType, 2]]] === -Sqrt[2 En[2]]];
  legacyCheck[{"Zeta pair", legacyHead, legacyType},
    Simplify[ReduceSpinorProducts[SpinorChain[legacyHead["Zeta", legacyType, 1], legacyHead["Zeta", legacyType, 2]]] +
      SpinorChain[legacyHel[legacyHead, If[legacyType === "Angle", "Square", "Angle"], 1],
        legacyHel[legacyHead, If[legacyType === "Angle", "Square", "Angle"], 2]]/Sqrt[4 En[1] En[2]],
      En[1] > 0 && En[2] > 0] === 0],
  {legacyHead, {Spinor, SpinorHat}}, {legacyType, {"Angle", "Square"}}
];
legacyCheck["Zeta momentum right", ReduceSpinorProducts[SpinorChain[legacySpin[Spinor, "Square", 1], Mom[2],
  Spinor["Zeta", "Angle", 2]]] === Sqrt[2 En[2]] SpinorChain[legacySpin[Spinor, "Square", 1], legacyHel[Spinor, "Square", 2]]];
legacyCheck["Zeta momentum left", ReduceSpinorProducts[SpinorChain[Spinor["Zeta", "Square", 2], Mom[2],
  legacySpin[Spinor, "Angle", 4]]] === -Sqrt[2 En[2]] SpinorChain[legacyHel[Spinor, "Angle", 2], legacySpin[Spinor, "Angle", 4]]];

(* ::Section:: *)
(* Existing higher-level simplifier: explicit momentum set and shell opt-in. *)
legacySimplify[expr_, opts___] := SimplifySpinorProducts[expr, PrintWarnings -> False, opts];
legacyCheck["three-point scalar momentum conservation", legacySame[
  legacySimplify[MomProd[1, 2], Momenta -> {1, 2, 3}], (Mass[3]^2 - Mass[1]^2 - Mass[2]^2)/2]];
legacyCheck["three-point other scalar pair", legacySame[
  legacySimplify[MomProd[2, 3], Momenta -> {1, 2, 3}], (Mass[1]^2 - Mass[2]^2 - Mass[3]^2)/2]];
legacyCheck["four-point complementary invariant", legacySame[
  legacySimplify[MomProd[3, 4]], MomProd[1, 2] + (Mass[1]^2 + Mass[2]^2 - Mass[3]^2 - Mass[4]^2)/2]];
legacyCheck["internal shell disabled by default", legacySimplify[MomProd[1, 2]] === MomProd[1, 2]];
legacyCheck["explicit internal shell and mass rules", legacySimplify[MomProd[1, 2], InternalLineOnShell -> True,
  massReplacementRules -> {Mass[1] -> legacyM1, Mass[2] -> legacyM2, Mass[Multiparticle[1, 2]] -> legacyInternalMass}] ===
  (legacyInternalMass^2 - legacyM1^2 - legacyM2^2)/2];
legacyCheck["mass substitutions without an internal shell", legacySimplify[Mass[1]^2 + MomProd[1, 2],
  massReplacementRules -> {Mass[1] -> legacyM1}] === legacyM1^2 + MomProd[1, 2]];
legacyCheck["simplifier contracts explicit spin indices first", legacySimplify[
  SpinorChain[legacySpin[Spinor, "Angle", 1], Spinor["Spin", "Upper", "Angle", 2, legacyJ]]
  SpinorChain[Spinor["Spin", "Lower", "Angle", 2, legacyJ], legacySpin[Spinor, "Angle", 4]]] ===
  Mass[2] legacyBracket[Spinor, "Angle", 1, 4]];

(* ::Section:: *)
(* Existing x-factor interface and helicity sum. *)
legacyCheck["ordinary x factors remain symbolic individually", ReducexFactors[xFactor[1, 2]] === xFactorReduced[1, 2]];
legacyCheck["ordinary tilde factors remain symbolic individually", ReducexFactors[xTildeFactor[1, 2]] === xTildeFactorReduced[1, 2]];
legacyCheck["x-factor reversed composite sign", legacySame[
  ReducexFactors[xFactor[Multiparticle[2, 3], 1]], -ReducexFactors[xFactor[1, Multiparticle[2, 3]]]]];
legacyCheck["tilde-factor reversed composite sign", legacySame[
  ReducexFactors[xTildeFactor[Multiparticle[2, 3], 1]], -ReducexFactors[xTildeFactor[1, Multiparticle[2, 3]]]]];
legacyCheck["x-factor channel mass preserved", !FreeQ[ReducexFactors[xFactor[1, Multiparticle[2, 3]], ChannelMass -> legacyChannelMass], legacyChannelMass]];
legacyFourFermion = -(legacyBracket[Spinor, "Angle", 1, 4] legacyBracket[Spinor, "Square", 2, 3] +
  legacyBracket[Spinor, "Angle", 1, 3] legacyBracket[Spinor, "Square", 2, 4] +
  legacyBracket[Spinor, "Angle", 2, 4] legacyBracket[Spinor, "Square", 1, 3] +
  legacyBracket[Spinor, "Angle", 2, 3] legacyBracket[Spinor, "Square", 1, 4]);
legacyCheck["four-fermion x/tilde helicity sum", ReducexFactors[
  xFactor[1, 2] xTildeFactor[3, 4] legacyBracket[Spinor, "Angle", 1, 2] legacyBracket[Spinor, "Square", 3, 4] +
  xTildeFactor[1, 2] xFactor[3, 4] legacyBracket[Spinor, "Square", 1, 2] legacyBracket[Spinor, "Angle", 3, 4]] === legacyFourFermion];

(* ::Section:: *)
(* Check the old multi-chain Schouten families against independent exact matrices.
   Independent angle/square columns define complex massless momenta and obey the
   external Weyl equations, without momentum conservation or internal cuts. *)
legacyProbe[type_, i_, momenta_List, other_, j_] := SpinorChain @@ Join[
  {Spinor["Helicity", type, i]}, Mom /@ momenta, {Spinor["Helicity", other, j]}];
legacyA[i_, j_] := legacyProbe["Angle", i, {}, "Angle", j];
legacyS[i_, j_] := legacyProbe["Square", i, {}, "Square", j];
legacyOne[i_, p_, j_] := legacyProbe["Square", i, {p}, "Angle", j];
legacyTwoA[i_, p_, q_, j_] := legacyProbe["Angle", i, {p, q}, "Angle", j];
legacyTwoS[i_, p_, q_, j_] := legacyProbe["Square", i, {p, q}, "Square", j];
legacySchoutenInputs = {
  legacyOne[1, 3, 2] legacyOne[3, 1, 4],
  legacyOne[1, 4, 2] legacyOne[3, 1, 4],
  legacyOne[1, 3, 2] legacyOne[3, 2, 4],
  legacyOne[1, 4, 2] legacyOne[3, 2, 4],
  legacyOne[1, 5, 2] legacyOne[3, 1, 4]^2,
  legacyOne[4, 1, 3]^2 legacyOne[1, 3, 2],
  legacyOne[4, 1, 3]^2 legacyOne[2, 5, 1],
  legacyA[1, 2] legacyTwoA[3, 1, 2, 3],
  legacyA[2, 4] legacyTwoA[3, 1, 2, 3],
  legacyS[1, 2] legacyOne[3, 1, 4] legacyOne[5, 2, 6],
  legacyA[1, 2] legacyOne[3, 1, 4] legacyOne[5, 2, 6],
  legacyOne[1, 3, 2] legacyTwoA[3, 5, 6, 4],
  legacyOne[1, 4, 2] legacyTwoA[3, 5, 6, 4],
  legacyTwoS[1, 5, 6, 2] legacyOne[3, 2, 4],
  legacyTwoS[1, 5, 6, 2] legacyOne[3, 1, 4],
  legacyOne[4, 1, 2] legacyTwoA[3, 1, 2, 3],
  legacyOne[4, 3, 2] legacyTwoS[3, 1, 2, 3],
  legacyOne[4, 2, 1] legacyTwoA[3, 1, 2, 3],
  legacyOne[1, 2, 3] legacyTwoS[4, 1, 2, 4],
  legacyOne[2, 1, 3] legacyTwoS[4, 1, 2, 4],
  legacyOne[2, 1, 3] legacyTwoA[4, 1, 2, 4],
  legacyTwoS[3, 2, 4, 5] legacyTwoA[2, 3, 5, 4],
  legacyTwoS[3, 1, 2, 3] legacyTwoA[2, 4, 3, 2],
  legacyTwoS[3, 1, 2, 3] legacyTwoA[4, 1, 2, 4],
  legacyS[1, 2] legacyTwoA[3, 1, 2, 3] legacyTwoA[4, 1, 2, 4]
};
legacyEps = {{0, 1}, {-1, 0}};
legacyAdj[p_] := {{p[[2, 2]], -p[[1, 2]]}, {-p[[2, 1]], p[[1, 1]]}};
legacyVector[type_, i_] := If[type === "Angle", legacyAngleVectors[i], legacySquareVectors[i]];
legacyMatrixChain[c_SpinorChain] := Module[{xs = List @@ c, angle, row, ps},
  angle = xs[[1, 2]] === "Angle";
  row = legacyVector[xs[[1, 2]], xs[[1, 3]]].legacyEps If[angle, 1, -1];
  ps = Drop[Rest[xs], -1];
  Do[row = row.If[angle, legacyMatrices[p[[1]]], legacyAdj[legacyMatrices[p[[1]]]]]; angle = !angle, {p, ps}];
  row.legacyVector[xs[[-1, 2]], xs[[-1, 3]]]
];
legacyMatrixValue[expr_] := Expand[expr /. c_SpinorChain :> legacyMatrixChain[c] /.
  {Mass[i_]^2 :> Det[legacyMatrices[i]],
   MomProd[i_, j_] :> Tr[legacyMatrices[i].legacyAdj[legacyMatrices[j]]]/2}];
Do[
  BlockRandom[
    SeedRandom[legacySeed];
    legacyAngleVectors = AssociationThread[Range[6], Table[RandomInteger[{-5, 5}, 2], {6}]];
    legacySquareVectors = AssociationThread[Range[6], Table[RandomInteger[{-5, 5}, 2], {6}]];
    legacyMatrices = AssociationThread[Range[6], Table[-Outer[Times,
      legacyAngleVectors[legacyK], legacySquareVectors[legacyK].legacyEps], {legacyK, 6}]];
  ];
  Do[
    legacyCheck[{"multi-chain exact matrix identity", legacySeed, legacyI},
      legacyMatrixValue[ReduceSpinorProducts[legacySchoutenInputs[[legacyI]]]] ===
        legacyMatrixValue[legacySchoutenInputs[[legacyI]]]],
    {legacyI, Length[legacySchoutenInputs]}],
  {legacySeed, {1701, 2917}}
];

(* ::Section:: *)
(* Scalar content must not hide an existing Schouten reduction. No rational
   denominator is combined to manufacture a polynomial identity. *)
Do[
  legacySchoutenScaled = Expand[legacyCoefficient (
    legacyBracket[Spinor, legacyType, 1, 4] legacyBracket[Spinor, legacyType, 2, 3] -
    legacyBracket[Spinor, legacyType, 1, 3] legacyBracket[Spinor, legacyType, 2, 4])];
  legacySchoutenScaledTarget = -legacyCoefficient legacyBracket[Spinor, legacyType, 1, 2]
    legacyBracket[Spinor, legacyType, 3, 4];
  legacyCheck[{"shared scalar content exposes Schouten", legacyType, legacyCoefficient},
    ReduceSpinorProducts[legacySchoutenScaled] === legacySchoutenScaledTarget];
  legacyCheck[{"scalar-content Schouten agrees with exact matrices", legacyType, legacyCoefficient},
    legacyMatrixValue[ReduceSpinorProducts[legacySchoutenScaled]] ===
      legacyMatrixValue[legacySchoutenScaled]];
  legacyCheck[{"scalar-content Schouten is idempotent", legacyType, legacyCoefficient},
    With[{once = ReduceSpinorProducts[legacySchoutenScaled]}, ReduceSpinorProducts[once] === once]],
  {legacyType, {"Angle", "Square"}}, {legacyCoefficient, {3, legacyCommonScalar}}
];
legacyDifferentPoles = 3 legacyA[1, 4] legacyA[2, 3]/PropDen[Mom[Multiparticle[1, 3]], legacyMass] -
  3 legacyA[1, 3] legacyA[2, 4]/PropDen[Mom[Multiparticle[1, 4]], legacyMass];
legacyCheck["scalar-content pass does not combine different propagator denominators",
  ReduceSpinorProducts[legacyDifferentPoles] === legacyDifferentPoles];
legacyNonpolynomial = 3 legacyA[1, 4] legacyA[2, 3]/(1 + legacyA[1, 2]) -
  3 legacyA[1, 3] legacyA[2, 4]/(1 + legacyA[3, 4]);
legacyCheck["scalar-content pass leaves nonpolynomial spinor dependence uncombined",
  ReduceSpinorProducts[legacyNonpolynomial] === legacyNonpolynomial];

(* ::Section:: *)
(* General chain and trace reductions against independent, unconserved Weyl matrices.
   Generic interior cases use momentum labels disjoint from the endpoints. The
   dedicated indexed endpoint cases use explicit massive Dirac-compatible spinors. *)
Module[{matrix, columns, epsilon, adjugate, momentum, dot, endpoint, chainValue,
  traceValue, value, chain, indexedChain, examples, reduced, seed, i},
  epsilon = {{0, 1}, {-1, 0}};
  adjugate[p_] := {{p[[2, 2]], -p[[1, 2]]}, {-p[[2, 1]], p[[1, 1]]}};
  momentum[Mom[leg_Integer]] := matrix[leg];
  momentum[Mom[Multiparticle[legs__Integer]]] := Total[matrix /@ {legs}];
  momentum[p_Plus] := Total[momentum /@ List @@ p];
  momentum[p_Times] := Module[{factors = List @@ p, slot},
    slot = SelectFirst[factors, !FreeQ[#, _Mom] &, Missing["Momentum"]];
    If[MissingQ[slot], p IdentityMatrix[2], Times @@ DeleteCases[factors, slot] momentum[slot]]];
  momentum[0] := ConstantArray[0, {2, 2}];
  momentum[1] := IdentityMatrix[2];
  dot[a_, b_] := Tr[momentum[Mom[a]].adjugate[momentum[Mom[b]]]]/2;
  endpoint[s_] := If[Length[s] === 5, {s[[3]], s[[4]]}, {s[[2]], s[[3]]}];
  chainValue[c_SpinorChain] := Module[{parts = List @@ c, left, right, angle, row},
    left = endpoint[First[parts]]; right = endpoint[Last[parts]];
    angle = First[left] === "Angle";
    row = columns[left].epsilon If[angle, 1, -1];
    Do[row = row.If[angle, momentum[slot], adjugate[momentum[slot]]]; angle = !angle,
      {slot, Drop[Rest[parts], -1]}];
    row.columns[right]];
  traceValue[t_SpinorTrace] := If[Length[t] === 0, 2,
    Tr[Dot @@ MapIndexed[If[OddQ[First[#2]], momentum[#1], adjugate[momentum[#1]]] &,
      List @@ t]]];
  value[expression_] := Expand[expression /. {
      c_SpinorChain :> chainValue[c], t_SpinorTrace :> traceValue[t]} /. {
      Power[Mass[leg_Integer], exponent_Integer?EvenQ] :> Det[matrix[leg]]^(exponent/2),
      MomProd[a_, b_] :> dot[a, b],
      Mass[7] -> 1, Mass[8] -> 1}];
  chain[type_, left_, slots_List, other_, right_] := SpinorChain @@ Join[
    {Spinor["Xi", type, left]}, slots, {Spinor["Xi", other, right]}];
  indexedChain[type_, left_, slots_List, other_, right_] := SpinorChain @@ Join[
    {Spinor["Spin", "Upper", type, left, legacyJ]}, slots,
    {Spinor["Spin", "Lower", other, right, legacyK]}];
  examples = {
    chain["Angle", 8, Mom /@ {5, 3, 6, 2}, "Angle", 7],
    chain["Square", 8, Mom /@ {5, 3, 6, 2}, "Square", 7],
    chain["Angle", 8, Mom /@ {5, 3, 6, 2, 4}, "Square", 7],
    chain["Square", 8, Mom /@ {5, 3, 6, 2, 4}, "Angle", 7],
    chain["Angle", 8, Mom /@ {6, 4, 3, 5, 2, 1}, "Angle", 7],
    chain["Square", 8, Mom /@ {6, 4, 3, 5, 2, 1}, "Square", 7],
    indexedChain["Angle", 8, Mom /@ {5, 3, 6, 2}, "Angle", 7],
    indexedChain["Square", 8, Mom /@ {5, 3, 6, 2, 4}, "Angle", 7],
    7 chain["Square", 7, {2 Mom[1], -3 Mom[2], Mom[3], Mom[4]}, "Square", 8]^2/5,
    chain["Angle", 7, {Mom[1] + 2 Mom[2], Mom[3] - Mom[4]}, "Angle", 8],
    indexedChain["Square", 7, {0, Mom[3]}, "Square", 8],
    chain["Square", 7, {Mom[Multiparticle[1, 2, 3, 4]], Mom[5]}, "Square", 8],
    indexedChain["Angle", 7, {Mom[Multiparticle[2, 3]], Mom[Multiparticle[2, 3]]}, "Angle", 8],
    SpinorTrace[Mom[1], Mom[2], Mom[3], Mom[1]],
    SpinorTrace[Mom[1], Mom[2], Mom[3], Mom[4], Mom[5], Mom[1]],
    SpinorTrace[Mom[1], Mom[5], Mom[4], Mom[3], Mom[2], Mom[1]],
    3 SpinorTrace[2 Mom[1], Mom[2] - Mom[3], Mom[4], Mom[5]]^2,
    SpinorTrace[Mom[Multiparticle[1, 2, 3, 4]], Mom[2], Mom[5], Mom[6]],
    SpinorTrace[Mom[1], 0, Mom[2], Mom[3]],
    chain["Angle", 7, Mom /@ {1, 2, 3, 4, 2, 5}, "Angle", 8],
    chain["Square", 7, Mom /@ {1, 2, 3, 4, 2, 5}, "Square", 8],
    indexedChain["Angle", 7, Mom /@ {2, 7, 3, 4}, "Angle", 8],
    indexedChain["Square", 7, Mom /@ {2, 7, 3, 4}, "Square", 8],
    indexedChain["Angle", 7, Mom /@ {1, 2, 8, 4}, "Angle", 8],
    indexedChain["Square", 7, Mom /@ {1, 2, 8, 4}, "Square", 8],
    chain["Angle", 7, Mom /@ {1, 2, 3, 1, 2, 3}, "Angle", 8],
    chain["Square", 7, Mom /@ {1, 2, 3, 1, 2, 3}, "Square", 8],
    chain["Angle", 7, Mom /@ {1, 2, 1, 3, 2, 3}, "Angle", 8],
    chain["Square", 7, Mom /@ {3, 1, 2, 3, 2, 1}, "Square", 8],
    indexedChain["Square", 7, Mom /@ {1, 2, 3, 2, 1, 3}, "Square", 8],
    chain["Angle", 7, Mom /@ {1, 2, 3, 1, 2, 3, 4}, "Square", 8],
    chain["Square", 7, Mom /@ {1, 2, 3, 1, 2, 3}, "Square", 8]^2,
    chain["Square", 7, Mom /@ {1, 2, 3, 1, 2, 3}, "Square", 8]/
      PropDen[Mom[Multiparticle[2, 3]], legacyMass]
  };
  Do[
    BlockRandom[SeedRandom[seed];
      matrix = AssociationThread[Range[6], Table[
        RandomInteger[{-4, 4}, {2, 2}] + I RandomInteger[{-3, 3}, {2, 2}], {6}]];
      columns = Association[Flatten[Table[{type, leg} -> RandomInteger[{-4, 4}, 2],
        {type, {"Angle", "Square"}}, {leg, {7, 8}}], 1]];
      AssociateTo[matrix, {7 -> {{2, 1}, {1, 1}}, 8 -> {{3, 2}, {1, 1}}}];
      Do[AssociateTo[columns, {"Square", leg} ->
        -adjugate[matrix[leg]].columns[{"Angle", leg}]], {leg, {7, 8}}]];
    Do[
      legacyCheck[{"general chain and trace exact matrix identity", seed, i},
        value[ReduceSpinorProducts[examples[[i]]]] === value[examples[[i]]]],
      {i, Length[examples]}],
    {seed, {4507, 7919}}];
  Do[
    legacyCheck[{"general reduction is bounded and idempotent", i},
      reduced = ReduceSpinorProducts[examples[[i]]];
      FreeQ[reduced, _SpinorChainReduced | _SpinorTraceReduced] &&
        ReduceSpinorProducts[reduced] === reduced],
    {i, Length[examples]}];
  legacyCheck["general reductions extract scalar and zero momentum slots",
    FreeQ[ReduceSpinorProducts[Total[examples[[{9, 10, 11, 17, 19}]]]],
      SpinorChain[___, _Plus | _Times | 0, ___] | SpinorTrace[___, _Plus | _Times | 0, ___]]];
  legacyCheck["bare composite-square reduction does not invent an internal mass",
    FreeQ[ReduceSpinorProducts[examples[[13]]], Mass[_Multiparticle]]];
  legacyCheck["long-chain reductions retain explicit endpoint spin indices",
    Sort[DeleteDuplicates[Cases[ReduceSpinorProducts[examples[[7]]],
      Spinor["Spin", variance_, type_, leg_, index_] :> {variance, leg, index}, Infinity]]] ===
        Sort[{{"Upper", 8, legacyJ}, {"Lower", 7, legacyK}}]]
];
legacyCheck["bare reduction does not infer momentum conservation from displayed labels",
  ReduceSpinorProducts[SpinorChain[Spinor["Xi", "Square", 7],
    Mom[1] + Mom[2] + Mom[3] + Mom[4], Spinor["Xi", "Angle", 8]]] =!= 0];
legacyCheck["bare reduction does not infer a diagram shell from its propagator",
  ReduceSpinorProducts[MomProd[2, 5]/PropDen[Mom[Multiparticle[2, 5]], Mass[2]]] ===
    MomProd[2, 5]/PropDen[Mom[Multiparticle[2, 5]], Mass[2]]];

legacyCheck["interlaced repeated momenta contract below a low recursion bound",
  Block[{$RecursionLimit = 160},
    FreeQ[ReduceSpinorProducts[SpinorChain[Spinor["Xi", "Square", 7],
      Mom[1], Mom[2], Mom[3], Mom[1], Mom[2], Mom[3], Spinor["Xi", "Square", 8]]],
      _SpinorChainReduced | _SpinorTraceReduced]]];

(* ::Section:: *)
(* Termination and idempotence on representative mixed legacy inputs. *)
legacyRepresentativeInputs = {
  3 legacyBracket[Spinor, "Angle", 4, 1]^2 + SpinorTrace[Mom[3], Mom[3]],
  SpinorChain[legacySpin[Spinor, "Square", 1], Mom[Multiparticle[2, 3]], Mom[3], legacySpin[Spinor, "Square", 4]],
  SpinorChain[legacySpin[Spinor, "Square", 1], Mom[2], Mom[3], Mom[2], legacySpin[Spinor, "Angle", 4]],
  SpinorTrace[Mom[4], Mom[3], Mom[2], Mom[1]],
  legacyBracket[Spinor, "Angle", 1, 4] legacyBracket[Spinor, "Angle", 2, 3] -
    legacyBracket[Spinor, "Angle", 1, 3] legacyBracket[Spinor, "Angle", 2, 4],
  SpinorChain[Spinor["Zeta", "Angle", 2], Mom[2], legacySpin[Spinor, "Square", 4]]
};
Do[
  legacyCheck[{"reducer terminates and is idempotent", legacyI},
    Module[{once = ReduceSpinorProducts[legacyRepresentativeInputs[[legacyI]]]},
      FreeQ[once, _SpinorChainReduced | _SpinorTraceReduced] && ReduceSpinorProducts[once] === once]],
  {legacyI, Length[legacyRepresentativeInputs]}
];
legacyCheck["higher-level simplifier idempotence", Module[{once = legacySimplify[MomProd[3, 4] + 2 MomProd[2, 3]]},
  legacySimplify[once] === once]];
legacyCheck["contraction idempotence", Module[{once = ReduceSpinContractions[
  SpinorChain[legacySpin[Spinor, "Square", 1], Spinor["Spin", "Upper", "Square", 2, legacyJ]]
  SpinorChain[Spinor["Spin", "Lower", "Angle", 2, legacyJ], legacySpin[Spinor, "Angle", 4]]]},
  ReduceSpinContractions[once] === once]];

If[checks =!= 505, AppendTo[failures, {"suite did not reach every check", "Expected" -> 505, "Actual" -> checks}]];
Print[<|"Checks" -> checks, "Failed" -> Length[failures], "Failures" -> failures|>];
Print["HEPCAT_TEST_COMPLETED:legacy-algebra-tests.wl"];
Quit[If[failures === {}, 0, 1]];
