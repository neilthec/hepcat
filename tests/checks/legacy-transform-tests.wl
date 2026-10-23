(* Regression coverage for the original crossing, indices, kinematics and exporters.
   Expectations use explicit spinor formulas, matrices and strings, not the
   newer identity-candidate or physical-checker implementations. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, assertion_] := Module[{result},
  checks++;
  result = TimeConstrained[Check[assertion, False], 10, False];
  If[!TrueQ[result], AppendTo[failures, name]; Print["FAIL: ", name]]];
ltSame[a_, b_] := Expand[a - b] === 0;
ltH[t_, p_] := Spinor["Helicity", t, p];
ltS[t_, p_] := Spinor["Spin", t, p];
ltU[t_, p_, i_] := Spinor["Spin", "Upper", t, p, i];
ltL[t_, p_, i_] := Spinor["Spin", "Lower", t, p, i];
ltB[t_, i_, j_] := SpinorChain[ltH[t, i], ltH[t, j]];

(* Crossing is the package's all-ingoing convention: angle spinors change
   sign, square spinors do not, and an explicit crossed momentum changes sign. *)
Do[
 check["cross left " <> ltKind <> " " <> ltType,
  ReverseMomentum[SpinorChain[Spinor[ltKind, ltType, 3], ltH["Square", 8]], 3] ===
   If[ltType === "Angle", -1, 1] SpinorChain[Spinor[ltKind, ltType, 3], ltH["Square", 8]]];
 check["cross right " <> ltKind <> " " <> ltType,
  ReverseMomentum[SpinorChain[ltH["Square", 8], Spinor[ltKind, ltType, 3]], 3] ===
   If[ltType === "Angle", -1, 1] SpinorChain[ltH["Square", 8], Spinor[ltKind, ltType, 3]]];
 , {ltKind, {"Helicity", "Spin"}}, {ltType, {"Angle", "Square"}}];
Do[
 check["cross explicit " <> ltVariance <> " " <> ltType,
  ReverseMomentum[SpinorChain[Spinor["Spin", ltVariance, ltType, 3, ltIndex], ltH["Square", 8]], 3] ===
   If[ltType === "Angle", -1, 1] SpinorChain[Spinor["Spin", ltVariance, ltType, 3, ltIndex], ltH["Square", 8]]];
 , {ltVariance, {"Upper", "Lower"}}, {ltType, {"Angle", "Square"}}];
ltMomentumChain = SpinorChain[ltH["Square", 7], Mom[3], ltH["Angle", 8]];
check["cross a momentum insertion", ReverseMomentum[ltMomentumChain, 3] === -ltMomentumChain];
check["cross either slot of a scalar product", ReverseMomentum[MomProd[3, 7], 3] === -MomProd[3, 7] &&
 ReverseMomentum[MomProd[7, 3], 3] === -MomProd[7, 3]];
check["cross both scalar-product momenta", ReverseMomentum[MomProd[3, 7], {3, 7}] === MomProd[3, 7]];
check["cross selected photons in a product", ReverseMomentum[ltB["Angle", 3, 4] ltB["Square", 4, 5], {3, 5}] ===
 -ltB["Angle", 3, 4] ltB["Square", 4, 5]];
check["cross a sum inside a propagator", ReverseMomentum[1/PropDen[Mom[1] + Mom[3], ltMass], 3] ===
 1/PropDen[Mom[1] - Mom[3], ltMass]];
check["cross a subtracted propagator momentum", ReverseMomentum[PropDen[Mom[1] - Mom[3], ltMass], 3] ===
 PropDen[Mom[1] + Mom[3], ltMass]];
check["crossing is an involution for an individual external occurrence",
 ReverseMomentum[ReverseMomentum[ltMomentumChain, 3], 3] === ltMomentumChain];
check["crossing does not relabel numerical coefficients, powers, mass or spin indices",
 ReverseMomentum[3 ltB["Angle", 3, 8]^2 Mass[3] ltIndex^3, 3] ===
 3 ltB["Angle", 3, 8]^2 Mass[3] ltIndex^3];
ltRepeatedCrossings = {
 {"two momentum insertions", SpinorChain[ltH["Square", 7], Mom[3], Mom[3], ltH["Square", 8]], 1},
 {"three momentum insertions", SpinorChain[ltH["Square", 7], Mom[3], Mom[8], Mom[3], Mom[3], ltH["Angle", 9]], -1},
 {"both helicity angle endpoints", SpinorChain[ltH["Angle", 3], Mom[7], Mom[8], ltH["Angle", 3]], 1},
 {"both implicit massive angle endpoints", SpinorChain[ltS["Angle", 3], Mom[7], Mom[8], ltS["Angle", 3]], 1},
 {"both explicit massive angle endpoints", SpinorChain[ltU["Angle", 3, ltI], Mom[7], Mom[8], ltL["Angle", 3, ltJ]], 1},
 {"mixed explicit and implicit angle endpoints", SpinorChain[ltU["Angle", 3, ltI], Mom[7], Mom[8], ltS["Angle", 3]], 1},
 {"left angle endpoint and momentum", SpinorChain[ltH["Angle", 3], Mom[3], ltH["Square", 8]], 1},
 {"right angle endpoint and momentum", SpinorChain[ltH["Square", 8], Mom[3], ltL["Angle", 3, ltI]], 1},
 {"both angle endpoints and momentum", SpinorChain[ltS["Angle", 3], Mom[3], ltU["Angle", 3, ltI]], -1},
 {"both angle endpoints and two momenta", SpinorChain[ltH["Angle", 3], Mom[3], Mom[3], ltH["Angle", 3]], 1},
 {"square endpoints and momentum", SpinorChain[ltH["Square", 3], Mom[3], ltS["Square", 3]], -1}};
Do[
 check["cross every occurrence: " <> ltCrossing[[1]],
  ReverseMomentum[ltCrossing[[2]], 3] === ltCrossing[[3]] ltCrossing[[2]]];
 check["repeated-occurrence crossing is an involution: " <> ltCrossing[[1]],
  ReverseMomentum[ReverseMomentum[ltCrossing[[2]], 3], 3] === ltCrossing[[2]]];
 , {ltCrossing, ltRepeatedCrossings}];
check["cross repeated occurrences independently in a list of expressions",
 ReverseMomentum[ltRepeatedCrossings[[All, 2]], 3] ===
  (#[[3]] #[[2]] & /@ ltRepeatedCrossings)];
check["cross repeated occurrences inside an inverse chain",
 ReverseMomentum[1/ltRepeatedCrossings[[9, 2]], 3] === -1/ltRepeatedCrossings[[9, 2]]];
check["cross repeated occurrences inside a powered chain",
 ReverseMomentum[ltRepeatedCrossings[[9, 2]]^3, 3] === -ltRepeatedCrossings[[9, 2]]^3];
check["crossing a scalar-product self term leaves the mass squared unchanged",
 ReverseMomentum[MomProd[3, 3], 3] === Mass[3]^2];
ltMultipleCrossings = SpinorChain[ltH["Angle", 3], Mom[3], Mom[4], Mom[4], ltS["Angle", 4]];
check["cross a particle list with multiple occurrences of each particle",
 ReverseMomentum[ltMultipleCrossings, {3, 4}] === -ltMultipleCrossings];
check["particle-list crossing is an involution for repeated occurrences",
 ReverseMomentum[ReverseMomentum[ltMultipleCrossings, {3, 4}], {3, 4}] === ltMultipleCrossings];
check["repeated-occurrence crossing removes all temporary markers",
 FreeQ[ReverseMomentum[ltRepeatedCrossings[[All, 2]], 3],
  _MomR | _HelicitySpinorR | _SpinSpinorR | _MomProdL | _MomProdR | _MomProdLR]];
check["crossing leaves unsupported standalone and shifted heads unchanged",
 ReverseMomentum[{Mom[3], Spinor["Helicity", "Angle", 3],
   SpinorChain[SpinorHat["Helicity", "Angle", 3], MomHat[3], Spinor["Xi", "Angle", 3]]}, 3] ===
  {Mom[3], Spinor["Helicity", "Angle", 3],
   SpinorChain[SpinorHat["Helicity", "Angle", 3], MomHat[3], Spinor["Xi", "Angle", 3]]}];
ltPermutationInput = 3 SpinorChain[ltH["Angle", 1], Mom[2], ltH["Square", 3]] MomProd[1, 3] Mandelstahm[2, 3];
ltPermutationExpected = 3 SpinorChain[ltH["Angle", 2], Mom[3], ltH["Square", 1]] MomProd[2, 1] Mandelstahm[3, 1];
check["legacy cyclic particle permutation", PermuteParticles[ltPermutationInput, {1, 2, 3}] === ltPermutationExpected];
check["particle cycle returns after its order",
 Nest[PermuteParticles[#, {1, 2, 3}] &, ltPermutationInput, 3] === ltPermutationInput];

(* Particle labels occupy typed positions, not the final spinor argument. *)
Do[
 check["cyclic relabeling preserves indexed " <> ltVariance <> " " <> ltType,
  PermuteParticles[SpinorChain[Spinor["Spin", ltVariance, ltType, 2, 7], ltH[ltType, 11]],
    {2, 7, 11}] === SpinorChain[Spinor["Spin", ltVariance, ltType, 7, 7], ltH[ltType, 2]]],
 {ltVariance, {"Upper", "Lower"}}, {ltType, {"Angle", "Square"}}];
Do[
 check["cyclic relabeling preserves four-argument helicity index " <> ltType,
  PermuteParticles[SpinorChain[Spinor["Helicity", ltType, 2, 11], ltH[ltType, 7]], {2, 7, 11}] ===
   SpinorChain[Spinor["Helicity", ltType, 7, 11], ltH[ltType, 11]]],
 {ltType, {"Angle", "Square"}}];
ltTypedPermutation = {2 Mass[2]^2, Mass[Multiparticle[2, 11]],
 Mom[Multiparticle[2, Multiparticle[7, 11]]],
 PropDen[Mom[Multiparticle[2, 11]], Mass[7]],
 SpinorChain[Spinor["Xi", "Angle", 2], ltH["Angle", 7]]^2,
 xFactor[Multiparticle[2, 11], 7], xTildeFactor[2, Multiparticle[7, 11]]};
ltTypedExpected = {2 Mass[7]^2, Mass[Multiparticle[7, 2]],
 Mom[Multiparticle[7, Multiparticle[11, 2]]],
 PropDen[Mom[Multiparticle[7, 2]], Mass[11]],
 SpinorChain[Spinor["Xi", "Angle", 2], ltH["Angle", 11]]^2,
 xFactor[Multiparticle[7, 2], 11], xTildeFactor[7, Multiparticle[11, 2]]};
check["cyclic relabeling reaches composite kinematics but preserves coefficients and Xi labels",
 PermuteParticles[ltTypedPermutation, {2, 7, 11}] === ltTypedExpected];
check["typed noncontiguous relabeling returns after one cycle order",
 Nest[PermuteParticles[#, {2, 7, 11}] &, ltTypedPermutation, 3] === ltTypedPermutation];
check["empty and singleton particle cycles are identities",
 PermuteParticles[ltTypedPermutation, {}] === ltTypedPermutation &&
 PermuteParticles[ltTypedPermutation, {2}] === ltTypedPermutation];

(* Crossing a composite momentum is a signed sum, not a label permutation. *)
ltCompositeChain = SpinorChain[ltH["Square", 7], Mom[Multiparticle[2, 11]], ltH["Angle", 8]];
ltCompositeCrossed = SpinorChain[ltH["Square", 7], Mom[2], ltH["Angle", 8]] -
 SpinorChain[ltH["Square", 7], Mom[11], ltH["Angle", 8]];
ltCrossingExtensions = {
 {"two-momentum trace", SpinorTrace[Mom[2], Mom[11]], -SpinorTrace[Mom[2], Mom[11]]},
 {"repeated trace momentum", SpinorTrace[Mom[11], Mom[2], Mom[7], Mom[11]],
   SpinorTrace[Mom[11], Mom[2], Mom[7], Mom[11]]},
 {"composite chain", ltCompositeChain, ltCompositeCrossed},
 {"composite propagator", PropDen[Mom[Multiparticle[2, 11]], ltMass], PropDen[Mom[2] - Mom[11], ltMass]},
 {"weighted propagator", PropDen[2 Mom[11] + Mom[2], ltMass], PropDen[Mom[2] - 2 Mom[11], ltMass]},
 {"indexed helicity angle", SpinorChain[Spinor["Helicity", "Angle", 11, 7], ltH["Angle", 2]],
   -SpinorChain[Spinor["Helicity", "Angle", 11, 7], ltH["Angle", 2]]},
 {"indexed helicity square", SpinorChain[Spinor["Helicity", "Square", 11, 7], ltH["Square", 2]],
   SpinorChain[Spinor["Helicity", "Square", 11, 7], ltH["Square", 2]]},
 {"four-argument massive angle", SpinorChain[Spinor["Spin", "Angle", 11, 2], ltH["Angle", 7]],
   -SpinorChain[Spinor["Spin", "Angle", 11, 2], ltH["Angle", 7]]},
 {"composite trace", SpinorTrace[Mom[Multiparticle[2, 11]], Mom[7]],
   SpinorTrace[Mom[2], Mom[7]] - SpinorTrace[Mom[11], Mom[7]]}
};
Do[
 check["ordinary typed crossing: " <> ltCrossingCase[[1]],
  ReverseMomentum[ltCrossingCase[[2]], 11] === ltCrossingCase[[3]]],
 {ltCrossingCase, ltCrossingExtensions}];
check["trace crossing commutes with independent scalar-product reduction",
 ReduceSpinorProducts[ReverseMomentum[SpinorTrace[Mom[2], Mom[11]], 11]] ===
  ReverseMomentum[ReduceSpinorProducts[SpinorTrace[Mom[2], Mom[11]]], 11] === -2 MomProd[2, 11]];
check["composite chain crossing twice restores its linear momentum expansion",
 ltSame[ReverseMomentum[ReverseMomentum[ltCompositeChain, 11], 11],
  SpinorChain[ltH["Square", 7], Mom[2], ltH["Angle", 8]] +
  SpinorChain[ltH["Square", 7], Mom[11], ltH["Angle", 8]]]];
check["composite photon crossing preserves spin-index and reference-label numbers",
 ReverseMomentum[SpinorChain[Spinor["Helicity", "Angle", 11, 11], Mom[11],
   Spinor["Xi", "Square", 11]], 11] ===
  SpinorChain[Spinor["Helicity", "Angle", 11, 11], Mom[11], Spinor["Xi", "Square", 11]]];

(* Explicit spin indices, sums over little-group indices, and bosonic symmetrization. *)
Do[
 check["make half-spin explicit " <> ToString[ltHead] <> " " <> ltType,
  MakeIndicesExplicit[SpinorChain[ltHead["Spin", ltType, 3], ltH["Square", 8]], 3, {ltI}] ===
   SpinorChain[ltHead["Spin", "Upper", ltType, 3, ltI], ltH["Square", 8]]];
 , {ltHead, {Spinor, SpinorHat}}, {ltType, {"Angle", "Square"}}];
ltVectorChain = SpinorChain[ltS["Angle", 3], Mom[7], ltS["Square", 3]];
ltVectorIndexed[i_, j_] := SpinorChain[ltU["Angle", 3, i], Mom[7], ltU["Square", 3, j]];
check["make spin-one indices explicit within a single chain",
 MakeIndicesExplicit[ltVectorChain, 3, {ltI, ltJ}] === ltVectorIndexed[ltI, ltJ]];
check["symmetrize two vector indices with the one-half normalization",
 ltSame[SymmetrizeSpin[ltVectorChain, 3, {ltI, ltJ}],
  (ltVectorIndexed[ltI, ltJ] + ltVectorIndexed[ltJ, ltI])/2]];
ltSquaredVector = SpinorChain[ltS["Angle", 3], ltH["Angle", 8]]^2;
check["explicit vector indices distribute across a squared bracket",
 MakeIndicesExplicit[ltSquaredVector, 3, {ltI, ltJ}] ===
  SpinorChain[ltU["Angle", 3, ltI], ltH["Angle", 8]] *
  SpinorChain[ltU["Angle", 3, ltJ], ltH["Angle", 8]]];
check["longitudinal squared bracket has the required root-two weight",
 ChooseSpinIndex[ltSquaredVector, 3, 0] === Sqrt[2] *
  SpinorChain[ltU["Angle", 3, -1/2], ltH["Angle", 8]] *
  SpinorChain[ltU["Angle", 3, 1/2], ltH["Angle", 8]]];
ltIndexProduct[n_] := Times @@ Table[SpinorChain[ltS["Angle", 3], ltH["Angle", 10 + k]], {k, n}];
ltAssignedProduct[indices_List] := Times @@ MapIndexed[
 SpinorChain[ltU["Angle", 3, #1], ltH["Angle", 10 + First[#2]]] &, indices];
Do[
 ltIndices = Take[{ltI, ltJ, ltK, ltLIndex}, ltN];
 ltExplicit = MakeIndicesExplicit[ltIndexProduct[ltN], 3, ltIndices];
 check["explicit rank-" <> ToString[ltN] <> " spin tensor uses each index once",
  FreeQ[ltExplicit, Spinor["Spin", _, 3]] &&
  Sort[Cases[ltExplicit, Spinor["Spin", "Upper", "Angle", 3, ii_] :> ii, Infinity]] === Sort[ltIndices]];
 check["symmetrized rank-" <> ToString[ltN] <> " spin tensor equals the normalized permutation sum",
  ltSame[SymmetrizeSpin[ltIndexProduct[ltN], 3, ltIndices],
   Total[ltAssignedProduct /@ Permutations[ltIndices]]/Factorial[ltN]]];
 , {ltN, 2, 4}];
ltWrapped = ConstructiveAmplitude[ltCoupling, SpinorChain[ltS["Angle", 1], ltS["Square", 2]], ltDenominator];
check["list-index API preserves the constructive amplitude wrapper",
 MakeIndicesExplicit[ltWrapped, {{ltI}, {ltJ}}] === ConstructiveAmplitude[ltCoupling,
  SpinorChain[ltU["Angle", 1, ltI], ltU["Square", 2, ltJ]], ltDenominator]];
check["list symmetrization preserves coupling and denominator",
 SymmetrizeSpins[ltWrapped, {{ltI}, {ltJ}}] === ConstructiveAmplitude[ltCoupling,
  SpinorChain[ltU["Angle", 1, ltI], ltU["Square", 2, ltJ]], ltDenominator]];
check["make implicit restores ordinary upper indices",
 MakeIndicesImplicit[SpinorChain[ltU["Angle", 1, ltI], Mom[7], ltU["Square", 2, ltJ]]] ===
 SpinorChain[ltS["Angle", 1], Mom[7], ltS["Square", 2]]];
Do[
 check["choose spin-half projection " <> ToString[ltProjection],
  ChooseSpinIndex[SpinorChain[ltS["Angle", 3], ltH["Square", 8]], 3, ltProjection] ===
   SpinorChain[ltU["Angle", 3, ltProjection], ltH["Square", 8]]];
 , {ltProjection, {-1/2, 1/2}}];
check["choose both particle projections",
 ChooseSpinIndices[ltWrapped[[2]], -1/2, 1/2] === SpinorChain[ltU["Angle", 1, -1/2], ltU["Square", 2, 1/2]]];
Do[
 check["choose vector extremal projection " <> ToString[ltProjection],
  ChooseSpinIndex[ltIndexProduct[2], 3, ltProjection] === ltAssignedProduct[{ltProjection/2, ltProjection/2}]];
 , {ltProjection, {-1, 1}}];
check["choose longitudinal vector projection with one-over-root-two normalization",
 ltSame[ChooseSpinIndex[ltIndexProduct[2], 3, 0],
  (ltAssignedProduct[{-1/2, 1/2}] + ltAssignedProduct[{1/2, -1/2}])/Sqrt[2]]];
ltSummand[j_] := SpinorChain[ltU["Angle", 1, j], ltH["Angle", 8]] *
 SpinorChain[ltH["Square", 9], ltL["Square", 1, j]];
check["expand a contracted index across separate chains",
 ltSame[ExpandSummation[ltSummand[ltI]], ltSummand[-1/2] + ltSummand[1/2]]];
check["expand a contracted index inside one chain",
 ltSame[ExpandSummation[SpinorChain[ltU["Angle", 1, ltI], Mom[7], ltL["Square", 1, ltI]]],
  SpinorChain[ltU["Angle", 1, -1/2], Mom[7], ltL["Square", 1, -1/2]] +
  SpinorChain[ltU["Angle", 1, 1/2], Mom[7], ltL["Square", 1, 1/2]]]];
check["already fixed numeric indices are not summed again", ExpandSummation[ltSummand[1/2]] === ltSummand[1/2]];
check["a single free spin index is not spuriously summed",
 ExpandSummation[SpinorChain[ltU["Angle", 1, ltI], ltH["Angle", 8]]] ===
  SpinorChain[ltU["Angle", 1, ltI], ltH["Angle", 8]]];

(* Conjugation assumes real momenta and explicit massive indices. *)
Do[
 ltConjugateType = If[ltType === "Angle", "Square", "Angle"];
 check["conjugate helicity chain " <> ltType,
  Conjugate[SpinorChain[ltH[ltType, 1], Mom[3], ltH[ltConjugateType, 2]]] ===
   SpinorChain[ltH[ltType, 2], Mom[3], ltH[ltConjugateType, 1]]];
 , {ltType, {"Angle", "Square"}}];
Do[
 ltOtherType = If[ltType === "Angle", "Square", "Angle"];
 ltOtherVariance = If[ltVariance === "Upper", "Lower", "Upper"];
 ltConjugateSign = If[{ltVariance, ltType} === {"Lower", "Angle"} ||
   {ltVariance, ltType} === {"Upper", "Square"}, -1, 1];
 ltConjugateChain = SpinorChain[Spinor["Spin", ltVariance, ltType, 1, ltI], Mom[3], ltH[ltOtherType, 2]];
 check["conjugate massive " <> ltVariance <> " " <> ltType,
  Conjugate[ltConjugateChain] === ltConjugateSign SpinorChain[ltH[ltType, 2], Mom[3],
   Spinor["Spin", ltOtherVariance, ltOtherType, 1, ltI]]];
 check["double conjugation massive " <> ltVariance <> " " <> ltType,
  Conjugate[Conjugate[ltConjugateChain]] === ltConjugateChain];
 , {ltVariance, {"Upper", "Lower"}}, {ltType, {"Angle", "Square"}}];
check["real symbolic kinematic heads under conjugation",
 Conjugate[Mass[3] Coupling[1] MomProd[1, 2] Mandelstahm[2, 3]] ===
 Mass[3] Coupling[1] MomProd[1, 2] Mandelstahm[2, 3]];
check["finite-width propagator conjugation reverses its width",
 Conjugate[PropDen[Mom[3], ltMass, ltWidth]] === PropDen[Mom[3], ltMass, -ltWidth]];
check["zero-width propagator is real", Conjugate[PropDen[Mom[3], ltMass]] === PropDen[Mom[3], ltMass]];
check["conjugate scalar and amplitude wrapper",
 Conjugate[ConstructiveAmplitude[I, ltB["Angle", 1, 2], ltDenominator]] ===
 ConstructiveAmplitude[-I, ltB["Square", 2, 1], ltDenominator]];
check["square a purely helicity amplitude",
 SquareAmplitude[2 I ltB["Angle", 1, 2], {0, 0}] ===
 4 ltB["Angle", 1, 2] ltB["Square", 2, 1]];
check["square a two-fermion amplitude leaves explicit contracted spin indices",
 SquareAmplitude[SpinorChain[ltS["Angle", 1], ltS["Angle", 2]], {1/2, 1/2}] ===
 SpinorChain[ltU["Angle", 1, i1], ltU["Angle", 2, i2]] *
 SpinorChain[ltL["Square", 2, i2], ltL["Square", 1, i1]]];

(* Numerical spinor components are independently assembled from the angular
   two-spinor basis. On-shell test momenta use E=5, |p|=4, m=3. *)
ltEnergy = 5; ltMagnitude = 4; ltTheta = Pi/3; ltPhi = Pi/5;
ltC = Cos[ltTheta/2]; ltSine = Sin[ltTheta/2] Exp[I ltPhi]; ltSineConjugate = Sin[ltTheta/2] Exp[-I ltPhi];
ltUpperMomentum = {{ltEnergy - ltMagnitude Cos[ltTheta], -ltMagnitude Sin[ltTheta] Exp[-I ltPhi]},
 {-ltMagnitude Sin[ltTheta] Exp[I ltPhi], ltEnergy + ltMagnitude Cos[ltTheta]}};
ltLowerMomentum = {{ltEnergy + ltMagnitude Cos[ltTheta], ltMagnitude Sin[ltTheta] Exp[-I ltPhi]},
 {ltMagnitude Sin[ltTheta] Exp[I ltPhi], ltEnergy - ltMagnitude Cos[ltTheta]}};
Do[
 check["explicit momentum matrix at left " <> ltType,
  ExtractEnergy1[SpinorChain[ltH[ltType, 8], Mom[3], ltH["Square", 9]], 3,
    ltEnergy, ltMagnitude, ltTheta, ltPhi] ===
   SpinorChain[ltH[ltType, 8], If[ltType === "Angle", ltLowerMomentum, ltUpperMomentum], ltH["Square", 9]]];
 , {ltType, {"Angle", "Square"}}];
check["explicit scalar product is Minkowski rather than Euclidean",
 ExtractEnergy[MomProd[1, 2], {{1}, {2}}] /. {En[1] -> 5, En[2] -> 13,
 MomMagnitude[1] -> 4, MomMagnitude[2] -> 12, Theta[1] -> 0, Theta[2] -> Pi/2,
 Phi[1] -> 0, Phi[2] -> 0} // Simplify // (# === 65 &)];
check["explicit two-momentum trace has the independent two-by-two trace value",
 Simplify[ExtractEnergy[SpinorTrace[Mom[1], Mom[2]], {{1}, {2}}] /.
  {En[1] -> 5, En[2] -> 13, MomMagnitude[1] -> 4, MomMagnitude[2] -> 12,
   Theta[1] -> 0, Theta[2] -> Pi/2, Phi[1] -> 0, Phi[2] -> 0}] === 130];
ltMasslessPoint = {En[1] -> 5, En[2] -> 7, MomMagnitude[1] -> 5, MomMagnitude[2] -> 7,
 Theta[1] -> 0, Theta[2] -> Pi/2, Phi[1] -> 0, Phi[2] -> 0};
check["explicit massless angle and square brackets reproduce the Lorentz invariant",
 Simplify[ExtractEnergy[ltB["Angle", 1, 2] ltB["Square", 2, 1], {{1}, {2}}] /.
  ltMasslessPoint] === 70];
check["explicit massless bracket retains its complex phase away from the x-z plane",
 Simplify[ExtractEnergy[ltB["Angle", 1, 2], {{1}, {2}}] /.
  ReplaceAll[ltMasslessPoint, HoldPattern[Phi[2] -> 0] -> (Phi[2] -> Pi/2)]] === -I Sqrt[70]];
ltEnergySpinors = {
 {ltH["Angle", 3], Sqrt[10] {ltSine, -ltC}, Sqrt[10] {ltC, ltSine}},
 {ltH["Square", 3], Sqrt[10] {ltC, ltSineConjugate}, Sqrt[10] {ltSineConjugate, -ltC}},
 {ltU["Angle", 3, -1/2], 3 {ltSine, -ltC}, 3 {ltC, ltSine}},
 {ltU["Angle", 3, 1/2], {ltC, ltSineConjugate}, {-ltSineConjugate, ltC}},
 {ltL["Angle", 3, -1/2], {ltC, ltSineConjugate}, {-ltSineConjugate, ltC}},
 {ltL["Angle", 3, 1/2], -3 {ltSine, -ltC}, -3 {ltC, ltSine}},
 {ltU["Square", 3, -1/2], -{-ltSine, ltC}, -{ltC, ltSine}},
 {ltU["Square", 3, 1/2], 3 {ltC, ltSineConjugate}, 3 {ltSineConjugate, -ltC}},
 {ltL["Square", 3, -1/2], 3 {ltC, ltSineConjugate}, 3 {ltSineConjugate, -ltC}},
 {ltL["Square", 3, 1/2], {-ltSine, ltC}, {ltC, ltSine}}};
Do[
 check["explicit left spinor component " <> ToString[ltK],
  ExtractEnergy2[SpinorChain[ltEnergySpinors[[ltK, 1]], ltMarker], 3,
   ltEnergy, ltMagnitude, ltTheta, ltPhi] === SpinorChain[ltEnergySpinors[[ltK, 2]], ltMarker]];
 check["explicit right spinor component " <> ToString[ltK],
  ExtractEnergy2[SpinorChain[ltMarker, ltEnergySpinors[[ltK, 1]]], 3,
   ltEnergy, ltMagnitude, ltTheta, ltPhi] === SpinorChain[ltMarker, ltEnergySpinors[[ltK, 3]]]];
 , {ltK, Length[ltEnergySpinors]}];
Do[
 ltDirac = SpinorChain[ltU[ltType, 1, ltProjection], Mom[1], ltH["Angle", 2]] -
  3 SpinorChain[ltU[If[ltType === "Angle", "Square", "Angle"], 1, ltProjection], ltH["Angle", 2]];
 check["explicit massive Dirac equation " <> ltType <> " " <> ToString[ltProjection],
  Max[Abs[N[ExtractEnergy[ltDirac, {{1}, {2}}] /. {En[1] -> 5, MomMagnitude[1] -> 4,
    Theta[1] -> Pi/3, Phi[1] -> Pi/5, En[2] -> 7, MomMagnitude[2] -> 7,
    Theta[2] -> Pi/7, Phi[2] -> Pi/11}]]] < 10^-12];
 , {ltType, {"Angle", "Square"}}, {ltProjection, {-1/2, 1/2}}];
Do[
 ltSource = SpinorChain[ltS[ltType, 3], ltH["Square", 8]];
 ltExpectedSpinor = Which[
  {ltType, ltProjection} === {"Angle", -1/2}, 3/Sqrt[10] SpinorChain[ltH["Angle", 3], ltH["Square", 8]],
  {ltType, ltProjection} === {"Angle", 1/2}, SpinorChain[Spinor["Zeta", "Angle", 3], ltH["Square", 8]],
  {ltType, ltProjection} === {"Square", -1/2}, -SpinorChain[Spinor["Zeta", "Square", 3], ltH["Square", 8]],
  True, 3/Sqrt[10] SpinorChain[ltH["Square", 3], ltH["Square", 8]]];
 check["spin-to-helicity/Zeta half-spin " <> ltType <> " " <> ToString[ltProjection],
  ltSame[SpinToHelicitySpinors[ltSource, {3, 1/2, ltProjection}] /.
    {En[3] -> 5, MomMagnitude[3] -> 4}, ltExpectedSpinor]];
 , {ltType, {"Angle", "Square"}}, {ltProjection, {-1/2, 1/2}}];
check["spin-to-helicity longitudinal vector retains both helicity and Zeta sectors",
 ltSame[SpinToHelicitySpinors[ltVectorChain, {3, 1, 0}],
  ((En[3] + MomMagnitude[3])/(2 En[3]) SpinorChain[ltH["Angle", 3], Mom[7], ltH["Square", 3]] -
   (En[3] - MomMagnitude[3]) SpinorChain[Spinor["Zeta", "Angle", 3], Mom[7], Spinor["Zeta", "Square", 3]])/Sqrt[2]]];

(* Legacy complex-shift branches are tested independently from real-momentum
   conjugation and crossing, which explicitly do not promise shifted kinematics. *)
ltHatSource = MomProd[1, 2] SpinorChain[ltU["Square", 1, ltI], Mom[3], ltH["Angle", 2]];
check["complexify every typed spinor and momentum",
 ComplexifyMomenta[ltHatSource] === MomProdHat12[1, 2] SpinorChain[
  SpinorHat["Spin", "Upper", "Square", 1, ltI], MomHat[3], SpinorHat["Helicity", "Angle", 2]]];
check["unhat reverses full complexification", UnhatMomenta[ComplexifyMomenta[ltHatSource]] === ltHatSource];
check["complexify a selected BCFW pair leaves unrelated momentum unchanged",
 ComplexifyMomenta[ltHatSource, 1, 2] === MomProdHat12[1, 2] SpinorChain[
  SpinorHat["Spin", "Upper", "Square", 1, ltI], Mom[3], SpinorHat["Helicity", "Angle", 2]]];
check["selected shifted scalar product with an unshifted spectator",
 ComplexifyMomenta[MomProd[3, 1] + MomProd[3, 2] + MomProd[3, 4], 1, 2] ===
  MomProdHat1[1, 3] + MomProdHat1[2, 3] + MomProd[3, 4]];
check["massive/massive massless-channel shift replaces first momentum by minus complement",
 ExpandComplexMomenta[SpinorChain[ltH["Angle", 8], MomHat[1], ltH["Square", 9]],
  {1, ltMass1}, 3, {2, ltMass2}, 4, 0] ===
  -SpinorChain[ltH["Angle", 8], Mom[3], ltH["Square", 9]]];
check["massive/massive shift replaces second momentum by minus complement",
 ExpandComplexMomenta[MomProdHat1[2, 8], {1, ltMass1}, 3, {2, ltMass2}, 4, 0] === -MomProd[4, 8]];
check["massive/massive shifted endpoint uses its mass and complementary momentum",
 ExpandComplexMomenta[SpinorChain[SpinorHat["Spin", "Upper", "Square", 1, ltI], ltH["Angle", 8]],
  {1, ltMass1}, 3, {2, ltMass2}, 4, 0] ===
  -SpinorChain[ltU["Angle", 1, ltI], Mom[3], ltH["Angle", 8]]/Mass[1]];
check["massive/massive right shifted endpoint has the opposite endpoint sign",
 ExpandComplexMomenta[SpinorChain[ltH["Angle", 8], SpinorHat["Spin", "Lower", "Square", 1, ltI]],
  {1, ltMass1}, 3, {2, ltMass2}, 4, 0] ===
  SpinorChain[ltH["Angle", 8], Mom[3], ltL["Angle", 1, ltI]]/Mass[1]];
check["massive/massive unsupported massive-channel shift is not silently evaluated",
 ExpandComplexMomenta[MomProdHat1[1, 8], {1, ltMass1}, 3, {2, ltMass2}, 4,
  ltChannelMass] === MomProdHat1[1, 8]];
Do[
 check["mutual shifted invariant is unchanged in mass branch " <> ToString[ltBranch],
  ExpandComplexMomenta[MomProdHat12[1, 2], {1, ltBranch[[1]]}, 3,
   {2, ltBranch[[2]]}, 4, If[ltBranch === {ltMass1, ltMass2}, 0, ltChannelMass]] === MomProd[1, 2]];
 , {ltBranch, {{0, 0}, {0, ltMass2}, {ltMass1, 0}, {ltMass1, ltMass2}}}];
ltShiftCoefficient = PropDen[Mom[1] + Mom[3], ltChannelMass]/
 SpinorChain[ltH["Angle", 1], Mom[3], ltH["Square", 2]];
check["massless/massless shifted square endpoint has the exact deformation coefficient",
 ltSame[ExpandComplexMomenta[SpinorChain[SpinorHat["Helicity", "Square", 1], ltH["Square", 8]],
  {1, 0}, 3, {2, 0}, 4, ltChannelMass],
  ltB["Square", 1, 8] - ltB["Square", 2, 8] ltShiftCoefficient]];
check["replace massive SpinorZ includes square and momentum contributions",
 ReplaceSpinorZ[SpinorZ["Both", ltI, ltJ], {1, ltMass1}, {2, ltMass2}, 3] ===
  (ltMass1 SpinorChain[ltL["Square", 1, ltI], ltU["Square", 2, ltJ]] +
   SpinorChain[ltL["Angle", 1, ltI], Mom[3], ltU["Square", 2, ltJ]])/(ltMass1 ltMass2)];
check["SpinorZ remains unchanged outside the supported massive/massive case",
 ReplaceSpinorZ[SpinorZ["Both", ltI, ltJ], {1, 0}, {2, ltMass2}, 3] === SpinorZ["Both", ltI, ltJ]];

(* User-entered string amplitudes and human/code exporters are public legacy
   workflows even though formatting is not part of simplification itself. *)
check["parse mixed massive/massless bracket and momentum chain",
 ConvertStringAmplitude["<12> + [3|p1|4]", {1, 1, 0, 0}] ===
  SpinorChain[ltS["Angle", 1], ltS["Angle", 2]] + SpinorChain[ltH["Square", 3], Mom[1], ltH["Square", 4]]];
check["parse scalar invariants and x factors", ConvertStringAmplitude["2 p1.p2+s13+x24", {1, 1, 0, 0}] ===
 2 MomProd[1, 2] + Mandelstahm[1, 3] + xFactor[2, 4]];
check["parse explicit upper/lower spin indices", ConvertStringAmplitude["<1^I2_J>", {1, 1}] ===
 SpinorChain[ltU["Angle", 1, I], ltL["Angle", 2, J]]];
check["parse Zeta endpoints", ConvertStringAmplitude["<z1z2>", {1, 1}] ===
 SpinorChain[Spinor["Zeta", "Angle", 1], Spinor["Zeta", "Angle", 2]]];
check["parse a composite momentum insertion", ConvertStringAmplitude["<1|P23|4]", {1, 0, 0, 1}] ===
 SpinorChain[ltS["Angle", 1], Mom[Multiparticle[2, 3]], ltS["Square", 4]]];
check["parse a composite external endpoint with supplied internal mass",
 ConvertStringAmplitude["<P12 3>", {1, 1, 0}, InternalMasses -> {Mass[1, 2] -> 0}] ===
 SpinorChain[ltH["Angle", Multiparticle[1, 2]], ltH["Angle", 3]]];
check["parse hatted spinors and momenta",
 ConvertStringHattedAmplitude["<1|p2|3]", {0, 0, 0}] ===
 SpinorChain[SpinorHat["Helicity", "Angle", 1], MomHat[2], SpinorHat["Helicity", "Square", 3]]];
check["TeX angle-bracket notation", TeX[ltB["Angle", 1, 2]] === "\\langle12\\rangle"];
check["TeX square-bracket notation", TeX[ltB["Square", 1, 2]] === "\\lbrack12\\rbrack "];
check["TeX momentum insertion notation", TeX[SpinorChain[ltH["Angle", 1], Mom[3], ltH["Square", 2]]] ===
 "\\langle1\\lvert p_{3} \\rvert 2\\rbrack "];
check["TeX explicit massive index", TeX[ltU["Angle", 3, ltI]] === "\\mathbf{3}^{ltI}\\rangle "];
check["TeX Zeta notation", TeX[Spinor["Zeta", "Square", 3]] === "\\tilde{\\zeta}_{3}\\rbrack"];
check["TeX propagator keeps physical momentum and width", TeX[PropDen[Mom[3], 2, 4]] ===
 "\\left[p_{3} ^2-2^2+i24\\right]"];
check["PlainText bracket notation", PlainText[ltB["Angle", 1, 2]] === "<12>" && PlainText[ltB["Square", 1, 2]] === "[12]"];
check["PlainText indexed massive spinor", PlainText[ltL["Square", 3, ltI]] === "3_{ltI}]"];
check["PlainText Zeta notation", PlainText[Spinor["Zeta", "Square", 3]] === "tzeta_{3}]"];
check["display momentum and propagator preserve symbolic structure", nice[Mom[3]] === Subscript[p, 3] &&
 nice[PropDen[Mom[3], ltMass]] === Subscript[p, 3]^2 - ltMass^2];
check["display Xi and hatted spinor styles", nice[Spinor["Xi", "Angle", 3]] ===
 Row[{Subscript[\[Xi], 3], "\[RightAngleBracket]"}] &&
 nice[SpinorHat["Helicity", "Angle", 3]] === Row[{OverHat[3], "\[RightAngleBracket]"}]];
check["display r-spinor labels", nice[rr[3, 4]] === Subscript["r", "34"]];
Do[
 ltTeXBracket = If[ltType === "Angle", "\\rangle ", "\\rbrack "];
 ltPlainBracket = If[ltType === "Angle", ">", "]"];
 ltIndexNotation = If[ltVariance === "Upper", "^", "_"];
 check["TeX indexed branch " <> ltVariance <> " " <> ltType,
  TeX[Spinor["Spin", ltVariance, ltType, 3, ltI]] ===
   "\\mathbf{3}" <> ltIndexNotation <> "{ltI}" <> ltTeXBracket];
 check["PlainText indexed branch " <> ltVariance <> " " <> ltType,
  PlainText[Spinor["Spin", ltVariance, ltType, 3, ltI]] ===
   "3" <> ltIndexNotation <> "{ltI}" <> ltPlainBracket];
 , {ltVariance, {"Upper", "Lower"}}, {ltType, {"Angle", "Square"}}];
check["SPINAS helicity bracket expression", SPINAS[ltB["Angle", 1, 2]] === "a12a.v()"];
check["SPINAS massive endpoint projection", SPINAS[SpinorChain[ltU["Angle", 1, ltI], ltH["Square", 2]]] === "a12s.v(ltI)"];
ltExportChain = SpinorChain[ltH["Square", 1], Mom[3], ltH["Angle", 2]];
check["SPINAS reusable chain name", CreateSPName[ltExportChain] === "s132a"];
check["SPINAS initialization statement", CreateSPInitializationStatement[ltExportChain] ===
 "s132a= sproduct(SQUARE,&p1,&p3,&p2);"];
check["SPINAS update statement", CreateSPUpdateStatement[ltExportChain] === "s132a.update();"];
check["SPINAS declarations deduplicate repeated chains",
 DeclareSPINASSpinorProducts[ltExportChain + ltExportChain^2] === "//Spinor Products\n sproduct s132a;"];
check["SPINAS initializations deduplicate repeated chains",
 InitializeSPINASSpinorProducts[ltExportChain + ltExportChain^2] ===
 "//Spinor Products\ns132a= sproduct(SQUARE,&p1,&p3,&p2);"];
check["SPINAS updates deduplicate repeated chains",
 UpdateSPINASSpinorProducts[ltExportChain + ltExportChain^2] === "//Spinor Products\ns132a.update();"];
Do[
 ltLeftSpinor = If[ltLeftIndexed, ltU[ltLeftType, 1, ltI], ltH[ltLeftType, 1]];
 ltRightSpinor = If[ltRightIndexed, ltU[ltRightType, 2, ltJ], ltH[ltRightType, 2]];
 ltExportName = If[ltLeftType === "Angle", "a", "s"] <> "132" <>
  If[ltRightType === "Angle", "a", "s"];
 ltExportIndices = StringRiffle[Pick[{"ltI", "ltJ"}, {ltLeftIndexed, ltRightIndexed}, True], ","];
 check["SPINAS chirality and endpoint index branch " <>
   StringRiffle[{ltLeftType, ltRightType, ToString[ltLeftIndexed], ToString[ltRightIndexed]}, " "],
  SPINAS[SpinorChain[ltLeftSpinor, Mom[3], ltRightSpinor]] === ltExportName <> ".v(" <> ltExportIndices <> ")"];
 , {ltLeftType, {"Angle", "Square"}}, {ltRightType, {"Angle", "Square"}},
   {ltLeftIndexed, {False, True}}, {ltRightIndexed, {False, True}}];

(* SU(3) completeness: sum_a tr(Ta Ta)=4 and a closed color line has 3 states. *)
check["color empty trace dimension", ReduceColor[colorTrace[]] === 3];
check["color single-generator trace is zero", ReduceColor[colorTrace[colorT[ltA]]] === 0];
check["color summed generator pair", ReduceColor[colorTrace[colorT[ltA], colorT[ltA]]] === 4];
check["color explicit index loop reduces through the same SU3 normalization",
 ReduceColor[colorT[ltA, ltI, ltJ] colorT[ltA, ltJ, ltI]] === 4];
check["color disconnected single traces vanish", ReduceColor[colorTrace[colorT[ltA]] colorTrace[colorT[ltBColor]]] === 0];
check["color two contracted two-generator traces",
 ReduceColor[colorTrace[colorT[ltA], colorT[ltBColor]]^2] === 2];
check["color generators separated by a spectator generator obey the SU3 Fierz term",
 ReduceColor[colorTrace[colorT[ltA], colorT[ltBColor], colorT[ltA], colorT[ltBColor]]] === -2/3];

If[checks =!= 225, AppendTo[failures,
  {"suite did not reach every check", "Expected" -> 225, "Actual" -> checks}]];
Print["Legacy transformations: ", checks, " checks, ", Length[failures], " failures."];
If[failures =!= {}, Print[failures]];
Print["HEPCAT_TEST_COMPLETED:legacy-transform-tests.wl"];
Exit[If[failures === {}, 0, 1]];
