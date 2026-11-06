(* Research formula, separate from ConstructAmplitude. Load HEPCAT first.
   All momenta are ingoing. Fermion labels are {e1,E2,E3,e4}; positive
   photons occupy 5,...,photonCount+4. All four fermions have mass m.
   No internal on-shell condition is used. The expression retains the
   regular Pauli-Pauli contribution after its photon denominator cancels. *)

ClearAll[QEDFourFermionAllPlusFormula];
Options[QEDFourFermionAllPlusFormula] = {"AngleReference" -> Automatic};
QEDFourFermionAllPlusFormula[photonCount_Integer /; photonCount >= 0,
    mass_, charge_, OptionsPattern[]] := Module[
  {photons = Range[5, photonCount + 4], reference = OptionValue["AngleReference"],
   angleSpinor, squareSpinor, angle, referenceDenominator, referenceInsertion,
   angleToReference, current, weight, scalar, momentumWeights, dotMomenta,
   vectorInsertion, twoMomentumChain, squareMomentumChain, vectorPauli,
   orbitalDot, sewing, photonPole, amplitude, massRules},
  If[TrueQ[mass == 0], Return[Failure["MasslessFermion", <|"MessageTemplate" ->
    "The two-current representation requires a nonzero common fermion mass."|>]]];
  If[reference =!= Automatic && !MatchQ[reference, Spinor[_, "Angle", __]],
    Return[Failure["AngleReference", <|"MessageTemplate" ->
      "Use Automatic or an explicit angle spinor as the common reference."|>]]];
  If[MatchQ[reference, Spinor["Helicity", "Angle", _Integer]] &&
      MemberQ[photons, reference[[3]]],
    Return[Failure["SingularAngleReference", <|"MessageTemplate" ->
      "An all-positive photon cannot be the common angle reference because its own denominator vanishes."|>]]];
  angleSpinor[i_] := Spinor[If[MemberQ[Range[4], i], "Spin", "Helicity"], "Angle", i];
  squareSpinor[i_] := Spinor[If[MemberQ[Range[4], i], "Spin", "Helicity"], "Square", i];
  angle[i_, j_] := SpinorChain[angleSpinor[i], angleSpinor[j]];
  (* Automatic means |q> = p1 |5]. These signs follow the package's
     antisymmetric angle and square forms; no free Xi is introduced. *)
  referenceDenominator[i_] := If[reference === Automatic,
    -SpinorChain[squareSpinor[5], Mom[1], angleSpinor[i]],
    SpinorChain[reference, angleSpinor[i]]];
  referenceInsertion[p_, i_] := If[reference === Automatic,
    -SpinorChain[squareSpinor[5], Mom[1], Mom[p], squareSpinor[i]],
    SpinorChain[reference, Mom[p], squareSpinor[i]]];
  angleToReference[i_] := If[reference === Automatic,
    SpinorChain[angleSpinor[i], Mom[1], squareSpinor[5]],
    SpinorChain[angleSpinor[i], reference]];
  photonPole[labels_List] := PropDen[Mom[Multiparticle @@ Sort[labels]], 0];
  current[endpoint_, {}] = 1;
  current[endpoint_, labels_List] := current[endpoint, labels] =
    Total[Function[i, With[{previous = DeleteCases[labels, i]},
      current[endpoint, previous] (referenceInsertion[endpoint, i] +
        Total[referenceInsertion[#, i] & /@ previous])/referenceDenominator[i]]]
      /@ labels]/PropDen[Mom[Multiparticle @@ Sort[Prepend[labels, endpoint]]], mass];
  weight[a_, b_, labels_List, left_List] := With[{right = Complement[labels, left]},
    (-1)^Length[right] current[a, left] current[b, right]];
  scalar[a_, b_, labels_List] := scalar[a, b, labels] =
    Total[weight[a, b, labels, #] & /@ Subsets[labels]];
  (* V^mu = C p_a^mu + sum_i coefficient_i p_i^mu. Keeping its scalar
     momentum coefficients avoids expanding every pair of partitions. *)
  momentumWeights[a_, b_, labels_List] := momentumWeights[a, b, labels] =
    Prepend[Function[i, {Total[weight[a, b, labels, #] & /@
        Select[Subsets[labels], MemberQ[#, i] &]], i}] /@ labels,
      {scalar[a, b, labels], a}];
  dotMomenta[left_List, right_List] := Total[Flatten[Table[
    l[[1]] r[[1]] MomProd[l[[2]], r[[2]]], {l, left}, {r, right}], 1]];
  vectorInsertion[weights_List, i_] :=
    Total[#[[1]] referenceInsertion[#[[2]], i] & /@ weights];
  twoMomentumChain[c_, weights_List, exchanged_List, d_] :=
    Total[Flatten[Table[w[[1]] SpinorChain[angleSpinor[c], Mom[w[[2]]],
      Mom[p], angleSpinor[d]], {w, weights}, {p, exchanged}], 1]];
  squareMomentumChain[i_, exchanged_List, d_] :=
    Total[SpinorChain[squareSpinor[i], Mom[#], angleSpinor[d]] & /@ exchanged];
  vectorPauli[a_, b_, labels_List, c_, d_, exchanged_List] :=
    2 twoMomentumChain[c, momentumWeights[a, b, labels], exchanged, d] -
    2 Total[Function[i, scalar[a, b, DeleteCases[labels, i]] angleToReference[c]
      squareMomentumChain[i, exchanged, d]/referenceDenominator[i]] /@ labels];
  orbitalDot[a_, b_, left_List, c_, d_, right_List] :=
    4 dotMomenta[momentumWeights[a, b, left], momentumWeights[c, d, right]] -
    2 Total[Function[i, scalar[a, b, DeleteCases[left, i]]
      vectorInsertion[momentumWeights[c, d, right], i]/referenceDenominator[i]] /@ left] -
    2 Total[Function[i, scalar[c, d, DeleteCases[right, i]]
      vectorInsertion[momentumWeights[a, b, left], i]/referenceDenominator[i]] /@ right];
  sewing[a_, b_, c_, d_, left_List, right_List] := With[
    {exchanged = Join[{a, b}, left], firstScalar = scalar[a, b, left],
     secondScalar = scalar[c, d, right]},
    (angle[a, b] angle[c, d] orbitalDot[a, b, left, c, d, right] +
      angle[a, b] secondScalar vectorPauli[a, b, left, c, d, exchanged] -
      angle[c, d] firstScalar vectorPauli[c, d, right, a, b, exchanged])/
        photonPole[exchanged] -
      2 firstScalar secondScalar angle[a, c] angle[b, d]];
  amplitude = charge^2 (-Sqrt[2] charge)^photonCount/mass^2 Total[
    Function[left, With[{right = Complement[photons, left]},
      sewing[1, 2, 4, 3, left, right] - sewing[1, 3, 4, 2, left, right]]]
      /@ Subsets[photons]];
  massRules = Thread[Mass /@ Range[photonCount + 4] ->
    Join[ConstantArray[mass, 4], ConstantArray[0, photonCount]]];
  {massRules, amplitude}
];
QEDFourFermionAllPlusFormula[photonCount_, mass_, charge_, OptionsPattern[]] :=
  Failure["PhotonCount", <|"MessageTemplate" ->
    "The photon count must be a nonnegative integer."|>];
