(* Research formula, not a special case in ConstructAmplitude.
   All external momenta are ingoing. Legs 1,2 are equal-mass fermions;
   photonCount photons occupy legs 3,...,photonCount+2. Only the specified
   negative-helicity photon is negative. No internal shell is assumed.
   Get["~/code/hepcat/source/HEPCAT.wl"] before loading this file. *)

ClearAll[QEDOneMinusFormula];
Options[QEDOneMinusFormula] = {"NegativePhoton" -> Automatic,
  "SquareReference" -> Automatic, "ReturnCoefficients" -> False};
QEDOneMinusFormula[photonCount_Integer /; photonCount >= 2,
    mass_, charge_, OptionsPattern[]] := Module[
  {negative = Replace[OptionValue["NegativePhoton"], Automatic -> photonCount + 2],
   positive, reference = OptionValue["SquareReference"], angle, square,
   insertion, pole, current, subsets, weight, orbital, spin, ward, amplitude,
   massRules},
  If[!MemberQ[Range[3, photonCount + 2], negative],
    Return[Failure["NegativePhoton", <|"MessageTemplate" ->
      "NegativePhoton must name one of the external photon labels."|>]]];
  If[TrueQ[mass == 0], Return[Failure["MasslessFermion", <|"MessageTemplate" ->
    "This representation uses the massive Dirac equation divided by the fermion mass."|>]]];
  positive = DeleteCases[Range[3, photonCount + 2], negative];
  reference = Replace[reference, Automatic -> First[positive]];
  If[reference === negative,
    Return[Failure["SquareReference", <|"MessageTemplate" ->
      "The negative photon cannot be its own square reference."|>]]];
  If[!MemberQ[positive, reference] &&
      !MatchQ[reference, Spinor[_, "Square", __]],
    Return[Failure["SquareReference", <|"MessageTemplate" ->
      "Use a positive photon label or an explicit square spinor as the reference."|>]]];
  angle[i_] := Spinor[If[MemberQ[{1, 2}, i], "Spin", "Helicity"], "Angle", i];
  square[i_] := If[Head[i] === Spinor, i,
    Spinor[If[MemberQ[{1, 2}, i], "Spin", "Helicity"], "Square", i]];
  insertion[p_, j_] := SpinorChain[angle[negative], Mom[p], square[j]];
  pole[endpoint_, labels_List] := PropDen[
    Mom[Multiparticle @@ Sort[Prepend[labels, endpoint]]], mass];
  current[endpoint_, {}] = 1;
  current[endpoint_, labels_List] := current[endpoint, labels] =
    Total[Function[i, With[{previous = DeleteCases[labels, i]},
      current[endpoint, previous] (insertion[endpoint, i] +
        Total[insertion[#, i] & /@ previous])/
        SpinorChain[angle[negative], angle[i]]]] /@ labels]/pole[endpoint, labels];
  subsets = Subsets[positive];
  weight[left_List] := With[{right = Complement[positive, left]},
    (-1)^Length[right] current[1, left] current[2, right]];
  spin = Total[weight /@ subsets];
  orbital = Total[Function[left, weight[left]
      (insertion[1, reference] + Total[insertion[#, reference] & /@ left])/
      SpinorChain[square[negative], square[reference]]] /@ subsets];
  ward = Total[Function[left, weight[left]
      (insertion[1, negative] + Total[insertion[#, negative] & /@ left])] /@ subsets];
  amplitude = (-Sqrt[2] charge)^photonCount/mass (
    SpinorChain[angle[1], angle[2]] orbital +
    SpinorChain[angle[1], angle[negative]] SpinorChain[angle[2], angle[negative]] spin);
  massRules = Thread[Mass /@ Range[photonCount + 2] ->
    Join[{mass, mass}, ConstantArray[0, photonCount]]];
  If[TrueQ[OptionValue["ReturnCoefficients"]],
    <|"Amplitude" -> {massRules, amplitude}, "OrbitalCoefficient" -> orbital,
      "SpinCoefficient" -> spin, "WardIdentity" -> ward,
      "NegativePhoton" -> negative, "PositivePhotons" -> positive,
      "SquareReference" -> reference, "PartitionCount" -> Length[subsets]|>,
    {massRules, amplitude}]
];
