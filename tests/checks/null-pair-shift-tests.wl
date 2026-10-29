(* Independent external-shell checks of the typed two-null-leg deformation. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value],
  AppendTo[failures, name]; Print["FAIL: ", name]]);
nullHelicity[kind_, leg_] := Spinor["Helicity", kind, leg];
nullExpandWidth[expression_] := expression /.
  PropDen[momentum_, mass_, width_] :> momentum^2 - mass^2 + I mass width;

Do[
  {multiplicity, masses, pair} = fixture;
  external = Range[multiplicity]; {squareLeg, angleLeg} = pair;
  externalRules = Thread[(Mass /@ external) -> masses];
  channel = <|"ExternalLegs" -> external, "ExternalMassRules" -> externalRules,
    "Cut" -> {1, 2}, "Complement" -> Drop[external, 2], "Mass" -> 1,
    "Propagator" -> PropDen[Mom[Multiparticle[1, 2]], 1]|>;
  generated = ConstructiveDiagrams`Private`residueAutomaticChart[
    channel, 2, 991171 + multiplicity, {}];
  base = generated["Chart"] /. generated["Chart"]["PoleVariable"] -> 13;
  base = ConstructiveDiagrams`Private`completionHECoherentChart[base];
  parameter = Unique["nullPairReview"];
  base = Join[base, <|"PoleVariable" -> parameter, "AllowCutSpinors" -> False|>];

  (* Construct the comparison matrices directly, independently of the operator. *)
  angle = base["Angle"]; square = base["Square"]; momenta = base["Momenta"];
  rankOne = Outer[Times, angle[squareLeg], square[angleLeg]];
  AssociateTo[square, squareLeg -> (square[squareLeg] + parameter square[angleLeg])];
  AssociateTo[angle, angleLeg -> (angle[angleLeg] - parameter angle[squareLeg])];
  AssociateTo[momenta, {squareLeg -> (momenta[squareLeg] + parameter rankOne),
    angleLeg -> (momenta[angleLeg] - parameter rankOne)}];
  shiftedChart = Join[base, <|"Angle" -> angle, "Square" -> square, "Momenta" -> momenta|>];
  label = " at multiplicity " <> ToString[multiplicity];
  check["the independently shifted momenta conserve their total" <> label,
    ConstructiveDiagrams`Private`residueExactZero[Total[Values[momenta]]]];
  check["every external mass shell is preserved" <> label,
    AllTrue[external, ConstructiveDiagrams`Private`residueExactZero[
      Det[momenta[#]] - masses[[#]]^2] &]];
  check["both shifted null momenta retain their spinor wavefunctions" <> label,
    AllTrue[pair, ConstructiveDiagrams`Private`residueExactZero[
      Outer[Times, angle[#], square[#]] - momenta[#]] &]];

  massive = Select[external, masses[[#]] =!= 0 &];
  expressions = {
    PropDen[Mom[squareLeg] + 2 Mom[angleLeg] - Mom[1], 3],
    PropDen[3 (Mom[Multiparticle[squareLeg, 1]] - 2 Mom[angleLeg]), 3],
    PropDen[Mom[Multiparticle[squareLeg, 1]], 3, 1/11],
    MomProd[squareLeg, 1],
    MomProd[Multiparticle[squareLeg, 1], Multiparticle[angleLeg, 1]],
    Mandelstahm[squareLeg, 1], Mandelstahm[squareLeg, angleLeg],
    Mom[Multiparticle[squareLeg, 1]]^2, Mom[Multiparticle[squareLeg, 1]]^4,
    SpinorChain[nullHelicity["Angle", angleLeg], Mom[squareLeg], Mom[1], Mom[angleLeg],
      nullHelicity["Square", squareLeg]],
    SpinorChain[nullHelicity["Square", squareLeg], Mom[squareLeg], Mom[angleLeg], Mom[1],
      Mom[squareLeg], Mom[2], nullHelicity["Angle", angleLeg]],
    SpinorChain[Spinor["Spin", "Upper", "Angle", First[massive], -1/2],
      Mom[squareLeg] + Mom[1], Mom[angleLeg],
      Spinor["Spin", "Lower", "Angle", Last[massive], 1/2]],
    SpinorChain[Spinor["Xi", "Angle", 17], nullHelicity["Angle", angleLeg]],
    SpinorTrace[Mom[squareLeg], Mom[1]],
    (1 + MomProd[squareLeg, 1])^2/(3 + PropDen[Mom[Multiparticle[angleLeg, 1]], 2])};
  Do[
    shiftedExpression = ConstructiveDiagrams`Private`completionNullPairShift[
      expressions[[index]], parameter, pair, external];
    calculated = If[FailureQ[shiftedExpression], shiftedExpression,
      ConstructiveDiagrams`Private`residueEvaluateChart[
        nullExpandWidth[shiftedExpression], base, {}]];
    expected = ConstructiveDiagrams`Private`residueEvaluateChart[
      nullExpandWidth[expressions[[index]]], shiftedChart, {}];
    check["the typed shift equals the independent matrix deformation" <> label <>
      " fixture " <> ToString[index], !FailureQ[calculated] &&
        ConstructiveDiagrams`Private`residueExactZero[calculated - expected]],
    {index, Length[expressions]}];

  otherNull = First[Complement[Select[external, masses[[#]] === 0 &], pair]];
  Do[
    dirac = SpinorChain[nullHelicity[kind, leg], Mom[leg],
      nullHelicity[If[kind === "Angle", "Square", "Angle"], otherNull]];
    shiftedDirac = ConstructiveDiagrams`Private`completionNullPairShift[
      dirac, parameter, pair, external];
    check["the shifted null Dirac equation holds" <> label <> " " <> ToString[{leg, kind}],
      !FailureQ[shiftedDirac] && ConstructiveDiagrams`Private`residueExactZero[
        ConstructiveDiagrams`Private`residueEvaluateChart[shiftedDirac, base, {}]]],
    {leg, pair}, {kind, {"Angle", "Square"}}];

  unsupported = {
    SpinorChain[Spinor["Spin", "Angle", angleLeg], nullHelicity["Angle", squareLeg]],
    SpinorChain[Spinor["Helicity", "Angle", angleLeg, 99], nullHelicity["Angle", squareLeg]],
    SpinorChain[Spinor["Unknown", "Angle", First[Complement[external, pair]]],
      nullHelicity["Angle", squareLeg]],
    SpinorChain[Spinor["Spin", "Upper", "Angle", Multiparticle[1, squareLeg], -1/2],
      nullHelicity["Angle", squareLeg]],
    PropDen[Mom[squareLeg], MomProd[1, angleLeg]],
    SpinorTrace[Mom[squareLeg], Mom[1], Mom[angleLeg], Mom[2]],
    PropDen[Mom[squareLeg], MomHat[angleLeg]],
    PropDen[Mom[squareLeg], MomQ[angleLeg]],
    PropDen[Mom[squareLeg], SpinorHat["Helicity", "Angle", angleLeg]],
    PropDen[MomHat[angleLeg] Mom[squareLeg], 1],
    PropDen[MomQ[angleLeg] Mom[squareLeg], 1],
    PropDen[SpinorHat["Helicity", "Angle", angleLeg] Mom[squareLeg], 1],
    PropDen[Mom[angleLeg] + MomHat[angleLeg] Mom[squareLeg], 1],
    PropDen[Mom[squareLeg] + 7, 1],
    PropDen[Mom[squareLeg] + Mom[1] Mom[angleLeg], 1],
    SpinorChain[nullHelicity["Angle", angleLeg],
      Mom[squareLeg] + MomQ[angleLeg] Mom[1], nullHelicity["Square", squareLeg]]};
  Do[check["unsupported shifted structures fail explicitly" <> label <> " fixture " <> ToString[index],
    FailureQ[ConstructiveDiagrams`Private`completionNullPairShift[
      unsupported[[index]], parameter, pair, external]]], {index, Length[unsupported]}];
  normalized = <|"ExternalMassRules" -> externalRules, "Targets" -> {}|>;
  choices = ConstructiveDiagrams`Private`completionNullPairChoices[normalized];
  check["continuation choices use only distinct declared null legs" <> label,
    choices =!= {} && AllTrue[choices, DuplicateFreeQ[#] &&
      AllTrue[#, (Mass[#] /. externalRules) === 0 &] &]];
  check["a massive external leg cannot be used as a null continuation" <> label,
    ConstructiveDiagrams`Private`completionNullPairChannels[normalized,
      {First[massive], First[pair]}, {}, 991179, 64, 1]["Status"] === "UNAVAILABLE"],
  {fixture, {{5, {2, 0, 0, 2, 0}, {5, 2}},
    {8, {2, 0, 3, 0, 0, 0, 0, 0}, {8, 2}}}}];

check["the complete independent deformation regression inventory ran", checks === 80];
Print[checks, " null-pair deformation checks; ", Length[failures], " failures."];
If[failures =!= {}, Print[failures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:null-pair-shift-tests.wl"];
Quit[0];
