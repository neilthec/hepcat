(* Numerator momentum degree treats p.q and s_ij as two insertions. The
   degree measures numerator structure, independently of denominator choice,
   external labels, spin-index metadata and existing composite branch costs. *)
nmcRoot = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{nmcRoot, "source", "HEPCAT.wl"}]];
$HistoryLength = 0; nmcChecks = 0; nmcFailures = {};
SetAttributes[nmcCheck, HoldRest];
nmcCheck[name_, assertion_] := (nmcChecks++; If[!TrueQ[assertion],
  AppendTo[nmcFailures, name]; Print["FAIL: ", name]]);
nmcDegree[expression_] := ConstructiveDiagrams`Private`numeratorMomentumDegree[expression];
nmcAngle[leg_] := Spinor["Spin", "Angle", leg];
nmcSquare[leg_] := Spinor["Spin", "Square", leg];
nmcBracket = SpinorChain[nmcAngle[1], nmcAngle[3]];
nmcOne = SpinorChain[nmcAngle[1], Mom[2], nmcSquare[3]];
nmcTwo = SpinorChain[nmcAngle[1], Mom[2], Mom[4], nmcAngle[3]];
nmcTrace = SpinorTrace[Mom[1], Mom[2], Mom[3], Mom[4]];
nmcDot = MomProd[1, 2];
nmcMandel = Mandelstahm[1, 2];
nmcProp = PropDen[Mom[Multiparticle[1, 2]], nmcMass, nmcWidth];
nmcDegreeCases = {
  {"scalar constants", 7 nmcCoefficient, 0},
  {"a bare bracket", nmcBracket, 0},
  {"one chain insertion", nmcOne, 1},
  {"two chain insertions", nmcTwo, 2},
  {"a momentum product", nmcDot, 2},
  {"a generalized Mandelstam invariant", Mandelstahm[1, 2, 4], 2},
  {"a scalar vector square", Mom[2]^2, 2},
  {"a composite vector square", Mom[Multiparticle[1, 2, 4]]^2, 2},
  {"a bare vector", Mom[2], 1},
  {"a positive chain power", nmcOne^3, 3},
  {"a positive momentum-product power", nmcDot^3, 6},
  {"a positive invariant power", nmcMandel^2, 4},
  {"a product of independent momentum factors", nmcOne nmcDot nmcTwo, 5},
  {"a polynomial takes its highest numerator degree", nmcOne + nmcDot^2, 4},
  {"a powered polynomial multiplies its highest degree", (nmcOne + nmcDot)^3, 6},
  {"a four-slot chiral trace", nmcTrace, 4},
  {"repeated traces count multiplicity", nmcTrace^2, 8},
  {"a two-slot trace counts two insertions", SpinorTrace[Mom[1], Mom[2]], 2},
  {"propagator data stay opaque", nmcProp, 0},
  {"inverse propagators stay outside the numerator", nmcProp^-2, 0},
  {"an inverse momentum product", nmcDot^-3, 0},
  {"an inverse invariant", nmcMandel^-2, 0},
  {"an inverse inserted chain", nmcTwo^-1, 0},
  {"an inverse trace", nmcTrace^-1, 0},
  {"an inverse kinematic polynomial", (nmcDot + nmcOne + nmcMass^2)^-2, 0},
  {"mixed numerator and denominator", nmcOne nmcDot/nmcProp/nmcMandel^2/nmcTwo, 3},
  {"an unrelated scalar function is opaque", nmcScalarFunction[nmcDot, Mom[3]], 0},
  {"spin-index metadata is opaque", SpinorChain[
    Spinor["Spin", "Upper", "Angle", 1, nmcIndex[Mom[2], MomProd[2, 3]]],
    nmcAngle[3]], 0}
};
Do[nmcCheck["numerator degree: " <> entry[[1]],
  nmcDegree[entry[[2]]] === entry[[3]]], {entry, nmcDegreeCases}];

nmcScoreCases = {
  {"the established one-insertion score", nmcOne, 5},
  {"the established two-insertion score", nmcTwo, 9},
  {"one momentum product costs two insertions", nmcDot, 9},
  {"a squared momentum product costs four insertions", nmcDot^2, 17},
  {"a Mandelstam invariant costs two insertions", nmcMandel, 9},
  {"a squared invariant costs four insertions", nmcMandel^2, 17},
  {"a scalar vector square costs two insertions", Mom[2]^2, 9},
  {"a composite scalar square is one invariant", Mom[Multiparticle[1, 2]]^2, 9},
  {"trace insertions are counted", nmcTrace, 17},
  {"powered trace insertions are counted", nmcTrace^2, 33},
  {"dot and chain numerator work is additive", nmcDot nmcOne, 13},
  {"inverse spinor factors do not masquerade as numerator work", 1/nmcTwo, 1},
  {"inverse invariants do not masquerade as numerator work", 1/nmcMandel^2, 1},
  {"inverse scalar products do not masquerade as numerator work", 1/nmcDot^2, 1},
  {"kinematic denominator sums remain opaque", 1/(nmcDot + nmcOne), 1},
  {"width-bearing denominators remain opaque", nmcOne/nmcProp, 5},
  {"existing one-slot composite branching is retained", SpinorChain[
    nmcAngle[1], Mom[Multiparticle[2, 3]], nmcSquare[4]], 10},
  {"composite branching multiplies the full numerator cost", nmcDot SpinorChain[
    nmcAngle[1], Mom[Multiparticle[2, 3]], nmcSquare[4]], 26},
  {"two composite slots multiply branch counts", SpinorChain[nmcAngle[1],
    Mom[Multiparticle[2, 3]], Mom[Multiparticle[2, 3, 4]], nmcAngle[4]], 54},
  {"powered composite chains retain branch multiplicity", SpinorChain[
    nmcAngle[1], Mom[Multiparticle[2, 3]], nmcSquare[4]]^2, 36},
  {"composite trace branching is retained", SpinorTrace[
    Mom[Multiparticle[1, 2]], Mom[3]], 18}
};
Do[nmcCheck["weighted complexity: " <> entry[[1]],
  SpinorExpressionComplexity[entry[[2]]] === entry[[3]]], {entry, nmcScoreCases}];

nmcMassRules = {Mass[1] -> nmcM1, Mass[2] -> nmcM2};
nmcInvariantDefinition = Mandelstahm[1, 2] ->
  2 MomProd[1, 2] + Mass[1]^2 + Mass[2]^2;
nmcInvariantNumerator = Mandelstahm[1, 2] - Mass[1]^2 - Mass[2]^2;
nmcCheck["the invariant rewrite is exact with arbitrary external masses",
  Expand[(nmcInvariantNumerator - 2 nmcDot) /. nmcInvariantDefinition /. nmcMassRules] === 0];
nmcCheck["both sides of the massive invariant identity retain degree two",
  nmcDegree[nmcInvariantNumerator /. nmcMassRules] === 2 &&
    nmcDegree[2 nmcDot] === 2];
nmcCheck["explicit mass terms add terms, not hidden momentum insertions",
  SpinorExpressionComplexity[nmcInvariantNumerator /. nmcMassRules] ===
    SpinorExpressionComplexity[2 nmcDot] + 2];
nmcMasslessRules = {Mass[1] -> 0, Mass[2] -> 0};
nmcCheck["the massless invariant identity has the same two-insertion score",
  SpinorExpressionComplexity[nmcInvariantNumerator /. nmcMasslessRules] ===
    SpinorExpressionComplexity[2 nmcDot] === 9];
nmcCheck["complexity is independent of numerical coefficients and typed leg labels",
  SpinorExpressionComplexity[-37 MomProd[17, 29] SpinorChain[
    nmcAngle[17], Mom[31], nmcSquare[29]]/PropDen[Mom[Multiparticle[17, 31]], nmcMass]] ===
    SpinorExpressionComplexity[nmcDot nmcOne]];
nmcCheck["the zero expression has zero complexity", SpinorExpressionComplexity[0] === 0];
Print[nmcChecks, " numerator-momentum-complexity checks; ", Length[nmcFailures], " failures."];
If[nmcFailures =!= {}, Print[nmcFailures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:numerator-momentum-complexity-tests.wl"];
Quit[0];
