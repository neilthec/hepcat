(* Ordinary pair-virtuality cancellation before simple-pole source admission. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0; checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, expression_] := (checks++; If[!TrueQ[expression], AppendTo[failures, name]; Print["FAIL: ", name]]);
opNormalize = ConstructiveDiagrams`Private`completionOrdinaryPairPoleSource;
opRules = {Mass[7] -> opMass, Mass[11] -> 0, Mass[19] -> opOtherMass, Mass[23] -> 0};
opD = PropDen[Mom[Multiparticle[7, 11]], opMass];
opQ = PropDen[Mom[Multiparticle[19, 23]], opOtherMass];
opDot = MomProd[7, 11];
opSource = 2 opDot/opD^2;
opHash = Hash[opSource, "SHA256"];
opResult = opNormalize[opSource, opRules, 2];
check["an ordinary external pair definition exposes a simple physical pole",
  opResult["Status"] === "REDUCED" && opResult["Expression"] === 1/opD];
check["the identity is explicitly off shell",
  opResult["InternalOnShellConditions"] === {} && opResult["ExactForwardIdentity"] === True &&
    MemberQ[opResult["Rules"], opDot -> opD/2]];
check["the retained inverse definition exactly replays the original rational function",
  Cancel[(opSource - opResult["Expression"]) /. opResult["InverseDefinitionRules"]] === 0];
check["the source was not mutated", Hash[opSource, "SHA256"] === opHash];
check["a full polynomial quotient is retained, including a nonzero regular term",
  opNormalize[4 opDot^2/opD^2 + opSource, opRules, 2]["Expression"] === 1 + 1/opD];
check["uncanceled positive virtualities remain visible as numerator momentum products",
  opNormalize[8 opDot^3/opD^2, opRules, 2]["Expression"] === 2 opDot];
check["a genuine double pole is never relabeled as simple",
  opNormalize[(2 opDot + opRemainder)/opD^2, opRules, 2]["Expression"] ===
    (2 opDot + opRemainder)/opD^2];
check["two independent unequal massive legs use their own external pair definitions",
  opNormalize[4 opDot MomProd[19, 23]/(opD^2 opQ^2), opRules, 2]["Expression"] === 1/(opD opQ)];
check["inverse spectator factors remain unchanged",
  opNormalize[opSource/opSpectator, opRules, 2]["Expression"] === 1/(opD opSpectator)];
opHiddenInverse = (1 + MomProd[7, 11])^(-opPower);
check["unknown inverse spectator exponents protect their momentum products",
  opNormalize[opSource opHiddenInverse, opRules, 2]["Expression"] === opHiddenInverse/opD];
opFractionalInverse = (1 + MomProd[7, 11])^(-1/2);
check["fractional inverse spectators retain the same branch and argument",
  opNormalize[opSource opFractionalInverse, opRules, 2]["Expression"] === opFractionalInverse/opD];
check["reversed scalar-product order has the identical ordinary definition",
  opNormalize[2 MomProd[11, 7]/opD^2, opRules, 2]["Expression"] === 1/opD];
check["a different propagator mass cannot license cancellation",
  opNormalize[opSource /. opD -> PropDen[Mom[Multiparticle[7, 11]], opWrongMass], opRules, 2]["Status"] === "UNCHANGED"];
check["conflicting external masses are declined",
  opNormalize[opSource, Append[opRules, Mass[7] -> opWrongMass], 2]["Status"] === "UNCHANGED"];
check["nonscalar external mass declarations cannot justify cancellation",
  opNormalize[opSource, ReplacePart[opRules, 1 -> (Mass[7] -> {opMass})], 2]["Status"] === "UNCHANGED"];
check["an internal shell declaration cannot provide a missing external mass",
  opNormalize[opSource, {Mass[7] -> opMass, Mass[Multiparticle[7, 11]] -> opMass}, 2]["Status"] === "UNCHANGED"];
check["approximate mass information cannot certify an exact cancellation",
  opNormalize[opSource /. opMass -> 1., opRules /. opMass -> 1., 2]["Status"] === "UNCHANGED"];
check["zero and negative budgets preserve the original checkpoint",
  opNormalize[opSource, opRules, 0]["Expression"] === opSource &&
    opNormalize[opSource, opRules, -1]["Expression"] === opSource];
opTargets = {<|"Cut" -> {7, 11}, "Mass" -> opMass, "Propagator" -> opD,
  "ExternalLegs" -> {7, 11, 19, 23}|>};
check["the original higher-pole guard is retained for genuinely singular sources",
  ConstructiveDiagrams`Private`completionHigherPhysicalPoles[1/opD^2, opTargets, {7, 11, 19, 23}] =!= {}];
opSimple = ConstructiveDiagrams`Private`completionExplicitSimplePoleBoundQ;
check["a sum of simple poles is certified without combining its fractions",
  opSimple[1/opT + opCoefficient/opT, {opT, opU}] &&
    opSimple[1/opT - 1/opT, {opT, opU}]];
check["products in different physical poles retain a simple order in each",
  opSimple[1/(opT opU), {opT, opU}] &&
    opSimple[opCoefficient (opT + 2)/(opT opU), {opT, opU}]];
check["positive powers of a rational sum retain their full upper-bound multiplicity",
  !opSimple[(1/opT + 1/opU)^2, {opT, opU}]];
check["a tracked-dependent inverse sum stays on the exact rational fallback",
  !opSimple[1/(opT + opU)^2, {opT, opU}]];
check["fractional and symbolic tracked powers are never certified as simple",
  !opSimple[opT^(-1/2), {opT}] && !opSimple[opT^opExponent, {opT}]];
check["unknown functions of a physical token stay on the exact fallback",
  !opSimple[opFunction[opT], {opT}] && !opSimple[1/opFunction[opT], {opT}]];
check["unsupported explicit-bound cases still retain the old exact cancellation",
  ConstructiveDiagrams`Private`completionHigherPhysicalPoles[
    (opD + 2 opDot)/(opD^2 + 2 opD opDot), opTargets, {7, 11, 19, 23}] === {}];
check["canonical complementary cuts cannot conceal a product double pole",
  ConstructiveDiagrams`Private`completionHigherPhysicalPoles[
    1/(opD PropDen[Mom[Multiparticle[19, 23]], opMass]), opTargets, {7, 11, 19, 23}] =!= {}];
Print["Ordinary pair-pole tests: ", checks - Length[failures], "/", checks, " passed."];
If[failures =!= {}, Print[failures]];
Print["HEPCAT_TEST_COMPLETED:ordinary-pair-pole-tests.wl"];
Exit[If[failures === {}, 0, 1]];
