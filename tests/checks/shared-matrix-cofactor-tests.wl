(* Independent arbitrary-matrix identities and bounded numerator-only proposals. *)

(*
HEPCAT - High Energy Physics Constructive Amplitude Tool

Copyright (C) 2026 Neil Christensen

This file is part of HEPCAT.

HEPCAT is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

HEPCAT is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with HEPCAT.  If not, see <https://www.gnu.org/licenses/>.
*)


root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0; checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, expression_] := (checks++; If[!TrueQ[expression], AppendTo[failures, name]; Print["FAIL: ", name]]);
smPair = ConstructiveDiagrams`Private`rspSharedDiagonalPair;
smCofactor = ConstructiveDiagrams`Private`rspLiteralFactorCofactor;
smPrepare = ConstructiveDiagrams`Private`preparationSharedMatrixCofactors;
smAdj[m_] := {{m[[2, 2]], -m[[1, 2]]}, {-m[[2, 1]], m[[1, 1]]}};
smChart = <|"Momenta" -> <|7 -> Array[smP, {2, 2}], 11 -> Array[smR, {2, 2}], 19 -> Array[smQ, {2, 2}]|>,
  "Angle" -> <|31 -> {smU1, smU2}, 37 -> {smV1, smV2}|>,
  "Square" -> <|31 -> {smW1, smW2}, 37 -> {smY1, smY2}|>,
  "AngleBasis" -> <|31 -> Array[smAB, {2, 2}], 37 -> Array[smAC, {2, 2}]|>,
  "SquareBasis" -> <|31 -> Array[smSB, {2, 2}], 37 -> Array[smSC, {2, 2}]|>|>;
smEval[value_] := Expand[value /.
  chain_SpinorChain :> ConstructiveDiagrams`Private`residueChartChain[chain, smChart] /.
  {Mass[19]^2 -> Det[smChart["Momenta"][19]], MomProd[i_Integer, j_Integer] :> Tr[smChart["Momenta"][i].smAdj[smChart["Momenta"][j]]]/2}];
smAngle = Spinor["Helicity", "Angle", 31]; smSquare = Spinor["Helicity", "Square", 37];
smLeft = SpinorChain[smAngle, Mom[7], Mom[19], smAngle];
smRight = SpinorChain[smSquare, Mom[11], Mom[19], smSquare];
smInputHash = Hash[{smLeft, smRight}, "SHA256"];
smOpenChart = <|"Momenta" -> <|19 -> Array[smOpenQ, {2, 2}]|>,
  "Angle" -> Association[Table[i -> Array[smOpenAngle[i], 2], {i, {31, 37, 43, 47}}]],
  "Square" -> Association[Table[i -> Array[smOpenSquare[i], 2], {i, {31, 37, 43, 47}}]]|>;
smOpenProofs = Table[Module[{opposite, a, b, c, d, left, right, result, difference},
  opposite = If[kind === "Angle", "Square", "Angle"];
  {a, b, c, d} = {Spinor["Spin", kind, 31], Spinor["Spin", opposite, 37],
    Spinor["Spin", kind, 43], Spinor["Spin", opposite, 47]};
  left = SpinorChain[a, Mom[19], b]; right = SpinorChain[c, Mom[19], d];
  right = If[reverse, Reverse[right], right];
  result = ConstructiveDiagrams`Private`completionSharedMomentumDeterminantPair[left, right];
  difference = left right - result["Expression"];
  difference = difference /. chain_SpinorChain :>
    ConstructiveDiagrams`Private`residueChartChain[chain, smOpenChart];
  difference = difference /. MomProd[19, 19] -> Det[smOpenChart["Momenta"][19]];
  AssociationQ[result] && Expand[difference] === 0 && result["InternalOnShellConditions"] === {}],
  {kind, {"Angle", "Square"}}, {reverse, {False, True}}];
check["shared momentum determinants hold for arbitrary four massive endpoints in both chiralities",
  And @@ Flatten[smOpenProofs]];
smResult = smPair[smLeft, smRight];
check["opposite diagonal words supply an ordinary candidate", AssociationQ[smResult]];
check["arbitrary independent matrices and endpoint spinors prove the determinant identity",
  smEval[smLeft smRight - smResult["Amplitude"]] === 0];
smReversalProofs = Flatten[Table[Module[{l = If[flipLeft, ReplacePart[smLeft, {2 -> Mom[19], 3 -> Mom[7]}], smLeft],
    r = If[flipRight, ReplacePart[smRight, {2 -> Mom[19], 3 -> Mom[11]}], smRight], result},
    result = If[swap, smPair[r, l], smPair[l, r]];
    AssociationQ[result] && smEval[l r - result["Amplitude"]] === 0],
  {flipLeft, {False, True}}, {flipRight, {False, True}}, {swap, {False, True}}]];
check["all eight word and product orientations have independently verified signs", And @@ smReversalProofs];
smVarianceProofs = Flatten[Table[Module[{a = Spinor["Spin", va, "Angle", 31, ia],
    b = Spinor["Spin", vb, "Square", 37, ib], l, r, result},
    l = SpinorChain[a, Mom[7], Mom[19], a]; r = SpinorChain[b, Mom[11], Mom[19], b];
    result = smPair[l, r]; AssociationQ[result] && smEval[l r - result["Amplitude"]] === 0 &&
      Sort[DeleteDuplicates[Cases[result["Amplitude"], _Spinor, Infinity]]] === Sort[{a, b}]],
  {va, {"Upper", "Lower"}}, {vb, {"Upper", "Lower"}}, {ia, {-1/2, 1/2}}, {ib, {-1/2, 1/2}}]];
check["all fixed Upper Lower spin components retain their typed metadata and signs", And @@ smVarianceProofs];
check["the helper does not alter its supplied chains", Hash[{smLeft, smRight}, "SHA256"] === smInputHash];
smRules = {Mass[7] -> smM7, Mass[11] -> smM11, Mass[19] -> smM19,
  Mass[31] -> 0, Mass[37] -> 0, Mass[43] -> smM43};
smMassResult = smPair[smLeft, smRight, smRules];
check["only the declared external virtuality is replaced by its own unequal mass",
  smMassResult["OriginalVirtuality"] === Mass[19]^2 &&
    smMassResult["ExternalVirtuality"] === smM19^2];
smCompositeLeft = smLeft /. Mom[19] -> Mom[Multiparticle[7, 11]];
smCompositeRight = smRight /. Mom[19] -> Mom[Multiparticle[7, 11]];
smComposite = smPair[smCompositeLeft, smCompositeRight,
  Append[smRules, Mass[Multiparticle[7, 11]] -> 0]];
check["composite momentum squares remain off shell despite an internal mass declaration",
  AssociationQ[smComposite] && !FreeQ[smComposite["ExternalVirtuality"], MomProd[7, 11]] &&
    smComposite["InternalOnShellIdentity"] === False];
check["conflicting external masses cannot license a virtuality substitution",
  MissingQ[smPair[smLeft, smRight, Append[smRules, Mass[19] -> 0]]]];
check["nonscalar and delayed mass declarations are declined",
  MissingQ[smPair[smLeft, smRight, {Mass[19] -> {smM19}}]] &&
    MissingQ[smPair[smLeft, smRight, {Mass[19] :> smM19}]]];
check["two equal matrices in a diagonal word are left to the existing reducer",
  MissingQ[smPair[smLeft /. Mom[7] -> Mom[19], smRight]]];
check["different endpoint spin components do not form a diagonal word",
  MissingQ[smPair[ReplacePart[smLeft, 4 -> Spinor["Spin", "Angle", 43]], smRight]]];
check["same chirality diagonal pairs are not rewritten", MissingQ[smPair[smLeft, smLeft]]];
smSpectator = SpinorChain[smAngle, Mom[7], smSquare];
smLiteral = smCofactor[5 smSpectator^2 smRight, smSpectator];
check["a positive repeated spectator is removed without division",
  AssociationQ[smLiteral] && smLiteral["Cofactor"] === 5 smSpectator smRight &&
    smLiteral["ExactLiteralReconstruction"] === True];
check["a nested spectator is not a literal polynomial factor",
  MissingQ[smCofactor[smFunction[smSpectator] smRight, smSpectator]]];
check["inverse spectators and inverse kinematic cofactors are declined",
  MissingQ[smCofactor[1/smSpectator, smSpectator]] &&
    MissingQ[smCofactor[smSpectator/smRight, smSpectator]]];
smIdentity = smLeft smRight - smMassResult["Amplitude"];
smPole = PropDen[Mom[Multiparticle[7, 11]], smInternalMass];
smPrepared = smPrepare[smIdentity/smPole, smRules, False, 5];
check["a bounded complete numerator proposal can expose an exact zero identity",
  smPrepared["Status"] === "IMPROVED" && smPrepared["Amplitude"] === 0 &&
    smPrepared["Route"] =!= {}];
check["accepted routes retain matrix and literal witnesses without internal shells",
  AllTrue[smPrepared["Route"], #["InternalOnShellIdentity"] === False &&
    If[#["Operation"] === "OrdinaryNumeratorScalarFactorCollection",
      #["ExactFormalReplay"] === True,
      #["ExactLiteralFactorization"] === True && #["MatrixIdentityRoute"] =!= {}] &]];
check["a zero resource budget keeps the original atomically",
  smPrepare[smIdentity/smPole, smRules, True, 0]["Amplitude"] === smIdentity/smPole];
smTiny = smPrepare[smIdentity/smPole, smRules, True, 10^-12];
check["an interrupted proposal has only an original or completely witnessed checkpoint",
  smTiny["Amplitude"] === smIdentity/smPole || smTiny["Route"] =!= {}];
check["a completed zero is idempotent", smPrepare[smPrepared["Amplitude"], smRules]["Amplitude"] === 0];
check["the helper does not edit protected inverse arguments",
  MissingQ[smPair[1/smLeft, smRight]] && smCofactor[1/(smSpectator + smRight), smSpectator] === Missing["NoLiteralPositiveFactor"]];
smA[i_] := Spinor["Spin", "Angle", i]; smS[i_] := Spinor["Spin", "Square", i];
smAA[i_, j_] := SpinorChain[smA[i], smA[j]];
smSS[i_, j_] := SpinorChain[smS[i], smS[j]];
smAS[i_, k_, j_] := SpinorChain[smA[i], Mom[k], smS[j]];
smOrdinaryCore = smSS[31, 37] (smAA[7, 11] smAS[19, 7, 43] -
  smM7 smAA[19, 11] smSS[7, 43] - smAA[7, 19] smAS[11, 7, 43]);
smExistingSpectator = smAS[31, 19, 37];
smExisting = smPrepare[smExistingSpectator smOrdinaryCore/smPole, smRules, False, 5];
check["an existing six-spinor cofactor is reduced even without a new diagonal pair",
  smExisting["Status"] === "IMPROVED" && smExisting["Amplitude"] === 0];
check["ordinary cofactor routes retain their preexisting spectator witnesses",
  AnyTrue[smExisting["Route"], Lookup[#, "CofactorRoute", {}] =!= {} &]];
check["fractional inverse kinematic cofactors are declined",
  MissingQ[smCofactor[smSpectator smRight^(-1/2), smSpectator]]];
check["unknown-exponent kinematic cofactors are declined",
  MissingQ[smCofactor[smSpectator smRight^smUnknownExponent, smSpectator]]];
smOtherInverse = SpinorChain[Spinor["Spin", "Angle", 43], Mom[19], Mom[11], Spinor["Spin", "Angle", 43]];
smFractional = smPrepare[smIdentity/(smPole Sqrt[smOtherInverse]), smRules, False, 5];
check["fractional inverse arguments remain in the fixed denominator witness",
  smFractional["Status"] === "IMPROVED" && smFractional["Amplitude"] === 0 &&
    AnyTrue[smFractional["Route"], !FreeQ[#["Denominator"], Power[smOtherInverse, -1/2]] &]];
smUnknownPower = smPrepare[smIdentity smOtherInverse^smUnknownExponent/smPole, smRules, False, 5];
check["unknown powers remain fixed outside every ordinary numerator edit",
  smUnknownPower["Status"] === "IMPROVED" && smUnknownPower["Amplitude"] === 0 &&
    AnyTrue[smUnknownPower["Route"], !FreeQ[#["Denominator"], Power[smOtherInverse, smUnknownExponent]] &]];
Print["Shared-matrix cofactor checks: ", checks, "; failures: ", Length[failures]];
If[failures =!= {}, Print[failures]; Exit[1]];
Print["HEPCAT_TEST_COMPLETED:shared-matrix-cofactor-tests.wl"]; Exit[0];
