(* Ordinary coefficient reductions preserve each complete inverse-factor group. *)

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
$HistoryLength = 0;
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);
spReduce = ConstructiveDiagrams`Private`preparationNumeratorSpectatorSums;
spRank = ConstructiveDiagrams`Private`preparationRank;
spBetter = ConstructiveDiagrams`Private`preparationBetterQ;
spA[i_] := Spinor[If[MemberQ[spMassless, i], "Helicity", "Spin"], "Angle", i];
spS[i_] := Spinor[If[MemberQ[spMassless, i], "Helicity", "Spin"], "Square", i];
spCurrent[a_, b_, c_, d_] := SpinorChain[spA[b], spA[d]] SpinorChain[spS[a], spS[c]] +
 SpinorChain[spA[b], spA[c]] SpinorChain[spS[a], spS[d]] +
 SpinorChain[spA[a], spA[d]] SpinorChain[spS[b], spS[c]] +
 SpinorChain[spA[a], spA[c]] SpinorChain[spS[b], spS[d]];
spFixture[a_, b_, c_, d_, k_, l_] := Module[{bilinear, difference, den},
 bilinear[j_] := SpinorChain[spS[a], spS[l]] SpinorChain[spS[j], spS[k]] -
   SpinorChain[spS[a], spS[k]] SpinorChain[spS[j], spS[l]];
 difference[j_] := SpinorChain[spS[a], spS[l]] SpinorChain[spS[k], Mom[a], spA[j]] -
   SpinorChain[spS[a], spS[k]] SpinorChain[spS[l], Mom[a], spA[j]];
 den = PropDen[Mom[Multiparticle[a, k]], spMass] PropDen[Mom[Multiparticle[a, l]], spMass]
   PropDen[Mom[Multiparticle[a, k, l]], spMass];
 {-4 spMass SpinorChain[spS[k], spS[l]] (spMass SpinorChain[spA[b], spA[d]] bilinear[c] +
   spMass SpinorChain[spA[b], spA[c]] bilinear[d] -
   SpinorChain[spS[b], spS[d]] difference[c] -
   SpinorChain[spS[b], spS[c]] difference[d])/den,
  4 spMass^2 SpinorChain[spS[k], spS[l]]^2 spCurrent[a, b, c, d]/den}];
spRules[legs_List] := (Mass[#] -> If[MemberQ[spMassless, #], 0, spMass] & /@ legs);
spOrdinaryZero[x_, rules_] := Module[{difference, component},
 difference = Factor[ReduceSpinorProducts[Expand[x]] /. rules];
 If[difference === 0, Return[True]];
 (* Independent two-component bracket arithmetic certifies any residual pure
    Schouten polynomial. It cannot certify a momentum-chain identity. *)
 ConstructiveDiagrams`Private`withProtectedPropagators[difference,
  Function[body, component = body /. SpinorChain[first_Spinor,second_Spinor] :>
   (spComponent[first,1] spComponent[second,2] - spComponent[first,2] spComponent[second,1]);
   FreeQ[component,_SpinorChain | _SpinorTrace | _Mom] && TrueQ[Expand[component] === 0]]]
];
spInverse[x_] := Sort[DeleteDuplicates[Cases[{x}, Power[base_, n_] /; !TrueQ[n >= 0], Infinity]]];

Do[
 spMassless = labels[[-2 ;;]]; spPair = spFixture @@ labels; spExternal = spRules[labels];
 spResult = spReduce[spPair[[1]], 3, spExternal];
 check["grouped Schouten and external Dirac reach compact numerator " <> ToString[labels],
   spOrdinaryZero[spResult - spPair[[2]], spExternal]];
 check["the whole expression rank strictly improves " <> ToString[labels],
   spBetter[spRank[spResult], spRank[spPair[[1]]]]];
 check["all existing inverse factors are unchanged " <> ToString[labels],
   spInverse[spResult] === spInverse[spPair[[1]]]];
 check["only ordinary external masses were applied " <> ToString[labels],
   FreeQ[Numerator[spResult], _Mom] && !FreeQ[spResult, PropDen[Mom[Multiparticle @@ labels[[{1, 5, 6}]]], spMass]]],
 {labels, {{1,2,3,4,5,6},{3,1,4,2,6,5},{9,7,2,4,11,13},{4,3,2,1,5,6}}}];

spMassless = {5,6}; spPair = spFixture[1,2,3,4,5,6]; spExternal = spRules[Range[6]];
spSource = spPair[[1]]; spTarget = spPair[[2]];
spInverseBracket = SpinorChain[spA[5],spA[6]];
Do[
 spInput = factor spSource; spResult = spReduce[spInput,3,spExternal];
 check["complex or inverse spectator remains unchanged " <> ToString[factor,InputForm],
   spOrdinaryZero[spResult-factor spTarget,spExternal] && spInverse[spInput] === spInverse[spResult]],
 {factor,{3+2I,1/spInverseBracket^2,1/(1+spInverseBracket^2),
   1/PropDen[Mom[Multiparticle[2,3]],Mass[Multiparticle[2,3]]]}}];
spInternal = Join[spExternal,{Mass[Multiparticle[2,3]]->0}];
spInput = spSource/PropDen[Mom[Multiparticle[2,3]],Mass[Multiparticle[2,3]]];
spResult = spReduce[spInput,3,spInternal];
check["supplied internal mass rules never enter the ordinary numerator pass",
 !FreeQ[spResult,PropDen[Mom[Multiparticle[2,3]],Mass[Multiparticle[2,3]]]] &&
 spInverse[spResult] === spInverse[spInput]];
spOther = PropDen[Mom[Multiparticle[1,3]],spOtherMass];
spInput = spSource + (spExtra + spExtra^2)/spOther;
spResult = spReduce[spInput,3,spExternal];
check["separate complete denominator groups remain separate",
 spOrdinaryZero[spResult - spTarget - (spExtra+spExtra^2)/spOther,spExternal] &&
 spInverse[spResult] === spInverse[spInput]];
spWrongSign = spReduce[spSource + spTarget,3,spExternal];
check["an incorrect overall sign is not erased by collection",
 spWrongSign =!= 0 && spOrdinaryZero[spWrongSign - 2 spTarget,spExternal]];
spUnequalRules = {Mass[1]->spMass,Mass[2]->spOtherMass,Mass[3]->spThirdMass,
 Mass[4]->spFourthMass,Mass[5]->0,Mass[6]->0};
spUnequal = spReduce[spSource,3,spUnequalRules];
check["spectator fermion masses need not coincide",
 spOrdinaryZero[spUnequal-spTarget,spUnequalRules]];
check["a completed compact checkpoint is never replaced by a worse grouping",
 !spBetter[spRank[spTarget],spRank[spReduce[spTarget,3,spExternal]]]];
check["the public final-compaction path uses the same ordinary reduction",
 spBetter[spRank[ConstructiveDiagrams`Private`preparationCompactExternalSums[spSource,5,spExternal]],spRank[spSource]]];
check["a zero time budget preserves the original checkpoint", spReduce[spSource,0,spExternal] === spSource];
check["a polynomial without chains is unchanged", spReduce[(spX+spY)^2,1,{}] === (spX+spY)^2];
check["noncommutative/color objects decline this scalar candidate", spReduce[spSource ColorGenerator[spAnti,spFund,spColor],1,spExternal] === spSource ColorGenerator[spAnti,spFund,spColor]];
check["large prospective expansion preserves the old checkpoint",
 spReduce[spSource (1+spX)^2048,1,spExternal] === spSource (1+spX)^2048];
check["no temporary inverse token escapes", FreeQ[spResult,s_Symbol /; StringStartsQ[SymbolName[Unevaluated[s]],"spectatorInverse"]]];
spNoRules = spReduce[spSource,3,{}];
check["unprovided external masses remain symbolic rather than being invented",
 FreeQ[spNoRules,Mass[Multiparticle[__]]] &&
 spOrdinaryZero[(spNoRules/.spExternal)-spTarget,spExternal]];
Print["Numerator spectator checks: ", checks, "; failures: ", Length[failures]];
If[failures =!= {}, Print[failures]; Exit[1]];
Print["HEPCAT_TEST_COMPLETED:numerator-spectator-tests.wl"]; Exit[0];
