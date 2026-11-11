(* Stable numerical external wavefunctions without a virtual-momentum shell projection. *)

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
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);
nsRoot = ConstructiveDiagrams`Private`externalShellRoot;
nsScalar = ConstructiveDiagrams`Private`externalShellScalarProduct;
nsBasis = ConstructiveDiagrams`Private`highEnergyDirectionBasis;
nsOldBasis[v_] := With[{r = Sqrt[v.v]}, If[TrueQ[r + v[[3]] == 0], {{0, -1}, {1, 0}},
  {{r + v[[3]], -v[[1]] + I v[[2]]}, {v[[1]] + I v[[2]], r + v[[3]]}}/Sqrt[2 r (r + v[[3]])]]];
nsNear[a_, b_, tolerance_:10^-13] := Module[{aa = N[Rationalize[Flatten[{a}],0],40], bb = N[Rationalize[Flatten[{b}],0],40]},
 Length[aa] === Length[bb] && And @@ MapThread[
  If[TrueQ[#2 == 0], TrueQ[Abs[#1] <= tolerance], TrueQ[Abs[(#1 - #2)/#2] <= tolerance]] &, {aa, bb}]];

check["external mass information is opt-in", ("MassRules" /. Options[ExtractEnergy]) === {}];
nsSymbolic = ExtractEnergy[SpinorChain[Spinor["Spin", "Upper", "Angle", 1, 1/2],
 Spinor["Helicity", "Angle", 2]], {{1}, {2}}];
check["default symbolic small radical is unchanged", !FreeQ[nsSymbolic, Sqrt[En[1] - MomMagnitude[1]]] &&
 FreeQ[nsSymbolic, _ConstructiveDiagrams`Private`externalShellRoot]];
nsMass = 1/2000; nsEnergy = 10^12; nsMomentum = Sqrt[nsEnergy^2 - nsMass^2];
nsSmallReference = N[Sqrt[nsEnergy - nsMomentum], 70];
nsPoint = {En[1] -> N[nsEnergy], MomMagnitude[1] -> N[nsMomentum], Theta[1] -> 0., Phi[1] -> 0.,
 En[2] -> 1., MomMagnitude[2] -> 1., Theta[2] -> 0., Phi[2] -> 0.};
nsStableSymbolic = ExtractEnergy[SpinorChain[Spinor["Spin", "Upper", "Angle", 1, 1/2],
 Spinor["Helicity", "Angle", 2]], {{1}, {2}}, "MassRules" -> {Mass[1] -> nsMass, Mass[2] -> 0}];
check["the legacy machine subtraction actually loses the small component", nsSymbolic /. nsPoint // PossibleZeroQ];
check["symbolic construction followed by numerical replacement retains the mass",
 nsNear[nsStableSymbolic /. nsPoint, Sqrt[2] nsSmallReference, 3 10^-15]];
check["the verified public example is nonzero", TrueQ[Abs[(nsStableSymbolic /. nsPoint)/(5 10^-10) - 1] < 3 10^-15]];
check["a symbolic supplied mass can also be replaced later",
 nsNear[ExtractEnergy[SpinorChain[Spinor["Spin", "Upper", "Angle", 1, 1/2], Spinor["Helicity", "Angle", 2]],
 {{1}, {2}}, "MassRules" -> {Mass[1] -> nsInputMass}] /. Join[nsPoint, {nsInputMass -> nsMass}],
 Sqrt[2] nsSmallReference, 3 10^-15]];

Do[
 nsE = e; nsM = mass; nsP = Sqrt[nsE^2 - nsM^2];
 nsReference = N[Sqrt[nsE - nsP], 70];
 check["small component at energy/mass " <> ToString[{e, mass}, InputForm],
  nsNear[nsRoot[N[nsE], N[nsP], N[nsM], -1], nsReference, 4 10^-15]],
 {e, {1, 10^6, 10^12}}, {mass, {1/2000, 1/7}}];
check["arbitrary precision recovers a component lost at the input precision",
 nsNear[nsRoot[N[nsEnergy, 25], N[nsMomentum, 25], N[nsMass, 25], -1], nsSmallReference, 10^-23]];
check["scaled positive root avoids an intermediate overflow",
 nsNear[nsRoot[10.^300, 10.^300, 1., 1], N[Sqrt[2 10^300], 70], 4 10^-15]];
check["scaled small root avoids an unnecessary mass-square underflow",
 nsNear[nsRoot[10.^200, 10.^200, 10.^-200, -1], N[10^-200/Sqrt[2 10^200], 70], 4 10^-15]];
check["rest roots retain their principal values", nsRoot[3, 0, 3, 1] === Sqrt[3] && nsRoot[3, 0, 3, -1] === Sqrt[3]];
check["signed mass preserves the original positive root", nsRoot[5, 4, -3, -1] === 1];
check["massless and zero-energy limits contain no zero-over-zero", nsRoot[3., 3., 0., -1] === 0 &&
 nsRoot[0., 0., 0., 1] === 0 && nsRoot[0., 0., 0., -1] === 0];
check["negative-energy branches remain unchanged", nsRoot[-5, 4, 3, -1] === 3 I && nsRoot[-5, 4, 3, 1] === I];
check["complex branches remain unchanged", nsRoot[3 + I, 2, Sqrt[4 + 6 I], -1] === Sqrt[1 + I]];
check["resolved off-shell data are not projected onto the supplied mass", nsRoot[5., 4., 1., -1] === 1.];
nsResolvedEnergy = N[1,60];nsResolvedMomentum = N[1-10^-20,60];
check["a tiny machine mass cannot hide resolved arbitrary-precision virtuality",
 !TrueQ[ConstructiveDiagrams`Private`externalShellNumericalQ[nsResolvedEnergy,nsResolvedMomentum,10.^-20]] &&
 nsRoot[nsResolvedEnergy,nsResolvedMomentum,10.^-20,-1] === Sqrt[nsResolvedEnergy-nsResolvedMomentum]];
check["too little momentum precision does not authorize a shell projection",
 !TrueQ[ConstructiveDiagrams`Private`externalShellNumericalQ[SetPrecision[5,1],SetPrecision[4,1],1.]]];

nsMarker = nsEndpoint;
Do[
 nsEndpointSpinor = Spinor["Spin", position, chirality, 1, spin];
 nsSource = If[side === "Left", SpinorChain[nsEndpointSpinor, nsMarker], SpinorChain[nsMarker, nsEndpointSpinor]];
 nsActual = ExtractEnergy2[nsSource, 1, N[nsEnergy], N[nsMomentum], .7, -.3, N[nsMass]];
 nsReference = N[ExtractEnergy2[nsSource, 1, nsEnergy, nsMomentum, 7/10, -3/10], 60];
 check["every explicit endpoint remains accurate " <> ToString[{position, chirality, spin, side}, InputForm],
   nsNear[First[Cases[nsActual, x_List :> x, Infinity]], First[Cases[nsReference, x_List :> x, Infinity]], 10^-13]],
 {position, {"Upper", "Lower"}}, {chirality, {"Angle", "Square"}}, {spin, {-1/2, 1/2}}, {side, {"Left", "Right"}}];
nsMatrixSource = SpinorTrace[Mom[1]];
nsStableMatrix = First[ExtractEnergy1[nsMatrixSource, 1, N[nsEnergy], N[nsMomentum], 0., 0., N[nsMass]]];
check["near-lightlike momentum diagonal is nonzero", nsNear[nsStableMatrix[[2, 2]], nsSmallReference^2, 4 10^-15]];
check["near-lightlike momentum determinant retains the supplied mass", nsNear[Quiet[Det[nsStableMatrix],Det::luc], nsMass^2, 5 10^-15]];
check["the antiparallel diagonal is equally stable",
 nsNear[First[ExtractEnergy1[nsMatrixSource, 1, N[nsEnergy], N[nsMomentum], Pi, 0., N[nsMass]]][[1,1]], nsSmallReference^2, 5 10^-15]];
Do[
 {ee,pp,mm}=nsTriple;
 check["momentum matrices preserve unresolved shell/branch " <> ToString[{ee, pp, mm}, InputForm],
 ExtractEnergy1[nsMatrixSource, 1, ee, pp, 7/10, -3/10, mm] ===
 ExtractEnergy1[nsMatrixSource, 1, ee, pp, 7/10, -3/10]],
 {nsTriple, {{5,4,1},{-5,4,3},{3+I,2,Sqrt[4+6I]}}}];

nsDot = ExtractEnergy[MomProd[1, 1], {{1}}, "MassRules" -> {Mass[1] -> nsMass}] /. nsPoint;
check["scalar self-products retain their supplied shell mass", nsDot === nsMass^2];
nsTrace = ExtractEnergy[SpinorTrace[Mom[1], Mom[1]], {{1}}, "MassRules" -> {Mass[1] -> nsMass}] /. nsPoint;
check["two-momentum traces use the same stable scalar invariant", nsTrace === 2 nsMass^2];
nsParallel = nsScalar[{10.^12,10.^12,0.,0.,1/2000},{10.^12,10.^12,0.,0.,1/2000}];
check["nearly parallel massive scalar products remain nonzero", nsNear[nsParallel, (1/2000)^2, 5 10^-15]];
nsNearlyParallelNull = nsScalar[{1.,1.,0.,0.,0},{1.,1.,10.^-10,0.,0}];
check["tiny opening-angle invariant uses a half-angle difference", nsNear[nsNearlyParallelNull, 2 Sin[10^-10/2]^2, 4 10^-15]];
check["off-shell scalar products retain their literal value", nsScalar[{5.,4.,0.,0.,1},{5.,4.,0.,0.,1}] === 9.];
check["unannotated momentum invariants are unchanged", ExtractEnergy[MomProd[1,2], {{1},{2}}] ===
 ExtractEnergy[MomProd[1,2], {{1},{2}}, "MassRules" -> {}]];
check["an unrelated mass rule cannot affect an external momentum", ExtractEnergy[MomProd[1,2], {{1},{2}}, "MassRules" -> {Mass[Multiparticle[1,2]] -> 7}] ===
 ExtractEnergy[MomProd[1,2], {{1},{2}}]];

nsTransformed = SpinToHelicitySpinors[SpinorChain[Spinor["Spin","Square",1],nsEndpoint]^2,{1,1,-1}];
nsTransformedPrepared = ExtractEnergy[nsTransformed, {{1}}, "MassRules" -> {Mass[1] -> nsMass}];
check["spin-to-helicity scalar differences use the stable external metadata",
 !FreeQ[nsTransformedPrepared, _ConstructiveDiagrams`Private`externalShellDifference] &&
 FreeQ[nsTransformedPrepared /. nsPoint, _ConstructiveDiagrams`Private`externalShellDifference] &&
 !TrueQ[PossibleZeroQ[nsTransformedPrepared /. nsPoint]]];
nsRootPrefactor = ExtractEnergy[Sqrt[En[1]-MomMagnitude[1]]/Sqrt[En[1]+MomMagnitude[1]],
 {{1}}, "MassRules" -> {Mass[1] -> nsMass}] /. nsPoint;
check["preexisting small radicals and ratios are stabilized", nsNear[nsRootPrefactor, nsMass/(2 nsEnergy), 5 10^-15]];

Do[check["exact direction basis is byte-identical " <> ToString[v,InputForm], nsBasis[v] === nsOldBasis[v]],
 {v, {{3,4,12},{3,-4,-12},{0,0,-1},{0,0,1},{0,0,0}}}];
nsComplexDirection = {1+I,2,-3};
check["complex direction basis remains unchanged", nsBasis[nsComplexDirection] === nsOldBasis[nsComplexDirection]];
Do[
 nsVector = v; nsActualBasis = nsBasis[nsVector];
 nsReferenceBasis = N[nsOldBasis[SetPrecision[nsVector, 600]], 60];
 check["inexact real direction retains its old phases " <> ToString[v,InputForm], nsNear[nsActualBasis, nsReferenceBasis, 10^-13]],
 {v, {{3.,4.,12.},{3.,-4.,-12.},{10.^-20,2 10.^-20,-1.},{10.^-200,2 10.^-200,-1.},{10.^200,2 10.^200,-10.^200}}}];
nsTinyBasis = nsBasis[{10.^-20,2 10.^-20,-1.}];
check["near-south-pole basis keeps the tiny transverse component", TrueQ[nsTinyBasis[[1,1]]>0] &&
 nsNear[ConjugateTranspose[nsTinyBasis].nsTinyBasis, IdentityMatrix[2], 10^-14]];
check["invalid mass-rule containers fail explicitly", Quiet[ExtractEnergy[1,{{1}},"MassRules" -> {1->2}],ExtractEnergy::mass] === $Failed];

Print["NUMERICAL_SPINOR_STABILITY_CHECKS=",checks,"; FAILURES=",Length[failures]];
If[failures=!={},Print[failures];Quit[1]];
Print["HEPCAT_TEST_COMPLETED:numerical-spinor-stability-tests.wl"];
Quit[0];
