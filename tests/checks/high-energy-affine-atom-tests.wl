(* Typed affine atoms share computation without changing correction coordinates. *)

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


root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];Get[FileNameJoin[{root,"source","HEPCAT.wl"}]];
checks=0;failures={};SetAttributes[check,HoldRest];check[name_,test_]:=(checks++;If[!TrueQ[test],AppendTo[failures,name];Print["FAIL: ",name]]);
aaTime=Unique["atomEnergy"];aaSpins=Table[Unique["atomSpin"],{3}];aaUnknowns=Table[Unique["atomCoefficient"],{20}];
aaChart=<|"PoleVariable"->aaTime,"SpinVariables"->aaSpins,"Masses"-><||>|>;
aaField=(1+aaSpins[[1]]+aaSpins[[2]]^2)(1+aaTime^3)/((1+2aaTime)aaTime^2);
aaTrial=(3+Range[20].aaUnknowns)aaField+(1+Total[aaUnknowns])aaSpins[[3]]/aaTime;
aaSaved=aaTrial;
aaReport=ConstructiveDiagrams`Private`completionHEAffineAtomLaurentRows[aaTrial,aaChart,{},aaUnknowns,0,10];
space[rows_,vars_]:=Module[{arrays,matrix},If[rows==={},Return[{}]];arrays=CoefficientArrays[rows,vars];matrix=Join[List/@Normal[First[arrays]],Normal[If[Length[arrays]===2,Last[arrays],ConstantArray[0,{Length[rows],Length[vars]}]]],2];DeleteCases[RowReduce[matrix],ConstantArray[0,Length[vars]+1]]];
aaNative=Normal[Series[aaTrial,{aaTime,0,0}]];
aaNativeRows=Flatten[Table[Last/@CoefficientRules[Coefficient[aaNative,aaTime,k],aaSpins],{k,-2,-1}]];
check["twenty coupled coordinates agree with independent native Laurent coefficients",
 AssociationQ[aaReport]&&space[aaReport["Rows"],aaUnknowns]===space[aaNativeRows,aaUnknowns]];
check["every original coordinate and constant offset is retained",
 aaReport["OriginalCoordinates"]===aaUnknowns&&aaReport["SourceParts"]===21&&aaReport["Certificate"]["ConstantPartIncluded"]];
check["shared atoms never become independent correction coefficients",
 aaReport["Certificate"]["NoIndependentAtomCorrectionParameters"]&&aaReport["AtomCount"]<Length[aaUnknowns]];
check["every typed affine part reconstructs exactly",aaReport["ExactAffineReconstruction"]&&aaReport["Certificate"]["EveryOriginalAffinePartReconstructed"]];
check["every atom retains strict jet coverage before finite coefficient extraction",
 aaReport["FullCoefficientCoverage"]&&aaReport["Certificate"]["AllAtomJetCertificatesVerified"]&&aaReport["Certificate"]["NormalOnlyAfterFullCoverage"]];
check["all atoms use the same energy chart",aaReport["Certificate"]["SameConservedEnergyChartForAllAtoms"]];
check["ordinary numeric canonicalization replays every coefficient",
 aaReport["NumericComposition"]["Certificate"]["EveryDistinctCoefficientReplayedExactly"]&&
 aaReport["Certificate"]["NumericCoefficientReplay"]["AllCoordinatesRetained"]];
check["the original input family is immutable",aaTrial===aaSaved];
aaTiny=ConstructiveDiagrams`Private`completionHEAffineAtomLaurentRows[(1+10^-100 Total[aaUnknowns])aaSpins[[1]]/aaTime,aaChart,{},aaUnknowns,0,10];
check["exact tiny coefficients are retained rather than numerically dropped",
 AssociationQ[aaTiny]&&space[aaTiny["Rows"],aaUnknowns]===space[{1+10^-100 Total[aaUnknowns]},aaUnknowns]];
aaAllowed=ConstructiveDiagrams`Private`completionHEAffineAtomLaurentRows[(1+Total[aaUnknowns])aaSpins[[1]]aaTime^2,aaChart,{},aaUnknowns,2,10];
check["the first allowed power creates no forbidden rows",AssociationQ[aaAllowed]&&aaAllowed["Rows"]==={}];
check["zero budget is an explicit fallback",!AssociationQ[ConstructiveDiagrams`Private`completionHEAffineAtomLaurentRows[aaTrial,aaChart,{},aaUnknowns,0,0]]];
check["nonlinear correction coordinates are rejected",!AssociationQ[Quiet[ConstructiveDiagrams`Private`completionHEAffineAtomLaurentRows[aaUnknowns[[1]]aaUnknowns[[2]]/aaTime,aaChart,{},aaUnknowns,0,5]]]];
check["a correction coefficient in a denominator is rejected",!AssociationQ[Quiet[ConstructiveDiagrams`Private`completionHEAffineAtomLaurentRows[1/(aaUnknowns[[1]]aaTime),aaChart,{},aaUnknowns,0,5]]]];
aaFractional=ConstructiveDiagrams`Private`completionHEAffineAtomLaurentRows[(1+Total[aaUnknowns])aaSpins[[1]]/Sqrt[aaTime],aaChart,{},aaUnknowns,0,5];
check["fractional Laurent powers are not silently discarded",!AssociationQ[aaFractional]];
aaFailure=Internal`InheritedBlock[{ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeries},Clear[ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeries];
 ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeries[___]:=<|"Status"->"UNRESOLVED","Reason"->"UnprovedSeriesZeroAssumption"|>;
 ConstructiveDiagrams`Private`completionHEAffineAtomLaurentRows[aaTrial,aaChart,{},aaUnknowns,0,5]];
check["a rejected scalar-series certificate remains a terminal failure",FailureQ[aaFailure]&&aaFailure[[1]]==="JetUncertifiedAtomSeries"];
aaLargeField=aaOpaque@@Range[30000];aaTensor=aaLargeField Times@@Table[SpinorChain[Spinor["Spin","Angle",leg],Spinor["Spin","Square",leg]],{leg,3}];
aaMeta=<|"ExternalMassRules"->{Mass[1]->1,Mass[2]->2,Mass[3]->3,Mass[4]->0}|>;
check["large degree-two tensors admit shared affine atoms without process names",
 ConstructiveDiagrams`Private`completionHEAffineAtomLaurentEligibleQ[aaTensor+Total[aaUnknowns]aaTensor,aaUnknowns,aaMeta]];
check["a small affine family keeps the established energy engine",
 !ConstructiveDiagrams`Private`completionHEAffineAtomLaurentEligibleQ[aaTensor,Take[aaUnknowns,16],aaMeta]];
check["a small expression keeps the established energy engine",
 !ConstructiveDiagrams`Private`completionHEAffineAtomLaurentEligibleQ[1+Total[aaUnknowns],aaUnknowns,aaMeta]];
aaFermionTensor=aaLargeField Times@@Table[SpinorChain[Spinor["Spin","Angle",leg],Spinor["Spin","Square",leg+3]],{leg,3}];
check["six degree-one fermion legs retain their established path",
 !ConstructiveDiagrams`Private`completionHEAffineAtomLaurentEligibleQ[aaFermionTensor,aaUnknowns,
 <|"ExternalMassRules"->Table[Mass[leg]->1,{leg,6}]|>]];
Print["CHECKS: ",checks,"; FAILURES: ",Length[failures]];If[failures=!={},Print[failures]];Print["HEPCAT_TEST_COMPLETED:high-energy-affine-atom-tests.wl"];Quit[If[failures==={},0,1]];
