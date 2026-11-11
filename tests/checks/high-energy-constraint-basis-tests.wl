(* Only homogeneous high-energy zero constraints may use an invertible,
   energy-independent external-spin basis normalization. *)

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
Get[FileNameJoin[{root, "source", "HEPCAT.wl"}]];
$HistoryLength=0; checks=0; failures={}; SetAttributes[check,HoldRest];
check[name_,assertion_] := (checks++;
  If[!TrueQ[assertion],AppendTo[failures,name];Print["FAIL: ",name]]);
hcbBasis=ConstructiveDiagrams`Private`highEnergyConstraintChartBasis;
hcbChart=ConstructiveDiagrams`Private`highEnergyAutomaticChart;
hcbRows=ConstructiveDiagrams`Private`completionHEAffineAtomLaurentRows;
hcbA[i_]:=Spinor["Spin","Angle",i]; hcbS[i_]:=Spinor["Spin","Square",i];
hcbExternal=Thread[(Mass/@Range[4])->{2,2,3,3}];
hcbMetadata=<|"ExternalLegs"->Range[4],"ExternalMassRules"->hcbExternal|>;
hcbTensor=Times[SpinorChain[hcbA[1],hcbA[2]], SpinorChain[hcbS[1],hcbS[2]],
  SpinorChain[hcbA[3],hcbA[4]], SpinorChain[hcbS[3],hcbS[4]]];
hcbFamily=(1+hcbCoefficient)hcbTensor;
hcbOriginal=hcbChart[hcbFamily,hcbMetadata,2,1729,{}]; hcbSaved=hcbOriginal;
hcbScaled=hcbBasis[hcbFamily,hcbOriginal,hcbExternal];
hcbCertificate=Lookup[hcbScaled,"ConstraintBasisRescaling",<||>];
check["a homogeneous massive spin family receives an exact constraint certificate",
  Lookup[hcbCertificate,"Status",None]==="EXACT" &&
    hcbCertificate["HomogeneousSpinorDegrees"]==={2,2,2,2}];
check["the original normalized chart is immutable", hcbOriginal===hcbSaved &&
  !KeyExistsQ[hcbOriginal,"ConstraintBasisRescaling"]];
check["normalization is confined to high-energy constraints",
  hcbCertificate["Scope"]==="HighEnergyConstraintsOnly" &&
    hcbCertificate["AbsoluteAmplitudeComparisonAllowed"]===False];
check["momentum mass reference and energy data are unchanged",
  KeyTake[hcbScaled,{"Momenta","Masses","ReferenceOverrides","EnergyScale","KinematicChecks"}]
    ===KeyTake[hcbOriginal,{"Momenta","Masses","ReferenceOverrides","EnergyScale","KinematicChecks"}]];
check["incoming spinor bases remain normalized and unchanged",
  And@@Flatten[Table[hcbScaled[key][leg]===hcbOriginal[key][leg],
    {key,{"Angle","Square","AngleBasis","SquareBasis"}},{leg,{1,2}}]]];
check["outgoing constant direction-basis identities replay exactly",
  AllTrue[hcbCertificate["BasisCertificates"],
    Map[RootReduce,#["ConstraintDirectionBasis"]-#["Scale"]#["NormalizedDirectionBasis"],{2}]
      ===ConstantArray[0,{2,2}]&]];
check["both Weyl bases and coherent vectors have exact entrywise scaling",
  AllTrue[hcbCertificate["BasisCertificates"],#["ExactSpinorBasisResiduals"]===
    {ConstantArray[0,{2,2}],ConstantArray[0,{2,2}],{0,0},{0,0}}&]];
check["all rescaling constants are provably nonzero and independent of energy",
  AllTrue[hcbCertificate["BasisCertificates"],TrueQ[#["ScaleSquared"]>0]&&
    FreeQ[#["Scale"],hcbOriginal["PoleVariable"]]&&#["Invertible"]&]];
check["normalized determinant convention is explicitly not asserted",
  hcbCertificate["PhysicalStateNormalization"]==="UnnormalizedConstraintBasis" &&
    AllTrue[hcbCertificate["BasisCertificates"],
      RootReduce[Det[#["ConstraintDirectionBasis"]]-#["ScaleSquared"]
        Det[#["NormalizedDirectionBasis"]]]===0&]];
check["every actual source spinor is covered by an exact map witness",
  hcbCertificate["WholeFamilyScalingCertificate"]["EveryRescaledSpinorMappedExactly"] &&
    AllTrue[hcbCertificate["WholeFamilyScalingCertificate"]["MappedSpinorResiduals"],#==={0,0}&]];

hcbScales=hcbCertificate["LegScales"]; hcbFactor=hcbCertificate["CommonAmplitudeFactor"];
hcbTypedScaled=hcbFamily/.chain_SpinorChain:>
  (Lookup[hcbScales,ConstructiveDiagrams`Private`spinorLeg[First[chain]],1]
    Lookup[hcbScales,ConstructiveDiagrams`Private`spinorLeg[Last[chain]],1]chain);
check["the complete affine family has one exact common multiplicative factor",
  Expand[hcbTypedScaled-hcbFactor hcbFamily]===0 &&
    hcbCertificate["WholeFamilyScalingCertificate"]["AllAdditiveBranchesHomogeneous"]];
check["the common factor cannot alter high-energy powers",
  ConstructiveDiagrams`Private`highEnergyZeroState[hcbFactor]==="Nonzero" &&
    FreeQ[hcbFactor,hcbOriginal["PoleVariable"]] &&
    hcbCertificate["WholeFamilyScalingCertificate"]["NoInternalOnShellConditions"]];
hcbOldRows=hcbRows[hcbFamily,hcbOriginal,{}, {hcbCoefficient},0,8];
hcbNewRows=hcbRows[hcbFamily,hcbScaled,{}, {hcbCoefficient},0,8];
check["both normalized and constraint charts retain complete coefficient coverage",
  AssociationQ[hcbOldRows]&&AssociationQ[hcbNewRows]&&
    hcbOldRows["FullCoefficientCoverage"]&&hcbNewRows["FullCoefficientCoverage"]];
check["all coherent high-energy rows differ only by the certified factor",
  hcbOldRows["Rows"]=!={} && hcbOldRows["RowKeys"]===hcbNewRows["RowKeys"]&&
    AllTrue[RootReduce[Flatten[Normal/@CoefficientArrays[
      Expand[hcbNewRows["Rows"]-hcbFactor hcbOldRows["Rows"]],{hcbCoefficient}]]],#===0&]];
check["no independent correction coefficient or spin component is removed",
  hcbNewRows["OriginalCoordinates"]==={hcbCoefficient}&&
    hcbNewRows["Certificate"]["AllOriginalCoordinatesRetained"]];

check["nonhomogeneous amplitudes retain the original normalized chart",
  hcbBasis[hcbFamily+1,hcbOriginal,hcbExternal]===hcbOriginal];
check["unexpanded reference factors retain the original normalized chart",
  hcbBasis[hcbFamily xFactor[3],hcbOriginal,hcbExternal]===hcbOriginal];
check["explicit spin epsilons retain the original normalized chart",
  hcbBasis[hcbFamily SpinorEps[1,2],hcbOriginal,hcbExternal]===hcbOriginal];
check["unsupported nonlinear spinor functions retain the original chart",
  hcbBasis[Sin[hcbTensor],hcbOriginal,hcbExternal]===hcbOriginal];
hcbBad=hcbOriginal; AssociateTo[hcbBad,"OutgoingSpatialDirections"->
  ReplacePart[hcbBad["OutgoingSpatialDirections"],1->{0,0,-1}]];
check["a singular direction normalization is declined unchanged",
  hcbBasis[hcbFamily,hcbBad,hcbExternal]===hcbBad];
hcbBad=hcbOriginal; AssociateTo[hcbBad,"OutgoingSpatialDirections"->
  ReplacePart[hcbBad["OutgoingSpatialDirections"],1->{0.5,1,2}]];
check["approximate angular directions are declined unchanged",
  hcbBasis[hcbFamily,hcbBad,hcbExternal]===hcbBad];
hcbBad=hcbOriginal; AssociateTo[hcbBad["AngleBasis"],3->hcbBad["AngleBasis"][3]+IdentityMatrix[2]];
check["an unproved entrywise basis identity cannot be admitted",
  hcbBasis[hcbFamily,hcbBad,hcbExternal]===hcbBad];
check["incomplete chart metadata is declined unchanged",
  With[{bad=KeyDrop[hcbOriginal,"Momenta"]},hcbBasis[hcbFamily,bad,hcbExternal]===bad]];

(* A massless outgoing leg keeps its existing rational null factorization;
   only the separate massive outgoing solution is eligible for rescaling. *)
hcbMixedRules=Thread[(Mass/@Range[4])->{2,2,0,3}];
hcbMixedMeta=<|"ExternalLegs"->Range[4],"ExternalMassRules"->hcbMixedRules|>;
hcbMixed=Times[SpinorChain[hcbA[1],hcbA[2]], SpinorChain[hcbS[1],hcbS[2]],
  SpinorChain[Spinor["Helicity","Angle",3],hcbA[4]]^2];
hcbMixedChart=hcbChart[hcbMixed,hcbMixedMeta,2,1729,{}];
hcbMixedScaled=hcbBasis[hcbMixed,hcbMixedChart,hcbMixedRules];
check["a homogeneous mixed massive-massless chart rescales only the massive leg",
  KeyExistsQ[hcbMixedScaled,"ConstraintBasisRescaling"]&&
    Keys[hcbMixedScaled["ConstraintBasisRescaling"]["LegScales"]]==={4}];
check["massless external spinor normalization is unchanged",
  hcbMixedScaled["Angle"][3]===hcbMixedChart["Angle"][3]&&
    hcbMixedScaled["Square"][3]===hcbMixedChart["Square"][3]];
check["ordinary chart construction remains physically normalized by default",
  !KeyExistsQ[hcbChart[hcbFamily,hcbMetadata,3,1729,{}],"ConstraintBasisRescaling"]];

Print["CHECKS: ",checks,"; FAILURES: ",Length[failures]];
If[failures=!={},Print[failures]];
Print["HEPCAT_TEST_COMPLETED:high-energy-constraint-basis-tests.wl"];
Quit[If[failures==={},0,1]];
