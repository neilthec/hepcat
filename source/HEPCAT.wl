(* ::Package:: *)

(* Load HEPCAT through this entry point with Get[".../source/HEPCAT.wl"].
   It checks the required files, loads the algebra and model implementations,
   then registers the public functions and the Mathematica Help pages.
   Supporting implementation modules live in source/internal. *)

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



(* ::Subsection:: *)
(*Begin Package*)


Module[{sourceDirectory = If[ValueQ[Global`$HEPCATpath], Global`$HEPCATpath,
    DirectoryName[ExpandFileName[$InputFileName]]], sourceFiles, missingFiles},
sourceFiles = If[StringQ[sourceDirectory],
  FileNameJoin[{sourceDirectory, #}] & /@ {"HEPCAT-Base.wl", "internal/HEPCAT-Parameters.wl", "HEPCAT-Model.wl",
    "internal/HEPCAT-Feynman.wl", "internal/HEPCAT-Color.wl", "internal/HEPCAT-SPINAS.wl",
    "internal/HEPCAT-MadGraph.wl", "internal/HEPCAT-CovariantSources.wl",
    "internal/HEPCAT-CovariantContacts.wl", "internal/HEPCAT-ColoredCurrents.wl",
    "internal/HEPCAT-QCDCurrents.wl"}, {}];
missingFiles = Select[Join[sourceFiles, If[StringQ[sourceDirectory],
  {FileNameJoin[{sourceDirectory, "internal", "HEPCAT-SPINAS-Optimize.wl"}]}, {}]],
  !FileExistsQ[#] || DirectoryQ[#] &];
If[!StringQ[sourceDirectory] || missingFiles =!= {},
  Message[Get::noopen, If[StringQ[sourceDirectory], StringRiffle[missingFiles, ", "],
    "the source directory specified by Global`$HEPCATpath"]];
  Return[$Failed]];

Global`$HEPCATpath = sourceDirectory;

BeginPackage["ConstructiveDiagrams`"];
Unprotect @@ Names["ConstructiveDiagrams`*"];
ClearAll @@ Names["ConstructiveDiagrams`*"];
Print["----------HEPCAT----------"];
Print[];
Print["Version: "];
Print["Authors: N. Christensen"];
Print["Please cite: ..."];
Print["Contributions by: ..."];
Print["Instructions can be found..."];
Print[];
Print["We welcome contributors.  Please contact Neil if interested."];
Print["------------------------------------"];
Begin["ConstructiveDiagrams`"];


(* ::Subsubsection:: *)
(*Load code*)


Scan[Get, sourceFiles];


(* ::Subsection::Closed:: *)
(*End Package*)


End[];
(* Resolve names after BeginPackage, not while the enclosing Module is parsed. *)
Protect @@ (ToExpression /@ {
  "nice", "lnice", "TeX", "lTeX", "Spinor", "rr", "Multiparticle", "MultiparticleDiff",
  "Particle", "HelicitySpinorHat", "SpinSpinorHat", "sigma", "delta", "SpinorChain", "Mom", "Mass", "SpinorTrace",
  "xFactor", "xTildeFactor", "MomProd", "MomProdHat1", "MomProdHat12", "MomProdHat2", "Output", "PropDen", "Coupling",
  "ReverseSequence", "ReduceSpinContractions", "ReduceSpinorProducts", "SpinorChainReduced", "SpinorTraceReduced",
  "ConvertStringAmplitude", "SimplifySpinorProducts", "ReduceXFactorsP", "ReduceXFactorsM", "ConstructiveDiagrams`ApplyToNumerator",
  "ExtractEnergy", "ExtractEnergy1", "ExtractEnergy2", "rSpinor", "ExpandSummation",
  "ReverseMomentum", "MomProdL", "MomProdR", "ChooseSpinIndices", "ChooseSpinIndex", "MakeIndicesExplicit", "SymmetrizeSpins", "SymmetrizeSpin",
  "SquareAmplitude", "ReadModel", "ReadVariables", "ReadFunctions", "ReadParticles", "ParticleMass", "ReadVertices", "ConvertVertex",
  "ModelParameter", "ModelFunction", "ModelParameterName", "ModelParameterRules", "AppendModelAmplitudes", "WriteSPINASProcess",
  "SChannelDiagram", "UChannelDiagram", "TChannelDiagram", "ConstructiveDiagram",
  "CompareAmpToSquaredAmp", "DeleteRandomConstructiveAmplitude", "CheckLengthConstructive", "CheckNumberOfSpinors", "CreateRandomConstructiveSpinorProduct",
  "CreateRandomMassFactor", "CreateRandomConstructiveAmplitudeTerm", "AddRandomConstructiveAmplitudeTerm", "CreateInitialRandomPopulationConstructiveAmplitudes",
  "RandomDeleteSpinorProduct", "RandomAddSpinorProduct", "RandomChangeCoefficientSpinorProduct", "MateConstructiveAmplitudes", "MakeNRandomChanges", "FindAmp",
  "CreateDiagrams", "CreateArrowDiagram", "CreateArrowDiagrams", "DiagramAmplitudes", "diagramAmplitude",
  "SchoutenRewrite", "SchoutenCandidates", "SpinorIdentityCandidates", "ApplySpinorIdentity", "SpinorExpressionComplexity", "ComplicateAmplitude",
  "DiagramAmplitudeRecords", "PhysicalPoleResidue", "CheckAmplitudes", "CheckPhysicalResidues",
  "PrepareAmplitudes", "PrepareDiagramAmplitudes", "CompleteFactorization", "CompleteHighEnergy", "ConstructAmplitude",
  "FeynmanModel", "DiracWord", "DiracGamma", "DiracGamma5", "DiracSlash",
  "LorentzIndex", "LorentzMetric", "LorentzMomentum", "LorentzScalarProduct", "DiracChain", "FermionBar", "FermionKet",
  "PolarizationVector", "ColorDelta", "ColorGenerator", "ColorStructure", "ColorIndex", "ColorTrace", "ReduceColor",
  "FeynmanToSpinorProducts", "VectorSpinIndex", "ValidateMadGraph"
});
EndPackage[];

(* Make local reference pages available to F1 and the Documentation Center.
   This optional registration never opens the help browser during loading. *)
With[{documentationLoader = FileNameJoin[{sourceDirectory, "internal", "HEPCAT-Documentation.wl"}]},
  If[FileExistsQ[documentationLoader], Get[documentationLoader]]
];
]
