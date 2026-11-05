(* ::Package:: *)

(* ::Subsection:: *)
(*Begin Package*)


Module[{sourceDirectory = If[ValueQ[Global`$HEPCATpath], Global`$HEPCATpath,
    DirectoryName[ExpandFileName[$InputFileName]]], sourceFiles, missingFiles},
sourceFiles = If[StringQ[sourceDirectory],
  FileNameJoin[{sourceDirectory, #}] & /@ {"HEPCAT-Base.wl", "HEPCAT-Parameters.wl", "HEPCAT-Model.wl", "HEPCAT-Feynman.wl", "HEPCAT-SPINAS.wl", "HEPCAT-MadGraph.wl"}, {}];
missingFiles = Select[Join[sourceFiles, If[StringQ[sourceDirectory],
  {FileNameJoin[{sourceDirectory, "HEPCAT-SPINAS-Optimize.wl"}]}, {}]],
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
  "PolarizationVector", "ColorDelta", "ColorGenerator", "ColorStructure", "ColorIndex",
  "FeynmanToSpinorProducts", "VectorSpinIndex", "ValidateMadGraph"
});
EndPackage[];

(* Make local reference pages available to F1 and the Documentation Center.
   This optional registration never opens the help browser during loading. *)
With[{documentationLoader = FileNameJoin[{sourceDirectory, "HEPCAT-Documentation.wl"}]},
  If[FileExistsQ[documentationLoader], Get[documentationLoader]]
];
]
