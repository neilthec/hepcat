(* ::Package:: *)

(* ::Subsection:: *)
(*Begin Package*)


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
Print["------------------------------------"];
Begin["ConstructiveDiagrams`"];


(* ::Subsubsection:: *)
(*Load code*)


Get[ToFileName[Global`$HEPCATpath,"HEPCAT-Base.wl"]];
Get[ToFileName[Global`$HEPCATpath,"HEPCAT-Model.wl"]];
Get[ToFileName[Global`$HEPCATpath,"HEPCAT-SymReg.wl"]];
Get[ToFileName[Global`$HEPCATpath,"Hayward.wl"]];


(* ::Subsection:: *)
(*End Package*)


End[]
Protect[nice,lnice,TeX,lTeX,Spinor,rr,Multiparticle,MultiparticleDiff,Particle,HelicitySpinorHat,SpinSpinorHat,sigma,delta,SpinorChain,Mom,Mass,SpinorTrace];
Protect[xFactor,xTildeFactor,MomProd,MomProdHat1,MomProdHat12,MomProdHat2,Output,PropDen,Coupling];
Protect[ReverseSequence,ReduceSpinContractions,ReduceSpinorProducts,SpinorChainReduced,SpinorTraceReduced];
Protect[ConvertStringAmplitude];
Protect[SimplifySpinorProducts,ReduceXFactorsP,ReduceXFactorsM];
Protect[ExtractEnergy,ExtractEnergy1,ExtractEnergy2,rSpinor,ExpandSummation];
Protect[ReverseMomentum,MomProdL,MomProdR,ExpandSummation,ChooseSpinIndices,ChooseSpinIndex,MakeIndicesExplicit,SymmetrizeSpins,SymmetrizeSpin];
Protect[SquareAmplitude,ReadModel,ReadVariables,ReadFunctions,ReadParticles,ParticleMass,ReadVertices,ConvertVertex];
Protect[SChannelDiagram,SChannelDiagram,UChannelDiagram,TChannelDiagram,ConstructiveDiagram]; 
Protect[CompareAmpToSquaredAmp,DeleteRandomConstructiveAmplitude,CheckLengthConstructive,CheckNumberOfSpinors,CreateRandomConstructiveSpinorProduct];
Protect[CreateRandomMassFactor,CreateRandomConstructiveAmplitudeTerm,AddRandomConstructiveAmplitudeTerm,CreateInitialRandomPopulationConstructiveAmplitudes];
Protect[RandomDeleteSpinorProduct,RandomAddSpinorProduct,RandomChangeCoefficientSpinorProduct,MateConstructiveAmplitudes,MakeNRandomChanges];
Protect[FindAmp];
EndPackage[]
