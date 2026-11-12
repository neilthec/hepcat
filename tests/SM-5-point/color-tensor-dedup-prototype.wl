(* Isolated exact-color deduplication prototype. Load HEPCAT first.
   This file never replaces package definitions and is not used by construction.
   Only literal SameQ tensors are shared. Every scalar occurrence is retained. *)
Begin["HEPCATColorTensorDedupPrototype`Private`"];

HEPCATColorTensorDedupPrototype`Private`tensorBasis[tensors_List, representations_List,
    maxStates_Integer:65536, materializeOriginalGram_:True, progress_:None] := Module[
  {unique, occurrence, firstPositions, vectors, gram, reduced, pivots, basisGram,
   solve, uniqueCoordinates, coordinates, states, reconstructed, originalGram,notify},
  notify[phase_,details_]:=If[progress=!=None,progress[phase,details]];
  If[!AllTrue[representations, MemberQ[{1,3,-3,8},#]&],
    Return[ConstructiveDiagrams`Private`colorFailure["ColorRepresentation",
      "Only SU(3) singlet, fundamental, antifundamental and adjoint external states are supported."]]];
  states=Times@@Abs[representations];
  If[states>maxStates,Return[ConstructiveDiagrams`Private`colorFailure["ColorBudget",
    "The exact external-color state space exceeds the configured allowance.",
    <|"ColorStateCount"->states,"MaxColorStates"->maxStates|>]]];
  If[tensors==={},Return[ConstructiveDiagrams`Private`colorFailure["ColorTensor",
    "At least one diagram tensor is required."]]];
  If[!MemberQ[{True,False},materializeOriginalGram],Return[Failure["PrototypeGramMode",<||>]]];
  unique=DeleteDuplicates[tensors,SameQ];
  occurrence=Map[Function[tensor,First@FirstPosition[unique,
    value_/;SameQ[value,tensor],Missing["NotFound"],{1},Heads->False]],tensors];
  If[!VectorQ[occurrence,IntegerQ]||unique[[occurrence]]=!=tensors,
    Return[Failure["PrototypeOccurrenceReconstruction",<||>]]];
  firstPositions=Table[First@FirstPosition[occurrence,i],{i,Length[unique]}];
  vectors=Table[notify["TensorVectorStart",<|"Index"->i,"Count"->Length[unique]|>];
    With[{vector=ConstructiveDiagrams`Private`colorTensorVector[unique[[i]],representations]},
      notify["TensorVectorComplete",<|"Index"->i,"Count"->Length[unique]|>];vector],{i,Length[unique]}];
  If[AnyTrue[vectors,FailureQ],Return[SelectFirst[vectors,FailureQ]]];
  notify["UniqueGramStart",<|"Dimensions"->{Length[unique],Length[unique]}|>];
  gram=Simplify[Conjugate[vectors].Transpose[vectors]];
  If[!MatrixQ[gram,NumericQ],Return[ConstructiveDiagrams`Private`colorFailure["ColorContraction",
    "The color contractions did not evaluate to an exact numeric Gram matrix."]]];
  notify["UniqueRowReductionStart",<|"Dimensions"->Dimensions[gram]|>];
  reduced=RowReduce[gram];
  pivots=First/@DeleteCases[Map[FirstPosition[#,Except[0],Missing["Zero"],{1},Heads->False]&,reduced],_Missing];
  basisGram=gram[[pivots,pivots]];
  notify["UniqueCoordinateSolveStart",<|"Rank"->Length[pivots]|>];
  uniqueCoordinates=If[pivots==={},ConstantArray[{},Length[unique]],
    solve=LinearSolve[basisGram];Simplify[solve[gram[[pivots,#]]]]&/@Range[Length[unique]]];
  reconstructed=If[pivots==={},True,
    TrueQ[Simplify[gram-Conjugate[uniqueCoordinates].basisGram.Transpose[uniqueCoordinates]]===
      ConstantArray[0,Dimensions[gram]]]];
  If[!reconstructed,Return[ConstructiveDiagrams`Private`colorFailure["ColorBasis",
    "The exact color basis failed its reconstruction certificate."]]];
  coordinates=uniqueCoordinates[[occurrence]];
  originalGram=If[TrueQ[materializeOriginalGram],gram[[occurrence,occurrence]],Missing["NotMaterialized"]];
  <|"Status"->"COMPLETE","Representations"->representations,
    "ExternalIndices"->(ConstructiveDiagrams`ColorIndex/@Select[Range[Length[representations]],Abs[representations[[#]]]>1&]),
    "DiagramTensors"->tensors,"Basis"->unique[[pivots]],"BasisPositions"->firstPositions[[pivots]],
    "DiagramCoordinates"->coordinates,"GramMatrix"->basisGram,"DiagramGramMatrix"->originalGram,
    "ColorStateCount"->states,"ColorTensorDeduplication"-><|
      "OriginalTensorCount"->Length[tensors],"UniqueTensorCount"->Length[unique],
      "UniqueTensors"->unique,"OccurrenceToUnique"->occurrence,"UniqueGramMatrix"->gram,
      "UniqueCoordinates"->uniqueCoordinates,"UniqueBasisPositions"->pivots,
      "OccurrenceReconstructionVerified"->True,"UniqueGramReconstructionVerified"->reconstructed,
      "OriginalGramMaterialized"->TrueQ[materializeOriginalGram]|>|>
];

(* This is the package projection's original weighted occurrence sum, with only
   the basis provider replaced. The optional repeated Gram is not materialized. *)
HEPCATColorTensorDedupPrototype`Private`projectionData[pairs_List,channels_List,
    representations_List,benchmarkPairs_List:{}] := Module[
  {diagramTerms,channelTerms,benchmarkTerms,terms,counts,data,coordinates,offset=0,
   split,diagramCoordinates,channelCoordinates,benchmarkCoordinates,rank},
  diagramTerms=ConstructiveDiagrams`Private`colorExpressionTerms[Last[#]]&/@pairs;
  channelTerms=ConstructiveDiagrams`Private`colorExpressionTerms[Last[#]]&/@channels;
  benchmarkTerms=ConstructiveDiagrams`Private`colorExpressionTerms[Last[#]]&/@benchmarkPairs;
  If[AnyTrue[Join[diagramTerms,channelTerms,benchmarkTerms],FailureQ],
    Return[SelectFirst[Join[diagramTerms,channelTerms,benchmarkTerms],FailureQ]]];
  terms=Join[diagramTerms,channelTerms,benchmarkTerms];counts=Length/@terms;
  If[Total[counts]===0,Return[ConstructiveDiagrams`Private`colorFailure["ZeroColorProjection",
    "A zero expression without any color tensor needs a nonzero channel or diagram to define its color basis."]]];
  data=HEPCATColorTensorDedupPrototype`Private`tensorBasis[First/@Flatten[terms,1],representations,65536,False];
  If[FailureQ[data],Return[data]];
  rank=Length[data["Basis"]];coordinates=data["DiagramCoordinates"];
  split=Table[With[{start=offset+1,count=counts[[i]]},offset+=count;
    If[count===0,ConstantArray[0,rank],Total[MapThread[#1 Last[#2]&,
      {Take[coordinates,{start,offset}],terms[[i]]}]]]],{i,Length[terms]}];
  diagramCoordinates=Take[split,Length[pairs]];
  channelCoordinates=Take[Drop[split,Length[pairs]],Length[channels]];
  benchmarkCoordinates=Drop[split,Length[pairs]+Length[channels]];
  Join[data,<|"DiagramCoefficients"->diagramCoordinates,"ChannelCoefficients"->channelCoordinates,
    "FeynmanCoefficients"->benchmarkCoordinates,
    "ProjectedDiagrams"->Table[MapThread[{First[#1],#2[[i]]}&,{pairs,diagramCoordinates}],{i,rank}],
    "ProjectedChannels"->Table[MapThread[Append[Most[#1],#2[[i]]]&,{channels,channelCoordinates}],{i,rank}],
    "ProjectedFeynmanAmplitudes"->Table[MapThread[{First[#1],#2[[i]]}&,
      {benchmarkPairs,benchmarkCoordinates}],{i,rank}]|>]
];
End[];
