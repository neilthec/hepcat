(* Exact regression against the retained enumeration and basis algorithms. *)
HEPCATColorDedupTests`Private`root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{HEPCATColorDedupTests`Private`root,"source","HEPCAT.wl"}]];
Begin["HEPCATColorDedupTests`Private`"];
$ContextPath={"System`"};
$HistoryLength=0;
sourceFiles=Sort[FileNames["*.wl",FileNameJoin[{root,"source"}],Infinity]];
sourceBefore=FileHash[#,"SHA256"]&/@sourceFiles;
protectedDefinitions={DownValues[ConstructiveDiagrams`Private`colorTensorBasis],
 DownValues[ConstructiveDiagrams`Private`colorTensorVector],DownValues[ConstructiveDiagrams`Private`colorProjectionData]};
(* The reference is the retained pre-optimization algorithm, not the new implementation. *)
legacyBasis[tensors_List, representations_List, maxStates_Integer:65536] := Module[
  {vectors, gram, reduced, pivots, basis, coordinates, states},
  If[!AllTrue[representations, MemberQ[{1, 3, -3, 8}, #] &],
    Return[ConstructiveDiagrams`Private`colorFailure["ColorRepresentation", "Only SU(3) singlet, fundamental, antifundamental and adjoint external states are supported."]]];
  states = Times @@ Abs[representations];
  If[states > maxStates, Return[ConstructiveDiagrams`Private`colorFailure["ColorBudget", "The exact external-color state space exceeds the configured allowance.",
    <|"ColorStateCount" -> states, "MaxColorStates" -> maxStates|>]]];
  If[tensors === {}, Return[ConstructiveDiagrams`Private`colorFailure["ColorTensor", "At least one diagram tensor is required."]]];
  vectors = ConstructiveDiagrams`Private`colorTensorVectorEnumerated[#, representations] & /@ tensors;
  If[AnyTrue[vectors, FailureQ], Return[SelectFirst[vectors, FailureQ]]];
  gram = Simplify[Conjugate[vectors].Transpose[vectors]];
  If[!MatrixQ[gram, NumericQ], Return[ConstructiveDiagrams`Private`colorFailure["ColorContraction", "The color contractions did not evaluate to an exact numeric Gram matrix."]]];
  reduced = RowReduce[gram];
  pivots = DeleteCases[Map[FirstPosition[#, Except[0], Missing["Zero"], {1}, Heads -> False] &, reduced], _Missing];
  pivots = First /@ pivots;
  basis = tensors[[pivots]];
  coordinates = If[pivots === {}, ConstantArray[{}, Length[tensors]],
    Simplify[LinearSolve[gram[[pivots, pivots]], gram[[pivots, #]]]] & /@ Range[Length[tensors]]];
  If[pivots =!= {} && !TrueQ[Simplify[gram -
      Conjugate[coordinates].gram[[pivots, pivots]].Transpose[coordinates]] ===
        ConstantArray[0, Dimensions[gram]]],
    Return[ConstructiveDiagrams`Private`colorFailure["ColorBasis", "The exact color basis failed its reconstruction certificate."]]];
  <|"Status" -> "COMPLETE", "Representations" -> representations,
    "ExternalIndices" -> (ConstructiveDiagrams`ColorIndex /@ Select[Range[Length[representations]], Abs[representations[[#]]] > 1 &]),
    "DiagramTensors" -> tensors, "Basis" -> basis, "BasisPositions" -> pivots,
    "DiagramCoordinates" -> coordinates, "GramMatrix" -> gram[[pivots, pivots]],
    "DiagramGramMatrix" -> gram, "ColorStateCount" -> states|>
];

legacyProjection[pairs_List, channels_List, representations_List,
    benchmarkPairs_List: {}] := Module[
  {diagramTerms, channelTerms, benchmarkTerms, terms, counts, data, coordinates,
   offset = 0, split, diagramCoordinates, channelCoordinates, benchmarkCoordinates, rank},
  diagramTerms = ConstructiveDiagrams`Private`colorExpressionTerms[Last[#]] & /@ pairs;
  channelTerms = ConstructiveDiagrams`Private`colorExpressionTerms[Last[#]] & /@ channels;
  benchmarkTerms = ConstructiveDiagrams`Private`colorExpressionTerms[Last[#]] & /@ benchmarkPairs;
  If[AnyTrue[Join[diagramTerms, channelTerms, benchmarkTerms], FailureQ],
    Return[SelectFirst[Join[diagramTerms, channelTerms, benchmarkTerms], FailureQ]]];
  terms = Join[diagramTerms, channelTerms, benchmarkTerms]; counts = Length /@ terms;
  If[Total[counts] === 0, Return[ConstructiveDiagrams`Private`colorFailure["ZeroColorProjection",
    "A zero expression without any color tensor needs a nonzero channel or diagram to define its color basis."]]];
  data = legacyBasis[First /@ Flatten[terms, 1], representations];
  If[FailureQ[data], Return[data]];
  rank = Length[data["Basis"]]; coordinates = data["DiagramCoordinates"];
  split = Table[With[{start = offset + 1, count = counts[[i]]},
    offset += count;
    If[count === 0, ConstantArray[0, rank],
      Total[MapThread[#1 Last[#2] &, {Take[coordinates, {start, offset}], terms[[i]]}]]]],
    {i, Length[terms]}];
  diagramCoordinates = Take[split, Length[pairs]];
  channelCoordinates = Take[Drop[split, Length[pairs]], Length[channels]];
  benchmarkCoordinates = Drop[split, Length[pairs] + Length[channels]];
  Join[data, <|"DiagramCoefficients" -> diagramCoordinates,
    "ChannelCoefficients" -> channelCoordinates,
    "FeynmanCoefficients" -> benchmarkCoordinates,
    "ProjectedDiagrams" -> Table[MapThread[{First[#1], #2[[i]]} &,
      {pairs, diagramCoordinates}], {i, rank}],
    "ProjectedChannels" -> Table[MapThread[Append[Most[#1], #2[[i]]] &,
      {channels, channelCoordinates}], {i, rank}],
    "ProjectedFeynmanAmplitudes" -> Table[MapThread[{First[#1], #2[[i]]} &,
      {benchmarkPairs, benchmarkCoordinates}], {i, rank}]|>]
];

checks=0;failures={};SetAttributes[check,HoldRest];
check[name_,test_]:=(checks++;If[!TrueQ[test],AppendTo[failures,name];Print["FAIL: ",name]]);
old[t_List,r_List,rest___]:=legacyBasis[t,r,rest];
new[t_List,r_List,rest___]:=ConstructiveDiagrams`Private`colorTensorBasis[t,r,rest];
ci[i_]:=ConstructiveDiagrams`ColorIndex[i];
dummy[i_,d_]:=ConstructiveDiagrams`Private`colorInternalIndex[i,d];
d1=ConstructiveDiagrams`ColorDelta[ci[1],ci[2]]ConstructiveDiagrams`ColorDelta[ci[3],ci[4]];
d2=ConstructiveDiagrams`ColorDelta[ci[1],ci[4]]ConstructiveDiagrams`ColorDelta[ci[3],ci[2]];
g1=ConstructiveDiagrams`ColorGenerator[ci[2],ci[1],dummy[1,8]]*
 ConstructiveDiagrams`ColorGenerator[ci[4],ci[3],dummy[1,8]];
inputs={d2,d1,d2,0,I d1,d1,g1,d2};
oldRepeatedSeconds=First@AbsoluteTiming[oldRepeated=old[inputs,{3,-3,3,-3}]];
newRepeatedSeconds=First@AbsoluteTiming[newRepeated=new[inputs,{3,-3,3,-3}]];
check["all original fields agree exactly for repeats, dependencies, complex phases and zero tensors",
 AssociationQ[oldRepeated]&&AssociationQ[newRepeated]&&KeyTake[newRepeated,Keys[oldRepeated]]===oldRepeated];
check["basis positions retain original occurrence indices",
 newRepeated["BasisPositions"]===oldRepeated["BasisPositions"]&&newRepeated["BasisPositions"]==={1,2}];
dedup=newRepeated["ColorTensorDeduplication"];
check["the exact literal occurrence map reconstructs the original tensors",
 dedup["UniqueTensors"][[dedup["OccurrenceToUnique"]]]===inputs&&dedup["OccurrenceReconstructionVerified"]];
check["complex phase multiples remain distinct literal tensors",
 MemberQ[dedup["UniqueTensors"],I d1]&&dedup["UniqueTensorCount"]===5];
check["every original coordinate is restored in its original position",
 dedup["UniqueCoordinates"][[dedup["OccurrenceToUnique"]]]===newRepeated["DiagramCoordinates"]];
check["the original Hermitian Gram is the incidence-map expansion of the unique Gram",
 dedup["UniqueGramMatrix"][[dedup["OccurrenceToUnique"],dedup["OccurrenceToUnique"]]]===oldRepeated["DiagramGramMatrix"]];
compact=new[inputs,{3,-3,3,-3},65536,False];
check["compact diagnostics preserve every non-Gram legacy field",
 KeyDrop[KeyTake[compact,Keys[oldRepeated]],"DiagramGramMatrix"]===KeyDrop[oldRepeated,"DiagramGramMatrix"]];
check["an unmaterialized repeated Gram is explicit and reconstructible",
 compact["DiagramGramMatrix"]===Missing["NotMaterialized"]&&
 !compact["ColorTensorDeduplication"]["OriginalGramMaterialized"]&&dedup["UniqueGramReconstructionVerified"]];
trace=ConstructiveDiagrams`ColorTrace[ci[1],ci[2]];
traceInputs={trace,trace^2,trace+I trace^2,trace,0,trace^2};
oldTraceSeconds=First@AbsoluteTiming[oldTrace=old[traceInputs,{8,8}]];
newTraceSeconds=First@AbsoluteTiming[newTrace=new[traceInputs,{8,8}]];
check["trace powers and complex polynomial interference remain exactly unchanged",
 AssociationQ[newTrace]&&KeyTake[newTrace,Keys[oldTrace]]===oldTrace];
check["all-zero repeated tensors retain their rank-zero coordinates and Gram",
 With[{a=old[{0,0},{3,-3}],b=new[{0,0},{3,-3}]},AssociationQ[b]&&KeyTake[b,Keys[a]]===a&&b["DiagramCoordinates"]==={{},{}}]];
check["singlet tensor repetitions preserve exact normalization",
 With[{a=old[{1,1,1},{1}],b=new[{1,1,1},{1}]},AssociationQ[b]&&KeyTake[b,Keys[a]]===a]];
check["representation and color-space allowance failures remain identical",
 SameQ[old[{1},{6,-6}],new[{1},{6,-6}]]&&SameQ[old[{d1},{3,-3,3,-3},1],new[{d1},{3,-3,3,-3},1]]];
check["inexact coefficients and malformed dummy dimensions remain rejected",
 FailureQ[new[{0.5 d1},{3,-3,3,-3}]]&&
 FailureQ[new[{ConstructiveDiagrams`ColorGenerator[ci[2],ci[1],dummy[1,3]]},{3,-3}]]];
check["empty input preserves the existing explicit failure",SameQ[old[{},{}],new[{},{}]]];
external=Thread[(ConstructiveDiagrams`Mass/@Range[4])->0];
pairs={{external,a d1},{external,b d1},{external,c d1}};
channels={{{1,2},massOne,d d1},{{1,3},massTwo,e d1}};
benchmarks={{external,f d1+g d2},{external,h d2}};
oldProjection=legacyProjection[pairs,channels,{3,-3,3,-3},benchmarks];
newProjection=ConstructiveDiagrams`Private`colorProjectionData[pairs,channels,{3,-3,3,-3},benchmarks];
projectionKeys={"Basis","BasisPositions","GramMatrix","DiagramTensors","DiagramCoordinates",
 "DiagramCoefficients","ChannelCoefficients","FeynmanCoefficients","ProjectedDiagrams",
 "ProjectedChannels","ProjectedFeynmanAmplitudes"};
check["weighted source/channel/benchmark projections agree exactly with the original backend",
 AssociationQ[newProjection]&&KeyTake[newProjection,projectionKeys]===KeyTake[oldProjection,projectionKeys]];
check["a direction appearing only in the independent benchmark remains in the common basis",
 newProjection["Basis"]==={d1,d2}&&newProjection["FeynmanCoefficients"]==={{f,g},{0,h}}];
check["repeated diagram contributions are summed with every original coefficient",
 Total[newProjection["DiagramCoefficients"]]==={a+b+c,0}&&Length[newProjection["ProjectedDiagrams"][[1]]]===3];
check["channel and independent-benchmark contributions are neither averaged nor discarded",
 Total[newProjection["ChannelCoefficients"]]==={d+e,0}&&Total[newProjection["FeynmanCoefficients"]]==={f,g+h}];
check["the projection stores only the unique Gram plus an exact occurrence map",
 newProjection["DiagramGramMatrix"]===Missing["NotMaterialized"]&&
 newProjection["ColorTensorDeduplication"]["UniqueTensorCount"]===2&&
 newProjection["ColorTensorDeduplication"]["OriginalTensorCount"]===8];
check["polynomial color sums with term-local trace dummies retain exact occurrence projection",
 With[{ps={{external,a trace+b trace^2},{external,c trace}},bs={{external,I trace^2}}},
  KeyTake[ConstructiveDiagrams`Private`colorProjectionData[ps,{},{8,8},bs],projectionKeys]===
   KeyTake[legacyProjection[ps,{},{8,8},bs],projectionKeys]]];
check["zero-only projection retains the original failure semantics",
 SameQ[ConstructiveDiagrams`Private`colorProjectionData[{{external,0}},{},{3,-3,3,-3}],
  legacyProjection[{{external,0}},{},{3,-3,3,-3}]]];
check["the prototype never replaces existing package algebra definitions",
 protectedDefinitions==={DownValues[ConstructiveDiagrams`Private`colorTensorBasis],
 DownValues[ConstructiveDiagrams`Private`colorTensorVector],DownValues[ConstructiveDiagrams`Private`colorProjectionData]}];
check["every package source file remains byte-identical",sourceBefore===(FileHash[#,"SHA256"]&/@sourceFiles)];
Print["COLOR TENSOR DEDUP CHECKS: ",checks,"; FAILURES: ",Length[failures]];
Print["COLOR TENSOR DEDUP SMALL TIMINGS: ",InputForm[<|
 "RepeatedOldSeconds"->oldRepeatedSeconds,"RepeatedDedupSeconds"->newRepeatedSeconds,
 "TraceOldSeconds"->oldTraceSeconds,"TraceDedupSeconds"->newTraceSeconds|>]];
If[failures==={},Print["HEPCAT_TEST_COMPLETED:color-tensor-dedup-tests.wl"],Print[failures]];
Quit[If[failures==={},0,1]];
