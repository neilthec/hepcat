(* ::Package:: *)

(* Represent SU(3) color factors and simplify their contractions exactly.
   These routines sew internal color indices, build common color bases and
   retain interference when amplitudes are squared and external colors summed.
   They operate on the color tensors carried by the supplied expressions. *)

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



(* ::Section::Closed:: *)
(*Exact SU(3) Tree Color*)

(* A direct reload preserves the caller's symbol protection state. The main
   package loader also loads this module before protecting public symbols. *)
ConstructiveDiagrams`Private`$colorModuleProtected = Select[
  {ConstructiveDiagrams`ColorTrace, ConstructiveDiagrams`ReduceColor,
   ConstructiveDiagrams`ColorGenerator, ConstructiveDiagrams`ColorDelta,
   ConstructiveDiagrams`ColorStructure, ConstructiveDiagrams`ColorIndex},
  MemberQ[Attributes[#],Protected] &];
Unprotect @@ ConstructiveDiagrams`Private`$colorModuleProtected;

ColorTrace::usage = "ColorTrace[a,b,...] denotes the ordered fundamental SU(3) trace Tr(T^a T^b ...), with Tr(T^a T^b)=delta(a,b)/2. ReduceColor reduces its contracted indices.";
ReduceColor::usage = "ReduceColor[expression] applies exact SU(3) trace, completeness and contraction identities to ColorGenerator, ColorStructure, ColorDelta and ColorTrace. ColorIndex labels remain external and unsummed. IndexDimensions can declare otherwise ambiguous dummy dimensions; MaxTerms bounds color expansion.";
Options[ReduceColor] = {"IndexDimensions" -> {}, "SumExternalColors" -> False, "MaxTerms" -> 4096};

ColorGenerator /: Conjugate[ColorGenerator[i_, j_, a_]] := ColorGenerator[j, i, a];
ColorDelta /: Conjugate[ColorDelta[i_, j_]] := ColorDelta[i, j];
ColorStructure /: Conjugate[ColorStructure[a_, b_, c_]] := ColorStructure[a, b, c];
ColorTrace /: Conjugate[ColorTrace[labels___]] := ColorTrace @@ Reverse[{labels}];

Begin["ConstructiveDiagrams`Private`"];

(* ::Subsection::Closed:: *)
(*Symbolic SU(3) contractions*)

colorReductionObjectQ[value_] := MatchQ[value,
  _ColorDelta | _ColorGenerator | _ColorStructure | _ColorTrace];
colorReductionIndices[value_ColorTrace] := List @@ value;
colorReductionIndices[value_] := If[colorReductionObjectQ[value], List @@ value, {}];
colorReductionDummyQ[index_] := !MatchQ[index, _ColorIndex | _Integer];
colorReductionWord[labels_List, left_, right_] := Module[{nodes},
  If[labels === {}, Return[ColorDelta[left, right]]];
  nodes = Join[{left}, Table[colorInternalIndex[Unique["trace"], 3], {Length[labels]-1}], {right}];
  Times @@ MapThread[ColorGenerator, {Most[nodes], Rest[nodes], labels}]
];
(* Every occurrence has its own fundamental sum. In particular, expand a
   trace power occurrence by occurrence before Times can combine factors:
   (Sum_i M_ii)^2 is not Sum_i (M_ii)^2. *)
colorExpandTraces[expression_] := Module[{walk},
  walk[trace_ColorTrace] := If[Length[trace] === 0,3,
    With[{start=colorInternalIndex[Unique["trace"],3]},
      colorReductionWord[List @@ trace,start,start]]];
  walk[Power[base_,exponent_Integer?Positive]] :=
    Times @@ Table[walk[base],{exponent}];
  walk[value_Times] := Times @@ (walk /@ List @@ value);
  walk[value_Plus] := Plus @@ (walk /@ List @@ value);
  walk[value_] := value;
  walk[expression]
];

(* The conjugate amplitude is a second copy of every internal color sum.
   Rename only tensor slots, term by term, so a scalar using the same symbol
   as a color dummy is unchanged and external color states stay shared. *)
colorFreshenConjugateDummies[expression_] := Module[{terms,freshTensor},
  freshTensor[tensor_] := Module[{indices,dummies,rules},
    indices=Flatten[colorReductionIndices /@ Flatten[Map[
      If[MatchQ[#,Power[_,_Integer?Positive]],ConstantArray[#[[1]],#[[2]]],{#}] &,
      If[Head[tensor]===Times,List @@ tensor,{tensor}]],1]];
    dummies=Select[DeleteDuplicates[indices],Count[indices,#]===2 &&
      (Head[#]===Symbol || MatchQ[#,colorInternalIndex[_,_]]) &];
    rules=Map[Function[index,index -> If[MatchQ[index,colorInternalIndex[_,_]],
      colorInternalIndex[Unique["conjugateColor"],Last[index]],Unique["conjugateColor"]]],dummies];
    tensor /. object:(_ColorDelta|_ColorGenerator|_ColorStructure|_ColorTrace) :>
      Map[Replace[#,rules] &,object]];
  terms=colorExpressionTerms[expression];
  If[FailureQ[terms],Return[terms]];
  Total[(Last[#] freshTensor[First[#]]) & /@ terms]
];

(* Keep spinor and other scalar coefficients factored. Only color-bearing
   polynomials are distributed, with an explicit user-controlled allowance. *)
colorReductionTerms[expression_, maxTerms_, tag_] := Module[{walk, multiply},
  multiply[left_List, right_List] := Module[{a,b},
    If[Length[left] Length[right] > maxTerms,
      Throw[colorFailure["ColorBudget", "The color reduction exceeded MaxTerms."], tag]];
    Flatten[Table[{a[[1]] b[[1]], a[[2]] b[[2]]}, {a,left}, {b,right}],1]];
  walk[value_] := Which[
    FreeQ[value, _ColorDelta | _ColorGenerator | _ColorStructure | _ColorTrace], {{1,value}},
    colorReductionObjectQ[value], {{value,1}},
    Head[value] === Plus, With[{parts = Join @@ (walk /@ List @@ value)},
      If[Length[parts] > maxTerms,
        Throw[colorFailure["ColorBudget", "The color reduction exceeded MaxTerms."], tag]]; parts],
    Head[value] === Times, Fold[multiply, {{1,1}}, walk /@ List @@ value],
    Head[value] === Power && IntegerQ[value[[2]]] && value[[2]] >= 0,
      Nest[multiply[#, walk[value[[1]]]] &, {{1,1}}, value[[2]]],
    True, Throw[colorFailure["NonPolynomialColor",
      "ReduceColor requires a polynomial in its explicit color tensors."], tag]];
  walk[expression]
];

(* A repeated symbolic label is an Einstein dummy; integer labels specify
   fixed components. External ColorIndex labels stay free even when repeated.
   Dimensions are inferred before contractions
   erase the tensors that distinguish fundamental from adjoint indices. *)
colorReductionDimensions[objects_List, supplied_List, tag_] := Module[
  {domains = Association[supplied], assign, indices, index, object, changed = True, get},
  get[index_] := Lookup[domains, index, Missing["Dimension"]];
  assign[index_Integer, dimension_] := If[!TrueQ[1 <= index <= dimension],
    Throw[colorFailure["ColorIndices", "A fixed color component is outside its representation.",
      <|"Index" -> index, "Dimension" -> dimension|>],tag]];
  assign[index_, dimension_] := If[MissingQ[get[index]],
    AssociateTo[domains,index -> dimension]; changed = True,
    If[get[index] =!= dimension, Throw[colorFailure["ColorIndices",
      "A color index has incompatible fundamental and adjoint dimensions.",
      <|"Index" -> index|>],tag]]];
  indices = Flatten[colorReductionIndices /@ objects];
  If[AnyTrue[indices,NumberQ[#] && (!IntegerQ[#] || !TrueQ[1 <= # <= 8]) &],
    Throw[colorFailure["ColorIndices", "A fixed SU(3) color component must be an integer from 1 through its representation dimension."],tag]];
  Do[If[MatchQ[index,colorInternalIndex[_,_]],
    If[!MemberQ[{3,8},Last[index]], Throw[colorFailure["ColorIndices",
      "An internal color index must have dimension 3 or 8."],tag]];
    assign[index,Last[index]]],{index,DeleteDuplicates[indices]}];
  Do[Switch[Head[object],
    ColorGenerator, MapThread[assign,{List @@ object,{3,3,8}}],
    ColorStructure | ColorTrace, Scan[assign[#,8] &,List @@ object]],{object,objects}];
  While[changed, changed = False;
    Do[If[Head[object] === ColorDelta,
      If[!MissingQ[get[object[[1]]]],assign[object[[2]],get[object[[1]]]]];
      If[!MissingQ[get[object[[2]]]],assign[object[[1]],get[object[[2]]]]]],{object,objects}]];
  Do[If[colorReductionDummyQ[index] && Count[indices,index] > 2,
    Throw[colorFailure["ColorMultiplicity",
      "An Einstein color dummy occurs more than twice in one term; use distinct contracted labels or an explicit sum.",
      <|"Index" -> index, "Occurrences" -> Count[indices,index]|>],tag]],
    {index,DeleteDuplicates[indices]}];
  domains
];

colorReductionCanonicalTrace[labels_List] := Which[
  labels === {}, 3,
  VectorQ[labels,IntegerQ], Tr[Dot @@ colorSU3Generators[][[labels]]],
  Length[labels] === 1, 0,
  Length[labels] === 2, (ColorDelta @@ Sort[labels])/2,
  True, ColorTrace @@ First[Sort[RotateLeft[labels,#] & /@ Range[0,Length[labels]-1]]]
];

colorReduceTensor[input_, supplied_, maxTerms_, tag_, sumExternal_] := Module[
  {tensor=input,initialObjects, domains, reduce, factorsOf, dimension, remove,
   external, indices, rules, descendants = 0, reduced},
  factorsOf[value_] := Flatten[Map[
    If[MatchQ[#,Power[_,_Integer?Positive]], ConstantArray[#[[1]],#[[2]]], {#}] &,
    If[Head[value]===Times,List @@ value,{value}]],1];
  initialObjects = Select[factorsOf[tensor],colorReductionObjectQ];
  domains = colorReductionDimensions[initialObjects,supplied,tag];
  dimension[index_] := If[MatchQ[index,colorInternalIndex[_,_]],Last[index],
    Lookup[domains,index,Missing["Dimension"]]];
  If[TrueQ[sumExternal],
    indices=Flatten[colorReductionIndices /@ initialObjects];
    external=Select[DeleteDuplicates[indices],MatchQ[#,_ColorIndex] &];
    rules=Map[Function[index,
      If[Count[indices,index]=!=2,Throw[colorFailure["ExternalColorMultiplicity",
        "SumExternalColors requires paired external slots of a squared or interference color expression.",
        <|"Index"->index|>],tag]];
      If[MissingQ[dimension[index]],Throw[colorFailure["ColorDimensions",
        "Declare an otherwise ambiguous external color dimension with IndexDimensions before summing it.",
        <|"Index"->index|>],tag]];
      index -> colorInternalIndex[Unique["externalColor"],dimension[index]]],external];
    tensor=tensor /. rules];
  remove[list_, positions_List] := Delete[list,List /@ Sort[positions]];
  reduce[value_] := Module[
    {factors, indices, frequency, deltas, generators, traces, position, d, x, y, other,
     start, chain, used, edge, current, found, left, right, word, a, occurrences,
     first, second, firstWord, secondWord, p, q, remainder, coefficient,
     canonical, terms, done, exitTag=Unique["colorReduced"]},
    done[answer_] := Throw[answer,exitTag];
    Catch[
    descendants++;
    If[descendants > 128 maxTerms,
      Throw[colorFailure["ColorBudget", "The color reduction exceeded its bounded contraction allowance."],tag]];
    canonical = value /. {
      trace_ColorTrace :> colorReductionCanonicalTrace[List @@ trace],
      ColorDelta[i_Integer,j_Integer] :> KroneckerDelta[i,j],
      delta : ColorDelta[i_,j_] /; !OrderedQ[{i,j}] :> ColorDelta[j,i],
      ColorGenerator[i_Integer,j_Integer,a_Integer] :> colorSU3Generators[][[a,i,j]]};
    If[canonical === 0, done[0]];
    terms = colorReductionTerms[canonical,maxTerms,tag];
    If[Length[terms] > 1, done[Total[(Last[#] reduce[First[#]]) & /@ terms]]];
    coefficient = Last[First[terms]];
    factors = Select[factorsOf[First[First[terms]]],colorReductionObjectQ];
    If[factors === {},done[coefficient]];
    indices = Flatten[colorReductionIndices /@ factors];
    frequency[index_] := Count[indices,index];
    deltas = Flatten[Position[factors,_ColorDelta,{1}]];
    Do[
      d = factors[[position]]; {x,y}=List @@ d;
      If[x === y,
        If[colorReductionDummyQ[x] && frequency[x]===2 && !MissingQ[dimension[x]],
          done[coefficient dimension[x] reduce[Times @@ remove[factors,{position}]]]],
        other = Which[colorReductionDummyQ[x] && frequency[x]===2,x,
          colorReductionDummyQ[y] && frequency[y]===2,y,True,None];
        If[other =!= None,
          done[coefficient reduce[(Times @@ remove[factors,{position}]) /.
            other -> If[other === x,y,x]]]]], {position,deltas}];

    (* Follow a closed fundamental cycle of any length. Fixed external color
       slots never qualify as its summed matrix-index links. *)
    generators = Flatten[Position[factors,_ColorGenerator,{1}]];
    Do[
      start = factors[[position,1]];
      If[!colorReductionDummyQ[start] || frequency[start]=!=2,Continue[]];
      used={};word={};edge=position;found=False;
      While[!MemberQ[used,edge],
        AppendTo[used,edge];AppendTo[word,factors[[edge,3]]];
        current=factors[[edge,2]];
        If[current === start,found=True;Break[]];
        If[!colorReductionDummyQ[current] || frequency[current]=!=2,Break[]];
        chain=Select[generators,!MemberQ[used,#] && factors[[#,1]]===current &];
        If[Length[chain]=!=1,Break[]];edge=First[chain]];
      If[found,done[coefficient reduce[(Times @@ remove[factors,used])
        (ColorTrace @@ word)]]],{position,generators}];

    (* Completeness removes one repeated adjoint label at a time. It applies
       within traces, between traces, and between open matrix words. *)
    traces=Flatten[Position[factors,_ColorTrace,{1}]];
    Do[
      If[!colorReductionDummyQ[a] || frequency[a]=!=2 || dimension[a]=!=8,Continue[]];
      occurrences=Join[
        Flatten[Table[If[factors[[p,3]]===a,{{p,0}},{}],{p,generators}],1],
        Flatten[Table[({p,First[#]} & /@ Position[List @@ factors[[p]],a,{1},Heads->False]),
          {p,traces}],1]];
      If[Length[occurrences]=!=2,Continue[]];
      {first,second}=occurrences;
      If[first[[1]]===second[[1]],
        p=first[[1]];word=List @@ factors[[p]];{x,y}=Sort[{first[[2]],second[[2]]}];
        firstWord=Take[word,{x+1,y-1}];
        secondWord=Join[Drop[word,y],Take[word,x-1]];
        remainder=Times @@ remove[factors,{p}];
        done[coefficient (reduce[remainder (ColorTrace @@ firstWord)(ColorTrace @@ secondWord)]/2-
          reduce[remainder (ColorTrace @@ Join[firstWord,secondWord])]/6)]];
      {p,q}={first[[1]],second[[1]]};
      remainder=Times @@ remove[factors,{p,q}];
      Which[
        first[[2]]===0 && second[[2]]===0,
          {x,y}=Take[List @@ factors[[p]],2];{left,right}=Take[List @@ factors[[q]],2];
          done[coefficient (reduce[remainder ColorDelta[x,right] ColorDelta[left,y]]/2-
            reduce[remainder ColorDelta[x,y] ColorDelta[left,right]]/6)],
        first[[2]]>0 && second[[2]]>0,
          firstWord=Delete[RotateLeft[List @@ factors[[p]],first[[2]]-1],1];
          secondWord=Delete[RotateLeft[List @@ factors[[q]],second[[2]]-1],1];
          done[coefficient (reduce[remainder (ColorTrace @@ Join[firstWord,secondWord])]/2-
            reduce[remainder (ColorTrace @@ firstWord)(ColorTrace @@ secondWord)]/6)],
        True,
          If[first[[2]]===0,{x,y}=Take[List @@ factors[[p]],2];
            word=List @@ factors[[q]];position=second[[2]],
            {x,y}=Take[List @@ factors[[q]],2];word=List @@ factors[[p]];position=first[[2]]];
          firstWord=Delete[RotateLeft[word,position-1],1];
          done[coefficient (reduce[remainder colorReductionWord[firstWord,x,y]]/2-
            reduce[remainder ColorDelta[x,y](ColorTrace @@ firstWord)]/6)]];
      ,{a,DeleteDuplicates[indices]}];
    coefficient (Times @@ factors)
    ,exitTag]
  ];
  reduced=reduce[tensor];
  Total[(First[#] Last[#]) & /@ colorReductionTerms[reduced,maxTerms,tag]]
];

ConstructiveDiagrams`ReduceColor[expression_,OptionsPattern[]] := Module[
  {dimensions=OptionValue["IndexDimensions"],sumExternal=OptionValue["SumExternalColors"],
   maxTerms=OptionValue["MaxTerms"],tag=Unique["colorReduction"],terms},
  If[AssociationQ[dimensions],dimensions=Normal[dimensions]];
  If[!ListQ[dimensions] || !AllTrue[dimensions,MatchQ[#,Rule[_,3|8]] &] ||
      !MemberQ[{True,False},sumExternal] || !(maxTerms===Infinity || IntegerQ[maxTerms] && maxTerms>0),
    Return[colorFailure["ColorOptions","IndexDimensions must declare dimensions 3 or 8, SumExternalColors must be True or False, and MaxTerms must be positive or Infinity."]]];
  If[AnyTrue[GatherBy[dimensions,First],Length[DeleteDuplicates[Last /@ #]]>1 &],
    Return[colorFailure["ColorOptions","IndexDimensions contains conflicting declarations for one index."]]];
  Catch[
    terms=colorReductionTerms[expression /. ColorStructure[a_,b_,c_] :>
      -2 I (ColorTrace[a,b,c]-ColorTrace[a,c,b]),maxTerms,tag];
    Total[(Last[#] colorReduceTensor[First[#],dimensions,maxTerms,tag,sumExternal]) & /@ terms],tag]
];

(* This module keeps color separate from the spinor coefficients. In
   particular, different color tensors are never added as scalar diagrams.
   The generators obey Tr[T_a T_b] = delta_ab/2. Their first matrix index is
   the antifundamental port, matching ColorGenerator in the Feynman reader. *)
colorSU3Generators[] := {
  {{0, 1/2, 0}, {1/2, 0, 0}, {0, 0, 0}},
  {{0, -I/2, 0}, {I/2, 0, 0}, {0, 0, 0}},
  DiagonalMatrix[{1/2, -1/2, 0}],
  {{0, 0, 1/2}, {0, 0, 0}, {1/2, 0, 0}},
  {{0, 0, -I/2}, {0, 0, 0}, {I/2, 0, 0}},
  {{0, 0, 0}, {0, 0, 1/2}, {0, 1/2, 0}},
  {{0, 0, 0}, {0, 0, -I/2}, {0, I/2, 0}},
  DiagonalMatrix[{1, 1, -2}]/(2 Sqrt[3])};

colorSU3StructureConstants[] := colorSU3StructureConstants[] = With[
  {t = colorSU3Generators[]}, SparseArray[Table[
    Simplify[-2 I Tr[(t[[a]].t[[b]] - t[[b]].t[[a]]).t[[c]]]],
    {a, 8}, {b, 8}, {c, 8}]]];

colorFailure[tag_String, message_String, details_: <||>] :=
  Failure[tag, Join[<|"MessageTemplate" -> message|>, details]];

colorParticleRow[name_String, model_List] := SelectFirst[model[[1]],
  ListQ[#] && Length[#] >= 8 && MemberQ[#[[{2, 3}]], name] &, Missing["Particle", name]];

colorParticleRepresentation[name_String, model_List] := Module[{row, representation},
  row = colorParticleRow[name, model];
  If[MissingQ[row], Return[colorFailure["ColorParticle", "The model does not declare this color species.",
    <|"Particle" -> name|>]]];
  representation = row[[8]];
  If[!MemberQ[{1, 3, -3, 8}, representation],
    Return[colorFailure["ColorRepresentation", "Only SU(3) singlet, fundamental, antifundamental and adjoint representations are supported.",
      <|"Particle" -> name, "Representation" -> representation|>]]];
  If[Abs[representation] === 3 && name === row[[3]] && row[[2]] =!= row[[3]],
    -representation, representation]
];

colorCanonicalSpinorChains[expression_] := expression /. chain_SpinorChain :>
  With[{items = List @@ chain}, If[OrderedQ[{items, Reverse[items]}], chain,
    (-1)^(Length[items] - 1) Apply[SpinorChain, Reverse[items]]]];

(* Reps alone do not distinguish f_abc from d_abc. An antisymmetric cubic
   spinor factor on identical bosonic ports certifies the antisymmetric
   invariant. Its i is fixed by [T_a,T_b] = i f_abc T_c, as in the Feynman
   reader, rather than fitted to a squared amplitude. *)
colorAntisymmetricAdjointVertexQ[row_List, model_List] := Module[
  {particles = DeleteCases[Drop[row, -2], ""], pairs, expression, exchanged, parity},
  If[Length[particles] =!= 3, Return[False]];
  pairs = Select[Subsets[Range[3], {2}], particles[[First[#]]] === particles[[Last[#]]] &&
    With[{particle = colorParticleRow[particles[[First[#]]], model]},
      ListQ[particle] && IntegerQ[particle[[5]]] && EvenQ[particle[[5]]]] &];
  If[pairs === {}, Return[False]];
  expression = colorCanonicalSpinorChains[row[[-1]]];
  If[TrueQ[expression === 0], Return[False]];
  AllTrue[pairs, Function[pair,
    exchanged = colorCanonicalSpinorChains[replaceVertexIndices[row[[-1]],
      Association[First[pair] -> Last[pair], Last[pair] -> First[pair]]]];
    TrueQ[Cancel[Together[expression + exchanged]] === 0]]]
];

colorVertexTensor[legs_List, model_List, row_List] := Module[
  {representations, positions, indices, anti, fund, adj},
  representations = colorParticleRepresentation[#["Particle"], model] & /@ legs;
  If[AnyTrue[representations, FailureQ], Return[SelectFirst[representations, FailureQ]]];
  positions = Select[Range[Length[legs]], representations[[#]] =!= 1 &];
  indices = Lookup[legs[[positions]], "ColorIndex"];
  representations = representations[[positions]];
  Which[
    positions === {}, 1,
    Sort[representations] === {-3, 3} || representations === {8, 8}, Apply[ColorDelta, indices],
    Sort[representations] === {-3, 3, 8},
      anti = First[FirstPosition[representations, -3]];
      fund = First[FirstPosition[representations, 3]];
      adj = First[FirstPosition[representations, 8]];
      ColorGenerator[indices[[anti]], indices[[fund]], indices[[adj]]],
    representations === {8, 8, 8} && colorAntisymmetricAdjointVertexQ[row, model],
      I Apply[ColorStructure, indices],
    True, colorFailure["AmbiguousColorVertex",
      "The particle representations and recorded vertex do not specify a supported unique color invariant. Supply an explicit color tensor; it cannot be inferred by multiplying a stripped amplitude.",
      <|"Particles" -> Lookup[legs, "Particle"], "Representations" -> representations|>]
  ]
];

(* Internal dummy indices are shared by the two endpoints of an edge and
   summed exactly. External indices retain the original all-ingoing labels. *)
colorDiagramTensor[diagram_Association, model_List] := Module[
  {vertices, tensors, structures, selected, legs, representation, edgeIndices, internal, failure},
  vertices = Lookup[diagram, "InternalVertices", Missing["Vertices"]];
  If[!ListQ[vertices], Return[colorFailure["ColorDiagram", "Expected a generated diagram with vertex provenance."]]];
  edgeIndices = Association @ KeyValueMap[Function[{edge, prop},
    representation = colorParticleRepresentation[prop["Particle"], model];
    edge -> If[FailureQ[representation], representation,
      colorInternalIndex[Sort[List @@ edge], Abs[representation]]]], Lookup[diagram, "Propagators", <||>]];
  If[AnyTrue[Values[edgeIndices], FailureQ], Return[SelectFirst[Values[edgeIndices], FailureQ]]];
  tensors = Table[
    structures = Lookup[diagram["VertexStructures"], vertex, {}];
    If[structures === {}, Return[colorFailure["ColorVertex", "A diagram has no attached vertex structure."]]];
    selected = Last[structures];
    legs = Map[Function[leg, Join[leg, <|"ColorIndex" -> If[leg["Kind"] === "External",
      ColorIndex[leg["Index"]], edgeIndices[leg["Source"]]]|>]], selected["IncidentLegs"]];
    colorVertexTensor[legs, model, selected["ModelRow"]], {vertex, vertices}];
  If[AnyTrue[tensors, FailureQ], SelectFirst[tensors, FailureQ], Times @@ tensors]
];

colorNormalizeInternalIndices[tensor_, representations_List] := Module[
  {objects, indices, unknown, domains = <||>, assign, dimension, changed, tag = Unique["colorIndices"]},
  objects = Cases[tensor, _ColorDelta | _ColorGenerator | _ColorStructure | _ColorTrace, {0, Infinity}];
  indices = DeleteDuplicates[Flatten[List @@ # & /@ objects]];
  dimension[index_] := Lookup[domains, index, Missing["Dimension"]];
  assign[index_, value_] := If[MissingQ[dimension[index]],
    AssociateTo[domains, index -> value]; changed = True,
    If[dimension[index] =!= value, Throw[colorFailure["ColorIndices",
      "A contracted color index has incompatible representation dimensions."], tag]]];
  Catch[
    Do[Which[
      MatchQ[index, ColorIndex[_Integer]],
        If[!TrueQ[1 <= First[index] <= Length[representations]],
          Throw[colorFailure["ColorIndices", "An external color label is out of range."], tag]];
        assign[index, Abs[representations[[First[index]]]]],
      MatchQ[index, colorInternalIndex[_, _Integer]], assign[index, Last[index]],
      Head[index] === Symbol, Null,
      True, Throw[colorFailure["ColorIndices", "An internal color contraction must have a symbolic or dimensioned dummy index."], tag]],
      {index, indices}];
    unknown = Select[indices, Head[#] === Symbol &];
    If[AnyTrue[unknown, Count[Flatten[List @@ # & /@ objects], #] < 2 &],
      Throw[colorFailure["ColorIndices", "An internal color dummy is not paired with another tensor port."], tag]];
    Do[Switch[Head[object],
      ColorGenerator, MapThread[assign, {List @@ object, {3, 3, 8}}],
      ColorStructure, Scan[assign[#, 8] &, List @@ object]], {object, objects}];
    changed = True;
    While[changed, changed = False;
      Do[If[Head[object] === ColorDelta,
        If[!MissingQ[dimension[object[[1]]]], assign[object[[2]], dimension[object[[1]]]]];
        If[!MissingQ[dimension[object[[2]]]], assign[object[[1]], dimension[object[[2]]]]]], {object, objects}]];
    If[AnyTrue[unknown, MissingQ[dimension[#]] &],
      Throw[colorFailure["ColorIndices", "The dimension of a color dummy is ambiguous; retain its representation metadata."], tag]];
    tensor /. (Rule[#, colorInternalIndex[#, dimension[#]]] & /@ unknown), tag]
];

(* Retain the original finite-state enumerator as the exact fallback.
   Unsupported incidence patterns do not change their previous semantics. *)
ConstructiveDiagrams`Private`colorTensorVectorEnumerated[input_, representations_List] := Module[
  {tensor, n = Length[representations], external, internal, indices, dimensions, states, internalStates,
   generators = colorSU3Generators[], structure, rules, values, terms, stripped, vectors},
  terms=colorExpressionTerms[input];
  If[FailureQ[terms],Return[terms]];
  If[!FreeQ[input,_Real] || !AllTrue[terms,NumericQ[Last[#]] &],
    Return[colorFailure["ColorTensor", "A color tensor must have exact numeric coefficients and supported tensor heads."]]];
  If[terms==={},Return[ConstantArray[0,Times @@ Abs[representations]]]];
  (* Dummy sums belong to a monomial, not to the union of labels in a sum.
     Evaluate each color polynomial term before combining its vector. *)
  If[Length[terms]>1,
    vectors=Map[Function[term,With[{vector=ConstructiveDiagrams`Private`colorTensorVectorEnumerated[First[term],representations]},
      If[FailureQ[vector],vector,Last[term] vector]]],terms];
    Return[If[AnyTrue[vectors,FailureQ],SelectFirst[vectors,FailureQ],Total[vectors]]]];
  tensor=colorNormalizeInternalIndices[colorExpandTraces[input],representations];
  If[FailureQ[tensor], Return[tensor]];
  stripped = tensor /. (_ColorDelta | _ColorGenerator | _ColorStructure | _ColorTrace) -> 1;
  If[!NumericQ[stripped] || !FreeQ[tensor, _Real],
    Return[colorFailure["ColorTensor", "A color tensor must have exact numeric coefficients and supported tensor heads."]]];
  indices = DeleteDuplicates[Flatten[(List @@ # &) /@
    Cases[tensor, _ColorDelta | _ColorGenerator | _ColorStructure | _ColorTrace, {0, Infinity}]]];
  If[!AllTrue[indices, MatchQ[#, ColorIndex[i_Integer] /; 1 <= i <= n && Abs[representations[[i]]] > 1] ||
      MatchQ[#, colorInternalIndex[_, d_Integer] /; MemberQ[{1, 3, 8}, d]] &],
    Return[colorFailure["ColorIndices", "Color indices must identify declared external legs or dimensioned internal contractions."]]];
  external = ColorIndex /@ Select[Range[n], Abs[representations[[#]]] > 1 &];
  If[Complement[external, indices] =!= {}, Return[colorFailure["ColorIndices",
    "Each nonzero color tensor must carry every colored external leg explicitly."]]];
  internal = Cases[indices, _colorInternalIndex];
  dimensions = Abs[representations[[Select[Range[n], Abs[representations[[#]]] > 1 &]]]];
  states = Tuples[Range /@ dimensions];
  internalStates = Tuples[Range /@ (Last /@ internal)];
  structure = If[FreeQ[tensor, _ColorStructure], None, colorSU3StructureConstants[]];
  Table[
    Total[Table[
      rules = Join[Thread[external -> state], Thread[internal -> inner]];
      tensor /. rules /. {
        ColorDelta[a_Integer, b_Integer] :> KroneckerDelta[a, b],
        ColorGenerator[a_Integer, b_Integer, c_Integer] :> generators[[c, a, b]],
        ColorStructure[a_Integer, b_Integer, c_Integer] :> structure[[a, b, c]]},
      {inner, internalStates}]], {state, states}]
];


(* Sparse primitive contraction reorders only exact finite sums/products.
   Each internal index joins two distinct primitive factors, and external
   indices occur once; repeated/self ports use the retained enumerator. *)
ConstructiveDiagrams`Private`colorNetworkIndexDimension[index_,representations_List]:=Which[
 MatchQ[index,ColorIndex[i_Integer]/;1<=i<=Length[representations]],Abs[representations[[First[index]]]],
 MatchQ[index,colorInternalIndex[_,d_Integer]/;MemberQ[{1,3,8},d]],Last[index],
 True,Missing["UnsupportedIndex"]];
ConstructiveDiagrams`Private`colorNetworkAxisOrder[array_,axes_List,order_List]:=Module[{positions},
 positions=(First@FirstPosition[axes,#]&)/@order;
 If[positions===Range[Length[axes]],array,Transpose[array,Ordering[positions]]]];
ConstructiveDiagrams`Private`colorNetworkContract[left_Association,right_Association,representations_List]:=Module[
 {shared,freeLeft,freeRight,axes,dimensions,inner,leftWidth,rightWidth,a,b,product},
 shared=Select[left["Axes"],MemberQ[right["Axes"],#]&&MatchQ[#,colorInternalIndex[_,_]]&];
 freeLeft=Select[left["Axes"],!MemberQ[shared,#]&];freeRight=Select[right["Axes"],!MemberQ[shared,#]&];
 axes=Join[freeLeft,freeRight];dimensions=ConstructiveDiagrams`Private`colorNetworkIndexDimension[#,representations]&/@axes;
 inner=Times@@(ConstructiveDiagrams`Private`colorNetworkIndexDimension[#,representations]&/@shared);
 leftWidth=Times@@(ConstructiveDiagrams`Private`colorNetworkIndexDimension[#,representations]&/@freeLeft);
 rightWidth=Times@@(ConstructiveDiagrams`Private`colorNetworkIndexDimension[#,representations]&/@freeRight);
 a=ArrayReshape[ConstructiveDiagrams`Private`colorNetworkAxisOrder[left["Data"],left["Axes"],Join[freeLeft,shared]],{leftWidth,inner}];
 b=ArrayReshape[ConstructiveDiagrams`Private`colorNetworkAxisOrder[right["Data"],right["Axes"],Join[shared,freeRight]],{inner,rightWidth}];
 product=SparseArray[a].SparseArray[b];
 <|"Axes"->axes,"Data"->If[axes==={},First[First[Normal[product]]],ArrayReshape[product,dimensions]],
   "ContractedInternalAxes"->shared|>
];
ConstructiveDiagrams`Private`colorNetworkMonomial[tensor_,representations_List]:=Module[
 {normalized,factors,primitive,scalar,nodes,axes,allIndices,internal,external,expected,dimensions,head,data,
  pairs,shared,free,width,choices,pair,joined,steps={},closed,vector,generators,tag=Unique["network"]},
 Catch[
 normalized=colorNormalizeInternalIndices[
  colorExpandTraces[tensor],representations];
 If[FailureQ[normalized],Throw[Missing["NormalizationRejected"],tag]];
 factors=If[Head[normalized]===Times,List@@normalized,{normalized}];
 factors=Flatten[Map[If[MatchQ[#,Power[_,_Integer?Positive]],ConstantArray[#[[1]],#[[2]]],{#}]&,factors],1];
 scalar=Times@@Select[factors,NumericQ];primitive=Select[factors,!NumericQ[#]&];
 If[!FreeQ[scalar,_Real]||!AllTrue[primitive,MatchQ[#,_ColorDelta|
   _ColorGenerator|_ColorStructure]&],Throw[Missing["UnsupportedPrimitiveGrammar"],tag]];
 axes=List@@#&/@primitive;
 If[AnyTrue[axes,!DuplicateFreeQ[#]&],Throw[Missing["RepeatedPortWithinPrimitive"],tag]];
 allIndices=Flatten[axes,1];internal=Select[DeleteDuplicates[allIndices],MatchQ[#,colorInternalIndex[_,_]]&];
 external=Select[DeleteDuplicates[allIndices],MatchQ[#,_ColorIndex]&];
 expected=ColorIndex/@Select[Range[Length[representations]],Abs[representations[[#]]]>1&];
 If[Sort[external]=!=Sort[expected]||!AllTrue[expected,Count[allIndices,#]===1&],Throw[Missing["RepeatedOrMissingExternalPorts"],tag]];
 If[!AllTrue[internal,Count[allIndices,#]===2&]||Length[allIndices]=!=Length[external]+2Length[internal],
  Throw[Missing["UnsupportedInternalIncidence"],tag]];
 dimensions=ConstructiveDiagrams`Private`colorNetworkIndexDimension[#,representations]&/@allIndices;
 If[AnyTrue[dimensions,MissingQ],Throw[Missing["UnsupportedIndexDimension"],tag]];
 generators=colorSU3Generators[];
 nodes=MapThread[Function[{object,indices},
  head=Head[object];dimensions=ConstructiveDiagrams`Private`colorNetworkIndexDimension[#,representations]&/@indices;
  data=Which[
   head===ColorDelta&&Length[indices]===2&&SameQ@@dimensions,SparseArray[IdentityMatrix[First[dimensions]]],
   head===ColorGenerator&&dimensions==={3,3,8},
    SparseArray[Table[generators[[a,i,j]],{i,3},{j,3},{a,8}]],
   head===ColorStructure&&dimensions==={8,8,8},colorSU3StructureConstants[],
   True,Throw[Missing["PrimitiveDimensionMismatch"],tag]];
  <|"Axes"->indices,"Data"->data|>],{primitive,axes}];
 While[Length[nodes]>1,
  pairs=Subsets[Range[Length[nodes]],{2}];
  choices=Map[Function[positions,
   shared=Select[nodes[[First[positions]]]["Axes"],MemberQ[nodes[[Last[positions]]]["Axes"],#]&&MemberQ[internal,#]&];
   free=Join[Complement[nodes[[First[positions]]]["Axes"],shared],Complement[nodes[[Last[positions]]]["Axes"],shared]];
   width=Times@@(ConstructiveDiagrams`Private`colorNetworkIndexDimension[#,representations]&/@free);
   {If[shared==={},1,0],width,positions}],pairs];
  pair=Last[First[Sort[choices]]];
  joined=ConstructiveDiagrams`Private`colorNetworkContract[nodes[[First[pair]]],nodes[[Last[pair]]],representations];
  AppendTo[steps,<|"ContractedInternalAxes"->joined["ContractedInternalAxes"],"OutputAxes"->joined["Axes"]|>];
  nodes=Append[Delete[nodes,List/@pair],KeyDrop[joined,"ContractedInternalAxes"]];
  closed=Select[nodes,#["Axes"]==={}&];scalar*=Times@@Lookup[closed,"Data",{}];
  nodes=Select[nodes,#["Axes"]=!={}&]];
 If[nodes==={},vector=SparseArray[{1->scalar},{1}],
  If[Sort[First[nodes]["Axes"]]=!=Sort[expected],Throw[Missing["UncontractedInternalAxes"],tag]];
  data=ConstructiveDiagrams`Private`colorNetworkAxisOrder[First[nodes]["Data"],First[nodes]["Axes"],expected];
  vector=scalar ArrayReshape[data,{Times@@Abs[representations]}]];
 <|"Value"->SparseArray[vector],"ContractionPlan"->steps,"EveryInternalIndexPaired"->True,
   "EveryExternalIndexAppearsOnce"->True|>,tag]
];
ConstructiveDiagrams`Private`colorTensorVectorSparseDetails[input_,representations_List]:=Module[
 {terms,results,value,reason,fallback},
 terms=colorExpressionTerms[input];
 reason=Which[!AllTrue[representations,MemberQ[{1,3,-3,8},#]&],"UnsupportedRepresentation",
  FailureQ[terms],"UnsupportedColorPolynomial",!FreeQ[input,_Real]||!AllTrue[terms,NumericQ[Last[#]]&],"NonexactTensorCoefficient",True,None];
 If[reason===None,
  results=ConstructiveDiagrams`Private`colorNetworkMonomial[First[#],representations]&/@terms;
  If[AllTrue[results,AssociationQ],
   value=If[terms==={},SparseArray[{},{Times@@Abs[representations]}],Total[MapThread[Last[#1] #2["Value"]&,{terms,results}]]];
   Return[<|"Backend"->"ExactSparseTensorNetwork","Value"->SparseArray[value],"TermCertificates"->results,
     "FiniteSumReorderingOnly"->True|>]];
  reason=ToString[SelectFirst[results,!AssociationQ[#]&],InputForm]];
 fallback=ConstructiveDiagrams`Private`colorTensorVectorEnumerated[input,representations];
 <|"Backend"->"LegacyFallback","FallbackReason"->reason,
   "Value"->If[ListQ[fallback],SparseArray[fallback],fallback],"FiniteSumReorderingOnly"->False|>
];
ConstructiveDiagrams`Private`colorTensorVectorSparse[input_,representations_List]:=
 ConstructiveDiagrams`Private`colorTensorVectorSparseDetails[input,representations]["Value"];
ConstructiveDiagrams`Private`colorSparseTensorGram[vectors_List]:=Module[{rules,matrix,states},
 If[vectors==={}||!AllTrue[vectors,Head[#]===SparseArray&&Length[Dimensions[#]]===1&],Return[Failure["ColorSparseVectorShape",<||>]]];
 states=First[Dimensions[First[vectors]]];
 If[!AllTrue[vectors,Dimensions[#]==={states}&],Return[Failure["ColorSparseVectorShape",<||>]]];
 rules=Flatten[MapIndexed[Function[{v,row},
   (Most[ArrayRules[v]]/.Rule[{column_Integer},value_]:>Rule[{First[row],column},value])],vectors],1];
 matrix=SparseArray[rules,{Length[vectors],states}];
 Simplify[Normal[Conjugate[matrix].Transpose[matrix]]]
];

(* Preserve the existing dense list interface for private callers. Basis
   construction uses the sparse provider directly, without expanding zeros. *)
colorTensorVector[input_,representations_List] := With[
  {value=ConstructiveDiagrams`Private`colorTensorVectorSparse[input,representations]},
  If[Head[value]===SparseArray,Normal[value],value]];

(* For amplitude coefficients a_i, the color sum is Conjugate[a].G.a.
   DiagramCoordinates[[d,i]] obey C_d = Sum_i coordinates[[d,i]] C_i.
   Gram rank selects an independent tensor basis exactly, including all
   cross terms. No color multiplicity is guessed from a single diagram. *)
colorTensorBasis[tensors_List, representations_List,
    maxStates_Integer:65536, materializeOriginalGram_:True] := Module[
  {unique, occurrence, firstPositions, vectors, gram, reduced, pivots, basisGram,
   solve, uniqueCoordinates, coordinates, states, reconstructed, originalGram},
  If[!AllTrue[representations, MemberQ[{1,3,-3,8},#]&],
    Return[colorFailure["ColorRepresentation",
      "Only SU(3) singlet, fundamental, antifundamental and adjoint external states are supported."]]];
  states=Times@@Abs[representations];
  If[states>maxStates,Return[colorFailure["ColorBudget",
    "The exact external-color state space exceeds the configured allowance.",
    <|"ColorStateCount"->states,"MaxColorStates"->maxStates|>]]];
  If[tensors==={},Return[colorFailure["ColorTensor",
    "At least one diagram tensor is required."]]];
  If[!MemberQ[{True,False},materializeOriginalGram],Return[Failure["ColorGramMode",<||>]]];
  unique=DeleteDuplicates[tensors,SameQ];
  occurrence=Map[Function[tensor,First@FirstPosition[unique,
    value_/;SameQ[value,tensor],Missing["NotFound"],{1},Heads->False]],tensors];
  If[!VectorQ[occurrence,IntegerQ]||unique[[occurrence]]=!=tensors,
    Return[Failure["ColorOccurrenceReconstruction",<||>]]];
  firstPositions=Table[First@FirstPosition[occurrence,i],{i,Length[unique]}];
  vectors=ConstructiveDiagrams`Private`colorTensorVectorSparse[#,representations]&/@unique;
  If[AnyTrue[vectors,FailureQ],Return[SelectFirst[vectors,FailureQ]]];
  gram=ConstructiveDiagrams`Private`colorSparseTensorGram[vectors];
  If[!MatrixQ[gram,NumericQ],Return[colorFailure["ColorContraction",
    "The color contractions did not evaluate to an exact numeric Gram matrix."]]];
  reduced=RowReduce[gram];
  pivots=First/@DeleteCases[Map[FirstPosition[#,Except[0],Missing["Zero"],{1},Heads->False]&,reduced],_Missing];
  basisGram=gram[[pivots,pivots]];
  uniqueCoordinates=If[pivots==={},ConstantArray[{},Length[unique]],
    solve=LinearSolve[basisGram];Simplify[solve[gram[[pivots,#]]]]&/@Range[Length[unique]]];
  reconstructed=If[pivots==={},True,
    TrueQ[Simplify[gram-Conjugate[uniqueCoordinates].basisGram.Transpose[uniqueCoordinates]]===
      ConstantArray[0,Dimensions[gram]]]];
  If[!reconstructed,Return[colorFailure["ColorBasis",
    "The exact color basis failed its reconstruction certificate."]]];
  coordinates=uniqueCoordinates[[occurrence]];
  originalGram=If[TrueQ[materializeOriginalGram],gram[[occurrence,occurrence]],Missing["NotMaterialized"]];
  <|"Status"->"COMPLETE","Representations"->representations,
    "ExternalIndices"->(ColorIndex/@Select[Range[Length[representations]],Abs[representations[[#]]]>1&]),
    "DiagramTensors"->tensors,"Basis"->unique[[pivots]],"BasisPositions"->firstPositions[[pivots]],
    "DiagramCoordinates"->coordinates,"GramMatrix"->basisGram,"DiagramGramMatrix"->originalGram,
    "ColorStateCount"->states,"ColorTensorDeduplication"-><|
      "OriginalTensorCount"->Length[tensors],"UniqueTensorCount"->Length[unique],
      "UniqueTensors"->unique,"OccurrenceToUnique"->occurrence,"UniqueGramMatrix"->gram,
      "UniqueCoordinates"->uniqueCoordinates,"UniqueBasisPositions"->pivots,
      "OccurrenceReconstructionVerified"->True,"UniqueGramReconstructionVerified"->reconstructed,
      "OriginalGramMaterialized"->TrueQ[materializeOriginalGram]|>|>
];

(* Distribute only color-bearing sums. Spinor numerators and their factored
   sums remain intact. Each returned pair is {color tensor, spinor scalar}. *)
colorAmplitudeTerms[expression_] := colorExpressionTerms[expression];

colorDiagramBasis[diagrams_List, suppliedModel_] := Module[
  {model = If[Head[suppliedModel] === FeynmanModel, List @@ suppliedModel, suppliedModel],
   process, representations, resolved = {}, origins = {}, assignments, tensors = {}, basis,
   block, blockTensors},
  If[diagrams === {} || !ListQ[model] || Length[model] =!= 4,
    Return[colorFailure["ColorDiagram", "Provide generated diagrams and their model."]]];
  process = Lookup[First[diagrams], "ExternalParticles", Missing["Process"]];
  If[!ListQ[process] || !AllTrue[diagrams, Lookup[#, "ExternalParticles", None] === process &],
    Return[colorFailure["ColorDiagram", "Every diagram must have the same ordered external particles."]]];
  representations = colorParticleRepresentation[#, model] & /@ process;
  If[AnyTrue[representations, FailureQ], Return[SelectFirst[representations, FailureQ]]];
  Do[
    assignments = If[Lookup[diagrams[[i]], "HelicitySumEdges", {}] === {}, {None},
      Lookup[diagrams[[i]], "HelicityAssignments", {}]];
    If[assignments === {}, Return[colorFailure["ColorDiagram", "An internal-helicity sum has no recorded assignments."]]];
    block = If[# === None, diagrams[[i]], resolveDiagramHelicities[diagrams[[i]], #]] & /@ assignments;
    blockTensors = colorDiagramTensor[#, model] & /@ block;
    If[AnyTrue[blockTensors, FailureQ], Return[SelectFirst[blockTensors, FailureQ]]];
    (* Existing paired-helicity reductions remain valid only after checking
       that their color tensor is identical in every summed assignment. *)
    If[SameQ @@ blockTensors,
      AppendTo[resolved, diagrams[[i]]]; AppendTo[origins, i]; AppendTo[tensors, First[blockTensors]],
      resolved = Join[resolved, block]; origins = Join[origins, ConstantArray[i, Length[block]]];
      tensors = Join[tensors, blockTensors]], {i, Length[diagrams]}];
  basis = colorTensorBasis[tensors, representations];
  If[FailureQ[basis], basis, Join[basis, <|"ExternalParticles" -> process,
    "Diagrams" -> resolved, "DiagramOrigins" -> origins|>]]
];

colorProcessQ[process_List, model_List] := Length[model] === 4 &&
  AnyTrue[process, With[{row = colorParticleRow[#, model]},
    ListQ[row] && row[[8]] =!= 1] &];
colorProcessQ[___] := False;

colorLowerAmplitudeTensor[tagged_List, row_List, model_List, external_List] := Module[
  {rules = amplitudeParticleLabelRules[tagged, row], particles = DeleteCases[Drop[row, -2], ""], legs,
   label, rep, cut},
  legs = Table[
    label = i /. rules; rep = colorParticleRepresentation[particles[[i]], model];
    If[FailureQ[rep], Return[rep]];
    cut = If[MatchQ[label, _Multiparticle], canonicalPhysicalCut[List @@ label, external], None];
    <|"Particle" -> particles[[i]], "ColorIndex" -> If[IntegerQ[label], ColorIndex[label],
      colorInternalIndex[cut, Abs[rep]]]|>, {i, Length[particles]}];
  colorVertexTensor[legs, model, row]
];

(* A saved complete amplitude already carries its full color polynomial.
   Its external slots follow the kinematic relabeling, while every lookup
   receives a fresh namespace for its private internal contractions. *)
colorRelabelModelAmplitude[expression_, tagged_List, row_List, model_List, external_List] := Module[
  {rules = amplitudeParticleLabelRules[tagged, row], particles, slots, replacements,
   dummies, namespace, dummyRules, label, representation, cut},
  particles = DeleteCases[Drop[row, -2], ""];
  slots = DeleteDuplicates[Cases[expression, index_ColorIndex :> index, Infinity]];
  If[!AllTrue[slots, MatchQ[#, ColorIndex[_Integer]] &&
      1 <= First[#] <= Length[particles] &],
    Return[colorFailure["ColorModelAmplitude", "A saved external color slot is outside its particle list."]]];
  replacements = Table[
    label = First[slot] /. rules;
    representation = colorParticleRepresentation[particles[[First[slot]]], model];
    Which[
      FailureQ[representation], representation,
      Abs[representation] === 1,
        colorFailure["ColorModelAmplitude", "A saved color slot belongs to a colorless particle."],
      True,
        cut = If[MatchQ[label, _Multiparticle],
          If[external === {}, Sort[List @@ label], canonicalPhysicalCut[List @@ label, external]], None];
        slot -> If[IntegerQ[label], ColorIndex[label], colorInternalIndex[cut, Abs[representation]]]],
    {slot, slots}];
  If[AnyTrue[replacements, FailureQ], Return[SelectFirst[replacements, FailureQ]]];
  dummies = DeleteDuplicates[Cases[expression, index_colorInternalIndex :> index, Infinity]];
  namespace = CreateUUID["modelColor-"];
  dummyRules = (# -> colorInternalIndex[{namespace, First[#]}, Last[#]]) & /@ dummies;
  expression /. object : (_ColorDelta | _ColorGenerator | _ColorStructure | _ColorTrace) :>
    Map[Replace[#, Join[dummyRules, replacements]] &, object]
];

colorFactorizationChannels[process_List, model_List] := Module[{rows, failure},
  rows = Block[{$colorFactorizationEnabled = True, $colorExternalLegs = Range[Length[process]]},
    FactorizationAmplitudeChannels[process, model]];
  failure = Cases[rows, _Failure, {0, Infinity}];
  If[failure === {}, rows, First[failure]]
];

colorDiagramAmplitudes[diagrams_List] := Module[{model, data, pairs},
  model = {First[diagrams]["ColorParticles"], {}, {}, {}};
  data = colorDiagramBasis[diagrams, model];
  If[FailureQ[data], Return[data]];
  pairs = Block[{$colorScalarDiagrams = True},
    (First[DiagramAmplitudes[{#}]] &) /@ data["Diagrams"]];
  MapThread[{First[#1], Last[#1] #2} &, {pairs, data["DiagramTensors"]}]
];

(* Expand only the color polynomial. A large, color-free spinor numerator
   remains one coefficient, with its ordinary rational grouping intact. *)
colorExpressionTerms[expression_] := Module[{walk, multiply, result, tag = Unique["colorTerms"]},
  multiply[left_List, right_List] := If[Length[left] Length[right] > 4096,
    Throw[colorFailure["ColorBudget", "The color expansion exceeds 4096 tensor terms."], tag],
    Flatten[Table[{a[[1]] b[[1]], a[[2]] b[[2]]}, {a, left}, {b, right}], 1]];
  walk[value_] := Which[
    FreeQ[value, _ColorDelta | _ColorGenerator | _ColorStructure | _ColorTrace], {{1, value}},
    MatchQ[value, _ColorDelta | _ColorGenerator | _ColorStructure | _ColorTrace], {{value, 1}},
    Head[value] === Plus, Join @@ (walk /@ List @@ value),
    Head[value] === Times, Fold[multiply, {{1, 1}}, walk /@ List @@ value],
    Head[value] === Power && IntegerQ[value[[2]]] && 0 <= value[[2]],
      Nest[multiply[#, walk[value[[1]]]] &, {{1, 1}}, value[[2]]],
    True, Throw[colorFailure["NonPolynomialColor",
      "The amplitude must be polynomial in its explicit color tensors."], tag]];
  result = Catch[walk[expression], tag];
  If[FailureQ[result], result,
    If[Length[result] > 4096, colorFailure["ColorBudget", "The color expansion exceeds 4096 tensor terms."],
      Select[result, Last[#] =!= 0 &]]]
];

(* Include independent benchmark tensors in the same exact basis. A tensor
   present only in the benchmark must remain a separate zero candidate
   coefficient, not disappear because the diagram/channel basis omits it. *)
colorProjectionData[pairs_List, channels_List, representations_List,
    benchmarkPairs_List: {}] := Module[
  {diagramTerms, channelTerms, benchmarkTerms, terms, counts, data, coordinates,
   offset = 0, split, diagramCoordinates, channelCoordinates, benchmarkCoordinates, rank},
  diagramTerms = colorExpressionTerms[Last[#]] & /@ pairs;
  channelTerms = colorExpressionTerms[Last[#]] & /@ channels;
  benchmarkTerms = colorExpressionTerms[Last[#]] & /@ benchmarkPairs;
  If[AnyTrue[Join[diagramTerms, channelTerms, benchmarkTerms], FailureQ],
    Return[SelectFirst[Join[diagramTerms, channelTerms, benchmarkTerms], FailureQ]]];
  terms = Join[diagramTerms, channelTerms, benchmarkTerms]; counts = Length /@ terms;
  If[Total[counts] === 0, Return[colorFailure["ZeroColorProjection",
    "A zero expression without any color tensor needs a nonzero channel or diagram to define its color basis."]]];
  data = colorTensorBasis[First /@ Flatten[terms, 1], representations, 65536, False];
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

(* Preserve benchmark model scopes and external mass declarations here.
   Numerical resolution and mass agreement remain the scalar checker's job. *)
colorFeynmanPairs[input_, seconds_] := Module[{value = input, pairs},
  If[AssociationQ[value], value = Which[
    KeyExistsQ[value, "PreparedAmplitudes"], value["PreparedAmplitudes"],
    KeyExistsQ[value, "Amplitude"], {Lookup[value, "ExternalMassRules", {}], value["Amplitude"]},
    True, Failure["InvalidFeynmanInput", <||>]]];
  If[FailureQ[value] || value === $Failed,
    Return[colorFailure["InvalidFeynmanInput", "Supply a complete independent Feynman expression or amplitude pairs."]]];
  pairs = Which[residueAmplitudePairQ[value], {value},
    ListQ[value] && AllTrue[value, residueAmplitudePairQ], value,
    !ListQ[value], {{{}, value}},
    True, Return[colorFailure["InvalidFeynmanInput", "Supply a complete independent Feynman expression or amplitude pairs."]]];
  If[TrueQ[feynmanRawExpressionQ[pairs]],
    pairs = ConstructiveDiagrams`FeynmanToSpinorProducts[pairs,
      "ReduceSpinorProducts" -> False, "TimeConstraint" -> seconds]];
  If[!ListQ[pairs] || !AllTrue[pairs, residueAmplitudePairQ] ||
      !residueFiniteQ[pairs],
    colorFailure["InvalidFeynmanInput", "The colored Feynman benchmark did not convert completely; prepare its external helicities first when required."], pairs]
];

(* Each coefficient of an exact independent color basis has its own ordinary
   residue obligations. Statistics can mix these coefficients, so a scalar
   coefficient must never be independently Bose-averaged or antisymmetrized. *)
(* A two-fermion scalar/vector current can be reused only after proving its
   color flow. These exact contractions distinguish a common fundamental
   delta/generator from a guessed color multiplicity or a stripped amplitude. *)
colorScalarCurrentProjectionCertificate[diagrams_List, model_List,
    data_Association, representations_List] := Module[
  {fund, anti, adjoint, canonical, tensors, basis, coordinates, common},
  If[Length[representations] =!= 4 || Length[Lookup[data,"Basis",{}]] =!= 1,
    Return[Missing["NoRankOneCurrentColorFlow"]]];
  fund = Flatten[Position[representations,3]];
  anti = Flatten[Position[representations,-3]];
  adjoint = Flatten[Position[representations,8]];
  If[Length[fund] =!= 1 || Length[anti] =!= 1 || Length[adjoint] > 1 ||
      Count[representations,1] =!= 2-Length[adjoint],
    Return[Missing["NoFundamentalCurrentColorFlow"]]];
  canonical = If[adjoint === {},
    ColorDelta[ColorIndex[First[anti]],ColorIndex[First[fund]]],
    ColorGenerator[ColorIndex[First[anti]],ColorIndex[First[fund]],
      ColorIndex[First[adjoint]]]];
  tensors = colorDiagramTensor[#,model] & /@ diagrams;
  If[tensors === {} || AnyTrue[tensors,FailureQ],
    Return[Missing["UnverifiedCurrentColorFlow"]]];
  basis = colorTensorBasis[Join[data["Basis"],{canonical},tensors],representations];
  If[FailureQ[basis] || Length[basis["Basis"]] =!= 1,
    Return[Missing["NonuniqueCurrentColorFlow"]]];
  coordinates = First /@ basis["DiagramCoordinates"];
  common = Drop[coordinates,2];
  If[common === {} || !TrueQ[SameQ @@ common] || TrueQ[First[common] === 0],
    Return[Missing["DifferentDiagramColorFlows"]]];
  <|"Certified"->True,"Method"->"ExactRankOneFundamentalColorFlow",
    "Basis"->First[data["Basis"]],"CanonicalTensor"->canonical,
    "CanonicalCoordinate"->coordinates[[2]],
    "CommonDiagramCoordinate"->First[common],"DiagramCoordinates"->common,
    "Representations"->representations|>
];

(* A field convention multiplies the complete amplitude by one phase, not
   each color coefficient independently. Opposite relative color signs must
   therefore remain failures even when the separate magnitudes agree. *)
colorFeynmanSignAgreement[checks_List] := Module[{rows, signs},
  rows = Select[checks, Lookup[#, "Type", None] === "Feynman" &];
  If[rows === {} || !AllTrue[rows, MemberQ[{"PASS", "Opp Sign"},
      Lookup[#, "Status", None]] && ListQ[Lookup[#, "MatchingSigns", None]] &],
    Return[<|"Status" -> "NotEstablished", "MatchingSigns" -> {}|>]];
  signs = Fold[Intersection, {1, -1}, Lookup[rows, "MatchingSigns"]];
  <|"Status" -> If[signs === {}, "FAIL", "PASS"], "MatchingSigns" -> signs,
    "Scope" -> "OneOverallSignForEveryIndependentColorCoefficient"|>
];

(* Identical external states act on the whole color vector, not on its
   individual scalar coefficients. All matrices below are exact SU(3) data. *)
colorOrbitZeroQ[value_] := TrueQ[value === 0] || TrueQ[Cancel[Together[value]] === 0];
colorOrbitMatrixEqualQ[left_List, right_List] := Dimensions[left] === Dimensions[right] &&
  AllTrue[Flatten[left - right], colorOrbitZeroQ];

colorOrbitStateGroups[process_List, model_List] := Module[{rows, keys},
  rows = colorParticleRow[#, model] & /@ process;
  If[!AllTrue[rows, ListQ[#] && Length[#] >= 8 && IntegerQ[#[[5]]] &],
    Return[Missing["UnsupportedExternalState"]]];
  (* The literal state name distinguishes helicities sharing one model row. *)
  keys = MapIndexed[{#1[[2]], #1[[3]], process[[First[#2]]]} &, rows];
  <|"Rows" -> rows, "Groups" -> Select[GatherBy[Range[Length[process]], keys[[#]] &],
    Length[#] > 1 &]|>
];

colorOrbitRelabel[tensor_, permutation_List] := tensor /.
  ColorIndex[leg_Integer] /; 1 <= leg <= Length[permutation] :>
    ColorIndex[permutation[[leg]]];

colorOrbitPlan[process_List, model_List, data_Association, representations_List,
    seconds_: 12] := Module[
  {basis = Lookup[data, "Basis", {}], gram = Lookup[data, "GramMatrix", {}],
   state, groups, particleRows, rank, count = Length[process], generators,
   vectors, representation, result, generatorData, actions, rows = {},
   permutations = {}, grades = {}, rowSeeds = {}, seeds = {}, attempts = {},
   cursor = 1, row, permutation, grade, seed, direct, inverse, outcome},
  rank = Length[basis];
  If[count < 5 || rank < 2 || rank > 16 || Dimensions[gram] =!= {rank, rank} ||
      Length[representations] =!= count || !TrueQ[seconds > 0],
    Return[Missing["ColorOrbitNotApplicable"]]];
  state = colorOrbitStateGroups[process, model];
  If[!AssociationQ[state], Return[state]];
  groups = state["Groups"]; particleRows = state["Rows"];
  If[groups === {}, Return[Missing["NoIdenticalExternalStates"]]];
  outcome = TimeConstrained[Catch[
    generators = Flatten[Table[Table[With[{a = group[[j]], b = group[[j + 1]]},
      <|"Permutation" -> ReplacePart[Range[count], {a -> b, b -> a}],
        "Legs" -> {a, b}, "Spin2" -> particleRows[[a, 5]],
        "FermionGrade" -> If[OddQ[Abs[particleRows[[a, 5]]]], -1, 1]|>],
      {j, Length[group] - 1}], {group, groups}], 1];
    If[!AllTrue[generators, representations[[#["Permutation"]]] === representations &],
      Throw[Missing["ExternalRepresentationPermutation"]]];
    vectors = colorTensorVectorSparse[#, representations] & /@ basis;
    If[AnyTrue[vectors, FailureQ], Throw[Missing["UnsupportedSparseColorVector"]]];
    representation[p_List] := representation[p] = Module[
      {images, projected, matrix, imageVectors, reconstructed, exact, gramInvariant},
      images = colorOrbitRelabel[#, p] & /@ basis;
      projected = colorTensorBasis[Join[basis, images], representations, 65536, False];
      If[!AssociationQ[projected] || projected["Basis"] =!= basis,
        Return[Missing["ColorPermutationLeavesBasis"]]];
      matrix = Take[projected["DiagramCoordinates"], -rank];
      imageVectors = colorTensorVectorSparse[#, representations] & /@ images;
      If[AnyTrue[imageVectors, FailureQ], Return[Missing["UnsupportedSparseColorImage"]]];
      reconstructed = Table[Total[MapThread[#1 #2 &, {matrix[[i]], vectors}]], {i, rank}];
      exact = And @@ Table[AllTrue[SparseArray[imageVectors[[i]] - reconstructed[[i]]]["NonzeroValues"],
        colorOrbitZeroQ], {i, rank}];
      gramInvariant = colorOrbitMatrixEqualQ[Conjugate[matrix].gram.Transpose[matrix], gram];
      If[!exact || !gramInvariant || MatrixRank[matrix] =!= rank,
        Return[Missing["UncertifiedColorPermutation"]]];
      <|"Permutation" -> p, "D" -> matrix, "ExactSparseComponentClosure" -> True,
        "GramInvariant" -> True|>
    ];
    generatorData = Map[Function[g, result = representation[g["Permutation"]];
      If[!AssociationQ[result], Throw[result]];
      Join[g, result, <|"CoefficientAction" ->
        g["FermionGrade"] Transpose[Inverse[result["D"]]]|>]], generators];
    actions = Lookup[generatorData, "CoefficientAction"];
    (* A new coordinate seed is needed only after closing the current row span.
       At most rank independent rows and rank*generators edges are admitted. *)
    Do[
      If[MatrixRank[Append[rows, UnitVector[rank, seed]]] === Length[rows], Continue[]];
      AppendTo[seeds, seed]; AppendTo[rows, UnitVector[rank, seed]];
      AppendTo[permutations, Range[count]]; AppendTo[grades, 1]; AppendTo[rowSeeds, seed];
      While[cursor <= Length[rows] && Length[rows] < rank,
        Do[
          row = Together /@ (rows[[cursor]].actions[[j]]);
          permutation = generatorData[[j, "Permutation"]][[permutations[[cursor]]]];
          grade = grades[[cursor]] generatorData[[j, "FermionGrade"]];
          AppendTo[attempts, <|"ParentRow" -> cursor, "Generator" -> j,
            "Permutation" -> permutation, "RankAfter" -> MatrixRank[Append[rows, row]]|>];
          If[Last[attempts]["RankAfter"] > Length[rows],
            direct = representation[permutation];
            If[!AssociationQ[direct] || !AllTrue[
                row - grade UnitVector[rank, rowSeeds[[cursor]]].Transpose[Inverse[direct["D"]]],
                colorOrbitZeroQ], Throw[Missing["UncertifiedLiteralColorOrbit"]]];
            AppendTo[rows, row]; AppendTo[permutations, permutation];
            AppendTo[grades, grade]; AppendTo[rowSeeds, rowSeeds[[cursor]]]];
          If[Length[rows] === rank, Break[]], {j, Length[actions]}];
        cursor++];
      If[Length[rows] === rank, Break[]], {seed, rank}];
    If[Length[seeds] >= rank || Dimensions[rows] =!= {rank, rank},
      Throw[Missing["NoColorCoefficientReuse"]]];
    inverse = Inverse[rows];
    If[!colorOrbitMatrixEqualQ[rows.inverse, IdentityMatrix[rank]] ||
        !colorOrbitMatrixEqualQ[inverse.rows, IdentityMatrix[rank]],
      Throw[Missing["UncertifiedColorOrbitInverse"]]];
    <|"Status" -> "AVAILABLE", "Method" -> "ExactGradedColorRepresentationOrbit",
      "Process" -> process, "Basis" -> basis, "Representations" -> representations,
      "IdenticalStateGroups" -> groups, "Generators" -> generatorData,
      "SeedIndices" -> seeds, "Rows" -> rows, "InverseRows" -> inverse,
      "RowSeeds" -> rowSeeds, "Permutations" -> permutations, "FermionGrades" -> grades,
      "Attempts" -> attempts, "ExactMatrixInverseVerified" -> True,
      "NoAmplitudeOrBenchmarkCoefficientInput" -> True|>
  ], Max[0.001, seconds], Missing["ColorOrbitTimeConstraint"]];
  outcome
];

colorOrbitTransport[plan_Association, seedReports_Association] := Module[
  {images, cycles, seed, report, amplitudes, rank = Length[plan["Basis"]]},
  If[Lookup[plan, "Status", None] =!= "AVAILABLE" ||
      !AllTrue[plan["SeedIndices"], KeyExistsQ[seedReports, #] &&
        AssociationQ[seedReports[#]] && Lookup[seedReports[#], "Status", None] === "COMPLETE" &],
    Return[Missing["IncompleteColorOrbitSeeds"]]];
  images = Table[seed = plan["RowSeeds"][[i]]; report = seedReports[seed];
    cycles = completionBoseCycles[Range[Length[plan["Process"]]], plan["Permutations"][[i]]];
    Fold[completionBoseCycle, report["Amplitude"], cycles], {i, rank}];
  (* The orbit row already includes the statistics grade, so its image is not
     multiplied by that grade a second time. *)
  amplitudes = plan["InverseRows"].images;
  If[!residueFiniteQ[amplitudes], Return[Missing["NonfiniteColorOrbitTransport"]]];
  <|"Status" -> "UNVALIDATED", "Amplitudes" -> amplitudes,
    "TransportedSeedImages" -> images, "InternalOnShellConditions" -> {}|>
];

(* Inverse sums can hide a simple physical bracket behind Schouten. This
   bounded pass uses only declared external rank-one momenta and exact
   polynomial identities; an unproved proposal is discarded unchanged. *)
colorOrbitPolish[expression_, external_List, seconds_: 2] := Module[
  {massless, split, normalize, angle, square, invariantRules, inverseNormalize,
   shorten, routes = {}, formal, component, candidate, proofs, outcome, started = AbsoluteTime[]},
  If[LeafCount[expression] > 8000 || !preparationScalarExternalRulesQ[external] ||
      !FreeQ[external, Mass[_Multiparticle]],
    Return[<|"Status" -> "UNCHANGED", "Amplitude" -> expression,
      "InternalOnShellConditions" -> {}|>]];
  massless = Cases[external, HoldPattern[Mass[i_Integer] -> 0] :> i];
  outcome = TimeConstrained[
    split[chain_SpinorChain] := Module[{value}, Catch[
      Do[value = rspSplitMasslessMomentum[chain, leg];
        If[!MissingQ[value], Throw[value]], {leg, massless}]; chain]];
    angle[i_, j_] := SpinorChain[Spinor["Helicity", "Angle", i], Spinor["Helicity", "Angle", j]];
    square[i_, j_] := SpinorChain[Spinor["Helicity", "Square", i], Spinor["Helicity", "Square", j]];
    invariantRules = DeleteDuplicates[Cases[expression,
      prop : PropDen[Mom[Multiparticle[i_Integer, j_Integer]], 0] /;
        MemberQ[massless, i] && MemberQ[massless, j] :> (prop -> -angle[i, j] square[i, j]), Infinity]];
    normalize[value_] := FixedPoint[(# /. chain_SpinorChain :> split[chain]) &, value, 8] /. invariantRules;
    shorten[poly_Plus] := Module[{moves, trials, best},
      If[LeafCount[poly] > 1000, Return[poly]];
      moves = TimeConstrained[Take[SchoutenCandidates[poly], UpTo[64]], 0.2, {}];
      trials = Join[{<|"Expression" -> poly, "Move" -> None|>},
        (<|"Expression" -> Expand[poly - #["Subtract"] + #["Insert"]], "Move" -> #|> & /@ moves)];
      best = First[MinimalBy[trials, LeafCount[#["Expression"]] &]];
      If[best["Expression"] =!= poly,
        AppendTo[routes, <|"Before" -> poly, "After" -> best["Expression"], "Move" -> best["Move"]|>]];
      best["Expression"]];
    inverseNormalize[value_] := value /. HoldPattern[Power[base_, exponent_Integer?Negative]] :>
      Power[(base /. poly_Plus :> shorten[poly]), exponent];
    candidate = normalize[expression] /. HoldPattern[Power[base_, exponent_Integer?Negative]] :>
      Power[Factor[ReduceSpinorProducts[Expand[base]]], exponent];
    candidate = ReduceSpinorProducts[Cancel[Together[candidate]]];
    candidate = FixedPoint[Cancel[Together[inverseNormalize[#]]] &, candidate, 4];
    formal[value_] := normalize[value] /. HoldPattern[
      SpinorChain[Spinor["Helicity", kind : ("Angle" | "Square"), i_Integer],
        Spinor["Helicity", kind_, j_Integer]]] :>
      (component[kind, i, 1] component[kind, j, 2] - component[kind, i, 2] component[kind, j, 1]);
    proofs = TrueQ[Cancel[Together[Expand[formal[candidate] - formal[expression]]]] === 0];
    If[!proofs || !residueFiniteQ[candidate] || LeafCount[candidate] > LeafCount[expression],
      <|"Status" -> "UNCHANGED", "Amplitude" -> expression,
        "ExactIdentityVerified" -> proofs, "InternalOnShellConditions" -> {}|>,
      <|"Status" -> If[candidate === expression, "UNCHANGED", "REDUCED"],
        "Amplitude" -> candidate, "ExactIdentityVerified" -> True,
        "ProofMethod" -> "ExternalRankOneSplittingAndIndependentTwoComponentPolynomialIdentity",
        "ExternalMassRules" -> external, "PhysicalInvariantRules" -> invariantRules,
        "OfficialSchoutenRoutes" -> routes, "InternalOnShellConditions" -> {},
        "BeforeLeaves" -> LeafCount[expression], "AfterLeaves" -> LeafCount[candidate]|>],
    Max[0.001, seconds], <|"Status" -> "UNCHANGED", "Amplitude" -> expression,
      "Reason" -> "ColorOrbitPolishTimeConstraint", "InternalOnShellConditions" -> {}|>];
  Join[outcome, <|"ElapsedTime" -> AbsoluteTime[] - started|>]
];

colorOrbitValidate[process_List, amplitudes_List, external_List, data_Association,
    model_List, settings_Association, deadline_] := Module[
  {bag = constructRuleOptions[settings["CheckOptions"]], common, baseParameters,
   assignments, checkSettings, overrides, report, reports = {}, byCoefficient,
   passed = True, pair, benchmark, seedBase, seconds, combined, phase, began = AbsoluteTime[]},
  If[!MemberQ[{Automatic, True}, Lookup[bag, "HighEnergyCheck", Automatic]] ||
      !MemberQ[{Automatic, True}, Lookup[bag, "ReferenceCheck", Automatic]] ||
      Lookup[bag, "SpuriousPoles", Automatic] =!= Automatic,
    Return[<|"Status" -> "NOT_SELECTED", "Reason" -> "DisabledConstructionChecks"|>]];
  baseParameters = Lookup[bag, "ParameterRules", settings["ParameterRules"]];
  assignments = completionValidationRuleSets[model, baseParameters,
    Lookup[constructRuleOptions[settings["HighEnergyOptions"]], "ValidationParameterRules", Automatic]];
  If[!ListQ[assignments] || assignments === {},
    Return[<|"Status" -> "NOT_SELECTED", "Reason" -> "InvalidValidationAssignments"|>]];
  common = KeyTake[settings, {"ParameterRules", "Samples", "HighEnergyBound", "Parallelization", "MaxKernels"}];
  common = Join[common, <|"HighEnergySamples" -> settings["Samples"],
    "ReferenceSamples" -> settings["Samples"], "FeynmanSamples" -> settings["Samples"]|>];
  seedBase = Lookup[bag, "RandomSeed", settings["RandomSeed"] + 2000003] /.
    Automatic -> settings["RandomSeed"] + 2000003;
  Do[
    overrides = constructValidationOverrides[model, baseParameters, assignments[[parameter]]];
    If[overrides === $Failed, passed = False; Break[]];
    Do[
      seconds = Max[0, deadline - AbsoluteTime[]];
      If[seconds <= 0.001, passed = False; Break[]];
      pair = {external, amplitudes[[coefficient]]};
      benchmark = If[settings["FeynmanModel"] === None, None,
        data["ProjectedFeynmanAmplitudes"][[coefficient]]];
      checkSettings = Join[<|"TimeConstraint" -> Automatic|>, common, bag,
        <|"Output" -> "Details", "SpuriousPoles" -> Automatic, "HighEnergyCheck" -> True,
          "ReferenceCheck" -> True, "ParameterRules" -> overrides,
          "RandomSeed" -> seedBase + 104729 (parameter - 1) + 7919 (coefficient - 1)|>,
        If[benchmark === None, <||>, <|"FeynmanAmplitudes" -> benchmark,
          "FeynmanModel" -> settings["FeynmanModel"]|>]];
      checkSettings = amplitudeAutomaticSettings["Checks", checkSettings,
        amplitudeResourceMetrics[pair, data["ProjectedChannels"][[coefficient]], process]]["Settings"];
      Do[If[Lookup[checkSettings, name, None] === Automatic,
        checkSettings[name] = Lookup[checkSettings, "Samples", 4]],
        {name, {"HighEnergySamples", "ReferenceSamples", "FeynmanSamples"}}];
      checkSettings["TimeConstraint"] = Min[checkSettings["TimeConstraint"], seconds];
      If[TrueQ[settings["ProgressReporting"]], Print["Validating color symmetry coefficient ",
        coefficient, "/", Length[amplitudes], "; parameter set ", parameter, "/", Length[assignments]]];
      report = TimeConstrained[Block[{$colorCheckingProjection = True},
        CheckAmplitudes[pair, data["ProjectedChannels"][[coefficient]], model,
          Sequence @@ Normal[checkSettings]]], Max[0.000001, seconds],
        Missing["ColorOrbitValidationTimeConstraint"]];
      AppendTo[reports, <|"ColorCoefficient" -> coefficient, "ParameterSetIndex" -> parameter,
        "ParameterRules" -> assignments[[parameter]], "CheckParameterOverrides" -> overrides,
        "CheckOptions" -> checkSettings, "Report" -> report|>];
      If[!constructAllChecksPassQ[report, pair, data["ProjectedChannels"][[coefficient]], model, benchmark],
        passed = False; Break[]], {coefficient, Length[amplitudes]}];
    If[!passed, Break[]], {parameter, Length[assignments]}];
  passed = passed && Length[reports] === Length[assignments] Length[amplitudes];
  If[!passed, Return[<|"Status" -> "NOT_SELECTED", "ValidationReports" -> reports,
    "ExpectedReports" -> Length[assignments] Length[amplitudes], "ElapsedTime" -> AbsoluteTime[] - began|>]];
  combined = Flatten[Lookup[Lookup[reports, "Report"], "Checks"], 1];
  phase = colorFeynmanSignAgreement[combined];
  If[Lookup[phase, "Status", None] === "FAIL" ||
      settings["FeynmanModel"] =!= None && Lookup[phase, "Status", None] =!= "PASS",
    Return[<|"Status" -> "NOT_SELECTED", "ValidationReports" -> reports,
      "FeynmanPhaseAgreement" -> phase, "ElapsedTime" -> AbsoluteTime[] - began|>]];
  byCoefficient = Table[With[{selected = Select[reports, #["ColorCoefficient"] === i &]},
    <|"Status" -> "COMPLETE", "Amplitude" -> amplitudes[[i]],
      "AmplitudePair" -> {external, amplitudes[[i]]}, "Checks" -> First[selected]["Report"],
      "ColorOrbitValidationReports" -> selected,
      "ConstructionMethod" -> "ExactGradedColorOrbitTransport"|>], {i, Length[amplitudes]}];
  <|"Status" -> "COMPLETE", "CoefficientReports" -> byCoefficient,
    "ValidationReports" -> reports, "FeynmanPhaseAgreement" -> phase,
    "ElapsedTime" -> AbsoluteTime[] - began|>
];

colorConstructAmplitude[process_List, diagramModel_List, factorizationModel_List,
    settings_Association] := Module[
  {started = AbsoluteTime[], diagrams, pairs, channels, reps, data, reports = {},
   options, report, amplitudes, expression, external, summary, output = settings["Output"],
   deadline, seconds, rank, projected, combinedChecks, checkRows,
   callback = settings["ProgressFunction"], tag = Unique["colorConstruction"], zeroResult,
   currentColorCertificate, feynmanModel = Lookup[settings,"FeynmanModel",None],
   feynmanDiagrams, feynmanRaw, feynmanPrepared, feynmanPairs = {}, benchmarkSettings,
   benchmarkSeconds, phaseAgreement, constructCoefficient, coefficientCache = <||>,
   orbitPlan, orbitTransport, orbitPolishReports = {}, orbitValidation,
   orbitAttempt = <|"Attempted" -> False|>, orbitSelected = False},
  Catch[
  seconds = Replace[settings["TimeConstraint"], Automatic ->
    amplitudeAutomaticSettings["Construction", settings,
      amplitudeConstructionResourceMetrics[process,factorizationModel]]["Settings"]["TimeConstraint"]];
  deadline = started + seconds;
  diagrams = CreateDiagrams[process, diagramModel];
  If[FailureQ[diagrams], Return[diagrams]];
  pairs = DiagramAmplitudes[diagrams];
  channels = FactorizationAmplitudeChannels[process, factorizationModel];
  If[FailureQ[pairs] || FailureQ[channels], Return[SelectFirst[{pairs, channels}, FailureQ]]];
  reps = colorParticleRepresentation[#, diagramModel] & /@ process;
  If[AnyTrue[reps, FailureQ], Return[SelectFirst[reps, FailureQ]]];
  If[feynmanModel =!= None,
    If[TrueQ[settings["ProgressReporting"]], Print["Building independent colored Feynman benchmark."]];
    feynmanDiagrams = CreateDiagrams[process,feynmanModel];
    If[FailureQ[feynmanDiagrams], Return[feynmanDiagrams]];
    feynmanRaw = DiagramAmplitudes[feynmanDiagrams];
    If[FailureQ[feynmanRaw] || feynmanRaw === $Failed, Return[feynmanRaw]];
    If[feynmanRaw === {}, feynmanRaw = {{Thread[(Mass /@ Range[Length[process]]) ->
      constructExternalMasses[process,feynmanModel]],0}}];
    benchmarkSettings = Join[
      Association[Select[Normal[settings],MemberQ[First /@ Options[PrepareAmplitudes],First[#]] &]],
      constructRuleOptions[settings["FeynmanPreparationOptions"]],
      <|"ReturnDetails"->True,"Output"->"Amplitudes","InternalOnShell"->False,
        "ExternalParticles"->Automatic|>];
    benchmarkSeconds = Replace[Lookup[benchmarkSettings,"TimeConstraint",Automatic],
      Automatic -> Max[0.001,deadline-AbsoluteTime[]]];
    benchmarkSettings["TimeConstraint"] = Min[benchmarkSeconds,Max[0.001,deadline-AbsoluteTime[]]];
    If[Length[settings["FeynmanPreparationOptions"]]===0,
      (* A benchmark needs an exact off-shell expression, not a costly
         simplification search. Shared per-leg Xi references remain literal
         for the independent full-amplitude checker. *)
      feynmanPairs = ConstructiveDiagrams`FeynmanToSpinorProducts[feynmanRaw,
        "ReduceSpinorProducts"->False,"TimeConstraint"->benchmarkSettings["TimeConstraint"]];
      If[!ListQ[feynmanPairs] || !AllTrue[feynmanPairs,residueAmplitudePairQ] ||
          !residueFiniteQ[feynmanPairs],
        Return[colorFailure["ColoredFeynmanConversion",
          "The independent Feynman benchmark did not convert completely.",
          <|"Report"->feynmanPairs|>]]];
      feynmanPrepared = <|"Status"->"PREPARED","PreparedAmplitudes"->feynmanPairs,
        "Method"->"ExactOffShellConversionWithoutReduction","InternalOnShell"->False|>,
      feynmanPrepared = PrepareAmplitudes[feynmanRaw,feynmanModel,
        Sequence @@ Normal[benchmarkSettings]];
      If[!constructPreparationUsableQ[feynmanPrepared],
        Return[colorFailure["ColoredFeynmanPreparation",
          "The independent Feynman benchmark did not prepare completely.",
          <|"Report"->feynmanPrepared|>]]];
      feynmanPairs = feynmanPrepared["PreparedAmplitudes"]]];
  If[FreeQ[{pairs,channels,feynmanPairs}, _ColorDelta | _ColorGenerator | _ColorStructure | _ColorTrace] &&
      AllTrue[Last /@ pairs, TrueQ[# === 0] &] &&
      AllTrue[Last /@ channels, TrueQ[# === 0] &],
    external = Thread[(Mass /@ Range[Length[process]]) -> constructExternalMasses[process,diagramModel]];
    projected = Join[<|"Diagrams" -> diagrams, "DiagramAmplitudes" -> {{external,0}}, "Channels" -> channels|>,
      If[feynmanModel===None,<||>,<|"FeynmanAmplitudes"->feynmanPairs|>]];
    zeroResult = Block[{$colorConstructingProjection = True, $colorConstructionInput = projected},
      ConstructAmplitude[process,diagramModel,factorizationModel,Sequence @@ Normal[settings]]];
    Return[zeroResult]];
  data = colorProjectionData[pairs, channels, reps, feynmanPairs];
  If[FailureQ[data], Return[data]];
  rank = Length[data["Basis"]];
  If[rank === 0,
    external = Thread[(Mass /@ Range[Length[process]]) -> constructExternalMasses[process,diagramModel]];
    projected = Join[<|"Diagrams" -> diagrams, "DiagramAmplitudes" -> {{external,0}},
      "Channels" -> (Append[Most[#],0] & /@ channels)|>,
      If[feynmanModel===None,<||>,<|"FeynmanAmplitudes"->feynmanPairs|>]];
    Return[Block[{$colorConstructingProjection = True, $colorConstructionInput = projected},
      ConstructAmplitude[process,diagramModel,factorizationModel,Sequence @@ Normal[settings]]]]];
  currentColorCertificate = colorScalarCurrentProjectionCertificate[
    diagrams,diagramModel,data,reps];
  If[TrueQ[settings["ProgressReporting"]], Print["Constructing ", rank,
    " independent color coefficients."]];
  reports = ConstantArray[Missing["NotConstructed"], rank];
  constructCoefficient[index_Integer] := Module[{coefficientOptions, coefficientInput, value},
    If[KeyExistsQ[coefficientCache, index], Return[coefficientCache[index]]];
    If[AbsoluteTime[] >= deadline, Return[Missing["ColorConstructionTimeConstraint"]]];
    coefficientOptions = Join[settings, <|"Output" -> "Details", "PrintReport" -> False,
      "BoseSymmetryGroups" -> {}, "FermionSymmetryGroups" -> {},
      "TimeConstraint" -> Max[0.001, deadline - AbsoluteTime[]]|>];
    If[TrueQ[settings["ProgressReporting"]], Print["Color coefficient ", index, "/", rank]];
    coefficientInput = <|"Diagrams" -> diagrams,
      "DiagramAmplitudes" -> data["ProjectedDiagrams"][[index]],
      "Channels" -> data["ProjectedChannels"][[index]],
      "ColorProjectionCertificate" -> currentColorCertificate|>;
    If[feynmanModel =!= None, AssociateTo[coefficientInput,
      "FeynmanAmplitudes" -> data["ProjectedFeynmanAmplitudes"][[index]]]];
    value = Block[{$colorConstructingProjection = True, $colorConstructionInput = coefficientInput},
      ConstructAmplitude[process, diagramModel, factorizationModel,
        Sequence @@ Normal[coefficientOptions]]];
    AssociateTo[coefficientCache, index -> value]; reports[[index]] = value; value
  ];
  orbitPlan = If[settings["BoseSymmetryGroups"] === Automatic &&
      settings["FermionSymmetryGroups"] === Automatic,
    colorOrbitPlan[process, diagramModel, data, reps,
      Min[12, Max[0.001, (deadline - AbsoluteTime[])/10]]],
    Missing["ExplicitSymmetryOverride"]];
  If[AssociationQ[orbitPlan] && Lookup[orbitPlan, "Status", None] === "AVAILABLE",
    orbitAttempt = <|"Attempted" -> True, "Selected" -> False, "Plan" -> orbitPlan|>;
    If[TrueQ[settings["ProgressReporting"]], Print["Using identical-particle color symmetry: ",
      Length[orbitPlan["SeedIndices"]], " construction seeds for ", rank, " coefficients."]];
    Do[report = constructCoefficient[index];
      If[!AssociationQ[report] || Lookup[report, "Status", None] =!= "COMPLETE", Break[]],
      {index, orbitPlan["SeedIndices"]}];
    orbitTransport = colorOrbitTransport[orbitPlan, coefficientCache];
    AssociateTo[orbitAttempt, {"OriginalSeedReports" -> coefficientCache,
      "Transport" -> orbitTransport}];
    If[AssociationQ[orbitTransport],
      external = First[coefficientCache[First[orbitPlan["SeedIndices"]]]["AmplitudePair"]];
      orbitPolishReports = Map[colorOrbitPolish[#, external,
        Min[2, Max[0.001, (deadline - AbsoluteTime[])/(2 rank)]]] &, orbitTransport["Amplitudes"]];
      amplitudes = Lookup[orbitPolishReports, "Amplitude"];
      AssociateTo[orbitAttempt, {"ExternalIdentityPolish" -> orbitPolishReports,
        "FixedCandidateAmplitudes" -> amplitudes}];
      orbitValidation = colorOrbitValidate[process, amplitudes, external, data,
        factorizationModel, settings, deadline];
      AssociateTo[orbitAttempt, "Validation" -> orbitValidation];
      If[Lookup[orbitValidation, "Status", None] === "COMPLETE",
        reports = orbitValidation["CoefficientReports"]; orbitSelected = True;
        AssociateTo[orbitAttempt, "Selected" -> True]]]];
  If[!orbitSelected,
    Do[
      If[AbsoluteTime[] >= deadline, Throw[colorFailure["ColorConstructionTimeConstraint",
        "The shared color construction clock expired before every coefficient was validated.",
        <|"CompletedColorCoefficients" -> Count[reports, item_Association /;
            Lookup[item, "Status", None] === "COMPLETE"], "ColorCoefficients" -> rank,
          "ColorBasis" -> data["Basis"], "ColorCoefficientReports" -> reports,
          "ColorOrbitAttempt" -> orbitAttempt|>], tag]];
      report = constructCoefficient[i];
      If[!AssociationQ[report] || Lookup[report, "Status", None] =!= "COMPLETE",
        Throw[colorFailure["IncompleteColorAmplitude",
          "An independent color coefficient did not complete. No full color amplitude was returned.",
          <|"ColorCoefficient" -> i, "ColorBasis" -> data["Basis"], "Report" -> report,
            "ColorCoefficientReports" -> reports, "ColorOrbitAttempt" -> orbitAttempt|>], tag]],
      {i, rank}]];
  amplitudes = Lookup[reports, "Amplitude"];
  expression = data["Basis"].amplitudes;
  external = First[First[reports]["AmplitudePair"]];
  summary = Merge[Lookup[Lookup[reports, "Checks"], "Summary"], Total];
  combinedChecks = Flatten[MapIndexed[Function[{item,index},
    (Join[#, <|"ColorCoefficient" -> First[index]|>] &) /@ item["Checks"]["Checks"]], reports],1];
  phaseAgreement = colorFeynmanSignAgreement[combinedChecks];
  If[Lookup[phaseAgreement, "Status", None] === "FAIL" ||
      feynmanModel =!= None && Lookup[phaseAgreement, "Status", None] =!= "PASS",
    Throw[colorFailure["InconsistentFeynmanColorPhases",
      "The independent color coefficients require different Feynman signs. No common overall phase relates the full amplitudes.",
      <|"ColorBasis" -> data["Basis"], "Reports" -> reports,
        "ColorOrbitAttempt" -> orbitAttempt, "PhaseAgreement" -> phaseAgreement|>], tag]];
  checkRows = Map[Function[row, <|"Color" -> row["ColorCoefficient"],
    "Type" -> row["Type"], "Test" -> row["Pole"], "Status" -> row["Status"]|>], combinedChecks];
  report = <|"Status" -> "COMPLETE", "Process" -> process, "Amplitude" -> expression,
    "AmplitudePair" -> {external, expression}, "TotalTime" -> AbsoluteTime[] - started,
    "Complexity" -> constructComplexity[expression], "ColorBasis" -> data["Basis"],
    "ColorGramMatrix" -> data["GramMatrix"], "ColorCoefficientReports" -> reports,
    "ColorOrbitAttempt" -> orbitAttempt,
    "FeynmanModel" -> feynmanModel,
    "FeynmanPreparation" -> If[feynmanModel===None,Missing["NotRequested"],feynmanPrepared],
    "Checks" -> <|"Summary" -> summary, "Checks" -> combinedChecks,
      "FeynmanPhaseAgreement" -> phaseAgreement,
      "Table" -> Dataset[checkRows, MaxItems -> {All,All}],
      "Scope" -> "EveryIndependentColorCoefficient"|>|>;
  If[TrueQ[settings["PrintReport"]], Print["Total time: ", Round[report["TotalTime"], 0.1], " s"];
    Print["Complexity: ", report["Complexity"]]; Print[report["Checks"]["Table"]]];
  Switch[output, "Details", report, "Amplitude", report["AmplitudePair"], _, expression],tag]
];

colorCheckAmplitudes[amplitudes_, channels_List, model_List, supplied_Association] := Module[
  {normalized, external, indices, reps, candidates, tensors, data, reports, options,
   output, checks, rows, combined, summary, report, benchmarkPairs = {},
   benchmarkEnabled, coefficientOptions, benchmarkSeconds, phaseAgreement},
  options = Join[Association[Options[CheckAmplitudes]], supplied]; output = options["Output"];
  benchmarkEnabled = options["FeynmanAmplitudes"] =!= None;
  If[benchmarkEnabled,
    benchmarkSeconds = Replace[options["FeynmanTimeConstraint"],Automatic -> options["TimeConstraint"]];
    benchmarkPairs = colorFeynmanPairs[options["FeynmanAmplitudes"],benchmarkSeconds];
    If[FailureQ[benchmarkPairs],Return[benchmarkPairs]]];
  normalized = completionNormalizeInput[amplitudes, channels, model];
  If[FailureQ[normalized] || !AssociationQ[normalized], Return[normalized]];
  external = normalized["ExternalMassRules"];
  tensors = Cases[{amplitudes, channels, benchmarkPairs}, _ColorDelta | _ColorGenerator | _ColorStructure | _ColorTrace, Infinity];
  indices = DeleteDuplicates[Cases[tensors, ColorIndex[i_Integer] :> i, Infinity]];
  reps = Table[If[!MemberQ[indices, i], 1,
    candidates = DeleteDuplicates[Abs[#[[8]]] & /@ Select[model[[1]],
      Length[#] >= 8 && #[[8]] =!= 1 &&
        TrueQ[#[[6]] === (Mass[i] /. external)] &]];
    If[Length[candidates] === 1, First[candidates],
      Return[colorFailure["AmbiguousExternalColor",
        "External mass metadata does not uniquely determine an explicit color index's representation.",
        <|"Leg" -> i|>]]]], {i, Length[external]}];
  data = colorProjectionData[{{external, normalized["Expression"]}}, channels, reps, benchmarkPairs];
  If[FailureQ[data], Return[data]];
  If[data["Basis"] === {}, Return[Block[{$colorCheckingProjection = True},
    CheckAmplitudes[{external,0},Append[Most[#],0] & /@ channels,model,
      Sequence @@ Normal[supplied]]]]];
  options["Output"] = "Details";
  reports = Table[
    coefficientOptions = If[benchmarkEnabled,
      Join[options,<|"FeynmanAmplitudes"->data["ProjectedFeynmanAmplitudes"][[i]]|>],options];
    Block[{$colorCheckingProjection = True},
      CheckAmplitudes[First[data["ProjectedDiagrams"][[i]]],
        data["ProjectedChannels"][[i]], model, Sequence @@ Normal[coefficientOptions]]],
    {i, Length[data["Basis"]]}];
  If[AnyTrue[reports, FailureQ[#] || !AssociationQ[#] &],
    Return[colorFailure["ColorChecks", "An independent color coefficient could not be checked.",
      <|"Reports" -> reports|>]]];
  checks = Flatten[MapIndexed[Function[{item,index},
    (Join[#, <|"ColorCoefficient" -> First[index]|>] &) /@
      Lookup[item,"Checks", (Join[#, <|"Type" -> "Physical",
        "Pole" -> Lookup[#, "CutCondition", Lookup[#, "Cut", Missing["UnspecifiedCut"]]]|>] &) /@
          Lookup[item,"Channels",{}]]], reports],1];
  phaseAgreement = colorFeynmanSignAgreement[checks];
  If[Lookup[phaseAgreement, "Status", None] === "FAIL",
    AppendTo[checks, <|"Type" -> "Feynman", "Pole" -> "Common overall color phase",
      "ColorCoefficient" -> All, "Status" -> "FAIL", "MatchingSigns" -> {},
      "Diagnostics" -> {"Different signs on independent color coefficients change their coherent interference; this is not an overall amplitude phase."}|>]];
  summary = Counts[Lookup[checks,"Status",{}]];
  rows = Map[Function[row, <|"Color" -> row["ColorCoefficient"],
    "Type" -> row["Type"], "Test" -> row["Pole"], "Status" -> row["Status"]|>], checks];
  report = <|"ColorBasis" -> data["Basis"], "ColorGramMatrix" -> data["GramMatrix"],
    "ColorCoefficientReports" -> reports, "Checks" -> checks, "Summary" -> summary,
    "Table" -> Dataset[rows, MaxItems -> {All,All}], "FeynmanPhaseAgreement" -> phaseAgreement,
    "Scope" -> "EveryIndependentColorCoefficient"|>;
  Switch[output, "Details", report, "Summary", summary, "Table", report["Table"],
    _, Dataset[MapIndexed[Function[{item,index}, <|"Color" -> First[index],
      "Result" -> Switch[output,
        "Residues", residueUnifiedSamples[item["Channels"],Lookup[item,"SpuriousChecks",{}]],
        "HighEnergy", item["HighEnergyTable"], "References", item["ReferenceTable"],
        "Feynman", Lookup[item,"FeynmanTable",Dataset[{}]],
        _, Missing["NotAvailable"]]|>], reports], MaxItems -> {All,All}]]
];

(* ::Section::Closed:: *)
(*Color Tensor Display*)

(* Display carries no contraction or model lookup. External indices use a
   neutral c label, while dimensioned internal dummies distinguish fundamental
   and adjoint sums. InterpretationBox preserves the exact typed expression. *)
colorDisplayProtectedSymbols = Select[
  {ColorIndex, ColorDelta, ColorGenerator, ColorStructure,
    ConstructiveDiagrams`ColorTrace, ConstructiveDiagrams`nice},
  MemberQ[Attributes[#], Protected] &];
Unprotect @@ colorDisplayProtectedSymbols;

colorDisplayLabel[label_List] := Row[Riffle[colorDisplayLabel /@ label, ","]];
colorDisplayLabel[label_Symbol] := SymbolName[Unevaluated[label]];
colorDisplayLabel[label_] := label;
colorDisplayIndex[index : (_ColorIndex | _colorInternalIndex)] := nice[index];
colorDisplayIndex[index_] := colorDisplayLabel[index];

nice[ColorIndex[label_]] := Subscript["c", colorDisplayLabel[label]];
nice[colorInternalIndex[label_, dimension_Integer]] :=
  Subscript[Switch[dimension, 3, "d", 8, "a", _, "c"], colorDisplayLabel[label]];
nice[ColorDelta[left_, right_]] :=
  Subscript["\[Delta]", Row[{colorDisplayIndex[left], ",", colorDisplayIndex[right]}]];
nice[ColorGenerator[anti_, fundamental_, adjoint_]] :=
  Subscript[Row[{"(", Superscript["T", colorDisplayIndex[adjoint]], ")"}],
    Row[{colorDisplayIndex[anti], ",", colorDisplayIndex[fundamental]}]];
nice[ColorStructure[first_, second_, third_]] :=
  Superscript["f", Row[Riffle[colorDisplayIndex /@ {first, second, third}, ","]]];
nice[ConstructiveDiagrams`ColorTrace[labels___]] :=
  Row[{"Tr(", Row[Riffle[(Superscript["T", colorDisplayIndex[#]] &) /@ {labels},
    "\[ThinSpace]"]], ")"}];

ColorIndex /: MakeBoxes[index : ColorIndex[_], form : (StandardForm | TraditionalForm)] :=
  With[{boxes = ToBoxes[nice[index], form]},
    InterpretationBox[boxes, index, Editable -> False]];
ColorDelta /: MakeBoxes[tensor : ColorDelta[_, _], form : (StandardForm | TraditionalForm)] :=
  With[{boxes = ToBoxes[nice[tensor], form]},
    InterpretationBox[boxes, tensor, Editable -> False]];
ColorGenerator /: MakeBoxes[tensor : ColorGenerator[_, _, _], form : (StandardForm | TraditionalForm)] :=
  With[{boxes = ToBoxes[nice[tensor], form]},
    InterpretationBox[boxes, tensor, Editable -> False]];
ColorStructure /: MakeBoxes[tensor : ColorStructure[_, _, _], form : (StandardForm | TraditionalForm)] :=
  With[{boxes = ToBoxes[nice[tensor], form]},
    InterpretationBox[boxes, tensor, Editable -> False]];
ConstructiveDiagrams`ColorTrace /: MakeBoxes[trace : ConstructiveDiagrams`ColorTrace[___],
    form : (StandardForm | TraditionalForm)] :=
  With[{boxes = ToBoxes[nice[trace], form]},
    InterpretationBox[boxes, trace, Editable -> False]];
colorInternalIndex /: MakeBoxes[index : colorInternalIndex[_, _Integer],
    form : (StandardForm | TraditionalForm)] :=
  With[{boxes = ToBoxes[nice[index], form]},
    InterpretationBox[boxes, index, Editable -> False]];

Protect @@ colorDisplayProtectedSymbols;
Clear[colorDisplayProtectedSymbols];

End[];
Protect @@ ConstructiveDiagrams`Private`$colorModuleProtected;
Clear[ConstructiveDiagrams`Private`$colorModuleProtected];
