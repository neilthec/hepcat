(* HEPCAT campaign prototype. Load HEPCAT first. No reference model or saved amplitude is read.
   Minimal covariant continuations of exactly matched elementary vertices are candidates,
   not a proof that an arbitrary theory has no independent contact interactions. *)
(* Implementation heads and calls are fully qualified so existing ambient symbols
   cannot capture definitions when this file is loaded after another package. *)
Begin["HEPCATFourFermionVectorPrototype`Private`"];
HEPCATFourFermionVectorPrototype`Private`rowParticles[row_List] := DeleteCases[Drop[row, -2], ""];
HEPCATFourFermionVectorPrototype`Private`particleRow[name_, model_List] := SelectFirst[model[[1]], MemberQ[#[[{2, 3}]], name] &, Missing["UnknownParticle"]];
HEPCATFourFermionVectorPrototype`Private`objectSHA[value_] := IntegerString[Hash[value, "SHA256"], 16, 64];
HEPCATFourFermionVectorPrototype`Private`reject[tag_String, data_: <||>] := Failure[tag, data];
(* Minimal-current coefficients may depend on model-owned scalars, never momenta,
   spinors, reference labels or undeclared ambient symbols. This positive grammar
   also covers kinematic heads added to HEPCAT in future. *)
HEPCATFourFermionVectorPrototype`Private`modelScalarQ[value_, model_List] := Module[{declared = First /@ Join[model[[3]], model[[4]]], scalar},
 scalar[x_] := Which[
  MemberQ[declared, x], True,
  IntegerQ[x] || Head[x] === Rational, True,
  Head[x] === Complex, AllTrue[List @@ x, scalar],
  MemberQ[{Pi, E}, x], True,
  MemberQ[{Plus, Times, Power, Sin, Cos, Tan, ArcSin, ArcCos, ArcTan, Exp, Log}, Head[x]],
    AllTrue[List @@ x, scalar],
  True, False];
 TrueQ[scalar[value]]
];
(* Declared field metadata fixes a massless Weyl endpoint; no helicity is guessed. *)
HEPCATFourFermionVectorPrototype`Private`fermionEndpointHand[name_,model_List]:=Module[{r=HEPCATFourFermionVectorPrototype`Private`particleRow[name,model]},
 If[!ListQ[r]||r[[5]]=!=1||r[[2]]===r[[3]],Return[HEPCATFourFermionVectorPrototype`Private`reject["UnsupportedFermionEndpointMetadata"]]];
 If[r[[6]]=!=0,Return[If[MemberQ[{"L","R"},r[[9]]],HEPCATFourFermionVectorPrototype`Private`reject["InconsistentMassiveChiralEndpointMetadata"],"Dirac"]]];
 If[MemberQ[{"L","R"},r[[9]]],r[[9]],HEPCATFourFermionVectorPrototype`Private`reject["UndeclaredMasslessFermionChirality"]]
];
HEPCATFourFermionVectorPrototype`Private`supportedFermionRowQ[r_List]:=Length[r]===11&&r[[5]]===1&&r[[2]]=!=r[[3]]&&
 If[r[[6]]===0,MemberQ[{"L","R"},r[[9]]],!MemberQ[{"L","R"},r[[9]]]];
HEPCATFourFermionVectorPrototype`Private`declaredExternalSpinWeights[process_List,model_List]:=Module[
 {rows=HEPCATFourFermionVectorPrototype`Private`particleRow[#,model]&/@process,weights,hand,role},
 If[!AllTrue[rows,ListQ],Return[HEPCATFourFermionVectorPrototype`Private`reject["UnknownExternalSpinMetadata"]]];
 weights=MapThread[Function[{name,r},Which[
  r[[6]]=!=0,r[[5]],
  r[[5]]===0,0,
  r[[5]]===2&&StringEndsQ[r[[2]],".+"]&&r[[3]]===StringDrop[r[[2]],-1]<>"-",If[name===r[[2]],-2,2],
  r[[5]]===1,hand=HEPCATFourFermionVectorPrototype`Private`fermionEndpointHand[name,model];
   If[FailureQ[hand],hand,role=If[name===r[[2]],1,-1];If[hand==="L",role,-role]],
  True,HEPCATFourFermionVectorPrototype`Private`reject["UnsupportedMasslessExternalSpinMetadata"]]],{process,rows}];
 If[AnyTrue[weights,FailureQ],First[Select[weights,FailureQ]],weights]
];
HEPCATFourFermionVectorPrototype`Private`exactVertexMatch[target_,rawTemplates_List,vectorLegs_List,model_List]:=Module[
 {indices,explicitTarget,templates,activePositions,activeTemplates,activeCoefficients,atoms,variables,unknowns,polynomial,equations,solutions,coefficients,difference},
 indices=AssociationThread[vectorLegs,Table[{Unique["vectorSlot"],Unique["vectorSlot"]},{Length[vectorLegs]}]];
 explicitTarget=Fold[SymmetrizeSpin[Expand[#1],#2,indices[#2]]&,target,vectorLegs];
 explicitTarget=ConstructiveDiagrams`Private`canonicalChains[Expand[explicitTarget]];
 templates=FeynmanToSpinorProducts[#,"ReduceSpinorProducts"->False,"TimeConstraint"->10]&/@rawTemplates;
 If[AnyTrue[templates,FailureQ],Return[Failure["TemplateConversionIncomplete",<||>]]];
 templates=ConstructiveDiagrams`Private`canonicalChains[Expand[#/.VectorSpinIndex[leg_Integer,slot_Integer]:>indices[leg][[slot]]]]&/@templates;
 activePositions=Select[Range[Length[templates]],templates[[#]]=!=0&];
 If[activePositions==={},Return[Failure["NoNonzeroMinimalCurrentTemplate",<||>]]];
 activeTemplates=templates[[activePositions]];
 atoms=DeleteDuplicates[Cases[{explicitTarget,activeTemplates},_SpinorChain,Infinity]];
 variables=Table[Unique["formalSpinorAtom"],{Length[atoms]}];unknowns=Table[Unique["vertexCoefficient"],{Length[activeTemplates]}];
 polynomial=Expand[(explicitTarget-unknowns.activeTemplates)/.Thread[atoms->variables]];
 If[variables =!= {} && !PolynomialQ[polynomial,variables],Return[Failure["NonminimalVertexStructure",<||>]]];
 equations=If[variables==={},{polynomial},Last/@CoefficientRules[polynomial,variables]];
 solutions=TimeConstrained[Solve[Thread[equations==0],unknowns],10,Missing["SymbolicMatchTimeConstraint"]];
 If[!ListQ[solutions]||Length[solutions]=!=1,Return[Failure["VertexMatchNotUnique",<||>]]];
 activeCoefficients=unknowns/.First[solutions];
 coefficients=ConstantArray[0,Length[templates]];coefficients[[activePositions]]=activeCoefficients;
 If[!AllTrue[coefficients,HEPCATFourFermionVectorPrototype`Private`modelScalarQ[#,model]&],
   Return[Failure["NonModelScalarVertexCoefficients",<||>]]];
 If[!FreeQ[coefficients,_Spinor|_SpinorChain|_Mom|_MomProd|_PropDen|_Mass|_Real]||!FreeQ[coefficients,Alternatives@@unknowns],
  Return[Failure["NonScalarOrNumericalVertexCoefficients",<||>]]];
 difference=Cancel[Together[Expand[coefficients.templates-explicitTarget]]];
 If[difference=!=0,Return[Failure["SymbolicVertexMatchFailed",<||>]]];
 <|"Status"->"EXACT_SYMBOLIC_MATCH","Coefficients"->coefficients,"OriginalAmplitude"->target,
  "OriginalSymmetricTensor"->explicitTarget,"ConvertedTemplates"->templates,
  "ActiveTemplatePositions"->activePositions,"IdenticallyZeroTemplatePositions"->Complement[Range[Length[templates]],activePositions],
  "VanishingTemplatesDiscardedOnlyAfterExactConversion"->True,
  "ExactDifference"->difference,"ModelDefinitionExpandedDifference"->ConstructiveDiagrams`Private`modelExpandParameterDefinitions[difference,model],
  "NumericalParameterAssignments"->{},"InternalOnShellConditions"->{},
  "ContinuationAssumption"->"MinimalRenormalizableCovariantCurrent"|>
];
HEPCATFourFermionVectorPrototype`Private`liftVertex[row_List,model_List]:=Module[
 {parts=HEPCATFourFermionVectorPrototype`Private`rowParticles[row],rows,spins,fermions,vectors,scalars,canonical,masses,mu,word,axial,pol,raw,body,vectorLegs={},
  type,match,coefficients,liftedRow,particleNames,antiNames,target,hands},
 rows=HEPCATFourFermionVectorPrototype`Private`particleRow[#,model]&/@parts;
 If[!AllTrue[rows,ListQ]||!AllTrue[rows,#[[8]]===1&],Return[Missing["ColoredOrUnknownVertex"]]];
 spins=rows[[All,5]];fermions=Pick[parts,spins,1];vectors=Pick[parts,spins,2];scalars=Pick[parts,spins,0];
 Which[
 Length[parts]===3&&Length[fermions]===2&&Length[vectors]===1&&AllTrue[HEPCATFourFermionVectorPrototype`Private`particleRow[#,model]&/@vectors,#[[6]]=!=0&]&&AllTrue[HEPCATFourFermionVectorPrototype`Private`particleRow[#,model]&/@fermions,HEPCATFourFermionVectorPrototype`Private`supportedFermionRowQ],type="FermionFermionMassiveVector",
 Length[parts]===3&&Length[fermions]===2&&Length[scalars]===1&&AllTrue[HEPCATFourFermionVectorPrototype`Private`particleRow[#,model]&/@fermions,#[[6]]=!=0&],type="FermionFermionScalar",
 Length[parts]===3&&Length[vectors]===2&&Length[scalars]===1&&AllTrue[HEPCATFourFermionVectorPrototype`Private`particleRow[#,model]&/@vectors,#[[6]]=!=0&],type="VectorVectorScalar",
 Length[parts]===3&&Length[scalars]===3,type="ScalarCubic",
 True,Return[Missing["UnsupportedVertexClass"]]];
 If[Length[fermions]===2,
  particleNames=model[[1,All,2]];antiNames=model[[1,All,3]];
  canonical=Join[Select[fermions,MemberQ[particleNames,#]&],Select[fermions,MemberQ[antiNames,#]&],Join[vectors,scalars]];
  If[Length[canonical]=!=3||Length[DeleteDuplicates[canonical[[1;;2]]]]=!=2,Return[Missing["AmbiguousFermionRoles"]]],
  canonical=Join[vectors,scalars]];
 masses=HEPCATFourFermionVectorPrototype`Private`particleRow[#,model][[6]]&/@canonical;
 If[!AllTrue[masses,HEPCATFourFermionVectorPrototype`Private`modelScalarQ[#,model]&],Return[Missing["NonModelScalarParticleMass"]]];
 hands=If[Length[fermions]===2,HEPCATFourFermionVectorPrototype`Private`fermionEndpointHand[#,model]&/@Take[canonical,2],{}];
 If[AnyTrue[hands,FailureQ],Return[First[Select[hands,FailureQ]]]];
 pol[leg_]:=PolarizationVector[leg,LorentzIndex[leg],masses[[leg]],Automatic];
 target=Block[{ConstructiveDiagrams`Private`$colorFactorizationEnabled=False},
  LookupAmplitude[MapIndexed[{#1,First[#2]}&,canonical],ReplacePart[model,2->{row}],False,Table[Unique["vertexSpin"],{4}]]];
 Switch[type,
 "FermionFermionMassiveVector",
  word=DiracChain[FermionBar[2,masses[[2]],hands[[2]]],DiracWord[DiracGamma[LorentzIndex[3]]],FermionKet[1,masses[[1]],hands[[1]]]];
  axial=DiracChain[FermionBar[2,masses[[2]],hands[[2]]],DiracWord[DiracGamma[LorentzIndex[3]],DiracGamma5],FermionKet[1,masses[[1]],hands[[1]]]];
  raw={(word-axial)pol[3]/2,(word+axial)pol[3]/2};
  body={(DiracWord[DiracGamma[LorentzIndex[3]]]-DiracWord[DiracGamma[LorentzIndex[3]],DiracGamma5])/2,
        (DiracWord[DiracGamma[LorentzIndex[3]]]+DiracWord[DiracGamma[LorentzIndex[3]],DiracGamma5])/2};vectorLegs={3},
 "FermionFermionScalar",
  raw={DiracChain[FermionBar[2,masses[[2]],hands[[2]]],DiracWord[],FermionKet[1,masses[[1]],hands[[1]]]],
       DiracChain[FermionBar[2,masses[[2]],hands[[2]]],DiracWord[DiracGamma5],FermionKet[1,masses[[1]],hands[[1]]]]};
  body={DiracWord[],DiracWord[DiracGamma5]},
 "VectorVectorScalar",
  raw={LorentzMetric[LorentzIndex[1],LorentzIndex[2]]pol[1]pol[2]};body={LorentzMetric[LorentzIndex[1],LorentzIndex[2]]};vectorLegs={1,2},
 _,raw={1};body={1}];
 match=HEPCATFourFermionVectorPrototype`Private`exactVertexMatch[target,raw,vectorLegs,model];If[!AssociationQ[match],Return[match]];
 coefficients=match["Coefficients"];
 liftedRow=Join[If[Length[fermions]===2,Join[Reverse[Take[canonical,2]],Drop[canonical,2]],canonical],
  If[Length[canonical]===3,{""},{}],{1,coefficients.body}];
 Join[match,<|"Class"->type,"Particles"->canonical,"DeclaredFermionEndpointHands"->hands,"SourceRows"->{row},"LiftedRow"->liftedRow|>]
];

(* Only used rows enter this catalog. A row that is unsupported cannot disappear. *)
HEPCATFourFermionVectorPrototype`Private`usedRows[diagrams_List] := DeleteDuplicates[Flatten[
  (Lookup[#, "ModelRow", Missing["MissingModelRow"]] & /@
    Flatten[Values[#["VertexStructures"]], 1]) & /@ diagrams, 1]];
HEPCATFourFermionVectorPrototype`Private`sourceCatalog[model_List, rows_List] := Module[
 {certificates = {}, liftedRows = {}, aliases = <||>, used, match, row, vector,
  fermion, species, names, pairedRows, coupling, base, particles, needed, declared},
 declared = First /@ Join[model[[3]], model[[4]]];
 If[Complement[DeleteDuplicates[Cases[rows, _ModelParameter, Infinity]], declared] =!= {},
   Return[HEPCATFourFermionVectorPrototype`Private`reject["ForeignModelParameter"]]];
 If[AnyTrue[rows, !ListQ[#] || Length[HEPCATFourFermionVectorPrototype`Private`rowParticles[#]] =!= 3 &],
   Return[HEPCATFourFermionVectorPrototype`Private`reject["UnsupportedUsedVertexArity"]]];
 used = rows;
 (* Both helicities certify a minimal massless current. Aliases are metadata-derived. *)
 Do[
  If[vector[[5]] =!= 2 || vector[[6]] =!= 0 || vector[[8]] =!= 1 ||
     Intersection[vector[[{2, 3}]], Flatten[HEPCATFourFermionVectorPrototype`Private`rowParticles /@ used]] === {}, Continue[]];
  names = vector[[{2, 3}]];
  If[!StringEndsQ[First[names], ".+"] || Last[names] =!= StringDrop[First[names], -1] <> "-",
    Return[HEPCATFourFermionVectorPrototype`Private`reject["UnsupportedMasslessVectorMetadata"]]];
  base = StringDrop[First[names], -2];
  If[!ConstructiveDiagrams`Private`feynmanIdentifierQ[base] ||
      MemberQ[Flatten[model[[1, All, {2, 3}]]], base], Return[HEPCATFourFermionVectorPrototype`Private`reject["MasslessVectorAliasCollision"]]];
  AssociateTo[aliases, {First[names] -> base, Last[names] -> base}];
  Do[
   If[fermion[[5]] =!= 1 || fermion[[6]] === 0 || fermion[[8]] =!= 1, Continue[]];
   species = fermion[[{2, 3}]];
   pairedRows = Select[model[[2]], MatchQ[#, {_, _, _, "", _, _}] &&
      Take[#, 2] === species && MemberQ[names, #[[3]]] &];
   If[Intersection[pairedRows, used] === {}, Continue[]];
   coupling = ConstructiveDiagrams`Private`constructVectorCoupling[pairedRows, species, names];
   If[MissingQ[coupling] || !HEPCATFourFermionVectorPrototype`Private`modelScalarQ[coupling, model], Return[HEPCATFourFermionVectorPrototype`Private`reject["NonminimalMasslessVectorCurrent"]]];
   row = {Last[species], First[species], base, "", coupling/Sqrt[2],
      DiracWord[DiracGamma[LorentzIndex[3]]]};
   AppendTo[liftedRows, row];
   AppendTo[certificates, <|"Status" -> "EXACT_STANDARD_X_XT_STRUCTURE_MATCH",
    "Class" -> "FermionFermionMasslessVector", "Particles" -> Join[species, {base}],
    "SourceRows" -> pairedRows, "LiftedRow" -> row, "Coefficients" -> {coupling/Sqrt[2]},
    "NumericalParameterAssignments" -> {}, "InternalOnShellConditions" -> {},
    "SourceCertificateHelper" -> "constructVectorCoupling/standardFermionVectorVertexQ"|>],
   {fermion, model[[1]]}], {vector, model[[1]]}];
 Do[
  If[MemberQ[Flatten[Lookup[certificates, "SourceRows", {}], 1], row], Continue[]];
  match = HEPCATFourFermionVectorPrototype`Private`liftVertex[row, model];
  If[!AssociationQ[match], Return[HEPCATFourFermionVectorPrototype`Private`reject["UsedVertexNotExactlyMatched", <|"Row" -> row, "Reason" -> match|>]]];
  If[Length[HEPCATFourFermionVectorPrototype`Private`rowParticles[match["LiftedRow"]]] =!= 3,
    Return[HEPCATFourFermionVectorPrototype`Private`reject["UnsupportedUsedVertexArity"]]];
  AppendTo[certificates, match]; AppendTo[liftedRows, match["LiftedRow"]], {row, rows}];
 certificates = (Append[#, "CertificateID" -> HEPCATFourFermionVectorPrototype`Private`objectSHA[KeyTake[#, {"Class", "Particles", "Coefficients"}]]] &) /@ certificates;
 needed = DeleteDuplicates[Flatten[HEPCATFourFermionVectorPrototype`Private`rowParticles /@ liftedRows]];
 particles = Map[Function[p, With[{r = ReplacePart[p, 7 -> 0]},
   If[KeyExistsQ[aliases, r[[2]]], ReplacePart[r, {2 -> aliases[r[[2]]], 3 -> aliases[r[[3]]], 9 -> "G"}], r]]], model[[1]]];
 particles = Select[particles, Intersection[#[[{2, 3}]], needed] =!= {} &];
 <|"Certificates" -> certificates, "Aliases" -> aliases,
   "Model" -> FeynmanModel[particles, DeleteDuplicates[liftedRows], model[[3]], model[[4]]]|>
];
(* An ordered port is identified by the external leaves on its side of an edge.
   Hence no graph vertex numbering or composite-label ordering is assumed. *)
HEPCATFourFermionVectorPrototype`Private`portAddress[diagram_Association, vertex_, leg_Association] := Module[{edge, graph, component},
 If[leg["Kind"] === "External", Return[{leg["Index"]}]];
 edge = leg["Source"]; graph = EdgeDelete[diagram["Graph"], edge];
 component = SelectFirst[ConnectedComponents[graph], !MemberQ[#, vertex] &&
   Intersection[#, List @@ edge] =!= {} &];
 Sort[Intersection[component, diagram["LeafVertices"]]]
];
HEPCATFourFermionVectorPrototype`Private`diagramCertificate[diagram_Association, catalog_Association, original_List, lifted_: False] := Module[
 {aliases = catalog["Aliases"], certificates = catalog["Certificates"], vertices, poles,
  flows, roles = <||>, row, structure, certificate, ports, fermions, bosons, vertex, path,
  name, edge, prop, spin, edges, components, ends, flowGraph},
 Do[If[row[[5]] === 1, AssociateTo[roles, {row[[2]] -> 0, row[[3]] -> 1}]], {row, original[[1]]}];
 vertices = Table[
  DeleteDuplicates[Table[
   certificate = SelectFirst[certificates,
     If[TrueQ[lifted], #["LiftedRow"] === structure["ModelRow"],
       MemberQ[#["SourceRows"], structure["ModelRow"]]] &, Missing["UncertifiedVertex"]];
   If[!AssociationQ[certificate], Return[HEPCATFourFermionVectorPrototype`Private`reject["UncertifiedOrderedVertex"]]];
   ports = ({Lookup[aliases, #["Particle"], #["Particle"]],
      HEPCATFourFermionVectorPrototype`Private`portAddress[diagram, vertex, #]} &) /@ structure["IncidentLegs"];
   fermions = Select[ports, KeyExistsQ[roles, First[#]] &];
   bosons = Complement[ports, fermions];
   fermions = SortBy[fermions, roles[First[#]] &];
   {certificate["CertificateID"], fermions, Sort[bosons]},
   {structure, diagram["VertexStructures"][vertex]}]], {vertex, diagram["InternalVertices"]}];
 If[!ListQ[vertices] || !FreeQ[vertices, _Failure], Return[HEPCATFourFermionVectorPrototype`Private`reject["UncertifiedOrderedVertex"]]];
 poles = Table[prop = diagram["Propagators"][edge];
   {Sort[{Lookup[aliases, prop["Particle"], prop["Particle"]],
      Lookup[aliases, prop["Antiparticle"], prop["Antiparticle"]]}], prop["Mass"], prop["Spin2"],
    Sort[Sort[Intersection[#, diagram["LeafVertices"]]] & /@
      ConnectedComponents[EdgeDelete[diagram["Graph"], edge]]]},
   {edge, Keys[diagram["Propagators"]]}];
 edges = Select[EdgeList[diagram["Graph"]], Function[e, With[{v = List @@ e},
   If[Min[v] <= Length[diagram["ExternalParticles"]],
     KeyExistsQ[roles, diagram["ExternalParticles"][[Min[v]]]],
     diagram["Propagators"][e]["Spin2"] === 1]]]];
 flowGraph = Graph[edges];
 flows = Table[ends = Intersection[component, diagram["LeafVertices"]];
   If[Length[ends] =!= 2 || Sort[Lookup[roles, diagram["ExternalParticles"][[ends]]]] =!= {0, 1},
     Return[HEPCATFourFermionVectorPrototype`Private`reject["InvalidDiracEndpointFlow"]]];
   SortBy[ends, roles[diagram["ExternalParticles"][[#]]] &], {component, ConnectedComponents[flowGraph]}];
 If[!ListQ[flows] || !FreeQ[flows, _Failure], Return[HEPCATFourFermionVectorPrototype`Private`reject["InvalidDiracEndpointFlow"]]];
 <|"OrderedVertices" -> Sort[Sort /@ vertices], "PhysicalPoles" -> Sort[poles],
   "OrientedFermionEndpoints" -> Sort[flows]|>
];
Options[HEPCATFourFermionVectorPrototype`Private`covariantCurrentSource] = {"TimeConstraint" -> 60};
HEPCATFourFermionVectorPrototype`Private`covariantCurrentSource[process_List, model_List, OptionsPattern[]] := Module[
 {rows, fermions, vectors, scalars, diagrams, catalog, liftedDiagrams, sourceSignatures, liftedSignatures,
  pairs, converted, expression, external, used, coverage, expected, weights, massiveDiracQ,
  seconds = OptionValue["TimeConstraint"], before},
 before = HEPCATFourFermionVectorPrototype`Private`objectSHA[model];
 If[Length[model] =!= 4 || !AllTrue[model, ListQ] || Length[process] =!= 5 ||
     !AllTrue[process, StringQ], Return[HEPCATFourFermionVectorPrototype`Private`reject["UnsupportedCurrentSourceInput"]]];
 rows = HEPCATFourFermionVectorPrototype`Private`particleRow[#, model] & /@ process;
 If[!AllTrue[rows, ListQ], Return[HEPCATFourFermionVectorPrototype`Private`reject["UnknownExternalParticle"]]];
 fermions = Select[rows, #[[5]] === 1 &]; vectors = Select[rows, #[[5]] === 2 &]; scalars = Select[rows, #[[5]] === 0 &];
 massiveDiracQ=AllTrue[fermions, #[[6]] =!= 0 && #[[2]] =!= #[[3]] && !MemberQ[{"L", "R"}, #[[9]]] &];
 If[!AllTrue[rows, #[[8]] === 1 &] || !massiveDiracQ ||
   !((Length[fermions] === 4 && (Length[scalars] === 1 || (Length[vectors] === 1 && First[vectors][[6]] =!= 0))) ||
     (Length[fermions] === 2 && Length[vectors] === 1 && Length[scalars] === 2 && First[vectors][[6]] === 0)),
   Return[HEPCATFourFermionVectorPrototype`Private`reject["UnsupportedExternalCurrentClass"]]];
 diagrams = CreateDiagrams[process, model];
 If[!ListQ[diagrams] || diagrams === {} || !AllTrue[diagrams, AssociationQ],
   Return[HEPCATFourFermionVectorPrototype`Private`reject["OriginalDiagramGenerationIncomplete"]]];
 used = HEPCATFourFermionVectorPrototype`Private`usedRows[diagrams]; catalog = HEPCATFourFermionVectorPrototype`Private`sourceCatalog[model, used];
 If[!AssociationQ[catalog], Return[catalog]];
 liftedDiagrams = CreateDiagrams[process, catalog["Model"]];
 If[!ListQ[liftedDiagrams] || !AllTrue[liftedDiagrams, AssociationQ],
   Return[HEPCATFourFermionVectorPrototype`Private`reject["LiftedDiagramGenerationIncomplete"]]];
 sourceSignatures = HEPCATFourFermionVectorPrototype`Private`diagramCertificate[#, catalog, model, False] & /@ diagrams;
 liftedSignatures = HEPCATFourFermionVectorPrototype`Private`diagramCertificate[#, catalog, model, True] & /@ liftedDiagrams;
 If[!AllTrue[Join[sourceSignatures, liftedSignatures], AssociationQ] ||
     Sort[sourceSignatures] =!= Sort[liftedSignatures],
   Return[HEPCATFourFermionVectorPrototype`Private`reject["OrderedGraphCoverageMismatch", <|"Original" -> sourceSignatures, "Lifted" -> liftedSignatures|>]]];
 pairs = DiagramAmplitudes[liftedDiagrams];
 converted = FeynmanToSpinorProducts[pairs, "ReferenceSpinors" -> Automatic,
   "ReduceSpinorProducts" -> False, "TimeConstraint" -> seconds];
 If[!TrueQ[ConstructiveDiagrams`Private`constructBenchmarkConversionCompleteQ[pairs, converted]],
   Return[HEPCATFourFermionVectorPrototype`Private`reject["OffShellConversionIncomplete"]]];
 external = First[First[converted]];
 If[!AllTrue[converted, First[#] === external &], Return[HEPCATFourFermionVectorPrototype`Private`reject["ExternalMassOrderingMismatch"]]];
 expression = Total[Last /@ converted];
 expected=HEPCATFourFermionVectorPrototype`Private`declaredExternalSpinWeights[process,model];
 If[FailureQ[expected],Return[HEPCATFourFermionVectorPrototype`Private`reject["UnsupportedDeclaredExternalSpinWeights",<|"Reason"->expected|>]]];
 weights=ConstructiveDiagrams`Private`completionExternalSpinWeights[expression,external];
 If[weights=!=expected,Return[HEPCATFourFermionVectorPrototype`Private`reject["UnsupportedCoherentExternalTensorEncoding"]]];
 If[before =!= HEPCATFourFermionVectorPrototype`Private`objectSHA[model], Return[HEPCATFourFermionVectorPrototype`Private`reject["SourceModelChanged"]]];
 <|"Status" -> "UNVALIDATED_CANDIDATE", "Amplitude" -> {external, expression},
  "Expression" -> expression, "ExternalMassRules" -> external, "ExternalParticles" -> process,
  "CompleteSpinDegrees" -> Abs[weights], "CompleteSpinWeights" -> weights,
  "VertexCertificates" -> catalog["Certificates"], "SourceUsedRows" -> used,
  "OrderedGraphCertificates" -> sourceSignatures, "LiftedGraphCertificates" -> liftedSignatures,
  "EveryOriginalDiagramCovered" -> True, "EveryUsedOrderedVertexCertified" -> True,
  "CompleteOffShellConversionCoverage" -> True, "SourceModelSHA256" -> before,
  "OriginalDiagramCount" -> Length[diagrams], "LiftedDiagramCount" -> Length[liftedDiagrams],
  "NativeValidationPerformed" -> False, "IndependentBenchmarkRead" -> False,
  "NumericalCoefficientFit" -> False, "NumericalParameterAssignments" -> {},
  "InternalOnShellConditions" -> {}, "NewIndependentContacts" -> False,
  "RequiresCheckAmplitudes" -> True,
  "ContinuationAssumption" -> "MinimalRenormalizableCovariantCurrentsOfExactlyMatchedElementaryVertices"|>
];
HEPCATFourFermionVectorPrototype`Private`covariantCurrentSource[___] := HEPCATFourFermionVectorPrototype`Private`reject["UnsupportedCurrentSourceInput"];
End[];
