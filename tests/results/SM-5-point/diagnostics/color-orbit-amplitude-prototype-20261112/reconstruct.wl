(* Prototype: exact finite color representation plus typed external-state transport.
   The process and all matrices are input data. No amplitude formula is built in. *)
Begin["HEPCATColorOrbitReconstruction`Private`"];
HEPCATColorOrbitReconstruction`Private`reconstruct[seed_,orbit_Association,process_List,model_List]:=Module[
 {basis=orbit["Basis"],witness=orbit["FirstCoefficientOrbit"],rows,images,permutations,grades,
  particleRows,groups,expectedGrades,rank,inverse,cycles,coefficients},
 If[orbit["SourceModelSHA256"]=!=HEPCATCovariantCurrentSources`Private`objectSHA[model],Return[Failure["SourceModelOwnershipMismatch",<||>]]];
 rank=Length[basis];rows=witness["Rows"];permutations=witness["Permutations"];grades=witness["FermionGrades"];
 If[orbit["Summary"]["Particles"]=!=process||witness["Rank"]=!=rank||Dimensions[rows]=!={rank,rank}||
  !TrueQ[orbit["Summary"]["AllGeneratorClosuresVerified"]]||!TrueQ[orbit["Summary"]["AllGeneratorGramInvariantsVerified"]],
  Return[Failure["IncompleteExactColorOrbit",<||>]]];
 particleRows=HEPCATCovariantCurrentSources`Private`particleRow[#,model]&/@process;
 If[!AllTrue[particleRows,ListQ],Return[Failure["UnknownExternalState",<||>]]];
 groups=Select[GatherBy[Range[Length[process]],{particleRows[[#,2]],particleRows[[#,3]],process[[#]]}&],Length[#]>1&];
 If[!AllTrue[permutations,Sort[#]===Range[Length[process]]&&process[[#]]===process&],Return[Failure["ExternalStatePermutationMismatch",<||>]]];
 expectedGrades=Table[Times@@Table[If[OddQ[Abs[particleRows[[First[group],5]]]],Signature[permutation[[group]]],1],
  {group,groups}],{permutation,permutations}];
 If[expectedGrades=!=grades,Return[Failure["FermionGradeMismatch",<||>]]];
 inverse=Inverse[rows];
 If[rows.inverse=!=IdentityMatrix[rank]||inverse.rows=!=IdentityMatrix[rank],Return[Failure["ColorOrbitInverse",<||>]]];
 cycles=ConstructiveDiagrams`Private`completionBoseCycles[Range[Length[process]],#]&/@permutations;
 images=Fold[ConstructiveDiagrams`Private`completionBoseCycle,seed,#]&/@cycles;
 (* The recorded orbit rows already include the fermion grade. Do not apply it twice. *)
 coefficients=inverse.images;
 <|"Status"->"UNVALIDATED_SYMMETRY_RECONSTRUCTION","SeedCoefficient"->witness["SeedCoefficient"],
  "Coefficients"->coefficients,"FullExpression"->basis.coefficients,"TransportedSeedImages"->images,
  "LiteralPermutations"->permutations,"Cycles"->cycles,"FermionGrades"->grades,
  "RowMatrix"->rows,"InverseRowMatrix"->inverse,"ExactMatrixInverseVerified"->True,
  "AmplitudeInputCount"->1,"NoKnownAmplitudeOrCoefficientFit"->True,
  "InternalOnShellConditions"->{},"RequiresFullNativeAndIndependentValidation"->True|>
];
End[];
