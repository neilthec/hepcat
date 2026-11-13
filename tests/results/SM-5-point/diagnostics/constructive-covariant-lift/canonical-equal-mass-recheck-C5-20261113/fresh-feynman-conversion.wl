(* Independent full off-shell Feynman comparison, generated from the reference model. *)
qConvertBenchmark[process_List,feynmanModel_]:=Module[{graphs,raw,benchmark},
 graphs=CreateDiagrams[process,feynmanModel];raw=DiagramAmplitudes[graphs];
 benchmark=FeynmanToSpinorProducts[raw,"ReferenceSpinors"->Automatic,
  "ReduceSpinorProducts"->False,"TimeConstraint"->30];
 If[!TrueQ[ConstructiveDiagrams`Private`constructBenchmarkConversionCompleteQ[raw,benchmark]],
  Return[Failure["IncompleteReferenceConversion",<||>]]];
 <|"GraphCount"->Length[graphs],"Benchmark"->benchmark|>
];
