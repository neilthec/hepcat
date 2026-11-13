(* Opt-in exact-vector tests for the production exact tensor-network backend. *)
HEPCATColorNetworkTests`Private`root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`colorNetworkContract=911;
Global`colorTensorVectorSparseDetails[___]:=777;
Global`colorTensorVectorEnumerated=313;
Get[FileNameJoin[{HEPCATColorNetworkTests`Private`root,"source","HEPCAT.wl"}]];
Begin["HEPCATColorNetworkTests`Private`"];$ContextPath={"System`"};$HistoryLength=0;
sourceFiles=Sort[FileNames["*.wl",FileNameJoin[{root,"source"}],Infinity]];
sourceBefore=FileHash[#,"SHA256"]&/@sourceFiles;
protectedDefinitions={DownValues[ConstructiveDiagrams`Private`colorTensorBasis],
 DownValues[ConstructiveDiagrams`Private`colorTensorVector],DownValues[ConstructiveDiagrams`Private`colorProjectionData]};
checks=0;failures={};timings={};SetAttributes[check,HoldRest];
check[name_,test_]:=(checks++;If[!TrueQ[test],AppendTo[failures,name];Print["FAIL: ",name]]);
ci[i_]:=ConstructiveDiagrams`ColorIndex[i];di[i_,d_]:=ConstructiveDiagrams`Private`colorInternalIndex[i,d];
delta=ConstructiveDiagrams`ColorDelta;gen=ConstructiveDiagrams`ColorGenerator;
structure=ConstructiveDiagrams`ColorStructure;trace=ConstructiveDiagrams`ColorTrace;
d1=delta[ci[1],ci[2]]delta[ci[3],ci[4]];d2=delta[ci[1],ci[4]]delta[ci[3],ci[2]];
g1=gen[ci[2],ci[1],di[1,8]]gen[ci[4],ci[3],di[1,8]];
q34=gen[ci[2],di[1,3],ci[3]]gen[di[1,3],ci[1],ci[4]];
q43=gen[ci[2],di[1,3],ci[4]]gen[di[1,3],ci[1],ci[3]];
f1=structure[ci[1],ci[2],di[1,8]]structure[di[1,8],ci[3],ci[4]];
f2=structure[ci[2],ci[3],di[1,8]]structure[di[1,8],ci[1],ci[4]];
tr2=trace[ci[1],ci[2]];tr3=trace[ci[1],ci[2],ci[3]];
cases={
 {"single generator axis order",gen[ci[1],ci[2],ci[3]],{3,-3,8},"ExactSparseTensorNetwork"},
 {"conjugated generator phase",(2+3I)Conjugate[gen[ci[1],ci[2],ci[3]]],{3,-3,8},"ExactSparseTensorNetwork"},
 {"disconnected singlet products",d1,{3,-3,3,-3},"ExactSparseTensorNetwork"},
 {"crossed singlet products",d2,{3,-3,3,-3},"ExactSparseTensorNetwork"},
 {"fundamental completeness network",g1,{3,-3,3,-3},"ExactSparseTensorNetwork"},
 {"ordered quark gluons",q34,{3,-3,8,8},"ExactSparseTensorNetwork"},
 {"complex ordered quark interference",q34+I q43,{3,-3,8,8},"ExactSparseTensorNetwork"},
 {"four-adjoint network",f1,{8,8,8,8},"ExactSparseTensorNetwork"},
 {"complex adjoint polynomial",f1-I f2,{8,8,8,8},"ExactSparseTensorNetwork"},
 {"two-generator trace network",tr2,{8,8},"ExactSparseTensorNetwork"},
 {"three-generator conjugated trace",Conjugate[tr3],{8,8,8},"ExactSparseTensorNetwork"},
 {"independent trace powers use exact fallback",tr2^2,{8,8},"LegacyFallback"},
 {"polynomial trace terms preserve local dummies",tr2+I tr2^2,{8,8},"LegacyFallback"},
 {"external repeated port uses exact fallback",delta[ci[1],ci[1]],{3},"LegacyFallback"},
 {"self-contracted primitive uses exact fallback",gen[di[1,3],di[1,3],ci[1]],{8},"LegacyFallback"},
 {"closed tensor network",delta[di[1,3],di[2,3]]^2,{1},"ExactSparseTensorNetwork"},
 {"zero tensor",0,{8,8},"ExactSparseTensorNetwork"},
 {"singlet scalar phase",-2I,{1},"ExactSparseTensorNetwork"}};
Do[
 oldSeconds=First@AbsoluteTiming[old=ConstructiveDiagrams`Private`colorTensorVectorEnumerated[case[[2]],case[[3]]]];
 newSeconds=First@AbsoluteTiming[new=ConstructiveDiagrams`Private`colorTensorVectorSparseDetails[case[[2]],case[[3]]]];
 AppendTo[timings,<|"Name"->First[case],"OldSeconds"->oldSeconds,"NetworkSeconds"->newSeconds|>];
 check[First[case]<>" exact vector",AssociationQ[new]&&ListQ[old]&&Normal[new["Value"]]===old&&
  ConstructiveDiagrams`Private`colorTensorVector[case[[2]],case[[3]]]===old];
 check[First[case]<>" admission",new["Backend"]===Last[case]],{case,cases}];
sample=ArrayReshape[Range[24],{2,3,4}];
ordered=ConstructiveDiagrams`Private`colorNetworkAxisOrder[sample,{a,b,c},{b,c,a}];
check["axis reordering is verified componentwise without a transpose-convention assumption",
 Dimensions[ordered]==={3,4,2}&&And@@Flatten[Table[ordered[[j,k,i]]===sample[[i,j,k]],{i,2},{j,3},{k,4}]]];
vectors=ConstructiveDiagrams`Private`colorTensorVectorSparse[#,{3,-3,3,-3}]&/@{d1,d2,g1,I d1,d1};
oldBasis=ConstructiveDiagrams`Private`colorTensorBasis[{d1,d2,g1,I d1,d1},{3,-3,3,-3}];
check["sparse exact Gram preserves all coherent complex interference and repeated rows",
 ConstructiveDiagrams`Private`colorSparseTensorGram[vectors]===oldBasis["DiagramGramMatrix"]];
invalid=ConstructiveDiagrams`Private`colorTensorVectorSparseDetails[0.5d1,{3,-3,3,-3}];
check["inexact inputs retain the original failure through fallback",
 invalid["Backend"]==="LegacyFallback"&&invalid["Value"]===ConstructiveDiagrams`Private`colorTensorVectorEnumerated[0.5d1,{3,-3,3,-3}]];
invalid=ConstructiveDiagrams`Private`colorTensorVectorSparseDetails[gen[ci[2],ci[1],di[1,3]],{3,-3}];
check["dimension mismatch retains the original failure through fallback",
 invalid["Backend"]==="LegacyFallback"&&invalid["Value"]===ConstructiveDiagrams`Private`colorTensorVectorEnumerated[gen[ci[2],ci[1],di[1,3]],{3,-3}]];
check["new private backend names never capture or alter ambient Global definitions",
 Global`colorNetworkContract===911&&Global`colorTensorVectorSparseDetails[1]===777&&Global`colorTensorVectorEnumerated===313];
check["the prototype never replaces package algebra definitions",
 protectedDefinitions==={DownValues[ConstructiveDiagrams`Private`colorTensorBasis],
 DownValues[ConstructiveDiagrams`Private`colorTensorVector],DownValues[ConstructiveDiagrams`Private`colorProjectionData]}];
check["all package source files remain byte-identical",sourceBefore===(FileHash[#,"SHA256"]&/@sourceFiles)];
Print["COLOR TENSOR NETWORK CHECKS: ",checks,"; FAILURES: ",Length[failures]];
Print["COLOR TENSOR NETWORK SMALL TIMINGS: ",InputForm[timings]];
If[failures==={},Print["HEPCAT_TEST_COMPLETED:color-tensor-network-tests.wl"],Print[failures]];
Quit[If[failures==={},0,1]];
