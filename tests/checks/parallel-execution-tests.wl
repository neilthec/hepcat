(* Independent workers must preserve ordering, caller state and index scope. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures,name]; Print["FAIL: ", name]]);
peRun[fun_, jobs_, opts_:<||>, callback_:None] :=
  ConstructiveDiagrams`Private`amplitudeParallelMap[fun,jobs,
    Join[<|"Parallelization"->True,"MaxKernels"->2|>,opts],callback];
peUnrelated = LaunchKernels[KernelConfiguration["localhost", "KernelCommand" -> First[$CommandLine],
  "KernelCount" -> 1]];
ParallelEvaluate[Global`unrelatedParallelSentinel = 914, peUnrelated, DistributedContexts -> None];
peBefore = Kernels[];
peCold = peRun[Function[x,{x^2,$KernelID}],Range[6]];
check["parallel jobs ran on two real workers", peCold["Execution"]["Mode"] === "Parallel" &&
  peCold["Execution"]["KernelCount"] === 2 && Min[peCold["Results"][[All,2]]] > 0];
check["input ordering is retained", peCold["Results"][[All,1]] === Range[6]^2];
check["preexisting user workers are never closed", Complement[peBefore,Kernels[]] === {} &&
  ParallelEvaluate[Global`unrelatedParallelSentinel, peUnrelated, DistributedContexts -> None] === {914} &&
  Intersection[peUnrelated,ConstructiveDiagrams`Private`$amplitudeParallelKernels] === {}];
pePool = ConstructiveDiagrams`Private`$amplitudeParallelKernels;
peEvents = {};
peWarm = peRun[Function[x,{x,$KernelID}],Range[5],<||>,
  Function[{result,index,total},AppendTo[peEvents,{index,total,$KernelID,result[[1]]}]]];
check["warm calls reuse the same owned workers", pePool === ConstructiveDiagrams`Private`$amplitudeParallelKernels];
check["all callbacks execute on master with correct original index",
  peEvents === ({#,5,0,#}& /@ Range[5])];
peOne = peRun[Function[x,$KernelID],Range[4],<|"MaxKernels"->1|>];
check["one-worker cap means serial without growing pool", peOne["Results"] === {0,0,0,0} &&
  peOne["Execution"]["Mode"] === "Serial" && pePool === ConstructiveDiagrams`Private`$amplitudeParallelKernels];
peSerial = peRun[Function[x,{x,$KernelID}],Range[3],<|"Parallelization"->False|>];
check["explicit false bypasses all workers", peSerial["Results"] === {{1,0},{2,0},{3,0}}];
peNested = peRun[Function[x,ConstructiveDiagrams`Private`amplitudeParallelMap[
  Function[y,$KernelID],{1,2},<|"Parallelization"->True,"MaxKernels"->2|>]],Range[2]];
check["worker tasks never launch nested pools", AllTrue[peNested["Results"],
  #["Execution"]["Mode"] === "Serial" && Min[#["Results"]] > 0 &]];
peRetry = peRun[Function[x,If[$KernelID > 0,$Failed,x+1]],Range[3]];
check["failed worker jobs are retried in original position", peRetry["Results"] === {2,3,4} &&
  peRetry["Execution"]["RetriedJobs"] === {1,2,3} && peRetry["Execution"]["Diagnostics"] =!= {}];
peCoordinates = peRun[Function[x,{Unique["poleCoordinate"],Module[{peIndex},peIndex]}],Range[4]];
check["generated coordinates cannot collide across workers or items",
  DuplicateFreeQ[Flatten[peCoordinates["Results"]]]];
peGlobal = 91;
peValues = With[{value = peGlobal},peRun[Function[x,value+x],Range[2]]];
check["explicit closures carry values without global definitions", peValues["Results"] === {92,93}];
peNoGlobal = peRun[Function[x,OwnValues[Global`peGlobal]],Range[2]];
check["user global definitions were not distributed", peNoGlobal["Results"] === {{},{}}];
peTemporary = ParallelEvaluate[Names["ConstructiveDiagrams`Private`amplitudeJob$*"],
  pePool,DistributedContexts->None];
check["temporary job closure definitions are removed", peTemporary === {{},{}}];
SeedRandom[19729]; peState = $RandomState;
peRun[Function[x,x+1],Range[2]];
check["worker scheduling does not consume caller random state", $RandomState === peState];
check["empty input remains empty and serial", peRun[Identity,{}]["Results"] === {}];
peStale = Block[{ConstructiveDiagrams`Private`$amplitudeParallelLoadedSignature = -1},
  peRun[Function[x,{x,$KernelID}],Range[2]]];
check["changed package files cannot silently mix worker and master versions",
  peStale["Results"] === {{1,0},{2,0}} && peStale["Execution"]["Mode"] === "Serial" &&
    StringContainsQ[First[peStale["Execution"]["Diagnostics"]],"Reload"]];
Parallel`Developer`Send[First[peUnrelated], Pause[0.5]; "UNRELATED FINISHED"];
peTimeout = TimeConstrained[peRun[Function[x,Pause[2];x],Range[2]],0.1,"TIMEOUT"];
check["a time-limited batch stops rather than accepting partial results", peTimeout === "TIMEOUT" &&
  ConstructiveDiagrams`Private`$amplitudeParallelKernels === {}];
peUnrelatedResult = Parallel`Developer`Receive[First[peUnrelated]];
check["interrupting package work does not abort an unrelated running job",
  peUnrelatedResult === "UNRELATED FINISHED"];
check["batch interruption preserves unrelated user workers and their values",
  Complement[peUnrelated,Kernels[]] === {} &&
    ParallelEvaluate[Global`unrelatedParallelSentinel,peUnrelated,DistributedContexts->None] === {914}];
Print["CHECKS: ",checks,"; FAILURES: ",Length[failures]];
If[failures =!= {}, Print[failures]];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:parallel-execution-tests.wl"]];
Quit[If[failures === {},0,1]];
