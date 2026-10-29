(* Independent workers must preserve ordering, caller state and index scope. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures,name]; Print["FAIL: ", name]]);
peRun[fun_, jobs_, opts_:<||>, callback_:None] :=
  ConstructiveDiagrams`Private`amplitudeParallelMap[fun,jobs,
    Join[<|"Parallelization"->True,"MaxKernels"->2|>,opts],callback];
(* Failed or malformed send receipts must return promptly, before trying to
   receive from nonexistent workers. InheritedBlock restores Wolfram's API. *)
peBadSend = Internal`InheritedBlock[{Parallel`Developer`Send},
  Unprotect[Parallel`Developer`Send]; Clear[Parallel`Developer`Send];
  Parallel`Developer`Send[_, _] := $Failed;
  TimeConstrained[ConstructiveDiagrams`Private`amplitudeParallelEvaluate[
    "UNUSED", {peTransportKernel1, peTransportKernel2}], 1, "TIMEOUT"]];
check["a failed send receipt returns failed results without entering the receive loop",
  peBadSend === {$Failed, $Failed}];
peShortSend = Internal`InheritedBlock[{Parallel`Developer`Send},
  Unprotect[Parallel`Developer`Send]; Clear[Parallel`Developer`Send];
  Parallel`Developer`Send[_, _] := {};
  TimeConstrained[ConstructiveDiagrams`Private`amplitudeParallelEvaluate[
    "UNUSED", {peTransportKernel1, peTransportKernel2}], 1, "TIMEOUT"]];
check["a malformed send receipt returns one failure per requested worker",
  peShortSend === {$Failed, $Failed}];
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
check["all callbacks execute on master once with correct original index",
  SortBy[peEvents, First] === ({#,5,0,#}& /@ Range[5])];
(* A slow first item must not leave the other worker idle at a wave barrier.
   Use event order and job timestamps rather than a machine-specific speedup. *)
peQueueEvents = {};
peQueue = peRun[Function[x, Module[{started = AbsoluteTime[]},
  Pause[If[x === 1, 1.5, 0.02]]; {x, $KernelID, started, AbsoluteTime[]}]],
  Range[6], <||>, Function[{result,index,total},
    AppendTo[peQueueEvents, {index, total, $KernelID, AbsoluteTime[]}]]];
check["dynamic scheduling retains input order for uneven-duration jobs",
  peQueue["Results"][[All,1]] === Range[6] &&
    peQueue["Execution"]["Scheduler"] === "DynamicQueue"];
check["an idle worker starts its next job before a slower first job finishes",
  peQueue["Results"][[3,2]] === peQueue["Results"][[2,2]] &&
    peQueue["Results"][[3,3]] < peQueue["Results"][[1,4]]];
check["completion callbacks arrive while the slow worker is still busy",
  FirstPosition[peQueueEvents[[All,1]], 3][[1]] <
    FirstPosition[peQueueEvents[[All,1]], 1][[1]] &&
    SortBy[peQueueEvents, First][[All, {1,2,3}]] === ({#,6,0}& /@ Range[6])];
peForeignCount = Length[Complement[Kernels[], ConstructiveDiagrams`Private`$amplitudeParallelKernels]];
peAvailableLicense = If[IntegerQ[$MaxLicenseSubprocesses] && $MaxLicenseSubprocesses >= 0,
  Max[0, $MaxLicenseSubprocesses - peForeignCount], Infinity];
peLimits = Block[{$ProcessorCount = 12},
  ConstructiveDiagrams`Private`amplitudeParallelLimit[Range[#1], Identity, #2] & @@@
    {{20, Automatic}, {20, 3}, {5, Automatic}}];
check["automatic worker limits follow processors, licensing and job count with explicit reductions",
  peLimits === {Min[12,peAvailableLicense], Min[3,peAvailableLicense], Min[5,peAvailableLicense]}];
check["the automatic default has no arbitrary two-worker ceiling when the license permits",
  Block[{$ProcessorCount = 16},
    ConstructiveDiagrams`Private`amplitudeParallelLimit[Range[12], Identity, Automatic]] ===
      Min[12,16,peAvailableLicense]];
peOne = peRun[Function[x,$KernelID],Range[4],<|"MaxKernels"->1|>];
check["one-worker cap means serial without growing pool", peOne["Results"] === {0,0,0,0} &&
  peOne["Execution"]["Mode"] === "Serial" && pePool === ConstructiveDiagrams`Private`$amplitudeParallelKernels];
peSerial = peRun[Function[x,{x,$KernelID}],Range[3],<|"Parallelization"->False|>];
check["explicit false bypasses all workers", peSerial["Results"] === {{1,0},{2,0},{3,0}}];
peNested = peRun[Function[x,ConstructiveDiagrams`Private`amplitudeParallelMap[
  Function[y,$KernelID],{1,2},<|"Parallelization"->True,"MaxKernels"->2|>]],Range[2]];
check["worker tasks never launch nested pools", AllTrue[peNested["Results"],
  #["Execution"]["Mode"] === "Serial" && Min[#["Results"]] > 0 &]];
peRetryEvents = {};
peRetry = peRun[Function[x,If[$KernelID > 0,$Failed,x+1]],Range[3], <||>,
  Function[{result,index,total}, AppendTo[peRetryEvents, {index,total,$KernelID,result}]]];
check["failed worker jobs are retried in original position", peRetry["Results"] === {2,3,4} &&
  peRetry["Execution"]["RetriedJobs"] === {1,2,3} && peRetry["Execution"]["Diagnostics"] =!= {}];
check["each serially retried job emits exactly one master callback",
  SortBy[peRetryEvents, First] === Table[{i,3,0,i+1}, {i,3}]];
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
(* A genuinely closed worker must not strand the dynamic queue. Only its
   unfinished item is retried; other package and user workers keep running. *)
peDropKernel = First[ConstructiveDiagrams`Private`$amplitudeParallelKernels];
peDropped = False; peDropEvents = {};
peDrop = peRun[Function[x, Pause[If[x === 1, 0.8, 0.02]]; {x,$KernelID}], Range[4], <||>,
  Function[{result,index,total},
    If[index === 2 && !peDropped, peDropped = True; Quiet[CloseKernels[{peDropKernel}]]];
    AppendTo[peDropEvents, {index,total,$KernelID}]]];
check["a lost worker retries only its unfinished item without stranding later jobs",
  peDropped && peDrop["Results"][[All,1]] === Range[4] &&
    peDrop["Results"][[1,2]] === 0 && Min[Rest[peDrop["Results"]][[All,2]]] > 0 &&
    peDrop["Execution"]["RetriedJobs"] === {1} &&
    SortBy[peDropEvents, First] === ({#,4,0}& /@ Range[4])];
check["a failed package worker does not close unrelated user workers",
  Complement[peUnrelated, Kernels[]] === {} &&
    ParallelEvaluate[Global`unrelatedParallelSentinel, peUnrelated, DistributedContexts -> None] === {914}];
SeedRandom[19729]; peState = $RandomState;
peRun[Function[x,x+1],Range[2]];
check["worker scheduling does not consume caller random state", $RandomState === peState];
check["empty input remains empty and serial", peRun[Identity,{}]["Results"] === {}];
(* Initialization and cleanup use the same transport primitive directly.
   Losing one worker here must still deliver the other worker's reply. *)
peTransportKernels = Take[ConstructiveDiagrams`Private`$amplitudeParallelKernels, 2];
peTransportIDs = ParallelEvaluate[$KernelID, peTransportKernels, DistributedContexts -> None];
peTransportLoss = With[{lost = First[peTransportIDs], pool = peTransportKernels},
  TimeConstrained[ConstructiveDiagrams`Private`amplitudeParallelEvaluate[
    If[$KernelID === lost, Quit[], 914], pool], 10, "TIMEOUT"]];
check["transport returns a failed lost-worker slot and the healthy worker reply",
  peTransportLoss === {$Failed, 914} && MemberQ[Kernels[], Last[peTransportKernels]] &&
    ParallelEvaluate[914, {Last[peTransportKernels]}, DistributedContexts -> None] === {914}];
check["transport worker loss leaves unrelated user workers and definitions intact",
  Complement[peUnrelated, Kernels[]] === {} &&
    ParallelEvaluate[Global`unrelatedParallelSentinel, peUnrelated, DistributedContexts -> None] === {914}];
ConstructiveDiagrams`Private`amplitudeParallelDropOwned[peTransportKernels];
(* Keep the existing timeout probe on a warm pool, interrupting active jobs
   rather than merely timing out startup after the intentional worker loss. *)
peRun[Identity, Range[2]];
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
(* Exercise the ordinary automatic setting beyond two actual workers when
   this host and license allow three package workers plus the unrelated one. *)
If[$ProcessorCount >= 3 && ($MaxLicenseSubprocesses === Infinity ||
    IntegerQ[$MaxLicenseSubprocesses] && $MaxLicenseSubprocesses >= Length[peUnrelated] + 3),
  peAutomatic = peRun[Function[x, {x, $KernelID}], Range[3], <|"MaxKernels" -> Automatic|>];
  check["automatic execution can use more than two real workers",
    peAutomatic["Execution"]["Mode"] === "Parallel" &&
      peAutomatic["Execution"]["KernelCount"] === 3 &&
      Length[DeleteDuplicates[peAutomatic["Results"][[All,2]]]] === 3 &&
      Min[peAutomatic["Results"][[All,2]]] > 0];
  check["automatic expansion still preserves unrelated user workers",
    Complement[peUnrelated, Kernels[]] === {} &&
      ParallelEvaluate[Global`unrelatedParallelSentinel, peUnrelated, DistributedContexts -> None] === {914}],
  Print["SKIP: more-than-two-worker execution requires three available processors and enough licensed subprocesses."]
];
Print["CHECKS: ",checks,"; FAILURES: ",Length[failures]];
If[failures =!= {}, Print[failures]];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:parallel-execution-tests.wl"]];
Quit[If[failures === {},0,1]];
