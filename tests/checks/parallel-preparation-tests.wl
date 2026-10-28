(* Independent items preserve order and physics across local kernels. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++;
  If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);
ppModel = {{{"Scalar", "s", "s", 1, 0, 0, 0, 1, 1, "", ""}}, {}, {}, {}};
ppMass = ModelParameter["parallelPreparationMass", "parallel-preparation-fixture", "Real"];
ppRules = {Mass[1] -> ppMass, Mass[2] -> ppMass, Mass[3] -> 0, Mass[4] -> 0};
ppBracket = SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle", 4]];
ppChain = SpinorChain[Spinor["Spin", "Angle", 2], Mom[1], Mom[1],
  Spinor["Helicity", "Angle", 4]];
ppPairs = Table[{Append[ppRules, Mass[Multiparticle[1, 3]] -> ppMass],
  index ppChain + MomProd[1, 3]}, {index, 4}];
ppOriginal = ppPairs;
ppPrepare[input_, opts___] := PrepareAmplitudes[input, ppModel, opts,
  "MaxSteps" -> 0, "TimeConstraint" -> Infinity, "ProgressReporting" -> False,
  "ReturnDetails" -> True];
ppSerial = ppPrepare[ppPairs, "Parallelization" -> False];
ppParallel = ppPrepare[ppPairs, "Parallelization" -> True, "MaxKernels" -> 2];
Print["PREPARATION EXECUTION: ", ppParallel["Execution"]];
check["parallel diagrams retain their exact input ordering and ordinary expressions",
  ppParallel["PreparedAmplitudes"] === ppSerial["PreparedAmplitudes"] &&
  Last /@ ppParallel["PreparedAmplitudes"] === Table[index ppMass^2 ppBracket, {index, 4}]];
check["preparation never mutates original pairs or assigns scoped model parameters",
  ppPairs === ppOriginal && Head[ppMass] === ModelParameter &&
    Names["Global`parallelPreparationMass"] === {} &&
    Names["ConstructiveDiagrams`parallelPreparationMass"] === {}];
check["each report preserves its source item index and diagram-only shell",
  Lookup[ppParallel["ItemReports"], "ItemIndex"] === Range[4] &&
    AllTrue[ppParallel["ItemReports"], #["InternalOnShellChannels"] === {{1, 3}} &]];
check["a requested worker cap is respected and fallback is diagnosed",
  AssociationQ[ppParallel["Execution"]] && ppParallel["Execution"]["KernelCount"] <= 2 &&
    (ppParallel["Execution"]["Mode"] === "Parallel" ||
      Lookup[ppParallel["Execution"], "Diagnostics", {}] =!= {})];
If[ppParallel["Execution"]["Mode"] === "Parallel",
  check["two actual subkernels execute preparation rather than only reporting a pool",
    ppParallel["Execution"]["KernelCount"] === 2 &&
      Sort[ppParallel["Execution"]["KernelIDs"]] ===
        Sort[DeleteDuplicates[Lookup[ppParallel["ItemReports"], "WorkerKernel"]]] &&
      FreeQ[Lookup[ppParallel["ItemReports"], "WorkerKernel"], 0]]];
ppLimited = ppPrepare[ppPairs, "MaxKernels" -> 1];
check["one requested kernel gives serial output without starting a worker pool",
  ppLimited["PreparedAmplitudes"] === ppSerial["PreparedAmplitudes"] &&
    ppLimited["Execution"]["Mode"] =!= "Parallel"];
check["default ordinary output is still a list of amplitude pairs",
  PrepareAmplitudes[ppPairs, ppModel, "Parallelization" -> False,
    "MaxSteps" -> 0, "ProgressReporting" -> False] === ppSerial["PreparedAmplitudes"]];
ppSingle = PrepareAmplitudes[First[ppPairs], ppModel, "MaxKernels" -> 2,
  "MaxSteps" -> 0, "ProgressReporting" -> False];
check["a single pair retains its single-pair output shape", ppSingle === First[ppSerial["PreparedAmplitudes"]]];
check["empty input remains an empty list",
  PrepareAmplitudes[{}, ppModel, "ProgressReporting" -> False] === {}];
ppChannel = Join[Last /@ ppRules, {Multiparticle[1, 3], ppMass, ppChain}];
ppRows = {ppChannel, ReplacePart[ppChannel, -1 -> 3 ppChain]};
ppRowsSerial = ppPrepare[ppRows, "Parallelization" -> False];
ppRowsParallel = ppPrepare[ppRows, "Parallelization" -> True, "MaxKernels" -> 2];
check["channel rows keep all channel metadata and exact ordered expressions",
  ppRowsParallel["PreparedAmplitudes"] === ppRowsSerial["PreparedAmplitudes"] &&
    Map[Drop[#, -1] &, ppRowsParallel["PreparedAmplitudes"]] === Map[Drop[#, -1] &, ppRows] &&
    Lookup[ppRowsParallel["ItemReports"], "Cut"] === {{1, 3}, {1, 3}}];
ppEvents = {};
ppCallback = Function[event, AppendTo[ppEvents,
  Join[event, <|"CallbackKernel" -> $KernelID|>]]];
ppProgress = PrepareAmplitudes[ppPairs, ppModel,
  "Parallelization" -> True, "MaxKernels" -> 2, "MaxSteps" -> 0,
  "ProgressFunction" -> ppCallback, "ReturnDetails" -> True];
check["progress callbacks execute only on the main kernel",
  ppEvents =!= {} && AllTrue[ppEvents, #["CallbackKernel"] === 0 &]];
If[ppProgress["Execution"]["Mode"] === "Parallel",
  ppCompleted = Select[ppEvents, Lookup[#, "Operation", None] === "Prepared item" &];
  check["parallel progress counts completions independently of source order",
    Sort[Lookup[ppCompleted, "Item"]] === Range[4] &&
      Lookup[ppCompleted, "Completed"] === Range[4]]];
ppFeynmanModel = ReadModel[FileNameJoin[{root, "model_feynman"}], 2];
ppFeynmanPairs = diagramAmplitudes[CreateDiagrams[{"e", "E", "A.+", "A.-"}, ppFeynmanModel]];
ppFeynmanPrepare[opts___] := PrepareAmplitudes[ppFeynmanPairs, ppFeynmanModel, opts,
  "ReferenceSpinors" -> "AllSpurious", "MaxSteps" -> 0,
  "TimeConstraint" -> Infinity, "ProgressReporting" -> False, "ReturnDetails" -> True];
ppFeynmanSerial = ppFeynmanPrepare["Parallelization" -> False];
ppFeynmanParallel = ppFeynmanPrepare["Parallelization" -> True, "MaxKernels" -> 2];
check["raw Feynman tensor conversion has identical serial and parallel output",
  ppFeynmanParallel["Status"] === "PREPARED" &&
    ppFeynmanParallel["PreparedAmplitudes"] === ppFeynmanSerial["PreparedAmplitudes"]];
check["one common Feynman reference assignment is fixed before dispatch",
  ppFeynmanParallel["SharedReferences"] === ppFeynmanSerial["SharedReferences"] &&
    Sort[Lookup[ppFeynmanParallel["SharedReferences"]["Assignments"], "Leg"]] === {3, 4} &&
    AllTrue[ppFeynmanParallel["ItemReports"],
      Sort[KeyTake[#, {"Leg", "Mode", "Target"}] & /@
        #["ReferenceSpinors"]["Assignments"]] ===
      Sort[ppFeynmanParallel["SharedReferences"]["Assignments"]] &]];
check["Feynman internal numerators and denominators always stay off shell",
  AllTrue[ppFeynmanParallel["ItemReports"], #["InternalOnShellChannels"] === {} &&
    AllTrue[#["MassRules"], MatchQ[#, HoldPattern[Mass[_Integer] -> _]] &] &] &&
    FreeQ[ppFeynmanParallel["Amplitude"], Spinor["Xi", ___]]];
ppFeynmanPhysical = PrepareAmplitudes[ppFeynmanPairs, ppFeynmanModel,
  "ReferenceSpinors" -> "AllPhysical", "Parallelization" -> True, "MaxKernels" -> 2,
  "MaxSteps" -> 0, "TimeConstraint" -> Infinity, "ProgressReporting" -> False,
  "ReturnDetails" -> True];
check["physical Feynman references use the same per-leg plan for every diagram",
  ppFeynmanPhysical["Status"] === "PREPARED" &&
    AllTrue[ppFeynmanPhysical["SharedReferences"]["Assignments"], #["Mode"] === "AllPhysical" &] &&
    AllTrue[ppFeynmanPhysical["ItemReports"],
      Sort[KeyTake[#, {"Leg", "Mode", "Target"}] & /@
        #["ReferenceSpinors"]["Assignments"]] ===
      Sort[ppFeynmanPhysical["SharedReferences"]["Assignments"]] &]];
Do[check["invalid parallel settings fail cleanly: " <> ToString[invalid, InputForm],
  Quiet[PrepareAmplitudes[ppPairs, ppModel, invalid]] === $Failed],
  {invalid, {"Parallelization" -> "bad", "MaxKernels" -> 0, "MaxKernels" -> 1.5}}];
Print["CHECKS: ", checks, "; FAILURES: ", Length[failures]];
If[failures =!= {}, Print[failures]];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:parallel-preparation-tests.wl"]];
Quit[If[failures === {}, 0, 1]];
