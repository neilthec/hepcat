(* From the repository root, run this optional licensed test with
   WolframKernel -script csource/mathematica_roundtrip.wls /path/to/test_mathematica *)
wolframExportArgs = If[Length[$ScriptCommandLine] > 1, Rest[$ScriptCommandLine],
  Drop[$CommandLine, First[FirstPosition[$CommandLine, "-script"]] + 1]];
If[Length[wolframExportArgs] != 1, Print["Supply the path to the built test_mathematica executable."]; Exit[1]];
binary = ExpandFileName[First[wolframExportArgs]];
root = DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{root, "source", "HEPCAT.wl"}]];
$HistoryLength = 0;

compare[number_, nativeScope_: False] := Module[
  {directory = FileNameJoin[{root, "models"}], model, scope, expected, path, result, received, foreign},
  model = ReadModel[directory, number];
  If[FailureQ[model], Return[False]];
  scope = ModelParameter[model, "EE"][[2]];
  expected = Function[row, With[{legs = DeleteCases[Drop[row, -2], ""]},
      {Thread[(Mass /@ Range[Length[legs]]) -> (ParticleMass[#, model[[1]]] & /@ legs)],
       row[[-2]] Last[row]}]] /@ model[[2]];
  Internal`WithLocalSettings[path = CreateFile[],
    result = RunProcess[{binary, "--export-model", directory, ToString[number], If[nativeScope, "", scope], path}];
    If[result["ExitCode"] =!= 0, Print[result["StandardError"]]; Return[False]];
    (* Removing HEPCAT from the lookup path detects any unqualified physics heads. *)
    received = Block[{$Context = "ExportProbe`", $ContextPath = {"System`"}}, Get[path]];
    If[TrueQ[nativeScope],
      foreign = DeleteDuplicates@Cases[received, ModelParameter[_, s_String, _] :> s, Infinity];
      If[Length[foreign] != 1 || !StringStartsQ[First[foreign], "native-"], Return[False]];
      With[{source = First[foreign]},
        received = received /. ModelParameter[name_, source, _] :> ModelParameter[model, name]]];
    Print["Model ", number, ", ", Length[expected], " signed amplitudes and mass lists, ",
      If[nativeScope, "explicit scope binding", "shared model scope"], ": ", received === expected];
    received === expected,
    DeleteFile[path]]
];
passed = {compare[4], compare[4, True], compare[2]};
Exit[If[And @@ passed, 0, 1]];
