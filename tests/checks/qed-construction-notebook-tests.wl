(* Check native notebook cell structure as well as Wolfram syntax. Adjacent
   Cell[...] expressions can parse as Times without producing Syntax errors. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
notebookPath = FileNameJoin[{root, "tests", "QED-5-point-construction.nb"}];
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, test_] := (checks++; If[!TrueQ[test],
  AppendTo[failures, name]; Print["FAIL: ", name]]);
held = Quiet[Check[ToExpression[Import[notebookPath, "Text"], InputForm, HoldComplete], $Failed]];
notebooks = Cases[held, HoldPattern[Notebook[cells_List, ___]] :> HoldComplete[cells], {1}];
check["the file contains one native notebook", Length[notebooks] === 1];
check["top-level entries are individual cells, never products of cells",
  Length[notebooks] === 1 && MatchQ[First[notebooks], HoldComplete[{___Cell}]]];
groups = Cases[held, HoldPattern[CellGroupData[children_, ___]] :> HoldComplete[children], Infinity];
check["every group contains a proper list of separate cells",
  groups =!= {} && AllTrue[groups, MatchQ[#, HoldComplete[{___Cell}]] &]];
check["the seven-point spurious example is retained",
  !FreeQ[held, "Seven-Point Amplitudes (Max Spurious)"]];
check["the independent eight-point prediction test is retained",
  !FreeQ[held, "All-Positive Photons: A General AllSpurious Formula"]];
Print["QED NOTEBOOK STRUCTURE CHECKS: ", checks, "; FAILURES: ", Length[failures]];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:qed-construction-notebook-tests.wl"], Print[failures]];
Quit[If[failures === {}, 0, 1]];
