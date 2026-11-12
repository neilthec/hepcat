(* Check native notebook cell structure as well as Wolfram syntax. Adjacent
   Cell[...] expressions can parse as Times without producing Syntax errors. *)

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


root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
notebookPath = FileNameJoin[{root, "tests", "QED-6-point-construction.nb"}];
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
(* Inspect active inputs as held code. The user may rename a section or its
   amplitude variable; a matching formula assignment and actual validation
   call preserve the example independently of those presentation choices. *)
inputForms = Join[
  Cases[held, Cell[text_String, "Input", ___] :>
    Quiet[Check[ToExpression[text, InputForm, HoldComplete], $Failed]], Infinity],
  Cases[held, Cell[BoxData[boxes_], "Input", ___] :>
    Quiet[Check[ToExpression[boxes, StandardForm, HoldComplete], $Failed]], Infinity]];
sevenSpuriousFormulaBindings = Cases[inputForms,
  HoldPattern[Set[target_Symbol, Part[qedAllPositiveSpuriousFormula[model_, 5], 2]]] :>
    HoldComplete[target, model], Infinity];
sevenSpuriousValidatedBindings = Cases[inputForms,
  HoldPattern[ValidateMadGraph[
    {"e", "E", "A.+", "A.+", "A.+", "A.+", "A.+"}, amplitude_Symbol, model_, ___]] :>
      HoldComplete[amplitude, model], Infinity];
check["the seven-point AllSpurious formula and its explicit MadGraph validation are retained",
  AnyTrue[sevenSpuriousValidatedBindings, MemberQ[sevenSpuriousFormulaBindings, #] &]];
check["the independent eight-point prediction test is retained",
  !FreeQ[held, "All-Positive Photons: A General AllSpurious Formula"]];
Print["QED NOTEBOOK STRUCTURE CHECKS: ", checks, "; FAILURES: ", Length[failures]];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:qed-construction-notebook-tests.wl"], Print[failures]];
Quit[If[failures === {}, 0, 1]];
