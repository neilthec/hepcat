(* Parse before execution: Wolfram can otherwise exit zero after a syntax error. *)

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


hepcatTestFile = Environment["HEPCAT_TEST_SUITE_PATH"];
If[!StringQ[hepcatTestFile] || !FileExistsQ[hepcatTestFile],
  Print["Expected an existing HEPCAT_TEST_SUITE_PATH."]; Quit[2]];
hepcatTestSource = Check[Import[hepcatTestFile, "Text"], $Failed];
If[!StringQ[hepcatTestSource], Print["Cannot read regression suite: ", hepcatTestFile]; Quit[2]];
(* Do not precreate Global symbols that could shadow the package on loading. *)
hepcatTestParsed = Block[{$Context = "HEPCATTestSyntax`", $ContextPath = {"System`"}},
  Check[ToExpression[hepcatTestSource, InputForm, HoldComplete], $Failed]];
If[Head[hepcatTestParsed] =!= HoldComplete,
  Print["Regression suite failed syntax validation: ", hepcatTestFile]; Quit[2]];
Clear[hepcatTestParsed, hepcatTestSource];
Get[hepcatTestFile];
Print["Regression suite returned without its final exit: ", hepcatTestFile];
Quit[1];
