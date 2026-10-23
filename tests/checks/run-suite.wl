(* Parse before execution: Wolfram can otherwise exit zero after a syntax error. *)
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
