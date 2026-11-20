(* HEPCAT, Copyright (C) 2026 Neil Christensen. GPL-3.0-or-later.
   Run WolframKernel -script tests/benchmarks/native-sm-four-point/export-notebook.wls.
   Read saved output expressions and held process specifications only. *)
destination = DirectoryName[$InputFileName];
root = Nest[DirectoryName, destination, 3];
Get[FileNameJoin[{root, "source", "HEPCAT.wl"}]];
$HistoryLength = 0;
notebook = FileNameJoin[{root, "tests", "SM-4-point-construction.nb"}];
require[condition_, message_] := If[!TrueQ[condition], Print[message]; Quit[1]];
held = ToExpression[Import[notebook, "Text"], InputForm, HoldComplete];
inputs = Cases[held, Cell[text_String, "Input", ___] :> text, Infinity];
exports = Select[inputs, StringContainsQ[#, " = WriteSPINASProcess["] &];
familyByCase = <||>;
Do[
  call = ToExpression[input, InputForm, HoldComplete];
  details = Cases[call, HoldPattern[WriteSPINASProcess[_, Rule[in_List, out_List], entries_List, _, opts___]] :>
    {in, out, HoldComplete[entries],
     Cases[HoldComplete[opts], HoldPattern[Rule["ProcessName", name_String]] :> name, Infinity]}, Infinity];
  require[MatchQ[details, {{{_String, _String}, {_String, _String}, _HoldComplete, {_String}}}], "Unexpected export " <> input];
  {initial, final, entries, {family}} = First[details];
  members = Cases[entries, HoldPattern[{legs : {__String}, sym_Symbol}] :> {legs, SymbolName[sym]}, Infinity];
  require[members =!= {}, "Empty family " <> family];
  Do[With[{name = StringDrop[member[[2]], -StringLength["Amplitude"]]},
    require[!KeyExistsQ[familyByCase, name], "Duplicate export " <> name];
    AssociateTo[familyByCase, name -> {family, member[[1]], initial, final}]], {member, members}],
 {input, exports}];
groups = Cases[held, Cell[CellGroupData[{Cell[source_String, "Input", ___], children___}, _], ___] /;
  StringContainsQ[source, "ConstructAmplitude["] :> {source, HoldComplete[children]}, Infinity];
wire[expression_] := StringReplace[ToString[FullForm[expression /. p_ModelParameter :>
    ModelParameter[ModelParameterName[p], "NOTEBOOK", Last[p]]], OutputForm, PageWidth -> Infinity],
  {"ConstructiveDiagrams`Private`" -> "ConstructiveDiagrams`Private`", "ConstructiveDiagrams`" -> "", " " -> ""}];
rows = Table[
  {source, children} = group;
  id = First[StringCases[source, StartOfString ~~ name : ("sm" ~~ DigitCharacter .. ~~ LetterCharacter ...) ~~ "Particles" :> name]];
  require[KeyExistsQ[familyByCase, id], "Missing export " <> id];
  {family, legs, initial, final} = familyByCase[id];
  declared = Cases[ToExpression[source, InputForm, HoldComplete],
    HoldPattern[Set[_Symbol, items : {_String, _String, _String, _String}]] :> items, Infinity];
  require[declared === {legs}, "Export and construction disagree for " <> id];
  outputs = Cases[children, Cell[BoxData[boxes_], "Output", ___] :> HoldComplete[boxes], Infinity];
  require[Length[outputs] === 1, "Missing or duplicate saved output for " <> id];
  expr = ReleaseHold[Replace[First[outputs], HoldComplete[boxes_] :> ToExpression[boxes, StandardForm, HoldComplete]]];
  score = SpinorExpressionComplexity[expr];
  require[IntegerQ[score] && score >= 0 && FreeQ[expr, _Failure | $Failed | $Aborted], "Invalid expression for " <> id];
  {id, family, StringRiffle[legs, ","], StringRiffle[initial, ","], StringRiffle[final, ","],
   ToString[score], ToString[LeafCount[expr]], wire[expr]}, {group, groups}];
require[Length[rows] === 138 && Length[exports] === 78 && Length[familyByCase] === Length[rows] &&
   DuplicateFreeQ[rows[[All, 1]]], "Unexpected notebook coverage"];
table = Prepend[rows, {"case_id", "family", "particles", "incoming", "outgoing", "notebook_weight", "notebook_leaves", "expression"}];
path = FileNameJoin[{destination, "catalog.tsv"}];
Export[path, StringJoin[(StringRiffle[#, "\t"] <> "\n") & /@ table], "Text"];
Print["Exported ", Length[rows], " constructions in ", Length[exports], " families."];
Quit[0];
