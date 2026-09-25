(* Exercise the notebook's sample and seeded call without the neural backend. *)
root = DirectoryName[DirectoryName[$InputFileName]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{Global`$HEPCATpath, "HEPCAT.wl"}]];
Get[FileNameJoin[{root, "tests", "unscrambling.wl"}]];
notebook = Get[FileNameJoin[{root, "tests", "unscrambling.nb"}]];
inputs = Cases[notebook, Cell[s_String, "Input", ___] :> s, Infinity];
ToExpression[SelectFirst[inputs, StringContainsQ[#, "sampleAmplitudes ="] &]];
scr = ComplicateAmplitude[sampleAmplitudes[[3]], 33, 7];
failures = {}; checks = 0;
check[name_, result_] := (checks++; If[!TrueQ[result], AppendTo[failures, name]]);
check["seeded notebook call returns association", AssociationQ[scr]];
check["requested steps performed", scr["Steps"] === 33];
check["scrambled expression changes", scr["Expression"] =!= scr["Original"]];
check["second entry is original, not scrambled", scr[[2]] === scr["Original"]];
boxedInputs = Cases[notebook, Cell[BoxData[b_], "Input", ___] :> b, Infinity];
check["notebook does not index scr positionally",
  FreeQ[boxedInputs, RowBox[{"scr", "[", RowBox[{"[", _, "]"}], "]"}]]];
Print[<|"Checks" -> checks, "Failed" -> Length[failures], "Failures" -> failures|>];
Quit[If[failures === {}, 0, 1]];
