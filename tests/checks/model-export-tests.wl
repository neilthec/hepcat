(* Reversible plain-text amplitude export and atomic constructive-model append. *)
meRoot = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{meRoot, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
meChecks = 0; meFailures = {};
SetAttributes[meCheck, HoldRest];
meCheck[name_, assertion_] := Module[{answer},
  meChecks++;
  answer = TimeConstrained[Quiet[Check[assertion, $Failed]], 20, $TimedOut];
  If[!TrueQ[answer], AppendTo[meFailures, name]; Print["FAIL: ", name]]
];
meDirectory = CreateDirectory[];
meWrite[file_, text_] := Export[FileNameJoin[{meDirectory, file}], text, "Text"];
meWrite["prtcls91.mdl", StringRiffle[{
  "Export fixture", "Full Name | P | aP | number | spin2 | mass | width | color | aux | Latex | LatexAnti",
  "electron | e | E | 11 | 1 | Me | 0 | 1 |   | e | E",
  "photon | A.+ | A.- | 22 | 2 | 0 | 0 | 1 |   | A | A",
  "scalar | h | h | 25 | 0 | Me | 0 | 1 |   | h | h", "======"}, "\n"] <> "\n"];
meWrite["vars91.mdl", "Export fixture\nName | Value | Comment\nMe | 3/2 | mass\nEE | 1/3 | coupling\n======\n"];
meWrite["func91.mdl", "Export fixture\nName | Expression\nDoubleEE | 2*EE\n======\n"];
meOriginal = "Export fixture\r\n Amplitudes\r\nP1 | P2 | P3 | P4 | Factor | Amplitude/Factor\r\n" <>
  "% retained comment\r\ne | E | A.+ |   | -Sqrt[2]*EE | x12*<12>\r\n" <>
  "e | E | A.- |   | -Sqrt[2]*EE | xt12*[12]\r\n" <>
  "================================================================\r\n";
meFile = FileNameJoin[{meDirectory, "amps91.mdl"}];
CopyFile[FileNameJoin[{meRoot, "models", "amps3.mdl"}], meFile];
meStream = OpenWrite[meFile, CharacterEncoding -> "UTF-8"];
WriteString[meStream, meOriginal];
Close[meStream];
mePermissions = Lookup[Association[FileInformation[meFile]], "UnixPermissionsCode", Missing["Unavailable"]];
meModel = ReadModel[meDirectory, 91];
meMass = ModelParameter[meModel, "Me"];
meCharge = ModelParameter[meModel, "EE"];
meDerived = ModelParameter[meModel, "DoubleEE"];
meScope = ConstructiveDiagrams`Private`modelParameterScope[meDirectory, 91];
meAngle[i_, j_, left_: "Spin", right_: "Spin"] :=
  SpinorChain[Spinor[left, "Angle", i], Spinor[right, "Angle", j]];
meSquare[i_, j_, left_: "Helicity", right_: "Helicity"] :=
  SpinorChain[Spinor[left, "Square", i], Spinor[right, "Square", j]];
meFour = 2 meCharge^2 meMass meAngle[1, 2] meSquare[3, 4]^2 /
  (PropDen[Mom[Multiparticle[1, 3]], meMass] PropDen[Mom[Multiparticle[1, 4]], meMass]);
meAppend = AppendModelAmplitudes[meDirectory, 91,
  {{{"e", "E", "A.+", "A.+"}, meFour}}];
meCheck["append returns a compact path and counts",
  meAppend === <|"File" -> meFile, "Added" -> 1, "Skipped" -> 0|>];
meCheck["atomic append preserves the model's file permissions",
  !MissingQ[mePermissions] &&
    Lookup[Association[FileInformation[meFile]], "UnixPermissionsCode", Missing["Unavailable"]] === mePermissions];
meAfter = FromCharacterCode[BinaryReadList[meFile, "Byte"], "UTF-8"];
meCheck["original rows comments and line endings remain unchanged",
  StringStartsQ[meAfter, First[StringSplit[meOriginal, "================================================================"]]] &&
    StringEndsQ[meAfter, "================================================================\r\n"] &&
    !StringContainsQ[StringDelete[meAfter, "\r\n"], "\n"]];
meReload = ReadModel[meDirectory, 91];
meCheck["reloading retrieves the exact amplitude without assigning parameters",
  Last[meReload[[2]]][[-2]] Last[meReload[[2]]][[-1]] === meFour &&
    ModelParameter[meReload, "Me"] === meMass];
meBytes = BinaryReadList[meFile, "Byte"];
meAgain = AppendModelAmplitudes[meDirectory, 91,
  {{{"e", "E", "A.+", "A.+"}, meFour}}];
meCheck["an identical append is idempotent and does not rewrite the file",
  meAgain["Added"] === 0 && meAgain["Skipped"] === 1 && BinaryReadList[meFile, "Byte"] === meBytes];

meFiveParts = {"e", "E", "A.+", "A.+", "A.+"};
meFive = meDerived^3 meAngle[1, 2] meSquare[3, 4]^2 meSquare[4, 5]^2 /
  (PropDen[Mom[Multiparticle[1, 3, 4]], meMass] PropDen[Mom[Multiparticle[2, 5]], meMass]);
meElevenParts = Join[{"e", "E"}, ConstantArray["A.+", 9]];
meEleven = SpinorChain[Spinor["Spin", "Angle", 1], Mom[10], Mom[Multiparticle[3, 10, 11]],
    Spinor["Spin", "Angle", 2]] meSquare[10, 11] /
  PropDen[Mom[Multiparticle[1, 3, 10]], meMass];
meBatch = AppendModelAmplitudes[meDirectory, 91,
  {{meFiveParts, meCharge^3, meFive/meCharge^3}, {meElevenParts, meEleven},
   {ConstantArray["h", 6], meDerived^2/PropDen[Mom[Multiparticle[1, 2, 3]], meMass]}}];
meCheck["batch append accepts arbitrary multiplicity and a separate factor",
  meBatch["Added"] === 3 && meBatch["Skipped"] === 0];
meReload = ReadModel[meDirectory, 91];
meCheck["five-point factor and amplitude read back exactly",
  meReload[[2, -3, -2]] meReload[[2, -3, -1]] === meFive];
meCheck["multi-digit labels and long composite momentum chains survive export",
  meReload[[2, -2, -2]] meReload[[2, -2, -1]] === meEleven];
meCheck["new multiplicities receive readable matching column headers",
  StringContainsQ[Import[meFile, "Text"], "P1 | P2 | P3 | P4 | P5 | Factor | Amplitude/Factor"] &&
    StringContainsQ[Import[meFile, "Text"], "P9 | P10 | P11 | Factor | Amplitude/Factor"]];
meCheck["existing rows retain their ordering and scope",
  Take[meReload[[2]], Length[meModel[[2]]]] === meModel[[2]]];
meCheck["new amplitudes participate in ordinary lookup with typed relabeling",
  LookupAmplitude[Thread[{meFiveParts, {5, 4, 3, 2, 1}}], meReload, False, {meI, meJ, meK, meL}] =!= 0];

(* A failed member rejects the entire batch, including previously valid members. *)
meBytes = BinaryReadList[meFile, "Byte"];
meBadEntries = {
  {{{"e", "E", "A.-", "A.-"}, meFour}, {{"e", "unknown", "A.+"}, 1}},
  {{{"e", "E", "A.-", "A.-"}, meFour}, {{"e", "E", "A.+", "A.+"}, 2 meFour}},
  {{{"e", "E", "A.-", "A.-"}, meFour}, {{"E", "e", "A.+", "A.+"}, meFour}},
  {{{"e", "E", "A.-", "A.-"}, meFour}, {{"e", "E", "A.+", "A.-"}, meUnknownSymbol}},
  {{{"e", "E", "A.-", "A.-"}, meFour}, {{"e", "E", "A.+", "A.-"}, meUnknownHead[1]}},
  {{{"e", "E", "A.-", "A.-"}, meFour}, {{"e", "E", "A.+", "A.-"}, Indeterminate}},
  {{{"e", "E", "A.-", "A.-"}, meFour}, {{"e", "E", "A.+", "A.-"},
    ModelParameter["EE", "AnotherModel", "Real"]}},
  {{{"e", "E", "A.-", "A.-"}, meFour}, {{"e", "E", "A.+", "A.-"},
    ModelParameter["notDeclared", meScope, "Real"]}},
  {{{"e", "E", "A.-", "A.-"}, meFour}, {{"e", "E", "A.+", "A.-"},
    SpinorChain[Spinor["Spin|Other", "Angle", 1], Spinor["Spin", "Angle", 2]]}},
  {{{"e", "E", "A.-", "A.-"}, meFour}, {{"e", "E", "A.+", "A.-"},
    SpinorChain[Spinor["Spin\nOther", "Angle", 1], Spinor["Spin", "Angle", 2]]}},
  {{{"e", "E"}, 1}}, {1}
};
Do[
  meRejected = AppendModelAmplitudes[meDirectory, 91, meBadEntries[[meIndex]]];
  meCheck["invalid batch fails without any partial write " <> ToString[meIndex],
    FailureQ[meRejected] && BinaryReadList[meFile, "Byte"] === meBytes],
  {meIndex, Length[meBadEntries]}];
meCheck["no temporary amplitude files survive failed batches",
  FileNames[".amps*.tmp", meDirectory] === {}];
meCheck["empty batch is a no-op",
  AppendModelAmplitudes[meDirectory, 91, {}] === <|"File" -> meFile, "Added" -> 0, "Skipped" -> 0|> &&
    BinaryReadList[meFile, "Byte"] === meBytes];
meCheck["malformed public calls fail explicitly",
  AllTrue[{AppendModelAmplitudes[], AppendModelAmplitudes[meDirectory, "91", {}],
    AppendModelAmplitudes[meDirectory, 91, 1]}, FailureQ]];
meCheck["missing model fails without creating an amplitude file",
  FailureQ[AppendModelAmplitudes[meDirectory, 92, {}]] &&
    !FileExistsQ[FileNameJoin[{meDirectory, "amps92.mdl"}]]];
meCheck["typed momentum arguments are not interpreted as compact brackets",
  ConvertAmplitude[Join[meElevenParts, {1, "Mom[10]"}], meModel[[1]], meScope] === Mom[10]];
meCheck["whitespace before typed argument brackets is preserved safely",
  ConvertAmplitude[Join[meElevenParts, {1, "Mom \t[10]"}], meModel[[1]], meScope] === Mom[10] &&
    ConvertAmplitude[Join[meElevenParts, {1, "SpinorChain [Spinor [\"Spin\",\"Angle\",1],Mom [10],Spinor [\"Spin\",\"Square\",2]]"}],
      meModel[[1]], meScope] ===
        SpinorChain[Spinor["Spin", "Angle", 1], Mom[10], Spinor["Spin", "Square", 2]]];
meCheck["parameter names containing legacy-looking substrings are preserved",
  ConvertAmplitude[Join[meFiveParts, {1, "mass12+x12suffix+prefixs12"}],
    meModel[[1]], meScope] === Total[ModelParameter[#, meScope, "Real"] & /@
      {"mass12", "x12suffix", "prefixs12"}]];
meCheck["invalid compact external labels fail without part errors",
  FailureQ[ConvertAmplitude[{"e", "E", "A.+", 1, "[10]"}, meModel[[1]], meScope]]];
meCheck["malformed quotes cannot disappear during legacy notation conversion",
  AllTrue[{"Me\"", "\"Me", "\"bad\\escape\""},
    FailureQ[ConvertAmplitude[{"e", "E", "A.+", 1, #}, meModel[[1]], meScope]] &]];
Do[
  meCompact = meBrackets[[1]] <> "512345" <> meBrackets[[2]];
  meExpected = SpinorChain[Spinor["Helicity", meBrackets[[3]], 5],
    Mom[1], Mom[2], Mom[3], Mom[4], Spinor["Helicity", meBrackets[[4]], 5]];
  meCheck["long compact momentum chain " <> meCompact,
    ConvertAmplitude[Join[meFiveParts, {1, meCompact}], meModel[[1]], meScope] === meExpected],
  {meBrackets, {{"<", ">", "Angle", "Angle"}, {"[", "]", "Square", "Square"},
    {"<", "]", "Angle", "Square"}, {"[", ">", "Square", "Angle"}}}];
meRepeated = AppendModelAmplitudes[meDirectory, 91,
  {{{"e", "E", "A.-", "A.-"}, meFour}, {{"e", "E", "A.-", "A.-"}, meFour}}];
meCheck["identical new entries within one batch are written only once",
  meRepeated["Added"] === 1 && meRepeated["Skipped"] === 1];
meBytes = BinaryReadList[meFile, "Byte"];
meWrite["amps91.mdl", FromCharacterCode[meBytes, "UTF-8"] <> "ignored amplitude after separator\n"];
meTrailingBytes = BinaryReadList[meFile, "Byte"];
meCheck["trailing data is rejected instead of silently moving it into the model",
  FailureQ[AppendModelAmplitudes[meDirectory, 91, {}]] &&
    BinaryReadList[meFile, "Byte"] === meTrailingBytes];

DeleteDirectory[meDirectory, DeleteContents -> True];
Print["Model export checks: ", meChecks, "; failures: ", Length[meFailures]];
If[meFailures =!= {}, Print[meFailures]; Exit[1]];
Print["HEPCAT_TEST_COMPLETED:model-export-tests.wl"];
Quit[0];
