(* Automatic color notation must remain an executable typed expression. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root, "source", "HEPCAT.wl"}]];
$HistoryLength = 0;
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, assertion_] := (checks++; If[!TrueQ[assertion],
  AppendTo[failures, name]; Print["FAIL: ", name]]);
ci[n_] := ColorIndex[n];
internal[label_, dimension_] := ConstructiveDiagrams`Private`colorInternalIndex[label, dimension];
cdBoxes[expression_, form_] := Replace[ToBoxes[expression, form],
  FormBox[inside_, TraditionalForm] :> inside];
cdVisible[expression_, form_] := Replace[cdBoxes[expression, form],
  InterpretationBox[visible_, _, ___] :> visible];
cdExpressions = {
  ci[1], ci[12], ColorDelta[ci[1], ci[2]],
  ColorGenerator[ci[2], ci[1], ci[3]],
  ColorStructure[ci[3], ci[4], ci[5]],
  ColorTrace[cdA, cdB, cdC], ColorTrace[],
  internal[{1, 3}, 8], internal[cdDummy, 3],
  ColorGenerator[ci[2], internal[{1, 3}, 3], ci[3]]
    ColorGenerator[internal[{1, 3}, 3], ci[1], ci[4]],
  I ColorStructure[ci[1], ci[2], internal[{3, 4}, 8]]^2/3 +
    ModelParameter["g", "colorDisplay", "Real"] ColorDelta[ci[1], ci[2]],
  ColorTrace[cdC, cdA, cdB] + 2 ColorTrace[cdA, cdB, cdC]};
Do[
  check["color notebook round trip " <> ToString[cdForm] <> " " <> ToString[cdIndex],
    With[{expected = cdExpressions[[cdIndex]]},
      ReleaseHold[ToExpression[ToBoxes[expected, cdForm], cdForm, HoldComplete]] === expected]],
  {cdForm, {StandardForm, TraditionalForm}}, {cdIndex, Length[cdExpressions]}];
Do[
  check["external color labels have a neutral subscript " <> ToString[cdForm],
    cdVisible[ci[12], cdForm] === ToBoxes[Subscript["c", 12], cdForm]];
  check["the delta appears as an indexed Kronecker tensor " <> ToString[cdForm],
    !FreeQ[cdVisible[ColorDelta[ci[1], ci[2]], cdForm], _SubscriptBox]];
  check["generator display retains both ordered fundamental indices " <> ToString[cdForm],
    cdVisible[ColorGenerator[ci[2], ci[1], ci[3]], cdForm] =!=
      cdVisible[ColorGenerator[ci[1], ci[2], ci[3]], cdForm] &&
      !FreeQ[cdVisible[ColorGenerator[ci[2], ci[1], ci[3]], cdForm],
        _SuperscriptBox | TemplateBox[_, "Superscript", ___]]];
  check["the structure tensor has three ordered upper indices " <> ToString[cdForm],
    !FreeQ[cdVisible[ColorStructure[ci[1], ci[2], ci[3]], cdForm],
      _SuperscriptBox | TemplateBox[_, "Superscript", ___]] &&
      cdVisible[ColorStructure[ci[1], ci[2], ci[3]], cdForm] =!=
        cdVisible[ColorStructure[ci[2], ci[1], ci[3]], cdForm]];
  check["trace display keeps generator order " <> ToString[cdForm],
    cdVisible[ColorTrace[cdA, cdB, cdC], cdForm] =!=
      cdVisible[ColorTrace[cdA, cdC, cdB], cdForm]];
  check["internal representations have distinct compact dummy labels " <> ToString[cdForm],
    cdVisible[internal[{1, 3}, 3], cdForm] =!= cdVisible[internal[{1, 3}, 8], cdForm]];
  check["different internal edges remain visually distinguishable " <> ToString[cdForm],
    cdVisible[internal[{1, 23}, 8], cdForm] =!= cdVisible[internal[{12, 3}, 8], cdForm]];
  check["visible internal notation exposes no implementation wrappers " <> ToString[cdForm],
    FreeQ[cdVisible[internal[{1, 3}, 8], cdForm],
      text_String /; StringContainsQ[text, "Private`" | "colorInternalIndex" | "ColorIndex"]]];
  check["the visible form uses the same internal nice convention " <> ToString[cdForm],
    And @@ Table[cdVisible[cdExpressions[[cdIndex]], cdForm] ===
      ToBoxes[ConstructiveDiagrams`nice[cdExpressions[[cdIndex]]], cdForm],
      {cdIndex, {1, 3, 4, 5, 6, 7, 8, 9}}]],
  {cdForm, {StandardForm, TraditionalForm}}];
check["raw InputForm remains explicit and executable",
  And @@ Table[ReleaseHold[ToExpression[ToString[cdExpression, InputForm],
    InputForm, HoldComplete]] === cdExpression, {cdExpression, cdExpressions}]];
check["OutputForm retains the explicit public tensor head",
  StringContainsQ[ToString[ColorGenerator[ci[2], ci[1], ci[3]], OutputForm], "ColorGenerator"]];
check["unreduced empty traces keep their interpretation",
  !FreeQ[cdBoxes[ColorTrace[], StandardForm],
    InterpretationBox[_, ColorTrace[], Editable -> False]]];
Print["Color display: ", checks, " checks, ", Length[failures], " failures."];
If[failures =!= {}, Print[failures]];
Print["HEPCAT_TEST_COMPLETED:color-display-tests.wl"];
Quit[If[failures === {}, 0, 1]];
