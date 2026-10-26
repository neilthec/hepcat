(* ::Package:: *)

(* ::Section::Closed:: *)
(*Model Parameter Data*)

ModelParameter::usage = "ModelParameter[model, name] retrieves a declared parameter by its string name, or returns Failure when absent or ambiguous. ModelParameter[name, scope, domain] is its inert representation stored in a model. Its name is displayed without creating a Wolfram symbol. Only domain Real is automatically unchanged by conjugation. Imported models retain the earlier real-parameter convention.";
ModelFunction::usage = "ModelFunction[name, scope, arguments] retains an external model function as inert data. Reading a model never calls an external function with that name.";
ModelParameterName::usage = "ModelParameterName[parameter] returns the string name of a ModelParameter reference.";
ModelParameterRules::usage = "ModelParameterRules[model, overrides] returns locally resolved numerical parameter rules without assigning global values. Overrides default to Automatic and may use parameter references or string names. Unresolved dependencies, cycles and unsupported external functions are omitted; unrelated resolved parameters remain available.";

ModelParameterName[ModelParameter[name_String, _String, _String]] := name;
ModelParameter[model_List, name_String] := ConstructiveDiagrams`Private`modelParameterSelect[model, name];
ModelParameter[model_ConstructiveDiagrams`FeynmanModel, name_String] :=
  ConstructiveDiagrams`Private`modelParameterSelect[List @@ model, name];
ModelParameterRules[model_, overrides_: Automatic] := ConstructiveDiagrams`Private`residueParameterRules[
  If[Head[model] === ConstructiveDiagrams`FeynmanModel, List @@ model, model], overrides];

ModelParameter /: Conjugate[p : ModelParameter[_String, _String, "Real"]] := p;
ModelParameter /: MakeBoxes[p : ModelParameter[name_String, _String, _String],
    form : (StandardForm | TraditionalForm)] := InterpretationBox[name, p, Editable -> False];
Format[ModelParameter[name_String, _String, _String], OutputForm] := name;
ModelFunction /: MakeBoxes[f : ModelFunction[name_String, _String, arguments_List],
    form : (StandardForm | TraditionalForm)] := InterpretationBox[
  RowBox[{name, "[", RowBox[Riffle[MakeBoxes[#, form] & /@ arguments, ","]], "]"}], f,
  Editable -> False];
nice[ModelParameter[name_String, _String, _String]] := name;
PlainText[ModelParameter[name_String, _String, _String]] := name;
SPINAS[ModelParameter[name_String, _String, _String]] := name;
TeX[ModelParameter[name_String, _String, _String]] := name;
PlainText[ModelFunction[name_String, _String, args_List]] :=
  name <> "(" <> StringRiffle[PlainText /@ args, ","] <> ")";
SPINAS[ModelFunction[name_String, _String, args_List]] :=
  name <> "(" <> StringRiffle[SPINAS /@ args, ","] <> ")";
TeX[ModelFunction[name_String, _String, args_List]] :=
  name <> "\\left(" <> StringRiffle[TeX /@ args, ","] <> "\\right)";

Begin["ConstructiveDiagrams`Private`"];

modelParameterQ[value_] := MatchQ[value, ModelParameter[_String, _String, _String]];
modelParameterSelect[model_List, name_String] := Module[{parameters},
  If[Length[model] =!= 4 || !ListQ[model[[3]]] || !ListQ[model[[4]]],
    Return[Failure["InvalidModelParameters", <|"Name" -> name|>]]];
  parameters = DeleteDuplicates[Cases[Join[model[[3]], model[[4]]],
    row_List /; Length[row] >= 2 && modelParameterQ[First[row]] &&
      ModelParameterName[First[row]] === name :> First[row]]];
  If[Length[parameters] === 1, First[parameters],
    Failure[If[parameters === {}, "UnknownModelParameter", "AmbiguousModelParameter"],
      <|"Name" -> name|>]]
];
modelParameterScope[directory_String, number_] := Module[{dir, files, definitions},
  dir = DirectoryName[ExpandFileName[FileNameJoin[{directory, "HEPCATModelScope"}]]];
  files = FileNameJoin[{dir, # <> ToString[number] <> ".mdl"}] & /@ {"vars", "func"};
  definitions = If[AllTrue[files, FileExistsQ],
    Quiet[Check[Import[#, "Text"] & /@ files, $Failed]], $Failed];
  IntegerString[Hash[If[ListQ[definitions] && AllTrue[definitions, StringQ],
    {dir, definitions}, {dir, ToString[number]}], "SHA256"], 16, 64]
];
modelParameterScope[file_String] := Module[{base = FileBaseName[file], suffix},
  suffix = StringReplace[base, RegularExpression["^(?:prtcls|amps|vars|func|lgrng)"] -> ""];
  modelParameterScope[DirectoryName[ExpandFileName[file]], suffix]
];

modelExpressionNumber[text_String] := Module[
  {parts, mantissa, exponent = 0, point, scale = 0, sign = 1},
  parts = StringSplit[text, RegularExpression["[eE]|\\*\\^"]];
  mantissa = First[parts];
  If[Length[parts] === 2,
    If[StringStartsQ[parts[[2]], "-"], sign = -1];
    exponent = sign FromDigits[StringDelete[parts[[2]], {"+", "-"}]]];
  point = StringPosition[mantissa, "."];
  If[point =!= {}, scale = StringLength[mantissa] - First[First[point]]];
  FromDigits[StringDelete[mantissa, "."]] 10^(exponent - scale)
];

(* Parse only model arithmetic and typed amplitude objects. No input identifier
   is converted to a symbol or evaluated in the caller's context. *)
parseModelExpression[input_String, scope_String] := Module[
  {text, tokens, joined, position = 1, tag = Unique["modelExpression"],
   peek, take, expect, fail, primary, power, unary, product, sum, call, startsFactor,
   scalarFunctions, typedFunctions, result, numberPattern, tokenPattern},
  fail[reason_] := Throw[Failure["InvalidModelExpression", <|"Expression" -> input,
    "Reason" -> reason, "Token" -> position|>], tag];
  text = StringTrim[input];
  numberPattern = "(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:(?:[eE]|\\*\\^)[+-]?[0-9]+)?";
  tokenPattern = numberPattern <> "|[A-Za-z_][A-Za-z_0-9]*|\"[^\"\\\\]*\"|[+*/^(),\\[\\]\\-]";
  tokens = StringCases[text, RegularExpression[tokenPattern]];
  joined = StringJoin[tokens];
  If[text === "" || joined =!= StringReplace[text, WhitespaceCharacter -> ""],
    Return[Failure["InvalidModelExpression", <|"Expression" -> input,
      "Reason" -> "Unrecognized characters or an empty expression."|>]]];
  scalarFunctions = <|"Sqrt" -> Sqrt, "sqrt" -> Sqrt, "Sin" -> Sin, "sin" -> Sin,
    "Cos" -> Cos, "cos" -> Cos, "Tan" -> Tan, "tan" -> Tan, "ArcSin" -> ArcSin,
    "ArcCos" -> ArcCos, "ArcTan" -> ArcTan, "Exp" -> Exp, "exp" -> Exp,
    "Log" -> Log, "log" -> Log, "Abs" -> Abs, "abs" -> Abs, "Power" -> Power|>;
  typedFunctions = <|"SpinorChain" -> SpinorChain, "Spinor" -> Spinor,
    "SpinorTrace" -> SpinorTrace, "Mom" -> Mom, "MomProd" -> MomProd,
    "Multiparticle" -> Multiparticle, "Mass" -> Mass, "PropDen" -> PropDen,
    "xFactor" -> xFactor, "xTildeFactor" -> xTildeFactor,
    "Coupling" -> Coupling, "Mandelstahm" -> Mandelstahm,
    "VectorSpinIndex" -> ConstructiveDiagrams`VectorSpinIndex|>;
  peek[] := If[position <= Length[tokens], tokens[[position]], EndOfFile];
  take[] := Module[{token = peek[]}, position++; token];
  expect[token_] := If[take[] =!= token, fail["Expected " <> token <> "."]];
  call[name_String, arguments_List] := Which[
    KeyExistsQ[scalarFunctions, name],
      If[!MemberQ[If[name === "Power", {2}, If[MemberQ[{"Log", "ArcTan"}, name], {1, 2}, {1}]],
          Length[arguments]] || !FreeQ[arguments, _Spinor | _SpinorChain | _Mom | _PropDen],
        fail["Invalid scalar function arguments."]];
      Lookup[scalarFunctions, name] @@ arguments,
    KeyExistsQ[typedFunctions, name], Lookup[typedFunctions, name] @@ arguments,
    MemberQ[{"initQCD5", "MbEff", "McEff", "MqEff"}, name], ModelFunction[name, scope, arguments],
    True, fail["Unsupported model function: " <> name <> "."]];
  primary[] := Module[{token = take[], value, arguments = {}, closing},
    Which[
      token === "(", value = sum[]; expect[")"]; value,
      StringQ[token] && StringMatchQ[token, RegularExpression[numberPattern]], modelExpressionNumber[token],
      StringQ[token] && StringStartsQ[token, "\""], StringDrop[StringDrop[token, 1], -1],
      StringQ[token] && StringMatchQ[token, RegularExpression["[A-Za-z_][A-Za-z_0-9]*"]],
        If[MemberQ[{"(", "["}, peek[]],
          closing = If[take[] === "(", ")", "]"];
          If[peek[] =!= closing, AppendTo[arguments, sum[]];
            While[peek[] === ",", take[]; AppendTo[arguments, sum[]]]];
          expect[closing]; call[token, arguments],
          Switch[token, "I" | "i", I, "Pi", Pi, "Sqrt2", Sqrt[2],
            _, ModelParameter[token, scope, "Real"]]],
      True, fail["Expected a number, parameter or parenthesized expression."]]];
  power[] := Module[{value = primary[]}, If[peek[] === "^", take[]; value = value^unary[]]; value];
  unary[] := Which[peek[] === "+", take[]; unary[], peek[] === "-", take[]; -unary[], True, power[]];
  startsFactor[token_] := token === "(" || (StringQ[token] &&
    (StringMatchQ[token, RegularExpression[numberPattern]] ||
      StringMatchQ[token, RegularExpression["[A-Za-z_][A-Za-z_0-9]*"]]));
  product[] := Module[{value = unary[], operation, next},
    While[MemberQ[{"*", "/"}, peek[]] || startsFactor[peek[]],
      operation = If[MemberQ[{"*", "/"}, peek[]], take[], "*"]; next = unary[];
      value = If[operation === "*", value next, value/next]]; value];
  sum[] := Module[{value = product[], operation},
    While[MemberQ[{"+", "-"}, peek[]], operation = take[];
      value = value + If[operation === "+", product[], -product[]]]; value];
  Catch[result = sum[];
    If[position <= Length[tokens], fail["Unexpected trailing tokens."]];
    If[!FreeQ[result, Indeterminate | ComplexInfinity | _DirectedInfinity],
      fail["The model expression is not finite."]]; result, tag]
];

End[];
