(* ::Package:: *)

(* Keep parameter names, dependent functions and their values inside each model.
   These routines create model-scoped symbolic references and resolve numerical
   substitutions from the supplied model and any explicit local overrides.
   This allows several models to use the same parameter names independently. *)

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

(* Numerical export must not turn a model's derived couplings into unrelated
   adjustable inputs. Expand its ordinary scalar definitions before emitting
   the amplitude. Explicit local overrides remain inputs. External functions
   such as running-quark-mass routines are deliberately left as model data. *)
modelExpandParameterDefinitions[expression_, model_List, overrides_: Automatic] := Module[
  {rows, supplied, definitions, expanded = {}, resolve, result, tag = Unique["modelDependencies"]},
  If[Length[model] =!= 4 || !ListQ[model[[3]]] || !ListQ[model[[4]]],
    Return[Failure["InvalidModelParameters", <||>]]];
  rows = Select[model[[4]], ListQ[#] && Length[#] >= 2 && modelParameterQ[First[#]] &];
  supplied = modelParameterOverrideRules[Replace[overrides, Automatic -> {}],
    First /@ Select[Join[model[[3]], rows], ListQ[#] && Length[#] >= 2 &]];
  If[!ListQ[supplied], Return[Failure["InvalidModelParameterOverrides", <||>]]];
  definitions = First[#] -> #[[2]] & /@ Select[rows,
    !MemberQ[First /@ supplied, First[#]] && FreeQ[#[[2]], _ModelFunction] &];
  resolve[parameter_ModelParameter, path_List] := Module[{cached, definition, value},
    cached = SelectFirst[expanded, First[#] === parameter &, Missing["NotExpanded"]];
    If[!MissingQ[cached], Return[Last[cached]]];
    definition = SelectFirst[definitions, First[#] === parameter &, Missing["Independent"]];
    If[MissingQ[definition], Return[parameter]];
    If[MemberQ[path, parameter], Throw[Failure["CyclicModelParameters", <|
      "Parameters" -> (ModelParameterName /@ Append[path, parameter])|>], tag]];
    value = Last[definition] /. item_ModelParameter :> resolve[item, Append[path, parameter]];
    AppendTo[expanded, parameter -> value]; value];
  result = Catch[expression /. parameter_ModelParameter :> resolve[parameter, {}], tag];
  result
];

(* Declared functions can cancel scalar coefficients without changing a
   propagator's mass or a typed contraction. Keep their original names when
   expanding them is less compact. Independent variable values never enter
   this presentation-only competitor. *)
preparationModelScalarCompaction[expression_, model_, seconds_ : 0.5] := Module[
  {started = AbsoluteTime[], budget, remaining, checkpoint, outcome, inverses,
   inverseTokens, protected, atoms, tokens, forward, backward, formal, profile,
   definitions, active, expandedOriginal, local, simplify, rank, candidate,
   inspect, trialRank, proof, coefficientCalls = 0, touched = {}, modes,
   overrides, supplied, rows},
  checkpoint = <|"Amplitude" -> expression, "Status" -> "UNCHANGED", "Route" -> {}|>;
  budget = If[NumericQ[seconds] && TrueQ[seconds > 0], Min[1, seconds], 0];
  If[budget <= 0 || LeafCount[expression] > 12000 ||
      !ListQ[model] || Length[model] =!= 4 || !ListQ[model[[3]]] || !ListQ[model[[4]]] ||
      FreeQ[expression, _ModelParameter] ||
      !FreeQ[expression, _Dot | _NonCommutativeMultiply | _sigma],
    Return[Join[checkpoint, <|"TimedOut" -> False, "ElapsedTime" -> AbsoluteTime[] - started,
      "InternalOnShellConditions" -> {}|>]]];
  remaining[] := Max[0.000001, budget - (AbsoluteTime[] - started)];
  outcome = TimeConstrained[MemoryConstrained[
    (* A constructor may explicitly override a dependent parameter. It then
       remains an independent symbolic input for this call, including when
       expanding another parameter transitively depends on it. *)
    overrides = If[ValueQ[$preparationModelParameterOverrides],
      $preparationModelParameterOverrides, Automatic];
    rows = Select[Join[model[[3]], model[[4]]], ListQ[#] && Length[#] >= 2 &&
      modelParameterQ[First[#]] &];
    supplied = modelParameterOverrideRules[Replace[overrides, Automatic -> {}], First /@ rows];
    definitions = If[ListQ[supplied], Select[model[[4]], ListQ[#] && Length[#] >= 2 &&
      modelParameterQ[First[#]] && !MemberQ[First /@ supplied, First[#]] &&
      FreeQ[#[[2]], _ModelFunction] &], {}];
    active = Intersection[First /@ definitions,
      DeleteDuplicates[Cases[expression, _ModelParameter, {0, Infinity}]]];
    If[active === {}, "NoDependentCoefficients",
      (* An inverse containing typed kinematics is one opaque object. In
         particular, no dependent mass is substituted inside PropDen. *)
      inverses = DeleteDuplicates[Cases[expression,
        inverse : Power[base_, exponent_] /; !TrueQ[exponent >= 0] &&
          !FreeQ[base, _SpinorChain | _SpinorTrace | _PropDen | _MomProd |
            _Mom | _Mass | _Spinor | _SpinorHat | _MomHat | _MomQ |
            _xFactor | _xTildeFactor | _Mandelstahm | _ColorGenerator |
            _ColorDelta | _ColorStructure | _ColorTrace] :> inverse, {0, Infinity}]];
      inverseTokens = Table[Unique["modelCoefficientInverse"], Length[inverses]];
      protected = expression /. Thread[inverses -> inverseTokens];
      atoms = DeleteDuplicates[Cases[protected,
        _SpinorChain | _SpinorTrace | _PropDen | _MomProd | _Mom | _Mass |
          _Spinor | _SpinorHat | _MomHat | _MomQ | _xFactor | _xTildeFactor |
          _Mandelstahm | _ColorGenerator | _ColorDelta | _ColorStructure |
          _ColorTrace, {0, Infinity}]];
      tokens = Table[Unique["modelCoefficientAtom"], Length[atoms]];
      forward = Join[Thread[atoms -> tokens], Thread[inverses -> inverseTokens]];
      backward = Join[Thread[tokens -> atoms], Thread[inverseTokens -> inverses]];
      formal = protected /. Thread[atoms -> tokens];
      profile = rspExpansionProfile[formal, 256, 24000];
      If[profile[[1]] > 256 || profile[[2]] > 24000, "ExpansionLimit",
        definitions = DeleteCases[Map[Function[parameter,
          With[{value = modelExpandParameterDefinitions[parameter, model, overrides]},
            If[FailureQ[value] || value === parameter, Nothing, parameter -> value]]], active], Nothing];
        If[definitions === {}, "NoDependentCoefficients",
          rank[value_] := preparationRank[value];
          checkpoint = Join[checkpoint, <|"Rank" -> rank[expression]|>];
          expandedOriginal = modelExpandParameterDefinitions[formal, model, overrides];
          If[FailureQ[expandedOriginal], "InvalidModelDefinitions",
            simplify[value_] := Factor[Cancel[value]];
            local[value_, mode_] := Module[{present, choices, best = value, bestRank, nextRank},
              If[++coefficientCalls > 256, Return[value]];
              present = Select[definitions, !FreeQ[value, First[#]] &];
              If[present === {}, Return[value]];
              touched = Union[touched, First /@ present];
              If[mode === "ExpandCoefficients", Return[simplify[value /. present]]];
              bestRank = rank[value];
              choices = DeleteDuplicates[Join[{simplify[value /. present]},
                simplify[value /. #] & /@ Take[present, UpTo[12]]]];
              Do[nextRank = rank[choice];
                If[preparationBetterQ[nextRank, bestRank], best = choice; bestRank = nextRank],
                {choice, choices}]; best];
            inspect[value_, mode_] := Module[{restored},
              restored = value /. backward;
              If[!FreeQ[restored, _Collect | _Factor | _Cancel] || !residueFiniteQ[restored], Return[Null]];
              trialRank = rank[restored];
              If[!preparationBetterQ[trialRank, checkpoint["Rank"]], Return[Null]];
              proof = modelExpandParameterDefinitions[value, model, overrides];
              If[FailureQ[proof], Return[Null]];
              proof = Together[proof - expandedOriginal];
              If[proof =!= 0, Return[Null]];
              checkpoint = <|"Amplitude" -> restored, "Rank" -> trialRank, "Status" -> "IMPROVED",
                "Route" -> {<|"Operation" -> "CompactDeclaredScalarCoefficients", "Mode" -> mode,
                  "Before" -> expression, "After" -> restored, "InternalOnShellConditions" -> {},
                  "EquivalenceProof" -> <|"Method" -> "ExactIndependentAtomModelDefinitionIdentity",
                    "NormalForm" -> proof, "DependentParameterDefinitions" -> definitions,
                    "ProtectedAtomSubstitutions" -> forward|>|>}|>];
            modes = {"SelectiveDefinitions", "ExpandCoefficients"};
            Do[candidate = Collect[formal, Join[tokens, inverseTokens], Function[value, local[value, mode]]];
              inspect[candidate, mode], {mode, modes}];
            "Complete"]]]], 134217728, "MemoryLimit"], remaining[], "TimeLimit"];
  Join[checkpoint, <|"TimedOut" -> MemberQ[{"TimeLimit", "MemoryLimit"}, outcome],
    "StopReason" -> outcome, "ElapsedTime" -> AbsoluteTime[] - started,
    "CoefficientCalls" -> coefficientCalls, "DependentParametersConsidered" -> touched,
    "InternalOnShellConditions" -> {}|>]
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
    "VectorSpinIndex" -> ConstructiveDiagrams`VectorSpinIndex,
    "ColorIndex" -> ConstructiveDiagrams`ColorIndex,
    "ColorDelta" -> ConstructiveDiagrams`ColorDelta,
    "ColorGenerator" -> ConstructiveDiagrams`ColorGenerator,
    "ColorStructure" -> ConstructiveDiagrams`ColorStructure,
    "ColorTrace" -> ConstructiveDiagrams`ColorTrace, "List" -> List|>;
  peek[] := If[position <= Length[tokens], tokens[[position]], EndOfFile];
  take[] := Module[{token = peek[]}, position++; token];
  expect[token_] := If[take[] =!= token, fail["Expected " <> token <> "."]];
  call[name_String, arguments_List] := Which[
    KeyExistsQ[scalarFunctions, name],
      If[!MemberQ[If[name === "Power", {2}, If[MemberQ[{"Log", "ArcTan"}, name], {1, 2}, {1}]],
          Length[arguments]] || !FreeQ[arguments, _Spinor | _SpinorChain | _Mom | _PropDen],
        fail["Invalid scalar function arguments."]];
      Lookup[scalarFunctions, name] @@ arguments,
    name === "ColorInternalIndex",
      If[Length[arguments] =!= 2 || !MemberQ[{3, 8}, Last[arguments]],
        fail["An internal color index needs a label and dimension 3 or 8."]];
      colorInternalIndex @@ arguments,
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
