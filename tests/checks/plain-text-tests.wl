(* Lossless model text and readable plain-text regression coverage. *)

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
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
checks = 0; completedChecks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, assertion_] := Module[{result},
  checks++;
  result = TimeConstrained[Check[assertion, False], 10, False];
  If[!TrueQ[result], AppendTo[failures, name]; Print["FAIL: ", name]];
  completedChecks++];
ltH[t_, p_] := Spinor["Helicity", t, p];
ltS[t_, p_] := Spinor["Spin", t, p];
ltU[t_, p_, i_] := Spinor["Spin", "Upper", t, p, i];
ltL[t_, p_, i_] := Spinor["Spin", "Lower", t, p, i];
ltB[t_, i_, j_] := SpinorChain[ltH[t, i], ltH[t, j]];

check["PlainText multiplication separates numerical factors and names",
 PlainText[2 ModelParameter["EE", "textTest", "Real"] ltB["Angle", 1, 2]] === "2*EE*<12>"];
check["PlainText distinguishes a momentum insertion from endpoint labels",
 PlainText[SpinorChain[ltH["Angle", 1], Mom[3], ltH["Square", 2]]] === "<1|3|2]"];
check["PlainText supports arbitrary composite lengths and multiple-digit labels",
 PlainText[Mass[Multiparticle[1, 2, 3, 4]]] === "M_{1234} " &&
 PlainText[Mom[Multiparticle[1, 10, 11, 12]]] === "p_{1,10,11,12} "];
check["PlainText groups a fractional exponent instead of presenting an outer division",
 StringMatchQ[PlainText[ltTextPowerBase^(1/3)], "ltTextPowerBase^(" ~~ __ ~~ ") "]];
check["PlainText groups a product base before applying a fractional exponent",
 StringStartsQ[PlainText[(ltTextPowerLeft ltTextPowerRight)^(1/3)], "(ltTextPowerLeft*ltTextPowerRight)^("]];
ltModelTextRead[expression_, masses_List] := Module[{text, names, particles},
 text = PlainText[expression, "Model"];
 If[!StringQ[text], Return[text]];
 names = Table["textParticle" <> ToString[i], {i, Length[masses]}];
 particles = MapThread[{"text particle", #1, #1, 0, 0, #2} &, {names, masses}];
 ConvertAmplitude[Join[names, {1, text}], particles, "textTest"]];
ltTextMass = ModelParameter["Me", "textTest", "Real"];
ltTextCharge = ModelParameter["EE", "textTest", "Real"];
ltTextMasses = {ltTextMass, ltTextMass, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
ltTextExpressions = {
 0, 1, -3/7, 2 + 3 I, Pi,
 ltTextCharge^3 ltTextMass,
 SpinorChain[ltS["Angle", 1], ltS["Angle", 2]],
 SpinorChain[ltH["Square", 3], ltH["Square", 4]]^2,
 SpinorChain[ltS["Square", 1], Mom[3], ltS["Angle", 2]],
 SpinorChain[ltH["Angle", 10], ltH["Angle", 11]],
 SpinorChain[ltS["Square", 1], Mom[Multiparticle[3, 4, 5, 6]], Mom[10], ltH["Square", 12]],
 SpinorChain[ltH["Angle", 3], Mom[4] - Mom[5], ltH["Square", 6]],
 SpinorChain[ltU["Angle", 1, 1], ltL["Square", 2, 2]],
 SpinorChain[ltU["Angle", 1, VectorSpinIndex[1, 1]], ltL["Square", 2, VectorSpinIndex[2, 2]]],
 SpinorChain[Spinor["Xi", "Angle", 10], ltH["Square", 12]],
 SpinorTrace[Mom[1], Mom[2], Mom[Multiparticle[3, 4, 5, 6]], Mom[10]],
 MomProd[1, 10] + Mass[Multiparticle[3, 4, 5, 6]]^2,
 1/PropDen[Mom[Multiparticle[1, 3, 4, 5]], ltTextMass],
 1/PropDen[Mom[Multiparticle[1, 3, 4, 5]], ltTextMass, ltTextCharge],
 Sqrt[2] ltTextCharge^3 (ltB["Square", 3, 4]^2 ltB["Square", 5, 6] -
   ltB["Square", 3, 5]^2 ltB["Square", 4, 6])/
   (PropDen[Mom[Multiparticle[1, 3]], ltTextMass] PropDen[Mom[Multiparticle[2, 5]], ltTextMass]),
 ModelFunction["MbEff", "textTest", {ltTextMass, 2}],
 ModelParameter["mass12", "textTest", "Real"] + ModelParameter["x12value", "textTest", "Real"],
 xFactor[Multiparticle[1, 3, 4, 5], 10] + xTildeFactor[10, Multiparticle[2, 6, 7, 8]],
 SpinorChain[ltU["Angle", 1, "state10"], ltL["Square", 2, "state11"]],
 Sin[ltTextCharge]^2 + Cos[ltTextCharge]^2};
Do[check["model plain-text round trip " <> ToString[ltTextIndex],
 ltModelTextRead[ltTextExpressions[[ltTextIndex]], ltTextMasses] === ltTextExpressions[[ltTextIndex]]],
 {ltTextIndex, Length[ltTextExpressions]}];
check["model plain text preserves familiar unambiguous bracket notation",
 PlainText[ltB["Angle", 3, 4], "Model"] === "<34>" &&
 StringContainsQ[PlainText[ltH["Angle", 10] // (SpinorChain[#, ltH["Angle", 11]] &), "Model"], "SpinorChain"]];
check["model plain text compactly preserves ordinary multi-momentum bracket chains",
 PlainText[SpinorChain[ltH["Square", 5], Mom[1], Mom[2], ltH["Square", 5]], "Model"] === "[5125]" &&
 PlainText[SpinorChain[ltS["Angle", 1], Mom[2], ltH["Square", 3]], "Model"] === "<123]"];
check["compact model text round trips every chirality and several chain lengths",
 And @@ Flatten[Table[
   With[{chain = SpinorChain[ltS[leftType, 1],
       Sequence @@ Table[Mom[3 + Mod[k, 3]], {k, insertions}], ltS[rightType, 2]]},
     ltModelTextRead[chain, ltTextMasses] === chain],
   {leftType, {"Angle", "Square"}}, {rightType, {"Angle", "Square"}}, {insertions, 0, 5}]]];
check["model plain text uses familiar two-particle denominators and square roots",
 PlainText[PropDen[Mom[Multiparticle[1, 3]], ltTextMass], "Model"] === "(s13-Me^2)" &&
 PlainText[PropDen[Mom[Multiparticle[1, 4]], 0], "Model"] === "s14" &&
 PlainText[Sqrt[2], "Model"] === "Sqrt[2]"];
check["model plain text rejects an unsupported symbol or function instead of dropping it",
 FailureQ[PlainText[ltUnknownText, "Model"]] &&
 FailureQ[PlainText[ltUnknownText[1], "Model"]] &&
 FailureQ[PlainText[ModelFunction["UnknownFunction", "textTest", {1}], "Model"]]];
check["model plain text rejects reserved or malformed parameter identifiers",
 FailureQ[PlainText[ModelParameter["I", "textTest", "Real"], "Model"]] &&
 FailureQ[PlainText[ModelParameter["bad name", "textTest", "Real"], "Model"]] &&
 FailureQ[PlainText[ModelParameter["x12", "textTest", "Real"], "Model"]] &&
 FailureQ[PlainText[ModelParameter["EE", "textTest", "Complex"], "Model"]]];
check["model plain text rejects row delimiters and control characters in typed strings",
 FailureQ[PlainText[Spinor["Spin|Other", "Angle", 1], "Model"]] &&
 FailureQ[PlainText[Spinor["Spin\tOther", "Angle", 1], "Model"]] &&
 FailureQ[PlainText[Spinor["Spin\nOther", "Angle", 1], "Model"]] &&
 FailureQ[PlainText[Spinor["Spin\rOther", "Angle", 1], "Model"]]];
check["model plain text rejects inexact reals that the model reader would exactify",
 FailureQ[PlainText[0.125, "Model"]] &&
 FailureQ[PlainText[N[1/7, 40], "Model"]]];

check["ordinary plain text keeps a negative power base inside parentheses",
 PlainText[(-2)^ltTextExponent] === "(-2)^(ltTextExponent) "];

ltTextColorGenerator = ColorGenerator[ColorIndex[2], ColorIndex[1], ColorIndex[3]];
ltTextColorTrace = ColorTrace[ColorIndex[3], ColorIndex[4]];
ltTextFundamentalDummy = ConstructiveDiagrams`Private`colorInternalIndex[{1, 23}, 3];
ltTextAdjointDummy = ConstructiveDiagrams`Private`colorInternalIndex[{12, 3}, 8];
check["plain text displays an external color label without a wrapper",
 PlainText[ColorIndex[12]] === "c_{12}"];
check["plain text displays the color identity with distinct ordered labels",
 PlainText[ColorDelta[ColorIndex[1], ColorIndex[2]]] === "delta_{c_{1},c_{2}}"];
check["plain text preserves the antiquark quark and gluon generator slots",
 PlainText[ltTextColorGenerator] === "(T^{c_{3}})_{c_{2},c_{1}}"];
check["plain text displays real structure constants without an extra phase",
 PlainText[ColorStructure[ColorIndex[3], ColorIndex[4], ColorIndex[5]]] ===
  "f^{c_{3},c_{4},c_{5}}"];
check["plain text preserves the ordered generators of a color trace",
 PlainText[ltTextColorTrace] === "Tr(T^{c_{3}} T^{c_{4}})" &&
 PlainText[ColorTrace[]] === "Tr()"];
check["plain text separates internal edge labels and their representation types",
 AllTrue[{"d_{1,23}", "a_{12,3}"},
  StringContainsQ[PlainText[ColorGenerator[ColorIndex[1],
    ltTextFundamentalDummy, ltTextAdjointDummy]], #] &]];
check["plain text combines color factors with ordinary amplitude coefficients",
 StringContainsQ[PlainText[ltTextCharge ltTextColorGenerator], "EE"] &&
 StringContainsQ[PlainText[ltTextCharge ltTextColorGenerator], "*"] &&
 StringContainsQ[PlainText[ltTextCharge ltTextColorGenerator], "(T^{c_{3}})"]];
check["TeX displays an external color label without a wrapper",
 TeX[ColorIndex[12]] === "c_{12}"];
check["TeX displays the color identity with distinct ordered labels",
 TeX[ColorDelta[ColorIndex[1], ColorIndex[2]]] === "\\delta_{c_{1},c_{2}}"];
check["TeX preserves the antiquark quark and gluon generator slots",
 TeX[ltTextColorGenerator] === "\\left(T^{c_{3}}\\right)_{c_{2},c_{1}}"];
check["TeX displays real structure constants without an extra phase",
 TeX[ColorStructure[ColorIndex[3], ColorIndex[4], ColorIndex[5]]] ===
  "f^{c_{3},c_{4},c_{5}}"];
check["TeX preserves the ordered generators of a color trace",
 TeX[ltTextColorTrace] === "\\operatorname{Tr}\\left(T^{c_{3}}\\,T^{c_{4}}\\right)" &&
 TeX[ColorTrace[]] === "\\operatorname{Tr}\\left(\\right)"];
check["TeX separates internal edge labels and their representation types",
 AllTrue[{"d_{1,23}", "a_{12,3}"},
  StringContainsQ[TeX[ColorGenerator[ColorIndex[1],
    ltTextFundamentalDummy, ltTextAdjointDummy]], #] &]];
check["TeX combines color factors with ordinary amplitude coefficients",
 StringQ[TeX[ltTextCharge ltTextColorGenerator]] &&
 StringContainsQ[TeX[ltTextCharge ltTextColorGenerator], "EE"] &&
 StringContainsQ[TeX[ltTextCharge ltTextColorGenerator], "T^{c_{3}}"]];
check["readable color formatting never alters the underlying tensors",
 With[{before = {ltTextColorGenerator, ltTextColorTrace}},
  PlainText /@ before; TeX /@ before;
  before === {ColorGenerator[ColorIndex[2], ColorIndex[1], ColorIndex[3]],
   ColorTrace[ColorIndex[3], ColorIndex[4]]}]];
check["lossless model text round trips complete typed color expressions",
 AllTrue[{ColorIndex[1], ColorDelta[ColorIndex[1], ColorIndex[2]],
  ltTextColorGenerator, ColorStructure[ColorIndex[1], ColorIndex[2], ColorIndex[3]],
  ltTextColorTrace, ColorGenerator[ColorIndex[1], ltTextFundamentalDummy, ColorIndex[3]]},
  ConstructiveDiagrams`Private`parseModelExpression[PlainText[#, "Model"], "colorTextTest"] === # &]];


(* Plus has OneIdentity: Plus[a__] evaluates to a__ while a definition is
   installed. Match the actual Plus head, then visit strictly smaller terms. *)
check["readable plain text formats a genuine two-term sum",
 PlainText[ltTextSumLeft + ltTextSumRight] === "(ltTextSumLeft+ltTextSumRight)"];
check["readable plain text formats a genuine three-term sum",
 PlainText[1 + ltTextSumLeft + ltTextSumRight] === "(1+ltTextSumLeft+ltTextSumRight)"];
check["readable plain text terminates on nested square-root sums",
 PlainText[Sqrt[1 + Sqrt[1 + ltTextSumLeft]]] === "sqrt((1+sqrt((1+ltTextSumLeft))))"];
ltTextSM = ReadModel[FileNameJoin[{root, "models"}], 1];
ltTextQED = ReadModel[FileNameJoin[{root, "models"}], 3];
ltTextWeakDefinition = SelectFirst[ltTextSM[[4]], ModelParameterName[First[#]] === "CW" &][[2]];
ltTextWeak = ModelParameter[ltTextSM, "SW"];
ltTextSMCharge = ModelParameter[ltTextSM, "EE"];
ltTextQEDCharge = ModelParameter[ltTextQED, "EE"];
check["readable plain text formats the model's symbolic weak-angle definition",
 ltTextWeakDefinition === Sqrt[1 - ltTextWeak^2] &&
 StringStartsQ[PlainText[ltTextWeakDefinition], "sqrt("] &&
 StringContainsQ[PlainText[ltTextWeakDefinition], "SW"]];
check["readable plain text supports equal parameter names in two model scopes",
 ltTextSMCharge =!= ltTextQEDCharge &&
 PlainText[ltTextSMCharge + ltTextQEDCharge] === "(EE+EE)"];
check["readable rational expressions preserve their numerator and denominator sums",
 With[{value = (1 + ltTextSumLeft)/(2 + ltTextSumRight)},
  Cancel[ToExpression[PlainText[value], InputForm] - value] === 0]];
check["readable sums containing complex coefficients preserve their value",
 With[{value = 1 + I ltTextSumLeft},
  Expand[ToExpression[PlainText[value], InputForm] - value] === 0]];
check["model mode still round trips nested scalar sums and complex values",
 AllTrue[{Sqrt[1 - ltTextCharge^2], (1 + ltTextCharge)/(2 + ltTextMass),
   1 + I ltTextCharge}, ltModelTextRead[#, ltTextMasses] === # &]];
check["unsupported readable plain text remains an inert request without recursion",
 With[{formatted = PlainText[ltTextUnknownHead[ltTextSumLeft]]},
  HoldComplete[formatted] === HoldComplete[PlainText[ltTextUnknownHead[ltTextSumLeft]]]]];
check["TeX formats a genuine sum through its sum formatter",
 TeX[ltTextCharge + ltTextMass] === "\\left(EE+Me\\right)"];
check["SPINAS formats a genuine sum through its sum formatter",
 SPINAS[ltTextCharge + ltTextMass] === "(EE+Me)"];
check["TeX formats a square root containing a symbolic sum",
 TeX[Sqrt[1 + ltTextCharge]] === "\\sqrt{\\left(1+EE\\right)}"];
check["unsupported TeX and SPINAS remain inert requests instead of dropping their heads",
 With[{tex = TeX[ltTextUnknownHead[ltTextSumLeft]], code = SPINAS[ltTextUnknownHead[ltTextSumLeft]]},
  HoldComplete[tex] === HoldComplete[TeX[ltTextUnknownHead[ltTextSumLeft]]] &&
  HoldComplete[code] === HoldComplete[SPINAS[ltTextUnknownHead[ltTextSumLeft]]]]];
ltTextCompoundBefore = {ltTextWeakDefinition, ltTextSMCharge + ltTextQEDCharge};
check["formatting never changes model parameter scopes or symbolic definitions",
 PlainText /@ ltTextCompoundBefore; TeX /@ ltTextCompoundBefore;
 ltTextCompoundBefore === {ltTextWeakDefinition, ltTextSMCharge + ltTextQEDCharge} &&
 ModelParameterName[ltTextSMCharge] === "EE" && ModelParameterName[ltTextQEDCharge] === "EE"];
check["legacy SPINAS arithmetic works on the first package load before a reload",
 AllTrue[{1/xFormattingScalar,(xFormattingScalar+yFormattingScalar)^2,
   xFormattingScalar^(2/3),(xFormattingScalar+yFormattingScalar)^nFormattingScalar},
   StringQ[SPINAS[#]] && StringLength[SPINAS[#]] > 0 &]];

check["the supported package loader reloads structural sum formatters",
 Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
 PlainText[ltTextCharge + ltTextMass] === "(EE+Me)" &&
 TeX[ltTextCharge + ltTextMass] === "\\left(EE+Me\\right)" &&
 SPINAS[ltTextCharge + ltTextMass] === "(EE+Me)"];


(* A mixed complex coefficient is itself a sum and needs product grouping. *)
check["readable mixed complex product factors preserve their exact value",
 AllTrue[{1 + I, 1/3 + 2 I/5, -1 + 2 I, -1 - I},
  With[{value = # ltTextSumLeft},
   Expand[ToExpression[PlainText[value], InputForm] - value] === 0] &]];
check["readable pure imaginary and real rational factors keep unambiguous values",
 PlainText[I ltTextSumLeft] === "I*ltTextSumLeft" &&
 AllTrue[{I/3, -I, -3/7}, With[{value = # ltTextSumLeft},
   Expand[ToExpression[PlainText[value], InputForm] - value] === 0] &]];
check["readable powers preserve grouped mixed complex bases and factors",
 AllTrue[{(1 + I)^ltTextComplexExponent,
   ((1 + I) ltTextSumLeft)^ltTextComplexExponent},
  ToExpression[PlainText[#], InputForm] === # &]];
check["readable denominators preserve grouped complex numerator and denominator factors",
 AllTrue[{ltTextSumLeft/((1 + I) ltTextSumRight),
   (1 + (1 + I) ltTextSumLeft)/(2 + (3 - 2 I) ltTextSumRight)},
  Cancel[ToExpression[PlainText[#], InputForm] - #] === 0 &]];
check["TeX groups a mixed complex coefficient before an adjacent model parameter",
 StringStartsQ[TeX[(1 + I) ltTextCharge], "\\left("] &&
 StringContainsQ[TeX[(1 + I) ltTextCharge], "\\right)EE"]];
check["TeX leaves a pure imaginary product factor unambiguous",
 TeX[I ltTextCharge] === ToString[I,TeXForm] <> "EE" &&
 AllTrue[{I/3,-I/2},StringFreeQ[TeX[# ltTextCharge],"\n"] &&
   StringContainsQ[TeX[# ltTextCharge],"EE"] &]];
check["legacy SPINAS emits typed complex and rational coefficients without integer division",
 StringStartsQ[SPINAS[(1 + I) ltTextCharge], "cdouble("] &&
 StringContainsQ[SPINAS[(1 + I) ltTextCharge], ")*EE"] &&
 StringStartsQ[SPINAS[ltTextCharge/3], "ldouble("] &&
 !StringContainsQ[SPINAS[ltTextCharge/3], "(1)/(3)"] &&
 StringStartsQ[SPINAS[2 ltTextCharge], "ldouble("]];
check["model mode retains exact complex coefficients in products powers and denominators",
 AllTrue[{(1 + I) ltTextCharge, ((1 + I) ltTextCharge)^2,
   ltTextCharge/((1 + I) ltTextMass),
   (1 + (1 + I) ltTextCharge)/(2 + (3 - 2 I) ltTextMass)},
  ltModelTextRead[#, ltTextMasses] === # &]];


check["TeX and legacy SPINAS support named scalar variables in sums",
 TeX[xFormattingScalar+yFormattingScalar] === "\\left(\\text{xFormattingScalar}+\\text{yFormattingScalar}\\right)" &&
 SPINAS[xFormattingScalar+yFormattingScalar] === "(xFormattingScalar+yFormattingScalar)"];
check["TeX groups a complex power base before its exponent",
 StringStartsQ[TeX[(1+I)^ltTextCharge],"\\left("] &&
 StringContainsQ[TeX[(1+I)^ltTextCharge],"\\right)^{EE}"]];
check["TeX groups a whole product power base",
 StringStartsQ[TeX[(ltTextCharge ltTextMass)^ltTextCharge],"\\left("] &&
 StringContainsQ[TeX[(ltTextCharge ltTextMass)^ltTextCharge],"\\right)^{EE}"]];
check["TeX uses LaTeX fraction syntax for a rational exponent",
 StringContainsQ[TeX[ltTextCharge^(2/3)],"\\frac{2}{3}"] &&
 StringFreeQ[TeX[ltTextCharge^(2/3)],"\n"]];
check["TeX groups negative and already powered bases",
 StringStartsQ[TeX[(-2)^ltTextCharge],"\\left(-2\\right)^{"] &&
 StringStartsQ[TeX[(ltTextCharge^ltTextMass)^ltTextCharge],"\\left("]];
check["legacy SPINAS emits nonempty reciprocal and negative-power expressions",
 AllTrue[{1/xFormattingScalar,xFormattingScalar^-2},StringQ[SPINAS[#]] &&
  StringContainsQ[SPINAS[#],"xFormattingScalar"] && StringContainsQ[SPINAS[#],"/"] &]];
check["legacy SPINAS keeps a summed base grouped in an integer power",
 SPINAS[(xFormattingScalar+yFormattingScalar)^2] ===
  "(xFormattingScalar+yFormattingScalar)*(xFormattingScalar+yFormattingScalar)"];
check["legacy SPINAS emits symbolic powers without a symbolic Table iterator",
 StringStartsQ[SPINAS[(xFormattingScalar+yFormattingScalar)^nFormattingScalar],"std::pow(cdouble("] &&
 StringContainsQ[SPINAS[(xFormattingScalar+yFormattingScalar)^nFormattingScalar],"nFormattingScalar"]];
check["legacy SPINAS emits typed rational powers",
 StringStartsQ[SPINAS[xFormattingScalar^(2/3)],"std::pow(cdouble("] &&
 StringContainsQ[SPINAS[xFormattingScalar^(2/3)],"ldouble("]];
check["legacy SPINAS recursively formats a square-root argument",
 SPINAS[Sqrt[xFormattingScalar+yFormattingScalar]] ===
  "std::sqrt(cdouble((xFormattingScalar+yFormattingScalar)))"];
check["readable inexact complex factors preserve zero and tiny imaginary components",
 AllTrue[{Complex[1.2,0.],Complex[1.2,-0.],Complex[0.,1.2],Complex[-0.,-1.2],Complex[1.2,1.*^-300]},
 With[{value=# ltTextSumLeft},ToExpression[PlainText[value],InputForm] === value] &]];
check["model mode still round trips exact composite powers and roots",
 AllTrue[{(ltTextCharge+ltTextMass)^(2/3),Sqrt[ltTextCharge+ltTextMass],
   (ltTextCharge+ltTextMass)^-2},ltModelTextRead[#,ltTextMasses] === # &]];

If[checks =!= 91 || completedChecks =!= 91, AppendTo[failures,
  {"suite did not complete every check", "Expected" -> 91,
   "Attempted" -> checks, "Completed" -> completedChecks}]];
Print["Plain text: ", checks, " checks, ", Length[failures], " failures."];
If[failures =!= {}, Print[failures]];
Print["HEPCAT_TEST_COMPLETED:plain-text-tests.wl"];
Exit[If[failures === {}, 0, 1]];
