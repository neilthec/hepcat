(* Lossless model text and readable plain-text regression coverage. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, assertion_] := Module[{result},
  checks++;
  result = TimeConstrained[Check[assertion, False], 10, False];
  If[!TrueQ[result], AppendTo[failures, name]; Print["FAIL: ", name]]];
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

If[checks =!= 39, AppendTo[failures,
  {"suite did not reach every check", "Expected" -> 39, "Actual" -> checks}]];
Print["Plain text: ", checks, " checks, ", Length[failures], " failures."];
If[failures =!= {}, Print[failures]];
Print["HEPCAT_TEST_COMPLETED:plain-text-tests.wl"];
Exit[If[failures === {}, 0, 1]];
