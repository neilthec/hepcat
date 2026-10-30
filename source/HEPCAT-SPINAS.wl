(* ::Package:: *)

(* ::Section::Closed:: *)
(*Complete SPINAS Process Export*)

Begin["ConstructiveDiagrams`Private`"];

(* Numeric literals preserve the supplied numerical value without routing
   it through a machine-double C++ literal. *)
spinasCppReal[value_] := Module[{digits, exponent, sign},
  If[!TrueQ[NumberQ[value] && Im[value] == 0], Return[$Failed]];
  If[TrueQ[value == 0], Return["ldouble(0.0L)"]];
  sign = If[TrueQ[value < 0], "-", ""];
  {digits, exponent} = RealDigits[N[SetPrecision[Abs[value], 32], 32], 10, 24];
  If[!VectorQ[digits, IntegerQ], Return[$Failed]];
  "ldouble(" <> sign <> ToString[First[digits]] <> "." <> StringJoin[ToString /@ Rest[digits]] <>
    "e" <> ToString[exponent - 1] <> "L)"
];
spinasCppNumber[value_] := Module[{real, imaginary},
  If[!TrueQ[NumberQ[value]] || !FreeQ[value, Indeterminate | DirectedInfinity], Return[$Failed]];
  real = spinasCppReal[Re[value]]; imaginary = spinasCppReal[Im[value]];
  If[MemberQ[{real, imaginary}, $Failed], $Failed,
    If[TrueQ[Im[value] == 0], real, "cdouble(" <> real <> ", " <> imaginary <> ")"]]
];

(* Distinguish a typed Lorentz-vector expression from scalar products that
   happen to contain Mom objects inside their arguments. *)
spinasMomentumExpressionQ[_Mom] := True;
spinasMomentumExpressionQ[value_Plus] := AllTrue[List @@ value, spinasMomentumExpressionQ];
spinasMomentumExpressionQ[value_Times] := Count[spinasMomentumExpressionQ /@ (List @@ value), True] === 1;
spinasMomentumExpressionQ[_] := False;
spinasMomentumArgument[value : (_Integer | _Multiparticle)] := Mom[value];
spinasMomentumArgument[value_] := value;
spinasCrossVector[value_, outgoing_List] := value /. {
  Mom[Multiparticle[labels___]] :> Total[If[MemberQ[outgoing, #], -Mom[#], Mom[#]] & /@ {labels}],
  Mom[label_Integer] :> If[MemberQ[outgoing, label], -Mom[label], Mom[label]]};

spinasProcessIR[process_List, amplitude_, model_List, options_Association] := Module[
  {tag = Unique["spinasExport"], fail, count = Length[process], incoming,
   rows, masses, massRules, massValues, parameterRules, declared, usedParameters = {},
   parameterDefaults, parameterCpp, parameterNames, expression, original, crossed,
   weights, helicities, spin2, indexed, spinIndices, implicit, indexRules = {},
   emit, emitParameter, emitVector, momentumCoefficients, endpoint, chainCode,
   traceCode, chains = {}, chainStatements = {}, chainCpp, expressionCpp, massCpp,
   numerical, coefficient, parts, head, explicit, indices, leg, slots, state,
   canonicalSymbol, protectedCrossing, outgoing, pairRules = {}, normalizeInput,
   allocateImplicit, spinSlot, slotDegrees, reservedSlots, availableSlots, assigned},
  fail[reason_String, value_: None] := Throw[Failure["SPINASExport", <|
    "MessageTemplate" -> reason, "UnsupportedExpression" -> HoldForm[value]|>], tag];
  Catch[
    If[Length[model] =!= 4 || !AllTrue[model, ListQ] || count < 2 ||
        !AllTrue[process, StringQ], fail["Expected model data and at least two ordered external particle names."]];
    incoming = Lookup[options, "IncomingLegs", {1, 2}];
    If[!ListQ[incoming] || Length[incoming] =!= 2 || !DuplicateFreeQ[incoming] ||
        !AllTrue[incoming, IntegerQ[#] && 1 <= # <= count &],
      fail["IncomingLegs must contain exactly two distinct declared external leg numbers."]];
    rows = Map[Function[name,
      With[{matching = Select[model[[1]], ListQ[#] && Length[#] >= 6 &&
          MemberQ[#[[{2, 3}]], name] &]},
        If[Length[matching] =!= 1, fail["An external particle is absent or ambiguous in the supplied model.", name]];
        First[matching]]], process];
    spin2 = rows[[All, 5]]; masses = rows[[All, 6]];
    If[!AllTrue[spin2, IntegerQ[#] && 0 <= # <= 4 &],
      fail["The exporter currently supports massive spin through two and explicit massless helicities.", spin2]];
    parameterRules = ModelParameterRules[model, Lookup[options, "ParameterRules", Automatic]];
    If[!ListQ[parameterRules], fail["ModelParameterRules did not return resolved local parameter values."]];
    declared = DeleteDuplicates[Cases[Join[model[[3]], model[[4]]], _ModelParameter, Infinity]];
    canonicalSymbol[value_Symbol] := Module[{matches = Select[declared,
        ModelParameterName[#] === SymbolName[Unevaluated[value]] &]},
      If[Length[matches] === 1, First[matches], value]];
    canonicalSymbol[value_] := value;
    normalizeInput[value_, depth_: 0] := Which[
      depth > 32, fail["Amplitude report nesting exceeds the supported input depth."],
      AssociationQ[value] && KeyExistsQ[value, "Status"] &&
          !MemberQ[{"COMPLETE", "PREPARED", "PASS", "CONSTRUCTED"}, value["Status"]],
        fail["The amplitude report is not complete or successful. Export a completed report, or explicitly extract a candidate expression for diagnostic export."],
      AssociationQ[value] && KeyExistsQ[value, "Amplitude"],
        normalizeInput[value["Amplitude"], depth + 1],
      AssociationQ[value] && KeyExistsQ[value, "PreparedAmplitudes"],
        normalizeInput[value["PreparedAmplitudes"], depth + 1],
      MatchQ[value, {_List, _}] && AllTrue[First[value], MatchQ[#, _Rule] &],
        pairRules = Join[pairRules, First[value]]; normalizeInput[Last[value], depth + 1],
      ListQ[value] && value =!= {} && AllTrue[value, MatchQ[#, {_List, _}] &],
        Total[normalizeInput[#, depth + 1] & /@ value],
      MatchQ[value, _ConstructiveAmplitude],
        normalizeInput[Times @@ Take[List @@ value, 2]/Last[value], depth + 1],
      FailureQ[value] || AssociationQ[value] || ListQ[value],
        fail["Provide a spinor amplitude expression, amplitude pair, or successful preparation/completion report."],
      True, value];
    original = normalizeInput[amplitude];
    (* A bare legacy parameter name may be resolved locally by the supplied
       model. Scoped parameters from another model are never identified by name. *)
    original = original /. symbol_Symbol /; Context[Unevaluated[symbol]] === "Global`" :> canonicalSymbol[symbol];
    masses = masses /. symbol_Symbol /; Context[Unevaluated[symbol]] === "Global`" :> canonicalSymbol[symbol];
    pairRules = Select[pairRules, MatchQ[#, HoldPattern[Mass[_Integer] -> _]] &];
    Do[
      If[!MemberQ[Range[count], First[rule][[1]]] ||
          !TrueQ[residueExactZero[(Last[rule] /. symbol_Symbol /;
              Context[Unevaluated[symbol]] === "Global`" :> canonicalSymbol[symbol]) -
            masses[[First[rule][[1]]]]]],
        fail["Amplitude external mass rules must agree with the supplied process and model.", rule]],
      {rule, pairRules}];
    massRules = Thread[(Mass /@ Range[count]) -> masses];
    expression = original /. massRules;
    expression = expression /. {SpinorChainReduced -> SpinorChain, SpinorTraceReduced -> SpinorTrace};
    If[!FreeQ[expression, _xFactor | _xTildeFactor | _SpinorHat | _MomHat | _MomQ |
        _MomProdHat1 | _MomProdHat2 | _MomProdHat12],
      fail["Reduce x factors and hatted or shifted structures before exporting a physical process."]];
    If[!FreeQ[expression, Spinor["Xi", ___] | Spinor["Zeta", ___]],
      fail["Resolve auxiliary Xi and Zeta spinors before exporting a physical process."]];
    massValues = Map[Function[mass,
      numerical = Quiet[N[mass /. parameterRules, 24]];
      If[!NumberQ[numerical] || !FreeQ[numerical, Indeterminate | DirectedInfinity] ||
          !TrueQ[Im[numerical] == 0 && Re[numerical] >= 0],
        fail["Every external mass needs a finite nonnegative value from the supplied model or ParameterRules.", mass]];
      Re[numerical]], masses];
    weights = completionExternalSpinWeights[expression, massRules];
    If[expression =!= 0 && (!ListQ[weights] || Length[weights] =!= count),
      fail["The external spinor weights are not homogeneous; reduce unsupported contractions first."]];
    helicities = Table[If[TrueQ[massValues[[i]] == 0] && spin2[[i]] > 0,
      state = Which[StringEndsQ[process[[i]], ".+"], spin2[[i]],
        StringEndsQ[process[[i]], ".-"], -spin2[[i]],
        ListQ[weights] && IntegerQ[weights[[i]]] && Abs[weights[[i]]] === spin2[[i]], -weights[[i]],
        True, fail["A massless particle requires a definite helicity in its name or homogeneous spinor weights.", process[[i]]]];
      If[expression =!= 0 && ListQ[weights] && weights[[i]] =!= -state,
        fail["The massless particle name and amplitude spinor weight specify different helicities.", process[[i]]]];
      state, 0], {i, count}];
    If[ListQ[weights] && expression =!= 0,
      Do[If[massValues[[i]] > 0 && weights[[i]] =!= spin2[[i]],
        fail["Each massive external state must have the model's complete spin tensor degree.", process[[i]]]], {i, count}]];
    (* Allocate tensor slots without expanding factored sums. A spin-bearing
       power is unrolled only up to the declared external rank (at most four). *)
    allocateImplicit[value_, label_Integer] := Module[{walk, combine},
      combine[parts_List, start_Integer, head_] := Module[{result = {}, degree = 0, item},
        Do[item = walk[part, start + degree]; AppendTo[result, First[item]];
          degree += Last[item], {part, parts}];
        {head @@ result, degree}];
      walk[s_Spinor, start_Integer] := If[rspSpinorLeg[s] === label,
        If[start >= spin2[[label]], fail["An implicit massive tensor exceeds its model spin rank.", s]];
        {Append[s, spinasSpinIndex[label, start + 1]], 1}, {s, 0}];
      walk[sum_Plus, start_Integer] := Module[{terms = walk[#, start] & /@ List @@ sum, degrees},
        degrees = DeleteDuplicates[Last /@ terms];
        If[Length[degrees] =!= 1, fail["Every term must carry the same complete external spin tensor.", sum]];
        {Total[First /@ terms], First[degrees]}];
      walk[product : (_Times | _SpinorChain), start_Integer] :=
        combine[List @@ product, start, Head[product]];
      walk[power : Power[base_, exponent_Integer], start_Integer] := Module[{first, degree, copies},
        first = walk[base, start]; degree = Last[first];
        If[degree === 0, Return[{power, 0}]];
        If[exponent <= 0 || exponent degree > spin2[[label]],
          fail["A massive tensor must be polynomial and multilinear in its external spin slots.", power]];
        copies = Prepend[Table[walk[base, start + j degree], {j, 1, exponent - 1}], first];
        {Times @@ (First /@ copies), exponent degree}];
      walk[other_, start_Integer] := If[FreeQ[other, s_Spinor /; rspSpinorLeg[s] === label],
        {other, 0}, fail["Unsupported function of a massive external spin tensor.", other]];
      First[walk[value, 0]]
    ];
    (* Explicit slots allocate only external tensor indices. The runtime
       performs the normalized symmetric spin-state projection. *)
    indexed = expression;
    Do[If[massValues[[i]] > 0 && spin2[[i]] > 0,
      implicit = Cases[indexed, s_Spinor /; rspSpinorLeg[s] === i &&
        Length[Drop[List @@ s, First[FirstPosition[List @@ s, "Angle" | "Square"]] + 1]] === 0, Infinity];
      explicit = Cases[indexed, s_Spinor /; rspSpinorLeg[s] === i &&
        Length[Drop[List @@ s, First[FirstPosition[List @@ s, "Angle" | "Square"]] + 1]] > 0, Infinity];
      If[implicit =!= {} && explicit =!= {},
        fail["Use either implicit or explicit spin slots consistently for each massive external leg.", process[[i]]]];
      If[implicit =!= {}, indexed = allocateImplicit[indexed, i]];
      explicit = DeleteDuplicates[Cases[indexed, s_Spinor /; rspSpinorLeg[s] === i :>
        Drop[List @@ s, First[FirstPosition[List @@ s, "Angle" | "Square"]] + 1], Infinity]];
      If[AnyTrue[explicit, Length[#] =!= 1 &],
        fail["An external massive tensor could not be assigned complete spinor slots; make its indices explicit.", process[[i]]]];
      indices = DeleteDuplicates[Flatten[explicit, 1]];
      If[AnyTrue[indices, NumberQ],
        fail["Export the complete massive spin tensor, rather than fixed numeric spin-index components.", process[[i]]]];
      If[AnyTrue[indices, MatchQ[#, _VectorSpinIndex | _spinasSpinIndex] &&
          !MatchQ[#, VectorSpinIndex[i, _Integer] | spinasSpinIndex[i, _Integer]] &],
        fail["A reserved spin slot must belong to its external leg and have an integer slot number.", process[[i]]]];
      reservedSlots = DeleteDuplicates[Cases[indices,
        (VectorSpinIndex[i, slot_Integer] | spinasSpinIndex[i, slot_Integer]) :> slot]];
      If[!AllTrue[reservedSlots, 1 <= # <= spin2[[i]] &],
        fail["A reserved spin slot lies outside the model spin rank.", process[[i]]]];
      availableSlots = Complement[Range[spin2[[i]]], reservedSlots];
      indices = Select[indices, !MatchQ[#, spinasSpinIndex[i, _Integer] | VectorSpinIndex[i, _Integer]] &];
      If[Length[indices] > Length[availableSlots], fail["Too many external spin indices for a model particle.", process[[i]]]];
      Do[AppendTo[indexRules, {i, indices[[j]]} -> availableSlots[[j]]], {j, Length[indices]}]], {i, count}];
    spinSlot[spinor_Spinor, label_Integer] := Module[{data = List @@ spinor, position, indices, slot},
      position = First[FirstPosition[data, "Angle" | "Square"]]; indices = Drop[data, position + 1];
      If[Length[indices] =!= 1, fail["A massive endpoint has no unique assigned spinor slot.", spinor]];
      slot = If[MatchQ[First[indices], spinasSpinIndex[label, _Integer] | VectorSpinIndex[label, _Integer]],
        Last[First[indices]], Replace[{label, First[indices]}, indexRules, {0}]];
      If[!IntegerQ[slot] || !TrueQ[1 <= slot <= spin2[[label]]],
        fail["An explicit massive spinor index has no external tensor slot.", spinor]]; slot];
    slotDegrees[value_, label_Integer] := Module[{walk, zero = ConstantArray[0, spin2[[label]]]},
      walk[s_Spinor] := If[rspSpinorLeg[s] === label,
        UnitVector[spin2[[label]], spinSlot[s, label]], zero];
      walk[sum_Plus] := Module[{degrees = DeleteDuplicates[walk /@ List @@ sum]},
        If[Length[degrees] =!= 1, fail["Every additive term must use the same complete massive spin slots.", sum]];
        First[degrees]];
      walk[product : (_Times | _SpinorChain)] := Total[walk /@ List @@ product];
      walk[power : Power[base_, exponent_Integer]] := Module[{degree = walk[base]},
        If[degree === zero, Return[zero]];
        If[exponent <= 0, fail["A massive spin tensor cannot occur in a denominator.", power]];
        exponent degree];
      walk[other_] := If[FreeQ[other, s_Spinor /; rspSpinorLeg[s] === label], zero,
        fail["Unsupported function of a massive external spin tensor.", other]];
      walk[value]];
    If[expression =!= 0, Do[If[massValues[[i]] > 0 && spin2[[i]] > 0 &&
        slotDegrees[indexed, i] =!= ConstantArray[1, spin2[[i]]],
      fail["Each massive spin slot must occur exactly once in every additive term.", process[[i]]]], {i, count}]];
    (* Scalar invariant arguments need their own typed crossing: the legacy
       ReverseMomentum scalar rule accepts bare particle indices only. *)
    outgoing = Complement[Range[count], incoming];
    protectedCrossing = indexed /. {
      MomProd[a_, b_] :> spinasExportDot[spinasMomentumArgument[a], spinasMomentumArgument[b]],
      Mandelstahm[labels___] :> spinasExportSquare[Total[Mom /@ {labels}]],
      Power[vector_?spinasMomentumExpressionQ, 2] :> spinasExportSquare[vector]};
    crossed = ReverseMomentum[protectedCrossing, outgoing] /. {
      spinasExportDot[a_, b_] :> MomProd[spinasCrossVector[a, outgoing], spinasCrossVector[b, outgoing]],
      spinasExportSquare[vector_] :> spinasExportLorentzSquare[spinasCrossVector[vector, outgoing]]};
    emitParameter[parameter_] := Module[{position, matches, value = parameter},
      If[Head[value] === Symbol, value = canonicalSymbol[value]];
      If[!MemberQ[declared, value], fail["An amplitude parameter does not belong to the supplied model.", parameter]];
      position = FirstPosition[usedParameters, value, Missing["New"]];
      If[MissingQ[position], AppendTo[usedParameters, value]; position = {Length[usedParameters]}];
      "parameters[" <> ToString[First[position] - 1] <> "]"];
    momentumCoefficients[value_] := Module[{form = value, coefficientValues},
      form = form /. Mom[Multiparticle[items___]] :> Total[Mom /@ {items}];
      form = Expand[form];
      coefficientValues = Table[Coefficient[form, Mom[i]], {i, count}];
      If[!AllTrue[coefficientValues, NumberQ[#] && TrueQ[Im[#] == 0] &] ||
          !TrueQ[Expand[form - coefficientValues.(Mom /@ Range[count])] === 0],
        fail["A momentum insertion must be a linear combination of declared external momenta.", value]];
      coefficientValues];
    emitVector[value_] := Module[{values = momentumCoefficients[value], labels, components},
      If[AllTrue[values, IntegerQ[#] && Abs[#] <= 16 &],
        labels = Flatten[MapIndexed[ConstantArray[Sign[#1] First[#2], Abs[#1]] &, values]];
        Return["sum_momenta(momenta, {" <> StringRiffle[ToString /@ labels, ", "] <> "})"]];
      components = Table["(" <> StringRiffle[MapIndexed[If[TrueQ[#1 === 0], Nothing,
        "(" <> spinasCppReal[#1] <> ") * momenta[" <> ToString[First[#2] - 1] <> "][" <> ToString[mu] <> "]"] &, values], " + "] <> ")", {mu, 0, 3}];
      "FourMomentum{{" <> StringRiffle[components, ", "] <> "}}"];
    endpoint[spinor_Spinor, left_] := Module[{data = List @@ spinor, position, label, type, variance, indices, slot, method},
      position = FirstPosition[data, "Angle" | "Square", Missing["Type"]];
      If[MissingQ[position], fail["A spinor endpoint has no angle/square type.", spinor]];
      position = First[position]; label = data[[position + 1]]; type = data[[position]];
      If[!IntegerQ[label] || !MemberQ[Range[count], label] || !MemberQ[{"Spin", "Helicity"}, First[data]],
        fail["A spinor endpoint must name a declared physical external leg.", spinor]];
      method = If[left, If[type === "Angle", "langle", "lsquare"], If[type === "Angle", "rangle", "rsquare"]];
      If[TrueQ[massValues[[label]] == 0],
        If[First[data] =!= "Helicity" || Length[data] =!= 3,
          fail["Massless endpoints must be explicit physical helicity spinors.", spinor]];
        Return["particles[" <> ToString[label - 1] <> "]." <> method <> "()"]];
      If[First[data] =!= "Spin", fail["A massive endpoint must be a massive spinor.", spinor]];
      variance = If[position === 2, "Upper", data[[2]]];
      If[!MemberQ[{"Upper", "Lower"}, variance], fail["Unsupported massive spin-index variance.", spinor]];
      indices = Drop[data, position + 1];
      If[Length[indices] =!= 1, fail["A massive endpoint has no unique assigned spinor slot.", spinor]];
      slot = Which[
        MatchQ[First[indices], spinasSpinIndex[label, _Integer] | VectorSpinIndex[label, _Integer]], Last[First[indices]],
        True, Replace[{label, First[indices]}, indexRules, {0}]];
      If[!IntegerQ[slot] || !TrueQ[1 <= slot <= spin2[[label]]], fail["An explicit massive spinor index has no external tensor slot.", spinor]];
      "particles[" <> ToString[label - 1] <> "]." <> method <> "(spinor_spins[" <>
        ToString[label - 1] <> "][" <> ToString[slot - 1] <> "], " <>
        If[variance === "Upper", "true", "false"] <> ")"];
    chainCode[chain_SpinorChain] := Module[{items = List @@ chain, matrices, angle, expected},
      If[Length[items] < 2 || !MatchQ[First[items], _Spinor] || !MatchQ[Last[items], _Spinor],
        fail["SpinorChain must have two physical spinor endpoints.", chain]];
      angle = rspSpinorType[First[items]] === "Angle";
      expected = If[EvenQ[Length[items] - 2], rspSpinorType[First[items]], If[angle, "Square", "Angle"]];
      If[rspSpinorType[Last[items]] =!= expected, fail["Momentum-chain length and endpoint chiralities disagree.", chain]];
      matrices = MapIndexed["momentum_matrix(" <> emitVector[#1] <> ", " <>
        If[Xor[angle, OddQ[First[#2]]], "true", "false"] <> ")" &, Take[items, {2, -2}]];
      "(" <> StringRiffle[Join[{endpoint[First[items], True]}, matrices,
        {endpoint[Last[items], False]}], " * "] <> ")"];
    traceCode[trace_SpinorTrace] := Module[{items = List @@ trace, matrices},
      If[OddQ[Length[items]], fail["A chiral SpinorTrace requires an even number of momentum insertions.", trace]];
      If[items === {}, Return["ldouble(2.0L)"]];
      matrices = MapIndexed["momentum_matrix(" <> emitVector[#1] <> ", " <>
        If[EvenQ[First[#2]], "true", "false"] <> ")" &, items];
      "matrix_trace(" <> StringRiffle[matrices, " * "] <> ")"];
    emit[value_] := Which[
      Head[value] === ModelParameter, emitParameter[value],
      IntegerQ[value], "ldouble(" <> ToString[value] <> ".0L)",
      Head[value] === Rational, "ldouble(" <> ToString[Numerator[value]] <> ".0L / " <> ToString[Denominator[value]] <> ".0L)",
      NumberQ[value], With[{number = spinasCppNumber[value]}, If[number === $Failed, fail["Nonfinite C++ numeric literal.", value], number]],
      value === Pi, "std::acos(ldouble(-1))", value === E, "std::exp(ldouble(1))",
      Head[value] === Symbol, emitParameter[value],
      Head[value] === Plus, "(" <> StringRiffle[emit /@ List @@ value, " + "] <> ")",
      Head[value] === Times, "(" <> StringRiffle[emit /@ List @@ value, " * "] <> ")",
      Head[value] === spinasExportLorentzSquare, "mass2(" <> emitVector[First[value]] <> ")",
      Head[value] === Power && spinasMomentumExpressionQ[First[value]] && Last[value] === 2,
        "mass2(" <> emitVector[First[value]] <> ")",
      Head[value] === Power && IntegerQ[Last[value]] && Abs[Last[value]] <= 2147483647,
        "std::pow(cdouble(" <> emit[First[value]] <> "), " <> ToString[Last[value]] <> ")",
      Head[value] === Power, "std::pow(cdouble(" <> emit[First[value]] <> "), " <> emit[Last[value]] <> ")",
      Head[value] === SpinorChain,
        With[{position = FirstPosition[chains, value, Missing["New"]]},
          If[MissingQ[position], AppendTo[chains, value];
            AppendTo[chainStatements, "const cdouble chain_" <> ToString[Length[chains] - 1] <> " = " <> chainCode[value] <> ";"];
            "chain_" <> ToString[Length[chains] - 1], "chain_" <> ToString[First[position] - 1]]],
      Head[value] === SpinorTrace, traceCode[value],
      Head[value] === MomProd && Length[value] === 2,
        "dot(" <> emitVector[If[MatchQ[value[[1]], _Integer | _Multiparticle], Mom[value[[1]]], value[[1]]]] <> ", " <>
          emitVector[If[MatchQ[value[[2]], _Integer | _Multiparticle], Mom[value[[2]]], value[[2]]]] <> ")",
      Head[value] === Mandelstahm, "mass2(" <> emitVector[Total[Mom /@ List @@ value]] <> ")",
      Head[value] === PropDen && MemberQ[{2, 3}, Length[value]],
        "(mass2(" <> emitVector[First[value]] <> ") - (" <> emit[value[[2]]] <> ") * (" <> emit[value[[2]]] <> ")" <>
          If[Length[value] === 3, " + cdouble(0.0L, 1.0L) * (" <> emit[value[[2]]] <> ") * (" <> emit[value[[3]]] <> ")", ""] <> ")",
      Head[value] === Conjugate && Length[value] === 1, "std::conj(cdouble(" <> emit[First[value]] <> "))",
      Head[value] === Re && Length[value] === 1, "std::real(cdouble(" <> emit[First[value]] <> "))",
      Head[value] === Im && Length[value] === 1, "std::imag(cdouble(" <> emit[First[value]] <> "))",
      Head[value] === Abs && Length[value] === 1, "std::abs(cdouble(" <> emit[First[value]] <> "))",
      MemberQ[{Sin, Cos, Tan, ArcSin, ArcCos, ArcTan, Exp, Log, Sinh, Cosh, Tanh}, Head[value]] && Length[value] === 1,
        "std::" <> Switch[Head[value], Sin,"sin",Cos,"cos",Tan,"tan",ArcSin,"asin",ArcCos,"acos",ArcTan,"atan",Exp,"exp",Log,"log",Sinh,"sinh",Cosh,"cosh",Tanh,"tanh"] <>
          "(cdouble(" <> emit[First[value]] <> "))",
      True, fail["The amplitude contains an unsupported scalar, tensor, color, or model-function structure.", value]];
    massCpp = emit /@ masses;
    expressionCpp = emit[crossed];
    parameterDefaults = Map[Function[parameter,
      numerical = Quiet[N[parameter /. parameterRules, 24]];
      If[!NumberQ[numerical] || !FreeQ[numerical, Indeterminate | DirectedInfinity],
        fail["A used model parameter has no finite numerical default; supply ParameterRules.", parameter]];
      numerical], usedParameters];
    parameterCpp = spinasCppNumber /@ parameterDefaults;
    If[MemberQ[parameterCpp, $Failed], fail["A model parameter cannot be represented as a finite C++ numeric literal."]];
    parameterNames = ModelParameterName /@ usedParameters;
    If[!DuplicateFreeQ[parameterNames], fail["Used parameter names are ambiguous within the supplied model.", parameterNames]];
    <|"Process" -> process, "LegCount" -> count, "IncomingLegs" -> incoming,
      "Spin2" -> spin2, "Helicities" -> helicities, "Masses" -> masses,
      "MassValues" -> massValues, "MassCpp" -> massCpp,
      "ParameterNames" -> parameterNames, "ParameterSymbols" -> usedParameters,
      "ParameterDefaults" -> parameterDefaults, "ParameterCppDefaults" -> parameterCpp,
      "ExpressionCpp" -> expressionCpp, "ChainStatements" -> chainStatements,
      "OriginalExpression" -> original, "CrossedExpression" -> crossed,
      "IndexedExpression" -> indexed, "ExternalMassRules" -> massRules,
      "ColorStripped" -> AnyTrue[rows, Length[#] >= 8 && #[[8]] =!= 1 &]|>, tag]
];

End[];

(* ::Section::Closed:: *)
(*Complete SPINAS Process Projects*)

Begin["ConstructiveDiagrams`"];
WriteSPINASProcess::usage = "WriteSPINASProcess[directory, particles, amplitude, model] writes a complete C++11 SPINAS process project for two incoming and at least two outgoing particles. The amplitude uses HEPCAT's all-ingoing convention; IncomingLegs -> {1,2} selects physical incoming legs and outgoing legs are crossed automatically, including angle-spinor signs. The project contains process.hpp, process.cpp, main.cpp, a deterministic physical phase-space example generator, CMakeLists.txt, README.md and manifest.json. SPINASDirectory -> Automatic looks for SPINAS_ROOT or the sibling spinas checkout; an explicit path overrides it. Model parameter defaults come from ModelParameterRules; ParameterRules overrides them locally. ProcessName sets a C++ class/executable name. Build -> True configures and links the project against the existing SPINAS library; RunExamples -> True also builds and runs three sample points. Precision -> Automatic reads the local SPINAS build configuration; LongDouble and Double are explicit choices. Overwrite -> False protects existing project files. ReturnDetails -> False normally returns the output directory; True adds file paths and build/run results. Fixed massless helicities are retained. amp2 averages incoming massive spins and sums outgoing massive spins at those fixed helicities, without an identical-particle phase-space factor or color sums/averages. Unsupported expressions return Failure before files are written; this function does not complete or validate the physical amplitude.";
WriteSPINASProcess::usage = WriteSPINASProcess::usage <> " The family overload WriteSPINASProcess[directory, {{particles1, amplitude1}, ...}, model] combines helicity-related amplitudes into one class. It fills identical-species, same-role massless leg permutations with their proper statistics sign, without inferring parity or missing zero amplitudes. Incomplete coverage is rejected before writing files. amp(ds1,...,dsn) and amp(Spins) select doubled spins and helicities; spin_sum sums all external spin/helicity states; amp2 also averages incoming states and includes the outgoing identical-species factorial factor, including identical fermions. Canonical formulas are compiled once and reused through leg maps. Colored families are currently refused because a full color sum is not implemented. The original four-argument fixed-helicity overload retains its existing spin-sum and symmetry-factor conventions.";
WriteSPINASProcess::usage = WriteSPINASProcess::usage <> " CMakeExecutable -> Automatic searches PATH and standard installation locations, including the macOS CMake application and Homebrew, so desktop Mathematica need not inherit the shell's PATH. An explicit full path selects another CMake installation. A missing build tool is reported before generating files; source-only export does not require CMake.";
WriteSPINASProcess::usage = WriteSPINASProcess::usage <> " WriteSPINASProcess[directory, incomingNames -> outgoingNames, amplitudeEntries, model] explicitly describes the physical process. The entries still contain all-ingoing particle lists and un-crossed formulas in that same leg order, with incoming legs first. Outgoing charged particle names are checked against the conjugate all-ingoing species. Generic massless names such as A sum their helicities; A.+ or A.- selects one. The established HEPCAT/SPINAS helicity sign convention is retained, and ReverseMomentum crosses outgoing momenta and angle spinors exactly once. FixedHelicities -> {leg -> doubledHelicity, ...} selects massless legs in either grouped overload. Only unfixed incoming helicities are averaged. A selected outgoing helicity gives a labeled contribution with the same identical-species phase-space factor. Complex amp calls preserve phases and include no spin sums, incoming averages or phase-space factors; coherent production-decay sewing is not performed by this exporter. Runnable projects currently require two incoming particles.";
Options[WriteSPINASProcess] = {"SPINASDirectory" -> Automatic,
  "IncomingLegs" -> {1, 2}, "FixedHelicities" -> Automatic, "ParameterRules" -> Automatic,
  "ProcessName" -> "hepcat_process", "Precision" -> Automatic,
  "Build" -> False, "RunExamples" -> False, "CMakeExecutable" -> Automatic, "Overwrite" -> False,
  "ReturnDetails" -> False};
WriteSPINASProcess::args = "Expected an output directory, all-ingoing amplitude entries, a model and valid export options, optionally preceded by incoming particle names -> outgoing particle names. The fixed-helicity form takes an ordered all-ingoing particle list and amplitude instead of entries.";

Begin["ConstructiveDiagrams`Private`"];
$spinasExportSourceDirectory = DirectoryName[$InputFileName];

spinasCppIdentifierQ[name_String] := StringMatchQ[name, RegularExpression["[A-Za-z][A-Za-z0-9_]*"]] &&
  !StringContainsQ[name,"__"] && !StringMatchQ[name, RegularExpression["component_[0-9]+"]] && !MemberQ[{
    "std", "spinas", "hepcat_generated", "hepcat_spinas", "Parameters", "Momenta", "Spins",
    "all", "clean", "help", "test", "install", "package", "package_source",
    "edit_cache", "rebuild_cache", "list_install_components",
    "leg_count", "parameter_count", "default_parameters", "default_spins", "incoming_legs",
    "get_parameters", "masses", "set_parameters", "set_momenta", "amp", "spin_sum", "amp2",
    "helicity_count", "helicity_configurations", "incoming_spin_average", "symmetry_factor", "ParticleView",
    "parameters", "external_masses", "momenta", "particles", "momenta_ready", "component",
    "cdouble", "ldouble", "FourMomentum", "checked_mass", "validate_momenta", "finite_complex",
    "spinor_assignments", "spinor_normalization", "sum_momenta", "momentum_matrix",
    "matrix_trace", "dot", "mass2",
    "alignas","alignof","and","and_eq","asm","auto","bitand","bitor","bool","break",
    "case","catch","char","char16_t","char32_t","class","compl","const","constexpr",
    "const_cast","continue","decltype","default","delete","do","double","dynamic_cast",
    "else","enum","explicit","export","extern","false","float","for","friend","goto",
    "if","inline","int","long","mutable","namespace","new","noexcept","not","not_eq",
    "nullptr","operator","or","or_eq","private","protected","public","register",
    "reinterpret_cast","return","short","signed","sizeof","static","static_assert",
    "static_cast","struct","switch","template","this","thread_local","throw","true",
    "try","typedef","typeid","typename","union","unsigned","using","virtual","void",
    "volatile","wchar_t","while","xor","xor_eq"},name];
spinasCppQuoted[text_String] := "\"" <> StringReplace[text,
  {"\\" -> "\\\\", "\"" -> "\\\"", "\n" -> "\\n", "\r" -> "\\r", "\t" -> "\\t"}] <> "\"";
spinasCppComplex[number_] := "cdouble(" <> spinasCppReal[Re[number]] <> ", " <>
  spinasCppReal[Im[number]] <> ")";
spinasCppArray[values_List] := "{{" <> StringRiffle[values, ", "] <> "}}";
spinasProjectRoot[option_] := Module[{environment, candidates},
  If[StringQ[option], Return[ExpandFileName[option]]];
  environment = Environment["SPINAS_ROOT"];
  candidates = Join[If[StringQ[environment] && environment =!= "", {environment}, {}],
    {FileNameJoin[{ParentDirectory[ParentDirectory[$spinasExportSourceDirectory]], "spinas"}]}];
  SelectFirst[candidates, FileExistsQ[FileNameJoin[{#, "include", "spinas.h"}]] &, ""]
];
spinasProjectPrecision[root_String, option_] := Module[{cache, value},
  If[option =!= Automatic, Return[option]];
  cache = FileNameJoin[{root, "build", "CMakeCache.txt"}];
  If[FileExistsQ[cache],
    value = Quiet[Check[Import[cache, "Text"], ""]];
    If[StringContainsQ[value, RegularExpression["(?m)^PRECISION_TYPE:[^=]+=LONG_DOUBLE\\s*$"]],
      Return["LongDouble"]];
    If[StringContainsQ[value, RegularExpression["(?m)^PRECISION_TYPE:[^=]+=DOUBLE\\s*$"]],
      Return["Double"]]];
  (* This is SPINAS's documented default; an explicitly different ABI must
     select Precision rather than depending on application-wide compiler flags. *)
  "LongDouble"
];

spinasProjectHeader[name_String, ir_Association] := StringRiffle[{
  "#pragma once", "#include <array>", "#include <cstddef>", "#include <vector>",
  "#include \"spinas.h\"", "#include \"spinas-runtime.hpp\"", "",
  "namespace hepcat_generated {", "class " <> name <> " {", "public:",
  "  static constexpr std::size_t leg_count = " <> ToString[ir["LegCount"]] <> ";",
  "  static constexpr std::size_t parameter_count = " <> ToString[Length[ir["ParameterDefaults"]]] <> ";",
  "  using Parameters = std::array<cdouble, parameter_count>;",
  "  using Momenta = hepcat_spinas::Momenta<leg_count>;",
  "  using Spins = std::array<int, leg_count>;", "  " <> name <> "();",
  "  explicit " <> name <> "(const Parameters& values);",
  "  static Parameters default_parameters();", "  static Spins default_spins();",
  "  static std::array<std::size_t, 2> incoming_legs();",
  "  const Parameters& get_parameters() const { return parameters; }",
  "  const std::array<ldouble, leg_count>& masses() const { return external_masses; }",
  "  void set_parameters(const Parameters& values);",
  "  void set_momenta(const Momenta& values);", "  cdouble amp(const Spins& spins);",
  "  ldouble spin_sum();", "  ldouble amp2();", "private:",
  "  Parameters parameters;", "  std::array<ldouble, leg_count> external_masses;",
  "  Momenta momenta;", "  std::array<spinas::particle, leg_count> particles;",
  "  bool momenta_ready = false;",
  "  cdouble component(const std::vector<std::vector<int>>& spinor_spins);",
  "};", "} // namespace hepcat_generated", ""}, "\n"];

spinasProjectSource[name_String, ir_Association] := Module[
  {n = ir["LegCount"], spin2 = ir["Spin2"], helicities = ir["Helicities"],
   massValues = ir["MassValues"], defaults, spinDefaults, incoming, massLines, chainLines},
  defaults = spinasCppArray[spinasCppComplex /@ ir["ParameterDefaults"]];
  spinDefaults = spinasCppArray[ToString /@ MapThread[
    If[TrueQ[#1 == 0], #2, -#3] &, {massValues, helicities, spin2}]];
  incoming = spinasCppArray[ToString /@ (ir["IncomingLegs"] - 1)];
  massLines = MapIndexed["  external_masses[" <> ToString[First[#2] - 1] <>
    "] = checked_mass(cdouble(" <> #1 <> "));" &, ir["MassCpp"]];
  chainLines = "  " <> # & /@ ir["ChainStatements"];
  StringRiffle[Join[{
    "#include \"process.hpp\"", "#include <cmath>", "#include <functional>",
    "#include <stdexcept>", "", "namespace hepcat_generated {",
    "using namespace hepcat_spinas;",
    "constexpr std::size_t " <> name <> "::leg_count;",
    "constexpr std::size_t " <> name <> "::parameter_count;", "",
    name <> "::" <> name <> "() : " <> name <> "(default_parameters()) {}",
    name <> "::" <> name <> "(const Parameters& values) { set_parameters(values); }",
    name <> "::Parameters " <> name <> "::default_parameters() { return Parameters" <> defaults <> "; }",
    name <> "::Spins " <> name <> "::default_spins() { return Spins" <> spinDefaults <> "; }",
    "std::array<std::size_t,2> " <> name <> "::incoming_legs() { return " <> incoming <> "; }", "",
    "void " <> name <> "::set_parameters(const Parameters& values) {",
    "  momenta_ready = false;",
    "  for (const auto& value : values) if (!finite_complex(value))",
    "    throw std::invalid_argument(\"Model parameters must be finite.\");",
    "  parameters = values;"}, massLines, {
    "  static const std::array<int,leg_count> ranks = " <> spinasCppArray[ToString /@ spin2] <> ";",
    "  static const std::array<bool,leg_count> originally_massless = " <>
      spinasCppArray[If[TrueQ[#==0],"true","false"]& /@ massValues] <> ";",
    "  for (std::size_t i=0; i<leg_count; ++i) if (ranks[i]>0 &&",
    "      (external_masses[i]==0) != originally_massless[i])",
    "    throw std::invalid_argument(\"Changing between massive and massless spin states requires re-exporting the process.\");",
    "  for (std::size_t i=0; i<leg_count; ++i) particles[i] = spinas::particle(external_masses[i]);",
    "}", "", "void " <> name <> "::set_momenta(const Momenta& values) {",
    "  const auto validation = validate_momenta(values, external_masses, incoming_legs());",
    "  if (!validation.valid) throw std::invalid_argument(validation.message);",
    "  momenta_ready = false; momenta = values;",
    "  for (std::size_t i=0; i<leg_count; ++i) particles[i].set_momentum(momenta[i].data());",
    "  momenta_ready = true;", "}", "",
    "cdouble " <> name <> "::component(const std::vector<std::vector<int>>& spinor_spins) {"}, chainLines, {
    "  return " <> ir["ExpressionCpp"] <> ";", "}", "",
    "cdouble " <> name <> "::amp(const Spins& spins) {",
    "  if (!momenta_ready) throw std::logic_error(\"Call set_momenta before amp.\");",
    "  static const std::array<int,leg_count> ranks = " <> spinasCppArray[ToString /@ spin2] <> ";",
    "  static const std::array<int,leg_count> helicities = " <> spinasCppArray[ToString /@ helicities] <> ";",
    "  std::vector<std::vector<std::vector<int>>> assignments(leg_count);",
    "  std::vector<std::vector<int>> spinor_spins(leg_count); ldouble normalization=1;",
    "  for (std::size_t i=0; i<leg_count; ++i) {",
    "    if (external_masses[i] == 0) {",
    "      if (spins[i] != helicities[i]) throw std::invalid_argument(\"This process has fixed massless helicities.\");",
    "      assignments[i] = spinor_assignments(0,0);",
    "    } else {",
    "      assignments[i] = spinor_assignments(ranks[i],spins[i]);",
    "      normalization *= spinor_normalization(ranks[i],spins[i]);",
    "    }", "  }",
    "  std::function<cdouble(std::size_t)> sum = [&](std::size_t leg) -> cdouble {",
    "    if (leg == leg_count) return component(spinor_spins);",
    "    cdouble result(0,0); for (const auto& indices : assignments[leg]) {",
    "      spinor_spins[leg] = indices; result += sum(leg+1);", "    } return result;", "  };",
    "  return normalization * sum(0);", "}", "",
    "ldouble " <> name <> "::spin_sum() {",
    "  static const std::array<int,leg_count> ranks = " <> spinasCppArray[ToString /@ spin2] <> ";",
    "  Spins spins = default_spins();",
    "  std::function<ldouble(std::size_t)> sum = [&](std::size_t leg) -> ldouble {",
    "    if (leg == leg_count) return std::norm(amp(spins));",
    "    if (external_masses[leg] == 0 || ranks[leg] == 0) return sum(leg+1);",
    "    ldouble result=0; for (int ds=-ranks[leg]; ds<=ranks[leg]; ds+=2) {",
    "      spins[leg]=ds; result += sum(leg+1);", "    } return result;", "  };",
    "  return sum(0);", "}", "",
    "ldouble " <> name <> "::amp2() {",
    "  static const std::array<int,leg_count> ranks = " <> spinasCppArray[ToString /@ spin2] <> ";",
    "  ldouble average=1; for (const auto i : incoming_legs())",
    "    if (external_masses[i] != 0) average *= ranks[i]+1;",
    "  return spin_sum()/average;", "}", "} // namespace hepcat_generated", ""}], "\n"]
];

spinasProjectMain[name_String] := StringRiffle[{
  "#include \"process.hpp\"", "#include <iomanip>", "#include <iostream>",
  "#include <stdexcept>", "", "int main() { try {",
  "  using Process = hepcat_generated::" <> name <> ";",
  "  Process process; const auto incoming = Process::incoming_legs();",
  "  const ldouble energy = hepcat_spinas::example_center_of_mass_energy(process.masses(), incoming);",
  "  std::cout << std::setprecision(18);",
  "  std::cout << \"HEPCAT/SPINAS physical 2-to-N examples; fixed massless helicities.\\n\";",
  "  for (unsigned point=0; point<3; ++point) {",
  "    const auto momenta = hepcat_spinas::make_example_momenta(process.masses(), incoming, energy, point);",
  "    const auto check = hepcat_spinas::validate_momenta(momenta,process.masses(),incoming);",
  "    if (!check.valid) throw std::runtime_error(check.message);",
  "    process.set_momenta(momenta); const auto spins = Process::default_spins();",
  "    const auto amplitude = process.amp(spins); const auto averaged = process.amp2();",
  "    if (!hepcat_spinas::finite_complex(amplitude) || !std::isfinite(averaged))",
  "      throw std::runtime_error(\"An example amplitude is not finite.\");",
  "    std::cout << \"Point \" << point+1 << \"; sqrt(s) = \" << energy << '\\n';",
  "    for (std::size_t i=0; i<Process::leg_count; ++i) {",
  "      std::cout << \"  p\" << i+1 << \" =\"; for (const auto x : momenta[i]) std::cout << ' ' << x;",
  "      std::cout << '\\n';", "    }",
  "    std::cout << \"  conservation/shell scaled error = \" << check.maximum_scaled_error << '\\n'",
  "      << \"  amplitude(default doubled spins) = \" << amplitude << '\\n'",
  "      << \"  fixed-helicity, initial-spin-averaged |M|^2 = \" << averaged << '\\n';",
  "  }", "  std::cout << \"No identical-particle phase-space factor is included.\\n\";",
  "  return 0;", "} catch (const std::exception& error) {",
  "  std::cerr << \"Example failed: \" << error.what() << '\\n'; return 1;", "} }", ""}, "\n"];

spinasProjectCMake[name_String, root_String, precision_String] := StringRiffle[{
  "cmake_minimum_required(VERSION 3.10)", "project(" <> name <> " LANGUAGES CXX)",
  "set(CMAKE_CXX_STANDARD 11)", "set(CMAKE_CXX_STANDARD_REQUIRED ON)",
  "set(SPINAS_ROOT " <> spinasCppQuoted[StringReplace[root,"\\"->"/"]] <> " CACHE PATH \"SPINAS checkout or installation prefix\")",
  "find_path(SPINAS_INCLUDE_DIR spinas.h HINTS \"${SPINAS_ROOT}/include\")",
  "find_library(SPINAS_LIBRARY NAMES spinas spinas_shared HINTS \"${SPINAS_ROOT}/build\" \"${SPINAS_ROOT}/lib\")",
  "if(NOT SPINAS_INCLUDE_DIR OR NOT SPINAS_LIBRARY)",
  "  message(FATAL_ERROR \"SPINAS headers/library not found. Set -DSPINAS_ROOT=/path/to/spinas.\")", "endif()",
  "add_library(" <> name <> "_process STATIC process.cpp)",
  "target_include_directories(" <> name <> "_process PUBLIC \"${CMAKE_CURRENT_SOURCE_DIR}\" \"${SPINAS_INCLUDE_DIR}\")",
  "target_link_libraries(" <> name <> "_process PUBLIC \"${SPINAS_LIBRARY}\")",
  If[precision === "LongDouble", "target_compile_definitions(" <> name <> "_process PUBLIC WITH_LONG_DOUBLE)",
    "# Double precision ABI: no WITH_LONG_DOUBLE definition."],
  "add_executable(" <> name <> " main.cpp)",
  "target_link_libraries(" <> name <> " PRIVATE " <> name <> "_process)",
  "enable_testing()", "add_test(NAME phase_space_examples COMMAND " <> name <> ")", ""}, "\n"];

spinasProjectREADME[name_String, ir_Association, precision_String] := StringRiffle[{
  "# Generated HEPCAT/SPINAS Process", "", "Process (all-ingoing particle names): " <>
    StringRiffle[ir["Process"], ", "], "",
  "The C++ momentum inputs have positive energies. Legs " <>
    StringRiffle[ToString /@ ir["IncomingLegs"], ", "] <> " are incoming; all other legs are outgoing.",
  "Outgoing momenta and angle spinors were crossed from the supplied all-ingoing expression. Propagators remain off shell.",
  "HEPCAT's explicit +i*m*width denominator convention is preserved rather than replaced by SPINAS's opposite-width convention.", "",
  "## Build and run", "", "```sh", "cmake -S . -B build -DSPINAS_ROOT=/path/to/spinas",
  "cmake --build build --parallel", "./build/" <> name, "ctest --test-dir build --output-on-failure", "```", "",
  "This project uses the " <> precision <> " SPINAS ABI. It must match the library build; re-export with Precision if required.",
  "The existing SPINAS library is linked, not copied or rebuilt.", "",
  "## Use the process", "", "Include process.hpp and link the generated process library plus SPINAS.",
  "The class is hepcat_generated::" <> name <> ". Construct it with model defaults or a Parameters array.",
  "Parameter array order: " <> If[ir["ParameterNames"] === {}, "none", StringRiffle[ir["ParameterNames"], ", "]] <> ".",
  "Defaults are a resolved snapshot of the supplied model. When changing parameters, keep dependent values consistent.",
  "Call set_momenta with an array of four-vectors, then amp with the doubled-spin array.",
  "Massless helicities are fixed by the exported process. Massive states are normalized symmetric spinor tensors.",
  "spin_sum sums all massive external spins. amp2 also averages incoming massive spins. Neither includes an identical-particle phase-space factor or a sum over other massless-helicity processes.",
  If[TrueQ[ir["ColorStripped"]],
    "The process has colored external particles. This is a color-stripped amplitude; no color sum or average is inferred.",
    "No color sum or average is inferred. Explicit color tensors must be resolved before export."],
  "The executable prints three deterministic on-shell, conserved physical example points and checks that results are finite.",
  "These examples are numerical demonstrations, not a proof that the supplied amplitude is physically correct.", ""}, "\n"];

spinasProjectCommand[arguments_List] := Quiet[Check[RunProcess[arguments],
  <|"ExitCode" -> -1, "StandardOutput" -> "", "StandardError" -> "The external program could not be started."|>]];
(* Desktop Mathematica does not normally inherit the login shell's PATH.
   Locate CMake locally without changing the user's environment or installing
   command-line links into a system directory. An explicit path is authoritative. *)
spinasProjectCMakeExecutable[option_] := Module[
  {path, executableName, candidates, standard, programFiles, probe, executable},
  If[StringQ[option], candidates = {ExpandFileName[option]},
    path = Replace[Environment["PATH"], Except[_String] -> ""];
    executableName = If[$OperatingSystem === "Windows", "cmake.exe", "cmake"];
    standard = Switch[$OperatingSystem,
      "MacOSX", {"/Applications/CMake.app/Contents/bin/cmake",
        FileNameJoin[{$HomeDirectory, "Applications", "CMake.app", "Contents", "bin", "cmake"}],
        "/opt/homebrew/bin/cmake", "/usr/local/bin/cmake"},
      "Windows", programFiles = Environment /@ {"ProgramFiles", "ProgramFiles(x86)"};
        FileNameJoin[{#, "CMake", "bin", "cmake.exe"}] & /@
          Select[programFiles, StringQ[#] && # =!= "" &],
      _, {"/usr/local/bin/cmake", "/usr/bin/cmake", "/bin/cmake", "/snap/bin/cmake"}];
    candidates = Join[FileNameJoin[{#, executableName}] & /@
        Select[StringSplit[path, If[$OperatingSystem === "Windows", ";", ":"]], # =!= "" &],
      standard]];
  executable = SelectFirst[DeleteDuplicates[ExpandFileName /@ candidates],
    Function[candidate, FileExistsQ[candidate] && !DirectoryQ[candidate] &&
      (probe = spinasProjectCommand[{candidate, "--version"}];
       Lookup[probe, "ExitCode", -1] === 0 &&
         StringStartsQ[StringTrim[Lookup[probe, "StandardOutput", ""]], "cmake version "])], ""];
  If[executable =!= "", executable,
    Failure["SPINASBuild", <|"MessageTemplate" ->
      If[StringQ[option], "The specified CMake executable could not be run. Set CMakeExecutable to the full path of a working CMake installation.",
        "CMake could not be found on PATH or in its standard installation locations. Install CMake or set CMakeExecutable to its full path."],
      "CMakeExecutable" -> option|>]]
];
spinasProjectBuild[directory_String, name_String, root_String, runExamples_, cmakeOption_:Automatic] := Module[
  {cmake, configure, build, run, executable, configureArguments},
  cmake = spinasProjectCMakeExecutable[cmakeOption];
  If[FailureQ[cmake], Return[cmake]];
  configureArguments = {cmake,"-S",directory,"-B",FileNameJoin[{directory,"build"}]};
  (* A resolved checkout supersedes any prior generated build's cached root,
     header and library paths when the project is regenerated. *)
  If[root =!= "", configureArguments = Join[configureArguments,
    {"-U", "SPINAS_INCLUDE_DIR", "-U", "SPINAS_LIBRARY", "-DSPINAS_ROOT=" <> root}]];
  configure = spinasProjectCommand[configureArguments];
  If[Lookup[configure,"ExitCode",-1] =!= 0, Return[Failure["SPINASBuild", <|
    "MessageTemplate" -> "CMake could not configure the generated process.", "Directory" -> directory,
    "CommandResult" -> configure|>]]];
  build = spinasProjectCommand[{cmake,"--build",FileNameJoin[{directory,"build"}],"--parallel"}];
  If[Lookup[build,"ExitCode",-1] =!= 0, Return[Failure["SPINASBuild", <|
    "MessageTemplate" -> "The generated process did not compile or link.", "Directory" -> directory,
    "CommandResult" -> build|>]]];
  run = <|"Status" -> "NOT_REQUESTED"|>;
  If[TrueQ[runExamples],
    executable = FileNameJoin[{directory,"build",name <> If[$OperatingSystem === "Windows",".exe",""]}];
    run = spinasProjectCommand[{executable}];
    If[Lookup[run,"ExitCode",-1] =!= 0, Return[Failure["SPINASExamples", <|
      "MessageTemplate" -> "The compiled phase-space examples did not run successfully.",
      "Directory" -> directory, "CommandResult" -> run|>]]];
    run = Join[run,<|"Status" -> "RAN"|>]];
  <|"Build" -> Join[build,<|"Status" -> "BUILT", "Configure" -> configure,
    "CMakeExecutable" -> cmake|>], "Examples" -> run|>
];

WriteSPINASProcess[directory_String, process : {___String}, amplitude_, model_, opts:OptionsPattern[]] := Module[
  {options = Association[{opts}], name = OptionValue["ProcessName"], root, precision, ir,
   dir = ExpandFileName[directory], data, runtime, files, contents, written, result, build,
   overwrite = OptionValue["Overwrite"], examples = OptionValue["RunExamples"],
   details = OptionValue["ReturnDetails"], buildRequested = OptionValue["Build"],
   cmake = OptionValue["CMakeExecutable"], fixed, manifest},
  If[Complement[Keys[options],First /@ Options[WriteSPINASProcess]] =!= {} ||
      !StringQ[name] || !TrueQ[spinasCppIdentifierQ[name]] || StringTrim[directory] === "" || !AllTrue[{overwrite,examples,details,buildRequested},MemberQ[{True,False},#]&] ||
      !MemberQ[{Automatic,"LongDouble","Double"},OptionValue["Precision"]] ||
      !(OptionValue["SPINASDirectory"]===Automatic || StringQ[OptionValue["SPINASDirectory"]]) ||
      !(cmake === Automatic || (StringQ[cmake] && StringTrim[cmake] =!= "")),
    Message[WriteSPINASProcess::args]; Return[$Failed]];
  data = If[Head[model] === FeynmanModel,List @@ model,model];
  If[!ListQ[data],Return[Failure["InvalidModel",<||>]]];
  ir = spinasProcessIR[process,amplitude,data,<|"IncomingLegs"->OptionValue["IncomingLegs"],
    "ParameterRules"->OptionValue["ParameterRules"]|>];
  If[FailureQ[ir],Return[ir]];
  fixed = spinasFixedHelicities[OptionValue["FixedHelicities"], ir["Spin2"],
    TrueQ[# == 0] & /@ ir["MassValues"]];
  If[FailureQ[fixed], Return[fixed]];
  If[AnyTrue[Normal[fixed], ir["Helicities"][[First[#]]] =!= Last[#] &],
    Return[Failure["SPINASHelicities", <|"MessageTemplate" ->
      "FixedHelicities conflicts with the supplied fixed-helicity amplitude."|>]]];
  If[ir["LegCount"] < 4,Return[Failure["SPINASPhaseSpace",<|
    "MessageTemplate"->"The runnable project requires two incoming and at least two outgoing particles."|>]]];
  If[TrueQ[buildRequested] || TrueQ[examples],
    cmake = spinasProjectCMakeExecutable[cmake]; If[FailureQ[cmake], Return[cmake]]];
  runtime = FileNameJoin[{$spinasExportSourceDirectory,"spinas-runtime.hpp"}];
  If[!FileExistsQ[runtime],Return[Failure["SPINASRuntime",<|"MessageTemplate"->"The SPINAS runtime template is missing."|>]]];
  root = spinasProjectRoot[OptionValue["SPINASDirectory"]];
  precision = spinasProjectPrecision[root,OptionValue["Precision"]];
  files = AssociationThread[{"Header","Source","Demo","Runtime","CMake","README","Manifest"},
    FileNameJoin[{dir,#}]& /@ {"process.hpp","process.cpp","main.cpp","spinas-runtime.hpp","CMakeLists.txt","README.md","manifest.json"}];
  If[!TrueQ[overwrite] && AnyTrue[Values[files],FileExistsQ],
    Return[Failure["SPINASFilesExist",<|"MessageTemplate"->"Generated project files already exist. Use Overwrite -> True to replace them.","Directory"->dir|>]]];
  If[FileExistsQ[dir] && !DirectoryQ[dir],Return[Failure["SPINASDirectory",<|"Directory"->dir|>]]];
  manifest = <|"Generator"->"HEPCAT WriteSPINASProcess", "Process"->process,
    "IncomingLegs"->ir["IncomingLegs"], "Class"->name, "Precision"->precision,
    "ColorStripped"->ir["ColorStripped"],
    "ParameterOrder"->ir["ParameterNames"],
    "ParameterDefaults"->(<|"Real"->N[Re[#]],"Imaginary"->N[Im[#]]|>& /@ ir["ParameterDefaults"]),
    "Masses"->N[ir["MassValues"]], "Spin2"->ir["Spin2"], "Helicities"->ir["Helicities"]|>;
  contents = <|"Header"->spinasProjectHeader[name,ir], "Source"->spinasProjectSource[name,ir],
    "Demo"->spinasProjectMain[name], "Runtime"->Import[runtime,"Text"],
    "CMake"->spinasProjectCMake[name,root,precision], "README"->spinasProjectREADME[name,ir,precision]|>;
  If[!AllTrue[Values[contents],StringQ],Return[Failure["SPINASGeneration",<|"MessageTemplate"->"The C++ project could not be generated."|>]]];
  written = Quiet[Check[
    If[!DirectoryQ[dir],CreateDirectory[dir,CreateIntermediateDirectories->True]];
    AllTrue[KeyValueMap[Export[files[#1],#2,"Text"]&,contents],StringQ] &&
      StringQ[Export[files["Manifest"],manifest,"RawJSON"]],False]];
  If[!TrueQ[written],Return[Failure["SPINASWrite",<|"MessageTemplate"->"The output files could not all be written.","Directory"->dir|>]]];
  result = <|"Directory"->dir,"Files"->files,"Process"->process,"IncomingLegs"->ir["IncomingLegs"],
    "Build"-><|"Status"->"NOT_REQUESTED"|>,"Examples"-><|"Status"->"NOT_REQUESTED"|>|>;
  If[TrueQ[buildRequested] || TrueQ[examples],
    build = spinasProjectBuild[dir,name,root,examples,cmake]; If[FailureQ[build],Return[build]];
    result = Join[result,build]];
  If[TrueQ[details],result,dir]
];

(* A family contains one canonical formula per supplied helicity arrangement.
   Identical-leg maps select the other arrangements without copying formulas. *)
spinasHelicitySpecies[name_String, row_List, massless_] := {
  If[TrueQ[massless] && row[[5]] > 0,
    StringReplace[name, RegularExpression["\\.[+-]$"] -> ""], name],
  row[[4]], row[[5]], row[[6]], If[Length[row] >= 8, row[[8]], 1]};
spinasFixedHelicities[selection_, ranks_List, massless_List] := Module[{rules},
  rules = If[selection === Automatic, {}, selection];
  If[!ListQ[rules] || !AllTrue[rules, MatchQ[#, _Rule] &] ||
      !DuplicateFreeQ[First /@ rules] ||
      !AllTrue[rules, Function[rule, IntegerQ[First[rule]] &&
        1 <= First[rule] <= Length[ranks] && TrueQ[massless[[First[rule]]]] &&
        ranks[[First[rule]]] > 0 && IntegerQ[Last[rule]] &&
        Abs[Last[rule]] === ranks[[First[rule]]]]],
    Failure["SPINASHelicities", <|"MessageTemplate" ->
      "FixedHelicities must be a list of unique massless spinful leg numbers mapped to allowed doubled helicities."|>],
    Association[rules]]
];
spinasBareHelicityName[name_String] := StringReplace[name, RegularExpression["\\.[+-]$"] -> ""];
spinasFamilyIR[entries_List, model_List, options_Association, physicalSpec_:Automatic] := Module[
  {tag = Unique["spinasFamily"], fail, components, first, process, n, incoming,
   rows, rowFor, species, massless, ranks, states, modelNames, signed, groups,
   expected, configurations, mappings = {}, mappingFor, source, mapping, record,
   direct, symbols, defaults, names, firstPosition, rewriteCpp, rebind,
   finalNames, finalSpecies, finalCounts, symmetryDivisor, initialAverage,
   fixed, selectedStates, physicalNames, expectedName, partnerName, specifiedFixed = {},
   physicalProcess = Missing["NotSpecified"], processDescription = "", specifiedHelicity},
  fail[message_, data_: <||>] := Throw[Failure["SPINASHelicityFamily",
    Join[<|"MessageTemplate" -> message|>, data]], tag];
  Catch[
    If[entries === {} || Length[entries] > 4096 ||
        !AllTrue[entries, MatchQ[#, {_List, _}] && First[#] =!= {} &&
          AllTrue[First[#], StringQ] &],
      fail["Provide a nonempty list of {particle names, amplitude} pairs."]];
    components = spinasProcessIR[First[#], Last[#], model, options] & /@ entries;
    If[AnyTrue[components, FailureQ], Return[SelectFirst[components, FailureQ]]];
    first = First[components]; process = first["Process"]; n = first["LegCount"];
    incoming = first["IncomingLegs"]; ranks = first["Spin2"];
    massless = TrueQ[# == 0] & /@ first["MassValues"];
    fixed = spinasFixedHelicities[Lookup[options, "FixedHelicities", Automatic], ranks, massless];
    If[FailureQ[fixed], Return[fixed]];
    If[AnyTrue[components, TrueQ[#["ColorStripped"]] &],
      fail["A full color sum is not implemented. Use the fixed-helicity exporter for an explicitly color-stripped amplitude."]];
    rowFor[name_] := SelectFirst[model[[1]], ListQ[#] && Length[#] >= 6 &&
      MemberQ[#[[{2, 3}]], name] &, Missing["Particle"]];
    rows = rowFor /@ process;
    species = MapThread[spinasHelicitySpecies, {process, rows, massless}];
    If[AnyTrue[components, Function[component,
        component["LegCount"] =!= n || component["IncomingLegs"] =!= incoming ||
        component["Spin2"] =!= ranks || component["Masses"] =!= first["Masses"] ||
        MapThread[spinasHelicitySpecies,
          {component["Process"], rowFor /@ component["Process"], massless}] =!= species]],
      fail["Every family member must have the same ordered particle species, masses and incoming legs; only massless helicities may differ."]];
    If[physicalSpec =!= Automatic,
      If[!MatchQ[physicalSpec, _Rule] || !ListQ[First[physicalSpec]] ||
          !ListQ[Last[physicalSpec]] || Length[First[physicalSpec]] =!= 2 ||
          Length[Last[physicalSpec]] < 2 ||
          !AllTrue[Join[First[physicalSpec], Last[physicalSpec]], StringQ] ||
          Length[First[physicalSpec]] + Length[Last[physicalSpec]] =!= n,
        fail["Describe the physical process as two incoming particle names -> at least two outgoing names, in the formula's leg order."]];
      If[incoming =!= {1, 2},
        fail["The explicit physical process places its incoming legs first; IncomingLegs conflicts with that description."]];
      physicalNames = Join[First[physicalSpec], Last[physicalSpec]];
      Do[
        partnerName = If[process[[i]] === rows[[i, 2]], rows[[i, 3]], rows[[i, 2]]];
        expectedName = If[MemberQ[incoming, i], process[[i]], partnerName];
        If[massless[[i]] && ranks[[i]] > 0,
          (* Photon +/- are paired in the model, but their helicity labels
             are not flipped by HEPCAT's ReverseMomentum convention. Match
             physical flavor separately from the chosen SPINAS helicity. *)
          If[spinasBareHelicityName[physicalNames[[i]]] =!= spinasBareHelicityName[expectedName],
            fail["The physical process does not match the supplied all-ingoing particle species.",
              <|"Leg" -> i, "PhysicalParticle" -> physicalNames[[i]],
                "AllIngoingParticle" -> process[[i]]|>]];
          specifiedHelicity = Which[StringEndsQ[physicalNames[[i]], ".+"], ranks[[i]],
            StringEndsQ[physicalNames[[i]], ".-"], -ranks[[i]], True, 0];
          If[specifiedHelicity =!= 0, AppendTo[specifiedFixed, i -> specifiedHelicity]],
          If[physicalNames[[i]] =!= expectedName,
            fail["The physical process does not match the supplied all-ingoing particle species. Outgoing charged particles must be the conjugates of the analytic names.",
              <|"Leg" -> i, "PhysicalParticle" -> physicalNames[[i]],
                "ExpectedPhysicalParticle" -> expectedName|>]]],
        {i, n}];
      If[AnyTrue[specifiedFixed, KeyExistsQ[fixed, First[#]] && fixed[First[#]] =!= Last[#] &],
        fail["FixedHelicities conflicts with a helicity specified in the physical process."]];
      fixed = Join[fixed, Association[specifiedFixed]];
      physicalProcess = <|"Incoming" -> First[physicalSpec], "Outgoing" -> Last[physicalSpec]|>;
      processDescription = StringRiffle[First[physicalSpec], ", "] <> " -> " <>
        StringRiffle[Last[physicalSpec], ", "]];
    configurations = Lookup[components, "Helicities"];
    If[!DuplicateFreeQ[configurations],
      fail["Supply only one amplitude for each helicity arrangement. Alternative formulas for the same helicities do not form separate branches."]];
    modelNames = DeleteDuplicates[Flatten[(#[[{2, 3}]] &) /@ model[[1]]]];
    states = Table[If[!massless[[i]] || ranks[[i]] == 0, {0},
      signed = Select[modelNames, Function[name,
        StringMatchQ[name, ___ ~~ (".+" | ".-")] &&
        spinasHelicitySpecies[name, rowFor[name], True] === species[[i]]]];
      If[signed === {}, {-ranks[[i]], ranks[[i]]},
        Sort[DeleteDuplicates[If[StringEndsQ[#, ".+"], ranks[[i]], -ranks[[i]]] & /@ signed]]]],
      {i, n}];
    If[AnyTrue[Normal[fixed], !MemberQ[states[[First[#]]], Last[#]] &],
      fail["A selected helicity is not declared for this massless species in the supplied model."]];
    selectedStates = MapIndexed[If[KeyExistsQ[fixed, First[#2]], {fixed[First[#2]]}, #1] &, states];
    If[Times @@ (Length /@ selectedStates) > 4096,
      fail["The complete helicity family exceeds the supported export allowance of 4096 arrangements."]];
    expected = Tuples[selectedStates];
    groups = GatherBy[Select[Range[n], massless[[#]] && ranks[[#]] > 0 &],
      {species[[#]], MemberQ[incoming, #]} &];
    mappingFor[from_List, to_List] := Module[{image = Range[n], old, new, sign = 1},
      Do[
        Do[old = Select[group, from[[#]] === helicity &];
          new = Select[group, to[[#]] === helicity &];
          If[Length[old] =!= Length[new], Return[$Failed]];
          Do[image[[old[[j]]]] = new[[j]], {j, Length[old]}],
          {helicity, states[[First[group]]]}];
        If[OddQ[ranks[[First[group]]]], sign *= Signature[image[[group]]]],
        {group, groups}];
      If[!AllTrue[Range[n], from[[#]] === to[[image[[#]]]] &], Return[$Failed]];
      <|"LegPermutation" -> image, "FermionSign" -> sign|>];
    Do[
      direct = FirstPosition[configurations, configuration, Missing["NotSupplied"]];
      record = Missing["NoBranch"];
      If[!MissingQ[direct],
        record = <|"Helicities" -> configuration, "InputIndex" -> First[direct],
          "LegPermutation" -> Range[n], "FermionSign" -> 1, "Origin" -> "Supplied"|>,
        Do[mapping = mappingFor[configurations[[source]], configuration];
          If[AssociationQ[mapping], record = Join[<|"Helicities" -> configuration,
            "InputIndex" -> source, "Origin" -> "IdenticalLegPermutation"|>, mapping]; Break[]],
          {source, Length[components]}]];
      AppendTo[mappings, record], {configuration, expected}];
    If[AnyTrue[mappings, MissingQ],
      fail["The supplied amplitudes do not cover all requested massless helicities. Supply the missing helicity sectors; no zero amplitude or parity relation is assumed.",
        <|"MissingHelicities" -> Pick[expected, MissingQ /@ mappings, True]|>]];
    symbols = DeleteDuplicates[Flatten[Lookup[components, "ParameterSymbols"], 1]];
    names = ModelParameterName /@ symbols;
    If[!DuplicateFreeQ[names], fail["Model parameter names must be unambiguous across the complete family."]];
    defaults = Map[Function[symbol,
      source = SelectFirst[components, MemberQ[#["ParameterSymbols"], symbol] &];
      source["ParameterDefaults"][[First[FirstPosition[source["ParameterSymbols"], symbol]]]]], symbols];
    rewriteCpp[text_String, local_List] := StringReplace[text,
      MapIndexed[("parameters[" <> ToString[First[#2] - 1] <> "]") ->
        ("parameters[" <> ToString[First[FirstPosition[symbols, #1]] - 1] <> "]") &, local]];
    rebind[component_Association] := Join[component, <|
      "ExpressionCpp" -> rewriteCpp[component["ExpressionCpp"], component["ParameterSymbols"]],
      "ChainStatements" -> (rewriteCpp[#, component["ParameterSymbols"]] & /@ component["ChainStatements"]),
      "MassCpp" -> (rewriteCpp[#, component["ParameterSymbols"]] & /@ component["MassCpp"]),
      "ParameterSymbols" -> symbols, "ParameterNames" -> names,
      "ParameterDefaults" -> defaults|>];
    components = rebind /@ components; first = First[components];
    finalNames = Map[Function[i,
      If[process[[i]] === rows[[i, 2]], rows[[i, 3]], rows[[i, 2]]]], Complement[Range[n], incoming]];
    finalSpecies = MapThread[spinasHelicitySpecies,
      {finalNames, rows[[Complement[Range[n], incoming]]], massless[[Complement[Range[n], incoming]]]}];
    finalCounts = Length /@ Gather[finalSpecies];
    symmetryDivisor = Times @@ (Factorial /@ finalCounts);
    initialAverage = Times @@ Table[If[massless[[i]], Length[selectedStates[[i]]], ranks[[i]] + 1], {i, incoming}];
    Join[first, <|"Family" -> True, "Process" -> (First /@ species),
      "InputProcesses" -> Lookup[components, "Process"], "FamilyComponents" -> components,
      "Helicities" -> First[expected], "HelicityConfigurations" -> expected,
      "HelicityStates" -> selectedStates, "Branches" -> mappings,
      "FixedHelicities" -> Normal[fixed], "PhysicalProcess" -> physicalProcess,
      "ProcessDescription" -> processDescription,
      "InputBranchCount" -> Length[components],
      "GeneratedBranchCount" -> Count[Lookup[mappings, "Origin"], "IdenticalLegPermutation"],
      "InitialSpinAverage" -> initialAverage, "SymmetryFactor" -> 1/symmetryDivisor,
      "FinalStateSymmetryDivisor" -> symmetryDivisor,
      "FinalStateSpecies" -> (First /@ finalSpecies)|>], tag]
];

spinasFamilyProjectHeader[name_String, ir_Association] := Module[{header, scalarArguments, scalarValues, declarations},
  header = spinasProjectHeader[name, ir];
  scalarArguments = StringRiffle[Table["int ds" <> ToString[i], {i, ir["LegCount"]}], ", "];
  scalarValues = spinasCppArray[Table["ds" <> ToString[i], {i, ir["LegCount"]}]];
  declarations = StringRiffle[Table[
    "  cdouble component_" <> ToString[i - 1] <>
      "(const std::vector<std::vector<int>>& spinor_spins, const ParticleView& particle_view, const Momenta& momenta);",
    {i, ir["InputBranchCount"]}], "\n"];
  StringReplace[header, {
    "  using Spins = std::array<int, leg_count>;" ->
      "  using Spins = std::array<int, leg_count>;\n  static constexpr std::size_t helicity_count = " <>
        ToString[Length[ir["HelicityConfigurations"]]] <> ";\n  static std::array<Spins, helicity_count> helicity_configurations();\n" <>
        "  static ldouble incoming_spin_average();\n  static ldouble symmetry_factor();",
    "  cdouble amp(const Spins& spins);" -> "  cdouble amp(const Spins& spins);",
    "  ldouble spin_sum();" -> "  cdouble amp(" <> scalarArguments <> ") { return amp(Spins" <> scalarValues <> "); }\n  ldouble spin_sum();",
    "  cdouble component(const std::vector<std::vector<int>>& spinor_spins);" ->
      "  using ParticleView = std::array<spinas::particle*, leg_count>;\n" <>
      "  cdouble component(std::size_t source, const std::vector<std::vector<int>>& spinor_spins, const ParticleView& particle_view, const Momenta& momenta);\n" <> declarations}]
];

spinasFamilyProjectSource[name_String, ir_Association] := Module[
  {n = ir["LegCount"], ranks = ir["Spin2"], components = ir["FamilyComponents"],
   branches = ir["Branches"], prefix, componentCode, endpointRules, configurations,
   permutationRows, sourceIndices, signs, rankArray, dispatchCases, k},
  prefix = First[StringSplit[spinasProjectSource[name, ir], "cdouble " <> name <> "::component(", 2]];
  prefix = StringReplace[prefix, "constexpr std::size_t " <> name <> "::parameter_count;" ->
    "constexpr std::size_t " <> name <> "::parameter_count;\nconstexpr std::size_t " <> name <> "::helicity_count;"];
  endpointRules = Table[("particles[" <> ToString[i - 1] <> "].") ->
    ("particle_view[" <> ToString[i - 1] <> "]->"), {i, n}];
  componentCode = StringRiffle[MapIndexed[Function[{component, index}, StringRiffle[Join[{
      "cdouble " <> name <> "::component_" <> ToString[First[index] - 1] <>
        "(const std::vector<std::vector<int>>& spinor_spins, const ParticleView& particle_view, const Momenta& momenta) {"},
      ("  " <> StringReplace[#, endpointRules] &) /@ component["ChainStatements"], {
      "  return " <> StringReplace[component["ExpressionCpp"], endpointRules] <> ";", "}", ""}], "\n"]], components], "\n"];
  dispatchCases = Table["    case " <> ToString[i - 1] <> ": return component_" <> ToString[i - 1] <>
    "(spinor_spins,particle_view,momenta);", {i, Length[components]}];
  configurations = spinasCppArray[spinasCppArray[ToString /@ #] & /@ ir["HelicityConfigurations"]];
  permutationRows = spinasCppArray[spinasCppArray[ToString /@ (#["LegPermutation"] - 1)] & /@ branches];
  sourceIndices = spinasCppArray[ToString /@ (Lookup[branches, "InputIndex"] - 1)];
  signs = spinasCppArray[ToString /@ Lookup[branches, "FermionSign"]];
  rankArray = spinasCppArray[ToString /@ ranks];
  prefix <> componentCode <> StringRiffle[Join[{
    "cdouble " <> name <> "::component(std::size_t source, const std::vector<std::vector<int>>& spinor_spins, const ParticleView& particle_view, const Momenta& momenta) {",
    "  switch (source) {"}, dispatchCases, {
    "    default: throw std::logic_error(\"Unknown generated helicity component.\");", "  }", "}", "",
    "std::array<" <> name <> "::Spins," <> name <> "::helicity_count> " <> name <> "::helicity_configurations() {",
    "  return std::array<Spins,helicity_count>" <> configurations <> ";", "}", "",
    "ldouble " <> name <> "::incoming_spin_average() { return " <> spinasCppReal[ir["InitialSpinAverage"]] <> "; }",
    "ldouble " <> name <> "::symmetry_factor() { return " <> spinasCppReal[ir["SymmetryFactor"]] <> "; }", "",
    "cdouble " <> name <> "::amp(const Spins& spins) {",
    "  if (!momenta_ready) throw std::logic_error(\"Call set_momenta before amp.\");",
    "  static const std::array<int,leg_count> ranks = " <> rankArray <> ";",
    "  static const std::array<std::array<std::size_t,leg_count>,helicity_count> permutations = " <> permutationRows <> ";",
    "  static const std::array<std::size_t,helicity_count> sources = " <> sourceIndices <> ";",
    "  static const std::array<int,helicity_count> signs = " <> signs <> ";",
    "  const auto configurations = helicity_configurations(); std::size_t branch=helicity_count;",
    "  for (std::size_t candidate=0; candidate<helicity_count; ++candidate) {",
    "    bool match=true; for (std::size_t i=0; i<leg_count; ++i)",
    "      if (external_masses[i]==0 && spins[i]!=configurations[candidate][i]) match=false;",
    "    if (match) { branch=candidate; break; }", "  }",
    "  if (branch==helicity_count) throw std::invalid_argument(\"No supplied or identical-particle-related amplitude has these massless helicities.\");",
    "  ParticleView particle_view; Momenta selected_momenta;",
    "  for (std::size_t i=0; i<leg_count; ++i) {",
    "    particle_view[i]=&particles[permutations[branch][i]]; selected_momenta[i]=momenta[permutations[branch][i]];", "  }",
    "  std::vector<std::vector<std::vector<int>>> assignments(leg_count);",
    "  std::vector<std::vector<int>> spinor_spins(leg_count); ldouble normalization=1;",
    "  for (std::size_t i=0; i<leg_count; ++i) {",
    "    if (external_masses[i]==0) assignments[i]=spinor_assignments(0,0);",
    "    else { assignments[i]=spinor_assignments(ranks[i],spins[i]);",
    "      normalization*=spinor_normalization(ranks[i],spins[i]); }", "  }",
    "  std::function<cdouble(std::size_t)> sum = [&](std::size_t leg) -> cdouble {",
    "    if (leg==leg_count) return component(sources[branch],spinor_spins,particle_view,selected_momenta);",
    "    cdouble result(0,0); for (const auto& indices : assignments[leg]) {",
    "      spinor_spins[leg]=indices; result+=sum(leg+1);", "    } return result;", "  };",
    "  return ldouble(signs[branch])*normalization*sum(0);", "}", "",
    "ldouble " <> name <> "::spin_sum() {",
    "  static const std::array<int,leg_count> ranks = " <> rankArray <> ";",
    "  ldouble result=0;",
    "  for (const auto& helicities : helicity_configurations()) {",
    "    Spins spins=default_spins(); for (std::size_t i=0; i<leg_count; ++i)",
    "      if (external_masses[i]==0) spins[i]=helicities[i];",
    "    std::function<ldouble(std::size_t)> sum = [&](std::size_t leg) -> ldouble {",
    "      if (leg==leg_count) return std::norm(amp(spins));",
    "      if (external_masses[leg]==0 || ranks[leg]==0) return sum(leg+1);",
    "      ldouble value=0; for (int spin=-ranks[leg]; spin<=ranks[leg]; spin+=2) {",
    "        spins[leg]=spin; value+=sum(leg+1);", "      } return value;", "    };",
    "    result+=sum(0);", "  } return result;", "}", "",
    "ldouble " <> name <> "::amp2() { return spin_sum()*symmetry_factor()/incoming_spin_average(); }",
    "} // namespace hepcat_generated", ""}], "\n"]
];

spinasFamilyProjectMain[name_String] := StringReplace[spinasProjectMain[name], {
  "fixed massless helicities." -> "all exported massless helicities.",
  "fixed-helicity, initial-spin-averaged |M|^2" -> "spin/helicity-summed, initial-averaged, symmetry-weighted |M|^2",
  "No identical-particle phase-space factor is included." -> "The final-state identical-particle symmetry factor is included in amp2()."}];
spinasFamilyProjectREADME[name_String, ir_Association, precision_String] := StringReplace[
  spinasProjectREADME[name, ir, precision], {
    "## Build and run" ->
      If[ir["ProcessDescription"] === "", "", "Physical process: " <> ir["ProcessDescription"] <> ".\n\n"] <>
      "Selected fixed massless helicities (1-based leg, doubled helicity): " <>
      If[ir["FixedHelicities"] === {}, "none", ToString[ir["FixedHelicities"], InputForm]] <> ".\n\n" <>
      "Complex amp calls have no averaging or phase-space factor. Keep and coherently contract these amplitudes for spin correlations; multiplying separate spin sums loses interference. This project does not implement production-decay sewing.\n\n## Build and run",
    "Massless helicities are fixed by the exported process. Massive states are normalized symmetric spinor tensors." ->
      "The class includes every requested massless-helicity arrangement. Massive states use normalized symmetric spinor tensors. amp(ds1,...,dsn) and amp(Spins) accept doubled spins/helicities, as in native SPINAS processes.",
    "spin_sum sums all massive external spins. amp2 also averages incoming massive spins. Neither includes an identical-particle phase-space factor or a sum over other massless-helicity processes." ->
      "spin_sum sums |amp|^2 over all external massive spins and the requested massless helicities. amp2 divides by the unfixed incoming spin/helicity multiplicity (" <>
      ToString[ir["InitialSpinAverage"]] <> ") and multiplies the final-state identical-particle factor (1/" <>
      ToString[ir["FinalStateSymmetryDivisor"]] <> "). Fixed incoming helicities are not averaged. Identical particles are counted by species, including fermions, irrespective of helicity. A fixed outgoing helicity selects a labeled contribution to this fully symmetrized sum, not an automatically normalized tagged-particle cross section. No initial identical-particle factor is applied.",
    "The executable prints three deterministic" ->
      "Only canonical supplied formulas are compiled. Identical-particle-related helicity branches use role-preserving leg permutations, including the fermion exchange sign. No parity relation or missing zero amplitude is inferred.\n\nThe executable prints three deterministic"}];

spinasWriteFamily[directory_String, entries_List, model_, opts_List, physicalSpec_] := Module[
  {options = Join[Association[Options[WriteSPINASProcess]], Association[Reverse[Flatten[opts]]]],
   name, root, precision, ir, dir = ExpandFileName[directory], data, runtime,
   files, contents, written, result, build, cmake, manifest},
  name = options["ProcessName"];
  cmake = options["CMakeExecutable"];
  If[Complement[First /@ Flatten[opts], First /@ Options[WriteSPINASProcess]] =!= {} ||
      !StringQ[name] || !TrueQ[spinasCppIdentifierQ[name]] || StringTrim[directory] === "" ||
      !AllTrue[Lookup[options, {"Overwrite", "RunExamples", "ReturnDetails", "Build"}], MemberQ[{True, False}, #] &] ||
      !MemberQ[{Automatic, "LongDouble", "Double"}, options["Precision"]] ||
      !(options["SPINASDirectory"] === Automatic || StringQ[options["SPINASDirectory"]]) ||
      !(cmake === Automatic || (StringQ[cmake] && StringTrim[cmake] =!= "")),
    Message[WriteSPINASProcess::args]; Return[$Failed]];
  data = If[Head[model] === FeynmanModel, List @@ model, model];
  If[!ListQ[data], Return[Failure["InvalidModel", <||>]]];
  ir = spinasFamilyIR[entries, data,
    KeyTake[options, {"IncomingLegs", "ParameterRules", "FixedHelicities"}], physicalSpec];
  If[FailureQ[ir], Return[ir]];
  If[ir["LegCount"] < 4, Return[Failure["SPINASPhaseSpace", <|
    "MessageTemplate" -> "The runnable project requires two incoming and at least two outgoing particles."|>]]];
  If[TrueQ[options["Build"]] || TrueQ[options["RunExamples"]],
    cmake = spinasProjectCMakeExecutable[cmake]; If[FailureQ[cmake], Return[cmake]]];
  runtime = FileNameJoin[{$spinasExportSourceDirectory, "spinas-runtime.hpp"}];
  If[!FileExistsQ[runtime], Return[Failure["SPINASRuntime", <|"MessageTemplate" -> "The SPINAS runtime template is missing."|>]]];
  root = spinasProjectRoot[options["SPINASDirectory"]];
  precision = spinasProjectPrecision[root, options["Precision"]];
  files = AssociationThread[{"Header", "Source", "Demo", "Runtime", "CMake", "README", "Manifest"},
    FileNameJoin[{dir, #}] & /@ {"process.hpp", "process.cpp", "main.cpp", "spinas-runtime.hpp", "CMakeLists.txt", "README.md", "manifest.json"}];
  If[!TrueQ[options["Overwrite"]] && AnyTrue[Values[files], FileExistsQ],
    Return[Failure["SPINASFilesExist", <|"MessageTemplate" -> "Generated project files already exist. Use Overwrite -> True to replace them.", "Directory" -> dir|>]]];
  If[FileExistsQ[dir] && !DirectoryQ[dir], Return[Failure["SPINASDirectory", <|"Directory" -> dir|>]]];
  manifest = <|"Generator" -> "HEPCAT WriteSPINASProcess", "HelicityFamily" -> True,
    "Process" -> ir["Process"], "InputProcesses" -> ir["InputProcesses"],
    "IncomingLegs" -> ir["IncomingLegs"], "Class" -> name, "Precision" -> precision,
    "ColorStripped" -> False, "ParameterOrder" -> ir["ParameterNames"],
    "ParameterDefaults" -> (<|"Real" -> N[Re[#]], "Imaginary" -> N[Im[#]]|> & /@ ir["ParameterDefaults"]),
    "Masses" -> N[ir["MassValues"]], "Spin2" -> ir["Spin2"],
    "HelicityConfigurations" -> ir["HelicityConfigurations"], "Branches" -> ir["Branches"],
    "FixedHelicities" -> Association[(ToString[First[#]] -> Last[#]) & /@ ir["FixedHelicities"]],
    "InputBranchCount" -> ir["InputBranchCount"], "GeneratedBranchCount" -> ir["GeneratedBranchCount"],
    "InitialSpinAverage" -> ir["InitialSpinAverage"], "SymmetryFactor" -> N[ir["SymmetryFactor"]],
    "FinalStateSymmetryDivisor" -> ir["FinalStateSymmetryDivisor"],
    "SpinSumConvention" -> "All external massive spins and selected massless helicities; amp2 averages only unfixed incoming states and includes the outgoing identical-species factorial factor; selected outgoing helicities are labeled contributions; no color sum."|>;
  If[AssociationQ[ir["PhysicalProcess"]], manifest = Join[manifest,
    KeyTake[ir, {"PhysicalProcess", "ProcessDescription"}]]];
  contents = <|"Header" -> spinasFamilyProjectHeader[name, ir],
    "Source" -> spinasFamilyProjectSource[name, ir], "Demo" -> spinasFamilyProjectMain[name],
    "Runtime" -> Import[runtime, "Text"], "CMake" -> spinasProjectCMake[name, root, precision],
    "README" -> spinasFamilyProjectREADME[name, ir, precision]|>;
  If[!AllTrue[Values[contents], StringQ], Return[Failure["SPINASGeneration", <|"MessageTemplate" -> "The C++ project could not be generated."|>]]];
  written = Quiet[Check[
    If[!DirectoryQ[dir], CreateDirectory[dir, CreateIntermediateDirectories -> True]];
    AllTrue[KeyValueMap[Export[files[#1], #2, "Text"] &, contents], StringQ] &&
      StringQ[Export[files["Manifest"], manifest, "RawJSON"]], False]];
  If[!TrueQ[written], Return[Failure["SPINASWrite", <|"MessageTemplate" -> "The output files could not all be written.", "Directory" -> dir|>]]];
  result = Join[<|"Directory" -> dir, "Files" -> files, "Process" -> ir["Process"],
    "IncomingLegs" -> ir["IncomingLegs"], "Build" -> <|"Status" -> "NOT_REQUESTED"|>,
    "Examples" -> <|"Status" -> "NOT_REQUESTED"|>|>,
    KeyTake[ir, {"HelicityConfigurations", "InputBranchCount", "GeneratedBranchCount",
      "InitialSpinAverage", "SymmetryFactor", "FinalStateSymmetryDivisor", "FixedHelicities"}]];
  If[AssociationQ[ir["PhysicalProcess"]], result = Join[result,
    KeyTake[ir, {"PhysicalProcess", "ProcessDescription"}]]];
  If[TrueQ[options["Build"]] || TrueQ[options["RunExamples"]],
    build = spinasProjectBuild[dir, name, root, options["RunExamples"], cmake];
    If[FailureQ[build], Return[build]]; result = Join[result, build]];
  If[TrueQ[options["ReturnDetails"]], result, dir]
];

WriteSPINASProcess[directory_String, entries_List, model_, opts:OptionsPattern[]] :=
  spinasWriteFamily[directory, entries, model, {opts}, Automatic];
WriteSPINASProcess[directory_String, physicalProcess_Rule, entries_List, model_, opts:OptionsPattern[]] :=
  spinasWriteFamily[directory, entries, model, {opts}, physicalProcess];

WriteSPINASProcess[___] := (Message[WriteSPINASProcess::args];$Failed);
End[];
End[];
