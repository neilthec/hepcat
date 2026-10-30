(* ::Package:: *)

(* ::Section::Closed:: *)
(*Complete SPINAS Process Export*)

Begin["ConstructiveDiagrams`Private`"];

(* Numeric literals preserve the supplied numerical value without routing
   it through a machine-double C++ literal. *)
spinasCppReal[value_] := Module[{digits, exponent, sign, fraction},
  If[!TrueQ[NumberQ[value] && Im[value] == 0], Return[$Failed]];
  If[TrueQ[value == 0], Return["ldouble(0.0L)"]];
  sign = If[TrueQ[value < 0], "-", ""];
  {digits, exponent} = RealDigits[N[SetPrecision[Abs[value], 32], 32], 10, 24];
  If[!VectorQ[digits, IntegerQ], Return[$Failed]];
  fraction = StringReplace[StringJoin[ToString /@ Rest[digits]], RegularExpression["0+$"] -> ""];
  If[fraction === "", fraction = "0"];
  "ldouble(" <> sign <> ToString[First[digits]] <> "." <> fraction <>
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
   traceCode, chains = {}, chainStatements = {}, chainNames = {}, chainCpp, expressionCpp, massCpp,
   scalarExpressions = {}, scalarStatements = {}, scalarNames = {}, scalarComments = {},
   usedNames = {}, allocateName, cacheScalar, emitProduct, emitQuotient, emitSum,
   numerator, denominator, numeratorTerms, expressionPieces,
   numerical, coefficient, parts, head, explicit, indices, leg, slots, state,
   canonicalSymbol, protectedCrossing, outgoing, pairRules = {}, normalizeInput,
   allocateImplicit, spinSlot, slotDegrees, reservedSlots, availableSlots, assigned},
  fail[reason_String, value_: None] := Throw[Failure["SPINASExport", <|
    "MessageTemplate" -> reason, "UnsupportedExpression" -> HoldForm[value]|>], tag];
  allocateName[base_String] := Module[{name = base, suffix = 1},
    While[MemberQ[usedNames, name], suffix++; name = base <> "_" <> ToString[suffix]];
    AppendTo[usedNames, name]; name];
  cacheScalar[value_, base_String, code_String] := Module[{position, name},
    position = FirstPosition[scalarExpressions, value, Missing["New"]];
    If[!MissingQ[position], Return[scalarNames[[First[position]]]]];
    name = allocateName[base]; AppendTo[scalarExpressions, value]; AppendTo[scalarNames, name];
    AppendTo[scalarComments, spinasReadableMath[value]];
    AppendTo[scalarStatements, "const cdouble " <> name <> " = " <> code <> ";"]; name];
  emitProduct[values_List] := If[values === {}, "ldouble(1.0L)",
    StringRiffle[emit /@ values, " * "]];
  emitQuotient[value_] := Module[{top = Numerator[value], bottom = Denominator[value], topCode, bottomCode},
    topCode = If[Head[top] === Times, emitProduct[List @@ top], emit[top]];
    If[bottom === 1, Return[topCode]];
    bottomCode = If[Head[bottom] === Times, emitProduct[List @@ bottom], emit[bottom]];
    "(" <> topCode <> ") / (" <> bottomCode <> ")"];
  emitSum[values_List] := StringRiffle[MapIndexed[
    If[spinasNegativeTermQ[#1], If[First[#2] === 1, "-", "- "] <> emit[-#1],
      If[First[#2] === 1, "", "+ "] <> emit[#1]] &, values], " "];
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
      Head[value] === Plus, "(" <> emitSum[List @@ value] <> ")",
      Head[value] === Times, emitQuotient[value],
      Head[value] === spinasExportLorentzSquare, "mass2(" <> emitVector[First[value]] <> ")",
      Head[value] === Power && spinasMomentumExpressionQ[First[value]] && Last[value] === 2,
        "mass2(" <> emitVector[First[value]] <> ")",
      Head[value] === Power && IntegerQ[Last[value]] && Last[value] < 0,
        "(ldouble(1.0L) / (" <> emit[First[value]^(-Last[value])] <> "))",
      Head[value] === Power && MemberQ[{2, 3, 4}, Last[value]] &&
          MatchQ[First[value], _ModelParameter | _Symbol | _SpinorChain | _PropDen],
        With[{base = emit[First[value]]}, "(" <> StringRiffle[ConstantArray[base, Last[value]], " * "] <> ")"],
      Head[value] === Power && IntegerQ[Last[value]] && Abs[Last[value]] <= 2147483647,
        "std::pow(cdouble(" <> emit[First[value]] <> "), " <> ToString[Last[value]] <> ")",
      Head[value] === Power, "std::pow(cdouble(" <> emit[First[value]] <> "), " <> emit[Last[value]] <> ")",
      Head[value] === SpinorChain,
        With[{position = FirstPosition[chains, value, Missing["New"]]},
          If[MissingQ[position], AppendTo[chains, value];
            AppendTo[chainNames, allocateName[spinasChainName[value]]];
            AppendTo[chainStatements, "const cdouble " <> Last[chainNames] <> " = " <> chainCode[value] <> ";"];
            Last[chainNames], chainNames[[First[position]]]]],
      Head[value] === SpinorTrace, cacheScalar[value, spinasScalarName[value], traceCode[value]],
      Head[value] === MomProd && Length[value] === 2,
        cacheScalar[value, spinasScalarName[value],
          "dot(" <> emitVector[If[MatchQ[value[[1]], _Integer | _Multiparticle], Mom[value[[1]]], value[[1]]]] <> ", " <>
          emitVector[If[MatchQ[value[[2]], _Integer | _Multiparticle], Mom[value[[2]]], value[[2]]]] <> ")"],
      Head[value] === Mandelstahm,
        cacheScalar[value, spinasScalarName[value], "mass2(" <> emitVector[Total[Mom /@ List @@ value]] <> ")"],
      Head[value] === PropDen && MemberQ[{2, 3}, Length[value]],
        cacheScalar[value, spinasScalarName[value],
          "mass2(" <> emitVector[First[value]] <> ") - (" <> emit[value[[2]]] <> ") * (" <> emit[value[[2]]] <> ")" <>
          If[Length[value] === 3, " + cdouble(0.0L, 1.0L) * (" <> emit[value[[2]]] <> ") * (" <> emit[value[[3]]] <> ")", ""]],
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
    numerator = Numerator[crossed]; denominator = Denominator[crossed];
    numeratorTerms = If[Head[numerator] === Plus, List @@ numerator, {numerator}];
    expressionPieces = <|"NumeratorTerms" -> (emit[If[spinasNegativeTermQ[#], -#, #]] & /@ numeratorTerms),
      "NumeratorSigns" -> (If[spinasNegativeTermQ[#], -1, 1] & /@ numeratorTerms),
      "Denominator" -> emit[denominator], "UnitDenominator" -> (denominator === 1)|>;
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
      "ExpressionCpp" -> expressionCpp, "ExpressionPieces" -> expressionPieces,
      "ChainStatements" -> chainStatements, "Chains" -> chains, "ChainNames" -> chainNames,
      "ScalarExpressions" -> scalarExpressions, "ScalarNames" -> scalarNames,
      "ScalarStatements" -> scalarStatements, "ScalarComments" -> scalarComments,
      "OriginalExpression" -> original, "CrossedExpression" -> crossed,
      "IndexedExpression" -> indexed, "ExternalMassRules" -> massRules,
      "ColorStripped" -> AnyTrue[rows, Length[#] >= 8 && #[[8]] =!= 1 &]|>, tag]
];

(* ::Subsection::Closed:: *)
(*Readable Formula Output*)

spinasNegativeTermQ[value_] := With[{coefficient = If[Head[value] === Times, First[value], value]},
  NumberQ[coefficient] && TrueQ[Im[coefficient] == 0 && Re[coefficient] < 0]];

spinasReadableIdentifier[text_String] := Module[{name},
  name = StringTrim[StringReplace[text, {
    RegularExpression["[^A-Za-z0-9_]+"] -> "_", RegularExpression["_+"] -> "_"}], "_"];
  If[name === "", name = "value"];
  If[StringLength[name] > 48, name = StringTake[name, 48]];
  If[!spinasCppIdentifierQ[name], name = "parameter_" <> name]; name
];

spinasChainName[chain_SpinorChain] := Module[{items = List @@ chain, labels, separator},
  labels = Join[{ToString[rspSpinorLeg[First[items]]]},
    (If[MatchQ[#, Mom[_Integer]], ToString[First[#]], "P"] & /@ Take[items, {2, -2}]),
    {ToString[rspSpinorLeg[Last[items]]]}];
  separator = If[AnyTrue[labels, StringLength[#] > 1 &] || MemberQ[labels, "P"], "_", ""];
  If[rspSpinorType[First[items]] === "Angle", "a", "s"] <>
    StringRiffle[labels, separator] <> If[rspSpinorType[Last[items]] === "Angle", "a", "s"]
];

spinasReadableMath[value_] := Which[
  Head[value] === ModelParameter, ModelParameterName[value],
  Head[value] === Mom, If[Head[First[value]] === Multiparticle,
    "(" <> StringRiffle[("p" <> ToString[#]) & /@ List @@ First[value], " + "] <> ")",
    "p" <> ToString[First[value]]],
  Head[value] === Plus, "(" <> StringRiffle[MapIndexed[
    If[spinasNegativeTermQ[#1], If[First[#2] === 1, "-", "- "] <> spinasReadableMath[-#1],
      If[First[#2] === 1, "", "+ "] <> spinasReadableMath[#1]] &, List @@ value], " "] <> ")",
  Head[value] === Times, If[First[value] === -1,
    "-" <> spinasReadableMath[-value], StringRiffle[spinasReadableMath /@ List @@ value, " * "]],
  Head[value] === Power, "(" <> spinasReadableMath[First[value]] <> ")^" <> spinasReadableMath[Last[value]],
  Head[value] === SpinorChain, Module[{items = List @@ value, left, right, middle},
    left = If[rspSpinorType[First[items]] === "Angle", "<", "["];
    right = If[rspSpinorType[Last[items]] === "Angle", ">", "]"];
    middle = spinasReadableMath /@ Take[items, {2, -2}];
    left <> ToString[rspSpinorLeg[First[items]]] <>
      If[middle === {}, " ", "|" <> StringRiffle[middle, " "] <> "|"] <>
      ToString[rspSpinorLeg[Last[items]]] <> right],
  Head[value] === PropDen,
    spinasReadableMath[First[value]] <> "^2 - (" <> spinasReadableMath[value[[2]]] <> ")^2" <>
      If[Length[value] === 3, " + i * " <> spinasReadableMath[value[[2]]] <> " * " <>
        spinasReadableMath[value[[3]]], ""],
  Head[value] === MomProd, StringRiffle[
    spinasReadableMath[spinasMomentumArgument[#]] & /@ List @@ value, " . "],
  Head[value] === Mandelstahm, "(" <>
    StringRiffle[("p" <> ToString[#]) & /@ List @@ value, " + "] <> ")^2",
  Head[value] === spinasExportLorentzSquare, "(" <> spinasReadableMath[First[value]] <> ")^2",
  Head[value] === SpinorTrace, "chiral trace(" <> StringRiffle[spinasReadableMath /@ List @@ value, " "] <> ")",
  True, StringReplace[ToString[value, InputForm], {"\n" -> " ", "\r" -> " "}]
];

spinasScalarName[value_] := Module[{labels, vector, momentumLabel},
  momentumLabel[leg_Integer] := ToString[leg];
  momentumLabel[Mom[leg_Integer]] := ToString[leg];
  momentumLabel[_] := "P";
  Which[
    Head[value] === PropDen,
      vector = First[value] /. Mom[Multiparticle[items___]] :> Total[Mom /@ {items}];
      labels = DeleteDuplicates[Cases[vector, Mom[i_Integer] :> i, {0, Infinity}]];
      If[TrueQ[value[[2]] === 0], "s", "t"] <>
        If[labels === {}, "P", StringRiffle[ToString /@ labels,
          If[AnyTrue[labels, # > 9 &], "_", ""]]],
    Head[value] === Mandelstahm, "s" <> StringRiffle[ToString /@ List @@ value,
      If[AnyTrue[List @@ value, # > 9 &], "_", ""]],
    Head[value] === MomProd, "p" <> momentumLabel[First[value]] <> "_dot_p" <> momentumLabel[Last[value]],
    Head[value] === SpinorTrace, "trace",
    True, "invariant"]
];

(* This wrapping operates only on generated C++ arithmetic, never on string
   literals. Whitespace is free in these expressions and names remain intact. *)
spinasReadableCppLines[text_String, indent_String : "  ", width_Integer : 112] := Module[
  {words = StringSplit[text], lines = {}, line = indent, continuation = indent <> "  "},
  Do[If[StringLength[line] > StringLength[indent] && StringLength[line] + StringLength[word] + 1 > width,
      AppendTo[lines, line]; line = continuation <> word,
      line = line <> If[StringTrim[line] === "", "", " "] <> word], {word, words}];
  If[StringTrim[line] =!= "", AppendTo[lines, line]]; lines
];

spinasReadableCommentLines[text_String] := ("  // " <> # &) /@
  spinasReadableCppLines[StringReplace[text,
    {"\n" -> " ", "\r" -> " ", "\t" -> " ", "\\" -> "/"}], "", 104];

spinasComponentName[ir_Association] := "helicity_" <>
  If[AllTrue[ir["Helicities"], # === 0 &], "tensor",
    StringJoin[If[# > 0, "p", "m"] & /@ DeleteCases[ir["Helicities"], 0]]];

spinasReadableComponentBody[ir_Association, family_:False] := Module[
  {n = ir["LegCount"], ranks = ir["Spin2"], used, aliases, parameterRules, spinRules,
   legRules, substitutions, rewrite, lines = {}, parameterLines = {}, legLines = {}, spinLines = {},
   addSection, name, base, suffix, text, pieces, terms, signs, code, prefix, emitted,
   chains = Lookup[ir, "Chains", {}], scalars = Lookup[ir, "ScalarStatements", {}]},
  used = Join[Lookup[ir, "ChainNames", {}], Lookup[ir, "ScalarNames", {}],
    {"numerator", "denominator", "spinor_spins", "particle_view"},
    Table["p" <> ToString[i], {i, n}],
    Flatten[Table[If[ranks[[i]] > 1,
      Table["ds" <> ToString[i] <> "_" <> ToString[j], {j, ranks[[i]]}],
      {"ds" <> ToString[i]}], {i, n}]]];
  aliases = Table[
    base = spinasReadableIdentifier[ir["ParameterNames"][[i]]]; name = base; suffix = 1;
    If[MemberQ[used, name], base = "parameter_" <> base; name = base];
    While[MemberQ[used, name], suffix++; name = base <> "_" <> ToString[suffix]];
    AppendTo[used, name];
    AppendTo[parameterLines, "  const cdouble " <> name <> " = parameters[" <> ToString[i - 1] <> "];" ];
    name, {i, Length[ir["ParameterNames"]]}];
  parameterRules = MapIndexed[("parameters[" <> ToString[First[#2] - 1] <> "]") -> #1 &, aliases];
  legRules = Table[("particles[" <> ToString[i - 1] <> "].") -> ("p" <> ToString[i] <> "."), {i, n}];
  spinRules = Flatten[Table[
    Table[("spinor_spins[" <> ToString[i - 1] <> "][" <> ToString[j - 1] <> "]") ->
      ("ds" <> ToString[i] <> If[ranks[[i]] > 1, "_" <> ToString[j], ""]),
      {j, ranks[[i]]}], {i, n}]];
  substitutions = Join[parameterRules, legRules, spinRules];
  rewrite[value_String] := StringReplace[value, substitutions];
  text = StringRiffle[Join[ir["ChainStatements"], scalars], "\n"];
  Do[
    If[StringContainsQ[text, "particles[" <> ToString[i - 1] <> "]."],
      AppendTo[legLines, "  auto& p" <> ToString[i] <> " = " <>
        If[TrueQ[family], "*particle_view[" <> ToString[i - 1] <> "]", "particles[" <> ToString[i - 1] <> "]"] <> ";"]];
    Do[If[StringContainsQ[text, "spinor_spins[" <> ToString[i - 1] <> "][" <> ToString[j - 1] <> "]"],
      AppendTo[spinLines, "  const int ds" <> ToString[i] <> If[ranks[[i]] > 1, "_" <> ToString[j], ""] <>
        " = spinor_spins[" <> ToString[i - 1] <> "][" <> ToString[j - 1] <> "];" ]],
      {j, ranks[[i]]}], {i, n}];
  addSection[label_String, values_List] := If[values =!= {},
    lines = Join[lines, {"  // " <> label}, values, {""}]];
  addSection["Model parameters.", parameterLines];
  addSection["External particles and doubled spin indices.", Join[legLines, spinLines]];
  If[scalars =!= {},
    lines = Join[lines, {"  // Propagators and scalar invariants (physical incoming/outgoing momenta)."}];
    Do[lines = Join[lines,
      spinasReadableCommentLines[ir["ScalarNames"][[i]] <> " = " <> ir["ScalarComments"][[i]]],
      spinasReadableCppLines[rewrite[scalars[[i]]]]], {i, Length[scalars]}];
    AppendTo[lines, ""]];
  If[ir["ChainStatements"] =!= {},
    AppendTo[lines, "  // Spinor products. The a/s names denote angle/square endpoints."];
    Do[lines = Join[lines, If[i <= Length[chains],
        spinasReadableCommentLines[ir["ChainNames"][[i]] <> " = " <> spinasReadableMath[chains[[i]]]], {}],
      spinasReadableCppLines[rewrite[ir["ChainStatements"][[i]]]]], {i, Length[ir["ChainStatements"]]}];
    AppendTo[lines, ""]];
  pieces = ir["ExpressionPieces"]; terms = rewrite /@ pieces["NumeratorTerms"];
  signs = pieces["NumeratorSigns"];
  If[TrueQ[pieces["UnitDenominator"]] && Length[terms] === 1 && StringLength[First[terms]] < 84,
    Return[Join[lines, {"  return " <> If[First[signs] < 0, "-", ""] <> First[terms] <> ";"}]]];
  AppendTo[lines, "  const cdouble numerator ="];
  Do[prefix = If[i === 1, If[signs[[i]] < 0, "-", ""], If[signs[[i]] < 0, "- ", "+ "]];
    code = prefix <> terms[[i]] <> If[i === Length[terms], ";", ""];
    emitted = spinasReadableCppLines[code, "      "];
    lines = Join[lines, emitted], {i, Length[terms]}];
  If[TrueQ[pieces["UnitDenominator"]], AppendTo[lines, "  return numerator;"],
    lines = Join[lines, spinasReadableCppLines["const cdouble denominator = " <> rewrite[pieces["Denominator"]] <> ";"],
      {"", "  return numerator / denominator;"}]];
  lines
];

End[];

(* ::Section::Closed:: *)
(*Complete SPINAS Process Projects*)

Begin["ConstructiveDiagrams`"];
WriteSPINASProcess::usage = "WriteSPINASProcess[directory, particles, amplitude, model] writes a complete C++11 SPINAS process project for two incoming and at least two outgoing particles. The amplitude uses HEPCAT's all-ingoing convention; IncomingLegs -> {1,2} selects physical incoming legs and outgoing legs are crossed automatically, including angle-spinor signs. The project contains process.hpp, process.cpp, main.cpp, a deterministic physical phase-space example generator, CMakeLists.txt, README.md and manifest.json. SPINASDirectory -> Automatic looks for SPINAS_ROOT or the sibling spinas checkout; an explicit path overrides it. Model parameter defaults come from ModelParameterRules; ParameterRules overrides them locally. ProcessName sets a C++ class/executable name. Build -> True configures and links the project against the existing SPINAS library; RunExamples -> True also builds and runs three sample points. Precision -> Automatic reads the local SPINAS build configuration; LongDouble and Double are explicit choices. Overwrite -> False protects existing project files. ReturnDetails -> False normally returns the output directory; True adds file paths and build/run results. Fixed massless helicities are retained. amp2 averages incoming massive spins and sums outgoing massive spins at those fixed helicities, without an identical-particle phase-space factor or color sums/averages. Unsupported expressions return Failure before files are written; this function does not complete or validate the physical amplitude.";
WriteSPINASProcess::usage = WriteSPINASProcess::usage <> " The family overload WriteSPINASProcess[directory, {{particles1, amplitude1}, ...}, model] combines helicity-related amplitudes into one class. It fills identical-species, same-role massless leg permutations with their proper statistics sign, without inferring parity or missing zero amplitudes. Incomplete coverage is rejected before writing files. amp(ds1,...,dsn) and amp(Spins) select doubled spins and helicities; spin_sum sums all external spin/helicity states; amp2 also averages incoming states and includes the outgoing identical-species factorial factor, including identical fermions. Ordinary projects use persistent native SPINAS products and direct helicity branches; very large families retain canonical dispatch to bound source size. Colored families are currently refused because a full color sum is not implemented. The original four-argument fixed-helicity overload retains its existing spin-sum and symmetry-factor conventions.";
WriteSPINASProcess::usage = WriteSPINASProcess::usage <> " CMakeExecutable -> Automatic searches PATH and standard installation locations, including the macOS CMake application and Homebrew, so desktop Mathematica need not inherit the shell's PATH. An explicit full path selects another CMake installation. A missing build tool is reported before generating files; source-only export does not require CMake.";
WriteSPINASProcess::usage = WriteSPINASProcess::usage <> " WriteSPINASProcess[directory, incomingNames -> outgoingNames, amplitudeEntries, model] explicitly describes the physical process. The entries still contain all-ingoing particle lists and un-crossed formulas in that same leg order, with incoming legs first. Outgoing charged particle names are checked against the conjugate all-ingoing species. Generic massless names such as A sum their helicities; A.+ or A.- selects one. The established HEPCAT/SPINAS helicity sign convention is retained, and ReverseMomentum crosses outgoing momenta and angle spinors exactly once. FixedHelicities -> {leg -> doubledHelicity, ...} selects massless legs in either grouped overload. Only unfixed incoming helicities are averaged. A selected outgoing helicity gives a labeled contribution with the same identical-species phase-space factor. Complex amp calls preserve phases and include no spin sums, incoming averages or phase-space factors; coherent production-decay sewing is not performed by this exporter. Runnable projects currently require two incoming particles.";
WriteSPINASProcess::usage = WriteSPINASProcess::usage <> " Generated projects also contain validate_calchep(options) and a process_validate_calchep executable for an independent, seeded 1000-point comparison with an on-the-fly CalcHEP process. CalcHEPDirectory -> Automatic locates the local installation. CalcHEPModel -> {directory, number} selects the equivalent theory; Automatic recognizes the bundled massive-electron QED models. RunCalcHEPValidation -> True builds and runs this check, using CalcHEPPoints -> 1000 and CalcHEPSeed -> Automatic by default. Automatic chooses a seed from the time when validation runs and reports the concrete seed for replay; an explicit unsigned 64-bit integer fixes it. Complete unpolarized colorless processes are supported; fixed-helicity or color-stripped comparisons report unavailable. Ordinary SPINAS execution does not require CalcHEP.";
WriteSPINASProcess::usage = WriteSPINASProcess::usage <> " If the CalcHEP numerical libraries are available, Build -> True also creates build/n_calchep and a calchep/n_calchep session launcher. This uses CalcHEP's numerical menus, VEGAS, cuts and histograms with the exported SPINAS matrix element, without CalcHEP symbolic generation or a CalcHEPModel counterpart. Independent model parameters remain adjustable, dependent functions are recomputed, and mass/width changes are propagated. Fixed helicities remain labeled contributions. The ordinary SPINAS project remains usable without CalcHEP.";
Options[WriteSPINASProcess] = {"SPINASDirectory" -> Automatic,
  "IncomingLegs" -> {1, 2}, "FixedHelicities" -> Automatic, "ParameterRules" -> Automatic,
  "ProcessName" -> "hepcat_process", "Precision" -> Automatic,
  "Build" -> False, "RunExamples" -> False, "CMakeExecutable" -> Automatic, "Overwrite" -> False,
  "CalcHEPDirectory" -> Automatic, "CalcHEPModel" -> Automatic,
  "RunCalcHEPValidation" -> False, "CalcHEPPoints" -> 1000, "CalcHEPSeed" -> Automatic,
  "ReturnDetails" -> False};
WriteSPINASProcess::args = "Expected an output directory, all-ingoing amplitude entries, a model and valid export options, optionally preceded by incoming particle names -> outgoing particle names. The fixed-helicity form takes an ordered all-ingoing particle list and amplitude instead of entries.";

Begin["ConstructiveDiagrams`Private`"];
$spinasExportSourceDirectory = DirectoryName[$InputFileName];

spinasCppIdentifierQ[name_String] := StringMatchQ[name, RegularExpression["[A-Za-z][A-Za-z0-9_]*"]] &&
  !StringContainsQ[name,"__"] &&
  !StringMatchQ[name, RegularExpression["component_[0-9]+|helicity_[pm]+|helicity_tensor"]] && !MemberQ[{
    "std", "spinas", "hepcat_generated", "hepcat_spinas", "Parameters", "Momenta", "Spins",
    "all", "clean", "help", "test", "install", "package", "package_source",
    "edit_cache", "rebuild_cache", "list_install_components",
    "leg_count", "parameter_count", "default_parameters", "default_spins", "incoming_legs",
    "get_parameters", "masses", "set_parameters", "set_momenta", "set_masses", "initialize_products",
    "amp", "spin_sum", "amp2", "validate_calchep", "n_calchep",
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

(* ::Subsection::Closed:: *)
(*Independent CalcHEP Validation*)

spinasCalcHEPRoot[option_] := Module[{parents, candidates, valid},
  If[option === None, Return[""]];
  valid[path_] := DirectoryQ[path] &&
    FileExistsQ[FileNameJoin[{path, "c_source", "dynamicME", "include", "dynamic_cs.h"}]] &&
    FileExistsQ[FileNameJoin[{path, "bin", "s_calchep"}]];
  parents = If[StringQ[option], {ExpandFileName[option]},
    Join[Select[Environment /@ {"CALCHEP_ROOT", "CALCHEP"}, StringQ[#] && # =!= "" &],
      {FileNameJoin[{ParentDirectory[ParentDirectory[$spinasExportSourceDirectory]], "CalcHEP"}],
        FileNameJoin[{$HomeDirectory, "code", "CalcHEP"}]}]];
  candidates = Flatten[Table[Join[{parent},
    If[DirectoryQ[parent], Reverse[Sort[FileNames["calchep_*", parent]]], {}]], {parent, parents}]];
  SelectFirst[DeleteDuplicates[candidates], valid, ""]
];

spinasCalcHEPOptionsQ[options_Association] :=
  (options["CalcHEPDirectory"] === Automatic || options["CalcHEPDirectory"] === None ||
    StringQ[options["CalcHEPDirectory"]]) &&
  (MemberQ[{Automatic, None}, options["CalcHEPModel"]] ||
    MatchQ[options["CalcHEPModel"], {_String, _Integer?Positive}]) &&
  MemberQ[{True, False}, options["RunCalcHEPValidation"]] &&
  IntegerQ[options["CalcHEPPoints"]] && 0 < options["CalcHEPPoints"] <= 2147483647 &&
  (options["CalcHEPSeed"] === Automatic || IntegerQ[options["CalcHEPSeed"]] &&
    0 <= options["CalcHEPSeed"] <= 18446744073709551615);

(* Automatic model pairing recognizes the repository's QED models. Matching
   external species alone would confuse QED with the electroweak theory. *)
spinasCalcHEPQEDModelQ[data_List] := Module[{pdgs, scopes, knownScopes, triples, reference, canonical},
  If[Length[data] =!= 4 || !ListQ[First[data]], Return[False]];
  pdgs = Sort[DeleteDuplicates[Abs[data[[1, All, 4]]]]];
  If[pdgs =!= {11, 22}, Return[False]];
  scopes = DeleteDuplicates[Cases[data, ModelParameter[_, scope_String, _] :> scope, Infinity]];
  knownScopes = modelParameterScope[FileNameJoin[{ParentDirectory[$spinasExportSourceDirectory], "models"}], #] & /@ {2, 3};
  If[MemberQ[scopes, modelParameterScope[
      FileNameJoin[{ParentDirectory[$spinasExportSourceDirectory], "model_feynman"}], 2]], Return[True]];
  If[Intersection[scopes, knownScopes] === {}, Return[False]];
  reference = ReadModel[FileNameJoin[{ParentDirectory[$spinasExportSourceDirectory], "models"}], 2];
  If[!ListQ[reference], Return[False]];
  triples[rows_] := Select[rows, ListQ[#] && Length[#] >= 5 &&
    Count[Drop[#, -2], name_String /; name =!= ""] === 3 &];
  canonical[value_] := value /. ModelParameter[name_String, _, _] :> spinasCalcHEPParameterName[name];
  Sort[canonical[triples[data[[2]]]]] === Sort[canonical[triples[reference[[2]]]]]
];

spinasCalcHEPConfiguration[ir_Association, data_List, supplied_Association] := Module[
  {options = Join[Association[Options[WriteSPINASProcess]], supplied], root, spec, modelDirectory = "",
   modelNumber = 0, process, rows, incoming, order, pdgs, physical, selfConjugate,
   available = True, reasons = {}, massless, outgoing, groups, symmetry, factor = 1, physicalNames},
  root = spinasCalcHEPRoot[options["CalcHEPDirectory"]];
  spec = options["CalcHEPModel"];
  If[spec === Automatic && spinasCalcHEPQEDModelQ[data],
    spec = {FileNameJoin[{ParentDirectory[$spinasExportSourceDirectory], "model_feynman"}], 2}];
  If[MatchQ[spec, {_String, _Integer?Positive}],
    modelDirectory = ExpandFileName[First[spec]]; modelNumber = Last[spec],
    available = False; AppendTo[reasons, If[spec === None, "CalcHEP validation was disabled.",
      "Select the equivalent CalcHEP model with CalcHEPModel -> {directory, number}."]]];
  If[modelDirectory =!= "" && !AllTrue[{"prtcls", "vars", "func", "lgrng"},
      FileExistsQ[FileNameJoin[{modelDirectory, # <> ToString[modelNumber] <> ".mdl"}]] &],
    available = False; AppendTo[reasons, "The selected CalcHEP model tables are missing."]];
  process = If[TrueQ[Lookup[ir, "Family", False]], First[ir["InputProcesses"]], ir["Process"]];
  incoming = ir["IncomingLegs"]; outgoing = Complement[Range[ir["LegCount"]], incoming];
  order = Join[incoming, outgoing];
  rows = Map[Function[name, SelectFirst[data[[1]], MemberQ[#[[{2, 3}]], name] &]], process];
  pdgs = MapThread[Function[{name, row},
    selfConjugate = spinasBareHelicityName[row[[2]]] === spinasBareHelicityName[row[[3]]];
    If[selfConjugate || name === row[[2]], row[[4]], -row[[4]]]], {process, rows}];
  physical = MapIndexed[If[MemberQ[outgoing, First[#2]] &&
      spinasBareHelicityName[rows[[First[#2], 2]]] =!= spinasBareHelicityName[rows[[First[#2], 3]]], -#1, #1] &, pdgs];
  physicalNames = MapIndexed[Function[{particle, index}, With[{i = First[index]},
    spinasBareHelicityName[If[MemberQ[outgoing, i] &&
        spinasBareHelicityName[rows[[i, 2]]] =!= spinasBareHelicityName[rows[[i, 3]]],
      If[particle === rows[[i, 2]], rows[[i, 3]], rows[[i, 2]]], particle]]]], process];
  massless = TrueQ[# == 0] & /@ ir["MassValues"];
  If[TrueQ[Lookup[ir, "ColorStripped", False]] ||
      AnyTrue[rows, Length[#] >= 8 && #[[8]] =!= 1 &],
    available = False; AppendTo[reasons, "A color-stripped SPINAS expression cannot be compared with CalcHEP's full color sum."]];
  If[TrueQ[Lookup[ir, "Family", False]],
    If[Lookup[ir, "FixedHelicities", {}] =!= {}, available = False;
      AppendTo[reasons, "The unpolarized CalcHEP comparison requires all external massless helicities."]],
    If[AnyTrue[Range[ir["LegCount"]], massless[[#]] && ir["Spin2"][[#]] > 0 &],
      available = False; AppendTo[reasons, "Export the complete helicity family for an unpolarized CalcHEP comparison."]];
    groups = Gather[physical[[outgoing]]]; symmetry = 1/(Times @@ (Factorial[Length[#]] & /@ groups));
    factor = symmetry];
  If[AnyTrue[ir["ParameterDefaults"], !TrueQ[Im[#] == 0] &],
    available = False; AppendTo[reasons, "The CalcHEP parameter interface requires real model inputs."]];
  If[root === "", AppendTo[reasons, "CalcHEP was not found; set CalcHEPDirectory or CALCHEP_ROOT before building validation."]];
  <|"Status" -> If[available && root =!= "", "AVAILABLE", "UNAVAILABLE"],
    "Supported" -> available, "Reason" -> StringRiffle[reasons, " "],
    "CalcHEPDirectory" -> root, "ModelDirectory" -> modelDirectory, "ModelNumber" -> modelNumber,
    "PhysicalPDGs" -> physical[[order]], "LegOrder" -> order,
    "ProcessDescription" -> StringRiffle[physicalNames[[incoming]], ", "] <> " -> " <>
      StringRiffle[physicalNames[[outgoing]], ", "],
    "ParameterNames" -> ir["ParameterNames"], "SPINASNormalizationFactor" -> factor,
    "Points" -> options["CalcHEPPoints"],
    "Seed" -> Replace[options["CalcHEPSeed"], Automatic -> "Automatic"]|>
];

spinasCalcHEPSource[name_String, ir_Association, config_Association] := Module[
  {start, body, parameterLines, aliases, used, base, alias, suffix, strongIndex, strongCode,
   identity, orderLines, massCheck, referencePoint, factorLines, amplitudeCode},
  start = {"#include \"process.hpp\"", "#include <iostream>", "#include <stdexcept>", "",
    "#ifdef HEPCAT_HAS_CALCHEP", "#include \"calchep-runtime.hpp\"", "#endif", "",
    "namespace hepcat_generated {", "",
    "hepcat_spinas::ValidationReport " <> name <> "::validate_calchep(",
    "    const hepcat_spinas::ValidationOptions& options) {"};
  If[!TrueQ[config["Supported"]], Return[StringRiffle[Join[start, {
    "  return hepcat_spinas::unavailable_validation(options,",
    "    " <> spinasCppQuoted[config["Reason"]] <> ");", "}", "",
    "} // namespace hepcat_generated", ""}], "\n"]]];
  used = {"spinas_process", "calchep_process", "parameters", "model_parameters", "model_directory",
    "model_number", "particle_pdgs", "external_order", "options", "point", "final_state_factor",
    "spinas_squared", "calchep_squared", "name", "values"};
  aliases = Table[
    base = spinasReadableIdentifier[parameter]; alias = base; suffix = 1;
    While[MemberQ[used, alias], suffix++; alias = base <> "_" <> ToString[suffix]];
    AppendTo[used, alias]; alias, {parameter, config["ParameterNames"]}];
  parameterLines = MapIndexed["    const double " <> #1 <>
    " = hepcat_spinas::checked_calchep_parameter(parameters[" <> ToString[First[#2] - 1] <> "], " <>
    spinasCppQuoted[config["ParameterNames"][[First[#2]]]] <> ");" &, aliases];
  strongIndex = FirstPosition[config["ParameterNames"], "GG", Missing["NotUsed"]];
  strongCode = If[MissingQ[strongIndex], "1.0", aliases[[First[strongIndex]]]];
  identity = config["LegOrder"] === Range[ir["LegCount"]];
  orderLines = If[identity, {}, {
    "    // CalcHEP expects incoming legs first; SPINAS keeps the exported leg labels.",
    "    const std::array<std::size_t, leg_count> external_order = " <>
      spinasCppArray[ToString /@ (config["LegOrder"] - 1)] <> ";"}];
  massCheck = "    hepcat_spinas::check_calchep_masses(spinas_process.masses(), calchep_process.masses()" <>
    If[identity, "", ", external_order"] <> ");";
  referencePoint = If[identity, "point", "hepcat_spinas::calchep_momenta_in_order(point, external_order)"];
  factorLines = If[config["SPINASNormalizationFactor"] === 1, {}, {
    "    // The legacy fixed-spin API omits the identical-final-state factor.",
    "    const ldouble final_state_factor = " <> spinasCppReal[config["SPINASNormalizationFactor"]] <> ";"}];
  amplitudeCode = "      return spinas_process.amp2()" <>
    If[factorLines === {}, "", " * final_state_factor"] <> ";";
  body = Join[{
    "#ifdef HEPCAT_HAS_CALCHEP", "  try {",
    "    // Work on a copy: the caller's momenta and caches stay unchanged.",
    "    " <> name <> " spinas_process(*this);", "",
    "    // Physical process: " <> StringReplace[config["ProcessDescription"],
      {"\n" -> " ", "\r" -> " ", "\\" -> "/"}],
    "    const std::string model_directory = " <> spinasCppQuoted[config["ModelDirectory"]] <> ";",
    "    const int model_number = " <> ToString[config["ModelNumber"]] <> ";",
    "    const std::vector<int> particle_pdgs = {" <>
      StringRiffle[ToString /@ config["PhysicalPDGs"], ", "] <> "};", "",
    "    // Match the current SPINAS parameters to the CalcHEP model by name.",
    "    const auto parameters = get_parameters();"}, parameterLines, {
    "    const std::vector<hepcat_spinas::CalcHEPParameter> model_parameters = {"},
    MapIndexed["      {" <> spinasCppQuoted[#1] <> ", " <> aliases[[First[#2]]] <> "}" <>
      If[First[#2] < Length[aliases], ",", ""] &, config["ParameterNames"]], {
    "    };", "",
    "    // Generate and compile the CalcHEP process in its private workspace.",
    "    hepcat_spinas::CalcHEPProcess calchep_process(",
    "      model_directory, model_number, particle_pdgs, model_parameters, " <> strongCode <> ");"},
    orderLines, {massCheck, ""}, factorLines, {
    "    // Each evaluator receives exactly the same physical phase-space point.",
    "    auto spinas_squared = [&](const Momenta& point) -> ldouble {",
    "      spinas_process.set_momenta(point);", amplitudeCode, "    };", "",
    "    auto calchep_squared = [&](const Momenta& point) -> ldouble {",
    "      return static_cast<ldouble>(calchep_process.squared_amplitude(",
    "        " <> referencePoint <> "));", "    };", "",
    "    // Both results include the initial average and final symmetry factor.",
    "    return hepcat_spinas::compare_squared_amplitudes(",
    "      spinas_process.masses(), incoming_legs(),",
    "      spinas_squared, calchep_squared, options, &std::cout);",
    "  } catch (const std::exception& error) {",
    "    return hepcat_spinas::failed_validation(options, error.what());", "  }", "#else",
    "  return hepcat_spinas::unavailable_validation(options,",
    "    \"CalcHEP support was not built. Configure CALCHEP_ROOT and rebuild this validator.\");",
    "#endif"}];
  StringRiffle[Join[start, body, {"}", "", "} // namespace hepcat_generated", ""}], "\n"]
];

spinasCalcHEPMain[name_String, config_Association] := StringRiffle[{
  "#include \"process.hpp\"", "#include <iostream>", "#include <stdexcept>", "",
  "int main(int argc, char** argv) {", "  try {",
  "    if (hepcat_spinas::validation_help_requested(argc, argv)) {",
  "      hepcat_spinas::print_validation_usage(std::cout);", "      return 0;", "    }", "",
  "    hepcat_spinas::ValidationOptions defaults;",
  "    defaults.points = " <> ToString[config["Points"]] <> ";",
  If[config["Seed"] === "Automatic",
    "    // ValidationOptions chooses a fresh seed from the time at startup.",
    "    defaults.seed = UINT64_C(" <> ToString[config["Seed"]] <> ");"],
  "    const auto options = hepcat_spinas::parse_validation_arguments(argc, argv, defaults);", "",
  "    hepcat_generated::" <> name <> " process;",
  "    const auto report = process.validate_calchep(options);",
  "    hepcat_spinas::print_validation_report(report, std::cout);",
  "    return report.passed ? 0 : 1;", "  } catch (const std::exception& error) {",
  "    std::cerr << \"Validation failed: \" << error.what() << '\\n';", "    return 2;", "  }", "}", ""}, "\n"];


spinasCalcHEPCMake[name_String, config_Association] := StringRiffle[{
  "", "# Optional independent CalcHEP validator. Ordinary SPINAS code has no CalcHEP dependency.",
  "set(CALCHEP_ROOT " <> spinasCppQuoted[config["CalcHEPDirectory"]] <> " CACHE PATH \"CalcHEP installation\")",
  "add_library(" <> name <> "_calchep_validation STATIC calchep-validation.cpp)",
  "target_link_libraries(" <> name <> "_calchep_validation PUBLIC " <> name <> "_process)",
  "add_executable(" <> name <> "_validate_calchep calchep-main.cpp)",
  "target_link_libraries(" <> name <> "_validate_calchep PRIVATE " <> name <> "_calchep_validation)",
  "if(UNIX AND " <> If[TrueQ[config["Supported"]], "TRUE", "FALSE"] <>
    " AND EXISTS \"${CALCHEP_ROOT}/lib/dynamic_me.a\")",
  "  enable_language(C)",
  "  target_compile_definitions(" <> name <> "_calchep_validation PRIVATE HEPCAT_HAS_CALCHEP)",
  "  target_include_directories(" <> name <> "_calchep_validation PRIVATE",
  "    \"${CALCHEP_ROOT}/include\" \"${CALCHEP_ROOT}/c_source/dynamicME/include\"",
  "    \"${CALCHEP_ROOT}/c_source/ntools/include\")",
  "  target_sources(" <> name <> "_calchep_validation PRIVATE \"${CALCHEP_ROOT}/include/VandPgate.c\")",
  "  target_link_libraries(" <> name <> "_calchep_validation PUBLIC",
  "    \"${CALCHEP_ROOT}/lib/dynamic_vp.a\" \"${CALCHEP_ROOT}/lib/dynamic_me.a\"",
  "    \"${CALCHEP_ROOT}/lib/num_c.a\" \"${CALCHEP_ROOT}/lib/servNoX11.a\" \"${CALCHEP_ROOT}/lib/serv.a\"",
  "    \"${CALCHEP_ROOT}/lib/sqme_aux.so\" \"${CALCHEP_ROOT}/lib/libSLHAplus.a\"",
  "    \"${CALCHEP_ROOT}/lib/dummy.a\" \"${CALCHEP_ROOT}/lib/ntools.a\"",
  "    ${CMAKE_DL_LIBS} m pthread)",
  "  set_target_properties(" <> name <> "_validate_calchep PROPERTIES BUILD_RPATH \"${CALCHEP_ROOT}/lib\")",
  "else()",
  "  message(STATUS \"CalcHEP validator will report unavailable; set CALCHEP_ROOT to enable the backend.\")",
  "endif()", ""}, "\n"];

spinasCalcHEPProject[dir_String, name_String, ir_Association, config_Association] := Module[{files},
  files = AssociationThread[{"ValidationRuntime", "CalcHEPRuntime", "CalcHEPSource", "CalcHEPMain"},
    FileNameJoin[{dir, #}] & /@ {"spinas-validation.hpp", "calchep-runtime.hpp", "calchep-validation.cpp", "calchep-main.cpp"}];
  <|"Files" -> files, "Contents" -> <|
    "ValidationRuntime" -> Import[FileNameJoin[{$spinasExportSourceDirectory, "spinas-validation.hpp"}], "Text"],
    "CalcHEPRuntime" -> Import[FileNameJoin[{$spinasExportSourceDirectory, "calchep-runtime.hpp"}], "Text"],
    "CalcHEPSource" -> spinasCalcHEPSource[name, ir, config],
    "CalcHEPMain" -> spinasCalcHEPMain[name, config]|>|>
];

spinasCalcHEPREADME[name_String, config_Association] := StringRiffle[{
  "", "## Independent CalcHEP validation", "",
  "The generated validate_calchep(options) method compares 1000 seeded random physical phase-space points by default. The same momentum values are sent to SPINAS and CalcHEP. The validator generates the CalcHEP process at runtime in a private temporary workspace and leaves the installation unchanged.", "",
  "CalcHEPSeed -> Automatic chooses a seed from the current time when the validation executable runs, not when its source is exported. The report prints the actual Seed for replay. Set CalcHEPSeed to an unsigned 64-bit integer to choose a fixed executable default, or use --seed to override it for one run. In C++, assign options.seed directly; zero is a valid explicit seed.", "",
  "```sh", "./build/" <> name <> "_validate_calchep", "./build/" <> name <> "_validate_calchep --points 1000 --seed 12345", "```", "",
  "Link the " <> name <> "_calchep_validation library when calling the method from your own C++ program. Configure -DCALCHEP_ROOT=/path/to/calchep if it was not found. This separate backend keeps the ordinary SPINAS process usable without CalcHEP.",
  "Spin/color averaging and the final-state identical-particle factor follow CalcHEP's documented squared-matrix-element convention. This implementation validates complete unpolarized colorless processes; fixed-helicity and color-stripped comparisons report unavailable. It does not guess a missing helicity or fit a normalization ratio.",
  "The default relative tolerance is 1e-7; the default absolute tolerance is zero, so tiny wrong amplitudes cannot pass through an absolute floor. Failed points and their full momenta are printed. This random diagnostic is not a uniformly weighted phase-space integration or an analytic equality proof.",
  "Model selection: " <> If[config["ModelNumber"] > 0,
    config["ModelDirectory"] <> ", model " <> ToString[config["ModelNumber"]], "not configured"] <> ".",
  If[config["Reason"] === "", "", "Configuration note: " <> config["Reason"]], ""}, "\n"];

(* ::Subsection::Closed:: *)
(*Native SPINAS Process Classes*)

(* Native cache planning. Loaded in ConstructiveDiagrams`Private`. *)

spinasNativeRelabel[value_, permutation_List] := value /. {
  spinor_Spinor :> Module[{items = List @@ spinor, position, label},
    position = First[FirstPosition[items, "Angle" | "Square"]];
    label = items[[position + 1]];
    items[[position + 1]] = permutation[[label]];
    items = items /. {
      spinasSpinIndex[i_Integer, slot_] :> spinasSpinIndex[permutation[[i]], slot],
      VectorSpinIndex[i_Integer, slot_] :> VectorSpinIndex[permutation[[i]], slot]};
    Spinor @@ items],
  Mom[i_Integer] :> Mom[permutation[[i]]],
  Mom[Multiparticle[items___Integer]] :> Mom[Multiparticle @@ permutation[[{items}]]],
  MomProd[a_, b_] :> MomProd[
    spinasNativeRelabelMomentumArgument[a, permutation], spinasNativeRelabelMomentumArgument[b, permutation]],
  Mandelstahm[items___Integer] :> Mandelstahm @@ permutation[[{items}]]};

spinasNativeRelabelMomentumArgument[value_Integer, permutation_List] := permutation[[value]];
spinasNativeRelabelMomentumArgument[Multiparticle[items___Integer], permutation_List] :=
  Multiparticle @@ permutation[[{items}]];
spinasNativeRelabelMomentumArgument[value_, permutation_List] := spinasNativeRelabel[value, permutation];

spinasNativeCppRelabel[text_String, permutation_List, ranks_List] := Module[
  {result = text, calls, labels, mapped, replacements, callRules, n = Length[permutation]},
  calls = DeleteDuplicates[StringCases[result,
    RegularExpression["sum_momenta\\(momenta, \\{[-0-9, ]*\\}\\)"]]];
  callRules = Table[
    labels = ToExpression /@ StringCases[call, RegularExpression["-?[0-9]+"]];
    mapped = (Sign[#] permutation[[Abs[#]]]) & /@ labels;
    call -> ("sum_momenta(momenta, {" <>
      StringRiffle[ToString /@ mapped, ", "] <> "})"), {call, calls}];
  result = StringReplace[result, callRules];
  replacements = Join[
    Table[("momenta[" <> ToString[i - 1] <> "]") ->
      ("momenta[" <> ToString[permutation[[i]] - 1] <> "]"), {i, n}],
    Table[("particles[" <> ToString[i - 1] <> "].") ->
      ("p" <> ToString[permutation[[i]]] <> "."), {i, n}],
    Flatten[Table[Table[
      ("spinor_spins[" <> ToString[i - 1] <> "][" <> ToString[slot - 1] <> "]") ->
        ("ds" <> ToString[permutation[[i]]] <>
          If[ranks[[permutation[[i]]]] > 1, "_" <> ToString[slot], ""]),
      {slot, ranks[[i]]}], {i, n}]]];
  StringReplace[result, replacements]
];

spinasNativeTokenReplace[text_String, rules_List] := StringReplace[text,
  (RegularExpression["\\b" <> First[#] <> "\\b"] -> Last[#]) & /@ rules];

spinasNativeUnitMomentum[value_] := Module[{form, momenta, coefficient},
  form = Expand[value /. Mom[Multiparticle[items___]] :> Total[Mom /@ {items}]];
  If[MatchQ[form, Mom[_Integer]], Return[{First[form], 1}]];
  If[Head[form] =!= Times, Return[$Failed]];
  momenta = Cases[List @@ form, Mom[_Integer]];
  If[Length[momenta] =!= 1, Return[$Failed]];
  coefficient = Cancel[form/First[momenta]];
  If[NumberQ[coefficient] && TrueQ[Im[coefficient] == 0],
    {First[First[momenta]], coefficient}, $Failed]
];

spinasNativeEndpoint[spinor_Spinor, rank_Integer] := Module[{items = List @@ spinor, position},
  position = First[FirstPosition[items, "Angle" | "Square"]];
  <|"Leg" -> items[[position + 1]], "Angle" -> (items[[position]] === "Angle"),
    "Upper" -> (position === 2 || items[[2]] === "Upper"), "Rank" -> rank|>
];

spinasNativeCachePlan[ir_Association, reservedNames_List : {}] := Module[
  {n = ir["LegCount"], ranks = ir["Spin2"], components, branches, family,
   used, allocate, products = {}, productKeys = {}, scalars = {}, scalarKeys = {},
   results = {}, component, permutation, scalarRules, chainRules, pending, actual,
   position, name, code, source, fallback, items, left, right, middle, scale,
   key, pointerArguments, spinArguments, slotCodes, nativeCall, descriptor,
   aliases, parameterRules, parameters, base, realQ, branchPieces, expressionCode,
   normalizeParameterText, mapPieces, declarations, initializers, updates, metadata},
  family = TrueQ[Lookup[ir, "Family", False]];
  components = If[family, ir["FamilyComponents"], {ir}];
  branches = If[family, ir["Branches"], {<|"Helicities" -> ir["Helicities"],
    "InputIndex" -> 1, "LegPermutation" -> Range[n], "FermionSign" -> 1|>}];
  used = Join[reservedNames, {"parameters", "external_masses", "momenta", "momenta_ready", "initialize_products",
    "amp", "spin_sum", "amp2", "component", "numerator", "denominator", "values", "spins",
    "set_parameters", "set_momenta", "set_masses", "get_parameters", "validate_calchep",
    "amplitude", "result", "validation", "other", "ranks", "originally_massless", "check_spin"},
    Table["p" <> ToString[i], {i, n}],
    Table["assignments" <> ToString[i], {i, n}], Table["indices" <> ToString[i], {i, n}],
    Flatten[Table[Join[{"ds" <> ToString[i]},
      Table["ds" <> ToString[i] <> "_" <> ToString[slot], {slot, ranks[[i]]}]], {i, n}]]];
  allocate[candidate_String] := Module[{stem = spinasReadableIdentifier[candidate], chosen, suffix = 1},
    chosen = stem;
    While[MemberQ[used, chosen], suffix++; chosen = stem <> "_" <> ToString[suffix]];
    AppendTo[used, chosen]; chosen];
  Do[
    component = components[[branch["InputIndex"]]];
    permutation = branch["LegPermutation"];
    scalarRules = {}; chainRules = {}; pending = {}; fallback = {};
    (* Allocate every scalar name before rewriting dependencies within their code. *)
    Do[
      actual = spinasNativeRelabel[component["ScalarExpressions"][[i]], permutation];
      position = FirstPosition[scalarKeys, actual, Missing["New"], {1}];
      If[MissingQ[position],
        name = allocate[spinasScalarName[actual]];
        AppendTo[scalarKeys, actual]; AppendTo[scalars,
          <|"Name" -> name, "Expression" -> actual, "Statement" -> "", "Comment" -> spinasReadableMath[actual]|>];
        AppendTo[pending, {Length[scalars], i}], name = scalars[[First[position]]]["Name"]];
      AppendTo[scalarRules, component["ScalarNames"][[i]] -> name],
      {i, Length[component["ScalarExpressions"]]}];
    Do[
      code = spinasNativeCppRelabel[component["ScalarStatements"][[item[[2]]]], permutation, ranks];
      code = spinasNativeTokenReplace[code, scalarRules];
      code = StringReplace[code, StartOfString ~~ "const cdouble " -> ""];
      scalars[[item[[1]]]] = Join[scalars[[item[[1]]]], <|"Statement" -> code|>], {item, pending}];
    Do[
      source = component["Chains"][[i]];
      actual = spinasNativeRelabel[source, permutation];
      items = List @@ actual;
      left = spinasNativeEndpoint[First[items], ranks[[rspSpinorLeg[First[items]]]]];
      right = spinasNativeEndpoint[Last[items], ranks[[rspSpinorLeg[Last[items]]]]];
      middle = spinasNativeUnitMomentum /@ Take[items, {2, -2}];
      If[Length[middle] <= 6 && FreeQ[middle, $Failed],
        scale = Times @@ (Last /@ middle);
        key = {left["Angle"], left["Leg"], left["Upper"],
          right["Leg"], right["Upper"], First /@ middle};
        position = FirstPosition[productKeys, key, Missing["New"], {1}];
        If[MissingQ[position],
          name = allocate[spinasChainName[SpinorChain @@ Join[
            {First[items]}, Mom /@ (First /@ middle), {Last[items]}]]];
          pointerArguments = Join[{If[left["Angle"], "::ANGLE", "::SQUARE"],
              "&p" <> ToString[left["Leg"]]}, If[left["Upper"], {}, {"false"}],
            ("&p" <> ToString[#]) & /@ (First /@ middle),
            {"&p" <> ToString[right["Leg"]]}, If[right["Upper"], {}, {"false"}]];
          descriptor = <|"Name" -> name, "Key" -> key, "Expression" -> actual,
            "Initialization" -> (name <> " = spinas::sproduct(" <> StringRiffle[pointerArguments, ", "] <> ");"),
            "Comment" -> spinasReadableMath[actual]|>;
          AppendTo[productKeys, key]; AppendTo[products, descriptor],
          name = products[[First[position]]]["Name"]];
        slotCodes = StringCases[component["ChainStatements"][[i]],
          "spinor_spins[" ~~ DigitCharacter .. ~~ "][" ~~ slot : (DigitCharacter ..) ~~ "]" :> ToExpression[slot] + 1];
        spinArguments = {};
        If[left["Rank"] > 0 && !TrueQ[ir["MassValues"][[left["Leg"]]] == 0],
          AppendTo[spinArguments, "ds" <> ToString[left["Leg"]] <>
            If[left["Rank"] > 1, "_" <> ToString[First[slotCodes]], ""]]; slotCodes = Rest[slotCodes]];
        If[right["Rank"] > 0 && !TrueQ[ir["MassValues"][[right["Leg"]]] == 0],
          AppendTo[spinArguments, "ds" <> ToString[right["Leg"]] <>
            If[right["Rank"] > 1, "_" <> ToString[First[slotCodes]], ""]]];
        nativeCall = name <> ".v(" <> StringRiffle[spinArguments, ", "] <> ")";
        If[scale =!= 1, nativeCall = "(" <> spinasCppNumber[scale] <> " * " <> nativeCall <> ")"],
        (* Native particle objects enforce their mass shell. Composite or very
           long insertions retain direct off-shell matrix multiplication. *)
        name = allocate[spinasChainName[actual] <> "_direct"];
        code = spinasNativeCppRelabel[component["ChainStatements"][[i]], permutation, ranks];
        code = spinasNativeTokenReplace[code, {component["ChainNames"][[i]] -> name}];
        AppendTo[fallback, code]; nativeCall = name];
      AppendTo[chainRules, component["ChainNames"][[i]] -> nativeCall],
      {i, Length[component["Chains"]]}];
    mapPieces[text_String] := spinasNativeTokenReplace[
      spinasNativeCppRelabel[text, permutation, ranks], Join[scalarRules, chainRules]];
    branchPieces = <|"NumeratorTerms" -> (mapPieces /@ component["ExpressionPieces"]["NumeratorTerms"]),
      "NumeratorSigns" -> component["ExpressionPieces"]["NumeratorSigns"],
      "Denominator" -> mapPieces[component["ExpressionPieces"]["Denominator"]],
      "UnitDenominator" -> component["ExpressionPieces"]["UnitDenominator"]|>;
    expressionCode = mapPieces[component["ExpressionCpp"]];
    AppendTo[results, Join[branch, <|"ExpressionPieces" -> branchPieces,
      "ExpressionCpp" -> expressionCode, "FallbackStatements" -> fallback|>]], {branch, branches}];
  aliases = Table[
    base = spinasReadableIdentifier[ir["ParameterNames"][[i]]];
    If[MemberQ[used, base], base = "parameter_" <> base];
    allocate[base], {i, Length[ir["ParameterNames"]]}];
  parameterRules = MapIndexed[("parameters[" <> ToString[First[#2] - 1] <> "]") -> #1 &, aliases];
  normalizeParameterText[text_String] := StringReplace[text, parameterRules];
  parameters = MapIndexed[Function[{alias, index},
    realQ = MatchQ[ir["ParameterSymbols"][[First[index]]], ModelParameter[_, _, "Real"]];
    <|"Alias" -> alias, "Index" -> (First[index] - 1), "RealQ" -> realQ,
      "OriginalName" -> ir["ParameterNames"][[First[index]]],
      "DefaultCpp" -> spinasCppComplex[ir["ParameterDefaults"][[First[index]]]]|>], aliases];
  results = Map[Function[entry, Join[entry, <|
    "ExpressionPieces" -> <|
      "NumeratorTerms" -> (normalizeParameterText /@ entry["ExpressionPieces"]["NumeratorTerms"]),
      "NumeratorSigns" -> entry["ExpressionPieces"]["NumeratorSigns"],
      "Denominator" -> normalizeParameterText[entry["ExpressionPieces"]["Denominator"]],
      "UnitDenominator" -> entry["ExpressionPieces"]["UnitDenominator"]|>,
    "ExpressionCpp" -> normalizeParameterText[entry["ExpressionCpp"]],
    "FallbackStatements" -> (normalizeParameterText /@ entry["FallbackStatements"])|>]], results];
  scalars = Map[Function[entry, Join[entry, <|
    "Statement" -> normalizeParameterText[entry["Statement"]]|>]], scalars];
  <|"Parameters" -> parameters, "ParameterRules" -> parameterRules,
    "MassCpp" -> (normalizeParameterText /@ ir["MassCpp"]), "Branches" -> results,
    "Products" -> products, "Scalars" -> scalars,
    "ProductDeclarations" -> (("spinas::sproduct " <> #["Name"] <> ";") & /@ products),
    "ProductInitializers" -> Lookup[products, "Initialization", {}],
    "ProductUpdates" -> ((#["Name"] <> ".update();") & /@ products),
    "ScalarDeclarations" -> (("cdouble " <> #["Name"] <> ";") & /@ scalars),
    "ScalarUpdates" -> Lookup[scalars, "Statement", {}],
    "AuxDeclarations" -> {}, "MomentumUpdates" -> {},
    "CacheDeclarations" -> Join[("spinas::sproduct " <> #["Name"] <> ";") & /@ products,
      ("cdouble " <> #["Name"] <> ";") & /@ scalars],
    "HasDirectFallback" -> AnyTrue[results, #["FallbackStatements"] =!= {} &]|>
];

(* Native SPINAS class templates. The cache plan is process-independent. *)
spinasNativeMassParameterIndices[ir_Association] := DeleteDuplicates[Cases[
  ir["Masses"], mass_ModelParameter :>
    With[{position = FirstPosition[ir["ParameterSymbols"], mass, Missing["NotExported"]]},
      If[MissingQ[position], Nothing, First[position]]]]];

spinasNativeParameterOrder[ir_Association] := With[{mass = spinasNativeMassParameterIndices[ir]},
  Join[Complement[Range[Length[ir["ParameterNames"]]], mass], mass]];

spinasNativeScalarArguments[n_Integer] := StringRiffle[
  Table["const int& ds" <> ToString[i], {i, n}], ", "];
spinasNativeScalarValues[n_Integer, prefix_String : "ds"] := StringRiffle[
  Table[prefix <> ToString[i], {i, n}], ", "];
spinasNativeMassArguments[ir_Association, plan_Association] := StringRiffle[
  ("const ldouble& value_" <> plan["Parameters"][[#, "Alias"]] &) /@
    spinasNativeMassParameterIndices[ir], ", "];
spinasNativeRealConstructorQ[ir_Association, plan_Association] :=
  Length[ir["ParameterDefaults"]] > 0 && AllTrue[plan["Parameters"], TrueQ[#["RealQ"]] &];

spinasNativeExpression[pieces_Association, sign_:1] := Module[{terms, numerator},
  terms = MapIndexed[
    If[First[#2] === 1,
      If[pieces["NumeratorSigns"][[First[#2]]] < 0, "-", ""],
      If[pieces["NumeratorSigns"][[First[#2]]] < 0, " - ", " + "]] <> #1 &,
    pieces["NumeratorTerms"]];
  numerator = StringJoin[terms];
  If[Length[terms] > 1, numerator = "(" <> numerator <> ")"];
  If[!TrueQ[pieces["UnitDenominator"]],
    numerator = "(" <> numerator <> ") / (" <> pieces["Denominator"] <> ")"];
  If[sign === -1, "-(" <> numerator <> ")", numerator]
];

spinasNativeProjectHeader[name_String, ir_Association, plan_Association] := Module[
  {n = ir["LegCount"], params = plan["Parameters"], realArguments, massArguments,
   pointerArguments, fields, declarations, lines, family, branchCount},
  family = TrueQ[Lookup[ir, "Family", False]];
  branchCount = Length[plan["Branches"]];
  realArguments = StringRiffle[("const ldouble& value_" <> params[[#, "Alias"]] &) /@
    spinasNativeParameterOrder[ir], ", "];
  massArguments = spinasNativeMassArguments[ir, plan];
  pointerArguments = StringRiffle[Table["const ldouble mom" <> ToString[i] <> "[4]", {i, n}], ", "];
  lines = {"#pragma once", "#include <array>", "#include <cstddef>",
    "#include \"spinas.h\"", "#include \"spinas-runtime.hpp\"", "#include \"spinas-validation.hpp\"", "",
    "namespace hepcat_generated {", "", "class " <> name <> " {", "public:",
    "  static constexpr std::size_t leg_count = " <> ToString[n] <> ";",
    "  static constexpr std::size_t parameter_count = " <> ToString[Length[params]] <> ";",
    "  static constexpr std::size_t helicity_count = " <> ToString[branchCount] <> ";",
    "  using Parameters = std::array<cdouble, parameter_count>;",
    "  using Momenta = hepcat_spinas::Momenta<leg_count>;",
    "  using Spins = std::array<int, leg_count>;", "",
    "  " <> name <> "();", "  explicit " <> name <> "(const Parameters& values);"};
  If[spinasNativeRealConstructorQ[ir, plan], lines = Join[lines,
    spinasReadableCppLines[name <> "(" <> realArguments <> ");"]]];
  lines = Join[lines, {"  " <> name <> "(const " <> name <> "& other);",
    "  " <> name <> "& operator=(const " <> name <> "& other);", "",
    "  static Parameters default_parameters();", "  static Spins default_spins();",
    "  static std::array<std::size_t, 2> incoming_legs();",
    "  static std::array<Spins, helicity_count> helicity_configurations();",
    "  static ldouble incoming_spin_average();", "  static ldouble symmetry_factor();", "",
    "  const Parameters& get_parameters() const { return parameters; }",
    "  const std::array<ldouble, leg_count>& masses() const { return external_masses; }", "",
    "  void set_parameters(const Parameters& values);"}];
  If[massArguments =!= "", lines = Join[lines,
    spinasReadableCppLines["void set_masses(" <> massArguments <> ");"]]];
  lines = Join[lines, {"  void set_momenta(const Momenta& values);"},
    spinasReadableCppLines["void set_momenta(" <> pointerArguments <> ");"], {
    "", "  // Complex amplitude for explicit doubled spins and helicities."},
    spinasReadableCppLines["cdouble amp(" <> spinasNativeScalarArguments[n] <> ");"], {
    "  cdouble amp(const Spins& spins);", "  ldouble spin_sum();", "  ldouble amp2();", "",
    "  hepcat_spinas::ValidationReport validate_calchep(",
    "    const hepcat_spinas::ValidationOptions& options = hepcat_spinas::ValidationOptions());", "",
    "private:", "  Parameters parameters;", "  std::array<ldouble, leg_count> external_masses;",
    "  Momenta momenta;", "  bool momenta_ready = false;", "",
    "  // Model parameters."},
    ("  cdouble " <> #["Alias"] <> ";" &) /@ params, {
    "", "  // External particles."},
    Table["  spinas::particle p" <> ToString[i] <> ";", {i, n}], {
    "", "  // Spinor-product and scalar caches. Updated once in set_momenta(...)."},
    ("  " <> # &) /@ Lookup[plan, "ProductDeclarations", {}],
    ("  " <> # &) /@ Lookup[plan, "ScalarDeclarations", {}], {
    "", "  void initialize_products();", "};", "", "} // namespace hepcat_generated", ""}];
  StringRiffle[lines, "\n"]
];

spinasNativeProjectSource[name_String, ir_Association, plan_Association] := Module[
  {n = ir["LegCount"], ranks = ir["Spin2"], masses = ir["MassValues"], params = plan["Parameters"],
   branches = plan["Branches"], lines = {}, add, definitions, massIndices, realOrder,
   realArguments, ctorValues, massArguments, pointerArguments, defaultSpins, incoming,
   defaults, originalMassless, massCpp, family, initialAverage, symmetryFactor,
   massLines, massChecks, index, higher, helicityLegs, condition, branch, expression,
   fallback, branchBody, indent, projectionLoops, normalization, states,
   spinLines, loops, loopLegs, prefix, configurations, coefficients},
  add[values_List] := (lines = Join[lines, values]);
  family = TrueQ[Lookup[ir, "Family", False]];
  massIndices = spinasNativeMassParameterIndices[ir]; realOrder = spinasNativeParameterOrder[ir];
  realArguments = StringRiffle[("const ldouble& value_" <> params[[#, "Alias"]] &) /@ realOrder, ", "];
  massArguments = spinasNativeMassArguments[ir, plan];
  pointerArguments = StringRiffle[Table["const ldouble mom" <> ToString[i] <> "[4]", {i, n}], ", "];
  ctorValues = spinasCppArray[Table["cdouble(value_" <> params[[i, "Alias"]] <> ")", {i, Length[params]}]];
  defaultSpins = spinasCppArray[ToString /@ MapThread[
    If[TrueQ[#1 == 0], #2, -#3] &, {masses, ir["Helicities"], ranks}]];
  incoming = spinasCppArray[ToString /@ (ir["IncomingLegs"] - 1)];
  defaults = MapIndexed["    " <> spinasCppComplex[#1] <>
    If[First[#2] < Length[params], ",", ""] <> " // " <>
    spinasCppQuoted[ir["ParameterNames"][[First[#2]]]] &, ir["ParameterDefaults"]];
  initialAverage = If[family, ir["InitialSpinAverage"],
    Times @@ (If[TrueQ[masses[[#]] == 0], 1, ranks[[#]] + 1] & /@ ir["IncomingLegs"])];
  symmetryFactor = If[family, ir["SymmetryFactor"], 1];
  massCpp = Lookup[plan, "MassCpp", ir["MassCpp"]];
  add[{"#include \"process.hpp\"", "#include <algorithm>", "#include <cmath>", "#include <stdexcept>", "",
    "namespace hepcat_generated {", "using namespace hepcat_spinas;", "",
    "constexpr std::size_t " <> name <> "::leg_count;",
    "constexpr std::size_t " <> name <> "::parameter_count;",
    "constexpr std::size_t " <> name <> "::helicity_count;", "",
    name <> "::" <> name <> "() : " <> name <> "(default_parameters()) {}", "",
    name <> "::" <> name <> "(const Parameters& values) {", "  set_parameters(values);", "}", ""}];
  If[spinasNativeRealConstructorQ[ir, plan],
    add[spinasReadableCppLines[name <> "::" <> name <> "(" <> realArguments <> ")", ""]];
    add[spinasReadableCppLines[": " <> name <> "(Parameters" <> ctorValues <> ") {}", "  "]]; add[{""}]];
  add[{"// Copies rebuild caches, so every sproduct points into its own process.",
    name <> "::" <> name <> "(const " <> name <> "& other) {",
    "  set_parameters(other.parameters);", "  if (other.momenta_ready) set_momenta(other.momenta);", "}", "",
    name <> "& " <> name <> "::operator=(const " <> name <> "& other) {",
    "  if (this != &other) {", "    set_parameters(other.parameters);",
    "    if (other.momenta_ready) set_momenta(other.momenta);", "  }", "  return *this;", "}", "",
    name <> "::Parameters " <> name <> "::default_parameters() {", "  return Parameters{{"}];
  add[defaults]; add[{"  }};", "}", "",
    name <> "::Spins " <> name <> "::default_spins() {", "  return Spins" <> defaultSpins <> ";", "}", "",
    "std::array<std::size_t, 2> " <> name <> "::incoming_legs() {", "  return " <> incoming <> ";", "}", "",
    "std::array<" <> name <> "::Spins, " <> name <> "::helicity_count>",
    name <> "::helicity_configurations() {", "  return std::array<Spins, helicity_count>{{"}];
  add[MapIndexed["    " <> spinasCppArray[ToString /@ #1["Helicities"]] <>
    If[First[#2] < Length[branches], ",", ""] &, branches]];
  add[{"  }};", "}", "",
    "ldouble " <> name <> "::incoming_spin_average() {", "  return " <> spinasCppReal[initialAverage] <> ";", "}", "",
    "ldouble " <> name <> "::symmetry_factor() {", "  return " <> spinasCppReal[symmetryFactor] <> ";", "}", "",
    "void " <> name <> "::initialize_products() {"}];
  add[("  " <> # &) /@ plan["ProductInitializers"]];
  add[{"}", "", "void " <> name <> "::set_parameters(const Parameters& values) {",
    "  for (const auto& value : values) {", "    if (!finite_complex(value)) {",
    "      throw std::invalid_argument(\"Model parameters must be finite.\");", "    }", "  }",
    "  momenta_ready = false;", "  parameters = values;"}];
  add[("  " <> #["Alias"] <> " = parameters[" <> ToString[#["Index"]] <> "];" &) /@ params];
  add[MapIndexed["  external_masses[" <> ToString[First[#2] - 1] <> "] = checked_mass(cdouble(" <> #1 <> "));" &, massCpp]];
  Do[
    If[ranks[[i]] > 0,
      add[{"  if ((external_masses[" <> ToString[i - 1] <> "] == 0) != " <>
        If[TrueQ[masses[[i]] == 0], "true", "false"] <> ") {",
        "    throw std::invalid_argument(",
        "      \"Changing between massive and massless spin states requires re-exporting the process.\");", "  }"}]];
    add[{"  p" <> ToString[i] <> " = spinas::particle(external_masses[" <> ToString[i - 1] <> "]);"}], {i, n}];
  add[{"  initialize_products();", "}", ""}];
  If[massArguments =!= "",
    add[spinasReadableCppLines["void " <> name <> "::set_masses(" <> massArguments <> ") {", ""]];
    add[{"  Parameters values = parameters;"}];
    add[("  values[" <> ToString[# - 1] <> "] = cdouble(value_" <> params[[#, "Alias"]] <> ");" &) /@ massIndices];
    add[{"  set_parameters(values);", "}", ""}]];
  add[{"void " <> name <> "::set_momenta(const Momenta& values) {",
    "  const auto validation = validate_momenta(values, external_masses, incoming_legs());",
    "  if (!validation.valid) throw std::invalid_argument(validation.message);",
    "  momenta_ready = false;", "  momenta = values;", "", "  // External particle momenta."}];
  add[Table["  p" <> ToString[i] <> ".set_momentum(momenta[" <> ToString[i - 1] <> "].data());", {i, n}]];
  add[{"", "  // Invalidate the cached spinor products after every momentum update."}];
  add[("  " <> # &) /@ plan["ProductUpdates"]];
  If[plan["ScalarUpdates"] =!= {}, add[{"", "  // Propagator denominators and scalar invariants."}];
    add[Flatten[spinasReadableCppLines[#] & /@ plan["ScalarUpdates"]]]];
  add[{"  momenta_ready = true;", "}", ""}];
  add[spinasReadableCppLines["void " <> name <> "::set_momenta(" <> pointerArguments <> ") {", ""]];
  add[{"  Momenta values;"}];
  add[Table["  std::copy(mom" <> ToString[i] <> ", mom" <> ToString[i] <> " + 4, values[" <> ToString[i - 1] <> "].begin());", {i, n}]];
  add[{"  set_momenta(values);", "}", "",
    "cdouble " <> name <> "::amp(const Spins& spins) {"}];
  add[spinasReadableCppLines["return amp(" <> StringRiffle[Table["spins[" <> ToString[i - 1] <> "]", {i, n}], ", "] <> ");"]];
  add[{"}", "", "// set_momenta(...) must be called before evaluating an amplitude."}];
  add[spinasReadableCppLines["cdouble " <> name <> "::amp(" <> spinasNativeScalarArguments[n] <> ") {", ""]];
  add[{"  if (!momenta_ready) throw std::logic_error(\"Call set_momenta before amp.\");"}];
  Do[If[TrueQ[masses[[i]] == 0] && ranks[[i]] > 0,
      add[{"  if (ds" <> ToString[i] <> " != " <> ToString[-ranks[[i]]] <> " && ds" <> ToString[i] <> " != " <>
        ToString[ranks[[i]]] <> ") throw std::invalid_argument(\"Invalid massless helicity.\");"}],
      add[{"  check_spin(" <> ToString[ranks[[i]]] <> ", ds" <> ToString[i] <> ");"}]], {i, n}];
  higher = Select[Range[n], ranks[[#]] > 1 && !TrueQ[masses[[#]] == 0] &];
  helicityLegs = Select[Range[n], ranks[[#]] > 0 && TrueQ[masses[[#]] == 0] &];
  normalization = If[higher === {}, "ldouble(1.0L)", StringRiffle[
    ("spinor_normalization(" <> ToString[ranks[[#]]] <> ", ds" <> ToString[#] <> ")") & /@ higher, " * "]];
  Do[
    condition = StringRiffle[("ds" <> ToString[#] <> If[branch["Helicities"][[#]] > 0, " > 0", " < 0"]) & /@ helicityLegs, " && "];
    If[condition =!= "", add[{"", "  " <> If[index === 1, "if", "else if"] <> " (" <> condition <> ") {"}]];
    indent = If[condition === "", "  ", "    "];
    expression = spinasNativeExpression[branch["ExpressionPieces"], Lookup[branch, "FermionSign", 1]];
    If[higher === {},
      add[Flatten[spinasReadableCppLines[#, indent] & /@ Lookup[branch, "FallbackStatements", {}]]];
      add[spinasReadableCppLines["return " <> expression <> ";", indent]],
      add[{indent <> "cdouble amplitude(0, 0);"}];
      Do[add[{indent <> "const auto assignments" <> ToString[i] <> " = spinor_assignments(" <>
        ToString[ranks[[i]]] <> ", ds" <> ToString[i] <> ");"}], {i, higher}];
      Do[add[{indent <> "for (const auto& indices" <> ToString[i] <> " : assignments" <> ToString[i] <> ") {"}];
        indent = indent <> "  ";
        add[Table[indent <> "const int ds" <> ToString[i] <> "_" <> ToString[j] <> " = indices" <>
          ToString[i] <> "[" <> ToString[j - 1] <> "];", {j, ranks[[i]]}]], {i, higher}];
      add[Flatten[spinasReadableCppLines[#, indent] & /@ Lookup[branch, "FallbackStatements", {}]]];
      add[spinasReadableCppLines["amplitude += " <> expression <> ";", indent]];
      Do[indent = StringDrop[indent, -2]; add[{indent <> "}"}], {Length[higher]}];
      add[spinasReadableCppLines["return (" <> normalization <> ") * amplitude;", indent]]];
    If[condition =!= "", add[{"  }"}]], {index, Length[branches]}, {branch, {branches[[index]]}}];
  If[helicityLegs =!= {}, add[{"  throw std::invalid_argument(\"This exported process does not contain the selected helicities.\");"}]];
  add[{"}", "", "// Explicit spin and helicity sums.", "ldouble " <> name <> "::spin_sum() {",
    "  ldouble result = 0;", "  cdouble amplitude;"}];
  states = If[family, ir["HelicityStates"], Table[If[TrueQ[masses[[i]] == 0], {ir["Helicities"][[i]]}, {0}], {i, n}]];
  loopLegs = Select[Range[n], ranks[[#]] > 0 && (!TrueQ[masses[[#]] == 0] || Length[states[[#]]] > 1) &];
  Do[If[!MemberQ[loopLegs, i], add[{"  const int j" <> ToString[i] <> " = " <>
      ToString[If[ranks[[i]] == 0, 0, First[states[[i]]]]] <> ";"}]], {i, n}];
  indent = "  ";
  Do[add[{indent <> "for (int j" <> ToString[i] <> " = " <> ToString[-ranks[[i]]] <> "; j" <> ToString[i] <>
      " <= " <> ToString[ranks[[i]]] <> "; j" <> ToString[i] <> " += " <>
      ToString[If[TrueQ[masses[[i]] == 0], 2 ranks[[i]], 2]] <> ") {"}];
    indent = indent <> "  ", {i, loopLegs}];
  add[spinasReadableCppLines["amplitude = amp(" <> spinasNativeScalarValues[n, "j"] <> ");", indent]];
  add[{indent <> "result += std::norm(amplitude);"}];
  Do[indent = StringDrop[indent, -2]; add[{indent <> "}"}], {Length[loopLegs]}];
  add[{"  return result;", "}", "", "ldouble " <> name <> "::amp2() {",
    "  // Initial-state average: 1/" <> ToString[initialAverage, InputForm] <> ".",
    "  // Final-state identical-particle factor: " <> ToString[symmetryFactor, InputForm] <> "."}];
  coefficients = Together[symmetryFactor/initialAverage];
  If[Numerator[coefficients] === 1,
    add[{"  return spin_sum() / ldouble(" <> ToString[Denominator[coefficients]] <> ".0L);"}],
    add[{"  return spin_sum() * " <> spinasCppReal[coefficients] <> ";"}]];
  add[{"}", "", "} // namespace hepcat_generated", ""}];
  StringRiffle[lines, "\n"]
];

(* Use direct native branches for ordinary families. Very large helicity
   inventories retain canonical dispatch to bound duplicated formula text.
   This is a representation choice, never a multiplicity restriction. *)
spinasNativeStyleQ[name_String, ir_Association] := Module[{components, branches, estimate},
  If[MemberQ[Table["p" <> ToString[i], {i, ir["LegCount"]}], name], Return[False]];
  If[!TrueQ[Lookup[ir, "Family", False]], Return[True]];
  components = ir["FamilyComponents"]; branches = Length[ir["Branches"]];
  estimate = Total[StringLength /@ Lookup[components, "ExpressionCpp"]] branches/Length[components];
  branches <= 64 && estimate <= 8000000
];

spinasProjectCode[name_String, ir_Association] := Module[{plan, family},
  family = TrueQ[Lookup[ir, "Family", False]];
  If[spinasNativeStyleQ[name, ir],
    plan = spinasNativeCachePlan[ir, {name}];
    <|"Header" -> spinasNativeProjectHeader[name, ir, plan],
      "Source" -> spinasNativeProjectSource[name, ir, plan],
      "Style" -> "NativeSPINAS", "ProductCacheCount" -> Length[plan["Products"]],
      "ScalarCacheCount" -> Length[plan["Scalars"]],
      "DirectMatrixFallback" -> plan["HasDirectFallback"]|>,
    <|"Header" -> If[family, spinasCanonicalFamilyProjectHeader[name, ir], spinasCanonicalProjectHeader[name, ir]],
      "Source" -> If[family, spinasCanonicalFamilyProjectSource[name, ir], spinasCanonicalProjectSource[name, ir]],
      "Style" -> "CanonicalDispatch", "ProductCacheCount" -> 0, "ScalarCacheCount" -> 0,
      "DirectMatrixFallback" -> True|>]
];

spinasProjectHeader[name_String, ir_Association] := spinasProjectCode[name, ir]["Header"];
spinasProjectSource[name_String, ir_Association] := spinasProjectCode[name, ir]["Source"];
spinasFamilyProjectHeader[name_String, ir_Association] := spinasProjectCode[name, ir]["Header"];
spinasFamilyProjectSource[name_String, ir_Association] := spinasProjectCode[name, ir]["Source"];

spinasCanonicalProjectHeader[name_String, ir_Association] := StringRiffle[{
  "#pragma once", "#include <array>", "#include <cstddef>", "#include <vector>",
  "#include \"spinas.h\"", "#include \"spinas-runtime.hpp\"", "#include \"spinas-validation.hpp\"", "",
  "namespace hepcat_generated {", "", "class " <> name <> " {", "public:",
  "  static constexpr std::size_t leg_count = " <> ToString[ir["LegCount"]] <> ";",
  "  static constexpr std::size_t parameter_count = " <> ToString[Length[ir["ParameterDefaults"]]] <> ";",
  "  using Parameters = std::array<cdouble, parameter_count>;",
  "  using Momenta = hepcat_spinas::Momenta<leg_count>;",
  "  using Spins = std::array<int, leg_count>;", "", "  " <> name <> "();",
  "  explicit " <> name <> "(const Parameters& values);",
  "  static Parameters default_parameters();", "  static Spins default_spins();",
  "  static std::array<std::size_t, 2> incoming_legs();", "",
  "  const Parameters& get_parameters() const { return parameters; }",
  "  const std::array<ldouble, leg_count>& masses() const { return external_masses; }",
  "", "  void set_parameters(const Parameters& values);",
  "  void set_momenta(const Momenta& values);", "",
  "  // Complex amplitude for explicit doubled spins and helicities.",
  "  cdouble amp(const Spins& spins);",
  "  ldouble spin_sum();", "  ldouble amp2();", "",
  "  // Link the generated CalcHEP validation library when calling this method.",
  "  hepcat_spinas::ValidationReport validate_calchep(",
  "    const hepcat_spinas::ValidationOptions& options = hepcat_spinas::ValidationOptions());",
  "", "private:",
  "  Parameters parameters;", "  std::array<ldouble, leg_count> external_masses;",
  "  Momenta momenta;", "  std::array<spinas::particle, leg_count> particles;",
  "  bool momenta_ready = false;", "",
  "  cdouble component(const std::vector<std::vector<int>>& spinor_spins);",
  "};", "", "} // namespace hepcat_generated", ""}, "\n"];

spinasCanonicalProjectSource[name_String, ir_Association] := Module[
  {n = ir["LegCount"], spin2 = ir["Spin2"], helicities = ir["Helicities"],
   massValues = ir["MassValues"], defaults, spinDefaults, incoming, massLines, componentLines},
  defaults = MapIndexed["    " <> spinasCppComplex[#1] <>
    If[First[#2] < Length[ir["ParameterDefaults"]], ",", ""] <>
    " // " <> spinasCppQuoted[ir["ParameterNames"][[First[#2]]]] &, ir["ParameterDefaults"]];
  spinDefaults = spinasCppArray[ToString /@ MapThread[
    If[TrueQ[#1 == 0], #2, -#3] &, {massValues, helicities, spin2}]];
  incoming = spinasCppArray[ToString /@ (ir["IncomingLegs"] - 1)];
  massLines = MapIndexed["  external_masses[" <> ToString[First[#2] - 1] <>
    "] = checked_mass(cdouble(" <> #1 <> "));" &, ir["MassCpp"]];
  componentLines = spinasReadableComponentBody[ir];
  StringRiffle[Join[{
    "#include \"process.hpp\"", "#include <cmath>", "#include <functional>",
    "#include <stdexcept>", "", "namespace hepcat_generated {",
    "using namespace hepcat_spinas;", "",
    "constexpr std::size_t " <> name <> "::leg_count;",
    "constexpr std::size_t " <> name <> "::parameter_count;", "",
    "// Model parameters and external states.",
    name <> "::" <> name <> "() : " <> name <> "(default_parameters()) {}", "",
    name <> "::" <> name <> "(const Parameters& values) {",
    "  set_parameters(values);", "}", "",
    name <> "::Parameters " <> name <> "::default_parameters() {",
    "  return Parameters{{"}, defaults, {
    "  }};", "}", "",
    name <> "::Spins " <> name <> "::default_spins() {",
    "  return Spins" <> spinDefaults <> ";", "}", "",
    "std::array<std::size_t, 2> " <> name <> "::incoming_legs() {",
    "  return " <> incoming <> ";", "}", "",
    "void " <> name <> "::set_parameters(const Parameters& values) {",
    "  momenta_ready = false;",
    "  for (const auto& value : values) {",
    "    if (!finite_complex(value)) {",
    "      throw std::invalid_argument(\"Model parameters must be finite.\");",
    "    }", "  }",
    "  parameters = values;", ""}, massLines, {"",
    "  static const std::array<int, leg_count> ranks = " <> spinasCppArray[ToString /@ spin2] <> ";",
    "  static const std::array<bool, leg_count> originally_massless = " <>
      spinasCppArray[If[TrueQ[#==0],"true","false"]& /@ massValues] <> ";",
    "  for (std::size_t i = 0; i < leg_count; ++i) {",
    "    if (ranks[i] > 0 && (external_masses[i] == 0) != originally_massless[i]) {",
    "      throw std::invalid_argument(",
    "        \"Changing between massive and massless spin states requires re-exporting the process.\");",
    "    }",
    "    particles[i] = spinas::particle(external_masses[i]);", "  }",
    "}", "", "void " <> name <> "::set_momenta(const Momenta& values) {",
    "  const auto validation = validate_momenta(values, external_masses, incoming_legs());",
    "  if (!validation.valid) {",
    "    throw std::invalid_argument(validation.message);", "  }",
    "  momenta_ready = false;",
    "  momenta = values;",
    "  for (std::size_t i = 0; i < leg_count; ++i) {",
    "    particles[i].set_momentum(momenta[i].data());", "  }",
    "  momenta_ready = true;", "}", "",
    "// Amplitude formula. Momenta have already been crossed during export.",
    "cdouble " <> name <> "::component(const std::vector<std::vector<int>>& spinor_spins) {"}, componentLines, {
    "}", "",
    "// Normalize and symmetrize massive external spin states.",
    "cdouble " <> name <> "::amp(const Spins& spins) {",
    "  if (!momenta_ready) {",
    "    throw std::logic_error(\"Call set_momenta before amp.\");", "  }",
    "  static const std::array<int, leg_count> ranks = " <> spinasCppArray[ToString /@ spin2] <> ";",
    "  static const std::array<int, leg_count> helicities = " <> spinasCppArray[ToString /@ helicities] <> ";", "",
    "  std::vector<std::vector<std::vector<int>>> assignments(leg_count);",
    "  std::vector<std::vector<int>> spinor_spins(leg_count);",
    "  ldouble normalization = 1;", "",
    "  for (std::size_t i = 0; i < leg_count; ++i) {",
    "    if (external_masses[i] == 0) {",
    "      if (spins[i] != helicities[i]) {",
    "        throw std::invalid_argument(\"This process has fixed massless helicities.\");", "      }",
    "      assignments[i] = spinor_assignments(0, 0);",
    "    } else {",
    "      assignments[i] = spinor_assignments(ranks[i], spins[i]);",
    "      normalization *= spinor_normalization(ranks[i], spins[i]);",
    "    }", "  }", "",
    "  std::function<cdouble(std::size_t)> sum = [&](std::size_t leg) -> cdouble {",
    "    if (leg == leg_count) {",
    "      return component(spinor_spins);", "    }",
    "    cdouble result(0, 0);",
    "    for (const auto& indices : assignments[leg]) {",
    "      spinor_spins[leg] = indices;",
    "      result += sum(leg + 1);", "    }",
    "    return result;", "  };", "",
    "  return normalization * sum(0);", "}", "",
    "// Sum |M|^2 over the massive spins at the fixed massless helicities.",
    "ldouble " <> name <> "::spin_sum() {",
    "  static const std::array<int, leg_count> ranks = " <> spinasCppArray[ToString /@ spin2] <> ";",
    "  Spins spins = default_spins();", "",
    "  std::function<ldouble(std::size_t)> sum = [&](std::size_t leg) -> ldouble {",
    "    if (leg == leg_count) {",
    "      return std::norm(amp(spins));", "    }",
    "    if (external_masses[leg] == 0 || ranks[leg] == 0) {",
    "      return sum(leg + 1);", "    }",
    "    ldouble result = 0;",
    "    for (int ds = -ranks[leg]; ds <= ranks[leg]; ds += 2) {",
    "      spins[leg] = ds;",
    "      result += sum(leg + 1);", "    }",
    "    return result;", "  };", "",
    "  return sum(0);", "}", "",
    "ldouble " <> name <> "::amp2() {",
    "  static const std::array<int, leg_count> ranks = " <> spinasCppArray[ToString /@ spin2] <> ";",
    "  ldouble average = 1;",
    "  for (const auto i : incoming_legs()) {",
    "    if (external_masses[i] != 0) {",
    "      average *= ranks[i] + 1;", "    }", "  }",
    "  return spin_sum() / average;", "}", "", "} // namespace hepcat_generated", ""}], "\n"]
];

spinasProjectMain[name_String] := StringRiffle[{
  "#include \"process.hpp\"", "#include <iomanip>", "#include <iostream>",
  "#include <stdexcept>", "", "int main() {", "  try {",
  "    using Process = hepcat_generated::" <> name <> ";",
  "    Process process;",
  "    const auto incoming = Process::incoming_legs();",
  "    const ldouble energy = hepcat_spinas::example_center_of_mass_energy(",
  "      process.masses(), incoming);", "",
  "    std::cout << std::setprecision(18);",
  "    std::cout << \"HEPCAT/SPINAS physical 2-to-N examples; fixed massless helicities.\\n\";",
  "    for (unsigned point = 0; point < 3; ++point) {",
  "      const auto momenta = hepcat_spinas::make_example_momenta(",
  "        process.masses(), incoming, energy, point);",
  "      const auto check = hepcat_spinas::validate_momenta(",
  "        momenta, process.masses(), incoming);",
  "      if (!check.valid) {",
  "        throw std::runtime_error(check.message);", "      }", "",
  "      process.set_momenta(momenta);",
  "      const auto spins = Process::default_spins();",
  "      const auto amplitude = process.amp(spins);",
  "      const auto averaged = process.amp2();",
  "      if (!hepcat_spinas::finite_complex(amplitude) || !std::isfinite(averaged)) {",
  "        throw std::runtime_error(\"An example amplitude is not finite.\");", "      }", "",
  "      std::cout << \"Point \" << point + 1 << \"; sqrt(s) = \" << energy << '\\n';",
  "      for (std::size_t i = 0; i < Process::leg_count; ++i) {",
  "        std::cout << \"  p\" << i + 1 << \" =\";",
  "        for (const auto x : momenta[i]) {",
  "          std::cout << ' ' << x;", "        }",
  "        std::cout << '\\n';", "      }",
  "      std::cout << \"  conservation/shell scaled error = \" << check.maximum_scaled_error << '\\n'",
  "        << \"  amplitude(default doubled spins) = \" << amplitude << '\\n'",
  "        << \"  fixed-helicity, initial-spin-averaged |M|^2 = \" << averaged << '\\n';",
  "    }", "    std::cout << \"No identical-particle phase-space factor is included.\\n\";",
  "    return 0;", "  } catch (const std::exception& error) {",
  "    std::cerr << \"Example failed: \" << error.what() << '\\n';",
  "    return 1;", "  }", "}", ""}, "\n"];

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
  "## Read the formulas", "",
  If[spinasNativeStyleQ[name, ir],
    "process.cpp uses persistent native particle and sproduct members, with bracket names such as a12a and s314a. set_momenta updates product caches, scalar invariants and propagators. amp has direct helicity branches using .v(...), and spin_sum uses explicit spin loops. Composite or long chains retain direct momentum matrices without an internal on-shell assumption.",
    "process.cpp names products such as a12a and s314a, with bracket comments. This project uses canonical formula dispatch to bound source size. Local products are evaluated per component and helicity-related formulas reuse signed leg maps."],
  "Model parameters have readable names and long formulas use numerator and denominator blocks. Massive tensor projections retain their normalization. Export formatting does not simplify or change the supplied amplitude. The manifest records the selected CodeStyle.", "",
  "## Build and run", "", "```sh", "cmake -S . -B build -DSPINAS_ROOT=/path/to/spinas",
  "cmake --build build --parallel", "./build/" <> name, "ctest --test-dir build --output-on-failure", "```", "",
  "This project uses the " <> precision <> " SPINAS ABI. It must match the library build; re-export with Precision if required.",
  "The existing SPINAS library is linked, not copied or rebuilt.", "",
  "## Use the process", "", "Include process.hpp and link the generated process library plus SPINAS.",
  "The class is hepcat_generated::" <> name <> ". Construct it with model defaults or a Parameters array.",
  If[spinasNativeStyleQ[name, ir],
    "A convenience constructor takes the couplings followed by direct external mass parameters. When external masses are direct model parameters, set_masses updates them. After any mass or parameter change, call set_momenta before evaluating the amplitude.",
    "After any parameter change, call set_momenta before evaluating the amplitude."],
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
DownValues[spinasProjectBuild] = {};
spinasProjectBuild[directory_String, name_String, root_String, runExamples_, cmakeOption_:Automatic,
    calchepRoot_:Automatic] := Module[
  {cmake, configure, build, run, executable, configureArguments},
  cmake = spinasProjectCMakeExecutable[cmakeOption];
  If[FailureQ[cmake], Return[cmake]];
  configureArguments = {cmake,"-S",directory,"-B",FileNameJoin[{directory,"build"}]};
  (* A resolved checkout supersedes any prior generated build's cached root,
     header and library paths when the project is regenerated. *)
  If[root =!= "", configureArguments = Join[configureArguments,
    {"-U", "SPINAS_INCLUDE_DIR", "-U", "SPINAS_LIBRARY", "-DSPINAS_ROOT=" <> root}]];
  If[StringQ[calchepRoot], configureArguments = Append[configureArguments, "-DCALCHEP_ROOT=" <> calchepRoot]];
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

spinasRunCalcHEPValidation[directory_String, name_String, config_Association] := Module[
  {command, run, reportedSeeds, actualSeed, failure},
  command = {FileNameJoin[{directory, "build", name <> "_validate_calchep" <>
      If[$OperatingSystem === "Windows", ".exe", ""]}],
    "--points", ToString[config["Points"]]};
  If[config["Seed"] =!= "Automatic",
    command = Join[command, {"--seed", ToString[config["Seed"]]}]];
  run = spinasProjectCommand[command];
  reportedSeeds = DeleteDuplicates[StringCases[Lookup[run, "StandardOutput", ""],
    "Seed: " ~~ digits : DigitCharacter .. :> FromDigits[digits]]];
  actualSeed = If[Length[reportedSeeds] === 1, First[reportedSeeds], Missing["SeedNotReported"]];
  If[Lookup[run, "ExitCode", -1] =!= 0 ||
      !StringContainsQ[Lookup[run, "StandardOutput", ""], "Amplitude validation: PASS"] ||
      !IntegerQ[actualSeed] || !(0 <= actualSeed <= 18446744073709551615) ||
      config["Seed"] =!= "Automatic" && actualSeed =!= config["Seed"],
    failure = <|"MessageTemplate" ->
      "The CalcHEP comparison did not pass. Inspect the report and first failing phase-space point.",
      "Directory" -> directory, "CommandResult" -> run|>;
    If[IntegerQ[actualSeed], AssociateTo[failure, "Seed" -> actualSeed]];
    Return[Failure["CalcHEPValidation", failure]]];
  Join[run, <|"Status" -> "PASS", "Points" -> config["Points"], "Seed" -> actualSeed|>]
];

(* ::Subsection::Closed:: *)
(*CalcHEP Numerical Integration With SPINAS Matrix Elements*)

spinasCalcHEPNumericalRoot[option_] := Module[{parents, candidates, valid},
  If[option === None, Return[""]];
  valid[path_] := DirectoryQ[path] && AllTrue[
    {"lib/n_calchep.o", "lib/num_c.a", "include/num_out.h", "include/VandP.h"},
    FileExistsQ[FileNameJoin[{path, #}]] &];
  parents = If[StringQ[option], {ExpandFileName[option]},
    Join[Select[Environment /@ {"CALCHEP_ROOT", "CALCHEP"}, StringQ[#] && # =!= "" &],
      {FileNameJoin[{ParentDirectory[ParentDirectory[$spinasExportSourceDirectory]], "CalcHEP"}],
       FileNameJoin[{$HomeDirectory, "code", "CalcHEP"}]}]];
  candidates = Flatten[Table[Join[{parent}, If[DirectoryQ[parent],
    Reverse[Sort[FileNames["calchep_*", parent]]], {}]], {parent, parents}]];
  SelectFirst[DeleteDuplicates[candidates], valid, ""]
];

(* Electric charge is metadata for CalcHEP's event interfaces, not a factor in
   the supplied matrix element. Unknown custom PDGs are reported explicitly. *)
spinasCalcHEPCharge3[pdg_Integer] := Module[{absolute = Abs[pdg], charge},
  charge = Which[MemberQ[{11, 13, 15}, absolute], -3,
    MemberQ[{12, 14, 16, 21, 22, 23, 25}, absolute], 0,
    MemberQ[{1, 3, 5}, absolute], -1, MemberQ[{2, 4, 6}, absolute], 2,
    absolute === 24, 3, True, Missing["UnknownElectricCharge"]];
  If[MissingQ[charge], charge, Sign[pdg] charge]
];

spinasCalcHEPNumericalConfiguration[ir_Association, data_List, supplied_Association] := Module[
  {tag = Unique["numericalCalcHEP"], fail, options, root, variables, functions,
   rows, declared, resolved, overrides, inputs, pending, sorted = {}, available,
   symbols, names, values, aliases,
   used = {"REAL", "COMPLEX", "CALCHEP", "maxNp", "DENOMINATOR_ERROR",
     "nTypePre", "nTypePost", "nTypeSSS", "nTypeSSS_", "log10",
     "Fabs", "Sqrt", "Pow", "Exp", "Log", "Sin", "Cos", "Tan",
     "Asin", "Acos", "Atan", "Atan2", "Sinh", "Cosh", "Tanh",
     "Asinh", "Acosh", "Atanh", "Creal", "Cimag", "Carg", "Cabs",
     "Conj", "Cproj", "Csqrt", "Cpow", "Clog", "Clog10", "Cexp",
     "Csin", "Ccos", "Ctan", "Casin", "Cacos", "Catan",
     "Csinh", "Ccosh", "Ctanh", "Cacosh", "Casinh", "Catanh",
     "M_E", "M_LOG2E", "M_LOG10E", "M_LN2", "M_LN10", "M_PI", "M_PI_2",
     "M_PI_4", "M_1_PI", "M_2_PI", "M_2_SQRTPI", "M_SQRT2", "M_SQRT1_2"},
   base, alias, suffix, position,
   emit, expression, evaluateLines = {}, scope, normalize, parameterFor,
   synthetic = {}, syntheticDefinitions = {}, particleRows, particles, charge,
   unknownCharges = {}, particle, massName, widthName, massIndex, widthIndex,
   process, incoming, outgoing, order, externalRows, physicalNames, pdgs, legs,
   processIndices, expressions, propagators, denominators = {}, momentum,
   coefficients, labels, opposite, physicalCoefficients, c, index, definition,
   selfConjugate, widthPolicies = {}, symmetry, factor = 1, family,
   omittedParameters = {}, omittedParticles = {}, finiteDefaultQ, metadataAvailableQ,
   modelScalarQ, required, allParticleRows},
  options = Join[Association[Options[WriteSPINASProcess]], supplied];
  root = spinasCalcHEPNumericalRoot[options["CalcHEPDirectory"]];
  fail[reason_String] := Throw[<|"Status" -> "UNAVAILABLE", "Supported" -> False,
    "Reason" -> reason, "CalcHEPDirectory" -> root|>, tag];
  Catch[
    If[ir["LegCount"] > 20,
      fail["This CalcHEP numerical interface supports at most 20 external particles; the ordinary SPINAS export remains available."]];
    variables = Select[data[[3]], ListQ[#] && Length[#] >= 2 && modelParameterQ[First[#]] &];
    functions = Select[data[[4]], ListQ[#] && Length[#] >= 2 && modelParameterQ[First[#]] &];
    declared = First /@ Join[variables, functions];
    If[!DuplicateFreeQ[declared] || !DuplicateFreeQ[ModelParameterName /@ declared],
      fail["Numerical CalcHEP export needs unique model parameter names."]];
    If[!AllTrue[declared, MatchQ[#, ModelParameter[_, _, "Real"]] &],
      fail["Numerical CalcHEP currently requires real model parameters."]];
    resolved = ModelParameterRules[data, options["ParameterRules"]];
    If[!ListQ[resolved], fail["The numerical model parameter defaults could not be resolved."]];
    overrides = modelParameterOverrideRules[Replace[options["ParameterRules"], Automatic -> {}], declared];
    If[!ListQ[overrides], fail["The numerical model parameter overrides are invalid."]];
    finiteDefaultQ[value_] := With[{number = Quiet[N[value /. resolved, 24]]},
      NumberQ[number] && TrueQ[Im[number] == 0] && FreeQ[number, Indeterminate | DirectedInfinity]];
    modelScalarQ[value_] := Which[
      NumberQ[value], TrueQ[Im[value] == 0], value === Pi || value === E, True,
      modelParameterQ[value], True,
      MemberQ[{Plus, Times, Power}, Head[value]], AllTrue[List @@ value, modelScalarQ],
      MemberQ[{Sin, Cos, Tan, ArcSin, ArcCos, ArcTan, Exp, Log, Sinh, Cosh, Tanh, Abs}, Head[value]] &&
        Length[value] === 1, modelScalarQ[First[value]], True, False];
    inputs = DeleteDuplicates[Join[First /@ variables,
      Intersection[First /@ functions, First /@ overrides]]];
    omittedParameters = Select[inputs, !finiteDefaultQ[#] &];
    inputs = Select[inputs, finiteDefaultQ];
    pending = Select[functions, !MemberQ[inputs, First[#]] &&
      finiteDefaultQ[First[#]] && modelScalarQ[#[[2]]] &];
    omittedParameters = Join[omittedParameters, Complement[First /@ functions, Join[inputs, First /@ pending]]];
    available = inputs;
    While[pending =!= {},
      rows = Select[pending, ContainsAll[available,
        DeleteDuplicates[Cases[#[[2]], _ModelParameter, {0, Infinity}]]] &];
      If[rows === {}, omittedParameters = Join[omittedParameters, First /@ pending]; Break[]];
      sorted = Join[sorted, rows]; available = Join[available, First /@ rows];
      pending = Select[pending, !MemberQ[First /@ rows, First[#]] &]];
    symbols = Join[inputs, First /@ sorted]; names = ModelParameterName /@ symbols;
    If[!ContainsAll[symbols, ir["ParameterSymbols"]],
      fail["A parameter needed by the SPINAS amplitude has an unresolved, cyclic or unsupported numerical dependency."]];
    If[!AllTrue[names, StringLength[#] <= 11 &&
        StringMatchQ[#, RegularExpression["[A-Za-z_][A-Za-z0-9_]*"]] &],
      fail["CalcHEP numerical parameter names must be identifiers of at most 11 characters."]];
    values = Quiet[N[symbols /. resolved, 24]];
    If[!AllTrue[values, NumberQ[#] && TrueQ[Im[#] == 0] &&
        FreeQ[#, Indeterminate | DirectedInfinity] &],
      fail["Every numerical model parameter needs a finite real default; unsupported external model functions must be resolved first."]];
    scope = If[declared === {}, "numerical", declared[[1, 2]]];
    normalize[value_] := Module[{parsed = value},
      If[StringQ[parsed],
        If[StringStartsQ[parsed, "!"], AppendTo[widthPolicies, parsed]; parsed = StringDrop[parsed, 1]];
        parsed = parseModelExpression[parsed, scope];
        parsed = parsed /. p : ModelParameter[name_String, _, _] :>
          With[{matching = Select[declared, ModelParameterName[#] === name &]},
            If[Length[matching] === 1, First[matching], p]]];
      If[FailureQ[parsed], fail["A numerical particle mass or width could not be parsed."]];
      parsed /. symbol_Symbol /; Context[Unevaluated[symbol]] === "Global`" :>
        With[{matching = Select[declared, ModelParameterName[#] === SymbolName[Unevaluated[symbol]] &]},
          If[Length[matching] === 1, First[matching], symbol]]];
    parameterFor[value_, label_String] := Module[{form = normalize[value], found, newName, short},
      If[TrueQ[form === 0], Return["0"]];
      found = FirstPosition[symbols, form, Missing["NotParameter"]];
      If[!MissingQ[found], Return[names[[First[found]]]]];
      found = FirstPosition[syntheticDefinitions, form, Missing["New"], {1}];
      If[!MissingQ[found], Return[synthetic[[First[found]]]]];
      short = StringTake[label, UpTo[8]]; newName = short; suffix = 1;
      While[MemberQ[Join[names, synthetic], newName], suffix++;
        newName = short <> ToString[suffix];
        If[StringLength[newName] > 11, fail["The numerical model exhausted its short synthetic parameter names."]]];
      AppendTo[synthetic, newName]; AppendTo[syntheticDefinitions, form]; newName];
    metadataAvailableQ[row_] := Module[{forms = normalize /@ row[[{6, 7}]]},
      AllTrue[forms, finiteDefaultQ[#] && ContainsAll[symbols,
        DeleteDuplicates[Cases[#, _ModelParameter, {0, Infinity}]]] &]];
    allParticleRows = Select[data[[1]], ListQ[#] && Length[#] >= 8 &];
    particleRows = Select[allParticleRows, metadataAvailableQ];
    omittedParticles = Complement[allParticleRows[[All, 2]], particleRows[[All, 2]]];
    process = If[TrueQ[Lookup[ir, "Family", False]], First[ir["InputProcesses"]], ir["Process"]];
    If[!AllTrue[process, Function[name, AnyTrue[particleRows, MemberQ[#[[{2, 3}]], name] &]]],
      fail["An external particle mass or width needs unresolved or unsupported numerical model information."]];
    particles = MapIndexed[Function[{row, ix},
      massName = parameterFor[row[[6]], "HepMass" <> ToString[First[ix]]];
      widthName = parameterFor[row[[7]], "HepWidth" <> ToString[First[ix]]];
      charge = spinasCalcHEPCharge3[row[[4]]];
      selfConjugate = spinasBareHelicityName[row[[2]]] === spinasBareHelicityName[row[[3]]];
      If[MissingQ[charge], AppendTo[unknownCharges, row[[2]]]];
      <|"Name" -> spinasBareHelicityName[row[[2]]],
        "AntiName" -> spinasBareHelicityName[row[[3]]], "PDG" -> row[[4]],
        "Spin2" -> row[[5]], "Color" -> row[[8]],
        "Charge3" -> If[MissingQ[charge], 0, charge], "ChargeKnown" -> !MissingQ[charge],
        "Mass" -> massName, "Width" -> widthName, "SelfConjugate" -> selfConjugate|>], particleRows];
    If[!AllTrue[Flatten[Lookup[particles, {"Name", "AntiName"}]],
        StringLength[#] > 0 && StringLength[#] <= 11 &&
          StringFreeQ[#, WhitespaceCharacter | "," | "{" | "}" | RegularExpression["[\\x00-\\x1f\\x7f]"]] &],
      fail["CalcHEP numerical particle names must be nonempty, at most 11 characters, without whitespace or separators."]];
    family = TrueQ[Lookup[ir, "Family", False]];
    process = If[family, First[ir["InputProcesses"]], ir["Process"]];
    incoming = ir["IncomingLegs"]; outgoing = Complement[Range[ir["LegCount"]], incoming];
    order = Join[incoming, outgoing];
    externalRows = Map[Function[name, SelectFirst[particleRows, MemberQ[#[[{2, 3}]], name] &]], process];
    physicalNames = MapIndexed[Function[{name, ix}, index = First[ix]; particle = externalRows[[index]];
      spinasBareHelicityName[If[MemberQ[outgoing, index],
        If[name === particle[[2]], particle[[3]], particle[[2]]], name]]], process];
    pdgs = MapIndexed[Function[{name, ix}, index = First[ix]; particle = externalRows[[index]];
      selfConjugate = spinasBareHelicityName[particle[[2]]] === spinasBareHelicityName[particle[[3]]];
      c = If[selfConjugate || name === particle[[2]], particle[[4]], -particle[[4]]];
      If[MemberQ[outgoing, index] && !selfConjugate, -c, c]], process];
    legs = Table[particle = externalRows[[i]];
      <|"Name" -> physicalNames[[i]], "PDG" -> pdgs[[i]], "Spin2" -> particle[[5]],
        "Color" -> particle[[8]], "Neutral" ->
          (spinasBareHelicityName[particle[[2]]] === spinasBareHelicityName[particle[[3]]]),
        "States" -> If[TrueQ[ir["MassValues"][[i]] == 0],
          If[family, Length[ir["HelicityStates"][[i]]], 1], particle[[5]] + 1]|>, {i, order}];
    If[TrueQ[Lookup[ir, "ColorStripped", False]] || AnyTrue[legs, #["Color"] =!= 1 &],
      fail["Numerical CalcHEP integration requires a complete color sum; color-stripped matrix elements are not supported."]];
    expressions = If[family, Lookup[ir["FamilyComponents"], "CrossedExpression"], {ir["CrossedExpression"]}];
    propagators = DeleteDuplicates[Cases[expressions, _PropDen, Infinity]];
    Do[
      momentum = First[propagator] /. Mom[Multiparticle[items___]] :> Total[Mom /@ {items}];
      coefficients = Table[Coefficient[Expand[momentum], Mom[i]], {i, ir["LegCount"]}];
      If[!TrueQ[Expand[momentum - coefficients.(Mom /@ Range[ir["LegCount"]])] === 0], Continue[]];
      physicalCoefficients = MapIndexed[#1 If[MemberQ[incoming, First[#2]], 1, -1] &, coefficients];
      labels = Select[Range[ir["LegCount"]], physicalCoefficients[[#]] === 1 &];
      opposite = Select[Range[ir["LegCount"]], physicalCoefficients[[#]] === -1 &];
      If[!AllTrue[physicalCoefficients, MemberQ[{-1, 0, 1}, #] &] || labels =!= {} && opposite =!= {}, Continue[]];
      If[labels === {}, labels = opposite];
      If[Length[labels] > ir["LegCount"]/2, labels = Complement[Range[ir["LegCount"]], labels]];
      If[Length[labels] < 2 || Length[labels] > ir["LegCount"] - 2, Continue[]];
      massName = parameterFor[propagator[[2]], "HpMass" <> ToString[Length[denominators] + 1]];
      widthName = parameterFor[If[Length[propagator] === 3, propagator[[3]], 0],
        "HpWidth" <> ToString[Length[denominators] + 1]];
      particle = SelectFirst[Range[Length[particles]], particles[[#, "Mass"]] === massName &, -1];
      AppendTo[denominators, <|"Legs" -> Sort[First[FirstPosition[order, #]] & /@ labels],
        "Mass" -> massName, "Width" -> widthName, "ParticleIndex" -> If[particle === -1, -1, particle - 1]|>],
      {propagator, propagators}];
    aliases = Table[base = spinasReadableIdentifier[name]; alias = base; suffix = 1;
      While[MemberQ[used, alias] || MemberQ[{"values", "evaluate_model"}, alias],
        suffix++; alias = base <> "_" <> ToString[suffix]];
      AppendTo[used, alias]; alias, {name, Join[names, synthetic]}];
    emit[value_] := Which[
      NumberQ[value] && TrueQ[Im[value] == 0], StringReplace[spinasCppReal[value], "ldouble(" -> "static_cast<REAL>("],
      value === Pi, "std::acos(static_cast<REAL>(-1))", value === E, "std::exp(static_cast<REAL>(1))",
      modelParameterQ[value], With[{found = FirstPosition[symbols, value, Missing["Undeclared"]]},
        If[MissingQ[found], fail["A numerical model constraint references an undeclared parameter."], aliases[[First[found]]]]],
      Head[value] === Plus, "(" <> StringRiffle[emit /@ List @@ value, " + "] <> ")",
      Head[value] === Times, "(" <> StringRiffle[emit /@ List @@ value, " * "] <> ")",
      Head[value] === Power && Last[value] === 1/2, "std::sqrt(" <> emit[First[value]] <> ")",
      Head[value] === Power, "std::pow(" <> emit[First[value]] <> ", " <> emit[Last[value]] <> ")",
      Head[value] === Abs, "std::abs(" <> emit[First[value]] <> ")",
      MemberQ[{Sin, Cos, Tan, ArcSin, ArcCos, ArcTan, Exp, Log, Sinh, Cosh, Tanh}, Head[value]] && Length[value] === 1,
        "std::" <> Switch[Head[value], Sin, "sin", Cos, "cos", Tan, "tan", ArcSin, "asin",
          ArcCos, "acos", ArcTan, "atan", Exp, "exp", Log, "log", Sinh, "sinh", Cosh, "cosh", Tanh, "tanh"] <>
          "(" <> emit[First[value]] <> ")",
      True, fail["A numerical model constraint contains an unsupported external function or complex operation."]];
    Do[AppendTo[evaluateLines, "  const REAL " <> aliases[[i]] <> " = values[" <> ToString[i - 1] <> "];"];
      AppendTo[evaluateLines, "  (void)" <> aliases[[i]] <> ";"], {i, Length[inputs]}];
    Do[index = Length[inputs] + j; expression = sorted[[j, 2]];
      AppendTo[evaluateLines, "  const REAL " <> aliases[[index]] <> " = " <> emit[expression] <> ";"];
      AppendTo[evaluateLines, "  values[" <> ToString[index - 1] <> "] = " <> aliases[[index]] <> ";"], {j, Length[sorted]}];
    Do[index = Length[symbols] + j;
      AppendTo[evaluateLines, "  const REAL " <> aliases[[index]] <> " = " <> emit[syntheticDefinitions[[j]]] <> ";"];
      AppendTo[evaluateLines, "  values[" <> ToString[index - 1] <> "] = " <> aliases[[index]] <> ";"], {j, Length[synthetic]}];
    values = Join[values, Quiet[N[syntheticDefinitions /. resolved, 24]]];
    If[!AllTrue[values, NumberQ[#] && TrueQ[Im[#] == 0] && FreeQ[#, Indeterminate | DirectedInfinity] &],
      fail["A numerical particle mass, width or propagator parameter has no finite real default."]];
    names = Join[names, synthetic];
    processIndices = Table[position = FirstPosition[symbols, parameter, Missing["Absent"]];
      If[MissingQ[position], fail["A SPINAS parameter is missing from the numerical model."]]; First[position] - 1,
      {parameter, ir["ParameterSymbols"]}];
    If[!family,
      symmetry = 1/Times @@ (Factorial[Length[#]] & /@ Gather[pdgs[[outgoing]]]); factor = symmetry];
    <|"Status" -> If[root === "", "UNAVAILABLE", "AVAILABLE"], "Supported" -> True,
      "Reason" -> If[root === "", "CalcHEP numerical libraries were not found; set CalcHEPDirectory or CALCHEP_ROOT when building.", ""],
      "CalcHEPDirectory" -> root, "IndependentCount" -> Length[inputs],
      "ParameterNames" -> names, "ParameterDefaults" -> Re[values],
      "EvaluateLines" -> evaluateLines, "Particles" -> particles, "Legs" -> legs,
      "LegOrder" -> order, "ProcessParameterIndices" -> processIndices,
      "Denominators" -> DeleteDuplicates[denominators], "SPINASNormalizationFactor" -> factor,
      "UnknownElectricCharges" -> DeleteDuplicates[unknownCharges],
      "FixedModelWidths" -> DeleteDuplicates[widthPolicies],
      "OmittedUnsupportedParameters" -> (ModelParameterName /@ DeleteDuplicates[omittedParameters]),
      "OmittedParticleMetadata" -> omittedParticles,
      "RunningStrongParameter" -> Replace[FirstPosition[ModelParameterName /@ inputs, "GG", Missing["Absent"]],
        {Missing[__] -> -1, {i_Integer} :> i - 1}],
      "SelectedHelicities" -> If[family, Lookup[ir, "FixedHelicities", {}],
        (Rule[#, ir["Helicities"][[#]]] & /@ Select[Range[ir["LegCount"]],
          TrueQ[ir["MassValues"][[#]] == 0] && ir["Spin2"][[#]] > 0 &])],
      "SymbolicGeneration" -> False, "Backend" -> "SPINAS", "SessionDirectory" -> "calchep"|>, tag]
];

spinasCalcHEPNumericalSource[name_String, config_Association] := Module[
  {lines, add, real, boolean, massIndex, list, parameters, particles, legs, denominators},
  If[!TrueQ[config["Supported"]], Return["// CalcHEP numerical integration is unavailable.\n// " <>
    StringReplace[config["Reason"], {"\n" -> " ", "\r" -> " ", "\\" -> "/"}] <> "\n"]];
  real[value_] := StringReplace[spinasCppReal[value], "ldouble(" -> "static_cast<REAL>("];
  boolean[value_] := If[TrueQ[value], "true", "false"];
  massIndex[value_String] := If[value === "0", -1, First[FirstPosition[config["ParameterNames"], value]] - 1];
  list[values_List] := "{" <> StringRiffle[values, ", "] <> "}";
  parameters = MapThread["    {" <> spinasCppQuoted[#1] <> ", " <> real[#2] <> "}" &,
    {config["ParameterNames"], config["ParameterDefaults"]}];
  particles = Map[Function[row, "    " <> list[{
    spinasCppQuoted[row["Name"]], spinasCppQuoted[row["AntiName"]], ToString[row["PDG"]],
    ToString[row["Spin2"]], ToString[row["Color"]], ToString[row["Charge3"]],
    spinasCppQuoted[row["Mass"]], spinasCppQuoted[row["Width"]], boolean[row["SelfConjugate"]]}]], config["Particles"]];
  legs = Map[Function[row, "    " <> list[{spinasCppQuoted[row["Name"]], ToString[row["PDG"]],
    ToString[row["Spin2"]], ToString[row["Color"]], boolean[row["Neutral"]], ToString[row["States"]]}]], config["Legs"]];
  denominators = Map[Function[row, "    " <> list[{
    "\"" <> StringJoin[("\\" <> IntegerString[#, 8, 3]) & /@ row["Legs"]] <> "\"",
    ToString[massIndex[row["Mass"]]], ToString[massIndex[row["Width"]]], ToString[row["ParticleIndex"]]}]], config["Denominators"]];
  lines = {}; add[text_List] := (lines = Join[lines, text]);
  add[{"// CalcHEP supplies the numerical menus, VEGAS, cuts and histograms.",
    "// Every matrix element comes from the generated SPINAS class below.",
    "// No CalcHEP symbolic process generation is used by this executable.",
    "#include \"process.hpp\"", "#include \"calchep-spinas-runtime.hpp\"", "",
    "#ifndef HEPCAT_CALCHEP_ROOT", "#define HEPCAT_CALCHEP_ROOT " <> spinasCppQuoted[config["CalcHEPDirectory"]],
    "#endif", "", "namespace {", "", "using Process = hepcat_generated::" <> name <> ";", "",
    "// Independent values are changed by CalcHEP's parameter menu or set_param.",
    "// Recompute every dependent value before updating particle masses/caches.",
    "int evaluate_model(REAL* values) {"}];
  add[config["EvaluateLines"]];
  add[{"  return 0;", "}", "", "hepcat_calchep::Configuration numerical_configuration() {",
    "  hepcat_calchep::Configuration config;", "  config.calchep_directory = HEPCAT_CALCHEP_ROOT;",
    "  config.independent_count = " <> ToString[config["IndependentCount"]] <> ";",
    "  config.parameters = {", StringRiffle[parameters, ",\n"], "  };", "",
    "  // Complete model particle metadata, with explicit fixed mass/width inputs.",
    "  config.particles = {", StringRiffle[particles, ",\n"], "  };", "",
    "  // Physical incoming particles first, followed by outgoing particles.",
    "  config.legs = {", StringRiffle[legs, ",\n"], "  };",
    "  config.external_order = " <> list[ToString /@ (config["LegOrder"] - 1)] <> ";",
    "  config.process_parameter_indices = " <> list[ToString /@ config["ProcessParameterIndices"]] <> ";", "",
    "  // Physical propagators available to CalcHEP's numerical mapping tools.",
    "  config.denominators = {", StringRiffle[denominators, ",\n"], "  };",
    "  config.amplitude_factor = " <> real[config["SPINASNormalizationFactor"]] <> ";",
    "  config.running_strong_parameter = " <> ToString[config["RunningStrongParameter"]] <> ";",
    "  config.evaluate = &evaluate_model;", "  return config;", "}", "",
    "hepcat_calchep::NumericalProcess<Process> numerical_process(numerical_configuration());", "",
    "} // namespace", "",
    "// CalcHEP's existing numerical executable links against these model hooks.",
    "extern \"C\" {", "",
    "int nModelParticles = numerical_process.particle_count();",
    "ModelPrtclsStr* ModelPrtcls = numerical_process.model_particles();",
    "int nModelVars = numerical_process.independent_count();",
    "int nModelFunc = numerical_process.dependent_count();",
    "int current_variable = 0;", "int* currentVarPtr = &current_variable;",
    "char** varNames = numerical_process.model_names();",
    "REAL* varValues = numerical_process.model_values();", "",
    "int calcMainFunc() { return numerical_process.calculate_model(); }", "",
    "CalcHEP_interface interface_ext = numerical_process.process_interface();",
    "CalcHEP_interface* PtrInterface_ext = &interface_ext;", "",
    "int findval(char* name, double* value) {",
    "  if (name == nullptr || value == nullptr) return 1;",
    "  for (int i = 0; i < numerical_process.model_count(); ++i) {",
    "    if (std::strcmp(name, varNames[i]) == 0) {",
    "      *value = static_cast<double>(varValues[i]);", "      return 0;", "    }", "  }",
    "  return 1;", "}", "",
    "int qnumbers(char* name, int* spin2, int* charge3, int* color) {",
    "  return numerical_process.quantum_numbers(name, spin2, charge3, color);", "}", "",
    "double usrFF_(int incoming, int outgoing, double* momenta, char** names, int* pdgs) {",
    "  return usrFF(incoming, outgoing, momenta, names, pdgs);", "}", "",
    "double usrfun_(char* name, int incoming, int outgoing, double* momenta, char** names, int* pdgs) {",
    "  return usrfun(name, incoming, outgoing, momenta, names, pdgs);", "}", "",
    "} // extern C", ""}];
  StringRiffle[lines, "\n"]
];

spinasCalcHEPNumericalCMake[name_String, config_Association] := StringRiffle[{
  "", "# CalcHEP numerical integration with the SPINAS matrix element.",
  "# This target neither runs s_calchep nor compiles a symbolic CalcHEP process.",
  "if(NOT CALCHEP_ROOT)", "  set(CALCHEP_ROOT " <> spinasCppQuoted[config["CalcHEPDirectory"]] <>
    " CACHE PATH \"CalcHEP numerical installation\" FORCE)", "endif()",
  "if(UNIX AND " <> If[TrueQ[config["Supported"]], "TRUE", "FALSE"] <>
    " AND EXISTS \"${CALCHEP_ROOT}/lib/n_calchep.o\" AND EXISTS \"${CALCHEP_ROOT}/lib/num_c.a\")",
  "  add_executable(n_calchep calchep-spinas.cpp \"${CALCHEP_ROOT}/lib/n_calchep.o\")",
  "  target_include_directories(n_calchep PRIVATE \"${CALCHEP_ROOT}/include\")",
  "  target_compile_definitions(n_calchep PRIVATE HEPCAT_CALCHEP_ROOT=\"${CALCHEP_ROOT}\")",
  "  find_library(CALCHEP_X11_LIBRARY NAMES X11 HINTS /opt/X11/lib /usr/X11R6/lib)",
  "  target_link_libraries(n_calchep PRIVATE " <> name <> "_process",
  "    \"${CALCHEP_ROOT}/lib/num_c.a\" \"${CALCHEP_ROOT}/lib/sqme_aux.so\"",
  "    \"${CALCHEP_ROOT}/lib/lhapdf.so\" \"${CALCHEP_ROOT}/lib/dynamic_me.a\")",
  "  if(CALCHEP_X11_LIBRARY)",
  "    target_link_libraries(n_calchep PRIVATE \"${CALCHEP_ROOT}/lib/serv.a\" \"${CALCHEP_X11_LIBRARY}\")",
  "  else()",
  "    target_link_libraries(n_calchep PRIVATE \"${CALCHEP_ROOT}/lib/servNoX11.a\" \"${CALCHEP_ROOT}/lib/serv.a\")",
  "    message(STATUS \"CalcHEP numerical backend: headless menus and batch integration; X11 was not found.\")",
  "  endif()",
  "  target_link_libraries(n_calchep PRIVATE \"${CALCHEP_ROOT}/lib/libSLHAplus.a\"",
  "    \"${CALCHEP_ROOT}/lib/dummy.a\" \"${CALCHEP_ROOT}/lib/ntools.a\" ${CMAKE_DL_LIBS} m pthread)",
  "  set_target_properties(n_calchep PROPERTIES BUILD_RPATH \"${CALCHEP_ROOT}/lib\")",
  "  set(CALCHEP_SESSION \"${CMAKE_CURRENT_SOURCE_DIR}/calchep\")",
  "  file(MAKE_DIRECTORY \"${CALCHEP_SESSION}/aux/models\" \"${CALCHEP_SESSION}/aux/tmp\"",
  "    \"${CALCHEP_SESSION}/aux/results\" \"${CALCHEP_SESSION}/aux/so_generated\")",
  "  add_custom_command(TARGET n_calchep POST_BUILD",
  "    COMMAND ${CMAKE_COMMAND} -E remove -f \"${CALCHEP_SESSION}/n_calchep\"",
  "    COMMAND ${CMAKE_COMMAND} -E create_symlink \"$<TARGET_FILE:n_calchep>\" \"${CALCHEP_SESSION}/n_calchep\")",
  "else()",
  "  message(STATUS \"CalcHEP numerical integration is unavailable; ordinary SPINAS targets remain usable.\")",
  "endif()", ""}, "\n"];

spinasCalcHEPNumericalProject[dir_String, name_String, config_Association] := <|
  "Files" -> <|"CalcHEPNumericalSource" -> FileNameJoin[{dir, "calchep-spinas.cpp"}],
    "CalcHEPNumericalRuntime" -> FileNameJoin[{dir, "calchep-spinas-runtime.hpp"}]|>,
  "Contents" -> <|"CalcHEPNumericalSource" -> spinasCalcHEPNumericalSource[name, config],
    "CalcHEPNumericalRuntime" -> Import[FileNameJoin[{$spinasExportSourceDirectory,
      "calchep-spinas-runtime.hpp"}], "Text"]|>|>;

spinasCalcHEPNumericalPublicConfiguration[config_Association] := Join[
  KeyDrop[config, {"EvaluateLines", "SelectedHelicities", "ParameterDefaults", "SPINASNormalizationFactor"}],
  If[KeyExistsQ[config, "ParameterDefaults"], <|"ParameterDefaults" -> N[config["ParameterDefaults"]],
    "SPINASNormalizationFactor" -> N[config["SPINASNormalizationFactor"]],
    "SelectedHelicities" -> Association[(ToString[First[#]] -> Last[#]) & /@
      Lookup[config, "SelectedHelicities", {}]]|>, <||>]];

spinasCalcHEPNumericalREADME[config_Association] := StringRiffle[{
  "", "## CalcHEP numerical integration using the SPINAS amplitude", "",
  "When CalcHEP's numerical libraries are installed, the normal build also creates `build/n_calchep`. Start it in its private session directory:",
  "", "```sh", "cd calchep", "./n_calchep", "```", "",
  "The executable uses CalcHEP's numerical interface, phase-space mappings, VEGAS integration, cuts, histograms and event tools. Every matrix element is evaluated by this project's SPINAS class. It never calls the CalcHEP symbolic generator and does not need an equivalent CalcHEPModel. It leaves the installed CalcHEP files unchanged.",
  "Independent inputs appear in CalcHEP's parameter menu. Dependent model constraints are recalculated in dependency order, then the generated process updates masses and invalidates its momentum/product caches. An explicit ParameterRules override of a dependent parameter makes that value an independent input for this export; its downstream constraints continue to update normally.",
  "Masses, couplings and explicit widths are taken from the exported HEPCAT model. CalcHEP automatic width calculations and gauge-restoring width switches are not provided by the SPINAS backend; unsupported switches are rejected. Changing an external spinning particle between massive and massless requires re-exporting its spin-state representation.",
  "The supplied amp2 normalization is retained: incoming-state averaging and final identical-particle factors are applied exactly once. A selected massless helicity remains a labeled contribution, not an unpolarized result. CalcHEP's adjustable incoming-polarization controls are not advertised for this backend.",
  If[Lookup[config, "SelectedHelicities", {}] === {},
    "This export includes the requested complete massless-helicity family.",
    "Selected doubled helicities (original external leg -> helicity): " <>
      ToString[Lookup[config, "SelectedHelicities", {}], InputForm] <> "."],
  If[Lookup[config, "UnknownElectricCharges", {}] === {}, "",
    "Electric-charge metadata are unavailable for these custom species: " <>
      StringRiffle[config["UnknownElectricCharges"], ", "] <>
      ". Their integration is supported, but charge-dependent event metadata should not be treated as complete."],
  If[Lookup[config, "FixedModelWidths", {}] === {}, "",
    "Model widths marked automatic in their source tables are used as explicit fixed model inputs here; this backend does not calculate decay widths."],
  If[Lookup[config, "OmittedUnsupportedParameters", {}] === {} &&
      Lookup[config, "OmittedParticleMetadata", {}] === {}, "",
    "Unrelated unresolved model constraints or particle metadata were omitted from the numerical menus. The manifest lists them under OmittedUnsupportedParameters and OmittedParticleMetadata. Every dependency used by the supplied amplitude and its external particles remains required."],
  If[TrueQ[config["Supported"]], "", "Numerical backend unavailable for this export: " <> config["Reason"]],
  "Without numerical CalcHEP libraries this target is omitted; the ordinary SPINAS process and example executable still build. A later CMake configuration can select an installation with `-DCALCHEP_ROOT=/path/to/calchep`.", ""}, "\n"];

WriteSPINASProcess[directory_String, process : {___String}, amplitude_, model_, opts:OptionsPattern[]] := Module[
  {options = Association[{opts}], name = OptionValue["ProcessName"], root, precision, ir,
   dir = ExpandFileName[directory], data, runtime, files, contents, written, result, build,
   overwrite = OptionValue["Overwrite"], examples = OptionValue["RunExamples"],
   details = OptionValue["ReturnDetails"], buildRequested = OptionValue["Build"],
   cmake = OptionValue["CMakeExecutable"], fixed, manifest, fullOptions, calchep, validationProject,
   numerical, numericalProject, code},
  fullOptions = Join[Association[Options[WriteSPINASProcess]], Association[Reverse[{opts}]]];
  If[Complement[Keys[options],First /@ Options[WriteSPINASProcess]] =!= {} ||
      !StringQ[name] || !TrueQ[spinasCppIdentifierQ[name]] || StringTrim[directory] === "" || !AllTrue[{overwrite,examples,details,buildRequested},MemberQ[{True,False},#]&] ||
      !MemberQ[{Automatic,"LongDouble","Double"},OptionValue["Precision"]] ||
      !(OptionValue["SPINASDirectory"]===Automatic || StringQ[OptionValue["SPINASDirectory"]]) ||
      !(cmake === Automatic || (StringQ[cmake] && StringTrim[cmake] =!= "")) ||
      !spinasCalcHEPOptionsQ[fullOptions],
    Message[WriteSPINASProcess::args]; Return[$Failed]];
  data = If[Head[model] === FeynmanModel,List @@ model,model];
  If[!ListQ[data],Return[Failure["InvalidModel",<||>]]];
  ir = spinasProcessIR[process,amplitude,data,<|"IncomingLegs"->OptionValue["IncomingLegs"],
    "ParameterRules"->OptionValue["ParameterRules"]|>];
  If[FailureQ[ir],Return[ir]];
  calchep = spinasCalcHEPConfiguration[ir, data, fullOptions];
  numerical = spinasCalcHEPNumericalConfiguration[ir, data, fullOptions];
  If[TrueQ[fullOptions["RunCalcHEPValidation"]] && calchep["Status"] =!= "AVAILABLE",
    Return[Failure["CalcHEPUnavailable", <|"MessageTemplate" -> calchep["Reason"]|>]]];
  fixed = spinasFixedHelicities[OptionValue["FixedHelicities"], ir["Spin2"],
    TrueQ[# == 0] & /@ ir["MassValues"]];
  If[FailureQ[fixed], Return[fixed]];
  If[AnyTrue[Normal[fixed], ir["Helicities"][[First[#]]] =!= Last[#] &],
    Return[Failure["SPINASHelicities", <|"MessageTemplate" ->
      "FixedHelicities conflicts with the supplied fixed-helicity amplitude."|>]]];
  If[ir["LegCount"] < 4,Return[Failure["SPINASPhaseSpace",<|
    "MessageTemplate"->"The runnable project requires two incoming and at least two outgoing particles."|>]]];
  If[TrueQ[buildRequested] || TrueQ[examples] || TrueQ[fullOptions["RunCalcHEPValidation"]],
    cmake = spinasProjectCMakeExecutable[cmake]; If[FailureQ[cmake], Return[cmake]]];
  runtime = FileNameJoin[{$spinasExportSourceDirectory,"spinas-runtime.hpp"}];
  If[!FileExistsQ[runtime],Return[Failure["SPINASRuntime",<|"MessageTemplate"->"The SPINAS runtime template is missing."|>]]];
  root = spinasProjectRoot[OptionValue["SPINASDirectory"]];
  precision = spinasProjectPrecision[root,OptionValue["Precision"]];
  files = AssociationThread[{"Header","Source","Demo","Runtime","CMake","README","Manifest"},
    FileNameJoin[{dir,#}]& /@ {"process.hpp","process.cpp","main.cpp","spinas-runtime.hpp","CMakeLists.txt","README.md","manifest.json"}];
  validationProject = spinasCalcHEPProject[dir, name, ir, calchep];
  numericalProject = spinasCalcHEPNumericalProject[dir, name, numerical];
  files = Join[files, validationProject["Files"], numericalProject["Files"]];
  If[!TrueQ[overwrite] && AnyTrue[Values[files],FileExistsQ],
    Return[Failure["SPINASFilesExist",<|"MessageTemplate"->"Generated project files already exist. Use Overwrite -> True to replace them.","Directory"->dir|>]]];
  If[FileExistsQ[dir] && !DirectoryQ[dir],Return[Failure["SPINASDirectory",<|"Directory"->dir|>]]];
  code = spinasProjectCode[name, ir];
  manifest = <|"Generator"->"HEPCAT WriteSPINASProcess", "Process"->process,
    "IncomingLegs"->ir["IncomingLegs"], "Class"->name, "Precision"->precision,
    "ColorStripped"->ir["ColorStripped"],
    "ParameterOrder"->ir["ParameterNames"],
    "ParameterDefaults"->(<|"Real"->N[Re[#]],"Imaginary"->N[Im[#]]|>& /@ ir["ParameterDefaults"]),
    "Masses"->N[ir["MassValues"]], "Spin2"->ir["Spin2"], "Helicities"->ir["Helicities"],
    "CalcHEPValidation" -> Join[calchep, <|"SPINASNormalizationFactor" -> N[calchep["SPINASNormalizationFactor"]]|>],
    "CalcHEPNumerical" -> spinasCalcHEPNumericalPublicConfiguration[numerical],
    "CodeStyle" -> code["Style"]|>;
  contents = <|"Header"->code["Header"], "Source"->code["Source"],
    "Demo"->spinasProjectMain[name], "Runtime"->Import[runtime,"Text"],
    "CMake"->spinasProjectCMake[name,root,precision] <> spinasCalcHEPCMake[name,calchep] <>
      spinasCalcHEPNumericalCMake[name, numerical],
    "README"->spinasProjectREADME[name,ir,precision] <> spinasCalcHEPREADME[name,calchep] <>
      spinasCalcHEPNumericalREADME[numerical]|>;
  contents = Join[contents, validationProject["Contents"], numericalProject["Contents"]];
  If[!AllTrue[Values[contents],StringQ],Return[Failure["SPINASGeneration",<|"MessageTemplate"->"The C++ project could not be generated."|>]]];
  written = Quiet[Check[
    If[!DirectoryQ[dir],CreateDirectory[dir,CreateIntermediateDirectories->True]];
    AllTrue[KeyValueMap[Export[files[#1],#2,"Text"]&,contents],StringQ] &&
      StringQ[Export[files["Manifest"],manifest,"RawJSON"]],False]];
  If[!TrueQ[written],Return[Failure["SPINASWrite",<|"MessageTemplate"->"The output files could not all be written.","Directory"->dir|>]]];
  result = <|"Directory"->dir,"Files"->files,"Process"->process,"IncomingLegs"->ir["IncomingLegs"],
    "Build"-><|"Status"->"NOT_REQUESTED"|>,"Examples"-><|"Status"->"NOT_REQUESTED"|>,
    "CalcHEPValidation" -> <|"Status" -> "NOT_REQUESTED", "Configuration" -> calchep|>,
    "CalcHEPNumerical" -> Join[spinasCalcHEPNumericalPublicConfiguration[numerical],
      <|"Executable" -> FileNameJoin[{dir, "build", "n_calchep"}],
        "SessionDirectory" -> FileNameJoin[{dir, "calchep"}]|>],
    "CodeStyle" -> code["Style"]|>;
  If[TrueQ[buildRequested] || TrueQ[examples] || TrueQ[fullOptions["RunCalcHEPValidation"]],
    build = spinasProjectBuild[dir,name,root,examples,cmake,calchep["CalcHEPDirectory"]]; If[FailureQ[build],Return[build]];
    result = Join[result,build]];
  If[TrueQ[fullOptions["RunCalcHEPValidation"]],
    build = spinasRunCalcHEPValidation[dir,name,calchep]; If[FailureQ[build],Return[build]];
    result = Join[result,<|"CalcHEPValidation" -> build|>]];
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
      "ExpressionPieces" -> (component["ExpressionPieces"] /.
        text_String :> rewriteCpp[text, component["ParameterSymbols"]]),
      "ChainStatements" -> (rewriteCpp[#, component["ParameterSymbols"]] & /@ component["ChainStatements"]),
      "ScalarStatements" -> (rewriteCpp[#, component["ParameterSymbols"]] & /@ component["ScalarStatements"]),
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

spinasCanonicalFamilyProjectHeader[name_String, ir_Association] := Module[{header, scalarArguments, scalarValues, declarations},
  header = spinasCanonicalProjectHeader[name, ir];
  scalarArguments = StringRiffle[Table["int ds" <> ToString[i], {i, ir["LegCount"]}], ", "];
  scalarValues = spinasCppArray[Table["ds" <> ToString[i], {i, ir["LegCount"]}]];
  declarations = StringRiffle[Map[Function[component, StringRiffle[{
    "  cdouble " <> spinasComponentName[component] <> "(",
    "    const std::vector<std::vector<int>>& spinor_spins,",
    "    const ParticleView& particle_view, const Momenta& momenta);"}, "\n"]],
    ir["FamilyComponents"]], "\n\n"];
  StringReplace[header, {
    "  using Spins = std::array<int, leg_count>;" ->
      "  using Spins = std::array<int, leg_count>;\n\n  static constexpr std::size_t helicity_count = " <>
        ToString[Length[ir["HelicityConfigurations"]]] <> ";\n" <>
        "  static std::array<Spins, helicity_count> helicity_configurations();\n" <>
        "  static ldouble incoming_spin_average();\n  static ldouble symmetry_factor();",
    "  cdouble amp(const Spins& spins);" -> "  cdouble amp(const Spins& spins);",
    "  ldouble spin_sum();" -> "  cdouble amp(" <> scalarArguments <> ") {\n" <>
      "    return amp(Spins" <> scalarValues <> ");\n  }\n\n  ldouble spin_sum();",
    "  cdouble component(const std::vector<std::vector<int>>& spinor_spins);" ->
      "  using ParticleView = std::array<spinas::particle*, leg_count>;\n\n" <>
      "  cdouble component(\n    std::size_t source, const std::vector<std::vector<int>>& spinor_spins,\n" <>
      "    const ParticleView& particle_view, const Momenta& momenta);\n\n" <> declarations}]
];

spinasCanonicalFamilyProjectSource[name_String, ir_Association] := Module[
  {n = ir["LegCount"], ranks = ir["Spin2"], components = ir["FamilyComponents"],
   branches = ir["Branches"], prefix, componentCode, configurationRows,
   permutationRows, sourceIndices, signs, rankArray, dispatchCases, helicityLabel},
  prefix = First[StringSplit[spinasCanonicalProjectSource[name, ir], "cdouble " <> name <> "::component(", 2]];
  prefix = StringReplace[prefix, "constexpr std::size_t " <> name <> "::parameter_count;" ->
    "constexpr std::size_t " <> name <> "::parameter_count;\nconstexpr std::size_t " <> name <> "::helicity_count;"];
  componentCode = StringRiffle[Map[Function[component, StringRiffle[Join[{
      "cdouble " <> name <> "::" <> spinasComponentName[component] <> "(",
      "  const std::vector<std::vector<int>>& spinor_spins,",
      "  const ParticleView& particle_view, const Momenta& momenta) {"},
      spinasReadableComponentBody[component, True], {"}", ""}], "\n"]], components], "\n"];
  dispatchCases = Flatten[Table[{
    "    case " <> ToString[i - 1] <> ":",
    "      return " <> spinasComponentName[components[[i]]] <>
      "(spinor_spins, particle_view, momenta);"}, {i, Length[components]}]];
  helicityLabel[configuration_List] := "doubled helicities: " <>
    StringRiffle[ToString /@ configuration, ", "];
  configurationRows = MapIndexed["    " <> spinasCppArray[ToString /@ #1] <>
    If[First[#2] < Length[branches], ",", ""] <> " // " <> helicityLabel[#1] &,
    ir["HelicityConfigurations"]];
  permutationRows = MapIndexed["    " <> spinasCppArray[ToString /@ (#1["LegPermutation"] - 1)] <>
    If[First[#2] < Length[branches], ",", ""] <> " // " <> helicityLabel[#1["Helicities"]] &,
    branches];
  sourceIndices = spinasCppArray[ToString /@ (Lookup[branches, "InputIndex"] - 1)];
  signs = spinasCppArray[ToString /@ Lookup[branches, "FermionSign"]];
  rankArray = spinasCppArray[ToString /@ ranks];
  prefix <> componentCode <> StringRiffle[Join[{
    "// Reuse each canonical formula for its identical-particle-related helicities.",
    "cdouble " <> name <> "::component(",
    "  std::size_t source, const std::vector<std::vector<int>>& spinor_spins,",
    "  const ParticleView& particle_view, const Momenta& momenta) {",
    "  switch (source) {"}, dispatchCases, {
    "    default:", "      throw std::logic_error(\"Unknown generated helicity component.\");",
    "  }", "}", "",
    "std::array<" <> name <> "::Spins, " <> name <> "::helicity_count>",
    name <> "::helicity_configurations() {",
    "  // Massless doubled helicities; massive spins are supplied to amp(...).",
    "  return std::array<Spins, helicity_count>{{"}, configurationRows, {
    "  }};", "}", "",
    "ldouble " <> name <> "::incoming_spin_average() {",
    "  return " <> spinasCppReal[ir["InitialSpinAverage"]] <> ";", "}", "",
    "ldouble " <> name <> "::symmetry_factor() {",
    "  return " <> spinasCppReal[ir["SymmetryFactor"]] <> ";", "}", "",
    "// Select a helicity branch, then project the massive spin tensors.",
    "cdouble " <> name <> "::amp(const Spins& spins) {",
    "  if (!momenta_ready) {",
    "    throw std::logic_error(\"Call set_momenta before amp.\");", "  }",
    "  static const std::array<int, leg_count> ranks = " <> rankArray <> ";", "",
    "  // Canonical formula leg -> physical process leg (zero-based indices).",
    "  static const std::array<std::array<std::size_t, leg_count>, helicity_count> permutations = {{"},
    permutationRows, {
    "  }};",
    Sequence @@ spinasReadableCppLines[
      "static const std::array<std::size_t, helicity_count> sources = " <> sourceIndices <> ";"],
    Sequence @@ spinasReadableCppLines[
      "static const std::array<int, helicity_count> signs = " <> signs <> ";"], "",
    "  const auto configurations = helicity_configurations();",
    "  std::size_t branch = helicity_count;",
    "  for (std::size_t candidate = 0; candidate < helicity_count; ++candidate) {",
    "    bool match = true;",
    "    for (std::size_t i = 0; i < leg_count; ++i) {",
    "      if (external_masses[i] == 0 && spins[i] != configurations[candidate][i]) {",
    "        match = false;", "      }", "    }",
    "    if (match) {", "      branch = candidate;", "      break;", "    }", "  }",
    "  if (branch == helicity_count) {",
    "    throw std::invalid_argument(",
    "      \"No supplied or identical-particle-related amplitude has these massless helicities.\");", "  }", "",
    "  ParticleView particle_view;",
    "  Momenta selected_momenta;",
    "  for (std::size_t i = 0; i < leg_count; ++i) {",
    "    particle_view[i] = &particles[permutations[branch][i]];",
    "    selected_momenta[i] = momenta[permutations[branch][i]];", "  }", "",
    "  std::vector<std::vector<std::vector<int>>> assignments(leg_count);",
    "  std::vector<std::vector<int>> spinor_spins(leg_count);",
    "  ldouble normalization = 1;", "",
    "  for (std::size_t i = 0; i < leg_count; ++i) {",
    "    if (external_masses[i] == 0) {",
    "      assignments[i] = spinor_assignments(0, 0);",
    "    } else {",
    "      assignments[i] = spinor_assignments(ranks[i], spins[i]);",
    "      normalization *= spinor_normalization(ranks[i], spins[i]);", "    }", "  }", "",
    "  std::function<cdouble(std::size_t)> sum = [&](std::size_t leg) -> cdouble {",
    "    if (leg == leg_count) {",
    "      return component(sources[branch], spinor_spins, particle_view, selected_momenta);", "    }",
    "    cdouble result(0, 0);",
    "    for (const auto& indices : assignments[leg]) {",
    "      spinor_spins[leg] = indices;",
    "      result += sum(leg + 1);", "    }",
    "    return result;", "  };", "",
    "  return ldouble(signs[branch]) * normalization * sum(0);", "}", "",
    "// Sum all requested massless helicities and massive external spins.",
    "ldouble " <> name <> "::spin_sum() {",
    "  static const std::array<int, leg_count> ranks = " <> rankArray <> ";",
    "  ldouble result = 0;", "",
    "  for (const auto& helicities : helicity_configurations()) {",
    "    Spins spins = default_spins();",
    "    for (std::size_t i = 0; i < leg_count; ++i) {",
    "      if (external_masses[i] == 0) {",
    "        spins[i] = helicities[i];", "      }", "    }", "",
    "    std::function<ldouble(std::size_t)> sum = [&](std::size_t leg) -> ldouble {",
    "      if (leg == leg_count) {",
    "        return std::norm(amp(spins));", "      }",
    "      if (external_masses[leg] == 0 || ranks[leg] == 0) {",
    "        return sum(leg + 1);", "      }",
    "      ldouble value = 0;",
    "      for (int spin = -ranks[leg]; spin <= ranks[leg]; spin += 2) {",
    "        spins[leg] = spin;",
    "        value += sum(leg + 1);", "      }",
    "      return value;", "    };", "",
    "    result += sum(0);", "  }",
    "  return result;", "}", "",
    "// Initial-state average and final-state identical-particle phase-space factor.",
    "ldouble " <> name <> "::amp2() {",
    "  return spin_sum() * symmetry_factor() / incoming_spin_average();", "}", "",
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
   files, contents, written, result, build, cmake, manifest, calchep, validationProject,
   numerical, numericalProject, code},
  name = options["ProcessName"];
  cmake = options["CMakeExecutable"];
  If[Complement[First /@ Flatten[opts], First /@ Options[WriteSPINASProcess]] =!= {} ||
      !StringQ[name] || !TrueQ[spinasCppIdentifierQ[name]] || StringTrim[directory] === "" ||
      !AllTrue[Lookup[options, {"Overwrite", "RunExamples", "ReturnDetails", "Build"}], MemberQ[{True, False}, #] &] ||
      !MemberQ[{Automatic, "LongDouble", "Double"}, options["Precision"]] ||
      !(options["SPINASDirectory"] === Automatic || StringQ[options["SPINASDirectory"]]) ||
      !(cmake === Automatic || (StringQ[cmake] && StringTrim[cmake] =!= "")) ||
      !spinasCalcHEPOptionsQ[options],
    Message[WriteSPINASProcess::args]; Return[$Failed]];
  data = If[Head[model] === FeynmanModel, List @@ model, model];
  If[!ListQ[data], Return[Failure["InvalidModel", <||>]]];
  ir = spinasFamilyIR[entries, data,
    KeyTake[options, {"IncomingLegs", "ParameterRules", "FixedHelicities"}], physicalSpec];
  If[FailureQ[ir], Return[ir]];
  calchep = spinasCalcHEPConfiguration[ir, data, options];
  numerical = spinasCalcHEPNumericalConfiguration[ir, data, options];
  If[TrueQ[options["RunCalcHEPValidation"]] && calchep["Status"] =!= "AVAILABLE",
    Return[Failure["CalcHEPUnavailable", <|"MessageTemplate" -> calchep["Reason"]|>]]];
  If[ir["LegCount"] < 4, Return[Failure["SPINASPhaseSpace", <|
    "MessageTemplate" -> "The runnable project requires two incoming and at least two outgoing particles."|>]]];
  If[TrueQ[options["Build"]] || TrueQ[options["RunExamples"]] || TrueQ[options["RunCalcHEPValidation"]],
    cmake = spinasProjectCMakeExecutable[cmake]; If[FailureQ[cmake], Return[cmake]]];
  runtime = FileNameJoin[{$spinasExportSourceDirectory, "spinas-runtime.hpp"}];
  If[!FileExistsQ[runtime], Return[Failure["SPINASRuntime", <|"MessageTemplate" -> "The SPINAS runtime template is missing."|>]]];
  root = spinasProjectRoot[options["SPINASDirectory"]];
  precision = spinasProjectPrecision[root, options["Precision"]];
  files = AssociationThread[{"Header", "Source", "Demo", "Runtime", "CMake", "README", "Manifest"},
    FileNameJoin[{dir, #}] & /@ {"process.hpp", "process.cpp", "main.cpp", "spinas-runtime.hpp", "CMakeLists.txt", "README.md", "manifest.json"}];
  validationProject = spinasCalcHEPProject[dir, name, ir, calchep];
  numericalProject = spinasCalcHEPNumericalProject[dir, name, numerical];
  files = Join[files, validationProject["Files"], numericalProject["Files"]];
  If[!TrueQ[options["Overwrite"]] && AnyTrue[Values[files], FileExistsQ],
    Return[Failure["SPINASFilesExist", <|"MessageTemplate" -> "Generated project files already exist. Use Overwrite -> True to replace them.", "Directory" -> dir|>]]];
  If[FileExistsQ[dir] && !DirectoryQ[dir], Return[Failure["SPINASDirectory", <|"Directory" -> dir|>]]];
  code = spinasProjectCode[name, ir];
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
    "SpinSumConvention" -> "All external massive spins and selected massless helicities; amp2 averages only unfixed incoming states and includes the outgoing identical-species factorial factor; selected outgoing helicities are labeled contributions; no color sum.",
    "CalcHEPValidation" -> Join[calchep, <|"SPINASNormalizationFactor" -> N[calchep["SPINASNormalizationFactor"]]|>],
    "CalcHEPNumerical" -> spinasCalcHEPNumericalPublicConfiguration[numerical],
    "CodeStyle" -> code["Style"]|>;
  If[AssociationQ[ir["PhysicalProcess"]], manifest = Join[manifest,
    KeyTake[ir, {"PhysicalProcess", "ProcessDescription"}]]];
  contents = <|"Header" -> code["Header"],
    "Source" -> code["Source"], "Demo" -> spinasFamilyProjectMain[name],
    "Runtime" -> Import[runtime, "Text"],
    "CMake" -> spinasProjectCMake[name, root, precision] <> spinasCalcHEPCMake[name, calchep] <>
      spinasCalcHEPNumericalCMake[name, numerical],
    "README" -> spinasFamilyProjectREADME[name, ir, precision] <> spinasCalcHEPREADME[name, calchep] <>
      spinasCalcHEPNumericalREADME[numerical]|>;
  contents = Join[contents, validationProject["Contents"], numericalProject["Contents"]];
  If[!AllTrue[Values[contents], StringQ], Return[Failure["SPINASGeneration", <|"MessageTemplate" -> "The C++ project could not be generated."|>]]];
  written = Quiet[Check[
    If[!DirectoryQ[dir], CreateDirectory[dir, CreateIntermediateDirectories -> True]];
    AllTrue[KeyValueMap[Export[files[#1], #2, "Text"] &, contents], StringQ] &&
      StringQ[Export[files["Manifest"], manifest, "RawJSON"]], False]];
  If[!TrueQ[written], Return[Failure["SPINASWrite", <|"MessageTemplate" -> "The output files could not all be written.", "Directory" -> dir|>]]];
  result = Join[<|"Directory" -> dir, "Files" -> files, "Process" -> ir["Process"],
    "IncomingLegs" -> ir["IncomingLegs"], "Build" -> <|"Status" -> "NOT_REQUESTED"|>,
    "Examples" -> <|"Status" -> "NOT_REQUESTED"|>,
    "CalcHEPValidation" -> <|"Status" -> "NOT_REQUESTED", "Configuration" -> calchep|>,
    "CalcHEPNumerical" -> Join[spinasCalcHEPNumericalPublicConfiguration[numerical],
      <|"Executable" -> FileNameJoin[{dir, "build", "n_calchep"}],
        "SessionDirectory" -> FileNameJoin[{dir, "calchep"}]|>],
    "CodeStyle" -> code["Style"]|>,
    KeyTake[ir, {"HelicityConfigurations", "InputBranchCount", "GeneratedBranchCount",
      "InitialSpinAverage", "SymmetryFactor", "FinalStateSymmetryDivisor", "FixedHelicities"}]];
  If[AssociationQ[ir["PhysicalProcess"]], result = Join[result,
    KeyTake[ir, {"PhysicalProcess", "ProcessDescription"}]]];
  If[TrueQ[options["Build"]] || TrueQ[options["RunExamples"]] || TrueQ[options["RunCalcHEPValidation"]],
    build = spinasProjectBuild[dir, name, root, options["RunExamples"], cmake, calchep["CalcHEPDirectory"]];
    If[FailureQ[build], Return[build]]; result = Join[result, build]];
  If[TrueQ[options["RunCalcHEPValidation"]],
    build = spinasRunCalcHEPValidation[dir, name, calchep]; If[FailureQ[build], Return[build]];
    result = Join[result, <|"CalcHEPValidation" -> build|>]];
  If[TrueQ[options["ReturnDetails"]], result, dir]
];

WriteSPINASProcess[directory_String, entries_List, model_, opts:OptionsPattern[]] :=
  spinasWriteFamily[directory, entries, model, {opts}, Automatic];
WriteSPINASProcess[directory_String, physicalProcess_Rule, entries_List, model_, opts:OptionsPattern[]] :=
  spinasWriteFamily[directory, entries, model, {opts}, physicalProcess];

WriteSPINASProcess[___] := (Message[WriteSPINASProcess::args];$Failed);
End[];
End[];
