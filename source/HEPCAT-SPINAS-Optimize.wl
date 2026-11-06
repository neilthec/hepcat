(* ::Package:: *)

(* Dependency-aware numerical evaluation planning. No physics identities. *)
Begin["ConstructiveDiagrams`Private`"];

(* A zero-width propagator can use real arithmetic after checking its current
   mass exactly. Mutable parameters retain the original complex path. Any
   nonzero or unresolved width remains complex even when its value happens to
   produce a zero imaginary part at one point. *)
spinasOptimizedMass2Prefix[statement_String] := Module[
  {found = StringPosition[statement, "mass2("], opening, depth = 0, index, character, closing = Missing["NotClosed"]},
  If[found === {}, Return[$Failed]];
  opening = found[[1, 1]] + StringLength["mass2"];
  Do[
    character = StringTake[statement, {index}];
    If[character === "(", depth++];
    If[character === ")", depth--; If[depth === 0, closing = index; Break[]]],
    {index, opening, StringLength[statement]}];
  If[IntegerQ[closing], StringTake[statement, closing], $Failed]
];

spinasOptimizedRealMass[ir_Association, mass_, parameters_List : {}] := Module[
  {code, position, alias},
  If[NumberQ[mass] && TrueQ[Im[mass] == 0] && FreeQ[mass, Indeterminate | DirectedInfinity],
    code = spinasCppReal[mass];
    If[StringQ[code], Return[<|"Code" -> code, "Guard" -> "true",
      "AlwaysReal" -> True, "Proof" -> "Finite real numeric mass."|>]]];
  position = FirstPosition[ir["ParameterSymbols"], mass, Missing["NotParameter"]];
  If[Head[mass] === ModelParameter && !MissingQ[position] && Length[parameters] >= First[position],
    alias = parameters[[First[position]]]["Alias"];
    Return[<|"Code" -> "std::real(" <> alias <> ")",
      "Guard" -> "(std::imag(" <> alias <> ") == ldouble(0) && std::isfinite(std::real(" <> alias <> ")))",
      "AlwaysReal" -> False,
      "Proof" -> "The current mass is exactly real and finite; otherwise retain the original complex expression."|>]];
  Missing["UnprovedRealMass"]
];

spinasOptimizeRealScalars[ir_Association, input_Association] := Module[
  {plan = input, scalars = {}, realNames = {}, proofs = {}, entry, expression,
   statement, original, prefix, mass, realQ, proof, realRhs, rhs, equal,
   eligible = {}, guarded = {}, widthNames = {}},
  Do[
    expression = entry["Expression"]; statement = entry["Statement"];
    original = statement; realQ = False; proof = "";
    Which[
      MatchQ[expression, _MomProd],
        realQ = True; proof = "Lorentz product of real exported physical momenta.";
        AppendTo[eligible, entry["Name"]],
      MatchQ[expression, _Mandelstahm],
        realQ = True; proof = "Lorentz square of real exported physical momenta.";
        AppendTo[eligible, entry["Name"]],
      Head[expression] === PropDen && MemberQ[{2, 3}, Length[expression]] &&
          (Length[expression] === 2 || TrueQ[expression[[3]] == 0]),
        mass = spinasOptimizedRealMass[ir, expression[[2]], Lookup[plan, "Parameters", {}]];
        If[AssociationQ[mass],
          prefix = spinasOptimizedMass2Prefix[statement];
          If[StringQ[prefix],
            statement = prefix <> " - (" <> mass["Code"] <> ") * (" <> mass["Code"] <> ");";
            realQ = TrueQ[mass["AlwaysReal"]]; proof = "Zero-width denominator; " <> mass["Proof"];
            If[!realQ,
              equal = First[First[StringPosition[statement, "="]]];
              realRhs = StringTrim[StringDrop[StringDrop[statement, -1], equal]];
              equal = First[First[StringPosition[original, "="]]];
              rhs = StringTrim[StringDrop[StringDrop[original, -1], equal]];
              statement = entry["Name"] <> " = (" <> mass["Guard"] <> " ? cdouble(" <>
                realRhs <> ", ldouble(0)) : (" <> rhs <> "));";
              AppendTo[guarded, entry["Name"]]];
            AppendTo[eligible, entry["Name"]]]],
      Head[expression] === PropDen && Length[expression] === 3,
        AppendTo[widthNames, entry["Name"]]
    ];
    If[realQ, AppendTo[realNames, entry["Name"]];
      AppendTo[proofs, <|"Name" -> entry["Name"], "Expression" -> expression, "Proof" -> proof|>]];
    AppendTo[scalars, Join[entry, <|"Statement" -> statement, "RealQ" -> realQ|>]],
    {entry, Lookup[plan, "Scalars", {}]}];
  Join[plan, <|"Scalars" -> scalars,
    "ScalarDeclarations" -> ((If[TrueQ[#["RealQ"]], "ldouble ", "cdouble "] <> #["Name"] <> ";") & /@ scalars),
    "ScalarUpdates" -> Lookup[scalars, "Statement", {}],
    "CacheDeclarations" -> Join[Lookup[plan, "ProductDeclarations", {}],
      ((If[TrueQ[#["RealQ"]], "ldouble ", "cdouble "] <> #["Name"] <> ";") & /@ scalars)],
    "RealScalarNames" -> realNames, "RealScalarProofs" -> proofs,
    "RuntimeRealScalarNames" -> eligible, "GuardedRealPropagators" -> guarded,
    "ComplexWidthPropagators" -> widthNames|>]
];

(* Parse only ordered arithmetic emitted by HEPCAT. Calls and indexing remain
   opaque leaves; this never evaluates arbitrary C++ text as Wolfram code. *)
spinasOptEnclosedQ[text_String] := Module[{depth = 0, character, valid = True, length = StringLength[text]},
  If[length < 2 || StringTake[text, 1] =!= "(" || StringTake[text, -1] =!= ")", Return[False]];
  Do[character = StringTake[text, {i}];
    If[character === "(", depth++]; If[character === ")", depth--];
    If[depth === 0 && i < length, valid = False; Break[]], {i, length}];
  valid && depth === 0
];
spinasOptTopOperators[text_String, symbols_List] := Module[
  {depth = 0, square = 0, curly = 0, previous = "", character, positions = {}},
  Do[
    character = StringTake[text, {i}];
    If[depth === 0 && square === 0 && curly === 0 && MemberQ[symbols, character] &&
        i > 1 && !MemberQ[{"", "(", "+", "-", "*", "/", ",", "?", ":"}, previous],
      AppendTo[positions, i]];
    Switch[character, "(", depth++, ")", depth--, "[", square++, "]", square--,
      "{", curly++, "}", curly--];
    If[!StringMatchQ[character, WhitespaceCharacter], previous = character],
    {i, StringLength[text]}]; positions
];
spinasOptLiteralReplace[text_String, rules_List] := StringReplace[text,
  (RegularExpression["(?<![A-Za-z0-9_])\\Q" <> First[#] <> "\\E"] -> Last[#]) & /@ rules];

spinasOptimizeNativePlan[ir_Association, input_Association] := Module[
  {plan = spinasOptimizeRealScalars[ir, input], used, allocate, tables = {}, tableNames = {},
   pointDeclarations = {}, pointUpdates = {}, tableRules = {}, products, keep, reverseKeys = {},
   product, key, leftMassive, rightMassive, name, original, existing, sign, calls,
   call, arguments, replacement, allTexts, branches, branch, updatedBranches = {},
   registry = <||>, nodes = <||>, nodeCount = 0, counts = <||>, roots, intern, parse,
   text, parameterNames, scalarNames, realNames, eligibleNames, fallbackNames, known, visit, walk,
   pointSelected, pointVariables = <||>, pointDefinitions = {}, localSelected,
   localVariables, localDefinitions, render, preparePoint, prepareLocal, nodeIds,
   pointCap = 512, localCap = 512, candidates, massiveStates, rootIds, temp,
   localCount = 0, uniqueProducts = 0, duplicateProducts = 0, metadata, effectiveUses, effective, nextSelection,
   directDeclarations, directUpdates, directBranches, finishDirect, tag = Unique["spinasOptimize"], optimized,
   expressionCharacters, nodeLimit = 40000, timeBudget = 30,
   reciprocalVariables = <||>, reciprocalSelected, reciprocalGuardName,
   reciprocalGuard, prepareReciprocal, denominatorNode, cacheName, eligible},
  branches = plan["Branches"];
  allTexts = Join[Lookup[branches, "ExpressionCpp", {}],
    Flatten[Lookup[branches, "FallbackStatements", {}]], Lookup[plan, "ScalarUpdates", {}]];
  expressionCharacters = Total[StringLength /@ Lookup[branches, "ExpressionCpp", {}]];
  If[expressionCharacters > 1000000 || Length[plan["Products"]] > 1000,
    Return[Join[plan, <|"OptimizationReport" -> <|"Backend" -> "NativeCheckedValues",
      "Status" -> "BoundedFallback", "Reason" -> "The input exceeds the bounded native analysis allowance.",
      "CSESkipped" -> True, "ExpressionCharacters" -> expressionCharacters|>|>]]];
  used = DeleteDuplicates[Join[Lookup[plan, "ReservedNames", {}],
    Flatten[StringCases[allTexts, RegularExpression["[A-Za-z_][A-Za-z0-9_]*"]]],
    Lookup[plan["Parameters"], "Alias", {}],
    Lookup[Lookup[ir, "ConstantCaches", {}], "Name", {}],
    Lookup[Lookup[ir, "ParameterCaches", {}], "Name", {}]]];
  allocate[base_String] := Module[{chosen = base, suffix = 1},
    While[MemberQ[used, chosen], suffix++; chosen = base <> "_" <> ToString[suffix]];
    AppendTo[used, chosen]; chosen];

  (* Native APIs compute all small spin tables once per momentum point. *)
  products = plan["Products"];
  Do[
    original = product["Name"]; key = product["Key"];
    leftMassive = !TrueQ[ir["MassValues"][[key[[2]]]] == 0];
    rightMassive = !TrueQ[ir["MassValues"][[key[[4]]]] == 0];
    existing = Missing["New"]; sign = 1;
    If[!leftMassive && !rightMassive && key[[6]] === {},
      existing = FirstPosition[reverseKeys, {key[[1]], key[[4]], key[[5]], key[[2]], key[[3]], {}},
        Missing["New"], {1}]];
    If[MissingQ[existing],
      name = allocate["point_product_" <> ToString[Length[tables] + 1]];
      AppendTo[tables, <|"Name" -> name, "Native" -> original, "Key" -> key|>];
      AppendTo[reverseKeys, key]; uniqueProducts++; AppendTo[tableNames, name];
      Which[
        leftMassive && rightMassive,
          AppendTo[pointDeclarations, "cdouble " <> name <> "[2][2];"];
          Do[AppendTo[pointUpdates, name <> "[" <> ToString[a] <> "][" <> ToString[b] <> "] = " <>
            original <> ".v(" <> ToString[2 a - 1] <> ", " <> ToString[2 b - 1] <> ");"], {a, 0, 1}, {b, 0, 1}],
        leftMassive || rightMassive,
          AppendTo[pointDeclarations, "cdouble " <> name <> "[2];"];
          Do[AppendTo[pointUpdates, name <> "[" <> ToString[a] <> "] = " <>
            original <> ".v(" <> ToString[2 a - 1] <> ");"], {a, 0, 1}],
        True,
          AppendTo[pointDeclarations, "cdouble " <> name <> ";"];
          AppendTo[pointUpdates, name <> " = " <> original <> ".v();"]],
      name = tables[[First[existing]]]["Name"]; sign = -1; duplicateProducts++];
    calls = DeleteDuplicates[Flatten[StringCases[Lookup[branches, "ExpressionCpp", {}],
      RegularExpression["\\b" <> original <> "\\.v\\([^()]*\\)"]]]];
    Do[
      text = StringTake[call, {StringLength[original] + 4, -2}];
      arguments = If[StringTrim[text] === "", {}, StringTrim /@ StringSplit[text, ","]];
      If[Length[arguments] =!= Boole[leftMassive] + Boole[rightMassive], Continue[]];
      replacement = name <> StringJoin[("[(" <> # <> ") > 0 ? 1 : 0]") & /@ arguments];
      If[sign === -1, replacement = "(-" <> replacement <> ")"];
      AppendTo[tableRules, call -> replacement], {call, calls}],
    {product, products}];
  keep = Select[products, Function[p, MemberQ[Lookup[tables, "Native", {}], p["Name"]] ||
    AnyTrue[Lookup[plan, "ScalarUpdates", {}], Function[s, StringContainsQ[s, p["Name"]]]]]];
  plan = Join[plan, <|"Products" -> keep,
    "ProductDeclarations" -> (("spinas::sproduct " <> #["Name"] <> ";") & /@ keep),
    "ProductInitializers" -> Lookup[keep, "Initialization", {}],
    "ProductUpdates" -> ((#["Name"] <> ".update();") & /@ keep)|>];
  branches = Map[Function[b, Join[b, <|
    "ExpressionCpp" -> spinasOptLiteralReplace[b["ExpressionCpp"], tableRules],
    "ExpressionPieces" -> <|
      "NumeratorTerms" -> (spinasOptLiteralReplace[#, tableRules] & /@ b["ExpressionPieces"]["NumeratorTerms"]),
      "NumeratorSigns" -> b["ExpressionPieces"]["NumeratorSigns"],
      "Denominator" -> spinasOptLiteralReplace[b["ExpressionPieces"]["Denominator"], tableRules],
      "UnitDenominator" -> b["ExpressionPieces"]["UnitDenominator"]|>|>]], branches];

  directDeclarations = pointDeclarations; directUpdates = pointUpdates; directBranches = branches;
  finishDirect[reason_String] := Join[plan, <|"Branches" -> directBranches,
    "PointDeclarations" -> directDeclarations, "PointUpdates" -> directUpdates,
    "OptimizationReport" -> <|"Backend" -> "DirectSpinTables", "Status" -> "Applied",
      "CSESkipped" -> True, "Reason" -> reason, "PointProductTables" -> uniqueProducts,
      "ReversedMasslessProductsShared" -> duplicateProducts,
      "RealScalarCount" -> Length[Lookup[plan, "RealScalarNames", {}]],
      "GuardedRealPropagatorCount" -> Length[Lookup[plan, "GuardedRealPropagators", {}]],
      "ComplexWidthPropagatorCount" -> Length[Lookup[plan, "ComplexWidthPropagators", {}]],
      "ReusableRealDenominators" -> 0,
      "ArithmeticNodeLimit" -> nodeLimit, "CSETimeConstraint" -> timeBudget|>|>];
  If[expressionCharacters > 250000, Return[finishDirect["Expression-size analysis allowance"]]];
  optimized = TimeConstrained[Catch[Block[{$RecursionLimit = 4096},
  parameterNames = Join[Lookup[plan["Parameters"], "Alias", {}],
    Lookup[Lookup[ir, "ConstantCaches", {}], "Name", {}],
    Lookup[Lookup[ir, "ParameterCaches", {}], "Name", {}]];
  scalarNames = Lookup[plan["Scalars"], "Name", {}]; realNames = Lookup[plan, "RealScalarNames", {}];
  eligibleNames = Join[parameterNames, Lookup[plan, "RuntimeRealScalarNames", {}],
    {"ldouble", "cdouble", "std", "pow", "sqrt", "sin", "cos", "tan", "asin", "acos", "atan",
      "exp", "log", "sinh", "cosh", "tanh", "conj", "real", "imag", "abs"}];
  fallbackNames = DeleteDuplicates[Flatten[StringCases[Flatten[Lookup[branches, "FallbackStatements", {}]],
    RegularExpression["const cdouble ([A-Za-z_][A-Za-z0-9_]*)"] -> "$1"]]];
  known = Join[parameterNames, scalarNames, tableNames,
    {"ldouble", "cdouble", "std", "pow", "sqrt", "sin", "cos", "tan", "asin", "acos", "atan",
      "exp", "log", "sinh", "cosh", "tanh", "conj", "real", "imag", "abs", "external_masses"}];
  intern[op_String, args___] := Module[
    {values = {args}, cacheKey, found, dep, real, operationCost, item, identifiers, depth, reciprocalEligible},
    cacheKey = op <> ":" <> StringRiffle[ToString[#, InputForm] & /@ values, ","];
    found = Lookup[registry, cacheKey, Missing["New"]]; If[!MissingQ[found], Return[found]];
    If[op === "atom",
      item = First[values];
      identifiers = StringCases[StringReplace[item,
        RegularExpression["(?<![A-Za-z_0-9])[0-9]+(?:\\.[0-9]*)?(?:[eE][+-]?[0-9]+)?[fFlL]?"] -> ""],
        RegularExpression["[A-Za-z_][A-Za-z0-9_]*"]];
      dep = Which[StringContainsQ[item, RegularExpression["\\bds[0-9]+(?:_[0-9]+)?\\b"]], 2,
        AnyTrue[identifiers, MemberQ[fallbackNames, #] || !MemberQ[known, #] &], 2,
        AnyTrue[identifiers, MemberQ[scalarNames, #] || MemberQ[tableNames, #] || # === "external_masses" &], 1,
        True, 0];
      real = MemberQ[realNames, item] ||
        (StringStartsQ[item, "ldouble("] && spinasOptEnclosedQ[StringDrop[item, StringLength["ldouble"]]]) ||
        StringMatchQ[item, RegularExpression["[0-9]+(?:\\.[0-9]*)?(?:[eE][+-]?[0-9]+)?[fFlL]?"]];
      reciprocalEligible = dep <= 1 && AllTrue[identifiers, MemberQ[eligibleNames, #] &];
      depth = 0;
      operationCost = If[StringContainsQ[item, "std::pow" | "std::sqrt" | "std::exp" | "std::log"], 12, 0],
      depth = 1 + Max[Lookup[nodes[#], "Depth"] & /@ values];
      If[depth > 256, Throw["Arithmetic-depth analysis allowance", tag]];
      dep = Max[Lookup[nodes[#], "Dependency"] & /@ values];
      real = AllTrue[values, TrueQ[nodes[#]["RealQ"]] &];
      reciprocalEligible = dep <= 1 && AllTrue[values, TrueQ[nodes[#]["ReciprocalEligible"]] &];
      operationCost = Switch[op, "/", If[real, 4, 16], "*", If[real, 1, 4], "neg", 1, _, If[real, 1, 2]] + Total[Lookup[nodes[#], "Cost"] & /@ values]];
    nodeCount++; If[nodeCount > nodeLimit, Throw["Arithmetic-node analysis allowance", tag]];
    AssociateTo[registry, cacheKey -> nodeCount];
    AssociateTo[nodes, nodeCount -> <|"Operation" -> op, "Arguments" -> values,
      "Dependency" -> dep, "RealQ" -> real, "ReciprocalEligible" -> reciprocalEligible,
      "Cost" -> operationCost, "Depth" -> depth|>]; nodeCount];
  parse[source_String] := Module[{value = StringTrim[source], operators, first, at},
    While[spinasOptEnclosedQ[value], value = StringTrim[StringTake[value, {2, -2}]]];
    operators = spinasOptTopOperators[value, {"+", "-"}];
    If[operators === {}, operators = spinasOptTopOperators[value, {"*", "/"}]];
    If[operators =!= {},
      first = parse[StringTake[value, operators[[1]] - 1]];
      Do[at = operators[[j]];
        first = intern[StringTake[value, {at}], first,
          parse[StringTake[value, {at + 1, If[j < Length[operators], operators[[j + 1]] - 1, -1]}]]],
        {j, Length[operators]}]; Return[first]];
    If[StringStartsQ[value, "-"], Return[intern["neg", parse[StringDrop[value, 1]]]]];
    If[StringStartsQ[value, "+"], Return[parse[StringDrop[value, 1]]]];
    intern["atom", value]];
  roots = Map[Function[b, {parse /@ b["ExpressionPieces"]["NumeratorTerms"],
    parse[b["ExpressionPieces"]["Denominator"]]}], branches];
  visit[node_Integer] := (AssociateTo[counts, node -> (Lookup[counts, node, 0] + 1)];
    If[nodes[node]["Operation"] =!= "atom", Scan[visit, nodes[node]["Arguments"]]]);
  Scan[Function[r, Scan[visit, Join[First[r], {Last[r]}]]], roots];
  massiveStates = Times @@ MapThread[If[TrueQ[#1 == 0], 1, #2 + 1] &, {ir["MassValues"], ir["Spin2"]}];
  nodeIds = Keys[nodes];
  candidates = Select[nodeIds, nodes[#]["Dependency"] === 1 && nodes[#]["Cost"] >= 3 &&
    (Lookup[counts, #, 0] >= 2 || massiveStates >= 2) &];
  pointSelected = Take[Reverse[SortBy[candidates, (Lookup[counts, #, 0] massiveStates - 1) nodes[#]["Cost"] &]], UpTo[pointCap]];
  (* Once a parent has been cached its children no longer repeat at every
     original occurrence. Keep only temporaries with multiple effective uses. *)
  effectiveUses[selection_List, expressionRoots_List, rootWeight_Integer] := Module[{uses = <||>, traverse},
    traverse[node_Integer, weight_Integer] := If[!KeyExistsQ[pointVariables, node],
      If[MemberQ[selection, node],
        AssociateTo[uses, node -> (Lookup[uses, node, 0] + weight)],
        If[nodes[node]["Operation"] =!= "atom", Scan[traverse[#, weight] &, nodes[node]["Arguments"]]]]];
    Scan[traverse[#, rootWeight] &, expressionRoots];
    Do[If[nodes[node]["Operation"] =!= "atom", Scan[traverse[#, 1] &, nodes[node]["Arguments"]]], {node, selection}];
    uses];
  Do[
    effective = effectiveUses[pointSelected, Flatten[roots], massiveStates];
    nextSelection = Select[pointSelected, Lookup[effective, #, 0] >= 2 &];
    If[nextSelection === pointSelected, Break[]]; pointSelected = nextSelection,
    {16}];
  render[node_Integer, local_Association : <||>, exclude_Integer : 0, useReciprocals_:True] := Module[{entry, args, operation},
    If[node =!= exclude && KeyExistsQ[pointVariables, node], Return[pointVariables[node]]];
    If[node =!= exclude && KeyExistsQ[local, node], Return[local[node]]];
    entry = nodes[node]; args = entry["Arguments"]; operation = entry["Operation"];
    If[operation === "atom", Return[First[args]]];
    If[operation === "neg", Return["(-" <> render[First[args], local, 0, useReciprocals] <> ")"]];
    If[TrueQ[useReciprocals] && operation === "/" && KeyExistsQ[reciprocalVariables, Last[args]],
      Return[reciprocalVariables[Last[args]] <> ".divide(" <> render[First[args], local, 0, useReciprocals] <> ")"]];
    "(" <> render[First[args], local, 0, useReciprocals] <> " " <> operation <> " " <>
      render[Last[args], local, 0, useReciprocals] <> ")"];
  preparePoint[node_Integer] := Module[{entry = nodes[node], variable},
    If[KeyExistsQ[pointVariables, node], Return[]];
    If[entry["Operation"] =!= "atom", Scan[If[MemberQ[pointSelected, #], preparePoint[#]] &, entry["Arguments"]]];
    variable = allocate["point_factor_" <> ToString[Length[pointVariables] + 1]];
    AppendTo[pointDeclarations, If[TrueQ[entry["RealQ"]], "ldouble ", "cdouble "] <> variable <> ";"];
    AppendTo[pointDefinitions, variable <> " = " <> render[node, <||>, 0, False] <> ";"];
    AssociateTo[pointVariables, node -> variable]];
  Scan[preparePoint, Sort[pointSelected]];
  (* Cache a complete, unchanged point-only denominator. In particular, never
     distribute a common division over a sum or break a denominator product. *)
  reciprocalSelected = DeleteDuplicates[Join[
    Cases[nodeIds, node_Integer /; nodes[node]["Operation"] === "/" &&
      nodes[node]["Dependency"] === 2 &&
      TrueQ[nodes[Last[nodes[node]["Arguments"]]]["ReciprocalEligible"]] :>
        Last[nodes[node]["Arguments"]]],
    Cases[Range[Length[roots]], index_Integer /;
      !TrueQ[branches[[index]]["ExpressionPieces"]["UnitDenominator"]] &&
      TrueQ[nodes[Last[roots[[index]]]]["ReciprocalEligible"]] :> Last[roots[[index]]]]]];
  If[reciprocalSelected =!= {},
    reciprocalGuardName = allocate["real_denominator_domain"];
    reciprocalGuard = If[ir["ParameterSymbols"] === {}, "true", StringRiffle[
      Table["std::imag(parameters[" <> ToString[index - 1] <> "]) == ldouble(0)",
        {index, Length[ir["ParameterSymbols"]]}], " && "]];
    PrependTo[pointDeclarations, "bool " <> reciprocalGuardName <> ";"];
    PrependTo[pointUpdates, reciprocalGuardName <> " = " <> reciprocalGuard <> ";"]];
  prepareReciprocal[node_Integer] := Module[{variable},
    variable = allocate["point_denominator_" <> ToString[Length[reciprocalVariables] + 1]];
    AppendTo[pointDeclarations, "hepcat_spinas::CachedDenominator " <> variable <> ";"];
    AppendTo[pointDefinitions, variable <> " = hepcat_spinas::CachedDenominator(" <>
      render[node, <||>, 0, False] <> ", " <> reciprocalGuardName <> ");"];
    AssociateTo[reciprocalVariables, node -> variable]];
  Scan[prepareReciprocal, reciprocalSelected];
  Do[
    rootIds = Join[First[roots[[branchIndex]]], {Last[roots[[branchIndex]]]}];
    counts = <||>;
    walk[node_Integer] := If[!KeyExistsQ[pointVariables, node],
      AssociateTo[counts, node -> (Lookup[counts, node, 0] + 1)];
      If[nodes[node]["Operation"] =!= "atom", Scan[walk, nodes[node]["Arguments"]]]];
    Scan[walk, rootIds];
    candidates = Select[Keys[counts], nodes[#]["Dependency"] === 2 && Lookup[counts, #] >= 2 && nodes[#]["Cost"] >= 2 &];
    localSelected = Take[Reverse[SortBy[candidates, (Lookup[counts, #] - 1) nodes[#]["Cost"] &]], UpTo[localCap]];
    Do[
      effective = effectiveUses[localSelected, rootIds, 1];
      nextSelection = Select[localSelected, Lookup[effective, #, 0] >= 2 &];
      If[nextSelection === localSelected, Break[]]; localSelected = nextSelection,
      {16}];
    localVariables = <||>; localDefinitions = {};
    prepareLocal[node_Integer] := Module[{entry = nodes[node], variable},
      If[KeyExistsQ[localVariables, node], Return[]];
      If[entry["Operation"] =!= "atom", Scan[If[MemberQ[localSelected, #], prepareLocal[#]] &, entry["Arguments"]]];
      variable = allocate["spin_factor_" <> ToString[localCount + 1]]; localCount++;
      AppendTo[localDefinitions, "const " <> If[TrueQ[entry["RealQ"]], "ldouble ", "cdouble "] <> variable <>
        " = " <> render[node, localVariables] <> ";"];
      AssociateTo[localVariables, node -> variable]];
    Scan[prepareLocal, Sort[localSelected]];
    branch = branches[[branchIndex]];
    temp = Join[branch["ExpressionPieces"], <|
      "NumeratorTerms" -> (render[#, localVariables] & /@ First[roots[[branchIndex]]]),
      "Denominator" -> render[Last[roots[[branchIndex]]], localVariables]|>];
    If[!TrueQ[temp["UnitDenominator"]] && KeyExistsQ[reciprocalVariables, Last[roots[[branchIndex]]]],
      temp = Join[temp, <|"DenominatorCache" -> reciprocalVariables[Last[roots[[branchIndex]]]]|>]];
    AppendTo[updatedBranches, Join[branch, <|"ExpressionPieces" -> temp,
      "ExpressionCpp" -> spinasNativeExpression[temp, 1], "LocalStatements" -> localDefinitions|>]],
    {branchIndex, Length[branches]}];
  metadata = <|"Backend" -> "DependencyDAG", "Status" -> "Applied", "CSESkipped" -> False,
    "ArithmeticNodes" -> Length[nodes], "ArithmeticNodeLimit" -> nodeLimit, "ArithmeticDepthLimit" -> 256, "CSETimeConstraint" -> timeBudget,
    "PointProductTables" -> uniqueProducts, "ReversedMasslessProductsShared" -> duplicateProducts,
    "PointFactors" -> Length[pointVariables], "SpinFactors" -> localCount,
    "RealScalarCount" -> Length[Lookup[plan, "RealScalarNames", {}]],
    "GuardedRealPropagatorCount" -> Length[Lookup[plan, "GuardedRealPropagators", {}]],
    "ComplexWidthPropagatorCount" -> Length[Lookup[plan, "ComplexWidthPropagators", {}]],
    "ReusableRealDenominators" -> Length[reciprocalVariables],
    "ReciprocalPolicy" -> "Exactly real runtime parameters and width-free scalar denominators; original complex division outside the finite normal domain.",
    "PointFactorLimit" -> pointCap, "SpinFactorLimitPerBranch" -> localCap,
    "OperationOrder" -> "Original numerator and denominator products with guarded real reciprocal reuse; no fast-math"|>;
  Join[plan, <|"Branches" -> updatedBranches, "PointDeclarations" -> pointDeclarations,
    "PointUpdates" -> Join[pointUpdates, pointDefinitions], "OptimizationReport" -> metadata|>]
  ], tag, Function[{reason, thrownTag}, finishDirect[reason]]], timeBudget, finishDirect["Arithmetic analysis time allowance"]];
  optimized
];

spinasOptimizeCanonicalIR[ir_Association] := Join[ir, <|"OptimizationReport" -> <|
  "Backend" -> "CanonicalDispatch", "Status" -> "BoundedFallback",
  "Reason" -> "Direct spin tables require the native product layout; constants and parameter-only caches remain active."|>|>];

End[];
