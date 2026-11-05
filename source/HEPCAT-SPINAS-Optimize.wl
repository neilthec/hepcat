(* ::Package:: *)

(* Dependency-aware numerical evaluation planning. No physics identities. *)
Begin["ConstructiveDiagrams`Private`"];

(* Conservative real arithmetic for native scalar caches. Never infer reality
   from parameter defaults, declared parameter types, or checked external masses:
   checked_mass tolerates tiny imaginary components. Parameter-dependent masses
   therefore keep their original complex propagator arithmetic. *)
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

spinasOptimizedRealMass[ir_Association, mass_] := Module[{code},
  If[NumberQ[mass] && TrueQ[Im[mass] == 0] && FreeQ[mass, Indeterminate | DirectedInfinity],
    code = spinasCppReal[mass];
    If[StringQ[code], Return[<|"Code" -> code, "Proof" -> "Finite real numeric mass."|>]]];
  Missing["UnprovedRealMass"]
];

spinasOptimizeRealScalars[ir_Association, input_Association] := Module[
  {plan = input, scalars = {}, realNames = {}, proofs = {}, entry, expression,
   statement, prefix, mass, realQ, proof},
  Do[
    expression = entry["Expression"]; statement = entry["Statement"];
    realQ = False; proof = "";
    Which[
      MatchQ[expression, _MomProd],
        realQ = True; proof = "Lorentz product of real exported physical momenta.",
      MatchQ[expression, _Mandelstahm],
        realQ = True; proof = "Lorentz square of real exported physical momenta.",
      Head[expression] === PropDen && MemberQ[{2, 3}, Length[expression]] &&
          (Length[expression] === 2 || TrueQ[expression[[3]] == 0]),
        mass = spinasOptimizedRealMass[ir, expression[[2]]];
        If[AssociationQ[mass],
          prefix = spinasOptimizedMass2Prefix[statement];
          If[StringQ[prefix],
            statement = prefix <> " - (" <> mass["Code"] <> ") * (" <> mass["Code"] <> ");";
            realQ = True; proof = "Zero-width denominator; " <> mass["Proof"]]]
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
    "RealScalarNames" -> realNames, "RealScalarProofs" -> proofs|>]
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
   text, parameterNames, scalarNames, realNames, fallbackNames, known, visit, walk,
   pointSelected, pointVariables = <||>, pointDefinitions = {}, localSelected,
   localVariables, localDefinitions, render, preparePoint, prepareLocal, nodeIds,
   pointCap = 512, localCap = 512, candidates, massiveStates, rootIds, temp,
   localCount = 0, uniqueProducts = 0, duplicateProducts = 0, metadata, effectiveUses, effective, nextSelection,
   directDeclarations, directUpdates, directBranches, finishDirect, tag = Unique["spinasOptimize"], optimized,
   expressionCharacters, nodeLimit = 40000, timeBudget = 30},
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
      "ArithmeticNodeLimit" -> nodeLimit, "CSETimeConstraint" -> timeBudget|>|>];
  If[expressionCharacters > 250000, Return[finishDirect["Expression-size analysis allowance"]]];
  optimized = TimeConstrained[Catch[Block[{$RecursionLimit = 4096},
  parameterNames = Join[Lookup[plan["Parameters"], "Alias", {}],
    Lookup[Lookup[ir, "ConstantCaches", {}], "Name", {}],
    Lookup[Lookup[ir, "ParameterCaches", {}], "Name", {}]];
  scalarNames = Lookup[plan["Scalars"], "Name", {}]; realNames = Lookup[plan, "RealScalarNames", {}];
  fallbackNames = DeleteDuplicates[Flatten[StringCases[Flatten[Lookup[branches, "FallbackStatements", {}]],
    RegularExpression["const cdouble ([A-Za-z_][A-Za-z0-9_]*)"] -> "$1"]]];
  known = Join[parameterNames, scalarNames, tableNames,
    {"ldouble", "cdouble", "std", "pow", "sqrt", "sin", "cos", "tan", "asin", "acos", "atan",
      "exp", "log", "sinh", "cosh", "tanh", "conj", "real", "imag", "abs", "external_masses"}];
  intern[op_String, args___] := Module[
    {values = {args}, cacheKey, found, dep, real, operationCost, item, identifiers, depth},
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
      depth = 0;
      operationCost = If[StringContainsQ[item, "std::pow" | "std::sqrt" | "std::exp" | "std::log"], 12, 0],
      depth = 1 + Max[Lookup[nodes[#], "Depth"] & /@ values];
      If[depth > 256, Throw["Arithmetic-depth analysis allowance", tag]];
      dep = Max[Lookup[nodes[#], "Dependency"] & /@ values];
      real = AllTrue[values, TrueQ[nodes[#]["RealQ"]] &];
      operationCost = Switch[op, "/", If[real, 4, 16], "*", If[real, 1, 4], "neg", 1, _, If[real, 1, 2]] + Total[Lookup[nodes[#], "Cost"] & /@ values]];
    nodeCount++; If[nodeCount > nodeLimit, Throw["Arithmetic-node analysis allowance", tag]];
    AssociateTo[registry, cacheKey -> nodeCount];
    AssociateTo[nodes, nodeCount -> <|"Operation" -> op, "Arguments" -> values,
      "Dependency" -> dep, "RealQ" -> real, "Cost" -> operationCost, "Depth" -> depth|>]; nodeCount];
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
  render[node_Integer, local_Association : <||>, exclude_Integer : 0] := Module[{entry, args, operation},
    If[node =!= exclude && KeyExistsQ[pointVariables, node], Return[pointVariables[node]]];
    If[node =!= exclude && KeyExistsQ[local, node], Return[local[node]]];
    entry = nodes[node]; args = entry["Arguments"]; operation = entry["Operation"];
    If[operation === "atom", Return[First[args]]];
    If[operation === "neg", Return["(-" <> render[First[args], local] <> ")"]];
    "(" <> render[First[args], local] <> " " <> operation <> " " <> render[Last[args], local] <> ")"];
  preparePoint[node_Integer] := Module[{entry = nodes[node], variable},
    If[KeyExistsQ[pointVariables, node], Return[]];
    If[entry["Operation"] =!= "atom", Scan[If[MemberQ[pointSelected, #], preparePoint[#]] &, entry["Arguments"]]];
    variable = allocate["point_factor_" <> ToString[Length[pointVariables] + 1]];
    AppendTo[pointDeclarations, If[TrueQ[entry["RealQ"]], "ldouble ", "cdouble "] <> variable <> ";"];
    AppendTo[pointDefinitions, variable <> " = " <> render[node] <> ";"];
    AssociateTo[pointVariables, node -> variable]];
  Scan[preparePoint, Sort[pointSelected]];
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
    AppendTo[updatedBranches, Join[branch, <|"ExpressionPieces" -> temp,
      "ExpressionCpp" -> spinasNativeExpression[temp, 1], "LocalStatements" -> localDefinitions|>]],
    {branchIndex, Length[branches]}];
  metadata = <|"Backend" -> "DependencyDAG", "Status" -> "Applied", "CSESkipped" -> False,
    "ArithmeticNodes" -> Length[nodes], "ArithmeticNodeLimit" -> nodeLimit, "ArithmeticDepthLimit" -> 256, "CSETimeConstraint" -> timeBudget,
    "PointProductTables" -> uniqueProducts, "ReversedMasslessProductsShared" -> duplicateProducts,
    "PointFactors" -> Length[pointVariables], "SpinFactors" -> localCount,
    "RealScalarCount" -> Length[Lookup[plan, "RealScalarNames", {}]],
    "PointFactorLimit" -> pointCap, "SpinFactorLimitPerBranch" -> localCap,
    "OperationOrder" -> "Original ordered binary arithmetic; no reassociation or fast-math"|>;
  Join[plan, <|"Branches" -> updatedBranches, "PointDeclarations" -> pointDeclarations,
    "PointUpdates" -> Join[pointUpdates, pointDefinitions], "OptimizationReport" -> metadata|>]
  ], tag, Function[{reason, thrownTag}, finishDirect[reason]]], timeBudget, finishDirect["Arithmetic analysis time allowance"]];
  optimized
];

spinasOptimizeCanonicalIR[ir_Association] := Join[ir, <|"OptimizationReport" -> <|
  "Backend" -> "CanonicalDispatch", "Status" -> "BoundedFallback",
  "Reason" -> "Direct spin tables require the native product layout; constants and parameter-only caches remain active."|>|>];

End[];
