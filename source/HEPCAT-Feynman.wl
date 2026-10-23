(* ::Package:: *)

(* ::Subsection::Closed:: *)
(*Feynman Rule Models*)

FeynmanModel::usage = "FeynmanModel[particles, vertices, variables, functions] is a Feynman-rule model read from CalcHEP tables. Its four parts have the same positional organization as an ordinary HEPCAT model. The initial amplitude backend uses all-incoming momenta and unitary-gauge physical fields.";
DiracWord::usage = "DiracWord[operators] preserves the written order of Dirac gamma matrices and momentum slashes.";
DiracGamma::usage = "DiracGamma[index] denotes a Dirac gamma matrix with the indicated Lorentz index.";
DiracGamma5::usage = "DiracGamma5 denotes the chirality matrix in an ordered DiracWord.";
DiracSlash::usage = "DiracSlash[Mom[label]] denotes a momentum contracted with a Dirac gamma matrix.";
LorentzIndex::usage = "LorentzIndex[slot] is a local vertex Lorentz index. LorentzIndex[slot, 2] is the second index of a tensor field.";
LorentzMetric::usage = "LorentzMetric[index1, index2] denotes the Minkowski metric tensor.";
LorentzMomentum::usage = "LorentzMomentum[Mom[label], index] denotes a momentum carrying one Lorentz index.";
LorentzScalarProduct::usage = "LorentzScalarProduct[p, q] retains an off-shell momentum dot product in a Feynman rule until the vertex momentum slots are attached. It does not apply Mass[label]^2 identities.";

Begin["ConstructiveDiagrams`Private`"];

feynmanModelQ[model_] := MatchQ[model, FeynmanModel[_List, _List, _List, _List]];

(* Each product contains at most one ordered word; scalar factors still use Times. *)
feynmanWordParts[term_] := Module[{factors, words},
  factors = If[Head[term] === Times, List @@ term, {term}];
  words = Cases[factors, _DiracWord];
  If[Length[words] > 1, Return[Failure["InvalidDiracProduct", <|"Expression" -> term|>]]];
  {Times @@ DeleteCases[factors, _DiracWord], If[words === {}, {}, List @@ First[words]], words =!= {}}
];

feynmanMultiply[a_, b_] := Module[{left, right, terms, x, y},
  left = If[Head[a] === Plus, List @@ a, {a}];
  right = If[Head[b] === Plus, List @@ b, {b}];
  terms = Flatten[Table[
    x = feynmanWordParts[l]; y = feynmanWordParts[r];
    If[FailureQ[x] || FailureQ[y], Return[Failure["InvalidDiracProduct", <||>]]];
    x[[1]] y[[1]] If[TrueQ[x[[3]] || y[[3]]], DiracWord @@ Join[x[[2]], y[[2]]], 1],
    {l, left}, {r, right}], 1];
  Total[terms]
];

feynmanPower[base_, exponent_] := Which[
  FreeQ[base, _DiracWord], base^exponent,
  IntegerQ[exponent] && 0 <= exponent <= 32,
    Nest[feynmanMultiply[#, base] &, 1, exponent],
  True, Failure["UnsupportedDiracPower", <|"Exponent" -> exponent|>]
];

feynmanDot[a_, b_] := Which[
  MatchQ[a, _Mom] && MatchQ[b, _Mom], LorentzScalarProduct[a, b],
  MatchQ[a, _LorentzIndex] && MatchQ[b, _LorentzIndex], LorentzMetric[a, b],
  MatchQ[a, _Mom] && MatchQ[b, _LorentzIndex], LorentzMomentum[a, b],
  MatchQ[b, _Mom] && MatchQ[a, _LorentzIndex], LorentzMomentum[b, a],
  True, Failure["UnsupportedLorentzProduct", <|"Arguments" -> {a, b}|>]
];

feynmanIdentifierQ[name_String] := StringMatchQ[name, RegularExpression["[A-Za-z_][A-Za-z_0-9]*"]];
feynmanParameter[name_String, scope_String] :=
  ConstructiveDiagrams`ModelParameter[name, scope, "Real"];

(* Decimal and scientific notation are read exactly, without evaluating input text. *)
feynmanNumber[text_String] := Module[{parts, mantissa, exponent = 0, point, digits, scale = 0, sign = 1},
  parts = StringSplit[text, RegularExpression["[eE]"]];
  mantissa = First[parts];
  If[Length[parts] === 2,
    If[StringStartsQ[parts[[2]], "-"], sign = -1];
    exponent = sign FromDigits[StringDelete[parts[[2]], {"+", "-"}]]];
  point = StringPosition[mantissa, "."];
  If[point =!= {}, scale = StringLength[mantissa] - First[First[point]]];
  digits = StringDelete[mantissa, "."];
  FromDigits[digits] 10^(exponent - scale)
];

parseFeynmanExpression[input_String, scope_String : "StandaloneFeynmanExpression"] := Module[
  {text, tokens, joined, position = 1,
   tag = Unique["FeynmanParser"], peek, take, expect, fail, identifier, call,
   primary, dot, power, unary, product, sum, result, scalarFunctions, externalFunctions},
  fail[reason_] := Throw[Failure["InvalidFeynmanExpression",
    <|"Expression" -> input, "Reason" -> reason, "Token" -> position|>], tag];
  text = StringTrim[If[StringContainsQ[input, "%"],
    StringTake[input, First[First[StringPosition[input, "%"]]] - 1], input]];
  scalarFunctions = <|"sqrt" -> Sqrt, "sin" -> Sin, "cos" -> Cos,
    "tan" -> Tan, "asin" -> ArcSin, "acos" -> ArcCos, "atan" -> ArcTan,
    "exp" -> Exp, "log" -> Log, "abs" -> Abs, "fabs" -> Abs, "cabs" -> Abs,
    "pow" -> Power|>;
  externalFunctions = <|"initQCD5" -> 4, "MbEff" -> 1, "McEff" -> 1, "MqEff" -> 2|>;
  tokens = StringCases[text, RegularExpression[
    "(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?|[A-Za-z_][A-Za-z_0-9]*|[+*/^(),.\\-]"]];
  joined = StringJoin[tokens];
  If[text === "" || joined =!= StringReplace[text, WhitespaceCharacter -> ""],
    Return[Failure["InvalidFeynmanExpression", <|"Expression" -> input,
      "Reason" -> "Unrecognized characters or an empty expression."|>]]];
  peek[] := If[position <= Length[tokens], tokens[[position]], EndOfFile];
  take[] := Module[{token = peek[]}, position++; token];
  expect[token_] := If[take[] =!= token, fail["Expected " <> token <> "."]];
  identifier[name_] := Which[
    name === "i", I,
    name === "Sqrt2", Sqrt[2],
    name === "G5", DiracWord[DiracGamma5],
    StringMatchQ[name, RegularExpression["p[1-9][0-9]*"]], Mom[FromDigits[StringDrop[name, 1]]],
    StringMatchQ[name, RegularExpression["m[1-9][0-9]*"]], LorentzIndex[FromDigits[StringDrop[name, 1]]],
    StringMatchQ[name, RegularExpression["M[1-9][0-9]*"]], LorentzIndex[FromDigits[StringDrop[name, 1]], 2],
    StringMatchQ[name, RegularExpression["[pmM][0-9]+"]], fail["Invalid momentum or Lorentz slot number."],
    feynmanIdentifierQ[name], feynmanParameter[name, scope],
    True, fail["Invalid identifier."]
  ];
  call[name_, arguments_] := Which[
    name === "G" && Length[arguments] === 1 && MatchQ[First[arguments], _LorentzIndex],
      DiracWord[DiracGamma[First[arguments]]],
    name === "G" && Length[arguments] === 1 && MatchQ[First[arguments], _Mom],
      DiracWord[DiracSlash[First[arguments]]],
    KeyExistsQ[scalarFunctions, name] &&
        Length[arguments] === If[name === "pow", 2, 1] &&
        FreeQ[arguments, _DiracWord | _Mom | _LorentzIndex],
      Lookup[scalarFunctions, name] @@ arguments,
    KeyExistsQ[externalFunctions, name] && Length[arguments] === externalFunctions[name] &&
        FreeQ[arguments, _DiracWord | _Mom | _LorentzIndex],
      ConstructiveDiagrams`ModelFunction[name, scope, arguments],
    True, fail["Unsupported function or function arguments: " <> name <> "."]
  ];
  primary[] := Module[{token = take[], value, arguments = {}},
    Which[
      token === "(", value = sum[]; expect[")"]; value,
      StringQ[token] && StringMatchQ[token, RegularExpression[
        "(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?"]], feynmanNumber[token],
      StringQ[token] && feynmanIdentifierQ[token],
        If[peek[] === "(",
          take[];
          If[peek[] =!= ")", AppendTo[arguments, sum[]];
            While[peek[] === ",", take[]; AppendTo[arguments, sum[]]]];
          expect[")"]; call[token, arguments],
          identifier[token]],
      True, fail["Expected a number, identifier or parenthesized expression."]
    ]
  ];
  dot[] := Module[{value = primary[], next},
    While[peek[] === ".", take[]; next = feynmanDot[value, primary[]];
      If[FailureQ[next], fail["Unsupported Lorentz dot product."]]; value = next]; value];
  power[] := Module[{value = dot[], next, exponent},
    If[peek[] === "^", take[]; exponent = unary[];
      If[!FreeQ[exponent, _DiracWord | _Mom | _LorentzIndex], fail["A power must have a scalar exponent."]];
      next = feynmanPower[value, exponent];
      If[FailureQ[next], fail["Unsupported power of a Dirac expression."]]; value = next]; value];
  unary[] := Which[peek[] === "+", take[]; unary[], peek[] === "-", take[]; -unary[], True, power[]];
  product[] := Module[{value = unary[], operation, next},
    While[MemberQ[{"*", "/"}, peek[]], operation = take[]; next = unary[];
      If[operation === "/", next = feynmanPower[next, -1]];
      If[FailureQ[next], fail["A Dirac expression cannot appear in a denominator."]];
      value = feynmanMultiply[value, next];
      If[FailureQ[value], fail["Invalid ordered Dirac product."]]]; value];
  sum[] := Module[{value = product[], operation, next},
    While[MemberQ[{"+", "-"}, peek[]], operation = take[]; next = product[];
      value = value + If[operation === "+", next, -next]]; value];
  Catch[result = sum[];
    If[position <= Length[tokens], fail["Unexpected trailing tokens."]];
    If[!FreeQ[result, _Mom | _LorentzIndex],
      If[!FreeQ[result /. {_LorentzMetric -> 1, _LorentzMomentum -> 1, _LorentzScalarProduct -> 1, _DiracWord -> 1}, _Mom | _LorentzIndex],
        fail["Uncontracted momentum or index outside a tensor or Dirac word."]]];
    result, tag]
];

feynmanTableRows[file_String] := Module[{stream, line, lineNumber = 0, rows = {}, cuts, starts, ends, fields},
  stream = Quiet[Check[OpenRead[file], $Failed]];
  If[!MatchQ[stream, _InputStream], Return[Failure["ModelFileUnavailable", <|"File" -> file|>]]];
  CheckAbort[
    While[True,
      line = ReadLine[stream]; lineNumber++;
      If[line === EndOfFile, Break[]];
      If[StringStartsQ[StringTrim[line], "===="], Break[]];
      If[!StringContainsQ[line, "|"], Continue[]];
      cuts = First /@ StringPosition[line, "|"];
      starts = Prepend[cuts + 1, 1]; ends = Append[cuts - 1, StringLength[line]];
      fields = MapThread[If[#1 > #2, "", StringTrim[StringTake[line, {#1, #2}]]] &, {starts, ends}];
      If[StringEndsQ[StringTrim[line], "|"], fields = Most[fields]];
      AppendTo[rows, <|"Line" -> lineNumber, "Fields" -> fields|>]
    ], Close[stream]; Abort[]];
  Close[stream]; rows
];

feynmanVertexSlotsQ[row_List, particles_List] := Module[
  {names = DeleteCases[Drop[row, -2], ""], ranks, indices, momenta, terms, rank},
  rank[name_] := Module[{field},
    field = SelectFirst[particles, MemberQ[#[[{2, 3}]], name] &, Missing["UnknownField"]];
    If[!MissingQ[field], Return[Which[field[[9]] === "T", 2, field[[5]] === 2, 1, True, 0]]];
    If[!StringEndsQ[name, {".f", ".c", ".C", ".t"}], Return[$Failed]];
    field = SelectFirst[particles, MemberQ[#[[{2, 3}]], StringDrop[name, -2]] &, Missing["UnknownField"]];
    If[MissingQ[field], $Failed, If[StringEndsQ[name, ".t"] && field[[5]] === 2, 2, 0]]
  ];
  ranks = rank /@ names;
  If[MemberQ[ranks, $Failed], Return[False]];
  If[!FreeQ[row[[-1]], Power[base_, exponent_] /; !FreeQ[base, _LorentzIndex] && exponent =!= 1],
    Return[False]];
  indices = Cases[row[[-1]], index_LorentzIndex :> List @@ index, Infinity];
  momenta = Flatten[Cases[row[[-1]], Mom[i_Integer] :> {i}, Infinity]];
  momenta = Join[momenta, Flatten[Cases[row[[-1]], MomProd[i_Integer, j_Integer] :> {i, j}, Infinity]]];
  If[!AllTrue[momenta, 1 <= # <= Length[names] &] ||
      !AllTrue[indices, MatchQ[#, {_Integer} | {_Integer, 2}] && 1 <= First[#] <= Length[names] &], Return[False]];
  terms = Expand[row[[-1]]]; terms = If[Head[terms] === Plus, List @@ terms, {terms}];
  AllTrue[terms, Function[term, term === 0 || And @@ Table[
    Count[term, LorentzIndex[i], Infinity] === If[ranks[[i]] >= 1, 1, 0] &&
      Count[term, LorentzIndex[i, 2], Infinity] === If[ranks[[i]] === 2, 1, 0],
    {i, Length[names]}]]]
];

readFeynmanModel[dir_String, number_] := Module[
  {paths, tables, particles = {}, vertices = {}, variables = {}, functions = {},
   tag = Unique["FeynmanModelReader"], row, fields, parse, parameter, value, comment, expression, fail,
   scope = modelParameterScope[dir, number]},
  paths = AssociationThread[{"Particles", "Vertices", "Variables", "Functions"},
    FileNameJoin[{dir, # <> ToString[number] <> ".mdl"}] & /@ {"prtcls", "lgrng", "vars", "func"}];
  tables = Map[feynmanTableRows, paths];
  If[AnyTrue[Values[tables], FailureQ], Return[First[Select[Values[tables], FailureQ]]]];
  fail[file_, entry_, reason_] := Throw[Failure["InvalidFeynmanModel",
    <|"File" -> paths[file], "Line" -> entry["Line"], "Reason" -> reason|>], tag];
  parse[text_, file_, entry_] := Module[{parsed = parseFeynmanExpression[text, scope]},
    If[FailureQ[parsed], fail[file, entry, parsed]]; parsed];
  Catch[
    Do[fields = row["Fields"];
      If[Length[fields] >= 2 && StringTrim[fields[[1]]] === "Name", Continue[]];
      If[Length[fields] < 2 || !feynmanIdentifierQ[fields[[1]]], fail["Variables", row, "Invalid parameter row."]];
      parameter = feynmanParameter[fields[[1]], scope];
      value = parse[fields[[2]], "Variables", row];
      If[!FreeQ[value, _DiracWord | _LorentzMetric | _LorentzMomentum | _LorentzScalarProduct | _MomProd],
        fail["Variables", row, "A model parameter must be scalar."]];
      AppendTo[variables, {parameter, value, If[Length[fields] >= 3, fields[[3]], ""]}],
      {row, tables["Variables"]}];
    Do[fields = row["Fields"];
      If[Length[fields] >= 2 && StringTrim[fields[[1]]] === "Name", Continue[]];
      If[Length[fields] < 2 || !feynmanIdentifierQ[fields[[1]]], fail["Functions", row, "Invalid derived-parameter row."]];
      parameter = feynmanParameter[fields[[1]], scope];
      expression = StringPosition[fields[[2]], "%"];
      comment = If[expression === {}, "", StringTrim[StringDrop[fields[[2]], First[First[expression]]]]];
      value = parse[fields[[2]], "Functions", row];
      If[!FreeQ[value, _DiracWord | _LorentzMetric | _LorentzMomentum | _LorentzScalarProduct | _MomProd],
        fail["Functions", row, "A derived model parameter must be scalar."]];
      AppendTo[functions, {parameter, value, comment}], {row, tables["Functions"]}];
    Do[fields = row["Fields"];
      If[StringContainsQ[First[fields], "Full"], Continue[]];
      If[Length[fields] =!= 11, fail["Particles", row, "Expected eleven particle fields."]];
      Do[fields[[index]] = parse[fields[[index]], "Particles", row], {index, {4, 5, 6, 8}}];
      If[!AllTrue[fields[[{4, 5, 8}]], IntegerQ], fail["Particles", row, "Particle number, doubled spin and color must be integers."]];
      If[!MatchQ[fields[[6]], 0 | ConstructiveDiagrams`ModelParameter[_String, _String, _String]],
        fail["Particles", row, "A particle mass must be a parameter name or zero."]];
      AppendTo[particles, fields], {row, tables["Particles"]}];
    If[particles === {}, Throw[Failure["InvalidFeynmanModel", <|"File" -> paths["Particles"],
      "Reason" -> "The particle table contains no particles."|>], tag]];
    Do[fields = row["Fields"];
      If[First[fields] === "P1", Continue[]];
      If[Length[fields] =!= 6 || MemberQ[Take[fields, 3], ""], fail["Vertices", row, "Expected three or four fields, a factor and a Lorentz expression."]];
      fields[[-2]] = parse[fields[[-2]], "Vertices", row];
      If[!FreeQ[fields[[-2]], _DiracWord | _LorentzMetric | _LorentzMomentum | _LorentzScalarProduct | _MomProd],
        fail["Vertices", row, "A vertex factor must be scalar."]];
      fields[[-1]] = parse[fields[[-1]], "Vertices", row];
      If[!feynmanVertexSlotsQ[fields, particles], fail["Vertices", row,
        "Vertex momentum slots or external Lorentz ranks do not match its particle fields."]];
      AppendTo[vertices, fields], {row, tables["Vertices"]}];
    FeynmanModel[particles, vertices, variables, functions], tag]
];

End[];

(* ::Section::Closed:: *)
(*Unitary-Gauge Feynman Diagrams*)

DiracChain::usage = "DiracChain[FermionBar[leg, mass, chirality], DiracWord[operators], FermionKet[leg, mass, chirality]] is an ordered all-incoming external fermion bilinear. Internal slashes remain off shell.";
FermionBar::usage = "FermionBar[leg, mass, chirality] is the incoming antifermion endpoint of a DiracChain. Chirality names the underlying fermion field.";
FermionKet::usage = "FermionKet[leg, mass, chirality] is the incoming fermion endpoint of a DiracChain.";
PolarizationVector::usage = "PolarizationVector[leg, index, mass, helicity] is an external vector wavefunction. A generic massless vector needs a helicity before conversion to spinor products.";
ColorDelta::usage = "ColorDelta[index1, index2] is a contracted color Kronecker tensor.";
ColorGenerator::usage = "ColorGenerator[antifundamental, fundamental, adjoint] is the SU(3) generator lambda/2 in CalcHEP's convention.";
ColorStructure::usage = "ColorStructure[a, b, c] is the SU(3) structure constant f(a,b,c). Its CalcHEP vertex multiplier i is retained separately.";
ColorIndex::usage = "ColorIndex[leg] labels an external color state; internal color indices are local dummy symbols.";

Begin["ConstructiveDiagrams`Private`"];

feynmanFailure[name_, text_, extra_: <||>] := Failure[name,
  Join[<|"MessageTemplate" -> text|>, extra]];

feynmanUnphysicalNameQ[name_String] := StringEndsQ[name, {".f", ".c", ".C"}];

(* Tensor auxiliaries encode CalcHEP's four-gluon contact term. They have
   constant propagators, not a new physical pole or an on-shell condition. *)
feynmanPhysicalModel[model_FeynmanModel] := Module[
  {particles, vertices, names, auxiliary, base, row, unknown},
  If[!AllTrue[model[[1]], ListQ[#] && Length[#] === 11 &&
      AllTrue[#[[{1, 2, 3, 9}]], StringQ] && IntegerQ[#[[5]]] && IntegerQ[#[[8]]] &] ||
      !AllTrue[model[[2]], ListQ[#] && Length[#] === 6 && AllTrue[Take[#, 4], StringQ] &],
    Return[feynmanFailure["InvalidFeynmanModel", "Feynman particle or vertex rows have invalid shapes."]]];
  If[model[[1]] === {}, Return[feynmanFailure["EmptyParticleTable", "The Feynman model has no particle declarations."]]];
  particles = Select[model[[1]], !AnyTrue[#[[{2, 3}]], feynmanUnphysicalNameQ] &];
  vertices = Select[model[[2]], !AnyTrue[DeleteCases[Drop[#, -2], ""],
    feynmanUnphysicalNameQ] &];
  names = DeleteDuplicates[Flatten[particles[[All, {2, 3}]]]];
  auxiliary = Select[DeleteDuplicates[Flatten[DeleteCases[Drop[#, -2], ""] & /@ vertices]],
    !MemberQ[names, #] && StringEndsQ[#, ".t"] &];
  Do[
    base = StringDrop[name, -2];
    row = SelectFirst[particles, MemberQ[#[[{2, 3}]], base] &, Missing["UnknownTensorSource"]];
    If[MissingQ[row] || row[[5]] =!= 2 || row[[6]] =!= 0,
      Return[feynmanFailure["UnsupportedTensorAuxiliary",
        "Only the massless-vector tensor auxiliary is implemented.", <|"Particle" -> name|>]]];
    AppendTo[particles, {row[[1]] <> " contact auxiliary", name, name, 0, 4, 0,
      "0", row[[8]], "T", name, name}], {name, auxiliary}];
  names = DeleteDuplicates[Flatten[particles[[All, {2, 3}]]]];
  unknown = Complement[DeleteDuplicates[Flatten[
    DeleteCases[Drop[#, -2], ""] & /@ vertices]], names];
  If[unknown =!= {}, Return[feynmanFailure["UnknownFeynmanParticles",
    "Vertices contain undeclared physical fields.", <|"Particles" -> unknown|>]]];
  If[AnyTrue[particles, !MemberQ[{0, 1, 2}, #[[5]]] && #[[9]] =!= "T" &],
    Return[feynmanFailure["UnsupportedFeynmanSpin", "The Feynman backend supports scalars, Dirac fermions, vectors and the CalcHEP contact auxiliary."]]];
  If[AnyTrue[particles, OddQ[#[[5]]] && #[[2]] === #[[3]] &],
    Return[feynmanFailure["UnsupportedMajoranaFermion", "Majorana fermion flow is not implemented."]]];
  {particles, vertices, model[[3]], model[[4]]}
];

feynmanExternalParticle[name_String, names_List, spin_Association] := Module[{base, helicity},
  If[MemberQ[names, name], Return[{name, Automatic}]];
  helicity = Which[StringEndsQ[name, ".+"], 1, StringEndsQ[name, ".-"], -1,
    StringEndsQ[name, ".0"], 0, True, Missing["NotHelicityAlias"]];
  If[MissingQ[helicity], Return[Missing["UnknownParticle", name]]];
  base = StringDrop[name, -2];
  If[MemberQ[names, base] && Lookup[spin, base, None] === 2,
    {base, helicity}, Missing["UnknownParticle", name]]
];

feynmanMomentumTerms[p_Mom] := {{1, p}};
feynmanMomentumTerms[p_Plus] := Flatten[feynmanMomentumTerms /@ List @@ p, 1];
feynmanMomentumTerms[Times[c_, p_Mom]] /; FreeQ[c, _Mom] := {{c, p}};
feynmanMomentumTerms[0] := {};
feynmanMomentumTerms[_] := $Failed;

feynmanMomentumDot[p_, q_] := Module[{left, right},
  (* Expand sums before invoking MomProd, whose equal-label rule is on shell. *)
  left = feynmanMomentumTerms[Expand[p //. Mom[Multiparticle[labels___]] :> Total[Mom /@ {labels}]]];
  right = feynmanMomentumTerms[Expand[q //. Mom[Multiparticle[labels___]] :> Total[Mom /@ {labels}]]];
  If[!ListQ[left] || !ListQ[right], Return[feynmanFailure["UnsupportedMomentum", "Expected a linear combination of momenta."]]];
  Total[Flatten[Table[a[[1]] b[[1]] MomProd[a[[2, 1]], b[[2, 1]]],
    {a, left}, {b, right}]]]
];

feynmanSlashWord[p_] := Module[{terms = feynmanMomentumTerms[p]},
  If[!ListQ[terms], Return[feynmanFailure["UnsupportedMomentum", "Expected a linear slash momentum."]]];
  Total[(#[[1]] DiracWord[DiracSlash[#[[2]]]] &) /@ terms]
];

feynmanNormalizeWords[expr_] := expr /. word_DiracWord :>
  Fold[feynmanMultiply, DiracWord[],
    (If[MatchQ[#, _DiracSlash], feynmanSlashWord[#[[1]]], DiracWord[#]] &) /@ List @@ word];

feynmanRelabelVertex[expr_, ordered_List] := Module[{momenta, indices, result},
  (* CalcHEP vertex p_i points outwards; stored HEPCAT leg momenta point inwards.
     Propagator momentum flow is separate and is not reversed here. *)
  momenta = -Lookup[ordered, "Momentum"];
  indices = Lookup[ordered, "LorentzIndices"];
  If[!AllTrue[Cases[expr, _LorentzIndex, Infinity],
      MatchQ[#, LorentzIndex[_Integer] | LorentzIndex[_Integer, 2]] &&
        1 <= #[[1]] <= Length[ordered] && Length[indices[[#[[1]]]]] >= If[Length[#] === 2, 2, 1] &] ||
      !AllTrue[Flatten[Cases[expr, Mom[i_Integer] :> {i}, Infinity]], 1 <= # <= Length[ordered] &] ||
      !AllTrue[Flatten[Cases[expr, MomProd[i_Integer, j_Integer] :> {i, j}, Infinity]],
        1 <= # <= Length[ordered] &],
    Return[feynmanFailure["InvalidVertexSlots", "Vertex momentum or Lorentz slots do not match the attached fields."]]];
  result = expr /. {
    LorentzIndex[i_Integer, 2] :> indices[[i, 2]],
    LorentzIndex[i_Integer] :> indices[[i, 1]],
    Mom[i_Integer] :> momenta[[i]],
    MomProd[i_Integer, j_Integer] :> feynmanMomentumDot[momenta[[i]], momenta[[j]]]
  };
  result = result /. LorentzMomentum[p_, index_] :> With[{terms = feynmanMomentumTerms[p]},
    If[ListQ[terms], Total[(#[[1]] LorentzMomentum[#[[2]], index] &) /@ terms],
      feynmanFailure["UnsupportedMomentum", "Expected a linear tensor momentum."]]];
  result = result /. LorentzScalarProduct[p_, q_] :> feynmanMomentumDot[p, q];
  feynmanNormalizeWords[result]
];

feynmanVertexColor[ordered_List, md_Association] := Module[{colored, dimensions, indices, anti, fund, adj},
  colored = Select[ordered, Lookup[md["Color"], #["Particle"], 1] =!= 1 &];
  dimensions = Lookup[md["Color"], Lookup[colored, "Particle"]];
  indices = Lookup[colored, "ColorIndex"];
  Which[
    colored === {}, 1,
    Length[colored] === 2 && dimensions === {8, 8}, ColorDelta @@ indices,
    Length[colored] === 2 && Sort[dimensions] === {-3, 3}, ColorDelta @@ indices,
    Length[colored] === 3 && dimensions === {8, 8, 8}, I (ColorStructure @@ indices),
    Length[colored] === 3 && Sort[dimensions] === {-3, 3, 8},
      anti = First[FirstPosition[dimensions, -3]];
      fund = First[FirstPosition[dimensions, 3]];
      adj = First[FirstPosition[dimensions, 8]];
      ColorGenerator[indices[[anti]], indices[[fund]], indices[[adj]]],
    True, feynmanFailure["UnsupportedColorVertex", "The color representation of this vertex is not implemented.",
      <|"Particles" -> Lookup[colored, "Particle"], "Representations" -> dimensions|>]
  ]
];

feynmanVertexStructures[vertex_, legs_List, md_Association] := Module[
  {rows, output = {}, matches, ordered, expression, color, fermions},
  rows = DeleteDuplicatesBy[Lookup[md["BySorted"], Key[Sort[Lookup[legs, "Particle"]]], {}], #["Row"] &];
  Do[
    matches = vertexRowMatches[legs, row["Particles"]];
    If[matches === {}, Continue[]];
    (* The source is already a differentiated, Bose-symmetric vertex, not an
       interaction monomial requiring another factorial of permutations. *)
    ordered = First[matches];
    fermions = Select[ordered, Lookup[md["Spin2"], #["Particle"], 0] === 1 &];
    If[!MemberQ[{0, 2}, Length[fermions]], Return[feynmanFailure[
      "UnsupportedFermionVertex", "Each supported Feynman vertex has zero or two ordered Dirac fermion ports."]]];
    If[Length[fermions] === 2 && (Lookup[md["FermionRole"], fermions[[1, "Particle"]], None] =!= "Bar" ||
        Lookup[md["FermionRole"], fermions[[2, "Particle"]], None] =!= "Ket"),
      Return[feynmanFailure["UnsupportedFermionOrdering", "CalcHEP fermion vertices must be written with the antifermion port before the fermion port."]]];
    color = feynmanVertexColor[ordered, md];
    If[FailureQ[color], Return[color]];
    expression = Expand[I row["Row"][[-2]] color feynmanRelabelVertex[row["Row"][[-1]], ordered]];
    If[!FreeQ[expression, _Failure], Return[First[Cases[expression, _Failure, {0, Infinity}]]]];
    If[fermions === {} && !FreeQ[expression, _DiracWord], Return[feynmanFailure[
      "InvalidBosonicVertex", "A Dirac word occurs in a vertex without fermion ports."]]];
    AppendTo[output, <|"Vertex" -> vertex, "IncidentLegs" -> ordered,
      "ModelRow" -> row["Row"], "Expression" -> expression,
      "FermionLegs" -> fermions, "InternalHelicities" -> <||>|>], {row, rows}];
  output
];

feynmanAttachDiagram[raw_Association, ext_List, original_List, helicities_List, md_Association] := Module[
  {n = Length[ext], graph, extMap, incident, props, structures, edgeIndices = <||>,
   colorIndices = <||>, data, edge, mass, rank, inds, factor, name, mp, ends, labels,
   momentum, color, meta, one, values, externalMasses, externalSpins, polarizations = {}},
  graph = Graph[Join[Range[n], raw["InternalVertices"]], raw["Edges"]];
  extMap = AssociationThread[Range[n], tagParticles[original]];
  props = internalPropagatorData[raw["EdgeLabels"], raw["EdgeIndices"], raw["EdgeOrientation"], md];
  KeyValueMap[Function[{edge, data},
    mp = Apply[preferredInternalMultiparticle, Values[raw["EdgeIndices"][edge]]];
    AssociateTo[props, edge -> Join[data, <|"Index" -> mp|>]];
    AssociateTo[colorIndices, edge -> Unique["ConstructiveDiagrams`Private`color"]]], props];
  incident = AssociationMap[Function[v,
    Map[Function[leg,
      name = leg["Particle"]; mass = md["Mass"][name];
      rank = Which[Lookup[md["Aux"], name, ""] === "T", 2, md["Spin2"][name] === 2, 1, True, 0];
      inds = Table[Unique["ConstructiveDiagrams`Private`mu"], {rank}];
      momentum = If[leg["Kind"] === "External", Mom[leg["Index"]],
        data = props[leg["Source"]];
        If[leg["Index"] === data["Index"], Mom[data["Index"]], -Mom[data["Index"]]]];
      color = If[leg["Kind"] === "External", ColorIndex[leg["Index"]], colorIndices[leg["Source"]]];
      Join[leg, <|"LorentzIndices" -> inds, "Momentum" -> momentum, "ColorIndex" -> color|>]],
      vertexIncidentLegData[v, AdjacencyList[graph, v],
        AssociationThread[Range[n], tagParticles[ext]], raw["EdgeLabels"], raw["EdgeIndices"], Range[n]]]
  ], raw["InternalVertices"]];
  structures = AssociationMap[feynmanVertexStructures[#, incident[#], md] &, raw["InternalVertices"]];
  values = Values[structures];
  If[AnyTrue[values, FailureQ], Return[First[Select[values, FailureQ]]]];
  If[AnyTrue[values, # === {} &], Return[feynmanFailure["MissingFeynmanVertex", "A generated topology has no matching Feynman rule."]]];
  Do[
    data = props[edge]; ends = List @@ edge;
    inds = Table[SelectFirst[incident[v], #["Kind"] === "Internal" && #["Source"] === edge &]["LorentzIndices"], {v, ends}];
    mass = data["Mass"]; name = data["Particle"]; mp = data["Index"];
    factor = Which[
      Lookup[md["Aux"], name, ""] === "T",
        -I LorentzMetric[inds[[1, 1]], inds[[2, 1]]] LorentzMetric[inds[[1, 2]], inds[[2, 2]]],
      data["Spin2"] === 2,
        -I (LorentzMetric[inds[[1, 1]], inds[[2, 1]]] - If[mass === 0, 0,
          LorentzMomentum[Mom[mp], inds[[1, 1]]] LorentzMomentum[Mom[mp], inds[[2, 1]]]/mass^2])/
          PropDen[Mom[mp], mass],
      MemberQ[{0, 1}, data["Spin2"]], I/PropDen[Mom[mp], mass],
      True, feynmanFailure["UnsupportedFeynmanPropagator", "Unsupported internal spin."]];
    AssociateTo[props, edge -> Join[data, <|"Factor" -> factor,
      "Auxiliary" -> (Lookup[md["Aux"], name, ""] === "T")|>]], {edge, Keys[props]}];
  Do[Do[
    If[leg["Kind"] === "External" && md["Spin2"][leg["Particle"]] === 2,
      AppendTo[polarizations, PolarizationVector[leg["Index"], First[leg["LorentzIndices"]],
        md["Mass"][leg["Particle"]], helicities[[leg["Index"]]]]]], {leg, incident[v]}],
    {v, raw["InternalVertices"]}];
  externalMasses = Join[md["Mass"], AssociationThread[original, Lookup[md["Mass"], ext]]];
  externalSpins = Join[md["Spin2"], AssociationThread[original, Lookup[md["Spin2"], ext]]];
  <|"Graph" -> graph, "AmplitudeType" -> "Feynman", "Gauge" -> "Unitary",
    "ExternalParticles" -> original, "CanonicalExternalParticles" -> ext,
    "ExternalAssignment" -> extMap, "Spin2" -> externalSpins,
    "Masses" -> externalMasses, "FermionRole" -> md["FermionRole"], "Handedness" -> md["Handedness"],
    "InternalVertices" -> raw["InternalVertices"], "LeafVertices" -> Range[n],
    "InternalEdgeData" -> Association@KeyValueMap[Function[{e, l},
      e -> Join[raw["EdgeOrientation"][e], <|"EndpointLabels" -> l|>]], raw["EdgeLabels"]],
    "InternalEdgeLabels" -> raw["EdgeLabels"], "InternalEdgeOrientation" -> raw["EdgeOrientation"],
    "InternalEdgeIndices" -> raw["EdgeIndices"], "IncidentLegData" -> incident,
    "VertexStructures" -> structures, "Propagators" -> props,
    "Polarizations" -> polarizations, "HelicitySumEdges" -> {}, "HelicityAssignments" -> {<||>}|>
];

feynmanCreateDiagrams[original_List, model_FeynmanModel] := Module[
  {physical, md, external, ext, helicities, raw, diagrams, particles},
  physical = feynmanPhysicalModel[model]; If[FailureQ[physical], Return[physical]];
  particles = physical[[1]];
  If[particles === {}, Return[feynmanFailure["EmptyParticleTable", "The Feynman model has no particle declarations."]]];
  If[physical[[2]] === {},
    If[AllTrue[original, !MissingQ[feynmanExternalParticle[#, Flatten[particles[[All, {2, 3}]]],
        Association[Flatten[({#[[2]] -> #[[5]], #[[3]] -> #[[5]]} &) /@ particles]]]] &],
      Return[{}], Return[feynmanFailure["UnknownExternalParticle", "An external particle is absent from this Feynman model."]]]];
  md = buildModelData[physical];
  AssociateTo[md, "Color" -> Association@Flatten[
    If[#[[8]] === 3, {#[[2]] -> 3, #[[3]] -> -3}, {#[[2]] -> #[[8]], #[[3]] -> #[[8]]}] & /@ particles]];
  AssociateTo[md, "Aux" -> Association@Flatten[({#[[2]] -> #[[9]], #[[3]] -> #[[9]]} &) /@ particles]];
  AssociateTo[md, "Handedness" -> Association@Flatten[
    ({#[[2]] -> If[MemberQ[{"L", "R"}, #[[9]]], #[[9]], "Dirac"],
      #[[3]] -> If[MemberQ[{"L", "R"}, #[[9]]], #[[9]], "Dirac"]} &) /@ particles]];
  AssociateTo[md, "FermionRole" -> Association@Flatten[
    ({#[[2]] -> "Ket", #[[3]] -> "Bar"} &) /@ Select[particles, #[[5]] === 1 &]]];
  external = feynmanExternalParticle[#, md["Names"], md["Spin2"]] & /@ original;
  If[AnyTrue[external, MissingQ], Return[feynmanFailure["UnknownExternalParticle", "An external particle is absent from this Feynman model."]]];
  ext = First /@ external; helicities = Last /@ external;
  If[AnyTrue[ext, Lookup[md["Aux"], #, ""] === "T" &],
    Return[feynmanFailure["UnphysicalExternalParticle", "The contact auxiliary is not a physical external state."]]];
  If[AnyTrue[Range[Length[ext]], md["Mass"][ext[[#]]] === 0 && helicities[[#]] === 0 &],
    Return[feynmanFailure["InvalidVectorHelicity", "A massless vector has no longitudinal external state."]]];
  raw = generateRawDiagrams[ext, md];
  diagrams = feynmanAttachDiagram[#, ext, original, helicities, md] & /@ raw;
  If[AnyTrue[diagrams, FailureQ], First[Select[diagrams, FailureQ]], diagrams]
];

feynmanFermionPairSign[lines_List] := If[lines === {}, 1,
  Signature[Ordering[Last /@ SortBy[lines, First]]]];

feynmanFermionPaths[diag_Association] := Module[{edges, graph, ends, bars, paths, line},
  edges = Select[EdgeList[diag["Graph"]], Function[edge,
    With[{v = List @@ edge}, If[Min[v] <= Length[diag["ExternalParticles"]],
      Lookup[diag["Spin2"], diag["ExternalParticles"][[Min[v]]], 0] === 1,
      Lookup[diag["Propagators"][edge], "Spin2", 0] === 1]]]];
  If[edges === {}, Return[{}]];
  graph = Graph[edges];
  paths = Table[
    ends = Intersection[component, diag["LeafVertices"]];
    If[Length[ends] =!= 2, Return[feynmanFailure["InvalidFermionFlow", "An open tree fermion line must have two external endpoints."]]];
    bars = Select[ends, Lookup[diag["FermionRole"], diag["CanonicalExternalParticles"][[#]], None] === "Bar" &];
    If[Length[bars] =!= 1, Return[feynmanFailure["InvalidFermionFlow", "A supported Dirac line must join an antifermion to a fermion."]]];
    FindShortestPath[graph, First[bars], First[Complement[ends, bars]]],
    {component, ConnectedComponents[graph]}];
  SortBy[paths, First]
];

feynmanChainPolynomial[expression_, bar_, ket_] := Module[{terms, parts},
  terms = If[Head[expression] === Plus, List @@ expression, {expression}];
  Total[Map[Function[term,
    parts = feynmanWordParts[term];
    If[FailureQ[parts], parts, parts[[1]] DiracChain[bar, DiracWord @@ parts[[2]], ket]]], terms]]
];

feynmanDiagramAmplitude[diag_Association] := Module[
  {paths, word, vertices, edge, leg, momentum, mass, expression, chains = {},
   visited = {}, bar, ket, role, factors, pairs},
  paths = feynmanFermionPaths[diag]; If[FailureQ[paths], Return[paths]];
  Do[
    vertices = path[[2 ;; -2]]; word = DiracWord[];
    Do[
      expression = Total[Lookup[diag["VertexStructures"][vertices[[index]]], "Expression"]];
      word = feynmanMultiply[word, expression];
      AppendTo[visited, vertices[[index]]];
      If[index < Length[vertices],
        edge = UndirectedEdge @@ Sort[vertices[[{index, index + 1}]]];
        leg = SelectFirst[First[diag["VertexStructures"][vertices[[index]]]]["FermionLegs"],
          #["Kind"] === "Internal" && #["Source"] === edge &];
        If[Lookup[diag["FermionRole"], leg["Particle"], None] =!= "Ket",
          Return[feynmanFailure["InconsistentFermionFlow", "A fermion line changes its Dirac orientation."]]];
        momentum = leg["Momentum"]; mass = diag["Propagators"][edge]["Mass"];
        word = feynmanMultiply[word, feynmanSlashWord[momentum] + mass DiracWord[]]],
      {index, Length[vertices]}];
    bar = First[path]; ket = Last[path];
    AppendTo[chains, feynmanChainPolynomial[word,
      FermionBar[bar, diag["Masses"][diag["ExternalParticles"][[bar]]],
        diag["Handedness"][diag["CanonicalExternalParticles"][[bar]]]],
      FermionKet[ket, diag["Masses"][diag["ExternalParticles"][[ket]]],
        diag["Handedness"][diag["CanonicalExternalParticles"][[ket]]]]]], {path, paths}];
  factors = Total[Lookup[diag["VertexStructures"][#], "Expression"]] & /@
    Complement[diag["InternalVertices"], visited];
  pairs = ({First[#], Last[#]} &) /@ paths;
  expression = feynmanFermionPairSign[pairs] (Times @@ Join[chains, factors,
    (#["Factor"] & /@ Values[diag["Propagators"]]), diag["Polarizations"]])/I;
  If[!FreeQ[expression, _Failure], First[Cases[expression, _Failure, {0, Infinity}]],
    expression /. Thread[(Mass /@ diag["LeafVertices"]) ->
      Lookup[diag["Masses"], diag["ExternalParticles"]]]]
];

End[];

(* ::Subsection::Closed:: *)
(*Feynman amplitudes to spinor products*)

FeynmanToSpinorProducts::usage = "FeynmanToSpinorProducts[amplitude] converts contracted tree-level Feynman amplitudes to HEPCAT spinor chains without internal on-shell assumptions. It also accepts a {massRules, amplitude} pair or a list of pairs. The all-ingoing endpoint convention is u=(-|p>,|p]) and bar v=(<p|,-[p|), with gamma5=diag(-1,+1). The string option FermionHelicities supplies helicities for massless Dirac fields; ReferenceSpinors supplies leg -> reference rules, with automatic independent Xi references otherwise. Massive vectors retain explicit symmetric VectorSpinIndex labels. Unsupported or uncontracted tensors return Failure, not a partial answer.";
VectorSpinIndex::usage = "VectorSpinIndex[leg, slot] labels one of the two symmetric upper little-group indices of a converted massive-vector wavefunction.";
Options[FeynmanToSpinorProducts] = {"FermionHelicities" -> {}, "ReferenceSpinors" -> Automatic,
  "ReduceSpinorProducts" -> True, "MaxExpandedTerms" -> 100000, "TimeConstraint" -> 60};

Begin["ConstructiveDiagrams`Private`"];

Clear[fwBilinear, fwTrace, fwMomentum, fwSigma, fwEpsilon];
feynmanRawExpressionQ[expression_] := !FreeQ[expression,
  _DiracChain | _DiracWord | _DiracGamma | _DiracSlash | DiracGamma5 |
    _FermionBar | _FermionKet | _PolarizationVector | _LorentzMetric |
    _LorentzMomentum | _LorentzScalarProduct];
feynmanExternalMassRules[rules_List] := Select[rules,
  MatchQ[#, HoldPattern[Mass[_Integer] -> _]] &];
fwType[s_Spinor] := s[[If[Length[s] === 5, 3, 2]]];
fwFailure[reason_, data_: <||>] := Throw[Failure["FeynmanToSpinorProducts", Join[
  <|"MessageTemplate" -> reason|>, data]], fwConversionTag];

fwNormalizeMatrices[matrices_List] := Module[{list = matrices, sign = 1, at},
  While[(at = FirstPosition[Partition[list, 2, 1], {fwEpsilon, fwEpsilon}, Missing["NotFound"]]) =!= Missing["NotFound"],
    list = Delete[list, {{First[at]}, {First[at] + 1}}]; sign = -sign];
  {sign, list}
];

fwTransposeMatrices[matrices_List] := Module[{result},
  result = Flatten[Reverse[matrices] /. {
    fwMomentum[p_, type_] :> {fwEpsilon, fwMomentum[p, 1 - type], fwEpsilon},
    fwSigma[mu_, type_] :> {fwEpsilon, fwSigma[mu, 1 - type], fwEpsilon}}];
  result = fwNormalizeMatrices[result];
  {(-1)^Length[matrices] First[result], Last[result]}
];

fwB[left_Spinor, matrices_List, right_Spinor] := With[{reduced = fwNormalizeMatrices[matrices]},
  First[reduced] fwBilinear[left, Last[reduced], right]];
fwT[matrices_List] := With[{reduced = fwNormalizeMatrices[matrices]},
  First[reduced] If[Last[reduced] === {}, 2, fwTrace[Last[reduced]]]];

fwSpinor[kind_, type_, leg_, spin_: None] := If[spin === None,
  Spinor[kind, type, leg], Spinor[kind, "Upper", type, leg, spin]];

fwEndpointChoices[head_, leg_, mass_, hand_, helicities_Association] := Module[
  {bar = head === FermionBar, selected = hand, helicity, types, kind},
  kind = If[TrueQ[mass === 0], "Helicity", "Spin"];
  If[kind === "Helicity",
    helicity = Lookup[helicities, leg, Automatic];
    If[helicity =!= Automatic,
      If[!MemberQ[{-1/2, 1/2}, helicity], fwFailure["A massless fermion helicity must be -1/2 or +1/2.", <|"Leg" -> leg|>]];
      selected = If[bar, If[helicity === 1/2, "L", "R"], If[helicity === -1/2, "L", "R"]]];
    If[!MemberQ[{"L", "R"}, selected],
      fwFailure["Specify the helicity of each massless Dirac external fermion using FermionHelicities.", <|"Leg" -> leg|>]]];
  types = Switch[selected, "Dirac", {0, 1}, "L", If[bar, {1}, {0}], "R", If[bar, {0}, {1}],
    _, fwFailure["An external fermion has an unsupported chirality tag.", <|"Leg" -> leg, "Hand" -> hand|>]];
  Table[<|"Block" -> block, "Spinor" -> fwSpinor[kind, If[block === 0, "Angle", "Square"], leg],
    "Coefficient" -> If[bar, If[block === 0, 1, -1], If[block === 0, -1, 1]],
    "Matrices" -> If[(bar && block === 0) || (!bar && block === 1), {fwEpsilon}, {}],
    "BoundarySign" -> If[!bar && block === 1, -1, 1]|>, {block, types}]
];

fwConvertDiracChain[DiracChain[FermionBar[left_, lm_, lh_], DiracWord[operators___],
    FermionKet[right_, rm_, rh_]], helicities_Association] := Module[
  {lefts, rights, out = {}, block, sign, matrices, entries = {operators}},
  lefts = fwEndpointChoices[FermionBar, left, lm, lh, helicities];
  rights = fwEndpointChoices[FermionKet, right, rm, rh, helicities];
  Do[
    block = l["Block"]; sign = l["Coefficient"] l["BoundarySign"];
    matrices = l["Matrices"];
    Do[Which[
      MatchQ[operator, DiracGamma[_]], AppendTo[matrices, fwSigma[First[operator], block]]; block = 1 - block,
      MatchQ[operator, DiracSlash[_]], AppendTo[matrices, fwMomentum[First[operator], block]]; block = 1 - block,
      operator === DiracGamma5, sign *= If[block === 0, -1, 1],
      operator === 1, Null,
      True, fwFailure["An ordered Dirac word contains an unsupported operator.", <|"Operator" -> operator|>]],
      {operator, entries}];
    Do[If[r["Block"] === block,
      AppendTo[out, sign r["Coefficient"] r["BoundarySign"] fwB[
        l["Spinor"], Join[matrices, r["Matrices"]], r["Spinor"]]]], {r, rights}],
    {l, lefts}];
  Total[out]
];
fwConvertDiracChain[other_, _] := fwFailure["Unsupported external endpoints or Dirac-chain structure.", <|"Chain" -> other|>];

fwReference[leg_, type_, references_] := Module[{choice},
  choice = If[references === Automatic, Automatic, Lookup[references, leg, Automatic]];
  Which[choice === Automatic, Spinor["Xi", type, leg],
    MatchQ[choice, _Spinor], choice /. {"Angle" -> type, "Square" -> type},
    IntegerQ[choice] && choice =!= leg, Spinor["Helicity", type, choice],
    True, fwFailure["ReferenceSpinors entries must be another massless leg or an explicit Spinor.", <|"Leg" -> leg|>]]
];

fwConvertPolarization[PolarizationVector[leg_, mu_, 0, helicity_], references_] := Module[
  {type, other, reference, particle, denominator, sign},
  If[!MemberQ[{-1, 1}, helicity], fwFailure["Specify +1 or -1 for each massless-vector helicity.", <|"Leg" -> leg|>]];
  type = If[helicity === 1, "Angle", "Square"]; other = If[type === "Angle", "Square", "Angle"];
  reference = fwReference[leg, type, references];
  particle = Spinor["Helicity", other, leg];
  denominator = SpinorChain[reference, Spinor["Helicity", type, leg]];
  If[TrueQ[denominator === 0], fwFailure["A polarization reference gives an identically zero denominator.", <|"Leg" -> leg|>]];
  (* These phases match both HEPCAT's x/xt vertices and its massive-vector
     high-energy limit. Each physical polarization has norm -1. *)
  sign = -1;
  sign fwB[reference, Join[If[type === "Angle", {fwEpsilon}, {}],
    {fwSigma[mu, If[type === "Angle", 0, 1]]}, If[other === "Square", {fwEpsilon}, {}]], particle]
    If[other === "Square", -1, 1]/(Sqrt[2] denominator)
];
fwConvertPolarization[PolarizationVector[leg_, mu_, mass_, Automatic], _] /; mass =!= 0 := Module[
  {first = VectorSpinIndex[leg, 1], second = VectorSpinIndex[leg, 2]},
  Total[Table[fwB[fwSpinor["Spin", "Angle", leg, pair[[1]]],
    {fwEpsilon, fwSigma[mu, 0], fwEpsilon}, fwSpinor["Spin", "Square", leg, pair[[2]]]],
    {pair, {{first, second}, {second, first}}}]]/(2 Sqrt[2] mass)
];
fwConvertPolarization[other_, _] := fwFailure["Unsupported vector polarization. Massive vectors use Automatic spin states.", <|"Polarization" -> other|>];

fwFactors[term_] := Flatten[(If[MatchQ[#, Power[_fwBilinear | _fwTrace | _LorentzMomentum | _LorentzMetric, _Integer?Positive]],
    ConstantArray[First[#], Last[#]], {#}] &) /@ If[Head[term] === Times, List @@ term, {term}], 1];
fwMomentumTerms[Mom[Multiparticle[parts___]]] := Flatten[fwMomentumTerms /@ (Mom /@ {parts}), 1];
fwMomentumTerms[Mom[leg_]] := {{1, leg}};
fwMomentumTerms[sum_Plus] := Flatten[fwMomentumTerms /@ List @@ sum, 1];
fwMomentumTerms[product_Times] := Module[{parts = List @@ product, moments, coefficient},
  moments = Select[parts, !FreeQ[#, _Mom] &];
  If[Length[moments] =!= 1, fwFailure["Unsupported momentum polynomial.", <|"Momentum" -> product|>]];
  coefficient = Times @@ Select[parts, FreeQ[#, _Mom] &];
  ({coefficient First[#], Last[#]} &) /@ fwMomentumTerms[First[moments]]
];
fwMomentumTerms[other_] := fwFailure["Unsupported Lorentz momentum.", <|"Momentum" -> other|>];
fwMomentumDot[left_, right_] := Total[Flatten[Outer[
  Function[{a, b}, First[a] First[b] MomProd[Last[a], Last[b]]],
  fwMomentumTerms[left], fwMomentumTerms[right], 1]]];

fwRelabelLorentz[expression_, old_, new_] := expression /. {
  LorentzMetric[a_, b_] :> LorentzMetric[If[a === old, new, a], If[b === old, new, b]],
  LorentzMomentum[p_, a_] :> LorentzMomentum[p, If[a === old, new, a]],
  fwSigma[a_, type_] :> fwSigma[If[a === old, new, a], type]};

fwLorentzIndices[expression_] := Flatten[Cases[expression,
  tensor : (_LorentzMetric | _LorentzMomentum | _fwSigma) :> Switch[Head[tensor],
    LorentzMetric, List @@ tensor, LorentzMomentum, {Last[tensor]}, fwSigma, {First[tensor]}], {0, Infinity}], 1];

fwSigmaLocations[factors_List] := Flatten[MapIndexed[Function[{factor, index},
  Which[Head[factor] === fwBilinear,
    MapIndexed[If[Head[#1] === fwSigma, {First[index], First[#2], #1}, Nothing] &, factor[[2]]],
    Head[factor] === fwTrace,
    MapIndexed[If[Head[#1] === fwSigma, {First[index], First[#2], #1}, Nothing] &, factor[[1]]],
    True, {}]], factors], 1];

fwContractPair[factors_List, first_List, second_List] := Module[
  {i = first[[1]], j = second[[1]], at = first[[2]], bt = second[[2]],
   same = first[[3, 2]] === second[[3, 2]], a, b, p, q, r, s, transposed, result, rest},
  a = factors[[i]]; b = factors[[j]];
  If[i === j,
    If[at > bt, {at, bt} = {bt, at}];
    If[Head[a] === fwBilinear,
      p = Take[a[[2]], at - 1]; q = Take[a[[2]], {at + 1, bt - 1}]; s = Drop[a[[2]], bt];
      If[same, transposed = fwTransposeMatrices[q];
        result = 2 First[transposed] fwB[a[[1]], Join[p, {fwEpsilon}, Last[transposed], {fwEpsilon}, s], a[[3]]],
        result = 2 fwT[q] fwB[a[[1]], Join[p, s], a[[3]]]],
      p = Take[a[[1]], at - 1]; q = Take[a[[1]], {at + 1, bt - 1}]; s = Drop[a[[1]], bt];
      If[same, transposed = fwTransposeMatrices[q];
        result = 2 First[transposed] fwT[Join[p, {fwEpsilon}, Last[transposed], {fwEpsilon}, s]],
        result = 2 fwT[q] fwT[Join[p, s]]]];
    rest = Delete[factors, i],
    If[Head[a] === fwTrace && Head[b] === fwBilinear,
      {a, b} = {b, a}; {at, bt} = {bt, at}];
    Which[
      Head[a] === fwBilinear && Head[b] === fwBilinear,
        p = Take[a[[2]], at - 1]; q = Drop[a[[2]], at];
        r = Take[b[[2]], bt - 1]; s = Drop[b[[2]], bt];
        If[same,
          transposed = fwTransposeMatrices[r];
          result = 2 First[transposed] fwB[a[[1]], Join[p, {fwEpsilon}, Last[transposed]], b[[1]]];
          transposed = fwTransposeMatrices[q];
          result *= First[transposed] fwB[a[[3]], Join[Last[transposed], {fwEpsilon}, s], b[[3]]],
          result = 2 fwB[a[[1]], Join[p, s], b[[3]]] fwB[b[[1]], Join[r, q], a[[3]]]],
      Head[a] === fwBilinear && Head[b] === fwTrace,
        p = Take[a[[2]], at - 1]; q = Drop[a[[2]], at];
        r = Join[Drop[b[[1]], bt], Take[b[[1]], bt - 1]];
        If[same, transposed = fwTransposeMatrices[r];
          result = -2 First[transposed] fwB[a[[1]], Join[p, {fwEpsilon}, Last[transposed], {fwEpsilon}, q], a[[3]]],
          result = 2 fwB[a[[1]], Join[p, r, q], a[[3]]]],
      Head[a] === fwTrace && Head[b] === fwTrace,
        p = Join[Drop[a[[1]], at], Take[a[[1]], at - 1]];
        r = Join[Drop[b[[1]], bt], Take[b[[1]], bt - 1]];
        If[same, transposed = fwTransposeMatrices[r];
          result = -2 First[transposed] fwT[Join[p, {fwEpsilon}, Last[transposed], {fwEpsilon}]],
          result = 2 fwT[Join[p, r]]],
      True, fwFailure["Unsupported Weyl contraction."]];
    rest = Delete[factors, List /@ Sort[{i, j}]]];
  Expand[(Times @@ rest) result]
];

fwContractTerm[term_] := Module[{factors = fwFactors[term], metric, mu, nu, rest, locations,
    momentum, hits, first, second, bilinear, matrices, index, counts},
  counts = Tally[fwLorentzIndices[Times @@ factors]];
  If[AnyTrue[counts, Last[#] =!= 2 &],
    fwFailure["Each Lorentz index must be contracted exactly twice.",
      <|"IndexCounts" -> Select[counts, Last[#] =!= 2 &]|>]];
  metric = FirstPosition[factors, _LorentzMetric, Missing["NotFound"], {1}];
  If[!MissingQ[metric],
    {mu, nu} = List @@ factors[[First[metric]]]; rest = Delete[factors, First[metric]];
    Return[fwContract[If[mu === nu, 4 (Times @@ rest), fwRelabelLorentz[Times @@ rest, mu, nu]]]]];
  locations = fwSigmaLocations[factors];
  momentum = FirstPosition[factors, _LorentzMomentum, Missing["NotFound"], {1}];
  If[!MissingQ[momentum],
    {momentum, mu} = List @@ factors[[First[momentum]]];
    hits = Select[locations, #[[3, 1]] === mu &];
    If[Length[hits] === 1,
      first = First[hits]; index = first[[1]]; bilinear = factors[[index]];
      If[Head[bilinear] === fwBilinear,
        matrices = ReplacePart[bilinear[[2]], first[[2]] -> fwMomentum[momentum, first[[3, 2]]]];
        factors[[index]] = fwB[bilinear[[1]], matrices, bilinear[[3]]],
        matrices = ReplacePart[bilinear[[1]], first[[2]] -> fwMomentum[momentum, first[[3, 2]]]];
        factors[[index]] = fwT[matrices]];
      hits = FirstPosition[factors, LorentzMomentum[momentum, mu]];
      Return[fwContract[Times @@ Delete[factors, First[hits]]]]];
    hits = Position[factors, LorentzMomentum[_, mu], {1}];
    If[Length[hits] === 2,
      {first, second} = factors[[Flatten[hits]]];
      Return[fwContract[fwMomentumDot[First[first], First[second]] (Times @@ Delete[factors, hits])]]];
    fwFailure["A Lorentz momentum index is not contracted exactly twice.", <|"Index" -> mu|>]];
  If[locations === {}, Return[Times @@ factors]];
  first = First[locations]; mu = first[[3, 1]];
  hits = Select[locations, #[[3, 1]] === mu &];
  If[Length[hits] =!= 2,
    fwFailure["A Lorentz index is free or occurs more than twice.", <|"Index" -> mu|>]];
  fwContract[fwContractPair[factors, First[hits], Last[hits]]]
];
fwContract[expression_] := Module[{expanded = Expand[expression], terms},
  terms = If[Head[expanded] === Plus, List @@ expanded, {expanded}];
  If[Length[terms] > fwTermLimit, fwFailure["The exact Lorentz expansion exceeded MaxExpandedTerms.", <|"Terms" -> Length[terms]|>]];
  Total[fwContractTerm /@ terms]
];

fwFinishBilinear[fwBilinear[left_, matrices_List, right_]] := Module[
  {list = matrices, sign = 1, types, expected, momenta},
  If[fwType[left] === "Angle",
    If[list === {} || First[list] =!= fwEpsilon, fwFailure["An undotted Weyl boundary did not contract."]];
    list = Rest[list]];
  If[fwType[right] === "Square",
    If[list === {} || Last[list] =!= fwEpsilon, fwFailure["A dotted Weyl boundary did not contract."]];
    list = Most[list]; sign = -sign];
  If[!AllTrue[list, MatchQ[#, fwMomentum[_, 0 | 1]] &],
    fwFailure["An unsupported Weyl matrix remains after Lorentz contraction.", <|"Matrices" -> list|>]];
  types = If[list === {}, {}, list[[All, 2]]];
  expected = Table[Mod[If[fwType[left] === "Angle", 0, 1] + i - 1, 2], {i, Length[list]}];
  If[types =!= expected || (fwType[left] === fwType[right]) =!= EvenQ[Length[list]],
    fwFailure["The contracted Weyl chain has inconsistent chirality."]];
  momenta = If[list === {}, {}, list[[All, 1]]];
  sign (SpinorChain @@ Join[{left}, momenta, {right}])
];
fwFinishTrace[fwTrace[matrices_List]] := Module[{list = matrices, types},
  If[!AllTrue[list, MatchQ[#, fwMomentum[_, 0 | 1]] &] || OddQ[Length[list]],
    fwFailure["An unsupported Weyl trace remains after Lorentz contraction.", <|"Matrices" -> list|>]];
  If[list === {}, Return[2]];
  If[list[[1, 2]] === 1, list = RotateLeft[list]];
  types = list[[All, 2]];
  If[types =!= Table[Mod[i - 1, 2], {i, Length[list]}], fwFailure["A Weyl trace has inconsistent chirality."]];
  SpinorTrace @@ list[[All, 1]]
];

fwConvertExpression[expression_, helicities_Association, references_, reduce_] := Module[{converted, externalRules},
  externalRules = DeleteDuplicates[Cases[expression,
    FermionBar[leg_, mass_, _] | FermionKet[leg_, mass_, _] | PolarizationVector[leg_, _, mass_, _] :>
      (Mass[leg] -> mass), Infinity]];
  converted = expression /. dot_LorentzScalarProduct :> fwMomentumDot @@ (List @@ dot);
  converted = converted /. {
    chain_DiracChain :> fwConvertDiracChain[chain, helicities],
    vector_PolarizationVector :> fwConvertPolarization[vector, references]};
  If[!FreeQ[converted, _DiracChain | _DiracWord | _DiracGamma | _DiracSlash | DiracGamma5 | _PolarizationVector],
    fwFailure["Unsupported Dirac or polarization structure remains in the amplitude."]];
  converted = fwContract[converted] /. {b_fwBilinear :> fwFinishBilinear[b], t_fwTrace :> fwFinishTrace[t]};
  If[!FreeQ[converted, _LorentzMetric | _LorentzMomentum | _fwSigma | _fwMomentum | _fwBilinear | _fwTrace | fwEpsilon],
    fwFailure["Uncontracted Feynman tensors remain in the amplitude."]];
  If[TrueQ[reduce], ReduceSpinorProducts[converted] /. externalRules, converted]
];

FeynmanToSpinorProducts[input_, opts : OptionsPattern[]] := Catch[Module[
  {helicities, references, reduce, limit, seconds, unknownOptions, convert},
  unknownOptions = Complement[First /@ Flatten[{opts}], First /@ Options[FeynmanToSpinorProducts]];
  If[unknownOptions =!= {}, fwFailure["Unknown FeynmanToSpinorProducts options.", <|"Options" -> unknownOptions|>]];
  helicities = OptionValue["FermionHelicities"]; references = OptionValue["ReferenceSpinors"];
  reduce = OptionValue["ReduceSpinorProducts"]; limit = OptionValue["MaxExpandedTerms"];
  seconds = OptionValue["TimeConstraint"];
  If[!MemberQ[{True, False}, reduce], fwFailure["ReduceSpinorProducts must be True or False."]];
  If[!(seconds === Infinity || (NumberQ[seconds] && TrueQ[seconds > 0])),
    fwFailure["TimeConstraint must be a positive number or Infinity."]];
  If[!MatchQ[helicities, {(_Rule | _RuleDelayed) ...}] && !AssociationQ[helicities],
    fwFailure["FermionHelicities must be a rule list or association."]];
  If[references =!= Automatic && !MatchQ[references, {(_Rule | _RuleDelayed) ...}] && !AssociationQ[references],
    fwFailure["ReferenceSpinors must be Automatic, a rule list or an association."]];
  If[!IntegerQ[limit] || limit <= 0, fwFailure["MaxExpandedTerms must be a positive integer."]];
  helicities = Association[helicities]; If[references =!= Automatic, references = Association[references]];
  convert[value_] := Which[
    MatchQ[value, {{(_Rule | _RuleDelayed) ...}, _}], {First[value], fwConvertExpression[Last[value], helicities, references, reduce]},
    ListQ[value], convert /@ value,
    True, fwConvertExpression[value, helicities, references, reduce]];
  Block[{fwTermLimit = limit}, If[seconds === Infinity, convert[input],
    TimeConstrained[convert[input], seconds, Failure["FeynmanToSpinorProducts", <|
      "MessageTemplate" -> "The exact Feynman-to-spinor conversion exceeded TimeConstraint.",
      "TimeConstraint" -> seconds|>]]]]
], fwConversionTag];

End[];
