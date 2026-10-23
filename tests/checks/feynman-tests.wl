(* CalcHEP import, independent Feynman diagrams, and exact spinor conversion. *)
ffRoot = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{ffRoot, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
ffChecks = 0; ffFailures = {};
SetAttributes[ffCheck, HoldRest];
ffCheck[name_, assertion_] := Module[{result},
  ffChecks++;
  result = TimeConstrained[Check[assertion, $Failed], 30, $TimedOut];
  If[!TrueQ[result], AppendTo[ffFailures, name]; Print["FAIL: ", name]]
];
ffParse = ConstructiveDiagrams`Private`parseFeynmanExpression;
ffFiniteQ[expr_] := FreeQ[expr, _Failure | _Missing | Indeterminate |
  ComplexInfinity | DirectedInfinity | $Failed | $TimedOut];
ffTemporaryDirectory = CreateDirectory[];
ffOriginalStreams = Streams[];
ffWrite[name_, lines_List] := Export[FileNameJoin[{ffTemporaryDirectory, name}],
  StringRiffle[lines, "\n"] <> "\n", "Text"];

(* A complete tiny model keeps its light fermions massless consistently. *)
ffWrite["prtcls91.mdl", {"Feynman regression fixture", "Particles",
  "Full Name | P | aP | number | spin2 | mass | width | color | aux | LaTeX(A) | LaTeX(A+)",
  "electron | e | E | 11 | 1 | 0 | 0 | 1 | | e | E",
  "muon | m | M | 13 | 1 | 0 | 0 | 1 | | m | M",
  "photon | A | A | 22 | 2 | 0 | 0 | 1 | G | A | A",
  "scalar | h | h | 25 | 0 | ffScalarMass | 0 | 1 | | h | h",
  "vector | Z | Z | 23 | 2 | ffVectorMass | 0 | 1 | G | Z | Z"}];
ffWrite["vars91.mdl", {"Feynman regression fixture", "Name | Value | Comment",
  "ffCharge | 2/3 | charge", "ffScalarMass | 3 | scalar mass",
  "ffVectorMass | 5 | vector mass", "ffYukawa | 1/7 | scalar coupling",
  "ffTriple | 1/11 | cubic scalar coupling", "ffQuartic | 1/13 | quartic coupling"}];
ffWrite["func91.mdl", {"Feynman regression fixture", "Name | Expression",
  "ffDerived | 2*ffCharge^2 % derived fixture"}];
ffWrite["lgrng91.mdl", {"Feynman regression fixture", "Lagrangian",
  "P1 | P2 | P3 | P4 | Factor | Lorentz part",
  "E | e | A | | -ffCharge | G(m3)",
  "M | m | A | | -ffCharge | G(m3)",
  "E | e | h | | -ffYukawa | 1",
  "h | h | h | | -ffTriple | 1",
  "h | h | h | h | -ffQuartic | 1",
  "Z | Z | h | | ffVectorMass | m1.m2",
  "E | e | Z | | ffCharge | G(m3)*(1-G5)"}];
ffModel = ReadModel[ffTemporaryDirectory, 91];
ffCheck["ReadModel recognizes a raw EOF-terminated CalcHEP Lagrangian model",
  MatchQ[ffModel, FeynmanModel[_List, _List, _List, _List]] &&
    Length[ffModel[[1]]] === 5 && Length[ffModel[[2]]] === 7];
ffCheck["raw Feynman files are closed after reading", Streams[] === ffOriginalStreams];
ffCheck["Feynman model reading is idempotent", ReadModel[ffTemporaryDirectory, 91] === ffModel];
Do[
  ffCheck["model parameters remain scoped data without per-name symbols " <> ffSymbolName,
    MatchQ[ModelParameter[ffModel, ffSymbolName], ModelParameter[ffSymbolName, _String, "Real"]] &&
      Names["ConstructiveDiagrams`" <> ffSymbolName] === {}],
  {ffSymbolName, {"ffCharge", "ffScalarMass", "ffVectorMass", "ffDerived"}}];
ffChargeParameter = ModelParameter[ffModel, "ffCharge"];
ffScalarMassParameter = ModelParameter[ffModel, "ffScalarMass"];
ffVectorMassParameter = ModelParameter[ffModel, "ffVectorMass"];
ffTripleParameter = ModelParameter[ffModel, "ffTriple"];
ffQuarticParameter = ModelParameter[ffModel, "ffQuartic"];
ffDerivedParameter = ModelParameter[ffModel, "ffDerived"];
ffCheck["Feynman particle mass entries remain symbolic",
  ffModel[[1, 4, 6]] === ffScalarMassParameter &&
    ffModel[[1, 5, 6]] === ffVectorMassParameter];
ffCheck["declared and derived Feynman parameters resolve only through their model",
  ({ffChargeParameter, ffDerivedParameter} /. ModelParameterRules[ffModel]) === {2/3, 8/9}];
ffCheck["real model parameter conjugation is generic inert-data behavior",
  Conjugate[ffChargeParameter] === ffChargeParameter];
ffCheck["an unrelated global parameter value cannot affect a model read",
  Block[{Global`ffCharge = 123},
    ReadModel[ffTemporaryDirectory, 91] === ffModel &&
      Global`ffCharge === 123 &&
      (ffChargeParameter /. ModelParameterRules[ffModel]) === 2/3]];

(* Parsing is deliberately checked without evaluating CalcHEP text as Wolfram code. *)
ffParserCases = {
  {"i", I}, {"Sqrt2", Sqrt[2]}, {"sqrt(2)", Sqrt[2]},
  {"m1.m2", LorentzMetric[LorentzIndex[1], LorentzIndex[2]]},
  {"p1.m2", LorentzMomentum[Mom[1], LorentzIndex[2]]},
  {"m2.p1", LorentzMomentum[Mom[1], LorentzIndex[2]]},
  {"p1.p2", LorentzScalarProduct[Mom[1], Mom[2]]},
  {"p1.p1", LorentzScalarProduct[Mom[1], Mom[1]]},
  {"m1.M3", LorentzMetric[LorentzIndex[1], LorentzIndex[3, 2]]},
  {"G(m3)", DiracWord[DiracGamma[LorentzIndex[3]]]},
  {"G(p1)", DiracWord[DiracSlash[Mom[1]]]},
  {"G(m3)*G5", DiracWord[DiracGamma[LorentzIndex[3]], DiracGamma5]},
  {"G5*G(m3)", DiracWord[DiracGamma5, DiracGamma[LorentzIndex[3]]]},
  {"G(m3)*(1-G5)", DiracWord[DiracGamma[LorentzIndex[3]]] -
    DiracWord[DiracGamma[LorentzIndex[3]], DiracGamma5]},
  {"2*G(m1)*G(p2)+3*G5", 2 DiracWord[DiracGamma[LorentzIndex[1]],
    DiracSlash[Mom[2]]] + 3 DiracWord[DiracGamma5]}
};
Do[
  ffCheck["CalcHEP tensor parser " <> ffParserCase[[1]],
    ffParse[ffParserCase[[1]]] === ffParserCase[[2]]],
  {ffParserCase, ffParserCases}];
ffCheck["gamma multiplication is not commutative",
  ffParse["G(m3)*G5"] =!= ffParse["G5*G(m3)"]];
ffCheck["standalone Feynman parameters retain an explicit data scope",
  ffParse["ffCharge", "parser-regression"] ===
    ModelParameter["ffCharge", "parser-regression", "Real"]];
ffCheck["external CalcHEP functions are retained without creating or evaluating function symbols",
  ffParse["MqEff(2,3)", "parser-regression"] ===
    ModelFunction["MqEff", "parser-regression", {2, 3}] &&
    Names["ConstructiveDiagrams`MqEff"] === {}];
Do[
  ffCheck["malformed or unsupported Feynman expression is explicit " <> ffBadText,
    FailureQ[ffParse[ffBadText]]],
  {ffBadText, {"G(m1", "m1.", "G(m1,m2)", "G(m1)^-1", "p1+", "G(m1);1",
    "Run(\"false\")", "Get(\"missing\")", "unknownFunction(1)", "p0.p1", "m0.m1"}}];
ffCheck["unsupported text cannot create a Wolfram assignment",
  FailureQ[ffParse["ffMustNotBeAssigned=123"]] && OwnValues[ffMustNotBeAssigned] === {}];
ffWrite["lgrng92.mdl", {"Malformed fixture", "P1 | P2 | P3 | P4 | Factor | Lorentz part",
  "E | e | A | | 1 | G(m3"}];
Do[CopyFile[FileNameJoin[{ffTemporaryDirectory, stem <> "91.mdl"}],
  FileNameJoin[{ffTemporaryDirectory, stem <> "92.mdl"}]],
  {stem, {"prtcls", "vars", "func"}}];
ffCheck["a malformed vertex propagates a model-reader failure",
  FailureQ[ReadModel[ffTemporaryDirectory, 92]]];
ffCheck["failed raw import also closes its streams", Streams[] === ffOriginalStreams];
ffInvalidModels = {
  {"E | e | A | | 1 | G(p4)", "ffDerived | 2*ffCharge", False},
  {"E | e | h | | 1 | m1.m2", "ffDerived | 2*ffCharge", False},
  {"E | e | A | | 1 | 1", "ffDerived | 2*ffCharge", False},
  {"E | e | A | | 1 | G(m0)", "ffDerived | 2*ffCharge", False},
  {"E | e | A | | 1 | G(m3)", "ffDerived | ", False},
  {"E | e | A | | 1 | G(m3)", "ffDerived | % comment only", False},
  {"E | e | A | | 1 | G(m3)", "ffDerived | 2*ffCharge", True},
  {"A | A | h | | 1 | (m1.m2)^2", "ffDerived | 2*ffCharge", False},
  {"A | A | h | | 1 | (m1.m2)^(-1)", "ffDerived | 2*ffCharge", False}};
Do[
  ffInvalidNumber = 200 + ffInvalidIndex;
  Do[CopyFile[FileNameJoin[{ffTemporaryDirectory, ffStem <> "91.mdl"}],
    FileNameJoin[{ffTemporaryDirectory, ffStem <> ToString[ffInvalidNumber] <> ".mdl"}]],
    {ffStem, {"prtcls", "vars"}}];
  ffWrite["lgrng" <> ToString[ffInvalidNumber] <> ".mdl", {"Invalid slot fixture",
    "P1 | P2 | P3 | P4 | Factor | Lorentz part", ffInvalidModels[[ffInvalidIndex, 1]]}];
  ffWrite["func" <> ToString[ffInvalidNumber] <> ".mdl", {"Invalid expression fixture",
    "Name | Expression", ffInvalidModels[[ffInvalidIndex, 2]]}];
  If[TrueQ[ffInvalidModels[[ffInvalidIndex, 3]]],
    ffWrite["prtcls" <> ToString[ffInvalidNumber] <> ".mdl", {"Empty particle fixture",
      "Full Name | P | aP | number | spin2 | mass | width | color | aux | LaTeX(A) | LaTeX(A+)"}]];
  ffCheck["invalid Feynman vertex ranks, slots or model rows are rejected " <> ToString[ffInvalidIndex],
    FailureQ[ReadModel[ffTemporaryDirectory, ffInvalidNumber]]],
  {ffInvalidIndex, Length[ffInvalidModels]}];
ffWrite["func91.mdl", {"No derived parameters", "Name | Expression"}];
ffCheck["a model with no derived-parameter rows is valid",
  MatchQ[ReadModel[ffTemporaryDirectory, 91], FeynmanModel[_List, _List, _List, {}]]];
ffWrite["func91.mdl", {"Feynman regression fixture", "Name | Expression",
  "ffDerived | 2*ffCharge^2 % derived fixture"}];
Do[CopyFile[FileNameJoin[{ffTemporaryDirectory, ffStem <> "91.mdl"}],
  FileNameJoin[{ffTemporaryDirectory, ffStem <> "96.mdl"}]],
  {ffStem, {"prtcls", "func", "lgrng"}}];
ffWrite["vars96.mdl", {"Independent Feynman parameter fixture", "Name | Value | Comment",
  "ffCharge | 5/7 | different charge", "ffScalarMass | 3 | scalar mass",
  "ffVectorMass | 5 | vector mass", "ffYukawa | 1/7 | scalar coupling",
  "ffTriple | 1/11 | cubic scalar coupling", "ffQuartic | 1/13 | quartic coupling"}];
ffOtherModel = ReadModel[ffTemporaryDirectory, 96];
ffOtherChargeParameter = ModelParameter[ffOtherModel, "ffCharge"];
ffCheck["two models can own distinct parameters with the same displayed name",
  ffOtherChargeParameter =!= ffChargeParameter &&
    ModelParameterName[ffOtherChargeParameter] === ModelParameterName[ffChargeParameter] === "ffCharge"];
ffCheck["a second Feynman model cannot replace the first model's parameter values",
  (ffChargeParameter /. ModelParameterRules[ffModel]) === 2/3 &&
    (ffOtherChargeParameter /. ModelParameterRules[ffOtherModel]) === 5/7 &&
    (ffChargeParameter /. ModelParameterRules[ffOtherModel]) === ffChargeParameter];
ffCheck["derived dependencies retain their own model's parameter identity",
  (ModelParameter[ffOtherModel, "ffDerived"] /. ModelParameterRules[ffOtherModel]) === 50/49 &&
    (ffDerivedParameter /. ModelParameterRules[ffModel]) === 8/9];

ffStockModel = ReadModel[FileNameJoin[{ffRoot, "model_feynman"}], 1];
ffCheck["the complete copied SM parses as Feynman rules",
  MatchQ[ffStockModel, FeynmanModel[_List, _List, _List, _List]] &&
    Length[ffStockModel[[2]]] === 125];
ffConstructiveModel = ReadModel[FileNameJoin[{ffRoot, "models"}], 2];
ffCheck["constructive models retain their original four-list representation",
  MatchQ[ffConstructiveModel, {_List, _List, _List, _List}] &&
    Head[ffConstructiveModel] === List];
ffCheck["Feynman rules cannot silently use constructive factorization sewing",
  FailureQ[FactorizationAmplitudeChannels[{"e", "E", "A", "A"}, ffModel]]];
ffMalformedManualModels = {
  FeynmanModel[ffModel[[1]], {{"E", "e", "A"}}, ffModel[[3]], ffModel[[4]]],
  FeynmanModel[{{"short particle", "e", "E"}}, ffModel[[2]], ffModel[[3]], ffModel[[4]]],
  FeynmanModel[{}, {}, {}, {}]};
Do[
  ffCheck["malformed manual Feynman models return explicit failures " <> ToString[ffMalformedIndex],
    FailureQ[CreateDiagrams[{"e", "E", "A.+"}, ffMalformedManualModels[[ffMalformedIndex]]]]],
  {ffMalformedIndex, Length[ffMalformedManualModels]}];

(* Physical trees use one generic vector line, including every gamma numerator. *)
ffQEDModel = ffModel;
ffQEDModel[[2]] = Take[ffModel[[2]], 2];
ffThreeDiagrams = CreateDiagrams[{"e", "E", "A.+"}, ffQEDModel];
ffFourDiagrams = CreateDiagrams[{"e", "E", "A.+", "A.-"}, ffQEDModel];
ffFiveDiagrams = CreateDiagrams[{"e", "E", "A.+", "A.+", "A.-"}, ffQEDModel];
ffCheck["Feynman QED three-point diagram uses a helicity alias", Length[ffThreeDiagrams] === 1];
ffCheck["Feynman QED four-point trees have two electron orderings", Length[ffFourDiagrams] === 2];
ffCheck["Feynman QED five-point trees have six electron orderings", Length[ffFiveDiagrams] === 6];
ffThreePairs = diagramAmplitudes[ffThreeDiagrams];
ffFourPairs = diagramAmplitudes[ffFourDiagrams];
ffFivePairs = diagramAmplitudes[ffFiveDiagrams];
Do[
  {ffMultiplicity, ffPairs} = ffPairCase;
  ffCheck["Feynman diagram output keeps ordinary mass-rule pairs " <> ToString[ffMultiplicity],
    MatchQ[ffPairs, {{_List, _} ..}] && ffFiniteQ[ffPairs]];
  ffCheck["Feynman output is not constructive sewing " <> ToString[ffMultiplicity],
    !FreeQ[ffPairs, _DiracChain] && FreeQ[ffPairs, _SpinorChain | _xFactor | _xTildeFactor]],
  {ffPairCase, {{3, ffThreePairs}, {4, ffFourPairs}, {5, ffFivePairs}}}];
ffCheck["an internal electron is a slash propagator rather than an on-shell spin sum",
  !FreeQ[ffFourPairs, DiracSlash[Mom[_Multiparticle]]] &&
    !FreeQ[ffFourPairs, PropDen[Mom[_Multiparticle], 0]]];
ffCheck["five-point diagrams retain both independent off-shell electron poles",
  And @@ (Length[DeleteDuplicates[Cases[Last[#], _PropDen, Infinity]]] === 2 & /@ ffFivePairs)];
ffDistinctDiagrams = CreateDiagrams[{"e", "E", "M", "m"}, ffQEDModel];
ffIdenticalDiagrams = CreateDiagrams[{"e", "E", "E", "e"}, ffQEDModel];
ffCheck["a generic photon exchange is one distinct-flavor diagram", Length[ffDistinctDiagrams] === 1];
ffCheck["identical charged fermions retain both Wick pairings", Length[ffIdenticalDiagrams] === 2];
ffCheck["identical-fermion pairings have opposite exchange parities",
  Sort[diagramFermionSign /@ ffIdenticalDiagrams] === {-1, 1}];
Do[
  ffCheck["global Feynman fermion pairing parity " <> ToString[ffParityCase[[1]]],
    ConstructiveDiagrams`Private`feynmanFermionPairSign[ffParityCase[[1]]] === ffParityCase[[2]]],
  {ffParityCase, {{{{1, 2}, {3, 4}}, 1}, {{{1, 4}, {3, 2}}, -1},
    {{{2, 4}, {1, 3}}, 1}, {{{1, 6}, {3, 2}, {5, 4}}, 1},
    {{{1, 6}, {3, 4}, {5, 2}}, -1}}}];
ffScalarDiagrams = CreateDiagrams[{"h", "h", "h", "h"}, ffModel];
ffScalarPairs = diagramAmplitudes[ffScalarDiagrams];
ffCheck["cubic scalar exchanges coexist with the actual quartic contact",
  Length[ffScalarDiagrams] === 4 &&
    Count[ffScalarDiagrams, d_ /; Length[d["Propagators"]] === 0] === 1];
ffCheck["scalar Feynman amplitudes do not invent fermion or polarization factors",
  ffFiniteQ[ffScalarPairs] && FreeQ[ffScalarPairs, _DiracChain | _PolarizationVector]];
ffCheck["scalar tree amplitudes retain the vertex and propagator i-factor normalization",
  Together[Total[Last /@ ffScalarPairs] + ffQuarticParameter +
    ffTripleParameter^2 Total[1/PropDen[Mom[#], ffScalarMassParameter] & /@
      {Multiparticle[1, 2], Multiparticle[1, 3], Multiparticle[1, 4]}]] === 0];
ffVectorDiagrams = CreateDiagrams[{"e", "E", "e", "E"}, ffModel];
ffZDiagrams = Select[ffVectorDiagrams,
  MemberQ[Lookup[Values[#["Propagators"]], "Particle"], "Z"] &];
ffZPairs = diagramAmplitudes[ffZDiagrams];
ffCheck["massive-vector exchange includes the longitudinal unitary-gauge numerator",
  Length[ffZDiagrams] === 2 && !FreeQ[ffZPairs,
    LorentzMomentum[Mom[_Multiparticle], _]]];
ffPhysicalSM = CreateDiagrams[{"e", "E", "W+", "W-"}, ffStockModel];
ffCheck["physical SM trees omit internal ghosts, Goldstones, and auxiliary tensors",
  ListQ[ffPhysicalSM] && ffPhysicalSM =!= {} &&
    FreeQ[Flatten[Lookup[Values[#["Propagators"]], "Particle"] & /@ ffPhysicalSM],
      name_String /; StringContainsQ[name, ".f" | ".c" | ".C" | ".t"]]];
ffQuarkDiagrams = CreateDiagrams[{"u", "U", "C", "c"}, ffStockModel];
ffQuarkPairs = diagramAmplitudes[ffQuarkDiagrams];
ffCheck["colored exchange retains an explicit nonzero color structure",
  ffQuarkDiagrams =!= {} && ffFiniteQ[ffQuarkPairs] &&
    !FreeQ[ffQuarkPairs, _ColorGenerator | _ColorStructure]];
ffGluonDiagrams = CreateDiagrams[{"G.+", "G.+", "G.-", "G.-"}, ffStockModel];
ffGluonPairs = diagramAmplitudes[ffGluonDiagrams];
ffCheck["four-gluon trees retain the nonabelian interactions instead of returning zero",
  ListQ[ffGluonDiagrams] && ffGluonDiagrams =!= {} && ffFiniteQ[ffGluonPairs] &&
    !FreeQ[ffGluonPairs, _ColorStructure]];
ffCheck["auxiliary gluon contact sewing cannot invent an auxiliary propagator pole",
  FreeQ[ffGluonPairs, PropDen[_, mass_] /; !FreeQ[mass, "G.t" | "G.T"]]];

ffWrite["prtcls93.mdl", {"Derivative fixture",
  "Full Name | P | aP | number | spin2 | mass | width | color | aux | LaTeX(A) | LaTeX(A+)",
  "a | a | a | 101 | 0 | ffScalarMass | 0 | 1 | | a | a",
  "b | b | b | 102 | 0 | ffScalarMass | 0 | 1 | | b | b",
  "c | c | c | 103 | 0 | ffScalarMass | 0 | 1 | | c | c",
  "d | d | d | 104 | 0 | ffScalarMass | 0 | 1 | | d | d"}];
ffWrite["lgrng93.mdl", {"Derivative fixture", "P1 | P2 | P3 | P4 | Factor | Lorentz part",
  "a | b | c | d | 2 | (p1.p2)^2+3"}];
Do[CopyFile[FileNameJoin[{ffTemporaryDirectory, stem <> "91.mdl"}],
  FileNameJoin[{ffTemporaryDirectory, stem <> "93.mdl"}]], {stem, {"vars", "func"}}];
ffDerivativeModel = ReadModel[ffTemporaryDirectory, 93];
ffDerivativeDiagrams = CreateDiagrams[{"b", "d", "a", "c"}, ffDerivativeModel];
ffDerivativePairs = diagramAmplitudes[ffDerivativeDiagrams];
ffCheck["derivative scalar Feynman contact generates one diagram",
  Length[ffDerivativeDiagrams] === 1];
ffCheck["Feynman vertex relabeling changes typed slots, not coefficients or powers",
  Together[Last[First[ffDerivativePairs]] - 2 (MomProd[3, 1]^2 + 3)] === 0];
ffWrite["prtcls94.mdl", {"Odd-derivative fixture",
  "Full Name | P | aP | number | spin2 | mass | width | color | aux | LaTeX(A) | LaTeX(A+)",
  "vector | A | A | 22 | 2 | 0 | 0 | 1 | G | A | A",
  "a | a | a | 101 | 0 | ffScalarMass | 0 | 1 | | a | a",
  "b | b | b | 102 | 0 | ffScalarMass | 0 | 1 | | b | b"}];
ffWrite["lgrng94.mdl", {"Odd-derivative fixture", "P1 | P2 | P3 | P4 | Factor | Lorentz part",
  "A | a | b | | ffCharge | m1.p2-m1.p3"}];
Do[CopyFile[FileNameJoin[{ffTemporaryDirectory, ffStem <> "91.mdl"}],
  FileNameJoin[{ffTemporaryDirectory, ffStem <> "94.mdl"}]], {ffStem, {"vars", "func"}}];
ffOddDerivativeModel = ReadModel[ffTemporaryDirectory, 94];
ffOddDerivativePairs = diagramAmplitudes[CreateDiagrams[{"A.+", "a", "b"}, ffOddDerivativeModel]];
ffOddDerivativeExpression = Last[First[ffOddDerivativePairs]];
ffOddChargeParameter = ModelParameter[ffOddDerivativeModel, "ffCharge"];
ffOddDerivativeIndex = First[Cases[ffOddDerivativeExpression,
  PolarizationVector[1, index_, 0, 1] :> index, Infinity]];
ffCheck["CalcHEP's outgoing derivative momenta are reversed for HEPCAT's incoming legs",
  Expand[ffOddDerivativeExpression + ffOddChargeParameter
    PolarizationVector[1, ffOddDerivativeIndex, 0, 1]
    (LorentzMomentum[Mom[2], ffOddDerivativeIndex] -
      LorentzMomentum[Mom[3], ffOddDerivativeIndex])] === 0];

(* A source momentum square must not become the internal particle's mass. *)
ffWrite["prtcls95.mdl", {"Internal derivative fixture",
  "Full Name | P | aP | number | spin2 | mass | width | color | aux | LaTeX(A) | LaTeX(A+)",
  "phi | phi | phi | 101 | 0 | ffPhiMass | 0 | 1 | | phi | phi",
  "a | a | a | 102 | 0 | ffScalarMass | 0 | 1 | | a | a"}];
ffWrite["vars95.mdl", {"Internal derivative fixture", "Name | Value | Comment",
  "ffPhiMass | 7 | exchanged scalar mass", "ffScalarMass | 3 | external scalar mass",
  "ffPhiCharge | 2 | derivative coupling"}];
ffWrite["func95.mdl", {"Internal derivative fixture", "Name | Expression"}];
ffWrite["lgrng95.mdl", {"Internal derivative fixture", "P1 | P2 | P3 | P4 | Factor | Lorentz part",
  "phi | a | a | | ffPhiCharge | p1.p1"}];
ffPhiModel = ReadModel[ffTemporaryDirectory, 95];
ffPhiMassParameter = ModelParameter[ffPhiModel, "ffPhiMass"];
ffCheck["an internal derivative slot retains its raw momentum-square identity",
  ffPhiModel[[2, 1, -1]] === LorentzScalarProduct[Mom[1], Mom[1]]];
ffPhiDiagrams = CreateDiagrams[{"a", "a", "a", "a"}, ffPhiModel];
ffPhiPairs = diagramAmplitudes[ffPhiDiagrams];
ffCheck["the internal derivative fixture generates all three scalar exchanges",
  Length[ffPhiDiagrams] === 3 && ffFiniteQ[ffPhiPairs]];
ffCheck["an exchanged scalar mass occurs only in physical propagators, never its derivative numerator",
  AllTrue[ffPhiPairs, FreeQ[Last[#] /. _PropDen -> 1, ffPhiMassParameter] &&
    FreeQ[Last[#], _LorentzScalarProduct | Mass[_Multiparticle]] &]];
ffPhiMomenta = <|1 -> {{5, 0}, {0, 9/5}}, 2 -> {{9/5, 0}, {0, 5}},
  3 -> -{{17/5, 8/5}, {8/5, 17/5}}, 4 -> -{{17/5, -8/5}, {-8/5, 17/5}}|>;
ffCheck["the internal-square numerical oracle conserves momenta and external mass shells",
  Total[Values[ffPhiMomenta]] === ConstantArray[0, {2, 2}] &&
    Det /@ Values[ffPhiMomenta] === {9, 9, 9, 9}];

(* Converter checks and independent four-component comparisons follow below. *)
ffJ = {{0, -1}, {1, 0}}; ffL = -ffJ;
ffAdj[matrix_] := {{matrix[[2, 2]], -matrix[[1, 2]]},
  {-matrix[[2, 1]], matrix[[1, 1]]}};
ffZero2 = ConstantArray[0, {2, 2}];
ffSlashMatrix[matrix_] := ArrayFlatten[{{ffZero2, matrix}, {ffAdj[matrix], ffZero2}}];
ffGamma5Matrix = DiagonalMatrix[{-1, -1, 1, 1}];
ffPauli = {{{1, 0}, {0, 1}}, {{0, 1}, {1, 0}},
  {{0, -I}, {I, 0}}, {{1, 0}, {0, -1}}};
ffGammaMatrices = ffSlashMatrix /@ ffPauli;
ffMetric = {1, -1, -1, -1};
Do[
  ffCheck["independent Weyl gamma matrices obey the Clifford algebra " <> ToString[{ffMu, ffNu}],
    ffGammaMatrices[[ffMu]].ffGammaMatrices[[ffNu]] +
      ffGammaMatrices[[ffNu]].ffGammaMatrices[[ffMu]] ===
      2 If[ffMu === ffNu, ffMetric[[ffMu]], 0] IdentityMatrix[4]],
  {ffMu, 4}, {ffNu, 4}];
ffCheck["independent gamma five squares to the identity",
  ffGamma5Matrix.ffGamma5Matrix === IdentityMatrix[4]];
Do[
  ffCheck["independent gamma five anticommutes with gamma " <> ToString[ffMu],
    ffGamma5Matrix.ffGammaMatrices[[ffMu]] +
      ffGammaMatrices[[ffMu]].ffGamma5Matrix === ConstantArray[0, {4, 4}]],
  {ffMu, 4}];

(* Unconserved, off-shell matrices make an illicit internal-shell rule visible. *)
ffMomenta = <|3 -> {{3, 2 - I}, {1 + 2 I, 5}},
  4 -> {{7, -2 + I}, {3 - I, 2}}, 5 -> {{2, 4}, {3, -1}}|>;
ffAngles = <|1 -> {1, 2 + I}, 2 -> {3 - I, -2},
  3 -> {-1 + I, 4}, 4 -> {2, 5 - I}|>;
ffSquares = <|1 -> {4 - I, 3}, 2 -> {-2, 1 + I},
  3 -> {5, -3 + I}, 4 -> {1 - I, 2}|>;
ffAngleRow[i_] := ffAngles[i].ffJ;
ffBarVector[i_] := Join[ffAngleRow[i], -ffSquares[i]];
ffKetVector[i_] := Join[-ffAngles[i], ffL.ffSquares[i]];
ffNumericMomentum[Mom[leg_Integer]] := ffMomenta[leg];
ffNumericMomentum[Mom[Multiparticle[legs__]]] := Total[ffMomenta /@ {legs}];
ffNumericMomentum[sum_Plus] := Total[ffNumericMomentum /@ List @@ sum];
ffNumericMomentum[Times[c_?NumberQ, momentum_]] := c ffNumericMomentum[momentum];
ffNumericSpinor[Spinor[_, kind : ("Angle" | "Square"), leg_Integer, ___]] :=
  If[kind === "Angle", ffAngles[leg], ffSquares[leg]];
ffNumericSpinor[Spinor[_, _, kind : ("Angle" | "Square"), leg_Integer, ___]] :=
  If[kind === "Angle", ffAngles[leg], ffSquares[leg]];
ffNumericChain[chain_SpinorChain] := Module[{parts = List @@ chain, firstKind,
    lastKind, row, column, matrices},
  firstKind = SelectFirst[List @@ First[parts], MemberQ[{"Angle", "Square"}, #] &];
  lastKind = SelectFirst[List @@ Last[parts], MemberQ[{"Angle", "Square"}, #] &];
  row = ffNumericSpinor[First[parts]];
  column = ffNumericSpinor[Last[parts]];
  If[firstKind === "Angle", row = row.ffJ];
  If[lastKind === "Square", column = ffL.column];
  matrices = MapIndexed[Function[{momentum, position},
    If[OddQ[First[position]] === (firstKind === "Angle"),
      ffNumericMomentum[momentum], ffAdj[ffNumericMomentum[momentum]]]],
    parts[[2 ;; -2]]];
  Fold[Dot, row, matrices].column
];
ffNumericTrace[trace_SpinorTrace] := Tr[Fold[Dot, IdentityMatrix[2],
  MapIndexed[If[OddQ[First[#2]], ffNumericMomentum[#1],
    ffAdj[ffNumericMomentum[#1]]] &, List @@ trace]]];
ffNumericSpinorExpression[expr_] := (expr /. {chain_SpinorChain :> ffNumericChain[chain],
  trace_SpinorTrace :> ffNumericTrace[trace]}) /. {
  PropDen[momentum_, mass_] :> Det[ffNumericMomentum[momentum]] - mass^2,
  MomProd[a_, b_] :> (Det[ffNumericMomentum[Mom[a]] + ffNumericMomentum[Mom[b]]] -
    Det[ffNumericMomentum[Mom[a]]] - Det[ffNumericMomentum[Mom[b]]])/2};
Block[{ffMomenta = ffPhiMomenta},
  Do[
    ffPhiExpression = Last[ffPhiPairs[[ffPhiIndex]]] /. ModelParameterRules[ffPhiModel];
    ffPhiCut = First[Cases[ffPhiExpression, PropDen[Mom[cut_], _] :> cut, Infinity]];
    ffPhiInvariant = Det[ffNumericMomentum[Mom[ffPhiCut]]];
    ffCheck["an off-shell internal scalar square produces q^4, not the exchanged mass^4 " <> ToString[ffPhiIndex],
      ffPhiInvariant =!= 49 && Together[ffNumericSpinorExpression[ffPhiExpression] +
        4 ffPhiInvariant^2/(ffPhiInvariant - 49)] === 0];
    ffCheck["the internal-on-shell derivative numerator is rejected off shell " <> ToString[ffPhiIndex],
      Together[ffNumericSpinorExpression[ffPhiExpression] + 4*7^4/(ffPhiInvariant - 49)] =!= 0],
    {ffPhiIndex, Length[ffPhiPairs]}]];
ffWordMatrix[word_List] := Fold[Dot, IdentityMatrix[4],
  word /. {DiracSlash[momentum_] :> ffSlashMatrix[ffNumericMomentum[momentum]],
    DiracGamma5 -> ffGamma5Matrix}];
ffWords = {{}, {DiracGamma5}, {DiracGamma5, DiracGamma5},
  {DiracSlash[Mom[3]]}, {DiracSlash[Mom[3]], DiracGamma5},
  {DiracGamma5, DiracSlash[Mom[3]]},
  {DiracSlash[Mom[3]], DiracSlash[Mom[4]]},
  {DiracSlash[Mom[3]], DiracGamma5, DiracSlash[Mom[4]]},
  {DiracSlash[Mom[3]], DiracSlash[Mom[4]], DiracGamma5},
  {DiracSlash[Mom[3]], DiracSlash[Mom[4]], DiracSlash[Mom[5]]},
  {DiracSlash[Mom[3]], DiracSlash[Mom[3]]},
  {DiracSlash[Mom[Multiparticle[3, 4]]], DiracSlash[Mom[5]]}};
Do[
  ffInputChain = DiracChain[FermionBar[1, ffMass1, "Dirac"],
    DiracWord @@ ffWord, FermionKet[2, ffMass2, "Dirac"]];
  ffConvertedChain = FeynmanToSpinorProducts[ffInputChain, "ReduceSpinorProducts" -> False];
  ffCheck["open Dirac word agrees with independent four-component algebra " <> ToString[ffWordIndex],
    ffFiniteQ[ffConvertedChain] && FreeQ[ffConvertedChain, _DiracChain | _DiracWord] &&
      Together[ffNumericSpinorExpression[ffConvertedChain] -
        ffBarVector[1].ffWordMatrix[ffWord].ffKetVector[2]] === 0],
  {ffWordIndex, Length[ffWords]}, {ffWord, {ffWords[[ffWordIndex]]}}];

ffFierzInput = LorentzMetric[ffLorentz1, ffLorentz2] *
  DiracChain[FermionBar[1, ffMass1, "Dirac"], DiracWord[DiracGamma[ffLorentz1]],
    FermionKet[2, ffMass2, "Dirac"]] *
  DiracChain[FermionBar[3, ffMass3, "Dirac"], DiracWord[DiracGamma[ffLorentz2]],
    FermionKet[4, ffMass4, "Dirac"]];
ffConvertedFierz = FeynmanToSpinorProducts[ffFierzInput, "ReduceSpinorProducts" -> False];
ffFierzDifference = Together[ffNumericSpinorExpression[ffConvertedFierz] -
  Sum[ffMetric[[ffMu]] (ffBarVector[1].ffGammaMatrices[[ffMu]].ffKetVector[2])
    (ffBarVector[3].ffGammaMatrices[[ffMu]].ffKetVector[4]), {ffMu, 4}]];
If[ffFierzDifference =!= 0,
  Print[<|"FierzConverted" -> ffConvertedFierz, "FierzDifference" -> ffFierzDifference|>]];
ffCheck["Lorentz-contracted independent fermion currents obey the four-component Fierz identity",
  ffFiniteQ[ffConvertedFierz] &&
    FreeQ[ffConvertedFierz, _DiracChain | _DiracWord | _LorentzMetric | _DiracGamma] &&
    ffFierzDifference === 0];
Do[
  ffLeftWord = Join[If[ffLeftChiral, {DiracGamma5}, {}],
    {DiracSlash[Mom[3]], DiracGamma[ffLorentz1], DiracSlash[Mom[4]]}];
  ffRightWord = Join[{DiracGamma[ffLorentz2], DiracSlash[Mom[5]]},
    If[ffRightChiral, {DiracGamma5}, {}]];
  ffInteriorFierz = LorentzMetric[ffLorentz1, ffLorentz2]
    DiracChain[FermionBar[1, ffMass1, "Dirac"], DiracWord @@ ffLeftWord,
      FermionKet[2, ffMass2, "Dirac"]]
    DiracChain[FermionBar[3, ffMass3, "Dirac"], DiracWord @@ ffRightWord,
      FermionKet[4, ffMass4, "Dirac"]];
  ffInteriorConverted = FeynmanToSpinorProducts[ffInteriorFierz,
    "ReduceSpinorProducts" -> False];
  ffInteriorExpected = Sum[ffMetric[[ffMu]]
    (ffBarVector[1].ffWordMatrix[ffLeftWord /. DiracGamma[ffLorentz1] ->
      ffGammaMatrices[[ffMu]]].ffKetVector[2])
    (ffBarVector[3].ffWordMatrix[ffRightWord /. DiracGamma[ffLorentz2] ->
      ffGammaMatrices[[ffMu]]].ffKetVector[4]), {ffMu, 4}];
  ffCheck["interior gamma contraction agrees with independent four-component chains " <>
      ToString[{ffLeftChiral, ffRightChiral}],
    ffFiniteQ[ffInteriorConverted] &&
      Together[ffNumericSpinorExpression[ffInteriorConverted] - ffInteriorExpected] === 0],
  {ffLeftChiral, {False, True}}, {ffRightChiral, {False, True}}];
Do[
  ffSelfWord = Join[{DiracGamma[ffLorentz1]}, ffMiddleWord, {DiracGamma[ffLorentz1]}];
  ffSelfConverted = FeynmanToSpinorProducts[
    DiracChain[FermionBar[1, ffMass1, "Dirac"], DiracWord @@ ffSelfWord,
      FermionKet[2, ffMass2, "Dirac"]], "ReduceSpinorProducts" -> False];
  ffSelfExpected = Sum[ffMetric[[ffMu]] ffBarVector[1].
    ffWordMatrix[ffSelfWord /. DiracGamma[ffLorentz1] -> ffGammaMatrices[[ffMu]]].
    ffKetVector[2], {ffMu, 4}];
  ffSelfDifference = Together[ffNumericSpinorExpression[ffSelfConverted] - ffSelfExpected];
  If[ffSelfDifference =!= 0, Print[<|"SelfConverted" -> ffSelfConverted,
    "SelfDifference" -> ffSelfDifference|>]];
  ffCheck["same-chain Lorentz contraction agrees with independent four-component algebra " <>
      ToString[Length[ffMiddleWord]],
    ffFiniteQ[ffSelfConverted] && ffSelfDifference === 0],
  {ffMiddleWord, {{DiracSlash[Mom[3]]}, {DiracSlash[Mom[3]], DiracSlash[Mom[4]]}}}];
ffCheck["a self-contracted Lorentz metric has four-dimensional trace",
  FeynmanToSpinorProducts[LorentzMetric[ffLorentz1, ffLorentz1]] === 4];
ffCheck["uncontracted Lorentz indices are not silently discarded by spinor conversion",
  FailureQ[FeynmanToSpinorProducts[DiracChain[FermionBar[1, ffMass1, "Dirac"],
    DiracWord[DiracGamma[ffLorentz1]], FermionKet[2, ffMass2, "Dirac"]]]]];
ffCheck["a free Lorentz metric is not mistaken for a scalar identity",
  FailureQ[FeynmanToSpinorProducts[LorentzMetric[ffLorentz1, ffLorentz2]]]];
ffCheck["an over-repeated tensor index is not silently contracted",
  FailureQ[FeynmanToSpinorProducts[LorentzMetric[ffLorentz1, ffLorentz1]
    LorentzMomentum[Mom[3], ffLorentz1] LorentzMomentum[Mom[4], ffLorentz1]]]];
ffCheck["unknown Feynman tensors are explicit conversion failures",
  FailureQ[FeynmanToSpinorProducts[DiracChain[FermionBar[1, ffMass1, "Dirac"],
    DiracWord[ffUnsupportedGamma], FermionKet[2, ffMass2, "Dirac"]]]]];
Do[
  ffCheck["invalid Feynman converter option is explicit " <> ToString[ffBadOption, InputForm],
    FailureQ[FeynmanToSpinorProducts[1, ffBadOption]]],
  {ffBadOption, {"ReduceSpinorProducts" -> Automatic, "TimeConstraint" -> 0,
    "TimeConstraint" -> -1, "TimeConstraint" -> "1", "MaxExpandedTerms" -> 0,
    "FermionHelicities" -> 1, "ReferenceSpinors" -> "Cyclic", "UnknownOption" -> True}}];
ffCheck["an unlimited converter deadline remains an explicit supported option",
  FeynmanToSpinorProducts[1, "TimeConstraint" -> Infinity] === 1];

ffReferenceAngle[leg_] := {17 + leg, 2 - leg};
ffReferenceSquare[leg_] := {leg - 3, 13 + leg};
ffNumericSpinor[Spinor["Xi", "Angle", leg_Integer, ___]] := ffReferenceAngle[leg];
ffNumericSpinor[Spinor["Xi", "Square", leg_Integer, ___]] := ffReferenceSquare[leg];
ffPolarization[leg_, helicity_] := If[helicity === 1,
  Table[-ffReferenceAngle[leg].ffJ.ffPauli[[mu]].ffL.ffSquares[leg]/
    (Sqrt[2] ffReferenceAngle[leg].ffJ.ffAngles[leg]), {mu, 4}],
  Table[-ffReferenceSquare[leg].ffAdj[ffPauli[[mu]]].ffAngles[leg]/
    (Sqrt[2] ffReferenceSquare[leg].ffL.ffSquares[leg]), {mu, 4}]];
ffMomentumComponents[matrix_] := {Tr[matrix]/2,
  -(matrix[[1, 2]] + matrix[[2, 1]])/2,
  -(matrix[[2, 1]] - matrix[[1, 2]])/(2 I),
  -(matrix[[1, 1]] - matrix[[2, 2]])/2};
ffFourFactor[DiracGamma[index_], indices_] := ffGammaMatrices[[indices[index]]];
ffFourFactor[DiracSlash[momentum_], indices_] := ffSlashMatrix[ffNumericMomentum[momentum]];
ffFourFactor[DiracGamma5, indices_] := ffGamma5Matrix;
ffFourFactor[sum_Plus, indices_] := Total[ffFourFactor[#, indices] & /@ List @@ sum];
ffFourFactor[Times[c_?NumberQ, matrix_], indices_] := c ffFourFactor[matrix, indices];
ffFourFactor[c_?NumberQ, indices_] := c IdentityMatrix[4];
ffFourWord[word_DiracWord, indices_] := Fold[Dot, IdentityMatrix[4],
  ffFourFactor[#, indices] & /@ List @@ word];
ffLorentzLabels[expr_] := DeleteDuplicates[Join[
  Cases[expr, DiracGamma[index_] :> index, Infinity],
  Flatten[Cases[expr, LorentzMetric[a_, b_] :> {a, b}, Infinity]],
  Cases[expr, LorentzMomentum[_, index_] :> index, Infinity],
  Cases[expr, PolarizationVector[_, index_, _, _] :> index, Infinity]]];
ffTensorValue[expr_, indices_Association] := Module[{mapped},
  mapped = expr /. DiracChain[FermionBar[bar_, _, _], word_DiracWord,
      FermionKet[ket_, _, _]] :> ffBarVector[bar].ffFourWord[word, indices].ffKetVector[ket];
  mapped = mapped /. {
    LorentzMetric[a_, b_] :> If[indices[a] === indices[b], ffMetric[[indices[a]]], 0],
    LorentzMomentum[momentum_, index_] :> ffMomentumComponents[ffNumericMomentum[momentum]][[indices[index]]],
    PolarizationVector[leg_, index_, 0, helicity_] :> ffPolarization[leg, helicity][[indices[index]]],
    PropDen[momentum_, mass_] :> Det[ffNumericMomentum[momentum]] - mass^2};
  mapped
];
ffFourComponentAmplitude[expr_] := Module[{terms = Expand[expr], labels},
  Total[Function[term,
    labels = ffLorentzLabels[term];
    Total[Function[values, (Times @@ ffMetric[[values]])
      ffTensorValue[term, AssociationThread[labels, values]]] /@
      Tuples[Range[4], Length[labels]]]] /@
    If[Head[terms] === Plus, List @@ terms, {terms}]]
];
ffMassiveQEDModel = ffQEDModel;
ffMassiveQEDModel[[1, 1, 6]] = ffBenchmarkMass;
ffMomenta[1] = {{4, 1 - I}, {2 + I, 3}};
ffMomenta[2] = {{6, 3 + 2 I}, {-1 + I, 2}};
ffAngles[5] = {3 + I, -4}; ffSquares[5] = {-1, 6 - I};
Do[ffMomenta[ffLeg] = Outer[Times, ffAngles[ffLeg], ffSquares[ffLeg]],
  {ffLeg, {3, 4, 5}}];
ffOffShellDotPair = {{Mass[3] -> 0, Mass[4] -> 0,
    Mass[Multiparticle[3, 4]] -> ffForbiddenInternalMass},
  LorentzScalarProduct[Mom[Multiparticle[3, 4]], Mom[Multiparticle[3, 4]]]};
ffConvertedOffShellDotPair = FeynmanToSpinorProducts[ffOffShellDotPair,
  "ReduceSpinorProducts" -> False];
ffCheck["explicit composite scalar products remain off shell in Feynman conversion",
  First[ffConvertedOffShellDotPair] === First[ffOffShellDotPair] &&
    FreeQ[Last[ffConvertedOffShellDotPair], ffForbiddenInternalMass | Mass[_Multiparticle]] &&
    Together[ffNumericSpinorExpression[Last[ffConvertedOffShellDotPair] /.
      {Mass[3] -> 0, Mass[4] -> 0}] - Det[ffMomenta[3] + ffMomenta[4]]] === 0];
ffPolarizationNorm = FeynmanToSpinorProducts[
  PolarizationVector[3, ffLorentz1, 0, 1] PolarizationVector[3, ffLorentz1, 0, -1],
  "ReduceSpinorProducts" -> False];
ffCheck["opposite photon polarizations retain the CSM relative phase",
  ffFiniteQ[ffPolarizationNorm] &&
    Together[ffNumericSpinorExpression[ffPolarizationNorm] - 1] === 0];
Do[
  ffBenchmarkDiagrams = CreateDiagrams[ffBenchmarkParticles, ffMassiveQEDModel];
  ffBenchmarkPairs = diagramAmplitudes[ffBenchmarkDiagrams];
  ffBenchmarkSpinorPairs = FeynmanToSpinorProducts[ffBenchmarkPairs,
    "ReduceSpinorProducts" -> False];
  ffCheck["complete QED conversion keeps mass-rule pairs at " <> ToString[Length[ffBenchmarkParticles]] <> " points",
    MatchQ[ffBenchmarkSpinorPairs, {{_List, _} ..}] &&
      First /@ ffBenchmarkSpinorPairs === First /@ ffBenchmarkPairs];
  Do[
    ffBenchmarkExpression = Last[ffBenchmarkPairs[[ffDiagramIndex]]] /.
      {ffChargeParameter -> 2/3, ffBenchmarkMass -> 7/5};
    ffBenchmarkSpinorExpression = Last[ffBenchmarkSpinorPairs[[ffDiagramIndex]]] /.
      {ffChargeParameter -> 2/3, ffBenchmarkMass -> 7/5};
    ffCheck["QED diagram spinor conversion agrees with independent off-cut four-component tensors " <>
        ToString[{Length[ffBenchmarkParticles], ffDiagramIndex}],
      ffFiniteQ[ffBenchmarkSpinorExpression] &&
        FreeQ[ffBenchmarkSpinorExpression, _DiracChain | _DiracWord | _PolarizationVector |
          _LorentzMetric | _LorentzMomentum] &&
        Together[ffNumericSpinorExpression[ffBenchmarkSpinorExpression] -
          ffFourComponentAmplitude[ffBenchmarkExpression]] === 0],
    {ffDiagramIndex, Length[ffBenchmarkPairs]}],
  {ffBenchmarkParticles, {{"e", "E", "A.+"}, {"e", "E", "A.-"},
    {"e", "E", "A.+", "A.-"}, {"e", "E", "A.+", "A.+", "A.-"}}}];

(* Independent physical normalization against HEPCAT's three-point vertices. *)
ffCSMMass = 2;
ffCSMP1 = ffCSMMass IdentityMatrix[2];
ffCSMK = {{1, 1}, {-1, -1}};
ffCSMP2 = -ffCSMP1 - ffCSMK;
ffCSMA1 = {1, 2}; ffCSMA2 = {3, 5};
ffCSMAK = {1, -1}; ffCSMBK = {1, 1}; ffCSMXi = {2, 3};
ffCSMB1 = -ffJ.ffAdj[ffCSMP1].ffCSMA1/ffCSMMass;
ffCSMB2 = -ffJ.ffAdj[ffCSMP2].ffCSMA2/ffCSMMass;
ffCSMMomenta = <|1 -> ffCSMP1, 2 -> ffCSMP2, 3 -> ffCSMK|>;
ffCSMAngles = <|1 -> ffCSMA1, 2 -> ffCSMA2, 3 -> ffCSMAK|>;
ffCSMSquares = <|1 -> ffCSMB1, 2 -> ffCSMB2, 3 -> ffCSMBK|>;
ffCSMSpinor[Spinor["Xi", _, _]] := ffCSMXi;
ffCSMSpinor[Spinor[_, "Angle", leg_]] := ffCSMAngles[leg];
ffCSMSpinor[Spinor[_, "Square", leg_]] := ffCSMSquares[leg];
ffCSMChain[chain_SpinorChain] := Module[{parts = List @@ chain, row, column, kind, matrices},
  row = ffCSMSpinor[First[parts]]; column = ffCSMSpinor[Last[parts]];
  kind = First[parts][[2]];
  If[kind === "Angle", row = row.ffJ];
  If[Last[parts][[2]] === "Square", column = ffL.column];
  matrices = MapIndexed[If[OddQ[First[#2]] === (kind === "Angle"),
    ffCSMMomenta[First[#1]], ffAdj[ffCSMMomenta[First[#1]]]] &, parts[[2 ;; -2]]];
  Fold[Dot, row, matrices].column
];
ffCSMX = ffCSMXi.ffJ.(ffCSMP2 - ffCSMP1).ffL.ffCSMBK/
  (2 ffCSMMass ffCSMXi.ffJ.ffCSMAK);
ffCSMXTilde = ffCSMXi.ffAdj[ffCSMP2 - ffCSMP1].ffCSMAK/
  (2 ffCSMMass ffCSMXi.ffL.ffCSMBK);
ffCSMTargets = <|1 -> -Sqrt[2] ffCSMX (ffCSMA1.ffJ.ffCSMA2),
  -1 -> -Sqrt[2] ffCSMXTilde (ffCSMB1.ffL.ffCSMB2)|>;
ffCheck["the three-point phase fixture is conserved with the declared external masses",
  ffCSMP1 + ffCSMP2 + ffCSMK === ConstantArray[0, {2, 2}] &&
    Det /@ {ffCSMP1, ffCSMP2, ffCSMK} === {4, 4, 0}];
Do[
  ffCSMConverted = FeynmanToSpinorProducts[
    -DiracChain[FermionBar[2, ffCSMMass, "Dirac"], DiracWord[DiracGamma[ffLorentz1]],
      FermionKet[1, ffCSMMass, "Dirac"]] PolarizationVector[3, ffLorentz1, 0, ffHelicity],
    "ReduceSpinorProducts" -> False];
  ffCheck["converted QED vertex agrees with the independent CSM x-factor amplitude " <> ToString[ffHelicity],
    ffFiniteQ[ffCSMConverted] &&
      Together[(ffCSMConverted /. chain_SpinorChain :> ffCSMChain[chain]) -
        ffCSMTargets[ffHelicity]] === 0], {ffHelicity, {1, -1}}];

ffHZZConverted = FeynmanToSpinorProducts[
  ffElectric ffZMass/(ffCW ffSW) LorentzMetric[ffLorentz1, ffLorentz2]
    PolarizationVector[2, ffLorentz1, ffZMass, Automatic]
    PolarizationVector[3, ffLorentz2, ffZMass, Automatic], "ReduceSpinorProducts" -> False];
ffHZZTarget = -ffElectric/(4 ffCW ffZMass ffSW) Total[Flatten[Table[
  SpinorChain[Spinor["Spin", "Upper", "Angle", 2, VectorSpinIndex[2, ffP[[1]]]],
    Spinor["Spin", "Upper", "Angle", 3, VectorSpinIndex[3, ffQ[[1]]]]]
  SpinorChain[Spinor["Spin", "Upper", "Square", 2, VectorSpinIndex[2, ffP[[2]]]],
    Spinor["Spin", "Upper", "Square", 3, VectorSpinIndex[3, ffQ[[2]]]]],
  {ffP, {{1, 2}, {2, 1}}}, {ffQ, {{1, 2}, {2, 1}}}]]];
ffCheck["the hZZ conversion matches the symmetrized CSM three-point amplitude exactly",
  Expand[ffHZZConverted - ffHZZTarget] === 0];

ffMassiveComponentExpressions = Table[FeynmanToSpinorProducts[
  LorentzMomentum[Mom[100 + ffComponent], ffLorentz1]
    PolarizationVector[3, ffLorentz1, 2, Automatic], "ReduceSpinorProducts" -> False],
  {ffComponent, 4}];
ffVectorSpinor[Spinor["Spin", "Upper", "Angle", 3, VectorSpinIndex[3, index_]]] :=
  ffVectorAngleBasis[[All, index]];
ffVectorSpinor[Spinor["Spin", "Upper", "Square", 3, VectorSpinIndex[3, index_]]] :=
  ffVectorSquareBasis[[All, index]];
ffVectorChain[chain_SpinorChain] := Module[{parts = List @@ chain, row, column, kind, matrices},
  row = ffVectorSpinor[First[parts]]; column = ffVectorSpinor[Last[parts]];
  kind = First[parts][[3]];
  If[kind === "Angle", row = row.ffJ];
  If[Last[parts][[3]] === "Square", column = ffL.column];
  matrices = MapIndexed[If[OddQ[First[#2]] === (kind === "Angle"),
    ffPauli[[First[#1] - 100]], ffAdj[ffPauli[[First[#1] - 100]]]] &,
    parts[[2 ;; -2]]];
  Fold[Dot, row, matrices].column
];
Do[
  {ffVectorAngleBasis, ffVectorSquareBasis, ffVectorMomentum} = ffVectorCase;
  ffVectorModes = {
    ffMassiveComponentExpressions /. VectorSpinIndex[3, 2] -> VectorSpinIndex[3, 1],
    Sqrt[2] ffMassiveComponentExpressions,
    ffMassiveComponentExpressions /. VectorSpinIndex[3, 1] -> VectorSpinIndex[3, 2]};
  ffVectorModes = Simplify[ffVectorModes /. chain_SpinorChain :> ffVectorChain[chain]];
  Do[
    ffCheck["converted massive-vector polarization is transverse " <> ToString[{ffVectorCaseIndex, ffModeIndex}],
      Simplify[Total[ffMetric ffVectorMomentum ffVectorModes[[ffModeIndex]]]] === 0];
    ffCheck["converted massive-vector polarization has norm minus one " <> ToString[{ffVectorCaseIndex, ffModeIndex}],
      Simplify[Total[ffMetric ffVectorModes[[ffModeIndex]]
        Conjugate[ffVectorModes[[ffModeIndex]]]]] === -1], {ffModeIndex, 3}];
  ffCheck["converted massive-vector spin sum matches the unitary-gauge numerator " <> ToString[ffVectorCaseIndex],
    Simplify[Total[Outer[Times, #, Conjugate[#]] & /@ ffVectorModes] +
      DiagonalMatrix[ffMetric] - Outer[Times, ffVectorMomentum, ffVectorMomentum]/4] ===
      ConstantArray[0, {4, 4}]],
  {ffVectorCaseIndex, 2}, {ffVectorCase, {If[ffVectorCaseIndex === 1,
    {Sqrt[2] IdentityMatrix[2], -Sqrt[2] ffJ, {2, 0, 0, 0}},
    {{{2, 0}, {0, 1}}, {{0, 2}, {-1, 0}}, {5/2, 0, 0, -3/2}}]}}];

(* Color sums are termwise: unrelated diagrams do not share dummy indices. *)
ffColorMatrices = {
  {{0, 1, 0}, {1, 0, 0}, {0, 0, 0}}, {{0, -I, 0}, {I, 0, 0}, {0, 0, 0}},
  {{1, 0, 0}, {0, -1, 0}, {0, 0, 0}}, {{0, 0, 1}, {0, 0, 0}, {1, 0, 0}},
  {{0, 0, -I}, {0, 0, 0}, {I, 0, 0}}, {{0, 0, 0}, {0, 0, 1}, {0, 1, 0}},
  {{0, 0, 0}, {0, 0, -I}, {0, I, 0}}, DiagonalMatrix[{1, 1, -2}]/Sqrt[3]}/2;
ffColorF[a_Integer, b_Integer, c_Integer] := ffColorF[a, b, c] = Together[
  -2 I Tr[(ffColorMatrices[[a]].ffColorMatrices[[b]] -
    ffColorMatrices[[b]].ffColorMatrices[[a]]).ffColorMatrices[[c]]]];
ffColorTerm[expr_, externalRules_] := Module[{body = expr /. externalRules,
    fields, indices, domain},
  fields = Cases[body, _ColorGenerator | _ColorStructure | _ColorDelta, Infinity];
  indices = DeleteDuplicates[Cases[fields, index_Symbol, Infinity]];
  domain[index_] := If[!FreeQ[fields,
    ColorGenerator[index, _, _] | ColorGenerator[_, index, _]], 3, 8];
  Total[Function[values, (body /. Thread[indices -> values]) /. {
      ColorDelta[a_Integer, b_Integer] :> KroneckerDelta[a, b],
      ColorGenerator[a_Integer, b_Integer, c_Integer] :> ffColorMatrices[[c, a, b]],
      ColorStructure[a_Integer, b_Integer, c_Integer] :> ffColorF[a, b, c]}] /@
    Tuples[Range[domain[#]] & /@ indices]]
];
ffColorValue[expr_, externalRules_] := Total[ffColorTerm[#, externalRules] & /@
  If[Head[Expand[expr]] === Plus, List @@ Expand[expr], {Expand[expr]}]];
ffWardChannel = <|"Cut" -> {1, 2}, "Complement" -> {3, 4}, "Mass" -> 0,
  "ExternalLegs" -> Range[4], "ExternalMassRules" -> Thread[(Mass /@ Range[4]) -> 0],
  "Propagator" -> PropDen[Mom[Multiparticle[1, 2]], 0]|>;
ffWardParameterRules = ModelParameterRules[ffStockModel,
  {ModelParameter[ffStockModel, "GG"] -> 1, ModelParameter[ffStockModel, "EE"] -> 1}];
ffWardChartReport = ConstructiveDiagrams`Private`residueAutomaticChart[
  ffWardChannel, 1, 9317, ffWardParameterRules];
ffWardChart = ffWardChartReport["Chart"] /.
  ffWardChartReport["Chart"]["PoleVariable"] -> 7;
Do[
  ffWardDiagrams = CreateDiagrams[ffWardParticles, ffStockModel];
  ffWardAmplitude = Total[Last /@ diagramAmplitudes[ffWardDiagrams]];
  ffWardColorRules = If[First[ffWardParticles] === "u",
    {ColorIndex[1] -> 1, ColorIndex[2] -> 2, ColorIndex[3] -> 1, ColorIndex[4] -> 3},
    {ColorIndex[1] -> 1, ColorIndex[2] -> 2, ColorIndex[3] -> 1, ColorIndex[4] -> 2}];
  Do[
    ffWardReplaced = ffWardAmplitude /.
      PolarizationVector[ffWardLeg, index_, 0, _] :> LorentzMomentum[Mom[ffWardLeg], index];
    ffWardSpinors = FeynmanToSpinorProducts[ffWardReplaced,
      "FermionHelicities" -> {1 -> -1/2, 2 -> 1/2}];
    ffCheck["the complete Feynman amplitude satisfies its Ward identity " <>
        ToString[{ffWardParticles, ffWardLeg}, InputForm],
      ffFiniteQ[ffWardSpinors] && Together[ConstructiveDiagrams`Private`residueEvaluateChart[
        ffColorValue[ffWardSpinors, ffWardColorRules], ffWardChart, ffWardParameterRules]] === 0],
    {ffWardLeg, If[First[ffWardParticles] === "G.+", Range[4], {3, 4}]}],
  {ffWardParticles, {{"e", "E", "A.+", "A.-"}, {"u", "U", "G.+", "G.-"},
    {"G.+", "G.+", "G.+", "G.+"}}}];

(* Derive complete QED amplitudes before loading any known target formula. *)
ffPhysicalQED = ReadModel[FileNameJoin[{ffRoot, "model_feynman"}], 2];
ffPhysicalParameterRules = ModelParameterRules[ffPhysicalQED,
  {ModelParameter[ffPhysicalQED, "Me"] -> 1, ModelParameter[ffPhysicalQED, "EE"] -> 1}];
ffCheck["the independent massive-electron QED benchmark is a model, not a patched SM",
  MatchQ[ffPhysicalQED, FeynmanModel[_List, _List, _List, {}]] &&
    Length[ffPhysicalQED[[1]]] === 2 && Length[ffPhysicalQED[[2]]] === 1 &&
    (ModelParameter[ffPhysicalQED, "Me"] /. ModelParameterRules[ffPhysicalQED]) === 1/2000 &&
    (ModelParameter[ffPhysicalQED, "EE"] /. ModelParameterRules[ffPhysicalQED]) === 31333/100000];
ffPhysicalFiveDiagrams = CreateDiagrams[{"e", "E", "A.+", "A.+", "A.+"}, ffPhysicalQED];
ffPhysicalFivePairs = diagramAmplitudes[ffPhysicalFiveDiagrams];
ffPhysicalFiveConverted = FeynmanToSpinorProducts[ffPhysicalFivePairs,
  "ReduceSpinorProducts" -> False, "TimeConstraint" -> 90];
ffPhysicalFiveAmplitude = Total[ffPhysicalFiveConverted[[All, 2]]];
ffPhysicalFourProcesses = {{"e", "E", "A.+", "A.+"}, {"e", "E", "A.+", "A.-"}};
ffPhysicalFourAmplitudes = Table[
  ffPhysicalFourPairs = diagramAmplitudes[CreateDiagrams[ffPhysicalFourParticles, ffPhysicalQED]];
  Total[FeynmanToSpinorProducts[ffPhysicalFourPairs,
    "ReduceSpinorProducts" -> False][[All, 2]]],
  {ffPhysicalFourParticles, ffPhysicalFourProcesses}];
ffCheck["the physical five-point benchmark is independently generated from six diagrams",
  Length[ffPhysicalFiveDiagrams] === 6 && ffFiniteQ[ffPhysicalFiveAmplitude] &&
    FreeQ[ffPhysicalFiveAmplitude, _DiracChain | _PolarizationVector]];
Get[FileNameJoin[{ffRoot, "tests", "QED-5-point-residues.wl"}]];
Do[
  ffPhysicalChart = prDoubleChart[ffPhysicalPoint[[1]], ffPhysicalPoint[[2]], ffSpin1, ffSpin2];
  ffPhysicalValue = Cancel[Together[prMapOnChart[
    ffPhysicalFiveAmplitude /. ffPhysicalParameterRules, ffPhysicalChart]]];
  ffPhysicalKnownValue = Cancel[Together[prMapOnChart[prH, ffPhysicalChart]]];
  ffCheck["complete five-point Feynman QED agrees with the independently known H form " <>
      ToString[{ffPhysicalPoint, ffSpin1, ffSpin2}],
    ffFiniteQ[ffPhysicalValue] && Together[ffPhysicalValue - ffPhysicalKnownValue] === 0];
  ffCheck["the five-point benchmark stays away from every internal mass shell " <>
      ToString[{ffPhysicalPoint, ffSpin1, ffSpin2}],
    AllTrue[(Det[prMomentum[Multiparticle @@ #, ffPhysicalChart]] - 1 &) /@
      Join[({1, #} & /@ {3, 4, 5}), ({2, #} & /@ {3, 4, 5})], # =!= 0 &]],
  {ffPhysicalPoint, {{2, 3}, {1, 2}, {3/2, -2/3}}}, {ffSpin1, 2}, {ffSpin2, 2}];
Do[
  ffPhysicalFourRow = SelectFirst[prFixtureModel[[2]],
    Take[#, 4] === ffPhysicalFourProcesses[[ffProcessIndex]] &];
  ffPhysicalFourTarget = Times @@ ffPhysicalFourRow[[-2 ;; -1]];
  ffPhysicalFourChart = prFourPointChart[{1, 3}, 1, ffSpin1, ffSpin2];
  ffPhysicalFourValue = Cancel[Together[prMapOnChart[
    ffPhysicalFourAmplitudes[[ffProcessIndex]] /. ffPhysicalParameterRules, ffPhysicalFourChart]]];
  ffPhysicalFourExpected = Cancel[Together[prMapOnChart[ffPhysicalFourTarget, ffPhysicalFourChart]]];
  ffPhysicalFourWrongSlash = Cancel[Together[prMapOnChart[
    (ffPhysicalFourAmplitudes[[ffProcessIndex]] /. ffPhysicalParameterRules) /.
      SpinorChain[parts___] :> (-1)^Count[{parts}, Mom[_Multiparticle]] SpinorChain[parts],
    ffPhysicalFourChart]]];
  ffCheck["complete four-point Feynman QED agrees with the known model amplitude " <>
      ToString[{ffProcessIndex, ffSpin1, ffSpin2}],
    Together[ffPhysicalFourValue - ffPhysicalFourExpected] === 0];
  ffCheck["reversing the internal fermion slash is rejected by the known four-point amplitude " <>
      ToString[{ffProcessIndex, ffSpin1, ffSpin2}],
    Together[ffPhysicalFourWrongSlash - ffPhysicalFourExpected] =!= 0],
  {ffProcessIndex, 2}, {ffSpin1, 2}, {ffSpin2, 2}];

DeleteDirectory[ffTemporaryDirectory, DeleteContents -> True];
ffCheck["temporary Feynman model files are removed", !DirectoryQ[ffTemporaryDirectory]];
Print[<|"Checks" -> ffChecks, "Failed" -> Length[ffFailures], "Failures" -> ffFailures|>];
Print["HEPCAT_TEST_COMPLETED:feynman-tests.wl"];
Quit[If[ffFailures === {}, 0, 1]];
