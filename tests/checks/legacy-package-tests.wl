(* Legacy API availability, safe reloading, and explicitly tracked existing bugs. *)
legacyRoot = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
legacyMissingSource = FileNameJoin[{$TemporaryDirectory, "hepcat-missing-source-" <> CreateUUID[]}];
legacyMissingFresh = Block[{Global`$HEPCATpath = legacyMissingSource},
  Quiet[Get[FileNameJoin[{legacyRoot, "source", "HEPCAT.wl"}]]]];
legacyInitializedAfterMissing = MemberQ[$Packages, "ConstructiveDiagrams`"];
legacyGlobalsAfterMissing = Flatten[Names["Global`" <> #] & /@
  {"Spinor", "Mom", "Mass", "ReduceSpinorProducts"}];
Global`$HEPCATpath = FileNameJoin[{legacyRoot, "source"}];
Block[{$MessageList = {}},
  Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
  legacyLoadMessages = $MessageList];
legacyChecks = 0; legacyFailures = {};
SetAttributes[legacyCheck, HoldRest];
legacyCheck[name_, assertion_] := Module[{result},
  legacyChecks++;
  result = TimeConstrained[Quiet[Check[assertion, $Failed]], 10, $TimedOut];
  If[!TrueQ[result], AppendTo[legacyFailures, {name, ToString[result, InputForm]}]]
];
legacyDefinitionQ[name_String] := ToExpression["ConstructiveDiagrams`" <> name,
  InputForm, Function[symbol, Length[DownValues[symbol]] + Length[SubValues[symbol]] > 0, HoldAll]];
legacyProtectedQ[name_String] := ToExpression["ConstructiveDiagrams`" <> name,
  InputForm, Function[symbol, MemberQ[Attributes[symbol], Protected], HoldAll]];

legacyCheck["missing sources fail before fresh package initialization",
  legacyMissingFresh === $Failed && !legacyInitializedAfterMissing];
legacyCheck["a failed fresh load creates no shadowing global algebra symbols",
  legacyGlobalsAfterMissing === {}];
legacyCheck["fresh package load has no messages", legacyLoadMessages === {}];
legacyCheck["package context is registered", MemberQ[$Packages, "ConstructiveDiagrams`"]];
legacyCheck["public context is on the search path", MemberQ[$ContextPath, "ConstructiveDiagrams`"]];
legacyCheck["package loading restores the caller's context", $Context === "Global`"];
legacyCheck["private context does not leak onto the search path",
  !MemberQ[$ContextPath, "ConstructiveDiagrams`Private`"]];

(* These are interface-presence checks. The other legacy suites test their behavior. *)
legacyPublicFunctions = {
  "nice", "lnice", "TeX", "lTeX", "PlainText", "lPlainText", "SPINAS",
  "CreateSPName", "CreateSPInitializationStatement", "CreateSPUpdateStatement",
  "InitializeSPINASSpinorProducts", "UpdateSPINASSpinorProducts", "DeclareSPINASSpinorProducts",
  "ReverseSequence", "ReduceSpinContractions", "ReduceSpinorProducts", "SimplifySpinorProducts",
  "ReducexFactors", "xFactorReduced", "xTildeFactorReduced", "ExtractMandelstahm", "ReduceColor",
  "SpinToHelicitySpinors", "ExtractEnergy", "ExtractEnergy1", "ExtractEnergy2", "ExpandSummation",
  "ConvertStringAmplitude", "ConvertStringHattedAmplitude", "ReverseMomentum", "PermuteParticles",
  "ChooseSpinIndex", "ChooseSpinIndices", "MakeIndicesExplicit", "MakeIndicesImplicit",
  "SymmetrizeSpin", "SymmetrizeSpins", "ComplexifyMomenta", "ExpandComplexMomenta",
  "ReplaceSpinorZ", "UnhatMomenta", "SquareAmplitude",
  "ReadModel", "ReadVariables", "ReadFunctions", "ReadParticles", "ParticleMass",
  "ReadAmplitudes", "ConvertAmplitude", "AddOppositeHelicityAmplitudes",
  "ConstructiveDiagram", "SChannelDiagram", "TChannelDiagram", "UChannelDiagram",
  "FactorizationPartitions", "LookupAmplitude", "ExpandXFactors", "FactorizationAmplitudeChannels",
  "SimplifyFactorizationChannels", "ExpandMomHatAlt", "SumResiduesAlt", "UnHatSpinors",
  "CreateDiagrams", "CreateArrowDiagram", "CreateArrowDiagrams", "diagramAmplitude", "diagramAmplitudes"
};
Do[legacyCheck["legacy public function is defined: " <> name, legacyDefinitionQ[name]],
  {name, legacyPublicFunctions}];
legacyProtectedFunctions = {
  "nice", "lnice", "TeX", "lTeX", "Spinor", "SpinorChain", "SpinorTrace", "Mom", "Mass",
  "xFactor", "xTildeFactor", "MomProd", "PropDen", "ReverseSequence",
  "ReduceSpinContractions", "ReduceSpinorProducts", "SpinorChainReduced", "SpinorTraceReduced",
  "ConvertStringAmplitude", "SimplifySpinorProducts", "ExtractEnergy", "ExtractEnergy1",
  "ExtractEnergy2", "ExpandSummation", "ReverseMomentum", "ChooseSpinIndices", "ChooseSpinIndex",
  "MakeIndicesExplicit", "SymmetrizeSpins", "SymmetrizeSpin", "SquareAmplitude", "ReadModel",
  "ReadVariables", "ReadFunctions", "ReadParticles", "ParticleMass",
  "SChannelDiagram", "TChannelDiagram", "UChannelDiagram", "ConstructiveDiagram",
  "CreateDiagrams", "CreateArrowDiagram", "CreateArrowDiagrams", "diagramAmplitude", "diagramAmplitudes"
};
Do[legacyCheck["legacy public symbol remains protected: " <> name, legacyProtectedQ[name]],
  {name, legacyProtectedFunctions}];
legacyCheck["global arithmetic heads remain protected",
  AllTrue[{Attributes[Plus], Attributes[Times], Attributes[Conjugate]}, MemberQ[#, Protected] &]];

legacyReloadResults[] := {
  ReduceSpinorProducts[SpinorChain[Spinor["Spin", "Angle", 1], Mom[1],
    Spinor["Spin", "Square", 2]]],
  ReverseMomentum[SpinorChain[Spinor["Helicity", "Angle", 1],
    Spinor["Helicity", "Angle", 2]], 1],
  ConvertStringAmplitude["<12>", {0, 0}],
  Length[FactorizationPartitions[{"a", "b", "c", "d", "e"}]]
};
legacyBeforeReload = legacyReloadResults[];
legacySentinel = <|"Keep" -> {3, 5, 7}|>;
Block[{$MessageList = {}},
  Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
  legacyReloadMessages = $MessageList];
legacyCheck["reloading does not emit package or shadowing messages", legacyReloadMessages === {}];
legacyCheck["reloading preserves representative legacy calculations",
  legacyReloadResults[] === legacyBeforeReload];
legacyCheck["reloading preserves unrelated user data", legacySentinel === <|"Keep" -> {3, 5, 7}|>];
legacyCheck["reloading keeps the caller's context", $Context === "Global`"];
legacyCheck["reloading preserves the legacy public definitions",
  AllTrue[legacyPublicFunctions, legacyDefinitionQ]];
legacyCheck["reloading preserves public protections",
  AllTrue[legacyProtectedFunctions, legacyProtectedQ]];

(* A bad path must not clear the working package before either source opens. *)
legacyBeforeBadReload = DownValues[ReduceSpinorProducts];
legacyBeforeBadContext = {$Context, $ContextPath};
legacyBadReload = Block[{Global`$HEPCATpath = legacyMissingSource},
  Quiet[Get[FileNameJoin[{legacyRoot, "source", "HEPCAT.wl"}]]]];
legacyCheck["missing-source reload returns a failure",
  legacyBadReload === $Failed];
legacyCheck["missing-source reload preserves working definitions and unrelated data",
  DownValues[ReduceSpinorProducts] === legacyBeforeBadReload &&
    legacyReloadResults[] === legacyBeforeReload && legacySentinel === <|"Keep" -> {3, 5, 7}|>];
legacyCheck["missing-source reload preserves context and public protections",
  {$Context, $ContextPath} === legacyBeforeBadContext &&
    AllTrue[legacyProtectedFunctions, legacyProtectedQ]];
legacyPartialSource = CreateDirectory[];
CopyFile[FileNameJoin[{$HEPCATpath, "HEPCAT-Base.wl"}],
  FileNameJoin[{legacyPartialSource, "HEPCAT-Base.wl"}]];
legacyPartialReload = Block[{Global`$HEPCATpath = legacyPartialSource},
  Quiet[Get[FileNameJoin[{legacyRoot, "source", "HEPCAT.wl"}]]]];
DeleteDirectory[legacyPartialSource, DeleteContents -> True];
legacyCheck["a missing second source is detected before loading the first",
  legacyPartialReload === $Failed];
legacyCheck["a partial source directory preserves the complete loaded package",
  DownValues[ReduceSpinorProducts] === legacyBeforeBadReload &&
    AllTrue[legacyPublicFunctions, legacyDefinitionQ] &&
    {$Context, $ContextPath} === legacyBeforeBadContext];
legacyBadPathType = Block[{Global`$HEPCATpath = 17},
  Quiet[Get[FileNameJoin[{legacyRoot, "source", "HEPCAT.wl"}]]]];
legacyCheck["a non-string source path fails without damaging loaded definitions",
  legacyBadPathType === $Failed && DownValues[ReduceSpinorProducts] === legacyBeforeBadReload];

(* Regressions for the five formerly documented failure probes. *)
legacyCheck["powered left-left helicity contraction preserves the contraction sign",
  With[{
    left = SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Spin", "Square", 1]],
    right = SpinorChain[Spinor["Helicity", "Angle", 3], Spinor["Spin", "Angle", 4]],
    joined = SpinorChain[Spinor["Spin", "Angle", 4], Mom[3], Spinor["Spin", "Square", 1]]},
    Expand[ReduceSpinContractions[left^2 right] + left joined] === 0]];

legacyCheck["crossing flips both angle endpoints on the same chain",
  With[{chain = SpinorChain[Spinor["Helicity", "Angle", 1], Mom[7], Mom[8],
      Spinor["Helicity", "Angle", 1]]},
    ReverseMomentum[chain, 1] === chain]];

legacyCheck["crossing flips every repeated momentum insertion in a chain",
  With[{chain = SpinorChain[Spinor["Helicity", "Square", 1], Mom[2], Mom[2],
      Spinor["Helicity", "Square", 3]]},
    ReverseMomentum[chain, 2] === chain]];

legacyCheck["symmetrization is normalized when supplied spin indices coincide",
  SymmetrizeSpin[SpinorChain[Spinor["Spin", "Angle", 1], Mom[7], Spinor["Spin", "Square", 1]],
    1, {legacyIndex, legacyIndex}] ===
    SpinorChain[Spinor["Spin", "Upper", "Angle", 1, legacyIndex], Mom[7],
      Spinor["Spin", "Upper", "Square", 1, legacyIndex]]];

legacyCheck["shifted residues cancel singular factors before evaluating a sum of fractions",
  With[{pole = Multiparticle[1, 3]},
    Cancel[SumResiduesAlt[{{legacyMass, legacyMass, legacyMass, legacyMass, pole, legacyMass,
      legacyA/(z - zPlus[pole]) + legacyB/(z - zMinus[pole])}}] + legacyA + legacyB] === 0]];

If[legacyChecks =!= 137, AppendTo[legacyFailures,
  {"suite did not reach every check", "Expected" -> 137, "Actual" -> legacyChecks}]];
Print["Legacy package checks: ", legacyChecks, "; failures: ", Length[legacyFailures]];
If[legacyFailures =!= {}, Print[InputForm[legacyFailures]]];
If[legacyFailures =!= {}, Quit[1]];
Print["HEPCAT_TEST_COMPLETED:legacy-package-tests.wl"];
Quit[0];
