(* Model parameters are inert data, scoped to a model rather than global names. *)
miRoot = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{miRoot, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
miChecks = 0; miFailures = {};
SetAttributes[miCheck, HoldRest];
miCheck[name_, assertion_] := Module[{result},
  miChecks++;
  result = TimeConstrained[Check[assertion, $Failed], 30, $TimedOut];
  If[!TrueQ[result], AppendTo[miFailures, name]; Print["FAIL: ", name]]
];
If[AnyTrue[{ReadModel, ModelParameterName, ModelParameterRules}, DownValues[#] === {} &],
  Print["Model-isolation APIs did not load."]; Quit[1]];
miDirectory = CreateDirectory[];
miConstructiveDirectory = CreateDirectory[FileNameJoin[{miDirectory, "constructive"}]];
miFeynmanDirectory = CreateDirectory[FileNameJoin[{miDirectory, "feynman"}]];
miWrite[dir_, file_, lines_] := Export[FileNameJoin[{dir, file}],
  StringRiffle[lines, "\n"] <> "\n", "Text"];
miFixture[dir_, number_Integer, charge_, type_] := Module[{suffix = ToString[number]},
  miWrite[dir, "prtcls" <> suffix <> ".mdl", {
    "Model isolation fixture", "Particles",
    "Full Name | P | aP | number | spin2 | mass | width | color | aux | LaTeX(A) | LaTeX(A+)",
    "scalar | h | h | 25 | 0 | MIModelMass | 0 | 1 | | h | h", "======"}];
  miWrite[dir, "vars" <> suffix <> ".mdl", {
    "Model isolation fixture", "Name | Value | Comment",
    "EE | " <> ToString[charge, InputForm] <> " | coupling",
    "MIModelMass | 1 | scalar mass", "======"}];
  miWrite[dir, "func" <> suffix <> ".mdl", {
    "Model isolation fixture", "Name | Expression",
    "MIModelDerived | MIModelIntermediate+1",
    "MIModelIntermediate | 2*EE^2",
    "MIModelExternal | MbEff(EE)", "======"}];
  miWrite[dir, If[type === "Feynman", "lgrng", "amps"] <> suffix <> ".mdl", {
    "Model isolation fixture", "Lagrangian",
    "P1 | P2 | P3 | P4 | Factor | Lorentz part",
    "h | h | h | | EE | 1", "======"}]
];
miFixture[miConstructiveDirectory, 91, 2, "Constructive"];
miFixture[miFeynmanDirectory, 91, 3, "Feynman"];
miFixture[miConstructiveDirectory, 92, 5, "Constructive"];
miFixture[miConstructiveDirectory, 93, 2, "Constructive"];
miNamesBefore = {Names["Global`MI*"], Names["ConstructiveDiagrams`MI*"],
  Names["Global`MbEff"], Names["ConstructiveDiagrams`MbEff"]};
miDefinitionSnapshot = DownValues /@ {Conjugate, PlainText, SPINAS, TeX};
miSavedUserValues = OwnValues[Global`EE];
Global`EE = 991;
miSentinelValues = OwnValues[Global`EE];
miOriginalStreams = Streams[];
miConstructive = ReadModel[miConstructiveDirectory, 91];
miFeynman = ReadModel[miFeynmanDirectory, 91];
miOtherSuffix = ReadModel[miConstructiveDirectory, 92];
miParameter[model_, name_String] := ModelParameter[model, name];
miChargeA = miParameter[miConstructive, "EE"];
miChargeB = miParameter[miFeynman, "EE"];
miMassA = miParameter[miConstructive, "MIModelMass"];
miMassB = miParameter[miFeynman, "MIModelMass"];
miDerivedA = miParameter[miConstructive, "MIModelDerived"];
miDerivedB = miParameter[miFeynman, "MIModelDerived"];
miExternalA = miParameter[miConstructive, "MIModelExternal"];
miRulesA = ModelParameterRules[miConstructive];
miRulesB = ModelParameterRules[miFeynman];

miCheck["constructive and Feynman model readers preserve their four-part organization",
  MatchQ[miConstructive, {_List, _List, _List, _List}] &&
    MatchQ[miFeynman, FeynmanModel[_List, _List, _List, _List]]];
miCheck["model parameters are inert named data with a scope and domain",
  AllTrue[{miChargeA, miChargeB, miMassA, miMassB},
    MatchQ[#, ModelParameter[_String, _String, _String]] &]];
miCheck["the parameter-name accessor does not depend on an ordinary symbol",
  ModelParameterName[miChargeA] === "EE" &&
    ModelParameterName[miDerivedB] === "MIModelDerived"];
miCheck["identically named parameters from two model directories remain distinct",
  miChargeA =!= miChargeB && miMassA =!= miMassB];
miCheck["different parameter declarations retain distinct model scopes",
  miParameter[miOtherSuffix, "EE"] =!= miChargeA &&
    (miParameter[miOtherSuffix, "EE"] /. ModelParameterRules[miOtherSuffix]) === 5];
miCheck["identical parameter declarations in one directory share their data identity",
  ModelParameter[ReadModel[miConstructiveDirectory, 93], "EE"] === miChargeA];
miCheck["unknown parameter names return an explicit failure",
  FailureQ[ModelParameter[miConstructive, "NoSuchParameter"]]];
miCheck["all reader components refer to the same model-local parameters",
  miConstructive[[1, 1, 6]] === miMassA &&
    !FreeQ[miConstructive[[2]], miChargeA] &&
    !FreeQ[miConstructive[[4]], miChargeA] &&
    miFeynman[[1, 1, 6]] === miMassB && !FreeQ[miFeynman[[2]], miChargeB]];
miCheck["same-named numerical definitions are resolved independently",
  {miChargeA /. miRulesA, miChargeB /. miRulesB} === {2, 3}];
miCheck["out-of-order derived expressions resolve within their own model",
  {miDerivedA /. miRulesA, miDerivedB /. miRulesB} === {9, 19}];
miCheck["a preexisting user EE OwnValue neither contaminates nor changes model data",
  OwnValues[Global`EE] === miSentinelValues && Global`EE === 991 &&
    {miChargeA /. miRulesA, miChargeB /. miRulesB} === {2, 3}];
miCheck["readers install no individual named parameter symbols or definitions",
  {Names["Global`MI*"], Names["ConstructiveDiagrams`MI*"],
    Names["Global`MbEff"], Names["ConstructiveDiagrams`MbEff"]} === miNamesBefore];
miCheck["reading and resolving two models does not change global conjugation or export rules",
  (DownValues /@ {Conjugate, PlainText, SPINAS, TeX}) === miDefinitionSnapshot];
miCheck["reading a model again preserves its parameter identity and values",
  ReadModel[miConstructiveDirectory, 91] === miConstructive &&
    ReadModel[miFeynmanDirectory, 91] === miFeynman];
miCheck["repeated model reads leave global formatting and conjugation definitions unchanged",
  (DownValues /@ {Conjugate, PlainText, SPINAS, TeX}) === miDefinitionSnapshot];
miCheck["the public variable reader shares the ReadModel scope",
  ReadVariables[FileNameJoin[{miConstructiveDirectory, "vars91.mdl"}]] === miConstructive[[3]]];
miCheck["the public function reader shares the ReadModel scope",
  ReadFunctions[FileNameJoin[{miConstructiveDirectory, "func91.mdl"}]] === miConstructive[[4]]];
miCheck["reading and resolving model data closes all opened streams", Streams[] === miOriginalStreams];
miCheck["ordinary parameter display does not expose storage identifiers",
  PlainText[miChargeA] === "EE" && SPINAS[miChargeA] === "EE" && TeX[miChargeA] === "EE" &&
    ToString[miChargeA, OutputForm] === "EE"];
miCheck["declared real model parameters have the intended conjugation behavior",
  Conjugate[miChargeA] === miChargeA];
miCheck["the real-domain declaration is explicit stored model data",
  miChargeA[[3]] === "Real" && miChargeB[[3]] === "Real"];
miComplex = ModelParameter["MIComplex", "ConjugationFixture", "Complex"];
miCheck["parameters not declared real do not lose their complex conjugation",
  Head[Conjugate[miComplex]] === Conjugate && Conjugate[miComplex] =!= miComplex];
miCheck["unknown external functions remain inert data and are not evaluated",
  !FreeQ[miConstructive[[4]], ModelFunction["MbEff", _String, {miChargeA}]] &&
    !FreeQ[miFeynman[[4]], ModelFunction["MbEff", _String, {miChargeB}]] &&
    !MemberQ[First /@ miRulesA, miExternalA]];

miScopedOverride = ModelParameterRules[miConstructive, {miChargeA -> 4}];
miNamedOverride = ModelParameterRules[miConstructive, {"EE" -> 4}];
miCheck["explicit model-local parameter overrides recompute derived definitions",
  ({miChargeA, miDerivedA} /. miScopedOverride) === {4, 33}];
miCheck["string-name overrides are interpreted only within the supplied model",
  ({miChargeA, miDerivedA} /. miNamedOverride) === {4, 33} &&
    ({miChargeB, miDerivedB} /. ModelParameterRules[miFeynman]) === {3, 19}];
miCheck["a scoped override from another model cannot change a same-named parameter",
  ({miChargeB, miDerivedB} /.
    ModelParameterRules[miFeynman, {miChargeA -> 7}]) === {3, 19}];
miCheck["resolving defaults and overrides leaves every model unchanged",
  ReadModel[miConstructiveDirectory, 91] === miConstructive &&
    ReadModel[miFeynmanDirectory, 91] === miFeynman];
miIntermediateA = ModelParameter[miConstructive, "MIModelIntermediate"];
miCyclic = ReplacePart[miConstructive, 4 -> {
  {miDerivedA, miIntermediateA + 1, "cycle"},
  {miIntermediateA, miDerivedA - 1, "cycle"}}];
miCyclicRules = ModelParameterRules[miCyclic];
miCheck["cyclic scoped dependencies remain unresolved rather than looping",
  !NumericQ[miDerivedA /. miCyclicRules] && !NumericQ[miIntermediateA /. miCyclicRules]];
miCheck["a cycle does not discard unrelated model-local numerical definitions",
  (miChargeA /. miCyclicRules) === 2 && (miMassA /. miCyclicRules) === 1];

miSnapshot = FileNameJoin[{miDirectory, "models.wl"}];
Block[{$Context = "ModelIsolationSerialization`", $ContextPath = {"System`"}},
  Put[{miConstructive, miFeynman}, miSnapshot]];
miReloaded = Get[miSnapshot];
miCheck["ordinary model serialization preserves all scoped parameter identities",
  miReloaded === {miConstructive, miFeynman}];
miCheck["serialized models retain independent default and derived numerical values",
  {miChargeA /. ModelParameterRules[miReloaded[[1]]],
    miChargeB /. ModelParameterRules[miReloaded[[2]]],
    miDerivedB /. ModelParameterRules[miReloaded[[2]]]} === {2, 3, 19}];

miPairA = {Thread[(Mass /@ Range[4]) -> ConstantArray[miMassA, 4]], miChargeA^2};
miPairB = {Thread[(Mass /@ Range[4]) -> ConstantArray[miMassB, 4]], miChargeB^2};
miCheckOptions = {"SpuriousPoles" -> {}, "HighEnergyCheck" -> False,
  "ReferenceCheck" -> False, "Samples" -> 1, "TimeConstraint" -> 30,
  "FeynmanAmplitudes" -> {miPairB}, "FeynmanModel" -> miFeynman,
  "Output" -> "Details"};
miMismatch = CheckAmplitudes[{miPairA}, {}, miConstructive, Sequence @@ miCheckOptions];
miCheck["the full benchmark uses each model's own values, detecting different couplings",
  AssociationQ[miMismatch] && miMismatch["FeynmanCheck"]["Status"] === "FAIL"];
miMatchingFeynman = ReplacePart[miFeynman,
  {3, First[FirstPosition[First /@ miFeynman[[3]], miChargeB]], 2} -> 2];
miMatch = CheckAmplitudes[{miPairA}, {}, miConstructive,
  Sequence @@ ReplaceAll[miCheckOptions, HoldPattern["FeynmanModel" -> _] :>
    ("FeynmanModel" -> miMatchingFeynman)]];
miCheck["matching explicit model data gives agreement without sharing parameter objects",
  miChargeA =!= miChargeB && AssociationQ[miMatch] &&
    miMatch["FeynmanCheck"]["Status"] === "PASS"];
miCheck["numerical benchmark comparisons do not alter user variables or either model",
  OwnValues[Global`EE] === miSentinelValues &&
    (miChargeA /. ModelParameterRules[miConstructive]) === 2 &&
    (miChargeB /. ModelParameterRules[miFeynman]) === 3];
miCheck["overrides, serialization and checking never mutate global parameter formatting rules",
  (DownValues /@ {Conjugate, PlainText, SPINAS, TeX}) === miDefinitionSnapshot];

miSamplingMass = ModelParameter["DifferentMassName", "DifferentModelScope", "Real"];
miSamplingChannel[mass_] := <|"ExternalLegs" -> Range[4], "Cut" -> {1, 2},
  "Complement" -> {3, 4}, "Mass" -> mass,
  "ExternalMassRules" -> Thread[(Mass /@ Range[4]) -> ConstantArray[mass, 4]]|>;
miNormalizedSample[mass_, rules_, sample_] := Module[{result, chart},
  result = ConstructiveDiagrams`Private`residueAutomaticChart[
    miSamplingChannel[mass], sample, 1729, rules];
  If[!AssociationQ[result], Return[result]];
  chart = result["Chart"];
  chart["Momenta"] /. chart["PoleVariable"] -> miCommonPole];
miCheck["physical sample momenta depend on numeric masses, not parameter names or scopes",
  miMassA =!= miSamplingMass &&
    AllTrue[Range[2], Function[sample,
      With[{left = miNormalizedSample[miMassA, miRulesA, sample],
          right = miNormalizedSample[miSamplingMass, {miSamplingMass -> 1}, sample]},
        AssociationQ[left] && left === right]]]];

OwnValues[Global`EE] = miSavedUserValues;
DeleteDirectory[miDirectory, DeleteContents -> True];
If[miChecks =!= 36, AppendTo[miFailures, "Unexpected completed check count"]];
Print[miChecks, " model-isolation checks; ", Length[miFailures], " failures."];
If[miFailures =!= {}, Print[miFailures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:model-isolation-tests.wl"];
Quit[0];
