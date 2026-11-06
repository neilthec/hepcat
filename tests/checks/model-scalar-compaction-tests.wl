(* Final scalar coefficients use declared symbolic functions, never numerical
   variable defaults. Typed kinematics and model scopes remain independent. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0; checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++;
  If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);
msModel = ReadModel[FileNameJoin[{root, "models"}], 6];
msParameter[name_] := ModelParameter[msModel, name];
msCompact = ConstructiveDiagrams`Private`preparationModelScalarCompaction;
msExpand = ConstructiveDiagrams`Private`modelExpandParameterDefinitions;
msRank = ConstructiveDiagrams`Private`preparationRank;
msBetter = ConstructiveDiagrams`Private`preparationBetterQ;
msA = SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]];
msPole = PropDen[Mom[Multiparticle[1, 3]], msParameter["MZ"]];
msRules = Thread[Mass /@ Range[4] -> {msParameter["Me"], msParameter["Me"], 0, 0}];
msZeroCoefficient = 1 + msParameter["gLe"] - 2 msParameter["SW"]^2;
msInput = msZeroCoefficient msA/msPole;
msReport = msCompact[msInput, msModel, 1];
check["the actual SM chiral-coupling definition cancels a zero coefficient",
  msReport["Status"] === "IMPROVED" && msReport["Amplitude"] === 0];
check["a changed scalar coefficient has an exact declared-definition certificate",
  First[msReport["Route"]]["EquivalenceProof"]["NormalForm"] === 0 &&
  First[msReport["Route"]]["EquivalenceProof"]["Method"] ===
    "ExactIndependentAtomModelDefinitionIdentity"];
check["the route records no internal on-shell conditions",
  msReport["InternalOnShellConditions"] === {} &&
    First[msReport["Route"]]["InternalOnShellConditions"] === {}];
msMassCoefficient = msParameter["MZ"]^2 msParameter["CW"]^2 - msParameter["MW"]^2 + 1;
msMassInput = msMassCoefficient msA/msPole;
msMassReport = msCompact[msMassInput, msModel, 1];
check["transitive symbolic mass definitions cancel scalar coefficients",
  msMassReport["Amplitude"] === msA/msPole];
check["a propagator retains its original declared mass and momentum",
  Cases[msMassReport["Amplitude"], _PropDen, Infinity] === {msPole}];
msWidthPole = PropDen[Mom[Multiparticle[1, 3]], msParameter["MZ"], msWidth];
check["a complete width-bearing propagator is opaque",
  msCompact[msMassCoefficient msA/msWidthPole, msModel, 1]["Amplitude"] === msA/msWidthPole];
msInverse = 1/(msA + SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]);
check["an inverse spinor expression stays an intact factor",
  msCompact[msMassCoefficient msA msInverse/msPole, msModel, 1]["Amplitude"] === msA msInverse/msPole];
msForeign = ModelParameter["gLe", "another-model-scope", "Real"];
msForeignInput = (1 + msForeign - 2 msParameter["SW"]^2) msA/msPole;
check["matching printed names from a different model never borrow definitions",
  msCompact[msForeignInput, msModel, 1]["Amplitude"] === msForeignInput];
msIndependent = (msParameter["Me"] - 1/2000) msA/msPole;
check["an independent parameter is never replaced by its numerical default",
  msCompact[msIndependent, msModel, 1]["Amplitude"] === msIndependent];
msSimple = msParameter["gLe"] msA/msPole;
check["an already shorter dependent-parameter name is retained",
  msCompact[msSimple, msModel, 1]["Amplitude"] === msSimple];
msOverrideReport = Block[{ConstructiveDiagrams`Private`$preparationModelParameterOverrides =
    {"gLe" -> 17}}, msCompact[msInput, msModel, 1]];
check["an explicitly overridden dependent coupling remains symbolic and independent",
  msOverrideReport["Amplitude"] === msInput];
msWeakAngleIdentity = (msParameter["CW"]^2 - 1 + msParameter["SW"]^2) msA/msPole;
check["an overridden weak-angle function no longer supplies its defining relation",
  Block[{ConstructiveDiagrams`Private`$preparationModelParameterOverrides =
      {"CW" -> 1/3}}, msCompact[msWeakAngleIdentity, msModel, 1]["Amplitude"]] === msWeakAngleIdentity];
check["other declared functions still expand transitively through an overridden symbolic input",
  Block[{ConstructiveDiagrams`Private`$preparationModelParameterOverrides =
      {"CW" -> 1/3}}, msCompact[msMassInput, msModel, 1]["Amplitude"]] === msA/msPole];
check["a dynamically scoped override creates no persistent package state",
  OwnValues[ConstructiveDiagrams`Private`$preparationModelParameterOverrides] === {}];
msUnrelated = msParameter["EE"]^2 msA/msPole;
check["a defined mass inside only a propagator does not specialize it",
  msCompact[msUnrelated, msModel, 1]["Amplitude"] === msUnrelated];
msColor = ColorDelta[ColorIndex[1], ColorIndex[2]];
check["canonical color objects stay intact while their scalar coefficient simplifies",
  msCompact[msMassCoefficient msColor msA/msPole, msModel, 1]["Amplitude"] === msColor msA/msPole];
msExternalParameter = ModelParameter["External", "scalar-compaction-test", "Real"];
msExternalModel = msModel;
msExternalModel[[4]] = Append[msExternalModel[[4]],
  {msExternalParameter, ModelFunction["userFunction", "scalar-compaction-test", {}]}];
check["an external model function is never evaluated or invented",
  msCompact[msExternalParameter msA/msPole, msExternalModel, 1]["Amplitude"] ===
    msExternalParameter msA/msPole];
msCycleParameter = ModelParameter["Cycle", "scalar-compaction-test", "Real"];
msCycleModel = msModel;
msCycleModel[[4]] = Join[Select[msCycleModel[[4]], First[#] =!= msParameter["gLe"] &],
  {{msParameter["gLe"], msCycleParameter}, {msCycleParameter, msParameter["gLe"]}}];
check["a cyclic symbolic definition retains the original expression",
  msCompact[msInput, msCycleModel, 1]["Amplitude"] === msInput];
msBranchInput = Sqrt[msParameter["gLe"]^2] msA/msPole;
check["a square root is never replaced by an unsupported signed branch",
  msCompact[msBranchInput, msModel, 1]["Amplitude"] === msBranchInput];
check["a partial model without parameter tables keeps the original expression",
  msCompact[msInput, Take[msModel, 2], 1]["Amplitude"] === msInput];
msNoncommutative = msInput Dot[msLeft, msRight];
check["noncommutative expressions are declined",
  msCompact[msNoncommutative, msModel, 1]["Amplitude"] === msNoncommutative];
check["a zero budget leaves the original expression available",
  msCompact[msInput, msModel, 0]["Amplitude"] === msInput];
msTimed = Internal`InheritedBlock[{ConstructiveDiagrams`Private`modelExpandParameterDefinitions},
  Clear[ConstructiveDiagrams`Private`modelExpandParameterDefinitions];
  ConstructiveDiagrams`Private`modelExpandParameterDefinitions[___] := (Pause[1]; msInput);
  First[AbsoluteTiming[msTimedReport = msCompact[msInput, msModel, 0.02]]]];
check["a slow symbolic dependency resolver respects the shared deadline",
  msTimed < 0.5 && msTimedReport["TimedOut"] === True && msTimedReport["Amplitude"] === msInput];
msNoTokens[value_] := FreeQ[value, symbol_Symbol /;
  StringStartsQ[SymbolName[symbol], {"modelCoefficientAtom", "modelCoefficientInverse"}]];
check["temporary kinematic tokens never escape into an amplitude",
  msNoTokens[{msReport["Amplitude"], msMassReport["Amplitude"]}]];
msPrepared = PrepareAmplitudes[{msRules, msInput}, msModel,
  "InternalOnShell" -> False, "PolePreference" -> None, "MaxSteps" -> 0,
  "TimeConstraint" -> 3, "Parallelization" -> False, "ProgressReporting" -> False,
  "ReturnDetails" -> True];
check["ordinary final-amplitude preparation invokes declared scalar compaction",
  AssociationQ[msPrepared] && Last[First[msPrepared["PreparedAmplitudes"]]] === 0 &&
    MemberQ[Lookup[First[msPrepared["ItemReports"]]["Reductions"], "Operation"],
      "DeclaredModelScalarCompaction"]];
check["the integration preserves the public external mass-list format",
  First[First[msPrepared["PreparedAmplitudes"]]] === msRules];
check["the accepted result strictly improves the established rank",
  msBetter[msRank[msReport["Amplitude"]], msRank[msInput]]];
check["model compaction creates no global parameter assignments",
  OwnValues[msWidth] === {} && OwnValues[msLeft] === {} && OwnValues[msRight] === {}];
Print[checks, " model-scalar-compaction checks; ", Length[failures], " failures."];
If[failures =!= {}, Print[failures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:model-scalar-compaction-tests.wl"]; Quit[0];
