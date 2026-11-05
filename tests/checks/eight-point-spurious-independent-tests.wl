(* Opt-in: obtain an independent all-spurious eight-point Feynman expression
   before generating the proposed all-N current. No factorization answer or
   general-current expression is supplied to the reference calculation. *)
e8Root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{e8Root, "source", "HEPCAT.wl"}]];
$HistoryLength = 0;
e8Checks = 0; e8Failures = {};
SetAttributes[e8Check, HoldRest];
e8Check[name_, test_] := (e8Checks++;
  If[!TrueQ[test], AppendTo[e8Failures, name]; Print["FAIL: ", name]]);
e8Cubic = ReadModel[FileNameJoin[{e8Root, "models"}], 2];
e8Model = ReadModel[FileNameJoin[{e8Root, "models"}], 4];
e8Feynman = ReadModel[FileNameJoin[{e8Root, "model_feynman"}], 2];
e8Before = {e8Cubic, e8Model, e8Feynman};
e8Process = Join[{"e", "E"}, ConstantArray["A.+", 6]];
e8References = Thread[Range[3, 8] -> RotateLeft[Range[3, 8]]];
e8Cache = Environment["HEPCAT_EIGHT_SPURIOUS_REFERENCE"];
e8Inputs = {"source/HEPCAT-Feynman.wl", "source/HEPCAT-Base.wl",
  "source/HEPCAT-Parameters.wl", "model_feynman/prtcls2.mdl",
  "model_feynman/lgrng2.mdl", "model_feynman/vars2.mdl", "model_feynman/func2.mdl"};
If[StringQ[e8Cache] && e8Cache =!= "",
  e8Metadata = Import[FileNameJoin[{DirectoryName[e8Cache], "fresh-conversion-status.json"}], "RawJSON"];
  e8CacheValid = AssociationQ[e8Metadata] &&
    TrueQ[e8Metadata["FreshConversionCompleted"]] &&
    e8Metadata["ConvertedDiagrams"] === 720 &&
    e8Metadata["GeneralSpuriousFormulaUsed"] === False &&
    e8Metadata["InternalOnShell"] === False &&
    e8Metadata["SHA256"]["ConvertedPairs"] === FileHash[e8Cache, "SHA256", "HexString"] &&
    AllTrue[e8Inputs, Lookup[e8Metadata["ReferenceInputSHA256"], #, Missing[]] ===
      FileHash[FileNameJoin[{e8Root, #}], "SHA256", "HexString"] &];
  e8Check["cached independent reference matches its original sources and models", e8CacheValid];
  If[!TrueQ[e8CacheValid], Quit[2]];
  e8Reference = Import[e8Cache, "WXF"],
  Print["Obtaining 720 independent Feynman diagrams with shared cyclic references."];
  e8FeynmanGraphs = CreateDiagrams[e8Process, e8Feynman];
  e8Check["the independent Feynman inventory has 720 diagrams", Length[e8FeynmanGraphs] === 720];
  e8Raw = DiagramAmplitudes[e8FeynmanGraphs];
  e8Reference = TimeConstrained[MemoryConstrained[
    FeynmanToSpinorProducts[e8Raw, "ReferenceSpinors" -> e8References,
      "ReduceSpinorProducts" -> False, "TimeConstraint" -> 360],
    3221225472, Missing["ReferenceMemoryLimit"]], 370, Missing["ReferenceTimeLimit"]];
  Clear[e8FeynmanGraphs, e8Raw]
];
e8Check["independent AllSpurious case is complete before evaluating the prediction",
  ListQ[e8Reference] && Length[e8Reference] === 720 &&
    FreeQ[e8Reference, _Failure | _Missing | $Failed | $Aborted | $TimedOut |
      (Spinor | SpinorHat)["Xi", ___] | _MomHat | _SpinorHat]];
If[e8Failures =!= {}, Print[e8Failures]; Quit[3]];
e8Mass = ModelParameter[e8Model, "Me"];
e8External = Thread[Mass /@ Range[8] -> Join[{e8Mass, e8Mass}, ConstantArray[0, 6]]];
e8Graphs = CreateDiagrams[e8Process, e8Cubic];
e8Proposal = ConstructiveDiagrams`Private`constructVectorLineForm[
  e8Graphs, e8Process, e8Cubic, e8External, "AllSpurious"];
e8Check["the prediction uses the certified cubic topology and unchanged general recurrence",
  Length[e8Graphs] === 720 && AssociationQ[e8Proposal] &&
    e8Proposal["Method"] === "SameChiralSpuriousSubsetCurrent"];
If[!AssociationQ[e8Proposal], Quit[4]];
e8Expression = e8Proposal["Expression"];
e8Brackets = DeleteDuplicates[Cases[e8Expression,
  Power[chain_SpinorChain, _Integer?Negative] :> chain, Infinity]];
e8Check["the six-photon prediction retains resolved spurious brackets",
  e8Brackets =!= {} && FreeQ[e8Expression, (Spinor | SpinorHat)["Xi", ___]]];
e8Check["six-photon compactness does not regress from the verified formula",
  SpinorExpressionComplexity[e8Expression] <= 28050];

(* These62 channels supply geometric pole metadata only. No StateSum is
   asserted or used. Every comparison stays away from every physical cut. *)
e8Channels = ConstructiveDiagrams`Private`residueSimpleChannel[
    Prepend[#, 1], e8Mass, Range[8], e8External, e8Model] & /@
  Select[Subsets[Range[3, 8]], 0 < Length[#] < 6 &];
e8ReferenceBody = Last /@ e8Reference;
e8CyclicBrackets = Table[SpinorChain[Spinor["Helicity", "Angle", i /. e8References],
  Spinor["Helicity", "Angle", i]], {i, 3, 8}];
Do[
  e8Rules = ModelParameterRules[e8Model, {"Me" -> e8MassValue, "EE" -> 2/7}];
  e8FeynmanRules = ModelParameterRules[e8Feynman, {"Me" -> e8MassValue, "EE" -> 2/7}];
  e8Accepted = False;
  Do[
    e8Chart = ConstructiveDiagrams`Private`amplitudeRationalComparisonChart[
      e8Channels, e8Sample + 7 e8Retry, 202611058006 + 100 e8Sample + e8Retry, e8Rules];
    If[!AssociationQ[e8Chart] || !TrueQ[e8Chart["OffCutKinematicValidation"]["Valid"]], Continue[]];
    e8PoleValues = ConstructiveDiagrams`Private`residueEvaluateChart[
      Join[e8CyclicBrackets, e8Brackets], e8Chart, e8Rules];
    If[!ListQ[e8PoleValues] || !AllTrue[e8PoleValues,
      ConstructiveDiagrams`Private`residueFiniteQ[#] &&
        !ConstructiveDiagrams`Private`residueExactZero[#] &], Continue[]];
    e8Calculated = ConstructiveDiagrams`Private`residueEvaluateChart[e8Expression, e8Chart, e8Rules];
    e8Expected = Total[ConstructiveDiagrams`Private`residueEvaluateChart[
      e8ReferenceBody, e8Chart, e8FeynmanRules]];
    e8Spin = e8Chart["SpinVariables"];
    e8Difference = Expand[e8Calculated - e8Expected];
    e8Polynomial = PolynomialQ[e8Difference, e8Spin] &&
      And @@ Thread[Exponent[e8Difference, e8Spin] <= 1];
    e8Coefficients = If[e8Polynomial, Table[Cancel[Together[
      Coefficient[Coefficient[e8Difference, e8Spin[[1]], a], e8Spin[[2]], b]]],
      {a, 0, 1}, {b, 0, 1}], {Missing["NotPolynomial"]}];
    e8Check["exact off-cut coherent-spin agreement at mass " <> ToString[e8MassValue] <>
        " sample " <> ToString[e8Sample],
      Length[e8Channels] === 62 &&
        And @@ e8Chart["OffCutKinematicValidation"]["PhysicalPropagatorsNonzero"] &&
        e8Polynomial && Flatten[e8Coefficients] === {0, 0, 0, 0}];
    e8Accepted = True; Break[], {e8Retry, 0, 4}];
  e8Check["a regular exact chart was obtained at mass " <> ToString[e8MassValue] <>
    " sample " <> ToString[e8Sample], e8Accepted],
  {e8MassValue, {1, 3/2}}, {e8Sample, 1, 2}];
e8Check["the supplied model data are unchanged", e8Before === {e8Cubic, e8Model, e8Feynman}];
Print["EIGHT-POINT INDEPENDENT SPURIOUS CHECKS: ", e8Checks, "; FAILURES: ", Length[e8Failures]];
If[e8Failures === {}, Print["HEPCAT_TEST_COMPLETED:eight-point-spurious-independent-tests.wl"], Print[e8Failures]];
Quit[If[e8Failures === {}, 0, 1]];
