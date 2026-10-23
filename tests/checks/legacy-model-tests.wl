(* Original model readers, amplitude notation, and constructive sewing APIs. *)
lmRoot = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{lmRoot, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
lmChecks = 0; lmFailures = {};
SetAttributes[lmCheck, HoldRest];
lmCheck[name_, assertion_] := Module[{result},
  lmChecks++;
  result = TimeConstrained[Check[assertion, $Failed], 10, $TimedOut];
  If[!TrueQ[result], AppendTo[lmFailures, name]; Print["FAIL: ", name]]
];
lmAngle[i_, j_, kind_: "Spin"] :=
  SpinorChain[Spinor[kind, "Angle", i], Spinor[kind, "Angle", j]];
lmSquare[i_, j_, kind_: "Spin"] :=
  SpinorChain[Spinor[kind, "Square", i], Spinor[kind, "Square", j]];

(* Reader fixtures use the public text format and are deleted before exit. *)
lmTemporaryDirectory = CreateDirectory[];
lmOriginalStreams = Streams[];
lmWrite[name_, rows_] := Export[FileNameJoin[{lmTemporaryDirectory, name}],
  StringRiffle[rows, "\n"] <> "\n", "Text"];
lmWrite["vars91.mdl", {
  "Legacy regression fixture", "Name | Value | Comment",
  "lmMass | 3/2 | fermion mass", "lmScalarMass | 2 | scalar mass",
  "lmCharge | -1/3 | coupling", "lmScale | 0.125 | decimal input", "======"}];
lmWrite["func91.mdl", {
  "Legacy regression fixture", "Name | Expression",
  "lmDerived | 2*lmCharge^2 % derived coupling",
  "lmUncommented | Sqrt[lmMass^2+lmScalarMass^2]", "======"}];
lmWrite["prtcls91.mdl", {
  "Legacy regression fixture", "Full Name | P | aP | number | spin2 | mass | width | color | aux | LaTeX(A) | LaTeX(A+)",
  "Legacy electron | le | lE | 101 | 1 | lmMass | 0 | 1 |   | e | E",
  "Legacy photon | LA.+ | LA.- | 102 | 2 | 0 | 0 | 1 |   | A | A",
  "Legacy scalar | lh | lh | 103 | 0 | lmScalarMass | 0 | 1 |   | h | h",
  "======"}];
lmWrite["amps91.mdl", {
  "Legacy regression fixture", "P1 | P2 | P3 | P4 | Factor | Amplitude/Factor",
  "% ignored | le | lE | lh | 999 | <12>",
  "le | lE | LA.+ |   | -Sqrt[2]*lmCharge | x12*<12>",
  "le | lE | LA.- |   | -Sqrt[2]*lmCharge | xt12*[12]",
  "le | lE | lh | lh | lmCharge^2 | [132>/(s13-lmMass^2)",
  "======"}];
lmParticlesExpected = {
  {"Legacy electron", "le", "lE", 101, 1, lmMass, "0", 1, "", "e", "E"},
  {"Legacy photon", "LA.+", "LA.-", 102, 2, 0, "0", 1, "", "A", "A"},
  {"Legacy scalar", "lh", "lh", 103, 0, lmScalarMass, "0", 1, "", "h", "h"}};
lmParticles = ReadParticles[FileNameJoin[{lmTemporaryDirectory, "prtcls91.mdl"}]];
lmVariables = ReadVariables[FileNameJoin[{lmTemporaryDirectory, "vars91.mdl"}]];
lmFunctions = ReadFunctions[FileNameJoin[{lmTemporaryDirectory, "func91.mdl"}]];
lmAmplitudes = ReadAmplitudes[
  FileNameJoin[{lmTemporaryDirectory, "amps91.mdl"}], lmParticles];
lmCheck["particle reader preserves names, antiparticles and display columns",
  lmParticles === lmParticlesExpected];
lmCheck["variable reader ignores headers and retains comments and exact values",
  lmVariables === {{lmMass, 3/2, "fermion mass"},
    {lmScalarMass, 2, "scalar mass"}, {lmCharge, -1/3, "coupling"},
    {lmScale, 0.125, "decimal input"}}];
lmCheck["function reader separates optional percent comments",
  lmFunctions === {{lmDerived, 2 lmCharge^2, "derived coupling"},
    {lmUncommented, Sqrt[lmMass^2 + lmScalarMass^2], ""}}];
lmCheck["readers do not assign numeric values to model symbols",
  OwnValues[lmMass] === {} && OwnValues[lmCharge] === {} &&
    OwnValues[lmDerived] === {}];
Do[
  lmCheck["model symbol plain text metadata " <> ToString[lmSymbol, InputForm],
    PlainText[lmSymbol] === SymbolName[lmSymbol]];
  lmCheck["model symbol SPINAS metadata " <> ToString[lmSymbol, InputForm],
    SPINAS[lmSymbol] === SymbolName[lmSymbol]];
  lmCheck["model symbol TeX metadata " <> ToString[lmSymbol, InputForm],
    TeX[lmSymbol] === SymbolName[lmSymbol]];
  lmCheck["model variables and functions are real " <> ToString[lmSymbol, InputForm],
    Conjugate[lmSymbol] === lmSymbol],
  {lmSymbol, {lmMass, lmScalarMass, lmCharge, lmScale, lmDerived, lmUncommented}}];
lmCheck["amplitude reader skips comment rows and parses separate couplings",
  Length[lmAmplitudes] === 3 && lmAmplitudes[[All, -2]] ===
    {-Sqrt[2] lmCharge, -Sqrt[2] lmCharge, lmCharge^2}];
lmCheck["positive helicity model row converts x and massive angle bracket",
  lmAmplitudes[[1]] === {"le", "lE", "LA.+", "", -Sqrt[2] lmCharge,
    xFactor[1, 2] lmAngle[1, 2]}];
lmCheck["negative helicity model row converts tilde x and square bracket",
  lmAmplitudes[[2]] === {"le", "lE", "LA.-", "", -Sqrt[2] lmCharge,
    xTildeFactor[1, 2] lmSquare[1, 2]}];
lmCheck["four-point notation converts momentum insertion and massive propagator",
  lmAmplitudes[[3, -1]] ===
    SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 2]]/
      PropDen[Mom[Multiparticle[1, 3]], lmMass]];
lmFixtureModel = ReadModel[lmTemporaryDirectory, 91];
lmCheck["ReadModel assembles the four original reader results in order",
  lmFixtureModel === {lmParticles, lmAmplitudes, lmVariables, lmFunctions}];
lmCheck["loading a model a second time is idempotent",
  ReadModel[lmTemporaryDirectory, 91] === lmFixtureModel];
lmCheck["model readers close every file stream", Streams[] === lmOriginalStreams];
lmReaderFunctions = {ReadVariables, ReadFunctions, ReadParticles,
  Function[file, ReadAmplitudes[file, lmParticles]]};
lmReaderNames = {"variables", "functions", "particles", "amplitudes"};
lmUnreadMetadataBefore = DownValues[PlainText];
lmWrite["unterminated.mdl", {"Malformed reader fixture", "lmUnread | 1 | comment"}];
lmWrite["empty.mdl", {"", "", "======"}];
Do[
  lmBadReaderFile = FileNameJoin[{lmTemporaryDirectory,
    If[lmMissingFile, "missing.mdl", "unterminated.mdl"]}];
  lmReaderFailure = lmReaderFunctions[[lmReaderIndex]][lmBadReaderFile];
  lmCheck["reader reports " <> If[lmMissingFile, "missing file ", "missing terminator "] <>
      lmReaderNames[[lmReaderIndex]],
    MatchQ[lmReaderFailure, Failure[
      If[lmMissingFile, "ModelFileUnavailable", "UnterminatedModelFile"], _Association]] &&
      lmReaderFailure["File"] === lmBadReaderFile];
  lmCheck["invalid reader input closes streams " <> ToString[{lmMissingFile, lmReaderIndex}],
    Streams[] === lmOriginalStreams],
  {lmMissingFile, {True, False}}, {lmReaderIndex, Length[lmReaderFunctions]}];
Do[
  lmCheck["blank lines before an empty terminated model table are harmless " <>
      lmReaderNames[[lmReaderIndex]],
    lmReaderFunctions[[lmReaderIndex]][FileNameJoin[{lmTemporaryDirectory, "empty.mdl"}]] === {}],
  {lmReaderIndex, Length[lmReaderFunctions]}];
lmCheck["an unterminated file is rejected before defining any row metadata",
  DownValues[PlainText] === lmUnreadMetadataBefore && OwnValues[lmUnread] === {}];
Do[
  lmReaderFileNames = (# <> "92.mdl" &) /@ {"prtcls", "amps", "vars", "func"};
  Do[lmWrite[lmReaderFileName, {"======"}], {lmReaderFileName, lmReaderFileNames}];
  DeleteFile[FileNameJoin[{lmTemporaryDirectory, lmReaderFileNames[[lmMissingComponent]]}]];
  lmModelFailure = ReadModel[lmTemporaryDirectory, 92];
  lmCheck["ReadModel propagates component failure " <> ToString[lmMissingComponent],
    FailureQ[lmModelFailure] && lmModelFailure["File"] ===
      FileNameJoin[{lmTemporaryDirectory, lmReaderFileNames[[lmMissingComponent]]}]],
  {lmMissingComponent, 4}];
DeleteDirectory[lmTemporaryDirectory, DeleteContents -> True];
lmCheck["temporary model fixture is removed", !DirectoryQ[lmTemporaryDirectory]];

(* Independent expected conversions exercise mixed massless/massive slots. *)
Do[
  lmCheck["particle and antiparticle mass lookup " <> First[lmCase],
    ParticleMass[First[lmCase], lmParticles] === Last[lmCase]],
  {lmCase, {{"le", lmMass}, {"lE", lmMass}, {"lh", lmScalarMass},
    {"LA.+", 0}, {"LA.-", 0}, {"", 0}, {"not-in-model", -1}}}];
lmConversionParticles = {"le", "LA.+", "lE", "LA.-"};
Do[
  lmCheck["mixed angle endpoints " <> ToString[{lmI, lmJ}],
    ConvertAmplitude[Join[lmConversionParticles,
      {1, "<" <> ToString[lmI] <> ToString[lmJ] <> ">"}], lmParticles] ===
      SpinorChain[
        Spinor[If[MemberQ[{2, 4}, lmI], "Helicity", "Spin"], "Angle", lmI],
        Spinor[If[MemberQ[{2, 4}, lmJ], "Helicity", "Spin"], "Angle", lmJ]]];
  lmCheck["mixed square endpoints " <> ToString[{lmI, lmJ}],
    ConvertAmplitude[Join[lmConversionParticles,
      {1, "[" <> ToString[lmI] <> ToString[lmJ] <> "]"}], lmParticles] ===
      SpinorChain[
        Spinor[If[MemberQ[{2, 4}, lmI], "Helicity", "Spin"], "Square", lmI],
        Spinor[If[MemberQ[{2, 4}, lmJ], "Helicity", "Spin"], "Square", lmJ]]],
  {lmI, 4}, {lmJ, 4}];
lmCheck["notation conversion preserves sums, coefficients, powers and both pole masses",
  ConvertAmplitude[Join[lmConversionParticles,
    {1, "3*<12>^2/s24-2*[13]/(s13-lmMass^2)"}], lmParticles] ===
      3 SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle", 2]]^2/
        PropDen[Mom[Multiparticle[2, 4]], 0] -
      2 lmSquare[1, 3]/PropDen[Mom[Multiparticle[1, 3]], lmMass]];
lmCheck["notation conversion supports mixed endpoint momentum chains",
  ConvertAmplitude[Join[lmConversionParticles, {1, "[243>"}], lmParticles] ===
    SpinorChain[Spinor["Helicity", "Square", 2], Mom[4], Spinor["Spin", "Angle", 3]]];
lmPositiveRow = {"le", "lE", "LA.+", "", lmCharge,
  xFactor[1, 2] lmAngle[1, 2] + xTildeFactor[1, 2] lmSquare[1, 2]};
lmNegativeRow = {"le", "lE", "LA.-", "", lmCharge,
  xTildeFactor[1, 2] lmSquare[1, 2] + xFactor[1, 2] lmAngle[1, 2]};
lmCheck["opposite-helicity generation flips brackets, x factors and particle suffix",
  AddOppositeHelicityAmplitudes[{lmPositiveRow}] === {lmPositiveRow, lmNegativeRow}];
lmCheck["opposite-helicity generation leaves helicity-free rows alone",
  AddOppositeHelicityAmplitudes[{lmAmplitudes[[3]]}] === {lmAmplitudes[[3]]}];
lmCheck["opposite-helicity conversion is an involution for each appended row",
  Last[AddOppositeHelicityAmplitudes[{lmNegativeRow}]] === lmPositiveRow];

(* A spin-two internal state needs all four of the supplied sewing indices. *)
lmTensorNumerator = lmAngle[1, 2] lmSquare[1, 2] *
  SpinorChain[Spinor["Spin", "Angle", 1], Mom[3], Mom[1], Spinor["Spin", "Angle", 2]] *
  SpinorChain[Spinor["Spin", "Square", 1], Mom[2], Mom[3], Spinor["Spin", "Square", 2]];
lmTensorModel = {{{"Scalar", "s", "s", 201, 0, lmScalarMass, "0", 1, "", "s", "s"},
    {"Tensor", "T", "T", 202, 4, lmTensorMass, "0", 1, "", "T", "T"}},
  {{"T", "T", "s", "", lmTensorCoupling, lmTensorNumerator}}, {}, {}};
lmTensorCut = Multiparticle[3, 4];
Do[
  lmTensorLookup = LookupAmplitude[{{"s", 1}, {"T", 2}, {"T", lmTensorCut}},
    lmTensorModel, lmTensorRight, {lmI1, lmI2, lmI3, lmI4}];
  lmCheck["spin-two lookup supplies four explicit internal indices " <> ToString[lmTensorRight],
    Union[Cases[lmTensorLookup,
      Spinor["Spin", _, _, lmTensorCut, index_] :> index, Infinity]] ===
        {lmI1, lmI2, lmI3, lmI4} &&
      FreeQ[lmTensorLookup, Spinor["Spin", _, lmTensorCut]]];
  lmCheck["spin-two lookup assigns the correct sewing side " <> ToString[lmTensorRight],
    DeleteDuplicates[Cases[lmTensorLookup,
      Spinor["Spin", position_, _, lmTensorCut, _] :> position, Infinity]] ===
        {If[lmTensorRight, "Lower", "Upper"]}],
  {lmTensorRight, {False, True}}];

(* Tags distinguish identical external legs; complementary cuts occur once. *)
Do[
  lmExternalNames = ConstantArray["same-scalar", lmN];
  lmPartitions = FactorizationPartitions[lmExternalNames];
  lmCuts = Sort[Sort /@ (#[[All, All, 2]] & /@ lmPartitions)];
  lmCheck["factorization partition count n=" <> ToString[lmN],
    Length[lmPartitions] === If[lmN < 4, 0, 2^(lmN - 1) - lmN - 1]];
  lmCheck["factorization partitions conserve every tagged leg n=" <> ToString[lmN],
    AllTrue[lmPartitions, Sort[Flatten[#[[All, All, 2]]]] === Range[lmN] &]];
  lmCheck["factorization sides have at least two legs n=" <> ToString[lmN],
    AllTrue[lmPartitions, Min[Length /@ #] >= 2 &]];
  lmCheck["complementary factorization cuts are unique n=" <> ToString[lmN],
    DuplicateFreeQ[lmCuts]],
  {lmN, 0, 8}];

(* A hand-derived scalar cubic theory tests the old S/T/U sewing contract. *)
lmScalarModel = {{{"Scalar", "s", "s", 201, 0, lmScalarMass, "0", 1, "", "s", "s"}},
  {{"s", "s", "s", "", lmCubic, 1}}, {}, {}};
lmCheck["original three-point constructor returns the coupling",
  ConstructiveDiagram[{"s", "s", "s"}, lmScalarModel] === lmCubic];
lmCheck["original three-point constructor rejects an absent particle",
  ConstructiveDiagram[{"s", "s", "unknown"}, lmScalarModel] === 0];
lmNoVertices = ReplacePart[lmScalarModel, 2 -> {}];
lmCheck["original three-point constructor returns zero for an absent vertex",
  ConstructiveDiagram[{"s", "s", "s"}, lmNoVertices] === 0];
Do[
  lmCut = lmLegacyCase[[1]]; lmFunction = lmLegacyCase[[2]];
  lmExpected = -lmCubic^2/PropDen[Total[Mom /@ lmCut], lmScalarMass];
  lmCheck["legacy channel wrapper has the three-i sewing sign " <> ToString[lmCut],
    lmFunction["s", "s", "s", "s", "s", lmScalarModel] === lmExpected];
  lmCheck["legacy four-point constructor matches direct scalar sewing " <> ToString[lmCut],
    ConstructiveDiagram[ConstantArray["s", 4], "s", lmScalarModel, lmCut] === lmExpected];
  lmCheck["legacy scalar exchange vanishes without a vertex " <> ToString[lmCut],
    lmFunction["s", "s", "s", "s", "s", lmNoVertices] === 0],
  {lmLegacyCase, {{{1, 2}, SChannelDiagram}, {{1, 3}, TChannelDiagram},
    {{1, 4}, UChannelDiagram}}}];
lmRawScalarChannels = FactorizationAmplitudeChannels[ConstantArray["s", 4], lmScalarModel];
lmCheck["original factorization output consists of compact mass-cut-mass-expression rows",
  Length[lmRawScalarChannels] === 3 && AllTrue[lmRawScalarChannels,
    Length[#] === 7 && Take[#, 4] === ConstantArray[lmScalarMass, 4] &&
      MatchQ[#[[-3]], Multiparticle[_Integer, _Integer]] &&
      #[[-2]] === lmScalarMass && FreeQ[#, _Association] &]];
lmCheck["raw scalar channel expression is the product over z and one propagator",
  AllTrue[lmRawScalarChannels,
    Last[#] === lmCubic^2/(z PropDen[Mom[#[[-3]]], lmScalarMass]) &]];
lmCheck["cubic-only model has no five-point factorization without known four-point rows",
  FactorizationAmplitudeChannels[ConstantArray["s", 5], lmScalarModel] === {}];

(* Legacy channel simplification retains compact rows and each row's shells. *)
lmCheck["simplifying no factorization channels returns an empty list",
  SimplifyFactorizationChannels[{}] === {}];
lmSavedScalarChannels = lmRawScalarChannels;
lmSimplifiedScalarChannels = TimeConstrained[
  SimplifyFactorizationChannels[lmRawScalarChannels], 15, $TimedOut];
lmCheck["already simple cubic scalar channels remain unchanged",
  lmSimplifiedScalarChannels === lmRawScalarChannels];
lmCheck["factorization channel simplification does not mutate its input",
  lmRawScalarChannels === lmSavedScalarChannels];
lmCheck["scalar channel simplification preserves physical propagators off shell",
  Cases[lmSimplifiedScalarChannels, _PropDen, Infinity] ===
    Cases[lmRawScalarChannels, _PropDen, Infinity]];
lmInvariantChannelFixtures = {
  {{1, 2, 3, 4}, {1, 2}, 5},
  {{2, 3, 5, 7}, {1, 3}, 11},
  {{lmM1, lmM2, lmM3, lmM4}, {2, 4}, lmExchangeMass}};
lmInvariantChannels = Function[fixture, Join[fixture[[1]],
  {Multiparticle @@ fixture[[2]], fixture[[3]],
    (MomProd @@ fixture[[2]])/
      (z PropDen[Mom[Multiparticle @@ fixture[[2]]], fixture[[3]]])}]] /@
  lmInvariantChannelFixtures;
lmInvariantChannelsBefore = lmInvariantChannels;
lmSimplifiedInvariantChannels = TimeConstrained[
  SimplifyFactorizationChannels[lmInvariantChannels], 15, $TimedOut];
Do[
  lmInvariantFixture = lmInvariantChannelFixtures[[lmRowIndex]];
  lmInvariantCut = lmInvariantFixture[[2]];
  lmInvariantDenominator = z PropDen[Mom[Multiparticle @@ lmInvariantCut],
    lmInvariantFixture[[3]]];
  lmExpectedNumerator = (lmInvariantFixture[[3]]^2 -
    Total[lmInvariantFixture[[1, lmInvariantCut]]^2])/2;
  lmCheck["channel-specific invariant reduces using its own masses row " <> ToString[lmRowIndex],
    Expand[Last[lmSimplifiedInvariantChannels[[lmRowIndex]]] lmInvariantDenominator -
      lmExpectedNumerator] === 0];
  lmCheck["channel simplification preserves each row's compact metadata " <> ToString[lmRowIndex],
    Most[lmSimplifiedInvariantChannels[[lmRowIndex]]] ===
      Most[lmInvariantChannels[[lmRowIndex]]]];
  lmCheck["channel simplification never puts its propagator denominator on shell row " <>
      ToString[lmRowIndex],
    Cases[lmSimplifiedInvariantChannels[[lmRowIndex, -1]], _PropDen, Infinity] ===
      Cases[lmInvariantChannels[[lmRowIndex, -1]], _PropDen, Infinity]],
  {lmRowIndex, Length[lmInvariantChannelFixtures]}];
lmCheck["distinct-row shell assumptions do not overwrite the original channels",
  lmInvariantChannels === lmInvariantChannelsBefore];

(* Lookup must preserve the model's ordering and typed kinematic labels. *)
lmLookupModel = {lmParticles, {{"le", "lE", "lh", "", lmCharge, lmAngle[1, 2]}}, {}, {}};
Do[
  lmTagged = { {"le", lmLabels[[1]]}, {"lE", lmLabels[[2]]}, {"lh", lmLabels[[3]]} };
  lmCheck["lookup relabels original ordered spinors " <> ToString[lmLabels],
    LookupAmplitude[lmTagged, lmLookupModel, False, {lmI1, lmI2, lmI3, lmI4}] ===
      lmCharge lmAngle[lmLabels[[1]], lmLabels[[2]]]],
  {lmLabels, Permutations[{1, 2, 3}]}];
lmCheck["lookup of an amplitude absent from the model is zero",
  LookupAmplitude[{{"le", 1}, {"lE", 2}, {"LA.+", 3}}, lmLookupModel,
    False, {lmI1, lmI2, lmI3, lmI4}] === 0];
lmCheck["lookup recognizes scalar propagating particles without spin indices",
  LookupAmplitude[{{"s", 1}, {"s", 2}, {"s", Multiparticle[3, 4]}},
    lmScalarModel, True, {lmI1, lmI2, lmI3, lmI4}] === lmCubic];
lmCheck["lookup makes an internal particle spin index explicit and upper",
  LookupAmplitude[{{"lE", 2}, {"lh", 3}, {"le", Multiparticle[1, 4]}},
    lmLookupModel, False, {lmI1, lmI2, lmI3, lmI4}] ===
      lmCharge SpinorChain[Spinor["Spin", "Upper", "Angle", Multiparticle[1, 4], lmI1],
        Spinor["Spin", "Angle", 2]]];
lmCheck["lookup makes an internal antiparticle spin index explicit and lower",
  LookupAmplitude[{{"le", 1}, {"lh", 3}, {"lE", Multiparticle[2, 4]}},
    lmLookupModel, False, {lmI1, lmI2, lmI3, lmI4}] ===
      lmCharge SpinorChain[Spinor["Spin", "Angle", 1],
        Spinor["Spin", "Lower", "Angle", Multiparticle[2, 4], lmI1]]];
Do[
  lmNamedParticles = {"le", "lE", "lh"}[[lmPermutation]];
  lmCheck["original three-point constructor relabels a spinor vertex " <> ToString[lmPermutation],
    ConstructiveDiagram[lmNamedParticles, lmLookupModel] ===
      lmCharge lmAngle[First[First[Position[lmNamedParticles, "le"]]],
        First[First[Position[lmNamedParticles, "lE"]]]]],
  {lmPermutation, Permutations[Range[3]]}];
lmDistinctScalarModel = {
  MapIndexed[Function[{name, index}, {"Scalar " <> name, name, name, 300 + First[index], 0,
    If[name === "s", lmScalarMass, lmMass], "0", 1, "", name, name}],
    {"a", "b", "c", "d", "s"}],
  {{"a", "b", "s", "", lmLeftCoupling, 1},
    {"c", "d", "s", "", lmRightCoupling, 1}}, {}, {}};
lmCheck["distinct scalar sewing multiplies the two different cubic couplings",
  SChannelDiagram["a", "b", "c", "d", "s", lmDistinctScalarModel] ===
    -lmLeftCoupling lmRightCoupling/PropDen[Mom[1] + Mom[2], lmScalarMass]];
lmCheck["forbidden t channel does not invent a cubic vertex",
  TChannelDiagram["a", "b", "c", "d", "s", lmDistinctScalarModel] === 0];
lmCheck["forbidden u channel does not invent a cubic vertex",
  UChannelDiagram["a", "b", "c", "d", "s", lmDistinctScalarModel] === 0];

(* Original alternative complex-momentum reconstruction helpers. *)
lmHatChain = SpinorChain[SpinorHat["Spin", "Angle", 1], MomHat[2],
  SpinorHat["Spin", "Square", 3]];
lmCheck["UnHatSpinors removes spinor hats but leaves shifted momenta intact",
  UnHatSpinors[lmHatChain] === SpinorChain[Spinor["Spin", "Angle", 1], MomHat[2],
    Spinor["Spin", "Square", 3]]];
lmCheck["unhat spinor conversion is idempotent",
  UnHatSpinors[UnHatSpinors[lmHatChain]] === UnHatSpinors[lmHatChain]];
lmCheck["alternative momentum expansion separates ordinary and shift chains",
  ExpandMomHatAlt[lmHatChain] ===
    SpinorChain[SpinorHat["Spin", "Angle", 1], Mom[2], SpinorHat["Spin", "Square", 3]] +
    z SpinorChain[SpinorHat["Spin", "Angle", 1], MomQ[2], SpinorHat["Spin", "Square", 3]]];
lmPole = Multiparticle[1, 3];
lmCheck["alternative propagator expansion preserves its two shifted roots",
  ExpandMomHatAlt[PropDen[MomHat[lmPole], lmMass]] ===
    PropDen[Mom[lmPole], lmMass] (z - zPlus[lmPole]) (z - zMinus[lmPole])/
      (zPlus[lmPole] zMinus[lmPole])];
(* The contour helper must not depend on how its rational expression is arranged. *)
lmResidueRows = {{lmMass, lmMass, lmMass, lmMass, lmPole, lmMass,
  Together[lmA/(z - zPlus[lmPole]) + lmB/(z - zMinus[lmPole]) ]}};
lmCheck["alternative contour reconstruction sums both simple residues with minus sign",
  Cancel[SumResiduesAlt[lmResidueRows] + lmA + lmB] === 0];
lmCheck["alternative contour reconstruction sums all channel rows",
  Cancel[SumResiduesAlt[Join[lmResidueRows, lmResidueRows]] + 2 lmA + 2 lmB] === 0];
lmCheck["alternative contour reconstruction of no channels is zero", SumResiduesAlt[{}] === 0];

lmResidueInput[expression_, pole_: lmPole] :=
  {{lmMass, lmMass, lmMass, lmMass, pole, lmMass, expression}};
lmPartialFractions = lmA/(z - zPlus[lmPole]) + lmB/(z - zMinus[lmPole]);
lmResidueForms = {lmPartialFractions, Together[lmPartialFractions],
  Factor[Together[lmPartialFractions]], Expand[Together[lmPartialFractions]]};
Do[
  lmCheck["shifted residues are independent of expression layout " <> ToString[lmIndex],
    Cancel[SumResiduesAlt[lmResidueInput[lmResidueForms[[lmIndex]]]] + lmA + lmB] === 0],
  {lmIndex, Length[lmResidueForms]}];
lmCheck["shifted residues ignore regular polynomial terms",
  Cancel[SumResiduesAlt[lmResidueInput[lmPartialFractions + 3 + 2 z + z^2]] +
    lmA + lmB] === 0];
lmCheck["shifted residues evaluate analytic numerators at their own roots",
  Cancel[SumResiduesAlt[lmResidueInput[(lmA + z)/(z - zPlus[lmPole]) +
      (lmB + z^2)/(z - zMinus[lmPole])]] + lmA + zPlus[lmPole] +
    lmB + zMinus[lmPole]^2] === 0];
lmCheck["an absent shifted pole contributes no residue",
  Cancel[SumResiduesAlt[lmResidueInput[lmA/(z - zPlus[lmPole])]] + lmA] === 0];
lmCheck["a removable shifted pole contributes no residue",
  SumResiduesAlt[lmResidueInput[(z^2 - zPlus[lmPole]^2)/(z - zPlus[lmPole])]] === 0];
lmOtherPole = Multiparticle[2, 4];
lmCheck["distinct channel rows cancel and evaluate their own pole factors",
  Cancel[SumResiduesAlt[Join[lmResidueInput[lmPartialFractions],
    lmResidueInput[2 lmA/(z - zPlus[lmOtherPole]) +
      3 lmB/(z - zMinus[lmOtherPole]), lmOtherPole]]] + 3 lmA + 4 lmB] === 0];
lmPhysicalDenominator = PropDen[Mom[Multiparticle[1, 2]], lmMass];
lmCheck["shifted residues retain physical off-shell propagator factors",
  Module[{result = SumResiduesAlt[lmResidueInput[lmPartialFractions/lmPhysicalDenominator]]},
    Cancel[Together[result + (lmA + lmB)/lmPhysicalDenominator]] === 0 &&
      DeleteDuplicates[Cases[result, _PropDen, Infinity]] === {lmPhysicalDenominator}]];
lmCheck["shifted residue evaluation leaves its input rows unchanged",
  Module[{rows = lmResidueInput[lmPartialFractions], before},
    before = rows; SumResiduesAlt[rows]; rows === before]];

If[lmChecks =!= 211,
  AppendTo[lmFailures, "complete legacy model test inventory executed"]];
Print[<|"Checks" -> lmChecks, "Failed" -> Length[lmFailures], "Failures" -> lmFailures|>];
Print["HEPCAT_TEST_COMPLETED:legacy-model-tests.wl"];
Exit[If[lmFailures === {}, 0, 1]];
