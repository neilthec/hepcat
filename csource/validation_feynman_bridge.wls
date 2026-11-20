(* HEPCAT_ROOT and HEPCAT_BATCH_BINARY can point to an out-of-tree build. *)
$HistoryLength = 0;
base = DirectoryName[$InputFileName];
root = Replace[Environment["HEPCAT_ROOT"], Except[_String] :> DirectoryName[base]];
Get[FileNameJoin[{root, "source", "HEPCAT.wl"}]];
Get[FileNameJoin[{base, "NativeAmplitude.wl"}]];
models = FileNameJoin[{root, "models"}];
dir = FileNameJoin[{root, "tests", "reference-models", "calchep"}];
bin = Environment["HEPCAT_BATCH_BINARY"];
check[condition_, label_] := If[!TrueQ[condition], Print["FAIL ", label]; Exit[1]];

process = {"e", "E", "A.+", "A.-"};
plain = GenerateNativeAmplitude[process, models, 3, 4, bin, "Workers" -> 2];
joint = GenerateNativeAmplitude[process, models, 3, 4, bin, "Workers" -> 2, "FeynmanModel" -> {dir, 2}];
separate = GenerateNativeAmplitude[process, dir, 2, bin, "Workers" -> 1];
check[AllTrue[{plain, joint, separate}, AssociationQ], "generation"];
check[!KeyExistsQ[plain, "FeynmanDiagrams"], "original result format"];
check[Lookup[plain, {"Diagrams", "FactorizationProducts"}] ===
      Lookup[joint, {"Diagrams", "FactorizationProducts"}], "constructive rows preserved"];
check[Length[joint["FeynmanDiagrams"]] == 2 && joint["FeynmanDiagrams"] === separate["Diagrams"],
      "attached and standalone off-shell Feynman rows and mass scopes"];
feynman = ReadModel[dir, 2];
expected = DiagramAmplitudes[CreateDiagrams[process, feynman]];
residual = Total[Last /@ FeynmanToSpinorProducts[joint["FeynmanDiagrams"]]] -
           Total[Last /@ FeynmanToSpinorProducts[expected]];
check[Together[Expand[residual]] === 0, "signed Mathematica Feynman oracle"];
check[AllTrue[joint["FeynmanDiagrams"], FreeQ[First[#], _Multiparticle]&], "only external mass rules"];
Print["Joint constructive and Feynman generation bridge passed."];

(* Independently contract each native raw tree in Mathematica and compare
   signed spinor expressions with the native conversion diagram by diagram. *)
oracleBinary = Replace[Environment["HEPCAT_FEYNMAN_TEST_BINARY"],
  Except[_String] :> FileNameJoin[{DirectoryName[bin], "test_validation_feynman"}]];
records = RunProcess[{oracleBinary, dir, "--emit-oracle"}];
check[AssociationQ[records] && records["ExitCode"] === 0, "native conversion oracle records"];
groups = GatherBy[StringSplit[#, "\t"] & /@ StringSplit[records["StandardOutput"], "\n"], First];
Do[
  seconds = First @ AbsoluteTiming[
    Do[
      raw = ToExpression[row[[3]], InputForm];
      converted = ToExpression[row[[4]], InputForm];
      fromWolfram = FeynmanToSpinorProducts[raw, "ReduceSpinorProducts" -> False];
      check[!FailureQ[fromWolfram], row[[1]] <> " Wolfram tensor conversion"];
      externalRules = DeleteDuplicates@Cases[raw,
        (FermionBar[leg_, mass_, _] | FermionKet[leg_, mass_, _] |
          PolarizationVector[leg_, _, mass_, _]) :> (Mass[leg] -> mass), Infinity];
      diff = Expand[ReduceSpinorProducts[Expand[fromWolfram - converted]] /. externalRules];
      check[Together[diff] === 0, row[[1]] <> " signed diagram " <> row[[2]]],
      {row, group}]];
  Print[group[[1,1]], "   ", Length[group], " signed diagram(s) agree exactly, ", Round[seconds, 0.001], " s"],
  {group, groups}];
check[Length[groups] === 10 && Total[Length /@ groups] === 41, "complete oracle coverage"];
Exit[0];
