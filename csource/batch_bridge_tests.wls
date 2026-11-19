(* Run from a fresh kernel. HEPCAT_BATCH_BINARY points to the compiled batch CLI. *)
root = Replace[Environment["HEPCAT_ROOT"], Except[_String] :> DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root, "source", "HEPCAT.wl"}]];
Get[FileNameJoin[{DirectoryName[$InputFileName], "NativeAmplitude.wl"}]];
bin = Environment["HEPCAT_BATCH_BINARY"];
check[label_, test_] := If[!TrueQ[test], Print["FAIL ", label]; Exit[1]];
check["batch executable", StringQ[bin] && FileExistsQ[bin]];

(* Distinct contexts and mass scopes are intentional. Color and private operators
   travel unchanged even when an unrelated head is called Mom. *)
m = ModelParameter["m","bridge-diagram","Real"];
u = ModelParameter["m","bridge-channel","Real"];
v = ModelParameter["m","another-channel","Real"];
masses = {m, m, 0, 0};
rules = Thread[(Mass /@ Range[4]) -> masses];
cut = Multiparticle[1,3];
both = Join[rules,{Mass[cut] -> m, Mass[Multiparticle[2,4]] -> m}];
spectator = NativeTestOne`g + NativeTestTwo`g NativeTestOne`Mom[NativeTestTwo`x] +
  ColorGenerator[ColorIndex[5],ColorIndex[2],ColorIndex[1]];
original = MomProd[1,3];
channels = (Join[masses,{cut,#,original}] &) /@ {u,v};
result = ReduceNativeAmplitude[{{both, original}, {rules,spectator}},channels,bin,"Workers"->2];
check["reduction", AssociationQ[result]];
check["diagram ordering, untouched spectators and metadata", result["Diagrams"] === {{both,0},{rules,spectator}}];
check["discarded diagram numerator", result["DiagramRemainders"] === {original,0}];
check["separate products for two masses", Most /@ result["FactorizationProducts"] === Most /@ channels];
cuts = Last /@ result["FactorizationProducts"];
check["each row uses only its own cut mass", Expand /@ (cuts - ({u,v}^2-m^2)/2) === {0,0}];
check["off-cut recovery per row", Expand /@ (cuts + result["FactorizationRemainders"] - original) === {0,0}];
check["inconsistent masses rejected", FailureQ[ReduceNativeAmplitude[{{rules,1}},
    {ReplacePart[First[channels],1->u]},bin]]];
check["missing external mass rejected", FailureQ[ReduceNativeAmplitude[{{Rest[rules],1}}, {}, bin]]];
check["conflicting complementary cuts rejected", FailureQ[ReduceNativeAmplitude[
    {{Join[both,{Mass[Multiparticle[2,4]] -> u}],1}}, {}, bin]]];
check["empty", ReduceNativeAmplitude[{}, {}, bin, "ExternalMasses"->masses]["WorkersUsed"] === 0];
check["Feynman tensor conversion required", MatchQ[ReduceNativeAmplitude[
    {{rules, ConstructiveDiagrams`DiracSlash[Mom[1]]}}, {}, bin], Failure["NativeFeynmanTensors",_]]];

qed = ReadModel[FileNameJoin[{root,"models"}],3];
qedFactorization = ReadModel[FileNameJoin[{root,"models"}],4];
process = {"e","E","A.+","A.+"};
prepared = ReduceNativeAmplitude[process,qed,qedFactorization,bin,"PolePreference"->"Cyclic"];
check["common reference for actual generated diagrams and channels", AssociationQ[prepared] &&
  Length[prepared["Diagrams"]] === 2 && Length[prepared["FactorizationProducts"]] === 2 &&
  FreeQ[{prepared["Diagrams"],prepared["FactorizationProducts"],prepared["DiagramRemainders"],
    prepared["FactorizationRemainders"]},Spinor["Xi",___] | SpinorHat["Xi",___]]];
Print["Native Mathematica bridge checks passed."];
