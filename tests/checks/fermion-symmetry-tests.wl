(* Identical fermions transform in the signed exchange representation. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0;
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++; If[!TrueQ[value],
  AppendTo[failures, name]; Print["FAIL: ", name]]);
fsInfer = ConstructiveDiagrams`Private`completionInferFermionGroups;
fsPlan = ConstructiveDiagrams`Private`completionExchangePlan;
fsProject = ConstructiveDiagrams`Private`completionBoseProject;
fsSwap = ConstructiveDiagrams`Private`completionBoseCycle;
fsNormalize[expression_, labels_List, masses_List] := <|
  "Expression" -> expression, "ExternalMassRules" -> Thread[(Mass /@ labels) -> masses],
  "Targets" -> {}, "Records" -> {}|>;
fsEndpoint[leg_, rest___] := SpinorChain[Spinor["Spin", "Angle", leg, rest],
  Spinor["Xi", "Angle", 100 + leg]];
fsPhoton[leg_] := SpinorChain[Spinor["Helicity", "Square", leg],
  Spinor["Xi", "Square", 100 + leg]]^2;
fsModel = {{{"electron", "e", "E", 11, 1, fsMass, 0, 1, 1, "", ""},
  {"photon", "A.+", "A.-", 22, 2, 0, 0, 1, 1, "", ""}}, {}, {}, {}};
fsNames = {"e", "E", "E", "e", "A.+"};
fsState = Times @@ (fsEndpoint /@ Range[4]) fsPhoton[5];
fsNormalized = fsNormalize[fsState, Range[5], {fsMass, fsMass, fsMass, fsMass, 0}];
check["fermion exchange is automatic without changing the Bose default",
  ("FermionSymmetryGroups" /. Options[CompleteFactorization]) === Automatic &&
    ("BoseSymmetryGroups" /. Options[CompleteFactorization]) === Automatic &&
    ("FermionSymmetryGroups" /. Options[ConstructAmplitude]) === Automatic];
fsInfo = fsInfer[fsNormalized, fsModel, fsNames];
check["particle and antiparticle exchange groups are identified separately",
  fsInfo["Groups"] === {{1, 4}, {2, 3}}];
check["equal electron and positron masses do not identify an unlabeled species",
  fsInfer[fsNormalized, fsModel]["Groups"] === {}];
check["fermions are still excluded from the legacy boson-only inference",
  ConstructiveDiagrams`Private`completionInferBoseGroups[
    fsNormalized, fsModel, fsNames]["Groups"] === {}];
fsAmbiguous = fsModel;
fsAmbiguous[[1]] = Append[fsAmbiguous[[1]],
  {"other fermion", "f", "F", 13, 1, fsMass, 0, 1, 1, "", ""}];
check["different equal-mass fermion species are not antisymmetrized together",
  fsInfer[fsNormalized, fsAmbiguous, {"e", "E", "F", "f", "A.+"}]["Groups"] === {}];
fsTwoStates = fsNormalize[fsEndpoint[1, 1] fsEndpoint[2, 2], {1, 2}, {fsMass, fsMass}];
check["different fixed massive-spin projections remain separate components",
  fsInfer[fsTwoStates, fsModel, {"e", "e"}]["Groups"] === {}];
fsSameStates = fsNormalize[fsEndpoint[1, 1] fsEndpoint[2, 1], {1, 2}, {fsMass, fsMass}];
check["identical fixed massive-spin projections allow exchange",
  fsInfer[fsSameStates, fsModel, {"e", "e"}]["Groups"] === {{1, 2}}];
fsVariance = fsNormalize[SpinorChain[Spinor["Spin", "Upper", "Angle", 1, 1],
  Spinor["Xi", "Angle", 101]] SpinorChain[Spinor["Spin", "Lower", "Angle", 2, 1],
  Spinor["Xi", "Angle", 102]], {1, 2}, {fsMass, fsMass}];
check["different spin-index variances are not silently identified",
  fsInfer[fsVariance, fsModel, {"e", "e"}]["Groups"] === {}];
fsMasslessModel = {{{"massless fermion", "f.+", "f.-", 1, 1, 0, 0, 1, 1, "", ""}}, {}, {}, {}};
fsMassless = SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Helicity", "Square", 2]] *
  SpinorChain[Spinor["Helicity", "Angle", 3], Spinor["Helicity", "Angle", 4]];
check["opposite massless helicities form distinct fermionic state groups",
  fsInfer[fsNormalize[fsMassless, Range[4], ConstantArray[0, 4]], fsMasslessModel,
    {"f.+", "f.+", "f.-", "f.-"}]["Groups"] === {{1, 2}, {3, 4}}];
fsColored = fsModel;
fsColored[[1, 1, 8]] = 3;
check["color-stripped fermion amplitudes are not assigned full exchange symmetry",
  fsInfer[fsNormalized, fsColored, fsNames]["Groups"] === {}];
check["unsupported spinor expressions do not invent compatible external states",
  fsInfer[fsNormalize[fsOpaque[fsState],Range[5],{fsMass,fsMass,fsMass,fsMass,0}],
    fsModel,fsNames]["Groups"] === {}];
fsExchange = fsPlan[fsNormalized, fsModel, Automatic, Automatic, fsNames];
check["the two identical-fermion pairs produce four signed permutations",
  fsExchange["BoseGroups"] === {} && fsExchange["FermionGroups"] === {{1, 4}, {2, 3}} &&
    fsExchange["Order"] === 4 && Sort[fsExchange["Signs"]] === {-1, -1, 1, 1}];
fsToy = fsFunction[Mom[1], Mom[4], Mom[2], Mom[3]];
fsExpected = (fsFunction[Mom[1], Mom[4], Mom[2], Mom[3]] -
  fsFunction[Mom[4], Mom[1], Mom[2], Mom[3]] -
  fsFunction[Mom[1], Mom[4], Mom[3], Mom[2]] +
  fsFunction[Mom[4], Mom[1], Mom[3], Mom[2]])/4;
fsProjected = fsProject[fsToy, fsExchange];
check["the normalized projector is exactly (1-P14)(1-P23)/4",
  Expand[fsProjected - fsExpected] === 0];
check["either single fermion transposition changes the projected sign",
  Expand[fsProjected + fsSwap[fsProjected, {1, 4}]] === 0 &&
    Expand[fsProjected + fsSwap[fsProjected, {2, 3}]] === 0];
check["exchanging both identical pairs has positive parity",
  Expand[fsProjected - fsSwap[fsSwap[fsProjected, {1, 4}], {2, 3}]] === 0];
check["signed projection is idempotent without another Wick sign or normalization",
  Expand[fsProject[fsProjected, fsExchange] - fsProjected] === 0];
check["symmetric contamination is removed rather than amplified",
  fsProject[MomProd[1, 4] + MomProd[2, 3], fsExchange] === 0];
fsUnsorted = fsPlan[fsNormalized, fsModel, {}, {{4, 1}, {3, 2}}, fsNames];
check["unsorted explicit groups retain a positive identity and the same projector",
  !FailureQ[fsUnsorted] &&
    Expand[fsProject[fsToy, fsUnsorted] - fsProjected] === 0 &&
    First[fsUnsorted["Signs"]] === 1];
fsDisabled = fsPlan[fsNormalized, fsModel, Automatic, {}, fsNames];
check["fermion opt-out alone leaves an identity plan for this process",
  fsDisabled["FermionGroups"] === {} && fsDisabled["Order"] === 1 &&
    fsProject[fsToy, fsDisabled] === fsToy];
fsMixedNames = Append[fsNames, "A.+"];
fsMixed = fsNormalize[fsState fsPhoton[6], Range[6], {fsMass, fsMass, fsMass, fsMass, 0, 0}];
fsMixedPlan = fsPlan[fsMixed, fsModel, Automatic, Automatic, fsMixedNames];
check["boson and fermion symmetry combine into one correctly normalized group",
  fsMixedPlan["BoseGroups"] === {{5, 6}} &&
    fsMixedPlan["FermionGroups"] === {{1, 4}, {2, 3}} &&
    fsMixedPlan["Order"] === 8 && Sort[fsMixedPlan["Signs"]] === {-1,-1,-1,-1,1,1,1,1}];
fsMixedToy=fsFunction[Mom[1],Mom[4],Mom[2],Mom[3],Mom[5],Mom[6]];
fsMixedProjected=fsProject[fsMixedToy,fsMixedPlan];
check["one simultaneous projector is symmetric in photons and antisymmetric in each fermion pair",
  Expand[fsMixedProjected-fsSwap[fsMixedProjected,{5,6}]]===0 &&
    Expand[fsMixedProjected+fsSwap[fsMixedProjected,{1,4}]]===0 &&
    Expand[fsMixedProjected+fsSwap[fsMixedProjected,{2,3}]]===0];
fsInherited=ConstructiveDiagrams`Private`completionInheritedExchangePlan[
  Join[fsMixed,<|"ExchangeSymmetry"->KeyDrop[fsMixedPlan,"Permutations"],
    "ExternalParticles"->fsMixedNames|>],fsModel];
check["high-energy completion inherits the full combined exchange action",
  fsInherited["Order"]===8 &&
    Expand[fsProject[fsMixedToy,fsInherited]-fsMixedProjected]===0];
fsZeroInherited=ConstructiveDiagrams`Private`completionInheritedExchangePlan[
  Join[fsMixed,<|"Expression"->0,
    "ExchangeSymmetry"->KeyDrop[fsMixedPlan,"Permutations"],
    "ExternalParticles"->fsMixedNames|>],fsModel];
check["a completed zero expression retains the validated combined exchange action without reinference",
  !FailureQ[fsZeroInherited] && fsZeroInherited["Order"]===8 &&
    fsZeroInherited["BoseGroups"]===fsMixedPlan["BoseGroups"] &&
    fsZeroInherited["FermionGroups"]===fsMixedPlan["FermionGroups"] &&
    fsZeroInherited["Signs"]===fsMixedPlan["Signs"] &&
    fsZeroInherited["RequestedOrder"]===fsMixedPlan["RequestedOrder"] &&
    fsZeroInherited["FullGroupUsed"]===fsMixedPlan["FullGroupUsed"] &&
    fsProject[0,fsZeroInherited]===0];
fsHEBasis=ConstructiveDiagrams`Private`completionExchangeBasis[
  {fsMixedToy,2fsMixedToy,MomProd[1,4]+MomProd[2,3]+MomProd[5,6]},
  fsInherited,fsMixed["ExternalMassRules"]];
check["high-energy correction directions preserve Bose and Fermi symmetry together",
  Length[fsHEBasis]===2 && AllTrue[fsHEBasis,
    Expand[#+fsSwap[#,{1,4}]]===0 && Expand[#+fsSwap[#,{2,3}]]===0 &&
      Expand[#-fsSwap[#,{5,6}]]===0&]];
fsBoseOnly = fsPlan[fsMixed, fsModel, Automatic, {}, fsMixedNames];
fsFermiOnly = fsPlan[fsMixed, fsModel, {}, Automatic, fsMixedNames];
check["boson and fermion opt-outs are independent",
  fsBoseOnly["BoseGroups"] === {{5, 6}} && fsBoseOnly["FermionGroups"] === {} &&
    fsFermiOnly["BoseGroups"] === {} && fsFermiOnly["FermionGroups"] === {{1,4},{2,3}}];
fsThreeNames = {"e", "e", "e", "E"};
fsThreeNorm = fsNormalize[Times @@ (fsEndpoint /@ Range[4]), Range[4], ConstantArray[fsMass,4]];
fsThreePlan = fsPlan[fsThreeNorm,fsModel,{},{{1,2,3}},fsThreeNames];
fsThree = fsProject[fsFunction[Mom[1],Mom[2],Mom[3]],fsThreePlan];
check["a three-cycle of identical fermions is even",
  fsThreePlan["Order"] === 6 && Sort[fsThreePlan["Signs"]] === {-1,-1,-1,1,1,1} &&
    Expand[fsThree-fsSwap[fsThree,{1,2,3}]] === 0];
check["a transposition within three identical fermions is odd",
  Expand[fsThree+fsSwap[fsThree,{1,3}]] === 0];
fsNonconsecutive = fsNormalize[Times @@ (fsEndpoint /@ {2,4,7,9}),
  {2,4,7,9,11}, {fsMass,fsMass,fsMass,fsMass,0}];
fsNonconsecutivePlan = fsPlan[fsNonconsecutive,fsModel,{},{{2,9},{4,7}}];
check["explicit exchanges support nonconsecutive physical particle labels",
  !FailureQ[fsNonconsecutivePlan] && fsNonconsecutivePlan["Order"] === 4];
fsTripleLabels={8,2,5,11};
fsTripleNonconsecutive=fsNormalize[Times@@(fsEndpoint/@fsTripleLabels),fsTripleLabels,
  ConstantArray[fsMass,4]];
fsTripleUnsortedPlan=fsPlan[fsTripleNonconsecutive,fsModel,{},{{8,2,5}}];
fsTripleUnsorted=fsProject[fsFunction[Mom[8],Mom[2],Mom[5]],fsTripleUnsortedPlan];
check["unsorted nonconsecutive three-cycles retain even parity",
  fsTripleUnsortedPlan["Order"]===6 &&
    Expand[fsTripleUnsorted-fsSwap[fsTripleUnsorted,{8,2,5}]]===0 &&
    Expand[fsTripleUnsorted+fsSwap[fsTripleUnsorted,{8,2}]]===0];
fsTyped = {3,Spinor["Xi","Angle",1],Mom[Multiparticle[1,5]],
  PropDen[Mom[Multiparticle[1,3]],fsMass],VectorSpinIndex[1,2],ColorIndex[4],
  Spinor["Spin","Upper","Angle",1,1]};
check["momentum and typed external slots move while numeric data and Xi IDs stay fixed",
  fsSwap[fsTyped,{1,4}] === {3,Spinor["Xi","Angle",1],Mom[Multiparticle[4,5]],
    PropDen[Mom[Multiparticle[4,3]],fsMass],VectorSpinIndex[4,2],ColorIndex[1],
    Spinor["Spin","Upper","Angle",4,1]}];
check["overlapping boson and fermion exchange sets are rejected",
  FailureQ[fsPlan[fsMixed,fsModel,{{5,6}},{{1,5}},fsMixedNames]]];
check["overlapping identical-fermion groups are rejected",
  FailureQ[fsPlan[fsNormalized,fsModel,{},{{1,4},{4,2}},fsNames]]];
fsLargeNorm=fsNormalize[Times@@(fsEndpoint/@Range[6]),Range[6],ConstantArray[fsMass,6]];
fsLarge=fsPlan[fsLargeNorm,fsModel,Automatic,Automatic,ConstantArray["e",6]];
check["automatic large groups use a bounded exact signed subgroup",
  !FailureQ[fsLarge] && fsLarge["DetectedFermionGroups"] === {Range[6]} &&
    1 < fsLarge["Order"] <= 120 && Length[fsLarge["Permutations"]] === fsLarge["Order"] &&
    Length[fsLarge["Signs"]] === fsLarge["Order"]];
check["explicit factorial work beyond the combined cap is rejected",
  FailureQ[fsPlan[fsLargeNorm,fsModel,{}, {Range[6]},ConstantArray["e",6]]]];

fsCapNames={"e","e","e","A.+","A.+","A.+","A.+"};
fsCapNorm=fsNormalize[Times@@(fsEndpoint/@Range[3]) Times@@(fsPhoton/@Range[4,7]),
  Range[7],{fsMass,fsMass,fsMass,0,0,0,0}];
check["the work cap applies to the product of Bose and Fermi groups",
  FailureQ[fsPlan[fsCapNorm,fsModel,{{4,5,6,7}},{{1,2,3}},fsCapNames]]];
fsCapAutomatic=fsPlan[fsCapNorm,fsModel,Automatic,Automatic,fsCapNames];
check["automatic combined symmetry uses an exact subgroup within its shared cap",
  !FailureQ[fsCapAutomatic] && 1<fsCapAutomatic["Order"]<=120 &&
    fsCapAutomatic["DetectedBoseGroups"]==={{4,5,6,7}} &&
    fsCapAutomatic["DetectedFermionGroups"]==={{1,2,3}}];

fsReservationNames=Join[ConstantArray["e",5],{"A.+","A.+"}];
fsReservationNorm=fsNormalize[Times@@(fsEndpoint/@Range[5]) fsPhoton[6] fsPhoton[7],
  Range[7],Join[ConstantArray[fsMass,5],{0,0}]];
fsReservation=fsPlan[fsReservationNorm,fsModel,Automatic,Automatic,fsReservationNames];
check["a large fermion group cannot consume the shared budget before boson exchange is represented",
  fsReservation["RequestedOrder"]===240 && fsReservation["FullGroupUsed"]===False &&
    fsReservation["Order"]===48 && fsReservation["BoseGroups"]==={{6,7}} &&
    fsReservation["FermionGroups"] =!= {} &&
    fsReservation["MaximumOrder"]===120];

(* The saved QED formulas already carry Wick signs. Projection must leave
   their normalization and all independent physical checks unchanged. *)
fsRealModel=ReadModel[FileNameJoin[{root,"models"}],3];
fsDiagramModel=ReadModel[FileNameJoin[{root,"models"}],2];
fsFeynmanModel=ReadModel[FileNameJoin[{root,"model_feynman"}],2];
fsLookup[particles_List] := LookupAmplitude[MapIndexed[{#1,First[#2]}&,particles],
  fsRealModel,False,{fsSpin1,fsSpin2,fsSpin3,fsSpin4}];
fsCanonical[expression_,n_Integer] := ConstructiveDiagrams`Private`canonicalChains[
  ConstructiveDiagrams`Private`residueCanonicalPropagators[expression,Range[n]]];
fsZero[expression_,n_Integer] := TimeConstrained[
  Cancel[Together[Expand[fsCanonical[expression,n]]]] === 0,10,False];
fsRules={"Me"->1,"EE"->1};
fsAllPass[report_] := AssociationQ[report] && report["Checks"] =!= {} &&
  AllTrue[report["Checks"],Lookup[#, "Status",None] === "PASS"&];
fsVerify[particles_List,expectedCount_Integer] := Module[
  {amp,channels,normalized,plan,projected,feynman,before,after},
  amp=fsLookup[particles];channels=FactorizationAmplitudeChannels[particles,fsRealModel];
  normalized=ConstructiveDiagrams`Private`completionNormalizeInput[amp,channels,fsRealModel];
  plan=fsPlan[normalized,fsRealModel,Automatic,Automatic,particles];
  projected=fsCanonical[fsProject[amp,plan],Length[particles]];
  check["the saved QED amplitude has the required separate exchange signs "<>ToString[particles,InputForm],
    fsZero[amp+fsSwap[amp,{1,4}],Length[particles]] &&
      fsZero[amp+fsSwap[amp,{2,3}],Length[particles]]];
  check["signed averaging preserves the known QED normalization "<>ToString[particles,InputForm],
    fsZero[projected-amp,Length[particles]]];
  feynman=PrepareAmplitudes[diagramAmplitudes[CreateDiagrams[particles,fsFeynmanModel]],
    fsFeynmanModel,"PolePreference"->"IndependentXi","MaxSteps"->0,
    "TimeConstraint"->60,"Parallelization"->False,"ProgressReporting"->False];
  before=CheckAmplitudes[amp,channels,fsRealModel,"FeynmanAmplitudes"->feynman,
    "FeynmanModel"->fsFeynmanModel,"ParameterRules"->fsRules,"Samples"->1,
    "HighEnergySamples"->1,"ReferenceSamples"->1,"FeynmanSamples"->1,
    "RandomSeed"->913337,"TimeConstraint"->90,"Output"->"Details","Parallelization"->False];
  after=CheckAmplitudes[projected,channels,fsRealModel,"FeynmanAmplitudes"->feynman,
    "FeynmanModel"->fsFeynmanModel,"ParameterRules"->fsRules,"Samples"->1,
    "HighEnergySamples"->1,"ReferenceSamples"->1,"FeynmanSamples"->1,
    "RandomSeed"->1913371,"TimeConstraint"->90,"Output"->"Details","Parallelization"->False];
  check["the original saved QED formula passes independent diagrams and residues "<>ToString[particles,InputForm],
    fsAllPass[before] && before["Summary"] === <|"PASS"->expectedCount|>];
  check["fermion projection preserves all physical and independent Feynman checks "<>ToString[particles,InputForm],
    fsAllPass[after] && after["Summary"] === before["Summary"] &&
      AllTrue[after["FeynmanCheck"]["Samples"],Length[#["SpinVariables"]]===4&]];
  <|"Amplitude"->amp,"Channels"->channels,"Projection"->projected,"Plan"->plan,"After"->after|>
];
fsFour=fsVerify[{"e","E","E","e"},5];
fsFive=fsVerify[{"e","E","E","e","A.+"},11];
fsFirstPole=First[DeleteDuplicates[Cases[fsFour["Amplitude"],_PropDen,Infinity]]];
fsOneChannel=Total[Select[List@@Expand[fsFour["Amplitude"]],!FreeQ[#,fsFirstPole]&]];
check["averaging one four-fermion channel supplies the crossed channel with exact half normalization",
  fsZero[2fsProject[fsOneChannel,fsFour["Plan"]]-fsFour["Amplitude"],4]];
fsPublic=CompleteFactorization[fsFour["Amplitude"],fsFour["Channels"],fsRealModel,
  "ExternalParticles"->{"e","E","E","e"},"Samples"->1,"ValidationSamples"->1,
  "ParameterRules"->fsRules,"ValidationParameterRules"->{fsRules},
  "TimeConstraint"->60,"MaxCorrectionTerms"->64,"Parallelization"->False,
  "ProgressReporting"->False,"ReturnDetails"->True];
check["public completion automatically applies both identical-fermion exchanges",
  fsPublic["Status"]==="COMPLETE" && fsPublic["FermionSymmetry"]["Groups"]==={{1,4},{2,3}} &&
    fsPublic["ExchangeSymmetry"]["Order"]===4 && fsZero[fsPublic["Amplitude"]-fsFour["Amplitude"],4] &&
    AllTrue[fsPublic["ValidationReport"]["Channels"],#["Status"]==="PASS"&]];
fsSparsePublic=CompleteFactorization[fsOneChannel,fsFour["Channels"],fsRealModel,
  "ExternalParticles"->{"e","E","E","e"},"Samples"->1,"ValidationSamples"->1,
  "ParameterRules"->fsRules,"ValidationParameterRules"->{fsRules},
  "TimeConstraint"->60,"MaxCorrectionTerms"->64,"Parallelization"->False,
  "ProgressReporting"->False,"ReturnDetails"->True];
check["automatic signed completion repairs an input containing only one physical channel",
  fsSparsePublic["Status"]==="COMPLETE" &&
    fsZero[fsSparsePublic["Amplitude"]-fsFour["Amplitude"],4] &&
    AllTrue[fsSparsePublic["ValidationReport"]["Channels"],#["Status"]==="PASS"&]];
fsPublicHE=CompleteHighEnergy[fsPublic,fsFour["Channels"],fsRealModel,
  "Samples"->1,"ValidationSamples"->1,"ParameterRules"->fsRules,
  "ValidationParameterRules"->{fsRules},"TimeConstraint"->60,"Parallelization"->False,
  "ProgressReporting"->False,"Output"->"Details"];
check["the public high-energy stage retains identical-fermion symmetry and the correct amplitude",
  fsPublicHE["Status"]==="COMPLETE" && fsPublicHE["ExchangeSymmetry"]["Order"]===4 &&
    fsPublicHE["FermionSymmetry"]["Groups"]==={{1,4},{2,3}} &&
    fsZero[fsPublicHE["Amplitude"]-fsFour["Amplitude"],4]];
fsPublicDisabled=CompleteFactorization[fsFour["Amplitude"],fsFour["Channels"],fsRealModel,
  "ExternalParticles"->{"e","E","E","e"},"FermionSymmetryGroups"->{},
  "Samples"->1,"ValidationSamples"->1,"ParameterRules"->fsRules,
  "ValidationParameterRules"->{fsRules},"TimeConstraint"->60,"Parallelization"->False,
  "ProgressReporting"->False,"ReturnDetails"->True];
check["explicit empty fermion groups preserve successful unprojected completion",
  fsPublicDisabled["Status"]==="COMPLETE" && fsPublicDisabled["FermionSymmetry"]["Groups"]==={} &&
    fsZero[fsPublicDisabled["Amplitude"]-fsFour["Amplitude"],4]];
If[checks =!= 50, AppendTo[failures,"Not every fermion symmetry assertion executed."]];
Print["CHECKS: ",checks,"; FAILURES: ",Length[failures]];
If[failures =!= {}, Print[failures]];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:fermion-symmetry-tests.wl"]];
Quit[If[failures === {},0,1]];
