(* Public combined MadGraph validation and MadEvent export surface.
   These checks generate source only. The genuine integration regression is
   separate, so writing source cannot masquerade as a numerical PASS. *)
smeRoot = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{smeRoot, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0;
smeChecks = 0; smeFailures = {};
SetAttributes[smeCheck, HoldRest];
smeCheck[name_, value_] := (smeChecks++; If[!TrueQ[value],
  AppendTo[smeFailures, name]; Print["FAIL: ", name]]);
smeTemporary = CreateDirectory[];
smePath[name_] := FileNameJoin[{smeTemporary, name}];
smeModel = ReadModel[FileNameJoin[{smeRoot, "models"}], 3];
smeModelBefore = smeModel;
smeGlobalsBefore = {OwnValues[Global`Me], OwnValues[Global`EE]};
smeSpinas = Replace[Environment["HEPCAT_SPINAS_DIRECTORY"],
  $Failed | "" -> FileNameJoin[{ParentDirectory[smeRoot], "spinas"}]];
smeProcesses = Join[{"e", "E"}, #] & /@
  {{"A.+", "A.+"}, {"A.+", "A.-"}, {"A.-", "A.+"}, {"A.-", "A.-"}};
smeFamily = {#, LookupAmplitude[Thread[{#, Range[4]}], smeModel,
    False, {smeI, smeJ, smeK, smeL}]} & /@ smeProcesses;
(* No interpreter is executed by a source-only export. Isolate its availability
   probe so this unit suite does not require an installed Python or MadGraph. *)
smeExport[name_String, physical_, opts___] := Block[{
  ConstructiveDiagrams`Private`spinasMadGraphPython = Function[{directory},
    If[directory === "", "", "/source-only/python3"]]},
  WriteSPINASProcess[smePath[name], physical, smeFamily, smeModel, opts,
    "ProcessName" -> "madevent_stub", "SPINASDirectory" -> smeSpinas, "ReturnDetails" -> True]];
smeManifest[report_Association] := Import[report["Files"]["Manifest"], "RawJSON"];

smeDefault = smeExport["unconfigured", {"e", "E"} -> {"A", "A"}];
smeCheck["ordinary SPINAS export remains usable without a MadGraph installation",
  AssociationQ[smeDefault]];
If[!AssociationQ[smeDefault], Print[smeDefault]; Exit[1]];
smeDefaultManifest = smeManifest[smeDefault];
smeCheck["unconfigured numerical integration has an honest availability report",
  smeDefaultManifest["MadEventNumerical"]["Status"] === "UNAVAILABLE" &&
    StringLength[smeDefaultManifest["MadEventNumerical"]["Reason"]] > 0];
smeCheck["unconfigured exports require no numerical MadEvent launcher",
  !FileExistsQ[FileNameJoin[{smeDefault["Directory"], "madevent-runtime.py"}]] &&
    !StringContainsQ[Import[smeDefault["Files"]["CMake"], "Text"], "add_executable(madevent_stub_madevent"]];

(* A minimal installation marker suffices for a source-only export. No UFO
   import, topology generation, compilation or launch is allowed here. *)
smeFakeMG = CreateDirectory[smePath["chosen-MadGraph"]];
CreateDirectory[FileNameJoin[{smeFakeMG, "bin"}]];
Export[FileNameJoin[{smeFakeMG, "bin", "mg5_aMC"}], "#!/usr/bin/env python3\n", "Text"];
smeSelected = smeExport["combined", {"e", "E"} -> {"A", "A"},
  "MadGraphDirectory" -> smeFakeMG, "MadGraphModel" -> "explicit-test-ufo",
  "MadGraphAllowedParticles" -> {11, 22},
  "MadGraphParameterMap" -> {"EE" -> "charge", "Me" -> "electron_mass"}];
smeCheck["the same public call emits both workflows", AssociationQ[smeSelected]];
If[AssociationQ[smeSelected],
  smeSelectedManifest = smeManifest[smeSelected];
  smeNumerical = smeSelectedManifest["MadEventNumerical"];
  If[smeNumerical["Status"] =!= "AVAILABLE", Print["NUMERICAL CONFIGURATION: ", InputForm[smeNumerical]]];
  smeReference = smeSelectedManifest["MadGraphValidation"];
  smeCheck["both workflows preserve the explicitly chosen model and particle sector",
    smeNumerical["Status"] === "AVAILABLE" &&
      And @@ (smeNumerical[#] === smeReference[#] & /@
        {"MadGraphDirectory", "MadGraphModel", "AllowedParticlePDGs", "ParameterMap"})];
  smeCheck["both workflows ship their independent runtime files together",
    AllTrue[FileNameJoin[{smeSelected["Directory"], #}] & /@
      {"madgraph-validation.cpp", "madgraph-runtime.py", "madevent-spinas.cpp",
       "madevent-main.cpp", "madevent-runtime.py", "madevent-config.json"}, FileExistsQ]];
  smeCheck["topology generation remains lazy during source-only export",
    !DirectoryQ[FileNameJoin[{smeSelected["Directory"], "madevent"}]] &&
      FileNames["*", smeFakeMG, Infinity] === {FileNameJoin[{smeFakeMG, "bin"}],
        FileNameJoin[{smeFakeMG, "bin", "mg5_aMC"}]}];
  smeCheck["the numerical adapter keeps the raw matrix element separate from averaging",
    With[{source = Import[FileNameJoin[{smeSelected["Directory"], "madevent-spinas.cpp"}], "Text"]},
      StringContainsQ[source, "std::norm(process->amp(spins))"] &&
        !StringContainsQ[source, "process->amp2("]]];
  smeCheck["normalization metadata retains one initial average and one photon factorial",
    smeNumerical["InitialSpinAverage"] == 4 && smeNumerical["SymmetryFactor"] == 1/2];
  smeCheck["the optional static matrix bridge and launcher share the ordinary process library",
    With[{cmake = Import[smeSelected["Files"]["CMake"], "Text"]},
      StringContainsQ[cmake, "madevent_stub_madevent_matrix STATIC"] &&
        StringContainsQ[cmake, "add_executable(madevent_stub_madevent"] &&
        StringContainsQ[cmake, "madevent-link.json"]]];
];

smeFixed = smeExport["fixed-outgoing", {"e", "E"} -> {"A.+", "A"},
  "MadGraphDirectory" -> smeFakeMG, "MadGraphModel" -> "explicit-test-ufo"];
smeCheck["a fixed outgoing photon remains a selected contribution with the full identical factor",
  AssociationQ[smeFixed] && With[{c = smeManifest[smeFixed]["MadEventNumerical"]},
    c["InitialSpinAverage"] == 4 && c["SymmetryFactor"] == 1/2 &&
      Length[c["HelicityConfigurations"]] === 2 &&
      AllTrue[c["HelicityConfigurations"], #[[3]] === 2 &]]];

smeCrossed = Block[{
  ConstructiveDiagrams`Private`spinasMadGraphPython = Function[{directory},
    If[directory === "", "", "/source-only/python3"]]},
  WriteSPINASProcess[smePath["fixed-incoming"], smeFamily, smeModel,
    "IncomingLegs" -> {3, 1}, "FixedHelicities" -> {3 -> 2},
    "ProcessName" -> "madevent_stub", "SPINASDirectory" -> smeSpinas,
    "MadGraphDirectory" -> smeFakeMG, "MadGraphModel" -> "explicit-test-ufo",
    "ReturnDetails" -> True]];
smeCheck["a crossed fixed incoming photon preserves the explicit leg map and no helicity average",
  AssociationQ[smeCrossed] && With[{c = smeManifest[smeCrossed]["MadEventNumerical"]},
    c["LegOrder"] === {3, 1, 2, 4} && c["PhysicalPDGs"] === {22, 11, 11, 22} &&
      c["InitialSpinAverage"] == 2 && c["SymmetryFactor"] == 1]];
smeCheck["export does not mutate model parameters or global symbols",
  smeModel === smeModelBefore && {OwnValues[Global`Me], OwnValues[Global`EE]} === smeGlobalsBefore];
DeleteDirectory[smeTemporary, DeleteContents -> True];
Print["MadEvent public export: ", smeChecks - Length[smeFailures], " / ", smeChecks, " checks passed."];
If[smeFailures =!= {}, Print["Failures: ", smeFailures]; Exit[1]];
Print["HEPCAT_TEST_COMPLETED:spinas-madevent-tests.wl"];
Exit[0];
