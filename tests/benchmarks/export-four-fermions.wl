(* Export the saved complete four-fermion QED amplitudes for numerical timing.
   No construction, simplification, coefficient fitting or model mutation.
   Both photon helicities are retained, including polarized amp(...) calls.
   Run: WolframKernel -script tests/benchmarks/export-four-fermions.wl --output DIR
   Optional: --source DIR --models DIR --configuration MANIFEST --spinas DIR. *)

(*
HEPCAT - High Energy Physics Constructive Amplitude Tool

Copyright (C) 2026 Neil Christensen

This file is part of HEPCAT.

HEPCAT is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

HEPCAT is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with HEPCAT.  If not, see <https://www.gnu.org/licenses/>.
*)


$CharacterEncoding = "UTF-8";
ffRepository = DirectoryName[DirectoryName[DirectoryName[ExpandFileName[$InputFileName]]]];
ffArguments = If[$ScriptCommandLine =!= {}, Rest[$ScriptCommandLine],
  With[{ffScriptPosition = FirstPosition[$CommandLine, "-script", Missing["NotScript"]]},
    If[ListQ[ffScriptPosition], Drop[$CommandLine, First[ffScriptPosition] + 1], {}]]];
If[OddQ[Length[ffArguments]], Print["Expected option/value pairs."]; Quit[1]];
ffOptions = Association[Rule @@@ Partition[ffArguments, 2]];
If[Complement[Keys[ffOptions], {"--output","--source","--models","--configuration","--spinas"}] =!= {},
  Print["Unknown export option."]; Quit[1]];
ffSource = ExpandFileName[Lookup[ffOptions,"--source",FileNameJoin[{ffRepository,"source"}]]];
ffModels = ExpandFileName[Lookup[ffOptions,"--models",FileNameJoin[{ffRepository,"models"}]]];
ffOutput = ExpandFileName[Lookup[ffOptions,"--output",FileNameJoin[{ffRepository,"tests", "benchmarks","build-four-fermion-double","exports"}]]];
ffSPINAS = ExpandFileName[Lookup[ffOptions,"--spinas",FileNameJoin[{DirectoryName[ffRepository],"spinas"}]]];
ffConfigurationFile = ExpandFileName[Lookup[ffOptions,"--configuration",
  FileNameJoin[{ffRepository,"tests", "results", "spinas-output","eeee","manifest.json"}]]];
ffConfiguration = Import[ffConfigurationFile,"RawJSON"];
If[!AssociationQ[ffConfiguration], Print["Cannot read the reference configuration."]; Quit[1]];
ffCH = Lookup[ffConfiguration,"CalcHEPValidation",<||>];
ffMG = Lookup[ffConfiguration,"MadGraphValidation",<||>];
If[!AllTrue[{ffCH,ffMG},Lookup[#,"Status",""]==="AVAILABLE" &&
    Sort[Lookup[#,"AllowedParticlePDGs",{}]]==={11,22}&],
  Print["Explicit electron/photon-only CalcHEP and MadGraph models are required."]; Quit[1]];
Get[FileNameJoin[{ffSource,"HEPCAT.wl"}]];
ffLookup[particles_,model_] := LookupAmplitude[MapIndexed[{#1,First[#2]}&,particles],
  model,False,{ffSpin1,ffSpin2,ffSpin3,ffSpin4}];
Do[
 ffModelNumber = If[ffPreference==="AllPhysical",4,5];
 ffModel = ReadModel[ffModels,ffModelNumber];
 If[!ListQ[ffModel],Print["Cannot read model ",ffModelNumber];Quit[1]];
 ffModelHashes = Association@Table[name->FileHash[FileNameJoin[{ffModels,name}],"SHA256","HexString"],
   {name,(#<>ToString[ffModelNumber]<>".mdl"&/@{"amps","vars","func","prtcls"})}];
 Do[
  ffRecords={};
  ffEntries=Table[
    ffParticles=Join[{"e","E","E","e"},ConstantArray["A.+",ffPhotons-ffNegative],ConstantArray["A.-",ffNegative]];
    ffExpression=ffLookup[ffParticles,ffModel];
    If[ffExpression===0 || !FreeQ[ffExpression,_Failure|$Failed|Spinor["Xi",___]|_xFactor|_xTildeFactor],
      Print["Missing or unsupported saved amplitude: ",ffParticles];Quit[1]];
    ffInverseBases=DeleteDuplicates[Cases[ffExpression,Power[base_,_?Negative]:>base,Infinity]];
    If[ffPreference==="AllPhysical" && !AllTrue[ffInverseBases,NumberQ[#]||MatchQ[#,_PropDen|_ModelParameter]&],
      Print["Unexpected nonphysical inverse factor in saved AllPhysical formula: ",ffParticles];Quit[1]];
    AppendTo[ffRecords,<|"Particles"->ffParticles,"ExpressionSHA256"->Hash[ffExpression,"SHA256","HexString"],
      "ExpressionLeaves"->LeafCount[ffExpression],"Complexity"->SpinorExpressionComplexity[ffExpression]|>];
    {ffParticles,ffExpression},{ffNegative,0,ffPhotons}];
  ffClass="ffff"<>If[ffPreference==="AllPhysical","Physical","Spurious"]<>ToString[ffPhotons];
  ffDirectory=FileNameJoin[{ffOutput,If[ffPreference==="AllPhysical","physical","spurious"],ffClass}];
  ffExport=WriteSPINASProcess[ffDirectory,{"e","E"}->Join[{"e","E"},ConstantArray["A",ffPhotons]],ffEntries,ffModel,
    "ProcessName"->ffClass,"Precision"->"Double","CodeStyle"->"Optimized","HelicitySymmetry"->"Conjugation",
    "SPINASDirectory"->ffSPINAS,"CalcHEPDirectory"->ffCH["CalcHEPDirectory"],
    "CalcHEPModel"->{ffCH["ModelDirectory"],ffCH["ModelNumber"]},"CalcHEPAllowedParticles"->{11,22},
    "MadGraphDirectory"->ffMG["MadGraphDirectory"],"MadGraphModel"->ffMG["MadGraphModel"],
    "MadGraphAllowedParticles"->{11,22},
    "MadGraphParameterRules"->Normal[Lookup[ffMG,"ParameterRules",<||>]],
    "MadGraphParameterMap"->Normal[Lookup[ffMG,"ParameterMap",<||>]],"Overwrite"->True];
  If[FailureQ[ffExport]||ffExport===$Failed,Print[InputForm[ffExport]];Quit[1]];
  ffManifest=Import[FileNameJoin[{ffDirectory,"manifest.json"}],"RawJSON"];
  If[!AssociationQ[ffManifest] || !AllTrue[{"CalcHEPValidation","MadGraphValidation"},
      Lookup[Lookup[ffManifest,#,<||>],"Status",""]==="AVAILABLE"&],
    Print["Independent reference configuration is unavailable: ",
      InputForm[KeyTake[ffManifest,{"CalcHEPValidation","MadGraphValidation"}]]];Quit[1]];
  Export[FileNameJoin[{ffDirectory,"benchmark-amplitudes.json"}],<|
    "PolePreference"->ffPreference,"Photons"->ffPhotons,"ModelNumber"->ffModelNumber,
    "AmplitudeSource"->"Unchanged saved complete amplitudes from ReadModel and LookupAmplitude",
    "ModelFilesSHA256"->ffModelHashes,"InputHelicitySectors"->ffRecords,
    "CompletePhotonHelicityCount"->2^ffPhotons,"Precision"->"Double","CodeStyle"->"Optimized",
    "HelicitySymmetry"->"Conjugation","HelicitySymmetryScope"->"Declared vectorlike tree-level QED with real parameters; complete massive-spin sum",
    "ShellScope"->"External on-shell conditions only; no internal on-shell substitutions in export",
    "SourceDirectory"->ffSource,"ModelDirectory"->ffModels,
    "ExporterSHA256"->FileHash[FileNameJoin[{ffSource,"internal","HEPCAT-SPINAS.wl"}],"SHA256","HexString"],
    "OptimizerSHA256"->FileHash[FileNameJoin[{ffSource,"internal","HEPCAT-SPINAS-Optimize.wl"}],"SHA256","HexString"]|>,"RawJSON"];
  Print["Exported ",ffPreference," N=",ffPhotons,": ",ffDirectory],{ffPhotons,0,2}],
 {ffPreference,{"AllPhysical","AllSpurious"}}];
Quit[0];
