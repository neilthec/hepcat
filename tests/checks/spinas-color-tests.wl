(* Coherent color export: exact decomposition and a compiled explicit-color oracle. *)

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


root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root, "source", "HEPCAT.wl"}]];
$HistoryLength = 0;
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, assertion_] := (checks++; If[!TrueQ[assertion], AppendTo[failures, name]; Print["FAIL: ", name]]);
directory = CreateDirectory[];
sm = ReadModel[FileNameJoin[{root, "models"}], 1];
qed = ReadModel[FileNameJoin[{root, "models"}], 3];
spinas = Replace[Environment["HEPCAT_SPINAS_DIRECTORY"], $Failed | "" -> FileNameJoin[{ParentDirectory[root], "spinas"}]];
mu = ModelParameter[sm, "Mu"]; gg = ModelParameter[sm, "GG"];
qm = ModelParameter[qed, "Me"]; qe = ModelParameter[qed, "EE"];
dummy = ConstructiveDiagrams`Private`colorInternalIndex[1, 3];
c34 = ColorGenerator[ColorIndex[2], dummy, ColorIndex[3]] ColorGenerator[dummy, ColorIndex[1], ColorIndex[4]];
c43 = c34 /. {ColorIndex[3] -> ColorIndex[4], ColorIndex[4] -> ColorIndex[3]};
t = PropDen[Mom[Multiparticle[1, 3]], mu]; u = PropDen[Mom[Multiparticle[1, 4]], mu];
s = PropDen[Mom[Multiparticle[1, 2]], 0];
photonProcesses = Join[{"e", "E"}, #] & /@ {{"A.+", "A.+"}, {"A.+", "A.-"}, {"A.-", "A.-"}};
(* The fixture's ordered color currents follow the paper's common numerator.
   This supplies test data only, never a formula used by the exporter. *)
entries = Table[
  numerator = (LookupAmplitude[Thread[{p, Range[4]}], qed, False, {i, j, k, l}] /. {qm -> mu, qe -> gg}) t u;
  {{"u", "U", StringReplace[p[[3]], "A" -> "G"], StringReplace[p[[4]], "A" -> "G"]},
    c34 numerator/s/t + c43 numerator/s/u}, {p, photonProcesses}];
options = Join[Association[Options[WriteSPINASProcess]], <|"ParameterRules" -> {"Mu" -> 1, "GG" -> 1/3}|>];
ir = ConstructiveDiagrams`Private`spinasColorFamilyIR[entries, sm, options, Automatic];
check["all color coefficients share one certified basis", AssociationQ[ir] && TrueQ[ir["ColorData"]["Certified"]] && Length[ir["ColorData"]["Basis"]] === 2];
check["the color Gram matrix retains interference", ir["ColorData"]["GramMatrix"] === {{16/3, -2/3}, {-2/3, 16/3}}];
check["incoming colors are averaged once and final identical gluons receive their factorial",
  ir["ColorData"]["InitialColorAverage"] === 9 && ir["InitialSpinAverage"] === 4 && ir["SymmetryFactor"] === 1/2];
check["helicity closure permutes the full color amplitude", Length[ir["HelicityConfigurations"]] === 4 && ir["GeneratedBranchCount"] === 1];
bad = ConstructiveDiagrams`Private`colorAmplitudeTerms[1/ColorDelta[ColorIndex[1], ColorIndex[2]]];
check["a color-bearing denominator is rejected", FailureQ[bad]];
traceTensor = ColorDelta[ColorIndex[2], ColorIndex[1]] ColorTrace[ColorIndex[3], ColorIndex[4]];
traceEntries = ({First[#], traceTensor (Last[#] /. _ColorGenerator -> 1)} &) /@ entries;
traceIR = ConstructiveDiagrams`Private`spinasColorFamilyIR[traceEntries, sm, options, Automatic];
check["ordered traces export through the same exact coherent-color basis",
  AssociationQ[traceIR] && TrueQ[traceIR["ColorData"]["Certified"]] &&
    traceIR["ColorData"]["GramMatrix"] === {{6}}];
report = WriteSPINASProcess[FileNameJoin[{directory, "coherent_color"}], entries, sm,
  "ProcessName" -> "coherent_color", "SPINASDirectory" -> spinas,
  "ParameterRules" -> {"Mu" -> 1, "GG" -> 1/3}, "Build" -> True, "RunExamples" -> True, "ReturnDetails" -> True];
check["the complete coherent-color project compiles and runs", AssociationQ[report] && report["Build"]["ExitCode"] === 0 && report["Examples"]["ExitCode"] === 0];
If[!AssociationQ[report], Print["EXPORT FAILURE: ", InputForm[report]]; Quit[1]];
manifest = Import[report["Files"]["Manifest"], "RawJSON"];
check["the manifest records certified color normalization", TrueQ[manifest["FullColor"]] && manifest["InitialColorAverage"] === 9 && manifest["ColorBasisCount"] === 2];
check["unsupported colored event generation is explicit", report["MadEventNumerical"]["Status"] === "UNAVAILABLE" && StringContainsQ[report["MadEventNumerical"]["Reason"], "color-flow"]];
single = WriteSPINASProcess[FileNameJoin[{directory, "fixed_color"}], entries[[1, 1]], entries[[1, 2]], sm,
  "ProcessName" -> "fixed_color", "SPINASDirectory" -> spinas, "ReturnDetails" -> True];
check["the single-formula overload remains fixed-helicity and keeps its symmetry convention",
  AssociationQ[single] && Length[single["HelicityConfigurations"]] === 1 && single["SymmetryFactor"] === 1];
oracle = StringRiffle[{
  "#include \"process.hpp\"", "#include <cmath>", "#include <iostream>",
  "int main() { using P=hepcat_generated::coherent_color; P p;",
  "  const auto point=hepcat_spinas::make_example_momenta(p.masses(),P::incoming_legs(),10,1); p.set_momenta(point);",
  "  ldouble total=0; unsigned checked=0;",
  "  for (auto spins:P::helicity_configurations()) for(int a=-1;a<=1;a+=2) for(int b=-1;b<=1;b+=2) {",
  "    spins[0]=a;spins[1]=b;ldouble explicit_sum=0;",
  "    for(int c1=1;c1<=3;++c1) for(int c2=1;c2<=3;++c2) for(int c3=1;c3<=8;++c3) for(int c4=1;c4<=8;++c4)",
  "      explicit_sum+=std::norm(p.amp(spins,P::Colors{{c1,c2,c3,c4}}));",
  "    const ldouble coherent=p.color_spin_sum(spins);",
  "    if(std::abs(explicit_sum-coherent)>1e-11L*std::max(ldouble(1),explicit_sum)) return 1;",
  "    total+=explicit_sum;++checked;",
  "  }",
  "  if(std::abs(p.amp2()-total/72)>1e-11L*std::max(ldouble(1),total)) return 2;",
  "  P q; auto exchanged=point; std::swap(exchanged[2],exchanged[3]); q.set_momenta(exchanged);",
  "  const P::Spins minus_plus{{-1,1,-2,2}}, plus_minus{{-1,1,2,-2}};",
  "  for(int c1=1;c1<=3;++c1) for(int c2=1;c2<=3;++c2) for(int c3=1;c3<=8;++c3) for(int c4=1;c4<=8;++c4) {",
  "    const auto a=p.amp(minus_plus,P::Colors{{c1,c2,c3,c4}});",
  "    const auto b=q.amp(plus_minus,P::Colors{{c1,c2,c4,c3}});",
  "    if(std::abs(a-b)>1e-11L*std::max(ldouble(1),std::abs(a))) return 5;",
  "  }",
  "  try { p.amp(P::default_spins()); return 3; } catch(const std::invalid_argument&) {}",
  "  try { p.amp(P::default_spins(),P::Colors{{0,1,1,1}}); return 4; } catch(const std::invalid_argument&) {}",
  "  std::cout<<\"COHERENT_COLOR_COMPARISONS=\"<<checked<<'\\n'; return 0; }"}, "\n"];
oracleFile = FileNameJoin[{report["Directory"], "color_oracle.cpp"}]; Export[oracleFile, oracle, "Text"];
executable = FileNameJoin[{report["Directory"], "build", "color_oracle"}];
library = SelectFirst[FileNameJoin[{spinas, #, "libspinas.a"}] & /@ {"build", "lib"}, FileExistsQ];
compile = RunProcess[Join[{"c++", "-std=c++11", "-O1"}, If[manifest["Precision"] === "LongDouble", {"-DWITH_LONG_DOUBLE"}, {}],
  {"-I" <> report["Directory"], "-I" <> FileNameJoin[{spinas, "include"}], oracleFile,
    FileNameJoin[{report["Directory"], "build", "libcoherent_color_process.a"}], library, "-o", executable}]];
check["the independent explicit-color oracle compiles", compile["ExitCode"] === 0];
If[compile["ExitCode"] === 0, ran = RunProcess[{executable}];
  check["explicit colors agree with Gram contraction and helicity closure permutes the complete color tensor",
    ran["ExitCode"] === 0 && StringContainsQ[ran["StandardOutput"], "COHERENT_COLOR_COMPARISONS=16"]], Print[compile]];
Print["SPINAS COLOR CHECKS: ", checks, "; FAILURES: ", Length[failures]];
If[failures === {}, DeleteDirectory[directory, DeleteContents -> True]; Print["HEPCAT_TEST_COMPLETED:spinas-color-tests.wl"],
  Print[failures]; Print["COLOR TEST FILES: ", directory]];
Quit[If[failures === {}, 0, 1]];
