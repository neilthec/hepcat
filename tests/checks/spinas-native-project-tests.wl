(* Native-style persistent caches must survive state changes and object copies. *)

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
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0;
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);
npDirectory = CreateDirectory[];
npSpinas = Replace[Environment["HEPCAT_SPINAS_DIRECTORY"],
  $Failed | "" -> FileNameJoin[{ParentDirectory[root], "spinas"}]];
npModel = ReadModel[FileNameJoin[{root, "models"}], 4];
npMass = ModelParameter[npModel, "Me"]; npCharge = ModelParameter[npModel, "EE"];
npRules = {"Me" -> 1, "EE" -> 1/3};
npBefore = npModel;
npLookup[process_List] := LookupAmplitude[Thread[{process, Range[Length[process]]}],
  npModel, False, {npI, npJ, npK, npL}];
npFamily = {#, npLookup[#]} & /@ (Join[{"e", "E"}, #] & /@
  {{"A.+", "A.+"}, {"A.+", "A.-"}, {"A.-", "A.-"}});
npPath[name_] := FileNameJoin[{npDirectory, name}];
npExport[name_String, input_, model_:Automatic] := WriteSPINASProcess[npPath[name], input,
  If[model === Automatic, npModel, model], "ProcessName" -> name,
  "ParameterRules" -> npRules, "SPINASDirectory" -> npSpinas,
  "CalcHEPDirectory" -> None, "CalcHEPModel" -> None, "ReturnDetails" -> True];
npFamilyReport = npExport["native_lifecycle", npFamily];
npDotFamily = ({First[#], Last[#] MomProd[3, 4]} &) /@ npFamily;
npDotReport = npExport["native_dot_family", npDotFamily];
npCrossDotFamily = ({First[#], Last[#] MomProd[1, 3]} &) /@ npFamily;
npCrossDotReport = npExport["native_crossed_dot_family", npCrossDotFamily];
npSquareFamily = ({First[#], Last[#] Mom[1]^2} &) /@ npFamily;
npSquareReport = npExport["native_external_square", npSquareFamily];
npSquareIR = ConstructiveDiagrams`Private`spinasProcessIR[First[npFamily][[1]],
  First[npFamily][[2]] Mom[1]^2, npModel,
  <|"IncomingLegs"->{1,2},"ParameterRules"->npRules|>];
npClassMassReport = npExport["Me", npFamily];
npClassProductReport = npExport["a12a", npFamily];
check["a complete family exports the native process API", AssociationQ[npFamilyReport]];
If[!AssociationQ[npFamilyReport], Print[npFamilyReport]; Quit[1]];
npHeader = Import[npFamilyReport["Files"]["Header"], "Text"];
npSource = Import[npFamilyReport["Files"]["Source"], "Text"];
check["spinor products persist as native sproduct members",
  StringContainsQ[npHeader, "sproduct"] && StringContainsQ[npSource, ".update()"] &&
    StringContainsQ[npSource, ".v("]];
check["particle, momentum and mass entry points are directly readable",
  StringContainsQ[npHeader <> npSource, "set_masses("] &&
    StringContainsQ[npSource, "p1.set_momentum("] &&
    StringContainsQ[npSource, "p2.set_momentum("]];
check["spin sums and helicity choices are explicit generated loops and branches",
  StringContainsQ[npSource, "for ("] && StringContainsQ[npSource, "if ("] &&
    !StringContainsQ[npSource, "std::function"] &&
    !StringContainsQ[npSource, RegularExpression["::component_[0-9]+\\("]]];
check["cache ownership is explicit for copying process objects",
  StringContainsQ[npHeader <> npSource, "const native_lifecycle&"] &&
    StringContainsQ[npHeader <> npSource, "operator="]];
check["permuted helicity branches retain a two-momentum invariant",
  AssociationQ[npDotReport] && DirectoryQ[npDotReport["Directory"]]];
check["crossed external dot products retain both native particle arguments under leg maps",
  AssociationQ[npCrossDotReport] &&
    StringContainsQ[Import[npCrossDotReport["Files"]["Source"],"Text"],"p1.dot(p4)"]];
check["literal external squares are normalized to a live model mass before export",
  AssociationQ[npSquareReport] && AssociationQ[npSquareIR] &&
    FreeQ[npSquareIR["IndexedExpression"], HoldPattern[Mom[_Integer]^2]]];
check["class names may coincide with familiar mass and spinor-product labels",
  AssociationQ[npClassMassReport] && AssociationQ[npClassProductReport]];
npCHReport = WriteSPINASProcess[npPath["native_calchep_source"], npFamily, npModel,
  "ProcessName" -> "native_calchep_source", "ParameterRules" -> npRules,
  "SPINASDirectory" -> npSpinas, "CalcHEPDirectory" -> None,
  "CalcHEPModel" -> {FileNameJoin[{root, "tests", "reference-models", "calchep"}], 2}, "ReturnDetails" -> True];
check["the independent validator also exports readable source without requiring CalcHEP",
  AssociationQ[npCHReport]];
If[AssociationQ[npCHReport],
  npCHSource = Import[npCHReport["Files"]["CalcHEPSource"], "Text"];
  check["CalcHEP parameter values use recognizable model names",
    StringContainsQ[npCHSource, RegularExpression["\\bMe\\s*="]] &&
      StringContainsQ[npCHSource, RegularExpression["\\bEE\\s*="]]];
  check["identity leg ordering avoids a redundant per-point permutation array",
    !StringContainsQ[npCHSource, "std::array<std::size_t, leg_count> order"] &&
      !StringContainsQ[npCHSource, "point[order[leg]]"]]];

npGeneric = npModel;
npGeneric[[1]] = Append[npGeneric[[1]],
  {"native fixture scalar", "h", "h", 993, 0, 0, 0, 1, 1, "h", "h"}];
npDotModel = npGeneric;
npDotModel[[1]] = Append[npDotModel[[1]],
  {"massive external dot fixture", "s", "s", 995, 0, npMass, 0, 1, 1, "s", "s"}];
npStableDotReport = WriteSPINASProcess[npPath["native_collinear_dot"],
  ConstantArray["s",4], MomProd[2 Mom[1],-3 Mom[3]], npDotModel,
  "ProcessName"->"native_collinear_dot","ParameterRules"->npRules,
  "SPINASDirectory"->npSpinas,"CalcHEPDirectory"->None,"CalcHEPModel"->None,
  "ReturnDetails"->True];
npVirtualDotIR = ConstructiveDiagrams`Private`spinasProcessIR[ConstantArray["s",4],
  MomProd[Mom[Multiparticle[1,3]],Mom[2]],npDotModel,
  <|"IncomingLegs"->{1,2},"ParameterRules"->npRules|>];
check["scaled single-leg dots use declared particles while virtual arguments keep generic Lorentz algebra",
  AssociationQ[npStableDotReport] && AssociationQ[npVirtualDotIR] &&
    StringContainsQ[Import[npStableDotReport["Files"]["Source"],"Text"],"p1.dot(p3)"] &&
    StringContainsQ[StringJoin[npVirtualDotIR["ScalarStatements"]],"dot(sum_momenta("] &&
    !StringContainsQ[StringJoin[npVirtualDotIR["ScalarStatements"]],".dot("]];
npTensorParameter = ModelParameter["amplitude", "native-local-collision", "Real"];
npValidationParameter = ModelParameter["validation", "native-local-collision", "Real"];
npTensorModel = npGeneric;
npTensorModel[[1]] = Append[npTensorModel[[1]],
  {"native fixture vector", "V", "V", 994, 2, npMass, 0, 1, 1, "V", "V"}];
npTensorModel[[3]] = Join[npTensorModel[[3]], {{npTensorParameter, 2/7}, {npValidationParameter, 3/11}}];
npTensorExpression = npTensorParameter *
  SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] *
  SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] /
  PropDen[Mom[Multiparticle[1, 3]], npValidationParameter];
npTensorReport = WriteSPINASProcess[npPath["native_tensor_parameter"], {"V", "V", "h", "h"},
  npTensorExpression, npTensorModel, "ProcessName" -> "native_tensor_parameter",
  "ParameterRules" -> npRules, "SPINASDirectory" -> npSpinas,
  "CalcHEPDirectory" -> None, "CalcHEPModel" -> None, "ReturnDetails" -> True];
check["tensor coefficients may share a name with a generated local accumulator",
  AssociationQ[npTensorReport]];
npComposite = 2 npCharge^2 SpinorChain[
  Spinor["Spin", "Angle", 1], Mom[Multiparticle[1, 3]], Mom[2], Spinor["Spin", "Angle", 2]] /
  (PropDen[Mom[Multiparticle[1, 3]], npMass] PropDen[Mom[Multiparticle[1, 4]], npMass]);
npLongMomenta = {1, 2, 1, 2, 1, 2, 1, 3};
npLong = npCharge^2 (SpinorChain @@ Join[{Spinor["Spin", "Angle", 1]},
  Mom /@ npLongMomenta, {Spinor["Spin", "Angle", 2]}]);
npCompositeReport = WriteSPINASProcess[npPath["native_composite"], {"e", "E", "h", "h"},
  npComposite, npGeneric, "ProcessName" -> "native_composite", "ParameterRules" -> npRules,
  "SPINASDirectory" -> npSpinas, "CalcHEPDirectory" -> None, "CalcHEPModel" -> None,
  "ReturnDetails" -> True];
npLongReport = WriteSPINASProcess[npPath["native_long_chain"], {"e", "E", "h", "h"},
  npLong, npGeneric, "ProcessName" -> "native_long_chain", "ParameterRules" -> npRules,
  "SPINASDirectory" -> npSpinas, "CalcHEPDirectory" -> None, "CalcHEPModel" -> None,
  "ReturnDetails" -> True];
check["composite and longer-than-native momentum chains retain a supported path",
  AssociationQ[npCompositeReport] && AssociationQ[npLongReport]];
npVirtualSquareIR = ConstructiveDiagrams`Private`spinasProcessIR[
  {"e", "E", "h", "h"}, npComposite Mom[Multiparticle[1,3]]^2, npGeneric,
  <|"IncomingLegs"->{1,2},"ParameterRules"->npRules|>];
check["a genuine multiparticle square remains an off-shell numerical invariant",
  AssociationQ[npVirtualSquareIR] &&
    !FreeQ[npVirtualSquareIR["IndexedExpression"], HoldPattern[Mom[_Multiparticle]^2]] &&
    StringContainsQ[npVirtualSquareIR["ExpressionCpp"], "mass2("]];
check["native generation leaves the supplied model unchanged", npModel === npBefore];

npRun[command_List] := Quiet[Check[RunProcess[command], $Failed]];
npFailed[report_] := !AssociationQ[report] || Lookup[report, "ExitCode", -1] =!= 0;
npLibraries = Select[FileNameJoin[{npSpinas, #, "libspinas.a"}] & /@ {"build", "lib"}, FileExistsQ];
npCompiler = npRun[{"c++", "--version"}];
If[npLibraries =!= {} && !npFailed[npCompiler] &&
    AllTrue[{npDotReport, npCrossDotReport, npStableDotReport, npSquareReport,
      npClassMassReport, npClassProductReport, npTensorReport,
      npCompositeReport, npLongReport}, AssociationQ],
  npManifest = Import[npFamilyReport["Files"]["Manifest"], "RawJSON"];
  npMassIndex = First[FirstPosition[npManifest["ParameterOrder"], "Me"]] - 1;
  npChargeIndex = First[FirstPosition[npManifest["ParameterOrder"], "EE"]] - 1;
  npIncludeLines = ("#include \"" <> #["Files"]["Header"] <> "\"") & /@
    {npFamilyReport, npDotReport, npCrossDotReport, npStableDotReport,
      npSquareReport, npClassMassReport, npClassProductReport,
      npTensorReport, npCompositeReport, npLongReport};
  npCpp = StringRiffle[Join[npIncludeLines, {
    "#include \"SM/include/eeAA.h\"", "#include <algorithm>", "#include <iostream>",
    "#include <memory>", "#include <stdexcept>", "#include <vector>",
    "using Process = hepcat_generated::native_lifecycle;",
    "using DotProcess = hepcat_generated::native_dot_family;",
    "using CrossDotProcess = hepcat_generated::native_crossed_dot_family;",
    "using CollinearDot = hepcat_generated::native_collinear_dot;",
    "using SquareProcess = hepcat_generated::native_external_square;",
    "using ClassMass = hepcat_generated::Me;",
    "using ClassProduct = hepcat_generated::a12a;",
    "using Tensor = hepcat_generated::native_tensor_parameter;",
    "using Composite = hepcat_generated::native_composite;",
    "using LongChain = hepcat_generated::native_long_chain;",
    "static unsigned comparisons = 0;",
    "void same(cdouble actual, cdouble expected, const char* label) {",
    "  ++comparisons;",
    "  const auto error = std::abs(actual - expected);",
    "  const auto scale = std::max(std::abs(actual), std::abs(expected));",
    "  if (!hepcat_spinas::finite_complex(actual) || error > ldouble(2e-10L)*scale + ldouble(1e-24L)) {",
    "    std::cerr << label << \": actual=\" << actual << \" expected=\" << expected << '\\n';",
    "    throw std::runtime_error(label);", "  }", "}",
    "template<class P> typename P::Momenta point(P& process, unsigned index) {",
    "  const ldouble energy = hepcat_spinas::example_center_of_mass_energy(",
    "    process.masses(), process.incoming_legs())*(1+ldouble(index)/ldouble(20));",
    "  return hepcat_spinas::make_example_momenta(process.masses(), process.incoming_legs(),",
    "    energy, index);",
    "}",
    "template<class P> void check_qed(P& generated, const typename P::Momenta& momenta,",
    "    const char* label, ldouble factor=1, bool mapped_dot=false) {",
    "  const auto parameters = generated.get_parameters();",
    "  const ldouble charge = std::real(parameters[" <> ToString[npChargeIndex] <> "]);",
    "  const ldouble mass = std::real(parameters[" <> ToString[npMassIndex] <> "]);",
    "  spinas::eeAA reference(charge, mass);",
    "  reference.set_momenta(momenta[0].data(), momenta[1].data(), momenta[2].data(), momenta[3].data());",
    "  ldouble spin_sum = 0;",
    "  for (int ds1=-1; ds1<=1; ds1+=2) for (int ds2=-1; ds2<=1; ds2+=2)",
    "    for (int ds3=-2; ds3<=2; ds3+=4) for (int ds4=-2; ds4<=2; ds4+=4) {",
    "      const typename P::Spins spins = {{ds1,ds2,ds3,ds4}};",
    "      // The missing -+ branch is the photon-swap of the supplied +- fixture.",
    "      const ldouble branch_factor = mapped_dot ? -hepcat_spinas::dot(",
    "        momenta[0],momenta[ds3<0 && ds4>0 ? 3 : 2]) : factor;",
    "      const cdouble expected = branch_factor*ldouble(ds3==ds4 ? 1 : -1)*reference.amp(ds1,ds2,ds3,ds4);",
    "      same(generated.amp(spins), expected, label);",
    "      same(generated.amp(ds1,ds2,ds3,ds4), expected, \"scalar spin overload\");",
    "      spin_sum += std::norm(expected);", "    }",
    "  same(cdouble(generated.spin_sum()), cdouble(spin_sum), \"raw spin sum\");",
    "  same(cdouble(generated.amp2()), cdouble(spin_sum/ldouble(8)), \"average and symmetry factor\");", "}",
    "template<class P> cdouble direct_chain(P& process, const typename P::Momenta& momenta,",
    "    int ds1, int ds2, bool composite) {",
    "  spinas::particle first(process.masses()[0]), second(process.masses()[1]);",
    "  first.set_momentum(momenta[0].data()); second.set_momentum(momenta[1].data());",
    "  auto row = first.langle(ds1);",
    "  const ldouble charge = ldouble(1)/3;",
    "  if (composite) {",
    "    hepcat_spinas::FourMomentum q;",
    "    for (unsigned mu=0; mu<4; ++mu) q[mu]=momenta[0][mu]-momenta[2][mu];",
    "    const cdouble chain = row*spinas::cmatrix(q.data(),LOWER)*second.umat()*second.rangle(ds2);",
    "    hepcat_spinas::FourMomentum r;",
    "    for (unsigned mu=0; mu<4; ++mu) r[mu]=momenta[0][mu]-momenta[3][mu];",
    "    const auto mass = process.masses()[0];",
    "    return ldouble(2)*charge*charge*chain/(hepcat_spinas::mass2(q)-mass*mass)/",
    "      (hepcat_spinas::mass2(r)-mass*mass);", "  }",
    "  const unsigned legs[8]={0,1,0,1,0,1,0,2};",
    "  for (unsigned index=0; index<8; ++index) {",
    "    auto q = momenta[legs[index]];",
    "    if (legs[index] >= 2) for (auto& value : q) value = -value;",
    "    row = row*spinas::cmatrix(q.data(), index%2==1 ? UPPER : LOWER);", "  }",
    "  return charge*charge*(row*second.rangle(ds2));", "}",
    "int main() { try {",
    "  Process original;",
    "  for (unsigned index=0; index<5; ++index) {",
    "    const auto momenta = point(original,index);",
    "    original.set_momenta(momenta); check_qed(original,momenta,\"cache update\");",
    "    const auto other = point(original,index+21);",
    "    original.set_momenta(other[0].data(),other[1].data(),other[2].data(),other[3].data());",
    "    check_qed(original,other,\"native momentum overload\");", "  }",
    "  bool invalid_spin=false; try { original.amp(0,1,2,2); }",
    "    catch (const std::invalid_argument&) { invalid_spin=true; }",
    "  if (!invalid_spin) throw std::runtime_error(\"invalid doubled spin was silently accepted\");",
    "  DotProcess dot_process;",
    "  for (unsigned index=0; index<5; ++index) {",
    "    const auto momenta=point(dot_process,index); dot_process.set_momenta(momenta);",
    "    check_qed(dot_process,momenta,\"two-momentum invariant through swapped helicities\",",
    "      hepcat_spinas::dot(momenta[2],momenta[3]));", "  }",
    "  CrossDotProcess cross_dot;",
    "  for (unsigned index=0; index<5; ++index) {",
    "    const auto momenta=point(cross_dot,index); cross_dot.set_momenta(momenta);",
    "    check_qed(cross_dot,momenta,\"crossed external dot through photon leg maps\",1,true);", "  }",
    "  CollinearDot collinear_dot;",
    "  const auto check_collinear=[&](ldouble energy) {",
    "    const ldouble mass=collinear_dot.masses()[0];",
    "    const ldouble magnitude=std::sqrt((energy-mass)*(energy+mass));",
    "    CollinearDot::Momenta collinear;",
    "    collinear[0]={{energy,0,0,magnitude}}; collinear[2]=collinear[0];",
    "    collinear[1]={{energy,0,0,-magnitude}}; collinear[3]=collinear[1];",
    "    collinear_dot.set_momenta(collinear);",
    "    same(collinear_dot.amp({{0,0,0,0}}),cdouble(6*mass*mass),",
    "      \"stable collinear external product with crossing coefficients\");", "  };",
    "  collinear_dot.set_masses(ldouble(0.0005L)); check_collinear(50);",
    "  collinear_dot.set_masses(ldouble(0.0007L)); check_collinear(200);",
    "  auto dot_parameters=collinear_dot.get_parameters();",
    "  dot_parameters[0]=cdouble(ldouble(0.0009L),0);",
    "  collinear_dot.set_parameters(dot_parameters); check_collinear(200);",
    "  SquareProcess square_process;",
    "  for (const ldouble mass : {ldouble(0.0005L),ldouble(0.0007L)}) {",
    "    square_process.set_masses(mass);",
    "    const auto momenta=hepcat_spinas::make_example_momenta(square_process.masses(),",
    "      square_process.incoming_legs(),ldouble(100),3);",
    "    square_process.set_momenta(momenta);",
    "    check_qed(square_process,momenta,\"stable external square after set_masses\",mass*mass);",
    "  }",
    "  auto square_parameters=square_process.get_parameters();",
    "  square_parameters[" <> ToString[npMassIndex] <> "]=cdouble(ldouble(0.0008L),0);",
    "  square_process.set_parameters(square_parameters);",
    "  const auto square_point=hepcat_spinas::make_example_momenta(square_process.masses(),",
    "    square_process.incoming_legs(),ldouble(400),5);",
    "  square_process.set_momenta(square_point);",
    "  check_qed(square_process,square_point,\"stable external square after set_parameters\",",
    "    square_process.masses()[0]*square_process.masses()[0]);",
    "  ClassMass class_mass; ClassProduct class_product;",
    "  const auto mass_point=point(class_mass,18); class_mass.set_momenta(mass_point);",
    "  check_qed(class_mass,mass_point,\"class and mass name collision\");",
    "  const auto product_point=point(class_product,19); class_product.set_momenta(product_point);",
    "  check_qed(class_product,product_point,\"class and product name collision\");",
    "  Process convenience(ldouble(2)/5, ldouble(3)/2);",
    "  same(convenience.get_parameters()[" <> ToString[npChargeIndex] <> "], cdouble(ldouble(2)/5),",
    "    \"native constructor coupling position\");",
    "  same(convenience.get_parameters()[" <> ToString[npMassIndex] <> "], cdouble(ldouble(3)/2),",
    "    \"native constructor mass position\");",
    "  auto changed = point(convenience,6); convenience.set_momenta(changed);",
    "  check_qed(convenience,changed,\"coupling and mass constructor\");",
    "  convenience.set_masses(ldouble(7)/4);",
    "  same(convenience.get_parameters()[" <> ToString[npMassIndex] <> "], cdouble(ldouble(7)/4),",
    "    \"mass setter changes model input\");",
    "  same(cdouble(convenience.masses()[0]), cdouble(ldouble(7)/4), \"mass setter changes external masses\");",
    "  bool invalidated = false; try { convenience.amp(convenience.default_spins()); }",
    "    catch (const std::logic_error&) { invalidated = true; }",
    "  if (!invalidated) throw std::runtime_error(\"set_masses must invalidate old kinematics\");",
    "  changed = point(convenience,7); convenience.set_momenta(changed);",
    "  check_qed(convenience,changed,\"mass setter updates caches\");",
    "  auto parameters = convenience.get_parameters();",
    "  parameters[" <> ToString[npChargeIndex] <> "] = cdouble(ldouble(3)/7,0);",
    "  convenience.set_parameters(parameters);",
    "  same(convenience.get_parameters()[" <> ToString[npChargeIndex] <> "], cdouble(ldouble(3)/7),",
    "    \"parameter setter changes model input\");",
    "  changed=point(convenience,8);",
    "  convenience.set_momenta(changed); check_qed(convenience,changed,\"parameter setter\");",
    "  Process copied(convenience); Process assigned; assigned=convenience;",
    "  check_qed(copied,changed,\"copy preserves current kinematics\");",
    "  check_qed(assigned,changed,\"assignment preserves current kinematics\");",
    "  assigned=assigned; check_qed(assigned,changed,\"self assignment\");",
    "  const auto copied_point=point(copied,9); copied.set_momenta(copied_point);",
    "  check_qed(copied,copied_point,\"copied cache pointers\");",
    "  const auto assigned_point=point(assigned,10); assigned.set_momenta(assigned_point);",
    "  check_qed(assigned,assigned_point,\"assigned cache pointers\");",
    "  std::unique_ptr<Process> survivor;",
    "  { Process source; auto value=point(source,11); source.set_momenta(value);",
    "    survivor.reset(new Process(source)); }",
    "  const auto survivor_point=point(*survivor,12); survivor->set_momenta(survivor_point);",
    "  check_qed(*survivor,survivor_point,\"copy outlives source\");",
    "  std::vector<Process> moved;",
    "  for (unsigned index=0; index<5; ++index) moved.push_back(convenience);",
    "  for (unsigned index=0; index<moved.size(); ++index) {",
    "    const auto value=point(moved[index],index+13); moved[index].set_momenta(value);",
    "    check_qed(moved[index],value,\"vector relocation\");", "  }",
    "  Composite composite; LongChain long_chain;",
    "  for (unsigned index=0; index<5; ++index) {",
    "    const auto cpoint=point(composite,index); composite.set_momenta(cpoint);",
    "    const auto lpoint=point(long_chain,index); long_chain.set_momenta(lpoint);",
    "    for (int ds1=-1; ds1<=1; ds1+=2) for (int ds2=-1; ds2<=1; ds2+=2) {",
    "      same(composite.amp({{ds1,ds2,0,0}}),direct_chain(composite,cpoint,ds1,ds2,true),",
    "        \"composite off-shell insertion\");",
    "      same(long_chain.amp({{ds1,ds2,0,0}}),direct_chain(long_chain,lpoint,ds1,ds2,false),",
    "        \"long momentum chain\");", "    }", "  }",
    "  Tensor tensor;",
    "  for (unsigned index=0; index<3; ++index) {",
    "    const auto momenta=point(tensor,index); tensor.set_momenta(momenta);",
    "    spinas::particle p1(tensor.masses()[0]), p2(tensor.masses()[1]);",
    "    p1.set_momentum(momenta[0].data()); p2.set_momentum(momenta[1].data());",
    "    spinas::sproduct angle(ANGLE,&p1,&p2), square(SQUARE,&p1,&p2);",
    "    hepcat_spinas::FourMomentum q;",
    "    for (unsigned mu=0;mu<4;++mu) q[mu]=momenta[0][mu]-momenta[2][mu];",
    "    const ldouble denominator=hepcat_spinas::mass2(q)-ldouble(9)/121;",
    "    for (int ds1=-2; ds1<=2; ds1+=2) for (int ds2=-2; ds2<=2; ds2+=2) {",
    "      cdouble expected(0,0);",
    "      for (const auto& one : hepcat_spinas::spinor_assignments(2,ds1))",
    "        for (const auto& two : hepcat_spinas::spinor_assignments(2,ds2))",
    "          expected += angle.v(one[0],two[0])*square.v(one[1],two[1]);",
    "      expected *= (ldouble(2)/7)*hepcat_spinas::spinor_normalization(2,ds1)*",
    "        hepcat_spinas::spinor_normalization(2,ds2)/denominator;",
    "      same(tensor.amp({{ds1,ds2,0,0}}),expected,\"tensor coefficient name collision\");",
    "    }", "  }",
    "  std::cout << \"NATIVE_PROJECT_COMPARISONS=\" << comparisons << '\\n'; return 0;",
    "} catch (const std::exception& error) { std::cerr << error.what() << '\\n'; return 1; } }"}], "\n"];
  npCppPath = npPath["native_lifecycle_oracle.cpp"];
  npExecutable = npPath["native_lifecycle_oracle"];
  Export[npCppPath, npCpp, "Text"];
  npPrecision = If[FileExistsQ[FileNameJoin[{npSpinas,"build","CMakeCache.txt"}]] &&
      StringContainsQ[Import[FileNameJoin[{npSpinas,"build","CMakeCache.txt"}],"Text"],
        "PRECISION_TYPE:STRING=LONG_DOUBLE"], {"-DWITH_LONG_DOUBLE"}, {}];
  npCompiled = npRun[Join[{"c++","-std=c++11","-O1"}, npPrecision,
    {"-I" <> FileNameJoin[{npSpinas,"include"}], "-I" <> npSpinas, npCppPath},
    (#["Files"]["Source"] & /@ {npFamilyReport,npDotReport,npCrossDotReport,npStableDotReport,
      npSquareReport,npClassMassReport,npClassProductReport,
      npTensorReport,npCompositeReport,npLongReport}),
    {FileNameJoin[{npSpinas,"SM","eeAA.cpp"}], First[npLibraries], "-o", npExecutable}]];
  check["native cache and fallback lifecycle projects compile and link", !npFailed[npCompiled]];
  If[npFailed[npCompiled], Print["NATIVE PROJECT COMPILATION: ",npCompiled],
    npRan = npRun[{npExecutable}];
    check["native caches, copies, setters, permutations and fallback chains agree with independent amplitudes",
      !npFailed[npRan] && StringContainsQ[npRan["StandardOutput"],"NATIVE_PROJECT_COMPARISONS="]];
    If[npFailed[npRan],Print["NATIVE PROJECT EXECUTION: ",npRan]]],
  Print["SKIP: native project numerical tests require the compiler, SPINAS headers and static library."]];
Print["NATIVE SPINAS PROJECT CHECKS: ",checks,"; FAILURES: ",Length[failures]];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:spinas-native-project-tests.wl"],Print[failures]];
Quit[If[failures === {},0,1]];
