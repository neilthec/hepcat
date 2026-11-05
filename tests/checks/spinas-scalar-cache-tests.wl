(* Immutable constants and model-only factors must be reused without freezing
   parameter values, changing complex branches or colliding with model names. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root,"source","HEPCAT.wl"}]];
$HistoryLength = 0;
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures,name]; Print["FAIL: ",name]]);
scDirectory = CreateDirectory[];
scModel = ReadModel[FileNameJoin[{root,"models"}],3];
scMass = ModelParameter[scModel,"Me"]; scCharge = ModelParameter[scModel,"EE"];
scCoupling = ModelParameter["constant_sqrt2","scalar-cache-fixture","Complex"];
scModel[[1]] = Append[scModel[[1]], {"cache fixture scalar","r","r",992,0,scMass,0,1,1,"r","r"}];
scModel[[3]] = Append[scModel[[3]],{scCoupling,I/7}];
scProcess = ConstantArray["r",4];
scExpression = (Pi+Sqrt[2]+Exp[I/3]) (scCoupling+scCharge scMass)^2 Sqrt[scCoupling] *
  (1+MomProd[1,3])/PropDen[Mom[Multiparticle[1,3]],scMass];
scRules = {"Me"->1,"EE"->1/3};
scIR = ConstructiveDiagrams`Private`spinasProcessIR[scProcess,scExpression,scModel,<|"ParameterRules"->scRules|>];
check["closed numeric constants are recorded once", Count[Lookup[scIR["ConstantCaches"],"Expression"], Sqrt[2]] === 1 &&
  Count[Lookup[scIR["ConstantCaches"],"Expression"], Pi] === 1 &&
  DuplicateFreeQ[Lookup[scIR["ConstantCaches"],"Expression"]]];
check["only model-dependent expressions enter parameter factors",
  AllTrue[Lookup[scIR["ParameterCaches"],"Expression"],
    !FreeQ[#,_ModelParameter] && FreeQ[#,_Mom | _MomProd | _PropDen | _SpinorChain] &]];
check["the original symbolic amplitude is retained", scIR["OriginalExpression"] === scExpression];
scSpinas = Replace[Environment["HEPCAT_SPINAS_DIRECTORY"],$Failed|""->FileNameJoin[{ParentDirectory[root],"spinas"}]];
scReports = Table[
  name = "scalar_cache_"<>style;
  directory = FileNameJoin[{scDirectory,name}];
  report = WriteSPINASProcess[directory,scProcess,scExpression,scModel,
    "ProcessName"->name,"SPINASDirectory"->scSpinas,"ParameterRules"->scRules,
    "CalcHEPDirectory"->None,"CalcHEPModel"->None,"ReturnDetails"->True];
  If[style === "canonical" && AssociationQ[report],
    ir = ConstructiveDiagrams`Private`spinasMergeScalarCaches[scIR,{name}];
    Export[report["Files"]["Header"],ConstructiveDiagrams`Private`spinasCanonicalProjectHeader[name,ir],"Text"];
    Export[report["Files"]["Source"],ConstructiveDiagrams`Private`spinasCanonicalProjectSource[name,ir],"Text"]];
  check["project exported "<>style,AssociationQ[report]];
  If[AssociationQ[report],
    source = Import[report["Files"]["Source"],"Text"];
    header = Import[report["Files"]["Header"],"Text"];
    check["constants are immutable class members "<>style,StringContainsQ[header,"static const cdouble"]];
    check["positive numerical roots use real sqrt "<>style,StringContainsQ[source,"::constant_sqrt2_2 = std::sqrt("]];
    check["constant names do not shadow model inputs "<>style,StringContainsQ[header,"constant_sqrt2_2"]];
    check["parameter factors are refreshed by the setter "<>style,
      StringContainsQ[First[StringSplit[source,"::set_momenta",2]],"parameter_factor_"]]];
  report, {style,{"native","canonical"}}];
scCompiler = "c++";
scCompilerAvailable = Quiet[Check[RunProcess[{scCompiler,"--version"}]["ExitCode"] === 0, False]];
scLibrary = SelectFirst[{FileNameJoin[{scSpinas,"build","libspinas.a"}],FileNameJoin[{scSpinas,"lib","libspinas.a"}]},FileExistsQ,Missing["Library"]];
If[TrueQ[scCompilerAvailable] && StringQ[scLibrary] && AllTrue[scReports,AssociationQ],
  scNames = {"scalar_cache_native","scalar_cache_canonical"};
  scIncludes = ("#include "<>ToString[FileNameJoin[{scDirectory,#,"process.hpp"}],InputForm])& /@ scNames;
  scMassIndex = First[FirstPosition[scIR["ParameterNames"],"Me"]]-1;
  scChargeIndex = First[FirstPosition[scIR["ParameterNames"],"EE"]]-1;
  scCouplingIndex = First[FirstPosition[scIR["ParameterNames"],"constant_sqrt2"]]-1;
  scCpp = StringRiffle[Join[scIncludes,{
    "#include <iostream>","#include <cmath>",
    "template<class P> void exercise() {",
    "  P process;",
    "  for(int round=0; round<3; ++round) {",
    "    const ldouble mass=1+ldouble(round)/4, charge=ldouble(1)/3+ldouble(round)/20;",
    "    const cdouble coupling=round==0 ? cdouble(0,ldouble(1)/7) : round==1 ? cdouble(-2,0) : cdouble(ldouble(2)/5,-ldouble(1)/3);",
    "    auto parameters=P::default_parameters();",
    "    parameters["<>ToString[scMassIndex]<>"]=mass; parameters["<>ToString[scChargeIndex]<>"]=charge; parameters["<>ToString[scCouplingIndex]<>"]=coupling;",
    "    process.set_parameters(parameters);",
    "    bool rejected=false; try { process.amp(typename P::Spins{{0,0,0,0}}); } catch(const std::logic_error&) { rejected=true; }",
    "    if(!rejected) throw std::runtime_error(\"Parameter update retained stale kinematics\");",
    "    for(unsigned point=0;point<5;++point) {",
    "      const auto momenta=hepcat_spinas::make_example_momenta(process.masses(),P::incoming_legs(),ldouble(100),point);",
    "      process.set_momenta(momenta);",
    "      auto transferred=momenta[0]; for(int c=0;c<4;++c) transferred[c]-=momenta[2][c];",
    "      const cdouble constants=std::acos(ldouble(-1))+std::sqrt(ldouble(2))+std::exp(cdouble(0,ldouble(1)/3));",
    "      const cdouble expected=constants*std::pow(coupling+charge*mass,2)*std::pow(coupling,ldouble(0.5L))*(ldouble(1)-hepcat_spinas::dot(momenta[0],momenta[2]))/(hepcat_spinas::mass2(transferred)-mass*mass);",
    "      auto check=[&](P& value) { const auto actual=value.amp(typename P::Spins{{0,0,0,0}}); if(std::abs(actual-expected)>1e-11L*std::max(ldouble(1),std::abs(expected))) throw std::runtime_error(\"Cached amplitude differs from direct complex formula\"); };",
    "      check(process); P copied(process); check(copied); P assigned; assigned=process; check(assigned);",
    "    }",
    "  }",
    "}",
    "int main() { try { exercise<hepcat_generated::scalar_cache_native>(); exercise<hepcat_generated::scalar_cache_canonical>(); std::cout<<\"SCALAR_CACHE_NUMERICAL_PASS\\n\"; } catch(const std::exception& e) { std::cerr<<e.what()<<'\\n'; return 1; } }"}],"\n"];
  scCppPath = FileNameJoin[{scDirectory,"scalar_cache_oracle.cpp"}]; Export[scCppPath,scCpp,"Text"];
  scExecutable = FileNameJoin[{scDirectory,"scalar_cache_oracle"}];
  scDefinitions = If[StringContainsQ[Import[scReports[[1]]["Files"]["CMake"],"Text"],"PUBLIC WITH_LONG_DOUBLE"],{"-DWITH_LONG_DOUBLE"},{}];
  scCompiled = RunProcess[Join[{scCompiler,"-std=c++11","-O3"},scDefinitions,
    {"-I"<>FileNameJoin[{scSpinas,"include"}],scCppPath},Lookup[Lookup[scReports,"Files"],"Source"],
    {scLibrary,"-o",scExecutable}]];
  check["native and canonical cache projects compile",scCompiled["ExitCode"] === 0];
  If[scCompiled["ExitCode"] =!= 0,Print[scCompiled],
    scRun = RunProcess[{scExecutable}];
    check["complex phases, parameter updates and copies preserve the direct formula",scRun["ExitCode"] === 0 && StringContainsQ[scRun["StandardOutput"],"SCALAR_CACHE_NUMERICAL_PASS"]];
    If[scRun["ExitCode"] =!= 0,Print[scRun]]],
  Print["SKIP: scalar-cache numerical checks need a C++ compiler and SPINAS."]];
Print["SPINAS scalar-cache checks: ",checks,"; failures: ",Length[failures]];
If[failures === {},Print["HEPCAT_TEST_COMPLETED:spinas-scalar-cache-tests.wl"],Print[failures]];
Quit[If[failures === {},0,1]];
