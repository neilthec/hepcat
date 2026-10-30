(* The genuine CalcHEP numerical executable uses only the exported SPINAS
   amplitude. Its symbolic generator is disabled and trapped in these tests. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0;
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);
snDirectory = CreateDirectory[];
snPath[name_] := FileNameJoin[{snDirectory, name}];
snRun[command_List, directory_:Automatic] := Module[{previous = Directory[], result},
  If[StringQ[directory], SetDirectory[directory]];
  (* n_calchep.o embeds the original installation root. Override its runtime
     symbolic-library search explicitly so the private trap is authoritative. *)
  result = Quiet[Check[RunProcess[If[ValueQ[snNumericRoot] && StringQ[snNumericRoot],
    Join[{"/usr/bin/env", "CALCHEP=" <> snNumericRoot}, command], command]], <|"ExitCode" -> -1|>]];
  SetDirectory[previous]; result];
snSucceeded[report_] := AssociationQ[report] && Lookup[report, "ExitCode", -1] === 0;
snSpinas = Replace[Environment["HEPCAT_SPINAS_DIRECTORY"],
  $Failed | "" -> FileNameJoin[{ParentDirectory[root], "spinas"}]];
snCalcHEP = ConstructiveDiagrams`Private`spinasCalcHEPRoot[
  Replace[Environment["HEPCAT_CALCHEP_DIRECTORY"], $Failed | "" -> Automatic]];
snModel = ReadModel[FileNameJoin[{root, "models"}], 3]; snModelBefore = snModel;
snLookup[process_List] := LookupAmplitude[Thread[{process, Range[Length[process]]}],
  snModel, False, {snI, snJ, snK, snL}];
snFamily = {#, snLookup[#]} & /@ (Join[{"e", "E"}, #] & /@
  {{"A.+", "A.+"}, {"A.+", "A.-"}, {"A.-", "A.-"}});
snExport[name_String, calchep_, opts___] := WriteSPINASProcess[snPath[name],
  {"e", "E"} -> {"A", "A"}, snFamily, snModel, opts,
  "ProcessName" -> name, "ParameterRules" -> {"Me" -> 1, "EE" -> 1/3},
  "SPINASDirectory" -> snSpinas, "CalcHEPDirectory" -> calchep,
  "CalcHEPModel" -> None, "ReturnDetails" -> True];

snUnavailable = snExport["numerical_unavailable", None];
check["a numerical-disabled source export needs neither a CalcHEP model nor installation",
  AssociationQ[snUnavailable] && DirectoryQ[snUnavailable["Directory"]]];
If[!AssociationQ[snUnavailable], Print[snUnavailable]; Quit[1]];
snUnavailableManifest = Import[snUnavailable["Files"]["Manifest"], "RawJSON"];
check["independent symbolic validation remains unavailable when no counterpart model is selected",
  Lookup[Lookup[snUnavailableManifest, "CalcHEPValidation", <||>], "Status", None] === "UNAVAILABLE"];
check["the numerical engine has its own availability report",
  Lookup[Lookup[snUnavailableManifest, "CalcHEPNumerical", <||>], "Status", None] === "UNAVAILABLE"];
check["source export includes readable numerical adapter and runtime files",
  AllTrue[Lookup[snUnavailable["Files"], {"CalcHEPNumericalSource", "CalcHEPNumericalRuntime"}, ""],
    StringQ[#] && FileExistsQ[#] &]];
snAdapter = Import[snUnavailable["Files"]["CalcHEPNumericalSource"], "Text"];
check["the numerical adapter calls SPINAS rather than symbolic CalcHEP generation",
  StringContainsQ[snAdapter, "numerical_unavailable"] &&
    !StringContainsQ[snAdapter, "getMEcode" | "newProcess" | "s_calchep"]];

If[StringQ[snCalcHEP] && DirectoryQ[snCalcHEP] &&
    FileExistsQ[FileNameJoin[{snSpinas, "build", "libspinas.a"}]],
  (* A private numerical installation has all read-only libraries and help but
     a deliberately failing symbolic executable. Any accidental symbolic call
     is recorded independently of its return status. *)
  snNumericRoot = CreateDirectory[snPath["numeric-only-calchep"]];
  Do[If[FileExistsQ[FileNameJoin[{snCalcHEP, entry}]] || DirectoryQ[FileNameJoin[{snCalcHEP, entry}]],
    snRun[{"/bin/ln", "-s", FileNameJoin[{snCalcHEP, entry}], FileNameJoin[{snNumericRoot, entry}]}]],
    {entry, {"lib", "include", "c_source", "help", "pdTables", "FlagsForSh", "FlagsForMake"}}];
  CreateDirectory[FileNameJoin[{snNumericRoot, "bin"}]];
  snTrap = FileNameJoin[{snNumericRoot, "bin", "s_calchep"}];
  snTrapMarker = snPath["SYMBOLIC_GENERATOR_WAS_CALLED"];
  Export[snTrap, "#!/bin/sh\nprintf called >> '" <> snTrapMarker <> "'\nexit 91\n", "Text"];
  snRun[{"/bin/chmod", "+x", snTrap}];
  snUnusedModel = snModel;
  snUnsupported = ModelParameter["badFun", "numerical-unsupported-fixture", "Real"];
  snUnresolvedMass = ModelParameter["unsetMass", "numerical-unsupported-fixture", "Real"];
  snUnusedModel[[4]] = Append[snUnusedModel[[4]],
    {snUnsupported, Erf[ModelParameter[snModel, "Me"]]}];
  snUnusedModel[[1]] = Append[snUnusedModel[[1]],
    {"unused unresolved scalar", "uS", "uS", 990003, 0, snUnresolvedMass, 0, 1, "", "uS", "uS"}];
  snUnused = WriteSPINASProcess[snPath["numerical_unused"], {"e", "E"} -> {"A", "A"},
    snFamily, snUnusedModel, "ProcessName" -> "numerical_unused",
    "ParameterRules" -> {"Me" -> 1, "EE" -> 1/3}, "SPINASDirectory" -> snSpinas,
    "CalcHEPDirectory" -> snNumericRoot, "CalcHEPModel" -> None, "ReturnDetails" -> True];
  check["unrelated unsupported constraints and unresolved particle metadata do not disable a valid process",
    AssociationQ[snUnused] && Lookup[Lookup[snUnused, "CalcHEPNumerical", <||>], "Status", None] === "AVAILABLE"];
  If[AssociationQ[snUnused],
    snUnusedConfig = snUnused["CalcHEPNumerical"];
    check["omitted unused numerical definitions are identified explicitly",
      MemberQ[Lookup[snUnusedConfig, "OmittedUnsupportedParameters", {}], "badFun"] &&
        MemberQ[Lookup[snUnusedConfig, "OmittedParticleMetadata", {}], "uS"]]];
  snUsedFamily = ({First[#], snUnsupported Last[#]} &) /@ snFamily;
  snUsedUnsupported = WriteSPINASProcess[snPath["numerical_used_unsupported"],
    {"e", "E"} -> {"A", "A"}, snUsedFamily, snUnusedModel,
    "ProcessName" -> "numerical_used_unsupported", "ParameterRules" -> {"Me" -> 1, "EE" -> 1/3},
    "SPINASDirectory" -> snSpinas, "CalcHEPDirectory" -> snNumericRoot,
    "CalcHEPModel" -> None, "ReturnDetails" -> True];
  check["unsupported dependencies actually used by an amplitude are never silently dropped",
    FailureQ[snUsedUnsupported] || AssociationQ[snUsedUnsupported] &&
      Lookup[Lookup[snUsedUnsupported, "CalcHEPNumerical", <||>], "Status", None] === "UNAVAILABLE"];
  Do[
    snUnitExecutable = snPath["numerical-runtime-" <> snUnitPrecision];
    snUnitCompiled = snRun[Join[{"c++", "-std=c++11", "-O1", "-pthread"},
      If[snUnitPrecision === "long-double", {"-DWITH_LONG_DOUBLE"}, {}],
      {"-I" <> FileNameJoin[{root, "source"}], "-I" <> FileNameJoin[{snSpinas, "include"}],
       "-I" <> FileNameJoin[{snNumericRoot, "include"}],
       FileNameJoin[{root, "tests", "checks", "spinas-calchep-numerical-runtime-tests.cpp"}],
       "-o", snUnitExecutable}]];
    check["the numerical runtime compiles with " <> snUnitPrecision <> " SPINAS types", snSucceeded[snUnitCompiled]];
    If[snSucceeded[snUnitCompiled],
      snUnitRun = snRun[{snUnitExecutable}];
      check["permuted incoming legs, running couplings, constraints and error guards pass with " <> snUnitPrecision,
        snSucceeded[snUnitRun] && StringContainsQ[Lookup[snUnitRun, "StandardOutput", ""], "FAILURES=0"]];
      If[!snSucceeded[snUnitRun], Print["NUMERICAL RUNTIME UNIT: ", snUnitRun]],
      Print["NUMERICAL RUNTIME COMPILER: ", snUnitCompiled]],
    {snUnitPrecision, {"double", "long-double"}}];
  snBuilt = snExport["numerical_bridge", snNumericRoot, "Build" -> True];
  check["the public exporter builds n_calchep using only the numerical CalcHEP installation",
    AssociationQ[snBuilt] && DirectoryQ[snBuilt["Directory"]]];
  If[AssociationQ[snBuilt],
    snManifest = Import[snBuilt["Files"]["Manifest"], "RawJSON"];
    snConfiguration = Lookup[snManifest, "CalcHEPNumerical", <||>];
    check["numerical availability is independent of the disabled symbolic counterpart model",
      Lookup[snConfiguration, "Status", None] === "AVAILABLE" &&
        Lookup[Lookup[snManifest, "CalcHEPValidation", <||>], "Status", None] === "UNAVAILABLE"];
    snBinaries = FileNames["n_calchep", snBuilt["Directory"], Infinity];
    check["the generated project contains a genuine n_calchep executable", snBinaries =!= {}];
    If[snBinaries =!= {},
      snPreferred = Select[snBinaries, DirectoryQ[FileNameJoin[{DirectoryName[#], "aux"}]] &];
      snExecutable = First[If[snPreferred === {}, snBinaries, snPreferred]];
      snBinaryHash = FileHash[snExecutable, "SHA256"];
      snRunDirectory = DirectoryName[snExecutable];
      snStarted = snRun[{"/usr/bin/env", "nParProc=1", snExecutable, "-blind", "0"}, snRunDirectory];
      check["the real CalcHEP numerical menu starts headlessly with the SPINAS interface",
        snSucceeded[snStarted] && FileExistsQ[FileNameJoin[{snRunDirectory, "session.dat"}]]];
      If[!snSucceeded[snStarted], Print["NUMERICAL STARTUP: ", snStarted]];

      snParameterOrder = snManifest["ParameterOrder"];
      snMassIndex = First[FirstPosition[snParameterOrder, "Me"]] - 1;
      snChargeIndex = First[FirstPosition[snParameterOrder, "EE"]] - 1;
      snReference = snPath["numerical-reference"];
      snPrecision = If[Lookup[snManifest, "Precision", "LongDouble"] === "LongDouble", {"-DWITH_LONG_DOUBLE"}, {}];
      snCompiled = snRun[Join[{"c++", "-std=c++11", "-O1", "-pthread"}, snPrecision,
        {"-DHEPCAT_TEST_MASS_INDEX=" <> ToString[snMassIndex],
         "-DHEPCAT_TEST_CHARGE_INDEX=" <> ToString[snChargeIndex],
         "-I" <> snBuilt["Directory"], "-I" <> FileNameJoin[{snSpinas, "include"}],
         "-I" <> FileNameJoin[{snNumericRoot, "include"}],
         FileNameJoin[{root, "tests", "checks", "spinas-calchep-numerical-reference.cpp"}],
         snBuilt["Files"]["Source"], snBuilt["Files"]["CalcHEPNumericalSource"],
         FileNameJoin[{snSpinas, "build", "libspinas.a"}],
         FileNameJoin[{snNumericRoot, "lib", "dummy.a"}], "-o", snReference}]];
      check["the numerical adapter links to the process without the symbolic calculation library", snSucceeded[snCompiled]];
      If[snSucceeded[snCompiled],
        snCallbacks = snRun[{snReference}];
        check["direct callbacks preserve parameters, current masses, normalization and concurrent caches",
          snSucceeded[snCallbacks] && StringContainsQ[Lookup[snCallbacks, "StandardOutput", ""],
            "FAILURES=0"]];
        If[!snSucceeded[snCallbacks], Print["NUMERICAL CALLBACKS: ", snCallbacks]],
        Print["NUMERICAL REFERENCE COMPILER: ", snCompiled]];

      snDerivedModel = snModel;
      snModelMass = ModelParameter[snModel, "Me"];
      snModelCharge = ModelParameter[snModel, "EE"];
      snMassInput = ModelParameter["massInput", "numerical-derived-fixture", "Real"];
      snChargeInput = ModelParameter["chargeInput", "numerical-derived-fixture", "Real"];
      snMassIntermediate = ModelParameter["massMid", "numerical-derived-fixture", "Real"];
      snDerivedModel[[3]] = Join[Select[snDerivedModel[[3]],
        !MemberQ[{snModelMass, snModelCharge}, First[#]] &],
        {{snMassInput, 1/2}, {snChargeInput, 1/6},
         {ModelParameter["maxNp", "numerical-derived-fixture", "Real"], 11},
         {ModelParameter["Sin", "numerical-derived-fixture", "Real"], 2}}];
      (* Deliberately give constraints in reverse dependency order. *)
      snDerivedModel[[4]] = Join[snDerivedModel[[4]],
        {{snModelMass, snMassIntermediate/2}, {snModelCharge, 2 snChargeInput},
         {snMassIntermediate, 4 snMassInput}}];
      snDerived = WriteSPINASProcess[snPath["numerical_derived"], {"e", "E"} -> {"A", "A"},
        snFamily, snDerivedModel, "ProcessName" -> "numerical_derived",
        "SPINASDirectory" -> snSpinas, "CalcHEPDirectory" -> snNumericRoot,
        "CalcHEPModel" -> None, "ReturnDetails" -> True];
      check["model constraints may define process masses and couplings in arbitrary dependency order",
        AssociationQ[snDerived] && Lookup[Lookup[snDerived, "CalcHEPNumerical", <||>], "Status", None] === "AVAILABLE"];
      If[AssociationQ[snDerived],
        snDerivedManifest = Import[snDerived["Files"]["Manifest"], "RawJSON"];
        snDerivedOrder = snDerivedManifest["ParameterOrder"];
        snDerivedReference = snPath["derived-reference"];
        snDerivedCompiled = snRun[Join[{"c++", "-std=c++11", "-O1", "-pthread"}, snPrecision,
          {"-Dnumerical_bridge=numerical_derived",
           "-DHEPCAT_TEST_MASS_INDEX=" <> ToString[First[FirstPosition[snDerivedOrder, "Me"]] - 1],
           "-DHEPCAT_TEST_CHARGE_INDEX=" <> ToString[First[FirstPosition[snDerivedOrder, "EE"]] - 1],
           "-I" <> snDerived["Directory"], "-I" <> FileNameJoin[{snSpinas, "include"}],
           "-I" <> FileNameJoin[{snNumericRoot, "include"}],
           FileNameJoin[{root, "tests", "checks", "spinas-calchep-numerical-reference.cpp"}],
           snDerived["Files"]["Source"], snDerived["Files"]["CalcHEPNumericalSource"],
           FileNameJoin[{snSpinas, "build", "libspinas.a"}],
           FileNameJoin[{snNumericRoot, "lib", "dummy.a"}], "-o", snDerivedReference}]];
        check["a dependency-sorted numerical model compiles without symbolic calculation", snSucceeded[snDerivedCompiled]];
        If[snSucceeded[snDerivedCompiled],
          snDerivedRun = snRun[{snDerivedReference, "derived"}];
          check["independent parameter changes propagate through constraints into SPINAS masses and couplings",
            snSucceeded[snDerivedRun] && StringContainsQ[Lookup[snDerivedRun, "StandardOutput", ""], "FAILURES=0"]];
          If[!snSucceeded[snDerivedRun], Print["NUMERICAL DERIVED CALLBACKS: ", snDerivedRun]],
          Print["NUMERICAL DERIVED COMPILER: ", snDerivedCompiled]],
        Print["NUMERICAL DERIVED EXPORT: ", snDerived]];

      If[Environment["HEPCAT_TEST_CALCHEP_NUMERICAL_INTEGRATION"] === "1" && snSucceeded[snCompiled],
        snScripts = FileNameJoin[{snCalcHEP, "bin", #}] & /@ {"set_param", "set_momenta", "set_vegas", "run_vegas"};
        snSession = FileNameJoin[{snRunDirectory, "session.dat"}];
        snCommand[script_, arguments_List] := snRun[Join[{"/usr/bin/env", "nParProc=1", "/bin/bash", script}, arguments], snRunDirectory];
        snNumber[text_String] := Quiet[Check[ToExpression[StringReplace[StringTrim[text],
          {"E" -> "*^", "e" -> "*^"}]], $Failed]];
        snIntegral[run_Association, directory_String:snRunDirectory] := Module[{lines, pairs, integral, values},
          (* run_vegas prints a rounded mean and a relative percentage error.
             session.dat retains the full-precision sums and absolute variance. *)
          lines = StringSplit[Import[FileNameJoin[{directory, "session.dat"}], "Text"], "\n"];
          integral = SelectFirst[lines, StringStartsQ[StringTrim[#], "#Vegas_integral "] &, ""];
          If[integral =!= "",
            values = snNumber /@ Rest[StringSplit[integral]];
            If[Length[values] >= 4 && AllTrue[Take[values, 4], NumericQ] && values[[4]] > 0,
              Return[{values[[2]]/values[[4]], Sqrt[values[[1]]]/values[[4]]}]]];
          lines = StringSplit[Lookup[run, "StandardOutput", ""], "\n"];
          pairs = Select[StringSplit /@ lines, Length[#] === 2 && AllTrue[#, Function[token,
            StringMatchQ[token, RegularExpression["[+-]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:[Ee][+-]?[0-9]+)?"]]]] &];
          If[pairs === {}, $Failed, values = snNumber /@ Last[pairs];
            {First[values], Abs[First[values]] Last[values]/100}]];
        snIntegrate[mass_String, charge_String, calls_String] := Module[{parameter, beam, vegas, run, reference, measured, wanted},
          parameter = snCommand[snScripts[[1]], {"Me", mass, "EE", charge}];
          beam = snCommand[snScripts[[2]], {"1", "1"}];
          vegas = snCommand[snScripts[[3]], {"2", calls, "3", calls, "1000"}];
          run = snCommand[snScripts[[4]], {}];
          reference = snRun[{snReference, "quadrature", mass, charge, "1"}];
          measured = If[snSucceeded[run], snIntegral[run], $Failed];
          wanted = snNumber[StringTrim[StringReplace[Lookup[reference, "StandardOutput", ""], "CROSS_SECTION_PB=" -> ""]]];
          <|"Parameter" -> parameter, "Beam" -> beam, "Vegas" -> vegas, "Run" -> run,
            "Measured" -> measured, "Expected" -> wanted,
            "PASS" -> (AllTrue[{parameter, beam, vegas, run, reference}, snSucceeded] &&
              MatchQ[measured, {_?NumericQ, _?NumericQ}] && NumericQ[wanted] &&
              Abs[First[measured] - wanted] <= Max[6 Last[measured], 10^-6 Abs[wanted]])|>];
        snFirst = snIntegrate["1", "0.333333333333333333", "15000"];
        check["genuine CalcHEP VEGAS integration agrees with independent SPINAS angular quadrature", TrueQ[snFirst["PASS"]]];
        Print["NUMERICAL FIRST INTEGRAL: ", KeyTake[snFirst, {"Measured", "Expected", "PASS"}]];
        snCoupling = snIntegrate["1", "0.666666666666666667", "10000"];
        check["CalcHEP set_param changes the integrated coupling without rebuilding", TrueQ[snCoupling["PASS"]]];
        snMass = snIntegrate["1.5", "0.666666666666666667", "10000"];
        check["CalcHEP set_param changes masses in phase space and SPINAS without rebuilding", TrueQ[snMass["PASS"]]];
        Print["NUMERICAL CHANGED INTEGRALS: ", KeyTake[#, {"Measured", "Expected", "PASS"}] & /@ {snCoupling, snMass}];
        If[!AllTrue[{snFirst, snCoupling, snMass}, TrueQ[#["PASS"]] &],
          Print["NUMERICAL INTEGRATION DIAGNOSTICS: ", {snFirst, snCoupling, snMass}]];
        check["session parameter and mass changes did not rebuild the numerical executable",
          FileHash[snExecutable, "SHA256"] === snBinaryHash];

        (* A synthetic constant five-scalar amplitude tests genuine 2-to-3
           phase space without soft or collinear QED singularities. The three
           identical final scalars contribute a factor 1/3!, not an extra sum. *)
        snScalarModel = snModel;
        snScalar = ModelParameter["scalarG", "numerical-scalar-fixture", "Real"];
        snScalarModel[[1]] = Append[snScalarModel[[1]],
          {"numerical scalar", "q", "q", 990001, 0, 0, 0, 1, "", "q", "q"}];
        snScalarModel[[3]] = Append[snScalarModel[[3]], {snScalar, 2/7}];
        snScalarOutput = WriteSPINASProcess[snPath["numerical_scalar"], {"q", "q"} -> {"q", "q", "q"},
          {{ConstantArray["q", 5], snScalar}}, snScalarModel,
          "ProcessName" -> "numerical_scalar", "Build" -> True,
          "SPINASDirectory" -> snSpinas, "CalcHEPDirectory" -> snNumericRoot,
          "CalcHEPModel" -> None, "ReturnDetails" -> True];
        check["a five-leg scalar amplitude builds the same general numerical integration backend", AssociationQ[snScalarOutput]];
        If[AssociationQ[snScalarOutput],
          snScalarBinaries = FileNames["n_calchep", snScalarOutput["Directory"], Infinity];
          snScalarPreferred = Select[snScalarBinaries, DirectoryQ[FileNameJoin[{DirectoryName[#], "aux"}]] &];
          If[snScalarBinaries =!= {},
            snScalarExecutable = First[If[snScalarPreferred === {}, snScalarBinaries, snScalarPreferred]];
            snScalarDirectory = DirectoryName[snScalarExecutable];
            snScalarStarted = snRun[{"/usr/bin/env", "nParProc=1", snScalarExecutable, "-blind", "0"}, snScalarDirectory];
            snScalarBeam = snRun[{"/usr/bin/env", "nParProc=1", "/bin/bash", snScripts[[2]], "1", "1"}, snScalarDirectory];
            snScalarVegas = snRun[{"/usr/bin/env", "nParProc=1", "/bin/bash", snScripts[[3]], "2", "15000", "3", "15000", "1000"}, snScalarDirectory];
            snScalarRun = snRun[{"/usr/bin/env", "nParProc=1", "/bin/bash", snScripts[[4]]}, snScalarDirectory];
            snScalarMeasured = If[snSucceeded[snScalarRun], snIntegral[snScalarRun, snScalarDirectory], $Failed];
            snScalarExpected = N[389379660 (2/7)^2/(3072 Pi^3), 18];
            check["genuine three-body VEGAS agrees with analytic phase volume and identical-particle normalization",
              AllTrue[{snScalarStarted, snScalarBeam, snScalarVegas, snScalarRun}, snSucceeded] &&
                MatchQ[snScalarMeasured, {_?NumericQ, _?NumericQ}] &&
                Abs[First[snScalarMeasured] - snScalarExpected] <= Max[6 Last[snScalarMeasured], 10^-6 snScalarExpected]];
            Print["NUMERICAL THREE-BODY INTEGRAL: ", <|"Measured" -> snScalarMeasured, "Expected" -> snScalarExpected|>],
            check["the scalar project contains its numerical executable", False]],
          Print["NUMERICAL SCALAR EXPORT: ", snScalarOutput]],
        Print["SKIP: set HEPCAT_TEST_CALCHEP_NUMERICAL_INTEGRATION=1 for real VEGAS and session parameter-change checks."]]
    ];
    check["neither numerical startup nor integration invoked CalcHEP's symbolic generator", !FileExistsQ[snTrapMarker]],
    Print["NUMERICAL EXPORT: ", snBuilt]],
  Print["SKIP: real numerical backend checks require installed CalcHEP numerical libraries and built SPINAS."]];
check["numerical export leaves the supplied model unchanged", snModel === snModelBefore];
Print["SPINAS CALCHEP NUMERICAL CHECKS: ", checks, "; FAILURES: ", Length[failures]];
If[failures === {}, DeleteDirectory[snDirectory, DeleteContents -> True];
  Print["HEPCAT_TEST_COMPLETED:spinas-calchep-numerical-tests.wl"],
  Print[failures]; Print["CALCHEP NUMERICAL FILES: ", snDirectory]];
Quit[If[failures === {}, 0, 1]];
