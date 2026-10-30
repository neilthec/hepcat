(* GUI kernels can build SPINAS even when their PATH omits CMake's app bundle. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0;
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++; If[!TrueQ[value],
  AppendTo[failures, name]; Print["FAIL: ", name]]);
sbDirectory = CreateDirectory[];
sbPath[name_] := FileNameJoin[{sbDirectory, name}];
sbSpinas = Replace[Environment["HEPCAT_SPINAS_DIRECTORY"],
  $Failed | "" -> FileNameJoin[{ParentDirectory[root], "spinas"}]];
sbModel = ReadModel[FileNameJoin[{root, "models"}], 3];
sbRules = {"Me" -> 1, "EE" -> 1/3};
sbProcesses = Join[{"e", "E"}, #] & /@
  {{"A.+", "A.+"}, {"A.+", "A.-"}, {"A.-", "A.-"}};
sbLookup[process_List] := LookupAmplitude[Thread[{process, Range[Length[process]]}],
  sbModel, False, {sbI, sbJ, sbK, sbL}];
sbFamily = {#, sbLookup[#]} & /@ sbProcesses;
sbResolver[option_] := ConstructiveDiagrams`Private`spinasProjectCMakeExecutable[option];
sbRun[args_List] := Quiet[Check[RunProcess[args], <|"ExitCode" -> -1|>]];
sbOptions = {"SPINASDirectory" -> sbSpinas, "ParameterRules" -> sbRules,
  "ReturnDetails" -> True};
sbValidReport[report_] := AssociationQ[report] &&
  Lookup[Lookup[report, "Build", <||>], "Status", None] === "BUILT" &&
  Lookup[Lookup[report, "Examples", <||>], "Status", None] === "RAN" &&
  Lookup[Lookup[report, "Examples", <||>], "ExitCode", -1] === 0;
sbCMake = sbResolver[Automatic];
check["CMake discovery defaults to Automatic",
  ("CMakeExecutable" /. Options[WriteSPINASProcess]) === Automatic];
check["an explicit missing CMake program reports a useful failure",
  FailureQ[sbResolver[sbPath["does not exist/cmake"]]]];
check["a directory is not accepted as a CMake executable",
  FailureQ[sbResolver[sbDirectory]]];
If[$OperatingSystem =!= "Windows",
  check["a successful unrelated program is not mistaken for CMake",
    FailureQ[sbResolver["/usr/bin/true"]]]];

Do[
  sbMissingDirectory = sbPath["missing_tool_" <> ToString[sbKind]];
  sbMissing = If[sbKind === 1,
    WriteSPINASProcess[sbMissingDirectory, First[sbProcesses], Last[First[sbFamily]],
      sbModel, Sequence @@ sbOptions,
      "CMakeExecutable" -> sbPath["missing/cmake"], "Build" -> True],
    WriteSPINASProcess[sbMissingDirectory, sbFamily, sbModel, Sequence @@ sbOptions,
      "CMakeExecutable" -> sbPath["missing/cmake"], "RunExamples" -> True]];
  check["an unavailable explicitly requested tool fails before writing project " <> ToString[sbKind],
    FailureQ[sbMissing] && !DirectoryQ[sbMissingDirectory]],
  {sbKind, 2}];
sbWithoutBuild = WriteSPINASProcess[sbPath["source_only"], sbFamily, sbModel,
  Sequence @@ sbOptions, "CMakeExecutable" -> sbPath["missing/cmake"]];
check["source-only export does not require an installed build tool",
  AssociationQ[sbWithoutBuild] && sbWithoutBuild["Build"]["Status"] === "NOT_REQUESTED"];
sbBadTypePath = sbPath["invalid_option_type"];
sbBadType = Quiet[WriteSPINASProcess[sbBadTypePath, sbFamily, sbModel,
  Sequence @@ sbOptions, "CMakeExecutable" -> 17]];
check["an invalid CMake option type is rejected before creating files",
  sbBadType === $Failed && !DirectoryQ[sbBadTypePath]];

If[StringQ[sbCMake],
  check["automatic discovery returns an executable that identifies itself as CMake",
    FileExistsQ[sbCMake] && StringContainsQ[
      Lookup[sbRun[{sbCMake, "--version"}], "StandardOutput", ""], "cmake version"]];
  check["an explicit installed CMake path is honored",
    sbResolver[sbCMake] === sbCMake];
  sbNativeAvailable = FileExistsQ[FileNameJoin[{sbSpinas, "include", "spinas.h"}]] &&
    AnyTrue[FileNameJoin[{sbSpinas, #, "libspinas.a"}] & /@ {"build", "lib"}, FileExistsQ];
  If[sbNativeAvailable && $OperatingSystem === "MacOSX",
    sbOriginalPath = Environment["PATH"];
    sbNarrowPath = "/usr/bin:/bin:/usr/sbin:/sbin";
    SetEnvironment["PATH" -> sbNarrowPath];
    sbNarrowWhich = sbRun[{"/usr/bin/which", "cmake"}];
    check["the GUI-style PATH really omits CMake",
      Lookup[sbNarrowWhich, "ExitCode", -1] =!= 0];
    sbFallback = sbResolver[Automatic];
    check["automatic lookup finds the installed CMake outside the GUI-style PATH",
      StringQ[sbFallback] && FileExistsQ[sbFallback]];
    sbLegacy = WriteSPINASProcess[sbPath["legacy GUI build"], First[sbProcesses],
      Last[First[sbFamily]], sbModel, Sequence @@ sbOptions,
      "ProcessName" -> "gui_fixed", "Build" -> True, "RunExamples" -> True];
    sbGrouped = WriteSPINASProcess[sbPath["family GUI build"], sbFamily, sbModel,
      Sequence @@ sbOptions, "ProcessName" -> "gui_family",
      "Build" -> True, "RunExamples" -> True];
    SetEnvironment["PATH" -> sbOriginalPath];
    check["the fixed-helicity project builds and runs despite CMake missing from PATH",
      sbValidReport[sbLegacy]];
    check["the complete helicity family builds and runs despite CMake missing from PATH",
      sbValidReport[sbGrouped] && Length[sbGrouped["HelicityConfigurations"]] === 4];
    check["build details identify the CMake executable actually used",
      sbValidReport[sbLegacy] && sbValidReport[sbGrouped] &&
        sbLegacy["Build"]["CMakeExecutable"] === sbFallback &&
        sbGrouped["Build"]["CMakeExecutable"] === sbFallback];
    check["the generated family runs all three conserved example points",
      sbValidReport[sbGrouped] && AllTrue[{"Point 1", "Point 2", "Point 3"},
        StringContainsQ[sbGrouped["Examples"]["StandardOutput"], #] &]];
    check["the original process environment is restored", Environment["PATH"] === sbOriginalPath];
    If[!sbValidReport[sbLegacy], Print["FIXED BUILD DETAILS: ", sbLegacy]];
    If[!sbValidReport[sbGrouped], Print["FAMILY BUILD DETAILS: ", sbGrouped]];
    (* Use a real wrapper with spaces to exercise argument-vector execution. *)
    sbWrapper = sbPath["CMake command with spaces"];
    sbQuoted[argument_String] := "'" <> StringReplace[argument, "'" -> "'\"'\"'"] <> "'";
    Export[sbWrapper, "#!/bin/sh\nexec " <> sbQuoted[sbCMake] <> " \"$@\"\n", "Text"];
    sbRun[{"/bin/chmod", "+x", sbWrapper}];
    sbExplicit = WriteSPINASProcess[sbPath["explicit tool build"], sbFamily, sbModel,
      Sequence @@ sbOptions, "ProcessName" -> "explicit_family",
      "CMakeExecutable" -> sbWrapper, "RunExamples" -> True];
    check["an explicit executable path with spaces configures, builds and runs",
      sbValidReport[sbExplicit] && sbExplicit["Build"]["CMakeExecutable"] === sbWrapper];
    If[!sbValidReport[sbExplicit], Print["EXPLICIT BUILD DETAILS: ", sbExplicit]],
    Print["SKIP: GUI PATH compile checks need macOS and the built SPINAS library."]],
  Print["SKIP: installed CMake integration checks need a discoverable CMake executable."]];

Print["SPINAS BUILD TOOL CHECKS: ", checks, "; FAILURES: ", Length[failures]];
If[failures === {}, DeleteDirectory[sbDirectory, DeleteContents -> True];
  Print["HEPCAT_TEST_COMPLETED:spinas-build-tools-tests.wl"],
  Print[failures]; Print["BUILD TOOL TEST FILES: ", sbDirectory]];
Quit[If[failures === {}, 0, 1]];
