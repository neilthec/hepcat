(* Compare every model vertex, in file order, with the public Mathematica reader.
   Set HEPCAT_MODEL_TEST to the absolute path of the built test_model executable. *)
$HistoryLength=0;
root=Replace[Environment["HEPCAT_ROOT"], Except[_String] :> DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root,"source","HEPCAT.wl"}]];
binary=Environment["HEPCAT_MODEL_TEST"];
If[!StringQ[binary] || !FileExistsQ[binary],Print["Set HEPCAT_MODEL_TEST to the native test_model executable."];Exit[1]];
test[dir_,num_]:=Module[{original,scope,file,run,rows,expected},
  original=ReadModel[dir,num];
  If[FailureQ[original],Print[original];Return[False]];
  scope=ConstructiveDiagrams`Private`modelParameterScope[dir,num];
  Internal`WithLocalSettings[file=CreateFile[],
    run=RunProcess[{binary,"--export",dir,ToString[num],scope,file}];
    If[run["ExitCode"]=!=0,Print[run["StandardError"]];Return[False]];
    rows=Function[line, With[{f=StringSplit[line,"\t"]},
      {StringSplit[First[f]],ToExpression[f[[2]]],ToExpression[f[[3]]]}]] /@ Import[file,"Lines"];
    expected= {DeleteCases[Drop[#,-2],""],#[[-2]],Last[#]} & /@ original[[2]];
    Print[{dir,num,Length[rows],SameQ[rows,expected]}]; SameQ[rows,expected],
    DeleteFile[file]]];
results=Join[test[FileNameJoin[{root,"models"}],#]& /@ Range[5],
  test[FileNameJoin[{root,"tests","reference-models","calchep"}],#]& /@ Range[2]];
If[!And@@results,Exit[1]];
