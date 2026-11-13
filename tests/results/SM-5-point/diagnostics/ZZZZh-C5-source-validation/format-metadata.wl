Begin["ZZZZhMetadataFormat`Private`"]; $ContextPath={"System`"};
dir="/Users/neil/code/hepcat/tests/results/SM-5-point/diagnostics/ZZZZh-C5-source-validation";
s=Import[FileNameJoin[{dir,"metadata.wxf"}],"WXF"];
Clear[safe];safe[a_Association]:=Association[KeyValueMap[(ToString[#1]->safe[#2])&,a]];
safe[l_List]:=safe/@l;safe[v_?NumberQ]:=v;safe[v_String]:=v;safe[True]=True;safe[False]=False;safe[Null]=Null;
safe[v_]:=ToString[v,InputForm];
Export[FileNameJoin[{dir,"metadata.json"}],safe[s],"RawJSON"];
Do[he=SelectFirst[s["ValidationReports"][[i,"Checks"]],Lookup[#,"Type",""]==="HighEnergy"&,<||>];
 Print["ASSIGNMENT ",i," HE ",InputForm[he]],{i,3}];Quit[];
