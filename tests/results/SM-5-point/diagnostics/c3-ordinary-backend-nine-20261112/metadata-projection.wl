ids={"sm5f_46dff720d04e", "sm5f_1c93ccede818", "sm5f_33c3db088b44", "sm5f_0d47340a86fc", "sm5f_760cff433644", "sm5f_131dcfb0924a", "sm5f_8b597672754a", "sm5f_5647852b858c", "sm5f_4fcc3b7daa73"};
root="/Users/neil/code/hepcat";
rows=Table[With[{path=FileNameJoin[{root,"tests","results","SM-5-point",id<>"-export.wxf"}]},With[{c=Import[path,"WXF"]},<|"FamilyID"->id,"CapsuleSHA256"->IntegerString[FileHash[path,"SHA256"],16,64],"Record"->KeyTake[c,{"ContextSHA256","CaseIDs","CaseCapsulesSHA256","NativeCaseContexts","Status","Seconds","Points"}],"BackendResult"->KeyTake[Lookup[c,"Result",<||>],{"CalcHEPValidation","MadGraphValidation","Manifest","Files"}]|>]],{id,ids}];
If[!StringQ[Quiet[Check[Export["/private/tmp/hepcat-sm5-nine-backend-projection.json",rows/._Missing->Null/.None->Null,"RawJSON"],$Failed]]],Print["PROJECTION_FAILED"];Quit[2]];
Print["PROJECTED ",Length[rows]," small actual export capsules; no package load or physics."];
