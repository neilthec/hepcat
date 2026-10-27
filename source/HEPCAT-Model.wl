(* ::Package:: *)

(* ::Subsection::Closed:: *)
(*Model Reader*)


(* ::Subsubsection::Closed:: *)
(*Model Reader*)


(* ::Input::Initialization:: *)
ReadModel::usage = "ReadModel[directory, number] reads a constructive ampsN.mdl model as the original four-element list, or a CalcHEP lgrngN.mdl model as FeynmanModel[particles, vertices, variables, functions]. Raw Feynman vertices are not complete amplitudes for factorization sewing.";
ReadModel[dir_,num_]:=Module[{particleList,amplitudeList,variableList,functionList},
If[FileExistsQ[FileNameJoin[{dir,"lgrng"<>ToString[num]<>".mdl"}]]&&
  !FileExistsQ[FileNameJoin[{dir,"amps"<>ToString[num]<>".mdl"}]],
  Return[ConstructiveDiagrams`Private`readFeynmanModel[dir,num]]];
particleList=ReadParticles[dir<>"/prtcls"<>ToString[num]<>".mdl"];
If[FailureQ[particleList],Return[particleList]];
amplitudeList=ReadAmplitudes[dir<>"/amps"<>ToString[num]<>".mdl",particleList];
If[FailureQ[amplitudeList],Return[amplitudeList]];
variableList=ReadVariables[dir<>"/vars"<>ToString[num]<>".mdl"];
If[FailureQ[variableList],Return[variableList]];
functionList=ReadFunctions[dir<>"/func"<>ToString[num]<>".mdl"];
If[FailureQ[functionList],Return[functionList]];

{particleList,amplitudeList,variableList,functionList}
]

ConstructiveDiagrams`Private`readModelRows[file_]:=Module[
  {stream,line,rows={},terminated=False,result},
stream=Quiet[Check[OpenRead[file],$Failed]];
If[!MatchQ[stream,_InputStream],Return[Failure["ModelFileUnavailable",
  <|"File"->file,"MessageTemplate"->"The model file could not be opened."|>]]];
result=CheckAbort[
  While[True,
    line=ReadLine[stream];
    If[line===EndOfFile||!StringQ[line],Break[]];
    line=StringTrim[StringSplit[line,"|"]];
    If[line==={},Continue[]];
    If[StringContainsQ[First[line],"======"],terminated=True;Break[]];
    AppendTo[rows,line]
  ];
  If[terminated,rows,Failure["UnterminatedModelFile",
    <|"File"->file,"MessageTemplate"->"The model file is missing its final separator."|>]],
  Close[stream];Abort[]];
Close[stream];
result
]


(* ::Subsubsection::Closed:: *)
(*Variable Reader*)


(* ::Input::Initialization:: *)
ReadVariables[file_]:=Module[{rows,variableList={},line,scope,parsed,tag=Unique["modelVariables"]},
rows=ConstructiveDiagrams`Private`readModelRows[file];
If[FailureQ[rows],Return[rows]];
scope=ConstructiveDiagrams`Private`modelParameterScope[file];
Catch[
Do[
If[Length[line]==3&&line[[1]]=!="Name",
If[!StringMatchQ[line[[1]],RegularExpression["[A-Za-z_][A-Za-z_0-9]*"]],
Throw[Failure["InvalidModelParameter",<|"File"->file,"Name"->line[[1]]|>],tag]];
parsed=ConstructiveDiagrams`Private`parseModelExpression[line[[2]],scope];
If[FailureQ[parsed],Throw[parsed,tag]];
line[[1]]=ModelParameter[line[[1]],scope,"Real"];
line[[2]]=parsed;
AppendTo[variableList,line];
];
,{line,rows}];
variableList,tag]
]


(* ::Subsubsection::Closed:: *)
(*Function Reader*)


(* ::Input::Initialization:: *)
ReadFunctions[file_]:=Module[{rows,functionList={},line,rside,scope,parsed,tag=Unique["modelFunctions"]},
rows=ConstructiveDiagrams`Private`readModelRows[file];
If[FailureQ[rows],Return[rows]];
scope=ConstructiveDiagrams`Private`modelParameterScope[file];
Catch[
Do[
If[Length[line]==2&&line[[1]]=!="Name",
If[!StringMatchQ[line[[1]],RegularExpression["[A-Za-z_][A-Za-z_0-9]*"]],
Throw[Failure["InvalidModelParameter",<|"File"->file,"Name"->line[[1]]|>],tag]];
line[[1]]=ModelParameter[line[[1]],scope,"Real"];
rside=StringTrim[StringSplit[line[[2]],"%"]];
If[rside==={},Throw[Failure["InvalidModelExpression",<|"File"->file|>],tag]];
parsed=ConstructiveDiagrams`Private`parseModelExpression[rside[[1]],scope];
If[FailureQ[parsed],Throw[parsed,tag]];
line[[2]]=parsed;
If[Length[rside]==2,AppendTo[line,rside[[2]]],AppendTo[line,""]];
AppendTo[functionList,line];
];
,{line,rows}];
functionList,tag]
]


(* ::Subsubsection::Closed:: *)
(*Particle Reader*)


(* ::Input::Initialization:: *)
ReadParticles[file_]:=Module[{rows,particleList={},line,scope,parsed,tag=Unique["modelParticles"]},
rows=ConstructiveDiagrams`Private`readModelRows[file];
If[FailureQ[rows],Return[rows]];
scope=ConstructiveDiagrams`Private`modelParameterScope[file];
Catch[
Do[
If[Length[line]==11&&!StringContainsQ[line[[1]],RegularExpression["Full\\s*Name"]],
Do[parsed=ConstructiveDiagrams`Private`parseModelExpression[line[[ii]],scope];
If[FailureQ[parsed],Throw[parsed,tag]];line[[ii]]=parsed;,{ii,{4,5,6,8}}];
AppendTo[particleList,line];
];
,{line,rows}];
particleList,tag]
]


(* ::Input::Initialization:: *)
ParticleMass[prtcl_,particleList_]:=Module[{res=-1},
Do[
If[prtcl==particleList[[ii,2]]||prtcl==particleList[[ii,3]],res=particleList[[ii,6]]];
,{ii,1,Length[particleList]}];
If[prtcl=="",res=0];
res
]


(* ::Subsubsection::Closed:: *)
(*Amplitude Reader*)


(* ::Input::Initialization:: *)
ReadAmplitudes[file_,particleList_]:=Module[{rows,amplitudeList={},line,scope,parsed,tag=Unique["modelAmplitudes"]},
rows=ConstructiveDiagrams`Private`readModelRows[file];
If[FailureQ[rows],Return[rows]];
scope=ConstructiveDiagrams`Private`modelParameterScope[file];
Catch[
Do[
If[Length[line]>=5&&line[[3]]!="P3"&&!StringContainsQ[line[[1]],"%"],
parsed=ConstructiveDiagrams`Private`parseModelExpression[line[[-2]],scope];
If[FailureQ[parsed],Throw[parsed,tag]];line[[-2]]=parsed;
parsed=ConvertAmplitude[line,particleList,scope];
If[FailureQ[parsed],Throw[parsed,tag]];line[[-1]]=parsed;
AppendTo[amplitudeList,line];
];
,{line,rows}];
(*AddOppositeHelicityAmplitudes[amplitudeList]*)
amplitudeList,tag]
]


(* ::Input::Initialization:: *)
ConvertAmplitude[line_,particleList_,scope_:Automatic]:=Module[
{masses={},res,SpinHelicity,parameterScope,segments,compactIndices,compactPattern,
 compactChain,convertSegment},
parameterScope=If[scope===Automatic,FirstCase[particleList,
ModelParameter[_String,s_String,_String]:>s,"StandaloneConstructiveAmplitude",Infinity],scope];
If[!StringQ[parameterScope],Return[Failure["InvalidModelParameterScope",<||>]]];
SpinHelicity[mass_]:=If[mass===0,"Helicity","Spin"];
Do[AppendTo[masses,ParticleMass[line[[ii]],particleList]],{ii,1,Length[line]-2}];
(*If[line[[4]]=!="",AppendTo[masses,ParticleMass[line[[4]],particleList]]];*)
(* Compact brackets must not consume typed arguments such as Mom[10],
   quoted labels, or substrings of model parameter names. *)
segments = StringCases[line[[-1]], RegularExpression["\"[^\"\\\\]*\"|[^\"]+"]];
If[StringJoin[segments] =!= line[[-1]],
  Return[Failure["InvalidModelExpression", <|"Expression" -> line[[-1]],
    "Reason" -> "Unmatched quotes or unsupported quoted-string escapes."|>]]];
segments = If[StringStartsQ[#, "\""], #, StringReplace[#,
    RegularExpression["([A-Za-z_][A-Za-z_0-9]*)[ \\t\\r\\n]+\\["] -> "$1["]] & /@ segments;
compactPattern = RegularExpression["(?<![A-Za-z_0-9])(<|\\[)([0-9]{2,})(>|\\])"];
compactIndices = Flatten[Characters /@ Flatten[
  StringCases[#, compactPattern -> "$2"] & /@
    Select[segments, !StringStartsQ[#, "\""] &]]];
If[!AllTrue[compactIndices, 1 <= FromDigits[#] <= Length[masses] &],
  Return[Failure["InvalidAmplitudeParticleLabel", <|"Expression" -> line[[-1]]|>]]];
compactChain[left_, digits_, right_] := Module[{labels = FromDigits /@ Characters[digits], endpoints},
  endpoints = MapThread[Function[{label, kind},
    "Spinor[\"" <> SpinHelicity[masses[[label]]] <> "\",\"" <> kind <> "\"," <>
      ToString[label] <> "]"],
    {{First[labels], Last[labels]},
      {If[left === "<", "Angle", "Square"], If[right === ">", "Angle", "Square"]}}];
  "SpinorChain[" <> StringRiffle[Join[{First[endpoints]},
    ("Mom[" <> ToString[#] <> "]" & /@ Rest[Most[labels]]), {Last[endpoints]}], ","] <> "]"];
convertSegment[text_] := If[StringStartsQ[text, "\""], text, StringReplace[text, {
RegularExpression["(?<![A-Za-z_0-9])x(\\d)(\\d)(?![A-Za-z_0-9])"]:>"xFactor[$1,$2]",
RegularExpression["(?<![A-Za-z_0-9])xt(\\d)(\\d)(?![A-Za-z_0-9])"]:>"xTildeFactor[$1,$2]",
compactPattern :> compactChain["$1", "$2", "$3"],
RegularExpression["(?<![A-Za-z_0-9])s(\\d)(\\d)-([a-zA-Z_][a-zA-Z_0-9]*|[0-9]+)\\^2(?![A-Za-z_0-9])"]:>"PropDen[Mom[Multiparticle[$1,$2]],$3]",
RegularExpression["(?<![A-Za-z_0-9])s(\\d)(\\d)(?![A-Za-z_0-9])"]:>"PropDen[Mom[Multiparticle[$1,$2]],0]"
}]];
res = StringJoin[convertSegment /@ segments];
ConstructiveDiagrams`Private`parseModelExpression[res,parameterScope]
];


(* ::Input::Initialization:: *)
ConvertAmplitudeOld[amp_,particleList_]:=Module[{masses={},res,SpinHelicity},
(*SpinHelicity[mass_]:=If[mass===0,"Helicity","Spin"];
Do[AppendTo[masses,ParticleMass[line[[ii]],particleList]],{ii,1,3}];
If[line[[4]]=!="",AppendTo[masses,ParticleMass[line[[4]],particleList]]];*)
res=StringReplace[amp,{
RegularExpression["x(\\d)(\\d)"]:>"xFactor[$1,$2]",
RegularExpression["<(\\d)(\\d)>"]:>"SpinorChain[Spinor[\""<>SpinHelicity[masses[[ToExpression["$1"]]]]<>"\",\"Angle\",$1],Spinor[\""<>SpinHelicity[masses[[ToExpression["$2"]]]]<>"\",\"Angle\",$2]]",
RegularExpression["\[(\\d)(\\d)\]"]:>"SpinorChain[Spinor[\""<>SpinHelicity[masses[[ToExpression["$1"]]]]<>"\",\"Square\",$1],Spinor[\""<>SpinHelicity[masses[[ToExpression["$2"]]]]<>"\",\"Square\",$2]]"
}];
ToExpression[res]
];


AddOppositeHelicityAmplitudes[vertices_]:=Module[{newVertices={},i,j,k,containsHelicity,amplitude},
Do[
AppendTo[newVertices,vertices[[i]]];
containsHelicity=False;
Do[If[StringEndsQ[vertices[[i,j]],{".+",".-"}],containsHelicity=True],{j,1,Length[vertices[[i]]]-2}];
If[containsHelicity,
amplitude={};
Do[AppendTo[amplitude,StringReplace[vertices[[i,j]],{".+"->".-",".-"->".+"}]],{j,1,Length[vertices[[i]]]-2}];
AppendTo[amplitude,vertices[[i,-2]]];
AppendTo[amplitude,vertices[[i,-1]]/.{xFactor[a__]:>xTildeFactor[a],xTildeFactor[a__]:>xFactor[a],spinor_Spinor:>(spinor/.{"Angle"->"Square","Square"->"Angle"})}];
AppendTo[newVertices,amplitude];
];
,{i,1,Length[vertices]}];
newVertices
]


(* ::Subsubsection::Closed:: *)
(*Amplitude Writer*)


AppendModelAmplitudes::usage = "AppendModelAmplitudes[directory, number, entries] appends complete constructive amplitudes to ampsN.mdl before its final separator, preserving existing entries. Each entry is {particles, amplitude}, or {particles, factor, amplitudeOverFactor}. The target ReadModel supplies particle and parameter declarations. Identical existing entries are skipped; conflicting entries for the same particle multiset return Failure. Every row is checked by reading it back before a single atomic file replacement. This writes supplied amplitudes, not a physics validation or new diagram vertices. It returns the file path and Added/Skipped counts. Reload ReadModel after writing.";
AppendModelAmplitudes[directory_String, number_Integer, entries_List] :=
  ConstructiveDiagrams`Private`appendModelAmplitudes[directory, number, entries];
AppendModelAmplitudes[___] := Failure["InvalidModelAmplitudeExport", <|
  "MessageTemplate" -> "Expected a model directory, integer model number and a list of amplitude entries."|>];

Begin["`Private`"];

modelExportEqualQ[a_, b_] := SameQ[a, b] || TrueQ[Quiet[Check[Together[a - b] === 0, False]]];

appendModelAmplitudes[directory_String, number_Integer, entries_List] := Module[
  {model, file, original, text, scope, declared, names, positions, separator,
   prefix, suffix, newline, rows = {}, known, added = 0, skipped = 0,
   entry, particles, factor, expression, references, foreign, factorText,
   expressionText, readFactor, readExpression, existing, existingParticles,
   body, headers, columns, currentColumns, temporary = None, stream, result,
   tag = Unique["modelAmplitudeExport"]},
  file = FileNameJoin[{ExpandFileName[directory], "amps" <> ToString[number] <> ".mdl"}];
  original = Quiet[Check[BinaryReadList[file, "Byte"], $Failed]];
  If[!ListQ[original], Return[Failure["ModelFileUnavailable", <|"File" -> file|>]]];
  model = ReadModel[directory, number];
  If[FailureQ[model], Return[model]];
  If[!MatchQ[model, {_List, _List, _List, _List}], Return[
    Failure["ConstructiveModelRequired", <|"File" -> file,
      "MessageTemplate" -> "Complete spinor amplitudes must be written to a constructive ampsN.mdl model."|>]]];
  text = Quiet[Check[FromCharacterCode[original, "UTF-8"], $Failed]];
  If[!StringQ[text] || ToCharacterCode[text, "UTF-8"] =!= original,
    Return[Failure["InvalidModelEncoding", <|"File" -> file|>]]];
  positions = StringPosition[text, RegularExpression["(?m)^[^\\r\\n]*={6,}[^\\r\\n]*"]];
  If[Length[positions] =!= 1, Return[Failure["InvalidModelSeparator", <|"File" -> file|>]]];
  separator = First[positions];
  prefix = StringTake[text, First[separator] - 1];
  suffix = StringDrop[text, First[separator] - 1];
  If[StringTrim[StringDrop[text, Last[separator]]] =!= "",
    Return[Failure["TrailingModelData", <|"File" -> file,
      "MessageTemplate" -> "The model contains data after its final separator."|>]]];
  newline = If[StringContainsQ[text, "\r\n"], "\r\n", "\n"];
  scope = modelParameterScope[file];
  declared = DeleteDuplicates[First /@ Join[model[[3]], model[[4]]]];
  names = DeleteDuplicates[Flatten[#[[{2, 3}]] & /@ model[[1]]]];
  known = model[[2]];
  headers = readModelRows[file];
  If[FailureQ[headers], Return[headers]];
  headers = Select[headers, Length[#] >= 5 && #[[3]] === "P3" &];
  currentColumns = If[headers === {}, None, Length[Last[headers]] - 2];
  result = Catch[
    Do[
      If[!MatchQ[entry, {_List, _} | {_List, _, _}],
        Throw[Failure["InvalidModelAmplitudeEntry", <|"Entry" -> entry|>], tag]];
      particles = First[entry];
      If[Length[particles] < 3 || !AllTrue[particles,
          StringQ[#] && # =!= "" && !StringContainsQ[#, {"|", "\r", "\n"}] &&
            MemberQ[names, #] &],
        Throw[Failure["UnknownAmplitudeParticles", <|"Particles" -> particles|>], tag]];
      {factor, expression} = If[Length[entry] === 2, {1, Last[entry]}, Rest[entry]];
      references = DeleteDuplicates[Cases[{factor, expression},
        p_ModelParameter :> p, Infinity]];
      foreign = Select[references, !MemberQ[declared, #] &];
      If[foreign =!= {}, Throw[Failure["UndeclaredAmplitudeParameters", <|
        "Particles" -> particles, "Parameters" -> foreign,
        "MessageTemplate" -> "Amplitude parameters must belong to the target model; equal printed names do not identify different model scopes."|>], tag]];
      factorText = PlainText[factor, "Model"];
      expressionText = PlainText[expression, "Model"];
      If[!StringQ[factorText] || !StringQ[expressionText] ||
          AnyTrue[{factorText, expressionText},
            StringQ[#] && StringContainsQ[#, {"|", "\r", "\n"}] &],
        Throw[Failure["UnsupportedAmplitudeExport", <|"Particles" -> particles,
          "Diagnostics" -> {factorText, expressionText}|>], tag]];
      readFactor = parseModelExpression[factorText, scope];
      readExpression = ConvertAmplitude[Join[particles, {factor, expressionText}], model[[1]], scope];
      If[FailureQ[readFactor] || FailureQ[readExpression] ||
          !SameQ[readFactor, factor] || !SameQ[readExpression, expression],
        Throw[Failure["AmplitudeRoundTripFailed", <|"Particles" -> particles,
          "Factor" -> factorText, "Expression" -> expressionText|>], tag]];
      existing = Select[known, Sort[DeleteCases[Drop[#, -2], ""]] === Sort[particles] &];
      If[existing =!= {},
        existingParticles = DeleteCases[Drop[First[existing], -2], ""];
        If[Length[existing] === 1 && existingParticles === particles &&
            modelExportEqualQ[First[existing][[-2]] First[existing][[-1]], factor expression],
          skipped++; Continue[],
          Throw[Failure["DuplicateModelAmplitude", <|"Particles" -> particles,
            "MessageTemplate" -> "The model already contains this particle multiset with a different ordering or expression. Existing amplitudes have not been overwritten."|>], tag]]];
      columns = Max[4, Length[particles]];
      If[columns =!= currentColumns,
        AppendTo[rows, StringRiffle[Join["P" <> ToString[#] & /@ Range[columns],
          {"Factor", "Amplitude/Factor"}], " | "]];
        currentColumns = columns];
      AppendTo[rows, StringRiffle[Join[PadRight[particles, columns, " "],
        {factorText, expressionText}], " | "]];
      AppendTo[known, Join[particles, {factor, expression}]];
      added++,
      {entry, entries}];
    If[added === 0, Throw[<|"File" -> file, "Added" -> 0, "Skipped" -> skipped|>, tag]];
    body = prefix <> StringRiffle[rows, newline] <> newline <> suffix;
    temporary = FileNameJoin[{DirectoryName[file], "." <> FileNameTake[file] <>
      "." <> CreateUUID[] <> ".tmp"}];
    (* Truncating a copy retains the model's permissions during atomic replacement. *)
    result = Quiet[Check[CopyFile[file, temporary], $Failed]];
    If[result === $Failed,
      Throw[Failure["ModelExportUnavailable", <|"File" -> file|>], tag]];
    stream = Quiet[Check[OpenWrite[temporary, CharacterEncoding -> "UTF-8"], $Failed]];
    If[!MatchQ[stream, _OutputStream],
      Throw[Failure["ModelExportUnavailable", <|"File" -> file|>], tag]];
    result = CheckAbort[Quiet[Check[WriteString[stream, body]; Close[stream]; True, $Failed]],
      Quiet[Close[stream]]; If[FileExistsQ[temporary], DeleteFile[temporary]]; Abort[]];
    If[result =!= True || Quiet[Check[BinaryReadList[file, "Byte"], $Failed]] =!= original,
      Throw[Failure["ModelExportChanged", <|"File" -> file,
        "MessageTemplate" -> "The model changed during export or the temporary file could not be written. No update was applied."|>], tag]];
    result = Quiet[Check[RenameFile[temporary, file, OverwriteTarget -> True], $Failed]];
    If[result === $Failed, Throw[Failure["ModelExportUnavailable", <|"File" -> file|>], tag]];
    <|"File" -> file, "Added" -> added, "Skipped" -> skipped|>, tag];
  If[StringQ[temporary] && FileExistsQ[temporary], Quiet[DeleteFile[temporary]]];
  result
];

End[];


(* ::Subsection::Closed:: *)
(*Diagrams (old: to be removed)*)


(* ::Subsubsection::Closed:: *)
(*3-Point Diagrams*)


(* ::Input::Initialization:: *)
ConstructiveDiagram[parts_,model_]:=Module[{totFactor=1,numerator=0,vrtx=False,fparts={False,False,False},p1p=0,p2p=0,p3p=0,pp5,pp6,pp7},
If[Length[parts]=!=3,Print[parts," is not a list of 3 particles.  Quitting."];Return[]];
(*Make sure particles are in model*)
Do[
Do[If[parts[[jj]]===model[[1,ii,2]]||parts[[jj]]===model[[1,ii,3]],fparts[[jj]]=True];,{jj,1,3}];
,{ii,1,Length[model[[1]]]}];
Do[If[!fparts[[jj]],Print["Could not find particle ",parts[[jj]]," in model!"];Return[0]];,{jj,1,3}];


(*Look for vertices connecting particles*)
Do[
p1p=0;
p2p=0;
p3p=0;

Which[
	model[[2,ii,1]]===parts[[1]],p1p=1,
	model[[2,ii,2]]===parts[[1]],p1p=2,
	model[[2,ii,3]]===parts[[1]],p1p=3
];
Which[
	p1p!=0&&p1p!=1&&model[[2,ii,1]]===parts[[2]],p2p=1,
	p1p!=0&&p1p!=2&&model[[2,ii,2]]===parts[[2]],p2p=2,
	p1p!=0&&p1p!=3&&model[[2,ii,3]]===parts[[2]],p2p=3
];
Which[
	p1p p2p!=0&&p1p!=1&&p2p!=1&&model[[2,ii,1]]===parts[[3]],p3p=1,
	p1p p2p!=0&&p1p!=2&&p2p!=2&&model[[2,ii,2]]===parts[[3]],p3p=2,
	p1p p2p!=0&&p1p!=3&&p2p!=3&&model[[2,ii,3]]===parts[[3]],p3p=3
];If[p1p p2p p3p>0,Print[ii,": ",p1p,",",p2p,",",p3p]];
If[
p1p p2p p3p>0,
totFactor=model[[2,ii,-2]];
numerator=model[[2,ii,-1]]//.{
Spinor[a__,p1p]:>Spinor[a,pp5],
Spinor[a__,p2p]:>Spinor[a,pp6],
Spinor[a__,p3p]:>Spinor[a,pp7],
xFactor[a_,p1p]:>xFactor[a,pp5],
xFactor[a_,p2p]:>xFactor[a,pp6],
xFactor[a_,p3p]:>xFactor[a,pp7],
xFactor[p1p,a_]:>xFactor[pp5,a],
xFactor[p2p,a_]:>xFactor[pp6,a],
xFactor[p3p,a_]:>xFactor[pp7,a],
xTildeFactor[a_,p1p]:>xTildeFactor[a,pp5],xTildeFactor[a_,p2p]:>xTildeFactor[a,pp6],xTildeFactor[a_,p3p]:>xTildeFactor[a,pp7],
xTildeFactor[p1p,a_]:>xTildeFactor[pp5,a],xTildeFactor[p2p,a_]:>xTildeFactor[pp6,a],xTildeFactor[p3p,a_]:>xTildeFactor[pp7,a]
}/.{pp5->1,pp6->2,pp7->3};
vrtx=True;
];
If[vrtx,Break[]];
,{ii,1,Length[model[[2]]]}];



If[vrtx,totFactor  numerator,0]
]


(* ::Subsubsection::Closed:: *)
(*4-Point Diagrams*)


(* ::Input::Initialization:: *)
SChannelDiagram[p1_,p2_,p3_,p4_,ps_,model_]:=ConstructiveDiagram[{p1,p2,p3,p4},ps,model,{1,2}]
TChannelDiagram[p1_,p2_,p3_,p4_,ps_,model_]:=ConstructiveDiagram[{p1,p2,p3,p4},ps,model,{1,3}]
UChannelDiagram[p1_,p2_,p3_,p4_,ps_,model_]:=ConstructiveDiagram[{p1,p2,p3,p4},ps,model,{1,4}]


(* ::Input::Initialization:: *)
ConstructiveDiagram[parts_,pstmp_,model_,channel_]:=Module[{particles=parts,totFactor=1,numerator=0,numerator1=1,numerator2=1,numeratorl=0,numeratorr=0,lvrtx=False,rvrtx=False,ps=pstmp,aps="",fparts={False,False,False,False},p1p=0,p2p=0,psp=0,apsp=0,p3p=0,p4p=0,pp5,pp6,pp7,channel2=Complement[{1,2,3,4},channel],mps,spinp=0,i1,i2,i3,i4,indices={},helicitySigns={0,0,0,0},needxTilde=False},
(*Split .+ / .- to record helicity, but keep the model name (A.+, G.+).*)
Do[
particles[[ii]]=StringTrim[StringSplit[parts[[ii]],"."]];
If[Length[particles[[ii]]]>1,
helicitySigns[[ii]]=ToExpression[particles[[ii,2]]<>"1"];
particles[[ii]]=parts[[ii]],
particles[[ii]]=particles[[ii,1]]
];
,{ii,1,4}];
(*Print[particles,":",helicitySigns];*)

(*Make sure particles are in model*)
Do[
If[ps===model[[1,ii,2]],aps=model[[1,ii,3]];spinp=model[[1,ii,5]];];
If[ps===model[[1,ii,3]],ps=model[[1,ii,2]];aps=model[[1,ii,3]];spinp=model[[1,ii,5]];];(*ps is propagating particle, aps is propagating antiparticle*)
Do[If[particles[[jj]]===model[[1,ii,2]]||particles[[jj]]===model[[1,ii,3]],fparts[[jj]]=True];,{jj,1,4}];
,{ii,1,Length[model[[1]]]}];
If[aps==="",Print["Could not find particle ",ps," in model!"];Return[0]];
Do[If[!fparts[[jj]],Print["Could not find particle ",particles[[jj]]," in model!"];Return[0]];,{jj,1,4}];
mps=ParticleMass[ps,model[[1]]];
If[mps===0&&ps===aps,needxTilde=True];
If[spinp>0,AppendTo[indices,i1]];
If[spinp>1,AppendTo[indices,i2]];
If[spinp>2,AppendTo[indices,i3]];
If[spinp>3,AppendTo[indices,i4]];


(*Look for vertices connecting particles*)
Do[
p1p=0;
p2p=0;
If[psp!=-1,psp=0;];
If[apsp!=-1,apsp=0;];
p3p=0;
p4p=0;
If[!lvrtx,
Which[
model[[2,ii,1]]===particles[[channel[[1]]]],p1p=1,
model[[2,ii,2]]===particles[[channel[[1]]]],p1p=2,
model[[2,ii,3]]===particles[[channel[[1]]]],p1p=3
];
Which[
p1p!=0&&p1p!=1&&model[[2,ii,1]]===particles[[channel[[2]]]],p2p=1,
p1p!=0&&p1p!=2&&model[[2,ii,2]]===particles[[channel[[2]]]],p2p=2,
p1p!=0&&p1p!=3&&model[[2,ii,3]]===particles[[channel[[2]]]],p2p=3
];
Which[
psp==0&&p1p p2p!=0&&p1p!=1&&p2p!=1&&model[[2,ii,1]]===ps,psp=1,
psp==0&&p1p p2p!=0&&p1p!=2&&p2p!=2&&model[[2,ii,2]]===ps,psp=2,
psp==0&&p1p p2p!=0&&p1p!=3&&p2p!=3&&model[[2,ii,3]]===ps,psp=3
];
Which[
apsp==0&&p1p p2p!=0&&p1p!=1&&p2p!=1&&model[[2,ii,1]]===aps,apsp=1,
apsp==0&&p1p p2p!=0&&p1p!=2&&p2p!=2&&model[[2,ii,2]]===aps,apsp=2,
apsp==0&&p1p p2p!=0&&p1p!=3&&p2p!=3&&model[[2,ii,3]]===aps,apsp=3
];(*If[p1p p2p psp>0||p1p p2p apsp>0,Print[ii,"L: ",p1p,",",p2p,",",psp,",",apsp]];*)
Which[
p1p p2p psp>0,
totFactor=totFactor model[[2,ii,-2]];
numeratorl=model[[2,ii,-1]]//.{
Spinor[a__,p1p]:>Spinor[a,pp5],
Spinor[a__,p2p]:>Spinor[a,pp6],
Spinor[a__,psp]:>Spinor[a,pp7],
xFactor[a_,p1p]:>xFactor[a,pp5],
xFactor[a_,p2p]:>xFactor[a,pp6],
xFactor[a_,psp]:>xFactor[a,pp7],
xFactor[p1p,a_]:>xFactor[pp5,a],
xFactor[p2p,a_]:>xFactor[pp6,a],
xFactor[psp,a_]:>xFactor[pp7,a]
}/.{pp5->channel[[1]],pp6->channel[[2]],pp7->Multiparticle[channel2[[1]],channel2[[2]]]};
numeratorl=MakeIndicesExplicit[numeratorl,Multiparticle[channel2[[1]],channel2[[2]]],indices];
(*numeratorl=SymmetrizeSpin[numeratorl,Multiparticle[channel2[[1]],channel2[[2]]],indices];*)
psp=-1;
lvrtx=True;,
p1p p2p apsp>0,
totFactor=totFactor model[[2,ii,-2]];
numeratorl=model[[2,ii,-1]]//.{
Spinor[a__,p1p]:>Spinor[a,pp5],
Spinor[a__,p2p]:>Spinor[a,pp6],
Spinor[a__,apsp]:>Spinor[a,pp7],
xFactor[a_,p1p]:>xFactor[a,pp5],
xFactor[a_,p2p]:>xFactor[a,pp6],
xFactor[a_,apsp]:>xFactor[a,pp7],
xFactor[p1p,a_]:>xFactor[pp5,a],
xFactor[p2p,a_]:>xFactor[pp6,a],
xFactor[apsp,a_]:>xFactor[pp7,a]
}/.{pp5->channel[[1]],pp6->channel[[2]],pp7->Multiparticle[channel2[[1]],channel2[[2]]]};
numeratorl=MakeIndicesExplicit[numeratorl,Multiparticle[channel2[[1]],channel2[[2]]],indices]/."Upper"->"Lower";
(*numeratorl=SymmetrizeSpin[numeratorl,Multiparticle[channel2[[1]],channel2[[2]]],indices]/."Upper"->"Lower";*)(*Antiparticle propagator has lower indices*)
apsp=-1;
lvrtx=True;
];
If[lvrtx,
If[needxTilde,
numerator2=numerator2(numeratorl/.{
xFactor[a_,b_]:>xTildeFactor[a,b],
Spinor[a__,"Angle",b__]:>Spinor[a,"Square",b]
});
];
If[helicitySigns[[channel[[1]]]]<0||helicitySigns[[channel[[2]]]]<0,
numeratorl=numeratorl//.{
xFactor[a_,b_]:>xTildeFactor[a,b],
Spinor[a__,"Angle",b__]:>Spinor[a,"Square",b]
};
];
numerator1=numerator1 numeratorl;
];
];
If[!rvrtx,
Which[
model[[2,ii,1]]===particles[[channel2[[1]]]],p3p=1,
model[[2,ii,2]]===particles[[channel2[[1]]]],p3p=2,
model[[2,ii,3]]===particles[[channel2[[1]]]],p3p=3
];
Which[
p3p!=0&&p3p!=1&&model[[2,ii,1]]===particles[[channel2[[2]]]],p4p=1,
p3p!=0&&p3p!=2&&model[[2,ii,2]]===particles[[channel2[[2]]]],p4p=2,
p3p!=0&&p3p!=3&&model[[2,ii,3]]===particles[[channel2[[2]]]],p4p=3
];
Which[
psp==0&&p3p p4p!=0&&p3p!=1&&p4p!=1&&model[[2,ii,1]]===ps,psp=1,
psp==0&&p3p p4p!=0&&p3p!=2&&p4p!=2&&model[[2,ii,2]]===ps,psp=2,
psp==0&&p3p p4p!=0&&p3p!=3&&p4p!=3&&model[[2,ii,3]]===ps,psp=3
];
Which[
apsp==0&&p3p p4p!=0&&p3p!=1&&p4p!=1&&model[[2,ii,1]]===aps,apsp=1,
apsp==0&&p3p p4p!=0&&p3p!=2&&p4p!=2&&model[[2,ii,2]]===aps,apsp=2,
apsp==0&&p3p p4p!=0&&p3p!=3&&p4p!=3&&model[[2,ii,3]]===aps,apsp=3
];(*If[p3p p4p psp>0||p3p p4p apsp>0,Print[ii,"R: ",psp,",",apsp,",",p3p,",",p4p]];*)
Which[
p3p p4p psp>0,
totFactor=totFactor model[[2,ii,-2]];
numeratorr=model[[2,ii,-1]]//.{
Spinor[a__,p3p]:>Spinor[a,pp5],Spinor[a__,p4p]:>Spinor[a,pp6],Spinor[a__,psp]:>Spinor[a,pp7],
xFactor[a_,p3p]:>xFactor[a,pp5],xFactor[a_,p4p]:>xFactor[a,pp6],xFactor[a_,psp]:>xFactor[a,pp7],
xFactor[p3p,a_]:>xFactor[pp5,a],xFactor[p4p,a_]:>xFactor[pp6,a],xFactor[psp,a_]:>xFactor[pp7,a]
}/.{pp5->channel2[[1]],pp6->channel2[[2]],pp7->Multiparticle[channel[[1]],channel[[2]]]};
numeratorr=SymmetrizeSpin[numeratorr,Multiparticle[channel[[1]],channel[[2]]],indices];
psp=-1;
rvrtx=True;,
p3p p4p apsp>0,
totFactor=totFactor model[[2,ii,-2]];
numeratorr=model[[2,ii,-1]]//.{
Spinor[a__,p3p]:>Spinor[a,pp5],Spinor[a__,p4p]:>Spinor[a,pp6],Spinor[a__,apsp]:>Spinor[a,pp7],
xFactor[a_,p3p]:>xFactor[a,pp5],xFactor[a_,p4p]:>xFactor[a,pp6],xFactor[a_,apsp]:>xFactor[a,pp7],
xFactor[p3p,a_]:>xFactor[pp5,a],xFactor[p4p,a_]:>xFactor[pp6,a],xFactor[apsp,a_]:>xFactor[pp7,a]
}/.{pp5->channel2[[1]],pp6->channel2[[2]],pp7->Multiparticle[channel[[1]],channel[[2]]]};
numeratorr=SymmetrizeSpin[numeratorr,Multiparticle[channel[[1]],channel[[2]]],indices]/."Upper"->"Lower";(*Antiparticle propagator has lower indices*)
apsp=-1;
rvrtx=True;
];
If[rvrtx,
If[helicitySigns[[channel2[[1]]]]<0||helicitySigns[[channel2[[2]]]]<0,
numeratorr=numeratorr//.{
xFactor[a_,b_]:>xTildeFactor[a,b],
Spinor[a__,"Angle",b__]:>Spinor[a,"Square",b]
};
];
If[needxTilde,
numerator1=numerator1(numeratorr/.{
xFactor[a_,b_]:>xTildeFactor[a,b],
Spinor[a__,"Angle",b__]:>Spinor[a,"Square",b]
});
numerator2=numerator2 numeratorr;,
numerator1=numerator1 numeratorr;,
numerator1=numerator1 numeratorr;
];
];
];
If[lvrtx&&rvrtx,Break[]];
,{ii,1,Length[model[[2]]]}];

If[needxTilde,numerator=numerator1+numerator2;,numerator=numerator1;];
If[lvrtx&&rvrtx,numerator=numerator(*SymmetrizeSpin[numerator,Multiparticle[channel[[1]],channel[[2]]],indices]*)//.{
Spinor[a__,"Angle",Multiparticle[channel2[[1]],channel2[[2]]],b___]:>-Spinor[a,"Angle",Multiparticle[channel[[1]],channel[[2]]],b],
Spinor[a__,"Square",Multiparticle[channel2[[1]],channel2[[2]]],b___]:>Spinor[a,"Square",Multiparticle[channel[[1]],channel[[2]]],b]
}/.{
SpinorChain[a___,-Spinor[b__],c___]:>-SpinorChain[a,Spinor[b],c]
};
];

(*The - sign is due to the 3 factors of I in the vertices and propagator with one removed for the amplitude.*)
If[lvrtx&&rvrtx,-totFactor/PropDen[Mom[channel[[1]]]+Mom[channel[[2]]],mps] numerator,0]
]


(* ::Subsection::Closed:: *)
(*Factorization Channels*)


(* ::Subsubsection::Closed:: *)
(*FactorizationPartitions[particles_List]*)


(* ::Text:: *)
(*This function partitions the particle list into pairs of groups where each group has at least two particles and all duplicates are removed. *)


(* ::Input::Initialization:: *)
FactorizationPartitions[particles_List]:=Module[{partitions,taggedParticles,n=Length[particles]},
(*First create a list of all partitions.*)
taggedParticles=Thread[{particles,Range[Length[particles]]}];
partitions=Select[
    Subsets[taggedParticles,{2,Floor[n/2]}],
    2<=Length[#]<=n-2 &
  ];
 partitions= DeleteDuplicatesBy[
    Sort /@ {#,Complement[taggedParticles,#]} & /@ partitions,
    Sort
  ];
partitions
]


(* ::Text:: *)
(*Tests:*)


(* ::Input:: *)
(*(*FactorizationPartitions[{"e","E","M","m"}]==={{{{"e",1},{"E",2}},{{"m",4},{"M",3}}},{{{"e",1},{"M",3}},{{"E",2},{"m",4}}},{{{"e",1},{"m",4}},{{"E",2},{"M",3}}}}*)*)


(* ::Input:: *)
(*(*FactorizationPartitions[{"e","E","M","m","A.+"}]==={{{{"e",1},{"E",2}},{{"A.+",5},{"m",4},{"M",3}}},{{{"e",1},{"M",3}},{{"A.+",5},{"E",2},{"m",4}}},{{{"e",1},{"m",4}},{{"A.+",5},{"E",2},{"M",3}}},{{{"A.+",5},{"e",1}},{{"E",2},{"m",4},{"M",3}}},{{{"E",2},{"M",3}},{{"A.+",5},{"e",1},{"m",4}}},{{{"E",2},{"m",4}},{{"A.+",5},{"e",1},{"M",3}}},{{{"A.+",5},{"E",2}},{{"e",1},{"m",4},{"M",3}}},{{{"m",4},{"M",3}},{{"A.+",5},{"e",1},{"E",2}}},{{{"A.+",5},{"M",3}},{{"e",1},{"E",2},{"m",4}}},{{{"A.+",5},{"m",4}},{{"e",1},{"E",2},{"M",3}}}}*)*)


(* ::Input:: *)
(*(*FactorizationPartitions[{"e","E","M","m","A.+","A.-"}]==={{{{"e",1},{"E",2}},{{"A.-",6},{"A.+",5},{"m",4},{"M",3}}},{{{"e",1},{"M",3}},{{"A.-",6},{"A.+",5},{"E",2},{"m",4}}},{{{"e",1},{"m",4}},{{"A.-",6},{"A.+",5},{"E",2},{"M",3}}},{{{"A.+",5},{"e",1}},{{"A.-",6},{"E",2},{"m",4},{"M",3}}},{{{"A.-",6},{"e",1}},{{"A.+",5},{"E",2},{"m",4},{"M",3}}},{{{"E",2},{"M",3}},{{"A.-",6},{"A.+",5},{"e",1},{"m",4}}},{{{"E",2},{"m",4}},{{"A.-",6},{"A.+",5},{"e",1},{"M",3}}},{{{"A.+",5},{"E",2}},{{"A.-",6},{"e",1},{"m",4},{"M",3}}},{{{"A.-",6},{"E",2}},{{"A.+",5},{"e",1},{"m",4},{"M",3}}},{{{"m",4},{"M",3}},{{"A.-",6},{"A.+",5},{"e",1},{"E",2}}},{{{"A.+",5},{"M",3}},{{"A.-",6},{"e",1},{"E",2},{"m",4}}},{{{"A.-",6},{"M",3}},{{"A.+",5},{"e",1},{"E",2},{"m",4}}},{{{"A.+",5},{"m",4}},{{"A.-",6},{"e",1},{"E",2},{"M",3}}},{{{"A.-",6},{"m",4}},{{"A.+",5},{"e",1},{"E",2},{"M",3}}},{{{"A.-",6},{"A.+",5}},{{"e",1},{"E",2},{"m",4},{"M",3}}},{{{"e",1},{"E",2},{"M",3}},{{"A.-",6},{"A.+",5},{"m",4}}},{{{"e",1},{"E",2},{"m",4}},{{"A.-",6},{"A.+",5},{"M",3}}},{{{"A.+",5},{"e",1},{"E",2}},{{"A.-",6},{"m",4},{"M",3}}},{{{"A.-",6},{"e",1},{"E",2}},{{"A.+",5},{"m",4},{"M",3}}},{{{"e",1},{"m",4},{"M",3}},{{"A.-",6},{"A.+",5},{"E",2}}},{{{"A.+",5},{"e",1},{"M",3}},{{"A.-",6},{"E",2},{"m",4}}},{{{"A.-",6},{"e",1},{"M",3}},{{"A.+",5},{"E",2},{"m",4}}},{{{"A.+",5},{"e",1},{"m",4}},{{"A.-",6},{"E",2},{"M",3}}},{{{"A.-",6},{"e",1},{"m",4}},{{"A.+",5},{"E",2},{"M",3}}},{{{"A.-",6},{"A.+",5},{"e",1}},{{"E",2},{"m",4},{"M",3}}}}*)*)


(* ::Text:: *)
(*Right now, it treats each same particle differently since it has a different momentum.  It might be better to not duplicate the same particle.  I'm not sure if both factorization channels need to be included or not.*)


(* ::Input:: *)
(*(*FactorizationPartitions[{"e","E","M","m","A.+","A.+"}]==={{{{"e",1},{"E",2}},{{"A.+",5},{"A.+",6},{"m",4},{"M",3}}},{{{"e",1},{"M",3}},{{"A.+",5},{"A.+",6},{"E",2},{"m",4}}},{{{"e",1},{"m",4}},{{"A.+",5},{"A.+",6},{"E",2},{"M",3}}},{{{"A.+",5},{"e",1}},{{"A.+",6},{"E",2},{"m",4},{"M",3}}},{{{"A.+",6},{"e",1}},{{"A.+",5},{"E",2},{"m",4},{"M",3}}},{{{"E",2},{"M",3}},{{"A.+",5},{"A.+",6},{"e",1},{"m",4}}},{{{"E",2},{"m",4}},{{"A.+",5},{"A.+",6},{"e",1},{"M",3}}},{{{"A.+",5},{"E",2}},{{"A.+",6},{"e",1},{"m",4},{"M",3}}},{{{"A.+",6},{"E",2}},{{"A.+",5},{"e",1},{"m",4},{"M",3}}},{{{"m",4},{"M",3}},{{"A.+",5},{"A.+",6},{"e",1},{"E",2}}},{{{"A.+",5},{"M",3}},{{"A.+",6},{"e",1},{"E",2},{"m",4}}},{{{"A.+",6},{"M",3}},{{"A.+",5},{"e",1},{"E",2},{"m",4}}},{{{"A.+",5},{"m",4}},{{"A.+",6},{"e",1},{"E",2},{"M",3}}},{{{"A.+",6},{"m",4}},{{"A.+",5},{"e",1},{"E",2},{"M",3}}},{{{"A.+",5},{"A.+",6}},{{"e",1},{"E",2},{"m",4},{"M",3}}},{{{"e",1},{"E",2},{"M",3}},{{"A.+",5},{"A.+",6},{"m",4}}},{{{"e",1},{"E",2},{"m",4}},{{"A.+",5},{"A.+",6},{"M",3}}},{{{"A.+",5},{"e",1},{"E",2}},{{"A.+",6},{"m",4},{"M",3}}},{{{"A.+",6},{"e",1},{"E",2}},{{"A.+",5},{"m",4},{"M",3}}},{{{"e",1},{"m",4},{"M",3}},{{"A.+",5},{"A.+",6},{"E",2}}},{{{"A.+",5},{"e",1},{"M",3}},{{"A.+",6},{"E",2},{"m",4}}},{{{"A.+",6},{"e",1},{"M",3}},{{"A.+",5},{"E",2},{"m",4}}},{{{"A.+",5},{"e",1},{"m",4}},{{"A.+",6},{"E",2},{"M",3}}},{{{"A.+",6},{"e",1},{"m",4}},{{"A.+",5},{"E",2},{"M",3}}},{{{"A.+",5},{"A.+",6},{"e",1}},{{"E",2},{"m",4},{"M",3}}}}*)*)


(* ::Subsubsection::Closed:: *)
(*LookupAmplitude[taggedParticles_List,model_,rightSide_Boolean]*)


(* ::Input::Initialization:: *)
ConstructiveDiagrams`Private`amplitudeParticleLabelRules[tagged_List,row_List]:=Module[
  {used={},rules={},particles=Drop[row,-2]},
  Do[Do[
    If[tagged[[k,1]]===particles[[j]]&&!MemberQ[used,k],
      AppendTo[rules,j->tagged[[k,2]]];AppendTo[used,k];Break[]],
    {k,Length[tagged]}],{j,Length[particles]}];
  rules
];

LookupAmplitude[taggedParticles_List,model_,rightSide_,{i1_,i2_,i3_,i4_}]:=Module[{amp=0,i,j,k,replacements,doubleSpin,indices,buildReplacements,relabelIndex,relabelExpression},
(*Helper function*)
relabelIndex[index_Integer]:=index/.replacements;
relabelIndex[Multiparticle[labels___]]:=Multiparticle@@Flatten[
  (relabelIndex/@{labels})/.Multiparticle->List];
relabelIndex[index_]:=index;
(* Only particle-label positions are changed, not coefficients, powers or spin indices. *)
relabelExpression[expression_]:=expression/.{
  head:(Spinor|SpinorHat)[type:Except["Xi"],position:("Upper"|"Lower"),kind:("Angle"|"Square"),label_,spin_]:>
    Head[head][type,position,kind,relabelIndex[label],spin],
  head:(Spinor|SpinorHat)[type:Except["Xi"],kind:("Angle"|"Square"),label_,rest___]:>
    Head[head][type,kind,relabelIndex[label],rest],
  Mom[label_]:>Mom[relabelIndex[label]],
  MomHat[label_]:>MomHat[relabelIndex[label]],
  MomQ[label_]:>MomQ[relabelIndex[label]],
  Mass[label_]:>Mass[relabelIndex[label]],
  MomProd[a_,b_]:>MomProd[relabelIndex[a],relabelIndex[b]],
  Mandelstahm[a_,b_]:>Mandelstahm[relabelIndex[a],relabelIndex[b]],
  xFactor[labels___]:>xFactor@@(relabelIndex/@{labels}),
  xTildeFactor[labels___]:>xTildeFactor@@(relabelIndex/@{labels})
};
buildReplacements[i_]:=ConstructiveDiagrams`Private`amplitudeParticleLabelRules[
  taggedParticles,model[[2,i]]];
(*End helper function*)

(*Find the amp*)(*Print[taggedParticles];*)
Do[
If[Sort[DeleteCases[DeleteCases[Flatten[taggedParticles],_Integer],Multiparticle[__]]]===Sort[DeleteCases[Drop[model[[2,i]],-2],""]],
(*Print[model[[2,i]]];*)
(*Old, remove once satisfied.
replacements={};
Do[
If[taggedParticles[[k,1]]===model[[2,i,j]],AppendTo[replacements,j->taggedParticles[[k,2]]]];
,{k,1,Length[taggedParticles]},{j,1,Length[model[[2,i]]]-2}];*)
replacements=buildReplacements[i];
(*Print[replacements];*)
amp=relabelExpression[model[[2,i,-2]]model[[2,i,-1]]];
];
(*Print[Sort[DeleteCases[Flatten[taggedParticles],_Integer]]];
Print[Sort[DeleteCases[Drop[model[[2,i]],-2],""]]];*)
,{i,1,Length[model[[2]]]}];
(*Determine if the last particle (with a multiparticle momentum) is a particle or antiparticle and make the indices explicit appropriately.*)
Do[
If[MatchQ[taggedParticles[[-1,2]],Multiparticle[__]]&&(taggedParticles[[-1,1]]===model[[1,i,2]]||taggedParticles[[-1,1]]===model[[1,i,3]]),
doubleSpin=model[[1,i,5]];
indices={};
If[doubleSpin>0,AppendTo[indices,i1]];
If[doubleSpin>1,AppendTo[indices,i2]];
If[doubleSpin>2,AppendTo[indices,i3]];
If[doubleSpin>3,AppendTo[indices,i4]];
If[taggedParticles[[-1,1]]===model[[1,i,2]],amp=SymmetrizeSpin[amp,taggedParticles[[-1,2]],indices]];
If[taggedParticles[[-1,1]]===model[[1,i,3]]&&model[[1,i,2]]=!=model[[1,i,3]],amp=MakeIndicesExplicit[amp,taggedParticles[[-1,2]],indices]/."Upper"->"Lower"];
If[rightSide&&model[[1,i,2]]==model[[1,i,3]],amp=amp/."Upper"->"Lower"];
];
,{i,1,Length[model[[1]]]}];
amp
]


(* ::Input:: *)
(*(*LookupAmplitude[{{"E",1},{"A.+",2},{"e",Multiparticle[3,4]}},sm,True,{i1,i2,i3,i4}]*)*)


(* ::Input:: *)
(*(*LookupAmplitude[{{"A.-",3},{"e",4},{"E",Multiparticle[1,2]}},sm,False,{i1,i2,i3,i4}]*)*)


(* ::Subsubsection::Closed:: *)
(*ExpandXFactors[expr,n]*)


(* ::Input::Initialization:: *)
expandExternalXFactors[expr_,n_]:=Module[{particleIndices,missingXReplacement,missingXTildeReplacement,XiN=10},
particleIndices[term_]:=Which[
  IntegerQ[term],{term},
  MatchQ[term,Multiparticle[__]],List @@ term,
  True,{}
];
missingXReplacement[l_,j_]:=
  SpinorChain[Spinor["Xi","Angle",XiN],Mom[j],Spinor["Helicity","Square",l]]/Mass[j]/SpinorChain[ Spinor["Xi","Angle",XiN++],Spinor["Helicity","Angle",l]];
missingXTildeReplacement[l_,j_]:=  SpinorChain[Spinor["Xi","Square",XiN],Mom[j],Spinor["Helicity","Angle",l]]/Mass[j]/SpinorChain[ Spinor["Xi","Square",XiN++],Spinor["Helicity","Square",l]];
(*missingXReplacement[l_,j_]:=
  SpinorChain[Spinor["Helicity","Square",l],Mom[XiN],Mom[j],Spinor["Helicity","Square",l]]/Mass[j]/PropDen[Mom[Multiparticle[l,XiN]],Mass[XiN++]];
missingXTildeReplacement[l_,j_]:=  SpinorChain[Spinor["Helicity","Angle",l],Mom[XiN],Mom[j],Spinor["Helicity","Angle",l]]/Mass[j]/PropDen[Mom[Multiparticle[l,XiN]],Mass[XiN++]];*)

expr/.{
xFactor[i_,j_]:>Module[
  {allIndices,missingIndex},
  allIndices=Union@Join[particleIndices[i],particleIndices[j]];
  missingIndex=Complement[Range[n],allIndices];
  If[Length[missingIndex]===1,
    missingXReplacement[First[missingIndex],j],
    xFactor[i,j]
  ]
],
xTildeFactor[i_,j_]:>Module[
  {allIndices,missingIndex},
  allIndices=Union@Join[particleIndices[i],particleIndices[j]];
  missingIndex=Complement[Range[n],allIndices];
  If[Length[missingIndex]===1,
    missingXTildeReplacement[First[missingIndex],j],
    xTildeFactor[i,j]
  ]
]
}
];

ExpandXFactors[expr_,n_]:=Module[{newExp},
newExp=expr/.{xFactor[i_,j_]xTildeFactor[k_,l_]SpinorChain[Spinor["Spin","Angle",i_],Spinor["Spin","Angle",j_]]SpinorChain[Spinor["Spin","Square",k_],Spinor["Spin","Square",l_]]+xFactor[k_,l_]xTildeFactor[i_,j_]SpinorChain[Spinor["Spin","Angle",k_],Spinor["Spin","Angle",l_]]SpinorChain[Spinor["Spin","Square",i_],Spinor["Spin","Square",j_]]:>SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",k]]SpinorChain[Spinor["Spin","Square",j],Spinor["Spin","Square",l]]+SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",l]]SpinorChain[Spinor["Spin","Square",j],Spinor["Spin","Square",k]]+SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",k]]SpinorChain[Spinor["Spin","Angle",j],Spinor["Spin","Angle",l]]+SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",l]]SpinorChain[Spinor["Spin","Angle",j],Spinor["Spin","Angle",k]],
xFactor[i_,j_]xTildeFactor[k_,l_]SpinorChain[Spinor["Spin","Angle",i_],Spinor["Spin","Angle",j_]]^2SpinorChain[Spinor["Spin","Square",k_],Spinor["Spin","Square",l_]]:>-(SpinorChain[Spinor["Spin","Angle",j],Spinor["Spin","Angle",i]]+SpinorChain[Spinor["Spin","Square",j],Spinor["Spin","Square",i]])(SpinorChain[Spinor["Spin","Angle",k],Spinor["Spin","Angle",j]]SpinorChain[Spinor["Spin","Square",l],Spinor["Spin","Square",i]]+SpinorChain[Spinor["Spin","Angle",k],Spinor["Spin","Angle",i]]SpinorChain[Spinor["Spin","Square",l],Spinor["Spin","Square",j]]+SpinorChain[Spinor["Spin","Angle",l],Spinor["Spin","Angle",j]]SpinorChain[Spinor["Spin","Square",k],Spinor["Spin","Square",i]]+SpinorChain[Spinor["Spin","Angle",l],Spinor["Spin","Angle",i]]SpinorChain[Spinor["Spin","Square",k],Spinor["Spin","Square",j]])-1/Mass[3] SpinorChain[Spinor["Spin","Angle",j],Spinor["Spin","Angle",i]]SpinorChain[Spinor["Spin","Square",j],Spinor["Spin","Square",i]](SpinorChain[Spinor["Spin","Square",k],Mom[j],Spinor["Spin","Angle",l]]+SpinorChain[Spinor["Spin","Square",l],Mom[j],Spinor["Spin","Angle",k]])-xFactor[k,l]xTildeFactor[i,j]SpinorChain[Spinor["Spin","Angle",k],Spinor["Spin","Angle",l]]SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",j]]^2
};
expandExternalXFactors[newExp,n]
];


(* ::Subsubsection::Closed:: *)
(*FactorizationAmplitudeChannels[particles_List,model_]*)


(* ::Text:: *)
(*This function uses FactorizationPartitions to create the partitions and then calls LookupAmplitude to see if each side is nonzero.  If they are, it sews them together.*)


(* ::Input::Initialization:: *)
DownValues[FactorizationAmplitudeChannels] = {};
Options[FactorizationAmplitudeChannels] = {};
FactorizationAmplitudeChannels::usage = "FactorizationAmplitudeChannels[particles, model] returns one row for each nonzero factorization channel: the external masses, channel momentum combination, exchanged mass, and sewn product of lower-point amplitudes divided by z and the propagator denominator. Amplitudes absent from the supplied model are zero.";
FactorizationAmplitudeChannels[particles_List,model_]:=Module[{partitions,j,i,k,channel,momenta1,momenta2,amp1,amp2,amp12A,amp12B,amplitude={},masses={},xFactorReverseProtect,xFactorReverseProtectInverse,Multipart,XiN=1,massReplacements={}},
If[TrueQ[ConstructiveDiagrams`Private`feynmanModelQ[model]],
Return[Failure["FeynmanVerticesNotFactorizationAmplitudes",<|"MessageTemplate"->
"Factorization requires known on-shell spinor amplitudes, not bare Feynman vertices. Convert and validate the lower-point amplitudes first."|>]]];
(*Create replacement lists that protect the xFactor*)
xFactorReverseProtect={xFactor[a___,Multiparticle[b__],c___]:>xFactor[a,Multipart[b],c],xTildeFactor[a___,Multiparticle[b__],c___]:>xTildeFactor[a,Multipart[b],c]};
xFactorReverseProtectInverse={xFactor[a___,Multipart[b__],c___]:>xFactor[a,Multiparticle[b],c],xTildeFactor[a___,Multipart[b__],c___]:>xTildeFactor[a,Multiparticle[b],c]};
(*Get a list of the external masses.*)
Do[
If[particles[[i]]===model[[1,j,2]]||particles[[i]]===model[[1,j,3]],AppendTo[masses,model[[1,j,6]]]];
,{i,1,Length[particles]},{j,1,Length[model[[1]]]}];
(*Create the factorization channels*)
partitions=FactorizationPartitions[particles];
Do[
channel=partitions[[i]];
momenta1=Multiparticle[];Do[AppendTo[momenta1,channel[[2,k,2]]],{k,1,Length[channel[[2]]]}];momenta1=Sort[momenta1];
momenta2=Multiparticle[];Do[AppendTo[momenta2,channel[[1,k,2]]],{k,1,Length[channel[[1]]]}];momenta2=Sort[momenta2];
amp12A=0;
channel[[1]]=Append[channel[[1]],{model[[1,j,2]],momenta1}];
channel[[2]]=Append[channel[[2]],{model[[1,j,3]],momenta2}];
(*Print[channel];*)
amp1=ReverseMomentum[LookupAmplitude[channel[[1]],model,False,{i1,i2,i3,i4}],momenta1]//.xFactorReverseProtect/.momenta1->momenta2//.xFactorReverseProtectInverse;
If[amp1=!=0,amp2=LookupAmplitude[channel[[2]],model,True,{i1,i2,i3,i4}]];
If[amp1=!=0&&amp2=!=0,(*Print[amp1];Print[amp2];*)
amp12A=ConstructiveDiagrams`Private`factorizationExternalFermionSign[channel,model] ReduceSpinContractions[amp1 amp2]/.{
Mass[momenta1]->model[[1,j,6]],
SpinorChain[a__,Mom[Multiparticle[parts__]],b__]:>SpinorChain[a,Total[Mom/@{parts}],b]
};];(*Print[amp12A];*)
(*Now switch sides for particle and antiparticle, if they are different, and combine*)
amp12B=0;
If[model[[1,j,2]]=!=model[[1,j,3]],
channel=partitions[[i]];
channel[[1]]=Append[channel[[1]],{model[[1,j,3]],momenta1}];
channel[[2]]=Append[channel[[2]],{model[[1,j,2]],momenta2}];
(*Print[channel];*)
amp1=ReverseMomentum[LookupAmplitude[channel[[1]],model,False,{i1,i2,i3,i4}],momenta1]//.xFactorReverseProtect/.momenta1->momenta2//.xFactorReverseProtectInverse;
If[amp1=!=0,amp2=LookupAmplitude[channel[[2]],model,True,{i1,i2,i3,i4}]];
If[amp1=!=0&&amp2=!=0,(*Print[amp1];Print[amp2];*)
amp12B=ConstructiveDiagrams`Private`factorizationExternalFermionSign[channel,model] ReduceSpinContractions[amp1 amp2]/.{
Mass[momenta1]->model[[1,j,6]],
SpinorChain[a__,Mom[Multiparticle[parts__]],b__]:>SpinorChain[a,Total[Mom/@{parts}],b]
};];(*Print[amp12B];*)
];
(*Combine*)
(*I think it will be better to rewrite models to have separate lines for internal photons where there are 4-point vertices and no x factors and external photons.  Using the x factors (or their replacements) just create unecessary complications when they always simplify to the same thing based on the spin of the particles on the ends.*)
If[amp12A=!=0||amp12B=!=0,
massReplacements={};
Do[AppendTo[massReplacements,Mass[j]->masses[[j]]],{j,1,Length[masses]}];
AppendTo[massReplacements,Mass[momenta2]->model[[1,j,6]]];
AppendTo[massReplacements,Mass[momenta1]->model[[1,j,6]]];
(*Print[massReplacements];*)
AppendTo[amplitude,Flatten[{masses,momenta2,model[[1,j,6]],ExpandXFactors[Simplify[(amp12A+amp12B)/z/PropDen[Mom[momenta2],model[[1,j,6]]]],Length[particles]]}]/.massReplacements];
];
(*Print[Expand[amplitude[[-1]]]]];*)
,{j,1,Length[model[[1]]]},{i,1,Length[partitions]}];
amplitude/.massReplacements
]


Begin["`Private`"];

canonicalPhysicalCut[legs_List, externalLegs_List] := Module[{cut, complement},
  If[!AllTrue[legs, MemberQ[externalLegs, #] &], Return[$Failed]];
  cut = Sort[DeleteDuplicates[legs]];
  complement = Complement[externalLegs, cut];
  First[SortBy[{cut, complement}, {Length[#], #} &]]
];

physicalAmplitudeParticles[row_List] := DeleteCases[Drop[row, -2], ""];

physicalParticleMass[particle_, particleRows_List] := Module[{masses},
  masses = DeleteDuplicates[Cases[particleRows,
    row_List /; MemberQ[row[[{2, 3}]], particle] :> row[[6]]]];
  If[Length[masses] === 1, First[masses], Missing["UnknownParticleMass", particle]]
];

(* Keep external exchange parity and the Wick sign from moving the two
   internal fermion ports adjacent before contracting the model-row words. *)
factorizationExternalFermionSign[channel_List, model_List] := Module[
  {species, words, orders, rows, row, rules, internalPositions, contractionSign},
  species = DeleteDuplicates[Flatten[Cases[model[[1]],
    item_List /; item[[5]] === 1 :> item[[{2, 3}]]]]];
  words = Map[Function[tagged,
    rows = Select[model[[2]],
      Sort[physicalAmplitudeParticles[#]] === Sort[tagged[[All, 1]]] &];
    If[rows === {}, Return[{}]];
    row = Last[rows];
    rules = amplitudeParticleLabelRules[tagged, row];
    Select[MapIndexed[{#1, First[#2] /. rules} &, Drop[row, -2]],
      MemberQ[species, First[#]] &]
  ], channel];
  words = Flatten[words, 1];
  internalPositions = Flatten[Position[words, {_, _Multiparticle}, {1}]];
  contractionSign = If[Length[internalPositions] === 2,
    (-1)^(internalPositions[[2]] - internalPositions[[1]] - 1), 1];
  orders = Table[Cases[words, {particle, label_Integer} :> label], {particle, species}];
  contractionSign Times @@ (Signature[Ordering[#]] & /@ orders)
];


End[];


(* ::Subsubsection::Closed:: *)
(*SimplifyFactorizationChannels[amp_]*)


(* ::Text:: *)
(*This applies SimplifySpinorProducts to each channel*)


(* ::Input::Initialization:: *)
SimplifyFactorizationChannels[amp_List]:=Module[{newAmp=amp,massReplacements={}},
Do[
massReplacements=Join[
  Thread[Mass /@ Range[Length[newAmp[[i]]]-3]->Take[newAmp[[i]],Length[newAmp[[i]]]-3]],
  {Mass[newAmp[[i,-3]]]->newAmp[[i,-2]]}
];
newAmp[[i,-1]]=SimplifySpinorProducts[newAmp[[i,-1]],Momenta->Range[Length[newAmp[[i]]]-3],InternalLines->newAmp[[i,-3]]/.Multiparticle[parts__]:>{parts},InternalLineOnShell->True,massReplacementRules->massReplacements]
,{i,1,Length[amp]}];
newAmp
]


(* ::Subsubsection::Closed:: *)
(*ExpandMomHatAlt[amp_]*)


(* ::Input::Initialization:: *)
ExpandMomHatAlt[amp_]:=Module[{newAmp},
amp//.{
PropDen[MomHat[Multiparticle[a__]],m_]:>PropDen[Mom[Multiparticle[a]],m](z-zPlus[Multiparticle[a]])(z-zMinus[Multiparticle[a]])/zPlus[Multiparticle[a]]/zMinus[Multiparticle[a]],
SpinorChain[a__,MomHat[b_],c__]:>SpinorChain[a,Mom[b],c]+z SpinorChain[a,MomQ[b],c]
}
]


(* ::Subsubsection::Closed:: *)
(*SumResiduesAlt[amp_]*)


(* ::Input::Initialization:: *)
SumResiduesAlt[amp_]:=Module[{newAmp=0},
Do[
(* Cancel removable factors before evaluating either simple pole. *)
newAmp+=Cancel[Together[(z-zPlus[amp[[j,-3]]])amp[[j,-1]]]]/.{z->zPlus[amp[[j,-3]]]};
newAmp+=Cancel[Together[(z-zMinus[amp[[j,-3]]])amp[[j,-1]]]]/.{z->zMinus[amp[[j,-3]]]};
,{j,1,Length[amp]}];
-newAmp
]


(* ::Subsubsection::Closed:: *)
(*UnHatSpinors[amp_]*)


(* ::Input::Initialization:: *)
UnHatSpinors[amp_]:=amp/.SpinorHat[a__]:>Spinor[a]



(* ::Subsection::Closed:: *)
(*Tree-Level Diagrams*)


(* ::Text:: *)
(*Tree diagrams are built by recursively combining currents through model vertices. External legs are labeled from the start, so there is no n! leaf permutation, no enumeration of unlabeled Prufer trees, and no scan over the full particle list for every internal line.*)
(*Feynman rules, propagators, and spin indices are attached afterwards. Tree uniqueness is decided by the bipartition of external legs on each internal line, so graph isomorphism is not needed.*)
(*Public API: CreateDiagrams, CreateArrowDiagram, CreateArrowDiagrams, diagramAmplitude, diagramAmplitudes.*)


(* ::Subsubsection::Closed:: *)
(*buildModelData[model_]*)


(* ::Input::Initialization:: *)
(*
  Constructive tree-level diagram generator.

  Tree diagrams are built by recursively combining currents through
  model vertices.  External legs are
  labeled from the start, so there is no n! leaf permutation, no
  enumeration of unlabeled Prufer trees, and no scan over the full
  SM particle list for every internal line.
*)

(* Model lookup tables.  Built locally from the model argument of
   each CreateDiagrams call, so multiple models (e.g. sm and qed) can
   be used in the same notebook without shared state. *)

buildModelData[model_] := Module[
  {names, nameQ, anti, mass, spin2, rank, vertices, bySorted, combineLookup,
   helicities, internalSpecies, currentVertices, currentBySorted},
  names = DeleteDuplicates @ Flatten[model[[1, All, {2, 3}]]];
  nameQ = AssociationThread[names, True];
  anti = Association @ Flatten[
    {#[[2]] -> #[[3]], #[[3]] -> #[[2]]} & /@ model[[1]],
    1
  ];
  mass = Association @ Flatten[
    {#[[2]] -> #[[6]], #[[3]] -> #[[6]]} & /@ model[[1]],
    1
  ];
  spin2 = Association @ Flatten[
    {#[[2]] -> #[[5]], #[[3]] -> #[[5]]} & /@ model[[1]],
    1
  ];
  helicities = Association @ Select[
    {"A" -> {"A.+", "A.-"}, "G" -> {"G.+", "G.-"}},
    Function[pair, With[{ps = Last[pair]},
      AllTrue[ps, KeyExistsQ[nameQ, #] && Lookup[mass, #] === 0 &&
          Lookup[spin2, #] === 2 &] && Lookup[anti, ps[[1]]] === ps[[2]] &&
        Lookup[anti, ps[[2]]] === ps[[1]] && !KeyExistsQ[nameQ, First[pair]]]]
  ];
  internalSpecies = Association @ Flatten @ KeyValueMap[
    Function[{p, hs}, (# -> p & /@ hs)], helicities];
  KeyValueMap[Function[{p, hs},
    AppendTo[names, p]; anti[p] = p; mass[p] = 0; spin2[p] = 2], helicities];
  rank = AssociationThread[names, Range @ Length[names]];
  vertices = DeleteCases[
    Map[
      Function[vrow,
        Module[{ps},
          ps = TakeWhile[vrow, KeyExistsQ[nameQ, #] &];
          If[Length[ps] < 3,
            Nothing,
            <|
              "Particles" -> ps,
              "Sorted" -> Sort[ps],
              "Valence" -> Length[ps],
              "Row" -> vrow,
              "Expression" -> I*vrow[[-2]]*vrow[[-1]]
            |>
          ]
        ]
      ],
      model[[2]]
    ],
    Nothing
  ];
  bySorted = GroupBy[vertices, #["Sorted"] &];
  (* Only internal currents use generic vector names. Keep exact model rows
     for attachment, where external helicities and shared-edge choices agree. *)
  currentVertices = Flatten @ Map[Function[row,
    Map[Function[ps, Join[row, <|"CurrentParticles" -> ps|>]],
      Tuples[If[KeyExistsQ[internalSpecies, #], {#, internalSpecies[#]}, {#}] & /@
        row["Particles"]]]], vertices];
  currentBySorted = GroupBy[currentVertices, Sort[#["CurrentParticles"]] &];
  combineLookup = Merge[
    Flatten[
      Table[
        Module[{ps = currentVertices[[j, "CurrentParticles"]], nv},
          nv = Length[ps];
          Table[
            {nv - 1, Sort[Delete[ps, i]]} -> {Lookup[internalSpecies, ps[[i]], ps[[i]]]},
            {i, nv}
          ]
        ],
        {j, Length[currentVertices]}
      ],
      1
    ],
    DeleteDuplicates @ Flatten[#] &
  ];
  <|
    "Names" -> names,
    "Anti" -> anti,
    "Mass" -> mass,
    "Spin2" -> spin2,
    "Rank" -> rank,
    "Vertices" -> vertices,
    "BySorted" -> bySorted,
    "CurrentBySorted" -> currentBySorted,
    "InternalSpecies" -> internalSpecies,
    "InternalHelicities" -> helicities,
    "CombineLookup" -> combineLookup,
    "Valences" -> Sort @ DeleteDuplicates[vertices[[All, "Valence"]]]
  |>
];



(* ::Subsubsection::Closed:: *)
(*Usage*)


(* ::Input::Initialization:: *)
CreateDiagrams::usage = "CreateDiagrams[{p1, p2, ...}, model] generates tree-level diagrams. Constructive models use one generic internal photon/gluon containing both helicities. FeynmanModel inputs instead use off-shell Feynman propagators and unitary gauge for massive vectors; external helicities remain fixed.";
CreateArrowDiagram::usage = "CreateArrowDiagram[diag] draws a labeled tree diagram. Options: \"ShowVertexExpressions\", \"ShowPropagatorExpressions\".";
CreateArrowDiagrams::usage = "CreateArrowDiagrams[{diag1, diag2, ...}] draws a list of diagrams.";
diagramAmplitude::usage = "diagramAmplitude[diag] returns its amplitude. Constructive diagrams sum internal photon/gluon helicities and expand external x factors. Feynman diagrams retain ordered Dirac chains, Lorentz/color tensors, and fully off-shell internal numerators. Use diagramAmplitudes for a complete process.";
diagramAmplitudes::usage = "diagramAmplitudes[diagrams] returns {{massRules, amplitude}, ...}, one entry per generated diagram. Constructive diagrams sum internal photon/gluon helicities, including the simplified supported four-point exchanges and legacy helicity-resolved lists. Feynman diagrams retain their exact off-shell tensor amplitudes. Known mass rules are applied and retained for external legs and internal lines.";



(* ::Subsubsection::Closed:: *)
(*tagParticles / multiparticleOf*)


(* ::Input::Initialization:: *)
tagParticles[particles_List] := Module[{seen = <||>},
  MapIndexed[
    Function[{p, i},
      seen[p] = Lookup[seen, p, 0] + 1;
      <|"Particle" -> p, "ID" -> seen[p], "InputIndex" -> First[i]|>
    ],
    particles
  ]
];

multiparticleOf[inds_List] := Multiparticle @@ Sort[inds];



(* ::Subsubsection::Closed:: *)
(*Current algebra*)


(* ::Input::Initialization:: *)
(*Current algebra*)

emptyCurrent[particle_, index_] := <|
  "Particle" -> particle,
  "Externals" -> {index},
  "Root" -> index,
  "InternalVertices" -> {},
  "Edges" -> {},
  "InternalEdges" -> {},
  "EdgeLabels" -> <||>,
  "EdgeOrientation" -> <||>,
  "EdgeIndices" -> <||>
|>;

orientationOf[edge_, labels_, md_] := Module[
  {ends, p1, p2, r1, r2, canon, antiCanon, vCanon, vAnti},
  ends = List @@ edge;
  p1 = labels[ends[[1]]];
  p2 = labels[ends[[2]]];
  r1 = Lookup[md["Rank"], p1, Infinity];
  r2 = Lookup[md["Rank"], p2, Infinity];
  If[r1 <= r2,
    canon = p1; antiCanon = p2; vCanon = ends[[1]]; vAnti = ends[[2]],
    canon = p2; antiCanon = p1; vCanon = ends[[2]]; vAnti = ends[[1]]
  ];
  <|
    "Particle" -> canon,
    "Antiparticle" -> antiCanon,
    "ParticleVertex" -> vCanon,
    "AntiparticleVertex" -> vAnti
  |>
];

(* Glue currents onto a new vertex. leftoverP is the particle this vertex
   consumes on the outgoing line, or None if this vertex completes the diagram. *)
glueCurrents[currents_List, leftoverP_, nextId_, fullExternals_List, md_] := Module[
  {newV, anti = md["Anti"], newEdges, newInternal, newLabels, newOrients, newIndices,
   mergedVerts, mergedEdges, mergedInternal, mergedLabels, mergedOrients, mergedIndices,
   allExt, pAtNew, pAtOld, edge, labels, c},
  newV = nextId;
  allExt = Sort[fullExternals];
  newEdges = {};
  newInternal = {};
  newLabels = <||>;
  newOrients = <||>;
  newIndices = <||>;
  Do[
    c = currents[[i]];
    edge = UndirectedEdge @@ Sort[{c["Root"], newV}];
    AppendTo[newEdges, edge];
    If[c["InternalVertices"] =!= {},
      pAtNew = c["Particle"];
      pAtOld = Lookup[anti, pAtNew, pAtNew];
      labels = <|newV -> pAtNew, c["Root"] -> pAtOld|>;
      AppendTo[newInternal, edge];
      newLabels[edge] = labels;
      newOrients[edge] = orientationOf[edge, labels, md];
      newIndices[edge] = <|
        newV -> multiparticleOf[c["Externals"]],
        c["Root"] -> multiparticleOf[Complement[allExt, c["Externals"]]]
      |>
    ];
    ,
    {i, Length[currents]}
  ];
  mergedVerts = Join[Flatten[currents[[All, "InternalVertices"]]], {newV}];
  mergedEdges = Join[Flatten[currents[[All, "Edges"]]], newEdges];
  mergedInternal = Join[Flatten[currents[[All, "InternalEdges"]]], newInternal];
  mergedLabels = Join @@ Append[currents[[All, "EdgeLabels"]], newLabels];
  mergedOrients = Join @@ Append[currents[[All, "EdgeOrientation"]], newOrients];
  mergedIndices = Join @@ Append[currents[[All, "EdgeIndices"]], newIndices];
  <|
    "Particle" -> If[leftoverP === None, None, Lookup[anti, leftoverP, leftoverP]],
    "Externals" -> Sort[Join @@ currents[[All, "Externals"]]],
    "Root" -> newV,
    "InternalVertices" -> mergedVerts,
    "Edges" -> mergedEdges,
    "InternalEdges" -> mergedInternal,
    "EdgeLabels" -> mergedLabels,
    "EdgeOrientation" -> mergedOrients,
    "EdgeIndices" -> mergedIndices
  |>
];

diagramKey[curr_] := Sort @ KeyValueMap[
  Function[{edge, labels},
    {
      Sort[List @@ curr["EdgeIndices"][edge]],
      KeySort @ Association @ KeyValueMap[
        Function[{v, p}, curr["EdgeIndices"][edge][v] -> p],
        labels
      ]
    }
  ],
  curr["EdgeLabels"]
];



(* ::Subsubsection::Closed:: *)
(*Recursive generation*)


(* ::Input::Initialization:: *)
(*Recursive generation*)

generateRawDiagrams[ext_List, md_] := Module[
  {n, fullExternals, startCurrents, combineLookup, fuse, collected, seen = <||>},
  n = Length[ext];
  If[n < 3, Return[{}]];
  fullExternals = Range[n];
  startCurrents = MapThread[emptyCurrent[#1, #2] &, {ext, fullExternals}];
  combineLookup = md["CombineLookup"];
  collected = First[
    Reap[
      fuse[currents_, nextId_] := Module[
        {nc, parts, ConstructiveDiagrams`Private`state, subsets, chosen, remaining, lefts},
        (* Different construction orders can reach the same current forest.
           Keep all subsets, including trees with leg 1 at a middle vertex. *)
        ConstructiveDiagrams`Private`state =
          Sort[({#["Particle"], #["Externals"], diagramKey[#]} & /@ currents)];
        If[KeyExistsQ[seen, ConstructiveDiagrams`Private`state], Return[]];
        AssociateTo[seen, ConstructiveDiagrams`Private`state -> True];
        nc = Length[currents];
        parts = currents[[All, "Particle"]];
        If[Lookup[md["CurrentBySorted"], Key[Sort[parts]], {}] =!= {},
          Sow[glueCurrents[currents, None, nextId, fullExternals, md], "diag"]
        ];
        If[nc <= 3, Return[]];
        Do[
          subsets = Subsets[Range[nc], {k}];
          Do[
            chosen = currents[[subsets[[si]]]];
            remaining = currents[[Complement[Range[nc], subsets[[si]]]]];
            lefts = Lookup[
              combineLookup,
              Key[{Length[chosen], Sort[chosen[[All, "Particle"]]]}],
              {}
            ];
            Do[
              fuse[
                Prepend[remaining, glueCurrents[chosen, lefts[[li]], nextId, fullExternals, md]],
                nextId + 1
              ],
              {li, Length[lefts]}
            ],
            {si, Length[subsets]}
          ],
          {k, Select[md["Valences"] - 1, 2 <= # <= nc - 2 &]}
        ]
      ];
      fuse[startCurrents, n + 1];,
      "diag"
    ][[2]],
    {}
  ];
  DeleteDuplicatesBy[collected, diagramKey]
];



(* ::Subsubsection::Closed:: *)
(*Feynman rules*)


(* ::Input::Initialization:: *)
(*Feynman rules*)

replaceVertexIndices[expr_, slotRules_Association] := Module[{index},
  index[i_Integer] := Lookup[slotRules, i, i];
  index[Multiparticle[labels___]] := Multiparticle @@ Flatten[
    (index /@ {labels}) /. Multiparticle -> List];
  index[i_] := i;
  expr /. {
    head : (Spinor | SpinorHat)[type : Except["Xi"], position : ("Upper" | "Lower"),
        kind : ("Angle" | "Square"), label_, spin_] :>
      Head[head][type, position, kind, index[label], spin],
    head : (Spinor | SpinorHat)[type : Except["Xi"], kind : ("Angle" | "Square"),
        label_, rest___] :> Head[head][type, kind, index[label], rest],
    Mom[i_] :> Mom[index[i]], MomHat[i_] :> MomHat[index[i]], MomQ[i_] :> MomQ[index[i]],
    Mass[i_] :> Mass[index[i]], MomProd[i_, j_] :> MomProd[index[i], index[j]],
    Mandelstahm[i_, j_] :> Mandelstahm[index[i], index[j]],
    xFactor[labels___] :> xFactor @@ (index /@ {labels}),
    xTildeFactor[labels___] :> xTildeFactor @@ (index /@ {labels})
  }
];

legParticles[legs_List] := Map[#["Particle"] &, legs];

vertexRowMatches[incidentLegs_List, rowParticles_List, internalSpecies_: <||>] :=
  Select[Permutations[incidentLegs],
    And @@ MapThread[Function[{ConstructiveDiagrams`Private`leg, p},
      ConstructiveDiagrams`Private`leg["Particle"] === p ||
        (ConstructiveDiagrams`Private`leg["Kind"] === "Internal" &&
          Lookup[internalSpecies, p, p] === ConstructiveDiagrams`Private`leg["Particle"])],
      {#, rowParticles}] &];

vertexIncidentLegData[vertex_, neighbors_List, extMap_, edgeLabels_, edgeIndices_, leafSet_] :=
  Map[
    Function[neighbor,
      If[KeyExistsQ[extMap, neighbor],
        <|
          "Particle" -> extMap[neighbor, "Particle"],
          "Index" -> extMap[neighbor, "InputIndex"],
          "Kind" -> "External",
          "Source" -> neighbor
        |>,
        Module[{edge = UndirectedEdge @@ Sort[{vertex, neighbor}]},
          <|
            "Particle" -> edgeLabels[edge][vertex],
            "Index" -> edgeIndices[edge][vertex],
            "Kind" -> "Internal",
            "Source" -> edge
          |>
        ]
      ]
    ],
    neighbors
  ];

vertexStructureInstances[vertex_, incidentLegs_, md_, orientations_: <||>] := Module[
  {sorted, rows},
  sorted = Sort[legParticles[incidentLegs]];
  rows = DeleteDuplicatesBy[Lookup[md["CurrentBySorted"], Key[sorted], {}], #["Row"] &];
  Flatten[
    Map[
      Function[row,
        Map[
          Function[ordered,
            Module[{resolved, ConstructiveDiagrams`Private`choices},
              resolved = MapThread[Join[#1, <|"Particle" -> #2|>] &,
                {ordered, row["Particles"]}];
              ConstructiveDiagrams`Private`choices = Association @ MapThread[
                Function[{ConstructiveDiagrams`Private`leg, p},
                If[ConstructiveDiagrams`Private`leg["Kind"] === "Internal" &&
                    KeyExistsQ[md["InternalHelicities"], ConstructiveDiagrams`Private`leg["Particle"]],
                  ConstructiveDiagrams`Private`leg["Source"] ->
                    If[vertex === orientations[ConstructiveDiagrams`Private`leg["Source"]]["ParticleVertex"],
                  p, Lookup[md["Anti"], p, p]], Nothing]], {ordered, row["Particles"]}];
              <|
                "Vertex" -> vertex,
                "IncidentLegs" -> resolved,
                "InternalHelicities" -> ConstructiveDiagrams`Private`choices,
                "ModelRow" -> row["Row"],
                "SlotRules" -> AssociationThread[Range @ Length[ordered], Map[#["Index"] &, ordered]],
                "Expression" -> replaceVertexIndices[row["Expression"],
                  AssociationThread[Range @ Length[ordered], Map[#["Index"] &, ordered]]]
              |>
            ]
          ],
          vertexRowMatches[incidentLegs, row["Particles"], md["InternalSpecies"]]
        ]
      ],
      rows
    ],
    1
  ]
];

compatibleHelicities[a_Association, b_Association] :=
  AllTrue[Intersection[Keys[a], Keys[b]], a[#] === b[#] &];

diagramHelicityAssignments[structures_Association] := Fold[
  Function[{assignments, ConstructiveDiagrams`Private`choices}, DeleteDuplicates @ Flatten[
    Table[If[compatibleHelicities[a, b], KeySort[Join[a, b]], Nothing],
      {a, assignments}, {b, ConstructiveDiagrams`Private`choices}], 1]],
  {<||>},
  (DeleteDuplicates[Lookup[#, "InternalHelicities", <||>]] & /@ Values[structures])
];

internalPropagatorData[edgeLabels_, edgeIndices_, edgeOrientation_, md_] :=
  Association @ KeyValueMap[
    Function[{edge, labels},
      Module[{orient, particle, index, mass, s2},
        orient = edgeOrientation[edge];
        particle = orient["Particle"];
        index = First @ MinimalBy[Values[edgeIndices[edge]], Min[List @@ #] &];
        mass = Lookup[md["Mass"], particle, Missing["MassNotFound"]];
        s2 = Lookup[md["Spin2"], particle, 0];
        edge -> <|
          "Particle" -> particle,
          "Antiparticle" -> orient["Antiparticle"],
          "ParticleVertex" -> orient["ParticleVertex"],
          "AntiparticleVertex" -> orient["AntiparticleVertex"],
          "Index" -> index,
          "Mass" -> mass,
          "Spin2" -> s2,
          "Factor" -> If[MissingQ[mass],
            Missing["MassNotFound", particle],
            I/PropDen[Mom[index], mass]
          ]
        |>
      ]
    ],
    edgeLabels
  ];

attachFeynmanRules[raw_, ext_List, md_] := Module[
  {n, tags, extMap, leafVerts, graph, adj, incident, structures, props, assignments, sumEdges},
  n = Length[ext];
  tags = tagParticles[ext];
  extMap = AssociationThread[Range[n], tags];
  leafVerts = Range[n];
  graph = Graph[
    Join[leafVerts, raw["InternalVertices"]],
    raw["Edges"],
    VertexLabels -> None
  ];
  adj = AssociationMap[AdjacencyList[graph, #] &, raw["InternalVertices"]];
  incident = AssociationMap[
    Function[v,
      vertexIncidentLegData[
        v, adj[v], extMap, raw["EdgeLabels"], raw["EdgeIndices"], leafVerts
      ]
    ],
    raw["InternalVertices"]
  ];
  structures = AssociationMap[
    vertexStructureInstances[#, incident[#], md, raw["EdgeOrientation"]] &,
    raw["InternalVertices"]
  ];
  If[!AllTrue[Values[structures], Length[#] > 0 &], Return[Nothing]];
  assignments = diagramHelicityAssignments[structures];
  If[assignments === {}, Return[Nothing]];
  props = internalPropagatorData[raw["EdgeLabels"], raw["EdgeIndices"], raw["EdgeOrientation"], md];
  sumEdges = Select[Keys[props], KeyExistsQ[md["InternalHelicities"], props[#, "Particle"]] &];
  <|
    "Graph" -> graph,
    "ExternalParticles" -> ext,
    "Spin2" -> md["Spin2"],
    "ExternalAssignment" -> extMap,
    "InternalEdgeData" -> Association @ KeyValueMap[
      Function[{edge, labels},
        edge -> Join[
          raw["EdgeOrientation"][edge],
          <|"EndpointLabels" -> labels|>
        ]
      ],
      raw["EdgeLabels"]
    ],
    "InternalEdgeLabels" -> raw["EdgeLabels"],
    "InternalEdgeOrientation" -> raw["EdgeOrientation"],
    "InternalVertices" -> raw["InternalVertices"],
    "LeafVertices" -> leafVerts,
    "InternalEdgeIndices" -> raw["EdgeIndices"],
    "IncidentLegData" -> incident,
    "VertexStructures" -> structures,
    "HelicitySumEdges" -> sumEdges,
    "HelicityAssignments" -> assignments,
    "InternalHelicities" -> md["InternalHelicities"],
    "Propagators" -> props,
    "Masses" -> md["Mass"]
  |>
];



(* ::Subsubsection::Closed:: *)
(*CreateDiagrams*)


(* ::Input::Initialization:: *)
CreateDiagrams[externalParticles___String, model_] := Module[
  {ext = {externalParticles}, md, raw},
  If[Length[ext] < 3, Return[{}]];
  If[TrueQ[ConstructiveDiagrams`Private`feynmanModelQ[model]],
    Return[ConstructiveDiagrams`Private`feynmanCreateDiagrams[ext, model]]];
  md = buildModelData[model];
  If[!AllTrue[ext, MemberQ[md["Names"], #] && !KeyExistsQ[md["InternalHelicities"], #] &],
    Return[{}]];
  raw = generateRawDiagrams[ext, md];
  DeleteCases[attachFeynmanRules[#, ext, md] & /@ raw, Nothing]
];

CreateDiagrams[ext_List, model_] := CreateDiagrams[Sequence @@ ext, model];



(* ::Subsubsection::Closed:: *)
(*Spin indices and amplitudes*)


(* ::Input::Initialization:: *)
(*Spin indices and amplitudes*)

(* Number of constructive spin indices from the model's 2*spin column,
   matching ConstructiveDiagram. *)
spinIndexSymbols[spin2_] := Module[{idx = {}},
  If[spin2 > 0, AppendTo[idx, i1]];
  If[spin2 > 1, AppendTo[idx, i2]];
  If[spin2 > 2, AppendTo[idx, i3]];
  If[spin2 > 4, AppendTo[idx, i4]];
  idx
];

diagramChannel[diag_] := Module[{props, parts},
  props = Values[Lookup[diag, "Propagators", <||>]];
  If[props === {} || Length[diag["LeafVertices"]] =!= 4, Return["contact"]];
  parts = Sort[List @@ props[[1]]["Index"]];
  Which[
    parts === {1, 2} || parts === {3, 4}, "S",
    parts === {1, 3} || parts === {2, 4}, "T",
    parts === {1, 4} || parts === {2, 3}, "U",
    True, "other"
  ]
];

(* Trace fermion ports before a vector exchange is replaced by its local
   four-spinor tensor. Boson attachments do not change the endpoint matching.
   Vertex port order fixes line direction, independently of graph vertex IDs. *)
diagramFermionLines[diag_] := Module[
  {ext, spin2, leaves, ports, edges, graph, components, lines, ends,
   firstVertex, firstPort},
  ext = Lookup[diag, "ExternalParticles", {}];
  spin2 = Lookup[diag, "Spin2", <||>];
  leaves = Select[Range[Length[ext]], OddQ[Lookup[spin2, ext[[#]], 0]] &];
  If[leaves === {}, Return[{}]];
  ports = Association @ KeyValueMap[Function[{vertex, structures},
    vertex -> Select[Last[structures]["IncidentLegs"],
      OddQ[Lookup[spin2, #["Particle"], 0]] &]],
    Lookup[diag, "VertexStructures", <||>]];
  If[!AllTrue[Values[ports], Length[#] === 0 || Length[#] === 2 &],
    Return[Missing["NonBilinearFermionVertex"]]];
  edges = DeleteDuplicates @ Flatten @ KeyValueMap[Function[{vertex, legs},
    If[#["Kind"] === "External", UndirectedEdge @@ Sort[{vertex, #["Source"]}],
      #["Source"]] & /@ legs], ports];
  graph = Graph[edges];
  components = ConnectedComponents[graph];
  lines = Table[
    ends = Sort[Intersection[component, leaves]];
    If[Length[ends] =!= 2, Return[Missing["OpenFermionLine"]]];
    firstVertex = First[AdjacencyList[graph, First[ends]]];
    firstPort = First[ports[firstVertex]];
    If[firstPort["Kind"] === "External" && firstPort["Source"] === First[ends],
      ends, Reverse[ends]],
    {component, components}];
  If[Sort[Flatten[lines]] =!= leaves, Return[Missing["IncompleteFermionLines"]]];
  SortBy[lines, {ext[[#[[1]]]], ext[[#[[2]]]], Min[#]} &]
];

diagramAmplitude::fermion = "Cannot determine fermion exchange parity: `1`. Fermion vertices must provide an unambiguous pair of ordered fermion ports.";

(* The local spinor amplitudes already include their own ordering signs.
   Only permutations within identical external species remain to be applied.
   Adjacent reference pairs reproduce the established four-point convention. *)
diagramFermionSign[diag_] := Module[{ext, spin2, fermions, lines, order},
  ext = Lookup[diag, "ExternalParticles", {}];
  spin2 = Lookup[diag, "Spin2", <||>];
  fermions = Select[Range[Length[ext]], OddQ[Lookup[spin2, ext[[#]], 0]] &];
  If[DuplicateFreeQ[ext[[fermions]]], Return[1]];
  lines = diagramFermionLines[diag];
  If[MissingQ[lines], Message[diagramAmplitude::fermion, First[lines]]; Return[$Failed]];
  order = GroupBy[Flatten[lines], ext[[#]] &];
  Times @@ (Signature[Ordering[#]] & /@ Values[order])
];

(* Massless spin-1 exchange (photon/gluon) needs an extra minus relative
   to the massive-vector construction so the helicity sum matches the
   hardcoded 4-point templates. *)
masslessVectorSign[diag_] :=
  If[AnyTrue[Values[Lookup[diag, "Propagators", <||>]],
      #["Mass"] === 0 && Lookup[#, "Spin2", 0] === 2 &],
    -1, 1];

(* Prefer the shorter internal partition; if the two sides have the same
   length, keep the one that contains external 1. *)
preferredInternalMultiparticle[mp1_Multiparticle, mp2_Multiparticle] :=
  Module[{a = List @@ mp1, b = List @@ mp2},
    Which[
      Length[a] < Length[b], mp1,
      Length[b] < Length[a], mp2,
      MemberQ[a, 1], mp1,
      MemberQ[b, 1], mp2,
      Min[a] <= Min[b], mp1,
      True, mp2
    ]
  ];

(* Same dummy index on complementary Multiparticles: rewrite one side
   onto the other by momentum conservation. Angle spinors pick up a
   minus, pulled outside SpinorChain; Square spinors do not. *)
alignInternalMultiparticles[expr_, edgePartitions_: Automatic] := Module[
  {byIndex, result},
  byIndex = GroupBy[
    Cases[expr, Spinor[__, mp_Multiparticle, idx_Symbol] :> {idx, mp}, Infinity],
    First -> Last
  ];
  result = Fold[
    Function[{cur, idx},
      Module[{mps = DeleteDuplicates[byIndex[idx]], keep, drop},
        If[Length[mps] != 2, cur,
          keep = preferredInternalMultiparticle[mps[[1]], mps[[2]]];
          drop = First @ Complement[mps, {keep}];
          cur /. {
            Spinor[h___, "Angle", drop, idx] :> -Spinor[h, "Angle", keep, idx],
            Spinor[h___, "Square", drop, idx] :> Spinor[h, "Square", keep, idx]
          }
        ]
      ]
    ],
    expr,
    Keys[byIndex]
  ];
  (* Massless internals leave Helicity spinors on Multiparticles (no dummy
     index). Pair complementary partitions the same way. *)
  result = Module[{mps, pairs},
    mps = DeleteDuplicates @ Cases[result, Spinor[_, _, mp_Multiparticle] :> mp, Infinity];
    pairs = If[edgePartitions === Automatic,
      Select[Subsets[mps, {2}], Sort[Join[List @@ #[[1]], List @@ #[[2]]]] === Range[4] &],
      Select[edgePartitions, Length[#] === 2 && AllTrue[#, MemberQ[mps, #] &] &]];
    Fold[
      Function[{cur, pair},
        Module[{keep, drop},
          keep = preferredInternalMultiparticle[pair[[1]], pair[[2]]];
          drop = First @ Complement[pair, {keep}];
          cur /. {
            Spinor[t_, "Angle", drop] :> -Spinor[t, "Angle", keep],
            Spinor[t_, "Square", drop] :> Spinor[t, "Square", keep]
          }
        ]
      ],
      result,
      pairs
    ]
  ];
  result //. SpinorChain[a___, -Spinor[b__], c___] :> -SpinorChain[a, Spinor[b], c]
];

makeDiagramSpinIndicesExplicit[diag_] := Module[
  {newVertexStructures = diag["VertexStructures"], spinIndices = <||>, nextIndex = 1},
  KeyValueMap[
    Function[{edge, pdata},
      Module[{inds, mpOf},
        inds = If[pdata["Mass"] === 0, {},
          Table[Symbol["ConstructiveDiagrams`i" <> ToString[nextIndex++]],
            {Length[spinIndexSymbols[Lookup[pdata, "Spin2", 0]]]}]];
        spinIndices[edge] = inds;
        If[inds === {} || pdata["Mass"] === 0, Return[]];
        mpOf[v_] := diag["InternalEdgeIndices"][edge][v];
        Do[
          newVertexStructures[v] = Replace[
            newVertexStructures[v],
            {
              {} :> {},
              list_List :> ReplacePart[
                list,
                -1 -> Module[{vs = Last[list], expr},
                  expr = MakeIndicesExplicit[vs["Expression"], mpOf[v], inds];
                  If[v === pdata["AntiparticleVertex"],
                    expr = expr /. {
                      Spinor["Spin", "Upper", type_, mp_, idx_] /;
                          mp === mpOf[v] && MemberQ[inds, idx] :>
                        Spinor["Spin", "Lower", type, mp, idx],
                      SpinorHat["Spin", "Upper", type_, mp_, idx_] /;
                          mp === mpOf[v] && MemberQ[inds, idx] :>
                        SpinorHat["Spin", "Lower", type, mp, idx]
                    }
                  ];
                  Join[vs, <|"Expression" -> expr|>]
                ]
              ]
            }
          ],
          {v, Keys[diag["InternalEdgeIndices"][edge]]}
        ]
      ]
    ],
    Lookup[diag, "Propagators", <||>]
  ];
  Join[diag, <|"VertexStructures" -> newVertexStructures, "SpinIndices" -> spinIndices|>]
];

(* Massive spin 1 (model column 5 is 2): average a vertex with i1 <-> i2
   swapped, on one end of the propagator only. *)
symmetrizeMassiveSpin1Indices[expr_, inds_List] :=
  If[Length[inds] != 2, expr,
    (expr + (expr /. Thread[inds -> Reverse[inds]]))/2
  ];

(* Resolve a correlated edge assignment without generating another topology.
   Keep row/permutation order so each vertex uses the same Last convention. *)
resolveDiagramHelicities[diag_, assignment_Association] := Module[
  {structures, labels, orientations, props, incident, data},
  structures = Map[Select[#, Function[vs,
    AllTrue[Normal @ Lookup[vs, "InternalHelicities", <||>],
      Lookup[assignment, First[#], None] === Last[#] &]]] &, diag["VertexStructures"]];
  labels = Association @ KeyValueMap[Function[{edge, lab}, edge ->
    If[!KeyExistsQ[assignment, edge], lab,
      With[{orient = diag["InternalEdgeOrientation"][edge], h = assignment[edge],
          hs = diag["InternalHelicities"][diag["Propagators"][edge, "Particle"]]},
        Association @ KeyValueMap[Function[{v, p},
          v -> If[v === orient["ParticleVertex"], h, First @ DeleteCases[hs, h]]], lab]]]],
    diag["InternalEdgeLabels"]];
  orientations = Association @ KeyValueMap[Function[{edge, orient}, edge ->
    If[!KeyExistsQ[assignment, edge], orient,
      With[{hs = diag["InternalHelicities"][diag["Propagators"][edge, "Particle"]],
          swap = assignment[edge] =!= First[diag["InternalHelicities"][diag["Propagators"][edge, "Particle"]]]},
        Join[orient, <|"Particle" -> First[hs], "Antiparticle" -> Last[hs],
          "ParticleVertex" -> orient[If[swap, "AntiparticleVertex", "ParticleVertex"]],
          "AntiparticleVertex" -> orient[If[swap, "ParticleVertex", "AntiparticleVertex"]]|>]]]],
    diag["InternalEdgeOrientation"]];
  props = Association @ KeyValueMap[Function[{edge, ConstructiveDiagrams`Private`prop},
    edge -> Join[ConstructiveDiagrams`Private`prop, orientations[edge]]],
    diag["Propagators"]];
  incident = Association @ KeyValueMap[Function[{v, legs},
    v -> Map[Function[ConstructiveDiagrams`Private`leg,
      If[ConstructiveDiagrams`Private`leg["Kind"] === "Internal" &&
          KeyExistsQ[assignment, ConstructiveDiagrams`Private`leg["Source"]],
        Join[ConstructiveDiagrams`Private`leg,
          <|"Particle" -> labels[ConstructiveDiagrams`Private`leg["Source"]][v]|>],
        ConstructiveDiagrams`Private`leg]], legs]], diag["IncidentLegData"]];
  data = Association @ KeyValueMap[Function[{edge, orient},
    edge -> Join[orient, <|"EndpointLabels" -> labels[edge]|>]], orientations];
  Join[KeyDrop[diag, {"HelicitySumEdges", "HelicityAssignments"}],
    <|"VertexStructures" -> structures, "InternalEdgeLabels" -> labels,
      "InternalEdgeOrientation" -> orientations, "InternalEdgeData" -> data,
      "IncidentLegData" -> incident, "Propagators" -> props|>]
];

diagramVertexExpressions[processed_] := Module[{vertexExpr},
  vertexExpr = Association @ KeyValueMap[
    #1 -> Replace[#2, {{} :> 1, list_List :> Lookup[Last[list], "Expression", 1]}] &,
    processed["VertexStructures"]
  ];
  KeyValueMap[
    Function[{edge, pdata},
      If[Lookup[pdata, "Spin2", 0] === 2 && pdata["Mass"] =!= 0,
        Module[{v = pdata["AntiparticleVertex"], inds},
          inds = Lookup[processed["SpinIndices"], edge, {}];
          If[KeyExistsQ[vertexExpr, v] && Length[inds] == 2,
            vertexExpr[v] = symmetrizeMassiveSpin1Indices[vertexExpr[v], inds]
          ]
        ]
      ]
    ],
    Lookup[processed, "Propagators", <||>]
  ];
  vertexExpr
];

helicityResolvedDiagramAmplitude[diag_] := Module[
  {processed, vertexExpr, propagatorFactors, amp},
  processed = makeDiagramSpinIndicesExplicit[diag];
  vertexExpr = diagramVertexExpressions[processed];
  propagatorFactors = Lookup[Values[processed["Propagators"]], "Factor", 1];
  amp = diagramFermionSign[diag] *
    Times @@ Join[Values[vertexExpr], propagatorFactors]/I;
  expandExternalXFactors[alignInternalMultiparticles[amp, Values /@ Values[diag["InternalEdgeIndices"]]],
      Length[Lookup[diag, "ExternalParticles", {}]]] /.
    diagramMassReplacements[diag]
];

diagramAmplitude[diag_] := Module[{assignments, resolved, paired},
  If[Lookup[diag, "AmplitudeType", None] === "Feynman",
    Return[ConstructiveDiagrams`Private`feynmanDiagramAmplitude[diag]]];
  If[Lookup[diag, "HelicitySumEdges", {}] === {},
    Return[helicityResolvedDiagramAmplitude[diag]]];
  assignments = Lookup[diag, "HelicityAssignments", {}];
  If[Length[assignments] =!= 2 || Length[Lookup[diag, "ExternalParticles", {}]] =!= 4,
    paired = genericFermionCurrentAmplitude[diag];
    If[!MissingQ[paired], Return[paired]];
    Return[Total[helicityResolvedDiagramAmplitude[resolveDiagramHelicities[diag, #]] & /@
      assignments]]];
  resolved = resolveDiagramHelicities[diag, #] & /@ assignments;
  paired = genericMasslessSpin1Amplitude[diag, resolved];
  If[MissingQ[paired], Total[helicityResolvedDiagramAmplitude /@ resolved], paired]
];

(* External Mass[i] rules in leg order, then both complementary
   Multiparticles of each internal line. *)
diagramMassReplacements[diag_] := Module[{ext, masses, externalRules, internalRules},
  ext = Lookup[diag, "ExternalParticles", {}];
  masses = Lookup[diag, "Masses", <||>];
  externalRules = Table[
    Mass[i] -> Lookup[masses, ext[[i]], Missing["MassNotFound", ext[[i]]]],
    {i, Length[ext]}
  ];
  If[Lookup[diag, "AmplitudeType", None] === "Feynman", Return[externalRules]];
  internalRules = Flatten @ KeyValueMap[
    Function[{edge, pdata},
      Module[{mass, mps},
        mass = Lookup[pdata, "Mass", Missing["MassNotFound"]];
        mps = SortBy[
          DeleteDuplicates @ Values @ Lookup[
            Lookup[diag, "InternalEdgeIndices", <||>], edge, <||>
          ],
          List @@ # &
        ];
        Mass[#] -> mass & /@ mps
      ]
    ],
    Lookup[diag, "Propagators", <||>]
  ];
  Join[externalRules, internalRules]
];

(* Kinematic factor left after the two 3-point couplings are removed.
   Four fermions keep the minus produced by simplifying the x factors.
   ffVV is positive: that minus is already in the fermion-photon coupling.
   VVVV is Eq. (98) with the two vertex factors removed, so the kinematic
   piece is that expression divided by 2 EE^2/Mass^2.
   Legs {a,b} and {c,d} are the two vertices in 3-point slot order.
   For ffff and ffVV the paper's (1,2,3,4) is (a,b,d,c), so the
   vector mass in ffVV is Mass[d] (paper leg 3). For VVVV, paper
   leg 1 is p, and that mass is Mass[p]. *)
masslessSpin1Den[i_, j_] := PropDen[Mom[Multiparticle @@ Sort[{i, j}]], 0];

spinAngle[kind_, i_, kind2_, j_] :=
  SpinorChain[Spinor[kind, "Angle", i], Spinor[kind2, "Angle", j]];
spinSquare[kind_, i_, kind2_, j_] :=
  SpinorChain[Spinor[kind, "Square", i], Spinor[kind2, "Square", j]];
spinInsert[kind_, i_, p_, kind2_, j_] :=
  SpinorChain[Spinor[kind, "Square", i], Mom[p], Spinor[kind2, "Angle", j]];

fourFermionMasslessVector[a_, b_, c_, d_, kinds_] := Module[{ka, kb, kc, kd},
  {ka, kb, kc, kd} = kinds;
  -(spinAngle[ka, a, kd, d]*spinSquare[kb, b, kc, c] +
    spinAngle[ka, a, kc, c]*spinSquare[kb, b, kd, d] +
    spinAngle[kb, b, kd, d]*spinSquare[ka, a, kc, c] +
    spinAngle[kb, b, kc, c]*spinSquare[ka, a, kd, d])/masslessSpin1Den[a, b]
];

fermionVectorMasslessVector[a_, b_, c_, d_, kinds_] := Module[
  {ka, kb, kc, kd, pairs, s1, s2},
  {ka, kb, kc, kd} = kinds;
  pairs = spinAngle[ka, a, kd, d]*spinSquare[kb, b, kc, c] +
    spinAngle[ka, a, kc, c]*spinSquare[kb, b, kd, d] +
    spinAngle[kb, b, kd, d]*spinSquare[ka, a, kc, c] +
    spinAngle[kb, b, kc, c]*spinSquare[ka, a, kd, d];
  s1 = (spinAngle[kd, d, kc, c] + spinSquare[kd, d, kc, c])*pairs;
  s2 = spinAngle[kd, d, kc, c]*spinSquare[kd, d, kc, c]*
    (spinInsert[ka, a, d, kb, b] + spinInsert[kb, b, d, ka, a]);
  (s1 + s2/Mass[d])/masslessSpin1Den[a, b]
];

(* Eq. (98): T-channel photon. Paper (1,3) and (2,4) are the two vertices.
   The MW in that equation is Mass[i1], the mass of paper leg 1. *)
fourVectorMasslessPolynomial[i1_, i2_, i3_, i4_, kindOf_Association] := Module[
  {a, s, p, poly, g1, g2},
  a[i_, j_] := spinAngle[kindOf[i], i, kindOf[j], j];
  s[i_, j_] := spinSquare[kindOf[i], i, kindOf[j], j];
  p[i_, mom_, j_] := spinInsert[kindOf[i], i, mom, kindOf[j], j];
  poly = -2*a[i2, i4]*a[i3, i4]*s[i1, i2]*s[i1, i3] +
    a[i1, i3]*a[i2, i3]*a[i2, i4]*s[i1, i4] +
    2*a[i2, i3]*a[i2, i4]*s[i1, i3]*s[i1, i4] +
    a[i1, i4]*a[i2, i4]*s[i1, i3]*s[i2, i3] +
    2*a[i1, i3]*a[i2, i4]*s[i1, i4]*s[i2, i3] +
    a[i2, i4]*s[i1, i3]*s[i1, i4]*s[i2, i3] +
    a[i1, i3]*a[i1, i4]*a[i2, i3]*s[i2, i4] +
    2*a[i1, i4]*a[i2, i3]*s[i1, i3]*s[i2, i4] -
    2*a[i1, i3]*a[i2, i4]*s[i1, i3]*s[i2, i4] +
    a[i1, i2]*a[i3, i4]*s[i1, i3]*s[i2, i4] +
    a[i1, i3]*a[i2, i3]*s[i1, i4]*s[i2, i4] +
    2*a[i1, i3]*a[i1, i4]*s[i2, i3]*s[i2, i4] +
    a[i1, i4]*s[i1, i3]*s[i2, i3]*s[i2, i4] +
    a[i1, i3]*a[i2, i4]*s[i1, i2]*s[i3, i4] -
    2*a[i1, i2]*a[i1, i3]*s[i2, i4]*s[i3, i4];
  g1 = a[i1, i3]*a[i2, i4]*s[i2, i3] + a[i1, i3]*a[i2, i3]*s[i2, i4] +
    a[i2, i3]*s[i1, i3]*s[i2, i4] + a[i1, i3]*s[i2, i3]*s[i2, i4];
  g2 = a[i2, i3]*a[i2, i4]*s[i1, i3] + a[i1, i3]*a[i2, i4]*s[i2, i3] +
    a[i2, i4]*s[i1, i3]*s[i2, i3] + a[i2, i3]*s[i1, i3]*s[i2, i4];
  {poly, g1*p[i1, i3, i4] + g2*p[i4, i3, i1]}
];

(* (p,q) and (r,s) are the two vertices in slot order. Paper (1,3)=(p,q), (2,4)=(r,s).
   Vertex factors supply 2 EE^2/Mass^2, so this kinematic factor is half of Eq. (98)
   without that coupling. *)
fourVectorMasslessVector[p_, q_, r_, s_, kinds_] := Module[{k, poly, mom},
  k = AssociationThread[{p, q, r, s}, kinds];
  {poly, mom} = fourVectorMasslessPolynomial[p, r, q, s, k];
  (poly + mom/Mass[p])/(2*masslessSpin1Den[p, q])
];

vertexMatterEnds[diag_] := Map[
  Function[st,
    Module[{legs = Take[st["IncidentLegs"], 2]},
      <|
        "Factor" -> st["ModelRow"][[-2]],
        "Index" -> legs[[All, "Index"]],
        "Particle" -> legs[[All, "Particle"]],
        "Spin2" -> (Lookup[diag["Spin2"], #, 0] & /@ legs[[All, "Particle"]])
      |>
    ]
  ],
  Last /@ Values[diag["VertexStructures"]]
];

spinKindOf[diag_, particle_] :=
  If[Lookup[diag["Masses"], particle, 1] === 0, "Helicity", "Spin"];

masslessSpin1Edge[diag_] := Module[{edges},
  edges = Select[Normal @ Lookup[diag, "Propagators", <||>],
    #[[2, "Mass"]] === 0 && Lookup[#[[2]], "Spin2", 0] === 2 &];
  If[Length[edges] === 1, edges[[1, 1]], None]
];

masslessSpin1PairKey[diag_] := Module[{edge},
  edge = masslessSpin1Edge[diag];
  If[edge === None, None,
    {Lookup[diag, "ExternalParticles", {}],
     diag["Propagators"][edge, "Particle"],
     diag["Propagators"][edge, "Index"]}
  ]
];

masslessSpin1PairAmplitude[diag_] := Module[
  {ends, fa, fb, a, b, c, d, kinds, sA, sB, generic, particles},
  If[Length[Lookup[diag, "ExternalParticles", {}]] =!= 4,
    Return[Missing["UnsupportedMasslessSpin1"]]];
  ends = SortBy[vertexMatterEnds[diag], Min[#["Index"]] &];
  If[Length[ends] =!= 2, Return[Missing["UnsupportedMasslessSpin1"]]];
  sA = ends[[1, "Spin2"]];
  sB = ends[[2, "Spin2"]];
  (* Fermions occupy (a,b) when one vertex is a fermion current. *)
  If[sA === {2, 2} && sB === {1, 1}, ends = Reverse[ends]; {sA, sB} = {sB, sA}];
  {a, b} = ends[[1, "Index"]];
  {c, d} = ends[[2, "Index"]];
  particles = Join[ends[[1, "Particle"]], ends[[2, "Particle"]]];
  kinds = spinKindOf[diag, #] & /@ particles;
  (* The massive-vector formulas divide by matter masses, not by the
     exchange mass. Massless external gluons need the exact helicity sum. *)
  If[AnyTrue[Range[Length[particles]],
      Lookup[diag["Spin2"], particles[[#]], 0] === 2 && kinds[[#]] === "Helicity" &],
    Return[Missing["UnsupportedMasslessSpin1"]]];
  fa = ends[[1, "Factor"]];
  fb = ends[[2, "Factor"]];
  generic = Which[
    sA === {1, 1} && sB === {1, 1},
      fourFermionMasslessVector[a, b, c, d, kinds],
    sA === {1, 1} && sB === {2, 2},
      fermionVectorMasslessVector[a, b, c, d, kinds],
    sA === {2, 2} && sB === {2, 2},
      fourVectorMasslessVector[a, b, c, d, kinds],
    True, Missing["UnsupportedMasslessSpin1"]
  ];
  If[MissingQ[generic], Return[generic]];
  diagramFermionSign[diag]*fa*fb*generic /. diagramMassReplacements[diag]
];

genericMasslessSpin1Amplitude[diag_, resolved_List] := Module[{edges, edge, hs, ends},
  edges = Lookup[diag, "HelicitySumEdges", {}];
  If[Length[edges] =!= 1 || Length[resolved] =!= 2 ||
      Length[Lookup[diag, "ExternalParticles", {}]] =!= 4,
    Return[Missing["UnsupportedMasslessSpin1"]]];
  edge = First[edges];
  hs = diag["InternalHelicities"][diag["Propagators"][edge, "Particle"]];
  If[Sort[Lookup[diag["HelicityAssignments"], edge]] =!= Sort[hs],
    Return[Missing["IncompleteMasslessSpin1"]]];
  If[!AllTrue[resolved, AllTrue[Values[#["VertexStructures"]],
      Length[Last[#]["IncidentLegs"]] === 3 && Last[Last[#]["IncidentLegs"]]["Source"] === edge &] &],
    Return[Missing["UnsupportedMasslessSpin1"]]];
  ends = SortBy[vertexMatterEnds[#], Min[#["Index"]] &] & /@ resolved;
  If[ends[[1]] =!= ends[[2]], Return[Missing["UnequalHelicityCouplings"]]];
  masslessSpin1PairAmplitude[First[resolved]]
];

standardFermionVectorVertexQ[vs_] := MatchQ[vs["ModelRow"][[-1]],
  xFactor[1, 2] SpinorChain[Spinor["Spin" | "Helicity", "Angle", 1],
    Spinor["Spin" | "Helicity", "Angle", 2]] |
  xTildeFactor[1, 2] SpinorChain[Spinor["Spin" | "Helicity", "Square", 1],
    Spinor["Spin" | "Helicity", "Square", 2]]];

diagramMatterSpinor[processed_, ConstructiveDiagrams`Private`leg_, vertex_, chirality_] :=
  Module[{ConstructiveDiagrams`Private`prop, inds, kind},
  kind = spinKindOf[processed, ConstructiveDiagrams`Private`leg["Particle"]];
  If[ConstructiveDiagrams`Private`leg["Kind"] === "External" || kind === "Helicity",
    Return[Spinor[kind, chirality, ConstructiveDiagrams`Private`leg["Index"]]]];
  ConstructiveDiagrams`Private`prop = processed["Propagators"][ConstructiveDiagrams`Private`leg["Source"]];
  inds = processed["SpinIndices"][ConstructiveDiagrams`Private`leg["Source"]];
  Spinor["Spin", If[vertex === ConstructiveDiagrams`Private`prop["AntiparticleVertex"], "Lower", "Upper"],
    chirality, ConstructiveDiagrams`Private`leg["Index"], First[inds]]
];

(* Sew disjoint fermion currents locally. The same four-spinor tensor is
   valid for composite matter labels; explicit spin indices are spectators.
   Other vertices (notably shared three-gluon vertices) use the exact sum. *)
genericFermionCurrentAmplitude[diag_] := Module[
  {edges, vertices, locals, hs, processed, vertexExpr, factors, ends, legs,
   angle, square, current, amp},
  edges = Lookup[diag, "HelicitySumEdges", {}];
  vertices = Keys[diag["InternalEdgeLabels"][#]] & /@ edges;
  If[edges === {} || !DuplicateFreeQ[Flatten[vertices]] ||
      Length[diag["HelicityAssignments"]] =!= 2^Length[edges],
    Return[Missing["UnsupportedMasslessSpin1"]]];
  Do[
    hs = diag["InternalHelicities"][diag["Propagators"][edges[[i]], "Particle"]];
    Do[
      locals = Last /@ GatherBy[diag["VertexStructures"][v], #["InternalHelicities"] &];
      If[Length[locals] =!= 2 ||
          Sort[Lookup[Lookup[locals, "InternalHelicities"], edges[[i]]]] =!= Sort[hs] ||
          !AllTrue[locals, Length[#["IncidentLegs"]] === 3 &&
            Last[#["IncidentLegs"]]["Source"] === edges[[i]] &&
            (Lookup[diag["Spin2"], #, 0] & /@ Take[legParticles[#["IncidentLegs"]], 2]) === {1, 1} &&
            (SameQ @@ (Lookup[diag["Masses"], #] & /@ Take[legParticles[#["IncidentLegs"]], 2])) &&
            standardFermionVectorVertexQ[#] &] ||
          Length[DeleteDuplicates[({Take[#["IncidentLegs"], 2], #["ModelRow"][[-2]]} & /@ locals)]] =!= 1,
        Return[Missing["UnsupportedMasslessSpin1"]]],
      {v, vertices[[i]]}],
    {i, Length[edges]}];
  processed = makeDiagramSpinIndicesExplicit[
    resolveDiagramHelicities[diag, First[diag["HelicityAssignments"]]]];
  vertexExpr = diagramVertexExpressions[processed];
  factors = Association @ KeyValueMap[#1 -> #2["Factor"] &, processed["Propagators"]];
  Do[
    ends = Last[processed["VertexStructures"][#]] & /@ vertices[[i]];
    legs = Join @@ (Take[#["IncidentLegs"], 2] & /@ ends);
    angle = MapThread[diagramMatterSpinor[processed, #1, #2, "Angle"] &,
      {legs, Flatten[ConstantArray[#, 2] & /@ vertices[[i]]]}];
    square = MapThread[diagramMatterSpinor[processed, #1, #2, "Square"] &,
      {legs, Flatten[ConstantArray[#, 2] & /@ vertices[[i]]]}];
    current = SpinorChain[angle[[1]], angle[[4]]] SpinorChain[square[[2]], square[[3]]] +
      SpinorChain[angle[[1]], angle[[3]]] SpinorChain[square[[2]], square[[4]]] +
      SpinorChain[angle[[2]], angle[[4]]] SpinorChain[square[[1]], square[[3]]] +
      SpinorChain[angle[[2]], angle[[3]]] SpinorChain[square[[1]], square[[4]]];
    vertexExpr[vertices[[i, 1]]] = -I (Times @@ (#["ModelRow"][[-2]] & /@ ends)) current/
      PropDen[Mom[processed["Propagators"][edges[[i]], "Index"]], 0];
    vertexExpr[vertices[[i, 2]]] = 1;
    factors[edges[[i]]] = 1,
    {i, Length[edges]}];
  amp = diagramFermionSign[diag] (Times @@ Join[Values[vertexExpr], Values[factors]])/I;
  expandExternalXFactors[alignInternalMultiparticles[amp, Values /@ Values[diag["InternalEdgeIndices"]]],
    Length[diag["ExternalParticles"]]] /. diagramMassReplacements[diag]
];

diagramAmplitudes[diags_List] := Module[{n, used, out, i, key, j, paired},
  n = Length[diags];
  used = ConstantArray[False, n];
  out = {};
  Do[
    If[used[[i]], Continue[]];
    If[Lookup[diags[[i]], "AmplitudeType", None] === "Feynman",
      paired = diagramAmplitude[diags[[i]]];
      If[FailureQ[paired], Return[paired]];
      AppendTo[out, {diagramMassReplacements[diags[[i]]], paired}]; Continue[]];
    key = If[Lookup[diags[[i]], "HelicitySumEdges", {}] === {},
      masslessSpin1PairKey[diags[[i]]], None];
    If[key === None,
      AppendTo[out, {diagramMassReplacements[diags[[i]]], diagramAmplitude[diags[[i]]]}],
      j = SelectFirst[Range[i + 1, n],
        !used[[#]] && Lookup[diags[[#]], "HelicitySumEdges", {}] === {} &&
          masslessSpin1PairKey[diags[[#]]] === key &, None];
      If[j === None,
        AppendTo[out, {diagramMassReplacements[diags[[i]]], diagramAmplitude[diags[[i]]]}],
        paired = masslessSpin1PairAmplitude[diags[[i]]];
        (* Consume the partner only when the shortcut evaluates the full pair. *)
        If[MissingQ[paired],
          AppendTo[out, {diagramMassReplacements[diags[[i]]], diagramAmplitude[diags[[i]]]}],
          used[[j]] = True;
          AppendTo[out, {diagramMassReplacements[diags[[i]]], paired}]
        ]
      ]
    ],
    {i, n}
  ];
  out
];



(* ::Subsubsection::Closed:: *)
(*Drawing*)


(* ::Input::Initialization:: *)
(*Drawing*)

displayParticleName[edge_, internalEdgeLabels_Association] := Module[
  {lab, vals},
  lab = Lookup[internalEdgeLabels, edge, Missing["NotFound"]];
  If[MissingQ[lab],
    "",
    vals = DeleteDuplicates[Values[lab]];
    If[Length[vals] == 1, First[vals], Lookup[lab, First[List @@ edge]]]
  ]
];

arrowDirectionData[edge_, internalEdgeLabels_Association, particleToPrint_: Automatic] := Module[
  {lab, ends, printedParticle, targetVertex, sourceVertex},
  lab = Lookup[internalEdgeLabels, edge, Missing["NotFound"]];
  If[MissingQ[lab] || !AssociationQ[lab], Return[Missing["NotAvailable"]]];
  If[MemberQ[{"A", "G"}, First[Values[lab]]] && Length[DeleteDuplicates[Values[lab]]] === 1,
    Return[Missing["NotAvailable"]]];
  ends = List @@ edge;
  printedParticle = Replace[particleToPrint, Automatic :> First[DeleteDuplicates[Values[lab]]]];
  targetVertex = SelectFirst[ends, Lookup[lab, #, None] === printedParticle &, Missing["NotFound"]];
  If[MissingQ[targetVertex], Return[Missing["NotAvailable"]]];
  sourceVertex = SelectFirst[ends, # =!= targetVertex &, Missing["NotFound"]];
  If[MissingQ[sourceVertex], Return[Missing["NotAvailable"]]];
  {targetVertex, sourceVertex}
];

midArrowPoints[start_, end_, frac_: 0.22] := Module[{mid = (start + end)/2, vec = end - start},
  {mid - frac vec/2, mid + frac vec/2}
];

Options[CreateArrowDiagram] = {
  "ShowVertexExpressions" -> False,
  "ShowPropagatorExpressions" -> False
};

CreateArrowDiagram[diag_, OptionsPattern[]] := Module[
  {
    graph, extMap, intLabels, propagators, leafVerts, internalVerts, vertexStructures,
    showVertexExpressions, showPropagatorExpressions,
    embeddingGraph, coords, coordOf, edges,
    edgePrimitives, vertexPrimitives, externalLabelPrimitives,
    edgeLabelPrimitives, vertexExpressionPrimitives,
    midpoint, normalOffset, leafSet
  },
  showVertexExpressions = OptionValue["ShowVertexExpressions"];
  showPropagatorExpressions = OptionValue["ShowPropagatorExpressions"];
  graph = diag["Graph"];
  extMap = diag["ExternalAssignment"];
  intLabels = diag["InternalEdgeLabels"];
  propagators = Lookup[diag, "Propagators", <||>];
  vertexStructures = Lookup[diag, "VertexStructures", <||>];
  leafVerts = Lookup[diag, "LeafVertices", Select[VertexList[graph], VertexDegree[graph, #] == 1 &]];
  internalVerts = Lookup[diag, "InternalVertices", Complement[VertexList[graph], leafVerts]];
  leafSet = AssociationThread[leafVerts, True];
  embeddingGraph = Graph[VertexList[graph], EdgeList[graph], GraphLayout -> "SpringElectricalEmbedding"];
  coords = GraphEmbedding[embeddingGraph];
  coordOf = AssociationThread[VertexList[embeddingGraph], coords];
  edges = EdgeList[graph];
  midpoint[p1_, p2_] := (p1 + p2)/2;
  normalOffset[p1_, p2_] := Module[{n = {-(p2 - p1)[[2]], (p2 - p1)[[1]]}},
    0.12 Normalize[If[Norm[n] == 0, {1., 0.}, n]]
  ];
  edgePrimitives = Map[
    Function[edge,
      Module[{u, v, p1, p2, dirData, arrowPts, leaf, inner, pLeaf, pInner, startVertex, endVertex, pStart, pEnd},
        {u, v} = List @@ edge;
        {p1, p2} = coordOf /@ {u, v};
        If[KeyExistsQ[leafSet, u] || KeyExistsQ[leafSet, v],
          leaf = If[KeyExistsQ[leafSet, u], u, v];
          inner = If[leaf === u, v, u];
          pLeaf = coordOf[leaf];
          pInner = coordOf[inner];
          arrowPts = midArrowPoints[pLeaf, pInner];
          {Black, Thick, Line[{pLeaf, pInner}], Arrow[arrowPts]},
          dirData = arrowDirectionData[edge, intLabels];
          If[MissingQ[dirData],
            {Black, Thick, Line[{p1, p2}]},
            {startVertex, endVertex} = dirData;
            pStart = coordOf[startVertex];
            pEnd = coordOf[endVertex];
            arrowPts = midArrowPoints[pStart, pEnd];
            {Black, Thick, Line[{p1, p2}], Arrow[arrowPts]}
          ]
        ]
      ]
    ],
    edges
  ];
  edgeLabelPrimitives = Map[
    Function[edge,
      If[KeyExistsQ[intLabels, edge],
        Module[{p1, p2, pos, particleLabel, propData, propFactor},
          {p1, p2} = coordOf /@ (List @@ edge);
          pos = midpoint[p1, p2] + normalOffset[p1, p2];
          particleLabel = displayParticleName[edge, intLabels];
          propData = Lookup[propagators, edge, Missing["NotFound"]];
          propFactor = If[AssociationQ[propData], Lookup[propData, "Factor", Missing["NotFound"]], Missing["NotFound"]];
          {
            Blue, Text[Style[particleLabel, 14], pos],
            If[showPropagatorExpressions && !MissingQ[propFactor],
              Text[Style[TraditionalForm[propFactor], 11, Darker[Green]], pos + {0, -0.08}],
              Nothing
            ]
          }
        ],
        Nothing
      ]
    ],
    edges
  ];
  vertexPrimitives = {Black, Disk[coordOf /@ internalVerts, 0.03]};
  externalLabelPrimitives = Map[
    Function[v,
      Module[{p, nbr, dir, pos, data, label},
        p = coordOf[v];
        nbr = First[AdjacencyList[graph, v]];
        dir = Normalize[p - coordOf[nbr] /. {0, 0} | {0., 0.} -> {1., 0.}];
        pos = p + 0.12 dir;
        data = Lookup[extMap, v, Missing["NotFound"]];
        label = If[AssociationQ[data], Subscript[data["Particle"], data["InputIndex"]], data];
        Text[Style[label, 14, Black], pos]
      ]
    ],
    leafVerts
  ];
  vertexExpressionPrimitives = If[showVertexExpressions,
    Map[
      Function[v,
        Module[{pos, exprList, shownExpr},
          pos = coordOf[v] + {0, 0.14};
          exprList = Lookup[vertexStructures, v, {}];
          shownExpr = Which[
            exprList === {}, Missing["NotAvailable"],
            Length[DeleteDuplicates[Lookup[exprList, "InternalHelicities", <||>]]] > 1,
              Grid[({#["InternalHelicities"], TraditionalForm[#["Expression"]]} & /@
                (Last /@ GatherBy[exprList, Lookup[#, "InternalHelicities", <||>] &]))],
            KeyExistsQ[First[exprList], "Factor"], First[exprList]["Factor"],
            KeyExistsQ[First[exprList], "Expression"], First[exprList]["Expression"],
            True, Missing["NotAvailable"]
          ];
          If[MissingQ[shownExpr],
            Nothing,
            Text[Style[TraditionalForm[shownExpr], 10, Darker[Red]], pos]
          ]
        ]
      ],
      internalVerts
    ],
    {}
  ];
  Graphics[
    {edgePrimitives, edgeLabelPrimitives, vertexPrimitives, externalLabelPrimitives, vertexExpressionPrimitives},
    PlotRange -> All,
    ImagePadding -> 50,
    ImageSize -> 500
  ]
];

Options[CreateArrowDiagrams] = Options[CreateArrowDiagram];
CreateArrowDiagrams[diags_List, opts : OptionsPattern[CreateArrowDiagram]] :=
  CreateArrowDiagram[#, opts] & /@ diags;

(* Remove temporary symbols exposed by the earlier diagram implementation,
   so reloading into an existing notebook also removes its shadow warnings.
   Names can abbreviate the context, and removing a symbol can release its
   temporary locals. Qualify names and check they still exist before Remove. *)
Scan[If[Names[#] =!= {}, Remove[#]] &, DeleteDuplicates[
  ("ConstructiveDiagrams`" <> Last[StringSplit[#, "`"]] &) /@ Flatten[Names /@ {
  "ConstructiveDiagrams`state", "ConstructiveDiagrams`state$*",
  "ConstructiveDiagrams`leg", "ConstructiveDiagrams`leg$*",
  "ConstructiveDiagrams`choices", "ConstructiveDiagrams`choices$*",
  "ConstructiveDiagrams`prop", "ConstructiveDiagrams`prop$*"}]]];

(* ::Section::Closed:: *)
(*Physical Residue Diagnostics*)

(* ::Subsection::Closed:: *)
(*Public Functions*)

DiagramAmplitudeRecords::usage = "DiagramAmplitudeRecords[diagrams] retains each diagram's original expression, physical propagators, numerator conditions and mass rules. The string option SimplificationFunction accepts a function on {massRules, expression}; its default is Identity. NumeratorOnShellChannels declares any internal conditions used by that function; no internal-shell simplification is applied automatically.";
PhysicalPoleResidue::usage = "PhysicalPoleResidue[expression, channel] extracts a pole coefficient. A list of channels extracts a compatible multiple-pole coefficient. Without a coherent KinematicMap the result is only FORMAL. A map f[expression, channels, poleVariables] must evaluate all spinors, momenta and propagators consistently near the selected cuts. KinematicKind distinguishes Sample from Symbolic charts. StructuralCoefficient -> True also computes the formal opaque-propagator coefficient by default; False skips that optional symbolic calculation and returns Missing[NotRequested] in its field without changing the mapped physical residue or kinematic validation. No internal on-shell condition is inferred from diagram mass rules.";
CheckAmplitudes::usage = "CheckAmplitudes[diagramAmplitudes, channels, model] checks physical residues against the ordinary rows returned by FactorizationAmplitudeChannels, automatically discovers apparent denominator poles, and tests high-energy growth and explicit Xi reference-spinor independence of the complete summed amplitude. Only supplied channels certify physical poles; other supported denominator poles are tested for cancellation while physical propagators remain nonzero. The physical comparison applies the sewing factor -1 from (i A_L)(i A_R)(i/PropDen)/i. HighEnergyCheck -> Automatic enables exact high-energy expansions at sampled real, fixed-angle, momentum-conserving kinematics with external masses and couplings held fixed. HighEnergyBound -> Automatic assumes the renormalizable-tree bound E^(4-n); a numerical exponent overrides it, and HighEnergyCheck -> False disables this diagnostic. HighEnergySamples and HighEnergyTimeConstraint control the energy test. ReferenceCheck -> Automatic varies all literal Xi labels simultaneously at sampled conserved external kinematics away from physical poles, holding momenta and external spin states fixed. ReferenceSamples and ReferenceTimeConstraint control this test; ReferenceCheck -> False disables it. An expression without explicit reference spinors has no explicit Xi dependence; this is not a Ward-identity proof. ParameterRules -> Automatic uses numerical variables and dependent functions from the supplied model. An explicit rule list overrides selected model parameters; dependent functions are recomputed. Numerical values are used only for checking, leaving the amplitude symbolic. PASS denotes agreement, finiteness, acceptable growth or reference independence at the recorded kinematic samples, not a generic analytic proof. Opp Sign diagnoses an overall opposite sign in physical residues or a supplied Feynman comparison. Unsupported calculations and timeouts remain UNRESOLVED. Output -> \"HighEnergy\" shows energy samples, \"References\" shows reference comparisons, \"Residues\" shows physical and spurious-pole samples, \"Summary\" returns combined counts and \"Details\" retains all diagnostics. SpuriousPoles -> Automatic discovers denominators; a list overrides discovery and {} disables spurious checks. Set SpuriousPoles -> {}, HighEnergyCheck -> False and ReferenceCheck -> False for the legacy physical-only report. Samples, RandomSeed, ParameterRules and TimeConstraint control pole comparisons. Inputs remain unchanged and no amplitude correction or new interaction is added.";
DiagramAmplitudeRecords::args = "Expected a list of diagram associations and valid diagram-local simplification options.";
PhysicalPoleResidue::args = "Expected channel associations with Cut, Complement, Mass and ExternalLegs, and valid diagnostic options.";
CheckAmplitudes::args = "Expected diagram amplitudes, the ordinary channel rows returned by FactorizationAmplitudeChannels, a model, and valid comparison options.";
CheckAmplitudes::input = "`1`";
CheckAmplitudes::usage = CheckAmplitudes::usage <>
  " FeynmanAmplitudes -> None optionally supplies an independent full Feynman amplitude, a mass-rule/expression pair, or a list of pairs. The full sums are compared at common conserved external on-shell momenta away from all internal propagator poles, with no internal-shell substitutions and no sewing sign or normalization fitted to the benchmark. Explicit Xi references vary independently between the two full sums. FeynmanSamples -> Automatic uses Samples and FeynmanTimeConstraint -> Automatic uses TimeConstraint. Output -> \"Feynman\" shows visible amplitude samples; Details includes FeynmanCheck. PASS and Opp Sign are sampled comparisons of all implicit massive-spin components, not generic analytic proofs. Raw supported Feynman tensors are converted automatically; use PrepareAmplitudes with FermionHelicities first when massless Dirac helicities must be supplied.";
CheckPhysicalResidues::usage = "CheckPhysicalResidues is a compatibility alias for CheckAmplitudes, which checks physical residues, spurious poles, high-energy growth, reference independence and optional full Feynman-amplitude agreement.";
CheckAmplitudes::usage = CheckAmplitudes::usage <>
  " Supply FeynmanModel -> benchmarkModel when the benchmark comes from a separate model. Its numerical parameters are resolved only from that model; Automatic uses the third argument's model. Missing benchmark definitions remain UNRESOLVED rather than borrowing candidate values. ParameterRules may use string names such as \"Me\" -> 1 for local overrides.";

Begin["ConstructiveDiagrams`Private`"];

(* ::Subsection::Closed:: *)
(*Channel Metadata*)

residueChannelKey[channel_Association] := {channel["Cut"], channel["Mass"]};
residueChannelQ[channel_] := AssociationQ[channel] &&
  AllTrue[{"Cut", "Complement", "Mass", "ExternalLegs"}, KeyExistsQ[channel, #] &] &&
  MatchQ[channel["ExternalLegs"], {__Integer}] &&
  MatchQ[channel["Cut"], {__Integer}] && MatchQ[channel["Complement"], {__Integer}] &&
  DuplicateFreeQ[channel["ExternalLegs"]] && DuplicateFreeQ[channel["Cut"]] &&
  DuplicateFreeQ[channel["Complement"]] &&
  Intersection[channel["Cut"], channel["Complement"]] === {} &&
  Union[channel["Cut"], channel["Complement"]] === Sort[channel["ExternalLegs"]];

residueNormalizeChannel[channel_Association] := Module[{cut, external},
  external = Sort[channel["ExternalLegs"]];
  cut = canonicalPhysicalCut[channel["Cut"], external];
  Join[channel, <|"Cut" -> cut, "Complement" -> Complement[external, cut],
    "ExternalLegs" -> external,
    "Propagator" -> PropDen[Mom[Multiparticle @@ cut], channel["Mass"]]|>]
];

residueExternalMassSignature[channel_Association] := Module[{rules, masses, matches},
  rules = Lookup[channel, "ExternalMassRules", Missing["ExternalMassRules"]];
  If[!ListQ[rules] || !AllTrue[rules, MatchQ[#, _Rule] &],
    Return[Missing["ExternalMassRules"]]];
  masses = Map[Function[leg,
    matches = Select[rules, First[#] === Mass[leg] &];
    If[Length[matches] === 1, Last[First[matches]], Missing["ExternalMass", leg]]
  ], Sort[channel["ExternalLegs"]]];
  If[!FreeQ[masses, _Missing], Missing["ExternalMassRules"],
    {Sort[channel["ExternalLegs"]], masses}]
];

residueSameProcessQ[items_List] := items === {} || Length[DeleteDuplicates[
  DeleteMissing[Lookup[items, "ExternalParticles", Missing["ExternalParticles"]]]]] <= 1;

residueMomentumLegs[index_Integer] := {index};
residueMomentumLegs[Multiparticle[legs__Integer]] := Sort[{legs}];
residueMomentumLegs[_] := $Failed;
residueMomentumLegsFromExpression[momentum_] := Module[{terms, indices, sign},
  terms = If[Head[momentum] === Plus, List @@ momentum, {momentum}];
  sign = Which[AllTrue[terms, MatchQ[#, Mom[_]] &], 1,
    AllTrue[terms, MatchQ[#, -Mom[_]] &], -1, True, Return[$Failed]];
  indices = residueMomentumLegs /@ (terms /. If[sign === 1,
    Mom[index_] :> index, -Mom[index_] :> index]);
  If[MemberQ[indices, $Failed], $Failed, Sort[Flatten[indices]]]
];

residuePropagatorKey[PropDen[momentum_, mass_], external_List] := Module[{legs},
  legs = residueMomentumLegsFromExpression[momentum];
  If[legs === $Failed || !DuplicateFreeQ[legs] || legs === {} ||
      Complement[legs, external] =!= {} || Length[legs] === Length[external],
    Missing["UnsupportedPropagator"], {canonicalPhysicalCut[legs, external], mass}]
];
residuePropagatorKey[_, _] := Missing["UnsupportedPropagator"];

residueCanonicalPropagators[expr_, external_List] := expr /. p_PropDen :>
  With[{key = residuePropagatorKey[p, external]},
    If[MissingQ[key], p, PropDen[Mom[Multiparticle @@ First[key]], Last[key]]]];

(* Adapt the user's compact rows; never regenerate or sew factorization channels. *)
residueAmplitudePairQ[pair_] := MatchQ[pair, {_List, _}] &&
  AllTrue[First[pair], MatchQ[#, _Rule] &];

residueSimpleChannel[cut_List, mass_, external_List, massRules_List, model_List] :=
  residueNormalizeChannel[<|"Cut" -> cut,
    "Complement" -> Complement[external, cut], "ExternalLegs" -> external,
    "ExternalMassRules" -> massRules, "Mass" -> mass,
    "InternalParticle" -> DeleteDuplicates[Flatten[Cases[model[[1]],
      row_List /; row[[6]] === mass :> row[[{2, 3}]]]]]|>];

residueSimpleInputs[amplitudes_, channelRows_List, model_List] := Module[
  {pairs, expressions, providedRules, externalRules, external, masses, n,
   targets = {}, records = {}, channel, cut, mass, rowExpression, numerator,
   propagator, contour, candidate, keys, key, particleMasses, pairRules},
  If[Length[model] < 2 || !ListQ[model[[1]]] || !ListQ[model[[2]]] ||
      !AllTrue[model[[1]], ListQ[#] && Length[#] >= 6 &],
    Return[Failure["InvalidModel", <|"MessageTemplate" -> "The particle model is invalid."|>]]];
  pairs = Which[
    residueAmplitudePairQ[amplitudes], {amplitudes},
    ListQ[amplitudes] && AllTrue[amplitudes, residueAmplitudePairQ], amplitudes,
    !ListQ[amplitudes], {{{}, amplitudes}},
    True, Return[Failure["InvalidAmplitudes", <|
      "MessageTemplate" -> "Supply an expression or the mass-rule/expression pairs from diagramAmplitudes."|>]]
  ];
  If[!AllTrue[channelRows, ListQ[#] && Length[#] >= 7 &&
      MatchQ[#[[-3]], Multiparticle[__Integer]] &],
    Return[Failure["InvalidChannels", <|
      "MessageTemplate" -> "Supply the ordinary rows from FactorizationAmplitudeChannels."|>]]];
  providedRules = Map[Function[pair,
    Select[First[pair], MatchQ[First[#], Mass[_Integer]] &]], pairs];
  If[channelRows =!= {},
    n = Length[First[channelRows]] - 3;
    masses = Take[First[channelRows], n];
    If[!AllTrue[channelRows, Length[#] === n + 3 && Take[#, n] === masses &],
      Return[Failure["InconsistentMasses", <|
        "MessageTemplate" -> "All channel rows must describe the same external masses."|>]]];
    external = Range[n]; externalRules = Thread[Mass /@ external -> masses],
    externalRules = If[providedRules === {}, {},
      DeleteDuplicates[Flatten[providedRules, 1]]];
    external = Sort[Cases[externalRules, HoldPattern[Rule[Mass[leg_Integer], _]] :> leg]];
    If[!DuplicateFreeQ[external] || (external =!= {} && external =!= Range[Max[external]]),
      Return[Failure["InconsistentMasses", <|
        "MessageTemplate" -> "Diagram mass rules must consistently specify every external leg."|>]]];
    externalRules = SortBy[externalRules, First[#][[1]] &]
  ];
  If[!AllTrue[Flatten[providedRules, 1],
      MemberQ[external, First[#][[1]]] && (First[#] /. externalRules) === Last[#] &],
    Return[Failure["InconsistentMasses", <|
      "MessageTemplate" -> "Diagram and factorization external masses disagree."|>]]];
  expressions = Map[Function[pair,
    ReduceSpinContractions[Last[pair]] /. Join[First[pair], externalRules]], pairs];
  If[!FreeQ[expressions, _Failure | _Missing | $Failed | $Aborted | $TimedOut],
    Return[Failure["InvalidAmplitudes", <|"MessageTemplate" -> "An amplitude calculation failed."|>]]];
  If[external === {} && !TrueQ[Total[expressions] === 0],
    Return[Failure["ExternalMassesRequired", <|
      "MessageTemplate" -> "With no channel rows, supply the diagramAmplitudes mass-rule/expression pairs so the external masses are known."|>]]];
  targets = Map[Function[row,
    cut = List @@ row[[-3]]; mass = row[[-2]];
    If[!DuplicateFreeQ[cut] || Length[cut] < 2 || Length[cut] > Length[external] - 2 ||
        Complement[cut, external] =!= {},
      Failure["InvalidCut", <|"MessageTemplate" -> "A channel has invalid external momentum labels."|>],
    channel = residueSimpleChannel[cut, mass, external, externalRules, model];
    propagator = PropDen[Mom[row[[-3]]], mass];
    rowExpression = row[[-1]];
    contour = If[FreeQ[rowExpression, ConstructiveDiagrams`z], 1, ConstructiveDiagrams`z];
    numerator = Cancel[contour propagator rowExpression];
    If[!FreeQ[numerator, ConstructiveDiagrams`z | _MomHat | _SpinorHat] ||
        !residueFiniteQ[numerator],
      Failure["InvalidChannelExpression", <|
        "MessageTemplate" -> "Use unshifted channel expressions with at most the original 1/z contour factor."|>],
    (* Convert the sewn product to the same stripped-amplitude convention as
       diagramAmplitude: (i A_L)(i A_R)(i/PropDen)/i = -A_L A_R/PropDen. *)
    Join[channel, <|"Complete" -> True, "StateSum" -> -numerator,
      "SewingFactor" -> -1, "ChannelExpression" -> rowExpression,
      "ContourFactor" -> contour|>]]]
  ], channelRows];
  If[AnyTrue[targets, FailureQ], Return[SelectFirst[targets, FailureQ]]];
  particleMasses = DeleteDuplicates[model[[1, All, 6]]];
  Do[
    pairRules = First[pairs[[i]]];
    candidate = DeleteDuplicates[Cases[expressions[[i]], _PropDen, Infinity]];
    keys = DeleteMissing[residuePropagatorKey[#, external] & /@ candidate];
    candidate = residueSimpleChannel[First[#], Last[#], external, externalRules, model] & /@
      DeleteDuplicates[keys];
    AppendTo[records, <|"DiagramIndex" -> i, "MassRules" -> pairRules,
      "ExternalMassRules" -> externalRules, "OriginalExpression" -> Last[pairs[[i]]],
      "Expression" -> expressions[[i]], "PhysicalPropagators" -> candidate|>];
    Do[
      key = residueChannelKey[channel];
      If[!MemberQ[residueChannelKey /@ targets, key],
        AppendTo[targets, Join[channel,
          If[MemberQ[particleMasses, channel["Mass"]],
            <|"Complete" -> True, "StateSum" -> 0,
              "Diagnostics" -> {"This pole is absent from the supplied factorization channels; its expected residue is zero."}|>,
            <|"Complete" -> False, "StateSum" -> Missing["UnknownExchangeMass"],
              "Diagnostics" -> {"This propagator mass is absent from the supplied particle model."}|>]]]],
      {channel, candidate}],
    {i, Length[pairs]}];
  <|"Expression" -> Total[expressions], "Targets" -> targets, "Records" -> records|>
];

diagramPhysicalChannels[diagram_Association] := Module[{external, rules},
  external = Range[Length[diagram["ExternalParticles"]]];
  rules = diagramMassReplacements[diagram];
  KeyValueMap[Function[{edge, data}, Module[{legs},
    legs = residueMomentumLegs[data["Index"]];
    residueNormalizeChannel[<|"Cut" -> legs, "Complement" -> Complement[external, legs],
      "ExternalLegs" -> external, "ExternalParticles" -> diagram["ExternalParticles"],
      "Mass" -> data["Mass"],
      "InternalParticle" -> data["Particle"], "Spin2" -> Lookup[data, "Spin2", 0],
      "Edge" -> edge, "MomentumIndex" -> data["Index"],
      "ExternalMassRules" -> Select[rules, MatchQ[First[#], Mass[_Integer]] &]|>]
  ]], Lookup[diagram, "Propagators", <||>]]
];

(* ::Subsection::Closed:: *)
(*Diagram Amplitude Records*)

Options[DiagramAmplitudeRecords] = {"SimplificationFunction" -> Identity,
  "NumeratorOnShellChannels" -> {}};
diagramAmplitudeRecord[diagram_, index_, function_, declared_] := Module[
    {pair, transformed, channels, physicalDenominator, conditions, externalRules},
    pair = First[diagramAmplitudes[{diagram}]];
    transformed = withProtectedPropagators[Last[pair],
      Function[protected, function[{First[pair], protected}]]];
    If[!MatchQ[transformed, {_List, _}] || First[transformed] =!= First[pair],
      Return[Failure["InvalidSimplification", <|"DiagramIndex" -> index,
        "MessageTemplate" -> "The simplification function must retain the diagram's mass rules."|>]]];
    channels = diagramPhysicalChannels[diagram];
    physicalDenominator = Times @@ Lookup[channels, "Propagator", {}];
    externalRules = Select[First[pair], MatchQ[First[#], Mass[_Integer]] &];
    conditions = (PropDen[Mom[Multiparticle @@ #["Cut"]], #["Mass"]] == 0) & /@ channels;
    <|"DiagramIndex" -> index, "Diagram" -> diagram,
      "ExternalParticles" -> diagram["ExternalParticles"],
      "MassRules" -> First[pair], "ExternalMassRules" -> externalRules,
      "OriginalExpression" -> Last[pair], "Expression" -> Last[transformed],
      "OriginalNumerator" -> residueCanonicalPropagators[Last[pair],
        Range[Length[diagram["ExternalParticles"]]]] physicalDenominator,
      "Numerator" -> residueCanonicalPropagators[Last[transformed],
        Range[Length[diagram["ExternalParticles"]]]] physicalDenominator,
      "PhysicalDenominator" -> physicalDenominator,
      "PhysicalPropagators" -> channels,
      "ConstructionOnShellConditions" -> conditions,
      "DeclaredSimplificationOnShellChannels" -> declared,
      "NumeratorOnShellConditions" -> conditions,
      "DenominatorsKeptOffShell" -> True|>
];
DiagramAmplitudeRecords[diagrams_List, OptionsPattern[]] := Module[
  {function = OptionValue["SimplificationFunction"], declared = OptionValue["NumeratorOnShellChannels"]},
  If[!AllTrue[diagrams, AssociationQ[#] && KeyExistsQ[#, "ExternalParticles"] &&
      KeyExistsQ[#, "Propagators"] &] || !ListQ[declared],
    Message[DiagramAmplitudeRecords::args]; Return[$Failed]];
  MapIndexed[diagramAmplitudeRecord[#1, First[#2], function, declared] &, diagrams]
];

(* ::Subsection::Closed:: *)
(*Pole Restriction and Kinematic Checks*)

residueExactZero[expr_] := If[ListQ[expr], AllTrue[expr, residueExactZero],
  TrueQ[Cancel[Together[expr]] === 0]];
residueFiniteQ[expr_] := FreeQ[expr, Indeterminate | ComplexInfinity | _DirectedInfinity |
  _Missing | _Failure | _ConditionalExpression | $Failed | $Aborted | $TimedOut];
residueUnmappedQ[expr_] := !FreeQ[expr,
  _SpinorChain | _Spinor | _Mom | _MomProd | _PropDen | _Mass | _xFactor | _xTildeFactor |
  _Mandelstahm | _MandelstahmHat | _MomHat | _MomQ | _SpinorHat |
  _SpinorChainReduced | _SpinorTrace | _SpinorTraceReduced |
  _MomProdHat1 | _MomProdHat12 | _MomProdHat2 | _MomProdL | _MomProdR |
  _rSpinor | _SpinSpinorHat | _HelicitySpinorHat];

(* A normal-crossing tree residue must be regular after its pole factors
   have been removed. Do not substitute zero into a surviving denominator. *)
residueRegularRestriction[expr_, variables_List] := Module[{rational, value},
  rational = Cancel[Together[expr]];
  If[!PolynomialQ[Numerator[rational], variables] ||
      !PolynomialQ[Denominator[rational], variables], Return[Missing["NonRationalKinematics"]]];
  If[residueExactZero[Denominator[rational] /. Thread[variables -> 0]],
    Return[Missing["HigherOrderOrSingularPole"]]];
  value = Cancel[Together[rational /. Thread[variables -> 0]]];
  If[residueFiniteQ[value], value, Missing["HigherOrderOrSingularPole"]]
];

residueStructuralCoefficient[expr_, channels_List, variables_List] := Module[
  {external = First[channels]["ExternalLegs"], canonical},
  If[!residueFiniteQ[expr], Return[Missing["InvalidExpression"]]];
  canonical = residueCanonicalPropagators[expr, external] /.
    Thread[Lookup[channels, "Propagator"] -> variables];
  residueRegularRestriction[canonical Times @@ variables, variables]
];

residueKinematicChecks[map_, channels_List, variables_List, uncut_List] := Module[
  {external, massRules, coordinateChecks, shellChecks, conservation, uncutChecks,
   mapped, momenta, momentumTable, momentum, mappedMass, invariant},
  external = First[channels]["ExternalLegs"];
  massRules = Lookup[First[channels], "ExternalMassRules", {}];
  If[!AllTrue[external, !FreeQ[massRules, HoldPattern[Mass[#]] ] &],
    Return[<|"Valid" -> False, "Diagnostics" -> {"External masses must be supplied for every leg."}|>]];
  momenta = map[Mom[#], channels, variables] & /@ external;
  If[!AllTrue[momenta, MatrixQ[#] && Dimensions[#] === {2, 2} &&
      !residueUnmappedQ[#] && residueFiniteQ[#] &],
    Return[<|"Valid" -> False, "Diagnostics" -> {
      "KinematicMap must provide a finite two-by-two momentum matrix for every external leg."}|>]];
  momentumTable = AssociationThread[external, momenta];
  momentum[legs_] := Total[Lookup[momentumTable, legs]];
  mappedMass[mass_] := map[mass, channels, variables];
  invariant[legs_, mass_] := Det[momentum[legs]] - mappedMass[mass]^2;
  coordinateChecks = MapThread[Function[{channel, variable},
    AllTrue[{channel["Cut"], channel["Complement"]},
      residueExactZero[map[PropDen[Mom[Multiparticle @@ #], channel["Mass"]],
        channels, variables] - variable] &&
      residueExactZero[invariant[#, channel["Mass"]] - variable] &]], {channels, variables}];
  shellChecks = (residueExactZero[map[PropDen[Mom[#], Mass[#] /. massRules],
    channels, variables]] && residueExactZero[invariant[{#}, Mass[#] /. massRules]] &) /@ external;
  conservation = residueExactZero[Total[momenta]] &&
    residueExactZero[map[Total[Mom /@ external], channels, variables] - Total[momenta]];
  uncutChecks = Map[Function[channel,
    mapped = residueRegularRestriction[map[channel["Propagator"], channels, variables], variables];
    !MissingQ[mapped] && !residueExactZero[mapped] && !residueUnmappedQ[mapped] &&
      residueExactZero[map[channel["Propagator"], channels, variables] -
        invariant[channel["Cut"], channel["Mass"]]]], uncut];
  <|"Valid" -> (And @@ Join[coordinateChecks, shellChecks, {conservation}, uncutChecks]),
    "CutCoordinates" -> coordinateChecks, "ExternalMassShells" -> shellChecks,
    "MomentumConservation" -> conservation, "UncutChannelsRemainGeneric" -> uncutChecks,
    "MomentumMatricesValidated" -> True, "SpinorEvaluation" -> "SuppliedByKinematicMap",
    "Diagnostics" -> {}|>
];

residueOnChart[expr_, channels_List, variables_List, map_, active_List] := Module[{mapped},
  (* A sampled chart can land on an auxiliary denominator; reject it below
     without flooding the caller with expected singular-arithmetic messages. *)
  mapped = Quiet[map[expr, channels, variables], {Power::infy, Infinity::indet}];
  If[residueUnmappedQ[mapped] || !residueFiniteQ[mapped],
    Return[Missing["IncompleteKinematicMap"]]];
  residueRegularRestriction[mapped Times @@ variables[[active]], variables]
];

(* ::Subsection::Closed:: *)
(*Automatic Rational Cut Kinematics*)

residueAdjugate[p_] := {{p[[2, 2]], -p[[1, 2]]}, {-p[[2, 1]], p[[1, 1]]}};
residueRandomNonzero[] := RandomChoice[DeleteCases[Range[-7, 7], 0]];
residueRandomShell[mass_] := Module[{a, b, c},
  {a, b, c} = Table[residueRandomNonzero[], {3}];
  {{a, b}, {c, (mass^2 + b c)/a}}
];

(* Fixing the first row makes both two-body mass-shell equations linear
   in the second row. No square roots or process-specific kinematics occur. *)
residueSplitTwoBody[total_, firstMass_, secondMass_, transpose_] := Module[
  {r = If[TrueQ[transpose], Transpose[total], total], a, b, k, x, y, p},
  If[residueExactZero[r[[1, 1]]], Return[Failure["SingularChartPivot", <||>]]];
  {a, b} = Table[residueRandomNonzero[], {2}];
  If[residueExactZero[a r[[1, 2]] - b r[[1, 1]]],
    Return[Failure["SingularChartPivot", <||>]]];
  k = Det[r] + firstMass^2 - secondMass^2;
  x = Cancel[(firstMass^2 r[[1, 1]] -
      a (k - r[[2, 2]] a + r[[2, 1]] b))/(a r[[1, 2]] - b r[[1, 1]])];
  y = Cancel[(k - r[[2, 2]] a + r[[2, 1]] b + r[[1, 2]] x)/r[[1, 1]]];
  p = {{a, b}, {x, y}};
  If[TrueQ[transpose], p = Transpose[p]];
  {p, total - p}
];

residueSplitCutSide[legs_List, total_, masses_Association, transpose_] := Module[
  {chosen, remainder, pair},
  If[Length[legs] < 2, Return[Failure["CutNeedsTwoExternalLegs", <||>]]];
  chosen = residueRandomShell[masses[#]] & /@ Drop[legs, -2];
  remainder = total - If[chosen === {}, ConstantArray[0, {2, 2}], Total[chosen]];
  pair = residueSplitTwoBody[remainder, masses[legs[[-2]]], masses[Last[legs]], transpose];
  If[FailureQ[pair], pair, AssociationThread[legs, Join[chosen, pair]]]
];

residueChartMomentum[index_Integer, chart_] := Lookup[chart["Momenta"], index,
  Missing["UnknownExternalLeg", index]];
residueChartMomentum[Multiparticle[legs__], chart_] :=
  Total[residueChartMomentum[#, chart] & /@ {legs}];
residueChartMomentum[_, _] := Missing["UnsupportedMomentum"];
residueChartMomentumExpression[Mom[index_], chart_] := residueChartMomentum[index, chart];
residueChartMomentumExpression[sum_Plus, chart_] :=
  Total[residueChartMomentumExpression[#, chart] & /@ List @@ sum];
residueChartMomentumExpression[Times[c_?NumericQ, momentum_], chart_] :=
  c residueChartMomentumExpression[momentum, chart];
residueChartMomentumExpression[_, _] := Missing["UnsupportedMomentum"];

residueChartReference[kind_, index_, chart_] := Module[
  {hash, vectors, vector, trial, overrides, override,
   result = Missing["NoRegularReferenceSpinor"]},
  overrides = Lookup[chart, "ReferenceOverrides", <||>];
  override = Lookup[Lookup[overrides, kind, <||>], index, Missing["NoOverride"]];
  If[!MissingQ[override], Return[If[MatchQ[override, {_, _}] &&
      residueFiniteQ[override], override, Missing["InvalidReferenceOverride"]]]];
  hash = Hash[{chart["ReferenceSeed"], kind, index}, "CRC32"];
  vectors = Values[chart[kind]] /. chart["PoleVariable"] -> 0;
  If[!residueFiniteQ[vectors], Return[Missing["SingularExternalSpinorChart"]]];
  Do[
    vector = {1 + Mod[hash + 499 trial, 1009],
      1 + Mod[Quotient[hash, 1009] + 503 trial, 1013]};
    If[AllTrue[vectors, !residueExactZero[Det[{vector, #}]] &], result = vector; Break[]],
    {trial, 0, 15}];
  result
];
residueChartSpinor[Spinor["Xi", kind : ("Angle" | "Square"), index_, ___], chart_] :=
  residueChartReference[kind, index, chart];
residueChartSpinor[Spinor["Helicity", kind : ("Angle" | "Square"), index_Integer, ___], chart_] :=
  Lookup[chart[kind], index, Missing["UnknownExternalLeg", index]];
(* A cut photon has a rank-one momentum matrix. Assign its spinors only on
   that selected massless cut, never to an arbitrary off-shell composite. *)
residueChartSpinor[Spinor["Helicity", kind : ("Angle" | "Square"),
    index_Multiparticle, ___], chart_] := Module[{legs, momentum, pivot, row, column},
  legs = residueMomentumLegs[index];
  If[!TrueQ[Lookup[chart, "AllowCutSpinors", False]] ||
      !residueExactZero[Lookup[chart, "CutMass", Missing["NoCutMass"]]] ||
      !MemberQ[Lookup[chart, {"Cut", "Complement"}, {}], legs],
    Return[Missing["UnsupportedOffShellCompositeSpinor"]]];
  momentum = Cancel[Together[residueChartMomentum[index, chart] /.
      chart["PoleVariable"] -> 0]];
  If[!MatrixQ[momentum] || !residueFiniteQ[momentum] ||
      !residueExactZero[Det[momentum]], Return[Missing["InvalidMasslessCutMomentum"]]];
  pivot = SelectFirst[Tuples[Range[2], 2],
    !residueExactZero[Extract[momentum, #]] &, Missing["ZeroCutMomentum"]];
  If[MissingQ[pivot], Return[pivot]];
  {row, column} = pivot;
  If[kind === "Angle", momentum[[All, column]],
    momentum[[row, All]]/momentum[[row, column]]]
];
residueChartSpinor[Spinor["Spin", kind : ("Angle" | "Square"), index_Integer], chart_] :=
  Lookup[chart[kind], index, Missing["UnknownExternalLeg", index]];
residueChartSpinor[Spinor["Spin", position : ("Upper" | "Lower"),
    kind : ("Angle" | "Square"), index_Integer, spin : (-1/2 | 1/2)], chart_] :=
  Module[{basis = Lookup[chart[kind <> "Basis"], index, Missing["UnknownMassiveLeg"]], column},
    If[MissingQ[basis], Return[basis]];
    column = If[spin === -1/2, 1, 2];
    If[position === "Upper", basis[[All, column]],
      If[column === 1, basis[[All, 2]], -basis[[All, 1]]]]
  ];
(* Coherent evaluation uses one spin variable for both symmetric vector slots. *)
residueChartSpinor[Spinor["Spin", "Upper", kind : ("Angle" | "Square"),
    index_Integer, ConstructiveDiagrams`VectorSpinIndex[leg_Integer, 1 | 2]], chart_] /;
    TrueQ[Lookup[chart, "CoherentVectorSpinIndices", False]] && index === leg :=
  Lookup[chart[kind], index, Missing["UnknownMassiveLeg", index]];
residueChartSpinor[_, _] := Missing["UnsupportedSpinor"];

residueChartChain[chain_SpinorChain, chart_] := Module[
  {parts = List @@ chain, first, last, kind, row, matrices, endpoint, momentum, j, l},
  If[Length[parts] < 2 || Head[First[parts]] =!= Spinor || Head[Last[parts]] =!= Spinor,
    Return[Missing["UnsupportedSpinorChain"]]];
  first = First[parts]; last = Last[parts];
  kind = first[[If[Length[first] === 5, 3, 2]]];
  j = {{0, -1}, {1, 0}}; l = -j;
  row = residueChartSpinor[first, chart];
  endpoint = residueChartSpinor[last, chart];
  If[!FreeQ[{row, endpoint}, _Missing], Return[Missing["UnsupportedSpinor"]]];
  If[kind === "Angle", row = row.j];
  matrices = MapIndexed[Function[{insertion, position},
    momentum = residueChartMomentumExpression[insertion, chart];
    If[!MatrixQ[momentum], Missing["UnsupportedMomentum"],
      If[OddQ[First[position]] === (kind === "Angle"), momentum, residueAdjugate[momentum]]]],
    parts[[2 ;; -2]]];
  If[!FreeQ[matrices, _Missing], Return[Missing["UnsupportedMomentum"]]];
  Fold[Dot, row, matrices].If[last[[If[Length[last] === 5, 3, 2]]] === "Square",
    l.endpoint, endpoint]
];

residueChartTrace[trace_SpinorTrace, chart_] := Module[{matrices},
  If[OddQ[Length[trace]], Return[Missing["UnsupportedOddSpinorTrace"]]];
  matrices = MapIndexed[Function[{insertion, position},
    With[{p = residueChartMomentumExpression[insertion, chart]},
      If[MatrixQ[p], If[OddQ[First[position]], p, residueAdjugate[p]],
        Missing["UnsupportedMomentum"]]]], List @@ trace];
  If[!FreeQ[matrices, _Missing], Return[Missing["UnsupportedMomentum"]]];
  Tr[Fold[Dot, IdentityMatrix[2], matrices]]
];

residueChartValue[value_, tag_] := If[residueFiniteQ[value], value,
  Throw[Missing["UnsupportedKinematicStructure"], tag]];

residueExpandCutXFactors[expr_, chart_] := Module[{indices, expand},
  If[FreeQ[expr, _xFactor | _xTildeFactor], Return[expr]];
  indices = DeleteDuplicates[Cases[expr,
    Spinor["Helicity", _, index_Multiparticle, ___] :> index, Infinity]];
  expand[head_, i_, j_] := Module[{legs, index, sign, kind, other, reference,
      firstLegs, secondLegs, mass},
    firstLegs = residueMomentumLegs[i]; secondLegs = residueMomentumLegs[j];
    If[!ListQ[firstLegs] || !ListQ[secondLegs],
      Return[Missing["UnsupportedInternalXFactor"]]];
    legs = Sort[Join[firstLegs, secondLegs]];
    If[!TrueQ[Lookup[chart, "AllowCutSpinors", False]] ||
        !residueExactZero[Lookup[chart, "CutMass", Missing["NoCutMass"]]] ||
        !MemberQ[Lookup[chart, {"Cut", "Complement"}, {}], legs],
      Return[Missing["UnsupportedInternalXFactor"]]];
    index = SelectFirst[indices, MemberQ[{legs,
        Complement[Keys[chart["Momenta"]], legs]}, residueMomentumLegs[#]] &,
      Missing["MissingCutPhotonSpinor"]];
    If[MissingQ[index], Return[index]];
    sign = If[residueMomentumLegs[index] === legs, -1, 1];
    kind = If[head === xFactor, "Angle", "Square"];
    other = If[kind === "Angle", "Square", "Angle"];
    mass = Lookup[chart["Masses"], j, Missing["UnsupportedXFactorMass"]];
    If[MissingQ[mass] || residueExactZero[mass], Return[Missing["UnsupportedXFactorMass"]]];
    reference = Spinor["Xi", kind, "CutPhoton"];
    sign SpinorChain[reference, Mom[j], Spinor["Helicity", other, index]]/
      (mass SpinorChain[reference, Spinor["Helicity", kind, index]])
  ];
  expr /. {xFactor[i_, j_] :> expand[xFactor, i, j],
    xTildeFactor[i_, j_] :> expand[xTildeFactor, i, j]}
];
residueEvaluateChart[expr_, chart_, parameterRules_List] := Module[{mapped, momentum, tag = Unique[]},
  Catch[
  mapped = expr /. Mass[index_Integer] :> residueChartValue[
    Lookup[chart["Masses"], index, Missing["UnknownExternalMass", index]], tag];
  mapped = mapped /. parameterRules;
  mapped = residueChartValue[residueExpandCutXFactors[mapped, chart], tag];
  mapped = mapped /. chain_SpinorChain :> With[{value = residueChartChain[chain, chart]},
    If[!residueFiniteQ[value], Throw[Missing["UnsupportedSpinorChain"], tag], value]];
  mapped = mapped /. trace_SpinorTrace :> With[{value = residueChartTrace[trace, chart]},
    If[!residueFiniteQ[value], Throw[Missing["UnsupportedSpinorTrace"], tag], value]];
  (* MomentumSquare identities use Mom[label]^2 for the Lorentz invariant,
     not a componentwise square of the two-by-two momentum matrix. *)
  mapped = mapped /. HoldPattern[Power[p_Mom, exponent_Integer?EvenQ]] :>
    With[{matrix = residueChartMomentumExpression[p, chart]},
      If[MatrixQ[matrix], Det[matrix]^(exponent/2),
        Throw[Missing["UnsupportedMomentum"], tag]]];
  mapped = mapped /. {
    PropDen[p_, mass_] :> With[{matrix = residueChartMomentumExpression[p, chart]},
      If[MatrixQ[matrix], Det[matrix] - mass^2, Throw[Missing["UnsupportedMomentum"], tag]]],
    MomProd[i_, j_] :> With[{p = residueChartMomentum[i, chart], q = residueChartMomentum[j, chart]},
      If[MatrixQ[p] && MatrixQ[q], (Det[p + q] - Det[p] - Det[q])/2,
        Throw[Missing["UnsupportedMomentum"], tag]]],
    Mandelstahm[i_, j_] :> With[{p = residueChartMomentum[i, chart], q = residueChartMomentum[j, chart]},
      If[MatrixQ[p] && MatrixQ[q], Det[p + q], Throw[Missing["UnsupportedMomentum"], tag]]]};
  mapped = mapped /. Mom[index_] :> residueChartValue[residueChartMomentum[index, chart], tag];
  mapped /. parameterRules, tag]
];

residueAutomaticChart[channel_Association, sample_Integer, seed_Integer, parameterRules_List] :=
  BlockRandom[Module[{external, cut, complement, massRules, masses, pole, cutMomentum,
    left, right, momenta, angle, square, angleBasis, squareBasis, basis, references,
    vector, mass, p, chart, spinVectors, masslessScales, scale, attempt,
    result = Failure["NoRegularRationalChart", <||>], branches},
    external = channel["ExternalLegs"]; cut = channel["Cut"];
    complement = channel["Complement"];
    massRules = Lookup[channel, "ExternalMassRules", {}];
    masses = AssociationThread[external, (Mass /@ external /. massRules) /. parameterRules];
    If[!FreeQ[Values[masses], _Mass] || Min[Length[cut], Length[complement]] < 2,
      Return[Failure["InvalidCutMassMetadata", <||>]]];
    (* Storage scopes and parameter names must not change sampled kinematics. *)
    SeedRandom[Hash[{seed, sample, cut, channel["Mass"] /. parameterRules}, "CRC32"]];
    pole = Unique["automaticCut"];
    branches = {OddQ[sample], OddQ[Quotient[sample - 1, 2]]};
    Do[
      cutMomentum = residueRandomShell[Sqrt[(channel["Mass"] /. parameterRules)^2 + pole]];
      left = residueSplitCutSide[cut, cutMomentum, masses, branches[[1]]];
      right = residueSplitCutSide[complement, -cutMomentum, masses, branches[[2]]];
      If[FailureQ[left] || FailureQ[right], Continue[]];
      momenta = Join[left, right];
      angle = <||>; square = <||>; angleBasis = <||>; squareBasis = <||>; spinVectors = <||>;
      masslessScales = <||>;
      Do[
        p = momenta[leg]; mass = masses[leg];
        If[residueExactZero[mass],
          If[residueExactZero[p[[1, 1]]], Break[]];
          scale = residueRandomNonzero[]/residueRandomNonzero[];
          AssociateTo[masslessScales, leg -> scale];
          AssociateTo[angle, leg -> (scale p[[All, 1]])];
          AssociateTo[square, leg -> ({1, p[[1, 2]]/p[[1, 1]]}/scale)],
          basis = residueRandomShell[Sqrt[mass]];
          vector = Table[residueRandomNonzero[], {2}];
          AssociateTo[spinVectors, leg -> vector];
          AssociateTo[angleBasis, leg -> basis];
          AssociateTo[squareBasis, leg -> ({{0, 1}, {-1, 0}}.residueAdjugate[p].basis/mass)];
          AssociateTo[angle, leg -> (basis.vector)];
          AssociateTo[square, leg -> (squareBasis[leg].vector)]
        ], {leg, external}];
      If[Length[angle] =!= Length[external], Continue[]];
      references = <|"Angle" -> Table[residueRandomNonzero[], {2}],
        "Square" -> Table[residueRandomNonzero[], {2}]|>;
      chart = <|"Momenta" -> momenta, "Angle" -> angle, "Square" -> square,
        "AngleBasis" -> angleBasis, "SquareBasis" -> squareBasis,
        "References" -> references, "Masses" -> masses, "PoleVariable" -> pole,
        "ExternalSpinVectors" -> spinVectors, "MasslessBranchChoices" -> branches,
        "MasslessLittleGroupScales" -> masslessScales,
        "Cut" -> cut, "Complement" -> complement,
        "CutMass" -> (channel["Mass"] /. parameterRules),
        "AllowCutSpinors" -> True,
        "ReferenceSeed" -> RandomInteger[{1, 10^9}]|>;
      If[!AllTrue[Values[momenta],
          residueFiniteQ[Cancel[Together[# /. pole -> 0]]] &], Continue[]];
      result = With[{saved = chart, originalPole = pole, rules = parameterRules},
        <|"Map" -> Function[{expression, channels, variables},
            If[Length[channels] =!= 1 || Length[variables] =!= 1,
              Missing["AutomaticMultipleCutChartNotImplemented"],
              residueEvaluateChart[expression, saved /. originalPole -> First[variables], rules]]],
          "Chart" -> saved, "ParameterRules" -> rules,
          "Method" -> "AutomaticRationalCutChart", "Scope" -> "ExactRationalSample",
          "Sample" -> sample, "RandomSeed" -> seed|>];
      Break[], {attempt, 12}];
    result
  ]];

(* ::Subsection::Closed:: *)
(*Single and Multiple Residues*)

Options[PhysicalPoleResidue] = {"KinematicMap" -> Automatic, "KinematicKind" -> "Sample",
  "TimeConstraint" -> 30, "UncutChannels" -> {}, "StructuralCoefficient" -> True};
PhysicalPoleResidue[expr_, channel_Association, opts : OptionsPattern[]] :=
  PhysicalPoleResidue[expr, {channel}, opts];
PhysicalPoleResidue[expr_, input_List, opts : OptionsPattern[]] := Module[
  {channels, variables, structural, map = OptionValue["KinematicMap"],
   kind = OptionValue["KinematicKind"], seconds = OptionValue["TimeConstraint"],
   uncut = OptionValue["UncutChannels"], structuralRequested = OptionValue["StructuralCoefficient"],
   result, signatures},
  If[input === {} || !AllTrue[input, residueChannelQ] ||
      !MemberQ[{"Sample", "Symbolic"}, kind] ||
      !(seconds === Infinity || TrueQ[NumericQ[seconds] && seconds > 0]) ||
      !ListQ[uncut] || !AllTrue[uncut, residueChannelQ] ||
      !MemberQ[{True, False}, structuralRequested] ||
      Complement[First /@ Flatten[{opts}], First /@ Options[PhysicalPoleResidue]] =!= {},
    Message[PhysicalPoleResidue::args]; Return[$Failed]];
  channels = residueNormalizeChannel /@ input;
  If[Length[DeleteDuplicates[residueChannelKey /@ channels]] =!= Length[channels] ||
      !(SameQ @@ (Sort /@ Lookup[Join[channels, uncut], "ExternalLegs"])),
    Message[PhysicalPoleResidue::args]; Return[$Failed]];
  signatures = DeleteMissing[residueExternalMassSignature /@ Join[channels, uncut]];
  If[Length[DeleteDuplicates[signatures]] > 1 || !residueSameProcessQ[Join[channels, uncut]],
    Message[PhysicalPoleResidue::args]; Return[$Failed]];
  uncut = Select[residueNormalizeChannel /@ uncut,
    !MemberQ[residueChannelKey /@ channels, residueChannelKey[#]] &];
  variables = Table[Unique["physicalPole"], {Length[channels]}];
  result = TimeConstrained[Module[{checks, value},
    structural = If[TrueQ[structuralRequested],
      residueStructuralCoefficient[expr, channels, variables], Missing["NotRequested"]];
    If[map === Automatic, Return[<|"Status" -> "FORMAL",
      "StructuralCoefficient" -> structural, "PhysicalResidue" -> Missing["KinematicsRequired"],
      "Channels" -> channels, "Method" -> "OpaquePropagatorCoefficient",
      "Diagnostics" -> {"No physical cut or numerator kinematics has been imposed."}|>]];
    checks = residueKinematicChecks[map, channels, variables, uncut];
    If[!TrueQ[checks["Valid"]], Return[<|"Status" -> "UNRESOLVED",
      "StructuralCoefficient" -> structural, "PhysicalResidue" -> Missing["InvalidKinematics"],
      "Channels" -> channels, "KinematicChecks" -> checks,
      "Diagnostics" -> {"The chart must preserve all external shells, momentum conservation and the selected pole coordinates without imposing extra known cuts."}|>]];
    value = residueOnChart[expr, channels, variables, map, Range[Length[channels]]];
    <|"Status" -> If[MissingQ[value], "UNRESOLVED", "PASS"],
      "StructuralCoefficient" -> structural, "PhysicalResidue" -> value,
      "Channels" -> channels, "KinematicChecks" -> checks,
      "Method" -> If[kind === "Symbolic", "ExactOnProvidedChart", "SampledChart"],
      "Scope" -> "ProvidedKinematicChart", "PoleVariables" -> variables,
      "Diagnostics" -> If[MissingQ[value], {value}, {}]|>
  ], seconds, <|"Status" -> "UNRESOLVED", "PhysicalResidue" -> Missing["Timeout"],
    "Diagnostics" -> {"The residue calculation exceeded TimeConstraint."}|>];
  result
];

(* ::Subsection::Closed:: *)
(*Channel Reports*)

modelParameterKeyQ[key_] := MatchQ[key, _Symbol | _ModelParameter];

modelParameterOverrideRules[overrides_List, keys_List] := Module[
  {result = {}, key, matches, name},
  Do[
    If[!MatchQ[rule, _Rule], Return[$Failed]];
    key = First[rule];
    If[StringQ[key] || Head[key] === Symbol,
      name = If[StringQ[key], key, SymbolName[key]];
      matches = Select[keys, Head[#] === ModelParameter && ModelParameterName[#] === name &];
      If[Length[matches] > 1, Return[$Failed]];
      If[matches =!= {}, key = First[matches]]];
    If[!modelParameterKeyQ[key], Return[$Failed]];
    AppendTo[result, key -> Last[rule]], {rule, overrides}];
  result
];

residueParameterRules[model_List, overrides_] := Module[
  {rows, definitions, pending, resolved = {}, available, supplied},
  If[!(overrides === Automatic || ListQ[overrides] && AllTrue[overrides,
      MatchQ[#, _Rule] && (modelParameterKeyQ[First[#]] || StringQ[First[#]]) &&
        residueFiniteQ[Last[#]] && !residueUnmappedQ[Last[#]] &]), Return[$Failed]];
  rows = If[Length[model] >= 4 && ListQ[model[[3]]] && ListQ[model[[4]]],
    Join[model[[4]], model[[3]]], {}];
  rows = Select[rows, ListQ[#] && Length[#] >= 2 && modelParameterKeyQ[First[#]] &];
  supplied = modelParameterOverrideRules[Replace[overrides, Automatic -> {}], First /@ rows];
  If[supplied === $Failed, Return[$Failed]];
  definitions = DeleteDuplicatesBy[Join[supplied,
    (First[#] -> #[[2]] & /@ rows)], First];
  (* Exactify before resolving dependencies so the cut charts retain exact shells.
     Only numeric definitions propagate; missing or cyclic ones stay unresolved. *)
  pending = Rationalize[definitions, 0];
  Do[
    available = Select[pending /. resolved,
      NumericQ[Last[#]] && residueFiniteQ[Last[#]] &];
    If[available === {}, Break[]];
    resolved = Join[resolved, available];
    pending = Select[pending, !MemberQ[First /@ available, First[#]] &],
    {Length[definitions]}];
  resolved
];

residueNumericMassesQ[channel_, rules_List] := AllTrue[Append[
  (Mass /@ channel["ExternalLegs"] /. Lookup[channel, "ExternalMassRules", {}]) /. rules,
  channel["Mass"] /. rules], NumericQ[#] && residueFiniteQ[#] &];

DownValues[CheckAmplitudes] = {};
Options[CheckAmplitudes] = Join[
  DeleteCases[Options[PhysicalPoleResidue], HoldPattern["StructuralCoefficient" -> _]], {
  "Tolerance" -> 10^-10, "Samples" -> 4, "RandomSeed" -> 1729,
  "ParameterRules" -> Automatic, "Output" -> "Table", "SpuriousPoles" -> Automatic,
  "HighEnergyCheck" -> Automatic, "HighEnergyBound" -> Automatic,
  "HighEnergySamples" -> 3, "HighEnergyTimeConstraint" -> Automatic,
  "ReferenceCheck" -> Automatic, "ReferenceSamples" -> 3,
  "ReferenceTimeConstraint" -> Automatic,
  "FeynmanAmplitudes" -> None, "FeynmanModel" -> Automatic, "FeynmanSamples" -> Automatic,
  "FeynmanTimeConstraint" -> Automatic}];
Options[CheckPhysicalResidues] = Options[CheckAmplitudes];

residueComparisonZeroQ[value_, kind_, tolerance_] := value === 0 ||
  (kind === "Sample" && NumericQ[value] && !FreeQ[value, _Real] &&
    residueFiniteQ[value] && TrueQ[Abs[N[value]] <= tolerance]);

residueComparisonBase[expr_, channel_, records_, targets_] := Module[
  {matching, indices, lower, expected},
  matching = Select[targets,
    residueChannelKey[residueNormalizeChannel[#]] === residueChannelKey[channel] &];
  indices = Lookup[Select[records, AnyTrue[#["PhysicalPropagators"],
    residueChannelKey[#] === residueChannelKey[channel] &] &], "DiagramIndex", {}];
  lower = Flatten[Lookup[matching, "LowerAmplitudes", {}], 1];
  expected = If[matching =!= {} && AllTrue[matching,
      TrueQ[Lookup[#, "Complete", False]] &&
        !MissingQ[Lookup[#, "StateSum", Missing["NoStateSum"]]] &],
    Total[Lookup[matching, "StateSum"]], Missing["IncompleteLowerAmplitudes"]];
  <|"Cut" -> channel["Cut"], "Complement" -> channel["Complement"],
    "Mass" -> channel["Mass"], "Propagator" -> channel["Propagator"],
    "CutCondition" -> (channel["Propagator"] == 0),
    "InternalParticles" -> DeleteDuplicates[Flatten[Join[
      Lookup[matching, "InternalParticle", {}], Lookup[matching, "InternalAntiparticle", {}]]]],
    "DiagramIndices" -> indices, "FactorizationChannels" -> matching,
    "LowerAmplitudes" -> lower, "ExpectedStateSum" -> expected,
    "CandidateExpression" -> expr|>
];

residueCheckChannel[expr_, channel_, records_, targets_, channels_, map_, kind_,
    seconds_, tolerance_, uncut_] := Module[
    {matching, base, calculation, value, residual, oppositeResidual, matchingSigns,
     expected, indices, variables, status},
    matching = Select[targets, residueChannelKey[residueNormalizeChannel[#]] === residueChannelKey[channel] &];
    indices = Lookup[Select[records, AnyTrue[#["PhysicalPropagators"],
      residueChannelKey[#] === residueChannelKey[channel] &] &], "DiagramIndex", {}];
    base = residueComparisonBase[expr, channel, records, targets];
    If[matching === {} || !AllTrue[matching, TrueQ[Lookup[#, "Complete", False]] &&
        !MissingQ[Lookup[#, "StateSum", Missing["NoStateSum"]]] &],
      Return[Join[base, <|"Status" -> "UNRESOLVED",
        "CalculatedResidue" -> Missing["NotCompared"], "ExpectedResidue" -> Missing["IncompleteLowerAmplitudes"],
        "Difference" -> Missing["NotCompared"],
        "Diagnostics" -> {"A physical channel is missing or its lower-amplitude calculation failed."}|>]]];
    expected = Total[Lookup[matching, "StateSum"]];
    calculation = PhysicalPoleResidue[expr, channel, "KinematicMap" -> map,
      "KinematicKind" -> kind, "TimeConstraint" -> seconds,
      "StructuralCoefficient" -> (map === Automatic),
      "UncutChannels" -> DeleteDuplicatesBy[Join[channels, uncut], residueChannelKey]];
    If[calculation === $Failed, Return[Join[base, <|"Status" -> "UNRESOLVED",
      "Diagnostics" -> {"Invalid residue options."}|>]]];
    If[calculation["Status"] === "FORMAL", Return[Join[base, <|"Status" -> "FORMAL",
      "CalculatedResidue" -> calculation["StructuralCoefficient"], "ExpectedResidue" -> expected,
      "Difference" -> Missing["KinematicsRequired"], "Method" -> calculation["Method"],
      "ResidueCalculation" -> calculation,
      "Diagnostics" -> calculation["Diagnostics"]|>]]];
    If[calculation["Status"] =!= "PASS", Return[Join[base, <|"Status" -> "UNRESOLVED",
      "CalculatedResidue" -> calculation["PhysicalResidue"], "ExpectedResidue" -> expected,
      "Difference" -> Missing["NotCompared"], "ResidueCalculation" -> calculation,
      "Diagnostics" -> calculation["Diagnostics"]|>]]];
    variables = calculation["PoleVariables"];
    value = TimeConstrained[residueOnChart[expected, calculation["Channels"], variables, map, {}],
      seconds, Missing["Timeout"]];
    If[MissingQ[value], Return[Join[base, <|"Status" -> "UNRESOLVED",
      "CalculatedResidue" -> calculation["PhysicalResidue"], "ExpectedResidue" -> value,
      "Difference" -> Missing["NotCompared"], "Diagnostics" -> {value}|>]]];
    residual = TimeConstrained[Cancel[Together[calculation["PhysicalResidue"] - value]],
      seconds, Missing["Timeout"]];
    oppositeResidual = TimeConstrained[Cancel[Together[calculation["PhysicalResidue"] + value]],
      seconds, Missing["Timeout"]];
    matchingSigns = Pick[{1, -1},
      residueComparisonZeroQ[#, kind, tolerance] & /@ {residual, oppositeResidual}];
    status = Which[MemberQ[matchingSigns, 1], "PASS",
      !residueFiniteQ[residual] || !residueFiniteQ[oppositeResidual], "UNRESOLVED",
      MemberQ[matchingSigns, -1], "Opp Sign",
      residueFiniteQ[residual] && (NumericQ[residual] || kind === "Symbolic"), "FAIL", True, "UNRESOLVED"];
    Join[base, <|"Status" -> status, "CalculatedResidue" -> calculation["PhysicalResidue"],
      "ExpectedResidue" -> value, "Difference" -> residual,
      "OppositeSignDifference" -> oppositeResidual, "MatchingSigns" -> matchingSigns,
      "CandidatePoleCoefficient" -> calculation["StructuralCoefficient"],
      "ResidueCalculation" -> calculation,
      "Method" -> calculation["Method"], "Scope" -> calculation["Scope"],
      "KinematicChecks" -> calculation["KinematicChecks"],
      "Diagnostics" -> Which[status === "Opp Sign",
        {"The calculated residue is the negative of the expected sewn residue in the diagram amplitude convention."},
        !residueFiniteQ[residual], {residual},
        !residueFiniteQ[oppositeResidual], {oppositeResidual}, True, {}]|>]
];

residueReportTable[rows_List] := Dataset[KeyTake[#, {"Cut", "Mass", "Status"}] & /@ rows];
residueReportResidues[rows_List] := Dataset[Flatten[Function[row,
  With[{samples = Lookup[row, "Samples", {}]},
    If[ListQ[samples] && samples =!= {},
      MapIndexed[Join[KeyTake[row, {"Cut", "Mass"}],
        <|"Sample" -> Lookup[#1, "SampleIndex", First[#2]],
          "Status" -> row["Status"]|>,
        KeyTake[#1, {"CalculatedResidue", "ExpectedResidue", "Difference"}]] &,
        samples],
      {Join[KeyTake[row, {"Cut", "Mass"}], <|"Sample" -> 1|>,
        KeyTake[row, {"Status", "CalculatedResidue", "ExpectedResidue", "Difference"}]]}]]
  ] /@ rows, 1], MaxItems -> {All, All}];

residueAutomaticComparison[expr_, channel_, records_, targets_, channels_,
    seconds_, tolerance_, uncut_, samples_, seed_, rules_] := Module[
  {base, results = {}, chart, result, status, sample, attempt, completed,
   resolved, matchingSigns},
  base = residueComparisonBase[expr, channel, records, targets];
  If[MissingQ[base["ExpectedStateSum"]], Return[Join[base, <|
    "Status" -> "UNRESOLVED", "CalculatedResidue" -> Missing["NotCompared"],
    "ExpectedResidue" -> base["ExpectedStateSum"], "Difference" -> Missing["NotCompared"],
    "Samples" -> {}, "Diagnostics" -> {
      "A physical channel is missing or its lower-amplitude calculation failed."}|>]]];
  If[!residueNumericMassesQ[channel, rules], Return[Join[base, <|
    "Status" -> "UNRESOLVED", "CalculatedResidue" -> Missing["UnspecifiedMassParameters"],
    "ExpectedResidue" -> Missing["UnspecifiedMassParameters"], "Difference" -> Missing["NotCompared"],
    "Samples" -> {}, "Diagnostics" -> {
      "Numerical external and cut masses are required. Supply model values or ParameterRules overrides."}|>]]];
  completed = TimeConstrained[
    Do[
      result = <|"Status" -> "UNRESOLVED", "Diagnostics" -> {"No regular cut chart was found."}|>;
      Do[
        chart = residueAutomaticChart[channel, sample + attempt - 1,
          seed + 104729 (attempt - 1), rules];
        If[FailureQ[chart],
          result = <|"Status" -> "UNRESOLVED", "Diagnostics" -> {chart}|>,
          result = residueCheckChannel[expr, channel, records, targets, channels,
            chart["Map"], "Sample", seconds, tolerance, uncut];
          result = Join[KeyDrop[result, {"CandidateExpression", "LowerAmplitudes",
              "FactorizationChannels", "ExpectedStateSum"}], <|
            "SampleIndex" -> sample, "ChartSampleIndex" -> chart["Sample"], "Attempt" -> attempt,
            "Kinematics" -> chart["Chart"], "ParameterRules" -> chart["ParameterRules"],
            "Method" -> "AutomaticRationalCutChart", "Scope" -> "ExactOnProvidedRationalSample"|>]
        ];
        If[MemberQ[{"PASS", "Opp Sign", "FAIL"}, result["Status"]], Break[]],
        {attempt, 12}];
      AppendTo[results, result],
      {sample, samples}]; True,
    seconds, False];
  resolved = Select[results, MemberQ[{"PASS", "Opp Sign"}, #["Status"]] &];
  (* A zero/zero sample allows both signs; nonzero samples must share one sign. *)
  matchingSigns = Fold[Intersection, {1, -1}, Lookup[resolved, "MatchingSigns", {}]];
  status = Which[AnyTrue[results, #["Status"] === "FAIL" &] || matchingSigns === {}, "FAIL",
    TrueQ[completed] && Length[results] === samples &&
      Length[resolved] === samples,
      If[MemberQ[matchingSigns, 1], "PASS", "Opp Sign"], True, "UNRESOLVED"];
  Join[base, <|"Status" -> status,
    "CalculatedResidue" -> Lookup[results, "CalculatedResidue", Missing["NotCompared"]],
    "ExpectedResidue" -> Lookup[results, "ExpectedResidue", Missing["NotCompared"]],
    "Difference" -> Lookup[results, "Difference", Missing["NotCompared"]],
    "OppositeSignDifference" -> Lookup[results, "OppositeSignDifference", Missing["NotCompared"]],
    "MatchingSigns" -> matchingSigns,
    "Samples" -> results, "RequestedSamples" -> samples, "CompletedSamples" -> Length[results],
    "Method" -> "AutomaticRationalCutCharts", "Scope" -> "SampledAgreementOnly",
    "Diagnostics" -> Join[If[TrueQ[completed], {}, {"The channel comparison exceeded TimeConstraint."}],
      Switch[status,
        "PASS", {"Agreement at the recorded exact samples is not a generic analytic proof."},
        "Opp Sign", {"At the recorded samples the calculated residue is consistently the negative of the expected sewn residue in the diagram amplitude convention. This is not a generic analytic proof."},
        "FAIL", Join[If[matchingSigns === {},
          {"The nonzero samples do not agree with one consistent sign."}, {}],
          Flatten[Lookup[Select[results, #["Status"] =!= "PASS" &], "Diagnostics", {}], 1]],
        _, Flatten[Lookup[Select[results, #["Status"] =!= "PASS" &], "Diagnostics", {}], 1]]]|>]
];

residueCheckReport[expr_, targets_List, records_List,
    opts : OptionsPattern[CheckAmplitudes]] := Module[
  {map = OptionValue[CheckAmplitudes, {opts}, "KinematicMap"],
   kind = OptionValue[CheckAmplitudes, {opts}, "KinematicKind"],
   seconds = OptionValue[CheckAmplitudes, {opts}, "TimeConstraint"],
   tolerance = OptionValue[CheckAmplitudes, {opts}, "Tolerance"],
   uncut = OptionValue[CheckAmplitudes, {opts}, "UncutChannels"],
   samples = OptionValue[CheckAmplitudes, {opts}, "Samples"],
   seed = OptionValue[CheckAmplitudes, {opts}, "RandomSeed"],
   rules = Replace[OptionValue[CheckAmplitudes, {opts}, "ParameterRules"], Automatic -> {}],
   physical, channels, allChannels,
   signatures, rows, unmatched, external},
  If[!ListQ[records] || !AllTrue[records, AssociationQ[#] &&
      ListQ[Lookup[#, "PhysicalPropagators", None]] &&
      AllTrue[#["PhysicalPropagators"], residueChannelQ] &] ||
      !AllTrue[targets, residueChannelQ] || !ListQ[uncut] || !AllTrue[uncut, residueChannelQ] ||
      !MemberQ[{"Sample", "Symbolic"}, kind] ||
      !(seconds === Infinity || TrueQ[NumericQ[seconds] && seconds > 0]) ||
      !TrueQ[NumericQ[tolerance] && 0 <= tolerance < Infinity] ||
      !IntegerQ[samples] || samples < 1 || !IntegerQ[seed] || !ListQ[rules] ||
      !AllTrue[rules, MatchQ[#, _Rule] && modelParameterKeyQ[First[#]] &&
        !residueUnmappedQ[Last[#]] &],
    Message[CheckAmplitudes::args]; Return[$Failed]];
  physical = Flatten[Lookup[records, "PhysicalPropagators", {}], 1];
  allChannels = Join[physical, targets, uncut];
  signatures = residueExternalMassSignature /@ allChannels;
  If[AnyTrue[signatures, MissingQ] || Length[DeleteDuplicates[signatures]] > 1 ||
      !residueSameProcessQ[Join[records, allChannels]],
    Message[CheckAmplitudes::args]; Return[$Failed]];
  channels = DeleteDuplicatesBy[residueNormalizeChannel /@ Join[physical, targets], residueChannelKey];
  rows = If[map === Automatic,
    residueAutomaticComparison[expr, #, records, targets, channels, seconds,
      tolerance, uncut, samples, seed, rules] & /@ channels,
    residueCheckChannel[expr, #, records, targets, channels, map, kind,
      seconds, tolerance, uncut] & /@ channels];
  external = If[allChannels === {}, {}, First[allChannels]["ExternalLegs"]];
  unmatched = Select[DeleteDuplicates[Cases[expr, _PropDen, Infinity]],
    !MemberQ[residueChannelKey /@ channels, residuePropagatorKey[#, external]] &];
  <|"Channels" -> rows, "Table" -> residueReportTable[rows],
    "Summary" -> Counts[Lookup[rows, "Status", {}]],
    "FactorizationChannels" -> targets, "ExpressionUnchanged" -> expr,
    "UnmatchedPropagators" -> unmatched,
    "CompatibleDoubleCuts" -> DeleteDuplicates[Flatten[
      Subsets[residueChannelKey /@ #["PhysicalPropagators"], {2}] & /@ records, 1]],
    "Scope" -> If[map === Automatic, "AutomaticExactRationalSamples", "ProvidedKinematicCharts"],
    "RandomSeed" -> seed, "ParameterRules" -> rules,
    "Diagnostics" -> Join[If[channels === {}, {"No factorization channels were supplied; no comparison was performed."}, {}],
      If[unmatched === {}, {}, {"Unmatched explicit propagators are listed separately; their physical or reference origin has not been inferred."}]]|>
];

CheckAmplitudes[amplitudes_, channelRows_List, model_List,
    opts : OptionsPattern[]] := Module[
  {output = OptionValue["Output"], factors = OptionValue["SpuriousPoles"],
   inputs, report, physical, spurious, combined, externalRules, external,
   automatic, targets, records, keys, discovery = {}, candidates, check,
   seconds = OptionValue["TimeConstraint"], energy, energyMetadata, energyRows,
   energyEnabled = OptionValue["HighEnergyCheck"],
   energyBound = OptionValue["HighEnergyBound"],
   energySamples = OptionValue["HighEnergySamples"],
   energySeconds = OptionValue["HighEnergyTimeConstraint"],
   reference, referenceMetadata, referenceChannels, referenceRows,
   referenceEnabled = OptionValue["ReferenceCheck"],
   referenceSamples = OptionValue["ReferenceSamples"],
   referenceSeconds = OptionValue["ReferenceTimeConstraint"],
   benchmark = OptionValue["FeynmanAmplitudes"], benchmarkReport,
   benchmarkModel = OptionValue["FeynmanModel"], benchmarkParameters,
   benchmarkSamples = OptionValue["FeynmanSamples"],
   benchmarkSeconds = OptionValue["FeynmanTimeConstraint"],
   parameters = residueParameterRules[model, OptionValue["ParameterRules"]], numericOptions},
  If[parameters === $Failed ||
      Complement[First /@ Flatten[{opts}], First /@ Options[CheckAmplitudes]] =!= {} ||
      !MemberQ[{"Table", "Residues", "Summary", "Details", "HighEnergy", "References", "Feynman"}, output] ||
      !(factors === Automatic || ListQ[factors]) ||
      !MemberQ[{Automatic, True, False}, energyEnabled] ||
      !(energyBound === Automatic || TrueQ[NumericQ[energyBound] &&
        Element[energyBound, Reals] && Abs[energyBound] < Infinity]) ||
      !IntegerQ[energySamples] || energySamples < 1 ||
      !(energySeconds === Automatic || energySeconds === Infinity ||
        TrueQ[NumericQ[energySeconds] && energySeconds > 0]) ||
      !MemberQ[{Automatic, True, False}, referenceEnabled] ||
      !IntegerQ[referenceSamples] || referenceSamples < 1 ||
      !(referenceSeconds === Automatic || referenceSeconds === Infinity ||
        TrueQ[NumericQ[referenceSeconds] && referenceSeconds > 0]) ||
      !(benchmarkSamples === Automatic || IntegerQ[benchmarkSamples] && benchmarkSamples > 0) ||
      !(benchmarkSeconds === Automatic || benchmarkSeconds === Infinity ||
        TrueQ[NumericQ[benchmarkSeconds] && benchmarkSeconds > 0]),
    Message[CheckAmplitudes::args]; Return[$Failed]];
  numericOptions = Join[{"ParameterRules" -> parameters}, {opts}];
  inputs = residueSimpleInputs[amplitudes, channelRows, model];
  If[FailureQ[inputs],
    Message[CheckAmplitudes::input, inputs["MessageTemplate"]]; Return[$Failed]];
  If[inputs === $Failed,
    Message[CheckAmplitudes::args]; Return[$Failed]];
  automatic = factors === Automatic;
  targets = inputs["Targets"]; records = inputs["Records"];
  If[automatic,
    (* Only supplied factorization channels certify physical poles.
       Candidate-only denominators are checked separately for finiteness. *)
    targets = Select[targets, KeyExistsQ[#, "ChannelExpression"] &];
    keys = residueChannelKey /@ targets;
    records = Map[Function[record, Join[record, <|"PhysicalPropagators" ->
      Select[record["PhysicalPropagators"], MemberQ[keys, residueChannelKey[#]] &]|>]], records]
  ];
  report = residueCheckReport[inputs["Expression"], targets, records, Sequence @@ numericOptions];
  If[report === $Failed, Return[$Failed]];
  If[automatic || factors =!= {},
    physical = DeleteDuplicatesBy[residueNormalizeChannel /@ Join[
      If[automatic, targets, inputs["Targets"]],
      If[automatic, {}, Flatten[Lookup[inputs["Records"], "PhysicalPropagators", {}], 1]],
      OptionValue["UncutChannels"]], residueChannelKey];
    If[automatic,
      discovery = TimeConstrained[
        poleDiscoverDenominators[inputs["Expression"], physical, inputs["Records"]],
        seconds, Missing["PoleDiscoveryTimeout"]];
      If[!ListQ[discovery], discovery = {<|"Factor" -> Missing["UndiscoveredDenominators"],
        "CanonicalFactor" -> Missing["UndiscoveredDenominators"],
        "Classification" -> "Unclassified", "Multiplicity" -> Missing["NotDetermined"],
        "PhysicalChannels" -> {}, "Diagnostics" -> {
          "Denominator discovery could not be completed within TimeConstraint."}|>}]
    ];
    If[physical === {} && inputs["Records"] =!= {},
      externalRules = Lookup[First[inputs["Records"]], "ExternalMassRules", {}];
      external = Sort[Cases[externalRules, HoldPattern[Rule[Mass[leg_Integer], _]] :> leg]];
      If[Length[external] >= 4,
        (* A process without physical poles still supplies external kinematics.
           This chart descriptor deliberately has no physical Propagator. *)
        physical = {<|"ExternalLegs" -> external, "ExternalMassRules" -> externalRules,
          "Cut" -> Take[external, 2], "Complement" -> Drop[external, 2], "Mass" -> 0,
          "KinematicMetadataOnly" -> True|>}]
    ];
    spurious = If[automatic,
      candidates = Select[discovery, #["Classification"] =!= "Physical" &];
      Map[Function[candidate,
        check = If[candidate["Classification"] === "Spurious",
          First[spuriousCheckReport[inputs["Expression"],
            {candidate["CanonicalFactor"]}, physical, Sequence @@ numericOptions]],
          <|"Type" -> "Unclassified", "Status" -> "UNRESOLVED", "Samples" -> {},
            "Diagnostics" -> Join[Lookup[candidate, "Diagnostics", {}], {
              "This denominator could not be classified reliably from the supplied physical channels."}]|>];
        Join[check, <|"Pole" -> candidate["Factor"],
          "CanonicalFactor" -> candidate["CanonicalFactor"], "Discovery" -> candidate|>]
      ], candidates],
      spuriousCheckReport[inputs["Expression"], factors, physical, Sequence @@ numericOptions]];
    combined = Join[
      (Join[#, <|"Type" -> "Physical", "Pole" -> #["CutCondition"]|>] & /@ report["Channels"]),
      (Join[#, <|"Pole" -> If[MissingQ[#["Pole"]], #["Pole"], #["Pole"] == 0]|>] & /@ spurious)];
    report = Join[report, <|"SpuriousChecks" -> spurious,
      "Checks" -> combined, "PoleDiscovery" -> discovery,
      "AutomaticPoleDiscovery" -> automatic,
      "PhysicalSummary" -> report["Summary"],
      "SpuriousSummary" -> Counts[Lookup[spurious, "Status", {}]],
      "Summary" -> Counts[Lookup[combined, "Status", {}]],
      "Table" -> Dataset[KeyTake[#, {"Type", "Pole", "Status"}] & /@ combined],
      "SpuriousTable" -> spuriousReportSamples[spurious],
      "Diagnostics" -> If[automatic, report["Diagnostics"] /. {
        "No factorization channels were supplied; no comparison was performed." ->
          "No physical factorization channels were supplied.",
        "Unmatched explicit propagators are listed separately; their physical or reference origin has not been inferred." ->
          "Denominators absent from the physical channels are classified in PoleDiscovery and checked separately for cancellation."},
        report["Diagnostics"]]|>]
  ];
  If[energyEnabled =!= False,
    energyMetadata = poleDiscoveryMetadata[targets, inputs["Records"]];
    energySeconds = Replace[energySeconds, Automatic -> seconds];
    energy = If[AssociationQ[energyMetadata],
      highEnergyCheckReport[inputs["Expression"], energyMetadata, energySamples,
        OptionValue["RandomSeed"], parameters, energySeconds, energyBound],
      <|"Type" -> "HighEnergy", "Status" -> "UNRESOLVED",
        "ObservedPower" -> Missing["NotDetermined"], "ExpectedPower" -> energyBound,
        "ObservedGrowth" -> "Not determined", "ExpectedGrowth" -> "External legs required",
        "Samples" -> {}, "Diagnostics" -> {
          "External mass rules are required to construct physical high-energy kinematics."}|>];
    energy = Join[energy, <|"Type" -> "HighEnergy", "Pole" -> "Fixed-angle high energy"|>];
    combined = Join[Lookup[report, "Checks",
      (Join[#, <|"Type" -> "Physical", "Pole" -> #["CutCondition"]|>] & /@ report["Channels"])],
      {energy}];
    energyRows = highEnergyReportRows[energy];
    report = Join[report, <|"HighEnergyCheck" -> energy,
      "HighEnergySummary" -> Counts[{energy["Status"]}],
      "HighEnergyTable" -> Dataset[energyRows, MaxItems -> {All, All}],
      "PhysicalSummary" -> Lookup[report, "PhysicalSummary", report["Summary"]],
      "Checks" -> combined, "Summary" -> Counts[Lookup[combined, "Status", {}]],
      "Table" -> Dataset[Map[Function[row, <|
        "Type" -> row["Type"], "Test" -> row["Pole"], "Status" -> row["Status"],
        "ObservedGrowth" -> Lookup[row, "ObservedGrowth", ""],
        "ExpectedGrowth" -> Lookup[row, "ExpectedGrowth", ""]|>], combined],
        MaxItems -> {All, All}]|>]
  ];
  If[referenceEnabled =!= False,
    referenceMetadata = poleDiscoveryMetadata[targets, inputs["Records"]];
    referenceChannels = DeleteDuplicatesBy[residueNormalizeChannel /@ Join[
      targets, Flatten[Lookup[inputs["Records"], "PhysicalPropagators", {}], 1],
      OptionValue["UncutChannels"]], residueChannelKey];
    referenceSeconds = Replace[referenceSeconds, Automatic -> seconds];
    reference = If[AssociationQ[referenceMetadata],
      referenceCheckReport[inputs["Expression"], referenceMetadata, referenceChannels,
        referenceSamples, OptionValue["RandomSeed"], parameters, referenceSeconds],
      <|"Type" -> "Reference", "Status" -> "UNRESOLVED", "References" -> {},
        "Samples" -> {}, "Diagnostics" -> {
          "External mass rules are required to construct physical reference-check kinematics."}|>];
    reference = Join[reference, <|"Type" -> "Reference", "Pole" ->
      If[Lookup[reference, "Scope", ""] === "NoExplicitReferences",
        "No explicit Xi dependence", "External reference spinors"]|>];
    combined = Join[Lookup[report, "Checks",
      (Join[#, <|"Type" -> "Physical", "Pole" -> #["CutCondition"]|>] & /@ report["Channels"])],
      {reference}];
    referenceRows = referenceReportRows[reference];
    report = Join[report, <|"ReferenceCheck" -> reference,
      "ReferenceSummary" -> Counts[{reference["Status"]}],
      "ReferenceTable" -> Dataset[referenceRows, MaxItems -> {All, All}],
      "PhysicalSummary" -> Lookup[report, "PhysicalSummary", report["Summary"]],
      "Checks" -> combined, "Summary" -> Counts[Lookup[combined, "Status", {}]],
      "Table" -> Dataset[Map[Function[row,
        Join[<|"Type" -> row["Type"], "Test" -> row["Pole"], "Status" -> row["Status"]|>,
          If[energyEnabled === False, <||>, <|
            "ObservedGrowth" -> Lookup[row, "ObservedGrowth", ""],
            "ExpectedGrowth" -> Lookup[row, "ExpectedGrowth", ""]|>]]], combined],
        MaxItems -> {All, All}]|>]
  ];
  If[benchmark =!= None,
    benchmarkSamples = Replace[benchmarkSamples, Automatic -> OptionValue["Samples"]];
    benchmarkSeconds = Replace[benchmarkSeconds, Automatic -> seconds];
    benchmarkParameters = Which[benchmarkModel === Automatic, parameters,
      MatchQ[benchmarkModel, FeynmanModel[_List, _List, _List, _List]],
        residueParameterRules[List @@ benchmarkModel, OptionValue["ParameterRules"]],
      MatchQ[benchmarkModel, {_List, _List, _List, _List}],
        residueParameterRules[benchmarkModel, OptionValue["ParameterRules"]],
      True, $Failed];
    benchmarkReport = amplitudeFeynmanReport[inputs, benchmark, model, benchmarkSamples,
      OptionValue["RandomSeed"], parameters, benchmarkParameters,
      benchmarkSeconds, OptionValue["Tolerance"]];
    combined = Join[Lookup[report, "Checks",
      (Join[#, <|"Type" -> "Physical", "Pole" -> #["CutCondition"]|>] & /@ report["Channels"])],
      {benchmarkReport}];
    report = Join[report, <|"FeynmanCheck" -> benchmarkReport,
      "FeynmanSummary" -> Counts[{benchmarkReport["Status"]}],
      "FeynmanTable" -> Dataset[amplitudeFeynmanRows[benchmarkReport], MaxItems -> {All, All}],
      "PhysicalSummary" -> Lookup[report, "PhysicalSummary", report["Summary"]],
      "Checks" -> combined, "Summary" -> Counts[Lookup[combined, "Status", {}]],
      "Table" -> Dataset[Map[Function[row,
        Join[<|"Type" -> row["Type"], "Test" -> row["Pole"], "Status" -> row["Status"]|>,
          If[energyEnabled === False, <||>, <|
            "ObservedGrowth" -> Lookup[row, "ObservedGrowth", ""],
            "ExpectedGrowth" -> Lookup[row, "ExpectedGrowth", ""]|>]]], combined],
        MaxItems -> {All, All}]|>]
  ];
  Switch[output,
    "Table", report["Table"],
    "Residues", If[factors === {}, residueReportResidues[report["Channels"]],
      residueUnifiedSamples[report["Channels"], report["SpuriousChecks"]]],
    "Summary", report["Summary"],
    "HighEnergy", Lookup[report, "HighEnergyTable", Dataset[{}]],
    "References", Lookup[report, "ReferenceTable", Dataset[{}]],
    "Feynman", Lookup[report, "FeynmanTable", Dataset[{}]],
    "Details", report]
];
CheckAmplitudes[___] := (Message[CheckAmplitudes::args]; $Failed);
CheckPhysicalResidues[amplitudes_, channels_List, model_List, opts : OptionsPattern[]] :=
  CheckAmplitudes[amplitudes, channels, model,
    Sequence @@ Normal[Join[Association[Options[CheckPhysicalResidues]], Association[{opts}]]]];
CheckPhysicalResidues[args___] := CheckAmplitudes[args];

referenceReportRows[report_Association] := Module[{samples = Lookup[report, "Samples", {}]},
  If[samples === {}, Return[{KeyTake[report, {"Status", "References", "Scope", "Diagnostics"}]}]];
  Map[Function[sample, Join[<|
    "Sample" -> Lookup[sample, "Sample", Missing["NotRecorded"]],
    "Status" -> Lookup[sample, "Status", "UNRESOLVED"],
    "References" -> Lookup[report, "References", {}]|>,
    KeyTake[sample, {"Difference", "Diagnostics"}]]], samples]
];

highEnergyReportRows[report_Association] := Module[{samples = Lookup[report, "Samples", {}]},
  If[samples === {}, Return[{KeyTake[report,
    {"Status", "ObservedGrowth", "ExpectedGrowth", "Diagnostics"}]}]];
  Map[Function[sample, <|"Sample" -> Lookup[sample, "Sample", Missing["NotRecorded"]],
    "Status" -> Lookup[sample, "Status", "UNRESOLVED"],
    "ObservedGrowth" -> Lookup[sample, "ObservedGrowth", "Not determined"],
    "ExpectedGrowth" -> report["ExpectedGrowth"],
    "Diagnostics" -> Lookup[sample, "Diagnostics", {}]|>], samples]
];

(* ::Subsection::Closed:: *)
(*Apparent Denominator Discovery*)

poleKinematicFactorQ[expression_] := residueUnmappedQ[expression /. _Mass -> 1];

poleDiscoveryMetadata[channels_List, records_List] := Module[
  {candidates, rules, external, result = Missing["ExternalMassMetadata"]},
  candidates = Join[channels, records, Flatten[Lookup[records, "PhysicalPropagators", {}], 1]];
  Do[
    rules = Lookup[candidate, "ExternalMassRules", {}];
    external = Sort[Cases[rules, HoldPattern[Mass[i_Integer] -> _] :> i]];
    If[external =!= {} && DuplicateFreeQ[external],
      result = <|"ExternalLegs" -> external, "ExternalMassRules" -> rules|>; Break[]],
    {candidate, candidates}];
  result
];

(* Resolve rank-one external massless momenta only in denominator discovery.
   The full amplitude and every internal momentum remain unchanged. *)
poleExternalMasslessFactor[expression_, metadata_Association] := Module[
  {external = Lookup[metadata, "ExternalLegs", {}], rules, masslessQ, flip,
   factorChain},
  rules = Cases[Lookup[metadata, "ExternalMassRules", {}],
    rule : HoldPattern[Mass[_Integer] -> _] :> rule];
  masslessQ[leg_] := MemberQ[external, leg] && MemberQ[rules, Mass[leg] -> 0];
  flip[kind_] := If[kind === "Angle", "Square", "Angle"];
  factorChain[chain_SpinorChain] := Module[
    {parts = List @@ chain, left, right, middle, kind, current, factors = {}, leg},
    If[Length[parts] < 3, Return[chain]];
    left = First[parts]; right = Last[parts]; middle = Rest[Most[parts]];
    If[!AllTrue[{left, right}, MatchQ[#, Spinor["Xi" | "Helicity" | "Spin",
          "Angle" | "Square", _Integer]] &] ||
        !AllTrue[middle, MatchQ[#, Mom[_Integer]] && masslessQ[#[[1]]] &],
      Return[chain]];
    kind = left[[2]];
    If[right[[2]] =!= If[EvenQ[Length[middle]], kind, flip[kind]], Return[chain]];
    current = left;
    Do[
      leg = momentum[[1]];
      AppendTo[factors, SpinorChain[current, Spinor["Helicity", kind, leg]]];
      kind = flip[kind]; current = Spinor["Helicity", kind, leg],
      {momentum, middle}];
    AppendTo[factors, SpinorChain[current, right]];
    If[AnyTrue[factors, First[#] === Last[#] &], 0, Times @@ factors]
  ];
  expression /. {
    chain_SpinorChain :> factorChain[chain],
    trace : SpinorTrace[Mom[i_Integer], Mom[j_Integer]] /;
        masslessQ[i] && masslessQ[j] :> (ReduceSpinorProducts[trace] /. rules)}
];
poleExternalMasslessFactor[expression_, _] := expression;

(* Only external mass shells and momentum conservation enter this linear
   Gram representation. No diagram-local internal mass rules are used. *)
poleScalarForm[expression_, metadata_Association] := Module[
  {external = metadata["ExternalLegs"], masses, independent, dependent, pairs,
   pivot, relation, dot, indexDot, momentumDot, chainScalar, result},
  masses = AssociationThread[external, Mass /@ external /. metadata["ExternalMassRules"]];
  If[Length[external] < 3, Return[Missing["InsufficientExternalMetadata"]]];
  independent = Most[external]; dependent = Last[external];
  dot[i_, j_] := Which[
    i === dependent && j === dependent,
      Total[Flatten[Table[dot[a, b], {a, independent}, {b, independent}]]],
    i === dependent, -Total[dot[#, j] & /@ independent],
    j === dependent, -Total[dot[i, #] & /@ independent],
    i === j && KeyExistsQ[masses, i], masses[i]^2,
    ContainsAll[independent, {i, j}], poleGram @@ Sort[{i, j}],
    True, Missing["UnknownExternalMomentum"]];
  indexDot[i_, j_] := With[{left = residueMomentumLegs[i], right = residueMomentumLegs[j]},
    If[left === $Failed || right === $Failed, Missing["UnsupportedMomentumIndex"],
      Total[Flatten[Outer[dot, left, right]]]]];
  momentumDot[p_] := With[{legs = residueMomentumLegsFromExpression[p]},
    If[legs === $Failed, Missing["UnsupportedMomentumExpression"],
      Total[Flatten[Outer[dot, legs, legs]]]]];
  chainScalar[chain_SpinorChain] := Module[{first, last, middle, leg, momentum},
    If[Length[chain] =!= 3, Return[chain]];
    {first, middle, last} = List @@ chain;
    If[!MatchQ[first, Spinor["Helicity" | "Spin", "Angle" | "Square", _Integer]] ||
        !MatchQ[last, Spinor["Helicity" | "Spin", "Angle" | "Square", _Integer]] ||
        first[[3]] =!= last[[3]] || first[[2]] === last[[2]] ||
        !KeyExistsQ[masses, first[[3]]] || !residueExactZero[masses[first[[3]]]], Return[chain]];
    leg = first[[3]]; momentum = residueMomentumLegsFromExpression[middle];
    If[momentum === $Failed, chain, 2 Total[dot[leg, #] & /@ momentum]]
  ];
  result = expression /. chain_SpinorChain :> chainScalar[chain];
  result = result /. {
    MomProd[i_, j_] :> indexDot[i, j],
    Mandelstahm[i_, j_] :> indexDot[i, i] + indexDot[j, j] + 2 indexDot[i, j],
    PropDen[p_, mass_] :> momentumDot[p] - mass^2};
  result = result /. metadata["ExternalMassRules"];
  If[poleKinematicFactorQ[result] || !FreeQ[result, _Missing],
    Return[Missing["NotAScalarInvariant"]]];
  pairs = Subsets[independent, {2}];
  If[pairs =!= {},
    pivot = poleGram @@ Last[pairs];
    relation = (masses[dependent]^2 - Total[(masses[#]^2 &) /@ independent])/2 -
      Total[(poleGram @@ # &) /@ Most[pairs]];
    result = result /. pivot -> relation];
  Expand[result]
];
poleScalarForm[_, _] := Missing["ExternalMassMetadata"];

poleScalarProportionalQ[first_, second_] := Module[{ratio},
  If[MissingQ[first] || MissingQ[second] || residueExactZero[second], Return[False]];
  ratio = Cancel[Together[first/second]];
  FreeQ[ratio, _poleGram] && !residueExactZero[ratio]
];

poleFactorLegs[factor_] := DeleteDuplicates[Flatten[Join[
  Cases[factor, HoldPattern[MomProd[i_, j_]] :>
    {residueMomentumLegs[i], residueMomentumLegs[j]}, {0, Infinity}],
  Cases[factor, HoldPattern[Mandelstahm[i_, j_]] :>
    {residueMomentumLegs[i], residueMomentumLegs[j]}, {0, Infinity}],
  Cases[factor, HoldPattern[Mom[i_]] :> residueMomentumLegs[i], {0, Infinity}],
  Cases[factor, HoldPattern[Spinor[_, _, i_Integer]] :> {i}, {0, Infinity}]]]];

poleInferredScalarPropagator[factor_, scalar_, metadata_Association] := Module[
  {external = metadata["ExternalLegs"], legs, invariant, variables, variable,
   scale, constant, massSquared, mass},
  If[MissingQ[scalar] || residueExactZero[scalar], Return[Missing["NoScalarPole"]]];
  legs = Sort[poleFactorLegs[factor]];
  If[MemberQ[legs, $Failed] || !ContainsAll[external, legs] ||
      Min[Length[legs], Length[Complement[external, legs]]] < 2,
    Return[Missing["NoSingleInvariantCut"]]];
  legs = canonicalPhysicalCut[legs, external];
  invariant = poleScalarForm[PropDen[Mom[Multiparticle @@ legs], 0], metadata];
  If[MissingQ[invariant], Return[invariant]];
  variables = DeleteDuplicates[Cases[invariant, _poleGram, {0, Infinity}]];
  variable = SelectFirst[variables, !residueExactZero[Coefficient[invariant, #]] &,
    Missing["ConstantInvariant"]];
  If[MissingQ[variable], Return[variable]];
  scale = Cancel[Together[Coefficient[scalar, variable]/Coefficient[invariant, variable]]];
  If[!FreeQ[scale, _poleGram] || residueExactZero[scale], Return[Missing["NotAffineInvariant"]]];
  constant = Cancel[Together[scalar - scale invariant]];
  If[!FreeQ[constant, _poleGram], Return[Missing["NotAffineInvariant"]]];
  massSquared = Cancel[Together[-constant/scale]];
  mass = If[MatchQ[massSquared, Power[_, 2]], First[massSquared], Sqrt[massSquared]];
  PropDen[Mom[Multiparticle @@ legs], mass]
];
poleInferredScalarPropagator[_, _, _] := Missing["ExternalMassMetadata"];

poleClassifyFactor[factor_, power_, channels_List, metadata_] := Module[
  {canonical = factor, classification = "Unclassified", matches = {}, diagnostics = {},
   scalar, channelScalar, key, parts, pair, masses, inferred},
  scalar = poleScalarForm[factor, metadata];
  If[MissingQ[scalar] && TrueQ[poleExternalMasslessFactor[factor, metadata] === 0],
    scalar = 0];
  matches = Select[channels, Function[channel,
    poleScalarProportionalQ[scalar, poleScalarForm[channel["Propagator"], metadata]]]];
  If[matches =!= {},
    canonical = First[matches]["Propagator"]; classification = "Physical",
    Which[
      Head[factor] === PropDen && AssociationQ[metadata],
        key = residuePropagatorKey[factor, metadata["ExternalLegs"]];
        If[!MissingQ[key], canonical = PropDen[Mom[Multiparticle @@ First[key]], Last[key]];
          classification = "Spurious"],
      MatchQ[factor, SpinorChain[_Spinor, _Spinor]],
        parts = List @@ factor;
        If[AllTrue[parts, MatchQ[#, Spinor["Xi" | "Helicity" | "Spin",
              "Angle" | "Square", _Integer]] &] && parts[[1, 2]] === parts[[2, 2]],
          canonical = SpinorChain @@ Sort[parts]; classification = "Spurious";
          If[AssociationQ[metadata] && FreeQ[parts, Spinor["Xi", __]],
            pair = Sort[#[[3]] & /@ parts];
            masses = Mass /@ pair /. metadata["ExternalMassRules"];
            If[DuplicateFreeQ[pair] && ContainsAll[metadata["ExternalLegs"], pair] &&
                AllTrue[masses, residueExactZero],
              key = canonicalPhysicalCut[pair, metadata["ExternalLegs"]];
              matches = Select[channels, #["Cut"] === key && residueExactZero[#["Mass"]] &];
              If[matches =!= {}, classification = "Physical"]]]],
      !MissingQ[scalar],
        inferred = poleInferredScalarPropagator[factor, scalar, metadata];
        If[!MissingQ[inferred], canonical = inferred; classification = "Spurious"]
    ]
  ];
  If[!IntegerQ[power], classification = "Unclassified";
    AppendTo[diagnostics, "A noninteger or symbolic denominator power requires a branch-sensitive check."]];
  If[!MissingQ[scalar] && residueExactZero[scalar], classification = "Unclassified";
    AppendTo[diagnostics, "This denominator vanishes identically under the external mass shells and momentum conservation."]];
  If[classification === "Unclassified" && diagnostics === {},
    diagnostics = {"This apparent denominator could not be identified with a supplied physical channel or a supported spurious-pole factor."}];
  <|"Factor" -> factor, "CanonicalFactor" -> canonical,
    "Classification" -> classification, "Multiplicity" -> power,
    "PhysicalChannels" -> (residueChannelKey /@ matches), "Diagnostics" -> diagnostics|>
];

poleDiscoverDenominators[expr_, channels_List, records_List] := Module[
  {physical, metadata, scanExpressions, denominators, pieces, rows, groups,
   factorPieces, candidateFactorQ},
  physical = residueNormalizeChannel /@ Select[channels,
    residueChannelQ[#] && !TrueQ[Lookup[#, "KinematicMetadataOnly", False]] &];
  metadata = poleDiscoveryMetadata[channels, records];
  candidateFactorQ[factor_] := poleKinematicFactorQ[factor] ||
    (AssociationQ[metadata] && !FreeQ[factor, Mass[_Integer]] &&
      residueExactZero[factor /. metadata["ExternalMassRules"]]);
  scanExpressions = DeleteDuplicates[Join[{expr}, Flatten[Function[record,
    {Lookup[record, "Expression", 0], Lookup[record, "OriginalExpression", 0]} /.
      Lookup[record, "MassRules", {}]] /@ records, 1]]];
  denominators = DeleteDuplicates[Join[Cases[scanExpressions, Power[base_, exponent_] /;
      candidateFactorQ[base] &&
      !(NumericQ[exponent] && TrueQ[exponent >= 0]) :> {base, -exponent}, {0, Infinity}],
    Cases[scanExpressions, reciprocal : ((Csc | Sec | Cot | Csch | Sech | Coth)[argument_]) /;
        poleKinematicFactorQ[argument] :>
      {Switch[Head[reciprocal], Csc, Sin[argument], Sec, Cos[argument], Cot, Tan[argument],
        Csch, Sinh[argument], Sech, Cosh[argument], Coth, Tanh[argument]], 1}, {0, Infinity}]]];
  factorPieces[base_, power_] := Module[{factored, factors, transformed},
    transformed = If[IntegerQ[power], poleExternalMasslessFactor[base, metadata], base];
    factored = If[IntegerQ[power],
      Quiet[Check[TimeConstrained[Factor[transformed, Extension -> Automatic], 2,
        transformed], transformed]], transformed];
    If[TrueQ[factored === 0], Return[{{base, power}}]];
    factors = If[Head[factored] === Times, List @@ factored, {factored}];
    Select[Map[Function[factor, If[Head[factor] === Power,
      {factor[[1]], power factor[[2]]}, {factor, power}]],
      Select[factors, candidateFactorQ]],
      !(NumericQ[Last[#]] && TrueQ[Last[#] <= 0]) &]
  ];
  pieces = DeleteDuplicates[Flatten[(factorPieces @@ # &) /@ denominators, 1]];
  pieces = Select[pieces, Function[piece,
    With[{scalar = poleScalarForm[First[piece], metadata]},
      MissingQ[scalar] || !FreeQ[scalar, _poleGram] || residueExactZero[scalar]]]];
  rows = (poleClassifyFactor[#[[1]], #[[2]], physical, metadata] &) /@ pieces;
  groups = GatherBy[rows, #["CanonicalFactor"] &];
  Map[Function[group, Join[First[group], <|
    "Multiplicity" -> If[AllTrue[Lookup[group, "Multiplicity"], NumericQ],
      Max[Lookup[group, "Multiplicity"]], Missing["SymbolicPower"]],
    "ApparentForms" -> DeleteDuplicates[Lookup[group, "Factor"]],
    "Classification" -> If[MemberQ[Lookup[group, "Classification"], "Unclassified"],
      "Unclassified", First[group]["Classification"]],
    "Diagnostics" -> DeleteDuplicates[Flatten[Lookup[group, "Diagnostics"]]]|>]], groups]
];

(* ::Subsection::Closed:: *)
(*Spurious-Pole Finiteness Checks*)

residueUnifiedSamples[physical_List, spurious_List] := Module[{physicalRows, spuriousRows},
  physicalRows = Flatten[Map[Function[row, Module[{samples},
    samples = Lookup[row, "Samples", {}];
    If[samples === {}, samples = {row}];
    MapIndexed[Function[{sample, index}, <|"Type" -> "Physical",
      "Pole" -> row["CutCondition"], "Sample" -> Lookup[sample, "SampleIndex", First[index]],
      "Status" -> row["Status"],
      "Calculated" -> Lookup[sample, "CalculatedResidue", Missing["NotCompared"]],
      "Expected" -> Lookup[sample, "ExpectedResidue", Missing["NotCompared"]],
      "Difference" -> Lookup[sample, "Difference", Missing["NotCompared"]],
      "PoleOrder" -> 1, "Limit" -> Missing["NotApplicable"]|>], samples]
  ]], physical], 1];
  spuriousRows = Flatten[Map[Function[row, Module[{samples},
    samples = Lookup[row, "Samples", {}];
    If[samples === {}, samples = {<|"Status" -> "UNRESOLVED"|>}];
    MapIndexed[Function[{sample, index}, Module[{order, coefficient, resolved},
      order = Lookup[sample, "PoleOrder", Missing["NotCompared"]];
      resolved = MemberQ[{"PASS", "FAIL"}, Lookup[sample, "Status", "UNRESOLVED"]];
      coefficient = If[!resolved, Missing["NotCompared"],
        If[TrueQ[order > 0], Lookup[sample, "LeadingCoefficient", Missing["NotCompared"]], 0]];
      <|"Type" -> Lookup[row, "Type", "Spurious"],
        "Pole" -> If[MissingQ[row["Pole"]], row["Pole"], row["Pole"] == 0],
        "Sample" -> Lookup[sample, "Sample", First[index]], "Status" -> row["Status"],
        "Calculated" -> coefficient, "Expected" -> 0, "Difference" -> coefficient,
        "PoleOrder" -> order, "Limit" -> Lookup[sample, "Limit", Missing["NotCompared"]]|>
    ]], samples]
  ]], spurious], 1];
  Dataset[Join[physicalRows, spuriousRows], MaxItems -> {All, All}]
];

spuriousReportSamples[rows_List] := Dataset[Flatten[Function[row,
  With[{samples = Lookup[row, "Samples", {}]},
    If[samples === {},
      {<|"Pole" -> (row["Pole"] == 0), "Sample" -> Missing["NotSampled"],
        "Status" -> row["Status"], "Limit" -> Missing["NotSampled"],
        "PoleOrder" -> Missing["NotSampled"]|>},
      (Join[<|"Pole" -> (row["Pole"] == 0)|>,
        KeyTake[#, {"Sample", "Status", "Limit", "PoleOrder"}]] & /@ samples)]]
  ] /@ rows, 1], MaxItems -> {All, All}];

spuriousLaurentData[value_, variable_] := Module[
  {rational, numerator, denominator, numeratorRules, denominatorRules,
   numeratorOrder, denominatorOrder, order, leading},
  If[!residueFiniteQ[value] || residueUnmappedQ[value],
    Return[Missing["IncompleteKinematicEvaluation"]]];
  rational = Cancel[Together[value], Extension -> Automatic];
  If[rational === 0, Return[<|"Order" -> Infinity, "LeadingCoefficient" -> 0,
    "Limit" -> 0|>]];
  numerator = Numerator[rational]; denominator = Denominator[rational];
  If[!PolynomialQ[numerator, variable] || !PolynomialQ[denominator, variable] ||
      !AllTrue[Join[CoefficientList[numerator, variable],
        CoefficientList[denominator, variable]], NumericQ],
    Return[Missing["NonRationalOrUnspecifiedParameters"]]];
  numeratorRules = Select[CoefficientRules[numerator, {variable}],
    !residueExactZero[Last[#]] &];
  denominatorRules = Select[CoefficientRules[denominator, {variable}],
    !residueExactZero[Last[#]] &];
  If[numeratorRules === {} || denominatorRules === {},
    Return[Missing["IndeterminateLaurentOrder"]]];
  numeratorOrder = Min[First[First[#]] & /@ numeratorRules];
  denominatorOrder = Min[First[First[#]] & /@ denominatorRules];
  order = numeratorOrder - denominatorOrder;
  leading = Cancel[Coefficient[numerator, variable, numeratorOrder]/
    Coefficient[denominator, variable, denominatorOrder], Extension -> Automatic];
  <|"Order" -> order, "LeadingCoefficient" -> leading,
    "Limit" -> Which[order > 0, 0, order === 0, leading,
      True, Missing["Divergent"]]|>
];

spuriousPoleSpecification[factor_, metadata_] := Module[
  {parts, first, last, kind, external, masses, reference, leg, pair, cut, mass,
   masslessPairSides},
  external = metadata["ExternalLegs"];
  masses = AssociationThread[external, Mass /@ external /.
    metadata["ExternalMassRules"]];
  If[MatchQ[factor, PropDen[_, _]],
    pair = residueMomentumLegsFromExpression[First[factor]];
    mass = Last[factor];
    If[pair === $Failed || !DuplicateFreeQ[pair] ||
        !ContainsAll[external, pair] || !NumericQ[mass],
      Return[Missing["UnsupportedInvariantSpuriousFactor"]]];
    cut = canonicalPhysicalCut[pair, external];
    If[Min[Length[cut], Length[Complement[external, cut]]] < 2,
      Return[Missing["InvariantCutNeedsTwoLegsPerSide"]]];
    masslessPairSides = Map[residueExactZero[mass] && Length[#] === 2 &&
      AllTrue[Lookup[masses, #], residueExactZero] &,
      {cut, Complement[external, cut]}];
    Return[<|"Kind" -> "Invariant", "Mode" -> "Invariant", "Cut" -> cut,
      "Mass" -> mass, "MasslessPairSides" -> masslessPairSides,
      "Propagator" -> PropDen[Mom[Multiparticle @@ cut], mass]|>]];
  If[!MatchQ[factor, SpinorChain[_Spinor, _Spinor]],
    Return[Missing["UnsupportedSpuriousFactor"]]];
  parts = List @@ factor; first = First[parts]; last = Last[parts];
  If[!AllTrue[parts, MatchQ[#, Spinor["Xi" | "Helicity" | "Spin",
        "Angle" | "Square", _Integer]] &] || first[[2]] =!= last[[2]],
    Return[Missing["UnsupportedSpuriousFactor"]]];
  kind = first[[2]];
  If[Count[parts, Spinor["Xi", __]] === 1,
    reference = SelectFirst[parts, First[#] === "Xi" &];
    leg = SelectFirst[parts, First[#] =!= "Xi" &][[3]];
    If[!MemberQ[external, leg], Return[Missing["UnknownExternalLeg"]]];
    Return[<|"Kind" -> kind, "Reference" -> reference[[3]], "Leg" -> leg,
      "Mode" -> "ReferenceBracket"|>]];
  If[!FreeQ[parts, Spinor["Xi", __]], Return[Missing["UnsupportedSpuriousFactor"]]];
  pair = Sort[#[[3]] & /@ parts];
  If[!DuplicateFreeQ[pair] || !ContainsAll[external, pair] ||
      !AllTrue[Lookup[masses, pair], residueExactZero],
    Return[Missing["ExternalPairMustBeMassless"]]];
  <|"Kind" -> kind, "Cut" -> pair, "Mode" -> "MasslessBracket"|>
];

spuriousPhysicalGenericQ[expr_, channels_, chart_, rules_] := Module[
  {propagators, values, variable = chart["PoleVariable"], data},
  (* A PropDen in the candidate may itself be the suspected spurious pole.
     Only supplied physical channels, not notation alone, identify guards. *)
  propagators = DeleteDuplicates[Lookup[Select[channels,
    !TrueQ[Lookup[#, "KinematicMetadataOnly", False]] &], "Propagator", {}]];
  values = residueEvaluateChart[#, chart, rules] & /@ propagators;
  AllTrue[values, Function[value,
    data = spuriousLaurentData[value, variable];
    AssociationQ[data] && data["Order"] === 0 &&
      !residueExactZero[data["Limit"]]]]
];

spuriousAutomaticChart[spec_, metadata_, sample_, seed_, rules_] := Module[
  {channel, chartResult, chart, variable, reference, perturbation, base, external,
   factor, factorData, conjugateData, attempts, pair, chartSample,
   found = Missing["NoChart"]},
  external = metadata["ExternalLegs"];
  If[Length[external] < 4, Return[Failure["NeedsAtLeastFourExternalLegs", <||>]]];
  If[spec["Mode"] === "Invariant",
    pair = spec["Cut"];
    channel = Join[metadata, <|"Cut" -> pair,
      "Complement" -> Complement[external, pair], "Mass" -> spec["Mass"],
      "Propagator" -> spec["Propagator"]|>];
    (* The second chart-orientation bit changes every two samples. When
       only the complement is a massless pair, vary that bit directly. *)
    chartSample = If[Lookup[spec, "MasslessPairSides", {}] === {False, True},
      2 sample - 1, sample];
    chartResult = residueAutomaticChart[channel, chartSample, seed, rules];
    If[FailureQ[chartResult], Return[chartResult]];
    Return[Join[chartResult["Chart"], <|"AllowCutSpinors" -> False|>]]
  ];
  If[spec["Mode"] === "MasslessBracket",
    pair = spec["Cut"];
    channel = Join[metadata, <|"Cut" -> pair,
      "Complement" -> Complement[external, pair], "Mass" -> 0,
      "Propagator" -> PropDen[Mom[Multiparticle @@ pair], 0]|>];
    (* The two complex collinear branches are different. Select the branch
       where the requested bracket, not its conjugate, vanishes linearly. *)
    Do[
      chartResult = residueAutomaticChart[channel, 2 sample + attempts, seed, rules];
      If[FailureQ[chartResult], Continue[]];
      chart = Join[chartResult["Chart"], <|"AllowCutSpinors" -> False|>];
      variable = chart["PoleVariable"];
      factor = SpinorChain @@ (Spinor["Helicity", spec["Kind"], #] & /@ pair);
      factorData = spuriousLaurentData[residueEvaluateChart[factor, chart, rules], variable];
      factor = SpinorChain @@ (Spinor["Helicity",
        If[spec["Kind"] === "Angle", "Square", "Angle"], #] & /@ pair);
      conjugateData = spuriousLaurentData[residueEvaluateChart[factor, chart, rules], variable];
      If[AssociationQ[factorData] && factorData["Order"] === 1 &&
          AssociationQ[conjugateData] && conjugateData["Order"] === 0,
        found = chart; Break[]],
      {attempts, 0, 7}];
    If[AssociationQ[found], Return[found]];
    Return[Failure["NoTransverseMasslessBracketChart", <||>]]
  ];
  channel = metadata;
  If[Min[Length[channel["Cut"]], Length[channel["Complement"]]] < 2,
    channel = Join[channel, <|"Cut" -> Take[external, 2],
      "Complement" -> Drop[external, 2], "Mass" -> 0|>]];
  chartResult = residueAutomaticChart[channel, sample, seed, rules];
  If[FailureQ[chartResult], Return[chartResult]];
  chart = chartResult["Chart"];
  (* Freeze the auxiliary physical-cut coordinate away from zero. Only the
     reference spinor is varied in this chart, never an internal mass shell. *)
  base = chart /. chart["PoleVariable"] -> (11 + sample);
  variable = Unique["referencePole"];
  reference = residueChartSpinor[Spinor["Spin", spec["Kind"], spec["Leg"]], base];
  If[!VectorQ[reference, NumericQ] || AllTrue[reference, residueExactZero],
    Return[Failure["InvalidExternalReferenceTarget", <||>]]];
  perturbation = If[!residueExactZero[reference[[1]]], {0, 1}, {1, 0}];
  Join[base, <|"PoleVariable" -> variable, "AllowCutSpinors" -> False,
    "ReferenceOverrides" -> Association[spec["Kind"] ->
      Association[spec["Reference"] -> reference + variable perturbation]]|>]
];

spuriousCheckOne[expr_, factor_, channels_, seconds_, samples_, seed_, rules_, map_] := Module[
  {base, metadata, specification, results = {}, completed, chart, variable,
   factorData, amplitudeData, result, status, attempt, coincident = False,
   effectiveSamples, minimumSamples, retryStride, branchDiagnostics = {}},
  base = <|"Type" -> "Spurious", "Pole" -> factor, "Status" -> "UNRESOLVED",
    "Scope" -> "SampledFinitenessOnly", "UserRequestedSamples" -> samples,
    "RequestedSamples" -> samples, "BranchCoverageDiagnostics" -> {}|>;
  If[map =!= Automatic, Return[Join[base, <|"Samples" -> {},
    "CompletedSamples" -> 0, "Diagnostics" -> {
      "Automatic spurious-pole charts do not reuse a supplied physical-cut KinematicMap."}|>]]];
  If[channels === {} || !AllTrue[channels, residueChannelQ] ||
      AnyTrue[residueExternalMassSignature /@ channels, MissingQ] ||
      Length[DeleteDuplicates[residueExternalMassSignature /@ channels]] =!= 1,
    Return[Join[base, <|"Samples" -> {}, "CompletedSamples" -> 0,
      "Diagnostics" -> {"Consistent external mass metadata is required for spurious-pole sampling."}|>]]];
  metadata = First[channels];
  specification = spuriousPoleSpecification[factor /. rules, Join[metadata,
    <|"ExternalMassRules" -> (metadata["ExternalMassRules"] /. rules)|>]];
  If[MissingQ[specification], Return[Join[base, <|"Samples" -> {},
    "CompletedSamples" -> 0, "Diagnostics" -> {
      "This spurious factor is unsupported; use a massless angle/square pair, an external/reference bracket, or a quadratic PropDen invariant."}|>]]];
  minimumSamples = If[specification["Mode"] === "Invariant",
    Switch[Count[specification["MasslessPairSides"], True], 1, 2, 2, 4, _, 1], 1];
  effectiveSamples = Max[samples, minimumSamples];
  If[effectiveSamples > samples,
    branchDiagnostics = {"Samples increased from " <> ToString[samples] <> " to " <>
      ToString[effectiveSamples] <> If[minimumSamples === 4,
        " to cover all four complex branch combinations of the two massless pairs.",
        " to cover both complex branches of the massless pair invariant."]}];
  base = Join[base, <|"RequestedSamples" -> effectiveSamples,
    "BranchCoverageDiagnostics" -> branchDiagnostics|>];
  (* Retrying a singular chart must not change the branch being tested. *)
  retryStride = If[specification["Mode"] === "Invariant",
    4 Ceiling[effectiveSamples/4], effectiveSamples];
  completed = Quiet[TimeConstrained[
    Do[
      result = Missing["NoRegularSample"];
      Do[
        chart = spuriousAutomaticChart[specification, metadata,
          sample + retryStride attempt, seed, rules];
        If[FailureQ[chart], Continue[]];
        variable = chart["PoleVariable"];
        factorData = spuriousLaurentData[residueEvaluateChart[factor, chart, rules], variable];
        If[!AssociationQ[factorData] || factorData["Order"] =!= 1, Continue[]];
        If[!spuriousPhysicalGenericQ[expr, channels, chart, rules],
          coincident = True; Continue[]];
        amplitudeData = spuriousLaurentData[residueEvaluateChart[expr, chart, rules], variable];
        If[!AssociationQ[amplitudeData], Continue[]];
        result = <|"Sample" -> sample, "Status" -> If[amplitudeData["Order"] < 0, "FAIL", "PASS"],
          "PoleOrder" -> Max[0, -amplitudeData["Order"]],
          "LaurentOrder" -> amplitudeData["Order"],
          "LeadingCoefficient" -> amplitudeData["LeadingCoefficient"],
          "Limit" -> amplitudeData["Limit"], "SampleValues" -> amplitudeData["Limit"],
          "Chart" -> chart|>;
        Break[], {attempt, 0, 5}];
      If[MissingQ[result], result = <|"Sample" -> sample, "Status" -> "UNRESOLVED"|>];
      AppendTo[results, result], {sample, effectiveSamples}]; True,
    seconds, False], {Power::infy, Infinity::indet}];
  status = Which[AnyTrue[results, #["Status"] === "FAIL" &], "FAIL",
    TrueQ[completed] && Length[results] === effectiveSamples &&
      AllTrue[results, #["Status"] === "PASS" &], "PASS", True, "UNRESOLVED"];
  Join[base, <|"Status" -> status, "Samples" -> results,
    "CompletedSamples" -> Length[results],
    "PoleOrder" -> If[results === {}, Missing["NotSampled"],
      Max[Lookup[results, "PoleOrder", 0]]],
    "Limit" -> Lookup[results, "Limit", Missing["NotSampled"]],
    "SampleValues" -> Lookup[results, "SampleValues", Missing["NotSampled"]],
    "Method" -> "ExactTransverseLaurentCharts",
    "Diagnostics" -> Join[
      branchDiagnostics,
      If[TrueQ[completed], {}, {"The spurious-pole check exceeded TimeConstraint."}],
      If[status === "PASS", {"No negative Laurent powers survive in the full summed amplitude at the recorded exact samples. This is not a generic analytic proof."}, {}],
      If[status === "FAIL", {"A negative Laurent power survives in the full summed amplitude at a recorded sample."}, {}],
      If[status === "UNRESOLVED" && coincident,
        {"A regular sample could not be obtained while keeping every physical propagator away from zero; the proposed pole may coincide with a physical pole."}, {}],
      If[status === "UNRESOLVED" && !coincident && TrueQ[completed],
        {"No complete finite rational Laurent evaluation was available for every requested sample; unsupported expression structures or unspecified parameters may remain."}, {}]]|>]
];

spuriousCheckReport[expr_, factors_List, targets_List,
    opts : OptionsPattern[CheckAmplitudes]] := Module[
  {seconds = OptionValue[CheckAmplitudes, {opts}, "TimeConstraint"],
   samples = OptionValue[CheckAmplitudes, {opts}, "Samples"],
   seed = OptionValue[CheckAmplitudes, {opts}, "RandomSeed"],
   rules = Replace[OptionValue[CheckAmplitudes, {opts}, "ParameterRules"], Automatic -> {}],
   map = OptionValue[CheckAmplitudes, {opts}, "KinematicMap"]},
  spuriousCheckOne[expr, #, targets, seconds, samples, seed, rules, map] & /@
    DeleteDuplicates[factors]
];

(* ::Subsection::Closed:: *)
(*High-Energy Diagnostics*)

highEnergyGrowthString[power_] := If[MissingQ[power], "Not determined",
  "E^(" <> ToString[power, InputForm] <> ")"];

(* The parameter t is positive, with energy scale 1/t^2. This avoids
   square-root branch choices at infinity in external wavefunctions. *)
highEnergyMomentumMatrix[energy_, {x_, y_, z_}] :=
  {{energy + z, x - I y}, {x + I y, energy - z}};

highEnergyDirectionBasis[vector_List] := Module[{r = Sqrt[vector.vector], x, y, z},
  {x, y, z} = vector;
  If[TrueQ[r + z == 0], {{0, -1}, {1, 0}},
    {{r + z, -x + I y}, {x + I y, r + z}}/Sqrt[2 r (r + z)]]
];

highEnergyZeroState[expression_] := Module[{reduced},
  reduced = Quiet[Check[RootReduce[expression], $Failed]];
  Which[reduced === 0, "Zero",
    reduced =!= $Failed && FreeQ[reduced, _Real] &&
      TrueQ[Element[reduced, Algebraics]], "Nonzero", True, "Unknown"]
];

(* Rational null seeds with a prescribed rational total invariant mass
   give generic three-dimensional COM directions with rational norms. *)
highEnergyRationalDirections[count_Integer] := Module[
  {vectors, energies, u, v, direction, weight, total, totalEnergy, invariant,
   totalMass, lastEnergy, boosted, boostedEnergies},
  vectors = {}; energies = {};
  Do[
    {u, v} = RandomInteger[{-5, 5}, 2]/RandomInteger[{1, 5}];
    direction = {2 u, 2 v, 1 - u^2 - v^2}/(1 + u^2 + v^2);
    weight = RandomInteger[{1, 5}];
    AppendTo[vectors, weight direction]; AppendTo[energies, weight],
    {count - 1}];
  total = Total[vectors]; totalEnergy = Total[energies];
  invariant = totalEnergy^2 - total.total;
  {u, v} = RandomInteger[{-5, 5}, 2]/RandomInteger[{1, 5}];
  direction = {2 u, 2 v, 1 - u^2 - v^2}/(1 + u^2 + v^2);
  If[TrueQ[totalEnergy - total.direction == 0],
    Return[Failure["CollinearNullSeeds", <||>]]];
  totalMass = Ceiling[Sqrt[invariant]] + RandomInteger[{1, 5}];
  lastEnergy = (totalMass^2 - invariant)/(2 (totalEnergy - total.direction));
  vectors = Append[vectors, lastEnergy direction];
  energies = Append[energies, lastEnergy];
  total = Total[vectors]; totalEnergy = Total[energies];
  boosted = MapThread[#1 + ((total.#1)/(totalMass (totalEnergy + totalMass)) -
      #2/totalMass) total &, {vectors, energies}];
  boostedEnergies = MapThread[(totalEnergy #1 - total.#2)/totalMass &,
    {energies, vectors}];
  {boosted, boostedEnergies}
];

highEnergyAutomaticChart[expr_, metadata_Association, sample_Integer,
    seed_Integer, parameterRules_List] := BlockRandom[Module[
  {external = Lookup[metadata, "ExternalLegs", {}], massRules, masses, t,
   outgoing, radii, leadingEnergies, leadingMomenta, generic, totalEnergy,
   firstEnergy, secondEnergy, beam, energies, spatial, momenta, angle = <||>,
   square = <||>, angleBasis = <||>, squareBasis = <||>, spinVectors = <||>,
   spinVariables = {}, vector, basis, radius, energy, mass, large, small,
   abasis, sbasis, coherent, crossing, index, references, referenceOverrides,
   validationMomenta, physicalChecks, attempt, directionData, validationParameter},
  If[!MatchQ[external, {__Integer}] || !DuplicateFreeQ[external] || Length[external] < 4,
    Return[Failure["UnsupportedMultiplicity", <||>]]];
  massRules = Lookup[metadata, "ExternalMassRules", {}];
  masses = Rationalize[(Mass /@ external /. massRules) /. parameterRules, 0];
  If[!AllTrue[masses, NumericQ[#] && TrueQ[Element[#, Reals]] && TrueQ[# >= 0] &],
    Return[Failure["NumericNonnegativeExternalMassesRequired", <||>]]];
  SeedRandom[Hash[{seed, sample, "HighEnergy"}, "CRC32"]];
  generic = False;
  Do[
    directionData = highEnergyRationalDirections[Length[external] - 2];
    If[FailureQ[directionData], Continue[]];
    {outgoing, radii} = directionData;
    leadingEnergies = Join[{Total[radii]/2, Total[radii]/2}, -radii];
    leadingMomenta = Join[{{0, 0, Total[radii]/2}, {0, 0, -Total[radii]/2}}, -outgoing];
    generic = AllTrue[Subsets[Range[Length[external]], {2, Floor[Length[external]/2]}],
      highEnergyZeroState[Total[leadingEnergies[[#]]]^2 -
        Total[leadingMomenta[[#]]].Total[leadingMomenta[[#]]]] === "Nonzero" &];
    If[generic, Break[]], {attempt, 1, 20}];
  If[!generic, Return[Failure["NoGenericFixedAngleDirections", <||>]]];
  t = Unique["inverseEnergyRoot"];
  energies = MapThread[Sqrt[#1^2 + #2^2 t^4] &, {radii, Drop[masses, 2]}];
  totalEnergy = Total[energies];
  firstEnergy = (totalEnergy^2 + (masses[[1]]^2 - masses[[2]]^2) t^4)/(2 totalEnergy);
  secondEnergy = totalEnergy - firstEnergy;
  beam = Sqrt[(totalEnergy^2 - (masses[[1]] + masses[[2]])^2 t^4)
    (totalEnergy^2 - (masses[[1]] - masses[[2]])^2 t^4)]/(2 totalEnergy);
  energies = Join[{firstEnergy, secondEnergy}, energies];
  spatial = Join[{{0, 0, beam}, {0, 0, -beam}}, outgoing];
  momenta = AssociationThread[external, MapIndexed[
    If[First[#2] <= 2, 1, -1] highEnergyMomentumMatrix[#1,
      spatial[[First[#2]]]]/t^2 &, energies]];
  Do[
    index = external[[position]]; mass = masses[[position]]; energy = energies[[position]];
    crossing = If[position <= 2, 1, -1];
    vector = If[position <= 2, {0, 0, If[position === 1, 1, -1]}, spatial[[position]]];
    basis = highEnergyDirectionBasis[vector];
    radius = If[position <= 2, beam, radii[[position - 2]]];
    large = Sqrt[energy + radius]/t;
    If[TrueQ[mass == 0],
      AssociateTo[angle, index -> (crossing large basis[[All, 1]])];
      AssociateTo[square, index -> (large Conjugate[basis[[All, 1]]])],
      small = mass t/Sqrt[energy + radius];
      abasis = basis.DiagonalMatrix[{large, small}];
      (* This is J.adjugate(P).A/m, with its small component already
         rationalized using the external mass-shell equation. *)
      sbasis = {{0, 1}, {-1, 0}}.basis.DiagonalMatrix[{small, large}];
      coherent = {1, Unique["externalSpin"]};
      AppendTo[spinVariables, Last[coherent]];
      AssociateTo[spinVectors, index -> coherent];
      AssociateTo[angleBasis, index -> (crossing abasis)];
      AssociateTo[squareBasis, index -> sbasis];
      AssociateTo[angle, index -> (crossing abasis.coherent)];
      AssociateTo[square, index -> (sbasis.coherent)]
    ], {position, Length[external]}];
  references = DeleteDuplicates[Cases[expr,
    Spinor["Xi", kind : ("Angle" | "Square"), reference_, ___] :> {kind, reference}, Infinity]];
  referenceOverrides = Association[Table[kind -> Association[Map[
      Function[reference, reference -> With[{h = Hash[{seed, sample, kind, reference}, "CRC32"]},
        {1 + Mod[h, 17], 1 + Mod[Quotient[h, 17], 19]}]],
      Cases[references, {kind, reference_} :> reference]]],
    {kind, {"Angle", "Square"}}]];
  (* Validate inside the real scattering region, including when the
     incoming rest masses exceed the unscaled outgoing energy. *)
  validationParameter = 1/(2 + Ceiling[Total[masses]]);
  validationMomenta = Values[momenta] /. t -> validationParameter;
  physicalChecks = <|"MomentumConservation" -> And @@
      (highEnergyZeroState[#] === "Zero" & /@ Flatten[Total[validationMomenta]]),
    "ExternalMassShells" -> MapThread[
      highEnergyZeroState[Det[#1] - #2^2] === "Zero" &, {validationMomenta, masses}],
    "PositiveFutureEnergies" -> AllTrue[energies /. t -> validationParameter,
      NumericQ[#] && TrueQ[# > 0] &],
    "ValidationParameter" -> validationParameter,
    "LeadingPairInvariantsNonzero" -> generic,
    "LeadingMultiparticleInvariantsNonzero" -> generic,
    "Construction" -> "ExactOnShellCenterOfMassFamily"|>;
  If[!TrueQ[physicalChecks["MomentumConservation"]] ||
      !And @@ physicalChecks["ExternalMassShells"] ||
      !TrueQ[physicalChecks["PositiveFutureEnergies"]],
    Return[Failure["InvalidHighEnergyKinematics", physicalChecks]]];
  <|"Momenta" -> momenta, "Masses" -> AssociationThread[external, masses],
    "Angle" -> angle, "Square" -> square, "AngleBasis" -> angleBasis,
    "SquareBasis" -> squareBasis, "ExternalSpinVectors" -> spinVectors,
    "SpinVariables" -> spinVariables, "ReferenceOverrides" -> referenceOverrides,
    "ReferenceSeed" -> seed, "PoleVariable" -> t, "AllowCutSpinors" -> False,
    "EnergyScale" -> 1/t^2, "OutgoingSpatialDirections" -> outgoing,
    "KinematicChecks" -> physicalChecks|>
]];

highEnergyOneSample[expr_, chart_Association, parameterRules_List, bound_] := Module[
  {t = chart["PoleVariable"], variables = chart["SpinVariables"], mapped, order,
   series, expanded, powers, coefficients, components = {}, state, power,
   component, observed, unresolved = False, lower = Missing["NotObserved"],
   upper, lowest, spinRules, establishedViolation = False},
  mapped = residueEvaluateChart[Rationalize[expr, 0], chart, Rationalize[parameterRules, 0]];
  If[!residueFiniteQ[mapped] || residueUnmappedQ[mapped],
    Return[<|"Status" -> "UNRESOLVED", "Diagnostics" -> {
      "The expression contains unsupported kinematic structures."}|>]];
  If[variables =!= {} && !PolynomialQ[mapped, variables], Return[<|"Status" -> "UNRESOLVED",
    "Diagnostics" -> {"Massive external spin dependence must be polynomial; spin-dependent denominators are unsupported."}|>]];
  order = Max[0, Ceiling[-2 bound]];
  series = Quiet[Check[Series[mapped, {t, 0, order}], $Failed]];
  If[series === $Failed || !FreeQ[series, _Series | _ConditionalExpression] ||
      !residueFiniteQ[series], Return[<|"Status" -> "UNRESOLVED",
    "Diagnostics" -> {"An exact Laurent expansion at high energy could not be obtained."}|>]];
  expanded = Expand[Normal[series]];
  If[!FreeQ[expanded, t] && !PolynomialQ[Expand[expanded t^Max[0, -Exponent[expanded, t, Min]]], t],
    Return[<|"Status" -> "UNRESOLVED", "Diagnostics" -> {
      "The high-energy expansion is not an integer Laurent series."}|>]];
  lowest = If[expanded === 0, order + 1, Exponent[expanded, t, Min]];
  If[!IntegerQ[lowest], Return[<|"Status" -> "UNRESOLVED", "Diagnostics" -> {
    "The high-energy expansion contains an unsupported noninteger power."}|>]];
  powers = Range[lowest, order];
  Do[
    coefficients = If[variables === {}, {{} -> Coefficient[expanded, t, power]},
      CoefficientRules[Coefficient[expanded, t, power], variables]];
    Do[
      state = highEnergyZeroState[Last[coefficient]];
      If[state === "Unknown", unresolved = True];
      If[state === "Nonzero",
        spinRules = Thread[variables -> First[coefficient]];
        AppendTo[components, <|"SpinMonomial" -> spinRules, "Power" -> -power/2,
          "LaurentCoefficient" -> RootReduce[Last[coefficient]]|>];
        If[TrueQ[-power/2 > bound], establishedViolation = True];
        If[MissingQ[lower] || -power/2 > lower, lower = -power/2]],
      {coefficient, coefficients}], {power, powers}];
  observed = Which[unresolved, Missing["UnresolvedCoefficients"],
    MissingQ[lower], Missing["BelowExpansionOrder"], True, lower];
  upper = Which[unresolved, Missing["UnresolvedCoefficients"],
    MissingQ[observed], -(order + 1)/2, True, observed];
  <|"Status" -> Which[establishedViolation, "FAIL",
      unresolved, "UNRESOLVED", True, "PASS"],
    "ObservedPower" -> observed, "ObservedPowerUpperBound" -> upper,
    "ObservedGrowth" -> Which[unresolved, "Not determined",
      MissingQ[observed], "At most " <> highEnergyGrowthString[upper],
      True, highEnergyGrowthString[observed]], "ExpectedPower" -> bound,
    "ExpectedGrowth" -> "At most " <> highEnergyGrowthString[bound],
    "Components" -> components, "ExpansionOrder" -> order,
    "CoefficientConvention" -> "UnnormalizedCoherentSpinPolynomial",
    "SpinVariables" -> variables, "Chart" -> chart,
    "Diagnostics" -> Join[
      If[establishedViolation, {"An exact spin-component coefficient grows faster than the allowed bound."}, {}],
      If[unresolved, {"Some Laurent coefficients could not be proven zero or nonzero exactly."}, {}]]|>
];

highEnergyCheckReport[expr_, metadata_Association, samples_Integer, seed_Integer,
    parameterRules_List, seconds_, requestedBound_] := Module[
  {external = Lookup[metadata, "ExternalLegs", {}], bound, base, results = {},
   chart, result, completed, status, observed, upper, diagnostics},
  bound = If[requestedBound === Automatic, 4 - Length[external], requestedBound];
  base = <|"Type" -> "HighEnergy", "Pole" -> "Fixed-angle high energy",
    "Status" -> "UNRESOLVED", "ExpectedPower" -> bound,
    "ExpectedGrowth" -> "At most " <> highEnergyGrowthString[bound],
    "ObservedPower" -> Missing["NotDetermined"],
    "ObservedGrowth" -> "Not determined", "Samples" -> {},
    "Method" -> "ExactFixedAngleLaurentExpansion",
    "Scope" -> "SampledDirectionsAllImplicitMassiveSpinComponents"|>;
  If[Length[external] < 4 || !NumericQ[bound] || !TrueQ[Element[bound, Reals]],
    Return[Join[base, <|"Diagnostics" -> {
      "High-energy checks require at least four external legs and a real numerical growth bound."}|>]]];
  completed = TimeConstrained[
    Do[
      chart = highEnergyAutomaticChart[expr, metadata, sample, seed, parameterRules];
      result = If[FailureQ[chart], <|"Status" -> "UNRESOLVED",
        "Diagnostics" -> {ToString[chart[[1]], InputForm]}|>,
        highEnergyOneSample[expr, chart, parameterRules, bound]];
      AppendTo[results, Join[<|"Sample" -> sample|>, result]],
      {sample, samples}]; True, seconds, False];
  status = Which[AnyTrue[results, Lookup[#, "Status"] === "FAIL" &], "FAIL",
    TrueQ[completed] && Length[results] === samples &&
      AllTrue[results, Lookup[#, "Status"] === "PASS" &], "PASS", True, "UNRESOLVED"];
  observed = DeleteMissing[Lookup[results, "ObservedPower", Missing["NotDetermined"]]];
  upper = DeleteMissing[Lookup[results, "ObservedPowerUpperBound", Missing["NotDetermined"]]];
  observed = If[observed === {}, Missing["NotObserved"], Max[observed]];
  diagnostics = Flatten[Lookup[results, "Diagnostics", {}]];
  diagnostics = Join[diagnostics, If[TrueQ[completed], {}, {
      "The high-energy check exceeded TimeConstraint; no conclusion is drawn from unfinished samples."}],
    {"Each recorded family keeps all external legs exactly on shell and conserves momentum. Internal propagators remain off shell.",
     "PASS tests exact asymptotic powers at sampled real fixed-angle directions, not a generic analytic proof or a gauge-independence test."}];
  If[!TrueQ[completed] || AnyTrue[results, Lookup[#, "Status"] === "UNRESOLVED" &],
    observed = Missing["IncompleteChecks"]; upper = {}];
  Join[base, <|"Status" -> status, "Samples" -> results,
    "CompletedSamples" -> Length[results], "ObservedPower" -> observed,
    "ObservedPowerUpperBound" -> If[upper === {}, Missing["NotDetermined"], Max[upper]],
    "ObservedGrowth" -> Which[!MissingQ[observed], highEnergyGrowthString[observed],
      upper =!= {}, "At most " <> highEnergyGrowthString[Max[upper]], True, "Not determined"],
    "CoefficientConvention" -> "UnnormalizedCoherentSpinPolynomial",
    "Diagnostics" -> DeleteDuplicates[Append[diagnostics,
      "All coefficients of each massive coherent spin polynomial are checked, including mixed-index longitudinal states. Constant binomial normalization factors do not change their energy powers."]]|>]
];

(* ::Subsection::Closed:: *)
(*Reference-Independence Diagnostics*)

referenceExpressionMetadata[expr_] := Module[{references, legs, chains, ids, endpoints},
  references = DeleteDuplicates[Cases[expr,
    Spinor["Xi", _, id_, ___] :> id, {0, Infinity}]];
  legs = Association[(# -> {}) & /@ references];
  chains = Cases[expr, _SpinorChain, {0, Infinity}];
  Do[
    ids = DeleteDuplicates[Cases[chain,
      Spinor["Xi", _, id_, ___] :> id, Infinity]];
    endpoints = DeleteDuplicates[Cases[chain,
      Spinor["Helicity", "Angle" | "Square", leg_Integer, ___] :> leg, Infinity]];
    Scan[AssociateTo[legs, # -> Union[Lookup[legs, #], endpoints]] &, ids],
    {chain, chains}];
  <|"References" -> references, "ReferenceLegs" -> legs,
    "PotentialSharedReferences" -> Select[legs, Length[#] > 1 &],
    "ReferenceLegAssociationScope" -> "SpinorChainCooccurrenceOnly"|>
];

(* One literal reference ID denotes one spinor. Both chiralities share
   its four real coordinates, even when only one chirality is present. *)
referenceOverrideVectors[coordinates_Association] := Association[Table[
  kind -> Association[KeyValueMap[Function[{id, values},
    id -> If[kind === "Angle", {values[[1]] + I values[[2]],
        values[[3]] + I values[[4]]}, {values[[1]] - I values[[2]],
        values[[3]] - I values[[4]]}]], coordinates]],
  {kind, {"Angle", "Square"}}]];

referencePhysicalGenericQ[chart_Association, physicalChannels_List, rules_List] :=
  AllTrue[physicalChannels, Function[channel, Module[{propagator, value},
    If[!AssociationQ[channel], Return[False]];
    propagator = Lookup[channel, "Propagator", Missing["UnknownPropagator"]];
    If[MissingQ[propagator], Return[False]];
    value = residueEvaluateChart[propagator, chart, Rationalize[rules, 0]];
    residueFiniteQ[value] && !residueUnmappedQ[value] &&
      highEnergyZeroState[value] === "Nonzero"
  ]]];

referenceOneSample[expr_, chart_Association, references_List, sample_Integer,
    seed_Integer, attempt_Integer, rules_List] := BlockRandom[Module[
  {coordinates, assignments, symbolicChart, baselineChart, variables, spinVariables,
   baseline, mapped, baselinePoint, denominator, difference, numerator,
   coefficients, checks, nonzero, unresolved, result, genericSpin},
  SeedRandom[Hash[{seed, sample, attempt, "ReferenceBaseline"}, "CRC32"]];
  coordinates = Association[(# -> Table[Unique["referenceCoordinate"], {4}]) & /@ references];
  assignments = Association[(# -> RandomChoice[DeleteCases[Range[-7, 7], 0], 4]) & /@ references];
  symbolicChart = Join[chart, <|"ReferenceOverrides" -> referenceOverrideVectors[coordinates]|>];
  baselineChart = Join[chart, <|"ReferenceOverrides" -> referenceOverrideVectors[assignments]|>];
  spinVariables = Lookup[chart, "SpinVariables", {}];
  variables = Join[Flatten[Values[coordinates]], spinVariables];
  genericSpin = Thread[spinVariables -> RandomChoice[DeleteCases[Range[-7, 7], 0],
    Length[spinVariables]]];
  baseline = residueEvaluateChart[Rationalize[expr, 0], baselineChart, Rationalize[rules, 0]];
  result = <|"Sample" -> sample, "Status" -> "UNRESOLVED", "Retryable" -> False,
    "Chart" -> baselineChart, "BaselineAssignments" -> assignments,
    "ReferenceCoordinates" -> coordinates, "SpinVariables" -> spinVariables,
    "Diagnostics" -> {}|>;
  If[!residueFiniteQ[baseline], Return[Join[result, <|"Retryable" -> True,
    "Diagnostics" -> {"The selected reference baseline is singular or unsupported."}|>]]];
  If[residueUnmappedQ[baseline], Return[Join[result, <|"Diagnostics" -> {
    "The expression contains unsupported kinematic structures."}|>]]];
  baselinePoint = Quiet[Check[Cancel[Together[baseline /. genericSpin]], $Failed]];
  If[!residueFiniteQ[baselinePoint], Return[Join[result, <|"Retryable" -> True,
    "Diagnostics" -> {"The selected reference baseline is singular."}|>]]];
  denominator = Denominator[baselinePoint];
  Switch[highEnergyZeroState[denominator],
    "Zero", Return[Join[result, <|"Retryable" -> True,
      "Diagnostics" -> {"The selected reference baseline has a zero denominator."}|>]],
    "Unknown", Return[Join[result, <|"Diagnostics" -> {
      "A baseline denominator could not be proven nonzero with the supplied parameters."}|>]]];
  mapped = residueEvaluateChart[Rationalize[expr, 0], symbolicChart, Rationalize[rules, 0]];
  If[!residueFiniteQ[mapped] || residueUnmappedQ[mapped], Return[Join[result,
    <|"Diagnostics" -> {"The symbolic reference evaluation contains unsupported structures."}|>]]];
  difference = Quiet[Check[Cancel[Together[mapped - baseline]], $Failed]];
  If[!residueFiniteQ[difference], Return[Join[result, <|"Diagnostics" -> {
    "An exact rational reference difference could not be formed."}|>]]];
  numerator = Expand[Numerator[difference]];
  denominator = Denominator[difference];
  If[!PolynomialQ[numerator, variables] || !PolynomialQ[denominator, variables],
    Return[Join[result, <|"Diagnostics" -> {
      "Reference and massive-spin dependence must be rational with polynomial numerators and denominators."}|>]]];
  coefficients = If[variables === {}, {{} -> numerator}, CoefficientRules[numerator, variables]];
  checks = highEnergyZeroState[Last[#]] & /@ coefficients;
  nonzero = Pick[coefficients, checks, "Nonzero"];
  unresolved = Count[checks, "Unknown"];
  Join[result, <|"Status" -> Which[nonzero =!= {}, "FAIL", unresolved > 0,
      "UNRESOLVED", True, "PASS"], "BaselineValue" -> baseline,
    "Difference" -> difference,
    "DifferenceNumerator" -> numerator, "DifferenceDenominator" -> denominator,
    "CoefficientCount" -> Length[coefficients], "NonzeroCoefficientCount" -> Length[nonzero],
    "UnresolvedCoefficientCount" -> unresolved,
    "OffendingCoefficients" -> Take[nonzero, UpTo[8]],
    "Diagnostics" -> Which[nonzero =!= {}, {
        "A nonzero exact coefficient proves dependence on the jointly varied literal reference spinors at this off-cut point."},
      unresolved > 0, {"Some reference-difference coefficients could not be proven zero or nonzero exactly."},
      True, {"The exact difference vanishes for every reference coordinate and implicit massive-spin component at this sampled external momentum point."}]|>]
]];

referenceCheckReport[expr_, metadata_Association, physicalChannels_List,
    samples_Integer, seed_Integer, rules_List, seconds_] := Module[
  {references, base, results = {}, completed, chart, frozen, variable,
   validation, result, status, diagnostics, attempt},
  references = referenceExpressionMetadata[expr];
  base = Join[<|"Type" -> "Reference", "Pole" -> "Reference independence",
    "Status" -> "UNRESOLVED", "Samples" -> {},
    "Method" -> "ExactJointReferencePolynomialDifference",
    "Scope" -> "SampledOffCutMomentaAllLiteralReferencesAndImplicitMassiveSpinComponents"|>, references];
  If[!FreeQ[expr, _xFactor | _xTildeFactor], Return[Join[base,
    <|"Diagnostics" -> {"Unexpanded x factors contain implicit reference dependence and must be expanded before this check."}|>]]];
  If[AnyTrue[Cases[expr, Spinor["Xi", contents___] :> {contents}, {0, Infinity}],
      Length[#] < 2 || !MemberQ[{"Angle", "Square"}, First[#]] &],
    Return[Join[base, <|"Diagnostics" -> {
      "An explicit Xi spinor has an unsupported chirality or malformed reference label."}|>]]];
  If[references["References"] === {}, Return[Join[base, <|"Status" -> "PASS",
    "Scope" -> "NoExplicitReferences", "CompletedSamples" -> 0,
    "Diagnostics" -> {"No explicit Xi spinors are present. This structural result is not an independent Ward-identity proof."}|>]]];
  completed = TimeConstrained[
    Do[
      result = <|"Sample" -> sample, "Status" -> "UNRESOLVED", "Diagnostics" -> {
        "No regular off-cut reference sample was available."}|>;
      Do[
        chart = highEnergyAutomaticChart[expr, metadata, sample + samples attempt, seed, rules];
        If[FailureQ[chart], result = <|"Sample" -> sample, "Status" -> "UNRESOLVED",
          "KinematicFailure" -> chart, "Diagnostics" -> {
            "Could not construct real off-cut external kinematics: " <>
              ToString[chart[[1]], InputForm] <> "."}|>; Continue[]];
        variable = chart["PoleVariable"];
        validation = chart["KinematicChecks"]["ValidationParameter"];
        frozen = Join[chart /. variable -> validation,
          <|"PoleVariable" -> Unique["unusedReferencePole"],
            "SamplingParameter" -> validation|>];
        If[!referencePhysicalGenericQ[frozen, physicalChannels, rules],
          result = <|"Sample" -> sample, "Status" -> "UNRESOLVED", "Diagnostics" -> {
            "Every supplied physical propagator must be evaluable and provably nonzero at the off-cut sample."}|>;
          Continue[]];
        result = referenceOneSample[expr, frozen, references["References"],
          sample, seed, attempt, rules];
        If[!TrueQ[Lookup[result, "Retryable", False]], Break[]],
        {attempt, 0, 5}];
      AppendTo[results, KeyDrop[result, "Retryable"]],
      {sample, samples}]; True, seconds, False];
  status = Which[AnyTrue[results, Lookup[#, "Status"] === "FAIL" &], "FAIL",
    TrueQ[completed] && Length[results] === samples &&
      AllTrue[results, Lookup[#, "Status"] === "PASS" &], "PASS", True, "UNRESOLVED"];
  diagnostics = Join[Flatten[Lookup[results, "Diagnostics", {}]],
    If[TrueQ[completed], {}, {"The reference-independence check exceeded TimeConstraint."}],
    {"Every literal Xi ID varies independently; repeated IDs and their conjugate angle/square spinors remain linked. Distinct IDs are never merged.",
     "ReferenceLegs and PotentialSharedReferences report only spinor-chain cooccurrence, not a proven assignment of a reference to a physical external gauge boson.",
     "All external momenta and massive-spin bases are fixed during each reference variation. Physical propagators stay off shell and nonzero; no internal on-shell identities are applied.",
     "PASS is an exact identity in the reference coordinates at sampled external momenta, not a general proof over all external kinematics or an independent Ward-identity test."}];
  Join[base, <|"Status" -> status, "Samples" -> results,
    "CompletedSamples" -> Length[results], "Diagnostics" -> DeleteDuplicates[diagnostics]|>]
];

End[];

(* ::Subsection::Closed:: *)
(*Independent Full-Amplitude Comparison*)

Begin["ConstructiveDiagrams`Private`"];

amplitudeFeynmanInput[input_, externalRules_List, rules_List, benchmarkRules_List] := Module[
  {value = input, pairs, provided, external, expression},
  If[AssociationQ[value], value = Which[
    KeyExistsQ[value, "PreparedAmplitudes"], value["PreparedAmplitudes"],
    KeyExistsQ[value, "Amplitude"], {Lookup[value, "ExternalMassRules", {}], value["Amplitude"]},
    True, Failure["InvalidFeynmanInput", <||>]]];
  If[FailureQ[value] || value === $Failed, Return[Failure["InvalidFeynmanInput", <||>]]];
  pairs = Which[residueAmplitudePairQ[value], {value},
    ListQ[value] && AllTrue[value, residueAmplitudePairQ], value,
    !ListQ[value], {{{}, value}}, True, Return[Failure["InvalidFeynmanInput", <||>]]];
  external = Cases[externalRules, HoldPattern[Mass[i_Integer] -> _] :> i];
  provided = Flatten[feynmanExternalMassRules[First[#]] & /@ pairs, 1];
  If[!AllTrue[provided, MemberQ[external, First[#][[1]]] &&
      residueExactZero[((First[#] /. externalRules) /. rules) -
        (Last[#] /. benchmarkRules)] &],
    Return[Failure["FeynmanExternalMassMismatch", <||>]]];
  expression = Total[(Last[#] /. feynmanExternalMassRules[First[#]]) & /@ pairs];
  expression = expression /. benchmarkRules /. (externalRules /. rules);
  If[residueFiniteQ[expression], expression, Failure["InvalidFeynmanInput", <||>]]
];

amplitudeComparisonState[value_, variables_List, tolerance_] := Module[{numerator, coefficients, states},
  If[!residueFiniteQ[value] || residueUnmappedQ[value], Return["Unknown"]];
  numerator = Numerator[value];
  If[!PolynomialQ[numerator, variables], Return["Unknown"]];
  coefficients = If[variables === {}, {numerator}, Last /@ CoefficientRules[numerator, variables]];
  states = If[residueComparisonZeroQ[#, "Sample", tolerance], "Zero", highEnergyZeroState[#]] & /@ coefficients;
  Which[AllTrue[states, # === "Zero" &], "Zero",
    MemberQ[states, "Nonzero"], "Nonzero", True, "Unknown"]
];

amplitudeComparisonOneSample[expression_, benchmark_, chart_Association, sample_Integer,
    seed_Integer, attempt_Integer, rules_List, benchmarkRules_List, tolerance_] := BlockRandom[Module[
  {assignments, candidateChart, benchmarkChart, candidate, expected, difference,
   opposite, states, signs, variables = Lookup[chart, "SpinVariables", {}], base},
  SeedRandom[Hash[{seed, sample, attempt, "FeynmanComparison"}, "CRC32"]];
  assignments[value_] := Association[(# -> RandomChoice[DeleteCases[Range[-7, 7], 0], 4]) & /@
    referenceExpressionMetadata[value]["References"]];
  candidateChart = Join[chart, <|"ReferenceOverrides" -> referenceOverrideVectors[assignments[expression]]|>];
  benchmarkChart = Join[chart, <|"ReferenceOverrides" -> referenceOverrideVectors[assignments[benchmark]]|>];
  base = <|"Sample" -> sample, "Status" -> "UNRESOLVED", "Chart" -> chart,
    "SpinVariables" -> variables, "MatchingSigns" -> {}, "Retryable" -> False,
    "CandidateReferenceOverrides" -> candidateChart["ReferenceOverrides"],
    "FeynmanReferenceOverrides" -> benchmarkChart["ReferenceOverrides"]|>;
  candidate = Quiet[Check[Cancel[Together[residueEvaluateChart[
    Rationalize[expression, 0], candidateChart, Rationalize[rules, 0]]]], $Failed]];
  expected = Quiet[Check[Cancel[Together[residueEvaluateChart[
    Rationalize[benchmark, 0], benchmarkChart, Rationalize[benchmarkRules, 0]]]], $Failed]];
  If[!residueFiniteQ[candidate] || !residueFiniteQ[expected],
    Return[Join[base, <|"Retryable" -> True, "Diagnostics" -> {
      "The full amplitude or its Feynman benchmark was singular at this off-cut sample."}|>]]];
  If[residueUnmappedQ[candidate] || residueUnmappedQ[expected],
    Return[Join[base, <|"Diagnostics" -> {
      "The full-amplitude comparison contains unsupported kinematic structures."}|>]]];
  difference = Quiet[Check[Cancel[Together[candidate - expected]], $Failed]];
  opposite = Quiet[Check[Cancel[Together[candidate + expected]], $Failed]];
  states = amplitudeComparisonState[#, variables, tolerance] & /@ {difference, opposite};
  signs = Pick[{1, -1}, states, "Zero"];
  Join[base, <|"Status" -> Which[MemberQ[signs, 1], "PASS",
      MemberQ[signs, -1], "Opp Sign", states === {"Nonzero", "Nonzero"}, "FAIL", True, "UNRESOLVED"],
    "CalculatedAmplitude" -> candidate, "FeynmanAmplitude" -> expected,
    "Difference" -> difference, "OppositeSignDifference" -> opposite, "MatchingSigns" -> signs,
    "Diagnostics" -> Which[MemberQ[signs, 1], {}, MemberQ[signs, -1], {
        "The full amplitudes have opposite signs; no sign has been changed or fitted."},
      states === {"Nonzero", "Nonzero"}, {"An exact massive-spin coefficient disagrees at this off-cut sample."},
      True, {"Some full-amplitude difference coefficients could not be resolved."}]|>]
]];

(* Rational tree amplitudes admit complex samples without unnecessary radicals.
   Freeze the cut coordinate away from zero and reject every physical pole. *)
amplitudeRationalComparisonChart[channels_List, sample_Integer, seed_Integer, rules_List] := BlockRandom[Module[
  {eligible, generated, chart, pole, frozen, index, values,
   result = Failure["NoRegularRationalComparisonPoint", <||>]},
  eligible = Select[channels, Min[Length[#["Cut"]], Length[#["Complement"]]] >= 2 &];
  If[eligible === {}, Return[Failure["NoRationalComparisonChannel", <||>]]];
  index = 1 + Mod[sample - 1, Length[eligible]];
  generated = completionCutChart[eligible[[index]], sample, seed, rules];
  If[FailureQ[generated], Return[generated]];
  chart = generated["Chart"]; pole = chart["PoleVariable"];
  SeedRandom[Hash[{seed, sample, eligible[[index]]["Cut"], "FeynmanOffCut"}, "CRC32"]];
  values = DeleteDuplicates[RandomChoice[{-1, 1}, 8]
    RandomInteger[{2, 19}, 8]/RandomInteger[{1, 7}, 8]];
  Do[
    frozen = Join[chart /. pole -> value, <|"PoleVariable" -> Unique["unusedBenchmarkPole"],
      "SpinVariables" -> generated["SpinVariables"], "SamplingParameter" -> value,
      "CoherentVectorSpinIndices" -> True, "AllowCutSpinors" -> False,
      "SamplingConstruction" -> "ExactRationalComplexOffCut"|>];
    If[residueFiniteQ[frozen] && referencePhysicalGenericQ[frozen, channels, rules],
      result = frozen; Break[]],
    {value, values}];
  result
]];

amplitudeFeynmanReport[inputs_Association, supplied_, model_List, samples_Integer,
    seed_Integer, rules_List, benchmarkRules_, seconds_, tolerance_] := Module[
  {expression = inputs["Expression"], metadata, externalRules, external, benchmark,
   propagators, keys, channels, chart, frozen, parameter, validation,
   results = {}, result, completed, resolved, signs, status, diagnostics = {},
   tag = Unique["feynmanComparison"], base},
  base = <|"Type" -> "Feynman", "Pole" -> "Full Feynman amplitude", "Status" -> "UNRESOLVED",
    "Samples" -> {}, "RequestedSamples" -> samples, "CompletedSamples" -> 0,
    "Method" -> "ExactSampledFullAmplitudeSpinPolynomialComparison",
    "Scope" -> "SampledOffCutMomentaAllImplicitMassiveSpinComponents",
    "InternalOnShell" -> False, "NormalizationFitted" -> False,
    "ParameterRules" -> rules, "FeynmanParameterRules" -> benchmarkRules|>;
  If[!ListQ[benchmarkRules], Return[Join[base, <|"Diagnostics" -> {
    "FeynmanModel must be the model supplying the independent benchmark's parameter definitions."}|>]]];
  Catch[
  completed = TimeConstrained[
    metadata = poleDiscoveryMetadata[inputs["Targets"], inputs["Records"]];
    If[!AssociationQ[metadata], Throw[Join[base, <|"Diagnostics" -> {
      "External mass rules are required for an off-cut Feynman comparison."}|>], tag]];
    externalRules = metadata["ExternalMassRules"]; external = metadata["ExternalLegs"];
    benchmark = amplitudeFeynmanInput[supplied, externalRules, rules, benchmarkRules];
    If[FailureQ[benchmark], Throw[Join[base, <|"Diagnostics" -> {
      If[benchmark[[1]] === "FeynmanExternalMassMismatch",
        "The Feynman benchmark and candidate declare different external masses.",
        "Supply a Feynman expression or mass-rule/expression pairs with no failed calculations."]}|>], tag]];
    If[TrueQ[feynmanRawExpressionQ[benchmark]],
      benchmark = ConstructiveDiagrams`FeynmanToSpinorProducts[benchmark, "ReduceSpinorProducts" -> False,
        "TimeConstraint" -> seconds]];
    If[FailureQ[benchmark] || !residueFiniteQ[benchmark], Throw[Join[base, <|"Diagnostics" -> {
      "Feynman conversion did not finish. Prepare the diagrams first; specify FermionHelicities for massless Dirac fields."}|>], tag]];
    If[!FreeQ[benchmark, _ModelParameter | _ModelFunction],
      Throw[Join[base, <|"Diagnostics" -> {
        "The Feynman model does not resolve every benchmark parameter. Its values are never borrowed from the candidate model."}|>], tag]];
    propagators = DeleteDuplicates[Cases[{expression, benchmark}, _PropDen, Infinity]];
    keys = residuePropagatorKey[#, external] & /@ propagators;
    If[AnyTrue[keys, MissingQ], Throw[Join[base, <|"Diagnostics" -> {
      "An internal propagator has an unsupported momentum, so a generic off-cut sample cannot be certified."}|>], tag]];
    channels = DeleteDuplicatesBy[Join[inputs["Targets"],
      (residueSimpleChannel[First[#], Last[#], external, externalRules, model] & /@ keys)], residueChannelKey];
    Do[
      result = <|"Sample" -> sample, "Status" -> "UNRESOLVED", "MatchingSigns" -> {},
        "Diagnostics" -> {"No regular off-cut full-amplitude sample was available."}|>;
      Do[
        frozen = amplitudeRationalComparisonChart[channels, sample + samples attempt, seed, rules];
        If[FailureQ[frozen],
          chart = highEnergyAutomaticChart[expression + benchmark, metadata,
            sample + samples attempt, seed, rules];
          If[FailureQ[chart], result = Join[result, <|"KinematicFailure" -> chart|>]; Continue[]];
          parameter = chart["PoleVariable"];
          validation = chart["KinematicChecks"]["ValidationParameter"];
          frozen = Join[chart /. parameter -> validation,
            <|"PoleVariable" -> Unique["unusedBenchmarkPole"], "SamplingParameter" -> validation,
              "CoherentVectorSpinIndices" -> True,
              "SamplingConstruction" -> "ExactRealCenterOfMassOffCut"|>]];
        If[!referencePhysicalGenericQ[frozen, channels, rules], Continue[]];
        result = amplitudeComparisonOneSample[expression, benchmark, frozen, sample,
          seed, attempt, rules, benchmarkRules, tolerance];
        If[!TrueQ[Lookup[result, "Retryable", False]], Break[]], {attempt, 0, 5}];
      AppendTo[results, KeyDrop[result, "Retryable"]], {sample, samples}]; True,
    seconds, False];
  resolved = Select[results, MemberQ[{"PASS", "Opp Sign"}, Lookup[#, "Status", None]] &];
  signs = Fold[Intersection, {1, -1}, Lookup[resolved, "MatchingSigns", {}]];
  status = Which[AnyTrue[results, Lookup[#, "Status", None] === "FAIL" &] || signs === {}, "FAIL",
    TrueQ[completed] && Length[resolved] === samples,
      If[MemberQ[signs, 1], "PASS", "Opp Sign"], True, "UNRESOLVED"];
  diagnostics = Join[Flatten[Lookup[results, "Diagnostics", {}]],
    If[TrueQ[completed], {}, {"The Feynman comparison exceeded its time limit."}], {
      "The two complete sums use identical external momenta and spin bases, but independently sampled explicit Xi references. All evaluated internal propagators remain nonzero.",
      "Full-amplitude samples prefer exact rational complex momenta, with real center-of-mass kinematics as a fallback. High-energy checks retain real scattering kinematics.",
      "No internal on-shell conditions, sewing sign or fitted normalization are applied. PASS is sampled agreement of every coherent massive-spin coefficient, not a generic analytic proof."}];
  Join[base, <|"Status" -> status, "Samples" -> results, "MatchingSigns" -> signs,
    "CompletedSamples" -> Length[results], "FeynmanExpression" -> benchmark,
    "Diagnostics" -> DeleteDuplicates[diagnostics]|>], tag]
];

amplitudeFeynmanRows[report_Association] := Module[{samples = Lookup[report, "Samples", {}]},
  If[samples === {}, Return[{KeyTake[report, {"Status", "Scope", "Diagnostics"}]}]];
  KeyTake[#, {"Sample", "Status", "CalculatedAmplitude", "FeynmanAmplitude",
    "Difference", "OppositeSignDifference", "Diagnostics"}] & /@ samples
];

End[];

(* ::Section::Closed:: *)
(*Amplitude Completion*)

Begin["ConstructiveDiagrams`"];

CompleteFactorization::usage = "CompleteFactorization[amplitudes, channels, model] constructs a bounded linear correction from the supplied diagram expressions and sewn channel expressions, solving all physical residues simultaneously. It accepts ordinary amplitude pairs, a prepared-expression report, or an expression. CorrectionBasis supplies correction directions; Automatic uses the input and channel expressions, not a universal amplitude ansatz. ChannelReferences -> Automatic constructs bounded alternative reference continuations, checking each on its selected cut. Without declared Bose symmetry, automatic completion first tries externally reduced whole physical-reference channels and grouped pole-bearing input terms, then falls back to literal input and channel terms within the same term and time limits. Explicit pole-free input terms are retained, not added as cancellation directions in the first attempt. ChannelReferences -> None keeps only the literal correction space. Recorded preparation references are tried when available. Failed independent validation samples supply further exact equations, up to MaxRefinements -> 3, with fresh subsequent validation. Only proven algebraically zero or exactly linearly dependent directions are removed without reducing the correction span; other residue-preserving directions remain available to CompleteHighEnergy. These sampled checks are not a generic analytic reconstruction or uniqueness proof. ReturnDetails -> False is the default: successful completion returns only the updated amplitude. ReturnDetails -> True retains the candidate, correction, affine family, CorrectionSpace, ChannelContinuations, RefinementHistory and validation, and takes precedence over Output. Output -> \"Details\" remains available for existing code. Unresolved completion normally returns a concise Failure[\"IncompleteAmplitude\", ...], without an unchecked candidate or embedded report. ParameterRules -> Automatic uses the supplied model's variables and dependent functions; an explicit rule list overrides selected parameters. Numerical substitutions affect the checks and fitting only, not the returned symbolic amplitude. No new interactions are inferred.";
CompleteFactorization::args = "Expected amplitude pairs or an expression, ordinary factorization channels, a model and valid bounded-completion options.";
CompleteFactorization::usage = CompleteFactorization::usage <>
  " AlternativeChannels -> {} optionally supplies original or alternative compact channel rows. Only after automatic completion fails are their selected residues checked against the supplied channels and their correction directions tried within the remaining term and time budgets. The supplied channels remain the targets for fitting and independent validation; alternatives never replace them. AlternativeChannelFallback in the details records the checks and result.";
CompleteFactorization::usage = CompleteFactorization::usage <>
  " BoseSymmetryGroups -> {} optionally declares disjoint groups of identical external bosonic states, for example {{3,4,5,6}} for four identical-helicity photons. The caller must supply the correct species and helicity grouping; equal masses alone do not identify identical states. Automatic Bose completion combines available compact, literal and certified selected-cut-prepared channel terms, then exactly averages the seed and directions over typed particle permutations while preserving all original residue targets. Exact formal linear dependencies may be removed by sparse reduction within a 60-second budget; an unfinished pruning pass keeps the full projected space. MaxCorrectionTerms applies after projection and pruning. The full space is retained whenever it fits; otherwise a bounded alternative keeps whole pole-bearing input diagrams and all available channel terms, without splitting the input diagrams into individual corrections. CorrectionSpaceCandidates records both sizes. CorrectionSpace -> BoseChannelUnion, BoseSelectedChannelUnion or BoseGroupedInputChannelUnion identifies the selected space in details; SelectedChannelSpace records its selected-cut checks. No successful reconstruction is claimed until every original target and the full fitted family pass independent validation. The permutation group is bounded at order 120. Each intermediate source-space construction may grow with that order, capped at 4096 directions unless a larger MaxCorrectionTerms was already requested. The merged union can exceed one source-space cap; neither this cap nor the clock is a hard memory limit. The ordinary default {} performs no such averaging and retains compact-then-literal behavior.";

Begin["ConstructiveDiagrams`Private`"];

completionBoseCycles[group_List, image_List] := Module[
  {mapping = AssociationThread[group, image], remaining = group, cycles = {}, cycle, current},
  While[remaining =!= {},
    current = First[remaining]; cycle = {};
    While[!MemberQ[cycle, current],
      AppendTo[cycle, current]; current = Lookup[mapping, current]];
    remaining = Complement[remaining, cycle];
    If[Length[cycle] > 1, AppendTo[cycles, cycle]]];
  cycles
];

completionBosePlan[groups_, externalMassRules_List] := Module[
  {labels, masses, order, perGroup},
  If[!ListQ[groups] || !AllTrue[groups,
      ListQ[#] && Length[#] >= 2 && AllTrue[#, IntegerQ[#] && # > 0 &] &&
        DuplicateFreeQ[#] &] || !DuplicateFreeQ[Flatten[groups]],
    Return[Failure["InvalidBoseSymmetry", <|"Reason" ->
      "Bose symmetry groups must be disjoint lists of at least two distinct positive external labels."|>]]];
  labels = Cases[externalMassRules, HoldPattern[Mass[label_Integer] -> _] :> label];
  If[!SubsetQ[labels, Flatten[groups]],
    Return[Failure["InvalidBoseSymmetry", <|"Reason" ->
      "Every Bose symmetry label must have a declared external mass."|>]]];
  If[AnyTrue[groups, Function[group,
      masses = Mass /@ group /. externalMassRules;
      Length[DeleteDuplicates[masses]] =!= 1]],
    Return[Failure["InvalidBoseSymmetry", <|"Reason" ->
      "The declared external masses must agree within each Bose symmetry group."|>]]];
  order = Times @@ (Factorial[Length[#]] & /@ groups);
  If[order > 120,
    Return[Failure["BoseSymmetryLimit", <|"Reason" ->
      "The requested Bose permutation group exceeds the supported order of 120.",
      "Order" -> order, "MaximumOrder" -> 120|>]]];
  perGroup = Function[group, completionBoseCycles[group, #] & /@ Permutations[group]] /@ groups;
  <|"Groups" -> groups, "Order" -> order,
    "Permutations" -> If[groups === {}, {{}}, Flatten[#, 1] & /@ Tuples[perGroup]]|>
];

completionBoseCycle[expression_, cycle_List] := Module[
  {mapping = AssociationThread[cycle, RotateLeft[cycle]], index, result},
  index[Multiparticle[parts___]] := Multiparticle @@ (index /@ {parts});
  index[label_] := Lookup[mapping, label, label];
  result = PermuteParticles[expression, cycle];
  (* The public permutation already preserves Xi IDs and explicit spin projections. *)
  result = result /. {
    SpinorHat[kind:("Spin"|"Helicity"|"Zeta"), type:("Angle"|"Square"), label_, rest___] :>
      SpinorHat[kind, type, index[label], rest],
    SpinorHat[kind:("Spin"|"Helicity"|"Zeta"), variance_, type:("Angle"|"Square"), label_, rest___] :>
      SpinorHat[kind, variance, type, index[label], rest],
    MomHat[label_] :> MomHat[index[label]], MomQ[label_] :> MomQ[index[label]],
    (head:(MomProdHat1|MomProdHat2|MomProdHat12))[a_, b_] :> head[index[a], index[b]],
    MandelstahmHat[parts___] :> MandelstahmHat @@ (index /@ {parts})};
  result /. ConstructiveDiagrams`VectorSpinIndex[label_, slot_] :>
    ConstructiveDiagrams`VectorSpinIndex[index[label], slot]
];

completionBoseProject[expression_, plan_Association] :=
  Total[Fold[completionBoseCycle, expression, #] & /@ plan["Permutations"]]/plan["Order"];

completionModelQ[model_] := MatchQ[model, {_List, _List, _List, _List}] &&
  AllTrue[model[[1]], ListQ[#] && Length[#] >= 6 &];

completionParameterRulesQ[rules_] := ListQ[rules] && AllTrue[rules,
  MatchQ[#, _Rule] && (modelParameterKeyQ[First[#]] || StringQ[First[#]]) &&
    residueFiniteQ[Last[#]] && !residueUnmappedQ[Last[#]] &];

completionInputExpression[input_] := Which[
  AssociationQ[input] && KeyExistsQ[input, "Amplitude"], input["Amplitude"],
  AssociationQ[input] && KeyExistsQ[input, "PreparedAmplitudes"],
    Total[Last /@ input["PreparedAmplitudes"]],
  MatchQ[input, {_List, _}] && AllTrue[First[input], MatchQ[#, _Rule] &], Last[input],
  ListQ[input] && AllTrue[input, MatchQ[#, {_List, _}] &], Total[Last /@ input],
  True, input];

completionNormalizeInput[input_, channels_List, model_List] := Module[
  {value = input, normalized, extra = <||>},
  If[!completionModelQ[model], Return[Failure["InvalidModel", <||>]]];
  If[AssociationQ[value],
    extra = KeyTake[value, {"OriginalExpression", "FamilyAmplitude", "RemainingBasis",
      "FreeParameters", "CorrectionBasis", "ConstraintData", "DiagramReports"}];
    value = Which[KeyExistsQ[value, "PreparedAmplitudes"], value["PreparedAmplitudes"],
      KeyExistsQ[value, "Amplitude"], value["Amplitude"],
      True, Return[Failure["InvalidCompletionInput", <||>]]];
    If[KeyExistsQ[input, "ExternalMassRules"] && !ListQ[value],
      value = {input["ExternalMassRules"], value}]];
  normalized = residueSimpleInputs[value, channels, model];
  If[!AssociationQ[normalized] || FailureQ[normalized], Return[normalized]];
  Join[normalized, <|"OriginalExpression" -> normalized["Expression"],
    "ExternalMassRules" -> Lookup[
      poleDiscoveryMetadata[normalized["Targets"], normalized["Records"]],
      "ExternalMassRules", {}]|>, extra]
];

completionProgress[callback_, enabled_, event_Association] := Module[{text},
  If[!TrueQ[enabled], Return[Null]];
  If[callback =!= Automatic, Quiet[Check[callback[event], Null]]; Return[Null]];
  text = Lookup[event, "Stage", "Amplitude completion"];
  If[KeyExistsQ[event, "Operation"], text = text <> ": " <> ToString[event["Operation"]]];
  If[KeyExistsQ[event, "Phase"], text = text <> ": " <> ToString[event["Phase"]]];
  If[KeyExistsQ[event, "Status"], text = text <> "; " <> ToString[event["Status"]]];
  If[KeyExistsQ[event, "Item"] && KeyExistsQ[event, "Items"],
    text = text <> If[Lookup[event, "InputType", "Diagram"] === "FactorizationChannel", "; channel ", "; diagram "] <>
      ToString[event["Item"]] <> "/" <> ToString[event["Items"]],
  If[KeyExistsQ[event, "Diagram"] && KeyExistsQ[event, "Diagrams"],
    text = text <> "; diagram " <> ToString[event["Diagram"]] <> "/" <> ToString[event["Diagrams"]]]];
  If[KeyExistsQ[event, "Completed"] && KeyExistsQ[event, "Total"],
    text = text <> ": " <> ToString[event["Completed"]] <> "/" <> ToString[event["Total"]]];
  If[!KeyExistsQ[event, "Completed"] && KeyExistsQ[event, "Current"] && KeyExistsQ[event, "Total"],
    text = text <> ": " <> ToString[event["Current"]] <> "/" <> ToString[event["Total"]]];
  If[KeyExistsQ[event, "Unknowns"],
    text = text <> "; " <> ToString[event["Unknowns"]] <> " correction coefficients"];
  If[KeyExistsQ[event, "UnknownCount"],
    text = text <> "; " <> ToString[event["UnknownCount"]] <> " correction coefficients"];
  If[KeyExistsQ[event, "Round"],
    text = text <> "; refinement " <> ToString[event["Round"]]];
  If[KeyExistsQ[event, "EquationCount"],
    text = text <> "; " <> ToString[event["EquationCount"]] <> " equations"];
  If[KeyExistsQ[event, "Complexity"],
    text = text <> "; complexity " <> ToString[event["Complexity"]]];
  If[KeyExistsQ[event, "Terms"], text = text <> "; terms " <> ToString[event["Terms"]]];
  If[KeyExistsQ[event, "Elapsed"],
    text = text <> "; " <> ToString[Round[event["Elapsed"], 0.1], InputForm] <> " s"];
  If[$FrontEnd === Null, Print[text], PrintTemporary[text]];
  Null
];

completionValidationRuleSets[model_List, overrides_, requested_] := Module[
  {rules = residueParameterRules[model, overrides], massSymbols, alternate},
  If[requested =!= Automatic,
    Return[DeleteDuplicates[Prepend[
      (residueParameterRules[model, Join[#, Replace[overrides, Automatic -> {}]]] & /@ requested),
      rules]]]];
  If[model[[4]] =!= {}, Return[{rules}]];
  massSymbols = Select[DeleteDuplicates[model[[1, All, 6]]], modelParameterKeyQ];
  alternate = (First[#] -> If[MemberQ[massSymbols, First[#]] && NumericQ[Last[#]],
    2 Last[#], Last[#]] &) /@ rules;
  DeleteDuplicates[{rules, alternate}]
];

completionFormat[report_Association, output_, retainReport_: True] := If[output === "Details", report,
  If[report["Status"] === "COMPLETE", report["Amplitude"],
    Failure["IncompleteAmplitude", If[TrueQ[retainReport], <|"MessageTemplate" ->
      "Amplitude completion did not establish all required constraints. Inspect the retained report.",
      "Amplitude" -> report["Amplitude"], "Report" -> report|>,
      Module[{reason = SelectFirst[Lookup[report, "Diagnostics", {}], StringQ,
          "Factorization completion did not establish agreement with every channel."]},
        <|"MessageTemplate" -> reason <> " Request ReturnDetails -> True for diagnostics.",
          "Status" -> report["Status"], "Reason" -> reason|>]]]]];

completionCoefficientEquations[expression_, variables_List] := Module[{numerator},
  If[variables =!= {} && PolynomialQ[expression, variables],
    Return[Last /@ CoefficientRules[expression, variables]]];
  numerator = Numerator[Cancel[Together[expression]]];
  If[variables === {}, Return[{numerator}]];
  If[!PolynomialQ[numerator, variables], Return[Failure["NonPolynomialSpinDependence", <||>]]];
  Last /@ CoefficientRules[Expand[numerator], variables]
];

completionCoefficientEquations[expression_, variables_List, unknowns_List] := Module[
  {arrays, components, coefficientRules, exponents, lookups, matrix},
  If[variables === {} || unknowns === {},
    Return[completionCoefficientEquations[expression, variables]]];
  arrays = Quiet[Check[CoefficientArrays[expression, unknowns], $Failed]];
  If[!ListQ[arrays] || !(1 <= Length[arrays] <= 2),
    Return[completionCoefficientEquations[expression, variables]]];
  components = Prepend[If[Length[arrays] === 1,
    ConstantArray[0, Length[unknowns]], Normal[arrays[[2]]]], First[arrays]];
  If[!AllTrue[components, residueFiniteQ[#] && PolynomialQ[#, variables] &],
    Return[completionCoefficientEquations[expression, variables]]];
  (* Extract each small spin polynomial before reassembling affine rows. This
     avoids repeatedly expanding every fitting unknown against every spin. *)
  coefficientRules = CoefficientRules[#, variables] & /@ components;
  exponents = DeleteDuplicates[First /@ Flatten[coefficientRules, 1]];
  If[exponents === {}, Return[{}]];
  exponents = First /@ CoefficientRules[
    Total[Times @@ (variables^#) & /@ exponents], variables];
  lookups = Association /@ coefficientRules;
  matrix = Table[First[Lookup[#, {power}, 0]] & /@ lookups, {power, exponents}];
  First[#] + Rest[#].unknowns & /@ matrix
];

completionLinearComponents[trial_, unknowns_List, preserveForm_: True] := Module[{arrays, linear},
  If[unknowns === {}, Return[{trial}]];
  If[TrueQ[preserveForm], Return[Prepend[Coefficient[trial, #] & /@ unknowns,
    trial /. Thread[unknowns -> 0]]]];
  (* One affine pass avoids rescanning the full trial for every coefficient. *)
  arrays = Quiet[Check[CoefficientArrays[trial, unknowns], $Failed]];
  If[ListQ[arrays] && 1 <= Length[arrays] <= 2,
    linear = If[Length[arrays] === 1, ConstantArray[0, Length[unknowns]],
      Normal[arrays[[2]]]];
    If[VectorQ[linear] && Length[linear] === Length[unknowns],
      Return[Prepend[linear, trial /. Thread[unknowns -> 0]]]]];
  Prepend[Coefficient[trial, #] & /@ unknowns, trial /. Thread[unknowns -> 0]]
];

completionSupportInputs[trial_, externalRules_List, rows_List, model_List] := Module[{propagators},
  If[!residueFiniteQ[trial],
    Return[residueSimpleInputs[{externalRules, trial}, rows, model]]];
  (* Support discovery needs the propagator inventory, not spinor reduction of
     every affine correction. All components still undergo residue validation. *)
  propagators = DeleteDuplicates[Cases[trial, _PropDen, Infinity]];
  residueSimpleInputs[{externalRules, Total[1/propagators]}, rows, model]
];

completionPolynomialZeroQ[expression_, variables_List] := Module[{coefficients},
  coefficients = completionCoefficientEquations[expression, variables];
  !FailureQ[coefficients] && AllTrue[coefficients, highEnergyZeroState[#] === "Zero" &]
];

completionTargets[rows_List] := Map[Function[group,
  Module[{complete = AllTrue[group, TrueQ[Lookup[#, "Complete", False]] &&
      KeyExistsQ[#, "StateSum"] &]},
    Join[First[group], <|"Complete" -> complete,
      "StateSum" -> If[complete, Total[Lookup[group, "StateSum"]],
        Missing["IncompleteFactorizationChannel"]]|>]]],
  GatherBy[residueNormalizeChannel /@ rows, residueChannelKey]];

(* Preserve zero-residue obligations even after their original propagator has
   disappeared from the candidate. These rows are internal validation inputs. *)
completionCheckChannelRows[normalized_Association, rows_List] := Module[
  {external, keys, missing, masses},
  external = Cases[normalized["ExternalMassRules"], HoldPattern[Mass[i_Integer] -> _] :> i];
  keys = ({canonicalPhysicalCut[List @@ #[[-3]], external], #[[-2]]} & /@ rows);
  missing = Select[completionTargets[normalized["Targets"]],
    !MemberQ[keys, residueChannelKey[#]] && TrueQ[#["Complete"]] &&
      TrueQ[residueExactZero[#["StateSum"]]] &];
  masses = Mass /@ external /. normalized["ExternalMassRules"];
  Join[rows, Join[masses, {Multiparticle @@ #["Cut"], #["Mass"], 0}] & /@ missing]
];

completionBoundedRationalReconstruction[value_Integer, modulus_Integer, limit_Integer] := Module[
  {residue = Mod[value, modulus], r0 = modulus, r1, t0 = 0, t1 = 1, q, numerator, denominator},
  If[residue === 0, Return[0]];
  r1 = residue;
  While[Abs[r1] > limit,
    q = Quotient[r0, r1];
    {r0, r1} = {r1, r0 - q r1}; {t0, t1} = {t1, t0 - q t1}];
  If[t1 === 0, Return[Missing["NoReconstruction"]]];
  {numerator, denominator} = Sign[t1] {r1, t1};
  If[Abs[numerator] > limit || denominator > limit ||
      !CoprimeQ[numerator, denominator] || !CoprimeQ[denominator, modulus] ||
      Mod[numerator - residue denominator, modulus] =!= 0,
    Missing["NoReconstruction"], numerator/denominator]
];

completionCertifiedModularReduce[matrix_List, constant_List, seconds_: 60,
    primeInventory_: Automatic] := Module[
  {fallback = Missing["CertifiedModularUnavailable"], started = AbsoluteTime[], deadline,
   rows = Length[matrix], columns, rationalQ, primes = primeInventory,
   augmented, profile = None, free = {}, modulus = 1,
   residues, candidate = None, reduced, active, pivots, block, prime,
   unique, reconstructed, lookup, limit, used = {}, heldOut, scales, integerBlock,
   expected, proof, rationalRows, actualPivots, rank, bounded, stop,
   profileBetterQ, rationalModulo, certify, tag = Unique["certifiedModular"]},
  rationalQ[value_] := MatchQ[value, _Integer | _Rational];
  If[!TrueQ[NumericQ[seconds] && Element[seconds, Reals] && 0 < seconds < Infinity] ||
      Length[constant] =!= rows || !VectorQ[constant, rationalQ] ||
      !MatrixQ[matrix, rationalQ], Return[fallback]];
  If[rows === 0, Return[<|"ReducedRows" -> {}, "Rank" -> 0, "CheckedRows" -> 0|>]];
  columns = Length[First[matrix]];
  If[AllTrue[Join[Flatten[matrix], constant], # === 0 &],
    Return[<|"ReducedRows" -> {}, "Rank" -> 0, "CheckedRows" -> rows|>]];
  If[columns === 0, Return[fallback]];
  If[primes === Automatic, primes = NestList[NextPrime[#, -1] &, 2147483647, 24]];
  If[!ListQ[primes] || Length[primes] < 3 || !DuplicateFreeQ[primes] ||
      !AllTrue[primes, IntegerQ[#] && 2 < # <= 2147483647 && PrimeQ[#] &],
    Return[fallback]];
  deadline = started + seconds;
  stop[] := Throw[fallback, tag];
  SetAttributes[bounded, HoldAll];
  bounded[expression_] := If[AbsoluteTime[] >= deadline, stop[],
    TimeConstrained[expression, Max[0.001, deadline - AbsoluteTime[]], stop[]]];
  profileBetterQ[new_List, old_List] := Length[new] > Length[old] ||
    (Length[new] === Length[old] && new =!= old && OrderedQ[{new, old}]);
  rationalModulo[values_, p_Integer] := Map[Function[value,
    Mod[Numerator[value] PowerMod[Denominator[value], -1, p], p]], values, {2}];
  certify[values_, p_Integer] := Module[{},
    scales = bounded[Apply[LCM, Denominator[#]] & /@ Transpose[values]];
    integerBlock = Map[# scales &, values];
    If[!MatrixQ[integerBlock, IntegerQ], stop[]];
    expected = Map[# scales &, augmented[[All, Append[free, columns + 1]]]];
    (* A_p B = [A_free, c] certifies every original row and free direction.
       The free identity gives n-r independent null vectors; the modular
       rank lower bound proves that this is the complete exact family. *)
    proof = bounded[Normal[SparseArray[augmented[[All, profile]]].
      SparseArray[integerBlock]] === expected];
    If[!TrueQ[proof], Return[fallback]];
    rank = Length[profile];
    rationalRows = ConstantArray[0, {rank, columns + 1}];
    rationalRows[[All, profile]] = IdentityMatrix[rank];
    rationalRows[[All, Append[free, columns + 1]]] = values;
    actualPivots = Map[Function[row, SelectFirst[Range[columns],
      row[[#]] =!= 0 &, Missing["NoPivot"]]], rationalRows];
    If[actualPivots =!= profile, Return[fallback]];
    <|"ReducedRows" -> (Append[Most[#], -Last[#]] & /@ rationalRows),
      "Rank" -> rank, "CheckedRows" -> rows, "Backend" -> "CertifiedCRT",
      "CRTPrimes" -> used, "HeldOutPrime" -> p|>
  ];
  Quiet[Check[TimeConstrained[Catch[
    augmented = bounded[MapThread[Function[{row, rhs},
      With[{whole = Append[row, rhs]}, whole Apply[LCM, Denominator[whole]]]],
      {matrix, constant}]];
    Do[
      reduced = bounded[RowReduce[augmented, Modulus -> prime]];
      If[!MatrixQ[reduced, IntegerQ], stop[]];
      active = Select[reduced, AnyTrue[#, # =!= 0 &] &];
      pivots = First[FirstPosition[#, 1]] & /@ active;
      If[!DuplicateFreeQ[pivots] || pivots =!= Sort[pivots] ||
          !AllTrue[pivots, IntegerQ[#] && 1 <= # <= columns + 1 &], stop[]];
      If[pivots === {} || MemberQ[pivots, columns + 1], Continue[]];
      If[profile === None || profileBetterQ[pivots, profile],
        profile = pivots; free = Complement[Range[columns], profile];
        used = {}; modulus = 1; candidate = None,
        If[pivots =!= profile, Continue[]]];
      block = active[[All, Append[free, columns + 1]]];
      If[candidate =!= None,
        If[!AnyTrue[Flatten[candidate], Mod[Denominator[#], prime] === 0 &],
          heldOut = bounded[rationalModulo[candidate, prime] === block];
          If[TrueQ[heldOut],
            proof = certify[candidate, prime];
            If[AssociationQ[proof], Throw[proof, tag]]]];
        candidate = None];
      If[Length[used] >= 8, stop[]];
      If[modulus === 1, residues = block,
        residues = bounded[residues + modulus Mod[
          (block - Mod[residues, prime]) PowerMod[modulus, -1, prime], prime]]];
      modulus *= prime; AppendTo[used, prime];
      If[Length[used] < 2, Continue[]];
      unique = bounded[DeleteDuplicates[Flatten[residues]]];
      limit = Floor[Sqrt[(modulus - 1)/2]];
      reconstructed = bounded[completionBoundedRationalReconstruction[#, modulus, limit] & /@ unique];
      If[FreeQ[reconstructed, _Missing],
        lookup = AssociationThread[unique, reconstructed];
        candidate = bounded[Lookup[lookup, #] & /@ residues]],
      {prime, primes}];
    fallback, tag], Max[0.001, deadline - AbsoluteTime[]], fallback], fallback]]
];

completionExactBlockReduce[matrix_List, constant_List, prime_: 2147483647] := Module[
  {rationalQ, fallback = Missing["ExactBlockFallback"], rows = Length[matrix], columns,
   integerMatrix, rowScales, modular, pivotPositions, selectedRows, pivots, free,
   rank, square, right, block, width, blockQ, zeroVectorQ, bad, repair, remaining,
   batches, pivotMap, freeMap, affine, scales, integerAffine, certificate, reduced,
   repairsSucceeded, single, contradiction, tag = Unique["exactBlock"]},
  rationalQ[value_] := MatchQ[value, _Integer | _Rational];
  If[rows === 0 || !MatrixQ[matrix, rationalQ] || Length[constant] =!= rows ||
      !VectorQ[constant, rationalQ] || !IntegerQ[prime] || !PrimeQ[prime], Return[fallback]];
  columns = Length[First[matrix]];
  If[columns === 0, Return[fallback]];
  rowScales = (LCM @@ (Denominator /@ #) &) /@ matrix;
  integerMatrix = MapThread[Times, {matrix, rowScales}];
  pivotPositions[value_List] := DeleteCases[Map[Function[row,
    SelectFirst[Range[Length[row]], row[[#]] =!= 0 &, Missing["ZeroRow"]]], value], _Missing];
  modular = Quiet[Check[RowReduce[Mod[Transpose[integerMatrix], prime],
    Modulus -> prime], $Failed]];
  If[!MatrixQ[modular], Return[fallback]];
  selectedRows = pivotPositions[modular]; rank = Length[selectedRows];
  If[rank === 0,
    Return[If[AllTrue[Flatten[matrix], # === 0 &] && AllTrue[constant, # === 0 &],
      <|"ReducedRows" -> {}, "Rank" -> 0, "CheckedRows" -> rows|>, fallback]]];
  modular = Quiet[Check[RowReduce[Mod[integerMatrix[[selectedRows]], prime],
    Modulus -> prime], $Failed]];
  If[!MatrixQ[modular], Return[fallback]];
  pivots = pivotPositions[modular];
  If[Length[pivots] =!= rank, Return[fallback]];
  free = Complement[Range[columns], pivots]; width = 1 + Length[free];
  square = matrix[[selectedRows, pivots]];
  right = Join[Transpose[{-constant[[selectedRows]]}], -matrix[[selectedRows, free]], 2];
  blockQ[value_, count_] := MatrixQ[value, rationalQ] && Dimensions[value] === {rank, count};
  zeroVectorQ[value_] := VectorQ[value] && AllTrue[value, # === 0 &];
  block = Quiet[Check[LinearSolve[square, right], $Failed]];
  If[!blockQ[block, width], Return[fallback]];
  bad = Select[Range[width], !zeroVectorQ[square.block[[All, #]] - right[[All, #]]] &];
  (* Certify every right-hand side, repairing only failed columns in small batches. *)
  batches = Table[Take[bad, {index, Min[index + 15, Length[bad]]}],
    {index, 1, Length[bad], 16}];
  repairsSucceeded = Catch[
    Do[
      repair = Quiet[Check[LinearSolve[square, right[[All, batch]]], $Failed]];
      If[!blockQ[repair, Length[batch]], Throw[False, tag]];
      remaining = Select[Range[Length[batch]],
        !zeroVectorQ[square.repair[[All, #]] - right[[All, batch[[#]]]]] &];
      Do[
        single = Quiet[Check[LinearSolve[square, right[[All, batch[[index]]]]], $Failed]];
        If[!VectorQ[single, rationalQ] || Length[single] =!= rank ||
            !zeroVectorQ[square.single - right[[All, batch[[index]]]]], Throw[False, tag]];
        repair[[All, index]] = single,
        {index, remaining}];
      block[[All, batch]] = repair,
      {batch, batches}]; True,
    tag];
  If[!TrueQ[repairsSucceeded], Return[fallback]];
  If[!AllTrue[Flatten[square.block - right], # === 0 &], Return[fallback]];
  pivotMap = AssociationThread[pivots, Range[rank]];
  freeMap = AssociationThread[free, Range[Length[free]]];
  affine = Table[If[KeyExistsQ[pivotMap, index], block[[pivotMap[index]]],
    UnitVector[width, 1 + freeMap[index]]], {index, columns}];
  If[affine[[free, Range[2, width]]] =!= IdentityMatrix[Length[free]], Return[fallback]];
  scales = (LCM @@ (Denominator /@ #) &) /@ Transpose[affine];
  integerAffine = Transpose[MapThread[Times, {Transpose[affine], scales}]];
  certificate = matrix.integerAffine;
  certificate[[All, 1]] = certificate[[All, 1]] + constant First[scales];
  reduced = MapThread[Function[{pivot, values}, ReplacePart[ConstantArray[0, columns + 1],
    Join[{pivot -> 1, columns + 1 -> First[values]}, Thread[free -> (-Rest[values])]]]],
    {pivots, block}];
  (* The caller reads leading rational pivots, which can differ from modular
     pivots even when their ranks agree. Such a prime must use the native path. *)
  If[pivotPositions[Most /@ reduced] =!= pivots, Return[fallback]];
  (* A modular rank alone is inconclusive. The certified square block spans all
     of its solutions; an exact nonzero residual independent of every free
     coordinate proves that no solution can satisfy this original row. *)
  If[!AllTrue[Flatten[certificate], # === 0 &],
    (* Certify the full nullspace as well, so the reported coefficient rank is
       exact even when the chosen prime could have dropped a matrix rank. *)
    If[!AllTrue[Flatten[Rest /@ certificate], # === 0 &], Return[fallback]];
    contradiction = SelectFirst[Range[rows], Function[index,
      certificate[[index, 1]] =!= 0 && AllTrue[Rest[certificate[[index]]], # === 0 &]],
      Missing["NoExactContradiction"]];
    If[MissingQ[contradiction], Return[fallback]];
    Return[<|"ReducedRows" -> Append[reduced, Append[ConstantArray[0, columns], 1]],
      "Rank" -> rank, "CheckedRows" -> rows, "ContradictionRow" -> contradiction|>]];
  <|"ReducedRows" -> reduced, "Rank" -> rank, "CheckedRows" -> rows|>
];

DownValues[completionSolveLinear] = {};
completionSolveLinear[equations_List, unknowns_List, modularSeconds_ : 60] := Module[
  {eqs, arrays, constant, matrix, augmented, reduced, n = Length[unknowns],
   pivots = {}, rules = {}, row, pivot, free, rank, entries, inconsistent = False,
   rightScale = 1, nonzeroConstant, candidateScale, scaledConstant, rationalQ,
   columnScales, column, scaledColumn, firstEntry, j, blockReduction},
  If[!NumericQ[modularSeconds] || !TrueQ[0 < modularSeconds < Infinity],
    Return[<|"Status" -> "UNRESOLVED", "SolutionRules" -> {},
      "FreeParameters" -> unknowns, "Rank" -> 0, "Diagnostics" -> {
        "The certified modular solve requires a positive finite remaining time budget."}|>]];
  (* Extract affine rows once. Repeated polynomial tests and common-denominator
     formation over hundreds of unknowns dominate otherwise sparse systems. *)
  eqs = DeleteDuplicates[DeleteCases[equations, 0]];
  arrays = If[n > 0 && AllTrue[eqs, residueFiniteQ],
    Quiet[Check[CoefficientArrays[eqs, unknowns], $Failed]], $Failed];
  If[!ListQ[arrays] || Length[arrays] > 2,
    (* Algebraic zero coefficients can hide an apparent higher degree. Retain
       the original whole-row normalization before rejecting such equations. *)
    eqs = DeleteDuplicates[DeleteCases[Quiet[RootReduce[Numerator[Cancel[Together[#]]]]] & /@
      equations, 0]];
    arrays = $Failed;
    If[AnyTrue[eqs, !residueFiniteQ[#] || !PolynomialQ[#, unknowns] &],
      Return[<|"Status" -> "UNRESOLVED", "SolutionRules" -> {},
        "FreeParameters" -> unknowns, "Rank" -> 0,
        "Diagnostics" -> {"The correction equations are not finite polynomials."}|>]]];
  If[eqs === {}, Return[<|"Status" -> If[n === 0, "COMPLETE", "UNDERDETERMINED"],
    "SolutionRules" -> {}, "FreeParameters" -> unknowns, "Rank" -> 0, "Diagnostics" -> {}|>]];
  If[n === 0, Return[<|"Status" -> If[AnyTrue[eqs,
      highEnergyZeroState[#] === "Nonzero" &], "INCONSISTENT", "UNRESOLVED"],
    "SolutionRules" -> {}, "FreeParameters" -> {}, "Rank" -> 0,
    "Diagnostics" -> {"There are nonzero constraints but no available correction coefficients."}|>]];
  If[arrays === $Failed, arrays = Quiet[Check[CoefficientArrays[eqs, unknowns], $Failed]]];
  If[arrays === $Failed || Length[arrays] > 2,
    Return[<|"Status" -> "UNRESOLVED", "SolutionRules" -> {},
      "FreeParameters" -> unknowns, "Rank" -> 0,
      "Diagnostics" -> {"Correction coefficients must enter linearly."}|>]];
  constant = Quiet[RootReduce[Normal[First[arrays]]]];
  matrix = Quiet[RootReduce[If[Length[arrays] === 1,
    ConstantArray[0, {Length[eqs], n}], Normal[arrays[[2]]]]]];
  entries = Flatten[{constant, matrix}];
  If[AnyTrue[entries, highEnergyZeroState[#] === "Unknown" &],
    Return[<|"Status" -> "UNRESOLVED", "SolutionRules" -> {},
      "FreeParameters" -> unknowns, "Rank" -> 0,
      "Diagnostics" -> {"Supply numerical ParameterRules for unresolved masses or couplings in the linear equations."}|>]];
  (* Remove common algebraic units before exact row reduction, restoring both
     pivot and free-variable scales in the returned original coordinates. *)
  rationalQ[value_] := MatchQ[value, _Integer | _Rational];
  columnScales = ConstantArray[1, n];
  Do[
    column = matrix[[All, j]];
    If[AllTrue[column, rationalQ], Continue[]];
    firstEntry = SelectFirst[column, highEnergyZeroState[#] === "Nonzero" &,
      Missing["ZeroColumn"]];
    If[MissingQ[firstEntry], Continue[]];
    candidateScale = Quiet[Check[Last[FactorTermsList[RootReduce[firstEntry]]], 1]];
    If[candidateScale === 1 || highEnergyZeroState[candidateScale] =!= "Nonzero", Continue[]];
    scaledColumn = Quiet[Check[RootReduce[column/candidateScale], $Failed]];
    If[ListQ[scaledColumn] && AllTrue[scaledColumn, rationalQ],
      matrix[[All, j]] = scaledColumn; columnScales[[j]] = candidateScale],
    {j, n}];
  If[AllTrue[Flatten[matrix], rationalQ],
    nonzeroConstant = SelectFirst[constant, highEnergyZeroState[#] === "Nonzero" &,
      Missing["ZeroRightHandSide"]];
    If[!MissingQ[nonzeroConstant],
      candidateScale = Quiet[Check[Last[FactorTermsList[RootReduce[nonzeroConstant]]], 1]];
      If[candidateScale =!= 1 && highEnergyZeroState[candidateScale] === "Nonzero",
        scaledConstant = Quiet[Check[RootReduce[constant/candidateScale], $Failed]];
        If[ListQ[scaledConstant] && AllTrue[scaledConstant, rationalQ],
          rightScale = candidateScale; constant = scaledConstant]]]];
  augmented = MapThread[Append, {matrix, -constant}];
  blockReduction = If[n >= 64, completionCertifiedModularReduce[matrix, constant, modularSeconds],
    Missing["SmallLinearSystem"]];
  If[n >= 64 && !AssociationQ[blockReduction],
    blockReduction = completionExactBlockReduce[matrix, constant]];
  reduced = If[AssociationQ[blockReduction], blockReduction["ReducedRows"],
    Quiet[Check[RootReduce[RowReduce[augmented]], $Failed]]];
  If[reduced === $Failed, Return[<|"Status" -> "UNRESOLVED",
    "SolutionRules" -> {}, "FreeParameters" -> unknowns, "Rank" -> 0,
    "Diagnostics" -> {"Exact row reduction failed."}|>]];
  Do[
    pivot = SelectFirst[Range[n], highEnergyZeroState[row[[#]]] === "Nonzero" &,
      Missing["NoPivot"]];
    If[MissingQ[pivot],
      If[highEnergyZeroState[Last[row]] === "Nonzero",
        inconsistent = True; Break[]],
      AppendTo[pivots, pivot];
      AppendTo[rules, unknowns[[pivot]] ->
        RootReduce[(rightScale Last[row] -
          Total[Delete[Most[row] columnScales unknowns, pivot]])/
          (row[[pivot]] columnScales[[pivot]])]]],
    {row, reduced}];
  If[inconsistent, Return[<|"Status" -> "INCONSISTENT", "SolutionRules" -> {},
    "FreeParameters" -> unknowns, "Rank" -> Length[pivots],
    "Diagnostics" -> {"No correction in the current bounded space satisfies every channel."}|>]];
  free = unknowns[[Complement[Range[n], pivots]]]; rank = Length[pivots];
  <|"Status" -> If[free === {}, "COMPLETE", "UNDERDETERMINED"],
    "SolutionRules" -> rules, "FreeParameters" -> free, "Rank" -> rank,
    "Diagnostics" -> {}|>
];

DownValues[completionRefineLinear] = {};
completionRefineLinear[previous_Association, equations_List, modularSeconds_ : 60] := Module[{extension},
  extension = completionSolveLinear[equations /. previous["SolutionRules"],
    previous["FreeParameters"], modularSeconds];
  Join[extension, <|"Rank" -> previous["Rank"] + extension["Rank"],
    "SolutionRules" -> Join[previous["SolutionRules"] /. extension["SolutionRules"],
      extension["SolutionRules"]]|>]
];

completionCoherentCutChart[saved_Association, rules_List] := Module[
  {chart = saved, spin = {}, vector, originalPole, map},
  Do[
    vector = {1, Unique["completionSpin"]}; AppendTo[spin, Last[vector]];
    AssociateTo[chart["Angle"], leg -> chart["AngleBasis"][leg].vector];
    AssociateTo[chart["Square"], leg -> chart["SquareBasis"][leg].vector];
    AssociateTo[chart["ExternalSpinVectors"], leg -> vector],
    {leg, Keys[chart["AngleBasis"]]}];
  originalPole = chart["PoleVariable"];
  map = With[{frozenChart = chart, pole = originalPole, parameters = rules},
    Function[{expression, channels, variables},
      If[Length[channels] =!= 1 || Length[variables] =!= 1,
        Missing["AutomaticMultipleCutChartNotImplemented"],
        residueEvaluateChart[expression, frozenChart /. pole -> First[variables], parameters]]]];
  <|"Map" -> map, "Chart" -> chart, "SpinVariables" -> spin, "ParameterRules" -> rules,
    "AutomaticCoherentChart" -> True|>
];

completionCutChart[channel_, sample_, seed_, rules_] := Module[{generated},
  If[!residueNumericMassesQ[channel, rules],
    Return[Failure["UnspecifiedMassParameters", <||>]]];
  generated = residueAutomaticChart[channel, sample, seed, rules];
  If[FailureQ[generated], generated, completionCoherentCutChart[generated["Chart"], rules]]
];

completionSimpleCutEvaluator[channel_Association, chart_Association] := Module[
  {tag = Unique[], pole = Unique["simpleCut"], frozen, parameters, evaluate, primitive,
   evaluateTerm, evaluateExpression},
  If[!TrueQ[Lookup[chart, "AutomaticCoherentChart", False]] ||
      !AssociationQ[Lookup[chart, "Chart", None]] ||
      !ListQ[Lookup[chart, "ParameterRules", None]],
    Return[Missing["SimpleCutRestrictionNotApplicable"]]];
  frozen = chart["Chart"] /. chart["Chart"]["PoleVariable"] -> 0;
  parameters = chart["ParameterRules"];
  evaluate[atom_?NumericQ] := If[residueFiniteQ[atom], atom,
    Throw[Missing["NonfiniteCutAtom"], tag]];
  evaluate[sum_Plus] := Total[evaluate /@ List @@ sum];
  evaluate[product_Times] := Times @@ (evaluate /@ List @@ product);
  evaluate[Power[base_, exponent_Integer]] := Module[{value = evaluate[base]},
    If[exponent < 0 && residueExactZero[value],
      Throw[Missing["SingularCutDenominator"], tag]];
    value^exponent];
  evaluate[Power[momentum_Mom, exponent_Integer?EvenQ]] := primitive[momentum^exponent];
  evaluate[atom:(_SpinorChain | _SpinorTrace | _MomProd | _Mandelstahm)] := primitive[atom];
  evaluate[atom:PropDen[_, _?NumericQ]] := primitive[atom];
  evaluate[Mass[label_Integer]] := primitive[Mass[label]];
  evaluate[_] := Throw[Missing["UnsupportedCutStructure"], tag];
  primitive[atom_] := primitive[atom] = Module[{value},
    (* Composite cut spinors are defined only on the cut, not analytically nearby. *)
    If[!FreeQ[atom,
        Spinor[_, ("Angle" | "Square"), _Multiparticle, ___] |
        Spinor[_, ("Upper" | "Lower"), ("Angle" | "Square"), _Multiparticle, ___]],
      Throw[Missing["CutOnlySpinor"], tag]];
    value = Quiet[residueEvaluateChart[atom, frozen, parameters],
      {Power::infy, Infinity::indet}];
    If[!residueFiniteQ[value] || residueUnmappedQ[value] || ListQ[value],
      Throw[Missing["NonfiniteCutPrimitive"], tag]];
    value];
  evaluateTerm[term_] := Module[{factors, poleFactors, regularFactors, exponent, value},
    factors = If[Head[term] === Times, List @@ term, {term}];
    poleFactors = Select[factors, # === pole || (Head[#] === Power &&
      First[#] === pole && IntegerQ[Last[#]]) &];
    regularFactors = Select[factors, FreeQ[#, pole] &];
    exponent = Total[If[# === pole, 1, Last[#]] & /@ poleFactors];
    If[Length[poleFactors] + Length[regularFactors] =!= Length[factors] || exponent < -1,
      Throw[Missing["NonmonomialOrHigherOrderCut"], tag]];
    value = evaluate[Times @@ regularFactors];
    If[!residueFiniteQ[value], Throw[Missing["NonfiniteCutCoefficient"], tag]];
    If[exponent === -1, value, 0]];
  evaluateExpression[expression_] := Catch[Module[{canonical, terms, value},
    If[!FreeQ[expression, _xFactor | _xTildeFactor],
      Throw[Missing["ContextDependentCutXFactor"], tag]];
    canonical = residueCanonicalPropagators[expression, channel["ExternalLegs"]] /.
      channel["Propagator"] -> pole;
    canonical = canonical /. parameters;
    terms = If[Head[canonical] === Plus, List @@ canonical, {canonical}];
    (* Supported rational primitives are analytic here only after their frozen
       values are finite and every inverse base is nonzero. Hidden poles fall back. *)
    value = Total[evaluateTerm /@ terms];
    If[residueFiniteQ[value], value, Missing["NonfiniteCutResult"]]], tag];
  Function[expression, evaluateExpression[expression]]
];

(* Evaluate a linear correction term by term before combining its coefficients.
   Keeping fit symbols out of the rational kinematic restriction avoids large
   intermediate denominators without changing the residue equations. *)
completionLinearResidue[components_List, unknowns_List, channel_Association,
    targets_List, chart_Association, seconds_] := Module[
  {initial, residues, invalid, additive = TrueQ[Lookup[chart, "AutomaticCoherentChart", False]],
   restriction, cachedRestriction, exactNumericQ, component, base, simpleEvaluator,
   restrictionMethods = {}},
  initial = PhysicalPoleResidue[If[additive, 0, First[components]], channel,
    "KinematicMap" -> chart["Map"], "KinematicKind" -> "Symbolic",
    "StructuralCoefficient" -> False,
    "TimeConstraint" -> seconds, "UncutChannels" -> targets];
  If[!AssociationQ[initial] || FailureQ[initial] ||
      Lookup[initial, "Status", None] =!= "PASS", Return[initial]];
  simpleEvaluator = completionSimpleCutEvaluator[channel, chart];
  (* The chart and cut are identical for every direction. Validate them once,
     but still require each component's pole restriction to be regular. *)
  cachedRestriction[expression_] := cachedRestriction[expression] = residueOnChart[expression,
    initial["Channels"], initial["PoleVariables"], chart["Map"],
    Range[Length[initial["Channels"]]]];
  exactNumericQ[factor_] := NumericQ[factor] && Precision[factor] === Infinity &&
    residueFiniteQ[factor];
  restriction[expression_] := Module[{factors, numeric, body, value},
    factors = If[Head[expression] === Times, List @@ expression, {expression}];
    numeric = Times @@ Select[factors, exactNumericQ];
    body = Times @@ Select[factors, !exactNumericQ[#] &];
    value = cachedRestriction[body];
    If[MissingQ[value], value, numeric value]];
  component[expression_] := TimeConstrained[Module[{values = {}, value, failed = False},
    If[Head[simpleEvaluator] === Function,
      value = simpleEvaluator[expression];
      If[!MissingQ[value], AppendTo[restrictionMethods, "FrozenAutomaticSimplePole"];
        Return[value]]];
    AppendTo[restrictionMethods, "ExactMappedRestriction"];
    (* Cut x factors use the whole component's internal-spinor inventory. *)
    If[Head[expression] =!= Plus || !FreeQ[expression, _xFactor | _xTildeFactor],
      Return[restriction[expression]]];
    (* Individually singular terms may cancel. Only the original whole
       restriction may decide regularity when additivity cannot establish it. *)
    Do[value = restriction[term];
      If[MissingQ[value], failed = True; Break[]];
      AppendTo[values, value], {term, List @@ expression}];
    If[failed, restriction[expression], Total[values]]],
    seconds, Missing["Timeout"]];
  residues = If[additive, component /@ components,
    TimeConstrained[residueOnChart[#, initial["Channels"],
        initial["PoleVariables"], chart["Map"], Range[Length[initial["Channels"]]]],
      seconds, Missing["Timeout"]] & /@ Rest[components]];
  invalid = SelectFirst[residues, MissingQ, None];
  If[invalid =!= None, Return[<|"Status" -> "UNRESOLVED",
    "PhysicalResidue" -> invalid, "Diagnostics" -> {invalid}|>]];
  base = If[additive, First[residues], initial["PhysicalResidue"]];
  Join[initial, <|"ComponentRestrictionMethods" -> If[additive, restrictionMethods,
      ConstantArray["WholeCustomMap", Length[components]]], "PhysicalResidue" -> base +
    If[unknowns === {}, 0, unknowns.If[additive, Rest[residues], residues]]|>]
];

completionResidueConstraints[trial_, unknowns_List, normalized_Association,
    samples_Integer, seed_Integer, rules_List, seconds_, callback_, enabled_] := Module[
  {targets = completionTargets[normalized["Targets"]],
   equations = {}, used = {}, channel, chart, result, expected, difference, coefficients,
   variable, success, started = AbsoluteTime[], diagnostics = {}, count = 0, completed, components},
  components = completionLinearComponents[trial, unknowns, False];
  completed = TimeConstrained[
    Do[
      If[!TrueQ[Lookup[channel, "Complete", False]] || MissingQ[Lookup[channel, "StateSum", Missing[]]],
        AppendTo[diagnostics, "A required factorization channel is incomplete."]; Break[]];
      Do[
        success = False;
        Do[
          chart = completionCutChart[channel, sample + 4 attempt,
            seed + 104729 attempt, rules];
          If[FailureQ[chart], Continue[]];
          result = completionLinearResidue[components, unknowns, channel, targets, chart, seconds];
          If[!AssociationQ[result] || result["Status"] =!= "PASS", Continue[]];
          variable = First[result["PoleVariables"]];
          expected = residueOnChart[channel["StateSum"], {channel}, {variable}, chart["Map"], {}];
          If[!residueFiniteQ[expected] || residueUnmappedQ[expected], Continue[]];
          difference = result["PhysicalResidue"] - expected;
          coefficients = completionCoefficientEquations[difference, chart["SpinVariables"], unknowns];
          If[FailureQ[coefficients], Continue[]];
          equations = Join[equations, coefficients];
          AppendTo[used, <|"Cut" -> channel["Cut"], "Sample" -> sample,
            "Chart" -> chart["Chart"], "Difference" -> difference|>];
          success = True; Break[], {attempt, 0, 5}];
        If[!success, AppendTo[diagnostics,
          "A regular exact cut chart could not supply all correction equations."]; Break[]];
        completionProgress[callback, enabled, <|"Stage" -> "Factorization samples",
          "Completed" -> count samples + sample, "Total" -> Length[targets] samples,
          "Cut" -> channel["Cut"], "Unknowns" -> Length[unknowns],
          "Elapsed" -> AbsoluteTime[] - started|>],
        {sample, samples}];
      count++;
      completionProgress[callback, enabled, <|"Stage" -> "Factorization constraints",
        "Completed" -> count, "Total" -> Length[targets], "Unknowns" -> Length[unknowns],
        "Elapsed" -> AbsoluteTime[] - started|>];
      If[diagnostics =!= {}, Break[]], {channel, targets}]; True,
    seconds, False];
  If[!TrueQ[completed], AppendTo[diagnostics, "Factorization equation construction exceeded TimeConstraint."]];
  <|"Status" -> If[TrueQ[completed] && diagnostics === {} &&
      Length[used] === Length[targets] samples, "COMPLETE", "UNRESOLVED"],
    "Equations" -> equations, "Samples" -> used, "Diagnostics" -> diagnostics|>
];

completionCanonicalReduce[expression_, massRules_List] := Module[{externalRules, external},
  externalRules = Select[massRules, MatchQ[#, HoldPattern[Mass[_Integer] -> _]] &];
  external = Sort[Cases[externalRules, HoldPattern[Mass[i_Integer] -> _] :> i]];
  withProtectedPropagators[
    residueCanonicalPropagators[expression /. externalRules, external], Cancel]
];

(* Ordinary external identities only; diagram-local shell rules never enter here. *)
completionExternalReduce[expression_, massRules_List] := Module[
  {externalRules, external, result, momentum},
  externalRules = Select[massRules, MatchQ[#, HoldPattern[Mass[_Integer] -> _]] &];
  external = Sort[Cases[externalRules, HoldPattern[Mass[i_Integer] -> _] :> i]];
  momentum[index_Multiparticle] := Module[{legs = Sort[List @@ index], complement},
    If[!VectorQ[legs, IntegerQ] || !DuplicateFreeQ[legs] ||
        Complement[legs, external] =!= {}, Return[Mom[index]]];
    complement = Complement[external, legs];
    If[Length[complement] < Length[legs],
      -Total[Mom /@ complement], Total[Mom /@ legs]]];
  result = residueCanonicalPropagators[expression, external];
  (* Expand vector sums only in contractions. Otherwise ordinary Expand
     would multiply momentum symbols as scalars inside a Lorentz square. *)
  result = withProtectedPropagators[result, Function[body,
    ReduceSpinorProducts[body /. {
      chain_SpinorChain :> (chain /. Mom[index_Multiparticle] :> momentum[index]),
      trace_SpinorTrace :> (trace /. Mom[index_Multiparticle] :> momentum[index])}] /. externalRules]];
  result = canonicalChains[result];
  withProtectedPropagators[result, Cancel]
];

completionPhysicalCoverageQ[rows_List, targets_List] :=
  ContainsAll[residueChannelKey /@ rows, residueChannelKey /@ targets];

completionPhysicalPassQ[report_, targets_: Automatic] := AssociationQ[report] && !FailureQ[report] &&
  ListQ[Lookup[report, "Channels", None]] &&
  AllTrue[report["Channels"], AssociationQ[#] && Lookup[#, "Status", None] === "PASS" &] &&
  (targets === Automatic || completionPhysicalCoverageQ[report["Channels"], targets]);

(* Reuse the exact momenta that supplied each held-out comparison, checking all
   massive-spin components before adding its equations to a later fit. *)
completionValidationConstraints[trial_, unknowns_List, normalized_Association,
    reports_List, seconds_] := Module[
  {equations = {}, used = {}, diagnostics = {}, targets = normalized["Targets"],
   rows, matching, channel, chart, calculation, variable, expected, difference,
   coefficients, rules, saved, components, tag = Unique["completionValidation"]},
  components = completionLinearComponents[trial, unknowns, False];
  Catch[
    If[reports === {} && targets =!= {}, Throw[False, tag]];
    Do[
      rules = entry["ParameterRules"];
      If[!AssociationQ[entry["Report"]] || FailureQ[entry["Report"]],
        Throw[False, tag]];
      rows = Lookup[entry["Report"], "Channels", None];
      If[!ListQ[rows] || !AllTrue[rows, AssociationQ] ||
          !completionPhysicalCoverageQ[rows, targets], Throw[False, tag]];
      Do[
        matching = Lookup[row, "FactorizationChannels", None];
        If[!ListQ[matching] || matching === {} || !AllTrue[matching,
            TrueQ[Lookup[#, "Complete", False]] && KeyExistsQ[#, "StateSum"] &],
          Throw[False, tag]];
        channel = Join[First[matching], <|"StateSum" -> Total[Lookup[matching, "StateSum"]]|>];
        If[!ListQ[Lookup[row, "Samples", None]] || row["Samples"] === {} ||
            !AllTrue[row["Samples"], AssociationQ], Throw[False, tag]];
        Do[
          saved = Lookup[sample, "Kinematics", None];
          If[!AssociationQ[saved], Throw[False, tag]];
          chart = completionCoherentCutChart[saved, rules];
          calculation = completionLinearResidue[components, unknowns, channel, targets, chart, seconds];
          If[!AssociationQ[calculation] || FailureQ[calculation] ||
              Lookup[calculation, "Status", None] =!= "PASS", Throw[False, tag]];
          variable = First[calculation["PoleVariables"]];
          expected = residueOnChart[channel["StateSum"], {channel}, {variable}, chart["Map"], {}];
          difference = calculation["PhysicalResidue"] - expected;
          If[!residueFiniteQ[difference] || residueUnmappedQ[difference], Throw[False, tag]];
          coefficients = completionCoefficientEquations[difference, chart["SpinVariables"], unknowns];
          If[FailureQ[coefficients], Throw[False, tag]];
          equations = Join[equations, coefficients];
          AppendTo[used, <|"Cut" -> channel["Cut"],
            "Sample" -> Lookup[sample, "SampleIndex", Length[used] + 1],
            "RandomSeed" -> entry["RandomSeed"], "ParameterRules" -> rules,
            "Chart" -> chart["Chart"], "Difference" -> difference|>],
          {sample, row["Samples"]}], {row, rows}], {entry, reports}]; True,
    tag, (AppendTo[diagnostics,
      "A held-out comparison did not provide regular exact equations for refinement."]; False) &];
  <|"Status" -> If[diagnostics === {}, "COMPLETE", "UNRESOLVED"],
    "Equations" -> equations, "Samples" -> used, "Diagnostics" -> diagnostics|>
];

(* Restrict the original sparse components before inserting a dense fitted
   solution. The same held-out samples certify the candidate and affine family
   and supply raw equations for another fit, without remapping that family. *)
completionSparseValidation[trial_, unknowns_List, solution_Association,
    free_List, zeroFree_List, normalized_Association, samples_Integer,
    seed_Integer, rules_List, seconds_, callback_, enabled_] := Module[
  {targets = completionTargets[normalized["Targets"]], components, rows,
   equations = {}, used = {}, diagnostics = {}, familyPass = True, completed,
   channel, chart, calculation, variable, expected, difference, coefficients,
   candidateDifference, candidateResidue, opposite, candidateState, oppositeState,
   familyCoefficients, oneFamilyPass, sampleRows, result, success, signs, status,
   solutionRules = solution["SolutionRules"], candidateRules, state, base,
   started = AbsoluteTime[], summary, report, feedback, rowSigns},
  components = completionLinearComponents[trial, unknowns, False];
  candidateRules = Thread[solution["FreeParameters"] -> 0];
  state[value_, variables_List] := Module[{values, states},
    If[!residueFiniteQ[value] || residueUnmappedQ[value], Return["Unknown"]];
    values = completionCoefficientEquations[value, variables];
    If[FailureQ[values], Return["Unknown"]];
    states = highEnergyZeroState /@ values;
    Which[MemberQ[states, "Nonzero"], "Nonzero",
      AllTrue[states, # === "Zero" &], "Zero", True, "Unknown"]];
  rows = Map[Function[target, Join[KeyDrop[residueComparisonBase[0, target, {}, targets],
      "CandidateExpression"],
    <|"Status" -> "UNRESOLVED", "Samples" -> {}, "RequestedSamples" -> samples,
      "CompletedSamples" -> 0, "Diagnostics" -> {"No regular held-out sample was completed."}|>]], targets];
  completed = TimeConstrained[
    Do[
      channel = targets[[index]]; sampleRows = {};
      If[!TrueQ[Lookup[channel, "Complete", False]] ||
          MissingQ[Lookup[channel, "StateSum", Missing[]]],
        AppendTo[diagnostics, "A required factorization channel is incomplete."]; Break[]];
      Do[
        success = False;
        Do[
          chart = completionCutChart[channel, sample + 4 attempt,
            seed + 104729 attempt, rules];
          If[FailureQ[chart], Continue[]];
          calculation = completionLinearResidue[components, unknowns, channel,
            targets, chart, seconds];
          If[!AssociationQ[calculation] || FailureQ[calculation] ||
              Lookup[calculation, "Status", None] =!= "PASS", Continue[]];
          variable = First[calculation["PoleVariables"]];
          expected = residueOnChart[channel["StateSum"], {channel}, {variable}, chart["Map"], {}];
          If[!residueFiniteQ[expected] || residueUnmappedQ[expected], Continue[]];
          difference = calculation["PhysicalResidue"] - expected;
          coefficients = completionCoefficientEquations[difference, chart["SpinVariables"], unknowns];
          If[FailureQ[coefficients], Continue[]];
          candidateDifference = difference /. solutionRules /. candidateRules;
          candidateResidue = expected + candidateDifference;
          opposite = candidateResidue + expected;
          candidateState = state[candidateDifference, chart["SpinVariables"]];
          oppositeState = state[opposite, chart["SpinVariables"]];
          signs = Pick[{1, -1}, (# === "Zero" &) /@ {candidateState, oppositeState}];
          status = Which[candidateState === "Zero", "PASS",
            oppositeState === "Zero", "Opp Sign", candidateState === "Nonzero", "FAIL",
            True, "UNRESOLVED"];
          familyCoefficients = coefficients /. solutionRules /. Thread[zeroFree -> 0];
          oneFamilyPass = AllTrue[familyCoefficients, completionPolynomialZeroQ[#, free] &];
          familyPass = familyPass && oneFamilyPass;
          result = <|"Cut" -> channel["Cut"], "Mass" -> channel["Mass"],
            "SampleIndex" -> sample, "ChartSampleIndex" -> sample + 4 attempt,
            "Attempt" -> attempt + 1, "RandomSeed" -> seed, "ParameterRules" -> rules,
            "Status" -> status, "CalculatedResidue" -> candidateResidue,
            "ExpectedResidue" -> expected, "Difference" -> candidateDifference,
            "OppositeSignDifference" -> opposite, "MatchingSigns" -> signs,
            "FamilyPass" -> oneFamilyPass, "Kinematics" -> chart["Chart"],
            "KinematicChecks" -> calculation["KinematicChecks"],
            "SpinVariables" -> chart["SpinVariables"],
            "Method" -> "ExactSparseComponentCutValidation",
            "Scope" -> "SampledCutAllCoherentMassiveSpinComponents", "Diagnostics" -> {}|>;
          AppendTo[sampleRows, result];
          equations = Join[equations, coefficients];
          AppendTo[used, <|"Cut" -> channel["Cut"], "Mass" -> channel["Mass"],
            "Sample" -> sample, "RandomSeed" -> seed, "ParameterRules" -> rules,
            "Chart" -> chart["Chart"], "SpinVariables" -> chart["SpinVariables"],
            "Difference" -> difference|>];
          success = True; Break[], {attempt, 0, 5}];
        If[!success, AppendTo[diagnostics,
          "A regular exact held-out cut chart could not validate all sparse correction components."];
          Break[]], {sample, samples}];
      rowSigns = Fold[Intersection, {1, -1},
        Lookup[Select[sampleRows, MemberQ[{"PASS", "Opp Sign"}, #["Status"]] &], "MatchingSigns", {}]];
      status = Which[AnyTrue[sampleRows, #["Status"] === "FAIL" &] || rowSigns === {}, "FAIL",
        Length[sampleRows] === samples && AllTrue[sampleRows,
          MemberQ[{"PASS", "Opp Sign"}, #["Status"]] &],
          If[MemberQ[rowSigns, 1], "PASS", "Opp Sign"], True, "UNRESOLVED"];
      base = residueComparisonBase[0, channel, {}, targets];
      rows[[index]] = Join[KeyDrop[base, "CandidateExpression"],
        <|"Status" -> status, "Samples" -> sampleRows, "RequestedSamples" -> samples,
          "CompletedSamples" -> Length[sampleRows], "MatchingSigns" -> rowSigns,
          "CalculatedResidue" -> Lookup[sampleRows, "CalculatedResidue", {}],
          "ExpectedResidue" -> Lookup[sampleRows, "ExpectedResidue", {}],
          "Difference" -> Lookup[sampleRows, "Difference", {}],
          "Method" -> "ExactSparseComponentCutValidation",
          "Scope" -> "SampledCutAllCoherentMassiveSpinComponents", "Diagnostics" -> {}|>];
      completionProgress[callback, enabled, <|"Stage" -> "Validating sparse factorization",
        "Completed" -> index, "Total" -> Length[targets], "Unknowns" -> Length[free],
        "Elapsed" -> AbsoluteTime[] - started|>];
      If[diagnostics =!= {}, Break[]], {index, Length[targets]}]; True, seconds, False];
  If[!TrueQ[completed], AppendTo[diagnostics, "Sparse held-out validation exceeded TimeConstraint."]];
  summary = Counts[Lookup[rows, "Status", {}]];
  report = <|"Channels" -> rows, "Checks" ->
      (Join[#, <|"Type" -> "Physical", "Pole" -> #["CutCondition"]|>] & /@ rows),
    "Summary" -> summary, "PhysicalSummary" -> summary, "SpuriousSummary" -> <||>,
    "FactorizationChannels" -> targets, "Table" -> residueReportTable[rows],
    "Scope" -> "ExactSparseHeldOutComponentsAllCoherentMassiveSpinComponents",
    "RandomSeed" -> seed, "ParameterRules" -> rules, "Diagnostics" -> diagnostics|>;
  feedback = <|"Status" -> If[TrueQ[completed] && diagnostics === {} &&
      Length[used] === Length[targets] samples, "COMPLETE", "UNRESOLVED"],
    "Equations" -> equations, "Samples" -> used, "Diagnostics" -> diagnostics|>;
  <|"Report" -> report, "Feedback" -> feedback,
    "FamilyPass" -> (feedback["Status"] === "COMPLETE" && familyPass)|>
];

completionNormalizeBasis[terms_List] := DeleteDuplicates[DeleteCases[Map[Function[term,
  Module[{factors, numeric}, factors = If[Head[term] === Times, List @@ term, {term}];
    numeric = Times @@ Select[factors, NumericQ];
    If[TrueQ[numeric === 0], 0, Cancel[term/numeric]]]], terms], 0]];

(* Formal monomial dependence is exact, not a sampled physical relation.
   Incremental Gaussian elimination avoids a large bulk row reduction. *)
DownValues[completionGaussianRationalQ] = {};
DownValues[completionLegacyIndependentBasis] = {};
DownValues[completionGaussianIndependentBasis] = {};
DownValues[completionIndependentBasis] = {};
completionGaussianRationalQ[value_] := MatchQ[value, _Integer | _Rational] ||
  (Head[value] === Complex && MatchQ[Re[value], _Integer | _Rational] && MatchQ[Im[value], _Integer | _Rational]);

completionLegacyIndependentBasis[basis_List, seconds_] := TimeConstrained[Module[
  {terms, scalar, key, monomials, positions, columns, entries, matrix, reduced, pivots},
  If[Length[basis] < 2, Return[basis]];
  scalar[term_] := Times @@ Select[If[Head[term] === Times, List @@ term, {term}],
    NumericQ[#] && Precision[#] === Infinity &&
      residueFiniteQ[#] &];
  key[term_] := Cancel[term/scalar[term]];
  terms = Map[Function[value, With[{expanded = Expand[value]},
    If[Head[expanded] === Plus, List @@ expanded, If[expanded === 0, {}, {expanded}]]]], basis];
  If[!FreeQ[terms, number_?NumberQ /; Precision[number] =!= Infinity], Return[basis]];
  monomials = DeleteDuplicates[key /@ Flatten[terms, 1]];
  If[monomials === {}, Return[{}]];
  positions = AssociationThread[monomials, Range[Length[monomials]]];
  columns = Map[Function[column, GroupBy[
    ({Lookup[positions, key[#]], scalar[#]} &) /@ column, First -> Last, Total]], terms];
  entries = Flatten[MapIndexed[Function[{column, index},
    KeyValueMap[({#1, First[index]} -> #2) &, column]], columns], 1];
  matrix = SparseArray[entries, {Length[monomials], Length[basis]}];
  reduced = RowReduce[matrix];
  positions = Cases[ArrayRules[reduced],
    (HoldPattern[Rule[{row_Integer, column_Integer}, value_]] /; value =!= 0) :> {row, column}];
  pivots = Values[KeySort[GroupBy[positions, First -> Last, Min]]]; basis[[pivots]]
], seconds, basis];

completionGaussianIndependentBasis[basis_List] := Module[
  {scalar, key, terms, positions = <||>, rowCount = 0, pivots = <||>, selected = {},
   vector, value, row, pivot, coefficient, record, index = 0, tag = Unique["pruner"]},
  Catch[
    scalar[term_] := scalar[term] = Times @@ Select[If[Head[term] === Times, List @@ term, {term}],
      NumericQ[#] && Precision[#] === Infinity &&
        residueFiniteQ[#] &];
    (* Memoized Cancel preserves the legacy formal-monomial convention. *)
    key[term_] := key[term] = Cancel[term/scalar[term]];
    Do[
      index++;
      terms = With[{expanded = Expand[expression]},
        If[Head[expanded] === Plus, List @@ expanded, If[expanded === 0, {}, {expanded}]]];
      If[!FreeQ[terms, number_?NumberQ /; Precision[number] =!= Infinity],
        Throw[<|"Mode" -> "NoPruning", "Basis" -> basis|>, tag]];
      If[!AllTrue[terms, completionGaussianRationalQ[scalar[#]] &],
        Throw[<|"Mode" -> "LegacyExactFieldFallback"|>, tag]];
      vector = <||>;
      Do[
        row = Lookup[positions, key[term], Missing["New"]];
        If[MissingQ[row], rowCount++; row = rowCount; AssociateTo[positions, key[term] -> row]];
        value = Lookup[vector, row, 0] + scalar[term];
        If[value === 0, KeyDropFrom[vector, row], AssociateTo[vector, row -> value]],
        {term, terms}];
      While[Length[vector] > 0,
        pivot = Min[Keys[vector]]; coefficient = vector[pivot];
        If[KeyExistsQ[pivots, pivot],
          record = pivots[pivot];
          KeyValueMap[Function[{keyRow, factor},
            value = Lookup[vector, keyRow, 0] - coefficient factor;
            If[value === 0, KeyDropFrom[vector, keyRow], AssociateTo[vector, keyRow -> value]]], record],
          AssociateTo[pivots, pivot -> Map[#/coefficient &, vector]];
          AppendTo[selected, index]; vector = <||>]],
      {expression, basis}];
    <|"Mode" -> "ExactGaussianSparseColumns", "Basis" -> basis[[selected]],
      "Indices" -> selected, "FormalRank" -> Length[selected]|>, tag]
];

completionIndependentBasis[basis_List, seconds_: 60] := Module[{started, result, remaining},
  If[Length[basis] < 2 || !(seconds === Infinity ||
      (NumericQ[seconds] && TrueQ[seconds > 0])), Return[basis]];
  started = AbsoluteTime[];
  result = TimeConstrained[completionGaussianIndependentBasis[basis], seconds, <|"Mode" -> "NoPruning", "Basis" -> basis|>];
  If[Lookup[result, "Mode", None] === "LegacyExactFieldFallback",
    remaining = If[seconds === Infinity, Infinity, seconds - (AbsoluteTime[] - started)];
    If[remaining <= 0, basis, completionLegacyIndependentBasis[basis, remaining]], Lookup[result, "Basis", basis]]
];

completionRecordedReferenceTables[normalized_Association] := DeleteDuplicates[
  Select[(Lookup[Lookup[#, "ReferenceSpinors", <||>], "Assignments", {}] & /@
    Lookup[normalized, "DiagramReports", {}]), # =!= {} &]];

completionAssignedReferences[expression_, inferred_List, recorded_List,
    externalRules_List, physical_List, own_List] := Module[
  {settings = {}, choices, key, tag = Unique["completionReferences"], fail,
   rewrite, transformed, denominators = {}, external, opposite, selected},
  external = Cases[externalRules, HoldPattern[Mass[i_Integer] -> _] :> i];
  fail[] := Throw[Failure["UnavailableRecordedReferences", <||>], tag];
  Catch[
    Do[
      key = {assignment["Leg"], assignment["Reference"][[2]]};
      choices = DeleteDuplicatesBy[Select[recorded,
        Lookup[#, "Leg", None] === First[key] &&
          MatchQ[Lookup[#, "Reference", None], _Spinor] &&
          #["Reference"][[2]] === Last[key] &],
        {Lookup[#, "Mode", None], Lookup[#, "Target", None]} &];
      If[Length[choices] =!= 1, fail[]];
      selected = Join[First[choices], KeyTake[assignment, {"Reference", "Leg"}]];
      If[selected["Mode"] === "AllPhysical",
        key = {canonicalPhysicalCut[Sort[{selected["Leg"], selected["Target"]}], external],
          Mass[selected["Target"]] /. externalRules};
        If[!MemberQ[physical, key] || MemberQ[own, key], fail[]]];
      AppendTo[settings, selected], {assignment, inferred}];
    opposite["Angle"] := "Square"; opposite["Square"] := "Angle";
    rewrite[chain_SpinorChain] := Module[
      {parts = List @@ chain, left, right, middle, first, last, sign = 1, setting, a, b, ref},
      If[FreeQ[chain, Spinor["Xi", ___]], Return[chain]];
      If[Length[parts] < 2 || !FreeQ[Drop[Rest[parts], -1], Spinor["Xi", ___]], fail[]];
      left = First[parts]; right = Last[parts]; middle = Drop[Rest[parts], -1];
      first = {left}; last = {right};
      Do[
        ref = If[side === 1, left, right];
        If[MatchQ[ref, Spinor["Xi", ___]],
          choices = Select[settings, #["Reference"] === ref &];
          If[Length[choices] =!= 1,
            selected = If[side === 1, right, left];
            If[!MatchQ[selected, _Spinor], fail[]];
            choices = Select[choices, #["Leg"] === spinorLeg[selected] &]];
          If[Length[choices] =!= 1, fail[]];
          setting = First[choices]; a = setting["Leg"]; b = setting["Target"];
          If[setting["Mode"] === "AllPhysical",
            If[side === 1, first = {Spinor["Helicity", opposite[ref[[2]]], a], Mom[b]},
              last = {Mom[b], Spinor["Helicity", opposite[ref[[2]]], a]}; sign = -sign];
            AppendTo[denominators,
              SpinorChain[Spinor["Helicity", opposite[ref[[2]]], a], Mom[b],
                Spinor["Helicity", ref[[2]], a]] ->
                  PropDen[Mom[Multiparticle @@ Sort[{a, b}]], Mass[b] /. externalRules]],
            selected = Switch[setting["Mode"],
              "AllSpurious", Spinor["Helicity", ref[[2]], b],
              "Xi", b,
              "IndependentXi", ReplacePart[ref, 3 -> a], _, fail[]];
            If[side === 1, first = {selected}, last = {selected}]]], {side, {1, 2}}];
      sign (SpinorChain @@ Join[first, middle, last])];
    transformed = withProtectedPropagators[expression,
      Function[body, body /. chain_SpinorChain :> rewrite[chain]]];
    denominators = DeleteDuplicates[Join[denominators,
      (Reverse[First[#]] -> Last[#]) & /@ denominators]];
    transformed = withProtectedPropagators[transformed, Function[body,
      body /. HoldPattern[Power[chain_SpinorChain, power_?NumericQ] /; TrueQ[power < 0]] :>
        Power[chain /. denominators, power]]];
    <|"Status" -> "COMPLETE", "Expression" -> transformed, "Assignments" -> settings,
      "Diagnostics" -> {}|>, tag]
];

(* Each continuation is checked only on its own cut. This preserves the supplied
   residue without imposing that channel's shell condition on the full sum. *)
completionHigherPhysicalPoles[expression_, targets_List, external_List] := Module[
  {propagators, variables, rational, denominator, orders},
  propagators = DeleteDuplicates[Lookup[residueNormalizeChannel /@ targets, "Propagator"]];
  If[propagators === {}, Return[{}]];
  variables = Table[Unique["continuationPole"], {Length[propagators]}];
  rational = Cancel[Together[residueCanonicalPropagators[expression, external] /.
    Thread[propagators -> variables]]];
  denominator = Denominator[rational];
  If[!PolynomialQ[denominator, variables],
    Return[Failure["UnsupportedContinuationDenominator", <||>]]];
  orders = Exponent[denominator, #] & /@ variables;
  Pick[propagators, (# > 1 &) /@ orders]
];

completionContinuationCheck[source_, variant_, channel_Association,
    targets_List, rules_List, seed_Integer] := Module[
  {chart, result, equations, success, samples = {}, state, complete = True,
   tag = Unique["continuationCheck"]},
  Catch[
  Do[
    success = False;
    Do[
      chart = completionCutChart[channel, sample + 4 attempt, seed + 104729 attempt, rules];
      If[FailureQ[chart], Continue[]];
      result = PhysicalPoleResidue[variant - source, channel,
        "KinematicMap" -> chart["Map"], "KinematicKind" -> "Symbolic",
        "StructuralCoefficient" -> False,
        "UncutChannels" -> targets];
      If[!AssociationQ[result] || Lookup[result, "Status", None] =!= "PASS", Continue[]];
      equations = completionCoefficientEquations[result["PhysicalResidue"], chart["SpinVariables"]];
      If[FailureQ[equations], Continue[]];
      state = highEnergyZeroState /@ equations;
      If[MemberQ[state, "Nonzero"], Throw[<|"Status" -> "FAIL", "Samples" -> samples,
        "Scope" -> "ExactSampledSelectedCut", "Diagnostics" -> {
          "The reference continuation changes its selected channel residue."}|>, tag]];
      If[!AllTrue[state, # === "Zero" &], Continue[]];
      AppendTo[samples, <|"Sample" -> sample, "Chart" -> chart["Chart"],
        "ResidueDifference" -> 0|>]; success = True; Break[], {attempt, 0, 2}];
    If[!success, complete = False; Break[]], {sample, 2}];
  <|"Status" -> If[complete, "PASS", "UNRESOLVED"], "Samples" -> samples,
    "Scope" -> "ExactSampledSelectedCut", "Diagnostics" -> If[complete, {}, {
      "Regular exact cut samples did not establish preservation of this channel residue."}]|>, tag]
];

completionChannelContinuations[normalized_Association, model_List, rules_List,
    seed_Integer : 1729] := Module[
  {targets, poleTargets, externalRules = normalized["ExternalMassRules"], external, inventory,
   sources = {}, variants = {}, diagnostics = {}, recorded, source, propagators,
   ownKeys, ownRules, proposal, inferred, proposals, verification, accepted,
   higherPoles, channelIndex = 0, rotationGroups, rotationCount,
   rotationLimit = 16, selection, rotated},
  targets = Select[completionTargets[normalized["Targets"]],
    TrueQ[Lookup[#, "Complete", False]] && KeyExistsQ[#, "StateSum"] &];
  poleTargets = Select[targets, !TrueQ[residueExactZero[#["StateSum"]]] &];
  external = Cases[externalRules, HoldPattern[Mass[i_Integer] -> _] :> i];
  inventory = ({Join[externalRules, {Mass[Multiparticle @@ #["Cut"]] -> #["Mass"]}], 0} &) /@ poleTargets;
  recorded = completionRecordedReferenceTables[normalized];
  Do[
    channelIndex++;
    source = residueCanonicalPropagators[channel["StateSum"]/channel["Propagator"], external];
    AppendTo[sources, source];
    If[FreeQ[source, Spinor["Xi", ___]], Continue[]];
    propagators = DeleteDuplicates[Cases[source, p_PropDen :> p, {0, Infinity}]];
    ownKeys = residuePropagatorKey[#, external] & /@ propagators;
    If[AnyTrue[ownKeys, MissingQ],
      AppendTo[diagnostics, <|"Cut" -> channel["Cut"], "Reason" -> "UnsupportedSourcePropagator"|>];
      Continue[]];
    ownRules = Join[externalRules,
      (Mass[Multiparticle @@ First[#]] -> Last[#] & /@ ownKeys)];
    proposals = {};
    inferred = preparationReferences[source, ownRules, model, inventory, "IndependentXi"];
    If[AssociationQ[inferred] && Lookup[inferred, "Status", None] === "COMPLETE",
      Do[
        proposal = completionAssignedReferences[source, inferred["Assignments"], table,
          externalRules, residueChannelKey /@ poleTargets, ownKeys];
        If[AssociationQ[proposal] && !FailureQ[proposal],
          AppendTo[proposals, Join[proposal, <|"Mode" -> "PreparedReferences"|>]],
          AppendTo[diagnostics, <|"Cut" -> channel["Cut"],
            "Reason" -> "UnavailablePreparedReferenceAssignment"|>]], {table, recorded}]];
    Do[
      proposal = preparationReferences[source, ownRules, model, inventory, mode];
      If[AssociationQ[proposal] && Lookup[proposal, "Status", None] === "COMPLETE",
        AppendTo[proposals, Join[proposal, <|"Mode" -> mode|>]];
        If[mode === "AllSpurious",
          rotationGroups = Lookup[proposal, "Choices", {}];
          If[rotationGroups =!= {},
            rotationCount = Max[Length /@ rotationGroups];
            (* Rotate all eligible lists together. This visits each single
               reference choice without their exponential Cartesian product. *)
            Do[
              selection = (#[[1 + Mod[rotation, Length[#]]]] &) /@ rotationGroups;
              rotated = preparationReferences[source, ownRules, model, inventory,
                "AllSpurious", selection];
              If[AssociationQ[rotated] && Lookup[rotated, "Status", None] === "COMPLETE",
                AppendTo[proposals, Join[rotated,
                  <|"Mode" -> "RotatedCyclic", "Rotation" -> rotation|>]]],
              {rotation, 1, Min[rotationCount, rotationLimit] - 2}];
            If[rotationCount > rotationLimit, AppendTo[diagnostics,
              <|"Cut" -> channel["Cut"], "Mode" -> "RotatedCyclic",
                "Reason" -> "ReferenceRotationLimit", "AvailableRotations" -> rotationCount,
                "RotationLimit" -> rotationLimit|>]]]],
        AppendTo[diagnostics, <|"Cut" -> channel["Cut"], "Mode" -> mode,
          "Reason" -> "UnavailableReferenceContinuation"|>]],
      {mode, {"AllSpurious", "ReverseCyclic", "AllPhysical"}}];
    (* Keep the physical classification when a recorded assignment produces
       the same expression; compact basis selection depends on that mode. *)
    proposals = (First[SortBy[#, If[Lookup[#, "Mode", None] === "AllPhysical", 0, 1] &]] &) /@
      GatherBy[proposals, #["Expression"] &];
    Do[
      If[MemberQ[sources, proposal["Expression"]], Continue[]];
      higherPoles = completionHigherPhysicalPoles[proposal["Expression"], targets, external];
      verification = Which[
        FailureQ[higherPoles], <|"Status" -> "UNRESOLVED", "Samples" -> {},
          "Reason" -> "UnsupportedContinuationDenominator", "Diagnostics" -> {
            "The continuation's physical denominator orders could not be established."}|>,
        higherPoles =!= {}, <|"Status" -> "UNRESOLVED", "Samples" -> {},
          "Reason" -> "UnsupportedHigherOrderPhysicalPoleContinuation",
          "HigherOrderPropagators" -> higherPoles, "Diagnostics" -> {
            "This continuation has uncanceled higher-order physical poles, outside the simple-pole correction solver's supported space."}|>,
        True, completionContinuationCheck[source, proposal["Expression"], channel,
          targets, rules, seed + 1987 channelIndex]];
      accepted = verification["Status"] === "PASS";
      AppendTo[variants, Join[<|"Cut" -> channel["Cut"], "Mass" -> channel["Mass"],
        "Mode" -> proposal["Mode"], "Assignments" -> proposal["Assignments"],
        "Expression" -> proposal["Expression"], "Accepted" -> accepted,
        "Verification" -> verification|>, KeyTake[proposal, "Rotation"]]];
      If[accepted, AppendTo[sources, proposal["Expression"]]], {proposal, proposals}],
    {channel, targets}];
  <|"Sources" -> DeleteDuplicates[sources], "Variants" -> variants,
    "Diagnostics" -> DeleteDuplicates[diagnostics],
    "Scope" -> "BoundedReferenceContinuationsWithExactSampledSelectedCutChecks"|>
];

completionAutomaticBasis[normalized_Association, limit_Integer, references_ : None] := Module[
  {sources, terms = {}, expanded, external, exceeded = False},
  external = Cases[normalized["ExternalMassRules"], HoldPattern[Mass[i_Integer] -> _] :> i];
  sources = Join[{normalized["Expression"]},
    If[AssociationQ[references], Lookup[references, "Sources", {}],
      (#["StateSum"]/#["Propagator"] & /@ Select[normalized["Targets"],
        TrueQ[Lookup[#, "Complete", False]] && KeyExistsQ[#, "StateSum"] &])]];
  Do[
    expanded = residueCanonicalPropagators[expression, external];
    expanded = withProtectedPropagators[expanded, Expand];
    terms = Join[terms, If[Head[expanded] === Plus, List @@ expanded, {expanded}]];
    terms = completionNormalizeBasis[terms];
    If[Length[terms] > limit, exceeded = True; Break[]], {expression, sources}];
  If[exceeded, Failure["CorrectionSpaceLimit", <|
    "Terms" -> Length[terms], "Limit" -> limit|>], terms]
];

(* Whole channel reduction exposes directions hidden inside momentum chains.
   Keep diagram groups intact here; the literal termwise space remains a fallback. *)
completionReducedChannelBasis[normalized_Association, references_Association,
    limit_Integer, rules_List, seed_Integer] := Module[
  {variants, targets, external = normalized["ExternalMassRules"], sources = {},
   checks = {}, channel, reduced, verification, terms, groupedInputs, split,
   externalLegs, referenceFree, source, higherPoles, referenceFreeQ,
   selectedRules, selectedReduced, difference, groupedDirections = {}},
  (* An x factor or a surviving internal spinor is defined only on its own
     cut, even when no explicit Xi remains. It is not an off-cut direction. *)
  referenceFreeQ[expression_] := FreeQ[expression,
    _xFactor | _xTildeFactor | Spinor["Xi", ___] | SpinorHat["Xi", ___] |
      (Spinor | SpinorHat)[_, ("Angle" | "Square"), _Multiparticle, ___] |
      (Spinor | SpinorHat)[_, ("Upper" | "Lower"), ("Angle" | "Square"), _Multiparticle, ___]];
  variants = Select[Lookup[references, "Variants", {}],
    TrueQ[Lookup[#, "Accepted", False]] && Lookup[#, "Mode", None] === "AllPhysical" &&
      referenceFreeQ[#["Expression"]] &];
  targets = completionTargets[normalized["Targets"]];
  externalLegs = Cases[external, HoldPattern[Mass[i_Integer] -> _] :> i];
  (* A sewn channel may already contain no references, as with two known
     four-point amplitudes. It needs no reference variant to enter the space. *)
  referenceFree = Select[targets, TrueQ[Lookup[#, "Complete", False]] &&
    KeyExistsQ[#, "StateSum"] && !TrueQ[residueExactZero[#["StateSum"]]] &];
  Do[
    source = residueCanonicalPropagators[target["StateSum"]/target["Propagator"], externalLegs];
    If[referenceFreeQ[source],
      AppendTo[variants, <|"Cut" -> target["Cut"], "Mass" -> target["Mass"],
        "Mode" -> "ReferenceFree", "Expression" -> source|>]],
    {target, referenceFree}];
  If[variants === {}, Return[Missing["NoReducedPhysicalChannels"]]];
  Do[
    higherPoles = completionHigherPhysicalPoles[variant["Expression"], targets, externalLegs];
    If[FailureQ[higherPoles] || higherPoles =!= {},
      AppendTo[checks, <|"Cut" -> variant["Cut"], "Mass" -> variant["Mass"],
        "Mode" -> variant["Mode"], "Verification" -> <|"Status" -> "UNRESOLVED",
          "Reason" -> If[FailureQ[higherPoles], "UnsupportedContinuationDenominator",
            "UnsupportedHigherOrderPhysicalPoleContinuation"]|>|>]; Continue[]];
    reduced = TimeConstrained[completionExternalReduce[variant["Expression"], external],
      2, $TimedOut];
    If[!residueFiniteQ[reduced], Continue[]];
    channel = SelectFirst[targets,
      #["Cut"] === variant["Cut"] && residueExactZero[#["Mass"]^2 - variant["Mass"]^2] &,
      Missing["UnknownChannel"]];
    If[MissingQ[channel], Continue[]];
    verification = completionContinuationCheck[variant["Expression"], reduced,
      channel, targets, rules, seed];
    AppendTo[checks, <|"Cut" -> variant["Cut"], "Mass" -> variant["Mass"],
      "Mode" -> variant["Mode"], "Verification" -> verification|>];
    If[verification["Status"] =!= "PASS", Continue[]];
    sources = Join[sources, {reduced, variant["Expression"]}];
    (* A channel fixes only its selected cut. Keep the resulting off-cut
       change grouped, rather than expanding every new numerator term. *)
    selectedRules = Join[external,
      {Mass[Multiparticle @@ channel["Cut"]] -> channel["Mass"]}];
    selectedReduced = TimeConstrained[preparationReferenceTrial[
      preparationNumerators[variant["Expression"],
        Function[num, ReduceSpinContractions[num] /. selectedRules]],
      selectedRules, preparationDotRules[selectedRules, True]], 2, $TimedOut];
    If[!residueFiniteQ[selectedReduced], Continue[]];
    difference = selectedReduced - variant["Expression"];
    If[TrueQ[residueExactZero[difference]], Continue[]];
    higherPoles = completionHigherPhysicalPoles[selectedReduced, targets, externalLegs];
    If[FailureQ[higherPoles] || higherPoles =!= {}, Continue[]];
    verification = completionContinuationCheck[variant["Expression"], selectedReduced,
      channel, targets, rules, seed];
    AppendTo[checks, <|"Cut" -> variant["Cut"], "Mass" -> variant["Mass"],
      "Mode" -> "SelectedCutReduction", "SourceMode" -> variant["Mode"],
      "MassRules" -> selectedRules, "Verification" -> verification|>];
    If[verification["Status"] === "PASS", AppendTo[groupedDirections, difference]],
    {variant, variants}];
  If[sources === {}, Return[Missing["NoVerifiedReducedChannels"]]];
  split[expression_] := With[{expanded = withProtectedPropagators[expression, Expand]},
    If[Head[expanded] === Plus, List @@ expanded, {expanded}]];
  terms = completionNormalizeBasis[Flatten[split /@ sources, 1]];
  groupedInputs = Map[Function[record,
    Total[Select[If[Head[record["Expression"]] === Plus,
      List @@ record["Expression"], {record["Expression"]}], !FreeQ[Denominator[#], _PropDen] &]]],
    normalized["Records"]];
  groupedDirections = completionNormalizeBasis[groupedDirections];
  terms = completionNormalizeBasis[Join[terms, groupedInputs, groupedDirections]];
  If[Length[terms] > limit, Return[Missing["ReducedChannelSpaceLimit"]]];
  <|"Basis" -> terms, "Sources" -> DeleteDuplicates[sources],
    "GroupedDirections" -> groupedDirections,
    "Verification" -> checks,
    "Scope" -> "ReducedPhysicalAndReferenceFreeChannelsWithGroupedPoleBearingInputs"|>
];

(* A selected cut fixes the sum of its pair invariants, not any one pair.
   Each pivot defines a different numerator continuation away from that cut. *)
completionCutInvariantTrials[expression_, cut_List, cutMass_, externalRules_List] := Module[
  {rules, masses, pairs, products, constant, candidate, trials = {}, rule},
  If[Length[cut] < 2 || !VectorQ[cut, IntegerQ[#] && # > 0 &] || !DuplicateFreeQ[cut],
    Return[Missing["UnsupportedSelectedCut"]]];
  If[Length[cut] === 2, Return[{}]];
  rules = Cases[externalRules, rule : HoldPattern[Mass[_Integer] -> _] :> rule];
  masses = (Mass /@ cut) /. rules;
  If[!FreeQ[masses, _Mass], Return[Missing["MissingExternalMass"]]];
  pairs = Subsets[Sort[cut], {2}]; products = orderedDot @@@ pairs;
  constant = (cutMass^2 - Total[masses^2])/2;
  Do[
    rule = products[[index]] -> constant - Total[Delete[products, index]];
    candidate = preparationReduceDots[expression, {rule}];
    If[candidate =!= expression,
      AppendTo[trials, <|"Cut" -> cut, "Mass" -> cutMass, "PivotPair" -> pairs[[index]],
        "NumeratorRule" -> rule, "Expression" -> candidate|>]],
    {index, Length[pairs]}];
  trials
];

(* A broader optional space splits selected-cut-prepared channel numerators.
   Its certification preserves only that source's own residue, not its off-cut form. *)
DownValues[completionSelectedChannelBasis] = {};
completionSelectedChannelBasis[normalized_Association, references_Association,
    limit_Integer, rules_List, seed_Integer, seconds_ : 60] := Module[
  {targets = completionTargets[normalized["Targets"]], external = normalized["ExternalMassRules"],
   externalLegs, variants, closedQ, source, channel, selectedRules, reduced,
   higherPoles, verification, checks = {}, sources = {}, terms = {}, split,
   originalSources = {}, originalFinished = False, invariantFinished = True,
   proposals, proposalTerms, diagnostics = {}, finished, tag = Unique["selectedChannelSpace"]},
  closedQ[expression_] := FreeQ[expression, _xFactor | _xTildeFactor |
    (Spinor | SpinorHat)[_, ("Angle" | "Square"), _Multiparticle, ___] |
    (Spinor | SpinorHat)[_, ("Upper" | "Lower"), ("Angle" | "Square"), _Multiparticle, ___]];
  externalLegs = Cases[external, HoldPattern[Mass[i_Integer] -> _] :> i];
  variants = Select[Lookup[references, "Variants", {}],
    TrueQ[Lookup[#, "Accepted", False]] && closedQ[#["Expression"]] &];
  Do[
    If[!TrueQ[Lookup[target, "Complete", False]] || !KeyExistsQ[target, "StateSum"] ||
        TrueQ[residueExactZero[target["StateSum"]]], Continue[]];
    source = residueCanonicalPropagators[target["StateSum"]/target["Propagator"], externalLegs];
    If[closedQ[source] && FreeQ[source, Spinor["Xi", ___] | SpinorHat["Xi", ___]],
      AppendTo[variants, <|"Cut" -> target["Cut"], "Mass" -> target["Mass"],
        "Mode" -> "ReferenceFree", "Expression" -> source|>]], {target, targets}];
  variants = DeleteDuplicatesBy[variants, {#["Cut"], #["Mass"], #["Expression"]} &];
  If[variants === {}, Return[Missing["NoSelectedChannelSources"]]];
  split[expression_] := With[{expanded = withProtectedPropagators[expression, Expand]},
    If[Head[expanded] === Plus, List @@ expanded, {expanded}]];
  Catch[
    finished = TimeConstrained[Do[
      channel = SelectFirst[targets, #["Cut"] === variant["Cut"] &&
        residueExactZero[#["Mass"]^2 - variant["Mass"]^2] &, Missing["UnknownChannel"]];
      If[MissingQ[channel], Continue[]];
      higherPoles = completionHigherPhysicalPoles[variant["Expression"], targets, externalLegs];
      If[FailureQ[higherPoles] || higherPoles =!= {}, Continue[]];
      selectedRules = Join[external,
        {Mass[Multiparticle @@ channel["Cut"]] -> channel["Mass"]}];
      reduced = TimeConstrained[preparationReferenceTrial[
        preparationNumerators[variant["Expression"],
          Function[num, ReduceSpinContractions[num] /. selectedRules]],
        selectedRules, preparationDotRules[selectedRules, True]], 2, $TimedOut];
      If[!residueFiniteQ[reduced], Continue[]];
      higherPoles = completionHigherPhysicalPoles[reduced, targets, externalLegs];
      If[FailureQ[higherPoles] || higherPoles =!= {}, Continue[]];
      verification = completionContinuationCheck[variant["Expression"], reduced,
        channel, targets, rules, seed];
      AppendTo[checks, <|"Cut" -> channel["Cut"], "Mass" -> channel["Mass"],
        "Mode" -> "SelectedCutAtomicReduction", "SourceMode" -> variant["Mode"],
        "MassRules" -> selectedRules, "Verification" -> verification|>];
      If[verification["Status"] =!= "PASS", Continue[]];
      proposalTerms = completionNormalizeBasis[Join[terms, split[reduced]]];
      If[Length[proposalTerms] > limit, Throw[Failure["CorrectionSpaceLimit",
        <|"Terms" -> Length[proposalTerms], "Limit" -> limit|>], tag]];
      terms = proposalTerms; AppendTo[sources, reduced];
      AppendTo[originalSources, <|"Expression" -> reduced, "Channel" -> channel,
        "SourceMode" -> variant["Mode"]|>], {variant, variants}];
    originalFinished = True;
    (* Keep the original certified sources before spending the remaining
       construction budget on optional general-cut continuations. *)
    Do[
      channel = originalSource["Channel"];
      proposals = TimeConstrained[completionCutInvariantTrials[
        originalSource["Expression"], channel["Cut"], channel["Mass"], external],
        2, $TimedOut];
      If[proposals === $TimedOut,
        invariantFinished = False;
        AppendTo[diagnostics, <|"Cut" -> channel["Cut"],
          "Reason" -> "CutInvariantEnumerationTimeout"|>]; Continue[]];
      If[!ListQ[proposals],
        invariantFinished = False;
        AppendTo[diagnostics, <|"Cut" -> channel["Cut"],
          "Reason" -> "CutInvariantEnumerationUnavailable", "Result" -> proposals|>]; Continue[]];
      Do[
        reduced = TimeConstrained[preparationSpinorProducts[proposal["Expression"], external],
          2, $TimedOut];
        If[!residueFiniteQ[reduced],
          invariantFinished = False;
          AppendTo[diagnostics, <|"Cut" -> channel["Cut"],
            "PivotPair" -> proposal["PivotPair"], "Reason" -> "CutInvariantReductionUnavailable"|>];
          Continue[]];
        higherPoles = completionHigherPhysicalPoles[reduced, targets, externalLegs];
        If[!ListQ[higherPoles],
          invariantFinished = False;
          AppendTo[diagnostics, <|"Cut" -> channel["Cut"], "PivotPair" -> proposal["PivotPair"],
            "Reason" -> "CutInvariantPoleClassificationUnavailable"|>]; Continue[]];
        If[higherPoles =!= {},
          AppendTo[diagnostics, <|"Cut" -> channel["Cut"], "PivotPair" -> proposal["PivotPair"],
            "Status" -> "REJECTED", "Reason" -> "CutInvariantHigherOrderPhysicalPoles",
            "HigherOrderPropagators" -> higherPoles|>]; Continue[]];
        verification = completionContinuationCheck[originalSource["Expression"], reduced,
          channel, targets, rules, seed];
        If[!AssociationQ[verification], verification = <|"Status" -> "UNRESOLVED",
          "Reason" -> "CutInvariantVerificationUnavailable"|>];
        AppendTo[checks, <|"Cut" -> channel["Cut"], "Mass" -> channel["Mass"],
          "Mode" -> "SelectedCutInvariantReduction", "SourceMode" -> originalSource["SourceMode"],
          "PivotPair" -> proposal["PivotPair"], "NumeratorRule" -> proposal["NumeratorRule"],
          "Verification" -> verification|>];
        If[!MemberQ[{"PASS", "FAIL"}, Lookup[verification, "Status", None]],
          invariantFinished = False;
          AppendTo[diagnostics, <|"Cut" -> channel["Cut"], "PivotPair" -> proposal["PivotPair"],
            "Reason" -> "CutInvariantVerificationUnresolved"|>]; Continue[]];
        If[verification["Status"] =!= "PASS", Continue[]];
        proposalTerms = completionNormalizeBasis[Join[terms, split[reduced]]];
        If[Length[proposalTerms] > limit,
          invariantFinished = False;
          AppendTo[diagnostics, <|"Cut" -> channel["Cut"], "PivotPair" -> proposal["PivotPair"],
            "Reason" -> "CutInvariantCorrectionSpaceLimit", "Terms" -> Length[proposalTerms],
            "Limit" -> limit|>]; Continue[]];
        terms = proposalTerms; AppendTo[sources, reduced], {proposal, proposals}],
      {originalSource, originalSources}]; True,
      seconds, False];
    If[sources === {}, Return[Missing["NoVerifiedSelectedChannels"]]];
    <|"Basis" -> terms, "Sources" -> DeleteDuplicates[sources], "Verification" -> checks,
      "ConstructionStatus" -> If[TrueQ[finished && invariantFinished], "COMPLETE", "PARTIAL"],
      "OriginalSourcesComplete" -> originalFinished,
      "InvariantContinuationsComplete" -> TrueQ[finished && invariantFinished],
      "Diagnostics" -> Join[diagnostics, If[TrueQ[finished], {}, {
        "The selected-channel construction clock expired; only already certified directions are retained."}]],
      "Scope" -> "SelectedCutPreparedChannelTermsWithUnchangedTargets"|>, tag]
];

(* Alternative representations contribute directions, never new residue targets. *)
completionAlternativeChannelBasis[input_, normalized_Association, rows_List,
    model_List, rules_List, seed_Integer, limit_Integer, references_] := Module[
  {alternate, external, keys, targets, supplied, matched, checks = {}, verification,
   effective, continuations, reduced, basis, tag = Unique["alternativeChannels"]},
  Catch[
  alternate = completionNormalizeInput[input, rows, model];
  If[!AssociationQ[alternate] || FailureQ[alternate] ||
      alternate["ExternalMassRules"] =!= normalized["ExternalMassRules"],
    Throw[<|"Status" -> "UNRESOLVED", "Diagnostics" -> {
      "Alternative channels must describe the same external masses as the supplied channels."}|>, tag]];
  external = Cases[normalized["ExternalMassRules"], HoldPattern[Mass[i_Integer] -> _] :> i];
  keys = DeleteDuplicates[({canonicalPhysicalCut[List @@ #[[-3]], external], #[[-2]]} &) /@ rows];
  targets = completionTargets[normalized["Targets"]];
  supplied = Select[completionTargets[alternate["Targets"]],
    MemberQ[keys, residueChannelKey[#]] &];
  Do[
    matched = SelectFirst[targets, residueChannelKey[#] === residueChannelKey[channel] &,
      Missing["UnknownAlternativeChannel"]];
    If[MissingQ[matched] || !TrueQ[Lookup[matched, "Complete", False]] ||
        !TrueQ[Lookup[channel, "Complete", False]],
      Throw[<|"Status" -> "UNRESOLVED", "Verification" -> checks, "Diagnostics" -> {
        "An alternative channel has no complete matching supplied residue target."}|>, tag]];
    verification = completionContinuationCheck[matched["StateSum"]/matched["Propagator"],
      channel["StateSum"]/channel["Propagator"], matched, targets, rules, seed];
    AppendTo[checks, <|"Cut" -> channel["Cut"], "Mass" -> channel["Mass"],
      "Verification" -> verification|>];
    If[verification["Status"] =!= "PASS",
      Throw[<|"Status" -> "UNRESOLVED", "Verification" -> checks, "Diagnostics" -> {
        "Alternative channels did not preserve the supplied selected residues at the exact samples."}|>, tag]],
    {channel, supplied}];
  effective = Join[normalized, <|"Targets" -> Map[Function[channel,
    SelectFirst[supplied, residueChannelKey[#] === residueChannelKey[channel] &, channel]], targets]|>];
  continuations = If[references === Automatic,
    completionChannelContinuations[effective, model, rules, seed],
    <|"Sources" -> (#["StateSum"]/#["Propagator"] & /@ supplied), "Variants" -> {}|>];
  reduced = If[references === Automatic,
    completionReducedChannelBasis[effective, continuations, limit, rules, seed],
    Missing["NotRequested"]];
  basis = If[AssociationQ[reduced], reduced["Basis"],
    completionAutomaticBasis[effective, limit, continuations]];
  If[!ListQ[basis], Throw[<|"Status" -> "UNRESOLVED", "Verification" -> checks,
    "Diagnostics" -> {"Alternative channel directions exceed MaxCorrectionTerms."}|>, tag]];
  <|"Status" -> "COMPLETE", "Basis" -> basis, "Verification" -> checks,
    "ChannelContinuations" -> continuations, "ReducedChannelSpace" -> reduced,
    "Scope" -> "ExactSampledSelectedCutEquivalenceWithUnchangedResidueTargets",
    "Diagnostics" -> {}|>, tag]
];

Options[CompleteFactorization] = {"CorrectionBasis" -> Automatic,
  "ParameterRules" -> Automatic, "Samples" -> 4, "ValidationSamples" -> 4,
  "ValidationParameterRules" -> Automatic, "ChannelReferences" -> Automatic,
  "AlternativeChannels" -> {}, "BoseSymmetryGroups" -> {},
  "MaxRefinements" -> 3,
  "RandomSeed" -> 1729, "TimeConstraint" -> 60, "MaxCorrectionTerms" -> 64,
  "ProgressReporting" -> True, "ProgressFunction" -> Automatic,
  "ReturnDetails" -> False, "Output" -> "Expression"};

CompleteFactorization[input_, channelRows_List, model_List, opts : OptionsPattern[]] := Module[
  {output = OptionValue["Output"], details = OptionValue["ReturnDetails"],
   basisOption = OptionValue["CorrectionBasis"],
   references = OptionValue["ChannelReferences"], alternatives = OptionValue["AlternativeChannels"],
   boseGroups = OptionValue["BoseSymmetryGroups"], bosePlan, constructionLimit, project, projectedSeed,
   refinements = OptionValue["MaxRefinements"],
   rules = residueParameterRules[model, OptionValue["ParameterRules"]], samples = OptionValue["Samples"],
   validationSamples = OptionValue["ValidationSamples"], seed = OptionValue["RandomSeed"],
   validationRules = OptionValue["ValidationParameterRules"], validationSets, validationReports = {},
   seconds = OptionValue["TimeConstraint"], limit = OptionValue["MaxCorrectionTerms"],
   enabled = OptionValue["ProgressReporting"], callback = OptionValue["ProgressFunction"],
   normalized, support, checkRows, original, candidate, family, remaining = {}, basis = {}, unknowns = {},
   constraints, solution, validation, initial, report, finished, start = AbsoluteTime[], free = {},
   diagnostics = {}, status = "UNRESOLVED", continuations = <||>, history = {},
   trial, allEquations = {}, allSamples = {}, feedback = <||>, roundReports, round,
   validationSeed, validationCount, familyPass, zeroFree, cleaned, cleanup, fastPath, sparseValidation,
   weights, familyMaterialized = False,
   reducedChannels = Missing["NotRequested"], selectedChannels = Missing["NotRequested"],
   space = "NotRequired", fallbackBasis, literalBasis, channelLiteralBasis, groupedBasis,
   budgetSpaces = {},
   fallbackReport, fallbackOptions, remainingSeconds, attemptSummary, alternativeSpace},
  If[!MemberQ[{"Expression", "Details"}, output] || !MemberQ[{True, False}, details] ||
      !(basisOption === Automatic || ListQ[basisOption]) || !completionParameterRulesQ[rules] ||
      !IntegerQ[samples] || samples < 1 ||
      !IntegerQ[validationSamples] || validationSamples < 1 || !IntegerQ[seed] ||
      !MemberQ[{Automatic, None}, references] || !IntegerQ[refinements] || refinements < 0 ||
      !ListQ[alternatives] || !AllTrue[alternatives, preparationChannelQ] ||
      !(validationRules === Automatic || (ListQ[validationRules] &&
        AllTrue[validationRules, completionParameterRulesQ])) ||
      !(seconds === Infinity || TrueQ[NumericQ[seconds] &&
        Element[seconds, Reals] && 0 < seconds < Infinity]) ||
      !IntegerQ[limit] || limit < 1 || !MemberQ[{True, False}, enabled] ||
      !(callback === Automatic || MatchQ[callback, _Function | _Symbol]) ||
      Complement[First /@ {opts}, First /@ Options[CompleteFactorization]] =!= {},
    Message[CompleteFactorization::args]; Return[$Failed]];
  If[TrueQ[details], output = "Details"];
  normalized = TimeConstrained[completionNormalizeInput[input, channelRows, model], seconds,
    Missing["CompletionInputTimeout"]];
  If[MissingQ[normalized], Return[completionFormat[<|"Stage" -> "Factorization",
    "Status" -> "UNRESOLVED", "Amplitude" -> completionInputExpression[input],
    "OriginalInput" -> input, "Correction" -> 0, "FreeParameters" -> {},
    "Diagnostics" -> {"Input preparation exceeded TimeConstraint; the original input is retained."}|>, output, False]]];
  If[!AssociationQ[normalized] || FailureQ[normalized],
    Message[CompleteFactorization::args]; Return[$Failed]];
  bosePlan = completionBosePlan[boseGroups, normalized["ExternalMassRules"]];
  If[FailureQ[bosePlan], Return[bosePlan]];
  constructionLimit = If[bosePlan["Order"] === 1, limit,
    Max[limit, Min[4096, limit bosePlan["Order"]]]];
  project[value_] := If[bosePlan["Order"] === 1, value,
    Expand[canonicalChains[residueCanonicalPropagators[completionBoseProject[value, bosePlan],
      Cases[normalized["ExternalMassRules"], HoldPattern[Mass[i_Integer] -> _] :> i]]]]];
  original = normalized["Expression"]; candidate = original; family = original;
  projectedSeed = original;
  checkRows = completionCheckChannelRows[normalized, channelRows];
  validationSets = completionValidationRuleSets[model, OptionValue["ParameterRules"], validationRules];
  initial = Missing["NotChecked"]; validation = Missing["NotChecked"];
  constraints = <||>; solution = <||>;
  cleanup[value_] := Module[{reduced},
    reduced = TimeConstrained[completionExternalReduce[value, normalized["ExternalMassRules"]],
      2, value];
    If[residueFiniteQ[reduced] && !FailureQ[reduced], reduced, value]];
  completionProgress[callback, enabled, <|"Stage" -> "Checking initial factorization"|>];
  finished = TimeConstrained[
    If[bosePlan["Order"] > 1,
      completionProgress[callback, enabled, <|"Stage" -> "Averaging identical boson labels",
        "Total" -> bosePlan["Order"]|>];
      projectedSeed = project[original]; candidate = projectedSeed; family = projectedSeed];
    initial = If[bosePlan["Order"] > 1,
      completionSparseValidation[projectedSeed, {}, <|"SolutionRules" -> {}|>, {}, {},
        normalized, validationSamples, seed + 1000003, rules, seconds, callback, enabled]["Report"],
      CheckAmplitudes[{normalized["ExternalMassRules"], projectedSeed}, checkRows, model,
      "ParameterRules" -> rules, "Samples" -> validationSamples,
      "RandomSeed" -> seed + 1000003, "TimeConstraint" -> seconds, "Tolerance" -> 0,
      "KinematicMap" -> Automatic, "KinematicKind" -> "Sample",
      "SpuriousPoles" -> {}, "HighEnergyCheck" -> False, "ReferenceCheck" -> False,
      "Output" -> "Details"]];
    fastPath = (basisOption === Automatic || basisOption === {}) &&
      completionPhysicalPassQ[initial, normalized["Targets"]];
    If[fastPath,
      status = "COMPLETE"; familyMaterialized = True;
      Do[
        validation = If[bosePlan["Order"] > 1,
          completionSparseValidation[candidate, {}, <|"SolutionRules" -> {}|>, {}, {},
            normalized, validationSamples, seed + 1000003, validationSet,
            seconds, callback, enabled]["Report"],
          CheckAmplitudes[{normalized["ExternalMassRules"], candidate},
          checkRows, model, "ParameterRules" -> validationSet,
          "Samples" -> validationSamples, "RandomSeed" -> seed + 1000003,
          "TimeConstraint" -> seconds, "Tolerance" -> 0, "SpuriousPoles" -> {},
          "KinematicMap" -> Automatic, "KinematicKind" -> "Sample",
          "HighEnergyCheck" -> False, "ReferenceCheck" -> False, "Output" -> "Details"]];
        AppendTo[validationReports, <|"ParameterRules" -> validationSet,
          "RandomSeed" -> seed + 1000003, "Round" -> 0, "Report" -> validation|>];
        If[!completionPhysicalPassQ[validation, normalized["Targets"]],
          status = "UNRESOLVED";
          AppendTo[history, <|"Round" -> 0, "Status" -> "InitialParameterValidationFailed"|>];
          Break[]], {validationSet, validationSets}]];
    If[!fastPath || status =!= "COMPLETE",
      status = "UNRESOLVED"; familyMaterialized = False;
      If[basisOption === Automatic && references === Automatic,
        completionProgress[callback, enabled, <|"Stage" -> "Constructing channel continuations"|>];
        continuations = completionChannelContinuations[normalized, model, rules, seed];
        completionProgress[callback, enabled, <|"Stage" -> "Reducing channel correction terms"|>];
        reducedChannels = completionReducedChannelBasis[normalized, continuations,
          constructionLimit, rules, seed]];
      space = If[basisOption =!= Automatic, "SuppliedTerms",
        If[AssociationQ[reducedChannels], "ReducedChannels", "LiteralTerms"]];
      basis = If[basisOption === Automatic,
        If[AssociationQ[reducedChannels], reducedChannels["Basis"],
          completionAutomaticBasis[normalized, constructionLimit,
            If[references === Automatic, continuations, None]]],
        completionNormalizeBasis[basisOption]];
      If[basisOption === Automatic && bosePlan["Order"] > 1 && AssociationQ[reducedChannels],
        literalBasis = completionAutomaticBasis[normalized, constructionLimit, continuations];
        If[ListQ[literalBasis],
          basis = completionNormalizeBasis[Join[basis, literalBasis]];
          space = "BoseChannelUnion"];
        completionProgress[callback, enabled,
          <|"Stage" -> "Preparing selected-cut correction terms"|>];
        selectedChannels = completionSelectedChannelBasis[normalized, continuations,
          constructionLimit, rules, seed];
        If[AssociationQ[selectedChannels],
          basis = completionNormalizeBasis[Join[basis, selectedChannels["Basis"]]];
          space = "BoseSelectedChannelUnion"]];
      If[ListQ[basis] && bosePlan["Order"] > 1,
        completionProgress[callback, enabled,
          <|"Stage" -> "Reducing Bose-symmetric correction directions",
            "Terms" -> Length[basis], "Permutations" -> bosePlan["Order"]|>];
        basis = completionIndependentBasis[completionNormalizeBasis[project /@ basis]]];
      If[ListQ[basis] && Length[basis] > limit && basisOption === Automatic &&
          bosePlan["Order"] > 1 && AssociationQ[reducedChannels],
        AppendTo[budgetSpaces, <|"CorrectionSpace" -> space, "Terms" -> Length[basis],
          "WithinLimit" -> False|>];
        completionProgress[callback, enabled,
          <|"Stage" -> "Trying grouped input correction directions", "Terms" -> Length[basis]|>];
        (* Preserve the full space whenever it fits. This bounded alternative
           retains whole input diagrams and every available channel atom. *)
        channelLiteralBasis = completionAutomaticBasis[
          Join[normalized, <|"Expression" -> 0|>], constructionLimit, continuations];
        If[ListQ[channelLiteralBasis],
          groupedBasis = completionNormalizeBasis[Join[reducedChannels["Basis"],
            channelLiteralBasis, If[AssociationQ[selectedChannels], selectedChannels["Basis"], {}]]];
          groupedBasis = completionIndependentBasis[
            completionNormalizeBasis[project /@ groupedBasis]];
          AppendTo[budgetSpaces, <|"CorrectionSpace" -> "BoseGroupedInputChannelUnion",
            "Terms" -> Length[groupedBasis], "WithinLimit" -> (Length[groupedBasis] <= limit)|>];
          If[Length[groupedBasis] <= limit,
            basis = groupedBasis; space = "BoseGroupedInputChannelUnion"],
          AppendTo[budgetSpaces, <|"CorrectionSpace" -> "BoseGroupedInputChannelUnion",
            "Available" -> False, "WithinLimit" -> False,
            "Reason" -> "Channel terms exceed the intermediate source-space limit."|>]]];
      If[FailureQ[basis], AppendTo[diagnostics,
          "The channel-derived correction space exceeds MaxCorrectionTerms."],
        If[Length[basis] > limit, AppendTo[diagnostics,
          If[basisOption === Automatic, "The channel-derived correction space exceeds MaxCorrectionTerms.",
            "The supplied correction space exceeds MaxCorrectionTerms."]],
          basis = completionNormalizeBasis[
            completionCanonicalReduce[#, normalized["ExternalMassRules"]] & /@ basis];
          unknowns = Table[Unique["factorizationCoefficient"], {Length[basis]}];
          trial = completionCanonicalReduce[projectedSeed, normalized["ExternalMassRules"]] + unknowns.basis;
          (* A zero-parameter candidate can hide poles present in its free directions. *)
          support = completionSupportInputs[trial, normalized["ExternalMassRules"], channelRows, model];
          If[AssociationQ[support] && !FailureQ[support],
            normalized = Join[normalized, <|"Targets" ->
              Join[normalized["Targets"], Select[support["Targets"],
                !MemberQ[residueChannelKey /@ normalized["Targets"], residueChannelKey[#]] &]]|>];
            checkRows = completionCheckChannelRows[normalized, channelRows];
            constraints = completionResidueConstraints[trial, unknowns, normalized,
              samples, seed, rules, seconds, callback, enabled],
            constraints = <|"Status" -> "UNRESOLVED", "Equations" -> {}, "Samples" -> {},
              "Diagnostics" -> {"Physical poles in the correction directions could not be identified."}|>];
          If[constraints["Status"] =!= "COMPLETE",
            diagnostics = Join[diagnostics, constraints["Diagnostics"]],
            allEquations = constraints["Equations"]; allSamples = constraints["Samples"];
            Do[
              completionProgress[callback, enabled, <|"Stage" -> "Solving factorization",
                "Round" -> round, "Unknowns" -> If[round === 0, Length[unknowns],
                  Length[solution["FreeParameters"]]],
                "EquationCount" -> Length[allEquations]|>];
              (* Refit the original sparse equations. Substituting a dense
                 partial solution into new rows causes large exact fractions. *)
              solution = completionSolveLinear[allEquations, unknowns,
                If[seconds === Infinity, 900,
                  Min[900, Max[0, seconds - (AbsoluteTime[] - start)]]]];
              If[!MemberQ[{"COMPLETE", "UNDERDETERMINED"}, solution["Status"]],
                AppendTo[history, <|"Round" -> round, "FitSeed" -> seed,
                  "EquationCount" -> Length[allEquations], "Rank" -> solution["Rank"],
                  "Status" -> solution["Status"]|>];
                diagnostics = Join[diagnostics, solution["Diagnostics"]]; Break[]];
              free = solution["FreeParameters"];
              zeroFree = {};
              validationSeed = seed + 1000003 + 10000019 round;
              validationCount = validationSamples (round + 1); roundReports = {};
              completionProgress[callback, enabled, <|"Stage" -> "Validating factorization",
                "Round" -> round, "Unknowns" -> Length[free],
                "EquationCount" -> Length[allEquations]|>];
              feedback = <|"Status" -> "COMPLETE", "Equations" -> {}, "Samples" -> {},
                "Diagnostics" -> {}|>; familyPass = True;
              Do[
                sparseValidation = completionSparseValidation[trial, unknowns, solution,
                  free, zeroFree, normalized, validationCount, validationSeed, validationSet,
                  seconds, callback, enabled];
                validation = sparseValidation["Report"];
                feedback = <|"Status" -> If[feedback["Status"] === "COMPLETE" &&
                    sparseValidation["Feedback"]["Status"] === "COMPLETE", "COMPLETE", "UNRESOLVED"],
                  "Equations" -> Join[feedback["Equations"], sparseValidation["Feedback"]["Equations"]],
                  "Samples" -> Join[feedback["Samples"], sparseValidation["Feedback"]["Samples"]],
                  "Diagnostics" -> Join[feedback["Diagnostics"], sparseValidation["Feedback"]["Diagnostics"]]|>;
                familyPass = familyPass && TrueQ[sparseValidation["FamilyPass"]];
                AppendTo[roundReports, <|"ParameterRules" -> validationSet,
                  "RandomSeed" -> validationSeed, "Round" -> round, "Report" -> validation|>],
                {validationSet, validationSets}];
              validationReports = Join[validationReports, roundReports];
              AppendTo[history, <|"Round" -> round, "FitSeed" -> seed,
                "ValidationSeed" -> validationSeed, "ValidationSamples" -> validationCount,
                "EquationCount" -> Length[allEquations], "Rank" -> solution["Rank"],
                "FreeParameters" -> free, "RemovedZeroDirections" -> Length[zeroFree],
                "Status" -> If[AllTrue[roundReports,
                    completionPhysicalPassQ[#["Report"], normalized["Targets"]] &] &&
                  familyPass, "COMPLETE", "NeedsRefinement"]|>];
              If[AllTrue[roundReports,
                  completionPhysicalPassQ[#["Report"], normalized["Targets"]] &] && familyPass,
                completionProgress[callback, enabled,
                  <|"Stage" -> "Constructing validated factorization amplitude",
                    "Unknowns" -> Length[free]|>];
                weights = unknowns /. solution["SolutionRules"];
                candidate = (trial /. Thread[unknowns -> 0]) +
                  (weights /. Thread[free -> 0]).basis;
                remaining = (Function[parameter,
                  (Coefficient[#, parameter] & /@ weights).basis] /@ free);
                zeroFree = Pick[free, (# === 0 &) /@ remaining];
                free = Pick[free, (# =!= 0 &) /@ remaining];
                remaining = DeleteCases[remaining, 0];
                family = candidate + If[free === {}, 0, free.remaining];
                familyMaterialized = True;
                history[[-1]] = Join[Last[history], <|"FreeParameters" -> free,
                  "RemovedZeroDirections" -> Length[zeroFree]|>];
                status = "COMPLETE";
                Break[]];
              If[feedback["Status"] =!= "COMPLETE",
                diagnostics = Join[diagnostics, feedback["Diagnostics"]]; Break[]];
              If[round === refinements,
                AppendTo[diagnostics,
                  "Independent cut or affine-family validation still fails at MaxRefinements."];
                Break[]];
              allEquations = Join[allEquations, feedback["Equations"]];
              allSamples = Join[allSamples, feedback["Samples"]];
              constraints = Join[constraints, <|"Equations" -> allEquations,
                "Samples" -> allSamples|>];
              completionProgress[callback, enabled, <|"Stage" -> "Refining factorization",
                "Round" -> round + 1, "EquationCount" -> Length[allEquations],
                "Unknowns" -> Length[unknowns]|>],
              {round, 0, refinements}];
            constraints = Join[constraints, <|"Equations" -> allEquations,
              "Samples" -> allSamples|>]]]]]; True,
    If[seconds === Infinity, Infinity, Max[0.000001, seconds - (AbsoluteTime[] - start)]], False];
  If[!TrueQ[finished], status = "UNRESOLVED";
    AppendTo[diagnostics, "Factorization completion exceeded TimeConstraint."]];
  If[!familyMaterialized, free = {}; remaining = {}; family = candidate];
  (* Cosmetic reductions share one small budget after validation. A timeout
     must retain the validated family rather than invalidate its success. *)
  If[status === "COMPLETE" && history =!= {},
    remainingSeconds = If[seconds === Infinity, 2, Min[2, seconds - (AbsoluteTime[] - start)]];
    If[remainingSeconds > 0,
      cleaned = TimeConstrained[{cleanup[candidate], cleanup /@ remaining},
        remainingSeconds, $TimedOut];
      If[MatchQ[cleaned, {_, _List}],
        {candidate, remaining} = cleaned;
        zeroFree = Pick[free, (# === 0 &) /@ remaining];
        free = Pick[free, (# =!= 0 &) /@ remaining];
        remaining = DeleteCases[remaining, 0];
        family = candidate + If[free === {}, 0, free.remaining];
        history[[-1]] = Join[Last[history], <|"FreeParameters" -> free,
          "RemovedZeroDirections" -> Last[history]["RemovedZeroDirections"] + Length[zeroFree]|>]]]];
  report = <|"Stage" -> "Factorization", "Status" -> status,
    "Amplitude" -> candidate, "OriginalExpression" -> original,
    "Correction" -> candidate - original, "CorrectionBasis" -> basis,
    "FamilyAmplitude" -> family, "RemainingBasis" -> remaining, "FreeParameters" -> free,
    "ExternalMassRules" -> normalized["ExternalMassRules"],
    "InitialReport" -> initial, "ValidationReport" -> validation,
    "ParameterRules" -> rules,
    "ValidationReports" -> validationReports, "ValidationParameterRules" -> validationSets,
    "ConstraintData" -> <|"Fit" -> constraints, "LinearSolution" -> solution,
      "Validation" -> feedback|>, "RefinementHistory" -> history,
    "ChannelContinuations" -> continuations,
    "BoseSymmetry" -> KeyTake[bosePlan, {"Groups", "Order"}],
    "CorrectionSpace" -> space, "ReducedChannelSpace" -> reducedChannels,
    "SelectedChannelSpace" -> selectedChannels,
    "CorrectionSpaceCandidates" -> budgetSpaces,
    "Scope" -> "BoundedAdaptiveLinearCompletionAtExactSampledCutsWithHeldOutValidation",
    "ElapsedTime" -> AbsoluteTime[] - start, "Diagnostics" -> Join[diagnostics, {
      "Factorization completion alone does not assert high-energy or reference independence. Remaining residue-preserving directions are retained for the next stage.",
      "A fitted sampled correction is not a generic analytic uniqueness proof."}]|>;
  If[status =!= "COMPLETE" && TrueQ[finished] && space === "ReducedChannels",
    remainingSeconds = If[seconds === Infinity, Infinity, seconds - (AbsoluteTime[] - start)];
    If[remainingSeconds > 0.01,
      fallbackBasis = TimeConstrained[completionAutomaticBasis[normalized, constructionLimit, continuations],
        remainingSeconds, $TimedOut];
      If[ListQ[fallbackBasis] && fallbackBasis =!= basis &&
          (seconds === Infinity || seconds - (AbsoluteTime[] - start) > 0.01),
        completionProgress[callback, enabled, <|"Stage" -> "Trying literal channel correction terms"|>];
        fallbackOptions = Join[Association[Options[CompleteFactorization]], Association[{opts}],
          <|"CorrectionBasis" -> fallbackBasis, "AlternativeChannels" -> {}, "ReturnDetails" -> True,
            "TimeConstraint" -> If[seconds === Infinity, Infinity,
              Max[0.000001, seconds - (AbsoluteTime[] - start)]]|>];
        attemptSummary = KeyTake[report, {"Status", "CorrectionSpace", "Diagnostics", "ElapsedTime",
          "RefinementHistory"}];
        fallbackReport = CompleteFactorization[input, channelRows, model,
          Sequence @@ Normal[fallbackOptions]];
        If[AssociationQ[fallbackReport],
          report = Join[fallbackReport, <|"CorrectionSpace" -> "LiteralTerms",
            "CorrectionSpaceAttempts" -> {attemptSummary,
              KeyTake[fallbackReport, {"Status", "Diagnostics", "ElapsedTime", "RefinementHistory"}]},
            "ReducedChannelSpace" -> reducedChannels, "ChannelContinuations" -> continuations,
            "ElapsedTime" -> AbsoluteTime[] - start|>];
          status = report["Status"]]]]];
  If[status =!= "COMPLETE" && basisOption === Automatic && alternatives =!= {},
    remainingSeconds = If[seconds === Infinity, Infinity, seconds - (AbsoluteTime[] - start)];
    If[remainingSeconds > 0.01,
      completionProgress[callback, enabled, <|"Stage" -> "Checking alternative channel representations"|>];
      alternativeSpace = TimeConstrained[completionAlternativeChannelBasis[input, normalized,
        alternatives, model, rules, seed, constructionLimit, references],
        remainingSeconds, <|"Status" -> "UNRESOLVED", "Diagnostics" -> {
          "Alternative channel preparation exceeded the remaining TimeConstraint."}|>];
      report = Join[report, <|"AlternativeChannelFallback" -> alternativeSpace|>];
      If[alternativeSpace["Status"] === "COMPLETE" &&
          (seconds === Infinity || seconds - (AbsoluteTime[] - start) > 0.01),
        completionProgress[callback, enabled, <|"Stage" -> "Trying verified alternative channel directions"|>];
        fallbackOptions = Join[Association[Options[CompleteFactorization]], Association[{opts}],
          <|"CorrectionBasis" -> alternativeSpace["Basis"], "AlternativeChannels" -> {},
            "ReturnDetails" -> True, "TimeConstraint" -> If[seconds === Infinity, Infinity,
              Max[0.000001, seconds - (AbsoluteTime[] - start)]]|>];
        attemptSummary = KeyTake[report, {"Status", "CorrectionSpace", "Diagnostics", "ElapsedTime",
          "RefinementHistory"}];
        fallbackReport = CompleteFactorization[input, channelRows, model,
          Sequence @@ Normal[fallbackOptions]];
        If[AssociationQ[fallbackReport],
          report = Join[fallbackReport, <|"CorrectionSpace" -> "AlternativeChannels",
            "AlternativeChannelFallback" -> alternativeSpace,
            "CorrectionSpaceAttempts" -> Join[Lookup[report, "CorrectionSpaceAttempts", {attemptSummary}],
              {KeyTake[fallbackReport, {"Status", "Diagnostics", "ElapsedTime", "RefinementHistory"}]}],
            "ElapsedTime" -> AbsoluteTime[] - start|>];
          status = report["Status"]]]]];
  report = Join[report, <|"ElapsedTime" -> AbsoluteTime[] - start|>];
  completionProgress[callback, enabled, <|"Stage" -> "Factorization " <> status,
    "Elapsed" -> report["ElapsedTime"], "Unknowns" -> If[ListQ[report["CorrectionBasis"]],
      Length[report["CorrectionBasis"]], Length[unknowns]]|>];
  completionFormat[report, output, False]
];
CompleteFactorization[___] := (Message[CompleteFactorization::args]; $Failed);

End[];
End[];


(* ::Section::Closed:: *)
(*Amplitude Preparation*)

Begin["ConstructiveDiagrams`"];

PrepareAmplitudes::usage = "PrepareAmplitudes[items, model] prepares diagram amplitude pairs or compact factorization-channel rows, preserving their input formats. Each element is prepared independently by ReduceSpinContractions, ReferenceSpinors replacement and ReduceSpinorProducts with physical propagators protected. AllSpurious compares eligible other-massless-particle references, AllPhysical compares momentum-insertion references giving physical poles outside that element's own cuts, and Mixed compares both types per external leg. Automatic optimizes spurious references where available, otherwise physical references. Cyclic and ReverseCyclic retain fixed reproducible spurious choices; Xi uses one common Xi0, IndependentXi one generic reference per external particle, and None retains input references. The best completed trial is ranked after ordinary spinor reduction and diagram-local numerator-shell reductions. MaxReferenceChoices -> 64 bounds trials; ReferenceTimeConstraint -> Automatic uses at most five seconds and one third of the remaining per-element TimeConstraint, shared with subsequent preparation. This is a bounded finite-reference search, not a global-minimum claim. ReturnDetails -> False is the default; True exposes inputs, assignments, tested alternatives, search limits, reductions and diagnostics. InternalOnShell -> True reduces known numerator scalar products using only that element's mass rules, including two-leg complements when MomentumConservation is True, before and after spinor reduction even at MaxSteps -> 0. False disables these internal-shell reductions and edits. Unknown invariants and denominators are not put on shell. A bounded improving identity search follows. Output -> \"Expression\" returns the prepared sum; \"Amplitudes\" preserves input formats; \"Details\" and ReturnDetails -> True request diagnostics. ReturnDetails takes precedence. Unavailable reference choices or an overall timeout remain PARTIAL. ProgressReporting, ProgressFunction, MaxSteps, MaxCandidates and MomentumConservation control preparation. No model vertices or factorization corrections are added.";
PrepareAmplitudes::args = "Expected diagram {massRules, expression} pairs or compact factorization-channel rows, a model, and valid preparation options. All elements must declare consistent external masses.";
PrepareAmplitudes::usage = PrepareAmplitudes::usage <>
  " Ordinary ReduceSpinorProducts acts on the whole expression, including auxiliary denominators, with PropDen protected. Only external mass replacements act globally; internal replacements remain numerator-only. With MomentumConservation -> True, the bounded identity search also tries consistent whole-numerator external momentum eliminations and composite complements. Each trial is reduced before a strictly improving whole-expression rank is required. These trials use only declared external mass labels, share MaxSteps, MaxCandidates and the per-element time limit, and protect inverse factors from conservation and internal-shell substitutions. MomentumConservation -> False disables them.";
PrepareAmplitudes::usage = PrepareAmplitudes::usage <>
  " Input elements are detected individually: diagram {massRules, expression} pairs or compact factorization rows {m1, ..., mn, Multiparticle[cut], exchangedMass, expression}. The default output preserves each input element's format and metadata. Factorization rows supply only external shells and their selected cut; other propagators do not introduce shell assumptions. Channel 1/z factors and sewing signs are unchanged. Cut-only reductions preserve the selected residue, not an off-cut identity. Details add InputType, ElementTypes, ItemReports and ChannelReports.";
PrepareAmplitudes::usage = PrepareAmplitudes::usage <>
  " An unavailable reference choice returns Failure for ordinary output, for diagrams and channels alike; detailed output retains PARTIAL expressions and diagnostics. AllSpurious needs another eligible massless external particle. With a single external photon, use Automatic or AllPhysical for constructive preparation.";
PrepareAmplitudes::usage = PrepareAmplitudes::usage <>
  " With a FeynmanModel, only diagram pairs are accepted. Raw Feynman tensors are converted to spinor products before reference replacement and reduction; FermionHelicities supplies the helicities of massless Dirac external fields. Feynman preparation defaults to ordinary reduction with effective MaxSteps -> 0 unless MaxSteps is supplied explicitly; eager identity-route generation for large chains is opt-in. Feynman preparation always keeps every internal numerator and denominator off shell, regardless of InternalOnShell, and returns external-only mass rules. Existing propagators supply a separate reference-choice inventory, never shell assumptions. All Feynman diagrams use the same reference assignment per external leg: Automatic uses a fixed massless-particle cycle, or independent generic Xi references when fewer than two massless particles are available. AllSpurious and cyclic choices are deterministic rather than optimized separately per diagram. AllPhysical uses deterministic momentum-insertion references from the shared process pole inventory, preferring massive external targets; its reference poles may coincide with a diagram's existing propagators. Xi, IndependentXi and None retain their meanings. Shared Mixed Feynman choices remain unimplemented and PARTIAL. Failed or timed-out conversion, or an unavailable requested reference choice, remains PARTIAL in details and returns Failure for ordinary output.";
PrepareDiagramAmplitudes::usage = "PrepareDiagramAmplitudes is a compatibility alias for PrepareAmplitudes. Use PrepareAmplitudes for both diagram amplitude pairs and compact factorization-channel rows.";

Options[PrepareAmplitudes] = {
  "ReferenceSpinors" -> Automatic,
  "FermionHelicities" -> {},
  "MaxReferenceChoices" -> 64, "ReferenceTimeConstraint" -> Automatic,
  "InternalOnShell" -> True, "MaxSteps" -> 12, "MaxCandidates" -> 64,
  "TimeConstraint" -> 20, "MomentumConservation" -> True,
  "ProgressReporting" -> True, "ProgressFunction" -> Automatic,
  "ReturnDetails" -> False, "Output" -> "Amplitudes"};
Options[PrepareDiagramAmplitudes] = Options[PrepareAmplitudes];

Begin["ConstructiveDiagrams`Private`"];

preparationModelQ[model_] := ListQ[model] && Length[model] >= 2 &&
  ListQ[model[[1]]] && ListQ[model[[2]]] &&
  AllTrue[model[[1]], ListQ[#] && Length[#] >= 6 &&
    StringQ[#[[2]]] && StringQ[#[[3]]] &];

preparationPairQ[pair_] := MatchQ[pair, {_List, _}] &&
  residueFiniteQ[Last[pair]] && AllTrue[First[pair],
    MatchQ[#, HoldPattern[Mass[_Integer | Multiparticle[__Integer]] -> _]] &&
      residueFiniteQ[Last[#]] &];

preparationChannelQ[row_] := Module[{n, cut},
  If[!ListQ[row] || Length[row] < 7 || !MatchQ[row[[-3]], Multiparticle[__Integer]], Return[False]];
  n = Length[row] - 3; cut = List @@ row[[-3]];
  residueFiniteQ[row] && DuplicateFreeQ[cut] && 2 <= Length[cut] <= n - 2 &&
    AllTrue[cut, 1 <= # <= n &] &&
    AllTrue[Join[Take[row, n], {row[[-2]]}], FreeQ[#,
      _List | _Rule | _RuleDelayed | _Multiparticle | _Mom | _Spinor | _SpinorChain | _PropDen] &]
];

preparationInputType[item_] := Which[
  preparationPairQ[item], "Diagram",
  preparationChannelQ[item], "FactorizationChannel",
  True, $Failed];

preparationInputPair[item_, "Diagram"] := item;
preparationInputPair[row_, "FactorizationChannel"] := {
  Join[Thread[(Mass /@ Range[Length[row] - 3]) -> Drop[row, -3]],
    {Mass[row[[-3]]] -> row[[-2]]}], row[[-1]]};

preparationOutputItem[item_, "Diagram", expression_] := {First[item], expression};
preparationOutputItem[item_, "FeynmanDiagram", expression_] := {
  feynmanExternalMassRules[First[item]], expression};
preparationOutputItem[row_, "FactorizationChannel", expression_] := ReplacePart[row, -1 -> expression];

(* This inventory controls references only, not numerator shell reductions. *)
preparationFeynmanReferencePairs[pairs_List] := Map[Function[pair,
  With[{external = Cases[First[pair], HoldPattern[Mass[i_Integer] -> _] :> i]},
    {Join[First[pair], (Mass[Multiparticle @@ First[#]] -> Last[#] &) /@
      DeleteDuplicates[DeleteMissing[residuePropagatorKey[#, external] & /@
        Cases[Last[pair], _PropDen, {0, Infinity}]]]], Last[pair]}]], pairs];

preparationMassKey[Mass[Multiparticle[legs__Integer]]] := Mass[Multiparticle @@ Sort[{legs}]];
preparationMassKey[key_] := key;

preparationMassRulesConsistentQ[rules_List] := AllTrue[
  GatherBy[rules, preparationMassKey[First[#]] &], Function[group,
    AllTrue[Rest[group], residueExactZero[Last[#] - Last[First[group]]] &]]];

preparationCanonicalMassRules[rules_List] := DeleteDuplicatesBy[
  (preparationMassKey[First[#]] -> Last[#] & /@ rules), First];

preparationExternalRulesAgreeQ[first_List, second_List] := Module[{left, right},
  left = SortBy[first, First[#][[1]] &]; right = SortBy[second, First[#][[1]] &];
  (First /@ left) === (First /@ right) &&
    AllTrue[MapThread[Subtract, {Last /@ left, Last /@ right}], residueExactZero]
];

preparationExternalRules[rules_List] := Select[rules,
  MatchQ[#, HoldPattern[Mass[_Integer] -> _]] &];

preparationProtectedDenominators[expr_, function_] := withProtectedPropagators[expr,
  Function[body, Module[{denominators, tokens},
    denominators = DeleteDuplicates[Cases[{body},
      denominator : Power[_, exponent_] /; !TrueQ[exponent >= 0], Infinity]];
    tokens = Table[Unique["preparationDenominator"], {Length[denominators]}];
    function[body /. Thread[denominators -> tokens]] /. Thread[tokens -> denominators]
  ]]];

preparationNumerators[expr_, function_] := withProtectedPropagators[expr,
  Function[protected, Total[Function[term,
      preparationProtectedDenominators[Numerator[term], function]/Denominator[term]] /@
    If[Head[protected] === Plus, List @@ protected, {protected}]]]];

(* Ordinary spinor algebra also acts on auxiliary denominators. Only external
   mass replacements are global; diagram-local replacements stay in numerators. *)
preparationSpinorProducts[expr_, rules_List] := withProtectedPropagators[
  preparationNumerators[expr, Function[num, num /. rules]],
  Function[body, ReduceSpinorProducts[body] /. preparationExternalRules[rules]]];

preparationDotRules[rules_List, conserve_] := Module[
  {channels = knownInternalChannels[rules], external = externalIndices[rules],
   pairs, complement, replacements, masses, value},
  pairs = Select[channels, Length[First[#]] === 2 &];
  If[TrueQ[conserve], Do[
    complement = Complement[external, First[channel]];
    If[ContainsAll[external, First[channel]] && Length[complement] === 2,
      AppendTo[pairs, {complement, Last[channel]}]], {channel, channels}]];
  replacements = Table[
    masses = massValue[rules, #] & /@ First[pair];
    If[AnyTrue[masses, MissingQ], Nothing,
      value = ((Last[pair]^2 - Total[masses^2])/2) /. rules;
      (orderedDot @@ First[pair]) -> value], {pair, pairs}];
  (* Conflicting declarations do not justify a definite pair invariant. *)
  First /@ Select[GatherBy[DeleteDuplicates[replacements], First], Length[#] === 1 &]
];

preparationReduceDots[expr_, replacements_List] := Module[{denominators, tokens},
  If[replacements === {}, Return[expr]];
  (* Protect inverse factors and unknown-sign powers inside unexpanded sums. *)
  denominators = DeleteDuplicates[Cases[{expr},
    denominator : Power[_, exponent_] /; !TrueQ[exponent >= 0], Infinity]];
  tokens = Table[Unique["preparationDenominator"], {Length[denominators]}];
  preparationNumerators[expr /. Thread[denominators -> tokens],
    Function[num, num /. replacements]] /. Thread[tokens -> denominators]
];

preparationRank[expr_] := {SpinorExpressionComplexity[expr],
  If[Head[expr] === Plus, Length[expr], If[expr === 0, 0, 1]], LeafCount[expr]};

preparationBetterQ[left_List, right_List] := left =!= right && OrderedQ[{left, right}];

preparationConservationCandidates[expr_, rules_List] := Module[
  {external = DeleteDuplicates[externalIndices[rules]], momenta, products, used, composites},
  If[Length[external] < 2, Return[{}]];
  {momenta, products} = preparationProtectedDenominators[expr,
    Function[body, {DeleteDuplicates[Cases[body, Mom[label_] :> label, {0, Infinity}]],
      DeleteDuplicates[Cases[body, MomProd[a_, b_] :> {a, b}, {0, Infinity}]]}]];
  used = DeleteDuplicates[Join[momenta, Flatten[products, 1]]];
  used = Select[used, momentumMembers[#] =!= {} &&
    ContainsAll[external, momentumMembers[#]] &];
  composites = Select[momenta, MatchQ[#, _Multiparticle] &&
    momentumMembers[#] =!= {} && ContainsAll[external, momentumMembers[#]] &];
  Join[
    (<|"Name" -> "MomentumConservation", "Identity" -> "ExternalElimination",
      "Legs" -> {#}, "PreparationConservation" -> True|> &) /@
      Select[external, MemberQ[Flatten[momentumMembers /@ used], #] &],
    (<|"Name" -> "MomentumConservation", "Identity" -> "CompositeComplement",
      "Legs" -> momentumMembers[#], "PreparationConservation" -> True|> &) /@ composites]
];

(* Eliminate one momentum consistently across the whole numerator before
   ranking: the expanded substitution may expose distant cancellations. *)
preparationConservationTrial[expr_, edit_Association, rules_List] := Module[
  {external = DeleteDuplicates[externalIndices[rules]], eliminate, members,
   components, momentum, product, rewrite, complement},
  If[edit["Identity"] === "CompositeComplement",
    members = edit["Legs"]; complement = Complement[external, members];
    Return[preparationNumerators[expr, Function[num,
      num /. (HoldPattern[Mom[label_]] /; momentumMembers[label] === members) :>
        -Total[Mom /@ complement]]]]];
  eliminate = First[edit["Legs"]];
  components[label_] := Module[{legs = momentumMembers[label], coefficients},
    If[legs === {} || !ContainsAll[external, legs], Return[Missing["UnsupportedMomentum"]]];
    coefficients = Count[legs, #] - If[MemberQ[legs, eliminate], 1, 0] & /@ external;
    Transpose[{external, coefficients}]];
  momentum[label_] := Module[{parts = components[label]},
    If[MissingQ[parts] || !MemberQ[momentumMembers[label], eliminate], Return[Mom[label]]];
    Total[(Last[#] Mom[First[#]]) & /@ parts]];
  product[a_, b_] := Module[{left = components[a], right = components[b]},
    If[MissingQ[left] || MissingQ[right] ||
        !MemberQ[Join[momentumMembers[a], momentumMembers[b]], eliminate], Return[MomProd[a, b]]];
    Total[Flatten[Table[x[[2]] y[[2]] If[x[[1]] === y[[1]],
      Mass[x[[1]]]^2, orderedDot[x[[1]], y[[1]]]], {x, left}, {y, right}]]]];
  rewrite[num_] := num /. {HoldPattern[Mom[label_]] :> momentum[label],
    HoldPattern[MomProd[a_, b_]] :> product[a, b]};
  preparationNumerators[expr, rewrite]
];

preparationSearchCandidates[expr_, rules_List, shellOptions_List, conserve_, limit_] := Module[
  {local, global, candidates = {}, j = 1},
  local = preparationProtectedDenominators[expr, Function[body,
    SpinorIdentityCandidates[{rules, body}, Sequence @@ shellOptions]]];
  local = SortBy[local, Function[edit,
    If[MemberQ[{"OnShell", "MomentumSquare", "ChainSquare"}, edit["Name"]] &&
      Lookup[edit, "Direction", "Contract"] =!= "Expand", 0, 1]]];
  global = If[TrueQ[conserve], preparationConservationCandidates[expr, rules], {}];
  (* Interleave the bounded alternatives so local shell contractions retain
     budget even for processes with many external legs. *)
  While[Length[candidates] < limit && (j <= Length[local] || j <= Length[global]),
    If[j <= Length[local], AppendTo[candidates, local[[j]]]];
    If[Length[candidates] < limit && j <= Length[global], AppendTo[candidates, global[[j]]]];
    j++];
  <|"Candidates" -> candidates, "Generated" -> Length[local] + Length[global]|>
];

preparationReferenceChoices = {Automatic, "AllSpurious", "AllPhysical", "Mixed", "Xi", None,
  "ReverseCyclic", "IndependentXi", "Cyclic"};

preparationReferences[expr_, rules_List, model_List, pairs_List, requested_, forced_List : {}, shared_ : False] := Module[
  {choice = Replace[requested, "Cyclic" -> "AllSpurious"], references, massRules,
   external, massless, inventory, own, associations = <||>, assignments = {},
   cycle, denominatorChains, tag = Unique["referenceFailure"], fail, key, legs,
   referenceKey, opposite, compatibleMassQ, physicalQ, ownQ,
   choose, cache = <||>, infer, rewrite, transformed, physicalDenominators = {},
   protectedReferences, original = expr, pairChannels, bracketReferenceData,
   choiceGroups = {}},
  fail[message_] := Throw[<|"Status" -> "UNRESOLVED", "Expression" -> original,
    "Assignments" -> assignments, "Diagnostics" -> {message}|>, tag];
  Catch[
    If[!MemberQ[preparationReferenceChoices, requested], fail["Unsupported ReferenceSpinors choice."]];
    If[choice === None, Return[<|"Status" -> "COMPLETE", "Expression" -> expr,
      "Assignments" -> {}, "Diagnostics" -> {}|>]];
    protectedReferences = withProtectedPropagators[expr, Function[body,
      DeleteDuplicates[Cases[body, s : Spinor["Xi", ___] :> s, {0, Infinity}]]]];
    If[!AllTrue[protectedReferences, MatchQ[#, Spinor["Xi", "Angle" | "Square", _, ___]] &],
      fail["An Xi spinor has unsupported chirality or malformed contents."]];
    references = protectedReferences;
    If[references === {}, Return[<|"Status" -> "COMPLETE", "Expression" -> expr,
      "Assignments" -> {}, "Diagnostics" -> {}|>]];
    If[choice === "Xi",
      assignments = (<|"Reference" -> #, "Leg" -> Missing["NotNeeded"],
        "Mode" -> "Xi", "Target" -> ReplacePart[#, 3 -> 0]|> &) /@ references;
      transformed = withProtectedPropagators[expr,
        Function[body, body /. (Rule[#["Reference"], #["Target"]] & /@ assignments)]];
      Return[<|"Status" -> "COMPLETE", "Expression" -> transformed,
        "Assignments" -> assignments, "Diagnostics" -> {}|>]];
    massRules = preparationExternalRules[rules];
    If[!withProtectedPropagators[expr, Function[body,
        FreeQ[body /. _SpinorChain :> Unique["referenceChain"], Spinor["Xi", ___]]]],
      fail["An Xi occurs outside a supported spinor chain."]];
    external = Sort[Cases[massRules, HoldPattern[Mass[i_Integer] -> _] :> i]];
    massless = Select[external, TrueQ[(Mass[#] /. massRules) == 0] &];
    If[TrueQ[shared] && choice === "Mixed",
      fail["Shared Mixed reference choices are not yet supported for Feynman diagrams; use Automatic, AllSpurious, AllPhysical, a cyclic choice, Xi, IndependentXi or None."]];
    If[TrueQ[shared] && requested === Automatic,
      choice = If[Length[massless] < 2, "IndependentXi", "FeynmanCyclic"]];
    compatibleMassQ[a_, b_] := residueExactZero[a^2 - b^2];
    pairChannels[rs_] := Flatten[Function[item,
      {{First[item], Last[item]}, {Complement[external, First[item]], Last[item]}}] /@
      knownInternalChannels[rs], 1];
    inventory = DeleteDuplicates[Flatten[pairChannels[First[#]] & /@ pairs, 1]];
    (* Existing uncut propagators exclude reference choices too, but must not
       become numerator-shell assumptions or certify additional physical poles. *)
    own = DeleteDuplicates[Join[pairChannels[rules], Flatten[Function[prop,
      With[{propKey = residuePropagatorKey[prop, external]},
        If[MissingQ[propKey], {}, {propKey,
          {Complement[external, First[propKey]], Last[propKey]}}]]] /@
        DeleteDuplicates[Cases[expr, _PropDen, {0, Infinity}]], 1]]];
    physicalQ[a_, b_] := AnyTrue[inventory,
      First[#] === Sort[{a, b}] && compatibleMassQ[Last[#], Mass[b] /. massRules] &];
    ownQ[a_, b_] := AnyTrue[own,
      First[#] === Sort[{a, b}] && compatibleMassQ[Last[#], Mass[b] /. massRules] &];
    referenceKey[s_Spinor] := s[[3]];
    denominatorChains = withProtectedPropagators[expr, Function[body,
      DeleteDuplicates[Cases[body,
        HoldPattern[Power[chain_SpinorChain, power_?NumericQ]] /; TrueQ[power < 0] :> chain,
        {0, Infinity}]]]];
    bracketReferenceData[chain_] := Module[{parts = List @@ chain, ref, leg},
      If[Length[parts] =!= 2 || Count[parts, Spinor["Xi", ___]] =!= 1, Return[{}]];
      ref = SelectFirst[parts, MatchQ[#, Spinor["Xi", ___]] &];
      leg = SelectFirst[parts, !MatchQ[#, Spinor["Xi", ___]] &];
      If[!MatchQ[leg, _Spinor] || spinorType[leg] =!= ref[[2]] ||
          !MemberQ[massless, spinorLeg[leg]], Return[{}]];
      {referenceKey[ref], spinorLeg[leg]}];
    Do[legs = bracketReferenceData[chain];
      If[legs =!= {}, associations[First[legs]] = Union[
        Lookup[associations, First[legs], {}], {Last[legs]}]], {chain, denominatorChains}];
    If[AnyTrue[references, !KeyExistsQ[associations, referenceKey[#]] &],
      fail["An Xi could not be associated with a massless external leg through a reciprocal reference bracket."]];
    cycle = DeleteDuplicates[Join[Flatten[Function[ref,
      legs = associations[referenceKey[ref]]; If[Length[legs] === 1, legs, {}]] /@
      SortBy[references, If[IntegerQ[referenceKey[#]], {0, referenceKey[#]},
        {1, ToString[referenceKey[#], InputForm]}] &]], massless]];
    If[TrueQ[shared], cycle = massless];
    opposite["Angle"] := "Square"; opposite["Square"] := "Angle";
    choose[ref_, a_] := Module[{cacheKey = With[{r = ref, l = a}, HoldComplete[r, l]],
       at, options, spurious, physical, eligible, selected},
      If[KeyExistsQ[cache, cacheKey], Return[cache[cacheKey]]];
      If[choice === "IndependentXi",
        eligible = {<|"Leg" -> a, "Mode" -> "IndependentXi", "Target" -> a|>},
        at = First[FirstPosition[cycle, a]];
        options = Table[cycle[[1 + Mod[at - 1 + If[choice === "ReverseCyclic", -1, 1] step,
          Length[cycle]]]], {step, Length[cycle] - 1}];
        options = Select[options, Function[b, MemberQ[massless, b] &&
          (choice === "FeynmanCyclic" ||
            !AnyTrue[inventory, Function[item, First[item] === Sort[{a, b}] &&
              compatibleMassQ[Last[item], 0]]])]];
        spurious = (<|"Leg" -> a, "Mode" -> If[choice === "FeynmanCyclic", "Cyclic", "AllSpurious"],
          "Target" -> #|> &) /@ options;
        physical = {};
        If[MemberQ[{Automatic, "AllPhysical", "Mixed"}, choice],
          (* Feynman references may repeat a diagram's pole: no internal shell
             identity is used, and the assignment must be shared across diagrams. *)
          options = Select[DeleteCases[external, a],
            physicalQ[a, #] && (TrueQ[shared] || !ownQ[a, #]) &];
          options = Join[Select[options, !MemberQ[massless, #] &], Select[options, MemberQ[massless, #] &]];
          physical = (<|"Leg" -> a, "Mode" -> "AllPhysical", "Target" -> #|> &) /@ options];
        eligible = Switch[choice,
          "AllPhysical", physical, "Mixed", Join[spurious, physical],
          Automatic, If[spurious === {}, physical, spurious], _, spurious];
        If[eligible === {}, fail["No " <> ToString[choice, InputForm] <>
          " reference is supported for external leg " <> ToString[a] <>
          " by the supplied diagram pole inventory."]]];
      AppendTo[choiceGroups, eligible];
      selected = If[forced === {}, First[eligible],
        SelectFirst[forced, Lookup[#, "Leg", None] === a &, Missing["ReferenceAssignment"]]];
      If[!MemberQ[eligible, selected], fail["A requested reference assignment is not admissible."]];
      cache[cacheKey] = Join[<|"Reference" -> ref|>, selected];
      AppendTo[assignments, cache[cacheKey]]; cache[cacheKey]];
    infer[ref_, other_] := Module[{allowed = associations[referenceKey[ref]], otherLeg},
      If[Length[allowed] === 1, Return[First[allowed]]];
      otherLeg = If[MatchQ[other, _Spinor] && !MatchQ[other, Spinor["Xi", ___]], spinorLeg[other], None];
      If[MemberQ[allowed, otherLeg], otherLeg,
        fail["A shared Xi in a momentum chain cannot be assigned unambiguously to one external leg."]]];
    rewrite[chain_SpinorChain] := Module[{parts = List @@ chain, left, right, setting, a, b,
       first, last, middle, sign = 1},
      If[FreeQ[chain, Spinor["Xi", ___]], Return[chain]];
      If[Length[parts] < 2 || !FreeQ[Drop[Rest[parts], -1], Spinor["Xi", ___]],
        fail["Xi spinors are only supported at spinor-chain endpoints."]];
      left = First[parts]; right = Last[parts]; middle = Drop[Rest[parts], -1];
      first = {left}; last = {right};
      Do[If[MatchQ[ref, Spinor["Xi", ___]],
        a = infer[ref, If[side === 1, right, left]]; setting = choose[ref, a]; b = setting["Target"];
        If[setting["Mode"] === "AllPhysical",
          If[side === 1, first = {Spinor["Helicity", opposite[ref[[2]]], a], Mom[b]},
            last = {Mom[b], Spinor["Helicity", opposite[ref[[2]]], a]}; sign = -sign];
          AppendTo[physicalDenominators,
            SpinorChain[Spinor["Helicity", opposite[ref[[2]]], a], Mom[b],
              Spinor["Helicity", ref[[2]], a]] ->
                PropDen[Mom[Multiparticle @@ Sort[{a, b}]], Mass[b] /. massRules]],
          setting = If[setting["Mode"] === "IndependentXi", ReplacePart[ref, 3 -> a],
            Spinor["Helicity", ref[[2]], b]];
          If[side === 1, first = {setting}, last = {setting}]]],
        {side, {1, 2}}, {ref, {If[side === 1, left, right]}}];
      sign (SpinorChain @@ Join[first, middle, last])];
    transformed = withProtectedPropagators[expr, Function[body, body /. chain_SpinorChain :> rewrite[chain]]];
    physicalDenominators = DeleteDuplicates[Join[physicalDenominators,
      (Reverse[First[#]] -> Last[#]) & /@ physicalDenominators]];
    transformed = withProtectedPropagators[transformed, Function[body,
      body /. HoldPattern[Power[chain_SpinorChain, power_?NumericQ] /; TrueQ[power < 0]] :>
        Power[chain /. physicalDenominators, power]]];
    If[choice =!= "IndependentXi" && !withProtectedPropagators[transformed,
        Function[body, FreeQ[body, Spinor["Xi", ___]]]],
      fail["A reference spinor could not be replaced in a supported chain."]];
    <|"Status" -> "COMPLETE", "Expression" -> transformed,
      "Assignments" -> assignments, "Choices" -> DeleteDuplicatesBy[choiceGroups, First[#]["Leg"] &],
      "Diagnostics" -> {}|>, tag]
];

(* Visit the baseline and single-leg changes before deeper combinations. Never
   materialize the exponentially large Cartesian product. *)
preparationReferenceSelections[choices_List, limit_Integer] := Module[
  {sizes = Length /@ choices, strides, total, indices = {}, seen = <||>, add, j, k, n},
  total = Times @@ sizes;
  strides = Table[Times @@ Drop[sizes, j], {j, Length[sizes]}];
  add[n_Integer] := If[Length[indices] < limit && !KeyExistsQ[seen, n],
    AppendTo[indices, n]; AssociateTo[seen, n -> True]];
  add[0];
  For[j = 1, j <= Length[sizes] && Length[indices] < limit, j++,
    For[k = 1, k < sizes[[j]] && Length[indices] < limit, k++, add[k strides[[j]]]]];
  For[n = 0, n < total && Length[indices] < Min[limit, total], n++, add[n]];
  <|"AvailableChoices" -> total, "Selections" -> Map[Function[index,
    Table[choices[[j, 1 + Mod[Quotient[index, strides[[j]]], sizes[[j]]]]],
      {j, Length[sizes]}]], indices]|>
];

preparationReferenceTrial[expr_, rules_List, dotRules_List] := preparationReduceDots[
  preparationSpinorProducts[preparationReduceDots[expr, dotRules], rules], dotRules];

preparationReferenceSearch[expr_, rules_List, model_List, pairs_List, requested_,
    limit_Integer, seconds_, dotRules_List, progress_, shared_ : False] := Module[
  {baseline, plan, selections, available, best, alternatives = {}, cache = <||>,
   started = AbsoluteTime[], tried = 0, completed = 0, value, row,
   remaining, trialSeconds, outcome, timedOut = False, stop, search, diagnostics = {},
   cacheKey, rank, reduced},
  baseline = preparationReferences[expr, rules, model, pairs, requested, {}, shared];
  If[baseline["Status"] =!= "COMPLETE" ||
      TrueQ[shared] ||
      !MemberQ[{Automatic, "AllSpurious", "AllPhysical", "Mixed"}, requested] ||
      Lookup[baseline, "Choices", {}] === {}, Return[baseline]];
  plan = preparationReferenceSelections[baseline["Choices"], limit];
  selections = plan["Selections"]; available = plan["AvailableChoices"]; best = baseline;
  outcome = TimeConstrained[
    Do[
      remaining = If[seconds === Infinity, Infinity, seconds - (AbsoluteTime[] - started)];
      If[remaining <= 0, Break[]];
      tried++;
      trialSeconds = If[seconds === Infinity, Infinity,
        Min[remaining, Max[0.01, seconds/Min[Length[selections], 8]]]];
      value = TimeConstrained[Module[{report},
        report = If[tried === 1, baseline,
          preparationReferences[expr, rules, model, pairs, requested, selection, shared]];
        If[report["Status"] =!= "COMPLETE", Return[$Failed]];
        cacheKey = With[{body = report["Expression"]}, HoldComplete[body]];
        reduced = If[KeyExistsQ[cache, cacheKey], cache[cacheKey],
          preparationReferenceTrial[report["Expression"], rules, dotRules]];
        If[!residueFiniteQ[reduced], Return[$Failed]];
        AssociateTo[cache, cacheKey -> reduced];
        rank = preparationRank[reduced];
        <|"Report" -> report, "Expression" -> reduced, "Rank" -> rank|>
      ], trialSeconds, $TimedOut];
      If[AssociationQ[value],
        completed++;
        row = <|"Assignments" -> value["Report"]["Assignments"],
          "Expression" -> value["Expression"], "Rank" -> value["Rank"], "ChoiceIndex" -> tried|>;
        AppendTo[alternatives, row];
        If[!KeyExistsQ[best, "ReducedRank"] || preparationBetterQ[value["Rank"], best["ReducedRank"]],
          best = Join[value["Report"], <|"ReducedExpression" -> value["Expression"],
            "ReducedRank" -> value["Rank"]|>]],
        If[value === $TimedOut, timedOut = True]];
      progress[Join[<|"Completed" -> tried, "Total" -> Length[selections],
          "AvailableReferenceChoices" -> available|>,
        If[KeyExistsQ[best, "ReducedRank"],
          <|"Complexity" -> First[best["ReducedRank"]], "Terms" -> best["ReducedRank"][[2]],
            "BestReference" -> Join[best, <|
              "Search" -> <|"AvailableChoices" -> available, "TriedChoices" -> tried,
                "CompletedChoices" -> completed, "RejectedChoices" -> tried - completed,
                "Exhaustive" -> False, "StopReason" -> "TimeConstraint"|>,
              "Alternatives" -> SortBy[alternatives, {#["Rank"], #["ChoiceIndex"]} &]|>]|>, <||>]]],
      {selection, selections}]; True,
    seconds, False];
  stop = Which[!TrueQ[outcome] || tried < Length[selections], "TimeConstraint",
    timedOut, "TrialTimeConstraint", available > tried, "MaxReferenceChoices", True, "Exhausted"];
  search = <|"AvailableChoices" -> available, "TriedChoices" -> tried,
    "CompletedChoices" -> completed, "RejectedChoices" -> tried - completed,
    "Exhaustive" -> (completed === available), "StopReason" -> stop|>;
  If[completed === 0, AppendTo[diagnostics,
    "No reference trial completed within the selection budget; the deterministic reference assignment is retained."]];
  If[!TrueQ[search["Exhaustive"]], AppendTo[diagnostics,
    "Reference selection was bounded; only completed trials were ranked, not every admissible assignment."]];
  Join[best, <|"Search" -> search,
    "Alternatives" -> SortBy[alternatives, {#["Rank"], #["ChoiceIndex"]} &],
    "Diagnostics" -> Join[Lookup[best, "Diagnostics", {}], diagnostics]|>]
];

preparationOneAmplitude[pair_, index_, count_, options_Association, model_, pairs_, kind_] := Module[
  {rules = preparationCanonicalMassRules[First[pair]], original = Last[pair], expression = Last[pair],
   reductions = {}, history = {}, diagnostics = {}, channels, shellOptions, dotRules,
   started = AbsoluteTime[], outcome, stage = "Starting", stop = "MaxSteps",
   step, candidates, search, candidate, next, rank, best, bestRank, bestEdit,
   generated = 0, inspected = 0, event, reduceInvariants, before, after, identityStageComplete = False,
   invalid = False, conversionFailure = False, referenceFailure = False, referenceReport = <||>,
   referenceSeconds, referenceFallback = None, referenceInput,
   failureTag = Unique["preparationFailure"], progressMetadata},
  progressMetadata = Join[<|"Item" -> index, "Items" -> count, "InputType" -> kind|>,
    If[MemberQ[{"Diagram", "FeynmanDiagram"}, kind], <|"Diagram" -> index, "Diagrams" -> count|>, <||>]];
  channels = If[TrueQ[options["InternalOnShell"]],
    First /@ knownInternalChannels[rules], {}];
  dotRules = If[TrueQ[options["InternalOnShell"]],
    preparationDotRules[rules, options["MomentumConservation"]], {}];
  shellOptions = {"OnShellChannels" -> channels,
    "MomentumConservation" -> options["MomentumConservation"]};
  event[label_] := completionProgress[options["ProgressFunction"], options["ProgressReporting"],
    Join[progressMetadata, <|"Stage" -> "Preparation", "Operation" -> label,
    "Elapsed" -> AbsoluteTime[] - started,
    "Complexity" -> First[preparationRank[expression]],
    "Terms" -> preparationRank[expression][[2]], "IdentitySteps" -> Length[history]|>]];
  reduceInvariants[phase_] := If[TrueQ[options["InternalOnShell"]],
    stage = "InternalInvariantReduction";
    before = expression;
    after = preparationReduceDots[expression, dotRules];
    If[!residueFiniteQ[after], invalid = True; Throw[False, failureTag]];
    expression = after;
    AppendTo[reductions, <|"Operation" -> stage, "Phase" -> phase, "Before" -> before,
      "After" -> expression, "InternalOnShellIdentity" -> (expression =!= before),
      "Replacements" -> dotRules|>];
    event[stage]];
  event["Starting"];
  outcome = Catch[TimeConstrained[
    If[kind === "FeynmanDiagram" && feynmanRawExpressionQ[expression],
      stage = "FeynmanToSpinorProducts";
      before = expression;
      after = ConstructiveDiagrams`FeynmanToSpinorProducts[expression,
        "FermionHelicities" -> options["FermionHelicities"],
        "ReferenceSpinors" -> Automatic, "ReduceSpinorProducts" -> False,
        "TimeConstraint" -> Infinity];
      If[!residueFiniteQ[after], conversionFailure = True;
        AppendTo[diagnostics, If[FailureQ[after], Lookup[after[[2]], "MessageTemplate",
          "Feynman-to-spinor conversion failed."], "Feynman-to-spinor conversion failed."]];
        Throw[False, failureTag]];
      expression = after;
      AppendTo[reductions, <|"Operation" -> stage, "Before" -> before,
        "After" -> expression, "InternalOnShellIdentity" -> False|>];
      event[stage]];
    stage = "ReduceSpinContractions";
    before = expression;
    after = preparationNumerators[expression,
      Function[num, ReduceSpinContractions[num] /. rules]];
    If[!residueFiniteQ[after], invalid = True; Throw[False, failureTag]];
    expression = after;
    AppendTo[reductions, <|"Operation" -> stage, "Before" -> before,
      "After" -> expression, "InternalOnShellIdentity" -> False|>];
    event[stage];
    stage = "ReferenceSpinors";
    before = expression;
    referenceInput = expression;
    referenceSeconds = Replace[options["ReferenceTimeConstraint"], Automatic :>
      If[options["TimeConstraint"] === Infinity, 5,
        Max[0.000001, Min[5, (options["TimeConstraint"] - (AbsoluteTime[] - started))/3]]]];
    referenceReport = preparationReferenceSearch[expression, rules, model, pairs,
      options["ReferenceSpinors"], options["MaxReferenceChoices"], referenceSeconds, dotRules,
      Function[update,
        If[KeyExistsQ[update, "BestReference"], referenceFallback = update["BestReference"]];
        completionProgress[options["ProgressFunction"], options["ProgressReporting"],
          Join[progressMetadata, <|"Stage" -> "Preparation", "Operation" -> "Choosing references",
            "Elapsed" -> AbsoluteTime[] - started,
            "Complexity" -> First[preparationRank[expression]],
            "Terms" -> preparationRank[expression][[2]]|>, KeyDrop[update, "BestReference"]]]],
      kind === "FeynmanDiagram"];
    If[!AssociationQ[referenceReport] || referenceReport["Status"] =!= "COMPLETE",
      referenceFailure = True;
      diagnostics = Join[diagnostics, Lookup[referenceReport, "Diagnostics", {
        "The requested reference-spinor choice could not be established."}]];
      Throw[False, failureTag]];
    diagnostics = Join[diagnostics, Lookup[referenceReport, "Diagnostics", {}]];
    If[KeyExistsQ[referenceReport, "ReducedExpression"], referenceFallback = referenceReport];
    after = referenceReport["Expression"];
    If[!residueFiniteQ[after], invalid = True; Throw[False, failureTag]];
    expression = after;
    AppendTo[reductions, <|"Operation" -> stage, "Before" -> before,
      "After" -> expression, "InternalOnShellIdentity" -> False,
      "ReferenceChoice" -> options["ReferenceSpinors"]|>];
    event[stage];
    reduceInvariants["BeforeSpinorProducts"];
    stage = "ReduceSpinorProducts";
    before = expression;
    after = preparationSpinorProducts[expression, rules];
    If[!residueFiniteQ[after], invalid = True; Throw[False, failureTag]];
    expression = after;
    AppendTo[reductions, <|"Operation" -> stage, "Before" -> before,
      "After" -> expression, "InternalOnShellIdentity" -> False|>];
    event[stage];
    reduceInvariants["AfterSpinorProducts"];
    stage = "IdentitySearch";
    Do[
      after = preparationSpinorProducts[preparationNumerators[expression,
        Function[num, canonicalAmplitude[num, rules]]], rules];
      If[!residueFiniteQ[after], invalid = True; Throw[False, failureTag]];
      expression = after;
      rank = preparationRank[expression]; best = expression; bestRank = rank; bestEdit = None;
      search = preparationSearchCandidates[expression, rules, shellOptions,
        options["MomentumConservation"], options["MaxCandidates"]];
      candidates = search["Candidates"];
      generated += search["Generated"];
      If[candidates === {}, stop = "NoLegalIdentity"; Break[]];
      (* Edits are generated for this exact state, so re-enumerating them for each
         trial is unnecessary. Only a strictly improving trial is accepted. *)
      Do[
        inspected++;
        next = If[TrueQ[Lookup[candidate, "PreparationConservation", False]],
          preparationConservationTrial[expression, candidate, rules],
          preparationNumerators[expression - candidate["Subtract"] + candidate["Insert"], canonicalChains]];
        next = preparationSpinorProducts[next, rules];
        next = preparationReduceDots[next, dotRules];
        If[!residueFiniteQ[next], Continue[]];
        If[preparationBetterQ[preparationRank[next], bestRank],
          best = next; bestRank = preparationRank[next]; bestEdit = candidate],
        {candidate, candidates}];
      If[bestEdit === None, stop = "NoImprovingIdentity"; Break[]];
      AppendTo[history, <|"Name" -> bestEdit["Name"], "Edit" -> bestEdit,
        "Before" -> expression, "After" -> best,
        "OnShellChannels" -> channels, "OnShellDotReplacements" -> dotRules,
        "BeforeRank" -> rank, "AfterRank" -> bestRank|>];
      expression = best;
      event["Improving identities"],
      {step, options["MaxSteps"]}];
    identityStageComplete = True;
    True,
    options["TimeConstraint"], False], failureTag];
  If[!TrueQ[outcome] && MemberQ[{"ReferenceSpinors", "InternalInvariantReduction",
      "ReduceSpinorProducts"}, stage] && AssociationQ[referenceFallback],
    referenceReport = referenceFallback;
    expression = referenceFallback["ReducedExpression"];
    AppendTo[reductions, <|"Operation" -> "RetainedReferenceTrial", "Before" -> referenceInput,
      "After" -> expression, "InternalOnShellIdentity" -> TrueQ[options["InternalOnShell"]],
      "ReferenceChoice" -> options["ReferenceSpinors"]|>]];
  If[!TrueQ[outcome] && stage === "FeynmanToSpinorProducts", conversionFailure = True];
  If[!TrueQ[outcome], stop = Which[conversionFailure, "FeynmanConversionFailed", invalid, "InvalidReduction",
      referenceFailure, "ReferenceChoiceUnavailable", True, "TimeConstraint"];
    AppendTo[diagnostics, Which[conversionFailure,
      "Feynman-to-spinor conversion did not complete", invalid,
      "An invalid result was rejected during " <> stage, referenceFailure,
      "The requested reference-spinor selection was not applied", True,
      "The per-element time limit was reached during " <> stage] <>
        "; the last completed expression is retained."]];
  If[TrueQ[outcome] && stop === "MaxSteps" && options["MaxSteps"] > 0,
    AppendTo[diagnostics, "The bounded identity search reached MaxSteps; this is not a claim of minimality."]];
  If[TrueQ[outcome] && stop === "NoImprovingIdentity",
    AppendTo[diagnostics, "No strictly improving edit was found in the inspected candidate subset; this is not a proof of minimality."]];
  event[Which[TrueQ[outcome], "Prepared", referenceFailure, "Reference choice unavailable",
    conversionFailure, "Feynman conversion unavailable",
    invalid, "Invalid reduction retained previous state",
    True, "Time limit reached"]];
  Join[<|"ItemIndex" -> index, "InputType" -> kind|>,
    If[MemberQ[{"Diagram", "FeynmanDiagram"}, kind], <|"DiagramIndex" -> index|>, <||>],
  <|"Status" -> If[TrueQ[outcome], "PREPARED", "PARTIAL"],
    "MassRules" -> First[pair], "ExternalMassRules" -> preparationExternalRules[rules],
    "OriginalExpression" -> original, "PreparedExpression" -> expression,
    "InternalOnShellChannels" -> channels,
    "InternalOnShellConditions" -> ((PropDen[Mom[Multiparticle @@ First[#]], Last[#]] == 0) & /@
      Select[knownInternalChannels[rules], MemberQ[channels, First[#]] &]),
    "DenominatorsKeptOffShell" -> True,
    "FeynmanConversionFailed" -> conversionFailure,
    "ReferenceSpinors" -> referenceReport,
    "Reductions" -> reductions, "IdentitySteps" -> history,
    "GeneratedCandidates" -> generated, "InspectedCandidates" -> inspected,
    "IdentitySearchCompleted" -> identityStageComplete, "StopReason" -> stop,
    "ElapsedTime" -> AbsoluteTime[] - started, "Diagnostics" -> diagnostics|>]
];

PrepareAmplitudes[items_List, model_, opts : OptionsPattern[]] := Module[
  {options, reports, prepared, external, diagnostics, details, types, pairs, inputType,
   feynman = TrueQ[feynmanModelQ[model]], engineModel, referencePairs,
   output = OptionValue["Output"]},
  types = preparationInputType /@ items;
  If[MemberQ[types, $Failed], Message[PrepareAmplitudes::args]; Return[$Failed]];
  pairs = MapThread[preparationInputPair, {items, types}];
  inputType = Which[types === {} || AllTrue[types, # === "Diagram" &], "Diagrams",
    AllTrue[types, # === "FactorizationChannel" &], "FactorizationChannels", True, "Mixed"];
  options = Association[Map[Function[name, name -> OptionValue[name]],
    First /@ Options[PrepareAmplitudes]]];
  engineModel = If[feynman, List @@ model, model];
  If[!AllTrue[pairs, preparationPairQ] ||
      !AllTrue[pairs, preparationMassRulesConsistentQ[First[#]] &] || !preparationModelQ[engineModel] ||
      (feynman && !AllTrue[types, # === "Diagram" &]) ||
      !(AssociationQ[options["FermionHelicities"]] ||
        MatchQ[options["FermionHelicities"], {(_Rule | _RuleDelayed) ...}]) ||
      !MemberQ[{Automatic, None, "AllSpurious", "AllPhysical", "Mixed", "Xi", "Cyclic",
        "ReverseCyclic", "IndependentXi"}, options["ReferenceSpinors"]] ||
      !IntegerQ[options["MaxReferenceChoices"]] || options["MaxReferenceChoices"] < 1 ||
      !(options["ReferenceTimeConstraint"] === Automatic || options["ReferenceTimeConstraint"] === Infinity ||
        TrueQ[NumericQ[options["ReferenceTimeConstraint"]] && options["ReferenceTimeConstraint"] > 0]) ||
      !MemberQ[{True, False}, options["InternalOnShell"]] ||
      !MemberQ[{True, False}, options["MomentumConservation"]] ||
      !MemberQ[{True, False}, options["ProgressReporting"]] ||
      !MemberQ[{True, False}, options["ReturnDetails"]] ||
      !IntegerQ[options["MaxSteps"]] || options["MaxSteps"] < 0 ||
      !IntegerQ[options["MaxCandidates"]] || options["MaxCandidates"] < 1 ||
      !(options["TimeConstraint"] === Infinity ||
        TrueQ[NumericQ[options["TimeConstraint"]] && options["TimeConstraint"] > 0]) ||
      !MemberQ[{"Amplitudes", "Expression", "Details"}, output] ||
      !(MemberQ[{Automatic, None}, options["ProgressFunction"]] ||
        MatchQ[options["ProgressFunction"], _Function | _Symbol]) ||
      Complement[First /@ {opts}, First /@ Options[PrepareAmplitudes]] =!= {},
    Message[PrepareAmplitudes::args]; Return[$Failed]];
  If[feynman,
    pairs = {feynmanExternalMassRules[First[#]], Last[#]} & /@ pairs;
    types = ConstantArray["FeynmanDiagram", Length[pairs]];
    inputType = "FeynmanDiagrams";
    If[!MemberQ[First /@ {opts}, "MaxSteps"], options["MaxSteps"] = 0];
    options["InternalOnShell"] = False];
  referencePairs = If[feynman, preparationFeynmanReferencePairs[pairs], pairs];
  external = preparationExternalRules /@ (preparationCanonicalMassRules /@ First /@ pairs);
  If[external =!= {} && !AllTrue[Rest[external], preparationExternalRulesAgreeQ[First[external], #] &],
    Message[PrepareAmplitudes::args]; Return[$Failed]];
  reports = MapIndexed[Function[{pair, position}, With[{i = First[position]},
    Join[preparationOneAmplitude[pair, i, Length[pairs], options, engineModel, referencePairs, types[[i]]],
      If[types[[i]] === "FactorizationChannel",
        <|"Channel" -> items[[i, -3]], "Cut" -> List @@ items[[i, -3]], "Mass" -> items[[i, -2]]|>, <||>]]]], pairs];
  prepared = MapThread[preparationOutputItem[#1, #2, #3["PreparedExpression"]] &, {items, types, reports}];
  If[!TrueQ[options["ReturnDetails"]],
    If[output =!= "Details" && AnyTrue[reports, TrueQ[#["FeynmanConversionFailed"]] &],
      Return[Failure["FeynmanPreparation", <|"MessageTemplate" ->
        "Feynman-to-spinor conversion did not complete. Supply massless-fermion helicities if needed, or request ReturnDetails -> True for diagnostics."|>]]];
    If[output =!= "Details" &&
        AnyTrue[reports, #["StopReason"] === "ReferenceChoiceUnavailable" &],
      Return[Failure[If[feynman, "FeynmanReferences", "AmplitudeReferences"], <|"MessageTemplate" ->
        "The requested reference-spinor choice could not be applied. Use Automatic to allow a physical-reference fallback, or request ReturnDetails -> True for diagnostics.",
        "ReferenceSpinors" -> options["ReferenceSpinors"]|>]]];
    Switch[output, "Amplitudes", Return[prepared],
      "Expression", Return[Total[Last /@ prepared]]]];
  diagnostics = DeleteDuplicates[Flatten[Lookup[reports, "Diagnostics", {}], 1]];
  details = <|"Status" -> If[AnyTrue[reports, #["Status"] === "PARTIAL" &], "PARTIAL", "PREPARED"],
    "InputType" -> inputType, "ElementTypes" -> types,
    "OriginalAmplitudes" -> items, "PreparedAmplitudes" -> prepared,
    "Amplitude" -> Total[Last /@ prepared],
    "ExternalMassRules" -> If[external === {}, {}, First[external]],
    "ItemReports" -> reports,
    "DiagramReports" -> Select[reports, MemberQ[{"Diagram", "FeynmanDiagram"}, #["InputType"]] &],
    "ChannelReports" -> Select[reports, #["InputType"] === "FactorizationChannel" &],
    "Model" -> model, "Options" -> options,
    "Diagnostics" -> diagnostics|>;
  details
];
PrepareAmplitudes[___] := (Message[PrepareAmplitudes::args]; $Failed);
PrepareDiagramAmplitudes[args___] := PrepareAmplitudes[args];

End[];
End[];

(* ::Subsection::Closed:: *)
(*High-Energy Completion*)

Begin["ConstructiveDiagrams`"];

CompleteHighEnergy::usage = "CompleteHighEnergy[input, channels, model] fits a bounded set of residue-preserving correction directions to the high-energy, spurious-pole and explicit-reference conditions, then validates the complete candidate on independent samples. It uses remaining directions from CompleteFactorization details or an explicit CorrectionBasis. Automatic never invents an unrestricted contact-operator basis. ParameterRules -> Automatic uses the supplied model's variables and dependent functions; an explicit rule list overrides selected parameters without changing the symbolic amplitude. Output -> Details retains the candidate, affine family, fit equations and validation report. An incomplete result returns Failure unless Details is requested.";

Options[CompleteHighEnergy] = {"CorrectionBasis" -> Automatic,
  "ParameterRules" -> Automatic, "Samples" -> 4, "ValidationSamples" -> 4,
  "RandomSeed" -> 1729, "TimeConstraint" -> 60,
  "HighEnergyBound" -> Automatic, "MaxCorrectionTerms" -> 64,
  "ValidationParameterRules" -> Automatic,
  "ProgressReporting" -> True, "ProgressFunction" -> Automatic,
  "Output" -> "Expression"};

CompleteHighEnergy[input_, channels_List, model_List, opts : OptionsPattern[]] :=
  ConstructiveDiagrams`Private`completionHighEnergyStage[input, channels, model,
    Join[Association[Options[CompleteHighEnergy]], Association[{opts}]]];
CompleteHighEnergy[___] := Failure["InvalidCompletionArguments", <|
  "MessageTemplate" -> "Expected an amplitude or completion report, a list of factorization channels, and a model."|>];

Begin["ConstructiveDiagrams`Private`"];

completionHEMetadata[normalized_Association] := Module[{metadata, rules, legs},
  metadata = poleDiscoveryMetadata[Lookup[normalized, "Targets", {}],
    Lookup[normalized, "Records", {}]];
  If[AssociationQ[metadata], Return[metadata]];
  rules = Lookup[normalized, "ExternalMassRules", {}];
  legs = Sort[Cases[rules, HoldPattern[Mass[leg_Integer] -> _] :> leg]];
  If[Length[legs] < 4, Missing["ExternalMassMetadata"],
    <|"ExternalLegs" -> legs, "ExternalMassRules" -> rules|>]
];

completionHECoherentChart[chart_Association] := Module[
  {angle = chart["Angle"], square = chart["Square"], vectors = <||>, variables = {}, vector},
  Do[
    vector = {1, Unique["completionSpin"]};
    AppendTo[variables, Last[vector]];
    AssociateTo[vectors, leg -> vector];
    AssociateTo[angle, leg -> (chart["AngleBasis"][leg].vector)];
    AssociateTo[square, leg -> (chart["SquareBasis"][leg].vector)],
    {leg, Keys[Lookup[chart, "AngleBasis", <||>]]}];
  Join[chart, <|"Angle" -> angle, "Square" -> square,
    "ExternalSpinVectors" -> vectors, "SpinVariables" -> variables|>]
];

completionHEPolynomialRows[expression_, variables_List] := Module[{numerator, denominator},
  numerator = Expand[Numerator[Cancel[Together[expression]]]];
  denominator = Denominator[Cancel[Together[expression]]];
  If[!residueFiniteQ[numerator] || !residueFiniteQ[denominator] ||
      (variables =!= {} &&
        (!PolynomialQ[numerator, variables] || !PolynomialQ[denominator, variables])),
    Return[Failure["NonPolynomialConstraint", <||>]]];
  If[variables === {}, {numerator}, Last /@ CoefficientRules[numerator, variables]]
];

(* Keep coefficients affine in the unknown corrections. The Laurent variable
   and all coherent spin variables are eliminated, never fitted numerically. *)
completionHELaurentRows[mapped_, variable_, spinVariables_List, unknowns_List,
    threshold_] := Catch[Module[{order, series, polynomial, lowest, rows = {}, coefficients},
  If[!residueFiniteQ[mapped] || residueUnmappedQ[mapped] ||
      (unknowns =!= {} && !PolynomialQ[mapped, unknowns]),
    Return[Failure["UnsupportedLinearKinematics", <||>]]];
  order = Max[0, Ceiling[threshold]];
  series = Quiet[Check[Series[mapped, {variable, 0, order}], $Failed]];
  If[!residueFiniteQ[series] || !FreeQ[series, _Series | _ConditionalExpression],
    Return[Failure["UnsupportedLaurentExpansion", <||>]]];
  polynomial = Expand[Normal[series]];
  lowest = If[polynomial === 0, order + 1, Exponent[polynomial, variable, Min]];
  If[!IntegerQ[lowest] || !PolynomialQ[Expand[polynomial variable^Max[0, -lowest]], variable],
    Return[Failure["NonintegerLaurentExpansion", <||>]]];
  Do[
    coefficients = completionHEPolynomialRows[Coefficient[polynomial, variable, power],
      spinVariables];
    If[FailureQ[coefficients], Throw[coefficients]];
    rows = Join[rows, coefficients], {power, lowest, Ceiling[threshold] - 1}];
  Select[rows, highEnergyZeroState[#] =!= "Zero" &]
]];

completionHEEnergyConstraints[trial_, unknowns_List, metadata_Association,
    samples_Integer, seed_Integer, rules_List, bound_, callback_, enabled_] := Catch[Module[
  {rows = {}, charts = {}, chart, mapped, coefficients},
  Do[
    completionProgress[callback, enabled, <|"Stage" -> "HighEnergy",
      "Phase" -> "EnergyConstraints", "Current" -> sample, "Total" -> samples|>];
    chart = highEnergyAutomaticChart[trial, metadata, sample, seed, rules];
    If[FailureQ[chart], Throw[<|"Status" -> "UNRESOLVED", "Equations" -> rows,
      "Samples" -> charts, "Diagnostics" -> {chart}|>]];
    mapped = residueEvaluateChart[trial, chart, rules];
    coefficients = completionHELaurentRows[mapped, chart["PoleVariable"],
      chart["SpinVariables"], unknowns, -2 bound];
    If[FailureQ[coefficients], Throw[<|"Status" -> "UNRESOLVED", "Equations" -> rows,
      "Samples" -> charts, "Diagnostics" -> {coefficients}|>]];
    rows = Join[rows, coefficients];
    AppendTo[charts, <|"Sample" -> sample, "Chart" -> chart,
      "EquationCount" -> Length[coefficients]|>], {sample, samples}];
  <|"Status" -> "COMPLETE", "Equations" -> rows, "Samples" -> charts,
    "Diagnostics" -> {}|>
]];

completionHESpuriousConstraints[trial_, unknowns_List, normalized_Association,
    metadata_Association, samples_Integer, seed_Integer, rules_List,
    callback_, enabled_] := Catch[Module[
  {targets = Lookup[normalized, "Targets", {}], discovery, rows = {}, charts = {},
   candidates, specification, chart, coefficients, mapped, factor, targetsForMetadata,
   effectiveSamples, minimumSamples, chartMetadata},
  targetsForMetadata = If[targets =!= {}, targets,
    {Join[metadata, <|"Cut" -> Take[metadata["ExternalLegs"], 2],
      "Complement" -> Drop[metadata["ExternalLegs"], 2], "Mass" -> 0,
      "KinematicMetadataOnly" -> True|>]}];
  chartMetadata = If[targets =!= {}, Join[metadata,
    KeyTake[First[targets], {"Cut", "Complement", "Mass", "Propagator"}]],
    First[targetsForMetadata]];
  discovery = poleDiscoverDenominators[trial, targetsForMetadata,
    Lookup[normalized, "Records", {}]];
  If[!ListQ[discovery] || AnyTrue[discovery, Lookup[#, "Classification"] === "Unclassified" &],
    Return[<|"Status" -> "UNRESOLVED", "Equations" -> {}, "Samples" -> {},
      "Diagnostics" -> {"A candidate denominator could not be classified for spurious-pole constraints."}|>]];
  candidates = Select[discovery, Lookup[#, "Classification"] === "Spurious" &];
  Do[
    factor = candidate["CanonicalFactor"];
    specification = spuriousPoleSpecification[factor /. rules,
      Join[metadata, <|"ExternalMassRules" -> (metadata["ExternalMassRules"] /. rules)|>]];
    If[MissingQ[specification], Throw[<|"Status" -> "UNRESOLVED", "Equations" -> rows,
      "Samples" -> charts, "Diagnostics" -> {"A spurious denominator has no supported transverse chart."}|>]];
    minimumSamples = If[Lookup[specification, "Mode", ""] === "Invariant",
      Switch[Count[Lookup[specification, "MasslessPairSides", {}], True],
        1, 2, 2, 4, _, 1], 1];
    effectiveSamples = Max[samples, minimumSamples];
    Do[
      completionProgress[callback, enabled, <|"Stage" -> "HighEnergy",
        "Phase" -> "SpuriousConstraints", "Pole" -> factor,
        "Current" -> sample, "Total" -> effectiveSamples|>];
      chart = spuriousAutomaticChart[specification, chartMetadata, sample, seed, rules];
      If[FailureQ[chart], Throw[<|"Status" -> "UNRESOLVED", "Equations" -> rows,
        "Samples" -> charts, "Diagnostics" -> {chart}|>]];
      chart = completionHECoherentChart[chart];
      If[!spuriousPhysicalGenericQ[trial, targets, chart, rules],
        Throw[<|"Status" -> "UNRESOLVED", "Equations" -> rows, "Samples" -> charts,
          "Diagnostics" -> {"A spurious constraint chart coincides with a physical pole."}|>]];
      mapped = residueEvaluateChart[trial, chart, rules];
      coefficients = completionHELaurentRows[mapped, chart["PoleVariable"],
        chart["SpinVariables"], unknowns, 0];
      If[FailureQ[coefficients], Throw[<|"Status" -> "UNRESOLVED", "Equations" -> rows,
        "Samples" -> charts, "Diagnostics" -> {coefficients}|>]];
      rows = Join[rows, coefficients];
      AppendTo[charts, <|"Sample" -> sample, "Pole" -> factor, "Chart" -> chart,
        "EquationCount" -> Length[coefficients]|>], {sample, effectiveSamples}],
    {candidate, candidates}];
  <|"Status" -> "COMPLETE", "Equations" -> rows, "Samples" -> charts,
    "Diagnostics" -> {}|>
]];

(* Generate linear equations directly. Classifying coefficients containing
   unsolved correction parameters would repeat expensive algebraic-number
   reductions only to conclude that those parameters are still unknown. *)
completionHEReferenceRows[trial_, chart_Association, references_List,
    sample_Integer, seed_Integer, rules_List] := BlockRandom[Catch[Module[
  {coordinates, referenceVariables, symbolicChart, mapped, spinVariables,
   spinComponents, assignments, baselineRules, baselinePoint, genericSpin,
   found = False, rows = {}, coefficients, differences = {}},
  coordinates = Association[(# -> Table[Unique["completionReference"], {4}]) & /@ references];
  referenceVariables = Flatten[Values[coordinates]];
  symbolicChart = Join[chart, <|"ReferenceOverrides" -> referenceOverrideVectors[coordinates]|>];
  mapped = residueEvaluateChart[Rationalize[trial, 0], symbolicChart, Rationalize[rules, 0]];
  spinVariables = Lookup[chart, "SpinVariables", {}];
  If[!residueFiniteQ[mapped] || residueUnmappedQ[mapped] ||
      (spinVariables =!= {} && !PolynomialQ[mapped, spinVariables]),
    Return[Failure["UnsupportedReferenceSpinPolynomial", <||>]]];
  spinComponents = If[spinVariables === {}, {{} -> mapped},
    CoefficientRules[mapped, spinVariables]];
  Do[
    SeedRandom[Hash[{seed, sample, attempt, "ReferenceBaseline"}, "CRC32"]];
    assignments = Association[(# -> RandomChoice[DeleteCases[Range[-7, 7], 0], 4]) & /@ references];
    baselineRules = Thread[referenceVariables -> Flatten[Values[assignments]]];
    genericSpin = Thread[spinVariables -> Range[Length[spinVariables]]];
    baselinePoint = Quiet[Check[Cancel[Together[mapped /. baselineRules /. genericSpin]], $Failed]];
    If[residueFiniteQ[baselinePoint] &&
        highEnergyZeroState[Denominator[baselinePoint]] === "Nonzero",
      found = True; Break[]], {attempt, 0, 5}];
  If[!found, Return[Failure["NoRegularReferenceBaseline", <||>]]];
  Do[
    coefficients = completionHEPolynomialRows[
      Last[component] - (Last[component] /. baselineRules), referenceVariables];
    If[FailureQ[coefficients], Throw[coefficients]];
    (* Exact algebraic zero tests and row reduction belong to the common
       linear solver, after the unknown coefficients have been separated. *)
    coefficients = DeleteCases[coefficients, 0];
    rows = Join[rows, coefficients];
    AppendTo[differences, <|"SpinMonomial" -> First[component],
      "Equations" -> coefficients|>], {component, spinComponents}];
  <|"Equations" -> rows, "Sample" -> sample, "Status" -> "CONSTRAINTS",
    "Chart" -> chart, "ReferenceCoordinates" -> coordinates,
    "BaselineAssignments" -> assignments, "SpinVariables" -> spinVariables,
    "SpinComponents" -> differences, "EquationCount" -> Length[rows],
    "Method" -> "ExactReferenceDifferenceBySpinComponent"|>
]]];

completionHEReferenceConstraints[trial_, unknowns_List, normalized_Association,
    metadata_Association, samples_Integer, seed_Integer, rules_List,
    callback_, enabled_] := Catch[Module[
  {references, rows = {}, charts = {}, chart, t, validation, sampleData},
  If[!FreeQ[trial, _xFactor | _xTildeFactor], Return[<|"Status" -> "UNRESOLVED",
    "Equations" -> {}, "Samples" -> {}, "Diagnostics" -> {"Unexpanded x factors are unsupported."}|>]];
  references = referenceExpressionMetadata[trial]["References"];
  If[references === {}, Return[<|"Status" -> "COMPLETE", "Equations" -> {},
    "Samples" -> {}, "Diagnostics" -> {}|>]];
  Do[
    completionProgress[callback, enabled, <|"Stage" -> "HighEnergy",
      "Phase" -> "ReferenceConstraints", "Current" -> sample, "Total" -> samples|>];
    chart = highEnergyAutomaticChart[trial, metadata, sample, seed, rules];
    If[FailureQ[chart], Throw[<|"Status" -> "UNRESOLVED", "Equations" -> rows,
      "Samples" -> charts, "Diagnostics" -> {chart}|>]];
    t = chart["PoleVariable"]; validation = chart["KinematicChecks"]["ValidationParameter"];
    chart = Join[chart /. t -> validation, <|"PoleVariable" -> Unique["completionReferencePole"]|>];
    If[!referencePhysicalGenericQ[chart, Lookup[normalized, "Targets", {}], rules],
      Throw[<|"Status" -> "UNRESOLVED", "Equations" -> rows, "Samples" -> charts,
        "Diagnostics" -> {"A reference constraint sample lies on a physical pole."}|>]];
    sampleData = completionHEReferenceRows[trial, chart, references, sample, seed, rules];
    If[FailureQ[sampleData], Throw[<|"Status" -> "UNRESOLVED",
      "Equations" -> rows, "Samples" -> charts,
      "Diagnostics" -> {sampleData}|>]];
    rows = Join[rows, sampleData["Equations"]];
    AppendTo[charts, sampleData], {sample, samples}];
  <|"Status" -> "COMPLETE", "Equations" -> rows, "Samples" -> charts,
    "Diagnostics" -> {}|>
]];

completionHEAllPassQ[report_] := AssociationQ[report] &&
  AllTrue[Lookup[report, "Checks", {}], Lookup[#, "Status"] === "PASS" &] &&
  KeyExistsQ[report, "HighEnergyCheck"] && KeyExistsQ[report, "ReferenceCheck"];

completionHECheck[expression_, normalized_Association, channels_List, model_List,
    settings_Association, seed_Integer] := CheckAmplitudes[
  {normalized["ExternalMassRules"], expression}, channels, model,
  "Output" -> "Details", "ParameterRules" -> settings["ParameterRules"],
  "Samples" -> settings["ValidationSamples"], "RandomSeed" -> seed,
  "HighEnergySamples" -> settings["ValidationSamples"],
  "ReferenceSamples" -> settings["ValidationSamples"],
  "HighEnergyBound" -> settings["HighEnergyBound"],
  "HighEnergyCheck" -> True, "ReferenceCheck" -> True, "SpuriousPoles" -> Automatic,
  "HighEnergyTimeConstraint" -> settings["TimeConstraint"],
  "ReferenceTimeConstraint" -> settings["TimeConstraint"],
  "KinematicMap" -> Automatic, "KinematicKind" -> "Sample",
  "TimeConstraint" -> settings["TimeConstraint"], "Tolerance" -> 0];

completionHEAutomaticBasis[expression_, metadata_Association,
    settings_Association, bound_] := Module[{terms},
  terms = Expand[expression];
  terms = If[Head[terms] === Plus, List @@ terms, {terms}];
  Select[terms, !FreeQ[Denominator[#], _PropDen | _SpinorChain | _MomProd |
        _Mandelstahm | _Mom | _xFactor | _xTildeFactor] ||
    Lookup[highEnergyCheckReport[#, metadata, 1, settings["RandomSeed"],
      settings["ParameterRules"], settings["TimeConstraint"], bound], "Status"] === "FAIL" &]
];

completionHighEnergyWork[input_, channels_List, model_List, settings_Association,
    normalized_Association, retain_] := Module[
  {original = normalized["Expression"], metadata, before, basis, inheritedBasis,
   inheritedParameters, unknowns, trial, extra, bound, physical, energy,
   physicalSolution, physicalCandidate, spurious, reference, groups,
   equations, solution, family, free, candidate,
   validation, validations, familyValidations = {}, familyValid = True,
   ruleSets, status, diagnostics = {}, callback = settings["ProgressFunction"],
   enabled = settings["ProgressReporting"], report},
  report = <|"Stage" -> "HighEnergy", "Status" -> "UNRESOLVED",
    "Amplitude" -> original, "OriginalExpression" -> Lookup[normalized, "OriginalExpression", original],
    "ExternalMassRules" -> normalized["ExternalMassRules"],
    "FamilyAmplitude" -> original, "FreeParameters" -> {}, "CorrectionBasis" -> {}, "RemainingBasis" -> {},
    "Correction" -> 0, "Diagnostics" -> {}, "Scope" -> "BoundedSampledExactLinearCompletion"|>;
  retain[report];
  metadata = completionHEMetadata[normalized];
  If[!AssociationQ[metadata], Return[Join[report, <|"Diagnostics" -> {
    "Complete external mass metadata is required for high-energy completion."}|>]]];
  bound = Replace[settings["HighEnergyBound"], Automatic -> (4 - Length[metadata["ExternalLegs"]])];
  completionProgress[callback, enabled, <|"Stage" -> "HighEnergy", "Phase" -> "InitialChecks"|>];
  before = completionHECheck[original, normalized, channels, model, settings, settings["RandomSeed"]];
  report = Join[report, <|"BeforeReport" -> before, "ExpectedPower" -> bound|>];
  retain[report];
  inheritedParameters = Lookup[normalized, "FreeParameters", {}];
  inheritedBasis = Lookup[normalized, "RemainingBasis", {}];
  If[completionHEAllPassQ[before] && inheritedParameters === {},
    completionProgress[callback, enabled, <|"Stage" -> "HighEnergy", "Phase" -> "IndependentValidation"|>];
    ruleSets = completionValidationRuleSets[model, settings["ParameterRules"],
      settings["ValidationParameterRules"]];
    If[!ListQ[ruleSets] || ruleSets === {}, Return[Join[report,
      <|"Diagnostics" -> {"Independent validation parameter assignments are invalid or unavailable."}|>]]];
    validations = MapIndexed[Function[{parameterRules, index},
      <|"ParameterRules" -> parameterRules, "Report" -> completionHECheck[original,
        normalized, channels, model, Join[settings, <|"ParameterRules" -> parameterRules|>],
        settings["RandomSeed"] + 104729 First[index]]|>], ruleSets];
    Return[Join[report, <|"Status" -> If[
        AllTrue[validations, completionHEAllPassQ[#["Report"]] &], "COMPLETE", "UNRESOLVED"],
      "AfterReport" -> First[validations]["Report"], "ValidationReports" -> validations,
      "ValidationParameterRules" -> ruleSets,
      "Diagnostics" -> {"The supplied amplitude was not changed and was independently rechecked.",
        If[Length[ruleSets] === 1, "Only the supplied parameter assignment was validated; no generic parameter-independence claim is made.",
          "Additional supplied or theory-safe parameter assignments were also checked."]}|>]]];
  trial = Lookup[normalized, "FamilyAmplitude", original];
  unknowns = inheritedParameters;
  basis = settings["CorrectionBasis"];
  If[basis === Automatic,
    basis = If[inheritedParameters =!= {}, {},
      completionHEAutomaticBasis[original, metadata, settings, bound]]];
  basis = DeleteDuplicates[basis];
  If[Length[basis] + Length[inheritedParameters] > settings["MaxCorrectionTerms"],
    Return[Join[report, <|"Diagnostics" -> {"The correction family exceeds MaxCorrectionTerms; no directions were silently discarded."}|>]]];
  If[basis =!= {}, extra = Table[Unique["highEnergyCorrection"], {Length[basis]}];
    unknowns = Join[unknowns, extra]; trial = trial + Total[MapThread[Times, {extra, basis}]]];
  report = Join[report, <|"CorrectionBasis" -> Join[inheritedBasis, basis],
    "FamilyAmplitude" -> trial, "FreeParameters" -> unknowns|>];
  retain[report];
  If[unknowns === {}, Return[Join[report, <|"Diagnostics" -> {
    "The checks fail, but no supported residue-preserving correction directions are available. Supply a bounded CorrectionBasis or factorization-completion details with RemainingBasis; no independent contact operators were invented."}|>]]];
  completionProgress[callback, enabled, <|"Stage" -> "HighEnergy", "Phase" -> "PhysicalConstraints"|>];
  physical = completionResidueConstraints[trial, unknowns, normalized, settings["Samples"],
    settings["RandomSeed"], settings["ParameterRules"], settings["TimeConstraint"], callback, enabled];
  If[AssociationQ[physical] && Lookup[physical, "Status"] === "COMPLETE",
    physicalSolution = completionSolveLinear[physical["Equations"], unknowns];
    report = Join[report, <|"ConstraintData" -> <|"Physical" -> physical|>,
      "PhysicalLinearSystem" -> physicalSolution|>];
    retain[report];
    If[!MemberQ[{"COMPLETE", "UNDERDETERMINED"}, Lookup[physicalSolution, "Status"]],
      Return[Join[report, <|"Diagnostics" -> Join[{
        "No correction in this bounded family establishes the required physical residues; later conditions cannot repair an inconsistent physical system."},
        Lookup[physicalSolution, "Diagnostics", {}]]|>]]];
    If[physicalSolution["FreeParameters"] === {},
      physicalCandidate = trial /. physicalSolution["SolutionRules"];
      report = Join[report, <|"Amplitude" -> physicalCandidate,
        "FamilyAmplitude" -> physicalCandidate, "FreeParameters" -> {}, "RemainingBasis" -> {},
        "Correction" -> physicalCandidate - original,
        "SolutionRules" -> physicalSolution["SolutionRules"]|>];
      retain[report];
      If[residueExactZero[physicalCandidate - original] && !completionHEAllPassQ[before],
        completionProgress[callback, enabled, <|"Stage" -> "HighEnergy",
          "Phase" -> "Finished", "Status" -> "UNRESOLVED"|>];
        Return[Join[report, <|"Diagnostics" -> {
          "The fitted physical residues uniquely fix every available correction coefficient, leaving the input amplitude unchanged. No nonzero residue-preserving correction is available in this bounded family.",
          "The existing complete-check report is retained; its failed or unresolved checks were not repaired. A larger justified correction space is required."}|>]]];
      physical = Join[physical, <|"Equations" ->
        (physical["Equations"] /. physicalSolution["SolutionRules"])|>];
      trial = physicalCandidate; unknowns = {}]];
  energy = completionHEEnergyConstraints[trial, unknowns, metadata, settings["Samples"],
    settings["RandomSeed"], settings["ParameterRules"], bound, callback, enabled];
  spurious = completionHESpuriousConstraints[trial, unknowns, normalized, metadata,
    settings["Samples"], settings["RandomSeed"], settings["ParameterRules"], callback, enabled];
  reference = completionHEReferenceConstraints[trial, unknowns, normalized, metadata,
    settings["Samples"], settings["RandomSeed"], settings["ParameterRules"], callback, enabled];
  groups = <|"Physical" -> physical, "HighEnergy" -> energy,
    "Spurious" -> spurious, "Reference" -> reference|>;
  report = Join[report, <|"ConstraintData" -> groups|>];
  retain[report];
  If[AnyTrue[Values[groups], !AssociationQ[#] || Lookup[#, "Status"] =!= "COMPLETE" &],
    Return[Join[report, <|"Diagnostics" -> Join[{
      "The supported exact constraint set could not be completed; the input amplitude was retained."},
      Flatten[Lookup[Values[groups], "Diagnostics", {}]]]|>]]];
  equations = Flatten[Lookup[Values[groups], "Equations", {}]];
  completionProgress[callback, enabled, <|"Stage" -> "HighEnergy", "Phase" -> "Solve",
    "EquationCount" -> Length[equations], "UnknownCount" -> Length[unknowns]|>];
  solution = completionSolveLinear[equations, unknowns];
  report = Join[report, <|"LinearSystem" -> solution|>];
  retain[report];
  If[!MemberQ[{"COMPLETE", "UNDERDETERMINED"}, Lookup[solution, "Status"]],
    Return[Join[report, <|"Diagnostics" -> Join[{
      "No supported simultaneous correction was established within this bounded family."},
      Lookup[solution, "Diagnostics", {}]]|>]]];
  family = trial /. solution["SolutionRules"];
  free = Select[Lookup[solution, "FreeParameters", {}],
    !residueExactZero[D[family, #]] &];
  candidate = family /. Thread[free -> 0];
  report = Join[report, <|"Amplitude" -> candidate, "FamilyAmplitude" -> family,
    "FreeParameters" -> free, "SolutionRules" -> solution["SolutionRules"],
    "RemainingBasis" -> (D[family, #] & /@ free), "Correction" -> candidate - original|>];
  retain[report];
  completionProgress[callback, enabled, <|"Stage" -> "HighEnergy", "Phase" -> "IndependentValidation"|>];
  ruleSets = completionValidationRuleSets[model, settings["ParameterRules"],
    settings["ValidationParameterRules"]];
  If[!ListQ[ruleSets] || ruleSets === {}, Return[Join[report,
    <|"Diagnostics" -> {"Independent validation parameter assignments are invalid or unavailable."}|>]]];
  validations = MapIndexed[Function[{parameterRules, index},
    <|"ParameterRules" -> parameterRules, "Report" -> completionHECheck[candidate,
      normalized, channels, model, Join[settings, <|"ParameterRules" -> parameterRules|>],
      settings["RandomSeed"] + 104729 First[index]]|>], ruleSets];
  validation = First[validations]["Report"];
  If[free =!= {},
    completionProgress[callback, enabled, <|"Stage" -> "HighEnergy", "Phase" -> "IndependentFamilyValidation"|>];
    familyValidations = MapIndexed[Function[{parameterRules, index}, Module[
      {validationSeed = settings["RandomSeed"] + 104729 First[index], checks},
      checks = <|"Physical" -> completionResidueConstraints[family, free, normalized,
          settings["ValidationSamples"], validationSeed, parameterRules,
          settings["TimeConstraint"], callback, False],
        "HighEnergy" -> completionHEEnergyConstraints[family, free, metadata,
          settings["ValidationSamples"], validationSeed, parameterRules, bound, callback, False],
        "Spurious" -> completionHESpuriousConstraints[family, free, normalized, metadata,
          settings["ValidationSamples"], validationSeed, parameterRules, callback, False],
        "Reference" -> completionHEReferenceConstraints[family, free, normalized, metadata,
          settings["ValidationSamples"], validationSeed, parameterRules, callback, False]|>;
      <|"ParameterRules" -> parameterRules, "Checks" -> checks|>]], ruleSets];
    familyValid = AllTrue[familyValidations, AllTrue[Values[#["Checks"]],
      AssociationQ[#] && Lookup[#, "Status"] === "COMPLETE" &&
        AllTrue[Lookup[#, "Equations", {}], highEnergyZeroState[#] === "Zero" &] &] &]];
  status = Which[!AllTrue[validations, completionHEAllPassQ[#["Report"]] &] || !familyValid, "UNRESOLVED",
    free =!= {}, "UNDERDETERMINED", True, "COMPLETE"];
  diagnostics = Join[Lookup[solution, "Diagnostics", {}],
    If[free =!= {}, {"The requested conditions leave nonzero correction directions undetermined. Their zero-parameter representative is retained for inspection; additional model information is required."}, {}],
    If[!AllTrue[validations, completionHEAllPassQ[#["Report"]] &], {
      "The candidate failed or could not complete independent validation; no complete amplitude is claimed."}, {}],
    If[!familyValid, {"The remaining affine family did not satisfy every exact held-out constraint; the reported fit-only freedom is not a validated amplitude family."}, {}],
    If[Length[ruleSets] === 1, {
      "Only the supplied parameter assignment was validated; no generic parameter-independence claim is made."}, {
      "The fitted correction was also checked at additional parameter assignments supplied by the user or generated by a theory-safe validation rule."}],
    {"All fit constraints are exact at the recorded samples. Independent samples are supplementary checks, not a generic analytic proof or a universal contact reconstruction."}];
  completionProgress[callback, enabled, <|"Stage" -> "HighEnergy", "Phase" -> "Finished", "Status" -> status|>];
  Join[report, <|"Status" -> status, "Amplitude" -> candidate,
    "FamilyAmplitude" -> family, "FreeParameters" -> free,
    "RemainingBasis" -> (D[family, #] & /@ free),
    "SolutionRules" -> solution["SolutionRules"], "AfterReport" -> validation,
    "ValidationReports" -> validations, "FamilyValidation" -> familyValidations,
    "ValidationParameterRules" -> ruleSets,
    "Correction" -> candidate - original, "Diagnostics" -> diagnostics|>]
];

completionHighEnergyStage[input_, channels_List, model_List, settings_Association] := Module[
  {normalized, report, retained, remaining, rawExpression, started = AbsoluteTime[],
   seconds = settings["TimeConstraint"], output = settings["Output"],
   rules = residueParameterRules[model, settings["ParameterRules"]], effectiveSettings},
  If[!MemberQ[{"Expression", "Details"}, output] ||
      !(settings["CorrectionBasis"] === Automatic || ListQ[settings["CorrectionBasis"]]) ||
      !completionParameterRulesQ[rules] ||
      !(settings["ValidationParameterRules"] === Automatic ||
        ListQ[settings["ValidationParameterRules"]] &&
          AllTrue[settings["ValidationParameterRules"],
            completionParameterRulesQ]) ||
      !AllTrue[{settings["Samples"], settings["ValidationSamples"], settings["MaxCorrectionTerms"]},
        IntegerQ[#] && # > 0 &] || !IntegerQ[settings["RandomSeed"]] ||
      !MemberQ[{True, False}, settings["ProgressReporting"]] ||
      !(settings["ProgressFunction"] === Automatic ||
        MatchQ[settings["ProgressFunction"], _Function | _Symbol]) ||
      !(seconds === Infinity || NumericQ[seconds] &&
        TrueQ[Element[seconds, Reals] && 0 < seconds < Infinity]) ||
      !(settings["HighEnergyBound"] === Automatic || NumericQ[settings["HighEnergyBound"]] &&
        TrueQ[Element[settings["HighEnergyBound"], Reals] && Abs[settings["HighEnergyBound"]] < Infinity]) ||
      Complement[Keys[settings], First /@ Options[CompleteHighEnergy]] =!= {},
    Return[Failure["InvalidCompletionOptions", <|"MessageTemplate" -> "Invalid high-energy completion options."|>]]];
  rawExpression = completionInputExpression[input];
  normalized = TimeConstrained[completionNormalizeInput[input, channels, model],
    seconds, Missing["NormalizationTimeout"]];
  If[MissingQ[normalized], Return[completionFormat[<|"Stage" -> "HighEnergy",
    "Status" -> "UNRESOLVED", "Amplitude" -> rawExpression,
    "OriginalInput" -> input, "OriginalExpression" -> rawExpression,
    "Correction" -> 0, "CorrectionBasis" -> {}, "RemainingBasis" -> {},
    "FamilyAmplitude" -> rawExpression, "FreeParameters" -> {},
    "ElapsedSeconds" -> (AbsoluteTime[] - started),
    "Diagnostics" -> {"Input normalization exceeded TimeConstraint. The original input is retained without claiming a normalized amplitude."}|>, output]]];
  If[FailureQ[normalized] || normalized === $Failed, Return[normalized]];
  effectiveSettings = Join[settings, <|"ParameterRules" -> rules,
    "ValidationParameterRules" -> completionValidationRuleSets[model,
      settings["ParameterRules"], settings["ValidationParameterRules"]]|>];
  retained = <|"Stage" -> "HighEnergy", "Status" -> "UNRESOLVED",
      "Amplitude" -> normalized["Expression"], "OriginalExpression" -> normalized["OriginalExpression"],
      "ExternalMassRules" -> normalized["ExternalMassRules"],
      "Correction" -> 0, "CorrectionBasis" -> {},
      "RemainingBasis" -> Lookup[normalized, "RemainingBasis", {}],
      "FamilyAmplitude" -> Lookup[normalized, "FamilyAmplitude", normalized["Expression"]],
      "FreeParameters" -> Lookup[normalized, "FreeParameters", {}],
      "Diagnostics" -> {}|>;
  remaining = If[seconds === Infinity, Infinity, Max[0, seconds - (AbsoluteTime[] - started)]];
  If[remaining === 0, Return[completionFormat[Join[retained,
    <|"ElapsedSeconds" -> (AbsoluteTime[] - started), "Diagnostics" -> {
      "Input normalization exhausted TimeConstraint; the normalized input was retained."}|>], output]]];
  report = TimeConstrained[completionHighEnergyWork[input, channels, model, effectiveSettings, normalized,
      Function[value, retained = value]], remaining,
    Join[retained, <|"Status" -> "UNRESOLVED", "Diagnostics" -> Append[
      Lookup[retained, "Diagnostics", {}],
      "High-energy completion exceeded TimeConstraint. The best retained candidate is provided for inspection, not claimed complete."]|>]];
  completionFormat[Join[report, <|"ParameterRules" -> rules,
    "ElapsedSeconds" -> (AbsoluteTime[] - started)|>], output]
];

End[];
End[];
