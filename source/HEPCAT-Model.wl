(* ::Package:: *)

(* ::Subsection::Closed:: *)
(*Model Reader*)


(* ::Subsubsection::Closed:: *)
(*Model Reader*)


(* ::Input::Initialization:: *)
ReadModel[dir_,num_]:=Module[{particleList,amplitudeList,variableList,functionList},
particleList=ReadParticles[dir<>"/prtcls"<>ToString[num]<>".mdl"];
amplitudeList=ReadAmplitudes[dir<>"/amps"<>ToString[num]<>".mdl",particleList];
variableList=ReadVariables[dir<>"/vars"<>ToString[num]<>".mdl"];
functionList=ReadFunctions[dir<>"/func"<>ToString[num]<>".mdl"];

{particleList,amplitudeList,variableList,functionList}
]


(* ::Subsubsection::Closed:: *)
(*Variable Reader*)


(* ::Input::Initialization:: *)
ReadVariables[file_]:=Module[{str,variableList={},line={""}},
str=OpenRead[file];
line=StringTrim[StringSplit[ReadLine[str],"|"]];
While[!StringContainsQ[line[[1]],"======"],
If[Length[line]==3&&!StringContainsQ[line[[1]],RegularExpression["Name"]],
PlainText[ToExpression[line[[1]]]]=line[[1]];
SPINAS[ToExpression[line[[1]]]]=line[[1]];
Unprotect[TeX];TeX[ToExpression[line[[1]]]]=line[[1]];Protect[TeX];
Do[line[[ii]]=ToExpression[line[[ii]]];,{ii,1,2}];
Unprotect[Conjugate];Conjugate[line[[1]]]=line[[1]];Protect[Conjugate];
AppendTo[variableList,line];
];
line=StringTrim[StringSplit[ReadLine[str],"|"]];
];
Close[str];
variableList
]


(* ::Subsubsection::Closed:: *)
(*Function Reader*)


(* ::Input::Initialization:: *)
ReadFunctions[file_]:=Module[{str,functionList={},line={""},rside},
str=OpenRead[file];
line=StringTrim[StringSplit[ReadLine[str],"|"]];
While[!StringContainsQ[line[[1]],"======"],
If[Length[line]==2&&!StringContainsQ[line[[1]],RegularExpression["Name"]],
PlainText[ToExpression[line[[1]]]]=line[[1]];
SPINAS[ToExpression[line[[1]]]]=line[[1]];
Unprotect[TeX];TeX[ToExpression[line[[1]]]]=line[[1]];Protect[TeX];
line[[1]]=ToExpression[line[[1]]];
Unprotect[Conjugate];
Conjugate[line[[1]]]=line[[1]];Protect[Conjugate];
rside=StringTrim[StringSplit[line[[2]],"%"]];
line[[2]]=ToExpression[rside[[1]]];
If[Length[rside]==2,AppendTo[line,rside[[2]]],AppendTo[line,""]];
AppendTo[functionList,line];
];
line=StringTrim[StringSplit[ReadLine[str],"|"]];
];
Close[str];
functionList
]


(* ::Subsubsection::Closed:: *)
(*Particle Reader*)


(* ::Input::Initialization:: *)
ReadParticles[file_]:=Module[{str,particleList={},line={""}},
str=OpenRead[file];
While[!StringContainsQ[line[[1]],"======"],
line=StringTrim[StringSplit[ReadLine[str],"|"]];
If[Length[line]==11&&!StringContainsQ[line[[1]],RegularExpression["Full\\s*Name"]],
Do[line[[ii]]=ToExpression[line[[ii]]];,{ii,4,6}];
line[[8]]=ToExpression[line[[8]]];
AppendTo[particleList,line];
];
];
Close[str];
particleList
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
ReadAmplitudes[file_,particleList_]:=Module[{str,amplitudeList={},line={""}},
str=OpenRead[file];
While[!StringContainsQ[line[[1]],"======"],
line=StringTrim[StringSplit[ReadLine[str],"|"]];
If[Length[line]>=5&&line[[3]]!="P3"&&!StringContainsQ[line[[1]],"%"],
line[[-2]]=ToExpression[line[[-2]]];
line[[-1]]=ConvertAmplitude[line,particleList];
AppendTo[amplitudeList,line];
];
];
Close[str];
(*AddOppositeHelicityAmplitudes[amplitudeList]*)
amplitudeList
]


(* ::Input::Initialization:: *)
ConvertAmplitude[line_,particleList_]:=Module[{masses={},res,SpinHelicity},
SpinHelicity[mass_]:=If[mass===0,"Helicity","Spin"];
Do[AppendTo[masses,ParticleMass[line[[ii]],particleList]],{ii,1,Length[line]-2}];
(*If[line[[4]]=!="",AppendTo[masses,ParticleMass[line[[4]],particleList]]];*)
res=StringReplace[line[[-1]],{
RegularExpression["x(\\d)(\\d)"]:>"xFactor[$1,$2]",
RegularExpression["xt(\\d)(\\d)"]:>"xTildeFactor[$1,$2]",
RegularExpression["<(\\d)(\\d)>"]:>"SpinorChain[Spinor[\""<>SpinHelicity[masses[[ToExpression["$1"]]]]<>"\",\"Angle\",$1],Spinor[\""<>SpinHelicity[masses[[ToExpression["$2"]]]]<>"\",\"Angle\",$2]]",
RegularExpression["\[(\\d)(\\d)\]"]:>"SpinorChain[Spinor[\""<>SpinHelicity[masses[[ToExpression["$1"]]]]<>"\",\"Square\",$1],Spinor[\""<>SpinHelicity[masses[[ToExpression["$2"]]]]<>"\",\"Square\",$2]]"
}];
ToExpression[res]
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
AppendTo[amplitude,vertices[[i,-1]]/.{xFactor[a__]:>xTildeFactor[a],xTildeFactor[a__]:>xFactor[a],"Angle"->"Square","Square"->"Angle"}];
AppendTo[newVertices,amplitude];
];
,{i,1,Length[vertices]}];
newVertices
]


(* ::Subsection::Closed:: *)
(*Diagrams*)


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
(*Remove .+ and .- for massless particle names*)
Do[
particles[[ii]]=StringTrim[StringSplit[parts[[ii]],"."]];
If[Length[particles[[ii]]]>1,helicitySigns[[ii]]=ToExpression[particles[[ii,2]]<>"1"]];
particles[[ii]]=particles[[ii,1]];
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
If[spinp>4,AppendTo[indices,i4]];


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
LookupAmplitude[taggedParticles_List,model_,rightSide_,{i1_,i2_,i3_,i4_}]:=Module[{amp=0,i,j,k,replacements,doubleSpin,indices},
(*Find the amp*)
Do[
If[Sort[DeleteCases[DeleteCases[Flatten[taggedParticles],_Integer],Multiparticle[__]]]===Sort[DeleteCases[Drop[model[[2,i]],-2],""]],
(*Print[model[[2,i]]];*)
replacements={};
Do[
If[taggedParticles[[k,1]]===model[[2,i,j]],AppendTo[replacements,j->taggedParticles[[k,2]]]];
,{k,1,Length[taggedParticles]},{j,1,Length[model[[2,i]]]-2}];
(*Print[replacements];*)
amp=model[[2,i,-2]]model[[2,i,-1]]/.{SpinorChain[args__]:>SpinorChain@@(List[args]/.replacements),xFactor[args__]:>xFactor@@(List[args]/.replacements),xTildeFactor[args__]:>xTildeFactor@@(List[args]/.replacements)};
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
If[doubleSpin>4,AppendTo[indices,i4]];
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
(*ExpandXFactors[expr_]*)


(* ::Input::Initialization:: *)
ExpandXFactors[expr_,n_]:=Module[{newExp,particleIndices,missingXReplacement,missingXTildeReplacement,XiN=10},
particleIndices[term_]:=Which[
  IntegerQ[term],{term},
  MatchQ[term,Multiparticle[__]],List @@ term,
  True,{}
];
(*missingXReplacement[l_,j_]:=
  SpinorChain[SpinorHat["Xi","Angle",XiN],MomHat[j],SpinorHat["Helicity","Square",l]]/Mass[j]/SpinorChain[ SpinorHat["Xi","Angle",XiN++],SpinorHat["Helicity","Angle",l]];
missingXTildeReplacement[l_,j_]:=  SpinorChain[SpinorHat["Xi","Square",XiN],MomHat[j],SpinorHat["Helicity","Angle",l]]/Mass[j]/SpinorChain[ SpinorHat["Xi","Square",XiN++],SpinorHat["Helicity","Square",l]];*)
missingXReplacement[l_,j_]:=
  SpinorChain[SpinorHat["Helicity","Square",l],MomHat[XiN],MomHat[j],SpinorHat["Helicity","Square",l]]/Mass[j]/PropDen[MomHat[Multiparticle[l,XiN]],Mass[XiN++]];
missingXTildeReplacement[l_,j_]:=  SpinorChain[SpinorHat["Helicity","Angle",l],MomHat[XiN],MomHat[j],SpinorHat["Helicity","Angle",l]]/Mass[j]/PropDen[MomHat[Multiparticle[l,XiN]],Mass[XiN++]];

newExp=expr/.{xFactor[i_,j_]xTildeFactor[k_,l_]SpinorChain[SpinorHat["Spin","Angle",i_],SpinorHat["Spin","Angle",j_]]SpinorChain[SpinorHat["Spin","Square",k_],SpinorHat["Spin","Square",l_]]+xFactor[k_,l_]xTildeFactor[i_,j_]SpinorChain[SpinorHat["Spin","Angle",k_],SpinorHat["Spin","Angle",l_]]SpinorChain[SpinorHat["Spin","Square",i_],SpinorHat["Spin","Square",j_]]:>SpinorChain[SpinorHat["Spin","Angle",i],SpinorHat["Spin","Angle",k]]SpinorChain[SpinorHat["Spin","Square",j],SpinorHat["Spin","Square",l]]+SpinorChain[SpinorHat["Spin","Angle",i],SpinorHat["Spin","Angle",l]]SpinorChain[SpinorHat["Spin","Square",j],SpinorHat["Spin","Square",k]]+SpinorChain[SpinorHat["Spin","Square",i],SpinorHat["Spin","Square",k]]SpinorChain[SpinorHat["Spin","Angle",j],SpinorHat["Spin","Angle",l]]+SpinorChain[SpinorHat["Spin","Square",i],SpinorHat["Spin","Square",l]]SpinorChain[SpinorHat["Spin","Angle",j],SpinorHat["Spin","Angle",k]]
};
newExp/.{
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
    xFactor[i,j]
  ]
]
}
];


(* ::Subsubsection::Closed:: *)
(*FactorizationAmplitude[particles_List,model_]*)


(* ::Text:: *)
(*This function uses FactorizationPartitions to create the partitions and then calls LookupAmplitude to see if each side is nonzero.  If they are, it sews them together.*)


(* ::Input::Initialization:: *)
FactorizationAmplitudeChannels[particles_List,model_]:=Module[{partitions,j,i,k,channel,momenta1,momenta2,amp1,amp2,amp12A,amp12B,amplitude={},masses={},xFactorReverseProtect,xFactorReverseProtectInverse,Multipart,XiN=1},
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
If[amp1=!=0&&amp2=!=0,
amp12A=ReduceSpinContractions[amp1 amp2]/.{
Mass[momenta1]->model[[1,j,6]],
Mom[Multiparticle[parts__]]:>Total[Mom/@{parts}]
};];
(*Now switch sides for particle and antiparticle, if they are different, and combine*)
amp12B=0;
If[model[[1,j,2]]=!=model[[1,j,3]],
channel=partitions[[i]];
channel[[1]]=Append[channel[[1]],{model[[1,j,3]],momenta1}];
channel[[2]]=Append[channel[[2]],{model[[1,j,2]],momenta2}];
(*Print[channel];*)
amp1=ReverseMomentum[LookupAmplitude[channel[[1]],model,False,{i1,i2,i3,i4}],momenta1]//.xFactorReverseProtect/.momenta1->momenta2//.xFactorReverseProtectInverse;
If[amp1=!=0,amp2=LookupAmplitude[channel[[2]],model,True,{i1,i2,i3,i4}]];
If[amp1=!=0&&amp2=!=0,
amp12B=ReduceSpinContractions[amp1 amp2]/.{
Mass[momenta1]->model[[1,j,6]],
Mom[Multiparticle[parts__]]:>Total[Mom/@{parts}]
};];];
(*Combine*)
(*I think it will be better to rewrite models to have separate lines for internal photons where there are 4-point vertices and no x factors and external photons.  Using the x factors (or their replacements) just create unecessary complications when they always simplify to the same thing based on the spin of the particles on the ends.*)
If[amp12A=!=0||amp12B=!=0,
AppendTo[amplitude,Flatten[{masses,momenta2,model[[1,j,6]],ExpandXFactors[Simplify[ComplexifyMomenta[(amp12A+amp12B)/z/PropDen[Mom[momenta2],model[[1,j,6]]]]],Length[particles]]}]];
];
(*Print[Expand[amplitude[[-1]]]]];*)
,{j,1,Length[model[[1]]]},{i,1,Length[partitions]}];
amplitude
]


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
newAmp+=(z-zPlus[amp[[j,-3]]])amp[[j,-1]]/.{z->zPlus[amp[[j,-3]]]};
newAmp+=(z-zMinus[amp[[j,-3]]])amp[[j,-1]]/.{z->zMinus[amp[[j,-3]]]};
,{j,1,Length[amp]}];
newAmp
]


(* ::Subsubsection::Closed:: *)
(*UnHatSpinors[amp_]*)


(* ::Input::Initialization:: *)
UnHatSpinors[amp_]:=amp/.SpinorHat[a__]:>Spinor[a]
