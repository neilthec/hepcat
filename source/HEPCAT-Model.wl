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
RegularExpression["\[(\\d)(\\d)\]"]:>"SpinorChain[Spinor[\""<>SpinHelicity[masses[[ToExpression["$1"]]]]<>"\",\"Square\",$1],Spinor[\""<>SpinHelicity[masses[[ToExpression["$2"]]]]<>"\",\"Square\",$2]]",
RegularExpression["\[(\\d)(\\d)(\\d)>"]:>"SpinorChain[Spinor[\""<>SpinHelicity[masses[[ToExpression["$1"]]]]<>"\",\"Square\",$1],Mom[$2],Spinor[\""<>SpinHelicity[masses[[ToExpression["$3"]]]]<>"\",\"Angle\",$3]]",
RegularExpression["s(\\d)(\\d)-([a-zA-Z0-9]+)\\^2"]:>"PropDen[Mom[Multiparticle[$1,$2]],$3]",
RegularExpression["s(\\d)(\\d)"]:>"PropDen[Mom[Multiparticle[$1,$2]],0]"
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
newAmp+=(z-zPlus[amp[[j,-3]]])amp[[j,-1]]/.{z->zPlus[amp[[j,-3]]]};
newAmp+=(z-zMinus[amp[[j,-3]]])amp[[j,-1]]/.{z->zMinus[amp[[j,-3]]]};
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
CreateDiagrams::usage = "CreateDiagrams[p1, p2, ..., model] generates tree-level constructive Feynman diagrams. Each internal photon or gluon is one generic line containing both allowed helicities; external helicities remain fixed.";
CreateArrowDiagram::usage = "CreateArrowDiagram[diag] draws a labeled tree diagram. Options: \"ShowVertexExpressions\", \"ShowPropagatorExpressions\".";
CreateArrowDiagrams::usage = "CreateArrowDiagrams[{diag1, diag2, ...}] draws a list of diagrams.";
diagramAmplitude::usage = "diagramAmplitude[diag] returns the constructive amplitude, summing the correlated helicities of every internal photon/gluon, including propagator and spin-index structure, with external x factors expanded and known mass rules applied.";
diagramAmplitudes::usage = "diagramAmplitudes[diagrams] returns {{massRules, amplitude}, ...}, one entry per generated diagram. Internal photon/gluon helicities are summed inside each amplitude. Supported four-point vector exchanges use the simplified kinematic form. Legacy helicity-resolved four-point diagram lists are also accepted. Known mass rules are applied to the amplitude and returned for external legs and both ends of every internal line.";



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

replaceVertexIndices[expr_, slotRules_Association] := expr /. {
  Spinor[a___, i_Integer] /; KeyExistsQ[slotRules, i] :> Spinor[a, slotRules[i]],
  xFactor[i_Integer, j_Integer] /; KeyExistsQ[slotRules, i] && KeyExistsQ[slotRules, j] :>
    xFactor[slotRules[i], slotRules[j]],
  xTildeFactor[i_Integer, j_Integer] /; KeyExistsQ[slotRules, i] && KeyExistsQ[slotRules, j] :>
    xTildeFactor[slotRules[i], slotRules[j]]
};

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
PhysicalPoleResidue::usage = "PhysicalPoleResidue[expression, channel] extracts a pole coefficient. A list of channels extracts a compatible multiple-pole coefficient. Without a coherent KinematicMap the result is only FORMAL. A map f[expression, channels, poleVariables] must evaluate all spinors, momenta and propagators consistently near the selected cuts. KinematicKind distinguishes Sample from Symbolic charts. No internal on-shell condition is inferred from diagram mass rules.";
CheckPhysicalResidues::usage = "CheckPhysicalResidues[diagramAmplitudes, channels, model] checks physical residues against the ordinary rows returned by FactorizationAmplitudeChannels, automatically discovers apparent denominator poles, and reports both kinds together. Only the supplied channels certify physical poles; equivalent scalar invariants and massless bracket factors are recognized using external mass shells and momentum conservation. Other supported poles are tested for cancellation in the full summed amplitude while known physical propagators remain nonzero. Unsupported denominator structures remain UNRESOLVED. The physical comparison applies the sewing factor -1 from (i A_L)(i A_R)(i/PropDen)/i. PASS means sampled agreement or finiteness; Opp Sign means physical residues agree only with opposite signs; FAIL means disagreement or a surviving singularity. Output -> \"Residues\" shows one visible row per sample, including residues or leading divergent coefficients, pole orders and finite limits; \"Summary\" returns combined counts; \"Details\" includes PoleDiscovery, PhysicalSummary, SpuriousSummary and all diagnostics. SpuriousPoles -> Automatic is the default; an explicit list overrides discovery; {} requests the legacy physical-only report. Samples, RandomSeed, ParameterRules and TimeConstraint control the comparisons. These sampled checks are not a generic analytic proof. Inputs remain unchanged and no amplitude correction is added.";
DiagramAmplitudeRecords::args = "Expected a list of diagram associations and valid diagram-local simplification options.";
PhysicalPoleResidue::args = "Expected channel associations with Cut, Complement, Mass and ExternalLegs, and valid diagnostic options.";
CheckPhysicalResidues::args = "Expected diagram amplitudes, the ordinary channel rows returned by FactorizationAmplitudeChannels, a model, and valid comparison options.";
CheckPhysicalResidues::input = "`1`";

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
  mapped = map[expr, channels, variables];
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
    SeedRandom[Hash[{seed, sample, cut, channel["Mass"]}, "CRC32"]];
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
  "TimeConstraint" -> 30, "UncutChannels" -> {}};
PhysicalPoleResidue[expr_, channel_Association, opts : OptionsPattern[]] :=
  PhysicalPoleResidue[expr, {channel}, opts];
PhysicalPoleResidue[expr_, input_List, OptionsPattern[]] := Module[
  {channels, variables, structural, map = OptionValue["KinematicMap"],
   kind = OptionValue["KinematicKind"], seconds = OptionValue["TimeConstraint"],
   uncut = OptionValue["UncutChannels"], result, signatures},
  If[input === {} || !AllTrue[input, residueChannelQ] ||
      !MemberQ[{"Sample", "Symbolic"}, kind] ||
      !(seconds === Infinity || TrueQ[NumericQ[seconds] && seconds > 0]) ||
      !ListQ[uncut] || !AllTrue[uncut, residueChannelQ],
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
    structural = residueStructuralCoefficient[expr, channels, variables];
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

DownValues[CheckPhysicalResidues] = {};
Options[CheckPhysicalResidues] = Join[Options[PhysicalPoleResidue], {
  "Tolerance" -> 10^-10, "Samples" -> 4, "RandomSeed" -> 1729,
  "ParameterRules" -> {}, "Output" -> "Table", "SpuriousPoles" -> Automatic}];

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
    opts : OptionsPattern[CheckPhysicalResidues]] := Module[
  {map = OptionValue[CheckPhysicalResidues, {opts}, "KinematicMap"],
   kind = OptionValue[CheckPhysicalResidues, {opts}, "KinematicKind"],
   seconds = OptionValue[CheckPhysicalResidues, {opts}, "TimeConstraint"],
   tolerance = OptionValue[CheckPhysicalResidues, {opts}, "Tolerance"],
   uncut = OptionValue[CheckPhysicalResidues, {opts}, "UncutChannels"],
   samples = OptionValue[CheckPhysicalResidues, {opts}, "Samples"],
   seed = OptionValue[CheckPhysicalResidues, {opts}, "RandomSeed"],
   rules = OptionValue[CheckPhysicalResidues, {opts}, "ParameterRules"],
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
      !AllTrue[rules, MatchQ[#, _Rule] && Head[First[#]] === Symbol &&
        !residueUnmappedQ[Last[#]] &],
    Message[CheckPhysicalResidues::args]; Return[$Failed]];
  physical = Flatten[Lookup[records, "PhysicalPropagators", {}], 1];
  allChannels = Join[physical, targets, uncut];
  signatures = residueExternalMassSignature /@ allChannels;
  If[AnyTrue[signatures, MissingQ] || Length[DeleteDuplicates[signatures]] > 1 ||
      !residueSameProcessQ[Join[records, allChannels]],
    Message[CheckPhysicalResidues::args]; Return[$Failed]];
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

CheckPhysicalResidues[amplitudes_, channelRows_List, model_List,
    opts : OptionsPattern[]] := Module[
  {output = OptionValue["Output"], factors = OptionValue["SpuriousPoles"],
   inputs, report, physical, spurious, combined, externalRules, external,
   automatic, targets, records, keys, discovery = {}, candidates, check,
   seconds = OptionValue["TimeConstraint"]},
  If[!MemberQ[{"Table", "Residues", "Summary", "Details"}, output] ||
      !(factors === Automatic || ListQ[factors]),
    Message[CheckPhysicalResidues::args]; Return[$Failed]];
  inputs = residueSimpleInputs[amplitudes, channelRows, model];
  If[FailureQ[inputs],
    Message[CheckPhysicalResidues::input, inputs["MessageTemplate"]]; Return[$Failed]];
  If[inputs === $Failed,
    Message[CheckPhysicalResidues::args]; Return[$Failed]];
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
  report = residueCheckReport[inputs["Expression"], targets, records, opts];
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
            {candidate["CanonicalFactor"]}, physical, opts]],
          <|"Type" -> "Unclassified", "Status" -> "UNRESOLVED", "Samples" -> {},
            "Diagnostics" -> Join[Lookup[candidate, "Diagnostics", {}], {
              "This denominator could not be classified reliably from the supplied physical channels."}]|>];
        Join[check, <|"Pole" -> candidate["Factor"],
          "CanonicalFactor" -> candidate["CanonicalFactor"], "Discovery" -> candidate|>]
      ], candidates],
      spuriousCheckReport[inputs["Expression"], factors, physical, opts]];
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
  Switch[output,
    "Table", report["Table"],
    "Residues", If[factors === {}, residueReportResidues[report["Channels"]],
      residueUnifiedSamples[report["Channels"], report["SpuriousChecks"]]],
    "Summary", report["Summary"],
    "Details", report]
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
  scanExpressions = Join[{expr}, Flatten[Function[record,
    {Lookup[record, "Expression", 0], Lookup[record, "OriginalExpression", 0]} /.
      Lookup[record, "MassRules", {}]] /@ records, 1]];
  denominators = Join[Cases[scanExpressions, Power[base_, exponent_] /;
      candidateFactorQ[base] &&
      !(NumericQ[exponent] && TrueQ[exponent >= 0]) :> {base, -exponent}, {0, Infinity}],
    Cases[scanExpressions, reciprocal : ((Csc | Sec | Cot | Csch | Sech | Coth)[argument_]) /;
        poleKinematicFactorQ[argument] :>
      {Switch[Head[reciprocal], Csc, Sin[argument], Sec, Cos[argument], Cot, Tan[argument],
        Csch, Sinh[argument], Sech, Cosh[argument], Coth, Tanh[argument]], 1}, {0, Infinity}]];
  factorPieces[base_, power_] := Module[{factored, factors},
    factored = If[IntegerQ[power],
      Quiet[Check[TimeConstrained[Factor[base, Extension -> Automatic], 2, base], base]], base];
    factors = If[Head[factored] === Times, List @@ factored, {factored}];
    Select[Map[Function[factor, If[Head[factor] === Power,
      {factor[[1]], power factor[[2]]}, {factor, power}]],
      Select[factors, candidateFactorQ]],
      !(NumericQ[Last[#]] && TrueQ[Last[#] <= 0]) &]
  ];
  pieces = Flatten[(factorPieces @@ # &) /@ denominators, 1];
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
  completed = TimeConstrained[
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
    seconds, False];
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
    opts : OptionsPattern[CheckPhysicalResidues]] := Module[
  {seconds = OptionValue[CheckPhysicalResidues, {opts}, "TimeConstraint"],
   samples = OptionValue[CheckPhysicalResidues, {opts}, "Samples"],
   seed = OptionValue[CheckPhysicalResidues, {opts}, "RandomSeed"],
   rules = OptionValue[CheckPhysicalResidues, {opts}, "ParameterRules"],
   map = OptionValue[CheckPhysicalResidues, {opts}, "KinematicMap"]},
  spuriousCheckOne[expr, #, targets, seconds, samples, seed, rules, map] & /@
    DeleteDuplicates[factors]
];

End[];
