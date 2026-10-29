(* ::Package:: *)

(* ::Subsection::Closed:: *)
(*Formatting*)


(* ::Subsubsection::Closed:: *)
(*nice Formatting*)


(* ::Input::Initialization:: *)
Format[ConstructiveAmplitude[coupl_,numer_,denom_]]:=coupl numer/denom
nice[rr[a_,b_]]:=Subscript["r",ToString[a]<>ToString[b]]
nice[Multiparticle[a__]]:=Subscript[ToString[P],a]
nice[MultiparticleDiff[a__]]:=Subscript[ToString[P],a]
nice[Particle[a_]]:=ToString[a]
nice[Spinor["Helicity","Angle",a_]]:=Row[List[nice[a],"\[RightAngleBracket]"]]
lnice[Spinor["Helicity","Angle",a_]]:=Row[List["\[LeftAngleBracket]",nice[a]]]
nice[Spinor["Helicity","Square",a_]]:=Row[List[nice[a],"]"]]
lnice[Spinor["Helicity","Square",a_]]:=Row[List["[",nice[a]]]
nice[Spinor["Helicity","Angle",a_,J_]]:=Row[List[nice[a],"\[RightAngleBracket]"]]
lnice[Spinor["Helicity","Angle",a_,J_]]:=Row[List["\[LeftAngleBracket]",nice[a]]]
nice[Spinor["Helicity","Square",a_,J_]]:=Row[List[nice[a],"]"]]
lnice[Spinor["Helicity","Square",a_,J_]]:=Row[List["[",nice[a]]]
nice[Spinor["Zeta","Angle",a_]]:=Row[List[Subscript[\[Zeta],nice[a]],"\[RightAngleBracket]"]]
lnice[Spinor["Zeta","Angle",a_]]:=Row[List["\[LeftAngleBracket]",Subscript[\[Zeta],nice[a]]]]
nice[Spinor["Zeta","Square",a_]]:=Row[List[Subscript[\[Zeta],nice[a]],"]"]]
lnice[Spinor["Zeta","Square",a_]]:=Row[List["[",Subscript[\[Zeta],nice[a]]]]
nice[Spinor["Xi","Angle",a_]]:=Row[List[Subscript[\[Xi],nice[a]],"\[RightAngleBracket]"]]
lnice[Spinor["Xi","Angle",a_]]:=Row[List["\[LeftAngleBracket]",Subscript[\[Xi],nice[a]]]]
nice[Spinor["Xi","Square",a_]]:=Row[List[Subscript[\[Xi],nice[a]],"]"]]
lnice[Spinor["Xi","Square",a_]]:=Row[List["[",Subscript[\[Xi],nice[a]]]]
nice[Spinor["Spin","Angle",a_]]:=Row[List[Style[nice[a],Bold],"\[RightAngleBracket]"]]
lnice[Spinor["Spin","Angle",a_]]:=Row[List["\[LeftAngleBracket]",Style[nice[a],Bold]]]
nice[Spinor["Spin","Square",a_]]:=Row[List[Style[nice[a],Bold],"]"]]
lnice[Spinor["Spin","Square",a_]]:=Row[List["[",Style[nice[a],Bold]]]
nice[Spinor["Spin","Lower","Angle",a_,J_]]:=Row[List[Subscript[Style[nice[a],Bold],J],"\[RightAngleBracket]"]]
lnice[Spinor["Spin","Lower","Angle",a_,J_]]:=Row[List["\[LeftAngleBracket]",Subscript[Style[nice[a],Bold],J]]]
nice[Spinor["Spin","Lower","Square",a_,J_]]:=Row[List[Subscript[Style[nice[a],Bold],J],"]"]]
lnice[Spinor["Spin","Lower","Square",a_,J_]]:=Row[List["[",Subscript[Style[nice[a],Bold],J]]]
nice[Spinor["Spin","Upper","Angle",a_,J_]]:=Row[List[Superscript[Style[nice[a],Bold],J],"\[RightAngleBracket]"]]
lnice[Spinor["Spin","Upper","Angle",a_,J_]]:=Row[List["\[LeftAngleBracket]",Superscript[Style[nice[a],Bold],J]]]
nice[Spinor["Spin","Upper","Square",a_,J_]]:=Row[List[Superscript[Style[nice[a],Bold],J],"]"]]
lnice[Spinor["Spin","Upper","Square",a_,J_]]:=Row[List["[",Superscript[Style[nice[a],Bold],J]]]
nice[SpinorHat["Helicity","Angle",a_]]:=Row[List[OverHat[nice[a]],"\[RightAngleBracket]"]]
lnice[SpinorHat["Helicity","Angle",a_]]:=Row[List["\[LeftAngleBracket]",OverHat[nice[a]]]]
nice[SpinorHat["Helicity","Square",a_]]:=Row[List[OverHat[nice[a]],"]"]]
lnice[SpinorHat["Helicity","Square",a_]]:=Row[List["[",OverHat[nice[a]]]]
nice[SpinorHat["Xi","Angle",a_]]:=Row[List[Subscript[OverHat[\[Xi]],nice[a]],"\[RightAngleBracket]"]]
lnice[SpinorHat["Xi","Angle",a_]]:=Row[List["\[LeftAngleBracket]",Subscript[OverHat[\[Xi]],nice[a]]]]
nice[SpinorHat["Xi","Square",a_]]:=Row[List[Subscript[OverHat[\[Xi]],nice[a]],"]"]]
lnice[SpinorHat["Xi","Square",a_]]:=Row[List["[",Subscript[OverHat[\[Xi]],nice[a]]]]
nice[SpinorHat["Spin","Angle",a_]]:=Row[List[Style[OverHat[nice[a]],Bold],"\[RightAngleBracket]"]]
lnice[SpinorHat["Spin","Angle",a_]]:=Row[List["\[LeftAngleBracket]",Style[OverHat[nice[a]],Bold]]]
nice[SpinorHat["Spin","Square",a_]]:=Row[List[Style[OverHat[nice[a]],Bold],"]"]]
lnice[SpinorHat["Spin","Square",a_]]:=Row[List["[",Style[OverHat[nice[a]],Bold]]]
nice[SpinorHat["Spin","Lower","Angle",a_,J_]]:=Row[List[Subscript[Style[OverHat[nice[a]],Bold],J],"\[RightAngleBracket]"]]
lnice[SpinorHat["Spin","Lower","Angle",a_,J_]]:=Row[List["\[LeftAngleBracket]",Subscript[Style[OverHat[nice[a]],Bold],J]]]
nice[SpinorHat["Spin","Lower","Square",a_,J_]]:=Row[List[Subscript[Style[OverHat[nice[a]],Bold],J],"]"]]
lnice[SpinorHat["Spin","Lower","Square",a_,J_]]:=Row[List["[",Subscript[Style[OverHat[nice[a]],Bold],J]]]
nice[SpinorHat["Spin","Upper","Angle",a_,J_]]:=Row[List[Superscript[Style[OverHat[nice[a]],Bold],J],"\[RightAngleBracket]"]]
lnice[SpinorHat["Spin","Upper","Angle",a_,J_]]:=Row[List["\[LeftAngleBracket]",Superscript[Style[OverHat[nice[a]],Bold],J]]]
nice[SpinorHat["Spin","Upper","Square",a_,J_]]:=Row[List[Superscript[Style[OverHat[nice[a]],Bold],J],"]"]]
lnice[SpinorHat["Spin","Upper","Square",a_,J_]]:=Row[List["[",Superscript[Style[OverHat[nice[a]],Bold],J]]]
nice[sigma[i_]]:=Subscript["\[Sigma]",ToString[i]]
nice[delta]:="\[Delta]"
Format[SpinorChain[a__]]:=nice[SpinorChain[a]]
(*Format[SpinorChainReduced[a__]]:=nice[SpinorChain[a]]*)
nice[SpinorChain[a_,c_]]:=Row[List[lnice[a],nice[c]]]
nice[SpinorChain[a_,b__,c_]]:=Row[List[lnice[a],"|",((nice[#]&/@List[b])/.List[d__]:>d),"|",nice[c]]]
Format[SpinorTrace[a__]]:=nice[SpinorTrace[a]]
nice[SpinorTrace[a__]]:=Row[List["Tr(",((nice[#]&/@List[a])/.List[b__]:>b),")"]]
Format[xFactor[a__]]:=nice[xFactor[a]]
nice[xFactor[Multiparticle[a_,c_],b_]]:=Subscript["x",ToString[a]<>ToString[c]<>","<>ToString[b]]
nice[xFactor[a_,Multiparticle[b_,c_]]]:=Subscript["x",ToString[a]<>","<>ToString[b]<>ToString[c]]
nice[xFactor[Multiparticle[a_,d_],Multiparticle[b_,c_]]]:=Subscript["x",ToString[a]<>ToString[d]<>","<>ToString[b]<>ToString[c]]
nice[xFactor[a_,b_]]:=Subscript["x",ToString[a]<>","<>ToString[b]]
Format[xTildeFactor[a__]]:=nice[xTildeFactor[a]]
nice[xTildeFactor[Multiparticle[a_,c_],b_]]:=Subscript["\!\(\*OverscriptBox[\(x\), \(~\)]\)",ToString[a]<>ToString[c]<>","<>ToString[b]]
nice[xTildeFactor[a_,Multiparticle[b_,c_]]]:=Subscript["\!\(\*OverscriptBox[\(x\), \(~\)]\)",ToString[a]<>","<>ToString[b]<>ToString[c]]
nice[xTildeFactor[a_,b_]]:=Subscript["\!\(\*OverscriptBox[\(x\), \(~\)]\)",ToString[a]<>","<>ToString[b]]
nice[xFactor[Multiparticle[a_,c_,d_],b_]]:=Subscript["x",ToString[a]<>ToString[c]<>ToString[d]<>","<>ToString[b]]
nice[xFactor[a_,Multiparticle[b_,c_,d_]]]:=Subscript["x",ToString[a]<>","<>ToString[b]<>ToString[c]<>ToString[d]]
nice[xTildeFactor[Multiparticle[a_,c_,d_],b_]]:=Subscript["\!\(\*OverscriptBox[\(x\), \(~\)]\)",ToString[a]<>ToString[c]<>ToString[d]<>","<>ToString[b]]
nice[xTildeFactor[a_,Multiparticle[b_,c_,d_]]]:=Subscript["\!\(\*OverscriptBox[\(x\), \(~\)]\)",ToString[a]<>","<>ToString[b]<>ToString[c]<>ToString[d]]
Format[SpinorZ[a__]]:=nice[SpinorZ[a]]
nice[SpinorZ["Both",II_,JJ_]]:=Superscript[Subscript["z",II],JJ]
nice[SpinorZ["Lower",II_]]:=Subscript["z",II]
nice[SpinorZ["Upper",JJ_]]:=Superscript["z",JJ]
Format[SpinorEps[a__]]:=nice[SpinorEps[a]]
nice[SpinorEps["Lower",PP_,JJ_]]:=Subscript["\[Epsilon]",ToString[PP]<>","<>ToString[JJ]]
nice[SpinorEps["Upper",PP_,JJ_]]:=Superscript["\[Epsilon]",ToString[PP]<>","<>ToString[JJ]]
Format[Mass[a_]]:=nice[Mass[a]]
nice[Mass[Multiparticle[a__]]]:=Subscript[M, a]
nice[Mass[a_]]:=Subscript[M, a]
Format[Mom[a_]]:=nice[Mom[a]]
nice[Mom[Multiparticle[a__]]]:=Subscript[p, a]
nice[Mom[a_]]:=Subscript[p, a]
Format[MomQ[a_]]:=nice[MomQ[a]]
nice[MomQ[Multiparticle[a__]]]:=Subscript[q, a]
nice[MomQ[a_]]:=Subscript[q, a]
Format[MomMagnitude[a_]]:=nice[MomMagnitude[a]]
nice[MomMagnitude[a_]]:=Subscript[p, a]
Format[Theta[a_]]:=nice[Theta[a]]
nice[Theta[a_]]:=Subscript[\[Theta],a]
Format[Phi[a_]]:=nice[Phi[a]]
nice[Phi[a_]]:=Subscript[\[Phi],a]
Format[MomProd[a__]]:=nice[MomProd[a]]
nice[MomProd[a_,b_]]:=Subscript[p, a] . Subscript[p, b]
Format[MomHat[a_]]:=nice[MomHat[a]]
nice[MomHat[a_]]:=Subscript[OverHat[p], a]
nice[MomHat[Multiparticle[a__]]]:=Subscript[OverHat[p], a]
Format[MomProdHat1[a__]]:=nice[MomProdHat1[a]]
Format[MomProdHat2[a__]]:=nice[MomProdHat2[a]]
Format[MomProdHat12[a__]]:=nice[MomProdHat12[a]]
nice[MomProdHat1[a_,b_]]:=Subscript[OverHat[p], a] . Subscript[p, b]
nice[MomProdHat2[a_,b_]]:=Subscript[p, a] . Subscript[OverHat[p], b]
nice[MomProdHat12[a_,b_]]:=Subscript[OverHat[p], a] . Subscript[OverHat[p], b]
Format[En[a_]]:=nice[En[a]]
nice[En[a_]]:=Subscript[\[ScriptCapitalE],a]
nice[a_+b_]:=nice[a]+nice[b]
nice[a_*b_]:=nice[a]nice[b]
nice[a_/b_]:=nice[a]/nice[b]
nice[a_^b_]:=nice[a]^b
nice[a_/;NumberQ[a]]:=a
Output[zp]:=nice[zp]
nice[zp]:=Subscript[ToString[z],p]
nice[{a___}]:=Map[nice,{a}]
Format[PropDen[a__]]:=nice[PropDen[a]]
nice[PropDen[p_,m_]]:=nice[p]^2-m^2
nice[PropDen[p_,m_,w_]]:=nice[p]^2-m^2+I m w
Format[Coupling[a_]]:=nice[Coupling[a]]
nice[Coupling[a_]]:=Subscript["g", a]
Format[Mandelstahm[i_,j_]]:=nice[Mandelstahm[i,j]]
nice[Mandelstahm[i_,j_]]:=Subscript["s",ToString[i]<>ToString[j]]
Format[MandelstahmHat[i_,j_]]:=nice[MandelstahmHat[i,j]]
nice[MandelstahmHat[i_,j_]]:=Subscript[OverHat["s"],ToString[i]<>ToString[j]]
nice[xk]:=Subscript["k","x"]
nice[xq]:=Subscript["q","x"]
Format[zPlus[a_]]:=nice[zPlus[a]]
nice[zPlus[Multiparticle[a__]]]:=Power[Subscript["z",a],"+"]
Format[zMinus[a_]]:=nice[zMinus[a]]
nice[zMinus[Multiparticle[a__]]]:=Power[Subscript["z",a],"+"]


(* ::Subsubsection::Closed:: *)
(*LATEX Formatting*)


(* ::Input::Initialization:: *)
TeX[ConstructiveAmplitude[coupl_,numer_,denom_]]:=TeX[coupl numer/denom]
TeX[Multiparticle[a_,b_]]:="P_{"<>ToString[a]<>ToString[b]<>"} "
TeX[Multiparticle[a_,b_,c_]]:="P_{"<>ToString[a]<>ToString[b]<>ToString[c]<>"} "
TeX[Particle[a_]]:=ToString[a]
TeX[Spinor["Helicity","Angle",a_]]:=TeX[a]<>"\\rangle"
lTeX[Spinor["Helicity","Angle",a_]]:="\\langle"<>TeX[a]
TeX[Spinor["Helicity","Square",a_]]:=TeX[a]<>"\\rbrack "
lTeX[Spinor["Helicity","Square",a_]]:="\\lbrack"<>TeX[a]
TeX[Spinor["Zeta","Angle",a_]]:="\\zeta_{"<>TeX[a]<>"}\\rangle"
lTeX[Spinor["Zeta","Angle",a_]]:="\\langle\\zeta_{"<>TeX[a]<>"}"
TeX[Spinor["Zeta","Square",a_]]:="\\tilde{\\zeta}_{"<>TeX[a]<>"}\\rbrack"
lTeX[Spinor["Zeta","Square",a_]]:="\\lbrack\\tilde{\\zeta}_{"<>TeX[a]<>"}"
TeX[Spinor["Spin","Angle",a_]]:="\\mathbf{"<>TeX[a]<>"}\\rangle "
lTeX[Spinor["Spin","Angle",a_]]:="\\langle\\mathbf{"<>TeX[a]<>"}"
TeX[Spinor["Spin","Square",a_]]:="\\mathbf{"<>TeX[a]<>"}\\rbrack "
lTeX[Spinor["Spin","Square",a_]]:="\\lbrack\\mathbf{"<>TeX[a]<>"}"
TeX[Spinor["Spin","Lower","Angle",a_,J_]]:="\\mathbf{"<>TeX[a]<>"}_{"<>ToString[J]<>"}\\rangle "
lTeX[Spinor["Spin","Lower","Angle",a_,J_]]:="\\langle\\mathbf{"<>TeX[a]<>"}_{"<>ToString[J]<>"}"
TeX[Spinor["Spin","Lower","Square",a_,J_]]:="\\mathbf{"<>TeX[a]<>"}_{"<>ToString[J]<>"}\\rbrack "
lTeX[Spinor["Spin","Lower","Square",a_,J_]]:="\\lbrack\\mathbf{"<>TeX[a]<>"}_{"<>ToString[J]<>"}"
TeX[Spinor["Spin","Upper","Angle",a_,J_]]:="\\mathbf{"<>TeX[a]<>"}^{"<>ToString[J]<>"}\\rangle "
lTeX[Spinor["Spin","Upper","Angle",a_,J_]]:="\\langle\\mathbf{"<>TeX[a]<>"}^{"<>ToString[J]<>"}"
TeX[Spinor["Spin","Upper","Square",a_,J_]]:="\\mathbf{"<>TeX[a]<>"}^{"<>ToString[J]<>"}\\rbrack "
lTeX[Spinor["Spin","Upper","Square",a_,J_]]:="\\lbrack\\mathbf{"<>TeX[a]<>"}^{"<>ToString[J]<>"}"
TeX[sigma[i_]]:="\\sigma_{"<>ToString[i]<>"} "
TeX[delta]:="\\delta "
TeX[SpinorChain[a_,c_]]:=lTeX[a]<>TeX[c]
TeX[SpinorChain[a_,b__,c_]]:=lTeX[a]<>"\\lvert "<>((TeX[#]&/@List[b])/.List[d__]:>d)<>"\\rvert "<>TeX[c]
TeX[SpinorTrace[a__]]:="\\mbox{Tr}\\left("<>((TeX[#]&/@List[a])/.List[b__]:>b)<>"\\right) "
TeX[xFactor[a_,b_]]:="x_{"<>ToString[a]<>ToString[b]<>"} "
TeX[xTildeFactor[a_,b_]]:="\\tilde{x}_{"<>ToString[a]<>ToString[b]<>"} "
TeX[Mass[Multiparticle[a__]]]:="M_{"<>ToString[a]<>"} "
TeX[Mass[a_]]:="M_{"<>ToString[a]<>"} "
TeX[Mom[Multiparticle[a_,b_]]]:="p_{"<>ToString[a]<>ToString[b]<>"} "
TeX[Mom[Multiparticle[a_,b_,c_]]]:="p_{"<>ToString[a]<>ToString[b]<>ToString[c]<>"} "
TeX[Mom[a_]]:="p_{"<>ToString[a]<>"} "
TeX[MomProd[a_,b_]]:="p_{"<>ToString[a]<>"}\\cdot p_{"<>ToString[b]<>"} "
TeX[PropDen[p_,m_]]:="\\left["<>TeX[p]<>"^2-"<>TeX[m]<>"^2 "<>"\\right]"
TeX[PropDen[p_,m_,w_]]:="\\left["<>TeX[p]<>"^2-"<>TeX[m]<>"^2+i"<>TeX[m]<>TeX[w]<>"\\right]"
TeX[Coupling[a_]]:="g_{"<>ToString[a]<>"} "
TeX[Sqrt[a__]]:="\\sqrt{"<>TeX[a]<>"}"
(*TeX[a_+b_]:="\\left("<>TeX[a]<>"+"<>TeX[b]<>"\\right)"*)
TeX[Plus[a__]]:=a/.Plus->TeXPlus;
TeXPlus[a__]:="\\left("<>StringJoin[Riffle[TeX[#]&/@List[a],"+"]]<>"\\right)";
TeX[a_*b_]:=TeX[a]<>TeX[b]
TeX[Rational[a_,b_]]:="\\frac{"<>TeX[a]<>"}{"<>TeX[b]<>"} "
TeX[a_/b_]:="\\frac{"<>TeX[Numerator[a/b]]<>"}{"<>TeX[Denominator[a/b]]<>"} "
TeX[a_^b_]:=TeX[a]<>"^{"<>ToString[b]<>"} "
TeX[a_/;NumberQ[a]]:=ToString[a]
TeX[Mandelstahm[i_,j_]]:="s_{"<>ToString[i]<>ToString[j]<>"}"


(* ::Subsubsection::Closed:: *)
(*PlainText Formatting*)


(* ::Input::Initialization:: *)
PlainText[ConstructiveAmplitude[coupl_,numer_,denom_]]:=PlainText[coupl numer/denom]
PlainText::usage = "PlainText[expression] writes readable spinor-bracket notation. PlainText[expression, \"Model\"] writes a model-readable expression, with explicit multiplication and typed forms where compact brackets would be ambiguous; in this mode unsupported or inexact objects return Failure rather than losing information.";
PlainText[Multiparticle[a__]]:="P_{"<>ConstructiveDiagrams`Private`plainTextParticleLabels[{a}]<>"} "
PlainText[Particle[a_]]:=ToString[a]
PlainText[Spinor["Helicity","Angle",a_]]:=PlainText[a]<>">"
lPlainText[Spinor["Helicity","Angle",a_]]:="<"<>PlainText[a]
PlainText[Spinor["Helicity","Square",a_]]:=PlainText[a]<>"]"
lPlainText[Spinor["Helicity","Square",a_]]:="["<>PlainText[a]
PlainText[Spinor["Xi","Angle",a_]]:="xi_{"<>PlainText[a]<>"}>"
lPlainText[Spinor["Xi","Angle",a_]]:="<xi_{"<>PlainText[a]<>"}"
PlainText[Spinor["Xi","Square",a_]]:="xi_{"<>PlainText[a]<>"}]"
lPlainText[Spinor["Xi","Square",a_]]:="[xi_{"<>PlainText[a]<>"}"
PlainText[Spinor["Zeta","Angle",a_]]:="zeta_{"<>PlainText[a]<>"}>"
lPlainText[Spinor["Zeta","Angle",a_]]:="<zeta_{"<>PlainText[a]<>"}"
PlainText[Spinor["Zeta","Square",a_]]:="tzeta_{"<>PlainText[a]<>"}]"
lPlainText[Spinor["Zeta","Square",a_]]:="[tzeta_{"<>PlainText[a]<>"}"
PlainText[Spinor["Spin","Angle",a_]]:=PlainText[a]<>">"
lPlainText[Spinor["Spin","Angle",a_]]:="<"<>PlainText[a]
PlainText[Spinor["Spin","Square",a_]]:=PlainText[a]<>"]"
lPlainText[Spinor["Spin","Square",a_]]:="["<>PlainText[a]
PlainText[Spinor["Spin","Lower","Angle",a_,J_]]:=PlainText[a]<>"_{"<>ToString[J]<>"}>"
lPlainText[Spinor["Spin","Lower","Angle",a_,J_]]:="<"<>PlainText[a]<>"_{"<>ToString[J]<>"}"
PlainText[Spinor["Spin","Lower","Square",a_,J_]]:=PlainText[a]<>"_{"<>ToString[J]<>"}]"
lPlainText[Spinor["Spin","Lower","Square",a_,J_]]:="["<>PlainText[a]<>"_{"<>ToString[J]<>"}"
PlainText[Spinor["Spin","Upper","Angle",a_,J_]]:=PlainText[a]<>"^{"<>ToString[J]<>"}>"
lPlainText[Spinor["Spin","Upper","Angle",a_,J_]]:="<"<>PlainText[a]<>"^{"<>ToString[J]<>"}"
PlainText[Spinor["Spin","Upper","Square",a_,J_]]:=PlainText[a]<>"^{"<>ToString[J]<>"}]"
lPlainText[Spinor["Spin","Upper","Square",a_,J_]]:="["<>PlainText[a]<>"^{"<>ToString[J]<>"}"
PlainText[sigma[i_]]:="sigma_{"<>ToString[i]<>"} "
PlainText[delta]:="delta "
PlainText[SpinorChain[a_,c_]]:=lPlainText[a]<>PlainText[c]
PlainText[SpinorChain[a_,b__,c_]]:=lPlainText[a]<>"|"<>StringRiffle[PlainText/@{b},"|"]<>"|"<>PlainText[c]
PlainText[SpinorTrace[a__]]:="Tr("<>StringRiffle[PlainText/@{a},","]<>")"
PlainText[xFactor[a_,b_]]:="x_{"<>ToString[a]<>ToString[b]<>"} "
PlainText[xTildeFactor[a_,b_]]:="tx_{"<>ToString[a]<>ToString[b]<>"} "
PlainText[Mass[Multiparticle[a__]]]:="M_{"<>ConstructiveDiagrams`Private`plainTextParticleLabels[{a}]<>"} "
PlainText[Mass[a_]]:="M_{"<>ToString[a]<>"} "
PlainText[Mom[Multiparticle[a__]]]:="p_{"<>ConstructiveDiagrams`Private`plainTextParticleLabels[{a}]<>"} "
PlainText[Mom[a_]]:=ToString[a]
PlainText[MomProd[a_,b_]]:="p_{"<>ToString[a]<>"}.p_{"<>ToString[b]<>"} "
PlainText[PropDen[p_,m_]]:="("<>PlainText[p]<>"^2-"<>PlainText[m]<>"^2 "<>")"
PlainText[PropDen[p_,m_,w_]]:="("<>PlainText[p]<>"^2-"<>PlainText[m]<>"^2+i"<>PlainText[m]<>PlainText[w]<>")"
PlainText[Coupling[a_]]:="g_{"<>ToString[a]<>"} "
PlainText[Sqrt[a__]]:="sqrt("<>PlainText[a]<>")"
(*PlainText[a_+b_]:="("<>PlainText[a]<>"+"<>PlainText[b]<>")"*)
PlainText[Plus[a__]]:=PlainTextPlus[a];
PlainTextPlus[a__]:="("<>StringJoin[Riffle[PlainText[#]&/@List[a],"+"]]<>")";
PlainText[a_*b_]:=PlainText[a]<>"*"<>PlainText[b]
PlainText[Rational[a_,b_]]:="("<>PlainText[a]<>")/("<>PlainText[b]<>") "
PlainText[a_/b_]:="("<>PlainText[Numerator[a/b]]<>")/("<>PlainText[Denominator[a/b]]<>") "
PlainText[a_^b_]:=ConstructiveDiagrams`Private`plainTextPower[a,b]
PlainText[a_/;NumberQ[a]]:=ToString[a]
PlainText[Mandelstahm[i_,j_]]:="s"<>ToString[i]<>ToString[j]
PlainText[a_Symbol]:=SymbolName[a]
PlainText[a_String]:=a
PlainText[expression_,"Model"]:=ConstructiveDiagrams`Private`amplitudeModelText[expression]

Begin["ConstructiveDiagrams`Private`"];

plainTextParticleLabels[labels_List] := If[AllTrue[labels, IntegerQ[#] && 0 <= # <= 9 &],
  StringJoin[ToString /@ labels], StringRiffle[ToString /@ labels, ","]];

plainTextPower[base_, exponent_] := Module[{left = PlainText[base], right = PlainText[exponent]},
  If[MemberQ[{Plus, Times, Power, Rational, Complex}, Head[base]] || TrueQ[base < 0],
    left = "(" <> left <> ")"];
  If[!StringQ[right], right = ToString[exponent, InputForm]];
  left <> "^" <> If[IntegerQ[exponent], right, "(" <> right <> ")"] <> " "
];

amplitudeModelText[expression_] := Module[
  {tag = Unique["amplitudeText"], emit, fail, namePattern,
   typedHeads, scalarHeads, compactChainQ, compactPropagatorQ, result},
  namePattern = RegularExpression["[A-Za-z_][A-Za-z_0-9]*"];
  typedHeads = {SpinorChain, Spinor, SpinorTrace, Mom, MomProd, Multiparticle,
    Mass, PropDen, xFactor, xTildeFactor, Coupling, Mandelstahm, ConstructiveDiagrams`VectorSpinIndex};
  scalarHeads = {Sin, Cos, Tan, ArcSin, ArcCos, ArcTan, Exp, Log, Abs};
  fail[value_] := Throw[Failure["UnsupportedPlainText", <|
    "MessageTemplate" -> "This object has no lossless model-text representation.",
    "Object" -> value|>], tag];
  compactChainQ[chain_] := Head[chain] === SpinorChain && Length[chain] >= 2 &&
    AllTrue[{First[chain], Last[chain]}, MatchQ[#,
      Spinor["Spin" | "Helicity", "Angle" | "Square", _Integer]] &] &&
    AllTrue[Most[Rest[List @@ chain]], MatchQ[#, Mom[_Integer]] &] &&
    AllTrue[Join[{chain[[1, 3]], chain[[-1, 3]]},
      First /@ Most[Rest[List @@ chain]]], 1 <= # <= 9 &];
  compactPropagatorQ[propagator_] := MatchQ[propagator,
      PropDen[Mom[Multiparticle[_Integer, _Integer]],
        _Integer | ConstructiveDiagrams`ModelParameter[_String, _String, "Real"]]] &&
    AllTrue[List @@ propagator[[1, 1]], 1 <= # <= 9 &] &&
    (!IntegerQ[propagator[[2]]] || propagator[[2]] >= 0);
  emit[value_] := Which[
    IntegerQ[value], ToString[value, InputForm],
    Head[value] === Rational, "(" <> emit[Numerator[value]] <> "/" <> emit[Denominator[value]] <> ")",
    Head[value] === Real, fail[value],
    Head[value] === Complex, "(" <> emit[Re[value]] <> "+I*(" <> emit[Im[value]] <> "))",
    value === Pi, "Pi",
    StringQ[value] && StringFreeQ[value, {"\"", "\\", "|", "\t", "\n", "\r"}], "\"" <> value <> "\"",
    MatchQ[value, ConstructiveDiagrams`ModelParameter[_String, _String, "Real"]],
      If[StringMatchQ[value[[1]], namePattern] &&
          !MemberQ[{"I", "i", "Pi", "Sqrt2"}, value[[1]]] &&
          !StringMatchQ[value[[1]], RegularExpression["(?:x|xt|s)[0-9]{2}"]],
        value[[1]], fail[value]],
    MatchQ[value, ConstructiveDiagrams`ModelFunction[_String, _String, _List]],
      If[MemberQ[{"initQCD5", "MbEff", "McEff", "MqEff"}, value[[1]]],
        value[[1]] <> "(" <> StringRiffle[emit /@ value[[3]], ","] <> ")", fail[value]],
    Head[value] === ConstructiveAmplitude, emit[Times @@ Take[List @@ value, 2]/value[[3]]],
    Head[value] === Plus, "(" <> StringRiffle[emit /@ (List @@ value), "+"] <> ")",
    Head[value] === Times, "(" <> StringRiffle[emit /@ (List @@ value), "*"] <> ")",
    value === Sqrt[2], "Sqrt[2]",
    Head[value] === Power, "(" <> emit[value[[1]]] <> ")^(" <> emit[value[[2]]] <> ")",
    compactChainQ[value],
      If[value[[1, 2]] === "Angle", "<", "["] <>
        StringJoin[ToString /@ Join[{value[[1, 3]]},
          First /@ Most[Rest[List @@ value]], {value[[-1, 3]]}]] <>
        If[value[[-1, 2]] === "Angle", ">", "]"],
    compactPropagatorQ[value],
      If[value[[2]] === 0, "", "("] <> "s" <>
        StringJoin[ToString /@ (List @@ value[[1, 1]])] <>
        If[value[[2]] === 0, "", "-" <> emit[value[[2]]] <> "^2)"],
    MemberQ[typedHeads, Head[value]] || MemberQ[scalarHeads, Head[value]],
      SymbolName[Head[value]] <> "[" <> StringRiffle[emit /@ (List @@ value), ","] <> "]",
    True, fail[value]];
  Catch[result = emit[expression]; result, tag]
];

End[];


(* ::Subsubsection::Closed:: *)
(*SPINAS Formatting*)


(* ::Input::Initialization:: *)
SPINAS[ConstructiveAmplitude[coupl_,numer_,denom_]]:=SPINAS[coupl numer/denom];
(*PlainText[Multiparticle[a_,b_]]:="P_{"<>ToString[a]<>ToString[b]<>"} "
PlainText[Multiparticle[a_,b_,c_]]:="P_{"<>ToString[a]<>ToString[b]<>ToString[c]<>"} "*)
(*PlainText[Particle[a_]]:=ToString[a]*)
(*SPINAS[Spinor["Spin","Lower","Angle",a_,J_]]:=SPINAS[a]<>"_{"<>ToString[J]<>"}>"
lSPINAS[Spinor["Spin","Lower","Angle",a_,J_]]:="<"<>SPINAS[a]<>"_{"<>ToString[J]<>"}"
SPINAS[Spinor["Spin","Lower","Square",a_,J_]]:=SPINAS[a]<>"_{"<>ToString[J]<>"}]"
lSPINAS[Spinor["Spin","Lower","Square",a_,J_]]:="["<>SPINAS[a]<>"_{"<>ToString[J]<>"}"*)

(*<i...j>*)
SPINAS[SpinorChain[Spinor["Helicity","Angle",a_],b___,Spinor["Helicity","Angle",c_]]]:="a"<>ToString[a]<>((SPINAS[#]&/@List[b])/.List[d__]:>d)<>ToString[c]<>"a.v()";
SPINAS[SpinorChain[Spinor["Spin","Upper","Angle",a_,J_],b___,Spinor["Helicity","Angle",c_]]]:="a"<>ToString[a]<>((SPINAS[#]&/@List[b])/.List[d__]:>d)<>ToString[c]<>"a.v("<>ToString[J]<>")";
SPINAS[SpinorChain[Spinor["Helicity","Angle",a_],b___,Spinor["Spin","Upper","Angle",c_,J_]]]:="a"<>ToString[a]<>((SPINAS[#]&/@List[b])/.List[d__]:>d)<>ToString[c]<>"a.v("<>ToString[J]<>")";
SPINAS[SpinorChain[Spinor["Spin","Upper","Angle",a_,Ja_],b___,Spinor["Spin","Upper","Angle",c_,Jc_]]]:="a"<>ToString[a]<>((SPINAS[#]&/@List[b])/.List[d__]:>d)<>ToString[c]<>"a.v("<>ToString[Ja]<>","<>ToString[Jc]<>")";
(*[i...j>*)
SPINAS[SpinorChain[Spinor["Helicity","Square",a_],b___,Spinor["Helicity","Angle",c_]]]:="s"<>ToString[a]<>((SPINAS[#]&/@List[b])/.List[d__]:>d)<>ToString[c]<>"a.v()";
SPINAS[SpinorChain[Spinor["Spin","Upper","Square",a_,J_],b___,Spinor["Helicity","Angle",c_]]]:="s"<>ToString[a]<>((SPINAS[#]&/@List[b])/.List[d__]:>d)<>ToString[c]<>"a.v("<>ToString[J]<>")";
SPINAS[SpinorChain[Spinor["Helicity","Square",a_],b___,Spinor["Spin","Upper","Angle",c_,J_]]]:="s"<>ToString[a]<>((SPINAS[#]&/@List[b])/.List[d__]:>d)<>ToString[c]<>"a.v("<>ToString[J]<>")";
SPINAS[SpinorChain[Spinor["Spin","Upper","Square",a_,Ja_],b___,Spinor["Spin","Upper","Angle",c_,Jc_]]]:="s"<>ToString[a]<>((SPINAS[#]&/@List[b])/.List[d__]:>d)<>ToString[c]<>"a.v("<>ToString[Ja]<>","<>ToString[Jc]<>")";
(*<i...j]*)
SPINAS[SpinorChain[Spinor["Helicity","Angle",a_],b___,Spinor["Helicity","Square",c_]]]:="a"<>ToString[a]<>((SPINAS[#]&/@List[b])/.List[d__]:>d)<>ToString[c]<>"s.v()";
SPINAS[SpinorChain[Spinor["Spin","Upper","Angle",a_,J_],b___,Spinor["Helicity","Square",c_]]]:="a"<>ToString[a]<>((SPINAS[#]&/@List[b])/.List[d__]:>d)<>ToString[c]<>"s.v("<>ToString[J]<>")";
SPINAS[SpinorChain[Spinor["Helicity","Angle",a_],b___,Spinor["Spin","Upper","Square",c_,J_]]]:="a"<>ToString[a]<>((SPINAS[#]&/@List[b])/.List[d__]:>d)<>ToString[c]<>"s.v("<>ToString[J]<>")";
SPINAS[SpinorChain[Spinor["Spin","Upper","Angle",a_,Ja_],b___,Spinor["Spin","Upper","Square",c_,Jc_]]]:="a"<>ToString[a]<>((SPINAS[#]&/@List[b])/.List[d__]:>d)<>ToString[c]<>"s.v("<>ToString[Ja]<>","<>ToString[Jc]<>")";
(*[i...j]*)
SPINAS[SpinorChain[Spinor["Helicity","Square",a_],b___,Spinor["Helicity","Square",c_]]]:="s"<>ToString[a]<>((SPINAS[#]&/@List[b])/.List[d__]:>d)<>ToString[c]<>"s.v()";
SPINAS[SpinorChain[Spinor["Spin","Upper","Square",a_,J_],b___,Spinor["Helicity","Square",c_]]]:="s"<>ToString[a]<>((SPINAS[#]&/@List[b])/.List[d__]:>d)<>ToString[c]<>"s.v("<>ToString[J]<>")";
SPINAS[SpinorChain[Spinor["Helicity","Square",a_],b___,Spinor["Spin","Upper","Square",c_,J_]]]:="s"<>ToString[a]<>((SPINAS[#]&/@List[b])/.List[d__]:>d)<>ToString[c]<>"s.v("<>ToString[J]<>")";
SPINAS[SpinorChain[Spinor["Spin","Upper","Square",a_,Ja_],b___,Spinor["Spin","Upper","Square",c_,Jc_]]]:="s"<>ToString[a]<>((SPINAS[#]&/@List[b])/.List[d__]:>d)<>ToString[c]<>"s.v("<>ToString[Ja]<>","<>ToString[Jc]<>")";

(*PlainText[SpinorTrace[a__]]:=" Tr("<>((PlainText[#]&/@List[a])/.List[b__]:>b)<>")"*)
SPINAS[Mass[Multiparticle[a__]]]:=SPINAS[Mandelstahm[a]];
SPINAS[Mass[a_]]:="M"<>ToString[a];
(*PlainText[Mom[Multiparticle[a_,b_]]]:="p_{"<>ToString[a]<>ToString[b]<>"} "
PlainText[Mom[Multiparticle[a_,b_,c_]]]:="p_{"<>ToString[a]<>ToString[b]<>ToString[c]<>"} "*)
SPINAS[Mom[a_]]:=ToString[a];
SPINAS[MomProd[a_,b_]]:=SPINAS[ExtractMandelstahm[MomProd[a,b]]];
SPINAS[PropDen[p_,m_]]:="("<>SPINAS[ExtractMandelstahm[MomProd[p,p]]]<>"-"<>SPINAS[m]<>"*"<>SPINAS[m]<>")";
SPINAS[PropDen[p_,m_,w_]]:="("<>SPINAS[ExtractMandelstahm[MomProd[p,p]]]<>"-"<>SPINAS[m]<>"*"<>SPINAS[m]<>"+cdouble(0,1)*"<>SPINAS[m]<>"*"<>SPINAS[w]<>")";
SPINAS[Coupling[a_]]:="g"<>ToString[a];
SPINAS[Sqrt[a__]]:="sqrt"<>ToString[a];
SPINAS[Plus[a__]]:=a/.Plus->SPINASPlus;
SPINASPlus[a__]:="("<>StringJoin[Riffle[SPINAS[#]&/@List[a],"+"]]<>")";SPINAS[a_*b_]:=SPINAS[a]<>"*"<>SPINAS[b];
SPINAS[Rational[a_,b_]]:="("<>SPINAS[a]<>")/("<>SPINAS[b]<>") ";
SPINAS[a_/b_]:="("<>SPINAS[Numerator[a/b]]<>")/("<>SPINAS[Denominator[a/b]]<>") ";
SPINAS[a_^b_]:=Riffle[Table[ToString[a],b],"*"]/.List[c__]:>StringJoin[c]
SPINAS[a_/;NumberQ[a]]:=ToString[a];
SPINAS[Mandelstahm[i_,j_]]:="m"<>ToString[i]<>ToString[j];


(* ::Input::Initialization:: *)
CreateSPInitializationStatement[schain_]:=Module[{lhs="",rhs="= sproduct("},
(*Do left spinor*)
Which[
schain[[1,2]]=="Angle",
lhs=lhs<>"a";
rhs=rhs<>"ANGLE,&p";,
schain[[1,2]]=="Square",
lhs=lhs<>"s";
rhs=rhs<>"SQUARE,&p";
];
lhs=lhs<>ToString[schain[[1,3]]];
rhs=rhs<>ToString[schain[[1,3]]]<>",&p";
(*Do momenta*)
Do[
lhs=lhs<>ToString[schain[[j,1]]];
rhs=rhs<>ToString[schain[[j,1]]]<>",&p";
,{j,2,Length[schain]-1}];
(*Do right spinor*)
lhs=lhs<>ToString[schain[[-1,3]]];
Which[
schain[[-1,2]]=="Angle",
lhs=lhs<>"a";,
schain[[-1,2]]=="Square",
lhs=lhs<>"s";
];
rhs=rhs<>ToString[schain[[-1,3]]]<>");";

lhs<>rhs
];


(* ::Input::Initialization:: *)
CreateSPUpdateStatement[schain_]:=Module[{lhs="",rhs=".update();"},
(*Do left spinor*)
Which[
schain[[1,2]]=="Angle",
lhs=lhs<>"a";,
schain[[1,2]]=="Square",
lhs=lhs<>"s";
];
lhs=lhs<>ToString[schain[[1,3]]];
(*Do momenta*)
Do[
lhs=lhs<>ToString[schain[[j,1]]];
,{j,2,Length[schain]-1}];
(*Do right spinor*)
lhs=lhs<>ToString[schain[[-1,3]]];
Which[
schain[[-1,2]]=="Angle",
lhs=lhs<>"a";,
schain[[-1,2]]=="Square",
lhs=lhs<>"s";
];

lhs<>rhs
];


(* ::Input::Initialization:: *)
CreateSPName[schain_]:=Module[{lhs=""},
(*Do left spinor*)
Which[
schain[[1,2]]=="Angle",
lhs=lhs<>"a";,
schain[[1,2]]=="Square",
lhs=lhs<>"s";
];
lhs=lhs<>ToString[schain[[1,3]]];
(*Do momenta*)
Do[
lhs=lhs<>ToString[schain[[j,1]]];
,{j,2,Length[schain]-1}];
(*Do right spinor*)
lhs=lhs<>ToString[schain[[-1,3]]];
Which[
schain[[-1,2]]=="Angle",
lhs=lhs<>"a";,
schain[[-1,2]]=="Square",
lhs=lhs<>"s";
];

lhs
];


(* ::Input::Initialization:: *)
InitializeSPINASSpinorProducts[amp_]:=Module[{sps={}},
sps=Sort[DeleteDuplicates[Cases[amp,SpinorChain[__],Infinity]]];
"//Spinor Products\n"<>StringJoin[Riffle[CreateSPInitializationStatement/@sps,"\n"]]
];


(* ::Input::Initialization:: *)
UpdateSPINASSpinorProducts[amp_]:=Module[{sps={}},
sps=Sort[DeleteDuplicates[Cases[amp,SpinorChain[__],Infinity]]];
"//Spinor Products\n"<>StringJoin[Riffle[CreateSPUpdateStatement/@sps,"\n"]]
];


(* ::Input::Initialization:: *)
DeclareSPINASSpinorProducts[amp_]:=Module[{sps={}},
sps=Sort[DeleteDuplicates[Cases[amp,SpinorChain[__],Infinity]]];
"//Spinor Products\n sproduct "<>StringJoin[Riffle[CreateSPName/@sps,", "]]<>";"
];


(* ::Subsection::Closed:: *)
(*Simplification *)


(* ::Subsubsection::Closed:: *)
(*applyToNumerator*)


(* ::Input::Initialization:: *)
ConstructiveDiagrams`applyToNumerator::usage = "applyToNumerator[expr, rules] applies replacement rules only to the numerator of each existing additive term, leaving its denominator unchanged. It does not expand terms or combine them into a common denominator.";

Begin["ConstructiveDiagrams`Private`"];
ConstructiveDiagrams`applyToNumerator[expr_, rules_] := Module[{terms},
  terms = If[Head[expr] === Plus, List @@ expr, {expr}];
  Total[(With[{fraction = NumeratorDenominator[#]},
    (First[fraction] /. rules)/Last[fraction]] &) /@ terms]
];
End[];


(* ::Subsubsection::Closed:: *)
(*Rules to determine whether a head is Spinor or SpinorHat, Mom or MomHat, or MomProd or MomProdHat12.*)


spinorHeadQ[head_]:=MemberQ[{Spinor,SpinorHat},head];
momHeadQ[head_]:=MemberQ[{Mom,MomHat},head];
momHead[Spinor]:=Mom;
momHead[SpinorHat]:=MomHat;
momProdHeadQ[head_]:=MemberQ[{MomProd,MomProdHat12},head];
momProdHead[Spinor]:=MomProd;
momProdHead[SpinorHat]:=MomProdHat12;
momProdHead[Mom]:=MomProd;
momProdHead[MomHat]:=MomProdHat12;


(* ::Subsubsection::Closed:: *)
(*ReduceSpinContractions*)


(* ::Input::Initialization:: *)
ReverseSequence[a__]:=Reverse[List[a]]/.List[b__]:>b


(* ::Input::Initialization:: *)
ReduceSpinContractions[exp_] := ConstructiveDiagrams`Private`spinContractionReduce[exp, True];

Begin["ConstructiveDiagrams`Private`"];

spinContractionsPossibleQ[rscExpression_, rscIncludeBareHelicity_ : True] := Module[
  {rscSpinors, rscSignatures = {}, rscParts, rscKey, rscPolarizations, rscGroups},
  If[!FreeQ[rscExpression, _SpinorEps], Return[True]];
  rscSpinors = Cases[{rscExpression}, _Spinor | _SpinorHat, Infinity];
  Do[
    rscParts = List @@ rscSpinor;
    Which[
      MatchQ[rscParts, {"Spin", "Upper" | "Lower", "Angle" | "Square", _, _}],
        rscKey = {Head[rscSpinor], "Spin", rscParts[[4]], rscParts[[5]]};
        AppendTo[rscSignatures, {rscKey, rscParts[[2]]}],
      TrueQ[rscIncludeBareHelicity] && MatchQ[rscParts, {"Helicity", "Angle" | "Square", _}],
        rscKey = {Head[rscSpinor], "Helicity", 3, rscParts[[3]]};
        AppendTo[rscSignatures, {rscKey, rscParts[[2]]}],
      MatchQ[rscParts, {"Helicity", "Angle" | "Square", _, _}],
        rscKey = {Head[rscSpinor], "Helicity", 4, rscParts[[3]], rscParts[[4]]};
        AppendTo[rscSignatures, {rscKey, rscParts[[2]]}]], {rscSpinor, rscSpinors}];
  rscGroups = GatherBy[rscSignatures, First];
  AnyTrue[rscGroups, Function[rscGroup,
    rscPolarizations = Last /@ rscGroup;
    ContainsAll[rscPolarizations, {"Upper", "Lower"}] ||
      ContainsAll[rscPolarizations, {"Angle", "Square"}]]]
];

spinContractionReduce[rscExpression_, rscExpand_] := Module[
{rscResult=rscExpression,rscOldResult,rscReplacementRules,rscAttempts=1},
If[!spinContractionsPossibleQ[rscExpression],Return[rscExpression]];
rscReplacementRules={
(*SpinorChain[...Spinor...]SpinorChain[...Spinor...]*)
SpinorChain[a__,sHead_["Spin","Upper","Angle",b_,c_]]SpinorChain[sHead_["Spin","Lower","Angle",b_,c_],e__]/;spinorHeadQ[sHead]:>Mass[b]SpinorChain[a,e],
SpinorChain[a__,sHead_["Spin","Upper","Square",b_,c_]]SpinorChain[sHead_["Spin","Lower","Square",b_,c_],e__]/;spinorHeadQ[sHead]:>-Mass[b]SpinorChain[a,e],SpinorChain[a__,sHead_["Spin","Upper","Angle",b_,c_]]SpinorChain[sHead_["Spin","Lower","Square",b_,c_],e__]/;spinorHeadQ[sHead]:>SpinorChain[a,momHead[sHead][b],e],
SpinorChain[a__,sHead_["Spin","Upper","Square",b_,c_]]SpinorChain[sHead_["Spin","Lower","Angle",b_,c_],e__]/;spinorHeadQ[sHead]:>-SpinorChain[a,momHead[sHead][b],e],

SpinorChain[a__,sHead_["Spin","Lower","Angle",b_,c_]]SpinorChain[sHead_["Spin","Upper","Angle",b_,c_],e__]/;spinorHeadQ[sHead]:>-Mass[b]SpinorChain[a,e],
SpinorChain[a__,sHead_["Spin","Lower","Square",b_,c_]]SpinorChain[sHead_["Spin","Upper","Square",b_,c_],e__]/;spinorHeadQ[sHead]:>Mass[b]SpinorChain[a,e],SpinorChain[a__,sHead_["Spin","Lower","Angle",b_,c_]]SpinorChain[sHead_["Spin","Upper","Square",b_,c_],e__]/;spinorHeadQ[sHead]:>-SpinorChain[a,momHead[sHead][b],e],
SpinorChain[a__,sHead_["Spin","Lower","Square",b_,c_]]SpinorChain[sHead_["Spin","Upper","Angle",b_,c_],e__]/;spinorHeadQ[sHead]:>SpinorChain[a,momHead[sHead][b],e],

SpinorChain[sHead_["Spin","Upper","Angle",b_,c_],a__]SpinorChain[sHead_["Spin","Lower","Angle",b_,c_],e__]/;spinorHeadQ[sHead]:>(-1)^Length[List[a]] Mass[b]SpinorChain[ReverseSequence[a],e],
SpinorChain[sHead_["Spin","Upper","Square",b_,c_],a__]SpinorChain[sHead_["Spin","Lower","Square",b_,c_],e__]/;spinorHeadQ[sHead]:>-(-1)^Length[List[a]]Mass[b]SpinorChain[ReverseSequence[a],e],SpinorChain[sHead_["Spin","Upper","Angle",b_,c_],a__]SpinorChain[sHead_["Spin","Lower","Square",b_,c_],e__]/;spinorHeadQ[sHead]:>(-1)^Length[List[a]] SpinorChain[ReverseSequence[a],momHead[sHead][b],e],
SpinorChain[sHead_["Spin","Upper","Square",b_,c_],a__]SpinorChain[sHead_["Spin","Lower","Angle",b_,c_],e__]/;spinorHeadQ[sHead]:>-(-1)^Length[List[a]]SpinorChain[ReverseSequence[a],momHead[sHead][b],e],

SpinorChain[a__,sHead_["Spin","Upper","Angle",b_,c_]]SpinorChain[e__,sHead_["Spin","Lower","Angle",b_,c_]]/;spinorHeadQ[sHead]:>(-1)^Length[List[e]] Mass[b]SpinorChain[a,ReverseSequence[e]],
SpinorChain[a__,sHead_["Spin","Upper","Square",b_,c_]]SpinorChain[e__,sHead_["Spin","Lower","Square",b_,c_]]/;spinorHeadQ[sHead]:>-(-1)^Length[List[e]]Mass[b]SpinorChain[a,ReverseSequence[e]],SpinorChain[a__,sHead_["Spin","Upper","Angle",b_,c_]]SpinorChain[e__,sHead_["Spin","Lower","Square",b_,c_]]/;spinorHeadQ[sHead]:>(-1)^Length[List[e]] SpinorChain[a,momHead[sHead][b],ReverseSequence[e]],
SpinorChain[a__,sHead_["Spin","Upper","Square",b_,c_]]SpinorChain[e__,sHead_["Spin","Lower","Angle",b_,c_]]/;spinorHeadQ[sHead]:>-(-1)^Length[List[e]]SpinorChain[a,momHead[sHead][b],ReverseSequence[e]],

(*Helicity Amplitude with index dummy supports*)
SpinorChain[a__,sHead_["Helicity","Angle",b_,J_]]SpinorChain[sHead_["Helicity","Square",b_,J_],c__]/;spinorHeadQ[sHead]:>SpinorChain[a,momHead[sHead][b],c],
SpinorChain[a__,sHead_["Helicity","Square",b_,J_]]SpinorChain[sHead_["Helicity","Angle",b_,J_],c__]/;spinorHeadQ[sHead]:>SpinorChain[a,momHead[sHead][b],c],
SpinorChain[a__,sHead_["Helicity","Angle",b_,J_]]SpinorChain[sHead_["Helicity","Square",b_,J_],c__]^2/;spinorHeadQ[sHead]:>SpinorChain[a,momHead[sHead][b],c]SpinorChain[sHead["Helicity","Square",b],c],
SpinorChain[a__,sHead_["Helicity","Square",b_,J_]]SpinorChain[sHead_["Helicity","Angle",b_,J_],c__]^2/;spinorHeadQ[sHead]:>SpinorChain[a,momHead[sHead][b],c]SpinorChain[sHead["Helicity","Angle",b],c],

SpinorChain[sHead_["Helicity","Square",pN_,J_],b__]SpinorChain[sHead_["Helicity","Angle",pN_,J_],d__]/;spinorHeadQ[sHead]:>
(-1)^Length[List[d]] SpinorChain[ReverseSequence[d],momHead[sHead][pN],b],
SpinorChain[b__,sHead_["Helicity","Square",pN_,J_]]SpinorChain[d__,sHead_["Helicity","Angle",pN_,J_]]/;spinorHeadQ[sHead]:>(-1)^Length[List[d]] SpinorChain[b,momHead[sHead][pN],ReverseSequence[d]],
SpinorChain[sHead_["Helicity","Angle",pN_,J_],b__]SpinorChain[sHead_["Helicity","Square",pN_,J_],d__]/;spinorHeadQ[sHead]:>(-1)^Length[List[d]] SpinorChain[ReverseSequence[d],momHead[sHead][pN],b],
SpinorChain[b__,sHead_["Helicity","Angle",pN_,J_]]SpinorChain[d__,sHead_["Helicity","Square",pN_,J_]]/;spinorHeadQ[sHead]:>(-1)^Length[List[d]] SpinorChain[b,momHead[sHead][pN],ReverseSequence[d]],

(*Old Helicity Amplitude Rules*)
(*...|b>[b|... and ...|b]<b|...*)
SpinorChain[a__,sHead_["Helicity","Angle",b_]]SpinorChain[sHead_["Helicity","Square",b_],c__]/;spinorHeadQ[sHead]:>SpinorChain[a,momHead[sHead][b],c],
SpinorChain[a__,sHead_["Helicity","Square",b_]]SpinorChain[sHead_["Helicity","Angle",b_],c__]/;spinorHeadQ[sHead]:>SpinorChain[a,momHead[sHead][b],c],

SpinorChain[a__,sHead_["Helicity","Angle",b_]]^d_ SpinorChain[sHead_["Helicity","Square",b_],c__]^d_/;spinorHeadQ[sHead]:>SpinorChain[a,momHead[sHead][b],c]^d,
SpinorChain[a__,sHead_["Helicity","Square",b_]]^d_ SpinorChain[sHead_["Helicity","Angle",b_],c__]^d_/;spinorHeadQ[sHead]:>SpinorChain[a,momHead[sHead][b],c]^d,

SpinorChain[a__,sHead_["Helicity","Angle",b_]]SpinorChain[sHead_["Helicity","Square",b_],c__]^n_/;(n>0&&spinorHeadQ[sHead]):>SpinorChain[a,momHead[sHead][b],c]SpinorChain[sHead["Helicity","Square",b],c]^(n-1),
SpinorChain[a__,sHead_["Helicity","Angle",b_]]^n_ SpinorChain[sHead_["Helicity","Square",b_],c__]/;(n>0&&spinorHeadQ[sHead]):>SpinorChain[a,sHead["Helicity","Angle",b]]^(n-1)SpinorChain[a,momHead[sHead][b],c],
SpinorChain[a__,sHead_["Helicity","Square",b_]]SpinorChain[sHead_["Helicity","Angle",b_],c__]^n_/;(n>0&&spinorHeadQ[sHead]):>SpinorChain[a,momHead[sHead][b],c]SpinorChain[sHead["Helicity","Angle",b],c]^(n-1),
SpinorChain[a__,sHead_["Helicity","Square",b_]]^n_ SpinorChain[sHead_["Helicity","Angle",b_],c__]/;(n>0&&spinorHeadQ[sHead]):>SpinorChain[a,sHead["Helicity","Square",b]]^(n-1)SpinorChain[a,momHead[sHead][b],c],

(*...|b>...|b] and ...|b]...|b>*)
SpinorChain[sHead_["Helicity","Square",pN_],b__]SpinorChain[sHead_["Helicity","Angle",pN_],d__]/;spinorHeadQ[sHead]:>
(-1)^Length[List[d]] SpinorChain[ReverseSequence[d],momHead[sHead][pN],b],
SpinorChain[b__,sHead_["Helicity","Square",pN_]]SpinorChain[d__,sHead_["Helicity","Angle",pN_]]/;spinorHeadQ[sHead]:>(-1)^Length[List[d]] SpinorChain[b,momHead[sHead][pN],ReverseSequence[d]],
SpinorChain[sHead_["Helicity","Angle",pN_],b__]SpinorChain[sHead_["Helicity","Square",pN_],d__]/;spinorHeadQ[sHead]:>(-1)^Length[List[d]] SpinorChain[ReverseSequence[d],momHead[sHead][pN],b],
SpinorChain[b__,sHead_["Helicity","Angle",pN_]]SpinorChain[d__,sHead_["Helicity","Square",pN_]]/;spinorHeadQ[sHead]:>(-1)^Length[List[d]] SpinorChain[b,momHead[sHead][pN],ReverseSequence[d]],

SpinorChain[sHead_["Helicity","Square",pN_],b__]^pow_ SpinorChain[sHead_["Helicity","Angle",pN_],d__]^pow_/;spinorHeadQ[sHead]:>
(-1)^(pow Length[List[d]]) SpinorChain[ReverseSequence[d],momHead[sHead][pN],b]^pow,
SpinorChain[b__,sHead_["Helicity","Square",pN_]]^pow_ SpinorChain[d__,sHead_["Helicity","Angle",pN_]]^pow_/;spinorHeadQ[sHead]:>(-1)^(pow Length[List[d]]) SpinorChain[b,momHead[sHead][pN],ReverseSequence[d]]^pow,
SpinorChain[sHead_["Helicity","Angle",pN_],b__]^pow_ SpinorChain[sHead_["Helicity","Square",pN_],d__]^pow_/;spinorHeadQ[sHead]:>(-1)^(pow Length[List[d]]) SpinorChain[ReverseSequence[d],momHead[sHead][pN],b]^pow,
SpinorChain[b__,sHead_["Helicity","Angle",pN_]]^pow_ SpinorChain[d__,sHead_["Helicity","Square",pN_]]^pow_/;spinorHeadQ[sHead]:>(-1)^(pow Length[List[d]]) SpinorChain[b,momHead[sHead][pN],ReverseSequence[d]]^pow,

SpinorChain[sHead_["Helicity","Square",pN_],b__]^pow_ SpinorChain[sHead_["Helicity","Angle",pN_],d__]/;(pow>0&&spinorHeadQ[sHead]):>
SpinorChain[sHead["Helicity","Square",pN],b]^(pow-1)(-1)^Length[List[d]] SpinorChain[ReverseSequence[d],momHead[sHead][pN],b],
SpinorChain[sHead_["Helicity","Square",pN_],b__]SpinorChain[sHead_["Helicity","Angle",pN_],d__]^pow_/;(pow>0&&spinorHeadQ[sHead]):>
SpinorChain[sHead["Helicity","Angle",pN],d]^(pow-1)(-1)^Length[List[d]] SpinorChain[ReverseSequence[d],momHead[sHead][pN],b],

SpinorChain[b__,sHead_["Helicity","Square",pN_]]^pow_ SpinorChain[d__,sHead_["Helicity","Angle",pN_]]/;(pow>0&&spinorHeadQ[sHead]):>SpinorChain[b,sHead["Helicity","Square",pN]]^(pow-1)(-1)^Length[List[d]] SpinorChain[b,momHead[sHead][pN],ReverseSequence[d]],
SpinorChain[b__,sHead_["Helicity","Square",pN_]]SpinorChain[d__,sHead_["Helicity","Angle",pN_]]^pow_/;(pow>0&&spinorHeadQ[sHead]):>SpinorChain[d,sHead["Helicity","Angle",pN]]^(pow-1)(-1)^Length[List[d]] SpinorChain[b,momHead[sHead][pN],ReverseSequence[d]],

SpinorChain[sHead_["Helicity","Angle",pN_],b__]^pow_ SpinorChain[sHead_["Helicity","Square",pN_],d__]/;(pow>0&&spinorHeadQ[sHead]):>SpinorChain[sHead["Helicity","Angle",pN],b]^(pow-1)(-1)^Length[List[d]] SpinorChain[ReverseSequence[d],momHead[sHead][pN],b],
SpinorChain[sHead_["Helicity","Angle",pN_],b__]SpinorChain[sHead_["Helicity","Square",pN_],d__]^pow_/;(pow>0&&spinorHeadQ[sHead]):>SpinorChain[sHead["Helicity","Square",pN],d]^(pow-1)(-1)^Length[List[d]] SpinorChain[ReverseSequence[d],momHead[sHead][pN],b],

SpinorChain[b__,sHead_["Helicity","Angle",pN_]]^pow_ SpinorChain[d__,sHead_["Helicity","Square",pN_]]/;(pow>0&&spinorHeadQ[sHead]):>SpinorChain[b,sHead["Helicity","Angle",pN]]^(pow-1)(-1)^Length[List[d]] SpinorChain[b,momHead[sHead][pN],ReverseSequence[d]],
SpinorChain[b__,sHead_["Helicity","Angle",pN_]]SpinorChain[d__,sHead_["Helicity","Square",pN_]]^pow_/;(pow>0&&spinorHeadQ[sHead]):>SpinorChain[d,sHead["Helicity","Square",pN]]^(pow-1)(-1)^Length[List[d]] SpinorChain[b,momHead[sHead][pN],ReverseSequence[d]],

(*SpinorChain[Spinor...Spinor]\[Rule]SpinorTr[...]*)
SpinorChain[sHead_["Spin","Upper","Square",pN_,J_],a___,sHead_["Spin","Lower","Angle",pN_,J_]]/;spinorHeadQ[sHead]:>-SpinorTrace[momHead[sHead][pN],a],
SpinorChain[sHead_["Spin","Lower","Square",pN_,J_],a___,sHead_["Spin","Upper","Angle",pN_,J_]]/;spinorHeadQ[sHead]:>SpinorTrace[momHead[sHead][pN],a],
SpinorChain[sHead_["Spin","Upper","Angle",pN_,J_],a___,sHead_["Spin","Lower","Square",pN_,J_]]/;spinorHeadQ[sHead]:>SpinorTrace[a,momHead[sHead][pN]],
SpinorChain[sHead_["Spin","Lower","Angle",pN_,J_],a___,sHead_["Spin","Upper","Square",pN_,J_]]/;spinorHeadQ[sHead]:>-SpinorTrace[a,momHead[sHead][pN]],

SpinorChain[sHead_["Spin","Upper","Square",pN_,J_],a_,b__,sHead_["Spin","Lower","Square",pN_,J_]]/;spinorHeadQ[sHead]:>Mass[pN]SpinorTrace[b,a],
SpinorChain[sHead_["Spin","Lower","Square",pN_,J_],a_,b__,sHead_["Spin","Upper","Square",pN_,J_]]/;spinorHeadQ[sHead]:>-Mass[pN]SpinorTrace[b,a],
SpinorChain[sHead_["Spin","Upper","Square",pN_,J_],a_,sHead_["Spin","Lower","Square",pN_,J_]]/;spinorHeadQ[sHead]:>Mass[pN]SpinorTrace[a],
SpinorChain[sHead_["Spin","Lower","Square",pN_,J_],a_,sHead_["Spin","Upper","Square",pN_,J_]]/;spinorHeadQ[sHead]:>-Mass[pN]SpinorTrace[a],
SpinorChain[sHead_["Spin","Upper","Square",pN_,J_],sHead_["Spin","Lower","Square",pN_,J_]]/;spinorHeadQ[sHead]:>Mass[pN]SpinorTrace[],
SpinorChain[sHead_["Spin","Lower","Square",pN_,J_],sHead_["Spin","Upper","Square",pN_,J_]]/;spinorHeadQ[sHead]:>-Mass[pN]SpinorTrace[],
SpinorChain[sHead_["Spin","Upper","Angle",pN_,J_],a___,sHead_["Spin","Lower","Angle",pN_,J_]]/;spinorHeadQ[sHead]:>-Mass[pN]SpinorTrace[a],
SpinorChain[sHead_["Spin","Lower","Angle",pN_,J_],a___,sHead_["Spin","Upper","Angle",pN_,J_]]/;spinorHeadQ[sHead]:>Mass[pN]SpinorTrace[a],

SpinorChain[sHead_["Helicity","Square",pN_],a___,sHead_["Helicity","Angle",pN_]]/;spinorHeadQ[sHead]:>SpinorTrace[momHead[sHead][pN],a],
SpinorChain[sHead_["Helicity","Angle",pN_],a___,sHead_["Helicity","Square",pN_]]/;spinorHeadQ[sHead]:>SpinorTrace[a,momHead[sHead][pN]],

(*SpinorEps*)
SpinorChain[ls___,sHead_["Spin","Lower",as_,pN_,J_],rs___]SpinorEps["Upper",J_,K_]/;spinorHeadQ[sHead]:>SpinorChain[ls,sHead["Spin","Upper",as,pN,K],rs],
SpinorChain[ls___,sHead_["Spin","Lower",as_,pN_,J_],rs___]SpinorEps["Upper",K_,J_]/;spinorHeadQ[sHead]:>-SpinorChain[ls,sHead["Spin","Upper",as,pN,K],rs],
SpinorChain[ls___,sHead_["Spin","Upper",as_,pN_,J_],rs___]SpinorEps["Lower",J_,K_]/;spinorHeadQ[sHead]:>SpinorChain[ls,sHead["Spin","Lower",as,pN,K],rs],
SpinorChain[ls___,sHead_["Spin","Upper",as_,pN_,J_],rs___]SpinorEps["Lower",K_,J_]/;spinorHeadQ[sHead]:>-SpinorChain[ls,sHead["Spin","Lower",as,pN,K],rs]
};

While[rscResult=!=rscOldResult,
(*Print[rscAttempts];*)
rscOldResult=rscResult;
rscResult=If[TrueQ[rscExpand], Expand[rscResult//.rscReplacementRules], rscResult//.rscReplacementRules];
rscAttempts=rscAttempts+1;
If[rscAttempts>10,Print["Giving up after 10 attempts to reduce!"];Break[]];
];
rscResult
];

End[];


(* ::Subsubsection::Closed:: *)
(*ReduceSpinorProducts*)


(* ::Input::Initialization:: *)
ReduceSpinorProducts::usage = "ReduceSpinorProducts[expr] applies ordinary spinor algebra, multilinearity, chain reversal, Clifford contractions and external-particle mass identities. Compact rational amplitudes retain a competing factored form, with bounded prospective expansion and inverse-protected factor extraction. Bare contractions retain their canonical rules and off-cut remainders. It does not infer momentum conservation or impose a composite internal momentum's mass shell. PropDen arguments remain unchanged.";
SpinorChainReduced; SpinorTraceReduced;

Begin["ConstructiveDiagrams`Private`"];

rspSpinorType[sHead_[_, type:("Angle"|"Square"), _, ___]] /; spinorHeadQ[sHead] := type;
rspSpinorType[sHead_[_, _, type:("Angle"|"Square"), _, ___]] /; spinorHeadQ[sHead] := type;
rspSpinorType[_] := None;
rspSpinorLeg[sHead_[_, "Angle"|"Square", leg_, ___]] /; spinorHeadQ[sHead] := leg;
rspSpinorLeg[sHead_[_, _, "Angle"|"Square", leg_, ___]] /; spinorHeadQ[sHead] := leg;
rspFlipSpinor[s_] := ReplacePart[s, If[MemberQ[{"Angle", "Square"}, s[[2]]], 2, 3] ->
  If[rspSpinorType[s] === "Angle", "Square", "Angle"]];
rspMomentumLabels[Multiparticle[items___]] := Flatten[rspMomentumLabels /@ {items}, 1];
rspMomentumLabels[label_] := {label};
rspScalarQ[value_] := FreeQ[value, _Spinor|_SpinorHat|_SpinorChain|_SpinorChainReduced|
  _SpinorTrace|_SpinorTraceReduced|_Mom|_MomHat|_MomQ|_SpinorZ|_SpinorEps|
  _sigma|_colorT|_colorTrace|_Dot|_NonCommutativeMultiply];

(* Normalize only typed momentum slots, before a composite can meet a mass rule. *)
rspLinearMomentumTerms[arg_] := Module[{expanded = Expand[arg], factors, momenta, coefficient, terms = {}},
  If[expanded === 0, Return[{}]];
  Do[
    factors = If[Head[term] === Times, List @@ term, {term}];
    momenta = Select[factors, MatchQ[#, _Mom|_MomHat] &];
    If[Length[momenta] =!= 1, Return[$Failed]];
    coefficient = term/First[momenta];
    If[!rspScalarQ[coefficient], Return[$Failed]];
    terms = Join[terms, ({coefficient, Head[First[momenta]][#]} & /@
      rspMomentumLabels[First[momenta][[1]]])],
    {term, If[Head[expanded] === Plus, List @@ expanded, {expanded}]}];
  terms
];
rspLinearChain[chain_SpinorChain] := Module[{parts = List @@ chain, slots, choices},
  If[Length[parts] < 2 || rspSpinorType[First[parts]] === None ||
      rspSpinorType[Last[parts]] === None, Return[chain]];
  slots = DeleteCases[Drop[Rest[parts], -1], 1];
  If[SameQ[rspSpinorType[First[parts]], rspSpinorType[Last[parts]]] =!= EvenQ[Length[slots]], Return[chain]];
  choices = rspLinearMomentumTerms /@ slots;
  If[!FreeQ[choices, $Failed], Return[chain]];
  Total[(Times @@ (First /@ #)) (SpinorChain @@
    Join[{First[parts]}, Last /@ #, {Last[parts]}]) & /@ Tuples[choices]]
];
rspLinearTrace[trace_SpinorTrace] := Module[{choices},
  If[OddQ[Length[trace]], Return[trace]];
  choices = rspLinearMomentumTerms /@ List @@ trace;
  If[!FreeQ[choices, $Failed], Return[trace]];
  Total[(Times @@ (First /@ #)) (SpinorTrace @@ (Last /@ #)) & /@ Tuples[choices]]
];
rspReverseChainQ[left_, momenta_List, right_] := Module[{lt = rspSpinorType[left], rt = rspSpinorType[right]},
  If[lt === None || rt === None || !AllTrue[momenta, MatchQ[#, _Mom|_MomHat] &] ||
      SameQ[lt, rt] =!= EvenQ[Length[momenta]], Return[False]];
  If[OddQ[Length[momenta]], lt === "Angle" && rt === "Square",
    TrueQ[rspSpinorLeg[right] < rspSpinorLeg[left]]]
];
rspCommonScalarContent[expression_Plus] := Module[{atoms},
  atoms = DeleteDuplicates[Cases[expression, _SpinorChain|_SpinorTrace, Infinity]];
  If[atoms === {} || !PolynomialQ[expression, atoms], expression,
    FactorTerms[expression, atoms]]
];
rspCommonScalarContent[expression_] := expression;

(* A massless diagonal sandwich pair contracts in one direction: four chain
   momentum insertions become two, with its scalar-product remainder retained. *)
rspDiagonalSandwichData[chain_] := Module[{parts},
  If[Head[chain] =!= SpinorChain || Length[chain] =!= 4, Return[Missing["NotDiagonalSandwich"]]];
  parts = List @@ chain;
  If[First[parts] =!= Last[parts] || Head[First[parts]] =!= Spinor ||
      !MemberQ[{"Angle", "Square"}, rspSpinorType[First[parts]]] ||
      !AllTrue[parts[[{2, 3}]], MatchQ[#, Mom[_Integer]] &],
    Return[Missing["NotDiagonalSandwich"]]];
  {First[parts], parts[[2]], parts[[3]]}
];

(* A Helicity endpoint is an explicit massless state, not a Xi identifier. *)
rspHelicityScalarChain[chain_] := Module[{parts, left, right, leg},
  If[Head[chain] =!= SpinorChain || Length[chain] =!= 3 ||
      !MatchQ[chain[[2]], Mom[_Integer]], Return[chain]];
  parts = List @@ chain; {left, right} = parts[[{1, 3}]];
  If[!MatchQ[left, Spinor["Helicity", "Angle" | "Square", _Integer]] ||
      right =!= rspFlipSpinor[left], Return[chain]];
  leg = rspSpinorLeg[left];
  If[parts[[2]] === Mom[leg], 0, 2 MomProd[leg, parts[[2, 1]]]]
];

(* The caller supplies a witnessed or declared massless leg. Factor its first
   insertion without momentum conservation or any other mass-shell choice. *)
rspSplitMasslessMomentum[chain_, leg_Integer] := Module[
  {parts, slots, position, kind, opposite, supported},
  supported[spinor_] := MatchQ[spinor,
    Spinor["Spin" | "Helicity" | "Xi", "Angle" | "Square", _] |
    Spinor["Spin", "Upper" | "Lower", "Angle" | "Square", _, _]];
  If[Head[chain] =!= SpinorChain || Length[chain] < 3,
    Return[Missing["NoMasslessMomentumSplit"]]];
  parts = List @@ chain; slots = Rest[Most[parts]];
  If[!supported[First[parts]] || !supported[Last[parts]] ||
      !AllTrue[slots, MatchQ[#, Mom[_Integer]] &] ||
      SameQ[rspSpinorType[First[parts]], rspSpinorType[Last[parts]]] =!= EvenQ[Length[slots]],
    Return[Missing["NoMasslessMomentumSplit"]]];
  position = FirstPosition[slots, Mom[leg], Missing["NoMasslessMomentumSplit"]];
  If[MissingQ[position], Return[position]];
  position = First[position]; kind = rspSpinorType[First[parts]];
  If[OddQ[position - 1], kind = If[kind === "Angle", "Square", "Angle"]];
  opposite = If[kind === "Angle", "Square", "Angle"];
  (SpinorChain @@ Join[{First[parts]}, Take[slots, position - 1],
      {Spinor["Helicity", kind, leg]}]) *
    (SpinorChain @@ Join[{Spinor["Helicity", opposite, leg]}, Drop[slots, position],
      {Last[parts]}])
];

rspHelicityMomentumPair[witness_, chain_] := Module[
  {ends, helicities, replacement, tag = Unique["helicityMomentumPair"]},
  If[Head[witness] =!= SpinorChain || Length[witness] =!= 2,
    Return[Missing["NoHelicityMomentumPair"]]];
  ends = List @@ witness;
  If[!AllTrue[ends, MatchQ[#,
        Spinor["Spin" | "Helicity" | "Xi", "Angle" | "Square", _] |
        Spinor["Spin", "Upper" | "Lower", "Angle" | "Square", _, _]] &] ||
      rspSpinorType[First[ends]] =!= rspSpinorType[Last[ends]],
    Return[Missing["NoHelicityMomentumPair"]]];
  helicities = DeleteDuplicates[Cases[ends,
    Spinor["Helicity", "Angle" | "Square", k_Integer] :> k]];
  Catch[Do[
    replacement = rspSplitMasslessMomentum[chain, leg];
    If[!MissingQ[replacement], Throw[witness replacement, tag]],
    {leg, helicities}];
    Missing["NoHelicityMomentumPair"], tag]
];

(* Identify <a k> or [a k] without losing an explicit spin index on a. *)
rspMasslessBracketPartner[bracket_, helicity_] := Module[{ends, target, endpoint, sign},
  If[Head[bracket] =!= SpinorChain || Length[bracket] =!= 2 ||
      !MatchQ[helicity, Spinor["Helicity", "Angle" | "Square", _Integer]],
    Return[Missing["NoBracketPartner"]]];
  ends = List @@ bracket; target = rspFlipSpinor[helicity];
  Which[Last[ends] === target, endpoint = First[ends]; sign = 1,
    First[ends] === target, endpoint = Last[ends]; sign = -1,
    True, Return[Missing["NoBracketPartner"]]];
  If[!MatchQ[endpoint,
      Spinor["Spin" | "Helicity", "Angle" | "Square", _Integer] |
      Spinor["Spin", "Upper" | "Lower", "Angle" | "Square", _Integer, _]] ||
      rspSpinorType[endpoint] =!= rspSpinorType[target] ||
      rspSpinorLeg[endpoint] === rspSpinorLeg[helicity],
    Return[Missing["NoBracketPartner"]]];
  {endpoint, sign}
];

(* Schouten plus the external endpoint equation removes the sole momentum
   insertion. The reference spinor is arbitrary; its Xi label is not a leg. *)
rspBracketMomentumPair[bracket_, chain_] := Module[
  {parts, helicity, reference, partner, endpoint, leg, mass,
   tag = Unique["bracketMomentumPair"]},
  If[Head[chain] =!= SpinorChain || Length[chain] =!= 3 ||
      !MatchQ[chain[[2]], Mom[_Integer]], Return[Missing["NoBracketMomentumPair"]]];
  parts = List @@ chain;
  Catch[Do[
    {helicity, reference} = ends;
    If[!MatchQ[reference,
        Spinor["Spin" | "Helicity" | "Xi", "Angle" | "Square", _] |
        Spinor["Spin", "Upper" | "Lower", "Angle" | "Square", _, _]], Continue[]];
    partner = rspMasslessBracketPartner[bracket, helicity];
    If[MissingQ[partner], Continue[]];
    endpoint = First[partner]; leg = rspSpinorLeg[endpoint];
    If[chain[[2]] =!= Mom[leg] ||
        rspSpinorType[reference] =!= rspSpinorType[endpoint], Continue[]];
    mass = If[endpoint[[1]] === "Helicity", 0, Mass[leg]];
    Throw[Last[partner] (mass SpinorChain[reference, rspFlipSpinor[helicity]] *
        SpinorChain[rspFlipSpinor[endpoint], helicity] +
      2 MomProd[leg, rspSpinorLeg[helicity]] SpinorChain[endpoint, reference]), tag],
    {ends, {{First[parts], Last[parts]}, {Last[parts], First[parts]}}}];
    Missing["NoBracketMomentumPair"], tag]
];

(* The same contraction after a reference row is represented as [k|P.
   Keep its scalar-product remainder until the caller authorizes a cut. *)
rspBracketDiagonalPair[bracket_, chain_] := Module[
  {data = rspDiagonalSandwichData[chain], partner, endpoint, helicity,
   leg, momentum, mass, sign},
  If[MissingQ[data], Return[Missing["NoBracketDiagonalPair"]]];
  helicity = First[data]; partner = rspMasslessBracketPartner[bracket, helicity];
  If[MissingQ[partner], Return[Missing["NoBracketDiagonalPair"]]];
  endpoint = First[partner]; leg = rspSpinorLeg[endpoint]; sign = Last[partner];
  Which[data[[3]] === Mom[leg], momentum = data[[2]],
    data[[2]] === Mom[leg], momentum = data[[3]]; sign = -sign,
    True, Return[Missing["NoBracketDiagonalPair"]]];
  If[MemberQ[{Mom[leg], Mom[rspSpinorLeg[helicity]]}, momentum],
    Return[Missing["NoBracketDiagonalPair"]]];
  mass = If[endpoint[[1]] === "Helicity", 0, Mass[leg]];
  sign (2 mass MomProd[momentum[[1]], rspSpinorLeg[helicity]] *
      SpinorChain[rspFlipSpinor[endpoint], helicity] -
    2 MomProd[leg, rspSpinorLeg[helicity]] SpinorChain[endpoint, momentum, helicity])
];

(* Expand only the declared linear vector arguments. In particular a composite
   vector square is a sum of external products, not an internal mass shell. *)
rspOrdinaryMomentumDot[left_, right_] := Module[{a, b},
  a = rspLinearMomentumTerms[left]; b = rspLinearMomentumTerms[right];
  If[a === $Failed || b === $Failed ||
      !AllTrue[Join[a, b], MatchQ[Last[#], Mom[_Integer]] &],
    Return[Missing["UnsupportedMomentumProduct"]]];
  Total[Flatten[Table[first[[1]] second[[1]] MomProd[first[[2, 1]], second[[2, 1]]],
    {first, a}, {second, b}]]]
];

(* Adding its own null momentum at a physical helicity endpoint annihilates
   that chain. A Xi with the same integer identifier grants no such license. *)
rspEndpointEquivalentMomentumQ[from_, to_, endpoints_List] := Module[
  {a, b, massless, differences},
  If[from === to, Return[True]];
  a = rspLinearMomentumTerms[from]; b = rspLinearMomentumTerms[to];
  If[a === $Failed || b === $Failed ||
      !AllTrue[Join[a, b], MatchQ[Last[#], Mom[_Integer]] &], Return[False]];
  massless = DeleteDuplicates[Cases[endpoints,
    Spinor["Helicity", "Angle" | "Square", leg_Integer] :> leg]];
  differences = GatherBy[Join[a, ({-First[#], Last[#]} & /@ b)], Last];
  AllTrue[differences, TrueQ[Expand[Total[First /@ #]] === 0] ||
    MemberQ[massless, Last[First[#]][[1]]] &]
];

(* Recognize R-alpha Q=lambda p_b using ordinary vector coefficients only. *)
rspMomentumAlongLeg[from_, base_, alpha_, leg_Integer] := Module[
  {a, b, differences, along},
  a = rspLinearMomentumTerms[from]; b = rspLinearMomentumTerms[base];
  If[a === $Failed || b === $Failed ||
      !AllTrue[Join[a, b], MatchQ[Last[#], Mom[_Integer]] &],
    Return[Missing["NotAlongLeg"]]];
  differences = GatherBy[Join[a, ({-alpha First[#], Last[#]} & /@ b)], Last];
  If[!AllTrue[differences, Last[First[#]] === Mom[leg] ||
      TrueQ[Expand[Total[First /@ #]] === 0] &], Return[Missing["NotAlongLeg"]]];
  along = Select[differences, Last[First[#]] === Mom[leg] &];
  Expand[Total[First /@ Flatten[along, 1]]]
];

(* Contract a common vector before an adjacent Q+lambda p_b slot is expanded.
   The trailing typed chain is a spectator. This is Clifford algebra followed
   by the determinant identity below, with every scalar remainder retained. *)
rspSharedAdjacentMomentumPair[left_, right_] := Module[
  {supported, endpoints, parts, slots, orientations, a, b, c, d, momentum,
   tail, lambda, square, dot, nullMassRules, branch, sign,
   tag = Unique["sharedAdjacentMomentumPair"]},
  supported[spinor_] := MatchQ[spinor,
    Spinor["Spin" | "Helicity" | "Xi", "Angle" | "Square", _] |
    Spinor["Spin", "Upper" | "Lower", "Angle" | "Square", _, _]];
  If[Head[left] =!= SpinorChain || Length[left] =!= 3 ||
      Head[right] =!= SpinorChain || Length[right] < 4,
    Return[Missing["NoSharedAdjacentMomentumPair"]]];
  endpoints = {left[[1]], left[[3]]}; parts = List @@ right;
  If[!AllTrue[Join[endpoints, parts[[{1, -1}]]], supported],
    Return[Missing["NoSharedAdjacentMomentumPair"]]];
  slots = Rest[Most[parts]];
  If[SameQ[rspSpinorType[First[parts]], rspSpinorType[Last[parts]]] =!= EvenQ[Length[slots]] ||
      AnyTrue[slots, rspLinearMomentumTerms[#] === $Failed &],
    Return[Missing["NoSharedAdjacentMomentumPair"]]];
  nullMassRules = (Mass[#] -> 0 &) /@ DeleteDuplicates[Cases[
    Join[endpoints, parts[[{1, -1}]]],
    Spinor["Helicity", "Angle" | "Square", leg_Integer] :> leg]];
  orientations = {{1, parts}, {(-1)^(Length[slots] + 1), Reverse[parts]}};
  Catch[Do[
    {sign, parts} = orientation; slots = Rest[Most[parts]];
    {c, d} = parts[[{1, -1}]]; momentum = First[slots]; tail = Drop[slots, 2];
    b = endpoints[[i]]; a = endpoints[[3 - i]];
    If[!MatchQ[b, Spinor["Helicity", "Angle" | "Square", _Integer]] ||
        rspSpinorType[a] === rspSpinorType[b] ||
        rspSpinorType[c] =!= rspSpinorType[b] ||
        !rspEndpointEquivalentMomentumQ[left[[2]], momentum, endpoints], Continue[]];
    lambda = rspMomentumAlongLeg[slots[[2]], momentum, alpha, rspSpinorLeg[b]];
    If[MissingQ[lambda], Continue[]];
    square = rspOrdinaryMomentumDot[momentum, momentum];
    dot = rspOrdinaryMomentumDot[Mom[rspSpinorLeg[b]], momentum];
    If[MissingQ[square] || MissingQ[dot], Continue[]];
    {square, dot} = {square, dot} /. nullMassRules;
    branch = SpinorChain @@ Join[{b}, tail, {d}];
    Throw[sign (square (alpha left (SpinorChain @@ Join[{c}, tail, {d}]) +
        lambda SpinorChain[a, rspFlipSpinor[b]] SpinorChain[c, b] branch) +
      2 lambda dot SpinorChain[c, momentum, a] branch), tag],
    {orientation, orientations}, {i, 2}, {alpha, {1, -1}}];
    Missing["NoSharedAdjacentMomentumPair"], tag]
];

(* A two-dimensional determinant contracts a common vector in opposite
   chiralities. Only the shared Helicity endpoint is assumed massless; keep the
   complete vector-square and scalar-product remainder for the caller. *)
rspSharedMomentumPair[left_, right_] := Module[
  {supported, aEnds, bEnds, a, b, c, momentum, square, dot, nullMassRules,
   tag = Unique["sharedMomentumPair"]},
  supported[spinor_] := MatchQ[spinor,
    Spinor["Spin" | "Helicity" | "Xi", "Angle" | "Square", _] |
    Spinor["Spin", "Upper" | "Lower", "Angle" | "Square", _, _]];
  If[Head[left] === SpinorChain && Length[left] === 3 &&
      Head[right] === SpinorChain && Length[right] >= 4,
    Return[rspSharedAdjacentMomentumPair[left, right]]];
  If[Head[left] =!= SpinorChain || Head[right] =!= SpinorChain ||
      Length[left] =!= 3 || Length[right] =!= 3,
    Return[Missing["NoSharedMomentumPair"]]];
  aEnds = {left[[1]], left[[3]]}; bEnds = {right[[1]], right[[3]]};
  If[!AllTrue[Join[aEnds, bEnds], supported], Return[Missing["NoSharedMomentumPair"]]];
  nullMassRules = (Mass[#] -> 0 &) /@ DeleteDuplicates[Cases[Join[aEnds, bEnds],
    Spinor["Helicity", "Angle" | "Square", leg_Integer] :> leg]];
  momentum = SelectFirst[DeleteDuplicates[{left[[2]], right[[2]]}],
    rspEndpointEquivalentMomentumQ[left[[2]], #, aEnds] &&
      rspEndpointEquivalentMomentumQ[right[[2]], #, bEnds] &,
    Missing["NoCommonMomentum"]];
  If[MissingQ[momentum], Return[Missing["NoSharedMomentumPair"]]];
  Catch[Do[
    b = aEnds[[i]]; a = aEnds[[3 - i]];
    If[!MatchQ[b, Spinor["Helicity", "Angle" | "Square", _Integer]] ||
        rspSpinorType[a] === rspSpinorType[b], Continue[]];
    c = bEnds[[3 - j]];
    If[bEnds[[j]] =!= rspFlipSpinor[b] ||
        rspSpinorType[c] =!= rspSpinorType[b], Continue[]];
    square = rspOrdinaryMomentumDot[momentum, momentum];
    dot = rspOrdinaryMomentumDot[Mom[rspSpinorLeg[b]], momentum];
    If[MissingQ[square] || MissingQ[dot], Continue[]];
    square = square /. nullMassRules;
    dot = dot /. nullMassRules;
    Throw[square SpinorChain[a, rspFlipSpinor[b]] SpinorChain[c, b] +
      2 dot SpinorChain[c, momentum, a], tag],
    {i, 2}, {j, 2}];
    Missing["NoSharedMomentumPair"], tag]
];

rspSandwichPair[left_, right_] := Module[
  {a = rspDiagonalSandwichData[left], b = rspDiagonalSandwichData[right],
   endpoint, other, leg, bracket, sameType, p, q, sign, gp, gq,
   replacement, tag = Unique["sandwichPair"]},
  replacement = rspSharedMomentumPair[left, right];
  If[!MissingQ[replacement], Return[replacement]];
  replacement = rspBracketMomentumPair[left, right];
  If[!MissingQ[replacement], Return[replacement]];
  replacement = rspBracketDiagonalPair[left, right];
  If[!MissingQ[replacement], Return[replacement]];
  If[MissingQ[a] || MissingQ[b], Return[Missing["NoSandwichPair"]]];
  endpoint = First[a]; other = First[b]; leg = rspSpinorLeg[endpoint];
  If[endpoint[[1]] =!= "Helicity" || !IntegerQ[leg] || endpoint === other,
    Return[Missing["NoSandwichPair"]]];
  sameType = rspSpinorType[endpoint] === rspSpinorType[other];
  If[sameType, bracket = SpinorChain[endpoint, other]];
  Catch[Do[
    {p, q} = first[[2]];
    If[MemberQ[{p, q}, Mom[leg]] || p === q, Continue[]];
    If[second[[2]] === {p, Mom[leg]},
      sign = first[[1]] second[[1]];
      If[sameType,
        Throw[sign (2 MomProd[p[[1]], p[[1]]] MomProd[leg, q[[1]]] bracket^2 +
          2 MomProd[leg, p[[1]]] bracket SpinorChain[other, p, q, endpoint]), tag],
        gp = SpinorChain[other, p, endpoint]; gq = SpinorChain[other, q, endpoint];
        Throw[sign (2 MomProd[leg, q[[1]]] gp^2 -
          2 MomProd[leg, p[[1]]] gp gq), tag]]],
    {first, {{1, a[[{2, 3}]]}, {-1, Reverse[a[[{2, 3}]]]}}},
    {second, {{1, b[[{2, 3}]]}, {-1, Reverse[b[[{2, 3}]]]}}}];
    Missing["NoSandwichPair"], tag]
];

rspSandwichExpansionCount[expression_] := Module[{count, limit = 4096},
  count[value_Plus] := Min[limit + 1, Total[count /@ List @@ value]];
  count[value_Times] := Fold[Min[limit + 1, #1 #2] &, 1, count /@ List @@ value];
  count[Power[base_, exponent_Integer?Positive]] := With[{size = count[base]},
    Which[size <= 1, size, size > limit || exponent >= IntegerLength[limit, size], limit + 1,
      True, Min[limit + 1, size^exponent]]];
  count[_] := 1;
  count[expression]
];

DownValues[rspSandwichProducts] = {};
rspSandwichProducts[expression_, sharedOnly_ : False] := Module[
  {steps = 0, limit, tag = Unique["sandwichContractions"], reduce, result},
  (* The existing reducer may accept larger expressions. Only this additional
     contraction pass is skipped when it would create too many branches. *)
  If[rspSandwichExpansionCount[expression] > 4096, Return[expression]];
  limit = Min[4096, Max[64, Count[expression, _SpinorChain, Infinity]]];
  reduce[value_Plus] := Map[reduce, value];
  reduce[value_List] := Map[reduce, value];
  reduce[Power[base_, exponent_]] := Power[reduce[base], exponent];
  reduce[value_Times] := Module[{factors = reduce /@ List @@ value, chains, positions, replacement, trial,
      nodeTag = Unique["sandwichProduct"]},
    chains = Map[Which[Head[#] === SpinorChain, {#, 1},
        MatchQ[#, Power[_SpinorChain, _Integer?Positive]], {#[[1]], #[[2]]},
        True, Missing["NotPositiveChain"]] &, factors];
    positions = Select[Range[Length[chains]], ListQ[chains[[#]]] &];
    (* Finish stronger pair contractions before splitting a witnessed momentum. *)
    Catch[Do[Do[
      replacement = If[TrueQ[sharedOnly], rspSharedMomentumPair,
        If[split, rspHelicityMomentumPair, rspSandwichPair]][
        chains[[positions[[i]], 1]], chains[[positions[[j]], 1]]];
      If[MissingQ[replacement],
        replacement = If[TrueQ[sharedOnly], rspSharedMomentumPair,
          If[split, rspHelicityMomentumPair, rspSandwichPair]][
          chains[[positions[[j]], 1]], chains[[positions[[i]], 1]]]];
      If[!MissingQ[replacement],
        steps++;
        If[steps > limit, Throw[$Failed, tag]];
        Do[factors[[position]] = If[chains[[position, 2]] === 1, 1,
          chains[[position, 1]]^(chains[[position, 2]] - 1)],
          {position, positions[[{i, j}]]}];
        trial = (Times @@ factors) replacement;
        If[rspSandwichExpansionCount[trial] > 4096, Throw[$Failed, tag]];
        Throw[reduce[trial], nodeTag]],
      {i, Length[positions]}, {j, i + 1, Length[positions]}],
      {split, If[TrueQ[sharedOnly], {False}, {False, True}]}];
      Times @@ factors, nodeTag]
  ];
  reduce[value_] := value;
  result = Catch[reduce[expression], tag];
  If[result === $Failed || rspSandwichExpansionCount[result] > 4096, expression, result]
];

rspLegacyProducts[expression_] := (Expand[Expand[Expand[
    rspCommonScalarContent[expression] /. {SpinorChain[a___] :> SpinorChainReduced[a],
      SpinorTrace[a___] :> SpinorTraceReduced[a]}]]] /.
  {SpinorChainReduced[a___] :> SpinorChain[a], SpinorTraceReduced[a___] :> SpinorTrace[a]}) /.
    chain_SpinorChain :> rspHelicityScalarChain[chain];

(* Contract a Dirac--Schouten pair only when its mass contribution already
   occurs with the opposite coefficient. Optional mass data describe external
   integer legs only; the ordinary public reducer supplies no model data. *)
rspMassiveSchoutenSums[expression_, externalRules_List : {}] := Module[
  {rules, protected, inverses, tokens, steps = 0, limit = 64,
   tag = Unique["diracSchoutenSums"], supported, physical, normal, split,
   reduce, contract, result},
  rules = Cases[externalRules, rule : HoldPattern[Mass[_Integer] -> value_] /;
    rspScalarQ[value] && FreeQ[value, _Mass] :> rule];
  rules = First /@ Select[GatherBy[DeleteDuplicates[rules], First], Length[#] === 1 &];
  If[FreeQ[expression, _Plus] || (rules === {} && FreeQ[expression, Mass[_Integer]]),
    Return[expression]];
  supported[spinor_] := MatchQ[spinor,
    Spinor["Spin" | "Helicity" | "Xi", "Angle" | "Square", _] |
    Spinor["Spin", "Upper" | "Lower", "Angle" | "Square", _, _]];
  physical[spinor_] := MatchQ[spinor,
    Spinor["Spin" | "Helicity", "Angle" | "Square", _Integer] |
    Spinor["Spin", "Upper" | "Lower", "Angle" | "Square", _Integer, _]];
  normal[value_] := Expand[value /. chain_SpinorChain :>
      If[Length[chain] === 2 || (Length[chain] === 3 && MatchQ[chain[[2]], Mom[_Integer]]),
        rspLegacyProducts[chain], chain]] /. rules;
  split[term_] := Module[{factors = If[Head[term] === Times, List @@ term, {term}], scalar, monomial},
    scalar = Times @@ Select[factors, rspScalarQ];
    monomial = Times @@ Select[factors, !rspScalarQ[#] &];
    {With[{value = monomial}, HoldComplete[value]], scalar, monomial}];
  contract[value_Plus] := Module[
    {current = normal[value], terms, parts, index, entries, groups, group, unique,
     plans, plan, source, massIndex, trial, accepted, factors, chains, bare, momentum,
     a, b, r, s, leg, mass, rest, sign, massTerm, remainder, massPart, positions},
    If[rspSandwichExpansionCount[current] > 4096, Return[value]];
    While[True,
      If[steps >= limit, Throw[$Failed, tag]];
      terms = If[Head[current] === Plus, List @@ current, {current}];
      If[Length[terms] > 256, Return[If[steps === 0, value, current]]];
      parts = split /@ terms;
      index = AssociationThread[parts[[All, 1]], Range[Length[parts]]];
      entries = {};
      Do[
        If[!FreeQ[terms[[t]], _SpinorHat | _MomHat | _SpinorEps | _SpinorZ | _MomQ], Continue[]];
        factors = If[Head[terms[[t]]] === Times, List @@ terms[[t]], {terms[[t]]}];
        chains = MapIndexed[Which[
          Head[#1] === SpinorChain && Length[#1] >= 2, {#1, 1, First[#2]},
          MatchQ[#1, Power[_SpinorChain, _Integer?Positive]] && Length[#1[[1]]] >= 2,
            {#1[[1]], #1[[2]], First[#2]},
          True, Missing["NotPositiveChain"]] &, factors];
        chains = Select[chains, ListQ];
        Do[
          If[Length[bare[[1]]] =!= 2 || Length[momentum[[1]]] =!= 3 ||
              !MatchQ[momentum[[1, 2]], Mom[_Integer]] ||
              !AllTrue[Join[List @@ bare[[1]],
                {momentum[[1, 1]], momentum[[1, -1]]}], supported],
            Continue[]];
          a = bare[[1, endpoint]]; b = bare[[1, 3 - endpoint]];
          {r, s} = ends; leg = rspSpinorLeg[a];
          If[!physical[a] || momentum[[1, 2]] =!= Mom[leg] ||
              rspSpinorType[b] =!= rspSpinorType[a] ||
              rspSpinorType[r] =!= rspSpinorType[a] ||
              rspSpinorType[s] === rspSpinorType[a], Continue[]];
          rest = factors;
          Do[rest[[record[[3]]]] = If[record[[2]] === 1, 1,
            record[[1]]^(record[[2]] - 1)], {record, {bare, momentum}}];
          rest = Times @@ rest; sign = If[endpoint === 1, 1, -1];
          mass = If[a[[1]] === "Helicity", 0, Mass[leg] /. rules];
          massTerm = normal[sign rest mass SpinorChain[r, b] SpinorChain[rspFlipSpinor[a], s]];
          If[massTerm === 0 || Head[massTerm] === Plus, Continue[]];
          massPart = split[massTerm]; massIndex = Lookup[index, First[massPart], Missing["AbsentMassTerm"]];
          If[MissingQ[massIndex] || massIndex === t, Continue[]];
          remainder = normal[sign rest SpinorChain[a, r] SpinorChain[b, Mom[leg], s]];
          AppendTo[entries, {t, massIndex, massPart[[2]], remainder}];
          If[Length[entries] > 4096, Throw[$Failed, tag]],
          {bare, chains}, {momentum, chains}, {endpoint, 2},
          {ends, {{momentum[[1, 1]], momentum[[1, -1]]},
            {momentum[[1, -1]], momentum[[1, 1]]}}}],
        {t, Length[terms]}];
      entries = DeleteDuplicates[entries];
      groups = GatherBy[entries, #[[2]] &]; accepted = False;
      Do[
        unique = DeleteDuplicatesBy[group, First];
        plans = DeleteDuplicates[Join[{unique}, List /@ Take[unique, UpTo[12]],
          Subsets[Take[unique, UpTo[12]], {2}]]];
        Do[
          source = plan[[All, 1]]; massIndex = plan[[1, 2]];
          If[MemberQ[source, massIndex] ||
              !TrueQ[Expand[parts[[massIndex, 2]] + Total[plan[[All, 3]]]] === 0], Continue[]];
          trial = normal[current - Total[terms[[source]]] - terms[[massIndex]] + Total[plan[[All, 4]]]];
          If[rspSandwichExpansionCount[trial] > 4096, Continue[]];
          If[Length[If[Head[trial] === Plus, List @@ trial, {trial}]] < Length[terms] &&
              expressionComplexity[trial] < expressionComplexity[current],
            current = trial; steps++; accepted = True; Break[]],
          {plan, plans}];
        If[accepted, Break[]], {group, groups}];
      If[!accepted, Break[]]];
    If[current === normal[value], value, current]
  ];
  reduce[value_Plus] := contract[Map[reduce, value]];
  reduce[value_Times] := Map[reduce, value];
  reduce[value_List] := Map[reduce, value];
  reduce[Power[base_, exponent_Integer?Positive]] := Power[reduce[base], exponent];
  reduce[value_] := value;
  If[rspSandwichExpansionCount[expression] > 4096 || !FreeQ[expression,
      Power[base_, exponent_Integer] /; exponent > 64 && !FreeQ[base, _SpinorChain | _SpinorTrace]],
    Return[expression]];
  withProtectedPropagators[expression, Function[body,
    inverses = DeleteDuplicates[Cases[{body}, power : Power[_, exponent_] /;
      !TrueQ[exponent >= 0], Infinity]];
    tokens = Table[Unique["diracSchoutenInverse"], {Length[inverses]}];
    protected = body /. Thread[inverses -> tokens];
    result = Catch[reduce[protected], tag];
    If[result === $Failed, body, result /. Thread[tokens -> inverses]]]]
];

(* Estimate distribution work without constructing Cartesian products. Inverse
   factors and typed contractions remain atomic at this structural stage. *)
rspExpansionProfile[expression_, termLimit_ : 4096, leafLimit_ : 24000] := Module[
  {profile, overflow = {termLimit + 1, leafLimit + 1}, combine},
  combine[{n_, size_}, {m_, other_}] := If[n m > termLimit ||
      m size + n other + n m > leafLimit, overflow,
    {n m, m size + n other + n m}];
  profile[value_Plus] := Module[{parts = profile /@ (List @@ value)},
    {Min[termLimit + 1, Total[parts[[All, 1]]]],
      Min[leafLimit + 1, 1 + Total[parts[[All, 2]]]]}];
  profile[value_Times] := Fold[combine, {1, 0}, profile /@ (List @@ value)];
  profile[value : Power[base_, exponent_Integer?Positive]] := Module[{part = profile[base]},
    If[First[part] === 1, Return[{1, Min[leafLimit + 1, LeafCount[value]]}]];
    If[First[part] > termLimit || exponent >= IntegerLength[termLimit, First[part]],
      overflow, Fold[combine, {1, 0}, ConstantArray[part, exponent]]]];
  profile[value_] := {1, Min[leafLimit + 1, LeafCount[value]]};
  profile[expression]
];

rspExpansionBudgetQ[expression_] := With[{profile = rspExpansionProfile[expression]},
  profile[[1]] <= 4096 && profile[[2]] <= 24000];

(* Algebraic factoring only: inverse factors are independent opaque atoms, so
   this pass cannot combine physical poles or infer a momentum relation. *)
rspLocalFactorContent[expression_] := Module[
  {inverses, tokens, restore, reduce, protected, result, nodes = 0},
  If[LeafCount[expression] > 24000 || FreeQ[expression, _Plus] ||
      FreeQ[expression, _SpinorChain | _SpinorTrace] ||
      !FreeQ[expression, _Dot | _NonCommutativeMultiply | _sigma | _colorT | _colorTrace],
    Return[expression]];
  inverses = DeleteDuplicates[Cases[{expression}, power : Power[_, exponent_] /;
    !TrueQ[exponent >= 0], Infinity]];
  tokens = Table[Unique["spinorFactorInverse"], {Length[inverses]}];
  restore[value_] := value /. Thread[tokens -> inverses];
  protected = expression /. Thread[inverses -> tokens];
  reduce[value_Plus] := Module[{ordinary = Map[reduce, value], factored},
    If[nodes >= 256 || !rspExpansionBudgetQ[ordinary], Return[ordinary]];
    nodes++; factored = Factor[ordinary];
    If[LeafCount[restore[factored]] < LeafCount[restore[ordinary]], factored, ordinary]];
  reduce[value_Times] := Map[reduce, value];
  reduce[Power[base_, exponent_Integer?Positive]] := Power[reduce[base], exponent];
  reduce[value_] := value;
  result = TimeConstrained[reduce[protected], 2, protected];
  restore[result]
];

(* Normalize each distinct contraction once within this invocation. Surrounding
   products remain factored; completed ordinary reductions survive later guards. *)
rspContractionBudgetQ[node_] := Module[{choices, branches, parts = List @@ node},
  If[Head[node] === SpinorTrace,
    If[Length[node] > 8, Return[False]];
    choices = rspLinearMomentumTerms /@ parts,
    If[Length[parts] < 2, Return[True]];
    choices = rspLinearMomentumTerms /@ Rest[Most[parts]]];
  If[MemberQ[choices, $Failed], Return[True]];
  branches = Times @@ (Length /@ choices);
  branches <= 128 && branches LeafCount[node] <= 24000
];
rspNormalizeContraction[chain_SpinorChain] := If[rspContractionBudgetQ[chain],
  rspLegacyProducts[rspLinearChain[chain]], chain];
rspNormalizeContraction[trace_SpinorTrace] := If[rspContractionBudgetQ[trace],
  rspLegacyProducts[rspLinearTrace[trace]], trace];

rspFactoredContractions[body_, normalizer_] := Module[{nodes, replacements = {}, result},
  nodes = SortBy[DeleteDuplicates[Cases[{body}, _SpinorChain | _SpinorTrace, Infinity]], LeafCount];
  TimeConstrained[
    Do[result = normalizer[node];
      If[FreeQ[result, _TerminatedEvaluation | $Aborted | $Failed | Indeterminate |
          ComplexInfinity | _DirectedInfinity], AppendTo[replacements, node -> result]],
      {node, nodes}],
    5, Null];
  body /. Dispatch[replacements]
];

(* Do not retain a prettier original in place of a direct Dirac or Clifford
   contraction. Mere orientation and distribution are representation choices. *)
rspElementaryContractionQ[body_] := AnyTrue[Cases[{body}, _SpinorChain | _SpinorTrace, Infinity],
  Function[node, Module[{parts = List @@ node, slots, endpoints},
    If[Head[node] === SpinorTrace, Return[Length[node] <= 2]];
    If[Length[parts] < 2, Return[False]];
    slots = Rest[Most[parts]]; endpoints = parts[[{1, -1}]];
    (Length[parts] === 2 && First[parts] === Last[parts]) ||
      (slots =!= {} && (First[slots] === Mom[rspSpinorLeg[First[endpoints]]] ||
        Last[slots] === Mom[rspSpinorLeg[Last[endpoints]]])) ||
      AnyTrue[Partition[slots, 2, 1], MatchQ[First[#], _Mom | _MomHat] &&
        First[#] === Last[#] &]
  ]]];

rspProductReductionCandidates[body_, retainOriginal_ : False] := Module[
  {factored, linear, reduced, contracted, normalize, legacy = Missing["DistributionBudget"]},
  normalize[node_] := normalize[node] = rspNormalizeContraction[node];
  factored = rspFactoredContractions[body, normalize];
  (* The complete legacy route remains a competitor whenever its prospective
     expansion is bounded. Large rational factorizations never enter it. *)
  If[rspExpansionBudgetQ[factored] &&
      AllTrue[DeleteDuplicates[Cases[{factored}, _SpinorChain | _SpinorTrace, Infinity]],
        rspContractionBudgetQ],
    legacy = TimeConstrained[
      linear = rspSandwichProducts[body, True];
      linear = rspFactoredContractions[linear, normalize];
      linear = rspSandwichProducts[linear];
      If[!rspExpansionBudgetQ[linear], Missing["DistributionBudget"],
        reduced = rspLegacyProducts[linear];
        contracted = rspSandwichProducts[reduced];
        reduced = If[contracted === reduced, reduced, rspLegacyProducts[contracted]];
        If[rspExpansionBudgetQ[reduced], rspMassiveSchoutenSums[reduced], reduced]],
      5, Missing["DistributionBudget"]]];
  If[!FreeQ[legacy, _TerminatedEvaluation | $Aborted | $Failed | Indeterminate |
      ComplexInfinity | _DirectedInfinity], legacy = Missing["UnfinishedReduction"]];
  If[MissingQ[legacy], Return[DeleteDuplicates[Join[{factored},
    If[TrueQ[retainOriginal] && !FreeQ[body, _Plus], {body}, {}]]]]];
  If[!TrueQ[retainOriginal] || FreeQ[body, _Plus], Return[{legacy}]];
  DeleteDuplicates[Join[{legacy, factored},
    If[!TrueQ[rspElementaryContractionQ[body]], {body}, {}]]]
];

rspBestReduction[candidates_List, factor_ : True] := Module[{best, rank, next, nextRank},
  best = First[candidates];
  rank[value_] := rank[value] = {expressionComplexity[value],
    If[Head[value] === Plus, Length[value], If[value === 0, 0, 1]], LeafCount[value]};
  Do[
    If[rspExpansionBudgetQ[best] && rspExpansionBudgetQ[next],
      If[OrderedQ[{rank[next], rank[best]}] && rank[next] =!= rank[best], best = next],
      If[LeafCount[next] < LeafCount[best], best = next]],
    {next, Rest[candidates]}];
  If[!TrueQ[factor], Return[best]];
  next = rspLocalFactorContent[best];
  If[LeafCount[next] < LeafCount[best],
    If[!rspExpansionBudgetQ[best] || !rspExpansionBudgetQ[next] ||
        OrderedQ[{rank[next], rank[best]}], best = next]];
  best
];

ReduceSpinorProducts[amp_] /; (Head[amp] === SpinorChain && Length[amp] <= 3) ||
    (Head[amp] === SpinorTrace && Length[amp] <= 2) :=
  ConstructiveDiagrams`Private`withProtectedPropagators[amp,
    ConstructiveDiagrams`Private`rspNormalizeContraction];
ReduceSpinorProducts[amp_] := Module[
  {hasPropagators = !FreeQ[amp, _PropDen], factoredAmplitude},
  factoredAmplitude = hasPropagators && !FreeQ[amp, _Plus];
  ConstructiveDiagrams`Private`rspBestReduction[
    ConstructiveDiagrams`Private`withProtectedPropagators[amp,
      Function[body, ConstructiveDiagrams`Private`rspProductReductionCandidates[body,
        factoredAmplitude]]], hasPropagators]];

End[];


(* ::Subsubsection::Closed:: *)
(*Ordering and Splitting*)


(* ::Input::Initialization:: *)
(*[11] = <11> = 0*)
SpinorChainReduced[sHead_[sh_,sa_,p_],sHead_[sh_,sa_,p_]]/;spinorHeadQ[sHead]:=0;


(* ::Input::Initialization:: *)
(*[21]=-[12]*)
SpinorChainReduced[sHead_[sh1_,"Angle",p1_],mHead_[p2_],sHead_[sh3_,"Square",p3_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=SpinorChainReduced[sHead[sh3,"Square",p3],mHead[p2],sHead[sh1,"Angle",p1]];
SpinorChainReduced[sHead_[sh1_,"Angle",p1_],mHead_[p2_],mHead_[p4_],mHead_[p5_],sHead_[sh3_,"Square",p3_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=SpinorChainReduced[sHead[sh3,"Square",p3],mHead[p5],mHead[p4],mHead[p2],sHead[sh1,"Angle",p1]];
(*<p1|p2|p3] = [p3|p2|p1>*)
SpinorChainReduced[sHead_[sh1_,tp1_,p1_],sHead_[sh2_,tp2_,p2_]]/;(p2<p1&&spinorHeadQ[sHead]):=-SpinorChainReduced[sHead[sh2,tp2,p2],sHead[sh1,tp1,p1]];
(*[2|p5p6|1] = -[1|p6p5|2]*)
SpinorChainReduced[sHead_[sh1_,tp_,p1_],mHead_[p5_],mHead_[p6_],sHead_[sh2_,tp_,p2_]]/;(p2<p1&&spinorHeadQ[sHead]&&momHeadQ[mHead]):=-SpinorChainReduced[sHead[sh2,tp,p2],mHead[p6],mHead[p5],sHead[sh1,tp,p1]];
(*[1|p6p5|2] = 2p5.p6[12] -[1|p5p6|2]*)
SpinorChainReduced[sHead_[sh1_,tp_,p1_],mHead_[p5_],mHead_[p6_],sHead_[sh2_,tp_,p2_]]/;(p6<p5&&p6!=p2&&p5!=p1&&spinorHeadQ[sHead]&&momHeadQ[mHead]):=2momProdHead[sHead][p5,p6]SpinorChainReduced[sHead[sh1,tp,p1],sHead[sh2,tp,p2]]-SpinorChainReduced[sHead[sh1,tp,p1],mHead[p6],mHead[p5],sHead[sh2,tp,p2]];
(*[\[Zeta]2|p1p3|2] = - [2|p3p1|\[Zeta]2]*)
SpinorChainReduced[sHead_["Zeta",sa_,p2_],mHead_[p1_],mHead_[p3_],sHead_["Helicity",sa_,p2_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=-SpinorChainReduced[sHead["Helicity",sa,p2],mHead[p3],mHead[p1],sHead["Zeta",sa,p2]];

(* Reversal changes only orientation, never spin-index variance or labels. *)
Begin["ConstructiveDiagrams`Private`"];
SpinorChainReduced[left_, momenta___, right_] /;
  ConstructiveDiagrams`Private`rspReverseChainQ[left, {momenta}, right] :=
  (-1)^(Length[{momenta}] + 1) (SpinorChainReduced @@ Reverse[{left, momenta, right}]);
SpinorChainReduced[s_, s_] /;
  ConstructiveDiagrams`Private`rspSpinorType[s] =!= None := 0;
End[];


(* ::Input::Initialization:: *)
(*<p1|...(a+b)...|p2> = <p1|...a...|p2>+<p1|...b...|p2>*)
SpinorChainReduced[a___,-b_,d___]:=-SpinorChainReduced[a,b,d];
SpinorChainReduced[a___,Plus[b_,c__],d___]:=SpinorChainReduced[a,b,d]+SpinorChainReduced[a,Plus[c],d];
SpinorChainReduced[a___,1,b___]:=SpinorChainReduced[a,b];

SpinorChainReduced[a__,mHead_[Multiparticle[b_,c_]],d__]/;(momHeadQ[mHead]):=SpinorChainReduced[a,mHead[b],d]+SpinorChainReduced[a,mHead[c],d];
SpinorChainReduced[a__,mHead_[Multiparticle[b_,c__]],d__]/;(momHeadQ[mHead]):=SpinorChainReduced[a,mHead[b],d]+SpinorChainReduced[a,mHead[Multiparticle[c]],d];

(*Test*)
(*SpinorChainReduced[Spinor["Spin","Angle",1],Mom[2]+Mom[3]-Sum[Mom[ii],{ii,1,5}],Spinor["Spin","Square",4]]*)


(* ::Subsubsection::Closed:: *)
(*Spinor - Mass Identities (...p|p> = m ...|p])*)


(* ::Input::Initialization:: *)
(*...p|p> = m ...|p]*)
SpinorChainReduced[a__,mHead_[b_],sHead_["Helicity",as_,b_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=0;
SpinorChainReduced[sHead_["Helicity",as_,a_],mHead_[a_],b__]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=0;
SpinorChainReduced[sHead_["Helicity",as_,a_],sHead_["Helicity",as_,a_]]/;(spinorHeadQ[sHead]):=0;

SpinorChainReduced[a__,mHead_[b_],sHead_["Spin","Angle",b_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=-Mass[b]SpinorChainReduced[a,sHead["Spin","Square",b]];
SpinorChainReduced[a__,mHead_[b_],sHead_["Spin",ul_,"Angle",b_,J_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=-Mass[b]SpinorChainReduced[a,sHead["Spin",ul,"Square",b,J]];
SpinorChainReduced[a__,mHead_[b_],sHead_["Spin","Square",b_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=-Mass[b]SpinorChainReduced[a,sHead["Spin","Angle",b]];
SpinorChainReduced[a__,mHead_[b_],sHead_["Spin",ul_,"Square",b_,J_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=-Mass[b]SpinorChainReduced[a,sHead["Spin",ul,"Angle",b,J]];
SpinorChainReduced[sHead_["Spin","Angle",b_],mHead_[b_],a__]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=Mass[b]SpinorChainReduced[sHead["Spin","Square",b],a];
SpinorChainReduced[sHead_["Spin",ul_,"Angle",b_,J_],mHead_[b_],a__]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=Mass[b]SpinorChainReduced[sHead["Spin",ul,"Square",b,J],a];
SpinorChainReduced[sHead_["Spin","Square",b_],mHead_[b_],a__]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=Mass[b]SpinorChainReduced[sHead["Spin","Angle",b],a];
SpinorChainReduced[sHead_["Spin",ul_,"Square",b_,J_],mHead_[b_],a__]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=Mass[b]SpinorChainReduced[sHead["Spin",ul,"Angle",b,J],a];

(* The same endpoint equations hold with an explicit spin index. *)
Begin["ConstructiveDiagrams`Private`"];
SpinorChainReduced[a__, mHead_[leg_], endpoint_] /;
  (momHeadQ[mHead] && Length[endpoint] > 3 &&
    ConstructiveDiagrams`Private`rspSpinorType[endpoint] =!= None &&
    ConstructiveDiagrams`Private`rspSpinorLeg[endpoint] === leg &&
    MemberQ[{"Spin", "Helicity"}, endpoint[[1]]]) :=
  If[endpoint[[1]] === "Helicity", 0,
    -Mass[leg] SpinorChainReduced[a, ConstructiveDiagrams`Private`rspFlipSpinor[endpoint]]];
SpinorChainReduced[endpoint_, mHead_[leg_], a__] /;
  (momHeadQ[mHead] && Length[endpoint] > 3 &&
    ConstructiveDiagrams`Private`rspSpinorType[endpoint] =!= None &&
    ConstructiveDiagrams`Private`rspSpinorLeg[endpoint] === leg &&
    MemberQ[{"Spin", "Helicity"}, endpoint[[1]]]) :=
  If[endpoint[[1]] === "Helicity", 0,
    Mass[leg] SpinorChainReduced[ConstructiveDiagrams`Private`rspFlipSpinor[endpoint], a]];
End[];


(* ::Subsubsection::Closed:: *)
(*Forms with \[Zeta]*)


(*<2\[Zeta]2> = Sqrt[2E2]*)
SpinorChainReduced[sHead_["Helicity",sa1_,p1_],sHead_["Zeta",sa1_,p1_]]/;(spinorHeadQ[sHead]):=Sqrt[2En[p1]];
SpinorChainReduced[sHead_["Zeta",sa1_,p1_],sHead_["Helicity",sa1_,p1_]]/;(spinorHeadQ[sHead]):=-Sqrt[2En[p1]];
(*<\[Zeta]1\[Zeta]2> = <21>/Sqrt[4E1E2]*)
SpinorChainReduced[sHead_["Zeta","Angle",p1_],sHead_["Zeta","Angle",p2_]]/;(spinorHeadQ[sHead]):=SpinorChainReduced[sHead["Helicity","Square",p2],sHead["Helicity","Square",p1]]/Sqrt[4En[p1]En[p2]];
SpinorChainReduced[sHead_["Zeta","Square",p1_],sHead_["Zeta","Square",p2_]]/;(spinorHeadQ[sHead]):=SpinorChainReduced[sHead["Helicity","Angle",p2],sHead["Helicity","Angle",p1]]/Sqrt[4En[p1]En[p2]];
(*<1\[Zeta]2><\[Zeta]12> = <2\[Zeta]2><\[Zeta]11> - <21><\[Zeta]1\[Zeta]2>*)
Unprotect[Times];
SpinorChainReduced[sHead_["Helicity",sa_,p1_],sHead_["Zeta",sa_,p2_]]SpinorChainReduced[sHead_["Zeta",sa_,p1_],sHead_["Helicity",sa_,p2_]]/;(spinorHeadQ[sHead]):=
SpinorChainReduced[sHead["Helicity",sa,p2],sHead["Zeta",sa,p2]]SpinorChainReduced[sHead["Zeta",sa,p1],sHead["Helicity",sa,p1]]-
SpinorChainReduced[sHead["Helicity",sa,p2],sHead["Helicity",sa,p1]]SpinorChainReduced[sHead["Zeta",sa,p1],sHead["Zeta",sa,p2]];
Protect[Times];
(*[1|p2|\[Zeta]2> = [12]<2\[Zeta]2>*)
SpinorChainReduced[aa__,mHead_[p2_],sHead_["Zeta","Angle",p2_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=SpinorChainReduced[aa,sHead["Helicity","Square",p2]]SpinorChainReduced[sHead["Helicity","Angle",p2],sHead["Zeta","Angle",p2]];
SpinorChainReduced[aa__,mHead_[p2_],sHead_["Zeta","Square",p2_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=SpinorChainReduced[aa,sHead["Helicity","Angle",p2]]SpinorChainReduced[sHead["Helicity","Square",p2],sHead["Zeta","Square",p2]];
SpinorChainReduced[sHead_["Zeta","Angle",p2_],mHead_[p2_],aa__]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=SpinorChainReduced[sHead["Zeta","Angle",p2],sHead["Helicity","Angle",p2]]SpinorChainReduced[sHead["Helicity","Square",p2],aa];
SpinorChainReduced[sHead_["Zeta","Square",p2_],mHead_[p2_],aa__]:=SpinorChainReduced[sHead["Zeta","Square",p2],sHead["Helicity","Square",p2]]SpinorChainReduced[sHead["Helicity","Angle",p2],aa];


(* ::Subsubsection::Closed:: *)
(*Momentum Rearrangements (<p1|...p2p1... = )*)


(* ::Input::Initialization:: *)
(*<p1|...p2p1... = *)
SpinorChainReduced[a___,mHead_[pN_],mHead_[pN_],b___]/;(momHeadQ[mHead]):=Mass[pN]^2 SpinorChainReduced[a,b];
Begin["ConstructiveDiagrams`Private`"];
(* Contract a matched pair in one step. Every resulting word is shorter, so
   two interlaced repeated momenta cannot undo each other's anticommutations. *)
rspRepeatedMomentum[prefix_List, head_, label_, middle_List, suffix_List] := Module[
  {length = Length[middle], short},
  short[parts_List] := SpinorChainReduced @@ Join[prefix, parts, suffix];
  Total[Table[2 (-1)^(j - 1) momProdHead[head][label, middle[[j, 1]]]
      short[Append[Delete[middle, j], head[label]]], {j, length}]] +
    (-1)^length momProdHead[head][label, label] short[middle]
];
SpinorChainReduced[a___, mHead_[pN_], b__, mHead_[pN_], c___] /;
  (momHeadQ[mHead] && AllTrue[{b}, Head[#] === mHead &]) :=
  rspRepeatedMomentum[{a}, mHead, pN, {b}, {c}];
End[];

SpinorChainReduced[a___,mHead_[pN_],mHead_[pM_],b___,sHead_[sh_,ul_,pN_]]/;(pM!=pN&&spinorHeadQ[sHead]&&momHeadQ[mHead]):=2momProdHead[sHead][pN,pM]SpinorChainReduced[a,b,sHead[sh,ul,pN]]-SpinorChainReduced[a,mHead[pM],mHead[pN],b,sHead[sh,ul,pN]];
SpinorChainReduced[sHead_[sh_,ul_,pN_],b___,mHead_[pM_],mHead_[pN_],a___]/;(pM!=pN&&spinorHeadQ[sHead]&&momHeadQ[mHead]):=2momProdHead[sHead][pM,pN]SpinorChainReduced[sHead[sh,ul,pN],b,a]-SpinorChainReduced[sHead[sh,ul,pN],b,mHead[pN],mHead[pM],a];

Begin["ConstructiveDiagrams`Private`"];
SpinorChainReduced[a___, mHead_[pN_], mHead_[pM_], b___, endpoint_] /;
  (pM =!= pN && momHeadQ[mHead] && Length[endpoint] > 3 &&
    ConstructiveDiagrams`Private`rspSpinorType[endpoint] =!= None &&
    ConstructiveDiagrams`Private`rspSpinorLeg[endpoint] === pN &&
    MemberQ[{"Spin", "Helicity"}, endpoint[[1]]]) :=
  2 momProdHead[mHead][pN, pM] SpinorChainReduced[a, b, endpoint] -
    SpinorChainReduced[a, mHead[pM], mHead[pN], b, endpoint];
SpinorChainReduced[endpoint_, b___, mHead_[pM_], mHead_[pN_], a___] /;
  (pM =!= pN && momHeadQ[mHead] && Length[endpoint] > 3 &&
    ConstructiveDiagrams`Private`rspSpinorType[endpoint] =!= None &&
    ConstructiveDiagrams`Private`rspSpinorLeg[endpoint] === pN &&
    MemberQ[{"Spin", "Helicity"}, endpoint[[1]]]) :=
  2 momProdHead[mHead][pM, pN] SpinorChainReduced[endpoint, b, a] -
    SpinorChainReduced[endpoint, b, mHead[pN], mHead[pM], a];
End[];

(*Test*)
(*nice[SpinorChainReduced[Spinor["Spin","Square",4],Mom[1],Mom[2],Mom[4],Mom[1],Mom[3],Spinor["Spin","Angle",1]]/.SpinorChainReduced->SpinorChain]*)


(* ::Subsubsection::Closed:: *)
(*Schouten Identities ([12][34]=[42][31]-[41][32] and Generalizations)*)


(* ::Input::Initialization:: *)
Unprotect[Plus];
(*[14][23]-[13][24]*)
SpinorChainReduced[sHead_[sh1_,tp_,p1_],sHead_[sh4_,tp_,p4_]]SpinorChainReduced[sHead_[sh2_,tp_,p2_],sHead_[sh3_,tp_,p3_]]-SpinorChainReduced[sHead_[sh1_,tp_,p1_],sHead_[sh3_,tp_,p3_]]SpinorChainReduced[sHead_[sh2_,tp_,p2_],sHead_[sh4_,tp_,p4_]]/;(spinorHeadQ[sHead]):=-SpinorChainReduced[sHead[sh1,tp,p1],sHead[sh2,tp,p2]]SpinorChainReduced[sHead[sh3,tp,p3],sHead[sh4,tp,p4]];
(*[14][23]+[13][42]*)
SpinorChainReduced[sHead_[sh1_,tp_,p1_],sHead_[sh4_,tp_,p4_]]SpinorChainReduced[sHead_[sh2_,tp_,p2_],sHead_[sh3_,tp_,p3_]]-SpinorChainReduced[sHead_[sh1_,tp_,p1_],sHead_[sh3_,tp_,p3_]]SpinorChainReduced[sHead_[sh4_,tp_,p4_],sHead_[sh2_,tp_,p2_]]/;(spinorHeadQ[sHead]):=-SpinorChainReduced[sHead[sh1,tp,p1],sHead[sh2,tp,p2]]SpinorChainReduced[sHead[sh3,tp,p3],sHead[sh4,tp,p4]];
(*[14][23]+[31][24]*)
SpinorChainReduced[sHead_[sh1_,tp_,p1_],sHead_[sh4_,tp_,p4_]]SpinorChainReduced[sHead_[sh2_,tp_,p2_],sHead_[sh3_,tp_,p3_]]-SpinorChainReduced[sHead_[sh3_,tp_,p3_],sHead_[sh1_,tp_,p1_]]SpinorChainReduced[sHead_[sh2_,tp_,p2_],sHead_[sh4_,tp_,p4_]]/;(spinorHeadQ[sHead]):=-SpinorChainReduced[sHead[sh1,tp,p1],sHead[sh2,tp,p2]]SpinorChainReduced[sHead[sh3,tp,p3],sHead[sh4,tp,p4]];
(*[14][23]-[31][42]*)
SpinorChainReduced[sHead_[sh1_,tp_,p1_],sHead_[sh4_,tp_,p4_]]SpinorChainReduced[sHead_[sh2_,tp_,p2_],sHead_[sh3_,tp_,p3_]]-SpinorChainReduced[sHead_[sh3_,tp_,p3_],sHead_[sh1_,tp_,p1_]]SpinorChainReduced[sHead_[sh4_,tp_,p4_],sHead_[sh2_,tp_,p2_]]/;(spinorHeadQ[sHead]):=-SpinorChainReduced[sHead[sh1,tp,p1],sHead[sh2,tp,p2]]SpinorChainReduced[sHead[sh3,tp,p3],sHead[sh4,tp,p4]];
(*[12][34]+[14][23] = [42][31]*)
SpinorChainReduced[sHead_[sh1_,tp_,p1_],sHead_[sh2_,tp_,p2_]]SpinorChainReduced[sHead_[sh3_,tp_,p3_],sHead_[sh4_,tp_,p4_]]+SpinorChainReduced[sHead_[sh1_,tp_,p1_],sHead_[sh4_,tp_,p4_]]SpinorChainReduced[sHead_[sh2_,tp_,p2_],sHead_[sh3_,tp_,p3_]]/;(spinorHeadQ[sHead]):=SpinorChainReduced[sHead[sh4,tp,p4],sHead[sh2,tp,p2]]SpinorChainReduced[sHead[sh3,tp,p3],sHead[sh1,tp,p1]];
(*[12][34]-[41][23] = [42][31]*)
SpinorChainReduced[sHead_[sh1_,tp_,p1_],sHead_[sh2_,tp_,p2_]]SpinorChainReduced[sHead_[sh3_,tp_,p3_],sHead_[sh4_,tp_,p4_]]-SpinorChainReduced[sHead_[sh4_,tp_,p4_],sHead_[sh1_,tp_,p1_]]SpinorChainReduced[sHead_[sh2_,tp_,p2_],sHead_[sh3_,tp_,p3_]]/;(spinorHeadQ[sHead]):=SpinorChainReduced[sHead[sh4,tp,p4],sHead[sh2,tp,p2]]SpinorChainReduced[sHead[sh3,tp,p3],sHead[sh1,tp,p1]];
(*[12][34]-[14][32] = [42][31]*)
SpinorChainReduced[sHead_[sh1_,tp_,p1_],sHead_[sh2_,tp_,p2_]]SpinorChainReduced[sHead_[sh3_,tp_,p3_],sHead_[sh4_,tp_,p4_]]-SpinorChainReduced[sHead_[sh1_,tp_,p1_],sHead_[sh4_,tp_,p4_]]SpinorChainReduced[sHead_[sh3_,tp_,p3_],sHead_[sh2_,tp_,p2_]]/;(spinorHeadQ[sHead]):=SpinorChainReduced[sHead[sh4,tp,p4],sHead[sh2,tp,p2]]SpinorChainReduced[sHead[sh3,tp,p3],sHead[sh1,tp,p1]];
(*[12][34]+[41][32] = [42][31]*)
SpinorChainReduced[sHead_[sh1_,tp_,p1_],sHead_[sh2_,tp_,p2_]]SpinorChainReduced[sHead_[sh3_,tp_,p3_],sHead_[sh4_,tp_,p4_]]+SpinorChainReduced[sHead_[sh4_,tp_,p4_],sHead_[sh1_,tp_,p1_]]SpinorChainReduced[sHead_[sh3_,tp_,p3_],sHead_[sh2_,tp_,p2_]]/;(spinorHeadQ[sHead]):=SpinorChainReduced[sHead[sh4,tp,p4],sHead[sh2,tp,p2]]SpinorChainReduced[sHead[sh3,tp,p3],sHead[sh1,tp,p1]];
Protect[Plus];

(*Test*)
(*SpinorChain[Spinor["Spin","Angle",1],Spinor["Spin","Angle",4]]SpinorChain[Spinor["Spin","Angle",2],Spinor["Spin","Angle",3]]-SpinorChain[Spinor["Spin","Angle",1],Spinor["Spin","Angle",3]]SpinorChain[Spinor["Spin","Angle",2],Spinor["Spin","Angle",4]]
ReduceSpinorProducts[SpinorChain[Spinor["Spin","Angle",1],Spinor["Spin","Angle",4]]SpinorChain[Spinor["Spin","Angle",2],Spinor["Spin","Angle",3]]-SpinorChain[Spinor["Spin","Angle",1],Spinor["Spin","Angle",3]]SpinorChain[Spinor["Spin","Angle",2],Spinor["Spin","Angle",4]]]*)


Unprotect[Plus];
(*[13][4|p1|2>-[14][3|p1|2> = -[34][1|p1|2>*)
SpinorChainReduced[sHead_[sh1_,"Square",p1_],sHead_[sh3_,"Square",p3_]]SpinorChainReduced[sHead_[sh4_,"Square",p4_],mHead_[p1_],sHead_[sh2_,"Angle",p2_]]-
SpinorChainReduced[sHead_[sh1_,"Square",p1_],sHead_[sh4_,"Square",p4_]]SpinorChainReduced[sHead_[sh3_,"Square",p3_],mHead_[p1_],sHead_[sh2_,"Angle",p2_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=
-SpinorChainReduced[sHead[sh3,"Square",p3],sHead[sh4,"Square",p4]]SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p1],sHead[sh2,"Angle",p2]];
Protect[Plus];


(* ::Input::Initialization:: *)
Unprotect[Times];
(*<14>[4|p1|3> = <34>[4|p1|1> - <31>[4|p1|4>*)
(*SpinorChainReduced[sHead_[sh1_,"Angle",p1_],sHead_[sh4_,"Angle",p4_]]SpinorChainReduced[sHead_[sh4_,"Square",p4_],mHead_[p1_],sHead_[sh3_,"Angle",p3_]]/;(p4!=p1&&p4!=p3&&spinorHeadQ[sHead]&&momHeadQ[mHead]):=SpinorChainReduced[sHead[sh3,"Angle",p3],sHead[sh4,"Angle",p4]]SpinorChainReduced[sHead[sh4,"Square",p4],mHead[p1],sHead[sh1,"Angle",p1]]-SpinorChainReduced[sHead[sh3,"Angle",p3],sHead[sh1,"Angle",p1]]SpinorChainReduced[sHead[sh4,"Square",p4],mHead[p1],sHead[sh4,"Angle",p4]];
SpinorChainReduced[sHead_[sh4_,"Angle",p4_],sHead_[sh1_,"Angle",p1_]]SpinorChainReduced[sHead_[sh4_,"Square",p4_],mHead_[p1_],sHead_[sh3_,"Angle",p3_]]/;(p4!=p1&&p4!=p3&&spinorHeadQ[sHead]&&momHeadQ[mHead]):=-SpinorChainReduced[sHead[sh3,"Angle",p3],sHead[sh4,"Angle",p4]]SpinorChainReduced[sHead[sh4,"Square",p4],mHead[p1],sHead[sh1,"Angle",p1]]+SpinorChainReduced[sHead[sh3,"Angle",p3],sHead[sh1,"Angle",p1]]SpinorChainReduced[sHead[sh4,"Square",p4],mHead[p1],sHead[sh4,"Angle",p4]];*)
Protect[Times];


(* ::Input::Initialization:: *)
Unprotect[Times];
(*[1|p3|2>[3|p1|4>*)
SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p3_],sHead_[sh2_,"Angle",p2_]]SpinorChainReduced[sHead_[sh3_,"Square",p3_],mHead_[p1_],sHead_[sh4_,"Angle",p4_]]/;(p4!=p2&&spinorHeadQ[sHead]&&momHeadQ[mHead]):=-SpinorChainReduced[sHead[sh4,"Angle",p4],mHead[p1],mHead[p3],sHead[sh2,"Angle",p2]]SpinorChainReduced[sHead[sh3,"Square",p3],sHead[sh1,"Square",p1]]+SpinorChainReduced[sHead[sh4,"Angle",p4],mHead[p1],sHead[sh1,"Square",p1]]SpinorChainReduced[sHead[sh3,"Square",p3],mHead[p3],sHead[sh2,"Angle",p2]];
(*[1|p4|2>[3|p1|4>*)
SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p4_],sHead_[sh2_,"Angle",p2_]]SpinorChainReduced[sHead_[sh3_,"Square",p3_],mHead_[p1_],sHead_[sh4_,"Angle",p4_]]/;(p4!=p2&&spinorHeadQ[sHead]&&momHeadQ[mHead]):=-SpinorChainReduced[sHead[sh4,"Angle",p4],mHead[p1],mHead[p4],sHead[sh2,"Angle",p2]]SpinorChainReduced[sHead[sh3,"Square",p3],sHead[sh1,"Square",p1]]+SpinorChainReduced[sHead[sh4,"Angle",p4],mHead[p1],sHead[sh1,"Square",p1]]SpinorChainReduced[sHead[sh3,"Square",p3],mHead[p4],sHead[sh2,"Angle",p2]];
(*[1|p3|2>[3|p2|4>*)
SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p3_],sHead_[sh2_,"Angle",p2_]]SpinorChainReduced[sHead_[sh3_,"Square",p3_],mHead_[p2_],sHead_[sh4_,"Angle",p4_]]/;(p4!=p2&&spinorHeadQ[sHead]&&momHeadQ[mHead]):=
-SpinorChainReduced[sHead[sh4,"Angle",p4],mHead[p2],mHead[p3],sHead[sh2,"Angle",p2]]SpinorChainReduced[sHead[sh3,"Square",p3],sHead[sh1,"Square",p1]]+SpinorChainReduced[sHead[sh4,"Angle",p4],mHead[p2],sHead[sh1,"Square",p1]]SpinorChainReduced[sHead[sh3,"Square",p3],mHead[p3],sHead[sh2,"Angle",p2]];
(*[1|p4|2>[3|p2|4>*)
SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p4_],sHead_[sh2_,"Angle",p2_]]SpinorChainReduced[sHead_[sh3_,"Square",p3_],mHead_[p2_],sHead_[sh4_,"Angle",p4_]]/;(p3!=p1&&spinorHeadQ[sHead]&&momHeadQ[mHead]):=-SpinorChainReduced[sHead[sh4,"Angle",p4],sHead[sh2,"Angle",p2]]SpinorChainReduced[sHead[sh3,"Square",p3],mHead[p2],mHead[p4],sHead[sh1,"Square",p1]]+SpinorChainReduced[sHead[sh4,"Angle",p4],mHead[p4],sHead[sh1,"Square",p1]]SpinorChainReduced[sHead[sh3,"Square",p3],mHead[p2],sHead[sh2,"Angle",p2]];
Protect[Times];

(*Test*)
(*ReduceSpinorProducts[SpinorChain[Spinor["Spin","Square",1],Mom[4],Spinor["Spin","Angle",2]]SpinorChain[Spinor["Spin","Square",3],Mom[2],Spinor["Spin","Angle",4]]]*)


(* ::Input::Initialization:: *)
Unprotect[Times];
(* Times definitions survive package-symbol clearing on reload. Remove only
   these five obsolete unguarded orientations, not unrelated product rules. *)
Quiet[Unset[SpinorChainReduced[sHead_[sh1_,sq_,p1_],mHead_[p4_],sHead_[sh2_,an_,p2_]]SpinorChainReduced[sHead_[sh3_,sq_,p3_],mHead_[p5_],sHead_[sh4_,an_,p4_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead])], Unset::norep];
Quiet[Unset[SpinorChainReduced[sHead_[sh1_,sq_,p1_],mHead_[p5_],sHead_[sh2_,an_,p2_]]SpinorChainReduced[sHead_[sh3_,sq_,p3_],mHead_[p1_],sHead_[sh4_,an_,p4_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead])], Unset::norep];
Quiet[Unset[SpinorChainReduced[sHead_[sh1_,sq_,p1_],mHead_[p5_],sHead_[sh2_,an_,p2_]]SpinorChainReduced[sHead_[sh3_,sq_,p3_],mHead_[p1_],sHead_[sh4_,an_,p4_]]^pow_/;(spinorHeadQ[sHead]&&momHeadQ[mHead])], Unset::norep];
Quiet[Unset[SpinorChainReduced[sHead_[sh4_,sq_,p4_],mHead_[p1_],sHead_[sh3_,an_,p3_]]^pow_ SpinorChainReduced[sHead_[sh1_,sq_,p1_],mHead_[p3_],sHead_[sh2_,an_,p2_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead])], Unset::norep];
Quiet[Unset[SpinorChainReduced[sHead_[sh4_,sq_,p4_],mHead_[p1_],sHead_[sh3_,an_,p3_]]^pow_ SpinorChainReduced[sHead_[sh2_,sq_,p2_],mHead_[p5_],sHead_[sh1_,an_,p1_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead])], Unset::norep];
(*[1|p4|2>[3|p5|4> = -<42>[3|p5p4|1]+<4|p4|1][3|p5|2>*)
(* These orientations expose a physical endpoint for reduction. A Xi label
   is only an identifier; targeting it can reverse the same Schouten move. *)
SpinorChainReduced[sHead_[sh1_,sq_,p1_],mHead_[p4_],sHead_[sh2_,an_,p2_]]SpinorChainReduced[sHead_[sh3_,sq_,p3_],mHead_[p5_],sHead_[sh4_,an_,p4_]]/;(sh4=!= "Xi"&&spinorHeadQ[sHead]&&momHeadQ[mHead]):=-SpinorChainReduced[sHead[sh4,an,p4],sHead[sh2,an,p2]]SpinorChainReduced[sHead[sh3,sq,p3],mHead[p5],mHead[p4],sHead[sh1,sq,p1]]+SpinorChainReduced[sHead[sh4,an,p4],mHead[p4],sHead[sh1,sq,p1]]SpinorChainReduced[sHead[sh3,sq,p3],mHead[p5],sHead[sh2,an,p2]];
(*[1|p5|2>[3|p1|4> = -<4|p1p5|2>[31] + <4|p1|1][3|p5|2>*)
SpinorChainReduced[sHead_[sh1_,sq_,p1_],mHead_[p5_],sHead_[sh2_,an_,p2_]]SpinorChainReduced[sHead_[sh3_,sq_,p3_],mHead_[p1_],sHead_[sh4_,an_,p4_]]/;(sh1=!= "Xi"&&spinorHeadQ[sHead]&&momHeadQ[mHead]):=-SpinorChainReduced[sHead[sh4,an,p4],mHead[p1],mHead[p5],sHead[sh2,an,p2]]SpinorChainReduced[sHead[sh3,sq,p3],sHead[sh1,sq,p1]]+SpinorChainReduced[sHead[sh4,an,p4],mHead[p1],sHead[sh1,sq,p1]]SpinorChainReduced[sHead[sh3,sq,p3],mHead[p5],sHead[sh2,an,p2]];
(*[1|p5|2>[3|p1|4(>^2)*)
SpinorChainReduced[sHead_[sh1_,sq_,p1_],mHead_[p5_],sHead_[sh2_,an_,p2_]]SpinorChainReduced[sHead_[sh3_,sq_,p3_],mHead_[p1_],sHead_[sh4_,an_,p4_]]^pow_/;(sh1=!= "Xi"&&spinorHeadQ[sHead]&&momHeadQ[mHead]):=SpinorChainReduced[sHead[sh3,sq,p3],mHead[p1],sHead[sh4,an,p4]]^(pow-1)(-SpinorChainReduced[sHead[sh4,an,p4],mHead[p1],mHead[p5],sHead[sh2,an,p2]]SpinorChainReduced[sHead[sh3,sq,p3],sHead[sh1,sq,p1]]+SpinorChainReduced[sHead[sh4,an,p4],mHead[p1],sHead[sh1,sq,p1]]SpinorChainReduced[sHead[sh3,sq,p3],mHead[p5],sHead[sh2,an,p2]]);
(*[4|p1|3>^2[1|p3|2> = [4|p1|3>(-<23>[1|p3p1|4]+<2|p1|4][1|p3|3>)*)
SpinorChainReduced[sHead_[sh4_,sq_,p4_],mHead_[p1_],sHead_[sh3_,an_,p3_]]^pow_ SpinorChainReduced[sHead_[sh1_,sq_,p1_],mHead_[p3_],sHead_[sh2_,an_,p2_]]/;(sh3=!= "Xi"&&spinorHeadQ[sHead]&&momHeadQ[mHead]):=SpinorChainReduced[sHead[sh4,sq,p4],mHead[p1],sHead[sh3,an,p3]]^(pow-1)(-SpinorChainReduced[sHead[sh2,an,p2],sHead[sh3,an,p3]]SpinorChainReduced[sHead[sh1,sq,p1],mHead[p3],mHead[p1],sHead[sh4,sq,p4]]+SpinorChainReduced[sHead[sh2,an,p2],mHead[p1],sHead[sh4,sq,p4]]SpinorChainReduced[sHead[sh1,sq,p1],mHead[p3],sHead[sh3,an,p3]]);
(*[4|p1|3>^2[2|p5|1> = [4|p1|3>(-<13>[2|p5p1|4]+<1|p1|4][2|p5|3>)*)
SpinorChainReduced[sHead_[sh4_,sq_,p4_],mHead_[p1_],sHead_[sh3_,an_,p3_]]^pow_ SpinorChainReduced[sHead_[sh2_,sq_,p2_],mHead_[p5_],sHead_[sh1_,an_,p1_]]/;(sh1=!= "Xi"&&spinorHeadQ[sHead]&&momHeadQ[mHead]):=SpinorChainReduced[sHead[sh4,sq,p4],mHead[p1],sHead[sh3,an,p3]]^(pow-1)(-SpinorChainReduced[sHead[sh1,an,p1],sHead[sh3,an,p3]]SpinorChainReduced[sHead[sh2,sq,p2],mHead[p5],mHead[p1],sHead[sh4,sq,p4]]+SpinorChainReduced[sHead[sh1,an,p1],mHead[p1],sHead[sh4,sq,p4]]SpinorChainReduced[sHead[sh2,sq,p2],mHead[p5],sHead[sh3,an,p3]]);
Protect[Times];


(* ::Input::Initialization:: *)
Unprotect[Times];
(*[12][4|p1|3(>^2) = -<3|p1|2][41][4|p1|3>+<3|p1|1][42][4|p1|3>*)
(*              = Nothing better.*)
(*"\[LeftAngleBracket]"12"\[RightAngleBracket]" "["4"|"Subscript[p, 1]"|"3"\[RightAngleBracket]"^2*)
(*<12>[4|p1|3(>^2) = <32>[4|p1|1>[4|p1|3>-<31>[4|p1|2>[4|p1|3>*)
(*              = Nothing better.*)
Protect[Times];


(* ::Input::Initialization:: *)
Unprotect[Times];
(*<12><3|p1p2|3> = <3|p2p1|2><31> - <3|p2p1|1><32>*)
SpinorChainReduced[sHead_[sh1_,an_,p1_],sHead_[sh2_,an_,p2_]]SpinorChainReduced[sHead_[sh3_,an_,p3_],mHead_[p1_],mHead_[p2_],sHead_[sh3_,an_,p3_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=SpinorChainReduced[sHead[sh3,an,p3],mHead[p2],mHead[p1],sHead[sh2,an,p2]]SpinorChainReduced[sHead[sh3,an,p3],sHead[sh1,an,p1]]-SpinorChainReduced[sHead[sh3,an,p3],mHead[p2],mHead[p1],sHead[sh1,an,p1]]SpinorChainReduced[sHead[sh3,an,p3],sHead[sh2,an,p2]];
(*<24><3|p1p2|3> = <34><3|p1p2|2> - <32><3|p1p2|4>*)
(* Coinciding p4 and p3 return the original product after antisymmetry. *)
SpinorChainReduced[sHead_[sh2_,an_,p2_],sHead_[sh4_,an_,p4_]]SpinorChainReduced[sHead_[sh3_,an_,p3_],mHead_[p1_],mHead_[p2_],sHead_[sh3_,an_,p3_]]/;(p4=!=p3&&spinorHeadQ[sHead]&&momHeadQ[mHead]):=SpinorChainReduced[sHead[sh3,an,p3],sHead[sh4,an,p4]]SpinorChainReduced[sHead[sh3,an,p3],mHead[p1],mHead[p2],sHead[sh2,an,p2]]-SpinorChainReduced[sHead[sh3,an,p3],sHead[sh2,an,p2]]SpinorChainReduced[sHead[sh3,an,p3],mHead[p1],mHead[p2],sHead[sh4,an,p4]];
(*[12]<3|p1p2|3><4|p1p2|4> = - <3|p2|2]<3|p1|1]<4|p1p2|4> + <3|p2|1]<3|p1|2]<4|p1p2|4>*)
(*                         = - <3|p2|2]<3|p1|1]<4|p1p2|4> + <3|p2|1](<4|p2|2]<4|p1p1|3> - <4|p2p1|3><4|p1|2])*)
SpinorChainReduced[sHead_[sh1_,sq_,p1_],sHead_[sh2_,sq_,p2_]]SpinorChainReduced[sHead_[sh3_,an_,p3_],mHead_[p1_],mHead_[p2_],sHead_[sh3_,an_,p3_]]SpinorChainReduced[sHead_[sh4_,an_,p4_],mHead_[p1_],mHead_[p2_],sHead_[sh4_,an_,p4_]]/;(sq=!=an&&spinorHeadQ[sHead]&&momHeadQ[mHead]):=-SpinorChainReduced[sHead[sh3,an,p3],mHead[p2],sHead[sh2,sq,p2]]SpinorChainReduced[sHead[sh3,an,p3],mHead[p1],sHead[sh1,sq,p1]]SpinorChainReduced[sHead[sh4,an,p4],mHead[p1],mHead[p2],sHead[sh4,an,p4]]+SpinorChainReduced[sHead[sh3,an,p3],mHead[p2],sHead[sh1,sq,p1]]SpinorChainReduced[sHead[sh4,an,p4],mHead[p2],sHead[sh2,sq,p2]]SpinorChainReduced[sHead[sh4,an,p4],mHead[p1],mHead[p1],sHead[sh3,an,p3]]-SpinorChainReduced[sHead[sh3,an,p3],mHead[p2],sHead[sh1,sq,p1]]SpinorChainReduced[sHead[sh4,an,p4],mHead[p2],mHead[p1],sHead[sh3,an,p3]]SpinorChainReduced[sHead[sh4,an,p4],mHead[p1],sHead[sh2,sq,p2]];
Protect[Times];


(* ::Input::Initialization:: *)
Unprotect[Times];
(*[12][3|p1|4>[5|p2|6> = -<4|p1|2][31][5|p2|6>+<4|p1|1][32][5|p2|6>*)
SpinorChainReduced[sHead_[sh1_,sq_,p1_],sHead_[sh2_,sq_,p2_]]SpinorChainReduced[sHead_[sh3_,sq_,p3_],mHead_[p1_],sHead_[sh4_,an_,p4_]]SpinorChainReduced[sHead_[sh5_,sq_,p5_],mHead_[p2_],sHead_[sh6_,an_,p6_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=-SpinorChainReduced[sHead[sh4,an,p4],mHead[p1],sHead[sh2,sq,p2]]SpinorChainReduced[sHead[sh3,sq,p3],sHead[sh1,sq,p1]]SpinorChainReduced[sHead[sh5,sq,p5],mHead[p2],sHead[sh6,an,p6]]+SpinorChainReduced[sHead[sh4,an,p4],mHead[p1],sHead[sh1,sq,p1]]SpinorChainReduced[sHead[sh3,sq,p3],sHead[sh2,sq,p2]]SpinorChainReduced[sHead[sh5,sq,p5],mHead[p2],sHead[sh6,an,p6]];
(*<12>[3|p1|4>[5|p2|6> = <42>[3|p1|1>[5|p2|6> - <41>[3|p1|2>[5|p2|6>*)
SpinorChainReduced[sHead_[sh1_,an_,p1_],sHead_[sh2_,an_,p2_]]SpinorChainReduced[sHead_[sh3_,sq_,p3_],mHead_[p1_],sHead_[sh4_,an_,p4_]]SpinorChainReduced[sHead_[sh5_,sq_,p5_],mHead_[p2_],sHead_[sh6_,an_,p6_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=
SpinorChainReduced[sHead[sh4,an,p4],sHead[sh2,an,p2]]SpinorChainReduced[sHead[sh3,sq,p3],mHead[p1],sHead[sh1,an,p1]]SpinorChainReduced[sHead[sh5,sq,p5],mHead[p2],sHead[sh6,an,p6]]-SpinorChainReduced[sHead[sh4,an,p4],sHead[sh1,an,p1]]SpinorChainReduced[sHead[sh3,sq,p3],mHead[p1],sHead[sh2,an,p2]]SpinorChainReduced[sHead[sh5,sq,p5],mHead[p2],sHead[sh6,an,p6]];
Protect[Times];


(* ::Input::Initialization:: *)
Unprotect[Times];
(*[1|p3|2><3|p5p6|4> = -[1|p3|2><4|p6p5|3> = <32><4|p6p5p3|1] - <3|p3|1]<4|p6p5|2>*)
SpinorChainReduced[sHead_[sh1_,sq_,p1_],mHead_[p3_],sHead_[sh2_,an_,p2_]]SpinorChainReduced[sHead_[sh3_,an_,p3_],mHead_[p5_],mHead_[p6_],sHead_[sh4_,an_,p4_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=SpinorChainReduced[sHead[sh3,an,p3],sHead[sh2,an,p2]]SpinorChainReduced[sHead[sh4,an,p4],mHead[p6],mHead[p5],mHead[p3],sHead[sh1,sq,p1]]-SpinorChainReduced[sHead[sh3,an,p3],mHead[p3],sHead[sh1,sq,p1]]SpinorChainReduced[sHead[sh4,an,p4],mHead[p6],mHead[p5],sHead[sh2,an,p2]];
(*[1|p4|2><3|p5p6|4> = -<42><3|p5p6p4|1] + <4|p4|1]<3|p5p6|2>*)
SpinorChainReduced[sHead_[sh1_,sq_,p1_],mHead_[p4_],sHead_[sh2_,an_,p2_]]SpinorChainReduced[sHead_[sh3_,an_,p3_],mHead_[p5_],mHead_[p6_],sHead_[sh4_,an_,p4_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=
-SpinorChainReduced[sHead[sh4,an,p4],sHead[sh2,an,p2]]SpinorChainReduced[sHead[sh3,an,p3],mHead[p5],mHead[p6],mHead[p4],sHead[sh1,sq,p1]]+SpinorChainReduced[sHead[sh4,an,p4],mHead[p4],sHead[sh1,sq,p1]]SpinorChainReduced[sHead[sh3,an,p3],mHead[p5],mHead[p6],sHead[sh2,an,p2]];
(*[1|p5p6|2][3|p2|4> = -<4|p2|2][3|p6p5|1]+<4|p2p6p5|1][32]*)
SpinorChainReduced[sHead_[sh1_,sq_,p1_],mHead_[p5_],mHead_[p6_],sHead_[sh2_,sq_,p2_]]SpinorChainReduced[sHead_[sh3_,sq_,p3_],mHead_[p2_],sHead_[sh4_,an_,p4_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=-SpinorChainReduced[sHead[sh4,an,p4],mHead[p2],sHead[sh2,sq,p2]]SpinorChainReduced[sHead[sh3,sq,p3],mHead[p6],mHead[p5],sHead[sh1,sq,p1]]+SpinorChainReduced[sHead[sh4,an,p4],mHead[p2],mHead[p6],mHead[p5],sHead[sh1,sq,p1]]SpinorChainReduced[sHead[sh3,sq,p3],sHead[sh2,sq,p2]];
(*[1|p5p6|2][3|p1|4> = -<4|p1p5p6|2][31]+<4|p1|1][3|p5p6|2]*)
SpinorChainReduced[sHead_[sh1_,sq_,p1_],mHead_[p5_],mHead_[p6_],sHead_[sh2_,sq_,p2_]]SpinorChainReduced[sHead_[sh3_,sq_,p3_],mHead_[p1_],sHead_[sh4_,an_,p4_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=-SpinorChainReduced[sHead[sh4,an,p4],mHead[p1],mHead[p5],mHead[p6],sHead[sh2,sq,p2]]SpinorChainReduced[sHead[sh3,sq,p3],sHead[sh1,sq,p1]]+SpinorChainReduced[sHead[sh4,an,p4],mHead[p1],sHead[sh1,sq,p1]]SpinorChainReduced[sHead[sh3,sq,p3],mHead[p5],mHead[p6],sHead[sh2,sq,p2]];
(*[4|p1|2><3|p1p2|3> = -<32><3|p1p2p1|4]+<3|p1|4]<3|p1p2|2>*)
SpinorChainReduced[sHead_[sh4_,sq_,p4_],mHead_[p1_],sHead_[sh2_,an_,p2_]]SpinorChainReduced[sHead_[sh3_,an_,p3_],mHead_[p1_],mHead_[p2_],sHead_[sh3_,an_,p3_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=-SpinorChainReduced[sHead[sh3,an,p3],sHead[sh2,an,p2]]SpinorChainReduced[sHead[sh3,an,p3],mHead[p1],mHead[p2],mHead[p1],sHead[sh4,sq,p4]]+SpinorChainReduced[sHead[sh3,an,p3],mHead[p1],sHead[sh4,sq,p4]]SpinorChainReduced[sHead[sh3,an,p3],mHead[p1],mHead[p2],sHead[sh2,an,p2]];
(*[4|p3|2>[3|p1p2|3] = -<2|p3|3][4|p2p1|3] + <2|p3p2p1|3][43]*)
SpinorChainReduced[sHead_[sh4_,sq_,p4_],mHead_[p3_],sHead_[sh2_,an_,p2_]]SpinorChainReduced[sHead_[sh3_,sq_,p3_],mHead_[p1_],mHead_[p2_],sHead_[sh3_,sq_,p3_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=-SpinorChainReduced[sHead[sh2,an,p2],mHead[p3],sHead[sh3,sq,p3]]SpinorChainReduced[sHead[sh4,sq,p4],mHead[p2],mHead[p1],sHead[sh3,sq,p3]]+SpinorChainReduced[sHead[sh2,an,p2],mHead[p3],mHead[p2],mHead[p1],sHead[sh3,sq,p3]]SpinorChainReduced[sHead[sh4,sq,p4],sHead[sh3,sq,p3]];
(*[4|p2|1><3|p1p2|3> = -<31><3|p1p2p2|4]+<3|p2|4]<3|p1p2|1>*)
SpinorChainReduced[sHead_[sh4_,sq_,p4_],mHead_[p2_],sHead_[sh1_,an_,p1_]]SpinorChainReduced[sHead_[sh3_,an_,p3_],mHead_[p1_],mHead_[p2_],sHead_[sh3_,an_,p3_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=-SpinorChainReduced[sHead[sh3,an,p3],sHead[sh1,an,p1]]SpinorChainReduced[sHead[sh3,an,p3],mHead[p1],mHead[p2],mHead[p2],sHead[sh4,sq,p4]]+SpinorChainReduced[sHead[sh3,an,p3],mHead[p2],sHead[sh4,sq,p4]]SpinorChainReduced[sHead[sh3,an,p3],mHead[p1],mHead[p2],sHead[sh1,an,p1]];
(*[1|p2|3>[4|p1p2|4]=[4|p2p1p2|3>[41]-[4|p2p1|1][4|p2|3>*)
SpinorChainReduced[sHead_[sh1_,sq_,p1_],mHead_[p2_],sHead_[sh3_,an_,p3_]]SpinorChainReduced[sHead_[sh4_,sq_,p4_],mHead_[p1_],mHead_[p2_],sHead_[sh4_,sq_,p4_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=SpinorChainReduced[sHead[sh4,sq,p4],mHead[p2],mHead[p1],mHead[p2],sHead[sh3,an,p3]]SpinorChainReduced[sHead[sh4,sq,p4],sHead[sh1,sq,p1]]-SpinorChainReduced[sHead[sh4,sq,p4],mHead[p2],mHead[p1],sHead[sh1,sq,p1]]SpinorChainReduced[sHead[sh4,sq,p4],mHead[p2],sHead[sh3,an,p3]];
Protect[Times];
Unprotect[Times];
(*[2|p1|3>[4|p1p2|4]=[4|p2p1p1|3>[42]-[4|p2p1|2][4|p1|3>*)
SpinorChainReduced[sHead_[sh2_,sq_,p2_],mHead_[p1_],sHead_[sh3_,an_,p3_]]SpinorChainReduced[sHead_[sh4_,sq_,p4_],mHead_[p1_],mHead_[p2_],sHead_[sh4_,sq_,p4_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=SpinorChainReduced[sHead[sh4,sq,p4],mHead[p2],mHead[p1],mHead[p1],sHead[sh3,an,p3]]SpinorChainReduced[sHead[sh4,sq,p4],sHead[sh2,sq,p2]]-SpinorChainReduced[sHead[sh4,sq,p4],mHead[p2],mHead[p1],sHead[sh2,sq,p2]]SpinorChainReduced[sHead[sh4,sq,p4],mHead[p1],sHead[sh3,an,p3]];
(*[2|p1|3><4|p1p2|4> = - <4|p2p1|3><4|p1|2] + <4|p2p1p1|2]<43>*)
(* With p3 == p4, reversal turns the first term back into the input. *)
SpinorChainReduced[sHead_[sh2_,sq_,p2_],mHead_[p1_],sHead_[sh3_,an_,p3_]]SpinorChainReduced[sHead_[sh4_,an_,p4_],mHead_[p1_],mHead_[p2_],sHead_[sh4_,an_,p4_]]/;(p3=!=p4&&spinorHeadQ[sHead]&&momHeadQ[mHead]):=-SpinorChainReduced[sHead[sh4,an,p4],mHead[p2],mHead[p1],sHead[sh3,an,p3]]SpinorChainReduced[sHead[sh4,an,p4],mHead[p1],sHead[sh2,sq,p2]]+SpinorChainReduced[sHead[sh4,an,p4],mHead[p2],mHead[p1],mHead[p1],sHead[sh2,sq,p2]]SpinorChainReduced[sHead[sh4,an,p4],sHead[sh3,an,p3]];
Protect[Times];


(* ::Input::Initialization:: *)
Unprotect[Times];
(*[3|p2p4|5]<2|p3p5|4> = -<4|p5|5]<2|p3p4p2|3] + <4|p5p4p2|3]<2|p3|5]*)
SpinorChainReduced[sHead_[sh3_,sq_,p3_],mHead_[p2_],mHead_[p4_],sHead_[sh5_,sq_,p5_]]SpinorChainReduced[sHead_[sh2_,an_,p2_],mHead_[p3_],mHead_[p5_],sHead_[sh4_,an_,p4_]]/;(sq=!=an&&spinorHeadQ[sHead]&&momHeadQ[mHead]):=-SpinorChainReduced[sHead[sh4,an,p4],mHead[p5],sHead[sh5,sq,p5]]SpinorChainReduced[sHead[sh2,an,p2],mHead[p3],mHead[p4],mHead[p2],sHead[sh3,sq,p3]]+SpinorChainReduced[sHead[sh4,an,p4],mHead[p5],mHead[p4],mHead[p2],sHead[sh3,sq,p3]]SpinorChainReduced[sHead[sh2,an,p2],mHead[p3],sHead[sh5,sq,p5]];
(*[3|p1p2|3]<2|p4p3|2> = -<2|p3|3]<2|p4p2p1|3]+<2|p3p2p1|3]<2|p4|3]*)
SpinorChainReduced[sHead_[sh3_,sq_,p3_],mHead_[p1_],mHead_[p2_],sHead_[sh3_,sq_,p3_]]SpinorChainReduced[sHead_[sh2_,an_,p2_],mHead_[p4_],mHead_[p3_],sHead_[sh2_,an_,p2_]]/;(sq=!=an&&spinorHeadQ[sHead]&&momHeadQ[mHead]):=-SpinorChainReduced[sHead[sh2,an,p2],mHead[p3],sHead[sh3,sq,p3]]SpinorChainReduced[sHead[sh2,an,p2],mHead[p4],mHead[p2],mHead[p1],sHead[sh3,sq,p3]]+SpinorChainReduced[sHead[sh2,an,p2],mHead[p3],mHead[p2],mHead[p1],sHead[sh3,sq,p3]]SpinorChainReduced[sHead[sh2,an,p2],mHead[p4],sHead[sh3,sq,p3]];
(*[3|p1p2|3]<4|p1p2|4> = -<4|p2|3]<4|p1p2p1|3]+<4|p2p2p1|3]<4|p1|3]*)
SpinorChainReduced[sHead_[sh3_,sq_,p3_],mHead_[p1_],mHead_[p2_],sHead_[sh3_,sq_,p3_]]SpinorChainReduced[sHead_[sh4_,an_,p4_],mHead_[p1_],mHead_[p2_],sHead_[sh4_,an_,p4_]]/;(sq=!=an&&spinorHeadQ[sHead]&&momHeadQ[mHead]):=-SpinorChainReduced[sHead[sh4,an,p4],mHead[p2],sHead[sh3,sq,p3]]SpinorChainReduced[sHead[sh4,an,p4],mHead[p1],mHead[p2],mHead[p1],sHead[sh3,sq,p3]]+SpinorChainReduced[sHead[sh4,an,p4],mHead[p2],mHead[p2],mHead[p1],sHead[sh3,sq,p3]]SpinorChainReduced[sHead[sh4,an,p4],mHead[p1],sHead[sh3,sq,p3]];
(*[3|p1p2|3][4|p1p2|4]*)
(*SpinorChainReduced[sHead_[sh_,sq_,p3_],mHead_[p1_],mHead_[p2_],sHead_[sh_,sq_,p3_]]SpinorChainReduced[sHead_[sh_,sq_,p4_],mHead_[p1_],mHead_[p2_],sHead_[sh_,sq_,p4_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):>SpinorChainReduced[sHead[sh,sq,p4],sHead[sh,sq,p3]]SpinorChainReduced[sHead[sh,sq,p4],mHead[p1],mHead[p2],mHead[p2],mHead[p1],sHead[sh,sq,p3]]-SpinorChainReduced[sHead[sh,sq,p4],mHead[p2],mHead[p1],sHead[sh,sq,p3]]SpinorChainReduced[sHead[sh,sq,p4],mHead[p1],mHead[p2],sHead[sh,sq,p3]]*)
Protect[Times];


(* ::Input::Initialization:: *)
(*Unprotect[Times];
SpinorChainReduced[Spinor[sh3_,sq_,p3_],Mom[p2_], Mom[p4_], Spinor[sh3_, sq_,p3_]](SpinorChainReduced[Spinor[sh2_,an_, p2_],Spinor[sh4_,an_, p4_]]+SpinorChainReduced[Spinor[sh2_,sq_, p2_],Spinor[sh4_,sq_, p4_]])/;(sq\[Equal]"Square"&&an\[Equal]"Angle"):=-Mass[p4]SpinorChainReduced[Spinor[sh4,sq, p4], Spinor[sh3, sq,p3]]Mass[p2]SpinorChainReduced[Spinor[sh2,sq, p2],Spinor[sh3,sq,p3]]+SpinorChainReduced[Spinor[sh4,an, p4],Mom[p2],Spinor[sh3,sq,p3]]SpinorChainReduced[Spinor[sh2,an, p2], Mom[p4], Spinor[sh3, sq,p3]]+2MomProd[p2,p4]SpinorChainReduced[Spinor[sh4,sq, p4],Spinor[sh3, sq,p3]]SpinorChainReduced[Spinor[sh2,sq, p2],Spinor[sh3,sq,p3]]-Mass[p4]SpinorChainReduced[Spinor[sh4,sq, p4],Mom[p2], Spinor[sh3, sq,p3]]SpinorChainReduced[Spinor[sh2,sq, p2],Spinor[sh3,sq,p3]]-SpinorChainReduced[Spinor[sh4,sq, p4],Spinor[sh3,sq,p3]]Mass[p2]SpinorChainReduced[Spinor[sh2,an, p2] ,Mom[p4], Spinor[sh3, sq,p3]];
Protect[Times];*)


(* ::Input::Initialization:: *)
(*Standard Form, get 1 into first position and 2 into last position if possible*)
(*Unprotect[Times];
(*[13][5|p4|2> = -<2|p4|3][51]+<2|p4|1][53]*)
SpinorChainReduced[Spinor[sh1_,sq_,p1_],Spinor[sh3_,sq_,p3_]]SpinorChainReduced[Spinor[sh5_,sq_,p5_],Mom[p4_],Spinor[sh2_,an_,p2_]]/;(p5>p3&&p5!=p4&&p2!=p4):=-SpinorChainReduced[Spinor[sh2,an,p2],Mom[p4],Spinor[sh3,sq,p3]]SpinorChainReduced[Spinor[sh5,sq,p5],Spinor[sh1,sq,p1]]+SpinorChainReduced[Spinor[sh2,an,p2],Mom[p4],Spinor[sh1,sq,p1]]SpinorChainReduced[Spinor[sh5,sq,p5],Spinor[sh3,sq,p3]];
(*[35][4|p5|1> = -<1|p5|5][43] + <1|p5|3][45]*)
SpinorChainReduced[Spinor[sh3_,sq_,p3_],Spinor[sh5_,sq_,p5_]]SpinorChainReduced[Spinor[sh4_,sq_,p4_],Mom[p5_],Spinor[sh1_,an_,p1_]]/;(p3<p4&&p4!=p5&&p1!=p5):=-SpinorChainReduced[Spinor[sh1,an,p1],Mom[p5],Spinor[sh5,sq,p5]]SpinorChainReduced[Spinor[sh4,sq,p4],Spinor[sh3,sq,p3]]+SpinorChainReduced[Spinor[sh1,an,p1],Mom[p5],Spinor[sh3,sq,p3]]SpinorChainReduced[Spinor[sh4,sq,p4],Spinor[sh5,sq,p5]];
(*<24>[1|p2|1> = <14>[1|p2|2> - <12>[1|p2|4>*)
SpinorChainReduced[Spinor[sh2_,an_,p2_],Spinor[sh4_,an_,p4_]]SpinorChainReduced[Spinor[sh1_,sq_,p1_],Mom[p2_],Spinor[sh1_,an_,p1_]]/;(p2>p1):=SpinorChainReduced[Spinor[sh1,an,p1],Spinor[sh4,an,p4]]SpinorChainReduced[Spinor[sh1,sq,p1],Mom[p2],Spinor[sh2,an,p2]]-SpinorChainReduced[Spinor[sh1,an,p1],Spinor[sh2,an,p2]]SpinorChainReduced[Spinor[sh1,sq,p1],Mom[p2],Spinor[sh4,an,p4]];
(*<24>[1|p4|1> = <14>[1|p4|2> - <12>[1|p4|4>*)
SpinorChainReduced[Spinor[sh2_,an_,p2_],Spinor[sh4_,an_,p4_]]SpinorChainReduced[Spinor[sh1_,sq_,p1_],Mom[p4_],Spinor[sh1_,an_,p1_]]/;(p2>p1):=SpinorChainReduced[Spinor[sh1,an,p1],Spinor[sh4,an,p4]]SpinorChainReduced[Spinor[sh1,sq,p1],Mom[p4],Spinor[sh2,an,p2]]-SpinorChainReduced[Spinor[sh1,an,p1],Spinor[sh2,an,p2]]SpinorChainReduced[Spinor[sh1,sq,p1],Mom[p4],Spinor[sh4,an,p4]];
Protect[Times];*)


(* ::Input::Initialization:: *)
(*Standard Form, get 1 into first position and 2 into last position if possible*)
(*Unprotect[Times];
(*[13][2|p3p4|5] = [5|p4p3|3][21] - [5|p4p3|1][23] = [5|p4p3|3][21] + [1|p3p4|5][23]*)
SpinorChainReduced[Spinor[sh1_,sa_,p1_],Spinor[sh3_,sa_,p3_]]SpinorChainReduced[Spinor[sh2_,sa_,p2_],Mom[p3_],Mom[p4_],Spinor[sh5_,sa_,p5_]]/;(p2>p1&&p2<=p5&&p2!=p3&&p5!=p4&&p3!=p5&&p4!=p2):=SpinorChainReduced[Spinor[sh5,sa,p5],Mom[p4],Mom[p3],Spinor[sh3,sa,p3]]SpinorChainReduced[Spinor[sh2,sa,p2],Spinor[sh1,sa,p1]]+SpinorChainReduced[Spinor[sh5,sa,p1],Mom[p3],Mom[p4],Spinor[sh1,sa,p5]]SpinorChainReduced[Spinor[sh2,sa,p2],Spinor[sh3,sa,p3]];
(*[13][2|p4p3|5] = [5|p3p4|3][21] - [5|p3p4|1][23] = -[5|p4p3|3][21] + [1|p4p3|5][23]*)
SpinorChainReduced[Spinor[sh1_,sa_,p1_],Spinor[sh3_,sa_,p3_]]SpinorChainReduced[Spinor[sh2_,sa_,p2_],Mom[p4_],Mom[p3_],Spinor[sh5_,sa_,p5_]]/;(p2>p1&&p2<=p5&&p2!=p4&&p5!=p3&&p5!=p4&&p2!=p3):=-SpinorChainReduced[Spinor[sh5,sa,p5],Mom[p4],Mom[p3],Spinor[sh3,sa,p3]]SpinorChainReduced[Spinor[sh2,sa,p2],Spinor[sh1,sa,p1]]+SpinorChainReduced[Spinor[sh5,sa,p1],Mom[p4],Mom[p3],Spinor[sh1,sa,p5]]SpinorChainReduced[Spinor[sh2,sa,p2],Spinor[sh3,sa,p3]];
(*[23][1|p3p4|5] = [53][1|p3p4|2] - [52][1|p3p4|3]*)
SpinorChainReduced[Spinor[sh2_,sa_,p2_],Spinor[sh3_,sa_,p3_]]SpinorChainReduced[Spinor[sh1_,sa_,p1_],Mom[p3_],Mom[p4_],Spinor[sh5_,sa_,p5_]]/;((p2<p5||p5==1)&&p2>p1&&p1<=p5&&p1!=p4&&p5!=p3&&p5!=p4&&p1!=p3):=
SpinorChainReduced[Spinor[sh5,sa,p5],Spinor[sh3,sa,p3]]SpinorChainReduced[Spinor[sh1,sa,p1],Mom[p3],Mom[p4],Spinor[sh2,sa,p2]]-SpinorChainReduced[Spinor[sh5,sa,p5],Spinor[sh2,sa,p2]]SpinorChainReduced[Spinor[sh1,sa,p1],Mom[p3],Mom[p4],Spinor[sh3,sa,p3]];
(*[23][1|p4p3|5] = [53][1|p4p3|2] - [52][1|p4p3|3]*)
SpinorChainReduced[Spinor[sh2_,sa_,p2_],Spinor[sh3_,sa_,p3_]]SpinorChainReduced[Spinor[sh1_,sa_,p1_],Mom[p4_],Mom[p3_],Spinor[sh5_,sa_,p5_]]/;((p2<p5||p5==1)&&p2>p1&&p1<=p5&&p1!=p4&&p5!=p3&&p5!=p4&&p1!=p3):=
SpinorChainReduced[Spinor[sh5,sa,p5],Spinor[sh3,sa,p3]]SpinorChainReduced[Spinor[sh1,sa,p1],Mom[p4],Mom[p3],Spinor[sh2,sa,p2]]-SpinorChainReduced[Spinor[sh5,sa,p5],Spinor[sh2,sa,p2]]SpinorChainReduced[Spinor[sh1,sa,p1],Mom[p4],Mom[p3],Spinor[sh3,sa,p3]];
(*[23][1|p2p4|5] = [53][1|p2p4|2] - [52][1|p2p4|3] *)
SpinorChainReduced[Spinor[sh2_,sa_,p2_],Spinor[sh3_,sa_,p3_]]SpinorChainReduced[Spinor[sh1_,sa_,p1_],Mom[p2_],Mom[p4_],Spinor[sh5_,sa_,p5_]]/;(p5!=p1&&p5>p3&&p3>p1&&p2>p1&&p1!=p4&&p1!=p2&&p5!=p2&&p5!=p4):=
SpinorChainReduced[Spinor[sh5,sa,p5],Spinor[sh3,sa,p3]]SpinorChainReduced[Spinor[sh1,sa,p1],Mom[p2],Mom[p4],Spinor[sh2,sa,p2]]-SpinorChainReduced[Spinor[sh5,sa,p5],Spinor[sh2,sa,p2]]SpinorChainReduced[Spinor[sh1,sa,p1],Mom[p2],Mom[p4],Spinor[sh3,sa,p3]];
(*[23][1|p4p2|5] = [53][1|p4p2|2] - [52][1|p4p2|3] *)
SpinorChainReduced[Spinor[sh2_,sa_,p2_],Spinor[sh3_,sa_,p3_]]SpinorChainReduced[Spinor[sh1_,sa_,p1_],Mom[p4_],Mom[p2_],Spinor[sh5_,sa_,p5_]]/;(p5!=p1&&p5>p3&&p3>p1&&p2>p1&&p1!=p4&&p1!=p2&&p5!=p2&&p5!=p4):=
SpinorChainReduced[Spinor[sh5,sa,p5],Spinor[sh3,sa,p3]]SpinorChainReduced[Spinor[sh1,sa,p1],Mom[p4],Mom[p2],Spinor[sh2,sa,p2]]-SpinorChainReduced[Spinor[sh5,sa,p5],Spinor[sh2,sa,p2]]SpinorChainReduced[Spinor[sh1,sa,p1],Mom[p4],Mom[p2],Spinor[sh3,sa,p3]];
(*[23][1|p2p4|1] = [13][1|p2p4|2] - [12][1|p2p4|3]*)
SpinorChainReduced[Spinor[sh2_,sa_,p2_],Spinor[sh3_,sa_,p3_]]SpinorChainReduced[Spinor[sh1_,sa_,p1_],Mom[p2_],Mom[p4_],Spinor[sh1_,sa_,p1_]]/;(p3>p1&&p1!=p4&&p1!=p2):=
SpinorChainReduced[Spinor[sh1,sa,p1],Spinor[sh3,sa,p3]]SpinorChainReduced[Spinor[sh1,sa,p1],Mom[p2],Mom[p4],Spinor[sh2,sa,p2]]-SpinorChainReduced[Spinor[sh1,sa,p1],Spinor[sh2,sa,p2]]SpinorChainReduced[Spinor[sh1,sa,p1],Mom[p2],Mom[p4],Spinor[sh3,sa,p3]];
(*[23][1|p4p2|1] = [13][1|p4p2|2] - [12][1|p4p2|3]*)
SpinorChainReduced[Spinor[sh2_,sa_,p2_],Spinor[sh3_,sa_,p3_]]SpinorChainReduced[Spinor[sh1_,sa_,p1_],Mom[p4_],Mom[p2_],Spinor[sh1_,sa_,p1_]]/;(p3>p1&&p1!=p4&&p1!=p2):=
SpinorChainReduced[Spinor[sh1,sa,p1],Spinor[sh3,sa,p3]]SpinorChainReduced[Spinor[sh1,sa,p1],Mom[p4],Mom[p2],Spinor[sh2,sa,p2]]-SpinorChainReduced[Spinor[sh1,sa,p1],Spinor[sh2,sa,p2]]SpinorChainReduced[Spinor[sh1,sa,p1],Mom[p4],Mom[p2],Spinor[sh3,sa,p3]];
Protect[Times];*)


(*Standard form only has 1 momentum per spinor chain*)
(*<12>[4|p5p6|4]=-[4|p6|2>[4|p5|1>+[4|p6|1>[4|p5|2>*)
(*Unprotect[Times];
SpinorChainReduced[Spinor[sh1_,an_,p1_],Spinor[sh2_,an_,p2_]]SpinorChainReduced[Spinor[sh4_,sq_,p4_], Mom[p5_],Mom[p6_], Spinor[sh4_,sq_,p4_]]/;(p2!=p4&&p1!=p4&&p5!=p4&&p6!=p4&&an!=sq):=
-SpinorChainReduced[Spinor[sh4,sq,p4], Mom[p6],Spinor[sh2,an,p2]]SpinorChainReduced[Spinor[sh4,sq,p4], Mom[p5],Spinor[sh1,an,p1]]+
SpinorChainReduced[Spinor[sh4,sq,p4], Mom[p6],Spinor[sh1,an,p1]]SpinorChainReduced[Spinor[sh4,sq,p4], Mom[p5],Spinor[sh2,an,p2]];
Protect[Times];*)


(*This needs to be replaced with a standard form*)
(*Unprotect[Times];
(*[23][2|p1|4>[1|p3|4>*)
SpinorChainReduced[Spinor[sh2_, sq_, p2_],Spinor[sh3_, sq_, p3_]]SpinorChainReduced[Spinor[sh2_, sq_, p2_],Mom[p1_],Spinor[sh4_, an_, p4_]]SpinorChainReduced[Spinor[sh1_, sq_, p1_],Mom[p3_],Spinor[sh4_, an_, p4_]]:=
SpinorChainReduced[Spinor[sh2, sq, p2],Mom[p1],Spinor[sh4, an, p4]](-SpinorChainReduced[Spinor[sh4, an, p4],Mom[p3],Spinor[sh3, sq, p3]]SpinorChainReduced[Spinor[sh1, sq, p1],Spinor[sh2, sq, p2]]+
SpinorChainReduced[Spinor[sh4, an, p4],Mom[p3],Spinor[sh2, sq, p2]]SpinorChainReduced[Spinor[sh1, sq, p1],Spinor[sh3, sq, p3]]);
Protect[Times];*)


(* ::Input::Initialization:: *)
Unprotect[Plus];
(*<23><1\[Zeta]2>-<12><\[Zeta]23> = -<\[Zeta]22><13>*)
SpinorChainReduced[sHead_[sh2_,tp_,p2_],sHead_[sh3_,tp_,p3_]]SpinorChainReduced[sHead_[sh1_,tp_,p1_],sHead_["Zeta",tp_,p2_]]-SpinorChainReduced[sHead_[sh1_,tp_,p1_],sHead_[sh2_,tp_,p2_]]SpinorChainReduced[sHead_["Zeta",tp_,p2_],sHead_[sh3_,tp_,p3_]]/;(spinorHeadQ[sHead]):=-SpinorChainReduced[sHead["Zeta",tp,p2],sHead[sh2,tp,p2]]SpinorChainReduced[sHead[sh1,tp,p1],sHead[sh3,tp,p3]];
Protect[Plus];


(* ::Input::Initialization:: *)
Unprotect[Times];
(*<1\[Zeta]2>[2|p3|2>\[Rule]<2\[Zeta]2>[2|p3|1>-<21>[2|p3|\[Zeta]2>*)
SpinorChainReduced[sHead_[sh1_,"Angle",p1_],sHead_["Zeta","Angle",p2_]]SpinorChainReduced[sHead_["Helicity","Square",p2_],mHead_[p3_],sHead_["Helicity","Angle",p2_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=SpinorChainReduced[sHead["Helicity","Angle",p2],sHead["Zeta","Angle",p2]]SpinorChainReduced[sHead["Helicity","Square",p2],mHead[p3],sHead[sh1,"Angle",p1]]-SpinorChainReduced[sHead["Helicity","Angle",p2],sHead[sh1,"Angle",p1]]SpinorChainReduced[sHead["Helicity","Square",p2],mHead[p3],sHead["Zeta","Angle",p2]];
SpinorChainReduced[sHead_["Zeta","Angle",p2_],sHead_[sh1_,"Angle",p1_]]SpinorChainReduced[sHead_["Helicity","Square",p2_],mHead_[p3_],sHead_["Helicity","Angle",p2_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=-SpinorChainReduced[sHead["Helicity","Angle",p2],sHead["Zeta","Angle",p2]]SpinorChainReduced[sHead["Helicity","Square",p2],mHead[p3],sHead[sh1,"Angle",p1]]+SpinorChainReduced[sHead["Helicity","Angle",p2],sHead[sh1,"Angle",p1]]SpinorChainReduced[sHead["Helicity","Square",p2],mHead[p3],sHead["Zeta","Angle",p2]];
(*[1\[Zeta]2][2|p3|2>\[Rule][2\[Zeta]2]<2|p3|1]-[21]<2|p3|\[Zeta]2]*)
SpinorChainReduced[sHead_[sh1_,"Square",p1_],sHead_["Zeta","Square",p2_]]SpinorChainReduced[sHead_["Helicity","Square",p2_],mHead_[p3_],sHead_["Helicity","Angle",p2_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=SpinorChainReduced[sHead["Helicity","Square",p2],sHead["Zeta","Square",p2]]SpinorChainReduced[sHead["Helicity","Angle",p2],mHead[p3],sHead[sh1,"Square",p1]]-SpinorChainReduced[sHead["Helicity","Square",p2],sHead[sh1,"Square",p1]]SpinorChainReduced[sHead["Helicity","Angle",p2],mHead[p3],sHead["Zeta","Square",p2]];
SpinorChainReduced[sHead_["Zeta","Square",p2_],sHead_[sh1_,"Square",p1_]]SpinorChainReduced[sHead_["Helicity","Square",p2_],mHead_[p3_],sHead_["Helicity","Angle",p2_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=SpinorChainReduced[sHead["Helicity","Angle",p2],mHead[p3],sHead["Zeta","Square",p2]]SpinorChainReduced[sHead["Helicity","Square",p2],sHead[sh1,"Square",p1]]-SpinorChainReduced[sHead["Helicity","Angle",p2],mHead[p3],sHead[sh1,"Square",p1]]SpinorChainReduced[sHead["Helicity","Square",p2],sHead["Zeta","Square",p2]];
(*[13][4|p1|\[Zeta]3> = -<\[Zeta]3|p1|3][41] + <\[Zeta]3|p1|1][43]*)
SpinorChainReduced[sHead_[sh1_,"Square",p1_],sHead_["Helicity","Square",p3_]]SpinorChainReduced[sHead_[sh4_,"Square",p4_],mHead_[p1_],sHead_["Zeta","Angle",p3_]]/;(p4!=p3&&spinorHeadQ[sHead]&&momHeadQ[mHead]):=-SpinorChainReduced[sHead["Zeta","Angle",p3],mHead[p1],sHead["Helicity","Square",p3]]SpinorChainReduced[sHead[sh4,"Square",p4],sHead[sh1,"Square",p1]]+SpinorChainReduced[sHead["Zeta","Angle",p3],mHead[p1],sHead[sh1,"Square",p1]]SpinorChainReduced[sHead[sh4,"Square",p4],sHead["Helicity","Square",p3]];
SpinorChainReduced[sHead_["Helicity","Square",p3_],sHead_[sh1_,"Square",p1_]]SpinorChainReduced[sHead_[sh4_,"Square",p4_],mHead_[p1_],sHead_["Zeta","Angle",p3_]]/;(p4!=p3&&spinorHeadQ[sHead]&&momHeadQ[mHead]):=SpinorChainReduced[sHead["Zeta","Angle",p3],mHead[p1],sHead["Helicity","Square",p3]]SpinorChainReduced[sHead[sh4,"Square",p4],sHead[sh1,"Square",p1]]-SpinorChainReduced[sHead["Zeta","Angle",p3],mHead[p1],sHead[sh1,"Square",p1]]SpinorChainReduced[sHead[sh4,"Square",p4],sHead["Helicity","Square",p3]];
(*<13>[\[Zeta]3|p1|4> = <43>[\[Zeta]3|p1|1> - <41>[\[Zeta]3|p1|3>*)
SpinorChainReduced[sHead_[sh1_,"Angle",p1_],sHead_["Helicity","Angle",p3_]]SpinorChainReduced[sHead_["Zeta","Square",p3_],mHead_[p1_],sHead_[sh4_,"Angle",p4_]]/;(p4!=p3&&spinorHeadQ[sHead]&&momHeadQ[mHead]):=SpinorChainReduced[sHead[sh4,"Angle",p4],sHead["Helicity","Angle",p3]]SpinorChainReduced[sHead["Zeta","Square",p3],mHead[p1],sHead[sh1,"Angle",p1]]-SpinorChainReduced[sHead[sh4,"Angle",p4],sHead[sh1,"Angle",p1]]SpinorChainReduced[sHead["Zeta","Square",p3],mHead[p1],sHead["Helicity","Angle",p3]];
SpinorChainReduced[sHead_["Helicity","Angle",p3_],sHead_[sh1_,"Angle",p1_]]SpinorChainReduced[sHead_["Zeta","Square",p3_],mHead_[p1_],sHead_[sh4_,"Angle",p4_]]/;(p4!=p3&&spinorHeadQ[sHead]&&momHeadQ[mHead]):=-SpinorChainReduced[sHead[sh4,"Angle",p4],sHead["Helicity","Angle",p3]]SpinorChainReduced[sHead["Zeta","Square",p3],mHead[p1],sHead[sh1,"Angle",p1]]+SpinorChainReduced[sHead[sh4,"Angle",p4],sHead[sh1,"Angle",p1]]SpinorChainReduced[sHead["Zeta","Square",p3],mHead[p1],sHead["Helicity","Angle",p3]];
(*<1\[Zeta]3>[3|p1|4> = <4\[Zeta]3>[3|p1|1> - <41>[3|p1|\[Zeta]3>*)
SpinorChainReduced[sHead_[sp1_,"Angle",p1_],sHead_["Zeta","Angle",p3_]]SpinorChainReduced[sHead_["Helicity","Square",p3_],mHead_[p1_],sHead_[sh4_,"Angle",p4_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=SpinorChainReduced[sHead[sh4,"Angle",p4],sHead["Zeta","Angle",p3]]SpinorChainReduced[sHead["Helicity","Square",p3],mHead[p1],sHead[sp1,"Angle",p1]]-SpinorChainReduced[sHead[sh4,"Angle",p4],sHead[sp1,"Angle",p1]]SpinorChainReduced[sHead["Helicity","Square",p3],mHead[p1],sHead["Zeta","Angle",p3]];
SpinorChainReduced[sHead_["Zeta","Angle",p3_],sHead_[sp1_,"Angle",p1_]]SpinorChainReduced[sHead_["Helicity","Square",p3_],mHead_[p1_],sHead_[sh4_,"Angle",p4_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=-SpinorChainReduced[sHead[sh4,"Angle",p4],sHead["Zeta","Angle",p3]]SpinorChainReduced[sHead["Helicity","Square",p3],mHead[p1],sHead[sp1,"Angle",p1]]+SpinorChainReduced[sHead[sh4,"Angle",p4],sHead[sp1,"Angle",p1]]SpinorChainReduced[sHead["Helicity","Square",p3],mHead[p1],sHead["Zeta","Angle",p3]];
(*[1\[Zeta]3][4|p1|3> = -<3|p1|\[Zeta]3][41] + <3|p1|1][4\[Zeta]3]*)
SpinorChainReduced[sHead_[sp1_,"Square",p1_],sHead_["Zeta","Square",p3_]]SpinorChainReduced[sHead_[sh4_,"Square",p4_],mHead_[p1_],sHead_["Helicity","Angle",p3_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=-SpinorChainReduced[sHead["Helicity","Angle",p3],mHead[p1],sHead["Zeta","Square",p3]]SpinorChainReduced[sHead[sh4,"Square",p4],sHead[sp1,"Square",p1]]+SpinorChainReduced[sHead["Helicity","Angle",p3],mHead[p1],sHead[sp1,"Square",p1]]SpinorChainReduced[sHead[sh4,"Square",p4],sHead["Zeta","Square",p3]];
SpinorChainReduced[sHead_["Zeta","Square",p3_],sHead_[sp1_,"Square",p1_]]SpinorChainReduced[sHead_[sh4_,"Square",p4_],mHead_[p1_],sHead_["Helicity","Angle",p3_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=SpinorChainReduced[sHead["Helicity","Angle",p3],mHead[p1],sHead["Zeta","Square",p3]]SpinorChainReduced[sHead[sh4,"Square",p4],sHead[sp1,"Square",p1]]-SpinorChainReduced[sHead["Helicity","Angle",p3],mHead[p1],sHead[sp1,"Square",p1]]SpinorChainReduced[sHead[sh4,"Square",p4],sHead["Zeta","Square",p3]];
(*[4|p1|4>[3|p1|\[Zeta]4> = -<\[Zeta]44>[3|p1p1|4] + <\[Zeta]4|p1|4][3|p1|4>*)
SpinorChainReduced[sHead_["Helicity","Square",p4_],mHead_[p1_],sHead_["Helicity","Angle",p4_]]SpinorChainReduced[sHead_[sh3_,"Square",p3_],mHead_[p1_],sHead_["Zeta","Angle",p4_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=-SpinorChainReduced[sHead["Zeta","Angle",p4],sHead["Helicity","Angle",p4]]SpinorChainReduced[sHead[sh3,"Square",p3],mHead[p1],mHead[p1],sHead["Helicity","Square",p4]]+SpinorChainReduced[sHead["Zeta","Angle",p4],mHead[p1],sHead["Helicity","Square",p4]]SpinorChainReduced[sHead[sh3,"Square",p3],mHead[p1],sHead["Helicity","Angle",p4]];
(*[4|p1|4>[\[Zeta]4|p1|3> = -<34>[\[Zeta]4|p1p1|4] + <3|p1|4][\[Zeta]4|p1|4>*)
SpinorChainReduced[sHead_["Helicity","Square",p4_],mHead_[p1_],sHead_["Helicity","Angle",p4_]]SpinorChainReduced[sHead_["Zeta","Square",p4_],mHead_[p1_],sHead_[sh3_,"Angle",p3_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=-SpinorChainReduced[sHead[sh3,"Angle",p3],sHead["Helicity","Angle",p4]]SpinorChainReduced[sHead["Zeta","Square",p4],mHead[p1],mHead[p1],sHead["Helicity","Square",p4]]+SpinorChainReduced[sHead[sh3,"Angle",p3],mHead[p1],sHead["Helicity","Square",p4]]SpinorChainReduced[sHead["Zeta","Square",p4],mHead[p1],sHead["Helicity","Angle",p4]];
(*<\[Zeta]23>[2|p1p4|2] = -[2|p4|3>[2|p1|\[Zeta]2>+[2|p4|\[Zeta]2>[2|p1|3>*)
(*SpinorChainReduced[sHead_["Zeta",an_,p2_],sHead_[sh3_,an_,p3_]]SpinorChainReduced[sHead_["Helicity",sq_,p2_],mHead_[p1_],mHead_[p4_],sHead_["Helicity",sq_,p2_]]/;(sq\[NotEqual]an&&spinorHeadQ[sHead]&&momHeadQ[mHead]):=-SpinorChainReduced[sHead["Helicity",sq,p2],mHead[p4],sHead[sh3,an,p3]]SpinorChainReduced[sHead["Helicity",sq,p2],mHead[p1],sHead["Zeta",an,p2]]+SpinorChainReduced[sHead["Helicity",sq,p2],mHead[p4],sHead["Zeta",an,p2]]SpinorChainReduced[sHead["Helicity",sq,p2],mHead[p1],sHead[sh3,an,p3]];
SpinorChainReduced[sHead_[sh3_,an_,p3_],sHead_["Zeta",an_,p2_]]SpinorChainReduced[sHead_["Helicity",sq_,p2_],mHead_[p1_],mHead_[p4_],sHead_["Helicity",sq_,p2_]]/;(sq\[NotEqual]an&&spinorHeadQ[sHead]&&momHeadQ[mHead]):=SpinorChainReduced[sHead["Helicity",sq,p2],mHead[p4],sHead[sh3,an,p3]]SpinorChainReduced[sHead["Helicity",sq,p2],mHead[p1],sHead["Zeta",an,p2]]-SpinorChainReduced[sHead["Helicity",sq,p2],mHead[p4],sHead["Zeta",an,p2]]SpinorChainReduced[sHead["Helicity",sq,p2],mHead[p1],sHead[sh3,an,p3]];*)
(*[1\[Zeta]4][3|p1|\[Zeta]4> = -<\[Zeta]4|p1|\[Zeta]4][31]+<\[Zeta]4|p1|1][3\[Zeta]4]*)
SpinorChainReduced[sHead_[sh1_,"Square",p1_],sHead_["Zeta","Square",p4_]]SpinorChainReduced[sHead_[sh3_,"Square",p3_],mHead_[p1_],sHead_["Zeta","Angle",p4_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=-SpinorChainReduced[sHead["Zeta","Angle",p4],mHead[p1],sHead["Zeta","Square",p4]]SpinorChainReduced[sHead[sh3,"Square",p3],sHead[sh1,"Square",p1]]+SpinorChainReduced[sHead["Zeta","Angle",p4],mHead[p1],sHead[sh1,"Square",p1]]SpinorChainReduced[sHead[sh3,"Square",p3],sHead["Zeta","Square",p4]];
SpinorChainReduced[sHead_["Zeta","Square",p4_],sHead_[sh1_,"Square",p1_]]SpinorChainReduced[sHead_[sh3_,"Square",p3_],mHead_[p1_],sHead_["Zeta","Angle",p4_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=SpinorChainReduced[sHead["Zeta","Angle",p4],mHead[p1],sHead["Zeta","Square",p4]]SpinorChainReduced[sHead[sh3,"Square",p3],sHead[sh1,"Square",p1]]-SpinorChainReduced[sHead["Zeta","Angle",p4],mHead[p1],sHead[sh1,"Square",p1]]SpinorChainReduced[sHead[sh3,"Square",p3],sHead["Zeta","Square",p4]];
(*<1\[Zeta]4>[\[Zeta]4|p1|3> = <3\[Zeta]4>[\[Zeta]4|p1|1> - <31>[\[Zeta]4|p1|\[Zeta]4>*)
SpinorChainReduced[sHead_[sh1_,"Angle",p1_],sHead_["Zeta","Angle",p4_]]SpinorChainReduced[sHead_["Zeta","Square",p4_],mHead_[p1_],sHead_[sh3_,"Angle",p3_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=SpinorChainReduced[sHead[sh3,"Angle",p3],sHead["Zeta","Angle",p4]]SpinorChainReduced[sHead["Zeta","Square",p4],mHead[p1],sHead[sh1,"Angle",p1]]-SpinorChainReduced[sHead[sh3,"Angle",p3],sHead[sh1,"Angle",p1]]SpinorChainReduced[sHead["Zeta","Square",p4],mHead[p1],sHead["Zeta","Angle",p4]];
SpinorChainReduced[sHead_["Zeta","Angle",p4_],sHead_[sh1_,"Angle",p1_]]SpinorChainReduced[sHead_["Zeta","Square",p4_],mHead_[p1_],sHead_[sh3_,"Angle",p3_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=-SpinorChainReduced[sHead[sh3,"Angle",p3],sHead["Zeta","Angle",p4]]SpinorChainReduced[sHead["Zeta","Square",p4],mHead[p1],sHead[sh1,"Angle",p1]]+SpinorChainReduced[sHead[sh3,"Angle",p3],sHead[sh1,"Angle",p1]]SpinorChainReduced[sHead["Zeta","Square",p4],mHead[p1],sHead["Zeta","Angle",p4]];
(*[2\[Zeta]3][3|p1|\[Zeta]3> = -<\[Zeta]3|p1|\[Zeta]3][32] + <\[Zeta]3|p1|2][3\[Zeta]3]*)
SpinorChainReduced[sHead_[sh2_,"Square",p2_],sHead_["Zeta","Square",p3_]]SpinorChainReduced[sHead_["Helicity","Square",p3_],mHead_[p1_],sHead_["Zeta","Angle",p3_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=-SpinorChainReduced[sHead["Zeta","Angle",p3],mHead[p1],sHead["Zeta","Square",p3]]SpinorChainReduced[sHead["Helicity","Square",p3],sHead[sh2,"Square",p2]]+SpinorChainReduced[sHead["Zeta","Angle",p3],mHead[p1],sHead[sh2,"Square",p2]]SpinorChainReduced[sHead["Helicity","Square",p3],sHead["Zeta","Square",p3]];
SpinorChainReduced[sHead_["Zeta","Square",p3_],sHead_[sh2_,"Square",p2_]]SpinorChainReduced[sHead_["Helicity","Square",p3_],mHead_[p1_],sHead_["Zeta","Angle",p3_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=SpinorChainReduced[sHead["Zeta","Angle",p3],mHead[p1],sHead["Zeta","Square",p3]]SpinorChainReduced[sHead["Helicity","Square",p3],sHead[sh2,"Square",p2]]-SpinorChainReduced[sHead["Zeta","Angle",p3],mHead[p1],sHead[sh2,"Square",p2]]SpinorChainReduced[sHead["Helicity","Square",p3],sHead["Zeta","Square",p3]];
(*[3|p1|\[Zeta]4>[\[Zeta]4|p2|4> = -<4\[Zeta]4>[\[Zeta]4|p2p1|3] + <4|p1|3][\[Zeta]4|p2|\[Zeta]4>*)
SpinorChainReduced[sHead_[sh3_,"Square",p3_],mHead_[p1_],sHead_["Zeta","Angle",p4_]]SpinorChainReduced[sHead_["Zeta","Square",p4_],mHead_[p2_],sHead_["Helicity","Angle",p4_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=-SpinorChainReduced[sHead["Helicity","Angle",p4],sHead["Zeta","Angle",p4]]SpinorChainReduced[sHead["Zeta","Square",p4],mHead[p2],mHead[p1],sHead[sh3,"Square",p3]]+SpinorChainReduced[sHead["Helicity","Angle",p4],mHead[p1],sHead[sh3,"Square",p3]]SpinorChainReduced[sHead["Zeta","Square",p4],mHead[p2],sHead["Zeta","Angle",p4]];
Protect[Times];

(*Test*)
(*ReduceSpinorProducts[SpinorChain[Spinor["Spin","Angle",1],Spinor["Zeta","Angle",2]]SpinorChain[Spinor["Helicity","Square",2],Mom[1],Spinor["Helicity","Angle",2]]]
ReduceSpinorProducts[SpinorChain[Spinor["Spin","Square",3],Spinor["Zeta","Square",2]]SpinorChain[Spinor["Helicity","Square",2],Mom[1],Spinor["Helicity","Angle",2]]]*)


(* ::Input::Initialization:: *)
Unprotect[Times];
Unprotect[Plus];
(*[24][1Subscript[\[Zeta], 4]]+[14][2Subscript[\[Zeta], 4]] = [Subscript[\[Zeta], 4]4][12]+2[14][2Subscript[\[Zeta], 4]]*)
(*<24><1Subscript[\[Zeta], 4]>+<14><2Subscript[\[Zeta], 4]> = <Subscript[\[Zeta], 4]4><12>+2<14><2Subscript[\[Zeta], 4]>*)
SpinorChainReduced[sHead_[sh2_,sa_,p2_],sHead_[sh4_,sa_,p4_]]SpinorChainReduced[sHead_[sh1_,sa_,p1_],sHead_["Zeta",sa_,p4_]]+
SpinorChainReduced[sHead_[sh1_,sa_,p1_],sHead_[sh4_,sa_,p4_]]SpinorChainReduced[sHead_[sh2_,sa_,p2_],sHead_["Zeta",sa_,p4_]]/;(spinorHeadQ[sHead]):=SpinorChainReduced[sHead["Zeta",sa,p4],sHead[sh4,sa,p4]]SpinorChainReduced[sHead[sh1,sa,p1],sHead[sh2,sa,p2]]+2SpinorChainReduced[sHead[sh1,sa,p1],sHead[sh4,sa,p4]]SpinorChainReduced[sHead[sh2,sa,p2],sHead["Zeta",sa,p4]];
(*<34><1\[Zeta]3>+<13><\[Zeta]34> = 2<\[Zeta]34><13> - <\[Zeta]33><14>*)
SpinorChainReduced[sHead_[sh3_,sa_,p3_],sHead_[sh4_,sa_,p4_]]SpinorChainReduced[sHead_[sh1_,sa_,p1_],sHead_["Zeta",sa_,p3_]]+
SpinorChainReduced[sHead_[sh1_,sa_,p1_],sHead_[sh3_,sa_,p3_]]SpinorChainReduced[sHead_["Zeta",sa_,p3_],sHead_[sh4_,sa_,p4_]]/;(spinorHeadQ[sHead]):=2SpinorChainReduced[sHead["Zeta",sa,p3],sHead[sh4,sa,p4]]SpinorChainReduced[sHead[sh1,sa,p1],sHead[sh3,sa,p3]]-SpinorChainReduced[sHead["Zeta",sa,p3],sHead[sh3,sa,p3]]SpinorChainReduced[sHead[sh1,sa,p1],sHead[sh4,sa,p4]];
(*[1Subscript[\[Zeta], 4]][4|p5|2>+[14][Subscript[\[Zeta], 4]|p5|2> = <2|p5|1][4Subscript[\[Zeta], 4]] + 2[14][Subscript[\[Zeta], 4]|p5|2>*)
SpinorChainReduced[sHead_[sh1_,"Square",p1_],sHead_["Zeta","Square",p4_]]SpinorChainReduced[sHead_[sh4_,"Square",p4_],mHead_[p5_],sHead_[sh2_,"Angle",p2_]]+
SpinorChainReduced[sHead_[sh1_,"Square",p1_],sHead_[sh4_,"Square",p4_]]SpinorChainReduced[sHead_["Zeta","Square",p4_],mHead_[p5_],sHead_[sh2_,"Angle",p2_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=SpinorChainReduced[sHead[sh2,"Angle",p2],mHead[p5],sHead[sh1,"Square",p1]]SpinorChainReduced[sHead[sh4,"Square",p4],sHead["Zeta","Square",p4]]+2SpinorChainReduced[sHead[sh1,"Square",p1],sHead[sh4,"Square",p4]]SpinorChainReduced[sHead["Zeta","Square",p4],mHead[p5],sHead[sh2,"Angle",p2]];
(*<1\[Zeta]4>[2|p5|4>+<14>[2|p5|\[Zeta]4> = <4\[Zeta]4>[2|p5|1>+2<14>[2|p5|\[Zeta]4>*)
SpinorChainReduced[sHead_[sh1_,"Angle",p1_],sHead_["Zeta","Angle",p4_]]SpinorChainReduced[sHead_[sh2_,"Square",p2_],mHead_[p5_],sHead_[sh4_,"Angle",p4_]]+
SpinorChainReduced[sHead_[sh1_,"Angle",p1_],sHead_[sh4_,"Angle",p4_]]SpinorChainReduced[sHead_[sh2_,"Square",p2_],mHead_[p5_],sHead_["Zeta","Angle",p4_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=SpinorChainReduced[sHead[sh4,"Angle",p4],sHead["Zeta","Angle",p4]]SpinorChainReduced[sHead[sh2,"Square",p2],mHead[p5],sHead[sh1,"Angle",p1]]+2SpinorChainReduced[sHead[sh1,"Angle",p1],sHead[sh4,"Angle",p4]]SpinorChainReduced[sHead[sh2,"Square",p2],mHead[p5],sHead["Zeta","Angle",p4]];
(*<13>[2|p1|\[Zeta]3>-<1\[Zeta]3>[2|p1|3> = <\[Zeta]33>[2|p1|1>*)
SpinorChainReduced[sHead_[sh1_,"Angle",p1_],sHead_["Helicity","Angle",p3_]]SpinorChainReduced[sHead_[sh2_,"Square",p2_],mHead_[p5_],sHead_["Zeta","Angle",p3_]]-SpinorChainReduced[sHead_[sh1_,"Angle",p1_],sHead_["Zeta","Angle",p3_]]SpinorChainReduced[sHead_[sh2_,"Square",p2_],mHead_[p5_],sHead_["Helicity","Angle",p3_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):=SpinorChainReduced[sHead["Zeta","Angle",p3],sHead["Helicity","Angle",p3]]SpinorChainReduced[sHead[sh2,"Square",p2],mHead[p5],sHead[sh1,"Angle",p1]];
Protect[Plus];
Protect[Times];




(* ::Subsubsection::Closed:: *)
(*SpinorTrace [Tr(...p...)]*)


(* ::Input::Initialization:: *)
(*Tr(...p...)*)
SpinorTraceReduced[a___,-mHead_[b_],c___]/;(momHeadQ[mHead]):=-SpinorTraceReduced[a,mHead[b],c];
SpinorTraceReduced[a___,mHead_[Multiparticle[b_,c_]],d___]/;(momHeadQ[mHead]):=SpinorTraceReduced[a,mHead[b],d]+SpinorTraceReduced[a,mHead[c],d];
SpinorTraceReduced[a___,mHead_[Multiparticle[b_,c_,ee_]],d___]/;(momHeadQ[mHead]):=SpinorTraceReduced[a,mHead[b],d]+SpinorTraceReduced[a,mHead[c],d]+SpinorTraceReduced[a,mHead[ee],d];

SpinorTraceReduced[a___,mHead_[b_],mHead_[b_],c___]/;(momHeadQ[mHead]):=Mass[b]^2 SpinorTraceReduced[a,c];
(* Moving a boundary square past the trace origin flips the alternating chirality. *)
SpinorTraceReduced[mHead_[b_],a___,mHead_[b_]]/;(momHeadQ[mHead]&&EvenQ[Length[{a}]]):=
  Mass[b]^2 (SpinorTraceReduced @@ RotateLeft[{a}]);

SpinorTraceReduced[a___,mHead_[p1_],mHead_[p2_],c___]/;(p2<p1&&momHeadQ[mHead]):=2momProdHead[mHead][p1,p2]SpinorTraceReduced[a,c]-SpinorTraceReduced[a,mHead[p2],mHead[p1],c];

(*SpinorTraceReduced[mHead_[a_],mHead_[b_],mHead_[c_],mHead_[d_]]/;(momHeadQ[mHead]):=2momProdHead[mHead][a,b]momProdHead[mHead][c,d]-2momProdHead[mHead][a,c]momProdHead[mHead][b,d]+2momProdHead[mHead][a,d]momProdHead[mHead][b,c]+2I Eps[mHead_[a],mHead_[b],mHead_[c],mHead_[d]];*)
SpinorTraceReduced[mHead_[a_],mHead_[b_]]/;(momHeadQ[mHead]):=2momProdHead[mHead][a,b];
SpinorTraceReduced[]:=2;


(* ::Subsubsection::Closed:: *)
(*MomProd (p1.p2)*)


(* ::Input::Initialization:: *)
(*p2.p1*)
MomProd[a_,b_]/;b<a:=MomProd[b,a];
MomProd[-a_,b_]:=-MomProd[a,b];
MomProd[a_,-b_]:=-MomProd[a,b];
MomProd[Multiparticle[a_,b_],c_]:=MomProd[a,c]+MomProd[b,c];
MomProd[c_,Multiparticle[a_,b_]]:=MomProd[a,c]+MomProd[b,c];
MomProd[Multiparticle[a_,b_,d__],c_]:=MomProd[a,c]+MomProd[Multiparticle[b,d],c];
MomProd[c_,Multiparticle[a_,b_,d__]]:=MomProd[a,c]+MomProd[Multiparticle[b,d],c];
MomProd[a_,a_]:=Mass[a]^2


MomProdHat12[a_,b_]/;b<a:=MomProdHat12[b,a];
MomProdHat12[-a_,b_]:=-MomProdHat12[a,b];
MomProdHat12[a_,-b_]:=-MomProdHat12[a,b];
MomProdHat12[Multiparticle[a_,b_],c_]:=MomProdHat12[a,c]+MomProdHat12[b,c];
MomProdHat12[c_,Multiparticle[a_,b_]]:=MomProdHat12[a,c]+MomProdHat12[b,c];
MomProdHat12[Multiparticle[a_,b_,d__],c_]:=MomProdHat12[a,c]+MomProdHat12[Multiparticle[b,d],c];
MomProdHat12[c_,Multiparticle[a_,b_,d__]]:=MomProdHat12[a,c]+MomProdHat12[Multiparticle[b,d],c];
MomProdHat12[a_,a_]:=Mass[a]^2


(* ::Subsubsection::Closed:: *)
(*Mandelstahm*)


ExtractMandelstahm[exp_]:=ExtractMandelstahm[exp,4]
ExtractMandelstahm[exp_,nPoints_]:=Module[{newExp},
newExp=exp//.{
MomProd[i_,j_]/;i!=j:>1/2 (Mandelstahm[i,j]-Mass[i]^2-Mass[j]^2),
PropDen[Mom[i_]+Mom[j_],m_]/;i!=j:>Mandelstahm[i,j]-m^2,
PropDen[Mom[Multiparticle[i_,j_]],m_]/;i!=j:>Mandelstahm[i,j]-m^2,
MomProdHat12[i_,j_]/;i!=j:>1/2 (MandelstahmHat[i,j]-Mass[i]^2-Mass[j]^2),
PropDen[MomHat[i_]+MomHat[j_],m_]/;i!=j:>MandelstahmHat[i,j]-m^2,
PropDen[MomHat[Multiparticle[i_,j_]],m_]/;i!=j:>MandelstahmHat[i,j]-m^2
};
If[nPoints==4,
newExp=newExp//.{
Mandelstahm[3,4]->Mandelstahm[1,2],
Mandelstahm[2,4]->Mandelstahm[1,3],
Mandelstahm[2,3]->Mandelstahm[1,4],
MandelstahmHat[3,4]->MandelstahmHat[1,2],
MandelstahmHat[2,4]->MandelstahmHat[1,3],
MandelstahmHat[2,3]->MandelstahmHat[1,4]
}];
newExp
];


(* ::Subsubsection::Closed:: *)
(*PropDen*)


PropDen[-mHead_[a_]+mHead_[b_],m_]/;(a<b&&momHeadQ[mHead]):=PropDen[mHead[a]-mHead[b],m];
PropDen[-mHead_[a_]-mHead_[b_],m_]/;momHeadQ[mHead]:=PropDen[mHead[a]+mHead[b],m];


(* ::Subsubsection::Closed:: *)
(*SimplifySpinorProducts*)


(* ::Input::Initialization:: *)
(*Contracted Spin Indices*)
Options[SimplifySpinorProducts]={Momenta->{1,2,3,4},InternalLines->{1,2},InternalLineOnShell->False,nTimes->10,massReplacementRules->{},PrintWarnings->True};
SimplifySpinorProducts[exp_,OptionsPattern[]]:=Module[{res,oldRes,doubleMomentumRules,standardFormRules,finalStandardFormRules,momConsRules3,momConsRules4={},onShellRules,
numTimes=OptionValue[nTimes],
massReplaceRules=OptionValue[massReplacementRules],
momenta=Sort[OptionValue[Momenta]],
IntLines=Sort[OptionValue[InternalLines]],ExtLines,intOnShell=OptionValue[InternalLineOnShell],jjTimes=1},
If[OptionValue[PrintWarnings],Print["Applying ",nice[Sum[Mom[momenta[[ii]]],{ii,1,Length[momenta]}]],"=0 and reducing."]];
If[OptionValue[PrintWarnings]&&Length[momenta]>3&&intOnShell,Print["Assuming on-shell condition (",nice[Mom[IntLines[[1]]]],"+",nice[Mom[IntLines[[2]]]],"\!\(\*SuperscriptBox[\()\), \(2\)]\)=",nice[(Mass[Multiparticle[IntLines[[1]],IntLines[[2]]]]/.massReplaceRules)^2]]];
ExtLines=Sort[Complement[momenta,IntLines]];

(*These replacement rules use Schouten identities and momentum conservation*)
(*to put the amplitude in a standard form.*)
(*Schouten Identities*)
(*[12][34]=[42][31]-[41][32] and generalizations*)
(*Momentum Conservation*)
(*p1+p2+p3+p4=0*)

doubleMomentumRules={
(*Reduce spinor products with 2 momenta to products with only 1.*)
(*[3|p2p1|4]=-[3|p1p1|4]-[3|p3p1|4]-[3|p4p1|4]*)
SpinorChainReduced[sHead_[sh3_,ah_,p3_],mHead_[p2_],mHead_[p1_],sHead_[sh4_,ah_,p4_]]/;(p3!=p4&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>-Mass[p1]^2SpinorChainReduced[sHead[sh3,ah,p3],sHead[sh4,ah,p4]]+SpinorChainReduced[sHead[sh3,ah,p3],mHead[p1]+mHead[p2]-Sum[mHead[momenta[[ii]]],{ii,1,Length[momenta]}],mHead[p1],sHead[sh4,ah,p4]]
};
standardFormRules={
(*Define a standard form for spinor products with 1 momentum.*)
(*Very specific to 4-point amplitudes (p1+p2+p3+p4=0).*)
(*But, hopefully generalizable to higher-point amplitudes.*)
(*Perhaps define lines for each prpopagator and have some mom conservation for each prop.*)

(*Reduce all combinations with a single momentum to a standard form.*)
(*Focus on the oddball.*)
(*    <ab>[c|d|e> : c is the oddball.*)
(*    [ab][c|d|e> : e is the oddball.*)

(*<inex>[in|in|ex> \[Rule] <inex>[in|ex|ex>*)
(*<24>[1|p5|3> = <24>[1|p5-(p1+p2+p3+p4)|3>*)
SpinorChainReduced[sHead_[sh2_,"Angle",p2_],sHead_[sh4_,"Angle",p4_]]SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p5_],sHead_[sh3_,"Angle",p3_]]/;(p4!=p3&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[IntLines,p1]&&MemberQ[IntLines,p5]&&MemberQ[ExtLines,p3]&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>SpinorChainReduced[sHead[sh2,"Angle",p2],sHead[sh4,"Angle",p4]]SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5]-Sum[mHead[momenta[[ii]]],{ii,1,Length[momenta]}],sHead[sh3,"Angle",p3]],
(*<inex>[in|in|ex(>^n)*)
SpinorChainReduced[sHead_[sh2_,"Angle",p2_],sHead_[sh4_,"Angle",p4_]]SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p5_],sHead_[sh3_,"Angle",p3_]]^n_/;(p4!=p3&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[IntLines,p1]&&MemberQ[IntLines,p5]&&MemberQ[ExtLines,p3]&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>SpinorChainReduced[sHead[sh2,"Angle",p2],sHead[sh4,"Angle",p4]]SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5]-Sum[mHead[momenta[[ii]]],{ii,1,Length[momenta]}],sHead[sh3,"Angle",p3]]SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5],sHead[sh3,"Angle",p3]]^(n-1),
(*<inex(>^m)[in|in|ex>*)
SpinorChainReduced[sHead_[sh2_,"Angle",p2_],sHead_[sh4_,"Angle",p4_]]^m_ SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p5_],sHead_[sh3_,"Angle",p3_]]/;(p4!=p3&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[IntLines,p1]&&MemberQ[IntLines,p5]&&MemberQ[ExtLines,p3]&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>SpinorChainReduced[sHead[sh2,"Angle",p2],sHead[sh4,"Angle",p4]]^m SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5]-Sum[mHead[momenta[[ii]]],{ii,1,Length[momenta]}],sHead[sh3,"Angle",p3]],
(*<inex(>^m)[in|in|ex(>^n)*)
SpinorChainReduced[sHead_[sh2_,"Angle",p2_],sHead_[sh4_,"Angle",p4_]]^m_ SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p5_],sHead_[sh3_,"Angle",p3_]]^n_/;(p4!=p3&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[IntLines,p1]&&MemberQ[IntLines,p5]&&MemberQ[ExtLines,p3]&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>SpinorChainReduced[sHead[sh2,"Angle",p2],sHead[sh4,"Angle",p4]]^m SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5]-Sum[mHead[momenta[[ii]]],{ii,1,Length[momenta]}],sHead[sh3,"Angle",p3]]SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5],sHead[sh3,"Angle",p3]]^(n-1),
(*<inex>[in|ex|ex> \[Rule] <exex>[in|ex|in> + m<inex>[inex]*)
(*<24>[1|p5|3> = <34>[1|p5|2>-<32>[1|p5|4>*)
SpinorChainReduced[sHead_[sh2_,"Angle",p2_],sHead_[sh4_,"Angle",p4_]]SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p5_],sHead_[sh3_,"Angle",p3_]]/;(p4!=p3&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[IntLines,p1]&&MemberQ[ExtLines,p5]&&MemberQ[ExtLines,p3]&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>SpinorChainReduced[sHead[sh3,"Angle",p3],sHead[sh4,"Angle",p4]]SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5],sHead[sh2,"Angle",p2]]-SpinorChainReduced[sHead[sh3,"Angle",p3],sHead[sh2,"Angle",p2]]SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5],sHead[sh4,"Angle",p4]],
(*<inex>[in|ex|ex>^n*)
SpinorChainReduced[sHead_[sh2_,"Angle",p2_],sHead_[sh4_,"Angle",p4_]]SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p5_],sHead_[sh3_,"Angle",p3_]]^n_/;(p4!=p3&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[IntLines,p1]&&MemberQ[ExtLines,p5]&&MemberQ[ExtLines,p3]&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>(SpinorChainReduced[sHead[sh3,"Angle",p3],sHead[sh4,"Angle",p4]]SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5],sHead[sh2,"Angle",p2]]-SpinorChainReduced[sHead[sh3,"Angle",p3],sHead[sh2,"Angle",p2]]SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5],sHead[sh4,"Angle",p4]])SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5],sHead[sh3,"Angle",p3]]^(n-1),
(*<inex>^m[in|ex|ex>*)
SpinorChainReduced[sHead_[sh2_,"Angle",p2_],sHead_[sh4_,"Angle",p4_]]^m_ SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p5_],sHead_[sh3_,"Angle",p3_]]/;(p4!=p3&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[IntLines,p1]&&MemberQ[ExtLines,p5]&&MemberQ[ExtLines,p3]&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>SpinorChainReduced[sHead[sh2,"Angle",p2],sHead[sh4,"Angle",p4]]^(m-1) (SpinorChainReduced[sHead[sh3,"Angle",p3],sHead[sh4,"Angle",p4]]SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5],sHead[sh2,"Angle",p2]]-SpinorChainReduced[sHead[sh3,"Angle",p3],sHead[sh2,"Angle",p2]]SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5],sHead[sh4,"Angle",p4]]),
(*<inex>^m[in|ex|ex>^n*)
SpinorChainReduced[sHead_[sh2_,"Angle",p2_],sHead_[sh4_,"Angle",p4_]]^m_ SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p5_],sHead_[sh3_,"Angle",p3_]]^n_/;(p4!=p3&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[IntLines,p1]&&MemberQ[ExtLines,p5]&&MemberQ[ExtLines,p3]&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>SpinorChainReduced[sHead[sh2,"Angle",p2],sHead[sh4,"Angle",p4]]^(m-1) (SpinorChainReduced[sHead[sh3,"Angle",p3],sHead[sh4,"Angle",p4]]SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5],sHead[sh2,"Angle",p2]]-SpinorChainReduced[sHead[sh3,"Angle",p3],sHead[sh2,"Angle",p2]]SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5],sHead[sh4,"Angle",p4]])SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5],sHead[sh3,"Angle",p3]]^(n-1),
(*[in|ex_notlow|in> \[Rule] [in|ex_low|in>*)
(*[1|p4|2> = -[1|p1+p2+p3|2>*)
SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p5_],sHead_[sh2_,"Angle",p2_]]/;(MemberQ[IntLines,p1]&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p5]&&p5!=ExtLines[[1]]&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5]-Sum[mHead[momenta[[ii]]],{ii,1,Length[momenta]}],sHead[sh2,"Angle",p2]],

(*<inex>[ex|ex|in> \[Rule] <inex>[ex|in|in>*)
(*<24>[1|p5|3> = <24>[1|p5-(p1+p2+p3+p4)|3>*)
SpinorChainReduced[sHead_[sh2_,"Angle",p2_],sHead_[sh4_,"Angle",p4_]]SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p5_],sHead_[sh3_,"Angle",p3_]]/;(p2!=p3&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[ExtLines,p1]&&MemberQ[ExtLines,p5]&&MemberQ[IntLines,p3]&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>SpinorChainReduced[sHead[sh2,"Angle",p2],sHead[sh4,"Angle",p4]]SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5]-Sum[mHead[momenta[[ii]]],{ii,1,Length[momenta]}],sHead[sh3,"Angle",p3]],
(*<inex>[ex|ex|in>^n*)
SpinorChainReduced[sHead_[sh2_,"Angle",p2_],sHead_[sh4_,"Angle",p4_]]SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p5_],sHead_[sh3_,"Angle",p3_]]^n_/;(p2!=p3&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[ExtLines,p1]&&MemberQ[ExtLines,p5]&&MemberQ[IntLines,p3]&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>SpinorChainReduced[sHead[sh2,"Angle",p2],sHead[sh4,"Angle",p4]]SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5]-Sum[mHead[momenta[[ii]]],{ii,1,Length[momenta]}],sHead[sh3,"Angle",p3]]SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5],sHead[sh3,"Angle",p3]]^(n-1),
(*<inex>^m[ex|ex|in>*)
SpinorChainReduced[sHead_[sh2_,"Angle",p2_],sHead_[sh4_,"Angle",p4_]]^m_ SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p5_],sHead_[sh3_,"Angle",p3_]]/;(p2!=p3&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[ExtLines,p1]&&MemberQ[ExtLines,p5]&&MemberQ[IntLines,p3]&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>SpinorChainReduced[sHead[sh2,"Angle",p2],sHead[sh4,"Angle",p4]]^m SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5]-Sum[mHead[momenta[[ii]]],{ii,1,Length[momenta]}],sHead[sh3,"Angle",p3]],
(*<inex>^m[ex|ex|in>^n*)
SpinorChainReduced[sHead_[sh2_,"Angle",p2_],sHead_[sh4_,"Angle",p4_]]^m_ SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p5_],sHead_[sh3_,"Angle",p3_]]^n_/;(p2!=p3&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[ExtLines,p1]&&MemberQ[ExtLines,p5]&&MemberQ[IntLines,p3]&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>SpinorChainReduced[sHead[sh2,"Angle",p2],sHead[sh4,"Angle",p4]]^m SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5]-Sum[mHead[momenta[[ii]]],{ii,1,Length[momenta]}],sHead[sh3,"Angle",p3]]SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5],sHead[sh3,"Angle",p3]]^(n-1),
(*<inex>[ex|in|in> \[Rule] <inin>[ex|in|ex> + m<inex>[inex]*)
(*<24>[1|p5|3> = <34>[1|p5|2>-<32>[1|p5|4>*)
SpinorChainReduced[sHead_[sh2_,"Angle",p2_],sHead_[sh4_,"Angle",p4_]]SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p5_],sHead_[sh3_,"Angle",p3_]]/;(p2!=p3&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[ExtLines,p1]&&MemberQ[IntLines,p5]&&MemberQ[IntLines,p3]&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>SpinorChainReduced[sHead[sh3,"Angle",p3],sHead[sh4,"Angle",p4]]SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5],sHead[sh2,"Angle",p2]]-SpinorChainReduced[sHead[sh3,"Angle",p3],sHead[sh2,"Angle",p2]]SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5],sHead[sh4,"Angle",p4]],
(*<inex>[ex|in|in>^n*)
SpinorChainReduced[sHead_[sh2_,"Angle",p2_],sHead_[sh4_,"Angle",p4_]]SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p5_],sHead_[sh3_,"Angle",p3_]]^n_/;(p2!=p3&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[ExtLines,p1]&&MemberQ[IntLines,p5]&&MemberQ[IntLines,p3]&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>(SpinorChainReduced[sHead[sh3,"Angle",p3],sHead[sh4,"Angle",p4]]SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5],sHead[sh2,"Angle",p2]]-SpinorChainReduced[sHead[sh3,"Angle",p3],sHead[sh2,"Angle",p2]]SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5],sHead[sh4,"Angle",p4]])SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5],sHead[sh3,"Angle",p3]]^(n-1),
(*<inex>^m[ex|in|in>*)
SpinorChainReduced[sHead_[sh2_,"Angle",p2_],sHead_[sh4_,"Angle",p4_]]^m_ SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p5_],sHead_[sh3_,"Angle",p3_]]/;(p2!=p3&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[ExtLines,p1]&&MemberQ[IntLines,p5]&&MemberQ[IntLines,p3]&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>SpinorChainReduced[sHead[sh2,"Angle",p2],sHead[sh4,"Angle",p4]]^(m-1) (SpinorChainReduced[sHead[sh3,"Angle",p3],sHead[sh4,"Angle",p4]]SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5],sHead[sh2,"Angle",p2]]-SpinorChainReduced[sHead[sh3,"Angle",p3],sHead[sh2,"Angle",p2]]SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5],sHead[sh4,"Angle",p4]]),
(*<inex>^m[ex|in|in>^n*)
SpinorChainReduced[sHead_[sh2_,"Angle",p2_],sHead_[sh4_,"Angle",p4_]]^m_ SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p5_],sHead_[sh3_,"Angle",p3_]]^n_/;(p2!=p3&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[ExtLines,p1]&&MemberQ[IntLines,p5]&&MemberQ[IntLines,p3]&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>SpinorChainReduced[sHead[sh2,"Angle",p2],sHead[sh4,"Angle",p4]]^(m-1) (SpinorChainReduced[sHead[sh3,"Angle",p3],sHead[sh4,"Angle",p4]]SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5],sHead[sh2,"Angle",p2]]-SpinorChainReduced[sHead[sh3,"Angle",p3],sHead[sh2,"Angle",p2]]SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5],sHead[sh4,"Angle",p4]])SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5],sHead[sh3,"Angle",p3]]^(n-1),
(*[ex|in_notlow|ex> \[Rule] [ex|in_low|ex>*)
(*[1|p4|2> = -[1|p1+p2+p3|2>*)
SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p5_],sHead_[sh2_,"Angle",p2_]]/;(MemberQ[ExtLines,p1]&&MemberQ[ExtLines,p2]&&MemberQ[IntLines,p5]&&p5!=IntLines[[1]]&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5]-Sum[mHead[momenta[[ii]]],{ii,1,Length[momenta]}],sHead[sh2,"Angle",p2]],

(*[inex][ex|in|in> \[Rule] [inex][ex|ex|in>*)
(*[24][1|p5|3> = [24][1|p5-(p1+p2+p3+p4)|3>*)
SpinorChainReduced[sHead_[sh2_,"Square",p2_],sHead_[sh4_,"Square",p4_]]SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p5_],sHead_[sh3_,"Angle",p3_]]/;(p4!=p1&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[ExtLines,p1]&&MemberQ[IntLines,p5]&&MemberQ[IntLines,p3]&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>SpinorChainReduced[sHead[sh2,"Square",p2],sHead[sh4,"Square",p4]]SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5]-Sum[mHead[momenta[[ii]]],{ii,1,Length[momenta]}],sHead[sh3,"Angle",p3]],
(*[inex][ex|in|in>^n*)
SpinorChainReduced[sHead_[sh2_,"Square",p2_],sHead_[sh4_,"Square",p4_]]SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p5_],sHead_[sh3_,"Angle",p3_]]^n_/;(p4!=p1&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[ExtLines,p1]&&MemberQ[IntLines,p5]&&MemberQ[IntLines,p3]&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>SpinorChainReduced[sHead[sh2,"Square",p2],sHead[sh4,"Square",p4]]SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5]-Sum[mHead[momenta[[ii]]],{ii,1,Length[momenta]}],sHead[sh3,"Angle",p3]]SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5],sHead[sh3,"Angle",p3]]^(n-1),
(*[inex]^m[ex|in|in>*)
SpinorChainReduced[sHead_[sh2_,"Square",p2_],sHead_[sh4_,"Square",p4_]]^m_ SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p5_],sHead_[sh3_,"Angle",p3_]]/;(p4!=p1&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[ExtLines,p1]&&MemberQ[IntLines,p5]&&MemberQ[IntLines,p3]&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>SpinorChainReduced[sHead[sh2,"Square",p2],sHead[sh4,"Square",p4]]^m SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5]-Sum[mHead[momenta[[ii]]],{ii,1,Length[momenta]}],sHead[sh3,"Angle",p3]],
(*[inex]^m[ex|in|in>^n*)
SpinorChainReduced[sHead_[sh2_,"Square",p2_],sHead_[sh4_,"Square",p4_]]^m_ SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p5_],sHead_[sh3_,"Angle",p3_]]^n_/;(p4!=p1&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[ExtLines,p1]&&MemberQ[IntLines,p5]&&MemberQ[IntLines,p3]&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>SpinorChainReduced[sHead[sh2,"Square",p2],sHead[sh4,"Square",p4]]^m SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5]-Sum[mHead[momenta[[ii]]],{ii,1,Length[momenta]}],sHead[sh3,"Angle",p3]]SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5],sHead[sh3,"Angle",p3]]^(n-1),
(*[inex][ex|ex|in> \[Rule] [exex][in|ex|in> + m[inex]<inex>*)
(*[24][1|p5|3> = -[12][4|p5|3>+[14][2|p5|3>*)
SpinorChainReduced[sHead_[sh2_,"Square",p2_],sHead_[sh4_,"Square",p4_]]SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p5_],sHead_[sh3_,"Angle",p3_]]/;(p4!=p1&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[ExtLines,p1]&&MemberQ[ExtLines,p5]&&MemberQ[IntLines,p3]&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>-SpinorChainReduced[sHead[sh1,"Square",p1],sHead[sh2,"Square",p2]]SpinorChainReduced[sHead[sh4,"Square",p4],mHead[p5],sHead[sh3,"Angle",p3]]+SpinorChainReduced[sHead[sh1,"Square",p1],sHead[sh4,"Square",p4]]SpinorChainReduced[sHead[sh2,"Square",p2],mHead[p5],sHead[sh3,"Angle",p3]],
(*[inex][ex|ex|in>^n*)
SpinorChainReduced[sHead_[sh2_,"Square",p2_],sHead_[sh4_,"Square",p4_]]SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p5_],sHead_[sh3_,"Angle",p3_]]^n_/;(p4!=p1&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[ExtLines,p1]&&MemberQ[ExtLines,p5]&&MemberQ[IntLines,p3]&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>(-SpinorChainReduced[sHead[sh1,"Square",p1],sHead[sh2,"Square",p2]]SpinorChainReduced[sHead[sh4,"Square",p4],mHead[p5],sHead[sh3,"Angle",p3]]+SpinorChainReduced[sHead[sh1,"Square",p1],sHead[sh4,"Square",p4]]SpinorChainReduced[sHead[sh2,"Square",p2],mHead[p5],sHead[sh3,"Angle",p3]])SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5],sHead[sh3,"Angle",p3]]^(n-1),
(*[inex]^m[ex|ex|in>*)
SpinorChainReduced[sHead_[sh2_,"Square",p2_],sHead_[sh4_,"Square",p4_]]^m_ SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p5_],sHead_[sh3_,"Angle",p3_]]/;(p4!=p1&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[ExtLines,p1]&&MemberQ[ExtLines,p5]&&MemberQ[IntLines,p3]&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>SpinorChainReduced[sHead[sh2,"Square",p2],sHead[sh4,"Square",p4]]^(m-1) (-SpinorChainReduced[sHead[sh1,"Square",p1],sHead[sh2,"Square",p2]]SpinorChainReduced[sHead[sh4,"Square",p4],mHead[p5],sHead[sh3,"Angle",p3]]+SpinorChainReduced[sHead[sh1,"Square",p1],sHead[sh4,"Square",p4]]SpinorChainReduced[sHead[sh2,"Square",p2],mHead[p5],sHead[sh3,"Angle",p3]]),
(*[inex]^m[ex|ex|in>^n*)
SpinorChainReduced[sHead_[sh2_,"Square",p2_],sHead_[sh4_,"Square",p4_]]^m_ SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p5_],sHead_[sh3_,"Angle",p3_]]^n_/;(p4!=p1&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[ExtLines,p1]&&MemberQ[ExtLines,p5]&&MemberQ[IntLines,p3]&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>SpinorChainReduced[sHead[sh2,"Square",p2],sHead[sh4,"Square",p4]]^(m-1) (-SpinorChainReduced[sHead[sh1,"Square",p1],sHead[sh2,"Square",p2]]SpinorChainReduced[sHead[sh4,"Square",p4],mHead[p5],sHead[sh3,"Angle",p3]]+SpinorChainReduced[sHead[sh1,"Square",p1],sHead[sh4,"Square",p4]]SpinorChainReduced[sHead[sh2,"Square",p2],mHead[p5],sHead[sh3,"Angle",p3]])SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5],sHead[sh3,"Angle",p3]]^(n-1),
(*[in|ex_notlow|in> \[Rule] [in|ex_low|in>*)
(*[1|p4|2> = -[1|p1+p2+p3|2>*)
SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p5_],sHead_[sh2_,"Angle",p2_]]/;(MemberQ[IntLines,p1]&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p5]&&p5!=ExtLines[[1]]&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5]-Sum[mHead[momenta[[ii]]],{ii,1,Length[momenta]}],sHead[sh2,"Angle",p2]],

(*[inex][in|ex|ex> \[Rule] [inex][in|in|ex>*)
(*[24][1|p5|3> = [24][1|p5-(p1+p2+p3+p4)|3>*)
SpinorChainReduced[sHead_[sh2_,"Square",p2_],sHead_[sh4_,"Square",p4_]]SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p5_],sHead_[sh3_,"Angle",p3_]]/;(p2!=p1&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[IntLines,p1]&&MemberQ[ExtLines,p5]&&MemberQ[ExtLines,p3]&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>SpinorChainReduced[sHead[sh2,"Square",p2],sHead[sh4,"Square",p4]]SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5]-Sum[mHead[momenta[[ii]]],{ii,1,Length[momenta]}],sHead[sh3,"Angle",p3]],
(*[inex][in|ex|ex>^n*)
SpinorChainReduced[sHead_[sh2_,"Square",p2_],sHead_[sh4_,"Square",p4_]]SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p5_],sHead_[sh3_,"Angle",p3_]]^n_/;(p2!=p1&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[IntLines,p1]&&MemberQ[ExtLines,p5]&&MemberQ[ExtLines,p3]&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>SpinorChainReduced[sHead[sh2,"Square",p2],sHead[sh4,"Square",p4]]SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5]-Sum[mHead[momenta[[ii]]],{ii,1,Length[momenta]}],sHead[sh3,"Angle",p3]]SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5],sHead[sh3,"Angle",p3]]^(n-1),
(*[inex]^m[in|ex|ex>*)
SpinorChainReduced[sHead_[sh2_,"Square",p2_],sHead_[sh4_,"Square",p4_]]^m_ SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p5_],sHead_[sh3_,"Angle",p3_]]/;(p2!=p1&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[IntLines,p1]&&MemberQ[ExtLines,p5]&&MemberQ[ExtLines,p3]&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>SpinorChainReduced[sHead[sh2,"Square",p2],sHead[sh4,"Square",p4]]^m SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5]-Sum[mHead[momenta[[ii]]],{ii,1,Length[momenta]}],sHead[sh3,"Angle",p3]],
(*[inex]^m[in|ex|ex>^n*)
SpinorChainReduced[sHead_[sh2_,"Square",p2_],sHead_[sh4_,"Square",p4_]]^m_ SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p5_],sHead_[sh3_,"Angle",p3_]]^n_/;(p2!=p1&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[IntLines,p1]&&MemberQ[ExtLines,p5]&&MemberQ[ExtLines,p3]&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>SpinorChainReduced[sHead[sh2,"Square",p2],sHead[sh4,"Square",p4]]^m SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5]-Sum[mHead[momenta[[ii]]],{ii,1,Length[momenta]}],sHead[sh3,"Angle",p3]]SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5],sHead[sh3,"Angle",p3]]^(n-1),
(*[inex][in|in|ex> \[Rule] [inin][ex|in|ex> + m[inex]<inex>*)
(*[24][1|p5|3> = -[12][4|p5|3>+[14][2|p5|3>*)
SpinorChainReduced[sHead_[sh2_,"Square",p2_],sHead_[sh4_,"Square",p4_]]SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p5_],sHead_[sh3_,"Angle",p3_]]/;(p2!=p1&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[IntLines,p1]&&MemberQ[IntLines,p5]&&MemberQ[ExtLines,p3]&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>-SpinorChainReduced[sHead[sh1,"Square",p1],sHead[sh2,"Square",p2]]SpinorChainReduced[sHead[sh4,"Square",p4],mHead[p5],sHead[sh3,"Angle",p3]]+SpinorChainReduced[sHead[sh1,"Square",p1],sHead[sh4,"Square",p4]]SpinorChainReduced[sHead[sh2,"Square",p2],mHead[p5],sHead[sh3,"Angle",p3]],
(*[inex][in|in|ex>^n*)
SpinorChainReduced[sHead_[sh2_,"Square",p2_],sHead_[sh4_,"Square",p4_]]SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p5_],sHead_[sh3_,"Angle",p3_]]^n_/;(p2!=p1&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[IntLines,p1]&&MemberQ[IntLines,p5]&&MemberQ[ExtLines,p3]&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>(-SpinorChainReduced[sHead[sh1,"Square",p1],sHead[sh2,"Square",p2]]SpinorChainReduced[sHead[sh4,"Square",p4],mHead[p5],sHead[sh3,"Angle",p3]]+SpinorChainReduced[sHead[sh1,"Square",p1],sHead[sh4,"Square",p4]]SpinorChainReduced[sHead[sh2,"Square",p2],mHead[p5],sHead[sh3,"Angle",p3]])SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5],sHead[sh3,"Angle",p3]]^(n-1),
(*[inex]^m[in|in|ex>*)
SpinorChainReduced[sHead_[sh2_,"Square",p2_],sHead_[sh4_,"Square",p4_]]^m_ SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p5_],sHead_[sh3_,"Angle",p3_]]/;(p2!=p1&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[IntLines,p1]&&MemberQ[IntLines,p5]&&MemberQ[ExtLines,p3]&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>SpinorChainReduced[sHead[sh2,"Square",p2],sHead[sh4,"Square",p4]]^(m-1) (-SpinorChainReduced[sHead[sh1,"Square",p1],sHead[sh2,"Square",p2]]SpinorChainReduced[sHead[sh4,"Square",p4],mHead[p5],sHead[sh3,"Angle",p3]]+SpinorChainReduced[sHead[sh1,"Square",p1],sHead[sh4,"Square",p4]]SpinorChainReduced[sHead[sh2,"Square",p2],mHead[p5],sHead[sh3,"Angle",p3]]),
(*[inex]^m[in|in|ex>^n*)
SpinorChainReduced[sHead_[sh2_,"Square",p2_],sHead_[sh4_,"Square",p4_]]^m_ SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p5_],sHead_[sh3_,"Angle",p3_]]^n_/;(p2!=p1&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[IntLines,p1]&&MemberQ[IntLines,p5]&&MemberQ[ExtLines,p3]&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>SpinorChainReduced[sHead[sh2,"Square",p2],sHead[sh4,"Square",p4]]^(m-1) (-SpinorChainReduced[sHead[sh1,"Square",p1],sHead[sh2,"Square",p2]]SpinorChainReduced[sHead[sh4,"Square",p4],mHead[p5],sHead[sh3,"Angle",p3]]+SpinorChainReduced[sHead[sh1,"Square",p1],sHead[sh4,"Square",p4]]SpinorChainReduced[sHead[sh2,"Square",p2],mHead[p5],sHead[sh3,"Angle",p3]])SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5],sHead[sh3,"Angle",p3]]^(n-1),
(*[ex|in_notlow|ex> \[Rule] [ex|in_low|ex>*)
(*[1|p4|2> = -[1|p1+p2+p3|2>*)
SpinorChainReduced[sHead_[sh1_,"Square",p1_],mHead_[p5_],sHead_[sh2_,"Angle",p2_]]/;(MemberQ[ExtLines,p1]&&MemberQ[ExtLines,p2]&&MemberQ[IntLines,p5]&&p5!=IntLines[[1]]&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>SpinorChainReduced[sHead[sh1,"Square",p1],mHead[p5]-Sum[mHead[momenta[[ii]]],{ii,1,Length[momenta]}],sHead[sh2,"Angle",p2]],

(*Define a standard form for spinor chains with 2 momentum.*)
(*e.g. <4|p1p2|4> not <4|p1p3|4>*)
(*e.g. <2|p1p3|2> not <2|p1p4|2>*)
(*Very specific to 4-point amplitudes (p1+p2+p3+p4=0).*)
SpinorChainReduced[sHead_[sh4_,sq_,p4_],mHead_[p5_],mHead_[p6_],sHead_[sh4_,sq_,p4_]]/;(p6-p5>1&&(p6-p4>1||p6-p4<0)&&spinorHeadQ[sHead]&&momHeadQ[mHead]):>SpinorChainReduced[sHead[sh4,sq,p4],mHead[p5],mHead[p6]-Sum[mHead[momenta[[ii]]],{ii,1,Length[momenta]}],sHead[sh4,sq,p4]],
(*[12][3|p1|4(>^2) = -[12][3|p2|4>[3|p1|4>-[12][3|p3|4>[3|p1|4>-[12][3|p4|4>[3|p1|4>*)
(*<12>[3|p1|4(>^2)*)
SpinorChainReduced[sHead_[sh1_,sa_,p1_],sHead_[sh2_,sa_,p2_]]SpinorChainReduced[sHead_[sh3_,sq_,p3_],mHead_[p5_],sHead_[sh4_,an_,p4_]]^2/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):>SpinorChainReduced[sHead[sh1,sa,p1],sHead[sh2,sa,p2]]SpinorChainReduced[sHead[sh3,sq,p3],mHead[p5],sHead[sh4,an,p4]]SpinorChainReduced[sHead[sh3,sq,p3],mHead[p5]-Sum[mHead[momenta[[ii]]],{ii,1,Length[momenta]}],sHead[sh4,an,p4]],
(*[4|p1|2>[3|p1p2|3] = -[4|p2|2>[3|p1p2|3]-[4|p3|2>[3|p1p2|3]-[4|p4|2>[3|p1p2|3]*)
SpinorChainReduced[sHead_[sh4_,sq_,p4_],mHead_[p1_],sHead_[sh2_,an_,p2_]]SpinorChainReduced[sHead_[sh3_,sq_,p3_],mHead_[p1_],mHead_[p2_],sHead_[sh3_,sq_,p3_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):>SpinorChainReduced[sHead[sh4,sq,p4],mHead[p1]-Sum[mHead[momenta[[ii]]],{ii,1,Length[momenta]}],sHead[sh2,an,p2]]SpinorChainReduced[sHead[sh3,sq,p3],mHead[p1],mHead[p2],sHead[sh3,sq,p3]],

(*Massless <pN|...|pN]*)
SpinorChainReduced[sHead_["Helicity","Square",pN_],a___,sHead_["Helicity","Angle",pN_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):>SpinorTraceReduced[mHead_[pN],a],
SpinorChainReduced[sHead_["Helicity","Angle",pN_],a___,sHead_["Helicity","Square",pN_]]/;(spinorHeadQ[sHead]&&momHeadQ[mHead]):>SpinorTraceReduced[a,mHead_[pN]]



};
finalStandardFormRules={
(*If you can't put it in the form [in|ex|in> or [ex|in|ex>, then atleast make momentum in.*)
(*[ex|ex|in>\[Rule][ex|in|in>*)
SpinorChainReduced[sHead_[sh3_,"Square",p3_],mHead_[p4_],sHead_[sh2_,"Angle",p2_]]/;(MemberQ[ExtLines,p3]&&MemberQ[ExtLines,p4]&&MemberQ[IntLines,p2]&&(spinorHeadQ[sHead]&&momHeadQ[mHead])):>SpinorChainReduced[sHead[sh3,"Square",p3],mHead[p4]-Sum[mHead[momenta[[ii]]],{ii,1,Length[momenta]}],sHead[sh2,"Angle",p2]],
(*[in|ex|ex>\[Rule][in|in|ex>*)
SpinorChainReduced[sHead_[sh3_,"Square",p3_],mHead_[p4_],sHead_[sh2_,"Angle",p2_]]/;(MemberQ[IntLines,p3]&&MemberQ[ExtLines,p4]&&MemberQ[ExtLines,p2]&&(spinorHeadQ[sHead]&&momHeadQ[mHead])):>SpinorChainReduced[sHead[sh3,"Square",p3],mHead[p4]-Sum[mHead[momenta[[ii]]],{ii,1,Length[momenta]}],sHead[sh2,"Angle",p2]]
};
momConsRules3={
mProdHead_[1,2]/;momProdHeadQ[mProdHead]:>1/2 (Mass[3]^2-Mass[1]^2-Mass[2]^2),
mProdHead_[2,3]/;momProdHeadQ[mProdHead]:>1/2 (Mass[1]^2-Mass[2]^2-Mass[3]^2),
mProdHead_[1,3]/;momProdHeadQ[mProdHead]:>1/2 (Mass[2]^2-Mass[1]^2-Mass[3]^2)
};
If[Length[momenta]==4,
momConsRules4={
mProdHead_[ExtLines[[1]],ExtLines[[2]]]/;momProdHeadQ[mProdHead]:>1/2 (-Mass[ExtLines[[1]]]^2-Mass[ExtLines[[2]]]^2 +Mass[IntLines[[1]]]^2+Mass[IntLines[[2]]]^2)+mProdHead[IntLines[[1]],IntLines[[2]]],
mProdHead_[IntLines[[1]],ExtLines[[1]]]/;momProdHeadQ[mProdHead]:>-Mass[IntLines[[1]]]^2-mProdHead[IntLines[[1]],IntLines[[2]]]-mProdHead[IntLines[[1]],ExtLines[[2]]],
mProdHead_[IntLines[[2]],ExtLines[[1]]]/;momProdHeadQ[mProdHead]:>1/2 (-Mass[IntLines[[2]]]^2-Mass[ExtLines[[1]]]^2+Mass[IntLines[[1]]]^2+Mass[ExtLines[[2]]]^2)+mProdHead[IntLines[[1]],ExtLines[[2]]],
mProdHead_[IntLines[[2]],ExtLines[[2]]]/;momProdHeadQ[mProdHead]:>-mProdHead[IntLines[[1]],ExtLines[[2]]]-mProdHead[ExtLines[[1]],ExtLines[[2]]]-Mass[ExtLines[[2]]]^2
};(*Print[momConsRules4];*)
];
onShellRules={
mProdHead_[IntLines[[1]],IntLines[[2]]]/;momProdHeadQ[mProdHead]:>1/2 (Mass[Multiparticle[IntLines[[1]],IntLines[[2]]]]^2-Mass[IntLines[[1]]]^2-Mass[IntLines[[2]]]^2)
};

res=ReduceSpinContractions[exp];
res=res/.{SpinorChain[a___]:>SpinorChainReduced[a],SpinorTrace[a___]:>SpinorTraceReduced[a]};

While[res=!=oldRes,
(*Print[jjTimes];*)(*Print[jjTimes,": ",res];*)
oldRes=res;
res=Collect[Expand[res/.doubleMomentumRules]//.standardFormRules,{En[_],Coupling[_],Mass[_]},Simplify];
Which[
Length[momenta]==3,res=res//.momConsRules3;,
Length[momenta]==4,res=res//.momConsRules4;
];
If[intOnShell,res=res//.onShellRules;];
res=res/.massReplaceRules;
res=Collect[Expand[res],{En[_],Coupling[_],Mass[_]},Simplify];
jjTimes=jjTimes+1;
If[jjTimes>numTimes,Print["Giving up after ",numTimes," attempts to simplify!"];Break[]];
];
Simplify[res/.finalStandardFormRules]/.{SpinorChainReduced[a___]:>SpinorChain[a],SpinorTraceReduced[a___]:>SpinorTrace[a]}
]


(* ::Subsubsection::Closed:: *)
(*xFactor*)


Options[ReducexFactors]={Momenta->{1,2,3,4},ChannelMomenta->{1,4},ChannelMass->Mchannel};
Options[xFactorReduced]={Momenta->{1,2,3,4},ChannelMomenta->{1,4},ChannelMass->Mchannel};
Options[xTildeFactorReduced]={Momenta->{1,2,3,4},ChannelMomenta->{1,4},ChannelMass->Mchannel};


ReducexFactors[amp_,OptionsPattern[]]:=Module[{options,momenta,channelMomenta,complementMomenta},
momenta=OptionValue[Momenta];
channelMomenta=Sort[OptionValue[ChannelMomenta]];
complementMomenta=Sort[Complement[momenta,channelMomenta]];
options={Momenta->momenta,ChannelMomenta->channelMomenta,ChannelMass->OptionValue[ChannelMass]};

amp//.{
xFactor[i_,j_]/;(Head[i]==Integer&&Head[j]==Integer):>xFactorReduced[i,j],

xTildeFactor[i_,j_]/;(Head[i]==Integer&&Head[j]==Integer):>xTildeFactorReduced[i,j],

xFactor[a_,Multiparticle[b__]]:>xFactorReduced[a,Multiparticle[b],options],

xFactor[Multiparticle[b__],a_]:>-xFactorReduced[a,Multiparticle[b],options],

xFactor[Multiparticle[a__],Multiparticle[b__]]:>xFactorReduced[a,Multiparticle[b],options],

xTildeFactor[a_,Multiparticle[b__]]:>xTildeFactorReduced[a,Multiparticle[b],options],

xTildeFactor[Multiparticle[b__],a_]:>-xTildeFactorReduced[a,Multiparticle[b],options],

xTildeFactor[Multiparticle[a__],Multiparticle[b__]]:>xTildeFactorReduced[Multiparticle[a],Multiparticle[b],options]
}
]


xFactorReduced[p1_,Multiparticle[p2_,p3_],OptionsPattern[]]:=Module[{p4,momenta,channelMomenta,channelPs,channelM},
momenta=OptionValue[Momenta];
p4=Complement[momenta,{p1,p2,p3}][[1]];
channelMomenta=OptionValue[ChannelMomenta];
If[MemberQ[channelMomenta,p4],
channelPs=channelMomenta;
,
channelPs=Sort[Complement[momenta,channelMomenta]];
];
channelM=OptionValue[ChannelMass];

SpinorChainReduced[Spinor["Helicity","Square",p4],Sum[Mom[channelPs[[i]]],{i,1,Length[channelPs]}],Mom[p2]+Mom[p3]-Mom[p1],Spinor["Helicity","Square",p4]]/(2Mass[p1]((channelMomenta/.List->Mandelstahm)-channelM^2))/.SpinorChainReduced->SpinorChain
];


xFactorReduced[p1_,Multiparticle[p2_,p3_],OptionsPattern[]]:=Module[{p4,momenta,channelMomenta,channelPs,channelM},
momenta=OptionValue[Momenta];
p4=Complement[momenta,{p1,p2,p3}][[1]];
channelMomenta=OptionValue[ChannelMomenta];
If[MemberQ[channelMomenta,p4],
channelPs=channelMomenta;
,
channelPs=Sort[Complement[momenta,channelMomenta]];
];
channelM=OptionValue[ChannelMass];

SpinorChainReduced[Spinor["Helicity","Square",p4],Sum[Mom[channelPs[[i]]],{i,1,Length[channelPs]}],Mom[p2]+Mom[p3]-Mom[p1],Spinor["Helicity","Square",p4]]/(2Mass[p1]((channelMomenta/.List->Mandelstahm)-channelM^2))/.SpinorChainReduced->SpinorChain
];


xTildeFactorReduced[p1_,Multiparticle[p2_,p3_],OptionsPattern[]]:=Module[{p4,momenta,channelMomenta,channelPs,channelM},
momenta=OptionValue[Momenta];
p4=Complement[momenta,{p1,p2,p3}][[1]];
channelMomenta=OptionValue[ChannelMomenta];
If[MemberQ[channelMomenta,p4],
channelPs=channelMomenta;
,
channelPs=Sort[Complement[momenta,channelMomenta]];
];
channelM=OptionValue[ChannelMass];

SpinorChainReduced[Spinor["Helicity","Angle",p4],Sum[Mom[channelPs[[i]]],{i,1,Length[channelPs]}],Mom[p2]+Mom[p3]-Mom[p1],Spinor["Helicity","Angle",p4]]/(2Mass[p1]((channelMomenta/.List->Mandelstahm)-channelM^2))/.SpinorChainReduced->SpinorChain
];


Unprotect[Times];
Unprotect[Plus];
(*xij xtkl <ij>[kl] + xtij xkl [ij]<kl>*)
xFactorReduced[i_,j_]xTildeFactorReduced[k_,l_]SpinorChain[Spinor["Spin","Angle",i_],Spinor["Spin","Angle",j_]]SpinorChain[Spinor["Spin","Square",k_],Spinor["Spin","Square",l_]]+xTildeFactorReduced[i_,j_]xFactorReduced[k_,l_]SpinorChain[Spinor["Spin","Square",i_],Spinor["Spin","Square",j_]]SpinorChain[Spinor["Spin","Angle",k_],Spinor["Spin","Angle",l_]]:=-(SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",l]]SpinorChain[Spinor["Spin","Square",j],Spinor["Spin","Square",k]]+SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",k]]SpinorChain[Spinor["Spin","Square",j],Spinor["Spin","Square",l]]+SpinorChain[Spinor["Spin","Angle",j],Spinor["Spin","Angle",l]]SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",k]]+SpinorChain[Spinor["Spin","Angle",j],Spinor["Spin","Angle",k]]SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",l]]);

(*xij xtkl <ij>^2[kl] + xtij xkl [ij]^2<kl>*)
xFactorReduced[i_,j_]xTildeFactorReduced[k_,l_]SpinorChain[Spinor["Spin","Angle",i_],Spinor["Spin","Angle",j_]]^2SpinorChain[Spinor["Spin","Square",k_],Spinor["Spin","Square",l_]]+xTildeFactorReduced[i_,j_]xFactorReduced[k_,l_]SpinorChain[Spinor["Spin","Square",i_],Spinor["Spin","Square",j_]]^2SpinorChain[Spinor["Spin","Angle",k_],Spinor["Spin","Angle",l_]]:=
1/2(SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",j]]+SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",j]])(SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",l]]SpinorChain[Spinor["Spin","Square",j],Spinor["Spin","Square",k]]+SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",k]]SpinorChain[Spinor["Spin","Square",j],Spinor["Spin","Square",l]]+SpinorChain[Spinor["Spin","Angle",j],Spinor["Spin","Angle",l]]SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",k]]+SpinorChain[Spinor["Spin","Angle",j],Spinor["Spin","Angle",k]]SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",l]])+
1/2SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",j]]SpinorChain[Spinor["Spin","Square",k],Spinor["Spin","Square",l]](SpinorChain[Spinor["Spin","Square",i],Mom[l],Mom[k],Spinor["Spin","Square",j]]/Mass[l]/Mass[j]+SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",j]]*Mass[l]/Mass[j])+1/2SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",j]]SpinorChain[Spinor["Spin","Angle",k],Spinor["Spin","Angle",l]](SpinorChain[Spinor["Spin","Angle",i],Mom[l],Mom[k],Spinor["Spin","Angle",j]]/Mass[l]/Mass[j]+SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",j]]*Mass[l]/Mass[j]);

(*xij xtkl <ij>^2[kl]^2 + xtij xkl [ij]^2<kl>^2*)
xFactorReduced[i_,j_]xTildeFactorReduced[k_,l_]SpinorChain[Spinor["Spin","Angle",i_],Spinor["Spin","Angle",j_]]^2SpinorChain[Spinor["Spin","Square",k_],Spinor["Spin","Square",l_]]^2+xTildeFactorReduced[i_,j_]xFactorReduced[k_,l_]SpinorChain[Spinor["Spin","Square",i_],Spinor["Spin","Square",j_]]^2SpinorChain[Spinor["Spin","Angle",k_],Spinor["Spin","Angle",l_]]^2:=1/2(SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",j]]SpinorChain[Spinor["Spin","Square",k],Spinor["Spin","Square",l]]+SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",j]]SpinorChain[Spinor["Spin","Angle",k],Spinor["Spin","Angle",l]])(SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",l]]SpinorChain[Spinor["Spin","Square",j],Spinor["Spin","Square",k]]+SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",k]]SpinorChain[Spinor["Spin","Square",j],Spinor["Spin","Square",l]]+SpinorChain[Spinor["Spin","Angle",j],Spinor["Spin","Angle",l]]SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",k]]+SpinorChain[Spinor["Spin","Angle",j],Spinor["Spin","Angle",k]]SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",l]])+1/4SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",j]]SpinorChain[Spinor["Spin","Square",k],Spinor["Spin","Square",l]]^2(SpinorChain[Spinor["Spin","Square",i],Mom[l],Mom[k],Spinor["Spin","Square",j]]/Mass[l]/Mass[j]+SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",j]]*Mass[l]/Mass[j])+1/4SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",j]]SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",j]]SpinorChain[Spinor["Spin","Square",k],Spinor["Spin","Square",l]](SpinorChain[Spinor["Spin","Angle",k],Mom[j],Mom[i],Spinor["Spin","Angle",l]]/Mass[l]/Mass[j]+SpinorChain[Spinor["Spin","Angle",k],Spinor["Spin","Angle",l]]*Mass[j]/Mass[l])+1/4SpinorChain[Spinor["Spin","Square",k],Spinor["Spin","Square",l]]SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",j]]^2(SpinorChain[Spinor["Spin","Angle",k],Mom[j],Mom[i],Spinor["Spin","Angle",l]]/Mass[l]/Mass[j]+SpinorChain[Spinor["Spin","Angle",k],Spinor["Spin","Angle",l]]*Mass[i]/Mass[k])+1/4SpinorChain[Spinor["Spin","Square",k],Spinor["Spin","Square",l]]SpinorChain[Spinor["Spin","Angle",k],Spinor["Spin","Angle",l]]SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",j]](SpinorChain[Spinor["Spin","Square",i],Mom[l],Mom[k],Spinor["Spin","Square",j]]/Mass[l]/Mass[j]+SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",j]]*Mass[k]/Mass[i])+1/4SpinorChain[Spinor["Spin","Angle",k],Spinor["Spin","Angle",l]]SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",j]]^2(SpinorChain[Spinor["Spin","Square",k],Mom[j],Mom[i],Spinor["Spin","Square",l]]/Mass[l]/Mass[j]+SpinorChain[Spinor["Spin","Square",k],Spinor["Spin","Square",l]]*Mass[i]/Mass[k])+1/4SpinorChain[Spinor["Spin","Angle",k],Spinor["Spin","Angle",l]]SpinorChain[Spinor["Spin","Square",k],Spinor["Spin","Square",l]]SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",j]](SpinorChain[Spinor["Spin","Angle",i],Mom[l],Mom[k],Spinor["Spin","Angle",j]]/Mass[l]/Mass[j]+SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",j]]*Mass[k]/Mass[i])+1/4SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",j]]SpinorChain[Spinor["Spin","Angle",k],Spinor["Spin","Angle",l]]^2(SpinorChain[Spinor["Spin","Angle",i],Mom[l],Mom[k],Spinor["Spin","Angle",j]]/Mass[l]/Mass[j]+SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",j]]*Mass[l]/Mass[j])+1/4SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",j]]SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",j]]SpinorChain[Spinor["Spin","Angle",k],Spinor["Spin","Angle",l]](SpinorChain[Spinor["Spin","Square",k],Mom[j],Mom[i],Spinor["Spin","Square",l]]/Mass[l]/Mass[j]+SpinorChain[Spinor["Spin","Square",k],Spinor["Spin","Square",l]]*Mass[j]/Mass[l]);

Protect[Plus];
Protect[Times];


(*Unprotect[Times];
Unprotect[Plus];
(*xij xtlk <ij>[lk] + xtij xlk [ij]<lk>*)
xFactorReduced[i_,j_]xTildeFactorReduced[l_,k_]SpinorChain[Spinor["Spin","Angle",i_],Spinor["Spin","Angle",j_]]SpinorChain[Spinor["Spin","Square",l_],Spinor["Spin","Square",k_]]+xTildeFactorReduced[i_,j_]xFactorReduced[l_,k_]SpinorChain[Spinor["Spin","Square",i_],Spinor["Spin","Square",j_]]SpinorChain[Spinor["Spin","Angle",l_],Spinor["Spin","Angle",k_]]:=-(SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",k]]SpinorChain[Spinor["Spin","Square",j],Spinor["Spin","Square",l]]+SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",l]]SpinorChain[Spinor["Spin","Square",j],Spinor["Spin","Square",k]]+SpinorChain[Spinor["Spin","Angle",j],Spinor["Spin","Angle",k]]SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",l]]+SpinorChain[Spinor["Spin","Angle",j],Spinor["Spin","Angle",l]]SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",k]]);

(*xij xtlk <ij>^2[lk] + xtij xlk [ij]^2<lk>*)
xFactorReduced[i_,j_]xTildeFactorReduced[l_,k_]SpinorChain[Spinor["Spin","Angle",i_],Spinor["Spin","Angle",j_]]^2SpinorChain[Spinor["Spin","Square",l_],Spinor["Spin","Square",k_]]+xTildeFactorReduced[i_,j_]xFactorReduced[l_,k_]SpinorChain[Spinor["Spin","Square",i_],Spinor["Spin","Square",j_]]^2SpinorChain[Spinor["Spin","Angle",l_],Spinor["Spin","Angle",k_]]:=
-1/2(SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",j]]+SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",j]])(SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",k]]SpinorChain[Spinor["Spin","Square",j],Spinor["Spin","Square",l]]+SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",l]]SpinorChain[Spinor["Spin","Square",j],Spinor["Spin","Square",k]]+SpinorChain[Spinor["Spin","Angle",j],Spinor["Spin","Angle",k]]SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",l]]+SpinorChain[Spinor["Spin","Angle",j],Spinor["Spin","Angle",l]]SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",k]])+
1/2SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",j]]SpinorChain[Spinor["Spin","Square",l],Spinor["Spin","Square",k]](SpinorChain[Spinor["Spin","Square",i],Mom[l],Mom[k],Spinor["Spin","Square",j]]/Mass[l]/Mass[j]+SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",j]]*Mass[l]/Mass[j])+1/2SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",j]]SpinorChain[Spinor["Spin","Angle",l],Spinor["Spin","Angle",k]](SpinorChain[Spinor["Spin","Angle",i],Mom[l],Mom[k],Spinor["Spin","Angle",j]]/Mass[l]/Mass[j]+SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",j]]*Mass[l]/Mass[j]);

(*xij xtlk <ij>^2[lk]^2 + xtij xlk [ij]^2<lk>^2*)
xFactorReduced[i_,j_]xTildeFactorReduced[l_,k_]SpinorChain[Spinor["Spin","Angle",i_],Spinor["Spin","Angle",j_]]^2SpinorChain[Spinor["Spin","Square",l_],Spinor["Spin","Square",k_]]^2+xTildeFactorReduced[i_,j_]xFactorReduced[l_,k_]SpinorChain[Spinor["Spin","Square",i_],Spinor["Spin","Square",j_]]^2SpinorChain[Spinor["Spin","Angle",l_],Spinor["Spin","Angle",k_]]^2:=-1/2(SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",j]]SpinorChain[Spinor["Spin","Square",l],Spinor["Spin","Square",k]]+SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",j]]SpinorChain[Spinor["Spin","Angle",l],Spinor["Spin","Angle",k]])(SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",k]]SpinorChain[Spinor["Spin","Square",j],Spinor["Spin","Square",l]]+SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",l]]SpinorChain[Spinor["Spin","Square",j],Spinor["Spin","Square",k]]+SpinorChain[Spinor["Spin","Angle",j],Spinor["Spin","Angle",k]]SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",l]]+SpinorChain[Spinor["Spin","Angle",j],Spinor["Spin","Angle",l]]SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",k]])+1/4SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",j]]SpinorChain[Spinor["Spin","Square",l],Spinor["Spin","Square",k]]^2(SpinorChain[Spinor["Spin","Square",i],Mom[l],Mom[k],Spinor["Spin","Square",j]]/Mass[l]/Mass[j]+SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",j]]*Mass[l]/Mass[j])+1/4SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",j]]SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",j]]SpinorChain[Spinor["Spin","Square",l],Spinor["Spin","Square",k]](SpinorChain[Spinor["Spin","Angle",l],Mom[j],Mom[i],Spinor["Spin","Angle",k]]/Mass[l]/Mass[j]+SpinorChain[Spinor["Spin","Angle",l],Spinor["Spin","Angle",k]]*Mass[j]/Mass[l])+1/4SpinorChain[Spinor["Spin","Square",l],Spinor["Spin","Square",k]]SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",j]]^2(SpinorChain[Spinor["Spin","Angle",l],Mom[i],Mom[j],Spinor["Spin","Angle",k]]/Mass[l]/Mass[j]+SpinorChain[Spinor["Spin","Angle",l],Spinor["Spin","Angle",k]]*Mass[i]/Mass[k])+1/4SpinorChain[Spinor["Spin","Square",l],Spinor["Spin","Square",k]]SpinorChain[Spinor["Spin","Angle",l],Spinor["Spin","Angle",k]]SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",j]](SpinorChain[Spinor["Spin","Square",i],Mom[k],Mom[l],Spinor["Spin","Square",j]]/Mass[l]/Mass[j]+SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",j]]*Mass[k]/Mass[i])+1/4SpinorChain[Spinor["Spin","Angle",l],Spinor["Spin","Angle",k]]SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",j]]^2(SpinorChain[Spinor["Spin","Square",l],Mom[i],Mom[j],Spinor["Spin","Square",k]]/Mass[l]/Mass[j]+SpinorChain[Spinor["Spin","Square",l],Spinor["Spin","Square",k]]*Mass[i]/Mass[k])+1/4SpinorChain[Spinor["Spin","Angle",l],Spinor["Spin","Angle",k]]SpinorChain[Spinor["Spin","Square",l],Spinor["Spin","Square",k]]SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",j]](SpinorChain[Spinor["Spin","Angle",i],Mom[k],Mom[l],Spinor["Spin","Angle",j]]/Mass[l]/Mass[j]+SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",j]]*Mass[k]/Mass[i])+1/4SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",j]]SpinorChain[Spinor["Spin","Angle",l],Spinor["Spin","Angle",k]]^2(SpinorChain[Spinor["Spin","Angle",i],Mom[l],Mom[k],Spinor["Spin","Angle",j]]/Mass[l]/Mass[j]+SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",j]]*Mass[l]/Mass[j])+1/4SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",j]]SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",j]]SpinorChain[Spinor["Spin","Angle",l],Spinor["Spin","Angle",k]](SpinorChain[Spinor["Spin","Square",l],Mom[j],Mom[i],Spinor["Spin","Square",k]]/Mass[l]/Mass[j]+SpinorChain[Spinor["Spin","Square",l],Spinor["Spin","Square",k]]*Mass[j]/Mass[l]);

Protect[Plus];
Protect[Times];*)


(*Unprotect[Times];

xFactorReduced[i_,j_]SpinorChain[Spinor["Spin","Angle",i_],Spinor["Spin","Angle",j_]]:=-(SpinorChain[Spinor["Spin","Angle",j],Spinor["Helicity","Angle",xq]]SpinorChain[Spinor["Spin","Square",i],Spinor["Helicity","Square",xk]]+SpinorChain[Spinor["Spin","Angle",i],Spinor["Helicity","Angle",xq]]SpinorChain[Spinor["Spin","Square",j],Spinor["Helicity","Square",xk]])/SpinorChain[Spinor["Helicity","Angle",xq],Spinor["Helicity","Angle",xk]];
xTildeFactorReduced[i_,j_]SpinorChain[Spinor["Spin","Square",i_],Spinor["Spin","Square",j_]]:=-(SpinorChain[Spinor["Spin","Square",j],Spinor["Helicity","Square",xq]]SpinorChain[Spinor["Spin","Angle",i],Spinor["Helicity","Angle",xk]]+SpinorChain[Spinor["Spin","Square",i],Spinor["Helicity","Square",xq]]SpinorChain[Spinor["Spin","Angle",j],Spinor["Helicity","Angle",xk]])/SpinorChain[Spinor["Helicity","Square",xq],Spinor["Helicity","Square",xk]];

xFactorReduced[i_,j_]SpinorChain[Spinor["Spin","Angle",i_],Spinor["Spin","Angle",j_]]^n_/;n>0:=-SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",j]](SpinorChain[Spinor["Spin","Angle",j],Spinor["Helicity","Angle",xq]]SpinorChain[Spinor["Spin","Square",i],Spinor["Helicity","Square",xk]]+SpinorChain[Spinor["Spin","Angle",i],Spinor["Helicity","Angle",xq]]SpinorChain[Spinor["Spin","Square",j],Spinor["Helicity","Square",xk]])/SpinorChain[Spinor["Helicity","Angle",xq],Spinor["Helicity","Angle",xk]];
xTildeFactorReduced[i_,j_]SpinorChain[Spinor["Spin","Square",i_],Spinor["Spin","Square",j_]]^n_/;n>0:=-SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",j]](SpinorChain[Spinor["Spin","Square",j],Spinor["Helicity","Square",xq]]SpinorChain[Spinor["Spin","Angle",i],Spinor["Helicity","Angle",xk]]+SpinorChain[Spinor["Spin","Square",i],Spinor["Helicity","Square",xq]]SpinorChain[Spinor["Spin","Angle",j],Spinor["Helicity","Angle",xk]])/SpinorChain[Spinor["Helicity","Square",xq],Spinor["Helicity","Square",xk]];

xFactorReduced[i_,j_]^n_ SpinorChain[Spinor["Spin","Angle",i_],Spinor["Spin","Angle",j_]]/;n>0:=xFactorReduced[i,j]^(n-1)(SpinorChain[Spinor["Spin","Angle",j],Spinor["Helicity","Angle",xq]]SpinorChain[Spinor["Spin","Square",i],Spinor["Helicity","Square",xk]]+SpinorChain[Spinor["Spin","Angle",i],Spinor["Helicity","Angle",xq]]SpinorChain[Spinor["Spin","Square",j],Spinor["Helicity","Square",xk]])/SpinorChain[Spinor["Helicity","Angle",xq],Spinor["Helicity","Angle",xk]];
xTildeFactorReduced[i_,j_]^n_ SpinorChain[Spinor["Spin","Square",i_],Spinor["Spin","Square",j_]]/;n>0:=xTildeFactorReduced[i,j]^(n-1)(SpinorChain[Spinor["Spin","Square",j],Spinor["Helicity","Square",xq]]SpinorChain[Spinor["Spin","Angle",i],Spinor["Helicity","Angle",xk]]+SpinorChain[Spinor["Spin","Square",i],Spinor["Helicity","Square",xq]]SpinorChain[Spinor["Spin","Angle",j],Spinor["Helicity","Angle",xk]])/SpinorChain[Spinor["Helicity","Square",xq],Spinor["Helicity","Square",xk]];

xFactorReduced[i_,j_]^m_ SpinorChain[Spinor["Spin","Angle",i_],Spinor["Spin","Angle",j_]]^n_/;(m>0&&n>0):=xFactorReduced[i,j]^(m-1)SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",j]]^(n-1)(SpinorChain[Spinor["Spin","Angle",j],Spinor["Helicity","Angle",xq]]SpinorChain[Spinor["Spin","Square",i],Spinor["Helicity","Square",xk]]+SpinorChain[Spinor["Spin","Angle",i],Spinor["Helicity","Angle",xq]]SpinorChain[Spinor["Spin","Square",j],Spinor["Helicity","Square",xk]])/SpinorChain[Spinor["Helicity","Angle",xq],Spinor["Helicity","Angle",xk]];
xTildeFactorReduced[i_,j_]^m_ SpinorChain[Spinor["Spin","Square",i_],Spinor["Spin","Square",j_]]^n_/;(m>0&&n>0):=xTildeFactorReduced[i,j]^(m-1)SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",j]]^(n-1)(SpinorChain[Spinor["Spin","Square",j],Spinor["Helicity","Square",xq]]SpinorChain[Spinor["Spin","Angle",i],Spinor["Helicity","Angle",xk]]+SpinorChain[Spinor["Spin","Square",i],Spinor["Helicity","Square",xq]]SpinorChain[Spinor["Spin","Angle",j],Spinor["Helicity","Angle",xk]])/SpinorChain[Spinor["Helicity","Square",xq],Spinor["Helicity","Square",xk]];

Protect[Times];*)


(*xFactorReduced[p1_,Multiparticle[p2_,p3_],p4_]:=-SpinorChain[Spinor["Helicity","Angle",p4],Mom[p1],Spinor["Helicity","Square",Complement[{1,2,3,4},{p1,p2,p3}][[1]]]]/(Mass[p1]SpinorChain[Spinor["Helicity","Angle",p4],Spinor["Helicity","Angle",Complement[{1,2,3,4},{p1,p2,p3}][[1]]]]);
xFactorReduced[Multiparticle[p2_,p3_],p1_,p4_]:=SpinorChain[Spinor["Helicity","Angle",p4],Mom[p1],Spinor["Helicity","Square",Complement[{1,2,3,4},{p1,p2,p3}][[1]]]]/(Mass[p1]SpinorChain[Spinor["Helicity","Angle",p4],Spinor["Helicity","Angle",Complement[{1,2,3,4},{p1,p2,p3}][[1]]]]);
xTildeFactorReduced[p1_,Multiparticle[p2_,p3_],p4_]:=-SpinorChain[Spinor["Helicity","Square",p4],Mom[p1],Spinor["Helicity","Angle",Complement[{1,2,3,4},{p1,p2,p3}][[1]]]]/(Mass[p1]SpinorChain[Spinor["Helicity","Square",p4],Spinor["Helicity","Square",Complement[{1,2,3,4},{p1,p2,p3}][[1]]]]);
xTildeFactorReduced[Multiparticle[p2_,p3_],p1_,p4_]:=SpinorChain[Spinor["Helicity","Square",p4],Mom[p1],Spinor["Helicity","Angle",Complement[{1,2,3,4},{p1,p2,p3}][[1]]]]/(Mass[p1]SpinorChain[Spinor["Helicity","Square",p4],Spinor["Helicity","Square",Complement[{1,2,3,4},{p1,p2,p3}][[1]]]]);
*)


(*ReduceXFactors[exp_]:=exp//.{
xFactor[p1a___,Multiparticle[p2_,p3_],p1b___]xTildeFactor[p4a___,Multiparticle[p5_,p6_],p4b___]:>
xFactorReduced[p1a,Multiparticle[p2,p3],p1b,Complement[{1,2,3,4},{p4a,p4b,p5,p6}][[1]]]
xTildeFactorReduced[p4a,Multiparticle[p5,p6],p4b,Complement[{1,2,3,4},{p1a,p1b,p2,p3}][[1]]],
xFactor[p1a___,Multiparticle[p2_,p3_],p1b___]xFactor[p4a___,Multiparticle[p5_,p6_],p4b___]:>
xFactorReduced[p1a,Multiparticle[p2,p3],p1b,Complement[{1,2,3,4},{p4a,p4b,p5,p6}][[1]]]
xFactorReduced[p4a,Multiparticle[p5,p6],p4b,Complement[{1,2,3,4},{p1a,p1b,p2,p3}][[1]]],
xTildeFactor[p1a___,Multiparticle[p2_,p3_],p1b___]xTildeFactor[p4a___,Multiparticle[p5_,p6_],p4b___]:>
xTildeFactorReduced[p1a,Multiparticle[p2,p3],p1b,Complement[{1,2,3,4},{p4a,p4b,p5,p6}][[1]]]
xTildeFactorReduced[p4a,Multiparticle[p5,p6],p4b,Complement[{1,2,3,4},{p1a,p1b,p2,p3}][[1]]]
}*)


(* ::Input:: *)
(*(*More importantly, I need a way to specify that p3+p4=-p1-p2.  This will be important when going beyond 4-point amplitudes.*)*)


(* ::Input::Initialization:: *)
(*ReduceXFactorsInternalAngle[exp_]:=exp//.{
xFactor[p1_,p2_]xTildeFactor[p3_,p4_]SpinorChain[Spinor["Spin","Square",p3_],Spinor["Spin","Square",p4_]]/;(Sort[{p1,p2,p3,p4}]==Union[{p1,p2,p3,p4}]):>xFactor[p1,p2]xTildeFactor[p3,p4]SpinorChain[Spinor["Spin","Angle",p3],Spinor["Spin","Angle",p4]],

xFactor[p1_,p2_]xTildeFactor[p3_,p4_]SpinorChain[Spinor["Spin","Square",p3_],Spinor["Spin","Square",p4_]]^n_Integer/;(Sort[{p1,p2,p3,p4}]==Union[{p1,p2,p3,p4}]):>xFactor[p1,p2]xTildeFactor[p3,p4]SpinorChain[Spinor["Spin","Angle",p3],Spinor["Spin","Angle",p4]]^n,

xTildeFactor[p1_,p2_]xFactor[p3_,p4_]+xTildeFactor[p3_,p4_]xFactor[p1_,p2_]/;(Min[p1,p2]==Min[p1,p2,p3,p4]):>- (MomProd[p2,p4]-MomProd[p1,p4]-MomProd[p2,p3]+MomProd[p1,p3])/(2Mass[p2] Mass[p4])
}

ReduceXFactorsInternalSquare[exp_]:=exp//.{
xFactor[p1_,p2_]xTildeFactor[p3_,p4_]SpinorChain[Spinor["Spin","Angle",p1_],Spinor["Spin","Angle",p2_]]/;(Sort[{p1,p2,p3,p4}]==Union[{p1,p2,p3,p4}]):>xFactor[p1,p2]xTildeFactor[p3,p4]SpinorChain[Spinor["Spin","Square",p1],Spinor["Spin","Square",p2]],

xFactor[p1_,p2_]xTildeFactor[p3_,p4_]SpinorChain[Spinor["Spin","Angle",p1_],Spinor["Spin","Angle",p2_]]^n_Integer/;(Sort[{p1,p2,p3,p4}]==Union[{p1,p2,p3,p4}]):>xFactor[p1,p2]xTildeFactor[p3,p4]SpinorChain[Spinor["Spin","Square",p1],Spinor["Spin","Square",p2]]^n,

xTildeFactor[p1_,p2_]xFactor[p3_,p4_]+xTildeFactor[p3_,p4_]xFactor[p1_,p2_]/;(Min[p1,p2]==Min[p1,p2,p3,p4]):>- (MomProd[p2,p4]-MomProd[p1,p4]-MomProd[p2,p3]+MomProd[p1,p3])/(2Mass[p2] Mass[p4])
}*)


(*ReduceXFactorsInternalSquareAngle[exp_]:=exp//.{
xFactor[p1_,p2_]xTildeFactor[p3_,p4_]SpinorChain[Spinor["Spin","Angle",p1_],Spinor["Spin","Angle",p2_]]/;(Sort[{p1,p2,p3,p4}]==Union[{p1,p2,p3,p4}]&&Min[p1,p2]<Min[p3,p4]):>xFactor[p1,p2]xTildeFactor[p3,p4]SpinorChain[Spinor["Spin","Square",p1],Spinor["Spin","Square",p2]],

xFactor[p1_,p2_]xTildeFactor[p3_,p4_]SpinorChain[Spinor["Spin","Square",p3_],Spinor["Spin","Square",p4_]]/;(Sort[{p1,p2,p3,p4}]==Union[{p1,p2,p3,p4}]&&Min[p1,p2]<Min[p3,p4]):>xFactor[p1,p2]xTildeFactor[p3,p4]SpinorChain[Spinor["Spin","Angle",p3],Spinor["Spin","Angle",p4]],

xTildeFactor[p1_,p2_]xFactor[p3_,p4_]+xTildeFactor[p3_,p4_]xFactor[p1_,p2_]/;(Min[p1,p2]==Min[p1,p2,p3,p4]):>- (MomProd[p2,p4]-MomProd[p1,p4]-MomProd[p2,p3]+MomProd[p1,p3])/(2Mass[p2] Mass[p4])
}*)


(*ReduceXFactorsInternalAngleSquare[exp_]:=exp//.{
xFactor[p1_,p2_]xTildeFactor[p3_,p4_]SpinorChain[Spinor["Spin","Angle",p1_],Spinor["Spin","Angle",p2_]]/;(Sort[{p1,p2,p3,p4}]==Union[{p1,p2,p3,p4}]&&Min[p1,p2]>Min[p3,p4]):>xFactor[p1,p2]xTildeFactor[p3,p4]SpinorChain[Spinor["Spin","Square",p1],Spinor["Spin","Square",p2]],

xFactor[p1_,p2_]xTildeFactor[p3_,p4_]SpinorChain[Spinor["Spin","Square",p3_],Spinor["Spin","Square",p4_]]/;(Sort[{p1,p2,p3,p4}]==Union[{p1,p2,p3,p4}]&&Min[p1,p2]>Min[p3,p4]):>xFactor[p1,p2]xTildeFactor[p3,p4]SpinorChain[Spinor["Spin","Angle",p3],Spinor["Spin","Angle",p4]],

xTildeFactor[p1_,p2_]xFactor[p3_,p4_]+xTildeFactor[p3_,p4_]xFactor[p1_,p2_]/;(Min[p1,p2]==Min[p1,p2,p3,p4]):>- (MomProd[p2,p4]-MomProd[p1,p4]-MomProd[p2,p3]+MomProd[p1,p3])/(2Mass[p2] Mass[p4])
}*)


(*TransformXXTAngleToSquare[exp_,pi_,pj_]:=exp/.{
xFactor[pi,pj]xTildeFactor[pk_,pl_]SpinorChain[Spinor["Spin","Angle",pi],Spinor["Spin","Angle",pj]]/;(Length[Union[{pi,pj,pk,pl}]]==4):>xFactor[pi,pj]xTildeFactor[pk,pl]SpinorChain[Spinor["Spin","Square",pi],Spinor["Spin","Square",pj]]+SpinorChain[Spinor["Spin","Square",pi],Mom[pl],Mom[pk],Spinor["Spin","Square",pj]]/(Mass[pi]Mass[pk])+SpinorChain[Spinor["Spin","Square",pi],Spinor["Spin","Square",pj]]*Mass[pk]/Mass[pi],

xTildeFactor[pi,pj]xFactor[pk_,pl_]SpinorChain[Spinor["Spin","Angle",pi],Spinor["Spin","Angle",pj]]/;(Length[Union[{pi,pj,pk,pl}]]==4):>xTildeFactor[pi,pj]xFactor[pk,pl]SpinorChain[Spinor["Spin","Square",pi],Spinor["Spin","Square",pj]]-SpinorChain[Spinor["Spin","Angle",pi],Mom[pl],Mom[pk],Spinor["Spin","Angle",pj]]/(Mass[pi]Mass[pk])-SpinorChain[Spinor["Spin","Angle",pi],Spinor["Spin","Angle",pj]]*Mass[pk]/Mass[pi]
}*)


(*TransformXXTSquareToAngle[exp_,pi_,pj_]:=exp/.{
xFactor[pi,pj]xTildeFactor[pk_,pl_]SpinorChain[Spinor["Spin","Square",pi],Spinor["Spin","Square",pj]]/;(Length[Union[{pi,pj,pk,pl}]]==4):>xFactor[pi,pj]xTildeFactor[pk,pl]SpinorChain[Spinor["Spin","Angle",pi],Spinor["Spin","Angle",pj]]-SpinorChain[Spinor["Spin","Square",pi],Mom[pl],Mom[pk],Spinor["Spin","Square",pj]]/(Mass[pi]Mass[pk])-SpinorChain[Spinor["Spin","Square",pi],Spinor["Spin","Square",pj]]*Mass[pk]/Mass[pi],

xTildeFactor[pi,pj]xFactor[pk_,pl_]SpinorChain[Spinor["Spin","Square",pi],Spinor["Spin","Square",pj]]/;(Length[Union[{pi,pj,pk,pl}]]==4):>xTildeFactor[pi,pj]xFactor[pk,pl]SpinorChain[Spinor["Spin","Angle",pi],Spinor["Spin","Angle",pj]]+SpinorChain[Spinor["Spin","Angle",pi],Mom[pl],Mom[pk],Spinor["Spin","Angle",pj]]/(Mass[pi]Mass[pk])+SpinorChain[Spinor["Spin","Angle",pi],Spinor["Spin","Angle",pj]]*Mass[pk]/Mass[pi]
}*)


(*ReduceXXTpXTX[exp_]:=exp/.xTildeFactor[p1_,p2_]xFactor[p3_,p4_]+xTildeFactor[p3_,p4_]xFactor[p1_,p2_]/;(Length[Union[{p1,p2,p3,p4}]]==4):>- (MomProd[p2,p4]-MomProd[p1,p4]-MomProd[p2,p3]+MomProd[p1,p3])/(2Mass[p2] Mass[p4])
*)


(* ::Subsubsection::Closed:: *)
(*color Tracing*)


(* ::Text:: *)
(*These rules come from  Computer Physics Communications 48 (1988) 327- 334 and is the basis of CalcHEP.*)


(* ::Input::Initialization:: *)
ReduceColor[exp_]:=Module[{colorReplacementRules},
colorReplacementRules={
colorT[a_,k_,i_]colorT[b_,i_,k_]:>colorTrace[colorT[a],colorT[b]],
colorT[a_,k_,i_]colorT[b_,i_,j_]colorT[c_,j_,k_]:>colorTrace[colorT[a],colorT[b],colorT[c]],
colorT[a_,i_,j_]colorT[b_,j_,k_]colorT[c_,k_,l_]colorT[d_,l_,i_]:>colorTrace[colorT[a],colorT[b],colorT[c],colorT[d]],
colorf[a_,b_,c_]:>2(colorTrace[colorT[a],colorT[b],colorT[c]]-colorTrace[colorT[a],colorT[c],colorT[b]]),
colorTrace[X___,colorT[a_],Y___,colorT[a_],Z___]:>1/2 (colorTrace[Y]colorTrace[X,Z]-1/3 colorTrace[X,Y,Z]),
colorTrace[X___,colorT[a_],Y___]colorTrace[Z___,colorT[a_],U___]:>1/2 (colorTrace[X,U,Z,Y]-1/3 colorTrace[X,Y]colorTrace[Z,U]),
colorTrace[X___,colorT[a_],Y___]^2:>1/2 (colorTrace[X,Y,X,Y]-1/3 colorTrace[X,Y]colorTrace[X,Y]),
colorTrace[colorT[a_]]->0,
colorTrace[]->3
};
Expand[Expand[Expand[exp//.colorReplacementRules]//.colorReplacementRules]//.colorReplacementRules]//.colorReplacementRules
];


(* ::Subsection::Closed:: *)
(*Utilities*)


(* ::Subsubsection::Closed:: *)
(*SpinToHelicitySpinors*)


(* ::Input::Initialization:: *)
SpinToHelicitySpinors[amp_,{pn1_,spn1_,hel1_},{pn2_,spn2_,hel2_},{pn3_,spn3_,hel3_},{pn4_,spn4_,hel4_}]:=SpinToHelicitySpinors[SpinToHelicitySpinors[SpinToHelicitySpinors[SpinToHelicitySpinors[amp,{pn1,spn1,hel1}],{pn2,spn2,hel2}],{pn3,spn3,hel3}],{pn4,spn4,hel4}];

SpinToHelicitySpinors[amp_,{pn1_,spn1_,hel1_},{pn2_,spn2_,hel2_},{pn3_,spn3_,hel3_}]:=SpinToHelicitySpinors[SpinToHelicitySpinors[SpinToHelicitySpinors[amp,{pn1,spn1,hel1}],{pn2,spn2,hel2}],{pn3,spn3,hel3}];

SpinToHelicitySpinors[amp_,{pn1_,spn1_,hel1_},{pn2_,spn2_,hel2_}]:=SpinToHelicitySpinors[SpinToHelicitySpinors[amp,{pn1,spn1,hel1}],{pn2,spn2,hel2}];

SpinToHelicitySpinors[amp_,{pn_,spn_,hel_}]:=Module[{res=Expand[amp]},
(*Currently only implemented to linear order*)
Which[{spn,hel}=={1,-1},
res=res//.{
SpinorChain[a___,Spinor["Spin","Angle",pn],b___]SpinorChain[c___,Spinor["Spin","Angle",pn],d___]:>(En[pn]+MomMagnitude[pn])/(2En[pn])SpinorChain[a,Spinor["Helicity","Angle",pn],b]SpinorChain[c,Spinor["Helicity","Angle",pn],d],
SpinorChain[a___,Spinor["Spin","Square",pn],b___]SpinorChain[c___,Spinor["Spin","Angle",pn],d___]:>-Mass[pn]/Sqrt[2En[pn]]SpinorChain[a,Spinor["Zeta","Square",pn],b]SpinorChain[c,Spinor["Helicity","Angle",pn],d],
SpinorChain[Spinor["Spin","Square",pn],b___,Spinor["Spin","Angle",pn]]:>-Mass[pn]/Sqrt[2En[pn]]SpinorChain[Spinor["Zeta","Square",pn],b,Spinor["Helicity","Angle",pn]],
SpinorChain[Spinor["Spin","Angle",pn],d___,Spinor["Spin","Square",pn]]:>-Mass[pn]/Sqrt[2En[pn]]SpinorChain[Spinor["Helicity","Angle",pn],d,Spinor["Zeta","Square",pn]],
SpinorChain[a___,Spinor["Spin","Square",pn],b___]SpinorChain[c___,Spinor["Spin","Square",pn],d___]:>(En[pn]-MomMagnitude[pn]) SpinorChain[a,Spinor["Zeta","Square",pn],b]SpinorChain[c,Spinor["Zeta","Square",pn],d],
SpinorChain[a___,Spinor["Spin","Angle",pn],b___]^2:>(En[pn]+MomMagnitude[pn])/(2En[pn])SpinorChain[a,Spinor["Helicity","Angle",pn],b]^2,
SpinorChain[a___,Spinor["Spin","Square",pn],b___]^2:>(En[pn]-MomMagnitude[pn]) SpinorChain[a,Spinor["Zeta","Square",pn],b]^2
};,
{spn,hel}=={1,0},
res=res//.{
SpinorChain[a___,Spinor["Spin","Angle",pn],b___]SpinorChain[c___,Spinor["Spin","Angle",pn],d___]:>  Mass[pn]/(Sqrt[2]Sqrt[2En[pn]]) (SpinorChain[a,Spinor["Zeta","Angle",pn],b]SpinorChain[c,Spinor["Helicity","Angle",pn],d]+ SpinorChain[a,Spinor["Helicity","Angle",pn],b]SpinorChain[c,Spinor["Zeta","Angle",pn],d]),
SpinorChain[a___,Spinor["Spin","Square",pn],b___]SpinorChain[c___,Spinor["Spin","Angle",pn],d___]:>1/Sqrt[2]((En[pn]+MomMagnitude[pn])/(2En[pn])SpinorChain[a,Spinor["Helicity","Square",pn],b]SpinorChain[c,Spinor["Helicity","Angle",pn],d]-(En[pn]-MomMagnitude[pn]) SpinorChain[a,Spinor["Zeta","Square",pn],b]SpinorChain[c,Spinor["Zeta","Angle",pn],d]),
SpinorChain[Spinor["Spin","Square",pn],b___,Spinor["Spin","Angle",pn]]:>1/Sqrt[2]((En[pn]+MomMagnitude[pn])/(2En[pn])SpinorChain[Spinor["Helicity","Square",pn],b,Spinor["Helicity","Angle",pn]]-(En[pn]-MomMagnitude[pn]) SpinorChain[Spinor["Zeta","Square",pn],b,Spinor["Zeta","Angle",pn]]),
SpinorChain[Spinor["Spin","Angle",pn],d___,Spinor["Spin","Square",pn]]:>1/Sqrt[2]((En[pn]+MomMagnitude[pn])/(2En[pn])SpinorChain[Spinor["Helicity","Angle",pn],d,Spinor["Helicity","Square",pn]]-(En[pn]-MomMagnitude[pn]) SpinorChain[Spinor["Zeta","Angle",pn],d,Spinor["Zeta","Square",pn]]),
SpinorChain[a___,Spinor["Spin","Square",pn],b___]SpinorChain[c___,Spinor["Spin","Square",pn],d___]:>-Mass[pn]/(Sqrt[2]Sqrt[2En[pn]])(SpinorChain[a,Spinor["Zeta","Square",pn],b]SpinorChain[c,Spinor["Helicity","Square",pn],d]+ SpinorChain[a,Spinor["Helicity","Square",pn],b]SpinorChain[c,Spinor["Zeta","Square",pn],d]),
SpinorChain[a___,Spinor["Spin","Angle",pn],b___]^2:>  Sqrt[2]Mass[pn]/(Sqrt[2En[pn]]) SpinorChain[a,Spinor["Zeta","Angle",pn],b]SpinorChain[a,Spinor["Helicity","Angle",pn],b],
SpinorChain[a___,Spinor["Spin","Square",pn],b___]^2:>-Sqrt[2]Mass[pn]/(Sqrt[2En[pn]])SpinorChain[a,Spinor["Zeta","Square",pn],b]SpinorChain[a,Spinor["Helicity","Square",pn],b]
};,
{spn,hel}=={1,1},
res=res//.{
SpinorChain[a___,Spinor["Spin","Angle",pn],b___]SpinorChain[c___,Spinor["Spin","Angle",pn],d___]:>(En[pn]-MomMagnitude[pn]) SpinorChain[a,Spinor["Zeta","Angle",pn],b]SpinorChain[c,Spinor["Zeta","Angle",pn],d],
SpinorChain[a___,Spinor["Spin","Square",pn],b___]SpinorChain[c___,Spinor["Spin","Angle",pn],d___]:>-Mass[pn]/Sqrt[2En[pn]]SpinorChain[a,Spinor["Helicity","Square",pn],b]SpinorChain[c,Spinor["Zeta","Angle",pn],d],
SpinorChain[Spinor["Spin","Square",pn],b___,Spinor["Spin","Angle",pn]]:>-Mass[pn]/Sqrt[2En[pn]]SpinorChain[Spinor["Helicity","Square",pn],b,Spinor["Zeta","Angle",pn]],
SpinorChain[Spinor["Spin","Angle",pn],d___,Spinor["Spin","Square",pn]]:>-Mass[pn]/Sqrt[2En[pn]]SpinorChain[Spinor["Zeta","Angle",pn],d,Spinor["Helicity","Square",pn]],
SpinorChain[a___,Spinor["Spin","Square",pn],b___]SpinorChain[c___,Spinor["Spin","Square",pn],d___]:>(En[pn]+MomMagnitude[pn])/(2En[pn])SpinorChain[a,Spinor["Helicity","Square",pn],b]SpinorChain[c,Spinor["Helicity","Square",pn],d],
SpinorChain[a___,Spinor["Spin","Angle",pn],b___]^2:>Mass[pn]^2/(2En[pn]) SpinorChain[a,Spinor["Zeta","Angle",pn],b]^2,
SpinorChain[a___,Spinor["Spin","Square",pn],b___]^2:>(En[pn]+MomMagnitude[pn])/(2En[pn])SpinorChain[a,Spinor["Helicity","Square",pn],b]^2
};,
{spn,hel}=={1/2,-1/2},
res=res//.{
SpinorChain[a___,Spinor["Spin","Angle",pn],b___]:>Sqrt[En[pn]+MomMagnitude[pn]]/Sqrt[2En[pn]]SpinorChain[a,Spinor["Helicity","Angle",pn],b],
SpinorChain[a___,Spinor["Spin","Square",pn],b___]:>-Sqrt[En[pn]-MomMagnitude[pn]]SpinorChain[a,Spinor["Zeta","Square",pn],b]
};,
{spn,hel}=={1/2,1/2},
res=res//.{
SpinorChain[a___,Spinor["Spin","Angle",pn],b___]:>Sqrt[En[pn]-MomMagnitude[pn]] SpinorChain[a,Spinor["Zeta","Angle",pn],b],
SpinorChain[a___,Spinor["Spin","Square",pn],b___]:>Sqrt[En[pn]+MomMagnitude[pn]]/Sqrt[2En[pn]]SpinorChain[a,Spinor["Helicity","Square",pn],b]
};
];
res=res//.{
Mandelstahm[a_,b_]/;(a===pn||b===pn):>Mass[a]^2+Mass[b]^2+Sum[SpinorChain[Spinor["Spin","Upper","Angle",a,sa],Spinor["Spin","Upper","Angle",b,sb]]SpinorChain[Spinor["Spin","Lower","Square",b,sb],Spinor["Spin","Lower","Square",a,sa]],{sa,-1/2,1/2,1},{sb,-1/2,1/2,1}]
};
res=res//.{
SpinorChain[a___,Spinor["Spin","Upper","Angle",pn,-1/2],b___]:>Sqrt[En[pn]+MomMagnitude[pn]]/Sqrt[2En[pn]]SpinorChain[a,Spinor["Helicity","Angle",pn],b],
SpinorChain[a___,Spinor["Spin","Upper","Angle",pn,+1/2],b___]:>Sqrt[En[pn]-MomMagnitude[pn]]SpinorChain[a,Spinor["Zeta","Angle",pn],b],
SpinorChain[a___,Spinor["Spin","Lower","Angle",pn,+1/2],b___]:>-Sqrt[En[pn]+MomMagnitude[pn]]/Sqrt[2En[pn]]SpinorChain[a,Spinor["Helicity","Angle",pn],b],
SpinorChain[a___,Spinor["Spin","Lower","Angle",pn,-1/2],b___]:>Sqrt[En[pn]-MomMagnitude[pn]]SpinorChain[a,Spinor["Zeta","Angle",pn],b],

SpinorChain[a___,Spinor["Spin","Upper","Square",pn,+1/2],b___]:>Sqrt[En[pn]+MomMagnitude[pn]]/Sqrt[2En[pn]]SpinorChain[a,Spinor["Helicity","Square",pn],b],
SpinorChain[a___,Spinor["Spin","Upper","Square",pn,-1/2],b___]:>-Sqrt[En[pn]-MomMagnitude[pn]]SpinorChain[a,Spinor["Zeta","Square",pn],b],
SpinorChain[a___,Spinor["Spin","Lower","Square",pn,-1/2],b___]:>Sqrt[En[pn]+MomMagnitude[pn]]/Sqrt[2En[pn]]SpinorChain[a,Spinor["Helicity","Square",pn],b],
SpinorChain[a___,Spinor["Spin","Lower","Square",pn,+1/2],b___]:>Sqrt[En[pn]-MomMagnitude[pn]]SpinorChain[a,Spinor["Zeta","Square",pn],b]
};
res
]


(* ::Subsubsection::Closed:: *)
(*ExtractEnergy*)


(* ::Input::Initialization:: *)
ExtractEnergy[amp_,particleSpins_List]:=Module[{res=amp,particleSpin},
(*Insert spin indices for massive particles*)
Do[
If[Length[particleSpins[[ii]]]>1,particleSpin={particleSpins[[ii,2]],particleSpins[[ii,3]]};,
particleSpin={};];
Which[
particleSpin=={1/2,1/2},res=MakeIndicesExplicit[res,particleSpins[[ii,1]],{1/2}];,
particleSpin=={1/2,-1/2},res=MakeIndicesExplicit[res,particleSpins[[ii,1]],{-1/2}];,
particleSpin=={1,1},res=MakeIndicesExplicit[res,particleSpins[[ii,1]],{1/2,1/2}];,
particleSpin=={1,-1},res=MakeIndicesExplicit[res,particleSpins[[ii,1]],{-1/2,-1/2}];,
particleSpin=={1,0},res=1/Sqrt[2] (MakeIndicesExplicit[res,particleSpins[[ii,1]],{-1/2,1/2}]+MakeIndicesExplicit[res,particleSpins[[ii,1]],{1/2,-1/2}]);
],{ii,1,Length[particleSpins]}];

(*Extract explicit momentum*)
Do[res=ExtractEnergy1[res,particleSpins[[ii,1]],En[particleSpins[[ii,1]]],MomMagnitude[particleSpins[[ii,1]]],Theta[particleSpins[[ii,1]]],Phi[particleSpins[[ii,1]]]];
,{ii,1,Length[particleSpins]}];
Do[res=ExtractEnergy2[res,particleSpins[[ii,1]],En[particleSpins[[ii,1]]],MomMagnitude[particleSpins[[ii,1]]],Theta[particleSpins[[ii,1]]],Phi[particleSpins[[ii,1]]]];
,{ii,1,Length[particleSpins]}];


res/.{
SpinorChain[a__]:>Dot[a],
SpinorTrace[a__]:>Tr[Dot[a]],
MomProdExplicit[a__]:>Dot[a]
}
]


(* ::Input::Initialization:: *)
ExtractEnergy1[amp_,pn_,En_,pm_,\[Theta]_,\[Phi]_]:=Module[{res=amp,pupper,plower,p4vupper,p4vlower},
plower={{En+pm Cos[\[Theta]],pm Sin[\[Theta]]Exp[-I \[Phi]]},{pm Sin[\[Theta]]Exp[I \[Phi]],En-pm Cos[\[Theta]]}};
pupper={{En-pm Cos[\[Theta]],-pm Sin[\[Theta]]Exp[-I \[Phi]]},{-pm Sin[\[Theta]]Exp[I \[Phi]],En+pm Cos[\[Theta]]}};
p4vupper={En,pm Sin[\[Theta]]Cos[\[Phi]],pm Sin[\[Theta]]Sin[\[Phi]],pm Cos[\[Theta]]};
p4vlower={En,-pm Sin[\[Theta]]Cos[\[Phi]],-pm Sin[\[Theta]]Sin[\[Phi]],-pm Cos[\[Theta]]};

res=res//.{
SpinorChain[Spinor["Spin",ul___,"Angle",a__],Mom[pn],b___]:>SpinorChain[Spinor["Spin",ul,"Angle",a],plower,b],
(*SpinorChain[Spinor["Spin","Lower","Angle",a__],Mom[pn],b___]\[RuleDelayed]SpinorChain[Spinor["Spin","Lower","Angle",a],plower,b],*)
SpinorChain[Spinor["Helicity","Angle",a_],Mom[pn],b___]:>SpinorChain[Spinor["Helicity","Angle",a],plower,b],

SpinorChain[Spinor["Spin",ul___,"Square",a__],Mom[pn],b___]:>SpinorChain[Spinor["Spin",ul,"Square",a],pupper,b],
(*SpinorChain[Spinor["Spin","Lower","Square",a__],Mom[pn],b___]\[RuleDelayed]SpinorChain[Spinor["Spin","Lower","Square",a],pupper,b],*)
SpinorChain[Spinor["Helicity","Square",a_],Mom[pn],b___]:>SpinorChain[Spinor["Helicity","Square",a],pupper,b],

SpinorChain[b___,Mom[pn],Spinor["Spin",ul___,"Angle",a__]]:>SpinorChain[b,pupper,Spinor["Spin",ul,"Angle",a]],
(*SpinorChain[b___,Mom[pn],Spinor["Spin","Lower","Angle",a__]]\[RuleDelayed]SpinorChain[b,pupper,Spinor["Spin","Lower","Angle",a]],*)
SpinorChain[b___,Mom[pn],Spinor["Helicity","Angle",a_]]:>SpinorChain[b,pupper,Spinor["Helicity","Angle",a]],

SpinorChain[b___,Mom[pn],Spinor["Spin",ul___,"Square",a__]]:>SpinorChain[b,plower,Spinor["Spin",ul,"Square",a]],
(*SpinorChain[b___,Mom[pn],Spinor["Spin","Lower","Square",a__]]:>SpinorChain[b,plower,Spinor["Spin","Lower","Square",a]],*)
SpinorChain[b___,Mom[pn],Spinor["Helicity","Square",a_]]:>SpinorChain[b,plower,Spinor["Helicity","Square",a]],

SpinorTrace[Mom[pn],a___]:>SpinorTrace[plower,a],
SpinorTrace[b_,Mom[pn],a___]:>SpinorTrace[b,pupper,a],
SpinorTrace[b_,c_,Mom[pn],a___]:>SpinorTrace[b,c,plower,a],
SpinorTrace[b_,c_,d_,Mom[pn],a___]:>SpinorTrace[b,c,d,pupper,a],

MomProd[pn,a_]:>MomProdExplicit[p4vupper,a],
MomProd[a_,pn]:>MomProdExplicit[a,p4vlower],
MomProdExplicit[pn,a_]:>MomProdExplicit[p4vupper,a],
MomProdExplicit[a_,pn]:>MomProdExplicit[a,p4vlower],

Mandelstahm[pn,a_]:>2MomProd[pn,a]+Mass[pn]^2+Mass[a]^2,
Mandelstahm[a_,pn]:>2MomProd[pn,a]+Mass[pn]^2+Mass[a]^2
};

res];


ExtractEnergy2[amp_,pn_,En_,pm_,\[Theta]_,\[Phi]_]:=Module[{res=amp,c=Cos[\[Theta]/2],s=Sin[\[Theta]/2]Exp[I \[Phi]],sc=Sin[\[Theta]/2]Exp[-I \[Phi]]},
res=res//.{
SpinorChain[Spinor["Helicity","Angle",pn],a___]:>SpinorChain[Sqrt[2En]{s,-c},a],
SpinorChain[Spinor["Spin","Upper","Angle",pn,-1/2],a___]:>SpinorChain[Sqrt[En+pm]{s,-c},a],
SpinorChain[a___,Spinor["Helicity","Angle",pn]]:>SpinorChain[a,Sqrt[2En]{c,s}],
SpinorChain[a___,Spinor["Spin","Upper","Angle",pn,-1/2]]:>SpinorChain[a,Sqrt[En+pm]{c,s}],
SpinorChain[Spinor["Spin","Upper","Angle",pn,1/2],a___]:>SpinorChain[Sqrt[En-pm]{c,sc},a],
SpinorChain[a___,Spinor["Spin","Upper","Angle",pn,1/2]]:>SpinorChain[a,Sqrt[En-pm]{-sc,c}],

SpinorChain[Spinor["Spin","Lower","Angle",pn,-1/2],a___]:>SpinorChain[Sqrt[En-pm]{c,sc},a],
SpinorChain[a___,Spinor["Spin","Lower","Angle",pn,-1/2]]:>SpinorChain[a,Sqrt[En-pm]{-sc,c}],
SpinorChain[Spinor["Spin","Lower","Angle",pn,1/2],a___]:>SpinorChain[-Sqrt[En+pm]{s,-c},a],
SpinorChain[a___,Spinor["Spin","Lower","Angle",pn,1/2]]:>SpinorChain[a,-Sqrt[En+pm]{c,s}],

SpinorChain[Spinor["Spin","Upper","Square",pn,-1/2],a___]:>SpinorChain[-Sqrt[En-pm]{-s,c},a],
SpinorChain[a___,Spinor["Spin","Upper","Square",pn,-1/2]]:>SpinorChain[a,-Sqrt[En-pm]{c,s}],
SpinorChain[Spinor["Helicity","Square",pn],a___]:>SpinorChain[Sqrt[2En]{c,sc},a],
SpinorChain[Spinor["Spin","Upper","Square",pn,1/2],a___]:>SpinorChain[Sqrt[En+pm]{c,sc},a],
SpinorChain[a___,Spinor["Helicity","Square",pn]]:>SpinorChain[a,Sqrt[2En]{sc,-c}],
SpinorChain[a___,Spinor["Spin","Upper","Square",pn,1/2]]:>SpinorChain[a,Sqrt[En+pm]{sc,-c}],

SpinorChain[Spinor["Spin","Lower","Square",pn,-1/2],a___]:>SpinorChain[Sqrt[En+pm]{c,sc},a],
SpinorChain[a___,Spinor["Spin","Lower","Square",pn,-1/2]]:>SpinorChain[a,Sqrt[En+pm]{sc,-c}],
SpinorChain[Spinor["Spin","Lower","Square",pn,1/2],a___]:>SpinorChain[Sqrt[En-pm]{-s,c},a],
SpinorChain[a___,Spinor["Spin","Lower","Square",pn,1/2]]:>SpinorChain[a,Sqrt[En-pm]{c,s}]

};

res];


(* ::Input:: *)
(*(*Tests: All should give 0*)*)
(*N[ExtractEnergy[SpinorChain[Spinor["Spin","Upper","Angle",3,1/2],Mom[3]]-MW SpinorChain[Spinor["Spin","Upper","Square",3,1/2]],{En,Sqrt[En^2-Mt^2],0,0},{En,Sqrt[En^2-Mt^2],\[Pi],\[Pi]},{En,Sqrt[En^2-MW^2],\[Theta],0},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]},1,3]/.{En->500,MW->80,\[Theta]->\[Pi]/7}]*)
(*N[ExtractEnergy[SpinorChain[Spinor["Spin","Upper","Angle",3,-1/2],Mom[3]]-MW SpinorChain[Spinor["Spin","Upper","Square",3,-1/2]],{En,Sqrt[En^2-Mt^2],0,0},{En,Sqrt[En^2-Mt^2],\[Pi],\[Pi]},{En,Sqrt[En^2-MW^2],\[Theta],0},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]},1,3]/.{En->500,MW->80,\[Theta]->\[Pi]/7}]*)
(*N[ExtractEnergy[SpinorChain[Spinor["Spin","Lower","Angle",3,-1/2],Mom[3]]-MW SpinorChain[Spinor["Spin","Lower","Square",3,-1/2]],{En,Sqrt[En^2-Mt^2],0,0},{En,Sqrt[En^2-Mt^2],\[Pi],\[Pi]},{En,Sqrt[En^2-MW^2],\[Theta],0},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]},1,3]/.{En->500,MW->80,\[Theta]->\[Pi]/7}]*)
(*N[ExtractEnergy[SpinorChain[Spinor["Spin","Lower","Angle",3,1/2],Mom[3]]-MW SpinorChain[Spinor["Spin","Lower","Square",3,1/2]],{En,Sqrt[En^2-Mt^2],0,0},{En,Sqrt[En^2-Mt^2],\[Pi],\[Pi]},{En,Sqrt[En^2-MW^2],\[Theta],0},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]},1,3]/.{En->500,MW->80,\[Theta]->\[Pi]/7}]*)
(*N[ExtractEnergy[SpinorChain[Spinor["Spin","Upper","Square",3,1/2],Mom[3]]-MW SpinorChain[Spinor["Spin","Upper","Angle",3,1/2]],{En,Sqrt[En^2-Mt^2],0,0},{En,Sqrt[En^2-Mt^2],\[Pi],\[Pi]},{En,Sqrt[En^2-MW^2],\[Theta],0},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]},1,3]/.{En->500,MW->80,\[Theta]->\[Pi]/7}]*)
(*N[ExtractEnergy[SpinorChain[Spinor["Spin","Upper","Square",3,-1/2],Mom[3]]-MW SpinorChain[Spinor["Spin","Upper","Angle",3,-1/2]],{En,Sqrt[En^2-Mt^2],0,0},{En,Sqrt[En^2-Mt^2],\[Pi],\[Pi]},{En,Sqrt[En^2-MW^2],\[Theta],0},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]},1,3]/.{En->500,MW->80,\[Theta]->\[Pi]/7}]*)
(*N[ExtractEnergy[SpinorChain[Spinor["Spin","Lower","Square",3,-1/2],Mom[3]]-MW SpinorChain[Spinor["Spin","Lower","Angle",3,-1/2]],{En,Sqrt[En^2-Mt^2],0,0},{En,Sqrt[En^2-Mt^2],\[Pi],\[Pi]},{En,Sqrt[En^2-MW^2],\[Theta],0},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]},1,3]/.{En->500,MW->80,\[Theta]->\[Pi]/7}]*)
(*N[ExtractEnergy[SpinorChain[Spinor["Spin","Lower","Square",3,1/2],Mom[3]]-MW SpinorChain[Spinor["Spin","Lower","Angle",3,1/2]],{En,Sqrt[En^2-Mt^2],0,0},{En,Sqrt[En^2-Mt^2],\[Pi],\[Pi]},{En,Sqrt[En^2-MW^2],\[Theta],0},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]},1,3]/.{En->500,MW->80,\[Theta]->\[Pi]/7}]*)


(* ::Input:: *)
(*(*Tests:  All should give 2MW*)*)
(*AFSimplify[ExtractEnergy[ExpandSummation[-SpinorChain[Spinor["Spin","Upper","Angle",3,J],Spinor["Spin","Lower","Angle",3,J]]],{En,Sqrt[En^2-Mt^2],0,0},{En,Sqrt[En^2-Mt^2],\[Pi],\[Pi]},{En,Sqrt[En^2-MW^2],\[Theta],0},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]},1,3]]*)
(*AFSimplify[ExtractEnergy[ExpandSummation[SpinorChain[Spinor["Spin","Lower","Angle",3,J],Spinor["Spin","Upper","Angle",3,J]]],{En,Sqrt[En^2-Mt^2],0,0},{En,Sqrt[En^2-Mt^2],\[Pi],\[Pi]},{En,Sqrt[En^2-MW^2],\[Theta],0},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]},1,3]]*)
(*AFSimplify[ExtractEnergy[ExpandSummation[SpinorChain[Spinor["Spin","Upper","Square",3,J],Spinor["Spin","Lower","Square",3,J]]],{En,Sqrt[En^2-Mt^2],0,0},{En,Sqrt[En^2-Mt^2],\[Pi],\[Pi]},{En,Sqrt[En^2-MW^2],\[Theta],0},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]},1,3]]*)
(*AFSimplify[ExtractEnergy[ExpandSummation[-SpinorChain[Spinor["Spin","Lower","Square",3,J],Spinor["Spin","Upper","Square",3,J]]],{En,Sqrt[En^2-Mt^2],0,0},{En,Sqrt[En^2-Mt^2],\[Pi],\[Pi]},{En,Sqrt[En^2-MW^2],\[Theta],0},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]},1,3]]*)


(* ::Input:: *)
(*(*Tests: All should give 0*)*)
(*N[ExtractEnergy[ExpandSummation[SpinorChain[Spinor["Spin","Upper","Angle",rr[4,1],J],Spinor["Spin","Lower","Angle",rr[4,1],J]]],{En,Sqrt[En^2-Mt^2],0.3,0.7},{En,Sqrt[En^2-Mt^2],\[Pi]-0.3,\[Pi]+0.7},{En,Sqrt[En^2-MW^2],\[Theta],0.1},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]+0.1},1,4]/.{En->500,MW->80,Mt->175,\[Theta]->\[Pi]/7}]*)
(*N[ExtractEnergy[ExpandSummation[SpinorChain[Spinor["Spin","Lower","Angle",rr[4,1],J],Spinor["Spin","Upper","Angle",rr[4,1],J]]],{En,Sqrt[En^2-Mt^2],0.3,0.7},{En,Sqrt[En^2-Mt^2],\[Pi]-0.3,\[Pi]+0.7},{En,Sqrt[En^2-MW^2],\[Theta],0.1},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]+0.1},1,4]/.{En->500,MW->80,Mt->175,\[Theta]->\[Pi]/7}]*)
(*N[ExtractEnergy[ExpandSummation[SpinorChain[Spinor["Spin","Upper","Square",rr[1,4],J],Spinor["Spin","Lower","Square",rr[1,4],J]]],{En,Sqrt[En^2-Mt^2],0.3,0.7},{En,Sqrt[En^2-Mt^2],\[Pi]-0.3,\[Pi]+0.7},{En,Sqrt[En^2-MW^2],\[Theta],0.1},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]+0.1},1,4]/.{En->500,MW->80,Mt->175,\[Theta]->\[Pi]/7}]*)
(*N[ExtractEnergy[ExpandSummation[SpinorChain[Spinor["Spin","Lower","Square",rr[1,4],J],Spinor["Spin","Upper","Square",rr[1,4],J]]],{En,Sqrt[En^2-Mt^2],0.3,0.7},{En,Sqrt[En^2-Mt^2],\[Pi]-0.3,\[Pi]+0.7},{En,Sqrt[En^2-MW^2],\[Theta],0.1},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]+0.1},1,4]/.{En->500,MW->80,Mt->175,\[Theta]->\[Pi]/7}]*)


(* ::Input:: *)
(*(*Tests: All should give 0*)*)
(*ASimplify[ExtractEnergy[ExpandSummation[SpinorChain[Spinor["Spin","Upper","Angle",rr[4,3],J],Spinor["Spin","Lower","Angle",rr[4,3],J]]],{En,Sqrt[En^2-Mt^2],0.3,0.7},{En,Sqrt[En^2-Mt^2],\[Pi]-0.3,\[Pi]+0.7},{En,Sqrt[En^2-MW^2],\[Theta],\[Phi]},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]+\[Phi]},3,4]]*)
(*ASimplify[ExtractEnergy[ExpandSummation[SpinorChain[Spinor["Spin","Lower","Angle",rr[4,3],J],Spinor["Spin","Upper","Angle",rr[4,3],J]]],{En,Sqrt[En^2-Mt^2],0.3,0.7},{En,Sqrt[En^2-Mt^2],\[Pi]-0.3,\[Pi]+0.7},{En,Sqrt[En^2-MW^2],\[Theta],\[Phi]},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]+\[Phi]},3,4]]*)
(*ASimplify[ExtractEnergy[ExpandSummation[SpinorChain[Spinor["Spin","Upper","Square",rr[3,4],J],Spinor["Spin","Lower","Square",rr[3,4],J]]],{En,Sqrt[En^2-Mt^2],0.3,0.7},{En,Sqrt[En^2-Mt^2],\[Pi]-0.3,\[Pi]+0.7},{En,Sqrt[En^2-MW^2],\[Theta],\[Phi]},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]+\[Phi]},3,4]]*)
(*ASimplify[ExtractEnergy[ExpandSummation[SpinorChain[Spinor["Spin","Lower","Square",rr[3,4],J],Spinor["Spin","Upper","Square",rr[3,4],J]]],{En,Sqrt[En^2-Mt^2],0.3,0.7},{En,Sqrt[En^2-Mt^2],\[Pi]-0.3,\[Pi]+0.7},{En,Sqrt[En^2-MW^2],\[Theta],\[Phi]},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]+\[Phi]},3,4]]*)


(* ::Input:: *)
(*N[ExtractEnergy[ExpandSummation[SpinorChain[Spinor["Spin","Upper","Angle",4,J],Spinor["Spin","Lower","Angle",rr[4,1],J]]],{En,Sqrt[En^2-Mt^2],0.7,0.1},{En,Sqrt[En^2-Mt^2],\[Pi]-0.7,\[Pi]+0.1},{En,Sqrt[En^2-MW^2],\[Theta],0.3},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]+0.3},1,4]/.{En->500,MW->80,Mt->175,\[Theta]->\[Pi]/7}]*)
(*N[ExtractEnergy[ExpandSummation[SpinorChain[Spinor["Spin","Lower","Angle",4,J],Spinor["Spin","Upper","Angle",rr[4,1],J]]],{En,Sqrt[En^2-Mt^2],0.7,0.1},{En,Sqrt[En^2-Mt^2],\[Pi]-0.7,\[Pi]+0.1},{En,Sqrt[En^2-MW^2],\[Theta],0.3},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]+0.3},1,4]/.{En->500,MW->80,Mt->175,\[Theta]->\[Pi]/7}]*)
(*N[ExtractEnergy[ExpandSummation[SpinorChain[Spinor["Spin","Upper","Angle",rr[4,1],J],Spinor["Spin","Lower","Angle",4,J]]],{En,Sqrt[En^2-Mt^2],0.7,0.1},{En,Sqrt[En^2-Mt^2],\[Pi]-0.7,\[Pi]+0.1},{En,Sqrt[En^2-MW^2],\[Theta],0.3},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]+0.3},1,4]/.{En->500,MW->80,Mt->175,\[Theta]->\[Pi]/7}]*)
(*N[ExtractEnergy[ExpandSummation[SpinorChain[Spinor["Spin","Lower","Angle",rr[4,1],J],Spinor["Spin","Upper","Angle",4,J]]],{En,Sqrt[En^2-Mt^2],0.7,0.1},{En,Sqrt[En^2-Mt^2],\[Pi]-0.7,\[Pi]+0.1},{En,Sqrt[En^2-MW^2],\[Theta],0.3},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]+0.3},1,4]/.{En->500,MW->80,Mt->175,\[Theta]->\[Pi]/7}]*)


(* ::Input:: *)
(*N[ExtractEnergy[ExpandSummation[SpinorChain[Spinor["Spin","Upper","Square",1,J],Spinor["Spin","Lower","Square",rr[1,4],J]]],{En,Sqrt[En^2-Mt^2],0.7,0.1},{En,Sqrt[En^2-Mt^2],\[Pi]-0.7,\[Pi]+0.1},{En,Sqrt[En^2-MW^2],\[Theta],0.3},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]+0.3},1,4]/.{En->500,MW->80,Mt->175,\[Theta]->\[Pi]/7}]*)
(*N[ExtractEnergy[ExpandSummation[SpinorChain[Spinor["Spin","Lower","Square",1,J],Spinor["Spin","Upper","Square",rr[1,4],J]]],{En,Sqrt[En^2-Mt^2],0.7,0.1},{En,Sqrt[En^2-Mt^2],\[Pi]-0.7,\[Pi]+0.1},{En,Sqrt[En^2-MW^2],\[Theta],0.3},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]+0.3},1,4]/.{En->500,MW->80,Mt->175,\[Theta]->\[Pi]/7}]*)
(*N[ExtractEnergy[ExpandSummation[SpinorChain[Spinor["Spin","Upper","Square",rr[1,4],J],Spinor["Spin","Lower","Square",1,J]]],{En,Sqrt[En^2-Mt^2],0.7,0.1},{En,Sqrt[En^2-Mt^2],\[Pi]-0.7,\[Pi]+0.1},{En,Sqrt[En^2-MW^2],\[Theta],0.3},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]+0.3},1,4]/.{En->500,MW->80,Mt->175,\[Theta]->\[Pi]/7}]*)
(*N[ExtractEnergy[ExpandSummation[SpinorChain[Spinor["Spin","Lower","Square",rr[1,4],J],Spinor["Spin","Upper","Square",1,J]]],{En,Sqrt[En^2-Mt^2],0.7,0.1},{En,Sqrt[En^2-Mt^2],\[Pi]-0.7,\[Pi]+0.1},{En,Sqrt[En^2-MW^2],\[Theta],0.3},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]+0.3},1,4]/.{En->500,MW->80,Mt->175,\[Theta]->\[Pi]/7}]*)


(* ::Input:: *)
(*ExtractEnergy[ExpandSummation[SpinorChain[Spinor["Spin","Upper","Angle",rr[2,1],J],Spinor["Spin","Lower","Angle",rr[2,1],J]]],{En,Sqrt[En^2-Mt^2],0,0.7},{En,Sqrt[En^2-Mt^2],\[Pi],\[Pi]+0.7},{En,Sqrt[En^2-MW^2],\[Theta],0.1},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]+0.1},1,2]*)
(*ExtractEnergy[ExpandSummation[SpinorChain[Spinor["Spin","Lower","Angle",rr[2,1],J],Spinor["Spin","Upper","Angle",rr[2,1],J]]],{En,Sqrt[En^2-Mt^2],0,0.7},{En,Sqrt[En^2-Mt^2],\[Pi],\[Pi]+0.7},{En,Sqrt[En^2-MW^2],\[Theta],0.1},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]+0.1},1,2]*)
(*ExtractEnergy[ExpandSummation[SpinorChain[Spinor["Spin","Upper","Square",rr[1,2],J],Spinor["Spin","Lower","Square",rr[1,2],J]]],{En,Sqrt[En^2-Mt^2],0,0.7},{En,Sqrt[En^2-Mt^2],\[Pi],\[Pi]+0.7},{En,Sqrt[En^2-MW^2],\[Theta],0.1},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]+0.1},1,2]*)
(*ExtractEnergy[ExpandSummation[SpinorChain[Spinor["Spin","Lower","Square",rr[1,2],J],Spinor["Spin","Upper","Square",rr[1,2],J]]],{En,Sqrt[En^2-Mt^2],0,0.7},{En,Sqrt[En^2-Mt^2],\[Pi],\[Pi]+0.7},{En,Sqrt[En^2-MW^2],\[Theta],0.1},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]+0.1},1,2]*)


(* ::Input:: *)
(*ExtractEnergy[ExpandSummation[SpinorChain[Spinor["Spin","Upper","Angle",2,J],Spinor["Spin","Lower","Angle",rr[2,1],J]]],{En,Sqrt[En^2-Mt^2],0,0.7},{En,Sqrt[En^2-Mt^2],\[Pi],\[Pi]+0.7},{En,Sqrt[En^2-MW^2],\[Theta],0.1},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]+0.1},1,2]*)
(*ExtractEnergy[ExpandSummation[SpinorChain[Spinor["Spin","Lower","Angle",2,J],Spinor["Spin","Upper","Angle",rr[2,1],J]]],{En,Sqrt[En^2-Mt^2],0,0.7},{En,Sqrt[En^2-Mt^2],\[Pi],\[Pi]+0.7},{En,Sqrt[En^2-MW^2],\[Theta],0.1},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]+0.1},1,2]*)
(*ExtractEnergy[ExpandSummation[SpinorChain[Spinor["Spin","Upper","Square",1,J],Spinor["Spin","Lower","Square",rr[1,2],J]]],{En,Sqrt[En^2-Mt^2],0,0.7},{En,Sqrt[En^2-Mt^2],\[Pi],\[Pi]+0.7},{En,Sqrt[En^2-MW^2],\[Theta],0.1},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]+0.1},1,2]*)
(*ExtractEnergy[ExpandSummation[SpinorChain[Spinor["Spin","Lower","Square",1,J],Spinor["Spin","Upper","Square",rr[1,2],J]]],{En,Sqrt[En^2-Mt^2],0,0.7},{En,Sqrt[En^2-Mt^2],\[Pi],\[Pi]+0.7},{En,Sqrt[En^2-MW^2],\[Theta],0.1},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]+0.1},1,2]*)


(* ::Input:: *)
(*ExtractEnergy[ExpandSummation[SpinorChain[Spinor["Spin","Upper","Angle",rr[2,1],J],Spinor["Spin","Lower","Angle",2,J]]],{En,Sqrt[En^2-Mt^2],0,0.7},{En,Sqrt[En^2-Mt^2],\[Pi],\[Pi]+0.7},{En,Sqrt[En^2-MW^2],\[Theta],0.1},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]+0.1},1,2]*)
(*ExtractEnergy[ExpandSummation[SpinorChain[Spinor["Spin","Lower","Angle",rr[2,1],J],Spinor["Spin","Upper","Angle",2,J]]],{En,Sqrt[En^2-Mt^2],0,0.7},{En,Sqrt[En^2-Mt^2],\[Pi],\[Pi]+0.7},{En,Sqrt[En^2-MW^2],\[Theta],0.1},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]+0.1},1,2]*)
(*ExtractEnergy[ExpandSummation[SpinorChain[Spinor["Spin","Upper","Square",rr[1,2],J],Spinor["Spin","Lower","Square",1,J]]],{En,Sqrt[En^2-Mt^2],0,0.7},{En,Sqrt[En^2-Mt^2],\[Pi],\[Pi]+0.7},{En,Sqrt[En^2-MW^2],\[Theta],0.1},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]+0.1},1,2]*)
(*ExtractEnergy[ExpandSummation[SpinorChain[Spinor["Spin","Lower","Square",rr[1,2],J],Spinor["Spin","Upper","Square",1,J]]],{En,Sqrt[En^2-Mt^2],0,0.7},{En,Sqrt[En^2-Mt^2],\[Pi],\[Pi]+0.7},{En,Sqrt[En^2-MW^2],\[Theta],0.1},{En,Sqrt[En^2-MW^2],\[Pi]-\[Theta],\[Pi]+0.1},1,2]*)


(* ::Input:: *)
(*(*Trace should be cyclically symmetric.  We should get 0 for this test.*)*)
(*ExtractEnergy[SpinorTrace[Mom[1],Mom[2]]-SpinorTrace[Mom[2],Mom[1]],{En1,p1,\[Theta]1,\[Phi]1},{En3,p3,\[Theta]3,\[Phi]3},{En4,p4,\[Theta]4,\[Phi]4}]*)
(*FullSimplify[ExtractEnergy[SpinorTrace[Mom[1],Mom[2],Mom[3]]-SpinorTrace[Mom[2],Mom[3],Mom[1]],{En1,p1,\[Theta]1,\[Phi]1},{En3,p3,\[Theta]3,\[Phi]3},{En4,p4,\[Theta]4,\[Phi]4}]]*)


(* ::Input:: *)
(*Simplify[ExtractEnergy[SpinorTrace[Mom[1],Mom[2],Mom[3],Mom[4]]-SpinorTrace[Mom[3],Mom[4],Mom[1],Mom[2]],{En1,p1,\[Theta]1,\[Phi]1},{En2,p2,\[Theta]2,\[Phi]2},{En3,p3,\[Theta]3,\[Phi]3},{En4,p4,\[Theta]4,\[Phi]4},1,2]]*)


(* ::Subsubsection::Closed:: *)
(*ConvertStringAmplitude*)


(* ::Input::Initialization:: *)
Options[ConvertStringHattedAmplitude]={InternalMasses->{}};
ConvertStringHattedAmplitude[amp_,masses_List,opts:OptionsPattern[]]:=ConvertStringAmplitude[amp,masses,opts]/.{Spinor[a__]:>SpinorHat[a],Mom[a_]:>MomHat[a]}


(* ::Input::Initialization:: *)
Options[ConvertStringAmplitude]={InternalMasses->{}};
ConvertStringAmplitude[amp_,masses_List,OptionsPattern[]]:=Module[{res,MassP,MultiPart,AngleSquare,SpinHelicity,UpperLower,CharListToStringList,SplitMomenta,CreateSpinorChain},
res=StringReplace[amp,{" "->""}];
MassP[str_]:=Mass[(ToExpression[#]&/@StringSplit[str,""]/.List[a__]:>a)]/. OptionValue[InternalMasses];
MultiPart[str_]:=ToString[Multiparticle[(ToExpression[#]&/@StringSplit[str,""]/.List[a__]:>a)]];
AngleSquare[st_]:=If[st==="<"||st===">","Angle","Square"];
SpinHelicity[mass_]:=If[mass===0,"Helicity","Spin"];
UpperLower[st_]:=Which[st==="^","Upper",st==="_","Lower",1==1,"Remove"];
CharListToStringList[chars_List]:=StringDrop[StringJoin[#<>","&/@chars],-1];
SplitMomenta[mom_]:=CharListToStringList[If[ToExpression[#]<10,"Mom["<>#<>"]","Mom[Multiparticle["<>CharListToStringList[Characters[#]]<>"]]"]&/@If[StringContainsQ[mom,{"p","P"}],StringSplit[mom,RegularExpression["[pP]"]],Characters[mom]]];(*StringSplit[mom,RegularExpression["[pP]"]]];*)
CreateSpinorChain[as1_,MP1_,pn1_,ul1_,in1_,vb1_,mom_,vb2_,MP2_,pn2_,ul2_,in2_,as2_]:=StringReplace["SpinorChain[Spinor[\""<>
Which[MP1==="P",SpinHelicity[MassP[pn1]],MP1==="z","Zeta",1==1,SpinHelicity[masses[[ToExpression[pn1]]]]]<>"\",\""<>
UpperLower[ul1]<>"\",\""<>
AngleSquare[as1]<>"\","<>
If[MP1==="P",MultiPart[pn1],pn1]<>","<>
in1<>
"],"<>
If[mom=!=0,SplitMomenta[mom]<>",",""]
<>"Spinor[\""<>
Which[MP2==="P",SpinHelicity[MassP[pn2]],MP2==="z","Zeta",1==1,SpinHelicity[masses[[ToExpression[pn2]]]]]<>"\",\""<>
UpperLower[ul2]<>"\",\""<>
AngleSquare[as2]<>"\","<>
If[MP2==="P",MultiPart[pn2],pn2]<>","<>
in2<>
"]]",
{RegularExpression["\"Remove\","]:>"",RegularExpression[",\\]"]:>"]"}];
res=StringReplace[res,{
RegularExpression["p(\\d).p(\\d)"]:>"MomProd[$1,$2]",
RegularExpression["s(\\d)(\\d)"]:>"Mandelstahm[$1,$2]",
RegularExpression["x(\\d)(\\d)"]:>"xFactor[$1,$2]",
(*With Momenta*)
RegularExpression["([<\\[])(\\d)(\\d+)(\\d)([>\\]])"]:>CreateSpinorChain["$1","","$2","","",0,"$3",0,"","$4","","","$5"],
(*No Momentum*)
RegularExpression["([<\\[])([Pz]*)(\\d+)([\\_\\^]*)([a-yA-OQ-Z0-9]*)([Pz]*)(\\d+)([\\_\\^]*)([a-yA-OQ-Z0-9]*)([>\\]])"]:>CreateSpinorChain["$1","$2","$3","$4","$5",0,0,0,"$6","$7","$8","$9","$10"],
(*With Momenta*)
RegularExpression["([<\\[])([Pz]*)(\\d+)([\\_\\^]*)([a-yA-OQ-Z0-9]*)\\|([^|]*)\\|([Pz]*)(\\d+)([\\_\\^]*)([a-yA-OQ-Z0-9]*)([>\\]])"]:>CreateSpinorChain["$1","$2","$3","$4","$5",0,"$6",0,"$7","$8","$9","$10","$11"]
}];(*Print[res];*)
res=ToExpression[res];
(*Print[InputForm[res]];*)
res
];


(* ::Subsubsection::Closed:: *)
(*ReverseMomentum*)


(* ::Input::Initialization:: *)
ReverseMomentum[amp_,plist_List]:=Module[{res=amp},
Do[res=ReverseMomentum[res,plist[[ii]]],{ii,1,Length[plist]}];
res
];


(* ::Input::Initialization:: *)
Begin["ConstructiveDiagrams`Private`"];

crossingMomentum[arg_, leg_] := arg /. Mom[label_] :> If[Head[label] === Multiparticle &&
    MemberQ[rspMomentumLabels[label], leg],
  Total[If[# === leg, -Mom[#], Mom[#]] & /@ rspMomentumLabels[label]],
  If[label === leg, -Mom[label], Mom[label]]];
crossingMomentumChoices[Mom[label_], leg_] := If[Head[label] === Multiparticle &&
    MemberQ[rspMomentumLabels[label], leg],
  {If[# === leg, -1, 1], Mom[#]} & /@ rspMomentumLabels[label],
  {{If[label === leg, -1, 1], Mom[label]}}];
crossingMomentumChoices[arg_, leg_] := {{1, crossingMomentum[arg, leg]}};
crossingChain[chain_SpinorChain, leg_] := Module[{parts = List @@ chain, choices},
  choices = MapIndexed[Function[{part, position},
    If[MemberQ[{1, Length[parts]}, First[position]] && MatchQ[part, _Spinor] &&
        MemberQ[{"Spin", "Helicity"}, part[[1]]] &&
        rspSpinorType[part] === "Angle" && rspSpinorLeg[part] === leg,
      {{-1, part}}, crossingMomentumChoices[part, leg]]], parts];
  Total[(Times @@ (First /@ #)) (SpinorChain @@ (Last /@ #)) & /@ Tuples[choices]]
];
crossingTrace[trace_SpinorTrace, leg_] := Total[
  (Times @@ (First /@ #)) (SpinorTrace @@ (Last /@ #)) & /@
    Tuples[crossingMomentumChoices[#, leg] & /@ List @@ trace]];

ReverseMomentum[amp_, leg_] := amp /. {
  chain_SpinorChain :> crossingChain[chain, leg],
  trace_SpinorTrace :> crossingTrace[trace, leg],
  PropDen[momentum_, rest___] :> PropDen[crossingMomentum[momentum, leg], rest],
  MomProd[a_, b_] :> If[a === leg, -1, 1] If[b === leg, -1, 1] MomProd[a, b]
};

End[];


(* ::Subsubsection::Closed:: *)
(*PermuteParticles*)


(* ::Input::Initialization:: *)
Begin["ConstructiveDiagrams`Private`"];
ConstructiveDiagrams`PermuteParticles[amp_, particles_List] := Module[{labels, index},
  If[Length[particles] < 2, Return[amp]];
  labels = AssociationThread[particles, RotateLeft[particles]];
  index[Multiparticle[parts___]] := Multiparticle @@ (index /@ {parts});
  index[label_] := Lookup[labels, label, label];
  amp /. {
    Spinor[kind:("Spin"|"Helicity"|"Zeta"), type:("Angle"|"Square"), label_, rest___] :>
      Spinor[kind, type, index[label], rest],
    Spinor[kind:("Spin"|"Helicity"|"Zeta"), variance_, type:("Angle"|"Square"), label_, rest___] :>
      Spinor[kind, variance, type, index[label], rest],
    Mom[label_] :> Mom[index[label]], Mass[label_] :> Mass[index[label]],
    MomProd[a_, b_] :> MomProd[index[a], index[b]],
    Mandelstahm[parts___] :> Mandelstahm @@ (index /@ {parts}),
    xFactor[parts___] :> xFactor @@ (index /@ {parts}),
    xTildeFactor[parts___] :> xTildeFactor @@ (index /@ {parts})
  }
];
End[];


(* ::Subsubsection::Closed:: *)
(*ChooseSpinIndices*)


(* ::Input::Initialization:: *)
ExpandSummation[amp_]:=Expand[amp]//.{
SpinorChain[a___,Spinor["Spin",b1_,b2_,b3_,J_],c___]SpinorChain[d___,Spinor["Spin",e1_,e2_,e3_,J_],f___]/;!NumberQ[J]:>SpinorChain[a,Spinor["Spin",b1,b2,b3,-1/2],c]SpinorChain[d,Spinor["Spin",e1,e2,e3,-1/2],f]+SpinorChain[a,Spinor["Spin",b1,b2,b3,1/2],c]SpinorChain[d,Spinor["Spin",e1,e2,e3,1/2],f],


SpinorChain[Spinor["Spin",a1_,a2_,a3_,J_],b___,Spinor["Spin",c1_,c2_,c3_,J_]]/;!NumberQ[J]:>SpinorChain[Spinor["Spin",a1,a2,a3,-1/2],b,Spinor["Spin",c1,c2,c3,-1/2]]+SpinorChain[Spinor["Spin",a1,a2,a3,1/2],b,Spinor["Spin",c1,c2,c3,1/2]]
}


(* ::Input::Initialization:: *)
ChooseSpinIndices[amp_,i__]:=Module[{res=amp},
Do[res=ChooseSpinIndex[res,pn,List[i][[pn]]],{pn,1,Length[List[i]]}];
res
];


(* ::Input::Initialization:: *)
ChooseSpinIndex[amp_,pn_,i_]:=Module[{res=Expand[amp]},
(*Spin 1/2*)
Which[i==-1/2||i==1/2,
res=res//.{
Spinor["Spin",tp_,pn]:>Spinor["Spin","Upper",tp,pn,i]
};
];

(*Spin 1*)
Which[i==-1||i==1,
res=res//.{
SpinorChain[Spinor["Spin",tp1_,pn],a__]SpinorChain[Spinor["Spin",tp2_,pn],b__]:>SpinorChain[Spinor["Spin","Upper",tp1,pn,i/2],a]SpinorChain[Spinor["Spin","Upper",tp2,pn,i/2],b],
SpinorChain[Spinor["Spin",tp_,pn],a__]^2:>SpinorChain[Spinor["Spin","Upper",tp,pn,i/2],a]^2,
SpinorChain[Spinor["Spin",tp1_,pn],a__]SpinorChain[b__,Spinor["Spin",tp2_,pn]]:>SpinorChain[Spinor["Spin","Upper",tp1,pn,i/2],a]SpinorChain[b,Spinor["Spin","Upper",tp2,pn,i/2]],
SpinorChain[a__,Spinor["Spin",tp_,pn]]^2:>SpinorChain[a,Spinor["Spin","Upper",tp,pn,i/2]]^2,
SpinorChain[a__,Spinor["Spin",tp1_,pn]]SpinorChain[b__,Spinor["Spin",tp2_,pn]]:>SpinorChain[a,Spinor["Spin","Upper",tp1,pn,i/2]]SpinorChain[b,Spinor["Spin","Upper",tp2,pn,i/2]]
};,
i==0,
res=res//.{
SpinorChain[Spinor["Spin",tp1_,pn],a__]SpinorChain[Spinor["Spin",tp2_,pn],b__]:>1 /Sqrt[2] (SpinorChain[Spinor["Spin","Upper",tp1,pn,-1/2],a]SpinorChain[Spinor["Spin","Upper",tp2,pn,1/2],b]+SpinorChain[Spinor["Spin","Upper",tp1,pn,1/2],a]SpinorChain[Spinor["Spin","Upper",tp2,pn,-1/2],b]),

SpinorChain[Spinor["Spin",tp_,pn],a__]^2:> Sqrt[2]SpinorChain[Spinor["Spin","Upper",tp,pn,-1/2],a]SpinorChain[Spinor["Spin","Upper",tp,pn,1/2],a],

SpinorChain[Spinor["Spin",tp1_,pn],a__]SpinorChain[b__,Spinor["Spin",tp2_,pn]]:>1/Sqrt[2] (SpinorChain[Spinor["Spin","Upper",tp1,pn,-1/2],a]SpinorChain[b,Spinor["Spin","Upper",tp2,pn,1/2]]+SpinorChain[Spinor["Spin","Upper",tp1,pn,1/2],a]SpinorChain[b,Spinor["Spin","Upper",tp2,pn,-1/2]]),

SpinorChain[a__,Spinor["Spin",tp_,pn]]^2:> Sqrt[2]SpinorChain[a,Spinor["Spin","Upper",tp,pn,-1/2]]SpinorChain[a,Spinor["Spin","Upper",tp,pn,1/2]],

SpinorChain[a__,Spinor["Spin",tp1_,pn]]SpinorChain[b__,Spinor["Spin",tp2_,pn]]:>1/Sqrt[2] (SpinorChain[a,Spinor["Spin","Upper",tp1,pn,-1/2]]SpinorChain[b,Spinor["Spin","Upper",tp2,pn,1/2]]+SpinorChain[a,Spinor["Spin","Upper",tp1,pn,1/2]]SpinorChain[b,Spinor["Spin","Upper",tp2,pn,-1/2]])
};
];

res
];


(* ::Subsubsection::Closed:: *)
(*MakeIndicesExplicit*)


(* ::Input::Initialization:: *)
MakeIndicesExplicit[amp_,indices_]:=Module[{res=amp},
If[Head[amp]==ConstructiveAmplitude,res=amp[[2]]];
Do[
res=MakeIndicesExplicit[res,partN,indices[[partN]]];
,{partN,1,Length[indices]}];
If[Head[amp]===ConstructiveAmplitude,res=ConstructiveAmplitude[amp[[1]],res,amp[[3]]]];
res
]


(* ::Input::Initialization:: *)
MakeIndicesExplicit[amp_,part_,indices_]:=Module[{tmp,replacementRules={}},
Which[
(*Spin 2*)
Length[indices]==4,
replacementRules={
SpinorChain[Spinor["Spin",type1_,part],b1___,Spinor["Spin",type2_,part]]SpinorChain[Spinor["Spin",type3_,part],b2___,Spinor["Spin",type4_,part]]:>SpinorChain[Spinor["Spin","Upper",type1,part,indices[[1]]],b1,Spinor["Spin","Upper",type2,part,indices[[2]]]]SpinorChain[Spinor["Spin","Upper",type3,part,indices[[3]]],b2,Spinor["Spin","Upper",type4,part,indices[[4]]]],
SpinorChain[Spinor["Spin",type1_,part],b___,Spinor["Spin",type2_,part]]SpinorChain[a3___,Spinor["Spin",type3_,part],b3___]SpinorChain[a4___,Spinor["Spin",type4_,part],b4___]:>SpinorChain[Spinor["Spin","Upper",type1,part,indices[[1]]],b,Spinor["Spin","Upper",type2,part,indices[[2]]]]SpinorChain[a3,Spinor["Spin","Upper",type3,part,indices[[3]]],b3]SpinorChain[a4,Spinor["Spin","Upper",type4,part,indices[[4]]],b4],
SpinorChain[a1___,Spinor["Spin",type1_,part],b1___]SpinorChain[a2___,Spinor["Spin",type2_,part],b2___]SpinorChain[a3___,Spinor["Spin",type3_,part],b3___]SpinorChain[a4___,Spinor["Spin",type4_,part],b4___]:>SpinorChain[a1,Spinor["Spin","Upper",type1,part,indices[[1]]],b1]SpinorChain[a2,Spinor["Spin","Upper",type2,part,indices[[2]]],b2]SpinorChain[a3,Spinor["Spin","Upper",type3,part,indices[[3]]],b3]SpinorChain[a4,Spinor["Spin","Upper",type4,part,indices[[4]]],b4]};,
(*Spin 3/2*)
Length[indices]==3,
replacementRules={
SpinorChain[Spinor["Spin",type1_,part],c___,Spinor["Spin",type2_,part]]SpinorChain[a3___,Spinor["Spin",type3_,part],b3___]:>SpinorChain[Spinor["Spin","Upper",type1,part,indices[[1]]],c,Spinor["Spin","Upper",type2,part,indices[[2]]]]SpinorChain[a3,Spinor["Spin","Upper",type3,part,indices[[3]]],b3],
SpinorChain[a1___,Spinor["Spin",type1_,part],b1___]SpinorChain[a2___,Spinor["Spin",type2_,part],b2___]SpinorChain[a3___,Spinor["Spin",type3_,part],b3___]:>SpinorChain[a1,Spinor["Spin","Upper",type1,part,indices[[1]]],b1]SpinorChain[a2,Spinor["Spin","Upper",type2,part,indices[[2]]],b2]SpinorChain[a3,Spinor["Spin","Upper",type3,part,indices[[3]]],b3]};,
(*Spin 1*)
Length[indices]==2,
replacementRules={
SpinorChain[Spinor["Spin",type1_,part],d___,Spinor["Spin",type2_,part]]:>SpinorChain[Spinor["Spin","Upper",type1,part,indices[[1]]],d,Spinor["Spin","Upper",type2,part,indices[[2]]]],
SpinorChain[b___,Spinor["Spin",type1_,part],d___]SpinorChain[e___,Spinor["Spin",type2_,part],f___]:> SpinorChain[b,Spinor["Spin","Upper",type1,part,indices[[1]]],d]SpinorChain[e,Spinor["Spin","Upper",type2,part,indices[[2]]],f],
SpinorChain[b___,Spinor["Spin",type1_,part],d___]^2:> SpinorChain[b,Spinor["Spin","Upper",type1,part,indices[[1]]],d]SpinorChain[b,Spinor["Spin","Upper",type1,part,indices[[2]]],d]
};,
(* Spin 1/2*)
Length[indices]==1,
replacementRules={
Spinor["Spin",type1_,part]:>Spinor["Spin","Upper",type1,part,indices[[1]]],
SpinorHat["Spin",type1_,part]:>SpinorHat["Spin","Upper",type1,part,indices[[1]]]
}
];
Expand[amp]/.replacementRules
]


(* ::Input:: *)
(*(*Spin 2 Tests*)*)
(*nice[MakeIndicesExplicit[SpinorChain[Spinor["Spin","Angle",3],Mom[5],Mom[4],Mom[1],Spinor["Spin","Square",3]]SpinorChain[Spinor["Spin","Angle",3],Mom[2],Spinor["Spin","Square",3]],3,{aa,bb,cc,dd}]]*)
(*nice[MakeIndicesExplicit[SpinorChain[Spinor["Spin","Angle",3],Mom[5],Mom[4],Mom[1],Spinor["Spin","Square",3]]SpinorChain[Spinor["Spin","Angle",1],Mom[2],Spinor["Spin","Square",3]]SpinorChain[Spinor["Spin","Square",5],Mom[1],Mom[2],Spinor["Spin","Square",3]],3,{aa,bb,cc,dd}]]*)
(*nice[MakeIndicesExplicit[SpinorChain[Spinor["Spin","Angle",4],Spinor["Spin","Angle",3]]SpinorChain[Spinor["Spin","Angle",3],Mom[5],Spinor["Spin","Square",1]]SpinorChain[Spinor["Spin","Square",3],Mom[6],Spinor["Spin","Angle",6]]SpinorChain[Spinor["Spin","Angle",1],Spinor["Spin","Angle",3]],3,{aa,bb,cc,dd}]]*)


(* ::Input:: *)
(*(*Spin 3/2 Tests*)*)
(*nice[MakeIndicesExplicit[SpinorChain[Spinor["Spin","Angle",3],Mom[5],Mom[4],Mom[1],Spinor["Spin","Square",3]]SpinorChain[Spinor["Spin","Angle",1],Mom[2],Spinor["Spin","Square",3]],3,{aa,bb,cc}]]*)
(*nice[MakeIndicesExplicit[SpinorChain[Spinor["Spin","Angle",4],Spinor["Spin","Angle",3]]SpinorChain[Spinor["Spin","Angle",3],Mom[5],Spinor["Spin","Square",1]]SpinorChain[Spinor["Spin","Square",3],Mom[6],Spinor["Spin","Angle",6]],3,{aa,bb,cc}]]*)


(* ::Input:: *)
(*(*Spin 1 Tests*)*)
(*nice[MakeIndicesExplicit[SpinorChain[Spinor["Spin","Angle",3],Mom[5],Mom[4],Mom[1],Spinor["Spin","Square",3]],3,{i,j}]]*)
(*nice[MakeIndicesExplicit[SpinorChain[Spinor["Spin","Angle",4],Spinor["Spin","Angle",3]]SpinorChain[Spinor["Spin","Angle",3],Mom[5],Spinor["Spin","Square",1]],3,{i,j}]]*)


(* ::Input:: *)
(*(*Spin 1/2 Test*)*)
(*nice[MakeIndicesExplicit[SpinorChain[Spinor["Spin","Angle",4],Spinor["Spin","Angle",3]],3,{aa}]]*)


(* ::Input::Initialization:: *)
MakeIndicesImplicit[amp_]:=Module[{ampN},
Print["Warning: Assuming all indices are upper and uncontracted!"];
ampN=amp//.{
SpinorChain[ls___,Spinor["Spin","Upper",sa_,pi_,II_],rs___]:>SpinorChain[ls,Spinor["Spin",sa,pi],rs]
};
ampN
]


(* ::Subsubsection::Closed:: *)
(*SymmetrizeSpins*)


(* ::Input::Initialization:: *)
SymmetrizeSpins[amp_,indices_]:=Module[{res=amp},
If[Head[amp]==ConstructiveAmplitude,res=amp[[2]]];
Do[
res=SymmetrizeSpin[res,partN,indices[[partN]]];
,{partN,1,Length[indices]}];
If[Head[amp]===ConstructiveAmplitude,res=ConstructiveAmplitude[amp[[1]],res,amp[[3]]]];
res
]


(* ::Input::Initialization:: *)
SymmetrizeSpin[amp_,part_,indices_]:=Module[{tmp,replacementRules={},indexPermutations,normalization},
indexPermutations=If[2<=Length[indices]<=4,Permutations[indices],{{}}];
(* Equal index labels remove duplicate permutations, but not their total weight. *)
normalization=1/Length[indexPermutations];
(*Print["Reminder: SymmetrizeSpin only works on implicit indices."];*)
Which[
(*Spin 2*)
Length[indices]==4,
replacementRules={
SpinorChain[Spinor["Spin",type1_,part],b1___,Spinor["Spin",type2_,part]]SpinorChain[Spinor["Spin",type3_,part],b2___,Spinor["Spin",type4_,part]]:>(normalization SpinorChain[Spinor["Spin","Upper",type1,part,#[[1]]],b1,Spinor["Spin","Upper",type2,part,#[[2]]]]SpinorChain[Spinor["Spin","Upper",type3,part,#[[3]]],b2,Spinor["Spin","Upper",type4,part,#[[4]]]]&/@indexPermutations/.List->Plus),
SpinorChain[Spinor["Spin",type1_,part],b___,Spinor["Spin",type2_,part]]SpinorChain[a3___,Spinor["Spin",type3_,part],b3___]SpinorChain[a4___,Spinor["Spin",type4_,part],b4___]:>(normalization SpinorChain[Spinor["Spin","Upper",type1,part,#[[1]]],b,Spinor["Spin","Upper",type2,part,#[[2]]]]SpinorChain[a3,Spinor["Spin","Upper",type3,part,#[[3]]],b3]SpinorChain[a4,Spinor["Spin","Upper",type4,part,#[[4]]],b4]&/@indexPermutations/.List->Plus),
SpinorChain[a1___,Spinor["Spin",type1_,part],b1___]SpinorChain[a2___,Spinor["Spin",type2_,part],b2___]SpinorChain[a3___,Spinor["Spin",type3_,part],b3___]SpinorChain[a4___,Spinor["Spin",type4_,part],b4___]:>(normalization SpinorChain[a1,Spinor["Spin","Upper",type1,part,#[[1]]],b1]SpinorChain[a2,Spinor["Spin","Upper",type2,part,#[[2]]],b2]SpinorChain[a3,Spinor["Spin","Upper",type3,part,#[[3]]],b3]SpinorChain[a4,Spinor["Spin","Upper",type4,part,#[[4]]],b4]&/@indexPermutations/.List->Plus)};,
(*Spin 3/2*)
Length[indices]==3,
replacementRules={
SpinorChain[Spinor["Spin",type1_,part],c___,Spinor["Spin",type2_,part]]SpinorChain[a3___,Spinor["Spin",type3_,part],b3___]:>(normalization SpinorChain[Spinor["Spin","Upper",type1,part,#[[1]]],c,Spinor["Spin","Upper",type2,part,#[[2]]]]SpinorChain[a3,Spinor["Spin","Upper",type3,part,#[[3]]],b3]&/@indexPermutations/.List->Plus),
SpinorChain[a1___,Spinor["Spin",type1_,part],b1___]SpinorChain[a2___,Spinor["Spin",type2_,part],b2___]SpinorChain[a3___,Spinor["Spin",type3_,part],b3___]:>(normalization SpinorChain[a1,Spinor["Spin","Upper",type1,part,#[[1]]],b1]SpinorChain[a2,Spinor["Spin","Upper",type2,part,#[[2]]],b2]SpinorChain[a3,Spinor["Spin","Upper",type3,part,#[[3]]],b3]&/@indexPermutations/.List->Plus)};,
(*Spin 1*)
Length[indices]==2,
replacementRules={
SpinorChain[Spinor["Spin",type1_,part],d___,Spinor["Spin",type2_,part]]:>(normalization SpinorChain[Spinor["Spin","Upper",type1,part,#[[1]]],d,Spinor["Spin","Upper",type2,part,#[[2]]]]&/@indexPermutations/.List->Plus),
SpinorChain[b___,Spinor["Spin",type1_,part],d___]SpinorChain[e___,Spinor["Spin",type2_,part],f___]:> (normalization SpinorChain[b,Spinor["Spin","Upper",type1,part,#[[1]]],d]SpinorChain[e,Spinor["Spin","Upper",type2,part,#[[2]]],f]&/@indexPermutations/.List->Plus),
SpinorChain[b___,Spinor["Spin",type_,part],d___]^2:> (normalization SpinorChain[b,Spinor["Spin","Upper",type,part,#[[1]]],d]SpinorChain[b,Spinor["Spin","Upper",type,part,#[[2]]],d]&/@indexPermutations/.List->Plus)
};,
(* Spin 1/2*)
Length[indices]==1,
replacementRules={Spinor["Spin",type1_,part]:>Spinor["Spin","Upper",type1,part,indices[[1]]]}
];
Expand[amp]/.replacementRules
]


(* ::Input:: *)
(*(*Spin 2 Tests*)*)
(*nice[SymmetrizeSpin[SpinorChain[Spinor["Spin","Angle",3],Mom[5],Mom[4],Mom[1],Spinor["Spin","Square",3]]SpinorChain[Spinor["Spin","Angle",3],Mom[2],Spinor["Spin","Square",3]],3,{aa,bb,cc,dd}]]*)
(*nice[SymmetrizeSpin[SpinorChain[Spinor["Spin","Angle",3],Mom[5],Mom[4],Mom[1],Spinor["Spin","Square",3]]SpinorChain[Spinor["Spin","Angle",1],Mom[2],Spinor["Spin","Square",3]]SpinorChain[Spinor["Spin","Square",5],Mom[1],Mom[2],Spinor["Spin","Square",3]],3,{aa,bb,cc,dd}]]*)
(*nice[SymmetrizeSpin[SpinorChain[Spinor["Spin","Angle",4],Spinor["Spin","Angle",3]]SpinorChain[Spinor["Spin","Angle",3],Mom[5],Spinor["Spin","Square",1]]SpinorChain[Spinor["Spin","Square",3],Mom[6],Spinor["Spin","Angle",6]]SpinorChain[Spinor["Spin","Angle",1],Spinor["Spin","Angle",3]],3,{aa,bb,cc,dd}]]*)


(* ::Input:: *)
(*(*Spin 3/2 Tests*)*)
(*nice[SymmetrizeSpin[SpinorChain[Spinor["Spin","Angle",3],Mom[5],Mom[4],Mom[1],Spinor["Spin","Square",3]]SpinorChain[Spinor["Spin","Angle",1],Mom[2],Spinor["Spin","Square",3]],3,{aa,bb,cc}]]*)
(*nice[SymmetrizeSpin[SpinorChain[Spinor["Spin","Angle",4],Spinor["Spin","Angle",3]]SpinorChain[Spinor["Spin","Angle",3],Mom[5],Spinor["Spin","Square",1]]SpinorChain[Spinor["Spin","Square",3],Mom[6],Spinor["Spin","Angle",6]],3,{aa,bb,cc}]]*)


(* ::Input:: *)
(*(*Spin1 Tests*)*)
(*nice[SymmetrizeSpin[SpinorChain[Spinor["Spin","Angle",3],Mom[5],Mom[4],Mom[1],Spinor["Spin","Square",3]],3,{i,j}]]*)
(*nice[SymmetrizeSpin[SpinorChain[Spinor["Spin","Angle",4],Spinor["Spin","Angle",3]]SpinorChain[Spinor["Spin","Angle",3],Mom[5],Spinor["Spin","Square",1]],3,{i,j}]]*)


(* ::Input:: *)
(*(*Spin 1/2 Test*)*)
(*nice[SymmetrizeSpin[SpinorChain[Spinor["Spin","Angle",4],Spinor["Spin","Angle",3]],3,{i}]]*)


(* ::Subsubsection::Closed:: *)
(*Conjugate*)


(* ::Input::Initialization:: *)
Unprotect[Conjugate];
Conjugate[ConstructiveAmplitude[coupl_,numer_,denom_]]:=ConstructiveAmplitude[Conjugate[coupl],Conjugate[numer],denom];
Conjugate[SpinorChain[spinors__]]:=Module[{res},
(*Print["Reminder: Conjugate[SpinorChain] only works when all indices are explicit.  Also, does not work on complexified momenta states.  Also, contracted indices must already be replaced."];*)
res=SpinorChain[spinors]//.{
Spinor["Spin","Upper","Angle",a__]:>SpinSpinorTmp["Lower","Square",a],
Spinor["Spin","Lower","Angle",a__]:>-SpinSpinorTmp["Upper","Square",a],
Spinor["Spin","Upper","Square",a__]:>-SpinSpinorTmp["Lower","Angle",a],
Spinor["Spin","Lower","Square",a__]:>SpinSpinorTmp["Upper","Angle",a],
Spinor["Helicity","Angle",a_]:>HelicitySpinorTmp["Square",a],
Spinor["Helicity","Square",a_]:>HelicitySpinorTmp["Angle",a],

SpinorChain[a___,-b_,c___]:>-SpinorChain[a,b,c]
}/.{
SpinorChain[a__]:>Reverse[SpinorChain[a]]
}//.{
SpinSpinorTmp[a__]:>Spinor["Spin",a],
HelicitySpinorTmp[a__]:>Spinor["Helicity",a]
}
];
Conjugate[Coupling[a_]]:=Coupling[a]
Conjugate[Mass[a_]]:=Mass[a]
Conjugate[PropDen[a_,b_]]:=PropDen[a,b]
Conjugate[PropDen[a_,b_,c_]]:=PropDen[a,b,-c]
Conjugate[MomProd[a__]]:=MomProd[a]
Conjugate[Mandelstahm[a__]]:=Mandelstahm[a]
Conjugate[a_+b_]:=Conjugate[a]+Conjugate[b]
Protect[Conjugate];


(* ::Subsubsection::Closed:: *)
(*ComplexifyMomenta*)


ComplexifyMomenta[amp_]:=amp//.{
	MomProd[pi_,pk_]:>MomProdHat12[pi,pk],
	Spinor[sh_,sqan_,pi_]:>SpinorHat[sh,sqan,pi],
	Spinor["Spin",ul_,sqan_,pi_,J_]:>SpinorHat["Spin",ul,sqan,pi,J],
	Mom[pi_]:>MomHat[pi]
}


UnhatMomenta[expr_]:=expr/.{SpinorHat->Spinor,MomHat->Mom,MomProdHat12->MomProd}


ComplexifyMomenta[amp_,pi_,pj_]:=amp//.{
	MomProd[pi,pk_]/;(pk!=pj&&pk!=pi):>MomProdHat1[pi,pk],
	MomProd[pk_,pi]/;(pk!=pj&&pk!=pi):>MomProdHat1[pi,pk],
	MomProd[pj,pk_]/;(pk!=pj&&pk!=pi):>MomProdHat1[pj,pk],
	MomProd[pk_,pj]/;(pk!=pj&&pk!=pi):>MomProdHat1[pj,pk],
	MomProd[pi,pj]:>MomProdHat12[pi,pj],
	SpinorChain[Spinor[sh_,"Square",pi],rest__]:>SpinorChain[SpinorHat[sh,"Square",pi],rest],
	SpinorChain[Spinor["Spin",ul_,"Square",pi,J_],rest__]:>SpinorChain[SpinorHat["Spin",ul,"Square",pi,J],rest],
	SpinorChain[Spinor[sh_,"Angle",pj],rest__]:>SpinorChain[SpinorHat[sh,"Angle",pj],rest],
	SpinorChain[Spinor["Spin",ul_,"Angle",pj,J_],rest__]:>SpinorChain[SpinorHat["Spin",ul,"Angle",pj,J],rest],
	SpinorChain[rest__,Spinor[sh_,"Square",pi]]:>SpinorChain[rest,SpinorHat[sh,"Square",pi]],
	SpinorChain[rest__,Spinor["Spin",ul_,"Square",pi,J_]]:>SpinorChain[rest,SpinorHat["Spin",ul,"Square",pi,J]],
	SpinorChain[rest__,Spinor[sh_,"Angle",pj]]:>SpinorChain[rest,SpinorHat[sh,"Angle",pj]],
	SpinorChain[rest__,Spinor["Spin",ul_,"Angle",pj,J_]]:>SpinorChain[rest,SpinorHat["Spin",ul,"Angle",pj,J]],
	SpinorChain[sp1__,Mom[pi],sp2__]:>SpinorChain[sp1,MomHat[pi],sp2],
	SpinorChain[sp1__,Mom[pj],sp2__]:>SpinorChain[sp1,MomHat[pj],sp2]
}


(* ::Input::Initialization:: *)
ExpandComplexMomenta[amp_,{pi_,mi_},pk_,{pj_,mj_},pl_,Mik_]:=Module[{ampN=amp,iN=0,cTerm},
If[mi=!=0&&mj=!=0&&Mik===0,
ampN=ampN//.{
MomProdHat2[pm_,pi]:>MomProdHat1[pi,pm],
MomProdHat1[pi,pm_]:>-MomProd[pk,pm],
MomProdHat2[pm_,pj]:>MomProdHat1[pj,pm],
MomProdHat1[pj,pm_]:>-MomProd[pl,pm],
MomProdHat12[pj,pi]:>MomProd[pj,pi],
MomProdHat12[pi,pj]:>MomProd[pi,pj],
(*[pi|...*)
SpinorChain[SpinorHat["Spin",ul___,"Square",pi,II___],rs__]:>-1/Mass[pi] SpinorChain[Spinor["Spin",ul,"Angle",pi,II],Mom[pk],rs],
(*...|pi]*)
SpinorChain[ls__,SpinorHat["Spin",ul___,"Square",pi,II___]]:>1/Mass[pi] SpinorChain[ls,Mom[pk],Spinor["Spin",ul,"Angle",pi,II]],
(*<pj|...*)
SpinorChain[SpinorHat["Spin",ul___,"Angle",pj,JJ___],rs__]:>-1/Mass[pj] SpinorChain[Spinor["Spin",ul,"Square",pj,JJ],Mom[pl],rs],
(*...|pj>*)
SpinorChain[ls__,SpinorHat["Spin",ul___,"Angle",pj,JJ___]]:>1/Mass[pj] SpinorChain[ls,Mom[pl],Spinor["Spin",ul,"Square",pj,JJ]],
(*[p|...pi...|pp]*)
SpinorChain[ls__,MomHat[pi],rs__]:>-SpinorChain[ls,Mom[pk],rs],
(*[p|...pj...|pp]*)
SpinorChain[ls__,MomHat[pj],rs__]:>-SpinorChain[ls,Mom[pl],rs]
}
];

If[mi=!=0&&mj===0,
ampN=ampN//.{
MomProdHat2[pm_,pi]:>MomProdHat1[pi,pm],
MomProdHat1[pi,pm_]:>MomProd[pi,pk]-1/2 SpinorChain[Spinor["Helicity","Square",pj],Mom[pi],Mom[pm],Spinor["Helicity","Square",pj]]cTerm,
MomProdHat2[pm_,pj]:>MomProdHat1[pj,pm],
MomProdHat1[pj,pm_]:>MomProd[pj,pm]+1/2 SpinorChain[Spinor["Helicity","Square",pj],Mom[pi],Mom[pm],Spinor["Helicity","Square",pj]]cTerm,
MomProdHat12[pj,pi]:>MomProd[pj,pi],
MomProdHat12[pi,pj]:>MomProd[pi,pj],
(*...[pi|...*)
SpinorChain[ls___,SpinorHat["Spin",ul___,"Square",pi,II___],rs___]:>SpinorChain[ls,Spinor["Spin",ul,"Square",pi,II],rs]+SpinorChain[ls,Spinor["Helicity","Square",pj],rs]SpinorChain[Spinor["Spin",ul,"Square",pi,II],Spinor["Helicity","Square",pj]]cTerm,
(*<pj|...*)
SpinorChain[SpinorHat["Helicity","Angle",pj],rs__]:>SpinorChain[Spinor["Helicity","Angle",pj],rs]+SpinorChain[Spinor["Helicity","Square",pj],Mom[pi],rs]cTerm,
(*...|pj>*)
SpinorChain[ls__,SpinorHat["Helicity","Angle",pj]]:>SpinorChain[ls,Spinor["Helicity","Angle",pj]]-SpinorChain[ls,Mom[pi],Spinor["Helicity","Square",pj]]cTerm,
(*[p|pi...*)
SpinorChain[Spinor[hs__,"Square",pp__],MomHat[pi],rs__]:>SpinorChain[Spinor[hs,"Square",pp],Mom[pi],rs]-SpinorChain[Spinor[hs,"Square",pp],Spinor["Helicity","Square",pj]]
SpinorChain[Spinor["Helicity","Square",pj],Mom[pi],rs]cTerm,
(*[p|pj...*)
SpinorChain[Spinor[hs__,"Square",pp__],MomHat[pj],rs__]:>SpinorChain[Spinor[hs,"Square",pp],Mom[pj],rs]+SpinorChain[Spinor[hs,"Square",pp],Spinor["Helicity","Square",pj]]
SpinorChain[Spinor["Helicity","Square",pj],Mom[pi],rs]cTerm,
(*...pi|p]*)
SpinorChain[ls__,MomHat[pi],Spinor[hs__,"Square",pp__]]:>SpinorChain[ls,Mom[pi],Spinor[hs,"Square",pp]]+SpinorChain[Spinor["Helicity","Square",pj],Spinor[hs,"Square",pp]]
SpinorChain[ls,Mom[pi],Spinor["Helicity","Square",pj]]cTerm,
(*...pj|p]*)
SpinorChain[ls__,MomHat[pj],Spinor[hs__,"Square",pp__]]:>SpinorChain[ls,Mom[pj],Spinor[hs,"Square",pp]]-SpinorChain[Spinor["Helicity","Square",pj],Spinor[hs,"Square",pp]]
SpinorChain[ls,Mom[pi],Spinor["Helicity","Square",pj]]cTerm,
(*<p|pi...*)
SpinorChain[Spinor[hs__,"Angle",pp__],MomHat[pi],rs__]:>SpinorChain[Spinor[hs,"Angle",pp],Mom[pi],rs]+SpinorChain[Spinor[hs,"Angle",pp],Mom[pi],Spinor["Helicity","Square",pj]]
SpinorChain[Spinor["Helicity","Square",pj],rs]cTerm,
(*<p|pj...*)
SpinorChain[Spinor[hs__,"Angle",pp__],MomHat[pj],rs__]:>SpinorChain[Spinor[hs,"Angle",pp],Mom[pj],rs]-SpinorChain[Spinor[hs,"Angle",pp],Mom[pi],Spinor["Helicity","Square",pj]]
SpinorChain[Spinor["Helicity","Square",pj],rs]cTerm,
(*...pi|p>*)
SpinorChain[ls__,MomHat[pi],Spinor[hs__,"Angle",pp__]]:>SpinorChain[ls,Mom[pi],Spinor[hs,"Angle",pp]]-SpinorChain[Spinor["Helicity","Square",pj],Mom[pi],Spinor[hs,"Angle",pp]]
SpinorChain[ls,Spinor["Helicity","Square",pj]]cTerm,
(*...pj|p>*)
SpinorChain[ls__,MomHat[pi],Spinor[hs__,"Angle",pp__]]:>SpinorChain[ls,Mom[pi],Spinor[hs,"Angle",pp]]+SpinorChain[Spinor["Helicity","Square",pj],Mom[pi],Spinor[hs,"Angle",pp]]
SpinorChain[ls,Spinor["Helicity","Square",pj]]cTerm
}/.{
(*cTerm->(Mass[pi]^2+2MomProd[pi,pj]-Mik^2)/SpinorChain[Spinor["Helicity","Square",pj],Mom[pi],Mom[pk],Spinor["Helicity","Square",pj]]*)
cTerm->PropDen[Mom[pi]+Mom[pk],Mik]/SpinorChain[Spinor["Helicity","Square",pj],Mom[pi],Mom[pk],Spinor["Helicity","Square",pj]]
}
];

If[mi===0&&mj=!=0,
ampN=ampN//.{
MomProdHat2[pm_,pi]:>MomProdHat1[pi,pm],
MomProdHat1[pi,pm_]:>MomProd[pi,pk]-1/2 SpinorChain[Spinor["Helicity","Angle",pi],Mom[pj],Mom[pm],Spinor["Helicity","Angle",pi]]cTerm,
MomProdHat2[pm_,pj]:>MomProdHat1[pj,pm],
MomProdHat1[pj,pm_]:>MomProd[pj,pm]+1/2 SpinorChain[Spinor["Helicity","Angle",pi],Mom[pj],Mom[pm],Spinor["Helicity","Angle",pi]]cTerm,
MomProdHat12[pj,pi]:>MomProd[pj,pi],
MomProdHat12[pi,pj]:>MomProd[pi,pj],
(*[p|pi...*)
SpinorChain[Spinor[hs__,"Square",pp__],MomHat[pi],rs__]:>SpinorChain[Spinor[hs,"Square",pp],Mom[pi],rs]+SpinorChain[Spinor[hs,"Square",pp],Mom[pj],Spinor["Helicity","Angle",pi]]
SpinorChain[Spinor["Helicity","Angle",pi],rs]cTerm,
(*[p|pj...*)
SpinorChain[Spinor[hs__,"Square",pp__],MomHat[pj],rs__]:>SpinorChain[Spinor[hs,"Square",pp],Mom[pj],rs]-SpinorChain[Spinor[hs,"Square",pp],Mom[pj],Spinor["Helicity","Angle",pi]]
SpinorChain[Spinor["Helicity","Angle",pi],rs]cTerm,
(*...pi|p]*)
SpinorChain[ls__,MomHat[pi],Spinor[hs__,"Square",pp__]]:>SpinorChain[ls,Mom[pi],Spinor[hs,"Square",pp]]-SpinorChain[Spinor["Helicity","Angle",pi],Mom[pj],Spinor[hs,"Square",pp]]
SpinorChain[ls,Spinor["Helicity","Angle",pi]]cTerm,
(*...pj|p]*)
SpinorChain[ls__,MomHat[pj],Spinor[hs__,"Square",pp__]]:>SpinorChain[ls,Mom[pj],Spinor[hs,"Square",pp]]+SpinorChain[Spinor["Helicity","Angle",pi],Mom[pj],Spinor[hs,"Square",pp]]
SpinorChain[ls,Spinor["Helicity","Angle",pi]]cTerm,
(*<p|pi...*)
SpinorChain[Spinor[hs__,"Angle",pp__],MomHat[pi],rs__]:>SpinorChain[Spinor[hs,"Angle",pp],Mom[pi],rs]-SpinorChain[Spinor[hs,"Angle",pp],Spinor["Helicity","Angle",pi]]
SpinorChain[Spinor["Helicity","Angle",pi],Mom[pj],rs]cTerm,
(*<p|pj...*)
SpinorChain[Spinor[hs__,"Angle",pp__],MomHat[pj],rs__]:>SpinorChain[Spinor[hs,"Angle",pp],Mom[pj],rs]+SpinorChain[Spinor[hs,"Angle",pp],Spinor["Helicity","Angle",pi]]
SpinorChain[Spinor["Helicity","Angle",pi],Mom[pj],rs]cTerm,
(*...pi|p>*)
SpinorChain[ls__,MomHat[pi],Spinor[hs__,"Angle",pp__]]:>SpinorChain[ls,Mom[pi],Spinor[hs,"Angle",pp]]+SpinorChain[Spinor["Helicity","Angle",pi],Spinor[hs,"Angle",pp]]
SpinorChain[ls,Mom[pj],Spinor["Helicity","Angle",pi]]cTerm,
(*...pj|p>*)
SpinorChain[ls__,MomHat[pj],Spinor[hs__,"Angle",pp__]]:>SpinorChain[ls,Mom[pj],Spinor[hs,"Angle",pp]]-SpinorChain[Spinor["Helicity","Angle",pi],Spinor[hs,"Angle",pp]]
SpinorChain[ls,Mom[pj],Spinor["Helicity","Angle",pi]]cTerm,
(*...pipn|p>*)
SpinorChain[ls__,MomHat[pi],Mom[pn_],Spinor[hs__,"Angle",pp__]]:>SpinorChain[ls,Mom[pi],Mom[pn],Spinor[hs,"Angle",pp]]-SpinorChain[Spinor["Helicity","Angle",pi],Mom[pj],Mom[pn],Spinor[hs,"Angle",pp]]
SpinorChain[ls,Spinor["Helicity","Angle",pi]]cTerm,
(*...pjpn|p>*)
SpinorChain[ls__,MomHat[pj],Mom[pn_],Spinor[hs__,"Angle",pp__]]:>SpinorChain[ls,Mom[pj],Mom[pn],Spinor[hs,"Angle",pp]]+SpinorChain[Spinor["Helicity","Angle",pi],Mom[pj],Mom[pn],Spinor[hs,"Angle",pp]]
SpinorChain[ls,Spinor["Helicity","Angle",pi]]cTerm,
(*[pi|...*)
SpinorChain[SpinorHat["Helicity","Square",pi],rs__]:>SpinorChain[Spinor["Helicity","Square",pi],rs]-SpinorChain[Spinor["Helicity","Angle",pi],Mom[pj],rs]cTerm,
(*...|pi]*)
SpinorChain[ls__,SpinorHat["Helicity","Square",pi]]:>SpinorChain[ls,Spinor["Helicity","Square",pi]]+SpinorChain[ls,Mom[pj],Spinor["Helicity","Angle",pi]]cTerm,
(*...<pj|...*)
SpinorChain[ls___,SpinorHat["Spin",ul___,"Angle",pj,II___],rs___]:>SpinorChain[ls,Spinor["Spin",ul,"Angle",pj,II],rs]+SpinorChain[ls,Spinor["Helicity","Angle",pi],rs]SpinorChain[Spinor["Helicity","Angle",pi],Spinor["Spin",ul,"Angle",pj,II]]cTerm
}/.{
(*cTerm->(Mass[pj]^2+2MomProd[pi,pj]-Mik^2)/SpinorChain[Spinor["Helicity","Angle",pi],Mom[pj],Mom[pk],Spinor["Helicity","Angle",pi]]*)
cTerm->PropDen[Mom[pi]+Mom[pk],Mik]/SpinorChain[Spinor["Helicity","Angle",pi],Mom[pj],Mom[pk],Spinor["Helicity","Angle",pi]]
}
];


If[mi===0&&mj===0,
ampN=ampN//.{
MomProdHat2[pm_,pi]:>MomProdHat1[pi,pm],
MomProdHat1[pi,pm_]:>MomProd[pi,pk]-1/2 SpinorChain[Spinor["Helicity","Angle",pi],Mom[pm],Spinor["Helicity","Square",pj]]cTerm,
MomProdHat2[pm_,pj]:>MomProdHat1[pj,pm],
MomProdHat1[pj,pm_]:>MomProd[pj,pm]+1/2 SpinorChain[Spinor["Helicity","Angle",pi],Mom[pm],Spinor["Helicity","Square",pj]]cTerm,
MomProdHat12[pj,pi]:>MomProd[pj,pi],
MomProdHat12[pi,pj]:>MomProd[pi,pj],
(*[p|pi...*)
SpinorChain[Spinor[hs__,"Square",pp__],MomHat[pi],rs__]:>SpinorChain[Spinor[hs,"Square",pp],Mom[pi],rs]-SpinorChain[Spinor[hs,"Square",pp],Spinor["Helicity","Square",pj]]
SpinorChain[Spinor["Helicity","Angle",pi],rs]cTerm,
(*[p|pj...*)
SpinorChain[Spinor[hs__,"Square",pp__],MomHat[pj],rs__]:>SpinorChain[Spinor[hs,"Square",pp],Mom[pj],rs]+SpinorChain[Spinor[hs,"Square",pp],Spinor["Helicity","Square",pj]]
SpinorChain[Spinor["Helicity","Angle",pi],rs]cTerm,
(*...pi|p]*)
SpinorChain[ls__,MomHat[pi],Spinor[hs__,"Square",pp__]]:>SpinorChain[ls,Mom[pi],Spinor[hs,"Square",pp]]-SpinorChain[Spinor["Helicity","Square",pj],Spinor[hs,"Square",pp]]
SpinorChain[ls,Spinor["Helicity","Angle",pi]]cTerm,
(*...pj|p]*)
SpinorChain[ls__,MomHat[pj],Spinor[hs__,"Square",pp__]]:>SpinorChain[ls,Mom[pj],Spinor[hs,"Square",pp]]+SpinorChain[Spinor["Helicity","Square",pj],Spinor[hs,"Square",pp]]
SpinorChain[ls,Spinor["Helicity","Angle",pi]]cTerm,
(*<p|pi...*)
SpinorChain[Spinor[hs__,"Angle",pp__],MomHat[pi],rs__]:>SpinorChain[Spinor[hs,"Angle",pp],Mom[pi],rs]-SpinorChain[Spinor[hs,"Angle",pp],Spinor["Helicity","Angle",pi]]
SpinorChain[Spinor["Helicity","Square",pj],rs]cTerm,
(*<p|pj...*)
SpinorChain[Spinor[hs__,"Angle",pp__],MomHat[pj],rs__]:>SpinorChain[Spinor[hs,"Angle",pp],Mom[pj],rs]+SpinorChain[Spinor[hs,"Angle",pp],Spinor["Helicity","Angle",pi]]
SpinorChain[Spinor["Helicity","Square",pj],rs]cTerm,
(*...pi|p>*)
SpinorChain[ls__,MomHat[pi],Spinor[hs__,"Angle",pp__]]:>SpinorChain[ls,Mom[pi],Spinor[hs,"Angle",pp]]-SpinorChain[Spinor["Helicity","Angle",pi],Spinor[hs,"Angle",pp]]
SpinorChain[ls,Spinor["Helicity","Square",pj]]cTerm,
(*...pj|p>*)
SpinorChain[ls__,MomHat[pj],Spinor[hs__,"Angle",pp__]]:>SpinorChain[ls,Mom[pj],Spinor[hs,"Angle",pp]]+SpinorChain[Spinor["Helicity","Angle",pi],Spinor[hs,"Angle",pp]]
SpinorChain[ls,Spinor["Helicity","Square",pj]]cTerm,
(*...pipn|p>*)
SpinorChain[ls__,MomHat[pi],Mom[pn_],Spinor[hs__,"Angle",pp__]]:>SpinorChain[ls,Mom[pi],Mom[pn],Spinor[hs,"Angle",pp]]-SpinorChain[Spinor["Helicity","Square",pj],Mom[pn],Spinor[hs,"Angle",pp]]
SpinorChain[ls,Spinor["Helicity","Angle",pi]]cTerm,
(*...pjpn|p>*)
SpinorChain[ls__,MomHat[pj],Mom[pn_],Spinor[hs__,"Angle",pp__]]:>SpinorChain[ls,Mom[pj],Mom[pn],Spinor[hs,"Angle",pp]]+SpinorChain[Spinor["Helicity","Square",pj],Mom[pn],Spinor[hs,"Angle",pp]]
SpinorChain[ls,Spinor["Helicity","Angle",pi]]cTerm,
(*[pi|...*)
SpinorChain[SpinorHat["Helicity","Square",pi],rs__]:>SpinorChain[Spinor["Helicity","Square",pi],rs]-SpinorChain[Spinor["Helicity","Square",pj],rs]cTerm,
(*...|pi]*)
SpinorChain[ls__,SpinorHat["Helicity","Square",pi]]:>SpinorChain[ls,Spinor["Helicity","Square",pi]]-SpinorChain[ls,Spinor["Helicity","Square",pj]]cTerm,
(*...<pj|...*)
SpinorChain[ls___,SpinorHat["Helicity","Angle",pj],rs___]:>SpinorChain[ls,Spinor["Helicity","Angle",pj],rs]+SpinorChain[ls,Spinor["Helicity","Angle",pi],rs]cTerm
}/.{
(*cTerm->(Mass[pj]^2+2MomProd[pi,pj]-Mik^2)/SpinorChain[Spinor["Helicity","Angle",pi],Mom[pj],Mom[pk],Spinor["Helicity","Angle",pi]]*)
cTerm->PropDen[Mom[pi]+Mom[pk],Mik]/SpinorChain[Spinor["Helicity","Angle",pi],Mom[pk],Spinor["Helicity","Square",pj]]
}
];


ampN
]


(* ::Input:: *)
(*MomProdHat1[1,2]*)
(*ExpandComplexMomenta[MomProdHat1[1,2],{1,Me},{3,Mm}]*)


(* ::Input:: *)
(*MomProdHat1[3,2]*)
(*ExpandComplexMomenta[MomProdHat1[3,2],{1,Me},{3,Mm}]*)


(* ::Input:: *)
(*MomProdHat12[1,3]*)
(*ExpandComplexMomenta[MomProdHat12[1,3],{1,Me},{3,Mm}]*)


(* ::Input::Initialization:: *)
ReplaceSpinorZ[amp_,{pi_,mi_},{pj_,mj_},pk_]:=Module[{ampN=amp},
If[mi=!=0&&mj=!=0,
ampN=ampN//.{
SpinorZ["Both",II_,JJ_]:>1/(mi mj) (mi SpinorChain[Spinor["Spin","Lower","Square",pi,II],Spinor["Spin","Upper","Square",pj,JJ]]+SpinorChain[Spinor["Spin","Lower","Angle",pi,II],Mom[pk],Spinor["Spin","Upper","Square",pj,JJ]])
}];
ampN
]


(* ::Subsubsection::Closed:: *)
(*SquareAmplitude*)


(* ::Input::Initialization:: *)
SquareAmplitude::usage="Check this out!";
SquareAmplitude[amp_,spins_]:=Module[{res,ampInd=amp,ampHC=amp,indices={}},
(*Print["Reminder: SquareAmplitude only works when all indices are implicit and all contracted indices are replaced.  Also, it does not work on complexified momenta states."];*)
Do[
Which[
spins[[ii]]==0,AppendTo[indices,{}],
Abs[spins[[ii]]]==1/2,AppendTo[indices,{ToExpression["i"<>ToString[ii]]}],
Abs[spins[[ii]]]==1,AppendTo[indices,{ToExpression["i"<>ToString[ii]],ToExpression["j"<>ToString[ii]]}],
Abs[spins[[ii]]]==3/2,AppendTo[indices,{ToExpression["i"<>ToString[ii]],ToExpression["j"<>ToString[ii]],ToExpression["k"<>ToString[ii]]}],
Abs[spins[[ii]]]==2,AppendTo[indices,{ToExpression["i"<>ToString[ii]],ToExpression["j"<>ToString[ii]],ToExpression["k"<>ToString[ii]],ToExpression["l"<>ToString[ii]]}]
];
,{ii,1,Length[spins]}];(*Print[indices];*)

ampHC=MakeIndicesExplicit[ampHC,indices];(*Print[ampHC];*)
ampInd=SymmetrizeSpins[ampInd,indices];(*Print[ampInd];*)
ampHC=Conjugate[ampHC];(*Print[ampHC];*)

ampHC ampInd/.{ConstructiveAmplitude[coupl_,numer_,denom_]->coupl numer/denom}
];
(* ::Section::Closed:: *)
(*Symbolic Spinor Identities*)

(* ::Subsection::Closed:: *)
(*Public Functions*)

SchoutenRewrite::usage = "SchoutenRewrite[c1, c2, {m, n}] rewrites two spinor chains by generalized Schouten. The split counts momenta before each split; invalid chiralities or splits return $Failed.";
SchoutenCandidates::usage = "SchoutenCandidates[expr] lists exact generalized Schouten edits with Name, Subtract, Insert, Chains and Splits. No internal on-shell condition is assumed.";
SpinorIdentityCandidates::usage = "SpinorIdentityCandidates[{massRules, expr}] lists exact symbolic identity edits. OnShellChannels -> Automatic uses that diagram's internal mass rules; use an empty list for a combined amplitude. MomentumConservation defaults to True. Both option names are strings. PropDen factors remain opaque.";
ApplySpinorIdentity::usage = "ApplySpinorIdentity[{massRules, expr}, edit, opts] applies a currently legal identity edit and returns the resulting expression. The input is canonicalized exactly as in SpinorIdentityCandidates, using the same options. Invalid or stale edits return $Failed.";
SpinorExpressionComplexity::usage = "SpinorExpressionComplexity[expr] measures terms, momentum insertions and momentum products, including the branches implicit in composite momentum slots. PropDen factors remain opaque.";
ComplicateAmplitude::usage = "ComplicateAmplitude[{massRules, expr}, nSteps, opts] applies random reversible symbolic identities. RandomSeed controls reproducibility and the string option RecordSteps -> True retains Trajectory. It uses the same diagram-local OnShellChannels and MomentumConservation options as SpinorIdentityCandidates.";
ApplySpinorIdentity::edit = "The supplied edit is not legal for this expression and its current on-shell and momentum-conservation options.";
ApplySpinorIdentity::args = "Expected {massRules, expression}, an identity-edit association, and optional identity settings.";

Begin["ConstructiveDiagrams`Private`"];

(* ::Subsection::Closed:: *)
(*Generalized Schouten Identities*)

spinorType[Spinor[_, t: ("Angle" | "Square"), __]] := t;
spinorType[Spinor[_, _, t: ("Angle" | "Square"), __]] := t;
spinorType[_] := None;
spinorLeg[Spinor[_, "Angle" | "Square", i_, ___]] := i;
spinorLeg[Spinor[_, _, "Angle" | "Square", i_, ___]] := i;

validChainQ[c_SpinorChain] := Module[{xs = List @@ c, l, r},
  If[Length[xs] < 2, Return[False]];
  l = spinorType[First[xs]]; r = spinorType[Last[xs]];
  MemberQ[{"Angle", "Square"}, l] && MemberQ[{"Angle", "Square"}, r] &&
    AllTrue[Drop[Rest[xs], -1], MatchQ[#, _Mom] &] &&
    SameQ[l === r, EvenQ[Length[xs] - 2]]
];
validChainQ[_] := False;

SchoutenRewrite[c1_, c2_, {m_Integer, n_Integer}] := Module[
  {u, v, p, q, leftP, rightP, leftQ, rightQ, chains, sign},
  If[!validChainQ[c1] || !validChainQ[c2], Return[$Failed]];
  u = List @@ c1; v = List @@ c2;
  p = Drop[Rest[u], -1]; q = Drop[Rest[v], -1];
  If[! (0 <= m <= Length[p] && 0 <= n <= Length[q]), Return[$Failed]];
  leftP = Reverse[Take[p, m]]; rightP = Drop[p, m];
  leftQ = Take[q, n]; rightQ = Reverse[Drop[q, n]];
  chains = {
    SpinorChain @@ Join[{Last[v]}, rightQ, rightP, {Last[u]}],
    SpinorChain @@ Join[{First[v]}, leftQ, leftP, {First[u]}],
    SpinorChain @@ Join[{Last[v]}, rightQ, leftP, {First[u]}],
    SpinorChain @@ Join[{First[v]}, leftQ, rightP, {Last[u]}]
  };
  If[!AllTrue[chains, validChainQ], Return[$Failed]];
  sign = (-1)^(m + Length[q] - n);
  sign*(chains[[1]]*chains[[2]] - chains[[3]]*chains[[4]])
];
SchoutenRewrite[___] := $Failed;

(* Eq. (A28): reversal contributes (-1)^(number of momenta + 1). *)
canonicalChain[c_SpinorChain] := Module[{rev = Reverse[c]},
  If[validChainQ[c] && Length[c] === 2 && First[c] === Last[c], Return[0]];
  If[validChainQ[c] && !OrderedQ[{c, rev}], (-1)^(Length[c] - 1)*rev, c]
];
(* Treat propagators as opaque, including when they contain other algebraic heads. *)
withProtectedPropagators[expr_, function_] := Module[{denominators, tokens},
  denominators = DeleteDuplicates[Cases[{expr}, _PropDen, Infinity]];
  If[denominators === {}, Return[function[expr]]];
  tokens = Table[Unique["propagator"], {Length[denominators]}];
  function[expr /. Thread[denominators -> tokens]] /. Thread[tokens -> denominators]
];
(* Distribute only momentum slots, never arguments of a propagator or another head. *)
linearMomentumTerms[arg_] := Module[{terms = monomialsOf[arg], factors, momenta, coefficient, out = {}},
  If[arg === 0, Return[{}]];
  Do[
    factors = factorsOf[term]; momenta = Select[factors, MatchQ[#, _Mom] &];
    If[Length[momenta] =!= 1, Return[$Failed]];
    coefficient = term/First[momenta];
    If[!FreeQ[coefficient, _Mom | _SpinorChain | _Spinor], Return[$Failed]];
    AppendTo[out, {coefficient, First[momenta] /. Mom[Multiparticle[ii__Integer]] :>
      Mom[Multiparticle @@ Sort[{ii}]]}], {term, terms}];
  out
];
linearChain[c_SpinorChain] := Module[{xs = List @@ c, parts},
  If[Length[xs] < 2 || !MemberQ[{"Angle", "Square"}, spinorType[First[xs]]] ||
      !MemberQ[{"Angle", "Square"}, spinorType[Last[xs]]] ||
      SameQ[spinorType[First[xs]], spinorType[Last[xs]]] =!= EvenQ[Length[xs] - 2], Return[c]];
  parts = linearMomentumTerms /@ Drop[Rest[xs], -1];
  If[!FreeQ[parts, $Failed], Return[c]];
  Total[(Times @@ #[[All, 1]])*canonicalChain[
      SpinorChain @@ Join[{First[xs]}, #[[All, 2]], {Last[xs]}]] & /@ Tuples[parts]]
];
validTraceQ[t_SpinorTrace] := EvenQ[Length[t]] && AllTrue[List @@ t, MatchQ[#, _Mom] &];
validTraceQ[_] := False;
linearTrace[t_SpinorTrace] := Module[{parts},
  If[OddQ[Length[t]], Return[t]];
  parts = linearMomentumTerms /@ List @@ t;
  If[!FreeQ[parts, $Failed], Return[t]];
  Total[(Times @@ #[[All, 1]])*If[# === {}, 2, SpinorTrace @@ #[[All, 2]]] & /@ Tuples[parts]]
];
canonicalChains[expr_] := withProtectedPropagators[expr,
  Function[body, Expand[body /. {c_SpinorChain :> linearChain[c], t_SpinorTrace :> linearTrace[t],
    Mom[Multiparticle[ii__Integer]] :> Mom[Multiparticle @@ Sort[{ii}]]}]]];
canonicalAmplitude[expr_, rules_] := canonicalChains[
  withProtectedPropagators[expr, Function[body, body /. rules]]];

SchoutenCandidates[expr_] := Module[{moves = {}, mons, chains, rhs, sub, ins, mon, i, j, m, n},
  mons = monomialsOf[expr];
  Do[
    chains = Select[chainsIn[mon], validChainQ];
    Do[
      rhs = SchoutenRewrite[chains[[i]], chains[[j]], {m, n}];
      If[rhs === $Failed, Continue[]];
      sub = mon;
      ins = canonicalChains[mon/(chains[[i]]*chains[[j]])*rhs];
      If[!samePoly[canonicalChains[sub], ins],
        AppendTo[moves, <|"Name" -> "Schouten", "Subtract" -> sub,
          "Insert" -> ins, "Chains" -> chains[[{i, j}]], "Splits" -> {m, n}|>]
      ],
      {i, Length[chains]}, {j, i + 1, Length[chains]},
      {m, 0, Length[chains[[i]]] - 2}, {n, 0, Length[chains[[j]]] - 2}
    ],
    {mon, mons}
  ];
  DeleteDuplicatesBy[moves, {#["Subtract"], #["Insert"]} &]
];



(* ::Subsection::Closed:: *)
(*Expression Structure*)

expandAmplitude[expr_] := Expand[expr];

externalIndices[massRules_List] := Sort @ Cases[massRules, HoldPattern[Mass[i_Integer] -> _] :> i];

massValue[massRules_List, i_] := Replace[Mass[i], Join[massRules, {_ -> Missing["Mass"]}]];

monomialsOf[expr_] := Module[{ex = expandAmplitude[expr]},
  Which[
    ex === 0, {},
    Head[ex] === Plus, List @@ ex,
    True, {ex}
  ]
];

factorsOf[mon_] := If[Head[mon] === Times, List @@ mon, {mon}];

chainPairQ[SpinorChain[Spinor[_, _, _], Spinor[_, _, _]]] := True;
chainPairQ[_] := False;
chainKind[SpinorChain[Spinor[k_, _, _], ___]] := k;
chainType[SpinorChain[Spinor[_, tp_, _], Spinor[_, tp_, _]]] := tp;
chainEnds[SpinorChain[Spinor[_, _, i_], Spinor[_, _, j_]]] := {i, j};
threeChainQ[SpinorChain[Spinor[_, _, _], Mom[_], Spinor[_, _, _]]] := True;
threeChainQ[_] := False;

chainsIn[mon_] := Module[{factors = factorsOf[mon], out = {}, i},
  Do[
    Which[
      MatchQ[factors[[i]], _SpinorChain], AppendTo[out, factors[[i]]],
      MatchQ[factors[[i]], Power[_SpinorChain, _Integer?Positive]],
        Do[AppendTo[out, factors[[i, 1]]], {factors[[i, 2]]}]
    ],
    {i, Length[factors]}
  ];
  out
];

tracesIn[mon_] := Flatten[Function[factor, Which[
    MatchQ[factor, _SpinorTrace], {factor},
    MatchQ[factor, Power[_SpinorTrace, _Integer?Positive]], ConstantArray[factor[[1]], factor[[2]]],
    True, {}]] /@ factorsOf[mon]];
momentumSlots[c_SpinorChain] := Drop[Rest[List @@ c], -1];
momentumSlots[t_SpinorTrace] := List @@ t;
momentumSlotOffset[c_SpinorChain] := 1;
momentumSlotOffset[t_SpinorTrace] := 0;
traceMetadata[t_SpinorTrace] := <|"Trace" -> t|>;
traceMetadata[_] := <||>;

bracket[kind_, tp_, a_, b_] := SpinorChain[Spinor[kind, tp, a], Spinor[kind, tp, b]];
three[kind_, t1_, a_, p_, t2_, b_] :=
  SpinorChain[Spinor[kind, t1, a], Mom[p], Spinor[kind, t2, b]];

(* <ij><kl> = <ik><jl> - <il><jk>, and the same for square brackets.
   Checked against ReduceSpinorProducts. *)
schoutenSum[kind_, tp_, i_, j_, k_, l_] :=
  bracket[kind, tp, i, k]*bracket[kind, tp, j, l] -
  bracket[kind, tp, i, l]*bracket[kind, tp, j, k];

(* A bracket times a one-momentum chain. The contracted spinor is the chain
   end of the same chirality as the bracket. This is the epsilon identity
   <ab>(v·d) = <db>(v·a) - <da>(v·b). *)
schoutenChainSum[br_, sc_] := Module[{kind, a, b, c, p, d, tb, t1, t2},
  kind = chainKind[br];
  {a, b} = chainEnds[br];
  tb = chainType[br];
  t1 = sc[[1, 2]];
  t2 = sc[[3, 2]];
  c = sc[[1, 3]];
  p = sc[[2, 1]];
  d = sc[[3, 3]];
  Which[
    tb === "Angle" && t1 === "Square" && t2 === "Angle",
      bracket[kind, "Angle", d, b]*three[kind, "Square", c, p, "Angle", a] -
      bracket[kind, "Angle", d, a]*three[kind, "Square", c, p, "Angle", b],
    tb === "Square" && t1 === "Square" && t2 === "Angle",
      bracket[kind, "Square", c, b]*three[kind, "Square", a, p, "Angle", d] -
      bracket[kind, "Square", c, a]*three[kind, "Square", b, p, "Angle", d],
    tb === "Angle" && t1 === "Angle" && t2 === "Square",
      bracket[kind, "Angle", c, b]*three[kind, "Angle", a, p, "Square", d] -
      bracket[kind, "Angle", c, a]*three[kind, "Angle", b, p, "Square", d],
    tb === "Square" && t1 === "Angle" && t2 === "Square",
      bracket[kind, "Square", d, b]*three[kind, "Angle", c, p, "Square", a] -
      bracket[kind, "Square", d, a]*three[kind, "Angle", c, p, "Square", b],
    True, $Failed
  ]
];

structureQ[SpinorChain[__]] := True;
structureQ[SpinorTrace[___]] := True;
structureQ[MomProd[__]] := True;
structureQ[Mom[Multiparticle[__]]] := True;
structureQ[PropDen[__]] := True;
structureQ[Mandelstahm[__]] := True;
structureQ[Power[f_, _]] := structureQ[f];
structureQ[_] := False;

splitMon[mon_] := Module[{factors = factorsOf[mon], skeleton},
  skeleton = Times @@ Select[factors, structureQ];
  {Times @@ Select[factors, ! structureQ[#] &], skeleton}
];

groupsOf[expr_] := Module[{groups = <||>, mons, pair, i},
  mons = monomialsOf[expr];
  Do[
    pair = splitMon[mons[[i]]];
    groups[pair[[2]]] = Lookup[groups, pair[[2]], 0] + pair[[1]],
    {i, Length[mons]}
  ];
  groups
];


(* ::Subsection::Closed:: *)
(*Mass Rules and On-Shell Conditions*)

knownInternalChannels[massRules_List] := Select[Cases[massRules,
  HoldPattern[Mass[Multiparticle[ii__Integer]] -> m_] :> {Sort[{ii}], m}],
  Length[#[[1]]] >= 2 && DuplicateFreeQ[#[[1]]] &];
internalChannels[massRules_List] := Select[knownInternalChannels[massRules],
  $onShellChannels === Automatic || MemberQ[$onShellChannels, #[[1]]] &];
internalPairs[massRules_List] := Select[internalChannels[massRules], Length[#[[1]]] === 2 &];

shellOf[m_, ma_, mb_] := Expand[(m^2 - ma^2 - mb^2)/2];

(* coef = shell * quotient, with quotient free of the shell's variables. *)
shellQuotient[coef_, shell_] := Module[{vars, num, den, red},
  If[shell === 0, Return[None]];
  vars = Variables[shell];
  If[vars === {},
    Return[If[Expand[coef - shell] === 0, 1, None]]
  ];
  {num, den} = NumeratorDenominator[Together[coef]];
  If[! PolynomialQ[num, vars] || ! PolynomialQ[Expand[den], vars] || ! PolynomialQ[shell, vars],
    Return[None]
  ];
  red = PolynomialReduce[num, {shell}, vars];
  If[Expand[red[[2]]] =!= 0, None, Together[red[[1, 1]]/den]]
];

orderedDot[a_, b_] := MomProd @@ Sort[{a, b}];

lineMass[massRules_List, legs_List] := Module[{found},
  found = SelectFirst[internalChannels[massRules], #[[1]] === Sort[legs] &];
  If[MissingQ[found], None, found[[2]]]
];
lineMass[massRules_List, i_Integer, j_Integer] := lineMass[massRules, {i, j}];
momentumMass[rules_List, leg_Integer] := massValue[rules, leg];
momentumMass[rules_List, leg_Multiparticle] := lineMass[rules, List @@ leg];
momentumMass[_, _] := None;

scalarFactorPresentQ[sk_, atom_] := AnyTrue[factorsOf[sk], Function[factor,
  With[{f = If[MatchQ[factor, Power[_, _Integer?Positive]], List @@ factor, {factor, 1}],
      a = If[MatchQ[atom, Power[_, _Integer?Positive]], List @@ atom, {atom, 1}]},
    f[[1]] === a[[1]] && f[[2]] >= a[[2]]]]];
numeratorScalarAtoms[groups_] := DeleteDuplicates[Flatten[
  (Function[factor, If[MatchQ[factor, Power[_, _Integer?Positive]], factor[[1]], factor]] /@
    factorsOf[#]) & /@ Keys[groups]]];
scalarRelationContexts[groups_, atoms_List] := DeleteDuplicates[Flatten[Table[
  If[scalarFactorPresentQ[sk, atom], {sk/atom}, {}], {sk, Keys[groups]}, {atom, atoms}], 2]];

(* Match complete scalar relations in a shared numerator context. Structural
   factors, including entire propagators, never participate in the relation. *)
scalarRelationMoves[groups_, lhs_, rhs_, name_, legs_, identity_] := Module[
  {wanted = groupsOf[lhs], atoms, contexts, pivot, scale, moves = {}, subtract, insert},
  atoms = DeleteCases[Keys[wanted], 1];
  If[atoms === {}, Return[{}]];
  contexts = scalarRelationContexts[groups, atoms]; pivot = First[atoms];
  Do[
    scale = Lookup[groups, context pivot, 0]/wanted[pivot];
    If[samePoly[scale, 0] || !AllTrue[Keys[wanted],
        samePoly[Lookup[groups, context #, 0], scale wanted[#]] &], Continue[]];
    subtract = expandAmplitude[scale lhs context]; insert = expandAmplitude[scale rhs context];
    If[!samePoly[subtract, insert], AppendTo[moves, <|"Name" -> name,
      "Subtract" -> subtract, "Insert" -> insert, "Legs" -> legs, "Identity" -> identity|>]],
    {context, contexts}];
  moves
];

(* 2 p_i·p_j = (p_i+p_j)^2 - m_i^2 - m_j^2, written Mom[Multiparticle[i,j]]^2
   so Expand does not turn the square into ordinary products. An internal
   line also has (p_i+p_j)^2 = M^2. *)

(* ::Subsection::Closed:: *)
(*Momentum-Square Identities*)

momentumSquareMoves[expr_, massRules_List] := Module[{moves = {}, groups, keys, exts, atoms, pairs, pair, g},
  groups = groupsOf[expr];
  keys = Keys[groups];
  exts = externalIndices[massRules];
  atoms = numeratorScalarAtoms[groups];
  pairs = DeleteDuplicates[Join[Cases[atoms, MomProd[i_Integer, j_Integer] /; i =!= j :> Sort[{i, j}]],
    Cases[atoms, Mom[Multiparticle[i_Integer, j_Integer]] :> Sort[{i, j}]],
    First /@ internalPairs[massRules]]];
  Do[
    Module[{i = pair[[1]], j = pair[[2]], mi, mj, mp2, dot, kin, bigM, sk, coef, rest, subtract, insert},
      mi = massValue[massRules, i];
      mj = massValue[massRules, j];
      If[mi === Missing["Mass"] || mj === Missing["Mass"], Continue[]];
      mp2 = Mom[Multiparticle[i, j]]^2;
      dot = orderedDot[i, j];
      kin = mp2 - mi^2 - mj^2;
      bigM = lineMass[massRules, i, j];
      Do[
        sk = keys[[g]]; coef = groups[sk];
        If[MemberQ[factorsOf[sk], dot],
          rest = sk/dot;
          subtract = expandAmplitude[coef*sk];
          insert = expandAmplitude[coef*rest*kin/2];
          If[! samePoly[subtract, insert],
            AppendTo[moves, <|"Name" -> "MomentumSquare", "Subtract" -> subtract,
              "Insert" -> insert, "Legs" -> {i, j}|>]
          ]
        ];
        If[MemberQ[factorsOf[sk], mp2],
          rest = sk/mp2;
          subtract = expandAmplitude[coef*sk];
          insert = expandAmplitude[coef*rest*(2*dot + mi^2 + mj^2)];
          If[! samePoly[subtract, insert],
            AppendTo[moves, <|"Name" -> "MomentumSquare", "Subtract" -> subtract,
              "Insert" -> insert, "Legs" -> {i, j}|>]
          ];
          If[bigM =!= None && ! (bigM === 0 && mi === 0 && mj === 0),
            insert = expandAmplitude[coef*rest*bigM^2];
            If[! samePoly[subtract, insert],
              AppendTo[moves, <|"Name" -> "OnShell", "Subtract" -> subtract,
                "Insert" -> insert, "Legs" -> {i, j}, "Mass" -> bigM|>]
            ]
          ]
        ],
        {g, Length[keys]}
      ];
      If[bigM =!= None,
        Module[{shell2 = Expand[bigM^2 - mi^2 - mj^2], q},
          Do[
            sk = keys[[g]]; coef = groups[sk];
            If[withProtectedPropagators[sk, Function[body,
                FreeQ[body, Mom[Multiparticle[__]]] && FreeQ[body, dot]]],
              q = shellQuotient[coef, shell2];
              If[q =!= None && Expand[q] =!= 0,
                subtract = expandAmplitude[coef*sk];
                insert = expandAmplitude[q*kin*sk];
                If[! samePoly[subtract, insert],
                  AppendTo[moves, <|"Name" -> "MomentumSquare", "Subtract" -> subtract,
                    "Insert" -> insert, "Legs" -> {i, j}|>]
                ]
              ]
            ],
            {g, Length[keys]}
          ]
        ]
      ]
    ],
    {pair, pairs}
  ];
  Join[moves, subsetMomentumSquareMoves[expr, massRules]]
];

subsetMomentumSquareMoves[expr_, rules_List] := Module[
  {moves = {}, groups = groupsOf[expr], subsets, present, legs, label, square, masses,
   value, dots, mass, subtract, insert, rest, scale, coef},
  present = DeleteDuplicates[Cases[numeratorScalarAtoms[groups],
    Mom[q:Multiparticle[ii__Integer]] /; Length[{ii}] >= 3 && DuplicateFreeQ[{ii}] :> {q, Sort[{ii}]}]];
  subsets = DeleteDuplicates[Join[present,
    ({Multiparticle @@ #[[1]], #[[1]]} &) /@ Select[knownInternalChannels[rules], Length[#[[1]]] >= 3 &]]];
  Do[
    {label, legs} = subset; square = Mom[label]^2;
    masses = massValue[rules, #] & /@ legs; mass = lineMass[rules, legs];
    dots = orderedDot @@ # & /@ Subsets[legs, {2}];
    value = If[AnyTrue[masses, MissingQ], None, Total[masses^2] + 2 Total[dots]];
    Do[
      coef = groups[sk];
      If[scalarFactorPresentQ[sk, square],
        rest = sk/square; subtract = expandAmplitude[coef sk];
        If[value =!= None, AppendTo[moves, <|"Name" -> "MomentumSquare", "Subtract" -> subtract,
          "Insert" -> expandAmplitude[coef rest value], "Legs" -> legs,
          "Identity" -> "ExpandSubset"|>]];
        If[mass =!= None, AppendTo[moves, <|"Name" -> "OnShell", "Subtract" -> subtract,
          "Insert" -> expandAmplitude[coef rest mass^2], "Legs" -> legs,
          "Mass" -> mass, "Identity" -> "SubsetSquare"|>]]];
      If[mass =!= None && !TrueQ[mass == 0] && !FreeQ[coef, mass],
        scale = shellQuotient[coef, mass^2];
        If[scale =!= None && !samePoly[scale, 0],
          AppendTo[moves, <|"Name" -> "OnShell", "Subtract" -> expandAmplitude[coef sk],
            "Insert" -> expandAmplitude[scale square sk], "Legs" -> legs,
            "Mass" -> mass, "Identity" -> "ExpandSubsetMass"|>];
          If[value =!= None, AppendTo[moves, <|"Name" -> "OnShell", "Subtract" -> expandAmplitude[coef sk],
            "Insert" -> expandAmplitude[scale value sk], "Legs" -> legs,
            "Mass" -> mass, "Identity" -> "ExpandSubsetMassValue"|>]]]],
      {sk, Keys[groups]}];
    If[value =!= None,
      moves = Join[moves, scalarRelationMoves[groups, value, square,
        "MomentumSquare", legs, "CollectSubset"]];
      If[mass =!= None, moves = Join[moves, scalarRelationMoves[groups, value, mass^2,
        "OnShell", legs, "SubsetInvariant"]]]],
    {subset, subsets}];
  moves
];

scalarMomentumConservationMoves[expr_, rules_List] := Module[
  {moves = {}, groups = groupsOf[expr], exts = externalIndices[rules], dots,
   legs, mass, rhs, subtract, insert, scale, row, complement},
  If[!TrueQ[$useMomentumConservation] || Length[exts] < 2, Return[{}]];
  dots = Cases[numeratorScalarAtoms[groups], d:MomProd[i_Integer, j_Integer] /;
    i =!= j && MemberQ[exts, i] && MemberQ[exts, j] :> d];
  Do[
    legs = List @@ dot;
    Do[
      mass = massValue[rules, anchor];
      rhs = -mass^2 - Total[orderedDot[anchor, #] & /@ Complement[exts, legs]];
      Do[If[scalarFactorPresentQ[sk, dot],
        subtract = expandAmplitude[groups[sk] sk]; insert = expandAmplitude[groups[sk] sk/dot rhs];
        If[!samePoly[subtract, insert], AppendTo[moves, <|"Name" -> "MomentumConservation",
          "Subtract" -> subtract, "Insert" -> insert, "Legs" -> {anchor, First[DeleteCases[legs, anchor]]},
          "Identity" -> "ScalarDot"|>]]], {sk, Keys[groups]}],
      {anchor, legs}], {dot, dots}];
  (* The square of a pair equals the square of its complementary external
     momentum sum. Only external masses enter this off-shell relation. *)
  If[Length[exts] >= 4 && AllTrue[exts, !MissingQ[massValue[rules, #]] &],
    Do[
      legs = Sort[List @@ dot]; complement = Complement[exts, legs];
      rhs = (Total[massValue[rules, #]^2 & /@ complement] -
          Total[massValue[rules, #]^2 & /@ legs])/2 +
        Total[orderedDot @@ # & /@ Subsets[complement, {2}]];
      Do[If[scalarFactorPresentQ[sk, dot],
        subtract = expandAmplitude[groups[sk] sk];
        insert = expandAmplitude[groups[sk] sk/dot rhs];
        If[!samePoly[subtract, insert], AppendTo[moves, <|"Name" -> "MomentumConservation",
          "Subtract" -> subtract, "Insert" -> insert, "Legs" -> legs,
          "Identity" -> "ScalarComplement", "ScrambleEligible" -> False|>]]], {sk, Keys[groups]}],
      {dot, dots}]];
  Do[
    mass = massValue[rules, anchor]; row = Total[orderedDot[anchor, #] & /@ DeleteCases[exts, anchor]];
    moves = Join[moves, scalarRelationMoves[groups, row, -mass^2,
      "MomentumConservation", {anchor}, "ScalarRow"]];
    If[!TrueQ[mass == 0], Do[
      scale = shellQuotient[groups[sk], -mass^2];
      If[scale =!= None && !samePoly[scale, 0], AppendTo[moves, <|"Name" -> "MomentumConservation",
        "Subtract" -> expandAmplitude[groups[sk] sk], "Insert" -> expandAmplitude[scale row sk],
        "Legs" -> {anchor}, "Identity" -> "ScalarRowExpand"|>]], {sk, Keys[groups]}]],
    {anchor, exts}];
  moves
];

(* Massless helicity completeness: <i|p_j|i] = 2 p_i.p_j and
   <ij>[ji] = 2 p_i.p_j. Inverse brackets remain ordinary rational factors. *)
helicityInvariantMoves[expr_, rules_List] := Module[
  {moves = {}, mons = monomialsOf[expr], massless, chain, xs, dot, product, chains, rest},
  massless = Select[externalIndices[rules], TrueQ[massValue[rules, #] === 0] &];
  Do[
    chains = chainsIn[mon];
    Do[
      xs = List @@ chain;
      If[Length[xs] === 3 && validChainQ[chain] &&
          MatchQ[First[xs], Spinor["Helicity", "Angle" | "Square", _Integer]] &&
          Last[xs] === flipSpinor[First[xs]] && MemberQ[massless, spinorLeg[First[xs]]] &&
          MatchQ[xs[[2]], Mom[_Integer]],
        dot = If[spinorLeg[First[xs]] === xs[[2, 1]], 0,
          orderedDot[spinorLeg[First[xs]], xs[[2, 1]]] /. rules];
        AppendTo[moves, <|"Name" -> "MomentumSquare", "Identity" -> "HelicityTrace",
          "Direction" -> "Contract", "ScrambleEligible" -> False, "Subtract" -> mon,
          "Insert" -> canonicalChains[mon/chain*2 dot],
          "Legs" -> {spinorLeg[First[xs]], xs[[2, 1]]}|>]],
      {chain, DeleteDuplicates[chains]}];
    Do[
      dot = orderedDot @@ pair;
      product = canonicalChains[bracket["Helicity", "Angle", pair[[1]], pair[[2]]]*
        bracket["Helicity", "Square", pair[[2]], pair[[1]]]];
      If[scalarFactorPresentQ[mon, dot],
        AppendTo[moves, <|"Name" -> "MomentumSquare", "Identity" -> "MasslessBracket",
          "Direction" -> "Expand", "Subtract" -> mon,
          "Insert" -> canonicalChains[mon/dot*product/2], "Legs" -> pair|>]];
      If[AllTrue[Cases[product, _SpinorChain, Infinity], MemberQ[chains, #] &],
        rest = Cancel[mon/product];
        AppendTo[moves, <|"Name" -> "MomentumSquare", "Identity" -> "MasslessBracket",
          "Direction" -> "Contract", "Subtract" -> mon,
          "Insert" -> canonicalChains[rest*2 dot], "Legs" -> pair|>]],
      {pair, Subsets[massless, {2}]}],
    {mon, mons}];
  moves
];

(* Common-denominator algebra can introduce a propagator in the numerator.
   Expand that numerator factor off shell; never rewrite the denominator. *)
propagatorNumeratorMoves[expr_, rules_List] := Module[
  {moves = {}, groups = groupsOf[expr], buckets, terms, props, tokens, fraction, num, den,
   legs, masses, value, insert, subtract, replacements, usedLegs, valid},
  buckets = GroupBy[Keys[groups], # /. _PropDen -> 1 &];
  Do[
    terms = (groups[#] # &) /@ keys;
    props = DeleteDuplicates[Cases[terms, _PropDen, Infinity]];
    If[Length[props] < 2, Continue[]];
    tokens = Table[Unique["numeratorPropagator"], {Length[props]}];
    subtract = Total[terms];
    fraction = Together[subtract /. Thread[props -> tokens]];
    {num, den} = NumeratorDenominator[fraction];
    replacements = {}; usedLegs = {}; valid = True;
    Do[
      If[FreeQ[num, tokens[[i]]], Continue[]];
      If[!PolynomialQ[num, {tokens[[i]]}] ||
          !MatchQ[props[[i]], PropDen[Mom[_Integer | Multiparticle[__Integer]], _]],
        valid = False; Break[]];
      legs = Replace[props[[i, 1, 1]], {j_Integer :> {j}, Multiparticle[ii__Integer] :> {ii}}];
      If[!DuplicateFreeQ[legs], valid = False; Break[]];
      masses = massValue[rules, #] & /@ legs;
      If[AnyTrue[masses, MissingQ], valid = False; Break[]];
      value = Total[masses^2] + 2 Total[orderedDot @@ # & /@ Subsets[legs, {2}]] - props[[i, 2]]^2;
      AppendTo[replacements, tokens[[i]] -> value]; usedLegs = Union[usedLegs, legs],
      {i, Length[props]}];
    If[valid && replacements =!= {},
      insert = canonicalChains[((num /. replacements)/den) /. Thread[tokens -> props]];
      If[!samePoly[subtract, insert], AppendTo[moves, <|"Name" -> "MomentumSquare",
        "Identity" -> "PropagatorNumerator", "ScrambleEligible" -> False, "Subtract" -> subtract,
        "Insert" -> insert, "Legs" -> usedLegs|>]]],
    {keys, Values[buckets]}];
  moves
];


ComplicateAmplitude[pair:{_List, _}, nSteps_Integer, seed_Integer] :=
  ComplicateAmplitude[pair, nSteps, RandomSeed -> seed];

samePoly[a_, b_] := expandAmplitude[a - b] === 0;



(* ::Subsection::Closed:: *)
(*Kinematic Identities*)

kinematicMoves[expr_, massRules_List] := Module[
  {moves = {}, groups, keys, exts, buckets, r, g, n, momentumPlaceholder = Unique["conservedMomentum"]},
  groups = groupsOf[expr];
  keys = Keys[groups];
  exts = externalIndices[massRules];
  Do[
    Module[{legs = internalPairs[massRules][[r, 1]], m = internalPairs[massRules][[r, 2]],
        ma, mb, shell, dot, sk, coef, rest, subtract, insert, q},
      ma = massValue[massRules, legs[[1]]];
      mb = massValue[massRules, legs[[2]]];
      If[ma =!= Missing["Mass"] && mb =!= Missing["Mass"],
        shell = shellOf[m, ma, mb];
        dot = orderedDot @@ legs;
        Do[
          sk = keys[[g]]; coef = groups[sk];
          If[AnyTrue[factorsOf[sk], # === dot ||
              MatchQ[#, Power[base_, power_Integer?Positive] /; base === dot] &],
            rest = sk/dot;
            subtract = expandAmplitude[coef*sk];
            insert = expandAmplitude[coef*shell*rest];
            If[! samePoly[subtract, insert],
              AppendTo[moves, <|"Name" -> "OnShell", "Subtract" -> subtract, "Insert" -> insert,
                "Legs" -> Sort[legs]|>]
            ]
          ];
          (* Expand a scalar coefficient back into this diagram's on-shell dot product.
             PropDen remains a structural factor and is never rewritten. *)
          q = shellQuotient[coef, shell];
          If[q =!= None && !samePoly[q, 0],
            subtract = expandAmplitude[coef*sk];
            insert = expandAmplitude[q*dot*sk];
            AppendTo[moves, <|"Name" -> "OnShell", "Subtract" -> subtract,
              "Insert" -> insert, "Legs" -> Sort[legs]|>]
          ],
          {g, Length[keys]}
        ]
      ]
    ],
    {r, Length[internalPairs[massRules]]}
  ];
  buckets = <||>;
  Do[
    Module[{sk = keys[[g]], slots, offset, which, template},
      (* Extract one positive chain or trace factor before replacing a slot.
         Any remaining copies stay in the template, so a square itself is
         never treated as linear. Inverse factors are not selected. *)
      Do[
        If[!validChainQ[factor] && !validTraceQ[factor], Continue[]];
        slots = momentumSlots[factor]; offset = momentumSlotOffset[factor];
        Do[
          If[!MatchQ[slots[[n]], Mom[_Integer]], Continue[]];
          which = slots[[n, 1]];
          If[!IntegerQ[which] || !MemberQ[exts, which], Continue[]];
          template = sk/factor*ReplacePart[factor, n + offset -> momentumPlaceholder];
          buckets[template] = Append[Lookup[buckets, template, {}], {which, groups[sk], sk}],
          {n, Length[slots]}],
        {factor, DeleteDuplicates[Join[chainsIn[sk], tracesIn[sk]]]}]
    ],
    {g, Length[keys]}
  ];
  KeyValueMap[
    Function[{template, entries},
      Module[{byMom, present, missing, alpha, skJ, subtract, insert},
        byMom = <||>;
        Do[
          byMom[entries[[n, 1]]] = {entries[[n, 2]], entries[[n, 3]]},
          {n, Length[entries]}
        ];
        present = Keys[byMom];
        missing = Complement[exts, present];
        If[Length[missing] === 1 && Length[present] === Length[exts] - 1 && Length[exts] >= 2,
          alpha = byMom[present[[1]]][[1]];
          If[AllTrue[present, samePoly[byMom[#][[1]], alpha] &],
            skJ = template /. momentumPlaceholder -> Mom[missing[[1]]];
            subtract = expandAmplitude[Total[alpha*Table[byMom[present[[n]]][[2]], {n, Length[present]}]]];
            insert = expandAmplitude[-alpha*skJ];
            If[! samePoly[subtract, insert],
              AppendTo[moves, <|"Name" -> "MomentumConservation", "Subtract" -> subtract,
                "Insert" -> insert, "Momentum" -> missing[[1]]|>]
            ]
          ]
        ]
      ]
    ],
    buckets
  ];
  Do[AppendTo[moves, sq], {sq, momentumSquareMoves[expr, massRules]}];
  moves = Join[moves, scalarMomentumConservationMoves[expr, massRules]];
  DeleteDuplicatesBy[moves, {#["Name"], expandAmplitude[#["Subtract"]], expandAmplitude[#["Insert"]]} &]
];

(* ::Subsection::Closed:: *)
(*Diagram-Local Identity Settings*)

$onShellChannels = Automatic;
$useMomentumConservation = True;

normalizeOnShellChannels[Automatic] := Automatic;
normalizeOnShellChannels[channels_List] := Sort /@ channels;

Options[SpinorIdentityCandidates] = {"OnShellChannels" -> Automatic, "MomentumConservation" -> True};
Options[ComplicateAmplitude] = Join[{RandomSeed -> Automatic, "RecordSteps" -> False}, Options[SpinorIdentityCandidates]];
Options[ApplySpinorIdentity] = Options[SpinorIdentityCandidates];

(* ::Subsection::Closed:: *)
(*Legal Identity Edits*)

stopMove[] := <|"Name" -> "Stop", "Subtract" -> 0, "Insert" -> 0|>;

(* Relate both endpoints to their own momenta without changing the other
   endpoint's massive/massless kind or little-group indices. *)
flipSpinor[s_Spinor] := s /. {"Angle" -> "Square", "Square" -> "Angle"};
chainMoves[expr_, massRules_] := Module[{moves = {}, mons, chains, c, xs, ps, m, rhs, others, t, k, side, j},
  mons = monomialsOf[expr];
  Do[
    chains = Select[chainsIn[mons[[t]]], validChainQ];
    Do[
      c = chains[[k]]; xs = List @@ c; ps = Drop[Rest[xs], -1];
      Do[
        m = massValue[massRules, spinorLeg[xs[[side]]]];
        If[!MissingQ[m] && m =!= 0,
          rhs = If[side === 1,
            (SpinorChain @@ Join[{flipSpinor[First[xs]], Mom[spinorLeg[First[xs]]]}, ps, {Last[xs]}])/m,
            -(SpinorChain @@ Join[{First[xs]}, ps, {Mom[spinorLeg[Last[xs]]], flipSpinor[Last[xs]]}])/m];
          AppendTo[moves, <|"Name" -> "Mass", "Subtract" -> mons[[t]],
            "Insert" -> Expand[mons[[t]]/c*rhs], "Site" -> {t, k, side}, "Direction" -> "Expand"|>]
        ];
        If[ps === {}, Continue[]];
        If[If[side === 1, First[ps], Last[ps]] === Mom[spinorLeg[xs[[side]]]],
          m = massValue[massRules, spinorLeg[xs[[side]]]];
          If[!MissingQ[m],
            rhs = If[side === 1,
              m*(SpinorChain @@ Join[{flipSpinor[First[xs]]}, Rest[ps], {Last[xs]}]),
              -m*(SpinorChain @@ Join[{First[xs]}, Most[ps], {flipSpinor[Last[xs]]}])];
            AppendTo[moves, <|"Name" -> "Mass", "Subtract" -> mons[[t]],
              "Insert" -> Expand[mons[[t]]/c*rhs], "Site" -> {t, k, side}, "Direction" -> "Contract"|>]
          ]
        ], {side, {1, -1}}
      ];
      If[TrueQ[$useMomentumConservation],
        Do[
          If[!MemberQ[externalIndices[massRules], ps[[j, 1]]], Continue[]];
          others = DeleteCases[externalIndices[massRules], ps[[j, 1]]];
          If[others === {}, Continue[]];
          rhs = -Total[ReplacePart[c, j + 1 -> Mom[#]] & /@ others];
          AppendTo[moves, <|"Name" -> "MomentumConservation", "Subtract" -> mons[[t]],
            "Insert" -> Expand[mons[[t]]/c*rhs], "Site" -> {t, k, j}|>],
          {j, Length[ps]}
        ]
      ], {k, Length[chains]}
    ], {t, Length[mons]}
  ];
  moves
];

(* Interior Clifford identities preserve both endpoint spinors and their indices.
   Reverse square insertions are limited to the start of a chain, one per
   massive external leg, rather than every possible insertion site. *)
interiorChainMoves[expr_, rules_] := Module[
  {moves = {}, mons = monomialsOf[expr], chains, c, xs, ps, short, rhs, square, mass, leg, swapped},
  Do[
    chains = DeleteDuplicates[Select[chainsIn[mons[[t]]], validChainQ]];
    Do[
      c = chains[[k]]; xs = List @@ c; ps = Drop[Rest[xs], -1];
      Do[
        short = SpinorChain @@ Join[Take[xs, j], Drop[xs, j + 2]];
        If[ps[[j]] === ps[[j + 1]],
          leg = ps[[j, 1]];
          mass = momentumMass[rules, leg];
          square = If[MissingQ[mass] || mass === None, Mom[leg]^2, mass^2];
          rhs = square short;
          AppendTo[moves, <|"Name" -> "ChainSquare", "Subtract" -> mons[[t]],
            "Insert" -> Expand[mons[[t]]/c*rhs], "Site" -> {t, k, j}, "Direction" -> "Contract"|>],
          swapped = ReplacePart[c, {j + 1 -> ps[[j + 1]], j + 2 -> ps[[j]]}];
          rhs = 2 (orderedDot[ps[[j, 1]], ps[[j + 1, 1]]] /. rules) short - swapped;
          AppendTo[moves, <|"Name" -> "Anticommutation", "Subtract" -> mons[[t]],
            "Insert" -> Expand[mons[[t]]/c*rhs], "Site" -> {t, k, j}|>]
        ], {j, Length[ps] - 1}];
      (* Do not nest inverse square insertions in a chain already containing one. *)
      If[AnyTrue[Partition[ps, 2, 1], #[[1]] === #[[2]] &], Continue[]];
      Do[
        mass = massValue[rules, leg];
        If[MissingQ[mass] || TrueQ[mass == 0], Continue[]];
        rhs = (SpinorChain @@ Join[Take[xs, 1], {Mom[leg], Mom[leg]}, Rest[xs]])/mass^2;
        AppendTo[moves, <|"Name" -> "ChainSquare", "Subtract" -> mons[[t]],
          "Insert" -> Expand[mons[[t]]/c*rhs], "Site" -> {t, k, leg}, "Direction" -> "Expand"|>],
        {leg, externalIndices[rules]}],
      {k, Length[chains]}], {t, Length[mons]}];
  moves
];

(* A composite momentum is a sum, independently of momentum conservation or
   on-shell assumptions. Binary collection supplies reverse identity edits. *)
momentumMembers[i_Integer] := {i};
momentumMembers[Multiparticle[ii__Integer]] := With[{members = {ii}},
  If[Length[members] >= 2 && DuplicateFreeQ[members], Sort[members], {}]];
momentumMembers[_] := {};
momentumLabel[{i_}] := i;
momentumLabel[ii_List] := Multiparticle @@ Sort[ii];

momentumSumMoves[expr_, rules_] := Module[
  {moves = {}, mons = monomialsOf[expr], exts = externalIndices[rules], buckets = <||>,
    hole = Unique["momentumSlot"], chains, c, ps, offset, members, rhs, pair, split, template, coefficient},
  Do[
    chains = Join[DeleteDuplicates[Select[chainsIn[mons[[t]]], validChainQ]],
      DeleteDuplicates[Select[tracesIn[mons[[t]]], validTraceQ]]];
    Do[
      c = chains[[k]]; ps = momentumSlots[c]; offset = momentumSlotOffset[c];
      Do[
        members = momentumMembers[ps[[j, 1]]];
        If[members === {}, Continue[]];
        If[Length[members] >= 2,
          rhs = ReplacePart[c, j + offset -> Mom[First[members]]] +
            ReplacePart[c, j + offset -> Mom[momentumLabel[Rest[members]]]];
          AppendTo[moves, Join[<|"Name" -> "MomentumSum", "Direction" -> "Expand",
            "Subtract" -> mons[[t]], "Insert" -> canonicalChains[mons[[t]]/c*rhs],
            "Site" -> {t, k, j}, "Legs" -> {ps[[j, 1]]}, "Identity" -> "Split"|>, traceMetadata[c]]],
          If[MemberQ[exts, First[members]],
            Do[
              pair = momentumLabel[{First[members], other}];
              rhs = ReplacePart[c, j + offset -> Mom[pair]] - ReplacePart[c, j + offset -> Mom[other]];
              AppendTo[moves, Join[<|"Name" -> "MomentumSum", "Direction" -> "Expand",
                "Subtract" -> mons[[t]], "Insert" -> canonicalChains[mons[[t]]/c*rhs],
                "Site" -> {t, k, j}, "Legs" -> {pair, other}, "Identity" -> "Introduce"|>, traceMetadata[c]]],
              {other, Complement[exts, members]}]]];
        split = splitMon[mons[[t]]];
        template = splitMon[canonicalChains[split[[2]]/c*ReplacePart[c, j + offset -> Mom[hole]]]];
        coefficient = split[[1]] template[[1]];
        buckets[template[[2]]] = Append[Lookup[buckets, template[[2]], {}],
          {ps[[j, 1]], coefficient, mons[[t]]}],
        {j, Length[ps]}], {k, Length[chains]}], {t, Length[mons]}];
  KeyValueMap[Function[{context, entries}, Module[
    {byMomentum, labels, a, b, ma, mb, ca, cb, target, signs, scales, subtract},
    byMomentum = GroupBy[DeleteDuplicates[entries], First]; labels = Keys[byMomentum];
    Do[
      a = labels[[i]]; b = labels[[j]];
      ma = momentumMembers[a]; mb = momentumMembers[b];
      ca = Total[byMomentum[a][[All, 2]]]; cb = Total[byMomentum[b][[All, 2]]];
      target = None;
      If[Intersection[ma, mb] === {}, target = momentumLabel[Join[ma, mb]]; signs = {1, 1}];
      If[Complement[mb, ma] === {} && Length[ma] > Length[mb],
        target = momentumLabel[Complement[ma, mb]]; signs = {1, -1}];
      If[Complement[ma, mb] === {} && Length[mb] > Length[ma],
        target = momentumLabel[Complement[mb, ma]]; signs = {-1, 1}];
      If[target =!= None,
        (* Collect a shared portion even when another term has changed one coefficient. *)
        scales = DeleteDuplicates[{signs[[1]] ca, signs[[2]] cb}];
        Do[
          subtract = canonicalChains[scale*((signs[[1]] context /. Mom[hole] -> Mom[a]) +
            (signs[[2]] context /. Mom[hole] -> Mom[b]))];
          AppendTo[moves, <|"Name" -> "MomentumSum", "Direction" -> "Contract",
            "Subtract" -> subtract, "Insert" -> canonicalChains[scale*(context /. Mom[hole] -> Mom[target])],
            "Legs" -> {a, b}, "Momentum" -> target, "Identity" -> "Collect"|>], {scale, scales}]],
      {i, Length[labels]}, {j, i + 1, Length[labels]}]]], buckets];
  moves
];

(* Trace moves use the same Clifford algebra, with no external spinor endpoints. *)
traceMoves[expr_, rules_] := Module[
  {moves = {}, mons = monomialsOf[expr], traces, trace, ps, short, swapped, rhs, mass, square, leg, dots},
  Do[
    traces = DeleteDuplicates[Select[tracesIn[mons[[t]]], validTraceQ]];
    Do[
      trace = traces[[k]]; ps = List @@ trace;
      Do[
        short = SpinorTrace @@ Join[Take[ps, j - 1], Drop[ps, j + 1]];
        If[ps[[j]] === ps[[j + 1]],
          mass = momentumMass[rules, ps[[j, 1]]];
          square = If[MissingQ[mass] || mass === None, ps[[j]]^2, mass^2];
          rhs = square short;
          AppendTo[moves, <|"Name" -> "ChainSquare", "Identity" -> "TraceSquare", "Direction" -> "Contract",
            "Subtract" -> mons[[t]], "Insert" -> canonicalChains[mons[[t]]/trace*rhs],
            "Trace" -> trace, "Site" -> {t, k, j}|>],
          swapped = ReplacePart[trace, {j -> ps[[j + 1]], j + 1 -> ps[[j]]}];
          rhs = 2 (orderedDot[ps[[j, 1]], ps[[j + 1, 1]]] /. rules) short - swapped;
          AppendTo[moves, <|"Name" -> "Anticommutation", "Identity" -> "TraceAnticommutation",
            "Subtract" -> mons[[t]], "Insert" -> canonicalChains[mons[[t]]/trace*rhs],
            "Trace" -> trace, "Site" -> {t, k, j}|>]
        ], {j, Length[ps] - 1}];
      If[Length[ps] > 2 && First[ps] === Last[ps],
        mass = momentumMass[rules, First[ps][[1]]];
        square = If[MissingQ[mass] || mass === None, First[ps]^2, mass^2];
        (* Crossing the trace boundary flips chirality; one rotation restores it. *)
        rhs = square (SpinorTrace @@ RotateLeft[Most[Rest[ps]]]);
        AppendTo[moves, <|"Name" -> "ChainSquare", "Identity" -> "TraceCyclicSquare", "Direction" -> "Contract",
          "Subtract" -> mons[[t]], "Insert" -> canonicalChains[mons[[t]]/trace*rhs],
          "Trace" -> trace, "Site" -> {t, k, 0}|>]];
      If[Length[ps] === 2 && First[ps] =!= Last[ps],
        AppendTo[moves, <|"Name" -> "MomentumSquare", "Identity" -> "TracePair", "Direction" -> "Contract",
          "Subtract" -> mons[[t]], "Insert" -> canonicalChains[mons[[t]]/trace*2*((orderedDot @@ (First /@ ps)) /. rules)],
          "Trace" -> trace, "Site" -> {t, k, 1}, "Legs" -> (First /@ ps)|>]];
      If[TrueQ[$useMomentumConservation],
        Do[
          leg = ps[[j, 1]];
          If[!MemberQ[externalIndices[rules], leg], Continue[]];
          rhs = -Total[ReplacePart[trace, j -> Mom[#]] & /@ Complement[externalIndices[rules], {leg}]];
          AppendTo[moves, <|"Name" -> "MomentumConservation", "Identity" -> "TraceConservation",
            "Subtract" -> mons[[t]], "Insert" -> canonicalChains[mons[[t]]/trace*rhs],
            "Trace" -> trace, "Site" -> {t, k, j}, "Momentum" -> leg|>], {j, Length[ps]}]];
      If[!AnyTrue[Partition[ps, 2, 1], #[[1]] === #[[2]] &],
        Do[
          mass = massValue[rules, leg];
          If[MissingQ[mass] || TrueQ[mass == 0], Continue[]];
          rhs = (SpinorTrace @@ Join[{Mom[leg], Mom[leg]}, ps])/mass^2;
          AppendTo[moves, <|"Name" -> "ChainSquare", "Identity" -> "TraceSquare", "Direction" -> "Expand",
            "Subtract" -> mons[[t]], "Insert" -> canonicalChains[mons[[t]]/trace*rhs],
            "Trace" -> trace, "Site" -> {t, k, leg}|>], {leg, externalIndices[rules]}]],
      {k, Length[traces]}];
    dots = DeleteDuplicates[Cases[factorsOf[mons[[t]]],
      dot_MomProd :> dot, {1}] ~Join~ Cases[factorsOf[mons[[t]]],
        Power[dot_MomProd, _Integer?Positive] :> dot, {1}]];
    Do[
      rhs = (SpinorTrace @@ (Mom /@ (List @@ dot)))/2;
      AppendTo[moves, <|"Name" -> "MomentumSquare", "Identity" -> "TracePair", "Direction" -> "Expand",
        "Subtract" -> mons[[t]], "Insert" -> canonicalChains[mons[[t]]/dot*rhs], "Legs" -> (List @@ dot)|>],
      {dot, dots}], {t, Length[mons]}];
  moves
];

applyCandidate[expr_, cand_Association] :=
  If[cand["Name"] === "Stop", expr, canonicalChains[expr - cand["Subtract"] + cand["Insert"]]];

legalMoves[expr_, massRules_List] := Module[{legacy, moves, normalizationDelta},
  legacy = Select[kinematicMoves[expr, massRules],
    MemberQ[If[TrueQ[$useMomentumConservation],
      {"MomentumConservation", "MomentumSquare", "OnShell"},
      {"MomentumSquare", "OnShell"}], #["Name"]] &];
  moves = Join[chainMoves[expr, massRules], interiorChainMoves[expr, massRules],
    momentumSumMoves[expr, massRules], traceMoves[expr, massRules],
    helicityInvariantMoves[expr, massRules], propagatorNumeratorMoves[expr, massRules],
    SchoutenCandidates[expr], legacy];
  moves = DeleteDuplicatesBy[moves, {#["Name"], #["Subtract"], #["Insert"]} &];
  (* Canonicalization is linear, so unchanged edits need only their local delta.
     Include the input correction for callers supplying noncanonical states. *)
  normalizationDelta = canonicalChains[expr] - expr;
  moves = Select[moves,
    !samePoly[canonicalChains[#["Insert"] - #["Subtract"]] + normalizationDelta, 0] &];
  Append[moves, stopMove[]]
];

SpinorIdentityCandidates[{massRules_List, amplitude_}, OptionsPattern[]] :=
  Block[{$onShellChannels = normalizeOnShellChannels[OptionValue["OnShellChannels"]],
      $useMomentumConservation = OptionValue["MomentumConservation"]},
    Select[legalMoves[canonicalAmplitude[amplitude, massRules], massRules], #["Name"] =!= "Stop" &]
  ];

ApplySpinorIdentity[pair:{massRules_List, amplitude_}, edit_Association, OptionsPattern[]] := Module[
  {state, candidates, key},
  key = Lookup[edit, {"Name", "Subtract", "Insert"}, Missing["InvalidEdit"]];
  If[!FreeQ[key, _Missing] || First[key] === "Stop",
    Message[ApplySpinorIdentity::edit]; Return[$Failed]];
  state = canonicalAmplitude[amplitude, massRules];
  candidates = SpinorIdentityCandidates[pair,
    "OnShellChannels" -> OptionValue["OnShellChannels"],
    "MomentumConservation" -> OptionValue["MomentumConservation"]];
  If[!AnyTrue[candidates, Lookup[#, {"Name", "Subtract", "Insert"}] === key &],
    Message[ApplySpinorIdentity::edit]; Return[$Failed]];
  applyCandidate[state, edit]
];
ApplySpinorIdentity[___] := (Message[ApplySpinorIdentity::args]; $Failed);

(* ::Subsection::Closed:: *)
(*Typed Particle Relabeling*)

relabelLegs[expr_, ids_Association] := Module[{idx},
  idx[i_Integer] := Lookup[ids, i, i];
  idx[Multiparticle[ii__]] := Multiparticle @@ (idx /@ {ii});
  idx[x_] := x;
  expr /. {
    Spinor[k_, t: ("Angle" | "Square"), i_, rest___] :> Spinor[k, t, idx[i], rest],
    Spinor[k_, ud_, t: ("Angle" | "Square"), i_, rest___] :> Spinor[k, ud, t, idx[i], rest],
    Mom[i_] :> Mom[idx[i]], Mass[i_] :> Mass[idx[i]],
    MomProd[i_, j_] :> MomProd[idx[i], idx[j]],
    Mandelstahm[i_, j_] :> Mandelstahm[idx[i], idx[j]]
  }
];

(* ::Subsection::Closed:: *)
(*Complexity and Random Identity Checks*)

SpinorExpressionComplexity[expr_] := expressionComplexity[expr];
complexityMomentumMultiplicity[Mom[label_]] := Max[1, Length[momentumMembers[label]]];
complexityMomentumMultiplicity[_] := 1;
momentumBranchCount[slots_List] := Times @@ (complexityMomentumMultiplicity /@ slots);

(* Count the branches hidden in composite slots without constructing them.
   A product's slots multiply branch counts, including repeated chain factors. *)
monomialComplexity[mon_] := Module[{slots, branches},
  slots = Flatten[momentumSlots /@ Join[chainsIn[mon], tracesIn[mon]], 1];
  branches = momentumBranchCount[slots];
  branches*(1 + 4*Length[slots] + 4*Count[{mon}, _MomProd, Infinity] +
    2*Count[{mon}, Power[Mom[Multiparticle[__]], 2], Infinity])
];
expressionComplexity[expr_] := withProtectedPropagators[expr,
  Function[body, Total[monomialComplexity /@ monomialsOf[body]]]];
expressionTermCounts[expr_] := withProtectedPropagators[expr, Function[body,
  With[{mons = monomialsOf[body]}, <|"Terms" -> Length[mons],
    "ExpandedTerms" -> Total[momentumBranchCount[
      Flatten[momentumSlots /@ Join[chainsIn[#], tracesIn[#]], 1]] & /@ mons]|>]]];

(* Random scrambles use only reversible symbolic edits. *)
scramblePair[{rules_List, amp_}, n_, seed_, record_] := Module[
  {original = canonicalAmplitude[amp, rules], expr, moves, pick, next, history = {}},
  expr = original;
  BlockRandom[
    If[IntegerQ[seed], SeedRandom[seed]];
    Do[
      moves = Select[legalMoves[expr, rules],
        #["Name"] =!= "Stop" && TrueQ[Lookup[#, "ScrambleEligible", True]] &];
      If[moves === {}, Break[]];
      pick = RandomChoice[moves]; next = applyCandidate[expr, pick];
      AppendTo[history, <|"Before" -> expr, "After" -> next, "Candidate" -> pick|>];
      expr = next, {n}
    ]
  ];
  Join[<|"MassRules" -> rules, "Original" -> original, "Expression" -> expr, "Steps" -> Length[history]|>,
    If[TrueQ[record], <|"Trajectory" -> history|>, <||>]]
];

ComplicateAmplitude[pair:{_List, _}, nSteps_Integer:5, OptionsPattern[]] :=
  Block[{$onShellChannels = normalizeOnShellChannels[OptionValue["OnShellChannels"]],
      $useMomentumConservation = OptionValue["MomentumConservation"]},
    scramblePair[pair, Max[0, nSteps], OptionValue[RandomSeed], OptionValue["RecordSteps"]]
  ];

End[];
