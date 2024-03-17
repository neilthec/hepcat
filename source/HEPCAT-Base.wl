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
nice[xFactor[a_,b_]]:=Subscript["x",ToString[a]<>","<>ToString[b]]
Format[xTildeFactor[a__]]:=nice[xTildeFactor[a]]
nice[xTildeFactor[Multiparticle[a_,c_],b_]]:=Subscript["\!\(\*OverscriptBox[\(x\), \(~\)]\)",ToString[a]<>ToString[c]<>","<>ToString[b]]
nice[xTildeFactor[a_,Multiparticle[b_,c_]]]:=Subscript["\!\(\*OverscriptBox[\(x\), \(~\)]\)",ToString[a]<>","<>ToString[b]<>ToString[c]]
nice[xTildeFactor[a_,b_]]:=Subscript["\!\(\*OverscriptBox[\(x\), \(~\)]\)",ToString[a]<>","<>ToString[b]]
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
Format[MomMagnitude[a_]]:=nice[MomMagnitude[a]]
nice[MomMagnitude[a_]]:=Subscript[p, a]
Format[Theta[a_]]:=nice[Theta[a]]
nice[Theta[a_]]:=Subscript[\[Theta],a]
Format[Phi[a_]]:=nice[Phi[a]]
nice[Phi[a_]]:=Subscript[\[Phi],a]
Format[MomProd[a__]]:=nice[MomProd[a]]
nice[MomProd[a_,b_]]:=Subscript[p, a] . Subscript[p, b]
nice[MomHat[a_]]:=Subscript[OverHat[p], a]
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
nice[xk]:=Subscript["k","x"]
nice[xq]:=Subscript["q","x"]


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
PlainText[Multiparticle[a_,b_]]:="P_{"<>ToString[a]<>ToString[b]<>"} "
PlainText[Multiparticle[a_,b_,c_]]:="P_{"<>ToString[a]<>ToString[b]<>ToString[c]<>"} "
PlainText[Particle[a_]]:=ToString[a]
PlainText[Spinor["Helicity","Angle",a_]]:=PlainText[a]<>">"
lPlainText[Spinor["Helicity","Angle",a_]]:="<"<>PlainText[a]
PlainText[Spinor["Helicity","Square",a_]]:=PlainText[a]<>"]"
lPlainText[Spinor["Helicity","Square",a_]]:="["<>PlainText[a]
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
PlainText[SpinorChain[a_,b__,c_]]:=lPlainText[a]<>((PlainText[#]&/@List[b])/.List[d__]:>d)<>PlainText[c]
PlainText[SpinorTrace[a__]]:=" Tr("<>((PlainText[#]&/@List[a])/.List[b__]:>b)<>")"
PlainText[xFactor[a_,b_]]:="x_{"<>ToString[a]<>ToString[b]<>"} "
PlainText[xTildeFactor[a_,b_]]:="tx_{"<>ToString[a]<>ToString[b]<>"} "
PlainText[Mass[Multiparticle[a__]]]:="M_{"<>ToString[a]<>"} "
PlainText[Mass[a_]]:="M_{"<>ToString[a]<>"} "
PlainText[Mom[Multiparticle[a_,b_]]]:="p_{"<>ToString[a]<>ToString[b]<>"} "
PlainText[Mom[Multiparticle[a_,b_,c_]]]:="p_{"<>ToString[a]<>ToString[b]<>ToString[c]<>"} "
PlainText[Mom[a_]]:=ToString[a]
PlainText[MomProd[a_,b_]]:="p_{"<>ToString[a]<>"}.p_{"<>ToString[b]<>"} "
PlainText[PropDen[p_,m_]]:="("<>PlainText[p]<>"^2-"<>PlainText[m]<>"^2 "<>")"
PlainText[PropDen[p_,m_,w_]]:="("<>PlainText[p]<>"^2-"<>PlainText[m]<>"^2+i"<>PlainText[m]<>PlainText[w]<>")"
PlainText[Coupling[a_]]:="g_{"<>ToString[a]<>"} "
PlainText[Sqrt[a__]]:="sqrt("<>PlainText[a]<>")"
(*PlainText[a_+b_]:="("<>PlainText[a]<>"+"<>PlainText[b]<>")"*)
PlainText[Plus[a__]]:=a/.Plus->PlainTextPlus;
PlainTextPlus[a__]:="("<>StringJoin[Riffle[PlainText[#]&/@List[a],"+"]]<>")";
PlainText[a_*b_]:=PlainText[a]<>PlainText[b]
PlainText[Rational[a_,b_]]:="("<>PlainText[a]<>")/("<>PlainText[b]<>") "
PlainText[a_/b_]:="("<>PlainText[Numerator[a/b]]<>")/("<>PlainText[Denominator[a/b]]<>") "
PlainText[a_^b_]:=PlainText[a]<>"^"<>ToString[b]<>" "
PlainText[a_/;NumberQ[a]]:=ToString[a]
PlainText[Mandelstahm[i_,j_]]:="s"<>ToString[i]<>ToString[j]


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


(* ::Subsection:: *)
(*Simplification*)


(* ::Subsubsection::Closed:: *)
(*ReduceSpinContractions*)


(* ::Input::Initialization:: *)
ReverseSequence[a__]:=Reverse[List[a]]/.List[b__]:>b


(* ::Input::Initialization:: *)
ReduceSpinContractions[exp_]:=Module[{res=exp,oldRes,replacementRules,jjTimes=1},
replacementRules={
(*SpinorChain[...Spinor...]SpinorChain[...Spinor...]*)
SpinorChain[a__,Spinor["Spin","Upper","Angle",b_,c_]]SpinorChain[Spinor["Spin","Lower","Angle",b_,c_],e__]:>Mass[b]SpinorChain[a,e],
SpinorChain[a__,Spinor["Spin","Upper","Square",b_,c_]]SpinorChain[Spinor["Spin","Lower","Square",b_,c_],e__]:>-Mass[b]SpinorChain[a,e],SpinorChain[a__,Spinor["Spin","Upper","Angle",b_,c_]]SpinorChain[Spinor["Spin","Lower","Square",b_,c_],e__]:>SpinorChain[a,Mom[b],e],
SpinorChain[a__,Spinor["Spin","Upper","Square",b_,c_]]SpinorChain[Spinor["Spin","Lower","Angle",b_,c_],e__]:>-SpinorChain[a,Mom[b],e],

SpinorChain[a__,Spinor["Spin","Lower","Angle",b_,c_]]SpinorChain[Spinor["Spin","Upper","Angle",b_,c_],e__]:>-Mass[b]SpinorChain[a,e],
SpinorChain[a__,Spinor["Spin","Lower","Square",b_,c_]]SpinorChain[Spinor["Spin","Upper","Square",b_,c_],e__]:>Mass[b]SpinorChain[a,e],SpinorChain[a__,Spinor["Spin","Lower","Angle",b_,c_]]SpinorChain[Spinor["Spin","Upper","Square",b_,c_],e__]:>-SpinorChain[a,Mom[b],e],
SpinorChain[a__,Spinor["Spin","Lower","Square",b_,c_]]SpinorChain[Spinor["Spin","Upper","Angle",b_,c_],e__]:>SpinorChain[a,Mom[b],e],

SpinorChain[Spinor["Spin","Upper","Angle",b_,c_],a__]SpinorChain[Spinor["Spin","Lower","Angle",b_,c_],e__]:>(-1)^Length[List[a]] Mass[b]SpinorChain[ReverseSequence[a],e],
SpinorChain[Spinor["Spin","Upper","Square",b_,c_],a__]SpinorChain[Spinor["Spin","Lower","Square",b_,c_],e__]:>-(-1)^Length[List[a]]Mass[b]SpinorChain[ReverseSequence[a],e],SpinorChain[Spinor["Spin","Upper","Angle",b_,c_],a__]SpinorChain[Spinor["Spin","Lower","Square",b_,c_],e__]:>(-1)^Length[List[a]] SpinorChain[ReverseSequence[a],Mom[b],e],
SpinorChain[Spinor["Spin","Upper","Square",b_,c_],a__]SpinorChain[Spinor["Spin","Lower","Angle",b_,c_],e__]:>-(-1)^Length[List[a]]SpinorChain[ReverseSequence[a],Mom[b],e],

SpinorChain[a__,Spinor["Spin","Upper","Angle",b_,c_]]SpinorChain[e__,Spinor["Spin","Lower","Angle",b_,c_]]:>(-1)^Length[List[e]] Mass[b]SpinorChain[a,ReverseSequence[e]],
SpinorChain[a__,Spinor["Spin","Upper","Square",b_,c_]]SpinorChain[e__,Spinor["Spin","Lower","Square",b_,c_]]:>-(-1)^Length[List[e]]Mass[b]SpinorChain[a,ReverseSequence[e]],SpinorChain[a__,Spinor["Spin","Upper","Angle",b_,c_]]SpinorChain[e__,Spinor["Spin","Lower","Square",b_,c_]]:>(-1)^Length[List[e]] SpinorChain[a,Mom[b],ReverseSequence[e]],
SpinorChain[a__,Spinor["Spin","Upper","Square",b_,c_]]SpinorChain[e__,Spinor["Spin","Lower","Angle",b_,c_]]:>-(-1)^Length[List[e]]SpinorChain[a,Mom[b],ReverseSequence[e]],

(*Helicity Amplitude with index dummy supports*)
SpinorChain[a__,Spinor["Helicity","Angle",b_,J_]]SpinorChain[Spinor["Helicity","Square",b_,J_],c__]:>SpinorChain[a,Mom[b],c],
SpinorChain[a__,Spinor["Helicity","Square",b_,J_]]SpinorChain[Spinor["Helicity","Angle",b_,J_],c__]:>SpinorChain[a,Mom[b],c],
SpinorChain[a__,Spinor["Helicity","Angle",b_,J_]]SpinorChain[Spinor["Helicity","Square",b_,J_],c__]^2:>SpinorChain[a,Mom[b],c]SpinorChain[Spinor["Helicity","Square",b],c],
SpinorChain[a__,Spinor["Helicity","Square",b_,J_]]SpinorChain[Spinor["Helicity","Angle",b_,J_],c__]^2:>SpinorChain[a,Mom[b],c]SpinorChain[Spinor["Helicity","Angle",b],c],

SpinorChain[Spinor["Helicity","Square",pN_,J_],b__]SpinorChain[Spinor["Helicity","Angle",pN_,J_],d__]:>
(-1)^Length[List[d]] SpinorChain[ReverseSequence[d],Mom[pN],b],
SpinorChain[b__,Spinor["Helicity","Square",pN_,J_]]SpinorChain[d__,Spinor["Helicity","Angle",pN_,J_]]:>(-1)^Length[List[d]] SpinorChain[b,Mom[pN],ReverseSequence[d]],
SpinorChain[Spinor["Helicity","Angle",pN_,J_],b__]SpinorChain[Spinor["Helicity","Square",pN_,J_],d__]:>(-1)^Length[List[d]] SpinorChain[ReverseSequence[d],Mom[pN],b],
SpinorChain[b__,Spinor["Helicity","Angle",pN_,J_]]SpinorChain[d__,Spinor["Helicity","Square",pN_,J_]]:>(-1)^Length[List[d]] SpinorChain[b,Mom[pN],ReverseSequence[d]],

(*Old Helicity Amplitude Rules*)
SpinorChain[a__,Spinor["Helicity","Angle",b_]]SpinorChain[Spinor["Helicity","Square",b_],c__]:>SpinorChain[a,Mom[b],c],
SpinorChain[a__,Spinor["Helicity","Square",b_]]SpinorChain[Spinor["Helicity","Angle",b_],c__]:>SpinorChain[a,Mom[b],c],
SpinorChain[a__,Spinor["Helicity","Angle",b_]]^d_ SpinorChain[Spinor["Helicity","Square",b_],c__]^d_:>SpinorChain[a,Mom[b],c]^d,
SpinorChain[a__,Spinor["Helicity","Square",b_]]^d_ SpinorChain[Spinor["Helicity","Angle",b_],c__]^d_:>SpinorChain[a,Mom[b],c]^d,
SpinorChain[a__,Spinor["Helicity","Angle",b_]]SpinorChain[Spinor["Helicity","Square",b_],c__]^2:>SpinorChain[a,Mom[b],c]SpinorChain[Spinor["Helicity","Square",b],c],
SpinorChain[a__,Spinor["Helicity","Angle",b_]]^2 SpinorChain[Spinor["Helicity","Square",b_],c__]:>SpinorChain[a,Spinor["Helicity","Angle",b]]SpinorChain[a,Mom[b],c],
SpinorChain[a__,Spinor["Helicity","Square",b_]]SpinorChain[Spinor["Helicity","Angle",b_],c__]^2:>SpinorChain[a,Mom[b],c]SpinorChain[Spinor["Helicity","Angle",b],c],
SpinorChain[a__,Spinor["Helicity","Square",b_]]^2 SpinorChain[Spinor["Helicity","Angle",b_],c__]:>SpinorChain[a,Spinor["Helicity","Square",b]]SpinorChain[a,Mom[b],c],

SpinorChain[Spinor["Helicity","Square",pN_],b__]SpinorChain[Spinor["Helicity","Angle",pN_],d__]:>
(-1)^Length[List[d]] SpinorChain[ReverseSequence[d],Mom[pN],b],
SpinorChain[b__,Spinor["Helicity","Square",pN_]]SpinorChain[d__,Spinor["Helicity","Angle",pN_]]:>(-1)^Length[List[d]] SpinorChain[b,Mom[pN],ReverseSequence[d]],
SpinorChain[Spinor["Helicity","Angle",pN_],b__]SpinorChain[Spinor["Helicity","Square",pN_],d__]:>(-1)^Length[List[d]] SpinorChain[ReverseSequence[d],Mom[pN],b],
SpinorChain[b__,Spinor["Helicity","Angle",pN_]]SpinorChain[d__,Spinor["Helicity","Square",pN_]]:>(-1)^Length[List[d]] SpinorChain[b,Mom[pN],ReverseSequence[d]],
SpinorChain[Spinor["Helicity","Square",pN_],b__]^pow_ SpinorChain[Spinor["Helicity","Angle",pN_],d__]^pow_:>
(-1)^Length[List[d]] SpinorChain[ReverseSequence[d],Mom[pN],b]^pow,
SpinorChain[b__,Spinor["Helicity","Square",pN_]]^pow_ SpinorChain[d__,Spinor["Helicity","Angle",pN_]]^pow_:>(-1)^Length[List[d]] SpinorChain[b,Mom[pN],ReverseSequence[d]]^pow,
SpinorChain[Spinor["Helicity","Angle",pN_],b__]^pow_ SpinorChain[Spinor["Helicity","Square",pN_],d__]^pow_:>(-1)^Length[List[d]] SpinorChain[ReverseSequence[d],Mom[pN],b]^pow,
SpinorChain[b__,Spinor["Helicity","Angle",pN_]]^pow_ SpinorChain[d__,Spinor["Helicity","Square",pN_]]^pow_:>(-1)^Length[List[d]] SpinorChain[b,Mom[pN],ReverseSequence[d]]^pow,

(*SpinorChain[Spinor...Spinor]\[Rule]SpinorTr[...]*)
SpinorChain[Spinor["Spin","Upper","Square",pN_,J_],a___,Spinor["Spin","Lower","Angle",pN_,J_]]:>-SpinorTrace[Mom[pN],a],
SpinorChain[Spinor["Spin","Lower","Square",pN_,J_],a___,Spinor["Spin","Upper","Angle",pN_,J_]]:>SpinorTrace[Mom[pN],a],
SpinorChain[Spinor["Spin","Upper","Angle",pN_,J_],a___,Spinor["Spin","Lower","Square",pN_,J_]]:>SpinorTrace[a,Mom[pN]],
SpinorChain[Spinor["Spin","Lower","Angle",pN_,J_],a___,Spinor["Spin","Upper","Square",pN_,J_]]:>-SpinorTrace[a,Mom[pN]],

SpinorChain[Spinor["Spin","Upper","Square",pN_,J_],a_,b__,Spinor["Spin","Lower","Square",pN_,J_]]:>Mass[pN]SpinorTrace[b,a],
SpinorChain[Spinor["Spin","Lower","Square",pN_,J_],a_,b__,Spinor["Spin","Upper","Square",pN_,J_]]:>-Mass[pN]SpinorTrace[b,a],
SpinorChain[Spinor["Spin","Upper","Square",pN_,J_],a_,Spinor["Spin","Lower","Square",pN_,J_]]:>Mass[pN]SpinorTrace[a],
SpinorChain[Spinor["Spin","Lower","Square",pN_,J_],a_,Spinor["Spin","Upper","Square",pN_,J_]]:>-Mass[pN]SpinorTrace[a],
SpinorChain[Spinor["Spin","Upper","Square",pN_,J_],Spinor["Spin","Lower","Square",pN_,J_]]:>Mass[pN]SpinorTrace[],
SpinorChain[Spinor["Spin","Lower","Square",pN_,J_],Spinor["Spin","Upper","Square",pN_,J_]]:>-Mass[pN]SpinorTrace[],
SpinorChain[Spinor["Spin","Upper","Angle",pN_,J_],a___,Spinor["Spin","Lower","Angle",pN_,J_]]:>-Mass[pN]SpinorTrace[a],
SpinorChain[Spinor["Spin","Lower","Angle",pN_,J_],a___,Spinor["Spin","Upper","Angle",pN_,J_]]:>Mass[pN]SpinorTrace[a],

SpinorChain[Spinor["Helicity","Square",pN_],a___,Spinor["Helicity","Angle",pN_]]:>SpinorTrace[Mom[pN],a],
SpinorChain[Spinor["Helicity","Angle",pN_],a___,Spinor["Helicity","Square",pN_]]:>SpinorTrace[a,Mom[pN]],

(*SpinorEps*)
SpinorChain[ls___,Spinor["Spin","Lower",as_,pN_,J_],rs___]SpinorEps["Upper",J_,K_]:>SpinorChain[ls,Spinor["Spin","Upper",as,pN,K],rs],
SpinorChain[ls___,Spinor["Spin","Lower",as_,pN_,J_],rs___]SpinorEps["Upper",K_,J_]:>-SpinorChain[ls,Spinor["Spin","Upper",as,pN,K],rs],
SpinorChain[ls___,Spinor["Spin","Upper",as_,pN_,J_],rs___]SpinorEps["Lower",J_,K_]:>SpinorChain[ls,Spinor["Spin","Lower",as,pN,K],rs],
SpinorChain[ls___,Spinor["Spin","Upper",as_,pN_,J_],rs___]SpinorEps["Lower",K_,J_]:>-SpinorChain[ls,Spinor["Spin","Lower",as,pN,K],rs]
};

While[res=!=oldRes,
(*Print[jjTimes];*)
oldRes=res;
res=Expand[res//.replacementRules];
jjTimes=jjTimes+1;
If[jjTimes>10,Print["Giving up after 10 attempts to reduce!"];Break[]];
];
res
]


(* ::Subsubsection::Closed:: *)
(*ReduceSpinorProducts*)


(* ::Input::Initialization:: *)
ReduceSpinorProducts[amp_]:=Expand[Expand[Expand[amp//.{SpinorChain[a___]:>SpinorChainReduced[a],SpinorTrace[a___]:>SpinorTraceReduced[a]}]]]//.{SpinorChainReduced[a___]:>SpinorChain[a],SpinorTraceReduced[a___]:>SpinorTrace[a]}


(* ::Subsubsection::Closed:: *)
(*Ordering and Splitting*)


(* ::Input::Initialization:: *)
(*[11] = <11> = 0*)
SpinorChainReduced[Spinor[sh_,sa_,p_],Spinor[sh_,sa_,p_]]:=0;


(* ::Input::Initialization:: *)
(*[21]=-[12]*)
SpinorChainReduced[Spinor[sh1_,"Angle",p1_],Mom[p2_],Spinor[sh3_,"Square",p3_]]:=SpinorChainReduced[Spinor[sh3,"Square",p3],Mom[p2],Spinor[sh1,"Angle",p1]];
SpinorChainReduced[Spinor[sh1_,"Angle",p1_],Mom[p2_],Mom[p4_],Mom[p5_],Spinor[sh3_,"Square",p3_]]:=SpinorChainReduced[Spinor[sh3,"Square",p3],Mom[p5],Mom[p4],Mom[p2],Spinor[sh1,"Angle",p1]];
(*<p1|p2|p3] = [p3|p2|p1>*)
SpinorChainReduced[Spinor[sh1_,tp1_,p1_],Spinor[sh2_,tp2_,p2_]]/;p2<p1:=-SpinorChainReduced[Spinor[sh2,tp2,p2],Spinor[sh1,tp1,p1]];
(*[2|p5p6|1] = -[1|p6p5|2]*)
SpinorChainReduced[Spinor[sh1_,tp_,p1_],Mom[p5_],Mom[p6_],Spinor[sh2_,tp_,p2_]]/;p2<p1:=-SpinorChainReduced[Spinor[sh2,tp,p2],Mom[p6],Mom[p5],Spinor[sh1,tp,p1]];
(*[1|p6p5|2] = 2p5.p6[12] -[1|p5p6|2]*)
SpinorChainReduced[Spinor[sh1_,tp_,p1_],Mom[p5_],Mom[p6_],Spinor[sh2_,tp_,p2_]]/;(p6<p5&&p6!=p2&&p5!=p1):=2MomProd[p5,p6]SpinorChainReduced[Spinor[sh1,tp,p1],Spinor[sh2,tp,p2]]-SpinorChainReduced[Spinor[sh1,tp,p1],Mom[p6],Mom[p5],Spinor[sh2,tp,p2]];
(*[\[Zeta]2|p1p3|2] = - [2|p3p1|\[Zeta]2]*)
SpinorChainReduced[Spinor["Zeta",sa_,p2_],Mom[p1_],Mom[p3_],Spinor["Helicity",sa_,p2_]]:=-SpinorChainReduced[Spinor["Helicity",sa,p2],Mom[p3],Mom[p1],Spinor["Zeta",sa,p2]];


(* ::Input::Initialization:: *)
(*<p1|...(a+b)...|p2> = <p1|...a...|p2>+<p1|...b...|p2>*)
SpinorChainReduced[a___,-b_,d___]:=-SpinorChainReduced[a,b,d];
SpinorChainReduced[a___,Plus[b_,c__],d___]:=SpinorChainReduced[a,b,d]+SpinorChainReduced[a,Plus[c],d];
SpinorChainReduced[a___,1,b___]:=SpinorChainReduced[a,b];

SpinorChainReduced[a__,Mom[Multiparticle[b_,c_]],d__]:=SpinorChainReduced[a,Mom[b],d]+SpinorChainReduced[a,Mom[c],d];
SpinorChainReduced[a__,Mom[Multiparticle[b_,c__]],d__]:=SpinorChainReduced[a,Mom[b],d]+SpinorChainReduced[a,Mom[Multiparticle[c]],d];

(*Test*)
(*SpinorChainReduced[Spinor["Spin","Angle",1],Mom[2]+Mom[3]-Sum[Mom[ii],{ii,1,5}],Spinor["Spin","Square",4]]*)


(* ::Subsubsection::Closed:: *)
(*Spinor - Mass Identities (...p|p> = m ...|p])*)


(* ::Input::Initialization:: *)
(*...p|p> = m ...|p]*)
SpinorChainReduced[a__,Mom[b_],Spinor["Helicity",as_,b_]]:=0;
SpinorChainReduced[Spinor["Helicity",as_,a_],Mom[a_],b__]:=0;
SpinorChainReduced[Spinor["Helicity",as_,a_],Spinor["Helicity",as_,a_]]:=0;

SpinorChainReduced[a__,Mom[b_],Spinor["Spin","Angle",b_]]:=-Mass[b]SpinorChainReduced[a,Spinor["Spin","Square",b]];
SpinorChainReduced[a__,Mom[b_],Spinor["Spin",ul_,"Angle",b_,J_]]:=-Mass[b]SpinorChainReduced[a,Spinor["Spin",ul,"Square",b,J]];
SpinorChainReduced[a__,Mom[b_],Spinor["Spin","Square",b_]]:=-Mass[b]SpinorChainReduced[a,Spinor["Spin","Angle",b]];
SpinorChainReduced[a__,Mom[b_],Spinor["Spin",ul_,"Square",b_,J_]]:=-Mass[b]SpinorChainReduced[a,Spinor["Spin",ul,"Angle",b,J]];
SpinorChainReduced[Spinor["Spin","Angle",b_],Mom[b_],a__]:=Mass[b]SpinorChainReduced[Spinor["Spin","Square",b],a];
SpinorChainReduced[Spinor["Spin",ul_,"Angle",b_,J_],Mom[b_],a__]:=Mass[b]SpinorChainReduced[Spinor["Spin",ul,"Square",b,J],a];
SpinorChainReduced[Spinor["Spin","Square",b_],Mom[b_],a__]:=Mass[b]SpinorChainReduced[Spinor["Spin","Angle",b],a];
SpinorChainReduced[Spinor["Spin",ul_,"Square",b_,J_],Mom[b_],a__]:=Mass[b]SpinorChainReduced[Spinor["Spin",ul,"Angle",b,J],a];


(* ::Subsubsection::Closed:: *)
(*Forms with \[Zeta]*)


(*<2\[Zeta]2> = Sqrt[2E2]*)
SpinorChainReduced[Spinor["Helicity",sa1_,p1_],Spinor["Zeta",sa1_,p1_]]:=Sqrt[2En[p1]];
SpinorChainReduced[Spinor["Zeta",sa1_,p1_],Spinor["Helicity",sa1_,p1_]]:=-Sqrt[2En[p1]];
(*<\[Zeta]1\[Zeta]2> = <21>/Sqrt[4E1E2]*)
SpinorChainReduced[Spinor["Zeta","Angle",p1_],Spinor["Zeta","Angle",p2_]]:=SpinorChainReduced[Spinor["Helicity","Square",p2],Spinor["Helicity","Square",p1]]/Sqrt[4En[p1]En[p2]];
SpinorChainReduced[Spinor["Zeta","Square",p1_],Spinor["Zeta","Square",p2_]]:=SpinorChainReduced[Spinor["Helicity","Angle",p2],Spinor["Helicity","Angle",p1]]/Sqrt[4En[p1]En[p2]];
(*<1\[Zeta]2><\[Zeta]12> = <2\[Zeta]2><\[Zeta]11> - <21><\[Zeta]1\[Zeta]2>*)
Unprotect[Times];
SpinorChainReduced[Spinor["Helicity",sa_,p1_],Spinor["Zeta",sa_,p2_]]SpinorChainReduced[Spinor["Zeta",sa_,p1_],Spinor["Helicity",sa_,p2_]]:=
SpinorChainReduced[Spinor["Helicity",sa,p2],Spinor["Zeta",sa,p2]]SpinorChainReduced[Spinor["Zeta",sa,p1],Spinor["Helicity",sa,p1]]-
SpinorChainReduced[Spinor["Helicity",sa,p2],Spinor["Helicity",sa,p1]]SpinorChainReduced[Spinor["Zeta",sa,p1],Spinor["Zeta",sa,p2]];
Protect[Times];
(*[1|p2|\[Zeta]2> = [12]<2\[Zeta]2>*)
SpinorChainReduced[aa__,Mom[p2_],Spinor["Zeta","Angle",p2_]]:=SpinorChainReduced[aa,Spinor["Helicity","Square",p2]]SpinorChainReduced[Spinor["Helicity","Angle",p2],Spinor["Zeta","Angle",p2]];
SpinorChainReduced[aa__,Mom[p2_],Spinor["Zeta","Square",p2_]]:=SpinorChainReduced[aa,Spinor["Helicity","Angle",p2]]SpinorChainReduced[Spinor["Helicity","Square",p2],Spinor["Zeta","Square",p2]];
SpinorChainReduced[Spinor["Zeta","Angle",p2_],Mom[p2_],aa__]:=SpinorChainReduced[Spinor["Zeta","Angle",p2],Spinor["Helicity","Angle",p2]]SpinorChainReduced[Spinor["Helicity","Square",p2],aa];
SpinorChainReduced[Spinor["Zeta","Square",p2_],Mom[p2_],aa__]:=SpinorChainReduced[Spinor["Zeta","Square",p2],Spinor["Helicity","Square",p2]]SpinorChainReduced[Spinor["Helicity","Angle",p2],aa];


(* ::Subsubsection::Closed:: *)
(*Momentum Rearrangements (<p1|...p2p1... = )*)


(* ::Input::Initialization:: *)
(*<p1|...p2p1... = *)
SpinorChainReduced[a___,Mom[pN_],Mom[pN_],b___]:=Mass[pN]^2 SpinorChainReduced[a,b];
SpinorChainReduced[a___,Mom[pN_],Mom[pM_],b___,Mom[pN_],c___]/;pM!=pN:=2MomProd[pN,pM]SpinorChainReduced[a,b,Mom[pN],c]-SpinorChainReduced[a,Mom[pM],Mom[pN],b,Mom[pN],c];

SpinorChainReduced[a___,Mom[pN_],Mom[pM_],b___,Spinor[sh_,ul_,pN_]]/;pM!=pN:=2MomProd[pN,pM]SpinorChainReduced[a,b,Spinor[sh,ul,pN]]-SpinorChainReduced[a,Mom[pM],Mom[pN],b,Spinor[sh,ul,pN]];
SpinorChainReduced[Spinor[sh_,ul_,pN_],b___,Mom[pM_],Mom[pN_],a___]/;pM!=pN:=2MomProd[pM,pN]SpinorChainReduced[Spinor[sh,ul,pN],b,a]-SpinorChainReduced[Spinor[sh,ul,pN],b,Mom[pN],Mom[pM],a];

(*Test*)
(*nice[SpinorChainReduced[Spinor["Spin","Square",4],Mom[1],Mom[2],Mom[4],Mom[1],Mom[3],Spinor["Spin","Angle",1]]/.SpinorChainReduced->SpinorChain]*)


(* ::Subsubsection::Closed:: *)
(*Schouten Identities ([12][34]=[42][31]-[41][32] and Generalizations)*)


(* ::Input::Initialization:: *)
Unprotect[Plus];
(*[14][23]-[13][24]*)
SpinorChainReduced[Spinor[sh1_,tp_,p1_],Spinor[sh4_,tp_,p4_]]SpinorChainReduced[Spinor[sh2_,tp_,p2_],Spinor[sh3_,tp_,p3_]]-SpinorChainReduced[Spinor[sh1_,tp_,p1_],Spinor[sh3_,tp_,p3_]]SpinorChainReduced[Spinor[sh2_,tp_,p2_],Spinor[sh4_,tp_,p4_]]:=-SpinorChainReduced[Spinor[sh1,tp,p1],Spinor[sh2,tp,p2]]SpinorChainReduced[Spinor[sh3,tp,p3],Spinor[sh4,tp,p4]];
(*[14][23]+[13][42]*)
SpinorChainReduced[Spinor[sh1_,tp_,p1_],Spinor[sh4_,tp_,p4_]]SpinorChainReduced[Spinor[sh2_,tp_,p2_],Spinor[sh3_,tp_,p3_]]-SpinorChainReduced[Spinor[sh1_,tp_,p1_],Spinor[sh3_,tp_,p3_]]SpinorChainReduced[Spinor[sh4_,tp_,p4_],Spinor[sh2_,tp_,p2_]]:=-SpinorChainReduced[Spinor[sh1,tp,p1],Spinor[sh2,tp,p2]]SpinorChainReduced[Spinor[sh3,tp,p3],Spinor[sh4,tp,p4]];
(*[14][23]+[31][24]*)
SpinorChainReduced[Spinor[sh1_,tp_,p1_],Spinor[sh4_,tp_,p4_]]SpinorChainReduced[Spinor[sh2_,tp_,p2_],Spinor[sh3_,tp_,p3_]]-SpinorChainReduced[Spinor[sh3_,tp_,p3_],Spinor[sh1_,tp_,p1_]]SpinorChainReduced[Spinor[sh2_,tp_,p2_],Spinor[sh4_,tp_,p4_]]:=-SpinorChainReduced[Spinor[sh1,tp,p1],Spinor[sh2,tp,p2]]SpinorChainReduced[Spinor[sh3,tp,p3],Spinor[sh4,tp,p4]];
(*[14][23]-[31][42]*)
SpinorChainReduced[Spinor[sh1_,tp_,p1_],Spinor[sh4_,tp_,p4_]]SpinorChainReduced[Spinor[sh2_,tp_,p2_],Spinor[sh3_,tp_,p3_]]-SpinorChainReduced[Spinor[sh3_,tp_,p3_],Spinor[sh1_,tp_,p1_]]SpinorChainReduced[Spinor[sh4_,tp_,p4_],Spinor[sh2_,tp_,p2_]]:=-SpinorChainReduced[Spinor[sh1,tp,p1],Spinor[sh2,tp,p2]]SpinorChainReduced[Spinor[sh3,tp,p3],Spinor[sh4,tp,p4]];
(*[12][34]+[14][23] = [42][31]*)
SpinorChainReduced[Spinor[sh1_,tp_,p1_],Spinor[sh2_,tp_,p2_]]SpinorChainReduced[Spinor[sh3_,tp_,p3_],Spinor[sh4_,tp_,p4_]]+SpinorChainReduced[Spinor[sh1_,tp_,p1_],Spinor[sh4_,tp_,p4_]]SpinorChainReduced[Spinor[sh2_,tp_,p2_],Spinor[sh3_,tp_,p3_]]:=SpinorChainReduced[Spinor[sh4,tp,p4],Spinor[sh2,tp,p2]]SpinorChainReduced[Spinor[sh3,tp,p3],Spinor[sh1,tp,p1]];
(*[12][34]-[41][23] = [42][31]*)
SpinorChainReduced[Spinor[sh1_,tp_,p1_],Spinor[sh2_,tp_,p2_]]SpinorChainReduced[Spinor[sh3_,tp_,p3_],Spinor[sh4_,tp_,p4_]]-SpinorChainReduced[Spinor[sh4_,tp_,p4_],Spinor[sh1_,tp_,p1_]]SpinorChainReduced[Spinor[sh2_,tp_,p2_],Spinor[sh3_,tp_,p3_]]:=SpinorChainReduced[Spinor[sh4,tp,p4],Spinor[sh2,tp,p2]]SpinorChainReduced[Spinor[sh3,tp,p3],Spinor[sh1,tp,p1]];
(*[12][34]-[14][32] = [42][31]*)
SpinorChainReduced[Spinor[sh1_,tp_,p1_],Spinor[sh2_,tp_,p2_]]SpinorChainReduced[Spinor[sh3_,tp_,p3_],Spinor[sh4_,tp_,p4_]]-SpinorChainReduced[Spinor[sh1_,tp_,p1_],Spinor[sh4_,tp_,p4_]]SpinorChainReduced[Spinor[sh3_,tp_,p3_],Spinor[sh2_,tp_,p2_]]:=SpinorChainReduced[Spinor[sh4,tp,p4],Spinor[sh2,tp,p2]]SpinorChainReduced[Spinor[sh3,tp,p3],Spinor[sh1,tp,p1]];
(*[12][34]+[41][32] = [42][31]*)
SpinorChainReduced[Spinor[sh1_,tp_,p1_],Spinor[sh2_,tp_,p2_]]SpinorChainReduced[Spinor[sh3_,tp_,p3_],Spinor[sh4_,tp_,p4_]]+SpinorChainReduced[Spinor[sh4_,tp_,p4_],Spinor[sh1_,tp_,p1_]]SpinorChainReduced[Spinor[sh3_,tp_,p3_],Spinor[sh2_,tp_,p2_]]:=SpinorChainReduced[Spinor[sh4,tp,p4],Spinor[sh2,tp,p2]]SpinorChainReduced[Spinor[sh3,tp,p3],Spinor[sh1,tp,p1]];
Protect[Plus];

(*Test*)
(*SpinorChain[Spinor["Spin","Angle",1],Spinor["Spin","Angle",4]]SpinorChain[Spinor["Spin","Angle",2],Spinor["Spin","Angle",3]]-SpinorChain[Spinor["Spin","Angle",1],Spinor["Spin","Angle",3]]SpinorChain[Spinor["Spin","Angle",2],Spinor["Spin","Angle",4]]
ReduceSpinorProducts[SpinorChain[Spinor["Spin","Angle",1],Spinor["Spin","Angle",4]]SpinorChain[Spinor["Spin","Angle",2],Spinor["Spin","Angle",3]]-SpinorChain[Spinor["Spin","Angle",1],Spinor["Spin","Angle",3]]SpinorChain[Spinor["Spin","Angle",2],Spinor["Spin","Angle",4]]]*)


Unprotect[Plus];
(*[13][4|p1|2>-[14][3|p1|2> = <2|p1|1][34]*)
SpinorChainReduced[Spinor[sh1_,"Square",p1_],Spinor[sh3_,"Square",p3_]]SpinorChainReduced[Spinor[sh4_,"Square",p4_],Mom[p1_],Spinor[sh2_,"Angle",p2_]]-
SpinorChainReduced[Spinor[sh1_,"Square",p1_],Spinor[sh4_,"Square",p4_]]SpinorChainReduced[Spinor[sh3_,"Square",p3_],Mom[p1_],Spinor[sh2_,"Angle",p2_]]:=
SpinorChainReduced[Spinor[sh3,"Square",p3],Spinor[sh4,"Square",p4]]SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p1],Spinor[sh2,"Angle",p2]];
Protect[Plus];


(* ::Input::Initialization:: *)
Unprotect[Times];
(*<14>[4|p1|3> = <34>[4|p1|1> - <31>[4|p1|4>*)
(*SpinorChainReduced[Spinor[sh1_,"Angle",p1_],Spinor[sh4_,"Angle",p4_]]SpinorChainReduced[Spinor[sh4_,"Square",p4_],Mom[p1_],Spinor[sh3_,"Angle",p3_]]/;(p4!=p1&&p4!=p3):=SpinorChainReduced[Spinor[sh3,"Angle",p3],Spinor[sh4,"Angle",p4]]SpinorChainReduced[Spinor[sh4,"Square",p4],Mom[p1],Spinor[sh1,"Angle",p1]]-SpinorChainReduced[Spinor[sh3,"Angle",p3],Spinor[sh1,"Angle",p1]]SpinorChainReduced[Spinor[sh4,"Square",p4],Mom[p1],Spinor[sh4,"Angle",p4]];
SpinorChainReduced[Spinor[sh4_,"Angle",p4_],Spinor[sh1_,"Angle",p1_]]SpinorChainReduced[Spinor[sh4_,"Square",p4_],Mom[p1_],Spinor[sh3_,"Angle",p3_]]/;(p4!=p1&&p4!=p3):=-SpinorChainReduced[Spinor[sh3,"Angle",p3],Spinor[sh4,"Angle",p4]]SpinorChainReduced[Spinor[sh4,"Square",p4],Mom[p1],Spinor[sh1,"Angle",p1]]+SpinorChainReduced[Spinor[sh3,"Angle",p3],Spinor[sh1,"Angle",p1]]SpinorChainReduced[Spinor[sh4,"Square",p4],Mom[p1],Spinor[sh4,"Angle",p4]];*)
Protect[Times];


(* ::Input::Initialization:: *)
Unprotect[Times];
(*[1|p3|2>[3|p1|4>*)
SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p3_],Spinor[sh2_,"Angle",p2_]]SpinorChainReduced[Spinor[sh3_,"Square",p3_],Mom[p1_],Spinor[sh4_,"Angle",p4_]]/;p4!=p2:=-SpinorChainReduced[Spinor[sh4,"Angle",p4],Mom[p1],Mom[p3],Spinor[sh2,"Angle",p2]]SpinorChainReduced[Spinor[sh3,"Square",p3],Spinor[sh1,"Square",p1]]+SpinorChainReduced[Spinor[sh4,"Angle",p4],Mom[p1],Spinor[sh1,"Square",p1]]SpinorChainReduced[Spinor[sh3,"Square",p3],Mom[p3],Spinor[sh2,"Angle",p2]];
(*[1|p4|2>[3|p1|4>*)
SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p4_],Spinor[sh2_,"Angle",p2_]]SpinorChainReduced[Spinor[sh3_,"Square",p3_],Mom[p1_],Spinor[sh4_,"Angle",p4_]]/;p4!=p2:=-SpinorChainReduced[Spinor[sh4,"Angle",p4],Mom[p1],Mom[p4],Spinor[sh2,"Angle",p2]]SpinorChainReduced[Spinor[sh3,"Square",p3],Spinor[sh1,"Square",p1]]+SpinorChainReduced[Spinor[sh4,"Angle",p4],Mom[p1],Spinor[sh1,"Square",p1]]SpinorChainReduced[Spinor[sh3,"Square",p3],Mom[p4],Spinor[sh2,"Angle",p2]];
(*[1|p3|2>[3|p2|4>*)
SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p3_],Spinor[sh2_,"Angle",p2_]]SpinorChainReduced[Spinor[sh3_,"Square",p3_],Mom[p2_],Spinor[sh4_,"Angle",p4_]]/;p4!=p2:=
-SpinorChainReduced[Spinor[sh4,"Angle",p4],Mom[p2],Mom[p3],Spinor[sh2,"Angle",p2]]SpinorChainReduced[Spinor[sh3,"Square",p3],Spinor[sh1,"Square",p1]]+SpinorChainReduced[Spinor[sh4,"Angle",p4],Mom[p2],Spinor[sh1,"Square",p1]]SpinorChainReduced[Spinor[sh3,"Square",p3],Mom[p3],Spinor[sh2,"Angle",p2]];
(*[1|p4|2>[3|p2|4>*)
SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p4_],Spinor[sh2_,"Angle",p2_]]SpinorChainReduced[Spinor[sh3_,"Square",p3_],Mom[p2_],Spinor[sh4_,"Angle",p4_]]/;p3!=p1:=-SpinorChainReduced[Spinor[sh4,"Angle",p4],Spinor[sh2,"Angle",p2]]SpinorChainReduced[Spinor[sh3,"Square",p3],Mom[p2],Mom[p4],Spinor[sh1,"Square",p1]]+SpinorChainReduced[Spinor[sh4,"Angle",p4],Mom[p4],Spinor[sh1,"Square",p1]]SpinorChainReduced[Spinor[sh3,"Square",p3],Mom[p2],Spinor[sh2,"Angle",p2]];
Protect[Times];

(*Test*)
(*ReduceSpinorProducts[SpinorChain[Spinor["Spin","Square",1],Mom[4],Spinor["Spin","Angle",2]]SpinorChain[Spinor["Spin","Square",3],Mom[2],Spinor["Spin","Angle",4]]]*)


(* ::Input::Initialization:: *)
Unprotect[Times];
(*[1|p4|2>[3|p5|4> = -<42>[3|p5p4|1]+<4|p4|1][3|p5|2>*)
SpinorChainReduced[Spinor[sh1_,sq_,p1_],Mom[p4_],Spinor[sh2_,an_,p2_]]SpinorChainReduced[Spinor[sh3_,sq_,p3_],Mom[p5_],Spinor[sh4_,an_,p4_]]:=-SpinorChainReduced[Spinor[sh4,an,p4],Spinor[sh2,an,p2]]SpinorChainReduced[Spinor[sh3,sq,p3],Mom[p5],Mom[p4],Spinor[sh1,sq,p1]]+SpinorChainReduced[Spinor[sh4,an,p4],Mom[p4],Spinor[sh1,sq,p1]]SpinorChainReduced[Spinor[sh3,sq,p3],Mom[p5],Spinor[sh2,an,p2]];
(*[1|p5|2>[3|p1|4> = -<4|p1p5|2>[31] + <4|p1|1][3|p5|2>*)
SpinorChainReduced[Spinor[sh1_,sq_,p1_],Mom[p5_],Spinor[sh2_,an_,p2_]]SpinorChainReduced[Spinor[sh3_,sq_,p3_],Mom[p1_],Spinor[sh4_,an_,p4_]]:=-SpinorChainReduced[Spinor[sh4,an,p4],Mom[p1],Mom[p5],Spinor[sh2,an,p2]]SpinorChainReduced[Spinor[sh3,sq,p3],Spinor[sh1,sq,p1]]+SpinorChainReduced[Spinor[sh4,an,p4],Mom[p1],Spinor[sh1,sq,p1]]SpinorChainReduced[Spinor[sh3,sq,p3],Mom[p5],Spinor[sh2,an,p2]];
(*[1|p5|2>[3|p1|4(>^2)*)
SpinorChainReduced[Spinor[sh1_,sq_,p1_],Mom[p5_],Spinor[sh2_,an_,p2_]]SpinorChainReduced[Spinor[sh3_,sq_,p3_],Mom[p1_],Spinor[sh4_,an_,p4_]]^pow_:=SpinorChainReduced[Spinor[sh3,sq,p3],Mom[p1],Spinor[sh4,an,p4]]^(pow-1)(-SpinorChainReduced[Spinor[sh4,an,p4],Mom[p1],Mom[p5],Spinor[sh2,an,p2]]SpinorChainReduced[Spinor[sh3,sq,p3],Spinor[sh1,sq,p1]]+SpinorChainReduced[Spinor[sh4,an,p4],Mom[p1],Spinor[sh1,sq,p1]]SpinorChainReduced[Spinor[sh3,sq,p3],Mom[p5],Spinor[sh2,an,p2]]);
(*[4|p1|3>^2[1|p3|2> = [4|p1|3>(-<23>[1|p3p1|4]+<2|p1|4][1|p3|3>)*)
SpinorChainReduced[Spinor[sh4_,sq_,p4_],Mom[p1_],Spinor[sh3_,an_,p3_]]^pow_ SpinorChainReduced[Spinor[sh1_,sq_,p1_],Mom[p3_],Spinor[sh2_,an_,p2_]]:=SpinorChainReduced[Spinor[sh4,sq,p4],Mom[p1],Spinor[sh3,an,p3]]^(pow-1)(-SpinorChainReduced[Spinor[sh2,an,p2],Spinor[sh3,an,p3]]SpinorChainReduced[Spinor[sh1,sq,p1],Mom[p3],Mom[p1],Spinor[sh4,sq,p4]]+SpinorChainReduced[Spinor[sh2,an,p2],Mom[p1],Spinor[sh4,sq,p4]]SpinorChainReduced[Spinor[sh1,sq,p1],Mom[p3],Spinor[sh3,an,p3]]);
(*[4|p1|3>^2[2|p5|1> = [4|p1|3>(-<13>[2|p5p1|4]+<1|p1|4][2|p5|3>)*)
SpinorChainReduced[Spinor[sh4_,sq_,p4_],Mom[p1_],Spinor[sh3_,an_,p3_]]^pow_ SpinorChainReduced[Spinor[sh2_,sq_,p2_],Mom[p5_],Spinor[sh1_,an_,p1_]]:=SpinorChainReduced[Spinor[sh4,sq,p4],Mom[p1],Spinor[sh3,an,p3]]^(pow-1)(-SpinorChainReduced[Spinor[sh1,an,p1],Spinor[sh3,an,p3]]SpinorChainReduced[Spinor[sh2,sq,p2],Mom[p5],Mom[p1],Spinor[sh4,sq,p4]]+SpinorChainReduced[Spinor[sh1,an,p1],Mom[p1],Spinor[sh4,sq,p4]]SpinorChainReduced[Spinor[sh2,sq,p2],Mom[p5],Spinor[sh3,an,p3]]);
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
SpinorChainReduced[Spinor[sh1_,an_,p1_],Spinor[sh2_,an_,p2_]]SpinorChainReduced[Spinor[sh3_,an_,p3_],Mom[p1_],Mom[p2_],Spinor[sh3_,an_,p3_]]:=SpinorChainReduced[Spinor[sh3,an,p3],Mom[p2],Mom[p1],Spinor[sh2,an,p2]]SpinorChainReduced[Spinor[sh3,an,p3],Spinor[sh1,an,p1]]-SpinorChainReduced[Spinor[sh3,an,p3],Mom[p2],Mom[p1],Spinor[sh1,an,p1]]SpinorChainReduced[Spinor[sh3,an,p3],Spinor[sh2,an,p2]];
(*<24><3|p1p2|3> = <34><3|p1p2|2> - <32><3|p1p2|4>*)
SpinorChainReduced[Spinor[sh2_,an_,p2_],Spinor[sh4_,an_,p4_]]SpinorChainReduced[Spinor[sh3_,an_,p3_],Mom[p1_],Mom[p2_],Spinor[sh3_,an_,p3_]]:=SpinorChainReduced[Spinor[sh3,an,p3],Spinor[sh4,an,p4]]SpinorChainReduced[Spinor[sh3,an,p3],Mom[p1],Mom[p2],Spinor[sh2,an,p2]]-SpinorChainReduced[Spinor[sh3,an,p3],Spinor[sh2,an,p2]]SpinorChainReduced[Spinor[sh3,an,p3],Mom[p1],Mom[p2],Spinor[sh4,an,p4]];
(*[12]<3|p1p2|3><4|p1p2|4> = - <3|p2|2]<3|p1|1]<4|p1p2|4> + <3|p2|1]<3|p1|2]<4|p1p2|4>*)
(*                         = - <3|p2|2]<3|p1|1]<4|p1p2|4> + <3|p2|1](<4|p2|2]<4|p1p1|3> - <4|p2p1|3><4|p1|2])*)
SpinorChainReduced[Spinor[sh1_,sq_,p1_],Spinor[sh2_,sq_,p2_]]SpinorChainReduced[Spinor[sh3_,an_,p3_],Mom[p1_],Mom[p2_],Spinor[sh3_,an_,p3_]]SpinorChainReduced[Spinor[sh4_,an_,p4_],Mom[p1_],Mom[p2_],Spinor[sh4_,an_,p4_]]/;sq=!=an:=-SpinorChainReduced[Spinor[sh3,an,p3],Mom[p2],Spinor[sh2,sq,p2]]SpinorChainReduced[Spinor[sh3,an,p3],Mom[p1],Spinor[sh1,sq,p1]]SpinorChainReduced[Spinor[sh4,an,p4],Mom[p1],Mom[p2],Spinor[sh4,an,p4]]+SpinorChainReduced[Spinor[sh3,an,p3],Mom[p2],Spinor[sh1,sq,p1]]SpinorChainReduced[Spinor[sh4,an,p4],Mom[p2],Spinor[sh2,sq,p2]]SpinorChainReduced[Spinor[sh4,an,p4],Mom[p1],Mom[p1],Spinor[sh3,an,p3]]-SpinorChainReduced[Spinor[sh3,an,p3],Mom[p2],Spinor[sh1,sq,p1]]SpinorChainReduced[Spinor[sh4,an,p4],Mom[p2],Mom[p1],Spinor[sh3,an,p3]]SpinorChainReduced[Spinor[sh4,an,p4],Mom[p1],Spinor[sh2,sq,p2]];
Protect[Times];


(* ::Input::Initialization:: *)
Unprotect[Times];
(*[12][3|p1|4>[5|p2|6> = -<4|p1|2][31][5|p2|6>+<4|p1|1][32][5|p2|6>*)
SpinorChainReduced[Spinor[sh1_,sq_,p1_],Spinor[sh2_,sq_,p2_]]SpinorChainReduced[Spinor[sh3_,sq_,p3_],Mom[p1_],Spinor[sh4_,an_,p4_]]SpinorChainReduced[Spinor[sh5_,sq_,p5_],Mom[p2_],Spinor[sh6_,an_,p6_]]:=-SpinorChainReduced[Spinor[sh4,an,p4],Mom[p1],Spinor[sh2,sq,p2]]SpinorChainReduced[Spinor[sh3,sq,p3],Spinor[sh1,sq,p1]]SpinorChainReduced[Spinor[sh5,sq,p5],Mom[p2],Spinor[sh6,an,p6]]+SpinorChainReduced[Spinor[sh4,an,p4],Mom[p1],Spinor[sh1,sq,p1]]SpinorChainReduced[Spinor[sh3,sq,p3],Spinor[sh2,sq,p2]]SpinorChainReduced[Spinor[sh5,sq,p5],Mom[p2],Spinor[sh6,an,p6]];
(*<12>[3|p1|4>[5|p2|6> = <42>[3|p1|1>[5|p2|6> - <41>[3|p1|2>[5|p2|6>*)
SpinorChainReduced[Spinor[sh1_,an_,p1_],Spinor[sh2_,an_,p2_]]SpinorChainReduced[Spinor[sh3_,sq_,p3_],Mom[p1_],Spinor[sh4_,an_,p4_]]SpinorChainReduced[Spinor[sh5_,sq_,p5_],Mom[p2_],Spinor[sh6_,an_,p6_]]:=
SpinorChainReduced[Spinor[sh4,an,p4],Spinor[sh2,an,p2]]SpinorChainReduced[Spinor[sh3,sq,p3],Mom[p1],Spinor[sh1,an,p1]]SpinorChainReduced[Spinor[sh5,sq,p5],Mom[p2],Spinor[sh6,an,p6]]-SpinorChainReduced[Spinor[sh4,an,p4],Spinor[sh1,an,p1]]SpinorChainReduced[Spinor[sh3,sq,p3],Mom[p1],Spinor[sh2,an,p2]]SpinorChainReduced[Spinor[sh5,sq,p5],Mom[p2],Spinor[sh6,an,p6]];
Protect[Times];


(* ::Input::Initialization:: *)
Unprotect[Times];
(*[1|p3|2><3|p5p6|4> = -[1|p3|2><4|p6p5|3> = <32><4|p6p5p3|1] - <3|p3|1]<4|p6p5|2>*)
SpinorChainReduced[Spinor[sh1_,sq_,p1_],Mom[p3_],Spinor[sh2_,an_,p2_]]SpinorChainReduced[Spinor[sh3_,an_,p3_],Mom[p5_],Mom[p6_],Spinor[sh4_,an_,p4_]]:=SpinorChainReduced[Spinor[sh3,an,p3],Spinor[sh2,an,p2]]SpinorChainReduced[Spinor[sh4,an,p4],Mom[p6],Mom[p5],Mom[p3],Spinor[sh1,sq,p1]]-SpinorChainReduced[Spinor[sh3,an,p3],Mom[p3],Spinor[sh1,sq,p1]]SpinorChainReduced[Spinor[sh4,an,p4],Mom[p6],Mom[p5],Spinor[sh2,an,p2]];
(*[1|p4|2><3|p5p6|4> = -<42><3|p5p6p4|1] + <4|p4|1]<3|p5p6|2>*)
SpinorChainReduced[Spinor[sh1_,sq_,p1_],Mom[p4_],Spinor[sh2_,an_,p2_]]SpinorChainReduced[Spinor[sh3_,an_,p3_],Mom[p5_],Mom[p6_],Spinor[sh4_,an_,p4_]]:=
-SpinorChainReduced[Spinor[sh4,an,p4],Spinor[sh2,an,p2]]SpinorChainReduced[Spinor[sh3,an,p3],Mom[p5],Mom[p6],Mom[p4],Spinor[sh1,sq,p1]]+SpinorChainReduced[Spinor[sh4,an,p4],Mom[p4],Spinor[sh1,sq,p1]]SpinorChainReduced[Spinor[sh3,an,p3],Mom[p5],Mom[p6],Spinor[sh2,an,p2]];
(*[1|p5p6|2][3|p2|4> = -<4|p2|2][3|p6p5|1]+<4|p2p6p5|1][32]*)
SpinorChainReduced[Spinor[sh1_,sq_,p1_],Mom[p5_],Mom[p6_],Spinor[sh2_,sq_,p2_]]SpinorChainReduced[Spinor[sh3_,sq_,p3_],Mom[p2_],Spinor[sh4_,an_,p4_]]:=-SpinorChainReduced[Spinor[sh4,an,p4],Mom[p2],Spinor[sh2,sq,p2]]SpinorChainReduced[Spinor[sh3,sq,p3],Mom[p6],Mom[p5],Spinor[sh1,sq,p1]]+SpinorChainReduced[Spinor[sh4,an,p4],Mom[p2],Mom[p6],Mom[p5],Spinor[sh1,sq,p1]]SpinorChainReduced[Spinor[sh3,sq,p3],Spinor[sh2,sq,p2]];
(*[1|p5p6|2][3|p1|4> = -<4|p1p5p6|2][31]+<4|p1|1][3|p5p6|2]*)
SpinorChainReduced[Spinor[sh1_,sq_,p1_],Mom[p5_],Mom[p6_],Spinor[sh2_,sq_,p2_]]SpinorChainReduced[Spinor[sh3_,sq_,p3_],Mom[p1_],Spinor[sh4_,an_,p4_]]:=-SpinorChainReduced[Spinor[sh4,an,p4],Mom[p1],Mom[p5],Mom[p6],Spinor[sh2,sq,p2]]SpinorChainReduced[Spinor[sh3,sq,p3],Spinor[sh1,sq,p1]]+SpinorChainReduced[Spinor[sh4,an,p4],Mom[p1],Spinor[sh1,sq,p1]]SpinorChainReduced[Spinor[sh3,sq,p3],Mom[p5],Mom[p6],Spinor[sh2,sq,p2]];
(*[4|p1|2><3|p1p2|3> = -<32><3|p1p2p1|4]+<3|p1|4]<3|p1p2|2>*)
SpinorChainReduced[Spinor[sh4_,sq_,p4_],Mom[p1_],Spinor[sh2_,an_,p2_]]SpinorChainReduced[Spinor[sh3_,an_,p3_],Mom[p1_],Mom[p2_],Spinor[sh3_,an_,p3_]]:=-SpinorChainReduced[Spinor[sh3,an,p3],Spinor[sh2,an,p2]]SpinorChainReduced[Spinor[sh3,an,p3],Mom[p1],Mom[p2],Mom[p1],Spinor[sh4,sq,p4]]+SpinorChainReduced[Spinor[sh3,an,p3],Mom[p1],Spinor[sh4,sq,p4]]SpinorChainReduced[Spinor[sh3,an,p3],Mom[p1],Mom[p2],Spinor[sh2,an,p2]];
(*[4|p3|2>[3|p1p2|3] = -<2|p3|3][4|p2p1|3] + <2|p3p2p1|3][43]*)
SpinorChainReduced[Spinor[sh4_,sq_,p4_],Mom[p3_],Spinor[sh2_,an_,p2_]]SpinorChainReduced[Spinor[sh3_,sq_,p3_],Mom[p1_],Mom[p2_],Spinor[sh3_,sq_,p3_]]:=-SpinorChainReduced[Spinor[sh2,an,p2],Mom[p3],Spinor[sh3,sq,p3]]SpinorChainReduced[Spinor[sh4,sq,p4],Mom[p2],Mom[p1],Spinor[sh3,sq,p3]]+SpinorChainReduced[Spinor[sh2,an,p2],Mom[p3],Mom[p2],Mom[p1],Spinor[sh3,sq,p3]]SpinorChainReduced[Spinor[sh4,sq,p4],Spinor[sh3,sq,p3]];
(*[4|p2|1><3|p1p2|3> = -<31><3|p1p2p2|4]+<3|p2|4]<3|p1p2|1>*)
SpinorChainReduced[Spinor[sh4_,sq_,p4_],Mom[p2_],Spinor[sh1_,an_,p1_]]SpinorChainReduced[Spinor[sh3_,an_,p3_],Mom[p1_],Mom[p2_],Spinor[sh3_,an_,p3_]]:=-SpinorChainReduced[Spinor[sh3,an,p3],Spinor[sh1,an,p1]]SpinorChainReduced[Spinor[sh3,an,p3],Mom[p1],Mom[p2],Mom[p2],Spinor[sh4,sq,p4]]+SpinorChainReduced[Spinor[sh3,an,p3],Mom[p2],Spinor[sh4,sq,p4]]SpinorChainReduced[Spinor[sh3,an,p3],Mom[p1],Mom[p2],Spinor[sh1,an,p1]];
(*[1|p2|3>[4|p1p2|4]=[4|p2p1p2|3>[41]-[4|p2p1|1][4|p2|3>*)
SpinorChainReduced[Spinor[sh1_,sq_,p1_],Mom[p2_],Spinor[sh3_,an_,p3_]]SpinorChainReduced[Spinor[sh4_,sq_,p4_],Mom[p1_],Mom[p2_],Spinor[sh4_,sq_,p4_]]:=SpinorChainReduced[Spinor[sh4,sq,p4],Mom[p2],Mom[p1],Mom[p2],Spinor[sh3,an,p3]]SpinorChainReduced[Spinor[sh4,sq,p4],Spinor[sh1,sq,p1]]-SpinorChainReduced[Spinor[sh4,sq,p4],Mom[p2],Mom[p1],Spinor[sh1,sq,p1]]SpinorChainReduced[Spinor[sh4,sq,p4],Mom[p2],Spinor[sh3,an,p3]];
Protect[Times];
Unprotect[Times];
(*[2|p1|3>[4|p1p2|4]=[4|p2p1p1|3>[42]-[4|p2p1|2][4|p1|3>*)
SpinorChainReduced[Spinor[sh2_,sq_,p2_],Mom[p1_],Spinor[sh3_,an_,p3_]]SpinorChainReduced[Spinor[sh4_,sq_,p4_],Mom[p1_],Mom[p2_],Spinor[sh4_,sq_,p4_]]:=SpinorChainReduced[Spinor[sh4,sq,p4],Mom[p2],Mom[p1],Mom[p1],Spinor[sh3,an,p3]]SpinorChainReduced[Spinor[sh4,sq,p4],Spinor[sh2,sq,p2]]-SpinorChainReduced[Spinor[sh4,sq,p4],Mom[p2],Mom[p1],Spinor[sh2,sq,p2]]SpinorChainReduced[Spinor[sh4,sq,p4],Mom[p1],Spinor[sh3,an,p3]];
(*[2|p1|3><4|p1p2|4> = - <4|p2p1|3><4|p1|2] + <4|p2p1p1|2]<43>*)
SpinorChainReduced[Spinor[sh2_,sq_,p2_],Mom[p1_],Spinor[sh3_,an_,p3_]]SpinorChainReduced[Spinor[sh4_,an_,p4_],Mom[p1_],Mom[p2_],Spinor[sh4_,an_,p4_]]:=-SpinorChainReduced[Spinor[sh4,an,p4],Mom[p2],Mom[p1],Spinor[sh3,an,p3]]SpinorChainReduced[Spinor[sh4,an,p4],Mom[p1],Spinor[sh2,sq,p2]]+SpinorChainReduced[Spinor[sh4,an,p4],Mom[p2],Mom[p1],Mom[p1],Spinor[sh2,sq,p2]]SpinorChainReduced[Spinor[sh4,an,p4],Spinor[sh3,an,p3]];
Protect[Times];


(* ::Input::Initialization:: *)
Unprotect[Times];
(*[3|p2p4|5]<2|p3p5|4> = -<4|p5|5]<2|p3p4p2|3] + <4|p5p4p2|3]<2|p3|5]*)
SpinorChainReduced[Spinor[sh3_,sq_,p3_],Mom[p2_],Mom[p4_],Spinor[sh5_,sq_,p5_]]SpinorChainReduced[Spinor[sh2_,an_,p2_],Mom[p3_],Mom[p5_],Spinor[sh4_,an_,p4_]]/;sq=!=an:=-SpinorChainReduced[Spinor[sh4,an,p4],Mom[p5],Spinor[sh5,sq,p5]]SpinorChainReduced[Spinor[sh2,an,p2],Mom[p3],Mom[p4],Mom[p2],Spinor[sh3,sq,p3]]+SpinorChainReduced[Spinor[sh4,an,p4],Mom[p5],Mom[p4],Mom[p2],Spinor[sh3,sq,p3]]SpinorChainReduced[Spinor[sh2,an,p2],Mom[p3],Spinor[sh5,sq,p5]];
(*[3|p1p2|3]<2|p4p3|2> = -<2|p3|3]<2|p4p2p1|3]+<2|p3p2p1|3]<2|p4|3]*)
SpinorChainReduced[Spinor[sh3_,sq_,p3_],Mom[p1_],Mom[p2_],Spinor[sh3_,sq_,p3_]]SpinorChainReduced[Spinor[sh2_,an_,p2_],Mom[p4_],Mom[p3_],Spinor[sh2_,an_,p2_]]/;sq=!=an:=-SpinorChainReduced[Spinor[sh2,an,p2],Mom[p3],Spinor[sh3,sq,p3]]SpinorChainReduced[Spinor[sh2,an,p2],Mom[p4],Mom[p2],Mom[p1],Spinor[sh3,sq,p3]]+SpinorChainReduced[Spinor[sh2,an,p2],Mom[p3],Mom[p2],Mom[p1],Spinor[sh3,sq,p3]]SpinorChainReduced[Spinor[sh2,an,p2],Mom[p4],Spinor[sh3,sq,p3]];
(*[3|p1p2|3]<4|p1p2|4> = -<4|p2|3]<4|p1p2p1|3]+<4|p2p2p1|3]<4|p1|3]*)
SpinorChainReduced[Spinor[sh3_,sq_,p3_],Mom[p1_],Mom[p2_],Spinor[sh3_,sq_,p3_]]SpinorChainReduced[Spinor[sh4_,an_,p4_],Mom[p1_],Mom[p2_],Spinor[sh4_,an_,p4_]]/;sq=!=an:=-SpinorChainReduced[Spinor[sh4,an,p4],Mom[p2],Spinor[sh3,sq,p3]]SpinorChainReduced[Spinor[sh4,an,p4],Mom[p1],Mom[p2],Mom[p1],Spinor[sh3,sq,p3]]+SpinorChainReduced[Spinor[sh4,an,p4],Mom[p2],Mom[p2],Mom[p1],Spinor[sh3,sq,p3]]SpinorChainReduced[Spinor[sh4,an,p4],Mom[p1],Spinor[sh3,sq,p3]];
(*[3|p1p2|3][4|p1p2|4]*)
(*SpinorChainReduced[Spinor[sh_,sq_,p3_],Mom[p1_],Mom[p2_],Spinor[sh_,sq_,p3_]]SpinorChainReduced[Spinor[sh_,sq_,p4_],Mom[p1_],Mom[p2_],Spinor[sh_,sq_,p4_]]:>SpinorChainReduced[Spinor[sh,sq,p4],Spinor[sh,sq,p3]]SpinorChainReduced[Spinor[sh,sq,p4],Mom[p1],Mom[p2],Mom[p2],Mom[p1],Spinor[sh,sq,p3]]-SpinorChainReduced[Spinor[sh,sq,p4],Mom[p2],Mom[p1],Spinor[sh,sq,p3]]SpinorChainReduced[Spinor[sh,sq,p4],Mom[p1],Mom[p2],Spinor[sh,sq,p3]]*)
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
SpinorChainReduced[Spinor[sh2_,tp_,p2_],Spinor[sh3_,tp_,p3_]]SpinorChainReduced[Spinor[sh1_,tp_,p1_],Spinor["Zeta",tp_,p2_]]-SpinorChainReduced[Spinor[sh1_,tp_,p1_],Spinor[sh2_,tp_,p2_]]SpinorChainReduced[Spinor["Zeta",tp_,p2_],Spinor[sh3_,tp_,p3_]]:=-SpinorChainReduced[Spinor["Zeta",tp,p2],Spinor[sh2,tp,p2]]SpinorChainReduced[Spinor[sh1,tp,p1],Spinor[sh3,tp,p3]];
Protect[Plus];


(* ::Input::Initialization:: *)
Unprotect[Times];
(*<1\[Zeta]2>[2|p3|2>\[Rule]<2\[Zeta]2>[2|p3|1>-<21>[2|p3|\[Zeta]2>*)
SpinorChainReduced[Spinor[sh1_,"Angle",p1_],Spinor["Zeta","Angle",p2_]]SpinorChainReduced[Spinor["Helicity","Square",p2_],Mom[p3_],Spinor["Helicity","Angle",p2_]]:=SpinorChainReduced[Spinor["Helicity","Angle",p2],Spinor["Zeta","Angle",p2]]SpinorChainReduced[Spinor["Helicity","Square",p2],Mom[p3],Spinor[sh1,"Angle",p1]]-SpinorChainReduced[Spinor["Helicity","Angle",p2],Spinor[sh1,"Angle",p1]]SpinorChainReduced[Spinor["Helicity","Square",p2],Mom[p3],Spinor["Zeta","Angle",p2]];
SpinorChainReduced[Spinor["Zeta","Angle",p2_],Spinor[sh1_,"Angle",p1_]]SpinorChainReduced[Spinor["Helicity","Square",p2_],Mom[p3_],Spinor["Helicity","Angle",p2_]]:=-SpinorChainReduced[Spinor["Helicity","Angle",p2],Spinor["Zeta","Angle",p2]]SpinorChainReduced[Spinor["Helicity","Square",p2],Mom[p3],Spinor[sh1,"Angle",p1]]+SpinorChainReduced[Spinor["Helicity","Angle",p2],Spinor[sh1,"Angle",p1]]SpinorChainReduced[Spinor["Helicity","Square",p2],Mom[p3],Spinor["Zeta","Angle",p2]];
(*[1\[Zeta]2][2|p3|2>\[Rule][2\[Zeta]2]<2|p3|1]-[21]<2|p3|\[Zeta]2]*)
SpinorChainReduced[Spinor[sh1_,"Square",p1_],Spinor["Zeta","Square",p2_]]SpinorChainReduced[Spinor["Helicity","Square",p2_],Mom[p3_],Spinor["Helicity","Angle",p2_]]:=SpinorChainReduced[Spinor["Helicity","Square",p2],Spinor["Zeta","Square",p2]]SpinorChainReduced[Spinor["Helicity","Angle",p2],Mom[p3],Spinor[sh1,"Square",p1]]-SpinorChainReduced[Spinor["Helicity","Square",p2],Spinor[sh1,"Square",p1]]SpinorChainReduced[Spinor["Helicity","Angle",p2],Mom[p3],Spinor["Zeta","Square",p2]];
SpinorChainReduced[Spinor["Zeta","Square",p2_],Spinor[sh1_,"Square",p1_]]SpinorChainReduced[Spinor["Helicity","Square",p2_],Mom[p3_],Spinor["Helicity","Angle",p2_]]:=SpinorChainReduced[Spinor["Helicity","Angle",p2],Mom[p3],Spinor["Zeta","Square",p2]]SpinorChainReduced[Spinor["Helicity","Square",p2],Spinor[sh1,"Square",p1]]-SpinorChainReduced[Spinor["Helicity","Angle",p2],Mom[p3],Spinor[sh1,"Square",p1]]SpinorChainReduced[Spinor["Helicity","Square",p2],Spinor["Zeta","Square",p2]];
(*[13][4|p1|\[Zeta]3> = -<\[Zeta]3|p1|3][41] + <\[Zeta]3|p1|1][43]*)
SpinorChainReduced[Spinor[sh1_,"Square",p1_],Spinor["Helicity","Square",p3_]]SpinorChainReduced[Spinor[sh4_,"Square",p4_],Mom[p1_],Spinor["Zeta","Angle",p3_]]/;p4!=p3:=-SpinorChainReduced[Spinor["Zeta","Angle",p3],Mom[p1],Spinor["Helicity","Square",p3]]SpinorChainReduced[Spinor[sh4,"Square",p4],Spinor[sh1,"Square",p1]]+SpinorChainReduced[Spinor["Zeta","Angle",p3],Mom[p1],Spinor[sh1,"Square",p1]]SpinorChainReduced[Spinor[sh4,"Square",p4],Spinor["Helicity","Square",p3]];
SpinorChainReduced[Spinor["Helicity","Square",p3_],Spinor[sh1_,"Square",p1_]]SpinorChainReduced[Spinor[sh4_,"Square",p4_],Mom[p1_],Spinor["Zeta","Angle",p3_]]/;p4!=p3:=SpinorChainReduced[Spinor["Zeta","Angle",p3],Mom[p1],Spinor["Helicity","Square",p3]]SpinorChainReduced[Spinor[sh4,"Square",p4],Spinor[sh1,"Square",p1]]-SpinorChainReduced[Spinor["Zeta","Angle",p3],Mom[p1],Spinor[sh1,"Square",p1]]SpinorChainReduced[Spinor[sh4,"Square",p4],Spinor["Helicity","Square",p3]];
(*<13>[\[Zeta]3|p1|4> = <43>[\[Zeta]3|p1|1> - <41>[\[Zeta]3|p1|3>*)
SpinorChainReduced[Spinor[sh1_,"Angle",p1_],Spinor["Helicity","Angle",p3_]]SpinorChainReduced[Spinor["Zeta","Square",p3_],Mom[p1_],Spinor[sh4_,"Angle",p4_]]/;p4!=p3:=SpinorChainReduced[Spinor[sh4,"Angle",p4],Spinor["Helicity","Angle",p3]]SpinorChainReduced[Spinor["Zeta","Square",p3],Mom[p1],Spinor[sh1,"Angle",p1]]-SpinorChainReduced[Spinor[sh4,"Angle",p4],Spinor[sh1,"Angle",p1]]SpinorChainReduced[Spinor["Zeta","Square",p3],Mom[p1],Spinor["Helicity","Angle",p3]];
SpinorChainReduced[Spinor["Helicity","Angle",p3_],Spinor[sh1_,"Angle",p1_]]SpinorChainReduced[Spinor["Zeta","Square",p3_],Mom[p1_],Spinor[sh4_,"Angle",p4_]]/;p4!=p3:=-SpinorChainReduced[Spinor[sh4,"Angle",p4],Spinor["Helicity","Angle",p3]]SpinorChainReduced[Spinor["Zeta","Square",p3],Mom[p1],Spinor[sh1,"Angle",p1]]+SpinorChainReduced[Spinor[sh4,"Angle",p4],Spinor[sh1,"Angle",p1]]SpinorChainReduced[Spinor["Zeta","Square",p3],Mom[p1],Spinor["Helicity","Angle",p3]];
(*<1\[Zeta]3>[3|p1|4> = <4\[Zeta]3>[3|p1|1> - <41>[3|p1|\[Zeta]3>*)
SpinorChainReduced[Spinor[sp1_,"Angle",p1_],Spinor["Zeta","Angle",p3_]]SpinorChainReduced[Spinor["Helicity","Square",p3_],Mom[p1_],Spinor[sh4_,"Angle",p4_]]:=SpinorChainReduced[Spinor[sh4,"Angle",p4],Spinor["Zeta","Angle",p3]]SpinorChainReduced[Spinor["Helicity","Square",p3],Mom[p1],Spinor[sp1,"Angle",p1]]-SpinorChainReduced[Spinor[sh4,"Angle",p4],Spinor[sp1,"Angle",p1]]SpinorChainReduced[Spinor["Helicity","Square",p3],Mom[p1],Spinor["Zeta","Angle",p3]];
SpinorChainReduced[Spinor["Zeta","Angle",p3_],Spinor[sp1_,"Angle",p1_]]SpinorChainReduced[Spinor["Helicity","Square",p3_],Mom[p1_],Spinor[sh4_,"Angle",p4_]]:=-SpinorChainReduced[Spinor[sh4,"Angle",p4],Spinor["Zeta","Angle",p3]]SpinorChainReduced[Spinor["Helicity","Square",p3],Mom[p1],Spinor[sp1,"Angle",p1]]+SpinorChainReduced[Spinor[sh4,"Angle",p4],Spinor[sp1,"Angle",p1]]SpinorChainReduced[Spinor["Helicity","Square",p3],Mom[p1],Spinor["Zeta","Angle",p3]];
(*[1\[Zeta]3][4|p1|3> = -<3|p1|\[Zeta]3][41] + <3|p1|1][4\[Zeta]3]*)
SpinorChainReduced[Spinor[sp1_,"Square",p1_],Spinor["Zeta","Square",p3_]]SpinorChainReduced[Spinor[sh4_,"Square",p4_],Mom[p1_],Spinor["Helicity","Angle",p3_]]:=-SpinorChainReduced[Spinor["Helicity","Angle",p3],Mom[p1],Spinor["Zeta","Square",p3]]SpinorChainReduced[Spinor[sh4,"Square",p4],Spinor[sp1,"Square",p1]]+SpinorChainReduced[Spinor["Helicity","Angle",p3],Mom[p1],Spinor[sp1,"Square",p1]]SpinorChainReduced[Spinor[sh4,"Square",p4],Spinor["Zeta","Square",p3]];
SpinorChainReduced[Spinor["Zeta","Square",p3_],Spinor[sp1_,"Square",p1_]]SpinorChainReduced[Spinor[sh4_,"Square",p4_],Mom[p1_],Spinor["Helicity","Angle",p3_]]:=SpinorChainReduced[Spinor["Helicity","Angle",p3],Mom[p1],Spinor["Zeta","Square",p3]]SpinorChainReduced[Spinor[sh4,"Square",p4],Spinor[sp1,"Square",p1]]-SpinorChainReduced[Spinor["Helicity","Angle",p3],Mom[p1],Spinor[sp1,"Square",p1]]SpinorChainReduced[Spinor[sh4,"Square",p4],Spinor["Zeta","Square",p3]];
(*[4|p1|4>[3|p1|\[Zeta]4> = -<\[Zeta]44>[3|p1p1|4] + <\[Zeta]4|p1|4][3|p1|4>*)
SpinorChainReduced[Spinor["Helicity","Square",p4_],Mom[p1_],Spinor["Helicity","Angle",p4_]]SpinorChainReduced[Spinor[sh3_,"Square",p3_],Mom[p1_],Spinor["Zeta","Angle",p4_]]:=-SpinorChainReduced[Spinor["Zeta","Angle",p4],Spinor["Helicity","Angle",p4]]SpinorChainReduced[Spinor[sh3,"Square",p3],Mom[p1],Mom[p1],Spinor["Helicity","Square",p4]]+SpinorChainReduced[Spinor["Zeta","Angle",p4],Mom[p1],Spinor["Helicity","Square",p4]]SpinorChainReduced[Spinor[sh3,"Square",p3],Mom[p1],Spinor["Helicity","Angle",p4]];
(*[4|p1|4>[\[Zeta]4|p1|3> = -<34>[\[Zeta]4|p1p1|4] + <3|p1|4][\[Zeta]4|p1|4>*)
SpinorChainReduced[Spinor["Helicity","Square",p4_],Mom[p1_],Spinor["Helicity","Angle",p4_]]SpinorChainReduced[Spinor["Zeta","Square",p4_],Mom[p1_],Spinor[sh3_,"Angle",p3_]]:=-SpinorChainReduced[Spinor[sh3,"Angle",p3],Spinor["Helicity","Angle",p4]]SpinorChainReduced[Spinor["Zeta","Square",p4],Mom[p1],Mom[p1],Spinor["Helicity","Square",p4]]+SpinorChainReduced[Spinor[sh3,"Angle",p3],Mom[p1],Spinor["Helicity","Square",p4]]SpinorChainReduced[Spinor["Zeta","Square",p4],Mom[p1],Spinor["Helicity","Angle",p4]];
(*<\[Zeta]23>[2|p1p4|2] = -[2|p4|3>[2|p1|\[Zeta]2>+[2|p4|\[Zeta]2>[2|p1|3>*)
(*SpinorChainReduced[Spinor["Zeta",an_,p2_],Spinor[sh3_,an_,p3_]]SpinorChainReduced[Spinor["Helicity",sq_,p2_],Mom[p1_],Mom[p4_],Spinor["Helicity",sq_,p2_]]/;sq\[NotEqual]an:=-SpinorChainReduced[Spinor["Helicity",sq,p2],Mom[p4],Spinor[sh3,an,p3]]SpinorChainReduced[Spinor["Helicity",sq,p2],Mom[p1],Spinor["Zeta",an,p2]]+SpinorChainReduced[Spinor["Helicity",sq,p2],Mom[p4],Spinor["Zeta",an,p2]]SpinorChainReduced[Spinor["Helicity",sq,p2],Mom[p1],Spinor[sh3,an,p3]];
SpinorChainReduced[Spinor[sh3_,an_,p3_],Spinor["Zeta",an_,p2_]]SpinorChainReduced[Spinor["Helicity",sq_,p2_],Mom[p1_],Mom[p4_],Spinor["Helicity",sq_,p2_]]/;sq\[NotEqual]an:=SpinorChainReduced[Spinor["Helicity",sq,p2],Mom[p4],Spinor[sh3,an,p3]]SpinorChainReduced[Spinor["Helicity",sq,p2],Mom[p1],Spinor["Zeta",an,p2]]-SpinorChainReduced[Spinor["Helicity",sq,p2],Mom[p4],Spinor["Zeta",an,p2]]SpinorChainReduced[Spinor["Helicity",sq,p2],Mom[p1],Spinor[sh3,an,p3]];*)
(*[1\[Zeta]4][3|p1|\[Zeta]4> = -<\[Zeta]4|p1|\[Zeta]4][31]+<\[Zeta]4|p1|1][3\[Zeta]4]*)
SpinorChainReduced[Spinor[sh1_,"Square",p1_],Spinor["Zeta","Square",p4_]]SpinorChainReduced[Spinor[sh3_,"Square",p3_],Mom[p1_],Spinor["Zeta","Angle",p4_]]:=-SpinorChainReduced[Spinor["Zeta","Angle",p4],Mom[p1],Spinor["Zeta","Square",p4]]SpinorChainReduced[Spinor[sh3,"Square",p3],Spinor[sh1,"Square",p1]]+SpinorChainReduced[Spinor["Zeta","Angle",p4],Mom[p1],Spinor[sh1,"Square",p1]]SpinorChainReduced[Spinor[sh3,"Square",p3],Spinor["Zeta","Square",p4]];
SpinorChainReduced[Spinor["Zeta","Square",p4_],Spinor[sh1_,"Square",p1_]]SpinorChainReduced[Spinor[sh3_,"Square",p3_],Mom[p1_],Spinor["Zeta","Angle",p4_]]:=SpinorChainReduced[Spinor["Zeta","Angle",p4],Mom[p1],Spinor["Zeta","Square",p4]]SpinorChainReduced[Spinor[sh3,"Square",p3],Spinor[sh1,"Square",p1]]-SpinorChainReduced[Spinor["Zeta","Angle",p4],Mom[p1],Spinor[sh1,"Square",p1]]SpinorChainReduced[Spinor[sh3,"Square",p3],Spinor["Zeta","Square",p4]];
(*<1\[Zeta]4>[\[Zeta]4|p1|3> = <3\[Zeta]4>[\[Zeta]4|p1|1> - <31>[\[Zeta]4|p1|\[Zeta]4>*)
SpinorChainReduced[Spinor[sh1_,"Angle",p1_],Spinor["Zeta","Angle",p4_]]SpinorChainReduced[Spinor["Zeta","Square",p4_],Mom[p1_],Spinor[sh3_,"Angle",p3_]]:=SpinorChainReduced[Spinor[sh3,"Angle",p3],Spinor["Zeta","Angle",p4]]SpinorChainReduced[Spinor["Zeta","Square",p4],Mom[p1],Spinor[sh1,"Angle",p1]]-SpinorChainReduced[Spinor[sh3,"Angle",p3],Spinor[sh1,"Angle",p1]]SpinorChainReduced[Spinor["Zeta","Square",p4],Mom[p1],Spinor["Zeta","Angle",p4]];
SpinorChainReduced[Spinor["Zeta","Angle",p4_],Spinor[sh1_,"Angle",p1_]]SpinorChainReduced[Spinor["Zeta","Square",p4_],Mom[p1_],Spinor[sh3_,"Angle",p3_]]:=-SpinorChainReduced[Spinor[sh3,"Angle",p3],Spinor["Zeta","Angle",p4]]SpinorChainReduced[Spinor["Zeta","Square",p4],Mom[p1],Spinor[sh1,"Angle",p1]]+SpinorChainReduced[Spinor[sh3,"Angle",p3],Spinor[sh1,"Angle",p1]]SpinorChainReduced[Spinor["Zeta","Square",p4],Mom[p1],Spinor["Zeta","Angle",p4]];
(*[2\[Zeta]3][3|p1|\[Zeta]3> = -<\[Zeta]3|p1|\[Zeta]3][32] + <\[Zeta]3|p1|2][3\[Zeta]3]*)
SpinorChainReduced[Spinor[sh2_,"Square",p2_],Spinor["Zeta","Square",p3_]]SpinorChainReduced[Spinor["Helicity","Square",p3_],Mom[p1_],Spinor["Zeta","Angle",p3_]]:=-SpinorChainReduced[Spinor["Zeta","Angle",p3],Mom[p1],Spinor["Zeta","Square",p3]]SpinorChainReduced[Spinor["Helicity","Square",p3],Spinor[sh2,"Square",p2]]+SpinorChainReduced[Spinor["Zeta","Angle",p3],Mom[p1],Spinor[sh2,"Square",p2]]SpinorChainReduced[Spinor["Helicity","Square",p3],Spinor["Zeta","Square",p3]];
SpinorChainReduced[Spinor["Zeta","Square",p3_],Spinor[sh2_,"Square",p2_]]SpinorChainReduced[Spinor["Helicity","Square",p3_],Mom[p1_],Spinor["Zeta","Angle",p3_]]:=SpinorChainReduced[Spinor["Zeta","Angle",p3],Mom[p1],Spinor["Zeta","Square",p3]]SpinorChainReduced[Spinor["Helicity","Square",p3],Spinor[sh2,"Square",p2]]-SpinorChainReduced[Spinor["Zeta","Angle",p3],Mom[p1],Spinor[sh2,"Square",p2]]SpinorChainReduced[Spinor["Helicity","Square",p3],Spinor["Zeta","Square",p3]];
(*[3|p1|\[Zeta]4>[\[Zeta]4|p2|4> = -<4\[Zeta]4>[\[Zeta]4|p2p1|3] + <4|p1|3][\[Zeta]4|p2|\[Zeta]4>*)
SpinorChainReduced[Spinor[sh3_,"Square",p3_],Mom[p1_],Spinor["Zeta","Angle",p4_]]SpinorChainReduced[Spinor["Zeta","Square",p4_],Mom[p2_],Spinor["Helicity","Angle",p4_]]:=-SpinorChainReduced[Spinor["Helicity","Angle",p4],Spinor["Zeta","Angle",p4]]SpinorChainReduced[Spinor["Zeta","Square",p4],Mom[p2],Mom[p1],Spinor[sh3,"Square",p3]]+SpinorChainReduced[Spinor["Helicity","Angle",p4],Mom[p1],Spinor[sh3,"Square",p3]]SpinorChainReduced[Spinor["Zeta","Square",p4],Mom[p2],Spinor["Zeta","Angle",p4]];
Protect[Times];

(*Test*)
(*ReduceSpinorProducts[SpinorChain[Spinor["Spin","Angle",1],Spinor["Zeta","Angle",2]]SpinorChain[Spinor["Helicity","Square",2],Mom[1],Spinor["Helicity","Angle",2]]]
ReduceSpinorProducts[SpinorChain[Spinor["Spin","Square",3],Spinor["Zeta","Square",2]]SpinorChain[Spinor["Helicity","Square",2],Mom[1],Spinor["Helicity","Angle",2]]]*)


(* ::Input::Initialization:: *)
Unprotect[Times];
Unprotect[Plus];
(*[24][1Subscript[\[Zeta], 4]]+[14][2Subscript[\[Zeta], 4]] = [Subscript[\[Zeta], 4]4][12]+2[14][2Subscript[\[Zeta], 4]]*)
(*<24><1Subscript[\[Zeta], 4]>+<14><2Subscript[\[Zeta], 4]> = <Subscript[\[Zeta], 4]4><12>+2<14><2Subscript[\[Zeta], 4]>*)
SpinorChainReduced[Spinor[sh2_,sa_,p2_],Spinor[sh4_,sa_,p4_]]SpinorChainReduced[Spinor[sh1_,sa_,p1_],Spinor["Zeta",sa_,p4_]]+
SpinorChainReduced[Spinor[sh1_,sa_,p1_],Spinor[sh4_,sa_,p4_]]SpinorChainReduced[Spinor[sh2_,sa_,p2_],Spinor["Zeta",sa_,p4_]]:=SpinorChainReduced[Spinor["Zeta",sa,p4],Spinor[sh4,sa,p4]]SpinorChainReduced[Spinor[sh1,sa,p1],Spinor[sh2,sa,p2]]+2SpinorChainReduced[Spinor[sh1,sa,p1],Spinor[sh4,sa,p4]]SpinorChainReduced[Spinor[sh2,sa,p2],Spinor["Zeta",sa,p4]];
(*<34><1\[Zeta]3>+<13><\[Zeta]34> = 2<\[Zeta]34><13> - <\[Zeta]33><14>*)
SpinorChainReduced[Spinor[sh3_,sa_,p3_],Spinor[sh4_,sa_,p4_]]SpinorChainReduced[Spinor[sh1_,sa_,p1_],Spinor["Zeta",sa_,p3_]]+
SpinorChainReduced[Spinor[sh1_,sa_,p1_],Spinor[sh3_,sa_,p3_]]SpinorChainReduced[Spinor["Zeta",sa_,p3_],Spinor[sh4_,sa_,p4_]]:=2SpinorChainReduced[Spinor["Zeta",sa,p3],Spinor[sh4,sa,p4]]SpinorChainReduced[Spinor[sh1,sa,p1],Spinor[sh3,sa,p3]]-SpinorChainReduced[Spinor["Zeta",sa,p3],Spinor[sh3,sa,p3]]SpinorChainReduced[Spinor[sh1,sa,p1],Spinor[sh4,sa,p4]];
(*[1Subscript[\[Zeta], 4]][4|p5|2>+[14][Subscript[\[Zeta], 4]|p5|2> = <2|p5|1][4Subscript[\[Zeta], 4]] + 2[14][Subscript[\[Zeta], 4]|p5|2>*)
SpinorChainReduced[Spinor[sh1_,"Square",p1_],Spinor["Zeta","Square",p4_]]SpinorChainReduced[Spinor[sh4_,"Square",p4_],Mom[p5_],Spinor[sh2_,"Angle",p2_]]+
SpinorChainReduced[Spinor[sh1_,"Square",p1_],Spinor[sh4_,"Square",p4_]]SpinorChainReduced[Spinor["Zeta","Square",p4_],Mom[p5_],Spinor[sh2_,"Angle",p2_]]:=SpinorChainReduced[Spinor[sh2,"Angle",p2],Mom[p5],Spinor[sh1,"Square",p1]]SpinorChainReduced[Spinor[sh4,"Square",p4],Spinor["Zeta","Square",p4]]+2SpinorChainReduced[Spinor[sh1,"Square",p1],Spinor[sh4,"Square",p4]]SpinorChainReduced[Spinor["Zeta","Square",p4],Mom[p5],Spinor[sh2,"Angle",p2]];
(*<1\[Zeta]4>[2|p5|4>+<14>[2|p5|\[Zeta]4> = <4\[Zeta]4>[2|p5|1>+2<14>[2|p5|\[Zeta]4>*)
SpinorChainReduced[Spinor[sh1_,"Angle",p1_],Spinor["Zeta","Angle",p4_]]SpinorChainReduced[Spinor[sh2_,"Square",p2_],Mom[p5_],Spinor[sh4_,"Angle",p4_]]+
SpinorChainReduced[Spinor[sh1_,"Angle",p1_],Spinor[sh4_,"Angle",p4_]]SpinorChainReduced[Spinor[sh2_,"Square",p2_],Mom[p5_],Spinor["Zeta","Angle",p4_]]:=SpinorChainReduced[Spinor[sh4,"Angle",p4],Spinor["Zeta","Angle",p4]]SpinorChainReduced[Spinor[sh2,"Square",p2],Mom[p5],Spinor[sh1,"Angle",p1]]+2SpinorChainReduced[Spinor[sh1,"Angle",p1],Spinor[sh4,"Angle",p4]]SpinorChainReduced[Spinor[sh2,"Square",p2],Mom[p5],Spinor["Zeta","Angle",p4]];
(*<13>[2|p1|\[Zeta]3>-<1\[Zeta]3>[2|p1|3> = <\[Zeta]33>[2|p1|1>*)
SpinorChainReduced[Spinor[sh1_,"Angle",p1_],Spinor["Helicity","Angle",p3_]]SpinorChainReduced[Spinor[sh2_,"Square",p2_],Mom[p5_],Spinor["Zeta","Angle",p3_]]-SpinorChainReduced[Spinor[sh1_,"Angle",p1_],Spinor["Zeta","Angle",p3_]]SpinorChainReduced[Spinor[sh2_,"Square",p2_],Mom[p5_],Spinor["Helicity","Angle",p3_]]:=SpinorChainReduced[Spinor["Zeta","Angle",p3],Spinor["Helicity","Angle",p3]]SpinorChainReduced[Spinor[sh2,"Square",p2],Mom[p5],Spinor[sh1,"Angle",p1]];
Protect[Plus];
Protect[Times];




(* ::Subsubsection::Closed:: *)
(*SpinorTrace [Tr(...p...)]*)


(* ::Input::Initialization:: *)
(*Tr(...p...)*)
SpinorTraceReduced[a___,-Mom[b_],c___]:=-SpinorTraceReduced[a,Mom[b],c];
SpinorTraceReduced[a___,Mom[Multiparticle[b_,c_]],d___]:=SpinorTraceReduced[a,Mom[b],d]+SpinorTraceReduced[a,Mom[c],d];
SpinorTraceReduced[a___,Mom[Multiparticle[b_,c_,ee_]],d___]:=SpinorTraceReduced[a,Mom[b],d]+SpinorTraceReduced[a,Mom[c],d]+SpinorTraceReduced[a,Mom[ee],d];

SpinorTraceReduced[a___,Mom[b_],Mom[b_],c___]:=Mass[b]^2 SpinorTraceReduced[a,c];
SpinorTraceReduced[Mom[b_],a___,c___,Mom[b_]]:=Mass[b]^2 SpinorTraceReduced[a,c];

SpinorTraceReduced[a___,Mom[p1_],Mom[p2_],c___]/;p2<p1:=2MomProd[p1,p2]SpinorTraceReduced[a,c]-SpinorTraceReduced[a,Mom[p2],Mom[p1],c];

(*SpinorTraceReduced[Mom[a_],Mom[b_],Mom[c_],Mom[d_]]:=2MomProd[a,b]MomProd[c,d]-2MomProd[a,c]MomProd[b,d]+2MomProd[a,d]MomProd[b,c]+2I Eps[Mom[a],Mom[b],Mom[c],Mom[d]];*)
SpinorTraceReduced[Mom[a_],Mom[b_]]:=2MomProd[a,b];
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


(* ::Subsubsection::Closed:: *)
(*Mandelstahm*)


ExtractMandelstahm[exp_]:=ExtractMandelstahm[exp,4]
ExtractMandelstahm[exp_,nPoints_]:=Module[{newExp},
newExp=exp//.{
MomProd[i_,j_]/;i!=j:>1/2 (Mandelstahm[i,j]-Mass[i]^2-Mass[j]^2),
PropDen[Mom[i_]+Mom[j_],m_]/;i!=j:>Mandelstahm[i,j]-m^2
};
If[nPoints==4,
newExp=newExp//.{
Mandelstahm[3,4]->Mandelstahm[1,2],
Mandelstahm[2,4]->Mandelstahm[1,3],
Mandelstahm[2,3]->Mandelstahm[1,4]
}];
newExp
];


(* ::Subsubsection::Closed:: *)
(*PropDen*)


PropDen[-Mom[a_]+Mom[b_],m_]/;a<b:=PropDen[Mom[a]-Mom[b],m];
PropDen[-Mom[a_]-Mom[b_],m_]:=PropDen[Mom[a]+Mom[b],m];


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
SpinorChainReduced[Spinor[sh3_,ah_,p3_],Mom[p2_],Mom[p1_],Spinor[sh4_,ah_,p4_]]/;(p3!=p4):>-Mass[p1]^2SpinorChainReduced[Spinor[sh3,ah,p3],Spinor[sh4,ah,p4]]+SpinorChainReduced[Spinor[sh3,ah,p3],Mom[p1]+Mom[p2]-Sum[Mom[momenta[[ii]]],{ii,1,Length[momenta]}],Mom[p1],Spinor[sh4,ah,p4]]
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
SpinorChainReduced[Spinor[sh2_,"Angle",p2_],Spinor[sh4_,"Angle",p4_]]SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p5_],Spinor[sh3_,"Angle",p3_]]/;(p4!=p3&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[IntLines,p1]&&MemberQ[IntLines,p5]&&MemberQ[ExtLines,p3]):>SpinorChainReduced[Spinor[sh2,"Angle",p2],Spinor[sh4,"Angle",p4]]SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5]-Sum[Mom[momenta[[ii]]],{ii,1,Length[momenta]}],Spinor[sh3,"Angle",p3]],
(*<inex>[in|in|ex(>^n)*)
SpinorChainReduced[Spinor[sh2_,"Angle",p2_],Spinor[sh4_,"Angle",p4_]]SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p5_],Spinor[sh3_,"Angle",p3_]]^n_/;(p4!=p3&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[IntLines,p1]&&MemberQ[IntLines,p5]&&MemberQ[ExtLines,p3]):>SpinorChainReduced[Spinor[sh2,"Angle",p2],Spinor[sh4,"Angle",p4]]SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5]-Sum[Mom[momenta[[ii]]],{ii,1,Length[momenta]}],Spinor[sh3,"Angle",p3]]SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5],Spinor[sh3,"Angle",p3]]^(n-1),
(*<inex(>^m)[in|in|ex>*)
SpinorChainReduced[Spinor[sh2_,"Angle",p2_],Spinor[sh4_,"Angle",p4_]]^m_ SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p5_],Spinor[sh3_,"Angle",p3_]]/;(p4!=p3&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[IntLines,p1]&&MemberQ[IntLines,p5]&&MemberQ[ExtLines,p3]):>SpinorChainReduced[Spinor[sh2,"Angle",p2],Spinor[sh4,"Angle",p4]]^m SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5]-Sum[Mom[momenta[[ii]]],{ii,1,Length[momenta]}],Spinor[sh3,"Angle",p3]],
(*<inex(>^m)[in|in|ex(>^n)*)
SpinorChainReduced[Spinor[sh2_,"Angle",p2_],Spinor[sh4_,"Angle",p4_]]^m_ SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p5_],Spinor[sh3_,"Angle",p3_]]^n_/;(p4!=p3&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[IntLines,p1]&&MemberQ[IntLines,p5]&&MemberQ[ExtLines,p3]):>SpinorChainReduced[Spinor[sh2,"Angle",p2],Spinor[sh4,"Angle",p4]]^m SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5]-Sum[Mom[momenta[[ii]]],{ii,1,Length[momenta]}],Spinor[sh3,"Angle",p3]]SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5],Spinor[sh3,"Angle",p3]]^(n-1),
(*<inex>[in|ex|ex> \[Rule] <exex>[in|ex|in> + m<inex>[inex]*)
(*<24>[1|p5|3> = <34>[1|p5|2>-<32>[1|p5|4>*)
SpinorChainReduced[Spinor[sh2_,"Angle",p2_],Spinor[sh4_,"Angle",p4_]]SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p5_],Spinor[sh3_,"Angle",p3_]]/;(p4!=p3&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[IntLines,p1]&&MemberQ[ExtLines,p5]&&MemberQ[ExtLines,p3]):>SpinorChainReduced[Spinor[sh3,"Angle",p3],Spinor[sh4,"Angle",p4]]SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5],Spinor[sh2,"Angle",p2]]-SpinorChainReduced[Spinor[sh3,"Angle",p3],Spinor[sh2,"Angle",p2]]SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5],Spinor[sh4,"Angle",p4]],
(*<inex>[in|ex|ex>^n*)
SpinorChainReduced[Spinor[sh2_,"Angle",p2_],Spinor[sh4_,"Angle",p4_]]SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p5_],Spinor[sh3_,"Angle",p3_]]^n_/;(p4!=p3&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[IntLines,p1]&&MemberQ[ExtLines,p5]&&MemberQ[ExtLines,p3]):>(SpinorChainReduced[Spinor[sh3,"Angle",p3],Spinor[sh4,"Angle",p4]]SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5],Spinor[sh2,"Angle",p2]]-SpinorChainReduced[Spinor[sh3,"Angle",p3],Spinor[sh2,"Angle",p2]]SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5],Spinor[sh4,"Angle",p4]])SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5],Spinor[sh3,"Angle",p3]]^(n-1),
(*<inex>^m[in|ex|ex>*)
SpinorChainReduced[Spinor[sh2_,"Angle",p2_],Spinor[sh4_,"Angle",p4_]]^m_ SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p5_],Spinor[sh3_,"Angle",p3_]]/;(p4!=p3&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[IntLines,p1]&&MemberQ[ExtLines,p5]&&MemberQ[ExtLines,p3]):>SpinorChainReduced[Spinor[sh2,"Angle",p2],Spinor[sh4,"Angle",p4]]^(m-1) (SpinorChainReduced[Spinor[sh3,"Angle",p3],Spinor[sh4,"Angle",p4]]SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5],Spinor[sh2,"Angle",p2]]-SpinorChainReduced[Spinor[sh3,"Angle",p3],Spinor[sh2,"Angle",p2]]SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5],Spinor[sh4,"Angle",p4]]),
(*<inex>^m[in|ex|ex>^n*)
SpinorChainReduced[Spinor[sh2_,"Angle",p2_],Spinor[sh4_,"Angle",p4_]]^m_ SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p5_],Spinor[sh3_,"Angle",p3_]]^n_/;(p4!=p3&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[IntLines,p1]&&MemberQ[ExtLines,p5]&&MemberQ[ExtLines,p3]):>SpinorChainReduced[Spinor[sh2,"Angle",p2],Spinor[sh4,"Angle",p4]]^(m-1) (SpinorChainReduced[Spinor[sh3,"Angle",p3],Spinor[sh4,"Angle",p4]]SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5],Spinor[sh2,"Angle",p2]]-SpinorChainReduced[Spinor[sh3,"Angle",p3],Spinor[sh2,"Angle",p2]]SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5],Spinor[sh4,"Angle",p4]])SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5],Spinor[sh3,"Angle",p3]]^(n-1),
(*[in|ex_notlow|in> \[Rule] [in|ex_low|in>*)
(*[1|p4|2> = -[1|p1+p2+p3|2>*)
SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p5_],Spinor[sh2_,"Angle",p2_]]/;(MemberQ[IntLines,p1]&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p5]&&p5!=ExtLines[[1]]):>SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5]-Sum[Mom[momenta[[ii]]],{ii,1,Length[momenta]}],Spinor[sh2,"Angle",p2]],

(*<inex>[ex|ex|in> \[Rule] <inex>[ex|in|in>*)
(*<24>[1|p5|3> = <24>[1|p5-(p1+p2+p3+p4)|3>*)
SpinorChainReduced[Spinor[sh2_,"Angle",p2_],Spinor[sh4_,"Angle",p4_]]SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p5_],Spinor[sh3_,"Angle",p3_]]/;(p2!=p3&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[ExtLines,p1]&&MemberQ[ExtLines,p5]&&MemberQ[IntLines,p3]):>SpinorChainReduced[Spinor[sh2,"Angle",p2],Spinor[sh4,"Angle",p4]]SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5]-Sum[Mom[momenta[[ii]]],{ii,1,Length[momenta]}],Spinor[sh3,"Angle",p3]],
(*<inex>[ex|ex|in>^n*)
SpinorChainReduced[Spinor[sh2_,"Angle",p2_],Spinor[sh4_,"Angle",p4_]]SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p5_],Spinor[sh3_,"Angle",p3_]]^n_/;(p2!=p3&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[ExtLines,p1]&&MemberQ[ExtLines,p5]&&MemberQ[IntLines,p3]):>SpinorChainReduced[Spinor[sh2,"Angle",p2],Spinor[sh4,"Angle",p4]]SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5]-Sum[Mom[momenta[[ii]]],{ii,1,Length[momenta]}],Spinor[sh3,"Angle",p3]]SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5],Spinor[sh3,"Angle",p3]]^(n-1),
(*<inex>^m[ex|ex|in>*)
SpinorChainReduced[Spinor[sh2_,"Angle",p2_],Spinor[sh4_,"Angle",p4_]]^m_ SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p5_],Spinor[sh3_,"Angle",p3_]]/;(p2!=p3&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[ExtLines,p1]&&MemberQ[ExtLines,p5]&&MemberQ[IntLines,p3]):>SpinorChainReduced[Spinor[sh2,"Angle",p2],Spinor[sh4,"Angle",p4]]^m SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5]-Sum[Mom[momenta[[ii]]],{ii,1,Length[momenta]}],Spinor[sh3,"Angle",p3]],
(*<inex>^m[ex|ex|in>^n*)
SpinorChainReduced[Spinor[sh2_,"Angle",p2_],Spinor[sh4_,"Angle",p4_]]^m_ SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p5_],Spinor[sh3_,"Angle",p3_]]^n_/;(p2!=p3&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[ExtLines,p1]&&MemberQ[ExtLines,p5]&&MemberQ[IntLines,p3]):>SpinorChainReduced[Spinor[sh2,"Angle",p2],Spinor[sh4,"Angle",p4]]^m SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5]-Sum[Mom[momenta[[ii]]],{ii,1,Length[momenta]}],Spinor[sh3,"Angle",p3]]SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5],Spinor[sh3,"Angle",p3]]^(n-1),
(*<inex>[ex|in|in> \[Rule] <inin>[ex|in|ex> + m<inex>[inex]*)
(*<24>[1|p5|3> = <34>[1|p5|2>-<32>[1|p5|4>*)
SpinorChainReduced[Spinor[sh2_,"Angle",p2_],Spinor[sh4_,"Angle",p4_]]SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p5_],Spinor[sh3_,"Angle",p3_]]/;(p2!=p3&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[ExtLines,p1]&&MemberQ[IntLines,p5]&&MemberQ[IntLines,p3]):>SpinorChainReduced[Spinor[sh3,"Angle",p3],Spinor[sh4,"Angle",p4]]SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5],Spinor[sh2,"Angle",p2]]-SpinorChainReduced[Spinor[sh3,"Angle",p3],Spinor[sh2,"Angle",p2]]SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5],Spinor[sh4,"Angle",p4]],
(*<inex>[ex|in|in>^n*)
SpinorChainReduced[Spinor[sh2_,"Angle",p2_],Spinor[sh4_,"Angle",p4_]]SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p5_],Spinor[sh3_,"Angle",p3_]]^n_/;(p2!=p3&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[ExtLines,p1]&&MemberQ[IntLines,p5]&&MemberQ[IntLines,p3]):>(SpinorChainReduced[Spinor[sh3,"Angle",p3],Spinor[sh4,"Angle",p4]]SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5],Spinor[sh2,"Angle",p2]]-SpinorChainReduced[Spinor[sh3,"Angle",p3],Spinor[sh2,"Angle",p2]]SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5],Spinor[sh4,"Angle",p4]])SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5],Spinor[sh3,"Angle",p3]]^(n-1),
(*<inex>^m[ex|in|in>*)
SpinorChainReduced[Spinor[sh2_,"Angle",p2_],Spinor[sh4_,"Angle",p4_]]^m_ SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p5_],Spinor[sh3_,"Angle",p3_]]/;(p2!=p3&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[ExtLines,p1]&&MemberQ[IntLines,p5]&&MemberQ[IntLines,p3]):>SpinorChainReduced[Spinor[sh2,"Angle",p2],Spinor[sh4,"Angle",p4]]^(m-1) (SpinorChainReduced[Spinor[sh3,"Angle",p3],Spinor[sh4,"Angle",p4]]SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5],Spinor[sh2,"Angle",p2]]-SpinorChainReduced[Spinor[sh3,"Angle",p3],Spinor[sh2,"Angle",p2]]SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5],Spinor[sh4,"Angle",p4]]),
(*<inex>^m[ex|in|in>^n*)
SpinorChainReduced[Spinor[sh2_,"Angle",p2_],Spinor[sh4_,"Angle",p4_]]^m_ SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p5_],Spinor[sh3_,"Angle",p3_]]^n_/;(p2!=p3&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[ExtLines,p1]&&MemberQ[IntLines,p5]&&MemberQ[IntLines,p3]):>SpinorChainReduced[Spinor[sh2,"Angle",p2],Spinor[sh4,"Angle",p4]]^(m-1) (SpinorChainReduced[Spinor[sh3,"Angle",p3],Spinor[sh4,"Angle",p4]]SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5],Spinor[sh2,"Angle",p2]]-SpinorChainReduced[Spinor[sh3,"Angle",p3],Spinor[sh2,"Angle",p2]]SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5],Spinor[sh4,"Angle",p4]])SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5],Spinor[sh3,"Angle",p3]]^(n-1),
(*[ex|in_notlow|ex> \[Rule] [ex|in_low|ex>*)
(*[1|p4|2> = -[1|p1+p2+p3|2>*)
SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p5_],Spinor[sh2_,"Angle",p2_]]/;(MemberQ[ExtLines,p1]&&MemberQ[ExtLines,p2]&&MemberQ[IntLines,p5]&&p5!=IntLines[[1]]):>SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5]-Sum[Mom[momenta[[ii]]],{ii,1,Length[momenta]}],Spinor[sh2,"Angle",p2]],

(*[inex][ex|in|in> \[Rule] [inex][ex|ex|in>*)
(*[24][1|p5|3> = [24][1|p5-(p1+p2+p3+p4)|3>*)
SpinorChainReduced[Spinor[sh2_,"Square",p2_],Spinor[sh4_,"Square",p4_]]SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p5_],Spinor[sh3_,"Angle",p3_]]/;(p4!=p1&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[ExtLines,p1]&&MemberQ[IntLines,p5]&&MemberQ[IntLines,p3]):>SpinorChainReduced[Spinor[sh2,"Square",p2],Spinor[sh4,"Square",p4]]SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5]-Sum[Mom[momenta[[ii]]],{ii,1,Length[momenta]}],Spinor[sh3,"Angle",p3]],
(*[inex][ex|in|in>^n*)
SpinorChainReduced[Spinor[sh2_,"Square",p2_],Spinor[sh4_,"Square",p4_]]SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p5_],Spinor[sh3_,"Angle",p3_]]^n_/;(p4!=p1&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[ExtLines,p1]&&MemberQ[IntLines,p5]&&MemberQ[IntLines,p3]):>SpinorChainReduced[Spinor[sh2,"Square",p2],Spinor[sh4,"Square",p4]]SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5]-Sum[Mom[momenta[[ii]]],{ii,1,Length[momenta]}],Spinor[sh3,"Angle",p3]]SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5],Spinor[sh3,"Angle",p3]]^(n-1),
(*[inex]^m[ex|in|in>*)
SpinorChainReduced[Spinor[sh2_,"Square",p2_],Spinor[sh4_,"Square",p4_]]^m_ SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p5_],Spinor[sh3_,"Angle",p3_]]/;(p4!=p1&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[ExtLines,p1]&&MemberQ[IntLines,p5]&&MemberQ[IntLines,p3]):>SpinorChainReduced[Spinor[sh2,"Square",p2],Spinor[sh4,"Square",p4]]^m SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5]-Sum[Mom[momenta[[ii]]],{ii,1,Length[momenta]}],Spinor[sh3,"Angle",p3]],
(*[inex]^m[ex|in|in>^n*)
SpinorChainReduced[Spinor[sh2_,"Square",p2_],Spinor[sh4_,"Square",p4_]]^m_ SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p5_],Spinor[sh3_,"Angle",p3_]]^n_/;(p4!=p1&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[ExtLines,p1]&&MemberQ[IntLines,p5]&&MemberQ[IntLines,p3]):>SpinorChainReduced[Spinor[sh2,"Square",p2],Spinor[sh4,"Square",p4]]^m SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5]-Sum[Mom[momenta[[ii]]],{ii,1,Length[momenta]}],Spinor[sh3,"Angle",p3]]SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5],Spinor[sh3,"Angle",p3]]^(n-1),
(*[inex][ex|ex|in> \[Rule] [exex][in|ex|in> + m[inex]<inex>*)
(*[24][1|p5|3> = -[12][4|p5|3>+[14][2|p5|3>*)
SpinorChainReduced[Spinor[sh2_,"Square",p2_],Spinor[sh4_,"Square",p4_]]SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p5_],Spinor[sh3_,"Angle",p3_]]/;(p4!=p1&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[ExtLines,p1]&&MemberQ[ExtLines,p5]&&MemberQ[IntLines,p3]):>-SpinorChainReduced[Spinor[sh1,"Square",p1],Spinor[sh2,"Square",p2]]SpinorChainReduced[Spinor[sh4,"Square",p4],Mom[p5],Spinor[sh3,"Angle",p3]]+SpinorChainReduced[Spinor[sh1,"Square",p1],Spinor[sh4,"Square",p4]]SpinorChainReduced[Spinor[sh2,"Square",p2],Mom[p5],Spinor[sh3,"Angle",p3]],
(*[inex][ex|ex|in>^n*)
SpinorChainReduced[Spinor[sh2_,"Square",p2_],Spinor[sh4_,"Square",p4_]]SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p5_],Spinor[sh3_,"Angle",p3_]]^n_/;(p4!=p1&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[ExtLines,p1]&&MemberQ[ExtLines,p5]&&MemberQ[IntLines,p3]):>(-SpinorChainReduced[Spinor[sh1,"Square",p1],Spinor[sh2,"Square",p2]]SpinorChainReduced[Spinor[sh4,"Square",p4],Mom[p5],Spinor[sh3,"Angle",p3]]+SpinorChainReduced[Spinor[sh1,"Square",p1],Spinor[sh4,"Square",p4]]SpinorChainReduced[Spinor[sh2,"Square",p2],Mom[p5],Spinor[sh3,"Angle",p3]])SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5],Spinor[sh3,"Angle",p3]]^(n-1),
(*[inex]^m[ex|ex|in>*)
SpinorChainReduced[Spinor[sh2_,"Square",p2_],Spinor[sh4_,"Square",p4_]]^m_ SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p5_],Spinor[sh3_,"Angle",p3_]]/;(p4!=p1&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[ExtLines,p1]&&MemberQ[ExtLines,p5]&&MemberQ[IntLines,p3]):>SpinorChainReduced[Spinor[sh2,"Square",p2],Spinor[sh4,"Square",p4]]^(m-1) (-SpinorChainReduced[Spinor[sh1,"Square",p1],Spinor[sh2,"Square",p2]]SpinorChainReduced[Spinor[sh4,"Square",p4],Mom[p5],Spinor[sh3,"Angle",p3]]+SpinorChainReduced[Spinor[sh1,"Square",p1],Spinor[sh4,"Square",p4]]SpinorChainReduced[Spinor[sh2,"Square",p2],Mom[p5],Spinor[sh3,"Angle",p3]]),
(*[inex]^m[ex|ex|in>^n*)
SpinorChainReduced[Spinor[sh2_,"Square",p2_],Spinor[sh4_,"Square",p4_]]^m_ SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p5_],Spinor[sh3_,"Angle",p3_]]^n_/;(p4!=p1&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[ExtLines,p1]&&MemberQ[ExtLines,p5]&&MemberQ[IntLines,p3]):>SpinorChainReduced[Spinor[sh2,"Square",p2],Spinor[sh4,"Square",p4]]^(m-1) (-SpinorChainReduced[Spinor[sh1,"Square",p1],Spinor[sh2,"Square",p2]]SpinorChainReduced[Spinor[sh4,"Square",p4],Mom[p5],Spinor[sh3,"Angle",p3]]+SpinorChainReduced[Spinor[sh1,"Square",p1],Spinor[sh4,"Square",p4]]SpinorChainReduced[Spinor[sh2,"Square",p2],Mom[p5],Spinor[sh3,"Angle",p3]])SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5],Spinor[sh3,"Angle",p3]]^(n-1),
(*[in|ex_notlow|in> \[Rule] [in|ex_low|in>*)
(*[1|p4|2> = -[1|p1+p2+p3|2>*)
SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p5_],Spinor[sh2_,"Angle",p2_]]/;(MemberQ[IntLines,p1]&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p5]&&p5!=ExtLines[[1]]):>SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5]-Sum[Mom[momenta[[ii]]],{ii,1,Length[momenta]}],Spinor[sh2,"Angle",p2]],

(*[inex][in|ex|ex> \[Rule] [inex][in|in|ex>*)
(*[24][1|p5|3> = [24][1|p5-(p1+p2+p3+p4)|3>*)
SpinorChainReduced[Spinor[sh2_,"Square",p2_],Spinor[sh4_,"Square",p4_]]SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p5_],Spinor[sh3_,"Angle",p3_]]/;(p2!=p1&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[IntLines,p1]&&MemberQ[ExtLines,p5]&&MemberQ[ExtLines,p3]):>SpinorChainReduced[Spinor[sh2,"Square",p2],Spinor[sh4,"Square",p4]]SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5]-Sum[Mom[momenta[[ii]]],{ii,1,Length[momenta]}],Spinor[sh3,"Angle",p3]],
(*[inex][in|ex|ex>^n*)
SpinorChainReduced[Spinor[sh2_,"Square",p2_],Spinor[sh4_,"Square",p4_]]SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p5_],Spinor[sh3_,"Angle",p3_]]^n_/;(p2!=p1&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[IntLines,p1]&&MemberQ[ExtLines,p5]&&MemberQ[ExtLines,p3]):>SpinorChainReduced[Spinor[sh2,"Square",p2],Spinor[sh4,"Square",p4]]SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5]-Sum[Mom[momenta[[ii]]],{ii,1,Length[momenta]}],Spinor[sh3,"Angle",p3]]SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5],Spinor[sh3,"Angle",p3]]^(n-1),
(*[inex]^m[in|ex|ex>*)
SpinorChainReduced[Spinor[sh2_,"Square",p2_],Spinor[sh4_,"Square",p4_]]^m_ SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p5_],Spinor[sh3_,"Angle",p3_]]/;(p2!=p1&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[IntLines,p1]&&MemberQ[ExtLines,p5]&&MemberQ[ExtLines,p3]):>SpinorChainReduced[Spinor[sh2,"Square",p2],Spinor[sh4,"Square",p4]]^m SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5]-Sum[Mom[momenta[[ii]]],{ii,1,Length[momenta]}],Spinor[sh3,"Angle",p3]],
(*[inex]^m[in|ex|ex>^n*)
SpinorChainReduced[Spinor[sh2_,"Square",p2_],Spinor[sh4_,"Square",p4_]]^m_ SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p5_],Spinor[sh3_,"Angle",p3_]]^n_/;(p2!=p1&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[IntLines,p1]&&MemberQ[ExtLines,p5]&&MemberQ[ExtLines,p3]):>SpinorChainReduced[Spinor[sh2,"Square",p2],Spinor[sh4,"Square",p4]]^m SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5]-Sum[Mom[momenta[[ii]]],{ii,1,Length[momenta]}],Spinor[sh3,"Angle",p3]]SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5],Spinor[sh3,"Angle",p3]]^(n-1),
(*[inex][in|in|ex> \[Rule] [inin][ex|in|ex> + m[inex]<inex>*)
(*[24][1|p5|3> = -[12][4|p5|3>+[14][2|p5|3>*)
SpinorChainReduced[Spinor[sh2_,"Square",p2_],Spinor[sh4_,"Square",p4_]]SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p5_],Spinor[sh3_,"Angle",p3_]]/;(p2!=p1&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[IntLines,p1]&&MemberQ[IntLines,p5]&&MemberQ[ExtLines,p3]):>-SpinorChainReduced[Spinor[sh1,"Square",p1],Spinor[sh2,"Square",p2]]SpinorChainReduced[Spinor[sh4,"Square",p4],Mom[p5],Spinor[sh3,"Angle",p3]]+SpinorChainReduced[Spinor[sh1,"Square",p1],Spinor[sh4,"Square",p4]]SpinorChainReduced[Spinor[sh2,"Square",p2],Mom[p5],Spinor[sh3,"Angle",p3]],
(*[inex][in|in|ex>^n*)
SpinorChainReduced[Spinor[sh2_,"Square",p2_],Spinor[sh4_,"Square",p4_]]SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p5_],Spinor[sh3_,"Angle",p3_]]^n_/;(p2!=p1&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[IntLines,p1]&&MemberQ[IntLines,p5]&&MemberQ[ExtLines,p3]):>(-SpinorChainReduced[Spinor[sh1,"Square",p1],Spinor[sh2,"Square",p2]]SpinorChainReduced[Spinor[sh4,"Square",p4],Mom[p5],Spinor[sh3,"Angle",p3]]+SpinorChainReduced[Spinor[sh1,"Square",p1],Spinor[sh4,"Square",p4]]SpinorChainReduced[Spinor[sh2,"Square",p2],Mom[p5],Spinor[sh3,"Angle",p3]])SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5],Spinor[sh3,"Angle",p3]]^(n-1),
(*[inex]^m[in|in|ex>*)
SpinorChainReduced[Spinor[sh2_,"Square",p2_],Spinor[sh4_,"Square",p4_]]^m_ SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p5_],Spinor[sh3_,"Angle",p3_]]/;(p2!=p1&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[IntLines,p1]&&MemberQ[IntLines,p5]&&MemberQ[ExtLines,p3]):>SpinorChainReduced[Spinor[sh2,"Square",p2],Spinor[sh4,"Square",p4]]^(m-1) (-SpinorChainReduced[Spinor[sh1,"Square",p1],Spinor[sh2,"Square",p2]]SpinorChainReduced[Spinor[sh4,"Square",p4],Mom[p5],Spinor[sh3,"Angle",p3]]+SpinorChainReduced[Spinor[sh1,"Square",p1],Spinor[sh4,"Square",p4]]SpinorChainReduced[Spinor[sh2,"Square",p2],Mom[p5],Spinor[sh3,"Angle",p3]]),
(*[inex]^m[in|in|ex>^n*)
SpinorChainReduced[Spinor[sh2_,"Square",p2_],Spinor[sh4_,"Square",p4_]]^m_ SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p5_],Spinor[sh3_,"Angle",p3_]]^n_/;(p2!=p1&&MemberQ[IntLines,p2]&&MemberQ[ExtLines,p4]&&MemberQ[IntLines,p1]&&MemberQ[IntLines,p5]&&MemberQ[ExtLines,p3]):>SpinorChainReduced[Spinor[sh2,"Square",p2],Spinor[sh4,"Square",p4]]^(m-1) (-SpinorChainReduced[Spinor[sh1,"Square",p1],Spinor[sh2,"Square",p2]]SpinorChainReduced[Spinor[sh4,"Square",p4],Mom[p5],Spinor[sh3,"Angle",p3]]+SpinorChainReduced[Spinor[sh1,"Square",p1],Spinor[sh4,"Square",p4]]SpinorChainReduced[Spinor[sh2,"Square",p2],Mom[p5],Spinor[sh3,"Angle",p3]])SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5],Spinor[sh3,"Angle",p3]]^(n-1),
(*[ex|in_notlow|ex> \[Rule] [ex|in_low|ex>*)
(*[1|p4|2> = -[1|p1+p2+p3|2>*)
SpinorChainReduced[Spinor[sh1_,"Square",p1_],Mom[p5_],Spinor[sh2_,"Angle",p2_]]/;(MemberQ[ExtLines,p1]&&MemberQ[ExtLines,p2]&&MemberQ[IntLines,p5]&&p5!=IntLines[[1]]):>SpinorChainReduced[Spinor[sh1,"Square",p1],Mom[p5]-Sum[Mom[momenta[[ii]]],{ii,1,Length[momenta]}],Spinor[sh2,"Angle",p2]],

(*Define a standard form for spinor chains with 2 momentum.*)
(*e.g. <4|p1p2|4> not <4|p1p3|4>*)
(*e.g. <2|p1p3|2> not <2|p1p4|2>*)
(*Very specific to 4-point amplitudes (p1+p2+p3+p4=0).*)
SpinorChainReduced[Spinor[sh4_,sq_,p4_],Mom[p5_],Mom[p6_],Spinor[sh4_,sq_,p4_]]/;(p6-p5>1&&(p6-p4>1||p6-p4<0)):>SpinorChainReduced[Spinor[sh4,sq,p4],Mom[p5],Mom[p6]-Sum[Mom[momenta[[ii]]],{ii,1,Length[momenta]}],Spinor[sh4,sq,p4]],
(*[12][3|p1|4(>^2) = -[12][3|p2|4>[3|p1|4>-[12][3|p3|4>[3|p1|4>-[12][3|p4|4>[3|p1|4>*)
(*<12>[3|p1|4(>^2)*)
SpinorChainReduced[Spinor[sh1_,sa_,p1_],Spinor[sh2_,sa_,p2_]]SpinorChainReduced[Spinor[sh3_,sq_,p3_],Mom[p5_],Spinor[sh4_,an_,p4_]]^2:>SpinorChainReduced[Spinor[sh1,sa,p1],Spinor[sh2,sa,p2]]SpinorChainReduced[Spinor[sh3,sq,p3],Mom[p5],Spinor[sh4,an,p4]]SpinorChainReduced[Spinor[sh3,sq,p3],Mom[p5]-Sum[Mom[momenta[[ii]]],{ii,1,Length[momenta]}],Spinor[sh4,an,p4]],
(*[4|p1|2>[3|p1p2|3] = -[4|p2|2>[3|p1p2|3]-[4|p3|2>[3|p1p2|3]-[4|p4|2>[3|p1p2|3]*)
SpinorChainReduced[Spinor[sh4_,sq_,p4_],Mom[p1_],Spinor[sh2_,an_,p2_]]SpinorChainReduced[Spinor[sh3_,sq_,p3_],Mom[p1_],Mom[p2_],Spinor[sh3_,sq_,p3_]]:>SpinorChainReduced[Spinor[sh4,sq,p4],Mom[p1]-Sum[Mom[momenta[[ii]]],{ii,1,Length[momenta]}],Spinor[sh2,an,p2]]SpinorChainReduced[Spinor[sh3,sq,p3],Mom[p1],Mom[p2],Spinor[sh3,sq,p3]],

(*Massless <pN|...|pN]*)
SpinorChainReduced[Spinor["Helicity","Square",pN_],a___,Spinor["Helicity","Angle",pN_]]:>SpinorTraceReduced[Mom[pN],a],
SpinorChainReduced[Spinor["Helicity","Angle",pN_],a___,Spinor["Helicity","Square",pN_]]:>SpinorTraceReduced[a,Mom[pN]]



};
finalStandardFormRules={
(*If you can't put it in the form [in|ex|in> or [ex|in|ex>, then atleast make momentum in.*)
(*[ex|ex|in>\[Rule][ex|in|in>*)
SpinorChainReduced[Spinor[sh3_,"Square",p3_],Mom[p4_],Spinor[sh2_,"Angle",p2_]]/;(MemberQ[ExtLines,p3]&&MemberQ[ExtLines,p4]&&MemberQ[IntLines,p2]):>SpinorChainReduced[Spinor[sh3,"Square",p3],Mom[p4]-Sum[Mom[momenta[[ii]]],{ii,1,Length[momenta]}],Spinor[sh2,"Angle",p2]],
(*[in|ex|ex>\[Rule][in|in|ex>*)
SpinorChainReduced[Spinor[sh3_,"Square",p3_],Mom[p4_],Spinor[sh2_,"Angle",p2_]]/;(MemberQ[IntLines,p3]&&MemberQ[ExtLines,p4]&&MemberQ[ExtLines,p2]):>SpinorChainReduced[Spinor[sh3,"Square",p3],Mom[p4]-Sum[Mom[momenta[[ii]]],{ii,1,Length[momenta]}],Spinor[sh2,"Angle",p2]]
};
momConsRules3={
MomProd[1,2]->1/2 (Mass[3]^2-Mass[1]^2-Mass[2]^2),
MomProd[2,3]->1/2 (Mass[1]^2-Mass[2]^2-Mass[3]^2),
MomProd[1,3]->1/2 (Mass[2]^2-Mass[1]^2-Mass[3]^2)
};
If[Length[momenta]==4,
momConsRules4={
MomProd[ExtLines[[1]],ExtLines[[2]]]->1/2 (-Mass[ExtLines[[1]]]^2-Mass[ExtLines[[2]]]^2 +Mass[IntLines[[1]]]^2+Mass[IntLines[[2]]]^2)+MomProd[IntLines[[1]],IntLines[[2]]],
MomProd[IntLines[[1]],ExtLines[[1]]]->-Mass[IntLines[[1]]]^2-MomProd[IntLines[[1]],IntLines[[2]]]-MomProd[IntLines[[1]],ExtLines[[2]]],
MomProd[IntLines[[2]],ExtLines[[1]]]->1/2 (-Mass[IntLines[[2]]]^2-Mass[ExtLines[[1]]]^2+Mass[IntLines[[1]]]^2+Mass[ExtLines[[2]]]^2)+MomProd[IntLines[[1]],ExtLines[[2]]],
MomProd[IntLines[[2]],ExtLines[[2]]]->-MomProd[IntLines[[1]],ExtLines[[2]]]-MomProd[ExtLines[[1]],ExtLines[[2]]]-Mass[ExtLines[[2]]]^2
};(*Print[momConsRules4];*)
];
onShellRules={
MomProd[IntLines[[1]],IntLines[[2]]]:>1/2 (Mass[Multiparticle[IntLines[[1]],IntLines[[2]]]]^2-Mass[IntLines[[1]]]^2-Mass[IntLines[[2]]]^2)
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


(* ::Subsubsection:: *)
(*xFactor*)


Options[ReducexFactors]={Momenta->{1,2,3,4},ChannelMomenta->{1,4},ChannelMass->Mchannel};
Options[xFactorReduced]={Momenta->{1,2,3,4},ChannelMomenta->{1,4},ChannelMass->Mchannel};
Options[xTildeFactorReduced]={Momenta->{1,2,3,4},ChannelMomenta->{1,4},ChannelMass->Mchannel};


ReducexFactors[amp_,OptionsPattern[]]:=Module[{options,momenta,channelMomenta,complementMomenta},
momenta=OptionValue[Momenta];
channelMomenta=Sort[OptionValue[ChannelMomenta]];
complementMomenta=Sort[Complement[momenta,channelMomenta]];
options={Momenta->momenta,ChannelMomenta->channelMomenta,ChannelMass->OptionValue[ChannelMass]};

ReduceSpinorProducts[ReduceSpinorProducts[ReduceSpinorProducts[ReduceSpinorProducts[ReduceSpinContractions[amp//.{
xFactor[i_,j_]/;(Head[i]==Integer&&Head[j]==Integer):>xFactorReduced[i,j],

xTildeFactor[i_,j_]/;(Head[i]==Integer&&Head[j]==Integer):>xTildeFactorReduced[i,j],

xFactor[a_,Multiparticle[b__]]:>xFactorReduced[a,Multiparticle[b],options],

xFactor[Multiparticle[b__],a_]:>-xFactorReduced[a,Multiparticle[b],options],

xFactor[Multiparticle[a__],Multiparticle[b__]]:>xFactorReduced[a,Multiparticle[b],options],

xTildeFactor[a_,Multiparticle[b__]]:>xTildeFactorReduced[a,Multiparticle[b],options],

xTildeFactor[Multiparticle[b__],a_]:>-xTildeFactorReduced[a,Multiparticle[b],options],

xTildeFactor[Multiparticle[a__],Multiparticle[b__]]:>xTildeFactorReduced[Multiparticle[a],Multiparticle[b],options]
}]]/.{
SpinorChain[Spinor[sp_,"Square",p1_],Mom[xk],Spinor[sp_,"Angle",p2_]]SpinorChain[Spinor[sp_,"Square",p3_],Mom[xq],Spinor[sp_,"Angle",p4_]]:>-SpinorChain[Spinor[sp,"Angle",p4],Spinor[sp,"Angle",p2]]SpinorChain[Spinor[sp,"Square",p3],Mom[xq],Mom[xk],Spinor[sp,"Square",p1]]+SpinorChain[Spinor[sp,"Angle",p4],Mom[xk],Spinor[sp,"Square",p1]]SpinorChain[Spinor[sp,"Square",p3],Mom[xq],Spinor[sp,"Angle",p2]]
}]/.{
SpinorChain[Spinor[sp_,"Square",p1_],Mom[xq],Mom[xk],Spinor[sp_,"Square",p2_]]:>2MomProd[xq,xk]SpinorChain[Spinor[sp,"Square",p1],Spinor[sp,"Square",p2]]-SpinorChain[Spinor[sp,"Square",p1],Mom[xk],Mom[xq],Spinor[sp,"Square",p2]]
}]/.{
SpinorChain[Spinor[sp1_,sa1_,channelMomenta[[1]]],Mom[xk],Spinor[sp2_,sa2_,channelMomenta[[2]]]]->SpinorChain[Spinor[sp1,sa1,channelMomenta[[1]]],Mom[Multiparticle[channelMomenta[[1]],channelMomenta[[2]]]],Spinor[sp2,sa2,channelMomenta[[2]]]],
SpinorChain[Spinor[sp1_,sa1_,channelMomenta[[2]]],Mom[xk],Spinor[sp2_,sa2_,channelMomenta[[1]]]]->SpinorChain[Spinor[sp1,sa1,channelMomenta[[2]]],Mom[Multiparticle[channelMomenta[[1]],channelMomenta[[2]]]],Spinor[sp2,sa2,channelMomenta[[1]]]],
SpinorChain[Spinor[sp1_,sa1_,complementMomenta[[1]]],Mom[xk],Spinor[sp2_,sa2_,complementMomenta[[2]]]]->SpinorChain[Spinor[sp1,sa1,complementMomenta[[1]]],Mom[Multiparticle[complementMomenta[[1]],complementMomenta[[2]]]],Spinor[sp2,sa2,complementMomenta[[2]]]],
SpinorChain[Spinor[sp1_,sa1_,complementMomenta[[2]]],Mom[xk],Spinor[sp2_,sa2_,complementMomenta[[1]]]]->SpinorChain[Spinor[sp1,sa1,complementMomenta[[2]]],Mom[Multiparticle[complementMomenta[[1]],complementMomenta[[2]]]],Spinor[sp2,sa2,complementMomenta[[1]]]]
}]
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
xFactorReduced[i_,j_]SpinorChain[Spinor["Spin","Angle",i_],Spinor["Spin","Angle",j_]]:=(SpinorChain[Spinor["Spin","Angle",j],Spinor["Helicity","Angle",xq]]SpinorChain[Spinor["Spin","Square",i],Spinor["Helicity","Square",xk]]+SpinorChain[Spinor["Spin","Angle",i],Spinor["Helicity","Angle",xq]]SpinorChain[Spinor["Spin","Square",j],Spinor["Helicity","Square",xk]])/SpinorChain[Spinor["Helicity","Angle",xq],Spinor["Helicity","Angle",xk]];
xTildeFactorReduced[i_,j_]SpinorChain[Spinor["Spin","Square",i_],Spinor["Spin","Square",j_]]:=(SpinorChain[Spinor["Spin","Square",j],Spinor["Helicity","Square",xq]]SpinorChain[Spinor["Spin","Angle",i],Spinor["Helicity","Angle",xk]]+SpinorChain[Spinor["Spin","Square",i],Spinor["Helicity","Square",xq]]SpinorChain[Spinor["Spin","Angle",j],Spinor["Helicity","Angle",xk]])/SpinorChain[Spinor["Helicity","Square",xq],Spinor["Helicity","Square",xk]];
Protect[Times];


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
ReverseMomentum[amp_,pi_]:=amp/.{
MomProd[pi,pj_]:>-MomProdL[pi,pj],
MomProd[pj_,pi]:>-MomProdR[pj,pi],
MomProdR[pi,pj_]:>-MomProdLR[pi,pj],
MomProdL[pj_,pi]:>-MomProdLR[pj,pi],
SpinorChain[a__,Mom[pi],b__]:>-SpinorChain[a,MomR[pi],b],
SpinorChain[Spinor["Helicity","Angle",pi],a__]:>-SpinorChain[HelicitySpinorR["Angle",pi],a],
SpinorChain[a__,Spinor["Helicity","Angle",pi]]:>-SpinorChain[a,HelicitySpinorR["Angle",pi]],
SpinorChain[Spinor["Spin","Angle",pi],a__]:>-SpinorChain[SpinSpinorR["Angle",pi],a],
SpinorChain[a__,Spinor["Spin","Angle",pi]]:>-SpinorChain[a,SpinSpinorR["Angle",pi]],
SpinorChain[Spinor["Spin",ul_,"Angle",pi,ii_],a__]:>-SpinorChain[SpinSpinorR[ul,"Angle",pi,ii],a],
SpinorChain[a__,Spinor["Spin",ul_,"Angle",pi,ii_]]:>-SpinorChain[a,SpinSpinorR[ul,"Angle",pi,ii]],
PropDen[a__+Mom[pi],b__]:>PropDen[a-MomR[pi],b],
PropDen[a__-Mom[pi],b__]:>PropDen[a+MomR[pi],b]
}/.{
MomProdL[ps__]:>MomProd[ps],
MomProdR[ps__]:>MomProd[ps],
MomProdLR[ps__]:>MomProd[ps],
MomR[ps_]:>Mom[ps],
HelicitySpinorR[st__]:>Spinor["Helicity",st],
SpinSpinorR[st__]:>Spinor["Spin",st]
}


(* ::Subsubsection::Closed:: *)
(*PermuteParticles*)


(* ::Input::Initialization:: *)
PermuteParticles[amp_,plist_List]:=Module[{pTmp,ampNew=amp},
ampNew=ampNew//.{
Mom[plist[[-1]]]:>Mom[pTmp],
MomProd[a___,plist[[-1]],b___]:>MomProd[a,pTmp,b],
SpinorChain[a___,Spinor[b__,plist[[-1]]],c___]:>SpinorChain[a,Spinor[b,pTmp],c],
Mandelstahm[a___,plist[[-1]],b___]:>Mandelstahm[a,pTmp,b]
};
Do[
ampNew=ampNew//.{
Mom[plist[[i]]]:>Mom[plist[[i+1]]],
MomProd[a___,plist[[i]],b___]:>MomProd[a,plist[[i+1]],b],
SpinorChain[a___,Spinor[b__,plist[[i]]],c___]:>SpinorChain[a,Spinor[b,plist[[i+1]]],c],
Mandelstahm[a___,plist[[i]],b___]:>Mandelstahm[a,plist[[i+1]],b]
};
,{i,Length[plist]-1,1,-1}];
ampNew=ampNew//.{
Mom[pTmp]:>Mom[plist[[1]]],
MomProd[a___,pTmp,b___]:>MomProd[a,plist[[1]],b],
SpinorChain[a___,Spinor[b__,pTmp],c___]:>SpinorChain[a,Spinor[b,plist[[1]]],c],
Mandelstahm[a___,pTmp,b___]:>Mandelstahm[a,plist[[1]],b]
};
ampNew
];


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
SymmetrizeSpin[amp_,part_,indices_]:=Module[{tmp,replacementRules={}},
(*Print["Reminder: SymmetrizeSpin only works on implicit indices."];*)
Which[
(*Spin 2*)
Length[indices]==4,
replacementRules={
SpinorChain[Spinor["Spin",type1_,part],b1___,Spinor["Spin",type2_,part]]SpinorChain[Spinor["Spin",type3_,part],b2___,Spinor["Spin",type4_,part]]:>(1/24 SpinorChain[Spinor["Spin","Upper",type1,part,#[[1]]],b1,Spinor["Spin","Upper",type2,part,#[[2]]]]SpinorChain[Spinor["Spin","Upper",type3,part,#[[3]]],b2,Spinor["Spin","Upper",type4,part,#[[4]]]]&/@Permutations[indices]/.List->Plus),
SpinorChain[Spinor["Spin",type1_,part],b___,Spinor["Spin",type2_,part]]SpinorChain[a3___,Spinor["Spin",type3_,part],b3___]SpinorChain[a4___,Spinor["Spin",type4_,part],b4___]:>(1/24 SpinorChain[Spinor["Spin","Upper",type1,part,#[[1]]],b,Spinor["Spin","Upper",type2,part,#[[2]]]]SpinorChain[a3,Spinor["Spin","Upper",type3,part,#[[3]]],b3]SpinorChain[a4,Spinor["Spin","Upper",type4,part,#[[4]]],b4]&/@Permutations[indices]/.List->Plus),
SpinorChain[a1___,Spinor["Spin",type1_,part],b1___]SpinorChain[a2___,Spinor["Spin",type2_,part],b2___]SpinorChain[a3___,Spinor["Spin",type3_,part],b3___]SpinorChain[a4___,Spinor["Spin",type4_,part],b4___]:>(1/24 SpinorChain[a1,Spinor["Spin","Upper",type1,part,#[[1]]],b1]SpinorChain[a2,Spinor["Spin","Upper",type2,part,#[[2]]],b2]SpinorChain[a3,Spinor["Spin","Upper",type3,part,#[[3]]],b3]SpinorChain[a4,Spinor["Spin","Upper",type4,part,#[[4]]],b4]&/@Permutations[indices]/.List->Plus)};,
(*Spin 3/2*)
Length[indices]==3,
replacementRules={
SpinorChain[Spinor["Spin",type1_,part],c___,Spinor["Spin",type2_,part]]SpinorChain[a3___,Spinor["Spin",type3_,part],b3___]:>(1/6 SpinorChain[Spinor["Spin","Upper",type1,part,#[[1]]],c,Spinor["Spin","Upper",type2,part,#[[2]]]]SpinorChain[a3,Spinor["Spin","Upper",type3,part,#[[3]]],b3]&/@Permutations[indices]/.List->Plus),
SpinorChain[a1___,Spinor["Spin",type1_,part],b1___]SpinorChain[a2___,Spinor["Spin",type2_,part],b2___]SpinorChain[a3___,Spinor["Spin",type3_,part],b3___]:>(1/6 SpinorChain[a1,Spinor["Spin","Upper",type1,part,#[[1]]],b1]SpinorChain[a2,Spinor["Spin","Upper",type2,part,#[[2]]],b2]SpinorChain[a3,Spinor["Spin","Upper",type3,part,#[[3]]],b3]&/@Permutations[indices]/.List->Plus)};,
(*Spin 1*)
Length[indices]==2,
replacementRules={
SpinorChain[Spinor["Spin",type1_,part],d___,Spinor["Spin",type2_,part]]:>(1/2 SpinorChain[Spinor["Spin","Upper",type1,part,#[[1]]],d,Spinor["Spin","Upper",type2,part,#[[2]]]]&/@Permutations[indices]/.List->Plus),
SpinorChain[b___,Spinor["Spin",type1_,part],d___]SpinorChain[e___,Spinor["Spin",type2_,part],f___]:> (1/2 SpinorChain[b,Spinor["Spin","Upper",type1,part,#[[1]]],d]SpinorChain[e,Spinor["Spin","Upper",type2,part,#[[2]]],f]&/@Permutations[indices]/.List->Plus),
SpinorChain[b___,Spinor["Spin",type_,part],d___]^2:> (1/2 SpinorChain[b,Spinor["Spin","Upper",type,part,#[[1]]],d]SpinorChain[b,Spinor["Spin","Upper",type,part,#[[2]]],d]&/@Permutations[indices]/.List->Plus)
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
