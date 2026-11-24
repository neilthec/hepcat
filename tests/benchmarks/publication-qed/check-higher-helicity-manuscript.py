"""Read back the seven-, eight- and nine-point helicity displays from LaTeX.

Give the QED draft directory and the directory of constructive *.short.json and
*.json polynomials. Use --expand for an additional exact expansion of the
external signed permutations against the unreduced constructive coefficients.
"""
import argparse
from importlib import import_module
import json
import re
from pathlib import Path

import sympy as sp

tex = import_module("check-six-point-manuscript")
orbit = import_module("check-contracted-symmetry")
four_pairs = import_module("check-four-pair-spinors")

SECTIONS = {7: "seven-point", 8: "eight-point-reconstructed", 9: "nine-point-reconstructed"}


def expect(condition, message):
    if not condition:
        raise ValueError(message)


def compact(source):
    return re.sub(r"\s+", "", source)


def coupling_value(scalar):
    match=re.fullmatch(r"(\d*)(\\sqrt\{2\})?e\^\{(\d+)\}",scalar)
    expect(match is not None, "Unrecognized printed coupling "+scalar)
    return int(match[1] or 1)*(sp.sqrt(2) if match[2] else 1)*sp.Symbol("e")**int(match[3])


def check_helicities(source, n, k):
    photons = list(range(2*k+1, n+1))
    count = len(photons)
    rows = [compact(line) for line in source.splitlines() if line.startswith(r"\M^")]
    expect(len(rows) == 2*(count+1), f"Wrong number of displayed amplitudes at {n}-{k}")
    for index, row in enumerate(rows):
        minus = index//2
        word = "+"*(count-minus) + "-"*minus
        physical = index%2 == 0
        gauge = "phys" if physical else "spur"
        label = f"hel-{n}-{k}-{minus}-{gauge}"
        expect(row.startswith(r"\M^{"+chr(92)+gauge+"}_{"+word+r"}&=\frac{")
               and r"\label{"+label+"}" in row, f"Wrong printed amplitude or label at {label}")
        scalar, rest = row.split(r"\frac{")[1].split("}{", 1)
        value = coupling_value(scalar)
        tex.same(value, sp.sqrt(2)**count * sp.Symbol("e")**(n-2), label+" coupling")
        den, matrix = rest.split(r"}\,B_{"+str(count)+r"\gamma}\!\begin{pmatrix}")
        matrix = matrix.split(r"\end{pmatrix}")[0]
        top, bottom = (r.split("&") for r in matrix.split(r"\\"))
        expect(len(top)==len(bottom)==count, label+" column count")
        denominator = sp.Integer(1)
        for col,(leg,hel) in enumerate(zip(photons,word)):
            next_leg = photons[(col+1)%count]
            a,b = rf"\qa{{{leg}}}",rf"\qb{{{leg}}}"
            if physical or count==1:
                momentum = "p_1" if physical else "(p_1+p_2)"
                required=(b+momentum,b) if hel=="+" else (a,a+momentum)
                d=sp.Symbol(f"t1{leg}")
                if not physical:
                    d+=sp.Symbol(f"t2{leg}")
            else:
                required=(rf"\qa{{{next_leg}}}",b) if hel=="+" else (a,rf"\qb{{{next_leg}}}")
                d=sp.Symbol(f"<{next_leg}{leg}>" if hel=="+" else f"[{next_leg}{leg}]")
            expect((top[col],bottom[col])==required, f"{label}, external spinors on photon {leg}")
            denominator *= d
        tex.same(tex.read_tex(den),denominator,label+" reference denominator")


def polynomial(source, numerator):
    actual=tex.read_tex(source.replace(r"\quad",""))
    expected=sum(c*sp.prod(sp.Symbol(w) for w in words) for c,*words in numerator)
    tex.same(actual,expected,"factored spinor numerator")


def check_fractions(source,data):
    # Check the printed hatted and massive endpoints before interpreting any bracket.
    es=[i for i,x in enumerate(data["particles"],1) if x=="e"]
    ps=[i for i,x in enumerate(data["particles"],1) if x=="E"]
    photons=[i for i,x in enumerate(data["particles"],1) if x=="A.+"]
    for style,leg in re.findall(r"\\(widehat|mathbf)\{(\d+)\}",source):
        expect((style=="widehat") == (int(leg) in photons), "Photon endpoint marked as massive or vice versa")
    rows=re.findall(r"\\begin\{align\*\}(.*?)\\end\{align\*\}",source,re.S)
    expect(len(rows)==len(data["fractions"]),"Missing or duplicate propagator fraction")
    operators="".join(r"\mathcal"+op+"_{"+''.join(map(str,g))+"}"
                      for op,g in [("A",es),("A",ps),("S",photons)] if len(g)>1)
    outer=r"B_{"+str(len(photons))+r"\gamma}="+operators+r"(1+\mathcalC)"
    expected_sum=r"\,F_1" if len(rows)==1 else r"\sum_{j=1}^{"+str(len(rows))+"}F_j"
    if len(rows)>1:
        expect(outer+expected_sum+"." in compact(source),"Incorrect external permutation or chirality sum")
    for j,(body,f) in enumerate(zip(rows,data["fractions"]),1):
        head,num=body.split(f"N_{{{j}}}={{}}&",1)
        head=compact(head)
        if len(rows)==1:
            prefix=outer.replace("=","={}&")
            expect(head.startswith(prefix),"Incorrect external permutation or chirality sum")
            head=f"F_{{{j}}}={{}}&"+head[len(prefix):]
        c,exchanges,den=re.fullmatch(r"F_\{"+str(j)+r"\}=\{\}&(.*?)((?:\(1[+-]P_\{\d+\}\))+)"
                            +r"\\!\\left\(\\frac\{N_\{"+str(j)+r"\}\}\{(.*?)\}\\right\),\\\\\*",head).groups()
        coeff=tex.read_tex(c.replace(r"\tfrac",r"\frac")) if c else 1
        expect(coeff==sp.Rational(*f["factor"]),"Incorrect rational weight")
        expected_exchanges=[("+" if x>0 else "-",f"{a}{b}") for a,b,x in zip(es,ps,f["exchanges"])]
        expect(re.findall(r"\(1([+-])P_\{(\d+)\}\)",exchanges)==expected_exchanges,"Incorrect charged exchange")
        tex.same(tex.read_tex(den),sp.prod(sp.Symbol(d) for d in f["poles"]),"propagator denominator")
        polynomial(num,f["terms"])


def projected_numerator(rows, project, include_partner):
    """Project by literal endpoint chirality. Momenta always keep their labels."""
    value = 0
    for c,*words in rows:
        for flip in range(1+include_partner):
            term = c
            for w in words:
                if w == "m":
                    term *= sp.Symbol("m")
                    continue
                a,b = (w[0]=="<")^flip, (w[-1]==">")^flip
                i,j=project(int(w[1]),a),project(int(w[-2]),b)
                if len(w)==4 and i==j:
                    term=0
                    break
                term *= sp.Symbol(("<" if a else "[")+str(i)+w[2:-2]+str(j)+(">" if b else "]"))
            value += term
    return value


def check_mixed_example(source, data):
    n=len(data["particles"])
    photons=[i for i,s in enumerate(data["particles"],1) if s=="A.+"]
    number,word,body=re.search(r"N_\{(\d+)\}\^\{([+-]+)\}=\{\}&(.*?)\\end\{align\*\}",source,re.S).groups()
    expect(word=="+"*((len(photons)+1)//2)+"-"*(len(photons)//2),"Wrong mixed numerator helicities")
    def project(i,angle):
        if i in photons and (angle==(word[photons.index(i)]=="+")):
            return photons[0] if i==n else i+1
        return i
    rows=data["fractions"][int(number)-1]["terms"]
    actual=tex.read_tex(body.replace(r"\quad",""))
    tex.same(actual,projected_numerator(rows,project,False),"fully specialized mixed-helicity numerator")


def check_one_minus(source, data, export):
    n=len(data["particles"])
    expr=tex.equation(source,"eq:one-minus-ordered-"+str(n))
    word="+"*(n-3)+"-"
    formula=compact(expr)
    expect(r"\M^{\spur}_{"+word+"}" in formula,"Incorrect helicity in one-minus sum")
    pref,den=formula.split(r"\frac{")[1].split(r"}\sum_")[0].split("}{")
    tex.same(coupling_value(pref),sp.Symbol("e")**(n-2)*sp.sqrt(2)**(n-2)/2,"one-minus scalar")
    reference=sp.Symbol(f"[3{n}]")*sp.prod(sp.Symbol(f"<{n}{i}>") for i in range(3,n))
    tex.same(tex.read_tex(den),reference,"one-minus reference normalization")
    frac,=data["fractions"]
    names=re.search(r"\\sum_\{([a-z,]+)\}",formula)[1].split(",")
    expect(len(names)==len(set(names))==n-2,"Wrong range of photon permutations")
    trans=dict(zip(names,map(str,range(3,n+1))))
    sign=frac["exchanges"][0]
    expect(("(1+P_{12})" if sign==1 else "(1-P_{12})") in formula,"One-minus charged exchange")
    denominator=formula.split(r"\frac{N}{")[1].split(r"}\right)")[0]
    for token in re.findall(r"\\tau_\{(.*?)\}",denominator):
        expect(all(c in trans or c in "12" for c in token),"Invalid momentum subset")
    den=re.sub(r"(\\tau_\{)([^}]+)}",lambda m:m[1]+"".join(trans.get(c,c) for c in m[2])+"}",denominator)
    tex.same(tex.read_tex(den),sp.prod(sp.Symbol(d) for d in frac["poles"]),"one-minus physical propagators")
    body=source.split("N={}&")[1].split(r"\end{align*}")[0].replace(r"\quad","")
    body=re.sub(r"\\bar\{([a-z])\}",lambda m:trans[m[1]],body)
    body=re.sub(r"\(([12a-z]+)\)",lambda m:"("+"".join(trans.get(c,c) for c in m[1])+")",body)
    actual=sp.expand(tex.read_tex(body))
    expected=projected_numerator(frac["terms"],lambda i,a:n if a and i>2 else i,True)
    tex.same(actual,expected,"ordinary external spinors in the one-minus permutation sum")
    if export:
        export.mkdir(parents=True,exist_ok=True)
        with (export/f"{n}-minus.spinors").open("w") as f:
            print(sign, *frac["poles"],file=f)
            for monomial in sp.Add.make_args(actual):
                c,product=monomial.as_coeff_Mul()
                words=[str(var) for var,power in product.as_powers_dict().items() for _ in range(int(power))]
                print(c,*words,file=f)


def check_handwritten_three(section, data):
    rows=re.search(r"\\begin\{align\}\s*N_1=(.*?)\\end\{align\}",section,re.S)
    expect(rows is not None, "Missing three-pair numerator equations")
    numerators = re.split(r",\\\\\s*N_[23]=", rows[1])
    expect(len(numerators) == len(data["fractions"]) == 3, "Seven-point three-pair numerator count")
    for body,f in zip(numerators,data["fractions"]):
        polynomial(body,f["terms"])
    main=compact(tex.equation(section,"eq:seven-contracted"))
    for i,f in enumerate(data["fractions"],1):
        denominator="".join((r"\tau" if p[0]=="t" else "s")+"_{"+p[1:]+"}" for p in f["poles"])
        expect(r"\frac{"+str(f["factor"][0])+r"(1+\mathcalC)N_"+str(i)+"}{"+denominator+"}" in main,
               f"Seven-point three-pair fraction {i}")
    for term in (r"\mathcalA_{135}\mathcalA_{246}(1+P_{12})", "(1+P_{34})(1+P_{56})", "(1-P_{34})(1-P_{56})"):
        expect(term in main,"Seven-point charged exchanges")


def check_handwritten_four(section):
    expect(r"\M^{\phys}=\M^{\spur}=T_1+T_2." in section,"Missing photon-free common amplitude")
    for i,(label,terms,den,ops) in enumerate([
            ("three-pair-poles",four_pairs.first_numerator,"s12 s34 s56 t127 t348","(1+P_{12})(1+P_{34})(1+P_{56})"),
            ("four-particle-pole",four_pairs.second_numerator,"s12 s34 s1256 t125 t347","(1+P_{12})(1+P_{34})(1-P_{56})(1-P_{78})")],1):
        expr=tex.equation(section,"eq:eight-pairs-"+label)
        body=expr.split(r"\bigg\{")[1].split(r"+(\langle")[0]
        expected=[[c,*w.replace("m^2","m m").split()] for c,w in terms]
        polynomial(body,expected)
        head=compact(expr.split(r"\bigg\{")[0])
        expect(ops in head and r"\mathrm{sgn}(\pi)\mathrm{sgn}(\sigma)" in head,"Four-pair exchanges")
        expect(f"T_{i}={{}}&{8 if i==1 else 4}e^6" in head,"Four-pair normalization")
        denom=re.search(r"\\frac\{1\}\{(.*?)\}\Z",head)[1]
        tex.same(tex.read_tex(denom),sp.prod(sp.Symbol(d) for d in den.split()),"Four-pair propagators")


def verify(paper, tables, expand, export=None):
    families=words=0
    main=(paper/"main.tex").read_text().split(r"\appendix")[0]
    for n,filename in SECTIONS.items():
        expect(r"\input{"+filename+"}" in main,"Section is outside the main paper")
        section=(paper/(filename+".tex")).read_text()
        for k in range(1,n//2+1):
            tag=f"{n}-{k}"
            data=json.loads((tables/(tag+".short.json")).read_text())
            expect(r"\label{family-"+tag+"}" in section,"Missing charged family "+tag)
            nphot=n-2*k
            if k==1:
                inc=f"families/one-minus-{n}"
                expect(r"\input{"+inc+"}" in section,"Missing ordinary one-minus sum")
                check_one_minus((paper/(inc+".tex")).read_text(),data,export)
            if (n,k) in ((7,2),(8,3),(9,3)):
                inc=f"families/mixed-example-{tag}"
                expect(r"\input{"+inc+"}" in section,"Missing explicit mixed numerator")
                check_mixed_example((paper/(inc+".tex")).read_text(),data)
            if nphot:
                inc="families/helicities-"+tag
                expect(r"\input{"+inc+"}" in section,"Missing helicity equations "+tag)
                check_helicities((paper/(inc+".tex")).read_text(),n,k)
                if (n,k)==(7,3):
                    check_handwritten_three(section,data)
                else:
                    inc="families/polynomial-"+tag
                    expect(r"\input{"+inc+"}" in section,"Missing local polynomial "+tag)
                    check_fractions((paper/(inc+".tex")).read_text(),data)
            else:
                check_handwritten_four(section)
            if expand:
                orbit.check(json.loads((tables/(tag+".json")).read_text()),data)
            words+=1+nphot
            families+=1
    print(f"Verified {families} charge sectors, {words} helicity multisets and both pole choices.")


if __name__=="__main__":
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument("paper",type=Path)
    p.add_argument("tables",type=Path)
    p.add_argument("--expand",action="store_true")
    p.add_argument("--export-one-minus",type=Path,metavar="DIRECTORY")
    args=p.parse_args()
    verify(args.paper,args.tables,args.expand,args.export_one_minus)
