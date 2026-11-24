"""Typeset the constructive seven-, eight- and nine-point photon helicities.

Inputs are the contracted, coefficient-checked *.short.json polynomials.
Outputs are LaTeX fragments, one per charged family. The two rows of each
argument contain ordinary external angle and square bras in photon-label order.
"""
import argparse
from collections import Counter
from functools import lru_cache
from importlib import import_module
from pathlib import Path
import json
import math
import re

spin = import_module("render-contracted-catalog")


def factors(words):
    counts = Counter(words)
    order = sorted(counts, key=lambda s: (s != "m", s))
    return [("atom", w, counts[w]) for w in order]


def times(parts):
    coefficient, mass_power, ordered = 1, 0, []
    for part in parts:
        for item in (part[1] if part[0] == "mul" else (part,)):
            if item[0] == "int":
                coefficient *= item[1]
            elif item[:2] == ("atom", "m"):
                mass_power += item[2]
            else:
                ordered.append(item)
    if mass_power:
        ordered.insert(0, ("atom", "m", mass_power))
    if coefficient != 1 or not ordered:
        ordered.insert(0, ("int", coefficient))
    return ordered[0] if len(ordered) == 1 else ("mul", tuple(ordered))


def plus(parts):
    return parts[0][1] if len(parts) == 1 and parts[0][0] == 1 else ("sum", tuple(parts))


def remove(words, common):
    return tuple(sorted((Counter(words) - common).elements()))


def factor_polynomial(rows):
    """Extract common products by ordinary distributivity in the scalar brackets."""
    @lru_cache(None)
    def factor(terms):
        common = Counter(terms[0][1])
        for _, words in terms[1:]:
            common &= Counter(words)
        gcd = math.gcd(*(abs(c) for c, _ in terms))
        if terms[0][0] < 0:
            gcd = -gcd
        if common or gcd != 1:
            rest = tuple((c//gcd, remove(w, common)) for c,w in terms)
            parts = [] if gcd == 1 else [("int", gcd)]
            parts.extend(factors(common.elements()))
            rest_value = factor(rest)
            if rest_value != ("int",1):
                parts.append(rest_value)
            return times(parts)
        if len(terms) == 1:
            return ("int",1)
        # Pairs identify complete common products, including multi-bracket
        # ones that a single-variable Horner ordering can miss.
        counts = [Counter(w) for _,w in terms]
        options = {}
        for i,x in enumerate(counts):
            for y in counts[i+1:]:
                prod = tuple(sorted((x & y).elements()))
                if prod and prod not in options:
                    needed = Counter(prod)
                    selected = tuple(j for j,z in enumerate(counts) if not (needed-z))
                    saved = (len(selected)-1)*sum(max(1,len(w)) for w in prod)
                    options[prod] = (saved, selected)
        if not options:
            return plus([(1 if c>0 else -1, times(
                ([("int",abs(c))] if abs(c)!=1 or not words else [])+factors(words)))
                         for c,words in terms])
        _, selected = max(options.values(), key=lambda v: (v[0],len(v[1]),v[1]))
        picked = set(selected)
        left = factor(tuple(terms[i] for i in selected))
        right = factor(tuple(v for i,v in enumerate(terms) if i not in picked))
        return plus([(1,left),(1,right)])
    return factor(tuple((c,tuple(sorted(words))) for c,*words in rows))


def signed(node):
    if node[0]=="int" and node[1]<0:
        return -1, ("int",-node[1])
    if node[0]=="mul" and node[1][0][0]=="int" and node[1][0][1]<0:
        first = -node[1][0][1]
        tail = list(node[1][1:])
        return -1, times(([("int",first)] if first!=1 else [])+tail)
    return 1,node


def summands(node):
    if node[0] != "sum":
        sign, value = signed(node)
        if value[0] == "sum":
            return [(sign * s, term) for s, term in summands(value)]
        return [(sign, value)]
    result = []
    for sign,part in node[1]:
        for child_sign,child in summands(part):
            result.append((sign*child_sign,child))
    return result


def tex_numerator(fraction, charged, render_atom=None, atom_width=len):
    """Break at addition signs inside a factored expression without expanding it."""
    chunks = [["", 0, 0]]

    def emit(tex, width):
        chunks[-1][0] += tex
        chunks[-1][1] += width

    def visit(node, depth=0):
        kind, value, *_ = node
        if kind == "int":
            emit(str(value)+" ", len(str(value)))
        elif kind == "atom":
            power = node[2]
            exponent = "^{"+str(power)+"}" if power!=1 else ""
            if render_atom is None:
                rendered = value if value=="m" else spin.bracket(value,charged)
            else:
                rendered = render_atom(value)
            emit(rendered+exponent+" ", atom_width(value)+len(exponent))
        elif kind == "mul":
            sign, clean = signed(node)
            if sign == -1:
                emit("-", 1)
                if clean[0] == "sum":
                    emit(r"\big(", 1)
                    visit(clean, depth + 1)
                    emit(r"\big)", 1)
                else:
                    visit(clean, depth)
            else:
                for child in value:
                    if child[0] == "sum":
                        emit(r"\big(", 1)
                        visit(child, depth + 1)
                        emit(r"\big)", 1)
                    else:
                        visit(child, depth)
        else:
            for idx, (sign, child) in enumerate(summands(node)):
                if idx:
                    chunks.append(["", 0, depth])
                if idx or sign < 0:
                    emit("-" if sign < 0 else "+", 1)
                visit(child, depth)

    visit(factor_polynomial(fraction["terms"]))
    # Within the available width, favor a top-level addition as a line break.
    # A parenthesis can span multiple lines, keeping its common factors visible.
    lines = []
    while chunks:
        total = 0
        end = 0
        while end < len(chunks) and (end == 0 or total + chunks[end][1] <= 75):
            total += chunks[end][1]
            end += 1
        if end < len(chunks):
            candidates = [i for i in range(1, end+1)
                          if sum(c[1] for c in chunks[:i]) >= 45]
            if candidates:
                end = min(candidates, key=lambda i: (chunks[i][2], -i))
        indent = r"\;" * min(3, chunks[0][2]) if lines else ""
        lines.append(indent + "".join(c[0] for c in chunks[:end]).strip())
        chunks = chunks[end:]
    return lines


def external(data):
    return tuple(tuple(i for i,s in enumerate(data["particles"],1) if s==species)
                 for species in ("e","E","A.+"))


def digits(labels):
    return "".join(map(str,labels))


def coupling(data, divisor=1):
    q, n = data["charge_power"],data["sqrt_two_power"]
    scalar, remainder = divmod(2**(n//2), divisor)
    if remainder:
        raise ValueError("Nonintegral scalar in displayed coupling")
    return (str(scalar) if scalar!=1 else "")+(r"\sqrt{2}" if n%2 else "")+"e^{"+str(q)+"}"


def argument(photons, negatives, physical):
    top,bottom,den = [],[],[]
    for j,i in enumerate(photons):
        pos = j < len(photons)-negatives
        other = photons[(j+1)%len(photons)]
        ang,sq = rf"\qa{{{i}}}",rf"\qb{{{i}}}"
        if physical or len(photons)==1:
            q = "p_1" if physical else "(p_1+p_2)"
            if pos: ang = sq+q
            else: sq = ang+q
            den.append(spin.denominator("t1"+str(i)) if physical else
                       r"\big("+spin.denominator("t1"+str(i))+"+"+spin.denominator("t2"+str(i))+r"\big)")
        elif pos:
            ang = rf"\qa{{{other}}}"
            den.append(r"\langle"+str(other)+str(i)+r"\rangle")
        else:
            sq = rf"\qb{{{other}}}"
            den.append("["+str(other)+str(i)+"]")
        top.append(ang); bottom.append(sq)
    return r"\begin{pmatrix}"+'&'.join(top)+r'\\'+'&'.join(bottom)+r"\end{pmatrix}", "".join(den)


def write_helicities(data, name):
    electrons,_,photons=external(data)
    nphot=len(photons)
    sector="-".join(map(str,(len(data["particles"]),len(electrons))))
    lines = []
    for minus in range(nphot+1):
        word = "+"*(nphot-minus)+"-"*minus
        lines.append(r"\begin{align}")
        for physical in (True,False):
            matrix,den = argument(photons,minus,physical)
            pref = "phys" if physical else "spur"
            label = f"hel-{sector}-{minus}-{pref}"
            row = rf"\M^{{\{pref}}}_{{{word}}}&=\frac{{{coupling(data)}}}{{{den}}}\,{name}\!{matrix}\label{{{label}}}"
            lines.append(row+ (r",\\" if physical else "."))
        lines.append(r"\end{align}")
    return lines


def mixed_example(data):
    """Print one entire numerator at the displayed ordering with physical photon spinors."""
    es, ps, photons = external(data)
    negative = set(photons[-(len(photons)//2):])
    def project(word):
        if word == "m":
            return word
        def slot(label, angle):
            i = int(label)
            if i in photons and (angle != (i in negative)):
                i = photons[(photons.index(i)+1) % len(photons)]
            return str(i)
        a, b = slot(word[1],word[0]=="<"), slot(word[-2],word[-1]==">")
        if len(word)==4 and a==b:
            return None
        return word[0]+a+word[2:-2]+b+word[-1]
    choices = []
    for j,frac in enumerate(data["fractions"],1):
        terms=Counter()
        for c,*monomial in frac["terms"]:
            row=[project(w) for w in monomial]
            if None not in row:
                terms[tuple(sorted(row))]+=c
        rows=[[v,*words] for words,v in terms.items() if v]
        if rows:
            choices.append((len(rows),j,rows))
    _,j,rows=min(choices)
    charge=set(es+ps)
    def ordinary(word):
        return word if word=="m" else re.sub(r"\\widehat\{(\d)\}",r"\1", spin.bracket(word,charge))
    lines=tex_numerator({"terms":rows},charge,ordinary,len)
    word="+"*(len(photons)-len(negative))+"-"*len(negative)
    result=[r"\noindent For $"+word+r"$, the reference-pole expression has a particularly short",
            r"numerator at the displayed ordering. Before the external exchanges, $N_{"+str(j)+
            r"}$ with its actual photon spinors is",
            r"{\small\begin{align*}"]
    result.extend((rf"N_{{{j}}}^{{{word}}}={{}}&" if i==0 else r"&\quad{}")+v+
                  (r"\\" if i+1<len(lines) else ".") for i,v in enumerate(lines))
    result.append(r"\end{align*}}")
    return result


def one_minus(data):
    """Specialize the external polynomial to a common angle spinor on every photon."""
    n = len(data["particles"])
    fraction, = data["fractions"]
    order = dict(zip(range(3,n+1), "abcdfgk"))
    def endpoint(label, angle):
        i = int(label)
        if i < 3:
            return rf"\mathbf{{{i}}}"
        return str(n) if angle else r"\bar{"+order[i]+"}"
    def momentum(labels):
        return "".join(order.get(int(i), i) for i in labels)
    def projected(word, flip):
        if word == "m":
            return word
        left = (word[0] == "<") != flip
        right = (word[-1] == ">") != flip
        a, b = endpoint(word[1],left), endpoint(word[-2],right)
        if len(word)==4 and a==b:
            return None
        mids = re.sub(r"\d+", lambda m: momentum(m[0]), word[2:-2])
        return (r"\langle " if left else "[") + a + mids + b + (r"\rangle" if right else "]")

    projected_terms = Counter()
    for c, *words in fraction["terms"]:
        for flip in (False,True):
            factors = [projected(w, flip) for w in words]
            if None not in factors:
                projected_terms[tuple(sorted(factors))] += c * (fraction["chirality"] if flip else 1)
    raw = [[value,*monomial] for monomial,value in projected_terms.items() if value]
    def width(atom):
        return len(re.sub(r"\\(?:bar|mathbf)\{(.)\}",r"\1",atom)
                    .replace(r"\langle ","<").replace(r"\rangle",">"))
    numerator = tex_numerator({"terms":raw}, set(), lambda v: v, width)
    perm = ",".join(order.values())
    poles = "".join(r"\tau_{"+momentum(d[1:])+"}" for d in fraction["poles"])
    norm = f"[3{n}]"+"".join(r"\langle"+f"{n}{j}"+r"\rangle" for j in order if j!=n)
    coef, divisor = fraction["factor"]
    if (coef,divisor)!=(1,2):
        raise ValueError("Unexpected weight for the single ordered photon fraction")
    word = "+"*(n-3)+"-"
    op = exchange(1,2,fraction["exchanges"][0])
    new = [r"\paragraph{One negative photon.} Choose $\langle"+str(n)+r"|$ as the angle reference for",
           r"every positive photon and $[3|$ as the square reference for photon "+str(n)+".",
           r"Every photon angle endpoint then has the same spinor. A factor that pairs",
           r"two such endpoints is $\langle"+str(n)*2+r"\rangle=0$. Only "+{4:"four",5:"five"}[len(raw)]+
           r" numerator terms survive before collecting common factors.",
           r"In the permutation sum $("+perm+r")$ runs over the orders of $("+
           ",".join(str(i) for i in order)+r")$. A bar on a square endpoint makes one replacement:",
           rf"$\bar{{{n}}}=3$ and $\bar i=i$ for the other photons. The unbarred momenta retain their labels.",
           r"\begin{equation}\label{eq:one-minus-ordered-"+str(n)+"}",
           r"\M^{\spur}_{"+word+r"}=\frac{"+coupling(data,2)+"}{"+norm+r"}",
           r"\sum_{"+perm+"}"+op+r"\!\left(\frac{N}{"+poles+r"}\right).",
           r"\end{equation}",
           r"{\small\begin{align*}"]
    new.extend(("N={}&" if i==0 else r"&\quad{}")+line+(r"\\" if i+1<len(numerator) else ".")
               for i,line in enumerate(numerator))
    new.extend([r"\end{align*}}",
                r"The signs of the charged exchange apply to the entire fraction."])
    return new


def exchange(i,j,s):
    return ("(1+" if s==1 else "(1-")+f"P_{{{i}{j}}})"


def write_polynomial(data,name):
    es,ps,ph=external(data)
    fs=data["fractions"]
    n,k=len(data["particles"]),len(es)
    charged=set(es+ps)
    outer="".join(r"\mathcal A_{"+digits(g)+"}" for g in (es,ps) if len(g)>1)
    if len(ph)>1:outer+=r"\mathcal S_{"+digits(ph)+"}"
    outer+="(1+\\mathcal C)"
    if any(f["chirality"]!=1 for f in fs):
        raise ValueError("This catalog uses a common even chirality sum")
    result=[r"\par\smallskip\noindent\textit{Spinor numerator and poles.} " if len(fs)==1 else
            r"\par\smallskip\noindent\textit{Spinor numerators and poles.} "]
    if len(fs)>1:
        result.extend(["Each contribution includes its full propagator denominator.",
                       r"\begin{equation*}",name+"="+outer+r"\sum_{j=1}^{"+str(len(fs))+r"} F_j.",
                       r"\end{equation*}"])
    for j,f in enumerate(fs,1):
        a,b=f["factor"]
        scalar = spin.factor(a,b)
        ops = "".join(exchange(i,l,s) for i,l,s in zip(es,ps,f["exchanges"]))
        den="".join(spin.denominator(p) for p in f["poles"])
        # Exchange operators must act on both propagators and numerators.
        lhs=name+"={}&"+outer if len(fs)==1 else f"F_{{{j}}}={{}}&"
        result.extend([r"{\small\setlength{\jot}{2pt}\begin{align*}",
                       lhs+scalar+ops+r"\!\left(\frac{N_{"+str(j)+"}}{"+den+r"}\right),\\*"])
        lines=tex_numerator(f,charged)
        for i,line in enumerate(lines):
            lhs=f"N_{{{j}}}={{}}&" if i==0 else r"&\quad{}"
            result.append(lhs+line+(r"\\" if i+1<len(lines) else "."))
        result.append(r"\end{align*}}")
    return result


def generate(source, out):
    out.mkdir(parents=True,exist_ok=True)
    for n in (7,8,9):
        for k in range(1,n//2+1):
            data=json.loads((source/f"{n}-{k}.short.json").read_text())
            if len(data["particles"])!=n or data["particles"].count("e")!=k:
                raise ValueError(f"Incorrect particle inventory for {n}-{k}")
            if not n-2*k:
                continue
            # The symbol is local to its family. The subscript spells out
            # the photon count so the argument can be read without a dictionary.
            name=r"B_{"+str(n-2*k)+r"\gamma}"
            if (n,k) in ((7,2),(8,3),(9,3)):
                (out/f"mixed-example-{n}-{k}.tex").write_text("\n".join(mixed_example(data))+"\n")
            if k==1:
                (out/f"one-minus-{n}.tex").write_text("\n".join(one_minus(data))+"\n")
            fragments = [("helicities", write_helicities(data,name))]
            # The manuscript's three-pair, one-photon section already has a
            # shorter handwritten factorization. The read-back checks it too.
            if (n,k)!=(7,3):
                fragments.append(("polynomial",write_polynomial(data,name)))
            for kind,lines in fragments:
                (out/f"{kind}-{n}-{k}.tex").write_text("\n".join(lines)+"\n")


if __name__=="__main__":
    ap=argparse.ArgumentParser(description=__doc__)
    ap.add_argument("polynomials",type=Path)
    ap.add_argument("output",type=Path)
    args=ap.parse_args()
    generate(args.polynomials,args.output)
