"""Reduce the contracted QED polynomials by exact external-leg symmetries.

All coefficients are compared as integers before a shorter numerator is kept.
No reference amplitude enters these operations.
"""
import argparse
import importlib
import itertools
import json
import math
from pathlib import Path
from collections import Counter
from functools import lru_cache
from fractions import Fraction
base=importlib.import_module("contracted-polynomials")
word,deword,move_den,move_poly=base.word,base.deword,base.move_den,base.move_poly


def compose(p,q):
    return tuple(p[i] for i in q)


@lru_cache(maxsize=400000)
def moved(s,p,reverse):
    if s=="m": return "m",1
    l,i,ms,j,r=deword(s)
    if reverse:
        l="[" if l=="<" else "<"
        r="[" if r=="<" else "<"
    return word((l,i,ms,j,r),p)


def transformed(m,p,reverse=False):
    factors=[];value=1
    for s in m:
        t,v=moved(s,p,reverse);factors.append(t);value*=v
    return tuple(sorted(factors)),value


def signed(poly,p,reverse=False,sign=1):
    d=Counter()
    for m,v in poly.items():
        key,s=transformed(m,p,reverse)
        d[key]+=sign*s*v
    return {k:v for k,v in d.items() if v}


def character(a,b):
    if a==b:return 1
    if len(a)==len(b) and all(b.get(m)==-c for m,c in a.items()):return -1
    raise ValueError("External exchange does not reproduce the contracted coefficients")


def compact(data):
    legs=data["particles"]; n=len(legs)
    ident=tuple(range(n+1))
    electrons=[i for i in ident[1:] if legs[i-1]=="e"]
    positrons=[i for i in ident[1:] if legs[i-1]=="E"]
    photons=[i for i in ident[1:] if legs[i-1]=="A.+"]
    k=len(electrons); pairs=list(zip(electrons,positrons))
    paired=[]
    for order in itertools.permutations(range(k)):
        for q in itertools.permutations(photons):
            p=list(ident)
            for j,b in enumerate(order):
                p[electrons[j]]=electrons[b]; p[positrons[j]]=positrons[b]
            for i,j in zip(photons,q):p[i]=j
            paired.append(tuple(p))
    @lru_cache(None)
    def canonical(d):
        return min((move_den(d,p),p) for p in paired)
    flips=[]
    for which in itertools.product((0,1),repeat=k):
        p=list(ident)
        for b,(i,j) in zip(which,pairs):
            if b:p[i],p[j]=j,i
        flips.append((which,tuple(p)))
    complete={}
    for f in data["fractions"]:
        den=tuple(f["poles"]); raw={tuple(w):v for v,*w in f["terms"]}
        # Two endpoints belong to the same charged line exactly when no
        # neutral propagator separates them. This determines a unique pairing.
        neutral=[set(map(int,s[1:])) for s in den if s[0]=="s"]
        def signature(i):return tuple(i in g for g in neutral)
        ordered=[]
        for i in electrons:
            mates=[j for j in positrons if signature(i)==signature(j)]
            if len(mates)!=1:raise ValueError(("Unequal neutral cuts",den))
            ordered.append(mates[0])
        p=list(ident)
        for a,b in zip(ordered,positrons):p[a]=b
        sign=base.parity([p[i] for i in positrons])
        d,q=canonical(move_den(den,p))
        if d in complete:raise ValueError("Duplicated pole orbit")
        complete[d]=(signed(raw,compose(q,p),sign=sign),f["divisor"])
    remaining=set(complete); fractions=[]
    while remaining:
        den=min(remaining); poly,_=complete[den]
        all_images=[]
        for which,p in flips:
            d,q=canonical(move_den(den,p))
            if d not in complete:raise ValueError("Missing charge orientation")
            s=character(signed(poly,compose(q,p)),complete[d][0])
            all_images.append((which,p,d,s))
        signs=[next(s for w,p,d,s in all_images if sum(w)==1 and w[i]) for i in range(k)]
        orbit={d for w,p,d,s in all_images}
        if not orbit<=remaining:raise ValueError("Overlapping fraction orbits")
        remaining-=orbit
        stabilizer=[]
        for w,p,d,s in all_images:
            if s!=math.prod(signs[i] for i,x in enumerate(w) if x):raise ValueError("Orientation sign is not a character")
            if d==den:
                for q in paired:
                    r=compose(q,p)
                    if move_den(den,r)==den:
                        if signed(poly,r,sign=s)!=poly:raise ValueError("Pole stabilizer changed numerator")
                        stabilizer.append((r,s))
        star=character(signed(poly,ident,True),poly)
        todo=set(poly)
        seed={}
        while todo:
            # Prefer short expressions with no leading negative sign.
            mon=min(todo,key=lambda w:(poly[w]<0, sum(len(s) for s in w),w))
            images={}
            for rev in (False,True):
                for r,s in stabilizer:
                    m,v=transformed(mon,r,rev)
                    expected=v*s*(star if rev else 1)*poly[mon]
                    if poly.get(m)!=expected:raise ValueError("Inconsistent numerator orbit")
                    images[m]=expected
            if not images.keys()<=todo:raise ValueError("Overlapping monomial orbits")
            todo-=images.keys()
            seed[mon]=len(images)*poly[mon]
        # Each phase-space denominator occurs with equal multiplicity, checked
        # using the independently counted uncompressed denominator orbits.
        count=sum(data["permutation_order"]//complete[d][1] for d in orbit)
        div,remainder=divmod(2*(2**k)*data["permutation_order"],count)
        if remainder or div!=2*len(stabilizer):raise ValueError("Permutation multiplicity")
        nums={m:Fraction(v,div) for m,v in seed.items()}
        common=math.lcm(*(v.denominator for v in nums.values()))
        coefs=[int(v*common) for v in nums.values()]
        g=math.gcd(*coefs); multip=Fraction(g,common)
        fractions.append(dict(poles=den,exchanges=signs,chirality=star,
                          factor=[multip.numerator,multip.denominator],pole_products=count,
                          terms=[[c//g,*m] for c,m in zip(coefs,nums)]))
    if sum(f["pole_products"] for f in fractions)!=data["pole_products"]:raise ValueError("Incomplete amplitude")
    return {**data,"fractions":fractions}


if __name__=="__main__":
    p=argparse.ArgumentParser(description=__doc__);p.add_argument("input",type=Path);p.add_argument("output",type=Path)
    args=p.parse_args();out=compact(json.loads(args.input.read_text()))
    args.output.write_text(json.dumps(out,separators=(",",":"))+"\n")
    print(len(out["fractions"]),"pole patterns,",sum(len(f["terms"]) for f in out["fractions"]),"external monomials")
