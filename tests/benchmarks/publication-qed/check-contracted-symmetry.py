"""Expand the printed signed permutations and compare each coefficient exactly."""
import argparse
from collections import Counter
from fractions import Fraction
import importlib
import itertools
import json
from pathlib import Path

poly=importlib.import_module("contracted-polynomials")
sym=importlib.import_module("contracted-symmetry")


def check_external_weights(data):
    """Check external spin content and generic fixed-angle growth term by term."""
    legs=data["particles"]
    photons={i for i,leg in enumerate(legs,1) if leg.startswith("A.")}
    for fraction in data["fractions"]:
        for _,*monomial in fraction["terms"]:
            slots=Counter()
            degree=-2*len(fraction["poles"])-len(photons)
            mass_power=0
            for word in monomial:
                if word=="m":
                    mass_power+=1
                    continue
                left,i,momenta,j,right=poly.deword(word)
                slots[i,left]+=1
                slots[j,right]+=1
                degree+=1+len(momenta)
            for i in range(1,len(legs)+1):
                counts=(slots[i,"<"],slots[i,"["])
                if (counts!=(1,1) if i in photons else sum(counts)!=1):
                    raise ValueError(f"Wrong external spin content at leg {i}: {monomial}")
            # Both helicity prescriptions contribute E^-1 per photon after
            # counting its two external spinors. A fixed nonzero mass is O(1).
            if degree+mass_power!=4-len(legs):
                raise ValueError(f"Wrong mass dimension or fixed-angle bound: {monomial}")


def check(full, compact):
    check_external_weights(full)
    check_external_weights(compact)
    legs=full["particles"]
    for key in ("particles","coupling","permutation_order","pole_products","charge_power","sqrt_two_power"):
        if full.get(key)!=compact.get(key):raise ValueError("Mismatched "+key)
    expected={tuple(f["poles"]): {tuple(w):v for v,*w in f["terms"]} for f in full["fractions"]}
    found={d:Counter() for d in expected}
    es=[i for i,v in enumerate(legs,1) if v=="e"]
    ps=[i for i,v in enumerate(legs,1) if v=="E"]
    outer=list(poly.permutations(legs))
    for f in compact["fractions"]:
        den=tuple(f["poles"]);a,b=f["factor"]
        for choices in itertools.product((0,1),repeat=len(es)):
            lr=list(range(len(legs)+1));local=1
            for x,y,on,sign in zip(es,ps,choices,f["exchanges"]):
                if on:lr[x],lr[y]=y,x;local*=sign
            for order,sgn in outer:
                p=tuple(order[j] for j in lr)
                key=poly.move_den(den,p)
                if key not in expected:continue
                for rev in (False,True):
                    mult=Fraction(a*local*sgn*(f["chirality"] if rev else 1),b)
                    for c,*w in f["terms"]:
                        m,s=sym.transformed(tuple(w),p,rev)
                        found[key][m]+=mult*s*c
    for d,want in expected.items():
        got={m:v for m,v in found[d].items() if v}
        if got!=want:raise ValueError(f"Printed sum differs at {d}: {next(m for m in set(want)|set(got) if want.get(m,0)!=got.get(m,0))}")
    print(f"{len(expected)} independent denominators, {sum(map(len,expected.values()))} signed coefficients verified")


if __name__=="__main__":
    p=argparse.ArgumentParser(description=__doc__);p.add_argument("original",type=Path);p.add_argument("compact",type=Path)
    a=p.parse_args();check(json.loads(a.original.read_text()),json.loads(a.compact.read_text()))
