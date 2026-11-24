"""Prepare replacement model tables from verified external spinor polynomials.

Pending rows and previously encoded external-leg orbits are regenerated. The new directory must be separate
from the input model and is suitable for testing both package readers.
"""
import argparse
from collections import Counter
import importlib
import json
import math
from pathlib import Path
import shutil

base=importlib.import_module("contracted-polynomials")


class Shared:
    def __init__(self):
        self.ids={}
        self.rows=[]
    def record(self,s):
        if s not in self.ids:
            self.rows.append(s)
            self.ids[s]=len(self.rows)
        return self.ids[s]
    def op(self,k,*xs):
        if len(xs)==1:return xs[0]
        return self.record(k+":"+ ",".join(str(x) for x in xs))
    def number(self,x):
        return self.record(str(x))
    def factor(self,terms):
        if not terms:return self.number(0)
        common=Counter(terms[0][1])
        for _,m in terms[1:]:common&=Counter(m)
        if common:
            rest=[(c,tuple((Counter(m)-common).elements())) for c,m in terms]
            return self.op('*',*sorted(common.elements()),self.factor(rest))
        counts=Counter()
        for _,m in terms:counts.update(set(m))
        if not counts:return self.number(sum(c for c,_ in terms))
        v=min(counts,key=lambda x:(-counts[x],x))
        yes,no=[],[]
        for c,m in terms:
            if v in m:
                u=list(m);u.remove(v);yes.append((c,tuple(u)))
            else:no.append((c,m))
        part=self.op('*',v,self.factor(yes))
        return self.op('+',part,self.factor(no)) if no else part


def momentum(labels):
    return "Mom["+("Multiparticle["+ ",".join(map(str,labels))+"]" if len(labels)>1 else str(labels[0]))+"]"


def spinor(kind,ang,i):
    return f'Spinor["{kind}","{"Angle" if ang else "Square"}",{i}]'


def chain(s,massive):
    l,i,ms,j,r=base.deword(s)
    return "SpinorChain["+ ",".join([spinor("Spin" if i in massive else "Polarization",l=="<",i),
                   *map(momentum,ms),spinor("Spin" if j in massive else "Polarization",r=="<",j)])+"]"


def prop(d):
    return "PropDen["+momentum(tuple(map(int,d[1:])))+","+("Me" if d[0]=="t" else "0")+"]"


def kernel(data):
    massive={i for i,l in enumerate(data['particles'],1) if l!='A.+'}
    shared=Shared(); parts=[]
    for f in data['fractions']:
        terms=[]
        for c,*m in f['terms']:
            vs=tuple(sorted(shared.record("Me" if x=="m" else chain(x,massive)) for x in m))
            terms.append((c,vs))
        factors=[shared.factor(terms)]
        for d in f['poles']:
            factors.append(shared.op('^',shared.record(prop(d)),shared.number(-1)))
        if f['divisor']!=1:
            factors.append(shared.record("(1/"+str(f['divisor'])+")"))
        parts.append(shared.op('*',*factors))
    root=shared.op('+',*parts)
    assert root==len(shared.rows)
    return "Shared["+";".join(shared.rows)+"]"


def row_body(legs,physical,body):
    groups=[]
    for g in ("e","E","A"):
        members=[str(i) for i,s in enumerate(legs,1) if (s.startswith("A.") if g=="A" else s==g)]
        if len(members)>1:groups.append(("+" if g=="A" else "-")+".".join(members))
    photons=[i for i,s in enumerate(legs,1) if s.startswith("A.")]
    rules,den=[],[]
    for ix,i in enumerate(photons):
        ang=legs[i-1]=="A.+"
        if physical or len(photons)==1:
            rule=str(i)+"@"+("1" if physical else "1.2")
            ds=prop("t1"+str(i))
            if not physical:ds="("+ds+"+"+prop("t2"+str(i))+")"
        else:
            j=photons[(ix+1)%len(photons)]
            rule=str(j)
            ds="SpinorChain["+spinor("Helicity",ang,j)+","+spinor("Helicity",ang,i)+"]"
        den.append("("+ds+")^(-1)")
        rules.append(("A" if ang else "S")+str(i)+"="+rule)
    return "LegOrbit["+";".join([",".join(groups) or "-",",".join(rules) or "-","*".join(den) or "1",body])+"]"


def prepare(directory,data,out):
    if directory.resolve()==out.resolve():raise ValueError("Stage and verify the tables in a separate directory")
    out.mkdir(parents=True,exist_ok=True);compiled={};total=0
    for n in [4,5]:
        file=directory/f'amps{n}.mdl'; rows=file.read_text().splitlines();count=0
        for j,row in enumerate(rows):
            if 'PendingConstructiveDerivation:' not in row and 'LegOrbit[' not in row:continue
            fs=[s.strip() for s in row.split('|')];legs=[s for s in fs[:-2] if s]
            a=len(legs);k=legs.count('e');key=(a,k)
            if key not in compiled:
                full=json.loads((data/f"{a}-{k}.json").read_text())
                if full['particles']!=[s if s in ('e','E') else 'A.+' for s in legs]:
                    raise ValueError("Incorrect particle order")
                compiled[key]=kernel(full)
            src=row.split('|')
            nphot=a-2*k
            src[-2]=" EE^"+str(a-2)+(f"*Sqrt[2]^{nphot}" if nphot else "")+" "
            src[-1]=" "+row_body(legs,n==4,compiled[key])
            rows[j]='|'.join(src);count+=1
        (out/file.name).write_text('\n'.join(rows)+'\n')
        for name in ('vars','func','prtcls'):
            p=directory/f'{name}{n}.mdl';shutil.copyfile(p,out/p.name)
        total+=count
        print(file.name,count,"reconstructed rows",flush=True)
    if not total:raise ValueError("No external-leg orbit or pending rows")


if __name__=="__main__":
    p=argparse.ArgumentParser(description=__doc__)
    for name in ("models","contracted","destination"):p.add_argument(name,type=Path)
    a=p.parse_args();prepare(a.models,a.contracted,a.destination)
