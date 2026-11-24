"""Typeset all terms of the compact external-spinor formulas, in pole order."""
import argparse
import itertools
import json
import math
import re
from collections import Counter
from pathlib import Path


def bracket(s,charged):
    def endpoint(d):
        return r'\mathbf{'+d+'}' if int(d) in charged else r'\widehat{'+d+'}'
    left= r'\langle' if s[0]=='<' else '['
    right=r'\rangle' if s[-1]=='>' else ']'
    return left+endpoint(s[1])+s[2:-2]+endpoint(s[-2])+right


def denominator(s):
    return ('s' if s[0]=='s' else r'\tau')+'_{'+s[1:]+'}'


def factor(a,b):
    if b==1:return '' if a==1 else str(a)
    return r'\tfrac{'+str(a)+'}{'+str(b)+'}'


def printable(f,charged):
    terms=[]
    for coeff,*words in f['terms']:
        counts=Counter(words)
        mass=counts.pop('m',0)
        leading=('-' if coeff<0 else '+')+(str(abs(coeff)) if abs(coeff)!=1 else '')
        if mass:leading+='m'+('^{'+str(mass)+'}' if mass>1 else '')
        pieces=[]
        for w,k in sorted(counts.items()):
            pieces.append(bracket(w,charged)+('^{'+str(k)+'}' if k>1 else ''))
        terms.append(leading+''.join(pieces))
    terms[0]=terms[0].removeprefix('+')
    return terms


def generate(source,target):
    with target.open('w') as out:
        def put(s):print(s,file=out)
        put(r'''\section{External Spinor Polynomials Through Nine Points}\label{app:contracted-catalog}
Each subsection gives the complete signed permutation sum in Eq.~\eqref{eq:external-orbit}.
The three entries preceding a numerator are its coefficient, the signs of its charged-pair exchanges in the stated order, and its propagator denominator.
A plus sign for the pair $(a,b)$ inserts $1+P_{ab}$, and a minus sign inserts $1-P_{ab}$. Apply these exchanges and the external permutations to the denominator as well as the numerator.
Add the angle--square exchanged partner to each numerator before applying the photon prescription in Table~\ref{tab:polarization-slots}.
A hat marks only a photon endpoint. For either helicity, an ordinary digit in parentheses is the momentum of that external particle.
''')
        for path in sorted(source.glob('*.short.json')):
            data=json.loads(path.read_text());n,k=map(int,path.name.split('.')[0].split('-'))
            legs=data['particles']; charged={i for i,s in enumerate(legs,1) if s!='A.+'}
            es=[i for i,v in enumerate(legs,1) if v=='e'];ps=[i for i,v in enumerate(legs,1) if v=='E']
            pairlist=','.join(f'({i},{j})' for i,j in zip(es,ps))
            h='photon' if n-2*k==1 else 'photons'
            put(r'\subsection{'+f'{n} points, {k} charged '+('pair' if k==1 else 'pairs')+f' and {n-2*k} {h}'+r'}\label{app:spinors-'+f'{n}-{k}'+'}')
            put(r'Pair order $'+pairlist+r'$. The '+str(len(data['fractions']))+r' pole '+('fraction generates' if len(data['fractions'])==1 else 'fractions generate')+
                f" {data['pole_products']:,} distinct labeled contributions.")
            for j,f in enumerate(data["fractions"],1):
                scalar=factor(*f['factor']) or '1'; signs=','.join('+' if x>0 else '-' for x in f['exchanges'])
                den=''.join(denominator(x) for x in f['poles'])
                put(r'\par\smallskip\noindent\textbf{Fraction '+str(j)+r'.}\quad $'+scalar+r'\ ;\quad('+signs+r')\ ;\quad '+den+r'$\par\nopagebreak')
                ts=printable(f,charged)
                put(r'{\small\setlength{\jot}{1pt}\begin{align*}')
                chunks=[];row=''
                for term in ts:
                    # Keep lines short enough for the longest momentum insertions and hats.
                    width=lambda s:len(re.sub(r'\\(?:mathbf|widehat)\{(\d)\}',r'\1',s).replace(r'\langle','<').replace(r'\rangle','>'))
                    if row and width(row)+width(term)>74:chunks.append(row);row=''
                    row+=term
                if row:chunks.append(row)
                put(('\\\\\n').join('&'+s for s in chunks))
                put(r'\end{align*}}')


if __name__=="__main__":
    a=argparse.ArgumentParser(description=__doc__);a.add_argument("directory",type=Path);a.add_argument("output",type=Path)
    opts=a.parse_args();generate(opts.directory,opts.output)
