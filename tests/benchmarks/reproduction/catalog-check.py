"""Regenerate the catalog and check its transcription using exact rational arithmetic."""

# HEPCAT - High Energy Physics Constructive Amplitude Tool
#
# Copyright (C) 2026 Neil Christensen
#
# This file is part of HEPCAT.
#
# HEPCAT is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# HEPCAT is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with HEPCAT.  If not, see <https://www.gnu.org/licenses/>.



from pathlib import Path
from fractions import Fraction
import ast,re,runpy,sys
if not __debug__:
 raise RuntimeError("Catalog verification requires Python assertions; run without -O or PYTHONOPTIMIZE.")
sys.setrecursionlimit(20000)
p=Path(__file__).parent
# The builder consumes the shared --models, --output-directory and --paper-model-labels options.
g=runpy.run_path(str(p/'catalog-build.py'))
p=g['root']

def val(n,values):
 if isinstance(n,ast.Constant):return Fraction(n.value)
 if isinstance(n,ast.Name):return values[n.id]
 if isinstance(n,ast.UnaryOp):return -val(n.operand,values)
 a,b=val(n.left,values),val(n.right,values)
 if isinstance(n.op,ast.Add):return a+b
 if isinstance(n.op,ast.Sub):return a-b
 if isinstance(n.op,ast.Mult):return a*b
 if isinstance(n.op,ast.Div):return a/b
 if isinstance(n.op,ast.Pow):return a**int(b)
 raise ValueError(ast.dump(n))
def untex(tex,t):
 reverse={v:k for k,v in t.items()}
 pattern=re.compile('|'.join(re.escape(x) for x in sorted(reverse,key=len,reverse=True)))
 tex=pattern.sub(lambda m:reverse[m[0]],tex).replace(r'\,','*')
 def brace(s,pos):
  assert s[pos]=='{',(s,pos)
  level=1;i=pos+1
  while level:
   if s[i]=='{':level+=1
   if s[i]=='}':level-=1
   i+=1
  return s[pos+1:i-1],i
 def cv(s):
  out='';i=0
  while i<len(s):
   if s.startswith(r'\frac',i):
    a,j=brace(s,i+5);b,i=brace(s,j);out+='('+cv(a)+')/('+cv(b)+')'
   elif s.startswith('^{',i):
    a,i=brace(s,i+1);out+='**('+cv(a)+')'
   else:out+=s[i];i+=1
  return out
 return ast.parse(cv(tex),mode='eval').body
checks=0
for row in g['entries']:
 fermions={i+1 for i,x in enumerate(row['particles']) if x in ['e','E']}
 n,t=g['parse'](row['expression'],fermions)
 restored=untex(g['fmt'](n,t),t)
 texts=sorted(set(t.values()))
 for sample in range(3):
  tv={txt:Fraction(2+i+sample,1+sample) for i,txt in enumerate(texts)}
  v={k:tv[txt] for k,txt in t.items()};v.update(m=Fraction(2+sample),e=Fraction(3+sample))
  assert val(n,v)==val(restored,v),(row['model'],row['particles'],'formatter')
  expanded=g['expanded_terms'](n)
  assert val(n,v)==sum((val(x,v) for x in expanded),Fraction(0)),(row['model'],row['particles'],'distribution')
  checks+=2
print('Exact rational formatter roundtrips and distribution checks:',checks,'PASS across',len(g['entries']),'saved records')
text=(p/'catalog-expanded.tex').read_text()
sections=re.split(r'\\subsection\{',text)[1:]
selected=[]
for row in g['entries']:
 particles=row['particles'];hel=[x for x in particles if x.startswith('A.')]
 if len(particles)!=6 or (hel and hel.count('A.-')>len(hel)/2):continue
 if len(hel)==4 and (hel.count('A.-')==0 or (hel.count('A.-')==1 and row['preference']=='AllSpurious')):continue
 selected.append(row)
assert len(sections)==len(selected)
for row,sec in zip(selected,sections):
 n,t=g['parse'](row['expression'],{i+1 for i,x in enumerate(row['particles']) if x in ['e','E']})
 ds={int(i):untex(s.replace(' ',r'\,'),t) for i,s in re.findall(r'\\mathcal D_\{(\d+)\}=(.*?)\.\\end\{equation\}',sec)}
 ns={}
 for i,s in re.findall(r'\\begin\{align\*\}\\mathcal N_\{(\d+)\}=\{\}&(.*?)\\end\{align\*\}',sec,re.S):
  s=s.replace(r'\\','').replace('{}+{}&','+').replace('{}-{}&','-').replace('\n','')
  ns[int(i)]=untex(s,t)
 assert ds.keys()==ns.keys()
 texts=sorted(set(t.values()))
 for sample in range(3):
  tv={txt:Fraction(2+i+sample,1+sample) for i,txt in enumerate(texts)}
  v={k:tv[txt] for k,txt in t.items()};v.update(m=Fraction(2+sample),e=Fraction(3+sample))
  actual=sum((val(ns[k],v)/val(ds[k],v) for k in ns),Fraction(0))
  assert actual==val(n,v),(row['model'],row['particles'],'literal rendered blocks')
print('Literal TeX block reconstruction:',len(selected)*3,'exact rational PASS for',len(selected),'long representatives')
