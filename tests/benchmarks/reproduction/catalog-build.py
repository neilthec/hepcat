"""Transcribe frozen QED models into the machine-readable and literal TeX catalogs."""

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



import argparse
from pathlib import Path
import ast,re,json,hashlib

repository = Path(__file__).resolve().parents[3]
package = Path(__file__).resolve().parent
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--models", type=Path, default=repository / "models")
parser.add_argument("--output-directory", type=Path, default=repository / "tests/results/reproduction/catalog")
args = parser.parse_args()
root = args.output_directory
root.mkdir(parents=True, exist_ok=True)
entries=[]
for model in [3,4]:
 p=args.models/f'amps{model}.mdl'
 for line in p.read_text().splitlines():
  f=[v.strip() for v in line.split('|')]
  if len(f)<6 or f[0]!='e':continue
  particles=[v for v in f[:-2] if v]
  if not 4<=len(particles)<=6:continue
  entries.append(dict(model=model,preference='AllPhysical' if model==3 else 'AllSpurious',particles=particles,factor=f[-2],expression=f[-1],file_sha256=hashlib.sha256(p.read_bytes()).hexdigest()))
(root/'catalog-expressions.json').write_text(json.dumps(entries,indent=2)+'\n')

# A typography-only parser. Every symbolic token is retained, with no spinor identities.
def parse(txt, fermions=()):
 tokens={}
 def token(tex):
  key='token'+str(len(tokens));tokens[key]=tex;return key
 # Rare unexpanded linear insertion from the saved model.
 txt=re.sub(r'SpinorChain\[Spinor\["Helicity","Square",(\d+)\],\((.*?)\),Spinor\["Helicity","Angle",(\d+)\]\]',lambda m:token('['+m[1]+'|'+re.sub(r'Mom\[(\d+)\]',r'p_{\1}',m[2])+'|'+m[3]+r'\rangle'),txt)
 txt=re.sub(r'PropDen\[Mom\[Multiparticle\[([\d,]+)\]\],(Me|0)\]',lambda m:token(('s_{' if m[2]=='0' else r'\tau_{')+m[1].replace(',','')+'}'),txt)
 txt=re.sub(r'\(s(\d+)-Me\^2\)',lambda m:token(r'\tau_{'+m[1]+'}'),txt)
 txt=re.sub(r'MomProd\[(\d+),(\d+)\]',lambda m:token(r'(p_{'+m[1]+r'}\!\cdot p_{'+m[2]+'})'),txt)
 def chain(m):
  labels=list(m[2])
  for pos in [0,len(labels)-1]:
   if int(labels[pos]) in fermions:labels[pos]=r'\mathbf{'+labels[pos]+'}'
  return token((r'\langle' if m[1]=='<' else '[')+''.join(labels)+(r'\rangle' if m[3]=='>' else ']'))
 txt=re.sub(r'(<|\[)(\d+)(>|\])',chain,txt)
 txt=re.sub(r'\bs(\d+)\b',lambda m:token('s_{'+m[1]+'}'),txt)
 txt=txt.replace('Sqrt[2]',token(r'\sqrt{2}')).replace('Me','m').replace('EE','e').replace('^','**')
 txt=re.sub(r'(token\d+)(?=token)',r'\1*',txt)
 txt=re.sub(r'(token\d+)\s+(?=token)',r'\1*',txt)
 txt=re.sub(r'(?<=[0-9)])\s+(?=token)', '*', txt)
 txt=re.sub(r'\)(?=token)', ')*', txt)
 return ast.parse(txt,mode='eval').body,tokens

def fmt(n,t):
 if isinstance(n,ast.Name):return t.get(n.id,n.id)
 if isinstance(n,ast.Constant):return str(n.value)
 if isinstance(n,ast.UnaryOp):return '-'+wrap(n.operand,t)
 a,b=n.left,n.right
 if isinstance(n.op,ast.Add):
  bs=fmt(b,t)
  return fmt(a,t)+('' if bs.startswith('-') else '+')+bs
 if isinstance(n.op,ast.Sub):return fmt(a,t)+'-'+wrap(b,t)
 if isinstance(n.op,ast.Mult):
  sign=1;factors=[]
  def collect(x):
   nonlocal sign
   if isinstance(x,ast.UnaryOp) and isinstance(x.op,ast.USub):
    sign=-sign;collect(x.operand)
   elif isinstance(x,ast.BinOp) and isinstance(x.op,ast.Mult):collect(x.left);collect(x.right)
   elif isinstance(x,ast.Constant) and x.value==1:pass
   else:factors.append(x)
  collect(n)
  return ('-' if sign<0 else '')+(r'\,'.join(wrap(x,t) for x in factors) or '1')
 if isinstance(n.op,ast.Div):return r'\frac{'+fmt(a,t)+'}{'+fmt(b,t)+'}'
 if isinstance(n.op,ast.Pow):return wrap(a,t)+'^{'+fmt(b,t)+'}'
 raise ValueError(ast.dump(n))
def wrap(n,t):
 s=fmt(n,t)
 return '('+s+')' if (isinstance(n,ast.BinOp) and isinstance(n.op,(ast.Add,ast.Sub))) or isinstance(n,ast.UnaryOp) else s

def adds(n):
 if isinstance(n,ast.BinOp) and isinstance(n.op,ast.Add):return adds(n.left)+adds(n.right)
 if isinstance(n,ast.BinOp) and isinstance(n.op,ast.Sub):return adds(n.left)+[ast.UnaryOp(op=ast.USub(),operand=x) for x in adds(n.right)]
 return [n]
def muls(n):
 if isinstance(n,ast.BinOp) and isinstance(n.op,ast.Mult):return muls(n.left)+muls(n.right)
 return [n]
def expanded_terms(n):
 if isinstance(n,ast.UnaryOp):
  return [ast.UnaryOp(op=n.op,operand=x) for x in expanded_terms(n.operand)]
 if isinstance(n,ast.BinOp) and isinstance(n.op,ast.Add):return expanded_terms(n.left)+expanded_terms(n.right)
 if isinstance(n,ast.BinOp) and isinstance(n.op,ast.Sub):
  return expanded_terms(n.left)+[ast.UnaryOp(op=ast.USub(),operand=x) for x in expanded_terms(n.right)]
 if isinstance(n,ast.BinOp) and isinstance(n.op,ast.Mult):
  return [ast.BinOp(left=a,op=ast.Mult(),right=b) for a in expanded_terms(n.left) for b in expanded_terms(n.right)]
 return [n]
def number(n):
 try:return ast.literal_eval(n)
 except:return None

lines=[r'\section{Literal saved representatives}\label{app:saved-representatives}',
'The following expressions preserve the saved model representatives. The blocks introduced below only divide long numerators across printed lines. No on-shell identity, numerical fit or change of amplitude is made by this transcription. The fully expanded machine-readable expressions accompany the manuscript in \\texttt{catalog-expressions.json}. A shorter current representation is given in the main manuscript amplitude catalog.']
# Include independent representatives, omit parity partners and compact4/5 seeds covered in manuscript.
for row in entries:
 particles=row['particles']
 if len(particles)!=6:continue
 hel=[x for x in particles if x.startswith('A.')]
 if hel and hel.count('A.-')>len(hel)/2:continue
 if len(hel)==4 and (hel.count('A.-')==0 or (hel.count('A.-')==1 and row['model']==4)):continue
 fermions={i+1 for i,p in enumerate(particles) if p in ['e','E']}
 n,t=parse(row['expression'],fermions); f,ft=parse(row['factor'])
 groups={}
 for term in adds(n):
  den=[];num=[]
  for fac in muls(term):
   if isinstance(fac,ast.BinOp) and isinstance(fac.op,ast.Pow) and number(fac.right) is not None and number(fac.right)<0:
    powr=-number(fac.right);den.append((fmt(fac.left,t),powr))
   else:num.append(fac)
  key=tuple(sorted(den))
  prod=num[0]
  for nn in num[1:]:prod=ast.BinOp(left=prod,op=ast.Mult(),right=nn)
  groups.setdefault(key,[]).append(prod)
 title=','.join(x.replace('A.+',r'\gamma^+').replace('A.-',r'\gamma^-') for x in particles)
 plain_title=','.join(particles).replace('A.+','gamma+').replace('A.-','gamma-')
 lines += [r'\subsection{\texorpdfstring{$'+title+'$: '+row['preference']+'}{'+plain_title+': '+row['preference']+'}}',
  r'\begin{equation}\M='+fmt(f,ft)+r'\sum_{k=1}^{'+str(len(groups))+r'}\frac{\mathcal N_k}{\mathcal D_k}.\end{equation}']
 for k,(den,nums) in enumerate(groups.items(),1):
  dt=' '.join(a+('^{'+str(b)+'}' if b!=1 else '') for a,b in den) or '1'
  lines.append(r'\begin{equation}\mathcal D_{'+str(k)+'}='+dt+r'.\end{equation}')
  lines.append(r'\begin{align*}\mathcal N_{'+str(k)+'}={}&')
  # AST top products may contain a long sum. Distribute just this scalar sum for printable lines.
  pieces=[term for nn in nums for term in expanded_terms(nn)]
  for j,term in enumerate(pieces):
   ss=fmt(term,t)
   if j:lines.append((r'{}-{}&'+ss[1:]) if ss.startswith('-') else (r'{}+{}&'+ss))
   else:lines.append(ss)
   if j<len(pieces)-1:lines.append(r'\\')
  lines.append(r'\end{align*}')
(root/'catalog-expanded.tex').write_text('\n'.join(lines)+'\n')
print('records',len(entries),'expanded lines',len(lines))
