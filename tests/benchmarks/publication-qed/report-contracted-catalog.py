"""Collect the exact catalog checks, signed-amplitude tests and matched rate measurements."""
import argparse
import json
from pathlib import Path


def read_rows(path, columns):
    raw=path.read_text().splitlines()
    if raw[0].split()!=columns:
        raise ValueError(f"Invalid report header in {path}")
    if any(len(line.split())!=len(columns) for line in raw[1:]):
        raise ValueError(f"Incomplete measurement in {path}")
    return [dict(zip(columns,line.split())) for line in raw[1:]]


def collect(data,rates):
    result=[]
    for n in range(4,10):
        for k in range(1,n//2+1):
            stem=f"{n}-{k}"
            full=json.loads((data/f"{stem}.json").read_text())
            short=json.loads((data/f"{stem}.short.json").read_text())
            external=json.loads((rates/f"{stem}.json").read_text())
            labels="photons negatives points max_physical max_spurious worst_physical_seed worst_spurious_seed physical_s spurious_s feynman_s failures".split()
            signed=read_rows(data/f"{stem}.signed",labels)
            expected=n-2*k+1
            if len(signed)!=expected or sorted(int(row["negatives"]) for row in signed)!=list(range(expected)):
                raise ValueError(f"Incomplete signed run for {stem}")
            ward=(data/f"{stem}.ward").read_text().strip().split()
            if len(ward)!=2 or int(ward[0])!=8*2*expected*(n-2*k):
                raise ValueError(f"Incomplete Ward probes for {stem}")
            models=[]
            for m in (4,5):
                rows=[line.split() for line in (data/f"{stem}.{m}.readback").read_text().splitlines()]
                if len(rows)!=expected or any(len(row)!=6 or int(row[0])!=m or int(row[3])!=j for j,row in enumerate(rows)):
                    raise ValueError(f"Incomplete model read-back {m}, {stem}")
                models.append(max(float(row[-1]) for row in rows))
            result.append(dict(legs=n,pairs=k,photons=n-2*k,helicity_rows=expected,
                    pole_products=full["pole_products"],external_fractions=len(short["fractions"]),
                    independent_numerator_monomials=sum(len(v["terms"]) for v in short["fractions"]),
                    uncompressed_coefficients=sum(len(v["terms"]) for v in full["fractions"]),
                    ward_checks=int(ward[0]),max_relative_ward=float(ward[1]),readback_points_per_row=3,readback_relative_errors=models,
                    signed=[{s:(float(v) if s.startswith("max") or s.endswith("_s") else int(v))
                             for s,v in r.items()} for r in signed],external=external))
    return result


def write_tex(results,path):
    def number(x):
        return f"{x:.3g}" if x<100 else f"{x:,.0f}"
    def sci(x):
        a,p=f"{x:.2e}".split("e")
        return f"{a}\\times10^{{{int(p)}}}"
    maxmg=max(v["external"]["max_polarized_relative_madgraph"] for v in results)
    maxch=max(v["external"].get("max_relative_calchep",0) for v in results)
    maxread=max(max(v["readback_relative_errors"]) for v in results)
    totalward=sum(v["ward_checks"] for v in results)
    maxward=max(v["max_relative_ward"] for v in results)
    with path.open("w") as f:
        def emit(text): print(text,file=f)
        emit(r"""\subsection{Checks of the contracted catalog}
For every family, we expanded the signed permutations in its printed
numerators and compared the resulting rational coefficients separately at
each independent propagator product. This also checks the multiplicities
of the terms fixed by a permutation. Each massive leg occurs as one
external spinor in each monomial. Each photon contributes one angle
endpoint and one square endpoint before its helicity is selected.

These counts give a check of the generic fixed-angle bound across the
catalog. A spinor product with $r$ momentum insertions grows at most as
$E^{1+r}$, and each propagator denominator grows as $E^2$. Including
the external endpoint substitutions together with their denominators
removes one power of $E$ per photon compared with the unnormalized
pair of endpoints. For every monomial we checked that the resulting
energy degree is $4-n-r_m$, where $r_m$ is the number of explicit mass
factors. Thus no monomial exceeds $E^{4-n}$ at generic fixed angles
and fixed mass.

For an additional gauge check, we replaced one external polarization
at a time by the two spinors of that photon's own null momentum. At
eight independent points for every photon position, helicity representative
and pole choice, the largest longitudinal amplitude divided by
$\sqrt{s}\,|\mathcal M_{\rm transverse}|$ was $"""+sci(maxward)+r"""$ across
"""+f"{totalward:,}"+r""" probes. We read back the two HEPCAT model tables and compared the signed complex
amplitudes with the independent evaluation of the printed polynomials.
At three phase-space points per row the largest relative difference over
all 134 rows was $"""+sci(maxread)+r"""$. The signed comparisons with the independent
QED Feynman calculation used coherent external massive spins, $m=1$ and
$e=1/3$. Table~\ref{tab:contracted-checks} gives the point counts per
helicity representative and pole choice. A signed discrepancy is measured
relative to the larger modulus of the two amplitudes.""")
        emit(r"\begin{table}[ht]\centering\small\begin{tabular}{rrrrrr}\toprule")
        emit(r"Legs & Families & Helicity rows per form & Points per row & Maximum signed relative error & Flags\\\midrule")
        for n in range(4,10):
            a=[x for x in results if x["legs"]==n]
            signed=[y for x in a for y in x["signed"]]
            counts={y['points'] for y in signed}
            if len(counts)!=1: raise ValueError("Unequal samples within one multiplicity")
            worst=max(max(y["max_physical"],y["max_spurious"]) for y in signed)
            emit(f"{n} & {len(a)} & {len(signed)} & {counts.pop():,} & ${sci(worst)}$ & {sum(y['failures'] for y in signed)}"+r"\\")
        emit(r"""\bottomrule\end{tabular}
\caption{Off-pole signed checks against an independent Feynman calculation.
Both pole choices were evaluated at every point. A flag denotes a raw
relative difference above $10^{-7}$.}\label{tab:contracted-checks}\end{table}""")
        emit(r"""We evaluated every labeled photon helicity with SPINAS external
spinors and compared it with a live MadGraph helicity calculation after
summing over the massive spins. The largest relative difference was $"""+sci(maxmg)+r"""$.
Where archived CalcHEP rates exist, we regenerated their phase-space points
and compared both full spin sums directly with those rates. The largest
relative difference was $"""+sci(maxch)+r"""$. CalcHEP 3.9.2 rejects the
nine-point processes at its external-leg limit. Its symbolic generation
did not yield complete archived rates for the eight-point families with
two, three or four charged pairs.""")
        emit(r"""\begin{table}[ht]\centering\small
\begin{tabular}{rrrrrrrr}\toprule
Legs & Pairs & Photons & Points & Physical & Reference poles & MadGraph & CalcHEP\\\midrule""")
        for x in results:
            e=x["external"]; times=e["median_seconds_per_complete_spin_sum"]
            values=[number(1e3*times[s]) for s in ("physical","spurious","madgraph")]
            ch=number(1e3*e['archived_calchep_seconds']) if 'archived_calchep_seconds' in e else r""
            emit(" & ".join(map(str,[x['legs'],x['pairs'],x['photons'],e['points'],*values,ch]))+r"\\")
        emit(r"""\bottomrule\end{tabular}
\caption{Milliseconds per complete spin sum. The physical and reference-pole
columns evaluate the contracted polynomials directly with SPINAS external
spinors. The MadGraph column sums standalone helicity calls on the same
points. These three columns give the median per point with model setup,
phase-space generation and external spinor initialization outside the timer.
The CalcHEP column gives the earlier median timing for its native spin-summed
routine. Empty entries have no measured rate. The two-point entries are
indicative timings.}\label{tab:contracted-times}\end{table}
\begin{figure}[ht]\centering
\includegraphics[width=.89\linewidth]{contracted-rates.pdf}
\caption{Complete spin sums from Table~\ref{tab:contracted-times}. The vertical
scale is logarithmic. Each panel follows one charged-pair multiplicity as
photons are added. The near coincidence of the two spinor curves reflects
their shared contraction pattern. MadGraph is faster than this direct
polynomial evaluator on these samples.}\label{fig:contracted-times}\end{figure}""")


if __name__=="__main__":
    parser=argparse.ArgumentParser(description=__doc__)
    for name in ("data","rates","json","tex"): parser.add_argument(name,type=Path)
    args=parser.parse_args()
    data=collect(args.data,args.rates)
    args.json.write_text(json.dumps(data,indent=2)+"\n")
    write_tex(data,args.tex)
