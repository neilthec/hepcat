"""Build the QED paper's coverage, timing and source-size tables and plots."""
import argparse
import csv
import json
import math
import re
from pathlib import Path
from statistics import median

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D

HERE = Path(__file__).resolve().parent
ENGINES = [
    ("physical", "SPINAS physical", "#0072b2", "o", "-"),
    ("spurious", "SPINAS spurious", "#cc7900", "s", "--"),
    ("ch", "CalcHEP", "#008263", "^", ":"),
    ("mg", "MadGraph", "#963f79", "D", "-."),
]


def tabular(rows, headings, align, caption, label):
    body = [r"\begin{table}[!htbp]\centering\small", r"\begin{tabular}{" + align + "}",
            r"\toprule", " & ".join(headings) + r"\\", r"\midrule"]
    for row in rows:
        body.append(" & ".join(map(str, row)) + r"\\")
    body.extend([r"\bottomrule\end{tabular}", r"\caption{" + caption + "}", r"\label{" + label + "}", r"\end{table}"])
    return "\n".join(body) + "\n"


def plots(data, output, sizes, normalized=False):
    fig, axes = plt.subplots(2, 2, figsize=(7.1, 4.75), sharey=True, layout="constrained")
    for pairs, ax in enumerate(axes.flat, 1):
        subset = [r for r in data if r["pairs"] == pairs]
        for key, label, color, marker, style in ENGINES:
            if not sizes and key in ("physical", "spurious"):
                key="table_"+key
            x, y = [], []
            for r in subset:
                x.append(r["legs"])
                if sizes:
                    y.append(r["source_bytes"].get(key, float("nan")) / 1e6)
                else:
                    sample = r["rate"].get(key)
                    y.append(median(sample["seconds_per_point"]) * 1e6 if sample else float("nan"))
            if normalized:
                y = [v/y[0] for v in y]
            ax.plot(x, y, label=label, marker=marker, color=color, linestyle=style,
                    markersize=4.5, markerfacecolor="white", linewidth=1.5)
        ax.set_title(f"{pairs} charged pair" + ("s" if pairs > 1 else ""), fontsize=10, loc="left")
        ax.set_yscale("log")
        ax.set_xticks([r["legs"] for r in subset])
        ax.set_xlim(subset[0]["legs"] - .2, 9.2)
        ax.grid(axis="y", alpha=.20)
    fig.supxlabel("Number of external particles", fontsize=10)
    fig.supylabel("Generated process source (decimal MB)" if sizes else ("Time relative to smallest multiplicity" if normalized else "Time per complete rate (µs)"), fontsize=10)
    handles = [Line2D([], [], color=c, ls=s, marker=m, markersize=4, markerfacecolor="white", label=l + ((" (direct)" if sizes else " (tables)") if k in ("physical","spurious") else ""))
               for k, l, c, m, s in ENGINES]
    fig.legend(handles=handles, loc="outside upper center", ncol=4, frameon=False, fontsize=8.5)
    fig.savefig(output, metadata={"CreationDate": None, "ModDate": None})
    plt.close(fig)


def nine_point_validation(out):
    # The paper's high-statistics receipts report the native residual before any precision replay.
    values = []
    for line in (HERE/"nine-point.md").read_text().splitlines():
        cells = [word.strip().strip(chr(96)) for word in line.strip("|").split("|")]
        if len(cells)==6 and re.fullmatch(r"[1-4]",cells[0]) and re.fullmatch(r"[+-]+",cells[1]):
            values.append((int(cells[0]),cells[1],*[float(x) for x in cells[2:]]))
    assert len(values)==10
    fig, axes = plt.subplots(2,1,figsize=(7.2,4.25),sharex=True,layout="constrained")
    for ax, start, title in zip(axes, (2,4), ("Signed Feynman amplitude", "Polarized MadGraph rate")):
        for j, (color, marker, label) in enumerate([("#0072b2","o","Physical"),("#cc7900","s","Spurious")]):
            ax.scatter([i+(-.09 if j==0 else .09) for i in range(10)], [r[start+j] for r in values],
                       color=color, marker=marker,s=25,label=label)
        for split in (3.5,6.5,8.5):
            ax.axvline(split,color=".82",lw=.9)
        ax.set_yscale("log")
        ax.set_ylim(5e-12,2e-6)
        ax.set_title(title,loc="left",fontsize=10)
        ax.grid(axis="y",alpha=.2)
    axes[0].legend(frameon=False,ncol=2,loc="upper right",fontsize=8)
    axes[1].set_xticks(range(10),[f"{r[0]}, {r[1].count('-')}" for r in values])
    fig.supxlabel("Charged pairs, negative-helicity photons",fontsize=10)
    fig.supylabel("Largest native relative discrepancy",fontsize=10)
    fig.savefig(out/"catalog-validation.pdf",metadata={"CreationDate":None,"ModDate":None})
    plt.close(fig)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("destination", type=Path)
    args = parser.parse_args()
    out = args.destination
    out.mkdir(parents=True, exist_ok=True)
    figures = out / "figures"
    figures.mkdir(exist_ok=True)
    data = json.loads((HERE / "rate-study.json").read_text())
    assert len(data) == 18 and sum(r["labeled"] for r in data) == 333
    coverage = [[r[k] for k in ["legs", "pairs", "photons", "labeled", "bose", "parity_reps", "diagrams", "cuts"]] for r in data]
    (out / "coverage-table.tex").write_text(tabular(coverage,
        ["$n$", "Charged pairs", "Photons", "Labeled", "Bose", "Parity reps.", "Diagrams", "Cuts"],
        "@{}rrrrrrrr@{}", "The complete single-flavor tree catalog. Labeled counts refer to photon helicities. "
        "Bose classes identify photon permutations. Parity representatives also identify simultaneous helicity reversal. "
        "Diagrams and physical cuts are counted per labeled amplitude.", "tab:catalog"))
    def number(v):
        if v < 1:
            return f"{v:.3g}"
        if v < 100:
            return f"{v:.3g}"
        return f"{float(f'{v:.3g}'):,.0f}"

    keys = ["physical", "spurious", "table_physical", "table_spurious", "ch", "mg"]
    sizes, timing = [], []
    for r in data:
        group = [r["legs"], r["pairs"], r["photons"]]
        missing = r"\ensuremath{\cdot}"
        sizes.append(group + [number(r["source_bytes"][k]/1e6) if k in r["source_bytes"] else missing for k in keys])
        timing.append(group + [number(median(r["rate"][k]["seconds_per_point"])*1e6) if k in r["rate"] else missing for k in keys])
    headings = ["$n$", "Pairs", "$N_\\gamma$", "Phys.", "Spur.", "Phys.", "Spur.", "CalcHEP", "MG"]
    def family_table(rows, caption, label):
        table=tabular(rows,headings,"@{}rrrrrrrrr@{}",caption,label)
        return table.replace(r"\toprule", r"\toprule & & & \multicolumn{2}{c}{SPINAS direct} & \multicolumn{2}{c}{SPINAS tables} & & \\" + "\n" + r"\cmidrule(lr){4-5}\cmidrule(lr){6-7}")
    (out/"size-table.tex").write_text(family_table(sizes,
        "Uncompressed process source in decimal MB ($10^6$ bytes). The table-based build includes its binary operand data. Every entry covers all helicities of its process. A dot marks an unavailable measurement.","tab:source-bytes"))
    (out/"rate-table.tex").write_text(family_table(timing,
        "Median microseconds per complete rate on the same 24 momenta per row. A dot marks an unavailable measurement.","tab:rate-survey"))
    with (out/"catalog-measurements.csv").open("w",newline="") as file:
        writer=csv.writer(file,lineterminator="\n")
        writer.writerow(["legs","charged_pairs","photons","engine","source_bytes","median_us_per_point","max_relative_to_madgraph"])
        for r in data:
            for k in keys:
                rate=r["rate"].get(k)
                writer.writerow([r["legs"],r["pairs"],r["photons"],k,r["source_bytes"].get(k),median(rate["seconds_per_point"])*1e6 if rate else None,rate["max_relative_to_mg"] if rate else None])
    construction = []
    for n in range(4, 10):
        families = [r for r in data if r["legs"] == n]
        totals = [sum(t["seconds"] for r in families for t in r["construction"][k]) for k in ("physical", "spurious")]
        construction.append([n, sum(r["parity_reps"] for r in families)] + [f"{v/60:,.2f}" for v in [*totals, sum(totals)]])
    (out / "construction-table.tex").write_text(tabular(construction,
        ["$n$", "Representatives per form", "Physical (min)", "Spurious (min)", "Both (min)"], "@{}rrrrr@{}",
        "Recorded HEPCAT construction and internal validation time, summed over one successful 100,000-point run "
        "per parity class and pole preference. Each row sums process times. External generator setup, independent "
        "rate comparisons, formula development and unsuccessful trials are excluded.", "tab:catalog-construction"))
    def latex_scientific(x):
        exponent=math.floor(math.log10(x))
        return rf"{x / 10**exponent:.2f}\times10^{{{exponent}}}"

    error=max(v["max_relative_to_mg"] for r in data for v in r["rate"].values())
    overlaps=sum(k in r["rate"] and "table_"+k in r["rate"]
                 for r in data for k in ("physical","spurious"))
    def ms(r,k):
        return number(1000*median(r["rate"][k]["seconds_per_point"]))
    if all("table_spurious" in r["rate"] for r in data):
        first=next(r for r in data if r["legs"]==9 and r["pairs"]==1)
        last=next(r for r in data if r["legs"]==9 and r["pairs"]==4)
        summary=(f"Across all 18 families, the largest difference from the MadGraph "
                 f"complete rate in the timing sample is ${latex_scientific(error)}$. "
                 f"The {overlaps} direct SPINAS builds with a second operand-table build "
                 "give identical double-precision weights at every sampled momentum.\n\n"
                 f"At nine points the physical-pole SPINAS operand-table time is {ms(first,'table_physical')} ms "
                 f"per point for seven-photon annihilation, compared with {ms(first,'mg')} ms for MadGraph. "
                 f"For eight fermions and one photon the corresponding times are {ms(last,'table_physical')} ms "
                 f"and {ms(last,'mg')} ms. The number of external legs alone therefore gives little guidance "
                 "about the relative cost of the two implementations.\n")
        (out/"rate-summary.tex").write_text(summary)
    nine_point_validation(figures)
    for sizes, file in [(True, "catalog-source"), (False, "catalog-rates")]:
        plots(data, figures / (file + ".pdf"), sizes)
    plots(data, figures/"catalog-growth.pdf", False, normalized=True)


if __name__ == "__main__":
    main()
