"""Plot the timings of the external-spinor polynomial evaluations in the paper."""
import argparse
import json
from pathlib import Path
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt


def plot(source,target):
    rows=json.loads(source.read_text())
    fig,axes=plt.subplots(2,2,figsize=(7.2,4.7),layout="constrained")
    styles=[("physical","Physical poles","#0072b2","o","-"),
            ("spurious","Reference poles","#d55e00","x","--"),
            ("madgraph","MadGraph","#222222","s","-"),
            ("ch","CalcHEP","#009e73","^",":")]
    for pairs,ax in enumerate(axes.flat,1):
        group=[r for r in rows if r["pairs"]==pairs]
        for key,label,color,marker,style in styles:
            xs,ys=[],[]
            for r in group:
                external=r["external"]
                seconds=(external.get("archived_calchep_seconds") if key=="ch"
                         else external["median_seconds_per_complete_spin_sum"][key])
                if seconds is not None:
                    xs.append(r["legs"]);ys.append(1e3*seconds)
            ax.plot(xs,ys,label=label,color=color,marker=marker,ls=style,lw=1.2,ms=5)
        ax.set_yscale("log")
        ax.set_title(f"{pairs} charged pair"+("s" if pairs>1 else ""),fontsize=10)
        ax.set_xticks([v["legs"] for v in group]);ax.tick_params(labelsize=8)
        ax.grid(axis="y",alpha=.22)
    for ax in axes[1]:ax.set_xlabel("External legs")
    for ax in axes[:,0]:ax.set_ylabel("ms per spin sum")
    axes[0,0].legend(fontsize=7,loc="upper left")
    fig.savefig(target)
    plt.close(fig)


if __name__=="__main__":
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument("report",type=Path);p.add_argument("figure",type=Path)
    args=p.parse_args();plot(args.report,args.figure)
