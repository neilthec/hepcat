"""Plot timings and peak memory for each changed QED spin-sum export."""
import argparse
import json
from pathlib import Path
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("study", type=Path)
    parser.add_argument("figure", type=Path)
    args = parser.parse_args()
    rows = json.loads(args.study.read_text())["families"]
    data = {(r["family"], r["pole"]): r for r in rows if "new" in r}
    families = list(dict.fromkeys(family for family, pole in data))
    labels = [f"{name.count('e') // 2} pairs, {name.count('A')} " +
              ("photon" if name.count("A") == 1 else "photons") for name in families]
    fig, axes = plt.subplots(1, 2, sharey=True, figsize=(7.0, 0.5 * len(families) + 1.2),
                             layout="constrained")
    for ax, metric, title in zip(axes, ["median_seconds_per_point", "median_peak_rss_mib"],
                                  ["Time per complete rate", "Peak resident memory"]):
        ax.axvline(1, color="#555555", linewidth=0.9, linestyle="--")
        for pole, offset, color, marker in [("physical", -0.12, "#0072B2", "o"),
                                            ("spurious", 0.12, "#D55E00", "s")]:
            x, y = [], []
            for i, family in enumerate(families):
                row = data.get((family, pole))
                if row:
                    x.append(row["new"][metric] / row["old"][metric])
                    y.append(i + offset)
            ax.scatter(x, y, marker=marker, s=32, color=color, label="All" + pole.capitalize())
        ax.set(title=title, xlabel="Updated / previous")
        ax.set_xlim(0, max(1.06, ax.get_xlim()[1]))
        ax.grid(axis="x", alpha=0.2)
    axes[0].set_yticks(range(len(labels)), labels)
    axes[0].invert_yaxis()
    axes[1].legend(loc="lower left", fontsize=8)
    args.figure.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(args.figure)
    plt.close(fig)


if __name__ == "__main__":
    main()
