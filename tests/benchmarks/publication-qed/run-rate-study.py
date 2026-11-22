"""Time complete QED rates on matched points using a manifest of compiled libraries."""
import argparse
import json
import math
from pathlib import Path
import subprocess

BACKENDS = {"mg": "MadGraph", "ch": "CalcHEP", "physical": "SPINAS", "spurious": "SPINAS",
            "table_physical": "SPINAS", "table_spurious": "SPINAS"}


def weights(path):
    return [float(line.split()[1]) for line in path.read_text().splitlines() if line.startswith("value ")]


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("manifest", type=Path)
    p.add_argument("timer", type=Path)
    p.add_argument("destination", type=Path)
    p.add_argument("--points", type=int, default=24)
    p.add_argument("--sweeps", type=int, default=3)
    p.add_argument("--base-seed", type=int, default=896013)
    p.add_argument("--family", help="Measure just this family from the manifest")
    args = p.parse_args()
    manifest = json.loads(args.manifest.read_text())
    args.destination.mkdir(parents=True, exist_ok=True)
    for family, builds in manifest.items():
        if args.family and family != args.family:
            continue
        pairs, photons = family.count("e") // 2, family.count("A")
        if family != "ee" * pairs + "A" * photons or "mg" not in builds:
            raise ValueError("Incomplete family or missing reference: " + family)
        seed = args.base_seed + 1000 * len(family) + 100 * pairs
        measured = {}
        for key, library in builds.items():
            file = args.destination / f"{family}-{key}.txt"
            print(f"{family}  {key}", flush=True)
            subprocess.run([args.timer.resolve(), BACKENDS[key], library, str(pairs), str(photons),
                            str(args.points), str(args.sweeps), str(seed), file], check=True)
            measured[key] = weights(file)
            if len(measured[key]) != args.points or not all(math.isfinite(x) and x > 0 for x in measured[key]):
                raise ValueError(f"Incomplete or invalid weights in {file}")
        reference = measured["mg"]
        for key, values in measured.items():
            err = max(abs(v-w)/max(v,w) for v,w in zip(values, reference))
            print(f"  {key}  maximum relative difference from MadGraph {err:.4g}", flush=True)
            if err > 1e-7:
                raise RuntimeError(f"{family} {key} exceeds 1e-7; all raw results retained")


if __name__ == "__main__":
    main()
