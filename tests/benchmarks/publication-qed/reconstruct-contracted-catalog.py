"""Rebuild the 18 external-spinor polynomials from the on-shell QED model.

Run from any directory with a native build of the three-pair-photon-sewings
driver. The destination is new scratch space. Each exact coefficient check is
performed before moving to the next family.
"""
import argparse
import json
from pathlib import Path
import subprocess
import sys


def reconstruct(executable, destination):
    tools = Path(__file__).resolve().parent
    tests = tools.parents[1]
    destination.mkdir(parents=True,exist_ok=False)
    blocks = destination / "on-shell-blocks"
    subprocess.run([sys.executable, tests/"checks/reconstruction-block-inputs.py",
                    tests.parent, blocks, "--external-numerators"],check=True)
    for n in range(4,10):
        for pairs in range(1,n//2+1):
            legs = ["e","E"] * pairs
            if pairs == 2:
                legs[2:] = ["E","e"]
            legs += ["A.+"]*(n-len(legs))
            name = f"{n}-{pairs}"
            folder = destination / name
            full = destination / f"{name}.json"
            short = destination / f"{name}.short.json"
            numeric = destination / f"{name}.spinors"
            subprocess.run([executable,"--external",blocks,folder,*legs],check=True)
            for cmd in (["contracted-polynomials.py",folder,full],
                        ["contracted-symmetry.py",full,short],
                        ["check-contracted-symmetry.py",full,short],
                        ["contracted-table.py",full,numeric]):
                subprocess.run([sys.executable,tools/cmd[0],*cmd[1:]],check=True)
            if json.loads(full.read_text())["particles"] != legs:
                raise ValueError(f"Incorrect external order for {name}")
    subprocess.run([sys.executable,tools/"render-contracted-catalog.py",destination,
                    destination/"contracted-catalog.tex"],check=True)


if __name__ == "__main__":
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument("executable",type=Path)
    parser.add_argument("destination",type=Path)
    args=parser.parse_args()
    reconstruct(args.executable.resolve(),args.destination.resolve())
