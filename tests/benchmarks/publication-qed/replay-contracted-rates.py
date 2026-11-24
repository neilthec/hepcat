"""Check both pole forms against live polarized MadGraph and archived CalcHEP rates.

The CalcHEP record fixes the seed, model parameters, crossings and averaging.
Runs use one process at a time. A missing external result stays unreported.
"""
import argparse
from pathlib import Path
import json
import statistics
import subprocess


def replay(args):
    reference = json.loads(args.references.read_text())
    libraries = json.loads(args.libraries.read_text())
    args.output.mkdir(parents=True, exist_ok=True)
    for measurement in reference:
        family = measurement["family"]
        stems = f"{measurement['legs']}-{measurement['pairs']}"
        if args.family and family not in args.family:
            continue
        calchep = measurement["rate"].get("ch")
        count = args.points if calchep else args.other_points
        command = [args.executable, args.tables / f"{stems}.spinors",
                   libraries[family]["mg"], str(count), str(measurement["seed"])]
        run = subprocess.run(command, capture_output=True, text=True)
        text = run.stdout + run.stderr
        (args.output / f"{stems}.txt").write_text(text)
        if run.returncode:
            raise ValueError(f"MadGraph comparison failed for {family}: {text[-700:]}")
        values = [list(map(float, line.split())) for line in run.stdout.splitlines()
                  if line[:1].isdigit() and len(line.split()) == 8]
        if len(values) != count:
            raise ValueError(f"Incomplete sample for {family}")
        rec = dict(family=family, legs=measurement["legs"], pairs=measurement["pairs"],
                   points=count, first_seed=measurement["seed"],
                   fixed_photon_helicities=2**measurement["photons"],
                   max_polarized_relative_madgraph=max(row[4] for row in values),
                   median_seconds_per_complete_spin_sum=dict(zip(("physical","spurious","madgraph"),
                       [statistics.median(row[i] for row in values) for i in (5,6,7)])))
        if calchep:
            common = min(count,len(calchep["point_weights"]))
            rec["matched_calchep_points"] = common
            rec["max_relative_calchep"] = max(
                abs(extra[i]-expected)/abs(expected)
                for extra,expected in zip(values,calchep["point_weights"]) for i in (1,2))
            if rec["max_relative_calchep"] > 1e-6:
                raise ValueError(f"CalcHEP mismatch for {family}: {rec}")
            rec["archived_calchep_seconds"] = statistics.median(calchep["seconds_per_point"])
        (args.output / f"{stems}.json").write_text(json.dumps(rec,indent=2)+"\n")
        print(family, count, rec["max_polarized_relative_madgraph"],
              rec.get("max_relative_calchep"),flush=True)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    for key in ("executable","tables","libraries","output"):
        parser.add_argument(key,type=Path)
    parser.add_argument("--references",type=Path,default=Path(__file__).with_name("rate-study.json"))
    parser.add_argument("--points",type=int,default=24)
    parser.add_argument("--other-points",type=int,default=2)
    parser.add_argument("--family",action="append",default=[])
    args=parser.parse_args()
    if min(args.points,args.other_points)<1:
        parser.error("Point counts must be positive")
    replay(args)
