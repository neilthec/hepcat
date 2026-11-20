#!/usr/bin/env python3
"""Compare native all-positive formulas on the publication branch's fixed MadGraph streams."""
import argparse
import gzip
import hashlib
import json
from pathlib import Path
import shutil
import subprocess
import zipfile


def parse_args():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--publication-archive", required=True, type=Path,
                        help="Publication branch tests/benchmarks/benchmarks.zip")
    parser.add_argument("--validator", required=True, type=Path, help="Built qed-all-positive")
    parser.add_argument("--hepcat-qed", required=True, type=Path, help="Built native hepcat-qed")
    parser.add_argument("--root", required=True, type=Path, help="HEPCAT checkout containing models/")
    parser.add_argument("--general-library", help="MadGraph library path template containing {n}")
    parser.add_argument("--model-library", help="MadGraph library path template containing {n}")
    parser.add_argument("--model-n8-library", help="Optional distinct model library for N=8")
    parser.add_argument("--archive-rates", type=Path,
                        help="Optional dataset with general/ and public/printed/ rate streams")
    parser.add_argument("--model-summed-spurious", action="store_true",
                        help="Also use component-summed virtualities for the model spurious formula")
    parser.add_argument("--campaign", choices=["all", "general", "model"], default="all")
    parser.add_argument("--photons", type=int, choices=range(2, 9))
    parser.add_argument("--output", required=True, type=Path, help="New directory for receipts")
    args = parser.parse_args()
    args.validator = args.validator.resolve()
    args.hepcat_qed = args.hepcat_qed.resolve()
    if args.photons == 2 and args.campaign == "model":
        parser.error("the model-parameter campaign starts at three photons")
    for name in ("general", "model"):
        if args.campaign in ("all", name) and not getattr(args, name + "_library"):
            parser.error(f"--{name}-library is required for this campaign")
    return args


def digest(path):
    with gzip.open(path, "rb") as src:
        sha = hashlib.sha256()
        for chunk in iter(lambda: src.read(262144), b""):
            sha.update(chunk)
        return sha.hexdigest()


def compare(args, campaign, n, form, reference, published):
    general = campaign == "general"
    prefix = args.output / f"{campaign}-N{n}-{form}"
    masses = str(published["Mass"]) if general else "0.0005"
    count = published["Points" if general else "compared"]
    seed = published["Seed" if general else "seed"]
    old_failures = published["Failures" if general else "above_tolerance"]
    summed = not general and (form == "physical" or args.model_summed_spurious)
    command = [
        args.validator, "--photons", str(n), "--points", str(count), "--seed", str(seed),
        "--control-points", "0", "--mass", masses,
        "--minimum-energy", "10" if general else "1.5", "--maximum-energy", "100" if general else "6",
        "--poles", "summed" if summed else "expanded", "--output-mode", "outliers", "--output", prefix,
        "--check", form, "--reference", reference,
    ]
    if not general:
        command += ["--charge", "0.31333"]
    rates = None
    if args.archive_rates:
        if general:
            path = args.archive_rates / "general" / f"positive-N{n}-values.csv.gz"
            if path.exists():
                rates = path
        else:
            rates = args.archive_rates / "public" / "printed" / f"{form}-{n}" / "rates.csv.gz"
            if digest(rates) != published["rates_csv_sha256"]:
                raise ValueError(f"Archived rate checksum differs from the publication at {rates}")
    with prefix.with_suffix(".log").open("w") as log:
        if rates is None:
            status = subprocess.run(command, stdout=log, stderr=subprocess.STDOUT).returncode
        else:
            command += ["--archived-rates", "raw" if general else "averaged"]
            with gzip.open(rates, "rb") as source, subprocess.Popen(
                    command, stdin=subprocess.PIPE, stdout=log, stderr=subprocess.STDOUT) as proc:
                try:
                    shutil.copyfileobj(source, proc.stdin, 262144)
                    proc.stdin.close()
                except BrokenPipeError:
                    pass
                status = proc.wait()
    if status not in (0, 1):
        raise RuntimeError(f"Formula checker failed: {prefix}.log")
    result = json.loads(prefix.with_suffix(".json").read_text())
    if result["Points"] != count or result["Seed"] != seed or result["StoredRows"] != result["Failures"]:
        raise RuntimeError(f"Incomplete numerical receipt: {prefix}")
    print(f"{prefix.name}: {count} points, {result['LiveMadGraphPoints']} fresh MadGraph evaluations, "
          f"{result['Failures']} raw outliers (publication {old_failures})", flush=True)
    passes = result["Failures"] <= old_failures
    if result["Failures"]:
        replay = [args.hepcat_qed, "--root", args.root, ("--replay-model-summed" if summed else "--replay-" + campaign), prefix,
                  "--points", str(count), "--photons", str(n)]
        with prefix.with_suffix(".replay.log").open("w") as log:
            status = subprocess.run(replay, stdout=log, stderr=subprocess.STDOUT).returncode
        print(prefix.with_suffix(".replay.log").read_text().strip(), flush=True)
        if status > 1 or not Path(str(prefix) + "-precision.tsv").is_file():
            raise RuntimeError(f"Precision replay failed: {prefix}")
        # A comparison on projected input cannot clear a failure at the recorded event.
        passes &= status == 0
    return passes


def main():
    args = parse_args()
    args.output.mkdir(parents=True, exist_ok=False)
    with (args.output / "low-N-controls.log").open("w") as log:
        subprocess.run([args.validator, "--self-test"], stdout=log, stderr=subprocess.STDOUT, check=True)
    passed = True
    with zipfile.ZipFile(args.publication_archive) as archive:
        root = "tests/benchmarks/reproduction/100k"
        if args.campaign in ("all", "general"):
            for n in range(2, 9):
                if args.photons and args.photons != n:
                    continue
                data = json.loads(archive.read(f"{root}/general/results/positive-N{n}.json"))
                passed &= compare(args, "general", n, "both", args.general_library.format(n=n), data)
        if args.campaign in ("all", "model"):
            rows = json.loads(archive.read(f"{root}/all-positive/results.json"))["current"]
            for data in rows:
                n = data["photons"]
                if args.photons and args.photons != n:
                    continue
                lib = args.model_n8_library if n == 8 and args.model_n8_library else args.model_library
                passed &= compare(args, "model", n, data["form"], lib.format(n=n), data)
    return 0 if passed else 1


if __name__ == "__main__":
    raise SystemExit(main())
