"""Compare native spin schedules with the preceding scalar exporter on identical QED inputs."""
import argparse
import gzip
import hashlib
import importlib.util
import json
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile

HERE = Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location("cache_study", HERE / "replay-cache-study.py")
replay = importlib.util.module_from_spec(spec)
spec.loader.exec_module(replay)


def digest(path):
    with path.open("rb") as stream:
        checksum = hashlib.sha256()
        for block in iter(lambda: stream.read(1024*1024), b""):
            checksum.update(block)
        return checksum.hexdigest()


def compare(args, row, references, representatives, folder):
    family, pole = row["family"], args.pole
    expr = []
    if family in representatives:
        dest = folder / "inputs"
        dest.mkdir()
        for word, name in representatives[family][pole].items():
            path = Path(name)
            with (gzip.open if path.suffix == ".gz" else open)(path, "rb") as src:
                with (dest / (word + ".expr")).open("wb") as out:
                    shutil.copyfileobj(src, out)
        expr.append(dest)
    libs = {"mg": references[family]["mg"]}
    hashes = []
    stats = []
    for tag, exporter in (("old", args.old), ("new", args.new)):
        source = folder / tag
        replay.run(exporter, row["pairs"], row["photons"], 4 if pole == "physical" else 5, source, *expr)
        hashes.append([digest(source / name) for name in ("process.cpp", "process.hpp")])
    if hashes[0] == hashes[1]:
        return dict(family=family, pole=pole, identical_cpp=True, source_sha256=hashes[0])
    for tag in ("old", "new"):
        source, build = folder / tag, folder / (tag + "-build")
        replay.run(sys.executable, HERE / "build-rate-plan.py", source, build, args.include, args.library)
        libs[tag] = build / "rate.dylib"
        stats.append(json.loads((build / "plan-stats.json").read_text()))
        shutil.rmtree(source)
    return measure(args, row, libs, folder, hashes, stats)


def measure(args, row, libs, folder, hashes, stats):
    family = row["family"]
    polarized = replay.run(args.checker, libs["old"], libs["new"], row["pairs"], row["photons"], 4, row["seed"], libs["mg"])
    result = dict(family=family, pole=args.pole, points=args.points, seed=row["seed"],
                  source_sha256=hashes, polarized=dict(line.split() for line in polarized.splitlines()),
                  schedule=stats[1], layout="sequential")
    # Rotate the three executables so each runs once at each position.
    tags = ["old", "new", "mg"]
    samples = {key: [] for key in tags}
    for turn in range(3):
        for key in tags[turn:] + tags[:turn]:
            log = folder / "rate.txt"
            replay.run(args.timer, "MadGraph" if key == "mg" else "SPINAS", libs[key], row["pairs"],
                       row["photons"], args.points, 1, row["seed"], log)
            fields = {}
            for line in log.read_text().splitlines():
                k, v = line.split()
                fields.setdefault(k, []).append(float(v))
            if len(fields["value"]) != args.points:
                raise ValueError("Incomplete phase-space sweep")
            samples[key].append(fields)
    weights = samples["mg"][0]["value"]
    for key in tags:
        result[key] = dict(seconds_per_point=[s["seconds_per_point"][0] for s in samples[key]],
                           peak_rss_bytes=[int(s["peak_rss_bytes"][0]) for s in samples[key]],
                           max_relative=max(replay.max_error(s["value"], weights) for s in samples[key]),
                           weights=samples[key][0]["value"])
    result["max_relative_previous"] = replay.max_error(result["new"]["weights"], result["old"]["weights"])
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for key in ("old", "new", "timer", "checker", "include", "library", "references", "representatives", "output"):
        parser.add_argument(key, type=Path)
    parser.add_argument("--family", action="append")
    parser.add_argument("--points", type=int, default=12)
    args = parser.parse_args()
    if args.points < 1:
        parser.error("points must be positive")
    args.output.mkdir(parents=True, exist_ok=True)
    refs = json.loads(args.references.read_text())
    reps = json.loads(args.representatives.read_text())
    for row in json.loads((HERE / "rate-study.json").read_text()):
        family = row["family"]
        if args.family and family not in args.family:
            continue
        for args.pole in ("physical", "spurious"):
            output = args.output / f"{family}-{args.pole}.json"
            if output.exists():
                continue
            print(f"{family} {args.pole}", flush=True)
            with tempfile.TemporaryDirectory(dir=args.output) as work:
                result = compare(args, row, refs, reps, Path(work))
            output.write_text(json.dumps(result, indent=2) + "\n")


if __name__ == "__main__":
    main()
