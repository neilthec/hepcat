"""Extend the native notebook cases by the certified generation permutations."""
from collections import defaultdict
import csv
import json
from pathlib import Path
import re

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]
FIXTURE = ROOT / "tests/checks/fixtures/sm-factorization-catalog.json"


def species(legs):
    return tuple(sorted(leg.split(".")[0] for leg in legs.split(",")))


def rename(legs, particles):
    return ",".join(particles.get(leg, leg) for leg in legs.split(","))


def main():
    fixture = json.loads(FIXTURE.read_text())
    with (HERE / "catalog.tsv").open() as source:
        reader = csv.DictReader(source, delimiter="\t", quoting=csv.QUOTE_NONE)
        fields = reader.fieldnames
        rows = list(reader)
    families = defaultdict(list)
    for row in rows:
        families[row["family"]].append(row)
    seen = {species(members[0]["particles"]) for members in families.values()}
    added = []
    for members in families.values():
        for i, mapping in enumerate(fixture["FlavorMaps"]):
            particles = mapping["ParticleMap"]
            key = species(rename(members[0]["particles"], particles))
            if key in seen:
                continue
            seen.add(key)
            for original in members:
                row = original.copy()
                for k in ("case_id", "family"):
                    row[k] += f"_g{i:02}"
                for k in ("particles", "incoming", "outgoing"):
                    row[k] = rename(row[k], particles)
                masses = mapping["MassMap"]
                row["expression"] = re.sub(
                    r'(ModelParameter\[")([^"]+)', lambda m: m[1] + masses.get(m[2], m[2]),
                    row["expression"])
                added.append(row)
    signatures = {tuple(sorted(r["particles"].split(","))) for r in rows + added}
    expected = {tuple(sorted(r["Particles"])) for r in fixture["Entries"]}
    expected.update(tuple(sorted(legs)) for legs in fixture["KnownZeroMultisets"])
    if signatures != expected or len(signatures) != 361:
        raise ValueError(f"Incorrect closure, missing {expected-signatures}, extra {signatures-expected}")
    with (HERE / "flavor-closure.tsv").open("w") as target:
        writer = csv.DictWriter(target, fields, delimiter="\t", quoting=csv.QUOTE_NONE, quotechar=None, lineterminator="\n")
        writer.writeheader()
        writer.writerows(added)
    print(f"{len(added)} additional helicity/crossing cases, {len(rows)+len(added)} total; "
          f"{len(signatures)} distinct external multisets, including four vanishing helicity sectors.")


if __name__ == "__main__":
    main()
