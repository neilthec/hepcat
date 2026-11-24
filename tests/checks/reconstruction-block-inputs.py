"""Prepare the on-shell input blocks for the QED charged-sewing experiment.

Run: python3 tests/checks/reconstruction-block-inputs.py ROOT NEW_WORK_DIRECTORY
The second directory must be new. All physics inputs come from ROOT/models.
"""
import argparse
from pathlib import Path
import shutil


def legs(row):
    return tuple(x.strip() for x in row.split("|")[:-2] if x.strip())


def emission(helicity):
    # Project the s12 pole of e E E e onto the two helicity spinors of the
    # auxiliary pair. The surviving terms are <r b>[k a] + <r a>[k b] or
    # their parity image. The elementary x vertex fixes the charge convention.
    angle, square = ("Angle", "Square") if helicity == "+" else ("Square", "Angle")
    def spin(kind, chirality, label):
        return f'Spinor["{kind}","{chirality}",{label}]'
    def bracket(left, right):
        return f"SpinorChain[{left},{right}]"
    ref = spin("Xi", angle, 901)
    photon = spin("Helicity", square, 3)
    terms = [bracket(ref, spin("Spin", angle, i)) + "*" + bracket(photon, spin("Spin", square, j))
             for i, j in ((1, 2), (2, 1))]
    return f'e | E | A.{helicity} | -Sqrt[2]*EE | ({" + ".join(terms)})/{bracket(ref,spin("Helicity",angle,3))}'


def build(root, working):
    source = root / "models"
    rows = (source / "amps4.mdl").read_text().splitlines()
    matches = [row for row in rows if legs(row) == ("e", "E", "E", "e")]
    if len(matches) != 1 or "PendingConstructiveDerivation:" in matches[0]:
        raise ValueError("The checked complete four-fermion amplitude is required")
    working.mkdir(parents=True, exist_ok=False)
    models = working / "models"
    blocks = working / "charged-blocks"
    models.mkdir()
    blocks.mkdir()
    for name in ("vars", "func", "prtcls"):
        for number in (3, 4):
            shutil.copyfile(source / f"{name}{number}.mdl", models / f"{name}{number}.mdl")
        shutil.copyfile(source / f"{name}3.mdl", blocks / f"{name}3.mdl")
    shutil.copyfile(source / "amps3.mdl", models / "amps3.mdl")
    fallback = {legs(x): x for x in (source / "amps5.mdl").read_text().splitlines() if "|" in x}
    for i, row in enumerate(rows):
        if "PendingConstructiveDerivation:" in row:
            candidate = fallback.get(legs(row))
            if candidate and "PendingConstructiveDerivation:" not in candidate:
                rows[i] = candidate
    (models / "amps4.mdl").write_text("\n".join(rows) + "\n")
    (blocks / "amps3.mdl").write_text("\n".join(rows[:3] +
        [matches[0], emission("+"), emission("-"), rows[-1]]) + "\n")
    (working / "tests").mkdir()
    (working / "tests/reference-models").symlink_to((root / "tests/reference-models").resolve(),
                                                     target_is_directory=True)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("root", type=Path)
    parser.add_argument("working", type=Path)
    args = parser.parse_args()
    build(args.root, args.working)
