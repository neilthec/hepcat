"""Native regression for the seven-point building blocks at five points."""
import argparse
from pathlib import Path
import runpy
import subprocess
import tempfile


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("repo", type=Path)
    parser.add_argument("driver", type=Path)
    args = parser.parse_args()
    prepare = runpy.run_path(str(Path(__file__).with_name("reconstruction-block-inputs.py")))["build"]
    with tempfile.TemporaryDirectory() as tmp:
        work = Path(tmp) / "input"
        prepare(args.repo, work)
        for helicity in ("+", "-"):
            output = Path(tmp) / ("five" + helicity + ".mdl")
            subprocess.run([args.driver, work, output, "10000", "e", "E", "E", "e", "A." + helicity], check=True)
            if not output.read_text().startswith("Shared["):
                raise ValueError("The validated amplitude was not serialized")


if __name__ == "__main__":
    main()
