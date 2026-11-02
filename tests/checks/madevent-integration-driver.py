"""Opt-in genuine MadEvent integration checks for the public eeAA export.

The companion Wolfram suite creates and builds the exported process. This
helper performs actual survey/refine and unweighted-event runs, then checks
parameter-card changes. It never alters the installed MadGraph tree.
"""
import argparse
import gzip
import importlib.util
import json
import math
from pathlib import Path
import re
import subprocess
import sys

sys.dont_write_bytecode = True


def require(condition, message):
    if not condition:
        raise RuntimeError(message)


def load_runtime(project):
    path = project / "madevent-runtime.py"
    spec = importlib.util.spec_from_file_location("hepcat_exported_madevent", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def set_lha_value(path, block_name, code, value):
    lines = path.read_text().splitlines()
    block = None
    found = 0
    for index, line in enumerate(lines):
        start = re.match(r"\s*block\s+(\w+)", line, re.I)
        if start:
            block = start.group(1).lower()
            continue
        if re.match(r"\s*decay\b", line, re.I):
            block = None
        tokens = line.split("#", 1)[0].split()
        if block == block_name.lower() and len(tokens) >= 2 and tokens[0] == str(code):
            lines[index] = "  %d %.17e # HEPCAT integration regression" % (code, value)
            found += 1
    require(found == 1, "Expected exactly one %s/%s card input" % (block_name, code))
    path.write_text("\n".join(lines) + "\n")


def get_lha_value(path, block_name, code):
    block = None
    for line in path.read_text().splitlines():
        start = re.match(r"\s*block\s+(\w+)", line, re.I)
        if start:
            block = start.group(1).lower()
            continue
        if re.match(r"\s*decay\b", line, re.I):
            block = None
        tokens = line.split("#", 1)[0].split()
        if block == block_name.lower() and len(tokens) >= 2 and tokens[0] == str(code):
            return float(tokens[1].replace("D", "e").replace("d", "e"))
    raise RuntimeError("Missing %s/%s card input" % (block_name, code))


def inspect_events(process, name):
    directory = process / "Events" / name
    candidates = [directory / "unweighted_events.lhe.gz", directory / "unweighted_events.lhe"]
    path = next((p for p in candidates if p.is_file()), None)
    require(path is not None, "No unweighted LHE file for " + name)
    text = gzip.open(path, "rt").read() if path.suffix == ".gz" else path.read_text()
    init = re.search(r"<init>\s*(.*?)</init>", text, re.S)
    require(init is not None, "Missing LHE initialization")
    rows = [line.split() for line in init.group(1).splitlines() if line.strip() and not line.lstrip().startswith("#")]
    require(len(rows) >= 2, "Missing integrated cross section")
    cross_section, error = map(float, rows[1][:2])
    require(math.isfinite(cross_section) and cross_section > 0, "No positive finite integrated cross section")
    require(math.isfinite(error) and error >= 0, "Invalid integration error")
    events = re.findall(r"<event(?:\s[^>]*)?>\s*(.*?)</event>", text, re.S)
    masses, spins = [], []
    for event in events:
        rows = [line.split() for line in event.splitlines() if line.strip() and not line.lstrip().startswith("#")]
        require(rows, "Empty event")
        count = int(rows[0][0])
        particles = rows[1:count + 1]
        require(len(particles) == 4, "The eeAA fixture should contain four external particles")
        require(sorted(int(row[0]) for row in particles) == [-11, 11, 22, 22], "Wrong LHE particle species")
        masses.extend(float(row[10]) for row in particles if abs(int(row[0])) == 11)
        spins.extend(float(row[12]) for row in particles if abs(int(row[0])) == 11)
    return {"Run": name, "CrossSectionPB": cross_section, "IntegrationErrorPB": error,
            "Events": len(events), "ElectronMasses": masses, "ElectronSpinUP": spins,
            "EventFile": str(path)}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--project", type=Path, required=True)
    parser.add_argument("--launcher", type=Path, required=True)
    parser.add_argument("--scaled-project", type=Path, required=True)
    parser.add_argument("--scaled-launcher", type=Path, required=True)
    parser.add_argument("--timeout", type=float, default=900)
    args = parser.parse_args()
    project = args.project.resolve()
    launcher = args.launcher.resolve()
    workspace = project / "build"
    require(workspace.is_dir(), "The compiled export must have a build directory")
    runtime = load_runtime(project)
    runs = []

    def launch(arguments, log_name):
        with (project / log_name).open("w") as log:
            result = subprocess.run([str(launcher)] + arguments, cwd=workspace,
                stdout=log, stderr=subprocess.STDOUT, timeout=args.timeout)
        require(result.returncode == 0, "MadEvent launch failed; inspect " + str(project / log_name))

    launch(["--prepare"], "madevent-prepare-test.log")
    process = workspace / "madevent"
    require(process.is_dir(), "MadEvent was not created in the launch directory")
    require(not (project / "madevent").exists(), "MadEvent unexpectedly wrote to the exported source directory")
    require((workspace / ".hepcat-madevent-support").is_dir(), "Private staging did not follow the launch directory")
    manifest_path = process / "hepcat-madevent.json"
    manifest = json.loads(manifest_path.read_text())
    require(manifest.get("Status") == "READY", "Native numerical process is not READY")
    require(manifest.get("Backend") == "HEPCAT SPINAS", "Wrong numerical matrix element")
    require(manifest.get("WorkingDirectory") == str(workspace), "Working-directory metadata does not match the launch location")
    preparation_output = (project / "madevent-prepare-test.log").read_text()
    require(all(str(path) in preparation_output for path in
        [process / "index.html", process / "Events", process / "Cards"]),
        "Startup did not explain the HTML, event and card locations")
    hashes_before = manifest["MatrixHashes"]
    run_card = project / "integration-run-card.dat"
    run_card.write_text((process / "Cards" / "run_card.dat").read_text())
    for key, value in {"lpp1": 0, "lpp2": 0, "ebeam1": 5, "ebeam2": 5,
                       "nevents": 20, "iseed": 24681357, "pta": 1,
                       "etaa": 5, "draa": .1, "nhel": 0,
                       "hel_recycling": ".False."}.items():
        runtime.set_card_value(run_card, key, value)
    param_card = project / "integration-param-card.dat"
    param_card.write_text((process / "Cards" / "param_card.dat").read_text())
    initial_param_card = project / "integration-initial-param-card.dat"
    initial_param_card.write_text(param_card.read_text())

    def integrate(name):
        commands = project / (name + ".commands")
        commands.write_text("generate_events " + name + " -f\nquit\n")
        launch(["--run-card", str(run_card), "--param-card", str(param_card),
                "--command-file", str(commands)], name + ".log")
        row = inspect_events(process, name)
        require(row["Events"] == 20, "The requested unweighted event count was not produced")
        require(row["ElectronSpinUP"] and all(spin == 9 for spin in row["ElectronSpinUP"]),
                "Unidentified massive spin bases must not become LHE helicity claims")
        require(json.loads(manifest_path.read_text())["MatrixHashes"] == hashes_before,
                "Numerical integration replaced the SPINAS matrix hook")
        runs.append(row)
        return row

    baseline = integrate("hepcat_base")
    require(all(abs(mass - 1) < 1e-10 for mass in baseline["ElectronMasses"]), "Initial model mass was lost")
    # This fixture explicitly uses SM-full with QED electron/photon restrictions.
    # e doubles when alpha^{-1} is divided by four, hence sigma scales as e^4.
    alpha_inverse = get_lha_value(param_card, "sminputs", 1)
    set_lha_value(param_card, "sminputs", 1, alpha_inverse / 4)
    charged = integrate("hepcat_double_charge")
    ratio = charged["CrossSectionPB"] / baseline["CrossSectionPB"]
    relative_error = math.hypot(charged["IntegrationErrorPB"] / charged["CrossSectionPB"],
                                baseline["IntegrationErrorPB"] / baseline["CrossSectionPB"])
    require(abs(ratio / 16 - 1) <= max(.1, 6 * relative_error),
            "Live charge change did not produce the QED e^4 rate scaling")
    set_lha_value(param_card, "mass", 11, 2)
    set_lha_value(param_card, "yukawa", 11, 2)
    massive = integrate("hepcat_changed_mass")
    require(all(abs(mass - 2) < 1e-10 for mass in massive["ElectronMasses"]),
            "Updated mass did not reach the generated events")
    require(not math.isclose(massive["CrossSectionPB"], charged["CrossSectionPB"], rel_tol=1e-3),
            "A large mass change unexpectedly left the rate unchanged")
    # A deliberately scaled HEPCAT amplitude with the identical UFO theory is
    # an integration-level witness that the patched kernel is actually used.
    # A silent fallback to MG's own HELAS expression would leave this rate
    # unchanged instead of producing the required factor of four.
    scaled_project = args.scaled_project.resolve()
    scaled_launcher = args.scaled_launcher.resolve()
    scaled_workspace = scaled_project / "build"
    def launch_scaled(arguments, log_name):
        log_path = scaled_project / log_name
        with log_path.open("w") as log:
            result = subprocess.run([str(scaled_launcher)] + arguments,
                cwd=scaled_workspace, stdout=log, stderr=subprocess.STDOUT,
                timeout=args.timeout)
        require(result.returncode == 0, "Scaled MadEvent launch failed; inspect " + str(log_path))
    launch_scaled(["--prepare"], "madevent-prepare-test.log")
    scaled_process = scaled_workspace / "madevent"
    require(scaled_process.is_dir() and not (scaled_project / "madevent").exists(),
            "The scaled export did not keep output in its launch directory")
    scaled_commands = scaled_project / "hepcat_scaled.commands"
    scaled_commands.write_text("generate_events hepcat_scaled -f\nquit\n")
    launch_scaled(["--run-card", str(run_card), "--param-card", str(initial_param_card),
                   "--command-file", str(scaled_commands)], "hepcat_scaled.log")
    scaled = inspect_events(scaled_process, "hepcat_scaled")
    require(scaled["Events"] == 20, "The scaled integrand did not produce its requested events")
    require(all(abs(mass - 1) < 1e-10 for mass in scaled["ElectronMasses"]),
            "The scaled comparison must use the baseline mass")
    scaled_ratio = scaled["CrossSectionPB"] / baseline["CrossSectionPB"]
    scaled_error = math.hypot(scaled["IntegrationErrorPB"] / scaled["CrossSectionPB"],
                              baseline["IntegrationErrorPB"] / baseline["CrossSectionPB"])
    require(abs(scaled_ratio / 4 - 1) <= max(.1, 6 * scaled_error),
            "A doubled HEPCAT amplitude did not produce four times the rate; possible native-matrix fallback")
    runs.append(scaled)
    report = {"Status": "PASS", "Runs": runs, "ChargeRateRatio": ratio,
              "WorkingDirectory": str(workspace), "MadEventDirectory": str(process),
              "ScaledWorkingDirectory": str(scaled_workspace),
              "ScaledAmplitudeRateRatio": scaled_ratio, "ExpectedScaledAmplitudeRateRatio": 4,
              "ExpectedChargeRateRatio": 16,
              "Scope": "Genuine LO MadEvent integration and LHE output using the exported HEPCAT/SPINAS matrix element."}
    output = project / "madevent-integration-test.json"
    output.write_text(json.dumps(report, indent=2) + "\n")
    print("MADEVENT_INTEGRATION_PASS " + str(output))


if __name__ == "__main__":
    main()
