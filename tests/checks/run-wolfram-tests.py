"""Run HEPCAT tests with a hard timeout and clean up their kernel processes."""
import argparse
import math
import os
import platform
from pathlib import Path
import shutil
import signal
import subprocess
import sys


TEST_SUITES = (
    "legacy-package-tests.wl", "legacy-algebra-tests.wl",
    "legacy-transform-tests.wl", "legacy-model-tests.wl",
    "spinor-identity-tests.wl", "thesis-preparation-tests.wl", "diagram-tests.wl",
    "simplification-path-tests.wl", "physical-residue-tests.wl",
    "spurious-pole-tests.wl", "qed-five-point-simplification-tests.wl",
    "high-energy-tests.wl", "reference-independence-tests.wl",
    "amplitude-completion-tests.wl", "completion-input-fit-tests.wl",
    "bose-symmetry-tests.wl", "fermion-symmetry-tests.wl",
    "model-parameter-tests.wl", "feynman-tests.wl", "spinas-export-tests.wl",
    "amplitude-check-tests.wl",
    "model-isolation-tests.wl",
    "plain-text-tests.wl", "model-export-tests.wl", "qed-model-export-tests.wl",
    "six-point-completion-tests.wl",
    "preparation-remainders-tests.wl",
    "completed-amplitude-preparation-tests.wl",
    "final-amplitude-simplification-tests.wl",
    "parallel-execution-tests.wl", "parallel-preparation-tests.wl",
    "parallel-amplitude-check-tests.wl", "parallel-completion-tests.wl",
    "construct-amplitude-tests.wl",
)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--kernel", default=shutil.which("WolframKernel") or
                        "/Applications/Wolfram.app/Contents/MacOS/WolframKernel")
    parser.add_argument("--timeout", type=float, default=300)
    parser.add_argument("--suite", action="append", choices=TEST_SUITES,
                        help="Run only this suite; repeat to select several. Default: all suites.")
    parser.add_argument("--strict-known-issues", action="store_true",
                        help="Compatibility flag; repaired legacy probes already fail normally on regression.")
    args = parser.parse_args()
    if not math.isfinite(args.timeout) or args.timeout <= 0:
        parser.error("--timeout must be finite and positive")
    directory = Path(__file__).resolve().parent
    tests = args.suite or TEST_SUITES
    env = dict(os.environ)
    if args.strict_known_issues:
        env["HEPCAT_STRICT_KNOWN_ISSUES"] = "1"
    for name in ("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS", "MXNET_CPU_WORKER_NTHREADS"):
        env[name] = "1"
    if platform.system() == "Darwin" and platform.machine() == "arm64":
        env["OMP_NUM_THREADS"] = "4"
    env["OMP_DYNAMIC"] = "FALSE"
    for test in tests:
        print(f"Running {test}", flush=True)
        env["HEPCAT_TEST_SUITE_PATH"] = str(directory / test)
        process = subprocess.Popen([args.kernel, "-script", str(directory / "run-suite.wl"),
                                    str(directory / test)],
                                   stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                                   text=True, env=env, start_new_session=True)
        try:
            output, _ = process.communicate(timeout=args.timeout)
        except (subprocess.TimeoutExpired, KeyboardInterrupt) as exc:
            try:
                os.killpg(process.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            output, _ = process.communicate()
            print(output)
            print(f"{test}: {'timed out' if isinstance(exc, subprocess.TimeoutExpired) else 'interrupted'}; kernel group terminated.")
            return 124 if isinstance(exc, subprocess.TimeoutExpired) else 130
        print(output)
        if process.returncode:
            return process.returncode
        if "HEPCAT_TEST_COMPLETED:" + test not in output.splitlines():
            print(f"{test}: no completion marker; treating the run as incomplete.")
            return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
