"""Run HEPCAT tests with a hard timeout and clean up their kernel processes."""
import argparse
import os
import platform
from pathlib import Path
import shutil
import signal
import subprocess
import sys


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--kernel", default=shutil.which("WolframKernel") or
                        "/Applications/Wolfram.app/Contents/MacOS/WolframKernel")
    parser.add_argument("--timeout", type=float, default=60)
    parser.add_argument("--neural", action="store_true", help="also exercise numerical NN training/inference")
    parser.add_argument("--parallel", action="store_true", help="also launch two owned symbolic test workers")
    args = parser.parse_args()
    if args.timeout <= 0:
        parser.error("--timeout must be positive")
    directory = Path(__file__).resolve().parent
    tests = ["schouten-tests.wl", "candidate-policy-tests.wl", "candidate-training-tests.wl",
             "notebook-scrambling-tests.wl", "inference-limits-tests.wl", "shared-state-tests.wl",
             "training-pipeline-tests.wl", "training-summary-tests.wl"]
    if args.parallel:
        if args.timeout < 150:
            parser.error("--parallel needs --timeout >= 150 to allow worker startup and cleanup")
        tests.append("parallel-pipeline-tests.wl")
    if args.neural:
        tests.append("candidate-neural-tests.wl")
        if args.parallel:
            tests.append("neural-pipeline-tests.wl")
    env = dict(os.environ)
    for name in ("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS", "MXNET_CPU_WORKER_NTHREADS"):
        env[name] = "1"
    if platform.system() == "Darwin" and platform.machine() == "arm64":
        env["OMP_NUM_THREADS"] = "4"
    env["OMP_DYNAMIC"] = "FALSE"
    for test in tests:
        print(f"Running {test}", flush=True)
        process = subprocess.Popen([args.kernel, "-script", str(directory / test)],
                                   stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                                   text=True, env=env, start_new_session=True)
        try:
            output, _ = process.communicate(timeout=args.timeout)
        except (subprocess.TimeoutExpired, KeyboardInterrupt) as exc:
            os.killpg(process.pid, signal.SIGKILL)
            output, _ = process.communicate()
            print(output)
            print(f"{test}: {'timed out' if isinstance(exc, subprocess.TimeoutExpired) else 'interrupted'}; kernel group terminated.")
            return 124 if isinstance(exc, subprocess.TimeoutExpired) else 130
        print(output)
        if process.returncode:
            return process.returncode
    return 0


if __name__ == "__main__":
    sys.exit(main())
