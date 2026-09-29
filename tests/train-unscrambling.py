"""Run notebook training inputs headlessly with explicit CPU and data settings."""
import argparse
import os
import platform
from pathlib import Path
import shlex
import shutil
import signal
import subprocess
import tempfile
import time


def supervise(command, env, validation_timeout=1800, run_timeout=172800):
    """Bound native stalls outside Wolfram, including parallel model installation."""
    with tempfile.TemporaryDirectory(prefix="hepcat-status-") as directory:
        stage = Path(directory) / "stage"
        env = dict(env, HEPCAT_TRAIN_STAGE_FILE=str(stage))
        process = subprocess.Popen(command, env=env, start_new_session=True)
        started = time.monotonic()
        validation_started = None
        previous = None
        try:
            while process.poll() is None:
                current = stage.read_text().strip() if stage.exists() else "Starting Wolfram"
                now = time.monotonic()
                if current != previous:
                    print(f"Stage: {current}", flush=True)
                    previous = current
                if current == "Validation" and validation_started is None:
                    validation_started = now
                if now - started >= run_timeout or (validation_started is not None and
                        now - validation_started >= validation_timeout):
                    print("TIMEOUT: stopping this run's process group. Any saved model is retained; "
                          "validation is incomplete.", flush=True)
                    return 124
                time.sleep(0.2)
            return process.returncode
        finally:
            # Also clean up descendants if the launcher exits before its kernels.
            try:
                os.killpg(process.pid, signal.SIGTERM)
                time.sleep(1)
                process.poll()
                os.killpg(process.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            process.wait()


def training_environment(threads, base):
    env = dict(base)
    for name in ("OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS"):
        env[name] = str(threads)
    # Wolfram 15's bundled native backend stalls at OMP counts 1 and 2 on
    # the tested Apple-silicon installation. Keep other platforms unchanged.
    apple_silicon = platform.system() == "Darwin" and platform.machine() == "arm64"
    env["OMP_NUM_THREADS"] = str(max(4, threads) if apple_silicon else threads)
    env["OMP_DYNAMIC"] = "FALSE"
    # Parallelize operators, not multiple simultaneous operator thread pools.
    env["MXNET_CPU_WORKER_NTHREADS"] = "1"
    return env


def notebook_command(executable, notebook):
    return [executable, "-file", str(notebook.with_suffix(".wl"))]


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--threads", type=int, required=True,
                        help="native CPU threads per operator (OpenMP minimum 4 on Apple silicon)")
    parser.add_argument("--wolframscript", default="wolframscript")
    parser.add_argument("--validation-timeout", type=int, default=1800,
                        help="external validation-stage deadline in seconds (default 1800)")
    parser.add_argument("--run-timeout", type=int, default=172800,
                        help="external whole-run deadline in seconds (default 48 hours)")
    parser.add_argument("--kernels", type=int, help="independent data-generation and validation workers")
    controls = {
        "amplitudes": "number of reference amplitudes, or all",
        "scrambles": "training scrambles per amplitude",
        "steps": "identity applications per scramble (depth)",
        "rounds": "maximum training passes through the data",
        "holdout": "fresh validation scrambles per amplitude (0 disables validation)",
        "episode-length": "maximum identity applications per validation attempt",
        "batch-size": "training states per optimizer batch",
        "worker-threads": "native threads per symbolic/validation worker (normally 1 on Linux)",
    }
    for name, help_text in controls.items():
        value_type = (lambda value: "all" if value.lower() == "all" else int(value)) if name == "amplitudes" else int
        parser.add_argument(f"--{name}", type=value_type, help=help_text + "; default: notebook setting")
    parser.add_argument("--model-directory", type=Path, help="save this run's model in a separate directory")
    parser.add_argument("--policy-method", choices=("CandidateScorer", "ReverseMoves"),
                        default="CandidateScorer", help="candidate scorer or experimental state-only move predictor")
    parser.add_argument("--dry-run", action="store_true", help="print configuration without starting Wolfram")
    args = parser.parse_args(argv)
    if args.validation_timeout < 1 or args.run_timeout < 1:
        parser.error("timeouts must be positive seconds")
    if args.threads < 1:
        parser.error("--threads must be a positive integer")
    if args.kernels is not None and args.kernels < 1:
        parser.error("--kernels must be a positive integer")
    for name in controls:
        value = getattr(args, name.replace("-", "_"))
        minimum = 0 if name in ("steps", "holdout") else 1
        if value is not None and value != "all" and value < minimum:
            parser.error(f"--{name} must be an integer >= {minimum}")
    notebook = Path(__file__).resolve().with_suffix(".nb")
    if not notebook.is_file():
        parser.error(f"Notebook not found: {notebook}")
    executable = shutil.which(args.wolframscript)
    if not executable and not args.dry_run:
        parser.error(f"Executable not found: {args.wolframscript}")
    env = training_environment(args.threads, os.environ)
    env["HEPCAT_TRAIN_POLICY_METHOD"] = args.policy_method
    if args.kernels is not None:
        env["HEPCAT_TRAIN_KERNELS"] = str(args.kernels)
    for name in controls:
        value = getattr(args, name.replace("-", "_"))
        if value is not None:
            env["HEPCAT_TRAIN_" + name.upper().replace("-", "_")] = str(value)
    if args.model_directory is not None:
        env["HEPCAT_TRAIN_MODEL_DIRECTORY"] = str(args.model_directory.expanduser().resolve())
    command = notebook_command(executable or args.wolframscript, notebook)
    print(f"Native CPU threads requested: {args.threads}; symbolic workers: {args.kernels or 'notebook setting'}.", flush=True)
    print(f"Effective OpenMP threads: {env['OMP_NUM_THREADS']}", flush=True)
    print(f"Policy method: {args.policy_method}", flush=True)
    for name in controls:
        value = env.get("HEPCAT_TRAIN_" + name.upper().replace("-", "_"))
        if value is not None:
            print(f"{name}: {value}", flush=True)
    print(shlex.join(command), flush=True)
    if args.dry_run:
        return 0
    return supervise(command, env, args.validation_timeout, args.run_timeout)


if __name__ == "__main__":
    raise SystemExit(main())
