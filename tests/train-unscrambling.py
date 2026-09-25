"""Launch notebook training with explicit native CPU thread settings."""
import argparse
import json
import os
import platform
from pathlib import Path
import shlex
import shutil


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
    # Wolfram string syntax accepts the JSON quoting used for this path.
    path = json.dumps(str(notebook), ensure_ascii=True)
    return [executable, "-code",
            f'UsingFrontEnd[NotebookEvaluate[{path}, EvaluationElements -> "All"]]']


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--threads", type=int, required=True,
                        help="native CPU threads per operator (OpenMP minimum 4 on Apple silicon)")
    parser.add_argument("--wolframscript", default="wolframscript")
    parser.add_argument("--kernels", type=int, help="independent data-generation and validation workers")
    controls = {
        "amplitudes": "known amplitudes to use from the notebook's list",
        "scrambles": "training scrambles per amplitude",
        "steps": "identity applications per scramble (depth)",
        "rounds": "maximum training passes through the data",
        "holdout": "fresh validation scrambles per amplitude (0 disables validation)",
        "episode-length": "maximum identity applications per validation attempt",
        "batch-size": "training states per optimizer batch",
    }
    for name, help_text in controls.items():
        parser.add_argument(f"--{name}", type=int, help=help_text + "; default: notebook setting")
    parser.add_argument("--model-directory", type=Path, help="save this run's model in a separate directory")
    parser.add_argument("--dry-run", action="store_true", help="print configuration without starting Wolfram")
    args = parser.parse_args(argv)
    if args.threads < 1:
        parser.error("--threads must be a positive integer")
    if args.kernels is not None and args.kernels < 1:
        parser.error("--kernels must be a positive integer")
    for name in controls:
        value = getattr(args, name.replace("-", "_"))
        minimum = 0 if name in ("steps", "holdout") else 1
        if value is not None and value < minimum:
            parser.error(f"--{name} must be an integer >= {minimum}")
    notebook = Path(__file__).resolve().with_suffix(".nb")
    if not notebook.is_file():
        parser.error(f"Notebook not found: {notebook}")
    executable = shutil.which(args.wolframscript)
    if not executable and not args.dry_run:
        parser.error(f"Executable not found: {args.wolframscript}")
    env = training_environment(args.threads, os.environ)
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
    for name in controls:
        value = env.get("HEPCAT_TRAIN_" + name.upper().replace("-", "_"))
        if value is not None:
            print(f"{name}: {value}", flush=True)
    print(shlex.join(command), flush=True)
    if args.dry_run:
        return 0
    # Replace this process, retaining terminal signals and the command's exit status.
    os.execvpe(command[0], command, env)


if __name__ == "__main__":
    raise SystemExit(main())
