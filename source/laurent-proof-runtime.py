#!/usr/bin/env python3
"""Optional exact Laurent proof orchestration. Emit one JSON result, never a sampled proof.

INPUT is the HEPCAT_LAURENT_DAG 1 format consumed by the adjacent C++ runtime.
Chart coverage, typed denominator clearing, complete spin coverage and parameter
homogeneity are obligations of the caller. --seconds covers discovery, compilation,
factor collection, exact factor verification and evaluation together. RunStatus
describes traversal independently of the ZERO/NONZERO verdict of each root.
"""
import argparse
import hashlib
import json
import math
import os
from pathlib import Path
import re
import shlex
import shutil
import signal
import subprocess
import sys
import tempfile
import time

HERE = Path(__file__).resolve().parent
SOURCE_NAMES = ("laurent-proof-runtime.cpp", "laurent-proof-runtime.py",
                "laurent-proof-factor.py", "laurent-proof-verify.py")

class NotProved(Exception):
    pass

class Terminated(NotProved):
    pass

def terminate(signum, frame):
    raise Terminated("Terminated")

class Parser(argparse.ArgumentParser):
    def error(self, message):
        raise NotProved("InvalidArguments: " + message)

def digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()

def sources():
    hashes = {name: digest(HERE / name) for name in SOURCE_NAMES}
    fingerprint = hashlib.sha256("\n".join(name + "\0" + hashes[name] for name in sorted(hashes)).encode()).hexdigest()
    return hashes, fingerprint

class Clock:
    def __init__(self, seconds):
        self.started = time.monotonic()
        self.deadline = self.started + seconds
    def remaining(self):
        value = self.deadline - time.monotonic()
        if value <= 0:
            raise NotProved("TimeConstraint")
        return value
    def elapsed(self):
        return time.monotonic() - self.started

def run(command, clock, stdin=None, allowance=None):
    timeout = min(clock.remaining(), allowance) if allowance is not None else clock.remaining()
    process = None
    # Defer SIGTERM until Popen has returned and this scope owns its process
    # group. A pending signal is then delivered inside the cleanup-protected try.
    previous_mask = signal.pthread_sigmask(signal.SIG_BLOCK, {signal.SIGTERM}) if hasattr(signal, "pthread_sigmask") else None
    try:
        process = subprocess.Popen(command, stdin=subprocess.PIPE if stdin is not None else subprocess.DEVNULL,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, start_new_session=(os.name == "posix"))
        if previous_mask is not None:
            signal.pthread_sigmask(signal.SIG_SETMASK, previous_mask)
        stdout, stderr = process.communicate(stdin, timeout=timeout)
    except BaseException:
        if previous_mask is not None:
            signal.pthread_sigmask(signal.SIG_BLOCK, {signal.SIGTERM})
        if process is not None:
            try:
                if os.name == "posix":
                    os.killpg(process.pid, signal.SIGKILL)
                else:
                    process.kill()
            except ProcessLookupError:
                pass
            process.communicate()
        if previous_mask is not None:
            signal.pthread_sigmask(signal.SIG_SETMASK, previous_mask)
        raise
    if len(stdout) + len(stderr) > 8000000:
        raise NotProved("ProcessOutputLimit")
    return process.returncode, stdout, stderr

def discover_compiler(requested, clock):
    choices = [requested] if requested else [os.environ.get("CXX"), "clang++", "g++", "c++"]
    for choice in choices:
        if not choice:
            continue
        compiler = shutil.which(choice)
        if not compiler:
            continue
        code, version, errors = run([compiler, "--version"], clock, allowance=min(3, clock.remaining()))
        if code == 0:
            return str(Path(compiler).resolve()), version + errors
    raise NotProved("CompilerUnavailable")

def discover_boost(compiler, clock):
    include_dirs = []
    for name in ("BOOST_INCLUDEDIR", "BOOST_INCLUDE_DIR", "BOOST_ROOT"):
        if os.environ.get(name):
            directory = Path(os.environ[name]).expanduser()
            for candidate in (directory, directory / "include"):
                if (candidate / "boost/multiprecision/cpp_int.hpp").is_file():
                    include_dirs.append(candidate.resolve())
    candidates = [[]] + [["-I", str(path)] for path in dict.fromkeys(include_dirs)]
    for flags in candidates:
        code, dependencies, _ = run([compiler, "-std=c++17", *flags, "-M", "-x", "c++", "-"], clock,
            stdin="#include <boost/multiprecision/cpp_int.hpp>\n", allowance=min(5, clock.remaining()))
        if code:
            continue
        for entry in shlex.split(dependencies.replace("\\\n", " ")):
            if entry.endswith("boost/multiprecision/cpp_int.hpp") and Path(entry).is_file():
                return flags, Path(entry).resolve()
    raise NotProved("BoostHeadersUnavailable")

def ensure_binary(cache_directory, compiler_option, clock):
    compiler, version = discover_compiler(compiler_option, clock)
    include_flags, boost_header = discover_boost(compiler, clock)
    flags = ["-std=c++17", "-O2", *include_flags]
    compiler_hash = digest(compiler)
    boost_version = boost_header.parent.parent / "version.hpp"
    key_data = {"Source": digest(HERE / SOURCE_NAMES[0]), "Compiler": compiler,
                "CompilerSHA256": compiler_hash,
                "Version": version, "Flags": flags, "BoostHeader": str(boost_header),
                "BoostHeaderSHA256": digest(boost_header),
                "BoostVersionSHA256": digest(boost_version) if boost_version.is_file() else None,
                "IncludeEnvironment": {key: os.environ.get(key, "") for key in ("CPATH", "CPLUS_INCLUDE_PATH")}}
    key = hashlib.sha256(json.dumps(key_data, sort_keys=True).encode()).hexdigest()
    cache_directory.mkdir(parents=True, exist_ok=True)
    binary = cache_directory / ("laurent-" + key)
    metadata = binary.with_suffix(".json")
    reused = False
    try:
        saved = json.loads(metadata.read_text())
        reused = binary.is_file() and os.access(binary, os.X_OK) and saved["Key"] == key and saved["BinarySHA256"] == digest(binary)
    except (OSError, ValueError, KeyError):
        pass
    if not reused:
        with tempfile.TemporaryDirectory(prefix="compile-", dir=cache_directory) as temporary:
            candidate = Path(temporary) / "laurent-proof"
            code, _, errors = run([compiler, *flags, str(HERE / SOURCE_NAMES[0]), "-o", str(candidate)], clock)
            if code != 0 or not candidate.is_file():
                raise NotProved("CompilerFailure: " + errors[-1500:])
            clock.remaining()
            candidate.chmod(0o700)
            fingerprint = digest(candidate)
            os.replace(candidate, binary)
            meta_candidate = Path(temporary) / "metadata.json"
            meta_candidate.write_text(json.dumps({"Key": key, "BinarySHA256": fingerprint}))
            os.replace(meta_candidate, metadata)
    return binary, {"Compiler": compiler, "CompilerSHA256": compiler_hash, "CompilerIdentity": version.strip(), "BoostHeader": str(boost_header),
                    "CacheKey": key, "CacheHit": reused, "BinarySHA256": digest(binary)}

def native_result(text, original_roots, output_roots, expected_nonzero):
    matches = re.findall(r"^ROOT (\d+) NODE (\d+) (ZERO|NONZERO|NOT_PROVED) TERMS (\d+)$", text, re.M)
    if not matches:
        matches = re.findall(r"^CHECKPOINT_ROOT (\d+) NODE (\d+) (ZERO|NONZERO) TERMS (\d+) SECONDS \S+$", text, re.M)
    rows = [{"Index": index, "InputNode": source, "OutputNode": target, "Status": "NOT_PROVED",
             "ExpectedStatus": "NONZERO" if index in expected_nonzero else "ZERO", "Terms": 0}
            for index, (source, target) in enumerate(zip(original_roots, output_roots), 1)]
    seen = set()
    for index, target, status, terms in matches:
        index, target, terms = int(index), int(target), int(terms)
        if index in seen or not 1 <= index <= len(rows) or rows[index - 1]["OutputNode"] != target:
            raise NotProved("InvalidNativeRootMapping")
        seen.add(index)
        rows[index - 1].update(Status=status, Terms=terms)
    complete = "STATUS COMPLETE\n" in text and len(seen) == len(rows) and all(row["Status"] != "NOT_PROVED" for row in rows)
    metrics = {}
    for label in ("EVALUATED_NODES", "CACHE_HITS", "PEAK_LIVE_TERMS", "PEAK_NODE_TERMS", "PRODUCTS", "PEAK_SAMPLED_RSS_BYTES", "SECONDS"):
        match = re.search(r"^" + label + r" (\S+)$", text, re.M)
        if match:
            metrics[label] = float(match.group(1)) if label == "SECONDS" else int(match.group(1))
    reason = re.search(r"^REASON (.+)$", text, re.M)
    return rows, complete, metrics, reason.group(1) if reason else None

def execute(arguments, clock):
    result = {"Status": "NOT_PROVED", "RunStatus": "NOT_PROVED", "Method": "NativeExactLaurentPolynomial",
              "RootCount": 0, "Roots": [], "AllRootsZero": False, "AllExpectedStatuses": False,
              "NumericalKinematicSamples": 0, "CoherentSpinSamples": 0, "Timings": {}}
    hashes, fingerprint = sources()
    result.update(SourceHashes=hashes, SourceFingerprint=fingerprint)
    source = arguments.input.resolve()
    if not source.is_file() or source.stat().st_size > 32000000:
        raise NotProved("InputUnavailableOrTooLarge")
    result["InputSHA256"] = digest(source)
    result["ExpectedNonzeroRoots"] = sorted(set(arguments.expect_nonzero_root))
    declared = [int(value) for value in re.findall(r"^Root ([0-9]+)\s*$", source.read_bytes().decode("utf-8"), re.M)]
    if len(declared) > 4096:
        raise NotProved("RootLimit")
    result.update(RootCount=len(declared), OriginalRoots=declared,
        Roots=[{"Index": index, "InputNode": node, "OutputNode": None, "Status": "NOT_PROVED", "Terms": 0,
                "ExpectedStatus": "NONZERO" if index in result["ExpectedNonzeroRoots"] else "ZERO"}
               for index, node in enumerate(declared, 1)])
    result["Limits"] = {name: getattr(arguments, name) for name in ("seconds", "evaluation_seconds", "max_nodes", "max_terms", "max_live_terms", "max_products", "max_rss_mib", "max_bits")}
    temporary = None
    if arguments.artifact_directory:
        artifacts = arguments.artifact_directory.resolve()
        artifacts.mkdir(parents=True, exist_ok=True)
    else:
        temporary = tempfile.TemporaryDirectory(prefix="hepcat-laurent-")
        artifacts = Path(temporary.name)
    try:
        began = clock.elapsed()
        binary, compiler = ensure_binary(arguments.cache_directory.resolve(), arguments.compiler, clock)
        result["Compilation"] = compiler
        result["Timings"]["DiscoveryAndCompilation"] = clock.elapsed() - began
        factored, witness = artifacts / "factored.dag", artifacts / "factor-witnesses.json"
        began = clock.elapsed()
        code, factor_stdout, factor_errors = run([sys.executable, "-I", str(HERE / SOURCE_NAMES[2]), str(source), str(factored), str(witness),
            "--seconds", str(min(30, clock.remaining()))], clock)
        (artifacts / "factor.log").write_text(factor_stdout + factor_errors)
        if code != 0:
            raise NotProved("FactorCollectionFailed: " + factor_errors[-1000:])
        result["Timings"]["FactorCollection"] = clock.elapsed() - began
        began = clock.elapsed()
        code, verifier_stdout, verifier_errors = run([sys.executable, "-I", str(HERE / SOURCE_NAMES[3]), str(source), str(factored), str(witness)], clock)
        (artifacts / "factor-verification.log").write_text(verifier_stdout + verifier_errors)
        if code != 0:
            raise NotProved("ExactFactorVerificationFailed")
        certificate = json.loads(witness.read_text())
        if certificate["SourceSHA256"] != result["InputSHA256"] or certificate["OutputSHA256"] != digest(factored):
            raise NotProved("FactorHashMismatch")
        originals, outputs = certificate["OriginalRoots"], certificate["OutputRoots"]
        if not originals or len(originals) != len(outputs) or not set(result["ExpectedNonzeroRoots"]).issubset(range(1, len(originals) + 1)):
            raise NotProved("InvalidExpectedRootMapping")
        result.update(RootCount=len(originals), OriginalRoots=originals, FactoredRoots=outputs,
            OutputSHA256=digest(factored), FactorWitnessSHA256=digest(witness),
            ExactFactorVerification={"Status": "PASS", "LocalIdentities": len(certificate["LocalWitnesses"]),
                                     "OriginalNodes": certificate["OriginalNodes"], "OutputNodes": certificate["OutputNodes"]})
        result["Timings"]["ExactFactorVerification"] = clock.elapsed() - began
        allowance = min(arguments.evaluation_seconds, clock.remaining())
        command = [str(binary), str(factored), "--seconds", str(allowance), "--parent-pid", str(os.getpid())]
        for name in ("max_nodes", "max_terms", "max_live_terms", "max_products", "max_rss_mib", "max_bits"):
            command.extend(["--" + name.replace("_", "-"), str(getattr(arguments, name))])
        began = clock.elapsed()
        try:
            code, native_stdout, native_errors = run(command, clock, allowance=allowance)
        except subprocess.TimeoutExpired as error:
            native_stdout = error.output.decode() if isinstance(error.output, bytes) else error.output or ""
            native_errors = "Process deadline reached"
            code = 2
        (artifacts / "native.log").write_text(native_stdout + native_errors)
        rows, complete, metrics, reason = native_result(native_stdout, originals, outputs, set(result["ExpectedNonzeroRoots"]))
        result.update(Roots=rows, RunStatus="COMPLETE" if complete and code == 0 else "NOT_PROVED", Metrics=metrics)
        result["Timings"]["Evaluation"] = clock.elapsed() - began
        result["AllRootsZero"] = complete and code == 0 and all(row["Status"] == "ZERO" and row["Terms"] == 0 for row in rows)
        result["AllExpectedStatuses"] = complete and code == 0 and all(row["Status"] == row["ExpectedStatus"] and
            (row["Terms"] == 0 if row["Status"] == "ZERO" else row["Terms"] > 0) for row in rows)
        if result["AllExpectedStatuses"]:
            result["Status"] = "PROVED_EXPECTATIONS" if result["ExpectedNonzeroRoots"] else "PROVED_ZERO"
        else:
            result["FailureReason"] = reason or ("RootExpectationMismatch" if complete else "IncompleteNativeTraversal")
        if arguments.artifact_directory:
            result["Artifacts"] = {"Directory": str(artifacts), "FactoredDAG": str(factored), "FactorWitnesses": str(witness), "NativeLog": str(artifacts / "native.log")}
        result["ElapsedTime"] = clock.elapsed()
        return result
    except (NotProved, OSError, ValueError, KeyError, subprocess.TimeoutExpired) as error:
        result["FailureReason"] = "TimeConstraint" if isinstance(error, subprocess.TimeoutExpired) else str(error)
        result["ElapsedTime"] = clock.elapsed()
        return result
    finally:
        if temporary:
            temporary.cleanup()

def _main(argv=None):
    parser = Parser(description=__doc__)
    parser.add_argument("input", type=Path)
    parser.add_argument("--seconds", type=float, default=90)
    parser.add_argument("--evaluation-seconds", type=float, default=90)
    parser.add_argument("--cache-directory", type=Path, default=Path(tempfile.gettempdir()) / "hepcat-native-laurent-cache")
    parser.add_argument("--artifact-directory", type=Path)
    parser.add_argument("--compiler")
    parser.add_argument("--expect-nonzero-root", type=int, action="append", default=[])
    for name, default in (("max-nodes", 250000), ("max-terms", 2000000), ("max-live-terms", 6000000),
                          ("max-products", 500000000), ("max-rss-mib", 1024), ("max-bits", 4096)):
        parser.add_argument("--" + name, type=int, default=default)
    started = time.monotonic()
    try:
        arguments = parser.parse_args(argv)
        if not all(math.isfinite(value) and 0 < value <= 3600 for value in (arguments.seconds, arguments.evaluation_seconds)):
            raise NotProved("InvalidTimeConstraint")
        if any(getattr(arguments, name) < 1 for name in ("max_nodes", "max_terms", "max_live_terms", "max_products", "max_rss_mib", "max_bits")):
            raise NotProved("InvalidResourceLimit")
        if any(index < 1 for index in arguments.expect_nonzero_root):
            raise NotProved("InvalidExpectedRootMapping")
        result = execute(arguments, Clock(arguments.seconds))
    except (NotProved, OSError, ValueError, KeyError, subprocess.TimeoutExpired) as error:
        result = {"Status": "NOT_PROVED", "RunStatus": "NOT_PROVED", "RootCount": 0, "Roots": [],
                  "FailureReason": str(error), "ElapsedTime": time.monotonic() - started}
    print(json.dumps(result, sort_keys=True))
    return 0

def main(argv=None):
    previous_handler = signal.signal(signal.SIGTERM, terminate)
    try:
        return _main(argv)
    except Terminated:
        print(json.dumps({"Status": "NOT_PROVED", "RunStatus": "NOT_PROVED", "FailureReason": "Terminated",
                          "RootCount": 0, "Roots": []}))
        return 0
    finally:
        signal.signal(signal.SIGTERM, previous_handler)

if __name__ == "__main__":
    raise SystemExit(main())
