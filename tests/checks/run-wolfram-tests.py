"""Run HEPCAT tests with a hard timeout and clean up their kernel processes."""

# HEPCAT - High Energy Physics Constructive Amplitude Tool
#
# Copyright (C) 2026 Neil Christensen
#
# This file is part of HEPCAT.
#
# HEPCAT is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# HEPCAT is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with HEPCAT.  If not, see <https://www.gnu.org/licenses/>.

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
    "documentation-content-tests.wl", "documentation-registration-tests.wl",
    "legacy-package-tests.wl", "legacy-algebra-tests.wl",
    "legacy-transform-tests.wl", "legacy-model-tests.wl",
    "numerical-spinor-stability-tests.wl",
    "spinor-identity-tests.wl", "spin-contraction-guard-tests.wl",
    "ordinary-identity-normalization-tests.wl",
    "shared-matrix-cofactor-tests.wl", "paired-spectator-tests.wl",
    "dirac-schouten-reduction-tests.wl",
    "channel-current-normalization-tests.wl", "four-leg-schouten-lookahead-tests.wl",
    "final-conservation-compaction-tests.wl",
    "preparation-ranking-tests.wl", "ordinary-pair-pole-tests.wl",
    "native-laurent-identity-tests.wl",
    "native-rest-null-dual-tests.wl",
    "thesis-preparation-tests.wl", "diagram-tests.wl",
    "simplification-path-tests.wl", "physical-residue-tests.wl", "factorization-simplification-budget-tests.wl", "factorization-source-timeout-retry-tests.wl", "ordinary-source-closure-tests.wl", "massless-sewing-tests.wl",
    "fermion-sewing-grading-tests.wl", "fermion-row-reference-tests.wl",
    "spurious-pole-tests.wl", "qed-five-point-simplification-tests.wl",
    "high-energy-tests.wl", "high-energy-power-counting-tests.wl", "high-energy-family-tests.wl", "cubic-current-source-tests.wl",
    "high-energy-compact-retry-tests.wl", "high-energy-zero-family-retry-tests.wl", "five-point-identity-proof-tests.wl",
    "reference-independence-tests.wl",
    "reference-affine-row-tests.wl",
    "coherent-polynomial-rules-tests.wl", "high-energy-coefficient-series-tests.wl",
    "high-energy-dispatch-tests.wl", "high-energy-laurent-rows-tests.wl",
    "high-energy-laurent-jet-tests.wl",
    "high-energy-scalar-cache-tests.wl",
    "high-energy-constraint-basis-tests.wl",
    "high-energy-tensor-zero-tests.wl",
    "high-energy-source-normalization-tests.wl", "high-energy-affine-atom-tests.wl",
    "high-energy-fitting-specialization-tests.wl",
    "high-energy-affine-chunk-tests.wl",
    "retained-high-energy-validation-tests.wl",
    "amplitude-completion-tests.wl", "completion-input-fit-tests.wl",
    "exact-left-null-contradiction-tests.wl",
    "completion-structured-source-tests.wl",
    "physical-pole-completion-tests.wl", "schouten-sum-sign-tests.wl",
    "compact-physical-source-tests.wl", "chiral-current-tests.wl", "spurious-chiral-current-tests.wl", "retained-affine-coefficients-tests.wl",
    "null-pair-shift-tests.wl", "null-channel-completion-tests.wl",
    "bose-symmetry-tests.wl", "fermion-symmetry-tests.wl",
    "model-parameter-tests.wl", "model-scalar-compaction-tests.wl", "linear-row-normalization-tests.wl",
    "completion-numeric-matrix-tests.wl", "color-basis-tests.wl", "color-reduction-tests.wl", "color-display-tests.wl", "feynman-tests.wl", "feynman-coefficient-comparison-tests.wl", "spinas-export-tests.wl",
    "spinas-helicity-family-tests.wl", "spinas-analytic-closure-tests.wl", "spinas-color-tests.wl",
    "spinas-build-tools-tests.wl",
    "spinas-calchep-tests.wl",
    "spinas-madgraph-tests.wl",
    "spinas-madevent-tests.wl",
    "spinas-calchep-numerical-tests.wl",
    "spinas-native-project-tests.wl", "spinas-scalar-cache-tests.wl", "spinas-optimized-tests.wl",
    "spinas-polarization-tests.wl",
    "amplitude-check-tests.wl",
    "model-isolation-tests.wl", "feynman-vertex-convention-tests.wl",
    "sm-field-convention-tests.wl", "numerator-momentum-complexity-tests.wl",
    "four-leg-tensor-reduction-tests.wl",
    "four-leg-tensor-continuation-tests.wl",
    "four-leg-tensor-pole-group-tests.wl",
    "plain-text-tests.wl", "model-export-tests.wl", "sm-factorization-model-tests.wl", "qed-model-export-tests.wl",
    "six-point-completion-tests.wl",
    "preparation-remainders-tests.wl",
    "completed-amplitude-preparation-tests.wl",
    "final-amplitude-simplification-tests.wl",
    "reverse-normalization-tests.wl", "determinant-pair-tests.wl",
    "numerator-spectator-tests.wl",
    "ordinary-pole-compaction-tests.wl", "scalar-pole-tests.wl",
    "whole-fraction-schouten-tests.wl",
    "qed-worked-example-tests.wl", "qed-construction-notebook-tests.wl", "sm-construction-notebook-tests.wl", "sm-five-point-notebook-tests.wl",
    "parallel-execution-tests.wl", "parallel-preparation-tests.wl", "preparation-parallel-policy-tests.wl",
    "parallel-amplitude-check-tests.wl", "parallel-completion-tests.wl",
    "construct-amplitude-tests.wl", "construct-benchmark-recovery-tests.wl", "revalidation-benchmark-tests.wl",
    "automatic-resource-tests.wl", "random-seed-tests.wl",
)

# These execute full expensive notebook constructions rather than component
# regressions. Select them explicitly and give the external runner enough time.
INTEGRATION_TEST_SUITES = ("covariant-current-source-tests.wl",
                          "sm-generation-transport-tests.wl",
                          "sm-factorization-sewing-tests.wl",
                          "sm-charged-current-integration-tests.wl",
                          "sm-vector-construction-integration-tests.wl",
                          "physical-construction-integration-tests.wl",
                          "seven-point-construction-integration-tests.wl",
                          "seven-point-spurious-construction-integration-tests.wl",
                          "qed-spurious-model-export-tests.wl",
                          "eight-point-spurious-independent-tests.wl",
                          "five-point-construction-integration-tests.wl",
                          "spurious-construction-integration-tests.wl",
                          "fermion-construction-integration-tests.wl",
                          "spinas-calchep-integration-tests.wl",
                          "spinas-madgraph-integration-tests.wl",
                          "spinas-chiral-integration-tests.wl",
                          "spinas-color-integration-tests.wl",
                          "spinas-madevent-integration-tests.wl")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--kernel", default=shutil.which("WolframKernel") or
                        "/Applications/Wolfram.app/Contents/MacOS/WolframKernel")
    parser.add_argument("--timeout", type=float, default=300)
    parser.add_argument("--suite", action="append", choices=TEST_SUITES + INTEGRATION_TEST_SUITES,
                        help="Run only this suite; repeat to select several. Default: standard suites. Full constructions are opt-in.")
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
