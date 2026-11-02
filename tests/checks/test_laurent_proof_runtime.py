"""Focused exact arithmetic, replay, resource and optional-backend contract tests."""
import importlib.util
import json
import os
import signal
from pathlib import Path
import subprocess
import sys
import tempfile
import time
import unittest

SOURCE = Path(__file__).resolve().parents[2] / "source"
RUNTIME_PATH = SOURCE / "laurent-proof-runtime.py"
SPEC = importlib.util.spec_from_file_location("hepcat_laurent_runtime", RUNTIME_PATH)
RUNTIME = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(RUNTIME)

def dag(nodes, roots, variables=2):
    return (f"HEPCAT_LAURENT_DAG 1\nVariables {variables}\n{nodes}\n" +
            "\n".join(f"Root {root}" for root in roots) + "\nEnd\n")

def wait_file(path, seconds=5):
    end = time.monotonic() + seconds
    while time.monotonic() < end:
        if path.exists() and path.stat().st_size:
            return
        time.sleep(0.005)
    raise AssertionError("Owned child did not announce its PID")

def running(pid):
    status = Path("/proc") / str(pid) / "stat"
    try:
        if status.exists() and status.read_text().split(")", 1)[1].split()[0] == "Z":
            return False
        os.kill(pid, 0)
        return True
    except ProcessLookupError:
        return False

def assert_not_running(test, pids):
    end = time.monotonic() + 5
    while time.monotonic() < end and any(running(pid) for pid in pids):
        time.sleep(0.01)
    test.assertFalse(any(running(pid) for pid in pids), "An owned child is still running")

class NativeArithmeticTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.directory = tempfile.TemporaryDirectory(prefix="hepcat-laurent-tests-")
        cls.addClassCleanup(cls.directory.cleanup)
        cls.path = Path(cls.directory.name)
        try:
            cls.binary, _ = RUNTIME.ensure_binary(cls.path / "cache", None, RUNTIME.Clock(30))
        except (RUNTIME.NotProved, OSError, subprocess.TimeoutExpired) as error:
            raise unittest.SkipTest("Optional C++/Boost backend unavailable: " + str(error))

    def native(self, nodes, roots, *options):
        path = self.path / "input.dag"
        path.write_text(dag(nodes, roots))
        return subprocess.run([str(self.binary), str(path), *options], text=True,
                              stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=10)

    def invoke(self, content, *options, env=None):
        path = self.path / "contract.dag"
        path.write_text(content)
        result = subprocess.run([sys.executable, str(RUNTIME_PATH), str(path), "--seconds", "20",
                                 "--cache-directory", str(self.path / "cache"), *options], text=True,
                                stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=30, env=env)
        self.assertEqual(result.returncode, 0, result.stderr)
        return json.loads(result.stdout)

    def test_01_polynomial_cancellation_and_rational_laurent_inverse(self):
        nodes = """Variable 1 1
Variable 2 2
Add 3 2 1 2
Power 4 3 2
Power 5 1 2
Power 6 2 2
Multiply 7 2 1 2
Constant 8 2 1
Multiply 9 2 8 7
Add 10 3 5 6 9
Constant 11 -1 1
Multiply 12 2 11 10
Add 13 2 4 12
Constant 14 7 3
Multiply 15 2 14 1
Power 16 15 -2
Power 17 15 2
Multiply 18 2 16 17
Add 19 2 18 11"""
        result = self.native(nodes, [13, 19, 3])
        self.assertEqual(result.returncode, 0, result.stdout)
        for row in ("ROOT 1 NODE 13 ZERO TERMS 0", "ROOT 2 NODE 19 ZERO TERMS 0", "ROOT 3 NODE 3 NONZERO TERMS 2"):
            self.assertIn(row, result.stdout)

    def test_02_inverse_sum_declined_without_erasing_completed_root(self):
        result = self.native("Variable 1 1\nVariable 2 2\nAdd 3 2 1 2\nPower 4 3 -1", [1, 4])
        self.assertEqual(result.returncode, 2)
        self.assertIn("InverseNotNonzeroMonomial", result.stdout)
        self.assertIn("ROOT 1 NODE 1 NONZERO TERMS 1", result.stdout)
        self.assertIn("ROOT 2 NODE 4 NOT_PROVED", result.stdout)

    def test_03_exponent_overflow_is_checked(self):
        result = self.native("Variable 1 1\nPower 2 1 1000000\nPower 3 2 1000000", [3])
        self.assertIn("ExponentOverflow", result.stdout)
        self.assertEqual(result.returncode, 2)

    def test_04_term_cap_preserves_unproved_status(self):
        result = self.native("Variable 1 1\nVariable 2 2\nAdd 3 2 1 2", [3], "--max-terms", "1")
        self.assertIn("NodeTermLimit", result.stdout)
        self.assertEqual(result.returncode, 2)

    def test_05_large_rational_constants_are_exact(self):
        result = self.native("Constant 1 123456789012345678901234567890123456789 3\nConstant 2 -123456789012345678901234567890123456789 3\nAdd 3 2 1 2", [3])
        self.assertEqual(result.returncode, 0, result.stdout)
        self.assertIn("ROOT 1 NODE 3 ZERO TERMS 0", result.stdout)

    def test_06_non_topological_nodes_are_rejected(self):
        result = self.native("Add 1 1 1", [1])
        self.assertIn("NonTopologicalChild", result.stdout)
        self.assertEqual(result.returncode, 2)

    def test_07_zero_denominator_is_rejected(self):
        result = self.native("Constant 1 1 0", [1])
        self.assertIn("ZeroDenominator", result.stdout)
        self.assertEqual(result.returncode, 2)

    def test_08_coefficient_bit_cap_is_exact(self):
        result = self.native("Constant 1 256 1", [1], "--max-bits", "8")
        self.assertIn("CoefficientBitLimit", result.stdout)
        self.assertEqual(result.returncode, 2)

    def test_09_inverse_after_exact_cancellation_is_allowed(self):
        result = self.native("Variable 1 1\nConstant 2 -1 1\nMultiply 3 2 2 1\nConstant 4 1 1\nAdd 5 3 1 3 4\nPower 6 5 -1", [6])
        self.assertEqual(result.returncode, 0, result.stdout)
        self.assertIn("ROOT 1 NODE 6 NONZERO TERMS 1", result.stdout)

    def test_mixed_root_contract_and_warm_cache(self):
        content = dag("Variable 1 1\nConstant 2 -1 1\nMultiply 3 2 2 1\nAdd 4 2 1 3", [4, 1])
        report = self.invoke(content, "--expect-nonzero-root", "2")
        self.assertEqual(report["RunStatus"], "COMPLETE")
        self.assertEqual(report["Status"], "PROVED_EXPECTATIONS")
        self.assertEqual([row["Status"] for row in report["Roots"]], ["ZERO", "NONZERO"])
        self.assertFalse(report["AllRootsZero"])
        self.assertTrue(report["AllExpectedStatuses"])
        self.assertEqual(report["RootCount"], 2)
        self.assertTrue(report["Compilation"]["CacheHit"])
        self.assertEqual(set(report["SourceHashes"]), set(RUNTIME.SOURCE_NAMES))
        self.assertEqual(report["ExactFactorVerification"]["Status"], "PASS")

    def test_nonzero_root_remains_visible_when_zero_was_expected(self):
        report = self.invoke(dag("Variable 1 1", [1]))
        self.assertEqual(report["RunStatus"], "COMPLETE")
        self.assertEqual(report["Status"], "NOT_PROVED")
        self.assertEqual(report["Roots"][0]["Status"], "NONZERO")

    def test_resource_failure_and_invalid_expected_index(self):
        report = self.invoke(dag("Variable 1 1\nVariable 2 2\nAdd 3 2 1 2", [3]), "--max-terms", "1")
        self.assertEqual(report["Status"], "NOT_PROVED")
        self.assertFalse(report["AllExpectedStatuses"])
        self.assertIn("NodeTermLimit", report["FailureReason"])
        report = self.invoke(dag("Variable 1 1", [1]), "--expect-nonzero-root", "2")
        self.assertEqual(report["Status"], "NOT_PROVED")
        self.assertIn("InvalidExpectedRootMapping", report["FailureReason"])

    def test_unavailable_compiler_and_shared_deadline_return_json(self):
        content = dag("Variable 1 1", [1])
        report = self.invoke(content, "--compiler", str(self.path / "does-not-exist"))
        self.assertEqual(report["Status"], "NOT_PROVED")
        self.assertIn("CompilerUnavailable", report["FailureReason"])
        self.assertEqual(report["RootCount"], 1)
        self.assertEqual(report["Roots"][0]["Status"], "NOT_PROVED")
        report = self.invoke(content, "--seconds", "0.000000001")
        self.assertEqual(report["Status"], "NOT_PROVED")
        self.assertIn("TimeConstraint", report["FailureReason"])

    def test_python_optimization_cannot_disable_exact_verification(self):
        environment = dict(os.environ, PYTHONOPTIMIZE="1")
        report = self.invoke(dag("Constant 1 0 1", [1]), env=environment)
        self.assertEqual(report["Status"], "PROVED_ZERO")

    @unittest.skipUnless(os.name == "posix", "Parent-liveness guard requires POSIX")
    def test_native_exits_after_wrapper_sigkill(self):
        with tempfile.TemporaryDirectory(prefix="hepcat-native-parent-") as temporary:
            directory = Path(temporary)
            input_path, pid_path, log_path = (directory / name for name in ("input.dag", "pid", "native.log"))
            input_path.write_text(dag("\n".join(f"Variable {i} {i}" for i in range(1, 10)) +
                "\nAdd 10 9 1 2 3 4 5 6 7 8 9\nPower 11 10 1000", [11], variables=9))
            launcher = directory / "launch.py"
            launcher.write_text("import os,subprocess\nfrom pathlib import Path\n" +
                f"with open({str(log_path)!r},'w') as output:\n" +
                f" child=subprocess.Popen([{str(self.binary)!r},{str(input_path)!r},'--seconds','10'," +
                "'--max-terms','2000000','--max-live-terms','6000000','--max-products','500000000'," +
                "'--parent-pid',str(os.getpid())],stdout=output,stderr=subprocess.STDOUT,start_new_session=True)\n" +
                f" Path({str(pid_path)!r}).write_text(str(child.pid))\n child.wait()\n")
            wrapper = subprocess.Popen([sys.executable, str(launcher)])
            try:
                wait_file(pid_path)
                native_pid = int(pid_path.read_text())
                wrapper.kill()
                wrapper.wait(timeout=5)
                end = time.monotonic() + 5
                while time.monotonic() < end and "REASON ParentExited" not in log_path.read_text():
                    time.sleep(0.005)
                self.assertIn("REASON ParentExited", log_path.read_text())
                assert_not_running(self, [native_pid])
            finally:
                if wrapper.poll() is None:
                    wrapper.kill()
                    wrapper.wait(timeout=5)

class FactorVerificationTests(unittest.TestCase):
    def sleeper(self, directory):
        path = directory / "owned-sleeper"
        path.write_text("#!" + sys.executable + "\nimport os,subprocess,sys,time,json\nfrom pathlib import Path\n" +
            "child=subprocess.Popen([sys.executable,'-c','import time; time.sleep(60)'])\n" +
            "Path(os.environ['HEPCAT_TEST_PID_FILE']).write_text(json.dumps([os.getpid(),child.pid]))\n" +
            "time.sleep(60)\n")
        path.chmod(0o700)
        return path

    @unittest.skipUnless(os.name == "posix", "Owned process groups require POSIX")
    def test_timeout_kills_and_reaps_owned_process_group(self):
        with tempfile.TemporaryDirectory(prefix="hepcat-native-timeout-") as temporary:
            directory = Path(temporary)
            pid_path = directory / "pids"
            sleeper = self.sleeper(directory)
            previous = os.environ.get("HEPCAT_TEST_PID_FILE")
            os.environ["HEPCAT_TEST_PID_FILE"] = str(pid_path)
            try:
                with self.assertRaises(subprocess.TimeoutExpired):
                    RUNTIME.run([str(sleeper)], RUNTIME.Clock(0.3))
            finally:
                if previous is None:
                    os.environ.pop("HEPCAT_TEST_PID_FILE", None)
                else:
                    os.environ["HEPCAT_TEST_PID_FILE"] = previous
            self.assertTrue(pid_path.is_file())
            assert_not_running(self, json.loads(pid_path.read_text()))

    @unittest.skipUnless(os.name == "posix", "Owned process groups require POSIX")
    def test_wrapper_sigterm_cleans_compiler_group_and_returns_unproved(self):
        with tempfile.TemporaryDirectory(prefix="hepcat-native-sigterm-") as temporary:
            directory = Path(temporary)
            input_path, pid_path = directory / "input.dag", directory / "pids"
            input_path.write_text(dag("Constant 1 0 1", [1]))
            sleeper = self.sleeper(directory)
            environment = dict(os.environ, HEPCAT_TEST_PID_FILE=str(pid_path))
            wrapper = subprocess.Popen([sys.executable, str(RUNTIME_PATH), str(input_path),
                "--seconds", "20", "--compiler", str(sleeper), "--cache-directory", str(directory / "cache")],
                stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, env=environment)
            try:
                wait_file(pid_path)
                owned = json.loads(pid_path.read_text())
                wrapper.terminate()
                stdout, stderr = wrapper.communicate(timeout=5)
                self.assertEqual(wrapper.returncode, 0, stderr)
                result = json.loads(stdout)
                self.assertEqual(result["Status"], "NOT_PROVED")
                self.assertEqual(result["FailureReason"], "Terminated")
                assert_not_running(self, owned)
            finally:
                if wrapper.poll() is None:
                    wrapper.kill()
                    wrapper.wait(timeout=5)

    def test_subset_factor_witnesses_bind_source_output_and_formal_plans(self):
        with tempfile.TemporaryDirectory(prefix="hepcat-factor-tests-") as temporary:
            directory = Path(temporary)
            original, output, witness = (directory / name for name in ("input.dag", "output.dag", "witness.json"))
            original.write_text(dag("Variable 1 1\nVariable 2 2\nMultiply 3 2 1 2\nPower 4 2 2\nMultiply 5 2 1 4\nAdd 6 2 3 5", [6]))
            factor = subprocess.run([sys.executable, "-I", str(SOURCE / "laurent-proof-factor.py"), str(original), str(output), str(witness)],
                                    text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=10)
            self.assertEqual(factor.returncode, 0, factor.stderr)
            command = [sys.executable, "-I", str(SOURCE / "laurent-proof-verify.py"), str(original), str(output), str(witness)]
            verified = subprocess.run(command, capture_output=True, text=True, timeout=10)
            self.assertEqual(verified.returncode, 0, verified.stderr)
            certificate = json.loads(witness.read_text())
            certificate["LocalWitnesses"][0]["OutputNode"] = 0
            witness.write_text(json.dumps(certificate))
            rejected = subprocess.run(command, capture_output=True, text=True, timeout=10)
            self.assertNotEqual(rejected.returncode, 0)

    def test_verifier_explicitly_rejects_disabled_assertions(self):
        result = subprocess.run([sys.executable, "-O", str(SOURCE / "laurent-proof-verify.py"), "missing", "missing", "missing"],
                                capture_output=True, text=True, timeout=10)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("requires enabled assertions", result.stderr)

    def test_nonfinite_time_is_json_not_a_proof(self):
        for value in ("nan", "inf", "0", "-1"):
            result = subprocess.run([sys.executable, str(RUNTIME_PATH), "unused", "--seconds", value],
                                    capture_output=True, text=True, timeout=10)
            report = json.loads(result.stdout)
            self.assertEqual(report["Status"], "NOT_PROVED")

    def test_timeout_checkpoints_preserve_completed_roots(self):
        rows, complete, _, _ = RUNTIME.native_result("CHECKPOINT_ROOT 1 NODE 9 ZERO TERMS 0 SECONDS 1\n", [3, 4], [9, 10], set())
        self.assertFalse(complete)
        self.assertEqual([row["Status"] for row in rows], ["ZERO", "NOT_PROVED"])

if __name__ == "__main__":
    unittest.main()
