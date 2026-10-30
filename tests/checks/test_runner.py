"""Exercise test selection, error propagation, and hard process-group timeouts."""
import contextlib
import importlib.util
import io
import os
from pathlib import Path
import platform
import shutil
import signal
import subprocess
import sys
import tempfile
import time
import unittest
from unittest import mock


RUNNER_PATH = Path(__file__).with_name("run-wolfram-tests.py")
SPEC = importlib.util.spec_from_file_location("hepcat_test_runner", RUNNER_PATH)
RUNNER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(RUNNER)
WOLFRAM_KERNEL = shutil.which("WolframKernel") or "/Applications/Wolfram.app/Contents/MacOS/WolframKernel"


class RunnerTests(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory(prefix="hepcat-runner-")
        self.addCleanup(self.directory.cleanup)
        self.kernel = Path(self.directory.name) / "fake-kernel"
        self.kernel.write_text(
            "#!" + sys.executable + "\n"
            "import json, os, sys\n"
            "from pathlib import Path\n"
            "print('FAKE:' + json.dumps({'suite': Path(sys.argv[-1]).name,\n"
            " 'args': sys.argv[1:-1],\n"
            " 'omp': os.environ.get('OMP_NUM_THREADS'),\n"
            " 'blas': os.environ.get('OPENBLAS_NUM_THREADS'),\n"
            " 'dynamic': os.environ.get('OMP_DYNAMIC'),\n"
            " 'strict': os.environ.get('HEPCAT_STRICT_KNOWN_ISSUES')}))\n"
            "if os.environ.get('HEPCAT_FAKE_SLEEP') == '1':\n"
            " import subprocess, time\n"
            " subprocess.Popen([sys.executable, '-c', 'import time; time.sleep(10)'])\n"
            " print('child holds output pipe', flush=True)\n"
            " time.sleep(10)\n"
            "if os.environ.get('HEPCAT_FAKE_NO_MARKER') != '1':\n"
            " print('HEPCAT_TEST_COMPLETED:' + Path(sys.argv[-1]).name)\n"
            "sys.exit(int(os.environ.get('HEPCAT_FAKE_EXIT', '0')))\n",
            encoding="utf-8",
        )
        self.kernel.chmod(0o700)

    def invoke(self, *args, env=None):
        environment = dict(os.environ)
        environment.pop("HEPCAT_STRICT_KNOWN_ISSUES", None)
        environment.pop("HEPCAT_FAKE_EXIT", None)
        environment.pop("HEPCAT_FAKE_SLEEP", None)
        environment.pop("HEPCAT_FAKE_NO_MARKER", None)
        if env:
            environment.update(env)
        return subprocess.run(
            [sys.executable, str(RUNNER_PATH), "--kernel", str(self.kernel), *args],
            text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
            timeout=20, env=environment,
        )

    @staticmethod
    def records(result):
        import json
        return [json.loads(line[5:]) for line in result.stdout.splitlines()
                if line.startswith("FAKE:")]

    def test_default_includes_all_legacy_and_new_suites(self):
        result = self.invoke()
        self.assertEqual(result.returncode, 0, result.stdout)
        records = self.records(result)
        self.assertEqual([row["suite"] for row in records], list(RUNNER.TEST_SUITES))
        self.assertTrue(all(row["args"][0] == "-script" and
                            Path(row["args"][1]).name == "run-suite.wl" for row in records))

    def test_one_selected_suite(self):
        result = self.invoke("--suite", "legacy-algebra-tests.wl")
        self.assertEqual(result.returncode, 0, result.stdout)
        self.assertEqual([row["suite"] for row in self.records(result)],
                         ["legacy-algebra-tests.wl"])

    def test_expensive_construction_requires_explicit_selection(self):
        name = "physical-construction-integration-tests.wl"
        self.assertNotIn(name, RUNNER.TEST_SUITES)
        result = self.invoke("--suite", name)
        self.assertEqual(result.returncode, 0, result.stdout)
        self.assertEqual([row["suite"] for row in self.records(result)], [name])

    def test_repeated_selection_keeps_the_requested_order(self):
        result = self.invoke("--suite", "legacy-model-tests.wl",
                             "--suite", "spinor-identity-tests.wl")
        self.assertEqual(result.returncode, 0, result.stdout)
        self.assertEqual([row["suite"] for row in self.records(result)],
                         ["legacy-model-tests.wl", "spinor-identity-tests.wl"])

    def test_unknown_suite_is_rejected_before_starting_kernel(self):
        result = self.invoke("--suite", "../untrusted.wl")
        self.assertEqual(result.returncode, 2)
        self.assertEqual(self.records(result), [])

    def test_nonpositive_or_nonfinite_timeout_is_rejected(self):
        for timeout in ("0", "-1", "nan", "inf", "-inf"):
            with self.subTest(timeout=timeout):
                result = self.invoke("--timeout", timeout)
                self.assertEqual(result.returncode, 2)
                self.assertEqual(self.records(result), [])

    def test_nonzero_exit_stops_later_suites_and_preserves_output(self):
        result = self.invoke(env={"HEPCAT_FAKE_EXIT": "7"})
        self.assertEqual(result.returncode, 7)
        self.assertEqual(len(self.records(result)), 1)
        self.assertIn("legacy-package-tests.wl", result.stdout)

    def test_zero_exit_without_completion_marker_fails(self):
        result = self.invoke(env={"HEPCAT_FAKE_NO_MARKER": "1"})
        self.assertEqual(result.returncode, 1)
        self.assertEqual(len(self.records(result)), 1)
        self.assertIn("no completion marker", result.stdout)

    def test_thread_limits_are_local_to_the_child_environment(self):
        result = self.invoke("--suite", "legacy-package-tests.wl", env={
            "OMP_NUM_THREADS": "99", "OPENBLAS_NUM_THREADS": "99",
            "OMP_DYNAMIC": "TRUE",
        })
        self.assertEqual(result.returncode, 0, result.stdout)
        row = self.records(result)[0]
        expected_omp = "4" if (platform.system() == "Darwin" and
                               platform.machine() == "arm64") else "1"
        self.assertEqual(row["omp"], expected_omp)
        self.assertEqual(row["blas"], "1")
        self.assertEqual(row["dynamic"], "FALSE")

    def test_strict_known_issue_mode_reaches_the_kernel(self):
        result = self.invoke("--suite", "legacy-package-tests.wl",
                             "--strict-known-issues")
        self.assertEqual(result.returncode, 0, result.stdout)
        self.assertEqual(self.records(result)[0]["strict"], "1")

    def check_interruption(self, exception, status, message):
        process = mock.Mock(pid=12345)
        process.communicate.side_effect = [exception, ("partial test output", None)]
        output = io.StringIO()
        with mock.patch.object(sys, "argv", [str(RUNNER_PATH), "--kernel", "fake",
                                            "--suite", "legacy-algebra-tests.wl"]), \
                mock.patch.object(RUNNER.subprocess, "Popen", return_value=process) as start, \
                mock.patch.object(RUNNER.os, "killpg") as kill, \
                contextlib.redirect_stdout(output):
            self.assertEqual(RUNNER.main(), status)
        self.assertTrue(start.call_args.kwargs["start_new_session"])
        kill.assert_called_once_with(12345, signal.SIGKILL)
        self.assertEqual(process.communicate.call_count, 2)
        self.assertIn("partial test output", output.getvalue())
        self.assertIn(message, output.getvalue())

    def test_timeout_kills_and_reaps_the_entire_kernel_group(self):
        self.check_interruption(subprocess.TimeoutExpired("fake", 300), 124, "timed out")

    def test_keyboard_interrupt_kills_and_reaps_the_entire_kernel_group(self):
        self.check_interruption(KeyboardInterrupt(), 130, "interrupted")

    def test_process_group_cleanup_tolerates_an_already_exited_kernel(self):
        process = mock.Mock(pid=12345)
        process.communicate.side_effect = [subprocess.TimeoutExpired("fake", 300),
                                          ("last output", None)]
        with mock.patch.object(sys, "argv", [str(RUNNER_PATH), "--kernel", "fake",
                                            "--suite", "legacy-algebra-tests.wl"]), \
                mock.patch.object(RUNNER.subprocess, "Popen", return_value=process), \
                mock.patch.object(RUNNER.os, "killpg", side_effect=ProcessLookupError), \
                contextlib.redirect_stdout(io.StringIO()):
            self.assertEqual(RUNNER.main(), 124)
        self.assertEqual(process.communicate.call_count, 2)

    def test_live_timeout_terminates_descendants_holding_the_output_pipe(self):
        started = time.monotonic()
        result = self.invoke("--suite", "legacy-algebra-tests.wl", "--timeout", "0.5",
                             env={"HEPCAT_FAKE_SLEEP": "1"})
        self.assertEqual(result.returncode, 124, result.stdout)
        self.assertIn("child holds output pipe", result.stdout)
        self.assertIn("kernel group terminated", result.stdout)
        self.assertLess(time.monotonic() - started, 5)


@unittest.skipUnless(Path(WOLFRAM_KERNEL).is_file(), "A local Wolfram kernel is required.")
class WolframWrapperTests(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory(prefix="hepcat-syntax-")
        self.addCleanup(self.directory.cleanup)
        self.fixture = Path(self.directory.name) / "fixture.wl"

    def invoke_fixture(self, source):
        self.fixture.write_text(source, encoding="utf-8")
        environment = dict(os.environ)
        environment["HEPCAT_TEST_SUITE_PATH"] = str(self.fixture)
        return subprocess.run(
            [WOLFRAM_KERNEL, "-script", str(RUNNER_PATH.with_name("run-suite.wl"))],
            text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
            timeout=30, env=environment,
        )

    def test_malformed_source_is_rejected_before_any_side_effect(self):
        import json
        side_effect = Path(self.directory.name) / "must-not-exist.txt"
        result = self.invoke_fixture(
            "Export[" + json.dumps(str(side_effect)) + ", \"unexpected\", \"Text\"];\nunfinished["
        )
        self.assertEqual(result.returncode, 2, result.stdout)
        self.assertIn("failed syntax validation", result.stdout)
        self.assertFalse(side_effect.exists())

    def test_return_without_the_suites_final_exit_is_failure(self):
        result = self.invoke_fixture('Print["A deliberately truncated test suite."];')
        self.assertEqual(result.returncode, 1, result.stdout)
        self.assertIn("returned without its final exit", result.stdout)

    def test_complete_valid_source_runs_normally(self):
        result = self.invoke_fixture('Print["HEPCAT_TEST_COMPLETED:fixture.wl"]; Quit[0];')
        self.assertEqual(result.returncode, 0, result.stdout)
        self.assertIn("HEPCAT_TEST_COMPLETED:fixture.wl", result.stdout)


if __name__ == "__main__":
    unittest.main()
