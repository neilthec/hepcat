"""Test launcher configuration without starting a Wolfram process."""
import contextlib
import importlib.util
import io
from pathlib import Path
import unittest
from unittest.mock import patch

spec = importlib.util.spec_from_file_location("launcher", Path(__file__).with_name("train-unscrambling.py"))
launcher = importlib.util.module_from_spec(spec)
spec.loader.exec_module(launcher)


class LauncherTests(unittest.TestCase):
    def test_data_controls_reach_notebook_environment(self):
        with patch.object(launcher.shutil, "which", return_value="/bin/wolframscript"), \
                patch.object(launcher.os, "execvpe") as execute, contextlib.redirect_stdout(io.StringIO()):
            launcher.main(["--threads", "4", "--scrambles", "12", "--steps", "5",
                           "--rounds", "3", "--holdout", "0", "--amplitudes", "6",
                           "--episode-length", "8", "--batch-size", "2", "--worker-threads", "1",
                           "--model-directory", "/tmp/hepcat-test"])
            env = execute.call_args.args[2]
            for key, value in {"SCRAMBLES": "12", "STEPS": "5", "ROUNDS": "3", "HOLDOUT": "0",
                               "AMPLITUDES": "6", "EPISODE_LENGTH": "8", "BATCH_SIZE": "2", "WORKER_THREADS": "1",
                               "MODEL_DIRECTORY": str(Path("/tmp/hepcat-test").resolve())}.items():
                self.assertEqual(env["HEPCAT_TRAIN_" + key], value)

    def test_invalid_data_controls(self):
        for option, value in [("scrambles", "0"), ("rounds", "0"), ("amplitudes", "0"),
                              ("steps", "-1"), ("holdout", "-1"), ("batch-size", "0"),
                              ("episode-length", "0"), ("worker-threads", "0")]:
            with self.subTest(option=option), contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit):
                launcher.main(["--threads", "4", "--" + option, value, "--dry-run"])

    def test_apple_silicon_openmp_floor(self):
        with patch.object(launcher.platform, "system", return_value="Darwin"), \
                patch.object(launcher.platform, "machine", return_value="arm64"):
            for requested in (1, 2, 4, 8):
                env = launcher.training_environment(requested, {})
                self.assertEqual(env["OMP_NUM_THREADS"], str(max(4, requested)))
                self.assertEqual(env["OPENBLAS_NUM_THREADS"], str(requested))

    def test_linux_keeps_requested_openmp_threads(self):
        with patch.object(launcher.platform, "system", return_value="Linux"):
            self.assertEqual(launcher.training_environment(1, {})["OMP_NUM_THREADS"], "1")

    def test_thread_settings(self):
        base = {"PATH": "/bin", "OMP_NUM_THREADS": "1"}
        env = launcher.training_environment(4, base)
        for name in ("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS"):
            self.assertEqual(env[name], "4")
        self.assertEqual(env["MXNET_CPU_WORKER_NTHREADS"], "1")
        self.assertEqual(env["PATH"], "/bin")
        self.assertEqual(base["OMP_NUM_THREADS"], "1")

    def test_quoted_path(self):
        command = launcher.notebook_command("wolframscript", Path('/tmp/a "quoted" name.nb'))
        self.assertEqual(command, ["wolframscript", "-file", '/tmp/a "quoted" name.wl'])

    def test_invalid_threads(self):
        with contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit):
            launcher.main(["--threads", "0", "--dry-run"])

    def test_dry_run_does_not_start_process(self):
        with patch.object(launcher.os, "execvpe") as execute, contextlib.redirect_stdout(io.StringIO()):
            self.assertEqual(launcher.main(["--threads", "4", "--dry-run"]), 0)
            execute.assert_not_called()

    def test_exec_receives_environment(self):
        with patch.object(launcher.shutil, "which", return_value="/bin/wolframscript"), \
                patch.object(launcher.os, "execvpe") as execute, contextlib.redirect_stdout(io.StringIO()):
            launcher.main(["--threads", "4", "--kernels", "2"])
            executable, command, env = execute.call_args.args
            self.assertEqual(executable, command[0])
            self.assertEqual(env["OMP_NUM_THREADS"], "4")
            self.assertEqual(env["HEPCAT_TRAIN_KERNELS"], "2")
            self.assertIn("train-unscrambling.wl", command[2])


if __name__ == "__main__":
    unittest.main()
