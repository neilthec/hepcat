"""MadEvent adapter regressions without a MadGraph installation or physics fit.

The compiled fixture checks the Fortran-to-C ABI, current parameter values and
normalization. A genuine MadEvent survey/refine/LHE test is separate.
"""
import copy
import contextlib
import io
import os
import hashlib
import json
import importlib.util
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest
from unittest import mock

sys.dont_write_bytecode = True
_HELPER = Path(__file__).resolve().parents[2] / "source" / "madevent-runtime.py"
_SPEC = importlib.util.spec_from_file_location("hepcat_madevent_builder", _HELPER)
builder = importlib.util.module_from_spec(_SPEC)
_SPEC.loader.exec_module(builder)


class MadEventBuilderTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="hepcat-madevent-unit-")
        self.directory = Path(self.temporary.name).resolve()
        self.previous_directory = Path.cwd()
        os.chdir(self.directory)
        self.config = {
            "MadGraphDirectory": str(self.directory / "chosen-MadGraph"),
            "MadGraphModel": "explicit-test-ufo",
            "ProcessDirectory": str(self.directory / "export"),
            "BridgeLinkFile": "build/madevent-link.json",
            "PhysicalPDGs": [11, -11, 22, 22],
            "Spin2": [1, 1, 2, 2], "LegOrder": [1, 2, 3, 4],
            "Masses": [.0005, .0005, 0, 0],
            "ParameterNames": ["EE", "Me"], "ParameterDefaults": [.313, .0005],
            "ParameterMap": {"EE": "ee", "Me": "Me"},
            "InitialSpinAverage": 4, "SymmetryFactor": .5,
            "HelicityConfigurations": [[0, 0, a, b] for a in [-2, 2] for b in [-2, 2]],
        }

    def tearDown(self):
        os.chdir(self.previous_directory)
        self.temporary.cleanup()

    def make_export(self, config=None):
        config = self.config if config is None else config
        project = Path(config["ProcessDirectory"])
        project.mkdir(parents=True, exist_ok=True)
        link_path = Path(config["BridgeLinkFile"])
        if not link_path.is_absolute():
            link_path = project / link_path
        link_path.parent.mkdir(parents=True, exist_ok=True)
        library = link_path.parent / "libfixture.a"
        library.touch()
        headers = self.directory / "spinas-dependency" / "include"
        headers.mkdir(parents=True, exist_ok=True)
        (headers / "spinas.h").write_text("// standalone SPINAS header fixture\n")
        (headers / "types.h").write_text("// scalar and complex types fixture\n")
        for filename in ("process.cpp", "process.hpp", "madevent-spinas.cpp",
                         "spinas-runtime.hpp", "spinas-validation.hpp"):
            (project / filename).write_text("// exported amplitude source: " + filename + "\n")
        link_path.write_text(json.dumps({
            "Libraries": [str(library)], "CXXRuntime": "c++",
            "SPINASIncludeDirectory": str(headers)}))
        return project

    def make_owned_run(self, workspace, config=None):
        config = self.config if config is None else config
        self.make_export(config)
        process = workspace / config.get("SessionDirectory", "madevent")
        (process / "bin" / "internal").mkdir(parents=True)
        (process / "Cards").mkdir()
        (process / "bin" / "madevent").write_text("import internal.madevent_interface as cmd_interface\n")
        matrix = process / "matrix.f"
        matrix.write_text("SPINAS numerical hook\n")
        local_library = process / "lib" / "hepcat" / "libfixture.a"
        local_library.parent.mkdir(parents=True)
        local_library.write_bytes(b"self-contained archive fixture\n")
        source = process / "Source" / "HEPCAT" / "process.hpp"
        source.parent.mkdir(parents=True)
        source.write_text("// bundled amplitude declarations\n")
        relative_library = str(local_library.relative_to(process))
        bundle = {"Version": 1, "Libraries": [relative_library],
                  "Sources": [str(source.relative_to(process))],
                  "LibraryHashes": [{"Path": relative_library,
                                     "SHA256": hashlib.sha256(local_library.read_bytes()).hexdigest()}]}
        normalized = builder.validate_config(config)
        report = {"Status": "READY", "Backend": "HEPCAT SPINAS",
                  "ExportDirectory": config["ProcessDirectory"], "MadEventDirectory": str(process.resolve()),
                  "WorkingDirectory": str(workspace.resolve()),
                  "Bundle": bundle,
                  "LinkConfiguration": {"Libraries": [relative_library],
                      "CXXRuntime": "c++", "RuntimeLibraryDirectories": [],
                      "RelativeTo": "MadEventDirectory"},
                  "ConfigurationSHA256": builder.configuration_fingerprint(
                      normalized, builder.load_link_configuration(normalized)),
                  "MatrixHashes": [{"Path": "matrix.f", "SHA256": hashlib.sha256(matrix.read_bytes()).hexdigest()}]}
        (process / "hepcat-madevent.json").write_text(json.dumps(report))
        return process, report

    def test_validation_has_no_side_effects_or_implicit_model(self):
        original = copy.deepcopy(self.config)
        result = builder.validate_config(self.config)
        self.assertEqual(self.config, original)
        self.assertEqual(result["MadGraphModel"], "explicit-test-ufo")
        self.assertFalse((self.directory / "export").exists())
        for field in ["MadGraphDirectory", "MadGraphModel", "BridgeLinkFile"]:
            for value in [None, "", "bad\npath"]:
                with self.subTest(field=field, value=value):
                    with self.assertRaises(builder.MadEventSetupError):
                        builder.validate_config(dict(self.config, **{field: value}))

    def test_physical_order_controls_mass_and_spin_metadata(self):
        result = builder.validate_config(dict(self.config,
            PhysicalPDGs=[22, 11, 11, 22], LegOrder=[3, 1, 2, 4],
            InitialSpinAverage=4, SymmetryFactor=1))
        self.assertEqual(result["_PhysicalMasses"], [0, .0005, .0005, 0])
        self.assertEqual(result["_PhysicalSpin2"], [2, 1, 1, 2])
        self.assertEqual(result["_MassiveLegs"], [2, 3])

    def test_fixed_incoming_helicity_has_no_extra_average(self):
        result = builder.validate_config(dict(self.config,
            PhysicalPDGs=[22, 11, 11, 22], LegOrder=[3, 1, 2, 4],
            HelicityConfigurations=[[0, 0, 2, b] for b in [-2, 2]],
            InitialSpinAverage=2, SymmetryFactor=1))
        self.assertEqual(result["_MGInitialSpinDivisor"], 4)
        self.assertEqual(result["InitialSpinAverage"], 2)
        self.assertEqual(result["_FixedIncomingLegs"], [1])
        with self.assertRaisesRegex(builder.MadEventSetupError, "selected incoming"):
            builder.validate_config(dict(result, InitialSpinAverage=4))

    def test_fixed_outgoing_preserves_identical_species_factor(self):
        result = builder.validate_config(dict(self.config,
            HelicityConfigurations=[[0, 0, 2, b] for b in [-2, 2]]))
        self.assertEqual(result["_FixedIncomingLegs"], [])
        self.assertEqual(result["InitialSpinAverage"], 4)
        self.assertEqual(result["SymmetryFactor"], .5)
        with self.assertRaisesRegex(builder.MadEventSetupError, "factorial"):
            builder.validate_config(dict(result, SymmetryFactor=1))

    def test_leg_order_is_an_integer_permutation(self):
        for order in [[1, 1, 3, 4], [0, 1, 2, 3], [True, 2, 3, 4], [1., 2., 3., 4.]]:
            with self.subTest(order=order):
                with self.assertRaises(builder.MadEventSetupError):
                    builder.validate_config(dict(self.config, LegOrder=order))

    def test_bad_helicity_metadata_is_not_silently_unpolarized(self):
        for helicities in [{}, "all", [[0, 0, 2]], [[0, 0, 1, 2]], [[0, 0, [], 2]]]:
            with self.subTest(helicities=helicities):
                with self.assertRaises(builder.MadEventSetupError):
                    builder.validate_config(dict(self.config, HelicityConfigurations=helicities))

    def test_parameter_order_and_finite_values_are_required(self):
        for extra in [dict(ParameterNames=["EE", "ee"]),
                      dict(ParameterDefaults=[.3]),
                      dict(ParameterDefaults=[float("nan"), .1]),
                      dict(Masses=[.1, -.1, 0, 0]),
                      dict(TimeConstraint=0)]:
            with self.subTest(extra=extra):
                with self.assertRaises(builder.MadEventSetupError):
                    builder.validate_config(dict(self.config, **extra))

    def test_active_maps_must_be_distinct_and_complete(self):
        source = " DATA MAPCONFIG(0)/2/\n DATA MAPCONFIG(1)/1/\n DATA MAPCONFIG(2)/3/\n"
        self.assertEqual(builder.mapconfig_representatives(source), [1, 3])
        for bad in ["", source.replace("(2)/3", "(2)/1"), source.replace("(2)/3", "(3)/3")]:
            with self.subTest(bad=bad):
                with self.assertRaises(builder.MadEventSetupError):
                    builder.mapconfig_representatives(bad)

    def test_hook_preserves_native_wrapper_and_helpers(self):
        prefix = "      SUBROUTINE SMATRIX\n      RETURN\n      END\n"
        body = "      REAL*8 FUNCTION MATRIX(P,NHEL,IC,IVEC)\n      CALL OLD_HELAS\n      END\n"
        suffix = "      SUBROUTINE KEEP_HELPER\n      END\n"
        result = builder.matrix_hook(prefix + body + suffix, self.config,
                                     ["MDL_EE", "MDL_ME"], [1, 3])
        self.assertTrue(result.startswith(prefix))
        self.assertTrue(result.endswith(suffix))
        self.assertNotIn("CALL OLD_HELAS", result)
        self.assertIn("BIND(C,NAME='hepcat_madevent_matrix')", result)
        for bad in [body.replace("IC,IVEC", "IC"), body + body, body.replace("      END", "")]:
            with self.subTest(source=bad):
                with self.assertRaises(builder.MadEventSetupError):
                    builder.matrix_hook(bad, self.config, ["MDL_EE", "MDL_ME"], [1, 3])

    def test_live_fortran_parameters_are_verified_not_baked_defaults(self):
        model = self.directory / "Source" / "MODEL"
        model.mkdir(parents=True)
        (model / "input.inc").write_text("      DOUBLE PRECISION MDL_EE\n")
        (model / "coupl.inc").write_text("      DOUBLE PRECISION MDL_ME\n      DOUBLE COMPLEX GC_1\n")
        matches = [{"HEPCAT": "EE", "MadGraph": "ee"}, {"HEPCAT": "Me", "MadGraph": "Me"}]
        self.assertEqual(builder.fortran_parameter_expressions(self.directory, ["EE", "Me"], matches),
                         ["MDL_EE", "MDL_ME"])
        with self.assertRaisesRegex(builder.MadEventSetupError, "complex"):
            builder.fortran_parameter_expressions(self.directory, ["EE"], [{"HEPCAT": "EE", "MadGraph": "GC_1"}])
        with self.assertRaisesRegex(builder.MadEventSetupError, "unambiguous"):
            builder.fortran_parameter_expressions(self.directory, ["EE"], [{"HEPCAT": "EE", "MadGraph": "unknown"}])

    def test_run_card_update_keeps_unrelated_controls(self):
        path = self.directory / "run_card.dat"
        path.write_text(" 10 = nevents ! unchanged\n 1 = nhel ! forbidden sampling\n")
        builder.set_card_value(path, "nhel", "0")
        builder.set_card_value(path, "hel_recycling", ".False.")
        text = path.read_text()
        self.assertIn("10 = nevents", text)
        self.assertIn("0 = nhel", text)
        self.assertEqual(text.count("= nhel"), 1)
        self.assertIn(".False. = hel_recycling", text)

    def test_new_sessions_use_pointlike_beams_without_changing_energies_or_cuts(self):
        process = self.directory / "new-session"
        (process / "Cards").mkdir(parents=True)
        for name in ("run_card.dat", "run_card_default.dat"):
            (process / "Cards" / name).write_text(
                " 2 = lpp1 ! photon PDF chosen by MG\n"
                " 0 = lpp2 ! pointlike W beam\n"
                " 100 = ebeam1\n 200 = ebeam2\n 5 = ptj\n"
                " 1 = nhel\n True = hel_recycling\n")
        builder.initialize_run_cards(process)
        for name in ("run_card.dat", "run_card_default.dat"):
            text = (process / "Cards" / name).read_text()
            self.assertIn("0 = lpp1", text)
            self.assertIn("0 = lpp2", text)
            self.assertIn("0 = nhel", text)
            self.assertIn(".False. = hel_recycling", text)
            self.assertIn("100 = ebeam1", text)
            self.assertIn("200 = ebeam2", text)
            self.assertIn("5 = ptj", text)

    def test_native_run_card_api_and_matrix_integrity_are_guarded(self):
        process = self.directory / "madevent"
        (process / "bin" / "internal").mkdir(parents=True)
        (process / "bin" / "madevent").write_text("import internal.madevent_interface as cmd_interface\n")
        matrix = process / "matrix.f"
        matrix.write_text("verified SPINAS hook\n")
        report = {"MatrixHashes": [{"Path": "matrix.f", "SHA256": hashlib.sha256(matrix.read_bytes()).hexdigest()}],
                  "UnsupportedPolarizationLegs": [1]}
        (process / "hepcat-madevent.json").write_text(json.dumps(report))
        builder.install_runtime_guard(process)
        # Native MG ConfigFile.get accepts a different contract than dict.get.
        class NativeCard(dict):
            def get(self, key, *args):
                if args:
                    raise TypeError("Native ConfigFile.get has no default-value overload")
                return super().get(key)
        class Interface:
            InvalidCmd = RuntimeError
        interface = Interface()
        interface.me_dir = str(process)
        interface.run_card = NativeCard(nhel=0, hel_recycling=False, polbeam1=0)
        path = process / "bin" / "internal" / "hepcat_madevent_guard.py"
        spec = importlib.util.spec_from_file_location("hepcat_guard_fixture", path)
        guard = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(guard)
        guard.check(interface)
        for extra in [dict(nhel=1), dict(hel_recycling=True), dict(polbeam1=25)]:
            with self.subTest(extra=extra):
                interface.run_card = NativeCard(nhel=0, hel_recycling=False, polbeam1=0)
                interface.run_card.update(extra)
                with self.assertRaises(RuntimeError):
                    guard.check(interface)
        interface.run_card = NativeCard(nhel=0, hel_recycling=False, polbeam1=0)
        matrix.write_text("changed matrix\n")
        with self.assertRaisesRegex(RuntimeError, "changed or regenerated"):
            guard.check(interface)

    def test_native_generation_noops_do_not_enable_spin_adapters(self):
        process = self.directory / "madevent"
        (process / "bin" / "internal").mkdir(parents=True)
        (process / "Cards").mkdir()
        launcher = process / "bin" / "madevent"
        launcher.write_text("import internal.madevent_interface as cmd_interface\n")
        builder.install_runtime_guard(process)
        builder.install_runtime_guard(process)
        self.assertEqual(launcher.read_text().count("hepcat_guard.install(cmd_interface.MadEventCmd)"), 1)
        path = process / "bin" / "internal" / "hepcat_madevent_guard.py"
        spec = importlib.util.spec_from_file_location("hepcat_guard_noop_fixture", path)
        guard = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(guard)
        class NativeInterface:
            InvalidCmd = RuntimeError
            me_dir = str(process)
            def do_survey(self, *args):
                return "survey"
            def do_refine(self, *args):
                return "refine"
            def do_generate_events(self, *args):
                return "events"
        guard.install(NativeInterface)
        interface = NativeInterface()
        interface.do_reweight("--mode=OFF -from_cards")
        interface.do_reweight("-from_cards")
        interface.do_decay_events("-from_cards")
        for method in [interface.do_reweight, interface.do_decay_events, interface.do_madspin]:
            with self.subTest(method=method):
                with self.assertRaisesRegex(RuntimeError, "unsupported"):
                    method("--mode=ON")
        for filename, method in [("reweight_card.dat", interface.do_reweight),
                                 ("madspin_card.dat", interface.do_decay_events)]:
            (process / "Cards" / filename).write_text("active card\n")
            with self.assertRaisesRegex(RuntimeError, "unsupported"):
                method("-from_cards")
        # Explicit OFF remains a no-op even with an inactive card on disk.
        interface.do_reweight("--mode=OFF -from_cards")

    def test_existing_private_runs_cannot_be_silently_overwritten(self):
        project = self.make_export()
        process = self.directory / "madevent"
        process.mkdir()
        with self.assertRaisesRegex(builder.MadEventSetupError, "ownership"):
            builder.prepare(self.config)
        process.rmdir()
        process, report = self.make_owned_run(self.directory)
        self.assertEqual(builder.prepare(self.config), report)
        with self.assertRaisesRegex(builder.MadEventSetupError, "metadata changed"):
            builder.prepare(dict(self.config, ParameterDefaults=[.4, .0005]))
        link_path = project / "build" / "madevent-link.json"
        link_before = link_path.read_text()
        link_data = json.loads(link_before)
        link_data["CXXRuntime"] = "stdc++"
        link_path.write_text(json.dumps(link_data))
        with self.assertRaisesRegex(builder.MadEventSetupError, "metadata changed"):
            builder.prepare(self.config)
        link_path.write_text(link_before)
        (process / "matrix.f").write_text("regenerated HELAS matrix\n")
        with self.assertRaisesRegex(builder.MadEventSetupError, "modified or regenerated"):
            builder.prepare(self.config)

    def test_refresh_owns_the_export_and_keeps_old_files_on_failed_preparation(self):
        process, report = self.make_owned_run(self.directory)
        card = process / "Cards" / "run_card.dat"
        card.write_text("user controls\n")
        manifest = process / "hepcat-madevent.json"
        previous_support = self.directory / ".hepcat-madevent-support"
        previous_support.mkdir()
        changed = dict(self.config, MassParameterPDGs={"Me": [11]})
        with self.assertRaisesRegex(builder.MadEventSetupError, "metadata changed"):
            builder.prepare(changed)
        # Model generation must be reached despite the previous support folder.
        class FakeReference:
            ValidationSetupError = ValueError

            def checked_configuration(self, data):
                raise ValueError("deliberate preparation failure")
        with mock.patch.object(builder, "reference_runtime", return_value=FakeReference()):
            with self.assertRaisesRegex(builder.MadEventSetupError, "deliberate preparation failure"):
                builder.prepare(changed, refresh_generated=True)
        self.assertEqual(json.loads(manifest.read_text()), report)
        self.assertEqual(card.read_text(), "user controls\n")
        self.assertTrue(previous_support.is_dir())
        self.assertEqual(list(self.directory.glob("madevent.previous-*")), [])

        foreign = dict(report, ExportDirectory=str(self.directory / "another-export"))
        manifest.write_text(json.dumps(foreign))
        with self.assertRaisesRegex(builder.MadEventSetupError, "metadata changed"):
            builder.prepare(changed, refresh_generated=True)
        manifest.unlink()
        with self.assertRaisesRegex(builder.MadEventSetupError, "ownership"):
            builder.prepare(changed, refresh_generated=True)

    def test_ready_replacement_retains_the_previous_session_and_rolls_back_on_rename_error(self):
        final, _ = self.make_owned_run(self.directory)
        event = final / "Events" / "saved.lhe"
        event.parent.mkdir()
        event.write_text("original events\n")
        previous = (final / "hepcat-madevent.json").read_bytes()
        process = self.directory / "replacement"
        process.mkdir()
        (process / "new-matrix.f").write_text("new generated code\n")
        original_rename = Path.rename

        def fail_promotion(path, destination):
            if path == process:
                raise OSError("deliberate publish failure")
            return original_rename(path, destination)

        with mock.patch.object(Path, "rename", fail_promotion):
            with self.assertRaisesRegex(OSError, "deliberate publish failure"):
                builder.replace_session(process, final, {}, previous)
        self.assertEqual(event.read_text(), "original events\n")
        self.assertEqual((final / "hepcat-madevent.json").read_bytes(), previous)

        report = {}
        builder.replace_session(process, final, report, previous)
        archived = Path(report["PreviousSessionDirectory"])
        self.assertEqual((archived / "Events" / "saved.lhe").read_text(), "original events\n")
        self.assertEqual((archived / "hepcat-madevent.json").read_bytes(), previous)
        self.assertTrue((final / "new-matrix.f").is_file())
        self.assertFalse(event.exists())

    def test_legacy_default_session_uses_launch_directory_and_preserves_export(self):
        project = self.make_export()
        legacy = project / "madevent"
        legacy.mkdir()
        (legacy / "old-events.lhe").write_text("preserve previous results\n")
        process, report = self.make_owned_run(self.directory)
        (process / "Cards" / "run_card.dat").write_text("current user cards\n")
        self.assertEqual(builder.prepare(self.config), report)
        self.assertEqual(process, Path.cwd() / "madevent")
        self.assertEqual((process / "Cards" / "run_card.dat").read_text(), "current user cards\n")
        self.assertEqual((legacy / "old-events.lhe").read_text(), "preserve previous results\n")
        self.assertEqual(Path(self.config["ProcessDirectory"]), project)

    def test_build_directory_session_preserves_runs_and_ignores_launch_directory(self):
        config = dict(self.config, SessionDirectory="PROC_eeAA",
                      SessionDirectoryBase="BuildDirectory",
                      BridgeLinkFile="compiled outputs/madevent-link.json")
        project = self.make_export(config)
        build = project / "compiled outputs"
        process, report = self.make_owned_run(build, config)
        (process / "Cards" / "run_card.dat").write_text("edited numerical controls\n")
        event = process / "Events" / "old_run" / "unweighted_events.lhe"
        event.parent.mkdir(parents=True)
        event.write_text("keep the generated events\n")
        legacy = project / "PROC_eeAA"
        legacy.mkdir()
        (legacy / "older-events.lhe").write_text("keep the earlier source-root run\n")

        for launch_directory in (project, self.directory):
            with self.subTest(launch_directory=launch_directory):
                os.chdir(launch_directory)
                self.assertEqual(builder.prepare(config), report)
        self.assertEqual(Path(report["WorkingDirectory"]), build)
        self.assertEqual(process, build / "PROC_eeAA")
        self.assertEqual((process / "Cards" / "run_card.dat").read_text(), "edited numerical controls\n")
        self.assertEqual(event.read_text(), "keep the generated events\n")
        self.assertEqual((legacy / "older-events.lhe").read_text(), "keep the earlier source-root run\n")
        self.assertFalse((self.directory / "PROC_eeAA").exists())

    def test_build_directory_staging_does_not_mix_generated_support_with_source(self):
        config = dict(self.config, SessionDirectory="PROC_eeAA",
                      SessionDirectoryBase="BuildDirectory")
        project = self.make_export(config)
        class FakeReference:
            ValidationSetupError = ValueError
            def checked_configuration(self, data):
                raise ValueError("deliberate stop before model generation")
        os.chdir(project)
        with mock.patch.object(builder, "reference_runtime", return_value=FakeReference()):
            with self.assertRaisesRegex(builder.MadEventSetupError, "deliberate stop"):
                builder.prepare(config)
        self.assertTrue((project / "build" / ".hepcat-madevent-support").is_dir())
        self.assertFalse((project / ".hepcat-madevent-support").exists())
        self.assertFalse((project / "PROC_eeAA").exists())

    def test_generation_staging_is_in_launch_directory_not_export(self):
        project = self.make_export()
        class FakeReference:
            ValidationSetupError = ValueError
            def checked_configuration(self, data):
                raise ValueError("deliberate stop before model generation")
        with mock.patch.object(builder, "reference_runtime", return_value=FakeReference()):
            with self.assertRaisesRegex(builder.MadEventSetupError, "deliberate stop"):
                builder.prepare(self.config)
        self.assertTrue((self.directory / ".hepcat-madevent-support").is_dir())
        self.assertFalse((project / ".hepcat-madevent-support").exists())
        self.assertFalse((project / "madevent").exists())

    def test_explicit_workspace_and_cli_inputs_resolve_from_launch_directory(self):
        workspace = self.directory / "chosen output"
        workspace.mkdir()
        config = dict(self.config, SessionDirectoryBase="BuildDirectory")
        process, report = self.make_owned_run(workspace, config)
        project = Path(config["ProcessDirectory"])
        config_path = project / "madevent-config.json"
        config_path.write_text(json.dumps(config))
        inputs = self.directory / "inputs"
        inputs.mkdir()
        (inputs / "run_card.dat").write_text("requested run controls\n")
        (inputs / "param_card.dat").write_text("requested model inputs\n")
        (inputs / "commands.txt").write_text("quit\n")
        output = io.StringIO()
        with mock.patch.object(builder.subprocess, "call", return_value=0) as call, \
             contextlib.redirect_stdout(output):
            status = builder.main(["--configuration", str(config_path),
                "--work-directory", "chosen output", "--run-card", "inputs/run_card.dat",
                "--param-card", "inputs/param_card.dat", "--command-file", "inputs/commands.txt"])
        self.assertEqual(status, 0)
        self.assertEqual(call.call_args.kwargs["cwd"], process)
        self.assertEqual(Path(call.call_args.args[0][-1]), inputs / "commands.txt")
        self.assertEqual((process / "Cards" / "run_card.dat").read_text(), "requested run controls\n")
        self.assertEqual((process / "Cards" / "param_card.dat").read_text(), "requested model inputs\n")
        for path in [process / "index.html", process / "Events", process / "Cards"]:
            self.assertIn(str(path), output.getvalue())
        self.assertFalse((self.directory / "madevent").exists())
        self.assertFalse((project / "madevent").exists())
        # Preparation alone must report the same paths and retain edited cards.
        with contextlib.redirect_stdout(output):
            self.assertEqual(builder.main(["--configuration", str(config_path),
                "--work-directory", "chosen output", "--prepare"]), 0)
        self.assertEqual((process / "Cards" / "run_card.dat").read_text(), "requested run controls\n")
        self.assertEqual(builder.prepare(config, root=workspace), report)
        self.assertFalse((project / "build" / "madevent").exists())

    def test_cli_uses_the_selected_cmake_build_link_file(self):
        build = self.directory / "custom build"
        config = dict(self.config, SessionDirectory="PROC_eeAA",
                      SessionDirectoryBase="BuildDirectory",
                      BridgeLinkFile=str(build / "madevent-link.json"))
        process, _ = self.make_owned_run(build, config)
        project = Path(config["ProcessDirectory"])
        saved = dict(config, BridgeLinkFile="obsolete build/madevent-link.json")
        path = project / "madevent-config.json"
        path.write_text(json.dumps(saved))
        with contextlib.redirect_stdout(io.StringIO()):
            status = builder.main(["--configuration", str(path),
                "--bridge-link-file", str(build / "madevent-link.json"), "--prepare"])
        self.assertEqual(status, 0)
        self.assertTrue((process / "bin" / "madevent").is_file())
        self.assertFalse((project / "build" / "PROC_eeAA").exists())
        self.assertEqual(json.loads(path.read_text())["BridgeLinkFile"],
                         "obsolete build/madevent-link.json")

    def test_one_workspace_cannot_mix_different_exported_amplitudes(self):
        self.make_owned_run(self.directory)
        other = dict(self.config, ProcessDirectory=str(self.directory / "other export"))
        self.make_export(other)
        with self.assertRaisesRegex(builder.MadEventSetupError, "metadata changed"):
            builder.prepare(other)

    def test_process_named_sessions_can_share_a_working_directory(self):
        first = dict(self.config, SessionDirectory="PROC_first")
        second = dict(self.config, SessionDirectory="PROC_second",
                      ProcessDirectory=str(self.directory / "second export"))
        first_process, first_report = self.make_owned_run(self.directory, first)
        second_process, second_report = self.make_owned_run(self.directory, second)
        (first_process / "Cards" / "run_card.dat").write_text("first process controls\n")
        (second_process / "Cards" / "run_card.dat").write_text("second process controls\n")
        self.assertEqual(builder.prepare(first), first_report)
        self.assertEqual(builder.prepare(second), second_report)
        self.assertEqual((first_process / "Cards" / "run_card.dat").read_text(), "first process controls\n")
        self.assertEqual((second_process / "Cards" / "run_card.dat").read_text(), "second process controls\n")
        for name in [".", "..", "/absolute/output", "parent/output", "bad\nname"]:
            with self.subTest(name=name):
                with self.assertRaisesRegex(builder.MadEventSetupError, "SessionDirectory"):
                    builder.prepare(dict(self.config, SessionDirectory=name))

    def test_relative_source_paths_follow_configuration_not_output_directory(self):
        process, report = self.make_owned_run(self.directory)
        project = Path(self.config["ProcessDirectory"])
        config_path = project / "madevent-config.json"
        config_path.write_text(json.dumps(dict(self.config, ProcessDirectory=".")))
        output = io.StringIO()
        with contextlib.redirect_stdout(output):
            status = builder.main(["--configuration", "export/madevent-config.json", "--prepare"])
        self.assertEqual(status, 0)
        self.assertIn(str(process / "Events"), output.getvalue())
        self.assertFalse((project / "madevent").exists())

    def test_bundle_keeps_sources_headers_and_archives_after_export_removal(self):
        project = self.make_export()
        original = project / "build" / "libfixture.a"
        original.write_bytes(b"independent compiled amplitude archive\n")
        config = builder.validate_config(self.config)
        link = builder.load_link_configuration(config)
        process = self.directory / "bundle"
        process.mkdir()
        bundle, local_link = builder.bundle_dependencies(process, config, link)
        self.assertEqual(bundle["Version"], 1)
        self.assertTrue(bundle["Libraries"])
        self.assertEqual(local_link["RelativeTo"], "MadEventDirectory")
        self.assertEqual(local_link["Libraries"], bundle["Libraries"])
        for name in bundle["Libraries"] + bundle["Sources"]:
            self.assertFalse(Path(name).is_absolute())
            path = process / name
            self.assertTrue(path.is_file(), name)
            self.assertFalse(path.is_symlink(), name)
            self.assertTrue(path.resolve().is_relative_to(process.resolve()), name)
        for name in ("process.cpp", "process.hpp", "madevent-spinas.cpp"):
            self.assertTrue((process / "Source" / "HEPCAT" / name).is_file())
        self.assertTrue((process / "Source" / "HEPCAT" / "spinas" / "spinas.h").is_file())
        self.assertNotIn(str(project), json.dumps(local_link))
        self.assertNotIn(str(self.directory / "spinas-dependency"), json.dumps(local_link))
        self.assertEqual((process / bundle["Libraries"][0]).read_bytes(), original.read_bytes())
        for row in bundle["LibraryHashes"]:
            self.assertEqual(row["SHA256"], hashlib.sha256((process / row["Path"]).read_bytes()).hexdigest())
        shutil.rmtree(project)
        shutil.rmtree(self.directory / "spinas-dependency")
        self.assertTrue(all((process / name).is_file() for name in bundle["Libraries"] + bundle["Sources"]))

    def test_bundle_dereferences_external_library_and_header_symlinks(self):
        project = self.make_export()
        original = project / "build" / "libfixture.a"
        actual = self.directory / "actual-library.a"
        actual.write_bytes(b"archive behind a build-tree symlink\n")
        original.unlink()
        original.symlink_to(actual)
        header = self.directory / "actual-header.h"
        header.write_text("// header outside the include tree\n")
        include = self.directory / "spinas-dependency" / "include"
        (include / "external.h").symlink_to(header)
        process = self.directory / "bundle"
        process.mkdir()
        config = builder.validate_config(self.config)
        bundle, _ = builder.bundle_dependencies(process, config, builder.load_link_configuration(config))
        archived = process / bundle["Libraries"][0]
        copied_header = process / "Source" / "HEPCAT" / "spinas" / "external.h"
        self.assertFalse(archived.is_symlink())
        self.assertEqual(archived.read_bytes(), actual.read_bytes())
        self.assertFalse(copied_header.is_symlink())
        self.assertEqual(copied_header.read_text(), header.read_text())
        actual.unlink()
        header.unlink()
        self.assertTrue(archived.is_file() and copied_header.is_file())

    def test_bundle_prefers_available_static_spinas_over_shared_build_library(self):
        project = self.make_export()
        shared = project / "build" / "libspinas_shared.dylib"
        shared.write_bytes(b"not needed when a static SPINAS archive exists\n")
        static = project / "build" / "libspinas.a"
        static.write_bytes(b"standalone SPINAS archive\n")
        config = builder.validate_config(self.config)
        link = builder.load_link_configuration(config)
        link.update(Libraries=[str(shared)], SPINASStaticLibrary=str(static))
        process = self.directory / "bundle"
        process.mkdir()
        bundle, local_link = builder.bundle_dependencies(process, config, link)
        self.assertEqual([Path(name).name for name in bundle["Libraries"]], ["libspinas.a"])
        self.assertEqual(local_link.get("RuntimeLibraryDirectories", []), [])
        self.assertEqual((process / bundle["Libraries"][0]).read_bytes(), static.read_bytes())

    def test_cached_process_refreshes_changed_libraries_and_preserves_user_data(self):
        process, report = self.make_owned_run(self.directory)
        project = Path(self.config["ProcessDirectory"])
        original = project / "build" / "libfixture.a"
        original.write_bytes(b"first compiled amplitude\n")
        (process / "Source" / "make_opts").write_text("MACFLAG=-mmacosx-version-min=10.8\n")
        (process / "SubProcesses").mkdir()
        makefile = process / "SubProcesses" / "makefile"
        makefile.write_text("LINKLIBS = -lmodel\n")
        config = builder.validate_config(self.config)
        link = builder.load_link_configuration(config)
        bundle, local_link = builder.bundle_dependencies(process, config, link)
        builder.patch_linking(process, local_link)
        report.update(Bundle=bundle, LinkConfiguration=local_link)
        manifest = process / "hepcat-madevent.json"
        manifest.write_text(json.dumps(report))
        run_card = process / "Cards" / "run_card.dat"
        run_card.write_text("my selected beam energies and cuts\n")
        param_card = process / "Cards" / "param_card.dat"
        param_card.write_text("my updated masses and couplings\n")
        event = process / "Events" / "previous_run" / "unweighted_events.lhe"
        event.parent.mkdir(parents=True)
        event.write_text("previous numerical results\n")

        original.write_bytes(b"recompiled, corrected amplitude\n")
        (project / "process.cpp").write_text("// corrected generated amplitude\n")
        with mock.patch.object(builder, "reference_runtime", side_effect=AssertionError(
                "refresh must not regenerate the native process")):
            refreshed = builder.prepare(self.config)
        self.assertNotEqual(refreshed["Bundle"]["SourceLibraryHashes"],
                            bundle["SourceLibraryHashes"])
        self.assertEqual(refreshed["Bundle"]["SourceLibraryHashes"],
                         builder.library_source_hashes(link))
        self.assertEqual((process / refreshed["Bundle"]["Libraries"][0]).read_bytes(),
                         original.read_bytes())
        self.assertEqual((process / "Source" / "HEPCAT" / "process.cpp").read_text(),
                         "// corrected generated amplitude\n")
        self.assertEqual(run_card.read_text(), "my selected beam energies and cuts\n")
        self.assertEqual(param_card.read_text(), "my updated masses and couplings\n")
        self.assertEqual(event.read_text(), "previous numerical results\n")
        self.assertEqual(makefile.read_text().count("# HEPCAT: link"), 1)
        self.assertEqual(json.loads(manifest.read_text())["Bundle"], refreshed["Bundle"])
        with mock.patch.object(builder, "bundle_dependencies", side_effect=AssertionError(
                "an unchanged compiled amplitude must keep its cached bundle")):
            self.assertEqual(builder.prepare(self.config), refreshed)

    @unittest.skipUnless(sys.platform == "darwin", "Mach-O shared-library relocation is macOS-specific")
    def test_shared_only_bundle_relinks_and_runs_after_originals_are_removed(self):
        compiler = shutil.which("c++")
        make = shutil.which("make")
        if not compiler or not make or not shutil.which("install_name_tool"):
            self.skipTest("A C++ compiler, make and install_name_tool are required")
        project = self.make_export()
        shared = project / "build" / "libspinas_shared.dylib"
        source = project / "probe.cpp"
        source.write_text('extern "C" double hepcat_bundle_probe() { return 6.25; }\n')
        result = subprocess.run([compiler, "-dynamiclib", str(source),
            "-Wl,-install_name," + str(shared), "-o", str(shared)],
            capture_output=True, text=True)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        config = builder.validate_config(self.config)
        link = builder.load_link_configuration(config)
        link["Libraries"] = [str(shared)]
        process = self.directory / "shared-process"
        (process / "Source").mkdir(parents=True)
        (process / "Source" / "make_opts").write_text("MACFLAG=-mmacosx-version-min=10.8\n")
        subprocess_directory = process / "SubProcesses" / "P0"
        subprocess_directory.mkdir(parents=True)
        (subprocess_directory / "main.cpp").write_text(
            '#include <iostream>\nextern "C" double hepcat_bundle_probe();\n'
            'int main() { std::cout << hepcat_bundle_probe() << "\\n"; }\n')
        (process / "SubProcesses" / "makefile").write_text(
            "CXX := " + compiler + "\nPROG := madevent\nLINKLIBS =\n"
            "$(PROG): main.o\n\t$(CXX) main.o $(LINKLIBS) -o $(PROG)\n"
            "main.o: main.cpp\n\t$(CXX) -c main.cpp -o main.o\n")
        bundle, local_link = builder.bundle_dependencies(process, config, link)
        self.assertEqual(local_link["RuntimeLibraryDirectories"], ["lib/hepcat"])
        self.assertEqual([Path(name).name for name in bundle["Libraries"]], [shared.name])
        builder.patch_linking(process, local_link)

        def build_and_run(directory):
            result = subprocess.run([make, "-f", "../makefile"], cwd=directory,
                                    capture_output=True, text=True)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            environment = {key: value for key, value in os.environ.items()
                           if not key.startswith("DYLD_")}
            result = subprocess.run([str(directory / "madevent")], cwd=directory,
                                    env=environment, capture_output=True, text=True)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertEqual(result.stdout.strip(), "6.25")

        build_and_run(subprocess_directory)
        moved = self.directory / "moved-shared-process"
        process.rename(moved)
        shutil.rmtree(project)
        shutil.rmtree(self.directory / "spinas-dependency")
        subprocess_directory = moved / "SubProcesses" / "P0"
        (subprocess_directory / "main.o").unlink()
        (subprocess_directory / "madevent").unlink()
        build_and_run(subprocess_directory)

    def test_bundled_make_links_are_relative_to_subprocess_directories(self):
        process = self.directory / "bundle"
        (process / "SubProcesses").mkdir(parents=True)
        (process / "Source").mkdir()
        (process / "Source" / "make_opts").write_text("MACFLAG=-mmacosx-version-min=10.8\n")
        makefile = process / "SubProcesses" / "makefile"
        makefile.write_text("LINKLIBS = -lmodel\n")
        builder.patch_linking(process, {"Libraries": ["lib/hepcat/libfixture.a"],
            "CXXRuntime": "c++", "RuntimeLibraryDirectories": [],
            "RelativeTo": "MadEventDirectory"})
        text = makefile.read_text()
        self.assertIn("../../lib/hepcat/libfixture.a", text)
        self.assertNotIn(str(self.directory), text)
        self.assertIn("$(PROG):", text)

    def test_native_bundle_guard_uses_only_local_files_after_export_removal(self):
        process, report = self.make_owned_run(self.directory)
        builder.install_runtime_guard(process)
        shutil.rmtree(Path(self.config["ProcessDirectory"]))
        shutil.rmtree(self.directory / "spinas-dependency")
        path = process / "bin" / "internal" / "hepcat_madevent_guard.py"
        spec = importlib.util.spec_from_file_location("hepcat_portable_guard_fixture", path)
        guard = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(guard)
        class Interface:
            InvalidCmd = RuntimeError
            me_dir = str(process)
            run_card = None
        interface = Interface()
        guard.check(interface)
        library = process / report["Bundle"]["Libraries"][0]
        library.write_bytes(b"changed numerical amplitude archive\n")
        with self.assertRaises(RuntimeError):
            guard.check(interface)

    def test_native_error_exit_zero_does_not_become_success(self):
        cases = [("missing-events", ["--run", "run_01"], "missing", 1),
                 ("old-events", ["--run", "run_01"], "old", 1),
                 ("fresh-events", ["--run", "run_01"], "fresh", 0),
                 ("native-debug", ["--run", "run_01"], "debug", 1),
                 ("command-debug", ["--command-file", "commands.txt"], "debug", 1),
                 ("fresh-events-error-marker", ["--run", "run_01"], "marker", 1),
                 ("interactive", [], "missing", 0),
                 ("native-status", ["--run", "run_01"], "status", 7),
                 ("prepare-only", ["--prepare"], "missing", 0)]
        for label, arguments, behavior, expected in cases:
            with self.subTest(label=label):
                project = self.directory / label
                process = project / "madevent"
                (process / "Cards").mkdir(parents=True)
                config_path = project / "configuration.json"
                config_path.write_text("{}")
                event = process / "Events" / "run_01" / "unweighted_events.lhe"
                if behavior == "old":
                    event.parent.mkdir(parents=True)
                    event.write_text("old events")
                    os.utime(event, ns=(100, 100))
                def run_native(*args, **kwargs):
                    if behavior in ("fresh", "marker"):
                        event.parent.mkdir(parents=True)
                        event.write_text("fresh events")
                        os.utime(event, ns=(20000, 20000))
                        if behavior == "marker":
                            (process / "hepcat-native-error.json").write_text("{}")
                    elif behavior == "debug":
                        (process / "run_01_debug.log").write_text("native caught exception")
                    return 7 if behavior == "status" else 0
                error_output = io.StringIO()
                with mock.patch.object(builder, "prepare", return_value={"MadEventDirectory": str(process)}), \
                     mock.patch.object(builder.subprocess, "call", side_effect=run_native) as call, \
                     mock.patch.object(builder.time, "time_ns", return_value=10000), \
                     contextlib.redirect_stdout(io.StringIO()), \
                     contextlib.redirect_stderr(error_output):
                    actual = builder.main(["--configuration", str(config_path)] + arguments)
                self.assertEqual(actual, expected, error_output.getvalue())
                self.assertEqual(call.call_count, 0 if behavior == "missing" and arguments == ["--prepare"] else 1)
                if expected == 1:
                    self.assertIn("HEPCAT MadEvent", error_output.getvalue())

    def test_unknown_helicity_pruning_layout_cannot_drop_massive_spin_states(self):
        body = "      REAL*8 FUNCTION MATRIX(P,NHEL,IC,IVEC)\n      END\n"
        prefix = ("      NTRY=NTRY+1\n"
                  "      IF (GOODHEL(I) .OR. NTRY .LE. MAXTRIES.OR.(ISUM_HEL.NE.0))\n"
                  "     $ THEN\n      ENDIF\n")
        hooked = builder.matrix_hook(prefix + body, self.config, ["MDL_EE", "MDL_ME"], [1])
        self.assertIn("IF (.TRUE.) THEN", hooked)
        self.assertIn("HEPCAT MadEvent requires nhel=0", hooked)
        for bad in [prefix.replace("NTRY=NTRY+1", "NTRY=1"),
                    prefix.replace("MAXTRIES", "UNKNOWN_LIMIT"), prefix + prefix]:
            with self.subTest(prefix=bad):
                with self.assertRaisesRegex(builder.MadEventSetupError, "pruning"):
                    builder.matrix_hook(bad + body, self.config, ["MDL_EE", "MDL_ME"], [1])

    def test_compiled_hook_uses_live_parameters_and_correct_normalization(self):
        compiler = shutil.which("gfortran")
        if compiler is None:
            for path in ["/opt/homebrew/bin/gfortran", "/usr/local/bin/gfortran"]:
                if Path(path).is_file():
                    compiler = path
                    break
        if compiler is None:
            self.skipTest("gfortran is unavailable; no compiled Fortran evidence")
        subprocess_dir = self.directory / "SubProcesses" / "Ptest"
        model = self.directory / "Source" / "MODEL"
        subprocess_dir.mkdir(parents=True)
        model.mkdir(parents=True)
        (self.directory / "Source" / "vector.inc").write_text("")
        (subprocess_dir / "nexternal.inc").write_text("      INTEGER NEXTERNAL\n      PARAMETER (NEXTERNAL=4)\n")
        (subprocess_dir / "maxamps.inc").write_text("      INTEGER MAXAMPS,MAXFLOW\n      PARAMETER (MAXAMPS=3,MAXFLOW=1)\n")
        (subprocess_dir / "coupl.inc").write_text("      DOUBLE PRECISION MDL_ME\n      COMMON/TEST_MASS/MDL_ME\n")
        (model / "input.inc").write_text("      DOUBLE PRECISION MDL_EE\n      COMMON/TEST_CHARGE/MDL_EE\n")
        config = dict(self.config, InitialSpinAverage=2, _MGInitialSpinDivisor=4,
                      _MassiveLegs=[2, 3], _FixedIncomingLegs=[1])
        source = "      REAL*8 FUNCTION MATRIX(P,NHEL,IC,IVEC)\n      END\n"
        matrix = subprocess_dir / "matrix.f"
        matrix.write_text(builder.matrix_hook(source, config, ["MDL_EE", "MDL_ME"], [1, 3]))
        driver = subprocess_dir / "driver.f90"
        driver.write_text('''program test_hook
  implicit none
  double precision p(0:3,4), amp2(3), jamp2(0:1), pol(2), mdl_ee, mdl_me, answer, matrix
  integer h(4), ic(4), i
  common /to_amps/ amp2,jamp2
  common /to_polarization/ pol
  common /test_charge/ mdl_ee
  common /test_mass/ mdl_me
  p=0; h=1; ic=1; pol=1; amp2=0; jamp2=0
  do i=1,4
    p(0,i)=i
  enddo
  amp2(2)=7
  mdl_ee=.3d0; mdl_me=.5d0
  answer=matrix(p,h,ic,1)
  if (abs(answer-20.68d0)>1d-12) stop 11
  if (abs(amp2(1)-answer)>1d-12 .or. abs(amp2(3)-answer)>1d-12) stop 12
  if (amp2(2)/=7 .or. abs(jamp2(1)-answer)>1d-12) stop 13
  mdl_ee=.6d0; mdl_me=1d0
  answer=matrix(p,h,ic,1)
  if (abs(answer-22.72d0)>1d-12) stop 14
  if (abs(amp2(1)-43.4d0)>1d-12 .or. abs(amp2(3)-43.4d0)>1d-12) stop 15
  print *, 'COMPILED_MADEVENT_HOOK_PASS'
end program
integer(c_int) function numerical_matrix(p,h,param,answer) bind(C,name="hepcat_madevent_matrix")
  use iso_c_binding
  implicit none
  real(c_double), intent(in) :: p(*),param(*)
  integer(c_int), intent(in) :: h(*)
  real(c_double), intent(out) :: answer
  answer=param(1)**2+param(2)**2+p(1)+p(5)+p(9)+p(13)
  numerical_matrix=0
end function
function numerical_error() bind(C,name="hepcat_madevent_error") result(ptr)
  use iso_c_binding
  implicit none
  type(c_ptr) ptr
  ptr=c_null_ptr
end function
''')
        executable = subprocess_dir / "test-hook"
        built = subprocess.run([compiler, "-ffixed-line-length-none", str(matrix), str(driver), "-o", str(executable)],
                               cwd=subprocess_dir, capture_output=True, text=True, timeout=30)
        self.assertEqual(built.returncode, 0, built.stdout + built.stderr)
        run = subprocess.run([str(executable)], cwd=subprocess_dir, capture_output=True, text=True, timeout=10)
        self.assertEqual(run.returncode, 0, run.stdout + run.stderr)
        self.assertIn("COMPILED_MADEVENT_HOOK_PASS", run.stdout)


if __name__ == "__main__":
    unittest.main()
