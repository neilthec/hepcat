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
        self.directory = Path(self.temporary.name)
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
        self.temporary.cleanup()

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
        project = self.directory / "export"
        (project / "build").mkdir(parents=True)
        library = project / "build" / "libfixture.a"
        library.touch()
        (project / "build" / "madevent-link.json").write_text(json.dumps({
            "Libraries": [str(library)], "CXXRuntime": "c++"}))
        process = project / "madevent"
        process.mkdir()
        with self.assertRaisesRegex(builder.MadEventSetupError, "ownership"):
            builder.prepare(self.config)
        normalized = builder.validate_config(self.config)
        matrix = process / "matrix.f"
        matrix.write_text("SPINAS numerical hook\n")
        (process / "bin" / "internal").mkdir(parents=True)
        (process / "bin" / "madevent").write_text("import internal.madevent_interface as cmd_interface\n")
        report = {"ConfigurationSHA256": builder.configuration_fingerprint(normalized, builder.load_link_configuration(normalized)),
                  "MatrixHashes": [{"Path": "matrix.f", "SHA256": hashlib.sha256(matrix.read_bytes()).hexdigest()}]}
        (process / "hepcat-madevent.json").write_text(json.dumps(report))
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
        matrix.write_text("regenerated HELAS matrix\n")
        with self.assertRaisesRegex(builder.MadEventSetupError, "modified or regenerated"):
            builder.prepare(self.config)

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
