"""Lightweight MadGraph-builder checks; no MadGraph installation is required."""
import importlib.util
import hashlib
import json
import math
from pathlib import Path
import sys
import subprocess
import tempfile
import time
from types import SimpleNamespace
import unittest
from unittest.mock import patch

sys.dont_write_bytecode = True
_HELPER = Path(__file__).resolve().parents[2] / "source" / "madgraph-runtime.py"
_SPEC = importlib.util.spec_from_file_location("hepcat_madgraph_builder", _HELPER)
builder = importlib.util.module_from_spec(_SPEC)
_SPEC.loader.exec_module(builder)


class BuilderTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="hepcat-mg-builder-unit-")
        self.directory = Path(self.temporary.name)
        (self.directory / "bin").mkdir()
        (self.directory / "models").mkdir()
        (self.directory / "bin" / "mg5_aMC").touch()
        (self.directory / "models" / "import_ufo.py").touch()
        self.config = {
            "MadGraphDirectory": str(self.directory),
            "MadGraphModel": "explicit-reference-model",
            "PhysicalPDGs": [11, -11, 22, 22],
            "Masses": [.0005, .0005, 0, 0],
            "Parameters": [{"Name": "Me", "Value": .0005}, {"Name": "EE", "Value": .31333}],
        }
        external = [
            SimpleNamespace(name="mdl_Me", lhablock="MASS", lhacode=[11]),
            SimpleNamespace(name="aEWM1", lhablock="SMINPUTS", lhacode=[1]),
            SimpleNamespace(name="mdl_yme", lhablock="YUKAWA", lhacode=[11]),
        ]
        self.model = {"name": "sm-full", "parameters": {
            ("external",): external,
            (): [SimpleNamespace(name="mdl_ee")],
        }}

    def tearDown(self):
        self.temporary.cleanup()

    def test_reference_imports_do_not_write_python_caches(self):
        self.assertTrue(builder.sys.dont_write_bytecode)

    def test_short_card_fields_preserve_values(self):
        for value in [0, 1, -1, .0005, -.25, 1e-300, -1.234567890123456e-300,
                      1.7976931348623157e308]:
            with self.subTest(value=value):
                text = builder.safe_card_number(value)
                self.assertLessEqual(len(text), 20)
                self.assertTrue(builder.close_value(float(text), value))

    def test_finite_real_inputs_only(self):
        for value in [True, False, "1", None, math.inf, -math.inf, math.nan, 1j]:
            with self.subTest(value=value):
                with self.assertRaises(builder.ValidationSetupError):
                    builder.real_number(value, "parameter")

    def test_no_hidden_absolute_matching_floor(self):
        self.assertTrue(builder.close_value(0, 0))
        self.assertFalse(builder.close_value(0, 1e-250))
        self.assertFalse(builder.close_value(.0005, 0))
        self.assertFalse(builder.close_value(.0005, .0006))

    def test_valid_configuration_preserves_real_masses(self):
        result = builder.checked_configuration(self.config)
        self.assertEqual(result[1], [11, -11, 22, 22])
        self.assertEqual(result[2], [.0005, .0005, 0, 0])
        self.assertEqual(result[3]["ee"], ("EE", .31333))

    def test_jamp_split_is_explicit_bounded_and_defaults_to_native(self):
        for value, expected in [(None, 0), (0, 0), (1, 1), (128, 128), (4096, 4096)]:
            with self.subTest(value=value):
                self.assertEqual(builder.jamp_split_setting(value), expected)
                builder.checked_configuration(dict(self.config, JampSplit=value))
        for value in [False, True, -1, 1.5, "128", [], {}, 4097]:
            with self.subTest(value=value):
                with self.assertRaisesRegex(builder.ValidationSetupError, "JampSplit"):
                    builder.checked_configuration(dict(self.config, JampSplit=value))

    def test_large_reference_automatically_uses_native_split_when_supported(self):
        native = SimpleNamespace(get_JAMP_lines=lambda amplitudes, split=-1: None)
        exporter = SimpleNamespace(ProcessExporterFortran=native)
        with patch.dict(sys.modules, {"madgraph.iolibs.export_v4": exporter}):
            self.assertEqual(builder.reference_jamp_split(self.config, 9), 0)
            self.assertEqual(builder.reference_jamp_split(self.config, 10), 128)
            for override, expected in [(None, 0), (0, 0), (64, 64)]:
                with self.subTest(override=override):
                    self.assertEqual(
                        builder.reference_jamp_split(dict(self.config, JampSplit=override), 10), expected)
            native.get_JAMP_lines = lambda amplitudes: None
            self.assertEqual(builder.reference_jamp_split(self.config, 10), 0)

    def test_unselected_jamp_split_keeps_native_cli_and_writes_no_bootstrap(self):
        output = self.directory / "output"
        output.mkdir()
        command_file = output / "generate.mg5"
        for value in [0, None]:
            command, metadata = builder.madgraph_generation_command(
                self.directory, output, command_file, value)
            self.assertEqual(command, [sys.executable, self.directory / "bin" / "mg5_aMC", "-s", command_file])
            self.assertEqual(metadata, {"JampSplit": 0, "JampAccumulationPolicy": "NativeDefault", "JampBootstrap": None})
        self.assertFalse((output / "native-jamp-bootstrap.py").exists())

    def test_jamp_bootstrap_supplies_only_missing_or_nonpositive_native_split(self):
        package = self.directory / "madgraph" / "iolibs"
        package.mkdir(parents=True)
        (package.parent / "__init__.py").touch()
        (package / "__init__.py").touch()
        native_source = """class ProcessExporterFortran:
    def get_JAMP_lines(self, amplitudes, JAMP_format='JAMP(%s)', AMP_format='AMP(%s)', split=-1):
        return [amplitudes, JAMP_format, AMP_format, split]
"""
        (package / "export_v4.py").write_text(native_source)
        output = self.directory / "output"
        output.mkdir()
        result_file = output / "calls.json"
        script = """import json
from pathlib import Path
import sys
from madgraph.iolibs.export_v4 import ProcessExporterFortran
native = ProcessExporterFortran()
calls = [native.get_JAMP_lines([1,2]),
         native.get_JAMP_lines([3], split=0),
         native.get_JAMP_lines([4], split=7),
         native.get_JAMP_lines([5], 'J(%s)', 'A(%s)', 9)]
Path(sys.argv[-1]).write_text(json.dumps({'Calls': calls, 'Debug': __debug__}))
"""
        executable = self.directory / "bin" / "mg5_aMC"
        executable.write_text(script)
        command, metadata = builder.madgraph_generation_command(
            self.directory, output, result_file, 128)
        subprocess.run([str(item) for item in command], check=True, capture_output=True, text=True)
        result = json.loads(result_file.read_text())
        self.assertFalse(result["Debug"])
        self.assertEqual(result["Calls"], [
            [[1, 2], "JAMP(%s)", "AMP(%s)", 128],
            [[3], "JAMP(%s)", "AMP(%s)", 128],
            [[4], "JAMP(%s)", "AMP(%s)", 7],
            [[5], "J(%s)", "A(%s)", 9]])
        self.assertEqual((package / "export_v4.py").read_text(), native_source)
        self.assertEqual(executable.read_text(), script)
        self.assertFalse(list((self.directory / "madgraph").rglob("__pycache__")))
        self.assertEqual(metadata["JampSplit"], 128)
        bootstrap = metadata["JampBootstrap"]
        self.assertEqual(bootstrap["PythonFlags"], ["-O"])
        self.assertEqual(bootstrap["Source"], Path(bootstrap["Path"]).read_text())
        self.assertEqual(bootstrap["SHA256"], hashlib.sha256(bootstrap["Source"].encode()).hexdigest())

    def test_reference_installation_and_model_are_always_explicit(self):
        for key in ("MadGraphDirectory", "MadGraphModel"):
            for value in (None, "", "  ", [], False, "bad\nsetting"):
                with self.subTest(key=key, value=value):
                    config = dict(self.config)
                    if value is None:
                        config.pop(key)
                    else:
                        config[key] = value
                    with self.assertRaisesRegex(builder.ValidationSetupError, key):
                        builder.checked_configuration(config)

    def test_case_normalized_rule_collisions_are_refused(self):
        for extra in ({"ParameterRules": {"Me": 1, "me": 2}},
                      {"ParameterMap": {"Me": "Me", "me": "other"}}):
            with self.subTest(extra=extra):
                with self.assertRaisesRegex(builder.ValidationSetupError, "case-insensitive"):
                    builder.checked_configuration(dict(self.config, **extra))

    def test_unused_parameter_mapping_is_refused(self):
        with self.assertRaisesRegex(builder.ValidationSetupError, "not used"):
            builder.checked_configuration(dict(self.config, ParameterMap={"unused": "ee"}))

    def test_mass_metadata_is_required(self):
        for masses in [None, [], [.0005], [.0005, .0005, -1, 0], [.0005, .0005, math.inf, 0]]:
            with self.subTest(masses=masses):
                config = dict(self.config, Masses=masses)
                with self.assertRaises(builder.ValidationSetupError):
                    builder.checked_configuration(config)

    def test_pdgs_must_be_nonzero_integers(self):
        for pdgs in [[11, -11], [11, -11, 0, 22], [11, -11, "22", 22], [11, -11, True, 22]]:
            with self.subTest(pdgs=pdgs):
                with self.assertRaises(builder.ValidationSetupError):
                    builder.checked_configuration(dict(self.config, PhysicalPDGs=pdgs))

    def test_duplicate_used_parameter_names_are_refused(self):
        config = dict(self.config, Parameters=[{"Name": "Me", "Value": 1}, {"Name": "me", "Value": 2}])
        with self.assertRaisesRegex(builder.ValidationSetupError, "Repeated"):
            builder.checked_configuration(config)

    def test_invalid_parameter_keys_and_rules_are_refused(self):
        for extra in [{"ParameterRules": []}, {"ParameterMap": []},
                      {"ParameterRules": {"bad;quit": 1}}, {"ParameterMap": {"EE": "bad\nname"}},
                      {"Parameters": [{"Name": "EE", "Value": math.nan}]}]:
            with self.subTest(extra=extra):
                with self.assertRaises(builder.ValidationSetupError):
                    builder.checked_configuration(dict(self.config, **extra))

    def test_explicit_map_matches_differently_named_coupling(self):
        used = {"me": ("Me", .0005), "charge": ("charge", .31333)}
        unused, external, updates, targets, matches = builder.match_parameters(
            self.model, used, {}, {"charge": "ee"})
        self.assertEqual(targets["charge"], "ee")
        self.assertAlmostEqual(updates["aewm1"], 4 * math.pi / .31333 ** 2)
        self.assertEqual(updates["yme"], .0005)
        self.assertEqual(matches[1]["MadGraph"], "ee")

    def test_unmapped_relevant_coupling_is_not_silently_defaulted(self):
        with self.assertRaisesRegex(builder.ValidationSetupError, "No verified"):
            builder.match_parameters(self.model, {"charge": ("charge", .31333)}, {}, {})

    def test_mass_roles_match_different_names_without_mass_aliases(self):
        model = {"name": "custom-mass-model", "parameters": {
            ("external",): [SimpleNamespace(name="heavyPole", lhablock="MASS", lhacode=[6]),
                             SimpleNamespace(name="tauMass", lhablock="MASS", lhacode=[15])],
            (): []}, "particle_dict": {6: {"mass": "heavyPole"}, 15: {"mass": "tauMass"}}}
        used = {"mtp": ("Mtp", 172.5), "ml": ("Ml", 1.777)}
        class NativeModel(dict):
            # MadGraph's Model.get accepts one argument, unlike dict.get.
            def get(self, key):
                return super().get(key)
        model = NativeModel(model)
        unused, external, updates, targets, matches = builder.match_parameters(
            model, used, {}, {}, {"mtp": [6], "ml": [15]})
        self.assertEqual(targets, {"mtp": "heavypole", "ml": "taumass"})
        self.assertEqual(updates, {"heavypole": 172.5, "taumass": 1.777})
        self.assertEqual(matches[0]["Role"], "ParticleMass")
        self.assertEqual(matches[0]["PDGs"], [6])

    def test_mass_roles_do_not_override_an_explicit_parameter_map(self):
        model = dict(self.model, particle_dict={11: {"mass": "notDeclared"}})
        unused, external, updates, targets, matches = builder.match_parameters(
            model, {"unusualmass": ("unusualMass", .0005)}, {}, {"unusualmass": "me"},
            {"unusualmass": [11]})
        self.assertEqual(targets["unusualmass"], "me")

    def test_mass_roles_reject_incompatible_zero_mass_and_ambiguous_targets(self):
        for particles, roles in [({6: {"mass": "ZERO"}}, [6]),
                                 ({6: {"mass": "Me"}, 15: {"mass": "ee"}}, [6, 15]),
                                 ({}, [6])]:
            with self.subTest(particles=particles):
                with self.assertRaises(builder.ValidationSetupError):
                    builder.match_parameters(dict(self.model, particle_dict=particles),
                        {"unknownmass": ("unknownMass", 1.0)}, {}, {}, {"unknownmass": roles})

    def test_mass_role_metadata_requires_typed_positive_pdgs(self):
        self.assertEqual(builder.checked_mass_parameter_roles({"Mtp": [6, 6]}), {"mtp": [6]})
        for roles in ([], {"M": []}, {"M": [True]}, {"M": [-6]},
                      {"M": [1.5]}, {"M": [6], "m": [15]}):
            with self.subTest(roles=roles):
                with self.assertRaises(builder.ValidationSetupError):
                    builder.checked_mass_parameter_roles(roles)

    def test_conflicting_external_override_is_refused(self):
        with self.assertRaisesRegex(builder.ValidationSetupError, "conflicts"):
            builder.match_parameters(self.model, {"me": ("Me", .0005)}, {"me": 1}, {})

    def test_only_external_mg_inputs_can_be_overridden(self):
        for key in ["unknown", "ee"]:
            with self.subTest(key=key):
                with self.assertRaisesRegex(builder.ValidationSetupError, "external UFO"):
                    builder.match_parameters(self.model, {}, {key: 1}, {})

    def test_explicit_sm_yukawa_input_is_preserved(self):
        unused, external, updates, targets, matches = builder.match_parameters(
            self.model, {"me": ("Me", .0005)}, {"yme": .001}, {})
        self.assertEqual(updates["yme"], .001)

    def test_complete_tree_bounds_scale_with_multiplicity(self):
        model = {"interactions": [
            {"type": "base", "particles": [1, 2, 3], "orders": {"QED": 1, "NP": 2}},
            {"type": "base", "particles": [1, 2, 3, 4], "orders": {"QED": 2}},
        ]}
        for n in [4, 5, 6, 10]:
            self.assertEqual(builder.complete_tree_orders(model, n), {"QED": 2 * (n - 2), "NP": 2 * (n - 2)})

    def test_counterterm_vertices_do_not_restrict_tree_generation(self):
        model = {"interactions": [
            {"type": "UV", "particles": [1, 2], "orders": {"QED": 2}},
            {"type": "base", "particles": [1, 2, 3], "orders": {"QED": 1}},
        ]}
        self.assertEqual(builder.complete_tree_orders(model, 5), {"QED": 3})

    def test_two_point_born_vertices_are_not_given_an_incorrect_bound(self):
        model = {"interactions": [{"type": "base", "particles": [1, 2], "orders": {"QED": 1}}]}
        with self.assertRaisesRegex(builder.ValidationSetupError, "two-point"):
            builder.complete_tree_orders(model, 5)

    def test_direct_mass_input_is_matched_even_without_a_used_mass_symbol(self):
        model = dict(self.model, particle_dict={11: {"mass": "mdl_Me"}, -11: {"mass": "mdl_Me"}})
        unused, external = builder.parameter_index(model)
        updates, matches = builder.match_external_masses(model, [11, -11], [.001, .001], external, {})
        self.assertEqual(updates["me"], .001)
        self.assertTrue(all(row["DirectInput"] for row in matches))
        updates = builder.synchronize_sm_yukawa(model, external, updates, {})
        self.assertEqual(updates["yme"], .001)

    def test_mass_metadata_cannot_override_a_conflicting_parameter(self):
        model = dict(self.model, particle_dict={11: {"mass": "mdl_Me"}})
        unused, external = builder.parameter_index(model)
        with self.assertRaisesRegex(builder.ValidationSetupError, "conflicts"):
            builder.match_external_masses(model, [11], [.001], external, {"me": .0005})

    def test_unmatched_decay_widths_default_to_zero(self):
        external = {"wz": SimpleNamespace(name="mdl_WZ", lhablock="DECAY", lhacode=[23])}
        updates, zeroed, preserved = builder.bare_tree_widths(external, {})
        self.assertEqual(updates["wz"], 0)
        self.assertEqual(zeroed, [{"Name": "WZ", "Value": 0}])
        self.assertEqual(preserved, [])

    def test_explicit_or_matched_used_widths_are_always_zero_for_validation(self):
        external = {"wz": SimpleNamespace(name="mdl_WZ", lhablock="DECAY", lhacode=[23])}
        for value in [0, 2.5]:
            updates, zeroed, preserved = builder.bare_tree_widths(external, {"wz": value})
            self.assertEqual(updates["wz"], 0)
            self.assertEqual(zeroed, [{"Name": "WZ", "Value": 0}])
            self.assertEqual(preserved, [])

    def test_all_particle_widths_are_checked_even_matched_or_unused_ones(self):
        model = {"particles": [{"width": "mdl_WX"}]}
        for targets in [{}, {"width": "wx"}]:
            for matrix in ["call wave(mdl_WX)", "unrelated subprocess"]:
                with self.subTest(targets=targets, matrix=matrix):
                    with self.assertRaisesRegex(builder.ValidationSetupError, "zero particle width"):
                        builder.check_other_widths(model, {}, {"wx": 2}, targets, matrix)
                    self.assertEqual(builder.check_other_widths(model, {}, {"wx": 0}, targets, matrix), [])

    def width_model(self):
        external = [SimpleNamespace(name="mdl_WZ", lhablock="DECAY", lhacode=[23], value=2.5),
                    SimpleNamespace(name="mdl_WX", lhablock="CUSTOM", lhacode=[7], value=3),
                    SimpleNamespace(name="GG", lhablock="COUPLINGS", lhacode=[1], value=.3)]
        dependent = SimpleNamespace(name="mdl_WY", value=7, expr="GG*23")
        return {"parameters": {("external",): external, (): [dependent]},
                "particles": [{"width": name} for name in ["mdl_WZ", "mdl_WX", "mdl_WY", "ZERO"]]}

    def test_particle_roles_cover_non_decay_and_dependent_widths(self):
        model = self.width_model()
        used = {"width": ("Width", 7), "gg": ("GG", .3)}
        rules = {"wz": 5, "wx": 6, "wy": 7, "gg": .3}
        zero_used, zero_rules, widths = builder.validation_zero_width_inputs(
            model, used, rules, {"width": "wy"})
        self.assertEqual(set(widths), {"wz", "wx", "wy"})
        self.assertEqual(zero_used["width"], ("Width", 0))
        self.assertEqual(zero_used["gg"], ("GG", .3))
        self.assertEqual(zero_rules, {"wz": 0, "wx": 0, "gg": .3})
        self.assertEqual(widths["wy"].expr, "0.0")
        self.assertTrue(all(parameter.value == 0 for parameter in widths.values()))
        entries, external = builder.parameter_index(model)
        self.assertEqual(entries["gg"].value, .3)
        updates, zeroed, preserved = builder.bare_tree_widths(external, {"wz": 9, "gg": .3}, widths)
        self.assertEqual(updates, {"wz": 0, "wx": 0, "gg": .3})
        self.assertEqual(preserved, [])
        self.assertIn({"Name": "WY", "Value": 0, "Dependent": True}, zeroed)
        # The explicit dictionaries supplied by the caller are not rewritten.
        self.assertEqual(used["width"], ("Width", 7))
        self.assertEqual(rules["wy"], 7)

    def test_declared_width_without_parameter_fails_setup(self):
        model = self.width_model()
        model["particles"].append({"width": "missingWidth"})
        with self.assertRaisesRegex(builder.ValidationSetupError, "no UFO parameter"):
            builder.validation_width_parameters(model)

    def ufo_fixture(self):
        directory = self.directory / "ufo"
        directory.mkdir()
        text = ("from object_library import Parameter as UFOParameter\n"
                "# Keep this comment and the unrelated coupling exactly.\n"
                "WZ = UFOParameter(name='WZ', nature='external', value=2.5, texname='W_Z', lhablock='DECAY', lhacode=[23])\n"
                "WX = UFOParameter(name='WX', nature='external', value=3.0, texname='W_X', lhablock='CUSTOM', lhacode=[7])\n"
                "WY = UFOParameter(name='WY', texname='Γ_Y', nature='internal', value='GG*23')\n"
                "GG = UFOParameter(name='GG', nature='external', value=0.3, texname='g')\n")
        (directory / "parameters.py").write_text(text)
        return directory, text

    def test_validation_private_ufo_zeroes_only_width_values(self):
        source, original = self.ufo_fixture()
        output = self.directory / "validation"
        output.mkdir()
        restriction = self.directory / "restriction.dat"
        restriction.write_text("# original restriction\n")
        result = builder.private_ufo_copy(source, restriction, output, ["mdl_WZ", "WX", "WY"])
        self.assertEqual(result, "./reference-model-hepcat")
        private = (output / "reference-model" / "parameters.py").read_text()
        self.assertEqual(private, original.replace("value=2.5", "value=0.0").replace(
            "value=3.0", "value=0.0").replace("value='GG*23'", "value='0.0'"))
        self.assertEqual((source / "parameters.py").read_text(), original)
        self.assertEqual((output / "reference-model" / "restrict_hepcat.dat").read_bytes(), restriction.read_bytes())

    def test_private_restriction_cannot_restore_a_width(self):
        source, original = self.ufo_fixture()
        output = self.directory / "validation-restricted"
        output.mkdir()
        restriction = self.directory / "restricted.dat"
        card = ("DECAY 23 2.5 # declared width\n"
                "DECAY 25 8.0 # another unchanged input\n"
                "BLOCK CUSTOM\n  7 3.0 # declared non-DECAY width\n  8 4.0 # coupling\n"
                "BLOCK MASS\n  23 91.0 # mass\n")
        restriction.write_text(card)
        builder.private_ufo_copy(source, restriction, output, ["WZ", "WX", "WY"])
        private = (output / "reference-model" / "restrict_hepcat.dat").read_text()
        self.assertEqual(private, card.replace("23 2.5", "23 0.0").replace("7 3.0", "7 0.0"))
        self.assertEqual(restriction.read_text(), card)
        self.assertEqual((source / "parameters.py").read_text(), original)

    def test_normal_madevent_private_copy_keeps_widths_unchanged(self):
        source, original = self.ufo_fixture()
        restriction = self.directory / "normal-restriction.dat"
        card = "DECAY 23 2.5\nBLOCK CUSTOM\n 7 3.0\n"
        restriction.write_text(card)
        for index, selected in enumerate([None, restriction]):
            output = self.directory / ("madevent-" + str(index))
            output.mkdir()
            result = builder.private_ufo_copy(source, selected, output)
            self.assertEqual(result, "./reference-model-hepcat" if selected else "./reference-model-full")
            self.assertEqual((output / "reference-model" / "parameters.py").read_text(), original)
            self.assertEqual((source / "parameters.py").read_text(), original)
            if selected:
                self.assertEqual((output / "reference-model" / "restrict_hepcat.dat").read_text(), card)

    def test_unrewritable_or_missing_private_width_declarations_fail(self):
        for body in ["WX = make_parameter(name='WX', nature='internal', value='2*G')\n",
                     "WX = Parameter(name='WX', nature='internal', **other_arguments)\n",
                     "WX = Parameter(name=dynamic_name, nature='internal', value='2*G')\n"]:
            with self.subTest(body=body):
                (self.directory / "parameters.py").write_text(body)
                with self.assertRaisesRegex(builder.ValidationSetupError, "rewrite"):
                    builder.zero_private_ufo_widths(self.directory, ["WX"])
        (self.directory / "parameters.py").unlink()
        with self.assertRaisesRegex(builder.ValidationSetupError, "parameters.py is missing"):
            builder.zero_private_ufo_widths(self.directory, ["WX"])

    def test_release_provenance_comes_from_the_selected_installation(self):
        (self.directory / "VERSION").write_text("version = 3.8.1\ndate = 2026-10-09\n")
        self.assertEqual(builder.madgraph_version(self.directory), {"Version": "3.8.1", "Date": "2026-10-09"})

    def test_allowed_particle_types_use_absolute_species(self):
        self.assertEqual(builder.allowed_particle_types([22, -11, 11], [11, -11, 22, 22]), [11, 22])
        self.assertEqual(builder.allowed_particle_types([], [11, -11, 22, 22]), [])

    def test_allowed_particle_types_must_include_the_external_process(self):
        with self.assertRaisesRegex(builder.ValidationSetupError, "omits an external"):
            builder.checked_configuration(dict(self.config, AllowedParticlePDGs=[22]))
        for setting in [None, "QED", [True, 22], [0, 11, 22], [11.0, 22]]:
            with self.subTest(setting=setting):
                with self.assertRaises(builder.ValidationSetupError):
                    builder.checked_configuration(dict(self.config, AllowedParticlePDGs=setting))

    def test_exclusions_include_particles_and_antiparticles(self):
        class Particle:
            def __init__(self, name):
                self.name = name
            def get_name(self):
                return self.name
        model = {"particle_dict": {11: Particle("e-"), -11: Particle("e+"), 22: Particle("a"),
                 23: Particle("z"), 25: Particle("h"), 13: Particle("mu-"), -13: Particle("mu+")}}
        self.assertEqual(builder.reference_particle_exclusions(model, [11, 22]), ["h", "mu+", "mu-", "z"])
        self.assertEqual(builder.reference_particle_exclusions(model, []), [])
        with self.assertRaisesRegex(builder.ValidationSetupError, "lacks a requested"):
            builder.reference_particle_exclusions(model, [11, 22, 999])

    def test_unknown_model_never_triggers_a_download(self):
        with self.assertRaisesRegex(builder.ValidationSetupError, "not found locally"):
            builder.local_model_specification(self.directory, "no_such_model")

    def test_process_time_limit_terminates_the_child(self):
        with self.assertRaisesRegex(builder.ValidationSetupError, "timed out"):
            builder.run_logged([sys.executable, "-c", "import time; time.sleep(30)"],
                               self.directory, self.directory / "timeout.log", time.monotonic() + .1)

    def test_nonzero_build_exit_is_not_a_success(self):
        with self.assertRaisesRegex(builder.ValidationSetupError, "exit 23"):
            builder.run_logged([sys.executable, "-c", "raise SystemExit(23)"],
                               self.directory, self.directory / "failure.log", time.monotonic() + 5)


if __name__ == "__main__":
    unittest.main()
