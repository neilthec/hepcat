"""Lightweight MadGraph-builder checks; no MadGraph installation is required."""
import importlib.util
import math
from pathlib import Path
import sys
import tempfile
import time
from types import SimpleNamespace
import unittest

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

    def test_explicit_or_matched_used_widths_are_preserved(self):
        external = {"wz": SimpleNamespace(name="mdl_WZ", lhablock="DECAY", lhacode=[23])}
        for value in [0, 2.5]:
            updates, zeroed, preserved = builder.bare_tree_widths(external, {"wz": value})
            self.assertEqual(updates["wz"], value)
            self.assertEqual(zeroed, [])
            self.assertEqual(preserved, [{"Name": "WZ", "Value": value}])

    def test_unmatched_used_dependent_width_is_not_silently_zeroed(self):
        model = {"particles": [{"width": "mdl_WX"}]}
        with self.assertRaisesRegex(builder.ValidationSetupError, "dependent/non-DECAY"):
            builder.check_other_widths(model, {}, {"wx": 2}, {}, "call wave(mdl_WX)")
        self.assertEqual(builder.check_other_widths(model, {}, {"wx": 0}, {}, "call wave(mdl_WX)"), [])
        self.assertEqual(builder.check_other_widths(model, {}, {"wx": 2}, {}, "unrelated subprocess"), [])
        self.assertEqual(builder.check_other_widths(model, {}, {"wx": 2}, {"width": "wx"},
                         "call wave(mdl_WX)"), [{"Name": "WX", "Value": 2, "Dependent": True}])

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
