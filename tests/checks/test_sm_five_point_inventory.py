"""Fast structural safeguards for the complete five-point SM progress inventory."""
import importlib.util
import itertools
import json
from pathlib import Path
import unittest

ROOT = Path(__file__).resolve().parents[2]
INVENTORY_DIRECTORY = ROOT / "tests/SM-5-point"
spec = importlib.util.spec_from_file_location("sm_five_point_inventory", INVENTORY_DIRECTORY / "inventory.py")
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)


class SMFivePointInventoryTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.inventory = json.loads((INVENTORY_DIRECTORY / "inventory.json").read_text())
        cls.cases = {case["ID"]: case for case in cls.inventory["Cases"]}
        cls.families = {family["ID"]: family for family in cls.inventory["Families"]}

    def test_inventory_rebuilds_from_current_elementary_model(self):
        self.assertEqual(module.build_inventory(ROOT), self.inventory)

    def test_full_progress_inventory_counts(self):
        summary = self.inventory["Summary"]
        self.assertEqual((summary["TreeSupportedCases"], summary["Cases"], summary["Families"]), (1420, 1422, 869))
        self.assertEqual((summary["RepresentativeCases"], summary["RepresentativeFamilies"]), (380, 215))
        self.assertEqual(summary["CasesByFermionCount"], {"0": 33, "2": 663, "4": 726})
        self.assertEqual(len(self.cases), summary["Cases"])
        self.assertEqual(len(self.families), summary["Families"])

    def test_every_tree_topology_is_included(self):
        particles = {"x": {"Antiparticle": "x"}}
        cubic = {"Particles": ("x", "x", "x")}
        quartic = {"Particles": ("x", "x", "x", "x")}
        trees, _ = module.tree_signatures(particles, [cubic, quartic])
        self.assertEqual(trees, {("x",) * 5: {"ThreeCubics", "CubicQuartic"}})
        self.assertFalse(module.tree_signatures(particles, [quartic])[0])

    def test_each_export_has_every_labeled_helicity_assignment(self):
        for family in self.families.values():
            with self.subTest(family=family["ID"]):
                assignments = family["HelicityAssignments"]
                self.assertEqual(len(assignments), 2 ** len(family["MasslessBosonLegs"]))
                self.assertEqual(len({tuple(a["Particles"]) for a in assignments}), len(assignments))
                self.assertEqual({a["CaseID"] for a in assignments}, set(family["CaseIDs"]))
                for assignment in assignments:
                    source = self.cases[assignment["CaseID"]]
                    for index, target in enumerate(assignment["SourcePermutation"]):
                        self.assertEqual(source["Particles"][index], assignment["Particles"][target - 1])
        qed = next(f for f in self.families.values() if f["Signature"] == ["A", "A", "A", "E", "e"])
        self.assertEqual((len(qed["CaseIDs"]), len(qed["HelicityAssignments"])), (4, 8))

    def test_generation_maps_keep_equality_patterns_and_order_metadata(self):
        for certificate in self.inventory["FlavorCertificates"]:
            self.assertEqual(certificate["Status"], "EXACT_ROW_MATCH")
            self.assertEqual(certificate["ElementaryVertices"], 74)
        self.assertEqual(len(self.inventory["FlavorMaps"]), 36)
        for case in self.cases.values():
            representative = self.cases[case["RepresentativeID"]]
            self.assertTrue(representative["IsRepresentative"])
            mapping = self.inventory["FlavorMaps"][case["FlavorMap"]]["ParticleMap"]
            self.assertEqual(len(set(mapping.values())), len(mapping))
            mapped = [mapping.get(p, p) for p in representative["Particles"]]
            self.assertEqual(mapped, case["MappedRepresentativeParticles"])
            for index, target in enumerate(case["RepresentativePermutation"]):
                self.assertEqual(mapped[index], case["Particles"][target - 1])
        # A bijection can never turn a same-generation pair into two distinct flavors.
        for case in self.cases.values():
            source = self.cases[case["RepresentativeID"]]["Particles"]
            target = case["MappedRepresentativeParticles"]
            self.assertEqual(len(set(source)), len(set(target)))

    def test_structural_zero_slots_are_not_claimed_validated(self):
        zeros = [c for c in self.cases.values() if not c["HasDiagrams"]]
        self.assertEqual({tuple(c["Signature"]) for c in zeros}, {("G.+",) * 5, ("G.-",) * 5})
        for case in self.cases.values():
            self.assertNotIn("Status", case)
            self.assertNotIn("Completion", case)
            self.assertEqual(case["RequiresConstruction"], case["HasDiagrams"])

    def test_readable_export_names_are_portable_and_case_insensitive_unique(self):
        tokens = module.export_tokens(ROOT)
        names = [family["ExportName"] for family in self.families.values()]
        self.assertEqual(len(set(name.lower() for name in names)), 869)
        self.assertEqual(tokens["E"], "ebar")
        self.assertEqual(tokens["Ne"], "nebar")
        self.assertEqual(tokens["U"], "ubar")
        for family in self.families.values():
            self.assertRegex(family["ExportName"], r"^[A-Za-z][A-Za-z0-9]*$")
            self.assertEqual(family["ExportName"], "".join(tokens[p] for p in family["Particles"]))
        eehhh = next(f for f in self.families.values() if f["Particles"] == ["e", "E", "h", "h", "h"])
        self.assertEqual(eehhh["ExportName"], "eebarhhh")

    def test_physical_crossings_are_kinematically_available(self):
        anti = {p: data["Antiparticle"] for p, data in self.inventory["Particles"].items()}
        for family in self.families.values():
            self.assertEqual(family["Particles"][:2], family["Incoming"])
            self.assertEqual([anti.get(p, p) for p in family["Particles"][2:]], family["Outgoing"])
            kinematics = family["Kinematics"]
            self.assertGreater(kinematics["SuggestedSqrtSGeV"], kinematics["IncomingThresholdGeV"])
            self.assertGreater(kinematics["SuggestedSqrtSGeV"], kinematics["OutgoingThresholdGeV"])

    def test_selected_native_diagram_checks_match_current_model(self):
        evidence = json.loads((INVENTORY_DIRECTORY / "diagram-crosschecks.json").read_text())
        self.assertEqual(evidence["ModelFiles"], {name: digest for name, digest in self.inventory["Files"].items() if name.startswith("models/")})
        self.assertEqual(len(evidence["Checks"]), 16)
        self.assertTrue(all(check["Status"] == "PASS" for check in evidence["Checks"]))
        self.assertTrue(any(check["DiagramCount"] == 0 for check in evidence["Checks"]))
        self.assertTrue(any(check["DiagramCount"] > 0 for check in evidence["Checks"]))


if __name__ == "__main__":
    unittest.main()
