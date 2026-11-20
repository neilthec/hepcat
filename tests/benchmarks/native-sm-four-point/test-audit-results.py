"""Regression checks for SM validation receipt acceptance."""
import copy
import importlib.util
import json
from pathlib import Path
import tempfile
import unittest

spec = importlib.util.spec_from_file_location("sm4_audit", Path(__file__).with_name("audit-results.py"))
audit = importlib.util.module_from_spec(spec)
spec.loader.exec_module(audit)


class Receipts(unittest.TestCase):
    def test_only_commutative_operands_can_move(self):
        def key(text):
            return audit.canonical_expression(text.encode() + b"\n")
        expr = 'Times[Plus[a,a,b],SpinorChain[x,y],ModelParameter["a,b","scope","Real"]]'
        equivalent = 'Times[SpinorChain[x,y],ModelParameter["a,b","scope","Real"],Plus[b,a,a]]'
        self.assertEqual(key(expr), key(equivalent))
        for changed in (expr.replace("x,y", "y,x"), expr.replace("[Plus", "[-1,Plus"),
                        expr.replace("a,a,b", "a,b"), expr.replace("a,a,b", "a,Plus[a,b]"),
                        expr.replace('"a,b"', '"b,a"')):
            self.assertNotEqual(key(expr), key(changed))
        with self.assertRaises(ValueError):
            key("Plus[a,b")

    def test_a_summed_comparison_does_not_cover_fixed_helicities(self):
        helicities = {(-2, 0, 0, 0), (2, 0, 0, 0)}
        template = dict(Status="PASS", Passed=True, Mismatches=0, EvaluationErrors=0,
                        Points=10000, ComparedPoints=10000, RequestedPoints=10000, GeneratedPoints=10000)
        rows = [dict(template, Kind="FixedHelicity", Helicities=list(h)) for h in helicities]
        rows.append(dict(template, Kind="PolarizationSum", Helicities=[]))
        self.assertEqual(len(audit.mg_rows({"Rows": rows}, helicities, True)), 3)
        for incomplete in ([rows[-1]], rows[1:], rows + [rows[0]]):
            with self.assertRaises(ValueError):
                audit.mg_rows({"Rows": incomplete}, helicities, True)
        wrong = copy.deepcopy(rows)
        wrong[0]["ComparedPoints"] -= 1
        with self.assertRaises(ValueError):
            audit.mg_rows({"Rows": wrong}, helicities, True)

    def test_on_shell_chart_only_credits_bounded_exact_zero_replay(self):
        helicities = {(-2, -2, -2, -2), (-2, -2, 2, 2)}
        template = dict(Status="PASS", Passed=True, Mismatches=0, EvaluationErrors=0,
                        Points=10000, ComparedPoints=10000, RequestedPoints=10000, GeneratedPoints=10000)
        rows = [dict(template, Kind="FixedHelicity", Helicities=list(h), ExpectedExactZero=(h[-1] < 0))
                for h in sorted(helicities)]
        rows.append(dict(template, Kind="PolarizationSum", Helicities=[], ExpectedExactZero=False))
        previous = copy.deepcopy(rows)
        previous[0].update(Status="FAIL", Passed=False, Mismatches=1)
        report = dict(Rows=rows, UnprojectedBinary128Comparison=dict(Seed=8, Status="FAIL", Passed=False, Rows=previous),
                      MaximumChartComponentChangeOverBeamEnergy=1e-16)
        counts = audit.mg_rows(report, helicities, True)
        def run(r):
            return audit.audit_chart(r, helicities, counts, 8, dict(Masses=[0]*4), dict(MasslessTwoBodyChartLibrary="chart"))
        self.assertEqual(run(report), 1e-16)
        with self.assertRaises(ValueError):
            run(dict(report, MaximumChartComponentChangeOverBeamEnergy=1e-10))
        with self.assertRaises(ValueError):
            run(dict(report, MaximumChartComponentChangeOverBeamEnergy=float("nan")))
        previous[-1]["Mismatches"] = 2
        with self.assertRaises(ValueError):
            run(report)

    def test_absolute_complexity_and_leaf_count_must_both_pass(self):
        case = dict(notebook_weight="2", notebook_leaves="4")
        for weight, leaves in ((2, 5), (3, 4)):
            with self.assertRaisesRegex(ValueError, "complexity"):
                audit.native_case(dict(case, status="PASS", weight=weight, leaves=leaves), case, [])

    def test_calchep_precision_replay_covers_the_captured_points(self):
        with tempfile.TemporaryDirectory() as tempdir:
            folder = Path(tempdir)
            (folder / "calchep-comparison.log").write_text(
                "PROCESS: a,b->c,d\nAmplitude validation: FAIL\nSeed: 12\n"
                "Points requested/generated/compared: 10000/10000/10000\nMismatches/errors: 2/0\n")
            capture = folder / "calchep-outliers.csv"
            capture.write_text("point,hepcat,calchep\n7,1,1.1\n42,2.3,2.0\n")
            replay = dict(Status="PRECISION_REPLAY_PASS", Seed=12, Points=2, ExternalCount=4,
                          NativeControlReproducesCapture=True, PointsSHA256=audit.digest(capture),
                          Results={"Native": dict(Points=2, OriginalBitMatches=2),
                                   "GCC128": dict(Points=2, Errors=0, Mismatches=0, Status="PASS",
                                                  MantissaBits=113, REALBytes=16)})
            path = folder / "calchep-precision.json"
            path.write_text(json.dumps(replay))
            case = {"incoming": "a,b", "outgoing": "c,d"}
            self.assertEqual(audit.calchep(folder, case), (12, 10000, 2))
            replay["Results"]["GCC128"]["Points"] = 1
            path.write_text(json.dumps(replay))
            with self.assertRaises(ValueError):
                audit.calchep(folder, case)


if __name__ == "__main__":
    unittest.main()
