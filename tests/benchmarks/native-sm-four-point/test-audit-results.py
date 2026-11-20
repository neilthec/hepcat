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

    def test_wolfram_cross_score_binds_native_bytes_and_checks_leaf_ceiling(self):
        example = dict(expression_sha256="abc", family="sm01", native_weight="2", native_leaves="18",
                       wolfram_weight="2", wolfram_leaves="19", notebook_weight="3", notebook_leaves="20",
                       wolfram_version="15.0.0")
        case = {k: example[k] for k in ("family", "notebook_weight", "notebook_leaves")}
        native = dict(weight="2", leaves="18")
        self.assertEqual(audit.wolfram_score(example, case, native, "abc")["wolfram_leaves"], "19")
        for invalid in (dict(example, expression_sha256="abd"), dict(example, wolfram_leaves="21"),
                        dict(example, native_weight="5"), dict(example, wolfram_weight="4")):
            with self.assertRaises(ValueError):
                audit.wolfram_score(invalid, case, native, "abc")

    def test_absolute_complexity_and_leaf_count_must_both_pass(self):
        case = dict(notebook_weight="2", notebook_leaves="4")
        for weight, leaves in ((2, 5), (3, 4)):
            with self.assertRaisesRegex(ValueError, "complexity"):
                audit.native_case(dict(case, status="PASS", weight=weight, leaves=leaves), case, [])

    def test_calchep_common_chart_replays_both_backends_at_every_captured_point(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            folder = Path(tmpdir)
            momenta = {f"p{i}_{j}": str(x) for i,p in enumerate(((2,0,0,2),(2,0,0,-2),(2,2,0,0),(2,-2,0,0)),1)
                       for j,x in enumerate(p)}
            path = folder / "calchep-chart.csv"
            path.write_text("point," + ",".join(momenta) + "\n" +
                            "".join(str(index) + "," + ",".join(momenta.values()) + "\n" for index in (42,51)))
            original = {index: dict(momenta, hepcat="1", calchep="1.1") for index in (42,51)}
            rows = [dict(Point=index, RawSPINAS=1, RawCalcHEP=1.1, UnprojectedCalcHEP=0.9, SPINAS="1.0",
                         CalcHEP="1.0", RelativeError=0, ComponentChangeOverECM=0, RawToRefinedSPINASRelativeChange=0) for index in original]
            h = audit.digest(path)
            chart = dict(Status="PASS", Seed=15, RequestedPoints=2, ComparedPoints=2, Errors=0, Mismatches=0,
                         SPINASMantissaBits=113, CalcHEPMantissaBits=113, RelativeTolerance=1e-7, Masses=[0]*4,
                         InputSHA256=h, SPINASInputSHA256=h, CalcHEPInputSHA256=h, SourceOutliersSHA256="raw",
                         MaximumComponentChangeOverECM=0, ComponentChangeLimitOverECM=16*2**-52, MaximumRelativeError=0, MaximumSPINASRelativeChange=0, Rows=rows)
            replay = dict(RelativeTolerance=1e-7, PointsSHA256="raw", SameChartReplay=chart,
                          Results={"GCC128": dict(Status="FAIL", Mismatches=2, MaximumRelativeError=0.1)})
            self.assertEqual(audit.calchep_chart(folder, replay, original, 15)["calchep_chart_points"], 2)
            for change in (dict(SPINASInputSHA256="raw"), dict(Rows=rows[:-1]), dict(Seed=16), dict(Masses=[0,1,0,0]),
                           dict(Rows=[dict(rows[0], ComponentChangeOverECM=0.5), rows[1]]),
                           dict(Rows=[dict(rows[0], CalcHEP="0.9"), rows[1]]),
                           dict(Rows=[dict(rows[0], SPINAS="2.0", CalcHEP="2.0"), rows[1]])):
                with self.assertRaises(ValueError):
                    audit.calchep_chart(folder, dict(replay, SameChartReplay=dict(chart, **change)), original, 15)

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
                          Results={"Native": dict(Points=2, OriginalBitMatches=2, Mismatches=2, Errors=0, Status="FAIL"),
                                   "GCC128": dict(Points=2, Errors=0, Mismatches=0, Status="PASS",
                                                  MantissaBits=113, REALBytes=16)})
            path = folder / "calchep-precision.json"
            path.write_text(json.dumps(replay))
            case = {"incoming": "a,b", "outgoing": "c,d"}
            self.assertEqual(audit.calchep(folder, case), (12, 10000, 2, {}))
            replay["Results"]["GCC128"]["Points"] = 1
            path.write_text(json.dumps(replay))
            with self.assertRaises(ValueError):
                audit.calchep(folder, case)


if __name__ == "__main__":
    unittest.main()
