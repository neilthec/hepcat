"""Check the physical two-body reconstruction used for sparse CalcHEP precision replays."""
import csv
from decimal import Decimal, localcontext
import importlib.util
from pathlib import Path
import tempfile
import unittest

MODULE = Path(__file__).resolve().parents[2] / 'source/internal/export/calchep_chart.py'
spec = importlib.util.spec_from_file_location('calchep_chart', MODULE)
chart = importlib.util.module_from_spec(spec)
spec.loader.exec_module(chart)


class TwoBodyReplayTests(unittest.TestCase):
    def setUp(self):
        directory = tempfile.TemporaryDirectory()
        self.addCleanup(directory.cleanup)
        self.input = Path(directory.name) / 'original.csv'
        self.chart = Path(directory.name) / 'chart.csv'
        self.keys = [f'p{i}_{j}' for i in range(1, 5) for j in range(4)]
        self.masses = [0, 80.385, 1.3, 0.095]
        self.outlier = [157.43714327445457, 0, 0, 157.43714327445457,
                        176.77161058105776, 0, 0, -157.43714327445457,
                        167.10689178558172, -55.121502927825944, -147.59387990901695, -55.683837965952797,
                        167.10186206993063, 55.121502927825944, 147.59387990901695, 55.683837965952797]

    def write(self, points):
        with self.input.open('w', newline='') as stream:
            writer = csv.writer(stream)
            writer.writerow(['point', *self.keys])
            writer.writerows([[i, *p] for i, p in points])

    def test_common_chart_preserves_direction_and_restores_all_mass_shells(self):
        self.write([(1125, self.outlier)])
        changes = chart.chart_points(self.input, self.chart, self.masses)
        with localcontext() as ctx, self.chart.open() as stream:
            ctx.prec = 80
            row, = csv.DictReader(stream)
            self.assertEqual(int(row['point']), 1125)
            p = [[Decimal(row[f'p{i}_{j}']) for j in range(4)] for i in range(1, 5)]
            for (energy, *q), mass in zip(p, self.masses):
                self.assertLess(abs(energy*energy-sum(x*x for x in q)-Decimal(mass)**2), Decimal('1e-42'))
            for j in range(4):
                self.assertLess(abs(p[0][j]+p[1][j]-p[2][j]-p[3][j]), Decimal('1e-44'))
            self.assertLess(abs(p[2][1]*Decimal(self.outlier[10]) - p[2][2]*Decimal(self.outlier[9])),
                            Decimal('1e-43'))
            self.assertEqual(p[0][3], Decimal(self.outlier[3]))
            self.assertLess(changes[1125], chart.COMPONENT_LIMIT)

    def test_duplicate_and_non_rounding_kinematic_changes_are_rejected(self):
        self.write([(1125, self.outlier), (1125, self.outlier)])
        with self.assertRaisesRegex(ValueError, 'duplicated'):
            chart.chart_points(self.input, self.chart, self.masses)
        wrong = self.outlier.copy()
        wrong[12] += 0.0001
        self.write([(1125, wrong)])
        with self.assertRaisesRegex(ValueError, 'rounding'):
            chart.chart_points(self.input, self.chart, self.masses)

    def test_nonfinite_and_duplicate_evaluations_are_rejected(self):
        for rows in ('1,NaN', '1,-0.01', '1,0.1\n1,0.2'):
            with self.subTest(rows=rows):
                self.input.write_text('point,squared\n'+rows+'\n')
                with self.assertRaises(ValueError):
                    chart.read_values(self.input, 'squared')


if __name__ == '__main__':
    unittest.main()
