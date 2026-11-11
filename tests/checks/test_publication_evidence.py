"""Evidence tools reject incomplete, changed, or unsafe saved inputs.

These small checks exercise the public reproduction tools without running
Mathematica, an amplitude generator, or the full numerical precision study.
"""

# HEPCAT - High Energy Physics Constructive Amplitude Tool
#
# Copyright (C) 2026 Neil Christensen
#
# This file is part of HEPCAT.
#
# HEPCAT is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# HEPCAT is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with HEPCAT.  If not, see <https://www.gnu.org/licenses/>.

from decimal import Decimal
import importlib.util
import io
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
import zipfile

TOOLS = Path(__file__).resolve().parents[1] / 'benchmarks/reproduction'


def module(name):
    spec = importlib.util.spec_from_file_location(name.replace('-', '_'), TOOLS / (name + '.py'))
    loaded = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(loaded)
    return loaded


analysis = module('analyze-precision')
replay = module('replay-precision')
verify = module('verify')


class PublicationEvidenceTests(unittest.TestCase):
    def test_invalid_values_are_reported_not_dropped(self):
        result = analysis.summary([
            (('zero',), Decimal(0), Decimal(0)),
            (('valid',), Decimal(2), Decimal(1)),
            (('nan',), Decimal('NaN'), Decimal(1)),
            (('negative',), Decimal(-1), Decimal(1))])
        self.assertEqual(result['points'], 4)
        self.assertEqual(result['finite_pairs'], 2)
        self.assertEqual(result['invalid_pairs'], 2)
        self.assertEqual(result['both_zero'], 1)
        self.assertEqual(result['maximum'], Decimal('0.5'))
        self.assertEqual(result['invalid_point_ids'], ['nan', 'negative'])

    def test_duplicate_point_is_rejected(self):
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / 'rates.csv'
            path.write_text('point,physical,spurious,madgraph\n1,1,1,1\n1,2,2,2\n')
            with self.assertRaisesRegex(ValueError, 'Duplicate'):
                analysis.read_rows(path, ['point'])

    def test_changed_reference_or_missing_point_is_rejected(self):
        with tempfile.TemporaryDirectory() as temporary:
            paths = {mode: Path(temporary) / (mode + '.csv') for mode in analysis.MODES}
            for path in paths.values():
                path.write_text('point,physical,spurious,madgraph\n1,1,1,1\n2,2,2,2\n')
            paths['extended'].write_text('point,physical,spurious,madgraph\n1,1,1,2\n2,2,2,2\n')
            with self.assertRaisesRegex(ValueError, 'reference changed'):
                analysis.process(paths, ['point'])
            paths['extended'].write_text('point,physical,spurious,madgraph\n1,1,1,1\n')
            with self.assertRaisesRegex(ValueError, 'point identities'):
                analysis.process(paths, ['point'])

    def test_zip_traversal_and_symlink_are_rejected(self):
        with tempfile.TemporaryDirectory() as temporary:
            for member in ('../outside', '/outside'):
                content = io.BytesIO()
                with zipfile.ZipFile(content, 'w') as archive:
                    archive.writestr(member, 'unsafe')
                content.seek(0)
                with zipfile.ZipFile(content) as archive:
                    with self.assertRaisesRegex(ValueError, 'Unsafe path'):
                        replay.safe_extract(archive, Path(temporary))
            content = io.BytesIO()
            with zipfile.ZipFile(content, 'w') as archive:
                entry = zipfile.ZipInfo('link')
                entry.external_attr = 0o120777 << 16
                archive.writestr(entry, '../outside')
            content.seek(0)
            with zipfile.ZipFile(content) as archive:
                with self.assertRaisesRegex(ValueError, 'Symlinks'):
                    replay.safe_extract(archive, Path(temporary))

    def test_printed_rounding_must_match_recomputed_value(self):
        verify.matches('1.23e-8', Decimal('1.234e-8'))
        with self.assertRaisesRegex(ValueError, 'differs'):
            verify.matches('1.23e-8', Decimal('1.236e-8'))

    def test_current_catalog_and_explicit_paper_labels_share_unchanged_sources(self):
        with tempfile.TemporaryDirectory() as temporary:
            catalogs = []
            for name, flags in (('current', []), ('paper', ['--paper-model-labels'])):
                output = Path(temporary) / name
                result = subprocess.run(
                    [sys.executable, str(TOOLS / 'catalog-build.py'),
                     '--output-directory', str(output), *flags],
                    capture_output=True, text=True, timeout=30)
                self.assertEqual(result.returncode, 0, result.stderr)
                catalogs.append(json.loads((output / 'catalog-expressions.json').read_text()))
            current, paper = catalogs
            self.assertEqual({row['model'] for row in current}, {4, 5})
            self.assertEqual({row['model'] for row in paper}, {3, 4})
            self.assertEqual(paper, [{**row, 'model': row['model'] - 1} for row in current])
            self.assertTrue(all(row['preference'] == {4: 'AllPhysical', 5: 'AllSpurious'}[row['model']]
                                for row in current))
            self.assertEqual((Path(temporary) / 'current/catalog-expanded.tex').read_bytes(),
                             (Path(temporary) / 'paper/catalog-expanded.tex').read_bytes())

    def test_transcription_checker_rejects_disabled_assertions(self):
        environment = dict(os.environ, PYTHONDONTWRITEBYTECODE='1')
        result = subprocess.run([sys.executable, '-O', str(TOOLS / 'catalog-check.py')],
                                env=environment, capture_output=True, text=True)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn('requires Python assertions', result.stderr)


if __name__ == '__main__':
    unittest.main()
