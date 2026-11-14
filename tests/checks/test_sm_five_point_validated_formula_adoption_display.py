"""One tiny CLI notebook build covers adopted and ordinary display records.

All values are synthetic display fixtures. No amplitude construction, native
validation or numerical backend is executed.
"""
# HEPCAT - High Energy Physics Constructive Amplitude Tool
# Copyright (C) 2026 Neil Christensen
# SPDX-License-Identifier: GPL-3.0-or-later
import importlib.util
import json
import os
from pathlib import Path
import re
import shutil
import signal
import struct
import subprocess
import tempfile
import time
import unittest
import zlib

ROOT = Path(__file__).resolve().parents[2]
HERE = ROOT / 'tests/SM-5-point'
BUILDER = HERE / 'build-notebook.wl'
KERNEL = Path(os.environ.get('HEPCAT_TEST_KERNEL', shutil.which('WolframKernel') or
                           '/Applications/Wolfram.app/Contents/MacOS/WolframKernel'))
spec = importlib.util.spec_from_file_location('adoption_display_wire', HERE / 'prime-notebook-display-cache.py')
WIRE = importlib.util.module_from_spec(spec)
spec.loader.exec_module(WIRE)
S, A, V = WIRE.string_payload, WIRE.association_payload, WIRE.varint


def function(name, arguments):
    return b'f' + V(len(arguments)) + b's' + V(len(name)) + name.encode() + b''.join(arguments)


def sequence(values):
    return function('List', values)


def integer(value):
    return b'L' + struct.pack('<q', value)


def boolean(value):
    name = b'True' if value else b'False'
    return b's' + V(len(name)) + name


def case_groups(text, identifiers):
    """Match bracket spans while respecting strings, without evaluating code."""
    starts = {match.end() - 1: match.start()
              for match in re.finditer(r'Cell\s*\[\s*CellGroupData\s*\[', text)}
    spans, stack = [], []
    quoted = escaped = False
    for position, character in enumerate(text):
        if quoted:
            if escaped:
                escaped = False
            elif character == '\\':
                escaped = True
            elif character == '"':
                quoted = False
            continue
        if character == '"':
            quoted = True
        elif character == '[':
            stack.append(position)
        elif character == ']':
            opened = stack.pop()
            if opened in starts:
                spans.append((starts[opened], position + 1))
    groups = []
    for identifier in identifiers:
        position = text.index('sm5-construction-' + identifier)
        left, right = min((span for span in spans if span[0] < position < span[1]),
                          key=lambda span: span[1] - span[0])
        groups.append(text[left:right])
    return groups


@unittest.skipUnless(KERNEL.is_file(), 'Licensed Wolfram kernel required')
class ValidatedFormulaAdoptionDisplayTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.temp = tempfile.TemporaryDirectory(prefix='hepcat-adoption-display-')
        cls.addClassCleanup(cls.temp.cleanup)
        base = Path(cls.temp.name)
        results = base / 'results'
        results.mkdir()
        identifiers = ['sm5_fixture' + str(index) for index in range(1, 7)]
        kinds = ['fresh', 'retained', 'unknown', 'no-tables', 'bad-tables', 'ordinary']
        cases = [{'ID': identifier, 'FamilyID': 'sm5f_fixture', 'Particles': ['h'] * 5,
                  'Sector': 'PureHiggs'} for identifier in identifiers]
        family = {'ID': 'sm5f_fixture', 'CaseIDs': identifiers, 'Particles': ['h'] * 5,
                  'Incoming': ['h'] * 2, 'Outgoing': ['h'] * 3,
                  'ExportName': 'toyHiggs', 'Sector': 'PureHiggs'}
        inventory = base / 'inventory.json'
        inventory.write_text(json.dumps({'Cases': cases, 'Families': [family]}))
        for value, identifier, kind in zip(range(31, 37), identifiers, kinds):
            fields = [('ID', S(identifier)), ('Status', S('COMPLETE')), ('Expression', integer(value)),
                      ('Prints', sequence([S('ordinary actual print' if kind == 'ordinary'
                                            else 'legacy source print must not be displayed')]))]
            if kind != 'ordinary':
                fields.append(('RecordKind', S('ValidatedFormulaAdoption')))
                if kind != 'unknown':
                    fields.append(('ValidationAdoptionDisplay', A([
                        ('FreshHighEnergyReplayExecuted', boolean(kind == 'fresh'))])))
                if kind != 'no-tables':
                    rows = sequence([A([('Type', S('Physical')), ('Test', S('actual scoped composed row')),
                                        ('Status', S('PASS'))])])
                    scopes = sequence([] if kind == 'bad-tables'
                                      else [S('Original native row; unchanged validator')])
                    fields.append(('AdoptionValidationTables', sequence([A([
                        ('ParameterSetIndex', integer(1)), ('ColorCoefficient', integer(1)),
                        ('Rows', rows), ('EvidenceScopes', scopes)])])))
            (results / (identifier + '.wxf')).write_bytes(b'8C:' + zlib.compress(A(fields)))
        (results / 'sm5f_fixture-export.wxf').write_bytes(b'8C:' + zlib.compress(A([
            ('Status', S('PASS')), ('Prints', sequence([S('actual synthetic export')])),
            ('Result', A([('Directory', S('synthetic-directory'))]))])))
        output = base / 'worked.nb'
        arguments = ['--inventory', str(inventory), '--results', str(results),
                     '--families', 'sm5f_fixture', '--probes', str(base / 'absent-probes.json'),
                     '--output', str(output)]
        started, peak = time.monotonic(), 0
        with (base / 'kernel.log').open('w') as log:
            process = subprocess.Popen([str(KERNEL), '-script', str(BUILDER), *arguments], cwd=ROOT,
                                       stdout=log, stderr=subprocess.STDOUT, start_new_session=True)
            while process.poll() is None:
                info = subprocess.run(['ps', '-o', 'rss=', '-p', str(process.pid)],
                                      capture_output=True, text=True).stdout.strip()
                peak = max(peak, int(info or '0') * 1024)
                if time.monotonic() - started > 30 or peak > 512 << 20:
                    os.killpg(process.pid, signal.SIGKILL)
                    process.wait()
                    raise AssertionError('Synthetic formatter exceeded512MiB/30s')
                time.sleep(.1)
        if process.returncode:
            raise AssertionError((base / 'kernel.log').read_text())
        # Put inserts physical line continuations inside long string literals.
        text = output.read_text().replace('\\\n', '')
        cls.groups = dict(zip(kinds, case_groups(text, identifiers)))
        cls.text = text
        shutil.rmtree(results)
        cls.results_absent = not results.exists()

    def test_six_actual_case_groups(self):
        self.assertEqual(len(self.groups), 6)

    def test_ordinary_constructor_is_the_rerun_input(self):
        self.assertTrue(all(' = ConstructAmplitude[' in value for value in self.groups.values()))

    def test_no_disposable_cache_import_dependency(self):
        self.assertTrue(all(' = Import[' not in value for value in self.groups.values()))

    def test_exact_saved_formulas(self):
        for value, kind in zip(range(31, 37), self.groups):
            self.assertRegex(self.groups[kind], r'BoxData\["' + str(value) + r'"\]')

    def test_fresh_replay_is_scoped(self):
        self.assertIn('fresh high-energy checks', self.groups['fresh'])

    def test_retained_checks_do_not_claim_a_fresh_replay(self):
        self.assertIn('verified unchanged validators', self.groups['retained'])
        self.assertNotIn('fresh high-energy checks', self.groups['retained'])

    def test_unknown_scope_does_not_claim_a_fresh_replay(self):
        self.assertNotIn('fresh high-energy checks', self.groups['unknown'])

    def test_old_prints_are_not_repeated_as_current_checks(self):
        self.assertNotIn('legacy source print must not be displayed', self.text)

    def test_ordinary_prints_remain_visible(self):
        self.assertIn('ordinary actual print', self.groups['ordinary'])

    def test_actual_composed_rows_and_evidence_are_visible(self):
        self.assertIn('actual scoped composed row', self.groups['fresh'])
        self.assertIn('Original native row; unchanged validator', self.groups['fresh'])

    def test_missing_tables_remain_unavailable(self):
        self.assertIn('tables are unavailable', self.groups['no-tables'])
        self.assertNotIn('actual scoped composed row', self.groups['no-tables'])

    def test_malformed_scopes_cannot_invent_a_table(self):
        self.assertIn('no check rows are inferred', self.groups['bad-tables'])
        self.assertNotIn('actual scoped composed row', self.groups['bad-tables'])

    def test_new_input_is_not_called_the_saved_record_producer(self):
        self.assertIn('was not executed to produce this adopted record', self.groups['fresh'])

    def test_saved_formula_and_rerun_survive_cache_deletion(self):
        self.assertTrue(self.results_absent)
        self.assertIn('BoxData["31"]', self.groups['fresh'])
        self.assertIn(' = ConstructAmplitude[', self.groups['fresh'])
        self.assertNotIn(' = Import[', self.groups['fresh'])


if __name__ == '__main__':
    unittest.main()
