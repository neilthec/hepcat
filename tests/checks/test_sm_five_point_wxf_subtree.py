"""Metadata-only WXF wire-copy controls. No package or physics kernel is loaded."""
# HEPCAT - High Energy Physics Constructive Amplitude Tool
# Copyright (C) 2026 Neil Christensen
# SPDX-License-Identifier: GPL-3.0-or-later

import importlib.util
import json
import os
from pathlib import Path
import shutil
import struct
import subprocess
import sys
import tempfile
import unittest
from unittest import mock
import zlib

ROOT = Path(__file__).resolve().parents[2]
HERE = ROOT / 'tests/SM-5-point'
# The private preinstallation replay points this at the staged code, not package source.
if os.environ.get('HEPCAT_WXF_TOOL_DIRECTORY'):
    HERE = Path(os.environ['HEPCAT_WXF_TOOL_DIRECTORY'])
spec = importlib.util.spec_from_file_location('hepcat_wxf_subtree', HERE / 'wxf-subtree.py')
TOOL = importlib.util.module_from_spec(spec)
spec.loader.exec_module(TOOL)


def first_party_sdk():
    """Use an installed client or a discoverable bundled SDK, never require it."""
    candidates = [os.environ.get('WOLFRAMCLIENT_PYTHON_PATH'),
                  '/Applications/Wolfram.app/Contents/SystemFiles/Components/WolframClientForPython']
    candidates += [str(x) for x in Path('/usr/local/Wolfram').glob(
        '*/SystemFiles/Components/WolframClientForPython')]
    for candidate in candidates:
        if candidate and Path(candidate).is_dir():
            sys.path.insert(0, candidate)
    try:
        from wolframclient.deserializers import binary_deserialize
        from wolframclient.deserializers.wxf.wxfparser import WXFParser
        from wolframclient.serializers import export
        from wolframclient.language import wl
        return binary_deserialize, WXFParser, export, wl
    except ImportError:
        return None


SDK = first_party_sdk()


def varint(number):
    result = bytearray()
    while number >= 128:
        result.append((number & 127) | 128)
        number >>= 7
    result.append(number)
    return bytes(result)


def string(tag, value):
    data = value.encode('utf8')
    return tag + varint(len(data)) + data


def payload(wxf):
    return zlib.decompress(wxf[3:]) if wxf.startswith(b'8C:') else wxf[2:]


def all_atoms():
    atoms = [b'C' + struct.pack('<b', -128), b'j' + struct.pack('<h', -32000),
             b'i' + struct.pack('<i', -200000), b'L' + struct.pack('<q', -(2 ** 63)),
             b'r' + struct.pack('<d', 1.25), string(b'I', '123456789012345678901234567890'),
             string(b'R', '1.234567890123456789`24.'), string(b'S', 'héllø'),
             string(b's', 'Global`a'), b'B\4\0\xff\1\2',
             b'f\1f\1' + string(b's', 'f') + b'C\1C\2']
    signed = [(0, 'b', [-2, 3]), (1, 'h', [-300, 4]), (2, 'i', [-50000, 6]),
              (3, 'q', [-2 ** 40, 7]), (0x22, 'f', [1.5, 2.5]),
              (0x23, 'd', [1.5, 2.5]), (0x33, 'f', [1, 2, 3, 4]),
              (0x34, 'd', [1, 2, 3, 4])]
    unsigned = [(0x10, 'B', [1, 2]), (0x11, 'H', [1, 2]),
                (0x12, 'I', [1, 2]), (0x13, 'Q', [1, 2])]
    for tag in [b'\xc1', b'\xc2']:
        for kind, form, values in signed + (unsigned if tag == b'\xc2' else []):
            atoms.append(tag + bytes([kind]) + b'\1\2' +
                         struct.pack('<' + form * len(values), *values))
    return atoms


class RequestValidationTests(unittest.TestCase):
    def test_exact_mode_enum(self):
        for mode in ['index', 'EXTRACT', '', None, True, 1]:
            with self.subTest(mode=mode), self.assertRaises(ValueError):
                TOOL.requests_bytes([{'Mode': mode, 'Path': []}])

    def test_invalid_paths_and_request_size(self):
        invalid = [[], [{'Mode': 'Extract', 'Path': [True]}],
                   [{'Mode': 'Extract', 'Path': [{'head': 1}]}],
                   [{'Mode': 'Extract', 'Path': [{'arg': -1}]}],
                   [{'Mode': 'Extract', 'Path': [{'head': True}, 0]}],
                   [{'Mode': 'Extract', 'Path': ['a'] * 257}],
                   [{'Mode': 'Extract', 'Path': ['a' * (1 << 20)]}],
                   [{'Mode': 'Extract', 'Path': [], 'Extra': True}]]
        for requests in invalid:
            with self.subTest(requests=str(requests)[:80]), self.assertRaises(ValueError):
                TOOL.requests_bytes(requests)

    def test_finite_positive_time_and_bounds(self):
        for value in [0, -1, float('nan'), float('inf'), True, '1', 86401]:
            with self.subTest(seconds=value), self.assertRaises(ValueError):
                TOOL.validate_limits(value, 1, 1, 1, 1)
        for position in range(4):
            for bad in [0, -1, True, 1.5, 1 << 64]:
                args = [10, 10, 10, 10]
                args[position] = bad
                with self.subTest(position=position, value=bad), self.assertRaises(ValueError):
                    TOOL.validate_limits(1, *args)


@unittest.skipUnless(shutil.which(os.environ.get('CXX', 'c++')), 'C++17 compiler required')
class WireExtractionTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.build = TOOL.compiled_extractor()
        cls.executable, cls.build_info = cls.build.__enter__()

    @classmethod
    def tearDownClass(cls):
        cls.build.__exit__(None, None, None)

    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix='hepcat-wxf-test-')
        self.addCleanup(self.temp.cleanup)
        self.directory = Path(self.temp.name)
        self.counter = 0

    def run_extract(self, data, requests=None, **limits):
        self.counter += 1
        source = self.directory / ('input-' + str(self.counter) + '.wxf')
        source.write_bytes(data)
        output = self.directory / ('output-' + str(self.counter))
        manifest = TOOL.extract(source, output, requests or [{'Mode': 'Extract', 'Path': []}],
                                executable=self.executable, seconds=10, **limits)
        self.assertEqual(source.read_bytes(), data)
        self.assertTrue(manifest['SourceUnchanged'])
        return manifest, output

    def test_all_sdk_tags_and_array_types_copy_exact_bytes(self):
        atoms = all_atoms()
        body = b'f' + varint(len(atoms)) + string(b's', 'List') + b''.join(atoms)
        requests = [{'Mode': 'Extract', 'Path': [i]} for i in range(len(atoms))]
        requests += [{'Mode': 'Extract', 'Path': []}]
        for compressed in [False, True]:
            data = b'8C:' + zlib.compress(body) if compressed else b'8:' + body
            manifest, output = self.run_extract(data, requests)
            self.assertEqual(manifest['Status'], 'COMPLETE')
            for i, expected in enumerate(atoms + [body]):
                with self.subTest(compressed=compressed, atom=i):
                    self.assertEqual(payload((output / ('extract-' + str(i) + '.wxf')).read_bytes()), expected)

    def test_typed_association_keys_delayed_rules_and_compound_heads(self):
        head = b'f\1' + string(b's', 'f') + b'C\1'
        function = b'f\1' + head + b'C\2'
        body = (b'A\3-C\xff' + string(b'S', 'negative') + b':' +
                string(b's', 'Global`key') + function + b'-' +
                string(b'I', '999999999999999999999') + string(b'S', 'big'))
        paths = [[-1], [{'symbol': 'Global`key'}, {'head': True}],
                 [{'symbol': 'Global`key'}, {'arg': 0}], [999999999999999999999]]
        manifest, output = self.run_extract(b'8:' + body, [{'Mode': 'Extract', 'Path': p} for p in paths])
        for i, expected in enumerate([string(b'S', 'negative'), head, b'C\2', string(b'S', 'big')]):
            self.assertEqual(payload((output / ('extract-' + str(i) + '.wxf')).read_bytes()), expected)
        self.assertEqual(manifest['Status'], 'COMPLETE')

    def test_index_and_missing_paths_are_explicit(self):
        body = b'A\1-' + string(b'S', 'a') + b'f\2' + string(b's', 'List') + b'C\1C\2'
        requests = [{'Mode': 'Index', 'Path': []}, {'Mode': 'Index', 'Path': ['a']},
                    {'Mode': 'Extract', 'Path': ['missing']}]
        manifest, output = self.run_extract(b'8:' + body, requests)
        self.assertEqual(manifest['Status'], 'COMPLETE_WITH_MISSING_PATHS')
        targets = manifest['Parser']['Targets']
        self.assertEqual(targets[0]['Keys'], [{'Type': 's', 'Value': 'a'}])
        self.assertEqual((targets[1]['Kind'], targets[1]['Length']), ('List', 2))
        self.assertFalse(targets[2]['Found'])
        self.assertFalse((output / 'extract-2.wxf').exists())

    def test_failure_and_missing_argument_paths(self):
        body = (b'f\2' + string(b's', 'Failure') + string(b'S', 'Clock') +
                b'A\1-' + string(b'S', 'Seconds') + b'C\2')
        requests = [{'Mode': 'Extract', 'Path': ['Tag']},
                    {'Mode': 'Extract', 'Path': ['Failure', 1, 'Seconds']}]
        _, output = self.run_extract(b'8:' + body, requests)
        self.assertEqual(payload((output / 'extract-0.wxf').read_bytes()), string(b'S', 'Clock'))
        self.assertEqual(payload((output / 'extract-1.wxf').read_bytes()), b'C\2')

    def test_malformed_and_ambiguous_inputs_fail_closed(self):
        malformed = [b'8:f\1s\4List', b'8:C\1C\2', b'8C:' + zlib.compress(b'C\1')[:-2],
                     b'8:\xc1\0\1\0', b'8:\xc1\x10\1\1\0', b'8:Z', b'8:-C\1C\2',
                     b'8:A\2-S\1aC\1-S\1aC\2', b'8C:' + zlib.compress(b'C\1') + b'extra']
        for data in malformed:
            with self.subTest(data=data), self.assertRaises(RuntimeError):
                self.run_extract(data, [{'Mode': 'Extract', 'Path': ['a']}])
            manifest = json.loads((self.directory / ('output-' + str(self.counter)) / 'manifest.json').read_text())
            self.assertEqual(manifest['Status'], 'UNRESOLVED')
            self.assertTrue(manifest['SourceUnchanged'])

    def test_output_tokens_bytes_and_depth_limits(self):
        for limits in [{'max_output': 1}, {'max_tokens': 1}, {'max_bytes': 1}, {'max_depth': 1}]:
            with self.subTest(limits=limits), self.assertRaises(RuntimeError):
                self.run_extract(b'8:f\1s\4Listf\1s\4ListC\1', **limits)

    def test_cpp_limits_and_request_size_checked_before_trie(self):
        source = self.directory / 'small.wxf'; source.write_bytes(b'8:C\1')
        request = self.directory / 'huge.request'; request.write_bytes(b'\0' * ((1 << 20) + 1))
        command = [str(self.executable), str(source), str(request), str(self.directory),
                   '1', '10', '10', '10', '10']
        result = subprocess.run(command, capture_output=True, text=True, timeout=10)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn('request file limit', result.stderr)
        for bad in ['nan', 'inf', '0', '-1']:
            command[4] = bad
            result = subprocess.run(command, capture_output=True, text=True, timeout=10)
            self.assertNotEqual(result.returncode, 0)
            self.assertIn('positive finite bounds', result.stderr)

    def test_rss_units_for_macos_and_linux(self):
        source = self.directory / 'rss.cpp'
        source.write_text('#define main hidden_extractor_main\n#include ' + json.dumps(str(TOOL.SOURCE)) +
                          '\n#undef main\nint main(){return residentBytes(7,true)==7 && '
                          'residentBytes(7,false)==7168 ? 0:1;}\n')
        binary = self.directory / 'rss'
        command = [shutil.which(os.environ.get('CXX', 'c++')), '-std=c++17', str(source), '-lz', '-o', str(binary)]
        subprocess.run(command, check=True, capture_output=True, timeout=30)
        subprocess.run([str(binary)], check=True, timeout=10)

    def test_source_hash_change_and_existing_output_reject(self):
        source = self.directory / 'source.wxf'; source.write_bytes(b'8:C\1')
        with self.assertRaises(ValueError):
            TOOL.extract(source, self.directory / 'bad-sha', [{'Mode': 'Extract', 'Path': []}],
                         executable=self.executable, expected_sha='0' * 64)
        original_run = subprocess.run

        def change_source_after_read(*args, **kwargs):
            result = original_run(*args, **kwargs)
            source.write_bytes(b'8:C\2')
            return result

        with mock.patch.object(TOOL.subprocess, 'run', side_effect=change_source_after_read):
            with self.assertRaises(RuntimeError):
                TOOL.extract(source, self.directory / 'changed', [{'Mode': 'Extract', 'Path': []}],
                             executable=self.executable)
        saved = json.loads((self.directory / 'changed/manifest.json').read_text())
        self.assertEqual(saved['Status'], 'UNRESOLVED')
        self.assertNotIn('Parser', saved)
        self.assertFalse(saved['SourceUnchanged'])
        with self.assertRaises(ValueError):
            TOOL.extract(source, self.directory / 'changed', [{'Mode': 'Extract', 'Path': []}],
                         executable=self.executable)

    def test_fake_prebuilt_tool_cannot_claim_incomplete_target_protocol(self):
        source = self.directory / 'source.wxf'
        source.write_bytes(b'8:C\1')
        malformed_targets = [[], [{'Index': 1, 'Mode': 0, 'Found': False}],
                             [{'Index': 0, 'Mode': 1, 'Found': False}],
                             [{'Index': 0, 'Mode': 0, 'Found': 'false'}]]
        for number, targets in enumerate(malformed_targets):
            tool = self.directory / ('fake-tool-' + str(number))
            response = json.dumps({'CompleteParse': True, 'Targets': targets})
            tool.write_text('#!/usr/bin/env python3\nprint(' + repr(response) + ')\n')
            tool.chmod(0o755)
            output = self.directory / ('fake-result-' + str(number))
            with self.subTest(targets=targets), self.assertRaises(RuntimeError):
                TOOL.extract(source, output, [{'Mode': 'Extract', 'Path': []}],
                             executable=tool, seconds=10)
            manifest = json.loads((output / 'manifest.json').read_text())
            self.assertEqual(manifest['Status'], 'UNRESOLVED')
            self.assertNotIn('Parser', manifest)
            self.assertTrue(manifest['SourceUnchanged'])

    @unittest.skipUnless(SDK, 'first-party WolframClientForPython SDK is optional')
    def test_first_party_sdk_compressed_and_uncompressed_roundtrip(self):
        deserialize, parser, export, wl = SDK
        value = {'Details': {'check': {'Status': 'FAIL', 'Reason': wl.Failure('Clock', {'Seconds': 2})}},
                 'Amplitude': wl.Plus(wl.x, 1), 'list': [2 ** 80, -2 ** 63, 3.25, b'\0\xff']}
        paths = [['Details', 'check'], ['list', 0], ['Amplitude', {'head': True}]]
        for compressed in [False, True]:
            manifest, output = self.run_extract(export(value, target_format='wxf', compress=compressed),
                                                [{'Mode': 'Extract', 'Path': p} for p in paths])
            for i, expected in enumerate([value['Details']['check'], 2 ** 80, wl.Plus]):
                self.assertEqual(deserialize((output / ('extract-' + str(i) + '.wxf')).read_bytes()),
                                 deserialize(export(expected, target_format='wxf')))
        # Every supported array/scalar/function token is also accepted unchanged by the SDK parser.
        for atom in all_atoms():
            list(parser(b'8:' + atom).tokens())


if __name__ == '__main__':
    unittest.main()
