"""Portable exact export projections, with no Wolfram or physics evaluation."""
# HEPCAT - High Energy Physics Constructive Amplitude Tool
# Copyright (C) 2026 Neil Christensen
# SPDX-License-Identifier: GPL-3.0-or-later
import importlib.util
import json
import os
from pathlib import Path
import shutil
import struct
import sys
import tempfile
import unittest
from unittest import mock
import zlib

ROOT = Path(__file__).resolve().parents[2]
HERE = ROOT / 'tests/SM-5-point'
spec = importlib.util.spec_from_file_location('export_input_projector', HERE / 'project-export-inputs.py')
PROJECT = importlib.util.module_from_spec(spec); spec.loader.exec_module(PROJECT)
S = PROJECT.WIRE.string_payload
A = PROJECT.WIRE.association_payload
V = PROJECT.WIRE.varint
F = lambda name, values: b'f' + V(len(values)) + b's' + V(len(name)) + name.encode() + b''.join(values)
L = lambda values: F('List', values)
I = lambda value: b'L' + struct.pack('<q', value)

try:
    sys.path.insert(0, '/Applications/Wolfram.app/Contents/SystemFiles/Components/WolframClientForPython')
    from wolframclient.deserializers import binary_deserialize as SDK
except ImportError:
    SDK = None


class LiteralIdentityTests(unittest.TestCase):
    def test_literal_list_utf8_and_context_qualified_head(self):
        self.assertEqual(PROJECT.literal_particles(L([S('e'), S('é')])), ['e', 'é'])
        self.assertEqual(PROJECT.literal_particles(F('System`List', [S('Z')])), ['Z'])

    def test_nonliteral_wrong_head_trailing_and_truncated_reject(self):
        for payload in (S('e,E'), F('foo', [S('e')]), L([I(1)]), L([S('e')])+b'C\0', b'f\1s\4ListS\7x'):
            with self.subTest(payload=payload), self.assertRaises(ValueError):
                PROJECT.literal_particles(payload)


@unittest.skipUnless(shutil.which(os.environ.get('CXX', 'c++')), 'C++17 compiler required')
class ExportProjectionTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.compiler = PROJECT.WIRE.WIRE.compiled_extractor()
        cls.tool, _ = cls.compiler.__enter__()

    @classmethod
    def tearDownClass(cls):
        cls.compiler.__exit__(None, None, None)

    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix='hepcat-export-projection-test-')
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name); self.source = self.root/'native.wxf'; self.output = self.root/'projection'
        self.particles = ['u', 'U', 'Z', 'h', 'h']; self.seed = 9007199254740997
        self.expression = F('ConstructiveDiagrams`SpinorProduct', [I(-7), I(5)])
        self.fields = [('Expression', self.expression), ('Status', S('COMPLETE')),
                       ('Particles', L([S(x) for x in self.particles])), ('ID', S('fixture')),
                       ('ContextSHA256', S('a'*64)), ('RandomSeed', I(self.seed))]

    def source_record(self, fields=None, compressed=True):
        payload = A(self.fields if fields is None else fields)
        self.source.write_bytes(b'8C:'+zlib.compress(payload) if compressed else b'8:'+payload)
        self.before = self.source.read_bytes(); self.expected = PROJECT.WIRE.sha(self.source)
        return self.source

    def project(self, **options):
        return PROJECT.project_export_input(self.source, self.output, expected_sha=self.expected,
            case_id='fixture', particles=self.particles, expected_context='a'*64, executable=self.tool, **options)

    def test_exact_projection_and_seed_preserve_every_selected_token(self):
        self.source_record(); row = self.project()
        actual = PROJECT.WIRE.small_payload(self.output/'input.wxf', PROJECT.MAX_OUTPUT)
        self.assertEqual(actual, A(self.fields)); self.assertEqual(self.source.read_bytes(), self.before)
        self.assertFalse(row['FullNativeCheckpointImported']); self.assertFalse(row['ExpressionsEvaluated'])
        self.assertFalse(row['ValidationSynthesized']); self.assertEqual(row['SourceSHA256'], self.expected)
        self.assertEqual(row['PayloadSHA256'], PROJECT.WIRE.sha(self.output/'input.wxf'))
        self.assertEqual(row['Limits'], {'ParserSeconds':30,'ParserResidentBytes':512<<20,'ProjectionBytes':64<<20})
        if SDK:
            value = SDK((self.output/'input.wxf').read_bytes())
            self.assertEqual(value['Expression'], SDK(b'8:'+self.expression)); self.assertEqual(value['RandomSeed'], self.seed)

    def test_large_unselected_diagnostics_are_skipped_not_deserialized(self):
        large = b'B'+V(32<<20)+b'x'*(32<<20)
        self.source_record(self.fields + [('Details', A([('HugeSolverLedger', large)]))])
        row = self.project()
        self.assertLess(row['UncompressedProjectionBytes'], 1024)
        self.assertNotIn('Details', row['CopiedFields'])
        self.assertEqual(PROJECT.WIRE.small_payload(self.output/'input.wxf', 1024), A(self.fields))

    def test_process_alias_only_uncompressed_is_preserved_without_added_alias(self):
        fields=[(k if k!='Particles' else 'Process',v) for k,v in self.fields]
        self.source_record(fields,False); row=self.project()
        self.assertEqual(row['IdentityFields'],['Process'])
        self.assertEqual(PROJECT.WIRE.small_payload(self.output/'input.wxf',1024), A(fields))

    def test_both_agreeing_aliases_are_retained(self):
        fields=self.fields+[('Process',L([S(x)for x in self.particles]))]
        self.source_record(fields); row=self.project()
        self.assertEqual(row['IdentityFields'],['Particles','Process'])
        self.assertEqual(PROJECT.WIRE.small_payload(self.output/'input.wxf',1024), A([(k,dict(fields)[k])for k in PROJECT.FIELDS if k in dict(fields)]))

    def test_conflicting_alias_rejects(self):
        self.source_record(self.fields+[('Process',L([S('h')]*5))])
        with self.assertRaisesRegex(ValueError,'external particles disagree'):self.project()
        self.assertFalse(self.output.exists())

    def test_each_required_key_and_identity_are_mandatory(self):
        for missing in ('Expression','Status','Particles'):
            with self.subTest(missing=missing):
                self.source_record([(k,v)for k,v in self.fields if k!=missing])
                with self.assertRaises(ValueError):self.project()
                self.assertFalse(self.output.exists())

    def test_wrong_status_failure_expression_or_malformed_particles_reject(self):
        variants=[('Status',S('FAIL')),('Status',S('PASS')),
                  ('Expression',F('System`Failure',[S('bad'),A([])])),('Particles',L([I(5)]))]
        for key,value in variants:
            with self.subTest(key=key,value=value[:15]):
                self.source_record([(k,value if k==key else v)for k,v in self.fields])
                with self.assertRaises(ValueError):self.project()
                self.assertFalse(self.output.exists())

    def test_wrong_id_context_or_inventory_reject(self):
        for key,value in [('ID',S('other')),('ContextSHA256',S('b'*64)),('Particles',L([S('h')]*5))]:
            with self.subTest(key=key):
                self.source_record([(k,value if k==key else v)for k,v in self.fields])
                with self.assertRaises(ValueError):self.project()
                self.assertFalse(self.output.exists())

    def test_missing_optional_identity_provenance_is_supported(self):
        fields=[(k,v)for k,v in self.fields if k not in ('ID','ContextSHA256','RandomSeed')]
        self.source_record(fields);self.project()
        self.assertEqual(PROJECT.WIRE.small_payload(self.output/'input.wxf',1024),A(fields))

    def test_stale_admitted_checksum_rejects_before_parser(self):
        self.source_record();self.source.write_bytes(b'changed')
        with mock.patch.object(PROJECT.WIRE.WIRE,'extract')as extract:
            with self.assertRaisesRegex(ValueError,'admitted SHA256'):self.project()
            extract.assert_not_called()
        self.assertFalse(self.output.exists())

    def test_source_mutation_after_parser_prevents_publication(self):
        self.source_record(); original=PROJECT.WIRE.WIRE.extract
        def changed(*args,**kwargs):
            result=original(*args,**kwargs);self.source.write_bytes(b'changed');return result
        with mock.patch.object(PROJECT.WIRE.WIRE,'extract',side_effect=changed):
            with self.assertRaisesRegex(ValueError,'changed during'):self.project()
        self.assertFalse(self.output.exists())

    def test_copied_field_corruption_is_rejected(self):
        self.source_record();original=PROJECT.WIRE.WIRE.extract
        def changed(*args,**kwargs):
            result=original(*args,**kwargs)
            target=next(x for x in result['Parser']['Targets']if x['Mode']==0 and x['Found'])
            (Path(args[1])/target['File']).write_bytes(b'8:S\0');return result
        with mock.patch.object(PROJECT.WIRE.WIRE,'extract',side_effect=changed):
            with self.assertRaisesRegex(ValueError,'Copied field checksum'):self.project()
        self.assertFalse(self.output.exists())

    def test_mutation_at_publication_removes_our_owned_projection(self):
        self.source_record(); original=PROJECT.os.rename
        def changed(*args,**kwargs):
            result=original(*args,**kwargs)
            if Path(args[1])==self.output:self.source.write_bytes(b'changed after publication')
            return result
        with mock.patch.object(PROJECT.os,'rename',side_effect=changed):
            with self.assertRaisesRegex(ValueError,'changed after'):self.project()
        self.assertFalse(self.output.exists())
        self.assertFalse(list(self.root.glob('.export-input-*')))

    def test_failure_cleans_bounded_field_and_compilation_temporaries(self):
        self.source_record([(k,S('FAIL') if k=='Status' else v)for k,v in self.fields])
        with self.assertRaises(ValueError):self.project()
        self.assertFalse(list(self.root.glob('.export-input-*')))
        self.assertFalse(self.output.exists())

    def test_existing_stale_output_is_never_reused(self):
        self.source_record();self.output.mkdir();(self.output/'input.wxf').write_bytes(b'stale')
        with self.assertRaisesRegex(ValueError,'new private directory'):self.project()
        self.assertEqual((self.output/'input.wxf').read_bytes(),b'stale')

    def test_corrupt_wxf_and_duplicate_keys_reject(self):
        for payload in (b'8C:not-zlib', b'8:'+A(self.fields+[('Status',S('COMPLETE'))])):
            with self.subTest(payload=payload[:16]):
                self.source.write_bytes(payload);self.expected=PROJECT.WIRE.sha(self.source)
                with self.assertRaises(RuntimeError):self.project()
                self.assertFalse(self.output.exists())

    def test_projection_limit_is_enforced(self):
        self.source_record()
        with self.assertRaises(RuntimeError):self.project(max_output=8)
        self.assertFalse(self.output.exists())

    def test_resource_bounds_cannot_be_increased(self):
        self.source_record()
        for options in ({'seconds':31},{'seconds':0},{'max_output':(64<<20)+1}):
            with self.subTest(options=options),self.assertRaises(ValueError):self.project(**options)

    def test_unavailable_compiler_is_explicit_without_fallback(self):
        self.source_record()
        with mock.patch.object(PROJECT.WIRE.WIRE.shutil,'which',return_value=None):
            with self.assertRaisesRegex(RuntimeError,'compiler'):
                PROJECT.project_export_input(self.source,self.output,expected_sha=self.expected,case_id='fixture',particles=self.particles)
        self.assertFalse(self.output.exists());self.assertEqual(self.source.read_bytes(),self.before)

    def test_native_symlink_is_rejected(self):
        self.source_record();link=self.root/'link.wxf';link.symlink_to(self.source)
        with self.assertRaisesRegex(ValueError,'symlinks'):
            PROJECT.project_export_input(link,self.output,expected_sha=self.expected,case_id='fixture',particles=self.particles,executable=self.tool)


class WorkerReaderStructureTests(unittest.TestCase):
    def test_only_projection_is_imported_and_admission_is_rechecked(self):
        source=(HERE/'evaluate.wl').read_text()
        self.assertNotIn('entries=Import[FileNameJoin[{outputDirectory,#<>".wxf"}],"WXF"]',source)
        self.assertIn('checkpoint=Quiet[Check[Import[payload,"WXF"],$Failed]]',source)
        self.assertIn('Native export inputs changed during bounded projection',source)
        self.assertIn('"CaseCapsulesSHA256"',source)
        self.assertIn('"ExportInputProjections"->exportInputProjections',source)
        self.assertIn('recordParticlesForExport[checkpoint,expected]=!=expected',source)


if __name__=='__main__':unittest.main()
