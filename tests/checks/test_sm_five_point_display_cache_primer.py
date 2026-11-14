"""Exact display-token projection fixtures, without Wolfram or physics jobs."""
# HEPCAT - High Energy Physics Constructive Amplitude Tool
# Copyright (C) 2026 Neil Christensen
# SPDX-License-Identifier: GPL-3.0-or-later
import hashlib
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
spec = importlib.util.spec_from_file_location('display_cache_primer', HERE / 'prime-notebook-display-cache.py')
PRIMER = importlib.util.module_from_spec(spec); spec.loader.exec_module(PRIMER)
S = PRIMER.string_payload
A = PRIMER.association_payload
F = lambda name, args: b'f' + PRIMER.varint(len(args)) + b's' + PRIMER.varint(len(name.encode())) + name.encode() + b''.join(args)
L = lambda values: F('List', values)
I = lambda value: b'L' + struct.pack('<q', value)


def sdk():
    candidate = '/Applications/Wolfram.app/Contents/SystemFiles/Components/WolframClientForPython'
    if Path(candidate).is_dir(): sys.path.insert(0, candidate)
    try:
        from wolframclient.deserializers import binary_deserialize
        return binary_deserialize
    except ImportError:
        return None


SDK = sdk()


class ProfileTests(unittest.TestCase):
    def test_current_projection_is_exactly_reviewed(self):
        result = PRIMER.builder_profile(PRIMER.DEFAULT_BUILDER)
        self.assertEqual(result['ProjectionDefinitionSHA256'], PRIMER.PROJECTION_SHA256)
        self.assertEqual(result['BuilderSHA256'], PRIMER.sha(PRIMER.DEFAULT_BUILDER))

    def test_changed_projection_rejects_before_any_source_read(self):
        with tempfile.TemporaryDirectory() as directory:
            p = Path(directory) / 'builder.wl'
            p.write_text(PRIMER.DEFAULT_BUILDER.read_text().replace('"RevalidationPrints", "RevalidationSeconds"', '"NewRequiredDisplayField", "RevalidationSeconds"'))
            with self.assertRaisesRegex(ValueError, 'projection changed'):
                PRIMER.prime_record(Path(directory)/'missing.wxf', Path(directory)/'cache', builder=p)
            self.assertFalse((Path(directory)/'cache').exists())

    def test_bounded_string_and_payload_validation(self):
        self.assertEqual(PRIMER.literal_string(S('éà')), 'éà')
        for value in [b'I\1', b'S\x80', b'S\1ab']:
            with self.assertRaises(ValueError): PRIMER.literal_string(value)
        with tempfile.TemporaryDirectory() as directory:
            p=Path(directory)/'field.wxf';p.write_bytes(b'8C:'+zlib.compress(b'x'*100))
            with self.assertRaises(ValueError):PRIMER.small_payload(p, 20)


@unittest.skipUnless(shutil.which(os.environ.get('CXX','c++')), 'C++17 compiler required')
class ProjectionTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.compiler=PRIMER.WIRE.compiled_extractor();cls.tool,_=cls.compiler.__enter__()

    @classmethod
    def tearDownClass(cls): cls.compiler.__exit__(None,None,None)

    def setUp(self):
        self.temp=tempfile.TemporaryDirectory(prefix='hepcat-display-primer-test-')
        self.addCleanup(self.temp.cleanup);self.root=Path(self.temp.name);self.cache=self.root/'cache'
        self.seed=9007199254740997
        self.expression=F('ConstructiveDiagrams`SpinorProduct',[I(17),I(-2)])
        self.prints=L([F('Dataset',[A([('Checks',L([S('PASS')]))])]),S('Actual captured output')])
        self.cells=L([F('Cell',[S('Original table'),S('Print')])])
        self.options=A([('RandomSeed',I(self.seed)),('PolePreference',S('AllPhysical'))])
        self.ignored=F('UnneededLargeFitDiagnostic',[L([I(3)]*10000)])
        self.fields=[('ID',S('fixture')),('Status',S('COMPLETE')),('Seconds',b'r'+struct.pack('<d',1.25)),('Expression',self.expression),('Prints',self.prints),('NotebookCells',self.cells),('Details',A([('RequestedOptions',self.options),('HugeIgnoredStage',self.ignored)])),('Rows',self.ignored)]

    def source(self, fields=None, compressed=True):
        path=self.root/'fixture.wxf';body=A(self.fields if fields is None else fields)
        path.write_bytes(b'8C:'+zlib.compress(body) if compressed else b'8:'+body)
        self.summary={'ID':'fixture','Status':'COMPLETE','CapsuleSHA256':PRIMER.sha(path),'RandomSeed':self.seed}
        path.with_suffix('.json').write_text(json.dumps(self.summary))
        return path

    def prime(self,source,**options):
        return PRIMER.prime_record(source,self.cache,executable=self.tool,seconds=10,**options)

    def test_adoption_scope_and_actual_rows_copy_exactly(self):
        self.fields.extend([
            ('RecordKind', S('ValidatedFormulaAdoption')),
            ('ValidationAdoptionDisplay', A([('FreshHighEnergyReplayExecuted', b's\4True')])),
            ('AdoptionValidationTables', L([A([
                ('Rows', L([A([('Status', S('PASS'))])])),
                ('EvidenceScopes', L([S('Original native row; unchanged validator')]))])]))])
        source = self.source()
        self.assertEqual(self.prime(source)['Status'], 'PRIMED')
        expected = A([(key, value) for key, value in self.fields if key in PRIMER.FIELDS] +
                     [('Details', A([('RequestedOptions', self.options)]))])
        self.assertEqual(PRIMER.small_payload(self.cache / 'fixture.wxf', 1 << 20), expected)

    def test_native_exact_tokens_and_large_integer_seed(self):
        source=self.source();old=source.read_bytes();row=self.prime(source)
        self.assertEqual(row['Status'],'PRIMED');self.assertEqual(row['RecordedRandomSeed'],self.seed)
        self.assertFalse(row['FullCapsuleImported']);self.assertFalse(row['ExpressionsEvaluated'])
        self.assertEqual(source.read_bytes(),old)
        expected=A([(k,v)for k,v in self.fields if k in PRIMER.FIELDS]+[('Details',A([('RequestedOptions',self.options)]))])
        self.assertEqual(PRIMER.small_payload(self.cache/'fixture.wxf',1<<20),expected)
        manifest=json.loads((self.cache/'fixture.json').read_text())
        self.assertEqual(manifest['CapsuleSHA256'],self.summary['CapsuleSHA256'])
        self.assertEqual(manifest['PayloadSHA256'],PRIMER.sha(self.cache/'fixture.wxf'))
        self.assertNotIn('HugeIgnoredStage',str(row))
        if SDK:
            decoded=SDK((self.cache/'fixture.wxf').read_bytes())
            self.assertEqual(decoded['Details']['RequestedOptions']['RandomSeed'],self.seed)
            self.assertEqual(decoded['Prints'],SDK(b'8:'+self.prints))
            self.assertEqual(decoded['NotebookCells'],SDK(b'8:'+self.cells))
            self.assertEqual(decoded['Expression'],SDK(b'8:'+self.expression))
            self.assertNotIn('Rows',decoded)

    def test_uncompressed_record_without_optional_fields(self):
        source=self.source([('Status',S('COMPLETE')),('Expression',I(0)),('Prints',L([]))],False)
        row=self.prime(source);self.assertEqual(row['CapturedStatus'],'COMPLETE')
        self.assertEqual(PRIMER.small_payload(self.cache/'fixture.wxf',1000),A([('Status',S('COMPLETE')),('Expression',I(0)),('Prints',L([]))]))

    def test_export_only_retains_directory_and_captured_prints(self):
        fields=[('Status',S('PASS')),('Prints',self.prints),('Result',A([('Directory',S('/external/work/fixture')),('BuildDiagnostics',self.ignored)]))]
        source=self.source(fields);self.summary['Status']='PASS';source.with_suffix('.json').write_text(json.dumps(self.summary))
        row=self.prime(source)
        self.assertEqual(row['CapturedStatus'],'PASS')
        expected=A([('Status',S('PASS')),('Prints',self.prints),('Result',A([('Directory',S('/external/work/fixture'))]))])
        self.assertEqual(PRIMER.small_payload(self.cache/'fixture.wxf',1<<20),expected)

    def test_current_cache_reuses_and_tampered_payload_is_rebuilt(self):
        source=self.source();first=self.prime(source);second=self.prime(source)
        self.assertEqual(second['Status'],'REUSED')
        (self.cache/'fixture.wxf').write_bytes(b'bad')
        third=self.prime(source);self.assertEqual(third['Status'],'PRIMED')
        self.assertEqual(third['CachePayloadSHA256'],first['CachePayloadSHA256'])

    def test_builder_metadata_change_invalidates_existing_display_cache(self):
        source=self.source();self.prime(source)
        changed=self.root/'builder.wl'
        changed.write_text(PRIMER.DEFAULT_BUILDER.read_text()+'\n(* changed metadata display policy *)\n')
        second=PRIMER.prime_record(source,self.cache,builder=changed,executable=self.tool,seconds=10)
        self.assertEqual(second['Status'],'PRIMED')
        self.assertEqual(second['BuilderSHA256'],PRIMER.sha(changed))
        self.assertEqual(second['ProjectionDefinitionSHA256'],PRIMER.PROJECTION_SHA256)

    def test_status_failure_never_creates_a_success_cache(self):
        source=self.source([('Status',S('FAIL')),('Prints',self.prints),('Expression',self.expression)])
        with self.assertRaisesRegex(ValueError,'Only successful'):self.prime(source)
        self.assertFalse((self.cache/'fixture.wxf').exists())

    def test_failure_expression_and_missing_export_directory_reject(self):
        for fields in [[('Status',S('COMPLETE')),('Expression',F('System`Failure',[S('Timeout'),A([])])),('Prints',self.prints)],
                       [('Status',S('PASS')),('Result',A([])),('Prints',self.prints)]]:
            source=self.source(fields)
            with self.subTest(fields=fields[:1]),self.assertRaises(ValueError):self.prime(source)
            self.assertFalse((self.cache/'fixture.wxf').exists())

    def test_no_summary_fabricated_prints_or_cells(self):
        source=self.source([('Status',S('COMPLETE')),('Expression',self.expression)])
        with self.assertRaisesRegex(ValueError,'No actual captured'):self.prime(source)
        self.assertFalse((self.cache/'fixture.wxf').exists())

    def test_nonlist_prints_reject(self):
        source=self.source([('Status',S('COMPLETE')),('Expression',self.expression),('Prints',S('pretend table'))])
        with self.assertRaisesRegex(ValueError,'literal Lists'):self.prime(source)

    def test_summary_checksum_and_status_conflicts_reject(self):
        source=self.source();source.with_suffix('.json').write_text(json.dumps({**self.summary,'CapsuleSHA256':'0'*64}))
        with self.assertRaisesRegex(ValueError,'checksum'):self.prime(source)
        source.with_suffix('.json').write_text(json.dumps({**self.summary,'Status':'PASS'}))
        with self.assertRaisesRegex(ValueError,'status mismatch'):self.prime(source)

    def test_source_mutation_during_extraction_prevents_publication(self):
        source=self.source();original=PRIMER.WIRE.extract
        def changed(*args,**kwargs):
            result=original(*args,**kwargs);source.write_bytes(b'8:C\1');return result
        with mock.patch.object(PRIMER.WIRE,'extract',side_effect=changed):
            with self.assertRaisesRegex(ValueError,'changed during'):self.prime(source)
        self.assertFalse((self.cache/'fixture.wxf').exists())

    def test_output_budget_and_source_overwrite_guards(self):
        source=self.source()
        with self.assertRaises(RuntimeError):self.prime(source,max_output=5)
        with self.assertRaisesRegex(ValueError,'overwrite'):
            PRIMER.prime_record(source,source.parent,executable=self.tool)
        self.assertEqual(PRIMER.sha(source),self.summary['CapsuleSHA256'])

    def test_duplicate_association_keys_are_not_accepted(self):
        source=self.source(self.fields+[('Status',S('COMPLETE'))])
        with self.assertRaises(RuntimeError):self.prime(source)
        self.assertFalse((self.cache/'fixture.wxf').exists())
