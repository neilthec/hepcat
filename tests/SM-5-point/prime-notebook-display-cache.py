#!/usr/bin/env python3
"""Prime display-only caches by copying WXF subtrees, without Wolfram evaluation.

Only COMPLETE native records and PASS export records are supported. Captured
formulas, prints and notebook cells are copied literally. This never establishes
validation, changes a capsule, or writes a notebook. Failed records stay uncached.
"""
# HEPCAT - High Energy Physics Constructive Amplitude Tool
# Copyright (C) 2026 Neil Christensen
# SPDX-License-Identifier: GPL-3.0-or-later
from __future__ import annotations
import argparse
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import re
import tempfile
import zlib

HERE = Path(__file__).resolve().parent
DEFAULT_BUILDER = HERE / 'build-notebook.wl'
PROJECTION_SHA256 = '18797cea3252329779b98f981a9229d474110d712a80f8d6498636ff2aa0a3a9'
FIELDS = ('ID', 'Status', 'Seconds', 'Expression', 'Prints', 'NotebookCells',
          'RecordKind', 'ValidationAdoptionDisplay', 'AdoptionValidationTables',
          'RevalidationPrints', 'RevalidationSeconds', 'RevalidationCheckTimeConstraint')
_spec = importlib.util.spec_from_file_location('hepcat_display_wxf_subtree', HERE / 'wxf-subtree.py')
WIRE = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(WIRE)


def require(value, message):
    if not value:
        raise ValueError(message)


def sha(path):
    return WIRE.sha256(path)


def builder_profile(builder):
    text = Path(builder).read_text()
    begin = 'sm5NotebookDisplayProjection[record_Association] :='
    end = '\nsm5NotebookDisplayWrite['
    require(text.count(begin) == 1 and text.count(end) == 1, 'Unknown builder projection definition.')
    definition = text.split(begin, 1)[1].split(end, 1)[0]
    digest = hashlib.sha256(definition.encode()).hexdigest()
    require(digest == PROJECTION_SHA256, 'Builder projection changed; review the primer before reuse.')
    require('sm5NotebookDisplayCacheSchema = 2;' in text, 'Unsupported cache schema.')
    return {'BuilderSHA256': sha(builder), 'ProjectionDefinitionSHA256': digest, 'SchemaVersion': 2}


def varint(number):
    result = bytearray()
    while number >= 128:
        result.append((number & 127) | 128)
        number >>= 7
    return bytes(result) + bytes([number])


def string_payload(value):
    data = value.encode('utf8')
    return b'S' + varint(len(data)) + data


def association_payload(entries):
    return b'A' + varint(len(entries)) + b''.join(b'-' + string_payload(k) + v for k, v in entries)


def small_payload(path, limit):
    data = Path(path).read_bytes()
    if data.startswith(b'8C:'):
        stream = zlib.decompressobj()
        result = stream.decompress(data[3:], limit + 1)
        require(len(result) <= limit and stream.eof and not stream.unused_data
                and not stream.unconsumed_tail, 'Copied field exceeds the bounded WXF payload.')
        return result
    require(data.startswith(b'8:') and len(data) - 2 <= limit, 'Unsupported copied-field WXF.')
    return data[2:]


def literal_string(payload):
    require(payload[:1] == b'S', 'Expected a literal WXF string.')
    number = shift = 0
    position = 1
    while position < len(payload) and shift < 64:
        byte = payload[position]; position += 1
        number |= (byte & 127) << shift
        if byte < 128:
            require(len(payload) == position + number, 'Invalid literal string length.')
            return payload[position:].decode('utf8')
        shift += 7
    raise ValueError('Invalid literal string length.')


def requests():
    return ([{'Mode': 'Index', 'Path': []}] +
            [{'Mode': 'Extract', 'Path': [key]} for key in FIELDS] +
            [{'Mode': 'Extract', 'Path': ['Details', 'RequestedOptions']},
             {'Mode': 'Extract', 'Path': ['Result', 'Directory']}] +
            [{'Mode': 'Index', 'Path': [key]} for key in ('Expression', 'Prints', 'NotebookCells', 'Result')])


def project(stage, manifest, limit):
    """Construct only the reviewed KeyTake/Details/Result display projection."""
    targets = manifest['Parser']['Targets']
    indexed = {tuple(item['Path']): item for item in targets if item['Mode'] == 1}
    root = indexed[()]
    require(root.get('Found') and root.get('Kind') == 'Association', 'Captured record is not an association.')
    keys = root.get('Keys', [])
    require(all(item.get('Type') == 's' for item in keys), 'Non-string record keys are unsupported.')
    names = {item['Value'] for item in keys}
    copied = {}
    for item in targets:
        if item['Mode'] == 0 and item['Found']:
            path = Path(stage) / item['File']
            require(sha(path) == item['SHA256'], 'Copied field checksum mismatch.')
            copied[tuple(item['Path'])] = small_payload(path, limit)
    require(('Status',) in copied, 'Missing captured status.')
    status = literal_string(copied[('Status',)])
    require(status in ('COMPLETE', 'PASS'), 'Only successful records are supported; failure display needs the builder.')
    for key in ('Expression', 'Result'):
        shape = indexed.get((key,), {})
        head = shape.get('Head', '')
        require(not (isinstance(head, str) and head.split('`')[-1] == 'Failure'),
                'Failure-valued successful record is unsupported.')
    if status == 'COMPLETE':
        require(('Expression',) in copied, 'No captured amplitude expression.')
    else:
        require('Result' in names, 'No captured export result.')
    captured = []
    for key in ('Prints', 'NotebookCells'):
        if key in names:
            shape = indexed[(key,)]
            require(shape.get('Kind') == 'List', 'Captured prints/cells must be literal Lists.')
            captured.append(key)
    require(captured, 'No actual captured prints or notebook cells; never construct them from summaries.')
    entries = [(key, copied[(key,)]) for key in FIELDS if (key,) in copied]
    if ('Details', 'RequestedOptions') in copied:
        entries.append(('Details', association_payload([('RequestedOptions', copied[('Details', 'RequestedOptions')])])))
    if 'Result' in names:
        require(indexed[('Result',)].get('Kind') == 'Association' and ('Result', 'Directory') in copied,
                'Export Result must contain its original Directory.')
        literal_string(copied[('Result', 'Directory')])
        entries.append(('Result', association_payload([('Directory', copied[('Result', 'Directory')])])))
    payload = association_payload(entries)
    require(len(payload) <= limit, 'Display projection exceeds the output budget.')
    return payload, status, captured, {str(k): hashlib.sha256(v).hexdigest() for k, v in copied.items()}


def prime_record(source, cache_dir, *, builder=DEFAULT_BUILDER, summary=None,
                 expected_sha=None, seconds=30, max_output=8 << 20, executable=None):
    profile = builder_profile(builder)
    source = Path(source).resolve(strict=True); cache_dir = Path(cache_dir).absolute()
    before = WIRE.file_identity(source)
    require(re.fullmatch(r'[A-Za-z][A-Za-z0-9_-]*', source.stem), 'Unsafe record basename.')
    summary_path = Path(summary) if summary is not None else source.with_suffix('.json')
    summary_hash = None; summary_data = {}
    if summary_path.exists():
        summary_hash = sha(summary_path); summary_data = json.loads(summary_path.read_text())
        require(isinstance(summary_data, dict), 'Summary is not an object.')
        if expected_sha is None:
            expected_sha = summary_data.get('CapsuleSHA256')
    require(isinstance(expected_sha, str) and expected_sha == before['SHA256'], 'Expected capsule checksum is required and must match.')
    payload_path = cache_dir / (source.stem + '.wxf'); manifest_path = cache_dir / (source.stem + '.json')
    require(payload_path.resolve() != source and manifest_path.resolve() != summary_path.resolve(), 'Cache would overwrite a captured record.')
    require(not cache_dir.is_symlink() and not payload_path.is_symlink() and not manifest_path.is_symlink(), 'Cache symlinks are unsupported.')
    expected = {'SchemaVersion': 2, 'CapsuleSHA256': expected_sha,
                'DisplaySourceSHA256': profile['BuilderSHA256']}
    if manifest_path.is_file() and payload_path.is_file():
        old = json.loads(manifest_path.read_text())
        if all(old.get(k) == v for k, v in expected.items()) and old.get('PayloadSHA256') == sha(payload_path):
            return {'Status': 'REUSED', 'Source': str(source), **profile,
                    'CapsuleSHA256': expected_sha, 'CachePayloadSHA256': old['PayloadSHA256']}
    cache_dir.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix='.display-primer-', dir=cache_dir) as scratch:
        stage = Path(scratch) / 'fields'
        record = WIRE.extract(source, stage, requests(), seconds=seconds,
                              max_output=max_output, expected_sha=expected_sha, executable=executable)
        payload, status, captured, field_hashes = project(stage, record, max_output)
        if summary_data:
            require(summary_data.get('Status') == status, 'Captured/summary status mismatch.')
        require(WIRE.file_identity(source) == before and builder_profile(builder) == profile,
                'Capsule or builder changed during the read.')
        if summary_hash is not None:
            require(sha(summary_path) == summary_hash, 'Summary changed during the read.')
        encoded = b'8C:' + zlib.compress(payload)
        temporary_payload = Path(scratch) / 'cache.wxf'; temporary_payload.write_bytes(encoded)
        manifest = {**expected, 'PayloadSHA256': sha(temporary_payload)}
        temporary_manifest = Path(scratch) / 'cache.json'
        temporary_manifest.write_text(json.dumps(manifest, indent=2) + '\n')
        os.replace(temporary_payload, payload_path)
        os.replace(temporary_manifest, manifest_path)
    require(WIRE.file_identity(source) == before, 'Capsule changed after cache publication; cache is not reusable.')
    return {'Status': 'PRIMED', 'Source': str(source), 'CapturedStatus': status, **profile,
            'CapsuleSHA256': expected_sha, 'CachePayloadSHA256': manifest['PayloadSHA256'],
            'CacheBytes': payload_path.stat().st_size, 'ProjectionBytes': len(payload),
            'CapturedDisplayFields': captured, 'FieldTokenSHA256': field_hashes,
            'SummarySHA256': summary_hash, 'RecordedRandomSeed': summary_data.get('RandomSeed'),
            'ExpressionsEvaluated': False, 'FullCapsuleImported': False,
            'ParserToolSHA256': record['ToolSHA256'], 'ParserSourceSHA256': record['ToolSourceSHA256']}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('capsules', nargs='+', type=Path)
    parser.add_argument('--cache-dir', required=True, type=Path)
    parser.add_argument('--builder', default=DEFAULT_BUILDER, type=Path)
    parser.add_argument('--audit', required=True, type=Path)
    parser.add_argument('--seconds', default=30, type=float)
    parser.add_argument('--max-output', default=8 << 20, type=int)
    args = parser.parse_args(); records = []; code = 0
    require(not args.audit.exists(), 'Audit path must be new.')
    with WIRE.compiled_extractor() as (tool, _):
        for source in args.capsules:
            try:
                row = prime_record(source, args.cache_dir, builder=args.builder,
                                   seconds=args.seconds, max_output=args.max_output, executable=tool)
            except (ValueError, RuntimeError, OSError, KeyError) as error:
                row = {'Status': 'UNSUPPORTED_OR_UNRESOLVED', 'Source': str(source), 'Reason': str(error)}; code = 1
            records.append(row); print(json.dumps(row), flush=True)
    args.audit.parent.mkdir(parents=True, exist_ok=True)
    args.audit.write_text(json.dumps({'SchemaVersion': 1, 'Status': 'ALL_PRIMED_OR_REUSED' if code == 0 else 'INCOMPLETE',
        'Records': records, 'ExpressionsEvaluated': False, 'FullCapsulesImported': False,
        'NoNotebookWritten': True, 'NoValidationClaimed': True}, indent=2) + '\n')
    return code


if __name__ == '__main__':
    raise SystemExit(main())
