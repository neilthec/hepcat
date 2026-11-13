#!/usr/bin/env python3
"""Copy only exact amplitude and identity fields for one admitted native export.

This reads WXF tokens with the bounded C++ streamer. It never imports/evaluates
an amplitude or manufactures a validation report. Original checkpoints stay intact.
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
import shutil
import tempfile
import zlib

HERE = Path(__file__).resolve().parent
_spec = importlib.util.spec_from_file_location('hepcat_export_projection_wire', HERE / 'prime-notebook-display-cache.py')
WIRE = importlib.util.module_from_spec(_spec); _spec.loader.exec_module(WIRE)
FIELDS = ('Expression', 'Status', 'Particles', 'Process', 'ID', 'ContextSHA256', 'RandomSeed')
MAX_OUTPUT = 64 << 20
MAX_SECONDS = 30


def require(value, message):
    if not value:
        raise ValueError(message)


def literal_particles(payload):
    """Read a short literal List of strings; never deserialize the expression."""
    position = 0
    def byte():
        nonlocal position
        require(position < len(payload), 'Truncated particle-list payload.')
        value = payload[position]; position += 1
        return value
    def number():
        value = shift = 0
        for _ in range(10):
            part = byte(); value |= (part & 127) << shift
            if part < 128:
                return value
            shift += 7
        raise ValueError('Invalid particle-list length.')
    def text(token):
        nonlocal position
        require(byte() == token, 'Particle identity must be a literal List of strings.')
        length = number(); require(length <= 1 << 20 and position + length <= len(payload), 'Invalid particle name length.')
        value = payload[position:position+length].decode('utf8'); position += length
        return value
    require(byte() == ord('f'), 'Particle identity must be a literal List of strings.')
    count = number(); require(1 <= count <= 64, 'Invalid external-particle count.')
    require(text(ord('s')) in ('List', 'System`List'), 'Particle identity must use List.')
    values = [text(ord('S')) for _ in range(count)]
    require(position == len(payload), 'Trailing particle-list tokens.')
    return values


def requests():
    return ([{'Mode': 'Index', 'Path': []}, {'Mode': 'Index', 'Path': ['Expression']}] +
            [{'Mode': 'Extract', 'Path': [key]} for key in FIELDS])


def project_export_input(source, output, *, expected_sha, case_id, particles,
                         expected_context=None, seconds=MAX_SECONDS,
                         max_output=MAX_OUTPUT, executable=None):
    require(isinstance(expected_sha, str) and re.fullmatch('[0-9a-f]{64}', expected_sha), 'An admitted source SHA256 is required.')
    require(isinstance(case_id, str) and re.fullmatch('[A-Za-z][A-Za-z0-9_-]*', case_id), 'Invalid case ID.')
    require(isinstance(particles, list) and particles and len(particles) <= 64 and all(isinstance(x, str) and x for x in particles), 'Exact inventory particles are required.')
    require(type(max_output) is int and 0 < max_output <= MAX_OUTPUT, 'Projection output limit must be at most 64 MiB.')
    require(type(seconds) in (int, float) and 0 < seconds <= MAX_SECONDS, 'Parser time limit must be at most 30 seconds.')
    if expected_context is not None:
        require(isinstance(expected_context, str) and re.fullmatch('[0-9a-f]{64}', expected_context), 'Invalid admitted native context.')
    source = Path(source).absolute(); output = Path(output).absolute()
    require(not source.is_symlink(), 'Native checkpoint symlinks are unsupported.')
    source = source.resolve(strict=True)
    require(not output.exists() and not output.is_symlink(), 'Projection output must be a new private directory.')
    require(source not in output.parents and output not in source.parents, 'Projection output cannot contain the native checkpoint.')
    before = WIRE.WIRE.file_identity(source)
    require(before['SHA256'] == expected_sha, 'Native checkpoint differs from its admitted SHA256.')
    output.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix='.export-input-', dir=output.parent) as temporary:
        temporary = Path(temporary)
        extraction = WIRE.WIRE.extract(source, temporary / 'fields', requests(), seconds=seconds,
                                       max_output=max_output, expected_sha=expected_sha, executable=executable)
        targets = extraction['Parser']['Targets']
        root = targets[0]; shape = targets[1]
        require(root.get('Found') and root.get('Kind') == 'Association', 'Native record must be an association.')
        require(all(x.get('Type') == 's' for x in root.get('Keys', [])), 'Native record keys must be literal strings.')
        require(shape.get('Found') and shape.get('Head', '').split('`')[-1] not in ('Failure', 'Missing'), 'A completed native Expression is required.')
        copied = {}
        for target in targets:
            if target['Mode'] == 0 and target['Found']:
                field = temporary / 'fields' / target['File']
                require(WIRE.sha(field) == target['SHA256'], 'Copied field checksum mismatch.')
                copied[target['Path'][0]] = WIRE.small_payload(field, max_output)
        require('Expression' in copied and 'Status' in copied, 'Native record is missing Expression or Status.')
        require(WIRE.literal_string(copied['Status']) == 'COMPLETE', 'Only COMPLETE native inputs may be exported.')
        aliases = [name for name in ('Particles', 'Process') if name in copied]
        require(aliases, 'Native record is missing Particles/Process identity.')
        for name in aliases:
            require(literal_particles(copied[name]) == particles, 'Native record external particles disagree with inventory (' + name + ').')
        if 'ID' in copied:
            require(WIRE.literal_string(copied['ID']) == case_id, 'Native record ID disagrees with admission.')
        if 'ContextSHA256' in copied:
            context = WIRE.literal_string(copied['ContextSHA256'])
            require(re.fullmatch('[0-9a-f]{64}', context) and (expected_context is None or context == expected_context), 'Native record context disagrees with admission.')
        require(WIRE.WIRE.file_identity(source) == before, 'Native checkpoint changed during projection.')
        payload = WIRE.association_payload([(key, copied[key]) for key in FIELDS if key in copied])
        require(len(payload) <= max_output, 'Export projection exceeds its bounded output size.')
        prepared = temporary / 'published'; prepared.mkdir()
        projection = prepared / 'input.wxf'; projection.write_bytes(b'8C:' + zlib.compress(payload))
        manifest = {'SchemaVersion': 1, 'Status': 'PROJECTED', 'CaseID': case_id,
                    'Source': str(source), 'SourceSHA256': expected_sha,
                    'SourceIdentity': before, 'Particles': particles, 'IdentityFields': aliases,
                    'PayloadSHA256': WIRE.sha(projection), 'UncompressedProjectionBytes': len(payload),
                    'CopiedFields': list(copied), 'FieldTokenSHA256': {key: hashlib.sha256(value).hexdigest() for key, value in copied.items()},
                    'ProjectorSHA256': WIRE.sha(__file__), 'ParserSourceSHA256': extraction['ToolSourceSHA256'],
                    'ParserToolSHA256': extraction['ToolSHA256'], 'ParserSeconds': extraction['Parser']['Seconds'],
                    'Limits': {'ParserSeconds': seconds, 'ParserResidentBytes': 512 << 20, 'ProjectionBytes': max_output},
                    'SourceUnchanged': True, 'FullNativeCheckpointImported': False, 'ExpressionsEvaluated': False,
                    'ValidationSynthesized': False}
        (prepared / 'manifest.json').write_text(json.dumps(manifest, indent=2) + '\n')
        require(WIRE.WIRE.file_identity(source) == before, 'Native checkpoint changed before projection publication.')
        require(not output.exists(), 'Projection output race.')
        os.rename(prepared, output)
    try:
        require(WIRE.WIRE.file_identity(source) == before, 'Native checkpoint changed after projection publication.')
    except BaseException:
        # This directory was created by our own successful rename, never reused.
        # A failed final source check must not leave a seemingly usable projection.
        if output.is_dir() and not output.is_symlink():
            shutil.rmtree(output)
        raise
    return {**manifest, 'Projection': str(output / 'input.wxf'), 'Manifest': str(output / 'manifest.json')}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--capsule', required=True, type=Path)
    parser.add_argument('--output', required=True, type=Path)
    parser.add_argument('--expected-sha', required=True)
    parser.add_argument('--case-id', required=True)
    parser.add_argument('--particles-json', required=True)
    parser.add_argument('--context')
    args = parser.parse_args()
    try:
        result = project_export_input(args.capsule, args.output, expected_sha=args.expected_sha,
                                      case_id=args.case_id, particles=json.loads(args.particles_json),
                                      expected_context=args.context)
        print(json.dumps(result))
        return 0
    except (ValueError, RuntimeError, OSError, KeyError, TypeError) as error:
        print(json.dumps({'Status': 'UNAVAILABLE', 'Error': str(error), 'FullNativeCheckpointImported': False}))
        return 2


if __name__ == '__main__':
    raise SystemExit(main())
