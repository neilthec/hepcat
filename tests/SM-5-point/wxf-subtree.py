#!/usr/bin/env python3
"""Copy exact WXF subtrees without evaluating or loading a Wolfram package."""
# HEPCAT - High Energy Physics Constructive Amplitude Tool
# Copyright (C) 2026 Neil Christensen
# SPDX-License-Identifier: GPL-3.0-or-later

from __future__ import annotations

import argparse
from contextlib import contextmanager
import hashlib
import json
import math
import os
from pathlib import Path
import resource
import shutil
import struct
import subprocess
import tempfile
import time

HERE = Path(__file__).resolve().parent
SOURCE = HERE / 'wxf-subtree.cpp'
REQUEST_BYTES = 1 << 20


def sha256(path):
    digest = hashlib.sha256()
    with Path(path).open('rb') as stream:
        for chunk in iter(lambda: stream.read(1 << 20), b''):
            digest.update(chunk)
    return digest.hexdigest()


def file_identity(path):
    info = path.stat()
    return {
        'Bytes': info.st_size, 'Device': info.st_dev, 'Inode': info.st_ino,
        'MTimeNS': info.st_mtime_ns, 'SHA256': sha256(path),
    }


def path_segment(value):
    if isinstance(value, str):
        return b's', value
    if type(value) is int:
        return b'i', str(value)
    if isinstance(value, dict):
        if set(value) == {'symbol'} and isinstance(value['symbol'], str):
            return b'y', value['symbol']
        if set(value) == {'arg'} and type(value['arg']) is int and value['arg'] >= 0:
            return b'a', str(value['arg'])
        if set(value) == {'head'} and value['head'] is True:
            return b'h', ''
    raise ValueError('invalid path segment')


def requests_bytes(requests):
    if not isinstance(requests, list) or not 1 <= len(requests) <= 512:
        raise ValueError('provide between 1 and 512 requests')
    data = bytearray(struct.pack('<I', len(requests)))
    for request in requests:
        if not isinstance(request, dict) or set(request) != {'Mode', 'Path'}:
            raise ValueError('each request must contain exactly Mode and Path')
        if request['Mode'] not in ('Extract', 'Index'):
            raise ValueError('Mode must be Extract or Index')
        path = request['Path']
        if not isinstance(path, list) or len(path) > 256:
            raise ValueError('path must be a list with at most 256 segments')
        data.extend(bytes([int(request['Mode'] == 'Index')]))
        data.extend(struct.pack('<I', len(path)))
        for position, value in enumerate(path):
            kind, text = path_segment(value)
            if kind == b'h' and position != len(path) - 1:
                raise ValueError('head selection must be terminal')
            text = text.encode('utf8')
            if len(text) > REQUEST_BYTES:
                raise ValueError('request string bound')
            data.extend(kind + struct.pack('<I', len(text)) + text)
            if len(data) > REQUEST_BYTES:
                raise ValueError('request byte bound')
    return bytes(data)


def validate_limits(seconds, max_bytes, max_tokens, max_depth, max_output):
    if isinstance(seconds, bool) or not isinstance(seconds, (int, float)):
        raise ValueError('seconds must be a finite positive number')
    if not 0 < seconds <= 86400 or not math.isfinite(seconds):
        raise ValueError('seconds must be finite and between 0 and 86400')
    for name, value in [('max_bytes', max_bytes), ('max_tokens', max_tokens),
                        ('max_output', max_output)]:
        if type(value) is not int or not 0 < value <= (1 << 63) - 1:
            raise ValueError(name + ' must be a positive bounded integer')
    if type(max_depth) is not int or not 1 <= max_depth <= 4096:
        raise ValueError('max_depth must be an integer from 1 through 4096')


@contextmanager
def compiled_extractor(executable=None):
    """Compile in an owned temporary directory, or use an explicit existing tool."""
    if executable is not None:
        tool = Path(executable).resolve(strict=True)
        yield tool, {'Prebuilt': True}
        return
    compiler = shutil.which(os.environ.get('CXX', 'c++'))
    if compiler is None:
        raise RuntimeError('a C++17 compiler and zlib development headers are required')
    with tempfile.TemporaryDirectory(prefix='hepcat-wxf-build-') as directory:
        tool = Path(directory) / 'wxf-subtree'
        command = [compiler, '-std=c++17', '-O3', '-Wall', '-Wextra', '-Wpedantic',
                   str(SOURCE), '-lz', '-o', str(tool)]
        build = subprocess.run(command, capture_output=True, text=True, timeout=120)
        if build.returncode:
            raise RuntimeError('extractor compilation failed: ' + build.stderr.strip())
        version = subprocess.run([compiler, '--version'], capture_output=True,
                                 text=True, timeout=10)
        yield tool, {'Prebuilt': False, 'Command': command,
                     'Version': version.stdout.splitlines()[:2]}


def validate_parser_result(result, requests):
    targets = result.get('Targets')
    if result.get('CompleteParse') is not True or not isinstance(targets, list):
        raise RuntimeError('missing complete parser result')
    if len(targets) != len(requests):
        raise RuntimeError('parser target count mismatch')
    for index, (target, request) in enumerate(zip(targets, requests)):
        expected_mode = int(request['Mode'] == 'Index')
        if (not isinstance(target, dict) or type(target.get('Index')) is not int or
                target['Index'] != index or type(target.get('Mode')) is not int or
                target['Mode'] != expected_mode or type(target.get('Found')) is not bool):
            raise RuntimeError('parser target protocol mismatch')


def extract(source, output, requests, *, seconds=180, max_bytes=16 << 30,
            max_tokens=2_000_000_000, max_depth=1024, max_output=64 << 20,
            expected_sha=None, executable=None):
    """Return a manifest. A failed/changed read is recorded as UNRESOLVED."""
    validate_limits(seconds, max_bytes, max_tokens, max_depth, max_output)
    encoded_requests = requests_bytes(requests)
    source = Path(source).resolve(strict=True)
    output = Path(output).absolute()
    if output.exists():
        raise ValueError('output already exists')
    before = file_identity(source)
    if expected_sha is not None and before['SHA256'] != expected_sha:
        raise ValueError('input SHA mismatch')
    output.parent.mkdir(parents=True, exist_ok=True)
    stage = Path(tempfile.mkdtemp(prefix='.' + output.name + '-', dir=output.parent))
    (stage / 'requests.bin').write_bytes(encoded_requests)
    started = time.monotonic()
    manifest = {
        'SchemaVersion': 1, 'Status': 'UNRESOLVED', 'ExpressionsEvaluated': False,
        'Source': str(source), 'SourceBefore': before, 'Requests': requests,
        'Limits': {'Seconds': seconds, 'UncompressedBytes': max_bytes,
                   'Tokens': max_tokens, 'Depth': max_depth, 'OutputBytes': max_output,
                   'ResidentMemoryCheckBytes': 512 << 20, 'IndexMetadataBytes': 16 << 20},
        'ToolSourceSHA256': sha256(SOURCE), 'WrapperSHA256': sha256(__file__),
        'FormatReference': 'WolframClientForPython WXF constants.py and wxfparser.py',
    }
    try:
        with compiled_extractor(executable) as (tool, build):
            command = [str(tool), str(source), str(stage / 'requests.bin'), str(stage),
                       str(seconds), str(max_bytes), str(max_tokens), str(max_depth),
                       str(max_output)]
            manifest.update(Command=command, Build=build, ToolSHA256=sha256(tool))

            def cpu_limit():
                # macOS does not enforce RLIMIT_AS. The parser bounds allocations
                # and checks getrusage RSS; the parent adds a finite wall deadline.
                resource.setrlimit(resource.RLIMIT_CPU,
                                   (max(1, int(seconds) + 3), max(2, int(seconds) + 5)))

            process = subprocess.run(command, capture_output=True, text=True,
                                     timeout=seconds + 10, preexec_fn=cpu_limit)
        (stage / 'extractor.stdout').write_text(process.stdout)
        (stage / 'extractor.stderr').write_text(process.stderr)
        manifest['ExitCode'] = process.returncode
        if process.returncode:
            raise RuntimeError('extractor rejected input: ' + process.stderr.strip())
        result = json.loads(process.stdout)
        validate_parser_result(result, requests)
        after = file_identity(source)
        manifest['SourceAfter'] = after
        if after != before:
            raise RuntimeError('input changed during read')
        manifest.update(SourceUnchanged=True, Parser=result)
        for target, request in zip(result['Targets'], requests):
            target['Path'] = request['Path']
            if request['Mode'] == 'Extract' and target['Found']:
                path = stage / ('extract-' + str(target['Index']) + '.wxf')
                target.update(File=path.name, Bytes=path.stat().st_size, SHA256=sha256(path))
        manifest['Status'] = ('COMPLETE' if all(x['Found'] for x in result['Targets'])
                              else 'COMPLETE_WITH_MISSING_PATHS')
    except Exception as error:
        manifest['Error'] = str(error)
        try:
            manifest['SourceAfter'] = file_identity(source)
            manifest['SourceUnchanged'] = manifest['SourceAfter'] == before
        except OSError:
            manifest['SourceUnchanged'] = False
    manifest['WallSeconds'] = time.monotonic() - started
    (stage / 'manifest.json').write_text(json.dumps(manifest, indent=2, ensure_ascii=False) + '\n')
    if output.exists():
        raise RuntimeError('output path race; private stage retained: ' + str(stage))
    os.rename(stage, output)
    if manifest['Status'] == 'UNRESOLVED':
        raise RuntimeError(manifest.get('Error', 'unresolved') +
                           '; diagnostic ' + str(output / 'manifest.json'))
    return manifest


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('input')
    parser.add_argument('output')
    parser.add_argument('--extract', action='append', default=[], metavar='JSON_PATH')
    parser.add_argument('--index', action='append', default=[], metavar='JSON_PATH')
    parser.add_argument('--requests', help='JSON list of Mode/Path objects')
    parser.add_argument('--expected-sha')
    parser.add_argument('--seconds', type=float, default=180)
    parser.add_argument('--max-output', type=int, default=64 << 20)
    parser.add_argument('--max-bytes', type=int, default=16 << 30)
    parser.add_argument('--max-depth', type=int, default=1024)
    parser.add_argument('--max-tokens', type=int, default=2_000_000_000)
    parser.add_argument('--executable', help='use an already compiled extractor')
    args = parser.parse_args()
    requests = json.loads(Path(args.requests).read_text()) if args.requests else []
    requests += [{'Mode': 'Extract', 'Path': json.loads(x)} for x in args.extract]
    requests += [{'Mode': 'Index', 'Path': json.loads(x)} for x in args.index]
    try:
        manifest = extract(args.input, args.output, requests, seconds=args.seconds,
                           max_output=args.max_output, max_bytes=args.max_bytes,
                           max_depth=args.max_depth, max_tokens=args.max_tokens,
                           expected_sha=args.expected_sha, executable=args.executable)
        print(json.dumps({'Status': manifest['Status'], 'Manifest': str(Path(args.output) / 'manifest.json'),
                          'Seconds': manifest['Parser']['Seconds'],
                          'Targets': manifest['Parser']['Targets']}, ensure_ascii=False))
        return 0
    except (ValueError, RuntimeError, OSError) as error:
        parser.exit(1, str(error) + '\n')


if __name__ == '__main__':
    raise SystemExit(main())
