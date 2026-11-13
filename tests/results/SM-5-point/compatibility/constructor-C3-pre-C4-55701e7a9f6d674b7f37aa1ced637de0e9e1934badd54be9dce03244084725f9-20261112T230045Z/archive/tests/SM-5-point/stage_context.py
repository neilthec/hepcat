"""One exact exporter-only context bridge for the SM five-point workflow.

Native records and historical export reports retain their original contexts.
This module neither rewrites checkpoints nor runs physics or numerical exports.
"""
# HEPCAT - High Energy Physics Constructive Amplitude Tool
# Copyright (C) 2026 Neil Christensen
# SPDX-License-Identifier: GPL-3.0-or-later
from pathlib import Path
import hashlib
import json

EXPORTER = 'source/internal/HEPCAT-SPINAS.wl'
BRIDGE_NAME = 'exporter-context-bridge.json'


def digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def read_object(path):
    value = json.loads(Path(path).read_text())
    if not isinstance(value, dict):
        raise ValueError('Expected a JSON object.')
    return value


def manifest(root, here, configuration):
    root, here = Path(root), Path(here)
    paths = [path for path in (root / 'source').rglob('*')
             if path.is_file() and path.suffix in ('.wl', '.py', '.cpp', '.hpp', '.h')]
    paths += [root / 'models' / f'{stem}{number}.mdl'
              for number in (1, 2) for stem in ('amps', 'prtcls', 'vars', 'func', 'extlib')]
    paths += list((root / 'tests/reference-models/calchep').glob('*1.mdl'))
    paths += list((root / 'tests/reference-models/madgraph/sm').glob('*.py'))
    paths += [here / 'inventory.json']
    hashes = {str(path.relative_to(root)): digest(path)
              for path in sorted(paths) if path.is_file()}
    spinas = Path(configuration.get('SPINASDirectory', '/nonexistent'))
    runtime = list((spinas / 'include').rglob('*.h')) + list((spinas / 'include').rglob('*.hpp'))
    runtime += [path for path in spinas.rglob('libspinas*')
                if path.is_file() and path.suffix in ('.a', '.so', '.dylib')]
    for program in ('MadGraphDirectory', 'CalcHEPDirectory'):
        location = Path(configuration.get(program, '/nonexistent'))
        runtime += [location / 'VERSION', location / 'VERSION.txt']
    hashes.update({str(path): digest(path) for path in sorted(set(runtime)) if path.is_file()})
    programs = {key: str(configuration.get(key, '')) for key in
                ('SPINASDirectory', 'CalcHEPDirectory', 'MadGraphDirectory')}
    return {'Files': hashes, 'Programs': programs}


def fingerprint(value):
    return hashlib.sha256(json.dumps({'Files': value['Files'], 'Programs': value['Programs']},
                                     sort_keys=True).encode()).hexdigest()


def inside(root, relative):
    value = Path(relative)
    if value.is_absolute() or '..' in value.parts:
        raise ValueError('Bridge archives must be inside the checkout.')
    path = (Path(root) / value).resolve()
    path.relative_to(Path(root).resolve())
    return path


def _unchanged_except_exporter(before, after):
    return (before['Programs'] == after['Programs']
            and set(before['Files']) == set(after['Files'])
            and [name for name in before['Files']
                 if before['Files'][name] != after['Files'][name]] == [EXPORTER])


def verified_bridge(root, here, configuration, current_context):
    """Return this one reviewed transition only when every pinned byte matches."""
    try:
        path = Path(here) / BRIDGE_NAME
        bridge = read_object(path)
        if (bridge.get('SchemaVersion') != 1 or bridge.get('Kind') != 'ExactExporterOnlyTransition'
                or bridge.get('Status') != 'SEALED' or bridge.get('ChangedFile') != EXPORTER
                or bridge.get('ExporterContextSHA256') != current_context):
            return None
        before_path = inside(root, bridge['BeforeManifest'])
        after_path = inside(root, bridge['AfterManifest'])
        snapshot_path = inside(root, bridge['CheckpointSnapshot'])
        if (digest(before_path) != bridge['BeforeManifestSHA256']
                or digest(after_path) != bridge['AfterManifestSHA256']
                or digest(snapshot_path) != bridge['CheckpointSnapshotSHA256']):
            return None
        before, after = read_object(before_path), read_object(after_path)
        actual = manifest(root, here, configuration)
        if (fingerprint(before) != bridge['NativeContextSHA256']
                or fingerprint(after) != current_context or fingerprint(actual) != current_context
                or after['Files'] != actual['Files'] or after['Programs'] != actual['Programs']
                or not _unchanged_except_exporter(before, actual)
                or before['Files'][EXPORTER] != bridge['BeforeExporterSHA256']
                or actual['Files'][EXPORTER] != bridge['AfterExporterSHA256']):
            return None
        return {'Status': 'EXACT_EXPORTER_ONLY_COMPATIBILITY',
                'NativeContextSHA256': bridge['NativeContextSHA256'],
                'ExporterContextSHA256': current_context,
                'ChangedFile': EXPORTER, 'BeforeExporterSHA256': bridge['BeforeExporterSHA256'],
                'AfterExporterSHA256': bridge['AfterExporterSHA256'],
                'BeforeManifestSHA256': bridge['BeforeManifestSHA256'],
                'AfterManifestSHA256': bridge['AfterManifestSHA256'],
                'BridgeSHA256': digest(path), 'NativeRevalidationPerformed': False,
                '_checkpoints': read_object(snapshot_path)}
    except (OSError, ValueError, KeyError, TypeError):
        return None


def admits(recorded_context, current_context, proof):
    return recorded_context == current_context or bool(
        proof and proof.get('Status') == 'EXACT_EXPORTER_ONLY_COMPATIBILITY'
        and proof.get('NativeContextSHA256') == recorded_context
        and proof.get('ExporterContextSHA256') == current_context)


def public_proof(proof, stage):
    return {**{key: value for key, value in proof.items() if not key.startswith('_')}, 'Stage': stage}


def checkpoint_matches(summary_path, summary, proof):
    """Bind a finite old checkpoint, without claiming mutable project identity."""
    try:
        row = proof['_checkpoints'][Path(summary_path).name]
        return (row['RecordedContextSHA256'] == summary['ContextSHA256']
                and row['SummarySHA256'] == digest(summary_path)
                and row['CapsuleSHA256'] == summary.get('CapsuleSHA256'))
    except (OSError, KeyError, TypeError):
        return False


def seal(root, here, configuration, expected_exporter_sha):
    """Freeze the one pending reviewed transition; refuse a second migration."""
    path = Path(here) / BRIDGE_NAME
    bridge = read_object(path)
    if bridge.get('Status') != 'PENDING' or bridge.get('ChangedFile') != EXPORTER:
        raise ValueError('Only the reviewed pending exporter transition can be sealed.')
    before_path = inside(root, bridge['BeforeManifest'])
    snapshot_path = inside(root, bridge['CheckpointSnapshot'])
    before = read_object(before_path)
    if (digest(before_path) != bridge['BeforeManifestSHA256']
            or digest(snapshot_path) != bridge['CheckpointSnapshotSHA256']
            or fingerprint(before) != bridge['NativeContextSHA256']):
        raise ValueError('The prepatch archive changed.')
    after = manifest(root, here, configuration)
    if (not _unchanged_except_exporter(before, after)
            or after['Files'][EXPORTER] != expected_exporter_sha
            or before['Files'][EXPORTER] != bridge['BeforeExporterSHA256']):
        raise ValueError('Expected exactly the pinned exporter change; every other byte must match.')
    target = fingerprint(after)
    directory = Path(root) / 'tests/results/SM-5-point/compatibility' / ('postpatch-' + target)
    directory.mkdir(parents=True, exist_ok=True)
    after_path = directory / 'manifest.json'
    if after_path.exists():
        if read_object(after_path) != after:
            raise ValueError('The immutable postpatch archive differs.')
    else:
        after_path.write_text(json.dumps(after, indent=2, sort_keys=True) + '\n')
    bridge.update(Status='SEALED', ExporterContextSHA256=target,
                  AfterExporterSHA256=expected_exporter_sha,
                  AfterManifest=str(after_path.relative_to(root)), AfterManifestSHA256=digest(after_path))
    temporary = path.with_suffix('.json.tmp')
    temporary.write_text(json.dumps(bridge, indent=2, sort_keys=True) + '\n')
    temporary.replace(path)
    return bridge


def main():
    import argparse
    import importlib.util
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--seal-exporter-sha')
    parser.add_argument('--export-inputs')
    parser.add_argument('--configuration', type=Path)
    args = parser.parse_args()
    here = Path(__file__).resolve().parent
    root = here.parents[1]
    configuration_path = args.configuration or root / 'tests/results/SM-5-point/configuration.json'
    configuration = read_object(configuration_path)
    if args.seal_exporter_sha:
        print(json.dumps(seal(root, here, configuration, args.seal_exporter_sha), indent=2))
        return 0
    if args.export_inputs:
        specification = importlib.util.spec_from_file_location('hepcat_sm5_export_admission', here / 'run.py')
        workflow = importlib.util.module_from_spec(specification)
        specification.loader.exec_module(workflow)
        result = workflow.export_input_admission(args.export_inputs, configuration)
        print(json.dumps(result, indent=2))
        return 0 if result.get('Status') == 'APPROVED' else 2
    parser.error('Choose --seal-exporter-sha or --export-inputs.')


if __name__ == '__main__':
    raise SystemExit(main())
