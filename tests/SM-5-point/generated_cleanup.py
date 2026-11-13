"""Remove completed SM5 generated caches, preserving canonical numerical evidence.

Call only with fresh run.checked_summary/refresh records. Precision-qualified
raw failures are deliberately retained until their verifier supports relocation.
"""
# HEPCAT - High Energy Physics Constructive Amplitude Tool
# Copyright (C) 2026 Neil Christensen
# SPDX-License-Identifier: GPL-3.0-or-later
from __future__ import annotations
import contextlib
import datetime as dt
import fcntl
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import re
import shutil
import stat
import subprocess
import sys
import uuid


class CleanupRefused(ValueError):
    pass


def _sha(path):
    value = hashlib.sha256()
    with path.open('rb') as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b''):
            value.update(block)
    return value.hexdigest()


def _json(path):
    value = json.loads(path.read_text())
    if not isinstance(value, dict):
        raise CleanupRefused('Expected an object checkpoint.')
    return value


def _identifier(value):
    return isinstance(value, str) and re.fullmatch(r'[A-Za-z_][A-Za-z_0-9]*', value)


def _beneath(base, path):
    path = Path(path).absolute()
    if '..' in path.parts:
        raise CleanupRefused('Path traversal is not allowed.')
    relative = path.relative_to(base)
    current = base
    for part in relative.parts:
        current = current / part
        if current.is_symlink():
            raise CleanupRefused('A root or ancestor is a symlink.')
    return path


def _atomic(path, value):
    temporary = path.with_name(path.name + '.' + uuid.uuid4().hex + '.tmp')
    with temporary.open('x') as stream:
        json.dump(value, stream, indent=2)
        stream.write('\n')
        stream.flush()
        os.fsync(stream.fileno())
    temporary.replace(path)


@contextlib.contextmanager
def _lock(results, borrowed):
    path = _beneath(results, results / 'run.lock')
    owned = borrowed is None
    stream = borrowed if not owned else os.fdopen(os.open(path, os.O_RDWR | os.O_CREAT | os.O_NOFOLLOW, 0o600), 'r+')
    try:
        if (os.fstat(stream.fileno()).st_dev, os.fstat(stream.fileno()).st_ino) != (path.stat().st_dev, path.stat().st_ino):
            raise CleanupRefused('Runner lock has a different identity.')
        try:
            fcntl.flock(stream.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError as error:
            raise CleanupRefused('A campaign runner is active.') from error
        yield stream
    finally:
        if owned:
            stream.close()


def _active_jobs(root, target):
    result = subprocess.run(['ps', '-axo', 'pid=,command='], capture_output=True, text=True, check=True)
    workers = [str(root / 'tests/SM-5-point' / name) for name in ('evaluate.wl', 'revalidate.wl')]
    found = []
    for line in result.stdout.splitlines():
        parts = line.strip().split(None, 1)
        if len(parts) == 2 and int(parts[0]) != os.getpid() and (
                str(target) in parts[1] or any(name in parts[1] for name in workers)):
            found.append(int(parts[0]))
    return found


def _scan(target):
    rows = []
    for parent, directories, filenames in os.walk(target, followlinks=False):
        for name in sorted(directories + filenames):
            path = Path(parent) / name
            info = path.lstat()
            if stat.S_ISLNK(info.st_mode):
                kind = 'Symlink'  # Never traverse or open the target.
            elif stat.S_ISDIR(info.st_mode):
                kind = 'Directory'
            elif stat.S_ISREG(info.st_mode):
                kind = 'File'
            else:
                raise CleanupRefused('A generated tree contains a special file.')
            row = {'Path': str(path.relative_to(target)), 'Kind': kind, 'Bytes': info.st_size,
                   'Device': info.st_dev, 'Inode': info.st_ino, 'ModifiedNS': info.st_mtime_ns}
            if kind == 'Symlink':
                row['LinkTarget'] = os.readlink(path)
            rows.append(row)
    return sorted(rows, key=lambda row: row['Path'])


def _evidence(name):
    path = Path(name)
    lower = path.name.lower()
    raw_suffixes = {'.json', '.jsonl', '.csv', '.log', '.wxf'}
    suffixes = [part.lower() for part in path.suffixes]
    structured = bool(suffixes) and (suffixes[-1] in raw_suffixes or
        (len(suffixes) > 1 and suffixes[-1] in {'.gz', '.bz2', '.xz', '.zst', '.zip'}
         and suffixes[-2] in raw_suffixes))
    return (structured
            or lower in {'param_card.dat', 'run_card.dat'}
            or any(word in lower for word in ('failedpoint', 'failed-point', 'failure', 'replay', 'residual', 'report')))


def _binding(results, family, cases, export):
    members = family.get('CaseIDs')
    if (not isinstance(members, list) or not members or len(set(members)) != len(members)
            or not all(_identifier(cid) for cid in members) or export.get('CaseIDs') != members):
        raise CleanupRefused('Export member identities differ.')
    hashes = {}; contexts = {}; files = {}
    for cid in members:
        case = cases.get(cid, {})
        if (case.get('ID') != cid or case.get('Status') != 'COMPLETE'
                or type(case.get('Rows')) is not int or case['Rows'] < 1
                or type(case.get('FeynmanRows')) is not int or not 1 <= case['FeynmanRows'] <= case['Rows']
                or case.get('CheckStatuses') != {'PASS': case['Rows']}
                or case.get('SupplementalParameterProbes', {}).get('Status', 'PASS') != 'PASS'):
            raise CleanupRefused('A native or supplemental check is incomplete.')
        hashes[cid] = case.get('CapsuleSHA256'); contexts[cid] = case.get('ContextSHA256')
        for suffix in ('.json', '.wxf'):
            path = _beneath(results, results / (cid + suffix))
            if not path.is_file(): raise CleanupRefused('A native checkpoint is unavailable.')
            if suffix == '.json' and any(case.get(k) != v for k, v in _json(path).items()):
                raise CleanupRefused('The checked native summary differs from disk.')
            files[str(path)] = _sha(path)
        if files[str(results / (cid + '.wxf'))] != hashes[cid]:
            raise CleanupRefused('A native capsule checksum differs.')
    for suffix in ('-export.json', '-export.wxf'):
        path = _beneath(results, results / (family['ID'] + suffix))
        if not path.is_file(): raise CleanupRefused('The export checkpoint is unavailable.')
        if suffix.endswith('.json') and any(export.get(k) != v for k, v in _json(path).items()):
            raise CleanupRefused('The checked export summary differs from disk.')
        files[str(path)] = _sha(path)
    mixed = any(value != export.get('ContextSHA256') for value in contexts.values())
    if (files[str(results / (family['ID'] + '-export.wxf'))] != export.get('CapsuleSHA256')
            or export.get('CaseCapsulesSHA256') != hashes
            or (('NativeCaseContexts' in export or mixed) and export.get('NativeCaseContexts') != contexts)
            or (('ExporterContextSHA256' in export or mixed)
                and export.get('ExporterContextSHA256') != export.get('ContextSHA256'))):
        raise CleanupRefused('Export capsule, context or native input bindings differ.')
    return files


def cleanup_completed_family(root, results, family, case_summaries, export_summary, *,
                             active_ids=(), dry_run=False, runner_lock=None, required_points=1000):
    """Return CLEANED/PLANNED/ALREADY_ABSENT or RETAINED with a reason.

    Inputs must be fresh checked_summary/refresh records, including supplemental
    admission. Historical contexts are retained exactly. Pass the runner's held
    run.lock file to avoid reacquiring it; otherwise an exclusive lock is taken.
    Canonical artifacts, precision data and ledgers are never removed.
    """
    identifier = family.get('ID') if isinstance(family, dict) else None
    base = {'FamilyID': identifier, 'Status': 'RETAINED'}
    quarantine = None; journal_path = None
    try:
        if type(required_points) is not int or required_points < 1:
            raise CleanupRefused('Required points must be positive.')
        root = Path(root).resolve(strict=True)
        results = _beneath(root, Path(results))
        if results != root / 'tests/results/SM-5-point':
            raise CleanupRefused('Only the canonical SM5 results directory is supported.')
        name = family.get('ExportName')
        if not _identifier(identifier) or not _identifier(name):
            raise CleanupRefused('Invalid family or export directory identity.')
        export = export_summary if isinstance(export_summary, dict) else {}
        if (export.get('FamilyID') != identifier or export.get('Status') != 'PASS'
                or export.get('CalcHEPStatus') != 'PASS' or export.get('MadGraphStatus') != 'PASS'
                or type(export.get('Points')) is not int or export['Points'] < required_points
                or export.get('SupplementalParameterProbeStatus', 'PASS') != 'PASS'):
            raise CleanupRefused('Strict native/CalcHEP/MadGraph completion required; precision roots are retained.')
        if active_ids is None or identifier in active_ids or any(cid in active_ids for cid in family['CaseIDs']):
            raise CleanupRefused('The family or a member is active.')
        target = _beneath(root, root / 'tests/results/spinas-output/SM-5-point' / name)
        with _lock(results, runner_lock):
            binding = _binding(results, family, case_summaries, export)
            if not target.exists():
                interrupted = sorted(target.parent.glob('.' + name + '.cleanup-*'))
                if interrupted:
                    return {**base, 'Status': 'PARTIAL_CLEANUP', 'GeneratedRoot': str(target),
                            'Quarantines': [str(path) for path in interrupted],
                            'Reason': 'An interrupted generated-cache removal needs recovery.'}
                return {**base, 'Status': 'ALREADY_ABSENT', 'GeneratedRoot': str(target)}
            if not target.is_dir(): raise CleanupRefused('The generated root is not a directory.')
            if _active_jobs(root, target): raise CleanupRefused('A native/export worker or generated executable is active.')
            tracked = subprocess.run(['git', '-C', str(root), 'ls-files', '-z', '--', str(target.relative_to(root))],
                                     capture_output=True, check=True).stdout
            if tracked: raise CleanupRefused('The generated root contains tracked files.')
            manifest_path = _beneath(target, target / 'manifest.json')
            manifest = _json(manifest_path)
            expected_inputs = [case_summaries[cid].get('Particles', case_summaries[cid].get('Process')) for cid in family['CaseIDs']]
            if (not isinstance(manifest.get('Generator'), str) or not manifest['Generator'].startswith('HEPCAT')
                    or manifest.get('Class') != 'sm5' + name
                    or manifest.get('PhysicalProcess') != {'Incoming': family.get('Incoming'), 'Outgoing': family.get('Outgoing')}
                    or manifest.get('InputProcesses') != expected_inputs):
                raise CleanupRefused('The generated manifest does not match the family.')
            inventory = _scan(target)
            selected = [row for row in inventory if _evidence(row['Path'])]
            if any(row['Kind'] == 'Symlink' for row in selected):
                raise CleanupRefused('A raw evidence artifact is a symlink.')
            selected = [row for row in selected if row['Kind'] == 'File']
            if sum(row['Bytes'] for row in selected) > 64 * 1024 ** 2:
                raise CleanupRefused('Raw evidence exceeds the bounded archive size.')
            plan = {**base, 'GeneratedRoot': str(target), 'GeneratedFiles': len(inventory),
                    'GeneratedBytes': sum(row['Bytes'] for row in inventory if row['Kind'] == 'File'),
                    'RetainedEvidenceBytes': sum(row['Bytes'] for row in selected)}
            if dry_run: return {**plan, 'Status': 'PLANNED'}
            directory = _beneath(results, results / 'generated-cleanup' / identifier / export['CapsuleSHA256'])
            directory.mkdir(parents=True, exist_ok=True)
            evidence = []
            for row in selected:
                source = target / row['Path']; destination = _beneath(directory, directory / 'evidence' / row['Path'])
                destination.parent.mkdir(parents=True, exist_ok=True)
                digest = _sha(source)
                if destination.exists() and _sha(destination) != digest:
                    raise CleanupRefused('Previously retained evidence has different bytes.')
                if not destination.exists(): shutil.copyfile(source, destination, follow_symlinks=False)
                if _sha(destination) != digest: raise CleanupRefused('Retained raw evidence checksum differs.')
                evidence.append({'Path': row['Path'], 'SHA256': digest, 'Bytes': row['Bytes']})
            journal = {**plan, 'Status': 'EVIDENCE_SAVED', 'SchemaVersion': 1,
                       'RecordedUTC': dt.datetime.now(dt.timezone.utc).isoformat(),
                       'RawExportContextSHA256': export.get('ContextSHA256'), 'ExportCapsuleSHA256': export['CapsuleSHA256'],
                       'CanonicalArtifactSHA256': binding, 'GeneratedBoundaryInventory': inventory,
                       'BoundaryInventoryIsNotAHistoricalExecutionHash': True, 'RetainedEvidence': evidence,
                       'PrecisionQualifiedRootsDeleted': False, 'CanonicalArtifactsDeleted': False}
            journal_path = directory / 'journal.json'; _atomic(journal_path, journal)
            if (_scan(target) != inventory or _binding(results, family, case_summaries, export) != binding
                    or _active_jobs(root, target)):
                raise CleanupRefused('Inputs or activity changed before deletion.')
            quarantine = target.with_name('.' + name + '.cleanup-' + uuid.uuid4().hex)
            target.rename(quarantine)
            journal.update(Status='REMOVING_GENERATED_CACHE', Quarantine=str(quarantine)); _atomic(journal_path, journal)
            shutil.rmtree(quarantine)  # Python unlinks inner symlinks, never their targets.
            journal.update(Status='CLEANED', RemovedUTC=dt.datetime.now(dt.timezone.utc).isoformat()); _atomic(journal_path, journal)
            return {**plan, 'Status': 'CLEANED', 'Journal': str(journal_path)}
    except (OSError, ValueError, KeyError, TypeError, AttributeError, subprocess.SubprocessError) as error:
        if quarantine is not None and quarantine.exists():
            return {**base, 'Status': 'PARTIAL_CLEANUP', 'Reason': str(error),
                    'Quarantine': str(quarantine), 'Journal': str(journal_path)}
        return {**base, 'Reason': str(error)}


def main():
    parser = __import__('argparse').ArgumentParser(description=__doc__)
    parser.add_argument('--root', type=Path, default=Path(__file__).resolve().parents[2])
    parser.add_argument('--family', action='append', default=[])
    parser.add_argument('--apply', action='store_true', help='Delete admitted generated caches; otherwise only plan.')
    args = parser.parse_args(); root = args.root.resolve(); here = root / 'tests/SM-5-point'; results = root / 'tests/results/SM-5-point'
    spec = importlib.util.spec_from_file_location('cleanup_workflow', here / 'run.py'); workflow = importlib.util.module_from_spec(spec); spec.loader.exec_module(workflow)
    configuration = _json(results / 'configuration.json'); current = workflow.context_sha256(configuration)
    compatibility = workflow.context_compatibility(configuration, current); inventory = _json(here / 'inventory.json')
    output = []
    with _lock(results, None) as lease:
        probes = workflow.supplemental_probes.read_registry(here / 'supplemental-probes.json')
        for family in inventory['Families']:
            if args.family and family['ID'] not in args.family: continue
            cases = {cid: workflow.checked_summary(cid, '', current, compatibility) for cid in family['CaseIDs']}
            for cid, value in cases.items():
                if isinstance(value, dict):
                    report = workflow.supplemental_probes.case_report(cid, value.get('CapsuleSHA256'), probes, results, current)
                    if report: value['SupplementalParameterProbes'] = report
            export = workflow.checked_summary(family['ID'], '-export', current, compatibility)
            output.append(cleanup_completed_family(root, results, family, cases, export,
                dry_run=not args.apply, runner_lock=lease))
    print(json.dumps(output, indent=2))
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
