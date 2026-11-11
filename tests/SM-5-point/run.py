#!/usr/bin/env python3
"""Resume SM five-point construction and independent validation, one family at a time."""
# HEPCAT - High Energy Physics Constructive Amplitude Tool
# Copyright (C) 2026 Neil Christensen
# SPDX-License-Identifier: GPL-3.0-or-later

from __future__ import annotations
import argparse
import datetime as dt
import fcntl
import hashlib
import importlib.util
import json
import math
import os
from pathlib import Path
import secrets
import shutil
import signal
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[2]
HERE = Path(__file__).resolve().parent
RESULTS = ROOT / 'tests/results/SM-5-point'
PROGRESS = HERE / 'progress.json'


def timestamp():
    return dt.datetime.now(dt.timezone.utc).isoformat()


def atomic_json(path, data):
    temporary = path.with_suffix(path.suffix + '.tmp')
    temporary.write_text(json.dumps(data, indent=2) + '\n')
    temporary.replace(path)


def read_json(path, default=None):
    try:
        return json.loads(path.read_text()) if path.is_file() else default
    except (OSError, ValueError):
        return default


def context_sha256(configuration):
    paths = [path for path in (ROOT / 'source').rglob('*')
             if path.is_file() and path.suffix in ('.wl', '.py', '.cpp', '.hpp', '.h')]
    paths += [ROOT / 'models' / f'{stem}{number}.mdl'
              for number in (1, 2) for stem in ('amps', 'prtcls', 'vars', 'func', 'extlib')]
    paths += list((ROOT / 'tests/reference-models/calchep').glob('*1.mdl'))
    paths += list((ROOT / 'tests/reference-models/madgraph/sm').glob('*.py'))
    paths += [HERE / 'inventory.json']
    hashes = {str(path.relative_to(ROOT)): hashlib.sha256(path.read_bytes()).hexdigest()
              for path in sorted(paths) if path.is_file()}
    spinas = Path(configuration.get('SPINASDirectory', '/nonexistent'))
    runtime = list((spinas / 'include').rglob('*.h')) + list((spinas / 'include').rglob('*.hpp'))
    runtime += [path for path in spinas.rglob('libspinas*')
                if path.is_file() and path.suffix in ('.a', '.so', '.dylib')]
    for program in ('MadGraphDirectory', 'CalcHEPDirectory'):
        location = Path(configuration.get(program, '/nonexistent'))
        runtime += [location / 'VERSION', location / 'VERSION.txt']
    hashes.update({str(path): hashlib.sha256(path.read_bytes()).hexdigest()
                   for path in sorted(set(runtime)) if path.is_file()})
    paths_config = {key: str(configuration.get(key, '')) for key in
                    ('SPINASDirectory', 'CalcHEPDirectory', 'MadGraphDirectory')}
    return hashlib.sha256(json.dumps({'Files': hashes, 'Programs': paths_config},
                                    sort_keys=True).encode()).hexdigest()


def checked_summary(identifier, suffix, context_sha):
    path = RESULTS / (identifier + suffix + '.json')
    summary = read_json(path)
    if not isinstance(summary, dict):
        return None
    summary = dict(summary)
    identity = 'FamilyID' if suffix else 'ID'
    if summary.get(identity) != identifier:
        return {**summary, 'Status': 'STALE', 'Reason': 'Checkpoint identity differs.'}
    if summary.get('ContextSHA256') != context_sha:
        return {**summary, 'Status': 'STALE', 'Reason': 'Source/model/reference context differs.'}
    if summary.get('Status') in ('COMPLETE', 'PASS'):
        capsule = RESULTS / (identifier + suffix + '.wxf')
        if not capsule.is_file() or summary.get('CapsuleSHA256') != hashlib.sha256(capsule.read_bytes()).hexdigest():
            return {**summary, 'Status': 'STALE', 'Reason': 'Matching amplitude checkpoint is unavailable.'}
        if suffix == '' and (summary.get('FeynmanRows', 0) < 1 or summary.get('Rows', 0) < 1
                             or set(summary.get('CheckStatuses', {})) != {'PASS'}
                             or summary.get('CheckStatuses', {}).get('PASS') != summary.get('Rows')):
            return {**summary, 'Status': 'STALE', 'Reason': 'Mandatory native checks are incomplete.'}
    return summary


def refresh(inventory, previous=None, context_sha=None, points=1000):
    state = previous or {}
    context_sha = context_sha or context_sha256(read_json(RESULTS / 'configuration.json', {}))
    cases = {}
    for case in inventory['Cases']:
        summary = checked_summary(case['ID'], '', context_sha)
        if summary:
            cases[case['ID']] = summary
    families = {}
    for family in inventory['Families']:
        summary = checked_summary(family['ID'], '-export', context_sha)
        if summary:
            if summary.get('Status') == 'PASS' and (
                    summary.get('CalcHEPStatus') != 'PASS' or summary.get('MadGraphStatus') != 'PASS'
                    or summary.get('Points', 0) < points
                    or not all(cases.get(cid, {}).get('Status') == 'COMPLETE' for cid in family['CaseIDs'])
                    or summary.get('CaseIDs') != family['CaseIDs']
                    or summary.get('CaseCapsulesSHA256') != {cid: cases.get(cid, {}).get('CapsuleSHA256')
                                                            for cid in family['CaseIDs']}):
                summary = {**summary, 'Status': 'STALE', 'Reason': 'A member or independent validation is incomplete.'}
            families[family['ID']] = summary
    qualified = []
    precision_path = Path(__file__).with_name('precision_evidence.py')
    if precision_path.is_file():
        specification = importlib.util.spec_from_file_location('hepcat_sm5_precision_evidence', precision_path)
        precision = importlib.util.module_from_spec(specification)
        specification.loader.exec_module(precision)
        for family in inventory['Families']:
            summary = families.get(family['ID'])
            if summary and summary.get('Status') != 'PASS':
                evidence = precision.qualification(family, summary, cases, context_sha,
                    RESULTS, ROOT / 'tests/results/spinas-output/SM-5-point', points)
                if evidence:
                    summary['PrecisionQualification'] = evidence
                    qualified.append(family)
    fully = sum(len(f['CaseIDs']) for f in inventory['Families']
                if families.get(f['ID'], {}).get('Status') == 'PASS')
    qualified_cases = sum(len(f['CaseIDs']) for f in qualified)
    state.update(SchemaVersion=1, UpdatedUTC=timestamp(), ContextSHA256=context_sha,
                 InventorySHA256=hashlib.sha256((HERE / 'inventory.json').read_bytes()).hexdigest(),
                 Cases=cases, Families=families,
                 Counts={'TotalCases': len(inventory['Cases']),
                         'TotalFamilies': len(inventory['Families']),
                         'Constructed': sum(c.get('Status') == 'COMPLETE' for c in cases.values()),
                         'FullyValidatedCases': fully,
                         'PrecisionQualifiedCases': qualified_cases,
                         'ValidatedIncludingPrecisionQualifiedCases': fully + qualified_cases,
                         'PrecisionQualifiedFamilies': len(qualified),
                         'ValidatedFamilies': sum(f.get('Status') == 'PASS' for f in families.values()),
                         'FailedOrTimedOut': sum(c.get('Status') in ('FAIL', 'TIMEOUT', 'ERROR')
                                                for c in cases.values())})
    atomic_json(PROGRESS, state)
    return state


def stop_process(process):
    try:
        os.killpg(process.pid, signal.SIGTERM)
        process.wait(timeout=5)
    except subprocess.TimeoutExpired:
        os.killpg(process.pid, signal.SIGKILL)
        process.wait()
    except ProcessLookupError:
        pass


def run_worker(args, kind, identifier, config_path):
    suffix = '-export' if kind == 'export' else ''
    summary_path = RESULTS / (identifier + suffix + '.json')
    log_path = RESULTS / (identifier + suffix + '.log')
    configuration = read_json(config_path, {})
    context_sha = configuration.get('ContextSHA256', context_sha256(configuration))
    if any((RESULTS / (identifier + suffix + extension)).exists()
           for extension in ('.json', '.wxf', '.log', '.live.txt')):
        history = RESULTS / 'history'
        history.mkdir(exist_ok=True)
        stamp = dt.datetime.now(dt.timezone.utc).strftime('%Y%m%dT%H%M%S%f')
        for extension in ('.json', '.wxf', '.log', '.live.txt'):
            prior = RESULTS / (identifier + suffix + extension)
            if prior.exists():
                prior.replace(history / (identifier + suffix + '-' + stamp + extension))
    seed = secrets.randbelow(2**31 - 1) + 1
    environment = dict(os.environ, HEPCAT_SM5_CONFIGURATION=str(config_path),
                       HEPCAT_SM5_KIND=kind, HEPCAT_SM5_ID=identifier,
                       HEPCAT_SM5_SEED=str(seed), OMP_NUM_THREADS='1',
                       OPENBLAS_NUM_THREADS='1', MKL_NUM_THREADS='1')
    print(f'{timestamp()} {kind} {identifier}', flush=True)
    with log_path.open('w') as log:
        try:
            process = subprocess.Popen([args.kernel, '-script', str(HERE / 'evaluate.wl')],
                                       cwd=ROOT, env=environment, stdout=log,
                                       stderr=subprocess.STDOUT, start_new_session=True)
        except OSError as error:
            summary = {'ID' if kind == 'construct' else 'FamilyID': identifier,
                       'Status': 'ERROR', 'ContextSHA256': context_sha, 'RandomSeed': seed,
                       'Message': str(error), 'Log': str(log_path.relative_to(ROOT))}
            atomic_json(summary_path, summary)
            return summary
        try:
            process.wait(timeout=args.export_timeout if kind == 'export' else args.timeout)
        except subprocess.TimeoutExpired:
            stop_process(process)
            atomic_json(summary_path, {'ID' if kind == 'construct' else 'FamilyID': identifier,
                                      'Status': 'TIMEOUT', 'RandomSeed': seed, 'ContextSHA256': context_sha,
                                      'Log': str(log_path.relative_to(ROOT)),
                                      'Message': 'External deadline reached; not validated.'})
        except KeyboardInterrupt:
            stop_process(process)
            raise
    summary = read_json(summary_path)
    if process.returncode != 0 and isinstance(summary, dict) and summary.get('Status') in ('COMPLETE', 'PASS'):
        summary = None
    if summary is None:
        summary = {'ID' if kind == 'construct' else 'FamilyID': identifier,
                   'Status': 'ERROR', 'RandomSeed': seed, 'ContextSHA256': context_sha,
                   'Log': str(log_path.relative_to(ROOT)), 'ExitCode': process.returncode,
                   'Message': 'Worker exited without a completed checkpoint.'}
        atomic_json(summary_path, summary)
    if summary.get('Status') in ('COMPLETE', 'PASS'):
        summary = checked_summary(identifier, suffix, context_sha)
    print(f"{identifier}: {summary['Status']}", flush=True)
    return summary


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--kernel', default=shutil.which('WolframKernel') or
                        '/Applications/Wolfram.app/Contents/MacOS/WolframKernel')
    parser.add_argument('--family', action='append', default=[])
    parser.add_argument('--sector', action='append', default=[])
    parser.add_argument('--limit', type=int, help='Maximum families to attempt in this batch.')
    parser.add_argument('--representatives', action='store_true',
                        help='Begin with one family per certified generation orbit.')
    parser.add_argument('--retry-failed', action='store_true')
    parser.add_argument('--timeout', type=float, default=7200,
                        help='External deadline per construction, seconds.')
    parser.add_argument('--export-timeout', type=float, default=1800)
    parser.add_argument('--points', type=int, default=1000)
    parser.add_argument('--memory-gib', type=int, default=4)
    parser.add_argument('--spinas', type=Path, default=ROOT.parent / 'spinas')
    parser.add_argument('--calchep', type=Path, default=ROOT.parent / 'CalcHEP/calchep_3.9.2')
    parser.add_argument('--madgraph', type=Path, default=ROOT.parent / 'MadGraph')
    parser.add_argument('--notebook', action='store_true', help='Refresh the worked notebook at the end.')
    parser.add_argument('--refresh-only', action='store_true')
    args = parser.parse_args()
    if (not all(math.isfinite(v) and v > 0 for v in (args.timeout, args.export_timeout))
            or min(args.points, args.memory_gib) <= 0 or (args.limit is not None and args.limit < 1)):
        parser.error('Deadlines must be finite and positive; limits, points and memory must be positive.')
    RESULTS.mkdir(parents=True, exist_ok=True)
    inventory = read_json(HERE / 'inventory.json')
    if not inventory:
        parser.error('Run inventory.py first.')
    lock = (RESULTS / 'run.lock').open('w')
    try:
        fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
    except BlockingIOError:
        parser.error('Another SM five-point runner is already active.')
    configuration = {'SPINASDirectory': str(args.spinas.resolve()),
                     'CalcHEPDirectory': str(args.calchep.resolve()),
                     'MadGraphDirectory': str(args.madgraph.resolve()),
                     'MemoryGiB': args.memory_gib, 'Points': args.points}
    configuration['ContextSHA256'] = context_sha256(configuration)
    context_sha = configuration['ContextSHA256']
    config_path = RESULTS / 'configuration.json'
    atomic_json(config_path, configuration)
    state = refresh(inventory, read_json(PROGRESS, {}), context_sha, args.points)
    families = [f for f in inventory['Families']
                if (not args.family or f['ID'] in args.family)
                and (not args.sector or f['Sector'] in args.sector)
                and (not args.representatives or f.get('IsRepresentative', True))]
    selected = []
    for family in families:
        old = state['Families'].get(family['ID'], {})
        if old.get('Status') == 'PASS' or old.get('PrecisionQualification', {}).get('Status') == 'PRECISION_QUALIFIED':
            continue
        attempted = [state['Cases'].get(cid, {}) for cid in family['CaseIDs']]
        if not args.retry_failed and any(c.get('Status') in ('FAIL', 'TIMEOUT', 'ERROR') for c in attempted):
            continue
        selected.append(family)
    if args.limit is not None:
        selected = selected[:args.limit]
    stopped = 'BatchComplete'
    try:
        if not args.refresh_only:
            for family in selected:
                state.update(CurrentFamily=family['ID'], StopReason='RUNNING')
                atomic_json(PROGRESS, state)
                for cid in family['CaseIDs']:
                    current = state['Cases'].get(cid, {})
                    if current.get('Status') != 'COMPLETE':
                        state['CurrentCase'] = cid
                        atomic_json(PROGRESS, state)
                        if context_sha256(configuration) != context_sha:
                            raise RuntimeError('Inputs changed during the batch.')
                        run_worker(args, 'construct', cid, config_path)
                        if context_sha256(configuration) != context_sha:
                            raise RuntimeError('Inputs changed during the batch.')
                        state = refresh(inventory, state, context_sha, args.points)
                if all(state['Cases'].get(cid, {}).get('Status') == 'COMPLETE'
                       for cid in family['CaseIDs']):
                    if context_sha256(configuration) != context_sha:
                        raise RuntimeError('Inputs changed during the batch.')
                    run_worker(args, 'export', family['ID'], config_path)
                    if context_sha256(configuration) != context_sha:
                        raise RuntimeError('Inputs changed during the batch.')
                    state = refresh(inventory, state, context_sha, args.points)
                state['LastAttemptedFamily'] = family['ID']
                if state['Families'].get(family['ID'], {}).get('Status') == 'PASS':
                    state['LastCompletedFamily'] = family['ID']
                atomic_json(PROGRESS, state)
    except KeyboardInterrupt:
        stopped = 'InterruptedWithCheckpointsSaved'
    except RuntimeError as error:
        stopped = str(error)
    finally:
        state = refresh(inventory, state, context_sha256(configuration), args.points)
        state.update(StopReason=stopped, CurrentCase=None, CurrentFamily=None)
        pending = [f['ID'] for f in families
                   if state['Families'].get(f['ID'], {}).get('Status') != 'PASS'
                   and state['Families'].get(f['ID'], {}).get('PrecisionQualification', {}).get('Status') != 'PRECISION_QUALIFIED']
        state['NextFamily'] = pending[0] if pending else None
        atomic_json(PROGRESS, state)
        if args.notebook:
            subprocess.run([args.kernel, '-script', str(HERE / 'build-notebook.wl')],
                           cwd=ROOT, check=True)
    print(json.dumps(state['Counts'], indent=2), flush=True)
    print(f'Checkpoint: {PROGRESS}', flush=True)


if __name__ == '__main__':
    main()
