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


_stage_spec = importlib.util.spec_from_file_location(
    'hepcat_sm5_stage_context', Path(__file__).with_name('stage_context.py'))
stage_context = importlib.util.module_from_spec(_stage_spec)
_stage_spec.loader.exec_module(stage_context)

_probe_spec = importlib.util.spec_from_file_location(
    'hepcat_sm5_supplemental_probes', Path(__file__).with_name('supplemental_probes.py'))
supplemental_probes = importlib.util.module_from_spec(_probe_spec)
_probe_spec.loader.exec_module(supplemental_probes)


def context_sha256(configuration):
    return stage_context.fingerprint(stage_context.manifest(ROOT, HERE, configuration))


def context_compatibility(configuration, current_context=None):
    return stage_context.verified_bridge(ROOT, HERE, configuration,
        current_context or context_sha256(configuration))


def checked_summary(identifier, suffix, context_sha, compatibility=None):
    path = RESULTS / (identifier + suffix + '.json')
    summary = read_json(path)
    if not isinstance(summary, dict):
        return None
    summary = dict(summary)
    identity = 'FamilyID' if suffix else 'ID'
    if summary.get(identity) != identifier:
        return {**summary, 'Status': 'STALE', 'Reason': 'Checkpoint identity differs.'}
    if summary.get('ContextSHA256') != context_sha:
        if (not stage_context.admits(summary.get('ContextSHA256'), context_sha, compatibility)
                or not stage_context.checkpoint_matches(path, summary, compatibility)):
            return {**summary, 'Status': 'STALE', 'Reason': 'Source/model/reference context differs.'}
        summary['ContextCompatibility'] = stage_context.public_proof(compatibility,
            'HistoricalExportReport' if suffix else 'NativeCalculation')
        capsule = RESULTS / (identifier + suffix + '.wxf')
        if (summary.get('CapsuleSHA256') is not None
                and (not capsule.is_file() or stage_context.digest(capsule) != summary['CapsuleSHA256'])):
            return {**summary, 'Status': 'STALE', 'Reason': 'Archived checkpoint bytes differ.'}
    if summary.get('Status') in ('COMPLETE', 'PASS'):
        capsule = RESULTS / (identifier + suffix + '.wxf')
        if not capsule.is_file() or summary.get('CapsuleSHA256') != hashlib.sha256(capsule.read_bytes()).hexdigest():
            return {**summary, 'Status': 'STALE', 'Reason': 'Matching amplitude checkpoint is unavailable.'}
        if suffix == '' and (summary.get('FeynmanRows', 0) < 1 or summary.get('Rows', 0) < 1
                             or set(summary.get('CheckStatuses', {})) != {'PASS'}
                             or summary.get('CheckStatuses', {}).get('PASS') != summary.get('Rows')):
            return {**summary, 'Status': 'STALE', 'Reason': 'Mandatory native checks are incomplete.'}
    if suffix and summary.get('Status') in ('PASS', 'FAIL'):
        members = summary.get('CaseIDs')
        if not isinstance(members, list) or not members or len(set(members)) != len(members):
            return {**summary, 'Status': 'STALE', 'Reason': 'Export member bindings are missing.'}
        cases = {cid: checked_summary(cid, '', context_sha, compatibility) for cid in members}
        if not all(isinstance(row, dict) and row.get('Status') == 'COMPLETE' for row in cases.values()):
            return {**summary, 'Status': 'STALE', 'Reason': 'An export member is unavailable.'}
        hashes = {cid: row.get('CapsuleSHA256') for cid, row in cases.items()}
        contexts = {cid: row.get('ContextSHA256') for cid, row in cases.items()}
        mixed = any(value != summary.get('ContextSHA256') for value in contexts.values())
        if (summary.get('CaseCapsulesSHA256') != hashes
                or (('ExporterContextSHA256' in summary or mixed)
                    and summary.get('ExporterContextSHA256') != summary.get('ContextSHA256'))
                or (('NativeCaseContexts' in summary or mixed)
                    and summary.get('NativeCaseContexts') != contexts)):
            return {**summary, 'Status': 'STALE', 'Reason': 'Export inputs or native contexts differ.'}
    return summary


def export_input_admission(identifier, configuration):
    """Check exact native inputs before an export; perform no calculation or write."""
    current = context_sha256(configuration)
    rejected = {'Status': 'REJECTED', 'FamilyID': identifier,
                'Reason': 'Configuration or native input checkpoint is unavailable.'}
    if configuration.get('ContextSHA256') != current:
        return rejected
    inventory = read_json(HERE / 'inventory.json', {})
    family = next((row for row in inventory.get('Families', []) if row.get('ID') == identifier), None)
    if (not family or not isinstance(family.get('CaseIDs'), list) or not family['CaseIDs']
            or not all(isinstance(cid, str) for cid in family['CaseIDs'])
            or len(set(family['CaseIDs'])) != len(family['CaseIDs'])):
        return rejected
    proof = context_compatibility(configuration, current)
    cases = {cid: checked_summary(cid, '', current, proof) for cid in family['CaseIDs']}
    definitions = {row['ID']: row for row in inventory['Cases']}
    if not all(cid in definitions for cid in cases):
        return rejected
    if not all(isinstance(row, dict) and row.get('Status') == 'COMPLETE'
               and row.get('Particles') == definitions[cid]['Particles'] for cid, row in cases.items()):
        return rejected
    try:
        entries = supplemental_probes.read_registry(HERE / 'supplemental-probes.json')
        probes = [supplemental_probes.case_report(cid, row['CapsuleSHA256'],
                  entries, RESULTS, current) for cid, row in cases.items()]
        if any(probe and probe.get('Status') != 'PASS' for probe in probes):
            return {**rejected, 'Reason':
                    'A supplemental parameter check failed or requires a verified recheck.'}
    except (OSError, ValueError, KeyError, TypeError):
        return {**rejected, 'Reason': 'Supplemental parameter evidence cannot be verified.'}
    result = {'Status': 'APPROVED', 'FamilyID': identifier, 'ExporterContextSHA256': current,
              'CaseIDs': family['CaseIDs'],
              'CaseCapsulesSHA256': {cid: row['CapsuleSHA256'] for cid, row in cases.items()},
              'NativeCaseContexts': {cid: row['ContextSHA256'] for cid, row in cases.items()}}
    if any(row['ContextSHA256'] != current for row in cases.values()):
        result['NativeContextCompatibility'] = stage_context.public_proof(proof, 'NativeCalculation')
    return result


def refresh(inventory, previous=None, context_sha=None, points=1000):
    state = previous or {}
    configuration = read_json(RESULTS / 'configuration.json', {})
    context_sha = context_sha or context_sha256(configuration)
    compatibility = context_compatibility(configuration, context_sha)
    cases = {}
    for case in inventory['Cases']:
        summary = checked_summary(case['ID'], '', context_sha, compatibility)
        if summary:
            cases[case['ID']] = summary
    entries = supplemental_probes.read_registry(HERE / 'supplemental-probes.json')
    for cid, summary in cases.items():
        probe = supplemental_probes.case_report(cid, summary.get('CapsuleSHA256'),
            entries, RESULTS, context_sha)
        if probe:
            summary['SupplementalParameterProbes'] = probe
    def known_checks_pass(family):
        return all(cases.get(cid, {}).get('SupplementalParameterProbes', {}).get(
            'Status', 'PASS') == 'PASS' for cid in family['CaseIDs'])
    families = {}
    for family in inventory['Families']:
        summary = checked_summary(family['ID'], '-export', context_sha, compatibility)
        if summary:
            if summary.get('Status') == 'PASS' and (
                    summary.get('CalcHEPStatus') != 'PASS' or summary.get('MadGraphStatus') != 'PASS'
                    or summary.get('Points', 0) < points
                    or not all(cases.get(cid, {}).get('Status') == 'COMPLETE' for cid in family['CaseIDs'])
                    or summary.get('CaseIDs') != family['CaseIDs']
                    or summary.get('CaseCapsulesSHA256') != {cid: cases.get(cid, {}).get('CapsuleSHA256')
                                                            for cid in family['CaseIDs']}):
                summary = {**summary, 'Status': 'STALE', 'Reason': 'A member or independent validation is incomplete.'}
            if not known_checks_pass(family):
                summary['SupplementalParameterProbeStatus'] = 'NOT_ALL_PASS'
                summary['RecordedInputStatus'] = summary.get('Status')
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
                    RESULTS, ROOT / 'tests/results/spinas-output/SM-5-point', points,
                    context_compatibility=compatibility)
                if evidence:
                    summary['PrecisionQualification'] = evidence
                    qualified.append(family)
    recorded = sum(len(f['CaseIDs']) for f in inventory['Families']
                   if families.get(f['ID'], {}).get('Status') == 'PASS')
    fully = sum(len(f['CaseIDs']) for f in inventory['Families']
                if families.get(f['ID'], {}).get('Status') == 'PASS' and known_checks_pass(f))
    recorded_qualified = sum(len(f['CaseIDs']) for f in qualified)
    qualified = [f for f in qualified if known_checks_pass(f)]
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
                         'ValidatedFamilies': sum(families.get(f['ID'], {}).get('Status') == 'PASS'
                                                  and known_checks_pass(f) for f in inventory['Families']),
                         'RecordedInputPassCases': recorded + recorded_qualified,
                         'SupplementalParameterProbeFailedCases': sum(
                             c.get('SupplementalParameterProbes', {}).get('Status') == 'FAIL'
                             for c in cases.values()),
                         'SupplementalParameterProbeUnavailableCases': sum(
                             c.get('SupplementalParameterProbes', {}).get('Status') == 'UNAVAILABLE'
                             for c in cases.values()),
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
        summary = checked_summary(identifier, suffix, context_sha,
            context_compatibility(configuration, context_sha))
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
