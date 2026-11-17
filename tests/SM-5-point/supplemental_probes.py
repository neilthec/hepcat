"""Checksum-bound supplementary checks; never modify a native or export report."""
# HEPCAT - High Energy Physics Constructive Amplitude Tool
# Copyright (C) 2026 Neil Christensen
# SPDX-License-Identifier: GPL-3.0-or-later
from collections import Counter
import hashlib
import json
from pathlib import Path


def digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def read_registry(path):
    value = json.loads(Path(path).read_text())
    if not isinstance(value, dict):
        raise ValueError('Invalid supplemental-probe registry.')
    rows = value.get('Probes')
    if (value.get('SchemaVersion') != 1 or not isinstance(rows, list)
            or any(not isinstance(row, dict) or not isinstance(row.get('ID'), str)
                   or not isinstance(row.get('CaseID'), str) for row in rows)
            or len({row['ID'] for row in rows}) != len(rows)):
        raise ValueError('Invalid supplemental-probe registry.')
    return rows


def inside(root, relative):
    relative = Path(relative)
    if relative.is_absolute() or '..' in relative.parts:
        raise ValueError('Supplemental-probe paths must stay inside results.')
    result = (Path(root) / relative).resolve()
    result.relative_to(Path(root).resolve())
    return result


def verify(entry, source_sha, results, context):
    result = {'ID': entry['ID'], 'Status': 'UNAVAILABLE'}
    try:
        if (entry.get('Kind') != 'ParameterCheck'
                or entry.get('SourceCapsuleSHA256') != source_sha
                or entry.get('ProbeContextSHA256') != context):
            raise ValueError('The probe needs a recheck for this expression or context.')
        files = entry['Files']
        if set(files) != {'Capsule', 'Summary', 'Script', 'ConversionScript'}:
            raise ValueError('Incomplete supplemental-probe bindings.')
        paths = {key: inside(results, row['Path']) for key, row in files.items()}
        if any(digest(paths[key]) != row['SHA256'] for key, row in files.items()):
            raise ValueError('A supplemental-probe artifact is missing or changed.')
        summary = json.loads(paths['Summary'].read_text())
        if not isinstance(summary, dict):
            raise ValueError('Supplemental-probe summary must be an object.')
        rows = summary.get('Rows')
        counts = summary.get('StatusCounts')
        if (summary.get('SourceCaseID') != entry['CaseID']
                or summary.get('SourceCapsuleSHA256') != source_sha
                or summary.get('ContextSHA256') != context
                or summary.get('Status') != entry.get('Status')
                or counts != entry.get('StatusCounts')
                or not isinstance(rows, list) or not rows
                or any(not isinstance(row, dict) or not isinstance(row.get('Status'), str)
                       for row in rows)
                or dict(Counter(row['Status'] for row in rows)) != counts
                or summary.get('SourceAmplitudeUnchanged') is not True
                or summary.get('NoExpressionMassSlotOrFlavorTransport') is not True):
            raise ValueError('Supplemental-probe identity or actual check counts differ.')
        if any(digest(paths[key]) != row['SHA256'] for key, row in files.items()):
            raise ValueError('A supplemental-probe artifact changed during readback.')
        status = ('PASS' if set(counts) == {'PASS'} else
                  'FAIL' if any(key in counts for key in ('FAIL', 'Opp Sign')) else 'UNAVAILABLE')
        if ((status == 'PASS') != (summary['Status'] == 'ALL_PASS')):
            raise ValueError('Supplemental-probe status disagrees with its actual rows.')
        return {**result, 'Status': status, 'StatusCounts': counts,
                'SourceCapsuleSHA256': source_sha,
                'ProbeContextSHA256': context,
                'SummarySHA256': files['Summary']['SHA256'],
                'CapsuleSHA256': files['Capsule']['SHA256']}
    except (OSError, ValueError, KeyError, TypeError) as error:
        return {**result, 'Reason': str(error)}


def case_report(case_id, source_sha, entries, results, context, compatibility=None):
    selected = [row for row in entries if row['CaseID'] == case_id]
    contexts = {row['ID']: context for row in selected}
    historical = set()
    if isinstance(compatibility, dict) and compatibility.get('_FiniteRetentionV3Verified') is True:
        import importlib.util
        spec = importlib.util.spec_from_file_location('hepcat_exact_policy_probe_retention_v3',
            Path(__file__).with_name('finite_retention.py'))
        receipts = importlib.util.module_from_spec(spec); spec.loader.exec_module(receipts)
        plan = receipts.supplemental_plan(case_id, source_sha, entries, results, context, compatibility)
        if not isinstance(plan, dict) or plan.get('Status') != 'VERIFIED_CONTEXT_PLAN':
            return {'Status': 'UNAVAILABLE', 'Probes': [],
                    'Reason': (plan or {}).get('Reason', 'Historical supplemental evidence is unavailable.')}
        contexts = plan['ContextsByID']; historical = set(plan['HistoricalIDs'])
    elif isinstance(compatibility, dict) and compatibility.get('_ScopedV2ReceiptVerified') is True:
        import importlib.util
        spec = importlib.util.spec_from_file_location('hepcat_scoped_probe_receipts_v2',
            Path(__file__).with_name('constructor_receipts_v2.py'))
        receipts = importlib.util.module_from_spec(spec); spec.loader.exec_module(receipts)
        plan = receipts.supplemental_plan(case_id, source_sha, entries, results, context, compatibility)
        if not isinstance(plan, dict) or plan.get('Status') != 'VERIFIED_CONTEXT_PLAN':
            return {'Status': 'UNAVAILABLE', 'Probes': [],
                    'Reason': (plan or {}).get('Reason', 'Historical supplemental evidence is unavailable.')}
        contexts = plan['ContextsByID']; historical = set(plan['HistoricalIDs'])
    elif isinstance(compatibility, dict) and compatibility.get('_FiniteConstructorReceiptVerified') is True:
        import importlib.util
        spec = importlib.util.spec_from_file_location('hepcat_finite_probe_receipts',
            Path(__file__).with_name('constructor_receipts.py'))
        receipts = importlib.util.module_from_spec(spec); spec.loader.exec_module(receipts)
        plan = receipts.supplemental_plan(case_id, source_sha, entries, results, context, compatibility)
        if not isinstance(plan, dict) or plan.get('Status') != 'VERIFIED_CONTEXT_PLAN':
            return {'Status': 'UNAVAILABLE', 'Probes': [],
                    'Reason': (plan or {}).get('Reason', 'Historical supplemental evidence is unavailable.')}
        contexts = plan['ContextsByID']; historical = set(plan['HistoricalIDs'])
    if not selected:
        return None
    probes = [verify(row, source_sha, results, contexts[row['ID']]) for row in selected]
    probes = [{**row, 'HistoricalEvidenceOnly': True, 'CurrentValidatorPassClaimed': False}
              if row['ID'] in historical else row for row in probes]
    status = ('FAIL' if any(row['Status'] == 'FAIL' for row in probes) else
              'UNAVAILABLE' if any(row['Status'] != 'PASS' for row in probes) else 'PASS')
    return {'Status': status, 'Probes': probes}
