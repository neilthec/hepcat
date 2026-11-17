"""Finite recorded-evidence retention after a reviewed constructor-only change.

This reader performs no physics, writes, migration, implicit sealing or status
upgrade. An independently reviewed pinned registry entry is the trust anchor.
Absent or changed evidence is unavailable. The original producers, native
failures and prior conditional/precision history remain part of every record.
"""
# HEPCAT - High Energy Physics Constructive Amplitude Tool
# Copyright (C) 2026 Neil Christensen
# SPDX-License-Identifier: GPL-3.0-or-later
from pathlib import Path
import hashlib
import importlib.util
import json
import re

KIND = 'FiniteRecordedEvidenceRetentionAfterConstructorOnlyChange'
# Reviewed data-only trust anchors are outside receipt-bound engine files.
REGISTRY_FILENAME = 'constructor-receipts.json'
P = 'ConstructiveDiagrams`Private`'
FIELDS = ('DownValues', 'UpValues', 'SubValues', 'NValues', 'OwnValues',
          'DefaultValues', 'FormatValues', 'Options', 'Attributes', 'Messages')
CAPTURE_PATTERNS = ('ConstructiveDiagrams`*', 'ConstructiveDiagrams`Private`*',
    'HEPCATCovariantCurrentSources`*', 'HEPCATCovariantCurrentSources`Private`*',
    'HEPCATCovariantContactCalibration`*', 'HEPCATCovariantContactCalibration`Private`*',
    'HEPCATMinimalAdjointCubicSources`*', 'HEPCATMinimalAdjointCubicSources`Private`*',
    'HEPCATMinimalQCDCurrentSources`*', 'HEPCATMinimalQCDCurrentSources`Private`*')
# Exact implementation fields, never a prefix or suffix exemption.
CONSTRUCTOR_FIELDS = {
    'ConstructiveDiagrams`ConstructAmplitude': ['DownValues'],
    **{P + name: ['DownValues'] for name in (
        'colorCurrentSourceClass', 'colorCurrentSource', 'colorCurrentSourceCertificate',
        'colorQCDCurrentSourceClass', 'colorQCDCurrentSource', 'colorQCDCurrentSourceCertificate',
        'colorSpectatorSource')},
    'HEPCATCovariantCurrentSources`Private`covariantCurrentSource': ['DownValues'],
    'HEPCATCovariantContactCalibration`Private`calibratedCovariantSource': ['DownValues'],
}
LOADER = P + '$amplitudeParallelLoadedSignature'
LOADER_FORMULA = P + 'amplitudeParallelSignature'
PATH_CACHES = tuple(P + name for name in (
    '$amplitudeParallelSourceDirectory', '$spinasExportSourceDirectory', '$spinasExportRuntimeDirectory'))
SOURCE_PATHS = frozenset(('source/HEPCAT-Model.wl',
    'source/internal/HEPCAT-ColoredCurrents.wl', 'source/internal/HEPCAT-Color.wl',
    'source/internal/HEPCAT-QCDCurrents.wl', 'source/internal/HEPCAT-CovariantSources.wl',
    'source/internal/HEPCAT-CovariantContacts.wl'))
DERIVED_FIELDS = frozenset(('ContextCompatibility', 'PrecisionQualification',
    'SupplementalParameterProbes', 'SupplementalParameterProbeStatus', 'RecordedInputStatus'))
FALSE_CLAIMS = ('CurrentConstructorSuccessClaimed', 'ConstructorEquivalenceClaimed',
    'ValidationRerunPerformed', 'CurrentValidatorPassClaimed', 'UniversalHistoricalCorrectnessClaimed',
    'MutableGeneratedProjectIdentityClaimed', 'OperationalTimeBudgetEquivalenceClaimed',
    'HistoricalMessageStateReconstructed')


def digest(path):
    result = hashlib.sha256()
    with Path(path).open('rb') as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b''):
            result.update(block)
    return result.hexdigest()


def canonical_digest(value):
    return hashlib.sha256(json.dumps(value, sort_keys=True).encode()).hexdigest()


def inside(root, relative):
    path = Path(relative)
    if path.is_absolute() or '..' in path.parts:
        raise ValueError('Receipt paths must remain inside their checkout.')
    resolved = (Path(root) / path).resolve()
    resolved.relative_to(Path(root).resolve())
    return resolved


def object_at(path):
    value = json.loads(Path(path).read_text())
    if not isinstance(value, dict):
        raise ValueError('A receipt object is missing or malformed.')
    return value


def bound(root, binding):
    if not isinstance(binding, dict) or set(binding) != {'Path', 'SHA256'}:
        raise ValueError('An exact path and SHA256 binding is required.')
    path = inside(root, binding['Path'])
    if digest(path) != binding['SHA256']:
        raise ValueError('A bound evidence artifact is missing or changed.')
    return path


def bound_object(root, binding):
    path = bound(root, binding)
    value = object_at(path)
    if digest(path) != binding['SHA256']:
        raise ValueError('A bound artifact changed during readback.')
    return value


def analyze_transition(core, before, after, before_raw, after_raw, approved_new,
                       loader, *, installed_root_captures=False, private_paths=None):
    """Compare exact ten-field cold tables and literal source manifests.

    Private path substitution is analysis only. Actual installed-root twin
    captures, with literally equal cache paths, are required for admission.
    New symbols use the exact reviewed names, shapes and full field hashes.
    """
    if before.get('Programs') != after.get('Programs'):
        raise ValueError('A numerical program changed.')
    left_files, right_files = before['Files'], after['Files']
    if left_files.keys() != right_files.keys():
        raise ValueError('Source/model/runtime files were added or removed.')
    changed_files = {key for key in left_files if left_files[key] != right_files[key]}
    if changed_files != SOURCE_PATHS:
        raise ValueError('The changed-source set is not exactly the six reviewed constructor files.')
    if (len(before_raw) != 2 or len(after_raw) != 2
            or any(row.get('CapturedNamePatterns') != list(CAPTURE_PATTERNS)
                   for row in (*before_raw, *after_raw))):
        raise ValueError('Exactly the reviewed ten ordered namespace patterns are required.')
    old = core.cold_pair(*before_raw, require_post=False)
    new = core.cold_pair(*after_raw, require_post=False)
    if any(before_raw[0].get(key) != after_raw[0].get(key)
           for key in ('WolframVersion', 'SystemID', 'CapturedNamePatterns')):
        raise ValueError('Cold version, platform or namespace inventory changed.')
    if old.keys() - new.keys():
        raise ValueError('An existing symbol disappeared.')
    expected = {**CONSTRUCTOR_FIELDS, LOADER: ['OwnValues']}
    changes, artifacts = {}, {}
    for name, fields in old.items():
        current = dict(new[name])
        differing = [key for key in FIELDS if fields[key] != current[key]]
        if private_paths and name in PATH_CACHES and differing == ['OwnValues']:
            canonical, private = private_paths
            if current['OwnValues'].replace(private, canonical) != fields['OwnValues']:
                raise ValueError('Unexpected private-tree cache difference.')
            current['OwnValues'] = fields['OwnValues']
            artifacts[name] = ['OwnValues']
        differing = [key for key in FIELDS if fields[key] != current[key]]
        if differing:
            if differing != expected.get(name):
                raise ValueError('Unreviewed existing definition change: ' + name)
            changes[name] = differing
    if changes != expected or old.get(LOADER_FORMULA) != new.get(LOADER_FORMULA):
        raise ValueError('The exact ten constructor fields and loader cache are required; hash formula is unchanged.')
    if (loader.get('InputCountBefore') != 18 or loader.get('InputCountAfter') != 18
            or loader.get('IndependentBareRecomputationMatches') is not True
            or loader.get('FormulaAllTenFieldsUnchanged') is not True
            or core._loader_value(old[LOADER]['OwnValues']) != loader.get('Before')
            or core._loader_value(new[LOADER]['OwnValues']) != loader.get('After')):
        raise ValueError('Actual unchanged-formula eighteen-input loader recomputation is required.')
    if not isinstance(approved_new, dict) or set(approved_new) != new.keys() - old.keys():
        raise ValueError('Exact new-symbol inventory is required.')
    for name, row in approved_new.items():
        if ('`Private`' not in name or not isinstance(row, dict)
                or row.get('FieldsSHA256') != canonical_digest(new[name])
                or row.get('Fields') != new[name]):
            raise ValueError('A new symbol differs from its explicit reviewed inventory.')
    installed = installed_root_captures is True and not artifacts and private_paths is None
    return {'Status': 'ANALYZED_NOT_SEALED', 'ChangedExistingFields': changes,
        'ChangedSourcePaths': sorted(changed_files), 'PrivatePathArtifacts': artifacts,
        'ActualInstalledRootCaptures': installed,
        'AllOtherExistingFieldsLiterallyUnchanged': True,
        'BeforeDefinitionsSHA256': canonical_digest(old), 'AfterDefinitionsSHA256': canonical_digest(new),
        'BeforeManifestSHA256': canonical_digest(before), 'AfterManifestSHA256': canonical_digest(after),
        'NewSymbolInventorySHA256': canonical_digest(approved_new),
        'ConstructorEquivalenceClaimed': False, 'CurrentValidatorPassClaimed': False}


def _scope(receipt):
    flags = receipt.get('ScopeFlags', {})
    return (flags.get('HistoricalEvidenceOnly') is True
            and all(flags.get(key) is False for key in FALSE_CLAIMS))


def _stage(here):
    spec = importlib.util.spec_from_file_location('hepcat_finite_receipt_stage', Path(here) / 'stage_context.py')
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def receipt_status(root, here, configuration, current_context, *, registry=None, core=None):
    """Return explicit AVAILABLE/UNAVAILABLE and a finite proof, without writes."""
    try:
        root = Path(root).resolve()
        if registry is None:
            registry_data = object_at(Path(here) / REGISTRY_FILENAME)
            if (set(registry_data) != {'SchemaVersion', 'Receipts'}
                    or registry_data['SchemaVersion'] != 1
                    or not isinstance(registry_data['Receipts'], dict)):
                raise ValueError('The reviewed data-only trust-anchor registry is malformed.')
            registry = registry_data['Receipts']
        entry = registry.get(current_context)
        if entry is None:
            raise ValueError('No reviewed finite receipt is registered for this context.')
        receipt_path = inside(root, entry['Path'])
        receipt = bound_object(root, entry)
        if (receipt.get('SchemaVersion') != 1 or receipt.get('Kind') != KIND
                or receipt.get('Status') != 'SEALED_REVIEWED'
                or receipt.get('AfterContextSHA256') != current_context or not _scope(receipt)):
            raise ValueError('Receipt is unsealed, claims a rerun/upgrade, or has the wrong context.')
        if core is None:
            raise ValueError('The existing exact definition reader is required.')
        stage = _stage(here)
        before = bound_object(root, receipt['BeforeManifest'])
        after = bound_object(root, receipt['AfterManifest'])
        if (stage.fingerprint(before) != receipt.get('BeforeContextSHA256')
                or stage.fingerprint(after) != current_context
                or stage.manifest(root, here, configuration) != after):
            raise ValueError('Actual source, model or numerical runtime context differs.')
        inputs = receipt['DefinitionInputs']
        if inputs.get('ActualInstalledRootCaptures') is not True:
            raise ValueError('Actual installed-root twin captures are required.')
        raw_before = [bound_object(root, row) for row in inputs['BeforePair']]
        raw_after = [bound_object(root, row) for row in inputs['AfterPair']]
        if len(raw_before) != 2 or len(raw_after) != 2:
            raise ValueError('Exactly two independent captures are required on each side.')
        inventory = bound_object(root, inputs['NewSymbolInventory'])
        loader = bound_object(root, inputs['LoaderRecomputation'])
        analysis = analyze_transition(core, before, after, raw_before, raw_after, inventory, loader,
                                      installed_root_captures=True)
        if analysis != bound_object(root, receipt['Analysis']):
            raise ValueError('Exact mathematical/validator definition review differs.')
        tools = receipt['VerifierTools']
        required_tools = {'constructor_receipts.py', 'constructor_context.py', 'stage_context.py',
                          'supplemental_probes.py', 'run.py', 'generated_cleanup.py',
                          'precision_evidence.py', 'madgraph_precision_evidence.py',
                          'capture-constructor-definitions.wl', 'capture-constructor-definitions.py'}
        expected_tool_paths = {str((Path(here) / name).resolve().relative_to(root)) for name in required_tools}
        if (not isinstance(tools, list) or {r['Path'] for r in tools} != expected_tool_paths
                or len(tools) != len(required_tools)):
            raise ValueError('The actual receipt, resolver, workflow and evidence adapter tools must be bound.')
        current_tool_paths = [bound(root, row) for row in tools]
        archived_tools = receipt.get('HistoricalVerifierTools')
        if not isinstance(archived_tools, list) or not archived_tools:
            raise ValueError('The original verifier-tool preimages must be archived and bound.')
        for row in archived_tools:
            historical = bound(root, row)
            if any(historical == current or historical.samefile(current)
                   for current in current_tool_paths):
                raise ValueError('Historical verifier preimages must not alias mutable current tools.')
        snapshot = bound_object(root, receipt['Snapshot'])
        if (snapshot.get('Status') != 'UNSEALED_VERIFIED_HISTORICAL_SNAPSHOT'
                or snapshot.get('AdmissionContextSHA256') != receipt.get('BeforeContextSHA256')
                or snapshot.get('ActualProducerContextsPreserved') is not True
                or snapshot.get('PriorConditionsPreserved') is not True
                or snapshot.get('NewValidationPassClaimed') is not False):
            raise ValueError('Finite historical snapshot has invalid provenance claims.')
        if snapshot.get('AuxiliaryBindings') or snapshot.get('ExternalAuxiliaryBindings'):
            raise ValueError('Legacy mixed-scope global bindings must be reviewed and scoped explicitly.')
        for row in snapshot['SharedProofBindings']:
            bound(root, row)
        # Current numerical runtime identity is already required by the exact
        # after manifest. Replay compilers and family caches are not global.
        records = snapshot['Records']
        if (not isinstance(records, list) or not records
                or len(records) != snapshot.get('ArtifactCount')
                or sum(r.get('Kind') == 'Native' for r in records) != snapshot.get('NativeCaseCount')
                or sum(r.get('Kind') == 'Export' for r in records) != snapshot.get('ExportCompanionCount')):
            raise ValueError('Finite record counts differ.')
        index, ids = {}, set()
        for row in records:
            path = inside(root, row['SummaryPath'])
            key = (row['Kind'], row['ID'])
            raw = row['RawSummary']
            if (path in index or key in ids or row['Kind'] not in ('Native', 'Export')
                    or row['CapsulePath'] != str(Path(row['SummaryPath']).with_suffix('.wxf'))
                    or row.get('AdmittedAtContextSHA256') != receipt['BeforeContextSHA256']
                    or row.get('ActualProducerContextSHA256') != raw.get('ContextSHA256')
                    or row.get('RecordedStatus') != raw.get('Status')
                    or row.get('CapsuleSHA256') != raw.get('CapsuleSHA256')
                    or raw.get('ID' if row['Kind'] == 'Native' else 'FamilyID') != row['ID']):
                raise ValueError('Record identities, actual producers or statuses differ.')
            history = row.get('PriorHistoricalAdmission')
            if raw['ContextSHA256'] != receipt['BeforeContextSHA256'] and not isinstance(history, dict):
                raise ValueError('An older producer has no original verified conditional admission.')
            if isinstance(history, dict) and (history.get('ValidationRerunPerformed') is not False
                    or history.get('CurrentValidatorPassClaimed') is not False):
                raise ValueError('Prior conditional history was upgraded.')
            # Index immutable snapshot metadata only. Live record cache checks
            # are lazy, so regenerating one case cannot invalidate other cases.
            index[path] = row; ids.add(key)
        # Detect replacement during the metadata review as well as readback.
        bound(root, entry)
        # No raw capsule is imported. Per-record digest checks remain lazy and exact.
        return {'Status': 'AVAILABLE_FINITE_HISTORICAL_RETENTION', 'Reason': None,
            'Proof': {**receipt, '_FiniteConstructorReceiptVerified': True,
                      '_ProofSHA256': entry['SHA256'], '_Records': index,
                      '_Root': root, '_Snapshot': snapshot, '_Analysis': analysis}}
    except (OSError, ValueError, KeyError, TypeError, AttributeError) as error:
        return {'Status': 'UNAVAILABLE', 'Reason': str(error), 'Proof': None}


def verified_proof(root, here, configuration, current_context, *, registry=None, core=None):
    return receipt_status(root, here, configuration, current_context, registry=registry, core=core)['Proof']


def public_proof(proof, stage=None):
    if not isinstance(proof, dict) or proof.get('_FiniteConstructorReceiptVerified') is not True:
        return None
    return {'Status': 'FINITE_HISTORICAL_EVIDENCE_RETENTION', 'Stage': stage,
        'BeforeContextSHA256': proof['BeforeContextSHA256'], 'AfterContextSHA256': proof['AfterContextSHA256'],
        'ReceiptSHA256': proof['_ProofSHA256'], 'HistoricalEvidenceOnly': True,
        **{key: False for key in FALSE_CLAIMS}, 'OriginalProducersAndConditionsPreserved': True,
        'NativeExportInputSupplementalAndPrecisionChecksStillRequired': True}


def record_evidence(summary_path, summary, current_context, proof):
    try:
        if (not isinstance(proof, dict) or proof.get('_FiniteConstructorReceiptVerified') is not True
                or current_context != proof.get('AfterContextSHA256')):
            return None
        path = Path(summary_path).resolve()
        row = proof['_Records'].get(path)
        if row is None:
            return None
        raw = row['RawSummary']
        if (canonical_digest({key: summary.get(key) for key in raw}) != canonical_digest(raw)
                or set(summary) - set(raw) - DERIVED_FIELDS
                or canonical_digest(object_at(path)) != canonical_digest(raw) or digest(path) != row['SummarySHA256']
                or digest(path.with_suffix('.wxf')) != row['CapsuleSHA256']):
            return None
        if row['Kind'] == 'Export':
            # These retain exactly the old precision packet requirements. They
            # never upgrade raw FAIL to PASS and do not affect native admission.
            for dependency in row.get('PrecisionBindings', []):
                bound(proof['_Root'], dependency)
            for dependency in row.get('PrecisionExternalBindings', []):
                dependency_path = Path(dependency['Path'])
                if (set(dependency) != {'Path', 'SHA256'} or not dependency_path.is_absolute()
                        or '..' in dependency_path.parts or digest(dependency_path) != dependency['SHA256']):
                    return None
            members = raw.get('CaseIDs')
            if not isinstance(members, list) or not members or len(members) != len(set(members)):
                return None
            native = {r['ID']: r for r in proof['_Records'].values() if r['Kind'] == 'Native'}
            if any(cid not in native for cid in members):
                return None
            for cid in members:
                item = native[cid]
                if record_evidence(inside(proof['_Root'], item['SummaryPath']), item['RawSummary'], current_context, proof) is None:
                    return None
            hashes = {cid: native[cid]['CapsuleSHA256'] for cid in members}
            contexts = {cid: native[cid]['ActualProducerContextSHA256'] for cid in members}
            mixed = any(value != raw['ContextSHA256'] for value in contexts.values())
            if (raw.get('CaseCapsulesSHA256') != hashes
                    or (('NativeCaseContexts' in raw or mixed) and raw.get('NativeCaseContexts') != contexts)
                    or (('ExporterContextSHA256' in raw or mixed) and raw.get('ExporterContextSHA256') != raw['ContextSHA256'])):
                return None
        return {**public_proof(proof, 'HistoricalExportReport' if row['Kind'] == 'Export' else 'HistoricalNativeValidation'),
            'RecordedStatus': raw['Status'], 'ActualProducerContextSHA256': raw['ContextSHA256'],
            'CurrentContextSHA256': current_context, 'PriorHistoricalAdmission': row.get('PriorHistoricalAdmission'),
            'ExistingPrecisionQualification': row.get('ExistingPrecisionQualification')}
    except (OSError, ValueError, KeyError, TypeError, AttributeError):
        return None


def supplemental_plan(case_id, source_sha, entries, results, context, proof):
    """Choose contexts for exact old probes while current checks stay authoritative.

    No live registry digest is pinned. An unrelated new-case probe cannot stale
    an old case. Every recorded prior ID for the same retained expression must
    still be represented, and new/current failures or unavailable entries are
    never filtered out by historical admission.
    """
    try:
        if (not isinstance(proof, dict) or proof.get('_FiniteConstructorReceiptVerified') is not True
                or proof.get('AfterContextSHA256') != context):
            return None
        selected = [row for row in entries if row['CaseID'] == case_id]
        prior_rows = [row for row in proof['_Records'].values()
                      if row['Kind'] == 'Native' and row['ID'] == case_id
                      and row['CapsuleSHA256'] == source_sha]
        if len(prior_rows) > 1:
            raise ValueError('Ambiguous finite native probe history.')
        prior = []
        if prior_rows:
            row = prior_rows[0]
            if record_evidence(inside(proof['_Root'], row['SummaryPath']), row['RawSummary'], context, proof) is None:
                raise ValueError('The retained native input is missing or changed.')
            prior = row.get('PriorSupplementalEntries', [])
            if not isinstance(prior, list):
                raise ValueError('Missing exact per-native prior probe inventory.')
        old = {row['ID']: row for row in prior}
        if len(old) != len(prior) or set(old) - {row['ID'] for row in selected}:
            raise ValueError('A prior probe ID for this retained input was removed.')
        contexts, historical = {}, []
        for row in selected:
            if row.get('ProbeContextSHA256') == context:
                contexts[row['ID']] = context
            elif row['ID'] in old and row == old[row['ID']]:
                contexts[row['ID']] = row['ProbeContextSHA256']
                historical.append(row['ID'])
            else:
                raise ValueError('An old-context probe differs from its exact per-native admission.')
        return {'Status': 'VERIFIED_CONTEXT_PLAN', 'ContextsByID': contexts,
                'HistoricalIDs': historical}
    except (OSError, ValueError, KeyError, TypeError, AttributeError) as error:
        return {'Status': 'UNAVAILABLE', 'Reason': str(error)}


def record_status(summary_path, summary, current_context, proof):
    evidence = record_evidence(summary_path, summary, current_context, proof)
    return ({'Status': 'AVAILABLE_HISTORICAL_RECORD', 'Evidence': evidence} if evidence is not None
            else {'Status': 'UNAVAILABLE', 'Evidence': None,
                  'Reason': 'Record is outside the finite receipt or its raw evidence/input bindings are missing or changed.'})
