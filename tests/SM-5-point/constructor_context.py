"""Finite historical-validation compatibility for one reviewed constructor change.

This helper does not change admission, checkpoints, status, timings or precision
qualification. It proves that recorded checks used unchanged validator/model/
runtime definitions. Reuse is not a new construction or revalidation. Callers
must retain all ordinary native, supplemental-counterexample, export-input and
precision-evidence checks. No proof is sealed merely by importing this module.
"""
# HEPCAT - High Energy Physics Constructive Amplitude Tool
# Copyright (C) 2026 Neil Christensen
# SPDX-License-Identifier: GPL-3.0-or-later
from pathlib import Path
import hashlib
import importlib.util
import json
import re

FIELDS = ('DownValues', 'UpValues', 'SubValues', 'NValues', 'OwnValues',
          'DefaultValues', 'FormatValues', 'Options', 'Attributes', 'Messages')
CONSTRUCTOR = 'ConstructiveDiagrams`ConstructAmplitude'
LOADER_CACHE = 'ConstructiveDiagrams`Private`$amplitudeParallelLoadedSignature'
BASE_CONTEXT = '370fa1eabb7d41f56e45477db581572ca0d96bed2f4b2c98106c61eb5237706a'
BASE_DIRECTORY = ('tests/results/SM-5-point/compatibility/constructor-prepatch-'
                  + BASE_CONTEXT + '-20261112T211458Z')
ANCHORS = {
    'baseline.json': '0829405d4d190383443120f5e0abf4a3591ed0f4fe5afa5a9296c28b0ce1e1a5',
    'manifest.json': '43ec32f78269461d98210aa9024301cfecf219a3b44cdd2edbd946f90a23efb6',
    'checkpoint-snapshot.json': 'cf61300bce6184c9948dac42b7b356a052fd4fdeb205e9f61af619911956b62c',
    'definition-fingerprints.json': '98e503150f0abf1c9c46c854feebee407ed842b28685051a73e96e4e2869a49b',
}
CHANGED_FILES = ('source/HEPCAT-Model.wl', 'source/HEPCAT.wl')
ADDED_FILES = ('source/internal/HEPCAT-CovariantContacts.wl',
               'source/internal/HEPCAT-CovariantSources.wl')
PRIVATE_PREFIXES = ('HEPCATCovariantCurrentSources`Private`',
                    'HEPCATCovariantContactCalibration`Private`')
CAPTURE_PATTERNS = ('ConstructiveDiagrams`*', 'ConstructiveDiagrams`Private`*',
                    'HEPCATCovariantCurrentSources`*', 'HEPCATCovariantCurrentSources`Private`*',
                    'HEPCATCovariantContactCalibration`*', 'HEPCATCovariantContactCalibration`Private`*')
SIGNATURE_REQUIRED = ('HEPCAT.wl', 'HEPCAT-Base.wl', 'internal/HEPCAT-Parameters.wl',
                      'HEPCAT-Model.wl', 'internal/HEPCAT-Feynman.wl', 'internal/HEPCAT-Color.wl',
                      'internal/HEPCAT-SPINAS.wl', 'internal/HEPCAT-SPINAS-Optimize.wl',
                      'internal/export/spinas-runtime.hpp')
SIGNATURE_OPTIONAL = ('internal/proofs/laurent-proof-runtime.cpp', 'internal/proofs/laurent-proof-runtime.py',
                      'internal/proofs/laurent-proof-factor.py', 'internal/proofs/laurent-proof-verify.py')
REVIEW_FLAGS = ('ExistingImplementationChangesLimitedToConstructAmplitude',
                'PrivateModulesHaveNoLoadTimeExternalSideEffects',
                'NoExistingSymbolRuntimeMutation', 'NoValidationModelOrRuntimeChanges')


def digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def object_at(path):
    value = json.loads(Path(path).read_text())
    if not isinstance(value, dict):
        raise ValueError('Expected a JSON object.')
    return value


def canonical_digest(value):
    return hashlib.sha256(json.dumps(value, sort_keys=True).encode()).hexdigest()


def stage_module():
    spec = importlib.util.spec_from_file_location('hepcat_constructor_stage_context',
                                                 Path(__file__).with_name('stage_context.py'))
    value = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(value)
    return value


def inside(root, relative):
    path = Path(relative)
    if path.is_absolute() or '..' in path.parts:
        raise ValueError('Proof archives must be inside the checkout.')
    result = (Path(root) / path).resolve()
    result.relative_to(Path(root).resolve())
    return result


def definitions(raw, require_post=False):
    """Read exact strings, never normalize code or strip generated suffixes."""
    if (raw.get('PackageLoadedOnce') is not True or raw.get('ModelsRead') != []
            or raw.get('PhysicsCalculationsPerformed') is not False):
        raise ValueError('Cold capture must load the package once without models or physics.')
    if require_post and set(raw.get('CapturedNamePatterns', [])) != set(CAPTURE_PATTERNS):
        raise ValueError('Postcapture must inventory every approved public and private namespace.')
    rows = raw.get('Definitions')
    if not isinstance(rows, dict):
        raise ValueError('Missing exact definition table.')
    result = {}
    full_names = raw.get('FullNames', {})
    for name, fields in rows.items():
        qualified = full_names.get(name, name if '`' in name else 'ConstructiveDiagrams`' + name)
        if (not isinstance(qualified, str) or qualified in result
                or not isinstance(fields, dict) or set(fields) != set(FIELDS)
                or not all(isinstance(value, str) for value in fields.values())):
            raise ValueError('Ambiguous symbol identity or incomplete ten-field definition.')
        result[qualified] = dict(fields)
    if len(result) != raw.get('SymbolCount'):
        raise ValueError('Symbol count does not match the exact table.')
    return result


def cold_pair(first, second, require_post=False):
    a, b = definitions(first, require_post), definitions(second, require_post)
    if (a != b or first.get('WolframVersion') != second.get('WolframVersion')
            or first.get('SystemID') != second.get('SystemID')):
        raise ValueError('Two independent cold captures must match exactly.')
    return a


def _list_items(text):
    """Split a canonical InputForm list without interpreting Wolfram code."""
    if not isinstance(text, str) or not text.startswith('{') or not text.endswith('}'):
        raise ValueError('Expected canonical Messages list.')
    body, items, start, depth, quoted, escape = text[1:-1], [], 0, [], False, False
    pairs = {']': '[', '}': '{', ')': '('}
    for index, char in enumerate(body):
        if quoted:
            if escape:
                escape = False
            elif char == '\\':
                escape = True
            elif char == '"':
                quoted = False
            continue
        if char == '"':
            quoted = True
        elif char in '[{(':
            depth.append(char)
        elif char in ']})':
            if not depth or depth.pop() != pairs[char]:
                raise ValueError('Malformed Messages nesting.')
        elif char == ',' and not depth:
            items.append(body[start:index].strip())
            start = index + 1
    if quoted or depth:
        raise ValueError('Malformed Messages syntax.')
    if body.strip():
        items.append(body[start:].strip())
    return items


_USAGE = re.compile(r'^HoldPattern\[(?:ConstructiveDiagrams`)?ConstructAmplitude::usage\]\s*:>\s*"(?:\\.|[^"\\])*"$')


def usage_only(before, after):
    if before == after:
        return True
    try:
        a, b = _list_items(before), _list_items(after)
        ai = [index for index, value in enumerate(a) if _USAGE.fullmatch(value)]
        bi = [index for index, value in enumerate(b) if _USAGE.fullmatch(value)]
        if len(ai) != 1 or ai != bi or len(a) != len(b):
            return False
        a[ai[0]], b[bi[0]] = '<USAGE>', '<USAGE>'
        return a == b
    except ValueError:
        return False


def _loader_value(text):
    pattern = r'\{HoldPattern\[ConstructiveDiagrams`Private`\$amplitudeParallelLoadedSignature\] :> ([0-9]+)\}'
    match = re.fullmatch(pattern, text)
    if not match:
        raise ValueError('The loader cache is not the exact single integer OwnValue.')
    return int(match.group(1))


def loader_signature_evidence(root, before_manifest, after_manifest, directory):
    """Verify one recorded exact recomputation, without running a kernel or physics.

    The independent review must bind these immutable inputs, the literal
    unchanged file-hash formula and its two actual captured integer results.
    """
    root, directory = Path(root).resolve(), Path(directory).resolve()
    directory.relative_to(root)
    meta_path, values_path, script_path = [directory / name for name in
        ('loader-signature-inputs.json', 'loader-signature-values.json', 'recompute-loader-signature.wl')]
    meta, values = object_at(meta_path), object_at(values_path)
    names = SIGNATURE_REQUIRED + SIGNATURE_OPTIONAL
    rows = meta.get('Inputs')
    if (meta.get('Method') != 'ExactUnchangedLoaderFileHashFormula' or not isinstance(rows, list)
            or [row.get('Name') for row in rows] != list(names)):
        raise ValueError('Loader evidence must contain exactly the original thirteen named inputs.')
    for name, row in zip(names, rows):
        key = 'source/' + name
        archived, current = inside(root, BASE_DIRECTORY) / 'source' / name, root / 'source' / name
        chosen = archived if archived.is_file() else current
        if (row.get('Absent') or row.get('BeforePath') != str(chosen)
                or row.get('AfterPath') != str(current)
                or row.get('ArchivedCopyUsed') is not (chosen == archived)
                or row.get('BeforeSHA256') != before_manifest['Files'].get(key)
                or row.get('AfterSHA256') != after_manifest['Files'].get(key)
                or digest(chosen) != row['BeforeSHA256'] or digest(current) != row['AfterSHA256']):
            raise ValueError('Loader-signature source inputs are not the pinned exact bytes.')
    expected_paths = {'BeforeRequired': [row['BeforePath'] for row in rows[:len(SIGNATURE_REQUIRED)]],
                      'BeforeOptional': [row['BeforePath'] for row in rows[len(SIGNATURE_REQUIRED):]],
                      'AfterRequired': [row['AfterPath'] for row in rows[:len(SIGNATURE_REQUIRED)]],
                      'AfterOptional': [row['AfterPath'] for row in rows[len(SIGNATURE_REQUIRED):]]}
    if any(meta.get(key) != value for key, value in expected_paths.items()):
        raise ValueError('Loader-signature input ordering differs from the unchanged formula.')
    code = ''.join(name[0].lower() + name[1:] + '=' + json.dumps(paths).replace('[', '{').replace(']', '}') + ';\n'
                   for name, paths in expected_paths.items())
    code += ('record=<|"Before"->Hash[Join[FileHash/@beforeRequired,FileHash/@beforeOptional]],'
             '"After"->Hash[Join[FileHash/@afterRequired,FileHash/@afterOptional]],'
             '"Method"->"ExactUnchangedLoaderFileHashFormulaVerifiedArchiveOrIdenticalSHAInputs",'
             '"PhysicsCalculationsPerformed"->False|>;\nExport[' + json.dumps(str(values_path))
             + ',record,"RawJSON"];\nPrint[record];\n')
    if (script_path.read_text() != code or values.get('PhysicsCalculationsPerformed') is not False
            or values.get('Method') != 'ExactUnchangedLoaderFileHashFormulaVerifiedArchiveOrIdenticalSHAInputs'
            or any(type(values.get(key)) is not int for key in ('Before', 'After'))):
        raise ValueError('The actual loader recomputation record or literal script differs.')
    return {'Before': values['Before'], 'After': values['After'], 'InputCount': len(names),
            'InputsSHA256': digest(meta_path), 'ValuesSHA256': digest(values_path),
            'ScriptSHA256': digest(script_path), '_VerifiedInputHashes': True}


def compare_transition(before_manifest, after_manifest, before_raw, after_raw, loader_evidence=None):
    """Analyze exact input data only; this does not authorize runtime reuse."""
    stage = stage_module()
    if before_manifest.get('Programs') != after_manifest.get('Programs'):
        raise ValueError('Runtime installation paths changed.')
    before_files, after_files = before_manifest['Files'], after_manifest['Files']
    if set(before_files) - set(after_files):
        raise ValueError('An existing manifest path was removed.')
    changed = sorted(name for name in before_files if before_files[name] != after_files[name])
    added = sorted(set(after_files) - set(before_files))
    if changed != sorted(CHANGED_FILES) or added != sorted(ADDED_FILES):
        raise ValueError('Expected only the literal constructor/loader change and two private modules.')
    a = cold_pair(*before_raw)
    b = cold_pair(*after_raw, require_post=True)
    if (before_raw[0].get('WolframVersion') != after_raw[0].get('WolframVersion')
            or before_raw[0].get('SystemID') != after_raw[0].get('SystemID')):
        raise ValueError('Wolfram version or platform changed.')
    if set(a) - set(b):
        raise ValueError('An existing symbol disappeared.')
    changed_fields = {}
    for name, fields in a.items():
        differences = [field for field in FIELDS if fields[field] != b[name][field]]
        if not differences:
            continue
        if name == LOADER_CACHE:
            if (differences != ['OwnValues'] or not isinstance(loader_evidence, dict)
                    or loader_evidence.get('_VerifiedInputHashes') is not True
                    or _loader_value(fields['OwnValues']) != loader_evidence.get('Before')
                    or _loader_value(b[name]['OwnValues']) != loader_evidence.get('After')):
                raise ValueError('The mechanical loader cache needs both exact verified recomputations.')
            changed_fields[name] = differences
            continue
        if name != CONSTRUCTOR or not set(differences) <= {'DownValues', 'Messages'}:
            raise ValueError('An existing nonconstructor definition or protected constructor field changed: ' + name)
        if 'Messages' in differences and not usage_only(fields['Messages'], b[name]['Messages']):
            raise ValueError('Constructor Messages changes must be usage-only.')
        changed_fields[name] = differences
    if 'DownValues' not in changed_fields.get(CONSTRUCTOR, []):
        raise ValueError('No reviewed constructor implementation change was found.')
    new_defined, new_empty, new_temporary = [], [], []
    for name in sorted(set(b) - set(a)):
        fields = b[name]
        if any(name.startswith(prefix) for prefix in PRIVATE_PREFIXES):
            if any(value != '{}' for value in fields.values()):
                new_defined.append(name)
            else:
                new_empty.append(name)
        elif name.startswith('ConstructiveDiagrams`Private`') and all(value == '{}' for value in fields.values()):
            new_empty.append(name)
        elif (name.startswith('ConstructiveDiagrams`Private`') and name.endswith('$')
              and fields['Attributes'] == '{Temporary}'
              and all(value == '{}' for field, value in fields.items() if field != 'Attributes')):
            new_temporary.append(name)
        else:
            raise ValueError('New public, unreviewed private, or nonempty Module-local symbol: ' + name)
    return {'Kind': 'ExactConstructorOnlyHistoricalValidationTransition',
            'Status': 'ANALYZED_NOT_SEALED', 'BeforeContextSHA256': stage.fingerprint(before_manifest),
            'AfterContextSHA256': stage.fingerprint(after_manifest),
            'ChangedExistingFiles': changed, 'AddedFiles': added,
            'ChangedDefinitionFields': changed_fields, 'ExistingSymbolsChecked': len(a),
            'NewDefinedPrivateSymbols': new_defined, 'NewEmptyPrivateSymbols': new_empty,
            'NewTemporaryConstructorSymbols': new_temporary,
            'MechanicalLoaderSignatureEvidence': {key: value for key, value in (loader_evidence or {}).items() if not key.startswith('_')},
            'AllOtherTenFieldDefinitionsExactlyUnchanged': True,
            'BeforeColdDefinitionsSHA256': canonical_digest(a),
            'AfterColdDefinitionsSHA256': canonical_digest(b),
            'ValidationRerunPerformed': False, 'ConstructorRerunPerformed': False}


def load_baseline(root):
    """Load only the one prepatch archive explicitly reviewed in this campaign."""
    directory = inside(root, BASE_DIRECTORY)
    for name, expected in ANCHORS.items():
        if digest(directory / name) != expected:
            raise ValueError('The pinned preconstructor baseline changed: ' + name)
    baseline = object_at(directory / 'baseline.json')
    seal = object_at(directory / 'baseline-seal.json')
    if (baseline.get('ContextSHA256') != BASE_CONTEXT or baseline.get('Status') != 'BASELINE_COMPLETE'
            or seal.get('Status') != 'SEALED_BASELINE_ONLY'
            or seal.get('BaselineSHA256') != ANCHORS['baseline.json']
            or seal.get('ManifestSHA256') != ANCHORS['manifest.json']
            or seal.get('CheckpointSnapshotSHA256') != ANCHORS['checkpoint-snapshot.json']
            or seal.get('DefinitionFingerprintsSHA256') != ANCHORS['definition-fingerprints.json']):
        raise ValueError('The baseline seal is not the reviewed frozen evidence.')
    fingerprint = object_at(directory / 'definition-fingerprints.json')
    raw = []
    for name, expected in fingerprint['RawCaptureFiles'].items():
        if Path(name).name != name or digest(directory / name) != expected:
            raise ValueError('A pinned baseline cold capture changed.')
        raw.append(object_at(directory / name))
    if len(raw) != 2:
        raise ValueError('The baseline needs exactly two independent cold captures.')
    table = cold_pair(*raw)
    for name, fields in table.items():
        actual = {field: hashlib.sha256(value.encode()).hexdigest() for field, value in fields.items()}
        if fingerprint['Definitions'].get(name, {}).get('Fields') != actual:
            raise ValueError('The pinned baseline field fingerprints do not reconstruct.')
    exporter = object_at(directory / 'existing-exporter-context-bridge.json')
    if digest(Path(root) / 'tests/SM-5-point/exporter-context-bridge.json') != baseline['ExistingExporterBridgeSHA256']:
        raise ValueError('The original sealed exporter bridge changed.')
    if digest(directory / 'existing-exporter-context-bridge.json') != baseline['ExistingExporterBridgeSHA256']:
        raise ValueError('The archived exporter bridge changed.')
    archives = {}
    for kind, row in baseline['ExistingExporterBridgeArchives'].items():
        path = directory / row['ArchiveFile']
        if (Path(row['ArchiveFile']).name != row['ArchiveFile'] or digest(path) != row['SHA256']
                or digest(inside(root, row['OriginalPath'])) != row['SHA256']):
            raise ValueError('An archived exporter proof changed.')
        archives[kind] = object_at(path)
    stage = stage_module()
    if (exporter.get('Status') != 'SEALED' or exporter.get('ExporterContextSHA256') != BASE_CONTEXT
            or stage.fingerprint(archives['AfterManifest']) != BASE_CONTEXT
            or not stage._unchanged_except_exporter(archives['BeforeManifest'], archives['AfterManifest'])):
        raise ValueError('The old exporter proof does not end at the frozen baseline.')
    return {'Directory': directory, 'Record': baseline,
            'Manifest': object_at(directory / 'manifest.json'), 'Raw': raw,
            'Snapshot': object_at(directory / 'checkpoint-snapshot.json'),
            'ExporterBridge': exporter, 'ExporterArchives': archives}


def analyze(root, after_manifest_path, first_capture_path, second_capture_path, loader_evidence_directory=None):
    baseline = load_baseline(root)
    after = object_at(after_manifest_path)
    raws = [object_at(first_capture_path), object_at(second_capture_path)]
    evidence = (loader_signature_evidence(root, baseline['Manifest'], after, loader_evidence_directory)
                if loader_evidence_directory is not None else None)
    report = compare_transition(baseline['Manifest'], after, baseline['Raw'], raws, evidence)
    if report['BeforeContextSHA256'] != BASE_CONTEXT:
        raise ValueError('Unexpected constructor baseline context.')
    report.update(BaselineDirectory=BASE_DIRECTORY, BaselineSHA256=ANCHORS['baseline.json'],
                  BeforeManifestSHA256=ANCHORS['manifest.json'],
                  CheckpointSnapshotSHA256=ANCHORS['checkpoint-snapshot.json'],
                  AfterManifestSHA256=digest(after_manifest_path),
                  AfterCaptureSHA256=[digest(first_capture_path), digest(second_capture_path)])
    return report


def reviewed_payload(analysis, review):
    """Build a proof payload only after an exact pinned code-side-effect review."""
    if (analysis.get('Status') != 'ANALYZED_NOT_SEALED'
            or review.get('Status') != 'REVIEWED_CONSTRUCTOR_ONLY_LOADER_PRIVATE_MODULES'
            or review.get('AnalysisSHA256') != canonical_digest(analysis)
            or not isinstance(review.get('ReviewedBy'), str) or not review['ReviewedBy'].strip()
            or not all(review.get(flag) is True for flag in REVIEW_FLAGS)
            or (analysis.get('MechanicalLoaderSignatureEvidence')
                and review.get('LoaderSignatureInputsAndExactRecomputedValuesReviewed') is not True)
            or (analysis.get('NewTemporaryConstructorSymbols')
                and review.get('ListedConstructorTemporarySymbolsReviewed') is not True)):
        raise ValueError('The exact code and load/runtime side-effect review is missing.')
    return {**analysis, 'Status': 'SEALED', 'AnalysisSHA256': canonical_digest(analysis),
            'CodeReviewSHA256': canonical_digest(review),
            'HistoricalEvidenceOnly': True, 'CurrentConstructorSuccessClaimed': False,
            'ValidationRerunPerformed': False, 'ConstructorRerunPerformed': False,
            'SupplementalCounterexampleChecksStillRequired': True,
            'PrecisionQualificationUnchangedRequired': True}


def checkpoint_evidence(summary_path, summary, capsule_path, proof, baseline):
    """Finite historical evidence only, never a PASS or construction decision."""
    try:
        name = Path(summary_path).name
        row = baseline['Snapshot'][name]
        recorded = summary['ContextSHA256']
        if (proof.get('_VerifiedCurrentContext') is not True or proof.get('Status') != 'SEALED' or proof.get('BeforeContextSHA256') != BASE_CONTEXT
                or proof.get('HistoricalEvidenceOnly') is not True
                or proof.get('CurrentConstructorSuccessClaimed') is not False
                or recorded != row['RecordedContextSHA256'] or not row['SummaryValidJSON']
                or digest(summary_path) != row['SummarySHA256'] or summary.get('Status') != row['Status']):
            return None
        capsule_path = Path(capsule_path)
        if row['CapsuleExists']:
            if (not row['DeclaredCapsuleMatches'] or not capsule_path.is_file()
                    or digest(capsule_path) != row['CapsuleSHA256']
                    or summary.get('CapsuleSHA256') != row['CapsuleSHA256']):
                return None
        elif capsule_path.exists() or summary.get('Status') in ('COMPLETE', 'PASS'):
            return None
        chain = []
        if recorded != BASE_CONTEXT:
            old = baseline['ExporterBridge']
            old_row = baseline['ExporterArchives']['CheckpointSnapshot'][name]
            if (recorded != old['NativeContextSHA256']
                    or old_row['RecordedContextSHA256'] != recorded
                    or old_row['SummarySHA256'] != row['SummarySHA256']
                    or old_row.get('CapsuleSHA256') != row.get('CapsuleSHA256')):
                return None
            chain.append({'Kind': 'ExactExporterOnlyTransition',
                          'FromContextSHA256': recorded, 'ToContextSHA256': BASE_CONTEXT,
                          'BridgeSHA256': baseline['Record']['ExistingExporterBridgeSHA256']})
        chain.append({'Kind': proof['Kind'], 'FromContextSHA256': BASE_CONTEXT,
                      'ToContextSHA256': proof['AfterContextSHA256'],
                      'AnalysisSHA256': proof['AnalysisSHA256']})
        return {'Status': 'HISTORICAL_VALIDATION_COMPATIBLE_CONSTRUCTOR_CHANGE',
                'RecordedContextSHA256': recorded, 'CurrentContextSHA256': proof['AfterContextSHA256'],
                'Stage': 'HistoricalExportReport' if '-export.' in name else 'HistoricalNativeValidation',
                'CompatibilityChain': chain, 'OriginalTimingAndOptionsPreserved': True,
                'ValidationRerunPerformed': False, 'ConstructorRerunPerformed': False,
                'CurrentConstructorSuccessClaimed': False,
                'SupplementalCounterexampleChecksStillRequired': True,
                'PrecisionQualificationUnchangedRequired': True}
    except (OSError, KeyError, TypeError, ValueError):
        return None


def seal_payload(root, here, configuration, after_manifest_path, first_capture_path,
                 second_capture_path, review_path, loader_evidence_directory=None):
    """Prepare, but do not write, the one exact fully reviewed sealed payload."""
    root, here = Path(root), Path(here)
    paths = [Path(after_manifest_path), Path(first_capture_path),
             Path(second_capture_path), Path(review_path)]
    relative = []
    for path in paths:
        relative.append(str(path.resolve().relative_to(root.resolve())))
    analysis = analyze(root, *paths[:3], loader_evidence_directory)
    stage = stage_module()
    actual = stage.manifest(root, here, configuration)
    after = object_at(paths[0])
    if (actual != after or stage.fingerprint(actual) != analysis['AfterContextSHA256']
            or analysis['AfterContextSHA256'] == BASE_CONTEXT):
        raise ValueError('The exact reviewed postpatch manifest is not current.')
    review = object_at(paths[3])
    payload = reviewed_payload(analysis, review)
    payload.update(AfterManifest=relative[0], AfterCaptures=relative[1:3],
                   CodeReview=relative[3], CodeReviewFileSHA256=digest(paths[3]))
    if loader_evidence_directory is not None:
        payload['LoaderSignatureEvidenceDirectory'] = str(Path(loader_evidence_directory).resolve().relative_to(root.resolve()))
    return payload


def verified_proof(root, here, configuration, current_context, proof_path):
    """Read-only verification; callers decide status using their ordinary checks."""
    try:
        proof = object_at(proof_path)
        if (proof.get('Status') != 'SEALED'
                or proof.get('Kind') != 'ExactConstructorOnlyHistoricalValidationTransition'
                or proof.get('AfterContextSHA256') != current_context):
            return None
        captures = proof['AfterCaptures']
        if not isinstance(captures, list) or len(captures) != 2:
            return None
        paths = [inside(root, proof['AfterManifest']),
                 inside(root, captures[0]), inside(root, captures[1]),
                 inside(root, proof['CodeReview'])]
        if digest(paths[3]) != proof['CodeReviewFileSHA256']:
            return None
        loader_directory = (inside(root, proof['LoaderSignatureEvidenceDirectory'])
                            if proof.get('LoaderSignatureEvidenceDirectory') else None)
        expected = seal_payload(root, here, configuration, *paths, loader_directory)
        if proof != expected:
            return None
        return {**proof, '_VerifiedCurrentContext': True,
                '_ProofSHA256': digest(proof_path), '_Baseline': load_baseline(root)}
    except (OSError, ValueError, KeyError, TypeError, AttributeError):
        return None


def c10_module():
    spec = importlib.util.spec_from_file_location('hepcat_constructor_context_c10',
                                                 Path(__file__).with_name('validation_context_c10.py'))
    value = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(value)
    return value


def c9_module():
    spec = importlib.util.spec_from_file_location('hepcat_constructor_context_c9',
                                                 Path(__file__).with_name('validation_context_c9.py'))
    value = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(value)
    return value


def c8_module():
    spec = importlib.util.spec_from_file_location('hepcat_constructor_context_c8',
                                                 Path(__file__).with_name('validation_context_c8.py'))
    value = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(value)
    return value


def c7_module():
    spec = importlib.util.spec_from_file_location('hepcat_constructor_context_c7',
                                                 Path(__file__).with_name('validation_context_c7.py'))
    value = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(value)
    return value


def c6_module():
    spec = importlib.util.spec_from_file_location('hepcat_c6_validation_context',
                                                 Path(__file__).with_name('validation_context_c6.py'))
    value = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(value)
    return value


def c5_module():
    spec = importlib.util.spec_from_file_location('hepcat_c5_validation_context',
                                                 Path(__file__).with_name('validation_context_c5.py'))
    value = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(value)
    return value


def c4_module():
    spec = importlib.util.spec_from_file_location('hepcat_c4_validation_context',
                                                 Path(__file__).with_name('validation_context_c4.py'))
    value = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(value)
    return value


def public_proof(proof, stage=None):
    if proof.get('_C10VerifiedCurrentContext') is True:
        return c10_module().public_proof(proof, stage)
    if proof.get('_C9VerifiedCurrentContext') is True:
        return c9_module().public_proof(proof, stage)
    if proof.get('_C8VerifiedCurrentContext') is True:
        return c8_module().public_proof(proof, stage)
    if proof.get('_C7VerifiedCurrentContext') is True:
        return c7_module().public_proof(proof, stage)
    """Describe the actual provenance without repeating cached definition tables."""
    if proof.get('_C6VerifiedCurrentContext') is True:
        return c6_module().public_proof(proof, stage)
    if proof.get('_C5VerifiedCurrentContext') is True:
        return c5_module().public_proof(proof, stage)
    if proof.get('_C4VerifiedCurrentContext') is True:
        return c4_module().public_proof(proof, stage)
    if stage is None:
        return {key: value for key, value in proof.items() if not key.startswith('_')}
    if proof.get('_VerifiedCurrentContext') is not True:
        return stage_module().public_proof(proof, stage)
    return {'Status': 'EXACT_CONSTRUCTOR_ONLY_HISTORICAL_VALIDATION_COMPATIBILITY',
            'Stage': stage, 'BeforeContextSHA256': proof['BeforeContextSHA256'],
            'AfterContextSHA256': proof['AfterContextSHA256'],
            'AnalysisSHA256': proof['AnalysisSHA256'],
            'CodeReviewFileSHA256': proof['CodeReviewFileSHA256'],
            'ProofSHA256': proof['_ProofSHA256'],
            'HistoricalEvidenceOnly': True, 'OriginalTimingAndOptionsPreserved': True,
            'ValidationRerunPerformed': False, 'ConstructorRerunPerformed': False,
            'CurrentConstructorSuccessClaimed': False,
            'SupplementalCounterexampleChecksStillRequired': True,
            'PrecisionQualificationUnchangedRequired': True}


def resolve(root, here, configuration, current_context):
    """Resolve one exact reviewed historical transition; never infer a migration."""
    current = c10_module().verified_proof(root, here, configuration, current_context,
                                         Path(here) / 'constructor-C9-C10-context-bridge.json')
    if current is not None:
        return current
    current = c9_module().verified_proof(root, here, configuration, current_context,
                                        Path(here) / 'constructor-C8-C9-context-bridge.json')
    if current is not None:
        return current
    current = c8_module().verified_proof(root, here, configuration, current_context,
                                        Path(here) / 'constructor-C7-C8-context-bridge.json')
    if current is not None:
        return current
    current = c7_module().verified_proof(root, here, configuration, current_context,
                                        Path(here) / 'constructor-C6-C7-context-bridge.json')
    if current is not None:
        return current
    current = c6_module().verified_proof(root, here, configuration, current_context,
                                        Path(here) / 'constructor-C5-C6-context-bridge.json')
    if current is not None:
        return current
    newest = c5_module().verified_proof(root, here, configuration, current_context,
                                      Path(here) / 'constructor-C4-C5-context-bridge.json')
    if newest is not None:
        return newest
    extended = c4_module().verified_proof(root, here, configuration, current_context,
                                         Path(here) / 'constructor-C3-C4-context-bridge.json')
    if extended is not None:
        return extended
    proof = verified_proof(root, here, configuration, current_context,
                           Path(here) / 'constructor-context-bridge.json')
    return proof or stage_module().verified_bridge(root, here, configuration, current_context)


def record_evidence(summary_path, summary, current_context, proof):
    """Verify a finite old record while retaining its actual producer context."""
    if summary.get('ContextSHA256') == current_context:
        return None
    if not isinstance(proof, dict):
        return None
    if proof.get('_C10VerifiedCurrentContext') is True:
        return c10_module().record_evidence(summary_path, summary, current_context, proof)
    if proof.get('_C9VerifiedCurrentContext') is True:
        return c9_module().record_evidence(summary_path, summary, current_context, proof)
    if proof.get('_C8VerifiedCurrentContext') is True:
        return c8_module().record_evidence(summary_path, summary, current_context, proof)
    if proof.get('_C7VerifiedCurrentContext') is True:
        return c7_module().record_evidence(summary_path, summary, current_context, proof)
    if proof.get('_C6VerifiedCurrentContext') is True:
        return c6_module().record_evidence(summary_path, summary, current_context, proof)
    if proof.get('_C5VerifiedCurrentContext') is True:
        return c5_module().record_evidence(summary_path, summary, current_context, proof)
    if proof.get('_C4VerifiedCurrentContext') is True:
        return c4_module().record_evidence(summary_path, summary, current_context, proof)
    if proof.get('_VerifiedCurrentContext') is True:
        if proof.get('AfterContextSHA256') != current_context:
            return None
        return checkpoint_evidence(summary_path, summary, Path(summary_path).with_suffix('.wxf'),
                                   proof, proof.get('_Baseline', {}))
    stage = stage_module()
    if (not stage.admits(summary.get('ContextSHA256'), current_context, proof)
            or not stage.checkpoint_matches(summary_path, summary, proof)):
        return None
    capsule = Path(summary_path).with_suffix('.wxf')
    if summary.get('CapsuleSHA256') is not None and (
            not capsule.is_file() or digest(capsule) != summary['CapsuleSHA256']):
        return None
    return stage.public_proof(proof,
        'HistoricalExportReport' if '-export.' in Path(summary_path).name else 'NativeCalculation')


def admits_record(summary_path, summary, current_context, proof):
    return (summary.get('ContextSHA256') == current_context
            or record_evidence(summary_path, summary, current_context, proof) is not None)


def main():
    import argparse
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--after-manifest', required=True, type=Path)
    parser.add_argument('--first-capture', required=True, type=Path)
    parser.add_argument('--second-capture', required=True, type=Path)
    parser.add_argument('--review', type=Path)
    parser.add_argument('--loader-evidence-directory', type=Path)
    parser.add_argument('--configuration', type=Path)
    parser.add_argument('--output', type=Path)
    parser.add_argument('--seal', action='store_true',
                        help='Requires an exact code review; never changes runtime admission.')
    args = parser.parse_args()
    here = Path(__file__).resolve().parent
    root = here.parents[1]
    if args.seal:
        if not args.review or not args.output:
            parser.error('--seal requires --review and a new --output proof path.')
        config = object_at(args.configuration or root / 'tests/results/SM-5-point/configuration.json')
        result = seal_payload(root, here, config, args.after_manifest, args.first_capture,
                              args.second_capture, args.review, args.loader_evidence_directory)
    else:
        result = analyze(root, args.after_manifest, args.first_capture, args.second_capture, args.loader_evidence_directory)
        result['AnalysisSHA256'] = canonical_digest(result)
    if args.output:
        if args.output.exists():
            parser.error('Proof/analysis files are immutable; choose a new output path.')
        args.output.parent.mkdir(parents=True, exist_ok=True)
        temporary = args.output.with_suffix(args.output.suffix + '.tmp')
        temporary.write_text(json.dumps(result, indent=2, sort_keys=True) + '\n')
        temporary.replace(args.output)
    print(json.dumps(result, indent=2, sort_keys=True))
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
