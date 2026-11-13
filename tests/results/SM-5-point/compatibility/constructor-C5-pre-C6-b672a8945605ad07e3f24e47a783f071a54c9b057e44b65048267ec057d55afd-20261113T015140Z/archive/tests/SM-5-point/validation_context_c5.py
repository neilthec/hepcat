"""Narrow, reference-bound historical validation continuity for C4 to C5.

This metadata helper cannot promote a construction or claim a new check. It
requires literal cold definitions, exact code diffs and an independent review.
The old sealed proof is referenced by hash and its tiny verifier is frozen;
old proof archives and native amplitude capsules are never recopied.
"""
from pathlib import Path
import hashlib
import importlib.util
import json
import re

BASE_CONTEXT = 'a91edec8580c2721ccbe8d5304442e9e506f9fd89dc911c250a550c31723a238'
BASE_DIRECTORY = ('tests/results/SM-5-point/compatibility/constructor-C4-pre-C5-'
                  + BASE_CONTEXT + '-20261113T002645Z')
BASELINE_SHA256 = 'afcc27a0bb823dc0f9ed4c53e18eb6dafd2c737d2edf4b876dfa350168862677'
PRIOR_PROOF_SHA256 = '22f297d0ed6747f17b16a077ca4e9f9bea3a5f531904371a34c77f899a71ee0c'
REGISTRY = 'constructor-C4-C5-context-bridge.json'
KIND = 'ExactCheckpointAndGradedColourHistoricalValidationTransition'
CHANGED_FILES = ('source/HEPCAT-Model.wl', 'source/internal/HEPCAT-Color.wl',
                 'source/internal/HEPCAT-CovariantContacts.wl')
PREFIX = 'ConstructiveDiagrams`Private`'
CONSTRUCTOR = 'ConstructiveDiagrams`ConstructAmplitude'
FACTORIZATION = PREFIX + 'completionFactorizationCore'
COLOUR_CONSTRUCTOR = PREFIX + 'colorConstructAmplitude'
CONTACT = 'HEPCATCovariantContactCalibration`Private`calibratedCovariantSource'
IMPLEMENTATIONS = (CONSTRUCTOR, FACTORIZATION, COLOUR_CONSTRUCTOR, CONTACT)
NEW_FUNCTION_PREFIXES = (PREFIX + 'colorOrbit',)
EXTRA_NEW_FUNCTIONS = (PREFIX + 'completionFactorizationPointCheckpoint',
                       PREFIX + 'constructFactorizationFittedPoint')
FIELDS = ('DownValues', 'UpValues', 'SubValues', 'NValues', 'OwnValues',
          'DefaultValues', 'FormatValues', 'Options', 'Attributes', 'Messages')
REVIEW_FLAGS = (
    'ExistingCheckersModelsAndNumericalRuntimeUnchanged',
    'FittedCheckpointRetainsExactSolverPointOnly',
    'KnownCheckpointValidationFailuresCannotBeOverridden',
    'PreparedCheckpointUsesOriginalAllParameterValidation',
    'IdenticalParticleTransportIsTypedAndGraded',
    'EveryColourComponentRetainsIndependentValidation',
    'CompletedColourReportsRetainedWithoutFalseSuccess',
    'NeutralVectorSourceUsesOnlyExistingOperatorsAndLowerContact',
    'NoAnswerOrIndependentBenchmarkUsedForCandidateConstruction',
    'NewHelpersHaveNoLoadTimeSideEffects',
    'ColourOrbitTypedGradingControlsReviewed',
    'CheckpointSuccessfulAndFailurePathControlsReviewed',
    'MechanicalLoaderSignatureReviewed',
    'LiteralSourceAndDefinitionChangesReviewed',
)


def digest(path):
    value = hashlib.sha256()
    with Path(path).open('rb') as stream:
        for block in iter(lambda: stream.read(2**20), b''):
            value.update(block)
    return value.hexdigest()


def object_at(path):
    value = json.loads(Path(path).read_text())
    if not isinstance(value, dict):
        raise ValueError('Expected a JSON object.')
    return value


def canonical_digest(value):
    return hashlib.sha256(json.dumps(value, sort_keys=True).encode()).hexdigest()


def inside(root, relative):
    value = Path(relative)
    if value.is_absolute() or '..' in value.parts:
        raise ValueError('Proof paths must remain inside the checkout.')
    result = (Path(root) / value).resolve()
    result.relative_to(Path(root).resolve())
    return result


def frozen_module(root, name):
    path = inside(root, BASE_DIRECTORY) / 'archive/tests/SM-5-point' / name
    spec = importlib.util.spec_from_file_location('hepcat_frozen_c4_' + path.stem, path)
    value = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(value)
    return value


def before_source(baseline, name):
    """Reuse unchanged files only at their exact old hash; copy changed files once."""
    root = baseline['Root']
    choices = (baseline['Archive'] / name,
               baseline['Directory'] / 'supplemental-before-sources' / name,
               root / name)
    for path in choices:
        if path.is_file() and digest(path) == baseline['Manifest']['Files'].get(name):
            return path
    raise ValueError('Literal before-source bytes are unavailable: ' + name)


def load_baseline(root):
    root = Path(root).resolve()
    directory = inside(root, BASE_DIRECTORY)
    if digest(directory / 'baseline.json') != BASELINE_SHA256:
        raise ValueError('The sealed C4 baseline changed.')
    record = object_at(directory / 'baseline.json')
    if (record.get('Status') != 'SEALED' or record.get('ContextSHA256') != BASE_CONTEXT
            or record.get('RecursiveProofArchiveCopiesCreated') is not False):
        raise ValueError('The exact reference-bound C4 baseline is required.')
    for field in ('Manifest', 'CheckpointSnapshot', 'ArchiveIndex', 'PriorProofReferences', 'DefinitionFingerprints'):
        if digest(directory / record[field]) != record[field + 'SHA256']:
            raise ValueError('A pinned C4 baseline artifact changed: ' + field)
    archive = directory / 'archive'
    index = object_at(directory / record['ArchiveIndex'])['Files']
    for name, row in index.items():
        if not name.startswith('archive/') or digest(inside(directory, name)) != row['SHA256']:
            raise ValueError('A frozen C4 source, verifier or summary changed: ' + name)
    references = object_at(directory / record['PriorProofReferences'])
    tools = references['RequiredCurrentVerifier']
    for name, expected in references['Files'].items():
        # The C5 dispatcher may change. The exact prior dispatcher was copied once.
        path = inside(archive, name) if name in tools else inside(root, name)
        if digest(path) != expected:
            raise ValueError('An immutable prior proof reference changed: ' + name)
    for name, expected in tools.items():
        if digest(inside(archive, name)) != expected:
            raise ValueError('A frozen prior verifier changed: ' + name)
    prior = object_at(inside(root, references['PriorProof']))
    if (digest(inside(root, references['PriorProof'])) != PRIOR_PROOF_SHA256
            or references['PriorProofSHA256'] != PRIOR_PROOF_SHA256
            or prior.get('Status') != 'SEALED' or prior.get('AfterContextSHA256') != BASE_CONTEXT):
        raise ValueError('The exact already reviewed C4 proof is required.')
    core = frozen_module(root, 'constructor_context.py')
    fingerprints = object_at(directory / record['DefinitionFingerprints'])
    paths = [inside(root, name) for name in fingerprints['SourceCaptures']]
    if [digest(path) for path in paths] != fingerprints['SourceCaptureSHA256']:
        raise ValueError('The exact two C4 cold captures are required.')
    raw = [object_at(path) for path in paths]
    table = core.cold_pair(*raw, require_post=True)
    if len(table) != 8160 or canonical_digest(table) != fingerprints['DefinitionsSHA256']:
        raise ValueError('The exact C4 cold definitions do not reconstruct.')
    entries = object_at(directory / record['CheckpointSnapshot'])['Entries']
    snapshot = {Path(row['Summary']).name: row for row in entries}
    if len(snapshot) != len(entries) or len(snapshot) != 326:
        raise ValueError('The finite C4 checkpoint list changed.')
    old = frozen_module(root, 'validation_context_c4.py')
    old_proof = {**prior, '_C4VerifiedCurrentContext': True, '_VerifiedCurrentContext': True,
                 '_ProofSHA256': PRIOR_PROOF_SHA256, '_Baseline': old.load_baseline(root)}
    return {'Root': root, 'Directory': directory, 'Archive': archive, 'Record': record,
            'Manifest': object_at(directory / record['Manifest']), 'Raw': raw,
            'Snapshot': snapshot, 'PriorVerifier': old, 'PriorProof': old_proof, 'Core': core}


def usage_only(core, symbol, before, after):
    """Only that public symbol's usage message may change, never an error message."""
    try:
        a, b = core._list_items(before), core._list_items(after)
        short = symbol.split('`')[-1]
        usage = re.compile(r'^HoldPattern\[(?:ConstructiveDiagrams`)?' + re.escape(short)
                           + r'::usage\]\s*:>\s*"(?:\\.|[^"\\])*"$')
        ai = [i for i, value in enumerate(a) if usage.fullmatch(value)]
        bi = [i for i, value in enumerate(b) if usage.fullmatch(value)]
        if len(ai) != 1 or ai != bi or len(a) != len(b):
            return False
        a[ai[0]], b[bi[0]] = '<USAGE>', '<USAGE>'
        return a == b
    except (ValueError, TypeError):
        return False


def compare_transition(core, before, after, before_raw, after_raw, loader_evidence=None):
    """Analyze a literal candidate patch. Analysis alone never grants admission."""
    if before.get('Programs') != after.get('Programs') or set(before['Files']) != set(after['Files']):
        raise ValueError('Installation paths or manifest membership changed.')
    changed = sorted(name for name in before['Files'] if before['Files'][name] != after['Files'][name])
    if changed != sorted(CHANGED_FILES):
        raise ValueError('Only the three explicitly reviewed source files may change.')
    a, b = core.cold_pair(*before_raw, require_post=True), core.cold_pair(*after_raw, require_post=True)
    if any(before_raw[0].get(k) != after_raw[0].get(k) for k in ('WolframVersion', 'SystemID')):
        raise ValueError('Wolfram version or platform changed.')
    if set(a) - set(b):
        raise ValueError('An existing C4 symbol disappeared.')
    differences = {}
    for name, fields in a.items():
        delta = [field for field in FIELDS if fields[field] != b[name][field]]
        if not delta:
            continue
        if name == core.LOADER_CACHE:
            if (delta != ['OwnValues'] or not isinstance(loader_evidence, dict)
                    or loader_evidence.get('_VerifiedInputHashes') is not True
                    or core._loader_value(fields['OwnValues']) != loader_evidence.get('Before')
                    or core._loader_value(b[name]['OwnValues']) != loader_evidence.get('After')):
                raise ValueError('The mechanical loader cache needs exact recomputation.')
        elif name not in IMPLEMENTATIONS or not set(delta) <= {'DownValues', 'Messages'}:
            raise ValueError('An unlisted existing definition changed: ' + name)
        elif 'Messages' in delta and (name != CONSTRUCTOR
                                      or not usage_only(core, name, fields['Messages'], b[name]['Messages'])):
            raise ValueError('Only the declared constructor usage message may change.')
        differences[name] = delta
    if any('DownValues' not in differences.get(name, []) for name in IMPLEMENTATIONS):
        raise ValueError('The exact four expected implementation changes are required.')
    new_defined, new_empty, new_temporary = [], [], []
    for name in sorted(set(b) - set(a)):
        fields = b[name]
        if ((name in EXTRA_NEW_FUNCTIONS or any(name.startswith(prefix) for prefix in NEW_FUNCTION_PREFIXES))
                and fields['DownValues'] != '{}' and all(value == '{}' for field, value in fields.items()
                                                         if field not in ('DownValues', 'Options'))):
            new_defined.append(name)
        elif name.startswith(PREFIX) and all(value == '{}' for value in fields.values()):
            new_empty.append(name)
        elif (name.startswith(PREFIX) and name.endswith('$') and fields['Attributes'] == '{Temporary}'
              and all(value == '{}' for field, value in fields.items() if field != 'Attributes')):
            new_temporary.append(name)
        else:
            raise ValueError('A new public, unrelated or stateful symbol is not reviewed: ' + name)
    if not set(EXTRA_NEW_FUNCTIONS) <= set(new_defined):
        raise ValueError('The exact two declared fitted-point helpers are required.')
    return {'Status': 'ANALYZED_NOT_SEALED', 'Kind': KIND,
            'BeforeContextSHA256': core.stage_module().fingerprint(before),
            'AfterContextSHA256': core.stage_module().fingerprint(after),
            'ChangedExistingFiles': changed, 'ChangedDefinitionFields': differences,
            'ExistingSymbolsChecked': len(a), 'NewSymbolsChecked': len(b)-len(a),
            'NewDefinedPrivateSymbols': new_defined, 'NewEmptyPrivateSymbols': new_empty,
            'NewTemporaryPrivateSymbols': new_temporary,
            'BeforeColdDefinitionsSHA256': canonical_digest(a), 'AfterColdDefinitionsSHA256': canonical_digest(b),
            'AllOtherTenFieldDefinitionsExactlyUnchanged': True,
            'MechanicalLoaderSignatureEvidence': {key: value for key, value in (loader_evidence or {}).items()
                                                  if not key.startswith('_')},
            'HistoricalEvidenceOnly': True, 'ConstructorRerunPerformed': False,
            'ValidationRerunPerformed': False, 'CurrentConstructorSuccessClaimed': False}


def reviewed_payload(analysis, review, evidence):
    if (analysis.get('Status') != 'ANALYZED_NOT_SEALED'
            or review.get('Status') != 'REVIEWED_EXACT_CHECKPOINT_GRADED_COLOUR_AND_CONTACT_CHANGE'
            or review.get('AnalysisSHA256') != canonical_digest(analysis)
            or review.get('SourceDiffsSHA256') != analysis.get('SourceDiffsSHA256')
            or review.get('ExpectedChangedDefinitionFields') != analysis.get('ChangedDefinitionFields')
            or review.get('ExpectedNewDefinedPrivateSymbols') != analysis.get('NewDefinedPrivateSymbols')
            or not isinstance(review.get('ReviewedBy'), str) or not review['ReviewedBy'].strip()
            or not all(review.get(flag) is True for flag in REVIEW_FLAGS)
            or not isinstance(evidence, dict) or not evidence):
        raise ValueError('Exact source, definition and semantic review is required.')
    return {**analysis, 'Status': 'SEALED', 'AnalysisSHA256': canonical_digest(analysis),
            'CodeReviewSHA256': canonical_digest(review), 'ReviewEvidenceSHA256': canonical_digest(evidence),
            'SupplementalCounterexampleChecksStillRequired': True, 'PrecisionQualificationUnchangedRequired': True}


def record_evidence(summary_path, summary, current_context, proof):
    try:
        if (not isinstance(proof, dict) or proof.get('_C5VerifiedCurrentContext') is not True
                or proof.get('Status') != 'SEALED' or proof.get('Kind') != KIND
                or proof.get('BeforeContextSHA256') != BASE_CONTEXT
                or proof.get('AfterContextSHA256') != current_context
                or proof.get('HistoricalEvidenceOnly') is not True
                or proof.get('ConstructorRerunPerformed') is not False
                or proof.get('ValidationRerunPerformed') is not False
                or proof.get('CurrentConstructorSuccessClaimed') is not False):
            return None
        baseline = proof['_Baseline'];path = Path(summary_path)
        row = baseline['Snapshot'][path.name]
        if (digest(path) != row['SummarySHA256'] or summary.get('Status') != row['Status']
                or summary.get('ContextSHA256') != row['ContextSHA256']
                or (summary.get('ID') or summary.get('FamilyID')) != row['Identity']):
            return None
        capsule = path.with_suffix('.wxf')
        if row['Capsule']:
            if (not capsule.is_file() or digest(capsule) != row['ActualCapsuleSHA256']
                    or summary.get('CapsuleSHA256') != row['DeclaredCapsuleSHA256']
                    or row['ActualCapsuleSHA256'] != row['DeclaredCapsuleSHA256']):
                return None
        elif capsule.exists() or summary.get('Status') in ('COMPLETE', 'PASS'):
            return None
        chain = []
        recorded = summary['ContextSHA256']
        if recorded != BASE_CONTEXT:
            prior = baseline['PriorVerifier'].record_evidence(path, summary, BASE_CONTEXT, baseline['PriorProof'])
            if prior is None or prior.get('CurrentContextSHA256') != BASE_CONTEXT:
                return None
            chain.extend(prior['CompatibilityChain'])
        chain.append({'Kind': KIND, 'FromContextSHA256': BASE_CONTEXT, 'ToContextSHA256': current_context,
                      'AnalysisSHA256': proof['AnalysisSHA256'], 'CodeReviewSHA256': proof['CodeReviewSHA256'],
                      'ReviewEvidenceSHA256': proof['ReviewEvidenceSHA256']})
        return {'Status': 'HISTORICAL_VALIDATION_COMPATIBLE_REVIEWED_C5_CHANGE',
                'RecordedContextSHA256': recorded, 'CurrentContextSHA256': current_context,
                'Stage': 'HistoricalExportReport' if '-export.' in path.name else 'HistoricalNativeValidation',
                'CompatibilityChain': chain, 'OriginalTimingAndOptionsPreserved': True,
                'ConstructorRerunPerformed': False, 'ValidationRerunPerformed': False,
                'CurrentConstructorSuccessClaimed': False,
                'SupplementalCounterexampleChecksStillRequired': True, 'PrecisionQualificationUnchangedRequired': True}
    except (OSError, ValueError, KeyError, TypeError, AttributeError):
        return None


def prepare_loader_evidence(root, after_manifest_path, directory):
    root, directory = Path(root).resolve(), Path(directory).resolve()
    directory.relative_to(root)
    baseline = load_baseline(root)
    after = object_at(after_manifest_path)
    core, old = baseline['Core'], baseline['PriorVerifier']
    rows = []
    for name in core.SIGNATURE_REQUIRED + core.SIGNATURE_OPTIONAL:
        key = 'source/' + name
        before_path, after_path = before_source(baseline, key), root / key
        if digest(after_path) != after['Files'][key]:
            raise ValueError('An after loader input changed.')
        rows.append({'Name': name, 'BeforePath': str(before_path), 'AfterPath': str(after_path),
                     'BeforeSHA256': digest(before_path), 'AfterSHA256': digest(after_path)})
    count = len(core.SIGNATURE_REQUIRED)
    paths = {'BeforeRequired': [r['BeforePath'] for r in rows[:count]],
             'BeforeOptional': [r['BeforePath'] for r in rows[count:]],
             'AfterRequired': [r['AfterPath'] for r in rows[:count]],
             'AfterOptional': [r['AfterPath'] for r in rows[count:]]}
    (directory / 'loader-signature-inputs.json').write_text(json.dumps(
        {'Method': 'ExactUnchangedLoaderFileHashFormula', 'Inputs': rows, **paths}, indent=2, sort_keys=True)+'\n')
    script = directory / 'recompute-loader-signature.wl'
    script.write_text(old.loader_script(paths, directory / 'loader-signature-values.json'))
    return script


def loader_evidence(baseline, after, directory):
    directory = Path(directory).resolve()
    directory.relative_to(baseline['Root'])
    meta = object_at(directory / 'loader-signature-inputs.json')
    values = object_at(directory / 'loader-signature-values.json')
    script = directory / 'recompute-loader-signature.wl'
    core, old = baseline['Core'], baseline['PriorVerifier']
    names = core.SIGNATURE_REQUIRED + core.SIGNATURE_OPTIONAL
    rows = meta.get('Inputs')
    if (meta.get('Method') != 'ExactUnchangedLoaderFileHashFormula' or not isinstance(rows, list)
            or [row.get('Name') for row in rows] != list(names)):
        raise ValueError('Exactly the unchanged loader hash inputs are required.')
    for name, row in zip(names, rows):
        key = 'source/' + name
        a, b = before_source(baseline, key), baseline['Root'] / key
        if (row.get('BeforePath') != str(a) or row.get('AfterPath') != str(b)
                or row.get('BeforeSHA256') != baseline['Manifest']['Files'][key]
                or row.get('AfterSHA256') != after['Files'][key]
                or digest(a) != row['BeforeSHA256'] or digest(b) != row['AfterSHA256']):
            raise ValueError('The mechanical loader inputs are not exact before/after bytes.')
    count = len(core.SIGNATURE_REQUIRED)
    paths = {'BeforeRequired': [r['BeforePath'] for r in rows[:count]],
             'BeforeOptional': [r['BeforePath'] for r in rows[count:]],
             'AfterRequired': [r['AfterPath'] for r in rows[:count]],
             'AfterOptional': [r['AfterPath'] for r in rows[count:]]}
    if (any(meta.get(k) != value for k, value in paths.items())
            or script.read_text() != old.loader_script(paths, directory / 'loader-signature-values.json')
            or values.get('PhysicsCalculationsPerformed') is not False
            or values.get('Method') != 'ExactUnchangedLoaderFileHashFormulaVerifiedArchiveOrIdenticalSHAInputs'
            or any(type(values.get(k)) is not int for k in ('Before', 'After'))):
        raise ValueError('The literal unchanged loader hash calculation is unavailable.')
    return {'Before': values['Before'], 'After': values['After'], 'InputCount': len(rows),
            'InputsSHA256': digest(directory / 'loader-signature-inputs.json'),
            'ValuesSHA256': digest(directory / 'loader-signature-values.json'),
            'ScriptSHA256': digest(script), '_VerifiedInputHashes': True}


def code_diffs(baseline, after):
    import difflib
    result = {}
    for name in CHANGED_FILES:
        a, b = before_source(baseline, name), baseline['Root'] / name
        if digest(b) != after['Files'][name]:
            raise ValueError('Code review must use the pinned literal after bytes.')
        result[name] = ''.join(difflib.unified_diff(a.read_text().splitlines(True), b.read_text().splitlines(True),
                                                  fromfile='C4/'+name, tofile='C5/'+name))
    return result


def analyze(root, after_manifest_path, captures, evidence_directory):
    baseline = load_baseline(root)
    after = object_at(after_manifest_path)
    paths = [Path(path).resolve() for path in captures]
    if len(paths) != 2:
        raise ValueError('Two independent cold captures are required.')
    for path in paths:
        path.relative_to(Path(root).resolve())
    result = compare_transition(baseline['Core'], baseline['Manifest'], after, baseline['Raw'],
                                [object_at(path) for path in paths], loader_evidence(baseline, after, evidence_directory))
    result.update(BaselineDirectory=BASE_DIRECTORY, BaselineSHA256=BASELINE_SHA256,
                  PriorProofSHA256=PRIOR_PROOF_SHA256, RecursiveProofArchivesCopied=False,
                  AfterManifestSHA256=digest(after_manifest_path),
                  AfterCaptureSHA256=[digest(path) for path in paths],
                  SourceDiffsSHA256=canonical_digest(code_diffs(baseline, after)))
    return result


def review_evidence(root, review):
    files = review.get('EvidenceFiles')
    if not isinstance(files, dict) or not files:
        raise ValueError('Independently reviewed semantic controls must be bound to real files.')
    result = {}
    for label, item in files.items():
        if (not isinstance(label, str) or not label.strip() or not isinstance(item, dict)
                or not isinstance(item.get('SHA256'), str)):
            raise ValueError('A semantic evidence binding is malformed.')
        path = inside(root, item['Path'])
        if digest(path) != item['SHA256']:
            raise ValueError('A reviewed semantic artifact changed: ' + label)
        result[label] = {'Path': item['Path'], 'SHA256': item['SHA256']}
    return result


def seal_payload(root, here, configuration, manifest_path, captures, review_path, evidence_directory):
    root, here = Path(root).resolve(), Path(here).resolve()
    paths = [Path(manifest_path).resolve(), *(Path(path).resolve() for path in captures), Path(review_path).resolve()]
    relative = [str(path.relative_to(root)) for path in paths]
    if len(paths) != 4:
        raise ValueError('One manifest, two captures and one review are required.')
    analysis = analyze(root, paths[0], paths[1:3], evidence_directory)
    baseline = load_baseline(root)
    actual = baseline['Core'].stage_module().manifest(root, here, configuration)
    if actual != object_at(paths[0]) or baseline['Core'].stage_module().fingerprint(actual) != analysis['AfterContextSHA256']:
        raise ValueError('The reviewed C5 manifest is not current.')
    review = object_at(paths[3])
    evidence = review_evidence(root, review)
    payload = reviewed_payload(analysis, review, evidence)
    payload.update(AfterManifest=relative[0], AfterCaptures=relative[1:3], CodeReview=relative[3],
                   CodeReviewFileSHA256=digest(paths[3]), ReviewEvidenceFiles=evidence,
                   LoaderSignatureEvidenceDirectory=str(Path(evidence_directory).resolve().relative_to(root)),
                   VerifierToolsSHA256={str(path.relative_to(root)): digest(path) for path in
                     (Path(__file__).resolve(), Path(__file__).with_name('constructor_context.py').resolve(),
                      Path(__file__).with_name('stage_context.py').resolve())})
    return payload


def verified_proof(root, here, configuration, current_context, proof_path):
    try:
        proof = object_at(proof_path)
        if (proof.get('Kind') != KIND or proof.get('Status') != 'SEALED'
                or proof.get('BeforeContextSHA256') != BASE_CONTEXT
                or proof.get('AfterContextSHA256') != current_context):
            return None
        captures = proof.get('AfterCaptures')
        if not isinstance(captures, list) or len(captures) != 2:
            return None
        paths = [inside(root, proof['AfterManifest']), *(inside(root, name) for name in captures),
                 inside(root, proof['CodeReview'])]
        if digest(paths[3]) != proof['CodeReviewFileSHA256']:
            return None
        expected = seal_payload(root, here, configuration, paths[0], paths[1:3], paths[3],
                                inside(root, proof['LoaderSignatureEvidenceDirectory']))
        if expected != proof:
            return None
        return {**proof, '_C5VerifiedCurrentContext': True, '_VerifiedCurrentContext': True,
                '_ProofSHA256': digest(proof_path), '_Baseline': load_baseline(root)}
    except (OSError, ValueError, KeyError, TypeError, AttributeError):
        return None


def public_proof(proof, stage=None):
    if proof.get('_C5VerifiedCurrentContext') is not True:
        return None
    return {'Status': 'EXACT_CHECKPOINT_GRADED_COLOUR_AND_CONTACT_HISTORICAL_VALIDATION_COMPATIBILITY',
            'Stage': stage, 'BeforeContextSHA256': BASE_CONTEXT, 'AfterContextSHA256': proof['AfterContextSHA256'],
            'AnalysisSHA256': proof['AnalysisSHA256'], 'CodeReviewFileSHA256': proof['CodeReviewFileSHA256'],
            'ProofSHA256': proof['_ProofSHA256'], 'PriorProofSHA256': PRIOR_PROOF_SHA256,
            'ReviewEvidenceSHA256': proof['ReviewEvidenceSHA256'], 'HistoricalEvidenceOnly': True,
            'RecursiveProofArchivesCopied': False, 'OriginalTimingAndOptionsPreserved': True,
            'ConstructorRerunPerformed': False, 'ValidationRerunPerformed': False,
            'CurrentConstructorSuccessClaimed': False, 'SupplementalCounterexampleChecksStillRequired': True,
            'PrecisionQualificationUnchangedRequired': True}
