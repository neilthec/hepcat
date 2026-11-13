"""Narrow, reference-bound historical validation continuity for C7 to C8.

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

BASE_CONTEXT = '4165f24c7d5501c40c03ea91f850a5cde77389a1236e3af397fb6d7e2344a167'
BASE_DIRECTORY = ('tests/results/SM-5-point/compatibility/constructor-C7-pre-C8-'
                  + BASE_CONTEXT + '-20261113T060245Z')
BASELINE_SHA256 = 'c997583548288d6e27d099a8694662b4021a44d88d867f714997266804bb69dc'
PRIOR_PROOF_SHA256 = '5056ae60a8a14d898a1d0203e3f35e20194d3f57a08b3e91f0ebbb218fb58291'
REGISTRY = 'constructor-C7-C8-context-bridge.json'
KIND = 'ExactMassiveFourFermionVectorSourceClassHistoricalValidationTransition'
CHANGED_FILES = ('source/HEPCAT-Model.wl', 'source/internal/HEPCAT-CovariantSources.wl')
PREFIX = 'ConstructiveDiagrams`Private`'
CONSTRUCTOR = 'ConstructiveDiagrams`ConstructAmplitude'
COLOUR_CONSTRUCTOR = PREFIX + 'colorConstructAmplitude'
CURRENT_PREFIX = 'HEPCATCovariantCurrentSources`Private`'
CURRENT_SOURCE = CURRENT_PREFIX + 'covariantCurrentSource'
EXTERNAL_WEIGHTS = CURRENT_PREFIX + 'declaredExternalSpinWeights'
IMPLEMENTATIONS = (CONSTRUCTOR, CURRENT_SOURCE)
EXACT_MATCH = CURRENT_PREFIX + 'exactVertexMatch'
EMPTY_PRIVATE_PREFIXES = (PREFIX, CURRENT_PREFIX)
FIELDS = ('DownValues', 'UpValues', 'SubValues', 'NValues', 'OwnValues',
          'DefaultValues', 'FormatValues', 'Options', 'Attributes', 'Messages')
REVIEW_FLAGS = (
    'ExistingCheckersReducersModelsAndNumericalRuntimeUnchanged',
    'FourMassiveColourSingletDiracFermionsAndOneMassiveVectorOnly',
    'MasslessFourFermionVectorClassStillExcluded',
    'ExistingSupportedSourceAmplitudeOutputsLiterallyUnchanged',
    'ExactExistingCoherentVectorMapAndRawTensorRetained',
    'UnknownSlotsWrongLegsAndWrongSpinDegreesRejected',
    'EveryUsedLocalVertexCertified',
    'EveryOriginalOrderedGraphPoleAndFermionFlowRetained',
    'ExistingLocalThreePointMatchersUnchanged',
    'ExistingMetricContactCalibrationUnchanged',
    'AllProposalsReceiveEveryIndependentCheck',
    'NoAnswerOrIndependentBenchmarkForSourceConstruction',
    'NoNewDefinedHelpersOrLoadTimeSideEffects',
    'PublicOptionsDefaultsAndMessagesUnchanged',
    'ActualPublicDefaultNativeControlsReviewed',
    'SeparatePrototypeBackendReportsNotAttributedToDifferentPublicExpressions',
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
    path = inside(root, BASE_DIRECTORY) / 'frozen-verifiers' / name
    spec = importlib.util.spec_from_file_location('hepcat_frozen_c7_' + path.stem, path)
    value = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(value)
    return value


def before_source(baseline, name):
    recorded = baseline['Record']['SourceFiles'].get(name)
    paths = ([inside(baseline['Root'], recorded['Path'])] if recorded else []) + [baseline['Root'] / name]
    for path in paths:
        if path.is_file() and digest(path) == baseline['Manifest']['Files'].get(name):
            return path
    raise ValueError('Exact before-source bytes are unavailable: ' + name)


def load_baseline(root):
    root = Path(root).resolve(); directory = inside(root, BASE_DIRECTORY)
    if digest(directory / 'baseline.json') != BASELINE_SHA256:
        raise ValueError('The sealed C7 baseline changed.')
    record = object_at(directory / 'baseline.json')
    if (record.get('Status') != 'SEALED_C7_BASELINE_BEFORE_C8'
            or record.get('ContextSHA256') != BASE_CONTEXT
            or record.get('ExistingProofArchivesReferencedNotRecursivelyCopied') is not True
            or record.get('CapsulesChecksumBoundNotCopied') is not True):
        raise ValueError('The exact reference-bound C7 baseline is required.')
    for key in ('BeforeManifest', 'CheckpointBindings', 'FrozenReferenceFiles'):
        path = inside(root, record[key + 'Path'])
        if digest(path) != record[key + 'SHA256']:
            raise ValueError('A pinned baseline component changed: ' + key)
    for name, data in record['SourceFiles'].items():
        if digest(inside(root, data['Path'])) != data['SHA256'] or data['OriginalSHA256'] != data['SHA256']:
            raise ValueError('Frozen source bytes changed: ' + name)
    for name, data in record['FrozenVerifierFiles'].items():
        if digest(inside(root, data['Path'])) != data['SHA256'] or data['OriginalSHA256'] != data['SHA256']:
            raise ValueError('A frozen C7 verifier changed: ' + name)
    references = object_at(inside(root, record['FrozenReferenceFilesPath']))
    for name, data in references.items():
        if digest(inside(root, name)) != data['SHA256']:
            raise ValueError('An immutable prior proof reference changed: ' + name)
    core = frozen_module(root, 'constructor_context.py')
    paths = [inside(root, name) for name in record['ColdDefinitionReferences']]
    if len(paths) != 2 or any(digest(path) != record['ColdDefinitionReferences'][str(path.relative_to(root))] for path in paths):
        raise ValueError('Exactly two pinned C7 cold definition captures are required.')
    raw = [object_at(path) for path in paths]; definitions = core.cold_pair(*raw, require_post=True)
    if len(definitions) != 8342 or record['ColdDefinitionCount'] != len(definitions):
        raise ValueError('The exact complete C7 definition inventory is required.')
    entries = object_at(inside(root, record['CheckpointBindingsPath']))['Bindings']; snapshot = {}
    for name, row in entries.items():
        original = row['RawSummary']; identity = original.get('ID', original.get('FamilyID'))
        if (Path(name).name in snapshot or not isinstance(original, dict)
                or not isinstance(identity, str) or row['Status'] != original.get('Status')
                or row['ContextSHA256'] != original.get('ContextSHA256')
                or row['CapsuleSHA256'] != original.get('CapsuleSHA256')):
            raise ValueError('Duplicate or malformed frozen checkpoint.')
        snapshot[Path(name).name] = {'Summary':name,'SummarySHA256':row['SummarySHA256'],
            'Status':row['Status'],'ContextSHA256':row['ContextSHA256'],'Identity':identity,
            'Capsule':row['CapsulePath'],'ActualCapsuleSHA256':row['CapsuleSHA256'],
            'DeclaredCapsuleSHA256':original['CapsuleSHA256']}
    if len(snapshot) != 414 or record['NativeBindings'] != 245 or record['ExportBindings'] != 169:
        raise ValueError('The finite C7 checkpoint set changed.')
    old = frozen_module(root, 'validation_context_c7.py')
    prior_path = root / 'tests/SM-5-point/constructor-C6-C7-context-bridge.json'
    prior = object_at(prior_path)
    if digest(prior_path) != PRIOR_PROOF_SHA256 or prior.get('Status') != 'SEALED' or prior.get('AfterContextSHA256') != BASE_CONTEXT:
        raise ValueError('The exact already reviewed C7 proof is required.')
    old_proof = {**prior, '_C7VerifiedCurrentContext':True, '_VerifiedCurrentContext':True,
                 '_ProofSHA256':PRIOR_PROOF_SHA256, '_Baseline':old.load_baseline(root)}
    return {'Root':root,'Directory':directory,'Record':record,
            'Manifest':object_at(inside(root, record['BeforeManifestPath'])),
            'Raw':raw,'Snapshot':snapshot,'PriorVerifier':old,'PriorProof':old_proof,'Core':core}


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
        raise ValueError('Only the two explicitly reviewed source files may change.')
    a, b = core.cold_pair(*before_raw, require_post=True), core.cold_pair(*after_raw, require_post=True)
    if any(before_raw[0].get(k) != after_raw[0].get(k) for k in ('WolframVersion', 'SystemID')):
        raise ValueError('Wolfram version or platform changed.')
    if set(a) - set(b):
        raise ValueError('An existing C7 symbol disappeared.')
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
        elif name not in IMPLEMENTATIONS or delta != ['DownValues']:
            raise ValueError('An unlisted existing definition changed: ' + name)
        differences[name] = delta
    if any('DownValues' not in differences.get(name, []) for name in IMPLEMENTATIONS):
        raise ValueError('The exact two expected implementation changes are required.')
    new_defined, new_empty, new_temporary = [], [], []
    for name in sorted(set(b) - set(a)):
        fields = b[name]
        if any(name.startswith(p) for p in EMPTY_PRIVATE_PREFIXES) and all(value == '{}' for value in fields.values()):
            new_empty.append(name)
        elif (any(name.startswith(p) for p in EMPTY_PRIVATE_PREFIXES) and name.endswith('$') and fields['Attributes'] == '{Temporary}'
              and all(value == '{}' for field, value in fields.items() if field != 'Attributes')):
            new_temporary.append(name)
        else:
            raise ValueError('A new public, unrelated or stateful symbol is not reviewed: ' + name)
    return {'Status': 'ANALYZED_NOT_SEALED', 'Kind': KIND,
            'BeforeContextSHA256': core.stage_module().fingerprint(before),
            'AfterContextSHA256': core.stage_module().fingerprint(after),
            'ChangedExistingFiles': changed, 'ChangedDefinitionFields': differences,
            'ExistingSymbolsChecked': len(a), 'NewSymbolsChecked': len(b)-len(a),
            'NewDefinedPrivateSymbols': new_defined,
            'NoNewDefinedHelpers': True,
            'NewEmptyPrivateSymbols': new_empty,
            'NewTemporaryPrivateSymbols': new_temporary,
            'BeforeColdDefinitionsSHA256': canonical_digest(a), 'AfterColdDefinitionsSHA256': canonical_digest(b),
            'AllOtherTenFieldDefinitionsExactlyUnchanged': True,
            'MechanicalLoaderSignatureEvidence': {key: value for key, value in (loader_evidence or {}).items()
                                                  if not key.startswith('_')},
            'HistoricalEvidenceOnly': True, 'ConstructorRerunPerformed': False,
            'ValidationRerunPerformed': False, 'CurrentConstructorSuccessClaimed': False}


def reviewed_payload(analysis, review, evidence):
    if (analysis.get('Status') != 'ANALYZED_NOT_SEALED'
            or review.get('Status') != 'REVIEWED_EXACT_MASSIVE_FOUR_FERMION_VECTOR_SOURCE_CLASS_EXTENSION'
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
        if (not isinstance(proof, dict) or proof.get('_C8VerifiedCurrentContext') is not True
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
        return {'Status': 'HISTORICAL_VALIDATION_COMPATIBLE_REVIEWED_C8_CHANGE',
                'RecordedContextSHA256': recorded, 'CurrentContextSHA256': current_context,
                'Stage': 'HistoricalExportReport' if '-export.' in path.name else 'HistoricalNativeValidation',
                'CompatibilityChain': chain, 'OriginalTimingAndOptionsPreserved': True,
                'ConstructorRerunPerformed': False, 'ValidationRerunPerformed': False,
                'CurrentConstructorSuccessClaimed': False,
                'SupplementalCounterexampleChecksStillRequired': True, 'PrecisionQualificationUnchangedRequired': True}
    except (OSError, ValueError, KeyError, TypeError, AttributeError):
        return None


def loader_formula_owner(baseline):
    current = baseline
    for _ in range(12):
        old = current['PriorVerifier']
        if callable(getattr(old, 'loader_script', None)):
            return old
        current = current['PriorProof']['_Baseline']
    raise ValueError('The frozen unchanged loader formula is unavailable.')


def prepare_loader_evidence(root, after_manifest_path, directory):
    root, directory = Path(root).resolve(), Path(directory).resolve()
    directory.relative_to(root)
    baseline = load_baseline(root)
    after = object_at(after_manifest_path)
    core = baseline['Core']
    # The unchanged signature formula is owned by the frozen C4 verifier.
    old = loader_formula_owner(baseline)
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
    core = baseline['Core']
    # The unchanged signature formula is owned by the frozen C4 verifier.
    old = loader_formula_owner(baseline)
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
                                                  fromfile='C7/'+name, tofile='C8/'+name))
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
        raise ValueError('The reviewed C8 manifest is not current.')
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
        return {**proof, '_C8VerifiedCurrentContext': True, '_VerifiedCurrentContext': True,
                '_ProofSHA256': digest(proof_path), '_Baseline': load_baseline(root)}
    except (OSError, ValueError, KeyError, TypeError, AttributeError):
        return None


def public_proof(proof, stage=None):
    if proof.get('_C8VerifiedCurrentContext') is not True:
        return None
    return {'Status': 'EXACT_MASSIVE_FOUR_FERMION_VECTOR_SOURCE_HISTORICAL_VALIDATION_COMPATIBILITY',
            'Stage': stage, 'BeforeContextSHA256': BASE_CONTEXT, 'AfterContextSHA256': proof['AfterContextSHA256'],
            'AnalysisSHA256': proof['AnalysisSHA256'], 'CodeReviewFileSHA256': proof['CodeReviewFileSHA256'],
            'ProofSHA256': proof['_ProofSHA256'], 'PriorProofSHA256': PRIOR_PROOF_SHA256,
            'ReviewEvidenceSHA256': proof['ReviewEvidenceSHA256'], 'HistoricalEvidenceOnly': True,
            'RecursiveProofArchivesCopied': False, 'OriginalTimingAndOptionsPreserved': True,
            'ConstructorRerunPerformed': False, 'ValidationRerunPerformed': False,
            'CurrentConstructorSuccessClaimed': False, 'SupplementalCounterexampleChecksStillRequired': True,
            'PrecisionQualificationUnchangedRequired': True}
