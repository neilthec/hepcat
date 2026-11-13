"""Narrow, reference-bound historical validation continuity for C8 to C9.

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

BASE_CONTEXT = '48d82ad2adaf29f9a338c9fb6b9cf3da37ed7570f6de5852ee50c798fd0d8363'
BASE_DIRECTORY = ('tests/results/SM-5-point/compatibility/constructor-C8-pre-C9-'
                  + BASE_CONTEXT + '-20261113T074426Z')
BASELINE_SHA256 = 'da467b5602a19e8de2e8c4ee8150d4b586ef0bbc06972507a2e6edba73130b16'
PRIOR_PROOF_SHA256 = 'e76b8976ca0c2a448aebc73819c12962b29e9b4442d9a05b573bf43fa8ef5fbe'
REGISTRY = 'constructor-C8-C9-context-bridge.json'
KIND = 'ExactMassiveColouredFourFermionScalarSourceHistoricalValidationTransition'
CHANGED_FILES = ('source/HEPCAT-Model.wl', 'source/internal/HEPCAT-Color.wl', 'source/HEPCAT.wl')
ADDED_FILE = 'source/internal/HEPCAT-ColoredCurrents.wl'
PREFIX = 'ConstructiveDiagrams`Private`'
CONSTRUCTOR = 'ConstructiveDiagrams`ConstructAmplitude'
COLOUR_CONSTRUCTOR = PREFIX + 'colorConstructAmplitude'
CURRENT_PREFIX = 'HEPCATCovariantCurrentSources`Private`'
CURRENT_SOURCE = CURRENT_PREFIX + 'covariantCurrentSource'
EXTERNAL_WEIGHTS = CURRENT_PREFIX + 'declaredExternalSpinWeights'
SIGNATURE_FUNCTION = PREFIX + 'amplitudeParallelSignature'
IMPLEMENTATIONS = (COLOUR_CONSTRUCTOR, SIGNATURE_FUNCTION)
NEW_FUNCTIONS = tuple(PREFIX + name for name in ('colorCurrentSourceClass', 'colorCurrentSource',
    'colorCurrentSourceCertificate', 'colorCurrentPrepare', 'colorCurrentProjection'))
BEFORE_REQUIRED = ('HEPCAT.wl', 'HEPCAT-Base.wl', 'internal/HEPCAT-Parameters.wl', 'HEPCAT-Model.wl',
    'internal/HEPCAT-Feynman.wl', 'internal/HEPCAT-Color.wl', 'internal/HEPCAT-SPINAS.wl',
    'internal/HEPCAT-SPINAS-Optimize.wl', 'internal/export/spinas-runtime.hpp')
ADDED_REQUIRED = ('internal/HEPCAT-MadGraph.wl', 'internal/HEPCAT-CovariantSources.wl',
    'internal/HEPCAT-CovariantContacts.wl', 'internal/HEPCAT-ColoredCurrents.wl')
AFTER_REQUIRED = BEFORE_REQUIRED[:7] + ADDED_REQUIRED + BEFORE_REQUIRED[7:]
OPTIONAL = ('internal/proofs/laurent-proof-runtime.cpp', 'internal/proofs/laurent-proof-runtime.py',
    'internal/proofs/laurent-proof-factor.py', 'internal/proofs/laurent-proof-verify.py')
EXACT_MATCH = CURRENT_PREFIX + 'exactVertexMatch'
EMPTY_PRIVATE_PREFIXES = (PREFIX, CURRENT_PREFIX)
FIELDS = ('DownValues', 'UpValues', 'SubValues', 'NValues', 'OwnValues',
          'DefaultValues', 'FormatValues', 'Options', 'Attributes', 'Messages')
REVIEW_FLAGS = (
    'ExistingCheckersReducersModelsAndNumericalRuntimeUnchanged',
    'MassiveColouredFourDiracOneScalarClassOnly',
    'All251KnownCasesRemainOutsideNewClass',
    'Exactly45PendingMassiveColoredHiggsCasesEligible',
    'Prior36ColoredChiralCasesPreserved',
    'EveryUsedLorentzVertexSymbolicallyCertified',
    'EachLocalColourOperatorCertifiedAndKinematicsIndependent',
    'EveryOriginalOrderedGraphPoleAndFermionSignCovered',
    'NoAssumedColourMultiplicityOrIndependentSquareSum',
    'TypedColourSpinMomentumAndMassLabelsRetained',
    'WholeCandidateReceivesEveryOriginalIndependentCheck',
    'NoAnswerOrBenchmarkForSourceConstruction',
    'NoNumericCoefficientFitOrNewContactsOrInternalShells',
    'RejectedProposalsRetainOrdinaryCompletionFallback',
    'AllFourActualPublicDefaultControlsReviewed',
    'AdditionalEqualMassControlReviewed',
    'ExistingPublicOptionsDefaultsAndMessagesUnchanged',
    'ExactlyFivePrivateHelpersAndNoLoadTimeSideEffects',
    'LoaderOnlyAddsReviewedModule',
    'SignatureRequiredListExactly13OptionalExactly4',
    'SignatureFormulaAndOptionalPolicyUnchanged',
    'All17SourceSignatureMutationControlsReviewed',
    'MechanicalOldAndNewLoaderSignaturesIndependentlyRecomputed',
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
    spec = importlib.util.spec_from_file_location('hepcat_frozen_c8_' + path.stem, path)
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
        raise ValueError('The sealed C8 baseline changed.')
    record = object_at(directory / 'baseline.json')
    if (record.get('Status') != 'SEALED_C8_BASELINE_BEFORE_C9'
            or record.get('ContextSHA256') != BASE_CONTEXT
            or record.get('ExistingProofArchivesReferencedNotRecursivelyCopied') is not True
            or record.get('CapsulesChecksumBoundNotCopied') is not True):
        raise ValueError('The exact reference-bound C8 baseline is required.')
    if (record.get('PlannedChangedSourceFiles') != list(CHANGED_FILES)
            or record.get('PlannedAddedSourceFiles') != [ADDED_FILE]
            or record.get('ExistingSignatureRequired') != list(BEFORE_REQUIRED)
            or record.get('ExistingSignatureOptional') != list(OPTIONAL)
            or record.get('PlannedSignatureRequiredAdditions') != list(ADDED_REQUIRED)
            or record.get('ExistingSignatureRequiredCount') != 9
            or record.get('PlannedSignatureRequiredCount') != 13
            or record.get('SignatureOptionalCount') != 4):
        raise ValueError('The sealed exact source and signature transition plan changed.')
    for key in ('BeforeManifest', 'CheckpointBindings', 'FrozenReferenceFiles'):
        path = inside(root, record[key + 'Path'])
        if digest(path) != record[key + 'SHA256']:
            raise ValueError('A pinned baseline component changed: ' + key)
    for name, data in record['SourceFiles'].items():
        if digest(inside(root, data['Path'])) != data['SHA256'] or data['OriginalSHA256'] != data['SHA256']:
            raise ValueError('Frozen source bytes changed: ' + name)
    for name, data in record['FrozenVerifierFiles'].items():
        if digest(inside(root, data['Path'])) != data['SHA256'] or data['OriginalSHA256'] != data['SHA256']:
            raise ValueError('A frozen C8 verifier changed: ' + name)
    references = object_at(inside(root, record['FrozenReferenceFilesPath']))
    for name, data in references.items():
        if digest(inside(root, name)) != data['SHA256']:
            raise ValueError('An immutable prior proof reference changed: ' + name)
    core = frozen_module(root, 'constructor_context.py')
    paths = [inside(root, name) for name in record['ColdDefinitionReferences']]
    if len(paths) != 2 or any(digest(path) != record['ColdDefinitionReferences'][str(path.relative_to(root))] for path in paths):
        raise ValueError('Exactly two pinned C8 cold definition captures are required.')
    raw = [object_at(path) for path in paths]; definitions = core.cold_pair(*raw, require_post=True)
    if len(definitions) != 8344 or record['ColdDefinitionCount'] != len(definitions):
        raise ValueError('The exact complete C8 definition inventory is required.')
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
    if len(snapshot) != 426 or record['NativeBindings'] != 251 or record['ExportBindings'] != 175:
        raise ValueError('The finite C8 checkpoint set changed.')
    old = frozen_module(root, 'validation_context_c8.py')
    prior_path = root / 'tests/SM-5-point/constructor-C7-C8-context-bridge.json'
    prior = object_at(prior_path)
    if digest(prior_path) != PRIOR_PROOF_SHA256 or prior.get('Status') != 'SEALED' or prior.get('AfterContextSHA256') != BASE_CONTEXT:
        raise ValueError('The exact already reviewed C8 proof is required.')
    old_proof = {**prior, '_C8VerifiedCurrentContext':True, '_VerifiedCurrentContext':True,
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


def signature_delta_exact(before, after):
    old = '"internal/HEPCAT-SPINAS.wl", "internal/HEPCAT-SPINAS-Optimize.wl"'
    new = '"internal/HEPCAT-SPINAS.wl", ' + ', '.join(json.dumps(x) for x in ADDED_REQUIRED) + ', "internal/HEPCAT-SPINAS-Optimize.wl"'
    return before.count(old) == after.count(new) == 1 and after.replace(new, old, 1) == before


def loader_delta_exact(before, after):
    old = '"internal/HEPCAT-CovariantContacts.wl"'
    new = old + ', "internal/HEPCAT-ColoredCurrents.wl"'
    return before.count(old) == after.count(new) == 1 and after.replace(new, old, 1) == before


def compare_transition(core, before, after, before_raw, after_raw, loader_evidence=None):
    """Analyze a literal candidate patch. Analysis alone never grants admission."""
    if (before.get('Programs') != after.get('Programs')
            or set(after['Files']) != set(before['Files']) | {ADDED_FILE}
            or ADDED_FILE in before['Files']):
        raise ValueError('Installation paths or manifest membership changed.')
    changed = sorted(name for name in before['Files'] if before['Files'][name] != after['Files'][name])
    if changed != sorted(CHANGED_FILES):
        raise ValueError('Only the three reviewed existing source files may change, plus the one added module.')
    a, b = core.cold_pair(*before_raw, require_post=True), core.cold_pair(*after_raw, require_post=True)
    if any(before_raw[0].get(k) != after_raw[0].get(k) for k in ('WolframVersion', 'SystemID')):
        raise ValueError('Wolfram version or platform changed.')
    if set(a) - set(b):
        raise ValueError('An existing C8 symbol disappeared.')
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
        if name == SIGNATURE_FUNCTION and not signature_delta_exact(fields['DownValues'], b[name]['DownValues']):
            raise ValueError('The signature may only add the four required module paths.')
        differences[name] = delta
    if any('DownValues' not in differences.get(name, []) for name in IMPLEMENTATIONS):
        raise ValueError('The exact two expected implementation changes are required.')
    new_defined, new_empty, new_temporary = [], [], []
    for name in sorted(set(b) - set(a)):
        fields = b[name]
        if (name in NEW_FUNCTIONS and fields['DownValues'] != '{}'
                and all(value == '{}' for field, value in fields.items() if field != 'DownValues')):
            new_defined.append(name)
        elif any(name.startswith(p) for p in EMPTY_PRIVATE_PREFIXES) and all(value == '{}' for value in fields.values()):
            new_empty.append(name)
        elif (any(name.startswith(p) for p in EMPTY_PRIVATE_PREFIXES) and name.endswith('$') and fields['Attributes'] == '{Temporary}'
              and all(value == '{}' for field, value in fields.items() if field != 'Attributes')):
            new_temporary.append(name)
        else:
            raise ValueError('A new public, unrelated or stateful symbol is not reviewed: ' + name)
    if set(new_defined) != set(NEW_FUNCTIONS):
        raise ValueError('Exactly the five reviewed private current helpers are required.')
    return {'Status': 'ANALYZED_NOT_SEALED', 'Kind': KIND,
            'BeforeContextSHA256': core.stage_module().fingerprint(before),
            'AfterContextSHA256': core.stage_module().fingerprint(after),
            'ChangedExistingFiles': changed, 'ChangedDefinitionFields': differences,
            'ExistingSymbolsChecked': len(a), 'NewSymbolsChecked': len(b)-len(a),
            'NewDefinedPrivateSymbols': new_defined,
            'AddedSourceFiles': [ADDED_FILE],
            'SignatureRequiredBefore': list(BEFORE_REQUIRED), 'SignatureRequiredAfter': list(AFTER_REQUIRED),
            'SignatureOptionalUnchanged': list(OPTIONAL),
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
            or review.get('Status') != 'REVIEWED_EXACT_MASSIVE_COLOURED_FOUR_FERMION_SCALAR_SOURCE_EXTENSION'
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
        if (not isinstance(proof, dict) or proof.get('_C9VerifiedCurrentContext') is not True
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
        return {'Status': 'HISTORICAL_VALIDATION_COMPATIBLE_REVIEWED_C9_CHANGE',
                'RecordedContextSHA256': recorded, 'CurrentContextSHA256': current_context,
                'Stage': 'HistoricalExportReport' if '-export.' in path.name else 'HistoricalNativeValidation',
                'CompatibilityChain': chain, 'OriginalTimingAndOptionsPreserved': True,
                'ConstructorRerunPerformed': False, 'ValidationRerunPerformed': False,
                'CurrentConstructorSuccessClaimed': False,
                'SupplementalCounterexampleChecksStillRequired': True, 'PrecisionQualificationUnchangedRequired': True}
    except (OSError, ValueError, KeyError, TypeError, AttributeError):
        return None


def wl_literal(value):
    if isinstance(value, list):
        return '{' + ', '.join(wl_literal(x) for x in value) + '}'
    if isinstance(value, str):
        return json.dumps(value, ensure_ascii=False)
    raise ValueError('Only literal string lists are permitted in the hash script.')


def loader_script(paths, destination):
    """Recompute the literal same Hash formula using explicitly different lists."""
    return ('ClearAll[signature];\n'
        'signature[required_List, optional_List, names_List] := Hash[Join[FileHash /@ required, '
        'MapThread[If[FileExistsQ[#1], FileHash[#1], Missing["OptionalProofSourceAbsent", #2]] &, '
        '{optional, names}]]];\n'
        'Export[' + wl_literal(str(destination)) + ', <|"Before" -> signature['
        + wl_literal(paths['BeforeRequired']) + ', ' + wl_literal(paths['BeforeOptional']) + ', '
        + wl_literal(list(OPTIONAL)) + '], "After" -> signature['
        + wl_literal(paths['AfterRequired']) + ', ' + wl_literal(paths['AfterOptional']) + ', '
        + wl_literal(list(OPTIONAL)) + '], '
        '"Method" -> "ExactLoaderHashWithExplicitBeforeAndAfterRequiredLists", '
        '"PhysicsCalculationsPerformed" -> False|>, "RawJSON"];\n')


def loader_inputs(baseline, after):
    root = baseline['Root']
    if (tuple(baseline['Core'].SIGNATURE_REQUIRED) != BEFORE_REQUIRED
            or tuple(baseline['Core'].SIGNATURE_OPTIONAL) != OPTIONAL):
        raise ValueError('The frozen before signature lists do not match their exact declaration.')
    rows = {}
    for stage, names, manifest in (('Before', BEFORE_REQUIRED, baseline['Manifest']),
                                    ('After', AFTER_REQUIRED, after)):
        records = []
        for kind, group in (('Required', names), ('Optional', OPTIONAL)):
            for name in group:
                key = 'source/' + name
                wanted = manifest['Files'].get(key)
                if wanted is None:
                    if kind == 'Required':
                        raise ValueError('A required signature input is missing: ' + key)
                    path = root / key
                    if path.exists():
                        raise ValueError('An absent optional input now exists: ' + key)
                else:
                    path = before_source(baseline, key) if stage == 'Before' else root / key
                    if not path.is_file() or digest(path) != wanted:
                        raise ValueError('An exact loader input changed: ' + key)
                records.append({'Name': name, 'Kind': kind, 'Path': str(path), 'SHA256': wanted})
        rows[stage + 'Inputs'] = records
    paths = {stage + kind: [r['Path'] for r in rows[stage + 'Inputs'] if r['Kind'] == kind]
             for stage in ('Before', 'After') for kind in ('Required', 'Optional')}
    return {'Method': 'ExactLoaderHashWithExplicitBeforeAndAfterRequiredLists',
            'RequiredCountBefore': len(BEFORE_REQUIRED), 'RequiredCountAfter': len(AFTER_REQUIRED),
            'OptionalCount': len(OPTIONAL), **rows, **paths}


def prepare_loader_evidence(root, after_manifest_path, directory):
    root, directory = Path(root).resolve(), Path(directory).resolve()
    directory.relative_to(root)
    baseline = load_baseline(root)
    meta = loader_inputs(baseline, object_at(after_manifest_path))
    (directory / 'loader-signature-inputs.json').write_text(json.dumps(meta, indent=2, sort_keys=True)+'\n')
    script = directory / 'recompute-loader-signature.wl'
    script.write_text(loader_script(meta, directory / 'loader-signature-values.json'))
    return script


def loader_evidence(baseline, after, directory):
    directory = Path(directory).resolve()
    directory.relative_to(baseline['Root'])
    meta = object_at(directory / 'loader-signature-inputs.json')
    values = object_at(directory / 'loader-signature-values.json')
    script = directory / 'recompute-loader-signature.wl'
    expected = loader_inputs(baseline, after)
    if (meta != expected
            or script.read_text() != loader_script(expected, directory / 'loader-signature-values.json')
            or values.get('PhysicsCalculationsPerformed') is not False
            or values.get('Method') != 'ExactLoaderHashWithExplicitBeforeAndAfterRequiredLists'
            or any(type(values.get(k)) is not int for k in ('Before', 'After'))):
        raise ValueError('The literal independently recomputed before/after signature is unavailable.')
    return {'Before': values['Before'], 'After': values['After'],
            'InputCountBefore': len(BEFORE_REQUIRED) + len(OPTIONAL),
            'InputCountAfter': len(AFTER_REQUIRED) + len(OPTIONAL),
            'RequiredCountBefore': len(BEFORE_REQUIRED), 'RequiredCountAfter': len(AFTER_REQUIRED),
            'OptionalCount': len(OPTIONAL),
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
                                                  fromfile='C8/'+name, tofile='C9/'+name))
    loader_before = before_source(baseline, 'source/HEPCAT.wl').read_text()
    loader_after = (baseline['Root'] / 'source/HEPCAT.wl').read_text()
    if not loader_delta_exact(loader_before, loader_after):
        raise ValueError('The central loader may only append the one reviewed source module.')
    added = baseline['Root'] / ADDED_FILE
    if digest(added) != after['Files'][ADDED_FILE]:
        raise ValueError('The reviewed new module bytes changed.')
    result[ADDED_FILE] = ''.join(difflib.unified_diff([], added.read_text().splitlines(True),
        fromfile='/dev/null', tofile='C9/' + ADDED_FILE))
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
        raise ValueError('The reviewed C9 manifest is not current.')
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
        return {**proof, '_C9VerifiedCurrentContext': True, '_VerifiedCurrentContext': True,
                '_ProofSHA256': digest(proof_path), '_Baseline': load_baseline(root)}
    except (OSError, ValueError, KeyError, TypeError, AttributeError):
        return None


def public_proof(proof, stage=None):
    if proof.get('_C9VerifiedCurrentContext') is not True:
        return None
    return {'Status': 'EXACT_MASSIVE_COLOURED_FOUR_FERMION_SCALAR_HISTORICAL_VALIDATION_COMPATIBILITY',
            'Stage': stage, 'BeforeContextSHA256': BASE_CONTEXT, 'AfterContextSHA256': proof['AfterContextSHA256'],
            'AnalysisSHA256': proof['AnalysisSHA256'], 'CodeReviewFileSHA256': proof['CodeReviewFileSHA256'],
            'ProofSHA256': proof['_ProofSHA256'], 'PriorProofSHA256': PRIOR_PROOF_SHA256,
            'ReviewEvidenceSHA256': proof['ReviewEvidenceSHA256'], 'HistoricalEvidenceOnly': True,
            'RecursiveProofArchivesCopied': False, 'OriginalTimingAndOptionsPreserved': True,
            'ConstructorRerunPerformed': False, 'ValidationRerunPerformed': False,
            'CurrentConstructorSuccessClaimed': False, 'SupplementalCounterexampleChecksStillRequired': True,
            'PrecisionQualificationUnchangedRequired': True}
