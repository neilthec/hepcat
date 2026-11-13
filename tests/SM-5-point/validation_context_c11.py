"""Explicit C11 validator transition and optional historical-retention metadata.

A deleted local cache means no retained-record admission; the package and fresh
construction/validation remain independent of this optional history mechanism.
The HE validator changes explicitly. Analysis does not claim semantic identity,
new validation, or present generated-project identity.
"""
# HEPCAT - High Energy Physics Constructive Amplitude Tool
# Copyright (C) 2026 Neil Christensen
# SPDX-License-Identifier: GPL-3.0-or-later
from pathlib import Path
import hashlib
import json

FILES = ('source/HEPCAT-Model.wl', 'source/internal/HEPCAT-CovariantSources.wl',
         'source/internal/HEPCAT-CovariantContacts.wl')
P = 'ConstructiveDiagrams`Private`'
C = 'HEPCATCovariantContactCalibration`Private`'
S = 'HEPCATCovariantCurrentSources`Private`'
FIELDS = ('DownValues','UpValues','SubValues','NValues','OwnValues','DefaultValues',
          'FormatValues','Options','Attributes','Messages')
CHANGED = {
 'ConstructiveDiagrams`ConstructAmplitude': ('DownValues',),
 **{P + name: ('DownValues',) for name in (
  'completionHEAffineAtomLaurentRows','completionHEEnergyConstraints','completionHELaurentRows',
  'completionHighEnergyWork','highEnergyCheckSample','highEnergyLaurentJetCoefficients',
  'highEnergyLaurentJetExpansion','highEnergyScalarCoefficientSeries')},
 C+'calibratedCovariantSource': ('DownValues','DefaultValues','Options'),
 C+'metricContactCalibration': ('DownValues',),
 **{S+name: ('DownValues',) for name in ('covariantCurrentSource','liftVertex','sourceCatalog')},
}
LOADER = P+'$amplitudeParallelLoadedSignature'
PATH_CACHES = tuple(P+name for name in ('$amplitudeParallelSourceDirectory',
                                      '$spinasExportSourceDirectory','$spinasExportRuntimeDirectory'))
NEW_HEADS = tuple(P+name for name in (
 'constructBenchmarkVectorPreconditioningQ','constructCovariantGraphCoverageQ','constructFourVectorSourceBudget',
 'highEnergyJetCaptureZeroAssumptions','highEnergyJetCertifiedCore','highEnergyJetCertifyZeroAssumptions',
 'highEnergyJetCompactZeroArguments','highEnergyJetEffectiveParentDeadline','highEnergyJetRemainingTime',
 'highEnergyJetRetryableResourceQ','highEnergyLaurentJetCoefficientsNative')) + tuple(C+name for name in (
 'calibratedGraphUnion','coherentConversionNativeFields','coherentConversionWithLegs','coherentSourceConversion',
 'fourVectorAugmentationPlan','fourVectorCubicCatalog','fourVectorMetricCalibration','fourVectorMetricCalibrationWork',
 'fourVectorPairedLegOrders','fourVectorPairedOrder','preconditionCovariantPairs')) + tuple(S+name for name in (
 'minimalVectorAliasCertificate','minimalVectorLowerContactRequiredQ','minimalVectorVertexMatch','minimalVectorVertexOrder'))
PREFIXES = (P,C,S)
NATIVE = P+'highEnergyLaurentJetCoefficients'
NATIVE_ALIAS = NATIVE+'Native'
REVIEW_FLAGS = (
 'ConditionalHistoricalNormalFreshWorkerContractReviewed',
 'OldEnabledUncapturedWholeWarningsPreventComplete',
 'OuterQuietAndGeneralStopControlsReviewed',
 'GlobalOffAndInnerQuietCounterexamplesPreserved',
 'HistoricalMessageStateAndInitializationWereNotRecorded',
 'NoUniversalCheckerEquivalenceClaimed',
 'NativeCoreExactHeadRenameAndScalarVerifierPreservationReviewed',
 'NewStrictWholeWarningAndResourceClockControlsReviewed',
 'NewSourceProofAndFreshBackendEvidenceReviewedSeparately',
 'ActualProducerContextsAndRawResultsRetained',
)
DERIVED_SUMMARY_FIELDS = frozenset((
    'ContextCompatibility', 'PrecisionQualification', 'SupplementalParameterProbes',
    'SupplementalParameterProbeStatus', 'RecordedInputStatus'))


def digest_object(value):
    return hashlib.sha256(json.dumps(value,sort_keys=True).encode()).hexdigest()


def file_digest(path):
    value = hashlib.sha256()
    with Path(path).open('rb') as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b''):
            value.update(chunk)
    return value.hexdigest()


def _private_options(name, values):
    expected = {
        C+'calibratedCovariantSource':
            '{"TimeConstraint" -> 60, "AugmentFourVectorContacts" -> False, "ProgressFunction" -> None}',
        C+'fourVectorMetricCalibration':
            '{"TimeConstraint" -> 486, "ProgressFunction" -> None}',
    }
    if name in expected:
        options = expected[name]
        default = '{HoldPattern[Options[' + name + ']] :> ' + options + '}'
        if values['Options'] != options or values['DefaultValues'] != default:
            raise ValueError('The reviewed private options changed: '+name)


def _new_kind(name, values):
    nonempty = {k for k,v in values.items() if v != '{}'}
    if name in NEW_HEADS:
        expected = {'DownValues'}
        if name == P+'highEnergyJetCaptureZeroAssumptions': expected.add('Attributes')
        if name == C+'fourVectorMetricCalibration': expected |= {'Options','DefaultValues'}
        if nonempty != expected:
            raise ValueError('Unexpected new helper definition fields: '+name)
        if name == P+'highEnergyJetCaptureZeroAssumptions' and values['Attributes'] != '{HoldAll}':
            raise ValueError('The warning capture must hold its work.')
        _private_options(name, values)
        return 'Defined'
    if name.startswith(PREFIXES) and not nonempty: return 'Empty'
    if (name.startswith(PREFIXES) and name.endswith('$') and nonempty == {'Attributes'}
            and values['Attributes'] == '{Temporary}'): return 'Temporary'
    raise ValueError('Unreviewed new symbol or state: '+name)


def compare_transition(core, before, after, before_raw, after_raw, loader_evidence,
                       *, shadow_paths=None):
    """Literal analysis only. A checker change is NOT constructor-only equivalence."""
    if before['Programs'] != after['Programs'] or before['Files'].keys() != after['Files'].keys():
        raise ValueError('Runtime paths or manifest membership changed.')
    changed = {n for n in before['Files'] if before['Files'][n] != after['Files'][n]}
    if changed != set(FILES): raise ValueError('Only the three C11 source files may differ.')
    old = core.cold_pair(*before_raw, require_post=True)
    new = core.cold_pair(*after_raw, require_post=True)
    if any(before_raw[0].get(k) != after_raw[0].get(k) for k in ('WolframVersion','SystemID')):
        raise ValueError('Kernel version or platform changed.')
    if old.keys()-new.keys(): raise ValueError('An existing symbol disappeared.')
    differences = {}; path_artifacts = {}
    for name, values in old.items():
        right = dict(new[name])
        raw_fields = [f for f in FIELDS if values[f] != right[f]]
        if shadow_paths and name in PATH_CACHES and raw_fields == ['OwnValues']:
            a,b = shadow_paths
            if right['OwnValues'].replace(b,a) != values['OwnValues']:
                raise ValueError('Unknown shadow path cache change.')
            path_artifacts[name] = ['OwnValues']; right['OwnValues'] = values['OwnValues']
        fields = [f for f in FIELDS if values[f] != right[f]]
        if not fields: continue
        if name == LOADER:
            if (fields != ['OwnValues'] or loader_evidence.get('_VerifiedInputHashes') is not True
                    or loader_evidence.get('InputCount') != 17
                    or core._loader_value(values['OwnValues']) != loader_evidence.get('Before')
                    or core._loader_value(right['OwnValues']) != loader_evidence.get('After')):
                raise ValueError('Mechanical 17-input loader recomputation is required.')
        elif tuple(fields) != CHANGED.get(name):
            raise ValueError('An unlisted existing field changed: '+name)
        _private_options(name, right)
        differences[name] = fields
    if set(differences) != set(CHANGED)|{LOADER}:
        raise ValueError('The complete C11 implementation-change list is required.')
    if (old[NATIVE]['DownValues'] != new[NATIVE_ALIAS]['DownValues'].replace(
            'highEnergyLaurentJetCoefficientsNative','highEnergyLaurentJetCoefficients')):
        raise ValueError('The old native Laurent core is not an exact head rename.')
    additions = {name:_new_kind(name,values) for name,values in new.items() if name not in old}
    if {name for name,kind in additions.items() if kind=='Defined'} != set(NEW_HEADS):
        raise ValueError('Exactly the 26 declared private helpers are required.')
    return {'Status':'ANALYZED_NOT_SEALED','Kind':'ExplicitValidatorChangeWithConditionalHistoricalRetention',
            'ChangedExistingFields':differences,'NewSymbols':additions,'ShadowPathArtifacts':path_artifacts,
            'BeforeDefinitionsSHA256':digest_object(old),'AfterDefinitionsSHA256':digest_object(new),
            'BeforeManifestSHA256':digest_object(before),'AfterManifestSHA256':digest_object(after),
            'NativeCoreExactHeadRename':True,'ValidationImplementationChanged':True,
            'HistoricalEvidenceOnly':True,'ValidationRerunPerformed':False,
            'UniversalCheckerEquivalenceClaimed':False,'NewSourceValidationImplied':False,
            'FinalC10FiniteRecordSnapshotStillRequired':True}


def historical_record(summary_path, summary, capsule_path, frozen_record, review, analysis,
                      *, current_manifest, known_counterexamples=()):
    """Retain ONE original record as history, or return None. Never changes a file.

    The caller supplies a C10-admitted finite snapshot row and the separately
    reviewed finite history seal. This helper never constructs a new proof,
    updates a record or claims a current-validator pass.
    """
    try:
        if (review.get('Status') != 'SEALED_C11_HISTORICAL_RETENTION'
                or review.get('AnalysisSHA256') != digest_object(analysis)
                or review.get('CurrentManifestSHA256') != digest_object(current_manifest)
                or analysis.get('AfterManifestSHA256') != digest_object(current_manifest)
                or analysis.get('ValidationImplementationChanged') is not True
                or analysis.get('HistoricalEvidenceOnly') is not True
                or analysis.get('ValidationRerunPerformed') is not False
                or any(review.get(flag) is not True for flag in REVIEW_FLAGS)
                or known_counterexamples):
            return None
        raw = frozen_record['RawSummary']
        if (digest_object(frozen_record) not in review.get('FiniteSnapshotRowDigests', [])
                or not all(summary.get(key) == value for key,value in raw.items())
                or set(summary) - set(raw) - DERIVED_SUMMARY_FIELDS
                or summary.get('ContextSHA256') != frozen_record['ActualProducerContextSHA256']
                or summary.get('CapsuleSHA256') != frozen_record['CapsuleSHA256']):
            return None
        if (file_digest(summary_path) != frozen_record['SummarySHA256']
                or file_digest(capsule_path) != frozen_record['CapsuleSHA256']):
            return None
        return {'Status':'HISTORICAL_EVIDENCE_RETAINED_CONDITIONAL_FRESH_WORKER_CONTRACT',
                'ActualProducerContextSHA256':summary['ContextSHA256'],
                'RecordedStatus':summary['Status'],'ValidationRerunPerformed':False,
                'CurrentValidatorPassClaimed':False,'NewConstructorSuccessClaimed':False,
                'MutableGeneratedProjectIdentityClaimed':False}
    except (OSError,ValueError,TypeError,KeyError):
        return None


# The optional runtime part retains finite reviewed history only. All cache files
# live below ignored tests/results; importing the module never reads that cache.
BASE_CONTEXT = 'ac5e1399927d6be0e94a4b956591600fd949213f2ad479d43a7c3db27972fc74'
AFTER_CONTEXT = 'df7b89b04aef874fa933acb5daa1e502c7d34e387ccfba55e27cd8a21bd399ed'
KIND = 'ExplicitHEValidatorChangeConditionalHistoricalRetention'
CACHE = 'tests/results/SM-5-point/compatibility/validation-C11-installed-20261113'
REGISTRY = CACHE + '/history-seal.json'


def _object(path):
    value = json.loads(Path(path).read_text())
    if not isinstance(value, dict):
        raise ValueError('Expected a metadata object.')
    return value


def _inside(root, relative):
    value = Path(relative)
    if value.is_absolute() or '..' in value.parts:
        raise ValueError('Local history cache paths must remain inside the checkout.')
    result = (Path(root) / value).resolve()
    result.relative_to(Path(root).resolve())
    return result


def _stage(root):
    import importlib.util
    path = Path(root) / 'tests/SM-5-point/stage_context.py'
    spec = importlib.util.spec_from_file_location('hepcat_c11_stage', path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _read_bound(root, binding):
    path = _inside(root, binding['Path'])
    if file_digest(path) != binding['SHA256']:
        raise ValueError('A bound optional history cache changed.')
    return _object(path)


def verified_proof(root, here, configuration, current_context, proof_path=None):
    """Verify optional local history, or return None; no writes or calculations.

    The actual old checker is not declared equivalent to the current checker.
    A missing cache does not prevent new construction/validation. It only makes
    these old records unavailable as reviewed historical evidence.
    """
    try:
        root, here = Path(root).resolve(), Path(here).resolve()
        if current_context != AFTER_CONTEXT:
            return None
        path = Path(proof_path) if proof_path is not None else root / REGISTRY
        path.resolve().relative_to(root)
        seal = _object(path)
        if (seal.get('SchemaVersion') != 1 or seal.get('Kind') != KIND
                or seal.get('Status') != 'SEALED'
                or seal.get('BeforeContextSHA256') != BASE_CONTEXT
                or seal.get('AfterContextSHA256') != current_context
                or seal.get('CurrentValidatorPassClaimed') is not False
                or seal.get('ValidationRerunPerformed') is not False):
            return None
        stage = _stage(root)
        before = _read_bound(root, seal['BeforeManifest'])
        after = _read_bound(root, seal['AfterManifest'])
        actual = stage.manifest(root, here, configuration)
        if (stage.fingerprint(before) != BASE_CONTEXT
                or stage.fingerprint(after) != current_context or after != actual
                or before['Programs'] != after['Programs']
                or before['Files'].keys() != after['Files'].keys()
                or {k for k in before['Files'] if before['Files'][k] != after['Files'][k]} != set(FILES)):
            return None
        analysis = _read_bound(root, seal['Analysis'])
        review = _read_bound(root, seal['Review'])
        snapshot = _read_bound(root, seal['Snapshot'])
        expected_changes = {**CHANGED, LOADER: ('OwnValues',)}
        observed_changes = analysis.get('ChangedExistingFields', {})
        observed_helpers = analysis.get('NewSymbols', {})
        if (analysis.get('Status') != 'ANALYZED_NOT_SEALED'
                or not isinstance(observed_changes, dict)
                or {name: tuple(fields) for name,fields in observed_changes.items()} != expected_changes
                or not isinstance(observed_helpers, dict)
                or {name for name,kind in observed_helpers.items() if kind == 'Defined'} != set(NEW_HEADS)
                or analysis.get('NativeCoreExactHeadRename') is not True
                or analysis.get('ShadowPathArtifacts') != {}
                or analysis.get('BeforeManifestSHA256') != digest_object(before)
                or analysis.get('AfterManifestSHA256') != digest_object(after)
                or analysis.get('ValidationImplementationChanged') is not True
                or analysis.get('HistoricalEvidenceOnly') is not True
                or analysis.get('ValidationRerunPerformed') is not False
                or analysis.get('UniversalCheckerEquivalenceClaimed') is not False
                or review.get('Status') != 'SEALED_C11_HISTORICAL_RETENTION'
                or review.get('AnalysisSHA256') != digest_object(analysis)
                or review.get('CurrentManifestSHA256') != digest_object(actual)
                or any(review.get(flag) is not True for flag in REVIEW_FLAGS)
                or not isinstance(review.get('ReviewedBy'), str) or not review['ReviewedBy'].strip()
                or snapshot.get('Status') != 'FINAL_C10_ADMITTED_RECORD_SNAPSHOT'
                or snapshot.get('AdmissionContextSHA256') != BASE_CONTEXT
                or snapshot.get('ActualProducerContextsPreserved') is not True
                or snapshot.get('CurrentC11ValidationClaimed') is not False
                or snapshot.get('CapsulesCopied') is not False):
            return None
        # Bind the implementation used for admission, including this module and
        # the tiny dispatcher. These are ordinary code files, never raw results.
        tools = seal.get('VerifierToolsSHA256')
        if not isinstance(tools, dict) or set(tools) != {
                'tests/SM-5-point/validation_context_c11.py',
                'tests/SM-5-point/constructor_context.py',
                'tests/SM-5-point/stage_context.py'}:
            return None
        if any(file_digest(_inside(root, name)) != value for name,value in tools.items()):
            return None
        rows = snapshot.get('Records')
        if not isinstance(rows, list) or not rows:
            return None
        if review.get('FiniteSnapshotRowDigests') != [digest_object(row) for row in rows]:
            return None
        indexed = {}
        for row in rows:
            summary_path = _inside(root, row['SummaryPath'])
            capsule_path = _inside(root, row['CapsulePath'])
            if (summary_path in indexed or capsule_path != summary_path.with_suffix('.wxf')
                    or row.get('AdmittedAtContextSHA256') != BASE_CONTEXT
                    or row['RawSummary'].get('ContextSHA256') != row['ActualProducerContextSHA256']
                    or row['RawSummary'].get('CapsuleSHA256') != row['CapsuleSHA256']):
                return None
            indexed[summary_path] = row
        return {**seal, '_C11VerifiedCurrentContext': True, '_VerifiedCurrentContext': True,
                '_ProofSHA256': file_digest(path), '_Records': indexed, '_Review': review,
                '_Analysis': analysis, '_Manifest': actual}
    except (OSError, ValueError, KeyError, TypeError, AttributeError):
        return None


def public_proof(proof, stage=None):
    if not isinstance(proof, dict) or proof.get('_C11VerifiedCurrentContext') is not True:
        return None
    return {'Status': 'CONDITIONAL_HISTORICAL_EVIDENCE_RETENTION_EXPLICIT_VALIDATOR_CHANGE',
            'Kind': KIND, 'Stage': stage, 'BeforeContextSHA256': BASE_CONTEXT,
            'AfterContextSHA256': proof['AfterContextSHA256'],
            'AnalysisSHA256': proof['Analysis']['SHA256'],
            'ReviewSHA256': proof['Review']['SHA256'], 'ProofSHA256': proof['_ProofSHA256'],
            'HistoricalEvidenceOnly': True, 'ValidationImplementationChanged': True,
            'ValidationRerunPerformed': False, 'ConstructorRerunPerformed': False,
            'CurrentValidatorPassClaimed': False, 'CurrentConstructorSuccessClaimed': False,
            'UniversalCheckerEquivalenceClaimed': False,
            'HistoricalMessageStateWasNotRecorded': True,
            'ConditionalNormalFreshWorkerContractOnly': True,
            'MutableGeneratedProjectIdentityClaimed': False,
            'PrecisionQualificationUnchangedRequired': True}


def record_evidence(summary_path, summary, current_context, proof):
    try:
        if (not isinstance(proof, dict) or proof.get('_C11VerifiedCurrentContext') is not True
                or current_context != AFTER_CONTEXT or proof.get('AfterContextSHA256') != current_context):
            return None
        path = Path(summary_path).resolve()
        row = proof['_Records'].get(path)
        if row is None:
            return None
        evidence = historical_record(path, summary, path.with_suffix('.wxf'), row,
            proof['_Review'], proof['_Analysis'], current_manifest=proof['_Manifest'])
        if evidence is None:
            return None
        return {**public_proof(proof, 'HistoricalExportReport' if row['Kind']=='Export'
                                                   else 'HistoricalNativeValidation'),
                **evidence, 'RecordedContextSHA256': row['ActualProducerContextSHA256'],
                'CurrentContextSHA256': current_context}
    except (OSError, ValueError, KeyError, TypeError, AttributeError):
        return None
