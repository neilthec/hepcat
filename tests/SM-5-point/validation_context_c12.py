"""Private C12 constructor-only transition analysis. No installation or writes.

A C12 identity proof does not upgrade the earlier C11 conditional HE history.
Only the exact finite baseline may later be retained; incoming private pilots
need separate actual validation-context evidence and never enter implicitly.
"""
# HEPCAT - High Energy Physics Constructive Amplitude Tool
# Copyright (C) 2026 Neil Christensen
# SPDX-License-Identifier: GPL-3.0-or-later
import hashlib
import json

P='ConstructiveDiagrams`Private`'
A='HEPCATMinimalAdjointCubicSources`Private`'
Q='HEPCATMinimalQCDCurrentSources`Private`'
FIELDS=('DownValues','UpValues','SubValues','NValues','OwnValues','DefaultValues',
        'FormatValues','Options','Attributes','Messages')
PATTERNS=('ConstructiveDiagrams`*','ConstructiveDiagrams`Private`*',
          'HEPCATCovariantCurrentSources`*','HEPCATCovariantCurrentSources`Private`*',
          'HEPCATCovariantContactCalibration`*','HEPCATCovariantContactCalibration`Private`*',
          'HEPCATMinimalAdjointCubicSources`*','HEPCATMinimalAdjointCubicSources`Private`*',
          'HEPCATMinimalQCDCurrentSources`*','HEPCATMinimalQCDCurrentSources`Private`*')
NEW_HEADS=tuple(A+x for x in ('zero','canonicalColour','canonicalMetric','operator','chart','match'))+tuple(
 Q+x for x in ('sourceClass','canonicalColour','catalog','source','sourceCertificate'))+tuple(
 P+x for x in ('colorQCDCurrentSourceClass','colorQCDCurrentSource','colorQCDCurrentSourceCertificate','colorQCDCurrentPrepare','colorQCDCurrentProjection'))
CHANGED_FILES=('source/HEPCAT.wl','source/HEPCAT-Model.wl','source/internal/HEPCAT-Color.wl')
ADDED_FILE='source/internal/HEPCAT-QCDCurrents.wl'
NEW_MODULE='internal/HEPCAT-QCDCurrents.wl'
COLOUR=P+'colorConstructAmplitude'
SIGNATURE=P+'amplitudeParallelSignature'
LOADER=P+'$amplitudeParallelLoadedSignature'
PATH_CACHES=tuple(P+x for x in ('$amplitudeParallelSourceDirectory','$spinasExportSourceDirectory','$spinasExportRuntimeDirectory'))
EXPECTED_CHANGED={COLOUR:['DownValues'],SIGNATURE:['DownValues'],LOADER:['OwnValues']}


def digest_object(value):
    return hashlib.sha256(json.dumps(value,sort_keys=True).encode()).hexdigest()


def sha_text(value):
    return hashlib.sha256(value.encode()).hexdigest()


def cold_pair(core, first, second):
    """The exact ordered superset policy is checked before the legacy reader."""
    if first.get('CapturedNamePatterns')!=list(PATTERNS) or second.get('CapturedNamePatterns')!=list(PATTERNS):
        raise ValueError('Exactly the ordered six original and four new namespace patterns are required.')
    return core.cold_pair(first,second,require_post=False)


def source_scope(before, after, old_text, new_text):
    if before['Programs']!=after['Programs']:
        raise ValueError('A numerical program path changed.')
    old_files,new_files=before['Files'],after['Files']
    if set(new_files)-set(old_files)!={ADDED_FILE} or set(old_files)-set(new_files):
        raise ValueError('Exactly the new QCD source module may be added.')
    if {k for k in old_files if old_files[k]!=new_files[k]}!=set(CHANGED_FILES):
        raise ValueError('Only loader, signature registry and colour constructor files may change.')
    if set(old_text)!=set(CHANGED_FILES) or set(new_text)!=set(CHANGED_FILES)|{ADDED_FILE}:
        raise ValueError('All four reviewed literal source inputs are required.')
    if any(sha_text(value)!=old_files[name] for name,value in old_text.items()) or any(
            sha_text(value)!=new_files[name] for name,value in new_text.items()):
        raise ValueError('Literal source text is not bound to its manifest.')
    insertion='      "'+NEW_MODULE+'",\n'
    if new_text['source/HEPCAT-Model.wl'].count(insertion)!=1 or new_text['source/HEPCAT-Model.wl'].replace(insertion,'',1)!=old_text['source/HEPCAT-Model.wl']:
        raise ValueError('Model may only add the single required-file registry entry.')
    insertion=',\n    "'+NEW_MODULE+'"'
    if new_text['source/HEPCAT.wl'].count(insertion)!=1 or new_text['source/HEPCAT.wl'].replace(insertion,'',1)!=old_text['source/HEPCAT.wl']:
        raise ValueError('The package loader may only append the single new source file.')


def new_symbol_inventory(old, new, approved):
    rows=approved.get('NamesAndFieldHashes')
    if not isinstance(rows,list) or [r.get('Name') for r in rows]!=sorted(new.keys()-old.keys()):
        raise ValueError('Exact sorted new-name inventory is required; no suffix fallback exists.')
    counts={'Defined':0,'Empty':0,'Temporary':0}
    for row in rows:
        name=row['Name'];fields=new[name]
        if not name.startswith((P,A,Q)) or row.get('FieldsSHA256')!=digest_object(fields):
            raise ValueError('A new private symbol name or full field hash differs.')
        nonempty={k:v for k,v in fields.items() if v!='{}'}
        if name in NEW_HEADS:
            if set(nonempty)!={'DownValues'}:raise ValueError('A declared helper has unreviewed state or attributes.')
            kind='Defined'
        elif not nonempty:kind='Empty'
        elif nonempty=={'Attributes':'{Temporary}'} and name.endswith('$'):kind='Temporary'
        else:raise ValueError('A new symbol has unreviewed state or attributes.')
        if row.get('Kind')!=kind or row.get('NonemptyFields')!=nonempty:
            raise ValueError('A reviewed new symbol shape changed.')
        counts[kind]+=1
    if counts!={'Defined':16,'Empty':171,'Temporary':152} or approved.get('Counts')!=counts:
        raise ValueError('Exactly the reviewed16/171/152 inventory is required.')
    if set(NEW_HEADS)-set(new):raise ValueError('A declared new helper is absent.')
    return counts


def analyze(core,before,after,before_raw,after_raw,approved_inventory,loader_evidence,
            old_text,new_text,*,shadow_paths=None):
    source_scope(before,after,old_text,new_text)
    old=cold_pair(core,*before_raw);new=cold_pair(core,*after_raw)
    if any(before_raw[0].get(k)!=after_raw[0].get(k) for k in ('WolframVersion','SystemID')):
        raise ValueError('Kernel version/platform changed.')
    if old.keys()-new.keys():raise ValueError('An old symbol disappeared.')
    differences={};path_artifacts={}
    for name,left in old.items():
        right=dict(new[name]);fields=[f for f in FIELDS if left[f]!=right[f]]
        if shadow_paths and name in PATH_CACHES and fields==['OwnValues']:
            canonical,private=shadow_paths
            if right['OwnValues'].replace(private,canonical)!=left['OwnValues']:
                raise ValueError('Unknown private-tree path-cache artifact.')
            path_artifacts[name]=['OwnValues'];right['OwnValues']=left['OwnValues']
        fields=[f for f in FIELDS if left[f]!=right[f]]
        if not fields:continue
        if fields!=EXPECTED_CHANGED.get(name):raise ValueError('An unreviewed old definition field changed: '+name)
        if name==SIGNATURE:
            insertion='"'+NEW_MODULE+'", '
            if right['DownValues'].count(insertion)!=1 or right['DownValues'].replace(insertion,'',1)!=left['DownValues']:
                raise ValueError('The loader hash formula changed beyond the literal registry entry.')
        if name==LOADER:
            if (loader_evidence.get('_VerifiedInputHashes') is not True
                    or loader_evidence.get('BeforeInputCount')!=17 or loader_evidence.get('AfterInputCount')!=18
                    or core._loader_value(left['OwnValues'])!=loader_evidence.get('Before')
                    or core._loader_value(right['OwnValues'])!=loader_evidence.get('After')):
                raise ValueError('Actual independent17-to18-input recomputation is required.')
        differences[name]=fields
    if differences!=EXPECTED_CHANGED:raise ValueError('All and only the three reviewed existing symbols must change.')
    counts=new_symbol_inventory(old,new,approved_inventory)
    return {'Status':'ANALYZED_NOT_SEALED','Kind':'ConstructorOnlyQCDSourceTransitionAfterConditionalC11History',
            'ChangedExistingFields':differences,'ShadowPathArtifacts':path_artifacts,
            'BeforeDefinitionsSHA256':digest_object(old),'AfterDefinitionsSHA256':digest_object(new),
            'BeforeManifestSHA256':digest_object(before),'AfterManifestSHA256':digest_object(after),
            'NewSymbolInventorySHA256':digest_object(approved_inventory),'NewSymbolCounts':counts,
            'AllExistingValidatorFieldsLiterallyUnchanged':True,'AllExistingOptionsDefaultsMessagesAttributesUnchanged':True,
            'ExistingC11ConditionalHistoryMustRemain':True,'HistoricalProducerContextsMustRemain':True,
            'CurrentC12ValidationRerunClaimed':False,'CurrentC12ConstructorRerunClaimed':False,
            'PrivatePilotsOutsideFiniteSnapshotAdmitted':False,
            'OldC10QCDPilotHEUpgradeImplied':False,'ReadyForInstalledSeal':not bool(path_artifacts)}

# Optional finite local history. Missing cache means unavailable or regenerate.
from pathlib import Path
import importlib.util

BASE_CONTEXT = 'df7b89b04aef874fa933acb5daa1e502c7d34e387ccfba55e27cd8a21bd399ed'
AFTER_CONTEXT = '1a9929f037dca092fb9c41b8d55a8f94954a17fa0fd8f7de5293bf312a85f31b'
KIND = 'ConstructorOnlyQCDSourceAfterConditionalC11HistoricalRetention'
CACHE = 'tests/results/SM-5-point/compatibility/validation-C12-installed-20261113'
REGISTRY = CACHE + '/history-seal.json'
REVIEW_FLAGS = (
    'ExactFourSourceDiffsReviewed',
    'OnlyReviewedTwoExistingDownValuesAndMechanicalLoaderOwnValueChanged',
    'All8987OldSymbolsAllTenFieldsOtherwiseExact',
    'AllExistingValidatorDefinitionsLiterallyUnchangedFromC11',
    'ExactNew16Defined171Empty152TemporaryInventoryReviewed',
    'NoPublicOptionsDefaultsMessagesAttributesChanged',
    'Loader17To18RegistryAndHashFormulaReviewed',
    'NamespaceQualificationAndCompiledAlphaProofReviewed',
    'Finite609OldRawRecordsOnlyAndProducerContextsPreserved',
    'ExistingC11ConditionalHEHistoryPreservedWithoutUpgrade',
    'OldC10PrivateQCDPilotsOutsideFiniteSnapshotNotImplicitlyAdmitted',
    'NoConstructorOrValidationRerunClaimedForHistoricalRecords',
    'MissingLocalCacheMeansUnavailableOrRegenerate',
)
DERIVED_SUMMARY_FIELDS = frozenset((
    'ContextCompatibility', 'PrecisionQualification', 'SupplementalParameterProbes',
    'SupplementalParameterProbeStatus', 'RecordedInputStatus'))
VERIFIER_TOOLS = frozenset((
    'tests/SM-5-point/validation_context_c12.py',
    'tests/SM-5-point/constructor_context.py',
    'tests/SM-5-point/stage_context.py'))


def file_digest(path):
    value = hashlib.sha256()
    with Path(path).open('rb') as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b''):
            value.update(chunk)
    return value.hexdigest()


def _object(path):
    value = json.loads(Path(path).read_text())
    if not isinstance(value, dict):
        raise ValueError('Expected a metadata object.')
    return value


def _inside(root, relative):
    path = Path(relative)
    if path.is_absolute() or '..' in path.parts:
        raise ValueError('Optional history cache must remain inside the checkout.')
    result = (Path(root) / path).resolve()
    result.relative_to(Path(root).resolve())
    return result


def _bound(root, binding):
    path = _inside(root, binding['Path'])
    if file_digest(path) != binding['SHA256']:
        raise ValueError('A bound optional history cache changed.')
    return _object(path)


def _stage(root):
    spec = importlib.util.spec_from_file_location(
        'hepcat_c12_stage', Path(root) / 'tests/SM-5-point/stage_context.py')
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def verified_proof(root, here, configuration, current_context, proof_path=None):
    """Verify only the exact finite C11 snapshot, with no new validation claim."""
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
                or seal.get('ValidationRerunPerformed') is not False
                or seal.get('CurrentConstructorSuccessClaimed') is not False):
            return None
        stage = _stage(root)
        before = _bound(root, seal['BeforeManifest'])
        after = _bound(root, seal['AfterManifest'])
        actual = stage.manifest(root, here, configuration)
        if (stage.fingerprint(before) != BASE_CONTEXT
                or stage.fingerprint(after) != current_context or after != actual
                or before['Programs'] != after['Programs']
                or set(after['Files']) - set(before['Files']) != {ADDED_FILE}
                or set(before['Files']) - set(after['Files'])
                or {name for name in before['Files']
                    if before['Files'][name] != after['Files'][name]} != set(CHANGED_FILES)):
            return None
        analysis = _bound(root, seal['Analysis'])
        review = _bound(root, seal['Review'])
        snapshot = _bound(root, seal['Snapshot'])
        if (analysis.get('Status') != 'ANALYZED_NOT_SEALED'
                or analysis.get('ChangedExistingFields') != EXPECTED_CHANGED
                or analysis.get('NewSymbolCounts') != {'Defined': 16, 'Empty': 171, 'Temporary': 152}
                or analysis.get('BeforeManifestSHA256') != digest_object(before)
                or analysis.get('AfterManifestSHA256') != digest_object(after)
                or analysis.get('ShadowPathArtifacts') != {}
                or analysis.get('ReadyForInstalledSeal') is not True
                or analysis.get('ActualInstalledRootCaptures') is not True
                or analysis.get('AllExistingValidatorFieldsLiterallyUnchanged') is not True
                or analysis.get('ExistingC11ConditionalHistoryMustRemain') is not True
                or analysis.get('HistoricalProducerContextsMustRemain') is not True
                or analysis.get('CurrentC12ValidationRerunClaimed') is not False
                or analysis.get('CurrentC12ConstructorRerunClaimed') is not False
                or analysis.get('PrivatePilotsOutsideFiniteSnapshotAdmitted') is not False
                or analysis.get('OldC10QCDPilotHEUpgradeImplied') is not False
                or review.get('Status') != 'SEALED_C12_CONSTRUCTOR_ONLY_HISTORICAL_RETENTION'
                or review.get('AnalysisSHA256') != seal['Analysis']['SHA256']
                or review.get('BeforeContextSHA256') != BASE_CONTEXT
                or review.get('AfterContextSHA256') != current_context
                or any(review.get(flag) is not True for flag in REVIEW_FLAGS)
                or not isinstance(review.get('ReviewedBy'), str) or not review['ReviewedBy'].strip()
                or snapshot.get('Status') != 'FINAL_C11_ADMITTED_RECORD_SNAPSHOT'
                or snapshot.get('ActualProducerContextsPreserved') is not True
                or snapshot.get('ExistingC11ConditionalHEHistoryPreserved') is not True
                or snapshot.get('ExistingC11HistoryNotUpgradedToLiteralCheckerEquivalence') is not True
                or snapshot.get('C12ValidationRerunClaimed') is not False
                or snapshot.get('CapsulesCopied') is not False
                or snapshot.get('AdmissionContextSHA256') != BASE_CONTEXT):
            return None
        tools = seal.get('VerifierToolsSHA256')
        if not isinstance(tools, dict) or set(tools) != VERIFIER_TOOLS:
            return None
        if (review.get('VerifierToolsSHA256') != tools
                or any(file_digest(_inside(root, name)) != value for name, value in tools.items())):
            return None
        rows = snapshot.get('Records')
        if (not isinstance(rows, list) or len(rows) != 609
                or sum(row.get('Kind') == 'Native' for row in rows) != 356
                or sum(row.get('Kind') == 'Export' for row in rows) != 253
                or review.get('FiniteSnapshotRowDigests') != [digest_object(row) for row in rows]):
            return None
        indexed = {}
        for row in rows:
            summary_path = _inside(root, row['SummaryPath'])
            capsule_path = _inside(root, row['CapsulePath'])
            old_history = row.get('ExistingC11HistoricalAdmission')
            if (summary_path in indexed or capsule_path != summary_path.with_suffix('.wxf')
                    or row.get('AdmittedAtContextSHA256') != BASE_CONTEXT
                    or row.get('C12ValidationRerunClaimed') is not False
                    or row['RawSummary'].get('ContextSHA256') != row['ActualProducerContextSHA256']
                    or row['RawSummary'].get('CapsuleSHA256') != row['CapsuleSHA256']
                    or (row['ActualProducerContextSHA256'] != BASE_CONTEXT
                        and not isinstance(old_history, dict))):
                return None
            if isinstance(old_history, dict) and (
                    old_history.get('CurrentValidatorPassClaimed') is not False
                    or old_history.get('ValidationRerunPerformed') is not False):
                return None
            indexed[summary_path] = row
        return {**seal, '_C12VerifiedCurrentContext': True, '_VerifiedCurrentContext': True,
                '_ProofSHA256': file_digest(path), '_Records': indexed,
                '_Review': review, '_Analysis': analysis, '_Manifest': actual}
    except (OSError, ValueError, KeyError, TypeError, AttributeError):
        return None


def public_proof(proof, stage=None):
    if not isinstance(proof, dict) or proof.get('_C12VerifiedCurrentContext') is not True:
        return None
    return {'Status': 'CONSTRUCTOR_ONLY_FINITE_HISTORICAL_EVIDENCE_RETENTION',
            'Kind': KIND, 'Stage': stage, 'BeforeContextSHA256': BASE_CONTEXT,
            'AfterContextSHA256': proof['AfterContextSHA256'],
            'AnalysisSHA256': proof['Analysis']['SHA256'],
            'ReviewSHA256': proof['Review']['SHA256'], 'ProofSHA256': proof['_ProofSHA256'],
            'ExistingValidatorImplementationUnchangedFromC11': True,
            'ExistingC11ConditionalHistoryPreserved': True,
            'HistoricalEvidenceOnly': True, 'ValidationRerunPerformed': False,
            'ConstructorRerunPerformed': False, 'CurrentValidatorPassClaimed': False,
            'CurrentConstructorSuccessClaimed': False,
            'UniversalOldCheckerEquivalenceClaimed': False,
            'MutableGeneratedProjectIdentityClaimed': False,
            'PrecisionQualificationUnchangedRequired': True}


def record_evidence(summary_path, summary, current_context, proof):
    try:
        if (not isinstance(proof, dict) or proof.get('_C12VerifiedCurrentContext') is not True
                or current_context != AFTER_CONTEXT or proof.get('AfterContextSHA256') != current_context):
            return None
        path = Path(summary_path).resolve()
        row = proof['_Records'].get(path)
        if row is None:
            return None
        raw = row['RawSummary']
        if (not all(summary.get(key) == value for key, value in raw.items())
                or set(summary) - set(raw) - DERIVED_SUMMARY_FIELDS
                or summary.get('ContextSHA256') != row['ActualProducerContextSHA256']
                or summary.get('CapsuleSHA256') != row['CapsuleSHA256']
                or file_digest(path) != row['SummarySHA256']
                or file_digest(path.with_suffix('.wxf')) != row['CapsuleSHA256']):
            return None
        result = {**public_proof(proof, 'HistoricalExportReport' if row['Kind'] == 'Export'
                                                         else 'HistoricalNativeValidation'),
                  'RecordedStatus': summary['Status'],
                  'ActualProducerContextSHA256': row['ActualProducerContextSHA256'],
                  'RecordedContextSHA256': row['ActualProducerContextSHA256'],
                  'CurrentContextSHA256': current_context}
        if isinstance(row.get('ExistingC11HistoricalAdmission'), dict):
            result['ExistingC11HistoricalAdmission'] = row['ExistingC11HistoricalAdmission']
        return result
    except (OSError, ValueError, KeyError, TypeError, AttributeError):
        return None
