"""Finite historical validation continuity for the reviewed C3 to C4 change.

This module never promotes a result or claims a new construction/revalidation.
A sealed proof requires exact cold definitions plus an explicit finite-colour
algebra review. It does not exempt a namespace or an arbitrary colour change.
"""
from pathlib import Path
import hashlib
import importlib.util
import json

BASE_CONTEXT = '55701e7a9f6d674b7f37aa1ced637de0e9e1934badd54be9dce03244084725f9'
BASE_DIRECTORY = ('tests/results/SM-5-point/compatibility/constructor-C3-pre-C4-'
                  + BASE_CONTEXT + '-20261112T230045Z')
ANCHORS = {
    'baseline.json': '0a40de1349e06757a9e1b4a702bf7c78779b4f72c7655ad5ac418dbd9ab7c463',
    'manifest.json': '3c5e419bec33ab4a4547fda6cacffc5fff2491ac274f3fd5867b2b96313dadc0',
    'checkpoint-snapshot.json': 'a8fee7cc59d68c8227e053d02d0ac2cc4af8939a1420267130195b4b11804944',
    'definition-fingerprints.json': '41c567313dfea15d803129e40eff6140636e8ad7af6f74f5578f0c5835c37530',
    'archive-index.json': 'd9460c3f8f9765997cf3777033ab16cd0df691eaf0dc44da74b889a6ca8fe166',
}
CHANGED_FILES = ('source/HEPCAT-Model.wl', 'source/internal/HEPCAT-Color.wl',
                 'source/internal/HEPCAT-CovariantContacts.wl')
CONSTRUCTOR = 'ConstructiveDiagrams`ConstructAmplitude'
CONTACT = 'HEPCATCovariantContactCalibration`Private`calibratedCovariantSource'
COLOR_PREFIX = 'ConstructiveDiagrams`Private`'
COLOR_FUNCTIONS = tuple(COLOR_PREFIX + name for name in
                        ('colorTensorVector', 'colorTensorBasis', 'colorProjectionData'))
NEW_FUNCTIONS = tuple(COLOR_PREFIX + name for name in
                     ('colorTensorVectorEnumerated', 'colorNetworkIndexDimension',
                      'colorNetworkAxisOrder', 'colorNetworkContract', 'colorNetworkMonomial',
                      'colorTensorVectorSparseDetails', 'colorTensorVectorSparse', 'colorSparseTensorGram'))
KIND = 'ExactCandidateAndFiniteColourHistoricalValidationTransition'
REGISTRY = 'constructor-C3-C4-context-bridge.json'
REVIEW_FLAGS = ('ConstructorAndContactChangesAreMetadataGuardsOnly',
                'NoIndependentBenchmarkUsedForCandidateConstruction',
                'ExistingCheckerModelAndRuntimeDefinitionsUnchanged',
                'NewPrivateDefinitionsHaveNoLoadTimeMutation',
                'ExactColourFiniteSumAndIncidenceProofReviewed',
                'LegacyEnumeratorPreservedExactlyByRename',
                'ExternalAxisOrderAndComplexConjugationPreserved',
                'All65ColourControlsReviewed',
                'All45Rank6And3960OccurrenceCertificatesReviewed',
                'LiteralThird65536AndFourthTrueDefaultsReviewed',
                'MechanicalLoaderSignatureInputsAndResultsReviewed')


def legacy():
    spec = importlib.util.spec_from_file_location('hepcat_c4_legacy', Path(__file__).with_name('constructor_context.py'))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


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
        raise ValueError('Evidence paths must remain inside the checkout.')
    result = (Path(root) / value).resolve()
    result.relative_to(Path(root).resolve())
    return result


def load_baseline(root):
    directory = inside(root, BASE_DIRECTORY)
    for name, expected in ANCHORS.items():
        if digest(directory / name) != expected:
            raise ValueError('Pinned C3 baseline changed: ' + name)
    record = object_at(directory / 'baseline.json')
    manifest = object_at(directory / 'manifest.json')
    index = object_at(directory / 'archive-index.json')['Files']
    if record.get('Status') != 'SEALED' or record.get('ContextSHA256') != BASE_CONTEXT:
        raise ValueError('The exact reviewed C3 baseline is required.')
    archive = directory / 'archive'
    for relative, row in index.items():
        if digest(inside(archive, relative)) != row['SHA256']:
            raise ValueError('A C3 source/proof/summary archive changed: ' + relative)
    core = legacy()
    fp = object_at(directory / 'definition-fingerprints.json')
    paths = [inside(archive, name) for name in fp['ColdCapturePaths']]
    if len(paths) != 2 or [digest(path) for path in paths] != fp['ColdCaptureSHA256']:
        raise ValueError('The exact two archived C3 cold captures are required.')
    raw = [object_at(path) for path in paths]
    table = core.cold_pair(*raw, require_post=True)
    if len(table) != 8131 or canonical_digest(table) != fp['DefinitionTableSHA256']:
        raise ValueError('C3 cold definitions do not reconstruct.')
    old = object_at(archive / 'tests/SM-5-point/constructor-context-bridge.json')
    if (old.get('Status') != 'SEALED' or old.get('AfterContextSHA256') != BASE_CONTEXT
            or old.get('HistoricalEvidenceOnly') is not True):
        raise ValueError('The pinned C1/C2 to C3 proof chain is unavailable.')
    old_baseline = core.load_baseline(archive)
    snapshot = object_at(directory / 'checkpoint-snapshot.json')['Entries']
    rows = {Path(row['Summary']).name: row for row in snapshot}
    if len(rows) != len(snapshot) or len(rows) != 325:
        raise ValueError('The finite C3 checkpoint identities are not exact.')
    return {'Directory': directory, 'Archive': archive, 'Record': record,
            'Manifest': manifest, 'Raw': raw, 'Snapshot': rows,
            'PriorProof': {**old, '_VerifiedCurrentContext': True}, 'PriorBaseline': old_baseline}


def compare_transition(before, after, before_raw, after_raw, loader_evidence=None):
    """Pure exact comparison. The result is analysis only, not admission."""
    core = legacy()
    if before.get('Programs') != after.get('Programs') or set(before['Files']) != set(after['Files']):
        raise ValueError('Installation paths or manifest membership changed.')
    changed = sorted(name for name in before['Files'] if before['Files'][name] != after['Files'][name])
    if changed != sorted(CHANGED_FILES):
        raise ValueError('Only the three literally reviewed C4 source files may change.')
    a = core.cold_pair(*before_raw, require_post=True)
    b = core.cold_pair(*after_raw, require_post=True)
    if any(before_raw[0].get(key) != after_raw[0].get(key) for key in ('WolframVersion', 'SystemID')):
        raise ValueError('Wolfram version or platform changed.')
    if set(a) - set(b):
        raise ValueError('An existing C3 symbol disappeared.')
    differences = {}
    allowed = {CONSTRUCTOR: {'DownValues', 'Messages'}, CONTACT: {'DownValues'},
               **{name: {'DownValues'} for name in COLOR_FUNCTIONS}}
    for name, fields in a.items():
        delta = [field for field in core.FIELDS if fields[field] != b[name][field]]
        if not delta:
            continue
        if name == core.LOADER_CACHE:
            if (delta != ['OwnValues'] or not isinstance(loader_evidence, dict)
                    or loader_evidence.get('_VerifiedInputHashes') is not True
                    or core._loader_value(fields['OwnValues']) != loader_evidence.get('Before')
                    or core._loader_value(b[name]['OwnValues']) != loader_evidence.get('After')):
                raise ValueError('The mechanical loader cache needs exact verified recomputations.')
        elif name not in allowed or not set(delta) <= allowed[name]:
            raise ValueError('An unreviewed existing definition field changed: ' + name)
        elif 'Messages' in delta and not core.usage_only(fields['Messages'], b[name]['Messages']):
            raise ValueError('Constructor Messages differences must be usage-only.')
        differences[name] = delta
    if any('DownValues' not in differences.get(name, []) for name in allowed):
        raise ValueError('The exact five expected implementation changes are required.')
    enum = COLOR_PREFIX + 'colorTensorVectorEnumerated'
    if b.get(enum, {}).get('DownValues') != a[COLOR_FUNCTIONS[0]]['DownValues'].replace(COLOR_FUNCTIONS[0], enum):
        raise ValueError('The retained enumerator differs beyond its fully qualified self name.')
    signature = b[COLOR_FUNCTIONS[1]]['DownValues'].split(']] :>', 1)[0]
    if ('maxStates_Integer:65536' not in signature or 'materializeOriginalGram_:True' not in signature):
        raise ValueError('The literal private third/fourth defaults changed.')
    new_defined, new_empty, new_temporary = [], [], []
    for name in sorted(set(b) - set(a)):
        fields = b[name]
        if name in NEW_FUNCTIONS:
            if not fields['DownValues'] or fields['DownValues'] == '{}' or any(
                    value != '{}' for field, value in fields.items() if field != 'DownValues'):
                raise ValueError('A new colour helper has unexpected definition fields: ' + name)
            new_defined.append(name)
        elif name.startswith(COLOR_PREFIX) and all(value == '{}' for value in fields.values()):
            new_empty.append(name)
        elif (name.startswith(COLOR_PREFIX) and name.endswith('$')
              and fields['Attributes'] == '{Temporary}'
              and all(value == '{}' for field, value in fields.items() if field != 'Attributes')):
            new_temporary.append(name)
        else:
            raise ValueError('A new public/unlisted/defined temporary symbol is not reviewed: ' + name)
    if sorted(new_defined) != sorted(NEW_FUNCTIONS):
        raise ValueError('The exact eight new private colour helpers are required.')
    return {'Kind': KIND, 'Status': 'ANALYZED_NOT_SEALED',
            'BeforeContextSHA256': core.stage_module().fingerprint(before),
            'AfterContextSHA256': core.stage_module().fingerprint(after),
            'ChangedExistingFiles': changed, 'ChangedDefinitionFields': differences,
            'ExistingSymbolsChecked': len(a), 'NewSymbolsChecked': len(b)-len(a),
            'NewDefinedPrivateSymbols': new_defined, 'NewEmptyPrivateSymbols': new_empty,
            'NewTemporaryPrivateSymbols': new_temporary,
            'LiteralPrivateDefaultsPreserved': True, 'RetainedEnumeratorExactRename': True,
            'AllOtherTenFieldDefinitionsExactlyUnchanged': True,
            'BeforeColdDefinitionsSHA256': canonical_digest(a),
            'AfterColdDefinitionsSHA256': canonical_digest(b),
            'MechanicalLoaderSignatureEvidence': {key: value for key, value in (loader_evidence or {}).items() if not key.startswith('_')},
            'ConstructorRerunPerformed': False, 'ValidationRerunPerformed': False}


def loader_script(paths, output):
    code = ''.join(name[0].lower() + name[1:] + '=' + json.dumps(value).replace('[', '{').replace(']', '}') + ';\n'
                   for name, value in paths.items())
    return code + ('record=<|"Before"->Hash[Join[FileHash/@beforeRequired,FileHash/@beforeOptional]],'
                   '"After"->Hash[Join[FileHash/@afterRequired,FileHash/@afterOptional]],'
                   '"Method"->"ExactUnchangedLoaderFileHashFormulaVerifiedArchiveOrIdenticalSHAInputs",'
                   '"PhysicsCalculationsPerformed"->False|>;\nExport[' + json.dumps(str(output))
                   + ',record,"RawJSON"];\nPrint[record];\n')


def prepare_loader_evidence(root, after_manifest_path, directory):
    root, directory = Path(root).resolve(), Path(directory).resolve()
    directory.relative_to(root)
    baseline = load_baseline(root)
    after = object_at(after_manifest_path)
    core = legacy()
    names = core.SIGNATURE_REQUIRED + core.SIGNATURE_OPTIONAL
    rows = []
    for name in names:
        before_path = baseline['Archive'] / 'source' / name
        after_path = root / 'source' / name
        key = 'source/' + name
        if digest(before_path) != baseline['Manifest']['Files'][key] or digest(after_path) != after['Files'][key]:
            raise ValueError('A mechanical loader-signature input differs.')
        rows.append({'Name': name, 'BeforePath': str(before_path), 'AfterPath': str(after_path),
                     'BeforeSHA256': digest(before_path), 'AfterSHA256': digest(after_path)})
    paths = {'BeforeRequired': [row['BeforePath'] for row in rows[:len(core.SIGNATURE_REQUIRED)]],
             'BeforeOptional': [row['BeforePath'] for row in rows[len(core.SIGNATURE_REQUIRED):]],
             'AfterRequired': [row['AfterPath'] for row in rows[:len(core.SIGNATURE_REQUIRED)]],
             'AfterOptional': [row['AfterPath'] for row in rows[len(core.SIGNATURE_REQUIRED):]]}
    meta = {'Method': 'ExactUnchangedLoaderFileHashFormula', 'Inputs': rows, **paths}
    (directory / 'loader-signature-inputs.json').write_text(json.dumps(meta, indent=2, sort_keys=True)+'\n')
    script = directory / 'recompute-loader-signature.wl'
    script.write_text(loader_script(paths, directory / 'loader-signature-values.json'))
    return script


def loader_evidence(root, baseline, after, directory):
    root, directory = Path(root).resolve(), Path(directory).resolve()
    directory.relative_to(root)
    meta_path = directory / 'loader-signature-inputs.json'
    values_path = directory / 'loader-signature-values.json'
    script_path = directory / 'recompute-loader-signature.wl'
    meta, values = object_at(meta_path), object_at(values_path)
    core = legacy()
    names = core.SIGNATURE_REQUIRED + core.SIGNATURE_OPTIONAL
    rows = meta.get('Inputs')
    if meta.get('Method') != 'ExactUnchangedLoaderFileHashFormula' or not isinstance(rows, list) or [row.get('Name') for row in rows] != list(names):
        raise ValueError('Exactly the unchanged thirteen loader inputs are required.')
    for name, row in zip(names, rows):
        before_path = baseline['Archive'] / 'source' / name
        after_path = root / 'source' / name
        key = 'source/' + name
        if (row.get('BeforePath') != str(before_path) or row.get('AfterPath') != str(after_path)
                or row.get('BeforeSHA256') != baseline['Manifest']['Files'][key]
                or row.get('AfterSHA256') != after['Files'][key]
                or digest(before_path) != row['BeforeSHA256'] or digest(after_path) != row['AfterSHA256']):
            raise ValueError('Loader inputs are not the pinned literal before/after source bytes.')
    paths = {'BeforeRequired': [row['BeforePath'] for row in rows[:len(core.SIGNATURE_REQUIRED)]],
             'BeforeOptional': [row['BeforePath'] for row in rows[len(core.SIGNATURE_REQUIRED):]],
             'AfterRequired': [row['AfterPath'] for row in rows[:len(core.SIGNATURE_REQUIRED)]],
             'AfterOptional': [row['AfterPath'] for row in rows[len(core.SIGNATURE_REQUIRED):]]}
    if (any(meta.get(key) != value for key, value in paths.items())
            or script_path.read_text() != loader_script(paths, values_path)
            or values.get('PhysicsCalculationsPerformed') is not False
            or values.get('Method') != 'ExactUnchangedLoaderFileHashFormulaVerifiedArchiveOrIdenticalSHAInputs'
            or any(type(values.get(key)) is not int for key in ('Before', 'After'))):
        raise ValueError('The exact recorded mechanical hash calculation is unavailable.')
    return {'Before': values['Before'], 'After': values['After'], 'InputCount': len(names),
            'InputsSHA256': digest(meta_path), 'ValuesSHA256': digest(values_path),
            'ScriptSHA256': digest(script_path), '_VerifiedInputHashes': True}


def analyze(root, after_manifest_path, captures, evidence_directory):
    baseline = load_baseline(root)
    after = object_at(after_manifest_path)
    if not isinstance(captures, (list, tuple)) or len(captures) != 2:
        raise ValueError('Two independent postpatch captures are required.')
    evidence = loader_evidence(root, baseline, after, evidence_directory)
    analysis = compare_transition(baseline['Manifest'], after, baseline['Raw'],
                                  [object_at(path) for path in captures], evidence)
    analysis.update(BaselineDirectory=BASE_DIRECTORY, BaselineSHA256=ANCHORS['baseline.json'],
                    CheckpointSnapshotSHA256=ANCHORS['checkpoint-snapshot.json'],
                    AfterManifestSHA256=digest(after_manifest_path),
                    AfterCaptureSHA256=[digest(path) for path in captures])
    return analysis


def colour_evidence(root, review):
    """Bind reviewed finite-sum arguments and their actual retained certificates.

    Counts are supporting controls. They do not replace the explicit code-side
    finite-sum/incidence proof and review required by reviewed_payload.
    """
    bindings = review.get('ColourArtifacts')
    required = {'FormalExplanation', 'DedupController', 'NetworkController', 'ReviewSummary',
                'FiveGluonSummary', 'FiveGluonWXF', 'OneVectorWXF', 'ProductionTierSummary'}
    if not isinstance(bindings, dict) or set(bindings) != required:
        raise ValueError('Every exact colour proof/control artifact must be bound.')
    paths = {}
    for name, row in bindings.items():
        if not isinstance(row, dict) or set(row) != {'Path', 'SHA256'}:
            raise ValueError('Ambiguous colour artifact binding.')
        paths[name] = inside(root, row['Path'])
        if digest(paths[name]) != row['SHA256']:
            raise ValueError('A reviewed colour proof artifact changed: ' + name)
    dedup, network = [object_at(paths[name]) for name in ('DedupController', 'NetworkController')]
    summary = object_at(paths['ReviewSummary'])
    metric = object_at(paths['FiveGluonSummary'])
    production = object_at(paths['ProductionTierSummary'])
    if any(value.get('Status') != 'PASS' or value.get('ExitCode') != 0 for value in (dedup, network)):
        raise ValueError('The retained 65 colour controls did not pass.')
    if (summary.get('LiteralDedupSmallTests', {}).get('Checks') != 23
            or summary.get('LiteralDedupSmallTests', {}).get('Status') != 'PASS'
            or summary.get('SparseNetworkSmallTests', {}).get('Checks') != 42
            or summary.get('SparseNetworkSmallTests', {}).get('Status') != 'PASS'):
        raise ValueError('Both exact control counts are required.')
    reconstruction = summary.get('SparseUniqueMetric', {})
    if (reconstruction.get('Status') != 'COMPLETE' or reconstruction.get('OriginalOccurrences') != 3960
            or reconstruction.get('UniqueTensors') != 45 or reconstruction.get('Rank') != 6
            or reconstruction.get('ExpandedCoordinateDimensions') != [3960, 6]
            or reconstruction.get('OccurrenceReconstructionVerified') is not True
            or reconstruction.get('UniqueGramReconstructionVerified') is not True
            or reconstruction.get('SourceFilesUnchanged') is not True):
        raise ValueError('The exact 45/rank6/3960 reconstruction certificate is incomplete.')
    one = summary.get('FreshActualFiveGluonVector', {})
    if (one.get('OneVectorComponentwiseSameQ') is not True or one.get('OneVectorComponents') != 32768
            or one.get('OneVectorArtifactSHA256') != bindings['OneVectorWXF']['SHA256']
            or summary.get('ArtifactsSHA256', {}).get('one-vector.wxf') != bindings['OneVectorWXF']['SHA256']
            or summary.get('ArtifactsSHA256', {}).get('five-gluon.wxf') != bindings['FiveGluonWXF']['SHA256']):
        raise ValueError('The exact componentwise reference and tensor certificate bytes are unavailable.')
    if (metric.get('Status') != 'COMPLETE' or metric.get('OriginalOccurrences') != 3960
            or metric.get('UniqueTensors') != 45 or metric.get('OccurrenceReconstructionVerified') is not True):
        raise ValueError('The retained finite tensor calculation is incomplete.')
    # The production report is supplemental; pin all actual suite results without
    # reinterpreting them as a new amplitude construction or validation.
    if (production.get('Status') != 'PASS' or production.get('ExitCode') != 0
            or production.get('SourceAndModelsUnchanged') is not True
            or not production.get('RequestedSuites')
            or production.get('RequestedSuites') != production.get('CompletedSuites')):
        raise ValueError('The reviewed production colour regression tier did not pass.')
    old_colour = summary.get('UnchangedColorSourceSHA256')
    if any(value.get('FilesSHA256', {}).get('source/internal/HEPCAT-Color.wl') != old_colour for value in (dedup, network)) or metric.get('SourceSHA256', {}).get('source/internal/HEPCAT-Color.wl') != old_colour:
        raise ValueError('The controls must bind the exact retained C3 colour evaluator.')
    return {'ArtifactBindingsSHA256': canonical_digest(bindings), 'ControlChecks': 65,
            'BeforeColorSourceSHA256': old_colour,
            'AfterColorSourceSHA256': production.get('SourceAndModelSHA256', {}).get('source/internal/HEPCAT-Color.wl'),
            'ProductionContextSHA256': production.get('DeclaredFrozenContextSHA256'),
            'UniqueTensors': 45, 'Rank': 6, 'OriginalOccurrences': 3960,
            'ExactReferenceComponents': 32768, 'FiniteSumProofRequiresExplicitReview': True,
            'NoAmplitudeConstructionOrValidationClaimed': True}


def code_diffs(root, baseline, after):
    import difflib
    result = {}
    for name in CHANGED_FILES:
        before_path = baseline['Archive'] / name
        after_path = Path(root) / name
        if digest(before_path) != baseline['Manifest']['Files'][name] or digest(after_path) != after['Files'][name]:
            raise ValueError('Code review must use the literal pinned before/after bytes.')
        result[name] = ''.join(difflib.unified_diff(before_path.read_text().splitlines(True),
                         after_path.read_text().splitlines(True), fromfile='C3/'+name, tofile='C4/'+name))
    return result


def reviewed_payload(analysis, review, colour):
    if (analysis.get('Status') != 'ANALYZED_NOT_SEALED'
            or review.get('Status') != 'REVIEWED_CANDIDATE_AND_FINITE_COLOUR_VALIDATION_TRANSITION'
            or review.get('AnalysisSHA256') != canonical_digest(analysis)
            or not isinstance(review.get('ReviewedBy'), str) or not review['ReviewedBy'].strip()
            or not all(review.get(flag) is True for flag in REVIEW_FLAGS)
            or review.get('SourceDiffsSHA256') != analysis.get('SourceDiffsSHA256')
            or colour.get('ControlChecks') != 65 or colour.get('UniqueTensors') != 45
            or colour.get('OriginalOccurrences') != 3960 or colour.get('Rank') != 6):
        raise ValueError('The exact code and finite-colour semantic review is missing.')
    return {**analysis, 'Status': 'SEALED', 'AnalysisSHA256': canonical_digest(analysis),
            'CodeReviewSHA256': canonical_digest(review), 'ColourSemanticEvidence': colour,
            'HistoricalEvidenceOnly': True, 'ValidationRerunPerformed': False,
            'ConstructorRerunPerformed': False, 'CurrentConstructorSuccessClaimed': False,
            'SupplementalCounterexampleChecksStillRequired': True,
            'PrecisionQualificationUnchangedRequired': True}


def seal_payload(root, here, configuration, manifest_path, captures, review_path, evidence_directory):
    root, here = Path(root).resolve(), Path(here).resolve()
    paths = [Path(manifest_path).resolve(), *(Path(path).resolve() for path in captures), Path(review_path).resolve()]
    relative = [str(path.relative_to(root)) for path in paths]
    analysis = analyze(root, paths[0], paths[1:3], evidence_directory)
    baseline = load_baseline(root)
    after = object_at(paths[0])
    actual = legacy().stage_module().manifest(root, here, configuration)
    if actual != after or legacy().stage_module().fingerprint(actual) != analysis['AfterContextSHA256']:
        raise ValueError('The reviewed C4 manifest is not current.')
    analysis['SourceDiffsSHA256'] = canonical_digest(code_diffs(root, baseline, after))
    review = object_at(paths[3])
    colour = colour_evidence(root, review)
    if (colour['BeforeColorSourceSHA256'] != baseline['Manifest']['Files']['source/internal/HEPCAT-Color.wl']
            or colour['AfterColorSourceSHA256'] != after['Files']['source/internal/HEPCAT-Color.wl']
            or colour['ProductionContextSHA256'] != analysis['AfterContextSHA256']):
        raise ValueError('Colour semantic evidence is not bound to this exact C3/C4 change.')
    payload = reviewed_payload(analysis, review, colour)
    payload.update(AfterManifest=relative[0], AfterCaptures=relative[1:3], CodeReview=relative[3],
                   CodeReviewFileSHA256=digest(paths[3]),
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
        paths = [inside(root, proof['AfterManifest']), *(inside(root, path) for path in captures),
                 inside(root, proof['CodeReview'])]
        if digest(paths[3]) != proof['CodeReviewFileSHA256']:
            return None
        expected = seal_payload(root, here, configuration, paths[0], paths[1:3], paths[3],
                                inside(root, proof['LoaderSignatureEvidenceDirectory']))
        if expected != proof:
            return None
        return {**proof, '_C4VerifiedCurrentContext': True, '_VerifiedCurrentContext': True,
                '_ProofSHA256': digest(proof_path), '_Baseline': load_baseline(root)}
    except (OSError, ValueError, KeyError, TypeError, AttributeError):
        return None


def record_evidence(summary_path, summary, current_context, proof):
    try:
        if (not isinstance(proof, dict) or proof.get('_C4VerifiedCurrentContext') is not True
                or proof.get('Status') != 'SEALED' or proof.get('Kind') != KIND
                or proof.get('BeforeContextSHA256') != BASE_CONTEXT
                or proof.get('AfterContextSHA256') != current_context
                or proof.get('HistoricalEvidenceOnly') is not True
                or proof.get('CurrentConstructorSuccessClaimed') is not False):
            return None
        baseline = proof['_Baseline']
        path = Path(summary_path)
        row = baseline['Snapshot'][path.name]
        recorded = summary['ContextSHA256']
        if (digest(path) != row['SummarySHA256'] or summary.get('Status') != row['Status']
                or recorded != row['ContextSHA256']
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
        if recorded != BASE_CONTEXT:
            old = legacy().checkpoint_evidence(path, summary, capsule, baseline['PriorProof'], baseline['PriorBaseline'])
            if old is None or old.get('CurrentContextSHA256') != BASE_CONTEXT:
                return None
            chain.extend(old['CompatibilityChain'])
        chain.append({'Kind': KIND, 'FromContextSHA256': BASE_CONTEXT, 'ToContextSHA256': current_context,
                      'AnalysisSHA256': proof['AnalysisSHA256'],
                      'ColourSemanticEvidenceSHA256': canonical_digest(proof['ColourSemanticEvidence'])})
        return {'Status': 'HISTORICAL_VALIDATION_COMPATIBLE_REVIEWED_C4_CHANGE',
                'RecordedContextSHA256': recorded, 'CurrentContextSHA256': current_context,
                'Stage': 'HistoricalExportReport' if '-export.' in path.name else 'HistoricalNativeValidation',
                'CompatibilityChain': chain, 'OriginalTimingAndOptionsPreserved': True,
                'ValidationRerunPerformed': False, 'ConstructorRerunPerformed': False,
                'CurrentConstructorSuccessClaimed': False,
                'SupplementalCounterexampleChecksStillRequired': True,
                'PrecisionQualificationUnchangedRequired': True}
    except (OSError, ValueError, KeyError, TypeError, AttributeError):
        return None


def public_proof(proof, stage=None):
    if proof.get('_C4VerifiedCurrentContext') is not True:
        return None
    return {'Status': 'EXACT_CANDIDATE_AND_FINITE_COLOUR_HISTORICAL_VALIDATION_COMPATIBILITY',
            'Stage': stage, 'BeforeContextSHA256': BASE_CONTEXT,
            'AfterContextSHA256': proof['AfterContextSHA256'], 'AnalysisSHA256': proof['AnalysisSHA256'],
            'CodeReviewFileSHA256': proof['CodeReviewFileSHA256'], 'ProofSHA256': proof['_ProofSHA256'],
            'ColourSemanticEvidence': proof['ColourSemanticEvidence'], 'HistoricalEvidenceOnly': True,
            'OriginalTimingAndOptionsPreserved': True, 'ValidationRerunPerformed': False,
            'ConstructorRerunPerformed': False, 'CurrentConstructorSuccessClaimed': False,
            'SupplementalCounterexampleChecksStillRequired': True, 'PrecisionQualificationUnchangedRequired': True}
