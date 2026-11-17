"""Exact versioned transition analysis only. No admission or record dispatcher.

Legacy constructor_receipts.py is neither imported nor modified. The caller
supplies its separately pinned policy; this module never seals or registers it.
"""
from pathlib import Path
import hashlib
import json
import re

SCHEMA = 2
KIND = 'ExactC13CombinedExtensionsTransitionAnalysis'
P = 'ConstructiveDiagrams`Private`'
FIELDS = ('DownValues', 'UpValues', 'SubValues', 'NValues', 'OwnValues',
          'DefaultValues', 'FormatValues', 'Options', 'Attributes', 'Messages')
PATTERNS = ('ConstructiveDiagrams`*', 'ConstructiveDiagrams`Private`*',
    'HEPCATCovariantCurrentSources`*', 'HEPCATCovariantCurrentSources`Private`*',
    'HEPCATCovariantContactCalibration`*', 'HEPCATCovariantContactCalibration`Private`*',
    'HEPCATMinimalAdjointCubicSources`*', 'HEPCATMinimalAdjointCubicSources`Private`*',
    'HEPCATMinimalQCDCurrentSources`*', 'HEPCATMinimalQCDCurrentSources`Private`*',
    'HEPCATMasslessVectorCalibration`*', 'HEPCATMasslessVectorCalibration`Private`*',
    'HEPCATMasslessVectorIntegration`*', 'HEPCATMasslessVectorIntegration`Private`*')
CHANGED_SOURCE = frozenset(('source/HEPCAT.wl', 'source/HEPCAT-Model.wl',
    'source/internal/HEPCAT-CovariantContacts.wl', 'source/internal/HEPCAT-QCDCurrents.wl'))
ADDED_SOURCE = ('source/internal/HEPCAT-MixedNullCalibration.wl',
    'source/internal/HEPCAT-MixedNullIntegration.wl', 'source/internal/HEPCAT-MixedNullPublic.wl')
SIGNATURE = P + 'amplitudeParallelSignature'
LOADER = P + '$amplitudeParallelLoadedSignature'
REVIEWED_FIELDS = {'ConstructiveDiagrams`ConstructAmplitude': ['DownValues'],
    **{P+n: ['DownValues'] for n in ('constructCovariantSourceMetadata',
       'constructAllChecksPassQ', 'colorQCDCurrentSourceClass',
       'colorQCDCurrentPrepare', 'amplitudeParallelSignature')}, LOADER: ['OwnValues']}
PATH_CACHES = tuple(P+n for n in ('$amplitudeParallelSourceDirectory',
    '$spinasExportSourceDirectory', '$spinasExportRuntimeDirectory'))
BEFORE_REQUIRED = ('HEPCAT.wl', 'HEPCAT-Base.wl', 'internal/HEPCAT-Parameters.wl',
    'HEPCAT-Model.wl', 'internal/HEPCAT-Feynman.wl', 'internal/HEPCAT-Color.wl',
    'internal/HEPCAT-SPINAS.wl', 'internal/HEPCAT-MadGraph.wl',
    'internal/HEPCAT-CovariantSources.wl', 'internal/HEPCAT-CovariantContacts.wl',
    'internal/HEPCAT-ColoredCurrents.wl', 'internal/HEPCAT-QCDCurrents.wl',
    'internal/HEPCAT-SPINAS-Optimize.wl', 'internal/export/spinas-runtime.hpp')
AFTER_REQUIRED = BEFORE_REQUIRED[:12] + tuple(n.removeprefix('source/') for n in ADDED_SOURCE) + BEFORE_REQUIRED[12:]
OPTIONAL = ('internal/proofs/laurent-proof-runtime.cpp',
    'internal/proofs/laurent-proof-runtime.py', 'internal/proofs/laurent-proof-factor.py',
    'internal/proofs/laurent-proof-verify.py')
USAGE = 'Documentation/English/SymbolUsage.wl'


def require(value, message):
    if not value:
        raise ValueError(message)


def digest(path):
    value = hashlib.sha256()
    with Path(path).open('rb') as stream:
        for block in iter(lambda: stream.read(1024**2), b''):
            value.update(block)
    return value.hexdigest()


def canonical(value):
    return hashlib.sha256(json.dumps(value, sort_keys=True).encode()).hexdigest()


def object_at(binding):
    require(isinstance(binding, dict) and set(binding) == {'Path', 'SHA256'},
            'An exact artifact path/SHA256 binding is required.')
    require(digest(binding['Path']) == binding['SHA256'], 'Bound artifact changed.')
    value = json.loads(Path(binding['Path']).read_text())
    require(isinstance(value, dict), 'Bound artifact is not an object.')
    require(digest(binding['Path']) == binding['SHA256'], 'Artifact changed during readback.')
    return value


def loader_value(text):
    match = re.fullmatch(r'\{HoldPattern\[ConstructiveDiagrams`Private`\$amplitudeParallelLoadedSignature\] :> ([0-9]+)\}', text)
    require(match is not None, 'Loader OwnValues is not the exact single integer.')
    return int(match[1])


def verify_loader(bundle, policy, before_manifest, after_manifest, old, new):
    """Verify the pinned actual two-run bare recomputation, without executing it."""
    require(isinstance(bundle, dict) and set(bundle) == {'Controller', 'Before', 'After', 'Script'} and
            bundle == policy['LoaderArtifacts'], 'Exact actual loader artifact bundle required.')
    controller = object_at(bundle['Controller'])
    require(controller.get('Status') == 'BARE_LOADER_INPUTS_VERIFIED_NOT_SEALED' and
            len(controller.get('Records', [])) == 2, 'Bare loader controller is incomplete.')
    script_binding = bundle['Script']
    require(set(script_binding) == {'Path', 'SHA256'} and
            digest(script_binding['Path']) == script_binding['SHA256'] and
            Path(script_binding['Path']).read_text() == policy['LoaderScriptText'],
            'Literal independent bare loader script changed.')
    require(controller['Inputs'].get(script_binding['Path']) == script_binding['SHA256'],
            'Bare collector script was not an actual pinned input.')
    require(all(digest(path) == pin for path, pin in controller['Inputs'].items()),
            'An actual bare collector input changed.')
    results = {}
    for index, (side, names, manifest, fields) in enumerate((
            ('Before', BEFORE_REQUIRED, before_manifest, old),
            ('After', AFTER_REQUIRED, after_manifest, new))):
        row = object_at(bundle[side])
        record = controller['Records'][index]
        require(record['RawFile'] == bundle[side]['Path'] and
                record['RawSHA256'] == bundle[side]['SHA256'] and record['Pass'] == index+1 and
                record['Status'] == 'CAPTURED' and record['KernelExitCode'] == 0 and
                record['Bound'] is None and record['PinsUnchanged'] is True,
                'Bare output is not the exact successful owned run.')
        cleanup = record['OwnedCleanup']
        require(cleanup['Status'] == 'COMPLETE' and cleanup['AllObservedOwnedGroupsGone'] is True and
                cleanup['AllOwnedPopenLeadersReaped'] is True and cleanup['CleanupDeadlineSatisfied'] is True and
                cleanup['InspectionFailed'] is False, 'Bare collector ownership/cleanup is unresolved.')
        require(row['Status'] == 'INDEPENDENT_BARE_LOADER_RECOMPUTATION' and
                row['RequiredNames'] == list(names) and row['OptionalNames'] == list(OPTIONAL) and
                row['InputCount'] == len(names)+len(OPTIONAL),
                'Bare loader input names, count or order differ.')
        require(row['SourceDirectory'] == policy['LoaderSourceDirectories'][side] and
                row['WolframVersion'] == policy['WolframVersion'] and row['SystemID'] == policy['SystemID'] and
                row['NoModelOrPhysics'] is True, 'Bare loader source/platform/independence differs.')
        expected = {name: manifest['Files']['source/'+name] for name in (*names, *OPTIONAL)}
        require(row['SourceSHA256'] == expected, 'Bare loader source digest inventory differs.')
        paths = [Path(row['SourceDirectory'])/name for name in (*names, *OPTIONAL)]
        require(all(digest(path) == expected[name] for path, name in zip(paths, (*names, *OPTIONAL))),
                'Bare loader source bytes changed.')
        # These actual platform captures use the existing FileHash default
        # 128-bit byte digests. Check every recorded integer against the bytes;
        # do not replace or normalize Wolfram's enclosing Hash expression.
        text = row['BareInputsInputForm']
        require(re.fullmatch(r'\{[0-9]+(?:, [0-9]+)*\}', text) is not None,
                'Bare input integer list is malformed or contains missing inputs.')
        actual_inputs = [int(hashlib.md5(path.read_bytes()).hexdigest(), 16) for path in paths]
        require([int(value) for value in re.findall(r'[0-9]+', text)] == actual_inputs,
                'Bare input byte-digest sequence differs.')
        require(row['ActualFunctionDownValues'] == fields[SIGNATURE]['DownValues'] and
                type(row['Bare']) is int and type(row['Loaded']) is int and
                row['BareMatchesLoaded'] is True and row['BareMatchesFunction'] is True and
                row['Bare'] == row['Loaded'] == loader_value(fields[LOADER]['OwnValues']),
                'Bare recomputation/function/actual loaded OwnValue disagree.')
        results[side] = row['Bare']
    return {**results, 'InputCountBefore':18, 'InputCountAfter':21,
            'ControllerSHA256':bundle['Controller']['SHA256'],
            'BeforeRawSHA256':bundle['Before']['SHA256'], 'AfterRawSHA256':bundle['After']['SHA256'],
            'ScriptSHA256':bundle['Script']['SHA256']}


def inspect_delta(core, policy, expected_policy_sha, before, after, before_pair,
                  after_pair, new_inventory, loading_inputs, *, private_paths=None,
                  installed_root_captures=False):
    """Exact source/definition review, explicitly incomplete without loader proof."""
    require(canonical(policy) == expected_policy_sha, 'Unreviewed policy bytes/data.')
    require(policy.get('SchemaVersion') == SCHEMA and policy.get('Kind') == KIND,
            'Wrong versioned policy kind.')
    require(policy['ReviewedExistingFields'] == REVIEWED_FIELDS and
            policy['ChangedSourcePaths'] == sorted(CHANGED_SOURCE) and
            policy['AddedSourcePaths'] == list(ADDED_SOURCE) and
            policy['NamespacePatterns'] == list(PATTERNS), 'Policy mask differs from this exact version.')
    require(before.get('Programs') == after.get('Programs') == policy['Programs'],
            'Numerical programs changed.')
    left, right = before['Files'], after['Files']
    require(not set(left)-set(right) and set(right)-set(left) == set(ADDED_SOURCE),
            'Unexpected removed, missing or extra source/model/runtime file.')
    require({n for n in left if left[n] != right[n]} == CHANGED_SOURCE,
            'Changed existing source/model/runtime set differs.')
    require(before == policy['BeforeManifest'] and after == policy['AfterManifest'],
            'Manifest source/model/runtime bytes differ from pinned policy.')
    require(len([n for n in left if n.startswith('source/')]) == 25 and
            len([n for n in right if n.startswith('source/')]) == 28,
            'Exact before25/after28 source inventory required.')
    require(set(loading_inputs) == {'Before', 'After'}, 'Both actual loading input sets required.')
    for side in ('Before', 'After'):
        require(set(loading_inputs[side]) == {USAGE}, 'Usage-registry loading input missing or extra.')
        binding = loading_inputs[side][USAGE]
        require(set(binding) == {'Path','SHA256'} and
                binding == policy['LoadingInputs'][side][USAGE] and
                digest(binding['Path']) == binding['SHA256'], 'Actual literal usage registry differs.')
    require(loading_inputs['Before'][USAGE]['SHA256'] == loading_inputs['After'][USAGE]['SHA256'],
            'Messages/usage loading input changed.')
    require(len(before_pair) == len(after_pair) == 2, 'Exactly two cold captures per side required.')
    for row in (*before_pair, *after_pair):
        require(row.get('CapturedNamePatterns') == list(PATTERNS), 'Exact ordered fourteen namespaces required.')
        require(row.get('WolframVersion') == policy['WolframVersion'] and
                row.get('SystemID') == policy['SystemID'], 'Cold platform/version differs from pinned policy.')
    old, new = core.cold_pair(*before_pair, require_post=False), core.cold_pair(*after_pair, require_post=False)
    require(all(before_pair[0].get(k) == after_pair[0].get(k) for k in
                ('WolframVersion','SystemID','CapturedNamePatterns')), 'Cold platform/version/patterns changed.')
    require(canonical(old) == policy['BeforeDefinitionsSHA256'], 'Before definition preimage differs.')
    require(not set(old)-set(new), 'Existing symbol removed.')
    changes, artifacts = {}, {}
    for name, fields in old.items():
        differing = [key for key in FIELDS if fields[key] != new[name][key]]
        if name in PATH_CACHES and differing:
            require(private_paths is not None and installed_root_captures is False and
                    tuple(private_paths) == tuple(policy['AnalysisOnlySourceDirectories']) and
                    differing == ['OwnValues'] and
                    fields['OwnValues'].replace(private_paths[0], private_paths[1]) == new[name]['OwnValues'],
                    'Private path difference is not an exact analysis-only artifact.')
            artifacts[name] = differing
            continue
        if differing:
            require(differing == REVIEWED_FIELDS.get(name), 'Unreviewed existing symbol/field change: '+name)
            changes[name] = differing
    require(changes == REVIEWED_FIELDS, 'Missing or extra reviewed definition field.')
    require(set(policy['ReviewedFieldTables']) == set(REVIEWED_FIELDS), 'Policy field table inventory differs.')
    for name in REVIEWED_FIELDS:
        require(policy['ReviewedFieldTables'][name] == {'Before':old[name], 'After':new[name]},
                'Reviewed field preimage/postimage differs: '+name)
    # Verify the signature implementation differs only by the three literal
    # required-module names; no optional or hash formula relaxation is allowed.
    quote = lambda value: '"'+value+'"'
    before_list, after_list = (', '.join(quote(n) for n in names) for names in (BEFORE_REQUIRED, AFTER_REQUIRED))
    require(old[SIGNATURE]['DownValues'].count(before_list) == 1 and
            old[SIGNATURE]['DownValues'].replace(before_list, after_list, 1) == new[SIGNATURE]['DownValues'],
            'Loader formula changed beyond the exact required input expansion.')
    added = set(new)-set(old)
    require(isinstance(new_inventory, dict) and set(new_inventory) == added and len(added) == 1534,
            'Exact full1534 new-symbol inventory required.')
    for name, row in new_inventory.items():
        private_contexts = tuple(pattern[:-1] for pattern in PATTERNS if pattern.endswith('`Private`*'))
        require(name.startswith(private_contexts) and set(row) == {'Fields','FieldsSHA256'} and
                row['Fields'] == new[name] and row['FieldsSHA256'] == canonical(new[name]),
                'New symbol has unreviewed names or full ten-field table: '+name)
    require(canonical(new_inventory) == policy['NewSymbolInventorySHA256'], 'New symbol inventory bytes/data changed.')
    require(canonical(new) == policy['AfterDefinitionsSHA256'], 'After definition table differs from exact policy.')
    if private_paths is not None:
        require(set(artifacts) == set(PATH_CACHES), 'Exactly three analysis-only path artifacts required.')
    if installed_root_captures:
        require(private_paths is None and not artifacts and
                policy['CaptureSourceDirectories'][0] == policy['CaptureSourceDirectories'][1],
                'Actual installed-root captures required; private analysis cannot authorize admission.')
    return {'Status':'EXACT_DELTA_INSPECTED_LOADER_PROOF_STILL_REQUIRED',
        'ChangedExistingFields':changes, 'PrivatePathArtifacts':artifacts,
        'BeforeSymbols':len(old), 'AfterSymbols':len(new), 'NewSymbols':len(added),
        'BeforeDefinitionsSHA256':canonical(old), 'AfterDefinitionsSHA256':canonical(new),
        'ActualInstalledRootCaptures':installed_root_captures is True and not artifacts,
        'NoFieldOrMessageNormalization':True, 'NoAdmission':True}, old, new


def analyze_transition(core, policy, expected_policy_sha, before, after, before_pair,
                       after_pair, new_inventory, loading_inputs, loader_bundle,
                       *, private_paths=None, installed_root_captures=False):
    analysis, old, new = inspect_delta(core, policy, expected_policy_sha, before, after,
        before_pair, after_pair, new_inventory, loading_inputs, private_paths=private_paths,
        installed_root_captures=installed_root_captures)
    loader = verify_loader(loader_bundle, policy, before, after, old, new)
    return {**analysis, 'Status':'ANALYZED_NOT_SEALED', 'Loader':loader,
            'PolicySHA256':expected_policy_sha, 'NewSymbolInventorySHA256':canonical(new_inventory),
            'CurrentConstructorSuccessClaimed':False, 'ConstructorEquivalenceClaimed':False,
            'CurrentValidatorPassClaimed':False, 'ValidationRerunPerformed':False,
            'ReceiptSealed':False, 'RegistryActivationPerformed':False}
