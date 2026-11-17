"""Exact data-policy transition and finite historical evidence reader, schema 3.

No writes, migrations, model/physics work, sealing or implicit trust anchors.
Earlier readers are literal dependencies. Their record/probe checks stay lazy.
"""
from pathlib import Path
import hashlib
import importlib.util
import json
import re

SCHEMA = 3
KIND = 'ExactPolicyFiniteHistoricalRetention'
REGISTRY_FILENAME = 'constructor-receipts-v3.json'
LEGACY_SHA = '91649106592da6a4bea9999e7eb4f4814197789fc78efa9f449bcc3860644d11'
V2_SHA = '949134bf48ed7dea2b059c0012eb5f8b0d082780af54e03732b3ad3cd78ef3d8'
ANALYZER_V2_SHA = '77c86e9bf3b27ec9f205dc81f789c7abc6fc47883c184b9711e4d9784da02b99'
FIXTURE_SHA = '3cb89713c3fe9716d86ddd1c34d4b598f7dad90e5fb8500bc96e0bbb2542a0f9'
P = 'ConstructiveDiagrams`Private`'
LOADER = P+'$amplitudeParallelLoadedSignature'
SIGNATURE = P+'amplitudeParallelSignature'
FIELDS = ('DownValues','UpValues','SubValues','NValues','OwnValues','DefaultValues',
          'FormatValues','Options','Attributes','Messages')
USAGE = 'Documentation/English/SymbolUsage.wl'
PATHS = tuple(P+n for n in ('$amplitudeParallelSourceDirectory',
    '$spinasExportSourceDirectory','$spinasExportRuntimeDirectory'))
POLICY_KEYS = {'SchemaVersion','Kind','BeforeManifest','AfterManifest','BeforeContextSHA256',
    'AfterContextSHA256','ChangedSourcePaths','AddedSourcePaths','NamespacePatterns',
    'WolframVersion','SystemID','BeforeDefinitionsSHA256','AfterDefinitionsSHA256',
    'ReviewedExistingFields','ReviewedFieldTables','MechanicalSourceInventories','NewSymbolInventorySHA256',
    'LoadingInputs','CaptureSourceDirectories','Loader','Snapshot','VerifierTools'}
SNAPSHOT_KEYS = {'Counts','ProducerRecordCounts','QualifiedFamilyIDs','PriorRecordCount',
    'PriorReceipt','PriorSnapshot','PriorSchemaVersion'}


def require(value,message):
    if not value:
        raise ValueError(message)


def digest(path):
    value=hashlib.sha256()
    with Path(path).open('rb') as stream:
        for block in iter(lambda:stream.read(1048576),b''):
            value.update(block)
    return value.hexdigest()


def canonical(value):
    return hashlib.sha256(json.dumps(value,sort_keys=True).encode()).hexdigest()


def pin(value):
    return isinstance(value,str) and re.fullmatch('[0-9a-f]{64}',value) is not None


def natural(value):
    return type(value) is int and value >= 0


def module(name,path,expected):
    require(digest(path)==expected,'The literal verifier preimage changed: '+str(path))
    spec=importlib.util.spec_from_file_location(name,path)
    loaded=importlib.util.module_from_spec(spec)
    spec.loader.exec_module(loaded)
    return loaded


def primitive(here):
    return module('unchanged_schema1_finite_primitives',Path(here)/'constructor_receipts.py',LEGACY_SHA)


def version2(here):
    return module('unchanged_schema2_finite_reader',Path(here)/'constructor_receipts_v2.py',V2_SHA)


def artifact(binding):
    require(isinstance(binding,dict) and set(binding)=={'Path','SHA256'} and
        isinstance(binding['Path'],str) and pin(binding['SHA256']),'Exact path/SHA256 binding required.')
    path=Path(binding['Path'])
    require('..' not in path.parts and digest(path)==binding['SHA256'],'Bound artifact changed.')
    return path


def object_binding(binding):
    path=artifact(binding);value=json.loads(path.read_text())
    require(isinstance(value,dict) and digest(path)==binding['SHA256'],'Bound object changed or malformed.')
    return value


def validate_policy(policy,expected):
    require(isinstance(policy,dict) and canonical(policy)==expected and set(policy)==POLICY_KEYS,
        'Unreviewed or malformed exact data policy.')
    require(type(policy['SchemaVersion']) is int and policy['SchemaVersion']==SCHEMA and policy['Kind']==KIND,
        'Wrong versioned policy kind.')
    before,after=policy['BeforeManifest'],policy['AfterManifest']
    require(isinstance(before,dict) and isinstance(after,dict) and
        set(before)==set(after)=={'Files','Programs'} and before['Programs']==after['Programs'],
        'Manifest shape or numerical programs changed.')
    left,right=before['Files'],after['Files']
    require(isinstance(left,dict) and isinstance(right,dict) and all(pin(v) for v in (*left.values(),*right.values())),
        'Manifest digests are malformed.')
    changed,added=policy['ChangedSourcePaths'],policy['AddedSourcePaths']
    require(isinstance(changed,list) and isinstance(added,list) and changed==sorted(set(changed)) and
        added==sorted(set(added)) and all(isinstance(n,str) and n.startswith('source/') and
        n.endswith('.wl') and '..' not in Path(n).parts for n in changed+added),
        'Only an exact source-Wolfram file delta is eligible.')
    require(not set(left)-set(right) and set(right)-set(left)==set(added) and
        {n for n in left if left[n]!=right[n]}==set(changed),'Source/model/runtime delta differs from policy.')
    require(pin(policy['BeforeContextSHA256']) and pin(policy['AfterContextSHA256']) and
        policy['BeforeContextSHA256']!=policy['AfterContextSHA256'],'Malformed or unchanged transition contexts.')
    patterns=policy['NamespacePatterns']
    require(isinstance(patterns,list) and patterns and len(patterns)==len(set(patterns)) and
        all(isinstance(p,str) and p.endswith('`*') for p in patterns),'Exact ordered namespaces required.')
    mask=policy['ReviewedExistingFields'];mechanical=policy['MechanicalSourceInventories']
    require(isinstance(mechanical,dict) and len(mechanical)<=1,
        'At most one explicitly reviewed mechanical source inventory is eligible.')
    for name,spec in mechanical.items():
        require('`Private`' in name and name!=LOADER and isinstance(spec,dict) and
            set(spec)=={'OrderedPaths','AdditionalSourceFiles'},'Malformed mechanical inventory descriptor.')
        paths,extra=spec['OrderedPaths'],spec['AdditionalSourceFiles']
        require(isinstance(paths,list) and len(paths)==len(set(paths)) and isinstance(extra,dict) and
            all(isinstance(p,str) and p.startswith('source/') and '..' not in Path(p).parts for p in paths) and
            all(pin(v) for v in extra.values()) and not set(extra)&(set(left)|set(right)) and
            set(paths)=={p for p in left if p.startswith('source/')}|set(extra)==
                {p for p in right if p.startswith('source/')}|set(extra),
            'Mechanical inventory needs every source file, including explicit unchanged nonmanifest files.')
    require(isinstance(mask,dict) and mask and mask.get(LOADER)==['OwnValues'] and
        all((n in {LOADER,*mechanical} and fs==['OwnValues']) or
            ('`Private`' in n and fs==['DownValues'] and n!=SIGNATURE) for n,fs in mask.items()) and
        all(mask.get(n)==['OwnValues'] for n in mechanical),
        'Only reviewed private DownValues, loader and exact source-inventory OwnValues may change.')
    require(set(policy['ReviewedFieldTables'])==set(mask),'Exact reviewed field tables required.')
    for tables in policy['ReviewedFieldTables'].values():
        require(isinstance(tables,dict) and set(tables)=={'Before','After'} and
            all(isinstance(row,dict) and set(row)==set(FIELDS) and
                all(isinstance(v,str) for v in row.values()) for row in tables.values()),
            'Incomplete ten-field preimage/postimage.')
    require(set(policy['LoadingInputs'])=={'Before','After'} and
        all(set(v)=={USAGE} for v in policy['LoadingInputs'].values()) and
        policy['LoadingInputs']['Before'][USAGE]['SHA256']==policy['LoadingInputs']['After'][USAGE]['SHA256'],
        'Literal unchanged Usage registry is required.')
    require(isinstance(policy['CaptureSourceDirectories'],list) and len(policy['CaptureSourceDirectories'])==2,
        'Both capture source roots required.')
    s=policy['Snapshot']
    require(isinstance(s,dict) and set(s)==SNAPSHOT_KEYS and
        set(s['Counts'])=={'Native','Export','Total','ActualProducers'} and
        all(natural(v) for v in s['Counts'].values()) and s['Counts']['Total']>0 and
        s['Counts']['Native']+s['Counts']['Export']==s['Counts']['Total'] and
        natural(s['PriorRecordCount']) and s['PriorRecordCount']<=s['Counts']['Total'] and
        type(s['PriorSchemaVersion']) is int and s['PriorSchemaVersion'] in (1,2,3),
        'Finite snapshot counts or prior schema are malformed.')
    require(isinstance(s['ProducerRecordCounts'],dict) and all(pin(k) and natural(v) and v>0
        for k,v in s['ProducerRecordCounts'].items()) and
        sum(s['ProducerRecordCounts'].values())==s['Counts']['Total'] and
        len(s['ProducerRecordCounts'])==s['Counts']['ActualProducers'] and
        isinstance(s['QualifiedFamilyIDs'],list) and s['QualifiedFamilyIDs']==sorted(set(s['QualifiedFamilyIDs'])),
        'Exact finite producer/qualification inventory required.')
    require(isinstance(policy['VerifierTools'],dict) and policy['VerifierTools'] and
        all(isinstance(n,str) and not Path(n).is_absolute() and '..' not in Path(n).parts and pin(v)
        for n,v in policy['VerifierTools'].items()),'Exact verifier byte inventory required.')


def inspect_transition(core,policy,expected_policy_sha,before,after,before_pair,after_pair,new_inventory,
                       *,installed_root_captures=False,analysis_paths=None):
    """Exact source/ten-field delta only. A result is never an admission proof."""
    validate_policy(policy,expected_policy_sha)
    require(before==policy['BeforeManifest'] and after==policy['AfterManifest'],'Pinned manifests differ.')
    require(isinstance(before_pair,list) and isinstance(after_pair,list) and len(before_pair)==len(after_pair)==2,
        'Exactly two independent cold captures per side required.')
    for row in before_pair+after_pair:
        require(row.get('CapturedNamePatterns')==policy['NamespacePatterns'] and
            row.get('WolframVersion')==policy['WolframVersion'] and row.get('SystemID')==policy['SystemID'],
            'Cold namespace/platform/version differs from policy.')
    for side in ('Before','After'):
        artifact(policy['LoadingInputs'][side][USAGE])
    old,new=core.cold_pair(*before_pair,require_post=False),core.cold_pair(*after_pair,require_post=False)
    require(canonical(old)==policy['BeforeDefinitionsSHA256'] and
        canonical(new)==policy['AfterDefinitionsSHA256'],'Full definition table differs from policy.')
    require(not set(old)-set(new),'Existing symbol was removed.')
    changes,paths={},{}
    for name,fields in old.items():
        delta=[f for f in FIELDS if fields[f]!=new[name][f]]
        if delta and name in PATHS and analysis_paths is not None:
            require(installed_root_captures is False and len(analysis_paths)==2 and delta==['OwnValues'] and
                fields['OwnValues'].replace(analysis_paths[0],analysis_paths[1])==new[name]['OwnValues'],
                'Path artifacts are exact analysis-only substitutions.')
            paths[name]=delta
        elif delta:
            require(delta==policy['ReviewedExistingFields'].get(name),'Unreviewed existing field: '+name)
            changes[name]=delta
    require(changes==policy['ReviewedExistingFields'],'Missing or extra reviewed existing field.')
    require(all(policy['ReviewedFieldTables'][n]=={'Before':old[n],'After':new[n]} for n in changes),
        'Reviewed field preimage/postimage differs.')
    for name,spec in policy['MechanicalSourceInventories'].items():
        source_paths,extra=spec['OrderedPaths'],spec['AdditionalSourceFiles']
        for index,(table,manifest) in enumerate(((old,before),(new,after))):
            source=Path(policy['CaptureSourceDirectories'][index])
            entries=', '.join(json.dumps(str(source.parent/p))+' -> '+json.dumps(manifest['Files'].get(p,extra.get(p)))
                for p in source_paths)
            expected='{HoldPattern['+name+'] :> <|'+entries+'|>}'
            require(table[name]['OwnValues']==expected,
                'Mechanical OwnValue is not the exact full source digest inventory: '+name)
    added=set(new)-set(old)
    private=tuple(p[:-1] for p in policy['NamespacePatterns'] if p.endswith('`Private`*'))
    require(isinstance(new_inventory,dict) and set(new_inventory)==added and
        canonical(new_inventory)==policy['NewSymbolInventorySHA256'],'New symbol inventory differs.')
    for name,row in new_inventory.items():
        require(name.startswith(private) and set(row)=={'Fields','FieldsSHA256'} and
            row['Fields']==new[name] and row['FieldsSHA256']==canonical(new[name]),
            'Unreviewed new symbol/full ten-field table: '+name)
    if analysis_paths is not None:
        require(set(paths)==set(PATHS),'Exactly three analysis path artifacts required.')
    if installed_root_captures:
        require(analysis_paths is None and not paths and
            policy['CaptureSourceDirectories'][0]==policy['CaptureSourceDirectories'][1],
            'Actual same-root captures required for admission.')
    return {'Status':'EXACT_POLICY_DELTA_INSPECTED_NOT_SEALED','PolicySHA256':expected_policy_sha,
        'ChangedExistingFields':changes,'PrivatePathArtifacts':paths,'BeforeSymbols':len(old),
        'AfterSymbols':len(new),'NewSymbols':len(added),'BeforeDefinitionsSHA256':canonical(old),
        'AfterDefinitionsSHA256':canonical(new),'ActualInstalledRootCaptures':installed_root_captures is True,
        'NoFieldOrMessageNormalization':True,'NoAdmission':True},old,new


def exact_snapshot(old,root,snapshot,prior_snapshot,policy):
    """Validate the finite index only. Capsule/PQ/probe integrity stays per record."""
    spec=policy['Snapshot'];counts=spec['Counts'];before=policy['BeforeContextSHA256']
    require(type(snapshot.get('SchemaVersion')) is int and snapshot['SchemaVersion']==SCHEMA and
        snapshot.get('Status')=='UNSEALED_VERIFIED_HISTORICAL_SNAPSHOT' and
        snapshot.get('AdmissionContextSHA256')==before and
        all(type(snapshot.get(k)) is int and snapshot[k]==counts[v] for k,v in
            [('NativeCaseCount','Native'),('ExportCompanionCount','Export'),('ArtifactCount','Total'),
             ('ActualProducerContextCount','ActualProducers')]),'Wrong finite checkpoint or counts.')
    require(all(snapshot.get(k) is True for k in ('ActualProducerContextsPreserved','PriorConditionsPreserved',
        'PerNativePriorProbeEntriesBound','CurrentNewProbeFailuresRemainAuthoritative',
        'CaseAndExportCacheChecksAreLazy','PrecisionCacheChecksArePerExport','PrecisionVerifiersRemainAuthoritative',
        'Prior'+str(spec['PriorRecordCount'])+'RawRowsPreservedLiterally')) and
        all(snapshot.get(k) is False for k in ('NewValidationPassClaimed','CanonicalWritesPerformed','CapsulesCopied',
            'UnrelatedNewCaseRegistryAdditionsInvalidateHistoricalCases')),
        'Snapshot changes evidence scope or claims a rerun/status upgrade.')
    require(not any(snapshot.get(k) for k in ('AuxiliaryBindings','ExternalAuxiliaryBindings','SharedProofBindings')),
        'Family caches cannot become global dependencies.')
    require(snapshot.get('PriorReceipt')==spec['PriorReceipt'] and
        snapshot.get('PriorRecordSnapshot')==spec['PriorSnapshot'],'Historical trust anchors differ.')
    rows=snapshot.get('Records');require(isinstance(rows,list) and len(rows)==counts['Total'],'Exact finite records required.')
    index,by_id,producers={},{},{}
    for row in rows:
        kind,identifier,raw=row['Kind'],row['ID'],row['RawSummary']
        require(kind in ('Native','Export') and isinstance(identifier,str) and '/' not in identifier and
            (kind,identifier) not in by_id and isinstance(raw,dict),'Duplicate/malformed finite identity.')
        expected='tests/results/SM-5-point/'+identifier+('-export' if kind=='Export' else '')+'.json'
        path=old.inside(root,expected)
        require(row['SummaryPath']==expected and row['CapsulePath']==expected.removesuffix('.json')+'.wxf' and
            row['AdmittedAtContextSHA256']==before and raw.get('ID' if kind=='Native' else 'FamilyID')==identifier and
            row['ActualProducerContextSHA256']==raw.get('ContextSHA256') and
            row['RecordedStatus']==raw.get('Status') and row['CapsuleSHA256']==raw.get('CapsuleSHA256') and
            pin(row['SummarySHA256']) and pin(row['CapsuleSHA256']) and pin(raw.get('ContextSHA256')),
            'Finite path, producer, status or raw capsule identity differs.')
        producer=raw['ContextSHA256'];history=row.get('PriorHistoricalAdmission')
        require((producer==before and history is None) or
            (producer!=before and isinstance(history,dict) and history.get('HistoricalEvidenceOnly') is True and
             history.get('RecordedStatus')==raw['Status'] and history.get('ActualProducerContextSHA256')==producer and
             history.get('CurrentContextSHA256')==before and all(history.get(k) is False for k in old.FALSE_CLAIMS)),
            'Actual producer or original finite conditions were lost/upgraded.')
        if kind=='Native':
            entries=row.get('PriorSupplementalEntries')
            require(raw['Status']=='COMPLETE' and isinstance(entries,list) and
                len({e['ID'] for e in entries})==len(entries) and all(e['CaseID']==identifier and
                e['SourceCapsuleSHA256']==row['CapsuleSHA256'] for e in entries) and
                row.get('ExistingPrecisionQualification') is None,'Native/probe inventory is incomplete.')
        else:
            require(raw['Status'] in ('PASS','FAIL') and
                ((raw['Status']=='FAIL' and isinstance(row.get('ExistingPrecisionQualification'),dict) and
                  row.get('PrecisionBindings') and isinstance(row.get('PrecisionExternalBindings'),list)) or
                 (raw['Status']=='PASS' and row.get('ExistingPrecisionQualification') is None)),
                'Raw export FAIL upgraded or existing precision dependencies lost.')
            require(row['NativeCaseInputBindings']=={k:raw.get(k) for k in
                ('CaseIDs','CaseCapsulesSHA256','NativeCaseContexts')},'Export native bindings differ.')
        by_id[(kind,identifier)]=row;index[path]=row;producers[producer]=producers.get(producer,0)+1
    require(sum(r['Kind']=='Native' for r in rows)==counts['Native'] and producers==spec['ProducerRecordCounts']==
        snapshot.get('ProducerRecordCounts') and sorted(r['ID'] for r in rows if r['Kind']=='Export' and
        r['RecordedStatus']=='FAIL')==spec['QualifiedFamilyIDs']==snapshot.get('QualifiedFamilyIDs'),
        'Finite per-kind/producer/raw FAIL inventory differs.')
    require(type(prior_snapshot.get('ArtifactCount')) is int and
        prior_snapshot['ArtifactCount']==spec['PriorRecordCount'] and
        len(prior_snapshot.get('Records',[]))==spec['PriorRecordCount'],'Exact prior finite history required.')
    prior_ids=set()
    for prior in prior_snapshot['Records']:
        identity=(prior['Kind'],prior['ID']);row=by_id.get(identity)
        require(identity not in prior_ids and row is not None and all(row[k]==prior[k] for k in
            ('RawSummary','SummarySHA256','CapsuleSHA256','ActualProducerContextSHA256','RecordedStatus')),
            'Prior raw record was replaced, duplicated or dropped.')
        prior_ids.add(identity);history=row.get('PriorHistoricalAdmission')
        require(isinstance(history,dict) and history.get('PriorHistoricalAdmission')==prior.get('PriorHistoricalAdmission') and
            history.get('ExistingPrecisionQualification')==prior.get('ExistingPrecisionQualification'),
            'Recursive prior conditions/precision history changed.')
        if row['Kind']=='Native':
            current={e['ID']:e for e in row['PriorSupplementalEntries']}
            require(all(current.get(e['ID'])==e for e in prior.get('PriorSupplementalEntries',[])),
                'Prior per-native supplemental entry changed/disappeared.')
    return index


def public_proof(proof,stage=None):
    if not isinstance(proof,dict) or proof.get('_FiniteRetentionV3Verified') is not True:
        return None
    value=primitive(proof['_Here']).public_proof(proof,stage)
    return {**value,'TransitionSchemaVersion':SCHEMA,'PriorReceiptSHA256':proof['PriorReceipt']['SHA256']}


def record_evidence(summary_path,summary,current_context,proof):
    if not isinstance(proof,dict) or proof.get('_FiniteRetentionV3Verified') is not True:
        return None
    value=primitive(proof['_Here']).record_evidence(summary_path,summary,current_context,proof)
    return None if value is None else {**value,'TransitionSchemaVersion':SCHEMA,
        'PriorReceiptSHA256':proof['PriorReceipt']['SHA256']}


def supplemental_plan(case_id,source_sha,entries,results,context,proof):
    if not isinstance(proof,dict) or proof.get('_FiniteRetentionV3Verified') is not True:
        return {'Status':'UNAVAILABLE','Reason':'No verified finite schema-3 proof.'}
    return primitive(proof['_Here']).supplemental_plan(case_id,source_sha,entries,results,context,proof)


def owned_record(record,binding,pass_number):
    cleanup=record['OwnedCleanup']
    require(record.get('Status')=='CAPTURED' and type(record.get('Pass')) is int and
        record['Pass']==pass_number and record.get('RawSHA256')==binding['SHA256'] and
        record.get('KernelExitCode')==0 and type(record.get('KernelExitCode')) is int and
        record.get('Bound') is None and record.get('PinsUnchanged') is True and
        cleanup.get('Status')=='COMPLETE' and cleanup.get('AllObservedOwnedGroupsGone') is True and
        cleanup.get('AllOwnedPopenLeadersReaped') is True and cleanup.get('CleanupDeadlineSatisfied') is True and
        cleanup.get('InspectionFailed') is False,'Capture or owned cleanup is incomplete.')


def history_resolver(old,root,prefix,mapping,prior_binding):
    """Resolve exact historical versions, never a mutable logical-name alias.

    Returns a resolver and bound prior objects. Per-record/probe/PQ graphs are
    deliberately not traversed: the unchanged record primitive owns them.
    """
    root=Path(root).resolve();prefix=Path(prefix).resolve()
    require(mapping.get('CanonicalRoot')==str(root) and mapping.get('NoMutableVerifierAliases') is True and
        mapping.get('NoPrivateToInstalledAdmissionMapping') is True,'Explicit canonical/history mapping required.')
    copies,references=mapping['OriginalToArchivedFiles'],mapping['HashReferencesOnly']
    require(isinstance(copies,dict) and isinstance(references,dict),'Historical maps are malformed.')
    versions={}
    for row in mapping.get('VersionedHistoricalBindingResolutions',[]):
        require(isinstance(row,dict) and set(row)=={'OriginalLogicalPath','SHA256','ActualPreimagePath',
            'PriorReceiptSHA256','PriorRootMapping'} and pin(row['SHA256']),
            'Explicit versioned historical binding is malformed.')
        key=(row['OriginalLogicalPath'],row['SHA256'])
        require(key not in versions,'Duplicate historical logical version.')
        versions[key]=row
    def archived(absolute,expected):
        absolute=str(Path(absolute))
        row=copies.get(absolute)
        if row is not None and row.get('SHA256')==expected:
            path=old.inside(prefix,row['ArchivedRelativePath'])
            require(path!=Path(absolute).resolve() and digest(path)==expected and
                (not Path(absolute).exists() or not path.samefile(absolute)),
                'Historical copy changed or aliases a mutable original.')
            return path
        row=references.get(absolute)
        if row is not None and row.get('SHA256')==expected:
            path=Path(absolute)
            require(path.is_absolute() and '..' not in path.parts and row.get('Copied') is False and
                natural(row.get('Bytes')) and (path.suffix=='.wxf' or row['Bytes']>16*1024**2) and
                path.stat().st_size==row['Bytes'] and digest(path)==expected,'Historical hash reference changed.')
            return path
        raise ValueError('Missing immutable historical preimage: '+absolute)
    def resolve(binding):
        require(isinstance(binding,dict) and set(binding)=={'Path','SHA256'} and pin(binding['SHA256']),
            'Historical binding is malformed.')
        rel=Path(binding['Path']);require('..' not in rel.parts,'Historical path traversal.')
        candidates=[rel] if rel.is_absolute() else [root/rel,root/'tests/results/SM-5-point'/rel]
        found={}
        for candidate in candidates:
            logical=str(candidate)
            direct=copies.get(logical) or references.get(logical)
            if direct is not None and direct.get('SHA256')==binding['SHA256']:
                found[logical]=archived(logical,binding['SHA256'])
            version=versions.get((logical,binding['SHA256']))
            if version is not None:
                actual=archived(version['ActualPreimagePath'],binding['SHA256'])
                require(logical not in found or found[logical]==actual,'Ambiguous historical version.')
                found[logical]=actual
        require(len(found)==1,'Missing or ambiguous logical historical path/SHA256.')
        return next(iter(found.values()))
    require(isinstance(prior_binding,dict) and set(prior_binding) in ({'Path','SHA256'},
        {'Path','SHA256','PolicyCanonicalSHA256'}),'Prior trust anchor is malformed.')
    def historical_object(binding):
        path=resolve(binding);value=json.loads(path.read_text())
        require(digest(path)==binding['SHA256'], 'Historical JSON changed during readback.')
        return value
    prior=historical_object({k:prior_binding[k] for k in ('Path','SHA256')})
    require(isinstance(prior,dict),'Historical prior receipt is malformed.')
    # Any old logical path redirected to an earlier epoch must be justified by
    # the exact previous receipt and its already bound original-to-archive map.
    if versions:
        prior_map=historical_object(prior['RootMapping'])
        require(prior_map.get('CanonicalRoot')==str(root),'Prior Root mapping differs.')
        old_prefix=old.inside(root,prior['ArtifactPrefix'])
        for (logical,expected),row in versions.items():
            actual=Path(row['ActualPreimagePath'])
            if row['PriorReceiptSHA256'] is None:
                require(row['PriorRootMapping'] is None and str(actual)==logical,
                    'A direct historical binding was relabeled.')
            else:
                old_row=prior_map['OriginalToArchivedFiles'].get(logical)
                require(row['PriorReceiptSHA256']==prior_binding['SHA256'] and
                    row['PriorRootMapping']==prior['RootMapping'] and isinstance(old_row,dict) and
                    old_row['SHA256']==expected and actual==old.inside(old_prefix,old_row['ArchivedRelativePath']),
                    'Historical version is not in the exact prior receipt mapping.')
    pending,seen=[prior],set()
    def walk(value):
        if isinstance(value,dict):
            if set(value)=={'Path','SHA256'} and isinstance(value.get('Path'),str) and pin(value.get('SHA256')):
                path=resolve(value)
                if path.suffix=='.json' and path not in seen:
                    seen.add(path);pending.append(historical_object(value))
            for key,item in value.items():
                if key not in ('Records','Probes'):
                    walk(item)
        elif isinstance(value,list):
            for item in value:
                walk(item)
    while pending:
        walk(pending.pop())
    if 'PolicyCanonicalSHA256' in prior_binding:
        require(canonical(historical_object(prior['Policy']))==prior_binding['PolicyCanonicalSHA256'],
            'Prior registered policy canonical digest differs.')
    prior_snapshot=historical_object(prior['Snapshot'])
    return resolve,prior,prior_snapshot


def verify_cold_controls(old,root,inputs,before,after,policy,resolve):
    require(set(inputs)=={'ActualInstalledRootCaptures','BeforePair','AfterPair','CaptureControllers',
        'NewSymbolInventory'} and inputs['ActualInstalledRootCaptures'] is True and
        set(inputs['CaptureControllers'])=={'Before','After'},'Actual cold input bundle differs.')
    for side,manifest in [('Before',before),('After',after)]:
        controller=old.bound_object(root,inputs['CaptureControllers'][side])
        require(Path(controller['SourceRoot']).resolve()==root/'source' and
            controller.get('NamespacePatterns')==policy['NamespacePatterns'] and
            controller.get('NoModelReadsOrPhysics') is True and
            controller.get('NoCompatibilityAdmissionOrRootEdits') is True and
            controller.get('UsageRegistryPinnedBeforeAndAfterBothCaptures') is True,
            'Private, non-cold, or unbound Usage captures cannot authorize admission.')
        pins=controller['Inputs']
        for spec in policy['MechanicalSourceInventories'].values():
            exact={str(root/p):manifest['Files'].get(p,spec['AdditionalSourceFiles'].get(p))
                for p in spec['OrderedPaths']}
            observed={p:h for p,h in pins.items() if Path(p).is_relative_to(root/'source')}
            require(observed==exact,'Cold capture source tree differs from exact mechanical inventory.')
            if side=='After':
                require({str(p):digest(p) for p in (root/'source').rglob('*') if p.is_file()}==exact,
                    'Actual complete source tree has an extra/missing/changed inventory file.')
        expected={str(root/n):p for n,p in manifest['Files'].items() if n.startswith('source/') and n.endswith('.wl')}
        require(all(pins.get(p)==h for p,h in expected.items()) and
            pins.get(str(root/USAGE))==policy['LoadingInputs'][side][USAGE]['SHA256'],
            'Every source/Usage byte must be an actual cold input.')
        for path,expected_pin in pins.items():
            require(Path(path).is_absolute() and Path(path).is_relative_to(root),
                'Cold proof must be durable inside the actual checkout.')
            if side=='Before':
                resolve({'Path':path,'SHA256':expected_pin})
            else:
                require(digest(path)==expected_pin,'Actual after cold input changed.')
        pair=inputs[side+'Pair'];records=controller.get('Records')
        require(isinstance(records,list) and len(records)==len(pair)==2,'Two completed cold captures required.')
        for index,(record,binding) in enumerate(zip(records,pair),1):
            require(Path(record['RawFile']).resolve()==old.inside(root,binding['Path']),
                'Cold controller output path differs.')
            owned_record(record,binding,index)


def verify_loader(old,root,policy,before,after,old_fields,new_fields,resolve):
    spec=policy['Loader']
    require(isinstance(spec,dict) and set(spec)=={'RequiredNames','OptionalNames','ScriptText','Sides'} and
        set(spec['Sides'])=={'Before','After'} and isinstance(spec['ScriptText'],str),
        'Exact independently collected loader bundle required.')
    names=spec['RequiredNames']+spec['OptionalNames']
    require(isinstance(spec['RequiredNames'],list) and isinstance(spec['OptionalNames'],list) and
        len(names)==len(set(names)) and names and all(isinstance(n,str) and
        not Path(n).is_absolute() and '..' not in Path(n).parts for n in names),
        'Ordered loader inventory is malformed.')
    # This version retains the existing signature implementation literally.
    require(old_fields[SIGNATURE]['DownValues']==new_fields[SIGNATURE]['DownValues'],
        'Loader implementation/formula changed.')
    required_text=', '.join('"'+n+'"' for n in spec['RequiredNames'])
    optional_text=', '.join('"'+n+'"' for n in spec['OptionalNames'])
    require(old_fields[SIGNATURE]['DownValues'].count('{'+required_text+'}')==1 and
        old_fields[SIGNATURE]['DownValues'].count('{'+optional_text+'}')==1,
        'Loader name inventory differs from the unchanged actual function.')
    result={}
    for side,manifest,fields in [('Before',before,old_fields),('After',after,new_fields)]:
        bundle=spec['Sides'][side]
        require(set(bundle)=={'Controller','Raw','Script','RecordIndex'} and
            type(bundle['RecordIndex']) is int and bundle['RecordIndex']>=0,'Loader side bundle differs.')
        controller=old.bound_object(root,bundle['Controller']);row=old.bound_object(root,bundle['Raw'])
        script=old.bound(root,bundle['Script'])
        require(script.read_text()==spec['ScriptText'] and
            controller.get('Status')=='BARE_LOADER_INPUTS_VERIFIED_NOT_SEALED' and
            controller['Inputs'].get(str(script))==bundle['Script']['SHA256'],
            'Literal bare collector or completion status differs.')
        for path,expected in controller['Inputs'].items():
            require(Path(path).is_absolute() and Path(path).is_relative_to(root),'Bare proof has a temporary dependency.')
            if side=='Before':
                resolve({'Path':path,'SHA256':expected})
            else:
                require(digest(path)==expected,'Actual after bare input changed.')
        record=controller['Records'][bundle['RecordIndex']]
        require(Path(record['RawFile']).resolve()==old.inside(root,bundle['Raw']['Path']),'Bare raw output path differs.')
        owned_record(record,bundle['Raw'],bundle['RecordIndex']+1)
        require(row.get('Status')=='INDEPENDENT_BARE_LOADER_RECOMPUTATION' and
            Path(row['SourceDirectory']).resolve()==root/'source' and row.get('NoModelOrPhysics') is True and
            row.get('WolframVersion')==policy['WolframVersion'] and row.get('SystemID')==policy['SystemID'] and
            row.get('RequiredNames')==spec['RequiredNames'] and row.get('OptionalNames')==spec['OptionalNames'] and
            type(row.get('InputCount')) is int and row['InputCount']==len(names),
            'Bare source/platform/name/count provenance differs.')
        expected={n:manifest['Files']['source/'+n] for n in names}
        require(row['SourceSHA256']==expected,'Bare source byte inventory differs.')
        paths=[]
        for n in names:
            path=root/'source'/n
            paths.append(resolve({'Path':str(path),'SHA256':expected[n]}) if side=='Before' else path)
            require(digest(paths[-1])==expected[n],'Bare byte preimage differs.')
        text=row['BareInputsInputForm']
        require(isinstance(text,str) and re.fullmatch(r'\{[0-9]+(?:, [0-9]+)*\}',text) is not None and
            [int(v) for v in re.findall('[0-9]+',text)]==
            [int(hashlib.md5(p.read_bytes()).hexdigest(),16) for p in paths],
            'Bare ordered byte-hash inputs differ.')
        loaded=re.fullmatch(r'\{HoldPattern\[ConstructiveDiagrams`Private`\$amplitudeParallelLoadedSignature\] :> ([0-9]+)\}',
            fields[LOADER]['OwnValues'])
        require(loaded is not None and type(row.get('Bare')) is int and type(row.get('Loaded')) is int and
            row.get('BareMatchesLoaded') is True and row.get('BareMatchesFunction') is True and
            row['Bare']==row['Loaded']==int(loaded[1]) and
            row['ActualFunctionDownValues']==fields[SIGNATURE]['DownValues'],
            'Actual loaded/function/bare signature values disagree.')
        result[side]={'Value':row['Bare'],'InputCount':len(names),'RawSHA256':bundle['Raw']['SHA256'],
            'ControllerSHA256':bundle['Controller']['SHA256'],'ScriptSHA256':bundle['Script']['SHA256']}
    return result


def receipt_status(root,here,configuration,current_context,*,registry=None,core=None):
    """Registered refusals fail closed. Absent entries preserve old dispatch."""
    registered=False
    try:
        root,here=Path(root).resolve(),Path(here).resolve()
        if registry is None:
            path=here/REGISTRY_FILENAME
            if not path.exists():
                return {'Status':'UNAVAILABLE','Registered':False,'Reason':'No versioned registry.','Proof':None}
            registered=True
            registry=json.loads(path.read_text())
        else:
            registered=True
        require(isinstance(registry,dict),'Versioned registry must be an object.')
        entries=registry.get('Receipts')
        require(set(registry)=={'SchemaVersion','Receipts'} and isinstance(entries,dict) and
            type(registry['SchemaVersion']) is int and registry['SchemaVersion'] in (1,2,3),
            'Malformed/unknown versioned registry.')
        registered=current_context in entries
        if not registered:
            return {'Status':'UNAVAILABLE','Registered':False,'Reason':'No registered context entry.','Proof':None}
        old=primitive(here)
        require(digest(root/'tests/checks/test_constructor_receipts.py')==FIXTURE_SHA,'Original fixture changed.')
        if registry['SchemaVersion']==1:
            result=old.receipt_status(root,here,configuration,current_context,registry=entries,core=core)
            return {**result,'Registered':True,'DelegatedSchemaVersion':1}
        if registry['SchemaVersion']==2:
            result=version2(here).receipt_status(root,here,configuration,current_context,registry=registry,core=core)
            return {**result,'Registered':True,'DelegatedSchemaVersion':2}
        entry=entries[current_context]
        require(isinstance(entry,dict) and set(entry)=={'Path','SHA256','PolicyCanonicalSHA256'},
            'Registered schema-3 anchor is null/malformed.')
        receipt=old.bound_object(root,{k:entry[k] for k in ('Path','SHA256')})
        require(type(receipt.get('SchemaVersion')) is int and receipt['SchemaVersion']==SCHEMA and
            receipt.get('Kind')==KIND and receipt.get('Status')=='SEALED_REVIEWED' and
            receipt.get('AfterContextSHA256')==current_context and old._scope(receipt),
            'Unsealed/wrong-context receipt or historical status upgrade.')
        policy=old.bound_object(root,receipt['Policy'])
        validate_policy(policy,entry['PolicyCanonicalSHA256'])
        require(receipt['BeforeContextSHA256']==policy['BeforeContextSHA256'] and
            receipt['AfterContextSHA256']==policy['AfterContextSHA256'] and receipt['PriorReceipt']==policy['Snapshot']['PriorReceipt'] and
            policy['CaptureSourceDirectories']==[str(root/'source')]*2,'Private captures cannot authorize installed admission.')
        for relative,expected in policy['VerifierTools'].items():
            require(digest(old.inside(root,relative))==expected,'Current verifier tool changed.')
        require(policy['VerifierTools'].get(str(Path(__file__).resolve().relative_to(root)))==digest(__file__) and
            policy['VerifierTools'].get('tests/SM-5-point/constructor_receipts.py')==LEGACY_SHA and
            policy['VerifierTools'].get('tests/SM-5-point/constructor_receipts_v2.py')==V2_SHA and
            policy['VerifierTools'].get('tests/SM-5-point/scoped_transition_v2.py')==ANALYZER_V2_SHA,
            'Exact current/legacy verifier inventory missing.')
        stage=old._stage(here)
        before=old.bound_object(root,receipt['BeforeManifest']);after=old.bound_object(root,receipt['AfterManifest'])
        require(stage.fingerprint(before)==policy['BeforeContextSHA256'] and
            stage.fingerprint(after)==current_context and stage.manifest(root,here,configuration)==after,
            'Actual source/model/runtime context differs.')
        mapping=old.bound_object(root,receipt['RootMapping'])
        resolve,prior,prior_snapshot=history_resolver(old,root,old.inside(root,receipt['ArtifactPrefix']),mapping,receipt['PriorReceipt'])
        require(type(prior.get('SchemaVersion')) is int and prior['SchemaVersion']==policy['Snapshot']['PriorSchemaVersion'] and
            prior.get('Status')=='SEALED_REVIEWED' and prior.get('AfterContextSHA256')==policy['BeforeContextSHA256'] and
            prior['Snapshot']==policy['Snapshot']['PriorSnapshot'] and old._scope(prior),
            'Prior reviewed finite receipt does not authorize the exact before checkpoint.')
        for name,expected in before['Files'].items():
            if name.startswith('source/'):
                resolve({'Path':str(root/name),'SHA256':expected})
        require(core is not None,'Exact cold definition reader is required.')
        inputs=receipt['DefinitionInputs']
        verify_cold_controls(old,root,inputs,before,after,policy,resolve)
        before_pair=[old.bound_object(root,b) for b in inputs['BeforePair']]
        after_pair=[old.bound_object(root,b) for b in inputs['AfterPair']]
        inventory=old.bound_object(root,inputs['NewSymbolInventory'])
        analysis,old_fields,new_fields=inspect_transition(core,policy,entry['PolicyCanonicalSHA256'],before,after,
            before_pair,after_pair,inventory,installed_root_captures=True)
        loader=verify_loader(old,root,policy,before,after,old_fields,new_fields,resolve)
        analysis={**analysis,'Status':'ANALYZED_NOT_SEALED','Loader':loader,
            **{k:False for k in old.FALSE_CLAIMS},'ReceiptSealed':False,'RegistryActivationPerformed':False}
        require(analysis==old.bound_object(root,receipt['Analysis']),'Exact installed transition analysis differs.')
        snapshot=old.bound_object(root,receipt['Snapshot']);captured=old.bound_object(root,receipt['SnapshotVerification'])
        c=policy['Snapshot']['Counts']
        require(captured.get('Status')=='UNSEALED_FINITE_RECORD_SNAPSHOT_COMPLETE' and
            captured.get('SnapshotSHA256')==receipt['Snapshot']['SHA256'] and
            tuple(captured.get(k) for k in ('NativeCases','ExportFamilies','ActualProducerContexts'))==
                (c['Native'],c['Export'],c['ActualProducers']) and
            captured.get('Prior'+str(policy['Snapshot']['PriorRecordCount'])+'RecordsPreserved') is True and
            captured.get('NoKernelsPhysicsOrCapsuleImports') is True and captured.get('RootWrites') is False,
            'Actual locked finite snapshot verification differs.')
        old.bound(root,receipt['SnapshotBuilder'])
        index=exact_snapshot(old,root,snapshot,prior_snapshot,policy)
        old.bound(root,{k:entry[k] for k in ('Path','SHA256')})
        proof={**receipt,'_FiniteRetentionV3Verified':True,'_FiniteConstructorReceiptVerified':True,
            '_ProofSHA256':entry['SHA256'],'_Records':index,'_Root':root,'_Here':here,
            '_Snapshot':snapshot,'_Analysis':analysis}
        return {'Status':'AVAILABLE_FINITE_HISTORICAL_RETENTION_V3','Registered':True,'Reason':None,'Proof':proof}
    except (OSError,ValueError,KeyError,TypeError,AttributeError,ImportError,IndexError) as error:
        return {'Status':'UNAVAILABLE','Registered':registered,'Reason':str(error),'Proof':None}


def verified_proof(root,here,configuration,current_context,*,registry=None,core=None):
    return receipt_status(root,here,configuration,current_context,registry=registry,core=core)['Proof']
