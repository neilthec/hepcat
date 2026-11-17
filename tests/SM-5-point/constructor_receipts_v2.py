"""Separate exact schema-2 reader. Never seal, write, migrate or upgrade status.

The schema-1 reader is byte-literal. Only its already reviewed per-record and
supplemental primitives are reused, after this independent versioned verifier.
"""
from pathlib import Path
import importlib.util
import json

KIND = 'FiniteRecordedEvidenceRetentionAfterC13CombinedExtensions'
REGISTRY_FILENAME = 'constructor-receipts-v2.json'
BEFORE_CONTEXT = '1b970726a98303b901fea0fa5db54f8ba7e08d265271f920a823d280ad1ccb47'
PRIOR_RECEIPT_SHA = '2f229bfb865cd6ba2772af15bbd77fdf5c8dfc4ed54921eee3934b33b4d32bc0'
PRIOR_SNAPSHOT_SHA = '4390bfec9f1073b87456d16d18967e2c9914a5006a1b2ec80d8763fb1e0be043'
LEGACY_SHA = '91649106592da6a4bea9999e7eb4f4814197789fc78efa9f449bcc3860644d11'
ANALYZER_SHA = '77c86e9bf3b27ec9f205dc81f789c7abc6fc47883c184b9711e4d9784da02b99'
LEGACY_FIXTURE_SHA = '3cb89713c3fe9716d86ddd1c34d4b598f7dad90e5fb8500bc96e0bbb2542a0f9'
COUNTS = (523,352,875,13)
OLD_TOOLS = frozenset(('constructor_receipts.py','constructor_context.py','stage_context.py',
    'supplemental_probes.py','run.py','generated_cleanup.py','precision_evidence.py',
    'madgraph_precision_evidence.py','capture-constructor-definitions.wl','capture-constructor-definitions.py'))
NEW_TOOLS = frozenset(('constructor_receipts_v2.py','scoped_transition_v2.py'))

def require(value,message):
    if not value:
        raise ValueError(message)

def module(name,path):
    spec = importlib.util.spec_from_file_location(name,path)
    value = importlib.util.module_from_spec(spec);spec.loader.exec_module(value)
    return value

def primitive(here):
    path = Path(here)/'constructor_receipts.py'
    # Hash without importing potentially changed code.
    import hashlib
    require(hashlib.sha256(path.read_bytes()).hexdigest() == LEGACY_SHA,
        'The literal schema-1 primitive preimage changed.')
    return module('unchanged_schema1_record_primitives',path)

def source_analyzer(here,old):
    path = Path(here)/'scoped_transition_v2.py'
    require(old.digest(path) == ANALYZER_SHA,'The exact reviewed schema-2 analyzer changed.')
    return module('exact_scoped_transition_analyzer_v2',path)

def exact_snapshot(old,root,snapshot,prior_snapshot):
    require(snapshot.get('SchemaVersion') == 2 and
        snapshot.get('Status') == 'UNSEALED_VERIFIED_HISTORICAL_SNAPSHOT' and
        snapshot.get('AdmissionContextSHA256') == BEFORE_CONTEXT and
        tuple(snapshot.get(k) for k in ('NativeCaseCount','ExportCompanionCount','ArtifactCount','ActualProducerContextCount')) == COUNTS,
        'Wrong finite schema-2 checkpoint or counts.')
    require(all(snapshot.get(k) is True for k in ('ActualProducerContextsPreserved','PriorConditionsPreserved',
        'Prior645RawRowsPreservedLiterally','PerNativePriorProbeEntriesBound','CurrentNewProbeFailuresRemainAuthoritative',
        'CaseAndExportCacheChecksAreLazy','PrecisionCacheChecksArePerExport','PrecisionVerifiersRemainAuthoritative')) and
        snapshot.get('NewValidationPassClaimed') is False and snapshot.get('CanonicalWritesPerformed') is False and
        snapshot.get('CapsulesCopied') is False and
        snapshot.get('UnrelatedNewCaseRegistryAdditionsInvalidateHistoricalCases') is False,
        'Snapshot changes evidence scope or claims a rerun/status upgrade.')
    require(not snapshot.get('AuxiliaryBindings') and not snapshot.get('ExternalAuxiliaryBindings') and
        not snapshot.get('SharedProofBindings'),'Family caches cannot become global dependencies.')
    rows = snapshot.get('Records');require(isinstance(rows,list) and len(rows) == 875,'Exact875 records required.')
    index,identities,producers = {},set(),{}
    by_id = {}
    for row in rows:
        kind,identifier = row['Kind'],row['ID'];raw = row['RawSummary']
        require(kind in ('Native','Export') and (kind,identifier) not in identities,
            'Duplicate or unsupported finite record identity.')
        summary = old.inside(root,row['SummaryPath'])
        expected = 'tests/results/SM-5-point/'+identifier+('-export' if kind == 'Export' else '')+'.json'
        require(row['SummaryPath'] == expected and row['CapsulePath'] == expected.removesuffix('.json')+'.wxf' and
            summary not in index and row['AdmittedAtContextSHA256'] == BEFORE_CONTEXT and
            raw.get('ID' if kind == 'Native' else 'FamilyID') == identifier and
            row['ActualProducerContextSHA256'] == raw.get('ContextSHA256') and
            row['RecordedStatus'] == raw.get('Status') and row['CapsuleSHA256'] == raw.get('CapsuleSHA256'),
            'Finite record path, producer, status or raw capsule identity differs.')
        require(isinstance(row['SummarySHA256'],str) and len(row['SummarySHA256']) == 64 and
            isinstance(row['CapsuleSHA256'],str) and len(row['CapsuleSHA256']) == 64,
            'Malformed exact summary/capsule digest.')
        producer = raw['ContextSHA256'];history = row.get('PriorHistoricalAdmission')
        require((producer == BEFORE_CONTEXT and history is None) or
            (producer != BEFORE_CONTEXT and isinstance(history,dict) and
             history.get('HistoricalEvidenceOnly') is True and
             history.get('RecordedStatus') == raw['Status'] and
             history.get('ActualProducerContextSHA256') == producer and
             history.get('CurrentContextSHA256') == BEFORE_CONTEXT and
             all(history.get(k) is False for k in old.FALSE_CLAIMS)),
            'Actual producer or original finite admission history was upgraded/lost.')
        if kind == 'Native':
            require(raw['Status'] == 'COMPLETE' and isinstance(row.get('PriorSupplementalEntries'),list),
                'Native input or exact prior probe inventory is incomplete.')
            entries = row['PriorSupplementalEntries']
            require(len({e['ID'] for e in entries}) == len(entries) and
                all(e['CaseID'] == identifier and e['SourceCapsuleSHA256'] == row['CapsuleSHA256'] for e in entries),
                'Prior probe identity or retained native expression binding differs.')
            require(row.get('ExistingPrecisionQualification') is None,
                'A native record cannot depend on an export precision packet.')
        else:
            require(raw['Status'] in ('PASS','FAIL') and
                ((raw['Status'] == 'FAIL' and isinstance(row.get('ExistingPrecisionQualification'),dict)) or
                 (raw['Status'] == 'PASS' and row.get('ExistingPrecisionQualification') is None)),
                'Raw export FAIL was upgraded or its existing precision qualification lost.')
            require(row['NativeCaseInputBindings'] == {key:raw.get(key) for key in
                ('CaseIDs','CaseCapsulesSHA256','NativeCaseContexts')},'Export native bindings differ from raw record.')
            if raw['Status'] == 'FAIL':
                require(row.get('PrecisionBindings') and isinstance(row.get('PrecisionExternalBindings'),list),
                    'Existing precision dependencies must remain scoped per export.')
        producers[producer] = producers.get(producer,0)+1
        index[summary] = row;identities.add((kind,identifier));by_id[(kind,identifier)] = row
    require(sum(r['Kind'] == 'Native' for r in rows) == 523 and len(producers) == 13 and
        producers == snapshot.get('ProducerRecordCounts'),'Finite per-kind or producer inventory differs.')
    require(sorted(r['ID'] for r in rows if r['Kind'] == 'Export' and r['RecordedStatus'] == 'FAIL') ==
        sorted(snapshot['QualifiedFamilyIDs']) and len(snapshot['QualifiedFamilyIDs']) == 7,
        'The original seven raw precision failures must remain failures.')
    require(prior_snapshot.get('ArtifactCount') == 645 and len(prior_snapshot.get('Records',[])) == 645,
        'Exact prior645-record C13 history is required.')
    for prior in prior_snapshot['Records']:
        row = by_id.get((prior['Kind'],prior['ID']))
        require(row is not None and all(row[k] == prior[k] for k in
            ('RawSummary','SummarySHA256','CapsuleSHA256','ActualProducerContextSHA256','RecordedStatus')),
            'A prior645 raw record was replaced or dropped.')
        history = row['PriorHistoricalAdmission']
        require(isinstance(history,dict) and history.get('PriorHistoricalAdmission') == prior.get('PriorHistoricalAdmission') and
            history.get('ExistingPrecisionQualification') == prior.get('ExistingPrecisionQualification'),
            'Recursive prior conditions or precision history changed.')
        if row['Kind'] == 'Native':
            prior_entries = {e['ID']:e for e in prior.get('PriorSupplementalEntries',[])}
            current_entries = {e['ID']:e for e in row['PriorSupplementalEntries']}
            require(len(current_entries) == len(row['PriorSupplementalEntries']) and
                all(current_entries.get(k) == v for k,v in prior_entries.items()),
                'Prior per-native supplemental entry changed or disappeared.')
    # No live cache is hashed here. The old record primitive does that lazily.
    return index

def verify_history_archive(old,root,receipt,mapping,before):
    require(mapping.get('CanonicalRoot') == str(root) and mapping.get('NoMutableVerifierAliases') is True and
        mapping.get('NoPrivateToInstalledAdmissionMapping') is True,'Explicit original Root/archive mapping required.')
    prefix = old.inside(root,receipt['ArtifactPrefix'])
    copies = mapping['OriginalToArchivedFiles']
    references = mapping['HashReferencesOnly']
    def archived(original):
        original = str(Path(original))
        if original not in copies:
            row = references[original]
            path = Path(original)
            require(row.get('Copied') is False and path.stat().st_size == row['Bytes'] and
                (path.suffix == '.wxf' or row['Bytes'] > 16*1024**2) and
                old.digest(original) == row['SHA256'],
                'A hash-referenced shared historical artifact is missing or changed.')
            return Path(original),row
        row = copies[original]
        path = old.inside(prefix,row['ArchivedRelativePath'])
        require(old.digest(path) == row['SHA256'],'A historical immutable artifact changed.')
        live = Path(original)
        require(path != live and (not live.exists() or not path.samefile(live)),
            'Archived verifier/history cannot alias mutable original paths.')
        return path,row
    prior_path,prior_binding = archived(root/receipt['PriorC13Receipt']['Path'])
    require(prior_binding['SHA256'] == receipt['PriorC13Receipt']['SHA256'] == PRIOR_RECEIPT_SHA,
        'Wrong original C13 receipt.')
    prior = old.object_at(prior_path)
    prior_snapshot_path,prior_snapshot_binding = archived(root/prior['Snapshot']['Path'])
    require(prior_snapshot_binding['SHA256'] == prior['Snapshot']['SHA256'] == PRIOR_SNAPSHOT_SHA,
        'Wrong recursively preserved prior645 snapshot.')
    # Only shared proof metadata is a global dependency. Records, probes and
    # precision cache bindings remain lazy within their original native/export.
    pending,seen = [prior],set()
    def walk(value):
        if isinstance(value,dict):
            if isinstance(value.get('Path'),str) and isinstance(value.get('SHA256'),str) and len(value['SHA256']) == 64:
                relative = Path(value['Path'])
                require('..' not in relative.parts,'Historical path traverses parent directories.')
                candidates = [relative] if relative.is_absolute() else [root/relative,root/'tests/results/SM-5-point'/relative]
                matches = {p for p in candidates if (copies.get(str(p)) or references.get(str(p)) or {}).get('SHA256') == value['SHA256']}
                require(len(matches) == 1,'Historical Root/results path mapping is missing or ambiguous.')
                original = matches.pop()
                path,row = archived(original)
                require(row['SHA256'] == value['SHA256'],'Historical contract binding differs.')
                if path.suffix == '.json' and path not in seen:
                    seen.add(path);pending.append(json.loads(path.read_text()))
            for key,item in value.items():
                if key not in ('Records','Probes'):
                    walk(item)
        elif isinstance(value,list):
            for item in value:
                walk(item)
    while pending:
        walk(pending.pop())
    for name in OLD_TOOLS:
        path,row = archived(root/'tests/SM-5-point'/name)
        require(path.suffix in ('.py','.wl'),'Historical verifier preimage is missing.')
        if name == 'constructor_receipts.py':
            require(row['SHA256'] == LEGACY_SHA,'Original schema-1 verifier preimage differs.')
    fixture,row = archived(root/'tests/checks/test_constructor_receipts.py')
    require(row['SHA256'] == LEGACY_FIXTURE_SHA,'The original schema-1 fixture preimage differs.')
    for relative,pin in before['Files'].items():
        if relative.startswith('source/'):
            path,row = archived(root/relative)
            require(row['SHA256'] == pin,'The complete original C13 source preimage differs.')
    return old.object_at(prior_snapshot_path)

def verify_cold_controls(old,root,inputs,before,after,policy):
    require(set(inputs.get('CaptureControllers',{})) == {'Before','After'},
        'Both actual installed-root cold controllers are required.')
    for side,manifest in [('Before',before),('After',after)]:
        captured = old.bound_object(root,inputs['CaptureControllers'][side])
        require(Path(captured['SourceRoot']).resolve() == root/'source' and
            captured['NamespacePatterns'] == policy['NamespacePatterns'] and
            captured.get('NoModelReadsOrPhysics') is True and
            captured.get('NoCompatibilityAdmissionOrRootEdits') is True and
            captured.get('UsageRegistryPinnedBeforeAndAfterBothCaptures') is True,
            'A private-path, unbound-usage or non-cold controller cannot authorize admission.')
        source_pins = {str(root/name):pin for name,pin in manifest['Files'].items()
            if name.startswith('source/') and name.endswith('.wl')}
        require(all(captured['Inputs'].get(path) == pin for path,pin in source_pins.items()) and
            captured['Inputs'].get(str(root/'Documentation/English/SymbolUsage.wl')) ==
                policy['LoadingInputs'][side]['Documentation/English/SymbolUsage.wl']['SHA256'],
            'The actual cold controller did not pin every loaded source and usage byte.')
        records = captured.get('Records');pair = inputs[side+'Pair']
        require(isinstance(records,list) and len(records) == len(pair) == 2,
            'Exactly two completed owned cold captures are required per side.')
        for index,(record,binding) in enumerate(zip(records,pair),1):
            cleanup = record['OwnedCleanup']
            require(record['Status'] == 'CAPTURED' and record['Pass'] == index and
                record['RawSHA256'] == binding['SHA256'] and record['KernelExitCode'] == 0 and
                record['Bound'] is None and record['PinsUnchanged'] is True and
                cleanup['Status'] == 'COMPLETE' and cleanup['AllObservedOwnedGroupsGone'] is True and
                cleanup['AllOwnedPopenLeadersReaped'] is True and cleanup['CleanupDeadlineSatisfied'] is True and
                cleanup['InspectionFailed'] is False,'Cold captures or owned cleanup are incomplete.')


def receipt_status(root,here,configuration,current_context,*,registry=None,core=None):
    """A new unsealed/private packet always returns UNAVAILABLE, without writes."""
    registered = False
    try:
        root,here = Path(root).resolve(),Path(here).resolve()
        if registry is None:
            registry_path = here/REGISTRY_FILENAME
            if not registry_path.is_file():
                return {'Status':'UNAVAILABLE','Registered':False,'Reason':'No versioned registry.','Proof':None}
            # An unreadable present policy cannot prove absence of an entry.
            registered = True
            registry = json.loads(registry_path.read_text())
        elif not isinstance(registry,dict):
            registered = True
        require(isinstance(registry,dict),'The versioned registry must be an object.')
        entries = registry.get('Receipts')
        if isinstance(entries,dict):
            registered = current_context in entries
        else:
            registered = True
        require(set(registry) == {'SchemaVersion','Receipts'} and isinstance(entries,dict),
            'Malformed versioned trust-anchor registry.')
        require(registry['SchemaVersion'] in (1,2),'Unknown registered receipt schema.')
        if not registered:
            return {'Status':'UNAVAILABLE','Registered':False,'Reason':'No registered entry for this context.','Proof':None}
        old = primitive(here)
        require(old.digest(root/'tests/checks/test_constructor_receipts.py') == LEGACY_FIXTURE_SHA,
            'The original schema-1 fixtures changed.')
        if registry['SchemaVersion'] == 1:
            # Explicit legacy delegation, with its original schema/masks intact.
            result = old.receipt_status(root,here,configuration,current_context,registry=entries,core=core)
            return {**result,'Registered':True,'DelegatedSchemaVersion':1}
        entry = entries[current_context]
        require(isinstance(entry,dict),'The registered schema-2 entry is null or malformed.')
        require(set(entry) == {'Path','SHA256','PolicyCanonicalSHA256'},'Incomplete versioned trust anchor.')
        receipt = old.bound_object(root,{k:entry[k] for k in ('Path','SHA256')})
        require(receipt.get('SchemaVersion') == 2 and receipt.get('Kind') == KIND and
            receipt.get('Status') == 'SEALED_REVIEWED' and receipt.get('BeforeContextSHA256') == BEFORE_CONTEXT and
            receipt.get('AfterContextSHA256') == current_context and old._scope(receipt),
            'Versioned receipt is unsealed, has wrong context or upgrades historical claims.')
        require(core is not None,'Exact existing cold definition reader required.')
        stage = old._stage(here)
        before = old.bound_object(root,receipt['BeforeManifest']);after = old.bound_object(root,receipt['AfterManifest'])
        require(stage.fingerprint(before) == BEFORE_CONTEXT and stage.fingerprint(after) == current_context and
            stage.manifest(root,here,configuration) == after,'Actual source/model/runtime context differs.')
        inputs = receipt['DefinitionInputs']
        require(inputs.get('ActualInstalledRootCaptures') is True,'Actual installed-root twins are still required.')
        policy = old.bound_object(root,receipt['Policy'])
        require(policy['CaptureSourceDirectories'] == [str(root/'source'),str(root/'source')] and
            policy['AnalysisOnlySourceDirectories'] == [str(root/'source'),str(root/'source')],
            'Private copies/path allowances cannot authorize installed-root admission.')
        verify_cold_controls(old,root,inputs,before,after,policy)
        analyzer = source_analyzer(here,old)
        old_pair = [old.bound_object(root,b) for b in inputs['BeforePair']]
        new_pair = [old.bound_object(root,b) for b in inputs['AfterPair']]
        inventory = old.bound_object(root,inputs['NewSymbolInventory'])
        analysis = analyzer.analyze_transition(core,policy,entry['PolicyCanonicalSHA256'],before,after,
            old_pair,new_pair,inventory,policy['LoadingInputs'],policy['LoaderArtifacts'],installed_root_captures=True)
        require(analysis['ActualInstalledRootCaptures'] is True and
            analysis == old.bound_object(root,receipt['Analysis']),'Exact installed transition analysis differs.')
        tools = receipt['VerifierTools']
        require({b['Path'] for b in tools} == {str((here/name).relative_to(root)) for name in OLD_TOOLS|NEW_TOOLS} and
            len(tools) == len(OLD_TOOLS|NEW_TOOLS),'Exact current dispatcher/analyzer/evidence tools required.')
        for row in tools:
            old.bound(root,row)
        mapping = old.bound_object(root,receipt['RootMapping'])
        prior_snapshot = verify_history_archive(old,root,receipt,mapping,before)
        snapshot = old.bound_object(root,receipt['Snapshot'])
        captured = old.bound_object(root,receipt['SnapshotVerification'])
        require(captured.get('Status') == 'UNSEALED_875_RECORD_SNAPSHOT_COMPLETE' and
            captured.get('SnapshotSHA256') == receipt['Snapshot']['SHA256'] and
            tuple(captured.get(k) for k in ('NativeCases','ExportFamilies','ActualProducerContexts')) == (523,352,13) and
            captured.get('Prior645RecordsPreserved') is True and
            captured.get('NoKernelsPhysicsOrCapsuleImports') is True and captured.get('RootWrites') is False,
            'Actual locked pre-cutover verification is missing or differs.')
        old.bound(root,receipt['SnapshotBuilder'])
        index = exact_snapshot(old,root,snapshot,prior_snapshot)
        old.bound(root,{k:entry[k] for k in ('Path','SHA256')})
        proof = {**receipt,'_ScopedV2ReceiptVerified':True,'_FiniteConstructorReceiptVerified':True,
            '_ProofSHA256':entry['SHA256'],'_Records':index,'_Root':root,'_Here':here,
            '_Snapshot':snapshot,'_Analysis':analysis}
        return {'Status':'AVAILABLE_FINITE_HISTORICAL_RETENTION_V2','Registered':True,'Reason':None,'Proof':proof}
    except (OSError,ValueError,KeyError,TypeError,AttributeError,ImportError) as error:
        return {'Status':'UNAVAILABLE','Registered':registered,'Reason':str(error),'Proof':None}

def verified_proof(root,here,configuration,current_context,*,registry=None,core=None):
    return receipt_status(root,here,configuration,current_context,registry=registry,core=core)['Proof']

def public_proof(proof,stage=None):
    if not isinstance(proof,dict) or proof.get('_ScopedV2ReceiptVerified') is not True:
        return None
    old = primitive(proof['_Here'])
    value = old.public_proof(proof,stage)
    return {**value,'TransitionSchemaVersion':2,'PriorC13ReceiptSHA256':PRIOR_RECEIPT_SHA}

def record_evidence(summary_path,summary,current_context,proof):
    if not isinstance(proof,dict) or proof.get('_ScopedV2ReceiptVerified') is not True:
        return None
    old = primitive(proof['_Here'])
    value = old.record_evidence(summary_path,summary,current_context,proof)
    return None if value is None else {**value,'TransitionSchemaVersion':2,'PriorC13ReceiptSHA256':PRIOR_RECEIPT_SHA}

def supplemental_plan(case_id,source_sha,entries,results,context,proof):
    if not isinstance(proof,dict) or proof.get('_ScopedV2ReceiptVerified') is not True:
        return {'Status':'UNAVAILABLE','Reason':'No verified schema-2 finite proof.'}
    # The original selector preserves prior IDs and every new/current failure.
    return primitive(proof['_Here']).supplemental_plan(case_id,source_sha,entries,results,context,proof)
