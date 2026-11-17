#!/usr/bin/env python3
"""Opt in to generation transport while leaving the normal runner unchanged.

Normal runner flags are forwarded literally. The worker's --check-source call
adds a strict independently validated source-family gate using the original
runner's checked_summary and export_input_admission, including all probe rules.
"""
import argparse
import importlib.util
import json
import os
from pathlib import Path
import sys

HERE = Path(__file__).resolve().parent

def load_workflow():
    spec = importlib.util.spec_from_file_location('generation_source_admission', HERE / 'run.py')
    workflow = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(workflow)
    return workflow

def source_admission(workflow, family_id, case_id, configuration):
    rejected = {'Status': 'REJECTED', 'FamilyID': family_id,
                'Reason': 'The generation source family is not fully admitted.'}
    try:
        permit = workflow.export_input_admission(family_id, configuration)
        if permit.get('Status') != 'APPROVED' or case_id not in permit.get('CaseIDs', []):
            return rejected
        current = permit['ExporterContextSHA256']
        proof = workflow.context_compatibility(configuration, current)
        summary_path = workflow.RESULTS / (family_id + '-export.json')
        summary_sha = workflow.stage_context.digest(summary_path)
        export = workflow.checked_summary(family_id, '-export', current, proof)
        requested = configuration.get('Points', 1000)
        if (not isinstance(export, dict) or export.get('Status') != 'PASS'
                or export.get('CalcHEPStatus') != 'PASS' or export.get('MadGraphStatus') != 'PASS'
                or type(requested) is not int or requested < 1
                or type(export.get('Points')) is not int or export['Points'] < requested
                or export.get('CaseIDs') != permit['CaseIDs']
                or export.get('CaseCapsulesSHA256') != permit['CaseCapsulesSHA256']
                or workflow.stage_context.digest(summary_path) != summary_sha
                or workflow.stage_context.digest(summary_path.with_suffix('.wxf')) != export.get('CapsuleSHA256')):
            return rejected
        return {**permit, 'GenerationSourceCaseID': case_id,
                'GenerationSourceExportCapsuleSHA256': export['CapsuleSHA256'],
                'GenerationSourceExportSummarySHA256': summary_sha,
                'GenerationSourceExportContextSHA256': export['ContextSHA256'],
                'GenerationSourceExportContextCompatibility': export.get('ContextCompatibility'),
                'GenerationSourceQualification': 'NATIVE_AND_BOTH_EXTERNAL_BACKENDS_PASS',
                'GenerationSourceSupplementalChecks': 'PASS_OR_NOT_REQUIRED'}
    except (OSError, ValueError, KeyError, TypeError, AttributeError):
        return rejected

def main(argv=None):
    arguments = list(sys.argv[1:] if argv is None else argv)
    if arguments[:1] == ['--check-source']:
        parser = argparse.ArgumentParser(description='Verify one source family without writes or calculations.')
        parser.add_argument('--check-source', required=True, metavar='FAMILY')
        parser.add_argument('--case-id', required=True)
        parser.add_argument('--configuration', type=Path, required=True)
        args = parser.parse_args(arguments)
        workflow = load_workflow()
        configuration = workflow.read_json(args.configuration, {})
        result = source_admission(workflow, args.check_source, args.case_id, configuration)
        print(json.dumps(result, indent=2))
        return 0 if result['Status'] == 'APPROVED' else 2
    # exec preserves the ordinary runner's CLI, lock, ownership, deadlines,
    # history handling and exit status. Its existing worker inherits this flag.
    os.environ['HEPCAT_SM5_TRANSPORT_GENERATION'] = '1'
    os.execv(sys.executable, [sys.executable, str(HERE / 'run.py'), *arguments])
    return 0

if __name__ == '__main__':
    raise SystemExit(main())
