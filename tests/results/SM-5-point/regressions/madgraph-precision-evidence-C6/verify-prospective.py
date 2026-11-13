"""Read the actual frozen packet through a prospective binding, without admission."""
from pathlib import Path
import hashlib
import importlib.util
import json
import time

ROOT = Path(__file__).resolve().parents[5]
HERE = Path(__file__).resolve().parent
RESULTS = ROOT / 'tests/results/SM-5-point'
helper = ROOT / 'tests/SM-5-point/madgraph_precision_evidence.py'
spec = importlib.util.spec_from_file_location('sm5_prospective_mg_precision', helper)
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)
checksum = lambda path: hashlib.sha256(path.read_bytes()).hexdigest()
helper_before = checksum(helper)
binding = json.loads((HERE / 'prospective-binding.json').read_text())
configuration = json.loads((RESULTS / 'configuration.json').read_text())
inventory = json.loads((ROOT / 'tests/SM-5-point/inventory.json').read_text())
family = next(value for value in inventory['Families'] if value['ID'] == binding['FamilyID'])
export = json.loads((RESULTS / (family['ID'] + '-export.json')).read_text())
cases = {identifier: json.loads((RESULTS / (identifier + '.json')).read_text())
         for identifier in family['CaseIDs']}
canonical_link = RESULTS / (family['ID'] + '-precision.json')
link_before = canonical_link.read_bytes() if canonical_link.exists() else None
proof = module.constructor_context.resolve(ROOT, ROOT / 'tests/SM-5-point',
    configuration, configuration['ContextSHA256'])
started = time.monotonic()
verified = module._qualification_from_link(binding, family, export, cases,
    configuration['ContextSHA256'], RESULTS, ROOT / 'tests/results/spinas-output/SM-5-point', 1000, proof)
assert checksum(helper) == helper_before
assert (canonical_link.read_bytes() if canonical_link.exists() else None) == link_before
print(json.dumps({'Status': 'PASS' if verified else 'UNQUALIFIED',
    'ProspectiveBindingOnly': True, 'NoLinkWritten': link_before is None,
    'CanonicalLinkUnchanged': True, 'NoCampaignAdmissionPerformed': True,
    'HelperSHA256': helper_before, 'Seconds': time.monotonic() - started,
    'Result': verified}, indent=2))
