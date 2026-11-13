"""Bounded same-seed ordinary validation of an exactly atom-ring-equivalent factored pair."""
from pathlib import Path
import hashlib
import importlib.util
import json
import os
import signal
import subprocess
import time

ROOT = Path('/Users/neil/code/hepcat')
HERE = Path(__file__).resolve().parent
EXPECTED = '4165f24c7d5501c40c03ea91f850a5cde77389a1236e3af397fb6d7e2344a167'
spec = importlib.util.spec_from_file_location('vector_scalar_stage', ROOT / 'tests/SM-5-point/stage_context.py')
stage = importlib.util.module_from_spec(spec)
spec.loader.exec_module(stage)
configuration = json.loads((ROOT / 'tests/results/SM-5-point/configuration.json').read_text())
manifest = stage.manifest(ROOT, ROOT / 'tests/SM-5-point', configuration)
assert stage.fingerprint(manifest) == EXPECTED
(HERE / 'source-runtime-manifest.json').write_text(json.dumps(manifest, indent=2, sort_keys=True) + '\n')
sha = lambda path: hashlib.sha256(path.read_bytes()).hexdigest()
scripts = {path.name: sha(path) for path in [HERE / 'check.wl']}
started = time.monotonic()
with (HERE / 'pilot.log').open('w') as output:
    process = subprocess.Popen(
        ['/Applications/Wolfram.app/Contents/MacOS/WolframKernel', '-script', str(HERE / 'check.wl')],
        cwd=ROOT, stdout=output, stderr=subprocess.STDOUT, start_new_session=True,
        env=dict(os.environ, HEPCAT_V5_CONTEXT=EXPECTED, OMP_NUM_THREADS='1', OPENBLAS_NUM_THREADS='1'))
    try:
        code = process.wait(timeout=2460)
    except subprocess.TimeoutExpired:
        os.killpg(process.pid, signal.SIGTERM)
        try:
            process.wait(timeout=10)
        except subprocess.TimeoutExpired:
            os.killpg(process.pid, signal.SIGKILL)
            process.wait()
        code = 124
unchanged = stage.fingerprint(stage.manifest(ROOT, ROOT / 'tests/SM-5-point', configuration)) == EXPECTED
unchanged = unchanged and scripts == {path.name: sha(path) for path in [HERE / 'check.wl']}
record = {'ExitCode': code, 'Seconds': time.monotonic() - started, 'SourceContext': EXPECTED,
          'SourceAndScriptsUnchanged': unchanged, 'LoadedScriptsSHA256': scripts,
          'CanonicalOrProductionChanges': False, 'IndependentBenchmarkReadAfterCandidateFreeze': True,
          'InputCertificatesFromC6RetainedWithExactCurrentModelHashes': True}
(HERE / 'controller.json').write_text(json.dumps(record, indent=2) + '\n')
print('WWZZH_NATIVE_VALIDATION', record)
raise SystemExit(code if unchanged else 2)
