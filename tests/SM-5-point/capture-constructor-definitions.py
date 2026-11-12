"""Capture two exact cold definition inventories, without sealing compatibility.

Run only after the constructor integration is frozen. Each independent process
loads HEPCAT once, reads no model, and performs no physics calculation.
"""
from pathlib import Path
import argparse
import datetime
import importlib.util
import json
import os
import signal
import subprocess
import time


def atomic_json(path, value):
    temporary = path.with_suffix(path.suffix + '.tmp')
    temporary.write_text(json.dumps(value, indent=2, sort_keys=True) + '\n')
    temporary.replace(path)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--directory', required=True, type=Path)
    parser.add_argument('--expected-context', required=True)
    parser.add_argument('--configuration', type=Path)
    parser.add_argument('--kernel', type=Path, default=Path('/Applications/Wolfram.app/Contents/MacOS/WolframKernel'))
    parser.add_argument('--seconds', type=int, default=120)
    args = parser.parse_args()
    if not 1 <= args.seconds <= 120:
        parser.error('--seconds must be between 1 and 120.')
    here = Path(__file__).resolve().parent
    root = here.parents[1]
    directory = args.directory.resolve()
    directory.relative_to(root)
    directory.mkdir(parents=True, exist_ok=False)
    spec = importlib.util.spec_from_file_location('constructor_context_capture', here / 'constructor_context.py')
    context = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(context)
    stage = context.stage_module()
    configuration = context.object_at(args.configuration or root / 'tests/results/SM-5-point/configuration.json')
    before = stage.manifest(root, here, configuration)
    if stage.fingerprint(before) != args.expected_context:
        raise ValueError('Source/model/runtime inputs differ from the explicitly frozen context.')
    atomic_json(directory / 'after-manifest.json', before)
    script = here / 'capture-constructor-definitions.wl'
    loaded_hashes = {str(path.relative_to(root)): context.digest(path) for path in (Path(__file__).resolve(), script)}
    records = []
    for index in (1, 2):
        if stage.manifest(root, here, configuration) != before:
            raise ValueError('Inputs changed before a cold capture.')
        started = time.monotonic()
        record = {'Pass': index, 'StartedUTC': datetime.datetime.now(datetime.timezone.utc).isoformat(),
                  'ExternalSeconds': args.seconds, 'LoadedToolSHA256': loaded_hashes}
        env = dict(os.environ, HEPCAT_FINGERPRINT_DIRECTORY=str(directory),
                   HEPCAT_FINGERPRINT_PASS=str(index), HEPCAT_FINGERPRINT_PACKAGE=str(root / 'source/HEPCAT.wl'),
                   OMP_NUM_THREADS='1', OPENBLAS_NUM_THREADS='1')
        with (directory / f'definitions-{index}.log').open('x') as output:
            process = subprocess.Popen([str(args.kernel), '-script', str(script)], cwd=root,
                                       stdout=output, stderr=subprocess.STDOUT, env=env, start_new_session=True)
            try:
                record['ExitCode'] = process.wait(timeout=args.seconds)
            except subprocess.TimeoutExpired:
                os.killpg(process.pid, signal.SIGTERM)
                try:
                    process.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    os.killpg(process.pid, signal.SIGKILL)
                    process.wait()
                record.update(ExitCode=process.returncode, Timeout=True)
        path = directory / f'definitions-{index}-raw.json'
        record.update(Seconds=time.monotonic() - started, OutputExists=path.is_file(),
                      ContextUnchanged=stage.manifest(root, here, configuration) == before,
                      ToolBytesUnchanged=all(context.digest(root / name) == value for name, value in loaded_hashes.items()))
        if path.is_file():
            record.update(RawSHA256=context.digest(path), RawBytes=path.stat().st_size)
        records.append(record)
        atomic_json(directory / 'capture-status.json', {'Records': records, 'ExpectedContextSHA256': args.expected_context})
        print(json.dumps(record), flush=True)
        if record['ExitCode'] != 0 or not all(record[key] for key in ('OutputExists', 'ContextUnchanged', 'ToolBytesUnchanged')):
            raise RuntimeError('A cold capture failed or its frozen inputs changed.')
    context.cold_pair(context.object_at(directory / 'definitions-1-raw.json'),
                      context.object_at(directory / 'definitions-2-raw.json'), require_post=True)
    atomic_json(directory / 'capture-complete.json', {'Status': 'TWO_EXACT_COLD_CAPTURES_ONLY',
                'ExpectedContextSHA256': args.expected_context, 'ValidationRerunPerformed': False,
                'PhysicsCalculationsPerformed': False, 'Sealed': False})
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
