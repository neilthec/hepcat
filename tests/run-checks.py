"""Restore optional validation sources and run HEPCAT's component checks.

The checkout stays compact. Full tests, benchmark support and Help sources
are distributed in bundles and restored only when requested. Changed local
source files are preserved rather than overwritten.
"""

# HEPCAT - High Energy Physics Constructive Amplitude Tool
#
# Copyright (C) 2026 Neil Christensen
#
# This file is part of HEPCAT.
#
# HEPCAT is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# HEPCAT is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with HEPCAT.  If not, see <https://www.gnu.org/licenses/>.

import argparse
import ast
import hashlib
import json
import math
import os
from pathlib import Path, PurePosixPath
import re
import shutil
import stat
import subprocess
import sys
import tempfile
import zipfile

BUNDLES = (
    ("tests/checks/checks.zip", "tests/checks/", ()),
    ("tests/benchmarks/benchmarks.zip", "tests/benchmarks/", ()),
    ("Documentation/HEPCAT-Documentation.zip", "Documentation/", ("PacletInfo.wl",)),
)
MANIFEST = "BUNDLE-MANIFEST.json"


def digest(path):
    value = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            value.update(chunk)
    return value.hexdigest()


def safe_name(name):
    if not isinstance(name, str) or not name or "\\" in name or ":" in name:
        raise ValueError("Unsafe archive path: " + repr(name))
    if any(part in ("", ".", "..") for part in name.split("/")) or PurePosixPath(name).is_absolute():
        raise ValueError("Unsafe archive path: " + repr(name))
    return name


def destination(root, name):
    target = root.joinpath(*safe_name(name).split("/"))
    cursor = target
    while cursor != root:
        if cursor.is_symlink():
            raise ValueError("Refusing a symbolic link in the destination: " + str(cursor))
        cursor = cursor.parent
    return target


def _stage_bundle(root, spec, staging, start):
    relative, prefix, exceptions = spec
    archive = root / relative
    if not archive.is_file():
        raise ValueError("Required validation bundle is missing: " + str(archive))
    plan = []
    with zipfile.ZipFile(archive) as bundle:
        infos = bundle.infolist()
        names = [entry.filename for entry in infos]
        if len(names) != len(set(names)):
            raise ValueError("Duplicate archive members in " + relative)
        if MANIFEST not in names:
            raise ValueError("Missing bundle manifest in " + relative)
        if bundle.getinfo(MANIFEST).file_size > 8 * 1024 * 1024:
            raise ValueError("Bundle manifest is too large: " + relative)
        document = json.loads(bundle.read(MANIFEST))
        rows = document.get("files") if isinstance(document, dict) else None
        if not isinstance(rows, list):
            raise ValueError("Invalid files list in " + relative)
        declared = set()
        for row in rows:
            if not isinstance(row, dict):
                raise ValueError("Invalid file record in " + relative)
            name = safe_name(row.get("path"))
            if name in declared or name == MANIFEST:
                raise ValueError("Duplicate declared member in " + relative + ": " + name)
            declared.add(name)
            if not name.startswith(prefix) and name not in exceptions:
                raise ValueError("Member is outside its bundle scope: " + name)
            size, expected = row.get("bytes"), row.get("sha256")
            if type(size) is not int or size < 0 or not isinstance(expected, str) or not re.fullmatch(r"[0-9a-f]{64}", expected):
                raise ValueError("Invalid size or SHA256 for " + name)
            if name not in names:
                raise ValueError("Declared member is missing: " + name)
            entry = bundle.getinfo(name)
            mode = entry.external_attr >> 16
            if entry.is_dir() or stat.S_ISLNK(mode) or stat.S_IFMT(mode) not in (0, stat.S_IFREG):
                raise ValueError("Unsupported archive member type: " + name)
            if entry.file_size != size:
                raise ValueError("Declared size differs for " + name)
            target = destination(root, name)
            if target.exists() and (not target.is_file() or target.stat().st_size != size or digest(target) != expected):
                raise ValueError("Preserving changed local file; move it aside before unpacking: " + str(target))
            staged = staging / str(start + len(plan))
            value, actual_size = hashlib.sha256(), 0
            with bundle.open(entry) as source, staged.open("wb") as output:
                while True:
                    chunk = source.read(1024 * 1024)
                    if not chunk:
                        break
                    actual_size += len(chunk)
                    if actual_size > size:
                        raise ValueError("Expanded size exceeds declaration for " + name)
                    value.update(chunk)
                    output.write(chunk)
            if actual_size != size or value.hexdigest() != expected:
                raise ValueError("Size or SHA256 mismatch for " + name)
            plan.append((name, staged, size, expected))
        if set(names) != declared | {MANIFEST}:
            raise ValueError("Undeclared archive members in " + relative)
    return plan


def unpack_bundles(root, bundles=BUNDLES):
    """Verify every bundle and existing file before installing restored sources."""
    root = Path(root).resolve()
    with tempfile.TemporaryDirectory(prefix="hepcat-validation-unpack-") as temporary:
        staging, plan = Path(temporary), []
        for spec in bundles:
            plan.extend(_stage_bundle(root, spec, staging, len(plan)))
        names = [row[0] for row in plan]
        if len(names) != len(set(names)):
            raise ValueError("Different bundles declare the same destination")
        created, unchanged = 0, 0
        for name, staged, size, expected in plan:
            target = destination(root, name)
            if target.exists():
                if not target.is_file() or target.stat().st_size != size or digest(target) != expected:
                    raise ValueError("Preserving changed local file: " + str(target))
                unchanged += 1
                continue
            target.parent.mkdir(parents=True, exist_ok=True)
            destination(root, name)
            handle, filename = tempfile.mkstemp(prefix=".hepcat-unpack-", dir=target.parent)
            local = Path(filename)
            try:
                with os.fdopen(handle, "wb") as output, staged.open("rb") as source:
                    shutil.copyfileobj(source, output)
                os.chmod(local, 0o644)
                try:
                    # Atomic creation, without replacing a concurrently created file.
                    os.link(local, target)
                    created += 1
                except FileExistsError:
                    if target.is_symlink() or not target.is_file() or target.stat().st_size != size or digest(target) != expected:
                        raise ValueError("Preserving file created during unpack: " + str(target))
                    unchanged += 1
            finally:
                local.unlink(missing_ok=True)
    return {"files": len(plan), "created": created, "unchanged": unchanged}


def suite_inventory(root):
    path = Path(root) / "tests/checks/run-wolfram-tests.py"
    if path.is_file():
        source = path.read_text()
    else:
        with zipfile.ZipFile(Path(root) / "tests/checks/checks.zip") as bundle:
            source = bundle.read("tests/checks/run-wolfram-tests.py").decode("utf-8")
    values = {}
    for item in ast.parse(source).body:
        if isinstance(item, ast.Assign):
            for target in item.targets:
                if isinstance(target, ast.Name) and target.id in ("TEST_SUITES", "INTEGRATION_TEST_SUITES"):
                    values[target.id] = ast.literal_eval(item.value)
    if len(values) != 2:
        raise ValueError("Wolfram suite inventory is incomplete")
    return values["TEST_SUITES"], values["INTEGRATION_TEST_SUITES"]


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    modes = parser.add_mutually_exclusive_group()
    modes.add_argument("--unpack-only", action="store_true", help="Restore inspectable sources without running tests.")
    modes.add_argument("--python-only", action="store_true", help="Run the Python component tests only.")
    modes.add_argument("--wolfram-only", action="store_true", help="Run standard or explicitly selected Wolfram suites only.")
    modes.add_argument("--list", action="store_true", help="List default and opt-in Wolfram suites without unpacking.")
    parser.add_argument("--kernel", help="Path to a licensed WolframKernel executable.")
    parser.add_argument("--timeout", type=float, default=900, help="Hard limit in seconds for each Wolfram suite (default: 900).")
    parser.add_argument("--suite", action="append", help="Select a Wolfram suite; repeat to select several. Full constructions are opt-in.")
    args = parser.parse_args(argv)
    if not math.isfinite(args.timeout) or args.timeout <= 0:
        parser.error("--timeout must be finite and positive")
    if args.suite and (args.python_only or args.unpack_only):
        parser.error("--suite selects Wolfram tests; do not combine with --python-only or --unpack-only")
    root = Path(__file__).resolve().parent.parent
    try:
        defaults, integrations = suite_inventory(root)
        if args.suite and any(name not in defaults + integrations for name in args.suite):
            parser.error("Unknown --suite; use --list to see the available names")
        if args.list:
            print("Default Wolfram suites (" + str(len(defaults)) + "):")
            print("\n".join(defaults))
            print("\nOpt-in integration suites (" + str(len(integrations)) + "):")
            print("\n".join(integrations))
            return 0
        result = unpack_bundles(root)
        print("Validation sources verified: {files} files; {created} restored, {unchanged} unchanged.".format(**result), flush=True)
        if args.unpack_only:
            return 0
        if not args.wolfram_only:
            print("Running Python component tests.", flush=True)
            code = subprocess.call([sys.executable, "-m", "unittest", "discover", "-s", str(root / "tests/checks"), "-p", "test_*.py", "-v"], cwd=root)
            if code:
                return code
        if not args.python_only:
            kernel = args.kernel or shutil.which("WolframKernel")
            if not kernel:
                candidates = ["/Applications/Wolfram.app/Contents/MacOS/WolframKernel", "/Applications/Mathematica.app/Contents/MacOS/WolframKernel", "/usr/local/Wolfram/Wolfram/15.0/Executables/WolframKernel", "/usr/local/Wolfram/Mathematica/14.0/Executables/WolframKernel"]
                kernel = next((item for item in candidates if Path(item).is_file()), None)
            if not kernel:
                raise ValueError("WolframKernel was not found. Supply --kernel /path/to/WolframKernel, or use --python-only.")
            command = [sys.executable, str(root / "tests/checks/run-wolfram-tests.py"), "--kernel", kernel, "--timeout", str(args.timeout)]
            for name in args.suite or ():
                command.extend(["--suite", name])
            print("Running Wolfram suites with a per-suite limit of " + str(args.timeout) + " s.", flush=True)
            return subprocess.call(command, cwd=root)
        return 0
    except (OSError, ValueError, KeyError, zipfile.BadZipFile) as error:
        print("HEPCAT checks: " + str(error), file=sys.stderr)
        return 1
    except KeyboardInterrupt:
        return 130


if __name__ == "__main__":
    sys.exit(main())
