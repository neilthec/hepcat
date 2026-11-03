#!/usr/bin/env python3
"""Run native LO MadEvent with an exported HEPCAT SPINAS matrix element.

MadGraph supplies the process topology, phase-space maps, cards, integration and
unweighted-event machinery. MATRIX is replaced before compilation; its only
amplitude evaluation is the exported SPINAS C interface. The independent MG
validation matrix remains a different project and is never replaced.
"""

import argparse
import contextlib
import hashlib
import importlib.util
import json
import math
import os
import platform
from pathlib import Path
import re
import shlex
import shutil
import subprocess
import sys
import tempfile
import time

sys.dont_write_bytecode = True


class MadEventSetupError(RuntimeError):
    """An unsupported or unverifiable numerical setup, never a zero weight."""


def reference_runtime():
    path = Path(__file__).with_name("madgraph-runtime.py")
    spec = importlib.util.spec_from_file_location("hepcat_mg_validation_runtime", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def finite_real(value, name):
    if isinstance(value, bool) or not isinstance(value, (int, float)) or not math.isfinite(value):
        raise MadEventSetupError(name + " must be a finite real number.")
    return float(value)


def validate_config(data):
    """Validate export metadata without importing, changing, or running MG."""
    if not isinstance(data, dict):
        raise MadEventSetupError("The MadEvent configuration must be a JSON object.")
    config = dict(data)
    for field in ("MadGraphDirectory", "MadGraphModel", "ProcessDirectory", "BridgeLinkFile"):
        value = config.get(field)
        if not isinstance(value, str) or not value.strip() or any(c in value for c in "\n\r\x00"):
            raise MadEventSetupError(field + " must be explicitly specified.")
    pdgs = config.get("PhysicalPDGs")
    if not isinstance(pdgs, list) or len(pdgs) < 4 or any(
            isinstance(p, bool) or not isinstance(p, int) or p == 0 for p in pdgs):
        raise MadEventSetupError("MadEvent supports an explicit colorless LO two-to-N process.")
    count = len(pdgs)
    order = config.get("LegOrder", list(range(1, count + 1)))
    if not isinstance(order, list) or any(type(i) is not int for i in order) or sorted(order) != list(range(1, count + 1)):
        raise MadEventSetupError("LegOrder must be a one-based permutation of all external legs.")
    spins = config.get("Spin2")
    if not isinstance(spins, list) or len(spins) != count or any(type(s) is not int or s not in (0, 1, 2) for s in spins):
        raise MadEventSetupError("Spin2 must give supported scalar, fermion or vector spins for all original legs.")
    masses = config.get("Masses")
    if not isinstance(masses, list) or len(masses) != count:
        raise MadEventSetupError("Masses must specify the original ordered external masses.")
    masses = [finite_real(m, "External mass") for m in masses]
    if any(m < 0 for m in masses):
        raise MadEventSetupError("External masses must be nonnegative.")
    physical_spins = [spins[i - 1] for i in order]
    physical_masses = [masses[i - 1] for i in order]
    full_divisor = math.prod((1 if s == 0 else 3 if s == 2 and m > 0 else 2)
                            for s, m in zip(physical_spins[:2], physical_masses[:2]))
    divisor = finite_real(config.get("InitialSpinAverage", full_divisor), "InitialSpinAverage")
    if divisor <= 0 or divisor > full_divisor:
        raise MadEventSetupError("InitialSpinAverage must be a positive incoming state-count divisor.")
    symmetry = 1.0 / math.prod(math.factorial(pdgs[2:].count(p)) for p in set(pdgs[2:]))
    supplied_symmetry = finite_real(config.get("SymmetryFactor", symmetry), "SymmetryFactor")
    if not math.isclose(symmetry, supplied_symmetry, rel_tol=1e-12):
        raise MadEventSetupError("SymmetryFactor differs from MadEvent's identical-final-species factorial; it must be applied exactly once.")
    names = config.get("ParameterNames", [r["Name"] for r in config.get("Parameters", [])])
    defaults = config.get("ParameterDefaults", [r["Value"] for r in config.get("Parameters", [])])
    if not isinstance(names, list) or not isinstance(defaults, list) or len(names) != len(defaults):
        raise MadEventSetupError("ParameterNames and ParameterDefaults must have matching ordered entries.")
    if any(not isinstance(n, str) or not re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", n) for n in names) or len(set(n.lower() for n in names)) != len(names):
        raise MadEventSetupError("Used parameter names must be unique model identifiers.")
    defaults = [finite_real(v, n) for n, v in zip(names, defaults)]
    for key in ("ParameterMap", "ParameterRules"):
        if key in config and not isinstance(config[key], dict):
            raise MadEventSetupError(key + " must be a JSON object.")
    seconds = finite_real(config.get("TimeConstraint", 900), "TimeConstraint")
    if seconds <= 0:
        raise MadEventSetupError("TimeConstraint must be positive.")
    configurations = config.get("HelicityConfigurations", [])
    if not isinstance(configurations, list) or any(not isinstance(row, list) or len(row) != count for row in configurations):
        raise MadEventSetupError("HelicityConfigurations must list complete original-order spin selections.")
    for row in configurations:
        for index, value in enumerate(row):
            permitted = (0,) if masses[index] > 0 or spins[index] == 0 else (-spins[index], spins[index])
            if type(value) is not int or value not in permitted:
                raise MadEventSetupError("HelicityConfigurations must use doubled massless helicities and zero wildcards for all massive/scalar legs.")
    fixed_incoming = []
    if configurations:
        for leg in range(2):
            original = order[leg] - 1
            if physical_masses[leg] == 0 and physical_spins[leg] != 0 and len({row[original] for row in configurations}) == 1:
                fixed_incoming.append(leg + 1)
    expected_divisor = full_divisor / (2 ** len(fixed_incoming))
    if configurations and not math.isclose(divisor, expected_divisor):
        raise MadEventSetupError("InitialSpinAverage does not match the selected incoming helicities.")
    config.update(LegOrder=order, Masses=masses, ParameterNames=names, ParameterDefaults=defaults,
                  Parameters=[{"Name": n, "Value": v} for n, v in zip(names, defaults)],
                  InitialSpinAverage=divisor, SymmetryFactor=symmetry, TimeConstraint=seconds,
                  _PhysicalSpin2=physical_spins, _PhysicalMasses=physical_masses,
                  _MGInitialSpinDivisor=full_divisor,
                  _MassiveLegs=[i + 1 for i, m in enumerate(physical_masses) if m > 0],
                  _FixedIncomingLegs=fixed_incoming)
    return config


def mapconfig_representatives(source):
    entries = {int(i): int(value) for i, value in re.findall(
        r"DATA\s+MAPCONFIG\(\s*(\d+)\s*\)\s*/\s*(\d+)\s*/", source, re.I)}
    count = entries.get(0, 0)
    reps = [entries.get(i) for i in range(1, count + 1)]
    if count <= 0 or any(i is None or i <= 0 for i in reps) or len(set(reps)) != count:
        raise MadEventSetupError("MadEvent MAPCONFIG must have distinct positive representatives for every active phase-space channel.")
    return reps


def matrix_hook(source, config, parameter_expressions, mapconfig=None):
    """Replace the raw MATRIX body while retaining native SMATRIX normalization."""
    match = re.search(r"(?im)^\s*(?:REAL\*8|DOUBLE\s+PRECISION)\s+FUNCTION\s+MATRIX\s*\(\s*P\s*,\s*NHEL\s*,\s*IC\s*,\s*IVEC\s*\)\s*$", source)
    if not match or re.search(r"(?im)^\s*(?:REAL\*8|DOUBLE\s+PRECISION)\s+FUNCTION\s+MATRIX\s*\(", source[match.end():]):
        raise MadEventSetupError("Unsupported MadEvent MATRIX signature; expected MATRIX(P,NHEL,IC,IVEC).")
    end = re.search(r"(?im)^ {0,6}END\s*$", source[match.end():])
    if not end:
        raise MadEventSetupError("The MadEvent MATRIX routine has no identifiable END.")
    reps = list(mapconfig if mapconfig is not None else config.get("_MapConfigs", []))
    if not reps or any(type(i) is not int or i < 1 for i in reps) or len(set(reps)) != len(reps):
        raise MadEventSetupError("Unique active MAPCONFIG representatives are required.")
    if len(parameter_expressions) != len(config.get("ParameterNames", [])) or any(
            not re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", p) for p in parameter_expressions):
        raise MadEventSetupError("Every used parameter needs a verified real UFO Fortran symbol.")
    prefix = source[:match.start()]
    # Sampling/pruning in an unrelated HELAS helicity basis must not determine
    # which exported massive-spin configurations are evaluated.
    goodhel = r"IF\s*\(GOODHEL\(I\)\s*\.OR\.\s*NTRY\s*\.LE\.\s*MAXTRIES\.OR\.\(ISUM_HEL\.NE\.0\)\)\s*\n\s*\$\s*THEN"
    requires_full_spin_guard = "ISUM_HEL" in prefix.upper() or "GOODHEL" in prefix.upper()
    prefix, pruning_count = re.subn(goodhel, "IF (.TRUE.) THEN", prefix, flags=re.I)
    begin = re.search(r"(?im)^\s*NTRY\s*=\s*NTRY\s*\+\s*1\s*$", prefix)
    if requires_full_spin_guard and (pruning_count != 1 or begin is None):
        raise MadEventSetupError("Unsupported SMATRIX helicity-pruning layout; refusing an incomplete spin sum.")
    if begin:
        prefix = prefix[:begin.start()] + "\nC     HEPCAT requires the complete spin sum, with no helicity sampling.\n      IF (ISUM_HEL.NE.0) THEN\n        WRITE(*,*) 'HEPCAT MadEvent requires nhel=0.'\n        STOP 1\n      ENDIF\n" + prefix[begin.start():]
    scale = float(config.get("_MGInitialSpinDivisor", config["InitialSpinAverage"])) / float(config["InitialSpinAverage"])
    assignments = "\n".join("      PARAMETERS(%d)=%s" % (i + 1, p) for i, p in enumerate(parameter_expressions))
    guards = ""
    for leg in sorted(set(config.get("_MassiveLegs", [])) & {1, 2} | set(config.get("_FixedIncomingLegs", []))):
        guards += "      IF (POL(%d).NE.1D0) THEN\n        WRITE(*,*) 'Unsupported HEPCAT beam polarization, leg %d.'\n        STOP 1\n      ENDIF\n" % (leg, leg)
    weights = "\n".join("      AMP2(%d)=AMP2(%d)+MATRIX" % (i, i) for i in reps)
    body = """
      REAL*8 FUNCTION MATRIX(P,NHEL,IC,IVEC)
C     HEPCAT SPINAS numerical amplitude; no HELAS calls in this routine.
      USE ISO_C_BINDING
      IMPLICIT NONE
      INCLUDE '../../Source/vector.inc'
      INCLUDE 'nexternal.inc'
      INCLUDE 'maxamps.inc'
      INCLUDE 'coupl.inc'
      INCLUDE '../../Source/MODEL/input.inc'
      REAL(C_DOUBLE) P(0:3,NEXTERNAL),RAW
      REAL(C_DOUBLE) PARAMETERS(%(nparams)d)
      INTEGER(C_INT) NHEL(NEXTERNAL),STATUS
      INTEGER IC(NEXTERNAL),IVEC,I
      DOUBLE PRECISION AMP2(MAXAMPS),JAMP2(0:MAXFLOW),POL(2)
      COMMON/TO_AMPS/AMP2,JAMP2
      COMMON/TO_POLARIZATION/POL
      TYPE(C_PTR) ERROR_POINTER
      CHARACTER(KIND=C_CHAR),POINTER :: ERROR_TEXT(:)
      INTERFACE
        INTEGER(C_INT) FUNCTION HEPCAT_MATRIX(P,H,PARAM,ANSWER)
     $    BIND(C,NAME='hepcat_madevent_matrix')
          USE ISO_C_BINDING
          REAL(C_DOUBLE),INTENT(IN) :: P(*),PARAM(*)
          INTEGER(C_INT),INTENT(IN) :: H(*)
          REAL(C_DOUBLE),INTENT(OUT) :: ANSWER
        END FUNCTION
        TYPE(C_PTR) FUNCTION HEPCAT_ERROR()
     $    BIND(C,NAME='hepcat_madevent_error')
          USE ISO_C_BINDING
        END FUNCTION
      END INTERFACE
%(guards)s%(assignments)s
      STATUS=HEPCAT_MATRIX(P,NHEL,PARAMETERS,RAW)
      IF (STATUS.NE.0) THEN
        WRITE(*,*) 'HEPCAT SPINAS matrix evaluation failed:'
        ERROR_POINTER=HEPCAT_ERROR()
        IF (C_ASSOCIATED(ERROR_POINTER)) THEN
          CALL C_F_POINTER(ERROR_POINTER,ERROR_TEXT,[4096])
          DO I=1,4096
            IF (ERROR_TEXT(I).EQ.C_NULL_CHAR) EXIT
            WRITE(*,'(A)',ADVANCE='NO') ERROR_TEXT(I)
          ENDDO
          WRITE(*,*)
        ENDIF
        STOP 1
      ENDIF
C     Native SMATRIX applies its incoming average and final factorial once.
      MATRIX=RAW*%(scale)s
C     Uniform positive partition over the active native phase-space maps.
%(weights)s
      JAMP2(1)=JAMP2(1)+MATRIX
      END
""" % {"nparams": max(1, len(parameter_expressions)), "guards": guards, "assignments": assignments,
           "scale": ("%.17e" % scale).replace("e", "D"), "weights": weights}
    return prefix + body + source[match.end() + end.end():]


def fortran_parameter_expressions(process, names, matches):
    """Resolve current model COMMON symbols, including derived/run couplings."""
    inputs = (process / "Source" / "MODEL" / "input.inc").read_text()
    couplings = (process / "Source" / "MODEL" / "coupl.inc").read_text()
    # Join fixed-form continuations before parsing complete declarations.
    text = re.sub(r"\n\s*\$\s*", "", inputs + "\n" + couplings)
    real_symbols, complex_symbols = set(), set()
    for line in text.splitlines():
        match = re.match(r"\s*(DOUBLE\s+PRECISION|REAL\*8|DOUBLE\s+COMPLEX|COMPLEX\*16)\s+(.+)", line, re.I)
        if match:
            target = complex_symbols if "COMPLEX" in match.group(1).upper() else real_symbols
            target.update(n.upper() for n in re.findall(r"\b[A-Za-z_][A-Za-z0-9_]*\b", match.group(2)))
    by_name = {row["HEPCAT"].lower(): row["MadGraph"] for row in matches}
    result = []
    for source in names:
        name = by_name.get(source.lower())
        if name is None:
            raise MadEventSetupError("No verified runtime UFO parameter for " + source)
        candidates = [name.upper(), "MDL_" + name.upper()]
        found = [n for n in candidates if n in real_symbols]
        if len(found) != 1:
            if any(n in complex_symbols for n in candidates):
                raise MadEventSetupError("The SPINAS MadEvent bridge requires real parameters; " + name + " is complex.")
            raise MadEventSetupError("Cannot identify an unambiguous live real UFO symbol for " + name)
        result.append(found[0])
    return result


def patch_subprocess(path, config, parameter_expressions):
    """Patch one private, ungrouped colorless MadEvent subprocess."""
    path = Path(path)
    matrix = path / "matrix.f"
    original = matrix.read_text()
    reps = mapconfig_representatives((path / "configs.inc").read_text())
    ngraphs = re.search(r"PARAMETER\s*\(\s*NDIAGS\s*=\s*(\d+)\s*\)", original, re.I)
    colors = re.search(r"NCOLOR\s*=\s*(\d+)", original, re.I)
    iden = re.search(r"DATA\s+IDEN\s*/\s*(\d+)\s*/", original, re.I)
    if not ngraphs or max(reps) > int(ngraphs.group(1)) or not colors or int(colors.group(1)) != 1:
        raise MadEventSetupError("Only ungrouped, colorless LO MadEvent matrices with valid phase-space representatives are supported.")
    expected_iden = config["_MGInitialSpinDivisor"] / config["SymmetryFactor"]
    if not iden or not math.isclose(int(iden.group(1)), expected_iden):
        raise MadEventSetupError("Native MadEvent IDEN does not match the exported incoming-spin and identical-particle normalization.")
    ids = re.findall(r"DATA\s*\(IDUP\(I,1,1\),I=1,\s*\d+\)\s*/([^/]+)/", (path / "leshouche.inc").read_text(), re.I)
    if len(ids) != 1 or [int(i) for i in ids[0].split(",")] != config["PhysicalPDGs"]:
        raise MadEventSetupError("MadEvent reordered external PDGs; refusing an unverified momentum/helicity mapping.")
    backup = matrix.with_name("matrix.f.hepcat-original")
    if backup.exists():
        raise MadEventSetupError("The generated matrix was already patched.")
    backup.write_text(original)
    matrix.write_text(matrix_hook(original, config, parameter_expressions, reps))
    return {"Subprocess": path.name, "MapConfigurations": reps,
            "MGNormalizationDivisor": int(iden.group(1)), "RawSpinMultiplier": config["_MGInitialSpinDivisor"] / config["InitialSpinAverage"]}


def set_card_value(path, key, value):
    text = path.read_text()
    pattern = r"(?im)^\s*[^!\n=]+\s*=\s*" + re.escape(key) + r"\b[^\n]*$"
    line = "  " + str(value) + " = " + key + " ! HEPCAT SPINAS numerical integration"
    if re.search(pattern, text):
        text = re.sub(pattern, lambda _: line, text)
    else:
        text += "\n" + line + "\n"
    path.write_text(text)


def patch_event_spins(process, config):
    path = process / "SubProcesses" / "unwgt.f"
    text = path.read_text()
    anchor = "         jpart(7,isym(i, jsym)) = nhel(i)"
    if anchor not in text:
        raise MadEventSetupError("Unsupported MadEvent event-helicity writer.")
    # A full sum is basis-independent. Individual massive SPINAS spin labels
    # have not been identified with MG helicities and must not be advertised.
    conditions = ".OR.".join("I.EQ." + str(i) for i in config["_MassiveLegs"])
    if conditions:
        text = text.replace(anchor, anchor + "\nC        Massive SPINAS spin basis is not an MG helicity label.\n         IF (" + conditions + ") jpart(7,isym(i,jsym))=9", 1)
        path.write_text(text)


def install_runtime_guard(process):
    """Protect direct bin/madevent launches as well as the HEPCAT launcher."""
    guard = process / "bin" / "internal" / "hepcat_madevent_guard.py"
    guard.write_text(r'''"""HEPCAT matrix-integrity and supported-run guards."""
import hashlib
import json
import re
from pathlib import Path


def fail(interface, message):
    (Path(interface.me_dir) / 'hepcat-native-error.json').write_text(json.dumps({'Error': message}))
    raise interface.InvalidCmd(message)


def check(interface):
    root = Path(interface.me_dir)
    report = json.loads((root / 'hepcat-madevent.json').read_text())
    for row in report['MatrixHashes']:
        path = root / row['Path']
        if not path.is_file() or hashlib.sha256(path.read_bytes()).hexdigest() != row['SHA256']:
            fail(interface, 'HEPCAT MATRIX was changed or regenerated. Re-export before running.')
    for row in report.get('Bundle', {}).get('LibraryHashes', []):
        path = root / row['Path']
        if root.resolve() not in path.resolve().parents or not path.is_file() or hashlib.sha256(path.read_bytes()).hexdigest() != row['SHA256']:
            fail(interface, 'A bundled HEPCAT/SPINAS library is missing or changed: ' + str(path))
    card = getattr(interface, 'run_card', None)
    if card is not None:
        # MG RunCard is a ConfigFile, not dict.get(key, default).
        def value(key, default):
            return card[key] if key in card else default
        if int(value('nhel', 0)) != 0:
            fail(interface, 'HEPCAT MadEvent requires nhel=0, the complete spin sum.')
        if value('hel_recycling', False):
            fail(interface, 'HEPCAT MadEvent does not use HELAS helicity recycling.')
        for leg in report['UnsupportedPolarizationLegs']:
            if float(value('polbeam' + str(leg), 0)) != 0:
                fail(interface, 'Polarized massive beams or double-selected fixed helicities are unsupported.')


def install(cls):
    for name in ('do_survey', 'do_refine', 'do_generate_events'):
        original = getattr(cls, name)
        def guarded(self, *args, _original=original, **kwargs):
            check(self)
            return _original(self, *args, **kwargs)
        setattr(cls, name, guarded)
    for name in ('do_decay_events', 'do_madspin', 'do_reweight'):
        def no_spin_correlations(self, line='', *args, _kind=name, **kwargs):
            # A standard generate_events run calls these even when switched
            # off. Allow only those explicit no-ops, never an actual adapter.
            if _kind == 'do_reweight' and re.search(r'(?:^|\s)--mode=OFF(?:\s|$)', line, re.I):
                return
            card = 'reweight_card.dat' if _kind == 'do_reweight' else 'madspin_card.dat'
            if '-from_cards' in line and not (Path(self.me_dir) / 'Cards' / card).is_file():
                return
            fail(self, 'MadSpin and reweighting require additional spin/color adapters; they are unsupported for this HEPCAT backend.')
        setattr(cls, name, no_spin_correlations)


def show_paths():
    root = Path(__file__).resolve().parents[2]
    print('MadEvent process directory: ' + str(root), flush=True)
    print('  HTML:   ' + str(root / 'index.html'), flush=True)
    print('  Events: ' + str(root / 'Events'), flush=True)
    print('  Cards:  ' + str(root / 'Cards'), flush=True)
''')
    launcher = process / "bin" / "madevent"
    text = launcher.read_text()
    anchor = "import internal.madevent_interface as cmd_interface"
    if anchor not in text:
        raise MadEventSetupError("Unsupported native MadEvent launcher.")
    if "hepcat_guard.install(cmd_interface.MadEventCmd)" not in text:
        text = text.replace(anchor, anchor + "\nimport internal.hepcat_madevent_guard as hepcat_guard\nhepcat_guard.install(cmd_interface.MadEventCmd)", 1)
    if "hepcat_guard.show_paths()" not in text:
        text = text.replace("hepcat_guard.install(cmd_interface.MadEventCmd)", "hepcat_guard.install(cmd_interface.MadEventCmd)\nhepcat_guard.show_paths()", 1)
    launcher.write_text(text)
    generator = process / "bin" / "generate_events"
    if generator.is_file():
        text = generator.read_text()
        anchor = "import madevent_interface as ME"
        if anchor not in text:
            raise MadEventSetupError("Unsupported native MadEvent generate_events launcher.")
        if "hepcat_guard.install(ME.MadEventCmd)" not in text:
            text = text.replace(anchor, anchor + "\nimport hepcat_madevent_guard as hepcat_guard\nhepcat_guard.install(ME.MadEventCmd)", 1)
        if "hepcat_guard.show_paths()" not in text:
            text = text.replace("hepcat_guard.install(ME.MadEventCmd)", "hepcat_guard.install(ME.MadEventCmd)\nhepcat_guard.show_paths()", 1)
        generator.write_text(text)


def load_link_configuration(config):
    project = Path(config["ProcessDirectory"]).expanduser().resolve()
    path = Path(config["BridgeLinkFile"])
    if not path.is_absolute():
        path = project / path
    if not path.is_file():
        raise MadEventSetupError("Build the exported SPINAS project first; missing " + str(path))
    data = json.loads(path.read_text())
    libraries = data.get("Libraries", [])
    if not libraries or any(not isinstance(p, str) or not Path(p).is_file() for p in libraries):
        raise MadEventSetupError("BridgeLinkFile must list existing built SPINAS/C++ libraries.")
    if data.get("CXXRuntime") not in ("c++", "stdc++"):
        raise MadEventSetupError("BridgeLinkFile must select the actual C++ runtime.")
    for path in libraries + data.get("RuntimeLibraryDirectories", []):
        if any(c in path for c in "\n\r\x00$#"):
            raise MadEventSetupError("A linked library path contains unsupported Make characters.")
    return data


def bundle_library_sources(link):
    """Prefer SPINAS's static archive without changing ordinary project links."""
    originals = [Path(value) for value in link["Libraries"]]
    preferred = link.get("SPINASStaticLibrary")
    if preferred and Path(preferred).is_file():
        originals[-1] = Path(preferred)
    elif originals[-1].suffix != ".a":
        candidate = originals[-1].parent / "libspinas.a"
        if candidate.is_file():
            originals[-1] = candidate
    return originals


def library_source_hashes(link):
    return [{"Name": path.name, "SHA256": hashlib.sha256(path.read_bytes()).hexdigest()}
            for path in bundle_library_sources(link)]


def bundle_dependencies(process, config, link):
    """Copy the amplitude and SPINAS dependencies into a native process."""
    process = Path(process)
    project = Path(config["ProcessDirectory"]).expanduser().resolve()
    library_directory = process / "lib" / "hepcat"
    source_directory = process / "Source" / "HEPCAT"
    library_directory.mkdir(parents=True, exist_ok=True)
    source_directory.mkdir(parents=True, exist_ok=True)
    originals = bundle_library_sources(link)

    libraries, hashes = [], []
    shared = False
    for original in originals:
        destination = library_directory / original.name
        if destination.is_symlink():
            destination.unlink()
        if not destination.is_file() or hashlib.sha256(destination.read_bytes()).digest() != hashlib.sha256(original.read_bytes()).digest():
            shutil.copy2(original, destination)
            # A content change must force native relinking even for archives
            # delivered with an old or reproducible source timestamp.
            destination.touch()
        if destination.suffix != ".a":
            shared = True
            destination.chmod(destination.stat().st_mode | 0o200)
            if sys.platform == "darwin":
                tool = shutil.which("install_name_tool")
                if not tool:
                    raise MadEventSetupError("A shared SPINAS library needs install_name_tool for a movable MadEvent process; alternatively build libspinas.a.")
                subprocess.run([tool, "-id", "@rpath/" + destination.name, str(destination)], check=True)
                signer = shutil.which("codesign")
                if signer:
                    subprocess.run([signer, "--force", "--sign", "-", str(destination)], check=True,
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            else:
                reader = shutil.which("readelf")
                if not reader:
                    raise MadEventSetupError("A shared SPINAS library needs readelf to verify relocation, or provide libspinas.a.")
                dynamic = subprocess.check_output([reader, "-d", str(destination)], text=True)
                match = re.search(r"\(SONAME\).*?\[([^\]]+)\]", dynamic)
                if match and "/" in match.group(1):
                    patcher = shutil.which("patchelf")
                    if not patcher:
                        raise MadEventSetupError("The shared SPINAS library has an absolute SONAME. Provide libspinas.a or patchelf to make it relocatable.")
                    subprocess.run([patcher, "--set-soname", destination.name, str(destination)], check=True)
                elif match and match.group(1) != destination.name:
                    alias = library_directory / match.group(1)
                    if Path(match.group(1)).name != match.group(1):
                        raise MadEventSetupError("Invalid shared-library SONAME.")
                    shutil.copy2(destination, alias)
                    hashes.append({"Path": str(alias.relative_to(process)), "SHA256": hashlib.sha256(alias.read_bytes()).hexdigest()})
        relative = str(destination.relative_to(process))
        libraries.append(relative)
        hashes.append({"Path": relative, "SHA256": hashlib.sha256(destination.read_bytes()).hexdigest()})

    headers = link.get("SPINASIncludeDirectory")
    candidates = ([Path(headers)] if headers else []) + [
        originals[-1].parent.parent / "include",
        originals[-1].parent.parent / "include" / "spinas"]
    include = next((path for path in candidates if (path / "spinas.h").is_file()), None)
    if include is None:
        raise MadEventSetupError("Cannot find SPINAS headers to package. Reconfigure the export with SPINASIncludeDirectory in its link metadata.")
    shutil.copytree(include, source_directory / "spinas", dirs_exist_ok=True,
                    ignore=shutil.ignore_patterns("__pycache__", "*.pyc", "._*"))
    for path in sorted(list(project.glob("*.cpp")) + list(project.glob("*.hpp"))):
        shutil.copy2(path, source_directory / path.name)
    # These are generator/provenance tools, not required by bin/madevent.
    for name in ("madevent-runtime.py", "madgraph-runtime.py"):
        if (project / name).is_file():
            shutil.copy2(project / name, source_directory / name)
    metadata = {key: value for key, value in config.items()
                if not key.startswith("_") and key not in
                ("ProcessDirectory", "BridgeLibrary", "BridgeLinkFile", "Executable")}
    (source_directory / "amplitude.json").write_text(json.dumps(metadata, indent=2) + "\n")
    sources = [str(path.relative_to(process)) for path in sorted(source_directory.rglob("*")) if path.is_file()]
    local = {"Libraries": libraries, "CXXRuntime": link["CXXRuntime"],
             "RuntimeLibraryDirectories": ["lib/hepcat"] if shared else [],
             "RelativeTo": "MadEventDirectory"}
    bundle = {"Version": 1, "Libraries": libraries, "LibraryHashes": hashes,
              "SourceLibraryHashes": library_source_hashes(link), "Sources": sources}
    (source_directory / "link.json").write_text(json.dumps(local, indent=2) + "\n")
    card = process / "Cards" / "proc_card_mg5.dat"
    if card.is_file() and (process / "bin" / "internal" / "ufomodel").is_dir():
        text = card.read_text().replace("\\\n", "")
        text = re.sub(r"(?m)^import model [^\n]+$", "import model ./bin/internal/ufomodel-full", text)
        card.write_text(text)
    return bundle, local


def patch_linking(process, link):
    # MG's legacy template can request macOS 10.8 even when the linked C++
    # archives use the current SDK's stack probes. Match the private numerical
    # program to this host, without changing MG or the user's SPINAS build.
    if sys.platform == "darwin":
        version = ".".join(platform.mac_ver()[0].split(".")[:2])
        if not re.fullmatch(r"\d+\.\d+", version):
            raise MadEventSetupError("Cannot establish the macOS link target.")
        options = process / "Source" / "make_opts"
        options.write_text(re.sub(r"(?m)^MACFLAG=.*$", "MACFLAG=-mmacosx-version-min=" + version, options.read_text()))
    relative = link.get("RelativeTo") == "MadEventDirectory"
    libraries = [("../../" + path) if relative else path for path in link["Libraries"]]
    flags = []
    for path in libraries:
        if relative and sys.platform != "darwin" and not path.endswith(".a"):
            flags += ["-L" + shlex.quote(str(Path(path).parent)), "-l:" + shlex.quote(Path(path).name)]
        else:
            flags.append(shlex.quote(path))
    flags.append("-l" + link["CXXRuntime"])
    if relative and link.get("RuntimeLibraryDirectories"):
        if sys.platform == "darwin":
            flags.append("-Wl,-rpath,@loader_path/../../lib/hepcat")
        else:
            flags.append("-Wl,-rpath,'$$ORIGIN/../../lib/hepcat'")
    elif not relative:
        flags += [shlex.quote("-Wl,-rpath," + p) for p in link.get("RuntimeLibraryDirectories", [])]
    path = process / "SubProcesses" / "makefile"
    text = path.read_text()
    text = re.sub(r"\n# HEPCAT: link the exported amplitude after native objects and libraries\.\n"
                  r"(?:FFLAGS[^\n]*\n)?LINKLIBS[^\n]*\n\$\(PROG\):[^\n]*\n?", "\n", text)
    text += "\n# HEPCAT: link the exported amplitude after native objects and libraries.\n"
    text += "FFLAGS += -ffixed-line-length-none\n"
    text += "LINKLIBS += " + " ".join(flags) + "\n"
    text += "$(PROG): " + " ".join(p.replace(" ", "\\ ") for p in libraries) + "\n"
    path.write_text(text)


def configuration_fingerprint(config, link=None):
    keys = ("MadGraphDirectory", "MadGraphModel", "PhysicalPDGs", "AllowedParticlePDGs", "LegOrder", "Spin2", "Masses", "ParameterNames", "ParameterDefaults", "ParameterMap", "ParameterRules", "InitialSpinAverage", "SymmetryFactor", "HelicityConfigurations")
    payload = {k: config.get(k) for k in keys}
    payload["LinkConfiguration"] = link
    return hashlib.sha256(json.dumps(payload, sort_keys=True).encode()).hexdigest()


def prepare(data, root=None):
    """Build MadEvent under the compiled export, retaining its cards.

    New exports use the directory containing BridgeLinkFile as their default
    numerical workspace. An explicit root overrides it; older configurations
    retain their launch-directory default.
    """
    config = validate_config(data)
    project = Path(config["ProcessDirectory"]).expanduser().resolve()
    if not project.is_dir():
        raise MadEventSetupError("ProcessDirectory must be an existing exported SPINAS project.")
    link = load_link_configuration(config)
    default_workspace = Path.cwd()
    if config.get("SessionDirectoryBase") == "BuildDirectory":
        link_path = Path(config["BridgeLinkFile"]).expanduser()
        if not link_path.is_absolute():
            link_path = project / link_path
        default_workspace = link_path.resolve().parent
    workspace = Path(root if root is not None else default_workspace).expanduser().resolve()
    workspace.mkdir(parents=True, exist_ok=True)
    session = config.get("SessionDirectory", "madevent")
    if not isinstance(session, str) or not re.fullmatch(r"[A-Za-z0-9_.-]+", session) or session in (".", ".."):
        raise MadEventSetupError("SessionDirectory must be a simple process-directory name.")
    final = workspace / session
    fingerprint = configuration_fingerprint(config, link)
    if final.exists():
        manifest = final / "hepcat-madevent.json"
        if not manifest.is_file():
            raise MadEventSetupError("Refusing to overwrite a MadEvent directory without HEPCAT ownership metadata.")
        report = json.loads(manifest.read_text())
        legacy_link = {key: link[key] for key in ("Libraries", "CXXRuntime", "RuntimeLibraryDirectories") if key in link}
        if report.get("ConfigurationSHA256") not in (fingerprint, configuration_fingerprint(config, legacy_link)):
            raise MadEventSetupError("Export metadata changed or this MadEvent directory belongs to another process: " +
                str(final) + ". Select --work-directory DIR for a separate numerical session.")
        for row in report.get("MatrixHashes", []):
            path = final / row["Path"]
            if not path.is_file() or hashlib.sha256(path.read_bytes()).hexdigest() != row["SHA256"]:
                raise MadEventSetupError("A private MATRIX was modified or regenerated; refusing to run it.")
        previous_sources = report.get("Bundle", {}).get("SourceLibraryHashes")
        if not report.get("Bundle") or (previous_sources is not None and previous_sources != library_source_hashes(link)):
            bundle, local_link = bundle_dependencies(final, config, link)
            patch_linking(final, local_link)
            report.update(Bundle=bundle, LinkConfiguration=local_link, ConfigurationSHA256=fingerprint)
            (final / "hepcat-madevent.json").write_text(json.dumps(report, indent=2) + "\n")
        install_runtime_guard(final)
        return dict(report, MadEventDirectory=str(final), WorkingDirectory=str(workspace))
    support = workspace / ".hepcat-madevent-support"
    if support.exists():
        raise MadEventSetupError("A previous MadEvent preparation is incomplete. Inspect " + str(support) + " before removing or renaming it.")
    support.mkdir()
    deadline = time.monotonic() + config["TimeConstraint"]
    ref = reference_runtime()
    try:
        # The shared validation loader expects physical mass order; this builder
        # config retains the export's original order and states that explicitly.
        physical_config = dict(config, Masses=config["_PhysicalMasses"])
        mg, pdgs, masses, used, rules, name_map, _ = ref.checked_configuration(physical_config)
        compiler = ref.find_compiler(config.get("FortranCompiler"))
        with (support / "model.log").open("w") as log, contextlib.redirect_stdout(log), contextlib.redirect_stderr(log):
            model, model_path, restriction, model_reader, card_module = ref.read_model(mg, config["MadGraphModel"])
            names = []
            for i, pdg in enumerate(pdgs):
                if pdg not in model["particle_dict"]:
                    raise MadEventSetupError("The selected UFO has no external PDG " + str(pdg))
                particle = model["particle_dict"][pdg]
                if abs(particle.get("color")) != 1:
                    raise MadEventSetupError("Colored external processes require a color-flow adapter; only colorless LO processes are supported.")
                if particle.get("spin") != config["_PhysicalSpin2"][i] + 1:
                    raise MadEventSetupError("UFO and SPINAS external particle spins disagree.")
                name = particle.get_name()
                if not re.fullmatch(r"[A-Za-z0-9_~+.\-]+", name):
                    raise MadEventSetupError("Unsupported UFO particle command name: " + name)
                names.append(name)
            allowed = ref.allowed_particle_types(config.get("AllowedParticlePDGs", []), pdgs)
            exclusions = ref.reference_particle_exclusions(model, allowed)
            entries, external, updates, targets, matches = ref.match_parameters(model, used, rules, name_map)
            for target in targets.values():
                if entries[target].type != "real":
                    raise MadEventSetupError("Complex UFO parameter " + entries[target].name + " requires a complex numerical adapter.")
            updates, mass_matching = ref.match_external_masses(model, pdgs, masses, external, updates)
            updates = ref.synchronize_sm_yukawa(model, external, updates, rules)
            orders = ref.complete_tree_orders(model, len(pdgs))
            private_spec = ref.private_ufo_copy(model_path, restriction, support)
        process_string = " ".join(names[:2]) + " > " + " ".join(names[2:])
        generation = process_string + (" / " + " ".join(exclusions) if exclusions else "")
        commands = ["set auto_update 0 --no_save", "set automatic_html_opening False --no_save",
                    "set notification_center False --no_save", "set group_subprocesses False --no_save",
                    "set fortran_compiler " + compiler + " --no_save", "import model " + private_spec,
                    "generate " + generation + " " + " ".join(n + "<=" + str(v) for n, v in sorted(orders.items())),
                    "output madevent process --hel_recycling=False -f -nojpeg", "quit", ""]
        command = support / "generate.mg5"
        command.write_text("\n".join(commands))
        environment = dict(os.environ, PYTHONDONTWRITEBYTECODE="1")
        environment["PATH"] = str(Path(compiler).parent) + os.pathsep + environment.get("PATH", "")
        python = config.get("PythonExecutable") or sys.executable
        print("Preparing native MadEvent phase space for " + process_string, flush=True)
        ref.run_logged([python, mg / "bin" / "mg5_aMC", "-s", command], support, support / "generate.log", deadline, environment)
        process = support / "process"
        matrices = sorted((process / "SubProcesses").glob("P*/matrix.f"))
        if len(matrices) != 1:
            raise MadEventSetupError("Expected exactly one ungrouped fixed-PDG LO subprocess; see " + str(support / "generate.log"))
        with (support / "model.log").open("a") as log, contextlib.redirect_stdout(log), contextlib.redirect_stderr(log):
            card = process / "Cards" / "param_card.dat"
            ref.update_card(card, updates, external, card_module)
            ref.verify_model_values(model, card, masses, pdgs, used, targets, matches, model_reader)
        vector_source = (process / "Source" / "vector.inc").read_text()
        if not re.search(r"VECSIZE_MEMMAX\s*=\s*1\s*\)", vector_source, re.I):
            raise MadEventSetupError("Only scalar MadEvent SIMD width1 is supported by this C interface.")
        expressions = fortran_parameter_expressions(process, config["ParameterNames"], matches)
        patch_reports = [patch_subprocess(matrix.parent, config, expressions) for matrix in matrices]
        bundle, local_link = bundle_dependencies(process, config, link)
        patch_linking(process, local_link)
        patch_event_spins(process, config)
        install_runtime_guard(process)
        for card in ("run_card.dat", "run_card_default.dat"):
            set_card_value(process / "Cards" / card, "nhel", "0")
            set_card_value(process / "Cards" / card, "hel_recycling", ".False.")
        options = process / "Cards" / "me5_configuration.txt"
        options.write_text(options.read_text() + "\n# HEPCAT: serial default; the user can select native multicore mode.\nrun_mode = 0\nnb_core = 1\nautomatic_html_opening = False\nnotification_center = False\n")
        hashes = [{"Path": str(matrix.relative_to(process)), "SHA256": hashlib.sha256(matrix.read_bytes()).hexdigest()} for matrix in matrices]
        report = {"Status": "READY", "Backend": "HEPCAT SPINAS", "Process": process_string,
                  "MadEventDirectory": str(final), "WorkingDirectory": str(workspace),
                  "ExportDirectory": str(project), "ConfigurationSHA256": fingerprint,
                  "ParameterMatching": matches, "RuntimeParameterSymbols": expressions,
                  "LinkConfiguration": local_link, "Bundle": bundle,
                  "MassMatching": mass_matching, "Subprocesses": patch_reports,
                  "MatrixHashes": hashes, "UnsupportedPolarizationLegs": sorted(set(config["_MassiveLegs"]) & {1, 2} | set(config["_FixedIncomingLegs"])),
                  "MassiveLHE_SPINUP": 9, "PhysicalPDGs": pdgs,
                  "Scope": "LO colorless two-to-N; complete spin sums; native MadEvent cards, survey/refine and unweighted LHE. No MadSpin, reweighting, color-flow or NLO adapter.",
                  "WidthPolicy": "Native model card widths remain adjustable; only widths explicitly used in the HEPCAT amplitude change its propagators. No amplitude is reconstructed by MG."}
        ref.write_json(process / "hepcat-madevent.json", report)
        make = shutil.which("make")
        if not make:
            raise MadEventSetupError("make was not found.")
        flags = "-w -fPIC -ffixed-line-length-none"
        make_options = ["FC=" + compiler, "FFLAGS=" + flags]
        ref.run_logged([make, "-j2"] + make_options, process / "Source", support / "build.log", deadline, environment)
        for matrix in matrices:
            ref.run_logged([make, "madevent"] + make_options, matrix.parent, support / "build.log", deadline, environment)
        # Generation used a relative private UFO path. Keep that private source
        # stable, and move only the numerical project into its public location.
        process.rename(final)
        print("MadEvent ready: " + str(final / "bin" / "madevent"), flush=True)
        return report
    except ref.ValidationSetupError as error:
        raise MadEventSetupError(str(error)) from error


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--configuration", required=True, type=Path)
    parser.add_argument("--bridge-link-file", type=Path,
                        help="Use numerical link metadata from this CMake build directory.")
    parser.add_argument("--work-directory", type=Path,
                        help="Create or reuse PROC_<ProcessName>/ under this directory (new exports default to their build directory).")
    parser.add_argument("--prepare", action="store_true", help="Generate and build MadEvent without launching a run.")
    parser.add_argument("--command-file", type=Path, help="Native bin/madevent command script.")
    parser.add_argument("--run", help="Generate unweighted events with this native run name.")
    parser.add_argument("--run-card", type=Path)
    parser.add_argument("--param-card", type=Path)
    parser.add_argument("--timeout", type=float, help="Preparation time budget in seconds.")
    args = parser.parse_args(argv)
    if args.run and args.command_file:
        parser.error("Use --run or --command-file, not both.")
    try:
        config = json.loads(args.configuration.read_text())
        # Amplitude sources belong to the export. Numerical output defaults to
        # its compiled build directory unless --work-directory overrides it.
        base = args.configuration.resolve().parent
        config.setdefault("ProcessDirectory", str(base))
        if isinstance(config["ProcessDirectory"], str):
            source = Path(config["ProcessDirectory"]).expanduser()
            if not source.is_absolute():
                config["ProcessDirectory"] = str((base / source).resolve())
        if args.bridge_link_file is not None:
            config["BridgeLinkFile"] = str(args.bridge_link_file.expanduser().resolve())
        if args.timeout is not None:
            config["TimeConstraint"] = args.timeout
        report = prepare(config, root=args.work_directory)
        process = Path(report["MadEventDirectory"])
        print("MadEvent output directory: " + str(process), flush=True)
        print("  HTML:   " + str(process / "index.html"), flush=True)
        print("  Events: " + str(process / "Events"), flush=True)
        print("  Cards:  " + str(process / "Cards"), flush=True)
        for source, target in ((args.run_card, "run_card.dat"), (args.param_card, "param_card.dat")):
            if source:
                if not source.is_file():
                    raise MadEventSetupError("The supplied card does not exist: " + str(source))
                shutil.copyfile(source, process / "Cards" / target)
        if args.prepare:
            return 0
        command = [config.get("PythonExecutable") or sys.executable, str(process / "bin" / "madevent")]
        if args.command_file:
            command.append(str(args.command_file.resolve()))
        elif args.run:
            if not re.fullmatch(r"[A-Za-z0-9_][A-Za-z0-9_.-]*", args.run):
                raise MadEventSetupError("The run name must contain letters, digits, underscore, dot or hyphen.")
            command += ["generate_events", args.run, "-f"]
        environment = dict(os.environ, PYTHONDONTWRITEBYTECODE="1")
        def error_files():
            return list(process.glob("*debug*")) + [process / "hepcat-native-error.json"]
        debug_before = {str(p): p.stat().st_mtime_ns for p in error_files() if p.is_file()}
        started_ns = time.time_ns()
        status = subprocess.call(command, cwd=process, env=environment)
        if status:
            return status
        if args.run or args.command_file:
            failures = [p for p in error_files() if p.is_file() and
                        p.stat().st_mtime_ns != debug_before.get(str(p))]
            if failures:
                raise MadEventSetupError("Native MadEvent reported an error; see " + ", ".join(str(p) for p in failures))
        if args.run:
            events = list((process / "Events" / args.run).glob("unweighted_events*.lhe*"))
            if not any(p.is_file() and p.stat().st_mtime_ns >= started_ns for p in events):
                raise MadEventSetupError("Native MadEvent returned without new unweighted LHE events for " + args.run + ". Inspect its run log and cards.")
        return 0
    except (MadEventSetupError, OSError, ValueError, KeyError, subprocess.SubprocessError) as error:
        print("HEPCAT MadEvent: " + str(error), file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
