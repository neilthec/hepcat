#!/usr/bin/env python3
"""Build a private MadGraph reference matrix element for HEPCAT validation.

This helper never evaluates a HEPCAT expression. It imports an independently
specified UFO model, generates ordinary MadGraph standalone Fortran, matches
its numerical inputs, and exposes raw fixed-helicity and exhaustive spin-summed
squared amplitudes, without initial averages or final symmetry factors.
Only its private output directory is modified. No optional MG tools are needed.
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
import contextlib
import hashlib
import inspect
import json
import math
import os
from pathlib import Path
import re
import shlex
import shutil
import signal
import subprocess
import sys
import time
import traceback

# Importing reference metadata must not create bytecode files in the MG installation.
sys.dont_write_bytecode = True


class ValidationSetupError(RuntimeError):
    """The reference model or its build could not be verified."""


def real_number(value, label):
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValidationSetupError(label + " must be a real number.")
    result = float(value)
    if not math.isfinite(result):
        raise ValidationSetupError(label + " must be finite.")
    return result


def normalized_name(name):
    if not isinstance(name, str) or not re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", name):
        raise ValidationSetupError("Invalid model parameter name: " + repr(name))
    return re.sub(r"^mdl_", "", name, flags=re.I).lower()


def close_value(left, right, tolerance=2e-10):
    left, right = complex(left), complex(right)
    scale = max(abs(left), abs(right))
    return abs(left - right) <= tolerance * scale


def write_json(path, data):
    path.write_text(json.dumps(data, indent=2, allow_nan=False) + "\n")


def run_logged(arguments, directory, log_file, deadline, environment=None):
    remaining = deadline - time.monotonic()
    if remaining <= 0:
        raise ValidationSetupError("The MadGraph preparation time limit expired.")
    with log_file.open("a") as log:
        log.write("Command: " + shlex.join([str(x) for x in arguments]) + "\n")
        log.flush()
        process = subprocess.Popen(
            [str(x) for x in arguments], cwd=str(directory), env=environment,
            stdin=subprocess.DEVNULL, stdout=log, stderr=subprocess.STDOUT,
            start_new_session=(os.name == "posix"))
        try:
            status = process.wait(timeout=remaining)
        except subprocess.TimeoutExpired:
            if os.name == "posix":
                os.killpg(process.pid, signal.SIGKILL)
            else:
                process.kill()
            process.wait()
            raise ValidationSetupError("MadGraph preparation timed out; see " + str(log_file))
    if status != 0:
        raise ValidationSetupError("A MadGraph preparation command failed (exit " +
                                   str(status) + "); see " + str(log_file))


def find_compiler(setting):
    candidates = [setting] if setting else [
        shutil.which("gfortran"), "/opt/homebrew/bin/gfortran", "/usr/local/bin/gfortran"]
    for candidate in candidates:
        if not candidate:
            continue
        found = shutil.which(str(candidate))
        if found and os.access(found, os.X_OK):
            resolved = str(Path(found).resolve())
            if not re.fullmatch(r"[A-Za-z0-9_/+.\-]+", resolved):
                raise ValidationSetupError("The Fortran compiler path contains characters unsupported by the MadGraph command interface.")
            return resolved
    raise ValidationSetupError("GNU Fortran was not found. Install gfortran or select FortranCompiler.")


def local_model_specification(root, specification):
    """Resolve installed models only; do not let import_ufo download a UFO."""
    if not isinstance(specification, str) or not specification or any(
            c in specification for c in "\r\n\x00"):
        raise ValidationSetupError("MadGraphModel must name an installed UFO model or its directory.")
    candidate = Path(specification).expanduser()
    search = [candidate] if candidate.is_absolute() else [root / "models" / specification, candidate]
    for item in search:
        if item.is_dir():
            return "./" + os.path.relpath(item.resolve())
    if "-" in specification:
        base, restriction = specification.rsplit("-", 1)
        candidate = Path(base).expanduser()
        search = [candidate] if candidate.is_absolute() else [root / "models" / base, candidate]
        for item in search:
            if item.is_dir() and (restriction == "full" or
                    (item / ("restrict_" + restriction + ".dat")).is_file()):
                return "./" + os.path.relpath(item.resolve()) + "-" + restriction
    raise ValidationSetupError("The selected MadGraph UFO model was not found locally: " + specification)


def read_model(root, specification):
    sys.path.insert(0, str(root))
    from models import import_ufo, model_reader, check_param_card
    import_ufo.ReadWrite = False
    local_spec = local_model_specification(root, specification)
    path, restriction, unused = import_ufo.get_path_restrict(local_spec)
    model = import_ufo.import_model(local_spec)
    return model, Path(path).resolve(), Path(restriction).resolve() if restriction else None, model_reader, check_param_card


def parameter_index(model):
    entries = {}
    external = {}
    for group, parameters in model["parameters"].items():
        for parameter in parameters:
            key = normalized_name(parameter.name)
            if key in entries and entries[key].name != parameter.name:
                raise ValidationSetupError("Case-insensitive UFO parameter names are ambiguous: " + parameter.name)
            entries[key] = parameter
            if group == ("external",):
                external[key] = parameter
    return entries, external


def allowed_particle_types(setting, external_pdgs):
    if not isinstance(setting, list) or not all(
            isinstance(code, int) and not isinstance(code, bool) and code != 0 for code in setting):
        raise ValidationSetupError("AllowedParticlePDGs must be a list of nonzero integer particle types.")
    allowed = sorted({abs(code) for code in setting})
    if allowed and any(abs(code) not in allowed for code in external_pdgs):
        raise ValidationSetupError("AllowedParticlePDGs omits an external particle type.")
    return allowed


def reference_particle_exclusions(model, allowed):
    # A selected UFO can contain additional interactions absent from the theory
    # being checked. Restrict only by explicitly declared particle content, never
    # by reading the HEPCAT amplitude or fitting its numerical values.
    if not allowed:
        return []
    known = {abs(code) for code in model["particle_dict"]}
    if set(allowed).difference(known):
        raise ValidationSetupError("The MadGraph model lacks a requested AllowedParticlePDGs type.")
    excluded = set()
    for pdg, particle in model["particle_dict"].items():
        if abs(pdg) in allowed:
            continue
        name = particle.get_name()
        if not re.fullmatch(r"[A-Za-z0-9_~+.\-]+", name):
            raise ValidationSetupError("Unsupported MadGraph excluded-particle name: " + name)
        excluded.add(name)
    return sorted(excluded)


def checked_mass_parameter_roles(roles):
    if not isinstance(roles, dict):
        raise ValidationSetupError("MassParameterPDGs must be a model-parameter/PDG object.")
    result = {}
    for name, pdgs in roles.items():
        key = normalized_name(name)
        if key in result:
            raise ValidationSetupError("Repeated case-insensitive mass-role parameter: " + name)
        if not isinstance(pdgs, list) or not pdgs or any(
                isinstance(pdg, bool) or not isinstance(pdg, int) or
                pdg <= 0 or pdg > 2147483647 for pdg in pdgs):
            raise ValidationSetupError("A mass role requires positive absolute particle PDG codes: " + name)
        result[key] = sorted(set(pdgs))
    return result


def checked_configuration(data):
    if not isinstance(data, dict):
        raise ValidationSetupError("The MadGraph configuration must be a JSON object.")
    directory = data.get("MadGraphDirectory")
    if not isinstance(directory, str) or not directory.strip() or any(
            c in directory for c in "\r\n\x00"):
        raise ValidationSetupError("MadGraphDirectory must explicitly select a MadGraph installation directory.")
    specification = data.get("MadGraphModel")
    if not isinstance(specification, str) or not specification.strip() or any(
            c in specification for c in "\r\n\x00"):
        raise ValidationSetupError("MadGraphModel must explicitly select an installed UFO model or its directory.")
    root = Path(directory).expanduser().resolve()
    if not (root / "bin" / "mg5_aMC").is_file() or not (root / "models" / "import_ufo.py").is_file():
        raise ValidationSetupError("MadGraphDirectory does not contain a usable MadGraph installation.")
    pdgs = data.get("PhysicalPDGs")
    if not isinstance(pdgs, list) or len(pdgs) < 4 or not all(
            isinstance(x, int) and not isinstance(x, bool) and x != 0 for x in pdgs):
        raise ValidationSetupError("PhysicalPDGs must list two incoming and at least two outgoing PDG codes.")
    allowed_particle_types(data.get("AllowedParticlePDGs", []), pdgs)
    masses = data.get("Masses")
    if not isinstance(masses, list) or len(masses) != len(pdgs):
        raise ValidationSetupError("Masses must specify each physical external mass.")
    masses = [real_number(x, "External mass") for x in masses]
    if any(x < 0 for x in masses):
        raise ValidationSetupError("External masses must be nonnegative.")
    parameters = data.get("Parameters", [])
    if not isinstance(parameters, list):
        raise ValidationSetupError("Parameters must be a list of Name/Value objects.")
    used = {}
    for row in parameters:
        if not isinstance(row, dict) or "Name" not in row or "Value" not in row:
            raise ValidationSetupError("Each used HEPCAT parameter needs Name and Value.")
        key = normalized_name(row["Name"])
        if key in used:
            raise ValidationSetupError("Repeated HEPCAT parameter name: " + row["Name"])
        used[key] = (row["Name"], real_number(row["Value"], row["Name"]))
    rules = data.get("ParameterRules", {})
    name_map = data.get("ParameterMap", {})
    if not isinstance(rules, dict) or not isinstance(name_map, dict):
        raise ValidationSetupError("ParameterRules and ParameterMap must be JSON objects.")
    normalized_rules = {}
    for name, value in rules.items():
        key = normalized_name(name)
        if key in normalized_rules:
            raise ValidationSetupError("Repeated case-insensitive MadGraphParameterRules name: " + name)
        normalized_rules[key] = real_number(value, name)
    rules = normalized_rules
    normalized_map = {}
    for source, target in name_map.items():
        key = normalized_name(source)
        if key in normalized_map:
            raise ValidationSetupError("Repeated case-insensitive MadGraphParameterMap source: " + source)
        if key not in used:
            raise ValidationSetupError("MadGraphParameterMap source is not used by this HEPCAT export: " + source)
        normalized_map[key] = normalized_name(target)
    name_map = normalized_map
    checked_mass_parameter_roles(data.get("MassParameterPDGs", {}))
    optimization_flags(data.get("OptimizationLevel", 0))
    jamp_split_setting(data.get("JampSplit"))
    seconds = real_number(data.get("TimeConstraint", 600), "TimeConstraint")
    if seconds <= 0:
        raise ValidationSetupError("TimeConstraint must be positive.")
    return root, pdgs, masses, used, rules, name_map, seconds


def zero_private_ufo_widths(directory, width_names):
    """Rewrite only declared width Parameter.value arguments in a private UFO.

    Preserve source spelling and comments. AST byte spans avoid evaluating the
    model and also handle non-ASCII metadata before a value on the same line.
    Unsupported dynamic declarations fail instead of claiming a widthless check.
    """
    wanted = {normalized_name(name) for name in width_names if normalized_name(name) != "zero"}
    if not wanted:
        return {}
    path = directory / "parameters.py"
    if not path.is_file():
        raise ValidationSetupError("Cannot disable declared UFO widths: private parameters.py is missing.")
    source = path.read_bytes()
    try:
        tree = ast.parse(source.decode("utf-8"), filename=str(path))
    except (SyntaxError, UnicodeError) as error:
        raise ValidationSetupError("Cannot inspect private UFO width declarations: " + str(error)) from error
    constructors = {"Parameter"}
    for node in ast.walk(tree):
        if isinstance(node, ast.ImportFrom) and node.module and node.module.endswith("object_library"):
            for alias in node.names:
                if alias.name == "Parameter":
                    constructors.add(alias.asname or alias.name)
    lines = source.splitlines(keepends=True)
    offsets = [0]
    for line in lines:
        offsets.append(offsets[-1] + len(line))
    replacements, found, external_slots = [], set(), {}
    for node in ast.walk(tree):
        if not isinstance(node, ast.Call):
            continue
        constructor = (isinstance(node.func, ast.Name) and node.func.id in constructors) or (
            isinstance(node.func, ast.Attribute) and node.func.attr == "Parameter")
        if not constructor:
            continue
        keywords = {item.arg: item.value for item in node.keywords if item.arg is not None}
        name_node = keywords.get("name")
        if not isinstance(name_node, ast.Constant) or not isinstance(name_node.value, str):
            continue
        name = re.sub(r"^mdl_", "", name_node.value, flags=re.I).lower()
        if name not in wanted:
            continue
        if name in found:
            raise ValidationSetupError("Repeated private UFO width declaration: " + name_node.value)
        nature_node, value_node = keywords.get("nature"), keywords.get("value")
        if not isinstance(nature_node, ast.Constant) or nature_node.value not in ("external", "internal") or value_node is None:
            raise ValidationSetupError("Cannot safely rewrite private UFO width " + name_node.value + ".")
        if not hasattr(value_node, "end_lineno") or value_node.end_lineno is None:
            raise ValidationSetupError("The Python AST cannot locate private UFO width values.")
        start = offsets[value_node.lineno-1] + value_node.col_offset
        end = offsets[value_node.end_lineno-1] + value_node.end_col_offset
        replacement = b"0.0" if nature_node.value == "external" else b"'0.0'"
        if nature_node.value == "external":
            try:
                block = ast.literal_eval(keywords["lhablock"])
                code = ast.literal_eval(keywords["lhacode"])
            except (KeyError, ValueError, TypeError, SyntaxError) as error:
                raise ValidationSetupError("Cannot read the LHA slot for private UFO width " + name_node.value) from error
            if not isinstance(block, str) or not isinstance(code, (list, tuple)) or not code or not all(
                    isinstance(index, int) and not isinstance(index, bool) for index in code):
                raise ValidationSetupError("Unsupported LHA slot for private UFO width " + name_node.value)
            external_slots[name] = (block.lower(), tuple(code))
        replacements.append((start, end, replacement))
        found.add(name)
    missing = wanted - found
    if missing:
        raise ValidationSetupError("Cannot safely rewrite declared UFO width parameters: " + ", ".join(sorted(missing)))
    for start, end, replacement in sorted(replacements, reverse=True):
        source = source[:start] + replacement + source[end:]
    try:
        ast.parse(source.decode("utf-8"), filename=str(path))
    except (SyntaxError, UnicodeError) as error:
        raise ValidationSetupError("Private UFO width rewrite produced invalid Python.") from error
    path.write_bytes(source)
    return external_slots


def zero_private_width_restriction(path, external_slots):
    # Restriction cards are applied after Parameter defaults. Override only the
    # width slots, otherwise a selected card could restore a nonzero width.
    slots = set(external_slots.values())
    block, output = None, []
    for line in path.read_text().splitlines(keepends=True):
        body, separator, comment = line.partition("#")
        fields = list(re.finditer(r"\S+", body))
        words = [field.group(0) for field in fields]
        value_index = None
        if words and words[0].lower() == "block":
            block = words[1].lower() if len(words) > 1 else None
        elif words and words[0].lower() == "decay":
            block = None
            if len(words) >= 3:
                try:
                    if ("decay", (int(words[1]),)) in slots:
                        value_index = 2
                except ValueError:
                    pass
        elif block is not None and words:
            for slot_block, code in slots:
                if slot_block != block or len(words) != len(code)+1:
                    continue
                try:
                    if tuple(int(word) for word in words[:-1]) == code:
                        value_index = len(code)
                except ValueError:
                    continue
        if value_index is not None:
            field = fields[value_index]
            body = body[:field.start()] + "0.0" + body[field.end():]
        output.append(body + (separator + comment if separator else ""))
    path.write_text("".join(output))


def private_ufo_copy(source, restriction, output, validation_widths=None):
    target = output / "reference-model"
    shutil.copytree(source, target, ignore=shutil.ignore_patterns(
        "__pycache__", "*.pyc", "*.pyo", "*.pkl", "._*"))
    # The MadEvent numerical adapter uses the default and retains all widths.
    # Only independent validation passes this explicit private-copy override.
    width_slots = None
    if validation_widths is not None:
        width_slots = zero_private_ufo_widths(target, validation_widths)
    if restriction:
        copied_restriction = target / "restrict_hepcat.dat"
        shutil.copyfile(restriction, copied_restriction)
        if width_slots is not None:
            zero_private_width_restriction(copied_restriction, width_slots)
        return "./reference-model-hepcat"
    return "./reference-model-full"


def match_parameters(model, used, rules, name_map, mass_roles=None):
    entries, external = parameter_index(model)
    mass_roles = {} if mass_roles is None else mass_roles
    updates = {}
    matches = []
    for key, value in rules.items():
        if key not in external:
            raise ValidationSetupError("MadGraphParameterRules must select external UFO parameters; unknown or dependent name: " + key)
        updates[key] = value
    targets = {}
    for source, (name, value) in used.items():
        target = name_map.get(source, source)
        role_match = None
        # The conventional SM strong coupling is called G in the UFO.
        if source == "gg" and source not in name_map and target not in entries and "g" in entries:
            target = "g"
        if source not in name_map and target not in entries and source in mass_roles:
            role_targets = set()
            particles = model.get("particle_dict") or {}
            for pdg in mass_roles[source]:
                particle = particles.get(pdg)
                if particle is None:
                    particle = particles.get(-pdg)
                if particle is None:
                    raise ValidationSetupError("The reference model has no particle for mass role " +
                                               name + ": PDG " + str(pdg))
                mass_name = normalized_name(particle.get("mass"))
                if mass_name == "zero" and value != 0:
                    raise ValidationSetupError("The reference particle for PDG " + str(pdg) +
                                               " is massless, but HEPCAT mass " + name + " is nonzero.")
                role_targets.add(mass_name)
            if len(role_targets) != 1:
                raise ValidationSetupError("HEPCAT mass " + name + " serves several PDGs with different "
                                           "reference mass parameters. Supply an unambiguous MadGraphParameterMap.")
            target = next(iter(role_targets))
            role_match = {"Role": "ParticleMass", "PDGs": mass_roles[source]}
        if target not in entries:
            raise ValidationSetupError("No verified MadGraph counterpart for HEPCAT parameter " + name +
                ". Supply MadGraphParameterMap and, if needed, MadGraphParameterRules.")
        targets[source] = target
        if target in external:
            if target in updates and not close_value(updates[target], value):
                raise ValidationSetupError("An explicit MadGraph parameter value conflicts with HEPCAT parameter " + name)
            updates[target] = value
        elif target == "ee" and "aewm1" in external and "aewm1" not in updates:
            if value == 0:
                raise ValidationSetupError("Zero electromagnetic coupling cannot be encoded by a finite inverse-alpha input.")
            updates["aewm1"] = 4 * math.pi / (value * value)
        elif target == "g" and source == "gg" and "as" in external and "as" not in updates:
            updates["as"] = value * value / (4 * math.pi)
        match = {"HEPCAT": name, "MadGraph": re.sub(r"^mdl_", "", entries[target].name), "RequestedValue": value}
        if role_match is not None:
            match.update(role_match)
        matches.append(match)
    updates = synchronize_sm_yukawa(model, external, updates, rules)
    return entries, external, updates, targets, matches


def synchronize_sm_yukawa(model, external, updates, rules):
    # In the bundled SM UFO, YUKAWA entries are fermion mass inputs, not y itself.
    # Keep them coherent with matched masses unless the user explicitly sets them.
    updates = dict(updates)
    if str(model.get("name")).split("-", 1)[0].lower() == "sm":
        by_slot = {(p.lhablock.lower(), tuple(p.lhacode)): k for k, p in external.items()}
        for key, value in list(updates.items()):
            parameter = external[key]
            if parameter.lhablock.lower() == "mass":
                yukawa = by_slot.get(("yukawa", tuple(parameter.lhacode)))
                if yukawa and yukawa not in rules:
                    updates[yukawa] = value
    return updates


def match_external_masses(model, pdgs, masses, external, updates):
    # A supplied physical mass fixes a direct UFO mass input even when the
    # analytic amplitude contains a numerical mass rather than a named symbol.
    updates = dict(updates)
    matches = []
    for pdg, mass in zip(pdgs, masses):
        name = model["particle_dict"][pdg].get("mass")
        key = normalized_name(name)
        if key in external:
            if key in updates and not close_value(updates[key], mass):
                raise ValidationSetupError("MadGraph mass input " + name +
                    " conflicts with the supplied external mass for PDG " + str(pdg))
            updates[key] = mass
        matches.append({"PDG": pdg, "MadGraph": re.sub(r"^mdl_", "", name),
                        "RequestedMass": mass, "DirectInput": key in external})
    return updates, matches


def validation_width_parameters(model):
    entries, external = parameter_index(model)
    widths = {key: parameter for key, parameter in external.items()
              if parameter.lhablock.lower() == "decay"}
    for particle in model["particles"]:
        name = particle.get("width")
        key = normalized_name(name)
        if key == "zero":
            continue
        if key not in entries:
            raise ValidationSetupError("Declared particle width has no UFO parameter: " + name)
        widths[key] = entries[key]
    return widths


def validation_zero_width_inputs(model, used, rules, name_map):
    """Disable width-role inputs without changing unrelated masses or couplings."""
    entries, external = parameter_index(model)
    widths = validation_width_parameters(model)
    zero_used = {}
    for source, (name, value) in used.items():
        target = name_map.get(source, source)
        if source == "gg" and source not in name_map and target not in entries and "g" in entries:
            target = "g"
        zero_used[source] = (name, 0.0 if target in widths else value)
    # External width rules become zero. Internal width rules are replaced by
    # their private zero expression rather than sent through an external card.
    zero_rules = {key: (0.0 if key in widths else value) for key, value in rules.items()
                  if key not in widths or key in external}
    for key, parameter in widths.items():
        parameter.value = 0.0
        if key not in external:
            parameter.expr = "0.0"
    return zero_used, zero_rules, widths


def bare_tree_widths(external, updates, declared_widths=None):
    # Validation always uses zero widths, including explicit and matched inputs.
    updates = dict(updates)
    widths = {key: parameter for key, parameter in external.items()
              if parameter.lhablock.lower() == "decay"}
    if declared_widths is not None:
        widths.update(declared_widths)
    zeroed = []
    for key, parameter in widths.items():
        if key in external:
            updates[key] = 0.0
        row = {"Name": re.sub(r"^mdl_", "", parameter.name), "Value": 0.0}
        if key not in external:
            row["Dependent"] = True
        zeroed.append(row)
    return updates, zeroed, []


def check_other_widths(model, external, values, targets, matrix_source):
    # Check every declared particle width, even unused or explicitly matched
    # ones. An unsupported rewrite is a setup failure, never a numerical PASS.
    seen = set()
    for particle in model["particles"]:
        name = particle.get("width")
        key = normalized_name(name)
        if key in seen or key == "zero":
            continue
        seen.add(key)
        value = complex(values.get(key, float("nan")))
        if not close_value(value, 0):
            raise ValidationSetupError("Validation requires zero particle width " + name +
                "; the private UFO evaluates it to " + str(value) + ".")
    return []


def safe_card_number(value):
    # The standalone Fortran reader has CHARACTER*20 parameter-value fields.
    value = real_number(value, "Card value")
    for digits in range(16, 11, -1):
        text = format(value, "." + str(digits) + "g")
        if len(text) <= 20:
            return text
    raise ValidationSetupError("A model input exceeds MadGraph's 20-character numerical field.")


def update_card(path, updates, external, card_module):
    card = card_module.ParamCard(str(path))
    for key, value in updates.items():
        parameter = external[key]
        block = parameter.lhablock.lower()
        try:
            card[block].get(tuple(parameter.lhacode)).value = value
        except (KeyError, AttributeError) as error:
            raise ValidationSetupError("Generated parameter card has no entry for " + parameter.name) from error
    # Preserve all card sections and comments, but replace numeric fields with
    # strings short enough for the independent Fortran reader.
    text = card.write(precision=16)
    lines = []
    for line in text.splitlines():
        body, separator, comment = line.partition("#")
        pieces = body.split()
        if pieces and pieces[0].lower() == "decay" and len(pieces) >= 3:
            pieces[2] = safe_card_number(float(pieces[2].replace("D", "E").replace("d", "e")))
            body = " ".join(pieces)
        elif pieces and pieces[0].lstrip("+-").isdigit() and len(pieces) >= 2:
            try:
                number = float(pieces[-1].replace("D", "E").replace("d", "e"))
            except ValueError:
                pass
            else:
                pieces[-1] = safe_card_number(number)
                body = "  " + " ".join(pieces)
        lines.append(body + ((" #" + comment) if separator else ""))
    path.write_text("\n".join(lines) + "\n")


def verify_model_values(model, card, masses, pdgs, used, targets, matches, model_reader):
    reader = model_reader.ModelReader(model)
    reader.set_parameters_and_couplings(str(card))
    values = {normalized_name(k): v for k, v in reader["parameter_dict"].items()}
    for row in matches:
        key = normalized_name(row["HEPCAT"])
        actual = values[targets[key]]
        if not close_value(actual, used[key][1]):
            raise ValidationSetupError("MadGraph parameter " + row["MadGraph"] + " evaluates to " + str(actual) +
                ", not the HEPCAT value " + str(used[key][1]) + ". Adjust MadGraphParameterRules.")
        row["MadGraphValue"] = float(complex(actual).real)
    actual_masses = []
    for pdg, expected in zip(pdgs, masses):
        particle = model["particle_dict"][pdg]
        name = normalized_name(particle.get("mass"))
        actual = complex(values.get(name, 0 if name == "zero" else float("nan")))
        if not close_value(actual, expected):
            raise ValidationSetupError("MadGraph external mass for PDG " + str(pdg) + " is " + str(actual) +
                ", not " + str(expected) + ". Select an unrestricted massive model and match its inputs.")
        actual_masses.append(float(actual.real))
    return actual_masses, values


def complete_tree_orders(model, nexternal):
    # MG otherwise chooses the lowest WEIGHTED order, which is not a check of
    # the complete tree amplitude. A tree with 3+-point vertices has at most
    # nexternal-2 vertices, so these bounds include all of its coupling powers.
    maxima = {}
    for vertex in model["interactions"]:
        if vertex.get("type") != "base":
            continue
        if len(vertex.get("particles")) < 3:
            raise ValidationSetupError("Automatic full-tree validation does not support two-point UFO vertices.")
        for name, power in vertex.get("orders").items():
            if not re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", name) or not isinstance(power, int) or power < 0:
                raise ValidationSetupError("Unsupported UFO coupling-order declaration.")
            maxima[name] = max(maxima.get(name, 0), power)
    bounds = {name: power * (nexternal - 2) for name, power in maxima.items()}
    if not bounds:
        bounds = {"WEIGHTED": 0}
    return bounds


def madgraph_version(root):
    version_file = root / "VERSION"
    if not version_file.is_file():
        return {"Version": "Unknown", "Date": "Unknown"}
    fields = {}
    for line in version_file.read_text(errors="replace").splitlines():
        if "=" in line:
            name, value = line.split("=", 1)
            fields[name.strip().lower()] = value.strip()
    return {"Version": fields.get("version", "Unknown"), "Date": fields.get("date", "Unknown")}


def fortran_literal(value):
    return "'" + value.replace("'", "''") + "'"


def optimization_flags(level):
    """Keep validation defaults unchanged; timing clients can request -O3."""
    if isinstance(level, bool) or not isinstance(level, int) or level not in range(4):
        raise ValidationSetupError("OptimizationLevel must be an integer from 0 through 3.")
    return "-w -fPIC -ffixed-line-length-none" + (" -O" + str(level) if level else "")


def jamp_split_setting(value):
    """Optional native-exporter formatting for unusually large references."""
    if value is None:
        return 0
    if isinstance(value, bool) or not isinstance(value, int) or not 0 <= value <= 4096:
        raise ValidationSetupError("JampSplit must be null or an integer from 0 through 4096.")
    return value


def reference_jamp_split(data, external_count):
    """Bound large Fortran accumulations if the native exporter supports it."""
    if "JampSplit" in data or external_count < 10:
        return jamp_split_setting(data.get("JampSplit"))
    # The selected MadGraph installation has already been loaded by read_model.
    # Ten external legs can produce tens of thousands of terms in one sum.
    try:
        from madgraph.iolibs.export_v4 import ProcessExporterFortran
        parameters = inspect.signature(ProcessExporterFortran.get_JAMP_lines).parameters
    except (ImportError, AttributeError, TypeError, ValueError):
        return 0
    return 128 if "split" in parameters else 0


def madgraph_generation_command(root, output, command_file, split=0):
    """Use native MG unchanged unless bounded JAMP statements were requested.

    The private bootstrap changes only the default split argument of MG's own
    exporter. All graph, amplitude, color-coefficient and helicity definitions
    remain native. Neither the installed MG files nor the generated matrix
    formula is rewritten by this helper.
    """
    split = jamp_split_setting(split)
    metadata = {"JampSplit": split, "JampAccumulationPolicy": "NativeDefault",
                "JampBootstrap": None}
    executable = root / "bin" / "mg5_aMC"
    if not split:
        return [sys.executable, executable, "-s", command_file], metadata
    source = """# Private native-MadGraph exporter bootstrap. Installed files are unchanged.
import functools
import inspect
from pathlib import Path
import runpy
import sys

sys.dont_write_bytecode = True
root = Path(ROOT_LITERAL)
sys.path.insert(0, str(root))
from madgraph.iolibs import export_v4

original = export_v4.ProcessExporterFortran.get_JAMP_lines
signature = inspect.signature(original)
if "split" not in signature.parameters:
    raise RuntimeError("The selected MadGraph exporter lacks its native JAMP split argument.")

@functools.wraps(original)
def bounded_jamp(self, *args, **kwargs):
    bound = signature.bind(self, *args, **kwargs)
    selected = bound.arguments.get("split", signature.parameters["split"].default)
    if selected <= 0:
        bound.arguments["split"] = SPLIT_LITERAL
    return original(*bound.args, **bound.kwargs)

export_v4.ProcessExporterFortran.get_JAMP_lines = bounded_jamp
executable = root / "bin" / "mg5_aMC"
sys.argv[0] = str(executable)
runpy.run_path(str(executable), run_name="__main__")
""".replace("ROOT_LITERAL", repr(str(root.resolve()))).replace("SPLIT_LITERAL", str(split))
    bootstrap = output / "native-jamp-bootstrap.py"
    bootstrap.write_text(source)
    metadata.update({
        "JampAccumulationPolicy": "Native exporter split argument with color-code optimization disabled; diagrams, coefficients and helicities unchanged.",
        "JampBootstrap": {"Path": str(bootstrap.resolve()),
                          "SHA256": hashlib.sha256(source.encode()).hexdigest(), "Source": source}})
    # MG normally reexecutes its launcher with -O. Start in that same mode
    # here, so its reexec cannot drop this process-local exporter wrapper.
    metadata["JampBootstrap"]["PythonFlags"] = ["-O"]
    return [sys.executable, "-O", bootstrap, "-s", command_file], metadata


def complete_helicity_table(matrix_source, nexternal):
    """Read the actual generated state inventory, independently of HEPCAT.

    MATRIX returns raw |M|^2. SMATRIX adds averages and may prune helicities
    after a few points. Reuse its complete NHEL table while bypassing those
    stateful heuristics and its denominator, so every state is always summed.
    """
    # Current MG repeats NHEL in GET_NHEL. Read the SMATRIX unit only.
    unit = re.search(r"^ {6}SUBROUTINE\s+[A-Z0-9_]*SMATRIX\s*\(.*?^ {6}END[ \t]*$",
                     matrix_source, re.I | re.M | re.S)
    if unit:
        matrix_source = unit.group(0)
    source = " ".join(line[6:] for line in matrix_source.splitlines()
                      if len(line) > 6 and line[0] not in "cC*!")
    count = re.search(r"PARAMETER\s*\(\s*NCOMB\s*=\s*(\d+)\s*\)", source, re.I)
    rows = re.findall(r"DATA\s*\(\s*NHEL\s*\(\s*I\s*,\s*(\d+)\s*\)\s*,"
                      r"\s*I\s*=\s*1\s*,\s*(\d+)\s*\)\s*/([^/]+)/", source, re.I)
    if not count or not rows:
        raise ValidationSetupError("MadGraph's complete standalone helicity table was not found.")
    expected = int(count.group(1))
    table = {}
    for index, length, values in rows:
        index = int(index)
        if int(length) != nexternal or index in table:
            raise ValidationSetupError("MadGraph's standalone helicity table has inconsistent dimensions.")
        tokens = values.replace(" ", "").split(",")
        if len(tokens) != nexternal or not all(re.fullmatch(r"[+-]?\d+", token) for token in tokens):
            raise ValidationSetupError("MadGraph's standalone helicity table contains unsupported entries.")
        table[index] = [int(token) for token in tokens]
    if set(table) != set(range(1, expected + 1)):
        raise ValidationSetupError("MadGraph's standalone helicity table is incomplete.")
    return [table[index] for index in range(1, expected + 1)]


def summed_bridge(matrix_name, table, nexternal):
    """One native call evaluates the exact sum of every generated spin state."""
    declarations = []
    for index, row in enumerate(table, 1):
        # Free-form continuations also keep large external multiplicities valid.
        chunks = [row[begin:begin + 16] for begin in range(0, len(row), 16)]
        values = ", &\n    ".join(",".join(map(str, chunk)) for chunk in chunks)
        declarations.append("  data (helicities(leg," + str(index) + "),leg=1," +
                            str(nexternal) + ") / &\n    " + values + " /")
    return """
! Raw exhaustive spin/color sum. No pruning, sampling, average or factorial.
subroutine hepcat_mg_evaluate_sum(p, answer) bind(C, name="hepcat_mg_evaluate_sum")
  use iso_c_binding
  implicit none
  real(c_double), intent(in) :: p(4,""" + str(nexternal) + """)
  real(c_double), intent(out) :: answer
  integer(c_int) :: color(""" + str(nexternal) + """)
  integer(c_int), save :: helicities(""" + str(nexternal) + "," + str(len(table)) + """)
  integer :: leg, state
  double precision, external :: """ + matrix_name + "\n" + "\n".join(declarations) + """
  color = 1
  answer = 0d0
  do state = 1, """ + str(len(table)) + """
    answer = answer + """ + matrix_name + """(p, helicities(:,state), color)
  enddo
end subroutine
"""


def build_reference(data, output):
    root, pdgs, masses, used, rules, name_map, seconds = checked_configuration(data)
    mass_roles = checked_mass_parameter_roles(data.get("MassParameterPDGs", {}))
    deadline = time.monotonic() + seconds
    output.mkdir(parents=True, exist_ok=True)
    if (output / "process").exists() or (output / "reference-model").exists():
        raise ValidationSetupError("Use a fresh private MadGraph output directory.")
    compiler = find_compiler(data.get("FortranCompiler"))
    with (output / "model.log").open("w") as log, contextlib.redirect_stdout(log), contextlib.redirect_stderr(log):
        model, model_path, restriction, model_reader, card_module = read_model(root, data["MadGraphModel"])
        jamp_split = reference_jamp_split(data, len(pdgs))
        names = []
        for slot, pdg in enumerate(pdgs):
            if pdg not in model["particle_dict"]:
                raise ValidationSetupError("The MadGraph model has no particle with PDG " + str(pdg))
            particle = model["particle_dict"][pdg]
            if abs(particle.get("color")) != 1:
                representations = data.get("ColorRepresentations", [])
                if not data.get("FullColor") or len(representations) != len(pdgs):
                    raise ValidationSetupError("Colored external particles require a certified coherent HEPCAT color amplitude.")
                if abs(representations[slot]) != abs(particle.get("color")):
                    raise ValidationSetupError("The HEPCAT and MadGraph external color representations disagree.")
            name = particle.get_name()
            if not re.fullmatch(r"[A-Za-z0-9_~+.\-]+", name):
                raise ValidationSetupError("Unsupported MadGraph command particle name: " + name)
            names.append(name)
        allowed = allowed_particle_types(data.get("AllowedParticlePDGs", []), pdgs)
        excluded = reference_particle_exclusions(model, allowed)
        used, rules, declared_widths = validation_zero_width_inputs(model, used, rules, name_map)
        entries, external, updates, targets, matches = match_parameters(model, used, rules, name_map, mass_roles)
        updates, mass_matching = match_external_masses(model, pdgs, masses, external, updates)
        updates = synchronize_sm_yukawa(model, external, updates, rules)
        updates, zeroed_widths, preserved_widths = bare_tree_widths(external, updates, declared_widths)
        orders = complete_tree_orders(model, len(pdgs))
        private_spec = private_ufo_copy(model_path, restriction, output, declared_widths)
    process_string = " ".join(names[:2]) + " > " + " ".join(names[2:])
    generation_process = process_string + (" / " + " ".join(excluded) if excluded else "")
    command_file = output / "generate.mg5"
    command_file.write_text("\n".join([
        "set auto_update 0 --no_save", "set automatic_html_opening False --no_save",
        "set notification_center False --no_save", "set fortran_compiler " + compiler + " --no_save",
        "import model " + private_spec,
        "generate " + generation_process + " " + " ".join(name + "<=" + str(value) for name, value in sorted(orders.items())),
        "output standalone process" + (" --jamp_optim=False" if jamp_split else ""), "quit", ""]))
    environment = dict(os.environ)
    environment["PYTHONDONTWRITEBYTECODE"] = "1"
    environment["PATH"] = str(Path(compiler).parent) + os.pathsep + environment.get("PATH", "")
    generation_command, generation_metadata = madgraph_generation_command(root, output, command_file, jamp_split)
    run_logged(generation_command, output,
               output / "generate.log", deadline, environment)
    process_root = output / "process"
    matrices = sorted((process_root / "SubProcesses").glob("P*/matrix.f"))
    if len(matrices) != 1:
        raise ValidationSetupError("MadGraph did not generate exactly one standalone subprocess; see generate.log.")
    matrix = matrices[0]
    matrix_source = matrix.read_text()
    graph_match = re.search(r"PARAMETER\s*\(\s*NGRAPHS\s*=\s*(\d+)\s*\)", matrix_source, re.I)
    diagram_count = int(graph_match.group(1)) if graph_match else None
    match = re.search(r"(?:REAL\*8|DOUBLE\s+PRECISION)\s+FUNCTION\s+([A-Z0-9_]*MATRIX)\s*\(\s*P\s*,\s*NHEL\s*,\s*IC\s*\)", matrix_source, re.I)
    if not match:
        raise ValidationSetupError("This MadGraph standalone output lacks the supported raw fixed-helicity MATRIX interface.")
    card = process_root / "Cards" / "param_card.dat"
    with (output / "model.log").open("a") as log, contextlib.redirect_stdout(log), contextlib.redirect_stderr(log):
        update_card(card, updates, external, card_module)
        actual_masses, evaluated_values = verify_model_values(model, card, masses, pdgs, used, targets, matches, model_reader)
        preserved_widths += check_other_widths(model, external, evaluated_values, targets, matrix_source)
    level = data.get("OptimizationLevel", 0)
    flags = optimization_flags(level)
    helicity_table = complete_helicity_table(matrix_source, len(pdgs))
    make = shutil.which("make")
    if not make:
        raise ValidationSetupError("make was not found; install the developer command-line tools.")
    make_options = ["FC=" + shlex.quote(compiler), "FFLAGS=" + flags]
    # MG export may already have compiled model/HELAS objects with its default
    # flags. A timing build must not silently reuse those unoptimized archives.
    rebuild = ["-B"] if level else []
    run_logged([make, "-j2"] + rebuild + make_options, process_root / "Source", output / "build.log", deadline, environment)
    run_logged([make, "matrix.o"] + rebuild + make_options, matrix.parent, output / "build.log", deadline, environment)
    nexternal = len(pdgs)
    card_path = str(card.resolve())
    if len(card_path) > 500:
        raise ValidationSetupError("The private parameter-card path is too long for the MadGraph reader.")
    literal_parts = [fortran_literal(card_path[i:i+60]) for i in range(0, len(card_path), 60)]
    card_literal = " // &\n    ".join(literal_parts)
    bridge = output / "madgraph_bridge.f90"
    bridge.write_text("""! Independent MadGraph reference; raw fixed-helicity squared matrix element.
integer(c_int) function hepcat_mg_initialize() bind(C, name="hepcat_mg_initialize")
  use iso_c_binding
  implicit none
  call setParamLog(.false.)
  call setpara(""" + card_literal + """)
  hepcat_mg_initialize = 0
end function

subroutine hepcat_mg_evaluate(p, helicities, answer) bind(C, name="hepcat_mg_evaluate")
  use iso_c_binding
  implicit none
  real(c_double), intent(in) :: p(4,""" + str(nexternal) + """)
  integer(c_int), intent(in) :: helicities(""" + str(nexternal) + """)
  real(c_double), intent(out) :: answer
  integer :: color(""" + str(nexternal) + """)
  double precision, external :: """ + match.group(1) + """
  color = 1
  answer = """ + match.group(1) + """(p, helicities, color)
end subroutine
""" + summed_bridge(match.group(1), helicity_table, nexternal))
    library = output / ("libhepcat_madgraph.dylib" if sys.platform == "darwin" else "libhepcat_madgraph.so")
    link_mode = "-dynamiclib" if sys.platform == "darwin" else "-shared"
    run_logged([compiler, link_mode] + shlex.split(flags) + ["-o", library, bridge, matrix.parent / "matrix.o",
                process_root / "lib" / "libmodel.a", process_root / "lib" / "libdhelas.a"],
               output, output / "build.log", deadline, environment)
    # Fortran card I/O can terminate a process on malformed input. Probe it in a
    # child before the calling C++ validator opens the verified shared library.
    probe = ("import ctypes,sys; lib=ctypes.CDLL(sys.argv[1]); "
             "lib.hepcat_mg_initialize.restype=ctypes.c_int; "
             "status=lib.hepcat_mg_initialize(); "
             "print('HEPCAT_MADGRAPH_INITIALIZED'); sys.exit(status)")
    run_logged([sys.executable, "-c", probe, library], output, output / "initialize.log", deadline, environment)
    if "HEPCAT_MADGRAPH_INITIALIZED" not in (output / "initialize.log").read_text():
        raise ValidationSetupError("MadGraph initialization terminated before returning; see initialize.log.")
    return {"Status": "READY", "Library": str(library.resolve()), "ExternalCount": nexternal,
            "Process": process_string, "PhysicalPDGs": pdgs, "Masses": actual_masses,
            "AllowedParticlePDGs": allowed, "ExcludedParticles": excluded, "DiagramCount": diagram_count,
            "MadGraphModel": data["MadGraphModel"],
            "MadGraphVersion": madgraph_version(root), "MadGraphModelPath": str(model_path),
            "MadGraphRestriction": str(restriction) if restriction else None, "FortranCompiler": compiler,
            "ParameterMatching": matches, "MassMatching": mass_matching,
            "WidthPolicy": "Bare tree validation: all widths disabled. External DECAY inputs and every declared particle width are zero in a private UFO copy, including explicit, matched and dependent widths.",
            "ZeroedWidths": zeroed_widths, "PreservedWidths": preserved_widths,
            "ParameterRules": {re.sub(r"^mdl_", "", external[k].name): v for k, v in updates.items()},
            "CouplingOrders": orders, "Scope": "Complete tree amplitude within the supplied UFO expansion limits and declared allowed particle content.",
            "Normalization": "Raw fixed-helicity, color-summed squared amplitude; no initial averages or final symmetry factor.",
            "SpinSum": "Exhaustive generated helicity table; no pruning, sampling, initial average or final symmetry factor.",
            "HelicityStates": len(helicity_table), "OptimizationLevel": level, "FortranFlags": flags,
            **generation_metadata}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--config", required=True, type=Path)
    parser.add_argument("--output", required=True, type=Path)
    arguments = parser.parse_args()
    output = arguments.output.expanduser().resolve()
    output.mkdir(parents=True, exist_ok=True)
    try:
        data = json.loads(arguments.config.read_text())
        result = build_reference(data, output)
    except Exception as error:
        logs = {name: (output / name).read_text(errors="replace")[-4000:]
                for name in ("model.log", "generate.log", "build.log", "initialize.log") if (output / name).is_file()}
        result = {"Status": "ERROR", "Message": str(error), "ErrorType": type(error).__name__, "Logs": logs}
        (output / "exception.log").write_text(traceback.format_exc())
        write_json(output / "result.json", result)
        print("MadGraph reference preparation failed: " + str(error), file=sys.stderr)
        if logs:
            name = next(reversed(logs))
            print(name + ":\n" + logs[name], file=sys.stderr)
        return 1
    write_json(output / "result.json", result)
    print("MadGraph reference ready: " + result["Process"])
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
