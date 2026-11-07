#!/usr/bin/env python3
"""Configure four-fermion QED timings from existing complete SPINAS exports.

No amplitudes, model files or process exports are changed. Only build adapters
and provenance are written inside the selected build directory.
"""
import argparse
import hashlib
import itertools
import json
import math
from pathlib import Path
import re
import subprocess


def literal(value):
    if isinstance(value, list):
        return '{' + ', '.join(literal(item) for item in value) + '}'
    if isinstance(value, dict):
        return '{' + ', '.join('{' + literal(k) + ', ' + literal(v) + '}'
                               for k, v in value.items()) + '}'
    return json.dumps(value)


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def load_project(path, representation):
    directory = Path(path).resolve()
    data = json.loads((directory / 'manifest.json').read_text())
    name = data.get('Class', '')
    if not re.fullmatch(r'[A-Za-z_][A-Za-z0-9_]*', name):
        raise ValueError('Invalid exported class name: ' + name)
    n = len(data.get('Masses', [])) - 4
    if n < 0:
        raise ValueError(f'{name}: expected four massive fermions followed by photons')
    if data.get('Precision') != 'Double':
        raise ValueError(f'{name}: all four-backend timing exports must use Double precision')
    if data.get('IncomingLegs') != [1, 2] or data.get('Spin2') != [1] * 4 + [2] * n:
        raise ValueError(f'{name}: expected four spin-1/2 fermions with legs 1,2 incoming')
    masses = data['Masses']
    if not all(isinstance(m, (int, float)) and math.isfinite(m) for m in masses):
        raise ValueError(f'{name}: require finite numerical masses')
    if masses[0] <= 0 or masses[:4] != [masses[0]] * 4 or masses[4:] != [0] * n:
        raise ValueError(f'{name}: require a common positive electron mass and massless photons')
    if n and not data.get('HelicityFamily'):
        raise ValueError(f'{name}: use a complete grouped photon-helicity export')
    expected = {(0, 0, 0, 0) + row for row in itertools.product((-2, 2), repeat=n)}
    rows = [tuple(row) for row in data.get('HelicityConfigurations', [])]
    if len(rows) != len(expected) or set(rows) != expected or data.get('FixedHelicities'):
        raise ValueError(f'{name}: all {2 ** n} photon helicities and all massive spins are required')
    names, defaults = data.get('ParameterOrder', []), data.get('ParameterDefaults', [])
    if not names or len(names) != len(defaults) or len(set(names)) != len(names):
        raise ValueError(f'{name}: invalid process parameter ordering')
    physical_pdgs = [11, -11, 11, -11] + [22] * n
    if 'ProcessPDGs' in data and data['ProcessPDGs'] != physical_pdgs:
        raise ValueError(f'{name}: unexpected ProcessPDGs')
    for backend in ('CalcHEPValidation', 'MadGraphValidation'):
        config = data.get(backend, {})
        if config.get('Status') != 'AVAILABLE':
            raise ValueError(f'{name}: {backend} must be available in the export')
        if config.get('PhysicalPDGs') != physical_pdgs:
            raise ValueError(f'{name}: expected e- e+ > e- e+ plus photons in both references')
        if config.get('AllowedParticlePDGs') != [11, 22]:
            raise ValueError(f'{name}: restrict independent reference models to PDGs {{11,22}}')
        if config.get('LegOrder') != list(range(1, n + 5)):
            raise ValueError(f'{name}: references must retain physical external leg order')
        if config.get('ParameterNames') != names:
            raise ValueError(f'{name}: inconsistent reference parameter ordering')
    mg, ch = data['MadGraphValidation'], data['CalcHEPValidation']
    if mg.get('Spin2') != data['Spin2'] or mg.get('HelicityConfigurations') != data['HelicityConfigurations']:
        raise ValueError(f'{name}: MadGraph spin or helicity coverage differs from SPINAS')
    normalization = 1 / (4 * math.factorial(n))
    if (data.get('InitialSpinAverage') != 4
            or not math.isclose(data.get('SymmetryFactor', 0), 1 / math.factorial(n), rel_tol=1e-14)
            or not math.isclose(mg.get('Normalization', 0), normalization, rel_tol=1e-14)
            or ch.get('SPINASNormalizationFactor') != 1):
        raise ValueError(f'{name}: expected the common 1/(4 N!) normalization')
    sidecar = directory / 'benchmark-amplitudes.json'
    amplitude_source = json.loads(sidecar.read_text()) if sidecar.is_file() else {
        'PolePreference': 'Unspecified', 'Note': 'Representation source sidecar was not supplied.'}
    if sidecar.is_file() and amplitude_source.get('PolePreference') != representation:
        raise ValueError(f'{name}: amplitude provenance must identify {representation}')
    for filename in ('process.cpp', 'process.hpp'):
        if not (directory / filename).is_file():
            raise ValueError(f'{name}: missing {filename}')
    return directory, data, n, amplitude_source


def reference_signature(data):
    ch, mg = data['CalcHEPValidation'], data['MadGraphValidation']
    return {
        'Parameters': dict(zip(data['ParameterOrder'], data['ParameterDefaults'])),
        'Mass': data['Masses'][0],
        'CalcHEP': {key: ch[key] for key in ('ModelDirectory', 'ModelNumber', 'AllowedParticlePDGs')},
        'MadGraph': {key: mg.get(key) for key in ('MadGraphDirectory', 'MadGraphModel',
            'PythonExecutable', 'AllowedParticlePDGs', 'ParameterRules', 'ParameterMap',
            'MassParameterPDGs', 'WidthParameterIndices')},
    }


def configuration_lines(data, source):
    ch, mg = data['CalcHEPValidation'], data['MadGraphValidation']
    lines = ['    Configuration config;',
             '    config.name = ' + literal(data['Class']) + ';',
             '    config.calchep_model_directory = ' + literal(ch['ModelDirectory']) + ';',
             '    config.calchep_model_number = ' + str(ch['ModelNumber']) + ';',
             '    config.pdgs = ' + literal(ch['PhysicalPDGs']) + ';',
             '    config.allowed_pdgs = ' + literal(ch['AllowedParticlePDGs']) + ';',
             '    config.parameter_names = ' + literal(data['ParameterOrder']) + ';']
    fields = {'madgraph_directory': 'MadGraphDirectory', 'model': 'MadGraphModel',
              'python_executable': 'PythonExecutable', 'physical_pdgs': 'PhysicalPDGs',
              'allowed_particle_pdgs': 'AllowedParticlePDGs', 'spin2': 'Spin2',
              'helicities': 'HelicityConfigurations', 'parameter_names': 'ParameterNames',
              'parameter_rules': 'ParameterRules', 'parameter_map': 'ParameterMap',
              'normalization': 'Normalization'}
    for field, key in fields.items():
        lines.append('    config.madgraph.' + field + ' = ' + literal(mg[key]) + ';')
    for field, key in {'width_parameter_indices': 'WidthParameterIndices',
                       'mass_parameter_pdgs': 'MassParameterPDGs'}.items():
        if key in mg:
            value = mg[key]
            lines.append('    config.madgraph.' + field + ' = ' + literal(value) + ';')
    lines += ['    config.madgraph.helper_path = ' + literal(str(source / 'madgraph-runtime.py')) + ';',
              '    config.madgraph.leg_order = ' + literal([i - 1 for i in mg['LegOrder']]) + ';',
              '    config.madgraph.preparation_timeout_seconds = 1800;',
              '    config.madgraph.optimization_level = 3;']
    return lines


def model_files(projects):
    ch_paths, mg_paths = set(), set()
    for _, data, _, _ in projects:
        ch = data['CalcHEPValidation']
        ch_paths.update(Path(ch['ModelDirectory']) / (prefix + str(ch['ModelNumber']) + '.mdl')
                        for prefix in ('vars', 'func', 'prtcls', 'lgrng', 'extlib'))
        mg = data['MadGraphValidation']
        candidate = Path(mg['MadGraphModel'])
        if not candidate.is_dir():
            candidate = Path(mg['MadGraphDirectory']) / 'models' / mg['MadGraphModel']
        if not candidate.is_dir():
            candidate = Path(mg['MadGraphDirectory']) / 'models' / mg['MadGraphModel'].split('-', 1)[0]
        if candidate.is_dir():
            mg_paths.update(path for path in candidate.rglob('*')
                            if path.is_file() and path.suffix in ('.py', '.dat')
                            and '__pycache__' not in path.parts)
    return sorted({p.resolve() for p in ch_paths if p.is_file()}), sorted(p.resolve() for p in mg_paths)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--physical-projects', nargs='+', type=Path, required=True)
    parser.add_argument('--spurious-projects', nargs='+', type=Path, required=True)
    parser.add_argument('--compiler', default='unknown')
    parser.add_argument('--compiler-version', default='unknown')
    parser.add_argument('--flags', default='-O3 -DNDEBUG')
    parser.add_argument('--spinas-library', type=Path, required=True)
    parser.add_argument('--calchep-directory', type=Path, required=True)
    args = parser.parse_args()
    repository = Path(__file__).resolve().parents[1]
    source = repository / 'source'
    groups = []
    for paths, representation in ((args.physical_projects, 'AllPhysical'), (args.spurious_projects, 'AllSpurious')):
        group = sorted((load_project(path, representation) for path in paths), key=lambda row: row[2])
        if not group or group[0][2] != 0 or len({row[2] for row in group}) != len(group):
            raise ValueError('Supply exactly one complete export per N in each representation, including N=0')
        groups.append(group)
    if [row[2] for row in groups[0]] != [row[2] for row in groups[1]]:
        raise ValueError('Physical and spurious exports must cover the same photon multiplicities')
    projects = groups[0] + groups[1]
    if len({row[1]['Class'] for row in projects}) != len(projects):
        raise ValueError('Use distinct C++ class names for every physical and spurious export')
    if any(reference_signature(row[1]) != reference_signature(projects[0][1]) for row in projects):
        raise ValueError('All exports must use identical masses, parameters and reference models')
    if any(Path(row[1]['CalcHEPValidation']['CalcHEPDirectory']).resolve() != args.calchep_directory.resolve()
           for row in projects):
        raise ValueError('The linked CalcHEP installation must match the export configuration')
    args.output.mkdir(parents=True, exist_ok=True)
    lines = ['// Generated benchmark registry. No amplitudes are changed.']
    for directory, _, _, _ in projects:
        lines.append('#include ' + literal(str(directory / 'process.hpp')))
    lines += ['namespace hepcat_four_fermion_benchmark {',
              'void register_processes(std::vector<Entry>& entries) {']
    for physical, spurious in zip(groups[0], groups[1]):
        _, data, n, _ = physical
        lines.append('  {')
        lines.extend(configuration_lines(data, source))
        lines += ['    entries.push_back({' + str(n) + ', [config](const Options& options) {',
                  '      return run_process<hepcat_generated::' + data['Class'] + ', hepcat_generated::' + spurious[1]['Class'] + '>(options, config);',
                  '    }});', '  }']
    lines += ['}', '}', '']
    registry = args.output / 'four-fermion-processes.inc'
    registry.write_text('\n'.join(lines))
    support = [repository / 'benchmarks' / name for name in ('qed-four-fermions.cpp', 'benchmark.hpp', 'four-fermion-configure.py')]
    support += [repository / 'benchmarks' / 'four-fermion' / 'CMakeLists.txt']
    support += list(source.glob('*.hpp')) + [source / 'madgraph-runtime.py']
    support = sorted({path for path in support if path.is_file()})
    ch_models, mg_models = model_files(projects)
    library_inputs = sorted(path for path in args.calchep_directory.glob('lib/*')
                            if path.is_file() and path.suffix in ('.a', '.so', '.dylib'))
    compiler_inputs = [args.calchep_directory / 'FlagsForSh', args.calchep_directory / 'sbin' / 'newProcess',
                      args.spinas_library.parent / 'CMakeFiles' / 'spinas_static.dir' / 'flags.make']
    compiler_inputs = [path for path in compiler_inputs if path.is_file()]
    provenance, export_inputs = [], []
    for directory, data, n, amplitude_source in projects:
        files = sorted(set(directory.glob('*.hpp')) | {directory / 'process.cpp', directory / 'manifest.json'})
        if (directory / 'benchmark-amplitudes.json').is_file():
            files.append(directory / 'benchmark-amplitudes.json')
        export_inputs.extend(files)
        provenance.append({'N': n, 'Representation': 'AllPhysical' if (directory, data, n, amplitude_source) in groups[0] else 'AllSpurious',
                           'Export': str(directory), 'Class': data['Class'], 'Precision': data['Precision'],
                           'CodeStyle': data.get('CodeStyle'), 'CodeLayout': data.get('CodeLayout'),
                           'Optimization': data.get('Optimization'), 'AmplitudeSource': amplitude_source,
                           'SHA256': {path.name: sha(path) for path in files}})
    cmake = ['set(BENCHMARK_LONG_DOUBLE OFF)', 'set(FOUR_FERMION_REGISTRY [==[' + str(registry) + ']==])']
    for directory, _, _, _ in projects:
        cmake.append('list(APPEND FOUR_FERMION_SOURCES [==[' + str(directory / 'process.cpp') + ']==])')
    inputs = sorted(set(export_inputs + support + ch_models + mg_models + library_inputs + compiler_inputs + [args.spinas_library]))
    for path in inputs:
        if ';' in str(path) or ']==]' in str(path):
            raise ValueError('Unsupported CMake path: ' + str(path))
        cmake.append('set_property(DIRECTORY APPEND PROPERTY CMAKE_CONFIGURE_DEPENDS [==[' + str(path) + ']==])')
    (args.output / 'projects.cmake').write_text('\n'.join(cmake) + '\n')
    revision = subprocess.run(['git', '-C', str(repository), 'rev-parse', 'HEAD'], text=True, capture_output=True, check=True).stdout.strip()
    info = {'RepositoryRevision': revision, 'Compiler': args.compiler, 'CompilerVersion': args.compiler_version,
            'CXXFlags': args.flags, 'Precision': 'Double', 'Optimization': '-O3',
            'ReferenceOptimization': '-O3 (private builds)', 'ZeroWidths': True,
            'WidthPolicy': 'Independent CalcHEP and MadGraph private zero-width models; generated SPINAS widths disabled.',
            'SPINASLibrary': str(args.spinas_library.resolve()), 'SPINASLibrarySHA256': sha(args.spinas_library),
            'CalcHEPDirectory': str(args.calchep_directory.resolve()), 'Exports': provenance,
            'SupportSHA256': {str(p.relative_to(repository)): sha(p) for p in support},
            'CalcHEPModelSHA256': {str(p): sha(p) for p in ch_models},
            'MadGraphModelSHA256': {str(p): sha(p) for p in mg_models},
            'CalcHEPLibrarySHA256': {str(p): sha(p) for p in library_inputs},
            'CompilerConfigurations': {str(p): p.read_text() for p in compiler_inputs}}
    (args.output / 'build-info.json').write_text(json.dumps(info, indent=2) + '\n')


if __name__ == '__main__':
    main()
