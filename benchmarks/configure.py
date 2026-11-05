#!/usr/bin/env python3
"""Read existing SPINAS exports and generate only benchmark build adapters.

This never changes a process export, model, amplitude, or native event session.
The actual point generation, checks and all measurements are in C++.
"""
import argparse
import hashlib
import itertools
import json
from pathlib import Path
import re
import subprocess


def load_project(directory):
    directory = Path(directory).resolve()
    data = json.loads((directory / 'manifest.json').read_text())
    name = data.get('Class', '')
    if not re.fullmatch(r'[A-Za-z_][A-Za-z0-9_]*', name):
        raise ValueError('Invalid exported C++ class name: ' + name)
    n = len(data.get('Masses', [])) - 2
    if not data.get('HelicityFamily') or n < 2:
        raise ValueError(f'{name}: supply a grouped, complete photon-helicity export')
    if data.get('IncomingLegs') != [1, 2] or data.get('Spin2') != [1, 1] + [2] * n:
        raise ValueError(f'{name}: expected incoming massive e,E followed by photons')
    expected = {(0, 0) + row for row in itertools.product((-2, 2), repeat=n)}
    rows = [tuple(row) for row in data.get('HelicityConfigurations', [])]
    if set(rows) != expected or len(rows) != len(expected) or data.get('FixedHelicities'):
        raise ValueError(f'{name}: all {2 ** n} photon-helicity configurations are required')
    if data.get('Precision') not in ('LongDouble', 'Double'):
        raise ValueError(f'{name}: unsupported SPINAS scalar precision')
    for backend in ('CalcHEPValidation', 'MadGraphValidation'):
        config = data[backend]
        if config.get('Status') != 'AVAILABLE':
            raise ValueError(f'{name}: configure {backend} in WriteSPINASProcess first')
        if config.get('PhysicalPDGs') != [11, -11] + [22] * n:
            raise ValueError(f'{name}: expected the physical process e- e+ > N photons')
        if config.get('AllowedParticlePDGs') != [11, 22]:
            raise ValueError(f'{name}: explicitly restrict both reference models to PDGs {{11,22}}')
        if config.get('LegOrder') != list(range(1, n + 3)):
            raise ValueError(f'{name}: this QED benchmark requires physical external leg order')
        if config.get('ParameterNames') != data['ParameterOrder']:
            raise ValueError(f'{name}: reference parameter ordering is inconsistent')
    if data['CalcHEPValidation'].get('SPINASNormalizationFactor') != 1:
        raise ValueError(f'{name}: unexpected CalcHEP normalization')
    for filename in ('process.hpp', 'process.cpp'):
        if not (directory / filename).is_file():
            raise ValueError(f'{name}: missing {filename}')
    return directory, data, n


def literal(value):
    if isinstance(value, list):
        return '{' + ', '.join(literal(item) for item in value) + '}'
    if isinstance(value, dict):
        return '{' + ', '.join('{' + literal(k) + ', ' + literal(v) + '}' for k, v in value.items()) + '}'
    return json.dumps(value)


def adapter(directory, data, n, source):
    mg, ch, name = data['MadGraphValidation'], data['CalcHEPValidation'], data['Class']
    lines = ['// Generated from manifest.json; no formula is generated or changed.',
             '#include "benchmark.hpp"', '#include ' + literal(str(directory / 'process.hpp')),
             'void add_' + name + '(std::vector<hepcat_benchmark::Entry>& entries) {',
             '  hepcat_benchmark::Configuration config;',
             '  config.name = ' + literal(name) + ';',
             '  config.calchep_model_directory = ' + literal(ch['ModelDirectory']) + ';',
             '  config.calchep_model_number = ' + str(ch['ModelNumber']) + ';',
             '  config.pdgs = ' + literal(ch['PhysicalPDGs']) + ';',
             '  config.allowed_pdgs = ' + literal(ch['AllowedParticlePDGs']) + ';',
             '  config.parameter_names = ' + literal(data['ParameterOrder']) + ';']
    fields = {'madgraph_directory': 'MadGraphDirectory', 'model': 'MadGraphModel',
              'python_executable': 'PythonExecutable', 'physical_pdgs': 'PhysicalPDGs',
              'allowed_particle_pdgs': 'AllowedParticlePDGs', 'spin2': 'Spin2',
              'helicities': 'HelicityConfigurations', 'parameter_names': 'ParameterNames',
              'parameter_rules': 'ParameterRules', 'parameter_map': 'ParameterMap',
              'normalization': 'Normalization'}
    for field, key in fields.items():
        lines.append('  config.madgraph.' + field + ' = ' + literal(mg[key]) + ';')
    lines.extend([
        '  config.madgraph.helper_path = ' + literal(str(source / 'madgraph-runtime.py')) + ';',
        '  config.madgraph.leg_order = ' + literal([i - 1 for i in mg['LegOrder']]) + ';',
        '  config.madgraph.preparation_timeout_seconds = 1800;',
        '  entries.push_back({' + str(n) + ', [config](const hepcat_benchmark::Options& options) {',
        '    return hepcat_benchmark::run_process<hepcat_generated::' + name + '>(options, config);',
        '  }});', '}', ''])
    return '\n'.join(lines)


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--projects', nargs='+', required=True, type=Path)
    parser.add_argument('--compiler', default='unknown')
    parser.add_argument('--compiler-version', default='unknown')
    parser.add_argument('--flags', default='')
    parser.add_argument('--spinas-library', type=Path, required=True)
    parser.add_argument('--calchep-directory', type=Path, required=True)
    args = parser.parse_args()
    repository = Path(__file__).resolve().parents[1]
    source = repository / 'source'
    projects = sorted((load_project(path) for path in args.projects), key=lambda row: row[2])
    if not projects or projects[0][2] != 2:
        raise ValueError('Include the complete N=2 photon process for normalization')
    if len({n for _, _, n in projects}) != len(projects):
        raise ValueError('Supply exactly one complete export per photon multiplicity')
    if len({data['Precision'] for _, data, _ in projects}) != 1:
        raise ValueError('Use the same SPINAS scalar precision for all multiplicities')
    parameter_defaults = [dict(zip(data['ParameterOrder'], data['ParameterDefaults'])) for _, data, _ in projects]
    if any(default != parameter_defaults[0] for default in parameter_defaults):
        raise ValueError('Use the same masses and couplings for all multiplicities')
    args.output.mkdir(parents=True, exist_ok=True)
    sources, declarations, registrations, provenance = [], [], [], []
    for directory, data, n in projects:
        name = data['Class']
        generated = args.output / (name + '-adapter.cpp')
        generated.write_text(adapter(directory, data, n, source))
        sources.extend([directory / 'process.cpp', generated])
        declarations.append('void add_' + name + '(std::vector<hepcat_benchmark::Entry>&);')
        registrations.append('  add_' + name + '(entries);')
        record = {'N': n, 'Export': str(directory), 'Precision': data['Precision'],
                  'CodeStyle': data.get('CodeStyle', 'Legacy'),
                  'CodeLayout': data.get('CodeLayout', data.get('CodeStyle', 'Unspecified')),
                  'Optimization': data.get('Optimization', {'Status': 'Legacy'}),
                  'SHA256': {f: sha(directory / f) for f in ('manifest.json', 'process.hpp', 'process.cpp')}}
        amplitude_source = directory / 'benchmark-amplitudes.json'
        if amplitude_source.is_file():
            record['AmplitudeSource'] = json.loads(amplitude_source.read_text())
            record['SHA256']['benchmark-amplitudes.json'] = sha(amplitude_source)
        else:
            record['AmplitudeSource'] = {'PolePreference': 'Unspecified',
                                         'Note': 'Existing export; representation was not independently classified.'}
        provenance.append(record)
    registry = args.output / 'registry.cpp'
    registry.write_text('#include "benchmark.hpp"\n' + '\n'.join(declarations) +
                        '\nnamespace hepcat_benchmark {\nvoid register_processes(std::vector<Entry>& entries) {\n' +
                        '\n'.join(registrations) + '\n}\n}\n')
    sources.append(registry)
    cmake = []
    inputs = []
    for path in sources:
        if ';' in str(path) or ']==]' in str(path):
            raise ValueError('Unsupported CMake path: ' + str(path))
        cmake.append('list(APPEND BENCHMARK_SOURCES [==[' + str(path) + ']==])')
    cmake.append('set(BENCHMARK_LONG_DOUBLE ' + ('ON' if projects[0][1]['Precision'] == 'LongDouble' else 'OFF') + ')')
    revision = subprocess.run(['git', '-C', str(repository), 'rev-parse', 'HEAD'], text=True,
                              capture_output=True, check=True).stdout.strip()
    support = list((repository / 'benchmarks').glob('*.cpp')) + list((repository / 'benchmarks').glob('*.hpp'))
    support += list(source.glob('*.hpp')) + [source / 'madgraph-runtime.py']
    support += [repository / 'benchmarks' / name for name in ('configure.py', 'CMakeLists.txt')]
    model_inputs = []
    for directory, data, _ in projects:
        inputs.extend(directory.glob('*.hpp'))
        inputs.extend(directory / f for f in ('process.cpp', 'manifest.json'))
        if (directory / 'benchmark-amplitudes.json').is_file():
            inputs.append(directory / 'benchmark-amplitudes.json')
        ch = data['CalcHEPValidation']
        model_inputs.extend(Path(ch['ModelDirectory']) / (prefix + str(ch['ModelNumber']) + '.mdl')
                            for prefix in ('vars', 'func', 'prtcls', 'lgrng', 'extlib'))
    model_inputs = sorted({p.resolve() for p in model_inputs if p.is_file()})
    library_inputs = sorted(args.calchep_directory.glob('lib/*'))
    library_inputs = [p for p in library_inputs if p.is_file() and p.suffix in ('.a', '.so')]
    compiler_inputs = [args.calchep_directory / 'FlagsForSh', args.calchep_directory / 'sbin' / 'newProcess',
                       args.spinas_library.parent / 'CMakeFiles' / 'spinas_static.dir' / 'flags.make']
    compiler_inputs = [p for p in compiler_inputs if p.is_file()]
    inputs += support + model_inputs + library_inputs + compiler_inputs + [args.spinas_library]
    for path in sorted(set(inputs)):
        if ';' in str(path) or ']==]' in str(path):
            raise ValueError('Unsupported CMake input path: ' + str(path))
        cmake.append('set_property(DIRECTORY APPEND PROPERTY CMAKE_CONFIGURE_DEPENDS [==[' + str(path) + ']==])')
    (args.output / 'projects.cmake').write_text('\n'.join(cmake) + '\n')
    info = {'RepositoryRevision': revision, 'Compiler': args.compiler, 'CompilerVersion': args.compiler_version,
            'CXXFlags': args.flags, 'SPINASLibrary': str(args.spinas_library.resolve()),
            'SPINASLibrarySHA256': sha(args.spinas_library), 'CalcHEPDirectory': str(args.calchep_directory.resolve()),
            'ReferenceOptimization': '-O3 (private builds)', 'Exports': provenance,
            'SupportSHA256': {str(p.relative_to(repository)): sha(p) for p in support},
            'ModelSHA256': {str(p): sha(p) for p in model_inputs},
            'ExportHeadersSHA256': {str(p): sha(p) for p in inputs if p.suffix == '.hpp' and p.parent != source},
            'CalcHEPLibrarySHA256': {str(p): sha(p) for p in library_inputs},
            'CompilerConfigurations': {str(p): p.read_text() for p in compiler_inputs}}
    (args.output / 'build-info.json').write_text(json.dumps(info, indent=2) + '\n')


if __name__ == '__main__':
    main()
