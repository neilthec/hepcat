#!/usr/bin/env python3
"""Replay a compiled HEPCAT export against genuine binary128 CalcHEP arithmetic.

The process, particle count, model and parameters come from the export. No
amplitude formula is supplied by this tool. It saves the native failure and
reports the precision replay separately; it never changes workflow PASS counts.
Requires a Unix Makefiles export build, GCC with libquadmath, and CalcHEP sources.
"""
# HEPCAT - High Energy Physics Constructive Amplitude Tool
# Copyright (C) 2026 Neil Christensen
# SPDX-License-Identifier: GPL-3.0-or-later
from __future__ import annotations
import argparse
import csv
import datetime as dt
import hashlib
import json
import math
import os
from pathlib import Path
import re
import secrets
import shlex
import shutil
import signal
import subprocess
import sys
import time


def sha256(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def load_manifest(directory):
    data = json.loads((directory / 'manifest.json').read_text())
    config = data.get('CalcHEPValidation', {})
    name = data.get('Class', '')
    count = len(data.get('Masses', []))
    if not re.fullmatch(r'[A-Za-z_][A-Za-z_0-9]*', name):
        raise ValueError('The manifest must name a generated C++ class.')
    if not 4 <= count <= 20 or len(config.get('PhysicalPDGs', [])) != count:
        raise ValueError('The manifest has inconsistent external-particle counts.')
    if config.get('Status') != 'AVAILABLE' or not config.get('Supported', False):
        raise ValueError('The exported process does not support an unpolarized CalcHEP comparison.')
    if data.get('IncomingLegs') != [1, 2] or config.get('LegOrder') != list(range(1, count + 1)):
        raise ValueError('This replay requires the export to place its two incoming legs first.')
    names = data.get('ParameterOrder', [])
    if any(name not in names for name in config.get('ParameterNames', [])):
        raise ValueError('CalcHEP parameter mapping is not represented by the export parameter order.')
    return data, config, count


def normalize_points(source, target, count):
    columns = ['point'] + [f'p{leg}_{component}' for leg in range(1, count + 1) for component in range(4)]
    with source.open(newline='') as stream:
        reader = csv.DictReader(stream)
        if not set(columns[1:]).issubset(reader.fieldnames or []):
            raise ValueError('The point CSV must contain p1_0 through pN_3.')
        rows = []
        used = set()
        for number, row in enumerate(reader, 1):
            point = int(row.get('point', number))
            if point <= 0 or point in used:
                raise ValueError('Point IDs must be distinct positive integers.')
            used.add(point)
            values = [float(row[key]) for key in columns[1:]]
            if not all(math.isfinite(value) for value in values):
                raise ValueError('All saved momentum components must be finite.')
            rows.append([point, *values])
    if not rows:
        raise ValueError('The saved point CSV is empty.')
    with target.open('w', newline='') as stream:
        writer = csv.writer(stream)
        writer.writerow(columns)
        writer.writerows(rows)
    return len(rows)


def run(command, directory, log, deadline, environment=None):
    remaining = deadline - time.monotonic()
    if remaining <= 0:
        raise TimeoutError('The total precision-replay deadline expired.')
    with log.open('w') as output:
        process = subprocess.Popen(command, cwd=directory, env=environment,
            stdout=output, stderr=subprocess.STDOUT, start_new_session=True)
        try:
            code = process.wait(timeout=remaining)
        except subprocess.TimeoutExpired:
            os.killpg(process.pid, signal.SIGTERM)
            try:
                process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                os.killpg(process.pid, signal.SIGKILL)
                process.wait()
            raise TimeoutError(f'Precision replay timed out: {log}')
        except KeyboardInterrupt:
            os.killpg(process.pid, signal.SIGTERM)
            try:
                process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                os.killpg(process.pid, signal.SIGKILL)
                process.wait()
            raise
    if code:
        raise RuntimeError(f'Command failed ({code}); inspect {log}')


def cpp_list(values):
    return '{' + ','.join(json.dumps(value, allow_nan=False) for value in values) + '}'


CAPTURE = r"""
#include "process.hpp"
#include "calchep-runtime.hpp"
#include <fstream>
#include <iomanip>
#include <iostream>
int main(){try{
 using Process=hepcat_generated::@CLASS@;
 Process process;process.set_widths_enabled(false);
 const auto values=process.get_parameters();
 std::vector<hepcat_spinas::CalcHEPParameter> parameters={@PARAMETERS@};
 hepcat_spinas::CalcHEPProcess reference(@MODEL@,@MODELNUMBER@,@PDGS@,parameters,@GG@,true,@ALLOWED@,0,@MASSES@);
 hepcat_spinas::check_calchep_masses(process.masses(),reference.masses());
 std::ofstream state(@STATE@);state<<reference.work_directory()<<'\n'<<reference.process_name()<<'\n';
 for(std::size_t i=0;i<reference.excluded_particle_names().size();++i){if(i)state<<',';state<<reference.excluded_particle_names()[i];}state<<'\n';state.close();
 auto text=reference.process_name();std::vector<char> description(text.begin(),text.end());description.push_back(0);
 char library[]="hepcat_validation",empty[]="";
 numout* code=getMEcode(0,1,description.data(),NULL,empty,library);
 if(!code)throw std::runtime_error("The cached CalcHEP interface is unavailable.");
 std::ofstream params(@PARAMCSV@);params<<"name,hex_value\n"<<std::hexfloat;
 for(int i=1;i<=code->interface->nvar;++i)params<<code->interface->varName[i]<<','<<static_cast<long double>(code->interface->va[i])<<'\n';
 params.close();
 std::ofstream out(@POINTCSV@);out<<std::setprecision(17)<<"point,hepcat,calchep";
 for(std::size_t leg=1;leg<=Process::leg_count;++leg)for(int component=0;component<4;++component)out<<",p"<<leg<<'_'<<component;out<<'\n';
 auto evaluate=[&](std::size_t id,const Process::Momenta& point){
  const auto validation=hepcat_spinas::validate_momenta(point,process.masses(),Process::incoming_legs());
  if(!validation.valid)throw std::runtime_error("Saved point failed kinematic checks: "+validation.message);
  process.set_momenta(point);const double actual=double(process.amp2()*@NORMALIZATION@);
  const double expected=reference.squared_amplitude(point);
  if(!std::isfinite(actual)||!std::isfinite(expected)||actual<0||expected<0)throw std::runtime_error("Invalid squared amplitude.");
  out<<id<<','<<actual<<','<<expected;for(const auto& leg:point)for(auto value:leg)out<<','<<double(value);out<<'\n';
 };
 @POINTLOOP@
 std::cout<<"Captured native comparison for "<<reference.process_name()<<'\n';return 0;
}catch(const std::exception& error){std::cerr<<error.what()<<'\n';return 1;}}
"""

PROBE = r"""
#include <dlfcn.h>
#include <float.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "num_out.h"
int main(int argc,char**argv){
 if(argc!=7)return 1;
 void*handle=dlopen(argv[1],RTLD_NOW|RTLD_LOCAL);if(!handle){fprintf(stderr,"%s\n",dlerror());return 2;}
 CalcHEP_interface**exported=dlsym(handle,"PtrInterface_hepcat_precision");if(!exported||!*exported)return 3;
 CalcHEP_interface*interface=*exported;const int legs=atoi(argv[6]);
 if(interface->nin!=2||interface->nout+2!=legs||interface->nprc!=1)return 4;
 FILE*parameters=fopen(argv[3],"r");if(!parameters)return 5;char line[16384];fgets(line,sizeof line,parameters);
 int assigned[1024]={0};if(interface->nvar>=1024)return 6;
 while(fgets(line,sizeof line,parameters)){
  char*comma=strchr(line,',');if(!comma)return 7;*comma++=0;
  int index=0;for(int i=1;i<=interface->nvar;++i)if(!strcmp(line,interface->varName[i]))index=i;
  if(!index)return 8;
#ifdef _QUADGCC_
  interface->va[index]=strtoflt128(comma,NULL);
#elif defined(_LONG_)
  interface->va[index]=strtold(comma,NULL);
#else
  interface->va[index]=strtod(comma,NULL);
#endif
  assigned[index]=1;
 }
 fclose(parameters);for(int i=1;i<=interface->nvar;++i)if(!assigned[i])return 9;
 *interface->twidth=0;*interface->gtwidth=0;*interface->gswidth=0;if(interface->calcFunc())return 10;
 /* CalcHEP may sort external states. Recover the physical-to-reference map by PDG. */
 int pdgs[20]={@PDGS_C@},order[20],used[20]={0};
 for(int i=0;i<legs;i++){int pdg=0;interface->pinf(1,i+1,NULL,&pdg);order[i]=-1;
  for(int j=0;j<legs;j++)if(!used[j]&&pdgs[j]==pdg){order[i]=j;used[j]=1;break;}if(order[i]<0)return 11;}
 FILE*input=fopen(argv[2],"r"),*output=fopen(argv[4],"w");if(!input||!output)return 12;
 fprintf(output,"point,hepcat,original_calchep,replayed_calchep,relative_error,absolute_error\n");fgets(line,sizeof line,input);
 int points=0,mismatches=0,errors=0,matches=0;double maxrelative=0,maxabsolute=0,tolerance=strtod(argv[5],NULL);
 while(fgets(line,sizeof line,input)){
  double row[83];char*cursor=line;for(int i=0;i<3+4*legs;i++){char*end;row[i]=strtod(cursor,&end);if(end==cursor)return 13;cursor=*end==','?end+1:end;}
  REAL momenta[80];for(int leg=0;leg<legs;leg++)for(int j=0;j<4;j++)momenta[4*leg+j]=(REAL)row[3+4*order[leg]+j];
  int error=0;double value=interface->sqme(1,@GG@,momenta,NULL,&error);points++;
  if(error||!isfinite(value)||value<0)errors++;if(value==row[2])matches++;
  double absolute=fabs(value-row[1]),scale=fmax(fabs(value),fabs(row[1])),relative=scale?absolute/scale:0;
  if(relative>tolerance)mismatches++;if(relative>maxrelative)maxrelative=relative;if(absolute>maxabsolute)maxabsolute=absolute;
  fprintf(output,"%.0f,%.17g,%.17g,%.17g,%.17g,%.17g\n",row[0],row[1],row[2],value,relative,absolute);
 }
 fclose(input);fclose(output);
#ifdef _QUADGCC_
 const int bits=FLT128_MANT_DIG;
#elif defined(_LONG_)
 const int bits=LDBL_MANT_DIG;
#else
 const int bits=DBL_MANT_DIG;
#endif
 printf("{\"MantissaBits\":%d,\"REALBytes\":%zu,\"Points\":%d,\"Mismatches\":%d,\"Errors\":%d,\"OriginalBitMatches\":%d,\"MaximumRelativeError\":%.17g,\"MaximumAbsoluteError\":%.17g}\n",bits,sizeof(REAL),points,mismatches,errors,matches,maxrelative,maxabsolute);
 dlclose(handle);return errors?14:0;
}
"""


def render(template, substitutions):
    for name, value in substitutions.items():
        template = template.replace('@' + name + '@', str(value))
    if re.search(r'@[A-Z_]+@', template):
        raise ValueError('An audit source placeholder was not resolved.')
    return template


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('export_directory', type=Path)
    parser.add_argument('--output', type=Path, help='New directory for all evidence and private builds.')
    parser.add_argument('--seed', type=int, help='Seed for new points; recorded even when saved points are supplied.')
    parser.add_argument('--points', type=int, help='New point count; defaults to the exported comparison count.')
    parser.add_argument('--points-csv', type=Path, help='Reuse these exact momenta; recompute the current export values.')
    parser.add_argument('--calchep', type=Path, help='Override the CalcHEP installation from the manifest.')
    parser.add_argument('--gcc', default=shutil.which('gcc-16') or shutil.which('gcc-15'), help='GCC with libquadmath.')
    parser.add_argument('--relative-tolerance', type=float, default=1e-7)
    parser.add_argument('--timeout', type=float, default=1800)
    args = parser.parse_args()
    if (not args.gcc or not math.isfinite(args.timeout) or args.timeout <= 0
            or not math.isfinite(args.relative_tolerance) or args.relative_tolerance < 0
            or args.points is not None and args.points <= 0
            or args.seed is not None and not 0 <= args.seed < 2**64):
        parser.error('Provide GCC/libquadmath, finite positive limits, and a valid unsigned64-bit seed.')
    export = args.export_directory.resolve()
    manifest, config, count = load_manifest(export)
    calchep = (args.calchep or Path(config['CalcHEPDirectory'])).resolve()
    stamp = dt.datetime.now(dt.timezone.utc).strftime('%Y%m%dT%H%M%S%f')
    output = (args.output or export / 'build/precision-replay' / stamp).resolve()
    output.mkdir(parents=True, exist_ok=False)
    snapshot = output / 'export-snapshot'
    snapshot.mkdir()
    export_sources = {path.name: sha256(path) for pattern in ('*.cpp', '*.hpp', 'manifest.json')
                      for path in sorted(export.glob(pattern))}
    for name in export_sources:
        shutil.copy2(export / name, snapshot / name)
    shutil.copy2(Path(__file__), output / 'replay-tool.py')
    deadline = time.monotonic() + args.timeout
    seed = args.seed if args.seed is not None else secrets.randbelow(2**64)
    points = args.points or int(config.get('Points', 1000))
    if args.points_csv:
        points = normalize_points(args.points_csv, output / 'input-points.csv', count)
        if args.points is not None and args.points != points:
            raise ValueError('The saved point count differs from --points; no rows are dropped.')
        shutil.copy2(args.points_csv, output / 'supplied-points.csv')
    names = manifest['ParameterOrder']
    parameters = ','.join('{' + json.dumps(name) + ',hepcat_spinas::checked_calchep_parameter(values[' + str(names.index(name)) + '],' + json.dumps(name) + ')}'
                          for name in config['ParameterNames'])
    gg_index = names.index('GG') if 'GG' in names else None
    gg = float(manifest['ParameterDefaults'][gg_index]['Real']) if gg_index is not None else 1.0
    if args.points_csv:
        loop = '''std::ifstream input(@INPUT@);std::string line;std::getline(input,line);
 while(std::getline(input,line)){std::stringstream row(line);std::string token;std::getline(row,token,',');std::size_t id=std::stoull(token);Process::Momenta point={};
 for(auto& leg:point)for(auto& component:leg){if(!std::getline(row,token,','))throw std::runtime_error("Incomplete point row");component=std::stod(token);}evaluate(id,point);}'''.replace('@INPUT@', json.dumps(str(output / 'input-points.csv')))
    else:
        loop = f'''hepcat_spinas::ValidationOptions options;options.points={points};options.seed={seed}ULL;
 std::mt19937_64 engine(options.seed);for(std::size_t i=0;i<options.points;++i)evaluate(i+1,hepcat_spinas::make_random_validation_momenta(process.masses(),Process::incoming_legs(),engine,options));'''
    substitutions = {'CLASS': manifest['Class'], 'PARAMETERS': parameters,
        'MODEL': json.dumps(config['ModelDirectory']), 'MODELNUMBER': config['ModelNumber'],
        'PDGS': cpp_list(config['PhysicalPDGs']), 'ALLOWED': cpp_list(config.get('AllowedParticlePDGs', [])),
        'MASSES': cpp_list(config.get('IndependentMassParameters', [])), 'GG': repr(gg),
        'STATE': json.dumps(str(output / 'reference-state.txt')),
        'PARAMCSV': json.dumps(str(output / 'parameters.csv')), 'POINTCSV': json.dumps(str(output / 'points.csv')),
        'NORMALIZATION': repr(float(config.get('SPINASNormalizationFactor', 1))), 'POINTLOOP': loop}
    (output / 'capture.cpp').write_text(render(CAPTURE, substitutions))
    link = export / 'build/CMakeFiles' / (manifest['Class'] + '_validate_calchep.dir/link.txt')
    command = shlex.split(link.read_text())
    linked = [(Path(value) if Path(value).is_absolute() else export / 'build' / value).resolve()
              for value in command if Path(value).suffix in ('.a', '.so', '.dylib')]
    linked_hashes = {str(path): sha256(path) for path in linked if path.is_file()}
    objects = [i for i, value in enumerate(command) if value.endswith('/calchep-main.cpp.o')]
    if len(objects) != 1:
        raise ValueError('Expected one calchep-main.cpp object in the Unix Makefiles link command.')
    command[objects[0]] = str(output / 'capture.cpp')
    command[command.index('-o') + 1] = str(output / 'capture')
    command += ['-std=c++11'] + (['-DWITH_LONG_DOUBLE'] if manifest['Precision'] == 'LongDouble' else [])
    spinas_header = next(line.split('=', 1)[1].strip() for line in (export / 'build/CMakeCache.txt').read_text().splitlines()
                         if line.startswith('SPINAS_INCLUDE_DIR:'))
    for path in (export, Path(spinas_header), calchep / 'include', calchep / 'c_source/dynamicME/include', calchep / 'c_source/ntools/include'):
        command += ['-I' + str(path)]
    run(command, export / 'build', output / 'build-capture.log', deadline)
    run([str(output / 'capture')], output, output / 'capture.log', deadline)
    workspace, process, exclusions = (output / 'reference-state.txt').read_text().splitlines()[:3]
    native = output / 'reference-native'
    for name in ('models', 'results', 'tmp', 'safe', 'so_generated'):
        (native / name).mkdir(parents=True, exist_ok=True)
    for path in (Path(workspace) / 'aux/models').glob('*.mdl'):
        shutil.copy2(path, native / 'models' / path.name)
    env = dict(os.environ, nParProc='1')
    run([str(calchep / 'sbin/newProcess'), 'hepcat_precision', str(native / 'so_generated'), '', 'U', process, exclusions, ''],
        native, output / 'generate-reference.log', deadline, env)
    sources = native / 'results'
    symbolic_hashes = {path.name: sha256(path) for path in sorted(sources.glob('*.c'))}
    if not symbolic_hashes:
        raise RuntimeError('CalcHEP did not leave symbolic C sources for the audit.')
    probe = render(PROBE, {'PDGS_C': ','.join(str(value) for value in config['PhysicalPDGs']), 'GG': repr(gg)})
    (output / 'replay.c').write_text(probe)
    commands = []
    results = {}
    shared = '-dynamiclib' if sys.platform == 'darwin' else '-shared'
    for mode, quad in (('Native', None), ('GCC53', False), ('GCC128', True)):
        directory = native if quad is None else output / ('reference-' + mode)
        if quad is not None:
            directory.mkdir()
            for path in sources.iterdir():
                if path.suffix in ('.c', '.h', '.inc'):
                    shutil.copy2(path, directory / path.name)
            numeric = directory / 'nType.h'
            numeric.write_text(re.sub(r'^\s*#\s*define\s+_LONG_\b.*$', '// _LONG_ disabled only in this private precision audit', numeric.read_text(), flags=re.M))
            flags = ['-g', '-std=gnu99', '-fsigned-char', '-fPIC'] + (['-D_QUADGCC_'] if quad else [])
            helper = directory / 'sqme_aux.so'
            library = directory / 'hepcat_precision.so'
            for cmd in ([args.gcc, *flags, shared, '-I' + str(directory), str(calchep / 'c_source/num/sqme_aux.c'), str(calchep / 'c_source/num/ferror.c'), '-o', str(helper), '-lm', *(['-lquadmath'] if quad else [])],
                        [args.gcc, *flags, shared, '-I' + str(directory), *[str(directory / name) for name in symbolic_hashes], str(helper), '-o', str(library), '-lm', *(['-lquadmath'] if quad else [])]):
                commands.append(cmd)
                run(cmd, directory, output / ('build-' + mode + '-' + str(len(commands)) + '.log'), deadline)
            includes = directory
        else:
            library = native / 'so_generated/hepcat_precision.so'
            includes = sources
        executable = output / ('replay-' + mode)
        cmd = [args.gcc, '-std=gnu99', '-g', *(['-D_QUADGCC_'] if quad else []), '-I' + str(includes), str(output / 'replay.c'), '-o', str(executable), '-lm', *(['-lquadmath'] if quad else []), *(['-ldl'] if sys.platform != 'darwin' else [])]
        commands.append(cmd)
        run(cmd, output, output / ('build-probe-' + mode + '.log'), deadline)
        run([str(executable), str(library), str(output / 'points.csv'), str(output / 'parameters.csv'), str(output / (mode + '.csv')), str(args.relative_tolerance), str(count)], output, output / (mode + '.log'), deadline)
        results[mode] = json.loads((output / (mode + '.log')).read_text())
        results[mode]['Status'] = 'PASS' if results[mode]['Mismatches'] == 0 and results[mode]['Errors'] == 0 else 'FAIL'
        if results[mode]['Points'] != points:
            raise RuntimeError('The replay did not evaluate every requested point.')
    if any(not (export / name).is_file() or sha256(export / name) != value for name, value in export_sources.items()):
        raise RuntimeError('Export sources changed while the replay was running.')
    if any(not Path(name).is_file() or sha256(Path(name)) != value for name, value in linked_hashes.items()):
        raise RuntimeError('A linked numerical library changed while the replay was running.')
    native_reproduced = results['Native']['OriginalBitMatches'] == points
    extended = results['GCC128']
    complete = native_reproduced and extended['MantissaBits'] == 113 and extended['REALBytes'] == 16 and extended['Mismatches'] == 0 and extended['Errors'] == 0
    summary = {'SchemaVersion': 1, 'Status': 'PRECISION_REPLAY_PASS' if complete else 'PRECISION_REPLAY_FAIL',
        'Scope': 'Independent numerical precision replay, not automatic workflow admission.', 'Seed': seed,
        'Points': points, 'ExternalCount': count, 'RelativeTolerance': args.relative_tolerance,
        'Results': results, 'NativeControlReproducesCapture': native_reproduced,
        'ExportSourcesSHA256': export_sources, 'LinkedLibrariesSHA256': linked_hashes,
        'ManifestSHA256': sha256(export / 'manifest.json'),
        'PointsSHA256': sha256(output / 'points.csv'), 'ParametersSHA256': sha256(output / 'parameters.csv'),
        'SymbolicCFiles': symbolic_hashes, 'ToolSHA256': sha256(Path(__file__)),
        'Kinematics': 'Common binary64 momenta are reused exactly; no projection or correction.',
        'WidthPolicy': 'Private reference widths and width flags zero; SPINAS propagator widths disabled.'}
    (output / 'compiler-commands.json').write_text(json.dumps(commands, indent=2) + '\n')
    (output / 'precision-summary.json').write_text(json.dumps(summary, indent=2) + '\n')
    print(json.dumps(summary, indent=2))
    return 0 if complete else 1


if __name__ == '__main__':
    try:
        raise SystemExit(main())
    except (OSError, ValueError, RuntimeError, TimeoutError) as error:
        print(f'Precision replay did not complete: {error}', file=sys.stderr)
        raise SystemExit(2)
