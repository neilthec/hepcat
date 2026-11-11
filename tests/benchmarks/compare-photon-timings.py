#!/usr/bin/env python3
"""Validate and combine two complete, paired HEPCAT photon benchmark runs.

Example:
  python3 tests/benchmarks/compare-photon-timings.py --physical /path/physical \
    --spurious /path/spurious --output /path/fourway

Input arguments are filename prefixes, not directories. No amplitude or timing
is generated here. CalcHEP and MadGraph table times use the physical-run
medians. Pooled and separate run medians and ranges remain in JSON.
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
import csv
import hashlib
import itertools
import json
import math
from pathlib import Path
import statistics
import sys


def require(condition, message):
    if not condition:
        raise ValueError(message)


def file_for(prefix, suffix):
    return Path(str(prefix) + suffix)


def read_json(path):
    return json.loads(path.read_text())


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def read_csv(path):
    with path.open(newline='') as handle:
        return list(csv.DictReader(handle))


def relative_error(left, right):
    require(math.isfinite(left) and math.isfinite(right) and min(left, right) >= 0,
            'Nonfinite or negative squared amplitude')
    scale = max(abs(left), abs(right))
    return abs(left - right) / scale if scale else 0.0


def timing_stats(values):
    require(values and all(math.isfinite(x) and x > 0 for x in values),
            'Missing or invalid positive timing')
    median = statistics.median(values)
    return {'MedianSeconds': median, 'MinimumSeconds': min(values),
            'MaximumSeconds': max(values),
            'MedianAbsoluteDeviationSeconds': statistics.median(abs(x-median) for x in values),
            'Repetitions': len(values), 'Seconds': values}


def inspect(prefix, representation, points, export_root=None):
    metadata_file = file_for(prefix, '-metadata.json')
    metadata = read_json(metadata_file)
    require(metadata['Status'] == 'COMPLETE' and metadata['ValidatedRows'] == 'PASS',
            f'{prefix}: benchmark did not complete and validate')
    require(metadata['RequestedPhotonCounts'] == [2, 3, 4] and
            metadata['CompletedPhotonCounts'] == [2, 3, 4],
            f'{prefix}: expected exactly N=2,3,4')
    require(metadata['Points'] == points, f'{prefix}: expected {points} points')
    require(metadata['SPINASScalarType'] == 'double' and metadata['SPINASPrecisionBits'] == 53
            and metadata['ReferencePrecision'] == 'double',
            f'{prefix}: expected common double precision')
    exports = {item['N']: item for item in metadata['Build']['Exports']}
    require(set(exports) == {2, 3, 4}, f'{prefix}: incomplete export provenance')
    inputs = {str(metadata_file): digest(metadata_file)}
    manifest_info = {}
    for photons in [2, 3, 4]:
        item = exports[photons]
        require(item['Precision'] == 'Double' and item['CodeStyle'] == 'Optimized',
                f'{prefix} N={photons}: expected Optimized Double export')
        source = item['AmplitudeSource']
        require(source['Representation'] == representation and
                source['CompletePhotonHelicityCount'] == 2**photons,
                f'{prefix} N={photons}: incomplete or wrong amplitude source')
        sectors = source['InputHelicitySectors']
        expected_sectors = {
            tuple(['e', 'E'] + ['A.+'] * (photons-negative) + ['A.-'] * negative)
            for negative in range(photons+1)}
        require(len(sectors) == photons+1 and
                {tuple(sector['Particles']) for sector in sectors} == expected_sectors and
                all(sector.get('ExpressionSHA256') for sector in sectors),
                f'{prefix} N={photons}: canonical helicity source inventory is incomplete')
        expected_model = 3 if representation == 'AllPhysical' else 4
        require(all(f'{stem}{expected_model}.mdl' in source['ModelFilesSHA256']
                    for stem in ['amps', 'prtcls', 'vars', 'func']),
                f'{prefix} N={photons}: model provenance is missing')
        directory = (Path(export_root) / Path(item['Export']).name
                     if export_root is not None else Path(item['Export']))
        for filename, expected_hash in item['SHA256'].items():
            path = directory / filename
            require(digest(path) == expected_hash,
                    f'{path}: export changed after benchmark configuration')
            inputs[str(path)] = expected_hash
        manifest = read_json(directory / 'manifest.json')
        expected = {(0, 0) + row for row in itertools.product((-2, 2), repeat=photons)}
        rows = [tuple(row) for row in manifest['HelicityConfigurations']]
        require(set(rows) == expected and len(rows) == len(expected) and
                manifest['HelicityFamily'] and not manifest.get('FixedHelicities') and
                manifest['IncomingLegs'] == [1, 2],
                f'{prefix} N={photons}: full exported helicity inventory is missing')
        for backend in ['CalcHEPValidation', 'MadGraphValidation']:
            config = manifest[backend]
            require(config['AllowedParticlePDGs'] == [11, 22] and
                    config['PhysicalPDGs'] == [11, -11] + [22]*photons and
                    config['ParameterNames'] == manifest['ParameterOrder'],
                    f'{prefix} N={photons}: reference theory or parameters differ')
        require(manifest['CalcHEPValidation']['SPINASNormalizationFactor'] == 1 and
                abs(manifest['MadGraphValidation']['Normalization'] -
                    1/(4*math.factorial(photons))) < 1e-15,
                f'{prefix} N={photons}: reference normalization is incorrect')
        manifest_info[photons] = manifest
    repetition_file = file_for(prefix, '-repetitions.csv')
    repetitions = read_csv(repetition_file)
    timing = {}
    for photons in [2, 3, 4]:
        timing[photons] = {}
        for backend in ['SPINAS', 'CalcHEP', 'MadGraph']:
            rows = [row for row in repetitions if int(row['photons']) == photons and
                    row['backend'] == backend]
            require(len(rows) == metadata['Repetitions'] and
                    sorted(int(row['repetition']) for row in rows) ==
                    list(range(1, metadata['Repetitions']+1)) and
                    all(int(row['points']) == points for row in rows),
                    f'{prefix} N={photons}: incomplete repetition inventory')
            timing[photons][backend] = timing_stats([float(row['seconds']) for row in rows])
    require(len(repetitions) == 9*metadata['Repetitions'],
            f'{prefix}: unexpected duplicate timing rows')
    inputs[str(repetition_file)] = digest(repetition_file)
    return metadata, exports, manifest_info, timing, inputs


def compare(args):
    require(args.points > 0 and math.isfinite(args.tolerance) and args.tolerance > 0,
            'Point count and tolerance must be finite and positive')
    physical = inspect(args.physical, 'AllPhysical', args.points,
                       getattr(args, 'physical_exports', None))
    spurious = inspect(args.spurious, 'AllSpurious', args.points,
                       getattr(args, 'spurious_exports', None))
    pm, pe, pmanifest, pt, pinputs = physical
    sm, se, smanifest, st, sinputs = spurious
    equal_metadata = ['Seed', 'MultiplicitySeed', 'Points', 'Repetitions', 'Warmup',
                      'CenterOfMassEnergyGeV', 'Sampling', 'Normalization', 'MadGraphSum',
                      'SPINASScalarType', 'SPINASPrecisionBits', 'ParametersByMultiplicity',
                      'Timing', 'Excluded', 'ReferencePrecision', 'System']
    for key in equal_metadata:
        require(pm[key] == sm[key], f'Paired metadata differs: {key}')
    for key in ['Compiler', 'CompilerVersion', 'CXXFlags', 'SPINASLibrarySHA256',
                'CalcHEPDirectory', 'ReferenceOptimization', 'ModelSHA256',
                'CalcHEPLibrarySHA256', 'CompilerConfigurations', 'SupportSHA256']:
        require(pm['Build'][key] == sm['Build'][key], f'Paired build differs: {key}')
    require(pm['Tolerance'] <= args.tolerance and sm['Tolerance'] <= args.tolerance,
            'A run admitted points with a looser tolerance')
    point_hashes, errors, timings, references, mg_reports = {}, {}, {}, {}, {}
    for photons in [2, 3, 4]:
        ppoints = file_for(args.physical, f'-points-N{photons}.csv')
        spoints = file_for(args.spurious, f'-points-N{photons}.csv')
        require(digest(ppoints) == digest(spoints),
                f'N={photons}: phase-space points differ between representations')
        point_hashes[photons] = digest(ppoints)
        for key in ['CalcHEPValidation', 'MadGraphValidation', 'ParameterOrder', 'ParameterDefaults']:
            require(pmanifest[photons][key] == smanifest[photons][key],
                    f'N={photons}: paired manifest configuration differs: {key}')
        require(pe[photons]['AmplitudeSource']['ReferenceConfigurationSHA256'] ==
                se[photons]['AmplitudeSource']['ReferenceConfigurationSHA256'],
                f'N={photons}: reference configuration provenance differs')
        pmgfile = file_for(args.physical, f'-madgraph-N{photons}.json')
        smgfile = file_for(args.spurious, f'-madgraph-N{photons}.json')
        pmg, smg = read_json(pmgfile), read_json(smgfile)
        for report in [pmg, smg]:
            require(report['Status'] == 'READY' and report['HelicityStates'] == 4*2**photons
                    and report['AllowedParticlePDGs'] == [11,22] and
                    report['OptimizationLevel'] == 3,
                    f'N={photons}: MadGraph did not provide the exhaustive optimized QED sum')
        for key in ['ExternalCount', 'Process', 'PhysicalPDGs', 'Masses',
                    'AllowedParticlePDGs', 'ExcludedParticles', 'DiagramCount',
                    'MadGraphModel', 'MadGraphVersion', 'MadGraphModelPath',
                    'MadGraphRestriction', 'FortranCompiler', 'ParameterMatching',
                    'MassMatching', 'WidthPolicy', 'ZeroedWidths', 'PreservedWidths',
                    'ParameterRules', 'CouplingOrders', 'Scope', 'Normalization',
                    'SpinSum', 'HelicityStates', 'OptimizationLevel', 'FortranFlags']:
            require(pmg[key] == smg[key],
                    f'N={photons}: independent MadGraph build provenance differs: {key}')
        pinputs[str(pmgfile)], sinputs[str(smgfile)] = digest(pmgfile), digest(smgfile)
        mg_reports[photons] = {'PhysicalRun': pmg, 'SpuriousRun': smg}
        pvfile = file_for(args.physical, f'-values-N{photons}.csv')
        svfile = file_for(args.spurious, f'-values-N{photons}.csv')
        pv, sv = read_csv(pvfile), read_csv(svfile)
        pinputs[str(pvfile)], sinputs[str(svfile)] = digest(pvfile), digest(svfile)
        require(len(pv) == args.points and len(sv) == args.points,
                f'N={photons}: pointwise comparison inventory is incomplete')
        maxima = {name: 0.0 for name in ['PhysicalVsSpurious', 'PhysicalVsCalcHEP',
            'PhysicalVsMadGraph', 'SpuriousVsCalcHEP', 'SpuriousVsMadGraph',
            'CalcHEPAcrossRuns', 'MadGraphAcrossRuns', 'CalcHEPVsMadGraph']}
        for index, (p, s) in enumerate(zip(pv, sv), 1):
            require(int(p['point']) == index and int(s['point']) == index,
                    f'N={photons}: point ordering differs')
            a, b = float(p['spinas']), float(s['spinas'])
            c, d = float(p['calchep']), float(p['madgraph'])
            c2, d2 = float(s['calchep']), float(s['madgraph'])
            pairs = {'PhysicalVsSpurious': (a,b), 'PhysicalVsCalcHEP': (a,c),
                'PhysicalVsMadGraph': (a,d), 'SpuriousVsCalcHEP': (b,c2),
                'SpuriousVsMadGraph': (b,d2), 'CalcHEPAcrossRuns': (c,c2),
                'MadGraphAcrossRuns': (d,d2), 'CalcHEPVsMadGraph': (c,d)}
            for name, (left, right) in pairs.items():
                error = relative_error(left, right)
                maxima[name] = max(maxima[name], error)
                require(error <= args.tolerance,
                        f'N={photons} point={index} {name} relative error {error} exceeds tolerance')
        errors[photons] = maxima
        timings[photons] = {
            'HEPCAT_SPINAS_AllPhysical': pt[photons]['SPINAS'],
            'HEPCAT_SPINAS_AllSpurious': st[photons]['SPINAS'],
            'CalcHEP': pt[photons]['CalcHEP'],
            'MadGraph': pt[photons]['MadGraph']}
        references[photons] = {backend: {
            'PhysicalRun': pt[photons][backend], 'SpuriousRun': st[photons][backend],
            'PooledRuns': timing_stats(pt[photons][backend]['Seconds']+
                                      st[photons][backend]['Seconds']),
            'SpuriousRunMedianOverPhysicalRunMedian':
                st[photons][backend]['MedianSeconds']/pt[photons][backend]['MedianSeconds']}
            for backend in ['CalcHEP', 'MadGraph']}
    out_csv = file_for(args.output, '.csv')
    out_json = file_for(args.output, '-summary.json')
    backends = ['HEPCAT_SPINAS_AllPhysical', 'HEPCAT_SPINAS_AllSpurious', 'CalcHEP', 'MadGraph']
    out_csv.parent.mkdir(parents=True, exist_ok=True)
    with out_csv.open('w', newline='') as handle:
        fields = ['photons', 'points']+[name+'_seconds' for name in backends]+[
            name+'_relative_N2' for name in backends]
        writer = csv.DictWriter(handle, fieldnames=fields)
        writer.writeheader()
        for photons in [2, 3, 4]:
            row = {'photons': photons, 'points': args.points}
            for name in backends:
                value = timings[photons][name]['MedianSeconds']
                row[name+'_seconds'] = value
                row[name+'_relative_N2'] = value/timings[2][name]['MedianSeconds']
            writer.writerow(row)
    summary = {'Status': 'PASS', 'PointsPerMultiplicity': args.points, 'PhotonCounts': [2,3,4],
        'Tolerance': args.tolerance, 'Seed': pm['Seed'],
        'CenterOfMassEnergyGeV': pm['CenterOfMassEnergyGeV'],
        'TimingDefinition': pm['Timing'], 'ExcludedFromTiming': pm['Excluded'],
        'ReferenceTimingCombination': 'Physical-run median in table; separate and pooled medians retained in SeparateReferenceStatistics.',
        'CommonPointSHA256': point_hashes, 'MaximumRelativeErrors': errors,
        'TimingStatistics': timings, 'SeparateReferenceStatistics': references,
        'MadGraphBuildReports': mg_reports,
        'PhysicalPrefix': str(args.physical), 'SpuriousPrefix': str(args.spurious),
        'InputSHA256': dict(pinputs, **sinputs),
        'PhysicalMetadata': pm, 'SpuriousMetadata': sm}
    out_json.write_text(json.dumps(summary, indent=2)+'\n')
    print(f'PASS: all {3*args.points} common points agree in all four ways.')
    print(out_csv)
    print(out_json)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--physical', type=Path, required=True)
    parser.add_argument('--spurious', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--physical-exports', type=Path,
                        help='Relocated parent directory containing the unchanged physical exports')
    parser.add_argument('--spurious-exports', type=Path,
                        help='Relocated parent directory containing the unchanged spurious exports')
    parser.add_argument('--points', type=int, default=10000)
    parser.add_argument('--tolerance', type=float, default=1e-7)
    args = parser.parse_args()
    try:
        compare(args)
    except (ValueError, KeyError, OSError) as error:
        print('Comparison rejected: '+str(error), file=sys.stderr)
        return 1
    return 0


if __name__ == '__main__':
    sys.exit(main())
