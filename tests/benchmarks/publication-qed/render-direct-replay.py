"""Summarize the 10,000-point direct C++ replays and render their paper table."""
import argparse
import csv
import hashlib
import json
from pathlib import Path
from statistics import median
import struct


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('records', type=Path)
    parser.add_argument('paper', type=Path)
    args = parser.parse_args()
    if args.records.is_dir():
        records = []
        for path in args.records.glob('*.json'):
            r = json.loads(path.read_text())
            if r['build'] != 'direct':
                raise ValueError(f'Expected a direct export in {path}')
            for k in ('before','after','mg'):
                sample = r[k].pop('weights')
                r[k]['points'] = len(sample)
                r[k]['weights_sha256'] = hashlib.sha256(b''.join(struct.pack('<d', x) for x in sample)).hexdigest()
            records.append(r)
        records.sort(key=lambda r: (len(r['family']),r['family'].count('e'),r['pole']))
    else:
        records = json.loads(args.records.read_text())
    families = sorted({r['family'] for r in records}, key=lambda f: (len(f),f.count('e')))
    lookup = {(r['family'],r['pole']): r for r in records}
    samples = {r[k]['points'] for r in records for k in ('before','after','mg')}
    if samples != {10000} or len(families) != 6 or len(lookup) != 12:
        raise ValueError('Expected six complete direct families and 10,000 identical points per comparison')
    rows = [r'\begin{table}[!htbp]\centering\small',r'\begin{tabular}{rrrrrrr}\toprule',
            r' & & \multicolumn{2}{c}{Previous SPINAS} & \multicolumn{2}{c}{Updated SPINAS} & \\',
            r'\cmidrule(lr){3-4}\cmidrule(lr){5-6}',
            r'Pairs & $N_\gamma$ & Phys. & Spur. & Phys. & Spur. & MG \\ \midrule']
    for f in families:
        pair = [lookup[f, pole] for pole in ('physical','spurious')]
        values = [median(r[k]['times'])*1e6 for k in ('before','after') for r in pair]
        values.append(median([t for r in pair for t in r['mg']['times']])*1e6)
        rows.append(' & '.join([str(f.count('e')//2), str(f.count('A')), *[f'{v:.3g}' for v in values]])+r' \\')
    rows.extend([r'\bottomrule\end{tabular}',
                 r'\caption{Microseconds per complete rate from directly compiled native HEPCAT exports at 10,000 momentum configurations. The previous and updated sources use the same compiler flags. MG combines the two matched runs.}\label{tab:native-direct-replay}',
                 r'\end{table}'])
    (args.paper/'direct-replay.tex').write_text('\n'.join(rows)+'\n')
    (args.paper/'direct-replay.json').write_text(json.dumps(records,indent=1)+'\n')
    with (args.paper/'direct-replay.csv').open('w',newline='') as f:
        writer = csv.writer(f,lineterminator='\n')
        writer.writerow('family pole points before_us after_us mg_us max_relative_to_mg'.split())
        for r in records:
            writer.writerow([r['family'],r['pole'],r['after']['points'],
                             *[median(r[k]['times'])*1e6 for k in ('before','after','mg')],r['mg_error']])


if __name__ == '__main__':
    main()
