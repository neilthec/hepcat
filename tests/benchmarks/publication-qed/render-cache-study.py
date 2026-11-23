"""Render the matched SPINAS replay, using archived CalcHEP rates on the same points."""
import argparse
import csv
import importlib.util
import json
from pathlib import Path
from statistics import median

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.ticker import NullFormatter

HERE = Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location("rate_plots", HERE/"render-rate-study.py")
rate_plots = importlib.util.module_from_spec(spec)
spec.loader.exec_module(rate_plots)


def fmt(value):
    return f'{value:,.3g}' if value < 1000 else f'{value:,.0f}'


def memory_plot(records, path):
    fig, axes = plt.subplots(2, 2, figsize=(7.1, 4.6), sharey=True, layout="constrained")
    for pairs, ax in enumerate(axes.flat, 1):
        for pole, color in [('physical', '#0072b2'), ('spurious', '#b46100')]:
            subset = sorted((r for r in records if r['family'].count('e') == 2*pairs and r['pole'] == pole),
                            key=lambda r: len(r['family']))
            for key, style in [('before', ':'), ('after', '-')]:
                ax.plot([len(r['family']) for r in subset], [r[key]['peak_rss']/2**20 for r in subset],
                        style, color=color, marker=('o' if pole == 'physical' else 's') if key == 'after' else None, markersize=3.5,
                        label=f'{pole.capitalize()} {key}')
        subset = [r for r in records if r['family'].count('e') == 2*pairs and r['pole'] == 'physical']
        subset.sort(key=lambda r: len(r['family']))
        ax.plot([len(r['family']) for r in subset], [r['mg']['peak_rss']/2**20 for r in subset],
                '--', color='#7c4672', marker='D', markersize=3, label='MadGraph')
        ax.set_title(f'{pairs} charged pair' + ('s' if pairs > 1 else ''), loc='left', fontsize=10)
        ax.set_yscale('log')
        ax.set_yticks([2, 5, 10, 20, 50], labels=['2','5','10','20','50'])
        ax.yaxis.set_minor_formatter(NullFormatter())
        ax.set_xticks([len(r['family']) for r in subset])
        ax.grid(axis='y', alpha=.2)
    handles, labels = axes.flat[0].get_legend_handles_labels()
    fig.legend(handles, labels, loc='outside upper center', ncol=3, frameon=False, fontsize=8)
    fig.supxlabel('Number of external particles', fontsize=10)
    fig.supylabel('Peak resident size (MiB)', fontsize=10)
    fig.savefig(path, metadata={"CreationDate": None, "ModDate": None})
    plt.close(fig)


def relative_plot(records, path):
    fig, axes = plt.subplots(2, 2, figsize=(7.1, 4.2), sharey=True, layout="constrained")
    for pairs, ax in enumerate(axes.flat, 1):
        ax.axhline(1, color='0.45', linewidth=.9)
        for pole, color, marker in [('physical','#0072b2','o'),('spurious','#b46100','s')]:
            subset = sorted((r for r in records if r['family'].count('e') == 2*pairs and r['pole'] == pole),
                            key=lambda r: len(r['family']))
            legs = [len(r['family']) for r in subset]
            speed = [median(r['mg']['times'])/median(r['after']['times']) for r in subset]
            ax.plot(legs, speed, color=color, marker=marker, markerfacecolor='white',
                    label='AllPhysical' if pole == 'physical' else 'AllSpurious')
        ax.set_title(f'{pairs} charged pair' + ('s' if pairs > 1 else ''), loc='left', fontsize=10)
        ax.set_yscale('log')
        ax.set_xticks(legs)
        ax.grid(axis='y', alpha=.2)
    handles, labels = axes.flat[0].get_legend_handles_labels()
    fig.legend(handles, labels, loc='outside upper center', ncol=2, frameon=False, fontsize=9)
    fig.supxlabel('Number of external particles', fontsize=10)
    fig.supylabel('MadGraph time / SPINAS time', fontsize=10)
    fig.savefig(path, metadata={"CreationDate": None, "ModDate": None})
    plt.close(fig)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('data', type=Path)
    parser.add_argument('paper', type=Path)
    args = parser.parse_args()
    original = json.loads((HERE/'rate-study.json').read_text())
    if args.data.is_dir():
        paths = [args.data / (r['family']+'-'+pole+'.json') for r in original for pole in ('physical','spurious')]
        records = [json.loads(path.read_text()) for path in paths]
    else:
        records = json.loads(args.data.read_text())
    lookup = {(r['family'],r['pole']): r for r in records}
    output = args.paper
    figure_dir = output/'figures'
    figure_dir.mkdir(parents=True, exist_ok=True)
    timing, sizes, memory, sheet, catalog = [], [], [], [], []
    for row in original:
        f = row['family']
        pair = [lookup[f,p] for p in ('physical', 'spurious')]
        group = [row['legs'],row['pairs'],row['photons']]
        mg = median([t for r in pair for t in r['mg']['times']])*1e6
        rates = [median(r[k]['times'])*1e6 for k in ('before', 'after') for r in pair]
        ch = row['rate'].get('ch')
        timing.append(group + [fmt(x) for x in rates] +
                      ([fmt(median(ch['seconds_per_point'])*1e6)] if ch else [r'\ensuremath{\cdot}']) + [fmt(mg)])
        rss = [r[k]['peak_rss']/2**20 for k in ('before','after') for r in pair]
        memory.append(group + [f'{v:.2f}' for v in rss] + [f"{pair[0]['mg']['peak_rss']/2**20:.2f}"])
        sz = [r[k]/1.e6 for k in ('source_bytes','table_bytes') for r in pair]
        sizes.append(group + [fmt(x) for x in sz] +
                     [fmt(row['source_bytes'][k]/1.e6) if k in row['source_bytes'] else r'\ensuremath{\cdot}' for k in ('ch','mg')])
        for r in pair:
            pole = r['pole']
            row['rate']['table_'+pole] = dict(seconds_per_point=r['after']['times'])
            row['source_bytes'][pole] = r['source_bytes']
            t0, t1 = (median(r[k]['times']) for k in ('before','after'))
            sheet.append([f,pole,t0,t1,median(r['mg']['times']),t0/t1,median(r['mg']['times'])/t1,
                          r['before']['peak_rss'],r['after']['peak_rss'],r['mg']['peak_rss'],
                          r['before_slots'],max(x['slots'] for x in r['stats']),
                          r['source_bytes'],r['table_bytes'],r['mg_error'],r.get('archived_ch_error','')])
            catalog.extend([group + [pole, r['source_bytes'], None, None],
                            group + ['table_'+pole, r['table_bytes'], t1*1e6, r['mg_error']],
                            group + ['previous_table_'+pole, row['sources']['table_'+pole]['bytes'], t0*1e6, r['before']['mg_error']]])
        catalog.append(group + ['mg', row['source_bytes']['mg'], mg, 0])
        if ch:
            catalog.append(group + ['ch',row['source_bytes']['ch'],median(ch['seconds_per_point'])*1e6,ch['max_relative_to_mg']])
        row['rate']['mg']['seconds_per_point'] = [mg*1e-6]
    def table(rows, headings, groups, caption, name, label):
        header = r'\toprule & & & ' + ' & '.join(r'\multicolumn{2}{c}{'+s+'}' for s in groups)
        header += ' &'*(len(headings)-7) + r' \\'+'\n'+r'\cmidrule(lr){4-5}\cmidrule(lr){6-7}'
        tex = rate_plots.tabular(rows,headings,'@{}'+'r'*len(headings)+'@{}',caption,label)
        (output/name).write_text(tex.replace(r'\toprule',header))
    headings = ['$n$','Pairs','$N_\\gamma$','Phys.','Spur.','Phys.','Spur.']
    table(timing,headings+['CalcHEP','MG'],['Previous (sequential)','Updated (compact)'],
          r'Microseconds per complete rate, including momentum setup. Both SPINAS builds use operand tables. The updated build in this survey uses the optional compact layout. The SPINAS entries are medians of three sweeps, and MG combines the two matched three-sweep runs. CalcHEP entries come from the original survey at the same momenta.',
          'rate-table.tex','tab:rate-survey')
    table(sizes,headings+['CalcHEP','MG'],['SPINAS direct','SPINAS tables'],
          'Process-specific source in decimal MB. Operand-table sizes include their binary operand stream.',
          'size-table.tex','tab:source-bytes')
    table(memory,headings+['MG'],['Previous (sequential)','Updated (compact)'],
          'Peak resident size in MiB for the driver and loaded evaluator in a fresh process. The MG column uses the run paired with the physical form.',
          'memory-table.tex','tab:rate-memory')
    for name, size, normalized in [('catalog-rates',False,False),('catalog-growth',False,True),('catalog-source',True,False)]:
        rate_plots.plots(original,figure_dir/(name+'.pdf'),size,normalized)
    memory_plot(records,figure_dir/'catalog-memory.pdf')
    relative_plot(records,figure_dir/'catalog-relative.pdf')
    with (output/'cache-measurements.csv').open('w',newline='') as file:
        writer=csv.writer(file, lineterminator='\n')
        writer.writerow('family pole before_s after_s mg_s speedup mg_over_after before_rss_bytes after_rss_bytes mg_rss_bytes before_slots after_slots source_bytes table_bytes max_relative_mg max_relative_archived_ch'.split())
        writer.writerows(sheet)
    with (output/'catalog-measurements.csv').open('w', newline='') as file:
        writer = csv.writer(file, lineterminator='\n')
        writer.writerow(['legs','charged_pairs','photons','engine','source_bytes','median_us_per_point','max_relative_to_madgraph'])
        writer.writerows(catalog)
    wins = sum(row[6] > 1 for row in sheet if row[1] == 'physical')
    (output/'rate-summary.tex').write_text(
        f'The updated physical-pole evaluator is faster than the matched MadGraph library for {wins} '
        f'of the {len(original)} complete families in this sample. '
        'Table~\\ref{tab:rate-survey} gives the times for each family and pole form.\n')
    (output/'cache-study.json').write_text(json.dumps(records, indent=1)+'\n')


if __name__ == '__main__':
    main()
