#!/usr/bin/env python3
"""Redraw the HEPCAT construction workflow figure from the paper.

This illustrates the calculation stages; it does not run an amplitude
calculation. Drawing requires the optional Matplotlib plotting dependency.
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
from pathlib import Path


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output-directory', type=Path,
                        default=Path(__file__).resolve().parents[2] / 'results/reproduction/figures')
    out = parser.parse_args().output_directory
    import matplotlib
    matplotlib.use('Agg')
    import matplotlib.pyplot as plt
    from matplotlib.patches import FancyBboxPatch, FancyArrowPatch
    out.mkdir(parents=True, exist_ok=True)
    fig,ax=plt.subplots(figsize=(7.2,5.6))
    ax.set_xlim(0,10); ax.set_ylim(0,8); ax.axis('off')
    blue='#174e78'; pale='#edf4f8'; green='#356653'
    def box(x,y,w,h,title,body='',color=pale):
        ax.add_patch(FancyBboxPatch((x,y),w,h,boxstyle='round,pad=0.08,rounding_size=0.12',facecolor=color,edgecolor=blue,linewidth=1.05))
        ax.text(x+w/2,y+h-.23,title,ha='center',va='top',fontsize=10.2,weight='bold',color=blue)
        if body: ax.text(x+w/2,y+.32,body,ha='center',va='center',fontsize=8.9,linespacing=1.35,color='#222222')
    def arrow(a,b,style='-',color=blue):
        ax.add_patch(FancyArrowPatch(a,b,arrowstyle='-|>',mutation_scale=11,color=color,linewidth=1.1,linestyle=style,connectionstyle='arc3,rad=0'))
    box(.35,6.83,4.15,.95,'Constructive diagrams','Numerators, propagators, local shells')
    box(5.2,6.83,4.45,.95,'Complete lower-point amplitudes','Original factorization targets')
    box(.35,5.22,4.15,1.02,'Prepare and retain','Simple seed + discarded remainders')
    box(5.2,5.22,4.45,1.02,'Prepare source continuations','Original targets remain unchanged')
    box(2.12,3.55,5.76,1.03,'Simultaneous residue completion','Fit all cuts together; validate on new cut points')
    box(2.12,1.86,5.76,1.03,'Constrain the remaining family','High energy, spurious poles, reference independence')
    box(.35,.18,4.15,1.03,'Check the fixed amplitude','Original cuts + independent Feynman result')
    box(5.2,.18,4.45,1.03,'Export and independently compare','SPINAS versus CalcHEP and MadGraph')
    arrow((2.42,6.8),(2.42,6.27)); arrow((7.42,6.8),(7.42,6.27))
    arrow((2.42,5.17),(3.72,4.61)); arrow((7.42,5.17),(6.28,4.61))
    arrow((5,3.51),(5,2.93)); arrow((3.7,1.81),(2.5,1.25))
    arrow((4.55,.69),(5.08,.69))
    fig.subplots_adjust(left=.01,right=.99,top=.99,bottom=.01)
    fig.savefig(out/'workflow.pdf',bbox_inches='tight')
    fig.savefig(out/'workflow.png',dpi=170,bbox_inches='tight')
    plt.close(fig)
    print(out / 'workflow.pdf')
    print(out / 'workflow.png')


if __name__ == '__main__':
    main()
