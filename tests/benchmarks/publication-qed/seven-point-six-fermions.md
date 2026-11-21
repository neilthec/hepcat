# Three identical fermion pairs and one photon

All momenta are incoming. Label the external particles
$(1_e,2_E,3_e,4_E,5_e,6_E,7^+)$ and set
$(a,b;x,y;c,d;k)=(1,2;3,4;5,6;7)$.
For an ordered assignment, the middle fermion line $(x,y)$ connects the two outer lines.
The external photon can attach twice on each outer line and three times on the middle line.
There are $3!3!=36$ assignments and 252 diagrams.

For either pole choice the result is

$$
-(\sqrt2e)^5\mathcal A_{135}\mathcal A_{246}
(1+P_{16}P_{25}P_{34})(1+P_{56})
\big[(1-P_{12})K_{\rm out}+(1+P_{12})K_{\rm mid}\big].
$$

Operators act right to left on momenta and spin labels together.
Each alternating sum is unnormalized. The two orientations of a bare outer current add.
The current emitting a single photon uses their difference. The opposite signs belong to
the individual currents inside the sum. Parity supplies the second photon helicity.

For $k^+$, put $J=p_c+p_d$ and introduce the two contractions with an open spinor,

$$
\begin{aligned}
H_{[]}(t)&=-[yd][t|J|c\rangle-[yt][dyc\rangle,\\
H_{\langle\rangle}(t)&=-\langle yd\rangle[c|J|t\rangle-\langle yt\rangle[dyc\rangle .
\end{aligned}
$$

With $f_{tu}=-[kutk]/(\tau_{tk}\tau_{uk})$ and $v_t=[kt]/\tau_{tk}$, define

$$
\begin{aligned}
B&=[bx]H_{\langle\rangle}(a)+\langle bx\rangle H_{[]}(a),&
B_a&=[kx]H_{\langle\rangle}(b)+\langle bx\rangle H_{[]}(k),\\
B_x&=[bk]H_{\langle\rangle}(a),&
B_o&=[ka]\langle bx\rangle H_{[]}(k).
\end{aligned}
$$

The two terms are

$$
K_{\rm out}=\frac{f_{ab} B+v_a B_a}{s_{cd}\tau_{cdy}s_{abk}},
\qquad
K_{\rm mid}=\frac{fB+v_xB_x+gB_o+T}{s_{cd}\tau_{cdy}s_{ab}}.
$$

Here $s_{ij\ldots}=(p_i+p_j+\cdots)^2$ and $\tau_{ij\ldots}=s_{ij\ldots}-m^2$.
AllPhysical uses

$$
\begin{aligned}
 f&=\tfrac12\left(f_{xy}+\frac{[k|p_yJ|k]}{\tau_{yk}\tau_{abx}}\right),
 & g&=\frac{1}{2\tau_{abx}},\\
 T&=\frac{\langle bx\rangle[ak][yd][kxc\rangle
             -[bx][kxa\rangle\langle yd\rangle[kc]}{2\tau_{xk}} .
\end{aligned}
$$

For AllSpurious let $Q=p_a+p_b+p_x$ and $w=2p_k\cdot Q=\tau_{cdy}-\tau_{abx}$. Then

$$
f=-\frac{[k|(p_a+p_b)p_x|k]}{\tau_{xk}w},\qquad g=-\frac{1}{w},\qquad T=0.
$$

The composite reference $Q|k]$ lets the middle-line insertion split into two contributions with one adjacent fermion propagator each. The only additional divisor is $w$ and its relabelings. The two adjacent propagators can coincide while both are finite. Summing the attachments then cancels $1/w$. The formulas contain only external spinors and momenta.

## Complexity and validation

| Pole choice | Elementary weight | Outer term | Middle term | Combined summand weight |
| --- | ---: | ---: | ---: | ---: |
| AllPhysical | 85,248 | 92 | 242 | 334 |
| AllSpurious | 85,248 | 92 | 162 | 254 |

The last column counts the expressions inside the signed sums. The weights include the coupling in each term. They measure symbolic complexity, not the cost of evaluating 288 signed label images. Successively restoring the two outer-current reversals improved the scores from 1,336 and 1,016 to 668 and 508, then to 334 and 254. The displayed two-term $H$ uses the external Dirac equation to combine the explicit mass with $p_d$ into $J$. The manuscript transcription before Dirac reduction has weights 382 and 294. The native terms after reduction have 904 and 769 leaves, respectively.

On construction, the C++ reducer groups complete attachment orders by their exchanged-vector poles and orbits under identical-particle permutations. For a path of three equal-mass fermion lines with minimal vector couplings, it joins the two orientations of a bare or radiating end current with the corresponding sign. On reloading an elementary expression, it checks all signed Laurent coefficients against the generated current up to one common scalar before applying those operations. A pole graph alone cannot fix relative signs.

We compared the signed complex amplitudes with the 252 independent Feynman diagrams at 100,000 common momenta and massive-spin assignments for each photon helicity (seed 706181). The signed replay uses the ordinary order of the 288 projected images, including for the spurious form. It reconstructs any point exceeding a relative error of $10^{-8}$ at 160-bit precision. The following are the raw maxima and the number of reconstructed points. Every reconstructed case passed.

| Photon | Representation | Paper raw maximum | Native raw maximum | Paper / native replays | Largest replay error |
| --- | --- | ---: | ---: | ---: | ---: |
| $+$ | Physical | $1.27\times10^{-11}$ | $1.61\times10^{-11}$ | 0 / 0 | |
| $-$ | Physical | $8.67\times10^{-12}$ | $6.90\times10^{-12}$ | 0 / 0 | |
| $+$ | Spurious | $3.13\times10^{-4}$ | $3.02\times10^{-4}$ | 41 / 42 | $3.97\times10^{-38}$ |
| $-$ | Spurious | $3.67\times10^{-4}$ | $3.30\times10^{-4}$ | 40 / 42 | $4.35\times10^{-38}$ |

On a conserved on-shell trajectory through $w=0$, the eight images that reconstruct one middle-line routing provide a direct check of the apparent pole. At $|w|=5.01\times10^{-14}$ the largest term has magnitude $2.07\times10^{10}$, while the signed sum has magnitude $1.03\times10^{-3}$. Approaching from either side at extended precision gives relative agreement with the physical middle-line current within $2.33\times10^{-32}$. At the cut, the magnitude of the cleared residue sum divided by the largest individual residue is below $7.7\times10^{-58}$ for four spin states. The [pole test](seven-point-six-fermions-pole.cpp) also probes much smaller $|w|$, comparing direct division with summing the residues first.

The crossed external reaction has two incoming fermions and four outgoing fermions plus a photon. The squared matrix element includes the initial spin average $1/4$ and the identical-final-particle factor $1/(2!\,2!)$, giving $1/16$ in total. The fixed-helicity MadGraph comparisons sum over all 64 massive-spin assignments per photon helicity. CalcHEP supplies the full polarization sum.

The physical and common-denominator spurious exports passed all external comparisons at native precision. CalcHEP used the original 100,000 independent matrix elements and identical saved momenta at seed 704761. The MadGraph rows use 100,000 common momenta at seed 706181, separately for each photon helicity and for its exhaustive polarization sum. Threshold $10^{-7}$, widths zero.

| Numerical form | CalcHEP, spin sum | MadGraph, photon $-$ | MadGraph, photon $+$ | MadGraph, spin sum |
| --- | ---: | ---: | ---: | ---: |
| AllPhysical | $7.626\times10^{-9}$ | $1.952\times10^{-10}$ | $1.952\times10^{-10}$ | $1.952\times10^{-10}$ |
| AllSpurious, common denominators | $7.626\times10^{-9}$ | $3.055\times10^{-10}$ | $3.055\times10^{-10}$ | $3.055\times10^{-10}$ |

For the raw separate divisions, the 100,000-point spin-summed comparison with CalcHEP had five outliers at $10^{-7}$, with maximum relative error $7.45\times10^{-6}$. MadGraph had six common outlying momenta across both helicities and the polarization sum, maximum $2.13\times10^{-6}$. We evaluated both signed formulas on the original binary64 momenta at 160-bit arithmetic and on a nearby exactly conserved, on-shell chart. The largest component shift was below $4.66\times10^{-16}$ GeV. On the restored chart the raw spurious form agreed with the physical form within $3.69\times10^{-42}$. The corresponding rates agreed with the original CalcHEP values within $2.25\times10^{-13}$ and with the 113-bit MadGraph references within $9.08\times10^{-13}$.

The [eleven original momenta and raw rates](seven-point-six-fermions-outliers.csv) allow both stages to be reproduced. For example, MadGraph point 32123 has a raw polarization-sum error of $2.13\times10^{-6}$. Restoring the kinematics shifts any component by at most $2.17\times10^{-16}$ GeV. The two helicity rates of the restored physical amplitude are both $0.02993120631273257$. On the original input the 160-bit direct spurious evaluation still differs by $1.10\times10^{-5}$. Collecting the common denominators gives a polarization-sum error of $2.49\times10^{-11}$ even in native arithmetic on the original momenta.

## Reproduce

From the repository root, build the QED notebook driver. Restrict the worker count to one to reproduce the calculation on a laptop.

```sh
build=tests/results/native-reduce-build
bench=tests/benchmarks/publication-qed
cmake -S csource -B "$build"
cmake --build "$build" --target hepcat-qed -j1
"$build/hepcat-qed" --family eeeeeeA --construct-only --skip-formulas \
    --workers 1 --points 100000 --seed 706181
```

Set `run_dir` to the output directory printed by the constructor. The following checks the independently transcribed paper expression and the reducer's two oriented terms against the signed Feynman amplitude. Add your GMP include and library paths to the compile command if needed.

```sh
c++ -std=c++17 -O2 -Icsource "$bench/seven-point-four-fermions-replay.cpp" \
    "$build/libhepcat_reduce.a" -lgmpxx -lgmp -o "$build/replay-fermion-currents"
for h in + -; do
    paper="$run_dir/paper/$h"
    mkdir -p "$paper"
    for pref in physical spurious; do
        input="$run_dir/$pref/eeeeeeA/$h.expr"
        for part in leaf center; do
            python3 "$bench/seven-point-six-fermions.py" "$pref" "$input" \
                --piece "$part" --helicity="$h" > "$paper/$pref-$part.expr"
        done
    done
    "$build/replay-fermion-currents" "$run_dir" "$paper" 100000 706181 "$h"
done
```

To repeat the cut and the two-sided approach on the same spin states, run

```sh
c++ -std=c++17 -O2 -Icsource "$bench/seven-point-six-fermions-pole.cpp" \
    "$build/libhepcat_reduce.a" -lgmpxx -lgmp -o "$build/replay-radiation-pole"
"$build/replay-radiation-pole" "$run_dir/paper/+/physical-center.expr" \
    "$run_dir/paper/+/spurious-center.expr"
```

For a numerical export of the full paper formula, replace `--piece "$part"` with `--expanded`. It relabels every spinor, insertion and propagator. In the spurious representation, reversing the central line sends $Q$ to $-Q-k$ and hence $w$ to $-w$. The expansion sums the residues over each of the nine distinct $w$ denominators before it divides. The option `--raw-expanded` exposes the original separate divisions for studying cancellation. Both choices implement the same printed expression on conserved, on-shell momenta.

To run the compact formulas themselves against an external matrix element, expand both helicities into the same directory. The example uses MadGraph with the QED-only interactions of its massive-electron model. To run the spin-summed comparison with CalcHEP, substitute `--calchep "$calchep_dir"` and choose a separate output directory.

```sh
c++ -std=c++17 -O2 -Icsource "$bench/seven-point-six-fermions-external.cpp" \
    "$build/libhepcat_reduce.a" -lgmpxx -lgmp -o "$build/replay-external"
for pref in physical spurious; do
    dir="$run_dir/paper-expanded/$pref"
    mkdir -p "$dir"
    for h in + -; do
        python3 "$bench/seven-point-six-fermions.py" "$pref" \
            "$run_dir/$pref/eeeeeeA/$h.expr" --expanded --helicity="$h" > "$dir/$h.expr"
    done
    "$build/replay-external" "$dir" "$dir/mg-project" "$spinas_dir" \
        --madgraph "$madgraph_dir" 100000 706181
done
```

The physical-momenta replay takes directories containing `+.expr` and `-.expr` for each form. Generate the spurious directory once with `--raw-expanded` and once with `--expanded` to compare their evaluation orders.

```sh
c++ -std=c++17 -O2 -Icsource "$bench/seven-point-six-fermions-outlier.cpp" \
    "$build/libhepcat_reduce.a" -lgmpxx -lgmp -o "$build/replay-radiation-outliers"
"$build/replay-radiation-outliers" "$physical_dir" "$spurious_dir" \
    "$bench/seven-point-six-fermions-outliers.csv"
```
