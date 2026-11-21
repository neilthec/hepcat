# Two identical massive fermion pairs and photons of helicities ++−

Take all momenta incoming, with the charged lines $(a,b;c,d)=(1_e,2_E;4_e,3_E)$, two positive photons $(i,j)=(5,6)$ or $(6,5)$, and a negative photon $k=7$. With $P_{uv}$ acting on momenta and spin labels together, both pole forms are

$$
-(\sqrt 2 e)^5 (1-P_{14})(1-P_{23})(1-P_{12}P_{43})
\sum_{(i,j)}
\left[
\frac{\mathcal L_0(T_{ijk})}{s_{34}}+
\frac{\mathcal L_k(T_{ij})}{s_{347}}+
\frac{\mathcal L_j(T_{ik})}{s_{34j}}
\right].
$$

Each $T$ is the oriented Weyl current on the first fermion line. The subscript on $\mathcal L$ identifies the photon on the second line, with $0$ denoting a bare line. The signed charge reversal restores the opposite Weyl orientation. The three terms partition the 240 diagrams into sets of 96, 48 and 96. Before replacing a symmetric current by an ordered representative, the photon sum has weights $1/2$, $1/2$ and $1$, respectively. The ordered formula already includes them.

The two forms differ in one scalar multiplying the unshifted spinor in the two-positive-photon current. Write $f_h=-[hbah]/(\tau_{ah}\tau_{bh})$ for $h=i,j$ and $C_t=-m^2[ij]^2/(\tau_{ti}\tau_{tj}\tau_{tij})$. Then

$$
\begin{aligned}
F_{\rm phys}(i,j)&=\tfrac12(f_i f_j+C_a+C_b),\\
F_{\rm spur}(i,j)&=\frac{1}{\langle ij\rangle}
\left[\frac{[iabj]}{\tau_{ai}\tau_{bj}}-
m^2[ij]\sum_{t=a,b}\frac{1}{\tau_{ti}\tau_{tij}}\right].
\end{aligned}
$$

The determinant of the two single-photon dipoles gives $F_{\rm spur}(i,j)+F_{\rm spur}(j,i)=2F_{\rm phys}(i,j)$. All factors multiplying $F$ are symmetric in these two photons. Their exchange cancels the sole spurious bracket. The [complete formula transcription](seven-point-four-fermions-mixed.py) spells out all endpoint factors and contractions. Its only inputs from a saved elementary calculation are the model identifiers for the mass and charge.

## Validation and complexity

The [signed replay](seven-point-four-fermions-replay.cpp) evaluates all 16 images of the independently transcribed sum and of the result of calling C++ `reduce()` on the saved elementary expression. Its reference is the signed sum of 240 diagrams generated from the Feynman model. The relative difference is normalized to the larger amplitude magnitude. At seed 704739 all 100,000 common phase-space and massive-spin samples passed at native precision (tolerance $10^{-8}$).

| Pole choice | Full elementary weight | Native summand weight | Native summand leaves | Maximum relative difference, printed vs. Feynman | Maximum relative difference, native vs. Feynman |
| --- | ---: | ---: | ---: | ---: | ---: |
| AllPhysical | 41,280 | 1,188 | 3,691 | $6.23192\times10^{-11}$ | $6.23190\times10^{-11}$ |
| AllSpurious | 27,648 | 1,124 | 3,539 | $6.23199\times10^{-11}$ | $6.23205\times10^{-11}$ |

We also summed the possible insertions of the negative photon in a common lightcone gauge. That representation reached the same spurious weight of 1,124 with more leaves. Using the external Dirac equation to collect endpoint momenta into one slash on the equal-helicity pair raised the reduced weights to 1,228 and 1,204. Both alternatives were checked against the signed diagrams on ten samples. These comparisons establish the smallest representations found in this search.

The constructor passed all 44 physical residues (1,056 chart samples), fixed-angle high-energy growth and reference independence. The spurious form also passed its cancellation test. Separate fixed-helicity SPINAS exports for $++-$ passed MadGraph at 100,000 momenta per pole choice (seed 704739, all fermion spins summed). The maximum relative differences were $2.6390\times10^{-10}$ and $9.2262\times10^{-9}$, respectively. The [full-family CalcHEP comparisons](../qed-seven-point-external.tsv) include all eight photon helicities and both electron spins at each leg. At seed 704721 the corresponding maxima were $6.29\times10^{-8}$ and $6.30\times10^{-8}$ over 100,000 momenta per pole choice.

The reducer groups photon allocations by helicity counts on the two lines and weights each representative by its multiplicity. For an eight-point check on different helicity partitions, `+++-` and `++--` gave four and five distinct allocations up to exchange, respectively. For each pole choice the signed orbit sum agreed with the elementary amplitude from 1,680 diagrams at ten momenta (seeds 704750 through 704759, largest relative difference $3.48\times10^{-14}$). This checks the occupation weights and the second Bose group when both helicities occur twice.

## Reproduce

Build the notebook driver from the checkout, and supply the locations of the local SPINAS and MadGraph installations to include the polarized external test.

```sh
build=tests/results/native-reduce-build
bench=tests/benchmarks/publication-qed
cmake -S csource -B "$build"
cmake --build "$build" --target hepcat-qed -j1
"$build/hepcat-qed" --family eeeeAAA --helicity '++-' --skip-formulas \
    --workers 1 --points 100000 --seed 704739 \
    --spinas "$spinas_dir" --madgraph "$madgraph_dir"
```

Set `run_dir` to the output directory printed by the driver. Add the GMP include and library directories to the compiler command if needed.

```sh
c++ -std=c++17 -O2 -Icsource "$bench/seven-point-four-fermions-replay.cpp" \
    "$build/libhepcat_reduce.a" -lgmpxx -lgmp -o "$build/replay-four-fermions"
mkdir -p "$run_dir/paper"
for pref in physical spurious; do
    python3 "$bench/seven-point-four-fermions-mixed.py" "$pref" \
        "$run_dir/$pref/eeeeAAA/++-.expr" > "$run_dir/paper/$pref.expr"
done
"$build/replay-four-fermions" "$run_dir" "$run_dir/paper" 100000 704739 '++-'
```
