# Two identical fermion pairs and three equal-helicity photons

Use the all-incoming ordering $1_e,2_E,3_E,4_e,5^+,6^+,7^+$. Choose the pairing $(a,b;c,d)=(1,2;4,3)$ and sum over the six orders $(i,j,k)$ of $5,6,7$. Let $P_{uv}$ exchange all occurrences of two external labels and $C=P_{12}P_{43}$ reverse both charged lines. The two pole forms share the expression

$$
-(\sqrt2 e)^5(1-P_{14})(1-P_{23})(1-C)\sum_{i,j,k}
\left(\frac{[X_3 Y_0]}{s_{34}}+\frac{[X_2 Y_1]}{s_{34k}}\right).
$$

The first contraction puts all three photons on the first charged line. The second puts two on the first line and one on the second. For one matching the two classes contain $2(4!)=48$ and $2\binom31(3!)(2!)=72$ diagrams, respectively. The Fermi exchange signs distinguish the two matchings. The joint reversal restores the other Weyl orientation with a minus sign from the five vector vertices. There are 48 signed images of this summand. Every permutation has unit weight.

## Evaluating a summand

Write $f_{tu}(h)=-[huth]/(\tau_{th}\tau_{uh})$, $v_t(h)=[h\mathbf t]/\tau_{th}$, and $f_h=f_{ab}(h)$. For the marked photon pair let

$$
c_t=\frac{-m^2[ij]^2}{\tau_{ti}\tau_{tj}\tau_{tij}},\qquad
w_t=\frac{[ki][kti\rangle}{\tau_{tk}\tau_{tijk}},\qquad
Q_a=p_a+p_i+p_j+p_k .
$$

For physical poles put $F=(f_i f_j+c_a+c_b)/2$. For spurious poles use

$$
F=\frac{1}{\langle ij\rangle}\left[
\frac{[iabj]}{\tau_{ai}\tau_{bj}}-
m^2[ij]\left(\frac1{\tau_{ai}\tau_{aij}}+\frac1{\tau_{bi}\tau_{bij}}\right)\right].
$$

The two ordered spurious coefficients for $(i,j)$ and $(j,i)$ add to twice the physical coefficient. This follows by taking the determinant of the two single-photon dipoles and using $s_{ij}=\tau_{tij}-\tau_{ti}-\tau_{tj}=-\langle ij\rangle[ij]$. The photon sum therefore cancels each spurious angle pole. The two bras and kets are

$$
\begin{aligned}
[X_2| &= F[\mathbf a|+f_i v_a(j)[j|-\frac{c_a}{m}\langle i\mathbf a\rangle[i|,\\
[X_3| &= \left[\tfrac13 f_k(F+c_a+c_b)-c_b w_b\right][\mathbf a|
 + F v_a(k)[k|+\frac{c_a}{m}\left(w_a\langle\mathbf a|Q_a-f_k\langle i\mathbf a\rangle[i|\right),\\
|Y_0] &= \langle\mathbf b\mathbf d\rangle|\mathbf c]+\langle\mathbf b\mathbf c\rangle|\mathbf d],\\
|Y_1] &= f_{cd}(k)|Y_0]+
 (\langle\mathbf b\mathbf d\rangle v_c(k)-\langle\mathbf b\mathbf c\rangle v_d(k))|k].
\end{aligned}
$$

The [formula transcription](seven-point-four-fermions.py) implements these equations. It reads only the mass and charge identifiers from the saved expression. The [signed replay](seven-point-four-fermions-replay.cpp) computes the permutation signs independently of the native renderer and compares each reconstructed amplitude directly with the sum of 240 Feynman diagrams.

## Scores and validation

| Pole choice | Full elementary weight | Reduced summand weight | Reduced summand leaves | Largest relative difference, paper vs. Feynman | Largest relative difference, reducer vs. Feynman |
| --- | ---: | ---: | ---: | ---: | ---: |
| AllPhysical | 33,792 | 388 | 1,408 | $6.04857\times10^{-10}$ | $6.05174\times10^{-10}$ |
| AllSpurious | 25,792 | 360 | 1,334 | $6.04817\times10^{-10}$ | $6.04115\times10^{-10}$ |

The scores measure one summand before relabeling. The replay tested complex signed amplitudes at the same 100,000 phase-space and massive-spin samples for each entry (seed 704733). All passed at relative tolerance $10^{-8}$ at native precision. The constructor separately passed all 44 physical channels (1,056 residue samples), fixed-angle high-energy growth, reference independence and the applicable spurious-pole checks.

A fresh polarized SPINAS export for each pole choice also passed MadGraph at 100,000 common momenta, with all four fermion spins summed at fixed photon helicities $+++$. At seed 704733 the largest relative differences in the squared amplitude were $5.62\times10^{-10}$ for AllPhysical and $1.24\times10^{-8}$ for AllSpurious. To repeat this test, omit `--construct-only` from the command below and pass the local SPINAS and MadGraph directories with `--spinas` and `--madgraph`.

The [external validation records](../qed-seven-point-external.tsv) use independent points and the elementary SPINAS exports for the full family. At seed 704721 the CalcHEP spin sums passed at 100,000 points for each pole form (largest relative differences $6.29\times10^{-8}$ and $6.30\times10^{-8}$). MadGraph also passed all eight photon helicities and their sum. For the $+++$ row alone, the largest relative differences were $2.09\times10^{-10}$ and $3.84\times10^{-10}$.

## Reproduce the signed comparison

From the HEPCAT checkout, build the QED notebook driver and run the two pole choices sequentially.

```sh
build=tests/results/native-reduce-build
cmake -S csource -B "$build"
cmake --build "$build" --target hepcat-qed -j1
"$build/hepcat-qed" --family eeeeAAA --helicity '+++' --construct-only \
    --skip-formulas --workers 1 --points 100000 --seed 704733
```

Set `run_dir` to the directory printed by the driver. Compile the replay against the same library. Supply the GMP include and library directories with `-I` and `-L` if your compiler requires them.

```sh
bench=tests/benchmarks/publication-qed
c++ -std=c++17 -O2 -Icsource "$bench/seven-point-four-fermions-replay.cpp" \
    "$build/libhepcat_reduce.a" -lgmpxx -lgmp -o "$build/replay-four-fermions"
mkdir -p "$run_dir/paper"
for pref in physical spurious; do
    expr="$run_dir/$pref/eeeeAAA/+++.expr"
    python3 "$bench/seven-point-four-fermions.py" "$pref" "$expr" > "$run_dir/paper/$pref.expr"
done
"$build/replay-four-fermions" "$run_dir" "$run_dir/paper" 100000 704733
```
