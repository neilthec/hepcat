# Three positive photons and two negative photons

Take the incoming ordering $1_e,2_E,3^+,4^+,5^+,6^-,7^-$. Choose the middle photon on the fermion line and add the two emission orders at each end. Write $D(ij|k|lm)$ for the result with photons $i,j$ on the electron side of $k$ and $l,m$ on the positron side. There are $5\binom{4}{2}=30$ distinct assignments. Each is the sum of two products, one from each chiral block of the middle photon vertex.

In both formulas below the normalization outside the sum is $(\sqrt{2}e)^5$. All permutations indicated are summed with unit weight.

* **AllPhysical.** Use leg 1 for all five photon references. Sum the following expression over all permutations of $3,4,5$ and both orders of $6,7$.

  $$
  \frac{D(34|5|67)+D(67|3|45)}4+D(36|4|57)
  +\frac{D(34|6|57)+D(36|7|45)}2 .
  $$

* **AllSpurious.** Use photon references $3\to4\to5\to3$ and $6\leftrightarrow7$. Sum the following expression over the three cyclic rotations of $3,4,5$ and both orders of $6,7$.

  $$
  \frac{D(34|5|67)+D(67|3|45)}2+D(36|4|57)+D(36|5|47)+D(34|6|57)+D(36|7|45).
  $$

The weights divide out repeated assignments, such as swapping the photons in a same-helicity endpoint pair. In the physical gauge the single-photon orbital emission from the electron endpoint vanishes. In the spurious gauge every reference is another external photon. The three angle brackets and one square bracket in the reference denominators cancel as singularities in the full sum.

The [independent transcription](seven-point-two-negative.py) specifies every bracket in $D$. Its function `current` evaluates the left or right open two-photon current on one spinor of the middle polarization. Its function `join` multiplies complementary chiral components. The script uses the saved expression only to read the model-local electron mass and charge identifiers.

## Native simplification and validation

| Pole choice | Full elementary spinor weight | Automatic summand spinor weight | Automatic summand leaves |
| --- | ---: | ---: | ---: |
| AllPhysical | 7,683 | 858 | 2,887 |
| AllSpurious | 12,672 | 1,864 | 4,075 |

The summand scores charge for the spinor algebra inside one permutation sum. They omit the cost of evaluating its permutations. The elementary expressions remain available for numerical export. To use the same independent permutation groups for both outputs, the native spurious representation averages the complete cyclic-gauge sum with its reverse. Each orientation gives the same signed amplitude. The paper uses one orientation.

Each elementary expression passed 30 physical-channel checks (720 chart samples), the reference, high-energy and spurious-pole checks, and 100,000 signed comparisons with the Feynman result. Separately, at seed 134791, each paper formula and each automatic summand agreed in sign and magnitude with the elementary amplitude at 100,000 points. The relative tolerance was $10^{-8}$. There were three 160-bit replays for each physical form and twelve for each spurious form. The largest relative discrepancies after replay were $1.03\times10^{-48}$ or less for the physical forms and $9.44\times10^{-47}$ or less for the spurious forms. The [seven-point external validation records](../qed-seven-point-external.tsv) separately cover the elementary exports with CalcHEP and MadGraph.

## Reproduce the check

Run from the HEPCAT checkout. The construction prints its output directory and the two elementary expressions. Set `run_dir` to that directory.

```sh
build=tests/results/native-reduce-build
cmake -S csource -B "$build"
cmake --build "$build" --target hepcat-qed
"$build/hepcat-qed" --family eeAAAAA --helicity '+++--' --construct-only \
    --skip-formulas --workers 1 --points 100000 --seed 134791
```

Compile the standalone replay. If GMP is outside the compiler search paths, supply its include and library directories using `-I` and `-L`.

```sh
bench=tests/benchmarks/publication-qed
c++ -std=c++17 -O2 -Icsource "$bench/seven-point-two-negative-replay.cpp" \
    "$build/libhepcat_reduce.a" -lgmpxx -lgmp -o "$build/replay-seven-point"

for pref in physical spurious; do
    expr="$run_dir/$pref/eeAAAAA/+++--.expr"
    seed="$run_dir/$pref/paper-summand.expr"
    python3 "$bench/seven-point-two-negative.py" "$pref" "$expr" > "$seed"
    "$build/replay-seven-point" "$pref" "$expr" "$seed"
    "$build/replay-seven-point" "$pref" "$expr" auto
done
```
