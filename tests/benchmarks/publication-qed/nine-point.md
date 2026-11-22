# Nine-point amplitudes in one-flavor tree QED

The manuscript formulas in [nine-point.tex](nine-point.tex) keep the massive spins open. For a fixed all-incoming order, the four families have 128, 32, 8 and 2 labeled photon helicity assignments. Permutations of equal-helicity photons, independent alternating sums for electrons and positrons, and parity reduce the calculation to ten representatives per pole preference.

| Charged pairs | Photon helicities up to parity | Labeled diagrams per helicity | Physical cuts | Terms in the central-vertex or charged-tree sum |
|---:|---|---:|---:|---|
| 1 | +++++++, ++++++-, +++++--, ++++--- | 5,040 | 126 | 1, 2, 3, 4 |
| 2 | +++++, ++++-, +++-- | 13,440 | 188 | 3, 5, 6 |
| 3 | +++, ++- | 18,144 | 186 | 6, 10 |
| 4 | + | 17,280 | 172 | 4 |

A term here is a current with its attachment orders already summed. HEPCAT also measures the weighted complexity of scalar spinor expressions. The two counts answer different questions. In the seven-positive example we give both a factored spurious summand and an ordered product. The product is shorter to print, while the factored form has a lower spinor complexity. Extracting three common reference brackets and resolving the massless momenta reduces its score from 372 to 208. Reducing the saved spurious expression recovers that score. The seven-positive physical summand scores 506. For the one-photon, four-pair family the paper's spurious form uses the momentum of the emitting pair as a reference. Its extra denominator is the difference of two invariants. The compact formulas are candidates for the simplest representation. The orbit calculation proves their multiplicities within the specified tree decomposition.

## Construction and factorization models

Build from the repository root. The native QED example reads the vertices from model 3, the lower amplitudes from model 4 or 5, and the independent Feynman rules from the CalcHEP-format QED model. Models 4 and 5 each contain the 16 eight-point rows, with both members of each distinct parity pair. The catalog test reads the saved model expressions and compares them with signed Feynman amplitudes.

```sh
build=tests/results/native-reduce-build
cmake -S csource -B "$build" -DCMAKE_BUILD_TYPE=Release
cmake --build "$build" --parallel 1
"$build"/hepcat-check-qed-catalog .
"$build"/hepcat-qed --legs 9 --family eeAAAAAAA --helicity '+++++++' \
    --construct-only --skip-formulas --points 100000 --workers 1 --seed 907111
```

Use `--legs 9` without a family or helicity filter to run every cell. The family names are `eeAAAAAAA`, `eeeeAAAAA`, `eeeeeeAAA` and `eeeeeeeeA`. The driver prints the location of the formulas, validation reports and timing summary. The compact projection accompanies the ordinary expression wherever the reducer recovers one.

## How the formulas were checked

The ordered-tree checker emits the actual labeled tree plans of the independent Feynman model. The Python checker enumerates the images of each manuscript occupation, verifies the printed divisor, and compares every attachment word and fermion matching with the planner. Across the four charged families this covers 53,904 distinct labeled diagrams at a fixed choice of photon helicities. The sign of a matching is its fermion permutation sign. Signed numerical checks exercise that sign with coherent massive spins.

The C++ replay evaluates the line tensors by summing the attachments on each line and contracting the exchanged photons. A separate subset recurrence adds all charged currents with Grassmann shuffle signs and a different gauge. The seven-photon checker independently expands the displayed Bose sums at fixed momenta and spin labels. The examples below assume Homebrew GMP and a sibling SPINAS checkout. On Linux use the corresponding GMP include and library directories.

```sh
bench=tests/benchmarks/publication-qed
for name in nine-point-ordered-trees nine-point-seven-photons nine-point-replay; do
    c++ -O3 -std=c++17 -Icsource -Isource/internal/export -I../spinas/include \
        -I/opt/homebrew/include "$bench/$name.cpp" "$build/libhepcat_reduce.a" \
        -L/opt/homebrew/lib -lgmpxx -lgmp -o "$build/$name"
done
"$build"/nine-point-ordered-trees | python3 "$bench/nine-point-orbits.py" --check-trees
"$build"/nine-point-seven-photons tests/results/nine-summands --paper
"$build"/nine-point-seven-photons tests/results/nine-ordered --ordered
"$build"/nine-point-seven-photons tests/results/nine-one-negative --gaps
"$build"/nine-point-seven-photons tests/results/nine-reloaded --reload
"$build"/test_radiative_current models tests/reference-models/calchep --nine-point
"$build"/nine-point-replay 4 1 0 32 954400 paper
```

The numerical replay arguments are the charged-pair count, photon count, negative-photon count (or an explicit helicity word, for example `+-++--+`), number of phase-space points, seed and reference. Select `signed` to evaluate the individually sewn Feynman diagrams or `paper` for a comparison of the manuscript tensors with the independent-gauge current. A path to a standalone MadGraph reference compares the polarized rate at exactly the same point, exhaustively summing the massive spins. Append `tensor` to evaluate the printed line tensors directly in that spin sum. The default MadGraph replay uses the faster subset currents. Its physical gauge fixes the first charged momentum globally. In the single-photon spurious replay it uses the sum of the first oppositely charged pair momenta. The literal tensor replay separately tests using each emitting pair as the reference, as in the paper. All replays here use mass 1 and charge 1/3. The native notebook also runs at the model parameters.

For MadGraph use the standard HEPCAT standalone builder in `source/internal/export/madgraph-runtime.py`, the `sm-full` model, allowed particle PDGs 11 and 22, and widths zero. Begin the physical PDG list with 11, -11. For two pairs the first two outgoing particles are 11, -11. For three or four pairs the successive outgoing massive particles have PDGs -11, 11. Append a 22 for each photon. Set the electron mass and charge as above. For the larger standalone outputs use `JampSplit` equal to 128 and `OptimizationLevel` equal to 0. These settings bound the lengths of the Fortran sums generated by MadGraph and the time spent optimizing its matrix subroutine.

The literal paper tensor sums in both pole preferences agreed with the independent-gauge current at 32 coherent-spin points for each of the 20 helicity counts. Anticommuting the momentum through the photon vertex on a singly emitting terminal line reduces its two diagrams to an orbital term and a field-strength insertion. Evaluating that form directly gives maximum relative differences of 7.64e-13 and 2.83e-12 across these samples. Separate eight-sample expansions of the seven-positive physical summand, the factored spurious summand, and the ordered spurious product agreed with the ordinary seven-photon current within 3.8e-15, 9.7e-13, and 3.2e-14 respectively. The corresponding gap-product check for one negative photon agreed within 1.3e-12.

For each of the 20 helicity counts, both literal tensor formulas were also compared with independently generated MadGraph at four phase-space points. The maximum relative differences in the massive-spin-summed rates were 1.41e-13 for the physical reference and 1.11e-12 for the spurious reference. At 32 additional points per count, the subset currents agreed with MadGraph to 1.72e-12.

All 170 labeled photon helicity assignments were also tested with the literal tensor formulas on two new MadGraph points per assignment. In lexicographic order with `+` before `-`, assignment `i` (zero based) for `p` charged pairs uses initial seed `998000 + 1000*p + 2*i`. The maximum relative differences were 3.81e-13 and 5.13e-12 for the physical and spurious forms.

For two or more charged pairs, a charge-conjugation projector on every line supplies the other chirality of a terminal current or the opposite ordering of two exchanged photons on a nonterminal current. This reduces the explicit chiral or ordered contributions in a seed by factors of 4, 8 and 16 in the three families. Real-photon references stay fixed when the endpoints of a single word are exchanged. Use `projected` in place of `paper` or `tensor` to test this version. At 32 coherent-spin points per helicity count (initial seed `974000 + 100*p + h`), the largest relative signed-amplitude differences were 3.22e-12 and 1.90e-12. Direct MadGraph comparisons of the projected tensors for all 42 labeled helicity assignments in these families gave maximum relative rate differences of 7.63e-13 and 6.24e-13 at two points per assignment (initial seed `980000 + 1000*p + 2*i`).

For the single charged pair, order the two triples by their negative-helicity counts. When the counts agree, retain one chirality at the central photon. Endpoint reversal recovers the missing words and chiralities. The orbit checker counts every one of the 5,040 words in each chirality exactly once. The projected current agreed with independent-gauge recursion at 32 coherent-spin points per helicity count, with initial seed `972100 + h`. Maximum relative differences were 3.50e-12 and 5.04e-12. For all 128 labeled assignments the maxima against MadGraph at two points each were 4.15e-13 and 2.76e-12. The seed rule is the same as in the other projected families.

For an odd photon count with photon-only references the C++ Weyl-current reducer can also apply endpoint reversal to its scalar output. Both Weyl blocks share the photon reference labels. The reducer keeps the smaller block if its projected kernel improves the score. For seven photons with two or three negatives the selected spurious summands went from 324,706 to 162,353 and from 832,942 to 416,471. The `--reload` replay recovers and prints the two reduced expressions after a text round trip, then sums all signed photon permutations against the full amplitudes. The `--central` replay additionally tests the unreduced central candidates by expanding their signed operators and photon permutations; its largest relative errors in these two sectors were 2.39e-13 and 3.71e-13.

For four fermions and five photons, the full occupation kernel exceeds the scalar expansion budget. The reducer can still compare the leaf counts of algebraically certified candidates. Keeping one chirality on each charged line yields finite, measurable weights for the sum of the individually projected kernels. In order of zero, one and two negative photons these weights are 1,096,644, 1,354,800 and 1,578,424 for physical poles, and 851,880, 791,616 and 787,009 for spurious poles. The extended radiative-current replay applies both fermion antisymmetrizations and the photon permutations, and compares the resulting signed amplitude against the independent-gauge recurrence at eight coherent-spin points per case. The maximum relative difference across these six cases was 2.95e-13.
