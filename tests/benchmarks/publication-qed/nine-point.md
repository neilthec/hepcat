# Nine-point amplitudes in one-flavor tree QED

The manuscript formulas in [nine-point.tex](nine-point.tex) keep the massive spins open. For a fixed all-incoming order, the four families have 128, 32, 8 and 2 labeled photon helicity assignments. Permutations of equal-helicity photons, independent alternating sums for electrons and positrons, and parity reduce the calculation to ten representatives per pole preference.

| Charged pairs | Photon helicities up to parity | Labeled diagrams per helicity | Physical cuts | Terms in the central-vertex or charged-tree sum |
|---:|---|---:|---:|---|
| 1 | +++++++, ++++++-, +++++--, ++++--- | 5,040 | 126 | 1, 3, 5, 7 |
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
"$build"/nine-point-replay 4 1 0 32 954400 paper
```

The numerical replay arguments are the charged-pair count, photon count, negative-photon count, number of phase-space points, seed and reference. Select `signed` to evaluate the individually sewn Feynman diagrams or `paper` for a comparison of the manuscript tensors with the independent-gauge current. A path to a standalone MadGraph reference compares the polarized rate at exactly the same point, exhaustively summing the massive spins. Append `tensor` to evaluate the printed line tensors directly in that spin sum. The default MadGraph replay uses the faster subset currents. All replays here use mass 1 and charge 1/3. The native notebook also runs at the model parameters.

For MadGraph use the standard HEPCAT standalone builder in `source/internal/export/madgraph-runtime.py`, the `sm-full` model, allowed particle PDGs 11 and 22, and widths zero. Begin the physical PDG list with 11, -11. For two pairs the first two outgoing particles are 11, -11. For three or four pairs the successive outgoing massive particles have PDGs -11, 11. Append a 22 for each photon. Set the electron mass and charge as above. For the larger standalone outputs use `JampSplit` equal to 128 and `OptimizationLevel` equal to 0. These settings bound the lengths of the Fortran sums generated by MadGraph and the time spent optimizing its matrix subroutine.

The literal paper tensor sums in both pole preferences agreed with the independent-gauge current at 32 coherent-spin points for each of the 20 helicity counts. Separate eight-sample expansions of the seven-positive physical summand, the factored spurious summand, and the ordered spurious product agreed with the ordinary seven-photon current within 3.8e-15, 9.7e-13, and 3.2e-14 respectively. The corresponding gap-product check for one negative photon agreed within 1.3e-12.

For each of the 20 helicity counts, both literal tensor formulas were also compared with independently generated MadGraph at four phase-space points. The maximum relative differences in the massive-spin-summed rates were 1.42e-13 for the physical reference and 4.31e-12 for the spurious reference. At 32 additional points per count, the subset currents agreed with MadGraph to 1.72e-12.
