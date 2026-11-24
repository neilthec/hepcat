# Central sewings for massive charged trees

For four fermion pairs the two electron propagators separate two three-leg branches. The remaining photon is unique. The 16 neutral partitions of size 2 against 6 and the 18 of size 4 against 4 give the 34 sewings derived in [the four-pair example](four-pair-sewing.tex).

For three pairs with real photons, remove the emissions temporarily. Each charged tree has one fermion propagator between the two photon exchanges. Restore the emissions and cut immediately beside the exchange on the net-electron side. This gives nine charged cuts and eight assignments of the three photons across each cut. The opposite side includes every emission between the exchanges. Each exchanged or internal charged momentum carries its full virtuality.

With electron labels 1, 3, 5, positrons 2, 4, 6 and three photons in P, the compact signed amplitude is

```text
-e^7 Alt(1,3,5) Alt(2,4,6) sum(L subset P) Wbar[6 <- 4,5](P minus L) X[1 <- 2,3](L)
```

Here an arrow attaches one ordered positron-electron pair to the indicated charged leg. X is amputated at its last exchange. Wbar includes the middle propagator and sums emissions after its exchange as well. Hold the photon polarization spinors fixed inside the unnormalized alternating sums. Each oriented image contains 504 diagrams. The two alternating sums supply all 36 images with their relative signs. The same formula works for every photon helicity and both choices of reference. The factor four in a projection of already antisymmetrized triple currents disappears once the seed contains only one local orientation.

## Reproduce the calculation

For eligible, verified constructions the native C++ library returns the compact form through `final_amplitude().sewing_form()`. Alternatively call `charged_tree_sewing::from_model` with an elementary model, ordered legs and pole preference. The latter constructs the same cut list directly from the interaction and charge inventory. The scalar expression, its complexity score and its SPINAS export are available separately. The measured implementation is the native cut evaluator.

Build from the repository root. The following commands assume a sibling SPINAS checkout and Homebrew GMP. Set the include and library locations for your installation.

```sh
cmake -S csource -B tests/results/native-reduce-build -DCMAKE_BUILD_TYPE=Release
cmake --build tests/results/native-reduce-build --target hepcat_reduce --parallel 1
c++ -O3 -std=c++17 -Icsource -Isource/internal/export -I../spinas/include \
    -I/opt/homebrew/include tests/benchmarks/publication-qed/centered-sewing-replay.cpp \
    tests/results/native-reduce-build/libhepcat_reduce.a -L/opt/homebrew/lib \
    -lgmpxx -lgmp -o tests/results/centered-sewing-replay
tests/results/centered-sewing-replay 3 3 512 20431001 seed
tests/results/centered-sewing-replay 3 3 10000 20421001 rooted
tests/results/centered-sewing-replay 3 3 96 20401001
tests/results/centered-sewing-replay 3 3 96 20411001 "$MG_THREE_PAIRS_THREE_PHOTONS"
tests/results/centered-sewing-replay 4 0 96 20411001 "$MG_FOUR_PAIRS"
tests/results/centered-sewing-replay 3 3 1000 20441001 "$MG_THREE_PAIRS_THREE_PHOTONS"
```

The first replay evaluates the ordered seed and alternating sums directly. The second uses recursion rooted on an external leg in an independent gauge. The third sums the previously published line tensors over pairings and emission orders. All include eight helicities at each of the indicated numbers of momenta. The remaining commands take a generated MadGraph reference library with the exhaustive-sum interface. Use mass 1, charge 1/3, zero widths, the canonical alternating fermion order, and outgoing photons. The code compares unnormalized rates and also checks each labeled photon polarization at the first twelve points of a sweep.

| Signed nine-point check | Momenta per helicity | Largest relative difference, physical | Largest relative difference, spurious |
|---|---:|---:|---:|
| Explicit 36-image seed | 512 | 8.56e-13 | 1.91e-12 |
| Ordered line tensors | 96 | 1.19e-12 | 1.19e-12 |
| Independent external-leg recursion | 10,000 | 6.00e-12 | 6.68e-12 |

On 1,000 further momenta (seed 20441001), the relative differences from the complete MadGraph spin and helicity sum stayed below 3.43e-13 for AllPhysical and 2.29e-12 for AllSpurious. For the first twelve of those points, checking the eight photon polarizations separately gave a maximum of 5.71e-14 across both pole choices.

The [timing data](centered-sewing-times.csv) contain each sweep. For three pairs and three photons the median time per complete rate over three 96-point sweeps was 0.910 ms physical, 0.907 ms spurious, and 111.0 ms for MadGraph on an Apple M3. The four-pair medians were 0.135, 0.135 and 4.06 ms. HEPCAT starts with external spinors, while MadGraph starts with momenta. The table also records the cost of generating the momenta and external spinors together. All internal state sums are coherent, and the external squared amplitudes are summed over every spin and photon helicity.

For peak memory, launch a fresh process for each evaluator. For example, append `--rate-only physical`, `--rate-only spurious`, or `--rate-only "$MG_THREE_PAIRS_THREE_PHOTONS"` after `3 3 12 20411001`. The [raw memory measurements](centered-sewing-memory.csv) include the checksum of the twelve complete rates from each run. Peak resident memory includes setup, the common phase-space driver and the selected evaluator. The respective medians of three fresh runs were 3.67, 3.67 and 4.61 MiB for three pairs and three photons, and 2.66, 2.66 and 2.55 MiB for four pairs.
