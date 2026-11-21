# Four positive photons and two negative photons

The [paper formulas](eight-point-two-negative.tex) describe the all-incoming
amplitude with $e,E$ on legs 1,2, positive photons on legs 3 through 6,
and negative photons on legs 7,8. The QED construction uses model 3 for
vertices and model 4 or 5 for the lower-point amplitudes on its cuts.

## From 720 diagrams to two or three terms

Split each ordered fermion line at its middle electron propagator.
There are 20 ways to put three photons on the electron side. Each split
contains all 36 orders of the photons within its two triples. The open
two-photon currents already derived at seven points give each three-photon
current as a sum over the photon closest to the middle. The mass term in
the propagator joins components of equal chirality and the momentum term
joins components of opposite chirality.

In AllPhysical, use the electron momentum as the reference for every
polarization. All the orbital emission factors at that endpoint vanish.
Independent permutations of the four positive and the two negative
photons leave three terms, corresponding to zero, one or two negatives
on the electron side.

In AllSpurious, use one negative photon as the reference of the four
positives and use each negative as the reference of the other. The
endpoint exchange acts in the same gauge and pairs complementary
partitions. The paper gives the result as two terms under a signed
endpoint exchange and the two photon permutations. The charged
propagators retain their physical poles. All remaining denominators
are external photon brackets.

The C++ reducer also tries absorbing the middle numerator into the left
or right three-photon current. It picks the smallest reduced summand
for each partition using the usual momentum-weighted complexity score.
For an imported elementary expression it first matches the complete
signed Laurent coefficients, including any overall scalar multiplier.
The construction supplies the same representation from its generated
fermion line. The separate replay below compares both the paper transcription and the
reduced, reloaded expressions to independent matrix elements.

## Reproduce the calculation

Build the QED driver and run it from the repository root.

```sh
cmake -S csource -B tests/results/native-reduce-build -DCMAKE_BUILD_TYPE=Release
cmake --build tests/results/native-reduce-build --target hepcat-qed --parallel 1
tests/results/native-reduce-build/hepcat-qed --family eeAAAAAA --helicity '++++--' \
  --construct-only --skip-formulas --workers 1 --points 100000 --seed 826181
```

Set `RUN_DIR` to the output directory printed by this command. To test the
[separate transcription of the paper equations](eight-point-two-negative.cpp)
and the simplifications of the driver's saved expressions, compile the
replay. This example assumes a sibling SPINAS checkout and Homebrew GMP.

```sh
c++ -std=c++17 -O2 -Icsource -Isource/internal/export -I../spinas/include \
  -I/opt/homebrew/include tests/benchmarks/publication-qed/eight-point-two-negative.cpp \
  tests/results/native-reduce-build/libhepcat_reduce.a -L/opt/homebrew/lib \
  -lgmpxx -lgmp -o tests/results/eight-point-two-negative
tests/results/eight-point-two-negative --construction "$RUN_DIR" 100000 826191
tests/results/eight-point-two-negative --construction "$RUN_DIR" 100000 826201 "$MG_REFERENCE"
```

Use a HEPCAT-compatible MadGraph standalone library for
`e- e+ > a a a a a a` with $m=1$, $e=1/3$ and widths off. The replay
fixes all six photon helicities. It sums the four initial spin weights
on both sides. The independent signed Feynman check uses the QED
electron mass $0.0005$ and coherent massive-spin states. Both comparisons
record the largest native discrepancy and replay points exceeding their
tolerance in 160-bit HEPCAT arithmetic. The MadGraph comparison retains
the original MadGraph weight during replay.

## Validation and complexity

The elementary AllPhysical expression has weighted complexity **137,062**
and the AllSpurious expression **199,628**. Applying the public reducer to
the saved elementary expressions gives permutation summands of **4,060**
and **5,525** respectively. These scores measure one summand with the
permutations and signed exchange left as operators. The earlier
constructor printed a physical summand of 8,863 and the full elementary
spurious expression.

Both constructions passed the 62 signed physical residues on 1,488 chart
samples, the fixed-angle $E^{-4}$ bound on 12 samples and the reference
check. The spurious-pole cancellation passed on 21 chart samples. Each
elementary amplitude also passed 100,000 signed Feynman comparisons
and 100,000 final reduction comparisons. The signed Feynman test
rechecked five physical and 28 spurious points at 160-bit precision.

For the separate comparisons, each entry below is the largest symmetric
relative discrepancy over 100,000 points. The numerator is the absolute
difference and the denominator is the larger of the two magnitudes.

| Expression tested | Signed Feynman amplitude | Polarized MadGraph rate |
| --- | ---: | ---: |
| Paper, AllPhysical | 8.44527e-11 | 5.86856e-12 |
| Paper, AllSpurious | 8.44946e-11 | 1.11660e-10 |
| Automatic summand, AllPhysical | 8.44508e-11 | 5.86856e-12 |
| Automatic summand, AllSpurious | 8.44413e-11 | 5.69797e-11 |

All four expressions met the native signed-amplitude tolerance of
$10^{-8}$ and rate tolerance of $10^{-7}$ at every point. These two
independent samples required zero high-precision replays.

All 47 native regression tests passed, including tests that permute the
external labels, reverse all helicities, exercise the three-plus,
three-minus middle split and reject multiplication by an extra momentum
factor when matching a saved expression. The native QED driver passed all
40 four-, five- and six-point helicity and pole selections at 10,000 points
per comparison. The seven-point single-negative controls retained summand
scores of 331 and 408. With two negatives they scored 858 and 1,864.

The fresh end-to-end construction from models 3, 4 and 5 took 731.54 s
for AllPhysical and 1,141.26 s for AllSpurious on one worker, including
the 100,000-point checks. Both elementary expressions were byte-identical
to the baseline construction. The freshly constructed summand scores were
4,060 and 5,530. The separate signed coefficient match on reloading the
spurious expression produced the slightly lower score of 5,525 above.
The text printed by the constructor fell from 31,569 to 5,557 bytes for
AllPhysical and from 51,466 to 10,008 bytes for AllSpurious.

We also read the two freshly printed plain-text summands back for a
separate numerical check. Each generalized Mandelstam was evaluated
as the square of its indicated momentum sum. We applied the printed
permutations and the indicated minus sign under endpoint exchange.
On the same 100,000-point samples, the printed physical and spurious
formulas agreed with signed Feynman amplitudes to 8.44462e-11 and
8.44798e-11, and with polarized MadGraph rates to 5.88968e-12 and
9.33659e-11, respectively. Both passed throughout in native precision.
