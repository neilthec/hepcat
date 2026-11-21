# Five positive photons and one negative photon

The [paper equations](eight-point-one-negative.tex) use all incoming
momenta, an electron and positron on legs 1 and 2, five positive photons
on legs 3 through 7, and a negative photon on leg 8. There are 720
ordered trees, 62 physical channels and 120 permutations of the positives.

## Construct and reproduce

From the repository root with the QED native driver built, run

```sh
tests/results/native-reduce-build/hepcat-qed --family eeAAAAAA --helicity '+++++-' \
  --construct-only --skip-formulas --workers 1 --points 100000 --seed 816181
```

Model 3 supplies the cubic vertices. Models 4 and 5 supply the lower-point
amplitudes on the cuts. The signed Feynman diagrams come from the separate
CalcHEP Feynman-rule model. The eight-point case has no supplied answer.
The C++ notebook prints the results directory. The native expression, plain
text, LaTeX and checks are together under each pole preference.

To rerun the independent [transcription of the paper](eight-point-one-negative.cpp)
with a sibling SPINAS checkout, one example build on macOS is

```sh
c++ -std=c++17 -O2 -Icsource -Isource/internal/export -I../spinas/include \
  -I/opt/homebrew/include tests/benchmarks/publication-qed/eight-point-one-negative.cpp \
  tests/results/native-reduce-build/libhepcat_reduce.a -L/opt/homebrew/lib \
  -lgmpxx -lgmp -o tests/results/eight-point-one-negative
tests/results/eight-point-one-negative 100000 2816191
tests/results/eight-point-one-negative 100000 3816201 "$MG_REFERENCE"
```

For the last command, use a HEPCAT MadGraph standalone reference library for
`e- e+ > a a a a a a` with $m=1$ and $e=1/3$ ($\alpha^{-1}=36\pi$).
The code fixes the six photon helicities and explicitly sums the four
electron/positron spin choices in both calculations. The signed Feynman
test uses the model parameters, including $m=0.0005$. It compares the
complex amplitudes for coherent massive spin states and keeps their phase.

## Where the simplification comes from

The physical formula has two terms when expressed in endpoint currents.
The electron end starts with a mixed pair or a positive pair. Separate
the possibilities at the middle propagator or the contact straddling it.
On each side, summing adjacent orderings leaves only two- and three-photon
currents. All denominators are physical electron virtualities.

For the short spurious formula the five orbital numerators are independent of
the position of the negative photon. The longitudinal Ward identity holds
for the sum of its insertions at fixed positive-photon order. Consequently
its square reference can be the middle positive photon in that order.
Reversal of the fermion line pairs the six gaps and leaves three terms
multiplying a common product. This form has only external photon brackets
as spurious denominators.

A different arrangement has a lower momentum-weighted score. It first
combines adjacent gap insertions using the Ward identity. There are now
five possible marked photons. Reflecting the fermion line reduces them to
two mirror pairs and a central term of weight one half under the endpoint
exchange projector. The four-emission tail splits into four independent
emissions and a marked two-photon contraction. Recursing only on the
remainder extends the same algebra used for shorter tails.

The public C++ reducer recovers this organization from a saved expression
and its masses by matching every signed Laurent coefficient. The comparison
identifies the two complementary label sets of a physical propagator before
matching, as either can occur in a saved result. The sign
of a reflected marked term is $(-1)^n$ for $n$ companion photons. The
rule requires a single minority helicity, a complete equal-mass fermion
line, and the signed current match or its generated diagram certificate.
The six-photon tests exercise the mirrored term in both the single-group
API and the signed presentation. The elementary amplitudes remain the
inputs for numerical export.

## Validation and complexity

Both native constructions passed all 62 signed factorization channels
(1,488 chart samples), the fixed-angle $E^{-4}$ bound (12 samples),
reference independence, and 100,000 comparisons apiece for final reduction
and signed Feynman agreement. Cancellation of the detected spurious poles
passed on 21 chart samples. The single-worker construction and checks took
575.81 s for AllPhysical and 949.18 s for AllSpurious before the final
permutation simplification change. The unchanged elementary expressions
score 59,110 and 69,988. Reducing their saved forms gives weighted summand
scores of **1,028** and **807**, counting the endpoint exchange as an
operator. The previous spurious presentation scored 1,634. The paper also
gives a form with only external-bracket spurious poles.

The following are maximum symmetric relative discrepancies for the
independent paper transcription. Each column uses 100,000 points per formula.
Replays use 160-bit HEPCAT arithmetic with consistent on-shell momenta.
For the MadGraph comparison its original native weight is retained.

| Formula | Signed Feynman, native | Replays | Max on replay | MadGraph rate, native | Replays | Max on replay |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Physical poles | 3.36e-9 | 0 | n/a | 7.79e-12 | 0 | n/a |
| External-bracket spurious poles | 3.72e-5 | 535 | 3.91e-45 | 5.08e-10 | 0 | n/a |
| Ward rearrangement | 0.948 | 11,317 | 2.14e-41 | 6.82e-4 | 77 | 3.09e-13 |

Every replay passed. The native thresholds were $10^{-8}$ for the signed
amplitudes and $10^{-7}$ for the polarized rates. In the independent
Feynman calculation the physical form met its tolerance at every point
in native arithmetic. The Ward rearrangement has the shortest automatic
summand and the greatest sensitivity to roundoff in this comparison.

All 47 native regression tests passed. At 10,000 points per case, all
40 lower QED constructions (10 four-point, 12 five-point, 18 six-point,
counting both pole preferences) passed their signed checks and complexity
targets. The seven-point single-negative-photon control retained summand
scores of 331 and 408 on the same 10,000-point regression setting.

A fresh eight-point construction with the final reducer, seed 816181 and
10,000 points per comparison printed summand scores of **1,028** and
**807** directly. It took 421.83 s and 788.18 s respectively on one worker.
Both saved elementary expressions were byte-identical to those in the
100,000-point construction.
