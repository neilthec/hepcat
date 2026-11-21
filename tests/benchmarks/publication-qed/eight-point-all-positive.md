# Six positive-helicity photons and one electron pair

The [paper equations](eight-point-all-positive.tex) use all incoming labels
$(1_e,2_E,3^+,4^+,5^+,6^+,7^+,8^+)$. There are 720 ordered trees and
62 physical channels. A cut can use a seven-point amplitude. QED models 4
and 5 now contain the full seven-point helicity catalog for this sewing.

Each model contains six five-photon representatives, four representatives
with two fermion pairs and three photons, and both photon helicities for
three fermion pairs. Permutations give 42 labeled photon-helicity
assignments in each model. We used the HEPCAT model writer, reloaded the
files, and compared every row with its input. The two mixed four-fermion
spurious entries needed exact polynomial expansion to verify the writer's
intermediate sign distribution. For the six-fermion spurious entries we
used the signed, common-denominator form with the composite-reference pole
described in [the seven-point derivation](seven-point-six-fermions.md).
The lower-multiplicity rows in both files are byte-identical to the
previous versions.

## Construction and simplification

From the repository root, build the C++ notebook and run

```sh
cmake -S csource -B tests/results/native-reduce-build -DCMAKE_BUILD_TYPE=Release
cmake --build tests/results/native-reduce-build --target hepcat-qed --parallel 1
tests/results/native-reduce-build/hepcat-qed --legs 8 --workers 1 \
  --construct-only --skip-formulas --points 100000 --seed 806181
```

The cubic diagram model generates the trees. The factorization model supplies
only lower-point amplitudes. The separate conjectured all-positive formula
enters the signed comparison after construction.

| Representation | Elementary weight | Weight of one automatic summand | Sizes of its signed monomial orbits |
| --- | ---: | ---: | --- |
| AllPhysical | 16,515 | 91 | 15, 90, 90, 360, 360, 360, 360 |
| AllSpurious | 16,530 | 60 | 30, 180, 360, 360, 360, 360, 720, 720 |

The reducer checks adjacent transpositions, including every coefficient
and spinor orientation. An orbit of size $d$ contributes $d/720$ times
any one of its images to the summand. This reduces 1,635 physical and
3,090 spurious monomials to seven and eight representatives. Factoring
can then choose different images to expose common factors. A bounded
expansion for large equal-mass groups lets this work on the eight-point
expressions. The rule uses external masses, label permutations and exact
algebra.

The second spurious formula in the paper is a single product for each
ordering. Expanding the ordered scalar-emission current selects a terminal
ordered pair and an ordering of its four remaining emissions. This gives
the 720 terms without a combinatorial prefactor. The product is compact
in print and has a larger momentum-weighted score. The automatic scores
above refer to the factored orbit formulas. The comparison is among these
representations and does not establish a global minimum.

## Independent replay

The [C++ replay](eight-point-all-positive.cpp) transcribes the printed
equations, sums all 720 photon permutations, and compares complex amplitudes
with the off-shell Feynman rules. The optional MadGraph mode calls its
standalone matrix element four times per momentum, covering the incoming
spin pairs at fixed photon helicities. On the HEPCAT side it sums the four
components of the common massive-spin bracket explicitly. Both sides use
the same momenta and the same model parameters.

With SPINAS in a sibling checkout and GMP installed, one example build on
macOS is

```sh
c++ -std=c++17 -O2 -Icsource -Isource/internal/export -I../spinas/include \
  -I/opt/homebrew/include tests/benchmarks/publication-qed/eight-point-all-positive.cpp \
  tests/results/native-reduce-build/libhepcat_reduce.a -L/opt/homebrew/lib \
  -lgmpxx -lgmp -o tests/results/eight-point-replay
tests/results/eight-point-replay 100000 806191
tests/results/eight-point-replay 100000 806201 "$MG_REFERENCE" 1 0.333333333333333333
```

For the last command, set `MG_REFERENCE` to a HEPCAT MadGraph standalone
reference library for `e- e+ > a a a a a a` and supply the electron mass
and charge in its parameter card. The example uses $m=1$ and
$\alpha^{-1}=36\pi$. The signed Feynman replay uses the QED model defaults.

## Validation results

The constructive runs used one worker and seed 806181. Each form passed 62
signed physical residues on 1,488 chart samples, the fixed-angle $E^{-4}$
bound on 12 samples, and the reference-spinor check. The spurious denominator
cancellation passed on 21 chart samples. The signed all-positive reference,
final reduction, and Feynman comparisons each passed at 100,000 points per
form. The largest relative difference from the direct Feynman sum was
$2.14\times10^{-8}$ in native arithmetic. Each form needed two 160-bit
on-shell replays, with a maximum discrepancy of $1.82\times10^{-48}$.
The original construction and checks took 471 s (physical) and 927 s
(spurious) on one worker. We measured the summand scores separately by
loading the saved expressions into the updated public reducer.

The independent transcription of the three paper formulas used seed
806191 and 100,000 points per formula. Each had two signed Feynman
comparisons above $10^{-8}$ in native arithmetic (maximum
$2.33\times10^{-8}$). All three passed on the reconstructed 160-bit
kinematics, with maximum replay discrepancy below $9.1\times10^{-47}$.

The live MadGraph standalone comparison used seed 806201, $m=1$ and
$e=1/3$. All 100,000 points passed at fixed positive photon helicities,
summing the four electron and positron spin assignments. Maximum relative
differences in the squared amplitude were $2.52\times10^{-12}$ for the
physical kernel, $1.12\times10^{-11}$ for the factored spurious kernel,
and $6.88\times10^{-12}$ for the ordered spurious product. The absolute
phases drop out of this comparison. The Feynman checks above retain them.

All 47 native regression tests passed. We also reconstructed the ten
four-point, twelve five-point and eighteen six-point QED cases counting
the two pole choices separately, at 10,000 comparison points per case
(seed 806211). All 40 passed their signed checks and elementary complexity
targets. The five-positive-photon regression retained summand scores of
39 and 24 for physical and spurious poles, respectively.
