# Massive QED with one negative-helicity photon

This research representation describes a massive electron and positron, one
negative photon, and any number of positive photons. It is separate from
`ConstructAmplitude`; it does not change preparation, completion, existing
models or notebook results. The analytic momenta are all ingoing. No internal
line is put on shell.

Choose the negative photon `r` as the angle reference of every positive photon.
All polarization bispinors then share the same angle spinor. The positive
photons have scalar chiral emission vertices. Only the negative photon has a
rank-one spin vertex. As a result the full amplitude needs just
`<1^a 2^b>` and `<1^a r><2^b r>` as its massive-spin tensors.

The scalar current for endpoint `c = 1,2` is

```math
J_{c,r}(\varnothing)=1,
\qquad
J_{c,r}(S)=\frac{
 \displaystyle\sum_{i\in S}J_{c,r}(S\setminus\{i\})
 \frac{\langle r|p_c+\sum_{j\in S\setminus\{i\}}p_j|i]}{\langle ri\rangle}}
 {\left(p_c+\sum_{i\in S}p_i\right)^2-m^2}.
```

For the positive-photon set `P`, the result is

```math
\mathcal M_N^{ab}=\frac{(-\sqrt2e)^N}{m}
 \sum_{L\sqcup R=P}(-1)^{|R|}J_{1,r}(L)J_{2,r}(R)
 \left[
 \langle1^a2^b\rangle
 \frac{\langle r|p_1+\sum_{i\in L}p_i|q]}{[rq]}
 +\langle1^ar\rangle\langle2^br\rangle
 \right].
```

`N` counts photons, not total external particles. The endpoint-2 current uses
positive `p2`; do not also insert the right-side orientation sign inside it.
The prefactor matches HEPCAT's existing cubic vertices and compact four-, five-
and six-point formulas. The relative plus sign between the two spin tensors
is essential.

`q` can be an actual positive photon, leaving no Xi reference spinor. Square
Schouten reduces changing `q` to a partition sum with insertion
`<r|p1 + sum(L)|r]`. This insertion equals the difference of the two endpoint
virtualities. Applying both current recurrences pairs every term into
`<r|-p_r-p_i|i] = 0`. Reference independence therefore follows using only
external shells and total momentum conservation. The C++ default chooses the
positive photon maximizing `abs([r q])^2/E_q`, avoiding an unnecessarily
collinear reference and dependence on photon list order.

This is a cached recursive formula, not a claim to the shortest nonrecursive
closed product. It requires `2^(N-1)` partitions instead of `N!` complete
Dirac-line diagrams. The straightforward implementation also adds the momenta
in each vertex, so its work is `O(N^2 2^(N-1))`. Matrix-cached subset sums can
reduce this to `O(N 2^(N-1))`. The representation has apparent bracket poles and
can lose accuracy through cancellations. It is not an AllPhysical formula.
Its `1/m` normalization requires a nonzero fermion mass; a massless limit
cannot be taken term by term.

## Mathematica

Load the normal package and the research helper:

```wolfram
Get["~/code/hepcat/source/HEPCAT.wl"];
Get["~/code/hepcat/tests/checks/fixtures/QED-one-minus-formula.wl"];
model = ReadModel["~/code/hepcat/models", 3];
amplitude = QEDOneMinusFormula[4,
  ModelParameter[model, "Me"], ModelParameter[model, "EE"]];
```

This example has three positive photons on legs 3,4,5 and one negative on
leg 6. The result is `{externalMassRules, expression}`. Optional
`"NegativePhoton"` chooses another photon label; `"SquareReference"` chooses
another positive photon or an explicit square spinor. The latter must be
noncollinear with the negative photon. `"ReturnCoefficients" -> True` exposes
the two scalar coefficients and the Ward diagnostic.

Run the exact low-point regression directly:

```sh
/Applications/Wolfram.app/Contents/MacOS/WolframKernel \
  -script tests/benchmarks/one-minus-symbolic-tests.wl
```

All 32 checks passed. Their rational on-shell/conserving charts keep one
coordinate symbolic and leave internal virtualities unconstrained. They
supplement the derivation and numerical tests rather than constituting a
generic symbolic proof by themselves.

## Independent MadGraph validation

Build the evaluator against a matching SPINAS precision:

```sh
clang++ -O2 -std=c++11 -DWITH_LONG_DOUBLE \
  -I/path/to/spinas/include -Isource/internal/export \
  tests/benchmarks/one-minus-validation.cpp /path/to/spinas/build/libspinas.a \
  -o /path/to/one-minus-validation
```

Use `source/internal/export/madgraph-runtime.py` to generate an independent reference, as in
the all-positive section of `tests/benchmarks/README.md`. Include only electron and
photon interactions, match masses and charge, and turn all widths off. The
fixed photon helicities remain independent of the HEPCAT formula. For example:

```sh
/path/to/one-minus-validation --photons 8 --points 100 \
  --mass 1.5 --charge 0.3333333333333333333 \
  --reference /path/to/reference/libhepcat_madgraph.dylib \
  --seed 19062034 --output /path/to/results/N8
```

The default seed uses the time. An explicit seed reproduces a run. The driver
stores complete momenta, all four complex spin amplitudes and every discrepancy.
It compares raw sums over the four massive spins, with the last photon fixed
negative and all others positive. There are no initial averages or final
identical-photon factors on either side. Photon outgoing crossing, including
angle-spinor signs, occurs once inside the C++ formula.

`validation-summary.json` records the November 6 run: 1,000 random points at
each `N=2,...,6` and 100 at each `N=7,8`. All 5,200 final comparisons and
reference/permutation/Ward checks passed. The largest relative MadGraph
discrepancy was `3.90e-9`. Direct complex-component comparisons with established
three-/four-photon expressions also passed, without a fitted phase.

The original label-first-reference eight-photon run is retained as a numerical
conditioning control. One point narrowly failed its `1e-7` internal
permutation threshold (`1.08e-7`). Replaying the same input bits with a true
64-bit mantissa reduced this to `7.30e-11`, without correcting or regenerating
the inputs. The stable geometric reference reduced the native-precision
discrepancy to `2.09e-9` on those same points. No point was discarded and no
tolerance was relaxed. Apple arm64 `long double` used here has a 53-bit mantissa.

## Relation to published work

All-multiplicity massive one-minus color-ordered QCD amplitudes are known:
[Ochirov, arXiv:1802.06730v3](https://arxiv.org/abs/1802.06730v3), section 4.2.
Proper Abelianization must include all photon orderings, or the corresponding
basis relations, such as those in
[Johansson and Ochirov, arXiv:1507.00332](https://arxiv.org/abs/1507.00332).
Simply summing positive-photon permutations of an adjacent-minus primitive
would omit orderings. This two-current QED representation needs a detailed
literature comparison before claiming novelty.
