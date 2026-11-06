# Independent published one-minus QED comparison

This benchmark independently transcribes A. Ochirov, *Helicity amplitudes for
QCD with massive quarks*, JHEP **04** (2018) 089,
[arXiv:1802.06730v3](https://arxiv.org/html/1802.06730v3).
It compares the resulting **complete Abelian amplitude** with
`one-minus-formulas.hpp` at two and three photons. The two implementations
share SPINAS spinor primitives and random physical momenta. They do not share
their amplitude construction: the published implementation uses ordered
closed expressions and BCJ relations, while the tested implementation uses
the two scalar subset currents. No HEPCAT diagram, reduction or completion
function is called.

## Exact source and equation dictionary

* [Eq. (42), section 4.2](https://arxiv.org/html/1802.06730v3#S4.SS2)
  is an all-multiplicity massive-quark primitive with its sole negative gluon
  adjacent to the quark. The quark spins remain arbitrary. It is not the full
  QED amplitude and is not sufficient by itself if only the positive photons
  are permuted.
* [Eq. (58), section 5](https://arxiv.org/html/1802.06730v3#S5)
  is the paper's rearrangement of Eq. (42) into the two massive-spin tensors
  `<1^a 2^b>` and `<1^a r><2^b r>`. The benchmark transcribes this expression
  directly at total multiplicities four and five. The product in its sum is
  empty at five points, so no inferred all-multiplicity product is needed.
* [Eqs. (28), (53)](https://arxiv.org/html/1802.06730v3#S3.E28)
  supply the fundamental BCJ relation. Section 4.2 explicitly explains that
  these relations reconstruct positions of the negative gluon not adjacent
  to the quark. At five points the two adjacent-minus orderings form a basis.
* [Eq. (32), section 3](https://arxiv.org/html/1802.06730v3#S3.E32)
  is a separate direct four-point expression. Its translation is checked
  against Eq. (58) before the Abelian sum.
* Eqs. (7), (15), the footnote to Eq. (24), and the text below Eq. (29)
  specify polarization phases, massive Weyl conventions, generator
  normalization and the QED specialization.

Here `N` counts photons and the paper's `n=N+2` counts all particles. Its legs
1 and 2 are our massive endpoints, its leg 3 is the negative photon `r`, and
its remaining legs are positive photons. At five points the two directly
transcribed primitives are `A(1,r,a,b,2)` and `A(1,r,b,a,2)`.

All formal momenta obey `q1+q2+sum(qphoton)=0`. On a physical
`e- e+ -> photons` point we evaluate `q1=p1`, `q2=p2` and
`qphoton=-kphoton`. Each outgoing photon angle spinor changes sign and its
square spinor does not. Every photon momentum inserted in a chain also
changes sign. All internal virtualities stay off shell.

The massive square-spinor convention must be translated when testing Eq.
(32): the published Eq. (15) has `<p|p=-m[p|`, while the SPINAS spinors have
the opposite sign in this mixed relation. A published massive square spinor
therefore contributes a minus relative to the SPINAS square spinor in the
selected angle-spinor basis. No fitted spin rotation is used. Eq. (58) uses
only the angle massive-spin tensors and is the main oracle.

### Complete Abelianization

Write `c_i=q1.q_i` and `d_ij=q_i.q_j`. For two photons the second ordering is

```math
A(1,a,r,2)=\frac{c_r}{c_a}A(1,r,a,2).
```

For three photons, abbreviate the *ordered primitives only* by their photon
order, and set `X=A(r,a,b)` and `Y=A(r,b,a)`. Fundamental BCJ gives

```math
\begin{aligned}
A(a,r,b)&=-\frac{(c_a+d_{ar})X+(c_a+d_{ar}+d_{ab})Y}{c_a},\\
A(b,r,a)&=-\frac{(c_b+d_{br})Y+(c_b+d_{br}+d_{ab})X}{c_b},\\
A(a,b,r)&=-\frac{c_rX+(c_r+d_{ra})A(a,r,b)}{c_r+d_{ra}+d_{rb}},\\
A(b,a,r)&=-\frac{c_rY+(c_r+d_{rb})A(b,r,a)}{c_r+d_{ra}+d_{rb}}.
\end{aligned}
```

The implementation sums **all six** orderings. The two unused fundamental
BCJ relations are separate numerical consistency controls. The color
generators normalized by `Tr(Ta Tb)=delta(a,b)` are replaced by `sqrt(2)e`.
The same coupling normalization is used for both multiplicities and all
masses, charges and spin components.

### Declared phase convention

The benchmark's ordered function returns the published `A/i`. Its final
one-minus QED result is `-(sqrt(2)e)^N sum(A/i)`. This fixed minus relative to
the all-positive transcription is the one-negative-polarization convention:
the one-minus scalar-current derivation uses the undotted polarization block
`+sqrt(2)|r><q|/[rq]`, whereas the published slash polarization in Eq. (7b)
has `-sqrt(2)|r><q|/[rq]`. Multiplying one photon polarization by minus one
changes the amplitude by the same phase. This convention is declared before
evaluation and held fixed at both multiplicities. There is no ratio fitting,
point-dependent phase or spin-dependent phase alignment. The squared
amplitude comparison does not depend on this overall phase.

## Reproduce

From the HEPCAT root, using the same SPINAS precision as its static archive:

```sh
clang++ -O2 -std=c++11 -DWITH_LONG_DOUBLE \
  -I/path/to/spinas/include -Isource -Ibenchmarks \
  benchmarks/literature-one-minus-validation.cpp \
  /path/to/spinas/build/libspinas.a -o /tmp/literature-one-minus
/tmp/literature-one-minus > benchmarks/literature-one-minus/validation-summary.json
```

Optional `--points-per-configuration N` and `--seed N` change the sample
count and reproducible seed. The default uses 1,000 points for each of six
mass/charge configurations, separately at each multiplicity: masses
`0.01,1,7`, charges `0.2,1.3`, and center-of-mass energy between `8m` and
`80m`. All four complex massive-spin components are compared at each point.
Non-finite quantities are errors. A missing-permutation negative control
keeps only the adjacent-minus primitives; it must fail at every sampled
point by more than `1e-6`.

The complex error is the largest absolute component difference at a point
divided by the largest component magnitude of either complete result at that
point. The spin-summed squared error is `abs(S1-S2)/max(S1,S2)`, with no spin
averaging or identical-particle factor. Both are then maximized over points.
The Eq. (32) control uses the same component-tensor normalization. A BCJ
control divides the absolute residual by the sum of the absolute terms in
that relation. The pass tolerance is `1e-8` for each positive control.

## Scope and literature assessment

The recorded test validates the complete Abelianized published one-minus
family at **N=2 and N=3 only**. It does not establish a new numerical
published-formula comparison at N=4 or higher, nor an analytic proof that
the scalar-current representation equals the published expression for all
N. The existing higher-N MadGraph tests are separate evidence.

Existence of all-multiplicity one-minus massive-quark amplitudes with arbitrary
spin projections is already published. Ochirov also explicitly relates Eq.
(42) to earlier results of
Schwinn and Weinzierl, *On-shell recursion relations for all Born QCD
amplitudes*, JHEP **04** (2007) 072,
[hep-ph/0703021](https://arxiv.org/abs/hep-ph/0703021), in Eqs. (57)-(60).
The present subset-current form may be a useful different organization, but
its novelty needs comparison with the earlier second-order QED currents and
worldline kernels. The matching low-point tests are validation evidence, not
evidence that the family of amplitudes was unknown.
