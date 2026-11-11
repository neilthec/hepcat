# Independent all-positive literature validation

This benchmark evaluates a published ordered massive-fermion amplitude and
sums **all** photon permutations. It compares that result with both HEPCAT
all-positive formulas. It does not use the HEPCAT current recurrences to
construct the published answer.

## Primary equations and scope

1. A. Ochirov, *Helicity amplitudes for QCD with massive quarks*,
   JHEP 04 (2018) 089, [arXiv:1802.06730v3](https://arxiv.org/html/1802.06730v3),
   Eq. (35). This is the implemented all-multiplicity ordered formula, with
   both massive little-group indices arbitrary. Its ordered numerator is
   `i*m*<1^a 2^b>*[3| product_j {slash(p_13...j)*slash(p_j+1)
   + (s_13...j-m^2) I} |n]`, with `j=3,...,n-2`.
   The denominator is the product of the consecutive massive propagators
   `(s_13-m^2)...(s_13...(n-1)-m^2)` and consecutive angle brackets
   `<34><45>...<n-1,n>`. The product acts from left to right as printed.
2. P. Ferrario, G. Rodrigo and P. Talavera,
   *Compact multigluonic scattering amplitudes with heavy scalars and fermions*,
   Phys. Rev. Lett. 96 (2006) 182001,
   [hep-th/0602043](https://arxiv.org/html/hep-th/0602043).
   Eq. (23) is the compact scalar ordered product; Eq. (24) gives its relation
   to a specified massive-fermion spin choice. The products are related by the
   ordinary anticommutator identity given explicitly in Ochirov Eq. (59).
   Ferrario et al. cite G. Rodrigo, JHEP 09 (2005) 079,
   hep-ph/0508138, as prior all-positive heavy-fermion work, and
   Schwinn and Weinzierl hep-th/0602012 for the SUSY-like relation.
3. S. D. Badger, E. W. N. Glover and V. V. Khoze,
   *Recursion Relations for Gauge Theory Amplitudes with Massive Vector Bosons
   and Fermions*, JHEP 01 (2006) 066,
   [hep-th/0507161](https://arxiv.org/pdf/hep-th/0507161).
   Section 6.1, Eqs. (6.5)-(6.11), checks the massive pair plus **two** gluons
   using recursion and Feynman diagrams. This is relevant historical context,
   not the implemented arbitrary-photon formula.
4. R. Das and M. Zeng,
   *Five-point spinor-helicity Compton amplitudes in gauge theory and gravity*,
   [arXiv:2608.18218v1](https://arxiv.org/html/2608.18218v1),
   PDF Eq. (4.10), HTML global Eq. (77), specialized to spin 1/2.
   A second independent N=3 transcription uses their complete six-term QED
   sum `2*sqrt(2)*i*e^3*m*<12>*sum [a|p1*q|c]/
   (tau_1a*<ab>*<bc>*tau_2c)`. Their massive legs 1 and 5 are relabeled 1 and 2;
   their photons 2,3,4 become 3,4,5. Their PDF Eq. (3.44) is a single
   color-ordered amplitude, not the full QED amplitude.

The amplitudes as a family are therefore prior art. Whether a particular
HEPCAT collected or subset representation is a useful new reorganization
requires a separate comparison. These numerical matches do not prove an
all-multiplicity identity between the representations.

## Convention and coupling dictionary

* The input vectors are positive-energy physical momenta for
  `e- e+ -> N gamma`. The analytic momenta are all incoming:
  `q1=p1`, `q2=p2`, `q_photon=-k_photon`.
* Crossing an outgoing photon reverses its momentum and angle spinor and
  leaves its square spinor unchanged. Each angle denominator bracket has two
  crossed photon spinors, so its sign is unchanged. Momentum insertions
  receive the signs of their analytic momenta.
* Use `P_j=q1+q_r1+...+q_rj` without assuming `P_j^2=m^2`.
  The propagator invariant is expanded using **external** on-shell conditions
  only. Composite momenta are 2 by 2 matrices, never `particle` objects.
* Ochirov's normalized generators satisfy `Tr(T^a*T^b)=delta^ab`.
  His Eq. (24) has the corresponding `1/sqrt(2)` per cubic vertex.
  Abelianization restores `(sqrt(2)*e)^N`, sums all `N!` permutations,
  and removes the one overall `i`. There is no fitted coefficient at any N.
  At N=2 the result is the explicit control
  `2*e^2*m*<12>*[34]^2/(tau_13*tau_14)`.
* The complete complex four-component massive spin tensor is compared in the
  common SPINAS spin basis. Pointwise errors use the maximum component to
  normalize the tensor, avoiding division by a vanishing spin component.
  Raw spin-summed squared amplitudes are also compared, with no initial
  average or phase-space symmetry factor.

## Exact five-point bridge

For a photon ordering `(a,b,c)`, Ochirov's Eq. (35) numerator reduces as

```
[a|{slash(p1+pa)*slash(pb)+tau_1a}|c]
  = [a|slash(p1)*slash(pa+pb)|c]
  = [a|slash(p1)*slash(q)|c],  q=pa+pb+pc.
```

The first equality uses `[a|slash(pa)=0` and the Clifford anticommutator;
the second uses `slash(pc)|c]=0`. Momentum conservation gives
`tau_1ab=tau_2c`. Thus the N=3 published ordered product is exactly the
ordered summand in Das and Zeng's PDF Eq. (4.10), after the stated convention
dictionary. No internal on-shell condition is used.

## Reproducing the comparison

From the HEPCAT repository root, replacing the SPINAS path as appropriate:

```sh
clang++ -O2 -std=c++11 -DWITH_LONG_DOUBLE \
  -I../spinas/include -I../spinas -Isource/internal/export -Ibenchmarks \
  tests/benchmarks/literature-all-positive-validation.cpp \
  ../spinas/build/libspinas.a -o /private/tmp/literature-all-positive
/private/tmp/literature-all-positive 1000 /private/tmp/literature-all-positive
```

The library and executable must use the same SPINAS precision setting.
The driver writes one CSV and one JSON report per N. The fixed seed is
`180206730+N`. Every N uses 1,000 phase-space points, cycling through masses
`{0.1,1,1.5,3}` and charges `{1/3,2/7,-1/4,0.7}`, with center-of-mass energy
between 8 and 80 times the mass. N ranges from 2 through 6. All four massive
spin components are checked at each point, giving 20,000 complex comparisons
against each HEPCAT representation. The tolerance is `1e-7`.

Two first-point negative controls must be rejected at every N: omitting all
but the first ordering, and omitting the `sqrt(2)^N` coupling restoration.
Their errors are included in the JSON reports.

The archived `native-N*.json` reports came from Apple arm64 with **53-bit**
mantissa, including when `long double` is requested. Maximum complex errors
against Physical and Spurious, respectively, were:

| Photons | Physical | Spurious |
| --- | --- | --- |
| 2 | 1.61e-15 | 5.68e-16 |
| 3 | 2.55e-13 | 2.57e-13 |
| 4 | 9.15e-13 | 9.20e-13 |
| 5 | 9.02e-12 | 9.09e-12 |
| 6 | 2.87e-11 | 2.87e-11 |

There were no failures. The independent Das-Zeng N=3 form agreed with the
Ochirov sum to 2.57e-13. The largest cancellation condition
`sum(abs(ordered term))/abs(sum(ordered term))` was 1.55e5 at N=6.
This is a numerical validation at the stated sampled kinematics, not a proof
at arbitrary N or at singular limits. The factorial permutation sum is a
validation implementation, not a recommended high-multiplicity evaluator.
