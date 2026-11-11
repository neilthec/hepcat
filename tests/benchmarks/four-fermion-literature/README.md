# Published four-fermion amplitude controls

Run from any directory on this Mac:

```sh
/Applications/Wolfram.app/Contents/MacOS/WolframKernel -script \
  /Users/neil/code/hepcat/tests/benchmarks/four-fermion-literature-validation.wl
```

On another installation, use its Wolfram kernel and the corresponding repository path. No diagram construction or lengthy completion is run. The script writes `tests/benchmarks/four-fermion-literature/validation-summary.json` and exits unsuccessfully if any comparison fails.

## Independent oracle

The oracle is transcribed from the original PDF of Lazopoulos, Ochirov and Shi, [arXiv:2111.06847v2](https://arxiv.org/pdf/2111.06847v2): the four-quark seed (B.3), and the five-point primitive (3.8). Equation (3.9) of this paper defines auxiliary spinors and is **not** the five-point amplitude. The HTML rendering renumbers equations, so use the PDF numbers.

The full one-photon QED amplitude is obtained using Johansson and Ochirov, [arXiv:1507.00332v3](https://arxiv.org/pdf/1507.00332v3), Eq. (3.14):

`A_full = c2 A(1,5,2,3,4) + c3 A(1,2,3,5,4) + c5 A(1,2,3,4,5)`.

For QED, set `c2 = c3 = 1`, `c5 = 0`, then use cyclicity `A(1,5,2,3,4) = A(3,4,1,5,2)`. Thus two relabeled instances of the simple published primitive supply radiation from both fermion lines. Finally, subtract the exchanged identical-antifermion pairing. No HEPCAT model amplitude, candidate current, diagram amplitude, or fitted kinematic coefficient enters the oracle.

## Convention dictionary

- Paper labels `(1_q,2_qbar,3_Q,4_Qbar)` map to HEPCAT `(1,2,4,3)` for the direct pairing.
- The published formulas are analytic functions of conserving all-outgoing momenta. Both expressions are evaluated on the same conserving analytic chart. No internal momentum is put on shell.
- The massive square-spinor sign in the paper's rest-frame convention, Eq. (A.12), is opposite to the chart used here. Each massive square endpoint acquires `-1`; massless square endpoints do not. The script applies this conversion to each chain. In Eq. (3.8), the only net change is the sign of its explicit mass term.
- The paper's stripped quark-gluon vertex has `1/sqrt(2)`. Restore the QED coupling by multiplying by `(sqrt(2) e)^(N+2)`.
- A single fixed overall `-i` converts the paper's amplitude convention to the HEPCAT invariant-amplitude convention. This convention is fixed at the four-point seed and then used unchanged at one photon.
- Identical-antifermion exchange is subtracted. This familiar Fermi-statistics prescription is not a new result.

## What passed

For each of `N = 0,1`, compare all 16 coherent massive-spin components at three exact mass/charge pairs and three exact SL(2,C) charts, including a chart with complex matrix entries. This gives **288 exact amplitude comparisons**, plus **288 exact external-mass-shell and momentum-conservation checks**. Every amplitude difference reduces exactly to zero. The mass/charge pairs are `(1,1/3)`, `(2/5,2/7)`, and `(7/3,-1/4)`.

These are **18 finite configurations in total**, nine per multiplicity, based on two rational seed kinematics and their mass-scaled Lorentz transforms. They are not 288 independent phase-space points, a generic symbolic identity proof, an arbitrary-multiplicity proof, or a test with unequal masses on the two fermion lines. Keeping all 16 spin components checks the coherent tensor and relative signs rather than only its squared spin sum.

Two expected-failure controls detect an omitted radiating line and an omitted massive-square-spinor conversion. These demonstrate why summing the complete Abelian amplitude and matching conventions are necessary.

## Boundary of this comparison

At `N >= 2`, simply summing permutations of the single ordered family (3.10) is not the complete QED amplitude. The other family (3.33) and placements with gluons on both sides of a quark must be accounted for through the full color decomposition. Section 3.3 of arXiv:2111.06847 explicitly supplies a recursion for the mixed placements instead of a closed expression. This test makes no full published-amplitude validation claim for `N >= 2`; the separate MadGraph comparisons cover higher photon multiplicities.

The second-order scalar/Pauli decomposition, its induced two-photon seagull, and application to several external fermion lines are already established by Morgan, [hep-ph/9502230](https://arxiv.org/abs/hep-ph/9502230), especially Eqs. (13), (24), and (26). Chalmers and Siegel, [hep-ph/9708251](https://arxiv.org/abs/hep-ph/9708251), supply related chiral rules. The symmetric cached subset-current representation in this project should be described as a candidate useful reorganization of established QED, without asserting priority until an equation-level all-multiplicity comparison is completed.
