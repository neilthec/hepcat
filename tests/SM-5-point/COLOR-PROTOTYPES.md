# Exact color performance prototypes

This is the historical C3 prototype record. The reviewed C4 implementation now
lives in `source/internal/HEPCAT-Color.wl` and passed 360 production checks. The
measurements below remain records of the isolated prototype runs; their original
evidence is preserved. The proposed production contract below was subsequently
implemented without changing the scalar amplitude contributions.

These opt-in files do not participate in HEPCAT construction or replace package
definitions. They examine two independent sources of repeated work:

- `color-tensor-dedup-prototype.wl` shares literal `SameQ` tensors and restores
  every original occurrence's coordinates and basis position.
- `color-tensor-network-prototype.wl` evaluates admitted primitive tensor
  networks by sparse finite-sum contraction. Unsupported incidence patterns use
  the unchanged package evaluator.

The portable checks are `tests/checks/color-tensor-dedup-prototype-tests.wl`
and `tests/checks/color-tensor-network-prototype-tests.wl`. They passed 23 and
42 exact checks respectively. They are not registered in the default suite.

## Why the transformations are exact

For an original tensor list `T`, let `U` contain its first-occurrence unique
tensors and let the real incidence matrix `S` select each original occurrence.
Then `T = S.U`, `G[T] = S.G[U].Transpose[S]`, and the original coordinate rows
are the corresponding rows of the unique-coordinate matrix. The first
independent columns retain their original occurrence positions. Scalar spinor
coefficients are never shared, averaged, removed or fitted. The projection
continues to sum every original coefficient times its restored coordinate.

For the sparse evaluator, every admitted internal index has exactly two ports
on different primitive factors, and every external index appears once.
Primitive arrays are the existing exact delta, generator and structure-constant
components. Reordering axes and reshaping to matrix products changes only the
order of finite sums and products. Connected factors contract shared internal
axes; disconnected factors form an outer product. The final axes are restored
to the external-color order used by the original evaluator. The generator axis
order is tested componentwise, including complex generators.

Trace expansion retains the package's independent dummy sums. A trace power
or repeated external port that violates the admitted incidence grammar falls
back to the original evaluator. No approximate coefficient, unsupported
dimension or failed original tensor is promoted to an exact certificate.

The Gram is computed as the exact sparse row matrix's conjugate dot its
transpose. Its independent block determines coordinates. The full unique Gram
must equal the reconstructed Gram exactly; the incidence map separately
reconstructs every original tensor. Neither certificate is a sampled test.

## Bounded measurements

The audit directory is
`tests/results/SM-5-point/diagnostics/five-gluon-stage-timing/`.
Its `sparse-network-prototype/review-summary.json` links the actual results.
The saved five-gluon catalog contains 3,960 occurrences and 45 unique tensors.
Literal sharing with the old vector evaluator still timed out at 180 seconds,
after five unique evaluations and before Gram formation.

The sparse prototype matched all 32,768 components of one fresh five-gluon
legacy vector exactly: 28.234342 seconds versus 0.002748 seconds. All 45 unique
vectors, the exact sparse Gram and its rank-six reconstruction took 0.978390
seconds, including diagnostic progress reporting. The original coordinate
array has dimensions 3,960 by 6. No repeated full Gram was materialized.

This is a color-algebra result. No full five-gluon amplitude construction,
Feynman amplitude comparison, CalcHEP comparison or MadGraph comparison was
performed by these prototypes. Production source and canonical results remain
unchanged.

## Proposed production integration, not yet implemented

1. Retain the old evaluator as an exact private fallback. Keep the existing
   `colorTensorVector` list-valued contract; an internal sparse provider can
   avoid normalizing sparse vectors before Gram multiplication.
2. Deduplicate at `colorTensorBasis`, preserving first-occurrence ordering.
   Compute the unique sparse Gram, reuse one exact linear solver and restore
   original tensor, coordinate and pivot-position fields.
3. Preserve the full `DiagramGramMatrix` default for existing private callers,
   including SPINAS. Only `colorProjectionData` should request compact
   diagnostics, explicitly marking the repeated Gram unmaterialized and
   retaining the unique Gram and occurrence map.
4. Preserve representation and state-count guards, every benchmark-only tensor,
   exact complex interference, and all original scalar contributions. Keep
   independent model scopes and typed dummy dimensions in the exact inputs.

Before a public construction pilot, run `color-basis-tests.wl`,
`color-reduction-tests.wl`, `color-display-tests.wl`,
`cubic-current-source-tests.wl`, `spinas-color-tests.wl`,
`spinas-analytic-closure-tests.wl`, `spinas-helicity-family-tests.wl` and
`spinas-polarization-tests.wl`. Then run the opt-in
`spinas-color-integration-tests.wl` and an actual bounded five-gluon construction
with its unchanged independent checks. A color-only speedup does not itself
establish that this final construction will complete.

Exact legacy invariants include `Tr[T_a.T_b] = delta_ab/2`, fundamental singlet
norm 3, the four-quark singlet Gram `{{9,3},{3,9}}`, four-adjoint Jacobi rank 2
and norms 72, and the complex relative-phase Gram `{{9,9I},{-9I,9}}`.
The existing tests also require independent benchmark-only directions to remain
visible and fail when the candidate omits them.
