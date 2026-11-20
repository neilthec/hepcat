# Native SM four-point validation references

The C++ runner constructs each amplitude from the elementary SM vertices. The expressions in these tables are signed validation references. The weight is Wolfram HEPCAT's `SpinorExpressionComplexity` evaluated on the complete saved expression, including color factors. The leaf count is Wolfram `LeafCount` of that same expression.

## Results

The [per-amplitude validation results](validation.tsv) cover all 392 ordered amplitudes in 258 process families for the notebook's generation-diagonal SM. Each row passed the built-in checks, signed comparisons with the notebook and the independent Feynman diagrams, and external comparisons with CalcHEP and MadGraph. Each numerical comparison used 10,000 phase-space points with seed 41711. CalcHEP compared the polarization sum. MadGraph compared each distinct massless helicity assignment and the sum. The externally tested expressions matched the final saved expressions byte for byte.

Mathematica 15 measured both the notebook formulas and the C++ formulas.

| Measure | C++ smaller | Equal | C++ larger |
| --- | ---: | ---: | ---: |
| `SpinorExpressionComplexity` | 124 | 268 | 0 |
| `LeafCount` | 345 | 47 | 0 |

Fourteen CalcHEP points across 13 families needed higher precision. Four of those points lay near radiation zeros and needed both programs to evaluate the event on the same binary128 mass shell. The largest momentum-component adjustment was below 1.65 × 10⁻¹⁶ of the center-of-mass energy, and the largest resulting relative difference was below 5.56 × 10⁻¹⁷. The results table identifies each replay and retains the four-gluon MadGraph residuals for the exactly vanishing helicities.

## Inventory

* `catalog.tsv` follows the evaluated `tests/SM-4-point-construction.nb`, SHA-256 `e3acd1c23f126a9de45ff88cdd697021585968f76f8dfaa3a21bba332e026b3c`. Its 138 rows include 78 physical export families. The two incoming and two outgoing species come from the notebook's export calls. The four all-incoming labels preserve the helicities, charge conventions and ordering of each construction.
* `flavor-closure.tsv` adds 254 helicity or crossing assignments in 180 families. The rows apply the simultaneous flavor and mass maps certified on the elementary model in `tests/checks/fixtures/sm-factorization-catalog.json`. Together the tables cover 361 unordered species/helicity multisets in the model with generation-diagonal couplings, including four vanishing gluon sectors. Permuting identical species or using a different incoming crossing can yield multiple ordered rows for one such multiset.
* `ModelParameter` scopes in the reference expressions are normalized to `NOTEBOOK` so the driver can map the names to its loaded model. Massless helicity labels use the HEPCAT all-incoming convention throughout.

To refresh the references from the evaluated notebook, run the following commands in the repository root.

```sh
WolframKernel -script tests/benchmarks/native-sm-four-point/export-notebook.wls
python3 tests/benchmarks/native-sm-four-point/expand-flavors.py
```

The first command reads the saved output cells and derives the two complexity measures. The second checks the union of the resulting flavor multisets against the complete independently recorded model inventory.

## Audit a completed campaign

Supply the final native run and the parent directory of the external validation runs. Repeat `--external-root` if the backend receipts reside in multiple locations.

```sh
python3 tests/benchmarks/native-sm-four-point/audit-results.py \
  --native-run tests/results/spinas-output/native-sm-four-point/run-FINAL \
  --external-root tests/results/spinas-output/native-sm-four-point \
  --receipt sm-four-point-validation.tsv --missing-families rerun.txt
```

A passing row requires both the notebook weight ceiling and its leaf-count ceiling, signed agreement with the notebook and the independent Feynman diagrams, the remaining built-in checks, and matching CalcHEP and MadGraph receipts for every exported helicity in its family. The checker translates the catalog's all-incoming labels to physical helicities using model 1, including chiral neutrinos. MadGraph must compare each distinct massless helicity at fixed values, sum the massive spins, and also compare the complete polarization sum. The signed and external numerical comparisons must complete at least 10,000 points. The TSV records how many CalcHEP points required 113-bit replay and whether MadGraph replayed the complete helicity and polarization comparison. The `calchep_quad_points` field gives the number of replayed points. Their indices remain in each original outlier capture. If unprojected binary128 CalcHEP still disagrees, a two-body replay can evaluate both SPINAS and CalcHEP in binary128 on the same refined on-shell momenta. The auditor checks every captured index and the SHA-256 of the momenta submitted to both backends. It recomputes momentum conservation, each external mass shell, the maximum input adjustment (limited to 16 binary64 epsilons in units of center-of-mass energy), and the relative difference between the two squared amplitudes. The `calchep_chart_*` columns record the number of refined points, the largest adjustment and the final maximum relative difference. `calchep_unprojected_quad_max_rel` preserves the discrepancy before refinement. `calchep_spinas_refinement_max_rel` records how much the SPINAS result moved from the original point to the refined one, which must also satisfy the original tolerance. For exact-zero massless two-body sectors, `mg_exact_zero_chart_delta` records the maximum component change in units of beam energy when the original generated input is projected onto the mass shell in binary128. The three `mg_zero_*_max_abs` columns preserve the raw, unprojected binary128 and final maximum absolute discrepancies for these helicities. The audit accepts this replay only with the original failed binary64 and binary128 comparisons, a component change within 16 binary64 epsilons, and identical unprojected rows for every nonzero helicity and polarization sum.

The receipt includes SHA-256 fingerprints of the native expression, the externally tested expression, and their common normalized expression. For `commutative-order` matches, the audit sorts the children of `Plus` and `Times` recursively. Every other operator, argument position, coefficient sign, spelling and multiplicity must agree. An exchange of endpoints in `SpinorChain` therefore fails the comparison. A discrepancy in any family member requires a fresh external run for the whole family.

The audit exits with status 1 if a family needs new evidence and writes its name to the rerun list. Use `--family NAME` for a focused comparison or `--notebook-only` for the original 138 constructions. The evidence checks can be exercised with `python3 -B tests/benchmarks/native-sm-four-point/test-audit-results.py`.

### Cross-check the final scores in Mathematica

The ordinary C++ run measures the native expression directly. To reproduce the additional columns in the recorded campaign, cross-score its output in a Wolfram kernel, then supply that file to the audit.

```sh
WolframKernel -script tests/benchmarks/native-sm-four-point/cross-score.wls \
  tests/results/spinas-output/native-sm-four-point/run-FINAL wolfram-scores.tsv
```

Add `--wolfram-scores wolfram-scores.tsv` to the audit command above. The script imports each native expression with the HEPCAT symbols, lets Mathematica evaluate it normally, and measures `SpinorExpressionComplexity` and `LeafCount`. It separately imports and scores the corresponding reference, including color factors, and checks the recorded notebook values. The auditor checks the SHA-256 of the scored native expression. A Wolfram kernel is optional for constructing amplitudes and running the physics validations in C++.

`weight` and `leaves` describe the C++ expression tree. `wolfram_weight` and `wolfram_leaves` describe the expression after normal Mathematica evaluation. These representations can have different leaf counts, for example when Mathematica distributes an integer power across product factors. Both sets of measurements must meet the notebook ceilings when cross-scoring is enabled. `wolfram_version` identifies the kernel that made the measurements.
