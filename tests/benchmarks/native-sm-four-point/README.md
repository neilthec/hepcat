# Native SM four-point validation references

The C++ runner constructs each amplitude from the elementary SM vertices. The expressions in these tables are signed validation references. The weight is Wolfram HEPCAT's `SpinorExpressionComplexity` evaluated on the complete saved expression, including color factors. The leaf count is Wolfram `LeafCount` of that same expression.

* `catalog.tsv` follows the evaluated `tests/SM-4-point-construction.nb`, SHA-256 `e3acd1c23f126a9de45ff88cdd697021585968f76f8dfaa3a21bba332e026b3c`. Its 138 rows include 78 physical export families. The two incoming and two outgoing species come from the notebook's export calls. The four all-incoming labels preserve the helicities, charge conventions and ordering of each construction.
* `flavor-closure.tsv` adds 254 helicity or crossing assignments in 180 families. The rows apply the simultaneous flavor and mass maps certified on the elementary model in `tests/checks/fixtures/sm-factorization-catalog.json`. Together the tables cover 361 unordered species/helicity multisets in the model with generation-diagonal couplings, including four vanishing gluon sectors. Permuting identical species or using a different incoming crossing can yield multiple ordered rows for one such multiset.
* `ModelParameter` scopes in the reference expressions are normalized to `NOTEBOOK` so the driver can map the names to its loaded model. Massless helicity labels use the HEPCAT all-incoming convention throughout.

To refresh the references from the evaluated notebook, run the following commands in the repository root.

```sh
WolframKernel -script tests/benchmarks/native-sm-four-point/export-notebook.wls
python3 tests/benchmarks/native-sm-four-point/expand-flavors.py
```

The first command reads the saved output cells and derives the two complexity measures. The second checks the union of the resulting flavor multisets against the complete independently recorded model inventory.
