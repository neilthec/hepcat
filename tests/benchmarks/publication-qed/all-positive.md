# Same-helicity QED formulas

These checks evaluate the two arbitrary-multiplicity expressions directly for two massive fermions and two through eight equal-helicity photons. The direct formula evaluator, the independent signed low-point controls and MadGraph test different parts of the calculation. The constructive catalog has its own tests.

The physical formula originally assigned a weight to each terminal pair in a photon subset. One can sum those weighted pairs before applying the recursion. Every possible first single removal multiplies the correction of the subset with that leg removed. Every possible first double removal multiplies the correction of the subset with that pair removed. The leading determinant term closes the recurrence. The collected version therefore caches one correction per subset. The driver evaluates the original and collected complex amplitudes for each of the four massive spin pairs and compares each spin-summed rate with MadGraph. It also compares the ordered and compact spurious-current recursions.

The [numeric receipt](all-positive-results.json) records the samples, seeds, tolerances, reference provenance and wall times for the publication comparison. In the general study, each photon multiplicity has 100,000 points with energies from 10 to 100 GeV. The charge is 1/3 and the mass is 1 GeV through seven photons and 1.5 GeV at eight. All 700,000 collected physical rates passed at native precision. The maximum relative difference between the original and collected complex physical amplitudes was 2.40 × 10⁻¹³. At eight photons 24 points triggered the raw spurious or signed-identity checks. All 24 passed on the original inputs at 160 bits, with a maximum spurious/MadGraph relative discrepancy of 6.14 × 10⁻⁸ against the 10⁻⁷ tolerance.

The model study uses a 0.0005 GeV mass, a charge of 0.31333 and energies from 1.5 to 6 GeV. Each representation has 100,000 points per multiplicity except at seven photons, where both have one million. The physical representation uses virtualities computed from the summed four-momentum components, as in the published SPINAS export. The spurious formula uses the native evaluator's expansion of those virtualities in external scalar products. Both prescriptions are identical on the exact external mass shells. They can have different roundoff on the recorded binary64 momenta. With these choices, every physical rate passed at native precision. The spurious formula had 12 raw rate outliers at eight photons, compared with 16 in the publication. All twelve passed against the original MadGraph rates at 160 bits. The worst relative discrepancy was 5.56 × 10⁻⁸.

An additional run used the publication's direct component-sum prescription for the spurious virtualities as well. It had 17 raw rate outliers. On unchanged inputs the maximum spurious/MadGraph discrepancy at 160 bits was 1.58 × 10⁻⁷, and one rate remained above tolerance. A second event exceeded the tolerance for a signed identity. The separate test on momenta projected onto the exact shell and conservation constraints compared only the formulas with one another, with a maximum signed discrepancy of 7.02 × 10⁻⁵⁰. The receipt preserves these results separately.

## Reproduce the comparison

Build the two native drivers from the HEPCAT checkout. Set `SPINAS` to the SPINAS checkout.

```sh
cmake -S csource -B build/native -DCMAKE_BUILD_TYPE=Release
cmake --build build/native --target hepcat-qed
cmake -S tests/benchmarks/all-positive -B build/formulas -DSPINAS_ROOT="$SPINAS" -DCMAKE_BUILD_TYPE=Release
cmake --build build/formulas --target qed-all-positive
```

The ordinary end-to-end run `build/native/hepcat-qed --formulas-only --photons 8 --points 100000 --madgraph "$MG" --spinas "$SPINAS"` generates its own MadGraph reference. It saves raw outlier rates, coordinates and a precision report. The full-run benchmark below accepts existing MadGraph standalone libraries initialized at the two parameter settings and uses the seeds and counts in the publication archive. The symbol `{n}` in each library path is replaced with the number of photons.

```sh
python3 tests/benchmarks/publication-qed/compare-all-positive.py \
  --root . --validator build/formulas/qed-all-positive --hepcat-qed build/native/hepcat-qed \
  --publication-archive "$PUB/tests/benchmarks/benchmarks.zip" \
  --general-library "$REFS/general-N{n}/ref.dylib" \
  --model-library "$REFS/model-N{n}/ref.dylib" --output tests/results/publication-formula-rerun
```

Every reference rate is evaluated by MadGraph unless `--archive-rates DIRECTORY` supplies the original CSV streams. In that mode the driver verifies the published model-stream SHA-256 checksums, regenerates every event from the recorded seed, and compares the archived rates with live MadGraph at the first, last and every 10,000th point, and every raw outlier. The receipt identifies how many points were checked live. The general checks at two, three and four photons in this audit used fresh MadGraph evaluations for every point. To reproduce the additional eight-photon numerical diagnostic, select `--campaign model --photons 8 --model-summed-spurious`. The script returns a failure if the raw outlier count exceeds the publication's or a replay exceeds tolerance on the original input.
