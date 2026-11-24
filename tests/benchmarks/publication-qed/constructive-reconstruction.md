# QED reconstruction from on-shell products

The only inputs to each candidate are complete amplitudes of lower multiplicity, their on-shell internal-state sums, and the channel propagators. A shift in the table lists the photon whose square spinor changes followed by the photon whose angle spinor changes. The six-fermion row sums the nine massive cuts without a shift. All physical channels, including the photon poles omitted from that sum, enter validation.

Each row below passed all intrinsic checks and a separate signed comparison with the Feynman amplitudes. The seed for the 100,000-point comparisons was 20261124. “Maximum raw relative error” is the largest value before precision replay. A successful comparison can include a replay. CalcHEP and MadGraph have separate validations and were not run for these new forms.

| External legs (all incoming) | Shift | Intrinsic checks | Signed comparisons | Maximum raw relative error |
| --- | --- | ---: | ---: | ---: |
| e, E, E, e, A.-, A.- | 5, 6 | 23 | 100000 | 1.69237e-09 |
| e, E, E, e, A.+, A.- | 6, 5 | 23 | 100000 | 2.56389e-09 |
| e, E, E, e, A.+, A.+ | 5, 6 | 23 | 100000 | 2.63122e-08 |
| e, E, e, E, e, E | massive cuts | 21 | 100000 | 2.60346e-07 |
| e, E, A.-, A.-, A.-, A.- | 3, 4 | 17 | 100000 | 8.70564e-09 |
| e, E, A.+, A.-, A.-, A.- | 4, 3 | 17 | 100000 | 1.37341e-09 |
| e, E, A.+, A.+, A.-, A.- | 5, 3 | 17 | 100000 | 8.92795e-07 |
| e, E, A.+, A.+, A.+, A.- | 6, 3 | 17 | 100000 | 1.80418e-09 |
| e, E, A.+, A.+, A.+, A.+ | 3, 4 | 17 | 100000 | 1.71965e-08 |
| e, E, E, e, A.-, A.-, A.- | 5, 6 | 47 | 100000 | 3.45968e-09 |
| e, E, E, e, A.+, A.-, A.- | 6, 5 | 47 | 100000 | 7.13406e-09 |
| e, E, E, e, A.+, A.+, A.- | 7, 5 | 47 | 100000 | 3.84782e-09 |
| e, E, E, e, A.+, A.+, A.+ | 5, 6 | 47 | 100000 | 4.22405e-09 |
| e, E, A.-, A.-, A.-, A.-, A.- | 3, 4 | 33 | 100000 | 1.36613e-08 |
| e, E, A.+, A.-, A.-, A.-, A.- | 4, 3 | 33 | 100000 | 2.66993e-09 |
| e, E, A.+, A.+, A.-, A.-, A.- | 5, 3 | 33 | 100000 | 6.6107e-07 |
| e, E, A.+, A.+, A.+, A.-, A.- | 6, 3 | 33 | 100000 | 4.27922e-07 |
| e, E, A.+, A.+, A.+, A.+, A.- | 7, 3 | 33 | 100000 | 5.99844e-08 |
| e, E, A.+, A.+, A.+, A.+, A.+ | 3, 4 | 33 | 100000 | 3.80698e-09 |
| e, E, A.+, A.+, A.+, A.+, A.+, A.+ | 3, 4 | 65 | 100000 | 2.60928e-08 |

The remaining two seven-point helicities use charged sewings of the complete four-fermion amplitude and the photon residue described below. Each has 63 sewings, 42 physical channels, one spurious-pole check, one reference check and one high-energy check.

| External particles | Signed comparisons | Failures after replay | Maximum raw relative error | 160-bit replays | Maximum replay relative error |
| --- | ---: | ---: | ---: | ---: | ---: |
| e, E, e, E, e, E, A.+ | 100000 | 0 | 6.58799e-8 | 6 | 9.35893e-43 |
| e, E, e, E, e, E, A.- | 100000 | 0 | 1.0542e-7 | 6 | 5.67482e-42 |

## Reproducing a row

From the repository root, build the native driver. For example, the following reconstructs the mixed four-photon amplitude. Choose a fresh destination for the expression.

```sh
cmake -S csource -B tests/results/reconstruction -DCMAKE_BUILD_TYPE=Release
cmake --build tests/results/reconstruction --target hepcat-reconstruct-qed
tests/results/reconstruction/hepcat-reconstruct-qed . tests/results/mixed-six.expr 100000 5 3 1 e E A.+ A.+ A.- A.-
```

For an unshifted massive-cut sum, supply `0 0 1` after the point count. The driver fixes the candidate before it attaches the Feynman model. The signed comparison and the intrinsic checks must both pass before the driver writes an expression. The point count is per process. For the eight-point result the lower inputs use model 4 through six points and the reconstructed model 5 seven-point entries. Prepare the combined working directory with the command below and pass it as the first argument.

The six-point and ten seven-point rows were also written and reread with Mathematica. The factorization catalog uses model 5 for the shifted results, which can contain spurious poles. A separate physical representation requires its own checks. Pending rows keep their process keys and the Git revision of the withdrawn values. Both HEPCAT readers report them as unavailable. An omitted, identically vanishing process still returns zero.

## Reconstructing the sectors with several charged pairs

Use the QED four-fermion amplitude in model 4 as one on-shell building block. Project its photon-pole residue onto an auxiliary massless fermion pair to obtain an emission factor. In the model ordering e, E, E, e, the four bracket products over s12 are multiplied by −2e². If auxiliary leg 1 contributes its square spinor and leg 2 its angle spinor, only <24>[13] and <23>[14] survive. On the branch with proportional square spinors, divide by the auxiliary three-point amplitude. For external charged slots a,b and photon k the surviving factor is

```
−sqrt(2) e ( <r b>[k a] + <r a>[k b] ) / <r k>.
```

The other photon helicity interchanges angle and square brackets. These two terms and their relative sign follow from the on-shell four-fermion residue. Their common sign can be checked on the original five-point charged residues. Both helicities passed all eight five-point physical residues in this convention. The full seven-point candidates sew two four-fermion amplitudes and one emission factor across their massive spin indices. All emissions use the same reference, formed from the momenta of the first two charged legs and the spinor of the last photon. Each resulting rational continuation is checked away from its defining cuts. In particular, simply averaging all massive cuts with weight 1/2 was ruled out by an exact residue counterexample.

Prepare an isolated set of model files. Its lower table prefers the checked physical representation, taking a checked alternate representation only for entries that are pending. The input model for the charged sewings contains the four-fermion amplitude and the two helicity projections of its photon residue. No Feynman inputs enter this step.

```sh
python3 tests/checks/reconstruction-block-inputs.py . tests/results/charged-input
cmake --build tests/results/reconstruction --target hepcat-reconstruct-charged
tests/results/reconstruction/hepcat-reconstruct-charged tests/results/charged-input tests/results/six-fermion-plus.shared 100000 e E e E e E A.+
```

The driver writes the shared expression only after all intrinsic checks and signed comparisons pass, and verifies that the model parser recovers the expression. Higher-point benchmarks obtained from previously withdrawn forms do not validate the constructive method.

## Four charged pairs

The 216 sewings of three four-fermion amplitudes passed all 82 physical residues using the newly reconstructed six-fermion, one-photon rows on the two-particle photon poles. Spurious-pole, reference and high-energy checks passed as well. At 100,000 signed Feynman comparisons, the maximum raw relative discrepancy was 2.82815e-7. All 23 precision replays passed with maximum relative discrepancy 4.79146e-42. Both QED pole-preference catalogs contain this physical-pole expression.

The two printed contributions use 4 and 8 spinor monomials before their stated exchanges. Their signed permutation expansion matches all 110,592 monomials under 1,728 propagator products, using chain reversal and complementary momentum sums. Reproduce this check from the repository root with `python3 tests/benchmarks/publication-qed/check-four-pair-spinors.py models/amps4.mdl`. This is an exact check of the printed algebra against the constructive expression. The comparison with the independent Feynman amplitude is numerical.

## Three charged pairs and three photons

The nine-point candidates sew two complete four-fermion amplitudes and three projections of the photon residue. There are nine ways to place the two four-fermion amplitudes across a charged cut. Inserting the three labeled photons gives 7, 8 and 9 successive choices. This yields 4,536 sewings per helicity assignment. The constructive driver checks the sign of the printed spinor expression independently on every sewing. It groups the terms under electron, positron and equal-helicity photon permutations and writes the literal elementary spinor expression for each representative.

| Photon helicities | Representatives | Signed points per reference prescription | Maximum relative discrepancy with sampled references | Maximum with printed references | 160-bit candidate replays |
| --- | ---: | ---: | ---: | ---: | --- |
| +++ | 45 | 1000 | 2.47605e-8 | 1.76546e-7 | One, giving 1.77336e-12 |
| ++- | 127 | 1000 | 1.67700e-9 | 1.22379e-9 | None |

The same 1,000 draws, starting at seed 20261124, were used for both reference prescriptions. The signed comparison uses an independent numerical Feynman evaluation. In the single replay, the constructive spinor sum uses 160-bit arithmetic and the Feynman reference retains its native precision. These candidates have yet to pass the full set of nine-point residue tests. CalcHEP and MadGraph have not been rerun for these expressions.

From the repository root, prepare the charged inputs as above. The output for a new construction must be a fresh directory.

```sh
cmake --build tests/results/reconstruction --target hepcat-three-pair-photon-sewings hepcat-check-three-pair-photon-sewings
tests/results/reconstruction/hepcat-three-pair-photon-sewings tests/results/charged-input/charged-blocks tests/results/nine-ppm e E e E e E A.+ A.+ A.-
tests/results/reconstruction/hepcat-check-three-pair-photon-sewings models tests/results/nine-ppm 1000 20261124 ++- printed
tests/results/reconstruction/hepcat-check-three-pair-photon-sewings models tests/results/nine-ppm 1000 20261124 ++- sampled
```

For three positive helicities use A.+ on all three photon legs, change ++- to +++, and use another output directory. The four charged cuts label each orbit. The files with the .permutations extension record the signed maps from its representative to each distinct sewing.
