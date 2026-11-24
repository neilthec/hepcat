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

## Reproducing a row

From the repository root, build the native driver. For example, the following reconstructs the mixed four-photon amplitude. Choose a fresh destination for the expression.

```sh
cmake -S csource -B tests/results/reconstruction -DCMAKE_BUILD_TYPE=Release
cmake --build tests/results/reconstruction --target hepcat-reconstruct-qed
tests/results/reconstruction/hepcat-reconstruct-qed . tests/results/mixed-six.expr 100000 5 3 1 e E A.+ A.+ A.- A.-
```

For an unshifted massive-cut sum, supply `0 0 1` after the point count. The driver fixes the candidate before it attaches the Feynman model. The signed comparison and the intrinsic checks must both pass before the driver writes an expression. The point count is per process. For the eight-point result the lower inputs use model 4 through six points and the newly reconstructed model 5 seven-point entries. When repeating it from a clean checkout, supply these inputs in the same working model under number 4.

The six-point and ten seven-point rows were also written and reread with Mathematica. The factorization catalog uses model 5 for the shifted results, which can contain spurious poles. A separate physical representation requires its own checks. Pending rows keep their process keys and the Git revision of the withdrawn values. Both HEPCAT readers report them as unavailable. An omitted, identically vanishing process still returns zero.

The two seven-point six-fermion, one-photon rows remain open. Neither a unit weight nor the naive inverse count of massive propagators establishes the correct continuation. The half-weight positive-helicity proposal fails physical residues and the high-energy condition. Higher-point benchmarks using withdrawn representations cannot establish either their derivation or the speed of a constructive solution.
