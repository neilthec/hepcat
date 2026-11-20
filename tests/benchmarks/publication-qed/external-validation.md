# Native QED rate comparisons

`external-rates.tsv` compares the native C++ constructions with CalcHEP 3.9.2 and MadGraph5_aMC@NLO 3.8.1 using the publication catalog settings. The reference columns come from the publication branch at commit `aa676a9c198f9b50a682adc2bc98be590b4adb0e`, archive `tests/benchmarks/benchmarks.zip`, entry `tests/benchmarks/reproduction/100k/catalog/results.json`.

Each family and pole representation uses 100,000 physical phase-space points, seed 861811, and center-of-mass energies sampled uniformly in the logarithm between 1.5 and 6 GeV. The electron mass is 0.0005 GeV, its charge magnitude is 0.31333, and all widths vanish. All comparisons use a relative tolerance of 1e-7 and an absolute tolerance of zero. An outlier has an absolute difference divided by the larger absolute weight above this threshold. The sum over labeled comparisons counts repeated evaluations of the same momenta. The Mac native arithmetic has 53 significand bits.

CalcHEP evaluates the full polarization sum. MadGraph also tests each labeled photon-helicity assignment with the massive spins summed. The native polarization sum and the external sum include the initial spin average and identical final particle factorials. The separate seven-point entry `eeAAAAA` fixes all five photon helicities. Its MadGraph sum is restricted to that assignment. The published 14-row complete catalog contains the seven families through six points.

`external-helicities.tsv` gives separate counts and maxima by helicity. The full 14-row catalog had zero failures in 1.4 million complete CalcHEP comparisons, 7.2 million labeled MadGraph comparisons and 1.4 million MadGraph polarization sums. The two extra seven-point rows passed at 100,000 points each for their single selected photon-helicity configuration.

For the four-photon spurious form the original publication recorded 50 fixed-helicity comparisons over tolerance, with a maximum relative difference of 1.318e-5. The native squared-amplitude evaluator can relabel identical photons to improve numerical conditioning. Its fresh comparison with MadGraph on that sample passed all 1.6 million labeled comparisons, with a maximum relative difference of 1.585e-9. Individual rounding-level maxima can change for other amplitudes.

`external-precision.tsv` repeats the comparison against the publication’s archived binary64 momenta and MadGraph weights, verified against their SHA-256 digests. The 53-bit sample covers the complete four-photon grid. The 64-bit-significand SPINAS build repeats the 50 original flagged helicity-events on their original momenta. An additional comparison between pole forms restores the external mass shells and momentum conservation at extended precision. Point indices start at one and helicity indices at zero in lexicographic order from `----` to `++++`.

To rerun the external comparisons, use `hepcat-qed --points 100000 --seed 861811` with the HEPCAT, SPINAS, CalcHEP and MadGraph paths described in `csource/README.md`. The external rows use `sm-full` restricted to the electron and photon for MadGraph and the bundled massive QED reference model for CalcHEP.

## Replay the Mathematica four-photon export

Run the four-photon `AllSpurious` family export in the QED notebook. Set `PROJECT` to its generated `eeAAAA` source directory, `SPINAS` to a matching long-double SPINAS checkout, and `CAPTURE` to the directory containing the publication's `binary64-momenta.dat` and `binary64-reference.dat`. If you need to generate these two files, unpack the publication branch's `tests/benchmarks/benchmarks.zip` and follow `tests/benchmarks/reproduction/100k/catalog/README.md` inside it. Its `replay.py --output "$OUT" --capture-four-photon --families eeAAAA --forms spurious --checks madgraph` writes them in `$OUT/spurious/eeAAAA`. The original streams' hashes are in [wolfram-external.json](wolfram-external.json).

From the HEPCAT repository root, compile the replay against the generated source and run it on those files.

```sh
c++ -std=c++17 -O2 -ffp-contract=off -DWITH_LONG_DOUBLE \
  -I"$PROJECT" -I"$SPINAS/include" \
  tests/benchmarks/publication-qed/replay-wolfram.cpp "$PROJECT/process.cpp" \
  "$SPINAS/build/libspinas.a" -o /tmp/replay-wolfram
/tmp/replay-wolfram "$CAPTURE/binary64-momenta.dat" \
  "$CAPTURE/binary64-reference.dat" > wolfram-rates.csv
```

The replay streams the saved momenta and each of the 16 MadGraph helicity streams. Its CSV reports each fixed helicity, their explicit sum and the generated `amp2()`, with a nonzero exit status if any relative difference exceeds `1e-7`. An optional final argument limits the run to the first specified number of points.
