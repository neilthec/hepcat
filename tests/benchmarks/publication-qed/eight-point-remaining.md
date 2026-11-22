# The remaining eight-point QED amplitudes

The [paper equations](eight-point-remaining.tex) cover the balanced six-photon sector and the remaining charged families. External momenta are all incoming. Electron and positron permutations are alternating, and each photon helicity has its own symmetric permutation group. Reversing all helicities gives the parity partners. Eight fermions admit the same representation in both pole preferences.

| Electron pairs | Representative photon helicities | Feynman diagrams per helicity | Terms under the permutations in the paper |
| ---: | --- | ---: | --- |
| 1 | `+++---` | 720 | 4 physical, 2 spurious |
| 2 | `++++`, `+++-`, `++--` | 1,680 | 3, 4, 5 |
| 3 | `++`, `+-` | 2,016 | 4, 5 |
| 4 | none | 1,728 | 2 |

These are counts of separately specified kernels. Each kernel includes the open currents or ordered products defined in the paper. They are a different size measure from HEPCAT's weighted complexity of a scalar spinor expression. A proof that these representations minimize the complexity over all identities is open.

## Reproduce the constructions and independent checks

From the repository root, build the C++ examples and choose the family and helicity to construct. The driver uses vertices from model 3 and the lower amplitudes from models 4 and 5. For example,

```sh
cmake -S csource -B tests/results/native-reduce-build -DCMAKE_BUILD_TYPE=Release
cmake --build tests/results/native-reduce-build --target hepcat-qed --parallel 1
tests/results/native-reduce-build/hepcat-qed --family eeeeAAAA --helicity '++--' \
    --construct-only --skip-formulas --workers 1 --points 100000 --seed 840300
```

For the eight-fermion family use `--family eeeeeeee` and omit the helicity option. Each completed run saves the elementary amplitude, the displayed formula, its complexity and its validation report in the directory printed by the driver.

The two replay programs below build the manuscript expressions directly. Both use the independent Feynman model for signed amplitudes and the MadGraph standalone matrix element for rates. On macOS with Homebrew GMP and a sibling SPINAS checkout, compile them with

```sh
for name in eight-point-two-negative eight-point-charged-trees; do
    c++ -std=c++17 -O2 -Icsource -Isource/internal/export -I../spinas/include \
        -I/opt/homebrew/include "tests/benchmarks/publication-qed/${name}.cpp" \
        tests/results/native-reduce-build/libhepcat_reduce.a -L/opt/homebrew/lib \
        -lgmpxx -lgmp -o "tests/results/${name}"
done
tests/results/eight-point-two-negative --negatives 3 100000 836191
tests/results/eight-point-two-negative --negatives 3 100000 80000000 "$MG_SIX_PHOTONS"
tests/results/eight-point-charged-trees 3 1 100000 105000000
tests/results/eight-point-charged-trees 3 1 100000 105200000 "$MG_SIX_FERMIONS"
```

The first two arguments of the charged-tree replay are the number of massive pairs and the number of negative photons. Its Feynman sample uses the model electron mass 0.0005 and compares complex signed amplitudes for coherent external spin states. The MadGraph sample uses mass 1, charge 1/3, zero widths, the selected photon helicities and an exhaustive sum over the massive spins. The two comparisons use disjoint ranges of phase-space seeds. Both pole choices are compared at the same momenta within a sample. Each replay records native residuals and rechecks above-tolerance points on the same phase-space draw at 160 bits.

The external library can be prepared with `source/internal/export/madgraph-runtime.py --config config.json --output reference`. Use the `sm-full` model, `AllowedParticlePDGs` equal to `[11,22]`, and `PhysicalPDGs` in beam-first order. For two fermion pairs the outgoing fermions are electron, positron. For three or four pairs the outgoing list alternates positron, electron. Photons come last.

For four and six fermions, `--compare-orders` also evaluates both original chiralities and all attachment orders, alongside the shorter endpoint-exchange formula. For eight fermions the signed replay evaluates the ordered Dirac expression and the spinor brackets separately. The MadGraph replay evaluates the bracket formula with the permutations grouped on each side of the propagator. Both orders follow from the same signed sum. Pass `--compare-orders` before the number of pairs to evaluate all three expressions at each point. In a 64-point fixed-helicity MadGraph control, the direct and grouped brackets agreed with the external rate to within `1.6e-14`. A separate 24-point signed control checked the relative sign of the grouped expression against the Feynman sum.

## Balanced six photons

Both native constructions passed 62 physical residues, the fixed-angle energy bound, reference independence and 100,000 signed Feynman and final-reduction checks. The AllSpurious form also passed the spurious-pole test.

| Pole preference | Elementary complexity | Automatic summand complexity | Construction time on one worker | Paper vs signed Feynman | Paper vs polarized MadGraph |
| --- | ---: | ---: | ---: | ---: | ---: |
| AllPhysical | 266,940 | 5,432 | 194.71 s | 8.10787e-11 | 4.44907e-12 |
| AllSpurious | 368,928 | 22,120 | 192.22 s | 8.10910e-11 | 8.56388e-12 |

Each manuscript comparison used 100,000 phase-space points per column. The seeds were 836191 for the signed diagrams and 80000000 for MadGraph. Both formulas passed throughout at native precision. The spurious paper formula fixes the polarization of each leg before permuting its assignment to the left or right current.

## Eight fermions

The elementary AllPhysical and AllSpurious expressions agree after ordering the commutative sums and products. Their expanded weighted complexity is 2,433,024 over 1,728 propagator classes. The automatic permutation summand fell from 5,120 to 434. Both original constructions passed all 82 channels and 100,000 signed Feynman and final-reduction comparisons per pole choice. With the shorter display and automatic complexity measurement, the C++ notebook repeated the construction with seed 840001. Both pole prescriptions met the 2,433,024 target, returned summand score 434, passed all 82 channels and agreed with signed diagrams on another 100,000 points per prescription, to within 4.26e-11.

On 100,000 new coherent-spin points (seed 106000000), the ordered Dirac formula and its bracket form had maximum relative differences of 9.32007e-11 and 8.15936e-11 against the independent Feynman sum. The bracket formula, grouped at the central propagator, agreed with the MadGraph spin-summed rate to 2.62454e-13 on 100,000 different momenta (seed 106200000). All three comparisons passed at native precision.

## Scores and validation of the seven representatives

The native constructor checked every listed residue in each pole prescription, the fixed-angle bound, reference independence and final reduction. The score columns use the same momentum-weighted complexity. For eight fermions the constructor measured the full elementary score by grouping disjoint propagator classes and validated the shorter generated summand in both pole prescriptions.

| Pairs | Photon helicities | Pole preference | Physical channels | Elementary score | Permutation summand score | Paper vs signed Feynman | Paper vs polarized MadGraph |
| ---: | --- | --- | ---: | ---: | ---: | ---: | ---: |
| 1 | `+++---` | physical | 62 | 266,940 | 5,432 | 8.11e-11 | 4.45e-12 |
| 1 | `+++---` | spurious | 62 | 368,928 | 22,120 | 8.11e-11 | 8.56e-12 |
| 2 | `++++` | physical | 92 | 1,006,800 | 37,182 | 3.10e-10 | 2.45e-12 |
| 2 | `++++` | spurious | 92 | 664,368 | 32,270 | 3.10e-10 | 2.44e-12 |
| 2 | `+++-` | physical | 92 | 1,032,384 | 47,034 | 5.13e-10 | 1.85e-12 |
| 2 | `+++-` | spurious | 92 | 589,056 | 31,518 | 7.73e-10 | 8.17e-11 |
| 2 | `++--` | physical | 92 | 1,062,768 | 55,502 | 1.10e-9 | 3.77e-12 |
| 2 | `++--` | spurious | 92 | 598,704 | 32,846 | 1.11e-9 | 2.34e-10 |
| 3 | `++` | physical | 90 | 2,020,032 | 6,588 | 5.33e-10 | 2.94e-12 |
| 3 | `++` | spurious | 90 | 2,009,376 | 5,811 | 7.97e-10 | 3.81e-11 |
| 3 | `+-` | physical | 90 | 2,233,728 | 8,289 | 7.18e-9 | 6.21e-12 |
| 3 | `+-` | spurious | 90 | 1,908,576 | 6,495 | 4.71e-8 | 1.56e-10 |
| 4 | `none` | both | 82 | 2,433,024 | 434 | 8.16e-11 | 2.62e-13 |

The last two columns give the largest native relative differences over 100,000 points per comparison. The charged-family signed seeds were 101000000 to 106000000 in steps of 1000000, in row order by representative. The MadGraph seeds were 200000 higher. The replay programs report native maxima and record the seeds and residuals of points requiring a 160-bit replay.

Only the six-fermion `+-` AllSpurious formula crossed the signed threshold of 1e-8. At seed 105058401 the native discrepancy was 4.70819e-8. Replaying the same draw at 160 bits gave 3.50439e-42 both with the original independent Feynman reference spinors and with fixed photon references. Reproduce the point with `tests/results/eight-point-charged-trees 3 1 1 105058401`.

For the three additional parity representatives (four fermions with `+---` or `----`, and six fermions with `--`), separate 256-point signed and MadGraph comparisons of both projected formulas also passed. Permutations of identical photon labels give the other assignments.

## Change in automatic summand complexity

The comparison uses the same complexity measure before and after the endpoint exchanges. Scores count the weighted expressions inside the displayed permutation sums.

| Pairs | Photon helicities | Physical before | Physical after | Spurious before | Spurious after |
| ---: | --- | ---: | ---: | ---: | ---: |
| 2 | ++++ | 192,284 | 37,182 | 115,988 | 32,270 |
| 2 | +++- | 211,312 | 47,034 | 111,908 | 31,518 |
| 2 | ++-- | 217,916 | 55,502 | 114,764 | 32,846 |
| 3 | ++ | 33,976 | 6,588 | 33,092 | 5,811 |
| 3 | +- | 62,048 | 8,289 | 53,016 | 6,495 |
| 4 | none | 5,120 | 434 | 5,120 | 434 |

The targeted six-point runs used 10,000 signed points per pole choice. They reproduced the previously validated scores of 692/712 for the balanced four-photon case, 1,600/1,632 for four fermions with opposite photon helicities, and 3,168 for six fermions in both prescriptions. All 47 CTest cases passed.
