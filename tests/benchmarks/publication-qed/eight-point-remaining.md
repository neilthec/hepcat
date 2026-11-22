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
