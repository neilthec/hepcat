# Native amplitude reduction prototype

`hepcat::amp_expression` holds a symbolic expression, the ordered external masses, and any internal numerator shells declared for that expression. Its particle count is the length of the mass list. Labels start at 1 and all momenta are incoming. Construct it once, then call `amp.reduce()` to simplify it in place. To keep the original for comparison, copy the object before reduction. Copies share their stored expression nodes. An ordinary native process needs no Wolfram installation.

```cpp
#include "amp_expression.hpp"
#include <iostream>

int main() {
    hepcat::amp_expression amp(
        "MomProd[1,3]", {"me", "me", "0", "0"}, {{{1, 3}, "me"}});
    amp.reduce();
    std::cout << amp.str() << '\n';
    if (auto remainder = amp.discarded_remainder())
        std::cout << remainder->str() << '\n';
}
```

Here the numerator shell is $(p_1+p_3)^2=m_e^2$. Since $p_1^2=m_e^2$ and $p_3^2=0$, the reduced dot product is zero. Its off-cut remainder is `MomProd[1,3]`. Omit the third constructor argument to use external masses and momentum conservation alone. The optional C interface is in [hepcat.h](hepcat.h), with a runnable short example in [c_api_test.c](c_api_test.c).

## Representation and identities

The reader accepts explicit Wolfram-style expression trees such as `Plus[x,Times[-1,y]]` and `SpinorChain[Spinor["Spin","Angle",1],Spinor["Spin","Angle",2]]`. Numeric algebra uses exact integers and rational numbers through GMP. Model parameters keep their full symbolic identity. Identical subexpressions have one node in an amplitude's storage. Candidate rewrites use temporary storage which is released at the end of a call. Input transfer parses the tree and interns identical nodes. Collection of like terms, algebraic normalization and identity selection occur inside the measured reduction.

The reducer supports chain reversal, external Dirac identities, massless momentum splitting, repeated momentum contractions, explicit massive spin-index contractions, short traces, Schouten identities, momentum conservation, common-factor extraction and bounded collection across terms. External masses and supplied subset masses simplify scalar products. Subsets and their complements have the same invariant mass. Propagator denominators stay intact, and inverse factors use external identities alone. General symbolic coefficient factorization, reference-spinor search, and the full identity search of Mathematica preparation are outside this prototype. For the QED benchmark the photon references are fixed identically before any timing.

When internal shells are supplied, the stored remainder represents the difference between the ordinary external-only reduction of the input and its chosen numerator-shell continuation. If a discarded numerator factor cancels a propagator, its amplitude remainder can be a finite nonzero term. Several cuts give one combined remainder. This preserves cross terms and avoids assigning them arbitrarily to individual cuts. Calling `discarded_remainder()` returns an amplitude with just the external assumptions. The original off-cut expression equals the current expression plus its accumulated remainder wherever the input rational expression is defined. With external assumptions alone there is no discarded remainder. A time-limited identity search in a different implementation may produce another equivalent form.

## Build and run

Use CMake 3.18 or newer, a C++17 compiler, and the GMP development headers and libraries including `gmpxx`. From the repository root run

```sh
cmake -S csource -B tests/results/native-reduce-build -DCMAKE_BUILD_TYPE=Release
cmake --build tests/results/native-reduce-build
ctest --test-dir tests/results/native-reduce-build --output-on-failure
```

If GMP is installed under a nonstandard prefix, pass its directory using `-DCMAKE_PREFIX_PATH=...`. Link your program to the `hepcat_reduce` CMake target and include `amp_expression.hpp` or `hepcat.h`. The Mathematica package and its notebooks can be used independently of the native build.

## Reproduce the comparison

A licensed Wolfram kernel is needed only for generating the diagram inputs, running the Wolfram baselines and independently checking agreement. The Python driver samples process RSS on macOS and Linux. From the repository root run

```sh
python3 csource/benchmark.py --kernel /path/to/WolframKernel \
  --binary tests/results/native-reduce-build/hepcat-reduce \
  --out tests/results/native-reduce-benchmark --repeats 3
```

The driver uses fresh processes serially. Each language loads or parses its input before the clock starts. The C++ timer covers `amp.reduce()`, including trial storage, the selection of the final result, and releasing intermediates. Rendering, output files, diagram construction, model loading and cross-checks are outside the clock. The native CLI also records import and export time separately. Both engines receive the same exact sums of HEPCAT diagrams, with model mass labels resolved before timing. This includes the internal exchanged-particle mass labels already supplied by each diagram. Scalar products of momenta are left off the internal shells. For the electron and positron with two, three, or four photons, every diagram uses the same cyclic choice of reference for each external photon. These inputs contain 2, 6, and 24 diagrams. The six-diagram vector process uses the notebook ordering `W+, W+, W-, W-`. All four inputs use external shells alone. These benchmarks measure simplification of a diagram sum before factorization completion.

For memory, the controller samples the resident bytes of each already-initialized process from the start to the end of the timed operation. The baseline includes the process, its runtime, and the input. The sampled peak minus baseline indicates extra resident memory during reduction. The prepared Wolfram expression can vary between repetitions because preparation has time budgets. The driver keeps each timed output and includes it in validation. Sampling can miss brief allocations, especially on the small QED case. On macOS and Linux the native benchmark also records the OS process high-water mark immediately before and after reduction. Wolfram's internal evaluation peak is reported separately in the run records.

Validation compares the input, the native result, `ReduceSpinorProducts`, and external-only `PrepareAmplitudes`. It uses conserved exact kinematics away from poles and checks all coefficients of the external spin variables with no sign adjustment. For the two-photon case the driver additionally checks equality symbolically with the charge, mass, spins and kinematics free on the rational chart. The higher-point tests verify that the exact rational momenta conserve momentum, put every external leg on shell, and keep the test away from physical poles, singular references and collinear photons.

### How the four-vector reduction works

The original C++ reduction scored 1,038 on the six-diagram WWWW sum. A saved Mathematica preparation of the same input scored 788. Its output contained no step history. In a fresh preparation with `"ReturnDetails" -> True`, the first four-leg tensor pass reduced the score from 2,232 to 746. The later tensor pass brought it to 742. A second independent trace followed 2,232 to 755 to 742 and recorded the method `OfficialSchoutenRewriteAndChainwiseExternalDirac` in both score-changing passes. Preparation has a time budget, so later runs can stop at different equivalent expressions. To inspect a fresh trace, append `--cases wwww --modes trace` to the benchmark command after generating the inputs. The trace records the changed stages, their scores and the types and counts of the recorded identity witnesses in `wwww-trace.json`.

One of the identities used by the Mathematica tensor pass is Schouten with a momentum acting on an external spinor,

$$
\langle a b\rangle\langle c|p|d]
-\langle a c\rangle\langle b|p|d]
+\langle b c\rangle\langle a|p|d] = 0.
$$

If momentum conservation places an external momentum next to its own spinor, the external Dirac equation removes it in exchange for the mass. This can make terms from different powers of an external mass combine. A single Schouten exchange can initially increase the number of terms even when two exchanges together shorten the numerator.

The native reducer works with exact coefficients inside each denominator group. It tries local coefficient cancellations and, for four massive vector legs, compares simultaneous combinations of identities in several external momentum bases. It recognizes that case by the number of spinors on each leg. The identities include the bare-bracket and momentum-inserted Schouten relations and the determinant of two sandwiches containing the same external momentum. A floating-point search proposes monomials to eliminate. Rational elimination using the generated identities constructs the candidate, and the reducer compares its complexity with the best expression already found. The native routine receives only the diagram sum and its external mass list in this comparison.

### Measured comparison

Measured on macOS arm64 with Apple clang 21.0.0, a Release build, and Wolfram 15.0.0. Times are medians of three fresh processes. Peak and extra RSS are the largest sampled process RSS during a call and the largest increase above the post-import baseline, in MiB. Mathematica's public `SpinorExpressionComplexity` supplies the final score for every method.

| Input | Operation | Reduction time | Peak RSS | Extra RSS | Output complexity |
|---|---|---:|---:|---:|---:|
| Two photons (++) | C++ reduce | 0.442 ms | 1.97 | 0.36 | 18 |
|  | Mathematica reduce | 13.86 ms | 311.83 | 1.20 | 18 |
|  | Mathematica prepare | 1.328 s | 346.05 | 37.25 | 18 |
| Four W bosons | C++ reduce | 2.289 s | 15.27 | 13.20 | 695 |
|  | Mathematica reduce | 0.135 s | 322.56 | 6.02 | 2232 |
|  | Mathematica prepare | 37.46 s | 433.69 | 117.98 | 760, 751 |
| Three photons (++−) | C++ reduce | 20.85 ms | 2.80 | 1.16 | 272 |
|  | Mathematica reduce | 5.059 s | 192.64 | 8.84 | 810 |
|  | Mathematica prepare | 15.94 s | 303.42 | 118.80 | 212 |
| Four photons (++++) | C++ reduce | 0.470 s | 6.58 | 4.83 | 932 |
|  | Mathematica reduce | 42.77 ms | 187.84 | 2.88 | 9792 |
|  | Mathematica prepare | 12.58 s | 350.11 | 168.00 | 9792 |

All 36 timed outputs matched their input at every spin and sample. The native result was byte-identical across repetitions for each input. The native WWWW expression scored 695. Relative to the archived 788 expression, its expanded numerator has 19 fewer one-insertion monomials, two more bare monomials, and the same number with two insertions. Under HEPCAT's weights this accounts for the 93-point difference. On this machine the deeper native search increased the old native time from 0.209 s to 2.29 s and the peak RSS from 5.66 MiB to 15.27 MiB. Against the current full Mathematica preparation it took about one sixteenth of the time. The benchmark validation includes the 788 ceiling for this exact input.

The mixed three-photon case illustrates the remaining gap in the native identity search. Mathematica preparation finds a representation with score 212, compared with 272 from C++. On four positive-helicity photons C++ reduces the score from 9,792 to 932. Mathematica's bare reduction finishes quickly on that input and reduces the leaf count from 3,745 to 2,228 while retaining the same public complexity score. All comparisons here use the original diagram sums, so the numbers describe reduction before amplitude completion.

The [run records](benchmark-results.json) include each repetition's timing, peak memory, input and output hashes, parameter choices and exact test coordinates.
