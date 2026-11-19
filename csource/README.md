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

The driver uses fresh processes serially. Each language loads or parses its input before the clock starts. The C++ timer covers `amp.reduce()`, including trial storage, the selection of the final result, and releasing intermediates. Rendering, output files, diagram construction, model loading and cross-checks are outside the clock. The native CLI also records import and export time separately. Both engines receive the same exact sums of HEPCAT diagrams, with model mass labels resolved before timing. This includes the internal exchanged-particle mass labels already supplied by each diagram. Scalar products of momenta are left off the internal shells. In the QED process the two photon references are chosen before summing the two diagrams. The six-diagram vector process uses the notebook ordering `W+, W+, W-, W-`. Neither benchmark requests an internal-shell rewrite of the sum.

For memory, the controller samples the resident bytes of each already-initialized process from the start to the end of the timed operation. The baseline includes the process, its runtime, and the input. The sampled peak minus baseline indicates extra resident memory during reduction. The prepared Wolfram expression can vary between repetitions because preparation has time budgets. The driver keeps each timed output and includes it in validation. Sampling can miss brief allocations, especially on the small QED case. On macOS and Linux the native benchmark also records the OS process high-water mark immediately before and after reduction. Wolfram's internal evaluation peak is reported separately in the run records.

Validation compares the input, the native result, `ReduceSpinorProducts`, and external-only `PrepareAmplitudes`. It uses conserved exact kinematics away from poles and checks all coefficients of the external spin variables with no sign adjustment. The QED driver additionally checks equality symbolically with charge, mass, spins and kinematics free on the rational chart.

### Measured comparison

Measured on macOS arm64 with AppleClang 21.0.3, a Release build, and Wolfram 15.0.0. Times are medians of three fresh processes. Memory columns are the largest sampled process RSS during the call and the largest increase above its post-import baseline, in MiB. Complexity uses Mathematica's public `SpinorExpressionComplexity` for all outputs.

| Input | Operation | Reduction time | Peak RSS | Extra RSS | Output complexity |
|---|---|---:|---:|---:|---:|
| QED, score 36 | C++ `amp.reduce()` | 0.331 ms | 1.91 | 0.30 | 18 |
| | Mathematica `ReduceSpinorProducts` | 12.58 ms | 311.0 | 1.20 | 18 |
| | Mathematica `PrepareAmplitudes` | 1.263 s | 346.1 | 37.33 | 18 |
| WWWW, score 2232 | C++ `amp.reduce()` | 0.209 s | 5.66 | 3.58 | 1038 |
| | Mathematica `ReduceSpinorProducts` | 0.370 s | 323.9 | 6.59 | 2232 |
| | Mathematica `PrepareAmplitudes` | 63.34 s | 450.3 | 134.53 | 898, 807, 788 |

The QED comparison reached equal complexity. For WWWW the native reducer removed more than half the input complexity in a fraction of a second. The broader Wolfram preparation search returned simpler expressions in all three runs. Its time budgets affected both timing and output. The three native expressions were byte-identical for each process. The measured native import/export medians were 0.059/0.091 ms for QED and 1.074/0.359 ms for WWWW. These are CLI file/FullForm costs, separate from the reported reduction times. Full run measurements, output hashes and validation scope are recorded in [benchmark-results.json](benchmark-results.json).
