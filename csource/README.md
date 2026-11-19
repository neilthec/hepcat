# Native amplitude generation, reduction and completion

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

## Generate from HEPCAT model files

A native program can start with particle names. Read each model once and reuse it across processes. The QED vertex model is number 3 and its physical-pole factorization model is number 4. Use models 1 and 2 for the corresponding SM pair.

```cpp
#include "amplitude.hpp"

hepcat::model vertices("models", 3);
hepcat::model lower_amplitudes("models", 4);
hepcat::amplitude amp({"e", "E", "A.+", "A.+", "A.-"}, vertices, lower_amplitudes);
amp.generate();

const auto& diagrams = amp.diagrams();                       // 6 reduced diagrams
const auto& channels = amp.factorization_products();          // 6 reduced products
const auto& factorized = amp.complete_factorization();
const auto& final_amp = amp.complete_high_energy();
```

The reader loads the particle, variable, function and interaction tables for the selected number. Model copies share those immutable tables. The two models can come from different directories if their common external particle definitions and parameter scopes agree. A separate model can be used at the same time without creating global parameter names.

`generate_diagrams()` and `generate_factorization_products()` are available separately. Calling `generate()` schedules both collections in one queue. A single worker sews the complete internal spin or helicity sum for its row and reduces that row with its own numerator shells. It keeps the reduced result and off-cut remainder and releases its working algebra. Output order is independent of worker completion order. Subsequent generation calls skip collections already built. Pass a worker limit, such as `amp.generate(4)`, or use `amp.generate(1)` for serial execution.

Automatic constructive references use a cycle of massless external particles when available and a massive spectator for a lone photon. Pass `hepcat::generation_references::keep` to keep explicit references or `hepcat::generation_references::physical` to request physical denominators where a suitable spectator exists. A factorization product is defined on its own pole. Its continuation can differ from the Mathematica representative while giving the same residue.

From Mathematica, after loading HEPCAT and the optional bridge, the equivalent call is

```wolfram
batch = HEPCATNative`GenerateNativeAmplitude[{"e","E","A.+","A.+","A.-"},
  ExpandFileName["models"], 3, 4, binary];
batch["Diagrams"]
batch["FactorizationProducts"]
```

For an independent Feynman diagram calculation, give the one-model constructor a CalcHEP model directory containing `lgrngN.mdl` and call `generate_diagrams()`. The result contains the raw Dirac and Lorentz tensors with off-shell internal propagators and only external mass rules. The four-argument Mathematica bridge call `GenerateNativeAmplitude[process, directory, number, binary]` returns those rows for use with the package's `FeynmanToSpinorProducts`. Native spinor reduction applies to constructive input.

In the Mathematica bridge, `"PolePreference" -> "AllSpurious"` requests the cyclic massless reference policy with the massive-spectator fallback.

The [generation benchmark](generation_benchmark.wls) compares individual diagram spin polynomials, including their remainders, at two conserved exact points. It compares each factorization product at two regular points on its own pole. It times the fused native generation and reduction separately from model loading and bridge transfer. Run it with a licensed Wolfram kernel after building the executable. The [model reader check](model_reader_oracle.wls) compares vertex expressions and factors with Mathematica for all shipped constructive and CalcHEP models. Additional checks cover [three identical fermion pairs](generation_six_fermion_checks.wls), [quark color coefficients](generation_color_checks.wls), [gluon channels](generation_gluon_checks.wls) and [off-shell Feynman tensors](generation_feynman_checks.wls).

On the eight-worker release build, the electron pair with four positive-helicity photons took 1.01 s with one worker and 0.32 s with eight for its 24 diagrams and 14 channels. Peak RSS went from 5.8 to 12.2 MiB. For four W bosons the corresponding times were 7.91 s and 2.76 s, with 15.2 and 37.0 MiB peak RSS. These timings cover topology generation, sewing and reduction after loading the models. The [measurements for all eight processes](generation-benchmark-results.json) also report model read time and Wolfram raw generation time separately.

## Reduce diagrams and factorization products in one batch

Use `hepcat::amplitude` when several items have the same ordered external masses. Add the diagrams and the sewn factorization products before calling `reduce()`. Each diagram can have several internal numerator shells. Each factorization product has its own channel shell. A product stores the complete HEPCAT channel expression, including its original denominator. Duplicate rows and their individual exchanged masses keep their positions.

```cpp
#include "amplitude.hpp"

hepcat::amplitude amp({"me", "me", "0", "0"});
amp.add_diagram("MomProd[1,3]", {{{1,3}, "me"}});
amp.add_factorization_product("MomProd[2,4]", {{2,4}, "me"});
amp.reduce();

const auto& diagram = amp.diagrams().front();
const auto& product = amp.factorization_products().front();
auto rest = diagram.expression.discarded_remainder();
```

In this small example both dot products reduce to zero. Adding the corresponding stored remainder recovers each input away from its cut. The two collections keep their original order when threads finish at different times. A successful call updates all pending items together. If one fails, the earlier visible expressions and remainders remain available. Adding an item after a successful call and calling `reduce()` again schedules just the new work.

The combined queue starts larger input expressions first. The default worker count is the smaller of the number of pending items and the hardware thread count (at least one for a nonempty queue). `amp.reduce(1)` runs serially. Use a positive limit such as `amp.reduce(4)` when the intermediates are large relative to available memory. `last_worker_count()` reports the size of the dispatch team, including the caller.

### Use the batch from Mathematica

Build the native code with the commands below. From a kernel whose working directory is the repository root, evaluate

```wolfram
Get["csource/NativeAmplitude.wl"]
```

Then evaluate

```wolfram
qed = ReadModel["models", 3];
qedFactorization = ReadModel["models", 4];
binary = ExpandFileName["tests/results/native-reduce-build/hepcat-reduce-amplitude"];
batch = ReduceNativeAmplitude[{"e", "E", "A.+", "A.+", "A.-"},
  qed, qedFactorization, binary];
batch["Diagrams"]
batch["FactorizationProducts"]
```

The bridge loads HEPCAT if needed, generates both lists in Mathematica, and sends them to a single native amplitude. To reuse already generated rows, call `ReduceNativeAmplitude[diagramPairs, channelRows, binary]`. Mathematica contracts spin indices and resolves each row's explicit mass labels before transfer. By default, the batch keeps the reference spinors of its input. With at least two massless external legs, `"PolePreference" -> "Cyclic"` requests a common cyclic reference choice across the batch. `"Workers" -> 1` requests serial native reduction.

The returned lists have the ordinary HEPCAT diagram and channel formats. `"DiagramRemainders"` and `"FactorizationRemainders"` hold the off-cut corrections in the same order, with zero for an empty remainder. A factorization product containing a cut-only object such as a massless exchanged-particle x factor has a physical interpretation on that pole. Its reduced form and remainder preserve the supplied symbolic continuation. Save the batch if a later step needs those corrections. Passing just the two lists to `CompleteFactorization` supplies the reduced rows only. `"ReductionSeconds"` measures the native `amp.reduce()` call. Wrap the Mathematica call in `AbsoluteTiming` to include generation, transfer and result reconstruction. This optional entry point can be called independently of `ConstructAmplitude`.

### Combined queue benchmark

These timings use the actual QED diagram and factorization rows on an Apple M3 with eight hardware threads. Each table entry is a median in milliseconds over seven interleaved runs after warmup. The two-queue timing adds the native time to reduce the diagrams in parallel and then the products in parallel. The combined queue uses the same reductions and worker limit. The caller columns time the complete Mathematica calls on pre-generated inputs, including conversion, process startup and reconstruction of the result.

| Electron pair plus photons | Diagrams + products | Serial native | Two parallel queues, native | Combined queue, native | Two calls, caller | Combined call, caller |
|---|---:|---:|---:|---:|---:|---:|
| Two, ++ | 2 + 2 | 0.71 | 0.78 | 0.80 | 41.7 | 25.9 |
| Three, ++− | 6 + 6 | 24.43 | 13.16 | 9.76 | 111.2 | 90.1 |
| Four, ++++ | 24 + 14 | 129.49 | 45.28 | 41.95 | 313.7 | 298.8 |

All three schedules returned identical ordered expressions, metadata and remainders. Mathematica independently checked every spin coefficient of each input against its reduction plus its remainder at four exact off-cut samples. Each of the 22 factorization products also retained its nonzero residue at a regular point on its own pole. Adding a term that vanishes on a chosen shell to a real diagram and to a different channel exercised nonzero remainders in each collection. See the [sample timings and validation counts](batch-benchmark-results.json).

A separate SM check used the six WWWW diagrams from model 1 and six factorization products from model 2. Serial and eight-worker reductions agreed item by item. The full spin polynomials of all six diagrams and four massive-exchange products agreed at an exact conserved point after restoring remainders. One diagram had a nonzero remainder. All six products preserved their nonzero residues on their respective poles, including the two photon-exchange products with cut-dependent x factors.

To reproduce the combined benchmark from the repository root, set `HEPCAT_BATCH_BINARY` to the absolute path of the built `hepcat-reduce-amplitude` executable and run `WolframKernel -script csource/batch_benchmark.wls`. This also writes its compact report under `tests/results/native-batch-benchmark`. With the same environment setting, run the focused bridge regressions using `WolframKernel -script csource/batch_bridge_tests.wls`.

## Complete the physical factorization channels

Once the process contains its diagrams and **all** its sewn channel rows, call
`amp.complete_factorization()`. The return value is a reference to the completed
`amp_expression` owned by the process. Copy it to keep it across later mutations
of the process. An explicit worker count of one runs the completion serially.

```cpp
#include "amplitude.hpp"
#include <iostream>

int main() {
    hepcat::amplitude amp({"me", "me", "0", "0"});
    amp.add_diagram("g");  // Contact contribution already in the input.
    amp.add_factorization_product(
        "Times[-1,Power[PropDen[Mom[Multiparticle[1,3]],me],-1]]",
        {{1, 3}, "me"});
    const auto& completed = amp.complete_factorization();
    std::cout << completed.str() << '\n';
    std::cout << amp.last_factorization().route << '\n';
}
```

This example restores the missing exchange and keeps the supplied contact
contribution, giving $g+1/((p_1+p_3)^2-m_e^2)$. Supply the full channel
expression in HEPCAT sewing conventions. The solver removes the integration
contour factor when present and applies the sewing sign once. It groups a cut
and its complement at the same exchanged mass and sums their supplied state
contributions. For data prepared elsewhere, `add_prepared_diagram` and
`add_prepared_factorization_product` accept the reduced expression, shell
metadata, and its discarded remainder. Ordinary additions are prepared on
demand.

On each cut the target residue minus the seed residue is the right-hand side
of an equation. The columns are residues of possible corrections. Every cut
constrains the same coefficients. The solver starts with the seed and tries a
common rescaling of its pole terms, retained remainders, and individual diagram
or channel contributions. It can also change a polarization reference or shift
two distinct massless legs to continue a sewn numerator off its original cut.
A shifted sum is a proposal, including when the shift may have a boundary term.

Workers assemble exact rational rows independently on different cuts. One
sparse elimination determines the shared coefficients, then fresh cut points
test the resulting expression. A failed validation point supplies another
constraint. The fit uses external momenta obeying momentum conservation,
the external mass shells and the selected channel shell. It varies kinematics,
spin and model parameter values. Internal numerator-shell assumptions from
unrelated diagrams are excluded from this check.

The returned expression has passed sampled **physical residues**. A term
regular on every physical cut can still change an off-cut amplitude, including
through spurious denominators. Follow with `CheckAmplitudes` for the spurious
poles, external references, high-energy behavior and an independent Feynman
comparison. A missing direction, unsupported input, or unsampleable cut
produces an error and publishes no new completion. Supply additional full
amplitude directions with `factorization_options::correction_basis` if
needed. The current native evaluator handles simple physical poles and a
bounded spinor/scalar dialect, with coefficients in the rationals extended
by $i$ and $\sqrt{2}$. Reduce color tensors to independent coefficients before
using this method. Sampled scalar parameters are independent unless their
algebraic relations have already been substituted in the input.

In Mathematica the optional bridge accepts the previous batch directly.

```wolfram
fit = HEPCATNative`CompleteNativeFactorization[batch, binary, "ReturnDetails" -> True];
fit["Amplitude"]
KeyTake[fit, {"Route", "Channels", "Rank", "ValidationSamples", "CompletionSeconds"}]
```

Without `"ReturnDetails" -> True` the result is simply `{externalMassRules, expression}`.
You can also supply two detailed `PrepareAmplitudes` reports, raw diagram and
channel lists, or a process and both models, using the same argument order
as for `ReduceNativeAmplitude`. For raw input `"PolePreference" -> Automatic`
chooses reference replacements before native reduction. The prepared-batch
form preserves the references and remainders you supplied. The native time
excludes model loading, input/output, Mathematica reference assignment and
initial diagram preparation.

### Reproduce the completion timings

Set `HEPCAT_BATCH_BINARY` to the absolute path of the Release executable.
Run the following scripts in separate Wolfram kernel sessions from the
repository root.

```sh
WolframKernel -script csource/factorization_benchmark.wls
WolframKernel -script csource/factorization_sm_benchmark.wls
```

The [QED timings and validations](factorization-benchmark-results.json) include
both the in-memory solve and the round trip through the Mathematica bridge.
The two native worker modes receive the same diagram and channel data as the
serial Mathematica baseline. In addition to the raw channel checks, the QED
benchmark compares every massive-spin coefficient with the stored amplitude
at two conserved off-cut points and two electron masses. The [Standard Model
checks](factorization-sm-benchmark-results.json) include recovery of a missing
Higgs exchange, exact agreement with the full four-Higgs expression, and the
separate high-energy check on four-W scattering. On the four-W input the
seed passes the physical cuts and still requires the next completion stage.

On the eight-core Apple Silicon benchmark, the physically prepared mixed
three-photon case took 28.93 s in Mathematica, 0.098 s in serial C++, and
0.068 s in parallel C++ (0.111 s including the bridge). For four positive
photons, all 14 seed residues initially failed with either preparation. With
AllPhysical preparation, parallel completion took 0.560 s. With cyclic
preparation it took 0.217 s. The serial Mathematica call reached its 90 s
budget for each input. Both native results passed the independent off-cut
comparison. These preparation labels describe the input gauge choices.

## Complete the fixed-angle high-energy behavior

After completing the physical residues, call the next method on the same process.

```cpp
const auto& result = amp.complete_high_energy();
const auto& report = amp.last_high_energy();
std::cout << result.str() << '\n';
std::cout << report.route << " (maximum energy power " << report.bound << ")\n";
```

For four or more external legs, the default bound for an amplitude with $n$ legs is $E^{4-n}$. The method can also start from a supplied factorization result with `amp.complete_high_energy(seed, options)`. Use `high_energy_options::energy_bound` for a different theory bound and `workers = 1` for serial execution. A pure C++ caller can put rational assignments for related masses and couplings in `parameter_points`. For example, an electroweak model with $M_W=M_Z c_W$ needs assignments that satisfy that relation at every point. Without bindings the native scalar evaluator treats distinct symbols as independent.

Each check follows a fixed-angle deformation that preserves momentum conservation and every external mass shell. The solver computes exact Laurent coefficients above the allowed power. It first checks the full seed, so amplitudes that already have the required behavior return directly. If the seed grows too fast, the solver tries supplied local corrections, local pieces of retained remainders and the seed, then spinor contractions built from its external spin weights, masses and scalar factors. These corrections have no kinematic poles, so they preserve every physical residue. Independent directions in momentum space can be evaluated concurrently. All workers contribute to a single coefficient fit.

The search returns one representative that passes new kinematic tests. The validation report distinguishes full interpolation over the external spin variables from sampled spin projections. A coefficient left free by the fit can describe either a redundant spinor identity or a contact term whose growth already meets the bound. Growth alone cannot fix the latter. Use the model or an independent off-cut comparison to determine such couplings. The automatic contact search has a finite basis. If no tested correction works, the call reports failure. A seed with a spurious pole or reference dependence can require a different factorization continuation.

### Call from Mathematica

```wolfram
he = HEPCATNative`CompleteNativeHighEnergy[fit, qedModel, binary];
checked = HEPCATNative`CompleteNativeHighEnergy[
  fit, originalChannelRows, qedModel, binary, "ReturnDetails" -> True];
checked["Amplitude"]
checked["ValidationReport"]["Summary"]
```

Here `fit` is the external-mass pair or detailed factorization result. The bridge evaluates the model's parameter definitions at generic exact points. Supply `"ParameterPoints" -> {rules1, rules2, ...}` if the model requires special relations or if the automatic choices do not give rational masses and couplings. Passing the original channel rows runs Wolfram's `CheckAmplitudes` independently and requires its checks to pass. Its time is listed under `"ValidationSeconds"`. `"CompletionSeconds"` covers the native stage with the input already in C++.

To include the shell remainders when repairing a known seed, use `CompleteNativeHighEnergy[fit, batch, model, binary]`. Use `CompleteNativeHighEnergy[batch, model, binary]` to factorize a prepared native batch and run the high-energy stage in one process. The default output is `{externalMassRules, expression}` for each call.

The [measured comparisons](high-energy-benchmark-results.json) include four-W scattering, four-Higgs scattering and electron-positron annihilation with two, three and four photons. They report the native serial and parallel time, time including the bridge, peak process RSS, independent Wolfram validation and exact off-cut comparisons with the stored amplitudes. On the four-W input the native correction canceled growth through $E^4$ and matched the stored amplitude at both independent off-cut points. The median native times were 4.51 s serial and 1.76 s with eight workers. Peak process RSS rose from 5.78 MiB in serial execution to 14.95 MiB in parallel. The Mathematica call on the same input returned unresolved after 17.80 s.

Set `HEPCAT_BATCH_BINARY` to the absolute path of the Release executable and run

```sh
WolframKernel -script csource/high_energy_bridge_tests.wls
WolframKernel -script csource/high_energy_benchmark.wls
```

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
