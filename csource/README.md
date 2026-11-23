# Native amplitude generation, reduction, completion and validation

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

## Read, publish or reuse an expression

| In C++ | Notebook output | Use |
| --- | --- | --- |
| `expr.plain_text()` | `.txt` | Read spinor chains such as `[213>` and denominators such as `s_13 - Me^2` in the terminal. |
| `expr.latex()` | `.tex` | Include the math fragment inside an equation in a paper. It supplies its brackets, fractions, masses, indices and color factors. You choose the equation numbers and line breaks. |
| `expr.mathematica()` | `.wl` | After loading HEPCAT, use `pair = Get["amplitude.wl"]` to recover `{massRules, expression}`. The rules include declared diagram-local conditions. Use `expr.mathematica(false)` for the expression alone. |
| `expr.str()` | `.expr` | Read the exact native expression back into `amp_expression` with the original masses and any applicable shells. |
| `write_spinas_process(...)` | SPINAS project | Export one or more helicity expressions with their model and physical incoming legs as numerical C++. |

A reduced expression with repeated external masses can have a compact permutation sum. HEPCAT checks the action of adjacent leg swaps on every signed term, including spinor-chain reversal signs. When the sum shortens the formula, `plain_text()` and `latex()` print one summand and the legs to permute. Use `permutation_form()` to retrieve that summand in native syntax and the number of distinct terms represented by each orbit. The sum runs over every permutation with its normalization included. The complexity reported by `complexity()` is the weight of the full expression. The [five-positive example](../tests/benchmarks/publication-qed/seven-point-all-positive.md) gives both weights. For a complete fermion line with one photon of opposite helicity, the constructor can also sum the insertions along the line. In that case the formula exposes the permuted legs and the summand, and its list of monomial orbit sizes is empty. A saved elementary expression also carries enough information for `reduce()` to recover this presentation. The first request for `plain_text()`, `latex()` or `permutation_form()` reads the external spin weights, reconstructs a candidate ordered current, and checks every signed coefficient up to one common scalar factor. The search is bounded and uses the external masses supplied with the expression. The [four-positive, one-negative example](../tests/benchmarks/publication-qed/seven-point-one-negative.md) derives both pole choices and gives their numerical checks.

For two independent sets of identical legs, `grouped_permutation_form()` returns the leg groups and a single summand. Sum over every permutation within each group. The older `permutation_form()` expands the additional groups into its summand for callers that loop over one group. With two photons at each end of a fermion line and one in the middle, the two orders at either end can be added before joining the left and right currents. The [three-positive, two-negative example](../tests/benchmarks/publication-qed/seven-point-two-negative.md) gives the pole choices, symmetry factors, and a signed check of the paper formulas.

For identical fermions, `signed_permutation_form()` returns a summand, symmetric and alternating leg groups, and an optional `joint_exchange`. If the exchange is present, start with the summand plus `joint_exchange->sign` times its image with all listed pairs exchanged simultaneously. Then sum over permutations in each group, multiplying by the sign of every alternating permutation. Spin labels move with the momenta. The unsigned accessors expand all signed operations automatically.

`complexity()` scores the full elementary expression. `permutation_summand_complexity()` gives the score inside the displayed sum, or an empty optional when there is no compact permutation form. The QED C++ notebook reports both scores and checks its historical thresholds against the elementary score.

For photon exchange between two identical charged pairs, the reducer groups ways of distributing photons between the two lines. An occupation specifies how many photons of each helicity attach to one line. Its complement represents the same occupation after exchanging the two lines. Each representative carries the weight required to recover every attachment once. This applies to any photon multiplicity for which the complete exchange topology is present.

At three photons, the [equal-helicity amplitude](../tests/benchmarks/publication-qed/seven-point-four-fermions.md) needs two current contractions and the [mixed-helicity amplitude](../tests/benchmarks/publication-qed/seven-point-four-fermions-mixed.md) needs three. A simultaneous reversal of both lines supplies the second Weyl orientation. The reducer infers helicities, particle labels, and the two identical-fermion groups. When it reads a saved elementary expression, it verifies the signed coefficients of all 16 exchange allocations before accepting the current identity.

For a certified signed sum, the reducer can also combine terms related by a Bose exchange, a Fermi exchange or a simultaneous line reversal. It may apply the identity inside a product when the exchange leaves the other factors invariant. It keeps a rewrite only when the summand has fewer leaves and its weighted complexity is at least as good.

The Wolfram export preserves each model parameter's name, scope and type. To
combine it with a separate Mathematica `ReadModel`, give both readers the
same scope. For example, obtain it with `ModelParameter[qed, "Me"][[2]]`
in Mathematica and pass that string as the third argument to the C++ `model`
constructor. For an existing export, bind its particular native scope to the
corresponding loaded model.

```wolfram
With[{source = "native-..."},  (* copy the scope from the exported file *)
  pair /. ModelParameter[name_, source, _] :> ModelParameter[qed, name]]
```

## Try an identity in C++

Use `reduce()` for automatic simplification. During a derivation, direct the
next step yourself. For example, a scalar product multiplying a chain can be
inserted into that chain using the Clifford anticommutator. This can expose
a cancellation with the next term.

```cpp
using namespace hepcat;
identity_options search;
search.rule = identity_rule::dot_into_chain;
for (const auto& move : expr.identity_candidates(search)) {
    const auto size = expr.preview_identity(move);
    std::cout << move.description() << "   complexity " << size.complexity
              << ", leaves " << size.leaves << '\n';
}
```

Use `auto next = expr.apply_identity(move)` to keep an alternative. The input
retains its original value. For options and rule names see [identities.hpp](identities.hpp).
You can restrict enumeration to one subtree (zero-based argument indices),
cap the number of moves, or exclude expansion rules. Applying a move rebuilds
the chosen occurrence even if another occurrence shares the same underlying
nodes. Re-enumerate after modifying the expression.

Use `expr.check_identity(next, options)` to evaluate both signed expressions
independently in exact arithmetic at sampled conserved four-dimensional
kinematics for four or more external legs. External masses are on shell and
internal propagators are free. The report distinguishes agreement at the
sampled points, a mismatch, and an unresolved calculation. Record the seed
to repeat the check. You can also supply a formula you entered by hand with
the same ordered external masses. The default varies free scalar parameters
independently. Supply rational parameter values for a relation specific to a
model, and give the external color representations for colored expressions.
Unsupported functions or radicals outside the chart's coefficient field
produce an unresolved result. For the final process, run the full validations.

The [worked six-point QED derivation](../tests/Spinor-identity-derivation.cpp)
starts from the independent sum of Feynman diagrams for an electron pair
and four mixed-helicity photons. It previews common-factor extractions,
applies an improving edit, and reduces the spinor chains. Each step includes
a signed check, followed by a comparison with the constructive amplitude in
model 4 at fresh kinematic points. Run it from the repository root.

```sh
cmake -S csource -B tests/results/native-reduce-build -DCMAKE_BUILD_TYPE=Release
cmake --build tests/results/native-reduce-build --target hepcat-derive-example --parallel
tests/results/native-reduce-build/hepcat-derive-example models
```

## Construct an amplitude in C++

From the repository root, build the native executable and list the QED examples.

```sh
cmake -S csource -B tests/results/native-reduce-build -DCMAKE_BUILD_TYPE=Release
cmake --build tests/results/native-reduce-build --parallel
tests/results/native-reduce-build/hepcat-qed --list
```

The C++ equivalent of the notebook's construction call returns the reduced, mass-tagged expression. An optional third model supplies independently generated off-shell Feynman diagrams.

```cpp
#include "amplitude.hpp"
#include <iostream>

int main() {
    hepcat::model vertices("models", 3), channels("models", 4);
    hepcat::model feynman("tests/reference-models/calchep", 2);
    hepcat::amplitude amp({"e", "E", "A.+", "A.+", "A.-"}, vertices, channels, feynman);
    hepcat::construct_options options;
    options.progress = [](const std::string& stage) { std::cout << stage << '\n'; };
    const auto& result = amp.construct(options);
    std::cout << result.str() << "\nComplexity " << result.complexity() << '\n';
    for (const auto& [stage, seconds] : amp.last_construction().seconds)
        std::cout << stage << "  " << seconds << " s\n";
}
```

The call generates and reduces the tree diagrams and factorization products, completes the residues and fixed-angle high-energy behavior, reduces the final expression and validates it. It publishes the result once all required checks pass. A failed check throws `construction_validation_error` with its report. A previously successful result remains available after a failed attempt. Physical poles are the default. To reproduce the QED spurious-pole construction, use factorization model 5 and set `options.pole_preference = hepcat::generation_references::cyclic`.

### Run the QED examples and external comparisons

Both C++ notebooks save each amplitude in the four expression formats above. The terminal shows short amplitudes inline. Each validation table gives the check, its outcome and the number of checks or phase-space points covered.

The [C++ QED example](../tests/QED-6-point-construction.cpp) covers the notebook's helicity families through six external legs, all three seven-point tree families of electron-photon QED, all four eight-point families, and the separate all-positive formula study. From the HEPCAT repository root, run one complete family with

```sh
tests/results/native-reduce-build/hepcat-qed --family eeAA --preference physical --skip-formulas \
  --spinas ../spinas --calchep ../CalcHEP/calchep_3.9.2 --madgraph ../MadGraph
```

Point `--calchep` to the CalcHEP installation containing `include` and `lib`. If MadGraph uses a particular Python installation, pass its interpreter with `--python`. Remove the family, preference and formula filters to run the full study. Use `--construct-only` to run the built-in and signed comparisons alone. Each run prints its directory under `tests/results/spinas-output/run-*`, the QED notebook’s output area. The terminal shows construction stages, validation outcomes, and a final summary. Its `construction.tsv` contains the automatic and reference scores, and each family's `checks.txt` records individual checks. The driver constructs from elementary vertices and lower-point amplitudes. It reads the stored same-multiplicity expression separately to test the signed answer. If the notebook has a shorter worked form, the driver validates that form too and records which one it exports. A fixed seed, for example `--seed 9123`, reproduces a run.

The generated process evaluates a signed momentum sum and each required chiral matrix once in `set_momenta`. For chains that begin with the same external spinor and ordered matrix insertions, it also shares the resulting two-component spinor. For example, both $\langle a|Q|b]$ and $\langle a|Q|c]$ use the same $\langle a|Q$. The endpoint spin, chirality, crossing signs and order of insertions enter the calculation. $Q$ can be off shell.

Spin-independent scalar coefficients are shared among polarization assignments. With three or more massive spinning legs, the writer also caches contractions whose only spin dependence is one external spinor slot. Each such contraction has two values. The cache holds at most 4,096 of these contractions per process. Components that track numerical conditioning retain their arithmetic and error propagation. The QED benchmark's operand-table compiler uses sequential writes by default. Its optional compact layout reuses each arithmetic slot after its last read. The [rate replay](../tests/benchmarks/publication-qed/rate-study.md) measures the effect on complete rates and compares complex polarized amplitudes with the archived export.

The native writer supports color singlets, SU(3) triplets, antitriplets and adjoints. A colored process exposes the fixed-color amplitude as `amp(spins, colors)`, with each color numbered from zero. Its `amp_squared(spins)` contracts all color components coherently and `amp2()` also averages the incoming colors. Supply the helicity components and the zero-based incoming leg positions to `write_spinas_process`. Set the model and installation paths for each generator in `external_options`, call `write_external_project`, then `validate_external_project`. These C++ APIs are declared in [spinas_export.hpp](spinas_export.hpp) and [external_export.hpp](external_export.hpp). CalcHEP tests the polarization sum. For validation the build uses the standalone denominator helper from the selected CalcHEP sources. CalcHEP 3.9.2's optional micrOMEGAs helper indexes a 40-entry width array starting at one. A process with 40 or more distinct denominators can then acquire unintended widths. A binary-only installation that supplies this helper reports the affected process before evaluation. The native validator appends `-g0` to a private copy of the CalcHEP compiler flags for its temporary matrix library. This saves disk space when linking processes with thousands of squared diagrams and preserves the selected arithmetic and optimization settings. MadGraph tests each labeled massless helicity with massive spins summed, and checks the complete rate independently against the exported spin sum. For spurious-pole expressions with identical massless bosons, the generated squared-rate routine estimates cancellation and can choose a better-conditioned, Bose-equivalent ordering. The complex polarized amplitude keeps the original ordering and phase convention for use in spin correlations. Widths are disabled during comparisons. Both C++ notebook drivers default to 100,000 common phase-space points per family and engine. Use `--points N` to choose another sample size (minimum 10,000, or 100,000 in QED publication mode). At seven points the full families include all 32 ordered five-photon helicities, all eight ordered three-photon helicities with four fermions, and both single-photon helicities with six fermions. The exporter obtains permutations of identical photons from the constructed representatives. Selecting only the published five-positive-photon case runs a fixed-helicity MadGraph comparison.

To run just the seven-point calculations, use the following command. The default covers both pole preferences. Each helicity representative goes through factorization, high-energy and signed Feynman checks. Once all the representatives in a family pass, its full SPINAS process is built for the external comparisons.

```sh
tests/results/native-reduce-build/hepcat-qed --legs 7
```

Use `--legs 7 --list` to see the three families and their 12 representatives. For a single construction, try `--family eeeeAAA --helicity ++- --construct-only`. The [seven-point measurements](../tests/benchmarks/qed-seven-point.tsv) give the weighted complexity, the automatic and shortest validated leaf counts, diagram and channel counts, and signed Feynman comparisons for each pole preference. Each run writes the formulas in plain text, LaTeX, Mathematica and native form. The size targets in the example are regression checks. Construction uses the cubic vertices and lower-point products. Select `--publication` to compare the earlier published cases against their signed formulas.

For six photons with one negative helicity, select `--family eeAAAAAA --helicity '+++++-'`. The [eight-point formulas and validation results](../tests/benchmarks/publication-qed/eight-point-one-negative.md) include two spurious-pole representations and compare their numerical conditioning. For two negative photons, use `--family eeAAAAAA --helicity '++++--'`. The [midpoint formulas and validation results](../tests/benchmarks/publication-qed/eight-point-two-negative.md) show how three-photon currents at opposite ends of the electron line cover all 720 diagrams.

At eight points use `--legs 8` or select a family with `--family eeAAAAAA`, `eeeeAAAA`, `eeeeeeAA` or `eeeeeeee`. The [remaining eight-point formulas](../tests/benchmarks/publication-qed/eight-point-remaining.tex) give the balanced six-photon result and the helicity representatives of the multi-pair families. For large generated Dirac trees, the constructor tries conserving the full current before searching for a smaller expression for each individual diagram. It checks that candidate on every factorization channel. For trees containing only massive charged pairs, the reducer also reverses ordered vector insertions along a charged line and writes a bare vector current as the two Weyl orientations related by endpoint exchange. It counts the ordered diagrams covered by those operations before assigning a weight. For eight fermions the star covers 576 diagrams and the path covers 1,152.

With radiation, the same endpoint exchange supplies the second Weyl orientation of a complete terminal current. Each emitted photon changes the exchange sign. On an interior line, retaining one of the two opposite orders of the virtual photons gives a further reduction. The real photons still occupy every position relative to those exchanges. HEPCAT counts the original orders and compares the reduced summand weights before adopting a presentation. It keeps the elementary expression for residue tests and numerical export.

For large final expressions, `amp_expression::complexity()` can collect by monomials in the propagators and expand one coefficient at a time. It uses the same momentum-weighted score as direct expansion. The [eight-point QED comparison](../tests/benchmarks/publication-qed/eight-point-remaining.md) gives the full and permutation-summand scores separately.

The five-photon line also admits a check against the independent [one-negative-photon formula](../tests/benchmarks/one-minus/README.md). Its spurious-pole form and the parity-conjugate construction each agreed on 32 exact external-kinematic samples. The [two-pair all-positive formula](../tests/benchmarks/four-fermion-all-plus/README.md) gives a separate 32-sample signed check for four fermions and three positive photons. The expressions can be shorter when written in terms of recursively defined currents. The table measures the full HEPCAT expression returned by the constructor.

After both helicity partners have been constructed, a [worked C++ derivation](../tests/benchmarks/qed-helicity-partners.cpp) can search for a shorter way to print either partner. It exchanges angle and square brackets, relabels the photons and checks the overall sign against the target. For each improvement it runs 32 fresh exact signed comparisons, the built-in checks and 100,000 signed comparisons with independent Feynman diagrams. The formulas and measured sizes are saved in the family's `parity-alternatives` directory. Six seven-point expressions became shorter this way at the same weighted score. The physical five-photon `++++-` expression fell from 7,192 to 6,871 leaves and the spurious four-fermion `+--` expression from 56,798 to 56,110. For example, set `run_dir` to the output directory printed by the constructor and run

```sh
cmake --build tests/results/native-reduce-build --target hepcat-qed-helicity-partners
tests/results/native-reduce-build/hepcat-qed-helicity-partners "$run_dir/physical/eeAAAAA"
```

If native reference arithmetic misses the tolerance, the external validators can replay the same phase-space points with 113-bit arithmetic. CalcHEP replays every saved outlier and checks the original binary64 values. MadGraph rebuilds its generated matrix and HELAS routines, then repeats every requested helicity and the polarization sum. The reports keep the original discrepancies and identify the precision used for the final comparison. The CalcHEP replay requires GCC and libquadmath. The MadGraph replay requires GNU Fortran with binary128 support.

For massless two-body scattering, rounded four-vector components can leave a small mass-shell residual near a forward pole. If every nonzero helicity passes the unchanged-point MadGraph replay and only symbolic zeros still fail, the validator reconstructs those zero-sector points from the same beam energy and scattering direction at 113 bits. Each component must agree with the original within 16 binary64 machine epsilons times the beam energy. The report retains both unchanged-point comparisons and the largest component adjustment. The original absolute tolerance for zero amplitudes also applies to the reconstructed points.

Near a radiation zero, even small mass-shell defects can affect the relative accuracy of CalcHEP's squared amplitude. If its unchanged-input 113-bit replay still disagrees for a two-body final state, the validator reconstructs the on-shell momenta from the sampled beam momentum and scattering direction. It evaluates the generated SPINAS process and CalcHEP at 113 bits on those identical components. This comparison also checks the original SPINAS result against its high-precision value. The report preserves the raw and refined results and saves the shared input in `calchep-chart.csv`. This optional replay uses the SPINAS sources beside its include directory and a GNU C++23 compiler with binary128 and libquadmath.

For event generation, enter a generated family's `spinas` directory and run `cmake --build build`. This builds the Monte Carlo targets as well as the validators. For an installed SPINAS library with no precision setting in its CMake cache, first configure the generated project with `cmake -S . -B build -DSPINAS_PRECISION=DOUBLE` or `LONG_DOUBLE` to match its ABI. For the two-photon example, launch CalcHEP with `(cd build/results && ./n_calchep)` or MadEvent with `(cd build/PROC_eeAA && ./bin/madevent)`. Type `generate_events` at the MadEvent prompt. Each integration uses the exported HEPCAT amplitude through SPINAS. Its independent validator uses the external generator's own matrix element.

### Why grouping photon emissions helps

For a model with the minimal Abelian Dirac coupling, the constructor first checks the two cubic spinor vertices, the internal helicity sum, and the identical-fermion sign on every topology. On a fermion line, moving a propagator momentum past a polarization gives the ordinary Clifford identity

$$
(\not{P}+\not{k}+m)\not{\epsilon}
 = 2P\cdot\epsilon+\not{k}\not{\epsilon}
   -\not{\epsilon}(\not{P}-m).
$$

The last term either vanishes on an external spinor or cancels the adjoining fermion propagator. Combining its two emission orders gives the definite contact contribution proportional to $-2\epsilon_i\cdot\epsilon_j$. The spin term acts on one chirality and produces a spinor of the emitted photon. These relations let the code sum over subsets of photons attached to each end of a charged line and reuse the resulting spinors. With two charged lines it sums these placements before contracting the two currents at the exchanged photon. With additional lines it also retains emissions between two exchanges. All intermediate electron momenta remain off shell. The assembled candidate must agree with every signed lower-point residue before the constructor accepts it. The [focused C++ regression](radiative_current_tests.cpp) also tests identical-fermion exchange directly.

### Compare with the publication catalog

From the repository root, run the automatic construction and its signed benchmarks with

```sh
tests/results/native-reduce-build/hepcat-qed --root . --publication --construct-only
```

The test reads the signed expressions from publication commit `aa676a9` after selecting the validation reference for each helicity. Candidate generation uses only the vertices and lower-point amplitudes. The publication comparison requires at least 100,000 points, uses seed 861811 by default and fails if the automatic expression misses a complexity target. Each result records the weighted score, syntax tree size, wall time and check counts.

| Photon helicities | C++ physical | C++ spurious | Best published physical | Best published spurious |
| --- | ---: | ---: | ---: | ---: |
| Three photons, one opposite | 62 | 63 | 62 | 63 |
| Four photons, one opposite | 327 | 416 | 327 | 416 |
| Four photons, two of each | 692 | 712 | 692 | 712 |
| Four fermions and two opposite photons | 1,600 | 1,632 | 1,600 | 1,632 |
| Five positive photons | 820 | 920 | 820 | 1,160 |

The [publication reference and reproduction instructions](../tests/benchmarks/publication-qed/publication-catalog.md) identify the model conventions and the separate, earlier worked expressions. The [full comparison](../tests/benchmarks/publication-qed/complexities.tsv) gives the Wolfram-measured score and leaf count for each representation. On these 42 entries, 31 native weighted scores equal the validated catalog and 11 are lower. Ten native expressions have more Mathematica leaves despite meeting their weighted targets.

### Run the four-point Standard Model notebook in C++

The [SM four-point example](../tests/SM-4-point-construction.cpp) starts each construction from model 1's elementary interactions. It uses the notebook's all-incoming ordering for construction and its two incoming and two outgoing species for numerical export. The default inventory includes the three generations of fermions with the same diagonal flavor mixing as the notebook. Use `--notebook-only` to run precisely its 138 saved calculations in 78 export families. Use `--list` to see the family names.

```sh
cmake -S csource -B tests/results/native-reduce-build -DCMAKE_BUILD_TYPE=Release
cmake --build tests/results/native-reduce-build --target hepcat-sm-four-point
tests/results/native-reduce-build/hepcat-sm-four-point --family sm01_uUCc
tests/results/native-reduce-build/hepcat-sm-four-point
```

Each family produces the reduced expressions, their weighted complexity and syntax tree size, and a report for each check. The signed notebook expressions enter only at final validation. The constructor checks physical and spurious poles, fixed-angle growth and reference independence. By default it compares the amplitudes at 100,000 points against the saved expressions and the independent Feynman diagrams. SPINAS then runs the unpolarized CalcHEP comparison and both the polarized and summed MadGraph comparisons. A nonzero exit code indicates a failed check, an incomplete family or an expression exceeding either the notebook’s weighted complexity or its leaf count. The [validation inventory](../tests/benchmarks/native-sm-four-point/README.md) explains how the notebook and generation-expanded reference cases were obtained.

Each family directory contains one `.expr` file per constructed amplitude, `checks.txt`, `calchep-comparison.log`, and `madgraph-comparison.json`. For example, the first notebook amplitude is saved as `sm01_uUCc/sm01.expr` under the displayed run directory. Its parent contains `scores.tsv` for the whole run. The runner removes successful temporary builds after recording their results. When a precision replay is necessary, it retains the original residuals as well as the replay outcome.

Run from the repository root or pass `--root DIR`. By default the executable looks for the three external programs alongside HEPCAT. Set `--spinas DIR`, `--calchep DIR`, or `--madgraph DIR` to use another installation. Supply `--seed N` to repeat a draw. Each invocation prints its run directory under `tests/results/spinas-output/SM-4-point`, the same output area used by the Mathematica notebook. With `--construct-only` it runs the native checks and skips the two external programs.

For a four-fermion exchange, the native constructor can recognize a cubic chiral current and contract it with the massive vector spin projector before expanding its momenta. The resulting numerator keeps the transverse contraction and the product of current divergences together. Each divergence simplifies against its external fermion equations. The original sewn expression remains available to factorization completion through the discarded remainder.

For four massive vectors, the final reduction proposes shorter spinor tensors on each physical pole, then fits the remaining local term to the complete amplitude at generic kinematics. A candidate must also agree on 24 independent exact rational samples away from the poles. Colored amplitudes use an exact SU(3) tensor basis. The constructor completes each independent coefficient, recombines them, and validates the full amplitude including color interference. Once the color terms are back together, the four-point reducer factors common products, recognizes exact binomial squares, and tries collecting over existing propagators. It ranks equivalent candidates first by momentum-weighted complexity and then by expression size. This can shorten a printed formula even when the weighted score is already minimal among the candidates.

## Generate from HEPCAT model files

A native program can start with particle names. Read each model once and reuse it across processes. The QED vertex model is number 3. For factorization through nine external legs, use model 4 for physical poles or model 5 for the spurious-pole representation. Use models 1 and 2 for the corresponding SM pair.

To read a catalog entry directly, supply the ordered all-incoming particle names, including photon helicities:

```cpp
hepcat::model catalog("models", 4);
auto amp = catalog.retrieve_amplitude({"e", "E", "A.+", "A.-"});
std::cout << amp.plain_text() << '\n';
```

The result owns its expression and external masses. It includes the stored factor and uses the requested leg numbering. Repeated species keep their occurrence order. An absent row gives the zero amplitude with the requested external masses. A complete model only needs to list its nonzero amplitudes. Exact row order takes priority over multiset matching. An ambiguous match or a Feynman model throws `std::invalid_argument`.

Large amplitude rows can store shared arithmetic in dependency order. For example, `Shared[<13>;EE;*:1,2;+:3,3]` computes the spinor bracket and coupling, multiplies them, then adds the product to itself. Slot numbers start at one. The last slot is the amplitude. Both readers return ordinary HEPCAT expressions with model-local parameters.

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

Automatic constructive references use a cycle of massless external particles when available and a massive spectator for a lone photon. Pass `hepcat::generation_references::keep` to keep explicit references or `hepcat::generation_references::physical` to request physical denominators where a suitable spectator exists. For a bare three-point vertex with a massless vector and an equal-mass pair, both momentum references are singular. The native generator retains the model's cubic x or x-tilde factor in that case. A factorization product is defined on its own pole. Its continuation can differ from the Mathematica representative while giving the same residue.

For `e,E,A.+,A.+` and its negative-helicity partner, choose `generation_references::physical` for end-to-end completion. A four-point determinant identity removes the extra momentum insertions on each numerator's own shell. Both helicities then give a single term with two physical propagators and pass the high-energy check. The automatic cyclic choice for the positive-helicity pair still leaves its high-energy correction unresolved.

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

On the eight-worker release build, the electron pair with four positive-helicity photons took 1.02 s with one worker and 0.32 s with eight for its 24 diagrams and 14 channels. Peak RSS went from 5.7 to 12.1 MiB. For four W bosons the corresponding times were 8.03 s and 2.51 s, with 15.1 and 37.3 MiB peak RSS. These timings cover topology generation, sewing and reduction after loading the models. The [measurements for all eight processes](generation-benchmark-results.json) also report model read time and Wolfram raw generation time separately.

## Validate the final amplitude

Give the process its elementary model, lower-point amplitude model and, optionally, a separate CalcHEP model. Here the first two external legs are the incoming electron and positron. The stored analytic expression uses all momenta incoming.

```cpp
#include "amplitude.hpp"
#include <iostream>

int main() {
    hepcat::model vertices("models", 3), channels("models", 4);
    hepcat::model rules("tests/reference-models/calchep", 2);
    hepcat::amplitude amp({"e", "E", "A.+", "A.+"}, vertices, channels, rules,
                          hepcat::generation_references::physical);
    amp.generate();
    amp.complete_high_energy();  // Includes factorization and final external-only reduction.
    auto report = amp.validate();
    std::cout << "Validation seed " << report.seed << '\n';
    for (const auto& check : report.checks)
        std::cout << check.kind << " " << check.label << ": " << check.detail << '\n';
    for (const auto& check : report.numerical)
        std::cout << check.kind << ": " << check.compared << " points, " << check.failures
                  << " mismatches, max relative error " << check.max_relative_error
                  << " " << check.detail << '\n';
    if (!report.passed()) return 1;
    std::cout << amp.final_amplitude().str() << '\n';
}
```

`generate()` schedules the independent Feynman diagrams alongside the constructive work. `amp.feynman_diagrams()` returns the raw tensors. To attach the model to an existing process, call `amp.set_feynman_model(rules)`. Each Feynman tree uses off-shell internal momenta. Its Dirac and Lorentz contractions are performed once per validation before the numerical loop. In the optional Mathematica bridge, pass `"FeynmanModel" -> {directory, number}` to `GenerateNativeAmplitude` and inspect `batch["FeynmanDiagrams"]`.

Physical residues use the signed lower-point products on their cuts. The checker searches the expression for other kinematic denominators and tests their cancellation. Fixed-angle checks examine the powers of energy above the bound. These checks run on dedicated kinematic charts. The separate numerical sweep draws **10,000 distinct conserved scattering points by default**, with two incoming particles, the model masses, fixed specified helicities and sampled coherent massive-spin states. Colored numerical amplitudes share the same external colors. A remaining reference spinor is varied with momenta and spin states held fixed. When the final reduction changes the expression, the sweep also compares its values before and after reduction. A sweep with only a finiteness check is identified as `FiniteAmplitude`.

Use `amp.validate(candidate, options)` to check a proposed result directly. This overload accepts a mass-tagged `amp_expression`. It leaves the completion result available. Options include `points` (at least 10,000), `workers`, `seed`, `relative_tolerance` and `absolute_tolerance`. Both models evaluate their numerical parameters from their own files. Use `options.parameters["Me"]` for a common override or the complete scoped name for a model-specific one. The residue and energy charts use exact parameter assignments, independently configurable through `exact_parameter_points`. Both tolerances apply to the complex amplitude itself. The defaults are relative tolerance `1e-8` and absolute tolerance zero. Every out-of-tolerance comparison is repeated using 160-bit GMP arithmetic. The replay regenerates the physical momenta, spins and references from the original random draws, resolves supported model relations at that precision and reevaluates the signed difference. This matters near cancellations where rounding the external kinematics alone can exceed the comparison tolerance. The report keeps both sets of maxima, the replay count, the first raw outlier seed and the first unresolved or confirmed failure seed. The separate `samples_per_cut` and `energy_directions` options control the dedicated charts. A mismatch, unsupported operation or incomplete point count prevents a passing report. Real scattering samples require four or more external legs.

The independent comparison matters for contact interactions. For example, adding a constant to the four-Higgs result preserves its residues and allowed high-energy power. The Feynman comparison detects the shift. For external quarks, antiquarks and gluons the dedicated checks project the SU(3) tensor onto scalar components. Exact contractions in a rescaled Gell-Mann basis preserve vanishing external color components in the numerical comparison. An exact check outside the supported color basis reports `unresolved`. Raw Feynman diagrams use the tree-level propagators with zero widths.

The mixed five-point QED, four-W and ZZWW end-to-end tests carry the CTest label `integration`. Run `ctest -L integration -V` in the build directory to see the pre- and post-reduction expression sizes, numerical errors and replay counts. The [Wolfram comparison](validation_feynman_bridge.wls) checks the signed conversion of individual native Feynman diagrams, including four-gluon and four-W diagrams.

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
