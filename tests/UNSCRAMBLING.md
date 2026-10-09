# Amplitude Unscrambling

## Files

- `unscrambling.nb`: interactive experiments.
- `SM-4-point-unscramble-test.nb`: diagram-to-reference benchmark for the 73
  cases in the old SM test notebook, without its process-specific rewrites.
  Uses all-ingoing particle lists and green PASS/red other statuses. Tests all
  24 label permutations by default (3,552 checks). Set
  `particlePermutations = {Range[4]}` for identity-only runs. A permutation maps
  old labels to new labels in inputs, mass rules, and references together.
  Set `unscrambleModel` at the top to `"CandidateScorer"` or `"CompactActions"`
  to select the network for all diagrams, channel sums, and permutations.
- `unscrambling.wl`: the complete package, with foldable sections for identities,
  candidates, encoding, neural scoring, training, and inference.
- `unscramble.wlnet` and `unscramble.m`: the candidate scorer network and metadata.
  Training replaces these files by default. Git keeps their history.
- `unscramble-compact.wlnet` and `unscramble-compact.m`: the new compact
  legal-action network and metadata, kept separately for comparison.
- `train-unscrambling.nb`: training parameters and corpus selection.
- `training-amplitudes.wl`: the reference corpus copied from both SM notebooks,
  with external mass rules and source references.
- `train-unscrambling.py` and `train-unscrambling.wl`: supervised headless entry points.
- `training-notebook-helpers.wl`: settings and readable report formatting.
- `checks/`: automated regression tests and their runner.

## Mathematica

Load HEPCAT, then `unscrambling.wl`, as in the notebook. The package uses the
model alongside it. Reload with `Get` after replacing model files to clear the
in-memory model. No model-directory setting is needed for normal use.

```wolfram
scrambled = ComplicateAmplitude[pair, 4, 1];
result = UnscrambleSpinorAmplitudes[{pair[[1]], scrambled["Expression"]}];
SeedRandom[1];
trace = UnscrambleTrace[{pair[[1]], scrambled["Expression"]}];
trace["Result"]
trace["Checkpoints"]
trace["TerminationReason"]
```

Choose either model explicitly:

```wolfram
UnscrambleSpinorAmplitudes[pair, "Model" -> "CandidateScorer"]
UnscrambleSpinorAmplitudes[pair, "Model" -> "CompactActions"]
UnscrambleTrace[pair, "Model" -> "CompactActions"]
```

`"CandidateScorer"` scores legal edits using the original expression and their
symbolic before/after edit fragments. `"CompactActions"` uses the new network
trained on reverse scramble trajectories. The compact network reads each state
once and scores legal edits without the state-only predictor's 512-action limit.
New training uses variable-length numerical edit tokens: every member of a
momentum subset and every ordered momentum in the focused chains is represented,
including particle 5. There is no fixed external-particle ceiling. Arbitrary
integer labels are normalized consistently with the state encoding, and zero
padding is masked before pooling. This removes a representation restriction;
it does not establish that a trained model generalizes to unseen higher-point
amplitudes.

The installed compact weights now use particle tokens. They are a short,
fresh-weight pilot trained for 20 rounds on 187 expression states, not a
replacement quality benchmark for the longer four-point training. Reload the
package after replacing weights. Older four-point compact weights remain
loadable with their original 146-number descriptors through `"ModelDirectory"`;
reloading them does not change their encoding. Both formats use the same
`"CompactActions"` selector and filenames.
Metadata selects the appropriate encoder and checks the legal-identity and
encoding source sections for compatibility.
Wolfram's compiled checkpoint has a fixed token axis for training, recorded as
`NativeTokenCount`. Everyday particle-token inference uses the shared saved
weights with variable token and action counts; it does not truncate to that
training shape. Native/portable agreement is also checked with enlarged padding.
Compact inference evaluates the saved weights using packed CPU matrix
operations, avoiding the native convolution runtime that stalled on this Mac.
The numerical path was checked against the original server network.

`training-amplitudes.wl` contains the 195 original notebook references plus
48 synthetic five-point algebraic fixtures. The added examples span six mass
families and eight templates: bracket products, one- to three-momentum chains,
dot-product sums, and internal momentum chains/squares. They are identity
training fixtures, not assertions about physical Standard Model amplitudes.
Their five external labels all occur in the expressions; optional internal
on-shell channel rules are explicit. Existing particle-permutation augmentation
also applies to these examples.

The package default remains `"Model" -> Automatic`, which preserves the
existing `"PolicyMethod"` option. An explicit model name takes precedence over
`"PolicyMethod"`. `"ModelDirectory"` works with either choice. There is no
automatic fallback or ensemble, and switching models loads the selected
weights even within the same Mathematica session.

The Load cell in `unscrambling.nb` sets `unscrambleModel = "CompactActions"`
for its experiments. Change that line to `"CandidateScorer"` and reevaluate
the cell to compare. `SetOptions` can also set either function's default independently:

```wolfram
SetOptions[UnscrambleSpinorAmplitudes, "Model" -> "CompactActions"];
SetOptions[UnscrambleTrace, "Model" -> "CompactActions"];
```

Input and output are `{massRules, amplitude}`. Inference retains every strict
complexity improvement. Defaults are `"MaxSteps" -> Infinity`, `"Attempts" -> 5`,
`"MaxStagnantSteps" -> 10`, `"TimeLimit" -> 60`, and `"Kernels" -> Automatic`. Productive attempts no longer
end at a fixed step count; a nonnegative finite `"MaxSteps"` is still available
for controlled tests. In parallel mode each kernel explores one route. Every
strict new global best cancels all outstanding edits and restarts every route
from that expression, including routes whose attempts had already finished.
Opening moves use different NN-ranked non-Stop choices where possible; subsequent
choices use independent random streams. One route follows the greedy policy on
its first attempt. Sampled routes span cautious to exploratory temperatures
(0.5 to 2 times the selected policy's sampling scale), not just different opening moves.
Each route avoids returning to its own previously visited states during an attempt,
including immediate undo moves. Those histories reset on a fresh attempt or group restart.
Routes share counts of explored state/move pairs: sampling gently downweights repeated
choices, and a duplicate choice already explored by another route is retried at most
three times. Rejected choices do not consume edit or stagnation limits, but do consume
wall time. Overlap is allowed when choices are scarce or retries are exhausted;
reaching another route's state by itself is not forbidden. No hard top-k filter
removes lower-ranked legal choices. If only one edit exists, routes can share that
edit and diverge when more choices become available. Stagnation resets at the start of each attempt and
on improvement. Changed, score-neutral `MomentumSum` splits neither increment
nor reset the counter: unpacking a composite momentum is not a stalled edit.
Neutral collection, no-op moves, and other non-improving edits still count.
Parallel `"TimeLimit"` includes normalization, model loading, worker startup,
and all group restarts. Cleanup may take additional time. Wolfram's internal
time constraint cannot reliably interrupt a stuck native library.

`"Kernels" -> Automatic` uses all available local logical CPUs, limited by the
remaining parallel-kernel license capacity. On Linux the CPU count honors process
affinity; on macOS it uses the available logical CPU count. If the operating-system
query is unavailable, it falls back to Wolfram's `$ProcessorCount`.
Use `"Kernels" -> 2` or another positive integer to reduce concurrency;
`"Kernels" -> 1` retains the original sequential attempts. These are separate
Wolfram kernels, not the native numerical thread count of one kernel. Inference
launches and closes its own pool without sending work to or interrupting kernels
already connected by the caller. Explicit counts above available capacity are
reduced with a message. Each worker loads the selected model once, so more workers
also use more memory. `RandomSeed -> integer` fixes route random choices, but
asynchronous completion order can still change which improvement is adopted first.

```wolfram
result = UnscrambleSpinorAmplitudes[pair,
  "Model" -> "CompactActions", "Kernels" -> Automatic,
  "MaxStagnantSteps" -> 100, "TimeLimit" -> 600];
```

`"ProgressReporting" -> Automatic` displays one temporary live panel in
Mathematica notebooks, without accumulating print cells. Parallel mode shows
the shared best term counts and complexity, group restart count, elapsed time,
and a row per route with its kernel ID, state, attempt, greedy/temperature setting,
total edits, skipped duplicate/loop choices, current terms and complexity,
patience counter, and accumulated edit wall time. Sequential
mode shows the original attempt, current/best counts, neutral splits, and elapsed time.
The display reads cached numbers rather than repeatedly traversing an amplitude. Use
`"ProgressReporting" -> False` to disable it or `True` to request it explicitly.
Headless runs remain silent. `UnscrambleTrace` also returns these final metrics
under `"Progress"`. The two unscrambling notebooks expose `maxSteps`,
`maxStagnantSteps`, and `liveProgress` together in their setup cells.

## Identities and Network

The symbolic engine enumerates legal edits plus Stop. The network scores those
edits; it does not invent transformations. Candidates include generalized
Schouten, endpoint mass identities, momentum conservation, and momentum-square
and on-shell identities. Coverage is extensible, not exhaustive.

Interior chains also support `ChainSquare` (two adjacent identical momenta)
and `Anticommutation` (swap adjacent distinct momenta and generate the shorter
dot-product term). These preserve endpoint chirality and little-group indices
and act on one chain factor at a time, including positive powers. External
squares use supplied masses; internal squares use masses only for enabled
on-shell channels, otherwise retaining the momentum square.

Reverse square insertion uses nonzero-mass external legs at the start of a
chain, and only when that chain has no adjacent repeated momenta. This bounds
the inverse sites and avoids division by zero. Anticommutation reverses through
another swap plus cancellation. Scrambling and inference share these moves;
training still checks that each reverse target is reachable. The candidate scorer
can score the new serialized edits, but has not been trained on these move
types yet. Retraining is required to learn their use; old generation caches
are invalidated by the source fingerprint. Search limits are unchanged.

`MomentumSum` edits split a composite momentum inside a chain, introduce
`p_a = P_{a,b} - p_b`, or collect matching chain terms back into a composite
momentum. Larger subsets split recursively, without enumerating every subset.
Explicit sums and scalar multiples in momentum slots distribute multilinearly,
preserving the order of the other insertions. These are momentum definitions,
not on-shell assumptions, so they work with momentum conservation disabled.
Propagator arguments and inverse spinor chains are not rewritten.

The complexity score counts the terms implicit in composite momentum slots.
A chain with `Mom[Multiparticle[1,3]]` counts as two expanded chain terms;
two independent two-member slots count as four. The count also weights the
momentum insertions and spectator factors repeated in those implicit terms.
It uses arithmetic branch counts, without constructing the expanded amplitude.
Splitting alone is therefore score-neutral unless it exposes like-term
collection or cancellation; subsequent contractions can lower the score sooner.
Complete `PropDen` factors remain opaque to this score. Changed, equal-score
`MomentumSum` splits are exempt from `"MaxStagnantSteps"`; other equal-score
edits still consume patience. This changes checkpoint acceptance and search
control, not the neural weights or edit
features; existing CompactActions models remain loadable without retraining.

Identical endpoint spinors in a two-spinor bracket normalize to zero, including
zeros produced by Schouten. Endpoints must match completely: different
little-group indices on the same particle are not discarded.

Scalar momentum conservation now supports any number of external legs:
`p_i . p_j = -m_i^2 - Sum[p_i . p_k, k != i,j]`. Complete scalar rows can
also be collected. Internal on-shell mass rules can name any distinct subset
of two or more particles. The corresponding numerator square can be expanded
into external masses and pairwise products, collected again, or replaced with
the enabled internal mass squared. `"OnShellChannels" -> {}` disables these
internal on-shell replacements without disabling algebraic sum expansions.

Even-length `SpinorTrace` factors support momentum linearity, composite
splitting/collection, adjacent contraction, anticommutation, momentum
conservation, and two-momentum trace/scalar-product conversion. Empty chiral
traces normalize to two. Boundary contractions preserve trace chirality;
odd-length traces are left unsupported. All these operations treat complete
`PropDen` expressions as opaque. Hats and Zetas are deliberately outside this
extension. The new trace and subset descriptors keep the existing feature
widths, but existing weights have not been trained on this added coverage.

The same moves generate scrambles and exact reverse training targets. Compact
descriptors reuse the existing momentum-rewrite feature block, retaining the
146/166 input widths. The known earlier compact models remain loadable, but
their weights have not learned these new moves. New training uses a new source
fingerprint and regenerates cached examples. A running isolated training job
continues using its snapshot; it does not pick up these changes mid-run.

`SchoutenRewrite[c1, c2, {m, n}]` implements Decay Rates, Appendix B, Eq. (B11).
The split integers count preceding momentum insertions. Incompatible chirality
returns `$Failed`; endpoint spin indices and momentum order are preserved.
`SchoutenCandidates[expr]` enumerates chain-factor pairs in expanded terms,
including positive powers and spectators. It does not rewrite inside inverse
spinor chains or arbitrary function heads.

`"OnShellChannels" -> Automatic` infers internal on-shell channels from mass
rules. `"MomentumConservation" -> True` enables momentum conservation. Use
matching physical assumptions for training and inference. Disabling on-shell
conditions changes legal identities, not just performance.

The candidate scorer encodes the full expression, mass rules, and conditions
once, then scores each encoded edit against that state. UTF-8 FullForm encoding
preserves heads, argument order, coefficients, denominators, powers, and spin
indices. Particle labels are normalized only in particle positions. There is
no fixed leg count, candidate cutoff, or byte truncation. Original packet tags
are retained because they are trained input bytes, not model release selectors.

Each encoder uses an embedding and GRU. Edits are left-padded with reserved ID
257; padding is learned, not masked, and can affect scores. Training pads to the
dataset's longest edit and restores variable-width inference afterward. Runtime
and memory grow with expression length and candidate count. Higher-point inputs
are supported, but generalization must still be measured.

`TrainUnscrambleNet[..., "MaskPadding" -> True]` enables an experimental masked
candidate GRU. Reserved token 257 leaves the hidden state unchanged, before and
after training. Its fixed mask is not learned. This mode requires retraining;
it does not modify existing models, which remain loadable. The metadata records
the encoder architecture in `unscramble.wlnet` and `unscramble.m`.

Masking is a correctness option, not yet a speed optimization: the explicit
recurrent implementation is slower on the initial real-data benchmark, so the
default remains False. Training still uses one uninterrupted NetTrain call and
fixed edit width, preserving optimizer state. Variable-width bucket training is
not enabled: the masked fold hit a backend error with varying sequence lengths,
and separate calls per bucket would reset Adam state. Numerical padding,
training, and serialization checks run with `run-wolfram-tests.py --neural`.

`UnscrambleCandidates[pair]` exposes legal edits. `UnscrambleEncoding[pair, edit]`
returns complete `"State"` and `"Candidate"` sequences; decode either with
`FromCharacterCode[sequence - 1, "UTF8"]`.

## Training

The corpus contains 195 distinct amplitude/mass-rule pairs from 337 explicit
reference occurrences in `SM-4-point.nb` and `SM-4-point-test.nb`. The latter
contributes references for 73 named cases. Entries include individual channels,
channel sums, helicity choices, and alternative forms, not 195 different physical
processes. Exact duplicates share source references. Scalar references are kept.
Original mass/coupling conventions are preserved, including `Mt` versus `Mtp`;
copying the corpus is not an independent physics validation. No additional
internal on-shell assumptions are imposed. Diagram-only comparisons and unfinished
derivations without an explicit reference formula are not new training targets.

`--amplitudes all` selects the complete corpus and is also the notebook default.
A positive integer selects the first N entries. The current saved network does
not change until another training run completes. Larger expressions can cost
much more than the old toy seeds, so timing must be measured again.

Edit the first input cell of `train-unscrambling.nb`, or use the launcher from
the repository root (an absolute script path also works):

```sh
python3 tests/train-unscrambling.py --threads 4 --kernels 2 \
  --amplitudes 4 --scrambles 24 --steps 5 --rounds 2 --holdout 8
```

The launcher evaluates notebook text Input cells without a front end. Other
controls include `--episode-length`, `--batch-size`, and `--worker-threads`.
`--dry-run` prints configuration without starting Wolfram.

### Particle Numbering

The encoding sorts particle labels and maps them to consecutive integers. This
handles sparse labels but is not invariant under arbitrary label permutations.
Each training scramble now receives a deterministic random permutation of its
particle labels before its trajectory is generated, including a relabeled Stop
example. Spinors, momenta, invariants, external/internal mass rules, and explicit
on-shell channel settings are relabeled together, never bare integer coefficients
or spin indices. Seeds are independent of worker assignment and do not change
the caller's random state. Original jobs retain the input numbering; job metadata
records each permutation. Source fingerprints invalidate pre-augmentation caches.
This is a change of notation, not a physical crossing operation
or an extra fermion sign. Four legs permit 24 permutations, but adding all of
them as stored examples multiplies training cost and can overweight duplicates.
Current holdouts retain their original numbering and still test fresh scrambles
of the same starting amplitudes. For unseen-process evaluation, split by starting
process before augmentation to avoid leakage. The separate diagram benchmark
now tests all 24 label permutations. Augmentation encourages
robustness; it does not guarantee mathematical permutation equivariance.

Training learns reverse scramble trajectories. Predecessors are positive targets
only when legal candidates can reach them; unreachable steps are skipped and
reported. Originals supply Stop examples. Loss weights balance positive and
negative labels. One batch item is a full state and its candidate set. Neural
optimization runs on the coordinator, not on every worker.

### Curated Simplification Routes

`training-amplitudes.wl` also defines `trainingSimplificationRoutes`, separately
from its returned reference catalog. Seeds generate random reverse-scramble
examples; curated routes provide worked difficult-to-simple examples. The
training run combines their rows only after both kinds of jobs are generated.
The first worked route is the actual `e, E, M, m` Z-exchange numerator, based on
the handwritten derivation at the end of `SM-4-point-unscramble-test.nb`.
Its 96 edits cover composite splitting, mass contractions, both handwritten
Schouten blocks, anticommutation, numerator on-shell identities, and momentum
conservation. The short notebook derivation batches many of these operations.
Regression checks compare its first state to `diagramAmplitudes` followed by
`ReduceSpinContractions`, and its final state directly to the reference with
the electroweak convention `MW == MZ CW`. That scalar convention is only a
comparison substitution, not a training action. Every state keeps the original
propagator.

Three further routes cover `e, E, Z, h`: Z exchange (52 edits), electron
exchange in `(1, 3)` (10 edits), and electron exchange in `(1, 4)` (4 edits).
Their initial states are checked against freshly generated diagrams with
`ReduceSpinContractions`, and their endpoints against the accepted channel
references. These endpoints can retain useful single-particle momentum chains.
The final angle Schouten identity uses the sign consistent with bracket
antisymmetry: `<23>[341> = <13>[342> + <21>[343>`. The source notebook is not
modified.

Two routes cover the electron exchanges for `e, E, Z, Z`, matching the separate
`ampffZZfTTested` and `ampffZZfUTested` targets in `SM-4-point-test.nb`. Each has
four edits: split the two composite momentum chains, then contract their
leg-3 or leg-4 momenta against the endpoint spinors. The remaining leg-1
insertions belong to the accepted final expressions and are retained. The
Higgs channel already agrees at the diagram boundary and needs no worked
route. Initial canonical normalization applies the accompanying mass rules,
including scalar internal masses reintroduced by `ReduceSpinContractions`;
it never puts the propagator denominator on shell. All 192 new transitions
across all 24 label permutations are checked against complete legal move sets.

Writing `y[i,q,j] = <i|p_q|j]`, the `(1,3)` channel uses these four edits:

1. `y[3,Multiparticle[1,3],4] -> y[3,1,4] + y[3,3,4]`.
2. `y[4,Multiparticle[1,3],3] -> y[4,1,3] + y[4,3,3]`.
3. `y[3,3,4] -> MZ [34]`.
4. `y[4,3,3] -> MZ <34>`.

The `(1,4)` channel uses:

1. `y[3,Multiparticle[1,4],4] -> y[3,1,4] + y[3,4,4]`.
2. `y[4,Multiparticle[1,4],3] -> y[4,1,3] + y[4,4,3]`.
3. `y[3,4,4] -> -MZ <34>`.
4. `y[4,4,3] -> -MZ [34]`.

These are numerator edits in the full expression, with its scalar coefficients
and propagator carried unchanged. Canonical normalization can order the two
equivalent split edits differently; every recorded intermediate state remains
an individual legal candidate application.

The `e, E, W-, W+` Z-exchange channel supplies a genuinely unresolved diagram
example: the current CompactActions model retained complexity 376 under a
60-second, single-worker test, while the accepted reference has complexity 22.
Its reconstructed route has 302 elementary edits and four stages:

1. Split 32 composite insertions and apply 40 endpoint mass contractions:
   complexity `376 -> 216`.
2. Apply 8 Schouten, 8 anticommutation, 16 mass, and 8 numerator on-shell edits:
   complexity temporarily rises to `232`. Here `p1.p2 = (MZ^2 - 2 Me^2)/2`.
3. Transfer remaining insertions towards a `p3` chain between the fermion legs,
   using 42 momentum-conservation, 16 Schouten, and 100 mass edits:
   complexity reaches `182`.
4. Apply 16 locally neutral Schouten rewrites and their 16 endpoint mass
   contractions, exposing distant cancellations: complexity reaches `22`.

The basis preference is a deterministic choice inside this particular teacher
recipe, not a change to inference scoring or expression complexity. The full
route begins at a freshly checked diagram boundary and ends exactly at
`ampffWWZTested`, without a trained network, a black-box spinor reducer, or an
electroweak coupling substitution. Its original `PropDen` survives every edit.

Two further routes cover W exchange for `ne, E, W-, h` and `u, D, W-, h`.
Both use the `ampffWhWTested` reference in `SM-4-point-test.nb`, with the
appropriate massless-neutrino or massive-quark spinors. The present model
missed their endpoints in seeded single-worker checks: the neutrino case
reached complexity 29 within 30 seconds, and the quark case selected Stop
at 21. Their accepted endpoints have complexities 6 and 11, respectively.
The recorded routes have 18 and 26 elementary edits:

1. Split the composite momenta and contract endpoint insertions.
2. Apply Schouten to join the remaining momentum-chain product, then
   anticommutation and mass contractions.
3. Use `p1.p2 = (MW^2 - m1^2 - m2^2)/2` in the numerator only.
4. Use Schouten, momentum conservation, and endpoint contractions to expose
   the cancellations and leave the reference's `p4` insertion.

The quark route temporarily reaches complexity 59 and takes 17 edits before
beating its earlier best complexity. These intermediate states teach non-Stop
actions even when an immediate complexity decrease is unavailable; this does
not change inference's patience setting. Every original edge is checked
against the public candidate set, and every application is replayed under all
24 particle permutations. Both endpoints match their notebook references
exactly, with the original propagator unchanged.

The bosonic `Z, h, W-, W+` Z-exchange channel adds 172 elementary edits,
reducing complexity `188 -> 14`. Its source helper `simplifyZhWWChannel`
eliminates `p2`, contracts mass insertions, and applies two Schouten identities.
The recipe decomposes those operations into 16 composite splits, 108 mass
contractions, 28 momentum-conservation edits, 12 Schouten edits, 4
anticommutations, and 4 numerator on-shell edits. The last four use
`p3.p4 = (MZ^2 - 2 MW^2)/2`, justified by the diagram's accompanying mass
rules. No propagator is put on shell. The original notebook explicitly accepts
this channel as `PASS (opposite overall sign)`: the route preserves the fresh
diagram's sign and ends at `-ampZhWWSTested`, with `"TemplateSign" -> -1`
recording that comparison convention. It never applies a sign-changing edit.
All 24 label permutations replay the 172 edits with unchanged propagators.
Fresh diagram comparisons sometimes use the complementary orientation of
the same internal cut (`q -> -q`); this is normalized only for comparison,
never by altering a recorded state or adding an on-shell denominator rule.

Five additional paths target channels the current CompactActions model did not
finish in bounded 25-30 second probes:

- `u, D, T, b`, internal W exchange: 24 edits, complexity `37 -> 5`.
- `E, ne, Z, W-`, internal W exchange: 109 edits, complexity `188 -> 11`.
- `U, d, Z, W+`, internal W exchange: 155 edits, complexity `188 -> 13`.
- `Z, h, W-, W+`, T- and U-channel W exchange: 72 edits each, complexity `188 -> 19`.

These paths use composite-momentum splits, mass contractions, Schouten,
anticommutation, and numerator on-shell identities; the fermion paths also use
momentum conservation. They begin at freshly generated diagram amplitudes plus
`ReduceSpinContractions`, and their endpoints are checked independently against
the reference notebook. The two bosonic W channels retain its explicitly accepted
opposite overall sign, recorded as `"TemplateSign" -> -1`, without any sign-changing
edit. No new process has an external photon or gluon. Every recorded propagator
is unchanged throughout its path. All 24 particle-label permutations regenerate
the corresponding legal identities, rather than only replaying relabeled edits;
the unpermuted paths are also checked against complete public candidate boards.

With `"RoutePermutations" -> All`, the fifteen routes supply 360 route jobs,
alongside the independently generated random scrambles.
Further successful hand or teacher routes can join this list.

The larger isolated server run stores the training descriptors as lossless
zero-background sparse arrays, restoring one packed dense row at a time for
the network. This changes storage only, not legal choices, targets, labels,
or the trained model architecture. It retains all route permutations without
keeping every mostly-zero dense tensor in memory. Full legal-board route
validation remains mandatory before neural training starts.

The route builder records elementary identities, not calls to `Collect`,
`Simplify`, `ReduceSpinorProducts`, or a trained network. Notebook shorthand
such as `rsp` can hide additional reductions; these must also become individual
legal edits. Ordinary expansion and collection of identical polynomial terms
are the same canonical normalization used by every training state. The worked
route is training data, not an independent validation case.

Pass `"SimplificationRoutes" -> trainingSimplificationRoutes` to
`TrainUnscrambleNet`. The training notebook enables it by default; set
`simplificationRoutes = {}` after loading the catalog to disable it. Each route
is an association with `"Name"`, `"Masses"`, and `"States"` ordered from difficult
input to accepted endpoint. Optional `"OnShellChannels"` and
`"MomentumConservation"` override the training conditions for that route.

Every adjacent state must be the normalized result of one current legal edit.
Malformed, illegal, or timed-out curated routes fail the job instead of silently
dropping their labels. Temporary complexity growth is allowed, and only the
final state receives Stop. If a random-scramble reference teaches Stop at a
curated intermediate state, the curated non-Stop target takes priority.
States, mass rules, and explicit channels are permuted together; route contents
and conditions participate in the existing job-cache keys.
`"RoutePermutations" -> All` enumerates every particle-label permutation:
24 for a four-particle route and 120 for a five-particle route. Exhaustive
augmentation is limited to five particles to avoid unbounded factorial growth;
larger routes can use an integer sampling count. Integer values preserve the
existing seeded sampling: `4` draws four relabelings plus the original numbering,
independently of `Scrambles`, and duplicate permutations are removed. Set it to
zero for original numbering only. These generated permutations and random
scrambles are not added to the source catalog. Optional job caching and exported
run datasets
are separate from the curated source recipes. A route endpoint's Stop target is
also suppressed if another curated route teaches a legal continuation from
that same state.

The operational CompactActions particle-token runner uses the same verification
and route jobs. It defaults to the catalog's routes with four extra label
permutations. `UseSimplificationRoutes: false` disables them in its JSON config;
`RoutePermutations: "All"` enables exhaustive coverage, while an integer keeps
seeded sampling. `CuratedTrainingFraction: 0.2` targets approximately 20% curated
updates using finite epochs that retain every broad and curated row. Only the
underweighted category is repeated; the unpadded corpus tensors are not copied.
Progress and checkpoint metadata report the sampling plan and completed-update
counts, rather than counting prefetched examples. Before-and-after route reports
measure teacher choices by channel, label permutation, path phase, and the later Z-path
suffix. These are training-route diagnostics, not independent validation.
A route overlapping a
heldout external-mass family is rejected before training, and rejected curated
rows cannot be hidden by the ordinary random-data rejection allowance.
Long curated jobs have a separate `RouteJobSeconds` limit of 600 seconds;
`RowSeconds` now bounds each state's canonicalization and each complete
verification/encoding step, not just encoding after the whole route was checked.
Rows are encoded immediately after full legal-board verification, releasing that
board before the next step instead of retaining every board for the entire path.
No-op filtering checks the exact local edit delta rather than rebuilding the
whole amplitude for each candidate; candidate order and all legal targets stay
unchanged. Per-worker `route-progress` files and completed-row log entries identify
the route, label permutation, step, and phase. A timeout reports that exact
location and rejects the whole job, never a silently shortened path.
The handwritten Z path exposes more legal choices than the former 512-action
data limit: a checked relabeling reaches 775. The compact runner now defaults
to `MaxActions: 1024`; older JSON configurations with smaller explicit limits
must also be increased. A rejected curated row still fails the run.

Worked routes can temporarily increase complexity. Inference's default ten
stagnant steps can still terminate before a later improvement. The saved Z
plateau at complexity 84 is state 29 of the 97-state teacher route. Its known
68-edit continuation reaches complexity 16, but first rises to 182 and needs
55 non-improving edits before beating 84. This particular route therefore needs
at least `"MaxStagnantSteps" -> 56`; `80` gives it some room. This is not a bound
on every possible simplification path. Use sufficient `"TimeLimit"` as well;
`"MaxSteps"` defaults to `Infinity`, but time and learned Stop decisions can
still end an attempt. Neutral composite splits do not consume patience.
Increasing patience does not guarantee the model chooses the teacher path;
having the route in the corpus does not mean the installed weights have been
retrained on it, nor does it establish the shortest possible reduction.

The full legacy `ReduceSpinorProducts` is not automatic NN preprocessing. It
forcibly splits composite momenta, traverses arbitrary expression heads, and
uses mass-square identities without consulting `OnShellChannels`. A future
preprocessing pass should instead use bounded, option-aware legal contractions
and protect entire propagators. Training already exposes these identities as
actions, without silently simplifying the teaching input first.

Holdouts are fresh scrambles of the SAME starting amplitudes, not unseen
processes. Reports distinguish simplification from recovery of the original
polynomial form. `--holdout 0` disables validation.

`Kernels -> 1` is serial; larger integers launch that many owned workers.
`Automatic` requests one fewer than the processor count, minimum one. Owned
workers close on success, failure, or abort. Explicit existing pools are borrowed
and remain open; do not supply workers busy with other tasks. `"WorkerRoot"`
supports another repository path on workers. Source fingerprints must match.
Deterministic seeds and dataset order do not depend on worker count.

Generated examples are cached in `.unscramble-cache/`, ignored by Git. Keys
include source contents, physics settings, amplitude, seed, and depth. Successful
jobs are cached after generation returns. Source consolidation invalidates the
previous cache; examples will regenerate normally.

The launcher configures native thread settings. Apple silicon keeps OpenMP at
a minimum of four because the tested Wolfram backend stalled at one or two.
Workers default to one native thread on Linux. More symbolic workers do not
automatically accelerate coordinator neural training.

## Background Runs and Deadlines

From the repository root:

```sh
nohup nice -n 10 python3 -u tests/train-unscrambling.py \
  --threads 4 --kernels 32 --worker-threads 1 \
  --amplitudes 4 --scrambles 24 --steps 5 --rounds 2 --holdout 8 \
  > tests/training.log 2>&1 < /dev/null &
```

This is a baseline workload, not a 24-hour estimate. Increase it after measuring.
Do not run two trainers against the same files. `--model-directory` is available
when separate output is explicitly needed.

The Python supervisor logs stage changes and enforces external deadlines:
`--validation-timeout 1800` covers the entire validation stage, including model
distribution; `--run-timeout 172800` covers the entire run (48 hours). Both accept
positive seconds. Timeout exits with status 124 and terminates the owned process
group without deleting saved models. This contains native stalls; it does not
repair their cause. These external limits do not apply to direct notebook or
direct `wolframscript` execution.

Sequential inference (`"Kernels" -> 1`) applies `"MaxStagnantSteps"` separately to each attempt. A stalled
attempt ends, then the next attempt starts from the best checkpoint with a fresh
counter. The first attempt is greedy and later attempts sample moves. Total work
is bounded by `"Attempts"` and the overall `"TimeLimit"`, plus `"MaxSteps"` per
attempt when a finite cap is supplied. Non-improving attempts end after
`"MaxStagnantSteps"`; model Stop and scoring failures also terminate search.
Parallel mode has the same per-route attempt limits, but any new global best
restarts the entire group with fresh attempt and patience counters. The overall
clock does not restart. Late replies from a previous generation are discarded;
the return value remains the best expression even if timeout interrupts an
uphill route. `UnscrambleTrace` includes route/kernel/generation identifiers on
edits, global improvement checkpoints, and final `"Routes"` and `"Restarts"`.

`training-checkpoint.m` is saved before validation. It records training statistics,
not resumable optimizer state. `validation-results.m` is written after validation
completes. A validation timeout leaves quality assessment incomplete even when
training finished successfully.

## Regression Checks

From the repository root:

```sh
python3 -m unittest discover -s tests/checks -p 'test_*.py'
python3 tests/checks/run-wolfram-tests.py
python3 tests/checks/run-wolfram-tests.py --neural --parallel --timeout 150
```

Each Wolfram test has a hard deadline. Checks cover independent exact spinor
identities, encoding, candidate generation, checkpoint loading, inference limits,
cache behavior, and worker ownership. Some use stubbed scoring. `--neural` tests
actual numerical training and inference; `--parallel` tests real workers. Tests
use temporary model directories and do not replace the current trained model.
# Choosing a Policy

Training prints progress every 30 seconds and at each completed round: completed
batches, percent, round, loss, elapsed minutes and an estimated time remaining.
The estimate can change as batch costs vary. The latest values are saved in
`training-progress.m` (scorer) or `move-training-progress.m` (move predictor).
These are progress snapshots, not resumable model checkpoints. The printed
Wolfram memory figure is managed memory, not total process resident memory.
The move predictor also prints its exact training-set move accuracy after
training. This is not holdout accuracy; a small loss alone does not establish
that it chooses the recorded reverse moves.

The existing `"CandidateScorer"` remains the default. The experimental
`"ReverseMoves"` policy reads only the current expression and mass/condition
packet, then predicts a slot in the deterministic legal-move list. It has no
candidate-expression encoder. Symbolic candidate construction is still used
to enumerate legal moves; this prototype does not remove that cost.

The move encoder uses two local convolution layers, then mean and maximum
aggregation across the whole byte sequence. No expression bytes are truncated.
This avoids relying only on a final recurrent state, which lost earlier
differences in the four-state diagnostic. The scorer retains its existing GRU
encoders and balanced squared-error objective.

The move predictor outputs logits and trains with categorical cross-entropy.
Softmax normalization includes only legal slots; illegal slots receive no
probability or gradient. Equivalent reverse actions share the target probability
equally. Inference takes the highest legal logit, or samples the legal softmax on
later attempts. Its metadata records `"Objective" -> "LegalMoveCrossEntropy"`.

Both methods currently learn from recorded reverse scramble trajectories.
The difference is the network input and output, not a new source of labels.
Every positive action is checked to reach the preceding expression. Unreachable
reverse steps are skipped and reported; original states teach Stop. Multiple
legal actions that reach the same target are all positive labels.

```wolfram
report = TrainUnscrambleNet[trainingAmplitudes,
  "PolicyMethod" -> "ReverseMoves", Steps -> 3, Scrambles -> 2,
  MaxTrainingRounds -> 2, HoldOut -> 1, Kernels -> 1];
UnscrambleSpinorAmplitudes[pair, "PolicyMethod" -> "ReverseMoves"]
UnscrambleTrace[pair, "PolicyMethod" -> "CandidateScorer"]
```

The shell launcher also accepts `--policy-method ReverseMoves`. Model files
are `unscramble-moves.wlnet` and `unscramble-moves.m`; the scorer continues to
use `unscramble.wlnet` and `unscramble.m`. Move-policy checkpoint and validation
reports also have separate filenames. No automatic ensemble is enabled.

This first move predictor has 512 output slots. Lists exceeding that limit fail
explicitly, never truncate. Unused slots are excluded from training loss and
inference selection. Slots depend on legal-move ordering, not universal identity
names, which may make generalization harder. Move metadata pins the implementation
source hash so changed action semantics cannot silently reuse incompatible weights.
After changing `unscrambling.wl`, retrain this experimental model. Older move
weights are incompatible with this encoder and objective. The old scorer
has no new source-hash restriction. Larger-corpus quality and speed remain to be
measured; passing smoke tests is not evidence of improved simplification.
