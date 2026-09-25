# Neural amplitude simplification

Load HEPCAT, then `tests/unscrambling.wl`. The notebook entry points and
`{massRules, amplitude}` interface remain. Retraining is required: the old
fixed-action and version-2 weights are incompatible with the shared-state scorer.

## Shared-state candidate policy (version 3)

The symbolic engine enumerates concrete legal edits and a Stop action. A
shared neural network scores each edit, with no fixed number of actions,
particle labels, chain occurrences, or Schouten split positions. Selecting
an edit applies its recorded subtraction and insertion, not a guessed leg
tuple. The list can grow with the expression; there is no top-four cutoff.

The state encoder serializes the complete expression tree, mass rules, and
kinematic conditions once per state. A separate encoder receives each proposed
edit, using the same particle-label mapping. FullForm preserves heads, argument order,
coefficients, powers, denominators, and explicit spin indices. UTF-8 bytes
map to IDs 1-256, so new symbols cannot become unknown vocabulary tokens.
Particle labels are consistently renumbered in particle-bearing nodes;
coefficients and exponents are not mistaken for particle labels.

The state embedding and GRU run once, producing a vector shared by every
candidate. A separate embedding and GRU encode the edits, and a mapped scoring
head combines each edit vector with that state vector. One graph call returns
all candidate scores, with no fixed candidate count. No tokens or expression terms are truncated.
Candidate byte sequences are left-padded to the longest edit in that group
using reserved ID 257. This padding is learned, not masked, so its amount
can affect scores. State bytes are never padded to candidate count or length.
The recurrent state is still finite: seeing every byte is not a guarantee
that the model can learn every long-range relationship. Runtime and memory
also grow with expression length and the number of candidate edits.

Training uses reverse scramble trajectories as supervised examples. A
reverse step is included only when an available inference edit can reach
its predecessor. Unreachable reverse steps are counted in the report.
Supplied simplified originals provide Stop examples. Positive examples are
weighted to balance the larger pool of unsuccessful alternatives, without
duplicating examples. Every training example contains one full state and all
its candidates, labels, and loss weights. Both encoders train jointly through
the shared graph; the state encoder is neither frozen nor cached across weight
updates. `"TrainingBatchSize"` defaults to one state group; it can be increased
to expose more neural work concurrently, at higher memory cost. Varying state lengths and
candidate counts. NetTrain permits only the first input dimension to vary,
so training pads edits to the longest edit in that dataset and fixes the
second dimension to that width. No bytes are truncated. After training,
the saved policy's edit width is restored to variable, allowing longer
future edits. Training-wide padding adds work when edit lengths differ
greatly. The optimizer minimizes class-balanced
squared error on scores 0 and 1. EpisodeLength controls
the step budget for holdout inference; it no longer controls random policy
training episodes. Data generation and validation support parallel workers;
neural weight updates remain a single NetTrain job on the coordinator.

New weights and metadata use `unscramble-shared-v3.wlnet` and
`unscramble-shared-v3.m`. Old files remain untouched and are never loaded
as version-3 weights. Run `train-unscrambling.nb` again to train the new model.
The string option `ModelDirectory` can select another
directory for training and inference.

```wolfram
TrainUnscrambleNet[trainingAmplitudes, Steps -> 4, Scrambles -> 10,
  MaxTrainingRounds -> 6, HoldOut -> 2, Kernels -> 4,
  "WorkerThreads" -> 1, "DataCacheDirectory" -> "tests/.unscramble-cache"]
UnscrambleSpinorAmplitudes[pair, "MaxSteps" -> 12, "Attempts" -> 5,
  "MaxStagnantSteps" -> 3, "TimeLimit" -> 60]
```

The initial training data can all be four-point amplitudes. Higher
multiplicity is supported by the representation and action selection, but
generalization must still be measured on held-out higher-point examples.

## Parallel execution

`Kernels -> 1` runs serially. `Kernels -> n` launches n owned workers for
independent scrambling/label-generation jobs and validation examples.
`Automatic` requests one fewer than the processor count, minimum one.
The notebook defaults to two workers; explicitly size the pool for your
Wolfram license and memory. Insufficient worker startup fails visibly.
Job seeds and returned dataset order are independent of the worker count.
Jobs are statically balanced by estimated expression size; a single unusually
expensive trajectory can still dominate a worker's completion time.

Owned workers are closed on success, failure, or abort. Existing kernels are
not closed. To use a dedicated preconnected cluster pool, pass its kernel
objects instead: `Kernels -> pool`. These workers remain connected afterward.
Do not pass workers busy with other tasks: the initializer loads HEPCAT and
configures their thread environment. Set `"WorkerRoot" -> "/cluster/hepcat"`
when the repository is at a different path on workers. All workers must have
the same code; a source fingerprint is checked before dispatch. No shared
model-file path is needed: the coordinator broadcasts the trained model once
for validation and is the only model-file writer. Concurrent training runs
should use different `"ModelDirectory"` values.

`"DataCacheDirectory" -> path` stores exact generated examples on the
coordinator. Matching jobs skip symbolic generation on subsequent runs,
including optimizer/batch-size experiments. Cache keys include the amplitude,
seed, scramble depth, physics settings, and source-file contents. Changing
those invalidates affected entries. The notebook enables this cache in
`tests/.unscramble-cache`, which is ignored by Git. This saves CPU time as
well as wall time. Only successful generation results are cached; a failed
job never silently produces a partial training dataset. Completed successful
jobs are cached even when another returned job fails. Cache writes occur after
the generation stage returns, not incrementally during a worker job.

`"JobTimeLimit"` defaults to 300 seconds for each generation/validation job;
`"ValidationTimeLimit"` defaults to 60 seconds for a validation search.
Native-library hangs may not honor Wolfram time constraints. Use the external
test supervisor for integration tests, and a scheduler wall limit on a cluster.

Reports include coordinator wall times (`DataGenerationSeconds`,
`TrainingSeconds`, `ValidationSeconds`, `WorkerSetupSeconds`, `TotalWallSeconds`),
coordinator training CPU time, summed worker CPU time, training-data size,
cache-hit counts, per-job worker/timing records, and
`EncodingWork` estimates. The latter exposes the state-encoding work saved by
sharing and the edit work introduced by padding. Compare uncached runs to
measure parallel scaling, then warm-cache runs to measure reuse.

## CPU threads

### Scaling a run

The notebook remains the source of default settings and known amplitudes.
You do not need to edit the Python launcher. Override settings for one run:

```sh
time python3 /Users/neil/code/hepcat/tests/train-unscrambling.py --threads 4 --kernels 2 --scrambles 12 --steps 5 --rounds 2 --holdout 3
```

Compared with the notebook defaults, this triples training scrambles (48
instead of 16 across four starting amplitudes) and validation scrambles (12
instead of 4). It does not triple the number of distinct starting amplitudes.
Scramble depth and training passes remain unchanged; wall time need not scale
linearly, especially with cached generation jobs.

Other controls are `--amplitudes` (take the first N known amplitudes, capped
at the notebook list size), `--episode-length` (validation search depth), and
`--batch-size` (states per training batch). `--holdout 0` disables validation.
`--model-directory /absolute/path` saves a separate model, preserving the default
model; without it, a successful run replaces the default v3 model. Use
`--help` to list controls or `--dry-run` to inspect overrides without training.
Overrides are passed through `HEPCAT_TRAIN_*` environment variables, not saved
as new notebook defaults. An already open notebook should be reloaded before
using the updated cells; avoid saving an older open copy over these changes.

The final notebook output now summarizes effective data settings, expression
states and candidate edits, used/skipped reverse steps, cache reuse, elapsed
stage timings, and validation counts/percentages. Full diagnostics remain in
`report`. Validation uses fresh scrambles of the training amplitudes, so these
figures do not measure generalization to unseen physics processes. Shell
`time` includes Wolfram startup; the reported pipeline time does not.

Wolfram worker processes and native neural threads are separate controls.
`"WorkerThreads"` requests one native thread per worker by default, except
that OpenMP has a minimum of four on Apple silicon (see below). Account for
these additional threads when choosing the worker count. The coordinator's CPU neural backend
may already use multiple cores within its single process.

For the next training run, request native threads before Wolfram starts:

```sh
python3 /Users/neil/code/hepcat/tests/train-unscrambling.py --threads 4 --kernels 4
```

The launcher runs the same training notebook, with its existing parameters,
and sets `OMP_NUM_THREADS`, `OPENBLAS_NUM_THREADS`, and `MKL_NUM_THREADS`.
It leaves the MXNet operator scheduler at one worker to avoid multiplying
simultaneous thread pools. `--kernels` overrides the notebook's worker count.
`--dry-run` prints
the command without starting training. Existing training runs are unaffected.

On the tested Apple-silicon Wolfram 15 installation, forcing
`OMP_NUM_THREADS=1` or `2` stalled even tiny numerical neural tests. Default
settings and `4` completed successfully. Stack sampling showed the neural
engine waiting for a BLAS/OpenMP computation. This is a native runtime
interaction, not evidence that the model is too large. The launcher, test
supervisor, and worker initialization now use a minimum OpenMP count of four
on Apple silicon; BLAS/MKL retain the requested count. Other platforms retain
the requested OpenMP count. This is a local compatibility workaround, not a
general claim that Wolfram needs four threads. Start fresh kernels when
changing native thread settings; already initialized libraries may retain
their previous configuration. The launcher prints the effective OpenMP count.

Compare `TrainingSeconds` on identical inputs with 4 and 8 threads,
running only one training job at a time. These environment settings request
thread counts, not guaranteed utilization or speedup. The sequential GRU
steps, small matrix operations, and synchronization can limit scaling.
Setting more threads can make a small model slower. The test supervisor's
settings apply only to its own child tests, not notebook training.

Neural optimization also accepts the standard `TargetDevice` option, default
`"CPU"`. On a supported CUDA host, use `TargetDevice -> "GPU"` for one GPU or
`TargetDevice -> {"GPU", All}` for all local GPUs, with an adequately sized
`"TrainingBatchSize"` (for example 8). These options are passed to NetTrain;
GPU hardware and multi-GPU throughput have not been tested here. Wolfram's
[TargetDevice documentation](https://reference.wolfram.com/language/ref/TargetDevice)
describes the supported CUDA configurations; CUDA training is not available
on macOS. Validation workers continue using CPU inference.

This is not multi-node distributed gradient training. The neural optimizer
still runs on one host, optionally using its supported GPUs; more worker
nodes accelerate data preparation and validation, not that optimizer.
Larger `"TrainingBatchSize"` values need benchmarking on your backend. If
optimization still dominates, the next step is a distributed training backend,
not additional symbolic kernels.

MXNet documents the distinction between [operator threads and scheduler
threads](https://mxnet.apache.org/versions/master/api/faq/env_var).
The installed Apple-silicon MXNet library links to OpenMP and OpenBLAS.

## Generalized Schouten identities

`SchoutenRewrite[c1, c2, {m, n}]` implements Decay Rates, Appendix B,
Eq. (B11). The integers count momentum insertions before the split in each
chain, starting at zero. The split must have matching dotted/undotted
chirality; incompatible splits return `$Failed`. Momentum order is retained,
reversed segments receive the prescribed sign, and full endpoint spinors
(including massive spin indices) are carried through unchanged.

`SchoutenCandidates[expression]` enumerates these edits between pairs of
chain factors in each expanded numerator term. Positive integer powers and
spectator factors are supported. Each edit records `Subtract`, `Insert`,
`Chains`, and `Splits`. It uses only Schouten and chain reversal, with no
on-shell or momentum-conservation assumption. It does not rewrite inside
inverse spinor chains or other arbitrary function heads.

`UnscrambleCandidates[pair]` returns the shared inference/scrambling action
list. Its edits apply to the expanded, mass-substituted expression with
chain reversal canonicalized. Additional candidates include endpoint mass
identities in both directions, momentum conservation, and momentum-square
identities. Candidate coverage is still an extensible identity library,
not a complete algorithm for every possible amplitude.

## Inspecting the policy

```wolfram
scrambled = ComplicateAmplitude[pair, 4, 1];
SeedRandom[1];
trace = UnscrambleTrace[{pair[[1]], scrambled["Expression"]}];
trace["Result"]
KeyTake[#, {"Candidate", "Index", "Count", "Score", "ComplexityBefore", "ComplexityAfter"}] & /@
  trace["Trace"]
trace["Checkpoints"]
```

Tracing runs the same inference routine as `UnscrambleSpinorAmplitudes`.
It records exact edits, candidate counts, scores, Stop actions, and accepted
checkpoints. Inference is stochastic after the first attempt; use
`SeedRandom` when comparing runs. Intermediate expressions may grow during
an attempt. Every strict improvement is retained immediately, even if later
steps worsen the expression. By default, three consecutive steps without a
new best complexity stop the entire search, including further attempts.
The counter resets on improvement and otherwise carries across attempts.
`"TimeLimit"` bounds model loading and the search in seconds (default 60);
on timeout the best completed checkpoint is returned. Wolfram's
`TimeConstrained` cannot guarantee interruption of an unresponsive native
backend. Initial symbolic normalization precedes this time budget.
`trace["TerminationReason"]` reports `"Stagnation"`, `"TimeLimit"`,
`"Stop"`, `"StepLimit"`, or `"ScoringFailed"` for a loaded search.
Use `ComplicateAmplitude[pair, 4, "RecordSteps" -> True]` to inspect the
scrambling trajectory. `UnscrambleEncoding[pair, candidate]` exposes the
complete variable-length input for inspection as an association:
`encoding["State"]` and `encoding["Candidate"]`. This replaces the version-2
single byte vector. Decode either sequence with
`FromCharacterCode[sequence - 1, "UTF8"]`.

Training reports `"DataGenerationSeconds"`, `"TrainingSeconds"`, and
`"ValidationSeconds"` separately. These distinguish symbolic candidate
generation from neural optimization and post-training simplification trials.
`"TrainingStates"` counts grouped examples; `"TrainingRows"` counts their
candidate labels for comparison with previous reports. Each state is encoded
once per group evaluation, both when training and simplifying. Edit encoding
still costs work proportional to candidate count and padded edit length;
large edits can include an entire affected monomial. Symbolic enumeration
also remains a potential bottleneck. No wall-clock speedup is claimed until
native numerical benchmarks run successfully.

The graph uses Wolfram's [NetMapThreadOperator](https://reference.wolfram.com/language/ref/NetMapThreadOperator.html)
to share the state vector across candidate scores, with the state encoder
outside that mapped operation.

## Identity checks

Run `WolframKernel -script tests/schouten-tests.wl` from any directory using
an absolute script path if necessary. On this Mac the kernel is
`/Applications/Wolfram.app/Contents/MacOS/WolframKernel`.

The tests compare the identities against independent exact complex
two-component matrix calculations, not HEPCAT's simplification rules.
They cover both chiralities, every split for chain lengths zero through
four, invalid splits, explicit spin indices, spectators, denominators,
powers, and the active policy's ability to shorten a momentum-chain example.
They neither train a model nor write network files.

`candidate-policy-tests.wl` checks higher labels, six-leg candidates, more
than four split choices, long inputs, sparse relabeling, explicit kinematic
assumptions, and supervised labels. `candidate-training-tests.wl` tests the
training orchestration and checkpoint save/load using a stubbed optimizer
and scorer; it is not a numerical neural-network test.
`shared-state-tests.wl` checks the shared graph structure, grouped labels,
loss balancing, padding without truncation, and a single model call for the
whole candidate set. `inference-limits-tests.wl` checks stagnation, timeouts,
and preservation of intermediate improvements with a stubbed scorer.
`training-pipeline-tests.wl` checks reproducible job decomposition, cache
invalidation/retries, timeouts, and ownership-safe cleanup using test doubles.
Add `--parallel --timeout 150` to run the real two-worker symbolic integration
test, including serial/parallel equivalence and borrowed-pool ownership.
In a local unsandboxed run, its 26-job test took 0.85 s serially versus 0.52 s
on two workers, excluding startup. This is a small benchmark, not a promise
of end-to-end training speedup. Sandbox IPC restrictions can stall startup.

`python3 tests/run-wolfram-tests.py` runs these checks with a process-group
timeout. Add `--neural` to test numerical inference and training, also with
a hard timeout. Numerical training and inference now pass with the OpenMP
workaround above, including a batch with unequal full-state lengths and
candidate counts. Add both `--neural --parallel --timeout 150` for the real
end-to-end `neural-pipeline-tests.wl`: six-leg data generation, optimization,
worker numerical validation, temporary model save/reload, numerical inference,
and owned-worker cleanup. A small local run took 6.83 s overall, with 2.10 s
training and 1.46 s validation. These are smoke-test timings, not a large-data
benchmark or a measure of learned simplification quality. Tests do not replace
the user's saved models. The supervisor kills timed-out test kernels.

## Paper map and next steps

The locally supplied papers are intentionally ignored by Git. Relevant
references, identified by filename and equation, are:

- `Decay_Rates.pdf`: Appendix A, Eqs. (A14)-(A20), mass and contraction
  identities; Eq. (A28), chain reversal; Eq. (A38), adjacent momentum
  anticommutation. Appendix B, Eqs. (B9)-(B11), general Schouten, with
  examples (B12)-(B20). Appendix C supplies explicit spinor products.
- `SM_Vertices.pdf`: Appendix A fixes spinor, epsilon, and momentum
  conventions; Appendix B gives massless three-point vertices.
- `QED-Challenges.pdf`: appendices contain worked multi-step reductions,
  x-factor identities, and shifted-momentum calculations. These supply
  useful example trajectories with their kinematic conditions attached.
- `SM_4_Point_Amplitudes.pdf`: Appendix A revisits internal photons and
  includes terms proportional to a channel invariant. These should not be
  lost through an implicit on-shell substitution.
- `Perturbative_Unitarity.pdf`: appendices contain lengthy boson amplitudes
  and high-energy expansions useful for later evaluation cases.
- `Lagrangian.pdf`: Appendix A gives conventions, Appendix B treats locality,
  and Appendix C derives constructive rules.

On-shell conditions are automatic: internal mass rules such as
`Mass[Multiparticle[1, 2]] -> m` enable the corresponding on-shell identities.
No extra option is needed. The default `"OnShellChannels" -> Automatic`
infers channels separately from each amplitude's mass rules during training,
scrambling, candidate enumeration, encoding, and inference. The neural input
includes the resolved channels. An explicit channel list overrides this
selection; `{}` disables internal-channel on-shell moves when explicitly requested.
Only two-leg channel on-shell moves are currently implemented. Momentum
conservation assumes a complete list of external legs in the all-incoming
convention and can be disabled with `"MomentumConservation" -> False`.

Evaluate generalization by withholding entire amplitudes/processes, not
only fresh random scrambles of training amplitudes. Measure identity
correctness separately from expression complexity and recovery rate.
