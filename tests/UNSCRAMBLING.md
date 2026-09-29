# Amplitude Unscrambling

## Files

- `unscrambling.nb`: interactive experiments.
- `SM-4-point-unscramble-test.nb`: diagram-to-reference benchmark for the 73
  cases in the old SM test notebook, without its process-specific rewrites.
  Uses all-ingoing particle lists and green PASS/red other statuses. Tests all
  24 label permutations by default (3,552 checks). Set
  `particlePermutations = {Range[4]}` for identity-only runs. A permutation maps
  old labels to new labels in inputs, mass rules, and references together.
- `unscrambling.wl`: the complete package, with foldable sections for identities,
  candidates, encoding, neural scoring, training, and inference.
- `unscramble.wlnet` and `unscramble.m`: the current network and metadata.
  Training replaces these files by default. Git keeps their history.
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

Input and output are `{massRules, amplitude}`. Inference retains every strict
complexity improvement. Defaults are `"MaxSteps" -> 12`, `"Attempts" -> 5`,
`"MaxStagnantSteps" -> 3`, and `"TimeLimit" -> 60`. The first attempt is greedy;
later attempts sample scores. Stagnation carries across attempts and resets on
improvement. Initial normalization precedes the time budget. Wolfram's internal
time constraint cannot reliably interrupt a stuck native library.

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
training still checks that each reverse target is reachable. The current model
can score the new serialized edits, but has not been trained on these move
types yet. Retraining is required to learn their use; old generation caches
are invalidated by the source fingerprint. Search limits are unchanged.

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

The shared-state network encodes the full expression, mass rules, and conditions
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
the encoder architecture in the same current model files.

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

The existing `"CandidateScorer"` remains the default. The experimental
`"ReverseMoves"` policy reads only the current expression and mass/condition
packet, then predicts a slot in the deterministic legal-move list. It has no
candidate-expression encoder. Symbolic candidate construction is still used
to enumerate legal moves; this prototype does not remove that cost.

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
After changing `unscrambling.wl`, retrain this experimental model. The old scorer
has no new source-hash restriction. Larger-corpus quality and speed remain to be
measured; passing smoke tests is not evidence of improved simplification.
