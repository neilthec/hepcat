# Amplitude Unscrambling

## Files

- `unscrambling.nb`: interactive experiments.
- `unscrambling.wl`: the complete package, with foldable sections for identities,
  candidates, encoding, neural scoring, training, and inference.
- `unscramble.wlnet` and `unscramble.m`: the current network and metadata.
  Training replaces these files by default. Git keeps their history.
- `train-unscrambling.nb`: training parameters and starting amplitudes.
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

`UnscrambleCandidates[pair]` exposes legal edits. `UnscrambleEncoding[pair, edit]`
returns complete `"State"` and `"Candidate"` sequences; decode either with
`FromCharacterCode[sequence - 1, "UTF8"]`.

## Training

Edit the first input cell of `train-unscrambling.nb`, or use the launcher from
the repository root (an absolute script path also works):

```sh
python3 tests/train-unscrambling.py --threads 4 --kernels 2 \
  --amplitudes 4 --scrambles 24 --steps 5 --rounds 2 --holdout 8
```

The launcher evaluates notebook text Input cells without a front end. Other
controls include `--episode-length`, `--batch-size`, and `--worker-threads`.
`--dry-run` prints configuration without starting Wolfram.

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
