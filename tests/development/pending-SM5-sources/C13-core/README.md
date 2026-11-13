Pending C13 source code for review

This directory is not loaded by HEPCAT. It contains four source patches, small
checksum/validation metadata, portable metadata tests and restore tools. It has
no amplitude, captured WXF, raw physics output, notebook, generated project or
full source snapshot. Nothing here is a production release or canonical result.

The verified baseline commit is 9b13f7f18f75d7451a40e6543f95eea7f516ca8f.
All baseline checksums must match manifest.json. Restore into a new scratch copy:

    python3 restore.py --baseline /path/to/C12-checkout --output /path/to/C13-review

The restorer verifies exact context lines and target hashes, copies only into the
new output, and never loads code or changes the baseline checkout. The original
four-fermion QCD file is a byte-exact prefix; the appended 2F helpers are identical
to the separately tested prototype. The existing loader remains unchanged with
18 inputs and there is no extra per-multiplicity source module.

The Model and CovariantSources patches add a cubic-current eligibility arm for
two supported fermions, two vectors and one scalar with at least one massless
vector. The Color patch adds the same arm to the existing rank-one spectator
provider. Its colour-path/Gram/union/ownership checks remain unchanged. Unknown
operators and used quartics still reject. The original all-massive calibrated
branch remains literal. The QCD extension admits one or two massless adjoints;3adjoints
remain excluded until a certified four-adjoint provider is available.

The metadata tier passed 67/67 under 30s/512MiB per suite. It checks 13 old helpers'
all 10 definition fields, old wrapper rules/other 9 fields,3 new helpers' compiled
fields and binders, ambient Global sentinels, disjoint metadata dispatch and
SM4/QED4-6 exclusions. Generation tests derive charged-lepton/quark rows from
model metadata and block every amplitude/conversion/check/lookup API. These are
metadata and definition controls, not positive physics validation.

Run that tier only when a metadata compute slot is approved:

    python3 verify-metadata.py --baseline /path/to/C12-checkout --candidate /path/to/C13-review --output /path/to/C13-metadata

Its logs and captures stay outside this directory and the baseline repository.
The kernel path can be supplied with --kernel. No public constructor runs.

Separate private GGh and muon AAh families passed their required representative
public constructor calls and complete backend comparisons. Their original
private producer contexts are retained elsewhere and are not relabeled as this
merged tree. This archive does not contain those research outputs.

Open before production integration: merge the parent's separate F4V and 3V2S
candidates, run combined-tree public controls, validate the new rank-one spectator
arm physically, and review GWW or another multi-massive-vector QCD pilot. GWW is
still untested. The 270 cases/84 families count states metadata eligibility only.

No installed source, checker, model, runtime or public option is changed by
creating this archive. Commit, merge and production installation need review.
