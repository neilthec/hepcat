WWZZh exact algebraic factoring and interrupted native checks

This is an isolated source-only pilot, not a default ConstructAmplitude result.
No canonical amplitude, validation status, production source, model, or tolerance was changed.
The source candidate was sealed before the independent reference model was read.

The external-only prepared pair has 124,041 leaves, 1,323 top-level terms,
28 distinct spinor chains, 10 distinct physical propagators, and 21 existing
propagator-denominator groups. Full polynomial Factor applied separately to
those groups gives 38,097 leaves. Every spinor chain, propagator, and model
parameter is protected as an independent formal atom. All 21 differences
re-expand exactly to zero in that independent-atom ring. There is no external
or internal on-shell substitution in this additional factoring step.
FactorTerms alone gave 126,485 leaves, so extracting a common scalar factor
alone does not account for the improvement. The atom substitution also provides
an explicit audit that no spinor or momentum identity was silently assumed.

The first ordinary native attempt on the 124,041-leaf pair was intentionally
stopped after 975.25 seconds. The factored-pair attempt was intentionally stopped
after 685.66 seconds for stage profiling. Neither returned a completed parameter
set. These interruptions are not physics failures or successful validations.
Both use seed 1409237488 for set 1 and the normal +104729 increment thereafter,
all three standard parameter assignments, four samples per ordinary check,
automatic pole discovery, all original factorization channels, and complete
independent Feynman coverage. Check allowances were 600 seconds; overall bounds
were finite and were never increased. No phase/sign fit or tolerance adjustment
was used. CalcHEP/MadGraph comparisons have not yet been run on this candidate.

Files in this directory are exact copies of the factored retry's files.
The sibling original-pair-native-interrupted directory contains the full raw
and converted independent reference checkpoints required by check.wl, which
pins their SHA256 digests. The worker paths refer to the original scratch tree
/private/tmp/hepcat-four-vector-scalar-candidate. To replay, restore these files
there with their original basenames or update the scratch-prefix paths only,
then recompute the script hash. The production/source/model context must match
the recorded C7 manifest. The factor-inspection script additionally imports the
original fixed pair and writes its separate inspection record under /private/tmp.

The saved exact-atom-ring-factoring.wxf contains the original and factored groups
and all replayed identities. fixed-prepared-candidate.wxf contains the exact
38,097-leaf pair. The benchmark checkpoint is a validator only; no reference
term or coefficient entered construction or factoring.
