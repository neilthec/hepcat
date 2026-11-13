# Reviewed C4 to C5 validation continuity

This is a preparation workflow, not a sealed compatibility claim. The helper
`validation_context_c5.py` is outside the package and cannot change amplitudes,
checks, model data or numerical adapters. It preserves an old record's original
context, seed, options and timing. It never claims that the current constructor
was run or that validation was repeated.

The C4 baseline is in
`tests/results/SM-5-point/compatibility/constructor-C4-pre-C5-a91edec8580c2721ccbe8d5304442e9e506f9fd89dc911c250a550c31723a238-20261113T002645Z/`.
Its sealed `baseline.json` has SHA256
`afcc27a0bb823dc0f9ed4c53e18eb6dafd2c737d2edf4b876dfa350168862677`.
It records 194 complete native amplitudes, 129 strict passing exports and two
raw failing exports qualified by their separately verified precision studies.
The old unresolved five-gluon timeout remains unvalidated. Every complete
capsule was checked against its actual bytes.

The small new archive contains only before-source bytes, prior verifier tools
and 326 short checkpoint summaries. The existing sealed proof and its older
archives are referenced by immutable hashes. They are never copied recursively.
The exact Contacts before-source was recovered from commit `7062bef` and checked
against the already sealed C4 manifest. The two exact 8,160-symbol C4 cold
captures are reused only after that manifest and their original hashes match.

1. Freeze the complete new source and its manifest before capturing or accepting
   evidence. The planned source changes are exactly `HEPCAT-Model.wl`,
   `HEPCAT-Color.wl` and `HEPCAT-CovariantContacts.wl`; the loader and every
   numerical or model input stay unchanged.
2. Run `capture-constructor-definitions.py` with the explicit frozen new context
   and a new output directory. It uses two independent cold kernels, reads no
   models and runs no scattering calculation. Existing validators, reducers and
   numerical adapters must retain all ten exact definition fields.
3. Generate and run the small mechanical loader-signature calculation. An
   unchanged input can be referenced at its exact old hash; a changed input uses
   its one archived before-source copy. No package or model is evaluated by this
   calculation.
4. Analyze the literal source diffs and the exact definition tables. The current
   scaffold permits changes only to the four explicitly named implementation
   DownValues: `ConstructAmplitude`, private `completionFactorizationCore`,
   private `colorConstructAmplitude` and private `calibratedCovariantSource`.
   The two fitted-point helpers have exact declared names; other new defined
   helpers must begin with private `colorOrbit`. Empty parser symbols and empty
   temporary Module symbols are distinguished from functions or stored state.
   An analysis result is always `ANALYZED_NOT_SEALED`.
5. Obtain an independent review bound to the exact analysis, literal source diff,
   exact new defined-symbol list and real semantic-control artifact hashes.
   Review the typed graded color transport, solver-point retention, rejection of an already failed current checkpoint, ordinary
   external-only preparation, all-parameter independent validation and existing
   lower-contact calibration. No function name or namespace is automatically
   exempted from review.
6. Only after that review, integrate the metadata dispatcher, freeze its bytes,
   and seal the exact registry payload. Test the portable negative controls and
   perform a write-disabled refresh of all old records. The previous verifier
   tools are loaded from their small immutable archive, so a new dispatcher
   cannot retrospectively change the old proof.
7. Repeat any strict current-context supplemental probe normally. Keep the raw
   old reports and their actual contexts. Preserve all existing precision
   qualification requirements. Only the parent campaign owner then writes the
   global configuration, progress ledger or notebook and starts fresh C5 runs.

The narrow comparison is a provenance boundary. The independent semantic review
and its controls justify the allowed construction changes; the boundary does
not prove a new amplitude or promise that a future constructor will succeed.
A newly built amplitude still needs every ordinary native and backend check.
