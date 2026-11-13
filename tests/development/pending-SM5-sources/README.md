# Pending SM five-point source work

These are development patches, not installed package code. The package loader
never reads this directory. Keep the patches here because `tests/results/` and
system temporary directories may be deleted at any time.

- `C13-core/` preserves the two-fermion adjoint-current extension and the
  photon/Higgs eligibility changes, together with focused development controls.
- `F4V/` preserves the four-fermion singlet-vector helper extension. Its model
  dispatch patch is a sketch: preserve the existing pure-Abelian QED path before
  using that dispatch in production, and qualify all new symbol names.
- `Three-vector-two-scalar/` preserves the two eligibility changes used for the
  private WWZhh pilot. It does not address the separate WWWWh memory limit.

Each manifest pins the production baseline, candidate bytes, and patch bytes.
Check the baseline before restoring a draft. Several patches touch the same
model/source eligibility code, so merge those hunks deliberately rather than
applying the directories as one patch stack. An individual `git apply --check`
passing is not an integrated physics validation.

Before installation, combine the supported typed dispatches, run the focused
namespace, rejection and old-class preservation controls, and test the actual
public constructor with its independent Feynman comparison. Run full helicity
families against CalcHEP and MadGraph at common points. Preserve SM four-point
and QED through six-point behavior. The development diagnostics do not establish
that all eligible Standard Model processes now complete.

Actual amplitude expressions, numerical outputs, snapshots and generated projects
are intentionally absent. Local reports are disposable evidence caches. Missing
reports must be regenerated or marked unavailable. Consult the SM five-point
campaign ledger for its separately verified progress; these drafts do not add
qualified cases to that ledger.
