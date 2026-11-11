# HEPCAT Help

HEPCAT includes native Mathematica documentation: a main guide, topic guides,
worked tutorials, and individual reference pages for every public function and
symbol. The reference pages include syntax, details, options, examples, related
functions, and links back to the guides.

## Use the help

With the Wolfram working directory set to the HEPCAT checkout root, load:

```wolfram
Get[FileNameJoin[{"source", "HEPCAT.wl"}]]
```

The publication checkout keeps the complete Help in
`HEPCAT-Documentation.zip`, including the editable documentation source.
Loading HEPCAT automatically checks and expands this bundle into a writable
cache under `$UserBaseDirectory/ApplicationData/HEPCAT/Documentation/`, then
registers those pages. Nothing needs to be unpacked manually to use Help.
The checkout itself can be read-only. A changed bundle gets a different cache;
an incomplete or damaged cache is rebuilt from the supplied bundle.

The loader does not open windows, evaluate examples or write into the
Mathematica application. Loading a source-only checkout without documentation
remains supported. A development checkout with loose documentation uses that
local tree directly.

In Mathematica:

- Search Help for **HEPCAT** to open the main guide.
- Select a function name and press **F1** for its reference page.
- Enter `?ReadModel`, for example, for its short usage description.
- Follow the main guide into topic guides and worked tutorials.

The main documentation URI is `paclet:HEPCAT/guide/HEPCAT`. A reference URI has
the form `paclet:HEPCAT/ref/PrepareAmplitudes`. Contributors who unpack the documentation can also open the notebooks
directly from `English/Guides`, `English/ReferencePages/Symbols`, and
`English/Tutorials`.

The manifest version in `PacletInfo.wl` identifies the documentation paclet. It
does not create separate versions of amplitudes or trained models.

## Maintain the documentation

The editable content and build tools are preserved in the bundle. To restore
them at their ordinary checkout paths, run this from the checkout root:

```sh
python3 tests/run-checks.py --unpack-only
```

This also restores the bundled tests and benchmark tools. Expanded support
files are ignored by Git in the publication branch. Keep documentation edits
in the development branch and rebuild the publication bundle deliberately;
ordinary package loading never rewrites the supplied bundle.

The editable content lives in `Source`:

- `base.json`: symbolic algebra, identities, formatting, and earlier utilities.
- `model.json`: models, parameters, diagrams, completion, and validation.
- `feynman-export.json`: Feynman tensors, conversion, and complete SPINAS export.
- `guides.json`: main page, topic navigation, and alphabetical reference index.
- `tutorials.json`: worked calculations with explanations of each stage.
- `option-descriptions.json`: meanings of the current runtime options.
- `example-results.json`: small outputs from examples actually run in a licensed
  kernel. This is display data, never evaluated on package loading.

To regenerate the native notebooks, usage-message fallback registry, and search
index, run the following in a licensed Wolfram kernel:

```wolfram
Get[FileNameJoin[{hepcatDirectory, "Documentation", "Build.wl"}]]
```

The builder reads actual `Options` definitions rather than maintaining duplicate
option defaults. It parses example inputs without executing them. Update the
verified output cache when an example or its result changes; do not invent
outputs. Examples that write files, invoke optional libraries, or start longer
calculations are clearly marked and run only when a reader evaluates them.

To rebuild only the native search index:

```wolfram
Get[FileNameJoin[{hepcatDirectory, "Documentation", "BuildSearchIndex.wl"}]]
```

The checked-in search index uses the installed Wolfram index format. If another
Wolfram version needs a different format, rebuild it there. Direct page links
and F1 help do not depend on search indexing.

## Verify changes

From the repository root:

```sh
python3 tests/checks/run-wolfram-tests.py \
  --suite documentation-content-tests.wl \
  --suite documentation-registration-tests.wl \
  --suite legacy-package-tests.wl
```

The content checks validate public API coverage, page structure, held example
syntax, links, native notebook syntax, and the instruction to avoid em dashes.
The registration checks exercise package loading and reloading, optional missing
documentation, usage descriptions, F1 links, main-page resolution, and native
Help search, including rendering the search results that Mathematica displays.
Generated pages retain the native title, type, category label, summary, context
and URI metadata required by the search display. Neither suite changes model
data or runs a construction job.

Catalog entries default to public. Mark implementation helpers and reserved names
with `"visibility": "internal"`; they remain in the reviewed inventory but have no
public page, search entry or generated usage text. Use `retiredNames` only to
remove obsolete generated page files, not to create callable aliases.

Write explanations around what a reader supplies, what the function returns,
and how the result fits the next step. Put links such as
`[ReadModel](paclet:HEPCAT/ref/ReadModel)` directly into the prose. Group longer
explanations with `detail_sections`, and use example `section` headings for
applications and possible issues. The builder converts these to native Help
cells and links. Setup is collapsed and includes only the paths an example needs.

Keep reference pages honest about limits. A numerical
PASS is evidence from sampled kinematics, not an analytic proof. Document the
scope of internal on-shell assumptions and protect actual propagator denominators.
