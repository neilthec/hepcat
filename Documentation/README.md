# HEPCAT Help

HEPCAT includes native Mathematica documentation: a main guide, topic guides,
worked tutorials, and individual reference pages for every public function and
symbol. The reference pages include syntax, details, options, examples, related
functions, and links back to the guides.

## Use the help

Load the package normally:

```wolfram
Global`$HEPCATpath = FileNameJoin[{hepcatDirectory, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
```

The loader registers the documentation from this checkout. It does not open
windows, rebuild the index, install anything into the Mathematica application,
or evaluate examples. A checkout containing only the source still loads.

In Mathematica:

- Search Help for **HEPCAT** to open the main guide.
- Select a function name and press **F1** for its reference page.
- Enter `?ReadModel`, for example, for its short usage description.
- Follow the main guide into topic guides and worked tutorials.

The main documentation URI is `paclet:HEPCAT/guide/HEPCAT`. A reference URI has
the form `paclet:HEPCAT/ref/PrepareAmplitudes`. The notebooks can also be opened
directly from `English/Guides`, `English/ReferencePages/Symbols`, and
`English/Tutorials`.

The manifest version in `PacletInfo.wl` identifies the documentation paclet. It
does not create separate versions of amplitudes or trained models.

## Maintain the documentation

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
Help search. Neither suite changes model data or runs a construction job.

Keep reference pages honest about limits. Reserved compatibility names have no
callable implementation on this branch and are labeled accordingly. A numerical
PASS is evidence from sampled kinematics, not an analytic proof. Document the
scope of internal on-shell assumptions and protect actual propagator denominators.
