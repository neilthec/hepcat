# Massive Standard Model reference for MadGraph

The `sm` UFO is an independent Feynman-rule reference for the SM examples.
It is based on the Standard Model UFO supplied with MadGraph 3.8.1, by
N. Christensen and C. Duhr. The upstream license is retained in `sm/LICENSE`.
No constructive amplitude or fitted matrix element is used in these files.

The upstream model gives u, d and s zero mass and omits their Higgs couplings.
This copy restores independent `MU`, `MD`, `MS` mass inputs and matching
`ymu`, `ymd`, `yms` Yukawa-mass inputs. The Higgs vertices use the same
Lorentz structure and coupling normalization as the existing charm and
bottom vertices. Their defaults match constructive model 1:
0.00216, 0.00470 and 0.095 GeV, respectively.

Use this model in **unitary gauge**. Its UFO gauge declaration permits only
that gauge. The restored physical masses and Higgs couplings do not include
a new set of Goldstone couplings, and this model must not be advertised as
a Feynman-gauge reference. No restriction card is included that could
silently remove small masses or Yukawa interactions.

Select the directory explicitly:

```wolfram
"MadGraphModel" -> FileNameJoin[{hepcatDirectory, "tests", "reference-models", "madgraph", "sm"}]
```

For comparison with the constructive SM model, use diagonal quark mixing
(`"MadGraphParameterRules" -> {"lamWS" -> 0}`). The electroweak input
conversion uses the supplied constructive model's consistent EE, SW and MW
definitions. Numerical validation sets all widths to zero only in its private
reference copy. Parameter and mass updates must keep the MASS and YUKAWA
inputs consistent; the SM validation adapter synchronizes matching PDG slots.

This reference supplies interactions, not known higher-point amplitude
answers. Full quark/gluon comparisons additionally require coherent color
interference and color averages in the exported HEPCAT process.
