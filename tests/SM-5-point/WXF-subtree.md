# Inspecting large saved WXF reports

`wxf-subtree.py` is an optional metadata tool. It reads compressed or uncompressed
WXF, copies requested subtrees byte for byte, and writes standalone compressed
WXF files. It does not load HEPCAT, evaluate Wolfram expressions, change a report,
or establish that an amplitude passes validation.

A POSIX Python 3 environment, a C++17 compiler and zlib development headers are
required. The wrapper compiles its small parser in an owned temporary directory;
no binary is added to the repository. Set `CXX` to another compiler executable if
needed. A first-party Wolfram Python SDK is only used by optional roundtrip tests.

For example, from the repository root:

```sh
python3 tests/SM-5-point/wxf-subtree.py saved-report.wxf /tmp/report-audit \
  --extract '["Status"]' \
  --index '["Details","Stages"]'
```

The output directory must be new. `manifest.json` records the complete original
file's SHA256 before and after reading, stat identity, parser/build hashes,
requested paths, and each extracted file's SHA256. An optional
`--expected-sha HASH` pins an already known input. Only a complete parse with an
unchanged input receives `COMPLETE`; missing paths are reported explicitly as
`COMPLETE_WITH_MISSING_PATHS`. Parse/resource errors retain an `UNRESOLVED`
diagnostic and must not be treated as a complete extraction.

Paths use strings for association string keys, and integers for association
integer keys or zero-based List positions. ``{"symbol":"Global`key"}`` selects an
association symbol key. `{"arg":0}` selects an ordinary function argument;
`{"head":true}` selects its complete head and must be terminal. Failure and
Missing use `"Tag"` for their first argument and, for example,
`["Failure",1,"Reason"]` for the second argument's association. No algebra is
performed when walking these paths.

Repeated `--extract` and `--index` options share one streaming pass. A JSON file
passed with `--requests` may instead contain objects with exactly `Mode`
(`Extract` or `Index`) and `Path`. The parser accepts the first-party SDK's WXF
scalar/function/association and packed/numeric-array token types. It checks wire
structure and bounds; it is not an evaluator or a general semantic validator for
arbitrary symbolic expressions.

Default limits are 180 seconds, 16 GiB of inflated input, two billion tokens,
1024 levels, and 64 MiB of copied uncompressed payload. Requests are bounded at
1 MiB, metadata indexes at 16 MiB, and resident memory at 512 MiB. The parser uses
buffered zlib streams and checks RSS in platform-correct units. Limits may be
reduced using `--seconds`, `--max-bytes`, `--max-tokens`, `--max-depth`, and
`--max-output`. Successful extraction preserves the original token payload,
including exact integer seeds, decimal precision text, arrays and symbolic heads.

Run the focused tests without a Wolfram kernel:

```sh
PYTHONDONTWRITEBYTECODE=1 python3 -B -m unittest discover -s tests/checks \
  -p 'test_sm_five_point_wxf_subtree.py' -v
```

The SDK roundtrip test discovers an installed client, the macOS bundle, a standard
Linux bundle, or `WOLFRAMCLIENT_PYTHON_PATH`. It skips if none is available. The
remaining controls use exact wire fixtures and the standard library.
