This repo hosts several nested Alire crates (aaa_base, aaa_texts,
aaa_ansi, ...), each buildable/testable independently with `alr` from
its own directory.

To build and test everything at once, run from the repo root:

    alr -C dev test

This uses the `dev` crate (see dev/alire.toml), which aggregates all
the other crates' test suites via `[[test]]` entries.

## Crate layout

- Directory name and Alire crate `name` can differ: `aaa_base/`
  hosts the crate named `aaa` (root package `AAA`, in
  aaa_base/src/aaa.ads). Other crates depend on it as `aaa`, pinned
  to `../aaa_base` (or `../../aaa_base` from a `tests/` subdir).
- Nested crates that add children to the shared `AAA` root package
  (aaa_texts, aaa_ansi) each need: `aaa = "~0.3.0"` in
  `[[depends-on]]`, a pin to `../aaa_base`, `with "aaa.gpr";` in
  their own `.gpr`, and the same `aaa` pin repeated in
  `tests/alire.toml`.
- `AAA.ANSI` (aaa_ansi crate) is the low-level ANSI escape-sequence
  builder (ported from ~/prog/ansi-ada, package `AnsiAda` ->
  `AAA.ANSI`), Pure, no external deps. `AAA.ANSI.Tools` (aaa_texts
  crate) is a different, unrelated package: display-width
  measurement (`Length`/`Count_Extra`) built on `Umwi`. Don't
  conflate the two when grepping for "ANSI".

## Alire manifest gotchas

- A bare `[pins.foo]` table in `alire.toml` without a preceding
  `[[pins]]` array-of-tables header makes `alr` fail with
  `field pins: expected a TOML_ARRAY but got a TOML_TABLE`. Either
  write pins inline right after `[[pins]]`
  (`foo = { path = "../foo" }`) or put an empty `[[pins]]` line
  before any `[pins.foo]` subtables (see aaa_texts/alire.toml).
- If `alr test` in a crate's `tests/` dir errors with an Alire
  "bug box" (`No release visited in round 3`), the generated
  `tests/config/*_config.gpr` is likely stale/missing (e.g. after
  editing `[[depends-on]]`). Fix by running `alr build` (or
  `alr update`) directly inside that `tests/` directory to
  regenerate config, then retry `alr test` from wherever.
