# Changelog

## astgrepr 0.1.2

CRAN release: 2026-08-04

- Fix build on Windows ARM64, bumped to `extendr` 0.8.2
  ([\#49](https://github.com/etiennebacher/astgrepr/issues/49),
  [@jeroen](https://github.com/jeroen)).

## astgrepr 0.1.1

CRAN release: 2025-06-07

- Fix build on Fedora, no user visible changes on other platforms
  ([\#42](https://github.com/etiennebacher/astgrepr/issues/42)).
- Fix warning when building package on macOS 15.4
  ([\#45](https://github.com/etiennebacher/astgrepr/issues/45)).

## astgrepr 0.1.0

CRAN release: 2025-06-04

- This is the first CRAN release.
- Update `extendr` to 0.8.0
  ([\#40](https://github.com/etiennebacher/astgrepr/issues/40)).

## astgrepr 0.0.10

- In several lints can be fixed but overlap, the first lint is fixed but
  those that are nested in it are ignored. The text output of
  [`tree_rewrite()`](https://astgrepr.etiennebacher.com/reference/tree_rewrite.md)
  now has an attribute `has_skipped_fixes` indicating if some lints were
  not fixed because they were nested
  ([\#34](https://github.com/etiennebacher/astgrepr/issues/34)).

## astgrepr 0.0.9

- Updated ast-grep crate to 0.32.2.

## astgrepr 0.0.8

- Add support for using multi-metavars in replacement.

## astgrepr 0.0.7

- Fix edge case when a matched node is on the last line and there isn’t
  an empty line at the end.
- Internals: add CI, bump ast-grep to 0.26.3.

## astgrepr 0.0.6

- Replacing text that contains escaping characters (e.g. in regular
  expressions) doesn’t remove those characters anymore.
- Enable multiple replacements on the same line of code.
- Replacing text now takes into account leading empty lines.
- Ignore specific rules, e.g. `# ast-grep-ignore: any_duplicated` to
  ignore the rules stored in `any_duplicated.yml` only. Accepts several
  rules separated with a comma.

## astgrepr 0.0.5

- Support metavars in the `message` field in YAML files.

## astgrepr 0.0.4

- Bump `ast-grep` to 0.25.0.
- Refactor Rust code.
- Add support for `constraints` field when using rules YAML files
  ([\#19](https://github.com/etiennebacher/astgrepr/issues/19),
  [\#21](https://github.com/etiennebacher/astgrepr/issues/21)).
- Accept digits in metavar.

## astgrepr 0.0.3

- Add support to tags to ignore lines and chunks of code with
  `# ast-grep-ignore`, `# ast-grep-ignore-start` and
  `# ast-grep-ignore-end`.
- Lots of refactor.
- Support digits in meta-variables.
- More tests.

## astgrepr 0.0.2

- More robust handling of yaml files.
- Update `extendr` to 0.7.0.
- Many changes that hard to list: basically everything works better.
- Considerably reduce binary size
  ([\#8](https://github.com/etiennebacher/astgrepr/issues/8)).

## astgrepr 0.0.1

- Initial version.
