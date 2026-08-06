# Build a pattern rule

This is a specific type of rule. It can be used in the more general
ruleset built with
[`ast_rule()`](https://astgrepr.etiennebacher.com/reference/ast_rule.md).

## Usage

``` r
pattern_rule(selector = NULL, context = NULL, strictness = "smart")
```

## Arguments

- selector:

  Defines the surrounding code that helps to resolve any ambiguity in
  the syntax.

- context:

  Defines the sub-syntax node kind that is the actual matcher of the
  pattern.

- strictness:

  Optional, defines how strictly pattern will match against nodes. See
  'Details'.

## Value

An list of class `astgrep_pattern_rule`

## Details

The `strictness` parameter defines the type of nodes the `ast-grep`
matcher should consider. It has the following values:

- `cst`: All nodes in the pattern and target code must be matched. No
  node is skipped.

- `smart`: All nodes in the pattern must be matched, but it will skip
  unnamed nodes in target code. This is the default behavior.

- `ast`: Only named AST nodes in both pattern and target code are
  matched. All unnamed nodes are skipped.

- `relaxed`: Named AST nodes in both pattern and target code are
  matched. Comments and unnamed nodes are ignored.

- `signature`: Only named AST nodes' kinds are matched. Comments,
  unnamed nodes and text are ignored.

More information:
<https://astgrep.com/guide/rule-config/atomic-rule.html#pattern-object>
