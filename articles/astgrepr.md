# Getting started

``` r

library(astgrepr)
```

This vignette will give you the basic knowledge so that you can start
using `astgrepr`. If you want to know more about advanced rules and
other topics, I invite you to read the docs of the Rust crate
[`ast-grep`](https://ast-grep.github.io/guide/pattern-syntax.html), on
which this package is built.

## First steps

My main incentive for building this package was to provide a faster
linter for R code. [`lintr`](https://lintr.r-lib.org/) is a great tool,
but I’m jealous of the Python ecosystem that has a lightning-fast linter
([`Ruff`](https://docs.astral.sh/ruff/)).

Therefore, as a motivation for this vignette, let’s say we have the
following code and that we want to find bad patterns:

``` r

src <- "x <- rnorm(100, mean = 2)
any(is.na(y))
plot(x)
any(is.na(x))
any(duplicated(variable))"
```

I can already see two of them:

- `any(is.na())` is slower than
  [`anyNA()`](https://rdrr.io/r/base/NA.html)
  ([`lintr`](https://lintr.r-lib.org/reference/any_is_na_linter.html))
- `any(duplicated())` is slower than `anyDuplicated() > 0`
  ([`lintr`](https://lintr.r-lib.org/reference/any_duplicated_linter.html))

Let’s start by building the abstract syntax tree (AST) corresponding to
this code. This has to be the first step, all other functions depend on
this tree:

``` r

root <- src |>
  tree_new() |>
  tree_root()

root
#> <AST node>
```

## Rules and nodes

“Rules” are one of the key elements of `astgrepr`. They basically define
what we are looking for in the code. One can build a simple rule with
[`ast_rule()`](https://astgrepr.etiennebacher.com/reference/ast_rule.md):

``` r

ast_rule(id = "any_na", pattern = "any(is.na($VAR))")
#> <ast-grep rule: 'any_na'>
#> pattern: any(is.na($VAR))
```

There are many arguments in
[`ast_rule()`](https://astgrepr.etiennebacher.com/reference/ast_rule.md),
and one can also include
[`pattern_rule()`](https://astgrepr.etiennebacher.com/reference/pattern_rule.md)
and
[`relational_rule()`](https://astgrepr.etiennebacher.com/reference/relational_rule.md)
but we keep it simple for now. Once a rule is created, it can be applied
on a node:

``` r

root |> 
  node_find(
    ast_rule(id = "any_na", pattern = "any(is.na($VAR))"),
    ast_rule(id = "any_dup", pattern = "any(duplicated($VAR))")
  )
#> <List of 2 rules>
#> |--any_na: 1 node
#> |--any_dup: 1 node
```

We can see that most `astgrepr` functions will return a nested list.
Lists are nested on two levels: rules and nodes. For each rule, there is
a specific number of nodes that were matched.

Here,
[`node_find()`](https://astgrepr.etiennebacher.com/reference/node-find.md)
returned a list of two rules, and each of them contains a single node.
This is expected:
[`node_find()`](https://astgrepr.etiennebacher.com/reference/node-find.md)
stops after the first node that matches the rule. If we want to look for
all nodes that match this rule, we can use
[`node_find_all()`](https://astgrepr.etiennebacher.com/reference/node-find.md):

``` r

found_nodes <- root |> 
  node_find_all(
    ast_rule(id = "any_na", pattern = "any(is.na($VAR))"),
    ast_rule(id = "any_dup", pattern = "any(duplicated($VAR))")
  )

found_nodes
#> <List of 2 rules>
#> |--any_na: 2 nodes
#> |--any_dup: 1 nodes
```

More generally, most functions come with a single-node and a multi-node
variants. For instance, , we use
[`node_text_all()`](https://astgrepr.etiennebacher.com/reference/node-text.md)
to extract the text corresponding to each node and
[`node_range_all()`](https://astgrepr.etiennebacher.com/reference/node-range.md)
to get their start and end coordinates in the original code[^1]:

``` r

found_nodes |> 
  node_text_all()
#> $any_na
#> $any_na$node_1
#> [1] "any(is.na(y))"
#> 
#> $any_na$node_2
#> [1] "any(is.na(x))"
#> 
#> 
#> $any_dup
#> $any_dup$node_1
#> [1] "any(duplicated(variable))"
found_nodes |> 
  node_range_all()
#> $any_na
#> $any_na$node_1
#> $any_na$node_1$start
#> [1] 1 0
#> 
#> $any_na$node_1$end
#> [1]  1 13
#> 
#> 
#> $any_na$node_2
#> $any_na$node_2$start
#> [1] 3 0
#> 
#> $any_na$node_2$end
#> [1]  3 13
#> 
#> 
#> 
#> $any_dup
#> $any_dup$node_1
#> $any_dup$node_1$start
#> [1] 4 0
#> 
#> $any_dup$node_1$end
#> [1]  4 25
```

Let’s sum up what we have. So far, we have the original code
(`root$text()`), the location (`$range()`) and content (`$text()`) of
the patterns we were looking for. This is already enough to build a
linter[^2].

`astgrepr` offers another feature: code rewriting.

## Modifying nodes

Wouldn’t it be nice if our IDE (say, RStudio) could automatically fix
those patterns?

To do so, we need two new functions:
[`node_replace_all()`](https://astgrepr.etiennebacher.com/reference/node-fix.md)
and
[`tree_rewrite()`](https://astgrepr.etiennebacher.com/reference/tree_rewrite.md).
The first one takes a list of replacements for each rule, and the second
one rewrites a node based on those replacements. First, let’s see what
[`node_find_all()`](https://astgrepr.etiennebacher.com/reference/node-find.md)
looks like:

``` r

nodes_to_replace <- root |>
  node_find_all(
    ast_rule(id = "any_na", pattern = "any(is.na($VAR))"),
    ast_rule(id = "any_dup", pattern = "any(duplicated($VAR))")
  )

nodes_to_replace
#> <List of 2 rules>
#> |--any_na: 2 nodes
#> |--any_dup: 1 nodes
fixes <- nodes_to_replace |>
  node_replace_all(
    any_na = "anyNA(~~VAR~~)",
    any_dup = "anyDuplicated(~~VAR~~) > 0"
  )

fixes
#> $node_1
#> $node_1[[1]]
#> [1] 26
#> 
#> $node_1[[2]]
#> [1] 39
#> 
#> $node_1[[3]]
#> [1] "anyNA(y)"
#> 
#> 
#> $node_2
#> $node_2[[1]]
#> [1] 48
#> 
#> $node_2[[2]]
#> [1] 61
#> 
#> $node_2[[3]]
#> [1] "anyNA(x)"
#> 
#> 
#> $node_1
#> $node_1[[1]]
#> [1] 62
#> 
#> $node_1[[2]]
#> [1] 87
#> 
#> $node_1[[3]]
#> [1] "anyDuplicated(variable) > 0"
#> 
#> 
#> attr(,"class")
#> [1] "astgrep_replacements" "list"
```

It returns a nested list (once again) with the replacement for each node
and the coordinates indicating where this replacement should be
inserted. To finalize our code rewrite, we now need to apply those
changes to the original tree with
[`tree_rewrite()`](https://astgrepr.etiennebacher.com/reference/tree_rewrite.md):

``` r

# original code
cat(src)
#> x <- rnorm(100, mean = 2)
#> any(is.na(y))
#> plot(x)
#> any(is.na(x))
#> any(duplicated(variable))
# new code
tree_rewrite(root, fixes)
#> x <- rnorm(100, mean = 2)
#> anyNA(y)
#> plot(x)
#> anyNA(x)
#> anyDuplicated(variable) > 0
```

And that’s it. Building a linter or a code rewriter is a massive effort
that is not among `astgrepr` objectives, but I hope this tool can serve
as a foundation to build one.

[^1]: Note that in each sublist, the first value refers to the row and
    second one to the column. Also, those values are 0-indexed, so `1`
    corresponds to the second row/column.

[^2]: Of course, more work is needed to make the IDE report those lints,
    but this is outside the scope of `astgrepr`.
