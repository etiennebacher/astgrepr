# Package index

## Build an Abstract Syntax Tree (AST)

- [`tree_new()`](https://astgrepr.etiennebacher.com/reference/tree_new.md)
  : Create a syntax tree
- [`tree_rewrite()`](https://astgrepr.etiennebacher.com/reference/tree_rewrite.md)
  : Rewrite the tree with a list of replacements
- [`tree_root()`](https://astgrepr.etiennebacher.com/reference/tree_root.md)
  : Get the root of the syntax tree

## Navigate the AST

- [`node_kind()`](https://astgrepr.etiennebacher.com/reference/node_kind.md)
  : Find the kind of a node
- [`node_find()`](https://astgrepr.etiennebacher.com/reference/node-find.md)
  [`node_find_all()`](https://astgrepr.etiennebacher.com/reference/node-find.md)
  : Find node(s) matching a pattern
- [`node_get_root()`](https://astgrepr.etiennebacher.com/reference/node_get_root.md)
  : Recover the tree root from a node
- [`node_get_match()`](https://astgrepr.etiennebacher.com/reference/node-get-match.md)
  [`node_get_multiple_matches()`](https://astgrepr.etiennebacher.com/reference/node-get-match.md)
  : Get the match(es) from a meta-variable
- [`node_matches()`](https://astgrepr.etiennebacher.com/reference/node-info.md)
  [`node_inside()`](https://astgrepr.etiennebacher.com/reference/node-info.md)
  [`node_has()`](https://astgrepr.etiennebacher.com/reference/node-info.md)
  [`node_precedes()`](https://astgrepr.etiennebacher.com/reference/node-info.md)
  [`node_follows()`](https://astgrepr.etiennebacher.com/reference/node-info.md)
  : Get more precise information on a node
- [`node_is_leaf()`](https://astgrepr.etiennebacher.com/reference/node-is.md)
  [`node_is_named()`](https://astgrepr.etiennebacher.com/reference/node-is.md)
  [`node_is_named_leaf()`](https://astgrepr.etiennebacher.com/reference/node-is.md)
  : Get information on nodes
- [`node_range()`](https://astgrepr.etiennebacher.com/reference/node-range.md)
  [`node_range_all()`](https://astgrepr.etiennebacher.com/reference/node-range.md)
  : Get the start and end positions of a node
- [`node_text()`](https://astgrepr.etiennebacher.com/reference/node-text.md)
  [`node_text_all()`](https://astgrepr.etiennebacher.com/reference/node-text.md)
  : Extract the code corresponding to one or several nodes
- [`node_parent()`](https://astgrepr.etiennebacher.com/reference/node-traversal.md)
  [`node_child()`](https://astgrepr.etiennebacher.com/reference/node-traversal.md)
  [`node_ancestors()`](https://astgrepr.etiennebacher.com/reference/node-traversal.md)
  [`node_children()`](https://astgrepr.etiennebacher.com/reference/node-traversal.md)
  [`node_next()`](https://astgrepr.etiennebacher.com/reference/node-traversal.md)
  [`node_next_all()`](https://astgrepr.etiennebacher.com/reference/node-traversal.md)
  [`node_prev()`](https://astgrepr.etiennebacher.com/reference/node-traversal.md)
  [`node_prev_all()`](https://astgrepr.etiennebacher.com/reference/node-traversal.md)
  : Navigate the tree

## Modify the AST

- [`node_replace()`](https://astgrepr.etiennebacher.com/reference/node-fix.md)
  [`node_replace_all()`](https://astgrepr.etiennebacher.com/reference/node-fix.md)
  : Change the code in the tree

## Building rules

- [`ast_rule()`](https://astgrepr.etiennebacher.com/reference/ast_rule.md)
  : Build a rule
- [`pattern_rule()`](https://astgrepr.etiennebacher.com/reference/pattern_rule.md)
  : Build a pattern rule
- [`relational_rule()`](https://astgrepr.etiennebacher.com/reference/relational_rule.md)
  : Build a relational rule
