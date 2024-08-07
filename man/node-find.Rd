% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/node.R
\name{node-find}
\alias{node-find}
\alias{node_find}
\alias{node_find_all}
\title{Find node(s) matching a pattern}
\usage{
node_find(x, ..., files = NULL)

node_find_all(x, ..., files = NULL)
}
\arguments{
\item{x}{A node, either from \code{\link[=tree_root]{tree_root()}} or from another \verb{node_*()}
function.}

\item{...}{Any number of rules created with \code{ast_rule()}.}

\item{files}{A vector of filenames containing rules. Those must be \code{.yaml}
files.}
}
\value{
\code{node_find()} returns a single \code{SgNode}.

\code{node_find_all()} returns a list of \code{SgNode}s.
}
\description{
Those functions find one or several nodes based on some rule:
\itemize{
\item \code{node_find()} returns the first node that is found;
\item \code{node_find_all()} returns a list of all nodes found.
}

Some arguments (such as \code{kind}) require some knowledge of the tree-sitter
grammar of R. This grammar can be found here:
\url{https://github.com/r-lib/tree-sitter-r/blob/main/src/grammar.json}.
}
\examples{
src <- "x <- rnorm(100, mean = 2)
    any(duplicated(y))
    plot(mtcars)
    any(duplicated(x))"

root <- src |>
  tree_new() |>
  tree_root()

root |>
  node_find(ast_rule(pattern = "any(duplicated($A))"))

root |>
  node_find_all(ast_rule(pattern = "any(duplicated($A))"))

# using the 'kind' of the nodes to find elements
src <- "
  a <- 1
  while (TRUE) { print('a') }
"

root <- src |>
  tree_new() |>
  tree_root()

root |>
  node_find(ast_rule(kind = "while_statement"))

# one can pass several rules at once
src <- "x <- rnorm(100, mean = 2)
    any(duplicated(y))
    plot(mtcars)
    any(duplicated(x))
    while (TRUE) { print('a') }"
root <- src |>
  tree_new() |>
  tree_root()

root |>
  node_find(
    ast_rule(pattern = "any(duplicated($A))"),
    ast_rule(kind = "while_statement")
  )

root |>
  node_find_all(
    ast_rule(pattern = "any(duplicated($A))"),
    ast_rule(kind = "while_statement")
  )
}
