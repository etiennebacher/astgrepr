% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/node.R
\name{node_get_root}
\alias{node_get_root}
\title{Recover the tree root from a node}
\usage{
node_get_root(x)
}
\arguments{
\item{x}{A node, either from \code{\link[=tree_root]{tree_root()}} or from another \verb{node_*()}
function.}
}
\value{
A list of two elements: \code{start} and \code{end}. Each of those is a vector
with two values indicating the row and column. Those are 0-indexed.
}
\description{
Recover the tree root from a node
}
\examples{
src <- "
print('hi')
fn <- function() {
  print('hello')
}
"
root <- src |>
  tree_new() |>
  tree_root()

root |>
  node_find(ast_rule(pattern = "print($A)")) |>
  node_get_root() |>
  tree_root() |>
  node_text()
}
