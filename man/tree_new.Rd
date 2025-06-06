% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tree.R
\name{tree_new}
\alias{tree_new}
\title{Create a syntax tree}
\usage{
tree_new(txt, file, ignore_tags = "ast-grep-ignore")
}
\arguments{
\item{txt}{A character string of length 1 containing the code to parse.
If provided, \code{file} must not be provided.}

\item{file}{Path to file containing the code to parse. If provided, \code{txt}
must not be provided.}

\item{ignore_tags}{Character vector indicating the tags to ignore. Default is
\code{"ast-grep-ignore"}, meaning that any line that follows \verb{# ast-grep-ignore}
will be ignored in the output of \verb{node_*()} functions.}
}
\value{
An abstract syntax tree containing nodes
}
\description{
This function takes R code as string and creates the corresponding abstract
syntax tree (AST) from which we can query nodes.
}
\examples{
src <- "x <- rnorm(100, mean = 2)
    any(duplicated(y))
    plot(x)
    any(duplicated(x))"

tree_new(src)
}
