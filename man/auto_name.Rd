% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{auto_name}
\alias{auto_name}
\title{Ensure that all elements of a list of expressions are named}
\usage{
auto_name(exprs)
}
\arguments{
\item{exprs}{A list of expressions.}
}
\value{
A named list of expressions
}
\description{
Nearly identical to \code{\link[rlang:exprs_auto_name]{rlang::exprs_auto_name()}}, but \code{\link[rlang:as_name]{rlang::as_name()}}
is used instead of \code{\link[rlang:as_label]{rlang::as_label()}}. For String items the string will
returned without wrapping in double quotes. The naming of functions and
formulas is not optimal, it is better to manually name theme.
}
\examples{
funs <- list(
  "mean",
  function(x) stats::quantile(x, probs = 0.95),
  ~ stats::quantile(., probs = 0.95),
  q95 = function(x) stats::quantile(x, probs = 0.95)
)

auto_name(funs)

# exprs_autoname adds double quotes to strings
rlang::exprs_auto_name(funs)
}
