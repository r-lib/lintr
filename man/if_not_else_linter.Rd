% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/if_not_else_linter.R
\name{if_not_else_linter}
\alias{if_not_else_linter}
\title{Block statements like if (!A) x else y}
\usage{
if_not_else_linter(exceptions = c("is.null", "is.na", "missing"))
}
\arguments{
\item{exceptions}{Character vector of calls to exclude from linting.
By default, \code{\link[=is.null]{is.null()}}, \code{\link[=is.na]{is.na()}}, and \code{\link[=missing]{missing()}} are excluded
given the common idiom \code{!is.na(x)} as "x is present".}
}
\description{
\code{if (!A) x else y} is the same as \code{if (A) y else x}, but the latter is
easier to reason about in the \verb{else} case. The former requires
double negation that can be avoided by switching the statement order.
}
\details{
This only applies in the simple \verb{if/else} case. Statements like
\code{if (!A) x else if (B) y else z} don't always have a simpler or
more readable form.

It also applies to \code{\link[=ifelse]{ifelse()}} and the package equivalents
\code{dplyr::if_else()} and \code{data.table::fifelse()}.
}
\examples{
# will produce lints
lint(
  text = "if (!A) x else y",
  linters = if_not_else_linter()
)

lint(
  text = "if (!A) x else if (!B) y else z",
  linters = if_not_else_linter()
)

lint(
  text = "ifelse(!is_treatment, x, y)",
  linters = if_not_else_linter()
)

lint(
  text = "if (!is.null(x)) x else 2",
  linters = if_not_else_linter(exceptions = character())
)

# okay
lint(
  text = "if (A) x else y",
  linters = if_not_else_linter()
)

lint(
  text = "if (!A) x else if (B) z else y",
  linters = if_not_else_linter()
)

lint(
  text = "ifelse(is_treatment, y, x)",
  linters = if_not_else_linter()
)

lint(
  text = "if (!is.null(x)) x else 2",
  linters = if_not_else_linter()
)

}
\seealso{
\link{linters} for a complete list of linters available in lintr.
}
\section{Tags}{
\link[=configurable_linters]{configurable}, \link[=consistency_linters]{consistency}, \link[=readability_linters]{readability}
}
