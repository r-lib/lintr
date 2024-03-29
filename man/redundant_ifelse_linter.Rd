% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redundant_ifelse_linter.R
\name{redundant_ifelse_linter}
\alias{redundant_ifelse_linter}
\title{Prevent \code{ifelse()} from being used to produce \code{TRUE}/\code{FALSE} or \code{1}/\code{0}}
\usage{
redundant_ifelse_linter(allow10 = FALSE)
}
\arguments{
\item{allow10}{Logical, default \code{FALSE}. If \code{TRUE}, usage like
\code{ifelse(x, 1, 0)} is allowed, i.e., only usage like
\code{ifelse(x, TRUE, FALSE)} is linted.}
}
\description{
Expressions like \code{ifelse(x, TRUE, FALSE)} and \code{ifelse(x, FALSE, TRUE)} are
redundant; just \code{x} or \code{!x} suffice in R code where logical vectors are a
core data structure. \code{ifelse(x, 1, 0)} is also \code{as.numeric(x)}, but even
this should be needed only rarely.
}
\examples{
# will produce lints
lint(
  text = "ifelse(x >= 2.5, TRUE, FALSE)",
  linters = redundant_ifelse_linter()
)

lint(
  text = "ifelse(x < 2.5, 1L, 0L)",
  linters = redundant_ifelse_linter()
)

# okay
lint(
  text = "x >= 2.5",
  linters = redundant_ifelse_linter()
)

# Note that this is just to show the strict equivalent of the example above;
# converting to integer is often unnecessary and the logical vector itself
# should suffice.
lint(
  text = "as.integer(x < 2.5)",
  linters = redundant_ifelse_linter()
)

lint(
  text = "ifelse(x < 2.5, 1L, 0L)",
  linters = redundant_ifelse_linter(allow10 = TRUE)
)

}
\seealso{
\link{linters} for a complete list of linters available in lintr.
}
\section{Tags}{
\link[=best_practices_linters]{best_practices}, \link[=configurable_linters]{configurable}, \link[=consistency_linters]{consistency}, \link[=efficiency_linters]{efficiency}
}
