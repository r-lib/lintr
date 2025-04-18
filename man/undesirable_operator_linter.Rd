% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/undesirable_operator_linter.R
\name{undesirable_operator_linter}
\alias{undesirable_operator_linter}
\title{Undesirable operator linter}
\usage{
undesirable_operator_linter(
  op = default_undesirable_operators,
  call_is_undesirable = TRUE
)
}
\arguments{
\item{op}{Named character vector. \code{names(op)} correspond to undesirable operators,
while the values give a description of why the operator is undesirable.
If \code{NA}, no additional information is given in the lint message. Defaults to
\link{default_undesirable_operators}. To make small customizations to this list,
use \code{\link[=modify_defaults]{modify_defaults()}}.}

\item{call_is_undesirable}{Logical, default \code{TRUE}. Should lints also be produced
for prefix-style usage of the operators provided in \code{op}?}
}
\description{
Report the use of undesirable operators, e.g. \code{\link[base:ns-dblcolon]{:::}} or
\code{\link[base:assignOps]{<<-}} and suggest an alternative.
}
\examples{
# defaults for which functions are considered undesirable
names(default_undesirable_operators)

# will produce lints
lint(
  text = "a <<- log(10)",
  linters = undesirable_operator_linter()
)

lint(
  text = "mtcars$wt",
  linters = undesirable_operator_linter(op = c("$" = "As an alternative, use the `[[` accessor."))
)

lint(
  text = "`:::`(utils, hasName)",
  linters = undesirable_operator_linter()
)

# okay
lint(
  text = "a <- log(10)",
  linters = undesirable_operator_linter()
)
lint(
  text = 'mtcars[["wt"]]',
  linters = undesirable_operator_linter(op = c("$" = NA))
)

lint(
  text = 'mtcars[["wt"]]',
  linters = undesirable_operator_linter(op = c("$" = "As an alternative, use the `[[` accessor."))
)

lint(
  text = "`:::`(utils, hasName)",
  linters = undesirable_operator_linter(call_is_undesirable = FALSE)
)

}
\seealso{
\link{linters} for a complete list of linters available in lintr.
}
\section{Tags}{
\link[=best_practices_linters]{best_practices}, \link[=configurable_linters]{configurable}, \link[=robustness_linters]{robustness}, \link[=style_linters]{style}
}
