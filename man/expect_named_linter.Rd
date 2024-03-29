% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/expect_named_linter.R
\name{expect_named_linter}
\alias{expect_named_linter}
\title{Require usage of \code{expect_named(x, n)} over \code{expect_equal(names(x), n)}}
\usage{
expect_named_linter()
}
\description{
\code{\link[testthat:expect_named]{testthat::expect_named()}} exists specifically for testing the \code{\link[=names]{names()}} of
an object. \code{\link[testthat:equality-expectations]{testthat::expect_equal()}} can also be used for such tests,
but it is better to use the tailored function instead.
}
\examples{
# will produce lints
lint(
  text = 'expect_equal(names(x), "a")',
  linters = expect_named_linter()
)

# okay
lint(
  text = 'expect_named(x, "a")',
  linters = expect_named_linter()
)

lint(
  text = 'expect_equal(colnames(x), "a")',
  linters = expect_named_linter()
)

lint(
  text = 'expect_equal(dimnames(x), "a")',
  linters = expect_named_linter()
)

}
\seealso{
\link{linters} for a complete list of linters available in lintr.
}
\section{Tags}{
\link[=best_practices_linters]{best_practices}, \link[=package_development_linters]{package_development}, \link[=pkg_testthat_linters]{pkg_testthat}, \link[=readability_linters]{readability}
}
