% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/numeric_leading_zero_linter.R
\name{numeric_leading_zero_linter}
\alias{numeric_leading_zero_linter}
\title{Require usage of a leading zero in all fractional numerics}
\usage{
numeric_leading_zero_linter()
}
\description{
While .1 and 0.1 mean the same thing, the latter is easier to read due
to the small size of the '.' glyph.
}
\examples{
# will produce lints
lint(
  text = "x <- .1",
  linters = numeric_leading_zero_linter()
)

lint(
  text = "x <- -.1",
  linters = numeric_leading_zero_linter()
)

# okay
lint(
  text = "x <- 0.1",
  linters = numeric_leading_zero_linter()
)

lint(
  text = "x <- -0.1",
  linters = numeric_leading_zero_linter()
)

}
\seealso{
\link{linters} for a complete list of linters available in lintr.
}
\section{Tags}{
\link[=consistency_linters]{consistency}, \link[=readability_linters]{readability}, \link[=style_linters]{style}
}
