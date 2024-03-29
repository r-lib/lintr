% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trailing_whitespace_linter.R
\name{trailing_whitespace_linter}
\alias{trailing_whitespace_linter}
\title{Trailing whitespace linter}
\usage{
trailing_whitespace_linter(allow_empty_lines = FALSE, allow_in_strings = TRUE)
}
\arguments{
\item{allow_empty_lines}{Suppress lints for lines that contain only whitespace.}

\item{allow_in_strings}{Suppress lints for trailing whitespace in string constants.}
}
\description{
Check that there are no space characters at the end of source lines.
}
\examples{
# will produce lints
lint(
  text = "x <- 1.2  ",
  linters = trailing_whitespace_linter()
)

code_lines <- "a <- TRUE\n \nb <- FALSE"
writeLines(code_lines)
lint(
  text = code_lines,
  linters = trailing_whitespace_linter()
)

# okay
lint(
  text = "x <- 1.2",
  linters = trailing_whitespace_linter()
)

lint(
  text = "x <- 1.2  # comment about this assignment",
  linters = trailing_whitespace_linter()
)

code_lines <- "a <- TRUE\n \nb <- FALSE"
writeLines(code_lines)
lint(
  text = code_lines,
  linters = trailing_whitespace_linter(allow_empty_lines = TRUE)
)

}
\seealso{
\link{linters} for a complete list of linters available in lintr.
}
\section{Tags}{
\link[=configurable_linters]{configurable}, \link[=default_linters]{default}, \link[=style_linters]{style}
}
