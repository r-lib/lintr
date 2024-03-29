% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/terminal_close_linter.R
\name{terminal_close_linter}
\alias{terminal_close_linter}
\title{Prohibit close() from terminating a function definition}
\usage{
terminal_close_linter()
}
\description{
Functions that end in \code{close(x)} are almost always better written by using
\code{on.exit(close(x))} close to where \code{x} is defined and/or opened.
}
\examples{
# will produce lints
code <- paste(
  "f <- function(fl) {",
  "  conn <- file(fl, open = 'r')",
  "  readLines(conn)",
  "  close(conn)",
  "}",
  sep = "\n"
)
writeLines(code)
lint(
  text = code,
  linters = terminal_close_linter()
)

# okay
code <- paste(
  "f <- function(fl) {",
  "  conn <- file(fl, open = 'r')",
  "  on.exit(close(conn))",
  "  readLines(conn)",
  "}",
  sep = "\n"
)
writeLines(code)
lint(
  text = code,
  linters = terminal_close_linter()
)

}
\seealso{
\link{linters} for a complete list of linters available in lintr.
}
\section{Tags}{
\link[=best_practices_linters]{best_practices}, \link[=robustness_linters]{robustness}
}
