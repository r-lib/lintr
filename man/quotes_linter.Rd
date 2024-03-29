% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quotes_linter.R
\name{quotes_linter}
\alias{quotes_linter}
\title{Character string quote linter}
\usage{
quotes_linter(delimiter = c("\\"", "'"))
}
\arguments{
\item{delimiter}{Which quote delimiter to accept. Defaults to the tidyverse
default of \verb{"} (double-quoted strings).}
}
\description{
Check that the desired quote delimiter is used for string constants.
}
\examples{
# will produce lints
lint(
  text = "c('a', 'b')",
  linters = quotes_linter()
)

# okay
lint(
  text = 'c("a", "b")',
  linters = quotes_linter()
)

code_lines <- "paste0(x, '\"this is fine\"')"
writeLines(code_lines)
lint(
  text = code_lines,
  linters = quotes_linter()
)

# okay
lint(
  text = "c('a', 'b')",
  linters = quotes_linter(delimiter = "'")
)

}
\seealso{
\itemize{
\item \link{linters} for a complete list of linters available in lintr.
\item \url{https://style.tidyverse.org/syntax.html#character-vectors}
}
}
\section{Tags}{
\link[=configurable_linters]{configurable}, \link[=consistency_linters]{consistency}, \link[=default_linters]{default}, \link[=readability_linters]{readability}, \link[=style_linters]{style}
}
