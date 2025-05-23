% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/paste_linter.R
\name{paste_linter}
\alias{paste_linter}
\title{Raise lints for several common poor usages of \code{paste()}}
\usage{
paste_linter(
  allow_empty_sep = FALSE,
  allow_to_string = FALSE,
  allow_file_path = c("double_slash", "always", "never")
)
}
\arguments{
\item{allow_empty_sep}{Logical, default \code{FALSE}. If \code{TRUE}, usage of
\code{paste()} with \code{sep = ""} is not linted.}

\item{allow_to_string}{Logical, default \code{FALSE}. If \code{TRUE}, usage of
\code{paste()} and \code{paste0()} with \code{collapse = ", "} is not linted.}

\item{allow_file_path}{String, one of \code{"never"}, \code{"double_slash"}, or \code{"always"}; \code{"double_slash"} by default.
If \code{"always"}, usage of \code{paste()} and \code{paste0()} to construct file paths is not linted. If \code{"double_slash"},
strings containing consecutive forward slashes will not lint. The main use case here is for URLs -- "paths" like
\code{"https://"} will not induce lints, since constructing them with \code{file.path()} might be deemed unnatural.
Lastly, if \code{"never"}, strings with consecutive forward slashes will also lint. Note that \code{"//"} is never linted
when it comes at the beginning or end of the input, to avoid requiring empty inputs like
\code{file.path("", ...)} or \code{file.path(..., "")}.}
}
\description{
The following issues are linted by default by this linter
(see arguments for which can be de-activated optionally):
\enumerate{
\item Block usage of \code{\link[=paste]{paste()}} with \code{sep = ""}. \code{\link[=paste0]{paste0()}} is a faster, more concise alternative.
\item Block usage of \code{paste()} or \code{paste0()} with \code{collapse = ", "}. \code{\link[=toString]{toString()}} is a direct
wrapper for this, and alternatives like \code{\link[glue:glue_collapse]{glue::glue_collapse()}} might give better messages for humans.
\item Block usage of \code{paste0()} that supplies \verb{sep=} -- this is not a formal argument to \code{paste0}, and
is likely to be a mistake.
\item Block usage of \code{paste()} / \code{paste0()} combined with \code{\link[=rep]{rep()}} that could be replaced by
\code{\link[=strrep]{strrep()}}. \code{strrep()} can handle the task of building a block of repeated strings
(e.g. often used to build "horizontal lines" for messages). This is both more readable and
skips the (likely small) overhead of putting two strings into the global string cache when only one is needed.

Only target scalar usages -- \code{strrep} can handle more complicated cases (e.g. \code{strrep(letters, 26:1)},
but those aren't as easily translated from a \code{paste(collapse=)} call.
}
}
\examples{
# will produce lints
lint(
  text = 'paste("a", "b", sep = "")',
  linters = paste_linter()
)

lint(
  text = 'paste(c("a", "b"), collapse = ", ")',
  linters = paste_linter()
)

lint(
  text = 'paste0(c("a", "b"), sep = " ")',
  linters = paste_linter()
)

lint(
  text = 'paste0(rep("*", 10L), collapse = "")',
  linters = paste_linter()
)

lint(
  text = 'paste0("http://site.com/", path)',
  linters = paste_linter(allow_file_path = "never")
)

lint(
  text = 'paste0(x, collapse = "")',
  linters = paste_linter()
)

# okay
lint(
  text = 'paste0("a", "b")',
  linters = paste_linter()
)

lint(
  text = 'paste("a", "b", sep = "")',
  linters = paste_linter(allow_empty_sep = TRUE)
)

lint(
  text = 'toString(c("a", "b"))',
  linters = paste_linter()
)

lint(
  text = 'paste(c("a", "b"), collapse = ", ")',
  linters = paste_linter(allow_to_string = TRUE)
)

lint(
  text = 'paste(c("a", "b"))',
  linters = paste_linter()
)

lint(
  text = 'strrep("*", 10L)',
  linters = paste_linter()
)

lint(
  text = 'paste0(year, "/", month, "/", day)',
  linters = paste_linter(allow_file_path = "always")
)

lint(
  text = 'paste0("http://site.com/", path)',
  linters = paste_linter()
)

lint(
  text = 'paste(x, collapse = "")',
  linters = paste_linter()
)

}
\seealso{
\link{linters} for a complete list of linters available in lintr.
}
\section{Tags}{
\link[=best_practices_linters]{best_practices}, \link[=configurable_linters]{configurable}, \link[=consistency_linters]{consistency}
}
