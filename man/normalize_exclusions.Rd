% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exclude.R
\name{normalize_exclusions}
\alias{normalize_exclusions}
\title{Normalize lint exclusions}
\usage{
normalize_exclusions(x, normalize_path = TRUE, root = getwd(), pattern = NULL)
}
\arguments{
\item{x}{Exclusion specification
\itemize{
\item A character vector of filenames or directories relative to \code{root}. Interpreted as globs, see \code{\link[=Sys.glob]{Sys.glob()}}.
\item A named list of integers specifying lines to be excluded per file
\item A named list of named lists specifying linters and lines to be excluded for the linters per file.
}}

\item{normalize_path}{Should the names of the returned exclusion list be normalized paths?
If \code{FALSE}, they will be relative to \code{root}.}

\item{root}{Base directory for relative filename resolution.}

\item{pattern}{If non-NULL, only exclude files in excluded directories if they match
\code{pattern}. Passed to \code{\link[=list.files]{list.files()}} if a directory is excluded.}
}
\value{
A named list of file exclusions.
The names of the list specify the filenames to be excluded.

Each file exclusion is a possibly named list containing line numbers to exclude, or the sentinel \code{Inf} for
completely excluded files. If the an entry is named, the exclusions only take effect for the linter with
the same name.

If \code{normalize_path} is \code{TRUE}, file names will be normalized relative to \code{root}.
Otherwise the paths are left as provided (relative to \code{root} or absolute). That also means
existence is not checked.
}
\description{
Normalize lint exclusions
}
\keyword{internal}
