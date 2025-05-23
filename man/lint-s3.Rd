% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lint.R
\name{lint-s3}
\alias{lint-s3}
\alias{Lint}
\title{Create a \code{lint} object}
\usage{
Lint(
  filename,
  line_number = 1L,
  column_number = 1L,
  type = c("style", "warning", "error"),
  message = "",
  line = "",
  ranges = NULL
)
}
\arguments{
\item{filename}{path to the source file that was linted.}

\item{line_number}{line number where the lint occurred.}

\item{column_number}{column number where the lint occurred.}

\item{type}{type of lint.}

\item{message}{message used to describe the lint error}

\item{line}{code source where the lint occurred}

\item{ranges}{a list of ranges on the line that should be emphasized.}
}
\value{
an object of class \code{c("lint", "list")}.
}
\description{
Create a \code{lint} object
}
