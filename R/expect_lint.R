#' Lint expectation
#'
#' @param content the file content to be linted
#' @param checks a list of named vectors of checks to be performed.  Performs
#' different checks depending on the value of checks.
#' \itemize{
#'   \item NULL check if the lint returns no lints.
#'   \item unnamed-vector check if the lint's message matches the value.
#'   \item named-vector check if the lint's field matches the named field.
#'   \item list-vectors check if the given lint matches (use if more than one lint is returned for the content)
#' }
#' @param ... one or more linters to use for the check
#' @param file if not \code{NULL} read content from a file rather than from \code{content}
expect_lint <- function(content, checks, ..., file = NULL) {

  if (!is.null(file)) {
    content <- readChar(file, file.info(file)$size)
  }

  expectation_lint(content, checks, ...)
}

expectation_lint <- function(content, checks, ...) {

  filename <- tempfile()
  on.exit(unlink(filename))
  cat(file = filename, content, sep = "\n")

  lints <- lint(filename, ...)

  linter_names <- substitute(alist(...))[-1]

  if (is.null(checks)) {
    return(testthat::expect(length(lints) %==% 0L,
        paste0(paste(collapse = ", ", linter_names),
          " returned ", print(lints),
          " lints when it was expected to return none!"),
        ))
  }

  if (!is.list(checks)) {
    checks <- list(checks)
  }
  checks[] <- lapply(checks, fix_names, "message")

  if (length(lints) != length(checks)) {
    return(testthat::expect(FALSE,
        paste0(paste(collapse = ", ", linter_names),
          " did not return ", length(checks),
          " lints as expected from content:", content, lints)))
  }

  itr <- 0L #nolint
  res <- mapply(function(lint, check) {
    itr <- itr + 1L
    lapply(names(check), function(field) {
      value <- lint[[field]]
      check <- check[[field]]
      if (field == "message") {
        testthat::expect(re_matches(value, check),
          sprintf("lint: %d %s: %s did not match: %s",
            itr,
            field,
            value,
            check))
      } else {
        testthat::expect(`==`(value, check),
          sprintf("lint: %d %s: %s did not match: %s",
            itr,
            field,
            value,
            check))
      }
    })
  },
  lints,
  checks)
  res[[1]]
}

#' Test that the package is lint free
#'
#' This function is a thin wrapper around lint_package that simply tests there are no
#' lints in the package.  It can be used to ensure that your tests fail if the package
#' contains lints.
#'
#' @param ... arguments passed to \code{\link{lint_package}}
#' @export
expect_lint_free <- function(...) {
  lints <- lint_package(...)
  has_lints <- length(lints) > 0

  lint_output <- NULL
  if (has_lints) {
    lint_output <- paste(collapse = "\n", capture.output(print(lints)))
  }
  result <- testthat::expect(!has_lints,
                        paste(sep = "\n",
                              "Not lint free",
                              lint_output))

  invisible(result)
}
