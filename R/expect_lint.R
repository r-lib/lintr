#' Lint expectation
#'
#' @param content a character vector for the file content to be linted.
#' @param checks checks to be performed:
#' \describe{
#'   \item{NULL}{check that no lints are returned.}
#'   \item{unnamed vector or regex object}{check if the returned lint's message matches the given
#'   regular expression.}
#'   \item{named-vector}{check if the returned lint's fields match the named fields. Accepted fields
#'   are the same as those taken by \code{\link{Lint}}.}
#'   \item{list of vectors}{check if the returned lints matches (use if more than one lint is
#'   returned).}
#' }
#' @param ... arguments passed to \code{\link{lint}}, e.g. the linters or cache to use.
#' @param file if not \code{NULL}, read content from the specified file rather than from \code{content}.
#' @return \code{NULL}, invisibly.
#' @examples
#' # no expected lint
#' lintr:::expect_lint("a", NULL, trailing_blank_lines_linter)
#'
#' # one expected lint
#' lintr:::expect_lint("a\n", "superfluous", trailing_blank_lines_linter)
#' lintr:::expect_lint("a\n", c(message="superfluous", line_number=2), trailing_blank_lines_linter)
#'
#' # several expected lints
#' lintr:::expect_lint("a\n\n", list("superfluous", "superfluous"), trailing_blank_lines_linter)
#' lintr:::expect_lint(
#'   "a\n\n",
#'   list(c(message="superfluous", line_number=2), c(message="superfluous", line_number=3)),
#'   trailing_blank_lines_linter)
expect_lint <- function(content, checks, ..., file = NULL) {

  content <- if (!is.null(file)) {
    readChar(file, file.info(file)$size)
  } else {
    encodeString(content)
  }

  expectation_lint(content, checks, ...)
}


# Note: this function cannot be properly unit tested because testthat's
# expect_success() and expect_failure() do not like the way we execute several
# expect() statements

expectation_lint <- function(content, checks, ...) {

  filename <- tempfile()
  on.exit(unlink(filename))
  cat(file = filename, content, sep = "\n")

  lints <- lint(filename, ...)
  n_lints <- length(lints)
  lint_str <- if (n_lints) {paste0(c("", lints), collapse="\n")} else {""}

  wrong_number_fmt  <- "got %d lints instead of %d%s"
  if (is.null(checks)) {
    msg <- sprintf(wrong_number_fmt, n_lints, length(checks), lint_str)
    return(testthat::expect(n_lints %==% 0L, msg))
  }

  if (!is.list(checks)) {
    checks <- list(checks)
  }
  checks[] <- lapply(checks, fix_names, "message")

  if (n_lints != length(checks)) {
    msg <- sprintf(wrong_number_fmt, n_lints, length(checks), lint_str)
    return(testthat::expect(FALSE, msg))
  }

  local({
    itr <- 0L #nolint
    lintFields <- names(formals(Lint))
    Map(function(lint, check) {
      itr <<- itr + 1L
      lapply(names(check), function(field) {
        if (!field %in% lintFields) {
          stop(sprintf(
            "check #%d had an invalid field: \"%s\"\nValid fields are: %s\n",
            itr, field, toString(lintFields)))
        }
        check <- check[[field]]
        value <- lint[[field]]
        msg <- sprintf("check #%d: %s \"%s\" did not match \"%s\"",
                       itr, field, value, check)
        exp <- if (field == "message") {
          re_matches(value, check)
        } else {
           `==`(value, check)
        }
        testthat::expect(exp, msg)
        })
      },
      lints,
      checks)
    })

  invisible(NULL)
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
                             paste(sep = "\n", "Not lint free", lint_output))

  invisible(result)
}
