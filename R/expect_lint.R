#' Lint expectation
#'
#' This is an expectation function to test that the lints produced by \code{lint} satisfy a number
#' of checks.
#'
#' @param content a character vector for the file content to be linted, each vector element
#' representing a line of text.
#' @param checks checks to be performed:
#' \describe{
#'   \item{NULL}{check that no lints are returned.}
#'   \item{single string or regex object}{check that the single lint returned has a matching
#'   message.}
#'   \item{named list}{check that the single lint returned has fields that match. Accepted fields
#'   are the same as those taken by \code{\link{Lint}}.}
#'   \item{list of named lists}{for each of the multiple lints returned, check that it matches
#'   the checks in the corresponding named list (as described in the point above).}
#' }
#' Named vectors are also accepted instead of named lists, but this is a compatibility feature that
#' is not recommended for new code.
#' @param ... arguments passed to \code{\link{lint}}, e.g. the linters or cache to use.
#' @param file if not \code{NULL}, read content from the specified file rather than from \code{content}.
#' @param language temporarily override Rs \code{LANGUAGE} envvar, controlling localisation of base
#' R error messages. This makes testing them reproducible on all systems irrespective of their
#' native R language setting.
#' @return \code{NULL}, invisibly.
#' @examples
#' # no expected lint
#' expect_lint("a", NULL, trailing_blank_lines_linter)
#'
#' # one expected lint
#' expect_lint("a\n", "superfluous", trailing_blank_lines_linter)
#' expect_lint("a\n", list(message="superfluous", line_number=2), trailing_blank_lines_linter)
#'
#' # several expected lints
#' expect_lint("a\n\n", list("superfluous", "superfluous"), trailing_blank_lines_linter)
#' expect_lint(
#'   "a\n\n",
#'   list(list(message="superfluous", line_number=2), list(message="superfluous", line_number=3)),
#'   trailing_blank_lines_linter()
#' )
#' @export
expect_lint <- function(content, checks, ..., file = NULL, language = "en") {
  old_lang <- set_lang(language)
  on.exit(reset_lang(old_lang))

  if (is.null(file)) {
    file <- tempfile()
    con <- base::file(file, encoding = "UTF-8")
    on.exit(unlink(file), add = TRUE)
    writeLines(content, con = con, sep = "\n")
    close(con)
  }

  lints <- lint(file, ...)
  n_lints <- length(lints)
  lint_str <- if (n_lints) paste0(c("", lints), collapse = "\n") else ""

  wrong_number_fmt  <- "got %d lints instead of %d%s"
  if (is.null(checks)) {
    msg <- sprintf(wrong_number_fmt, n_lints, length(checks), lint_str)
    return(testthat::expect(n_lints %==% 0L, msg))
  }

  if (!is.list(checks) | !is.null(names(checks))) { # vector or named list
    checks <- list(checks)
  }
  checks[] <- lapply(checks, fix_names, "message")

  if (n_lints != length(checks)) {
    msg <- sprintf(wrong_number_fmt, n_lints, length(checks), lint_str)
    return(testthat::expect(FALSE, msg))
  }

  local({
    itr <- 0L
    # keep 'linter' as a field even if we remove the deprecated argument from Lint() in the future
    lint_fields <- unique(c(names(formals(Lint)), "linter"))
    Map(function(lint, check) {
      itr <<- itr + 1L
      lapply(names(check), function(field) {
        if (!field %in% lint_fields) {
          stop(sprintf(
            "check #%d had an invalid field: \"%s\"\nValid fields are: %s\n",
            itr, field, toString(lint_fields)))
        }
        check <- check[[field]]
        value <- lint[[field]]
        msg <- sprintf("check #%d: %s %s did not match %s",
                       itr, field, deparse(value), deparse(check))
               # deparse ensures that NULL, list(), etc are handled gracefully
        exp <- if (field == "message") {
          re_matches(value, check)
        } else {
          isTRUE(all.equal(value, check))
        }
        if (!is.logical(exp)) {
          stop(
            "Invalid regex result, did you mistakenly have a capture group in the regex? ",
            "Be sure to escape parenthesis with `[]`",
            call. = FALSE
          )
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
  testthat::skip_on_cran()
  testthat::skip_on_covr()

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
