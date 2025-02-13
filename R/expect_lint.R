#' Lint expectation
#'
#' These are expectation functions to test specified linters on sample code in the `testthat` testing framework.
#' * `expect_lint` asserts that specified lints are generated.
#' * `expect_no_lint` asserts that no lints are generated.
#'
#' @param content a character vector for the file content to be linted, each vector element representing a line of
#' text.
#' @param checks checks to be performed:
#' \describe{
#'   \item{NULL}{check that no lints are returned.}
#'   \item{single string or regex object}{check that the single lint returned has a matching message.}
#'   \item{named list}{check that the single lint returned has fields that match. Accepted fields are the same as those
#'     taken by [Lint()].}
#'   \item{list of named lists}{for each of the multiple lints returned, check that it matches the checks in the
#'     corresponding named list (as described in the point above).}
#' }
#' Named vectors are also accepted instead of named lists, but this is a compatibility feature that
#' is not recommended for new code.
#' @param ... arguments passed to [lint()], e.g. the linters or cache to use.
#' @param file if not `NULL`, read content from the specified file rather than from `content`.
#' @param language temporarily override Rs `LANGUAGE` envvar, controlling localization of base R error messages.
#' This makes testing them reproducible on all systems irrespective of their native R language setting.
#' @return `NULL`, invisibly.
#' @examples
#' # no expected lint
#' expect_no_lint("a", trailing_blank_lines_linter())
#'
#' # one expected lint
#' expect_lint("a\n", "trailing blank", trailing_blank_lines_linter())
#' expect_lint("a\n", list(message = "trailing blank", line_number = 2), trailing_blank_lines_linter())
#'
#' # several expected lints
#' expect_lint("a\n\n", list("trailing blank", "trailing blank"), trailing_blank_lines_linter())
#' expect_lint(
#'   "a\n\n",
#'   list(
#'     list(message = "trailing blank", line_number = 2),
#'     list(message = "trailing blank", line_number = 3)
#'   ),
#'   trailing_blank_lines_linter()
#' )
#' @export
expect_lint <- function(content, checks, ..., file = NULL, language = "en") {
  require_testthat()

  old_lang <- set_lang(language)
  on.exit(reset_lang(old_lang))

  if (is.null(file)) {
    file <- tempfile()
    on.exit(unlink(file), add = TRUE)
    local({
      con <- base::file(file, encoding = "UTF-8")
      on.exit(close(con))
      writeLines(content, con = con, sep = "\n")
    })
  }

  lints <- lint(file, ...)
  n_lints <- length(lints)
  lint_str <- if (n_lints) paste(c("", lints), collapse = "\n") else ""

  wrong_number_fmt <- "got %d lints instead of %d%s"
  if (is.null(checks)) {
    msg <- sprintf(wrong_number_fmt, n_lints, length(checks), lint_str)
    return(testthat::expect(n_lints %==% 0L, msg))
  }

  if (!is.list(checks) || !is.null(names(checks))) { # vector or named list
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
    Map(
      function(lint, check) {
        itr <<- itr + 1L
        lapply(names(check), function(field) {
          if (!field %in% lint_fields) {
            cli_abort(c(
              x = "Check {.val {itr}} has an invalid field: {.field {field}}.",
              i = "Valid fields are: {.field {lint_fields}}."
            ))
          }
          check <- check[[field]]
          value <- lint[[field]]
          msg <- sprintf(
            "check #%d: %s %s did not match %s",
            itr, field, deparse(value), deparse(check)
          )
          # deparse ensures that NULL, list(), etc are handled gracefully
          ok <- if (field == "message") {
            re_matches_logical(value, check)
          } else {
            isTRUE(all.equal(value, check))
          }
          testthat::expect(ok, msg)
        })
      },
      lints,
      checks
    )
  })

  invisible(NULL)
}

#' @rdname expect_lint
#' @export
expect_no_lint <- function(content, ..., file = NULL, language = "en") {
  require_testthat()
  expect_lint(content, NULL, ..., file = file, language = language)
}

#' Test that the package is lint free
#'
#' This function is a thin wrapper around lint_package that simply tests there are no
#' lints in the package. It can be used to ensure that your tests fail if the package
#' contains lints.
#'
#' @param ... arguments passed to [lint_package()]
#' @export
expect_lint_free <- function(...) {
  require_testthat()

  testthat::skip_on_cran()
  testthat::skip_on_covr()

  lints <- lint_package(...)
  has_lints <- length(lints) > 0L

  lint_output <- NULL
  if (has_lints) {
    lint_output <- format(lints)
  }
  result <- testthat::expect(
    !has_lints,
    paste0("Not lint free\n", lint_output)
  )

  invisible(result)
}

# Helper function to check if testthat is installed.
require_testthat <- function() {
  parent_call = sys.call(-1L)[[1L]]
  # supported: foo() or lintr::foo(). Undefined behavior otherwise.
  name = as.character(if (is.name(parent_call)) parent_call else parent_call[[3L]])
  if (!requireNamespace("testthat", quietly = TRUE)) {
    cli_abort(
      "{.fun {name}} is designed to work within the {.pkg testthat} testing framework, which could not be loaded."
    )
  }
}
