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
expect_lint <- function(content, checks, ...) {

  results <- expectation_lint(content, checks, ...)

  reporter <- testthat::get_reporter()

  # flatten list if a list of lists
  if (is.list(results) &&
    is.list(results[[1]]) &&
    !testthat::is.expectation(results[[1]])) {

    results <- unlist(recursive = FALSE, results)
  }

  if (testthat::is.expectation(results)) {
    reporter$add_result(results)
  }
  else {
    lapply(results, reporter$add_result)
  }

  invisible(results)
}

expectation_lint <- function(content, checks, ...) {

  filename <- tempfile()
  on.exit(unlink(filename))
  cat(file=filename, content, sep="\n")

  lints <- lint(filename, list(...))

  linter_names <- substitute(alist(...))[-1]

  if (is.null(checks)) {
    return(testthat::expectation(length(lints) %==% 0L,
        paste0(paste(collapse=", ", linter_names),
          " returned ", print(lints),
          " lints when it was expected to return none!"),
        paste0(paste(collapse=", ", linter_names),
          " returned 0 lints as expected.")))
  }

  if (!is.list(checks)) {
    checks <- list(checks)
  }
  checks[] <- lapply(checks, fix_names, "message")

  if (length(lints) != length(checks)) {
    return(testthat::expectation(FALSE,
        paste0(paste(collapse=", ", linter_names),
          " did not return ", length(checks),
          " lints as expected from content:", content, lints)))
  }

  itr <- 0L
  mapply(function(lint, check) {
    itr <- itr + 1L
    lapply(names(check), function(field) {
      value <- lint[[field]]
      check <- check[[field]]
      if (field == "message") {
        testthat::expectation(re_matches(value, check),
          sprintf("lint: %d %s: %s did not match: %s",
            itr,
            field,
            value,
            check
          ),
          sprintf("lint: %d %s: %s matched: %s",
            itr,
            field,
            value,
            check
          )
        )
      } else {
        testthat::expectation(`==`(value, check),
          sprintf("lint: %d %s: %s did not match: %s",
            itr,
            field,
            value,
            check
          ),
          sprintf("lint: %d %s: %s matched: %s",
            itr,
            field,
            value,
            check
          )
        )
      }
    })
  },
  lints,
  checks)
}
