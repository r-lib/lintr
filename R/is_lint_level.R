#' Is this an expression- or a file-level source object?
#'
#' Helper for determining whether the current `source_expression` contains
#'   all expressions in the current file, or just a single expression.
#'
#' @param source_expression A parsed expression object, i.e., an element
#'   of the object returned by [get_source_expressions()].
#' @param level Which level of expression is being tested? `"expression"`
#'   means an individual expression, while `"file"` means all expressions
#'   in the current file are available.
#'
#' @examplesIf requireNamespace("withr", quietly = TRUE)
#' tmp <- withr::local_tempfile(lines = c("x <- 1", "y <- x + 1"))
#' source_exprs <- get_source_expressions(tmp)
#' is_lint_level(source_exprs$expressions[[1L]], level = "expression")
#' is_lint_level(source_exprs$expressions[[1L]], level = "file")
#' is_lint_level(source_exprs$expressions[[3L]], level = "expression")
#' is_lint_level(source_exprs$expressions[[3L]], level = "file")
#'
#' @export
is_lint_level <- function(source_expression, level = c("expression", "file")) {
  stopifnot(!missing(level))
  level <- match.arg(level)
  required_key <- paste0(if (level == "file") "full_", "parsed_content")
  required_key %in% names(source_expression)
}

#' Determine whether a linter needs to run for a given source_expression level
#'
#' Used by [lint()] to avoid unneccessary calls to linters.
#'
#' @param linter A linter.
#' @param level Which level of expression is being tested? `"expression"`
#'   means an individual expression, while `"file"` means all expressions
#'   in the current file are available.
#'
#' @keywords internal
#' @noRd
is_linter_level <- function(linter, level = c("expression", "file")) {
  linter_level <- attr(linter, "linter_level", exact = TRUE)
  if (is.null(linter_level) || is.na(linter_level)) {
    return(TRUE)
  }
  level <- match.arg(level)
  identical(linter_level, level)
}
