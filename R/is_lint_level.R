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
#' @examples
#' tmp <- tempfile()
#' writeLines(c("x <- 1", "y <- x + 1"), tmp)
#' source_exprs <- get_source_expressions(tmp)
#' is_lint_level(source_exprs$expressions[[1L]], level = "expression")
#' is_lint_level(source_exprs$expressions[[1L]], level = "file")
#' is_lint_level(source_exprs$expressions[[3L]], level = "expression")
#' is_lint_level(source_exprs$expressions[[3L]], level = "file")
#' unlink(tmp)
#'
#' @export
is_lint_level <- function(source_expression, level = c("expression", "file")) {
  level <- match.arg(level)
  required_key <- switch(level, file = "full_parsed_content", expression = "parsed_content")
  required_key %in% names(source_expression)
}

#' Determine whether a linter needs to run for a given source_expression level
#'
#' Used by [lint()] to avoid unnecessary calls to linters.
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
