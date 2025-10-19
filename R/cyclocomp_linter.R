#' Cyclomatic complexity linter
#'
#' Check for overly complicated expressions. See `cyclocomp()` function from `{cyclocomp}`.
#'
#' @param complexity_limit Maximum cyclomatic complexity, default `15`. Expressions more complex
#' than this are linted.
#'
#' @examplesIf requireNamespace("cyclocomp", quietly = TRUE)
#' # will produce lints
#' lint(
#'   text = "if (TRUE) 1 else 2",
#'   linters = cyclocomp_linter(complexity_limit = 1L)
#' )
#'
#' # okay
#' lint(
#'   text = "if (TRUE) 1 else 2",
#'   linters = cyclocomp_linter(complexity_limit = 2L)
#' )
#'
#' @evalRd rd_tags("cyclocomp_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
cyclocomp_linter <- function(complexity_limit = 15L) {
  # nocov start
  if (!requireNamespace("cyclocomp", quietly = TRUE)) {
    cli::cli_warn(c(
      "Cyclocomp complexity is computed using {.fn cyclocomp::cyclocomp}.",
      i = "Please install the needed {.pkg cyclocomp} package."
    ))
    return(Linter(function(.) cli_abort("cyclocomp_linter is disabled due to lack of the {.pkg cyclocomp} package")))
  }
  # nocov end
  Linter(linter_level = "expression", function(source_expression) {
    complexity <- try_silently(
      cyclocomp::cyclocomp(parse(text = source_expression$content))
    )
    if (inherits(complexity, "try-error") || complexity <= complexity_limit) {
      return(list())
    }
    col1 <- source_expression[["column"]][1L]
    Lint(
      filename = source_expression[["filename"]],
      line_number = source_expression[["line"]][1L],
      column_number = source_expression[["column"]][1L],
      type = "style",
      message = sprintf(
        "The cyclomatic complexity of this expression is %d, which exceeds the limit of %d. %s",
        complexity, complexity_limit,
        "Consider replacing high-complexity sections like loops and branches with helper functions."
      ),
      ranges = list(rep(col1, 2L)),
      line = source_expression$lines[1L]
    )
  })
}
