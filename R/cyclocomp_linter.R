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
  Linter(linter_level = "expression", function(source_expression) {
    # nocov start
    if (!requireNamespace("cyclocomp", quietly = TRUE)) {
      cli::cli_abort(c(
        "Cyclocomp complexity is computed using {.fn cyclocomp::cyclocomp}.",
        i = "Please install the needed {.pkg cyclocomp} package."
      ))
    }
    # nocov end

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
        "Reduce the cyclomatic complexity of this function from %d to at most %d.",
        complexity, complexity_limit
      ),
      ranges = list(rep(col1, 2L)),
      line = source_expression$lines[1L]
    )
  })
}
